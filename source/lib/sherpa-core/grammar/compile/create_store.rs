use super::parser::{
  sherpa::{ASTNode, Grammar, Name},
  *,
};
use crate::{
  ascript::types::{ascript_first_node_id, ascript_last_node_id},
  compile::{GrammarRef, GrammarStore, ProductionId, Symbol, SymbolID},
  grammar::{compile::parser::sherpa::Export, create_production_guid_name, create_scanner_name},
  types::{self, ImportedGrammarReferences, RuleId, RuleSymbol, SherpaErrorSeverity, StringId},
  Journal,
  ReportType,
  SherpaError,
  SherpaResult,
};
use lazy_static::__Deref;
use regex::Regex;
use sherpa_runtime::types::{BlameColor, Token};
use std::{
  collections::{btree_map, HashMap, HashSet, VecDeque},
  path::PathBuf,
  sync::Arc,
};

/// Runs an initial configuration of a GrammarStore from data contained in
/// a parsed source grammar AST.
pub fn create_store<'a>(
  j: &mut Journal,
  ast: &'a Grammar,
  source_path: PathBuf,
  imports: ImportedGrammarReferences,
) -> Arc<GrammarStore> {
  let mut g = GrammarStore {
    id: GrammarRef::new(get_grammar_name(ast, &source_path), source_path.clone()),
    imports,
    ..Default::default()
  };

  j.set_active_report(
    &format!("Grammar [{}] Compilation", g.id.name),
    ReportType::GrammarCompile(g.id.guid),
  );

  j.report_mut().start_timer("Initial Prep");

  // Process metadata
  let mut global_ignore_symbols = vec![];

  for metadata in &ast.preamble {
    match metadata {
      ASTNode::Ignore(box ignore) => {
        for symbol in &ignore.symbols {
          if let Some(sym_id) = record_symbol(j, &mut g, &symbol, false) {
            global_ignore_symbols.push(sym_id);
          }
        }
      }
      ASTNode::Export(box Export { production, reference }) => {
        let id = get_prod_id(j, &mut g, production);
        g.exports.push((id, reference.to_string()));
      }
      _ => {}
    }
  }

  let mut post_process_productions: VecDeque<Box<sherpa::Production>> = VecDeque::new();

  for prod in &ast.productions {
    match prod.is_append {
      false => {
        let p = process_production_node(j, &mut g, prod, &mut post_process_productions);
        // Add the default export production if exports have not yet been defined.
        if p != ProductionId::default() && g.exports.is_empty() {
          g.exports.push((p, "default".to_string()));
        }
      }
      true => {
        let (prod_id, ..) = get_production_identifiers(j, &mut g, &prod);
        let mut list_index = 0;
        for rule in &prod.rules {
          match process_rule(j, &mut g, &prod, rule, &mut list_index) {
            SherpaResult::Ok((mut rules, _)) => {
              match g.merge_productions.entry(prod_id) {
                btree_map::Entry::Vacant(e) => {
                  e.insert((prod.name_sym.to_string(), rules));
                }
                btree_map::Entry::Occupied(mut e) => {
                  e.get_mut().1.append(&mut rules);
                }
              };
            }
            err => create_unexpected_rule_error(j, rule, &g, err),
          }
        }
      }
    }
  }

  // Continue processing any generated _. This may loop
  // for a while as any given production may have several nested
  // anonymous productions through lists `...(+) | ...(*)` and
  // groups `(... | ...)`

  while let Some(node) = post_process_productions.pop_front() {
    process_production_node(j, &mut g, &node, &mut post_process_productions);
  }

  // Add ignore symbols to productions
  g.production_ignore_symbols =
    g.productions.keys().map(|k| (*k, global_ignore_symbols.clone())).collect::<HashMap<_, _>>();

  j.report_mut().stop_timer("Initial Prep");

  Arc::new(g)
}

fn create_unexpected_rule_error<R>(
  j: &mut Journal,
  rule: &sherpa::Rule,
  g: &GrammarStore,
  err: SherpaResult<R>,
) {
  j.report_mut().add_error(SherpaError::SourceError {
    loc:        rule.tok.clone(),
    path:       g.id.path.clone(),
    id:         "unexpected-rule-compile-error",
    msg:        "Could not parse rule".to_string(),
    inline_msg: Default::default(),
    ps_msg:     format!("{}", err.get_error_string()),
    severity:   SherpaErrorSeverity::Critical,
  })
}

fn process_production_node<'a>(
  j: &mut Journal,
  g: &mut GrammarStore,
  prod_node: &sherpa::Production,
  post_process_productions: &mut VecDeque<Box<sherpa::Production>>,
) -> ProductionId {
  let (prod_id, guid_name, plain_name) = get_production_identifiers(j, g, prod_node);
  match (prod_id, &prod_node.name_sym, prod_node, g.productions.get(&prod_id)) {
    (_, ASTNode::Production_Import_Symbol(_), ..) => {
      j.report_mut().add_error(
        SherpaError::SourceError {
          loc:        prod_node.tok.clone(),
          path:       g.id.path.clone(),
          id:         "invalid-production",
          msg:        "Cannot define a production of an imported grammar.".into(),
          inline_msg: "".into(),
          ps_msg:     "note: Try using the `+>` operator to extend an existing production with additional bodies.".into(),
          severity:   SherpaErrorSeverity::Critical,
        });
      prod_id
    }
    (prod, _, ..) if prod == Default::default() => {
      j.report_mut().add_error(SherpaError::SourceError {
        loc:        prod_node.tok.clone(),
        path:       g.id.path.clone(),
        id:         "invalid-production",
        msg:        "This is not a valid production.".into(),
        inline_msg: "".into(),
        ps_msg:     "".into(),
        severity:   SherpaErrorSeverity::Critical,
      });
      prod_id
    }
    (_, _, prod, None) => {
      // Extract rule data and gather symbol information
      let mut list_index = 0;
      let bodies = prod
        .rules
        .iter()
        .flat_map(|rule| match process_rule(j, g, &prod, rule, &mut list_index) {
          SherpaResult::Ok((new_rules, productions)) => {
            for prod in productions {
              post_process_productions.push_back(prod);
            }

            new_rules
          }
          err => {
            create_unexpected_rule_error(j, rule, &g, err);
            vec![]
          }
        })
        .collect();

      insert_production(
        g,
        types::Production {
          id: prod_id,
          guid_name,
          name: plain_name, // prod.symbol.Token().to_string()
          loc: prod_node.tok.clone(),
          sym_id: SymbolID::Production(prod_id, g.id.guid),
          ..Default::default()
        },
        bodies,
      );

      prod_id
    }
    (_, _, prod, Some(existing_production)) => {
      j.report_mut().add_error(SherpaError::SourcesError {
        id:       "production-redefinition",
        sources:  vec![
          (
            prod.tok.clone(),
            g.id.path.clone(),
            format!("Redefinition of {} occurs here.", plain_name),
          ),
          (
            existing_production.loc.clone(),
            g.id.path.clone(),
            format!("{} first defined here.", plain_name),
          ),
        ],
        msg:      format!("Redefinition of {} is not allowed", plain_name),
        ps_msg:   Default::default(),
        severity: SherpaErrorSeverity::Critical,
      });
      ProductionId::default()
    }
  }
}

#[inline]
pub fn insert_production(
  g: &mut GrammarStore,
  mut prod: types::Production,
  bodies: Vec<types::Rule>,
) {
  let prod_id = prod.id;

  prod.number_of_rules = insert_rules(g, &prod_id, bodies).len() as u16;

  g.productions.insert(prod_id, prod);
}

#[inline]
pub fn insert_rules(
  g: &mut GrammarStore,
  prod_id: &ProductionId,
  rules: Vec<types::Rule>,
) -> Vec<RuleId> {
  let offset_index = g.production_rules.get(&prod_id).map_or(0, |b| b.len());

  let body_ids = rules
    .into_iter()
    .enumerate()
    .map(|(i, mut b)| {
      let id = RuleId::new(&prod_id, offset_index + i);
      b.id = id;
      g.rules.insert(id, b);
      id
    })
    .collect::<Vec<_>>();

  match g.production_rules.entry(*prod_id) {
    btree_map::Entry::Vacant(e) => {
      e.insert(body_ids.clone());
    }
    btree_map::Entry::Occupied(mut e) => {
      e.get_mut().append(&mut body_ids.clone());
    }
  };

  g.production_rules.get(prod_id).unwrap().to_owned()
}

struct SymbolData<'a> {
  pub annotation:       String,
  pub is_list:          bool,
  pub is_group:         bool,
  pub is_optional:      bool,
  pub is_shift_nothing: bool,
  pub is_exclusive:     bool,
  pub sym_atom:         Option<&'a ASTNode>,
}

/// Get a flattened view of a symbol's immediate AST
fn get_symbol_details<'a>(
  j: &mut Journal,
  g: &mut GrammarStore,
  mut sym: &'a ASTNode,
) -> SymbolData<'a> {
  let mut data = SymbolData {
    annotation:       String::new(),
    is_list:          false,
    is_group:         false,
    is_optional:      false,
    is_shift_nothing: false,
    is_exclusive:     false,
    sym_atom:         None,
  };

  loop {
    match sym {
      ASTNode::AnnotatedSymbol(annotated) => {
        if annotated.reference.len() > 0 {

          debug_assert_eq!(
            &annotated.reference[0..1], "^",
            "Annotation values are no longer prefixed with \"^\". The following line needs to be changed to:
            data.annotation = annotated.reference.clone(); 
            This assert can be removed after the change is made.");

            data.annotation = annotated.reference[1..].to_string();
          }
        data.is_exclusive = annotated.prority.is_some();
        data.is_optional |= annotated.is_optional;
        sym = &annotated.symbol;
      }
      ASTNode::Group_Production(_) => {
        data.is_group = true;
        break;
      }
      ASTNode::List_Production(box p) => {
        data.is_list = true;
        data.is_optional |= p.optional;
        break;
      }
      ASTNode::Terminal(box t) => {
        data.is_exclusive |= t.is_exclusive;
        break;
      }
      // This symbol types are "real" symbols, in as much
      // as they represent actual parsable entities which are
      // submitted to the bytecode compiler for evaluation
      ASTNode::ClassSymbol(_)
      | ASTNode::AnyGroup(_)
      | ASTNode::Production_Terminal_Symbol(_)
      //| ASTNode::TemplateProductionSymbol(_)
      | ASTNode::Production_Symbol(_)
      | ASTNode::Production_Import_Symbol(_) => {
        break;
      }
      _ => {
        j.report_mut().add_error(
          SherpaError::SourceError { loc: sym.to_token(), path: g.id.path.clone(), id: "unknown-symbol-node", msg: "[INTERNAL ERROR]".into(), inline_msg: format!("Unexpected ASTNode {:?}", sym), ps_msg: "".into(), severity: SherpaErrorSeverity::Critical }
         );
        break;
      }
    }
  }

  data.sym_atom = Some(sym);

  data
}

fn create_rule_vectors<'a>(
  j: &mut Journal,
  g: &mut GrammarStore,
  token: &Token,
  symbols: &Vec<(usize, &ASTNode)>,
  production_name: &String,
  list_index: &mut u32,
) -> SherpaResult<(Vec<(Token, Vec<RuleSymbol>)>, Vec<Box<sherpa::Production>>)> {
  let mut rules = vec![(token.clone(), vec![])];
  let mut productions: Vec<Box<sherpa::Production>> = vec![];

  for (index, sym) in symbols {
    let original_bodies = 0..rules.len();

    let SymbolData {
      annotation,
      is_list,
      is_group,
      is_optional,
      is_shift_nothing,
      is_exclusive,
      sym_atom,
    } = get_symbol_details(j, g, sym);

    if let Some(mut sym) = sym_atom.to_owned() {
      let generated_symbol;

      if is_optional {
        // Need to create new bodies that contains all permutations
        // of encountered symbols except for the currently
        // considered symbol. This is achieved by duplicating all
        // rule vectors, then adding the current symbol to the
        // original vectors, but not the duplicates.
        for entry in rules.clone() {
          rules.push(entry)
        }
      }
      if let ASTNode::AnyGroup(group) = sym {
        // New bodies are created with the values of the any group
        // symbol being distributed to each rule.

        let mut pending_rules = vec![];

        fn get_index_permutations(indice_candidates: Vec<usize>) -> Vec<Vec<usize>> {
          if indice_candidates.len() > 1 {
            let mut out = vec![];
            for (i, candidate) in indice_candidates.iter().enumerate() {
              let mut remainder = indice_candidates.clone();
              remainder.remove(i);
              for mut permutation in get_index_permutations(remainder) {
                permutation.insert(0, *candidate);
                out.push(permutation)
              }
            }
            out
          } else {
            vec![indice_candidates]
          }
        }

        let indices = group.symbols.iter().enumerate().map(|(i, _)| i).collect();

        let candidate_symbols =
          group.symbols.iter().enumerate().map(|(i, s)| (i + index, s)).collect::<Vec<_>>();

        for permutation in
          if group.unordered { get_index_permutations(indices) } else { vec![indices] }
        {
          let symbols = permutation.iter().map(|i| candidate_symbols[*i]).collect::<Vec<_>>();

          let (mut new_bodies, mut new_productions) =
            create_rule_vectors(j, g, token, &symbols, production_name, list_index)?;

          pending_rules.append(&mut new_bodies);

          productions.append(&mut new_productions);
        }

        let mut new_rules = vec![];

        for pending_rule in pending_rules {
          if pending_rule.1.len() == 0 {
            continue;
          }

          for rule in &mut rules[original_bodies.clone()] {
            let mut new_body = rule.clone();
            new_body.1.extend(pending_rule.1.iter().cloned());
            new_rules.push(new_body)
          }
        }

        rules = new_rules;

        continue;
      } else if is_group {
        // Need to create new production that the virtual group
        // production is bound to, add it to the list of
        // currently considered productions, and replace
        // this symbol with a production symbol pointing
        // to the group.

        // Except, if there are no functions within the production
        // bodies we can simply lower bodies of the group production
        // into the host production.
        let ASTNode::Group_Production(group) = sym else {
          unreachable!("Sym should be an ASTNode::Group_Production");
        };
        // All bodies are plain without annotations or functions
        if annotation.is_empty() && !some_rules_have_ast_definitions(&group.rules) {
          // For each rule in the group clone the existing rule lists and
          // process each list independently, inserting the new symbols
          // into the existing bodies. We must make sure the indices are
          // preserved since only the last symbol in each rule can be bound
          // to the index of the group production symbol.

          let mut pending_bodies = vec![];

          for rule in &group.rules {
            let (mut new_bodies, mut new_productions) = create_rule_vectors(
              j,
              g,
              &rule.tok,
              &rule.symbols.iter().map(|s| (9999, s)).collect(),
              production_name,
              list_index,
            )?;
            // The last symbol in each of these new bodies is set
            // with the original symbol id

            for r in &mut new_bodies {
              if r.1.is_empty() {
                dbg!(&rule.symbols);
              }
              r.1.last_mut()?.original_index = *index as u32;
            }

            pending_bodies.append(&mut new_bodies);
            productions.append(&mut new_productions);
          }

          let mut new_bodies = vec![];

          for pending_body in pending_bodies {
            for rule in &mut rules[original_bodies.clone()] {
              let mut new_body = rule.clone();
              new_body.1.extend(pending_body.1.iter().cloned());
              new_bodies.push(new_body)
            }
          }

          rules.splice(original_bodies, new_bodies);

          // We do not to process the existing symbol as it is
          // now replaced with its component rule symbols,
          // so we'll skip the rest of the loop
          continue;
        } else {
          let (prod_sym, production) = create_ast_production(
            &(production_name.to_owned() + "_group_" + &index.to_string()),
            &group.rules,
            group.tok.clone(),
          );

          productions.push(Box::new(production));
          generated_symbol = prod_sym;
          sym = &generated_symbol;
        }
      } else if is_list {
        // Create a new production that turns
        // `A => a` into `A => a | A => A a`
        // and produce a SymbolId that points to that production.

        match sym {
          ASTNode::List_Production(_) => {
            let (symbol, terminal_symbol, tok) = match sym {
              ASTNode::List_Production(list) => {
                (&list.symbols, &list.terminal_symbol, list.tok.clone())
              }
              _ => unreachable!(),
            };

            let mut rule_a = sherpa::Rule {
              ast_definition: None,
              syntax_definition: None,
              recover_definition: None,
              is_priority: false,
              symbols: vec![symbol.clone()],
              tok: symbol.to_token(),
            };

            let mut rule_b = rule_a.clone();
            rule_b.tok = sym.to_token();

            rule_a.ast_definition = Some(Box::new(sherpa::Ascript::new(
              ASTNode::AST_Statements(Box::new(sherpa::AST_Statements::new(
                vec![ASTNode::AST_Vector(Box::new(sherpa::AST_Vector::new(
                  vec![ASTNode::AST_NamedReference(Box::new(sherpa::AST_NamedReference {
                    value: ascript_first_node_id.to_string(),
                    tok:   tok.clone(),
                  }))],
                  tok.clone(),
                )))],
                tok.clone(),
              ))),
              tok.clone(),
            )));

            rule_b.ast_definition = Some(Box::new(sherpa::Ascript::new(
              ASTNode::AST_Statements(Box::new(sherpa::AST_Statements::new(
                vec![ASTNode::AST_Vector(Box::new(sherpa::AST_Vector::new(
                  vec![
                    ASTNode::AST_NamedReference(Box::new(sherpa::AST_NamedReference {
                      value: ascript_first_node_id.to_string(),
                      tok:   tok.clone(),
                    })),
                    ASTNode::AST_NamedReference(Box::new(sherpa::AST_NamedReference {
                      value: ascript_last_node_id.to_string(),
                      tok:   tok.clone(),
                    })),
                  ],
                  tok.clone(),
                )))],
                tok.clone(),
              ))),
              tok.clone(),
            )));

            if let Some(terminal_symbol) = terminal_symbol {
              rule_b.symbols.insert(0, ASTNode::Terminal(terminal_symbol.clone()));
            }

            (*list_index) += 1;

            let (prod_sym, mut production) = create_ast_production(
              &(production_name.to_owned() + "_list_" + &(*list_index).to_string()),
              &[Box::new(rule_b), Box::new(rule_a)],
              tok,
            );

            // Add the production symbol to the front of the rule
            // to make the production left recursive
            production.rules[0].symbols.insert(0, prod_sym.clone());

            productions.push(Box::new(production));
            generated_symbol = prod_sym;
            sym = &generated_symbol;
          }
          _ => unreachable!(
            "I don't know what to do with this. \n{}",
            sym.to_token().blame(1, 1, "", BlameColor::RED)
          ),
        }
      }

      if let Some(id) = record_symbol(j, g, sym, is_exclusive) {
        for (_, vec) in &mut rules[original_bodies] {
          vec.push(RuleSymbol {
            original_index: *index as u32,
            sym_id: id,
            annotation: annotation.clone(),
            consumable: !is_shift_nothing,
            precedence: is_exclusive as u32,
            tok: sym.to_token(),
            grammar_ref: g.id.clone(),
            ..Default::default()
          });
        }
      }
    }
  }

  SherpaResult::Ok((rules, productions))
}

fn create_ast_production(
  name: &str,
  rules: &[Box<sherpa::Rule>],
  token: Token,
) -> (ASTNode, sherpa::Production) {
  // Create a virtual production and symbol to go in its place
  let symbol = ASTNode::Production_Symbol(Box::new(sherpa::Production_Symbol::new(
    name.to_string(),
    token.clone(),
  )));

  let production = sherpa::Production::new(
    false,
    false,
    name.to_string(),
    symbol.clone(),
    None,
    rules.to_vec(),
    vec![],
    token,
  );

  (symbol, production)
}

fn some_rules_have_ast_definitions(rules: &[Box<sherpa::Rule>]) -> bool {
  rules.iter().any(|rule| rule.ast_definition.is_some())
}

fn process_rule<'a>(
  j: &mut Journal,
  g: &mut GrammarStore,
  production: &sherpa::Production,
  rule: &sherpa::Rule,
  list_index: &mut u32,
) -> SherpaResult<(Vec<types::Rule>, Vec<Box<sherpa::Production>>)> {
  match get_productions_names(j, g, &production.name_sym) {
    Some((_, prod_name)) => {
      let (bodies, productions) = create_rule_vectors(
        j,
        g,
        &rule.tok,
        &rule
          .symbols
          .iter()
          .fold((0, vec![]), |mut b, s| {
            b.1.push((b.0, s));
            b.0 += match s {
              ASTNode::AnyGroup(box s) => s.symbols.len(),
              _ => 1,
            };
            b
          })
          .1,
        &prod_name,
        list_index,
      )?;

      let mut unique_bodies = vec![];
      let mut seen = HashSet::new();

      for (t, b) in bodies {
        let sym = RuleId::from_syms(&b.iter().map(|s| s.sym_id).collect::<Vec<_>>());
        if !seen.contains(&sym) {
          unique_bodies.push(types::Rule {
            syms: b.clone(),
            len: b.len() as u16,
            prod_id: get_production_identifiers(j, g, production).0,
            ast_definition: rule.ast_definition.as_ref().map(|d| d.deref().clone()),
            tok: t.clone(),
            grammar_ref: g.id.clone(),
            ..Default::default()
          });
          seen.insert(sym);
        }
      }

      SherpaResult::Ok((unique_bodies, productions))
    }
    _ => SherpaResult::Ok(Default::default()),
  }
}

/// Returns the ProductionId  of a production symbol.
/// Returns an empty ProductionId if the node cannot be resolved to a production symbol.
///
/// ASTNodes that can be resolved to a production symbol:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::Production_Terminal_Symbol]
#[inline]
fn get_prod_id(j: &mut Journal, g: &mut GrammarStore, production: &ASTNode) -> ProductionId {
  get_production_identifiers_from_node(j, g, production).0
}

/// Returns the ProductionId, guid name String, and the normal name String of a production symbol.
/// Returns default values if the node cannot be resolved to a production symbol.
///
/// ASTNodes that can be resolved to a production symbol:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::Production_Terminal_Symbol]
#[inline]
fn get_production_identifiers_from_node(
  j: &mut Journal,
  g: &mut GrammarStore,
  node: &ASTNode,
) -> (ProductionId, String, String) {
  match get_productions_names(j, g, node) {
    Some((guid_name, plain_name)) => {
      let id = ProductionId::from(&guid_name);

      if !g.production_names.contains_key(&id) {
        g.production_names.insert(id, (plain_name.clone(), guid_name.clone()));
      }

      (id, guid_name, plain_name)
    }
    _ => (Default::default(), Default::default(), Default::default()),
  }
}
/// Return the `Production_Import_Symbol` or `Production_Symbol` from a valid node tree.
/// Accepts
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::Production_Terminal_Symbol]
fn get_production_symbol<'a>(node: &'a ASTNode, g: &'a GrammarStore) -> Option<&'a ASTNode> {
  match node {
    ASTNode::Production_Import_Symbol(..) | ASTNode::Production_Symbol(..) => Some(node),
    ASTNode::Production(prod) => get_production_symbol(&prod.name_sym, g),
    ASTNode::Production_Terminal_Symbol(prod_tok) => get_production_symbol(&prod_tok.production, g),
    _ => None,
  }
}

/// Get the resolved grammar data from compatible nodes.
///
/// ASTNodes that can be resolved to a grammar:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::Production_Terminal_Symbol]
///
/// ## Returns
/// A Tuple comprised of the grammar 0:uuid_name, 1:local_name, and
/// 2:absolute_path. local_name is `root` if the grammar maps to
/// currently rendered grammar.

fn get_grammar_info_from_node<'a>(
  j: &mut Journal,
  g: &'a GrammarStore,
  node: &'a ASTNode,
) -> Option<Arc<GrammarRef>> {
  match get_production_symbol(node, g) {
    Some(ASTNode::Production_Import_Symbol(prod_imp_sym)) => {
      let local_import_grammar_name = &prod_imp_sym.module;

      match g.imports.get(local_import_grammar_name) {
        None => {
          j.report_mut().add_error(SherpaError::SourceError {
            loc:        node.to_token(),
            path:       g.id.path.clone(),
            id:         "nonexistent-import-source",
            msg:        format!(
              "Unknown Grammar : The imported grammar reference \u{001b}[31m{}\u{001b}[0m does not match any imported grammar.",
              local_import_grammar_name
            ),
            inline_msg: Default::default(),
            ps_msg:     Default::default(),
            severity:   SherpaErrorSeverity::Critical,
          });
          None
        }
        Some(g_ref) => Some(g_ref.clone()),
      }
    }
    Some(ASTNode::Production_Symbol(_)) => Some(g.id.clone()),
    _ => {
      j.report_mut().add_error(SherpaError::SourceError {
        loc:        node.to_token(),
        path:       g.id.path.clone(),
        id:         "unexpected-node",
        msg:        " Unable to resolve production name of this node".to_string(),
        inline_msg: Default::default(),
        ps_msg:     Default::default(),
        severity:   SherpaErrorSeverity::Critical,
      });
      None
    }
  }
}

/// Returns the ProductionId, guid name String, and the normal name String of a production.
/// Returns default values if the node cannot be resolved to a production symbol.
#[inline]
fn get_production_identifiers(
  j: &mut Journal,
  g: &mut GrammarStore,
  node: &sherpa::Production,
) -> (ProductionId, String, String) {
  match get_productions_names(j, g, &node.name_sym) {
    Some((guid_name, plain_name)) => {
      let id = ProductionId::from(&guid_name);

      if !g.production_names.contains_key(&id) {
        g.production_names.insert(id, (plain_name.clone(), guid_name.clone()));
      }

      (id, guid_name, plain_name)
    }
    _ => (Default::default(), Default::default(), Default::default()),
  }
}

/// Get the guid_name and plain_name of the production name symbol.
///
/// Returns `Option<(guid_name: String, friendly_name: String)>`
///
/// This guid_name is guaranteed to be unique amongst all grammars imported by
/// the root grammar.
fn get_productions_names(
  j: &mut Journal,
  g: &GrammarStore,
  name_sym: &ASTNode,
) -> Option<(String, String)> {
  match name_sym {
    ASTNode::Production_Import_Symbol(prod_imp_sym) => {
      let production_name = &prod_imp_sym.name;
      let local_import_grammar_name = &prod_imp_sym.module;

      match g.imports.get(local_import_grammar_name) {
        None => {
          j.report_mut().add_error(SherpaError::SourceError {
            loc:        name_sym.to_token().into(),
            path:       g.id.path.clone(),
            id:         "nonexistent-import-production",
            msg:        format!(
              "The production {} cannot be found in the imported grammar {}.",
              production_name, local_import_grammar_name
            ),
            inline_msg: "Could not locate this production".to_string(),
            ps_msg:     Default::default(),
            severity:   SherpaErrorSeverity::Critical,
          });
          None
        }
        Some(g_ref) => Some((
          create_production_guid_name(&g_ref.guid_name, production_name),
          prod_imp_sym.tok.to_string(),
        )),
      }
    }
    ASTNode::Production_Symbol(prod_sym) => {
      Some((create_production_guid_name(&g.id.guid_name, &prod_sym.name), prod_sym.name.clone()))
    }
    _ => {
      j.report_mut().add_error(SherpaError::SourceError {
        loc:        name_sym.to_token(),
        path:       g.id.path.clone(),
        id:         "unresolved-production-symbol",
        msg:        "Unexpected node: Unable to resolve production name of this node!".into(),
        inline_msg: "".into(),
        ps_msg:     "".into(),
        severity:   SherpaErrorSeverity::Critical,
      });
      None
    }
  }
}

lazy_static::lazy_static! {
  static ref identifier_re: Regex = Regex::new(r"[\w_-][\w\d_-]*$").unwrap();
  static ref number_re: Regex = Regex::new(r"\d+$").unwrap();
  static ref escaped: Regex = Regex::new(r"\\([^$])").unwrap();
}

/// Returns an appropriate SymbolID::Defined* based on the input
/// string
#[inline]
pub fn get_terminal_id(string: &String, exclusive: bool) -> SymbolID {
  match (exclusive, number_re.is_match(string), identifier_re.is_match(string)) {
    (true, true, false) => SymbolID::ExclusiveDefinedNumeric(StringId::from(string)),
    (false, true, false) => SymbolID::DefinedNumeric(StringId::from(string)),
    (true, false, true) => SymbolID::ExclusiveDefinedIdentifier(StringId::from(string)),
    (false, false, true) => SymbolID::DefinedIdentifier(StringId::from(string)),
    (true, ..) => SymbolID::ExclusiveDefinedSymbol(StringId::from(string)),
    (false, ..) => SymbolID::DefinedSymbol(StringId::from(string)),
  }
}

/// Converts a Symbol AST node into a symbol object and
/// inventories the symbol into the grammar store.
fn record_symbol(
  j: &mut Journal,
  g: &mut GrammarStore,
  sym_node: &ASTNode,
  exclusive: bool,
) -> Option<SymbolID> {
  match sym_node {
    ASTNode::AnnotatedSymbol(box annotated) => {
      record_symbol(j, g, &annotated.symbol, annotated.prority.is_some())
    }
    ASTNode::Terminal(box terminal) => {
      let old = &terminal.val;
      let string = escaped.replace_all(old, "$1").to_string();
      let sym_id = get_terminal_id(&string, terminal.is_exclusive | exclusive);

      if let std::collections::btree_map::Entry::Vacant(e) = g.symbols.entry(sym_id) {
        g.symbol_strings.insert(sym_id, string.clone());

        let byte_length = string.bytes().len() as u32;
        let code_point_length = string.chars().count() as u32;

        e.insert(Symbol {
          bytecode_id: 0,
          guid: sym_id,
          byte_length,
          cp_len: code_point_length,
          scanner_only: false,
          friendly_name: String::new(),
          loc: terminal.tok.clone(),
          g_ref: Some(g.id.clone()),
        });
      }
      Some(sym_id)
    }
    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      process_production(j, g, sym_node)
    }
    ASTNode::ClassSymbol(gen) => match gen.val.as_str() {
      "sp" => Some(SymbolID::GenericSpace),
      "tab" => Some(SymbolID::GenericHorizontalTab),
      "nl" => Some(SymbolID::GenericNewLine),
      "id" => Some(SymbolID::GenericIdentifier),
      "num" => Some(SymbolID::GenericNumber),
      "sym" => Some(SymbolID::GenericSymbol),
      _ => Some(SymbolID::Undefined),
    },
    ASTNode::Production_Terminal_Symbol(token_prod) => process_token_production(j, g, token_prod),
    _ => {
      j.report_mut().add_error(SherpaError::SourceError {
        loc:        sym_node.to_token(),
        path:       g.id.path.clone(),
        id:         "unexpected-node",
        msg:        "Unexpected ASTNode while attempting to record symbol".to_string(),
        inline_msg: Default::default(),
        ps_msg:     format!("{sym_node:#?}"),
        severity:   SherpaErrorSeverity::Critical,
      });
      None
    }
  }
}

fn process_production(j: &mut Journal, g: &mut GrammarStore, node: &ASTNode) -> Option<SymbolID> {
  match node {
    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      let production_id = get_prod_id(j, g, node);

      match get_grammar_info_from_node(j, g, node)
        .map(|data| SymbolID::Production(production_id, data.guid))
      {
        Some(id) => {
          g.production_symbols.insert(id, node.to_token());
          Some(id)
        }
        _ => None,
      }
    }
    _ => {
      j.report_mut().add_error(SherpaError::SourceError {
        loc:        node.to_token(),
        path:       g.id.path.clone(),
        id:         "invalid-production-node",
        msg:        "This is not a hashable production symbol.".to_string(),
        inline_msg: Default::default(),
        ps_msg:     Default::default(),
        severity:   SherpaErrorSeverity::Critical,
      });
      None
    }
  }
}

fn get_grammar_name(ast: &Grammar, source_path: &PathBuf) -> String {
  let name = ast
    .preamble
    .iter()
    .filter_map(|p| match p {
      ASTNode::Name(box Name { name }) => Some(name.to_owned()),
      _ => None,
    })
    .next()
    .or_else(|| -> Option<String> { Some(source_path.file_stem()?.to_str()?.to_string()) })
    .unwrap_or_else(|| "unnamed".to_string());
  name
}

/// Create a TokenProduction symbol and intern the symbol info.
fn process_token_production(
  j: &mut Journal,
  g: &mut GrammarStore,
  node: &sherpa::Production_Terminal_Symbol,
) -> Option<SymbolID> {
  let tok = &node.tok;
  match process_production(j, g, &node.production) {
    Some(SymbolID::Production(prod_id, grammar_id)) => {
      let scanner_prod_id = ProductionId::from(&create_scanner_name(prod_id, grammar_id));
      let guid = SymbolID::Production(scanner_prod_id, grammar_id);
      let tok_id = SymbolID::TokenProduction(prod_id, grammar_id, scanner_prod_id);

      g.production_symbols.insert(tok_id, tok.clone());

      g.symbols.entry(tok_id).or_insert(Symbol {
        guid,
        friendly_name: tok.to_string(),
        loc: tok.clone(),
        g_ref: Some(g.id.clone()),
        ..Default::default()
      });

      Some(tok_id)
    }
    _ => None,
  }
}
