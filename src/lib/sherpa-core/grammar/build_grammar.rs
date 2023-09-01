use super::{
  super::{
    parser::{self, ASTNode, Grammar},
    types::*,
  },
  utils::{get_symbol_details, SymbolData},
};
use crate::{
  grammar::utils::resolve_grammar_path,
  journal::Journal,
  parser::ast::escaped_from,
  types::error_types::add_invalid_import_source_error,
  utils::create_u64_hash,
};
#[cfg(debug_assertions)]
use parser::GetASTNodeType;
use sherpa_rust_runtime::types::Token;
use std::{hash::Hash, path::PathBuf, sync::Arc};

/// Temporary structure to host rule data during
/// construction.
pub struct RuleData<'a> {
  symbols: &'a [(usize, &'a ASTNode)],
  ast_ref: Option<ASTToken>,
}

/// Temporary structure to host non-terminal data during
/// construction.
pub struct NonTermData<'a> {
  root_nterm: NonTermId,
  root_g_name: IString,
  root_f_name: IString,
  symbols: &'a mut OrderedSet<SymbolId>,
  sub_nonterminals: &'a mut Array<Box<SubNonTerminal>>,
  rules: &'a mut Array<Rule>,
}

impl<'b> NonTermData<'b> {
  /// Create a new [ProductionData] that references a different
  /// `rules` array, but keeps all other references the same
  /// as `self`.
  pub fn set_rules<'d>(&'d mut self, rules: &'d mut Array<Rule>) -> NonTermData<'d> {
    NonTermData {
      rules,
      root_g_name: self.root_g_name,
      root_f_name: self.root_f_name,
      root_nterm: self.root_nterm,
      symbols: &mut self.symbols,
      sub_nonterminals: &mut self.sub_nonterminals,
    }
  }
}

/// Intermediate structure to host grammar data during
/// construction.
pub struct GrammarData {
  pub id: GrammarIdentities,
  pub imports: Map<IString, GrammarIdentities>,
  pub exports: Array<(IString, (NonTermId, Token))>,
  pub global_skipped: Array<ASTNode>,
  pub grammar: Box<Grammar>,
}

pub fn convert_grammar_data_to_header(import_id: GrammarIdentities, g_data: GrammarData) -> Box<GrammarHeader> {
  let mut identity = import_id;
  identity.guid_name = g_data.id.guid_name;
  Box::new(GrammarHeader {
    identity,
    pub_nterms: g_data.exports.into_iter().collect(),
    imports: g_data.imports.values().map(|v| v.guid).collect(),
  })
}

/// Parse grammar string and create a Grammar AST.
pub fn parse_grammar(string_data: &str) -> SherpaResult<Box<Grammar>> {
  SherpaResult::Ok(Grammar::from_str(string_data)?)
}

/// Do an initial preparation of the grammar data.
pub fn create_grammar_data(
  j: &mut Journal,
  grammar: Box<Grammar>,
  grammar_path: &PathBuf,
  string_store: &IStringStore,
) -> SherpaResult<GrammarData> {
  const EXTENSIONS: [&str; 3] = ["sg", "sherpa", "hcg"];

  let source_dir = grammar_path.parent().map_or(PathBuf::default(), |p| p.to_owned());

  let mut imports = Map::default();
  let mut exports = Array::default();
  let mut skipped = Array::default();
  let mut name = grammar_path.file_stem().and_then(|d| d.to_str()).unwrap_or("default");

  for preamble in &grammar.preamble {
    match preamble {
      ASTNode::Import(import) => {
        let parser::Import { reference, uri, .. } = import.as_ref();
        let path = PathBuf::from(uri);

        match resolve_grammar_path(&path, &source_dir, &EXTENSIONS) {
          SherpaResult::Ok(path) => {
            imports.insert(reference.intern(string_store), GrammarIdentities::from_path(&path, string_store));
          }
          _ => {
            add_invalid_import_source_error(j, import, &path, &source_dir);
            imports.insert(reference.intern(string_store), GrammarIdentities::from_path(&path, string_store));
          }
        }
      }
      ASTNode::Export(export) => {
        // Saving exports to a temporary store until we are able extract import
        // information from the preambles, since some exported symbols may
        // be Production_Import_Symbol types.
        exports.push(export.clone());
      }
      ASTNode::Ignore(ignore) => {
        for sym in &ignore.symbols {
          skipped.push(sym.clone())
        }
      }
      ASTNode::Name(name_ast) => {
        name = name_ast.name.as_str();
      }
      _ => {
        #[allow(unreachable_code)]
        {
          #[cfg(debug_assertions)]
          unreachable!("Unrecognized node: {:?}", preamble.get_type());
          unreachable!()
        }
      }
    }
  }

  let mut g_data = GrammarData {
    id: GrammarIdentities {
      guid:      grammar_path.into(),
      guid_name: grammar_guid_name(name, grammar_path, string_store),
      name:      name.intern(string_store),
      path:      grammar_path.intern(string_store),
    },
    global_skipped: skipped,
    grammar,
    imports,
    exports: Default::default(),
  };

  // Once we have g_data, and more importantly g_data.imports, we can convert
  // our exports into ProductionIds.
  if exports.is_empty() != true {
    for export in exports {
      let nterm = get_nonterminal_id_from_ast_node(&g_data, &export.production)?;
      g_data.exports.push((export.reference.intern(string_store), (nterm, export.production.to_token())));
    }
  } else {
    // Use the fist declared non-terminal as the default entry
    let nterm = &g_data.grammar.productions[0];
    if let Ok(nterm_id) = get_nonterminal_id_from_ast_node(&g_data, &nterm) {
      g_data.exports.push(("default".intern(string_store), (nterm_id, nterm.to_token())));
    }
  }

  SherpaResult::Ok(g_data)
}

pub fn extract_nonterminals<'a>(
  _j: &mut Journal,
  g_data: &'a GrammarData,
  s_store: &IStringStore,
) -> SherpaResult<(Array<(Box<NonTerminal>, &'a ASTNode)>, Array<Box<CustomState>>)> {
  let mut nterms = Array::new();
  let mut parse_states = Array::new();
  for nterm in &g_data.grammar.productions {
    match nterm {
      ASTNode::State(parse_state) => {
        let (guid_name, friendly_name) = nterm_names(parse_state.id.name.as_str(), &g_data.id, s_store);
        parse_states.push(Box::new(CustomState {
          id: NonTermId::from((g_data.id.guid, parse_state.id.name.as_str())),
          guid_name,
          friendly_name,
          g_id: g_data.id.guid,
          tok: parse_state.tok.clone(),
          state: parse_state.clone(),
          symbols: Default::default(),
        }))
      }
      ASTNode::PrattProduction(box parser::PrattProduction { name_sym, rules: _, tok })
      | ASTNode::CFProduction(box parser::CFProduction { name_sym, rules: _, tok })
      | ASTNode::PegProduction(box parser::PegProduction { name_sym, rules: _, tok }) => {
        let (guid_name, f_name) = nterm_names(name_sym.name.as_str(), &g_data.id, s_store);
        nterms.push((
          Box::new(NonTerminal {
            id: NonTermId::from((g_data.id.guid, name_sym.name.as_str())),
            guid_name,
            friendly_name: f_name,
            g_id: g_data.id.guid,
            type_: NonTermType::ContextFree,
            rules: Default::default(),
            sub_nterms: Default::default(),
            symbols: Default::default(),
            tok_nterms: Default::default(),
            tok: tok.clone(),
            asts: Default::default(),
          }),
          nterm,
        ));
      }
      ASTNode::AppendProduction(box parser::AppendProduction { name_sym, rules: _, tok }) => {
        match get_nonterminal_symbol(g_data, &name_sym) {
          (Some(name_sym), _) => {
            let (guid_name, f_name) = nterm_names(name_sym.name.as_str(), &g_data.id, s_store);
            nterms.push((
              Box::new(NonTerminal {
                id: NonTermId::from((g_data.id.guid, name_sym.name.as_str())),
                guid_name,
                friendly_name: f_name,
                g_id: g_data.id.guid,
                type_: NonTermType::ContextFree,
                rules: Default::default(),
                sub_nterms: Default::default(),
                symbols: Default::default(),
                tok_nterms: Default::default(),
                tok: tok.clone(),
                asts: Default::default(),
              }),
              nterm,
            ));
          }
          (_, Some(name_sym)) => {
            let import_grammar_name = name_sym.module.to_string().intern(s_store);

            let import_g_id = o_to_r(g_data.imports.get(&import_grammar_name), "could not find grammar")?;

            let (guid_name, f_name) = nterm_names(name_sym.name.as_str(), import_g_id, s_store);
            nterms.push((
              Box::new(NonTerminal {
                id: NonTermId::from((import_g_id.guid, name_sym.name.as_str())),
                guid_name,
                friendly_name: f_name,
                g_id: import_g_id.guid,
                type_: NonTermType::ContextFree,
                rules: Default::default(),
                sub_nterms: Default::default(),
                symbols: Default::default(),
                tok_nterms: Default::default(),
                tok: tok.clone(),
                asts: Default::default(),
              }),
              nterm,
            ));
          }
          _ => unreachable!(),
        };
      }
      _ast => unreachable!("Unrecognized node "),
    }
  }

  SherpaResult::Ok((nterms, parse_states))
}

pub fn process_parse_state<'a>(
  mut parse_state: Box<CustomState>,
  g_data: &GrammarData,
  s_store: &IStringStore,
) -> SherpaResult<Box<CustomState>> {
  // Extract symbol information from the state.

  let CustomState { id, state, symbols, guid_name, .. } = parse_state.as_mut();
  {
    let parser::Statement { branch, .. } = state.statement.as_ref();

    if let Some(branch) = &branch {
      match branch {
        ASTNode::TerminalMatches(term_matches) => {
          for match_ in &term_matches.matches {
            match match_ {
              ASTNode::TermMatch(term_match) => {
                record_symbol(
                  &term_match.sym,
                  &mut NonTermData {
                    root_nterm: *id,
                    root_g_name: *guid_name,
                    root_f_name: *guid_name,
                    symbols,
                    sub_nonterminals: &mut Default::default(),
                    rules: &mut Default::default(),
                  },
                  g_data,
                  s_store,
                )?;
              }
              _ => {
                // Default
                // Hint
              }
            }
          }
        }
        ASTNode::ProductionMatches(..) => {}
        ASTNode::Matches(ast_match) => match ast_match.mode.as_str() {
          "TERMINAL" => {}
          _ => {
            /*
             * PRODUCTION
             * TERMINAL
             */
          }
        },
        _ => {
          // ASTNode::Gotos
          // ASTNode::Pass
          // ASTNode::Fail
          // ASTNode::Accept
        }
      }
    }
  }

  SherpaResult::Ok(parse_state)
}

pub fn process_nonterminal<'a>(
  (mut nterm, nterm_ast): (Box<NonTerminal>, &'a ASTNode),
  g_data: &GrammarData,
  s_store: &IStringStore,
) -> SherpaResult<Box<NonTerminal>> {
  let (type_, rules) = match nterm_ast {
    ASTNode::CFProduction(nterm) => (NonTermType::ContextFree, &nterm.rules),
    ASTNode::AppendProduction(nterm) => (NonTermType::ContextFree, &nterm.rules),
    ASTNode::PrattProduction(nterm) => (NonTermType::Pratt, &nterm.rules),
    ASTNode::PegProduction(nterm) => (NonTermType::Peg, &nterm.rules),
    _ast => {
      #[cfg(debug_assertions)]
      todo!("Create build for {_ast:?}");
      #[cfg(not(debug_assertions))]
      todo!()
    }
  };

  nterm.type_ = type_;

  for rule in rules {
    process_rule(&mut nterm, rule, g_data, s_store)?;
  }

  for rule in &nterm.rules {
    if rule.symbols.is_empty() {
      panic!(
        "{}",
        rule.tok.blame(
          1,
          1,
          &format!(
            "Rules that can derive the empty rule `{} => ε` are currently not allowed in Sherpa Grammars!",
            nterm.friendly_name.to_str(s_store).as_str()
          ),
          None
        )
      )
    }
  }

  SherpaResult::Ok(nterm)
}

fn process_rule(nterm: &mut NonTerminal, rule: &parser::Rule, g_data: &GrammarData, s_store: &IStringStore) -> SherpaResult<()> {
  let ast_syms = rule.symbols.iter().enumerate().collect::<Array<_>>();

  let NonTerminal { id, rules, sub_nterms, symbols, .. } = nterm;
  let mut nterm_data = NonTermData {
    root_nterm: *id,
    symbols,
    sub_nonterminals: sub_nterms,
    rules,
    root_g_name: nterm.guid_name,
    root_f_name: nterm.friendly_name,
  };

  let ast_ref = intern_ast(rule, &mut nterm_data);

  process_rule_symbols(&RuleData { symbols: &ast_syms, ast_ref }, &mut nterm_data, g_data, s_store, rule.tok.clone())?;

  SherpaResult::Ok(())
}

fn process_rule_symbols(
  rule_data: &RuleData,
  p_data: &mut NonTermData,
  g_data: &GrammarData,
  s_store: &IStringStore,
  tok: Token,
) -> SherpaResult<()> {
  let mut rules: Array<Rule> = Default::default();

  rules.push(Rule {
    symbols: Default::default(),
    skipped: Default::default(),
    ast:     rule_data.ast_ref.clone(),
    tok:     tok.clone(),
    g_id:    g_data.id,
  });

  for (index, sym) in rule_data.symbols.iter() {
    let original_bodies = 0..rules.len();
    let SymbolData {
      annotation, is_optional, symbol_precedence, token_precedence, sym_atom, ..
    } = get_symbol_details(sym);

    // If the symbol is optional, then we create new set
    // of rule symbols that will not contain the optional
    // symbol.
    is_optional.then(|| rules.append(&mut rules.clone()));

    let (sym, tok) = match sym_atom.unwrap() {
      ASTNode::NotEmptySet(set) => {
        let mut pending_rules = vec![];

        fn get_index_permutations(indexes: Array<usize>) -> Array<Array<usize>> {
          if indexes.len() > 1 {
            let mut out = vec![];
            for (i, candidate) in indexes.iter().enumerate() {
              let mut remainder = indexes.clone();
              remainder.remove(i);
              for mut permutation in get_index_permutations(remainder) {
                permutation.insert(0, *candidate);
                out.push(permutation)
              }
            }
            out
          } else {
            vec![indexes]
          }
        }

        let indices = set.symbols.iter().enumerate().map(|(i, _)| i).collect();

        let candidate_symbols = set.symbols.iter().enumerate().map(|(i, s)| (i + index, s)).collect::<Array<_>>();

        for permutation in if set.unordered { get_index_permutations(indices) } else { vec![indices] } {
          let symbols = permutation.iter().map(|i| candidate_symbols[*i]).collect::<Array<_>>();

          process_rule_symbols(
            &RuleData { symbols: &symbols, ast_ref: None },
            &mut p_data.set_rules(&mut pending_rules),
            g_data,
            s_store,
            tok.clone(),
          )?;
        }

        let mut new_rules = Array::new();
        let mut rule_id = Set::new();

        for pending_rule in pending_rules {
          // Prevent empty rules from being created.
          if pending_rule.symbols.is_empty() {
            continue;
          }

          // Prevent duplicate rules from being created.
          if !rule_id.insert(create_u64_hash(&pending_rule.symbols)) {
            continue;
          }

          for rule in &mut rules[original_bodies.clone()] {
            let mut new_rule = rule.clone();
            new_rule.symbols.extend(pending_rule.symbols.iter().cloned());
            new_rules.push(new_rule)
          }
        }

        rules = new_rules;

        // This rule has been replaced with the derived rules, so
        // it no longer needs to be processed, continuing on to
        // the next iteration.
        continue;
      }
      ASTNode::GroupProduction(group) => {
        // All bodies are plain without annotations or functions
        if annotation.is_empty() && !some_rules_have_ast_definitions(&group.rules) {
          // For each rule in the group clone the existing rule lists and
          // process each list independently, inserting the new symbols
          // into the existing bodies. We must make sure the indices are
          // preserved since only the last symbol in each rule can be bound
          // to the index of the group non-terminal symbol.

          let mut p = vec![];

          for rule in &group.rules {
            process_rule_symbols(
              &RuleData { symbols: &map_symbols_to_unindexed(&rule.symbols), ast_ref: None },
              &mut p_data.set_rules(&mut p),
              g_data,
              s_store,
              rule.tok.clone(),
            )?;
          }

          let mut new_rules = vec![];

          for pending_rule in &mut p {
            // The last symbol in each of these new bodies is set
            // with the original symbol id
            pending_rule.symbols.last_mut().expect("There should be at least one symbol").original_index = *index as u32;
            for rule in &mut rules[original_bodies.clone()] {
              let mut new_rule = rule.clone();
              new_rule.symbols.extend(pending_rule.symbols.iter().cloned());
              new_rules.push(new_rule)
            }
          }

          rules.splice(original_bodies, new_rules);

          // We do not to process the existing symbol as it is
          // now replaced with its component rule symbols,
          // so we'll skip the rest of the loop
          continue;
        } else {
          let mut sub_nterm_rules = Default::default();

          for rule in &group.rules {
            let n_rule = &RuleData {
              symbols: &rule.symbols.iter().enumerate().collect::<Array<_>>(),
              ast_ref: intern_ast(rule, p_data),
            };
            process_rule_symbols(n_rule, &mut p_data.set_rules(&mut sub_nterm_rules), g_data, s_store, rule.tok.clone())?
          }

          let nterm = p_data.root_nterm;
          let index = p_data.sub_nonterminals.len();
          let id = NonTermId::from((nterm, index));

          let (guid_name, friendly_name) = sub_nterm_names("group", p_data, s_store);

          p_data.sub_nonterminals.push(Box::new(SubNonTerminal {
            id,
            guid_name,
            friendly_name,
            type_: SubNonTermType::Group,
            g_id: g_data.id.guid,
            rules: sub_nterm_rules,
          }));

          (id.as_sym(), group.tok.clone())
        }
      }
      ASTNode::List_Production(box parser::List_Production { symbol, terminal_symbol, tok, .. }) => {
        let mut rule_syms = vec![symbol.clone()];
        let mut rules = Default::default();

        if let Some(terminal_symbol) = terminal_symbol {
          rule_syms.insert(0, terminal_symbol.clone());
        }

        let r_data = &RuleData {
          symbols: &map_symbols_to_indexed(&rule_syms),
          ast_ref: Some(ASTToken::ListEntry(symbol.to_token().get_tok_range())),
        };

        process_rule_symbols(r_data, &mut p_data.set_rules(&mut rules), g_data, s_store, symbol.to_token().clone())?;

        let nterm = p_data.root_nterm;
        let nterm_index = p_data.sub_nonterminals.len();
        let id = NonTermId::from((nterm, nterm_index));
        let sym = id.as_sym();

        let mut secondary_rules = rules.clone();

        // Add the ProductionID as a non-term symbol
        // to the beginning of each rule, making the
        // resulting list non-terminal left recursive.
        for rule in &mut secondary_rules {
          rule.ast = Some(ASTToken::ListIterate(tok.get_tok_range()));

          rule.tok = tok.clone();

          rule.symbols.insert(0, SymbolRef {
            id: sym,
            annotation: annotation.intern(s_store),
            loc: tok.clone(),
            original_index: 0,
            ..Default::default()
          });

          for (i, SymbolRef { original_index: index, .. }) in rule.symbols.iter_mut().enumerate() {
            *index = i as u32;
          }
        }

        if terminal_symbol.is_some() {
          for rule in &mut rules {
            rule.symbols.remove(0);
            for (i, SymbolRef { original_index: index, .. }) in rule.symbols.iter_mut().enumerate() {
              *index = i as u32;
            }
          }
        }

        rules.append(&mut secondary_rules);

        let (guid_name, friendly_name) = sub_nterm_names("list", p_data, s_store);

        p_data.sub_nonterminals.push(Box::new(SubNonTerminal {
          type_: SubNonTermType::List,
          guid_name,
          friendly_name,
          id,
          g_id: g_data.id.guid,
          rules,
        }));

        (sym, tok.clone())
      }
      sym_atom => (record_symbol(sym, p_data, g_data, s_store)?, sym_atom.to_token()),
    };

    for rule in &mut rules[original_bodies] {
      rule.symbols.push(SymbolRef {
        id: sym,
        annotation: annotation.intern(s_store),
        loc: tok.clone(),
        original_index: *index as u32,
        token_precedence,
        symbol_precedence,
      });
    }
  }

  let skipped_symbols = g_data
    .global_skipped
    .iter()
    .map(|sym_node| {
      let Ok(data) = record_symbol(sym_node, p_data, g_data, s_store) else {
        panic!("Could not record symbol");
      };
      data
    })
    .collect::<Array<_>>();

  for rule in &mut rules {
    rule.skipped = skipped_symbols.clone();
  }

  p_data.rules.append(&mut rules);

  SherpaResult::Ok(())
}

fn intern_ast(rule: &parser::Rule, _p_data: &mut NonTermData) -> Option<ASTToken> {
  let ast_ref = rule.ast.as_ref().map(|s| ASTToken::Defined(Arc::new(*s.clone())));
  ast_ref
}

fn map_symbols_to_unindexed(ast_syms: &Array<ASTNode>) -> Array<(usize, &ASTNode)> {
  ast_syms.iter().map(|s| (9999, s)).collect::<Array<_>>()
}

fn map_symbols_to_indexed(ast_syms: &Array<ASTNode>) -> Array<(usize, &ASTNode)> {
  ast_syms.iter().enumerate().map(|(i, s)| (i, s)).collect::<Array<_>>()
}

fn some_rules_have_ast_definitions(rules: &[Box<parser::Rule>]) -> bool {
  rules.iter().any(|rule| rule.ast.is_some())
}

/// Converts a Symbol AST node into a symbol object and
/// records the symbol in the grammar store.
fn record_symbol(
  sym_node: &ASTNode,
  p_data: &mut NonTermData,
  g_data: &GrammarData,
  s_store: &IStringStore,
) -> SherpaResult<SymbolId> {
  let id = match sym_node {
    ASTNode::AnnotatedSymbol(annotated) => record_symbol(&annotated.symbol, p_data, g_data, s_store)?,

    ASTNode::TerminalToken(terminal) => {
      let string = escaped_from((&terminal.val).into())?.join("");
      let val = string.intern(s_store);
      let id = SymbolId::Token { val };

      id
    }

    ASTNode::EOFSymbol(_) => SymbolId::EndOfFile {},

    ASTNode::ClassSymbol(gen) => match gen.val.as_str() {
      "sp" => SymbolId::ClassSpace {},
      "tab" => SymbolId::ClassHorizontalTab {},
      "nl" => SymbolId::ClassNewLine {},
      "id" => SymbolId::ClassIdentifier {},
      "num" => SymbolId::ClassNumber {},
      "sym" => SymbolId::ClassSymbol {},
      _ => {
        #[allow(unreachable_code)]
        {
          #[cfg(debug_assertions)]
          unreachable!("unsupported generic {}", gen.val);
          unreachable!()
        }
      }
    },

    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      let id = get_nonterminal_id_from_ast_node(g_data, sym_node)?.as_sym();
      // Bypass the registration of this symbol as a symbol.
      return SherpaResult::Ok(id);
    }

    ASTNode::Production_Terminal_Symbol(token_prod) => {
      let id = get_nonterminal_id_from_ast_node(g_data, &token_prod.production)?.as_tok_sym();
      // Bypass the registration of this symbol as a symbol.
      return SherpaResult::Ok(id);
    }

    _ => {
      #[allow(unreachable_code)]
      {
        #[cfg(debug_assertions)]
        unreachable!("Unrecognized node: {:?}", sym_node.get_type());
        unreachable!()
      }
    }
  };

  p_data.symbols.insert(id);

  SherpaResult::Ok(id)
}

/// Returns `(None, Some(&Production_Import_Symbol))` or
/// `(Some(&Production_Symbol), None)`from a valid non-terminal like node.
///
/// Valid Node:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production_Terminal_Symbol]
/// - [ASTNode::State]
/// - [ASTNode::PrattProduction]
/// - [ASTNode::PegProduction]
/// - [ASTNode::CFProduction]
fn get_nonterminal_symbol<'a>(
  g_data: &GrammarData,
  node: &'a ASTNode,
) -> (Option<&'a parser::Production_Symbol>, Option<&'a parser::Production_Import_Symbol>) {
  match node {
    ASTNode::Production_Import_Symbol(prod_import) => (None, Some(prod_import.as_ref())),
    ASTNode::Production_Symbol(prod_sym) => (Some(prod_sym.as_ref()), None),
    ASTNode::Production_Terminal_Symbol(prod_tok) => get_nonterminal_symbol(g_data, &prod_tok.production),
    ASTNode::State(box parser::State { id, .. })
    | ASTNode::PrattProduction(box parser::PrattProduction { name_sym: id, .. })
    | ASTNode::PegProduction(box parser::PegProduction { name_sym: id, .. })
    | ASTNode::CFProduction(box parser::CFProduction { name_sym: id, .. }) => (Some(id.as_ref()), None),
    ASTNode::AppendProduction(box parser::AppendProduction { name_sym, .. }) => get_nonterminal_symbol(g_data, name_sym),
    _ => unreachable!(),
  }
}

/// Return the `ProductionId` from a valid non-terminal like node.
///
/// Valid Node:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production_Terminal_Symbol]
/// - [ASTNode::State]
/// - [ASTNode::PrattProduction]
/// - [ASTNode::PegProduction]
/// - [ASTNode::CFProduction]
fn get_nonterminal_id_from_ast_node(g_data: &GrammarData, node: &ASTNode) -> SherpaResult<NonTermId> {
  match get_nonterminal_symbol(g_data, node) {
    (Some(nterm), None) => Ok(NonTermId::from((g_data.id.guid, nterm.name.as_str()))),
    (None, Some(nterm)) => {
      let ref_name = nterm.module.to_token();

      match g_data.imports.get(&ref_name) {
        Some(GrammarIdentities { guid, .. }) => Ok(NonTermId::from((*guid, nterm.name.as_str()))),
        _ => SherpaResult_Err("Could not retrieve ProductionID from node"),
      }
    }
    _ => {
      #[allow(unreachable_code)]
      {
        #[cfg(debug_assertions)]
        unreachable!("Unrecognized node: {:?}", node.get_type());
        unreachable!()
      }
    }
  }
}

// NAMES ------------------------------------------------------------------------

/// Creates a globally unique name and friendly name for a non-terminal
pub fn nterm_names(base_name: &str, g_data: &GrammarIdentities, s_store: &IStringStore) -> (IString, IString) {
  ((g_data.guid_name.to_string(s_store) + "____" + base_name).intern(s_store), base_name.intern(s_store))
}

/// Creates a globally unique name and friendly name for a sub-non-terminal.
/// `sub_name` should be a string indicating the type of symbol that
/// the sub-non-terminal was derived from.
fn sub_nterm_names(sub_name: &str, p_data: &NonTermData, s_store: &IStringStore) -> (IString, IString) {
  if p_data.sub_nonterminals.is_empty() {
    (
      (p_data.root_g_name.to_string(s_store) + "_" + sub_name).intern(s_store),
      (p_data.root_f_name.to_string(s_store) + "_" + sub_name).intern(s_store),
    )
  } else {
    (
      (p_data.root_g_name.to_string(s_store) + "_" + sub_name + "_" + &p_data.sub_nonterminals.len().to_string()).intern(s_store),
      (p_data.root_f_name.to_string(s_store) + "_" + sub_name + "_" + &p_data.sub_nonterminals.len().to_string()).intern(s_store),
    )
  }
}
/// Creates a globally unique name for a grammar
fn grammar_guid_name(name: &str, grammar_path: &PathBuf, string_store: &IStringStore) -> IString {
  (name.to_string() + "_" + &to_base64_name(grammar_path)).intern(string_store)
}

fn to_base64_name<T: Hash>(val: T) -> String {
  let mut string = Vec::new();

  let val = create_u64_hash(val);

  for i in 0..4 {
    let val = (val >> (i * 6)) & 0x3E;
    let ascii_base = if val < 10 {
      val + 48 // ASCII numbers
    } else if val < 36 {
      val - 10 + 65 // Uppercase ASCII letters
    } else if val < 62 {
      val - 36 + 97 // Lowercase ASCII letters
    } else {
      95 // ASCII underscore
    };
    string.push(ascii_base as u8);
  }

  String::from_utf8(string).unwrap()
}

/// Remove artifacts related to a single grammar from a soup through mutation.
pub fn remove_grammar_mut(id: GrammarId, soup: &mut GrammarSoup) -> SherpaResult<()> {
  let GrammarSoup { grammar_headers, nonterminals, custom_states, .. } = soup;

  {
    let mut grammar_headers = grammar_headers.write()?;

    if grammar_headers.remove(&id).is_none() {
      return SherpaResult::Ok(());
    }
  }

  {
    let mut nonterminals = nonterminals.write()?;

    let nonterminals_temp = nonterminals.drain(..).collect::<Vec<_>>();

    nonterminals.extend(nonterminals_temp.into_iter().filter(|p| p.g_id != id))
  }

  {
    let mut custom_states = custom_states.write()?;

    let custom_states_temp = custom_states.drain().collect::<Vec<_>>();

    custom_states.extend(custom_states_temp.into_iter().filter(|(_, s)| s.g_id != id))
  }

  SherpaResult::Ok(())
}

#[cfg(test)]
mod test {
  use std::path::PathBuf;

  use crate::{
    journal::{Journal, ReportType},
    parser::Grammar,
    types::*,
  };

  fn create_test_data(input: &str) -> SherpaResult<(Journal, Box<Grammar>, PathBuf, IStringStore)> {
    let mut j = Journal::new();
    j.set_active_report("test", ReportType::GrammarCompile(Default::default()));

    SherpaResult::Ok((j, super::parse_grammar(input)?, PathBuf::from("/test.sg"), IStringStore::default()))
  }
  #[test]
  fn extract_nonterminals() -> SherpaResult<()> {
    let (mut j, g, path, s_store) =
      create_test_data(r##"  <> test-sweet-home-alabama > c:id{3} | ("test"{2} "2" :ast $1 ) :ast $1 "##)?;

    let g_data = super::create_grammar_data(&mut j, g, &path, &s_store)?;

    let (mut nterms, ..) = super::extract_nonterminals(&mut j, &g_data, &s_store)?;

    assert_eq!(nterms.len(), 1);

    let nterm = super::process_nonterminal(o_to_r(nterms.pop(), "")?, &g_data, &s_store)?;

    dbg!(&nterm.symbols);

    assert_eq!(nterm.sub_nterms.len(), 1);
    assert_eq!(nterm.symbols.len(), 3);

    SherpaResult::Ok(())
  }

  #[test]
  fn process_custom_parse_state() -> SherpaResult<()> {
    let (mut j, g, path, s_store) = create_test_data(
      r##" 
      
      test-sweet-home-alabama =!> match: TERMINAL {
          ( "test" ) { reduce 2 symbols to data :ast { t_Test } then pass }
          ( "failed" ) { fail }
          ( "accepted in kind" ) { accept }
      }
      
       "##,
    )?;

    let g_data = super::create_grammar_data(&mut j, g, &path, &s_store)?;

    let (nonterminals, mut parse_states) = super::extract_nonterminals(&mut j, &g_data, &s_store)?;

    assert_eq!(nonterminals.len(), 0);
    assert_eq!(parse_states.len(), 1);

    let parse_state = super::process_parse_state(parse_states.pop().unwrap(), &g_data, &s_store)?;

    dbg!(&parse_state, &s_store);

    assert_eq!(parse_state.symbols.len(), 3);

    SherpaResult::Ok(())
  }
}
