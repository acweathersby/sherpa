#![allow(unused)]
use super::{
  parser::{
    ast::escaped_from,
    sherpa_bc::GetASTNodeType,
    ASTNode,
    AST_NamedReference,
    AST_Statements,
    AST_Vector,
    AppendProduction,
    Ascript,
    Export,
    Grammar,
    List_Production,
    Production_Symbol,
    Production_Terminal_Symbol,
    Rule,
    State,
  },
  utils::{get_grammar_info_from_symbol, get_symbol_details, SymbolData},
};
use crate::{
  ascript::types::{ascript_first_node_id, ascript_last_node_id},
  compile::{GrammarRef, GrammarStore, ProductionId},
  grammar::{
    create_scanner_name,
    new::{
      errors::{add_invalid_import_source_error, add_production_redefinition_error},
      parser::ast::type_eval_from,
      utils::{
        create_production_guid_name,
        get_productions_names_from_ast_node,
        resolve_grammar_path,
      },
    },
  },
  types::{self, ImportedGrammarReferences, RuleId, RuleSymbol, StringId},
  Journal,
  SherpaResult,
};
use sherpa_runtime::types::Token;
use std::{collections::HashMap, path::PathBuf, rc::Rc, sync::Arc};
use types::SymbolID;

/// Loads grammar and creates initial store. Also
/// extracts imports
pub(super) fn pre_load(
  j: &mut Journal,
  string_data: &str,
  source_dir: &PathBuf,
) -> SherpaResult<((Box<GrammarStore>, Box<Grammar>))> {
  let grammar = parse_grammar(string_data)?;

  dbg!(&grammar);

  let mut store = build_store(j, &grammar, source_dir);

  store.imports = extract_imports(j, &grammar, source_dir);

  SherpaResult::Ok((store, grammar))
}

/// Completes the process of building out the GrammarStore,
/// Including build Productions, Rules, Symbols, and custom
/// ParseStates.
pub(super) fn load(
  j: &mut Journal,
  store: &mut GrammarStore,
  grammar: &Grammar,
) -> SherpaResult<()> {
  let (rules, states, append_productions) = extract_prods(j, store, &grammar);

  for (prod_id, _, rule) in rules {
    let mut index = 0;
    for rule in process_rule(
      j,
      store,
      prod_id,
      &rule.symbols.iter().enumerate().collect::<Vec<_>>(),
      &mut index,
      &rule.tok,
      &rule.ast,
    )? {
      insert_rule(rule, store, prod_id);
    }
  }

  // Process states
  if (states.len() > 0) {
    todo!("Process custom states")
  }

  // Process exports and ignore
  extract_exports_and_ignore(grammar, j, store);

  SherpaResult::Ok(())
}

fn extract_exports_and_ignore(grammar: &Grammar, j: &mut Journal, store: &mut GrammarStore) {
  let mut global_ignore_symbols = vec![];

  for amble in &grammar.preamble {
    match amble {
      ASTNode::Export(box Export { production, reference }) => {
        let id = get_prod_id(j, store, production);

        if let Some(g) = get_grammar_info_from_symbol(&store, production) {
          store.exports.push((id, g.guid, reference.to_string()));
        }
      }
      ASTNode::Ignore(box ignore) => {
        for symbol in &ignore.symbols {
          if let Some(sym_id) = record_symbol(j, store, &symbol, false) {
            global_ignore_symbols.push(sym_id);
          }
        }
      }
      _ => {}
    }

    if store.exports.is_empty() {
      // Add the first production as the export for the grammar
      let production = &grammar.productions[0];
      let id = get_prod_id(j, store, production);
      if let Some(g) = get_grammar_info_from_symbol(&store, production) {
        store.exports.push((id, g.guid, "default".into()));
      }
    }
  }

  store.production_ignore_symbols = store
    .productions
    .keys()
    .map(|k| (*k, global_ignore_symbols.clone()))
    .collect::<HashMap<_, _>>();
}

/// Parse string data and do an initial preparation of the grammar store.

pub(super) fn parse_grammar(string_data: &str) -> SherpaResult<Box<Grammar>> {
  SherpaResult::Ok(Grammar::from_str(string_data)?)
}

/// Extract import data from a parsed grammar and resolves
/// path references to actual files. Expects `source_dir` to
/// be the parent directory of the source grammar.
///
/// Invalid import errors will be added to the current report if any of
/// the import preambles fail to resolve to actual files.
pub(super) fn extract_imports(
  j: &mut Journal,
  grammar: &Grammar,
  source_dir: &PathBuf,
) -> (ImportedGrammarReferences) {
  debug_assert!(source_dir.is_dir());

  use super::parser::sherpa_bc::*;
  const allowed_extensions: [&str; 3] = ["sg", "sherpa", "hcg"];

  grammar
    .preamble
    .iter()
    .filter_map(|preamble| match preamble {
      ASTNode::Import(box import) => Some(import),
      _ => None,
    })
    .filter_map(|import| {
      let Import { reference, uri, tok } = import;
      let import_path = PathBuf::from(uri);
      match resolve_grammar_path(&import_path, &source_dir, &allowed_extensions) {
        SherpaResult::Ok(path) => {
          Some((reference.clone(), GrammarRef::new(reference.to_string(), path.clone())))
        }
        _ => {
          add_invalid_import_source_error(j, import, &import_path, &source_dir);
          None
        }
      }
    })
    .collect()
}

/// Builds a grammar store, initializing `imports` and `id` properties
/// and setting all other properties to their default values.
///
/// Invalid import errors will be added to the current report if any of
/// the import preambles fail to resolve to actual files.
pub(super) fn build_store(
  j: &mut Journal,
  grammar: &super::parser::Grammar,
  source_dir: &PathBuf,
) -> Box<GrammarStore> {
  Box::new(GrammarStore {
    id: GrammarRef::new(get_grammar_name(grammar, &source_dir), source_dir.clone()),
    imports: extract_imports(j, grammar, source_dir),
    ..Default::default()
  })
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub enum ProductionType {
  Invalid,
  Pratt,
  Peg,
  ContextFree,
  ParseState,
}

impl From<&ASTNode> for ProductionType {
  fn from(value: &ASTNode) -> Self {
    use super::parser::ASTNodeType::*;
    use ProductionType::*;
    match value.get_type() {
      CFProduction => ContextFree,
      PrattProduction => Pratt,
      PegProduction => Peg,
      State => ParseState,
      _ => Invalid,
    }
  }
}

/// Returns a mapping of all productions and their initial rules.
fn extract_prods<'a>(
  j: &mut Journal,
  store: &mut GrammarStore,
  grammar: &'a super::parser::Grammar,
) -> (
  Vec<(ProductionId, ProductionType, &'a Rule)>,
  Vec<(ProductionId, &'a State)>,
  Vec<(ProductionId, Arc<GrammarRef>, &'a AppendProduction)>,
) {
  let g_id = store.id.clone();

  use super::parser::sherpa_bc::*;

  let mut rules_to_process = vec![];
  let mut custom_parse_state = vec![];
  let mut append_productions = vec![];

  for item in grammar.productions.iter() {
    if let Some((plain_name, loc, rules)) = match item {
      ASTNode::PrattProduction(box PrattProduction { name_sym, rules, tok })
      | ASTNode::CFProduction(box CFProduction { name_sym, rules, tok })
      | ASTNode::PegProduction(box PegProduction { name_sym, rules, tok }) => {
        Some((&name_sym.name, tok.clone(), Some(rules)))
      }
      ASTNode::State(state) => {
        let State { id, tok, .. } = state.as_ref();
        Some((&id.name, tok.clone(), None))
      }
      ASTNode::AppendProduction(append_production) => {
        let AppendProduction { name_sym, .. } = append_production.as_ref();
        if let Some((guid_name, _)) = get_productions_names_from_ast_node(j, store, name_sym) {
          let local_import_grammar_name = &name_sym.as_Production_Import_Symbol().unwrap().module;
          let id = store.imports.get(local_import_grammar_name).unwrap().clone();
          append_productions.push((ProductionId::from(&guid_name), id, append_production.as_ref()))
        }
        None
      }
      _ => None,
    } {
      if let Some(id) = insert_production(j, store, plain_name, loc) {
        if let Some(rules) = rules {
          for rule in rules {
            rules_to_process.push((id, ProductionType::from(item), rule.as_ref()));
          }
        }
      }
    }
  }

  (rules_to_process, custom_parse_state, append_productions)
}

/// Creates and inserts a production into the store. Adds error
/// to report if there exist a production with a matching name.
fn insert_production(
  j: &mut Journal,
  store: &mut GrammarStore,
  plain_name: &str,
  loc: Token,
) -> Option<ProductionId> {
  let guid_name = create_production_guid_name(&store.id.guid_name, plain_name);
  let id = ProductionId::from(&guid_name);
  match store.productions.insert(id, types::Production {
    id,
    g_id: store.id.clone(),
    guid_name,
    name: plain_name.into(),
    loc: loc.clone(),
    sym_id: SymbolID::Production(id, store.id.guid),
    ..Default::default()
  }) {
    Some(val) => {
      add_production_redefinition_error(
        j,
        &store.id.path,
        val.loc.clone(),
        loc.clone(),
        plain_name,
      );
      None
    }
    None => Some(id),
  }
}

fn process_rule(
  j: &mut Journal,
  store: &mut GrammarStore,
  prod_id: ProductionId,
  symbols: &[(usize, &ASTNode)],
  list_index: &mut u32,
  token: &Token,
  ast: &Option<Box<Ascript>>,
) -> SherpaResult<Vec<types::Rule>> {
  let mut rules: Vec<types::Rule> = vec![types::Rule {
    //ast_definition: ast.to_owned(),
    tok: token.clone(),
    prod_id,
    g_id: store.id.clone(),
    ..Default::default()
  }];

  for (index, sym) in symbols.iter() {
    // Create a copy of the rules bodies to serve as
    // the original rule set. We'll be adding new rules
    // as we convert optional symbols into a rule split
    // with the original rule and another rule without the
    // optional symbol.
    let original_bodies = 0..rules.len();

    let SymbolData {
      annotation,
      is_list,
      is_group,
      is_optional,
      is_shift_nothing,
      precedence,
      sym_atom,
      is_eof,
    } = get_symbol_details(sym);

    let generated_symbol;
    let mut sym = sym_atom.unwrap();

    is_optional.then(|| rules.append(&mut rules.clone()));
    match sym {
      ASTNode::AnyGroup(group) => {
        // Ensure all permutations of optional members leave
        // a set containing at least one symbol.

        let mut pending_rules = vec![];

        fn get_index_permutations(indexes: Vec<usize>) -> Vec<Vec<usize>> {
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

        let indices = group.symbols.iter().enumerate().map(|(i, _)| i).collect();

        let candidate_symbols =
          group.symbols.iter().enumerate().map(|(i, s)| (i + index, s)).collect::<Vec<_>>();

        for permutation in
          if group.unordered { get_index_permutations(indices) } else { vec![indices] }
        {
          let symbols = permutation.iter().map(|i| candidate_symbols[*i]).collect::<Vec<_>>();
          pending_rules
            .append(&mut process_rule(j, store, prod_id, &symbols, list_index, token, &None)?);
        }

        let mut new_bodies = vec![];

        for pending_rule in pending_rules {
          for rule in &mut rules[original_bodies.clone()] {
            let mut new_body = rule.clone();
            new_body.syms.extend(pending_rule.syms.iter().cloned());
            new_bodies.push(new_body)
          }
        }

        rules.splice(original_bodies, new_bodies);

        continue;
      }
      ASTNode::Group_Production(group) => {
        // All bodies are plain without annotations or functions
        if annotation.is_empty() && !some_rules_have_ast_definitions(&group.rules) {
          // For each rule in the group clone the existing rule lists and
          // process each list independently, inserting the new symbols
          // into the existing bodies. We must make sure the indices are
          // preserved since only the last symbol in each rule can be bound
          // to the index of the group production symbol.

          let mut pending_rules = vec![];

          for rule in &group.rules {
            for mut store_rule in process_rule(
              j,
              store,
              prod_id,
              &map_symbols_to_unindexed(&rule.symbols),
              list_index,
              &rule.tok,
              &rule.ast,
            )? {
              // The last symbol in each of these new bodies is set
              // with the original symbol id
              if store_rule.syms.is_empty() {
                dbg!(&rule.symbols);
              }
              store_rule.syms.last_mut()?.original_index = *index as u32;
              pending_rules.push(store_rule);
            }
          }

          let mut new_bodies = vec![];

          for pending_body in pending_rules {
            for rule in &mut rules[original_bodies.clone()] {
              let mut new_body = rule.clone();
              new_body.syms.extend(pending_body.syms.iter().cloned());
              new_bodies.push(new_body)
            }
          }

          rules.splice(original_bodies, new_bodies);

          // We do not to process the existing symbol as it is
          // now replaced with its component rule symbols,
          // so we'll skip the rest of the loop
          continue;
        } else {
          let group_prod_name =
            &(store.productions.get(&prod_id)?.name.to_owned() + "_group_" + &index.to_string());

          let id = insert_production(j, store, group_prod_name, group.tok.clone())?;

          for rule in &group.rules {
            for mut rule in process_rule(
              j,
              store,
              id,
              &map_symbols_to_unindexed(&rule.symbols),
              list_index,
              token,
              &rule.ast,
            )? {
              insert_rule(rule, store, prod_id);
            }
          }
        }
      }
      ASTNode::List_Production(box List_Production { symbol, terminal_symbol, tok, .. }) => {
        let list_prod_name =
          &(store.productions.get(&prod_id)?.name.to_owned() + "_list_" + &index.to_string());

        let id = insert_production(j, store, list_prod_name, tok.clone())?;

        let ast_a = Some(Box::new(Ascript::new(
          ASTNode::AST_Statements(Box::new(AST_Statements::new(
            vec![ASTNode::AST_Vector(Box::new(AST_Vector::new(
              vec![ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
                value: ascript_first_node_id.to_string(),
                tok:   tok.clone(),
              }))],
              tok.clone(),
            )))],
            tok.clone(),
          ))),
          tok.clone(),
        )));

        let ast_b = Some(Box::new(Ascript::new(
          ASTNode::AST_Statements(Box::new(AST_Statements::new(
            vec![ASTNode::AST_Vector(Box::new(AST_Vector::new(
              vec![
                ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
                  value: ascript_first_node_id.to_string(),
                  tok:   tok.clone(),
                })),
                ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
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

        let mut rule_syms = vec![symbol.clone()];

        for rule in
          process_rule(j, store, id, &vec![(9999, symbol)], list_index, &sym.to_token(), &ast_a)?
        {
          insert_rule(rule, store, prod_id);
        }

        let prod_sym = ASTNode::Production_Symbol(Box::new(Production_Symbol::new(
          store.get_production(&id)?.name.clone(),
          token.clone(),
        )));
        rule_syms.insert(0, prod_sym.clone());
        if let Some(terminal_symbol) = terminal_symbol {
          rule_syms.insert(0, terminal_symbol.clone());
        }

        for rule in process_rule(
          j,
          store,
          id,
          &map_symbols_to_unindexed(&rule_syms),
          list_index,
          &sym.to_token(),
          &ast_b,
        )? {
          insert_rule(rule, store, prod_id);
        }

        (*list_index) += 1;

        generated_symbol = prod_sym;

        sym = &generated_symbol;
      }
      _ => {}
    }

    if let Some(id) = record_symbol(j, store, sym, precedence > 0) {
      let sym = RuleSymbol {
        original_index: if is_eof { u32::MAX } else { *index as u32 },
        sym_id: id,
        annotation: annotation.clone(),
        consumable: !is_shift_nothing,
        precedence: precedence,
        tok: sym.to_token(),
        g_id: store.id.clone(),
        ..Default::default()
      };
      for rule in &mut rules[original_bodies] {
        rule.syms.push(sym.clone());
      }
    }
  }

  SherpaResult::Ok(rules)
}

fn map_symbols_to_unindexed(ast_syms: &Vec<ASTNode>) -> Vec<(usize, &ASTNode)> {
  ast_syms.iter().map(|s| (9999, s)).collect::<Vec<_>>()
}

fn insert_rule(mut rule: types::Rule, store: &mut GrammarStore, prod_id: ProductionId) {
  let id = RuleId::from_syms(&rule.syms.iter().map(|s| s.sym_id).collect::<Vec<_>>());
  rule.id = id;
  store.production_rules.entry(prod_id).or_default().push(id);
  store.rules.insert(id, rule);
}

fn some_rules_have_ast_definitions(rules: &[Box<Rule>]) -> bool {
  rules.iter().any(|rule| rule.ast.is_some())
}

fn get_grammar_name(ast: &super::parser::Grammar, source_path: &PathBuf) -> String {
  use super::parser::sherpa_bc::*;
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

/// Returns an appropriate SymbolID::Defined* based on the input
/// string
#[inline]
pub fn get_terminal_id(string: &String, exclusive: bool) -> SymbolID {
  use super::parser::ASTNodeType::*;
  use SymbolID::*;
  if let Ok(node) = type_eval_from(string.into()) {
    match node.get_type() {
      DEFINED_TYPE_IDENT => DefinedIdentifier(StringId::from(string)),
      DEFINED_TYPE_NUM => DefinedNumeric(StringId::from(string)),
      _ => DefinedSymbol(StringId::from(string)),
    }
  } else {
    match exclusive {
      true => ExclusiveDefinedSymbol(StringId::from(string)),
      false => DefinedSymbol(StringId::from(string)),
    }
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
      record_symbol(j, g, &annotated.symbol, annotated.precedence.is_some())
    }
    ASTNode::TerminalToken(box terminal) => {
      let old = &terminal.val;

      let string = escaped_from(old.into()).unwrap().join("");

      let sym_id = get_terminal_id(&string, terminal.is_exclusive | exclusive);

      if let std::collections::btree_map::Entry::Vacant(e) = g.symbols.entry(sym_id) {
        g.symbol_strings.insert(sym_id, string.clone());

        let byte_length = string.bytes().len() as u32;
        let code_point_length = string.chars().count() as u32;

        e.insert(types::Symbol {
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
      process_production_symbol(j, g, sym_node)
    }
    ASTNode::EOFSymbol(_) => Some(SymbolID::EndOfFile),
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
    _ => unreachable!(),
  }
}

fn process_production_symbol(
  j: &mut Journal,
  g: &mut GrammarStore,
  node: &ASTNode,
) -> Option<SymbolID> {
  match node {
    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      let production_id = get_prod_id(j, g, node);

      match get_grammar_info_from_symbol(g, node)
        .map(|data| SymbolID::Production(production_id, data.guid))
      {
        Some(id) => {
          g.production_symbols.insert(id, node.to_token());
          Some(id)
        }
        _ => None,
      }
    }
    _ => unreachable!(),
  }
}

/// Create a TokenProduction symbol and intern the symbol info.
fn process_token_production(
  j: &mut Journal,
  g: &mut GrammarStore,
  node: &Production_Terminal_Symbol,
) -> Option<SymbolID> {
  let tok = &node.tok;
  match process_production_symbol(j, g, &node.production) {
    Some(SymbolID::Production(prod_id, grammar_id)) => {
      let scanner_prod_id = ProductionId::from(&create_scanner_name(prod_id, grammar_id));
      let guid = SymbolID::Production(scanner_prod_id, grammar_id);
      let tok_id = SymbolID::TokenProduction(prod_id, grammar_id, scanner_prod_id);

      g.production_symbols.insert(tok_id, tok.clone());

      g.symbols.entry(tok_id).or_insert(types::Symbol {
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
  get_prod_id_from_node(j, g, production).0
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
fn get_prod_id_from_node(
  j: &mut Journal,
  g: &mut GrammarStore,
  node: &ASTNode,
) -> (ProductionId, String, String) {
  match get_productions_names_from_ast_node(j, g, node) {
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

#[cfg(test)]
mod test {
  use std::path::PathBuf;

  use crate::{Journal, SherpaResult};

  #[test]
  fn parse_grammar() {
    assert!(super::parse_grammar("<> test > c:id").is_ok());
    assert!(super::parse_grammar("<> test | c:id").is_faulty());
  }

  #[test]
  fn extract_imports() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    j.set_active_report("test", crate::ReportType::GrammarCompile(Default::default()));
    let grammar = super::parse_grammar(
      " 
    IMPORT ../load.sg as name
    IMPORT ./missing_production_definition.sg as other
    <> test > c:id",
    )?;

    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/errors/")
      .canonicalize()
      .unwrap();

    let imports = super::extract_imports(&mut j, &grammar, &dir);

    assert!(imports.contains_key("name"));
    assert!(imports.contains_key("other"));

    SherpaResult::Ok(())
  }

  #[test]
  fn extract_productions() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    j.set_active_report("test", crate::ReportType::GrammarCompile(Default::default()));

    let grammar = super::parse_grammar("  <> test > c:id | \"test\"")?;

    let dir = PathBuf::from("/");

    let mut store = super::build_store(&mut j, &grammar, &dir);

    let (rules, ..) = super::extract_prods(&mut j, &mut store, &grammar);

    assert_eq!(rules.len(), 2);
    assert_eq!(grammar.productions.len(), 1);

    SherpaResult::Ok(())
  }

  #[test]
  fn production_redefinition_error() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    j.set_active_report("test", crate::ReportType::GrammarCompile(Default::default()));

    let grammar = super::parse_grammar("  <> test > c:id  #> test > c:id | c:sym ")?;

    let dir = PathBuf::from("/");

    let mut store = super::build_store(&mut j, &grammar, &dir);

    let (..) = super::extract_prods(&mut j, &mut store, &grammar);

    j.flush_reports();

    assert!(j.debug_error_report());

    SherpaResult::Ok(())
  }

  #[test]
  fn production_redefinition_error_with_parse_state() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    j.set_active_report("test", crate::ReportType::GrammarCompile(Default::default()));

    let grammar = super::parse_grammar("  <> test > c:id  test => pass ")?;

    let dir = PathBuf::from("/");

    let mut store = super::build_store(&mut j, &grammar, &dir);

    let (..) = super::extract_prods(&mut j, &mut store, &grammar);

    j.flush_reports();

    assert!(j.debug_error_report());

    SherpaResult::Ok(())
  }

  #[test]
  fn processes_rules() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    j.set_active_report("test", crate::ReportType::GrammarCompile(Default::default()));
    let grammar = super::parse_grammar(
      r##"
<> test > ( c:id | "object" ) :ast u32(tok)
  "##,
    )?;

    let dir = PathBuf::from("/");
    let mut store = super::build_store(&mut j, &grammar, &dir);
    let (pending_rules, ..) = super::extract_prods(&mut j, &mut store, &grammar);
    j.flush_reports();

    assert!(!j.debug_error_report());

    let (prod_id, _, rule) = pending_rules[0];
    let mut index = 0;
    let rules = super::process_rule(
      &mut j,
      &mut store,
      prod_id,
      &rule.symbols.iter().enumerate().collect::<Vec<_>>(),
      &mut index,
      &rule.tok,
      &rule.ast,
    )?;

    assert_eq!(rules.len(), 2);

    SherpaResult::Ok(())
  }
}
