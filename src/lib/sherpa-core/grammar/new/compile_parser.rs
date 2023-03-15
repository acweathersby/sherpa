use std::{path::PathBuf, sync::Arc};

use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
  grammar::{
    compile::parser::sherpa::Ascript,
    new::compile::compile_grammars_from_path,
  },
  tasks::{new_taskman, Executor, Spawner},
  Journal,
  ReportType,
  SherpaResult,
};

use super::{
  string::IString,
  types::{
    Array,
    GrammarIdentity,
    GrammarSoup,
    Map,
    OrderedSet,
    ProductionId,
    Queue,
    Rule,
    Set,
    SymbolId,
    TokenSymbol,
  },
};

/// Represents a fully resolved parser, which can be
/// compiled into bytecode or llvm parsers.
#[derive(Default, Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct ParserData {
  /// The name of the parser as defined by the `NAME <name>` preamble in
  /// the root grammar, or by the filename stem in the path for the root
  /// grammar.
  pub name:         IString,
  /// All productions that are `public`, that is productions that are
  /// accessible as entry points to the parser.
  pub pub_prods:    Map<ProductionId, (u16,)>,
  /// All parse states produced during the parser compile step.
  pub states:       Array<Box<ParseState>>,
  /// True if the parse states where optimized during the optimize step.
  pub optimized:    bool,
  /// A list of terminal symbol strings indexed by a symbols
  /// `parse_id`. Non-primitive symbols start at index
  /// `16`.
  pub symbols:      Array<IString>,
  /// A list of all production names that are publicly exposed, that
  /// is all productions that are reachable from entry productions and
  /// are not token productions.
  pub prod_names:   Array<IString>,
  /// A list of lists of all symbols ids a scanner expects to
  /// encounter. Can be used to aid the in hinting when the scanner
  /// fails to recognize an input.
  pub scan_sym_ids: Array<Array<u16>>,
  /// The associated AScript data for this parser.
  pub ascript:      Option<Box<AScriptStore>>,
}

pub(crate) async fn compile_parser<'a>(
  j: Journal,
  g: GrammarIdentity,
  gs: &'a GrammarSoup,
  spawner: &Spawner<SherpaResult<()>>,
) -> SherpaResult<ParserData> {
  // Gain read access to all parts of the GrammarCloud.
  // We don't want anything changing during these next steps.

  let GrammarSoup { grammar_headers, productions, string_store, .. } = gs;

  let productions = productions.read()?;
  let grammar_headers = grammar_headers.read()?;
  let root_grammar = grammar_headers.get(&g.guid)?.as_ref();

  // Build production list.

  let root_productions = &root_grammar.pub_prods;
  let mut production_map = Map::with_capacity(productions.len());
  let mut parse_rules = Array::with_capacity(32);
  let mut symbols: Map<SymbolId, TokenSymbol> = Map::new();
  let mut production_queue =
    Queue::from_iter(root_grammar.pub_prods.iter().cloned());

  while let Some(prod_id) = production_queue.pop_front() {
    if !production_map.contains_key(&prod_id) {
      production_map.insert(prod_id, production_map.len());
      let prod = productions.get(&prod_id)?;

      symbols.extend(prod.symbols.iter());

      for rule in &prod.rules {
        parse_rules.push(rule.clone());

        for (sym, _) in &rule.symbols {
          match sym {
            SymbolId::NonTerminal { id, .. } => {
              production_queue.push_back(*id);
            }
            SymbolId::NonTerminalToken { id, .. } => {}
            _ => {}
          }
        }
      }
    }
  }

  SherpaResult::Ok(ParserData { name: g.name, ..Default::default() })
}

// Extract all productions referenced by the entry production of the grammar.

// Merge any outstanding production merges to create finalized productions.

// Extract token information

// Extract all rules.

// Remap any rule that needs to be right recursive (for scanner states)

// Prepare Custom states.

// Merge all tokens into a token set.

// ---------------------------------------------

// For each production. Produce a parse graph and parse state IR.

// For each token production. Produce scanner graph and parse state IR

// For each token set in applicable parse states extract token sets, merge
// and produce scanner graphs and parse state IRs

// (invariant) Ensure 1 to 1 mappings between parse_state ids and parse state
// IR.

// (required) Run parse states through one GC pass.

// (optional) Run parse states through optimizer and additional GC passes.

// Return parse state IR as the finalized parser before lowering to
// executable code.

// --------------------------------------------

#[test]
fn build_parser() -> SherpaResult<()> {
  let grammar_soup = GrammarSoup::new();
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../grammar/json/json.sg")
    .canonicalize()
    .unwrap();

  let mut j = Journal::new(None);
  j.set_active_report("test", ReportType::Any);

  let (executor, spawner) = new_taskman(1000);

  let local_spawner = spawner.clone();
  let local_soup = grammar_soup.clone();
  let mut local_j = j.transfer();

  executor.execute(
    async move {
      let id = compile_grammars_from_path(
        local_j.transfer(),
        grammar_source_path,
        &local_soup,
        &local_spawner,
      )
      .await?;

      assert_eq!(
        local_soup
          .grammar_headers
          .read()
          .unwrap()
          .get(&id.guid)
          .unwrap()
          .pub_prods
          .len(),
        1
      );

      local_j.flush_reports();

      let parser_data =
        compile_parser(local_j.transfer(), id, &local_soup, &local_spawner)
          .await?;

      SherpaResult::Ok(())
    },
    spawner,
  )?;

  println!("{:#?}", grammar_soup);

  j.flush_reports();

  SherpaResult::Ok(())
}
