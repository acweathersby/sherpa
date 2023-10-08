use js_sys::Array;
use sherpa_bytecode::{compile_bytecode, BytecodePackage};
use sherpa_core::*;
use sherpa_rust_build::build_rust;
use sherpa_rust_runtime::{
  kernel::{disassemble_bytecode, disassemble_parse_block},
  types::{
    bytecode::{ByteCodeIterator, Instruction, Opcode},
    TableHeaderData,
  },
};
use std::{path::PathBuf, rc::Rc};
use wasm_bindgen::prelude::*;

use crate::{error::PositionedErrors, JSParserConfig, JSParserMetrics};

/// A Grammar Identity
#[wasm_bindgen]
pub struct JSGrammarIdentities(pub(crate) Box<GrammarIdentities>);

/// A Parser database derived from grammar defined in a JSSoup
#[wasm_bindgen]
pub struct JSParserDB(pub(crate) Box<SherpaDatabaseBuilder>);

/// Parser states generated from the compilation of parser db
#[wasm_bindgen]
pub struct JSParseStates {
  pub(crate) states:  Box<SherpaParserBuilder>,
  pub parser_metrics: JSParserMetrics,
}

/// An arbitrary collection of grammars
#[wasm_bindgen]
pub struct JSSoup(pub(crate) Box<Option<SherpaGrammarBuilder>>);

/// Bytecode produced from parse states
#[wasm_bindgen]
#[derive(Clone)]
pub struct JSBytecodePackage(pub(crate) Rc<BytecodePackage>);

impl AsRef<[u8]> for JSBytecodePackage {
  fn as_ref(&self) -> &[u8] {
    self.0.as_ref().as_ref()
  }
}

fn to_err(e: SherpaError) -> PositionedErrors {
  (&vec![e]).into()
}

#[wasm_bindgen]
impl JSSoup {
  /// Adds or replaces grammar in the soup, or throws an error
  /// if the grammar is invalid. Returns the grammar
  /// id if successful.
  pub fn add_grammar(&mut self, grammar_source: String, path: String) -> Result<JSGrammarIdentities, PositionedErrors> {
    let path = &PathBuf::from(&path);
    if let Some(grammar) = self.0.as_mut() {
      grammar.add_source_from_string(&grammar_source, &path, true).map_err(to_err)?;

      let id: GrammarIdentities = grammar.path_to_id(path);

      Ok(JSGrammarIdentities(Box::new(id)))
    } else {
      Err((&vec![SherpaError::StaticText("Failed To Load Grammar")]).into())
    }
  }

  /// Adds a non-terminal targeting a specific grammar
  pub fn add_nonterminal(&mut self, _grammar_name: String) -> Result<(), JsError> {
    Ok(())
  }
}

/// Creates an empty grammar soup object.
/// Use soup modifiers to add grammars and nonterminals
///
/// Pass soup to parser compiler functions to create parsers, generate bytecode,
/// and construct ASCript AST and CST structures.
#[wasm_bindgen]
pub fn create_soup() -> Result<JSSoup, JsError> {
  Ok(JSSoup(Box::new(Some(SherpaGrammarBuilder::new()))))
}

/// Creates a parser db from a soup and a root grammar, or returns semantic
/// errors.
#[wasm_bindgen]
pub fn create_parse_db(grammar_id: String, soup: &JSSoup) -> Result<JSParserDB, PositionedErrors> {
  if let Some(grammar) = soup.0.as_ref() {
    let parser_db = grammar.build_db(&PathBuf::from(grammar_id)).map_err(to_err)?;

    Ok(JSParserDB(Box::new(parser_db)))
  } else {
    Err(Default::default())
  }
}

/// Temporary simple AST output implementation.
#[wasm_bindgen]
pub fn create_rust_ast_output(js_db: &JSParserDB) -> Result<String, PositionedErrors> {
  let mut j = Journal::new();
  j.set_active_report("ast compile", sherpa_core::ReportType::Any);

  let db = &js_db.0;

  let SherpaResult::Ok(output) = build_rust(j.transfer(), db) else {
    return Result::Err(convert_journal_errors(&mut j));
  };

  Ok(output)
}

/// Temporary simple AST output implementation.
#[wasm_bindgen]
pub fn create_parser_states(
  js_db: &JSParserDB,
  optimize_states: bool,
  config: &JSParserConfig,
) -> Result<JSParseStates, PositionedErrors> {
  let mut j = Journal::new();

  j.set_active_report("state construction", sherpa_core::ReportType::Any);

  let db = js_db.0.as_ref();

  let parser = db.build_parser(config.into()).map_err(to_err)?;

  let parser = if optimize_states { parser.optimize(true).map_err(to_err)? } else { parser };

  Ok(JSParseStates {
    parser_metrics: parser.get_meterics().into(),
    states:         Box::new(parser),
  })
}

fn convert_journal_errors(j: &mut Journal) -> PositionedErrors {
  let mut errors = PositionedErrors::default();
  j.flush_reports();
  if let Some(reports) = j.get_faulty_reports() {
    for report in &reports {
      errors.extend_from_refs(&report.errors());
    }
  }

  errors
}

/// Temporary simple disassembly implementation.
#[wasm_bindgen]
pub fn create_bytecode(states: &JSParseStates) -> Result<JSBytecodePackage, PositionedErrors> {
  let mut j = Journal::new();

  j.set_active_report("bytecode compile", sherpa_core::ReportType::Any);

  let SherpaResult::Ok(pkg) = compile_bytecode(states.states.as_ref(), true) else {
    return Result::Err(convert_journal_errors(&mut j));
  };

  Ok(JSBytecodePackage(Rc::new(pkg)))
}

/// Temporary simple disassembly implementation.
#[wasm_bindgen]
pub fn create_bytecode_disassembly(pkg: &JSBytecodePackage) -> Result<String, PositionedErrors> {
  Ok(disassemble_bytecode(&pkg.0.bytecode))
}

/// Temporary simple disassembly of a single instruction
#[wasm_bindgen]
pub fn create_instruction_disassembly(address: u32, pkg: &JSBytecodePackage) -> String {
  disassemble_parse_block(Some((pkg.0.bytecode.as_slice(), address as usize).into()), false).0
}

/// Return a list of symbols ids if the opcode of the instruction is
/// HashTable or VecTable
#[wasm_bindgen]
pub fn get_debug_symbol_ids(address: u32, pkg: &JSBytecodePackage) -> JsValue {
  let bc = pkg.0.bytecode.as_slice();
  let i: Instruction = (bc, address as usize).into();

  if i.get_opcode() == Opcode::HashBranch {
    let TableHeaderData { table_start, table_length, .. } = i.into();
    let vals = (0..table_length as usize).map(|offset| {
      let mut iter: ByteCodeIterator = (bc, table_start + offset * 4).into();
      let cell = iter.next_u32_le().unwrap();
      let value = cell & 0x7FF;
      JsValue::from_f64(value as f64)
    });
    Array::from_iter(vals).into()
  } else {
    Default::default()
  }
}

#[wasm_bindgen]
pub fn get_debug_state_name(address: u32, pkg: &JSBytecodePackage, db: &JSParserDB) -> JsValue {
  if let Some(name) = pkg.0.address_to_state_name.get(&(address)) {
    name.to_string(db.0.get_db().string_store()).into()
  } else {
    "".into()
  }
}

#[wasm_bindgen]
#[derive(Default)]
pub struct TokenOffsets {
  pub start: u32,
  pub end:   u32,
}

/// Return a list of symbols ids if the opcode of the instruction is
/// Op::DebugExpectedSymbols
#[wasm_bindgen]
pub fn get_debug_tok_offsets(address: u32, pkg: &JSBytecodePackage) -> JsValue {
  if let Some(token) = pkg.0.ir_token_lookup.get(&(address)) {
    (TokenOffsets { start: token.get_start() as u32, end: token.get_end() as u32 }).into()
  } else {
    Default::default()
  }
}

#[wasm_bindgen]
pub fn get_state_source_string(name: String, states: &JSParseStates) -> JsValue {
  let lu_name: IString = name.to_token();

  let parser = states.states.as_ref();

  let code = parser.get_states().iter().find(|f| f.0 == lu_name).map(|f| f.1.print(parser.get_db(), true).unwrap_or_default());

  code.into()
}

/// Givin an symbol index, returns the symbol's friendly name.
#[wasm_bindgen]
pub fn get_symbol_name_from_id(id: u32, db: &JSParserDB) -> JsValue {
  let db = db.0.as_ref().get_db();
  db.token(id.into()).name.to_string(db.string_store()).into()
}

/// Returns a list of entrypoint names
#[wasm_bindgen]
pub fn get_entry_names(db: &JSParserDB) -> JsValue {
  let db = db.0.as_ref().get_db();
  db.entry_points().iter().map(|ep| JsValue::from(ep.entry_name.to_string(db.string_store()))).collect::<Array>().into()
}

#[wasm_bindgen]
pub fn get_nonterminal_name_from_id(id: u32, db: &JSParserDB) -> String {
  let db = db.0.get_db();
  if (id as usize) < db.nonterms_len() {
    let id = DBNonTermKey::from(id);
    db.nonterm_friendly_name_string(id)
  } else {
    Default::default()
  }
}

#[wasm_bindgen]
pub fn get_rule_expression_string(id: u32, db: &JSParserDB) -> String {
  let db = db.0.get_db();
  if (id as usize) < db.rules().len() {
    let item = Item::from((DBRuleKey::from(id), db));
    item.to_canonical().to_complete()._debug_string_()
  } else {
    Default::default()
  }
}

#[wasm_bindgen]
#[derive(Default, Clone, Copy)]
pub enum JSReductionType {
  /// Any reduction resulting in the execution of a some kind of semantic
  /// action. At this point only `:ast` semantic actions are available.
  SemanticAction,
  /// A reduction of a terminal symbol to a nonterminal
  SingleTerminal,
  /// A reduction of single nonterminal symbol to another nonterminal
  SingleNonTerminal,
  /// A reduction of a left-recursive rule
  LeftRecursive,
  /// A reduction of more than one symbol to a nonterminal
  #[default]
  Mixed,
}

impl From<ReductionType> for JSReductionType {
  fn from(value: ReductionType) -> Self {
    match value {
      ReductionType::SemanticAction => JSReductionType::SemanticAction,
      ReductionType::SingleTerminal => JSReductionType::SingleTerminal,
      ReductionType::SingleNonTerminal => JSReductionType::SingleNonTerminal,
      ReductionType::LeftRecursive => JSReductionType::LeftRecursive,
      ReductionType::Mixed => JSReductionType::Mixed,
    }
  }
}

#[wasm_bindgen]
pub fn get_rule_reduce_type(id: u32, db: &JSParserDB) -> JSReductionType {
  let db = db.0.get_db();
  if (id as usize) < db.rules().len() {
    JSReductionType::from(db.get_reduce_type(DBRuleKey::from(id)))
  } else {
    JSReductionType::default()
  }
}
