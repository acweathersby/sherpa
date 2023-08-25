use js_sys::Array;
use sherpa_bytecode::compile_bytecode;
use sherpa_core::{proxy::Map, *};
use sherpa_rust_build::build_rust;
use sherpa_rust_runtime::{
  bytecode::{disassemble_bytecode, disassemble_parse_block},
  types::bytecode::Instruction,
};
use std::path::PathBuf;
use wasm_bindgen::prelude::*;

use crate::error::PositionedErrors;

/// A Grammar Identity
#[wasm_bindgen]
pub struct JSGrammarIdentities(pub(crate) Box<GrammarIdentities>);

/// A Parser database derived from grammar defined in a JSSoup
#[wasm_bindgen]
pub struct JSParserDB(pub(crate) Box<SherpaDatabaseBuilder>);

/// Parser states generated from the compilation of parser db
#[wasm_bindgen]
pub struct JSParseStates {
  pub(crate) states: Box<SherpaParserBuilder>,
  pub num_of_states: u32,
}

/// An arbitrary collection of grammars
#[wasm_bindgen]
pub struct JSSoup(pub(crate) Box<Option<SherpaGrammarBuilder>>);

/// Bytecode produced from parse states
#[wasm_bindgen]
pub struct JSBytecode(pub(crate) Box<(Vec<u8>, Map<IString, usize>)>);

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
    if let Some(grammar) = self.0.take() {
      let grammar = grammar.remove_grammar(&path).map_err(to_err)?;

      let grammar = grammar.add_source_from_string(&grammar_source, &path).map_err(to_err)?;

      let id: GrammarIdentities = grammar.path_to_id(path);

      self.0.replace(grammar);

      Ok(JSGrammarIdentities(Box::new(id)))
    } else {
      Err((&vec![SherpaError::StaticText("Failed To Load Grammar")]).into())
    }
  }

  /// Adds a production targeting a specific grammar
  pub fn add_production(&mut self, _grammar_name: String) -> Result<(), JsError> {
    Ok(())
  }
}

/// Creates an empty grammar soup object.
/// Use soup modifiers to add grammars and productions
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
pub fn create_parser_states(js_db: &JSParserDB, optimize_states: bool) -> Result<JSParseStates, PositionedErrors> {
  let mut j = Journal::new();

  j.set_active_report("state construction", sherpa_core::ReportType::Any);

  let db = js_db.0.as_ref();

  let parser = db.build_parser().map_err(to_err)?;

  let parser = if optimize_states { parser.optimize(true).map_err(to_err)? } else { parser };

  Ok(JSParseStates {
    num_of_states: parser.get_states().len() as u32,
    states:        Box::new(parser),
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
pub fn create_bytecode(states: &JSParseStates) -> Result<JSBytecode, PositionedErrors> {
  let mut j = Journal::new();

  j.set_active_report("bytecode compile", sherpa_core::ReportType::Any);

  let SherpaResult::Ok((bc, state_lu)) = compile_bytecode(states.states.as_ref(), true) else {
    return Result::Err(convert_journal_errors(&mut j));
  };

  Ok(JSBytecode(Box::new((bc, state_lu))))
}

/// Temporary simple disassembly implementation.
#[wasm_bindgen]
pub fn create_bytecode_disassembly(bytecode: &JSBytecode) -> Result<String, PositionedErrors> {
  Ok(disassemble_bytecode(&bytecode.0 .0))
}

/// Temporary simple disassembly of a single instruction
#[wasm_bindgen]
pub fn create_instruction_disassembly(address: u32, bytecode: &JSBytecode) -> String {
  disassemble_parse_block(Some((bytecode.0 .0.as_slice(), address as usize).into()), false).0
}

/// Return a list of symbols ids if the opcode of the instruction is
/// Op::DebugExpectedSymbols
#[wasm_bindgen]
pub fn get_debug_symbol_ids(address: u32, bytecode: &JSBytecode) -> JsValue {
  let i: Instruction = (bytecode.0 .0.as_slice(), address as usize).into();

  let vec = i.get_debug_symbols();

  Array::from_iter(vec.iter().map(|i| JsValue::from_f64(*i as f64))).into()
}

/// Return a list of symbols ids if the opcode of the instruction is
/// Op::DebugExpectedSymbols
#[wasm_bindgen]
pub fn get_debug_state_name(address: u32, bytecode: &JSBytecode) -> JsValue {
  let i: Instruction = (bytecode.0 .0.as_slice(), address as usize).into();

  i.get_active_state_name().into()
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
pub fn get_debug_tok_offsets(address: u32, bytecode: &JSBytecode) -> JsValue {
  let i: Instruction = (bytecode.0 .0.as_slice(), address as usize).into();
  match i.get_debug_tok_offsets() {
    Some((start, end)) => (TokenOffsets { start, end }).into(),
    None => Default::default(),
  }
}

#[wasm_bindgen]
pub fn get_state_source_string(name: String, states: &JSParseStates) -> JsValue {
  let lu_name: IString = name.to_token();

  let parser = states.states.as_ref();

  let code = parser.get_states().iter().find(|f| f.0 == lu_name).map(|f| f.1.print(parser.get_db(), true).unwrap());

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
