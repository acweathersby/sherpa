use js_sys::{Array, ArrayBuffer, JsString, Number, Uint8Array};
use radlr_bytecode::compile_bytecode;
use radlr_core::{worker_pool::SingleThreadPool, RadlrDatabase, RadlrGrammar, *};
use radlr_rust_runtime::{
  kernel::{disassemble_bytecode, disassemble_parse_block},
  types::{
    bytecode::{ByteCodeIterator, Instruction, Opcode},
    BytecodeParserDB,
    RuntimeDatabase,
    TableHeaderData,
  },
};
use std::{path::PathBuf, rc::Rc};
use wasm_bindgen::prelude::*;

use crate::{
  error::{ErrorOrigin, PositionedErrors},
  JSParserClassification,
  JSParserConfig,
};

/// A Grammar Identity
#[wasm_bindgen]
pub struct JSGrammarIdentities(pub(crate) Box<GrammarIdentities>);

/// A Parser database derived from grammar defined in a JSSoup
#[wasm_bindgen]
pub struct JSParserDB(pub(crate) Box<RadlrDatabase>);

impl AsRef<RadlrDatabase> for JSParserDB {
  fn as_ref(&self) -> &RadlrDatabase {
    &self.0
  }
}

/// A Parser database derived from grammar defined in a JSSoup
#[wasm_bindgen]
pub struct JSParserGraph(pub(crate) Box<RadlrParseGraph>);

impl AsRef<RadlrParseGraph> for JSParserGraph {
  fn as_ref(&self) -> &RadlrParseGraph {
    &self.0
  }
}

/// Parser states generated from the compilation of parser db
#[wasm_bindgen]
pub struct JSIRParser {
  pub(crate) states: Box<RadlrIRParser>,
}

#[wasm_bindgen]
impl JSIRParser {
  #[wasm_bindgen(getter)]
  pub fn classification(&self) -> JSParserClassification {
    self.states.get_classification().into()
  }

  #[wasm_bindgen(getter)]
  pub fn num_of_states(&self) -> JsValue {
    (self.states.get_metrics().num_of_states as u32).into()
  }

  #[wasm_bindgen(getter)]
  pub fn optimized(&self) -> JsValue {
    (self.states.get_metrics().optimized).into()
  }
}

impl AsRef<RadlrIRParser> for JSIRParser {
  fn as_ref(&self) -> &RadlrIRParser {
    &self.states
  }
}

/// An arbitrary collection of grammars
#[wasm_bindgen]
pub struct JSRadlrGrammar(pub(crate) Box<RadlrGrammar>);

impl AsRef<RadlrGrammar> for JSRadlrGrammar {
  fn as_ref(&self) -> &RadlrGrammar {
    &self.0
  }
}

impl AsMut<RadlrGrammar> for JSRadlrGrammar {
  fn as_mut(&mut self) -> &mut RadlrGrammar {
    &mut self.0
  }
}

/// Bytecode produced from parse states
#[wasm_bindgen]
#[derive(Clone)]
pub struct JSBytecodeParserDB(pub(crate) Rc<BytecodeParserDB>);

impl AsRef<[u8]> for JSBytecodeParserDB {
  fn as_ref(&self) -> &[u8] {
    self.0.as_ref().as_ref()
  }
}

fn to_err(e: RadlrError, origin: ErrorOrigin) -> PositionedErrors {
  (&vec![e], origin).into()
}

#[wasm_bindgen]
impl JSBytecodeParserDB {
  /// Returns the bytecode of the parser as bytes.
  #[wasm_bindgen(method, getter)]
  pub fn bytecode(&self) -> Uint8Array {
    let data = &self.0.bytecode;

    let buffer = Uint8Array::new_with_length(data.len() as u32);

    buffer.copy_from(data);

    buffer
  }

  /// A list of enterble non-terminal names and their respective bytecode entry
  /// point address address
  #[wasm_bindgen(method, getter)]
  pub fn entry_points(&self) -> JsValue {
    let entry_points = Array::new();

    for (name, address) in self.0.entrypoints() {
      let pair = Array::new();
      pair.push(&JsString::from(name).into());
      pair.push(&Number::from(address).into());
      entry_points.push(&pair.into());
    }

    entry_points.into()
  }
}

#[wasm_bindgen]
impl JSRadlrGrammar {
  /// Adds or replaces grammar in the soup, or throws an error
  /// if the grammar is invalid. Returns the grammar
  /// id if successful.
  pub fn add_grammar(&mut self, grammar_source: String, path: String) -> Result<JSGrammarIdentities, PositionedErrors> {
    let path = &PathBuf::from(&path);
    let grammar = self.as_mut();

    grammar.add_source_from_string(&grammar_source, &path, true).map_err(|e| to_err(e, ErrorOrigin::Grammar))?;

    let id: GrammarIdentities = grammar.path_to_id(path);

    Ok(JSGrammarIdentities(Box::new(id)))
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
pub fn create_soup() -> Result<JSRadlrGrammar, JsError> {
  Ok(JSRadlrGrammar(Box::new(RadlrGrammar::new())))
}

/// Creates a parser db from a soup and a root grammar, or returns semantic
/// errors.
#[wasm_bindgen]
pub fn create_parse_db(
  grammar_id: String,
  soup: &JSRadlrGrammar,
  config: &JSParserConfig,
) -> Result<JSParserDB, PositionedErrors> {
  let grammar = soup.as_ref();

  let parser_db =
    grammar.build_db(&PathBuf::from(grammar_id), (*config).into()).map_err(|e| to_err(e, ErrorOrigin::ParserBuild))?;

  Ok(JSParserDB(Box::new(parser_db)))
}

/// Temporary simple AST output implementation.
#[wasm_bindgen]
pub fn create_rust_ast_output(js_db: &JSParserDB) -> Result<String, PositionedErrors> {
  let db = &js_db.0;

  Ok(String::default())

  /*   let RadlrResult::Ok(output) = build_rust(j.transfer(), db) else {
    return Result::Err(convert_journal_errors(&mut j));
  };

  Ok(output) */
}

/// Temporary simple disassembly implementation.
pub fn create_bytecode(states: &JSIRParser) -> Result<JSBytecodeParserDB, PositionedErrors> {
  match compile_bytecode(states.states.as_ref(), true) {
    Err(errors) => Result::Err(convert_journal_errors(errors)),
    Ok(pkg) => Ok(JSBytecodeParserDB(Rc::new(pkg))),
  }
}

/// Import a bytecode database from a JS ArrayBuffer
#[wasm_bindgen]
pub fn import_bytecode_db(buffer: ArrayBuffer) -> Result<JSBytecodeParserDB, PositionedErrors> {
  let buffer: Vec<u8> = Uint8Array::new(&buffer).to_vec();

  match radlr_lab::serialize::bytecode_db::import_bytecode_db(&buffer) {
    Ok(db) => Ok(JSBytecodeParserDB(Rc::new(db))),
    Err(err) => Err((RadlrError::from(err), ErrorOrigin::BytecodeImport).into()),
  }
}

/// Export a bytecode database into a JS ArrayBuffer
#[wasm_bindgen]
pub fn export_bytecode_db(states: &JSIRParser) -> Result<ArrayBuffer, PositionedErrors> {
  let bc = match compile_bytecode(states.states.as_ref(), true) {
    Err(errors) => return Result::Err(convert_journal_errors(errors)),
    Ok(pkg) => pkg,
  };

  let buffer = radlr_lab::serialize::bytecode_db::export_bytecode_db(&bc);

  let array_buffer = ArrayBuffer::new(buffer.len() as u32);

  let output = Uint8Array::new(&array_buffer);

  output.copy_from(&buffer);

  Ok(array_buffer)
}

fn read_primitive_at_offset<T: Copy + Default>(buffer: &[u8], offset: &mut usize) -> T {
  unsafe {
    let size: usize = size_of::<T>();
    let data: T = Default::default();
    let bytes: *mut u8 = std::mem::transmute(&data);
    buffer.as_ptr().offset(*offset as isize).copy_to(bytes, size);
    *offset += size;
    data
  }
}

///
#[wasm_bindgen]
pub fn create_parser_states(
  js_db: &JSParserDB,
  optimize_states: bool,
  config: &JSParserConfig,
) -> Result<JSIRParser, PositionedErrors> {
  let pool = SingleThreadPool {};

  let db = js_db.as_ref();

  let parser = db.build_states((*config).into(), &pool).map_err(|e| to_err(e, ErrorOrigin::StatesCreation))?;

  let parser =
    (if optimize_states { parser.build_ir_parser(true, true, &pool) } else { parser.build_ir_parser(false, true, &pool) })
      .map_err(|e| to_err(e, ErrorOrigin::StatesCreation))?;

  Ok(JSIRParser { states: Box::new(parser) })
}

fn convert_journal_errors(in_errors: RadlrError) -> PositionedErrors {
  let mut errors = PositionedErrors::default();

  errors.extend_from_refs(&in_errors.flatten(), ErrorOrigin::StatesCreation);

  errors
}

/// Temporary simple disassembly implementation.
#[wasm_bindgen]
pub fn create_bytecode_disassembly(pkg: &JSBytecodeParserDB) -> Result<String, PositionedErrors> {
  Ok(disassemble_bytecode(&pkg.0.bytecode))
}

/// Temporary simple disassembly of a single instruction
#[wasm_bindgen]
pub fn create_instruction_disassembly(address: u32, pkg: &JSBytecodeParserDB) -> String {
  disassemble_parse_block(Some((pkg.0.bytecode.as_slice(), address as usize).into()), false).0
}

/// Return a list of symbols ids if the opcode of the instruction is
/// HashTable or VecTable
#[wasm_bindgen]
pub fn get_debug_symbol_ids(address: u32, pkg: &JSBytecodeParserDB) -> JsValue {
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
pub fn get_debug_state_name(address: u32, pkg: &JSBytecodeParserDB, db: &JSParserDB) -> JsValue {
  let bc_db = pkg.0.clone();

  if let Some(name) = bc_db.address_to_state_name.get(&(address)) {
    name.to_string().into()
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
pub fn get_debug_tok_offsets(address: u32, pkg: &JSBytecodeParserDB) -> JsValue {
  if let Some(token) = pkg.0.ir_token_lookup.get(&(address)) {
    (TokenOffsets { start: token.get_start() as u32, end: token.get_end() as u32 }).into()
  } else {
    Default::default()
  }
}

#[wasm_bindgen]
pub fn get_state_source_string(name: String, states: &JSIRParser) -> JsValue {
  let lu_name: IString = name.to_token();

  let parser = states.states.as_ref();

  let code = parser.get_states().find(|f| f.0 == lu_name).map(|f| f.1.print(parser.get_db(), true).unwrap_or_default());

  code.into()
}

/// Givin an symbol index, returns the symbol's friendly name.
#[wasm_bindgen]
pub fn get_symbol_name_from_id(id: u32, db: &JSParserDB) -> JsValue {
  let db = db.as_ref().get_internal();
  db.token(id.into()).name.to_string(db.string_store()).into()
}

/// Returns a list of entrypoint names
#[wasm_bindgen]
pub fn get_entry_names(db: &JSParserDB) -> JsValue {
  let db = db.as_ref().get_internal();
  db.entry_points().iter().map(|ep| JsValue::from(ep.entry_name.to_string(db.string_store()))).collect::<Array>().into()
}

#[wasm_bindgen]
pub fn get_nonterminal_name_from_id(id: u32, db: &JSParserDB) -> String {
  let db = db.as_ref().get_internal();
  if (id as usize) < db.nonterms_len() {
    let id = DBNonTermKey::from(id);
    db.nonterm_friendly_name_string(id)
  } else {
    Default::default()
  }
}

#[wasm_bindgen]
pub fn get_nonterminal_names_from_db(db: &JSParserDB) -> JsValue {
  let db = db.as_ref().get_internal();

  let array = Array::new();

  for non_term_key in db.parser_nonterms() {
    let non_term = db.nonterm_friendly_name(non_term_key);
    array.push(&non_term.to_str(db.string_store()).as_str().into());
  }

  return array.into();
}

/// Returns the offset and length of a token rule.
#[wasm_bindgen]
pub fn get_rule_location(rule_id: u32, db: &JSParserDB) -> JsValue {
  let db = db.as_ref().get_internal();

  if (rule_id as usize) < db.rules().len() {
    let rule: &Rule = db.rule(DBRuleKey::from(rule_id));

    let tok = &rule.tok;

    let array = Array::new();

    array.push(&JsValue::from_f64(tok.get_start() as f64));
    array.push(&JsValue::from_f64(tok.get_end() as f64));

    array.into()
  } else {
    Default::default()
  }
}

/// Returns a diagram of a grammar rule  
#[wasm_bindgen]
pub fn get_rule_expression_string(rule_id: u32, db: &JSParserDB) -> String {
  let db = db.as_ref().get_internal();

  if (rule_id as usize) < db.rules().len() {
    let item = Item::from((DBRuleKey::from(rule_id), db));
    rule_expression(item.to_canonical().to_complete(), db)
  } else {
    Default::default()
  }
}

pub fn rule_expression(item: Item, db: &ParserDatabase) -> String {
  if item.is_null() {
    "null".to_string()
  } else {
    let mut string = String::new();

    let s_store = db.string_store();

    string += &item.nonterm_name(db).to_string(s_store);

    string += " >";

    let mut init_item = Some(item.to_initial());

    while let Some(i) = init_item.clone() {
      if i.is_complete() {
        break;
      };

      string += " ";

      string += &debug_string(&i.sym_id(db), db);

      if !item.is_canonical() {
        string += &match (i.symbol_precedence(db), i.token_precedence(db)) {
          (0, 0) => String::default(),
          (sym, 0) => "{".to_string() + &sym.to_string() + "}",
          (0, tok) => "{:".to_string() + &tok.to_string() + "}",
          (sym, tok) => "{".to_string() + &sym.to_string() + ":" + &tok.to_string() + "}",
        };
      }

      init_item = i.increment();
    }

    string.replace("\n", "\\n")
  }
}

pub fn debug_string(sym: &SymbolId, db: &ParserDatabase) -> String {
  use SymbolId::*;
  let mut w = CodeWriter::new(vec![]);
  match *sym {
    Undefined => &mut w + "Undefine",
    Default => &mut w + "Default",
    EndOfFile { .. } => &mut w + "'$'",
    ClassSpace => &mut w + "'\\s'",
    ClassHorizontalTab => &mut w + "'\\t'",
    ClassNewLine => &mut w + "'\\n'",
    ClassIdentifier => &mut w + "c:id",
    ClassNumber => &mut w + "'\\d'",
    ClassSymbol => &mut w + "c:sym",
    Any => &mut w + "'.'",
    Token { val } => &mut w + "'" + val.to_str(db.string_store()).as_str() + "'",
    NonTerminalState { .. } => &mut w + "nonterm_state",
    NonTerminal { .. } => &mut w + "nonterm",
    NonTerminalToken { .. } => &mut w + "tk:" + "nonterm",
    Codepoint { val } => &mut w + "" + val.to_string(),
    DBNonTerminal { key } => {
      let guard_str = db.nonterm_friendly_name_string(key);
      let name = guard_str.as_str();
      &mut w + name
    }
    DBNonTerminalToken { nonterm_key: nterm_key, .. } => {
      let guard_str = db.nonterm_friendly_name_string(nterm_key);
      &mut w + "tk:" + guard_str
    }
    DBToken { key: index } => &mut w + db.sym(index).debug_string(db),
    Char { char } => {
      if char < 128 {
        &mut w + "'" + char::from(char).to_string() + "'"
      } else {
        &mut w + "[ byte:" + char.to_string() + "]"
      }
    }
  };
  w.to_string()
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
  let db = db.as_ref().get_internal();
  if (id as usize) < db.rules().len() {
    JSReductionType::from(db.get_reduce_type(DBRuleKey::from(id)))
  } else {
    JSReductionType::default()
  }
}
