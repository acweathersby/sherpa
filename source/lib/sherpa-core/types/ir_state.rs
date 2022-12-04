use super::*;
use crate::grammar::{data::ast::IR_STATE, hash_id_value_u64, parse::compile_ir_ast};
use std::{
  collections::BTreeSet,
  fmt::{Debug, Display},
};

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash, Eq, Ord, Default)]
/// Identifies an IR scanner state for a particular set of SymbolIds
pub struct ScannerStateId(u64);

impl ScannerStateId {
  pub fn new(symbol_set: &SymbolSet) -> Self {
    Self(hash_id_value_u64(symbol_set))
  }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash, Eq, Ord, Default)]
/// Identifies an IR state
pub struct StateId(u64);

impl StateId {
  pub fn new(state_name: &String) -> Self {
    Self(hash_id_value_u64(state_name))
  }
}

#[derive(PartialEq, Eq, Clone, Copy)]

pub(crate) enum PeekType {
  None,
  PeekStart,
  PeekContinue,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub(crate) enum BranchType {
  PRODUCTION,
  TOKEN,
  BYTE,
  CLASS,
  CODEPOINT,
  UNKNOWN,
}

impl From<String> for BranchType {
  fn from(value: String) -> Self {
    match value.as_str() {
      "PRODUCTION" => Self::PRODUCTION,
      "TOKEN" => Self::TOKEN,
      "BYTE" => Self::BYTE,
      "CLASS" => Self::CLASS,
      "CODEPOINT" => Self::CODEPOINT,
      _ => Self::UNKNOWN,
    }
  }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IRStateType {
  Undefined,
  ProductionStart,
  ProductionGoto,
  ScannerStart,
  Scanner,
  Parser,
  ScannerGoto,
  ProductionIntermediateState,
  ScannerIntermediateState,
  ForkState,
  ProductionEndState,
  ScannerEndState,
}

impl Default for IRStateType {
  fn default() -> Self {
    Self::Undefined
  }
}

pub struct IRState {
  pub(crate) code: String,
  pub(crate) name: String,
  pub(crate) comment: String,
  pub(crate) hash: u64,
  pub(crate) graph_id: NodeId,
  pub(crate) normal_symbols: Vec<SymbolID>,
  pub(crate) skip_symbols: Vec<SymbolID>,
  pub(crate) ast: Result<IR_STATE, SherpaError>,
  pub(crate) state_type: IRStateType,
  pub(crate) stack_depth: u32,
  pub(crate) peek_type: PeekType,
}

impl Default for IRState {
  fn default() -> Self {
    Self {
      state_type: IRStateType::default(),
      comment: String::default(),
      code: String::default(),
      name: String::default(),
      hash: u64::default(),
      graph_id: NodeId::default(),
      normal_symbols: Vec::default(),
      skip_symbols: Vec::default(),
      ast: Err(SherpaError::ir_warn_not_parsed),
      stack_depth: u32::default(),
      peek_type: PeekType::None,
    }
  }
}

impl IRState {
  pub fn get_state_name_from_hash(hash: u64) -> String {
    format!("s{:02x}", hash)
  }

  pub fn into_hashed(mut self) -> Self {
    self.hash = hash_id_value_u64(self.code.clone());
    self.code = self.code.replace("%%%%", &self.get_name());
    self
  }

  pub(crate) fn get_type(&self) -> IRStateType {
    self.state_type
  }

  pub fn get_stack_depth(&self) -> u32 {
    self.stack_depth
  }

  pub fn get_name(&self) -> String {
    if self.name.is_empty() {
      match self.state_type {
        IRStateType::ProductionGoto | IRStateType::ScannerGoto => {
          Self::get_state_name_from_hash(self.hash) + "_goto"
        }
        _ => Self::get_state_name_from_hash(self.hash),
      }
    } else {
      self.name.clone()
    }
  }

  pub fn get_hash(&self) -> u64 {
    self.hash
  }

  pub fn get_code(&self) -> String {
    format!("{}{}\n{}\n", self.get_state_header(), self.get_scanner_header(), self.code,)
  }

  pub fn get_comment(&self) -> &String {
    &self.comment
  }

  pub fn get_state_header(&self) -> String {
    format!("state [ {} ] \n", self.get_name())
  }

  pub fn get_scanner_header(&self) -> String {
    if let Some(name) = self.get_scanner_state_name() {
      format!(" scanner [ {} ] \n", name)
    } else {
      String::new()
    }
  }

  pub fn get_symbols(&self) -> (&Vec<SymbolID>, &Vec<SymbolID>) {
    (&self.normal_symbols, &self.skip_symbols)
  }

  pub fn get_scanner_symbol_set(&self) -> Option<SymbolSet> {
    let (norm, peek) = self.get_symbols();

    let scanner_syms = norm.iter().chain(peek.iter()).cloned().collect::<BTreeSet<_>>();

    if scanner_syms.is_empty() {
      None
    } else {
      Some(scanner_syms)
    }
  }

  pub fn get_scanner_state_name(&self) -> Option<String> {
    self.get_scanner_symbol_set().map(|symbols| format!("scan_{:02X}", hash_id_value_u64(&symbols)))
  }

  pub(crate) fn get_graph_id(&self) -> NodeId {
    self.graph_id
  }

  pub fn compile_ast(&mut self) -> Result<&mut IR_STATE, &mut SherpaError> {
    match self.ast {
      Ok(_) => self.ast.as_mut(),
      Err(SherpaError::ir_warn_not_parsed) => {
        let string = self.get_code();
        self.ast = match compile_ir_ast(Vec::from(string.as_bytes())) {
          Ok(ast) => Ok(*ast),
          Err(err) => Err(err),
        };
        self.ast.as_mut()
      }
      _ => self.ast.as_mut(),
    }
  }

  pub fn get_ast_mut(&mut self) -> Option<&mut IR_STATE> {
    if self.ast.is_ok() {
      Some(self.ast.as_mut().ok().unwrap())
    } else {
      None
    }
  }

  pub fn get_ast(&self) -> Option<&IR_STATE> {
    if self.ast.is_ok() {
      Some(self.ast.as_ref().ok().unwrap())
    } else {
      None
    }
  }

  pub fn is_scanner(&self) -> bool {
    match self.state_type {
      IRStateType::ScannerStart
      | IRStateType::ScannerGoto
      | IRStateType::Scanner
      | IRStateType::ScannerIntermediateState
      | IRStateType::ScannerEndState => true,
      _ => false,
    }
  }
}

impl Debug for IRState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "{}\\*\n {} \n*\\\n{}\n\n\n",
      self.get_state_header(),
      self.comment,
      self.code,
    ))
  }
}

impl Display for IRState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "{}\\*\n {} \n*\\\n{}\n\n\n",
      self.get_state_header(),
      self.comment,
      self.code,
    ))
  }
}
