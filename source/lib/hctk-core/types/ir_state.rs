use super::*;
use crate::grammar::{data::ast::IR_STATE, hash_id_value_u64, parse::compile_ir_ast};
use std::{
  collections::BTreeSet,
  fmt::{Debug, Display},
};

#[derive(PartialEq, Eq, Clone, Copy)]

pub enum PeekType {
  None,
  PeekStart,
  PeekContinue,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IRStateType {
  Undefined,
  ProductionStart,
  ProductionGoto,
  ScannerStart,
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
  pub code: String,
  pub id: String,
  pub comment: String,
  pub hash: u64,
  pub graph_id: TGNId,
  pub normal_symbols: Vec<SymbolID>,
  pub skip_symbols: Vec<SymbolID>,
  pub ast: Result<IR_STATE, HCError>,
  pub state_type: IRStateType,
  pub stack_depth: u32,
  pub peek_type: PeekType,
}

impl Default for IRState {
  fn default() -> Self {
    Self {
      state_type: IRStateType::default(),
      comment: String::default(),
      code: String::default(),
      id: String::default(),
      hash: u64::default(),
      graph_id: TGNId::default(),
      normal_symbols: Vec::default(),
      skip_symbols: Vec::default(),
      ast: Err(HCError::ir_warn_not_parsed),
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

  pub fn get_type(&self) -> IRStateType {
    self.state_type
  }

  pub fn get_stack_depth(&self) -> u32 {
    self.stack_depth
  }

  pub fn get_name(&self) -> String {
    if self.id.is_empty() {
      match self.state_type {
        IRStateType::ProductionGoto | IRStateType::ScannerGoto => {
          Self::get_state_name_from_hash(self.hash) + "_goto"
        }
        _ => Self::get_state_name_from_hash(self.hash),
      }
    } else {
      self.id.clone()
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

  pub fn get_scanner_symbol_set(&self) -> Option<BTreeSet<SymbolID>> {
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

  pub fn get_graph_id(&self) -> TGNId {
    self.graph_id
  }

  pub fn compile_ast(&mut self) -> Result<&mut IR_STATE, &mut HCError> {
    match self.ast {
      Ok(_) => self.ast.as_mut(),
      Err(HCError::ir_warn_not_parsed) => {
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
