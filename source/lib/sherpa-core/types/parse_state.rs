use super::*;
use crate::grammar::{
  compile::{compile_ir_ast, parser::sherpa::IR_STATE},
  hash_id_value_u64,
  hash_values,
};
use std::{
  collections::BTreeSet,
  fmt::{Debug, Display},
  hash::Hash,
};

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash, Eq, Ord, Default)]
/// Identifies an IR scanner state for a particular set of SymbolIds
pub struct ScannerStateId(u64);

impl ScannerStateId {
  /// TODO: Docs
  pub fn new(symbol_set: &SymbolSet) -> Self {
    Self(hash_id_value_u64(symbol_set))
  }

  /// Create a state id from a set of symbol strings separated by `~~`
  pub fn from_string(name: &str, g: &GrammarStore) -> Self {
    let symbols =
      name.split("~~").map(|s| SymbolID::from_string(s, Some(g))).collect::<SymbolSet>();
    println!(
      "{:?} : {}",
      ScannerStateId::new(&symbols),
      symbols
        .iter()
        .map(|s| { format!("{:?}{}", s, s.debug_string(g)) })
        .collect::<Vec<_>>()
        .join("  ")
    );
    Self::new(&symbols)
  }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash, Eq, Ord, Default)]
/// Identifies an IR state
pub struct StateId(u64);

impl StateId {
  /// TODO: Docs
  pub fn _new(state_name: &String) -> Self {
    Self(hash_id_value_u64(state_name))
  }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum BranchType {
  PRODUCTION,
  TOKEN,
  BYTE,
  CLASS,
  CODEPOINT,
  UNKNOWN,
}

impl From<&str> for BranchType {
  fn from(value: &str) -> Self {
    match value {
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

#[derive(Clone)]
/// Wrapper for the intermediate parser language code and AST
pub struct ParseState {
  /// The intermediate representation parse state code as written in ir.rs
  pub(crate) code: String,
  pub(crate) name: String,
  pub(crate) comment: String,
  pub(crate) hash: u64,
  pub(crate) normal_symbols: Vec<SymbolID>,
  pub(crate) skip_symbols: Vec<SymbolID>,
  pub(crate) ast: SherpaResult<IR_STATE>,
  pub(crate) state_type: IRStateType,
}

impl Default for ParseState {
  fn default() -> Self {
    Self {
      state_type: IRStateType::default(),
      comment: String::default(),
      code: String::default(),
      name: String::default(),
      hash: u64::default(),
      normal_symbols: Vec::default(),
      skip_symbols: Vec::default(),
      ast: SherpaResult::None,
    }
  }
}

impl ParseState {
  pub fn get_state_name_from_hash(hash: u64) -> String {
    format!("s{:02x}", hash)
  }

  pub fn into_hashed(mut self) -> Self {
    self.hash = hash_values(&[
      &|h| {
        self.code.hash(h);
      },
      &|h| {
        &self.normal_symbols.hash(h);
      },
      &|h| {
        &self.skip_symbols.hash(h);
      },
    ]);
    self
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
    format!(
      "{}{}\n{}\n",
      self.get_state_header(),
      self.get_scanner_header(),
      self.code.replace("%%%%", &self.get_name())
    )
  }

  pub fn get_code_body(&self) -> String {
    format!("{}\n{}\n", self.get_scanner_header(), self.code.replace("%%%%", &self.get_name()))
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

  pub fn compile_ast(&self) -> SherpaResult<IR_STATE> {
    let code = self.get_code();
    compile_ir_ast(&code)
  }

  pub fn get_cached_ast(&mut self) -> SherpaResult<&mut IR_STATE> {
    if self.ast.is_none() {
      self.ast = self.compile_ast();
      self.get_cached_ast()
    } else {
      self.ast.as_mut()
    }
  }

  pub fn get_ast_mut(&mut self) -> Option<&mut IR_STATE> {
    if self.ast.is_ok() {
      Some(self.ast.as_mut().unwrap())
    } else {
      None
    }
  }

  pub fn get_ast(&self) -> Option<&IR_STATE> {
    if self.ast.is_ok() {
      Some(self.ast.as_ref().unwrap())
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

impl Debug for ParseState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "{}{}/*\n {} \n*/\n{}\n\n\n",
      self.get_state_header(),
      self.get_scanner_header(),
      self.comment,
      self.code.replace("%%%%", &self.get_name()),
    ))
  }
}

impl Display for ParseState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!(
      "{}{}/*\n {} \n*/\n{}\n\n\n",
      self.get_state_header(),
      self.get_scanner_header(),
      self.comment,
      self.code.replace("%%%%", &self.get_name()),
    ))
  }
}
