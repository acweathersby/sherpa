use std::rc::Rc;

use sherpa_core::IStringStore;
use sherpa_rust_runtime::{
  parsers::error_recovery::ErrorRecoveringDatabase,
  types::{BytecodeParserDB, CSTNode, Printer, RuntimeDatabase, StringInput},
};
use wasm_bindgen::prelude::wasm_bindgen;

use crate::grammar::JSBytecodeParserDB;

/// A basic language server like interface for parsers constructed to support
/// advance language editing features.

#[wasm_bindgen]
#[derive(Clone)]
pub struct LSPSystem {
  db: JSBytecodeParserDB,
}

#[wasm_bindgen]
impl LSPSystem {
  pub fn new(db: &JSBytecodeParserDB) -> Self {
    Self { db: db.clone() }
  }
}

impl LSPSystem {
  pub fn parse(&mut self, entry_name: &str, input: String) -> Option<CSTNode> {
    let db = self.db.0.as_ref();

    if let Ok(entry) = db.get_entry_data_from_name(entry_name) {
      match db.parse_with_recovery(&mut StringInput::from(input), entry) {
        Ok(mut cst) => {
          if let Some(mut ctx) = cst.drain(..).next() {
            if ctx.symbols.len() != 1 {
              None
            } else {
              Some(unsafe { ctx.symbols.pop().unwrap_unchecked() })
            }
          } else {
            Default::default()
          }
        }
        Err(err) => Default::default(),
      }
    } else {
      Default::default()
    }
  }

  pub fn parse_partial(&mut self, input: String, start_node: u32) {}
}

#[wasm_bindgen]
pub struct EditGraph {
  root: Option<CSTNode>,
  lsp:  LSPSystem,
}

#[wasm_bindgen]
impl EditGraph {
  pub fn new(lsp: &LSPSystem) -> Self {
    Self { root: None, lsp: lsp.clone() }
  }

  pub fn parse(&mut self, input: String) {
    self.root = self.lsp.parse("default", input);
  }

  pub fn insert(offset: usize, string: String) {
    
  }

  pub fn remove(offset: usize, len: usize) {}

  pub fn to_string(&self) -> String {
    match self.root.as_ref() {
      Some(node) => {
        let mut vec = Vec::with_capacity(1024);
        match Printer::new(node, self.lsp.db.0.as_ref()).write(&mut vec) {
          Ok(()) => unsafe { String::from_utf8_unchecked(vec) },
          Err(_) => Default::default(),
        }
      }
      _ => Default::default(),
    }
  }
}
