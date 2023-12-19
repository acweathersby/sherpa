#![allow(unused)]

/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.0-beta2
/// - **SOURCE**: /home/work/projects/lib_radlr/src/grammar/v2_0_0/grammar.sg
///
/// #### WARNING WARNING WARNING WARNING
/// #### WARNING WARNING WARNING WARNING
/// #### WARNING WARNING WARNING WARNING
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### GNINRAW GNINRAW GNINRAW GNINRAW
/// #### GNINRAW GNINRAW GNINRAW GNINRAW
/// #### GNINRAW GNINRAW GNINRAW GNINRAW
///
/// #### License:

/// Copyright (c) 2020-2024 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the 'Software'), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE

use radlr_rust_runtime::{kernel::ByteCodeParserNew, parsers::Parser, types::*, *};
use std::{collections::HashMap, rc::Rc};

const BINARY: &'static [u8] = include_bytes!("/home/work/projects/lib_radlr/test/workflow_test/parser.bin");

const NONTERM_NAME_TO_ID: [(&'static str, u32); 1] = [("default",0),];

const TOKEN_ID_TO_STRING: [(u32, &'static str); 5] = [(0, r###"Default"###),(1, r###"c:sp"###),(2, r###"c:nl"###),(3, r###"World"###),(4, r###"Hello"###),];

const NONTERM_ID_TO_ADDRESS: [(u32, u32); 1] = [(0, 8),];

const STATE_TO_TOKEN_IDS: [(u32, &'static [u32]); 2] = [(21, &TOKENS_1),(61, &TOKENS_0),];

const TOKENS_0: [u32;1]=[3,];

const TOKENS_1: [u32;1]=[4,];


/// Parser database for the "" parser
pub struct ParserDB {
  pub bytecode: &'static [u8],
  pub nonterm_name_to_id: HashMap<&'static str, u32>,
  pub state_to_token_ids_map: HashMap<u32, &'static [u32]>,
  pub nonterm_id_to_address: HashMap<u32, u32>,
  pub token_id_to_str: HashMap<u32, &'static str>,

}

impl ParserDB {
  pub fn new() -> Self {Self {
      bytecode: BINARY,
      nonterm_name_to_id: HashMap::from_iter(NONTERM_NAME_TO_ID),
      state_to_token_ids_map: HashMap::from_iter(STATE_TO_TOKEN_IDS),
      nonterm_id_to_address: HashMap::from_iter(NONTERM_ID_TO_ADDRESS),
      token_id_to_str: HashMap::from_iter(TOKEN_ID_TO_STRING)
    
    }
  
  }

}

impl AsRef<[u8]> for ParserDB {
  fn as_ref(&self) -> &[u8] {
    self.bytecode
  }
}


impl RuntimeDatabase for ParserDB {
  fn default_entrypoint(&self) -> EntryPoint {
      EntryPoint { nonterm_id: 0 }
  }

  fn get_entry_data_from_name(&self, entry_name: &str) -> Result<EntryPoint, ParserError> {
    if let Some(id) = self.nonterm_name_to_id.get(entry_name) {
      Ok(EntryPoint { nonterm_id: *id })
    } else {
      Err(ParserError::InvalidEntryName)
    }
  
  }

  fn get_expected_tok_ids_at_state(&self, state_id: u32) -> Option<&[u32]> {
    self.state_to_token_ids_map.get(&state_id).map(|s| *s)
  }

  fn token_id_to_str(&self, tok_id: u32) -> Option<&str> {
    self.token_id_to_str.get(&tok_id).map(|s| *s)
  }

}

impl<T: ParserInput> ParserProducer<T> for ParserDB {
  fn get_parser(&self) -> Result<Box<dyn Parser<T>>, ParserError> {
    Ok(Box::new(ByteCodeParserNew::new(Rc::new(self.bytecode), self.nonterm_id_to_address.clone())))
  
  }

}

