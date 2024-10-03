#![allow(warnings)]
#![allow(unused, improper_ctypes)]
#![allow(bad_style)]
#![allow(non_snake_case)]


#[cfg(feature = "binary-parser")]
#[rustfmt::skip]
pub(crate) mod radlr;

#[cfg(feature = "binary-parser")]
pub use radlr::{Goto, Range, *};

#[cfg(not(feature = "binary-parser"))]
#[rustfmt::skip]
pub mod radlr_bc;

#[cfg(not(feature = "binary-parser"))]
pub use radlr_bc::{Goto, Range, *};
use radlr_rust_runtime::{
  parsers::ast::AstDatabase,
  types::{RuntimeDatabase, StringInput, Token},
};

use crate::RadlrResult;

pub mod radlr_bc_ast;
pub mod radlr_bc_parser;

/// Parses input based on the LL grammar.
pub fn parse_grammar(input: &str) -> RadlrResult<Box<radlr_bc_ast::Grammar<Token>>> {
  let parser_db = radlr_bc_parser::ParserDB::new();
  match parser_db.build_ast(
    &mut StringInput::from(input),
    parser_db.get_entry_data_from_name("grammar").unwrap(),
    radlr_bc_ast::ReduceRules::<radlr_rust_runtime::types::Token>::new(),
  ) {
    Err(err) => {
      println!("{err:?}");
      Err("Failed to parse input".to_string().into())
    }
    Ok(node) => Ok(node.into_Grammar().unwrap()),
  }
}
