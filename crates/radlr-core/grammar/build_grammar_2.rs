use radlr_rust_runtime::types::Token;

use crate::{
  parser::{self, radlr_bc_ast},
  RadlrResult,
};

/// Parse grammar string and create a Grammar AST.
pub fn parse_grammar(string_data: &str) -> RadlrResult<Box<radlr_bc_ast::Grammar<Token>>> {
  parser::parse_grammar(string_data)
}
