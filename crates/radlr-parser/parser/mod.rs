use std::fmt::Debug;

use radlr_rust_runtime::{
  parsers::ast::{AstDatabase, Tk},
  types::{ParserError, RuntimeDatabase, StringInput},
};

mod ast;
mod parser;

pub fn parse_grammar_input<Token: Tk + Debug>(input: &str) -> Result<ast::ASTNode<Token>, ParserError> {
  let parser_db = parser::ParserDB::new();

  let result = parser_db.build_ast(
    &mut StringInput::from(input),
    parser_db.get_entry_data_from_name("grammar").unwrap(),
    ast::ReduceRules::<Token>::new(),
  );

  result
}
