use radlr_rust_runtime::types::{ParserError, Token};

use crate::parser_core::{parse_ast_input, parse_grammar_input};

struct TemplateNonTerminals {}
struct LocalGrammarContext {}

pub fn parse_grammar(input_str: &str) -> Result<(), ParserError> {
  let grammar_ast = parse_grammar_input::<Token>(input_str)?;

  Ok(())
}
