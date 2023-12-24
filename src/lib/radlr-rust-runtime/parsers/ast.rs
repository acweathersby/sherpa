//! A parser that produce the tokens, including skipped tokens, of an input.

use std::fmt::Debug;
use crate::types::*;

pub trait Tk: Clone + Default + std::hash::Hash {
  fn to_string(&self) -> String;
  fn trim(&self, start: usize, end: usize) -> Self;
  fn from_range(start: usize, end: usize, id: u32, source: SharedSymbolBuffer) -> Self;
  fn from_slice(slice: &[Self]) -> Self;
}

pub trait Node<Token: Tk>: Default {}

impl<Token: Tk, T: Default> Node<Token> for T {}

pub type Reducer<Token, N> = fn(*mut [N], &[Token], Token) -> N;

pub trait AstDatabase<I: ParserInput>: ParserProducer<I> + Sized {
  /// Parse while attempting to recover from any errors encountered in the
  /// input.
  ///
  /// This extends the fork parser by allowing recovery methods to be applied
  /// when the base parser encounters input that prevent it from continuing. As
  /// several error recovery strategies are employed, the resulting parse
  /// forest may include several trees containing error corrections of varying
  /// quality. In this case, the tree containing the least number of error
  /// correction assumptions will be ordered in front of trees that employ more
  /// guesswork to recover parsing.
  fn build_ast<Token: Tk, N: Node<Token> + Debug, R: AsRef<[Reducer<Token, N>]>>(
    &self,
    input: &mut I,
    entry: EntryPoint,
    rules: R,
  ) -> Result<N, ParserError> {
    build_ast(input, entry, self, rules)
  }
}

impl<I: ParserInput, T: ParserProducer<I> + Sized> AstDatabase<I> for T {}

fn build_ast<I: ParserInput, DB: ParserProducer<I>, Token: Tk, N: Node<Token> + Debug, R: AsRef<[Reducer<Token, N>]>>(
  input: &mut I,
  entry: EntryPoint,
  db: &DB,
  rules: R,
) -> Result<N, ParserError> {
  let mut tokens = vec![];
  let mut nodes = vec![];

  let mut parser = db.get_parser()?;

  let mut ctx = parser.init(entry)?;

  while let Some(action) = parser.next(input, &mut ctx) {
    match action {
      ParseAction::Accept { nonterminal_id, final_offset } => {
        return if final_offset != input.len() {
          Err(ParserError::InputError {
            inline_message: format!("Failed to read entire input {} {}", input.len(), final_offset),
            last_nonterminal: nonterminal_id,
            loc: Default::default(),
            message: "Failed to read entire input".to_string(),
          })
        } else if nonterminal_id != entry.nonterm_id {
          Err(ParserError::InputError {
            inline_message: "Top symbol did not match the target nonterminal".to_string(),
            last_nonterminal: nonterminal_id,
            loc: Default::default(),
            message: "CST is incorrect".to_string(),
          })
        } else {
          return Ok(nodes.pop().unwrap());
        };
      }
      ParseAction::Error { .. } => {
        todo!("Create panic error");
      }
      ParseAction::Fork { .. } => {
        panic!("No implementation of fork resolution is available")
      }
      ParseAction::Skip { .. } => {}
      ParseAction::Shift {
        byte_length: token_byte_length,
        byte_offset: token_byte_offset,
        token_id,
        ..
      } => {
        let offset_start = token_byte_offset as usize;
        let offset_end = (token_byte_offset + token_byte_length) as usize;

        let token = Token::from_range(offset_start, offset_end, token_id, input.get_owned_ref());

        tokens.push(token.clone());
        nodes.push(N::default());
      }
      ParseAction::Reduce { rule_id: _rule_id, symbol_count, .. } => {
        let rule = &rules.as_ref()[_rule_id as usize];

        let nodes_start = nodes.len() - symbol_count as usize;
        let tok_start = tokens.len() - symbol_count as usize;

        let node_slice = nodes[nodes_start..].as_mut();
        let token_slice = tokens[tok_start..].as_ref();

        let non_term_token = Token::from_slice(token_slice);

        let new_node = rule(node_slice as *mut [N], token_slice, non_term_token.clone());

        nodes.drain(nodes_start..);
        nodes.push(new_node);
        tokens.drain(tok_start..);
        tokens.push(non_term_token);
      }
      _ => panic!("Unexpected Action!"),
    }
  }
  return Err(ParserError::Unexpected);
}
