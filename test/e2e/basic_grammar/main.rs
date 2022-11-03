mod ast;

use crate::ast::ASTNode;
use crate::ast::REDUCE_FUNCTIONS;
use hctk::types::UTF8StringReader;
use hctk::types::*;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use test_basic_grammar::*;

pub fn main() {
  let mut nodes: Vec<HCObj<ASTNode>> = Vec::with_capacity(8);
  let mut messages = Vec::<String>::with_capacity(10);

  let start = Instant::now();

  let actions =
    Context::new_banner_parser(&mut UTF8StringReader::new("hello world")).collect::<Vec<_>>();

  assert!(matches!(actions[0], ParseAction::Shift { .. }));
  assert!(matches!(actions[1], ParseAction::Shift { .. }));
  assert!(matches!(actions[2], ParseAction::Reduce { production_id, .. } if production_id == 1));
  assert!(matches!(actions[3], ParseAction::Accept { production_id } if production_id == 1));

  let duration = start.elapsed();

  messages.iter().for_each(|s| println!("{}", s));

  println!("-- dur: {:?}", duration)
}
