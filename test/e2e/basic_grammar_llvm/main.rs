use hctk::types::UTF8StringReader;
use hctk::types::*;
use std::time::Instant;
use test_basic_grammar_llvm::*;
pub fn main() {
  let mut messages = Vec::<String>::with_capacity(10);

  let start = Instant::now();

  let mut a = ParseAction::Undefined;

  for action in llvm_test::Context::new_banner_parser(&mut UTF8StringReader::new("hello world")) {
    match action {
      ParseAction::Shift { skipped_characters: skip, token } => {
        messages.push(format!("Skip {:? } & Extract token {:?} ", skip, token));
      }
      ParseAction::Accept { production_id } => {
        messages.push(format!("Accept production {}", production_id));
        break;
      }
      _ => {
        a = action;
      }
    }
  }
  let duration = start.elapsed();

  println!("-- dur: {:?}", duration);

  messages.iter().for_each(|s| println!("{}", s));

  println!("{:?}", a);
}
