use hctk::types::UTF8StringReader;
use hctk::types::*;
use std::time::Instant;
use test_basic_language_llvm::*;
pub fn main()
{
  let mut messages = Vec::<String>::with_capacity(10);

  let start = Instant::now();

  let mut a = ParseAction::Undefined;

  for action in Context::new_entry_parser(&mut UTF8StringReader::new("2 + 2 * 2")) {
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

  messages.iter().for_each(|s| println!("{}", s));

  println!("-- dur: {:?}", duration);

  println!("{:?}", a);
}
