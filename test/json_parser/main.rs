use json_test_parser::*;
use sherpa_runtime::types::{ParseAction, UTF8StringReader};
use std::time::Instant;
pub fn main() {
  let mut messages = Vec::<String>::with_capacity(10);

  let start = Instant::now();
  let str_ref = r##"{  "d": ["a", "b", "c"], "test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test": "mango", "dogo":{ "test": 1234 }, 
  "test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test 
  test test test test test test test test test test test test
  test" : 123 } "##;

  let parser = Parser::<_, u32>::new_entry_parser(UTF8StringReader::from(str_ref));

  for action in parser.into_iter() {
    dbg!(action);
  }

  println!("{:?}", start.elapsed());

  let json = Json::from_str(str_ref);

  println!("{:#?}", json);

  let start = Instant::now();
  return;
  let mut dur = 0;

  loop {
    let mut a = ParseAction::Undefined;
    let result = Json::from_str(str_ref);
    assert!(result.is_ok());
    dur += 1;
    if dur > 100000 {
      println!("{:?}", result);
      break;
    }
  }

  let duration = start.elapsed();

  // messages.iter().for_each(|s| println!("{}", s));

  println!("-- dur: {:?}", duration);

  // println!("{:?}", a);
}
