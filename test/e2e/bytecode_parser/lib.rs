pub mod bytecode_based_parser {
  include!(concat!(env!("OUT_DIR"), "/bytecode_parser.rs"));
}

#[cfg(test)]
mod test {

  use super::bytecode_based_parser as bc;
  #[test]
  pub fn test_build() {
    let result = bc::ast::entry_from("hello world".into());
    dbg!(result);
  }
}
