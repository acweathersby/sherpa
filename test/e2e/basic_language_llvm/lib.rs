pub mod llvm_language_test_parser {
  include!(concat!(env!("OUT_DIR"), "/llvm_language_test.rs"));
}

pub use llvm_language_test_parser::*;

#[cfg(test)]
mod test {

  use crate::{Context, *};
  use sherpa::types::*;

  #[test]
  pub fn test_build() {
    let ast = Context::parse_entry(&mut UTF8StringReader::new("(2+(2*2))+1+1+1"));

    println!("{:?}", ast);

    assert!(ast.is_ok());

    dbg!(ast);
  }
}
