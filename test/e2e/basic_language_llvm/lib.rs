pub mod llvm_language_test_parser {
  include!(concat!(env!("OUT_DIR"), "/llvm_language_test.rs"));
}

pub use llvm_language_test_parser::*;

#[cfg(test)]
mod test {

  use std::time::Instant;

  use sherpa_runtime::types::*;

  use crate::{Json, Parser};

  #[test]
  pub fn test_build() {
    let n = Instant::now();
    let ast = Json::from_str(r##"{ "" : [233.0, { "test" : "test" }] }"##);

    println!("{:?}", n.elapsed());

    println!("{:#?}", ast);

    assert!(ast.is_ok());
  }

  #[test]
  pub fn test_spirv() {
    let n = Instant::now();
    let string = include_str!("./spirv.core.grammar.json");
    let ast = Json::from_str(string);

    println!("{:?}", n.elapsed());

    //println!("{:?}", ast);

    assert!(ast.is_ok());
  }
}
