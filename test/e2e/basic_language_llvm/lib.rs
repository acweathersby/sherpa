pub mod llvm_language_test_parser {
  include!(concat!(env!("OUT_DIR"), "/llvm_language_test.rs"));
}

pub use llvm_language_test_parser::*;

#[cfg(test)]
mod test {

  use std::time::Instant;

  use sherpa_runtime::types::*;

  use crate::{Parser, Stmt};

  #[test]
  pub fn test_build() {
    let n = Instant::now();
    let ast = Stmt::from_str("1");

    println!("{:?}", n.elapsed());

    println!("{:?}", ast);

    assert!(ast.is_ok());
  }
}
