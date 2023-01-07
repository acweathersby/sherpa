pub mod parser {
  sherpa::compile! {

  <> A > alligator

  <> alligator > 1 | "test"

    f:ast { { t_Test, tok } }
  }

  impl Test {
    pub fn my_string(&self) -> String {
      self.tok.to_string()
    }
  }
}

#[cfg(test)]
mod test {
  use crate::parser::*;
  use sherpa::UTF8StringReader;

  #[test]
  fn test_unordered() {
    let node = AST::default_from("1,,");

    println!("{:?}", node);
    if !node.is_ok() {
      panic!("Failed to parse input");
    } else {
      node.unwrap().my_string();
    }ou
  }
}
