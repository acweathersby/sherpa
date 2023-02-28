pub mod json_parser {
  include!(concat!(env!("OUT_DIR"), "/json.rs"));
}

pub use json_parser::*;

#[cfg(test)]
mod test {

  use crate::Json;
  use std::time::Instant;

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
    let string = include_str!("./spirv.core.grammar.json");
    let n = Instant::now();
    let ast = Json::from_str(string);

    println!("{:?}", n.elapsed());

    println!("{:#?}", ast);

    assert!(ast.is_ok());
  }
}
