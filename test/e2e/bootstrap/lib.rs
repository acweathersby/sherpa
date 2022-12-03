pub mod sherpa_parser {
  include!(concat!(env!("OUT_DIR"), "/sherpa_out/sherpa.rs"));
}

#[cfg(test)]
mod sherpa_tests {
  use crate::sherpa_parser::{ASTParse, AST};

  #[test]
  fn basic_test() {
    dbg!(AST::grammar_from(
      r###"NAME sherpa_mini 
EXPORT A as dir

<> lazy A > c:id{1} B{3}^test => $1 
   | B C ( D | tk:sam::R )? => { t_Test, val:$2 }

<T> B > "function"! "{" expr(+) "}" => [ $1 + $2 + $3 ] 
"###
    ));
  }
}
