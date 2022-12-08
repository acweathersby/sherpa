pub mod sherpa_parser {
  include!(concat!(env!("OUT_DIR"), "/sherpa_out/sherpa.rs"));
}

#[cfg(test)]
mod sherpa_tests {
  use std::time::Instant;

  use crate::sherpa_parser::Grammar;

  #[test]
  fn basic_test() {
    let i = Instant::now();

    let ast = Grammar::from_str(
      r###"NAME sherpa_mini 
EXPORT A as dir

<> lazy A > c:id{1} B{3}^test?        => $1 
   | B<A> C ( D | tk:sam::R )?        => { t_Test, val:$2 }

<T> B > "function"! ."{" ( T )(+) "}" => [ $1 + $2 + $3 ] 
"###,
    )
    .unwrap();
    println!("{:?}", i.elapsed());

    dbg!(ast);
  }
}
