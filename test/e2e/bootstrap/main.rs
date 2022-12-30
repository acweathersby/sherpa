use std::time::Instant;

use sherpa::SherpaResult;
use test_boot_lib;

fn main() -> SherpaResult<()> {
  use test_boot_lib::sherpa_parser::Grammar;

  let i = Instant::now();
  let ast = Grammar::from_str(
    r###"NAME sherpa_mini 
  EXPORT A as dir

  <> lazy A > c:id{1} B{3}^test? => $1 
     | B<A> C ( D | tk:sam::R )? => { t_Test, val:$2 }

  <T> B > "function"! ."{" ( T )(+) "}" => [ $1 + $2 + $3 ] 
  "###,
  )?;
  println!("{:?}", i.elapsed());

  println!("{:#?}", ast);

  SherpaResult::Ok(())
}
