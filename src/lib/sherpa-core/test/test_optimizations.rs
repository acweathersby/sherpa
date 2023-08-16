use super::utils::build_parse_states_from_multi_sources;
use crate::{
  optimize,
  test::utils::{build_parse_states_from_source_str as build, TestPackage as Tp},
  DBProdKey,
  ParseStatesVec,
  SherpaResult as R,
};

#[test]
fn basic_optimize_unknown() -> R<()> {
  build_parse_states_from_multi_sources(
    &[r##"
    IGNORE { c:sp  } 

    <> A > ( B | ":" C )(+)
    
    <> B > id "=>" c:id
    
    <> C > a_id(+)
    
    <> a_id > id "!"? 
    
    <> id > tk:id_tok
    
    <> id_tok > c:id
    

    "##],
    "/".into(),
    Default::default(),
    &|Tp { states, db, .. }| {
      for state in &states {
        println!("A: {:#}\n", state.1.source_string(db.string_store()))
      }

      let states = optimize::<ParseStatesVec>(db, states)?;

      println!("AFTER -------------------");

      for state in states {
        println!("B: {:#}\n", state.1.source_string(db.string_store()))
      }

      R::Ok(())
    },
  )
}
