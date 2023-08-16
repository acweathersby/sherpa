use crate::{
  test::utils::{build_parse_states_from_source_str as build, TestPackage as Tp},
  DBProdKey,
  SherpaResult as R,
};

#[test]
fn basic_optimize_unknown() -> R<()> {
  build("<> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}
