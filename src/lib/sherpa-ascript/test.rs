use crate::types::{AScriptStore, AScriptTypeVal};

use sherpa_core::{
  test::utils::{build_parse_db_from_source_str, DBPackage},
  *,
};

/* fn create_dummy_body(id: RuleId) -> Rule {
  Rule { id, ..Default::default() }
}
 */
#[test]
fn parse_errors_when_struct_prop_type_is_redefined() -> SherpaResult<()> {
  build_parse_db_from_source_str(
    r##"

  <> a > "a" :ast { t_TestA, apple: u32 } |  "b"  :ast { t_TestA, apple: i64 }

          "##,
    "/test.sg".into(),
    Default::default(),
    &|DBPackage { journal, db, .. }| {
      let results = AScriptStore::new(journal, &db);

      assert!(results.is_faulty());

      SherpaResult::Ok(())
    },
  )
}

#[test]
fn parse_errors_when_production_has_differing_return_types() -> SherpaResult<()> {
  build_parse_db_from_source_str(
    r#"<> A > "1" :ast { t_Test } | 'a' "#,
    "/test.sg".into(),
    Default::default(),
    &|DBPackage { journal, db, .. }| {
      let results = AScriptStore::new(journal, &db);

      assert!(results.is_faulty());

      SherpaResult::Ok(())
    },
  )
}

#[test]
fn prop_is_made_optional_when_not_present_or_introduced_in_subsequent_definitions() -> SherpaResult<()> {
  build_parse_db_from_source_str(
    r#"
    <> start > A | B

    <> A > "1234" :ast { t_R, d:str($1) }

    <> B > "1234" :ast { t_R, o: u32 }"#,
    "/test.sg".into(),
    Default::default(),
    &|DBPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal, &db)?;

      for prop in &store.props {
        assert!(prop.1.optional)
      }

      SherpaResult::Ok(())
    },
  )
}

#[test]
fn group_rules_as_vectors() -> SherpaResult<()> {
  build_parse_db_from_source_str(
    r#"<> A > ( "1" :ast u32($1) )(+"|") "#,
    "/test.sg".into(),
    Default::default(),
    &|DBPackage { journal, db, .. }| {
      let results = AScriptStore::new(journal, &db)?;

      assert_eq!(results.prod_types.first_key_value()?.1.first_key_value()?.0.type_, AScriptTypeVal::U32Vec);

      SherpaResult::Ok(())
    },
  )
}
