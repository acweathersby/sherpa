use crate::{types::AscriptDatabase, AscriptAggregateType, AscriptRule, AscriptType, StringId};
use radlr_core::{RadlrGrammar, RadlrResult};

#[test]
fn parse_errors_when_struct_prop_type_is_redefined() -> RadlrResult<()> {
  let source = r##"

  <> a > "a" :ast { t_TestA, apple: u32 } |  "b"  :ast { t_TestA, apple: i64 }

          "##;

  let db = RadlrGrammar::new().add_source_from_string(source, "", false)?.build_db("", Default::default())?;

  let adb: AscriptDatabase = db.into();

  assert!(adb.errors.len() > 0);

  println!("{}", adb.errors[0]);

  Ok(())
}

#[test]
fn group_rules_as_vectors() -> RadlrResult<()> {
  let source = r#"<> A > ( "1" :ast u32($1) )(+"|") "#;

  let db = RadlrGrammar::new().add_source_from_string(source, "", false)?.build_db("", Default::default())?;

  let adb: AscriptDatabase = db.into();

  let AscriptRule::ListInitial(_, init) = &adb.rules[2] else {
    panic!("Expected AscriptRule::ListInitial - got {:?}", &adb.rules[2])
  };

  assert!(matches!(init.ty, AscriptType::Aggregate(AscriptAggregateType::Vec { .. })));

  Ok(())
}

#[test]
fn prop_is_made_optional_when_not_present_or_introduced_in_subsequent_definitions() -> RadlrResult<()> {
  let source = r#" <> start > A | B

  <> A > "1234" :ast { t_R, d:str($1) }

  <> B > "1234" :ast { t_R, o: u32 }"#;

  let db = RadlrGrammar::new().add_source_from_string(source, "", false)?.build_db("", Default::default())?;

  let adb: AscriptDatabase = db.into();

  let entry = adb.structs.0.first_key_value().unwrap().1;

  assert!(entry.properties.get(&StringId::from("d")).unwrap().is_optional);
  assert!(entry.properties.get(&StringId::from("o")).unwrap().is_optional);

  Ok(())
}

#[test]
fn handles_numeric_expressions() -> RadlrResult<()> {
  let source = r#" IGNORE { c:sp c:nl }

  <> statement > e            :ast { t_Result, v:$1 }
  
  <> e > e "+"{1} e{2}        :ast $1 + $3  
       | e "*"{3} e{4}        :ast $1 * $3 
       | term                   
  
  <> term > tk:num            :ast f64($1)  
          | "(" e ")"         :ast $2      
          
  <> num > c:num(+)"#;

  let db = RadlrGrammar::new().add_source_from_string(source, "", false)?.build_db("", Default::default())?;

  let adb: AscriptDatabase = db.into();

  dbg!(adb);

  Ok(())
}
