use crate::*;
use sherpa_core::{
  test::frame::{build_parse_states_from_source_str as build_states, TestPackage},
  *,
};
use sherpa_runtime::{
  bytecode::ByteCodeParser,
  types::{bytecode::FIRST_PARSE_BLOCK_ADDRESS, SherpaParser, UTF8StringReader},
};
use std::path::PathBuf;

type Parser<'a> = ByteCodeParser<'a, UTF8StringReader<'a>, u32>;

#[test]
pub fn construct_basic_recursive_descent() -> SherpaResult<()> {
  build_states(
    r#"<> A > 'h' 'e' 'l' 'l' 'o'"#,
    "".into(),
    Default::default(),
    &|TestPackage { db, states, .. }| {
      let (bc, _) = compile_bytecode(&db, states)?;
      assert!(Parser::new(&mut ("hello".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_ok());
      SherpaResult::Ok(())
    },
  )
}

#[test]
pub fn construct_descent_on_scanner_symbol() -> SherpaResult<()> {
  build_states(
    r#"
    <> A > tk:B

    <> B > C | D
    
    <> C > 'a' D 'c'
    
    <> D > 'a' 'b'
"#,
    "".into(),
    Default::default(),
    &|TestPackage { db, states, .. }| {
      let (bc, _) = compile_bytecode(&db, states)?;
      Parser::new(&mut ("aabc".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).into()
    },
  )
}

#[test]
pub fn construct_recursive_ascent() -> SherpaResult<()> {
  build_states(
    r#"
    IGNORE { c:sp } 
      
    <> A > X 'c'
         | Y 'd'
    
    <> X > 'x' X?
    
    <> Y > 'x' Y?
"#,
    "".into(),
    Default::default(),
    &|TestPackage { db, states, .. }| {
      let (bc, _) = compile_bytecode(&db, states)?;
      assert!(Parser::new(&mut ("xxxxd".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_ok());
      assert!(Parser::new(&mut ("xxxxc".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_ok());
      assert!(Parser::new(&mut ("xxxxf".into()), &bc)
        .completes(FIRST_PARSE_BLOCK_ADDRESS)
        .is_err());
      SherpaResult::Ok(())
    },
  )
}

#[test]
fn parser_of_grammar_with_append_productions() -> SherpaResult<()> {
  build_states(
    r#"
  <> A > "B"
  +> A >  "C"
  +> A >  "D"
"#,
    "".into(),
    Default::default(),
    &|TestPackage { db, states, .. }| {
      let (bc, _) = compile_bytecode(&db, states)?;
      assert!(Parser::new(&mut ("B".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_ok());
      assert!(Parser::new(&mut ("C".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_ok());
      assert!(Parser::new(&mut ("D".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_ok());
      assert!(Parser::new(&mut ("d".into()), &bc).completes(FIRST_PARSE_BLOCK_ADDRESS).is_err());
      SherpaResult::Ok(())
    },
  )
}

#[test]
fn json_parser() -> SherpaResult<()> {
  use sherpa_core::test::frame::*;

  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../grammar/json/json.sg")
    .canonicalize()
    .unwrap();

  build_states(
    std::fs::read_to_string(grammar_source_path.as_path())?.as_str(),
    grammar_source_path,
    Default::default(),
    &|TestPackage { db, states, .. }| {
      let (bc, _) = compile_bytecode(&db, states)?;

      let input = r##"{"test":[{ "test":"12\"34", "test":"12\"34"}]}"##;

      let mut parser = ByteCodeParser::<UTF8StringReader, u32>::new(&mut (input.into()), &bc);

      dbg!(parser.collect_shifts_and_skips(
        8,
        0,
        &mut console_debugger(db.clone(), Default::default())
      ));
      SherpaResult::Ok(())
    },
  )
}

#[test]
pub fn test__sgml_like_grammar_parsing() -> SherpaResult<()> {
  let input = r#"
NAME wick_element

IGNORE { c:sp c:nl }

<> element_block > '<' component_identifier
    ( element_attribute(+)  :ast { t_Attributes, c_Attribute, attributes: $1 } )?
    ( element_attributes | general_data | element_block | general_binding )(*)
    ">"

                                                                :ast { t_Element, id:$2, children: [$3, $4], tok }
<> component_identifier >
    identifier ( ':' identifier )?
                                                                :ast { t_Ident, name:str($1), sub_name:str($2), tok }

<> element_attributes >c:nl element_attribute(+)
                                                                :ast { t_Attributes, c_Attribute, attributes: $2 }

<> element_attribute > '-' identifier attribute_chars c:sp

                                                                :ast { t_GeneralAttr, c_Attribute, key:str($2), val1: str($3) }

    | '-' identifier ':' identifier
                                                                :ast { t_BindingAttr, c_Attribute, key:str($2), val2: str($4) }

    | '-' "store" '{' local_values? '}'
                                                                :ast { t_StoreAttr, c_Attribute, children: $4 }
    | '-' "local" '{' local_values? '}'
                                                                :ast { t_LocalAttr, c_Attribute, children: $4 }
    | '-' "param" '{' local_values? '}'
                                                                :ast { t_ParamAttr, c_Attribute, children: $4 }
    | '-' "model" '{' local_values? '}'
                                                                :ast { t_ModelAttr, c_Attribute, children: $4 }

<> general_binding > ':' identifier
                                                                :ast { t_OutputBinding, val3:str($2) }

<> local_values > local_value(+)

<> local_value > identifier ( '`' identifier )? ( '='  c:num )? ( ',' )(*)

                                                                :ast { t_Var, c_Attribute, name:str($1), meta:str($2), value:$3 }

<> attribute_chars > ( c:id | c:num | c:sym  )(+)
                                                                :ast { t_AttributeData, tok }
<> general_data > ( c:id | c:num  | c:nl  )(+)
                                                                :ast { t_GeneralData, tok }

<> identifier > tk:tok_identifier

<> tok_identifier > ( c:id | c:num )(+)
"#;

  build_states(input, "".into(), Default::default(), &|TestPackage { db, states, .. }| {
    let (bc, _) = compile_bytecode(&db, states)?;

    assert!(Parser::new(&mut ("<i -test : soLongMySwanSong - store { test } <i>>".into()), &bc)
      .completes(FIRST_PARSE_BLOCK_ADDRESS)
      .is_ok());

    SherpaResult::Ok(())
  })
}
