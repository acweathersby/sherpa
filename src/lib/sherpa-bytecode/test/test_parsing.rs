use crate::{
  test::utils::{map_reduce_function, TestParser},
  *,
};
use sherpa_core::{
  test::utils::{build_parse_states_from_source_str as build_states, TestPackage},
  *,
};
use sherpa_rust_runtime::types::{ast::AstSlot, bytecode::FIRST_PARSE_BLOCK_ADDRESS, SherpaParser};
use std::path::PathBuf;

use super::utils::compile_and_run_grammars;

#[test]
pub fn symbols_requiring_peek() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"

IGNORE { c:sp  } 

<> A > gs(+">>") ">>" ge

<> gs > "t:" tk:name

<> ge > "e:" tk:name

<> name > c:id(+)

"#],
    &[("default", "t:a >> e:b", true)],
  )
}

#[test]
pub fn other_symbols_requiring_peek() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
IGNORE { c:sp  } 

<> A > ( B | ":" C )(+)

<> B > id "=>" c:id

<> C > a_id(+)

<> a_id > id "!"? 

<> id > tk:id_tok

<> id_tok > c:id

"#],
    &[("default", ":t! t t => g :t!", true)],
  )
}

#[test]
pub fn construct_basic_recursive_descent() -> SherpaResult<()> {
  compile_and_run_grammars(&[r#"<> A > 'h' 'e' 'l' 'l' 'o'"#], &[("default", "hello ", true)])
}

#[test]
pub fn construct_descent_on_scanner_symbol() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    IGNORE { c:sp  } 

    <> A > tk:B 'c'

    <> B > C | D
    
    <> C > 'a' D 'c'
    
    <> D > 'a' 'b'
"#],
    &[("default", "aabcc", true)],
  )
}

#[test]
pub fn construct_recursive_ascent() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    IGNORE { c:sp } 
      
    <> A > X 'c'
         | Y 'd'
    
    <> X > 'x' X?
    
    <> Y > 'x' Y?
"#],
    &[("default", "xxxxd", true), ("default", "xxxxc", true), ("default", "xxxxf", false)],
  )
}

#[test]
pub fn tokens_with_hyphens_and_underscores() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    <> id > tk:id_tok{100} " a"

    <> id_tok > ( "-" | "_" | c:id ) ( c:id | c:num | '_' | '-'  )(*)
"#],
    &[("default", "test-test a", true)],
  )
}

#[test]
pub fn local_rule_append() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    IGNORE { c:sp } 
      
    <> A > "one"
    
    +> A > "two"
"#],
    &[("default", "one ", true), ("default", "two", true)],
  )
}

#[test]
pub fn cross_source_rule_append() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[
      r#"
IMPORT B as B
IGNORE { c:sp }
  
<> A > "one"
"#,
      r#"
IMPORT A as A

+> A::A > "two"
"#,
    ],
    &[("default", "one ", true), ("default", "two", true)],
  )
}

#[test]
pub fn skipped_symbol() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    IGNORE { "A" } 
      
    <> A > "B" "T"
"#],
    &[("default", "BAT ", true), ("default", "BT", true), ("default", "BA AT", false)],
  )
}

#[test]
pub fn skipped_nonterm_token_symbol() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    IGNORE { tk:vowels } 
      
    <> A > "B" "T"

    <> vowels > "A" | "E" | "I" | "O" | "U" | "Y"
"#],
    &[
      ("default", "BAT ", true),
      ("default", "BIT", true),
      ("default", "BUT", true),
      ("default", "BOT", true),
      ("default", "BYTE", true),
    ],
  )
}

#[test]
fn parser_of_grammar_with_append_productions() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
  <> A > "B"
  +> A >  "C"
  +> A >  "D"
"#],
    &[("default", "B ", true), ("default", "C", true), ("default", "D", true), ("default", "d", false)],
  )
}

#[test]
fn parsing_using_trivial_custom_state() -> SherpaResult<()> {
  compile_and_run_grammars(&[r##"A => match : BYTE  (65 /* A */ | 66 /* B */) { shift then pass }"##], &[
    ("default", "A ", true),
    ("default", "B", true),
    ("default", "C", false),
  ])
}

#[test]
fn json_parser() -> SherpaResult<()> {
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  compile_and_run_grammars(&[std::fs::read_to_string(grammar_source_path.as_path())?.as_str()], &[
    ("entry", r##"{"test":[{ "test":"12\"34", "test":"12\"34"}]}"##, true),
    ("entry", r##"{"\"":2}"##, true),
  ])
}

/* #[test]
fn lalr_pop_parser() -> SherpaResult<()> {
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../grammar/lalrpop/lalrpop.sg")
    .canonicalize()
    .unwrap();
  compile_and_run_grammar(std::fs::read_to_string(grammar_source_path.as_path())?.as_str(), &[(
    "entry",
    r##"grammar ;"##,
    true,
  )])
} */

#[test]
fn handles_grammars_that_utilize_eof_symbol() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
EXPORT A as A
EXPORT B as B
      
<> A > "a" "b" "c" $

<> B > "c" "b" "a"    
    "##],
    &[("A", "abc ", false), ("A", "abc", true), ("B", "cba", true), ("B", "cba ", true)],
  )
}

#[test]
pub fn test_sgml_like_grammar_parsing() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
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
"##],
    &[("default", "<i -test : soLongMySwanSong - store { test } <i>>", true)],
  )
}

#[test]
fn generic_grammar() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
IGNORE { c:sp c:nl } 

<> script > block(+)

<> block > "DECLARE" tk:name "{" declare_content(+) "}"

<> declare_content > execute_content(+)

<> execute_content > "aaa"(+)

<> name > c:id(+)
"##],
    &[(
      "default",
      r##"

    DECLARE AS {
        aaa
        aaa
    }
    
    DECLARE BS {
       aaa
       aaa
       aaa
    }
    
    DECLARE CS {
       aaa
    }
    
      "##,
      true,
    )],
  )
}

#[test]
pub fn intermediate_exclusive_symbols() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"

<> R > tk:A "ly"

<> A > "test" | "tester" | 'testing'
"##],
    &[("default", "testly", true), ("default", "testerly", true), ("default", "testingly", false)],
  )
}

#[test]
pub fn c_style_comment_blocks() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
    <> A > tk:comment
    
    <> comment > tk:block  | tk:line  | c:id(+)
       
    <> block > "/*"  ( c:sym | c:id | c:sp )(*) "*/"
    
    <> line > "//"  ( c:sym | c:id | c:sp )(*) c:nl?
    "##],
    &[
      ("default", r##"//test"##, true),
      ("default", r##"//\n"##, true),
      ("default", r##"/* triangle */"##, true),
      ("default", r##"walker"##, true),
    ],
  )
}

#[test]
fn json_object_with_specialized_key() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
    IGNORE { c:sp c:nl }
    
    <> json > '{'  value(*',') '}'
    
    <> value > tk:string ':' tk:string
    
        | "\"test\"" ':' c:num
    
    <> string > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) '\"'
        
    <> escape > "\\"   ( c:sym | c:num | c:sp | c:id | c:nl)
    "##],
    &[
      ("default", r##"{"test":2}"##, true),
      ("default", r##"{ "tester" : "mango"  }"##, true),
      ("default", r##"{ "tester" : 2  }"##, false),
      ("default", r##"{ "test" : "mango"  }"##, false),
    ],
  )
}

#[test]
fn scientific_number() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
    <> sci_number > tk:number
    
    <> number > ( '+' | '-' )? c:num(+) ( '.' c:num(+) )? ( ( 'e' | 'E' ) ( '+' | '-' )? c:num(+) )?
    "##],
    &[("default", r##"2.3e-22"##, true), ("default", r##"0.3e-22"##, true)],
  )
}

#[test]
fn escaped_values() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
<> escaped_string > ( escaped_vals  )(+)

<> escaped_vals > c:num | c:id | c:sym | c:nl | c:sp | escaped

<> escaped > '\\' ( c:num | c:id | c:sym | c:nl | c:sp )
           | '\\'
        "##],
    &[("default", r##"\"##, true)],
  )
}

#[test]
fn escaped_string() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
        <> string > tk:string_tk
        
        <> string_tk > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) "\""
        
        <> escape > "\\"  ( c:sym | c:num | c:sp | c:id )
        "##],
    &[
      ("default", r##""""##, true),
      ("default", r##""\\""##, true),
      ("default", r##""1234""##, true),
      ("default", r##""12\"34""##, true),
    ],
  )
}

/// Bytecode counterpart to sherpa_llvm::test::compile::simple_newline_tracking
#[test]
fn simple_newline_tracking() -> SherpaResult<()> {
  build_states(
    r##"
    IGNORE { c:sp c:nl }

    <> test > 'hello' P

    <> P > 'world' 'goodby' B

    <> B > 'mango'
        "##,
    "".into(),
    Default::default(),
    &|TestPackage { db, states, .. }| {
      let states = optimize::<ParseStatesVec>(&db, states)?;

      let (bc, _) = compile_bytecode(&db, states.iter())?;

      let mut parser = TestParser::new(&mut ("hello\nworld\n\ngoodby\nmango".into()), &bc);
      parser.init_parser(FIRST_PARSE_BLOCK_ADDRESS);
      let result = parser.parse_ast(
        &map_reduce_function(db, vec![
          ("test", 0, |ctx, slots| {
            assert_eq!(slots[0].1.to_slice(unsafe { &*ctx }.get_str()), "hello");
            assert_eq!(slots[0].1.line_num, 0, "Line number of `hello` should be 0");
            assert_eq!(slots[0].1.line_off, 0, "Line offset of `hello` should be 0");

            slots.assign(0, AstSlot(1010101, Default::default(), Default::default()))
          }),
          ("B", 0, |ctx, slots| {
            assert_eq!(slots[0].1.to_slice(unsafe { &*ctx }.get_str()), "mango");
            assert_eq!(slots[0].1.line_num, 4, "Line number of `mango` should be 4");
            assert_eq!(slots[0].1.line_off, 19, "Line offset of `mango` should be 19");
          }),
          ("P", 0, |ctx, slots| {
            assert_eq!(slots[0].1.to_slice(unsafe { &*ctx }.get_str()), "world");
            assert_eq!(slots[0].1.line_num, 1, "Line number of `world` should be 1");
            assert_eq!(slots[0].1.line_off, 5, "Line offset of `world` should be 5");
          }),
        ]),
        &mut None,
      );

      assert!(matches!(result, Result::Ok(AstSlot(1010101, ..))));

      SherpaResult::Ok(())
    },
  )
}
