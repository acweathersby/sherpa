use crate::*;
use sherpa_core::{
  proxy::OrderedMap,
  test::frame::{
    build_parse_db_from_source_str,
    build_parse_states_from_source_str as build_states,
    TestPackage,
  },
  *,
};
use sherpa_rust_runtime::{
  bytecode::ByteCodeParser,
  types::{
    ast::{AstObject, AstSlot, AstStackSlice, Reducer},
    bytecode::FIRST_PARSE_BLOCK_ADDRESS,
    ByteReader,
    MutByteReader,
    ParseContext,
    ParseResult,
    SherpaParser,
    UTF8StringReader,
  },
};
use std::path::PathBuf;

type Parser<'a> = ByteCodeParser<'a, UTF8StringReader<'a>, u32>;

#[test]
pub fn construct_basic_recursive_descent() -> SherpaResult<()> {
  compile_and_run_grammar(r#"<> A > 'h' 'e' 'l' 'l' 'o'"#, &[("default", "hello ", true)])
}

#[test]
pub fn construct_descent_on_scanner_symbol() -> SherpaResult<()> {
  compile_and_run_grammar(
    r#"
    <> A > tk:B

    <> B > C | D
    
    <> C > 'a' D 'c'
    
    <> D > 'a' 'b'
"#,
    &[("default", "aabc ", true)],
  )
}

#[test]
pub fn construct_recursive_ascent() -> SherpaResult<()> {
  compile_and_run_grammar(
    r#"
    IGNORE { c:sp } 
      
    <> A > X 'c'
         | Y 'd'
    
    <> X > 'x' X?
    
    <> Y > 'x' Y?
"#,
    &[("default", "xxxxd ", true), ("default", "xxxxc", true), ("default", "xxxxf", false)],
  )
}

#[test]
fn parser_of_grammar_with_append_productions() -> SherpaResult<()> {
  compile_and_run_grammar(
    r#"
  <> A > "B"
  +> A >  "C"
  +> A >  "D"
"#,
    &[
      ("default", "B ", true),
      ("default", "C", true),
      ("default", "D", true),
      ("default", "d", false),
    ],
  )
}

#[test]
fn parsing_using_trivial_custom_state() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"A => match : BYTE  (65 /* A */ | 66 /* B */) { shift then pass }"##,
    &[("default", "A ", true), ("default", "B", true), ("default", "C", false)],
  )
}

#[test]
fn json_parser() -> SherpaResult<()> {
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../grammar/json/json.sg")
    .canonicalize()
    .unwrap();
  compile_and_run_grammar(std::fs::read_to_string(grammar_source_path.as_path())?.as_str(), &[
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
  compile_and_run_grammar(
    r##"
EXPORT A as A
EXPORT B as B
      
<> A > "a" "b" "c" $

<> B > "c" "b" "a"    
    "##,
    &[("A", "abc ", false), ("A", "abc", true), ("B", "cba", true), ("B", "cba ", true)],
  )
}

#[test]
pub fn test_sgml_like_grammar_parsing() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
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
"##,
    &[("default", "<i -test : soLongMySwanSong - store { test } <i>>", true)],
  )
}

#[test]
fn generic_grammar() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
IGNORE { c:sp c:nl } 

<> script > block(+)

<> block > "DECLARE" tk:name "{" declare_content(+) "}"

<> declare_content > execute_content(+)

<> execute_content > "aaa"(+)

<> name > c:id(+)
"##,
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
  compile_and_run_grammar(
    r##"

<> R > tk:A "ly"

<> A > "test" | "tester" | 'testing'
"##,
    &[("default", "testly", true), ("default", "testerly", true), ("default", "testingly", false)],
  )
}

#[test]
pub fn c_style_comment_blocks() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
    <> A > tk:comment
    
    <> comment > tk:block  | tk:line  | c:id(+)
       
    <> block > "/*"  ( c:sym | c:id | c:sp )(*) "*/"
    
    <> line > "//"  ( c:sym | c:id | c:sp )(*) c:nl?
    "##,
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
  compile_and_run_grammar(
    r##"
    IGNORE { c:sp c:nl }
    
    <> json > '{'  value(*',') '}'
    
    <> value > tk:string ':' tk:string
    
        | '"test"'{1} ':' c:num
    
    <> string > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) '\"'
        
    <> escape > "\\"{101}   ( c:sym | c:num | c:sp | c:id | c:nl)
    "##,
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
  compile_and_run_grammar(
    r##"
    <> sci_number > tk:number
    
    <> number > ( '+' | '-' )? c:num(+) ( '.' c:num(+) )? ( ( 'e' | 'E' ) ( '+' | '-' )? c:num(+) )?
    "##,
    &[("default", r##"2.3e-22"##, true), ("default", r##"0.3e-22"##, true)],
  )
}

#[test]
fn temp() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"<> A > hello" "world


        "##,
    &[
      ("default", r##""""##, true),
      ("default", r##""\\""##, true),
      ("default", r##""1234""##, true),
      ("default", r##""12\"34""##, true),
    ],
  )
}

#[test]
fn escaped_string() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
        <> string > tk:string_tk
        
        <> string_tk > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) "\""
        
        <> escape > "\\"  ( c:sym | c:num | c:sp | c:id )
        "##,
    &[
      ("default", r##""""##, true),
      ("default", r##""\\""##, true),
      ("default", r##""1234""##, true),
      ("default", r##""12\"34""##, true),
    ],
  )
}

fn compile_and_run_grammar(source: &str, inputs: &[(&str, &str, bool)]) -> SherpaResult<()> {
  build_states(source, "".into(), Default::default(), &|TestPackage { db, states, .. }| {
    let (bc, state_map) = compile_bytecode(&db, states)?;

    for (entry_name, input, should_pass) in inputs {
      let ok = Parser::new(&mut ((*input).into()), &bc)
      .completes(
        db.get_entry_offset(entry_name, &state_map).expect(&format!(
        "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
        db.entry_points().iter().map(|e| {
          e.entry_name.to_string(db.string_store())
        }).collect::<Vec<_>>().join(" | ")
      )) as u32)
      .is_ok();

      if (ok != *should_pass) {
        Parser::new(&mut ((*input).into()), &bc)
      .collect_shifts_and_skips(
        db.get_entry_offset(entry_name, &state_map).expect(&format!(
        "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
        db.entry_points().iter().map(|e| {
          e.entry_name.to_string(db.string_store())
        }).collect::<Vec<_>>().join(" | ")
      )) as u32, 0, &mut console_debugger(db.to_owned(), Default::default()));
        panic!(
          "\n\nParsing of input\n   \"{input}\"\nthrough entry point [{entry_name}] should {}.\n",
          if *should_pass { "pass" } else { "fail" }
        );
      }
    }

    SherpaResult::Ok(())
  })
}

// Sorts reduce functions according to their respective
// rules. This assumes the number of rules in the array
// matches the number of rules in the parser.
fn map_reduce_function<'a, R, ExtCTX, ASTNode>(
  db: &ParserDatabase,
  fns: Vec<(&str, usize, fn(*mut ParseContext<R, ExtCTX>, &AstStackSlice<AstSlot<ASTNode>, true>))>,
) -> Vec<Reducer<R, ExtCTX, ASTNode, true>>
where
  R: ByteReader + MutByteReader,
  ASTNode: AstObject,
{
  fns
    .into_iter()
    .filter_map(|(name, rule_number, b)| {
      let prod = db.prod_from_name(name);
      if prod != Default::default() {
        let rule_id = db.prod_rules(prod).unwrap()[rule_number];
        Some((Into::<usize>::into(rule_id), b))
      } else {
        None
      }
    })
    .collect::<OrderedMap<_, _>>()
    .into_values()
    .collect::<Vec<_>>()
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
      let (bc, _) = compile_bytecode(&db, states)?;

      let mut parser = Parser::new(&mut ("hello\nworld\n\ngoodby\nmango".into()), &bc);
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
