use crate::{
  test::utils::{map_reduce_function, TestParser},
  *,
};
use sherpa_core::{test::utils::build_parse_states_from_source_str as build_states, *};
use sherpa_rust_runtime::types::{ast::AstSlot, bytecode::FIRST_PARSE_BLOCK_ADDRESS, SherpaParser};
use std::path::PathBuf;

use super::utils::compile_and_run_grammars;

#[test]
pub fn basic_scanner() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
      IGNORE { c:sp c:nl }

      <> A > "sym" [ tk:ref? "?" ?  "ref"? ]!
          | "sym"

      <> ref > "^" id

      <> id > tk:id_tok

      <> id_tok >  ( "-" | "_" | c:id ) ( c:id | '_' | '-' | c:num )(*)

  "#],
    &[("default", "sym?^name", true)],
  )
}

#[test]
pub fn basic_left_recursion() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
      <> A > A c:num
          | c:num
  "#],
    &[("default", "123", true)],
  )
}

#[test]
pub fn bread_crumb_parsing() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
      IGNORE { c:sp } 

      <> E > A " A-test" | B " B-test"

      <> B > R "=>" "()"

      <> A > O "=>" "()"

      <> O > I

      <> R > I

      <> I > "id"
  "#],
    &[("default", "id=>() A-test", true)],
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
    &[("default", "xd", true), ("default", "xxxxc", true), ("default", "xxxxf", false)],
  )
}

#[test]
pub fn recursive_nonterminal() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
  
      <> expr > expr "+" expr      | c:num 
  "#],
    &[("default", "1+2+3", true)],
  )
}

#[test]
pub fn expr_term() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"

    <> expr > expr '+' term 
    | expr '-' term   
    | term            
    

    <> term > '(' expr ')'   
        | num             
        

    <> num > '0'            
      | '1'              
      

"#],
    &[("default", "0+(1-1)", true)],
  )
}

#[test]
pub fn precedence() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r#"
    IGNORE { tk:space }

    <> expr > expr "+"{1} expr{1}
    | expr "^"{4} expr{4}
    | expr "*"{3} expr{3}
    | expr "/"{2} expr{2}
    | expr "-"{1} expr{1}
    | c:num

    <> space > c:sp(+)

"#],
    &[("default", "1 + 2 * 2 ^ 2 + 2 * 2 + 1 + 1", true)],
  )
}

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
    &[/* ("default", "t => g :t t", true), ("default", ":t t => a :t!", true), */ ("default", ":t t => g :t! t!", true)],
  )
}

#[test]
pub fn construct_basic_recursive_descent() -> SherpaResult<()> {
  compile_and_run_grammars(&[r#"<> A > B ' ' C <> B > 'hello' <> C > 'world' "#], &[("default", "hello world", true)])
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
pub fn cross_source_symbol_lookup() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[
      r#"
IMPORT B as B
IGNORE { c:sp }

<> result > import_clause "end"

<> import_clause >

    "A" ( c:id | c:sym | c:num )(+) c:sp "as" B::id
"#,
      r#"
<> id > tk:id_tok
  
<> id_tok > ( "-" | "_" | c:id ) ( c:id | '_' | '-' | c:num )(*)
"#,
    ],
    &[("default", "A test as id end", true)],
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
fn parser_of_grammar_with_append_nonterminals() -> SherpaResult<()> {
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
    ("entry", r##"[]"##, true),
    ("entry", r##"{"test":[{ "test":"12\"34", "test":"12\"34"}]}"##, true),
  ])
}

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
IGNORE { c:sp c:nl }

<> element_block > '<' component_identifier
    ( element_attribute(+) )?
    ( element_attributes | general_data | element_block | general_binding )(*)
    ">"

<> component_identifier >
    identifier ( ':' identifier )?

<> element_attributes >c:nl element_attribute(+)

<> element_attribute > '-' identifier attribute_chars c:sp


    | '-' identifier ':' identifier

    | '-' "store" '{' local_values? '}'
    | '-' "local" '{' local_values? '}'
    | '-' "param" '{' local_values? '}'
    | '-' "model" '{' local_values? '}'

<> general_binding > ':' identifier

<> local_values > local_value(+)

<> local_value > identifier ( '`' identifier )? ( '='  c:num )? ( ',' )(*)


<> attribute_chars > ( c:id | c:num | c:sym  )(+)
<> general_data > ( c:id | c:num  | c:nl  )(+)

<> identifier > tk:tok_identifier

<> tok_identifier > ( c:id | c:num )(+)
"##],
    &[("default", "<i -t: a>", true), ("default", "<i -test : soLongMySwanSong - store { test } <i> <i>>", true)],
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

<> A > "test"{:1} | "tester"{:1} | 'testing'
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
pub fn recursive_skipped_comments() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
    IGNORE { tk:comment c:sp }

    <> A > "hello" "world"

    <> comment > '/' "*"{:9999} comment_body '*'{:9999} "/"{:9999}

    <> comment_body >  ( c:nl | c:sym | c:num | c:sp | c:id | comment )(+)
    "##],
    &[
      ("default", r##"hello /* */ world"##, true),
      ("default", r##"hello /* This is the only way to go /* to */ the moon */ world"##, true),
      ("default", r##"hello /* This is the only way to go /* to */ the moon / world"##, false),
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
        <> string > tk:string_tk "a"
        
        <> string_tk > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) "\""
        
        <> escape > "\\"{:9999}  ( c:sym | c:num | c:sp | c:id )
        "##],
    &[
      ("default", r##"""a"##, true),
      ("default", r##""\\"a"##, true),
      ("default", r##""1234"a"##, true),
      ("default", r##""12\"34"a"##, true),
    ],
  )
}

#[test]
fn simple_newline_tracking_sanity() -> SherpaResult<()> {
  compile_and_run_grammars(
    &[r##"
    IGNORE { c:sp c:nl }

    <> test > 'hello' P

    <> P > 'world' 'goodby' B

    <> B > 'mango'
        "##],
    &[("default", "hello\nworld\n\ngoodby\nmango", true)],
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
    true,
    &|tp| {
      let pkg = compile_bytecode(&tp, true)?;

      let TestPackage { db, .. } = tp;

      let mut parser = TestParser::new(&mut ("hello\nworld\n\ngoodby\nmango".into()), &pkg);
      parser.init_parser(FIRST_PARSE_BLOCK_ADDRESS);
      let result = parser.parse_ast(
        &map_reduce_function(&db, vec![
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

      assert!(matches!(result, Result::Ok(AstSlot(1010101, ..))), "{:?}", result);

      SherpaResult::Ok(())
    },
  )
}
