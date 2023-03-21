use super::utils::{console_debugger, TestConfig};
use crate::{
  journal::config::DebugConfig,
  test::utils::test_runner,
  Config,
  SherpaResult,
};

#[test]
fn escaped_string() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
        <> string > tk:string_tk
        
        <> string_tk > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) "\""
        
        <> escape > "\\"  ( c:sym | c:num | c:sp | c:id )
        "##,
    &[
      ("string", r##""""##, true),
      ("string", r##""\\""##, true),
      ("string", r##""1234""##, true),
      ("string", r##""12\"34""##, true),
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
    &[
      ("sci_number", r##"2.3e-22"##, true),
      ("sci_number", r##"0.3e-22"##, true),
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

    | '"test"' ':' c:num

<> string > '"' ( c:sym | c:num | c:sp | c:id | escape )(*) '\"'
    
<> escape > "\\"  ( c:sym | c:num | c:sp | c:id )
    "##,
    &[
      ("json", r##"{ "test" : 2  }"##, true),
      ("json", r##"{ "tester" : "mango"  }"##, true),
      ("json", r##"{ "tester" : 2  }"##, false),
      ("json", r##"{ "test" : "mango"  }"##, false),
    ],
  )
}

#[test]
pub fn cpp_comment_blocks() -> SherpaResult<()> {
  compile_and_run_grammar(
    r#"
<> A > tk:comment

<> comment > tk:block  | tk:line  | c:id(+)
   
<> block > "/*"  ( c:sym | c:id | c:sp )(*) "*/"

<> line > "//"  ( c:sym | c:id | c:sp )(*) c:nl?

"#,
    &[
      ("A", r##"//test"##, true),
      ("A", r##"//\n"##, true),
      ("A", r##"/* triangle */"##, true),
      ("A", r##"walker"##, true),
    ],
  )
}

fn compile_and_run_grammar(
  grammar: &'static str,
  test_inputs: &[(&str, &str, bool)],
) -> SherpaResult<()> {
  test_runner(
    &test_inputs.iter().map(|a| a.into()).collect::<Vec<_>>(),
    Some(Config {
      debug: DebugConfig { allow_parse_state_name_collisions: true },
      ..Default::default()
    }),
    TestConfig {
      optimize: true,
      print_disassembly: true,
      llvm_parse: true,
      bytecode_parse: true,
      grammar_string: Some(grammar),
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )?;

  SherpaResult::Ok(())
}
