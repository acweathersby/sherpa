use sherpa_rust_runtime::types::{ParseAction, SherpaParser};

use super::utils::{console_debugger, test_runner, TestConfig};
#[cfg(test)]
use crate::journal::Journal;
use crate::{
  bytecode::compile_bytecode,
  journal::config::DebugConfig,
  parser::{compile_parse_states, optimize_parse_states},
  test::{test_reader::TestUTF8StringReader, utils::TestInput},
  types::{GrammarStore, SherpaResult},
  util::get_num_of_available_threads,
  Config,
};
use std::path::PathBuf;

#[test]
fn test_trivial_peek() -> SherpaResult<Journal> {
  test_runner(&[("Term", "xxxxd =  \" test test test \"", true).into()], None, TestConfig {
    grammar_string: Some(
      r#" 
      IGNORE { c:sp } 
      
      <> Term > Ident "=" Value

      <> Value > '\"' Ident(+' ') ' '? '\"'

      <> Ident > tk:ident

      <> ident > c:id(+)
      "#,
    ),
    bytecode_parse: true,
    llvm_parse: true,
    debugger_handler: Some(&|g| console_debugger(g, Default::default())),
    ..Default::default()
  })
}

#[test]
pub fn production_reduction_decisions() -> SherpaResult<Journal> {
  let input = "
<> A > B | C | R 
     | 'g'

<> C > 'c' c

<> c >  'a' | 'b' 

<> B > C 'd'
     | 'a' 'c'

<> R > G 'o' 
    | C 'x'

<> G > 'xx'

  ";

  test_runner(&[], None, TestConfig { grammar_string: Some(input), ..Default::default() })
}

#[test]
pub fn compile_production_states_with_basic_grammar() -> SherpaResult<Journal> {
  let input = "<> A > 'h' 'e' 'l' 'l' 'o'";

  test_runner(
    &[TestInput { entry_name: "A", input: "heleo", should_succeed: true }],
    None,
    TestConfig {
      bytecode_parse: true,
      grammar_string: Some(input),
      ..Default::default()
    },
  );

  test_runner(
    &[TestInput { entry_name: "A", input: "hello", should_succeed: true }],
    None,
    TestConfig {
      bytecode_parse: true,
      grammar_string: Some(input),
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )
}

#[test]
pub fn compile_production_states_with_basic_grammar_with_one_optional_token(
) -> SherpaResult<Journal> {
  let input = "<> A > 'h'? 'e'? 'l' 'l' 'o'";

  test_runner(&[], None, TestConfig { grammar_string: Some(input), ..Default::default() })
}

#[test]
pub fn compile_production_states_with_basic_grammar_with_left_recursion() -> SherpaResult<Journal> {
  let input = "<> A > A '1' | '2' ";

  test_runner(&[], None, TestConfig { grammar_string: Some(input), ..Default::default() })
}

#[test]
pub fn compile_production_states_with_synthesized_scanner_state() -> SherpaResult<Journal> {
  let input = "<> A > '1' | '2' | '3' ";

  test_runner(&[], None, TestConfig { grammar_string: Some(input), ..Default::default() })
}

#[test]
pub fn basic_math_expressions() -> SherpaResult<Journal> {
  test_runner(&[("P", "1*2^2+3", true).into()], None, TestConfig {
    grammar_string: Some(
      r#"
    <> P > S 
    <> S > S "+" M  | M
    <> M > M "*" E  | E
    <> E > T | T "^" T
    <> T > c:num
    "#,
    ),
    llvm_parse: true,
    bytecode_parse: true,
    ..Default::default()
  })
}

#[test]
pub fn generate_block_comment() -> SherpaResult<Journal> {
  let input = r#"
<> A > tk:comment

<> comment > tk:block  | tk:line  | c:sym
   
<> block > '&&'  (  c:sym  )(*) "%>"

<> line > '&/'  ( c:sym  )(*) c:nl

"#;

  test_runner(&[], None, TestConfig { grammar_string: Some(input), ..Default::default() })
}

#[test]
pub fn generate_production_state_with_scanner_function() -> SherpaResult<Journal> {
  let input = "
<> A > tk:B

<> B > C | D

<> C > 'a' D 'c'

<> D > 'a' 'b'
";

  test_runner(&[], None, TestConfig { grammar_string: Some(input), ..Default::default() })
}

#[test]
pub fn reports_grammar_with_conflicts_caused_by_an_import_merge() -> SherpaResult<()> {
  assert!(test_runner(&[], None, TestConfig {
    build_parse_states: true,
    grammar_path: Some(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../test/grammars/merge_conflict_host.sg")
        .canonicalize()
        .unwrap(),
    ),
    ..Default::default()
  })
  .is_faulty());

  SherpaResult::Ok(())
}

#[test]
pub fn generate_production_with_ambiguity() -> SherpaResult<()> {
  let result = test_runner(&[], None, TestConfig {
    grammar_string: Some(
      "
      <> A > B | C
      
      <> B > 'a' 'b' 'c' (*)
      
      <> C > 'a' 'b' 'c' (*)
      ",
    ),
    num_of_threads: 1,
    ..Default::default()
  });

  println!("{}", result);

  assert!(result.is_faulty());

  SherpaResult::Ok(())
}

#[test]
pub fn inner_goto_loops() -> SherpaResult<Journal> {
  test_runner(&[], None, TestConfig {
    grammar_string: Some(
      r##"


    <> A > B "r"
    
    <> B > "entry" ( C | D ) "e"?
    
    <> C > C c:id
       | E
       | "one"
    
    <> D > D "two"
         | "three"
         | E "four"
    
    <> E > "a" "b" "c"
    
    "##,
    ),
    print_parser_states_compile_reports: &["B"],
    ..Default::default()
  })
}

#[test]
pub fn generate_production_with_recursion() -> SherpaResult<Journal> {
  test_runner(&[], None, TestConfig {
    grammar_string: Some(
      "
  IGNORE { c:sp }
  
  EXPORT statement as entry
  
  NAME llvm_language_test
  
  <> statement > expression
  
  <> expression > sum 
  
  <> sum > mul '+' sum
      | mul
  
  <> mul > term '*' expression
      | term
  
  <> term > c:num
      | '(' expression ')'
  
  ",
    ),
    ..Default::default()
  })
}

#[test]
pub fn generate_scanner_production_with_recursion() -> SherpaResult<Journal> {
  test_runner(&[], None, TestConfig {
    grammar_string: Some(
      "
  IGNORE { c:sp }
  
  EXPORT statement as entry
  
  NAME llvm_language_test
  
  <> statement > tk:test tk:V
  
  <> test > V test?
      | A test 't'
  
  <> V > V c:num | 'dd'
  
  <> A > 'a' '-' 'b'
  
  ",
    ),
    ..Default::default()
  })
}

#[test]
pub fn merge_non_transitive_branches() -> SherpaResult<Journal> {
  test_runner(&[("A", "doggo ranger ready doggo ranger test", true).into()], None, TestConfig {
    grammar_string: Some(
      r##"
  IGNORE { c:sp }
    
  <> A > B "test"

  <> B > C "ready" C 

  <> C > D "ranger"

  <> D > "doggo"

  "##,
    ),
    bytecode_parse: true,
    llvm_parse: true,
    //debugger_handler: Some(&|g| console_debugger(g, Default::default())),
    ..Default::default()
  })
}

#[test]
fn test_peek() -> SherpaResult<Journal> {
  test_runner(&[], None, TestConfig {
    grammar_string: Some(
      r##"
  IGNORE { c:sp }
  
  <> term >  tk:ident '=' value_list
  
  <> value_list > '"' formal_value_list(+" ") '"'
  
  <> formal_value_list > ident
  
  <> ident > c:id(+) 
  "##,
    ),
    ..Default::default()
  })
}

#[test]
fn test_peek3() -> SherpaResult<Journal> {
  test_runner(&[("term", "x x x ( c )", true).into()], None, TestConfig {
    grammar_string: Some(
      r##"
      IGNORE { c:sp }
      
      <> term >  'x' A '(' c:id? ')'  :ast { t_Function_Definition }
              |  'x' B ';'           :ast { t_Type_Definition }
      
      <> A > Adent 'x'
      
      <> B > Bdent
      
      <> Adent > Cdent
      
      <> Cdent > c:id
      
      <> Bdent > c:id
      "##,
    ),
    bytecode_parse: true,
    llvm_parse: true,
    ..Default::default()
  })
}

#[test]
fn grammar_with_exclusive_symbols() -> SherpaResult<Journal> {
  test_runner(&[("A", "x x x c", true).into()], None, TestConfig {
    grammar_string: Some(
      r##" 
      IGNORE { c:sp } 
      
      <> A >   X 'c'
        | Y 'd'
      
      <> X > 'x' X?
      
      <> Y > 'x' Y?
        "##,
    ),
    bytecode_parse: true,
    llvm_parse: true,
    ..Default::default()
  })
}

const test_grammar: &'static str = r##"
IGNORE { c:sp c:nl }
EXPORT A as entry
NAME llvm_language_test

<> A > 'b' 'disaster' 'danger' | B 

<> B > 'a' 'disaster' 'ranger'

<> R > 'a'
"##;

#[test]
fn test_parsing_with_trivial_peek() -> SherpaResult<Journal> {
  test_runner(&[("A", "a disaster ranger", true).into()], None, TestConfig {
    grammar_string: Some(test_grammar),
    bytecode_parse: true,
    llvm_parse: true,
    ..Default::default()
  })
}

#[test]
fn ir_parser_build() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/v1_0_0/ir.sg").canonicalize()?,
  )?;

  j.flush_reports();

  let states = compile_parse_states(&mut j, 1)?;

  let states = optimize_parse_states(&mut j, states);

  compile_bytecode(&mut j, &states);

  assert!(!j.debug_error_report());

  SherpaResult::Ok(())
}

#[test]
pub fn reduction_on_root_production() -> SherpaResult<()> {
  test_runner(&[("A", "<<<d>>>", true).into()], None, TestConfig {
    grammar_string: Some(
      r#"
      NAME wick_element
      
      IGNORE { c:sp c:nl }
      
      <> A > '<' (B | A )(+) '>'
      
      <> B > c:id 
      "#,
    ),
    bytecode_parse: true,
    llvm_parse: true,
    //debugger_handler: Some(&|g| console_debugger(g, Default::default())),
    ..Default::default()
  })?;

  SherpaResult::Ok(())
}

#[test]
fn grammar_with_multiple_entry_points() -> SherpaResult<Journal> {
  test_runner(
    &[
      ("A", "abc", true).into(),
      ("B", "cba", true).into(),
      ("B", "abc", false).into(),
      ("A", "cba", false).into(),
    ],
    None,
    TestConfig {
      grammar_string: Some(
        r##"

IGNORE { c:sp }

EXPORT A
EXPORT B
      
<> A > "a" "b" "c"

<> B > "c" "b" "a"
      
      "##,
      ),
      llvm_parse: true,
      bytecode_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )
}

#[test]
fn handles_grammars_that_utilize_eof_symbol() -> SherpaResult<Journal> {
  test_runner(
    &[
      ("A", "abc ", false).into(),
      ("A", "abc", true).into(),
      ("B", "cba", true).into(),
      ("B", "cba ", true).into(),
    ],
    None,
    TestConfig {
      grammar_string: Some(
        r##"

EXPORT A
EXPORT B
      
<> A > "a" "b" "c" $

<> B > "c" "b" "a"
      
      "##,
      ),
      bytecode_parse: true,
      llvm_parse: true,
      ..Default::default()
    },
  )
}
#[cfg(all(feature = "llvm", not(feature = "wasm-target")))]
#[test]
pub fn tracks_line_numbers() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
    IGNORE { c:sp c:nl }

    <> A > \"A\"(+)
    ",
  );
  assert!(!j.debug_error_report(), "Should not have grammar errors");

  let states = compile_parse_states(&mut j, get_num_of_available_threads())?;
  let opt_states = optimize_parse_states(&mut j, states);
  let ctx = inkwell::context::Context::create();
  let mut parser = crate::types::JitParser::<_, u32, u32>::new(&mut j, opt_states, &ctx)?;
  let mut r = TestUTF8StringReader::new("A\n  A\n  A A\n    A\n   \n   \n  A");
  parser.set_reader(&mut r);

  parser.init_parser(0);

  let mut tok_count: usize = 0;

  let line_counts = [(0, 0), (1, 1), (2, 5), (2, 5), (3, 11), (6, 25)];

  loop {
    match parser.get_next_action(&mut None) {
      ParseAction::Shift { token_line_count, token_line_offset, .. } => {
        dbg!((token_line_count, token_line_offset));
        assert_eq!((token_line_count, token_line_offset), line_counts[tok_count]);
        tok_count += 1;
      }
      ParseAction::Accept { .. } => {
        break;
      }
      ParseAction::Error { .. } => panic!("Should not have failed to parse"),
      _ => {}
    }
  }

  assert_eq!(tok_count, line_counts.len());

  SherpaResult::Ok(())
}

#[test]
pub fn lr2() -> SherpaResult<()> {
  test_runner(
    &[(
      "grammar",
      r##"

<> test > c:id test test 'test'

test =!> 
  match : CLASS {
    (1|2) { 
      match : PRODUCTION ( sym::grammar ) { goto test }
    }
    { fail }
    fail-hint { "Let this be a lesson to you all. Don't mess with philips!" }
}

"##,
      true,
    )
      .into()],
    Some(Config {
      debug: DebugConfig { allow_parse_state_name_collisions: false, ..Default::default() },
      ..Default::default()
    }),
    TestConfig {
      //print_states: true,
      //print_disassembly: false,
      bytecode_parse: true,
      optimize: false,
      print_parser_states_compile_reports: &["grammar"],
      grammar_path: Some(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
          .join("../../grammar/v2_0_0/grammar.sg")
          .canonicalize()
          .unwrap(),
      ),
      debugger_handler: Some(&|g| {
        console_debugger(g, super::utils::PrintConfig {
          display_scanner_output: true,
          display_input_data: false,
          display_instruction: true,
          display_state: false,
          ..Default::default()
        })
      }),
      ..Default::default()
    },
  )?;

  SherpaResult::Ok(())
}

use super::utils::{console_debugger, TestConfig};
use crate::{journal::config::DebugConfig, test::utils::test_runner, Config, SherpaResult};
