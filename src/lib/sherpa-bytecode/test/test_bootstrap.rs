//!
//! End to end test for the compilation of a sherpa grammar parser the
//! using bytecode parser engine.

/*
use crate::{
  test::utils::{path_from_source, test_runner, TestConfig},
  Journal,
  SherpaResult,
};

use super::utils::{console_debugger, PrintConfig, TestInput};

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn compile_sherpa_grammar_v2_and_parse_simple_expression() -> SherpaResult<Journal> {
  test_runner(
    &[TestInput {
      entry_name:     "grammar",
      input:          r##"<> t > "\\" "##,
      should_succeed: true,
    }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v2_0_0/grammar.sg")?),
      //llvm_parse: true,
      bytecode_parse: true,
      optimize: false,
      debugger_handler: Some(&|g| {
        console_debugger(g, PrintConfig {
          display_scanner_output: true,
          display_input_data: true,
          display_instruction: true,
          display_state: true,
          ..Default::default()
        })
      }),
      ..Default::default()
    },
  )
}

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn compile_sherpa_grammar_and_parse_simple_grammar_expression() -> SherpaResult<Journal> {
  test_runner(
    &[TestInput {
      entry_name:     "grammar",
      input:          r##"<> t > "t" :ast { $2 + $3 + tok<1,2> } <> B > [unordered "d" "b" "c" ]"##,
      should_succeed: true,
    }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
      llvm_parse: true,
      bytecode_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )
}

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn compile_sherpa_grammar_and_parse_trivial_grammar_expression() -> SherpaResult<Journal> {
  test_runner(
    &[TestInput {
      entry_name:     "grammar",
      input:          r##"<> hello > 'world'"##,
      should_succeed: true,
    }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
      llvm_parse: true,
      bytecode_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )
}

/// Test declared symbol type evaluator
#[test]
fn compile_sherpa_grammar_and_run_type_eval() -> SherpaResult<Journal> {
  test_runner(&[("type_eval", r##"123456577"##, true).into()], None, TestConfig {
    grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
    llvm_parse: true,
    bytecode_parse: true,
    debugger_handler: Some(&|g| console_debugger(g, Default::default())),
    ..Default::default()
  })
}

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn compile_ir_state() -> SherpaResult<Journal> {
  test_runner(
    &[TestInput {
      entry_name:     "state",
      input:          r##"
state [test]

      pass

"##,
      should_succeed: true,
    }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
      llvm_parse: true,
      bytecode_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )
}

#[test]
fn compile_sherpa_grammar_and_parse_root_sherpa_grammar_file() -> SherpaResult<()> {
  let input = r#"
  NAME sherpa

  IMPORT ./symbol as sym
  IMPORT ./syntax as syn
  IMPORT ./ir as ir
  IMPORT ./comment as cmt
  IMPORT ./ascript as ast

  IGNORE { c:sp c:nl tk:cmt::line tk:cmt::block }

  EXPORT grammar as grammar
  EXPORT ast::struct as ast_struct
  EXPORT ast::expression as ast_expression
  EXPORT ir::state as ir



  <> grammar >

          preamble(*) ( non-terminal | append_nonterminal )(+)

              :ast { t_Grammar, c_Version_1_0, preamble:$1, nonterminals:$2, tok }

  <> preamble >

          export_clause

          | import_clause

          | name_clause

          | ignore_clause

  <> export_clause >

          "EXPORT" sym::non_terminal (( "AS" | "as" ) sym::identifier)?

              :ast { t_Export, c_Preamble, non-terminal:$2, reference:$3 }

  <> import_clause >

          "IMPORT" ( c:id | c:sym  )(+) c:sp ( "AS" | "as" ) sym::identifier

              :ast { t_Import, c_Preamble, uri: str($2), reference:str($5), tok }

  <> ignore_clause >

          "IGNORE" "{"  ( sym::terminal_non_terminal | sym::terminal | sym::class )(+) "}"

              :ast { t_Ignore, c_Preamble, symbols: $3 }

  <> name_clause >

          "NAME" sym::identifier

              :ast { t_Name, c_Preamble, name: str($2) }

  <> non-terminal >

          "<"  (template_name)(*",")^t ">" "lazy"?^l sym::priority?^p sym::non_terminal^n">" rules^r

              :ast { t_Non-terminal, is_lazy:bool($l), priority:$p, name:str($n), name_sym:$n, rules: $r, template_names:$t, tok }

  <> append_nonterminal >

          "+>" sym::priority?^p sym::non_terminal^n ">" rules^r

              :ast { t_Non-terminal, is_append: true, priority:$p, name:str($n), name_sym:$n, rules: $r, tok }

  <> template_name >

      sym::identifier

             :ast str(tok)

  <> rules >

          rule(+"|")

  <> rule >

          "!"?^p ( sym::annotated_symbol | any_group )(+)^s ast_definition?^a
          syntax_definition?^syn recover_definition?^rec

                :ast {
                  t_Rule,
                  is_priority:bool($p),
                  symbols:$s,
                  ast_definition:$a,
                  syntax_definition:$syn,
                  recover_definition:$rec, tok
                }


  <> ast_definition >

          ":ast" ast::body^ast

              :ast  { t_Ascript, c_Function, ast:$ast, tok }

  <> syntax_definition >

          ":syn" syn::declaration^syn

  <> recover_definition >

          ":rec" "{" ir::state^state "}"

              :ast  { t_Recovery, c_Function, state:$state, tok }


  +> sym::symbol > group

  <> group >

          "(" rules ")"{1}

              :ast { t_Group_Non-terminal, c_Symbol, rules:$2,  tok }

  <> any_group >

          "[" "unordered"? sym::annotated_symbol(+)^s ']'

              :ast { t_AnyGroup, unordered: bool($2), symbols:$s, tok }


"#;
  test_runner(
    &[TestInput { entry_name: "grammar", input, should_succeed: true }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
      bytecode_parse: true,
      llvm_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, PrintConfig { ..Default::default() })),
      ..Default::default()
    },
  )?;

  SherpaResult::Ok(())
}
*/
