use crate::{
  test::utils::{path_from_source, test_runner, TestConfig},
  Journal,
  SherpaResult,
};

use super::utils::{console_debugger, PrintConfig, TestInput};

#[test]
fn compile_and_parse_sherpa_grammar_with_bytecode_parser() -> SherpaResult<()> {
  let input = r#"
  NAME sherpa

  IMPORT ./script.hcg as ast 
  IMPORT ./symbol.hcg as sym
  
  IGNORE { c:sp c:nl }
  
  EXPORT grammar as grammar
  
  <> grammar > 
  
          preamble(*) ( production | append_production )(+)
  
              :ast { t_Grammar, c_Version_1_0, preamble:$1, productions:$2, tok }
  
  <> preamble >
  
          export_clause 
  
          | import_clause
  
          | name_clause
  
          | ignore_clause
  
  <> export_clause > 
  
          "EXPORT" sym::non_terminal (( "AS" | "as" ) sym::identifier)?
  
              :ast { t_Export, c_Preamble, production:$2, reference:$3 } 
  
  <> import_clause > 
  
          "IMPORT" ( c:id | c:sym  )(+)^test ( "AS" | "as" ) sym::identifier
  
              :ast { t_Import, c_Preamble, uri: str($2), reference:str($4), tok }
  
  <> ignore_clause >
  
          "IGNORE" "{"  ( sym::terminal | sym::class )(+) "}"
  
              :ast { t_Ignore, c_Preamble, symbols: $3 }
  
  <> name_clause >
  
          "NAME" sym::identifier
  
              :ast { t_Name, c_Preamble, name: str($2) }
  
  <> production > 
  
          "<"  (template_name)(*\, )^t \> \lazy?^l sym::priority?^p sym::non_terminal^n \> rules^r
  
              :ast { t_Production, is_lazy:bool($l), priority:$p, name:str($n), name_sym:$n, rules: $r, template_names:$t, tok }
  
  <> append_production > 
  
          "+>" sym::priority?^p sym::non_terminal^n \> rules^r
  
              :ast { t_Production, is_append: true, is_lazy:false, priority:$p, name:str($n), name_sym:$n, rules: $r, tok }
  
  <> template_name >  
  
      sym::identifier
  
             :ast str(tok)
  
  <> rules > 
  
          rule(+"|")
  
  <> rule > 
  
          \!?^p ( sym::annotated_symbol | any_group )(+)^s ast_definition?^a
  
              :ast { t_Rule, is_priority:bool($p), symbols:$s, ast_definition:$a, tok }
  
  <> ast_definition > 
  
          ":ast" ast::body^ast
  
              :ast  { t_Ascript, c_Function, ast:$ast, tok }
  
  
  +> sym::symbol > group
  
  +> sym::non_terminal > sym::non_terminal^p "<" sym::production_symbol^t ">"
  
          :ast { t_TemplateProductionSymbol, prod_sym:$p, template_productions:$t }
  
  
  <> group > 
  
          "(" rules ")"{1}
  
              :ast { t_Group_Production, c_Symbol, rules:$2,  tok }
  
  <> any_group > 
  
          \[ "unordered"? sym::annotated_symbol(+)^s \]
  
              :ast { t_AnyGroup, unordered: bool($2), symbols:$s, tok }
  
  
"#;
  test_runner(
    &[TestInput { entry_name: "grammar", input, should_succeed: true }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
      bytecode_parse: true,
      //optimize: false,
      debugger_handler: Some(&|g| {
        console_debugger(g, PrintConfig {
          display_input_data: true,
          display_scanner_output: true,
          ..Default::default()
        })
      }),
      ..Default::default()
    },
  )?;

  SherpaResult::Ok(())
}

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn compile_and_parse_sherpa_grammar_with_llvm_jit_parser() -> SherpaResult<Journal> {
  test_runner(
    &[TestInput {
      entry_name:     "grammar",
      input:          r##"<> t > "t" :ast { $2 + $3 + tok<1,2> } <> B > [unordered "d" "b" "c" ] "##,
      should_succeed: true,
    }],
    None,
    TestConfig {
      grammar_path: Some(path_from_source("grammar/v1_0_0/grammar.sg")?),
      //llvm_parse: true,
      bytecode_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      ..Default::default()
    },
  )
}
