//!
//! End to end test for the compilation of a sherpa grammar parser the
//! using bytecode parser engine.

use sherpa_core::{ParserStore, PrintConfig, SherpaGrammarBuilder, SherpaResult};
use sherpa_rust_runtime::types::SherpaParser;

use crate::compile_bytecode;

use super::utils::TestParser;

fn build_sherpa_grammar_and_parse(input: &str) -> SherpaResult<()> {
  let sherpa_grammar =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/sherpa/2.0.0/grammar.sg").canonicalize().unwrap();

  let grammar = SherpaGrammarBuilder::new();

  let database = grammar.add_source(&sherpa_grammar)?.build_db(&sherpa_grammar)?;

  let parser = database.build_parser()?.optimize(false)?;

  #[cfg(all(debug_assertions))]
  parser.write_states_to_temp_file()?;

  let (bc, state_map) = compile_bytecode(&parser, true)?;

  let db = parser.get_db();

  let mut cd = if true {
    #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
    sherpa_core::file_debugger(db.to_owned(), PrintConfig {
      display_scanner_output: false,
      display_instruction: false,
      display_input_data: true,
      display_state: true,
      ..Default::default()
    })
  } else {
    None
  };

  match TestParser::new(&mut ((*input).into()), &bc).collect_shifts_and_skips(
    db.get_entry_offset("grammar", &state_map).expect(&format!(
      "\nCan't find entry offset for entry point [default].\nValid entry names are\n    {}\n",
      db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
    )) as u32,
    4,
    &mut cd.as_deref_mut(),
  ) {
    sherpa_rust_runtime::types::ShiftsAndSkipsResult::Accepted { .. } => Ok(()),
    sherpa_rust_runtime::types::ShiftsAndSkipsResult::FailedParse(d) => Err(d.into()),
    sherpa_rust_runtime::types::ShiftsAndSkipsResult::IncorrectNonTerminal { expected_nterm, actual_nterm, .. } => {
      Err(sherpa_core::SherpaError::Text(format!("Expect {expected_nterm} got {actual_nterm}")))
    }
  }
}

#[test]
fn test_trivial_grammar() -> SherpaResult<()> {
  build_sherpa_grammar_and_parse("<> test > 'test'^t ")
}

#[test]
fn grammar_with_import() -> SherpaResult<()> {
  build_sherpa_grammar_and_parse("IMPORT a as t \n<> test > 'test'^t ")
}

#[test]
fn grammar_with_append() -> SherpaResult<()> {
  build_sherpa_grammar_and_parse("<> a > b +> c > d <> e > f")
}

#[test]
fn test_full_grammar() -> SherpaResult<()> {
  let file_names = ["grammar.sg", "ascript.sg", "ir.sg", "symbol.sg", "token.sg"];
  let grammar_folder =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/sherpa/2.0.0").canonicalize().unwrap();
  let sherpa_grammar = grammar_folder.join("grammar.sg");
  let grammar = SherpaGrammarBuilder::new();
  let database = grammar.add_source(&sherpa_grammar)?.build_db(&sherpa_grammar)?;
  let parser = database.build_parser()?.optimize(false)?;
  let (bc, state_map) = compile_bytecode(&parser, true)?;

  #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
  parser.write_states_to_temp_file()?;

  for file_name in file_names {
    println!("{file_name}");

    let input = std::fs::read_to_string(grammar_folder.join(file_name))?;

    let db = parser.get_db();
    let mut cd = if true {
      #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
      sherpa_core::file_debugger(db.to_owned(), PrintConfig {
        display_scanner_output: false,
        display_instruction: false,
        display_input_data: true,
        display_state: true,
        ..Default::default()
      })
    } else {
      None
    };

    match TestParser::new(&mut ((&input).into()), &bc).collect_shifts_and_skips(
      db.get_entry_offset("grammar", &state_map).expect(&format!(
        "\nCan't find entry offset for entry point [default].\nValid entry names are\n    {}\n",
        db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
      )) as u32,
      4,
      &mut cd.as_deref_mut(),
    ) {
      sherpa_rust_runtime::types::ShiftsAndSkipsResult::Accepted { .. } => {}
      sherpa_rust_runtime::types::ShiftsAndSkipsResult::FailedParse(d) => return Err(d.into()),
      sherpa_rust_runtime::types::ShiftsAndSkipsResult::IncorrectNonTerminal { expected_nterm, actual_nterm, .. } => {
        return Err(sherpa_core::SherpaError::Text(format!("Expect {expected_nterm} got {actual_nterm}")));
      }
    };
  }
  Ok(())
}

#[test]
fn test_complex_grammar() -> SherpaResult<()> {
  build_sherpa_grammar_and_parse(
    r#"
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


"#,
  )
}
