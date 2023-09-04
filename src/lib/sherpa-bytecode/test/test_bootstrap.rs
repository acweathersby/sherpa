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

  let parser = database.build_parser(Default::default())?.optimize(false)?;

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
  let parser = database.build_parser(Default::default())?.optimize(false)?;
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
