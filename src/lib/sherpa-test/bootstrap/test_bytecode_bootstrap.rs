//!
//! End to end test for the compilation of a sherpa grammar parser the
//! using bytecode parser engine.

use sherpa_bytecode::compile_bytecode;
use sherpa_core::{ParserConfig, SherpaGrammar, SherpaResult};
use sherpa_rust_runtime::types::{ParserProducer, RuntimeDatabase, StringInput};

use crate::{
  debug::{file_debugger, PrintConfig},
  utils::_write_states_to_temp_file_,
};

#[test]
fn test_full_grammar() -> SherpaResult<()> {
  let grammar_folder =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/sherpa/2.0.0").canonicalize().unwrap();

  // Build our parser;

  let sherpa_grammar = grammar_folder.join("grammar.sg");
  let mut grammar = SherpaGrammar::new();
  let config = ParserConfig::new().hybrid().enable_fork(false);
  let database = grammar.add_source(&sherpa_grammar)?.build_db(&sherpa_grammar, config)?;
  let parser_builder = database.build_states(config)?.build_ir_parser(true, false)?;
  let pkg = compile_bytecode(&parser_builder, true)?;

  // Gather list of files to validate. This includes the latest sherpa grammar
  // itself and all files found in the "validate" folder adjacent to the latest
  // grammar version folder.

  let grammar_files =
    grammar_folder.read_dir()?.filter_map(|f| f.ok()).filter(|f| f.file_type().is_ok_and(|t| t.is_file())).map(|f| f.path());

  let validation_files = grammar_folder
    .join("../validate")
    .canonicalize()?
    .read_dir()?
    .filter_map(|f| f.ok())
    .filter(|f| f.file_type().is_ok_and(|t| t.is_file()))
    .map(|f| f.path());

  _write_states_to_temp_file_(&parser_builder)?;

  let db = parser_builder.get_db();

  let mut parser = pkg.get_parser()?;

  parser.set_debugger(file_debugger(
    db.to_owned(),
    PrintConfig {
      display_scanner_output: false,
      display_instruction: true,
      display_input_data: true,
      display_state: true,
      ..Default::default()
    },
    pkg.address_to_state_name.clone(),
  ));

  let entry = pkg.get_entry_data_from_name("grammar").expect("Grammar export should be enterable");

  for file_path in validation_files.chain(grammar_files.into_iter()) {
    println!("{}", file_path.file_name().and_then(|f| f.to_str()).expect("Could not read file name"));

    let input = std::fs::read_to_string(file_path)?;

    match parser.collect_shifts_and_skips(&mut StringInput::from(input), entry) {
      Ok(_) => {}
      Err(_) => {
        return Err(sherpa_core::SherpaError::Text("Failed to parse input".into()));
      }
    };
  }
  Ok(())
}
