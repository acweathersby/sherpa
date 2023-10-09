use crate::utils::{_write_disassembly_to_temp_file_, _write_states_to_temp_file_};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::*;
use sherpa_rust_runtime::{parsers::error_recovery::ErrorRecoveringDatabase, types::*};
use std::path::PathBuf;

#[test]
pub fn construct_error_recovering_parser() -> SherpaResult<()> {
  let source = r#"

  IGNORE { c:sp }

  <> taco > apple ";" $

  <> apple > topic ";"

  <> topic > test "green" "toast"

  <> test > fn "{}" 

  <> fn > ("fn" | "funct" | "function") "(" field(*",") ")" "{" field(*",") "}" 

  <> field > tk:id ":" val

  <> val > c:num

  <> id > c:id(+)
  
   "#;

  let input = r#"fn()(}greentoast;;"#;

  let root_path = PathBuf::from("test.sg");
  let mut grammar = SherpaGrammarBuilder::new();
  grammar.add_source_from_string(source, &root_path, false)?;

  let config = ParserConfig::default().cst_editor();
  let parser_data = grammar.build_db(&root_path, &config)?.build_parser(config)?.optimize(false)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db())?;

  pkg.parse_with_recovery(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?)?;

  Ok(())
}

#[test]
pub fn construct_error_recovering_erlang_toy() -> SherpaResult<()> {
  let source = r#"

  IGNORE { c:sp c:nl }
  
  <> fun > tk:name args body $
  
  <> args > "()"
  
  <> body > "->" expr

  <> expr > call

  <> call > "fn " tk:name "(" ")"

  <> name > c:id(+)
  
   "#;

  let input = "";

  println!("------------------");

  let root_path = PathBuf::from("test.sg");
  let mut grammar = SherpaGrammarBuilder::new();
  grammar.add_source_from_string(source, &root_path, false)?;

  let config = ParserConfig::default().cst_editor();
  let parser_data = grammar.build_db(&root_path, &config)?.build_parser(config)?.optimize(false)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db())?;

  let result = pkg.parse_with_recovery(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?)?;

  // assert_eq!(result.trees().first().to_string(), "<name>()->fn <name>()");

  Ok(())
}
