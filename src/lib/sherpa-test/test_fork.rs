use crate::{
  utils::{_write_disassembly_to_temp_file_, _write_states_to_temp_file_},
  *,
};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::*;
use sherpa_rust_runtime::types::*;
use std::path::PathBuf;

#[test]
pub fn construct_forking_parser_glr() -> SherpaResult<()> {
  let source = r#"

  IGNORE { c:sp } 


  <> F > E "test"

  <> E > A " !" | B

  <> B > "id" "()" " !"

  <> A > "id" "()"
  
   "#;

  let input = r#"id () ! test"#;

  let root_path = PathBuf::from("test.sg");

  let mut grammar = SherpaGrammarBuilder::new();

  grammar.add_source_from_string(source, &root_path, false)?;

  let parser_data =
    grammar.build_db(&root_path)?.build_parser(ParserConfig::default().cst_editor().enable_fork(true))?.optimize(false)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db())?;

  let mut parser = pkg.get_parser()?;

  parser.fork_parse(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?)?;

  Ok(())
}
