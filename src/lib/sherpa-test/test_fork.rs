use crate::{utils::_write_disassembly_to_temp_file_, *};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::*;
use sherpa_rust_runtime::{parsers::fork::ForkableParser, types::*};
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

  let input = r#"fn ( d:2){  test:2  } {} green toast ; ;"#;

  println!("------------------");

  let root_path = PathBuf::from("test.sg");

  let mut grammar = SherpaGrammarBuilder::new();

  grammar.add_source_from_string(source, &root_path, false)?;

  let parser_data = grammar.build_db(&root_path)?.build_parser(ParserConfig::default().cst_editor())?.optimize(false)?;

  parser_data._write_states_to_temp_file_()?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db())?;

  let mut parser = pkg.get_parser()?;

  parser.fork_parse(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?)?;

  Ok(())
}
