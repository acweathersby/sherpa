use crate::utils::{_write_disassembly_to_temp_file_, _write_states_to_temp_file_};
use radlr_bytecode::compile_bytecode;
use radlr_core::*;
use radlr_rust_runtime::{parsers::error_recovery::ErrorRecoveringDatabase, types::*};
use std::path::PathBuf;

#[test]
pub fn construct_error_recovering_parser() -> RadlrResult<()> {
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

  let pool = radlr_core::worker_pool::StandardPool::new_with_max_workers().unwrap();

  let input = r#"fn()(}greentoast;;"#;

  let root_path = PathBuf::from("test.sg");
  let mut grammar = RadlrGrammar::new();
  grammar.add_source_from_string(source, &root_path, false)?;

  let config = ParserConfig::default().cst_editor();
  let parser_data = grammar.build_db(&root_path, config)?.build_states(config, &pool)?.build_ir_parser(false, false, &pool)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db(), config)?;

  pkg.parse_with_recovery(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?, &Default::default())?;

  Ok(())
}

#[test]
pub fn construct_error_recovering_erlang_toy() -> RadlrResult<()> {
  let source = r#"

  IGNORE { c:sp c:nl }
  
  <> fun > tk:name args body $
  
  <> args > "()"
  
  <> body > "->" expr

  <> expr > call

  <> call > "fn " tk:name "(" ")"

  <> name > c:id(+)
  
   "#;

  let input = "test test";

  eprintln!("------------------");

  let root_path = PathBuf::from("test.sg");
  let mut grammar = RadlrGrammar::new();
  grammar.add_source_from_string(source, &root_path, false)?;

  let pool = radlr_core::worker_pool::StandardPool::new_with_max_workers().unwrap();

  let config = ParserConfig::default().cst_editor();
  let parser_data = grammar.build_db(&root_path, config)?.build_states(config, &pool)?.build_ir_parser(false, false, &pool)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db(), config)?;

  let result =
    pkg.parse_with_recovery(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?, &Default::default())?;

  if let Some(best) = result.first() {
    for (_, sym) in &best.symbols {
      Printer::new(sym, true, &pkg).print();
      eprintln!("\n");
      Printer::new(sym, true, &pkg).print_all();
    }
    eprintln!("\n");
  }

  Ok(())
}

#[test]
pub fn temp_lab_test() -> RadlrResult<()> {
  let source = r#"

  IGNORE { c:sp }

  <> B > A(+) ";"

  <> A > "Hello" "{" tk:(c:id+)(+) "}" 
  
   "#;

  let input = "{a  { a } {bbbb }  ;";

  let root_path = PathBuf::from("test.sg");
  let mut grammar = RadlrGrammar::new();
  grammar.add_source_from_string(source, &root_path, false)?;

  let pool = radlr_core::worker_pool::StandardPool::new_with_max_workers().unwrap();

  let config = ParserConfig::default().cst_editor();
  let parser_data = grammar.build_db(&root_path, config)?.build_states(config, &pool)?.build_ir_parser(false, false, &pool)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db(), config)?;

  let result =
    pkg.parse_with_recovery(&mut StringInput::from(input), pkg.get_entry_data_from_name("default")?, &Default::default())?;

  eprintln!("------------------");
  if let Some(best) = result.first() {
    for (_, sym) in &best.symbols {
      for alt in split_alternates(sym) {
        Printer::new(alt.first().unwrap(), true, &pkg).print();
        eprintln!("\n");
      }
    }
    eprintln!("\n");
  }

  // assert_eq!(result.trees().first().to_string(), "<name>()->fn <name>()");

  Ok(())
}
