use crate::utils::{_write_disassembly_to_temp_file_, _write_states_to_temp_file_};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::*;
use sherpa_rust_runtime::{parsers::cst::EditGraph, types::*};
use std::{path::PathBuf, rc::Rc};

#[test]
pub fn construct_error_recovering_parser() -> SherpaResult<()> {
  let source = r#"
  
  IGNORE { c:sp }

  <> taco > apple ";" $

  <> apple > topic ";"

  <> topic > test+ "green" "toast"

  <> test > fn "{}" 

  <> fn > ("fn" | "funct" | "function") "(" field(*",") ")" "{" field(*",") "}" 

  <> field > tk:id ":" val

  <> val > c:num+

  <> id > c:id(+)
  
   "#;

  let input = r#"fn(das:2){}{}green toast;;"#;

  let root_path = PathBuf::from("test.sg");
  let mut grammar = SherpaGrammar::new();

  grammar.add_source_from_string(source, &root_path, false)?;

  let config = ParserConfig::default().cst_editor();
  let parser_data = grammar.build_db(&root_path, config)?.build_states(config)?.build_ir_parser(false, false)?;

  _write_states_to_temp_file_(&parser_data)?;

  let pkg = compile_bytecode(&parser_data, false)?;

  let pkg = Rc::new(pkg);

  _write_disassembly_to_temp_file_(&pkg, parser_data.get_db())?;

  let mut graph: EditGraph<StringInput, BytecodeParserDB> =
    EditGraph::parse(pkg.default_entrypoint(), input.to_string(), pkg.clone())?;

  //dbg!(&graph);'

  println!("\n\n--");
  Printer::new(graph.cst().unwrap(), true, pkg.as_ref()).print();
  println!("\n\n");

  graph.insert(8, "2,     d")?;

  println!("\n\n--");
  Printer::new(graph.cst().unwrap(), true, pkg.as_ref()).print();
  println!("\n\n");

  graph.insert(8, ")} fn(")?;

  println!("\n\n--");
  Printer::new(graph.cst().unwrap(), true, pkg.as_ref()).print();
  println!("\n\n");

  Ok(())
}
