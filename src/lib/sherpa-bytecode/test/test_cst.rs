use crate::*;
use sherpa_core::*;
use sherpa_rust_runtime::types::{CSTEditor, StringInput};
use std::path::PathBuf;

#[test]
pub fn construct_trivial_patcher() -> SherpaResult<()> {
  let source = r#"

  IGNORE { c:sp }

    <> A > tk:test(+)

    <> test > "apple" | "tree"
  
   "#;

  let input = r#"apple tree"#;

  let root_path = PathBuf::from("test.sg");
  let mut grammar = SherpaGrammarBuilder::new();
  grammar.add_source_from_string(source, &root_path, false)?;
  let parser_data =
    grammar.build_db(&root_path)?.build_parser(ParserConfig::default().cst_editor().enable_calls(false))?.optimize(false)?;

  parser_data._write_states_to_temp_file_()?;

  let pkg = compile_bytecode(&parser_data, false)?;

  pkg._write_disassembly_to_temp_file_(parser_data.get_db())?;

  let mut cst_editor = CSTEditor::new(Box::new(pkg));

  let cst = cst_editor.create_cst("default", &mut StringInput::from(input), 0)?;

  dbg!(&cst);

  //let cst = cst_editor.patch_cst_array(&cst, &mut StringInput::from("hello let
  // world"), 6, 8, 15)?;
  //
  //dbg!(cst);

  Ok(())
}
