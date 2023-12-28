use super::common::build_ast_source;
use crate::BuildConfig;
use radlr_core::*;

const SCRIPT: &'static str = include_str!("typescript_ast_script.atat");

pub fn build(db: &RadlrDatabase, build_config: BuildConfig, parser_config: ParserConfig) -> RadlrResult<()> {
  let (out_dir, lib_dir) = (build_config.source_out, build_config.lib_out);

  let states = db.build_states(Default::default())?;
  let parser = states.build_ir_parser(true, false)?;

  let bytecode = radlr_bytecode::compile_bytecode(&parser, false)?;

  let parser_path = out_dir.join("parser.ts");
  let ast_path = out_dir.join("ast.ts");

  /* {
    {
      let mut parser_binary = OpenOptions::new().append(false).truncate(true).write(true).create(true).open(&binary_path)?;
      bytecode.write_binary(&mut parser_binary)?;
      parser_binary.flush()?;
    }

    build_parser_source(db, BC_SCRIPT, bytecode, binary_path, parser_path)?;
  } */

  if build_config.build_ast {
    build_ast_source(db, SCRIPT, ast_path, build_config)?;
  }

  Ok(())
}
