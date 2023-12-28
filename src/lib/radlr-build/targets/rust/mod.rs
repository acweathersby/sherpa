use super::common::{build_ast_source, build_parser_source, build_parser_states};
use crate::BuildConfig;
use radlr_core::*;
use std::{fs::*, io::Write};

const SCRIPT: &'static str = include_str!("rust_ast_script.atat");
const BC_SCRIPT: &'static str = include_str!("rust_bytecode_script.atat");

pub fn build(db: &RadlrDatabase, build_config: BuildConfig, parser_config: ParserConfig) -> RadlrResult<()> {
  let (out_dir, lib_dir) = (build_config.source_out, build_config.lib_out);

  let parser = build_parser_states(db, parser_config)?;

  let bytecode = radlr_bytecode::compile_bytecode(&parser, false)?;

  let binary_path = lib_dir.join("parser.bin");
  let parser_path = out_dir.join("parser.rs");
  let ast_path = out_dir.join("ast.rs");

  {
    let mut parser_binary = OpenOptions::new().append(false).truncate(true).write(true).create(true).open(&binary_path)?;
    bytecode.write_binary(&mut parser_binary)?;
    parser_binary.flush()?;
  }

  build_parser_source(db, BC_SCRIPT, bytecode, binary_path, parser_path)?;

  if build_config.build_ast {
    build_ast_source(db, SCRIPT, ast_path, build_config)?;
  }

  Ok(())
}
