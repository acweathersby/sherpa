#![allow(unused)]
use std::fs::OpenOptions;

use super::common::{build_ast_source, build_parser_source};
use crate::BuildConfig;
use radlr_core::*;

const PARSER_SCRIPT: &'static str = include_str!("javascript_bytecode_script.atat");
const AST_SCRIPT: &'static str = include_str!("javascript_ast_script.atat");

pub fn build(db: &RadlrDatabase, build_config: BuildConfig, parser_config: ParserConfig) -> RadlrResult<()> {
  let (out_dir, lib_dir) = (build_config.source_out, build_config.lib_out);

  let pool = radlr_core::worker_pool::SingleThreadPool {};

  let states = db.build_states(Default::default(), &pool)?;
  let parser = states.build_ir_parser(true, false, &pool)?;

  let bytecode = radlr_bytecode::compile_bytecode(&parser, false)?;

  let parser_path = out_dir.join("parser.js");
  let ast_path = out_dir.join("ast.js");

  build_parser_source(db, PARSER_SCRIPT, bytecode, parser_path.clone(), parser_path)?;

  if build_config.build_ast {
    build_ast_source(db, AST_SCRIPT, ast_path, build_config)?;
  }

  Ok(())
}
