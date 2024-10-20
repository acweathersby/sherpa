#![allow(unused)]
use super::common::build_ast_source;
use crate::BuildConfig;
use radlr_core::*;

const SCRIPT: &'static str = include_str!("typescript_ast_script.atat");

pub fn build(db: &RadlrGrammarDatabase, build_config: BuildConfig, parser_config: ParserConfig) -> RadlrResult<()> {
  let (out_dir, lib_dir) = (build_config.source_out, build_config.lib_out);

  let pool = radlr_core::worker_pool::SingleThreadPool {};

  let states = db.build_states(Default::default(), &pool)?;
  let parser = states.build_ir_parser(true, false, &pool)?;

  let bytecode = radlr_bytecode::compile_bytecode(&parser, false)?;

  let parser_path = out_dir.join("parser.ts");
  let ast_path = out_dir.join("ast.ts");

  if build_config.build_ast {
    build_ast_source(db, SCRIPT, ast_path, build_config.ast_struct_name, &[])?;
  }

  Ok(())
}
