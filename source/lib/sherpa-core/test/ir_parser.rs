use std::path::PathBuf;

use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states},
  journal::*,
  types::{GrammarStore, SherpaResult},
};

#[test]
fn ir_parser_build() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../grammar/v1_0_0_strap/ir.hcg")
      .canonicalize()?,
  )?;

  j.flush_reports();

  let states = compile_states(&mut j, 1)?;

  let states = optimize_ir_states(&mut j, states);

  compile_bytecode(&mut j, states);

  assert!(!j.debug_error_report());

  SherpaResult::Ok(())
}
