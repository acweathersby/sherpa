#[cfg(test)]
mod test_parser_build {
  use std::path::PathBuf;

  use crate::{
    compile::{compile_bytecode, compile_states, optimize_ir_states},
    GrammarStore,
    Journal,
    SherpaResult,
  };

  #[test]
  fn build() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_path(
      &mut j,
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../test/e2e/bootstrap/grammar/ir_base.hcg")
        .canonicalize()
        .unwrap(),
    )
    .unwrap();

    let states = compile_states(&mut j, 10)?;
    let pre_opt_length = states.len();

    let mut states = optimize_ir_states(&mut j, states);
    let post_opt_length = states.len();

    compile_bytecode(&mut j, states);

    j.flush_reports();
    j.debug_report(crate::ReportType::Any);

    SherpaResult::Ok(())
  }
}
