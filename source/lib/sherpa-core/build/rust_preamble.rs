use super::{disclaimer, pipeline::PipelineTask};

/// Build artifacts for a Bytecode based parser
pub fn build_rust_preamble() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      Ok(Some((
        0,
        format!(
          "{}

use sherpa_runtime::types::{{ast::*, *}};
",
          disclaimer::DISCLAIMER("Rust Parser", "///", task_ctx)
        ),
      )))
    }),
    require_ascript: true,
    require_bytecode: false,
  }
}
