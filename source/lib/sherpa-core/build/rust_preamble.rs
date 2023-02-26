use super::{disclaimer, pipeline::PipelineTask};

/// Build artifacts for a Bytecode based parser
pub fn build_rust_preamble() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      Ok(Some((
        0,
        format!(
          "{}
use std::{{
  collections::hash_map::DefaultHasher,
  hash::{{Hash, Hasher}},
}};
use sherpa_runtime::types::{{ast::*, *}};
use sherpa_runtime::llvm_parser::*;
",
          disclaimer::DISCLAIMER("Rust Parser", "///", task_ctx)
        ),
      )))
    }),
    ..Default::default()
  }
}
