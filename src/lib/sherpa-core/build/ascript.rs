use crate::{
  ascript::{
    output_base::AscriptWriter,
    rust::{self, add_ascript_functions_for_rust, create_rust_writer_utils},
  },
  pipeline::SourceType,
  types::*,
  writer::code_writer::CodeWriter,
};

use super::pipeline::PipelineTask;

/// Build artifacts for a Bytecode based parser
pub fn build_ascript_types_and_functions(
  source_type: SourceType,
) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      match source_type {
      SourceType::Rust => {
        if let Some(store) = task_ctx.get_ascript() {
          let u = create_rust_writer_utils(&store);
          let mut w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
          match add_ascript_functions_for_rust(&mut w, &task_ctx.get_journal().grammar().unwrap()) {
            Err(err) => Err(vec![SherpaError::from(err)]),
            _ => match rust::write_rust_ast(w) {
              SherpaResult::Err(err) => Err(vec![SherpaError::from(err)]),
              SherpaResult::None => Err(vec![SherpaError::from("Could not build rust")]),
              SherpaResult::Ok(writer) => Ok(Some((2, unsafe {
                String::from_utf8_unchecked(writer.into_writer().into_output())
              }))),
            },
          }
        } else {
          Err(vec![SherpaError::from("Could not acquire an Ascript source")])
        }
      }
      _ => Err(vec![SherpaError::from(format!(
        "Not Implemented: Unable to build an AST output for the source type {:?}",
        source_type
      ))]),
    }
    }),
    require_ascript: true,
    ..Default::default()
  }
}
