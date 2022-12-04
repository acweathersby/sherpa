pub mod compile;
pub mod errors;
pub mod rust;
#[cfg(test)]
mod test;
pub mod types;

use crate::{
  builder::pipeline::PipelineTask,
  types::SherpaError,
  writer::code_writer::CodeWriter,
  SourceType,
};

/// Constructs a task that compiles a grammar's Ascript into an AST module of the given `source_type`.
/// The module is placed at `<source_output_dir>/<grammar_name>_parser_ast.rs`.
pub fn build_ast(source_type: SourceType) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |ctx| match (source_type, ctx.get_ascript()) {
      (SourceType::Rust, Some(ascript)) => {
        let mut writer = CodeWriter::new(vec![]);
        match rust::write(ascript, &mut writer) {
          Ok(_) => Ok(Some(unsafe { String::from_utf8_unchecked(writer.into_output()) })),
          Err(err) => Err(vec![SherpaError::from(err)]),
        }
      }
      _ => Ok(Some(String::default())),
    }),
    require_ascript: true,
    require_bytecode: false,
  }
}
