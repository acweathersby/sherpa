use crate::{
  ascript::{
    self,
    output_base::AscriptWriter,
    rust::{create_rust_writer_utils, write_rust_bytecode_parser_file},
    types::AScriptStore,
  },
  bytecode::BytecodeOutput,
  types::*,
  writer::code_writer::CodeWriter,
  Journal,
};
use std::io::Write;

use super::pipeline::PipelineTask;

/// Build artifacts for a Rust Bytecode parser
pub fn build_bytecode_parser() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let mut j = task_ctx.get_journal();

      let Some(bytecode) = task_ctx.get_bytecode() else {
        return Err(vec![SherpaError::from("Cannot build Bytecode parser: Bytecode is not available")]);
      };
      let writer = CodeWriter::new(vec![]);
      match write_parser_file(&mut j, writer, bytecode, task_ctx.get_ascript())
      {
        SherpaResult::Err(err) => Err(vec![SherpaError::from(err)]),
        SherpaResult::Ok(writer) => Ok(Some((20, unsafe {
          String::from_utf8_unchecked(writer.into_output())
        }))),
        _ => unreachable!(),
      }
    }),
    require_bytecode: true,
    ..Default::default()
  }
}

fn write_parser_file<W: Write>(
  j: &mut Journal,
  writer: CodeWriter<W>,
  bytecode_output: &BytecodeOutput,
  ascript: Option<&ascript::types::AScriptStore>,
) -> SherpaResult<CodeWriter<W>> {
  let BytecodeOutput { bytecode, state_name_to_offset: state_lookups, .. } =
    bytecode_output;
  let dummy = AScriptStore::dummy(j).unwrap();
  let store = if let Some(store) = ascript { store } else { &dummy };
  let utils = create_rust_writer_utils(store);
  let w = AscriptWriter::new(&utils, writer);

  match write_rust_bytecode_parser_file(w, &state_lookups, &bytecode) {
    SherpaResult::Err(err) => SherpaResult::Err(err),
    SherpaResult::Ok(w) => SherpaResult::Ok(w.into_writer()),
    _ => unreachable!(),
  }
}
