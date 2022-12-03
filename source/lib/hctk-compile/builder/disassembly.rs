use super::pipeline::PipelineTask;
use crate::builder::disclaimer::DISCLAIMER;
use hctk_core::{debug::generate_disassembly, writer::code_writer::CodeWriter, *};
use std::io::BufWriter;

/// Generate a disassembly file of the grammar bytecode
pub fn build_bytecode_disassembly() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let output_path = task_ctx.get_source_output_dir().clone();

      let mut j = task_ctx.get_journal();

      if let Ok(parser_data_file) = task_ctx
        .create_file(output_path.join(format!("./{}.sherpa.dasm", j.grammar().unwrap().id.name)))
      {
        let Some(bytecode) = task_ctx.get_bytecode() else {
          return Err(vec![HCError::from("Cannot disassemble Bytecode: Bytecode is not available")]);
        };

        let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

        writer.write(&DISCLAIMER("Parser Data", "//!", task_ctx)).unwrap();

        writer.write(&generate_disassembly(bytecode, Some(&mut j))).unwrap();
      }
      Ok(None)
    }),
    require_ascript: false,
    require_bytecode: true,
  }
}
