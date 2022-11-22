use hctk_core::debug::generate_disassembly;
use hctk_core::debug::BytecodeGrammarLookups;
use hctk_core::types::HCError;
use std::io::BufWriter;

use crate::builder::disclaimer::DISCLAIMER;
use hctk_core::writer::code_writer::CodeWriter;

use super::pipeline::PipelineTask;

/// Generate a disassembly file of the grammar bytecode
pub fn build_bytecode_disassembly() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let output_path = task_ctx.get_source_output_dir().clone();
      let parser_name = task_ctx.get_parser_name().clone();

      if let Ok(parser_data_file) =
        task_ctx.create_file(output_path.join(format!("./{}_dasm.txt", parser_name)))
      {
        let grammar = task_ctx.get_grammar();

        let Some(bytecode) = task_ctx.get_bytecode() else {
          return Err(vec![HCError::from("Cannot disassemble Bytecode: Bytecode is not available")]);
        };

        let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

        writer.write(&DISCLAIMER("Parser Data", "//!", task_ctx)).unwrap();

        writer
          .write(&generate_disassembly(
            bytecode_output,
            Some(&BytecodeGrammarLookups::new(grammar)),
          ))
          .unwrap();
      }
      Ok(None)
    }),
    require_ascript: false,
    require_bytecode: true,
  }
}
