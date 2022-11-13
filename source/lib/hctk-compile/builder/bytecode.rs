use hctk_core::bytecode::compile_bytecode;

use hctk_core::bytecode::BytecodeOutput;
use hctk_core::types::*;

use std::collections::BTreeMap;

use std::io::Write;

use crate::ascript::types::AScriptStore;
use crate::CompileError;
use crate::SourceType;

use hctk_core::writer::code_writer::CodeWriter;

use super::common::add_ascript_functions;
use super::common::write_rust_entry_functions_bytecode;
use super::pipeline::PipelineTask;

/// Build artifacts for a LLVM based parser.
pub fn build_byte_code_parse(
  source_type: SourceType,
  include_ascript_mixins: bool,
) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| match source_type {
      SourceType::Rust => {
        let mut writer = CodeWriter::new(vec![]);

        if let Err(err) = write_parser_file(
          &mut writer,
          &task_ctx.get_grammar(),
          // Leave two threads available for building
          // the
          // ascript code if necessary
          1,
          if include_ascript_mixins { Some(task_ctx.get_ascript()) } else { None },
        ) {
          Err(CompileError::from_io_error(&err))
        } else {
          Ok(Some(unsafe { String::from_utf8_unchecked(writer.into_output()) }))
        }
      }
      _ => Err(CompileError::from_string(&format!(
        "Unable to build an AST output for the source type {:?}",
        source_type
      ))),
    }),
    require_ascript: include_ascript_mixins,
    require_bytecode: true,
  }
}

fn write_parser_file<W: Write>(
  writer: &mut CodeWriter<W>,
  g: &GrammarStore,
  thread_count: usize,
  ascript: Option<&AScriptStore>,
) -> std::io::Result<()> {
  let BytecodeOutput { bytecode, state_name_to_offset: state_lookups, .. } =
    compile_bytecode(g, thread_count);

  if let Err(err) = write_rust_parser_file(writer, &state_lookups, g, &bytecode, ascript) {
    eprintln!("{}", err);
  }

  Ok(())
}

fn write_rust_parser_file<W: Write>(
  writer: &mut CodeWriter<W>,
  state_lookups: &BTreeMap<String, u32>,
  g: &GrammarStore,
  bc: &Vec<u32>,
  ast: Option<&AScriptStore>,
) -> std::io::Result<()> {
  writer
    .wrt(
      "
use hctk::runtime::*;
use hctk::types::*;

pub struct Parser<T: ByteCharacterReader + BaseCharacterReader + MutCharacterReader>(ParseContext<T>, T, bool);

impl<T: ByteCharacterReader + BaseCharacterReader + MutCharacterReader> Iterator for Parser<T>
{
    type Item = ParseAction;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item>
    {
        let Parser(ctx, reader, active) = self;

        if *active {
            let action = get_next_action::<T>(reader, ctx, &bytecode);
            match action {
                ParseAction::Error { .. } | ParseAction::Accept { .. } => {
                    *active = false;
                    Some(action)
                }
                action => Some(action),
            }
        } else {
            None
        }
    }
}

impl<T: ByteCharacterReader + BaseCharacterReader + MutCharacterReader> Parser<T>
{
    #[inline(always)]
    fn new(reader: T) -> Self
    {
        Self(ParseContext::<T>::bytecode_context(), reader, true)
    }
    ",
    )?
    .indent();

  write_rust_entry_functions_bytecode(g, state_lookups, writer)?;
  writer.dedent().wrtln("}")?;

  add_ascript_functions(ast, g, writer)?;

  writer.wrtln(&format!("static bytecode: [u32; {}] = [", bc.len()))?.indent();

  for chunk in bc.chunks(9) {
    writer.insert_newline()?;
    for val in chunk {
      writer.wrt(&val.to_string())?.wrt(", ")?;
    }
  }

  writer.dedent().write_line("];")?;

  Ok(())
}

#[cfg(test)]
mod test {
  use crate::ascript::compile::compile_ascript_store;
  use crate::ascript::rust;
  use crate::ascript::types::AScriptStore;
  use hctk_core::writer::code_writer::StringBuffer;
  #[test]
  fn test_output_rust_on_practical_grammar() {
    use hctk_core::debug::compile_test_grammar;

    let grammar = compile_test_grammar(
        "
        <> A > \\vec num num^tom num f:ast { { t_Vec, x:f32($tom), y:f32($3), z:f32($4), first: { t_Num, val:u32($1) } } }
        
        <> num > \\temp g:num 
        ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_ascript_store(&grammar, &mut ascript);

    for error in &errors {
      eprintln!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.structs.len(), 2);

    let mut writer = StringBuffer::default();

    rust::write(&grammar, &ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
