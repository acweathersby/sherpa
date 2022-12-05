use crate::{compile::BytecodeOutput, types::*, writer::code_writer::CodeWriter};
use std::{collections::BTreeMap, io::Write};

use super::{
  common::write_rust_entry_functions_bytecode,
  pipeline::{PipelineTask, SourceType},
};

/// Build artifacts for a Bytecode based parser
pub fn build_bytecode_parser(source_type: SourceType) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let Some(bytecode) = task_ctx.get_bytecode() else {
        return Err(vec![SherpaError::from("Cannot build Bytecode parser: Bytecode is not available")]);
      };
      match source_type {
        SourceType::Rust => {
          let mut writer = CodeWriter::new(vec![]);

          if let Err(err) =
            write_parser_file(&mut writer, &task_ctx.get_journal().grammar().unwrap(), bytecode)
          {
            Err(vec![SherpaError::from(err)])
          } else {
            Ok(Some(unsafe { String::from_utf8_unchecked(writer.into_output()) }))
          }
        }
        _ => Err(vec![SherpaError::from(format!(
          "Unable to build an AST output for the source type {:?}",
          source_type
        ))]),
      }
    }),
    require_ascript: false,
    require_bytecode: true,
  }
}

fn write_parser_file<W: Write>(
  writer: &mut CodeWriter<W>,
  g: &GrammarStore,
  bytecode_output: &BytecodeOutput,
) -> std::io::Result<()> {
  let BytecodeOutput { bytecode, state_name_to_offset: state_lookups, .. } = bytecode_output;

  if let Err(err) = write_rust_parser_file(writer, &state_lookups, g, &bytecode) {
    eprintln!("{}", err);
  }

  Ok(())
}

fn write_rust_parser_file<W: Write>(
  writer: &mut CodeWriter<W>,
  state_lookups: &BTreeMap<String, u32>,
  g: &GrammarStore,
  bc: &Vec<u32>,
) -> std::io::Result<()> {
  writer
    .wrt(
      "
use sherpa::rt::*;

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

  writer.wrtln(&format!("pub static bytecode: [u32; {}] = [", bc.len()))?.indent();

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
  use crate::{
    ascript::{rust, types::AScriptStore},
    journal::*,
    types::*,
    writer::code_writer::StringBuffer,
  };
  #[test]
  fn test_output_rust_on_practical_grammar() {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(&mut j,
        "
        <> A > \\vec num num^tom num f:ast { { t_Vec, x:f32($tom), y:f32($3), z:f32($4), first: { t_Num, val:u32($1) } } }
        
        <> num > \\temp g:num 
        ",
    ).unwrap();

    let mut ascript = AScriptStore::new(g).unwrap();

    assert_eq!(ascript.structs.len(), 2);

    let mut writer = StringBuffer::default();

    rust::write(&ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
