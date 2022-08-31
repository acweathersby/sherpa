use hctk::bytecode::compile_bytecode;

use hctk::bytecode::BytecodeOutput;

use hctk::grammar::get_exported_productions;
use hctk::grammar::ExportedProduction;
use hctk::types::*;
use std::collections::BTreeMap;
use std::io::BufWriter;

use std::io::Write;

use crate::CompileError;
use crate::SourceType;

use crate::builder::common;
use crate::builder::disclaimer::DISCLAIMER;
use crate::writer::code_writer::CodeWriter;

use super::pipeline::PipelineTask;

/// Build artifacts for a LLVM based parser.
pub fn build_byte_code_parse(
  source_type: SourceType,
  include_ascript_mixins: bool,
) -> PipelineTask
{
  PipelineTask {
    fun: Box::new(move |task_ctx| match source_type {
      SourceType::Rust => {
        let output_path = task_ctx.get_source_output_dir().clone();
        let parser_name = task_ctx.get_parser_name().clone();

        if let Ok(parser_data_file) =
          task_ctx.create_file(output_path.join(format!("./{}_bc.rs", parser_name)))
        {
          let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

          writer.write(&DISCLAIMER(&parser_name, "Parser Data", "//!"));

          if include_ascript_mixins {
            writer.wrtln(&format!("mod {}_ast;", parser_name));
            writer.wrtln(&format!("use {}_ast::*;", parser_name));
          }

          if let Err(err) = write_parser_file(
            writer,
            &task_ctx.get_grammar(),
            // Leave two threads available for building
            // the
            // ascript code if necessary
            1,
            if include_ascript_mixins {
              Some(task_ctx.get_ascript())
            } else {
              None
            },
          ) {
            Err(CompileError::from_io_error(&err))
          } else {
            Ok(())
          }
        } else {
          Err(CompileError::from_string(&format!(
            "Unable to build an AST output for the source type {:?}",
            source_type
          )))
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
  mut writer: CodeWriter<W>,
  grammar: &GrammarStore,
  threads: usize,
  ascript: Option<&AScriptStore>,
) -> std::io::Result<()>
{
  let BytecodeOutput { bytecode, state_name_to_offset: state_lookups, .. } =
    compile_bytecode(grammar, threads);

  if let Err(err) =
    write_rust_parser_file(writer, &state_lookups, grammar, &bytecode, ascript)
  {
    eprintln!("{}", err);
  }

  Ok(())
}

fn write_rust_parser_file<W: Write>(
  mut writer: CodeWriter<W>,
  state_lookups: &BTreeMap<String, u32>,
  grammar: &GrammarStore,
  bytecode: &Vec<u32>,
  ascript: Option<&AScriptStore>,
) -> std::io::Result<()>
{
  writer
    .wrt(
      "
use hctk::runtime::*;
use hctk::types::*;

pub struct Context<'a, T: CharacterReader>(ParseContext<T>, &'a mut T, bool);

impl<'a, T: CharacterReader> Iterator for Context<'a, T>
{
    type Item = ParseAction;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item>
    {
        let Context(ctx, reader, active) = self;

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

impl<'a, T: CharacterReader> Context<'a, T>
{
    #[inline(always)]
    fn new(reader: &'a mut T) -> Self
    {
        Self(ParseContext::<T>::bytecode_context(), reader, true)
    }
    ",
    )?
    .indent();

  common::write_rust_entry_function_bytecode(grammar, state_lookups, &mut writer)?;

  if let Some(ascript) = ascript {
    for (_, ExportedProduction { export_name, production, .. }) in
      get_exported_productions(grammar).iter().enumerate()
    {
      writer
        .newline()?
        .wrtln(&format!(
          "pub fn parse_{}(reader: &mut T) -> Result<{}, ParseError>{{ ",
          export_name, "HCO"
        ))?
        .indent()
        .wrtln(&format!(
          "
        let ctx = Self::new_{}_parser(reader);
        ",
          export_name
        ))?
        .wrtln(
          "
        let mut stack = Vec::new();
        for action in ctx {
          match action {
            ParseAction::Error { last_input, .. } => {
              return Err(ParserError::COMPILE_PROBLEM(
                CompileProblem {
                  message: \"Unable to parse input\",
                  inline_message: \"Invalid Token\",
                  loc: last_input
                }
              ));
            }
            ParseAction::Shift { skipped_characters: skip, token } => {
              stack.push(HCO::TOKEN(Token::from_kernel_token(&token)));
            }
            ParseAction::Reduce { body_id, .. } => {
              REDUCE_FUNCTIONS[body_id as usize](&mut stack, Token::new());
            }
            ParseAction::Accept { production_id } => {
              break;
            }
            _ => {
              break;
            }
          }
        }
        Ok(stack.first.unwrap())
        ",
        )?
        .dedent()
        .wrtln("}")?;
    }
  }

  writer.dedent().wrtln("}")?;

  writer.wrtln(&format!("static bytecode: [u32; {}] = [", bytecode.len()))?.indent();

  for chunk in bytecode.chunks(9) {
    writer.insert_newline()?;
    for val in chunk {
      writer.wrt(&val.to_string())?.wrt(", ")?;
    }
  }

  writer.dedent().write_line("];")?;

  writer.into_output();

  Ok(())
}

#[cfg(test)]
mod test
{
  use crate::ast::rust;
  use crate::writer::code_writer::StringBuffer;
  use hctk::ascript::compile::compile_reduce_function_expressions;
  use hctk::types::AScriptStore;
  #[test]
  fn test_output_rust_on_practical_grammar()
  {
    use hctk::debug::compile_test_grammar;

    let grammar = compile_test_grammar(
        "
        <> A > \\vec num num^tom num f:ast { { t_Vec, x:f32($tom), y:f32($3), z:f32($4), first: { t_Num, val:u32($1) } } }
        
        <> num > \\temp g:num 
        ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut ascript);

    for error in &errors {
      eprintln!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.struct_table.len(), 2);

    let mut writer = StringBuffer::default();

    rust::write(&grammar, &ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
