use crate::{
  ascript::{self, rust::create_type_initializer_value},
  bytecode::BytecodeOutput,
  types::*,
  writer::code_writer::CodeWriter,
};
use std::{collections::BTreeMap, io::Write};

use super::{ascript::get_ascript_export_data, pipeline::PipelineTask};

/// Build artifacts for a Rust Bytecode parser
pub fn build_bytecode_parser() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let Some(bytecode) = task_ctx.get_bytecode() else {
        return Err(vec![SherpaError::from("Cannot build Bytecode parser: Bytecode is not available")]);
      };
      let mut writer = CodeWriter::new(vec![]);

      if let Err(err) = write_parser_file(
        &mut writer,
        &task_ctx.get_journal().grammar().unwrap(),
        bytecode,
        task_ctx.get_ascript(),
      ) {
        Err(vec![SherpaError::from(err)])
      } else {
        Ok(Some((20, unsafe { String::from_utf8_unchecked(writer.into_output()) })))
      }
    }),
    require_bytecode: true,
    ..Default::default()
  }
}

fn write_parser_file<W: Write>(
  writer: &mut CodeWriter<W>,
  g: &GrammarStore,
  bytecode_output: &BytecodeOutput,
  ascript: Option<&ascript::types::AScriptStore>,
) -> std::io::Result<()> {
  let BytecodeOutput { bytecode, state_name_to_offset: state_lookups, .. } = bytecode_output;

  if let Err(err) = write_rust_parser_file(writer, &state_lookups, g, &bytecode, ascript) {
    println!("{}", err);
  }

  Ok(())
}

fn write_rust_parser_file<W: Write>(
  writer: &mut CodeWriter<W>,
  state_lookups: &BTreeMap<String, u32>,
  g: &GrammarStore,
  bc: &Vec<u32>,
  ascript: Option<&ascript::types::AScriptStore>,
) -> std::io::Result<()> {
  writer
    .wrt(
      "

pub trait Reader: ByteReader + MutByteReader + std::fmt::Debug {}

impl<T> Reader for T
  where T: ByteReader + MutByteReader + std::fmt::Debug 
  {}

pub struct Parser<T: Reader, UserCTX>(ParseContext<T, UserCTX>, bool, Vec<u32>);

impl<T: Reader, UserCTX> Iterator for Parser<T, UserCTX>
{
    type Item = ParseAction;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item>
    {
        let Parser(ctx, active, stack) = self;

        if *active {
            let action = sherpa_runtime::functions::get_next_action(ctx, stack, &bytecode, None);
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

impl<T: Reader, UserCTX> Parser<T, UserCTX>
{
    #[inline(always)]
    fn new(reader: &mut T, entry_point:u32) -> Self
    {
        Self(ParseContext::<T, UserCTX>::new(reader), true, vec![0, entry_point | NORMAL_STATE_FLAG])
    }
    ",
    )?
    .indent();

  for ExportedProduction { export_name, guid_name, production } in g.get_exported_productions() {
    if let Some(bytecode_offset) = state_lookups.get(guid_name) {
      writer
        .wrt(&format!("pub fn new_{}_parser(reader: &mut T) -> Self{{", export_name))?
        .indent()
        .wrtln(&format!("Self::new(reader, {})", bytecode_offset))?
        .dedent()
        .wrtln("}")?
        .newline()?;
    } else {
      println!("Unable to get bytecode offset for production {} ", production.name,);
    }
  }
  writer.dedent().wrtln("}")?;

  writer.wrtln(&format!("pub static bytecode: [u32; {}] = [", bc.len()))?.indent();

  for chunk in bc.chunks(9) {
    writer.insert_newline()?;
    for val in chunk {
      writer.wrt(&val.to_string())?.wrt(", ")?;
    }
  }

  writer.dedent().write_line("];")?;

  if let Some(ascript) = ascript {
    let export_node_data = get_ascript_export_data(g, ascript);

    writer.wrtln("pub mod ast  {")?.indent();
    writer.wrtln("use super::*; ")?;
    writer.wrtln(&format!("type Node = {};", ascript.ast_type_name))?;
    writer.wrtln("impl AstObject for Node {}\n")?;

    // Create a module that will store convience functions for compiling AST
    // structures based on on grammar entry points.

    for (ref_, ast_type, ast_type_string, export_name, guid_name) in &export_node_data {
      writer
        .newline()?
        .wrtln(&format!(
          "pub fn {0}_from<'a>(mut reader: UTF8StringReader<'a>)  -> Result<{1}, SherpaParseError> {{ ",
          export_name, ast_type_string
        ))?
        .indent()
        .wrtln(&format!(
          "
let reduce_functions = ReduceFunctions::new();
let mut ctx = ParseContext::<UTF8StringReader<'a>, u32>::new(&mut reader);
let mut stack = vec![0, NORMAL_STATE_FLAG | {} ];
let AstSlot (i0, tok0, _) = sherpa_runtime::functions::parse_ast(
  &mut ctx, &mut stack, &bytecode, &reduce_functions.0, None
)?;
{}

", state_lookups.get(guid_name).unwrap(), &{
  let (string, ref_) =
    create_type_initializer_value(ref_.clone(), &ast_type, false, ascript);
  if let Some(exp) = ref_ {
    format!("{}\nOk({})", exp.to_init_string(), string)
      .split("\n")
      .map(|i| i.trim())
      .collect::<Vec<_>>()
      .join("\n")
  } else {
    "Ok(i0)".to_string()
  }
}))?
        .dedent()
        .wrtln("}")?;
    }

    writer.dedent().wrtln("}")?;
  }

  Ok(())
}
