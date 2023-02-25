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
  bc: &Vec<u8>,
  ascript: Option<&ascript::types::AScriptStore>,
) -> std::io::Result<()> {
  writer.wrt(
    "
pub trait Reader: ByteReader + MutByteReader + UTF8Reader + std::fmt::Debug {}

impl<T> Reader for T
  where T: ByteReader + MutByteReader + UTF8Reader + std::fmt::Debug 
  {}

pub type Parser<'a, T: Reader, UserCTX> = ByteCodeParser<'a, T, UserCTX>;",
  )?;

  for ExportedProduction { export_name, guid_name, production } in g.get_exported_productions() {
    if let Some(bytecode_offset) = state_lookups.get(guid_name) {
      writer
        .wrt(&format!(
          "pub fn new_{}_parser<'a, T: Reader, UserCTX> (reader: &'a mut T) -> Parser<'a, T, UserCTX>{{",
          export_name
        ))?
        .indent()
        .wrtln(&format!("let mut parser = Parser::new(reader, &bytecode);"))?
        .wrtln(&format!("parser.init_parser({});", bytecode_offset))?
        .wrtln(&format!("parser"))?
        .dedent()
        .wrtln("}")?
        .newline()?;
    } else {
      println!("Unable to get bytecode offset for production {} ", production.name,);
    }
  }

  writer.wrtln(&format!("pub static bytecode: [u8; {}] = [", bc.len()))?.indent();

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

    // Create a module that will store convenience functions for compiling AST
    // structures based on on grammar entry points.

    for (ref_, ast_type, ast_type_string, export_name, guid_name) in &export_node_data {
      writer
        .newline()?
        .wrtln(&format!(
          "pub fn {0}_from<'a>(mut reader: UTF8StringReader)  -> Result<{1}, SherpaParseError> {{ ",
          export_name, ast_type_string
        ))?
        .indent()
        .wrtln(&format!(
          "
let reduce_functions = ReduceFunctions::<_, u32>::new();

let mut parser = Parser::new(&mut reader, &bytecode);

parser.init_parser({});

let AstSlot (i0, tok0, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;

dbg!(&i0);

{}

",
          state_lookups.get(guid_name).unwrap(),
          &{
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
          }
        ))?
        .dedent()
        .wrtln("}")?;
    }

    writer.dedent().wrtln("}")?;
  }

  Ok(())
}
