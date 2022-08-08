use std::collections::BTreeMap;
use std::io::Write;

use hctk::grammar::get_exported_productions;
use hctk::grammar::ExportedProduction;
use hctk::types::GrammarStore;

use crate::writer::code_writer::CodeWriter;

/// Returns a tuple comprised of a grammar name and a parser name
pub(crate) fn get_parser_names(grammar: &GrammarStore) -> (String, String)
{
  let grammar_name = grammar.friendly_name.to_owned();
  let parser_name = grammar_name.to_owned() + "_parser";
  (grammar_name, parser_name)
}

pub(crate) fn write_rust_entry_function_bytecode<W: Write>(
  grammar: &GrammarStore,
  state_lookups: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error>
{
  Ok(
    for ExportedProduction { export_name, guid_name, production } in
      get_exported_productions(grammar)
    {
      if let Some(bytecode_offset) = state_lookups.get(guid_name) {
        writer
          .wrt(&format!(
            "pub fn new_{}_parser(reader: &'a mut T) -> Self{{",
            export_name
          ))?
          .indent()
          .wrtln("let mut ctx = Self::new(reader);")?
          .wrtln(&format!(
            "ctx.0.init_normal_state(NORMAL_STATE_FLAG | {});",
            bytecode_offset
          ))?
          .wrtln("ctx")?
          .dedent()
          .wrtln("}")?
          .newline()?;
      } else {
        println!(
          "Unable to get bytecode offset for production {} ",
          production.original_name,
        );
      }
    },
  )
}

pub(crate) fn write_rust_entry_function<W: Write>(
  grammar: &GrammarStore,
  state_lookups: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error>
{
  Ok(
    for (i, ExportedProduction { export_name, production, .. }) in
      get_exported_productions(grammar).iter().enumerate()
    {
      writer
        .newline()?
        .wrtln(&format!(
          "/// `{}`",
          production.original_location.to_string().replace("\n", "\n// ")
        ))?
        .wrtln(&format!("pub fn new_{}_parser(reader: &mut T) -> Self{{", export_name))?
        .indent()
        .wrtln("let mut ctx = Self::new(reader);")?
        .wrtln(&format!("ctx.set_start_point({});", i))?
        .wrtln("ctx")?
        .dedent()
        .wrtln("}")?
        .newline()?;
    },
  )
}
