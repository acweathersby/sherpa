use crate::{
  ascript::{
    rust::{ascript_type_to_string, create_type_initializer_value, render_expression},
    types::AScriptStore,
  },
  grammar::data::ast::{ASTNode, AST_NamedReference},
  types::*,
  writer::code_writer::CodeWriter,
  *,
};
use std::{collections::BTreeMap, io::Write};

pub(crate) fn write_rust_entry_functions_bytecode<W: Write>(
  g: &GrammarStore,
  states: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(
    for ExportedProduction { export_name, guid_name, production } in g.get_exported_productions() {
      if let Some(bytecode_offset) = states.get(guid_name) {
        writer
          .wrt(&format!("pub fn new_{}_parser(reader: T) -> Self{{", export_name))?
          .indent()
          .wrtln("let mut ctx = Self::new(reader);")?
          .wrtln(&format!("ctx.0.init_normal_state(NORMAL_STATE_FLAG | {});", bytecode_offset))?
          .wrtln("ctx")?
          .dedent()
          .wrtln("}")?
          .newline()?;
      } else {
        println!("Unable to get bytecode offset for production {} ", production.name,);
      }
    },
  )
}

pub(crate) fn write_rust_entry_functions<W: Write>(
  g: &GrammarStore,
  _states: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(
    for (i, ExportedProduction { export_name, production, .. }) in
      g.get_exported_productions().iter().enumerate()
    {
      writer
        .newline()?
        .wrtln(&format!("/// `{}`", production.loc.to_string().replace("\n", "\n// ")))?
        .wrtln(&format!("pub fn new_{}_parser(reader: T) -> Self{{", export_name))?
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
