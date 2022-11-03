use std::collections::BTreeMap;
use std::io::Write;

use hctk_core::grammar::get_exported_productions;
use hctk_core::grammar::ExportedProduction;
use hctk_core::types::GrammarStore;

use hctk_core::writer::code_writer::CodeWriter;

pub(crate) fn write_rust_entry_function_bytecode<W: Write>(
  g: &GrammarStore,
  states: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(for ExportedProduction { export_name, guid_name, production } in get_exported_productions(g) {
    if let Some(bytecode_offset) = states.get(guid_name) {
      writer
        .wrt(&format!("pub fn new_{}_parser(reader: &'a mut T) -> Self{{", export_name))?
        .indent()
        .wrtln("let mut ctx = Self::new(reader);")?
        .wrtln(&format!("ctx.0.init_normal_state(NORMAL_STATE_FLAG | {});", bytecode_offset))?
        .wrtln("ctx")?
        .dedent()
        .wrtln("}")?
        .newline()?;
    } else {
      println!("Unable to get bytecode offset for production {} ", production.original_name,);
    }
  })
}

pub(crate) fn write_rust_entry_function<W: Write>(
  g: &GrammarStore,
  _states: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(
    for (i, ExportedProduction { export_name, production, .. }) in
      get_exported_productions(g).iter().enumerate()
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
