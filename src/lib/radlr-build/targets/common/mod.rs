use crate::BuildConfig;
use radlr_ascript_beta::*;
use radlr_core::{proxy::OrderedMap, *};
use radlr_formatter::{Formatter, FormatterContext, FormatterResult, ToValue, Value};
use std::{collections::HashMap, fs::*, io::Write, path::Path};

pub fn build_ascript_ast(
  db: &RadlrDatabase,
  script: &str,
  ast_path: std::path::PathBuf,
  build_config: BuildConfig<'_>,
) -> RadlrResult<()> {
  let adb: AscriptDatabase = db.into();
  Ok(if let Some(errors) = adb.get_errors() {
    for error in errors {
      eprintln!("{}", error);
    }
  } else {
    let parser = OpenOptions::new().append(false).truncate(true).write(true).create(true).open(&ast_path)?;

    adb.format(script, parser, 100, build_config.ast_struct_name)?.flush()?;
  })
}
