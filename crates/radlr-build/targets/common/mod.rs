use crate::BuildConfig;
use radlr_ascript_beta::*;
use radlr_core::{proxy::OrderedMap, *};
use radlr_formatter::{Formatter, FormatterContext, FormatterResult, ToValue, Value};
use std::{collections::HashMap, fs::*, io::Write};

pub fn build_ast_source(
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

pub fn build_parser_states(db: &RadlrDatabase, parser_config: ParserConfig) -> Result<RadlrIRParser, RadlrError> {
  let states = db.build_states(parser_config)?;
  let parser = states.build_ir_parser(true, false)?;
  Ok(parser)
}

pub fn build_parser_source(
  db: &RadlrDatabase,
  parser_script: &str,
  bytecode: radlr_rust_runtime::types::BytecodeParserDB,
  binary_out_path: std::path::PathBuf,
  parser_out_path: std::path::PathBuf,
) -> Result<(), RadlrError> {
  let parser = OpenOptions::new().append(false).truncate(true).write(true).create(true).open(&parser_out_path)?;

  let s_store = db.get_internal().string_store();

  let mut ctx: FormatterContext = FormatterContext::new("RustForm", s_store.clone());

  let states_lu = bytecode
    .state_to_token_ids_map
    .iter()
    .map(|(id, val)| (id.to_string(), val.clone().into_iter().map(|v| -> Value { v.into() }).collect::<Vec<_>>()))
    .collect::<HashMap<_, _>>();
 
  let nonterm_name_to_id =
    bytecode.nonterm_name_to_id.iter().map(|(id, val)| (id.clone(), Value::from(*val))).collect::<OrderedMap<_, _>>();
 
  let token_id_to_str =
    bytecode.token_id_to_str.iter().map(|(id, val)| (id.to_string(), val.into_val(s_store))).collect::<OrderedMap<_, _>>();
 
 
  let nonterm_id_to_address = bytecode
    .nonterm_id_to_address
    .iter()
    .map(|(id, val)| (id.to_string(), (*val).into_val(s_store)))
    .collect::<OrderedMap<_, _>>();
  let mut token_maps: std::collections::BTreeMap<Vec<u32>, usize> = OrderedMap::new();
 
  let state_to_token_ids_map = bytecode
    .state_to_token_ids_map
    .iter()
    .map(|(state, ids)| {
      let index = token_maps.len();
      let id = *token_maps.entry(ids.clone()).or_insert(index);
      ((*state).to_string(), id)
    })
    .collect::<OrderedMap<_, _>>();

  let token_maps = token_maps.iter().map(|(v, k)| (k.to_string(), v.clone())).collect::<OrderedMap<_, _>>();
  
  ctx.set_val("ir_token_lookup", Value::Obj(&states_lu));
  ctx.set_val("binary_path", binary_out_path.intern(db.get_internal().string_store()).into());
  ctx.set_val("nonterm_name_to_id", Value::Obj(&nonterm_name_to_id));
  ctx.set_val("token_id_to_str", Value::Obj(&token_id_to_str));
  ctx.set_val("nonterm_id_to_address", Value::Obj(&nonterm_id_to_address));
  ctx.set_val("state_to_token_ids_map", Value::Obj(&state_to_token_ids_map));
  ctx.set_val("token_maps", Value::Obj(&token_maps));
  ctx.max_width = 100;
 
  let f: Formatter = FormatterResult::from(parser_script).into_result()?;
 
  f.write_to_output(&mut ctx, parser)?.flush()?;
 
  Ok(())
}
