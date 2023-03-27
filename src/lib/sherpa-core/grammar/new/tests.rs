use core::panic;
use std::{path::PathBuf, sync::Arc};

use sherpa_runtime::{
  bytecode_parser::ByteCodeParser,
  types::{ByteReader, MutByteReader, SherpaParser, UTF8StringReader},
};

use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
  debug,
  grammar::{
    compile::parser::sherpa::Ascript,
    new::{
      bytecode::{compile_bytecode, generate_disassembly},
      compile::{compile_parse_states, garbage_collect, optimize},
      debug::console_debugger,
      load::{build_db::build_compile_db, compile::compile_grammars_from_path},
    },
  },
  tasks::{new_taskman, Executor, Spawner},
  test::utils::PrintConfig,
  types::{graph::Origin, Items},
  Journal,
  ReportType,
  SherpaResult,
};

use super::types::*;

#[test]
fn build_parser() -> SherpaResult<()> {
  let grammar_soup = GrammarSoup::new();
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../grammar/json/test.sg")
    .canonicalize()
    .unwrap();

  let mut j = Journal::new(None);
  j.set_active_report("test", ReportType::Any);

  let (executor, spawner) = new_taskman(1000);

  let local_spawner = spawner.clone();
  let local_soup = grammar_soup.clone();
  let mut local_j = j.transfer();

  executor.execute(
    async move {
      let id = compile_grammars_from_path(
        local_j.transfer(),
        grammar_source_path,
        &local_soup,
        &local_spawner,
      )
      .await?;

      assert_eq!(
        local_soup
          .grammar_headers
          .read()
          .unwrap()
          .get(&id.guid)
          .unwrap()
          .pub_prods
          .len(),
        1
      );

      local_j.flush_reports();

      let parser_data =
        build_compile_db(local_j.transfer(), id, &local_soup, &local_spawner)
          .await?;

      SherpaResult::Ok(())
    },
    spawner,
  )?;

  j.flush_reports();

  SherpaResult::Ok(())
}

#[test]
fn build_states() -> SherpaResult<()> {
  let grammar_soup = GrammarSoup::new();
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../grammar/json/test.sg")
    .canonicalize()
    .unwrap();

  let mut j = Journal::new(None);
  j.set_active_report("test", ReportType::Any);

  let (executor, spawner) = new_taskman(1000);

  let local_spawner = spawner.clone();
  let local_soup = grammar_soup.clone();
  let mut local_j = j.transfer();

  executor.execute(
    async move {
      let id = compile_grammars_from_path(
        local_j.transfer(),
        grammar_source_path,
        &local_soup,
        &local_spawner,
      )
      .await?;

      assert_eq!(
        local_soup
          .grammar_headers
          .read()
          .unwrap()
          .get(&id.guid)
          .unwrap()
          .pub_prods
          .len(),
        1
      );

      let db =
        build_compile_db(local_j.transfer(), id, &local_soup, &local_spawner)
          .await?;

      let parse_states = compile_parse_states(local_j.transfer(), &db).await?;

      let parse_states = garbage_collect::<Array<_>>(&db, parse_states)?;

      let (bc, _) = compile_bytecode(&db, parse_states)?;

      let input = r##""12\"34""##;
      let mut parser = ByteCodeParser::<UTF8StringReader, u32>::new(
        &mut (input.into()),
        bc.as_ref(),
      );

      dbg!(parser.collect_shifts_and_skips(
        8,
        0,
        &mut console_debugger(db.clone(), PrintConfig {
          display_input_data: true,
          display_instruction: true,
          display_scanner_output: true,
          display_state: true,
          ..Default::default()
        })
      ));

      SherpaResult::Ok(())
    },
    spawner,
  )?;

  j.flush_reports();

  SherpaResult::Ok(())
}
