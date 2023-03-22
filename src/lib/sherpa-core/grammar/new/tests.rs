use core::panic;
use std::{path::PathBuf, sync::Arc};

use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
  grammar::{
    compile::parser::sherpa::Ascript,
    new::{
      compile::compile_parse_states,
      load::{build_db::build_compile_db, compile::compile_grammars_from_path},
    },
  },
  tasks::{new_taskman, Executor, Spawner},
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

      dbg!(&local_soup);

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

      compile_parse_states(local_j.transfer(), &db).await;

      SherpaResult::Ok(())
    },
    spawner,
  )?;

  j.flush_reports();

  SherpaResult::Ok(())
}
