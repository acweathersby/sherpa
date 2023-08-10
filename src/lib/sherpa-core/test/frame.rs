use std::{path::PathBuf, process::Output};

use crate::{
  build_compile_db,
  compile_grammar_from_str,
  compile_parse_states,
  garbage_collect,
  optimize,
  Config,
  GrammarSoup,
  Journal,
  ParseStatesVec,
  ParserDatabase,
  ReportType,
  SherpaResult,
};
pub struct TestPackage<'a> {
  pub journal: Journal,
  pub states:  ParseStatesVec<'a>,
  pub db:      &'a ParserDatabase,
  pub soup:    &'a GrammarSoup,
}

pub struct DBPackage<'a> {
  pub journal: Journal,
  pub db:      &'a ParserDatabase,
  pub soup:    &'a GrammarSoup,
}

/// Simple single thread compilation of a grammar source string.
/// `test_fn` is called after a successful compilation of parse states.
pub fn build_parse_states_from_source_str<'a, T>(
  source: &str,
  source_path: PathBuf,
  config: Config,
  test_fn: &dyn Fn(TestPackage) -> SherpaResult<T>,
) -> SherpaResult<T> {
  build_parse_db_from_source_str(source, source_path, config, &|DBPackage { journal, db, soup }| {
    let states = compile_parse_states(journal.transfer(), &db)?;

    let states = optimize::<ParseStatesVec>(&db, states)?;

    test_fn(TestPackage { journal, states, db: &db, soup: &soup })
  })
}

/// Compile a parser Data base
pub fn build_parse_db_from_source_str<'a, T>(
  source: &str,
  source_path: PathBuf,
  config: Config,
  test_fn: &dyn Fn(DBPackage) -> SherpaResult<T>,
) -> SherpaResult<T> {
  let mut journal = Journal::new(Some(config));
  let soup = GrammarSoup::new();

  journal.set_active_report("test", ReportType::Any);

  let id = compile_grammar_from_str(&mut journal, source, source_path, &soup)?;

  journal.flush_reports();

  let db = build_compile_db(journal.transfer(), id, &soup)?;

  journal.flush_reports();

  test_fn(DBPackage { journal, db: &db, soup: &soup })
}
