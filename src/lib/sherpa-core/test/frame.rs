use std::path::PathBuf;

use crate::{
  build_compile_db,
  compile_grammar_from_str,
  compile_parse_states,
  garbage_collect,
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

/// Simple single thread compilation of a grammar source string.
/// `test_fn` is called after a successful compilation of parse states.
pub fn build_parse_states_from_source_str<'a>(
  source: &str,
  source_path: PathBuf,
  config: Config,
  test_fn: fn(pkg: TestPackage) -> SherpaResult<()>,
) -> SherpaResult<()> {
  let mut journal = Journal::new(Some(config));
  let soup = GrammarSoup::new();

  journal.set_active_report("test", ReportType::Any);

  let id = compile_grammar_from_str(&mut journal, source, source_path, &soup)?;

  journal.flush_reports();

  let db = build_compile_db(journal.transfer(), id, &soup)?;

  journal.flush_reports();

  let states = compile_parse_states(journal.transfer(), &db)?;

  let states = garbage_collect::<ParseStatesVec>(&db, states)?;

  journal.flush_reports();

  test_fn(TestPackage { journal, states, db: &db, soup: &soup })
}
