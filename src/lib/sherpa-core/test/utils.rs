use crate::{
  DBPackage,
  GrammarSoup,
  Journal,
  ParseStatesMap,
  ParserDatabase,
  ReportType,
  SherpaGrammarBuilder,
  SherpaParserBuilder,
  SherpaResult,
  TestPackage,
};
use std::path::PathBuf;

/// Simple single thread compilation of a grammar source string.
/// `test_fn` is called after a successful compilation of parse states.
pub fn build_parse_states_from_source_str<'a, T>(
  source: &str,
  source_path: PathBuf,
  optimize: bool,
  test_fn: &dyn Fn(TestPackage) -> SherpaResult<T>,
) -> SherpaResult<T> {
  if optimize {
    test_fn(
      SherpaGrammarBuilder::new()
        .add_source_from_string(source, &source_path)?
        .build_db(&source_path)?
        .build_parser()?
        .optimize(false)?
        .into(),
    )
  } else {
    test_fn(
      SherpaGrammarBuilder::new().add_source_from_string(source, &source_path)?.build_db(&source_path)?.build_parser()?.into(),
    )
  }
}

/// Builds a set of states from one or more source strings.
/// Each `source` is mapped to a single character  name in this sequence
/// `ABCDEFGHIJKLMNOPQRSTUVWXYZ`
pub fn build_parse_states_from_multi_sources<'a, T>(
  sources: &[&str],
  source_path: PathBuf,
  optimize: bool,
  test_fn: &dyn Fn(TestPackage) -> SherpaResult<T>,
) -> SherpaResult<T> {
  let mut grammar = SherpaGrammarBuilder::new();

  for (index, source) in sources.iter().enumerate() {
    let source_path = source_path.join("ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars().nth(index).unwrap().to_string());
    grammar = grammar.add_source_from_string(source, &source_path)?;
  }

  let root_path = source_path.join("A");

  if optimize {
    test_fn(grammar.build_db(&root_path)?.build_parser()?.optimize(false)?.into())
  } else {
    test_fn(grammar.build_db(&root_path)?.build_parser()?.into())
  }
}

/// Compile a parser Data base
pub fn build_parse_db_from_source_str<'a, T>(
  source: &str,
  source_path: PathBuf,
  test_fn: &dyn Fn(DBPackage) -> SherpaResult<T>,
) -> SherpaResult<T> {
  test_fn(SherpaGrammarBuilder::new().add_source_from_string(source, &source_path)?.build_db(&source_path)?.into())
}
