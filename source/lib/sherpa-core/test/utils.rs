use std::{path::PathBuf, str::FromStr, sync::Arc};

use crate::{compile::GrammarStore, Config, Journal, SherpaResult};

/// Return the full filepath of a grammar stored in
/// ./tests/grammars/
pub(super) fn get_test_grammar_path(partial_path: &str) -> PathBuf {
  let path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../test/grammars/").join(partial_path);
  dbg!(&path);
  path.canonicalize().unwrap()
}

/// Creates and compiles a Journal and GrammarStore from a file path and configuration object.
pub(super) fn build_grammar_from_file(
  input: PathBuf,
  config: Config,
) -> (Journal, SherpaResult<Arc<GrammarStore>>) {
  let mut j = Journal::new(Some(config));
  let g = GrammarStore::from_path(&mut j, input);
  (j, g)
}

/// Return a path relative to `<sherpa_repo>/source/`
pub(crate) fn path_from_source(source_path: &str) -> SherpaResult<PathBuf> {
  let grammar_path = std::env::var("CARGO_MANIFEST_DIR")
    .map(|val| PathBuf::from_str(&val).unwrap().join("../../"))
    .unwrap_or_else(|_| {
      std::env::current_dir().unwrap().join(PathBuf::from_str("./source/").unwrap())
    })
    .join(source_path);

  if let Ok(grammar_path) = grammar_path.canonicalize() {
    SherpaResult::Ok(grammar_path)
  } else {
    panic!("File path {} not found in file system", grammar_path.to_str().unwrap());
  }
}
