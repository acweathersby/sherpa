use std::{path::PathBuf, sync::Arc};

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

  j.flush_reports();

  (j, g)
}
