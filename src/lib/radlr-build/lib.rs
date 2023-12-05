mod targets;

use std::path::Path;

use radlr_core::RadlrGrammar;
pub use radlr_core::RadlrResult;

pub fn build(grammar_path: &Path, output_dir: &Path) -> RadlrResult<()> {
  let db = RadlrGrammar::new().add_source(grammar_path)?.build_db(grammar_path, Default::default())?;

  targets::rust::build(&db, output_dir)?;

  Ok(())
}
