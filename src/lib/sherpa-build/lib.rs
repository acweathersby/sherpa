mod targets;

use std::path::Path;

use sherpa_core::SherpaGrammar;
pub use sherpa_core::SherpaResult;

pub fn build(grammar_path: &Path, output_dir: &Path) -> SherpaResult<()> {
  let db = SherpaGrammar::new().add_source(grammar_path)?.build_db(grammar_path, Default::default())?;

  targets::rust::build(&db, output_dir)?;

  Ok(())
}
