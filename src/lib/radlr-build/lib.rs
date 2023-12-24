mod targets;

use std::path::Path;

use radlr_core::RadlrGrammar;
pub use radlr_core::RadlrResult;

pub enum RadlrBuildTarget {
  Rust,
  TypeScript,
  JavaScript,
  C,
  Cpp,
  Llvm,
}

/// Configurations for artifacts built by radlr-build.
#[derive(Clone, Copy)]
pub struct BuildConfig<'a> {
  pub ast_struct_name: &'a str,
  pub build_ast: bool,
  pub include_debug_symbols: bool,
}

const DEFAULT_AST_NAME: &str = "AST_TestNode";

impl<'a> Default for BuildConfig<'a> {
  fn default() -> Self {
    BuildConfig {
      ast_struct_name: DEFAULT_AST_NAME,
      build_ast: true,
      include_debug_symbols: false,
    }
  }
}

/// Build a Radlr parser from a set grammar files.
pub fn fs_build(
  root_grammar: &Path,
  output_dir: &Path,
  build_config: BuildConfig,
  parser_config: radlr_core::ParserConfig,
  target: RadlrBuildTarget,
) -> RadlrResult<()> {
  let db = RadlrGrammar::new().add_source(root_grammar)?.build_db(root_grammar, parser_config)?;

  match target {
    RadlrBuildTarget::Rust => {
      targets::rust::build(&db, build_config, parser_config, output_dir)?;
    }
    RadlrBuildTarget::TypeScript => {
      targets::typescript::build(&db, build_config, parser_config, output_dir)?;
    }
    RadlrBuildTarget::JavaScript => {}
    RadlrBuildTarget::Cpp => {}
    RadlrBuildTarget::C => {}
    RadlrBuildTarget::Llvm => {}
    _ => {}
  }

  Ok(())
}

#[test]
fn builds_basic_grammar() -> RadlrResult<()> {
  let root = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).canonicalize()?;
  let path = root.join("test_grammar.radlr");
  let output = root.join("build");
  fs_build(&path, &output, Default::default(), Default::default(), RadlrBuildTarget::TypeScript)?;
  Ok(())
}
