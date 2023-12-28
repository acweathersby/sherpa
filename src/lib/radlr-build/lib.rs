use radlr_core::RadlrGrammar;
pub use radlr_core::RadlrResult;
use std::path::Path;

mod targets;

#[derive(Clone, Copy)]
pub enum TargetLanguage {
  Rust,
  TypeScript,
  JavaScript,
  C,
  Cpp,
  Llvm,
}

#[derive(Clone, Copy)]
pub enum ParserType {
  /// Builds a bytecode parser to be run by the Radlr bytecode interpreter
  /// of target langauge.
  Bytecode,
  /// Build a binary bytecode parser for the target machine. Not all target
  /// languages support this parsing mode.
  Binary,
}

/// Configurations for artifacts built by radlr-build.
#[derive(Clone, Copy)]
pub struct BuildConfig<'a> {
  pub ast_struct_name: &'a str,

  /// Create an ast artifact file in the target language based on `:ast`
  /// definitions found in grammar files.
  ///
  /// Defaults to true
  pub build_ast: bool,

  pub include_debug_symbols: bool,

  /// Directory to place library type artifacts including
  /// binary files and shared/static libraries.
  ///
  /// Defaults to the parent dir of `root_grammar_path` if lib out is not
  /// set.
  pub lib_out: &'a Path,

  /// Directory to place source type artifacts such as
  /// generated parser and AST code files.
  ///
  /// Defaults the parent dir of `root_grammar_path` if lib out is not
  /// set.
  pub source_out: &'a Path,

  /// The type of parser to construct.
  ///
  /// Defaults to Bytecode.
  pub parser_type: ParserType,

  /// Path of the root grammar file.
  root_grammar_path: &'a Path,

  /// Paths to search for `radlr` input files.
  include_paths: &'a [&'a Path],
}

impl<'a> BuildConfig<'a> {
  pub fn new(root_grammar: &'a Path) -> Self {
    BuildConfig {
      ast_struct_name: "AST_TestNode",
      lib_out: &root_grammar.parent().unwrap(),
      source_out: &root_grammar.parent().unwrap(),
      build_ast: true,
      include_debug_symbols: false,
      root_grammar_path: root_grammar,
      parser_type: ParserType::Bytecode,
      include_paths: &[],
    }
  }
}

/// Build a Radlr parser from a grammar file
pub fn fs_build(build_config: BuildConfig, parser_config: radlr_core::ParserConfig, target: TargetLanguage) -> RadlrResult<()> {
  let BuildConfig { lib_out, source_out, root_grammar_path, .. } = build_config;

  let db = RadlrGrammar::new().add_source(root_grammar_path)?.build_db(root_grammar_path, parser_config)?;

  std::fs::create_dir_all(lib_out)?;
  std::fs::create_dir_all(source_out)?;

  match target {
    TargetLanguage::Rust => {
      targets::rust::build(&db, build_config, parser_config)?;
    }
    TargetLanguage::TypeScript => {
      targets::typescript::build(&db, build_config, parser_config)?;
    }
    TargetLanguage::JavaScript => {
      todo!("Build Javascript: Not yet supported")
    }
    TargetLanguage::Cpp => {
      todo!("Build Cpp: Not yet supported")
    }
    TargetLanguage::C => {
      todo!("Build C: Not yet supported")
    }
    TargetLanguage::Llvm => {
      todo!("Build Llvm: Not yet supported")
    }
    _ => {}
  }

  Ok(())
}

#[test]
fn builds_basic_grammar() -> RadlrResult<()> {
  let root = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).canonicalize()?;
  let path = root.join("test_grammar.radlr");
  let output = root.join("build");

  let mut build_config = BuildConfig::new(&path);
  build_config.source_out = &output;
  build_config.lib_out = &output;

  fs_build(build_config, Default::default(), TargetLanguage::Rust)?;

  Ok(())
}
