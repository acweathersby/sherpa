use bitmask_enum::bitmask;

#[bitmask]
pub enum ParseAlgorithm {
  /// Allow the use of LR states to resolve conflicts when building regular parsers
  LR_Resolution = 0b1,
  /// Allow the use of recursive ascent states to resolve left recursion
  RecursiveAscent = 0b10,
  /// Resort to creating Fork states when there are ambiguities.
  /// If this is not enabled, ambiguous grammar will fail to compile.
  Fork = 0b100,
  /// Allow the use of LR states to resolve
  /// conflicts when building token parsers
  Token_LR_Resolution = 0b1000,
  /// Use all methods available to resolve
  /// conflicts
  All  = 0b1111,
}

impl Default for ParseAlgorithm {
  fn default() -> Self {
    Self::All
  }
}

#[derive(Debug, Clone)]
pub struct Config {
  /// Combine [ParseAlgorithm] flags to specify what parser algorithms
  /// are available to the compiler.
  ///
  /// Defaults to [ParseAlgorithm::All]
  pub enabled_algorithms: ParseAlgorithm,
  /// The Parser will produce peek productions for symbols that occlude.
  /// An extra compile step must be taken to produce the symbol occlusion table.
  ///
  /// Defaults to `false`.
  ///
  /// The occlusion table maps a symbol to all lower precedent symbols that may occlude it.
  ///
  /// The idea here is to add symbols with lower precedence to the occlusion table
  /// of symbols with higher precedence. For example, given this grammar
  /// ```
  /// <> A > \funct \(
  ///    |   tk:id  \{
  ///
  /// <> id > g:id(+)
  /// ```
  /// The DefinedSymbol `\funct` has a higher precedence then TokenProduction symbol `tk:id`.
  /// When using the occlusion table, we force the compiler to consider the symbols as the
  /// "same", which should then cause it to generate a peek state that uses the
  /// symbols `(` and `{` to resolve the conflict.
  ///
  pub allow_occluding_symbols: bool,
  /// Convert bytecode into a human readable "disassembly" format. This can be
  /// accessed in the journal through `Disassembly` report type. The main note
  /// in that report is labeled "Output", which will contain the
  /// dissemble text.
  ///
  /// Defaults to `false`.
  pub build_disassembly: bool,
  /// Add IR states string dump to IR compilation reports. This affects the ReportTypes
  /// - [ReportType::TokenProductionCompile(ProductionId)](ReportType)
  /// - [ReportType::ScannerCompile(ScannerId)](ReportType)
  /// - [ReportType::ProductionCompile(ProductionId)](ReportType)
  ///
  /// The note can be found labeled `IRStates`
  ///
  /// Defaults to `false`.
  pub debug_add_ir_states_note: bool,
  /// Uses the bread crumb parser technique to overcome local ambiguities
  /// without backtracking.
  ///
  /// Defaults to `false`.
  pub enable_breadcrumb_parsing: bool,
  /// Inline instructions of branches that have the same signature
  /// and contain no transitive actions (shift, reduce, accept).
  /// This will decrease parser instructions at the expense of increasing
  /// bytecode size.
  ///
  /// Defaults to `false`.
  pub opt_inline_redundant_assertions: bool,
  /// Remove goto statements that reference a pass state (a state that only
  /// has one `pass` instruction).
  ///
  /// Defaults to `true`.
  pub opt_remove_gotos_to_pass_states: bool,
  /// Run optimization passes on LLVM code.
  ///
  /// Defaults to `false`.
  pub opt_llvm: bool,

  /// Enable the generation of AScripT AST code.
  ///
  /// Defaults to `true`
  pub enable_ascript: bool,

  /// The language type of non-LLVM outputs
  ///
  /// Defaults to [SourceType::Rust]
  pub source_type:     SourceType,
  /// Path to the `llvm_ar` executable.
  ///
  /// Defaults to `llvm-ar-14`
  pub llvm_ar_path:    String,
  /// Path to the `clang` executable.
  ///
  /// Defaults to `clang-14`
  pub llvm_clang_path: String,
  /// Enable LLVM light link time optimizations
  ///
  /// Default to `false`
  pub llvm_light_lto:  bool,
}

impl Default for Config {
  fn default() -> Self {
    Self {
      enabled_algorithms: ParseAlgorithm::All,
      allow_occluding_symbols: false,
      build_disassembly: false,
      debug_add_ir_states_note: false,
      enable_breadcrumb_parsing: false,
      opt_inline_redundant_assertions: false,
      opt_remove_gotos_to_pass_states: true,
      enable_ascript: true,
      source_type: SourceType::Rust,
      llvm_ar_path: "llvm-ar-14".to_string(),
      llvm_clang_path: "clang-14".to_string(),
      llvm_light_lto: false,
      opt_llvm: false,
    }
  }
}

use std::collections::BTreeSet;

use crate::build::pipeline::SourceType;
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Language {
  Rust,
  TypeScript,
  Cpp,
  Go,
  WebAssembly,
  Java,
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Recognizer {
  Assembly,
  Bytecode,
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Platform {
  Windows,
  Linux,
  MacOS,
  Android,
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Architecture {
  X8664,
  Arm64,
}

impl Architecture {
  pub fn is_compatible(&self, pl: Platform, ot: Language, rt: Recognizer) -> bool {
    use Language::*;
    use Platform::*;
    use Recognizer::*;

    if matches!(rt, Recognizer::Bytecode) {
      false
    } else {
      match ot {
        Rust | Cpp | Go => match pl {
          Windows => matches!(self, X8664),
          Android => false,
          _ => true,
        },
        _ => false,
      }
    }
  }
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Extension {
  AVX2,
  AVX512,
  SSE,
}

pub struct BuildOptions {
  pub architecture: Architecture,
  pub recognizer:   Recognizer,
  pub platform:     Platform,
  pub language:     Language,
  pub extensions:   BTreeSet<Extension>,
}

impl Default for BuildOptions {
  fn default() -> Self {
    Self {
      architecture: Architecture::X8664,
      language:     Language::Rust,
      platform:     Platform::Linux,
      recognizer:   Recognizer::Bytecode,
      extensions:   BTreeSet::new(),
    }
  }
}

impl BuildOptions {
  pub fn has_extension(&self, ext: Extension) -> bool {
    self.extensions.contains(&ext)
  }
}
