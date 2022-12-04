use bitmask_enum::bitmask;

#[bitmask]
pub enum ResolutionMode {
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

impl Default for ResolutionMode {
  fn default() -> Self {
    Self::All
  }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Config {
  pub resolution_mode: ResolutionMode,

  /// The Parser will produce peek productions for symbols that occlude.
  /// An extra compile step must be taken to produce the symbol occlusion table.
  pub allow_occluding_symbols:   bool,
  /// Convert bytecode into a human readable "disassembly" format. This can be
  /// accessed in the journal through `Disassembly` report type. The main note
  /// in that report is labeled "Output", which will contain the
  /// dissemble text.
  pub build_disassembly:         bool,
  /// Add IR states string dump to IR compilation reports. This affects the ReportTypes
  /// - [ReportType::TokenProductionCompile(ProductionId)](ReportType)
  /// - [ReportType::ScannerCompile(ScannerId)](ReportType)
  /// - [ReportType::ProductionCompile(ProductionId)](ReportType)
  ///
  /// The note can be found labeled `IRStates`
  pub debug_add_ir_states_note:  bool,
  /// Uses the bread crumb parser technique to overcome local
  /// ambiguities.
  pub enable_breadcrumb_parsing: bool,
}

use std::collections::BTreeSet;
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
#[derive(Debug, Clone, Copy)]
pub enum SourceType {
  Rust,
  TypeScript,
  Go,
  Cpp,
}
