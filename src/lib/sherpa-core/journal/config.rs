#[derive(Default, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DebugConfig {
  /// Allow further processing of the parse states when
  /// parse states with the same name but different contents
  /// are encourntered
  pub allow_parse_state_name_collisions: bool,
}

/// General Compiler Configuration
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Config {
  /// > !NOT IMPLEMENTED!
  ///
  /// The Parser will produce peek productions for symbols that occlude.
  /// An extra compile step must be taken to produce the symbol occlusion
  /// table.
  ///
  /// Defaults to `false`.
  ///
  /// The occlusion table maps a symbol to all lower precedent symbols that may
  /// occlude it.
  ///
  /// The idea here is to add symbols with lower precedence to the occlusion
  /// table of symbols with higher precedence. For example, given this
  /// grammar ```hcg
  /// <> A > \funct \(
  ///    |   tk:id  \{
  ///
  /// <> id > g:id(+)
  /// ```
  /// The DefinedSymbol `\funct` has a higher precedence then TokenProduction
  /// symbol `tk:id`. When using the occlusion table, we force the compiler
  /// to consider the symbols as the "same", which should then cause it to
  /// generate a peek state that uses the symbols `(` and `{` to resolve the
  /// conflict.
  pub allow_occluding_tokens: bool,
  /// Convert bytecode into a human readable "disassembly" format. This can be
  /// accessed in the journal through `Disassembly` report type. The main note
  /// in that report is labeled "Output", which will contain the
  /// dissemble text.
  ///
  /// Defaults to `false`.
  pub build_disassembly: bool,
  /// Add IR states string dump to IR compilation reports. This affects the
  /// ReportTypes
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
  /// This will decrease parser states at the expense of increasing
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
  /// Configurations when running in debug mode.
  pub debug:          DebugConfig,
}

impl Default for Config {
  fn default() -> Self {
    Self {
      allow_occluding_tokens: false,
      build_disassembly: false,
      debug_add_ir_states_note: false,
      enable_breadcrumb_parsing: false,
      opt_inline_redundant_assertions: false,
      opt_remove_gotos_to_pass_states: true,
      enable_ascript: true,
      opt_llvm: false,
      debug: Default::default(),
    }
  }
}

use std::collections::BTreeSet;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Language {
  Rust,
  _TypeScript,
  _Cpp,
  _Go,
  _WebAssembly,
  _Java,
}
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Recognizer {
  _Assembly,
  Bytecode,
}
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Platform {
  _Windows,
  Linux,
  _MacOS,
  _Android,
}
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Architecture {
  X8664,
  _Arm64,
}

impl Architecture {
  pub fn _is_compatible(
    &self,
    pl: Platform,
    ot: Language,
    rt: Recognizer,
  ) -> bool {
    use Architecture::*;
    use Language::*;
    use Platform::*;

    if matches!(rt, Recognizer::Bytecode) {
      false
    } else {
      match ot {
        Rust | _Cpp | _Go => match pl {
          _Windows => matches!(self, X8664),
          _Android => false,
          _ => true,
        },
        _ => false,
      }
    }
  }
}
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Extension {
  _AVX2,
  _AVX512,
  _SSE,
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
  pub fn _has_extension(&self, ext: Extension) -> bool {
    self.extensions.contains(&ext)
  }
}
