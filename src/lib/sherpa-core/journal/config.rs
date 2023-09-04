#[derive(Default, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DebugConfig {
  /// Allow further processing of the parse states when
  /// parse states with the same name but different contents
  /// are encountered
  pub allow_parse_state_name_collisions: bool,
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
  pub fn _is_compatible(&self, pl: Platform, ot: Language, rt: Recognizer) -> bool {
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
