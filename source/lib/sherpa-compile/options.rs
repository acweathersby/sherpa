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
