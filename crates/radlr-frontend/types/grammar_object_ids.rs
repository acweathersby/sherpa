use std::fmt::Debug;

use super::symbol::SymbolId;
use crate::types::{NonTermId, NonTermSubType};
use radlr_core_common::{utils::create_u64_hash, CachedString, IString};

macro_rules! indexed_id_implementations {
  ($id_type:ty) => {
    impl $id_type {
      pub fn to_string(&self) -> String {
        self.0.to_string()
      }
    }

    impl From<u32> for $id_type {
      fn from(value: u32) -> Self {
        Self(value)
      }
    }

    impl From<usize> for $id_type {
      fn from(value: usize) -> Self {
        Self(value as u32)
      }
    }

    impl Into<usize> for $id_type {
      fn into(self) -> usize {
        self.0 as usize
      }
    }

    impl Into<u32> for $id_type {
      fn into(self) -> u32 {
        self.0 as u32
      }
    }

    impl Default for $id_type {
      fn default() -> Self {
        Self(u32::MAX)
      }
    }
  };
}

/// A globally unique identifier for a single grammar source file. Derived
/// from the source's absolute resource path.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct GrammarId(pub(crate) IString);

impl GrammarId {
  pub fn as_u64(&self) -> u64 {
    self.0.as_u64()
  }
}

/// Set of identifiers for a single grammar source
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GrammarIdentities {
  /// A globally unique identifier for this GrammarStore instance. Derived
  /// from the source path. Assumes the source path is an absolute path
  /// to a grammar source file.
  pub guid: GrammarId,

  /// A globally unique name for this grammar.
  pub guid_name: IString,

  /// A name defined by the grammar author. This is either the value of the
  /// `NAME` preamble, or the original file name stem if this preamble is
  /// not present.
  pub local_name: IString,

  /// The absolute path of the grammar's source file. This may be empty if the
  /// source code was passed in as a string, as with the case of grammars
  /// compiled with
  /// [compile_grammar_from_string](radlr_core::grammar::compile_grammar_from_string)).
  pub path: IString,
}

impl Debug for GrammarIdentities {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("{}[{}]", self.guid_name.to_str().as_str(), self.local_name.to_str().as_str()))
  }
}

impl GrammarIdentities {
  pub fn from_path(grammar_source_path: &std::path::Path) -> Self {
    let path = grammar_source_path.intern();
    Self { guid: GrammarId(path), path: path, ..Default::default() }
  }
}
