use std::ops::Index;

use super::{
  ASTToken,
  Array,
  GuardedStr,
  IString,
  IStringStore,
  Rule,
  SymbolId,
};

pub type IndexedRule = (Rule, IndexedProdId);

/// Data used for the compilation of parse states. contains
/// additional metadata for compilation of LLVM and Bytecode
/// parsers.
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct CompileDatabase {
  /// The name of the parser as defined by the `NAME <name>` preamble in
  /// the root grammar, or by the filename stem in the path for the root
  /// grammar.
  pub name:      IString,
  /// Table of production symbols.
  prod_syms:     Array<SymbolId>,
  /// Table of production names for public productions.
  /// This is a 1-to-1 mapping of all production indices, so productions
  /// that are scanner or are sub-productions map to empty strings.
  prod_name_lu:  Array<IString>,
  /// Table mapping production indices to rule indices.
  prod_to_rules: Array<Array<IndexRuleKey>>,
  /// Table of all rules within the grammar and the non-terminal they reduce
  /// to.
  rules:         Array<IndexedRule>,
  /// Table of all AUTHORED symbols within the grammar. Indices MUST be
  /// offset by `SymbolId::DefinedSymbolIndexBasis` to be used as bytecode
  /// token values.
  sym_lu:        Array<(SymbolId, Option<u32>)>,
  /// The entry point productions of the grammar.
  entry_points:  Array<(usize, IString)>,
  /// The global string store
  string_store:  IStringStore,
}

impl CompileDatabase {
  pub fn new(
    name: IString,
    prod_syms: Array<SymbolId>,
    prod_name_lu: Array<IString>,
    prod_to_rules: Array<Array<IndexRuleKey>>,
    rules: Array<IndexedRule>,
    sym_lu: Array<(SymbolId, Option<u32>)>,
    entry_points: Array<(usize, IString)>,
    string_store: IStringStore,
  ) -> Self {
    Self {
      name,
      prod_syms,
      prod_name_lu,
      prod_to_rules,
      rules,
      sym_lu,
      entry_points,
      string_store,
    }
  }

  /// Returns the number of productions stored in the DB
  pub fn prod_len(&self) -> usize {
    self.prod_syms.len()
  }

  /// Returns an ordered array of all productions within the DB
  pub fn productions(&self) -> &Array<SymbolId> {
    &self.prod_syms
  }

  /// Given an [IndexedProdId] returns the SymbolId representing the production,
  /// or [SymbolId::Undefined] if the id is invalid.
  pub fn prod_sym(&self, key: IndexedProdId) -> SymbolId {
    self.prod_syms.get(key.0 as usize).cloned().unwrap_or_default()
  }

  /// Given an [IndexedProdId] returns an IString comprising the name of the
  /// production, or an empty string if the id is invalid.
  pub fn prod_name(&self, key: IndexedProdId) -> IString {
    self.prod_name_lu.get(key.0 as usize).cloned().unwrap_or_default()
  }

  /// Given an [IndexedProdId] returns a [GuardedStr] of the production's name.
  /// Returns an empty string if the key is invalid.
  pub fn prod_name_str(&self, key: IndexedProdId) -> GuardedStr {
    let string =
      self.prod_name_lu.get(key.0 as usize).cloned().unwrap_or_default();
    string.to_str(&self.string_store)
  }

  /// Given an [IndexedSymId] returns the SymbolId representing the production,
  /// or [SymbolId::Undefined] if the id is invalid.
  pub fn sym(&self, key: IndexedSymId) -> SymbolId {
    self.sym_lu.get(key.0 as usize).cloned().unwrap_or_default().0
  }

  /// Given an [IndexedSymId] returns the bytecode id of the symbol, if it
  /// has ono, or `None`
  pub fn sym_bytecode_id(&self, key: IndexedSymId) -> Option<u32> {
    self.sym_lu.get(key.0 as usize).cloned().unwrap_or_default().1
  }

  /// Given an [IndexedProdId] returns an [Array] of [IndexRuleKey], or `None`
  /// if the id is invalid.
  pub fn prod_rules(&self, key: IndexedProdId) -> Option<&Array<IndexRuleKey>> {
    self.prod_to_rules.get(key.0 as usize)
  }

  /// Given an [IndexRuleKey] returns an [Rule], or `None` if
  /// the id is invalid.
  pub fn rule(&self, key: IndexRuleKey) -> &Rule {
    self.rules.get(key.0 as usize).map(|(r, _)| r).unwrap()
  }

  /// Given an [IndexRuleKey] returns the [IndexedProdId] the rule reduces to.
  pub fn rule_prod(&self, key: IndexRuleKey) -> IndexedProdId {
    self.rules.get(key.0 as usize).map(|(_, p)| *p).unwrap_or_default()
  }

  /// Returns a reference to the [IStringStore]
  pub fn string_store(&self) -> &IStringStore {
    &self.string_store
  }
}

macro_rules! indexed_id_implementations {
  ($id_type:ty) => {
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

    impl Default for $id_type {
      fn default() -> Self {
        Self(u32::MAX)
      }
    }
  };
}

/// An opaque key used for the access of a rule in a [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct IndexRuleKey(u32);
indexed_id_implementations!(IndexRuleKey);

/// Used as a lookup key for a symbol stored within a
/// [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct IndexedSymId(u32);
indexed_id_implementations!(IndexedSymId);

/// Used as a lookup key for production properties stored within a
/// [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct IndexedProdId(u32);
indexed_id_implementations!(IndexedProdId);

impl IndexedSymId {
  pub fn to_state_val(&self) -> u32 {
    self.0
  }

  pub fn to_index(&self) -> usize {
    (self.0 - SymbolId::DefinedSymbolIndexBasis) as usize
  }

  pub fn to_sym(&self, db: &CompileDatabase) -> SymbolId {
    match self.0 {
      1 => SymbolId::EndOfFile { precedence: 0 },
      2 => SymbolId::GenericSymbol { precedence: 0 },
      3 => SymbolId::GenericIdentifier { precedence: 0 },
      4 => SymbolId::GenericNumber { precedence: 0 },
      5 => SymbolId::GenericNewLine { precedence: 0 },
      6 => SymbolId::GenericSpace { precedence: 0 },
      7 => SymbolId::GenericHorizontalTab { precedence: 0 },
      _ if self.to_index() < db.sym_lu.len() => db.sym_lu[self.0 as usize].0,
      _ => SymbolId::Undefined,
    }
  }
}
