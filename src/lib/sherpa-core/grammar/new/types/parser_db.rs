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

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DBRule {
  pub rule:       Rule,
  pub prod_id:    DBProdKey,
  pub is_scanner: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DBTokenData {
  /// The symbol type and precedence.
  pub sym_id:  SymbolId,
  /// The scanner production id of this symbol.
  pub prod_id: DBProdKey,
  /// The id of the symbol when used as a lexer token.
  pub tok_id:  usize,
}

#[derive(Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct EntryPoint {
  pub prod_key:   DBProdKey,
  pub entry_name: IString,
}

/// Data used for the compilation of parse states. contains
/// additional metadata for compilation of LLVM and Bytecode
/// parsers.
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ParserDatabase {
  /// The name of the parser as defined by the `NAME <name>` preamble in
  /// the root grammar, or by the filename stem in the path for the root
  /// grammar.
  pub name:     IString,
  /// Table of production symbols.
  prod_syms:    Array<SymbolId>,
  /// Table of production names for public productions.
  /// This is a 1-to-1 mapping of all production indices, so productions
  /// that are scanner or are sub-productions map to empty strings.
  prod_names:   Array<IString>,
  /// Table mapping production indices to rule indices.
  prod_rules:   Array<Array<DBRuleKey>>,
  /// Table of all rules within the grammar and the non-terminal they reduce
  /// to.
  rules:        Array<DBRule>,
  /// Table of all AUTHORED symbols within the grammar. Indices MUST be
  /// offset by `SymbolId::DefinedSymbolIndexBasis` to be used as bytecode
  /// token values.
  tokens:       Array<DBTokenData>,
  /// The entry point productions of the grammar.
  entry_points: Array<EntryPoint>,
  /// The global string store
  string_store: IStringStore,
}

impl ParserDatabase {
  pub fn new(
    name: IString,
    prod_syms: Array<SymbolId>,
    prod_name_lu: Array<IString>,
    prod_to_rules: Array<Array<DBRuleKey>>,
    rules: Array<DBRule>,
    token_lu: Array<DBTokenData>,
    entry_points: Array<EntryPoint>,
    string_store: IStringStore,
  ) -> Self {
    Self {
      name,
      prod_syms,
      prod_names: prod_name_lu,
      prod_rules: prod_to_rules,
      rules,
      tokens: token_lu,
      entry_points,
      string_store,
    }
  }

  /// Returns an array [DBProdKey]s of the entry point productions.
  pub fn entry_prod_keys(&self) -> Array<DBProdKey> {
    self.entry_points.iter().map(|k| k.prod_key).collect()
  }

  /// Returns the number of productions stored in the DB
  pub fn prod_len(&self) -> usize {
    self.prod_syms.len()
  }

  /// Returns an ordered array of all productions within the DB
  pub fn productions(&self) -> &Array<SymbolId> {
    &self.prod_syms
  }

  /// Given an [DBProdKey] returns the SymbolId representing the production,
  /// or [SymbolId::Undefined] if the id is invalid.
  pub fn prod_sym(&self, key: DBProdKey) -> SymbolId {
    self.prod_syms.get(key.0 as usize).cloned().unwrap_or_default()
  }

  /// Given an [DBProdKey] returns an IString comprising the name of the
  /// production, or an empty string if the id is invalid.
  pub fn prod_name(&self, key: DBProdKey) -> IString {
    self.prod_names.get(key.0 as usize).cloned().unwrap_or_default()
  }

  /// Given an [DBProdKey] returns a [GuardedStr] of the production's name.
  /// Returns an empty string if the key is invalid.
  pub fn prod_name_str<'a>(&'a self, key: DBProdKey) -> GuardedStr<'a> {
    self.prod_names.get(key.0 as usize).unwrap().to_str(&self.string_store)
  }

  /// Given an [DBSymKey] returns the associated [SymbolId]
  pub fn sym(&self, key: DBTokenKey) -> SymbolId {
    self.tokens.get(key.0 as usize).map(|s| s.sym_id).unwrap_or_default()
  }

  /// Given an [DBSymKey] returns the token identifier representing the symbol,
  pub fn tok_id(&self, key: DBTokenKey) -> usize {
    self.tokens.get(key.0 as usize).map(|s| s.tok_id).unwrap_or_default()
  }

  /// Given an [DBSymKey] returns the token identifier representing the symbol,
  pub fn tok_data(&self, key: DBTokenKey) -> &DBTokenData {
    self.tokens.get(key.0 as usize).as_ref().unwrap()
  }

  /// Given an [DBSymKey] returns the SymbolId representing the scanner
  /// production for the symbol, or None
  pub fn tok_prod(&self, key: DBTokenKey) -> Option<DBProdKey> {
    self.tokens.get(key.0 as usize).map(|s| s.prod_id)
  }

  /// Given an [DBProdKey] returns an [Array] of [DBRuleKey], or `None`
  /// if the id is invalid.
  pub fn prod_rules(&self, key: DBProdKey) -> Option<&Array<DBRuleKey>> {
    self.prod_rules.get(key.0 as usize)
  }

  /// Given an [DBRuleKey] returns an [Rule], or `None` if
  /// the id is invalid.
  pub fn rule(&self, key: DBRuleKey) -> &Rule {
    self.rules.get(key.0 as usize).map(|e| &e.rule).unwrap()
  }

  /// Given an [DBRuleKey] returns the [DBProdKey] the rule reduces to.
  pub fn rule_prod(&self, key: DBRuleKey) -> DBProdKey {
    self.rules.get(key.0 as usize).map(|e| e.prod_id).unwrap_or_default()
  }

  /// Returns a reference to the [IStringStore]
  pub fn string_store(&self) -> &IStringStore {
    &self.string_store
  }
}

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
pub struct DBRuleKey(u32);
indexed_id_implementations!(DBRuleKey);

/// Used as a lookup key for production data stored within a
/// [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DBProdKey(u32);
indexed_id_implementations!(DBProdKey);

impl DBProdKey {
  /// Returns the symbol representation of this index.
  pub fn to_sym(&self) -> SymbolId {
    SymbolId::DBNonTerminal { key: *self }
  }
}

/// Used as a lookup key for a symbol data within a
/// [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DBTokenKey(u32);
indexed_id_implementations!(DBTokenKey);

impl DBTokenKey {
  pub fn default_sym() -> Self {
    Self(0)
  }

  /// Retrieves the binary / bytecode form of the symbol.
  pub fn to_state_val(&self) -> u32 {
    self.0
  }

  pub fn to_index(&self) -> usize {
    (self.0) as usize
  }
}
