use std::{fmt::Debug, hash::Hash};

use self::{grammar_object_ids::GrammarIdentities, symbol::SymbolId};
use radlr_core_common::{utils::create_u64_hash, IString};
use radlr_rust_runtime::types::Token;

pub mod grammar_object_ids;
pub mod item;
pub mod parser_db;
pub mod rule;
pub mod symbol;

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, Hash, Default)]
pub enum GraphType {
  /// Classic Recursive Descent Ascent with unlimited lookahead.
  #[default]
  Parser,
  // Scanner mode for creating tokens, analogous to regular expressions.
  Scanner,
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

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum NonTermType {
  ContextFree,
  Pratt,
  Peg,
  ParseState,
}

#[derive(Clone, Default)]
pub struct SymbolRef {
  /// The type of this symbol.
  pub id:             SymbolId,
  /// The original location of this symbol within the grammar source
  pub loc:            Token,
  /// The reference name of this symbol
  pub annotation:     IString,
  /// The original positional index of the symbol within the original base rule.
  pub original_index: u32,
  /// The precedence of this symbol when present in a scanner state
  pub tok_prec:       u16,
  /// Precedence of this symbol when present in a parser state
  pub sym_prec:       u16,
}

impl Debug for SymbolRef {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.id.fmt(f)?;
    match (self.sym_prec, self.tok_prec) {
      (0, 0) => {}
      (sym, 0) => {
        f.write_fmt(format_args!("{{ {sym} }}"))?;
      }
      (0, tok) => {
        f.write_fmt(format_args!("{{ , {tok} }}"))?;
      }
      (sym, tok) => {
        f.write_fmt(format_args!("{{ {sym}, {tok} }}"))?;
      }
    }
    Ok(())
  }
}

/// A globally unique identifier for a single non-terminal.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NonTermId {
  pub db_key:        DBNonTermKey,
  pub sym:           SymbolId,
  pub uu_name:       IString,
  pub friendly_name: IString,
  pub is_terminal:   bool,
}

impl Debug for NonTermId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.is_terminal {
      f.write_fmt(format_args!("tok_{}[{:?}]", self.uu_name.to_str().as_str(), self.db_key))
    } else {
      f.write_fmt(format_args!("{}[{:?}]", self.uu_name.to_str().as_str(), self.db_key))
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum NonTermSubType {
  Parser,
  Scanner,
  ScannerToken,
  ScannerSym,
}

/// Used as a lookup key for a symbol data within a
/// [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct DBTermKey(pub u32);

impl Debug for DBTermKey {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("tok[")?;
    self.0.fmt(f)?;
    f.write_str("]")
  }
}

impl DBTermKey {
  pub fn default_sym() -> Self {
    Self(0)
  }

  /// Retrieves the binary / bytecode id of the symbol.
  pub fn to_val(&self) -> u32 {
    self.0
  }

  pub fn to_index(&self) -> usize {
    (self.0) as usize
  }
}

/// An opaque key used for the access of a rule in a [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct DBRuleKey(pub(crate) u32);

impl Debug for DBRuleKey {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("rule[")?;
    self.0.fmt(f)?;
    f.write_str("]")
  }
}

/// Used as a lookup key for non-terminal data stored within a
/// [CompileDatabase]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct DBNonTermKey(pub u32);

impl Debug for DBNonTermKey {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("nt[")?;
    self.0.fmt(f)?;
    f.write_str("]")
  }
}

indexed_id_implementations!(DBNonTermKey);
indexed_id_implementations!(DBRuleKey);
indexed_id_implementations!(DBTermKey);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StateId(pub usize);

impl Debug for StateId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut t = f.debug_tuple("StateId");
    t.field(&self.index());
    t.finish()
  }
}

impl Hash for StateId {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.index().hash(state)
  }
}

impl Default for StateId {
  fn default() -> Self {
    Self(usize::MAX)
  }
}

impl StateId {
  pub fn index(&self) -> usize {
    (self.0 & 0x0F_FF_FF_FF) as usize
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct AnnotatedStateId {
  id:       StateId,
  sub_type: GraphIdSubType,
}

impl Debug for AnnotatedStateId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut t = f.debug_tuple("AnnotatedStateId");
    t.field(&self.id);
    t.field(&self.subtype());
    t.finish()
  }
}

impl AnnotatedStateId {
  pub fn new(index: StateId, sub_type: GraphIdSubType) -> Self {
    Self { id: index, sub_type }
  }

  pub fn to_post_reduce(&self) -> Self {
    Self::new(self.id, GraphIdSubType::PostReduce)
  }

  pub fn to_goto(&self) -> Self {
    Self::new(self.id, GraphIdSubType::Goto)
  }

  pub fn subtype(&self) -> GraphIdSubType {
    self.sub_type
  }

  /// Indicates the state was generated by transitioning from a goal state to
  /// items that follow from non-terms of goal items.
  pub fn is_oos(&self) -> bool {
    match self.subtype() {
      GraphIdSubType::ExtendSled | GraphIdSubType::ExtendedClosure => true,
      _ => false,
    }
  }

  pub fn is_root(&self) -> bool {
    self.subtype() == GraphIdSubType::Root
  }

  pub fn root() -> Self {
    Self { id: StateId(0), sub_type: GraphIdSubType::Root }
  }
}

impl From<AnnotatedStateId> for StateId {
  fn from(value: AnnotatedStateId) -> Self {
    value.id
  }
}

// STATE ID -------------------------------------------------------------
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum GraphIdSubType {
  Root    = 0,
  Regular,
  Goto,
  PostReduce,
  ExtendedClosure,
  ExtendSled,
  #[default]
  Invalid = 0xF,
}

impl GraphIdSubType {
  /// A single use subtype to represent the transition from a an
  /// in-scope state to an out-of-scope state, item, or closure.
  pub fn is_oos_entry(&self) -> bool {
    *self == GraphIdSubType::ExtendSled
  }

  pub fn is_oos_closure(&self) -> bool {
    *self == GraphIdSubType::ExtendedClosure
  }

  pub fn extended_entry_base() -> Self {
    GraphIdSubType::ExtendSled
  }

  pub fn root() -> Self {
    GraphIdSubType::Root
  }

  pub fn is_invalid(&self) -> bool {
    *self == GraphIdSubType::Invalid
  }

  pub fn is_root(&self) -> bool {
    *self == GraphIdSubType::Root
  }

  pub fn is_rootish(&self) -> bool {
    *self == GraphIdSubType::Root
  }

  pub fn is_post_reduce(&self) -> bool {
    *self == GraphIdSubType::PostReduce
  }

  pub fn is_goto(&self) -> bool {
    *self == GraphIdSubType::Goto
  }
}
