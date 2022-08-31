#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

pub struct StringId(pub u64);

impl From<&String> for StringId
{
  fn from(string: &String) -> Self
  {
    StringId(hash_id_value_u64(string))
  }
}

impl Display for StringId
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
  {
    f.write_str(&self.0.to_string())
  }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash, Eq, Ord)]

pub enum SymbolID
{
  /// Represents a defined sequence of characters from the set `0-9`.
  DefinedNumeric(StringId),

  /// Represents a defined sequence of characters that are members of the
  /// unicode character classes `ID_Start` and `ID_Nonstart`.
  DefinedIdentifier(StringId),

  /// Represents any defined token sequence that contains a mixture of
  /// identifier, numeric, and general characters. Examples include
  /// `ba$$`, `#123`, `$10.20`, and `h3ll0W0rld!`.
  DefinedSymbol(StringId),

  /// Represents any non-terminal production symbol.
  Production(ProductionId, GrammarId),

  /// Represents any terminal production symbol defined through
  /// the production token specifier, `tk:`, as in `tk:<production_name>`.
  TokenProduction(ProductionId, GrammarId),

  /// Represent the grammar symbol `g:sp`.
  GenericSpace,

  /// Represent the grammar symbol `g:tab`.
  GenericHorizontalTab,

  /// Represent the grammar symbol `g:nl`.
  GenericNewLine,

  /// Represent the grammar symbol `g:id`.
  GenericIdentifier,

  /// Represent the grammar symbol `g:num`.
  GenericNumber,

  /// Represent the grammar symbol `g:sym`.
  GenericSymbol,

  /// Represent the grammar symbol `g:rec`.
  Recovery,
  Default,
  EndOfFile,
  Undefined,
}

impl Default for SymbolID
{
  fn default() -> Self
  {
    SymbolID::Undefined
  }
}

impl SymbolID
{
  pub const DefinedSymbolIndexBasis: u32 = 8;
  pub const Generics: [SymbolID; 6] = [
    SymbolID::GenericSpace,
    SymbolID::GenericHorizontalTab,
    SymbolID::GenericNewLine,
    SymbolID::GenericIdentifier,
    SymbolID::GenericNumber,
    SymbolID::GenericSymbol,
  ];

  pub fn to_string(&self, grammar: &GrammarStore) -> String
  {
    match self {
      Self::DefinedNumeric(_) | Self::DefinedIdentifier(_) | Self::DefinedSymbol(_) => {
        format!("\\{}", grammar.symbols_string_table.get(self).unwrap())
      }
      Self::Production(prod_id, _) => {
        grammar.production_table.get(prod_id).unwrap().guid_name.to_string()
      }
      Self::TokenProduction(prod_id, _) => {
        format!("tk:{}", grammar.production_table.get(prod_id).unwrap().guid_name)
      }
      Self::Default => "default".to_string(),
      Self::Undefined => "[??]".to_string(),
      Self::Recovery => "g:rec".to_string(),
      Self::EndOfFile => "$eof".to_string(),
      Self::GenericHorizontalTab => "g:tab".to_string(),
      Self::GenericNewLine => "g:nl".to_string(),
      Self::GenericSpace => "g:sp".to_string(),
      Self::GenericIdentifier => "g:id".to_string(),
      Self::GenericNumber => "g:num".to_string(),
      Self::GenericSymbol => "g:sym".to_string(),
    }
  }

  pub fn to_default_string(&self) -> String
  {
    match self {
      Self::DefinedNumeric(_) | Self::DefinedIdentifier(_) | Self::DefinedSymbol(_) => {
        "__defined".to_string()
      }
      Self::Production(prod_id, _) => "__defined".to_string(),
      Self::TokenProduction(prod_id, _) => "__defined".to_string(),
      Self::Default => "__default".to_string(),
      Self::Undefined => "__undefined".to_string(),
      Self::Recovery => "__rec".to_string(),
      Self::EndOfFile => "__eof".to_string(),
      Self::GenericHorizontalTab => "__tab".to_string(),
      Self::GenericNewLine => "__nl".to_string(),
      Self::GenericSpace => "__sp".to_string(),
      Self::GenericIdentifier => "__id".to_string(),
      Self::GenericNumber => "__num".to_string(),
      Self::GenericSymbol => "__sym".to_string(),
    }
  }

  pub fn isDefinedSymbol(&self) -> bool
  {
    match self {
      Self::DefinedNumeric(_) | Self::DefinedIdentifier(_) | Self::DefinedSymbol(_) => {
        true
      }
      _ => false,
    }
  }

  pub fn getProductionId(&self) -> Option<ProductionId>
  {
    match self {
      Self::Production(id, _) => Some(*id),
      Self::TokenProduction(id, _) => Some(*id),
      _ => None,
    }
  }

  pub fn getGrammarId(&self) -> Option<GrammarId>
  {
    match self {
      Self::Production(_, id) => Some(*id),
      Self::TokenProduction(_, id) => Some(*id),
      _ => None,
    }
  }

  pub fn bytecode_id(&self, grammar: &GrammarStore) -> u32
  {
    match self {
      Self::DefinedNumeric(_)
      | Self::DefinedIdentifier(_)
      | Self::DefinedSymbol(_)
      | Self::TokenProduction(..) => grammar.symbols_table.get(self).unwrap().bytecode_id,
      Self::Default => 9999,
      Self::Production(..) => 0,
      Self::Undefined => 0,
      Self::Recovery => 0,
      Self::EndOfFile => 1,
      Self::GenericHorizontalTab => HORIZONTAL_TAB as u32,
      Self::GenericNewLine => NEW_LINE as u32,
      Self::GenericSpace => SPACE as u32,
      Self::GenericIdentifier => IDENTIFIER as u32,
      Self::GenericNumber => NUMBER as u32,
      Self::GenericSymbol => SYMBOL as u32,
    }
  }
}

pub type SymbolUUID = SymbolID;

#[repr(C, align(64))]
#[derive(Debug, Clone)]

pub struct Symbol
{
  /// The globally unique identifier of this symbol
  /// which encapsulates the set of Symbols that are
  /// unique based on the combination of the symbol's
  /// class_id,
  pub guid: SymbolUUID,
  /// The unique identifier of the class of this symbol
  /// which either identifies symbol's generic class id
  /// i.e (g:sp , g:nl, g:tab, g:id ...) or by the unique
  /// or the explicit character sequence this symbol represents.
  pub bytecode_id: u32,
  /// The length in bytes of the character sequence
  /// represented by this symbol
  pub byte_length: u32,
  /// The number of utf8 code points represented by
  /// this symbol.
  pub code_point_length: u32,
  ////
  /// True if only scanner productions use
  /// this symbol
  pub scanner_only: bool,
  /// A name that can be used in debug and
  /// error reports .
  pub friendly_name: String,
}

impl Symbol
{
  pub const Generics: [&'static Symbol; 6] = [
    &Symbol {
      guid: SymbolID::GenericSpace,
      bytecode_id: SPACE as u32,
      byte_length: 1,
      code_point_length: 1,
      scanner_only: false,
      friendly_name: String::new(),
    },
    &Symbol {
      guid: SymbolID::GenericHorizontalTab,
      bytecode_id: HORIZONTAL_TAB as u32,
      byte_length: 1,
      code_point_length: 1,
      scanner_only: false,
      friendly_name: String::new(),
    },
    &Symbol {
      guid: SymbolID::GenericNewLine,
      bytecode_id: NEW_LINE as u32,
      byte_length: 1,
      code_point_length: 1,
      scanner_only: false,
      friendly_name: String::new(),
    },
    &Symbol {
      guid: SymbolID::GenericIdentifier,
      bytecode_id: IDENTIFIER as u32,
      byte_length: 0,
      code_point_length: 0,
      scanner_only: false,
      friendly_name: String::new(),
    },
    &Symbol {
      guid: SymbolID::GenericNumber,
      bytecode_id: NUMBER as u32,
      byte_length: 0,
      code_point_length: 0,
      scanner_only: false,
      friendly_name: String::new(),
    },
    &Symbol {
      guid: SymbolID::GenericSymbol,
      bytecode_id: SYMBOL as u32,
      byte_length: 0,
      code_point_length: 0,
      scanner_only: false,
      friendly_name: String::new(),
    },
  ];

  pub fn generics_lu() -> BTreeMap<SymbolID, &'static Symbol>
  {
    BTreeMap::from_iter(Self::Generics.clone().iter().map(|s| (s.guid, *s)))
  }
}

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Display;
use std::str::FromStr;

use crate::grammar::uuid::hash_id_value_u64;
use crate::utf8::lookup_table::HORIZONTAL_TAB;
use crate::utf8::lookup_table::IDENTIFIER;
use crate::utf8::lookup_table::NEW_LINE;
use crate::utf8::lookup_table::NUMBER;
use crate::utf8::lookup_table::SPACE;
use crate::utf8::lookup_table::SYMBOL;

use super::GrammarId;
use super::GrammarStore;
use super::ProductionId;

/// A table that maps a symbol class_id to a utf8 string.

pub type SymbolStringTable = BTreeMap<SymbolID, String>;

/// A table that maps a symbol uuid to a production id

pub type ProductionSymbolsTable = BTreeMap<u64, (u32, u32)>;

/// A table that contains defined symbols () keyed by their [SymbolUUID].
pub type SymbolsTable = BTreeMap<SymbolUUID, Symbol>;

/// A table that contains symbols defined by their class_id
pub type ExportSymbolsTable = BTreeMap<SymbolID, Symbol>;
