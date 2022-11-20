use bitmask_enum::bitmask;

use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::ASTNodeTraits;
use crate::grammar::hash_id_value_u64;
use crate::types::SymbolID;
use crate::types::Token;

#[bitmask]
pub enum RecursionType {
  NONE  = 0,
  LEFT_DIRECT = 1,
  LEFT_INDIRECT = 2,
  RIGHT = 4,
}

/// A convenient wrapper around information used to construct parser entry points
/// based on [productions](Production).
pub struct ExportedProduction<'a> {
  /// The name assigned to the production within the
  /// export clause of a grammar.
  /// e.g. `@EXPORT production as <export_name>`
  pub export_name: &'a str,
  /// The GUID name assigned of the corresponding production.
  pub guid_name:   &'a str,
  /// The exported production.
  pub production:  &'a Production,
}

impl Default for RecursionType {
  fn default() -> Self {
    RecursionType::NONE
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct ProductionId(pub u64);

impl From<&String> for ProductionId {
  fn from(string: &String) -> Self {
    ProductionId(hash_id_value_u64(string))
  }
}

impl std::fmt::Display for ProductionId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self.0.to_string())
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

pub struct BodyId(pub u64);

impl BodyId {
  pub fn new(prod_id: &ProductionId, body_index: usize) -> Self {
    BodyId((prod_id.0 & 0xFFFF_FFFF_FFFF_F000) + body_index as u64)
  }

  pub fn from_syms(syms: &[SymbolID]) -> Self {
    let val = hash_id_value_u64(syms);
    BodyId(val)
  }

  #[inline(always)]
  pub fn default() -> Self {
    Self(0)
  }

  #[inline(always)]
  pub fn is_null(&self) -> bool {
    self.0 == 0
  }
}

impl std::fmt::Display for BodyId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self.0.to_string())
  }
}

#[derive(Debug, Clone, Default)]
pub struct Production {
  pub guid_name: String,
  pub original_name: String,
  pub number_of_bodies: u16,
  pub id: ProductionId,
  pub is_scanner: bool,
  pub is_entry: bool,
  pub recursion_type: RecursionType,
  pub priority: u32,
  /// The token defining the substring in the source
  /// code from which this production was derived.
  pub original_location: Token,
  /// An integer value used by bytecode
  /// to refer to this production
  pub bytecode_id: u32,
  /// If this is a scanner production,
  /// then this is a non-zero integer value
  /// that mirrors the TokenProduction or Defined* symbol
  /// bytecode_id that this production produces.
  pub symbol_bytecode_id: u32,
}

#[derive(Debug, Clone, Default)]
pub struct BodySymbolRef {
  pub sym_id:         SymbolID,
  pub original_index: u32,
  pub annotation:     String,
  pub consumable:     bool,
  /// The number of related symbols that comprise
  /// a scanned token. For use by scanner code.
  /// If this symbol does not exist in scanner space then it is
  /// set to 0
  pub scanner_length: u32,
  /// The zero-based sequence index of this symbol in relation
  /// to other related symbols that comprise a scanned token.
  /// If this symbol does not exist in scanner space then it is
  /// set to 0
  pub scanner_index:  u32,
  /// Always captures, regardless of other symbols
  pub exclusive:      bool,

  pub tok: Token,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ReduceFunctionId(u64);

impl ReduceFunctionId {
  pub fn new(reduce_function: &ASTNode) -> Self {
    ReduceFunctionId(hash_id_value_u64(reduce_function.Token().to_string()))
  }

  pub fn is_undefined(&self) -> bool {
    self.0 == 0
  }
}

/// A single body derived from a production
#[derive(Debug, Clone, Default)]
pub struct Body {
  pub syms: Vec<BodySymbolRef>,
  pub len: u16,
  pub prod: ProductionId,
  pub id: BodyId,
  pub bc_id: u32,
  pub reduce_fn_ids: Vec<ReduceFunctionId>,
  pub origin_location: Token,
}

/// Maps a [ProductionId] to a [Production].
pub type ProductionTable = std::collections::BTreeMap<ProductionId, Production>;

pub type ProductionEntryNamesTable = std::collections::BTreeMap<String, ProductionId>;

/// Maps [ProductionId] to a vector of [BodyIds](BodyId).
pub type ProductionBodiesTable = std::collections::BTreeMap<ProductionId, Vec<BodyId>>;

pub type BodyTable = std::collections::BTreeMap<BodyId, Body>;
