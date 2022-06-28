use std::fmt::Display;

use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::ASTNodeTraits;
use crate::grammar::hash_id_value_u64;

use super::SymbolID;
use super::Token;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

pub struct ProductionId(pub u64);

impl From<&String> for ProductionId
{
    fn from(string: &String) -> Self
    {
        ProductionId(hash_id_value_u64(string))
    }
}

impl Display for ProductionId
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

pub struct BodyId(pub u64);

impl BodyId
{
    pub fn new(prod_id: &ProductionId, body_index: usize) -> Self
    {
        BodyId((prod_id.0 & 0xFFFF_FFFF_FFFF_F000) + body_index as u64)
    }

    #[inline(always)]
    pub fn default() -> Self
    {
        Self(0)
    }

    #[inline(always)]
    pub fn is_null(&self) -> bool
    {
        self.0 == 0
    }
}

impl Display for BodyId
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, Clone)]

pub struct Production
{
    pub name: String,
    pub number_of_bodies: u16,
    pub id: ProductionId,
    pub is_scanner: bool,
    pub is_entry: bool,
    pub is_recursive: bool,
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

impl Production
{
    pub fn new(
        name: String,
        id: ProductionId,
        number_of_bodies: u16,
        token: Token,
        is_scanner: bool,
    ) -> Self
    {
        Production {
            name,
            id,
            is_entry: false,
            is_recursive: false,
            is_scanner,
            number_of_bodies,
            priority: 0,
            original_location: token,
            bytecode_id: 0,
            symbol_bytecode_id: 0,
        }
    }
}

#[derive(Debug, Clone)]

pub struct BodySymbolRef
{
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ReduceFunctionId(u64);

impl ReduceFunctionId
{
    pub fn new(reduce_function: &ASTNode) -> Self
    {
        ReduceFunctionId(hash_id_value_u64(reduce_function.Token().String()))
    }

    pub fn is_undefined(&self) -> bool
    {
        self.0 == 0
    }
}

/// A single body derived from a production
#[derive(Debug, Clone)]
pub struct Body
{
    pub symbols: Vec<BodySymbolRef>,
    pub length: u16,
    pub production: ProductionId,
    pub id: BodyId,
    pub bytecode_id: u32,
    pub reduce_fn_ids: Vec<ReduceFunctionId>,
    pub origin_location: Token,
}

pub type ProductionTable = std::collections::BTreeMap<ProductionId, Production>;

pub type ProductionEntryNamesTable =
    std::collections::BTreeMap<String, ProductionId>;

pub type ProductionBodiesTable =
    std::collections::BTreeMap<ProductionId, Vec<BodyId>>;

pub type BodyTable = std::collections::BTreeMap<BodyId, Body>;
