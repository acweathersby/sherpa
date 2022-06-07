use std::fmt::Display;

use crate::grammar::hash_id_value;

use super::{SymbolID, Token};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct ProductionId(pub u64);

impl From<&String> for ProductionId {
    fn from(string: &String) -> Self {
        ProductionId(hash_id_value(string))
    }
}

impl Display for ProductionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct BodyId(pub u64);

impl BodyId {
    pub fn new(prod_id: &ProductionId, body_index: usize) -> Self {
        BodyId((prod_id.0 & 0xFFFFFFFF_FFFFF000) + body_index as u64)
    }
}

impl Display for BodyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct Production {
    pub name: String,
    pub number_of_bodies: u16,
    pub id: ProductionId,
    pub is_scanner: bool,
    pub is_entry: bool,
    pub is_recursive: bool,
    pub priority: u32,
    pub token: Token,
}

#[derive(Debug, Clone)]
pub struct BodySymbolRef {
    pub sym_id: SymbolID,
    pub original_index: u32,
    pub annotation: String,
    pub consumable: bool,
    ///
    /// The number of related symbols that comprise
    /// a scanned token. For use by scanner code.
    /// If this symbol does not exist in scanner space then it is
    /// set to 0
    pub scanner_length: u32,
    ///
    /// The zero-based sequence index of this symbol in relation
    /// to other related symbols that comprise a scanned token.
    /// If this symbol does not exist in scanner space then it is
    /// set to 0
    pub scanner_index: u32,
    ///
    /// Always captures, regardless of other symbols
    pub exclusive: bool,
}

#[derive(Debug, Clone)]
pub struct Body {
    pub symbols: Vec<BodySymbolRef>,
    pub length: u16,
    pub production: ProductionId,
    pub id: BodyId,
}

pub type ProductionTable = std::collections::BTreeMap<ProductionId, Production>;
pub type ProductionEntryNamesTable = std::collections::BTreeMap<String, ProductionId>;
pub type ProductionBodiesTable = std::collections::BTreeMap<ProductionId, Vec<BodyId>>;
pub type BodyTable = std::collections::BTreeMap<BodyId, Body>;
