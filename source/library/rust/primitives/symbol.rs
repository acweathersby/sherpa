#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct StringId(pub u64);

impl From<&String> for StringId {
    fn from(string: &String) -> Self {
        StringId(hash_id_value(string))
    }
}

impl Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Hash, Eq, Ord)]
pub enum SymbolID {
    DefinedNumeric(StringId),
    DefinedIdentifier(StringId),
    DefinedGeneric(StringId),
    Production(ProductionId, GrammarId),
    /// Stores both the target production hash id
    /// and the target grammar hash id
    ///
    /// Stores both the target production hash id
    TokenProduction(ProductionId, GrammarId),
    GenericSpace,
    GenericHorizontalTab,
    GenericNewLine,
    GenericIdentifier,
    GenericNumber,
    GenericSymbol,
    GenericIdentifiers,
    GenericNumbers,
    GenericSymbols,
    Undefined,
    Recovery,
    EndOfFile,
}

impl SymbolID {
    pub fn to_string(&self, grammar: &GrammarStore) -> String {
        match self {
            Self::DefinedNumeric(_) | Self::DefinedIdentifier(_) | Self::DefinedGeneric(_) => {
                format!("\\{}", grammar.symbols_string_table.get(&self).unwrap())
            }
            Self::Production(prod_id, _) => {
                format!("{}", grammar.production_table.get(&prod_id).unwrap().name)
            }
            Self::TokenProduction(prod_id, _) => {
                format!(
                    "tk:{}",
                    grammar.production_table.get(&prod_id).unwrap().name
                )
            }
            Self::Undefined => "[??]".to_string(),
            Self::Recovery => "g:rec".to_string(),
            Self::EndOfFile => "$eof".to_string(),
            Self::GenericHorizontalTab => "g:tab".to_string(),
            Self::GenericNewLine => "g:nl".to_string(),
            Self::GenericSpace => "g:sp".to_string(),
            Self::GenericIdentifier => "g:id".to_string(),
            Self::GenericNumber => "g:num".to_string(),
            Self::GenericSymbol => "g:sym".to_string(),
            Self::GenericIdentifiers => "g:ids".to_string(),
            Self::GenericNumbers => "g:nums".to_string(),
            Self::GenericSymbols => "g:syms".to_string(),
        }
    }

    pub fn getProductionId(&self) -> Option<ProductionId> {
        match self {
            Self::Production(id, _) => Some(id.clone()),
            Self::TokenProduction(id, _) => Some(id.clone()),
            _ => None,
        }
    }

    pub fn getGrammarId(&self) -> Option<GrammarId> {
        match self {
            Self::Production(_, id) => Some(id.clone()),
            Self::TokenProduction(_, id) => Some(id.clone()),
            _ => None,
        }
    }

    pub const DefinedSymbolIndexBasis: u32 = 11;

    pub fn as_key(&self) -> u32 {
        match self {
            Self::DefinedNumeric(_) | Self::DefinedIdentifier(_) => 100,
            Self::DefinedGeneric(_) => 100,
            Self::Production(_, _) => 100,
            Self::TokenProduction(_, _) => 100,
            Self::Undefined => 0,
            Self::Recovery => 0,
            Self::EndOfFile => 1,
            Self::GenericHorizontalTab => 2,
            Self::GenericNewLine => 3,
            Self::GenericSpace => 4,
            Self::GenericIdentifier => 5,
            Self::GenericNumber => 6,
            Self::GenericSymbol => 7,
            Self::GenericIdentifiers => 8,
            Self::GenericNumbers => 9,
            Self::GenericSymbols => 10,
        }
    }
}

pub type SymbolUUID = SymbolID;

#[repr(C, align(64))]
#[derive(Debug, Clone)]
pub struct Symbol {
    ///
    /// The globally unique identifier of this symbol
    /// which encapsulates the set of Symbols that are
    /// unique based on the combination of the symbol's
    /// class_id,
    pub uuid: SymbolUUID,
    ///
    /// The unique identifier of the class of this symbol
    /// which either identifies symbol's generic class id
    /// i.e (g:sp , g:nl, g:tab, g:id ...) or by the unique
    /// or the explicit character sequence this symbol represents.
    pub index: u32,
    ///
    /// The length in bytes of the character sequence
    /// represented by this symbol
    pub byte_length: u32,
    ///
    /// The number of utf8 code points represented by
    /// this symbol.
    pub code_point_length: u32,
    ////
    /// True if only scanner productions use
    /// this symbol
    pub scanner_only: bool,
}

use std::{collections::BTreeMap, fmt::Display};

use crate::grammar::hash::hash_id_value;

use super::{GrammarId, GrammarStore, ProductionId};

///
/// A table that maps a symbol class_id to a utf8 string.
pub type SymbolStringTable = BTreeMap<SymbolID, String>;

///
/// A table that maps a symbol uuid to a production id
pub type ProductionSymbolsTable = BTreeMap<u64, (u32, u32)>;

///
/// A table that contains all symbols keyed by their uuid
pub type SymbolsTable = BTreeMap<SymbolUUID, Symbol>;

///
/// A table that contains symbols defined by their class_id
pub type ExportSymbolsTable = BTreeMap<SymbolID, Symbol>;
