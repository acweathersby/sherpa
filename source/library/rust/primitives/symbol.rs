pub enum SymbolID {
    DefinedNumeric(u32),
    DefinedIdentifier(u32),
    DefinedGeneric(u32),
    Production(u32),
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
    pub const DefinedSymbolIndexBasis: u32 = 11;

    pub fn as_key(&self) -> u32 {
        match self {
            Self::DefinedNumeric(val) => *val,
            Self::DefinedIdentifier(val) => *val,
            Self::DefinedGeneric(val) => *val,
            Self::Production(val) => *val,
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

    pub fn production_id(&self) -> Option<u32> {
        match *self {
            Self::Production(val) => Some(val),
            _ => None,
        }
    }
}

pub type SymbolUUID = SymbolID;

#[repr(C, align(64))]
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
    pub class_id: SymbolID,
    ///
    /// The length in bytes of the character sequence
    /// represented by this symbol
    pub byte_length: u32,
    ///
    /// The number of utf8 code points represented by
    /// this symbol.
    pub code_point_length: u32,
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
}

use std::collections::BTreeMap;

///
/// A table that maps a symbol class_id to a utf8 string.
pub type SymbolStringTable = BTreeMap<u32, (u64, String)>;

///
/// A table that maps a symbol uuid to a production id
pub type ProductionSymbolsTable = BTreeMap<u64, (u32, u32)>;

///
/// A table that contains all symbols keyed by their uuid
pub type SymbolsTable = BTreeMap<u32, (u64, Symbol)>;

///
/// A table that contains symbols defined by their class_id
pub type ExportSymbolsTable = BTreeMap<u32, (u64, Symbol)>;
