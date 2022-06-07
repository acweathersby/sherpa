use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    path::PathBuf,
};

use super::{
    BodyTable, Item, ProductionBodiesTable, ProductionTable, SymbolID, SymbolStringTable,
    SymbolsTable,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct GrammarId(pub u64);

impl Display for GrammarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

///
/// Stores absolute paths and source code of a hcg file.
#[derive(Debug, Clone)]
pub struct HCGSource {
    ///
    /// The absolute path of a hcg file
    pub absolute_path: PathBuf,
    pub source: String,
}

///
/// Holds essential data tables and other information
/// needed to compile a parser and  other derivatives from
/// HCG grammar files. Such data includes:
/// - Production information
/// - Production body information
/// - Symbol information
/// - Source code and origin paths
#[derive(Debug, Clone)]
pub struct GrammarStore {
    ///
    /// The absolute path of original source file.
    pub source_path: PathBuf,
    ///
    /// A globally unique name to refer to this grammar by. Derived from
    /// the source_path.
    pub uuid_name: String,
    ///
    /// Hash of the uuid_name.
    pub uuid: GrammarId,
    ///
    /// Maps ProductionID to a list of BodyIds
    pub production_bodies_table: ProductionBodiesTable,
    ///
    /// Maps `super::ProductionId` to production data.
    pub production_table: ProductionTable,
    ///
    /// Maps BodyId to body data.
    pub bodies_table: BodyTable,
    ///
    /// Maps SymbolId to symbol data. Only stores
    /// Defined and TokenProduction symbols.
    pub symbols_table: SymbolsTable,
    ///
    /// Maps SymbolId to it's original source token string.
    pub symbols_string_table: SymbolStringTable,
    ///
    /// Store of all production ids encountered in grammar.
    pub production_symbols_table: BTreeSet<SymbolID>,
    ///
    /// Maps a local import name to an absolute file path and its UUID.
    pub imports: HashMap<String, (String, PathBuf)>,
    ///
    /// Closure of all items that can be produced by this grammar.
    pub closures: HashMap<Item, Vec<Item>>,
}
