use super::{BodyTable, ProductionBodiesTable, ProductionTable, SymbolStringTable, SymbolsTable};

///
/// Stores absolute paths and source code of a hcg file.
pub struct HCGSource {
    ///
    /// The absolute path of a hcg file
    pub absolute_path: String,
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
pub struct GrammarStore {
    ///
    /// Stores absolute paths and source code of grammar files. The entry
    /// grammar file is always placed at index 0.
    pub sources: Vec<HCGSource>,
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
    /// Maps SymbolId to symbol data.
    pub symbols_table: SymbolsTable,
    ///
    /// Maps SymbolId to it's original source token string.
    pub symbols_string_table: SymbolStringTable,
}
