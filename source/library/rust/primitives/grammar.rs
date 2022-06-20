use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::path::PathBuf;

use crate::ascript;
use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::Ascript;
use crate::grammar::data::ast::Reduce;
use crate::grammar::parse::ParseError;

use super::BodyId;
use super::BodyTable;
use super::Item;
use super::ProductionBodiesTable;
use super::ProductionId;
use super::ProductionTable;
use super::ReduceFunctionId;
use super::SymbolID;
use super::SymbolStringTable;
use super::SymbolsTable;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

pub struct GrammarId(pub u64);

impl Display for GrammarId
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_str(&self.0.to_string())
    }
}

/// Stores absolute paths and source code of a hcg file.
#[derive(Debug, Clone)]

pub struct HCGSource
{
    /// The absolute path of a hcg file
    pub absolute_path: PathBuf,
    pub source:        String,
}
#[derive(Debug, Clone)]
pub enum ReduceFunction
{
    Generic(Reduce),
    Ascript(Ascript),
    Undefined,
}

impl ReduceFunction
{
    pub fn new(node: &ASTNode) -> Self
    {
        match node {
            ASTNode::Reduce(box reduce) => {
                ReduceFunction::Generic(reduce.clone())
            }
            ASTNode::Ascript(box ascript) => {
                ReduceFunction::Ascript(ascript.clone())
            }
            _ => ReduceFunction::Undefined,
        }
    }
}

pub type ReduceFunctionTable = BTreeMap<ReduceFunctionId, ReduceFunction>;

/// Holds essential data tables and other information needed to
/// compile a parser and other derivatives from HCG grammar files.
/// Such data includes:
/// - Production information
/// - Production body information
/// - Symbol information
/// - Source code and origin paths
#[derive(Debug, Clone, Default)]

pub struct GrammarStore
{
    /// The absolute path of original source file.
    pub source_path: PathBuf,
    /// A globally unique name to refer to this grammar by. Derived
    /// from the source_path.
    pub uuid_name: String,
    /// Hash of the uuid_name.
    pub uuid: GrammarId,
    /// Maps ProductionID to a list of BodyIds
    pub production_bodies_table: ProductionBodiesTable,
    /// Maps `super::ProductionId` to production data.
    pub production_table: ProductionTable,
    /// Maps BodyId to body data.
    pub bodies_table: BodyTable,
    /// Maps SymbolId to symbol data. Only stores
    /// Defined and TokenProduction symbols.
    pub symbols_table: SymbolsTable,
    /// Maps SymbolId to its original source token string.
    pub symbols_string_table: SymbolStringTable,
    /// Store of all production ids encountered in grammar.
    pub production_symbols_table: BTreeSet<SymbolID>,
    /// Maps a local import name to an absolute file path and its
    /// UUID.
    pub imports: HashMap<String, (String, PathBuf)>,

    /// Closure of all items that can be produced by this grammar.
    pub closures: HashMap<Item, Vec<Item>>,

    /// Closure of all items that can be produced by this grammar.
    pub item_peek_symbols: HashMap<Item, Vec<SymbolID>>,

    /// Closure of all items that can be produced by this grammar.
    pub production_peek_symbols: HashMap<ProductionId, Vec<SymbolID>>,

    pub lr_items: BTreeMap<ProductionId, HashSet<Item>>,

    pub reduce_functions: ReduceFunctionTable,
}

pub type ImportProductionNameTable = HashMap<String, (String, PathBuf)>;

/// A temporary store of table references that can be passed as one
/// argument to functions that require access to such tables.

pub struct TempGrammarStore<'a>
{
    /// Maps an imported symbol name to a universally unique string
    /// that may be used to resolve imported grammar production
    /// names.
    pub local_uuid: &'a String,
    pub absolute_path: &'a PathBuf,
    pub import_names_lookup: &'a mut ImportProductionNameTable,
    pub symbols_table: &'a mut SymbolsTable,
    pub symbols_string_table: &'a mut SymbolStringTable,
    pub bodies_table: &'a mut BodyTable,
    pub production_table: &'a mut ProductionTable,
    pub production_symbols_table: &'a mut BTreeSet<SymbolID>,
    pub production_bodies_table: &'a mut ProductionBodiesTable,
    pub errors: &'a mut Vec<ParseError>,
    pub reduce_functions: &'a mut ReduceFunctionTable,
}
