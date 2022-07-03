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

use super::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

/// A Globally Unique Id to quickly distinguish instances of [GrammarStore]. This
/// value is derived from the filepath of the grammar's source code.
pub struct GrammarId(pub u64);

impl Display for GrammarId
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_str(&self.0.to_string())
    }
}

/// Stores the absolute paths and source code of a `*.hcg` file.
#[derive(Debug, Clone)]
pub struct HCGSource
{
    /// The absolute path of a hcg file.
    pub absolute_path: PathBuf,
    /// The source code of a hcg file.
    pub source:        String,
}
#[derive(Debug, Clone)]
pub enum ReduceFunctionType
{
    Generic(Reduce),
    Ascript(Ascript),
    Undefined,
}

impl ReduceFunctionType
{
    pub fn new(node: &ASTNode) -> Self
    {
        match node {
            ASTNode::Reduce(box reduce) => {
                ReduceFunctionType::Generic(reduce.clone())
            }
            ASTNode::Ascript(box ascript) => {
                ReduceFunctionType::Ascript(ascript.clone())
            }
            _ => ReduceFunctionType::Undefined,
        }
    }
}

pub type ReduceFunctionTable = BTreeMap<ReduceFunctionId, ReduceFunctionType>;

/// Houses data essential to the compilation and analysis of Hydrocarbon
/// grammar source code.
///
/// # Instantiation
///
/// Use one of the following functions to construct a GrammarStore:
/// - [compile_from_path](crate::grammar::compile_from_path)
///     
///     # Examples
///     ```
///     use hctk::grammar::compile_from_path;
///
///     let number_of_threads = 1;
///     let (grammar_store_option, errors) = compile_from_path(
///         PathBuf::from_str("./my_grammar.hcg"),
///         number_of_threads
///     );
///     ```
/// - [compile_from_string](crate::grammar::compile_from_string)
///     
///     # Examples
///     ```
///     use hctk::grammar::compile_from_source;
///
///     let source = "<> start > \\hello \\world";
///     let faux_source_path = "/home/user/grammars/my_grammar.hcg";
///
///     let (grammar_store_option, errors) = compile_from_source(
///         source,
///         faux_source_path
///     );
///     ```
#[derive(Debug, Clone, Default)]
pub struct GrammarStore
{
    /// The absolute path of the grammar's source file. If the source code was passed
    /// in as string (as in the case of [compile_from_string](crate::grammar::compile_from_string))
    /// then this may be empty.
    pub source_path: PathBuf,

    /// A globally unique name to refer to this grammar by. Derived from the
    /// grammar's filepath.
    pub guid_name: String,

    /// A globally unique identifier for this GrammarStore instance. Derived
    /// from the source path
    pub guid: GrammarId,

    /// Maps [ProductionId] to a list of [BodyIds](BodyId)
    pub production_bodies_table: ProductionBodiesTable,

    /// Maps a [ProductionId] to a [Production].
    pub production_table: ProductionTable,

    /// Maps BodyId to body data.
    pub bodies_table: BodyTable,

    /// Maps [SymbolId] to [Symbol] data. Only stores [Symbols](Symbol) that
    /// represent one of the following:
    /// - [SymbolID::DefinedNumeric]
    /// - [SymbolID::DefinedIdentifier]
    /// - [SymbolID::DefinedSymbol]
    /// - [SymbolID::TokenProduction]
    pub symbols_table: SymbolsTable,

    /// Maps SymbolId to its original source token string.
    pub symbols_string_table: SymbolStringTable,

    /// Store of all production ids encountered in grammar.
    pub production_symbols_table: BTreeMap<SymbolID, Token>,

    /// Maps a local import name to an absolute file path and its
    /// UUID.
    pub imports: HashMap<String, (String, PathBuf)>,

    /// Closure of all items that can be produced by this grammar.
    pub closures: HashMap<Item, Vec<Item>>,

    /// Closure of all items that can be produced by this grammar.
    pub item_peek_symbols: HashMap<Item, Vec<SymbolID>>,

    /// Closure of all items that can be produced by this grammar.
    pub production_peek_symbols: HashMap<ProductionId, Vec<SymbolID>>,

    /// A mapping of [ProductionId]s to export names
    ///
    /// These export names are generated from the grammar production:
    ///
    /// ```hgc
    /// <> export_preamble > \@EXPORT sym::production_symbol ( t:AS | t:as ) tk:export_id
    /// ```
    /// where `tk:export_id` is assigned to the second tuple position.
    ///
    /// If no export names are declared in the root grammar, then this will contain
    /// the id of the first production declared in the root grammar, assigned to the
    /// name `default`.
    pub export_names: Vec<(ProductionId, String)>,

    /// All items in the grammar that are `B => . A b` for some production `A`.
    pub lr_items: BTreeMap<ProductionId, HashSet<Item>>,

    /// All reduce functions defined in the grammar.
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
    pub local_guid: &'a String,
    pub absolute_path: &'a PathBuf,
    pub import_names_lookup: &'a mut ImportProductionNameTable,
    pub symbols_table: &'a mut SymbolsTable,
    pub symbols_string_table: &'a mut SymbolStringTable,
    pub bodies_table: &'a mut BodyTable,
    pub production_table: &'a mut ProductionTable,
    pub production_symbols_table: &'a mut BTreeMap<SymbolID, Token>,
    pub production_bodies_table: &'a mut ProductionBodiesTable,
    pub errors: &'a mut Vec<ParseError>,
    pub reduce_functions: &'a mut ReduceFunctionTable,
}
