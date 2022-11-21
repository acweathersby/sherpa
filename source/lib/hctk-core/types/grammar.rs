use super::*;
use crate::compile_grammar_from_path;
use crate::compile_grammar_from_string;
use crate::grammar::create_closure;
use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::Ascript;
use crate::grammar::data::ast::Reduce;
use crate::grammar::get_closure_cached;
use crate::grammar::get_guid_grammar_name;
use crate::grammar::get_production_start_items;
use crate::grammar::hash_id_value_u64;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]

/// A Globally Unique Id to quickly distinguish instances of [GrammarStore]. This
/// value is derived from the filepath of the grammar's source code.
pub struct GrammarId(pub u64);

impl Display for GrammarId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self.0.to_string())
  }
}

/// Stores the absolute paths and source code of a `*.hcg` file.
#[derive(Debug, Clone)]
pub struct HCGSource {
  /// The absolute path of a hcg file.
  pub absolute_path: PathBuf,
  /// The source code of a hcg file.
  pub source:        String,
}
#[derive(Debug, Clone)]
pub enum ReduceFunctionType {
  Generic(Reduce),
  Ascript(Ascript),
  Undefined,
}

impl ReduceFunctionType {
  pub fn new(node: &ASTNode) -> Self {
    match node {
      ASTNode::Reduce(box reduce) => ReduceFunctionType::Generic(reduce.clone()),
      ASTNode::Ascript(box ascript) => ReduceFunctionType::Ascript(ascript.clone()),
      _ => ReduceFunctionType::Undefined,
    }
  }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct GrammarRef {
  /// A globally unique name to refer to this grammar by. Derived from the
  /// grammar's filepath.
  pub guid_name: String,
  /// The user defined name. This is either the value of the `@NAME` preamble,
  /// or the original file name stem if this preamble is not present.
  pub name:      String,

  /// A globally unique identifier for this GrammarStore instance. Derived
  /// from the source path
  pub guid: GrammarId,

  /// The absolute path of the grammar's source file. This may be empty if the source code was passed
  /// in as a string, as with the case of grammars compiled with
  /// [compile_grammar_from_string](hctk_core::grammar::compile_grammar_from_string)).
  pub path: PathBuf,
}

impl GrammarRef {
  pub fn new(local_name: String, absolute_path: PathBuf) -> Arc<Self> {
    let guid_name = get_guid_grammar_name(&absolute_path).unwrap();
    Arc::new(GrammarRef {
      guid: GrammarId(hash_id_value_u64(&guid_name)),
      guid_name,
      name: local_name,
      path: absolute_path,
    })
  }
}

pub type ImportedGrammarReferences = HashMap<String, Arc<GrammarRef>>;

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
///     use hctk::compile_grammar_from_path;
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
pub struct GrammarStore {
  pub id: Arc<GrammarRef>,

  /// Maps [ProductionId] to a list of [BodyIds](BodyId)
  pub production_bodies: ProductionBodiesTable,

  /// Maps a [ProductionId] to a [Production].
  pub productions: ProductionTable,

  /// Maps a production's id to it's original name and guid name
  pub production_names: BTreeMap<ProductionId, (String, String)>,

  /// Maps BodyId to body data.
  pub bodies: BodyTable,

  /// Maps [SymbolId] to [Symbol] data. Only stores [Symbols](Symbol) that
  /// represent one of the following:
  /// - [SymbolID::DefinedNumeric]
  /// - [SymbolID::DefinedIdentifier]
  /// - [SymbolID::DefinedSymbol]
  /// - [SymbolID::TokenProduction]
  pub symbols: SymbolsTable,

  /// Maps SymbolId to its original source token string.
  pub symbol_strings: SymbolStringTable,

  /// Store of all production ids encountered in grammar.
  pub production_symbols: BTreeMap<SymbolID, Token>,

  /// Maps a local import name to an absolute file path and its
  /// UUID.
  pub imports: ImportedGrammarReferences,

  /// Closure of all items that can be produced by this grammar.
  pub closures: HashMap<Item, Vec<Item>>,

  pub item_ignore_symbols: HashMap<Item, Vec<SymbolID>>,

  pub production_ignore_symbols: HashMap<ProductionId, Vec<SymbolID>>,

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
  pub exports: Vec<(ProductionId, String)>,

  /// All items in the grammar that are `B => . A b` for some production `A`.
  pub lr_items: BTreeMap<ProductionId, Vec<Item>>,

  /// All reduce functions defined in the grammar.
  pub reduce_functions: ReduceFunctionTable,

  pub merge_productions: BTreeMap<ProductionId, Vec<Body>>,
}
impl GrammarStore {
  pub fn from_path(path: PathBuf) -> HCResult<Arc<GrammarStore>> {
    match compile_grammar_from_path(path, 0) {
      (Some(grammar), _) => HCResult::Ok(grammar),
      (_, Some(errors)) => {
        HCResult::Err(HCError::Many { message: "Unable to compile Grammar".to_string(), errors })
      }
      (None, None) => unreachable!("compile_grammar_from_string should never return (None, None)"),
    }
  }

  pub fn from_str(string: &str) -> HCResult<Arc<GrammarStore>> {
    match compile_grammar_from_string(string, &PathBuf::from("/internal/")) {
      (Some(grammar), _) => HCResult::Ok(grammar),
      (_, Some(errors)) => {
        HCResult::Err(HCError::Many { message: "Unable to compile Grammar".to_string(), errors })
      }
      (None, None) => unreachable!("compile_grammar_from_string should never return (None, None)"),
    }
  }

  pub fn from_string(string: String) -> HCResult<Arc<GrammarStore>> {
    return Self::from_str(string.as_str());
  }

  /// Returns the [Body] that's mapped to [`body_id`](BodyId)
  /// within the grammar
  pub fn get_body(&self, body_id: &BodyId) -> HCResult<&Body> {
    HCResult::Ok(self.bodies.get(body_id)?)
  }

  /// Returns the [Production] that's mapped to [`production_id`](ProductionId)
  /// within the grammar
  pub fn get_production(&self, production_id: &ProductionId) -> HCResult<&Production> {
    HCResult::Ok(self.productions.get(production_id)?)
  }

  /// Returns a list of [ExportedProductions](ExportedProduction) extracted from
  /// the grammar.
  #[inline]
  pub fn get_exported_productions(&self) -> Vec<ExportedProduction> {
    self
      .exports
      .iter()
      .map(|(id, name)| {
        let production = self.productions.get(id).unwrap();
        ExportedProduction { export_name: name, guid_name: &production.guid_name, production }
      })
      .collect::<Vec<_>>()
  }

  /// Retrieve the non-import and unmangled name of a [Production](Production).
  pub fn get_production_plain_name(&self, prod_id: &ProductionId) -> &str {
    if let Some(prod) = self.productions.get(prod_id) {
      &prod.name
    } else if let Some((name, _)) = self.production_names.get(prod_id) {
      name
    } else {
      ""
    }
  }

  /// Retrieve the non-import and unmangled name of a [Production](Production).
  pub fn get_production_guid_name(&self, prod_id: &ProductionId) -> &str {
    if let Some(prod) = self.productions.get(prod_id) {
      &prod.guid_name
    } else if let Some((_, name)) = self.production_names.get(prod_id) {
      name
    } else {
      ""
    }
  }

  /// Attempts to retrieve a production from the grammar with the matching name.
  /// If the grammar is an aggregate of multiple grammars which define productions
  /// with the same name, the production that is selected is undetermined.
  pub fn get_production_by_name(&self, name: &str) -> HCResult<&Production> {
    for production_id in self.productions.keys() {
      if name == self.get_production_plain_name(production_id) {
        return HCResult::Ok(self.productions.get(production_id).unwrap());
      }
    }

    HCResult::None
  }

  /// Retrieves first the production_id of the first production
  /// whose plain or guid name matches the query string.
  /// Returns None if no production matches the query.
  pub fn get_production_id_by_name(&self, name: &str) -> Option<ProductionId> {
    for (prod_id, prod) in self.productions.iter() {
      if name == self.get_production_plain_name(prod_id) {
        return Some(prod_id.to_owned());
      }
      if name == prod.guid_name {
        return Some(prod_id.to_owned());
      }
    }

    None
  }

  /// Evaluates whether a production is recursive. Returns
  /// a double of booleans.
  ///
  /// The first boolean value indicates that production is recursive.
  ///
  /// The second boolean value indicates a production has left
  /// recursive, either directly or indirectly.
  pub fn get_production_recursion_type(&self, prod_id: ProductionId) -> RecursionType {
    let mut seen = HashSet::<Item>::new();

    let mut pipeline = VecDeque::from_iter(
      create_closure(&get_production_start_items(&prod_id, self), self).iter().map(|i| (0, *i)),
    );

    let mut recurse_type = RecursionType::NONE;

    while let Some((offset, item)) = pipeline.pop_front() {
      if !item.is_end() {
        let other_prod_id = item.get_production_id_at_sym(self);

        if prod_id == other_prod_id {
          if (offset == 0) {
            if (item.get_prod_id(self) == prod_id) {
              recurse_type |= RecursionType::LEFT_DIRECT;
            } else {
              recurse_type |= RecursionType::LEFT_INDIRECT;
            }
          } else {
            recurse_type |= RecursionType::RIGHT;
          }
        }

        if seen.insert(item) {
          let new_item = item.increment().unwrap();

          pipeline.push_back((offset + 1, new_item));

          if let SymbolID::Production(..) = new_item.get_symbol(self) {
            for item in get_closure_cached(&new_item, self) {
              pipeline.push_back((offset + 1, *item));
            }
          }
        }
      }
    }

    recurse_type
  }

  // pub fn map_symbols<, OutputType>
}

#[cfg(test)]
mod production_utilities_tests {
  use super::GrammarStore as G;
  use super::*;

  #[test]

  fn test_compile_test_grammar() {
    let g = G::from_str(
      "
        <> A > B 
        <> B > C
        <> C > D
        <> D > E
        <> E > B A | t:g",
    )
    .unwrap();

    assert_eq!(g.id.path.as_os_str().to_str().unwrap(), "/-internal-/test");
  }

  #[test]
  fn test_get_default_production() {
    let g = G::from_str(
      "
@EXPORT start as test

<> start > \\hello \\and end

<> end > \\goodby
",
    )
    .unwrap();

    let exported_productions = g.get_exported_productions();
    let first = exported_productions.first().unwrap();

    assert_eq!(first.production.name, "start");

    assert_eq!(first.export_name, "test");
  }

  #[test]

  fn test_get_production_plain_name() {
    let g = G::from_str("<>billofolious_tantimum^a>\\o").unwrap();

    let prod = g.get_production_id_by_name("billofolious_tantimum").unwrap();

    assert_eq!(g.get_production_plain_name(&prod), "billofolious_tantimum");

    assert_ne!(g.get_production(&prod).unwrap().guid_name, "billofolious_tantimum");
  }

  #[test]

  fn test_get_production_by_name() {
    let g = G::from_str(
      "
      <> Apple > \\o
      <> Bad_Cakes > \\b
      ",
    )
    .unwrap();

    assert!(g.get_production_id_by_name("Apple").is_some());

    assert!(g.get_production_id_by_name("Bad_Cakes").is_some());

    assert!(g.get_production_id_by_name("Bandible").is_none());
  }

  #[test]

  fn test_is_production_recursive() {
    let g = G::from_str(
      "
      <> A > B 
      <> B > C
      <> C > D
      <> D > E
      <> E > B A R | \\e
      <> R > R A | R B O | \\r
      <> O > \\o
      ",
    )
    .unwrap();

    let production = g.get_production_id_by_name("A").unwrap();

    assert_eq!(g.get_production_recursion_type(production), RecursionType::RIGHT);

    let production = g.get_production_id_by_name("R").unwrap();

    assert!(g
      .get_production_recursion_type(production)
      .contains(RecursionType::LEFT_DIRECT | RecursionType::RIGHT));

    let production = g.get_production_id_by_name("B").unwrap();

    assert!(g
      .get_production_recursion_type(production)
      .contains(RecursionType::LEFT_INDIRECT | RecursionType::RIGHT));

    let production = g.get_production_id_by_name("C").unwrap();

    assert!(g
      .get_production_recursion_type(production)
      .contains(RecursionType::LEFT_INDIRECT | RecursionType::RIGHT));

    let production = g.get_production_id_by_name("O").unwrap();

    assert_eq!(g.get_production_recursion_type(production), RecursionType::NONE);
  }
}
