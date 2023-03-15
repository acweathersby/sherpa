#![allow(unused)]

use std::{hash::Hash, path::PathBuf, sync::Arc};

use sherpa_runtime::types::{Token, TokenRange};

use crate::{compile::ParseState, grammar::hash_id_value_u64};

use super::{
  parser::State,
  string::{CachedString, IString, StringStore},
};

// Creating type aliases of common collections in the
// event we decide to use alternate implementations.

pub(crate) type Map<K, V> = ::std::collections::HashMap<K, V>;
pub(crate) type Set<K> = ::std::collections::HashSet<K>;
pub(crate) type OrderedMap<K, V> = ::std::collections::BTreeMap<K, V>;
pub(crate) type OrderedSet<K> = std::collections::BTreeSet<K>;
pub(crate) type Array<V> = ::std::vec::Vec<V>;
pub(crate) type Queue<V> = ::std::collections::VecDeque<V>;

/// A globally unique identifier for a single production.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum ProductionId {
  /// Productions directly defined within a grammar.
  Standard(u64),
  /// Productions derived from grammar symbols such as the
  /// group `(...)` symbol.
  Sub(u64, u32),
}
impl From<(GrammarId, &str)> for ProductionId {
  fn from(value: (GrammarId, &str)) -> Self {
    ProductionId::Standard(hash_id_value_u64(value))
  }
}

impl From<(ProductionId, usize)> for ProductionId {
  fn from((prod_id, index): (ProductionId, usize)) -> Self {
    if let ProductionId::Standard(id) = prod_id {
      ProductionId::Sub(id, index as u32)
    } else {
      unreachable!()
    }
  }
}

/// A globally unique identifier for a single grammar file.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct GrammarId(u64);

impl From<&PathBuf> for GrammarId {
  fn from(value: &PathBuf) -> Self {
    GrammarId(hash_id_value_u64(&value))
  }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct RuleId(u64);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct TokenSymbol {
  pub type_: SymbolType,
  pub val:   IString,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct ProductionRef(u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct TokenProductionRef(u32);

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct Rule {
  pub symbols: Array<(SymbolId, usize)>,
  pub ast:     Option<ASTToken>,
}

/// A reference to some Ascript AST data that is either automatically generated
/// depending on the reference type, or is stored on a Production node.
#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum ASTToken {
  /// Represents the ast expression `:ast [ $1 ]`.
  ///
  ///
  /// Automatically generated when when a list production (` A(+) | A(*) `) is
  /// processed.
  ListEntry(TokenRange),
  /// Represents the ast expression `:ast [ $1, $--last-- ]`, where `--last--`
  /// represents the last symbol in a rule.
  ///
  /// Automatically generated when when a list production (` A(+) | A(*) `) is
  /// processed.
  ListIterate(TokenRange),
  /// An AST expression defined within a grammar. `0` Is the production id
  /// in which a copy if the AST expressions is stored. `1` is the index
  /// into the Productions's `asts` array for that stored production.
  Defined(ProductionId, usize),
}

/// A custom parse state defined within a grammar e.g `state_name => ...`
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct CustomState {
  pub id:      ProductionId,
  pub g_id:    GrammarId,
  pub symbols: Map<SymbolId, TokenSymbol>,
  pub state:   Box<State>,
  pub tok:     Token,
}

#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct Production {
  /// The unique identifier of this production.
  pub id: ProductionId,

  /// The unique identifier of the owning GrammarHEader.
  pub g_id: GrammarId,

  /// All symbols that are referenced by the rules of this
  /// production.
  pub symbols: Map<SymbolId, TokenSymbol>,

  /// All rules that reduce to this production
  pub rules: Array<Rule>,

  /// Productions generated from the expansion of "production" type
  /// symbols such as groups & lists. These productions are only referenced
  /// by the rules defined by this production.
  pub sub_prods: Array<Box<SubProduction>>,

  /// Productions derived from `tk:` invocations of normal productions.
  /// These productions have the special characteristic where none of
  /// their rules contain left recursions
  pub tok_prods: Array<Box<SubProduction>>,

  /// The type of this production
  pub type_: ProductionType,

  /// The name of the production as it is found in the source grammar.
  pub name: IString,

  pub tok: Token,

  pub asts: Array<Box<super::parser::Ascript>>,
}

/// Productions generated from the expansion of "production" type
/// symbols such as groups & lists. These productions are only referenced
/// by the rules defined by this production.
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct SubProduction {
  pub id: ProductionId,

  pub g_id: GrammarId,

  pub rules: Array<Rule>,

  pub type_: SubProductionType,
}

/// Types of [SubProduction]s that may be derived from rule symbols.
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum SubProductionType {
  /// List sub productions are left recursive productions
  /// that are derived from `list` symbols e.g: `A(+) | A(*) | A(+sym) |
  /// A(*sym)` .
  List,
  /// Group productions are derived from group symbols e.g `(...)` and are
  /// created when they are present in rules that have AST definitions to
  /// maintain expected behaviors when referencing symbols in an ast
  /// expression.
  Group,
}

#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct GrammarHeader {
  pub identity:  GrammarIdentity,
  /// Productions that are accessible as entry points to this
  /// grammar.
  pub pub_prods: Array<ProductionId>,

  pub imports: Array<GrammarId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum SymbolType {
  /// A single token string
  Token,
  /// A single, tokenized, production
  TokenProduction(TokenProductionRef),
  Production(ProductionRef),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum ProductionType {
  ContextFree,
  Pratt,
  Peg,
  ParseSTate,
}

/// Identifiers for a Grammar
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct GrammarIdentity {
  /// A globally unique identifier for this GrammarStore instance. Derived
  /// from the source path
  pub guid: GrammarId,

  /// A name defined by the grammar author. This is either the value of the
  /// `@NAME` preamble, or the original file name stem if this preamble is
  /// not present.
  pub name: IString,

  /// The absolute path of the grammar's source file. This may be empty if the
  /// source code was passed in as a string, as with the case of grammars
  /// compiled with
  /// [compile_grammar_from_string](sherpa_core::grammar::compile_grammar_from_string)).
  pub path: IString,
}

impl GrammarIdentity {
  pub fn from_path(
    grammar_source_path: &PathBuf,
    string_store: &StringStore,
  ) -> Self {
    Self {
      guid: grammar_source_path.into(),
      path: grammar_source_path.intern(string_store),
      ..Default::default()
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum SymbolId {
  EndOfFile { precedence: u16 },
  GenericSpace { precedence: u16 },
  GenericHorizontalTab { precedence: u16 },
  GenericNewLine { precedence: u16 },
  GenericIdentifier { precedence: u16 },
  GenericNumber { precedence: u16 },
  GenericSymbol { precedence: u16 },
  Token { id: u64, precedence: u16 },
  NonTerminal { id: ProductionId, precedence: u16 },
  NonTerminalToken { id: ProductionId, precedence: u16 },
  Codepoint { val: u32, precedence: u16 },
}

use ::std::sync;
/// This contains all grammars, productions, and parser states that have
/// been derived from source grammar inputs.
///
/// This object is generally only created once and then passed to entry
/// functions for parser, compilers, and analyzers, with which appropriate
/// derivatives can be created for the respective task.
#[derive(Clone, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct GrammarSoup {
  pub grammar_headers: Arc<sync::RwLock<Map<GrammarId, Box<GrammarHeader>>>>,
  pub productions:     Arc<sync::RwLock<Map<ProductionId, Box<Production>>>>,
  pub custom_states:   Arc<sync::RwLock<Map<ProductionId, Box<CustomState>>>>,
  pub string_store:    StringStore,
}

impl GrammarSoup {
  pub fn new() -> sync::Arc<Self> {
    sync::Arc::new(GrammarSoup {
      grammar_headers: Default::default(),
      productions:     Default::default(),
      custom_states:   Default::default(),
      string_store:    Default::default(),
    })
  }
}

struct ParseBuild {
  root_grammar: GrammarId,
  entries:      Vec<ProductionId>,
  states:       Map<IString, Box<ParseState>>,
  bytecode:     Option<Vec<u8>>,
}
