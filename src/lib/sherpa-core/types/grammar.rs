#![allow(unused)]

use std::{
  hash::Hash,
  path::{Path, PathBuf},
  rc::Rc,
  sync::Arc,
};

use sherpa_rust_runtime::{
  types::{Token, TokenRange},
  utf8::lookup_table::CodePointClass,
};

use crate::{
  parser::{self, ASTNode},
  types::*,
  utils::create_u64_hash,
  writer::code_writer::CodeWriter,
};

use super::{Array, CachedString, IString, IStringStore, Map, Set, SymbolId};

/// A globally unique identifier for a single non-terminal.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum NonTermId {
  /// Non-terminals directly defined within a grammar.
  Standard(u64, NonTermSubType),
  /// Non-terminals derived from grammar symbols such as the
  /// group `(...)` symbol and at-least-one `sym(+)` symbols. All Sub
  /// non-terminals belong to only one Standard non-terminal
  Sub(u64, u32, NonTermSubType),
}

impl Default for NonTermId {
  fn default() -> Self {
    NonTermId::Standard(0, NonTermSubType::Parser)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum NonTermSubType {
  Parser,
  Scanner,
  ScannerToken,
  ScannerSym,
}

impl From<(GrammarId, &str)> for NonTermId {
  fn from(value: (GrammarId, &str)) -> Self {
    NonTermId::Standard(create_u64_hash(value), NonTermSubType::Parser)
  }
}

impl From<(NonTermId, usize)> for NonTermId {
  fn from((nterm, index): (NonTermId, usize)) -> Self {
    if let NonTermId::Standard(id, sub_type) = nterm {
      NonTermId::Sub(id, index as u32, sub_type)
    } else {
      unreachable!()
    }
  }
}

impl NonTermId {
  pub fn as_state_sym(&self) -> SymbolId {
    SymbolId::NonTerminalState { id: self.as_parse_prod() }
  }

  pub fn as_sym(&self) -> SymbolId {
    SymbolId::NonTerminal { id: self.as_parse_prod() }
  }

  pub fn as_tok_sym(&self) -> SymbolId {
    SymbolId::NonTerminalToken { id: self.as_scan_prod() }
  }

  pub fn as_parse_prod(&self) -> NonTermId {
    match self {
      NonTermId::Standard(id, _) => NonTermId::Standard(*id, NonTermSubType::Parser),
      NonTermId::Sub(id, index, _) => NonTermId::Sub(*id, *index, NonTermSubType::Parser),
    }
  }

  pub fn as_scan_prod(&self) -> NonTermId {
    match self {
      NonTermId::Standard(id, _) => NonTermId::Standard(*id, NonTermSubType::Scanner),
      NonTermId::Sub(id, index, _) => NonTermId::Sub(*id, *index, NonTermSubType::Scanner),
    }
  }

  pub fn set_index(&mut self, index: usize) {
    match self {
      NonTermId::Standard(id, ..) | NonTermId::Sub(id, ..) => {
        *id = index as u64;
      }
    }
  }
}

/// A globally unique identifier for a single grammar source file. Derived from
/// the source's absolute resource path.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct GrammarId(u64);

impl From<&PathBuf> for GrammarId {
  fn from(value: &PathBuf) -> Self {
    GrammarId(create_u64_hash(&value))
  }
}

impl From<&Path> for GrammarId {
  fn from(value: &Path) -> Self {
    GrammarId(create_u64_hash(&value))
  }
}

impl From<&String> for GrammarId {
  fn from(value: &String) -> Self {
    GrammarId(create_u64_hash(&value))
  }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct RuleId(u64);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TokenSymbol {
  pub type_: SymbolType,
  pub val:   IString,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NontermRef(u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TokenNonTermRef(u32);

#[derive(Clone, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SymbolRef {
  /// The type of this symbol.
  pub id: SymbolId,
  /// The original location of this symbol within the grammar source
  pub loc: Token,
  /// The reference name of this symbol
  pub annotation: IString,
  /// The original positional index of the symbol within the original base rule.
  pub original_index: u32,
  /// The precedence of this symbol when present in a scanner state
  pub token_precedence: u16,
  /// Precedence of this symbol when present in a parser state
  pub symbol_precedence: u16,
}

impl Hash for SymbolRef {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    //`tok` is ignored since it does not contribute to
    // the uniqueness of a symbol.
    self.id.hash(state);
    self.annotation.hash(state);
    self.original_index.hash(state);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Rule {
  /// A list of [SymbolId]s and their position within the source grammar
  pub symbols: Array<SymbolRef>,
  pub skipped: Array<SymbolId>,
  pub ast:     Option<ASTToken>,
  pub tok:     Token,
  pub g_id:    GrammarIdentities,
}

/// A reference to some Ascript AST data that is either automatically generated
/// depending on the reference type, or is stored on a NonTerminal node.
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum ASTToken {
  /// Represents the ast expression `:ast [ $1 ]`.
  ///
  ///
  /// Automatically generated when when a list non-terminal (` A(+) | A(*) `) is
  /// processed.
  ListEntry(TokenRange),
  /// Represents the ast expression `:ast [ $1, $--last-- ]`, where `--last--`
  /// represents the last symbol in a rule.
  ///
  /// Automatically generated when a list non-terminal (` A(+) | A(*) `) is
  /// processed.
  ListIterate(TokenRange),
  /// An AST expression defined within a grammar. `0` Is the non-terminal id
  /// in which a copy if the AST expressions is stored. `1` is the index
  /// into the Non-terminals's `asts` array for that stored non-terminal.
  Defined(Arc<parser::Ascript>),
}

#[derive(Clone)]
/// A custom parse state defined within a grammar e.g `state_name => ...`
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct CustomState {
  pub id: NonTermId,
  pub g_id: GrammarId,
  pub guid_name: IString,
  pub friendly_name: IString,
  pub symbols: OrderedSet<SymbolId>,
  pub nterm_refs: OrderedSet<(Token, IString, NonTermId)>,
  pub state: Box<parser::State>,
  pub tok: Token,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTerminalTemplate {
  /// The globally unique name string of the non-terminal. Similar to a C++
  /// mangled name
  pub guid_name: IString,

  /// The name of the non-terminal as it is found in the source grammar. This is
  /// the same as name within the definition for the template rules appended
  /// with  `_template`
  pub friendly_name: IString,

  /// The unique identifier of the owning GrammarHeader.
  pub g_id:  GrammarId,
  
  /// All rules that reduce to this non-terminal
  pub rules: Array<Box<crate::parser::Rule>>,

  /// A list of non-terminal symbol names that are to be replaced by template args
  pub templates: Array<String>,

  pub tok: Token,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTerminal {
  /// The unique identifier of this non-terminal.
  pub id: NonTermId,

  /// The unique identifier of the owning GrammarHEader.
  pub g_id: GrammarId,

  /// All symbols that are referenced by the rules of the
  /// non-terminal and its sub-nonterminals.
  pub symbols: OrderedSet<SymbolId>,

  /// All rules that reduce to this non-terminal
  pub rules: Array<Rule>,

  /// Non-terminals generated from the expansion of "non-terminal" type
  /// symbols such as groups & lists. These nonterminals are only referenced
  /// by the rules defined by this non-terminal.
  pub sub_nterms: Array<Box<SubNonTerminal>>,

  /// Non-terminals derived from `tk:` invocations of normal nonterminals.
  /// These nonterminals have the special characteristic where none of
  /// their rules contain left recursions
  pub tok_nterms: Array<Box<SubNonTerminal>>,

  /// The type of this non-terminal
  pub type_: NonTermType,

  /// The globally unique name string of the non-terminal. Similar to a C++
  /// mangled name
  pub guid_name: IString,

  /// The name of the non-terminal as it is found in the source grammar.
  pub friendly_name: IString,

  pub tok: Token,

  pub asts: Array<Box<parser::Ascript>>,
}

/// Non-terminals generated from the expansion of "non-terminal" type
/// symbols such as groups & lists. These nonterminals are only referenced
/// by the rules defined by this non-terminal.
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SubNonTerminal {
  pub id: NonTermId,

  pub g_id: GrammarId,

  /// The globally unique name string of the non-terminal. Similar to a C++
  /// mangled name
  pub guid_name: IString,

  /// The name of the non-terminal as it is found in the source grammar.
  pub friendly_name: IString,

  pub rules: Array<Rule>,

  pub type_: SubNonTermType,
}

/// Types of [SubNonTerminal]s that may be derived from rule symbols.
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum SubNonTermType {
  /// List sub nonterminals are left recursive nonterminals
  /// that are derived from `list` symbols e.g: `A(+) | A(*) | A(+sym) |
  /// A(*sym)` .
  List,
  /// Group nonterminals are derived from group symbols e.g `(...)` and are
  /// created when they are present in rules that have AST definitions to
  /// maintain expected behaviors when referencing symbols in an ast
  /// expression.
  Group,
}

impl SubNonTermType {
  pub fn to_string(&self) -> String {
    match self {
      SubNonTermType::Group => "grp".into(),
      SubNonTermType::List => "lst".into(),
    }
  }
}

/// Data from a single grammar source file
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct GrammarHeader {
  pub identity:   GrammarIdentities,
  /// Non-terminals that are accessible as entry points to this
  /// grammar. Contains the global id of the public non-terminal
  /// and its export name.
  pub pub_nterms: OrderedMap<IString, (NonTermId, Token)>,

  pub imports: Array<GrammarId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum SymbolType {
  /// A single token string
  Token,
  /// A single, tokenized, non-terminal
  TokenNonterminal(TokenNonTermRef),
  NonTerminal(NontermRef),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum NonTermType {
  ContextFree,
  Pratt,
  Peg,
  ParseState,
}

/// Set of identifiers for a single grammar source
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct GrammarIdentities {
  /// A globally unique identifier for this GrammarStore instance. Derived
  /// from the source path. Assumes the source path is an absolute path
  /// to a grammar source file.
  pub guid: GrammarId,

  /// A globally unique name for this grammar.
  pub guid_name: IString,

  /// A name defined by the grammar author. This is either the value of the
  /// `NAME` preamble, or the original file name stem if this preamble is
  /// not present.
  pub name: IString,

  /// The absolute path of the grammar's source file. This may be empty if the
  /// source code was passed in as a string, as with the case of grammars
  /// compiled with
  /// [compile_grammar_from_string](sherpa_core::grammar::compile_grammar_from_string)).
  pub path: IString,
}

impl GrammarIdentities {
  pub fn from_path(grammar_source_path: &std::path::Path, string_store: &IStringStore) -> Self {
    Self {
      guid: grammar_source_path.into(),
      path: grammar_source_path.intern(string_store),
      ..Default::default()
    }
  }
}

use super::ParserDatabase;

use ::std::sync;
/// This contains all grammars, nonterminals, and parser states that have
/// been derived from source grammar inputs.
///
/// This object is generally only created once and then passed to entry
/// functions for parser, compilers, and analyzers, with which appropriate
/// derivatives can be created for the respective task.
#[derive(Clone, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct GrammarSoup {
  pub grammar_headers: Arc<sync::RwLock<OrderedMap<GrammarId, Box<GrammarHeader>>>>,
  //pub nonterminals:     Arc<sync::RwLock<Map<Non-terminalId, Box<Non-terminal>>>>,
  pub nonterminals:    Arc<sync::RwLock<Array<Box<NonTerminal>>>>,
  pub custom_states:   Arc<sync::RwLock<Map<NonTermId, Box<CustomState>>>>,
  pub string_store:    IStringStore,
}

impl GrammarSoup {
  pub fn from_string_store(string_store: IStringStore) -> sync::Arc<Self> {
    sync::Arc::new(GrammarSoup {
      grammar_headers: Default::default(),
      nonterminals: Default::default(),
      custom_states: Default::default(),
      string_store,
    })
  }

  pub fn new() -> sync::Arc<Self> {
    Self::from_string_store(Default::default())
  }
}
