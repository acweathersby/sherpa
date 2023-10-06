//! Sherpa Core
//!
//! A Parser Compiler Framework
//!
//! This library contains all functions and types needed to create
//! and update Sherpa grammars, generate IR parsers, and
//! optimize to create the best parsers possible.
//!
//! Visit [Sherpa Docs](https://localhost:1313) to learn more about Sherpa.

#![allow(non_snake_case)]
#![feature(box_patterns)]
#![feature(return_position_impl_trait_in_trait)]

mod builder;
mod compile;
mod debug;
mod grammar;
mod journal;
pub mod parser;
mod types;
mod utils;
mod writer;

pub use journal::{Journal, Report, ReportType};
pub use types::{
  o_to_r,
  proxy,
  ASTToken,
  CachedString,
  DBNonTermKey,
  DBRule,
  DBRuleKey,
  DBTermKey,
  DBTokenData,
  EntryPoint,
  ErrorClass,
  GrammarHeader,
  GrammarId,
  GrammarIdentities,
  GrammarSoup,
  IString,
  IStringStore,
  Item,
  NonTermId,
  NonTerminal,
  ParseState,
  ParseStatesMap,
  ParseStatesVec,
  ParserClassification,
  ParserConfig,
  ParserDatabase,
  ParserMetrics,
  ReductionType,
  Rule,
  SherpaError,
  SherpaErrorSeverity,
  SherpaResult,
  SubNonTermType,
  SubNonTerminal,
  SymbolId,
  SymbolRef,
};

pub use builder::*;
pub use utils::{create_u64_hash as hash_id_value_u64, hash_group_btreemap};
pub use writer::code_writer::CodeWriter;

pub mod test;

pub use debug::{console_debugger, PrintConfig};

pub use debug::file_debugger;
