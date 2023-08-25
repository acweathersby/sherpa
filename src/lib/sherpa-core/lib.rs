//! Sherpa Core
//!
//! A Parser Compiler Framework
//!
//! This library contains all functions and types needed to create
//! and update Sherpa grammars, generate Sherpa parser IR states, and
//! optimize to create the best parsers possible.
//!
//! Visit [Sherpa Docs](https://localhost:1313) to learn more about Sherpa.

#![allow(non_snake_case)]
#![feature(box_patterns)]

mod compile;
mod debug;
mod grammar;
mod journal;
pub mod parser;
mod tasks;
mod types;
mod utils;
mod writer;

pub use compile::{compile_parse_states, garbage_collect, optimize};

pub use grammar::{build_compile_db, compile_grammar_from_str, compile_grammars_from_path, remove_grammar_mut};
pub use journal::{Config, Journal, Report, ReportType};
pub use tasks::new_taskman;
pub use types::{
  o_to_r,
  proxy,
  ASTToken,
  CachedString,
  DBProdKey,
  DBRule,
  DBRuleKey,
  DBTokenData,
  DBTokenKey,
  EntryPoint,
  GrammarHeader,
  GrammarId,
  GrammarIdentities,
  GrammarSoup,
  IString,
  IStringStore,
  Item,
  ParseState,
  ParseStatesMap,
  ParseStatesVec,
  ParserDatabase,
  Production,
  ProductionId,
  Rule,
  RuleId,
  SherpaError,
  SherpaErrorSeverity,
  SherpaResult,
  SubProduction,
  SubProductionType,
  SymbolId,
  SymbolRef,
};

pub use utils::{create_u64_hash as hash_id_value_u64, hash_group_btreemap};
pub use writer::code_writer::CodeWriter;

#[cfg(debug_assertions)]
pub mod test;

pub use debug::{console_debugger, string_debugger, PrintConfig};
