//! Sherpa Core
//!
//! This library contains all functions and types needed to create
//! an update Sherpa grammars, and to generate Sherpa parser IR states

#![feature(try_trait_v2)]
#![feature(box_patterns)]

mod bytecode;
mod compile;
mod debug;
mod grammar;
mod journal;
pub mod parser;
mod tasks;
mod types;
mod utils;
mod writer;

#[cfg(test)]
mod test;

pub use grammar::{compile_grammar_from_str, compile_grammars_from_path};
pub use journal::{Config, Journal};
pub use types::{
  GrammarSoup,
  IString,
  IStringStore,
  ParserDatabase,
  Production,
  ProductionId,
  Rule,
  SherpaError,
  SherpaResult,
  SubProduction,
  SubProductionType,
  SymbolId,
};
