//! # Sherpa
//! A parser compiler
//!
//! ## Examples:
//!
//! ### compile a grammar from a string
#![crate_type = "rlib"]
#![const_eval_limit = "0"]
#![feature(const_eval_limit)]
#![feature(box_patterns)]
#![feature(drain_filter)]
#![feature(btree_drain_filter)]
#![feature(try_trait_v2)]
#![allow(bad_style)]
#![allow(non_snake_case)]
#![warn(missing_docs)]

mod ascript;
mod build;
mod bytecode;
mod grammar;
mod journal;
mod llvm;
mod parser;
mod types;
mod util;
mod writer;

#[cfg(test)]
mod test;

/// Methods for debugging a grammar and it's artifacts.
pub mod debug;

pub use journal::{Config, Journal, Report, ReportType};

pub use types::{SherpaError, SherpaResult};

/// Methods compiling a parser from a grammar.
pub mod compile {
  pub use crate::types::{
    GrammarId,
    GrammarRef,
    GrammarStore,
    Production,
    ProductionId,
    Rule,
    ScannerStateId,
    Symbol,
    SymbolID,
  };
}

/// Error objects
pub mod errors {
  pub use crate::types::{SherpaError, SherpaError::*, SherpaErrorSeverity};
}

/// Create a build pipeline
pub mod pipeline {

  pub use crate::build::pipeline::{
    compile_bytecode_parser,
    compile_llvm_parser,
    BuildPipeline,
    SourceType,
  };
  /// Tasks that can be added to a build pipeline
  pub mod tasks {
    pub use crate::build::{
      ascript::build_ascript_types_and_functions,
      bytecode::build_bytecode_parser,
      disassembly::build_bytecode_disassembly,
      llvm::{build_llvm_parser, build_llvm_parser_interface},
      rust_preamble::build_rust_preamble,
    };
  }
}
