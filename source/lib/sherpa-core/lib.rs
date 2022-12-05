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
#![feature(int_roundings)]
#![feature(map_try_insert)]
#![allow(bad_style)]
#![allow(non_snake_case)]

use lazy_static::lazy_static;

mod ascript;
mod build;
mod bytecode;
pub mod debug;
mod deprecated_runtime;
mod grammar;
mod intermediate;
mod journal;
mod llvm;
mod runtime;
mod types;
mod utf8;
mod util;
mod writer;

#[cfg(test)]
mod test;

pub use journal::{Config, Journal, Report, ReportType};

pub mod compile {
  pub use crate::{
    bytecode::{compile_bytecode, BytecodeOutput},
    grammar::parse::{compile_ascript_ast, compile_grammar_ast, compile_ir_ast},
    intermediate::{compile::*, optimize::*},
    types::{
      GrammarId,
      GrammarRef,
      GrammarStore,
      Production,
      ProductionId,
      Rule,
      ScannerStateId,
      Symbol,
      SymbolID,
    },
  };
}
pub mod errors {
  pub use crate::{
    intermediate::errors::*,
    types::{SherpaError, SherpaError::*, SherpaErrorSeverity},
  };
}
/// All types and functions used by bytecode parsers.
pub mod rt {
  pub use crate::{
    runtime::*,
    types::{
      ast::*,
      BaseCharacterReader,
      ByteCharacterReader,
      HCObj,
      HCObjTrait,
      MutCharacterReader,
      ParseAction,
      ParseContext,
      ReduceFunction,
      SherpaError,
      SherpaResult,
      Token,
      UTF8CharacterReader,
      UTF8StringReader,
      NORMAL_STATE_FLAG,
    },
  };
}

/// Create a build pipeline
pub mod pipeline {

  pub use crate::build::pipeline::{compile_bytecode_parser, BuildPipeline, SourceType};

  pub mod tasks {
    pub use crate::build::{
      ascript::build_ascript_types_and_functions,
      bytecode::build_bytecode_parser,
      disassembly::build_bytecode_disassembly,
      llvm::{build_llvm_parser, build_llvm_parser_interface},
    };
  }
}
