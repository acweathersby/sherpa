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

use journal::config::SourceType;
use lazy_static::lazy_static;

mod ascript;
mod builder;
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
mod writer;

pub mod compile {
  pub use crate::{
    bytecode::{compile_bytecode, BytecodeOutput},
    grammar::parse::{compile_ascript_ast, compile_grammar_ast, compile_ir_ast},
    intermediate::{compile::*, optimize::*},
    journal::*,
    types::{
      GrammarId,
      GrammarRef,
      GrammarStore,
      Production,
      ProductionId,
      Rule,
      ScannerId,
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
  use std::{fs::create_dir_all, path::PathBuf};

  use crate::{journal::config::SourceType, types::SherpaResult};

  pub use crate::builder::pipeline::BuildPipeline;

  pub mod tasks {
    pub use crate::{
      ascript::build_ast,
      builder::{
        bytecode::build_bytecode_parser,
        disassembly::build_bytecode_disassembly,
        llvm::{build_llvm_parser, build_llvm_parser_interface},
      },
    };
  }
  /// Convenience function for building a bytecode based parser. Use this in
  /// build scripts to output a parser source file to `{OUT_DIR}/sherpa_out/{grammar_name}.rs`.
  pub fn compile_bytecode_parser(grammar_source_path: &PathBuf, include_ascript: bool) -> bool {
    let mut out_dir = std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap();

    out_dir.push("./sherpa_out/");

    create_dir_all(&out_dir).unwrap();

    let pipeline = BuildPipeline::from_source(&grammar_source_path, 0)
      .set_source_output_dir(&out_dir)
      .set_build_output_dir(&out_dir)
      .set_source_file_name("%.rs")
      .add_task(tasks::build_bytecode_parser(SourceType::Rust, include_ascript));

    match if include_ascript {
      pipeline.add_task(tasks::build_ast(SourceType::Rust))
    } else {
      pipeline
    }
    .add_task(tasks::build_bytecode_disassembly())
    .run(|errors| {
      for error in &errors {
        eprintln!("{}", error);
      }
    }) {
      SherpaResult::Ok(_) => true,
      _ => false,
    }
  }
}

/// Retrieve the number of threads that can be reasonably
/// run concurrently on the platform
pub(crate) fn get_num_of_available_threads() -> usize {
  std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()).get()
}

// Common utility functions
use std::num::NonZeroUsize;
#[cfg(test)]
mod test_end_to_end {
  use crate::{
    bytecode::compile::build_byte_code_buffer,
    debug::collect_shifts_and_skips,
    get_num_of_available_threads,
    intermediate::compile::compile_states,
    journal::Journal,
    types::*,
  };

  #[test]
  fn test_pipeline() -> SherpaResult<()> {
    let threads = get_num_of_available_threads();

    let mut j = Journal::new(None);

    let g = GrammarStore::from_str(
      &mut j,
      "
@IGNORE g:sp g:tab

<> start > \\hello \\world 
",
    )
    .unwrap();

    let mut states = compile_states(&mut j, threads)?;

    for state in states.values_mut() {
      if state.get_ast().is_none() {
        println!("--FAILED: {:?}", state.compile_ast())
      }
    }

    let entry_state_name = &g.get_production_by_name("start").unwrap().guid_name;

    let (bytecode, state_lookup) =
      build_byte_code_buffer(states.iter().map(|(_, s)| s.get_ast().unwrap()).collect::<Vec<_>>());

    let entry_point = *state_lookup.get(entry_state_name).unwrap();

    let target_production_id = g.get_production_by_name("start").unwrap().bytecode_id;

    let (reader, _, shifts, skips) =
      collect_shifts_and_skips("hello    \tworld", entry_point, target_production_id, bytecode);

    assert!(reader.at_end());

    assert_eq!(shifts, ["hello", "world"]);

    assert_eq!(skips, ["    \t"]);

    j.flush_reports();

    j.debug_report(crate::compile::ReportType::Any);

    SherpaResult::Ok(())
  }
}

#[cfg(test)]
mod library_smoke_tests {

  use std::path::PathBuf;

  use crate::{
    ascript::types::AScriptStore,
    builder::pipeline::BuildPipeline,
    compile::Journal,
    pipeline::tasks::build_ast,
    types::GrammarStore,
  };

  #[test]
  fn test_compile_pipeline() {
    BuildPipeline::from_string(
      "
    <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
    | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
    ",
      &PathBuf::from("/tmp"),
    )
    .set_source_output_dir(&std::env::temp_dir())
    .add_task(build_ast(crate::SourceType::Rust))
    .run(|e| {});
  }

  #[test]
  fn test_output_rust_on_trivial_grammar() {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(
      &mut j,
      "
        <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
        | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
        ",
    )
    .unwrap();

    let mut ascript = AScriptStore::new(g).unwrap();

    assert_eq!(ascript.structs.len(), 1);
  }
}
