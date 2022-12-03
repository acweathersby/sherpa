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

mod bytecode;
mod deprecated_runtime;
mod grammar;
mod intermediate;
mod journal;
mod types;

mod runtime;
pub mod utf8;
pub mod writer;

pub use lazy_static::lazy_static;

pub use types::*;

pub mod debug;

pub use journal::{
  config::{Config, ResolutionMode},
  report::{Report, ReportType},
  Journal,
};

pub mod guid {
  pub use crate::grammar::{hash_id_value_u128, hash_id_value_u64};
}

/// The AST types of the Sherpa Grammar
pub mod ast {
  pub use crate::grammar::data::ast::*;
}

pub mod compile {
  pub use crate::{
    bytecode::{compile_bytecode, BytecodeOutput},
    grammar::parse::{compile_ascript_ast, compile_grammar_ast, compile_ir_ast},
    intermediate::{compile::*, optimize::*},
  };
}

pub mod errors {
  pub use crate::{
    intermediate::errors::*,
    types::{HCError, HCError::*, HCErrorSeverity},
  };
}

/// All types and functions used by bytecode parsers.
pub mod rt {
  pub use crate::{
    ast::*,
    runtime::*,
    BaseCharacterReader,
    ByteCharacterReader,
    HCError,
    HCObj,
    HCObjTrait,
    HCResult,
    MutCharacterReader,
    ParseAction,
    ParseContext,
    ReduceFunction,
    Token,
    UTF8CharacterReader,
    UTF8StringReader,
    NORMAL_STATE_FLAG,
  };
}

/// Retrieve the number of threads that can be reasonably
/// run concurrently on the platform
pub fn get_num_of_available_threads() -> usize {
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
  fn test_pipeline() -> HCResult<()> {
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

    let (reader, state, shifts, skips) =
      collect_shifts_and_skips("hello    \tworld", entry_point, target_production_id, bytecode);

    assert!(reader.at_end());

    assert_eq!(shifts, ["hello", "world"]);

    assert_eq!(skips, ["    \t"]);

    j.flush_reports();

    j.debug_report(crate::ReportType::Any);

    HCResult::Ok(())
  }
}
