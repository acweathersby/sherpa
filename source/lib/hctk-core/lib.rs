#![crate_type = "rlib"]
#![const_eval_limit = "0"]
#![feature(const_eval_limit)]
#![feature(new_uninit)]
#![feature(get_mut_unchecked)]
#![feature(core_intrinsics)]
#![feature(box_patterns)]
#![feature(drain_filter)]
#![feature(btree_drain_filter)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![feature(const_trait_impl)]
#![feature(int_roundings)]
#![feature(map_try_insert)]
#![allow(bad_style, dead_code, unused, unused_allocation, unused_comparisons, unused_parens)]

mod deprecated_runtime;
pub mod grammar;
pub mod runtime;
pub mod types;
pub mod utf8;
pub mod writer;

pub use lazy_static::lazy_static;

pub mod bytecode;
pub mod debug;
pub mod intermediate;

pub use grammar::compile_grammar_from_path;
pub use grammar::compile_grammar_from_string;

// Common utility functions
use std::num::NonZeroUsize;

/// Retrieve the number of threads that can be reasonably
/// run concurrently on the platform

pub fn get_num_of_available_threads() -> usize {
  std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()).get()
}
#[cfg(test)]
mod test_end_to_end {
  use crate::bytecode::compile::build_byte_code_buffer;
  use crate::debug::collect_shifts_and_skips;
  use crate::debug::generate_disassembly;
  use crate::debug::BytecodeGrammarLookups;
  use crate::get_num_of_available_threads;
  use crate::intermediate::state::compile_states;
  use crate::runtime::get_next_action;
  use crate::types::*;
  use std::sync::Arc;

  #[test]
  fn test_basic_grammar_build() {
    let threads = get_num_of_available_threads();

    let g = GrammarStore::from_str(
      "
@IGNORE g:sp g:tab

<> start > \\hello \\world 
",
    )
    .unwrap();

    let (mut states, _) = compile_states(g.clone(), threads);

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
  }
}
