#![crate_type = "rlib"]
#![feature(const_eval_limit)]
#![const_eval_limit = "0"]
#![feature(new_uninit)]
#![feature(get_mut_unchecked)]
#![feature(scoped_threads)]
#![feature(core_intrinsics)]
#![feature(box_patterns)]
#![feature(map_first_last)]
#![allow(
    bad_style,
    dead_code,
    unused,
    unused_allocation,
    unused_comparisons,
    unused_parens
)]

pub mod runtime;
pub mod types;
pub mod utf8;

pub use lazy_static::lazy_static;

pub mod ascript;
pub mod bytecode;
pub mod debug;
pub mod grammar;
pub mod intermediate;

// Common utility functions

use std::num::NonZeroUsize;

/// Retrieve the number of threads that can be reasonably
/// run concurrently on the platform

pub fn get_num_of_available_threads() -> usize
{
    std::thread::available_parallelism()
        .unwrap_or(NonZeroUsize::new(1).unwrap())
        .get()
}
#[cfg(test)]
mod test_end_to_end
{
    use crate::bytecode::compile_bytecode::build_byte_code_buffer;
    use crate::debug::compile_test_grammar;
    use crate::debug::parser::collect_shifts_and_skips;
    use crate::debug::print_states;
    use crate::debug::BytecodeGrammarLookups;
    use crate::get_num_of_available_threads;
    use crate::grammar::get_production_by_name;
    use crate::grammar::get_production_id_by_name;
    use crate::intermediate::optimize::optimize_states;
    use crate::intermediate::state_construction::compile_states;
    use crate::runtime::parser::get_next_action;
    use crate::types::*;
    use std::sync::Arc;

    #[test]
    fn test_basic_grammar_build()
    {
        let threads = get_num_of_available_threads();

        let grammar = compile_test_grammar(
            "
@IGNORE g:sp g:tab

<> start > \\hello \\world 
",
        );

        let mut states = compile_states(&grammar, threads);
        for state in states.values_mut() {
            if state.get_ast().is_none() {
                println!("--FAILED: {:?}", state.compile_ast())
            }
        }
        let optimized_states = optimize_states(&mut states, &grammar);
        let (bytecode, state_lookup) = build_byte_code_buffer(optimized_states);
        let entry_point = *state_lookup.get("start").unwrap();

        let target_production_id = get_production_by_name("start", &grammar)
            .unwrap()
            .bytecode_id;
        let (reader, state, shifts, skips) = collect_shifts_and_skips(
            "hello    \tworld",
            entry_point,
            target_production_id,
            bytecode,
        );

        assert!(reader.at_end());

        assert_eq!(target_production_id, 0);

        assert_eq!(shifts, ["hello", "world"]);

        assert_eq!(skips, ["    \t"]);
    }
}
