//! Handles the compilation of a grammar.

mod build_graph;
mod build_ir;
mod build_states;
mod optimize;

pub use build_states::compile_parse_states;
pub use optimize::{garbage_collect, optimize};
