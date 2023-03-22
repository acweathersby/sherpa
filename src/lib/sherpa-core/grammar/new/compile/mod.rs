//! Handles the compilation of a grammar.

mod build_graph;
mod build_ir;
mod compile_states;
mod follow;
mod types;

use build_graph::*;
use build_ir::*;
use compile_states::*;
use follow::*;

pub use compile_states::compile_parse_states;
