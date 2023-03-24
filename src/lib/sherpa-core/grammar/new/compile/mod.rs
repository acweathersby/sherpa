//! Handles the compilation of a grammar.

mod build_graph;
mod build_ir;
mod compile_states;
mod follow;
mod optimize;
mod types;

use build_graph::*;
use build_ir::*;
use compile_states::*;
use follow::*;
use optimize::*;

pub use compile_states::compile_parse_states;
pub use optimize::{garbage_collect, optimize};
