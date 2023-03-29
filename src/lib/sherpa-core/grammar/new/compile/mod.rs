//! Handles the compilation of a grammar.

mod build_graph;
mod build_ir;
mod build_states;
mod follow;
mod optimize;
mod types;

use build_graph::*;
use build_ir::*;
use build_states::*;
use follow::*;
use optimize::*;

pub use build_states::compile_parse_states;
pub use optimize::{garbage_collect, optimize};
pub(crate) use types::{ItemRef, ItemSet, Items};
