//! Functions for constructing a
//! [GrammarStore](crate::types::GrammarStore) from various types of
//! grammar source files.
pub(crate) mod compile_old;
pub mod data;
pub mod item;
pub(crate) mod load;
mod multitask;
pub mod parse;
pub mod production;
pub mod uuid;

pub mod compile;

pub(crate) use compile_old::get_scanner_info_from_defined;
pub(crate) use item::*;
pub use production::*;
pub use uuid::*;

pub use compile_old::{compile_grammar_from_path, compile_grammar_from_string};
