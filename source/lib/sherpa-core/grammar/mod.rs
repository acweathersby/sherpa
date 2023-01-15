//! Functions for constructing a
//! [GrammarStore](crate::types::GrammarStore) from various types of
//! grammar source files.
pub(crate) mod compile;
//pub(crate) mod compiler_new;
pub mod data;
pub mod item;
pub(crate) mod load;
mod multitask;
pub mod parse;
//pub mod parser;
pub mod production;
pub mod uuid;

pub(crate) use compile::get_scanner_info_from_defined;
pub(crate) use item::*;
pub use production::*;
pub use uuid::*;

pub use compile::{compile_grammar_from_path, compile_grammar_from_string};
