//! Functions for constructing a
//! [GrammarStore](crate::types::GrammarStore) from various types of
//! grammar source files.
mod compile;
pub mod data;
pub mod item;
mod load;
mod multitask;
pub mod parse;
mod parser_new;
pub mod production;
pub mod uuid;

pub(crate) use compile::get_scanner_info_from_defined;
pub(crate) use item::*;
pub use production::*;
pub use uuid::*;

#[cfg(test)]
mod test;

pub use compile::{compile_grammar_from_path, compile_grammar_from_string};
