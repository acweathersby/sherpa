//! Sherpa Core
//!
//! This library contains all functions and types needed to create
//! an update Sherpa grammars, and to generate Sherpa parser IR states

#![feature(try_trait_v2)]
#![feature(box_patterns)]

pub mod bytecode;
pub mod compile;
pub mod debug;
pub mod grammar;
pub mod journal;
pub mod parser;
pub mod tasks;
pub mod types;
mod utils;
pub mod writer;

#[cfg(test)]
mod test;

pub(crate) use utils::*;
