//! # The Sherpa Runtime Library
//!
//! The Sherpa Runtime Library provides the runtime functions and types
//! necessary to execute Sherpa parsers in Rust.
#![feature(const_eval_limit)]
#![feature(const_mut_refs)]
#![feature(const_for)]
#![const_eval_limit = "0"]

pub mod bytecode;
pub mod llvm_parser;
pub mod types;
pub mod utf8;
