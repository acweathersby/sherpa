#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
pub mod ascript_functions;
pub mod jit_parser;
pub mod llvm_parser_build;
pub mod parse_functions;
pub mod standard_functions;
mod types;

pub use standard_functions::*;
pub use types::*;

#[cfg(debug_assertions)]
mod test;
