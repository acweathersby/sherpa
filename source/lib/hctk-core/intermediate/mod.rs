pub(crate) mod algorithm;
pub mod compile;
pub mod errors;
mod ir;
mod ir_parser;
pub mod optimize;
pub(crate) mod utils;
pub(crate) use algorithm::*;

#[cfg(test)]
mod test;
