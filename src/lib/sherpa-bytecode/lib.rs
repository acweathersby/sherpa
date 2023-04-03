#![feature(box_patterns)]

mod build_bytecode;
pub use build_bytecode::compile_bytecode;

#[cfg(test)]
mod test;
