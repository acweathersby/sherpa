mod build_bytecode;
mod disassemble;
pub use build_bytecode::compile_bytecode;
pub use disassemble::{disassemble_parse_block, generate_disassembly};
