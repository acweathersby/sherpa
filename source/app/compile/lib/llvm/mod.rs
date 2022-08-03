mod llvm_ir;
pub mod llvm_ir_inkwell;
mod llvm_utf8_ir;

use std::path::Path;

pub use llvm_ir::*;

#[test]
fn test_compile_from_bytecode() -> core::result::Result<(), ()>
{
  use crate::llvm::llvm_ir_inkwell::compile_from_bytecode;
  use crate::options::Architecture;
  use crate::options::BuildOptions;
  use hctk::bytecode::compile_bytecode;
  use hctk::debug::compile_test_grammar;
  use inkwell::context::Context;
  use std::fs::File;
  use std::io::Write;
  let grammar = compile_test_grammar(
    "
  @IGNORE g:sp
 
  <> test > \\hello \\world
  ",
  );

  let bytecode_output = compile_bytecode(&grammar, 1);

  let context = Context::create();

  if let Ok(ctx) = compile_from_bytecode(
    &context,
    &BuildOptions { architecture: Architecture::X8664, ..Default::default() },
    &bytecode_output,
  ) {
    ctx.module.write_bitcode_to_path(&Path::new("../parser.bc"));
    let mut file = File::create("../test.ll");
    if let Ok(mut file) = file {
      file.write_all(ctx.module.to_string().as_bytes());
    }
  }

  Ok(())
}
