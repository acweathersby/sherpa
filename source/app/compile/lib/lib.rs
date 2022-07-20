#![feature(scoped_threads)]
#![feature(is_some_with)]
#![feature(const_format_args)]
#![feature(const_fmt_arguments_new)]
#![feature(box_patterns)]
#![feature(map_first_last)]

mod asm;
mod ast;
mod builder;
mod llvm;
mod options;
mod writer;

pub use builder::asm::*;
pub use builder::bytecode::*;
pub use builder::llvm::*;

#[cfg(test)]
mod test
{

  use crate::ast::rust;
  use hctk::ascript::compile::compile_reduce_function_expressions;
  use hctk::types::*;

  use crate::writer::code_writer::StringBuffer;

  #[test]
  fn test_output_rust_on_trivial_grammar()
  {
    use hctk::debug::compile_test_grammar;

    let grammar = compile_test_grammar(
        "
        <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
        | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
        ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut ascript);

    for error in &errors {
      println!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.struct_table.len(), 1);

    let mut writer = StringBuffer::default();

    rust::write(&grammar, &ascript, &mut writer);

    println!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
