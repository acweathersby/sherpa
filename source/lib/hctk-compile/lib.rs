#![feature(is_some_with)]
#![feature(const_format_args)]
#![feature(const_fmt_arguments_new)]
#![feature(box_patterns)]
#![feature(map_first_last)]
#![allow(non_snake_case)]

mod ascript;
mod builder;
mod llvm;
mod options;
mod source_types;

pub use builder::bytecode::*;
pub use builder::llvm::*;

pub use crate::builder::pipeline::BuildPipeline;
pub use crate::builder::pipeline::CompileError;
pub use source_types::*;

pub mod tasks {
  pub use crate::ascript::build_ast;
  pub use crate::builder::bytecode::build_byte_code_parse;
  pub use crate::builder::disassembly::build_bytecode_disassembly;
  pub use crate::builder::llvm::build_llvm_parser;
  pub use crate::builder::llvm::build_llvm_parser_interface;
}

#[cfg(test)]
mod test {

  use std::path::PathBuf;

  use crate::ascript::compile::compile_ascript_store;
  use crate::ascript::rust;
  use crate::ascript::types::AScriptStore;
  use crate::builder::pipeline::BuildPipeline;
  use crate::tasks::build_ast;
  use hctk_core::types::*;

  use hctk_core::writer::code_writer::StringBuffer;

  #[test]
  fn test_compile_pipeline() {
    BuildPipeline::from_string(
      "
    <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
    | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
    ",
      &PathBuf::from("/tmp"),
    )
    .set_source_output_dir(&std::env::temp_dir())
    .add_task(build_ast(crate::SourceType::Rust))
    .run();
  }

  #[test]
  fn test_output_rust_on_trivial_grammar() {
    use hctk_core::debug::compile_test_grammar;

    let grammar = compile_test_grammar(
      "
        <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
        | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
        ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_ascript_store(&grammar, &mut ascript);

    for error in &errors {
      eprintln!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.structs.len(), 1);

    // let mut writer = StringBuffer::default();

    // rust::write(&grammar, &ascript, &mut writer);

    // println!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
