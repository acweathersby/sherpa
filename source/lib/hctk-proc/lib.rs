#![feature(proc_macro_span)]
#![feature(proc_macro_internals)]
use std::path::PathBuf;
use std::str::FromStr;

use hctk_compile::tasks;
use hctk_compile::BuildPipeline;

extern crate proc_macro;

#[proc_macro]
pub fn compile_mod(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let span = proc_macro::Span::call_site();
  let source = span.source_file();

  let root_dir = if let Some(dir) = source.path().parent() {
    dir.to_path_buf()
  } else {
    std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()).unwrap()
  };

  let grammar = input.to_string()[1..input.to_string().len() - 1]
    .to_string()
    .replace("\\\\", "\\")
    .replace("\\\"", "\"")
    .replace("\\'", "'");

  let (_, artifacts) = BuildPipeline::proc_context(&grammar, &source.path())
    .set_source_output_dir(&root_dir)
    .add_task(tasks::build_byte_code_parse(hctk_compile::SourceType::Rust, true))
    .add_task(tasks::build_ast(hctk_compile::SourceType::Rust))
    //.add_task(tasks::build_bytecode_disassembly())
    .set_error_handler(|errors| {
      for error in errors {
        eprintln!("cargo:error=\n{}", error);
      }
      panic!("failed")
    })
    .run();

  artifacts.join("\n").parse().unwrap()
}
