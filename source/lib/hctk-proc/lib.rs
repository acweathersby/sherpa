#![feature(proc_macro_span)]
#![feature(proc_macro_internals)]
use std::path::PathBuf;
use syn::parse::Parse;

use hctk_compile::tasks;
use hctk_compile::BuildPipeline;

extern crate proc_macro;

#[derive(Debug)]
enum ParserType {
  LLVM,
  BYTECODE,
}

#[derive(Debug)]
struct GrammarInput {
  pub parser_type:       ParserType,
  pub grammar:           String,
  pub build_disassembly: bool,
}

impl Parse for GrammarInput {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let mut output = Self {
      parser_type:       ParserType::BYTECODE,
      grammar:           Default::default(),
      build_disassembly: Default::default(),
    };
    while !input.is_empty() {
      let name: syn::Ident = input.parse()?;
      input.parse::<syn::Token![:]>()?;

      match name.to_string().as_str() {
        "parser_type" => {
          let parser_type_string = input.parse::<syn::Ident>()?;
          output.parser_type = match parser_type_string.to_string().as_str() {
            "asm" | "binary" | "assembly" | "llvm" => ParserType::LLVM,
            _ => ParserType::BYTECODE,
          }
        }
        "grammar" => {
          output.grammar = input.parse::<syn::LitStr>()?.value().to_string();
        }
        "disassembly" => {
          output.build_disassembly = input.parse::<syn::LitBool>()?.value();
        }
        _ => return Err(syn::Error::new(name.span(), format!("Unknown field {}", name))),
      }

      if input.peek(syn::Token![,]) {
        input.parse::<syn::Token![,]>()?;
      }
    }

    Ok(output)
  }
}

#[proc_macro]
pub fn compile_mod(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as GrammarInput);

  dbg!(&std::env::var("OUT_DIR"));

  let span = proc_macro::Span::call_site();
  let source = span.source_file();

  let root_dir = if let Some(dir) = source.path().parent() {
    dir.to_path_buf()
  } else {
    std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from(&d)).unwrap()
  };

  let pipeline = BuildPipeline::proc_context(&input.grammar, &source.path());

  let mut pipeline = pipeline.set_source_output_dir(&root_dir);

  pipeline = match (input.parser_type, std::env::var("OUT_DIR").map(|d| PathBuf::from(&d))) {
    (ParserType::LLVM, Ok(out_dir)) => pipeline
      .set_build_output_dir(&out_dir)
      .add_task(tasks::build_llvm_parser(None, "clang-14", "llvm-ar-14", false, true, false))
      .add_task(tasks::build_llvm_parser_interface(true)),

    _ => pipeline.add_task(tasks::build_byte_code_parse(hctk_compile::SourceType::Rust, true)),
  };

  pipeline = if input.build_disassembly {
    pipeline.add_task(tasks::build_bytecode_disassembly())
  } else {
    pipeline
  };

  let (_, artifacts) = pipeline
    .add_task(tasks::build_ast(hctk_compile::SourceType::Rust))
    .set_error_handler(|errors| {
      for error in errors {
        eprintln!("cargo:error=\n{}", error);
      }
      panic!("failed")
    })
    .run();

  artifacts.join("\n").parse().unwrap()
}
