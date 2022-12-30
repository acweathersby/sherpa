use std::{path::PathBuf, str::FromStr};

use sherpa::{
  pipeline::{
    compile_bytecode_parser,
    tasks::{self, build_bytecode_disassembly},
    BuildPipeline,
    SourceType,
  },
  Config,
};

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.hcg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    let out_dir = std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap();
    if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
      if false {
        if !compile_bytecode_parser(&input, Config {
          opt_inline_redundant_assertions: true,
          ..Default::default()
        }) {
          panic!("Failed to build grammar.hcg");
        }
      } else {
        BuildPipeline::from_source(&input, Config {
          llvm_light_lto: false,
          opt_llvm: false,
          ..Config::default()
        })
        .set_source_output_dir(&cwd)
        .set_build_output_dir(&out_dir)
        .set_source_file_name("%.rs")
        .add_task(tasks::build_rust_preamble())
        .add_task(tasks::build_llvm_parser(None, true, true))
        .add_task(tasks::build_llvm_parser_interface())
        .add_task(build_bytecode_disassembly())
        .add_task(tasks::build_ascript_types_and_functions(SourceType::Rust))
        .set_source_output_dir(&cwd)
        .run(|errors| {
          for error in errors {
            eprintln!("cargo:error=\n{}", error);
          }
          panic!("failed")
        });
      }
    }
  }
}
