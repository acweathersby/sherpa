use std::{path::PathBuf, str::FromStr};

use sherpa::*;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.hcg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    let out_dir = std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap();
    if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
      let (_, sources) = BuildPipeline::from_source(&input, 0)
        .set_source_output_dir(&cwd)
        .set_build_output_dir(&out_dir)
        .set_source_file_name("%.rs")
        .add_task(tasks::build_llvm_parser(None, "clang-14", "llvm-ar-14", false, true, false))
        .add_task(tasks::build_llvm_parser_interface(true))
        .add_task(tasks::build_ast(SourceType::Rust))
        .set_source_output_dir(&cwd)
        .set_error_handler(|errors| {
          for error in errors {
            eprintln!("cargo:error=\n{}", error);
          }
          panic!("failed")
        })
        .run();
    }
  }
}
