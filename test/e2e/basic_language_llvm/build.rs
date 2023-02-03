use std::{path::PathBuf, str::FromStr};

use sherpa::{
  pipeline::{compile_bytecode_parser, compile_llvm_parser},
  Config,
};

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.sg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if let Ok(source_path) = cwd.join("./grammar.sg").canonicalize() {
      if false {
        if !compile_bytecode_parser(&source_path, Config {
          opt_inline_redundant_assertions: true,
          ..Default::default()
        }) {
          panic!("Failed to build grammar.sg");
        }
      } else {
        if !compile_llvm_parser(&source_path, Config {
          opt_inline_redundant_assertions: true,
          opt_llvm: true,
          ..Default::default()
        }) {
          panic!("Failed to build grammar.sg");
        }
      }
    }
  }
}
