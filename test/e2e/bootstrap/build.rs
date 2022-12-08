use std::{path::PathBuf, str::FromStr};

use sherpa::{pipeline::compile_bytecode_parser, Config, *};

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=./grammar/*");
  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if !compile_bytecode_parser(
      &cwd.join("./grammar/grammar.hcg").canonicalize().unwrap(),
      Config { opt_inline_redundant_assertions: true, ..Default::default() },
    ) {
      panic!("Failed to build grammar.hcg");
    }
  }
}
