use std::{path::PathBuf, str::FromStr};

use sherpa::*;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=./grammar/*");
  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if !compile_bytecode_parser(&cwd.join("./grammar/grammar.hcg").canonicalize().unwrap(), true) {
      panic!("Failed to build grammar.hcg");
    }
  }
}
