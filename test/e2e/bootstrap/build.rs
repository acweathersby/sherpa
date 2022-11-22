use std::path::PathBuf;
use std::str::FromStr;

use hctk::*;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=./grammar/*");
  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if !compile_bytecode_parser(&cwd.join("./grammar/symbol.hcg").canonicalize().unwrap()) {
      panic!("Failed to build symbol.hcg");
    }

    if !compile_bytecode_parser(&cwd.join("./grammar/script.hcg").canonicalize().unwrap()) {
      panic!("Failed to build script.hcg");
    }

    if !compile_bytecode_parser(&cwd.join("./grammar/production.hcg").canonicalize().unwrap()) {
      panic!("Failed to build production.hcg");
    }
  }
}
