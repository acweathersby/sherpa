use std::{path::PathBuf, str::FromStr};

use sherpa::{
  pipeline::{
    compile_bytecode_parser,
    compile_llvm_parser,
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
    if let Ok(source_path) = cwd.join("./grammar.hcg").canonicalize() {
      println!("{}", source_path.to_str().unwrap());
      println!("cargo:rerun-if-changed={}", source_path.to_str().unwrap());
      compile_bytecode_parser(&source_path, Config {
        opt_inline_redundant_assertions: true,
        ..Default::default()
      });
    }
  }
}
