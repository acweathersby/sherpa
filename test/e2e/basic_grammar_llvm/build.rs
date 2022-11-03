use std::path::PathBuf;

use std::str::FromStr;

use hctk_compile::compile_llvm_files;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
      println!("cargo:rerun-if-changed={}", input.to_str().unwrap());
      compile_llvm_files(&input, &cwd, true, None);
    }
  }
}
// Testd
