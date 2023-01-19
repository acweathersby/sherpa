use std::{path::PathBuf, str::FromStr};

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.hcg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    let output_path = cwd.join("./grammar/compile/parser/").canonicalize().unwrap();
    println!("cargo:rustc-link-search=native={}", output_path.to_str().unwrap());
    println!("cargo:rustc-link-lib=static=sherpa");
  }
}
