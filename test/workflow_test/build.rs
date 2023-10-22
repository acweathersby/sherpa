use std::{path::PathBuf, str::FromStr};

use sherpa_build::{build, SherpaResult};

fn main() -> SherpaResult<()> {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=rust_script.form");
  println!("cargo:rerun-if-changed=rust_bytecode_script.form");
  println!("cargo:rerun-if-changed=grammar.sg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if let Ok(source_path) = cwd.join("grammar.sg").canonicalize() {
      build(&source_path, &cwd)?
    } else {
      panic!("Could not read grammar file");
    }
  } else {
    panic!("Could not load CARGO_MANIFEST_DIR");
  }

  Ok(())
}
