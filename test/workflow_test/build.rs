use radlr_build::{fs_build, BuildConfig, RadlrResult};
use std::{path::PathBuf, str::FromStr};

fn main() -> RadlrResult<()> {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=rust_script.form");
  println!("cargo:rerun-if-changed=rust_bytecode_script.form");
  println!("cargo:rerun-if-changed=grammar.sg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if let Ok(source_path) = cwd.join("grammar.sg").canonicalize() {
      let mut build_config = BuildConfig::new(&source_path);
      let build_dir = cwd.join("parser");
      build_config.source_out = &build_dir;
      build_config.lib_out = &build_dir;
      build_config.rust.add_mod = true;
      build_config.rust.flat_namespace = true;

      fs_build(build_config, Default::default(), radlr_build::TargetLanguage::Rust)?;
    } else {
      panic!("Could not read grammar file");
    }
  } else {
    panic!("Could not load CARGO_MANIFEST_DIR");
  }

  Ok(())
}
