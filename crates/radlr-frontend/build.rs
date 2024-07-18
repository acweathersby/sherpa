use radlr_build::*;
use std::path::{Path, PathBuf};

const RADLR_GRAMMAR_PATH: &'static str = "./grammars/radlr/3.0.0-pre-bootstrap";

const RADLR_GRAMMAR_ROOT: &'static str = "radlr.radlr";

const BUILD_OUTPUT_PATH: &'static str = "./parser/";

fn main() {}

fn main_() -> RadlrResult<()> {
  let cargo_file = PathBuf::from(
    String::from_utf8(
      std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout,
    )
    .unwrap(),
  );

  let RADLR_dir = cargo_file.parent().unwrap().clone();

  let grammar_root_dir = RADLR_dir.join(RADLR_GRAMMAR_PATH);

  dbg!(&grammar_root_dir);

  let out_dir =
    Path::new(&std::env::var("OUT_DIR").unwrap()).canonicalize().expect("Could not find output dir").join(BUILD_OUTPUT_PATH);

  println!("cargo:rerun-if-changed={}", grammar_root_dir.to_str().expect("Could not create str from RADLR dir path"));

  let radlr_root_file_path = grammar_root_dir.join(RADLR_GRAMMAR_ROOT);

  let mut build_config = BuildConfig::new(&radlr_root_file_path);
  build_config.source_out = &out_dir;
  build_config.lib_out = &out_dir;

  fs_build(build_config, Default::default(), TargetLanguage::Rust)?;

  Ok(())
}
