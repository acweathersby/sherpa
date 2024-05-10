use radlr_build::*;
use std::path::Path;

const RADLR_GRAMMAR_PATH: &'static str = "./grammars/radlr/3.0.0-pre-bootstrap";

const RADLR_GRAMMAR_ROOT: &'static str = "radlr.radlr";

const BUILD_OUTPUT_PATH: &'static str = "./parser/";

fn main() {}

fn _main() -> RadlrResult<()> {
  let RADLR_dir = std::process::Command::new(env!("CARGO"))
    .arg("locate-project")
    .arg("--workspace")
    .arg("--message-format=plain")
    .output()
    .unwrap()
    .stdout;

  let workspace_dir =
    Path::new(std::str::from_utf8(&RADLR_dir).expect("Could not locate workspace folder").trim()).parent().unwrap();

  let RADLR_dir = workspace_dir.join(RADLR_GRAMMAR_PATH).canonicalize().expect("Could not find RADLR grammar dir");
  let out_dir = Path::new(BUILD_OUTPUT_PATH).canonicalize().expect("Could not find output dir");

  println!("cargo:rerun-if-changed={}", RADLR_dir.to_str().expect("Could not create str from RADLR dir path"));

  let radlr_root_file_path = RADLR_dir.join(RADLR_GRAMMAR_ROOT);

  let mut build_config = BuildConfig::new(&radlr_root_file_path);
  build_config.source_out = &out_dir;
  build_config.lib_out = &out_dir;

  fs_build(build_config, Default::default(), TargetLanguage::Rust)?;

  Ok(())
}
