use std::{path::PathBuf, str::FromStr};

use sherpa::{
  pipeline::{tasks, BuildPipeline, SourceType},
  Config,
};

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.hcg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    let out_dir = std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap();
    if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
      BuildPipeline::from_source(&input, Config::default())
        .set_source_output_dir(&cwd)
        .set_build_output_dir(&out_dir)
        .set_source_file_name("%.rs")
        .add_task(tasks::build_llvm_parser(None, true, false))
        .add_task(tasks::build_llvm_parser_interface())
        .add_task(tasks::build_ascript_types_and_functions(SourceType::Rust))
        .set_source_output_dir(&cwd)
        .run(|errors| {
          for error in errors {
            eprintln!("cargo:error=\n{}", error);
          }
          panic!("failed")
        });
    }
  }
}
