use std::path::PathBuf;
use std::str::FromStr;

use hctk_compile::tasks;
use hctk_compile::BuildPipeline;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.hcg");

  if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap()) {
    if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
      match BuildPipeline::from_source(&input, 0) {
        Ok(mut pipeline) => {
          pipeline
            .set_source_output_dir(&std::env::temp_dir())
            .add_task(tasks::build_llvm_parser(None, false, true))
            .add_task(tasks::build_llvm_parser_interface())
            .add_task(tasks::build_rust_ast())
            .set_source_output_dir(&cwd)
            .set_build_output_dir(&std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap())
            .set_error_handler(|errors| {
              for error in errors {
                eprintln!("cargo:error=\n{}", error);
              }
              panic!("failed")
            })
            .run();
        }
        Err(_) => {}
      }
    }
  }
}
