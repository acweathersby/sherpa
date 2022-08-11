use hctk_compile::compile_llvm_files;
use std::path::PathBuf;
use std::str::FromStr;

fn main()
{
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=grammar.hcg");

  if let Ok(cwd) =
    std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from_str(&d).unwrap())
  {
    if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
      compile_llvm_files(&input, &cwd, true, None);
    }
  }
}
