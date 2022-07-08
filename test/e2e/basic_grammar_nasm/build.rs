use std::path::PathBuf;
use std::str::FromStr;

use hctk_compile::compile_asm_files;

fn main()
{
    if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR")
        .map(|d| PathBuf::from_str(&d).unwrap())
    {
        if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
            compile_asm_files(&input, &cwd, true);
            println!("cargo:rerun-if-changed={}", input.to_str().unwrap());
        }
    }
}
