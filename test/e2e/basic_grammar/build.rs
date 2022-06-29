use std::path::PathBuf;
use std::str::FromStr;

use hc_compile::*;

fn main()
{
    if let Ok(cwd) = std::env::var("CARGO_MANIFEST_DIR")
        .map(|d| PathBuf::from_str(&d).unwrap())
    {
        if let Ok(input) = cwd.join("./grammar.hcg").canonicalize() {
            println!("{}", input.to_str().unwrap());
            println!("cargo:rerun-if-changed={}", input.to_str().unwrap());
            build_files(&input, &cwd, true);
        }
    }
}
