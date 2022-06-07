use std::path::PathBuf;

use crate::{grammar::compiler::compile_string, primitives::GrammarStore};

/// Compiles a single file grammar for use with testing.
pub fn compile_test_grammar() -> GrammarStore {
    const grammar: &str = "
<> A > B 
<> B > C
<> C > D
<> D > E
<> E > B A | t:g";
    match compile_string(&String::from(grammar), &PathBuf::from("/-internal-/test")) {
        Ok(result) => result,
        Err(err) => panic!("{}", err),
    }
}

#[cfg(test)]
mod debug_grammar_tests {
    use super::*;
    #[test]
    fn test_compile_test_grammar() {
        let grammar = compile_test_grammar();

        assert_eq!(
            grammar.source_path.as_os_str().to_str().unwrap(),
            "/-internal-/test"
        );
    }
}
