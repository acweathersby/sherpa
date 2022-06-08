use std::path::PathBuf;

use crate::{grammar::compiler::compile_from_string, primitives::GrammarStore};

/// Compiles a single grammar string
pub fn compile_test_grammar(grammar: &str) -> GrammarStore {
    match compile_from_string(&String::from(grammar), &PathBuf::from("/-internal-/test")) {
        Ok(result) => result,
        Err(err) => panic!("{}", err),
    }
}

#[cfg(test)]
mod debug_grammar_tests {
    use super::*;
    #[test]
    fn test_compile_test_grammar() {
        let grammar = compile_test_grammar(
            "
        <> A > B 
        <> B > C
        <> C > D
        <> D > E
        <> E > B A | t:g",
        );

        assert_eq!(
            grammar.source_path.as_os_str().to_str().unwrap(),
            "/-internal-/test"
        );
    }
}
