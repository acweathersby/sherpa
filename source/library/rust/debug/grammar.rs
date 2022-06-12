use std::path::PathBuf;

use crate::grammar::compiler::compile_from_string;
use crate::primitives::GrammarStore;

/// Compiles a single grammar string

pub fn compile_test_grammar(grammar: &str) -> GrammarStore
{
    let (grammar, errors) = compile_from_string(
        &String::from(grammar),
        &PathBuf::from("/-internal-/test"),
    );

    for error in &errors {
        println!("{}", error);
    }

    assert!(grammar.is_some());
    assert!(errors.is_empty());

    grammar.unwrap()
}

#[cfg(test)]

mod debug_grammar_tests
{

    use super::*;

    #[test]

    fn test_compile_test_grammar()
    {
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
