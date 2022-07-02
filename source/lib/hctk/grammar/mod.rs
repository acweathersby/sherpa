//! Functions for constructing a
//! [GrammarStore](crate::types::GrammarStore) from various types of
//! grammar source files.
mod compile_grammar;
pub mod data;
pub mod item;
pub mod parse;
pub mod production;
pub mod uuid;

pub use compile_grammar::compile_from_path;
pub use compile_grammar::compile_from_string;
pub(crate) use compile_grammar::get_scanner_info_from_defined;
pub use item::*;
pub use production::*;
pub use uuid::*;

#[cfg(test)]
mod test_grammar
{

    use std::path::PathBuf;

    use crate::get_num_of_available_threads;
    use crate::grammar::compile_grammar::compile_from_path;
    use crate::grammar::compile_grammar::pre_process_grammar;

    use super::parse::compile_grammar_ast;
    use super::parse::{self};

    #[test]
    fn test_pre_process_grammar()
    {
        let grammar = String::from(
        "\n@IMPORT ./test/me/out.hcg as bob 
        <> a > tk:p?^test a(+,) ( \\1234 | t:sp? ( sp | g:sym g:sp ) f:r { basalt } ) \\nto <> b > tk:p p ",
    );

        if let Ok(grammar) = compile_grammar_ast(Vec::from(grammar.as_bytes()))
        {
            let (grammar, errors) =
                pre_process_grammar(&grammar, &PathBuf::from("/test"));

            for error in &errors {
                println!("{}", error);
            }

            assert_eq!(errors.len(), 1);
        } else {
            panic!("Failed to parse and produce an AST of '<> a > b'");
        }
    }

    #[test]
    fn test_trivial_file_compilation()
    {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        path.push("../../../test/compile/data/trivial.hcg");

        let (grammar, errors) =
            compile_from_path(&path, get_num_of_available_threads());

        for error in &errors {
            println!("{}", error);
        }

        assert!(grammar.is_some());
        assert!(errors.is_empty());
    }

    #[test]
    fn test_trivial_file_compilation_with_single_import()
    {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        path.push("../../../test/compile/data/trivial_importer.hcg");

        let (grammar, errors) =
            compile_from_path(&path, get_num_of_available_threads());

        assert!(grammar.is_some());

        for error in &errors {
            println!("{}", error);
        }

        assert!(errors.is_empty());
    }
}
