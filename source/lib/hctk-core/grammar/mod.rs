//! Functions for constructing a
//! [GrammarStore](crate::types::GrammarStore) from various types of
//! grammar source files.
mod compile;
pub mod data;
pub mod item;
mod load;
mod multitask;
pub mod parse;
pub mod production;
pub mod uuid;

pub use compile::compile_from_string;
pub(crate) use compile::get_scanner_info_from_defined;
pub use item::*;
pub use production::*;
pub use uuid::*;

#[cfg(test)]
mod test_grammar {

  use std::path::PathBuf;

  use lazy_static::__Deref;

  use crate::debug::debug_items;
  use crate::get_num_of_available_threads;
  use crate::grammar::compile::compile_grammars_into_store;
  use crate::grammar::compile::pre_process_grammar;
  use crate::grammar::load::load_all;
  use crate::types::RecursionType;

  use super::compile::convert_left_to_right;
  use super::compile_from_string;
  use super::get_production_id_by_name;
  use super::get_production_recursion_type;
  use super::get_production_start_items;
  use super::parse::compile_grammar_ast;
  use super::parse::{self};

  #[test]
  fn test_pre_process_grammar() {
    let grammar = String::from(
        "\n@IMPORT ./test/me/out.hcg as bob 
        <> a > tk:p?^test a(+,) ( \\1234 | t:sp? ( sp | g:sym g:sp ) f:r { basalt } ) \\nto <> b > tk:p p ",
    );

    if let Ok(grammar) = compile_grammar_ast(Vec::from(grammar.as_bytes())) {
      let (grammar, errors) = pre_process_grammar(&grammar, &PathBuf::from("/test"), "test");

      for error in &errors {
        eprintln!("{}", error);
      }

      assert_eq!(errors.len(), 1);
    } else {
      panic!("Failed to parse and produce an AST of '<> a > b'");
    }
  }

  #[test]
  fn test_string_list() {
    let grammar = String::from("<> A > ( g:id | g:sym )(+\\\' ) ");

    if let Ok(grammar) = compile_grammar_ast(Vec::from(grammar.as_bytes())) {
      let (grammar, errors) = pre_process_grammar(&grammar, &PathBuf::from("/test"), "test");

      for error in &errors {
        eprintln!("{}", error);
      }

      assert_eq!(errors.len(), 1);
    } else {
      panic!("Failed to parse and produce an AST of '<> a > b'");
    }
  }

  #[test]
  fn test_trivial_file_compilation() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/trivial.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammars, errors) = load_all(&path, thread_count);

    for error in &errors {
      eprintln!("{}", error);
    }

    let result = compile_grammars_into_store(grammars, thread_count);

    let (grammar, errors) = result.unwrap();

    assert!(grammar.is_some());
    assert!(errors.is_none());
  }

  #[test]
  fn test_trivial_file_compilation_with_single_import() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/trivial_importer.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammars, errors) = load_all(&path, thread_count);

    for error in &errors {
      eprintln!("{}", error);
    }

    assert!(errors.is_empty());

    let result = compile_grammars_into_store(grammars, thread_count);

    let (grammar, errors) = result.unwrap();

    assert!(grammar.is_some());
    assert!(errors.is_none());
    assert_eq!(grammar.unwrap().imports.len(), 1);
  }

  #[test]
  fn conversion_of_left_to_right_recursive() {
    let grammar = String::from("<> B > tk:A  <> A > A \\t \\y | A \\u | \\CCC | \\R A ");

    if let Some(mut g) = compile_from_string(&grammar, &PathBuf::from("/test")).0 {
      let prod = get_production_id_by_name("A", &g).unwrap();

      assert!(get_production_recursion_type(prod, &g).contains(RecursionType::LEFT_DIRECT));

      let mut g2 = g.deref().clone();

      let (a, a_prime) = convert_left_to_right(&mut g2, prod);

      assert!(get_production_recursion_type(prod, &g2).contains(RecursionType::RIGHT));
      assert!(!get_production_recursion_type(prod, &g2).contains(RecursionType::LEFT_DIRECT));
    }
  }

  #[test]
  fn processing_of_any_groups() {
    let grammar = String::from("<> A > [ unordered \\g ? ( \\r \\l ) ? ] \\ d ");

    let (grammar, errors) = compile_from_string(&grammar, &PathBuf::from("/test"));

    for error in &errors {
      eprintln!("{}", error);
    }

    assert!(grammar.is_some());

    if let Some(mut g) = grammar {
      let prod = get_production_id_by_name("A", &g).unwrap();

      // convert_left_to_right(&mut g, prod);

      debug_items("A", get_production_start_items(&prod, &g), &g);
    }
  }
}
