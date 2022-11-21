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

use std::path::PathBuf;
use std::sync::Arc;

pub(crate) use compile::get_scanner_info_from_defined;
pub use item::*;
pub use production::*;
pub use uuid::*;

use self::compile::compile_grammars_into_store;
use self::load::load_all;
use self::parse::compile_grammar_ast;
use crate::types::*;

#[test]
fn temp_test() {
  let (grammars, errors) = load_all(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/e2e/bootstrap/grammar/production.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  let result = compile_grammars_into_store(grammars, 10);

  assert!(result.is_ok());

  match result {
    HCResult::Ok((Some(grammar), _)) => {
      //  dbg!(grammar);
    }
    HCResult::Ok((_, Some(errors))) => {
      for err in errors {
        println!("{}", err);
      }
      panic!("Errors occurred while compiling")
    }
    _ => {}
  }
}

/// Loads and compiles a grammar from a source file.
///
/// # Example
///
/// Basic usage:
/// ```
///  # use std::path::PathBuf;
/// use hctk_core::compile_grammar_from_path;
///
/// let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
///
/// path.push("../../../test/grammars/trivial.hcg");
///
/// let (grammar, errors) = compile_grammar_from_path(path, 10);
///
/// assert_eq!(grammar.unwrap().name, "trivial")
/// ```
pub fn compile_grammar_from_path(
  path: PathBuf,
  thread_count: usize,
) -> (Option<Arc<GrammarStore>>, Option<Vec<HCError>>) {
  match load_all(&path, thread_count) {
    (_, errors) if !errors.is_empty() => (None, Some(errors)),
    (grammars, _) => compile_grammars_into_store(grammars, thread_count).unwrap(),
  }
}

/// Compiles a grammar from a string.
///
/// # Example
///
/// Basic usage:
/// ```
///  # use std::path::PathBuf;
///  use hctk_core::compile_grammar_from_string;
///
/// let (grammar, errors) = compile_grammar_from_string(
///   "@NAME my_grammar
///   <> A > \\hello \\world ",
///   &PathBuf::default(),
/// );
///
/// assert_eq!(grammar.unwrap().name, "my_grammar")
/// ```
pub fn compile_grammar_from_string(
  string: &str,
  absolute_path: &PathBuf,
) -> (Option<Arc<GrammarStore>>, Option<Vec<HCError>>) {
  match compile_grammar_ast(Vec::from(string.as_bytes())) {
    Ok(grammar) => {
      compile_grammars_into_store(vec![(absolute_path.clone(), Default::default(), grammar)], 0)
        .unwrap()
    }
    Err(err) => (None, Some(vec![err])),
  }
}

#[cfg(test)]
mod test_grammar {

  use std::path::PathBuf;

  use lazy_static::__Deref;

  use crate::debug::debug_items;
  use crate::get_num_of_available_threads;
  use crate::grammar::compile::compile_grammars_into_store;
  use crate::grammar::compile::pre_process_grammar;
  use crate::grammar::compile_grammar_from_path;
  use crate::grammar::load::load_all;
  use crate::types::RecursionType;

  use super::compile::convert_left_recursion_to_right;
  use super::compile_grammar_from_string;
  use super::get_production_start_items;
  use super::parse::compile_grammar_ast;
  use super::parse::{self};

  #[test]
  fn test_merge_productions_file() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/merge_root.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammar, errors) = compile_grammar_from_path(path, get_num_of_available_threads());

    if errors.is_some() {
      for error in errors.unwrap_or_default() {
        eprintln!("{}", error);
      }
      panic!("Errors encountered");
    } else {
      assert!(grammar.is_some());
    }
  }

  #[test]
  fn test_trivial_file_compilation() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/trivial.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammar, errors) = compile_grammar_from_path(path, get_num_of_available_threads());

    assert!(grammar.is_some());
    assert!(errors.is_none());
  }

  #[test]
  fn test_trivial_file_compilation_with_single_import() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/trivial_importer.hcg");

    let (grammar, errors) = compile_grammar_from_path(path, get_num_of_available_threads());

    assert!(grammar.is_some());
    assert!(errors.is_none());
    assert_eq!(grammar.unwrap().imports.len(), 1);
  }

  #[test]
  fn conversion_of_left_to_right_recursive() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/left_recursive_token_production.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammar, errors) = compile_grammar_from_path(path, get_num_of_available_threads());

    assert!(errors.is_none());
    assert!(grammar.is_some());
    let grammar = grammar.unwrap();

    let left_recursive_prod = grammar.get_production_by_name("tk:left_recursive").unwrap();

    assert!(!left_recursive_prod
      .recursion_type
      .contains(RecursionType::LEFT_INDIRECT | RecursionType::LEFT_DIRECT));
  }

  #[test]
  fn left_to_right_recursive_conversion() {
    if let Some(mut g) = compile_grammar_from_string(
      "<> B > tk:A  <> A > A \\t \\y | A \\u | \\CCC | \\R A ",
      &PathBuf::from("/test"),
    )
    .0
    {
      let prod = g.get_production_id_by_name("A").unwrap();

      assert!(g.get_production_recursion_type(prod).contains(RecursionType::LEFT_DIRECT));

      let mut g2 = g.deref().clone();

      let (a, a_prime) = convert_left_recursion_to_right(&mut g2, prod);

      assert!(g2.get_production_recursion_type(prod,).contains(RecursionType::RIGHT));
      assert!(!g2.get_production_recursion_type(prod).contains(RecursionType::LEFT_DIRECT));
    }
  }

  #[test]
  fn processing_of_any_groups() {
    let (grammar, errors) = compile_grammar_from_string(
      "<> A > [ unordered \\g ? ( \\r \\l ) ? ] \\ d ",
      &PathBuf::from("/test"),
    );

    for error in errors.unwrap_or_default() {
      eprintln!("{}", error);
    }

    assert!(grammar.is_some());

    if let Some(mut g) = grammar {
      let prod = g.get_production_id_by_name("A").unwrap();

      // convert_left_to_right(&mut g, prod);

      debug_items("A", get_production_start_items(&prod, &g), &g);
    }
  }
}
