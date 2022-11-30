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

use std::{path::PathBuf, sync::Arc};

pub(crate) use compile::get_scanner_info_from_defined;
pub use item::*;
pub use production::*;
pub use uuid::*;

use self::{compile::compile_grammars_into_store, load::load_all, parse::compile_grammar_ast};
use crate::{
  journal::{report::ReportType, Journal},
  types::*,
};

#[test]
fn temp_test() {
  let mut j = Journal::new(None);
  let (grammars, errors) = load_all(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/e2e/bootstrap/grammar/production.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  if errors.len() > 0 {
    for error in errors {
      println!("{}", error);
    }
  }

  let result = compile_grammars_into_store(&mut j, grammars);

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
  j: &mut Journal,
  path: PathBuf,
  thread_count: usize,
) -> (Option<Arc<GrammarStore>>, Option<Vec<HCError>>) {
  j.set_active_report("General Grammar Compile", ReportType::GrammarCompile(Default::default()));
  j.report_mut().start_timer("Grammar Compile Time");
  j.report_mut().add_note("Root Grammar Path", path.to_str().unwrap().to_string());
  match load_all(j, &path, thread_count) {
    (_, errors) if !errors.is_empty() => {
      j.report_mut().stop_timer("Grammar Compile Time");
      (None, Some(errors))
    }
    (grammars, _) => {
      j.report_mut().stop_timer("Grammar Compile Time");
      compile_grammars_into_store(j, grammars).unwrap()
    }
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
  j: &mut Journal,
  string: &str,
  absolute_path: &PathBuf,
) -> (Option<Arc<GrammarStore>>, Option<Vec<HCError>>) {
  j.set_active_report("General Grammar Compiler", ReportType::GrammarCompile(Default::default()));
  j.report_mut().start_timer("Compile");
  j.report_mut().add_note("Source", string.to_string());
  match compile_grammar_ast(Vec::from(string.as_bytes())) {
    Ok(grammar) => {
      j.report_mut().stop_timer("Compile");
      compile_grammars_into_store(j, vec![(absolute_path.clone(), Default::default(), grammar)])
        .unwrap()
    }
    Err(err) => {
      j.report_mut().stop_timer("Compile");
      (None, Some(vec![err]))
    }
  }
}

#[cfg(test)]
mod test_grammar {

  use std::path::PathBuf;

  use lazy_static::__Deref;

  use crate::{
    debug::debug_items,
    get_num_of_available_threads,
    grammar::compile_grammar_from_path,
    journal::Journal,
    types::{HCErrorContainer, RecursionType},
  };

  use super::{
    compile::convert_left_recursion_to_right,
    compile_grammar_from_string,
    get_production_start_items,
  };

  #[test]
  fn test_merge_productions_file() {
    let mut j = Journal::new(None);
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/merge_root.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammar, errors) = compile_grammar_from_path(&mut j, path, get_num_of_available_threads());

    if errors.is_some() {
      let errors = errors.unwrap_or_default();
      for error in &errors {
        eprintln!("{}", error);
      }

      if errors.have_critical() {
        panic!("Critical Errors encountered");
      }
    }

    assert!(grammar.is_some());
  }

  #[test]
  fn test_trivial_file_compilation() {
    let mut j = Journal::new(None);
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/trivial.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammar, errors) = compile_grammar_from_path(&mut j, path, get_num_of_available_threads());

    assert!(grammar.is_some());
    assert!(errors.is_none());
  }

  #[test]
  fn test_trivial_file_compilation_with_single_import() {
    let mut j = Journal::new(None);
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/trivial_importer.hcg");

    let (grammar, errors) = compile_grammar_from_path(&mut j, path, get_num_of_available_threads());

    assert!(grammar.is_some());
    assert!(errors.is_none());
    assert_eq!(grammar.unwrap().imports.len(), 1);
  }

  #[test]
  fn conversion_of_left_to_right_recursive() {
    let mut j = Journal::new(None);
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("../../../test/grammars/left_recursive_token_production.hcg");

    let thread_count = get_num_of_available_threads();

    let (grammar, errors) = compile_grammar_from_path(&mut j, path, get_num_of_available_threads());

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
    let mut j = Journal::new(None);
    if let Some(mut g) = compile_grammar_from_string(
      &mut j,
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
    let mut j = Journal::new(None);
    let (grammar, errors) = compile_grammar_from_string(
      &mut j,
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
