use super::compile_grammar_from_string;
use crate::{
  debug::debug_items,
  get_num_of_available_threads,
  grammar::{
    compile::{compile_grammars_into_store, convert_left_recursion_to_right},
    compile_grammar_from_path,
    get_production_start_items,
    load::load_all,
  },
  journal::{report::ReportType, Journal},
  types::{RecursionType, SherpaErrorContainer, *},
};
use lazy_static::__Deref;
use std::{path::PathBuf, sync::Arc};

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
    SherpaResult::Ok((Some(grammar), _)) => {
      //  dbg!(grammar);
    }
    SherpaResult::Ok((_, Some(errors))) => {
      for err in errors {
        println!("{}", err);
      }
      panic!("Errors occurred while compiling")
    }
    _ => {}
  }
}

#[test]
fn test_merge_productions_file() {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

  path.push("../../../test/grammars/merge_root.hcg");

  let thread_count = get_num_of_available_threads();

  let (grammar, errors) = compile_grammar_from_path(&mut j, path, 1);

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
