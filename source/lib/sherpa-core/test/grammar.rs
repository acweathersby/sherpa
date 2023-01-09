use crate::{
  compile::{
    compile_ascript_ast,
    compile_bytecode,
    compile_grammar_ast,
    compile_ir_ast,
    compile_states,
    compile_token_production_states,
    optimize_ir_states,
  },
  debug::{debug_items, generate_disassembly},
  grammar::{
    compile::{compile_grammars_into_store, convert_left_recursion_to_right, pre_process_grammar},
    compile_grammar_from_path,
    compile_grammar_from_string,
    get_production_start_items,
    load::{load_all, load_grammar, resolve_grammar_path},
  },
  journal::{report::ReportType, Journal, *},
  types::{RecursionType, SherpaErrorContainer, *},
  util::get_num_of_available_threads,
};
use lazy_static::__Deref;
use std::{io::Write, path::PathBuf, sync::Arc};

#[test]
fn test_load_all() {
  let mut j = Journal::new(None);
  let (grammars, errors) = load_all(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  for err in &errors {
    eprintln!("{}", err);
  }
  assert_eq!(grammars.len(), 2);
  assert_eq!(errors.len(), 0);

  let (grammars, errors) = load_all(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/invalid_load.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  for err in &errors {
    eprintln!("{}", err);
  }
  assert_eq!(grammars.len(), 1);
  assert_eq!(errors.len(), 1);
}

#[test]
fn test_load_grammar() {
  let mut j = Journal::new(None);
  let result = load_grammar(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test_grammars/load.hcg"),
  );

  assert!(result.is_faulty());

  let result = load_grammar(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.hcg")
      .canonicalize()
      .unwrap(),
  );

  assert!(result.is_ok());
}

#[test]
fn test_resolve_cargo_file() {
  let result = resolve_grammar_path(&Default::default(), &Default::default(), &[]);

  assert!(result.is_faulty());

  let result = resolve_grammar_path(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml"),
    &Default::default(),
    &[],
  );

  assert!(result.is_ok());

  let result = resolve_grammar_path(
    &PathBuf::from("./Cargo.toml"),
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")),
    &[],
  );

  assert!(result.is_ok());

  let result =
    resolve_grammar_path(&PathBuf::from("./Cargo"), &PathBuf::from(env!("CARGO_MANIFEST_DIR")), &[
      "toml",
    ]);

  assert!(result.is_ok());
}

#[test]
fn test_ir_trivial_state() {
  let input = String::from("state [ A ] \n pass");

  let result = compile_ir_ast(Vec::from(input.as_bytes()));

  assert!(result.is_ok());
}

#[test]

fn test_ir_trivial_branch_state() {
  let input = String::from("state [ A ] assert TOKEN [ /* a */ 11 ] ( pass )");

  let result = compile_ir_ast(Vec::from(input.as_bytes()));

  assert!(result.is_ok());

  print!("{:#?}", result.unwrap());
}

#[test]

fn test_trivial_ascript_struct() {
  let input = String::from(" { t_Type, t_Class, value: u32 } ");

  let result = compile_ascript_ast(Vec::from(input.as_bytes()));

  print!("{:#?}", result);

  assert!(result.is_ok());
}

#[test]

fn test_production_minimum() {
  let input = String::from("\n<> a > b\n");

  let result = compile_grammar_ast(Vec::from(input.as_bytes()));

  assert!(result.is_ok());
}

#[test]

fn test_production_with_generic_symbol() {
  let input = String::from("\n<> a > g:sp\n");

  let result = compile_grammar_ast(Vec::from(input.as_bytes()));

  assert!(result.is_ok());
}

#[test]
fn test_merge_productions_file() {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

  path.push("../../../test/grammars/merge_root.hcg");

  let (grammar, errors) = compile_grammar_from_path(&mut j, path, get_num_of_available_threads());

  j.flush_reports();

  assert_eq!(j.debug_error_report(), false);

  assert!(grammar.is_some());
}

#[test]
fn test_trivial_file_compilation() {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

  path.push("../../../test/grammars/trivial.hcg");

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

#[test]
fn pptest_compile_grammars_into_store() {
  let mut j = Journal::new(None);
  let (grammars, errors) = load_all(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  let result = compile_grammars_into_store(&mut j, grammars);

  j.flush_reports();

  assert_eq!(j.debug_error_report(), false);

  assert!(result.is_ok());

  match result {
    SherpaResult::Ok((Some(grammar), _)) => {
      dbg!(grammar);
    }
    SherpaResult::Ok((_, Some(errors))) => {
      for err in errors {
        eprintln!("{}", err);
      }
      panic!("Errors occurred while compiling")
    }
    _ => {}
  }
}

#[test]
fn test_get_default_production() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
@EXPORT start as test

<> start > \\hello \\and end

<> end > \\goodby
",
  )
  .unwrap();

  let exported_productions = g.get_exported_productions();
  let first = exported_productions.first().unwrap();

  assert_eq!(first.production.name, "start");

  assert_eq!(first.export_name, "test");
}

#[test]

fn test_get_production_plain_name() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<>billofolious_tantimum^a>\\o").unwrap();

  let prod = g.get_production_id_by_name("billofolious_tantimum").unwrap();

  assert_eq!(g.get_production_plain_name(&prod), "billofolious_tantimum");

  assert_ne!(g.get_production(&prod).unwrap().guid_name, "billofolious_tantimum");
}

#[test]

fn test_get_production_by_name() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
      <> Apple > \\o
      <> Bad_Cakes > \\b
      ",
  )
  .unwrap();

  assert!(g.get_production_id_by_name("Apple").is_some());

  assert!(g.get_production_id_by_name("Bad_Cakes").is_some());

  assert!(g.get_production_id_by_name("Bandible").is_none());
}

#[test]

fn test_is_production_recursive() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
      <> A > B 
      <> B > C
      <> C > D
      <> D > E
      <> E > B A R | \\e
      <> R > R A | R B O | \\r
      <> O > \\o
      ",
  )
  .unwrap();

  let production = g.get_production_id_by_name("A").unwrap();

  assert_eq!(g.get_production_recursion_type(production), RecursionType::RIGHT);

  let production = g.get_production_id_by_name("R").unwrap();

  assert!(g
    .get_production_recursion_type(production)
    .contains(RecursionType::LEFT_DIRECT | RecursionType::RIGHT));

  let production = g.get_production_id_by_name("B").unwrap();

  assert!(g
    .get_production_recursion_type(production)
    .contains(RecursionType::LEFT_INDIRECT | RecursionType::RIGHT));

  let production = g.get_production_id_by_name("C").unwrap();

  assert!(g
    .get_production_recursion_type(production)
    .contains(RecursionType::LEFT_INDIRECT | RecursionType::RIGHT));

  let production = g.get_production_id_by_name("O").unwrap();

  assert_eq!(g.get_production_recursion_type(production), RecursionType::NONE);
}
