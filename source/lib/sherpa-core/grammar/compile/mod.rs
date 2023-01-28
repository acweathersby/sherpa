pub(crate) mod create_store;
pub mod finalize;
pub mod merge;
pub mod parse;
pub mod parser;
use self::{
  finalize::finalize_grammar,
  merge::merge_grammars,
  parse::load_from_path,
  parser::sherpa,
};
use crate::{
  grammar::compile::{create_store::create_store, parse::grammar_from_string},
  types::*,
  util::get_num_of_available_threads,
  Config,
  Journal,
};
use std::{collections::HashMap, ffi::OsStr, path::PathBuf, sync::Arc};

fn compile_grammars(
  j: &mut Journal,
  grammars: &Vec<(PathBuf, ImportedGrammarReferences, Box<sherpa::Grammar>)>,
) -> SherpaResult<GrammarStore> {
  if grammars.is_empty() {
    j.report_mut().add_error("No grammars were generated.".into());
    SherpaResult::None
  } else {
    let results = std::thread::scope(|s| {
      grammars
        .chunks(
          (grammars.len() as f64 / get_num_of_available_threads() as f64).ceil().max(1.0) as usize
        )
        .into_iter()
        .map(|chunk| {
          let mut j = j.transfer();
          s.spawn(move || {
            chunk
              .iter()
              .map(|(absolute_path, import_refs, grammar)| {
                let (grammar) =
                  create_store(&mut j, &grammar, absolute_path.clone(), import_refs.clone());
                grammar
              })
              .collect::<Vec<_>>()
          })
        })
        .map(|s| s.join().unwrap())
        .collect::<Vec<_>>()
    });
    j.flush_reports();

    if j.have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    let mut grammars: Vec<Arc<GrammarStore>> = results.into_iter().flatten().collect();

    let rest = grammars.drain(1..).collect::<Vec<_>>();

    let mut grammar = Arc::try_unwrap(grammars.pop().unwrap()).unwrap();

    merge_grammars(j, &mut grammar, &rest);

    if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    let grammar = finalize_grammar(j, grammar);

    if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    SherpaResult::Ok(grammar)
  }
}

#[test]
fn grammar_name_from_preamble() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast = grammar_from_string(
    &mut j,
    r#"
    NAME test
    IGNORE { "tacos" }
    <> A > "hello" 
  "world" "#,
    Default::default(),
  );

  let grammar = compile_grammars(&mut j, &ast)?;

  assert_eq!(grammar.id.name, "test");

  SherpaResult::Ok(())
}
#[test]
fn grammar_name_from_path() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast =
    grammar_from_string(&mut j, r#" IGNORE { "a" } <> A > "h"  "w" "#, "/test_path.test".into());

  let grammar = compile_grammars(&mut j, &ast)?;

  assert_eq!(grammar.id.name, "test_path");

  SherpaResult::Ok(())
}

#[test]
fn missing_append_host_error() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast = grammar_from_string(
    &mut j,
    r#"
  NAME test
  IGNORE { "tacos" }
  +> A > "hello"{1} 
       | "goodby"{1} 
  
  "#,
    "/test_path.test".into(),
  );

  compile_grammars(&mut j, &ast);

  j.flush_reports();

  assert!(j.debug_error_report());

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    let error = &r.errors[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "missing-append-host");

    true
  }));

  SherpaResult::Ok(())
}

// Compile v1.0.0 grammar with v1.0.0_strap parser
#[test]
fn compile_latest_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let path = crate::test::utils::path_from_source("grammar/v1_0_0/grammar.sp")?;

  let grammar_data = load_from_path(&mut j, path);

  assert!(!j.debug_error_report());

  dbg!(grammar_data);

  SherpaResult::Ok(())
}
