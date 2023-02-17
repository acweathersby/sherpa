use super::utils::{build_grammar_from_file, get_test_grammar_path};
use crate::{compile::GrammarStore, types::SherpaErrorSeverity, Journal, SherpaError};

/// Error should be generated when an import production is referenced
/// that doesn't exist in the imported grammar.
#[test]
fn missing_import_production() {
  let (j, _) = build_grammar_from_file(
    get_test_grammar_path("errors/nonexistent_import_production/A.sg"),
    Default::default(),
  );

  assert!(j.debug_error_report(), "Expected to see errors");

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    if r.have_errors_of_type(SherpaErrorSeverity::Critical) {
      let error = &r.errors()[0];

      assert!(matches!(error, SherpaError::SourceError { .. }));

      let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
      };

      assert_eq!(*id, "nonexistent-import-production");

      true
    } else {
      false
    }
  }));
}

/// Invalid dependency should be generated when the source for an import
/// grammar cannot be found.
#[test]
fn invalid_dependency() {
  let path = get_test_grammar_path("errors/nonexistent_import_source/A.sg");
  let mut j = Journal::new(None);
  GrammarStore::from_path(&mut j, path.clone());

  j.flush_reports();

  assert!(j.debug_error_report(), "Expected to see errors");

  assert!(j.get_report(crate::ReportType::GrammarCompile((&path).into()), |r| {
    let error = &r.errors()[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "invalid-import-source");

    true
  }));
}

/// Missing production definition is reported
#[test]
fn missing_production_definition() {
  let path = get_test_grammar_path("errors/missing_production_definition.sg");
  let (j, _) = build_grammar_from_file(path.clone(), Default::default());

  assert!(j.debug_error_report(), "Expected to see errors");

  assert!(j.get_report(crate::ReportType::GrammarCompile((&path).into()), |r| {
    let error = &r.errors()[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "missing-production-definition");

    true
  }));
}
