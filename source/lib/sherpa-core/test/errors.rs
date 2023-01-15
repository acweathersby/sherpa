use super::utils::{build_grammar_from_file, get_test_grammar_path};
use crate::SherpaError;

/// Error should be generated when an import production is referenced
/// that doesn't exist in the imported grammar.
#[test]
fn missing_import_production() {
  let (j, _) = build_grammar_from_file(
    get_test_grammar_path("errors/nonexistent_import_production/A.hcg"),
    Default::default(),
  );

  assert!(j.debug_error_report());

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    let error = &r.errors[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "nonexistent-import-production");
  }));
}

/// Invalid dependency should be generated when the source for an import
/// grammar cannot be found.
#[test]
fn invalid_dependency() {
  let (j, _) = build_grammar_from_file(
    get_test_grammar_path("errors/nonexistent_import_source/A.hcg"),
    Default::default(),
  );

  assert!(j.debug_error_report());

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    let error = &r.errors[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "nonexistent-import-source");
  }));
}
