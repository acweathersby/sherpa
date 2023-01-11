use super::utils::{build_grammar_from_file, get_test_grammar_path};
use crate::SherpaError;

#[test]
fn missing_import_production() {
  let (j, _) = build_grammar_from_file(
    get_test_grammar_path("missing_production_import_A.hcg"),
    Default::default(),
  );

  assert!(j.debug_error_report());

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    let error = &r.errors[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "nonexistent-import");
  }));
}
