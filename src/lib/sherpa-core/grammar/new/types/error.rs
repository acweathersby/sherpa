use super::super::parser::{Import, Production_Import_Symbol};
use crate::{
  compile::{GrammarRef, GrammarStore},
  types::{self, SherpaErrorSeverity},
  Journal,
  SherpaError,
};
use sherpa_runtime::types::Token;
use std::path::PathBuf;

/// This error occurs when a values prop's id does not match any reference
/// names or non-terminals in the respective rule.
pub(crate) fn add_invalid_import_source_error(
  j: &mut Journal,
  import: &Import,
  import_path: &PathBuf,
  base_path: &PathBuf,
) {
  let Import { tok, .. } = import;
  j.report_mut().add_error(SherpaError::SourceError {
    loc:        tok.clone(),
    path:       import_path.clone(),
    id:         "invalid-import-source",
    msg:        format!(
      "Could not resolve filepath {}",
      base_path.to_str().unwrap()
    ),
    inline_msg: "source not found".to_string(),
    severity:   SherpaErrorSeverity::Critical,
    ps_msg:     Default::default(),
  });
}

pub fn create_missing_import_name_error(
  j: &mut Journal,
  g: &GrammarStore,
  prod_import_sym: &Production_Import_Symbol,
) {
  j.report_mut().add_error(SherpaError::SourceError {
    loc:        prod_import_sym.tok.clone(),
    path:       g.id.path.clone(),
    id:         "nonexistent-import-production",
    msg:        format!(
      "The production {} cannot be found in the imported grammar {}.",
      prod_import_sym.name, prod_import_sym.module
    ),
    inline_msg: "Could not locate this production".to_string(),
    ps_msg:     Default::default(),
    severity:   SherpaErrorSeverity::Critical,
  });
}

pub fn add_production_redefinition_error(
  j: &mut Journal,
  grammar_path: &PathBuf,
  old_loc: Token,
  new_loc: Token,
  plain_name: &str,
) {
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "production-redefinition",
    sources:  vec![
      (
        old_loc,
        grammar_path.clone(),
        format!("First definition of {} occurs here.", plain_name),
      ),
      (
        new_loc,
        grammar_path.clone(),
        format!("Redefinition of {} occurs here.", plain_name),
      ),
    ],
    msg:      format!("Redefinition of {} is not allowed", plain_name),
    ps_msg:   Default::default(),
    severity: SherpaErrorSeverity::Critical,
  });
}

pub fn add_missing_production_definition_error(
  j: &mut Journal,
  tok: Token,
  g_id: &GrammarRef,
) {
  j.report_mut().add_error(SherpaError::SourceError {
    id:         "missing-production-definition",
    msg:        format!("Could not find a definition for this production."),
    inline_msg: "could not find".to_string(),
    loc:        tok,
    path:       g_id.path.clone(),
    severity:   SherpaErrorSeverity::Critical,
    ps_msg:     "[B]".to_string(),
  });
}

pub fn add_missing_append_host_error(
  j: &mut Journal,
  name: String,
  rules: &[types::Rule],
) {
  j.report_mut().add_error(SherpaError::SourceError {
    id:         "missing-append-host",
    msg:        format!(
      "
Target production for appended rules does not exist.

Append productions must reference an existing production. In this case, the 
production [{0}] should have been defined with a normal production definition 
expression, e.g: `<> {0} > symA ... symN`
",
      name
    ),
    inline_msg: (if rules.len() > 1 {
      "These rules are unreachable"
    } else {
      "This rule is unreachable"
    })
    .to_string(),
    loc:        (&rules[0].tok + &rules.last().unwrap().tok).clone(),
    path:       Default::default(),
    severity:   SherpaErrorSeverity::Critical,
    ps_msg:     Default::default(),
  })
}

pub fn add_non_existent_import_production_error(
  j: &mut Journal,
  import_id: &GrammarRef,
  host_id: &GrammarRef,
  tok: Token,
) {
  j.report_mut().add_error(SherpaError::SourceError {
    id:         "nonexistent-import-production",
    msg:        format!(
      "Could not locate production in imported grammar {}",
      import_id.path.to_str().unwrap()
    ),
    inline_msg: "could not find".to_string(),
    loc:        tok,
    path:       host_id.path.clone(),
    severity:   SherpaErrorSeverity::Critical,
    ps_msg:     Default::default(),
  })
}
