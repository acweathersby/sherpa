use super::{super::*, GrammarIdentities};
use crate::types::*;
use radlr_rust_runtime::types::Token;
use std::path::PathBuf;

use ErrorClass::*;

/// This error occurs when source of an imported grammar cannot be found.
pub(crate) fn create_invalid_import_source_error(
  import: &parser::Import,
  import_path: &PathBuf,
  base_path: &PathBuf,
) -> RadlrError {
  let parser::Import { tok, .. } = import;
  RadlrError::SourceError {
    loc:        tok.clone(),
    path:       import_path.to_str().unwrap().to_string(),
    id:         (Imports, 0, "invalid-import-source").into(),
    msg:        format!("Could not resolve filepath {}", base_path.to_str().unwrap()),
    inline_msg: "source not found".to_string(),
    severity:   RadlrErrorSeverity::Critical,
    ps_msg:     Default::default(),
  }
}

pub fn _create_missing_import_name_error(
  g: &GrammarIdentities,
  s_store: &IStringStore,
  nterm_import_sym: &parser::NonTerminal_Import_Symbol,
) -> RadlrError {
  RadlrError::SourceError {
    loc:        nterm_import_sym.tok.clone(),
    path:       g.path.to_string(s_store).into(),
    id:         (Imports, 1, "nonexistent-import-non-terminal").into(),
    msg:        format!(
      "The non-terminal {} cannot be found in the imported grammar {}.",
      nterm_import_sym.name, nterm_import_sym.module
    ),
    inline_msg: "Could not locate this non-terminal".to_string(),
    ps_msg:     Default::default(),
    severity:   RadlrErrorSeverity::Critical,
  }
}

pub fn _create_missing_append_host_error(name: String, rules: &[Rule]) -> RadlrError {
  RadlrError::SourceError {
    id:         (Imports, 2, "missing-append-host").into(),
    msg:        format!(
      "
Target non-terminal for appended rules does not exist.

Append nonterminals must reference an existing non-terminal. In this case, the 
non-terminal [{0}] should have been defined with a normal non-terminal definition 
expression, e.g: `<> {0} > symA ... symN`
",
      name
    ),
    inline_msg: (if rules.len() > 1 { "These rules are unreachable" } else { "This rule is unreachable" }).to_string(),
    loc:        (&rules[0].tok + &rules.last().unwrap().tok).clone(),
    path:       Default::default(),
    severity:   RadlrErrorSeverity::Critical,
    ps_msg:     Default::default(),
  }
}

pub fn _create_non_existent_import_nonterminal_error(
  import_id: &GrammarIdentities,
  host_id: &GrammarIdentities,
  tok: Token,
  s_store: &IStringStore,
) -> RadlrError {
  RadlrError::SourceError {
    id:         (Imports, 3, "nonexistent-import-non-terminal").into(),
    msg:        format!("Could not locate non-terminal in imported grammar {}", import_id.path.to_string(s_store)),
    inline_msg: "could not find".to_string(),
    loc:        tok,
    path:       host_id.path.to_string(s_store).into(),
    severity:   RadlrErrorSeverity::Critical,
    ps_msg:     Default::default(),
  }
}
// #############################################################################
// #################### Grammar Errors

pub fn _create_nonterminal_redefinition_error(
  grammar_path: &PathBuf,
  old_loc: Token,
  new_loc: Token,
  plain_name: &str,
) -> RadlrError {
  RadlrError::SourcesError {
    id:       (Grammar, 0, "non-terminal-redefinition").into(),
    sources:  vec![
      (old_loc, grammar_path.clone(), format!("First definition of {} occurs here.", plain_name)),
      (new_loc, grammar_path.clone(), format!("Redefinition of {} occurs here.", plain_name)),
    ],
    msg:      format!("Redefinition of {} is not allowed", plain_name),
    ps_msg:   Default::default(),
    severity: RadlrErrorSeverity::Critical,
  }
}

pub fn _create_missing_nonterminal_definition_error(tok: Token, g_id: &GrammarIdentities, s_store: &IStringStore) -> RadlrError {
  RadlrError::SourceError {
    id:         (Grammar, 1, "missing-non-terminal-definition").into(),
    msg:        format!("Could not find a definition for this non-terminal."),
    inline_msg: "could not find".to_string(),
    loc:        tok,
    path:       g_id.path.to_string(s_store).into(),
    severity:   RadlrErrorSeverity::Critical,
    ps_msg:     "[B]".to_string(),
  }
}

pub fn create_empty_rule_error(rule: &Rule, s_store: &IStringStore) -> RadlrError {
  RadlrError::SourceError {
    loc:        rule.tok.clone(),
    path:       rule.g_id.path.to_string(s_store),
    id:         (Grammar, 2, "empty-rule-not-allowed").into(),
    msg:        "Rules that can derive the empty rule `{} => ε` are currently not allowed in Radlr Grammars!".into(),
    inline_msg: "This symbol is optional leads to a derivation of this rule that lacks any symbols".into(),
    ps_msg:     "Consider changing this rule to (+)".into(),
    severity:   RadlrErrorSeverity::Critical,
  }
}

pub fn create_invalid_nonterminal_alias(loc: Token, path: IString, s_store: &IStringStore) -> RadlrError {
  RadlrError::SourceError {
    loc,
    path: path.to_string(s_store),
    id: (Grammar, 3, "aliased-nonterminal-rule-definition").into(),
    inline_msg: Default::default(),
    msg: "Can not resolve grammar that has non-terminal definitions and state definitions with the same name: ".to_string(),
    ps_msg: Default::default(),
    severity: RadlrErrorSeverity::Critical,
  }
}

/// Emitted if the definition of a non-terminal (its production rules) could not
/// be found.
pub fn create_missing_nonterminal_rules(loc: Token, path: IString, s_store: &IStringStore) -> RadlrError {
  RadlrError::SourceError {
    loc,
    path: path.to_string(s_store),
    id: (Grammar, 4, "missing-nonterminal-rules").into(),
    inline_msg: Default::default(),
    msg: "Cannot find definition for non-terminal".into(),
    ps_msg: Default::default(),
    severity: RadlrErrorSeverity::Critical,
  }
}
