#![allow(unused)]
use sherpa_core::{parser::AST_Property, *};
use sherpa_rust_runtime::types::BlameColor;

use super::types::{AScriptProp, AScriptStore, AScriptTypeVal, TaggedType};
use crate::types::*;
use std::{collections::BTreeSet, path::PathBuf};

/// This error occurs when multiple definitions of the same Struct
/// define the same property with different types.
///
/// # Example
/// ```hcg
/// 
/// <> A > ... :{ t_TypeA, prop: str } // <- `prop` defined as `str` type
///
/// <> B > ... :{ t_TypeA, prop: u32 } // <- `prop` redefined to `u32` type
/// ```
/// ### Rust
/// ```ignore
/// # use sherpa_core::types::*;
/// # use sherpa_compile::types::AScriptStore;
/// let g = GrammarStore::from_str(
/// "
/// <> A > \\b f:ast { { t_TypeA, prop: str } }
///
/// <> B > \\a f:ast { { t_TypeA, prop: u32 } }
/// "
/// )?;
///
/// AScriptStore::new(g).unwrap();
///
/// # Ok(())
/// ```
pub(crate) fn add_prop_redefinition_error(
  j: &mut Journal,
  db: &ParserDatabase,
  struct_type: String,
  prop_name: String,
  existing_prop: AScriptProp,
  new_prop: AScriptProp,
) {
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "property-redefinition",
    sources:  vec![
      (
        existing_prop.loc.clone(),
        PathBuf::from(existing_prop.grammar_ref.path.to_string(db.string_store())),
        "First definition with type [".to_string() + &existing_prop.type_val.debug_string() + "]",
      ),
      (
        new_prop.loc.clone(),
        PathBuf::from(new_prop.grammar_ref.path.to_string(db.string_store())),
        "Second definition with type [".to_string() + &new_prop.type_val.debug_string() + "]",
      ),
    ],
    msg:      "Redefinition of property ".to_string() + &prop_name + " in struct " + &struct_type,
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}

/// Occurs when a production returns incompatible type values, such numeric
/// values and Structs, or Strings and Tokens.
///
/// # Example
/// ### HC Grammar
/// ```hcg
/// 
/// <> A > \\r :{ t_TypeA, prop: str }  // <- This rule produces a struct
///      | \\t                        // <- This rule produces a Token
/// ```
/// ### Rust
/// ```ignore
/// # use sherpa_core::types::*;
/// # use sherpa_compile::types::AScriptStore;
/// let g = GrammarStore::from_str(
/// "<> A > \\r f:ast { { t_TypeA, prop: str } }
///      | \\t
/// "
/// )?;
///
/// AScriptStore::new(g).unwrap();
///
/// # Ok(())
/// ```
pub(crate) fn add_incompatible_production_scalar_types_error(
  j: &mut Journal,
  ast: &mut AScriptStore,
  db: &ParserDatabase,
  prod_id: &DBProdKey,
  (type_a, rules_a): (AScriptTypeVal, Vec<DBRuleKey>),
  (type_b, rules_b): (AScriptTypeVal, Vec<DBRuleKey>),
) {
  let type_names = ast.get_type_names();

  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "incompatible-production-scalar-types [401]",
    sources:  rules_a
      .into_iter()
      .map(|r| {
        let rule = db.rule(r);
        (
          rule.tok.clone(),
          PathBuf::from(rule.g_id.path.clone().to_string(db.string_store())),
          format!("Rule produces type [{}]", type_a.blame_string(&type_names)),
        )
      })
      .chain(rules_b.into_iter().map(|r| {
        let rule = db.rule(r);
        (
          rule.tok.clone(),
          PathBuf::from(rule.g_id.path.clone().to_string(db.string_store())),
          format!("Rule produces type [{}]", type_b.blame_string(&type_names)),
        )
      }))
      .collect(),
    msg:      format!(
      "Incompatible combination of scalar types are produced by production [{}]",
      db.prod_friendly_name_string(*prod_id)
    ),
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}
/// Occurs when a production returns incompatible vector type values, such as
/// numeric values and Structs, or Strings and Tokens.
///
/// # Example
///
/// ### HC Grammar
/// ```hcg
/// <> A > (\r :{ t_TypeA, prop: str })(+)  // <- This rule produces a struct Vector
///
///      | (\t )(+)                       // <- This rule produces a Token Vector
/// ```
/// ### Rust
/// ```ignore
/// # use sherpa_core::types::*;
/// # use sherpa_compile::types::AScriptStore;
/// let g = GrammarStore::from_str(
/// " <> A > ( \\r f:ast{ { t_TypeA, prop:$1 } } )(+)
///      | (\\t )(+)
/// "
/// )?;
///
/// AScriptStore::new(g).unwrap();
///
/// # Ok(())
/// ```

pub(crate) fn add_incompatible_production_vector_types_error(
  j: &mut Journal,
  ast: &mut AScriptStore,
  db: &ParserDatabase,
  prod_id: &DBProdKey,
  _types: BTreeSet<TaggedType>,
) {
  let type_names = ast.get_type_names();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "incompatible-production-vector-types",
    sources:  _types
      .iter()
      .map(|t| {
        let rule = db.rule(t.tag);
        let type_: AScriptTypeVal = t.into();
        (
          rule.tok.clone(),
          PathBuf::from(rule.g_id.path.clone().to_string(db.string_store())),
          format!("rule produces vector type {}", type_.blame_string(&type_names)),
        )
      })
      .collect(),
    msg:      format!(
      "Incompatible combination of vector types are produced by production [{}]",
      db.prod_friendly_name_string(*prod_id)
    ),
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}

/// This error occurs when there are a mixture of Vector and Scalar return
/// types on a production.
///
/// # Example
/// ### HC Grammar
/// ```hcg
/// 
/// <> A > \r :{ t_TypeA, prop: str }  // <- This rule produces a struct
///      | \t (+)                    // <- This rule produces a Vector of Tokens
/// ```
/// ### Rust
/// ```ignore
/// # use sherpa_core::types::*;
/// # use sherpa_compile::types::AScriptStore;
/// let g = GrammarStore::from_str(
/// "<> A > \\r f:ast { { t_TypeA, prop: str } }
///      | \\t (+)
/// "
/// )?;
///
/// AScriptStore::new(g).unwrap();
///
/// # Ok(())
/// ```
pub(crate) fn add_incompatible_production_types_error(
  j: &mut Journal,
  ast: &mut AScriptStore,
  db: &ParserDatabase,
  prod_id: &DBProdKey,
  scalar_types: Vec<(AScriptTypeVal, DBRuleKey)>,
  vector_types: Vec<(AScriptTypeVal, DBRuleKey)>,
) {
  let type_names = ast.get_type_names();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "incompatible-production-types",
    sources:  scalar_types
      .iter()
      .map(|(t, r)| {
        let rule = db.rule(*r);
        (
          rule.tok.clone(),
          PathBuf::from(rule.g_id.path.clone().to_string(db.string_store())),
          format!("Rule reduces to scalar type {}", t.blame_string(&type_names)),
        )
      })
      .chain(vector_types.iter().map(|(t, r)| {
        let rule = db.rule(*r);
        (
          rule.tok.clone(),
          PathBuf::from(rule.g_id.path.clone().to_string(db.string_store())),
          format!("Rule reduces to vector type {}", t.blame_string(&type_names)),
        )
      }))
      .collect(),
    msg:      format!(
      "An incompatible combination of vector and scalar types are produced by production [{}]",
      db.prod_friendly_name_string(*prod_id)
    ),
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}

/// This error occurs when a values prop's id does not match any reference
/// names or non-terminals in the respective rule.
pub(crate) fn add_unmatched_prop_error(
  j: &mut Journal,
  rule: &Rule,
  db: &ParserDatabase,
  prop: &AST_Property,
) {
  j.report_mut().add_error(SherpaError::SourceError {
    id:         "unmatched-valueless-prop",
    path:       PathBuf::from(rule.g_id.path.clone().to_string(db.string_store())),
    inline_msg: Default::default(),
    loc:        prop.tok.clone(),
    msg:        format!("This property does nor resolve to a symbol within it's associated rule",),
    ps_msg:     format!(
      "{}",
      rule.tok.blame(
        0,
        0,
        "\nThis prop should reference either a non-terminal or symbol reference within this rule",
        BlameColor::RED,
      )
    ),
    severity:   SherpaErrorSeverity::Critical,
  });
}
