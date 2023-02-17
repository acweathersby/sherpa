use super::types::{AScriptProp, AScriptStore, AScriptTypeVal, TaggedType};
use crate::{types::*, Journal};
use std::collections::BTreeSet;

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
  struct_type: String,
  prop_name: String,
  existing_prop: AScriptProp,
  new_prop: AScriptProp,
) {
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "property-redefinition",
    sources:  vec![
      (
        existing_prop.location.clone(),
        existing_prop.grammar_ref.path.clone(),
        format!("First definition with type [{}]", existing_prop.type_val.debug_string(None)),
      ),
      (
        new_prop.location.clone(),
        new_prop.grammar_ref.path.clone(),
        format!("Second definition with type [{}]", new_prop.type_val.debug_string(None)),
      ),
    ],
    msg:      format!("Redefinition of property {} in struct {}", prop_name, struct_type,),
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}

/// Occurs when a production returns incompatible type values, such numeric values
/// and Structs, or Strings and Tokens.
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
  prod_id: &ProductionId,
  (type_a, rules_a): (AScriptTypeVal, Vec<RuleId>),
  (type_b, rules_b): (AScriptTypeVal, Vec<RuleId>),
) {
  let g = &(ast.g.clone());
  let type_names = ast.get_type_names();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "incompatible-production-scalar-types",
    sources:  rules_a
      .into_iter()
      .map(|r| {
        let rule = g.get_rule(&r).unwrap();
        (
          rule.tok.clone(),
          rule.grammar_ref.path.clone(),
          format!("Rule produces type [{}]", type_a.blame_string(g, &type_names)),
        )
      })
      .chain(rules_b.into_iter().map(|r| {
        let rule = g.get_rule(&r).unwrap();
        (
          rule.tok.clone(),
          rule.grammar_ref.path.clone(),
          format!("Rule produces type [{}]", type_b.blame_string(g, &type_names)),
        )
      }))
      .collect(),
    msg:      format!(
      "Incompatible combination of scalar types are produced by production [{}]",
      g.get_production_plain_name(prod_id)
    ),
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}
/// Occurs when a production returns incompatible vector type values, such as numeric values
/// and Structs, or Strings and Tokens.
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
  prod_id: &ProductionId,
  _types: BTreeSet<TaggedType>,
) {
  let g = &(ast.g.clone());
  let type_names = ast.get_type_names();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "incompatible-production-vector-types",
    sources:  _types
      .iter()
      .map(|t| {
        let rule = g.get_rule(&t.into()).unwrap();
        let type_: AScriptTypeVal = t.into();
        (
          rule.tok.clone(),
          rule.grammar_ref.path.clone(),
          format!("rule produces vector type {}", type_.blame_string(g, &type_names)),
        )
      })
      .collect(),
    msg:      format!(
      "Incompatible combination of vector types are produced by production [{}]",
      g.get_production_plain_name(prod_id)
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
  prod_id: &ProductionId,
  scalar_types: Vec<(AScriptTypeVal, RuleId)>,
  vector_types: Vec<(AScriptTypeVal, RuleId)>,
) {
  let g = &(ast.g.clone());
  let type_names = ast.get_type_names();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "incompatible-production-types",
    sources:  scalar_types
      .iter()
      .map(|(t, r)| {
        let rule = g.get_rule(&r).unwrap();
        (
          rule.tok.clone(),
          rule.grammar_ref.path.clone(),
          format!("Rule reduces to scalar type {}", t.blame_string(g, &type_names)),
        )
      })
      .chain(vector_types.iter().map(|(t, r)| {
        let rule = g.get_rule(&r).unwrap();
        (
          rule.tok.clone(),
          rule.grammar_ref.path.clone(),
          format!("Rule reduces to vector type {}", t.blame_string(g, &type_names)),
        )
      }))
      .collect(),
    msg:      format!(
      "An incompatible combination of vector and scalar types are produced by production [{}]",
      g.get_production_plain_name(prod_id)
    ),
    ps_msg:   "".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}
