use super::types::{AScriptProp, AScriptStructId, AScriptTypeVal, TaggedType};
use hctk_core::*;
use std::{collections::BTreeMap, sync::Arc};

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
/// ```
/// # use hctk_core::types::*;
/// # use hctk_compile::types::AScriptStore;
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
#[derive(Debug)]
pub struct ErrPropRedefinition {
  struct_type:   String,
  prop_name:     String,
  existing_prop: AScriptProp,
  new_prop:      AScriptProp,
}

impl ErrPropRedefinition {
  pub const friendly_name: &'static str = "AScript: Structure Property Redefinition";

  pub fn new(
    struct_type: String,
    prop_name: String,
    existing_prop: AScriptProp,
    new_prop: AScriptProp,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { struct_type, prop_name, existing_prop, new_prop }))
  }
}

impl ExtendedError for ErrPropRedefinition {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrPropRedefinition { existing_prop, new_prop, prop_name, struct_type } = self;
    f.write_fmt(format_args!(
      "Redefinition of the property {} in struct {}:
- First definition of type [{}] encountered here:
  [{}]
{}
- Second definition of type [{}] encountered here:
  [{}]
{}",
      prop_name,
      struct_type,
      existing_prop.type_val.debug_string(None),
      existing_prop.location.path_ref(&existing_prop.grammar_ref.path),
      existing_prop.location.blame(0, 0, "", BlameColor::Red),
      new_prop.type_val.debug_string(None),
      new_prop.location.path_ref(&new_prop.grammar_ref.path),
      new_prop.location.blame(0, 0, "", BlameColor::Red)
    ))
  }

  fn severity(&self) -> HCErrorSeverity {
    HCErrorSeverity::Critical
  }

  fn friendly_name(&self) -> &str {
    ErrPropRedefinition::friendly_name
  }
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
/// ```
/// # use hctk_core::types::*;
/// # use hctk_compile::types::AScriptStore;
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
#[derive(Debug)]
pub struct ErrUnionOfScalarsAndVectors {
  scalars:    Vec<(AScriptTypeVal, RuleId)>,
  vectors:    Vec<(AScriptTypeVal, RuleId)>,
  p:          ProductionId,
  g:          Arc<GrammarStore>,
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrUnionOfScalarsAndVectors {
  pub const friendly_name: &'static str = "AScript: Vector / Scalar Collision";

  pub fn new(
    grammar: Arc<GrammarStore>,
    prod_id: ProductionId,
    scalars: Vec<(AScriptTypeVal, RuleId)>,
    vectors: Vec<(AScriptTypeVal, RuleId)>,
    type_names: Arc<BTreeMap<AScriptStructId, String>>,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { scalars, vectors, p: prod_id, g: grammar, type_names }))
  }
}

impl ExtendedError for ErrUnionOfScalarsAndVectors {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrUnionOfScalarsAndVectors { g, p, scalars, vectors, type_names } = self;

    let create_blame_string = |(type_, rule_id): &(AScriptTypeVal, RuleId)| -> String {
      let rule = g.get_rule(rule_id).unwrap();
      format!(
        " -- [{}]\n    Body [{}]\n    reduces to [{}] type:\n\n{}",
        rule.tok.path_ref(&rule.grammar_ref.path),
        rule.item().blame_string(g),
        type_.blame_string(g, type_names),
        rule.tok.blame(0, 0, "defined here", BlameColor::Blue)
      )
    };

    f.write_fmt(format_args!(
      "[{}] produces an incompatible set of Vector and Scalar types:
{}
Scalar Types:
{}
Vector Types:
{}
",
      g.get_production_plain_name(p),
      g.get_production(p).unwrap().loc.blame(0, 0, "production defined here", BlameColor::Red),
      scalars.iter().map(create_blame_string).collect::<Vec<_>>().join("\n"),
      vectors.iter().map(create_blame_string).collect::<Vec<_>>().join("\n")
    ))
  }

  fn severity(&self) -> HCErrorSeverity {
    HCErrorSeverity::Critical
  }

  fn friendly_name(&self) -> &str {
    ErrUnionOfScalarsAndVectors::friendly_name
  }
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
/// ```
/// # use hctk_core::types::*;
/// # use hctk_compile::types::AScriptStore;
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
#[derive(Debug)]
pub struct ErrIncompatibleProductionScalerTypes {
  p:          ProductionId,
  g:          Arc<GrammarStore>,
  type_a:     (AScriptTypeVal, Vec<RuleId>),
  type_b:     (AScriptTypeVal, Vec<RuleId>),
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrIncompatibleProductionScalerTypes {
  pub const friendly_name: &'static str = "AScript: Incompatible Scalar Types";

  pub fn new(
    p: ProductionId,
    g: Arc<GrammarStore>,
    type_a: (AScriptTypeVal, Vec<RuleId>),
    type_b: (AScriptTypeVal, Vec<RuleId>),
    type_names: Arc<BTreeMap<AScriptStructId, String>>,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { p, g, type_a, type_b, type_names }))
  }
}

impl ExtendedError for ErrIncompatibleProductionScalerTypes {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrIncompatibleProductionScalerTypes {
      p,
      g,
      type_a: (type_a, a_bodies),
      type_b: (type_b, b_bodies),
      type_names,
    } = self;

    let body_draw = |&b| {
      let rule = g.get_rule(&b).unwrap();
      format!(
        "[{}]\n{}\n{}",
        rule.tok.path_ref(&rule.grammar_ref.path),
        rule.item().blame_string(g),
        rule.tok.blame(0, 0, "", BlameColor::Red)
      )
    };

    f.write_fmt(format_args!(
      "[{}] produces an incompatible set of scalar types:
- Type [{}] is produced by the following bodies:
{}
- Type [{}] is produced by the following bodies:
{}",
      g.get_production_plain_name(p),
      type_a.blame_string(g, type_names),
      a_bodies.iter().map(body_draw).collect::<Vec<_>>().join("\n"),
      type_b.blame_string(g, type_names),
      b_bodies.iter().map(body_draw).collect::<Vec<_>>().join("\n")
    ))
  }

  fn severity(&self) -> HCErrorSeverity {
    HCErrorSeverity::Critical
  }

  fn friendly_name(&self) -> &str {
    ErrIncompatibleProductionScalerTypes::friendly_name
  }
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
/// ```
/// # use hctk_core::types::*;
/// # use hctk_compile::types::AScriptStore;
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
#[derive(Debug)]
pub struct ErrIncompatibleProductionVectorTypes {
  p: ProductionId,
  g: Arc<GrammarStore>,
  vector_types: Vec<TaggedType>,
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrIncompatibleProductionVectorTypes {
  pub const friendly_name: &'static str = "AScript: Incompatible Vector Types";

  pub fn new(
    p: ProductionId,
    g: Arc<GrammarStore>,
    vector_types: Vec<TaggedType>,
    type_names: Arc<BTreeMap<AScriptStructId, String>>,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { p, g, vector_types, type_names }))
  }
}

impl ExtendedError for ErrIncompatibleProductionVectorTypes {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrIncompatibleProductionVectorTypes { p, g, vector_types, type_names } = self;

    let body_draw = |b: &TaggedType| {
      let rule = g.get_rule(&b.into()).unwrap();
      let type_: AScriptTypeVal = b.into();
      format!(
        "[{}]\n rule {}\n produces vector type [{}] \n{}",
        rule.tok.path_ref(&rule.grammar_ref.path),
        rule.item().blame_string(g),
        type_.blame_string(g, type_names),
        rule.tok.blame(0, 0, "defined here", BlameColor::Red)
      )
    };

    f.write_fmt(format_args!(
      "Incompatible vector types are produced by production [{}]:\n\n{}",
      g.get_production_plain_name(p),
      vector_types.iter().map(body_draw).collect::<Vec<_>>().join("\n"),
    ))
  }

  fn severity(&self) -> HCErrorSeverity {
    HCErrorSeverity::Critical
  }

  fn friendly_name(&self) -> &str {
    ErrIncompatibleProductionVectorTypes::friendly_name
  }
}
