use std::collections::BTreeMap;
use std::fmt::format;
use std::sync::Arc;

use hctk_core::types::*;

use super::types::AScriptProp;
use super::types::AScriptStructId;
use super::types::AScriptTypeVal;

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
#[derive(Debug)]
pub struct ErrPropRedefinition {
  struct_type:   String,
  prop_name:     String,
  existing_prop: AScriptProp,
  new_prop:      AScriptProp,
}

impl ErrPropRedefinition {
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
}

/// This error occurs when there are a mixture of Vector and Scalar return
/// types on a production.
///
/// # Example
/// ```hcg
/// 
/// <> A > \\a \\b // <- This body produces scalar type [Token]
///    
///    | \\c (+)   // <- This body produces vector type [Vector of [Token]]
/// ```
#[derive(Debug)]
pub struct ErrUnionOfScalarsAndVectors {
  scalars:    Vec<(AScriptTypeVal, BodyId)>,
  vectors:    Vec<(AScriptTypeVal, BodyId)>,
  p:          ProductionId,
  g:          Arc<GrammarStore>,
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrUnionOfScalarsAndVectors {
  pub fn new(
    grammar: Arc<GrammarStore>,
    prod_id: ProductionId,
    scalars: Vec<(AScriptTypeVal, BodyId)>,
    vectors: Vec<(AScriptTypeVal, BodyId)>,
    type_names: Arc<BTreeMap<AScriptStructId, String>>,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { scalars, vectors, p: prod_id, g: grammar, type_names }))
  }
}

impl ExtendedError for ErrUnionOfScalarsAndVectors {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrUnionOfScalarsAndVectors { g, p, scalars, vectors, type_names } = self;

    let create_blame_string = |(type_, body_id): &(AScriptTypeVal, BodyId)| -> String {
      let body = g.get_body(body_id).unwrap();
      format!(
        " -- [{}]\n    Body [{}]\n    reduces to [{}] type:\n\n{}",
        body.tok.path_ref(&body.grammar_ref.path),
        body.item().blame_string(g),
        type_.blame_string(g, type_names),
        body.tok.blame(0, 0, "defined here", BlameColor::Blue)
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
      g.get_production(p).unwrap().tok.blame(0, 0, "production defined here", BlameColor::Red),
      scalars.iter().map(create_blame_string).collect::<Vec<_>>().join("\n"),
      vectors.iter().map(create_blame_string).collect::<Vec<_>>().join("\n")
    ))
  }

  fn severity(&self) -> HCErrorSeverity {
    HCErrorSeverity::Critical
  }
}

/// Occurs when a production returns incompatible type values, such numeric values
/// and Structs, or Strings and Tokens.
///
/// # Example
/// ```hcg
/// 
/// <> A > B :{ t_TypeA, prop: str }  // <- This body produces a struct
///      | \\t                        // <- This body produces a Token
/// ```
#[derive(Debug)]
pub struct ErrIncompatibleProductionVectorScalar {
  p:          ProductionId,
  g:          Arc<GrammarStore>,
  type_a:     (AScriptTypeVal, Vec<BodyId>),
  type_b:     (AScriptTypeVal, Vec<BodyId>),
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrIncompatibleProductionVectorScalar {
  pub fn new(
    p: ProductionId,
    g: Arc<GrammarStore>,
    type_a: (AScriptTypeVal, Vec<BodyId>),
    type_b: (AScriptTypeVal, Vec<BodyId>),
    type_names: Arc<BTreeMap<AScriptStructId, String>>,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { p, g, type_a, type_b, type_names }))
  }
}

impl ExtendedError for ErrIncompatibleProductionVectorScalar {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrIncompatibleProductionVectorScalar {
      p,
      g,
      type_a: (type_a, a_bodies),
      type_b: (type_b, b_bodies),
      type_names,
    } = self;

    let body_draw = |&b| {
      let body = g.get_body(&b).unwrap();
      format!(
        "[{}]\n{}\n{}",
        body.tok.path_ref(&body.grammar_ref.path),
        body.item().blame_string(g),
        body.tok.blame(0, 0, "", BlameColor::Red)
      )
    };

    f.write_fmt(format_args!(
      "[{}] produces an incompatible set of types:
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
}

/// Occurs when a production returns incompatible type values, such numeric values
/// and Structs, or Strings and Tokens.
///
/// # Example
/// ```hcg
/// 
/// <> A > B :{ t_TypeA, prop: str }  // <- This body produces a struct
///      | \\t                        // <- This body produces a Token
/// ```
#[derive(Debug)]
pub struct ErrIncompatibleProductionScalerTypes {
  p:          ProductionId,
  g:          Arc<GrammarStore>,
  type_a:     (AScriptTypeVal, Vec<BodyId>),
  type_b:     (AScriptTypeVal, Vec<BodyId>),
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrIncompatibleProductionScalerTypes {
  pub fn new(
    p: ProductionId,
    g: Arc<GrammarStore>,
    type_a: (AScriptTypeVal, Vec<BodyId>),
    type_b: (AScriptTypeVal, Vec<BodyId>),
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
      let body = g.get_body(&b).unwrap();
      format!(
        "[{}]\n{}\n{}",
        body.tok.path_ref(&body.grammar_ref.path),
        body.item().blame_string(g),
        body.tok.blame(0, 0, "", BlameColor::Red)
      )
    };

    f.write_fmt(format_args!(
      "[{}] produces an incompatible set of vector types:
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
}

/// Occurs when a production returns incompatible vector type values, such numeric values
/// and Structs, or Strings and Tokens.
///
/// # Example
/// ```hcg
/// 
/// <> A > (B :{ t_TypeA, prop: str })(+)  // <- This body produces a struct Vector
///      | (\\t )(+)                       // <- This body produces a Token Vector
/// ```
#[derive(Debug)]
pub struct ErrIncompatibleProductionVectorTypes {
  p:          ProductionId,
  g:          Arc<GrammarStore>,
  type_a:     (AScriptTypeVal, Vec<BodyId>),
  type_b:     (AScriptTypeVal, Vec<BodyId>),
  type_names: Arc<BTreeMap<AScriptStructId, String>>,
}

impl ErrIncompatibleProductionVectorTypes {
  pub fn new(
    p: ProductionId,
    g: Arc<GrammarStore>,
    type_a: (AScriptTypeVal, Vec<BodyId>),
    type_b: (AScriptTypeVal, Vec<BodyId>),
    type_names: Arc<BTreeMap<AScriptStructId, String>>,
  ) -> HCError {
    HCError::ExtendedError(Arc::new(Self { p, g, type_a, type_b, type_names }))
  }
}

impl ExtendedError for ErrIncompatibleProductionVectorTypes {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ErrIncompatibleProductionVectorTypes {
      p,
      g,
      type_a: (type_a, a_bodies),
      type_b: (type_b, b_bodies),
      type_names,
    } = self;

    let body_draw = |&b| {
      let body = g.get_body(&b).unwrap();
      format!(
        "[{}]\n{}\n{}",
        body.tok.path_ref(&body.grammar_ref.path),
        body.item().blame_string(g),
        body.tok.blame(0, 0, "", BlameColor::Red)
      )
    };

    f.write_fmt(format_args!(
      "Incompatible types are produced by production [{}]:
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
}
