use super::{AscriptType, GraphNode};
use crate::StringId;
use radlr_core::{parser::ASTNode, proxy::OrderedMap, GrammarIdentities};
use radlr_formatter::*;
use std::fmt::Debug;

/// Intializes a value derived from a rule.
pub struct Initializer {
  // The type created by this initializer
  pub(crate) ty:           AscriptType,
  /// Optional name. Assigned if the initializer is used for a struct property.
  /// This is left as default if this Initializer is used as the node
  /// initialization of a rule.
  pub(crate) name:         StringId,
  pub(crate) output_graph: Option<GraphNode>,
  pub(crate) ast:          Option<ASTNode>,
  pub(crate) g_id:         GrammarIdentities,
}

impl Debug for Initializer {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("Initializer");
    s.field("ty", &self.ty);
    s.field("name", &self.name);
    s.field("output_graph", &self.output_graph);
    s.finish()
  }
}

impl ValueObj for Initializer {
  fn get_val<'scope>(&'scope self, key: &str, _: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.0),
      "node" => {
        if let Some(node) = &self.output_graph {
          Value::Obj(node)
        } else {
          Value::None
        }
      }
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "Initializer"
  }
}

#[derive(Debug)]
pub struct StructInitializer {
  pub(crate) name:     StringId,
  pub(crate) props:    AscriptStructInitProps,
  /// `true` if the number of `props`  is equal to the number of property
  /// definitions.
  pub(crate) complete: bool,
}

formatted_typed_ordered_map!(AscriptStructInitProps, StringId, Initializer, "AscriptStructInitProps");

impl ValueObj for StructInitializer {
  fn get_val<'scope>(&'scope self, key: &str, _: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "props" => Value::Obj(&self.props),
      "name" => Value::Str(self.name.0),
      "complete" => Value::Int(self.complete as isize),
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "StructInitializer"
  }
}
