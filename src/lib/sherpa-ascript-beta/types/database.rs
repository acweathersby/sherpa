use std::ops::IndexMut;

use super::{AscriptType, GraphNode, PendingType};
use crate::build_db;
use sherpa_core::{
  parser::{ASTNode, Rule},
  proxy::OrderedMap,
  CachedString,
  IString,
  SherpaDatabase,
  SherpaError,
};
use sherpa_formatter::*;
use sherpa_rust_runtime::types::Token;

#[derive(Debug)]
pub struct AscriptDatabase {
  pub(crate) structs: AscriptStructs,
  /// If errors are present then it is likely that the
  /// database is in an incomplate state and SHOULD not be
  /// used to construct parser outputs.
  pub(crate) errors:  Vec<SherpaError>,

  pub(crate) rules: AscriptRules,
}

impl ValueObj for AscriptDatabase {
  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptDatabase"
  }

  fn get_val<'scope>(&'scope self, key: &str, _: &sherpa_core::IStringStore) -> Value<'scope> {
    match key {
      _ => Value::None,
    }
  }

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {
    funct("structs".to_string(), Value::Obj(&self.structs));
    funct("rules".to_string(), Value::Obj(&self.rules));
  }
}

formatted_typed_vector!(AscriptRules, AscriptRule, "rules");
formatted_typed_ordered_map!(AscriptStructs, StringId, AscriptStruct, "structs");

impl From<&SherpaDatabase> for AscriptDatabase {
  fn from(value: &SherpaDatabase) -> Self {
    build_db::build_database((*value).clone())
  }
}

impl From<SherpaDatabase> for AscriptDatabase {
  fn from(value: SherpaDatabase) -> Self {
    build_db::build_database(value)
  }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StringId(IString);

impl Into<IString> for StringId {
  fn into(self) -> IString {
    self.0
  }
}

impl From<&str> for StringId {
  fn from(value: &str) -> Self {
    Self(value.to_token())
  }
}

impl From<String> for StringId {
  fn from(value: String) -> Self {
    Self(value.to_token())
  }
}

#[derive(Debug)]
pub struct AscriptStruct {
  pub(crate) id:         StringId,
  pub(crate) name:       String,
  pub(crate) properties: AscriptStructProps,
}

formatted_typed_ordered_map!(AscriptStructProps, StringId, AScriptProp, "AscriptStructProps");

impl ValueObj for AscriptStruct {
  fn get_val<'scope>(&'scope self, key: &str, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.intern(s_store)),
      "props" => Value::Obj(&self.properties),
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptStruct"
  }
}

#[derive(Debug)]
pub struct AScriptProp {
  pub(crate) is_optional: bool,
  pub(crate) name:        String,
  pub(crate) ty:          AscriptType,
  pub(crate) tok:         Token,
}

impl ValueObj for AScriptProp {
  fn get_val<'scope>(&'scope self, key: &str, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.intern(s_store)),
      "type" => Value::Obj(&self.ty),
      "optional" => {
        if self.is_optional {
          Value::Int(1)
        } else {
          Value::None
        }
      }
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AScriptProp"
  }
}

#[derive(Debug)]
pub enum AscriptRule {
  Struct(StructInitializer),
  Expression(Initializer),
  ListInitial(Initializer),
  ListContinue(Initializer),
  LastSymbol(Initializer),
  Invalid,
}

impl ValueObj for AscriptRule {}

#[derive(Debug)]
/// Intializes a value derived from a rule.
pub struct Initializer {
  // The type created by this initializer
  pub(crate) ty:           AscriptType,
  pub(crate) output_graph: Option<GraphNode>,
  pub(crate) ast:          Option<ASTNode>,
}

#[derive(Debug)]
pub struct StructInitializer {
  pub(crate) ty:    StringId,
  pub(crate) props: OrderedMap<StringId, Initializer>,
}
