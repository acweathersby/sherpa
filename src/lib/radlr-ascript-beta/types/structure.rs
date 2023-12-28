use super::AscriptType;
use crate::StringId;
use radlr_core::{proxy::OrderedMap, CachedString, GrammarIdentities};
use radlr_formatter::*;
use radlr_rust_runtime::types::Token;
use std::fmt::Debug;

formatted_typed_ordered_map!(AscriptStructs, StringId, AscriptStruct, "structs");

#[derive(Debug)]
pub struct AscriptStruct {
  #[allow(unused)]
  pub(crate) id:         StringId,
  pub(crate) name:       String,
  pub(crate) properties: AscriptStructProps,
  pub(crate) has_token:  bool,
}

formatted_typed_ordered_map!(AscriptStructProps, StringId, AscriptProp, "AscriptStructProps");

impl ValueObj for AscriptStruct {
  fn get_val<'scope>(&'scope self, key: &str, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "is_empty" => Value::Int((self.properties.len() == 0) as isize),
      "name" => Value::Str(self.name.intern(s_store)),
      "props" => Value::Obj(&self.properties),
      "has_token" => {
        dbg!(self);
        Value::Int(self.has_token as isize)
      }
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptStruct"
  }
}

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AscriptProp {
  pub(crate) is_optional: bool,
  pub(crate) name:        String,
  pub(crate) ty:          AscriptType,
  pub(crate) tok:         Token,
  pub(crate) g_id:        GrammarIdentities,
}

#[cfg(not(debug_assertions))]
impl Debug for AscriptProp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("AscriptProp")
  }
}

impl ValueObj for AscriptProp {
  fn get_val<'scope>(&'scope self, key: &str, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.intern(s_store)),
      "type" => Value::Obj(&self.ty),

      "optional" => {
        dbg!(self);
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
