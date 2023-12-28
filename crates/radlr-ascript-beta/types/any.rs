use crate::{AscriptAggregateType, AscriptScalarType, AscriptType, AscriptTypes};
use radlr_core::CachedString;
use radlr_formatter::*;
use std::fmt::Debug;

#[derive(Debug)]
pub struct AscriptAny {
  pub(crate) types: AscriptTypes,
  pub(crate) name:  String,
}

impl ValueObj for AscriptAny {
  fn get_keys<'scope>(&'scope self) -> &'static [&'static str] {
    &["name", "types"]
  }

  fn get_val<'scope>(&'scope self, key: &str, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.intern(s_store)),
      "types" => Value::Obj(&self.types),
      "has_token" => Value::Int(self.types.0.iter().any(contains_token_reference) as isize),
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptAnyEnum"
  }
}

fn contains_token_reference(t: &AscriptType) -> bool {
  use AscriptAggregateType::*;
  use AscriptScalarType::*;
  use AscriptType::*;
  match t {
    Scalar(Token) | Scalar(Struct(_, false)) | Scalar(TokenRange) => true,
    Aggregate(Vec { val_type }) => match val_type {
      Token | Struct(_, false) | TokenRange => true,
      _ => false,
    },
    Aggregate(Map { key_type, val_type }) => {
      (match key_type {
        Token | Struct(_, false) | TokenRange => true,
        _ => false,
      }) || (match val_type {
        Token | Struct(_, false) | TokenRange => true,
        _ => false,
      })
    }
    _ => false,
  }
}

formatted_typed_vector!(AscriptAnys, AscriptAny, "any_enums");
