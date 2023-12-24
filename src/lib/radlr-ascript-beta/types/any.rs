use crate::{AscriptScalarType, AscriptType, AscriptTypes};
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
      "has_token" => {
        use AscriptScalarType::*;
        use AscriptType::*;
        Value::Int(self.types.0.iter().any(|t| matches!(t, Scalar(Token) | Scalar(Struct(_)) | Scalar(TokenRange))) as isize)
      }
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptAnyEnum"
  }
}

formatted_typed_vector!(AscriptAnys, AscriptAny, "any_enums");
