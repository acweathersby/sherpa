use crate::{Initializer, StructInitializer};
use radlr_formatter::*;
use std::fmt::Debug;

formatted_typed_vector!(AscriptRules, AscriptRule, "rules");

#[derive(Debug)]
pub enum AscriptRule {
  Struct(usize, StructInitializer),
  Expression(usize, Initializer),
  ListInitial(usize, Initializer),
  ListContinue(usize, Initializer),
  LastSymbol(usize, Initializer),
  Invalid(usize),
}

impl ValueObj for AscriptRule {
  fn get_type<'scope>(&'scope self) -> &str {
    use AscriptRule::*;
    match self {
      Struct(..) => "StructRule",
      Expression(..) => "ExpressionRule",
      ListInitial(..) => "ListInitialRule",
      ListContinue(..) => "ListContinueRule",
      LastSymbol(..) => "LastSymbolRule",
      Invalid(..) => "InvalidRule",
    }
  }

  fn get_val<'scope>(&'scope self, key: &str, _: &radlr_core::IStringStore) -> Value<'scope> {
    use AscriptRule::*;
    match key {
      "init" => match self {
        Struct(_, init) => Value::Obj(init),
        Expression(_, init) => Value::Obj(init),
        ListInitial(_, init) => Value::Obj(init),
        ListContinue(_, init) => Value::Obj(init),
        LastSymbol(_, init) => Value::Obj(init),
        Invalid(..) => Value::None,
      },
      "id" => match self {
        Struct(id, ..)
        | Expression(id, ..)
        | ListInitial(id, ..)
        | ListContinue(id, ..)
        | LastSymbol(id, ..)
        | Invalid(id, ..) => Value::Int(*id as isize),
      },
      _ => Value::None,
    }
  }
}
