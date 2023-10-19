use std::{io::Write, ops::IndexMut, sync::Arc};

use super::{AscriptType, GraphNode, PendingType};
use crate::build_db;
use sherpa_core::{
  parser::{ASTNode, Rule},
  proxy::OrderedMap,
  CachedString,
  IString,
  ParserDatabase,
  SherpaDatabase,
  SherpaError,
  SherpaResult,
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

  pub(crate) db: Arc<ParserDatabase>,
}

impl AscriptDatabase {
  /// Format and print this database to a string using the a formatting
  /// script.
  pub fn format_string(&self, script: &str) -> SherpaResult<String> {
    unsafe { Ok(String::from_utf8_unchecked(self.format(script, Vec::with_capacity(1024))?)) }
  }

  /// Format and print this database to the given buffer using a formatting
  /// script.
  pub fn format<W: Write>(&self, script: &str, writer: W) -> SherpaResult<W> {
    let mut ctx: FormatterContext = FormatterContext::new(self.db.string_store().clone());
    ctx.set_val("structs", Value::Obj(&self.structs));
    ctx.set_val("rules", Value::Obj(&self.rules));
    ctx.max_width = 20;
    let f: Formatter = FormatterResult::from(script).into_result()?;
    f.write_to_output(&mut ctx, writer)
  }
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

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {}
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

#[derive(Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
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
  Struct(usize, StructInitializer),
  Expression(usize, Initializer),
  ListInitial(usize, Initializer),
  ListContinue(usize, Initializer),
  LastSymbol(usize, Initializer),
  Invalid(usize),
}

#[derive(Debug)]
/// Intializes a value derived from a rule.
pub struct Initializer {
  // The type created by this initializer
  pub(crate) ty:           AscriptType,
  pub(crate) name:         StringId,
  pub(crate) output_graph: Option<GraphNode>,
  pub(crate) ast:          Option<ASTNode>,
}

impl ValueObj for Initializer {
  fn get_val<'scope>(&'scope self, key: &str, _: &sherpa_core::IStringStore) -> Value<'scope> {
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
}

#[derive(Debug)]
pub struct StructInitializer {
  pub(crate) name:  StringId,
  pub(crate) props: AscriptStructInitProps,
}

formatted_typed_ordered_map!(AscriptStructInitProps, StringId, Initializer, "AscriptStructInitProps");

impl ValueObj for StructInitializer {
  fn get_val<'scope>(&'scope self, key: &str, _: &sherpa_core::IStringStore) -> Value<'scope> {
    match key {
      "props" => Value::Obj(&self.props),
      "name" => Value::Str(self.name.0),
      _ => Value::None,
    }
  }
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

  fn get_val<'scope>(&'scope self, key: &str, _: &sherpa_core::IStringStore) -> Value<'scope> {
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
