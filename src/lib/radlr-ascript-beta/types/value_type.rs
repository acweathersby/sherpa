use std::rc::Rc;

use radlr_core::{proxy::OrderedSet, CachedString, DBNonTermKey, IString};
use radlr_formatter::*;

use crate::types::value_type;

use super::StringId;

#[derive(Debug)]
pub enum AscriptPropType {}

impl ValueObj for AscriptPropType {
  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptPropType"
  }

  fn get_val<'scope>(&'scope self, key: &str, _: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      _ => Value::None,
    }
  }

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {
    funct("db".to_string(), Value::Obj(self))
  }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum AscriptType {
  Scalar(AscriptScalarType),
  Aggregate(AscriptAggregateType),
  NonTerminalVal(DBNonTermKey),
  #[default]
  Undefined,
}

impl AscriptType {
  pub fn friendly_name(&self) -> String {
    use AscriptType::*;
    match self {
      Scalar(scalar) => scalar.friendly_name(),
      Aggregate(aggregate) => aggregate.friendly_name(),
      NonTerminalVal(key) => "Undefined".into(),
      Undefined => "Undefined".into(),
    }
  }
}

impl PartialEq for AscriptType {
  fn eq(&self, other: &Self) -> bool {
    if std::mem::discriminant(self) != std::mem::discriminant(other) {
      false
    } else if let (Some(a), Some(b)) = (self.as_aggregate(), other.as_aggregate()) {
      match (a, b) {
        (
          AscriptAggregateType::Map { key_type: a_key, val_type: a_val },
          AscriptAggregateType::Map { key_type: b_key, val_type: b_val },
        ) => a_key == b_key && a_val == b_val,

        (AscriptAggregateType::Vec { base_type: a_val }, AscriptAggregateType::Vec { base_type: b_val }) => a_val == b_val,
        _ => false,
      }
    } else if let (Some(a), Some(b)) = (self.as_scalar(), other.as_scalar()) {
      a == b
    } else {
      false
    }
  }
}

#[derive(Debug)]
pub enum PendingType {
  Resolved(AscriptType),
  Unresolved {
    non_terms:  OrderedSet<DBNonTermKey>,
    scalars:    OrderedSet<AscriptScalarType>,
    aggregates: OrderedSet<AscriptAggregateType>,
  },
  Invalid,
}

impl AscriptType {
  pub fn as_scalar(&self) -> Option<AscriptScalarType> {
    match self {
      Self::Scalar(scalar) => Some(*scalar),
      _ => None,
    }
  }

  pub fn as_aggregate(&self) -> Option<AscriptAggregateType> {
    match self {
      Self::Aggregate(scalar) => Some(*scalar),
      _ => None,
    }
  }
}

impl ValueObj for AscriptType {
  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptType"
  }

  fn get_val<'scope>(&'scope self, key: &str, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    fn scalar_type<'scope>(scaler: &AscriptScalarType, s_store: &radlr_core::IStringStore) -> Value<'scope> {
      match scaler {
        AscriptScalarType::Bool(_) => Value::Str("bool".intern(s_store)),
        AscriptScalarType::U8(_) => Value::Str("u8".intern(s_store)),
        AscriptScalarType::U16(_) => Value::Str("u16".intern(s_store)),
        AscriptScalarType::U32(_) => Value::Str("u32".intern(s_store)),
        AscriptScalarType::U64(_) => Value::Str("u64".intern(s_store)),
        AscriptScalarType::I8(_) => Value::Str("i8".intern(s_store)),
        AscriptScalarType::I16(_) => Value::Str("i16".intern(s_store)),
        AscriptScalarType::I32(_) => Value::Str("i32".intern(s_store)),
        AscriptScalarType::I64(_) => Value::Str("i64".intern(s_store)),
        AscriptScalarType::F32(_) => Value::Str("f32".intern(s_store)),
        AscriptScalarType::F64(_) => Value::Str("f64".intern(s_store)),
        AscriptScalarType::Struct(_) => Value::Str("struct".intern(s_store)),
        AscriptScalarType::Token => Value::Str("tok".intern(s_store)),
        AscriptScalarType::TokenRange => Value::Str("tok_range".intern(s_store)),
        AscriptScalarType::String(_) => Value::Str("str".intern(s_store)),
        AscriptScalarType::Undefined => Value::None,
      }
    }

    match key {
      "agg_type" => match self {
        AscriptType::Aggregate(agg) => match agg {
          AscriptAggregateType::Map { .. } => Value::Str("map".intern(s_store)),
          AscriptAggregateType::Vec { .. } => Value::Str("vec".intern(s_store)),
        },
        _ => Value::None,
      },
      "key_type" => match self {
        AscriptType::Aggregate(agg) => match agg {
          AscriptAggregateType::Map { key_type, .. } => scalar_type(key_type, s_store),
          _ => Value::None,
        },
        _ => Value::None,
      },
      "base_type" => match self {
        AscriptType::Aggregate(agg) => match agg {
          AscriptAggregateType::Map { val_type, .. } => scalar_type(val_type, s_store),
          AscriptAggregateType::Vec { base_type, .. } => scalar_type(base_type, s_store),
        },
        AscriptType::Scalar(scaler) => scalar_type(scaler, s_store),
        _ => Value::None,
      },
      "base_val" => match self {
        AscriptType::Scalar(scaler) => match scaler {
          AscriptScalarType::Bool(val) => Value::Str(val.to_string().intern(s_store)),
          AscriptScalarType::U8(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::U16(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::U32(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::U64(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::I8(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::I16(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::I32(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::I64(val) => Value::Int(val.unwrap_or_default() as isize),
          AscriptScalarType::F32(val) => Value::Num(val.unwrap_or_default() as f64),
          AscriptScalarType::F64(val) => Value::Num(val.unwrap_or_default() as f64),
          AscriptScalarType::String(val) => Value::Str(val.unwrap_or_default()),
          _ => Value::None,
        },
        _ => Value::None,
      },
      _ => Value::None,
    }
  }

  fn get_len<'scope>(&'scope self) -> usize {
    match self {
      AscriptType::Aggregate(..) => 2,
      _ => 1,
    }
  }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum AscriptScalarType {
  U8(Option<usize>),
  U16(Option<usize>),
  U32(Option<usize>),
  U64(Option<usize>),
  I8(Option<isize>),
  I16(Option<isize>),
  I32(Option<isize>),
  I64(Option<isize>),
  F32(Option<f64>),
  F64(Option<f64>),
  String(Option<IString>),
  Bool(bool),
  Struct(StringId),
  Token,
  TokenRange,
  #[default]
  Undefined,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum BaseType {
  Bool,
  Uint,
  Int,
  Float,
  Token,
  String,
  Other,
}

impl From<AscriptScalarType> for BaseType {
  fn from(value: AscriptScalarType) -> Self {
    use AscriptScalarType::*;
    match value {
      AscriptScalarType::Bool(..) => BaseType::Bool,
      U8(..) | U16(..) | U32(..) | U64(..) => BaseType::Uint,
      I8(..) | I16(..) | I32(..) | I64(..) => BaseType::Int,
      F32(..) | F64(..) => BaseType::Float,
      TokenRange | Token => BaseType::Token,
      String(..) => BaseType::String,
      _ => BaseType::Other,
    }
  }
}

impl AscriptScalarType {
  pub fn byte_size(&self) -> usize {
    use AscriptScalarType::*;
    match self {
      Bool(..) | I8(..) | U8(..) => 1,
      I16(..) | U16(..) => 2,
      U32(..) | I32(..) | F32(..) => 4,
      U64(..) | I64(..) | F64(..) => 8,
      String(..) => 16,
      Struct(..) => 128,
      Token => 16,
      TokenRange => 8,
      Undefined => 0,
    }
  }

  pub fn friendly_name(&self) -> String {
    use AscriptScalarType::*;
    match self {
      U8(..) => "U8".into(),
      U16(..) => "U16".into(),
      U32(..) => "U32".into(),
      U64(..) => "U64".into(),
      I8(..) => "I8".into(),
      I16(..) => "I16".into(),
      I32(..) => "I32".into(),
      I64(..) => "I64".into(),
      F32(..) => "F32".into(),
      F64(..) => "F64".into(),
      String(..) => "String".into(),
      Bool(..) => "Bool".into(),
      Struct(..) => "Struct".into(),
      Token => "Token".into(),
      TokenRange => "TokenRange".into(),
      Undefined => "Undefined".into(),
    }
  }
}

impl PartialEq for AscriptScalarType {
  fn eq(&self, other: &Self) -> bool {
    std::mem::discriminant(self) == std::mem::discriminant(other)
  }
}

#[derive(Debug, Clone)]
pub enum GraphNode {
  Add(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Sub(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Mul(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Div(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Map(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Vec(GraphNodeVecInits, AscriptType),
  Str(Option<Rc<GraphNode>>, AscriptType),
  Bool(Option<Rc<GraphNode>>, AscriptType),
  Num(Option<Rc<GraphNode>>, AscriptType),
  Tok(Rc<GraphNode>, AscriptType),
  Sym(usize, bool, AscriptType),
  TokSym(usize, bool, AscriptType),
  TokRule(AscriptType),
  Undefined(AscriptType),
}

formatted_typed_vector!(GraphNodeVecInits, GraphNode, "nodes", derive(Clone, Debug));

impl GraphNode {
  pub fn get_type(&self) -> &AscriptType {
    use GraphNode::*;
    match self {
      Add(.., ty)
      | Sub(.., ty)
      | Mul(.., ty)
      | Div(.., ty)
      | Map(.., ty)
      | Vec(.., ty)
      | Sym(.., ty)
      | Num(.., ty)
      | Str(.., ty)
      | Bool(.., ty)
      | TokRule(ty)
      | TokSym(.., ty)
      | Tok(.., ty)
      | Undefined(ty) => ty,
    }
  }
}

impl ValueObj for GraphNode {
  fn get_type<'scope>(&'scope self) -> &str {
    use GraphNode::*;
    match self {
      Add(..) => "AddNode",
      Sub(..) => "SubNode",
      Mul(..) => "MulNode",
      Div(..) => "DivNode",
      Map(..) => "MapNode",
      Vec(..) => "VecNode",
      Sym(..) => "SymNode",
      Num(..) => "NumNode",
      Str(..) => "StrNode",
      Bool(..) => "BoolNode",
      TokRule(..) | TokSym(..) | Tok(..) => "TokNode",
      Undefined(..) => "UndefinedNode",
    }
  }

  fn get_keys<'scope>(&'scope self) -> &'static [&'static str] {
    &["left", "right", "key", "index", "init", "ast_type"]
  }

  fn get_val<'scope>(&'scope self, key: &str, core: &radlr_core::IStringStore) -> Value<'scope> {
    use GraphNode::*;
    match key {
      "left" => match self {
        Add(left, ..) => {
          dbg!(&self);
          Value::Obj(left.as_ref())
        }
        Sub(left, ..) => Value::Obj(left.as_ref()),
        Mul(left, ..) => Value::Obj(left.as_ref()),
        Div(left, ..) => Value::Obj(left.as_ref()),
        _ => Value::None,
      },
      "right" => match self {
        Add(_, right, ..) => Value::Obj(right.as_ref()),
        Sub(_, right, ..) => Value::Obj(right.as_ref()),
        Mul(_, right, ..) => Value::Obj(right.as_ref()),
        Div(_, right, ..) => Value::Obj(right.as_ref()),
        _ => Value::None,
      },
      "key" => match self {
        Map(key, ..) => Value::Obj(key.as_ref()),
        _ => Value::None,
      },
      "index" => match self {
        Sym(index, ..) => Value::Int(*index as isize),
        TokSym(index, ..) => Value::Int(*index as isize),
        _ => Value::None,
      },
      "is_last_ref" => match self {
        Sym(_, last_ref, ..) => Value::Int(*last_ref as isize),
        TokSym(_, last_ref, ..) => Value::Int(*last_ref as isize),
        _ => Value::None,
      },
      "init" => match self {
        Map(_, val, ..) => Value::Obj(val.as_ref()),
        Vec(val, ..) => Value::Obj(val),
        Str(val, ..) => val.as_ref().map(convertRCToValue).unwrap_or(Value::None),
        Bool(val, ..) => val.as_ref().map(convertRCToValue).unwrap_or(Value::None),
        Num(val, ..) => val.as_ref().map(convertRCToValue).unwrap_or(Value::None),
        _ => Value::None,
      },
      "ast_type" => Value::Obj(self.get_type()),
      _ => GraphNode::get_type(self).get_val(key, core),
    }
  }
}

fn convertRCToValue(v: &Rc<GraphNode>) -> Value {
  match v.as_ref() {
    GraphNode::Undefined(_) => Value::None,
    v => Value::Obj(v),
  }
}

#[derive(Debug, Clone, Copy)]
pub enum AscriptAggregateType {
  Vec { base_type: AscriptScalarType },
  Map { key_type: AscriptScalarType, val_type: AscriptScalarType },
}

impl AscriptAggregateType {
  pub fn friendly_name(&self) -> String {
    use AscriptAggregateType::*;
    match self {
      Vec { base_type } => base_type.friendly_name() + &"[]",
      Map { key_type, val_type } => "{ ".to_string() + &key_type.friendly_name() + ":" + &val_type.friendly_name() + "}",
    }
  }
}
