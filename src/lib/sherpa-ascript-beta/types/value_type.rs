use std::rc::Rc;

use sherpa_core::{proxy::OrderedSet, CachedString, DBNonTermKey, IString};
use sherpa_formatter::*;

use super::StringId;

#[derive(Debug)]
pub enum AscriptPropType {}

impl ValueObj for AscriptPropType {
  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptPropType"
  }

  fn get_val<'scope>(&'scope self, key: &str, _: &sherpa_core::IStringStore) -> Value<'scope> {
    match key {
      _ => Value::None,
    }
  }

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {
    funct("db".to_string(), Value::Obj(self))
  }
}

#[derive(Debug, Clone, Copy)]
pub enum AscriptType {
  Scalar(AscriptScalarType),
  Aggregate(AscriptAggregateType),
  NonTerminalVal(DBNonTermKey),
  Undefined,
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
}
impl ValueObj for AscriptType {
  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptType"
  }

  fn get_val<'scope>(&'scope self, key: &str, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
    fn scalar_type<'scope>(scaler: &AscriptScalarType, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
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

#[derive(Debug, Clone, Copy)]
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
}

#[derive(Debug, Clone)]
pub enum GraphNode {
  Add(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Sub(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Mul(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Div(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Map(Rc<GraphNode>, Rc<GraphNode>, AscriptType),
  Vec(Rc<GraphNode>, AscriptType),
  Str(Option<Rc<GraphNode>>),
  Bool(Option<Rc<GraphNode>>),
  Num(Option<Rc<GraphNode>>, AscriptType),
  Tok(Rc<GraphNode>),
  Sym(usize, AscriptType),
  TokSym(usize),
  TokRule,
  Undefined,
}

impl GraphNode {
  pub fn get_type(&self) -> AscriptType {
    use GraphNode::*;
    match self {
      Add(.., ty) => *ty,
      Sub(.., ty) => *ty,
      Mul(.., ty) => *ty,
      Div(.., ty) => *ty,
      Map(.., ty) => *ty,
      Vec(.., ty) => *ty,
      Sym(.., ty) => *ty,
      Num(.., ty) => *ty,
      Str(..) => AscriptType::Scalar(AscriptScalarType::String(None)),
      Bool(..) => AscriptType::Scalar(AscriptScalarType::Bool(false)),
      TokRule | TokSym(..) | Tok(..) => AscriptType::Scalar(AscriptScalarType::Token),
      Undefined => AscriptType::Undefined,
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum AscriptAggregateType {
  Vec { base_type: AscriptScalarType },
  Map { key_type: AscriptScalarType, val_type: AscriptScalarType },
}
