use hctk_core::grammar::data::ast::ASTNode;
use hctk_core::grammar::hash_id_value_u64;
use hctk_core::types::*;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::discriminant;

#[derive(Hash, Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Copy)]
pub struct AScriptStructId(u64);

impl AScriptStructId {
  pub fn new(name: &str) -> Self {
    AScriptStructId(hash_id_value_u64(name))
  }
}

#[derive(Hash, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct AScriptPropId {
  pub struct_id: AScriptStructId,
  pub name:      String,
}

impl AScriptPropId {
  pub fn new(struct_id: AScriptStructId, name: &str) -> Self {
    AScriptPropId { struct_id, name: name.to_owned() }
  }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum CompositeTypes {
  Atom(AScriptTypeVal),
  Node,
  Any,
  Token,
}

#[derive(PartialEq, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum AScriptTypeVal {
  TokenVec,
  StringVec,
  GenericStruct(BTreeSet<AScriptStructId>),
  GenericStructVec(BTreeSet<AScriptStructId>),
  GenericVec(Option<BTreeSet<AScriptTypeVal>>),
  Struct(AScriptStructId),
  String(Option<String>),
  Bool(Option<bool>),
  F64(Option<u64>),
  F32(Option<u64>),
  I64(Option<i64>),
  I32(Option<i32>),
  I16(Option<i16>),
  I8(Option<i8>),
  U64(Option<u64>),
  U32(Option<u32>),
  U16(Option<u16>),
  U8(Option<u8>),
  U8Vec,
  U16Vec,
  U32Vec,
  U64Vec,
  I8Vec,
  I16Vec,
  I32Vec,
  I64Vec,
  F32Vec,
  F64Vec,
  Token,
  UnresolvedProduction(ProductionId),
  UnresolvedStruct,
  Undefined,
  /// A generic struct
  Any,
}
impl Debug for AScriptTypeVal {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self.debug_string(None))
  }
}

impl AScriptTypeVal {
  pub fn is_vec(&self) -> bool {
    use AScriptTypeVal::*;
    matches!(
      self,
      TokenVec
        | U8Vec
        | U16Vec
        | U32Vec
        | U64Vec
        | I8Vec
        | I16Vec
        | I32Vec
        | I64Vec
        | F32Vec
        | F64Vec
        | StringVec
        | GenericStructVec(_)
    )
  }

  pub fn is_same_type(&self, other: &Self) -> bool {
    discriminant(self) == discriminant(other)
  }

  pub fn is_undefined(&self) -> bool {
    matches!(self, AScriptTypeVal::Undefined)
  }

  pub fn is_unresolved_production(&self) -> bool {
    matches!(self, AScriptTypeVal::UnresolvedProduction(..))
  }

  pub fn debug_string(&self, g: Option<&GrammarStore>) -> String {
    use AScriptTypeVal::*;
    match self {
      GenericVec(vec) => match vec {
        Some(vector) => format!(
          "Vector[{}]",
          vector.iter().map(|t| { t.hcobj_type_name(g) }).collect::<Vec<_>>().join(", ")
        ),
        None => "Vector[Undefined]".to_string(),
      },
      UnresolvedStruct => "UnresolvedStruct".to_string(),
      Struct(id) => format!("Struct<{:?}>", id),
      String(..) => "String".to_string(),
      Bool(..) => "Bool".to_string(),
      F64(..) => "F64".to_string(),
      F32(..) => "F32".to_string(),
      I64(..) => "I64".to_string(),
      I32(..) => "I32".to_string(),
      I16(..) => "I16".to_string(),
      I8(..) => "I8".to_string(),
      U64(..) => "U64".to_string(),
      U32(..) => "U32".to_string(),
      U16(..) => "U16".to_string(),
      U8(..) => "U8".to_string(),
      F64Vec => "F64Vec".to_string(),
      F32Vec => "F32Vec".to_string(),
      I64Vec => "I64Vec".to_string(),
      I32Vec => "I32Vec".to_string(),
      I16Vec => "I16Vec".to_string(),
      I8Vec => "I8Vec".to_string(),
      U64Vec => "U64Vec".to_string(),
      U32Vec => "U32Vec".to_string(),
      U16Vec => "U16Vec".to_string(),
      U8Vec => "U8Vec".to_string(),
      TokenVec => "Tokens".to_string(),
      StringVec => "Strings".to_string(),
      GenericStructVec(nodes) => format!("Nodes[{:?}]", nodes),
      Undefined => "Undefined".to_string(),
      Token => "Token".to_string(),
      GenericStruct(sub_types) => "Node".to_string(),
      Any => "Any".to_string(),
      UnresolvedProduction(id) => match g {
        Some(g) => {
          let name = g.get_production_plain_name(id);
          format!("UnresolvedProduction[{}]", name)
        }
        None => format!("UnresolvedProduction[{:?}]", id),
      },
    }
  }

  pub fn hcobj_type_name(&self, g: Option<&GrammarStore>) -> String {
    use AScriptTypeVal::*;
    match self {
      GenericVec(vec) => match vec {
        Some(vector) => format!(
          "GenericVec{}",
          vector.iter().map(|t| { t.hcobj_type_name(g) }).collect::<Vec<_>>().join(", ")
        ),
        None => "GenericVec".to_string(),
      },
      UnresolvedStruct => "UnresolvedStruct".to_string(),
      Struct(id) => format!("Struct<{:?}>", id),
      String(..) => "STRING".to_string(),
      Bool(..) => "BOOL".to_string(),
      F64(..) => "F64".to_string(),
      F32(..) => "F32".to_string(),
      I64(..) => "I64".to_string(),
      I32(..) => "I32".to_string(),
      I16(..) => "I16".to_string(),
      I8(..) => "I8".to_string(),
      U64(..) => "U64".to_string(),
      U32(..) => "U32".to_string(),
      U16(..) => "U16".to_string(),
      U8(..) => "U8".to_string(),
      F64Vec => "F64Vec".to_string(),
      F32Vec => "F32Vec".to_string(),
      I64Vec => "I64Vec".to_string(),
      I32Vec => "I32Vec".to_string(),
      I16Vec => "I16Vec".to_string(),
      I8Vec => "I8Vec".to_string(),
      U64Vec => "U64Vec".to_string(),
      U32Vec => "U32Vec".to_string(),
      U16Vec => "U16Vec".to_string(),
      U8Vec => "U8Vec".to_string(),
      TokenVec => "TOKENS".to_string(),
      StringVec => "Strings".to_string(),
      GenericStructVec(..) => "NODES".to_string(),
      Undefined => "Undefined".to_string(),
      Token => "TOKEN".to_string(),
      GenericStruct(sub_types) => "Node".to_string(),
      Any => "Any".to_string(),
      UnresolvedProduction(id) => match g {
        Some(g) => {
          let name = g.get_production_plain_name(id);
          format!("UnresolvedProduction[{}]", name)
        }
        None => format!("UnresolvedProduction[{:?}]", id),
      },
    }
  }
}

pub trait AScriptNumericType {
  type Type;
  fn none() -> AScriptTypeVal;
  fn from_f64(val: f64) -> AScriptTypeVal;
  fn string_from_f64(val: f64) -> String;
  fn to_fn_name() -> String;
  fn prim_type_name() -> &'static str;
}

macro_rules! num_type {
  ( $struct_name:ident, $type_val:ident, $type:ident, $conversion_type:ident ) => {
    pub struct $struct_name;
    impl AScriptNumericType for $struct_name {
      type Type = $type;

      #[inline(always)]
      fn none() -> AScriptTypeVal {
        AScriptTypeVal::$type_val(None)
      }

      #[inline(always)]
      fn from_f64(val: f64) -> AScriptTypeVal {
        AScriptTypeVal::$type_val(Some(val as $conversion_type))
      }

      #[inline(always)]
      fn string_from_f64(val: f64) -> String {
        (val as Self::Type).to_string()
      }

      #[inline(always)]
      fn prim_type_name() -> &'static str {
        stringify!($type)
      }

      #[inline(always)]
      fn to_fn_name() -> String {
        "to_".to_string() + stringify!($type)
      }
    }
  };
}

num_type!(AScriptTypeValF64, F64, f64, u64);

num_type!(AScriptTypeValF32, F32, f32, u64);

num_type!(AScriptTypeValU64, U64, u64, u64);

num_type!(AScriptTypeValU32, U32, u32, u32);

num_type!(AScriptTypeValU16, U16, u16, u16);

num_type!(AScriptTypeValU8, U8, u8, u8);

num_type!(AScriptTypeValI64, I64, i64, i64);

num_type!(AScriptTypeValI32, I32, i32, i32);

num_type!(AScriptTypeValI16, I16, i16, i16);

num_type!(AScriptTypeValI8, I8, i8, i8);

#[derive(Debug)]
pub struct AScriptProp {
  pub type_val: AScriptTypeVal,
  pub first_declared_location: Token,
  /// Tracks the number of times this property has been
  /// declared in a struct.
  pub define_count: usize,
  pub optional: bool,
}

#[derive(Debug)]
pub struct AScriptStruct {
  pub id: AScriptStructId,
  pub type_name: String,
  pub prop_ids: BTreeSet<AScriptPropId>,
  /// Tracks the number of times this struct has been defined
  pub define_count: usize,
  pub definition_locations: Vec<Token>,
  pub include_token: bool,
}

#[derive(Debug)]
pub struct AScriptStore {
  /// Store of unique AScriptStructs
  pub structs:        BTreeMap<AScriptStructId, AScriptStruct>,
  pub props:          BTreeMap<AScriptPropId, AScriptProp>,
  pub prod_types:     BTreeMap<ProductionId, HashMap<AScriptTypeVal, BTreeSet<Token>>>,
  pub body_reduce_fn: BTreeMap<BodyId, (AScriptTypeVal, ASTNode)>,
  pub name:           String,
}

impl AScriptStore {
  pub fn new() -> Self {
    AScriptStore {
      structs:        BTreeMap::new(),
      props:          BTreeMap::new(),
      prod_types:     BTreeMap::new(),
      body_reduce_fn: BTreeMap::new(),
      name:           "ASTNode".to_string(),
    }
  }

  pub fn set_name(&mut self, name: &str) {
    self.name = name.to_string();
  }

  pub fn type_name(&self) -> String {
    format!("{}Type", self.name)
  }

  /// The name for the generic type wrapper
  pub fn gen_name(&self) -> String {
    format!("Gen{}", self.name)
  }
}

impl Default for AScriptStore {
  fn default() -> Self {
    Self::new()
  }
}
