use super::compile::compile_ascript_store;
use crate::{
  grammar::{compile::parser::sherpa::ASTNode, hash_id_value_u64},
  types::*,
};
use std::{
  collections::{BTreeMap, BTreeSet, HashMap},
  fmt::Debug,
  hash::Hash,
  mem::discriminant,
  sync::Arc,
};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Default)]
pub struct TaggedType {
  pub type_:        AScriptTypeVal,
  pub tag:          RuleId,
  pub symbol_index: u32,
}

impl TaggedType {
  pub fn sanitize(self) -> Self {
    Self {
      symbol_index: 0,
      tag:          RuleId::default(),
      type_:        self.type_,
    }
  }

  pub fn debug_string(&self, g: Option<&GrammarStore>) -> String {
    format!("Tagged: [{}] {}", self.symbol_index, self.type_.debug_string(g))
  }
}

impl Into<AScriptTypeVal> for TaggedType {
  fn into(self) -> AScriptTypeVal {
    self.type_
  }
}

impl Into<AScriptTypeVal> for &TaggedType {
  fn into(self) -> AScriptTypeVal {
    self.type_.clone()
  }
}

impl Into<RuleId> for TaggedType {
  fn into(self) -> RuleId {
    self.tag
  }
}

impl Into<RuleId> for &TaggedType {
  fn into(self) -> RuleId {
    self.tag
  }
}

impl Into<AScriptStructId> for &TaggedType {
  fn into(self) -> AScriptStructId {
    match self.type_ {
      AScriptTypeVal::Struct(id) => id,
      _ => AScriptStructId(0),
    }
  }
}

impl Into<AScriptStructId> for TaggedType {
  fn into(self) -> AScriptStructId {
    match self.type_ {
      AScriptTypeVal::Struct(id) => id,
      _ => AScriptStructId(0),
    }
  }
}

#[derive(PartialEq, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum AScriptTypeVal {
  TokenVec,
  StringVec,
  GenericStruct(BTreeSet<TaggedType>),
  GenericStructVec(BTreeSet<TaggedType>),
  /// Exists during initial compile phases and are eventual replaced
  /// with one of the defined Vec types
  GenericVec(Option<BTreeSet<TaggedType>>),
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
  TokenRange,
  UnresolvedProduction(ProductionId),
  Undefined,
  /// A generic struct
  Any,
}

impl Default for AScriptTypeVal {
  fn default() -> Self {
    AScriptTypeVal::Undefined
  }
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
        | GenericVec(_)
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

  pub fn get_production_id(&self) -> ProductionId {
    match self {
      AScriptTypeVal::UnresolvedProduction(id) => id.clone(),
      _ => ProductionId::default(),
    }
  }

  /// This type is a non-aggregate scalar or vector type.
  /// For vectors, this means its elements are not an enum type.
  pub fn is_atom(&self) -> bool {
    use AScriptTypeVal::*;
    match self {
      Token | TokenRange | Struct(..) | String(..) | Bool(..) | F64(..) | F32(..) | I64(..)
      | I32(..) | I16(..) | I8(..) | U64(..) | U32(..) | U16(..) | U8(..) | F64Vec | F32Vec
      | I64Vec | I32Vec | I16Vec | I8Vec | U64Vec | U32Vec | U16Vec | U8Vec | TokenVec
      | StringVec => true,
      GenericStructVec(nodes) => {
        if nodes.len() == 1 {
          match nodes.first() {
            Some(TaggedType { type_: Struct(_), .. }) => true,
            _ => false,
          }
        } else {
          false
        }
      }
      Undefined | GenericVec(..) | GenericStruct(..) | Any | UnresolvedProduction(..) => false,
    }
  }

  pub fn blame_string(
    &self,
    g: &GrammarStore,
    struct_types: &BTreeMap<AScriptStructId, String>,
  ) -> String {
    use AScriptTypeVal::*;
    match self {
      GenericVec(vec) => match vec {
        Some(vector) => format!(
          "Vector of [{}]",
          vector
            .iter()
            .filter_map(|t| {
              match t.into() {
                Undefined => None,
                t => Some(t.blame_string(g, struct_types)),
              }
            })
            .collect::<Vec<_>>()
            .join(", ")
        ),
        None => "UndefinedVector".to_string(),
      },
      GenericStructVec(nodes) => format!(
        "Structs<[{}]>",
        nodes
          .iter()
          .map(|id| struct_types.get(&id.into()).cloned().unwrap_or_default())
          .collect::<Vec<_>>()
          .join(", ")
      ),
      Struct(id) => format!("Struct<{}>", struct_types.get(id).cloned().unwrap_or_default()),
      _ => self.debug_string(Some(g)),
    }
  }

  pub fn debug_string(&self, g: Option<&GrammarStore>) -> String {
    use AScriptTypeVal::*;
    match self {
      GenericVec(vec) => match vec {
        Some(vector) => format!(
          "Vector of [{}]",
          vector
            .iter()
            .filter_map(|t| {
              match t.into() {
                Undefined => None,
                t => Some(t.debug_string(g)),
              }
            })
            .collect::<Vec<_>>()
            .join(", ")
        ),
        None => "UndefinedVector".to_string(),
      },
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
      TokenRange => "Token".to_string(),
      Token => "Token".to_string(),
      GenericStruct(_) => "Node".to_string(),
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
          vector
            .iter()
            .map(|t| { AScriptTypeVal::from(Into::into(t)).hcobj_type_name(g) })
            .collect::<Vec<_>>()
            .join(", ")
        ),
        None => "GenericVec".to_string(),
      },
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
      StringVec => "STRINGS".to_string(),
      GenericStructVec(..) => "NODES".to_string(),
      Undefined => "Undefined".to_string(),
      Token => "TOKEN".to_string(),
      GenericStruct(_) => "Node".to_string(),
      Any => "Any".to_string(),
      UnresolvedProduction(id) => match g {
        Some(g) => {
          let name = g.get_production_plain_name(id);
          format!("UnresolvedProduction[{}]", name)
        }
        None => format!("UnresolvedProduction[{:?}]", id),
      },
      _ => "TOKEN_RANGE".to_string(),
    }
  }
}

pub trait AScriptNumericType {
  type Type;
  fn none() -> AScriptTypeVal;
  fn from_f64(val: f64) -> AScriptTypeVal;
  fn string_from_f64(val: f64) -> String;
  fn to_fn_name() -> String;
  fn from_tok_range_name() -> String;
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

      #[inline(always)]
      fn from_tok_range_name() -> String {
        format!("parse::<{}>", stringify!($type))
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

#[derive(Debug, Clone, Default)]
pub struct AScriptProp {
  pub type_val:    TaggedType,
  pub location:    Token,
  pub grammar_ref: Arc<GrammarRef>,
  /// Tracks the number of times this property has been
  /// declared in a struct.
  pub body_ids:    BTreeSet<RuleId>,
  pub optional:    bool,
}

#[derive(Debug)]
pub struct AScriptStruct {
  pub id: AScriptStructId,
  pub type_name: String,
  pub prop_ids: BTreeSet<AScriptPropId>,
  /// Tracks the number of times this struct has been defined
  pub body_ids: BTreeSet<RuleId>,
  pub definition_locations: BTreeSet<Token>,
  pub include_token: bool,
}

pub type ProductionTypesTable = BTreeMap<ProductionId, HashMap<TaggedType, BTreeSet<RuleId>>>;

#[derive(Debug)]
pub struct AScriptStore {
  /// Store of unique AScriptStructs
  pub structs: BTreeMap<AScriptStructId, AScriptStruct>,
  pub props: BTreeMap<AScriptPropId, AScriptProp>,
  pub prod_types: ProductionTypesTable,
  pub body_reduce_fn: BTreeMap<RuleId, (AScriptTypeVal, ASTNode)>,
  /// The type name of the AST Node enum,
  pub ast_type_name: String,
  pub g: Arc<GrammarStore>,
  // Maps a struct id to a struct type name
  pub struct_lookups: Option<Arc<BTreeMap<AScriptStructId, String>>>,
}

impl AScriptStore {
  pub fn new(grammar: Arc<GrammarStore>) -> SherpaResult<Self> {
    let mut new_self = AScriptStore {
      structs: BTreeMap::new(),
      props: BTreeMap::new(),
      prod_types: BTreeMap::new(),
      body_reduce_fn: BTreeMap::new(),
      ast_type_name: "ASTNode".to_string(),
      struct_lookups: None,
      g: grammar,
    };

    match compile_ascript_store(&mut new_self) {
      errors if errors.have_errors() => SherpaResult::MultipleErrors(errors),
      _ => SherpaResult::Ok(new_self),
    }
  }

  pub fn type_name(&self) -> String {
    format!("{}Type", self.ast_type_name)
  }

  pub fn get_type_names(&mut self) -> Arc<BTreeMap<AScriptStructId, String>> {
    self
      .struct_lookups
      .get_or_insert_with(|| {
        Arc::new(
          self
            .structs
            .iter()
            .map(|(_, struct_)| (struct_.id.clone(), struct_.type_name.clone()))
            .collect(),
        )
      })
      .clone()
  }
}

impl Default for AScriptStore {
  fn default() -> Self {
    AScriptStore {
      g: Default::default(),
      structs: BTreeMap::new(),
      props: BTreeMap::new(),
      prod_types: BTreeMap::new(),
      body_reduce_fn: BTreeMap::new(),
      struct_lookups: None,
      ast_type_name: "ASTNode".to_string(),
    }
  }
}
