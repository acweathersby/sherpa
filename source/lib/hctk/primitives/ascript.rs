use crate::grammar::data::ast::ASTNode;

use crate::primitives::BodyId;

use std::collections::HashMap;

use std::collections::BTreeMap;

use std::collections::BTreeSet;

use crate::grammar::get_production_plain_name;

use crate::primitives::GrammarStore;

use std::mem::discriminant;

use crate::primitives::ProductionId;

use crate::grammar::hash_id_value_u64;

use super::Token;

#[derive(Hash, Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Copy)]
pub struct AScriptStructId(u64);

impl AScriptStructId
{
    pub fn new(name: &str) -> Self
    {
        AScriptStructId(hash_id_value_u64(name))
    }
}

#[derive(Hash, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct AScriptPropId
{
    pub struct_id: AScriptStructId,
    pub name:      String,
}

impl AScriptPropId
{
    pub fn new(struct_id: AScriptStructId, name: &str) -> Self
    {
        AScriptPropId {
            struct_id,
            name: name.to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum CompositeTypes
{
    Atom(AScriptTypeVal),
    Node,
    Any,
    Token,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum AScriptTypeVal
{
    Vector(Option<Vec<AScriptTypeVal>>),
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
    Token,
    UnresolvedProduction(ProductionId),
    Undefined,
}

impl AScriptTypeVal
{
    pub fn is_same_type(&self, other: &Self) -> bool
    {
        discriminant(self) == discriminant(other)
    }

    pub fn type_name(&self, grammar: &GrammarStore) -> String
    {
        match self {
            AScriptTypeVal::Vector(vec) => match vec {
                Some(vector) => format!(
                    "Vector[{}]",
                    vector
                        .iter()
                        .map(|t| { t.type_name(grammar) })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                None => "Vector[Undefined]".to_string(),
            },
            AScriptTypeVal::Struct(..) => "Struct".to_string(),
            AScriptTypeVal::String(..) => "String".to_string(),
            AScriptTypeVal::Bool(..) => "Bool".to_string(),
            AScriptTypeVal::F64(..) => "F64".to_string(),
            AScriptTypeVal::F32(..) => "F32".to_string(),
            AScriptTypeVal::I64(..) => "I64".to_string(),
            AScriptTypeVal::I32(..) => "I32".to_string(),
            AScriptTypeVal::I16(..) => "I16".to_string(),
            AScriptTypeVal::I8(..) => "I8".to_string(),
            AScriptTypeVal::U64(..) => "U64".to_string(),
            AScriptTypeVal::U32(..) => "U32".to_string(),
            AScriptTypeVal::U16(..) => "U16".to_string(),
            AScriptTypeVal::U8(..) => "U8".to_string(),
            AScriptTypeVal::Undefined => "Undefined".to_string(),
            AScriptTypeVal::Token => "Token".to_string(),
            AScriptTypeVal::UnresolvedProduction(id) => {
                let name = get_production_plain_name(id, grammar);
                format!("UnresolvedProduction[{}]", name)
            }
        }
    }
}

pub trait AScriptNumericType
{
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
        impl AScriptNumericType for $struct_name
        {
            type Type = $type;

            #[inline(always)]
            fn none() -> AScriptTypeVal
            {
                AScriptTypeVal::$type_val(None)
            }

            #[inline(always)]
            fn from_f64(val: f64) -> AScriptTypeVal
            {
                AScriptTypeVal::$type_val(Some(val as $conversion_type))
            }

            #[inline(always)]
            fn string_from_f64(val: f64) -> String
            {
                (val as Self::Type).to_string()
            }

            #[inline(always)]
            fn prim_type_name() -> &'static str
            {
                stringify!($type)
            }

            #[inline(always)]
            fn to_fn_name() -> String
            {
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
pub struct AScriptProp
{
    pub type_val: AScriptTypeVal,
    pub first_declared_location: Token,
}

#[derive(Debug)]
pub struct AScriptStruct
{
    pub id: AScriptStructId,
    pub type_name: String,
    pub properties: BTreeSet<AScriptPropId>,
    pub definition_locations: Vec<Token>,
    pub include_token: bool,
}

pub struct AScriptStore
{
    /// Store of unique AScriptStructs
    pub struct_table: BTreeMap<AScriptStructId, AScriptStruct>,
    pub props_table: BTreeMap<AScriptPropId, AScriptProp>,
    pub production_types:
        BTreeMap<ProductionId, HashMap<AScriptTypeVal, Token>>,
    pub body_reduce_expressions: BTreeMap<BodyId, (AScriptTypeVal, ASTNode)>,
}

impl AScriptStore
{
    pub fn new() -> Self
    {
        AScriptStore {
            struct_table: BTreeMap::new(),
            props_table: BTreeMap::new(),
            production_types: BTreeMap::new(),
            body_reduce_expressions: BTreeMap::new(),
        }
    }
}
