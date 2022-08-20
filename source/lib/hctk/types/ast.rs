use super::token::Token;

pub type ReduceFunction<T> = fn(args: &mut Vec<HCObj<T>>, tok: Token) -> HCObj<T>;

#[derive(Debug, Clone)]

pub enum HCObj<T: 'static>
{
  NONE,
  LAZY(Lazy),
  NODE(T),
  NODES(Vec<T>),
  STRING(String),
  STRINGS(Vec<String>),
  F64(f64),
  F32(f32),
  I64(i64),
  I32(i32),
  I16(i16),
  I8(i8),
  U64(u64),
  U32(u32),
  U16(u16),
  U8(u8),
  TOKEN(Token),
  TOKENS(Vec<Token>),
  BOOL(bool),
  OBJECTS(Vec<HCObj<T>>),
}

macro_rules! default_val {
  ($fn_name:ident, $output_type:ident, $default_val: expr) => {
    fn $fn_name(&self) -> $output_type
    {
      $default_val
    }
  };
}

impl<T: 'static> HCObj<T>
{
  pub fn to_node(self) -> Option<T>
  {
    match self {
      Self::NODE(node) => Some(node),
      _ => None,
    }
  }
}
pub trait HCObjTrait
{
  default_val!(to_string, String, String::new());
  default_val!(to_f64, f64, f64::NAN);
  default_val!(to_f32, f32, f32::NAN);
  default_val!(to_u64, u64, 0);
  default_val!(to_u32, u32, 0);
  default_val!(to_u16, u16, 0);
  default_val!(to_u8, u8, 0);
  default_val!(to_i64, i64, 0);
  default_val!(to_i32, i32, 0);
  default_val!(to_i16, i16, 0);
  default_val!(to_i8, i8, 0);
  default_val!(to_bool, bool, false);
  default_val!(to_tok, Token, Token::empty());
}

macro_rules! num_conversion {
  ($fn_name:ident, $output_type:ident, $default_val: expr) => {
    fn $fn_name(&self) -> $output_type
    {
      match self {
        HCObj::TOKEN(tok) => {
          tok.to_string().parse::<$output_type>().unwrap_or($default_val)
        }
        HCObj::STRING(str) => str.parse::<$output_type>().unwrap(),
        HCObj::F64(val) => *val as $output_type,
        HCObj::F32(val) => *val as $output_type,
        HCObj::I64(val) => *val as $output_type,
        HCObj::I32(val) => *val as $output_type,
        HCObj::I16(val) => *val as $output_type,
        HCObj::I8(val) => *val as $output_type,
        HCObj::U64(val) => *val as $output_type,
        HCObj::U32(val) => *val as $output_type,
        HCObj::U16(val) => *val as $output_type,
        HCObj::U8(val) => *val as $output_type,
        HCObj::NODE(node) => node.$fn_name(),
        &HCObj::BOOL(val) => (val as u64) as $output_type,
        _ => $default_val,
      }
    }
  };
}

impl<T: HCObjTrait> HCObjTrait for HCObj<T>
{
  num_conversion!(to_u64, u64, 0);

  num_conversion!(to_u32, u32, 0);

  num_conversion!(to_u16, u16, 0);

  num_conversion!(to_u8, u8, 0);

  num_conversion!(to_i64, i64, 0);

  num_conversion!(to_i32, i32, 0);

  num_conversion!(to_i16, i16, 0);

  num_conversion!(to_i8, i8, 0);

  num_conversion!(to_f64, f64, f64::NAN);

  num_conversion!(to_f32, f32, f32::NAN);

  fn to_string(&self) -> String
  {
    match self {
      HCObj::NODE(node) => node.to_string(),
      HCObj::BOOL(val) => val.to_string(),
      HCObj::F64(val) => val.to_string(),
      HCObj::F32(val) => val.to_string(),
      HCObj::I64(val) => val.to_string(),
      HCObj::I32(val) => val.to_string(),
      HCObj::I16(val) => val.to_string(),
      HCObj::I8(val) => val.to_string(),
      HCObj::U64(val) => val.to_string(),
      HCObj::U32(val) => val.to_string(),
      HCObj::U16(val) => val.to_string(),
      HCObj::U8(val) => val.to_string(),
      HCObj::STRING(string) => string.to_owned(),
      HCObj::TOKEN(val) => val.to_string(),
      _ => String::from(""),
    }
  }

  fn to_tok(&self) -> Token
  {
    match self {
      HCObj::TOKEN(val) => val.clone(),
      _ => Token::empty(),
    }
  }

  fn to_bool(&self) -> bool
  {
    match self {
      HCObj::TOKEN(tok) => match tok.to_string().parse::<f64>() {
        Err(_) => false,
        Ok(val) => val != 0.0,
      },
      HCObj::F64(val) => *val != 0.0,
      HCObj::F32(val) => *val != 0.0,
      HCObj::I64(val) => *val != 0,
      HCObj::I32(val) => *val != 0,
      HCObj::I16(val) => *val != 0,
      HCObj::I8(val) => *val != 0,
      HCObj::U64(val) => *val != 0,
      HCObj::U32(val) => *val != 0,
      HCObj::U16(val) => *val != 0,
      HCObj::U8(val) => *val != 0,
      HCObj::NODE(node) => node.to_bool(),
      &HCObj::BOOL(val) => val,
      _ => false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Lazy
{
  tok:           Token,
  entry_pointer: u32,
  bytecode:      &'static [u32],
}
