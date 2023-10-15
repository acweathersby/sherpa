
/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta2
/// - **SOURCE**: /home/work/projects/lib_sherpa/src/grammar/v2_0_0/grammar.sg
///
/// #### WARNING:
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### License:
/// Copyright (c) 2023 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE.

// 
use std::hash::Hash;
use sherpa_rust_runtime::{
  llvm_parser::*,
  types::{ast::*, Token, TokenRange}, deprecate::*,
};



pub trait ASTParse<T>{
  fn default_from(input:T) -> Result<Vec<ASTNode>, SherpaParseError>;
}

macro_rules! into_vec {
  ($fn_name:ident, $out_type: ty, $type:ident) => {
    pub fn $fn_name(self) -> Vec<$out_type> {
      if let ASTNode::$type(v) = self {
        v
      } else {
        vec![]
      }
    }
  };
}

macro_rules! to_numeric {
  ($fn_name:ident,  $Num:ty) => {
    fn $fn_name(&self) -> $Num {
      if self.is_numeric() || matches!(self, ASTNode::STRING(..) | ASTNode::TOKEN(..)) {
        match self {
          ASTNode::STRING(str) => str.parse::<i64>().unwrap_or(0) as $Num,
          ASTNode::TOKEN(tok) => tok.to_string().parse::<i64>().unwrap_or(0) as $Num,
          ASTNode::F64(val) => *val as $Num,
          ASTNode::F32(val) => *val as $Num,
          ASTNode::I64(val) => *val as $Num,
          ASTNode::I32(val) => *val as $Num,
          ASTNode::I16(val) => *val as $Num,
          ASTNode::U64(val) => *val as $Num,
          ASTNode::U32(val) => *val as $Num,
          ASTNode::U16(val) => *val as $Num,
          ASTNode::U8(val) => *val as $Num,
          ASTNode::BOOL(val) => (*val as usize) as $Num,
          _ => 0 as $Num,
        }
      } else {
        0 as $Num
      }
    }
  };
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[repr(C, u32)]
pub enum ASTNode{
  NONE,
  NODES(Vec<ASTNode>),
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
  BOOL(bool),
  F32Vec(Vec<f32>),
  F64Vec(Vec<f64>),
  I64Vec(Vec<i64>),
  I32Vec(Vec<i32>),
  I16Vec(Vec<i16>),
  I8Vec(Vec<i8>),
  U64Vec(Vec<u64>),
  U32Vec(Vec<u32>),
  U16Vec(Vec<u16>),
  U8Vec(Vec<u8>),
  TOKEN(Token),
  TOKENS(Vec<Token>),
  Indent(Box<Indent>),
  Index(Box<Index>),
  Dedent(Box<Dedent>),
  NewLine(Box<NewLine>),
  Match(Box<Match>),
  Literal(Box<Literal>),
  StringType(Box<StringType>),
  Param(Box<Param>),
  ObjType(Box<ObjType>),
  Call(Box<Call>),
  Expression(Box<Expression>),
  BreakPoint(Box<BreakPoint>),
  Text(Box<Text>),
  Funct(Box<Funct>),
  Space(Box<Space>),
  Prop(Box<Prop>),
  Mul(Box<Mul>),
  Div(Box<Div>),
  IntType(Box<IntType>),
  Add(Box<Add>),
  Obj(Box<Obj>),
  Sub(Box<Sub>),
  Id(Box<Id>),
  SBlock(Box<SBlock>),
  MatchArm(Box<MatchArm>),
  NumType(Box<NumType>),
  Num(Box<Num>),
}

#[derive(Eq, PartialEq, Clone, Copy, Hash)]
 #[cfg_attr(debug_assertions, derive(Debug))]
pub enum ASTNodeType{
  NODES,
  STRING,
  STRINGS,
  F64,
  F32,
  I64,
  I32,
  I16,
  I8,
  U64,
  U32,
  U16,
  U8,
  F32Vec,
  F64Vec,
  I64Vec,
  I32Vec,
  I16Vec,
  I8Vec,
  U64Vec,
  U32Vec,
  U16Vec,
  U8Vec,
  TOKEN,
  TOKENS,
  BOOL,
  NONE,
  Indent,
  Index,
  Dedent,
  NewLine,
  Match,
  Literal,
  StringType,
  Param,
  ObjType,
  Call,
  Expression,
  BreakPoint,
  Text,
  Funct,
  Space,
  Prop,
  Mul,
  Div,
  IntType,
  Add,
  Obj,
  Sub,
  Id,
  SBlock,
  MatchArm,
  NumType,
  Num,
}

impl ASTNode{
  
  into_vec!(into_nodes, ASTNode, NODES);
  into_vec!(into_f64_vec, f64, F64Vec);
  into_vec!(into_f32_vec, f32, F32Vec);
  into_vec!(into_i64_vec, i64, I64Vec);
  into_vec!(into_i32_vec, i32, I32Vec);
  into_vec!(into_i16_vec, i16, I16Vec);
  into_vec!(into_i8_vec, i8, I8Vec);
  into_vec!(into_u64_vec, u64, U64Vec);
  into_vec!(into_u32_vec, u32, U32Vec);
  into_vec!(into_u16_vec, u16, U16Vec);
  into_vec!(into_u8_vec, u8, U8Vec);
  into_vec!(into_tokens, Token, TOKENS);
  to_numeric!(to_i8, i8);
  to_numeric!(to_i16, i16);
  to_numeric!(to_i32, i32);
  to_numeric!(to_i64, i64);
  to_numeric!(to_u8, u8);
  to_numeric!(to_u16, u16);
  to_numeric!(to_u32, u32);
  to_numeric!(to_u64, u64);
  to_numeric!(to_f32, f32);
  to_numeric!(to_f64, f64);
  
  pub fn is_numeric (&self)-> bool {
    use ASTNode::*;
    matches!(self, F64(_) | F32(_)| I64(_)| I32(_)| I16(_)| I8(_)| U64(_)| U32(_)| U16(_)| U8(_))
  }
  
  pub fn to_bool (&self)-> bool {
    self.to_u8() != 0
  }
  
  pub fn into_strings (self)-> Vec<String> {
    
    match self{
      ASTNode::STRINGS(strings) => strings,
      _ => Default::default(),
    }
  }
  
  pub fn to_string (&self)-> String {
    
    match self{
      ASTNode::BOOL(val) => val.to_string(),
      ASTNode::STRING(string) => string.to_owned(),
      ASTNode::TOKEN(val) => val.to_string(),
      _ => self.to_token().to_string(),
    }
  }
  
  pub fn to_token (&self)-> Token {
    
    match self{
      ASTNode::Indent(node) => node.tok.clone(),
      ASTNode::Index(node) => node.tok.clone(),
      ASTNode::Dedent(node) => node.tok.clone(),
      ASTNode::NewLine(node) => node.tok.clone(),
      ASTNode::Match(node) => node.tok.clone(),
      ASTNode::Literal(node) => node.tok.clone(),
      ASTNode::StringType(node) => node.tok.clone(),
      ASTNode::Param(node) => node.tok.clone(),
      ASTNode::ObjType(node) => node.tok.clone(),
      ASTNode::Call(node) => node.tok.clone(),
      ASTNode::Expression(node) => node.tok.clone(),
      ASTNode::BreakPoint(node) => node.tok.clone(),
      ASTNode::Text(node) => node.tok.clone(),
      ASTNode::Funct(node) => node.tok.clone(),
      ASTNode::Space(node) => node.tok.clone(),
      ASTNode::Prop(node) => node.tok.clone(),
      ASTNode::Mul(node) => node.tok.clone(),
      ASTNode::Div(node) => node.tok.clone(),
      ASTNode::IntType(node) => node.tok.clone(),
      ASTNode::Add(node) => node.tok.clone(),
      ASTNode::Obj(node) => node.tok.clone(),
      ASTNode::Sub(node) => node.tok.clone(),
      ASTNode::Id(node) => node.tok.clone(),
      ASTNode::SBlock(node) => node.tok.clone(),
      ASTNode::MatchArm(node) => node.tok.clone(),
      ASTNode::NumType(node) => node.tok.clone(),
      ASTNode::Num(node) => node.tok.clone(),
      ASTNode::TOKEN(val) => val.to_owned(),
      _ => Token::empty(),
    }
  }
}

pub trait GetASTNodeType{
  fn get_type(&self) -> ASTNodeType;
}

impl GetASTNodeType for ASTNode{
  
  fn get_type (&self)-> ASTNodeType {
    
    match self{
      ASTNode::Indent(..) => ASTNodeType::Indent,
      ASTNode::Index(..) => ASTNodeType::Index,
      ASTNode::Dedent(..) => ASTNodeType::Dedent,
      ASTNode::NewLine(..) => ASTNodeType::NewLine,
      ASTNode::Match(..) => ASTNodeType::Match,
      ASTNode::Literal(..) => ASTNodeType::Literal,
      ASTNode::StringType(..) => ASTNodeType::StringType,
      ASTNode::Param(..) => ASTNodeType::Param,
      ASTNode::ObjType(..) => ASTNodeType::ObjType,
      ASTNode::Call(..) => ASTNodeType::Call,
      ASTNode::Expression(..) => ASTNodeType::Expression,
      ASTNode::BreakPoint(..) => ASTNodeType::BreakPoint,
      ASTNode::Text(..) => ASTNodeType::Text,
      ASTNode::Funct(..) => ASTNodeType::Funct,
      ASTNode::Space(..) => ASTNodeType::Space,
      ASTNode::Prop(..) => ASTNodeType::Prop,
      ASTNode::Mul(..) => ASTNodeType::Mul,
      ASTNode::Div(..) => ASTNodeType::Div,
      ASTNode::IntType(..) => ASTNodeType::IntType,
      ASTNode::Add(..) => ASTNodeType::Add,
      ASTNode::Obj(..) => ASTNodeType::Obj,
      ASTNode::Sub(..) => ASTNodeType::Sub,
      ASTNode::Id(..) => ASTNodeType::Id,
      ASTNode::SBlock(..) => ASTNodeType::SBlock,
      ASTNode::MatchArm(..) => ASTNodeType::MatchArm,
      ASTNode::NumType(..) => ASTNodeType::NumType,
      ASTNode::Num(..) => ASTNodeType::Num,
      _ => ASTNodeType::NONE,
    }
  }
}

impl Default for ASTNode{
  
  fn default ()-> Self {
    ASTNode::NONE
  }
}

impl Hash for ASTNode{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    use ASTNode::*;
    
    match self{
      NONE => {},
      F32(val) => val.to_le_bytes().hash(hasher),
      F64(val) => val.to_le_bytes().hash(hasher),
      U8(val) => val.hash(hasher),
      U16(val) => val.hash(hasher),
      U32(val) => val.hash(hasher),
      U64(val) => val.hash(hasher),
      I8(val) => val.hash(hasher),
      I32(val) => val.hash(hasher),
      I16(val) => val.hash(hasher),
      I64(val) => val.hash(hasher),
      BOOL(val) => val.hash(hasher),
      I8Vec(val) => val.hash(hasher),
      I16Vec(val) => val.hash(hasher),
      I32Vec(val) => val.hash(hasher),
      I64Vec(val) => val.hash(hasher),
      U8Vec(val) => val.hash(hasher),
      U16Vec(val) => val.hash(hasher),
      U32Vec(val) => val.hash(hasher),
      U64Vec(val) => val.hash(hasher),
      STRING(string) => string.hash(hasher),
      STRINGS(strings) => strings.hash(hasher),
      Indent(node) => node.hash(hasher),
      Index(node) => node.hash(hasher),
      Dedent(node) => node.hash(hasher),
      NewLine(node) => node.hash(hasher),
      Match(node) => node.hash(hasher),
      Literal(node) => node.hash(hasher),
      StringType(node) => node.hash(hasher),
      Param(node) => node.hash(hasher),
      ObjType(node) => node.hash(hasher),
      Call(node) => node.hash(hasher),
      Expression(node) => node.hash(hasher),
      BreakPoint(node) => node.hash(hasher),
      Text(node) => node.hash(hasher),
      Funct(node) => node.hash(hasher),
      Space(node) => node.hash(hasher),
      Prop(node) => node.hash(hasher),
      Mul(node) => node.hash(hasher),
      Div(node) => node.hash(hasher),
      IntType(node) => node.hash(hasher),
      Add(node) => node.hash(hasher),
      Obj(node) => node.hash(hasher),
      Sub(node) => node.hash(hasher),
      Id(node) => node.hash(hasher),
      SBlock(node) => node.hash(hasher),
      MatchArm(node) => node.hash(hasher),
      NumType(node) => node.hash(hasher),
      Num(node) => node.hash(hasher),
      
      TOKEN(tk) =>{
        tk.to_string().replace(" ", "").replace("\n", "").hash(hasher);
      }
      
      TOKENS(tks) =>{
        
        for tk in tks{
          tk.to_string().replace(" ", "").replace("\n", "").hash(hasher);
        }
      }
      
      NODES(nodes) =>{
        
        for node in nodes{
          node.hash(hasher);
        }
      }
      
      F32Vec(vals) =>{
        
        for v in vals{
          v.to_le_bytes().hash(hasher);
        }
      }
      
      F64Vec(vals) =>{
        
        for v in vals{
          v.to_le_bytes().hash(hasher);
        }
      }
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Indent{
  pub tok: Token, 
}

impl Indent{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Indent
  }
}

impl ASTNode{
  
  pub fn to_Indent (self)-> Box::<Indent> {
    
    match self{
      Self::Indent(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Indent (&self)-> Option<&Indent> {
    
    match self{
      Self::Indent(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Indent_mut (&mut self)-> Option<&mut Indent> {
    
    match self{
      Self::Indent(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Indent{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Index{
  pub expr:ASTNode, 
  pub tok: Token, 
}

impl Index{
  
  pub fn new (expr: ASTNode, tok: Token)-> Self {
    
    Self{
      expr,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Index
  }
}

impl ASTNode{
  
  pub fn to_Index (self)-> Box::<Index> {
    
    match self{
      Self::Index(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Index (&self)-> Option<&Index> {
    
    match self{
      Self::Index(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Index_mut (&mut self)-> Option<&mut Index> {
    
    match self{
      Self::Index(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Index{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expr.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Dedent{
  pub tok: Token, 
}

impl Dedent{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Dedent
  }
}

impl ASTNode{
  
  pub fn to_Dedent (self)-> Box::<Dedent> {
    
    match self{
      Self::Dedent(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Dedent (&self)-> Option<&Dedent> {
    
    match self{
      Self::Dedent(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Dedent_mut (&mut self)-> Option<&mut Dedent> {
    
    match self{
      Self::Dedent(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Dedent{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NewLine{
  pub tok: Token, 
}

impl NewLine{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NewLine
  }
}

impl ASTNode{
  
  pub fn to_NewLine (self)-> Box::<NewLine> {
    
    match self{
      Self::NewLine(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NewLine (&self)-> Option<&NewLine> {
    
    match self{
      Self::NewLine(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NewLine_mut (&mut self)-> Option<&mut NewLine> {
    
    match self{
      Self::NewLine(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NewLine{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Match{
  pub expr:ASTNode, 
  pub matches:Vec<Box<MatchArm>>, 
  pub tok: Token, 
}

impl Match{
  
  pub fn new (expr: ASTNode, matches: Vec<Box<MatchArm>>, tok: Token)-> Self {
    
    Self{
      expr,
      matches,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Match
  }
}

impl ASTNode{
  
  pub fn to_Match (self)-> Box::<Match> {
    
    match self{
      Self::Match(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Match (&self)-> Option<&Match> {
    
    match self{
      Self::Match(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Match_mut (&mut self)-> Option<&mut Match> {
    
    match self{
      Self::Match(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Match{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expr.hash(hasher);
    
    for val in &self.matches{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Literal{
  pub val:String, 
  pub tok: Token, 
}

impl Literal{
  
  pub fn new (val: String, tok: Token)-> Self {
    
    Self{
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Literal
  }
}

impl ASTNode{
  
  pub fn to_Literal (self)-> Box::<Literal> {
    
    match self{
      Self::Literal(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Literal (&self)-> Option<&Literal> {
    
    match self{
      Self::Literal(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Literal_mut (&mut self)-> Option<&mut Literal> {
    
    match self{
      Self::Literal(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Literal{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct StringType{
  pub tok: Token, 
}

impl StringType{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::StringType
  }
}

impl ASTNode{
  
  pub fn to_StringType (self)-> Box::<StringType> {
    
    match self{
      Self::StringType(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_StringType (&self)-> Option<&StringType> {
    
    match self{
      Self::StringType(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_StringType_mut (&mut self)-> Option<&mut StringType> {
    
    match self{
      Self::StringType(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for StringType{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Param{
  pub name:Box<Id>, 
  pub ty:String, 
  pub tok: Token, 
}

impl Param{
  
  pub fn new (name: Box<Id>, ty: String, tok: Token)-> Self {
    
    Self{
      name,
      ty,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Param
  }
}

impl ASTNode{
  
  pub fn to_Param (self)-> Box::<Param> {
    
    match self{
      Self::Param(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Param (&self)-> Option<&Param> {
    
    match self{
      Self::Param(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Param_mut (&mut self)-> Option<&mut Param> {
    
    match self{
      Self::Param(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Param{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
    self.ty.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ObjType{
  pub tok: Token, 
}

impl ObjType{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::ObjType
  }
}

impl ASTNode{
  
  pub fn to_ObjType (self)-> Box::<ObjType> {
    
    match self{
      Self::ObjType(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_ObjType (&self)-> Option<&ObjType> {
    
    match self{
      Self::ObjType(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_ObjType_mut (&mut self)-> Option<&mut ObjType> {
    
    match self{
      Self::ObjType(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for ObjType{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Call{
  pub args:Vec<ASTNode>, 
  pub name:String, 
  pub tok: Token, 
}

impl Call{
  
  pub fn new (args: Vec<ASTNode>, name: String, tok: Token)-> Self {
    
    Self{
      args,
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Call
  }
}

impl ASTNode{
  
  pub fn to_Call (self)-> Box::<Call> {
    
    match self{
      Self::Call(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Call (&self)-> Option<&Call> {
    
    match self{
      Self::Call(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Call_mut (&mut self)-> Option<&mut Call> {
    
    match self{
      Self::Call(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Call{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.args{
      val.hash(hasher);
    }
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Expression{
  pub val:ASTNode, 
  pub tok: Token, 
}

impl Expression{
  
  pub fn new (val: ASTNode, tok: Token)-> Self {
    
    Self{
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Expression
  }
}

impl ASTNode{
  
  pub fn to_Expression (self)-> Box::<Expression> {
    
    match self{
      Self::Expression(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Expression (&self)-> Option<&Expression> {
    
    match self{
      Self::Expression(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Expression_mut (&mut self)-> Option<&mut Expression> {
    
    match self{
      Self::Expression(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Expression{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct BreakPoint{
  pub tok: Token, 
}

impl BreakPoint{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::BreakPoint
  }
}

impl ASTNode{
  
  pub fn to_BreakPoint (self)-> Box::<BreakPoint> {
    
    match self{
      Self::BreakPoint(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_BreakPoint (&self)-> Option<&BreakPoint> {
    
    match self{
      Self::BreakPoint(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_BreakPoint_mut (&mut self)-> Option<&mut BreakPoint> {
    
    match self{
      Self::BreakPoint(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for BreakPoint{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Text{
  pub val:String, 
  pub tok: Token, 
}

impl Text{
  
  pub fn new (val: String, tok: Token)-> Self {
    
    Self{
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Text
  }
}

impl ASTNode{
  
  pub fn to_Text (self)-> Box::<Text> {
    
    match self{
      Self::Text(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Text (&self)-> Option<&Text> {
    
    match self{
      Self::Text(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Text_mut (&mut self)-> Option<&mut Text> {
    
    match self{
      Self::Text(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Text{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Funct{
  pub content:Vec<ASTNode>, 
  pub name:String, 
  pub params:Vec<Box<Param>>, 
  pub tok: Token, 
}

impl Funct{
  
  pub fn new (content: Vec<ASTNode>, name: String, params: Vec<Box<Param>>, tok: Token)-> Self {
    
    Self{
      content,
      name,
      params,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Funct
  }
}

impl ASTNode{
  
  pub fn to_Funct (self)-> Box::<Funct> {
    
    match self{
      Self::Funct(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Funct (&self)-> Option<&Funct> {
    
    match self{
      Self::Funct(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Funct_mut (&mut self)-> Option<&mut Funct> {
    
    match self{
      Self::Funct(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Funct{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.content{
      val.hash(hasher);
    }
    self.name.hash(hasher);
    
    for val in &self.params{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Space{
  pub count:u32, 
  pub tok: Token, 
}

impl Space{
  
  pub fn new (count: u32, tok: Token)-> Self {
    
    Self{
      count,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Space
  }
}

impl ASTNode{
  
  pub fn to_Space (self)-> Box::<Space> {
    
    match self{
      Self::Space(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Space (&self)-> Option<&Space> {
    
    match self{
      Self::Space(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Space_mut (&mut self)-> Option<&mut Space> {
    
    match self{
      Self::Space(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Space{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.count.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Prop{
  pub name:String, 
  pub tok: Token, 
}

impl Prop{
  
  pub fn new (name: String, tok: Token)-> Self {
    
    Self{
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Prop
  }
}

impl ASTNode{
  
  pub fn to_Prop (self)-> Box::<Prop> {
    
    match self{
      Self::Prop(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Prop (&self)-> Option<&Prop> {
    
    match self{
      Self::Prop(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Prop_mut (&mut self)-> Option<&mut Prop> {
    
    match self{
      Self::Prop(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Prop{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Mul{
  pub l:ASTNode, 
  pub r:ASTNode, 
  pub tok: Token, 
}

impl Mul{
  
  pub fn new (l: ASTNode, r: ASTNode, tok: Token)-> Self {
    
    Self{
      l,
      r,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Mul
  }
}

impl ASTNode{
  
  pub fn to_Mul (self)-> Box::<Mul> {
    
    match self{
      Self::Mul(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Mul (&self)-> Option<&Mul> {
    
    match self{
      Self::Mul(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Mul_mut (&mut self)-> Option<&mut Mul> {
    
    match self{
      Self::Mul(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Mul{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.l.hash(hasher);
    self.r.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Div{
  pub l:ASTNode, 
  pub r:ASTNode, 
  pub tok: Token, 
}

impl Div{
  
  pub fn new (l: ASTNode, r: ASTNode, tok: Token)-> Self {
    
    Self{
      l,
      r,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Div
  }
}

impl ASTNode{
  
  pub fn to_Div (self)-> Box::<Div> {
    
    match self{
      Self::Div(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Div (&self)-> Option<&Div> {
    
    match self{
      Self::Div(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Div_mut (&mut self)-> Option<&mut Div> {
    
    match self{
      Self::Div(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Div{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.l.hash(hasher);
    self.r.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct IntType{
  pub tok: Token, 
}

impl IntType{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::IntType
  }
}

impl ASTNode{
  
  pub fn to_IntType (self)-> Box::<IntType> {
    
    match self{
      Self::IntType(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_IntType (&self)-> Option<&IntType> {
    
    match self{
      Self::IntType(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_IntType_mut (&mut self)-> Option<&mut IntType> {
    
    match self{
      Self::IntType(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for IntType{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Add{
  pub l:ASTNode, 
  pub r:ASTNode, 
  pub tok: Token, 
}

impl Add{
  
  pub fn new (l: ASTNode, r: ASTNode, tok: Token)-> Self {
    
    Self{
      l,
      r,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Add
  }
}

impl ASTNode{
  
  pub fn to_Add (self)-> Box::<Add> {
    
    match self{
      Self::Add(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Add (&self)-> Option<&Add> {
    
    match self{
      Self::Add(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Add_mut (&mut self)-> Option<&mut Add> {
    
    match self{
      Self::Add(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Add{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.l.hash(hasher);
    self.r.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Obj{
  pub id:Box<Id>, 
  pub path:Vec<ASTNode>, 
  pub tok: Token, 
}

impl Obj{
  
  pub fn new (id: Box<Id>, path: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      id,
      path,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Obj
  }
}

impl ASTNode{
  
  pub fn to_Obj (self)-> Box::<Obj> {
    
    match self{
      Self::Obj(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Obj (&self)-> Option<&Obj> {
    
    match self{
      Self::Obj(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Obj_mut (&mut self)-> Option<&mut Obj> {
    
    match self{
      Self::Obj(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Obj{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
    
    for val in &self.path{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Sub{
  pub l:ASTNode, 
  pub r:ASTNode, 
  pub tok: Token, 
}

impl Sub{
  
  pub fn new (l: ASTNode, r: ASTNode, tok: Token)-> Self {
    
    Self{
      l,
      r,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Sub
  }
}

impl ASTNode{
  
  pub fn to_Sub (self)-> Box::<Sub> {
    
    match self{
      Self::Sub(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Sub (&self)-> Option<&Sub> {
    
    match self{
      Self::Sub(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Sub_mut (&mut self)-> Option<&mut Sub> {
    
    match self{
      Self::Sub(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Sub{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.l.hash(hasher);
    self.r.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Id{
  pub at:bool, 
  pub name:String, 
  pub tok: Token, 
}

impl Id{
  
  pub fn new (at: bool, name: String, tok: Token)-> Self {
    
    Self{
      at,
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Id
  }
}

impl ASTNode{
  
  pub fn to_Id (self)-> Box::<Id> {
    
    match self{
      Self::Id(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Id (&self)-> Option<&Id> {
    
    match self{
      Self::Id(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Id_mut (&mut self)-> Option<&mut Id> {
    
    match self{
      Self::Id(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Id{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.at.hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SBlock{
  pub content:Vec<ASTNode>, 
  pub ty:String, 
  pub tok: Token, 
}

impl SBlock{
  
  pub fn new (content: Vec<ASTNode>, ty: String, tok: Token)-> Self {
    
    Self{
      content,
      ty,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SBlock
  }
}

impl ASTNode{
  
  pub fn to_SBlock (self)-> Box::<SBlock> {
    
    match self{
      Self::SBlock(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_SBlock (&self)-> Option<&SBlock> {
    
    match self{
      Self::SBlock(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SBlock_mut (&mut self)-> Option<&mut SBlock> {
    
    match self{
      Self::SBlock(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SBlock{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.content{
      val.hash(hasher);
    }
    self.ty.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct MatchArm{
  pub content:Vec<ASTNode>, 
  pub default:bool, 
  pub match_expr:Option<ASTNode>, 
  pub tok: Token, 
}

impl MatchArm{
  
  pub fn new (content: Vec<ASTNode>, default: bool, match_expr: Option<ASTNode>, tok: Token)-> Self {
    
    Self{
      content,
      default,
      match_expr,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::MatchArm
  }
}

impl ASTNode{
  
  pub fn to_MatchArm (self)-> Box::<MatchArm> {
    
    match self{
      Self::MatchArm(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_MatchArm (&self)-> Option<&MatchArm> {
    
    match self{
      Self::MatchArm(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_MatchArm_mut (&mut self)-> Option<&mut MatchArm> {
    
    match self{
      Self::MatchArm(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for MatchArm{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.content{
      val.hash(hasher);
    }
    self.default.hash(hasher);
    self.match_expr.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NumType{
  pub tok: Token, 
}

impl NumType{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NumType
  }
}

impl ASTNode{
  
  pub fn to_NumType (self)-> Box::<NumType> {
    
    match self{
      Self::NumType(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NumType (&self)-> Option<&NumType> {
    
    match self{
      Self::NumType(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NumType_mut (&mut self)-> Option<&mut NumType> {
    
    match self{
      Self::NumType(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NumType{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Num{
  pub val:String, 
  pub tok: Token, 
}

impl Num{
  
  pub fn new (val: String, tok: Token)-> Self {
    
    Self{
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Num
  }
}

impl ASTNode{
  
  pub fn to_Num (self)-> Box::<Num> {
    
    match self{
      Self::Num(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Num (&self)-> Option<&Num> {
    
    match self{
      Self::Num(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Num_mut (&mut self)-> Option<&mut Num> {
    
    match self{
      Self::Num(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Num{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}


/* script_statement+ */
fn reducer_000 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* script_statement */
fn reducer_001 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement+ */
fn reducer_002 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* text */
fn reducer_003 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* function */
fn reducer_004 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* format */
fn reducer_005 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* call */
fn reducer_006 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_007 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* block::<t_SBlock, script_statement+> */
fn reducer_008 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( c:sym ) 
                                    :ast { t_Text, val: str(tok), tok } */
fn reducer_009 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_token(unsafe{&mut *_ctx_}.get_reader_mut());
  let tok_rule_0 = tok_rule_0.to_string();
  let var_2_0 = Text::new(
    tok_rule_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Text(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" "match" ) 
                                    :ast { t_Text, val: str(tok<1>), tok } */
fn reducer_010 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.trim(1, 0);
  let tok_rule_0 = tok_rule_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_2_0 = Text::new(
    tok_rule_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Text(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* num */
fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression */
fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_3_0 = ref_3.into_nodes();
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let obj_1_2 = ref_1.into_nodes();
  let var_6_0 = Funct::new(
    obj_3_0,
    tok_0_1,
    obj_1_2.into_iter().map(|v|match v { ASTNode::Param(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2.into_nodes();
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let var_5_0 = Funct::new(
    obj_2_0,
    tok_0_1,
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let obj_1_2 = ref_1.into_nodes();
  let var_5_0 = Funct::new(
    vec![],
    tok_0_1,
    obj_1_2.into_iter().map(|v|match v { ASTNode::Param(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let var_4_0 = Funct::new(
    vec![],
    tok_0_1,
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* param */
fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* param* */
fn reducer_020 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_021 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement* */
fn reducer_022 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" "+"{:9999})          :ast { t_Indent, tok } */
fn reducer_023 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Indent::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Indent(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" "-"{:9999})          :ast { t_Dedent, tok } */
fn reducer_024 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Dedent::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Dedent(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" ";"{:9999})          :ast { t_BreakPoint, tok } */
fn reducer_025 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = BreakPoint::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::BreakPoint(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "@" tk:(c:num+)               :ast { t_Space, count: u32($2), tok } */
fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_3_0 = Space::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Space(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" " "{:9999})         :ast { t_Space, tok } */
fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Space::new(
    0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Space(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" "n"{:9999})         :ast { t_NewLine, tok } */
fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NewLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NewLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name args              :ast { t_Call, name: str($1), args: $2, tok } */
fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let var_3_0 = Call::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Call(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* id{1} object_accessor*{2}            
                                    :ast { t_Obj, id: $1, path:$2, tok } */
fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_0 = ref_0;
  let obj_0_0 = obj_0_0.to_Id();
  let obj_1_1 = ref_1.into_nodes();
  let var_3_0 = Obj::new(
    obj_0_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Obj(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* id{1} object_accessor*{2}            
                                    :ast { t_Obj, id: $1, path:$2, tok } */
fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let obj_0_0 = obj_0_0.to_Id();
  let var_2_0 = Obj::new(
    obj_0_0,
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Obj(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* object_accessor */
fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* object_accessor* */
fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    vec![],
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    vec![],
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    vec![],
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* script_statement */
fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement+ */
fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* script_statement */
fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement+ */
fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* script_statement */
fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement+ */
fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:( c:num+ ) 
                                    :ast { t_Num, val: str(tok), tok } */
fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_token(unsafe{&mut *_ctx_}.get_reader_mut());
  let tok_rule_0 = tok_rule_0.to_string();
  let var_2_0 = Num::new(
    tok_rule_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Num(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "{{"  c:any+  "}}"     :ast { t_Literal, val: str($2), tok } */
fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_tokens();
  let obj_1_0 = (obj_1_0.first().unwrap() + obj_1_0.last().unwrap()).to_string();
  let var_4_0 = Literal::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Literal(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* c:any */
fn reducer_048 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* c:any+ */
fn reducer_049 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[["  expr  "]]"              :ast { t_Expression, val: $2, tok } */
fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let var_4_0 = Expression::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Expression(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* id ":" type              
                                    :ast { t_Param, name: $1, ty: str($3), tok } */
fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_0_0 = obj_0_0.to_Id();
  let tok_2_1 = __tok_rng_2;
  let tok_2_1 = tok_2_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = Param::new(
    obj_0_0,
    tok_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Param(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* text */
fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* format */
fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* call */
fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* match */
fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* block::<t_SBlock, function_statement+> */
fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:( "#" ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) */
fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "(" expr(*",") ")" 
                                    :ast { [$2] } */
fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "(" expr(*",") ")" 
                                    :ast { [$2] } */
fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = vec![];;
  slots.assign(0, AstSlot(ASTNode::NODES(obj_3_0), __rule_rng__, TokenRange::default()));
}


/* expr */
fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expr(*",") */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "." id                       :ast { t_Prop, name: str($2), tok } */
fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = Prop::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Prop(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* ".[" expr "]"                 :ast { t_Index, expr: $2, tok } */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let var_4_0 = Index::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Index(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* )
                                    :ast { t_Id, name: str($1), at: true, tok } */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = true;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = Id::new(
    obj_2_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Id(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) 
                                    :ast { t_Id, name: str($1), tok } */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_2_0 = Id::new(
    false,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Id(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* expr "+"{1} expr{1}           :ast { t_Add, l:$1, r:$3, tok } */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Add::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Add(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "-"{1} expr{1}           :ast { t_Sub, l:$1, r:$3, tok } */
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Sub::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Sub(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "*"{3} expr{3}           :ast { t_Mul, l:$1, r:$3, tok } */
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Mul::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Mul(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "/"{2} expr{2}           :ast { t_Div, l:$1, r:$3, tok } */
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Div::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Div(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* num */
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "obj"                         :ast { t_ObjType, tok } */
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = ObjType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ObjType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "num"                         :ast { t_NumType, tok } */
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NumType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NumType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "int"                         :ast { t_IntType, tok } */
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = IntType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::IntType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "str"                         :ast { t_StringType, tok } */
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = StringType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::StringType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "match" expr "{"  match_arm*  "}" 
                                    :ast { t_Match, expr: $2, matches:$4, tok } */
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_1_0 = ref_1;
  let obj_3_1 = ref_3.into_nodes();
  let var_6_0 = Match::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::MatchArm(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "match" expr "{"  match_arm*  "}" 
                                    :ast { t_Match, expr: $2, matches:$4, tok } */
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let var_5_0 = Match::new(
    obj_1_0,
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* match_arm */
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* match_arm* */
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    vec![],
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    vec![],
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    obj_1_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    vec![],
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* expr "{" ( function_statement )*{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3, tok } */
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2.into_nodes();
  let obj_0_2 = ref_0;
  let var_5_0 = MatchArm::new(
    obj_2_0,
    false,
    Some(obj_0_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* expr "{" ( function_statement )*{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3, tok } */
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_2 = ref_0;
  let var_4_0 = MatchArm::new(
    vec![],
    false,
    Some(obj_0_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( function_statement )*{2} ";"? "}"
                                    :ast { t_MatchArm, default: true, content: $2, tok } */
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1.into_nodes();
  let obj_5_1 = true;
  let var_6_0 = MatchArm::new(
    obj_1_0,
    obj_5_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( function_statement )*{2} ";"? "}"
                                    :ast { t_MatchArm, default: true, content: $2, tok } */
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_1 = true;
  let var_5_0 = MatchArm::new(
    vec![],
    obj_4_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( function_statement )*{2} ";"? "}"
                                    :ast { t_MatchArm, default: true, content: $2, tok } */
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let obj_4_1 = true;
  let var_5_0 = MatchArm::new(
    obj_1_0,
    obj_4_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( function_statement )*{2} ";"? "}"
                                    :ast { t_MatchArm, default: true, content: $2, tok } */
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_1 = true;
  let var_4_0 = MatchArm::new(
    vec![],
    obj_3_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( function_statement ) */
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( function_statement )* */
fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( function_statement ) */
fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( function_statement )* */
fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 106]
);

impl<R: Reader + UTF8Reader, M, const UP: bool> ReduceFunctions<R, M, UP>{
  
  pub const fn new ()-> Self {
    
    Self([
      reducer_000::<R, M, UP>,
      reducer_001::<R, M, UP>,
      reducer_002::<R, M, UP>,
      reducer_003::<R, M, UP>,
      reducer_004::<R, M, UP>,
      reducer_005::<R, M, UP>,
      reducer_006::<R, M, UP>,
      reducer_007::<R, M, UP>,
      reducer_008::<R, M, UP>,
      reducer_009::<R, M, UP>,
      reducer_010::<R, M, UP>,
      reducer_011::<R, M, UP>,
      reducer_012::<R, M, UP>,
      reducer_013::<R, M, UP>,
      reducer_014::<R, M, UP>,
      reducer_015::<R, M, UP>,
      reducer_016::<R, M, UP>,
      reducer_017::<R, M, UP>,
      reducer_018::<R, M, UP>,
      reducer_019::<R, M, UP>,
      reducer_020::<R, M, UP>,
      reducer_021::<R, M, UP>,
      reducer_022::<R, M, UP>,
      reducer_023::<R, M, UP>,
      reducer_024::<R, M, UP>,
      reducer_025::<R, M, UP>,
      reducer_026::<R, M, UP>,
      reducer_027::<R, M, UP>,
      reducer_028::<R, M, UP>,
      reducer_029::<R, M, UP>,
      reducer_030::<R, M, UP>,
      reducer_031::<R, M, UP>,
      reducer_032::<R, M, UP>,
      reducer_033::<R, M, UP>,
      reducer_034::<R, M, UP>,
      reducer_035::<R, M, UP>,
      reducer_036::<R, M, UP>,
      reducer_037::<R, M, UP>,
      reducer_038::<R, M, UP>,
      reducer_039::<R, M, UP>,
      reducer_040::<R, M, UP>,
      reducer_041::<R, M, UP>,
      reducer_042::<R, M, UP>,
      reducer_043::<R, M, UP>,
      reducer_044::<R, M, UP>,
      reducer_045::<R, M, UP>,
      reducer_046::<R, M, UP>,
      reducer_047::<R, M, UP>,
      reducer_048::<R, M, UP>,
      reducer_049::<R, M, UP>,
      reducer_050::<R, M, UP>,
      reducer_051::<R, M, UP>,
      reducer_052::<R, M, UP>,
      reducer_053::<R, M, UP>,
      reducer_054::<R, M, UP>,
      reducer_055::<R, M, UP>,
      reducer_056::<R, M, UP>,
      reducer_057::<R, M, UP>,
      reducer_058::<R, M, UP>,
      reducer_059::<R, M, UP>,
      reducer_060::<R, M, UP>,
      reducer_061::<R, M, UP>,
      reducer_062::<R, M, UP>,
      reducer_063::<R, M, UP>,
      reducer_064::<R, M, UP>,
      reducer_065::<R, M, UP>,
      reducer_066::<R, M, UP>,
      reducer_067::<R, M, UP>,
      reducer_068::<R, M, UP>,
      reducer_069::<R, M, UP>,
      reducer_070::<R, M, UP>,
      reducer_071::<R, M, UP>,
      reducer_072::<R, M, UP>,
      reducer_073::<R, M, UP>,
      reducer_074::<R, M, UP>,
      reducer_075::<R, M, UP>,
      reducer_076::<R, M, UP>,
      reducer_077::<R, M, UP>,
      reducer_078::<R, M, UP>,
      reducer_079::<R, M, UP>,
      reducer_080::<R, M, UP>,
      reducer_081::<R, M, UP>,
      reducer_082::<R, M, UP>,
      reducer_083::<R, M, UP>,
      reducer_084::<R, M, UP>,
      reducer_085::<R, M, UP>,
      reducer_086::<R, M, UP>,
      reducer_087::<R, M, UP>,
      reducer_088::<R, M, UP>,
      reducer_089::<R, M, UP>,
      reducer_090::<R, M, UP>,
      reducer_091::<R, M, UP>,
      reducer_092::<R, M, UP>,
      reducer_093::<R, M, UP>,
      reducer_094::<R, M, UP>,
      reducer_095::<R, M, UP>,
      reducer_096::<R, M, UP>,
      reducer_097::<R, M, UP>,
      reducer_098::<R, M, UP>,
      reducer_099::<R, M, UP>,
      reducer_100::<R, M, UP>,
      reducer_101::<R, M, UP>,
      reducer_102::<R, M, UP>,
      reducer_103::<R, M, UP>,
      reducer_104::<R, M, UP>,
      reducer_105::<R, M, UP>,
    ])
  }
}
    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<T, UserCTX, Bytecode> = sherpa_rust_runtime::deprecate::ByteCodeParser<T, UserCTX, Bytecode>;

pub mod meta{
  
  pub const nonterm_names: [&'static str;40] = [
    "ascript_form",
    "ascript_form_list",
    "script_statement",
    "text",
    "text_group",
    "function",
    "function_list",
    "function_list_1",
    "format",
    "call",
    "object",
    "object_list",
    "block_template_7612667743488587483",
    "block_template_7612667743488587483_list",
    "block_template_7612667743488587483_list_1",
    "block_template_7612667743488587483_list_2",
    "num",
    "literal",
    "literal_list",
    "expression",
    "param",
    "function_statement",
    "fn_name",
    "args",
    "args_list",
    "object_accessor",
    "id",
    "expr",
    "type",
    "match",
    "match_list",
    "block_template_17636207641572139789",
    "block_template_17636207641572139789_list",
    "block_template_17636207641572139789_list_1",
    "block_template_17636207641572139789_list_2",
    "match_arm",
    "match_arm_group",
    "match_arm_list_1",
    "match_arm_group_2",
    "match_arm_list_3",
  ];
  
  pub const symbol_string: [&'static str;42] = [
    r####"Default"####,
    r####"c:sp"####,
    r####"c:nl"####,
    r####"c:sym"####,
    r####"tk:nonterm"####,
    r####" { "####,
    r####" } "####,
    r####" @ "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"nonterm"####,
    r####" ( "####,
    r####" ) "####,
    r####" [ "####,
    r####" ] "####,
    r####"c:any"####,
    r####" {{ "####,
    r####" }} "####,
    r####" [[ "####,
    r####" ]] "####,
    r####" : "####,
    r####"tk:nonterm"####,
    r####" , "####,
    r####" . "####,
    r####" .[ "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" * "####,
    r####" + "####,
    r####" - "####,
    r####" / "####,
    r####"nonterm"####,
    r####" obj "####,
    r####" num "####,
    r####" str "####,
    r####" int "####,
    r####" match "####,
    r####" ; "####,
  ];
}

pub fn new_default_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(8);
  parser
}

pub static bytecode: [u8; 41314] = [
  0,211,200,197,210,208,193,2,15,1,103,133,0,0,17,1,21,0,0,0,1,21,1,189,2,0,0,104,133,0,0,19,0,0,0,4,0,0,0,17,16,8,128,1,240,194,127,2,240,2,128,3,160,20,128,4,88,19,131,5, 
  136,18,128,22,16,6,128,7,184,17,128,8,112,16,128,9,40,15,130,10,224,13,128,11,64,12,128,12,248,10,128,13,176,73,129,30,248,2,128,15,224,8,128,20,16,7,128,25,72,5,128,29,32,4,128,8,4,19,26,0, 
  0,0,66,0,0,0,1,0,15,1,231,127,0,0,15,1,79,133,0,0,15,1,91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,231,127,0,0,15,1,79,133,0,0,15,1,91, 
  133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,231,127,0,0,17,1,202,125,0,0,1,4,15,1,231,127,0,0,15,1,79,133,0,0,15,1,178,125,0,0,15,1,190,125,0,0, 
  17,1,96,118,0,0,1,4,15,1,231,127,0,0,15,1,79,133,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,231,127,0,0,15,1,79,133,0,0,15,1,173,117,0,0,17,1, 
  132,108,0,0,1,4,15,1,231,127,0,0,15,1,79,133,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1, 
  0,0,0,1,0,17,1,231,127,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0,0,1,4,19,16,0,0,0,46, 
  0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0, 
  0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0, 
  17,1,231,127,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0,0,1,4,15,1,231,127,0,0,15,1,79,133,0, 
  0,15,1,79,99,0,0,17,1,34,99,0,0,1,4,15,1,231,127,0,0,15,1,79,133,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0, 
  0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0, 
  0,1,2,21,1,206,2,0,0,206,136,0,0,20,0,0,0,4,0,0,0,17,48,8,128,1,16,195,127,2,16,3,128,3,40,21,128,4,224,19,131,5,16,19,128,6,168,210,130,7,216,17,128,8,144,16,128,9,72,79, 
  130,10,0,14,128,11,96,12,128,12,24,11,128,13,208,137,129,30,24,3,128,15,0,9,128,20,48,7,128,22,48,6,128,25,104,5,128,29,64,4,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,2,90,0,0, 
  15,1,22,99,0,0,15,1,91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,2,90,0,0,15,1,22,99,0,0,15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0, 
  0,0,58,0,0,0,1,0,15,1,2,90,0,0,17,1,162,5,0,0,1,4,15,1,2,90,0,0,15,1,22,99,0,0,15,1,178,125,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,2,90,0,0, 
  15,1,22,99,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,2,90,0,0,15,1,22,99,0,0,15,1,173,117,0,0,17,1,132,108,0,0,1,4,15,1,2,90,0,0,15,1, 
  22,99,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,4,19, 
  8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0, 
  1,0,19,2,0,0,0,3,0,0,0,1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,44, 
  0,0,0,1,0,17,1,2,90,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,4,19,8,0,0,0,23, 
  0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,4,15,1,2,90,0,0,15,1,22,99,0,0,15,1,79,99,0,0,17,1,34,99,0,0,1, 
  4,19,12,0,0,0,39,0,0,0,2,0,1,4,15,1,2,90,0,0,15,1,22,99,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0, 
  1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0, 
  1,2,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,29, 
  82,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,29,82,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0, 
  1,4,17,1,34,6,0,0,1,2,21,1,248,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,128,9,128,1,48,195,127,2,48,3,128,3,120,22,128,4,48,21,131,5,96,20,128,6,248,211,130,7,40,19,128, 
  8,224,17,131,9,152,80,130,10,80,15,128,11,176,13,128,12,104,12,128,13,32,139,129,30,8,4,128,15,80,10,128,20,128,8,128,22,128,7,128,25,88,6,128,29,48,5,128,40,56,3,128,8,4,15,1,206,67,0,0, 
  15,1,230,71,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,206,67,0,0,15,1,230,71,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0, 
  0,0,65,0,0,0,1,0,15,1,206,67,0,0,15,1,230,71,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,206,67,0,0,15,1,230,71,0,0,15,1,234, 
  46,0,0,17,1,170,46,0,0,1,4,15,1,206,67,0,0,15,1,230,71,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,206,67,0,0,15,1,230,71,0,0,15,1,158,46,0, 
  0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,206,67,0,0,15,1,230,71,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,206,67,0,0,15,1,230,71,0,0,15,1,146,46,0,0,17, 
  1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19, 
  21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0, 
  1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0, 
  1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53, 
  0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,4,15,1,206,67,0,0,15,1,230,71,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,5,0,0,0,18,0,0,0,3, 
  0,1,4,15,1,206,67,0,0,15,1,230,71,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,21,0,0,0, 
  1,0,17,1,206,67,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,2,21,1,248,2,0,0,77,138,0, 
  0,21,0,0,0,4,0,0,0,17,128,9,128,1,48,195,127,2,48,3,128,3,120,22,128,4,48,21,131,5,96,20,128,6,248,211,130,7,40,19,128,8,224,17,131,9,152,80,130,10,80,15,128,11,176,13,128,12,104,12, 
  128,13,32,139,129,30,8,4,128,15,80,10,128,20,128,8,128,22,128,7,128,25,88,6,128,29,48,5,128,40,56,3,128,8,4,15,1,20,12,0,0,15,1,52,31,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1, 
  4,19,26,0,0,0,66,0,0,0,1,0,15,1,20,12,0,0,15,1,52,31,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,20,12,0,0,15,1,52,31,0, 
  0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,20,12,0,0,15,1,52,31,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,20,12,0,0,15,1, 
  52,31,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,20,12,0,0,15,1,52,31,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,20,12, 
  0,0,15,1,52,31,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,20,12,0,0,15,1,52,31,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19, 
  21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20,12,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0, 
  1,0,17,1,20,12,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20,12,0,0, 
  1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20,12,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53, 
  0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20,12,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20, 
  12,0,0,1,4,15,1,20,12,0,0,15,1,52,31,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,31,0,0,0,87,0,0,0,2,0,1,4,15,1,20,12,0,0,15,1,52,31,0,0,15,1,146,46, 
  0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20,12,0,0,1,4,19,3,0,0,0,9,0,0,0, 
  1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,92,0,0,0,1,0,17,1,20,12,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,8,232,7,129,9,88,199,129,10,200, 
  6,130,3,120,136,129,16,56,6,128,21,136,132,129,22,32,4,128,31,152,2,128,17,168,5,128,19,24,5,128,26,184,131,128,29,40,3,128,34,48,2,128,15,1,20,12,0,0,17,1,154,28,0,0,1,19,21,0,0,0, 
  57,0,0,0,1,0,17,1,20,12,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,20,12,0,0,1,15,1,20,12,0,0,17,1,54,13,0,0,1,15,1,20,12,0,0,17,1,170,46,0,0,1,19,34,0, 
  0,0,92,0,0,0,1,0,17,1,20,12,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,20,12,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,20,12,0,0,1,19,3,0,0,0,11,0,0,0, 
  1,0,17,1,20,12,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,20,12,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,20,12,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,20,12, 
  0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,20,12,0,0,1,2,21,1,75,0,0,0,190,140,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,80,2,128,2,80,2,128,27,176,1,128,4,15,1,3, 
  28,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,4,15,1,3,28,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,1,83,0,0,0,188,141,0,0,4, 
  0,0,0,2,0,0,0,29,216,1,128,1,16,193,127,2,16,65,128,30,24,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,19,25,0,0,0,63,0,0,0,2,0,1,4,19,26,0,0,0,65,0,0,0,1,0, 
  19,25,0,0,0,63,0,0,0,2,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0, 
  0,0,66,0,0,0,1,0,15,1,137,14,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,137,14,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15, 
  1,137,14,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,137,14,0,0,1,2,19,27,0,0,0,73,0,0,0,1,0, 
  1,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,16,184,2,128,17,40,194,128,10,72,195,128,27,80,1,128,25,32,2,128,26,184,1,128,15,1,137,14,0,0,17,1,243,15,0,0,1,15,1,137,14, 
  0,0,17,1,5,15,0,0,1,1,19,27,0,0,0,73,0,0,0,1,0,17,1,137,14,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,137,14,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,137, 
  14,0,0,1,2,21,1,75,0,0,0,188,142,0,0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,92,15,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,4, 
  15,1,92,15,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240, 
  0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,92,15,0,0,1,15,1,92,15,0,0,17,1,155,15,0,0,1,1,2,21,1,63,0,0,0,188,142,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1, 
  128,2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,19,11,0,0,0,33,0,0,0, 
  2,0,1,21,7,84,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,48,1,128,45,112,193,128,42,240,1,128,43,176,65,127,93,48,2,128,4,17,1,228,26,0,0,1,4,17,1,209,25,0,0,1,4,17,1, 
  8,20,0,0,1,4,17,1,99,16,0,0,1,4,19,25,0,0,0,64,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,155,0,0, 
  0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,255,16,0,0,15,1,247,27, 
  0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,255,16,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,255,16,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1, 
  4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,255,16,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131, 
  128,27,48,1,128,26,152,1,128,15,1,255,16,0,0,17,1,205,17,0,0,1,15,1,255,16,0,0,17,1,118,17,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,255,16,0,0,1,19,27,0,0,0,71,0, 
  0,0,1,0,17,1,255,16,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,255,16,0,0,1,2,21,1,75,0,0,0,188,142,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,80,2,128,2,80,2,128, 
  27,176,1,128,4,15,1,92,15,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,4,15,1,92,15,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,7,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,4,17,1,19,18,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,69, 
  0,0,0,3,0,14,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0, 
  1,0,15,1,175,18,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,175,18,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,175,18,0,0,15, 
  1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,175,18,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0, 
  0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,175,18,0,0,17,1,205,17,0,0,1,15,1,175,18,0,0,17,1,38,19,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1, 
  175,18,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,175,18,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,175,18,0,0,1,2,21,1,75,0,0,0,60,143,0,0,4,0,0,0,2,0,0,0, 
  28,16,1,128,1,176,1,128,2,176,1,128,27,184,1,128,4,15,1,125,19,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,4,15,1,125,19,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,19,10,0, 
  0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,125,19,0,0,1,15,1,125, 
  19,0,0,17,1,188,19,0,0,1,1,2,21,1,63,0,0,0,60,143,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1, 
  4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232, 
  3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,164,20,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,164,20,0,0, 
  15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,164,20,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,164,20, 
  0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,164,20,0,0,17,1,27,21,0,0,1,15,1,164,20,0, 
  0,17,1,118,17,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,164,20,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,164,20,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,164,20,0, 
  0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,80,1,128,45,208,1,128,42,16,1,128,43,144,65,127,4,17,1,19,18,0,0,1,4,17,1,108,24,0,0,1,4,17,1,89,23,0,0, 
  1,4,17,1,133,21,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,67,0,0,0,3,0,14,1,21,1,155,0,0,0,33,142,0,0,6, 
  0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,33,22,0,0,15,1,247,27,0,0,17,1,144,127, 
  0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,33,22,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,33,22,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0, 
  46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,33,22,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26, 
  152,1,128,15,1,33,22,0,0,17,1,239,22,0,0,1,15,1,33,22,0,0,17,1,152,22,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,33,22,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1, 
  33,22,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,33,22,0,0,1,2,21,1,75,0,0,0,60,143,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,80,2,128,2,80,2,128,27,176,1,128,4,15, 
  1,125,19,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,4,15,1,125,19,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,7,66,0,0,0,255,255,255, 
  255,4,0,0,0,2,0,0,0,47,144,1,128,45,16,1,128,42,208,1,128,43,80,65,127,4,17,1,133,21,0,0,1,4,17,1,89,23,0,0,1,4,17,1,108,24,0,0,1,4,17,1,19,18,0,0,1,21,9,27, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,68,0,0,0,3,0,14,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1, 
  80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,245,23,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0, 
  0,1,0,15,1,245,23,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,245,23,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71, 
  0,0,0,1,0,17,1,245,23,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,245,23,0,0,17,1,27, 
  21,0,0,1,15,1,245,23,0,0,17,1,152,22,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,245,23,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,245,23,0,0,1,19,27,0,0,0,72,0, 
  0,0,1,0,17,1,245,23,0,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0, 
  0,66,0,0,0,1,0,15,1,8,25,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,8,25,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1, 
  8,25,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,8,25,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,8,25,0,0,17,1,127,25,0,0,1,15,1,8,25,0,0,17,1,38,19,0,0,1,19,27,0,0,0,73,0,0, 
  0,1,0,17,1,8,25,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,8,25,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,8,25,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,42,208,0,128,47,16,1,128,4,17,1,19,18,0,0,1,4,17,1,108,24,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0, 
  0,70,0,0,0,3,0,14,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0, 
  0,0,1,0,15,1,109,26,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,109,26,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,109,26,0, 
  0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,109,26,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,109,26,0,0,17,1,239,22,0,0,1,15,1,109,26,0,0,17,1,118,17,0,0,1,19,27,0,0,0,73,0,0,0,1,0, 
  17,1,109,26,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,109,26,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,109,26,0,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0, 
  0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,128,27,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19, 
  26,0,0,0,65,0,0,0,1,0,15,1,128,27,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,128,27,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1, 
  0,19,27,0,0,0,71,0,0,0,1,0,17,1,128,27,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1, 
  128,27,0,0,17,1,127,25,0,0,1,15,1,128,27,0,0,17,1,118,17,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,128,27,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,128,27,0,0,1, 
  19,27,0,0,0,72,0,0,0,1,0,17,1,128,27,0,0,1,2,19,27,0,0,0,72,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0, 
  128,19,11,0,0,0,32,0,0,0,1,0,17,1,3,28,0,0,1,15,1,3,28,0,0,17,1,66,28,0,0,1,1,2,21,1,63,0,0,0,190,140,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128, 
  2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,19,11,0,0,0,32,0,0,0,1, 
  0,1,21,1,141,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,96,8,128,1,48,195,127,2,48,3,128,3,80,19,128,4,56,18,131,5,152,17,128,6,40,209,130,7,136,16,128,8,112,15,131,9,88,78,130, 
  10,64,13,128,11,208,11,128,12,184,10,128,13,160,137,129,30,216,3,128,15,0,9,128,20,144,7,128,22,192,6,128,25,200,5,128,29,208,4,128,40,56,3,128,8,4,15,1,40,31,0,0,15,1,242,71,0,0,17,1, 
  2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,40,31,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,40,31,0,0,15,1,246,46,0,0, 
  17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,40,31,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,40,31,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96, 
  118,0,0,1,4,15,1,40,31,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,40,31,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,40,31,0,0,15,1,146, 
  46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0, 
  0,0,53,0,0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,93, 
  0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0, 
  0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,4,15,1,40,31,0,0,15,1, 
  64,31,0,0,17,1,34,99,0,0,1,4,19,31,0,0,0,86,0,0,0,3,0,14,1,4,15,1,40,31,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0, 
  0,0,52,0,0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,93,0,0,0,2,0,1,2,19,34,0,0, 
  0,93,0,0,0,2,0,1,19,34,0,0,0,92,0,0,0,1,0,1,19,21,0,0,0,53,0,0,0,1,0,1,21,1,248,2,0,0,188,143,0,0,21,0,0,0,4,0,0,0,16,80,10,128,1,48,195,131,2,48, 
  3,128,3,120,22,128,4,48,85,131,5,96,20,128,22,128,7,128,7,144,19,128,8,72,18,131,9,0,81,130,10,184,15,128,11,24,14,128,12,208,12,128,13,136,139,129,30,8,4,128,15,184,10,128,17,128,9,128,20,128, 
  8,128,25,88,6,128,29,48,5,128,40,56,3,128,8,4,15,1,69,34,0,0,15,1,227,38,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,69,34,0,0,15, 
  1,227,38,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,69,34,0,0,15,1,227,38,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0, 
  0,58,0,0,0,1,0,15,1,69,34,0,0,15,1,227,38,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,69,34,0,0,15,1,227,38,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96, 
  118,0,0,1,4,15,1,69,34,0,0,15,1,227,38,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,69,34,0,0,15,1,227,38,0,0,15,1,146,46,0,0,17,1,239,38,0, 
  0,1,4,19,31,0,0,0,85,0,0,0,2,0,1,4,15,1,69,34,0,0,15,1,227,38,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0, 
  0,0,1,0,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34, 
  0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,4,19,8,0,0, 
  0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19, 
  33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,4,15, 
  1,69,34,0,0,15,1,227,38,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,15,1,69,34,0,0,15,1,227,38,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0, 
  1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,90, 
  0,0,0,1,0,17,1,69,34,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,8,232,7,129,9,88,199,129,10,200,6,130,3,120,136,129,16,56,6,128,21,136,132,129,22,32,4,128,31, 
  152,2,128,17,168,5,129,19,24,5,128,26,184,3,128,29,40,3,128,33,48,2,128,15,1,69,34,0,0,17,1,73,36,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,69,34,0,0,1,19,21,0,0,0,55, 
  0,0,0,1,0,17,1,69,34,0,0,1,15,1,69,34,0,0,17,1,103,35,0,0,1,15,1,69,34,0,0,17,1,170,46,0,0,1,19,33,0,0,0,90,0,0,0,1,0,17,1,69,34,0,0,1,19,3,0,0, 
  0,13,0,0,0,1,0,17,1,69,34,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,69,34,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,69,34,0,0,1,19,21,0,0,0,56,0,0,0,1, 
  0,17,1,69,34,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,69,34,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,69,34,0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,69,34,0, 
  0,1,2,21,1,75,0,0,0,139,144,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,80,2,128,2,80,2,128,27,176,1,128,4,15,1,190,35,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,4,15,1, 
  190,35,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128, 
  19,11,0,0,0,32,0,0,0,1,0,17,1,190,35,0,0,1,15,1,190,35,0,0,17,1,253,35,0,0,1,1,2,21,1,63,0,0,0,139,144,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2, 
  240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,21,1,141,2,0,0,188,143,0,0,21, 
  0,0,0,4,0,0,0,16,0,9,128,1,48,195,131,2,48,3,128,3,80,19,128,4,56,82,131,5,152,17,128,22,192,6,128,7,248,16,128,8,224,15,131,9,200,78,130,10,176,13,128,11,64,12,128,12,40,11,128,13, 
  16,138,129,30,216,3,128,15,112,9,128,17,96,8,128,20,144,7,128,25,200,5,128,29,208,4,128,40,56,3,128,8,4,15,1,215,38,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0, 
  0,0,1,0,15,1,215,38,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,215,38,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0, 
  58,0,0,0,1,0,15,1,215,38,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,215,38,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,215,38,0,0,15,1, 
  158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,215,38,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,19,31,0,0,0,84,0,0,0,3,0,14,1,4,15,1,215,38,0,0,15,1, 
  146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,91,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21, 
  0,0,0,53,0,0,0,1,0,19,33,0,0,0,91,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0, 
  91,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,91,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53, 
  0,0,0,1,0,19,33,0,0,0,91,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,91,0,0,0,2,0,1,4,15,1,215,38,0,0,15, 
  1,64,31,0,0,17,1,34,99,0,0,1,4,15,1,215,38,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0, 
  91,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,91,0,0,0,2,0,1,2,19,33,0,0,0,91,0,0,0,2,0,1,19,33,0,0,0, 
  90,0,0,0,1,0,1,21,1,248,2,0,0,102,145,0,0,21,0,0,0,4,0,0,0,17,232,9,128,1,48,195,127,2,48,131,131,3,120,22,128,4,48,85,131,5,96,20,128,22,128,7,128,7,144,19,128,8,72,18, 
  131,9,0,81,130,10,184,15,128,11,24,14,128,12,208,12,128,13,136,139,129,30,8,4,128,15,184,10,128,18,128,9,128,20,128,8,128,25,88,6,128,29,48,5,128,40,56,3,128,8,4,15,1,232,41,0,0,15,1,134, 
  46,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,232,41,0,0,15,1,134,46,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65, 
  0,0,0,1,0,15,1,232,41,0,0,15,1,134,46,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,232,41,0,0,15,1,134,46,0,0,15,1,234,46,0,0, 
  17,1,170,46,0,0,1,4,15,1,232,41,0,0,15,1,134,46,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,232,41,0,0,15,1,134,46,0,0,15,1,158,46,0,0,15,1, 
  84,118,0,0,17,1,185,117,0,0,1,4,19,31,0,0,0,83,0,0,0,2,0,1,4,15,1,232,41,0,0,15,1,134,46,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,232,41,0,0,15,1,134, 
  46,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,4,19,8, 
  0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1, 
  0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0, 
  0,0,1,0,17,1,232,41,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,4,19,8,0,0,0,23,0, 
  0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,4,15,1,232,41,0,0,15,1,134,46,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4, 
  15,1,232,41,0,0,15,1,134,46,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17, 
  1,232,41,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0, 
  0,0,3,0,0,0,8,232,7,129,9,88,199,129,10,200,6,130,3,120,136,129,16,56,6,130,21,136,132,129,22,32,4,128,31,152,2,128,17,168,5,128,19,24,5,128,26,184,3,128,29,40,3,128,32,48,2,128,15,1, 
  232,41,0,0,17,1,236,43,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,232,41,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,232,41,0,0,1,15,1,232,41,0,0,17,1,10,43,0,0,1, 
  15,1,232,41,0,0,17,1,170,46,0,0,1,19,32,0,0,0,88,0,0,0,1,0,17,1,232,41,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,232,41,0,0,1,19,3,0,0,0,12,0,0,0,1,0, 
  17,1,232,41,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,232,41,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,232,41,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,232,41,0,0, 
  1,19,21,0,0,0,53,0,0,0,1,0,17,1,232,41,0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,232,41,0,0,1,2,21,1,75,0,0,0,53,146,0,0,4,0,0,0,2,0,0,0,28,176,1,128, 
  1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,97,43,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,4,15,1,97,43,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0, 
  0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,97,43,0,0,1,15,1,97,43,0,0,17, 
  1,160,43,0,0,1,1,2,21,1,63,0,0,0,53,146,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231, 
  15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,21,1,141,2,0,0,102,145,0,0,21,0,0,0,4,0,0,0,17,208,8,128,1,48,195,127,2,48,131,131,3,80,19,128,4,56, 
  82,131,5,152,17,128,22,192,6,128,7,248,16,128,8,224,15,131,9,200,78,130,10,176,13,128,11,64,12,128,12,40,11,128,13,16,138,129,30,216,3,128,15,112,9,128,18,96,8,128,20,144,7,128,25,200,5,128,29,208, 
  4,128,40,56,3,128,8,4,15,1,122,46,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,122,46,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19, 
  26,0,0,0,65,0,0,0,1,0,15,1,122,46,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,122,46,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1, 
  4,15,1,122,46,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,122,46,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,19,31,0,0,0,82,0, 
  0,0,3,0,14,1,4,15,1,122,46,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,122,46,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21, 
  0,0,0,53,0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,19,16,0, 
  0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53, 
  0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,19,8,0,0,0,23,0, 
  0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,15,1,122,46,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,15,1,122,46,0,0,15,1,146,46,0,0, 
  17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52, 
  0,0,0,1,0,19,32,0,0,0,89,0,0,0,2,0,1,2,19,32,0,0,0,89,0,0,0,2,0,1,19,32,0,0,0,88,0,0,0,1,0,1,19,21,0,0,0,57,0,0,0,1,0,1,19,21,0,0,0,52, 
  0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,75,81,0,0,17,1,254,71,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,208,0,128,8,2,19,21,0,0,0,54,0,0,0,1,0,1,19,21,0,0,0,56,0,0,0,1,0,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128, 
  2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,158,47,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0, 
  15,1,158,47,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,158,47,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0, 
  1,0,17,1,158,47,0,0,1,2,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,16,184,2,128,17,40,2,129,10,72,131,128,27,88,1,128,26,192,1,128,29,80,1,128,1,15,1,158,47,0,0,17, 
  1,113,48,0,0,1,15,1,158,47,0,0,17,1,26,48,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,158,47,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,158,47,0,0,1,19,27,0,0,0, 
  72,0,0,0,1,0,17,1,158,47,0,0,1,2,21,1,75,0,0,0,60,143,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,80,2,128,2,80,2,128,27,176,1,128,4,15,1,125,19,0,0,15,1,142,28,0, 
  0,17,1,225,13,0,0,1,4,15,1,125,19,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,7,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47, 
  48,1,129,45,112,1,128,42,240,1,128,43,176,65,127,123,48,2,128,4,17,1,108,24,0,0,1,4,17,1,133,21,0,0,1,4,17,1,89,23,0,0,1,4,17,1,19,18,0,0,1,4,17,1,219,48,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,197,0,0,0,16,147,0,0,8,0,0,0,3,0,0,0,29,144,2,128,1,144,1,128,2,144,1,128,11,40,4, 
  128,20,136,3,128,5,136,197,126,6,24,69,128,30,152,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,236,57,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1, 
  0,15,1,236,57,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,236,57,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0, 
  0,1,0,17,1,236,57,0,0,1,4,19,29,0,0,0,79,0,0,0,4,0,14,1,4,15,1,236,57,0,0,15,1,224,57,0,0,17,1,161,49,0,0,1,2,21,1,4,3,0,0,152,147,0,0,22,0,0,0,4, 
  0,0,0,17,224,9,128,1,80,195,127,2,80,3,128,3,216,22,128,4,144,21,131,5,192,20,128,6,88,212,130,7,136,19,128,8,64,18,131,9,248,80,130,10,176,15,128,11,16,14,128,12,200,12,128,13,128,139,129,30, 
  104,4,128,15,176,10,128,20,224,8,128,22,224,7,128,25,184,198,128,29,144,5,128,40,152,3,128,41,88,3,128,8,4,17,1,161,57,0,0,1,4,15,1,166,52,0,0,15,1,149,57,0,0,15,1,242,71,0,0,17, 
  1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,166,52,0,0,15,1,149,57,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,166,52,0, 
  0,15,1,149,57,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,166,52,0,0,15,1,149,57,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1, 
  166,52,0,0,15,1,149,57,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,166,52,0,0,15,1,149,57,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0, 
  1,4,15,1,166,52,0,0,15,1,149,57,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,166,52,0,0,15,1,149,57,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28, 
  0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0, 
  0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,104,0,0,0,1,0, 
  17,1,166,52,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0, 
  19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,104,0,0, 
  0,1,0,17,1,166,52,0,0,1,4,15,1,166,52,0,0,15,1,149,57,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,35,0,0,0,99,0,0,0,2,0,1,4,15,1,166,52,0,0,15,1,149,57, 
  0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,4,19,3,0, 
  0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,2,21,0,38,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,16,8,129, 
  9,128,199,129,10,240,6,130,3,160,136,129,16,96,6,128,21,176,132,129,22,72,4,128,31,192,130,129,17,208,5,128,19,64,197,128,26,224,3,128,29,80,3,128,35,184,2,128,39,80,2,128,15,1,166,52,0,0,17,1, 
  175,54,0,0,1,1,19,21,0,0,0,57,0,0,0,1,0,17,1,166,52,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,166,52,0,0,1,15,1,166,52,0,0,17,1,205,53,0,0,1,15,1,166,52,0, 
  0,17,1,170,46,0,0,1,19,39,0,0,0,104,0,0,0,1,0,17,1,166,52,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,166,52,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,166,52,0, 
  0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,166,52,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,166,52,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,166,52,0,0,1,19,21,0,0, 
  0,53,0,0,0,1,0,17,1,166,52,0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,166,52,0,0,1,2,21,1,75,0,0,0,114,148,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,176,1,128,2, 
  176,1,128,27,184,1,128,4,15,1,36,54,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,4,15,1,36,54,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1, 
  21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,36,54,0,0,1,15,1,36,54,0,0,17,1,99,54,0,0, 
  1,1,2,21,1,63,0,0,0,114,148,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1, 
  225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,21,1,153,2,0,0,152,147,0,0,22,0,0,0,4,0,0,0,17,192,8,128,1,80,195,127,2,80,3,128,3,176,19,128,4,152,18,131,5,248,17, 
  128,6,136,209,130,7,232,16,128,8,208,15,131,9,184,78,130,10,160,13,128,11,48,12,128,12,24,11,128,13,0,138,129,30,56,4,128,15,96,9,128,20,240,7,128,22,32,7,128,25,40,198,128,29,48,5,128,40,152,3, 
  128,41,88,3,128,8,4,17,1,85,57,0,0,1,4,15,1,73,57,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,73,57,0,0,15,1,246,46,0,0,17,1, 
  144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,73,57,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,73,57,0,0,15,1,234,46,0,0, 
  17,1,170,46,0,0,1,4,15,1,73,57,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,73,57,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4, 
  15,1,73,57,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,73,57,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0, 
  1,0,19,39,0,0,0,105,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,105,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1, 
  0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,105,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39, 
  0,0,0,105,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,105,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0, 
  0,0,53,0,0,0,1,0,19,39,0,0,0,105,0,0,0,2,0,1,4,15,1,73,57,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,35,0,0,0,98,0,0,0,3,0,14,1,4,15,1,73,57,0, 
  0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,105,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1, 
  0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,105,0,0,0,2,0,1,2,19,39,0,0,0,105,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4, 
  19,35,0,0,0,96,0,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,39,0,0,0,104,0,0,0,1,0,1,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,35,0,0,0,97,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,30,0,0, 
  0,80,0,0,0,1,0,1,21,0,162,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,16,240,3,128,17,96,3,128,10,128,132,128,27,144,2,129,26,248,2,128,29,136,2,128,30,32,2,128,35,144,1,128,19,30, 
  0,0,0,80,0,0,0,1,0,17,1,236,57,0,0,1,15,1,236,57,0,0,17,1,22,66,0,0,1,1,15,1,236,57,0,0,17,1,143,58,0,0,1,15,1,236,57,0,0,17,1,38,19,0,0,1,19,27,0,0, 
  0,73,0,0,0,1,0,17,1,236,57,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,236,57,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,236,57,0,0,1,2,21,7,78,0,0,0,255,255,255, 
  255,5,0,0,0,2,0,0,0,47,48,1,129,45,112,1,128,42,240,1,128,43,176,65,127,123,48,2,128,4,17,1,108,24,0,0,1,4,17,1,133,21,0,0,1,4,17,1,89,23,0,0,1,4,17,1,19,18,0,0, 
  1,4,17,1,249,58,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,248,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,128,9,128,1, 
  48,195,127,2,48,3,128,3,120,22,128,4,48,21,131,5,96,20,128,6,248,211,130,7,40,19,128,8,224,17,131,9,152,80,130,10,80,15,128,11,176,13,128,12,104,12,128,13,32,139,129,30,8,4,128,15,80,10,128,20, 
  128,8,128,22,128,7,128,25,88,6,128,29,48,5,128,40,56,3,128,8,4,15,1,242,61,0,0,15,1,10,66,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1, 
  242,61,0,0,15,1,10,66,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,242,61,0,0,15,1,10,66,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1, 
  4,19,22,0,0,0,58,0,0,0,1,0,15,1,242,61,0,0,15,1,10,66,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,242,61,0,0,15,1,10,66,0,0,15,1,158,46,0,0,15,1,190,125, 
  0,0,17,1,96,118,0,0,1,4,15,1,242,61,0,0,15,1,10,66,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,242,61,0,0,15,1,10,66,0,0,15,1,146,46,0,0, 
  17,1,239,38,0,0,1,4,15,1,242,61,0,0,15,1,10,66,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0, 
  0,101,0,0,0,1,0,17,1,242,61,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,101,0,0,0,1,0,17,1,242,61,0,0,1,4,19,16,0,0, 
  0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,101,0,0,0,1,0,17,1,242,61,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19, 
  21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,101,0,0,0,1,0,17,1,242,61,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,101,0,0,0, 
  1,0,17,1,242,61,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,101,0,0,0,1,0,17,1,242,61,0,0,1,4,15,1,242,61,0,0,15,1,10, 
  66,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,35,0,0,0,95,0,0,0,3,0,1,4,15,1,242,61,0,0,15,1,10,66,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0, 
  0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,101,0,0,0,1,0,17,1,242,61,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19, 
  37,0,0,0,101,0,0,0,1,0,17,1,242,61,0,0,1,2,21,0,38,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,16,8,129,9,128,199,129,10,240,6,130,3,160,136,129,16,96,6,128,21,176,132,129, 
  22,72,4,128,31,192,2,128,17,208,5,128,19,64,197,128,26,224,3,128,29,80,131,128,35,184,2,128,37,80,2,128,15,1,242,61,0,0,17,1,112,63,0,0,1,1,19,21,0,0,0,57,0,0,0,1,0,17,1,242, 
  61,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,242,61,0,0,1,15,1,242,61,0,0,17,1,25,63,0,0,1,15,1,242,61,0,0,17,1,170,46,0,0,1,19,37,0,0,0,101,0,0,0,1,0,17, 
  1,242,61,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,242,61,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,242,61,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,242,61,0,0,1, 
  19,21,0,0,0,56,0,0,0,1,0,17,1,242,61,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,242,61,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,242,61,0,0,1,19,21,0,0,0,52, 
  0,0,0,1,0,17,1,242,61,0,0,1,2,21,1,75,0,0,0,190,140,0,0,4,0,0,0,2,0,0,0,28,184,1,128,1,176,1,128,2,176,1,128,27,16,1,128,4,15,1,3,28,0,0,15,1,142,28,0,0, 
  17,1,141,13,0,0,1,8,4,15,1,3,28,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,21,1,141,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,96, 
  8,128,1,48,195,127,2,48,3,128,3,80,19,128,4,56,18,131,5,152,17,128,6,40,209,130,7,136,16,128,8,112,15,131,9,88,78,130,10,64,13,128,11,208,11,128,12,184,10,128,13,160,137,129,30,216,3,128,15,0, 
  9,128,20,144,7,128,22,192,6,128,25,200,5,128,29,208,4,128,40,56,3,128,8,4,15,1,254,65,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,254,65,0, 
  0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,254,65,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,254, 
  65,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,254,65,0,0,15,1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,254,65,0,0,15,1,158,46,0,0,15,1,84,118,0, 
  0,17,1,185,117,0,0,1,4,15,1,254,65,0,0,15,1,146,46,0,0,17,1,239,38,0,0,1,4,15,1,254,65,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0, 
  19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,19, 
  16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0, 
  0,53,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,19,8,0,0,0, 
  23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,15,1,254,65,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,35,0,0,0,94,0,0,0,4, 
  0,14,1,4,15,1,254,65,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,4,19, 
  3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,102,0,0,0,2,0,1,2,19,37,0,0,0,102,0,0,0,2,0,1,19,37,0,0,0,101,0,0,0,1,0,1,21,1, 
  216,0,0,0,16,147,0,0,8,0,0,0,3,0,0,0,29,192,2,128,1,144,1,128,2,144,1,128,11,184,4,128,20,232,3,128,5,80,198,126,6,216,69,128,30,152,1,128,8,4,19,26,0,0,0,66,0,0,0,1, 
  0,15,1,239,66,0,0,15,1,251,66,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,239,66,0,0,15,1,251,66,0,0,15,1,247,27,0,0,17,1,174,126, 
  0,0,1,4,15,1,239,66,0,0,15,1,251,66,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,15,1,239,66,0,0,17,1, 
  251,66,0,0,1,4,19,29,0,0,0,78,0,0,0,5,0,14,14,1,4,15,1,239,66,0,0,17,1,161,49,0,0,1,2,19,30,0,0,0,81,0,0,0,2,0,1,21,0,123,0,0,0,255,255,255,255,6,0,0, 
  0,2,0,0,0,16,184,2,128,17,40,2,128,10,72,131,128,27,88,129,128,26,192,1,128,35,80,1,128,1,15,1,251,66,0,0,17,1,143,58,0,0,1,15,1,251,66,0,0,17,1,119,67,0,0,1,19,27,0,0, 
  0,73,0,0,0,1,0,17,1,251,66,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,251,66,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,251,66,0,0,1,2,21,1,75,0,0,0,60,143,0, 
  0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,125,19,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,4,15,1,125,19,0,0,15,1,142,28,0,0,17,1, 
  225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,38,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,160,7,129,9,16,199,129,10,128,70,130,3,160,136,129,16,240,5,128,5,152,72,129, 
  22,216,3,128,7,48,136,129,17,96,5,128,19,208,4,128,21,64,132,128,26,112,3,128,29,224,2,128,31,80,2,128,19,21,0,0,0,57,0,0,0,1,0,17,1,206,67,0,0,1,19,21,0,0,0,55,0,0,0,1, 
  0,17,1,206,67,0,0,1,15,1,206,67,0,0,17,1,143,71,0,0,1,15,1,206,67,0,0,17,1,170,46,0,0,1,19,7,0,0,0,21,0,0,0,1,0,17,1,206,67,0,0,1,19,3,0,0,0,13,0,0, 
  0,1,0,17,1,206,67,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,206,67,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,206,67,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,206, 
  67,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,206,67,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,206,67,0,0,1,15,1,206,67,0,0,17,1,245,68,0,0,1,1,19,21,0,0,0,52, 
  0,0,0,1,0,17,1,206,67,0,0,1,2,21,1,141,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,96,8,128,1,48,195,127,2,48,3,128,3,80,19,128,4,56,18,131,5,152,17,128,6,40,209,130,7, 
  136,16,128,8,112,15,131,9,88,78,130,10,64,13,128,11,208,11,128,12,184,10,128,13,160,137,129,30,216,3,128,15,0,9,128,20,144,7,128,22,192,6,128,25,200,5,128,29,208,4,128,40,56,3,128,8,4,15,1,131, 
  71,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,131,71,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15, 
  1,131,71,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,131,71,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,131,71,0,0,15,1,158,46, 
  0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,131,71,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,131,71,0,0,15,1,146,46,0,0,17,1,239,38,0,0, 
  1,4,15,1,131,71,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0, 
  0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52, 
  0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0,0,0,24,0, 
  0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2, 
  0,1,4,15,1,131,71,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,5,0,0,0,16,0,0,0,4,0,14,1,4,15,1,131,71,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19,3,0, 
  0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,22, 
  0,0,0,2,0,1,2,19,7,0,0,0,22,0,0,0,2,0,1,21,1,75,0,0,0,190,140,0,0,4,0,0,0,2,0,0,0,28,24,1,128,1,16,1,128,2,16,1,128,27,184,1,128,8,4,15,1,3,28,0, 
  0,15,1,142,28,0,0,17,1,225,13,0,0,1,4,15,1,3,28,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,19,7,0,0,0,21,0,0,0,1,0,1,19,21, 
  0,0,0,55,0,0,0,1,0,1,21,1,172,0,0,0,88,149,0,0,7,0,0,0,2,0,0,0,16,8,4,129,1,112,1,129,2,112,1,129,11,112,4,128,20,104,3,128,29,112,2,128,30,120,1,128,8,4,19,26, 
  0,0,0,66,0,0,0,1,0,15,1,171,72,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,171,72,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4, 
  15,1,171,72,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,23,0,0,0,60,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,171,72,0, 
  0,1,2,21,0,140,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,16,64,3,129,17,176,2,128,10,208,195,128,23,168,194,128,24,64,2,128,26,216,1,128,27,112,1,128,15,1,171,72,0,0,17,1,226,80,0, 
  0,1,15,1,171,72,0,0,17,1,139,80,0,0,1,15,1,171,72,0,0,17,1,56,73,0,0,1,1,19,27,0,0,0,73,0,0,0,1,0,17,1,171,72,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1, 
  171,72,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,171,72,0,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,208,0,128,41,16,1,128,4,17,1,132,73,0,0,1,4,19, 
  23,0,0,0,59,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72, 
  3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,32,74,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0, 
  65,0,0,0,1,0,15,1,32,74,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,32,74,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0, 
  0,0,71,0,0,0,1,0,17,1,32,74,0,0,1,2,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,16,184,2,129,17,40,2,128,10,72,195,128,27,80,1,128,24,32,2,128,26,184,1,128,15,1, 
  32,74,0,0,17,1,126,75,0,0,1,15,1,32,74,0,0,17,1,156,74,0,0,1,1,19,27,0,0,0,73,0,0,0,1,0,17,1,32,74,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,32,74,0,0, 
  1,19,27,0,0,0,72,0,0,0,1,0,17,1,32,74,0,0,1,2,21,1,75,0,0,0,224,149,0,0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,243,74,0, 
  0,15,1,142,28,0,0,17,1,141,13,0,0,1,4,15,1,243,74,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,243,74,0,0,1,15,1,243,74,0,0,17,1,50,75,0,0,1,1,2,21,1,63,0,0,0,224,149,0,0, 
  4,0,0,0,2,0,0,0,28,16,1,128,1,128,1,128,2,128,1,128,27,136,1,128,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,4,15,1,231,15,0,0,17,1,141,13,0,0,1,19,10,0,0,0,30,0, 
  0,0,2,0,14,1,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,80,1,128,45,208,1,128,42,144,1,128,43,16,65,127,4,17,1,120,79,0,0,1,4,17,1,101,78,0,0,1,4,17,1,82, 
  77,0,0,1,4,17,1,232,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,62,0,0,0,3,0,14,1,21,1,155,0,0,0,33,142, 
  0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,132,76,0,0,15,1,247,27,0,0,17, 
  1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,132,76,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,132,76,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16, 
  0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,132,76,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48, 
  1,128,26,152,1,128,15,1,132,76,0,0,17,1,239,22,0,0,1,15,1,132,76,0,0,17,1,251,76,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,132,76,0,0,1,19,27,0,0,0,71,0,0,0,1, 
  0,17,1,132,76,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,132,76,0,0,1,2,21,1,75,0,0,0,224,149,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,176,1,128,2,176,1,128,27,184,1, 
  128,4,15,1,243,74,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,4,15,1,243,74,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,21,1,155,0,0,0, 
  33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,238,77,0,0,15,1,247,27,0, 
  0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,238,77,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,238,77,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4, 
  19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,238,77,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128, 
  27,48,1,128,26,152,1,128,15,1,238,77,0,0,17,1,205,17,0,0,1,15,1,238,77,0,0,17,1,156,74,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,238,77,0,0,1,19,27,0,0,0,71,0,0, 
  0,1,0,17,1,238,77,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,238,77,0,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11, 
  232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,1,79,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,1,79,0, 
  0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,1,79,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,1, 
  79,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,1,79,0,0,17,1,127,25,0,0,1,15,1,1,79, 
  0,0,17,1,139,80,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,1,79,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,1,79,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,1,79, 
  0,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15, 
  1,20,80,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,20,80,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,20,80,0,0,15,1,125,14, 
  0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,20,80,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144, 
  2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,20,80,0,0,17,1,27,21,0,0,1,15,1,20,80,0,0,17,1,156,74,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,20,80,0, 
  0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,20,80,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,20,80,0,0,1,2,21,1,75,0,0,0,224,149,0,0,4,0,0,0,2,0,0,0,28,184,1, 
  128,1,176,1,128,2,176,1,128,27,16,1,128,4,15,1,243,74,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,4,15,1,243,74,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,19,10,0,0,0,31, 
  0,0,0,1,0,1,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,208,1,128,45,144,1,128,42,80,1,128,43,16,65,127,4,17,1,120,79,0,0,1,4,17,1,82,77,0,0,1,4,17,1,232, 
  75,0,0,1,4,17,1,101,78,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,61,0,0,0,1,0,1,19,9,0,0,0,29,0,0,0, 
  2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,145,81,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  2,21,1,139,0,0,0,107,150,0,0,6,0,0,0,2,0,0,0,36,152,3,128,1,80,193,128,2,80,193,128,39,88,1,128,37,216,2,128,38,24,2,128,8,4,19,28,0,0,0,76,0,0,0,1,0,19,20,0,0, 
  0,51,0,0,0,3,0,1,4,19,28,0,0,0,77,0,0,0,1,0,19,20,0,0,0,51,0,0,0,3,0,1,4,19,28,0,0,0,75,0,0,0,1,0,19,20,0,0,0,51,0,0,0,3,0,1,4,19,28,0, 
  0,0,74,0,0,0,1,0,19,20,0,0,0,51,0,0,0,3,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,184,1,128,5,80,193,128,6,72,194,128,23,88,1,128,9,80,1,128,26, 
  176,2,128,1,19,9,0,0,0,29,0,0,0,2,0,1,19,6,0,0,0,19,0,0,0,1,0,17,1,29,82,0,0,1,15,1,29,82,0,0,17,1,129,82,0,0,1,15,1,29,82,0,0,17,1,87,81,0,0,1, 
  2,21,1,97,0,0,0,204,151,0,0,5,0,0,0,2,0,0,0,5,200,194,128,1,48,193,127,2,48,129,128,29,0,2,128,30,56,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,234,89,0,0,17,1, 
  87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,234,89,0,0,17,1,87,81,0,0,1,4,17,1,227,82,0,0,1,2,21,1,249,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,128,9, 
  128,1,48,195,127,2,48,3,128,3,128,22,128,4,56,21,131,5,104,20,128,6,248,211,130,7,40,19,128,8,224,17,131,9,152,80,130,10,80,15,128,11,176,13,128,12,104,12,128,13,32,139,129,30,8,4,128,15,80,10, 
  128,20,128,8,128,22,128,7,128,25,88,6,128,29,48,5,128,40,56,3,128,8,4,15,1,221,85,0,0,15,1,230,71,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0, 
  15,1,221,85,0,0,15,1,230,71,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,221,85,0,0,15,1,230,71,0,0,15,1,246,46,0,0,17,1,174,126,0, 
  0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,221,85,0,0,15,1,230,71,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,221,85,0,0,15,1,230,71,0,0,15,1,158,46,0,0,15,1, 
  190,125,0,0,17,1,96,118,0,0,1,4,15,1,221,85,0,0,15,1,230,71,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,221,85,0,0,15,1,230,71,0,0,15,1,146,46, 
  0,0,17,1,239,38,0,0,1,4,15,1,221,85,0,0,15,1,230,71,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7, 
  0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,4,19,16, 
  0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,4,19,8,0,0,0,25,0,0,0,1, 
  0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0, 
  0,0,1,0,17,1,221,85,0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,4,15,1,221,85,0,0,15, 
  1,230,71,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,5,0,0,0,17,0,0,0,4,0,14,1,4,15,1,221,85,0,0,15,1,230,71,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1,4,19, 
  3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0, 
  1,0,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,2,21,0,38,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,160,7,129,9,16,199,129,10,128,70,130,3,160,136,129,16,240,5,128,5, 
  152,72,129,22,216,3,128,7,48,136,129,17,96,5,128,19,208,4,128,21,64,132,128,26,112,3,128,29,224,2,128,31,80,2,128,19,21,0,0,0,57,0,0,0,1,0,17,1,221,85,0,0,1,19,21,0,0,0,55,0, 
  0,0,1,0,17,1,221,85,0,0,1,15,1,221,85,0,0,17,1,147,89,0,0,1,15,1,221,85,0,0,17,1,170,46,0,0,1,19,7,0,0,0,21,0,0,0,1,0,17,1,221,85,0,0,1,19,3,0,0,0, 
  13,0,0,0,1,0,17,1,221,85,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,221,85,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,221,85,0,0,1,19,21,0,0,0,56,0,0,0,1,0, 
  17,1,221,85,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,221,85,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,221,85,0,0,1,15,1,221,85,0,0,17,1,4,87,0,0,1,1,19,21,0, 
  0,0,52,0,0,0,1,0,17,1,221,85,0,0,1,2,21,1,142,2,0,0,77,138,0,0,21,0,0,0,4,0,0,0,17,96,8,128,1,48,195,127,2,48,3,128,3,88,19,128,4,64,18,131,5,160,17,128,6,40, 
  209,130,7,136,16,128,8,112,15,131,9,88,78,130,10,64,13,128,11,208,11,128,12,184,10,128,13,160,137,129,30,216,3,128,15,0,9,128,20,144,7,128,22,192,6,128,25,200,5,128,29,208,4,128,40,56,3,128,8,4, 
  15,1,131,71,0,0,15,1,242,71,0,0,17,1,2,47,0,0,1,4,19,26,0,0,0,66,0,0,0,1,0,15,1,131,71,0,0,15,1,246,46,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0, 
  1,0,15,1,131,71,0,0,15,1,246,46,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,131,71,0,0,15,1,234,46,0,0,17,1,170,46,0,0,1,4,15,1,131,71,0,0,15, 
  1,158,46,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,131,71,0,0,15,1,158,46,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,131,71,0,0,15,1,146,46,0,0,17,1,239, 
  38,0,0,1,4,15,1,131,71,0,0,15,1,146,46,0,0,17,1,76,31,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4, 
  19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,21,0, 
  0,0,52,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0,0, 
  0,24,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,22,0, 
  0,0,2,0,1,4,15,1,131,71,0,0,15,1,64,31,0,0,17,1,34,99,0,0,1,4,19,5,0,0,0,15,0,0,0,5,0,14,14,1,4,15,1,131,71,0,0,15,1,146,46,0,0,17,1,27,9,0,0,1, 
  4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,22,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7, 
  0,0,0,22,0,0,0,2,0,1,2,21,1,75,0,0,0,190,140,0,0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,3,28,0,0,15,1,142,28,0,0,17,1, 
  141,13,0,0,1,4,15,1,3,28,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,19,6,0,0,0,20,0,0,0,2,0,1,19,6,0,0,0,19,0,0,0,1, 
  0,1,21,0,33,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,8,200,70,130,9,56,70,130,2,120,136,129,3,232,7,130,12,24,5,128,5,88,7,128,22,152,2,128,15,176,4,128,10,168,5,129,16,32,4,128, 
  17,144,3,128,19,0,3,128,26,48,2,128,15,1,2,90,0,0,17,1,52,98,0,0,1,15,1,2,90,0,0,17,1,80,97,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,2,90,0,0,1,19,3,0,0, 
  0,12,0,0,0,1,0,17,1,2,90,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,2,90,0,0,1,15,1,2,90,0,0,17,1,36,91,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,2,90, 
  0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,2,90,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,2,90,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,2,90,0,0,1,19,2,0, 
  0,0,4,0,0,0,1,0,17,1,2,90,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,2,90,0,0,1,19,15,0,0,0,44,0,0,0,1,0,17,1,2,90,0,0,1,2,21,1,117,2,0,0,206,136, 
  0,0,20,0,0,0,4,0,0,0,17,160,7,128,1,16,195,127,2,16,3,128,3,144,18,128,4,120,17,131,5,216,16,128,6,104,208,130,7,200,15,128,8,176,14,128,9,152,77,130,10,128,12,128,11,16,11,128,12,248, 
  9,128,13,224,136,129,30,24,3,128,15,64,8,128,20,208,6,128,22,0,6,128,25,8,5,128,29,16,4,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,68,97,0,0,15,1,91,133,0,0,17,1,144,127,0, 
  0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,68,97,0,0,15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,68,97,0,0,15,1,126,94,0,0,17,1,154, 
  93,0,0,1,4,15,1,68,97,0,0,15,1,178,125,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,68,97,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,68, 
  97,0,0,15,1,173,117,0,0,17,1,132,108,0,0,1,4,15,1,68,97,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19, 
  15,0,0,0,45,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,45,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3, 
  0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,15,0,0,0,45,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0, 
  45,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,15,0,0,0,45,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5, 
  0,0,0,1,0,19,15,0,0,0,45,0,0,0,2,0,1,4,15,1,68,97,0,0,15,1,79,99,0,0,17,1,34,99,0,0,1,4,19,12,0,0,0,38,0,0,0,3,0,14,1,4,15,1,68,97,0,0,15,1, 
  173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,15,0,0,0,45,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2, 
  0,0,0,3,0,0,0,1,0,19,15,0,0,0,45,0,0,0,2,0,1,2,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128, 
  30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,26,94,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,26,94,0,0,15,1,246,89,0,0, 
  17,1,87,81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,136,2,128,5,32,194,128,6,184,193,128, 
  23,40,2,128,9,32,2,128,26,80,1,128,15,1,26,94,0,0,17,1,87,81,0,0,1,15,1,26,94,0,0,17,1,129,82,0,0,1,1,19,9,0,0,0,29,0,0,0,2,0,1,19,6,0,0,0,19,0,0,0, 
  1,0,17,1,26,94,0,0,1,2,21,0,255,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,8,64,6,130,9,176,5,130,2,240,71,129,3,96,199,129,12,144,4,128,5,208,6,128,22,120,2,128,10,32,5,129, 
  16,0,4,128,17,112,3,128,19,224,2,128,26,16,2,128,15,1,126,94,0,0,17,1,98,96,0,0,1,15,1,126,94,0,0,17,1,126,95,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,126,94,0,0,1, 
  19,3,0,0,0,12,0,0,0,1,0,17,1,126,94,0,0,1,19,3,0,0,0,11,0,0,0,1,0,17,1,126,94,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,126,94,0,0,1,19,2,0,0,0,7, 
  0,0,0,1,0,17,1,126,94,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,126,94,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,126,94,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17, 
  1,126,94,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,126,94,0,0,1,1,2,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128, 
  29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,254,95,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,254,95,0,0,15,1, 
  246,89,0,0,17,1,87,81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,192,1,128,5,184,193,128, 
  6,80,194,128,23,184,2,128,9,184,1,128,26,80,1,128,15,1,254,95,0,0,17,1,87,81,0,0,1,1,19,6,0,0,0,19,0,0,0,1,0,17,1,254,95,0,0,1,15,1,254,95,0,0,17,1,129,82,0,0, 
  1,19,9,0,0,0,29,0,0,0,2,0,1,2,21,1,75,0,0,0,60,152,0,0,4,0,0,0,2,0,0,0,28,184,1,128,1,176,1,128,2,176,1,128,27,16,1,128,4,15,1,185,96,0,0,15,1,142,28,0, 
  0,17,1,141,13,0,0,1,8,4,15,1,185,96,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10, 
  232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,185,96,0,0,1,15,1,185,96,0,0,17,1,248,96,0,0,1,1,2,21,1,63,0,0,0,60,152,0,0,4,0,0,0,2,0, 
  0,0,28,16,1,128,1,128,1,128,2,128,1,128,27,136,1,128,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,4,15,1,231,15,0,0,17,1,141,13,0,0,1,19,10,0,0,0,30,0,0,0,2,0,14,1, 
  19,15,0,0,0,45,0,0,0,2,0,1,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0, 
  0,66,0,0,0,1,0,15,1,208,97,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,208,97,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,15,1, 
  75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,32,2,128,5,80,193,128,6,184,193,128,23,88,1,128,9,80,1,128,26,176, 
  2,128,1,19,9,0,0,0,29,0,0,0,2,0,1,15,1,208,97,0,0,17,1,129,82,0,0,1,19,6,0,0,0,19,0,0,0,1,0,17,1,208,97,0,0,1,15,1,208,97,0,0,17,1,87,81,0,0,1,2, 
  21,1,75,0,0,0,33,153,0,0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,139,98,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,4,15,1,139,98,0, 
  0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0, 
  0,0,32,0,0,0,1,0,17,1,139,98,0,0,1,15,1,139,98,0,0,17,1,202,98,0,0,1,1,2,21,1,63,0,0,0,33,153,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2,240,1,128, 
  27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,19,15,0,0,0,44,0,0,0,1,0,1,21,1, 
  44,0,0,0,240,153,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,11,248,0,128,8,4,19,8,0,0,0,26,0,0,0,2,0,1,2,19,2,0,0,0,5,0,0,0,1,0,1,21,1,206,2,0, 
  0,37,154,0,0,20,0,0,0,4,0,0,0,16,0,9,128,1,16,195,131,2,16,3,128,3,40,21,128,4,224,83,131,5,16,19,128,22,48,6,128,7,64,18,128,8,248,16,128,9,176,79,130,10,104,14,128,11,200,12, 
  128,12,128,11,128,13,56,138,129,30,24,3,128,15,104,9,128,17,48,8,128,20,48,7,128,25,104,5,128,29,64,4,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,14,103,0,0,15,1,120,108,0,0,15,1, 
  91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,14,103,0,0,15,1,120,108,0,0,15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0, 
  15,1,14,103,0,0,17,1,42,102,0,0,1,4,15,1,14,103,0,0,15,1,120,108,0,0,15,1,178,125,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,14,103,0,0,15,1,120,108,0,0,15,1, 
  178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,14,103,0,0,15,1,120,108,0,0,15,1,173,117,0,0,17,1,132,108,0,0,1,4,19,12,0,0,0,37,0,0,0,2,0,1,4,15,1,14, 
  103,0,0,15,1,120,108,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103, 
  0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0, 
  0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19, 
  14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,4,19, 
  8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,4,15,1,14,103,0,0,15,1,120,108,0,0,15,1,79,99,0,0,17,1, 
  34,99,0,0,1,4,15,1,14,103,0,0,15,1,120,108,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,14,0,0,0,42, 
  0,0,0,1,0,17,1,14,103,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,2,21,1,127,0,0,0, 
  145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,170,102,0,0,15,1,246,89,0, 
  0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,170,102,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0, 
  1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,192,1,128,5,80,193,128,6,88,193,128,23,80,2,128,9,80,1,128,26,176,2,128,1,15,1,170,102,0,0,17,1,129,82,0,0,1,19,6, 
  0,0,0,19,0,0,0,1,0,17,1,170,102,0,0,1,19,9,0,0,0,29,0,0,0,2,0,1,15,1,170,102,0,0,17,1,87,81,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0, 
  8,200,6,130,9,56,6,130,2,120,72,129,3,232,199,129,12,24,5,128,5,88,7,128,14,176,68,129,10,168,69,129,16,32,4,128,17,144,3,128,19,0,3,128,22,152,2,128,26,48,2,128,15,1,14,103,0,0,17,1, 
  150,107,0,0,1,15,1,14,103,0,0,17,1,178,106,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,14,103,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,14,103,0,0,1,19,3,0,0,0,11, 
  0,0,0,1,0,17,1,14,103,0,0,1,15,1,14,103,0,0,17,1,48,104,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,14,103,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,14,103,0,0, 
  1,19,2,0,0,0,6,0,0,0,1,0,17,1,14,103,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,14,103,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,14,103,0,0,1,19,2,0,0,0, 
  3,0,0,0,1,0,17,1,14,103,0,0,1,19,14,0,0,0,42,0,0,0,1,0,17,1,14,103,0,0,1,2,21,1,117,2,0,0,37,154,0,0,20,0,0,0,4,0,0,0,16,64,8,128,1,16,195,131,2,16, 
  3,128,3,144,18,128,4,120,81,131,5,216,16,128,22,0,6,128,7,56,16,128,8,32,15,128,9,8,78,130,10,240,12,128,11,128,11,128,12,104,10,128,13,80,137,129,30,24,3,128,15,176,8,128,17,160,7,128,20,208, 
  6,128,25,8,5,128,29,16,4,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,166,106,0,0,15,1,91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,166,106,0,0, 
  15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,166,106,0,0,15,1,126,94,0,0,17,1,154,93,0,0,1,4,15,1,166,106,0,0,15,1,178,125,0,0,15,1,190, 
  125,0,0,17,1,96,118,0,0,1,4,15,1,166,106,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,166,106,0,0,15,1,173,117,0,0,17,1,132,108,0,0,1,4,19,12,0, 
  0,0,36,0,0,0,3,0,14,1,4,15,1,166,106,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,43,0, 
  0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,43,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0, 
  0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,14,0,0,0,43,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,43,0,0,0,2,0, 
  1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,14,0,0,0,43,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19, 
  14,0,0,0,43,0,0,0,2,0,1,4,15,1,166,106,0,0,15,1,79,99,0,0,17,1,34,99,0,0,1,4,15,1,166,106,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0, 
  0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,14,0,0,0,43,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,14,0,0,0,43,0,0,0,2,0, 
  1,2,19,14,0,0,0,43,0,0,0,2,0,1,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26, 
  0,0,0,66,0,0,0,1,0,15,1,50,107,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,50,107,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4, 
  15,1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,80,1,128,5,224,193,128,6,232,193,128,23,184,2,128,9,224,1,128, 
  26,80,2,128,19,6,0,0,0,19,0,0,0,1,0,17,1,50,107,0,0,1,1,15,1,50,107,0,0,17,1,129,82,0,0,1,15,1,50,107,0,0,17,1,87,81,0,0,1,19,9,0,0,0,29,0,0,0,2,0, 
  1,2,21,1,75,0,0,0,232,154,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,176,1,128,2,176,1,128,27,184,1,128,4,15,1,237,107,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,4,15,1, 
  237,107,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19, 
  11,0,0,0,32,0,0,0,1,0,17,1,237,107,0,0,1,15,1,237,107,0,0,17,1,44,108,0,0,1,1,2,21,1,63,0,0,0,232,154,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2,240, 
  1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,19,14,0,0,0,42,0,0,0,1,0,1, 
  21,1,206,2,0,0,183,155,0,0,20,0,0,0,4,0,0,0,17,152,8,128,1,16,195,127,2,16,131,131,3,40,21,128,4,224,83,131,5,16,19,128,22,48,6,128,7,64,18,128,8,248,16,128,9,176,79,130,10,104, 
  14,128,11,200,12,128,12,128,11,128,13,56,138,129,30,24,3,128,15,104,9,128,18,48,8,128,20,48,7,128,25,104,5,128,29,64,4,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,55,112,0,0,15,1,161, 
  117,0,0,15,1,91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,55,112,0,0,15,1,161,117,0,0,15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58, 
  0,0,0,1,0,15,1,55,112,0,0,17,1,83,111,0,0,1,4,15,1,55,112,0,0,15,1,161,117,0,0,15,1,178,125,0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,55,112,0,0,15,1,161, 
  117,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,19,12,0,0,0,35,0,0,0,2,0,1,4,15,1,55,112,0,0,15,1,161,117,0,0,15,1,173,117,0,0,17,1,132,108,0,0, 
  1,4,15,1,55,112,0,0,15,1,161,117,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,40,0,0,0,1, 
  0,17,1,55,112,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,4,19,16,0,0,0,46,0,0,0,1, 
  0,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0, 
  0,0,1,0,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112, 
  0,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,4,15,1,55,112,0,0,15,1,161,117,0,0,15,1,79, 
  99,0,0,17,1,34,99,0,0,1,4,15,1,55,112,0,0,15,1,161,117,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19, 
  13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,2,21, 
  1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,211,111,0,0, 
  15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,211,111,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0,1,4,17, 
  1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,192,1,128,5,80,193,128,6,176,194,128,23,80,2,128,9,80,1,128,26,88,1,128,1,15,1,211,111,0,0,17,1,87,81, 
  0,0,1,19,6,0,0,0,19,0,0,0,1,0,17,1,211,111,0,0,1,19,9,0,0,0,29,0,0,0,2,0,1,15,1,211,111,0,0,17,1,129,82,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0,0, 
  0,3,0,0,0,8,200,70,130,9,56,70,130,2,120,72,129,3,232,7,130,12,24,5,128,5,88,199,128,22,152,2,128,10,168,69,129,13,176,4,128,16,32,4,128,17,144,3,128,19,0,3,128,26,48,2,128,15,1,55, 
  112,0,0,17,1,191,116,0,0,1,15,1,55,112,0,0,17,1,219,115,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,55,112,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,55,112,0,0,1,19, 
  3,0,0,0,11,0,0,0,1,0,17,1,55,112,0,0,1,15,1,55,112,0,0,17,1,89,113,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,55,112,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17, 
  1,55,112,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,55,112,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,55,112,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,55,112,0,0,1, 
  19,2,0,0,0,3,0,0,0,1,0,17,1,55,112,0,0,1,19,13,0,0,0,40,0,0,0,1,0,17,1,55,112,0,0,1,2,21,1,117,2,0,0,183,155,0,0,20,0,0,0,4,0,0,0,17,16,8,128,1, 
  16,195,127,2,16,131,131,3,144,18,128,4,120,81,131,5,216,16,128,22,0,6,128,7,56,16,128,8,32,15,128,9,8,78,130,10,240,12,128,11,128,11,128,12,104,10,128,13,80,137,129,30,24,3,128,15,176,8,128,18, 
  160,7,128,20,208,6,128,25,8,5,128,29,16,4,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,207,115,0,0,15,1,91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15, 
  1,207,115,0,0,15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,207,115,0,0,15,1,126,94,0,0,17,1,154,93,0,0,1,4,15,1,207,115,0,0,15,1,178,125, 
  0,0,15,1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,207,115,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,19,12,0,0,0,34,0,0,0,3,0,14,1,4,15,1,207,115, 
  0,0,15,1,173,117,0,0,17,1,132,108,0,0,1,4,15,1,207,115,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13, 
  0,0,0,41,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,41,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0, 
  0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,13,0,0,0,41,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,41, 
  0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,13,0,0,0,41,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5,0, 
  0,0,1,0,19,13,0,0,0,41,0,0,0,2,0,1,4,15,1,207,115,0,0,15,1,79,99,0,0,17,1,34,99,0,0,1,4,15,1,207,115,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0, 
  0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,13,0,0,0,41,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,13,0,0,0,41, 
  0,0,0,2,0,1,2,19,13,0,0,0,41,0,0,0,2,0,1,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1, 
  128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,91,116,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,91,116,0,0,15,1,246,89,0,0,17,1,87, 
  81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,32,2,128,5,176,193,128,6,176,194,128,23,80,1, 
  128,9,176,1,128,26,184,1,128,19,9,0,0,0,29,0,0,0,2,0,1,1,15,1,91,116,0,0,17,1,87,81,0,0,1,19,6,0,0,0,19,0,0,0,1,0,17,1,91,116,0,0,1,15,1,91,116,0,0,17, 
  1,129,82,0,0,1,2,21,1,75,0,0,0,122,156,0,0,4,0,0,0,2,0,0,0,28,184,1,128,1,176,1,128,2,176,1,128,27,16,1,128,4,15,1,22,117,0,0,15,1,142,28,0,0,17,1,141,13,0,0, 
  1,8,4,15,1,22,117,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128, 
  25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,22,117,0,0,1,15,1,22,117,0,0,17,1,85,117,0,0,1,1,2,21,1,63,0,0,0,122,156,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1, 
  240,1,128,2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,19,13,0,0,0,40,0, 
  0,0,1,0,1,19,2,0,0,0,8,0,0,0,1,0,1,21,1,50,0,0,0,73,157,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,19,248,0,128,8,4,19,18,0,0,0,48,0,0,0,1,0, 
  17,1,236,117,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,18,208,0,128,17,56,1,128,15,1,236,117,0,0,17,1,21,118,0,0,1,1,2,21,1,62,0,0,0,120,157,0,0,4, 
  0,0,0,2,0,0,0,21,24,1,128,1,16,193,127,2,16,1,128,19,136,1,128,8,4,19,17,0,0,0,47,0,0,0,3,0,14,1,4,19,18,0,0,0,49,0,0,0,2,0,1,2,19,3,0,0,0,12,0,0, 
  0,1,0,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15, 
  1,252,118,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,252,118,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,252,118,0,0,15,1,125,14, 
  0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,252,118,0,0,1,2,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,16,184, 
  2,128,17,40,2,128,10,72,131,128,19,32,130,128,26,184,1,128,27,80,1,128,15,1,252,118,0,0,17,1,90,120,0,0,1,15,1,252,118,0,0,17,1,120,119,0,0,1,1,19,27,0,0,0,73,0,0,0,1,0, 
  17,1,252,118,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,252,118,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,252,118,0,0,1,2,21,1,75,0,0,0,236,157,0,0,4,0,0,0,2,0, 
  0,0,28,16,1,128,1,80,2,128,2,80,2,128,27,176,1,128,4,15,1,207,119,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,4,15,1,207,119,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,19, 
  10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,207,119,0,0,1,15, 
  1,207,119,0,0,17,1,14,120,0,0,1,1,2,21,1,63,0,0,0,236,157,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,240,1,128,2,240,1,128,27,128,1,128,4,15,1,231,15,0,0,17,1,225,13,0, 
  0,1,4,15,1,231,15,0,0,17,1,141,13,0,0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,21,1,93,0,0,0,139,158,0,0,7,0,0,0,2,0,0,0,32,248,1,128,1,112,1,129,2,112,1,129, 
  23,120,66,128,31,56,2,128,33,184,1,128,34,120,1,128,8,4,17,1,72,124,0,0,1,4,17,1,53,123,0,0,1,4,17,1,34,122,0,0,1,4,17,1,184,120,0,0,1,4,19,19,0,0,0,50,0,0,0,3, 
  0,14,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15, 
  1,84,121,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,84,121,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,84,121,0,0,15,1,125,14, 
  0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,84,121,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144, 
  2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,84,121,0,0,17,1,205,17,0,0,1,15,1,84,121,0,0,17,1,203,121,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,84,121,0, 
  0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,84,121,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,84,121,0,0,1,2,21,1,75,0,0,0,236,157,0,0,4,0,0,0,2,0,0,0,28,176,1, 
  128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,207,119,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,4,15,1,207,119,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31, 
  0,0,0,1,0,1,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1, 
  0,15,1,190,122,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,190,122,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,190,122,0,0,15,1, 
  125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,190,122,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,190,122,0,0,17,1,27,21,0,0,1,15,1,190,122,0,0,17,1,120,119,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,190, 
  122,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,190,122,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,190,122,0,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20, 
  72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,209,123,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0, 
  0,65,0,0,0,1,0,15,1,209,123,0,0,15,1,247,27,0,0,17,1,174,126,0,0,1,4,15,1,209,123,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27, 
  0,0,0,71,0,0,0,1,0,17,1,209,123,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,209,123,0, 
  0,17,1,239,22,0,0,1,15,1,209,123,0,0,17,1,120,119,0,0,1,19,27,0,0,0,73,0,0,0,1,0,17,1,209,123,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,209,123,0,0,1,19,27,0, 
  0,0,72,0,0,0,1,0,17,1,209,123,0,0,1,2,21,1,155,0,0,0,33,142,0,0,6,0,0,0,2,0,0,0,20,72,3,128,1,80,193,128,2,80,193,128,11,232,3,128,29,80,2,128,30,88,1,128,8,4, 
  19,26,0,0,0,66,0,0,0,1,0,15,1,228,124,0,0,15,1,247,27,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,228,124,0,0,15,1,247,27,0,0,17,1,174,126,0,0, 
  1,4,15,1,228,124,0,0,15,1,125,14,0,0,17,1,185,117,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,71,0,0,0,1,0,17,1,228,124,0,0,1,2,21,0,118,0,0,0,255,255, 
  255,255,5,0,0,0,2,0,0,0,16,144,2,128,17,0,2,128,10,32,131,128,27,48,1,128,26,152,1,128,15,1,228,124,0,0,17,1,127,25,0,0,1,15,1,228,124,0,0,17,1,91,125,0,0,1,19,27,0,0, 
  0,73,0,0,0,1,0,17,1,228,124,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,228,124,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,228,124,0,0,1,2,21,1,75,0,0,0,236,157,0, 
  0,4,0,0,0,2,0,0,0,28,184,1,128,1,176,1,128,2,176,1,128,27,16,1,128,4,15,1,207,119,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,8,4,15,1,207,119,0,0,15,1,142,28,0,0,17, 
  1,225,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,19,2,0,0,0,3,0,0,0,1,0,1,19,3,0,0,0,13,0,0,0,1,0,1,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0, 
  5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,74,126,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0, 
  0,0,65,0,0,0,1,0,15,1,74,126,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,15,1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255, 
  6,0,0,0,2,0,0,0,20,32,2,128,5,176,193,128,6,184,193,128,23,80,1,128,9,176,1,128,26,176,2,128,19,9,0,0,0,29,0,0,0,2,0,1,1,15,1,74,126,0,0,17,1,129,82,0,0,1,19,6, 
  0,0,0,19,0,0,0,1,0,17,1,74,126,0,0,1,15,1,74,126,0,0,17,1,87,81,0,0,1,2,21,1,75,0,0,0,0,159,0,0,4,0,0,0,2,0,0,0,28,16,1,128,1,176,1,128,2,176,1,128, 
  27,184,1,128,4,15,1,5,127,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,4,15,1,5,127,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11,0,0,0,32,0,0,0,1,0,17,1,5,127,0,0,1,15,1,5,127,0,0,17,1,68,127,0,0,1,1,2, 
  21,1,63,0,0,0,0,159,0,0,4,0,0,0,2,0,0,0,28,128,1,128,1,240,1,128,2,240,1,128,27,16,1,128,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0, 
  0,1,8,19,10,0,0,0,30,0,0,0,2,0,14,1,21,1,75,0,0,0,0,159,0,0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,5,127,0,0,15,1,142, 
  28,0,0,17,1,141,13,0,0,1,4,15,1,5,127,0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,38,1,0,0,255,255,255,255,14,0,0,0,3,0,0, 
  0,0,40,201,129,1,192,200,129,2,48,200,129,3,160,71,130,12,208,4,128,5,16,7,128,22,184,2,128,8,128,198,128,9,240,197,128,10,96,5,129,16,64,4,128,17,176,3,128,19,32,3,128,26,80,2,128,15,1,231, 
  127,0,0,17,1,109,132,0,0,1,15,1,231,127,0,0,17,1,137,131,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,231,127,0,0,1,19,3,0,0,0,12,0,0,0,1,0,17,1,231,127,0,0,1,19, 
  3,0,0,0,11,0,0,0,1,0,17,1,231,127,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,231,127,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,231,127,0,0,1,19,2,0,0,0,6,0, 
  0,0,1,0,17,1,231,127,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,231,127,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,231,127,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1, 
  231,127,0,0,1,19,1,0,0,0,1,0,0,0,1,0,17,1,231,127,0,0,1,15,1,231,127,0,0,17,1,14,129,0,0,1,1,2,21,1,99,2,0,0,104,133,0,0,19,0,0,0,4,0,0,0,17,128,7,128, 
  1,240,194,127,2,240,2,128,3,0,18,128,4,232,16,131,5,72,16,128,22,224,5,128,7,168,15,128,8,144,14,128,9,120,13,130,10,96,12,128,11,240,10,128,12,216,9,128,13,192,72,129,30,248,2,128,15,32,8,128, 
  20,176,6,128,25,232,4,128,29,240,3,128,8,4,19,26,0,0,0,66,0,0,0,1,0,15,1,125,131,0,0,15,1,91,133,0,0,17,1,144,127,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,125,131, 
  0,0,15,1,91,133,0,0,17,1,174,126,0,0,1,4,19,22,0,0,0,58,0,0,0,1,0,15,1,125,131,0,0,15,1,126,94,0,0,17,1,154,93,0,0,1,4,15,1,125,131,0,0,15,1,178,125,0,0,15, 
  1,190,125,0,0,17,1,96,118,0,0,1,4,15,1,125,131,0,0,15,1,178,125,0,0,15,1,84,118,0,0,17,1,185,117,0,0,1,4,15,1,125,131,0,0,15,1,173,117,0,0,17,1,132,108,0,0,1,4,15, 
  1,125,131,0,0,15,1,173,117,0,0,17,1,91,99,0,0,1,4,19,8,0,0,0,28,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,27, 
  0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0, 
  1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,25,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,24,0,0,0,1, 
  0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,23,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4, 
  15,1,125,131,0,0,15,1,79,99,0,0,17,1,34,99,0,0,1,4,15,1,125,131,0,0,15,1,173,117,0,0,17,1,211,2,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0, 
  1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,3,0,0,0,9,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,19,0,0,0,0,0,0,0,0,1,0, 
  1,19,1,0,0,0,2,0,0,0,2,0,1,21,1,127,0,0,0,145,137,0,0,6,0,0,0,2,0,0,0,5,184,3,129,1,80,193,127,2,80,193,128,15,72,3,128,29,80,2,128,30,88,1,128,8,4,19,26,0, 
  0,0,66,0,0,0,1,0,15,1,9,132,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,19,26,0,0,0,65,0,0,0,1,0,15,1,9,132,0,0,15,1,246,89,0,0,17,1,87,81,0,0,1,4,15, 
  1,75,81,0,0,17,1,254,71,0,0,1,4,17,1,34,6,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,88,1,128,5,80,193,128,6,80,194,128,23,184,2,128,9,80,1,128,26, 
  232,1,128,1,19,6,0,0,0,19,0,0,0,1,0,17,1,9,132,0,0,1,15,1,9,132,0,0,17,1,87,81,0,0,1,15,1,9,132,0,0,17,1,129,82,0,0,1,19,9,0,0,0,29,0,0,0,2,0,1, 
  2,21,1,75,0,0,0,158,160,0,0,4,0,0,0,2,0,0,0,28,176,1,128,1,80,2,128,2,80,2,128,27,16,1,128,4,15,1,196,132,0,0,15,1,142,28,0,0,17,1,141,13,0,0,1,4,15,1,196,132, 
  0,0,15,1,142,28,0,0,17,1,225,13,0,0,1,8,19,10,0,0,0,31,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,10,232,1,128,11,128,65,128,25,240,0,128,19,11, 
  0,0,0,32,0,0,0,1,0,17,1,196,132,0,0,1,15,1,196,132,0,0,17,1,3,133,0,0,1,1,2,21,1,63,0,0,0,158,160,0,0,4,0,0,0,2,0,0,0,28,136,1,128,1,16,1,128,2,16,1, 
  128,27,24,1,128,8,4,15,1,231,15,0,0,17,1,141,13,0,0,1,4,15,1,231,15,0,0,17,1,225,13,0,0,1,19,10,0,0,0,30,0,0,0,2,0,14,1,19,1,0,0,0,1,0,0,0,1,0,1,19, 
  2,0,0,0,7,0,0,0,1,0,1,13,21,4,105,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,208,65,128,64,8,3,128,91,8,2,129,35,136,194,127,92,200,2,128,45,72,2,128,123,144,1,128,95,72, 
  2,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,21,2,78, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3, 
  17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,18,7,0,0,0,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,59,80,129,128,45,136,1,128,95,16,1,128,43,192,65,127,3,17,1,131,134, 
  0,0,1,3,18,10,0,0,0,1,3,18,9,0,0,0,1,3,18,8,0,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,131,134,0,0,1,1,18,29,0,0,0, 
  21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,131,134,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128, 
  3,17,1,131,134,0,0,1,1,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,32,104,129,128,109,40,1,128,110,240,0,128,3,18,13,0,0,0,1,3,17,1,2,135,0,0,1,3,18,12,0,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,33,135,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,64,135, 
  0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,95,135,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,4, 
  0,0,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,190,135,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0, 
  128,3,17,1,190,135,0,0,1,2,18,25,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,190,135,0,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,190,135,0,0,1,1,18,17,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,91,176,0,128,3,18,22,0,0,0,1,1,18, 
  3,0,0,0,1,18,30,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,49,136,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,4,208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,18,11,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,123,136,0,0,1,1,18,2,0,0,0, 
  1,18,1,0,0,0,1,18,5,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,3,18,20,0,0,0,1,1,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0, 
  40,240,65,128,64,40,3,128,91,40,2,129,35,168,194,127,92,232,2,128,45,104,194,128,123,176,1,128,95,104,2,128,125,104,3,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3, 
  17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,18,6,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128, 
  5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,68, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,112,129,128,45,224,1,128,64,48,1,128,95,224,65,128,123,168,1,128,3,17,1,12,138,0,0,1,3,18,15,0,0,0,1,3,18,5,0,0,0,1,3,17,1, 
  49,136,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,49,136,0,0,1,2, 
  21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,131,134,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,131, 
  134,0,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,16,66,128,64,72,3,128,91,72,130,129,35,200,194,127,92,8,3,128,45,136,66,128,109,136,195,128,95,136,2,128,123,208,1,128,125, 
  200,3,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17, 
  1,28,139,0,0,1,3,18,6,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3, 
  17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,18,30,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45, 
  48,193,127,97,240,0,128,3,17,1,114,139,0,0,1,3,17,1,49,136,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,18,30, 
  0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,49,136,0,0,1,3,17,1,200,139,0,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,18,30,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,48,1,128,3, 
  17,1,49,136,0,0,1,3,17,1,30,140,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,18,30,0,0,0,21,4,46,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,49,136,0,0,1,3,17,1,116,140,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,18,40,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,49,136,0,0,1,21,2,34,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,21,4,140,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,112,66,128,64,168,3,128,91,168,194,129, 
  35,40,195,127,92,104,3,128,45,232,194,128,46,232,3,128,95,232,2,128,109,240,129,128,123,48,2,128,125,40,4,128,3,17,1,28,139,0,0,1,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136, 
  0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,153,141,0,0,1,3,18,6,0,0,0,1,21,2,78,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1, 
  3,17,1,43,136,0,0,1,2,18,27,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,91,176,0,128,3,18,28,0,0,0,1,1,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,64,240,0,128,45,48,65,128,95,48,1,128,3,17,1,12,138,0,0,1,3,17,1,49,136,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128, 
  3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,49,136,0,0,1,2,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,64,16,1,128,45,80,1,128,123,144,1,128,95,80,193,127,3, 
  17,1,12,138,0,0,1,3,17,1,49,136,0,0,1,3,17,1,158,142,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1, 
  165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,3,18,20,0,0,0,1,2, 
  21,4,85,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,46,192,1,128,45,0,2,129,42,112,130,127,43,56,66,128,47,136,1,128,93,80,1,128,3,18,18,0,0,0,1,3,18,34,0,0,0,1,3,17,1,153, 
  141,0,0,1,3,18,33,0,0,0,1,3,18,32,0,0,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,165,136,0,0,1,3, 
  17,1,159,136,0,0,1,2,21,4,85,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,46,192,1,128,45,0,2,128,42,112,130,127,43,56,66,128,47,136,65,128,123,80,1,128,3,18,5,0,0,0,1,3,18,34, 
  0,0,0,1,3,17,1,153,141,0,0,1,3,18,33,0,0,0,1,3,18,32,0,0,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3, 
  17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,16,130,128,41,200,2,128,64,128,3,128,35,0,195,128,92,64,3,128,45,136,194,128,91,72, 
  194,128,95,136,2,128,109,192,3,128,123,208,1,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,18,16,0,0,0,1,3,17,1,125,135,0,0,1, 
  3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,28,139,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48, 
  1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,140,0,0,0,255,255,255,255,11,0,0,0,3,0,0, 
  0,40,112,130,128,41,40,3,128,64,224,3,128,35,96,67,129,92,160,3,128,45,232,2,129,46,32,4,128,95,232,2,128,91,168,130,128,109,240,1,128,123,48,2,128,3,17,1,28,139,0,0,1,3,17,1,171,136,0,0, 
  1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,18,16,0,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,153,141, 
  0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1, 
  123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,16,66,128,64,72,3,128,91,72,194,129,35,200,194,127,92,8,3,128, 
  45,136,66,128,93,136,131,128,95,136,2,128,109,192,3,128,123,208,1,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3, 
  17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,18,18,0,0,0,1,3,17,1,28,139,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128, 
  3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,140,0,0,0,255,255,255,255,11, 
  0,0,0,3,0,0,0,40,112,66,128,64,168,3,128,91,168,2,130,35,40,195,127,92,104,3,128,45,232,194,128,46,32,4,128,95,232,2,128,93,232,67,128,109,240,1,128,123,48,2,128,3,17,1,28,139,0,0,1,3, 
  17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,18,18,0,0,0, 
  1,3,17,1,153,141,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136, 
  0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,69,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,64,48,1,128,45,176,193,128,123,112,1,128,95,176, 
  193,127,125,240,1,128,3,17,1,12,138,0,0,1,3,17,1,171,136,0,0,1,3,17,1,49,136,0,0,1,3,18,6,0,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5, 
  80,1,128,6,16,1,128,3,208,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,2,21,4,139,0,0,0,255,255,255,255,11,0,0,0,3,0, 
  0,0,40,48,130,128,59,104,66,129,64,160,3,128,35,32,131,127,92,96,3,128,45,224,194,128,91,160,194,128,95,224,2,128,109,224,131,128,123,240,1,128,125,32,4,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0, 
  1,3,18,41,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,28,139,0,0,1,3,18,6,0, 
  0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1, 
  123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,151,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,144,130,128,59,200,194,129,64,0,4,128,35,128,131,127,92,192,3,128, 
  45,64,3,129,46,64,4,128,95,64,3,128,91,0,131,128,109,16,130,128,123,80,2,128,125,128,4,128,3,17,1,28,139,0,0,1,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,18,41,0,0,0,1,3,17, 
  1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,153,141,0,0,1,3,18,6,0,0,0,1,21,2,78,0,0,0,255, 
  255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136, 
  0,0,1,3,17,1,43,136,0,0,1,2,21,4,69,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,64,48,1,128,41,176,65,128,45,112,1,128,95,112,65,128,123,232,1,128,3,17,1,12,138,0,0,1,3,17, 
  1,49,136,0,0,1,3,18,16,0,0,0,1,3,17,1,158,142,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,165,136, 
  0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,2,21,4,96,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,44,32,2,128,41,200,194,128,42,144,194,128,43,88, 
  194,128,45,232,1,128,46,168,1,128,47,112,1,128,3,18,34,0,0,0,1,3,17,1,153,141,0,0,1,3,18,33,0,0,0,1,3,18,26,0,0,0,1,3,18,32,0,0,0,1,3,18,31,0,0,0,1,3,18,16, 
  0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0, 
  0,2,0,0,0,115,16,1,128,105,208,1,128,110,144,1,128,111,80,65,127,3,17,1,143,151,0,0,1,3,17,1,82,151,0,0,1,3,17,1,21,151,0,0,1,3,17,1,216,150,0,0,1,21,2,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17, 
  1,247,150,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,39,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3, 
  17,1,52,151,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,37,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128, 
  3,17,1,113,151,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,106,176,0,128,3,18,36,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0, 
  128,3,17,1,174,151,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,18,38,0,0,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,64,16, 
  1,128,45,136,1,128,123,80,1,128,95,136,193,127,3,17,1,12,138,0,0,1,3,18,5,0,0,0,1,3,17,1,49,136,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3, 
  112,65,128,5,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,49,136,0,0,1,2,21,4,150,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,80,130,128,41,8,3,128,64,192, 
  3,128,35,64,67,129,92,128,3,128,45,200,2,129,46,56,4,128,95,200,2,128,91,136,130,128,93,0,132,128,123,16,2,128,125,120,4,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0, 
  1,3,17,1,49,136,0,0,1,3,18,16,0,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,18,18,0,0,0,1,3,17,1,153,141,0,0,1,3,18,6,0,0, 
  0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123, 
  136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,16,66,128,64,72,3,128,91,72,130,129,35,200,194,127,92,8,3,128,45, 
  136,2,129,46,136,3,128,95,136,2,128,123,208,1,128,125,200,3,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17, 
  1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,153,141,0,0,1,3,18,6,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3, 
  240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,2,52,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,123,136,0,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40, 
  240,129,128,41,168,2,128,64,96,3,128,35,224,194,128,92,32,3,128,45,104,2,128,91,40,130,128,95,104,2,128,123,176,1,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17, 
  1,49,136,0,0,1,3,18,16,0,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5, 
  112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,128,0, 
  0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,16,130,128,41,200,2,128,64,128,3,128,35,0,67,129,92,64,3,128,45,136,2,128,46,192,3,128,95,136,2,128,91,72,66,128,123,208,1,128,3,17,1,171,136,0, 
  0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,18,16,0,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,17,1,153, 
  141,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17, 
  1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,240,65,128,64,40,3,128,91,40,130,129,35,168,194,127,92,232,2, 
  128,45,104,66,128,93,104,3,128,95,104,2,128,123,176,1,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205, 
  134,0,0,1,3,17,1,32,134,0,0,1,3,18,18,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1, 
  165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,16,66,128, 
  64,72,3,128,91,72,194,129,35,200,194,127,92,8,3,128,45,136,194,128,46,192,3,128,95,136,2,128,93,136,3,128,123,208,1,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3, 
  17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3,17,1,32,134,0,0,1,3,18,18,0,0,0,1,3,17,1,153,141,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0, 
  2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43, 
  136,0,0,1,2,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,18,19,0,0,0,1,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,125,176,0,128,3,17,1,200,157,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,165,136,0,0,1,3,17,1,159, 
  136,0,0,1,3,17,1,230,157,0,0,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,3,18,21,0,0,0,1,2,18,19,0,0,0,1,21,4,86,0,0,0,255,255,255,255,6,0, 
  0,0,2,0,0,0,46,200,1,128,45,8,2,129,42,120,130,127,43,64,66,128,47,144,1,128,93,80,1,128,3,17,1,109,158,0,0,1,3,18,34,0,0,0,1,3,17,1,153,141,0,0,1,3,18,33,0,0,0,1, 
  3,18,32,0,0,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,2,21,4,29, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,93,176,0,128,3,18,23,0,0,0,1,2,21,4,74,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,112,1,128,45,168,193,128,42,24,2,128,43,224,65, 
  127,93,48,1,128,3,17,1,109,158,0,0,1,3,18,34,0,0,0,1,3,18,33,0,0,0,1,3,18,32,0,0,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,16,1,128,3,17,1,165,136,0,0,1,3,17,1,159,136,0,0,1,2,21,4,226,0,0,0,255,255,255,255,18,0,0,0,4,0,0,0,64,136,4,128,59,200,68,128,91,72,132,131,35,208,6,128,92,8, 
  4,128,93,200,131,128,95,136,3,128,109,72,131,130,40,152,6,128,41,96,6,128,42,40,6,128,43,240,133,125,44,184,5,126,45,120,5,126,46,56,5,128,47,0,197,125,123,8,3,128,125,208,2,128,3,18,6,0,0,0, 
  1,3,17,1,171,136,0,0,1,3,17,1,28,139,0,0,1,3,17,1,49,136,0,0,1,3,17,1,123,160,0,0,1,3,17,1,205,134,0,0,1,3,17,1,8,136,0,0,1,3,17,1,32,134,0,0,1,3,18,41, 
  0,0,0,1,3,18,34,0,0,0,1,3,17,1,153,141,0,0,1,3,17,1,49,160,0,0,1,3,18,26,0,0,0,1,3,18,32,0,0,0,1,3,18,31,0,0,0,1,3,18,16,0,0,0,1,3,18,15,0,0, 
  0,1,3,17,1,125,135,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0,1,3,17,1,159, 
  136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2,18,33,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127, 
  3,17,1,49,136,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,136,0,0,1,1,18,18,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,93,176,0,128,3,18,23,0,0,0,1,1,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,240,65,128,64,40,3,128,91,40,130,129,35,168,194,127,92,232,2,128,45,104,2,128, 
  46,104,3,128,95,104,2,128,123,176,1,128,3,17,1,171,136,0,0,1,3,18,15,0,0,0,1,3,17,1,8,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,125,135,0,0,1,3,17,1,205,134,0,0,1,3, 
  17,1,32,134,0,0,1,3,17,1,153,141,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,165,136,0,0, 
  1,3,17,1,159,136,0,0,1,3,17,1,123,136,0,0,1,3,17,1,49,136,0,0,1,3,17,1,43,136,0,0,1,2, 
];

pub mod ast{
  impl AstObject for ASTNode {}
  type ASTSlot = (ASTNode, TokenRange, TokenRange);
  use super::*; 
  type Node = ASTNode;
  
  pub fn default_from<'a> (mut reader: UTF8StringReader)-> Result<Vec<ASTNode>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(8);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0.into_nodes();
    Ok(obj_0_0)
  }
}