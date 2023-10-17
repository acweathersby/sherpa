
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
  Assign(Box<Assign>),
  NewLine(Box<NewLine>),
  Match(Box<Match>),
  Literal(Box<Literal>),
  StringType(Box<StringType>),
  Param(Box<Param>),
  ObjType(Box<ObjType>),
  ExprTuple(Box<ExprTuple>),
  Call(Box<Call>),
  Tab(Box<Tab>),
  Expression(Box<Expression>),
  BreakPoint(Box<BreakPoint>),
  Text(Box<Text>),
  Funct(Box<Funct>),
  Space(Box<Space>),
  Prop(Box<Prop>),
  Mul(Box<Mul>),
  Div(Box<Div>),
  LiteralSpace(Box<LiteralSpace>),
  IntType(Box<IntType>),
  Pow(Box<Pow>),
  Add(Box<Add>),
  Ignore(Box<Ignore>),
  Obj(Box<Obj>),
  Sub(Box<Sub>),
  Id(Box<Id>),
  SBlock(Box<SBlock>),
  FloatType(Box<FloatType>),
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
  Assign,
  NewLine,
  Match,
  Literal,
  StringType,
  Param,
  ObjType,
  ExprTuple,
  Call,
  Tab,
  Expression,
  BreakPoint,
  Text,
  Funct,
  Space,
  Prop,
  Mul,
  Div,
  LiteralSpace,
  IntType,
  Pow,
  Add,
  Ignore,
  Obj,
  Sub,
  Id,
  SBlock,
  FloatType,
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
      ASTNode::Assign(node) => node.tok.clone(),
      ASTNode::NewLine(node) => node.tok.clone(),
      ASTNode::Match(node) => node.tok.clone(),
      ASTNode::Literal(node) => node.tok.clone(),
      ASTNode::StringType(node) => node.tok.clone(),
      ASTNode::Param(node) => node.tok.clone(),
      ASTNode::ObjType(node) => node.tok.clone(),
      ASTNode::Call(node) => node.tok.clone(),
      ASTNode::Tab(node) => node.tok.clone(),
      ASTNode::Expression(node) => node.tok.clone(),
      ASTNode::BreakPoint(node) => node.tok.clone(),
      ASTNode::Text(node) => node.tok.clone(),
      ASTNode::Funct(node) => node.tok.clone(),
      ASTNode::Space(node) => node.tok.clone(),
      ASTNode::Prop(node) => node.tok.clone(),
      ASTNode::Mul(node) => node.tok.clone(),
      ASTNode::Div(node) => node.tok.clone(),
      ASTNode::LiteralSpace(node) => node.tok.clone(),
      ASTNode::IntType(node) => node.tok.clone(),
      ASTNode::Pow(node) => node.tok.clone(),
      ASTNode::Add(node) => node.tok.clone(),
      ASTNode::Obj(node) => node.tok.clone(),
      ASTNode::Sub(node) => node.tok.clone(),
      ASTNode::Id(node) => node.tok.clone(),
      ASTNode::SBlock(node) => node.tok.clone(),
      ASTNode::FloatType(node) => node.tok.clone(),
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
      ASTNode::Assign(..) => ASTNodeType::Assign,
      ASTNode::NewLine(..) => ASTNodeType::NewLine,
      ASTNode::Match(..) => ASTNodeType::Match,
      ASTNode::Literal(..) => ASTNodeType::Literal,
      ASTNode::StringType(..) => ASTNodeType::StringType,
      ASTNode::Param(..) => ASTNodeType::Param,
      ASTNode::ObjType(..) => ASTNodeType::ObjType,
      ASTNode::ExprTuple(..) => ASTNodeType::ExprTuple,
      ASTNode::Call(..) => ASTNodeType::Call,
      ASTNode::Tab(..) => ASTNodeType::Tab,
      ASTNode::Expression(..) => ASTNodeType::Expression,
      ASTNode::BreakPoint(..) => ASTNodeType::BreakPoint,
      ASTNode::Text(..) => ASTNodeType::Text,
      ASTNode::Funct(..) => ASTNodeType::Funct,
      ASTNode::Space(..) => ASTNodeType::Space,
      ASTNode::Prop(..) => ASTNodeType::Prop,
      ASTNode::Mul(..) => ASTNodeType::Mul,
      ASTNode::Div(..) => ASTNodeType::Div,
      ASTNode::LiteralSpace(..) => ASTNodeType::LiteralSpace,
      ASTNode::IntType(..) => ASTNodeType::IntType,
      ASTNode::Pow(..) => ASTNodeType::Pow,
      ASTNode::Add(..) => ASTNodeType::Add,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::Obj(..) => ASTNodeType::Obj,
      ASTNode::Sub(..) => ASTNodeType::Sub,
      ASTNode::Id(..) => ASTNodeType::Id,
      ASTNode::SBlock(..) => ASTNodeType::SBlock,
      ASTNode::FloatType(..) => ASTNodeType::FloatType,
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
      Assign(node) => node.hash(hasher),
      NewLine(node) => node.hash(hasher),
      Match(node) => node.hash(hasher),
      Literal(node) => node.hash(hasher),
      StringType(node) => node.hash(hasher),
      Param(node) => node.hash(hasher),
      ObjType(node) => node.hash(hasher),
      ExprTuple(node) => node.hash(hasher),
      Call(node) => node.hash(hasher),
      Tab(node) => node.hash(hasher),
      Expression(node) => node.hash(hasher),
      BreakPoint(node) => node.hash(hasher),
      Text(node) => node.hash(hasher),
      Funct(node) => node.hash(hasher),
      Space(node) => node.hash(hasher),
      Prop(node) => node.hash(hasher),
      Mul(node) => node.hash(hasher),
      Div(node) => node.hash(hasher),
      LiteralSpace(node) => node.hash(hasher),
      IntType(node) => node.hash(hasher),
      Pow(node) => node.hash(hasher),
      Add(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      Obj(node) => node.hash(hasher),
      Sub(node) => node.hash(hasher),
      Id(node) => node.hash(hasher),
      SBlock(node) => node.hash(hasher),
      FloatType(node) => node.hash(hasher),
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
pub struct Assign{
  pub expr:ASTNode, 
  pub id:Box<Id>, 
  pub tok: Token, 
}

impl Assign{
  
  pub fn new (expr: ASTNode, id: Box<Id>, tok: Token)-> Self {
    
    Self{
      expr,
      id,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Assign
  }
}

impl ASTNode{
  
  pub fn to_Assign (self)-> Box::<Assign> {
    
    match self{
      Self::Assign(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Assign (&self)-> Option<&Assign> {
    
    match self{
      Self::Assign(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Assign_mut (&mut self)-> Option<&mut Assign> {
    
    match self{
      Self::Assign(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Assign{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expr.hash(hasher);
    self.id.hash(hasher);
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
pub struct ExprTuple{
  pub expressions:Vec<ASTNode>, 
}

impl ExprTuple{
  
  pub fn new (expressions: Vec<ASTNode>)-> Self {
    
    Self{
      expressions,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::ExprTuple
  }
}

impl ASTNode{
  
  pub fn to_ExprTuple (self)-> Box::<ExprTuple> {
    
    match self{
      Self::ExprTuple(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_ExprTuple (&self)-> Option<&ExprTuple> {
    
    match self{
      Self::ExprTuple(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_ExprTuple_mut (&mut self)-> Option<&mut ExprTuple> {
    
    match self{
      Self::ExprTuple(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for ExprTuple{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.expressions{
      val.hash(hasher);
    }
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
pub struct Tab{
  pub tok: Token, 
}

impl Tab{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Tab
  }
}

impl ASTNode{
  
  pub fn to_Tab (self)-> Box::<Tab> {
    
    match self{
      Self::Tab(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Tab (&self)-> Option<&Tab> {
    
    match self{
      Self::Tab(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Tab_mut (&mut self)-> Option<&mut Tab> {
    
    match self{
      Self::Tab(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Tab{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
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
pub struct LiteralSpace{
  pub content:Vec<ASTNode>, 
  pub tok: Token, 
}

impl LiteralSpace{
  
  pub fn new (content: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      content,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::LiteralSpace
  }
}

impl ASTNode{
  
  pub fn to_LiteralSpace (self)-> Box::<LiteralSpace> {
    
    match self{
      Self::LiteralSpace(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_LiteralSpace (&self)-> Option<&LiteralSpace> {
    
    match self{
      Self::LiteralSpace(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_LiteralSpace_mut (&mut self)-> Option<&mut LiteralSpace> {
    
    match self{
      Self::LiteralSpace(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for LiteralSpace{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.content{
      val.hash(hasher);
    }
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
pub struct Pow{
  pub l:ASTNode, 
  pub r:ASTNode, 
  pub tok: Token, 
}

impl Pow{
  
  pub fn new (l: ASTNode, r: ASTNode, tok: Token)-> Self {
    
    Self{
      l,
      r,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Pow
  }
}

impl ASTNode{
  
  pub fn to_Pow (self)-> Box::<Pow> {
    
    match self{
      Self::Pow(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Pow (&self)-> Option<&Pow> {
    
    match self{
      Self::Pow(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Pow_mut (&mut self)-> Option<&mut Pow> {
    
    match self{
      Self::Pow(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Pow{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.l.hash(hasher);
    self.r.hash(hasher);
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
pub struct Ignore{
}

impl Ignore{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Ignore
  }
}

impl ASTNode{
  
  pub fn to_Ignore (self)-> Box::<Ignore> {
    
    match self{
      Self::Ignore(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Ignore (&self)-> Option<&Ignore> {
    
    match self{
      Self::Ignore(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Ignore_mut (&mut self)-> Option<&mut Ignore> {
    
    match self{
      Self::Ignore(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Ignore{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
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
pub struct FloatType{
  pub tok: Token, 
}

impl FloatType{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::FloatType
  }
}

impl ASTNode{
  
  pub fn to_FloatType (self)-> Box::<FloatType> {
    
    match self{
      Self::FloatType(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_FloatType (&self)-> Option<&FloatType> {
    
    match self{
      Self::FloatType(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_FloatType_mut (&mut self)-> Option<&mut FloatType> {
    
    match self{
      Self::FloatType(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for FloatType{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
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


/* assignment */
fn reducer_004 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* function */
fn reducer_005 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* format */
fn reducer_006 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* call */
fn reducer_007 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_008 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal_space */
fn reducer_009 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* block::<t_SBlock, script_statement*> */
fn reducer_010 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( c:sym ) 
                                    :ast { t_Text, val: str(tok), tok } */
fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:( "\\" c:any ) 
                                    :ast { t_Text, val: str(tok<1>), tok } */
fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* at_string_literal */
fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression */
fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* binding_id "={"{:9999} expr "}"
                                    :ast { t_Assign, id: $1, expr: $3, tok } */
fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2;
  let obj_0_1 = ref_0;
  let obj_0_1 = obj_0_1.to_Id();
  let var_5_0 = Assign::new(
    obj_2_0,
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Assign(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{1}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* fn_name param* '{' function_statement* "}"{1}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* fn_name param* '{' function_statement* "}"{1}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_020 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* fn_name param* '{' function_statement* "}"{1}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4, tok } */
fn reducer_021 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_022 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* param* */
fn reducer_023 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_024 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement* */
fn reducer_025 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" "+"{:9999})          :ast { t_Indent, tok } */
fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Indent::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Indent(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" "-"{:9999})          :ast { t_Dedent, tok } */
fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Dedent::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Dedent(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" ";"{:9999})          :ast { t_BreakPoint, tok } */
fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = BreakPoint::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::BreakPoint(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "@" tk:(c:num+)               :ast { t_Space, count: u32($2), tok } */
fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Space::new(
    0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Space(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" "n"{:9999})         :ast { t_NewLine, tok } */
fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NewLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NewLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name args              :ast { t_Call, name: str($1), args: $2, tok } */
fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* id object_accessor*          
                                    :ast { t_Obj, id: $1, path:$2, tok } */
fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* id object_accessor*          
                                    :ast { t_Obj, id: $1, path:$2, tok } */
fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* object_accessor* */
fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "@{"  literal_space_statement+  "}"     :ast { t_LiteralSpace, content: $2, tok } */
fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = LiteralSpace::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::LiteralSpace(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement */
fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement+ */
fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "[" Content? "]"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" Content? ")"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" Content? ")"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "{" Content? "}"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "{" Content? "}"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement* */
fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* script_statement */
fn reducer_048 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement* */
fn reducer_049 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* script_statement */
fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* script_statement* */
fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "@\""  tk:( c:any | "\\" c:any )+  "\""     :ast { t_Literal, val: str($2), tok } */
fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:( c:any | "\\" c:any ) */
fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* tk:( c:any | "\\" c:any )+ */
fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "@["  expr  "]"              :ast { t_Expression, val: $2, tok } */
fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:( "@" ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* )
                                    :ast { t_Id, name: str($1), at: true, tok } */
fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* expr "+"{1} expr{1}           :ast { t_Add, l:$1, r:$3, tok } */
fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* expr "^"{4} expr{4}           :ast { t_Pow, l:$1, r:$3, tok } */
fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Pow::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pow(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "/"{2} expr{2}           :ast { t_Div, l:$1, r:$3, tok } */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* at_string_literal */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* plain_string_literal */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* id ":" type              
                                    :ast { t_Param, name: $1, ty: str($3), tok } */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* assignment */
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* format */
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* call */
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* match */
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal_space */
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* block::<t_SBlock, function_statement*> */
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:( '#' ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) */
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "(" expr(*",") ")" 
                                    :ast { [$2] } */
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "(" expr(*",") ")" 
                                    :ast { [$2] } */
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = vec![];;
  slots.assign(0, AstSlot(ASTNode::NODES(obj_3_0), __rule_rng__, TokenRange::default()));
}


/* expr */
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expr(*",") */
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "." id                        :ast { t_Prop, name: str($2), tok } */
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* binding_id */
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* non_binding_id */
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* text */
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* format */
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal_format */
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* call */
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* block::<t_SBlock, literal_space_statement+> */
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "\""  tk:( c:any | "\\" c:any )+  "\""     :ast { t_Literal, val: str($2), tok } */
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:( c:any | "\\" c:any ) */
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* tk:( c:any | "\\" c:any )+ */
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "obj"                         :ast { t_ObjType, tok } */
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = ObjType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ObjType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "num"                         :ast { t_NumType, tok } */
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NumType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NumType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "int"                         :ast { t_IntType, tok } */
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = IntType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::IntType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "flt"                         :ast { t_FloatType, tok } */
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = FloatType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::FloatType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "str"                         :ast { t_StringType, tok } */
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = StringType::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::StringType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "match" (expr | tuple) "{"  match_arm*  "}" 
                                    :ast { t_Match, expr: $2, matches:$4, tok } */
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "match" (expr | tuple) "{"  match_arm*  "}" 
                                    :ast { t_Match, expr: $2, matches:$4, tok } */
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "match" (expr | tuple) "{"  match_arm*  "}" 
                                    :ast { t_Match, expr: $2, matches:$4, tok } */
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "match" (expr | tuple) "{"  match_arm*  "}" 
                                    :ast { t_Match, expr: $2, matches:$4, tok } */
fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* expr */
fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tuple */
fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* match_arm */
fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* match_arm* */
fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "[" Content? "]"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" Content? ")"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" Content? ")"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "{" Content? "}"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "{" Content? "}"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement* */
fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement* */
fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement* */
fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:( ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) 
                                    :ast { t_Id, name: str($1), tok } */
fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:( c:tab )                  :ast { t_Tab, tok } */
fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Tab::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Tab(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( " " )                    :ast { t_Space, tok } */
fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Space::new(
    0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Space(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( c:nl )                   :ast { t_NewLine, tok } */
fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NewLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NewLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "[" Content? "]"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" Content? ")"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" Content? ")"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_126 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "{" Content? "}"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_127 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "{" Content? "}"{1}            :ast { t_T, ty: str($1), content: $2, tok } */
fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* literal_space_statement */
fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement+ */
fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement */
fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement+ */
fn reducer_132 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement */
fn reducer_133 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* literal_space_statement+ */
fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "(" ( expr | "_" :ast { t_Ignore } )(+",") ")" 
                                    :ast { t_ExprTuple, expressions: $2 } */
fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = ExprTuple::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::ExprTuple(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr */
fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "_" :ast { t_Ignore } */
fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Ignore::new();
  slots.assign(0, AstSlot(ASTNode::Ignore(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( expr | "_" :ast { t_Ignore } ) */
fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( expr | "_" :ast { t_Ignore } )(+",") */
fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* (expr | tuple) "{" ( function_statement )*{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3, tok } */
fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* (expr | tuple) "{" ( function_statement )*{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3, tok } */
fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* (expr | tuple) "{" ( function_statement )*{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3, tok } */
fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* (expr | tuple) "{" ( function_statement )*{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3, tok } */
fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* expr */
fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tuple */
fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( function_statement ) */
fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( function_statement )* */
fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( function_statement ) */
fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( function_statement )* */
fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 156]
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
      reducer_106::<R, M, UP>,
      reducer_107::<R, M, UP>,
      reducer_108::<R, M, UP>,
      reducer_109::<R, M, UP>,
      reducer_110::<R, M, UP>,
      reducer_111::<R, M, UP>,
      reducer_112::<R, M, UP>,
      reducer_113::<R, M, UP>,
      reducer_114::<R, M, UP>,
      reducer_115::<R, M, UP>,
      reducer_116::<R, M, UP>,
      reducer_117::<R, M, UP>,
      reducer_118::<R, M, UP>,
      reducer_119::<R, M, UP>,
      reducer_120::<R, M, UP>,
      reducer_121::<R, M, UP>,
      reducer_122::<R, M, UP>,
      reducer_123::<R, M, UP>,
      reducer_124::<R, M, UP>,
      reducer_125::<R, M, UP>,
      reducer_126::<R, M, UP>,
      reducer_127::<R, M, UP>,
      reducer_128::<R, M, UP>,
      reducer_129::<R, M, UP>,
      reducer_130::<R, M, UP>,
      reducer_131::<R, M, UP>,
      reducer_132::<R, M, UP>,
      reducer_133::<R, M, UP>,
      reducer_134::<R, M, UP>,
      reducer_135::<R, M, UP>,
      reducer_136::<R, M, UP>,
      reducer_137::<R, M, UP>,
      reducer_138::<R, M, UP>,
      reducer_139::<R, M, UP>,
      reducer_140::<R, M, UP>,
      reducer_141::<R, M, UP>,
      reducer_142::<R, M, UP>,
      reducer_143::<R, M, UP>,
      reducer_144::<R, M, UP>,
      reducer_145::<R, M, UP>,
      reducer_146::<R, M, UP>,
      reducer_147::<R, M, UP>,
      reducer_148::<R, M, UP>,
      reducer_149::<R, M, UP>,
      reducer_150::<R, M, UP>,
      reducer_151::<R, M, UP>,
      reducer_152::<R, M, UP>,
      reducer_153::<R, M, UP>,
      reducer_154::<R, M, UP>,
      reducer_155::<R, M, UP>,
    ])
  }
}
    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<T, UserCTX, Bytecode> = sherpa_rust_runtime::deprecate::ByteCodeParser<T, UserCTX, Bytecode>;

pub mod meta{
  
  pub const nonterm_names: [&'static str;58] = [
    "ascript_form",
    "ascript_form_list",
    "script_statement",
    "text",
    "text_group",
    "assignment",
    "function",
    "function_list",
    "function_list_1",
    "format",
    "call",
    "object",
    "object_list",
    "literal_space",
    "literal_space_list",
    "block_template_5912456036338616932",
    "block_template_5912456036338616932_list",
    "block_template_5912456036338616932_list_1",
    "block_template_5912456036338616932_list_2",
    "num",
    "at_string_literal",
    "at_string_literal_list",
    "expression",
    "binding_id",
    "expr",
    "param",
    "function_statement",
    "fn_name",
    "args",
    "args_list",
    "object_accessor",
    "id",
    "literal_space_statement",
    "plain_string_literal",
    "plain_string_literal_list",
    "type",
    "match",
    "match_group",
    "match_list_1",
    "block_template_7571693507362039102",
    "block_template_7571693507362039102_list",
    "block_template_7571693507362039102_list_1",
    "block_template_7571693507362039102_list_2",
    "non_binding_id",
    "literal_format",
    "block_template_9874968537493629707",
    "block_template_9874968537493629707_list",
    "block_template_9874968537493629707_list_1",
    "block_template_9874968537493629707_list_2",
    "tuple",
    "tuple_group",
    "tuple_list_1",
    "match_arm",
    "match_arm_group",
    "match_arm_group_1",
    "match_arm_list_2",
    "match_arm_group_3",
    "match_arm_list_4",
  ];
  
  pub const symbol_string: [&'static str;48] = [
    r####"Default"####,
    r####"c:sp"####,
    r####"c:nl"####,
    r####"c:sym"####,
    r####"tk:nonterm"####,
    r####" } "####,
    r####" ={ "####,
    r####" { "####,
    r####" @ "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" @{ "####,
    r####" ( "####,
    r####" ) "####,
    r####" [ "####,
    r####" ] "####,
    r####"tk:nonterm"####,
    r####" " "####,
    r####" @" "####,
    r####" @[ "####,
    r####"tk:nonterm"####,
    r####" * "####,
    r####" + "####,
    r####" - "####,
    r####" / "####,
    r####" ^ "####,
    r####"nonterm"####,
    r####" : "####,
    r####"tk:nonterm"####,
    r####" , "####,
    r####" . "####,
    r####" .[ "####,
    r####" obj "####,
    r####" num "####,
    r####" str "####,
    r####" flt "####,
    r####" int "####,
    r####" match "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" _ "####,
    r####" ; "####,
  ];
}

pub fn new_default_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(8);
  parser
}

pub static bytecode: [u8; 71884] = [
  0,211,200,197,210,208,193,2,15,1,29,235,0,0,17,1,21,0,0,0,1,21,1,218,2,0,0,30,235,0,0,20,0,0,0,4,0,0,0,16,248,136,132,1,16,3,128,2,16,195,128,3,136,21,128,4,64,20,128,18, 
  40,8,128,22,40,7,128,7,112,83,130,8,160,82,130,9,88,17,128,10,16,80,130,11,200,14,128,12,40,13,128,13,224,11,128,14,152,10,128,15,200,9,128,23,40,6,128,24,96,5,128,32,152,4,128,42,24,3,128,8, 
  4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,178,228,0,0,15,1,5,235,0,0,15,1,17,235,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0, 
  15,1,178,228,0,0,17,1,53,227,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,178,228,0,0,17,1,2,227,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,234,226,0,0,15,1,246, 
  226,0,0,17,1,230,224,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,51,224,0, 
  0,17,1,141,213,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,219,202,0,0,17,1,50,168,0,0,1, 
  4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,2,0,0,0,6,0, 
  0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0, 
  0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,19,9,0,0, 
  0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19, 
  1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15,1,178,228,0,0,15,1,5,235,0,0,15,1,51,224, 
  0,0,17,1,240,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,4,19,3,0,0,0,11,0,0,0, 
  1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,2,21,1,235,2,0,0,233,237,0,0,21,0,0,0,4,0,0,0,16,24,201,132,1,48,3,128,2,48, 
  131,131,3,16,22,128,4,200,20,128,5,96,20,128,22,72,7,128,7,144,147,130,8,192,146,130,9,120,17,128,10,48,144,130,11,232,14,128,12,72,13,128,13,0,12,128,14,184,10,128,15,232,9,128,18,72,8,128,23,72, 
  6,128,24,128,5,128,32,184,4,128,42,56,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,63,150,0,0,15,1,237,167,0,0,15,1,17,235,0,0,17,1,91,228, 
  0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,63,150,0,0,17,1,90,32,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,63,150,0,0,17,1,220,5,0,0,1,4,15,1,63,150,0,0, 
  15,1,237,167,0,0,15,1,234,226,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,63,150,0,0,15,1,237,167,0,0,15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1, 
  63,150,0,0,15,1,237,167,0,0,15,1,51,224,0,0,17,1,141,213,0,0,1,4,15,1,63,150,0,0,15,1,237,167,0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,63,150,0,0,15,1,237,167, 
  0,0,15,1,219,202,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,4,19,9,0, 
  0,0,30,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0, 
  19,2,0,0,0,3,0,0,0,1,0,19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,50,0,0, 
  0,1,0,17,1,63,150,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,4,19,9,0,0,0,26,0,0, 
  0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,4,15,1,63,150,0,0,15,1,237,167,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15, 
  1,63,150,0,0,15,1,237,167,0,0,15,1,51,224,0,0,17,1,240,2,0,0,1,4,19,15,0,0,0,45,0,0,0,2,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0, 
  19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,18,0,0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,2, 
  21,1,39,0,0,0,170,238,0,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0, 
  0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1, 
  0,15,1,20,31,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,20,31,0,0,15,1,78,32,0,0,17,1,140,7,0, 
  0,1,4,15,1,20,31,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,20,31,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63, 
  0,0,0,1,0,17,1,20,31,0,0,1,2,21,1,50,0,0,0,145,240,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,20,248,0,128,8,4,19,34,0,0,0,92,0,0,0,1,0,17,1,12,7, 
  0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,208,0,128,33,56,1,128,15,1,12,7,0,0,17,1,53,7,0,0,1,1,2,21,1,62,0,0,0,239,240,0,0,4,0,0,0,2, 
  0,0,0,20,136,1,128,1,16,129,128,2,16,1,128,21,24,1,128,8,4,19,33,0,0,0,91,0,0,0,3,0,14,1,4,19,34,0,0,0,93,0,0,0,2,0,1,2,19,24,0,0,0,66,0,0,0,1,0,1, 
  19,24,0,0,0,65,0,0,0,1,0,1,21,1,75,0,0,0,74,241,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,125,30,0,0,15,1,8,31,0,0,17, 
  1,19,30,0,0,1,4,15,1,125,30,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69, 
  129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,173,8,0,0,15,1,78,32,0, 
  0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,173,8,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,173,8,0,0,15,1,128, 
  7,0,0,17,1,63,224,0,0,1,4,15,1,173,8,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,173,8,0,0,1, 
  2,21,0,189,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,24,64,3,128,33,64,2,128,19,200,132,129,11,88,197,127,20,56,4,128,31,208,2,128,30,56,3,128,23,168,131,127,43,176,1,128,19,31,0,0,0, 
  84,0,0,0,1,0,17,1,173,8,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,173,8,0,0,1,15,1,173,8,0,0,17,1,188,29,0,0,1,1,15,1,173,8,0,0,17,1,107,9,0,0,1,19,31, 
  0,0,0,83,0,0,0,1,0,17,1,173,8,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,173,8,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,173,8,0,0,1,19,24,0,0,0,64,0,0, 
  0,1,0,17,1,173,8,0,0,1,2,21,7,96,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,47,144,1,128,45,208,193,128,42,80,194,128,43,16,66,127,93,144,2,128,94,80,1,128,4,17,1,57,28,0,0, 
  1,4,17,1,182,26,0,0,1,4,17,1,220,24,0,0,1,4,17,1,77,16,0,0,1,4,17,1,231,9,0,0,1,4,19,30,0,0,0,82,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1, 
  128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,177,10,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0, 
  0,83,0,0,0,1,0,15,1,177,10,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,177,10,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,177,10,0,0,15,1,116,7,0,0,17, 
  1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,177,10,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33, 
  32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,177,10,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,177,10, 
  0,0,1,15,1,177,10,0,0,17,1,188,29,0,0,1,15,1,177,10,0,0,17,1,106,11,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,177,10,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1, 
  177,10,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,177,10,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,177,10,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  42,16,65,128,94,208,0,128,4,17,1,45,14,0,0,1,4,17,1,188,11,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,60,0,0,0, 
  3,0,14,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0, 
  0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,134,12,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,134,12, 
  0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,134,12,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,134,12,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0, 
  0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,134,12,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127, 
  20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,134,12,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,134,12,0,0,1,15,1,134,12,0,0,17,1, 
  63,13,0,0,1,15,1,134,12,0,0,17,1,106,11,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,134,12,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,134,12,0,0,1,19,24,0,0,0,63, 
  0,0,0,1,0,17,1,134,12,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,134,12,0,0,1,2,21,1,75,0,0,0,146,244,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130, 
  127,35,176,1,128,4,15,1,150,13,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,150,13,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0, 
  62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,150,13,0,0,1,15,1,150,13,0,0,17,1,213,13,0,0,1,1, 
  2,21,1,63,0,0,0,146,244,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,240,1,128,2,240,129,127,35,128,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0,1,4,15,1,33,14,0,0,17,1,227,7, 
  0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,19,12,0,0,0,36,0,0,0,2,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21, 
  184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,247,14,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19, 
  23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,247,14,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,247,14,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1, 
  4,15,1,247,14,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,247,14,0,0,1,2,21,0,184,0,0,0,255,255,255, 
  255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,247,14,0,0,1,19, 
  24,0,0,0,66,0,0,0,1,0,17,1,247,14,0,0,1,15,1,247,14,0,0,17,1,246,15,0,0,1,15,1,247,14,0,0,17,1,176,15,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,247,14,0,0, 
  1,19,24,0,0,0,65,0,0,0,1,0,17,1,247,14,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,247,14,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,247,14,0,0,1,2,21,7,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,94,176,0,128,4,17,1,45,14,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,61,0, 
  0,0,3,0,14,1,21,1,75,0,0,0,146,244,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,176,1,128,2,176,129,127,35,184,1,128,4,15,1,150,13,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1, 
  8,4,15,1,150,13,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2, 
  112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,23,17,0,0,15,1,78,32,0,0,17,1,91,228,0, 
  0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,23,17,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,23,17,0,0,15,1,128,7,0,0,17,1,63, 
  224,0,0,1,4,15,1,23,17,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,23,17,0,0,1,2,21,0,184,0,0, 
  0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,23,17, 
  0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,23,17,0,0,1,15,1,23,17,0,0,17,1,250,23,0,0,1,15,1,23,17,0,0,17,1,208,17,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1, 
  23,17,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,23,17,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,23,17,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,23,17,0,0,1,2, 
  21,7,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,240,1,128,45,176,1,128,42,48,129,128,43,112,65,127,94,48,2,128,4,17,1,188,11,0,0,1,4,17,1,119,22,0,0,1,4,17,1,39,20,0, 
  0,1,4,17,1,70,18,0,0,1,4,17,1,45,14,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,58,0,0,0,3,0,14,1,21,1, 
  201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0, 
  0,0,84,0,0,0,1,0,15,1,16,19,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,16,19,0,0,15,1,78,32, 
  0,0,17,1,140,7,0,0,1,4,15,1,16,19,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,16,19,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1, 
  0,19,24,0,0,0,63,0,0,0,1,0,17,1,16,19,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176, 
  2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,16,19,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,16,19,0,0,1,15,1,16,19,0,0,17,1,63,13,0,0,1,15, 
  1,16,19,0,0,17,1,201,19,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,16,19,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,16,19,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17, 
  1,16,19,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,16,19,0,0,1,2,21,7,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,42,112,129,128,47,240,0,128,94,48,1,128,4,17,1,70,18, 
  0,0,1,4,17,1,45,14,0,0,1,4,17,1,188,11,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,62,0,0,0,3,0,14,1,21, 
  1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31, 
  0,0,0,84,0,0,0,1,0,15,1,241,20,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,241,20,0,0,15,1,78, 
  32,0,0,17,1,140,7,0,0,1,4,15,1,241,20,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,241,20,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0, 
  1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,241,20,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31, 
  176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,241,20,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,241,20,0,0,1,15,1,241,20,0,0,17,1,32,22,0,0,1, 
  15,1,241,20,0,0,17,1,170,21,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,241,20,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,241,20,0,0,1,19,24,0,0,0,63,0,0,0,1,0, 
  17,1,241,20,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,241,20,0,0,1,2,21,7,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,112,1,128,45,48,2,128,42,240,129,128,43,48,65,127, 
  94,176,1,128,4,17,1,119,22,0,0,1,4,17,1,70,18,0,0,1,4,17,1,45,14,0,0,1,4,17,1,188,11,0,0,1,4,17,1,39,20,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,59,0,0,0,3,0,14,1,21,1,75,0,0,0,146,244,0,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15, 
  1,150,13,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,150,13,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0, 
  0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1, 
  0,15,1,65,23,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,65,23,0,0,15,1,78,32,0,0,17,1,140,7,0, 
  0,1,4,15,1,65,23,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,65,23,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63, 
  0,0,0,1,0,17,1,65,23,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23, 
  128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,65,23,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,65,23,0,0,1,15,1,65,23,0,0,17,1,63,13,0,0,1,15,1,65,23,0,0,17,1, 
  208,17,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,65,23,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,65,23,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,65,23,0,0,1,19, 
  24,0,0,0,64,0,0,0,1,0,17,1,65,23,0,0,1,2,21,1,75,0,0,0,29,245,0,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,81,24,0,0,15, 
  1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,81,24,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,81,24,0,0,1,15,1,81,24,0,0,17,1,144,24,0,0,1,1,2,21,1,63,0,0,0,29,245,0,0,4,0, 
  0,0,2,0,0,0,34,128,1,128,1,240,1,128,2,240,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,33,0,0,0, 
  2,0,14,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0, 
  0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,166,25,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,166,25, 
  0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,166,25,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,166,25,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0, 
  0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,166,25,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127, 
  20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,166,25,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,166,25,0,0,1,15,1,166,25,0,0,17,1, 
  95,26,0,0,1,15,1,166,25,0,0,17,1,170,21,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,166,25,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,166,25,0,0,1,19,24,0,0,0,63, 
  0,0,0,1,0,17,1,166,25,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,166,25,0,0,1,2,21,1,75,0,0,0,29,245,0,0,4,0,0,0,2,0,0,0,34,184,1,128,1,176,1,128,2,176,129, 
  127,35,16,1,128,4,15,1,81,24,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,4,15,1,81,24,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,1, 
  201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0, 
  0,0,84,0,0,0,1,0,15,1,128,27,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,128,27,0,0,15,1,78,32, 
  0,0,17,1,140,7,0,0,1,4,15,1,128,27,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,128,27,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1, 
  0,19,24,0,0,0,63,0,0,0,1,0,17,1,128,27,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176, 
  2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,128,27,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,128,27,0,0,1,15,1,128,27,0,0,17,1,188,29,0,0,1,15, 
  1,128,27,0,0,17,1,201,19,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,128,27,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,128,27,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17, 
  1,128,27,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,128,27,0,0,1,2,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22, 
  24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,3,29,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0, 
  57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,3,29,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,3,29,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,3, 
  29,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,3,29,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,3,29,0,0,1,19,24,0,0,0, 
  66,0,0,0,1,0,17,1,3,29,0,0,1,15,1,3,29,0,0,17,1,188,29,0,0,1,15,1,3,29,0,0,17,1,176,15,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,3,29,0,0,1,19,24,0, 
  0,0,65,0,0,0,1,0,17,1,3,29,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,3,29,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,3,29,0,0,1,2,21,1,75,0,0,0,29,245, 
  0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,81,24,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,81,24,0,0,15,1,8,31,0,0,17, 
  1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,1,105,0,0,0,168,245,0,0,4,0,0,0,2,0,0,0,24,48,2,128,1,16,1,128,2,16,65,128,42,24,1,128,8,4,19,43,0,0,0, 
  119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,19,30,0,0,0,81,0,0,0,2,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,19,30,0,0,0,81,0,0, 
  0,2,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,125,30,0,0,1,15,1,125,30,0,0,17, 
  1,188,30,0,0,1,1,2,21,1,63,0,0,0,74,241,0,0,4,0,0,0,2,0,0,0,34,128,1,128,1,240,1,128,2,240,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33, 
  14,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,19,12,0,0,0,35,0,0,0,1,0,1,21,0,189,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,24,56,3,128,33,64, 
  2,128,19,192,132,129,11,80,197,127,20,48,4,128,5,224,5,128,31,208,2,128,23,160,195,127,43,176,1,128,19,31,0,0,0,84,0,0,0,1,0,17,1,20,31,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17, 
  1,20,31,0,0,1,15,1,20,31,0,0,17,1,32,22,0,0,1,15,1,20,31,0,0,17,1,210,31,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,20,31,0,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,20,31,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,20,31,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,20,31,0,0,1,1,2,21,7,96,0,0,0,255,255,255,255,6,0,0,0, 
  2,0,0,0,47,144,1,128,45,208,1,129,42,80,130,128,43,16,66,127,94,80,1,128,125,144,2,128,4,17,1,45,14,0,0,1,4,17,1,70,18,0,0,1,4,17,1,39,20,0,0,1,4,17,1,119,22,0,0,1, 
  4,17,1,188,11,0,0,1,4,19,5,0,0,0,17,0,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,24,0,0,0,64,0,0,0,1, 
  0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0, 
  0,0,84,0,0,0,1,0,15,1,220,141,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,220,141,0,0,15,1,51,150, 
  0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,240,32,0,0,1,2,21,1,21,3,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,104,202,132,1,80,3,128,2,80, 
  131,131,3,96,23,128,4,24,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14,8,12,128,15,56,11,128,18,152,9,128,23,152, 
  7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,84,131,0,0,15,1,109,136,0,0,15,1,121,136,0,0, 
  17,1,91,228,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,84,131,0,0,15,1,109,136,0,0,15,1,229, 
  64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,84,131,0,0,17,1,114,64,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,102,64,0,0,15,1,246,226,0,0, 
  17,1,230,224,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1, 
  190,55,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9, 
  0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1, 
  0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0, 
  0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,27,0, 
  0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0, 
  0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17, 
  1,6,36,0,0,1,4,19,6,0,0,0,21,0,0,0,3,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1, 
  4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,2,21,1,21,3,0,0,201,246,0,0,22,0,0,0,4,0,0,0, 
  16,104,202,132,1,80,3,128,2,80,131,131,3,96,23,128,4,24,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14,8,12,128, 
  15,56,11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,79,39,0,0,15,1, 
  254,46,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,79,39,0,0,15,1,254,46,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,79,39,0, 
  0,15,1,254,46,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,79,39,0,0,17,1,28,39,0,0,1,4,15,1,79,39,0,0,15,1,254,46,0,0,15,1, 
  102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,79,39,0,0,15,1,254,46,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,79,39,0,0,15,1,254,46, 
  0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,79,39,0,0,15,1,254,46,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,79,39,0,0,15,1,254,46,0,0,15,1,22,47,0,0, 
  17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0, 
  19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0, 
  0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0, 
  0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0, 
  70,0,0,0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1,4,15,1,79,39,0,0,15,1,254,46,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,79,39,0,0,15,1,254, 
  46,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,39,0,0,0,112,0,0,0,2,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,42,0,0,0,117,0,0, 
  0,1,0,17,1,79,39,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1,2,21,1,39,0,0,0,236,247, 
  0,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,0,116,1,0,0,255,255,255,255,17,0,0,0,4,0,0, 
  0,19,176,7,128,26,152,69,131,27,48,133,131,3,16,75,127,20,32,7,129,5,128,10,128,22,144,6,128,23,40,70,129,36,56,4,128,9,240,9,128,10,96,201,125,11,208,200,125,39,168,3,128,13,64,8,128,42,64,3, 
  128,31,200,4,128,43,176,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,79,39,0,0,1,15,1,79,39,0,0,17,1,166,41,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,79,39,0,0,1,19,26, 
  0,0,0,72,0,0,0,1,0,17,1,79,39,0,0,1,15,1,79,39,0,0,17,1,196,40,0,0,1,15,1,79,39,0,0,17,1,165,64,0,0,1,19,42,0,0,0,117,0,0,0,1,0,17,1,79,39,0,0,1, 
  15,1,79,39,0,0,17,1,28,39,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,79,39,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,79,39,0,0,1,19,3,0,0,0,13,0,0,0,1,0, 
  17,1,79,39,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,79,39,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,79,39,0,0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,79,39,0,0, 
  1,19,26,0,0,0,70,0,0,0,1,0,17,1,79,39,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,79,39,0,0,1,19,26,0,0,0,68,0,0,0,1,0,17,1,79,39,0,0,1,2,21,1,75,0, 
  0,0,209,248,0,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,27,41,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,27,41,0,0,15,1,8, 
  31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0, 
  0,0,1,0,17,1,27,41,0,0,1,15,1,27,41,0,0,17,1,90,41,0,0,1,1,2,21,1,63,0,0,0,209,248,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,240,1,128,2,240,129,127,35,128,1,128, 
  4,15,1,33,14,0,0,17,1,19,30,0,0,1,4,15,1,33,14,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,176,2,0,0,201,246,0,0,22,0,0,0,4,0,0,0, 
  16,120,201,132,1,80,3,128,2,80,131,131,3,104,20,128,4,80,19,128,5,224,18,128,22,8,8,128,7,64,146,130,8,160,145,130,9,136,208,130,10,112,207,130,11,88,14,128,12,232,12,128,13,208,11,128,14,184,10,128, 
  15,24,10,128,18,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,242,46,0,0,15,1, 
  121,136,0,0,17,1,91,228,0,0,1,4,15,1,242,46,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,242,46,0,0,15,1,229,64,0,0,17,1,165,64,0, 
  0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,242,46,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,242,46,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4, 
  15,1,242,46,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,242,46,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,242,46,0,0,15,1,90,64,0,0,17,1, 
  34,47,0,0,1,4,15,1,242,46,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,118,0,0,0,2,0,1, 
  4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,118,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26, 
  0,0,0,68,0,0,0,1,0,19,42,0,0,0,118,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,118,0,0,0,2,0,1,4,19,9,0, 
  0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,118,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,42,0,0,0,118, 
  0,0,0,2,0,1,4,15,1,242,46,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,242,46,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,39,0,0,0,111,0,0,0,3,0,14,1, 
  4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,42,0,0,0,118,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,42, 
  0,0,0,118,0,0,0,2,0,1,2,21,1,39,0,0,0,170,249,0,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1, 
  0,1,21,0,82,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,19,160,6,128,26,16,5,128,27,168,4,131,3,0,74,127,20,16,6,129,5,112,9,128,22,128,5,128,23,24,69,129,36,176,3,128,9,224,8,128, 
  10,80,200,125,11,192,199,125,39,32,3,128,13,48,7,128,43,144,2,128,31,64,4,128,19,31,0,0,0,84,0,0,0,1,0,17,1,138,44,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,138,44,0,0,1, 
  19,26,0,0,0,72,0,0,0,1,0,17,1,138,44,0,0,1,15,1,138,44,0,0,17,1,16,46,0,0,1,15,1,138,44,0,0,17,1,165,64,0,0,1,1,15,1,138,44,0,0,17,1,221,45,0,0,1,19,3, 
  0,0,0,15,0,0,0,1,0,17,1,138,44,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,138,44,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,138,44,0,0,1,19,26,0,0,0,73,0,0, 
  0,1,0,17,1,138,44,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,138,44,0,0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,138,44,0,0,1,19,26,0,0,0,70,0,0,0,1,0,17,1,138, 
  44,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,138,44,0,0,1,19,26,0,0,0,68,0,0,0,1,0,17,1,138,44,0,0,1,2,21,1,39,0,0,0,170,249,0,0,3,0,0,0,1,0,0,0,2, 
  48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,75,0,0,0,176,250,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80, 
  130,127,35,176,1,128,4,15,1,103,46,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,103,46,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21, 
  0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,103,46,0,0,1,15,1,103,46,0,0,17,1,166,46,0,0,1, 
  1,2,21,1,63,0,0,0,176,250,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,240,1,128,2,240,129,127,35,128,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0,1,4,15,1,33,14,0,0,17,1,227, 
  7,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,19,42,0,0,0,118,0,0,0,2,0,1,19,42,0,0,0,117,0,0,0,1,0,1,19,26,0,0,0,70,0,0,0,1,0,1,19,26,0,0,0,73, 
  0,0,0,1,0,1,21,1,21,3,0,0,170,251,0,0,22,0,0,0,4,0,0,0,16,208,202,132,1,80,3,129,2,80,131,131,3,96,23,128,4,24,22,128,17,104,10,128,22,152,8,128,7,72,149,130,8,120,148,130, 
  9,48,211,130,10,232,209,130,11,160,16,128,12,0,15,128,13,184,13,128,14,112,12,128,15,160,11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119, 
  0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,107,50,0,0,15,1,178,55,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,72,131,0,0, 
  17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,107,50,0,0,15,1,178,55,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,107,50, 
  0,0,17,1,56,50,0,0,1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,102,64,0,0, 
  15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,19,39,0,0,0,110,0,0,0,2,0,1,4,15,1,107,50,0,0,15, 
  1,178,55,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0, 
  0,70,0,0,0,1,0,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,115,0,0,0,1,0,17, 
  1,107,50,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,4,19, 
  9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0, 
  1,0,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0, 
  1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,107,50,0,0,15,1,178,55,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,3,0,0,0,12, 
  0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,41,0, 
  0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,2,21,1,39,0,0,0,119,252,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,6,248,0,128,8,4,17,1,15,6,0,0,1,19,31,0,0,0, 
  83,0,0,0,1,0,1,21,0,116,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,19,176,7,128,26,152,5,128,27,48,133,131,3,16,75,127,20,32,7,129,5,128,10,128,22,144,6,128,23,40,70,129,36,56,4, 
  128,9,240,73,129,10,96,201,125,11,208,200,125,39,168,3,128,13,64,8,128,41,64,3,128,31,200,4,128,43,176,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,107,50,0,0,1,15,1,107,50,0,0,17,1,245, 
  52,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,107,50,0,0,1,19,26,0,0,0,72,0,0,0,1,0,17,1,107,50,0,0,1,15,1,107,50,0,0,17,1,19,52,0,0,1,15,1,107,50,0,0,17, 
  1,165,64,0,0,1,19,41,0,0,0,115,0,0,0,1,0,17,1,107,50,0,0,1,15,1,107,50,0,0,17,1,224,51,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,107,50,0,0,1,19,3,0,0,0, 
  14,0,0,0,1,0,17,1,107,50,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,107,50,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,107,50,0,0,1,19,26,0,0,0,74,0,0,0,1,0, 
  17,1,107,50,0,0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,107,50,0,0,1,19,26,0,0,0,70,0,0,0,1,0,17,1,107,50,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,107,50,0,0, 
  1,19,26,0,0,0,68,0,0,0,1,0,17,1,107,50,0,0,1,2,21,1,39,0,0,0,119,252,0,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19, 
  31,0,0,0,83,0,0,0,1,0,1,21,1,75,0,0,0,92,253,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,106,52,0,0,15,1,8,31,0,0,17,1, 
  19,30,0,0,1,4,15,1,106,52,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128, 
  11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,106,52,0,0,1,15,1,106,52,0,0,17,1,169,52,0,0,1,1,2,21,1,63,0,0,0,92,253,0,0,4,0,0,0,2,0,0,0,34, 
  16,1,128,1,240,1,128,2,240,129,127,35,128,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0,1,4,15,1,33,14,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,176, 
  2,0,0,170,251,0,0,22,0,0,0,4,0,0,0,16,232,201,132,1,80,3,129,2,80,131,131,3,104,20,128,4,80,19,128,17,120,9,128,22,8,8,128,7,176,146,130,8,16,146,130,9,248,208,130,10,224,207,130,11, 
  200,14,128,12,88,13,128,13,64,12,128,14,40,11,128,15,136,10,128,18,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0, 
  0,84,0,0,0,1,0,15,1,166,55,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,166,55,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1, 
  166,55,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,166,55,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,166,55,0,0,15,1,102,64,0, 
  0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,166,55,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,166,55,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1, 
  4,19,39,0,0,0,109,0,0,0,3,0,14,1,4,15,1,166,55,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,166,55,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0, 
  31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,116,0,0, 
  0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0, 
  1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1, 
  4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1,4,15,1,166,55,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,166,55, 
  0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0, 
  1,0,19,26,0,0,0,68,0,0,0,1,0,19,41,0,0,0,116,0,0,0,2,0,1,2,19,41,0,0,0,116,0,0,0,2,0,1,19,41,0,0,0,115,0,0,0,1,0,1,21,1,21,3,0,0,53,254,0,0, 
  22,0,0,0,4,0,0,0,16,208,202,132,1,80,3,128,2,80,195,128,3,96,87,131,4,24,22,128,18,0,10,128,22,152,8,128,7,72,149,130,8,120,148,130,9,48,211,130,10,232,209,130,11,160,16,128,12,0,15,128, 
  13,184,13,128,14,112,12,128,15,160,11,128,19,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0, 
  15,1,7,59,0,0,15,1,78,64,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0, 
  0,1,0,15,1,7,59,0,0,15,1,78,64,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,7,59,0,0,17,1,212,58,0,0,1,4,15,1,7,59,0,0, 
  15,1,78,64,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,19,39, 
  0,0,0,108,0,0,0,2,0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1,90,64,0,0,17,1,34,47,0, 
  0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0,0,0,113,0,0,0, 
  1,0,17,1,7,59,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0,0,0,113,0,0,0,1,0,17,1,7,59,0,0,1,4,19,19,0,0,0,52,0,0,0, 
  1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,40,0,0,0,113,0,0,0,1,0,17,1,7,59,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70, 
  0,0,0,1,0,19,40,0,0,0,113,0,0,0,1,0,17,1,7,59,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0,0,0,113,0,0,0,1,0,17,1,7, 
  59,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0,0,0,113,0,0,0,1,0,17,1,7,59,0,0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1, 
  10,47,0,0,17,1,249,167,0,0,1,4,15,1,7,59,0,0,15,1,78,64,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0, 
  19,40,0,0,0,113,0,0,0,1,0,17,1,7,59,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,40,0,0,0,113,0,0,0,1,0,17,1,7,59,0,0,1,2, 
  21,1,39,0,0,0,2,255,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,6,248,0,128,8,4,17,1,15,6,0,0,1,19,31,0,0,0,83,0,0,0,1,0,1,21,0,116,1,0,0,255,255,255, 
  255,17,0,0,0,4,0,0,0,19,176,7,128,26,152,5,128,27,48,133,131,3,16,75,127,20,32,7,130,5,128,10,128,22,144,6,128,23,40,198,129,40,64,3,128,9,240,9,128,10,96,201,125,11,208,200,125,36,56,4, 
  128,13,64,8,128,39,168,3,128,31,200,4,128,43,176,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,7,59,0,0,1,15,1,7,59,0,0,17,1,145,61,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17, 
  1,7,59,0,0,1,19,26,0,0,0,72,0,0,0,1,0,17,1,7,59,0,0,1,15,1,7,59,0,0,17,1,175,60,0,0,1,15,1,7,59,0,0,17,1,165,64,0,0,1,19,40,0,0,0,113,0,0,0,1, 
  0,17,1,7,59,0,0,1,15,1,7,59,0,0,17,1,124,60,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,7,59,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,7,59,0,0,1,19,3,0, 
  0,0,13,0,0,0,1,0,17,1,7,59,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,7,59,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,7,59,0,0,1,19,26,0,0,0,71,0,0,0, 
  1,0,17,1,7,59,0,0,1,19,26,0,0,0,70,0,0,0,1,0,17,1,7,59,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,7,59,0,0,1,19,26,0,0,0,68,0,0,0,1,0,17,1,7,59, 
  0,0,1,2,21,1,39,0,0,0,2,255,0,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,75,0,0, 
  0,231,255,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,176,1,128,2,176,129,127,35,184,1,128,4,15,1,6,61,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,4,15,1,6,61,0,0,15,1,8, 
  31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0, 
  0,1,0,17,1,6,61,0,0,1,15,1,6,61,0,0,17,1,69,61,0,0,1,1,2,21,1,63,0,0,0,231,255,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,240,1,128,2,240,129,127,35,128,1,128,4, 
  15,1,33,14,0,0,17,1,19,30,0,0,1,4,15,1,33,14,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,176,2,0,0,53,254,0,0,22,0,0,0,4,0,0,0,16, 
  232,201,132,1,80,3,128,2,80,195,128,3,104,84,131,4,80,19,128,18,72,9,128,22,8,8,128,7,176,146,130,8,16,146,130,9,248,208,130,10,224,207,130,11,200,14,128,12,88,13,128,13,64,12,128,14,40,11,128,15, 
  136,10,128,19,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,66,64,0,0,15,1,121, 
  136,0,0,17,1,91,228,0,0,1,4,15,1,66,64,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,66,64,0,0,15,1,229,64,0,0,17,1,165,64,0,0, 
  1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,66,64,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,66,64,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15, 
  1,66,64,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,19,39,0,0,0,107,0,0,0,3,0,14,1,4,15,1,66,64,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15, 
  1,66,64,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,66,64,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1, 
  0,19,40,0,0,0,114,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0,0,0,114,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0, 
  19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,40,0,0,0,114,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0, 
  0,0,114,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,40,0,0,0,114,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0, 
  0,70,0,0,0,1,0,19,40,0,0,0,114,0,0,0,2,0,1,4,15,1,66,64,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,66,64,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4, 
  19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,40,0,0,0,114,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,40,0, 
  0,0,114,0,0,0,2,0,1,2,19,40,0,0,0,114,0,0,0,2,0,1,19,40,0,0,0,113,0,0,0,1,0,1,19,26,0,0,0,75,0,0,0,1,0,1,19,26,0,0,0,68,0,0,0,1,0,1,21,1, 
  39,0,0,0,236,247,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,6,248,0,128,8,4,17,1,15,6,0,0,1,19,31,0,0,0,83,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,40,176,0,128,4,15,1,238,140,0,0,17,1,133,136,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,26,0,0,0,71,0, 
  0,0,1,0,1,21,1,219,0,0,0,192,0,1,0,8,0,0,0,3,0,0,0,16,120,197,128,1,144,1,128,2,144,65,129,24,232,2,128,12,232,5,128,21,216,4,128,22,56,4,128,42,152,1,128,8,4,19,43,0, 
  0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,24,123,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1, 
  0,15,1,24,123,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,24,123,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,24,123,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1, 
  4,15,1,24,80,0,0,17,1,205,65,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,24,123,0,0,1,2,21,1,235,0,0,0,71,1,1,0,8,0,0,0,3, 
  0,0,0,24,216,3,128,1,144,1,128,2,144,65,128,42,136,2,128,12,104,6,128,21,200,5,128,22,40,69,128,46,152,1,128,8,4,19,50,0,0,0,137,0,0,0,1,0,19,51,0,0,0,138,0,0,0,1,0,17, 
  1,185,66,0,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,185,66,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0, 
  19,31,0,0,0,83,0,0,0,1,0,15,1,185,66,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,185,66,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,185,66,0,0,15,1,116, 
  7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,185,66,0,0,1,2,21,0,228,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,24, 
  120,4,128,33,128,3,130,50,88,2,128,11,144,134,128,20,112,5,128,19,0,198,128,31,16,4,128,23,224,196,127,43,240,130,128,49,232,2,128,51,240,1,128,15,1,185,66,0,0,17,1,137,77,0,0,1,19,51,0,0, 
  0,138,0,0,0,1,0,17,1,185,66,0,0,1,1,19,31,0,0,0,84,0,0,0,1,0,17,1,185,66,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,185,66,0,0,1,15,1,185,66,0,0,17,1,50, 
  77,0,0,1,15,1,185,66,0,0,17,1,158,67,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,185,66,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,185,66,0,0,1,19,24,0,0,0,63,0, 
  0,0,1,0,17,1,185,66,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,185,66,0,0,1,2,21,7,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,48,2,128,45,112,1,128,42,48,129,128, 
  43,176,65,127,94,240,1,128,4,17,1,175,75,0,0,1,4,17,1,213,73,0,0,1,4,17,1,82,72,0,0,1,4,17,1,120,70,0,0,1,4,17,1,19,68,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,50,0,0,0,136,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4, 
  128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,221,68,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0, 
  0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,221,68,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,221,68,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15, 
  1,221,68,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,221,68,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8, 
  0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,221,68,0,0,1,19,24,0, 
  0,0,66,0,0,0,1,0,17,1,221,68,0,0,1,15,1,221,68,0,0,17,1,150,69,0,0,1,15,1,221,68,0,0,17,1,201,19,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,221,68,0,0,1,19, 
  24,0,0,0,65,0,0,0,1,0,17,1,221,68,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,221,68,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,221,68,0,0,1,2,21,1,75,0,0,0, 
  21,2,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,237,69,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,237,69,0,0,15,1,8,31,0, 
  0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0, 
  1,0,17,1,237,69,0,0,1,15,1,237,69,0,0,17,1,44,70,0,0,1,1,2,21,1,63,0,0,0,21,2,1,0,4,0,0,0,2,0,0,0,34,128,1,128,1,240,1,128,2,240,129,127,35,16,1,128,4,15, 
  1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88, 
  69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,66,71,0,0,15,1,78,32, 
  0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,66,71,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,66,71,0,0,15,1, 
  128,7,0,0,17,1,63,224,0,0,1,4,15,1,66,71,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,66,71,0,0, 
  1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0, 
  0,1,0,17,1,66,71,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,66,71,0,0,1,15,1,66,71,0,0,17,1,251,71,0,0,1,15,1,66,71,0,0,17,1,176,15,0,0,1,19,31,0,0,0,83, 
  0,0,0,1,0,17,1,66,71,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,66,71,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,66,71,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,66,71,0,0,1,2,21,1,75,0,0,0,21,2,1,0,4,0,0,0,2,0,0,0,34,184,1,128,1,176,1,128,2,176,129,127,35,16,1,128,4,15,1,237,69,0,0,15,1,8,31,0,0,17,1,227,7,0,0, 
  1,8,4,15,1,237,69,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128, 
  2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,28,73,0,0,15,1,78,32,0,0,17,1,91,228, 
  0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,28,73,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,28,73,0,0,15,1,128,7,0,0,17,1, 
  63,224,0,0,1,4,15,1,28,73,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,28,73,0,0,1,2,21,0,184,0, 
  0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,28, 
  73,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,28,73,0,0,1,15,1,28,73,0,0,17,1,150,69,0,0,1,15,1,28,73,0,0,17,1,208,17,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17, 
  1,28,73,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,28,73,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,28,73,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,28,73,0,0,1, 
  2,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0, 
  19,31,0,0,0,84,0,0,0,1,0,15,1,159,74,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,159,74,0,0,15, 
  1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,159,74,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,159,74,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0, 
  0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,159,74,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4, 
  128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,159,74,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,159,74,0,0,1,15,1,159,74,0,0,17,1,88,75,0, 
  0,1,15,1,159,74,0,0,17,1,170,21,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,159,74,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,159,74,0,0,1,19,24,0,0,0,63,0,0,0, 
  1,0,17,1,159,74,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,159,74,0,0,1,2,21,1,75,0,0,0,21,2,1,0,4,0,0,0,2,0,0,0,34,184,1,128,1,16,1,128,2,16,129,127,35,24, 
  1,128,8,4,15,1,237,69,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,237,69,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,1,201,0,0, 
  0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84, 
  0,0,0,1,0,15,1,121,76,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,121,76,0,0,15,1,78,32,0,0,17, 
  1,140,7,0,0,1,4,15,1,121,76,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,121,76,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24, 
  0,0,0,63,0,0,0,1,0,17,1,121,76,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43, 
  144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,121,76,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,121,76,0,0,1,15,1,121,76,0,0,17,1,150,69,0,0,1,15,1,121,76, 
  0,0,17,1,106,11,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,121,76,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,121,76,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,121,76, 
  0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,121,76,0,0,1,2,21,1,75,0,0,0,21,2,1,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,237, 
  69,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,237,69,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,7,48,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,44,208,0,128,41,16,1,128,4,17,1,213,77,0,0,1,4,19,49,0,0,0,135,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  208,0,128,8,2,21,1,3,1,0,0,71,1,1,0,8,0,0,0,3,0,0,0,24,216,3,128,1,144,1,128,2,144,65,128,42,88,2,128,12,248,6,128,21,40,6,128,22,88,69,128,46,152,1,128,8,4,19,50,0, 
  0,0,137,0,0,0,1,0,19,51,0,0,0,139,0,0,0,3,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,217,78,0,0,15,1,229,78,0,0,15,1,78,32,0, 
  0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,217,78,0,0,15,1,229,78,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,217, 
  78,0,0,15,1,229,78,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,217,78,0,0,15,1,229,78,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0, 
  19,24,0,0,0,63,0,0,0,1,0,15,1,217,78,0,0,17,1,229,78,0,0,1,2,19,51,0,0,0,139,0,0,0,3,0,1,21,0,189,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,24,64,3,128,33, 
  72,2,128,50,176,1,128,11,88,133,128,20,56,4,128,19,200,196,128,31,216,2,128,23,168,195,127,43,184,1,128,1,19,31,0,0,0,84,0,0,0,1,0,17,1,229,78,0,0,1,19,24,0,0,0,66,0,0,0,1, 
  0,17,1,229,78,0,0,1,15,1,229,78,0,0,17,1,251,71,0,0,1,15,1,229,78,0,0,17,1,163,79,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,229,78,0,0,1,19,24,0,0,0,65,0,0, 
  0,1,0,17,1,229,78,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,229,78,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,229,78,0,0,1,2,21,7,78,0,0,0,255,255,255,255,5,0,0, 
  0,2,0,0,0,47,48,1,128,45,176,1,128,42,112,129,128,43,240,65,127,94,48,2,128,4,17,1,19,68,0,0,1,4,17,1,175,75,0,0,1,4,17,1,213,73,0,0,1,4,17,1,82,72,0,0,1,4,17,1, 
  120,70,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,50,0,0,0,136,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,123,176,0,128,4,17,1,82,80,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,16,1,0,0,171,2,1,0,10,0,0,0,3,0,0,0, 
  16,184,5,130,1,208,1,128,2,208,193,129,21,24,5,128,12,136,6,128,5,24,136,127,22,120,4,128,7,120,7,128,24,40,3,128,42,216,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0, 
  0,0,1,0,15,1,79,99,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,79,99,0,0,15,1,78,32,0,0,17,1, 
  140,7,0,0,1,4,15,1,79,99,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,79,99,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,15,1,79,99,0,0,15,1,155,90,0,0,15,1, 
  167,90,0,0,17,1,205,65,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,79,99,0,0,1,4,15,1,79,99,0,0,15,1,155,90,0,0,17,1,99,81,0,0, 
  1,4,19,36,0,0,0,102,0,0,0,4,0,1,2,21,1,33,3,0,0,72,3,1,0,23,0,0,0,4,0,0,0,16,200,202,132,1,112,3,128,2,112,131,131,3,192,23,128,4,120,22,128,5,16,22,128,22,248,8, 
  128,7,64,149,130,8,112,148,130,9,40,211,130,10,224,209,130,11,152,16,128,12,248,14,128,13,176,13,128,14,104,12,128,15,152,203,129,18,248,9,128,23,248,7,128,24,48,7,128,32,8,6,128,41,56,5,128,42,184,3, 
  128,47,120,3,128,8,4,17,1,92,90,0,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,184,84,0,0,15,1,80,90,0,0,15,1,121,136,0,0,17,1,91,228,0, 
  0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,184,84,0,0,15,1,80,90,0,0,15,1,229,64,0,0,17,1, 
  165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,184,84,0,0,17,1,133,84,0,0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0, 
  0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1, 
  4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0, 
  0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,154,0,0,0,1,0,17,1,184,84,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0, 
  0,154,0,0,0,1,0,17,1,184,84,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,57,0,0,0,154,0,0,0,1,0,17, 
  1,184,84,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,154,0,0,0,1,0,17,1,184,84,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19, 
  26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,154,0,0,0,1,0,17,1,184,84,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,154,0,0,0, 
  1,0,17,1,184,84,0,0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,184,84,0,0,15,1,80,90,0,0,15,1,90,64,0,0,17,1,6,36,0,0, 
  1,4,19,52,0,0,0,147,0,0,0,2,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,57,0,0,0,154,0,0,0,1,0,17,1,184,84,0,0,1,4,19,3,0,0, 
  0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,57,0,0,0,154,0,0,0,1,0,17,1,184,84,0,0,1,2,21,1,39,0,0,0,32,4,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1, 
  240,0,128,6,248,0,128,8,4,17,1,15,6,0,0,1,19,31,0,0,0,83,0,0,0,1,0,1,21,0,121,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,19,216,7,128,26,192,5,128,27,88,5,131,3,56, 
  75,127,20,72,7,129,5,168,10,128,22,184,6,128,23,80,70,129,36,96,4,130,9,24,10,130,10,136,201,125,11,248,200,125,39,208,3,128,13,104,8,128,43,64,3,128,31,240,4,128,52,56,3,128,57,208,2,128,15,1, 
  184,84,0,0,17,1,71,87,0,0,1,1,19,31,0,0,0,84,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0,0,0,72,0,0,0,1,0,17, 
  1,184,84,0,0,1,15,1,184,84,0,0,17,1,101,86,0,0,1,15,1,184,84,0,0,17,1,165,64,0,0,1,19,57,0,0,0,154,0,0,0,1,0,17,1,184,84,0,0,1,15,1,184,84,0,0,17,1,50,86, 
  0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,184,84,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,184,84,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0, 
  0,0,73,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0,0,0,70,0,0,0, 
  1,0,17,1,184,84,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,184,84,0,0,1,19,26,0,0,0,68,0,0,0,1,0,17,1,184,84,0,0,1,2,21,1,39,0,0,0,32,4,1,0,3,0,0,0, 
  1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,75,0,0,0,16,5,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1, 
  80,2,128,2,80,130,127,35,176,1,128,4,15,1,188,86,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,188,86,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0, 
  0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,188,86,0,0,1,15,1,188,86,0,0,17,1, 
  251,86,0,0,1,1,2,21,1,63,0,0,0,16,5,1,0,4,0,0,0,2,0,0,0,34,128,1,128,1,240,1,128,2,240,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33,14, 
  0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,188,2,0,0,72,3,1,0,23,0,0,0,4,0,0,0,16,216,201,132,1,112,3,128,2,112,131,131,3,200,20,128,4,176,19, 
  128,5,64,19,128,22,104,8,128,7,160,146,130,8,0,146,130,9,232,208,130,10,208,207,130,11,184,14,128,12,72,13,128,13,48,12,128,14,24,11,128,15,120,202,129,18,56,9,128,23,152,7,128,24,160,6,128,32,168,5, 
  128,41,8,5,128,42,184,3,128,47,120,3,128,8,4,17,1,16,90,0,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,4,90,0,0,15,1,121,136,0,0,17,1,91, 
  228,0,0,1,4,15,1,4,90,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,4,90,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0, 
  0,57,0,0,0,1,0,15,1,4,90,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,4,90,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,4,90,0,0,15, 
  1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,4,90,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,4,90,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15, 
  1,4,90,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,155,0,0,0,2,0,1,4,19,9,0,0,0,30, 
  0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,155,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0, 
  1,0,19,57,0,0,0,155,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,155,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1, 
  0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,155,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,57,0,0,0,155,0,0,0,2,0,1,4, 
  15,1,4,90,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,4,90,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,52,0,0,0,146,0,0,0,3,0,14,1,4,19,3,0,0,0,12, 
  0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,57,0,0,0,155,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,57,0,0,0,155,0,0,0, 
  2,0,1,2,19,57,0,0,0,155,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,52,0,0,0,144,0,0,0,4,0,14,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,57,0,0,0,154,0,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,52,0,0,0, 
  145,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,38,0,0,0,105,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,123,176,0,128,4,17,1,225,90,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,21,3,0,0,201,246,0,0,22,0,0,0,4, 
  0,0,0,16,104,202,132,1,80,3,128,2,80,131,131,3,96,23,128,4,24,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14, 
  8,12,128,15,56,11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,42,94,0, 
  0,15,1,67,99,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1, 
  42,94,0,0,15,1,67,99,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,42,94,0,0,17,1,247,93,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0, 
  0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,42,94,0,0,15, 
  1,67,99,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,22, 
  47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,9,0,0,0,30,0,0, 
  0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0, 
  68,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1, 
  42,94,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26, 
  0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,42,94,0,0, 
  15,1,67,99,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,52,0,0,0,143,0,0,0,3,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0, 
  151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,2,21,1,39,0,0, 
  0,236,247,0,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,0,121,1,0,0,255,255,255,255,18,0,0,0, 
  4,0,0,0,19,216,7,128,26,192,5,128,27,88,5,131,3,56,75,127,20,72,7,129,5,168,10,128,22,184,6,128,23,80,70,129,36,96,4,130,9,24,10,128,10,136,201,125,11,248,200,125,39,208,67,129,13,104,8,128, 
  43,64,3,128,31,240,4,128,52,56,3,128,55,208,2,128,15,1,42,94,0,0,17,1,134,96,0,0,1,1,19,31,0,0,0,84,0,0,0,1,0,17,1,42,94,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17, 
  1,42,94,0,0,1,19,26,0,0,0,72,0,0,0,1,0,17,1,42,94,0,0,1,15,1,42,94,0,0,17,1,164,95,0,0,1,15,1,42,94,0,0,17,1,165,64,0,0,1,19,55,0,0,0,151,0,0,0,1, 
  0,17,1,42,94,0,0,1,15,1,42,94,0,0,17,1,28,39,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,42,94,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,42,94,0,0,1,19,3,0, 
  0,0,13,0,0,0,1,0,17,1,42,94,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,42,94,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,42,94,0,0,1,19,26,0,0,0,71,0,0,0, 
  1,0,17,1,42,94,0,0,1,19,26,0,0,0,70,0,0,0,1,0,17,1,42,94,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,42,94,0,0,1,19,26,0,0,0,68,0,0,0,1,0,17,1,42,94, 
  0,0,1,2,21,1,75,0,0,0,209,248,0,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,251,95,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4,15, 
  1,251,95,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0, 
  128,19,12,0,0,0,35,0,0,0,1,0,17,1,251,95,0,0,1,15,1,251,95,0,0,17,1,58,96,0,0,1,1,2,21,1,63,0,0,0,209,248,0,0,4,0,0,0,2,0,0,0,34,128,1,128,1,240,1,128, 
  2,240,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,176,2,0,0,201,246,0,0, 
  22,0,0,0,4,0,0,0,16,120,201,132,1,80,3,128,2,80,131,131,3,104,20,128,4,80,19,128,5,224,18,128,22,8,8,128,7,64,146,130,8,160,145,130,9,136,208,130,10,112,207,130,11,88,14,128,12,232,12,128, 
  13,208,11,128,14,184,10,128,15,24,10,128,18,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0, 
  15,1,55,99,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,55,99,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,55,99,0,0,15,1,229, 
  64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,55,99,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,55,99,0,0,15,1,102,64,0,0,15,1,246,226,0,0, 
  17,1,230,224,0,0,1,4,15,1,55,99,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,55,99,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,55,99,0,0, 
  15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,55,99,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0, 
  0,152,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0, 
  13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0, 
  0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0, 
  1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,15,1,55,99,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,55,99,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,52,0,0,0, 
  141,0,0,0,4,0,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0, 
  68,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,2,19,55,0,0,0,152,0,0,0,2,0,1,19,55,0,0,0,151,0,0,0,1,0,1,21,0,245,0,0,0,255,255,255,255,12,0,0,0,3,0,0, 
  0,24,0,5,128,33,8,68,130,19,136,198,129,11,24,199,127,20,248,5,129,31,152,4,128,38,152,3,128,23,104,133,127,36,0,196,128,43,8,3,128,49,160,2,128,52,16,2,128,19,38,0,0,0,105,0,0,0,1,0, 
  17,1,79,99,0,0,1,15,1,79,99,0,0,17,1,200,119,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,79,99,0,0,1,15,1,79,99,0,0,17,1,201,117,0,0,1,1,19,24,0,0,0,66,0,0, 
  0,1,0,17,1,79,99,0,0,1,15,1,79,99,0,0,17,1,114,117,0,0,1,15,1,79,99,0,0,17,1,69,100,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,79,99,0,0,1,19,24,0,0,0,65, 
  0,0,0,1,0,17,1,79,99,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,79,99,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,79,99,0,0,1,2,21,7,90,0,0,0,255,255,255,255,6, 
  0,0,0,2,0,0,0,47,144,65,129,45,208,1,128,42,80,130,128,43,16,66,127,94,80,1,128,123,144,2,128,4,17,1,152,115,0,0,1,4,17,1,21,114,0,0,1,4,17,1,59,112,0,0,1,4,17,1,214,109, 
  0,0,1,4,17,1,83,108,0,0,1,4,17,1,187,100,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,21,3,0,0,201,246,0,0,22,0,0, 
  0,4,0,0,0,16,104,202,132,1,80,3,128,2,80,131,131,3,96,23,128,4,24,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13, 
  128,14,8,12,128,15,56,11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,209, 
  103,0,0,15,1,67,99,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,209,103,0,0,15,1,67,99,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0, 
  15,1,209,103,0,0,15,1,67,99,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,209,103,0,0,17,1,28,39,0,0,1,4,15,1,209,103,0,0,15,1,67, 
  99,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,209,103,0,0,15,1,67,99,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,209,103,0, 
  0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,209,103,0,0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,209,103,0,0,15,1,67,99,0,0,15, 
  1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,4,19,9,0,0,0,30, 
  0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0, 
  0,0,68,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0, 
  17,1,209,103,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0, 
  19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,4,15,1,209,103,0,0,15,1,67,99,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,209,103, 
  0,0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,52,0,0,0,142,0,0,0,3,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0, 
  0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,2,21,0,121, 
  1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,19,216,7,128,26,192,5,128,27,88,5,131,3,56,75,127,20,72,7,129,5,168,10,128,22,184,6,128,23,80,70,129,36,96,4,130,9,24,10,128,10,136,201,125,11, 
  248,200,125,39,208,67,129,13,104,8,128,43,64,3,128,31,240,4,128,52,56,3,128,55,208,2,128,15,1,209,103,0,0,17,1,162,105,0,0,1,1,19,31,0,0,0,84,0,0,0,1,0,17,1,209,103,0,0,1,19, 
  26,0,0,0,75,0,0,0,1,0,17,1,209,103,0,0,1,19,26,0,0,0,72,0,0,0,1,0,17,1,209,103,0,0,1,15,1,209,103,0,0,17,1,75,105,0,0,1,15,1,209,103,0,0,17,1,165,64,0,0, 
  1,19,55,0,0,0,151,0,0,0,1,0,17,1,209,103,0,0,1,15,1,209,103,0,0,17,1,28,39,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,209,103,0,0,1,19,3,0,0,0,14,0,0,0,1, 
  0,17,1,209,103,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,209,103,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,209,103,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,209,103,0, 
  0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,209,103,0,0,1,19,26,0,0,0,70,0,0,0,1,0,17,1,209,103,0,0,1,19,26,0,0,0,69,0,0,0,1,0,17,1,209,103,0,0,1,19,26,0,0, 
  0,68,0,0,0,1,0,17,1,209,103,0,0,1,2,21,1,75,0,0,0,209,248,0,0,4,0,0,0,2,0,0,0,34,16,1,128,1,176,1,128,2,176,129,127,35,184,1,128,4,15,1,251,95,0,0,15,1,8,31, 
  0,0,17,1,19,30,0,0,1,8,4,15,1,251,95,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,1,176,2,0,0,201,246,0,0,22,0,0,0,4,0,0,0, 
  16,120,201,132,1,80,3,128,2,80,131,131,3,104,20,128,4,80,19,128,5,224,18,128,22,8,8,128,7,64,146,130,8,160,145,130,9,136,208,130,10,112,207,130,11,88,14,128,12,232,12,128,13,208,11,128,14,184,10,128, 
  15,24,10,128,18,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,55,99,0,0,15,1, 
  121,136,0,0,17,1,91,228,0,0,1,4,15,1,55,99,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,55,99,0,0,15,1,229,64,0,0,17,1,165,64,0, 
  0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,55,99,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,55,99,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4, 
  15,1,55,99,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,55,99,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,55,99,0,0,15,1,90,64,0,0,17,1, 
  34,47,0,0,1,4,15,1,55,99,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1, 
  4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26, 
  0,0,0,68,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,9,0, 
  0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,152, 
  0,0,0,2,0,1,4,15,1,55,99,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,55,99,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,52,0,0,0,140,0,0,0,4,0,14,1, 
  4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,152,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55, 
  0,0,0,152,0,0,0,2,0,1,2,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19, 
  43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,29,109,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0, 
  0,1,0,15,1,29,109,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,29,109,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,29,109,0,0,15,1,116,7,0,0,17,1,217,6,0, 
  0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,29,109,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19, 
  160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,29,109,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,29,109,0,0,1,15, 
  1,29,109,0,0,17,1,114,117,0,0,1,15,1,29,109,0,0,17,1,106,11,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,29,109,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,29,109,0,0, 
  1,19,24,0,0,0,63,0,0,0,1,0,17,1,29,109,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,29,109,0,0,1,2,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129, 
  1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,160,110,0,0,15,1,78,32,0,0, 
  17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,160,110,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,160,110,0,0,15,1,128,7, 
  0,0,17,1,63,224,0,0,1,4,15,1,160,110,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,160,110,0,0,1,2, 
  21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1, 
  0,17,1,160,110,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,160,110,0,0,1,15,1,160,110,0,0,17,1,89,111,0,0,1,15,1,160,110,0,0,17,1,208,17,0,0,1,19,31,0,0,0,83,0,0, 
  0,1,0,17,1,160,110,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,160,110,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,160,110,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,160, 
  110,0,0,1,2,21,1,75,0,0,0,244,5,1,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4, 
  15,1,176,111,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240, 
  0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,176,111,0,0,1,15,1,176,111,0,0,17,1,239,111,0,0,1,1,2,21,1,63,0,0,0,244,5,1,0,4,0,0,0,2,0,0,0,34,128,1,128,1,240,1, 
  128,2,240,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,201,0,0,0,201,239,0, 
  0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1, 
  0,15,1,5,113,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,5,113,0,0,15,1,78,32,0,0,17,1,140,7,0, 
  0,1,4,15,1,5,113,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,5,113,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63, 
  0,0,0,1,0,17,1,5,113,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23, 
  128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,5,113,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,5,113,0,0,1,15,1,5,113,0,0,17,1,190,113,0,0,1,15,1,5,113,0,0,17,1, 
  170,21,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,5,113,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,5,113,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,5,113,0,0,1,19, 
  24,0,0,0,64,0,0,0,1,0,17,1,5,113,0,0,1,2,21,1,75,0,0,0,244,5,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,176,1,128,2,176,129,127,35,184,1,128,4,15,1,176,111,0,0,15, 
  1,8,31,0,0,17,1,19,30,0,0,1,8,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2, 
  0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,223,114,0, 
  0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,223,114,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,223, 
  114,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,223,114,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17, 
  1,223,114,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128,19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0, 
  0,0,84,0,0,0,1,0,17,1,223,114,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,223,114,0,0,1,15,1,223,114,0,0,17,1,89,111,0,0,1,15,1,223,114,0,0,17,1,201,19,0,0,1,19, 
  31,0,0,0,83,0,0,0,1,0,17,1,223,114,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,223,114,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,223,114,0,0,1,19,24,0,0,0,64,0, 
  0,0,1,0,17,1,223,114,0,0,1,2,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4, 
  19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,98,116,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0, 
  0,0,1,0,15,1,98,116,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,98,116,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,98,116,0,0,15,1,116,7,0,0,17,1,217,6, 
  0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,98,116,0,0,1,2,21,0,184,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,24,24,3,128,33,32,2,128, 
  19,160,4,129,11,48,197,127,20,16,4,128,31,176,2,128,43,144,1,128,23,128,131,127,19,31,0,0,0,84,0,0,0,1,0,17,1,98,116,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,98,116,0,0,1, 
  15,1,98,116,0,0,17,1,27,117,0,0,1,15,1,98,116,0,0,17,1,176,15,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,98,116,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,98,116,0, 
  0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,98,116,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,98,116,0,0,1,2,21,1,75,0,0,0,244,5,1,0,4,0,0,0,2,0,0,0,34,184,1, 
  128,1,176,1,128,2,176,129,127,35,16,1,128,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,19,11,0,0,0,34, 
  0,0,0,1,0,1,21,1,75,0,0,0,244,5,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1, 
  4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,1,35,1,0,0,171,2,1,0,10,0,0,0,3,0,0,0,16,120,6,130,1,208,1,128,2, 
  208,193,129,21,168,5,128,12,24,7,128,5,168,136,127,22,216,4,128,7,56,8,128,24,88,3,128,42,216,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,237,118,0, 
  0,15,1,249,118,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,237,118,0,0,15,1,249,118,0,0,15,1,78,32,0, 
  0,17,1,140,7,0,0,1,4,15,1,237,118,0,0,15,1,249,118,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,237,118,0,0,15,1,249,118,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1, 
  4,15,1,237,118,0,0,15,1,167,90,0,0,17,1,205,65,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,15,1,237,118,0,0,17,1,249,118,0,0,1,4,15,1,237, 
  118,0,0,17,1,99,81,0,0,1,4,19,36,0,0,0,100,0,0,0,5,0,14,1,2,19,38,0,0,0,106,0,0,0,2,0,1,21,0,206,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,24,200,3,128,33, 
  208,194,129,19,80,5,129,11,224,197,127,20,192,68,129,31,96,3,128,43,64,2,128,23,48,132,127,49,216,1,128,52,208,1,128,1,15,1,249,118,0,0,17,1,167,90,0,0,1,19,31,0,0,0,84,0,0,0,1,0, 
  17,1,249,118,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,249,118,0,0,1,15,1,249,118,0,0,17,1,89,111,0,0,1,15,1,249,118,0,0,17,1,69,100,0,0,1,19,31,0,0,0,83,0,0,0, 
  1,0,17,1,249,118,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,249,118,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,249,118,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,249,118, 
  0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,17,1,2,120,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,2,21,1,21,3,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,104,202,132,1,80,3,128,2,80,131,131,3,96,23,128,4,24,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130, 
  10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14,8,12,128,15,56,11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1, 
  0,19,31,0,0,0,84,0,0,0,1,0,15,1,42,94,0,0,15,1,67,99,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,72,131,0,0,17,1,241,64, 
  0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,42,94,0,0,15,1,67,99,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,42,94,0,0,17,1, 
  28,39,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,102,64,0,0,15,1,218,224, 
  0,0,17,1,63,224,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,34,47,0,0, 
  1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1, 
  0,17,1,42,94,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,19,0,0,0,52,0,0,0,1, 
  0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0, 
  0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94, 
  0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,10, 
  47,0,0,17,1,249,167,0,0,1,4,15,1,42,94,0,0,15,1,67,99,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,52,0,0,0,143,0,0,0,3,0,1,4,19,3,0,0,0,12,0,0,0,1, 
  0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,151,0,0,0,1,0,17,1,42,94,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,55,0,0,0,151,0, 
  0,0,1,0,17,1,42,94,0,0,1,2,21,0,206,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,24,200,3,128,33,208,2,130,19,80,133,129,11,224,197,127,20,192,132,128,31,96,3,128,36,200,2,128,23,48, 
  132,127,43,56,2,128,49,208,1,128,15,1,24,123,0,0,17,1,225,127,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,24,123,0,0,1,1,19,24,0,0,0,66,0,0,0,1,0,17,1,24,123,0,0,1, 
  15,1,24,123,0,0,17,1,138,127,0,0,1,15,1,24,123,0,0,17,1,231,123,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,24,123,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,24,123,0, 
  0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,24,123,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,24,123,0,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,47,144,65, 
  129,45,208,1,128,42,80,130,128,43,16,66,127,94,80,1,128,123,144,2,128,4,17,1,152,115,0,0,1,4,17,1,21,114,0,0,1,4,17,1,59,112,0,0,1,4,17,1,214,109,0,0,1,4,17,1,83,108,0,0, 
  1,4,17,1,93,124,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,17,1,0,0,171,2,1,0,10,0,0,0,3,0,0,0,16,184,5,130,1, 
  208,1,128,2,208,193,129,21,24,5,128,12,136,6,128,5,24,136,127,22,120,4,128,7,120,7,128,24,40,3,128,42,216,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15, 
  1,111,125,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,111,125,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1, 
  4,15,1,111,125,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,111,125,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,15,1,111,125,0,0,15,1,155,90,0,0,15,1,167,90,0,0,17, 
  1,205,65,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,111,125,0,0,1,4,15,1,111,125,0,0,15,1,155,90,0,0,17,1,99,81,0,0,1,4,19,36,0, 
  0,0,101,0,0,0,4,0,14,1,2,21,0,245,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,24,0,5,128,33,8,68,130,19,136,198,129,11,24,199,127,20,248,5,129,31,152,4,128,38,152,3,128,23,104,133, 
  127,36,0,196,128,43,8,3,128,49,160,2,128,52,16,2,128,19,38,0,0,0,105,0,0,0,1,0,17,1,111,125,0,0,1,15,1,111,125,0,0,17,1,200,119,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17, 
  1,111,125,0,0,1,15,1,111,125,0,0,17,1,101,126,0,0,1,1,19,24,0,0,0,66,0,0,0,1,0,17,1,111,125,0,0,1,15,1,111,125,0,0,17,1,89,111,0,0,1,15,1,111,125,0,0,17,1,69, 
  100,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,111,125,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,111,125,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,111,125,0,0,1,19,24, 
  0,0,0,64,0,0,0,1,0,17,1,111,125,0,0,1,2,21,1,36,1,0,0,171,2,1,0,10,0,0,0,3,0,0,0,16,120,6,130,1,208,1,128,2,208,193,129,21,168,5,128,12,24,7,128,5,168,136,127,22, 
  216,4,128,7,56,8,128,24,88,3,128,42,216,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,237,118,0,0,15,1,249,118,0,0,15,1,78,32,0,0,17,1,91, 
  228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,237,118,0,0,15,1,249,118,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,237,118,0,0,15, 
  1,249,118,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,237,118,0,0,15,1,249,118,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,15,1,237,118,0,0,15,1,167,90,0,0,17,1,205, 
  65,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,15,1,237,118,0,0,17,1,249,118,0,0,1,4,15,1,237,118,0,0,17,1,99,81,0,0,1,4,19,36,0,0,0, 
  99,0,0,0,5,0,14,14,1,2,21,1,75,0,0,0,244,5,1,0,4,0,0,0,2,0,0,0,34,24,1,128,1,16,1,128,2,16,129,127,35,184,1,128,8,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1, 
  19,30,0,0,1,4,15,1,176,111,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4, 
  17,1,27,128,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,17,1,0,0,171,2,1,0,10,0,0,0,3,0,0,0,16,184,5,130,1,208,1, 
  128,2,208,193,129,21,24,5,128,12,136,6,128,5,24,136,127,22,120,4,128,7,120,7,128,24,40,3,128,42,216,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,45, 
  129,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,45,129,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15, 
  1,45,129,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,45,129,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,15,1,45,129,0,0,15,1,155,90,0,0,15,1,167,90,0,0,17,1,205, 
  65,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,45,129,0,0,1,4,15,1,45,129,0,0,15,1,155,90,0,0,17,1,99,81,0,0,1,4,19,36,0,0,0, 
  102,0,0,0,4,0,14,1,2,21,0,245,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,24,0,5,128,33,8,68,130,19,136,198,129,11,24,199,127,20,248,5,129,31,152,4,128,38,152,3,128,23,104,133,127,36, 
  0,196,128,43,8,3,128,49,160,2,128,52,16,2,128,19,38,0,0,0,105,0,0,0,1,0,17,1,45,129,0,0,1,15,1,45,129,0,0,17,1,200,119,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,45, 
  129,0,0,1,15,1,45,129,0,0,17,1,35,130,0,0,1,1,19,24,0,0,0,66,0,0,0,1,0,17,1,45,129,0,0,1,15,1,45,129,0,0,17,1,114,117,0,0,1,15,1,45,129,0,0,17,1,69,100,0, 
  0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,45,129,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,45,129,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,45,129,0,0,1,19,24,0,0, 
  0,64,0,0,0,1,0,17,1,45,129,0,0,1,2,21,1,36,1,0,0,171,2,1,0,10,0,0,0,3,0,0,0,16,120,6,130,1,208,1,128,2,208,193,129,21,168,5,128,12,24,7,128,5,168,136,127,22,216,4, 
  128,7,56,8,128,24,88,3,128,42,216,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,237,118,0,0,15,1,249,118,0,0,15,1,78,32,0,0,17,1,91,228,0, 
  0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,237,118,0,0,15,1,249,118,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,237,118,0,0,15,1,249, 
  118,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,237,118,0,0,15,1,249,118,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,15,1,237,118,0,0,15,1,167,90,0,0,17,1,205,65,0, 
  0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,15,1,237,118,0,0,17,1,249,118,0,0,1,4,15,1,237,118,0,0,17,1,99,81,0,0,1,4,19,36,0,0,0,100,0, 
  0,0,5,0,14,14,1,2,19,26,0,0,0,72,0,0,0,1,0,1,21,0,121,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,19,104,7,128,22,72,6,128,26,80,5,128,3,56,75,127,20,216,134,130,5,168, 
  10,128,6,160,202,126,23,224,69,130,8,56,10,128,9,168,9,128,10,24,9,126,11,136,72,128,27,232,68,129,13,248,7,128,36,240,3,128,31,128,4,128,39,96,3,128,43,208,2,128,19,31,0,0,0,84,0,0,0,1, 
  0,17,1,84,131,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,84,131,0,0,1,19,26,0,0,0,72,0,0,0,1,0,17,1,84,131,0,0,1,15,1,84,131,0,0,17,1,139,135,0,0,1,15,1,84, 
  131,0,0,17,1,165,64,0,0,1,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,15,1,84,131,0,0,17,1,28,39,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,84,131,0,0,1,19, 
  3,0,0,0,14,0,0,0,1,0,17,1,84,131,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,84,131,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,84,131,0,0,1,19,26,0,0,0,74,0, 
  0,0,1,0,17,1,84,131,0,0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,84,131,0,0,1,19,26,0,0,0,70,0,0,0,1,0,17,1,84,131,0,0,1,15,1,84,131,0,0,17,1,206,132,0,0,1, 
  1,19,26,0,0,0,69,0,0,0,1,0,17,1,84,131,0,0,1,19,26,0,0,0,68,0,0,0,1,0,17,1,84,131,0,0,1,2,21,1,176,2,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,120,201,132, 
  1,80,3,128,2,80,131,131,3,104,20,128,4,80,19,128,5,224,18,128,22,8,8,128,7,64,146,130,8,160,145,130,9,136,208,130,10,112,207,130,11,88,14,128,12,232,12,128,13,208,11,128,14,184,10,128,15,24,10,128, 
  18,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,127,135,0,0,15,1,121,136,0,0, 
  17,1,91,228,0,0,1,4,15,1,127,135,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,127,135,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19, 
  23,0,0,0,57,0,0,0,1,0,15,1,127,135,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1,4,15,1,127,135,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,127,135, 
  0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,127,135,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,127,135,0,0,15,1,90,64,0,0,17,1,34,47,0,0, 
  1,4,15,1,127,135,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0, 
  0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68, 
  0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0,0,0,27,0, 
  0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2, 
  0,1,4,15,1,127,135,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,127,135,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,6,0,0,0,19,0,0,0,4,0,14,1,4,19,3,0, 
  0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,25, 
  0,0,0,2,0,1,2,19,8,0,0,0,25,0,0,0,2,0,1,21,1,75,0,0,0,209,248,0,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,226,135,0,0, 
  15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,226,135,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,226,135,0,0,1,15,1,226,135,0,0,17,1,33,136,0,0,1,1,2,21,1,63,0,0,0,209,248,0,0,4, 
  0,0,0,2,0,0,0,34,16,1,128,1,128,1,128,2,128,129,127,35,136,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,4,15,1,33,14,0,0,17,1,227,7,0,0,1,19,11,0,0,0,33,0,0, 
  0,2,0,14,1,19,8,0,0,0,24,0,0,0,1,0,1,19,26,0,0,0,74,0,0,0,1,0,1,21,1,218,0,0,0,127,6,1,0,8,0,0,0,3,0,0,0,24,232,2,128,1,144,129,128,2,144,65,129,17, 
  120,5,128,12,224,5,128,21,216,4,128,22,56,4,128,42,152,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,96,137,0,0,15,1,78,32,0,0,17,1,91,228,0, 
  0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,96,137,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,96,137,0,0,15,1,128,7,0,0,17,1,63, 
  224,0,0,1,4,15,1,96,137,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,28,0,0,0,78,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0, 
  17,1,96,137,0,0,1,2,21,0,206,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,24,200,3,128,33,96,2,128,19,80,197,129,11,224,197,127,20,192,132,128,29,88,3,128,28,192,3,128,23,48,68,128,31,240, 
  2,128,43,208,1,128,19,31,0,0,0,84,0,0,0,1,0,17,1,96,137,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,96,137,0,0,1,15,1,96,137,0,0,17,1,150,69,0,0,1,15,1,96,137,0, 
  0,17,1,164,138,0,0,1,1,15,1,96,137,0,0,17,1,47,138,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,96,137,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,96,137,0,0,1,19,24, 
  0,0,0,63,0,0,0,1,0,17,1,96,137,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,96,137,0,0,1,2,21,7,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,176,1,128,45,240,1, 
  128,42,112,129,128,43,48,65,127,94,48,2,128,4,17,1,82,72,0,0,1,4,17,1,175,75,0,0,1,4,17,1,19,68,0,0,1,4,17,1,213,73,0,0,1,4,17,1,120,70,0,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,29,0,0,0,79,0,0,0,1,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,208,0,128,41,16,1,128,4,17, 
  1,240,138,0,0,1,4,19,28,0,0,0,77,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,201,0,0,0,201,239,0,0,7,0, 
  0,0,2,0,0,0,12,88,69,129,1,112,129,128,2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1, 
  186,139,0,0,15,1,78,32,0,0,17,1,91,228,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,186,139,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4, 
  15,1,186,139,0,0,15,1,128,7,0,0,17,1,63,224,0,0,1,4,15,1,186,139,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0, 
  1,0,17,1,186,139,0,0,1,2,21,0,189,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,24,64,3,128,33,64,2,128,19,200,132,129,11,88,197,127,20,56,4,128,29,56,3,128,31,208,2,128,23,168,195,127, 
  43,176,1,128,19,31,0,0,0,84,0,0,0,1,0,17,1,186,139,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,186,139,0,0,1,15,1,186,139,0,0,17,1,251,71,0,0,1,1,15,1,186,139,0,0, 
  17,1,120,140,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,186,139,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,186,139,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,186,139,0,0, 
  1,19,24,0,0,0,64,0,0,0,1,0,17,1,186,139,0,0,1,2,21,7,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,112,1,128,45,240,1,128,42,48,130,128,43,48,65,127,94,176,1,128,4,17, 
  1,82,72,0,0,1,4,17,1,19,68,0,0,1,4,17,1,120,70,0,0,1,4,17,1,213,73,0,0,1,4,17,1,175,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,29,0,0,0,80,0,0,0,3,0,14,1,19,10,0,0,0,32,0,0,0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,52,141,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,167,0,0,0,6,7,1,0,7,0,0,0,2,0,0,0,36,120,132,129,1,112,193,128,2,112,193,128,39,56, 
  2,128,37,184,3,128,38,248,2,128,40,120,1,128,8,4,19,35,0,0,0,96,0,0,0,1,0,19,25,0,0,0,67,0,0,0,3,0,1,4,19,35,0,0,0,97,0,0,0,1,0,19,25,0,0,0,67,0,0,0, 
  3,0,1,4,19,35,0,0,0,98,0,0,0,1,0,19,25,0,0,0,67,0,0,0,3,0,1,4,19,35,0,0,0,95,0,0,0,1,0,19,25,0,0,0,67,0,0,0,3,0,1,4,19,35,0,0,0,94,0,0, 
  0,1,0,19,25,0,0,0,67,0,0,0,3,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,0,66,129,25,32,3,128,10,248,1,128,43,144,2,128,28,176,3,128,31,144,1,128,6,248, 
  1,128,7,16,68,126,15,1,220,141,0,0,17,1,250,140,0,0,1,1,19,31,0,0,0,83,0,0,0,1,0,17,1,220,141,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,220,141,0,0,1,19,7,0,0, 
  0,22,0,0,0,1,0,17,1,220,141,0,0,1,19,10,0,0,0,32,0,0,0,2,0,1,15,1,220,141,0,0,17,1,108,142,0,0,1,2,21,1,119,0,0,0,176,8,1,0,5,0,0,0,2,0,0,0,24,88, 
  2,128,1,48,1,128,2,48,129,128,7,120,3,128,42,56,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,39,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0, 
  0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,39,150,0,0,17,1,250,140,0,0,1,4,17,1,228,142,0,0,1,2,21,1,22,3,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,104, 
  202,132,1,80,3,128,2,80,131,131,3,104,23,128,4,32,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14,8,12,128,15,56, 
  11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,251,145,0,0,15,1,109,136, 
  0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,251,145,0,0,15, 
  1,109,136,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,251,145,0,0,17,1,28,39,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,102,64, 
  0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0, 
  15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,22,47,0,0,17,1, 
  50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26, 
  0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1, 
  0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1, 
  4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0, 
  0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0, 
  0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,6,0,0,0,20,0,0,0,4,0,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0, 
  1,0,17,1,251,145,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,2,21,0,121,1,0,0,255,255,255, 
  255,18,0,0,0,4,0,0,0,19,104,7,128,22,72,6,128,26,80,5,128,3,56,75,127,20,216,134,130,5,168,10,128,6,160,202,126,23,224,69,130,8,56,10,128,9,168,9,128,10,24,9,126,11,136,72,128,27,232,68, 
  129,13,248,7,128,36,240,3,128,31,128,4,128,39,96,3,128,43,208,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,251,145,0,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,251,145,0,0,1,19,26,0, 
  0,0,72,0,0,0,1,0,17,1,251,145,0,0,1,15,1,251,145,0,0,17,1,139,135,0,0,1,15,1,251,145,0,0,17,1,165,64,0,0,1,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,15, 
  1,251,145,0,0,17,1,114,64,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,251,145,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,251,145,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17, 
  1,251,145,0,0,1,19,26,0,0,0,73,0,0,0,1,0,17,1,251,145,0,0,1,19,26,0,0,0,74,0,0,0,1,0,17,1,251,145,0,0,1,19,26,0,0,0,71,0,0,0,1,0,17,1,251,145,0,0,1, 
  19,26,0,0,0,70,0,0,0,1,0,17,1,251,145,0,0,1,15,1,251,145,0,0,17,1,117,147,0,0,1,1,19,26,0,0,0,69,0,0,0,1,0,17,1,251,145,0,0,1,19,26,0,0,0,68,0,0,0,1, 
  0,17,1,251,145,0,0,1,2,21,1,177,2,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,120,201,132,1,80,3,128,2,80,131,131,3,112,20,128,4,88,19,128,5,224,18,128,22,8,8,128,7,64,146,130,8, 
  160,145,130,9,136,208,130,10,112,207,130,11,88,14,128,12,232,12,128,13,208,11,128,14,184,10,128,15,24,10,128,18,216,8,128,23,56,7,128,24,64,6,128,32,72,5,128,41,168,4,128,42,88,3,128,8,4,19,43,0, 
  0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,127,135,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,127,135,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19, 
  27,0,0,0,76,0,0,0,1,0,15,1,127,135,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,127,135,0,0,15,1,138,44,0,0,17,1,87,44,0,0,1, 
  4,15,1,127,135,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,127,135,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,127,135,0,0,15, 
  1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,127,135,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,127,135,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0, 
  0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2, 
  0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0, 
  19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,19, 
  9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,4,15,1,127,135,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,127,135,0,0, 
  15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,6,0,0,0,18,0,0,0,5,0,14,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,25,0,0,0, 
  2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,25,0,0,0,2,0,1,2,19,7,0,0,0,23,0,0,0,2,0,1,19,7,0,0,0,22,0,0,0, 
  1,0,1,21,0,116,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,18,40,6,128,19,152,5,128,2,16,139,127,3,128,138,127,20,8,5,128,5,240,9,128,6,96,137,128,23,16,4,128,22,120,4,128,9,208,8, 
  128,10,64,8,128,11,176,71,128,27,168,3,129,13,32,7,128,31,64,3,128,15,144,198,127,43,176,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,63,150,0,0,1,15,1,63,150,0,0,17,1,11,167,0,0,1, 
  15,1,63,150,0,0,17,1,229,165,0,0,1,15,1,63,150,0,0,17,1,220,5,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,63,150,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,63,150,0, 
  0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,63,150,0,0,1,15,1,63,150,0,0,17,1,180,151,0,0,1,19,2,0,0,0,10,0,0,0,1,0,17,1,63,150,0,0,1,19,2,0,0,0,9,0,0,0, 
  1,0,17,1,63,150,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,63,150,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,63,150,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,63,150, 
  0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,63,150,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,63,150,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,63,150,0,0,1,19,18,0, 
  0,0,50,0,0,0,1,0,17,1,63,150,0,0,1,2,21,1,152,2,0,0,233,237,0,0,21,0,0,0,4,0,0,0,16,184,200,132,1,48,3,128,2,48,131,131,3,168,19,128,4,144,18,128,5,32,18,128,22,72, 
  7,128,7,128,145,130,8,224,144,130,9,200,15,128,10,176,142,130,11,152,13,128,12,40,12,128,13,16,11,128,14,248,9,128,15,88,9,128,18,24,8,128,23,120,6,128,24,128,5,128,32,136,4,128,42,56,3,128,8,4, 
  19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,217,165,0,0,15,1,17,235,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,217,165,0,0,15, 
  1,166,155,0,0,17,1,128,154,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,217,165,0,0,15,1,166,155,0,0,17,1,77,154,0,0,1,4,15,1,217,165,0,0,15,1,234,226,0,0,15,1,246,226, 
  0,0,17,1,230,224,0,0,1,4,15,1,217,165,0,0,15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,217,165,0,0,15,1,51,224,0,0,17,1,141,213,0,0,1,4,15,1,217,165, 
  0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,217,165,0,0,15,1,219,202,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18, 
  0,0,0,51,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,51,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0, 
  0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,18,0,0,0,51,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,51, 
  0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,18,0,0,0,51,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,2,0,0,0,6,0, 
  0,0,1,0,19,18,0,0,0,51,0,0,0,2,0,1,4,15,1,217,165,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15,1,217,165,0,0,15,1,51,224,0,0,17,1,240,2,0,0,1,4,19,15,0, 
  0,0,44,0,0,0,3,0,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,18,0,0,0,51,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0, 
  0,0,3,0,0,0,1,0,19,18,0,0,0,51,0,0,0,2,0,1,2,21,1,39,0,0,0,32,9,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8, 
  19,31,0,0,0,83,0,0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0, 
  0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,22,155,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0, 
  15,1,22,155,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,240,32,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0, 
  23,136,66,129,25,248,1,128,10,240,1,128,43,24,3,128,28,144,1,128,31,16,4,128,6,240,1,128,7,168,67,126,19,10,0,0,0,32,0,0,0,2,0,1,1,19,7,0,0,0,22,0,0,0,1,0,17,1,22,155, 
  0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,22,155,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,22,155,0,0,1,15,1,22,155,0,0,17,1,108,142,0,0,1,15,1,22,155,0,0,17,1, 
  250,140,0,0,1,2,21,0,82,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,19,120,5,128,22,88,4,128,2,136,10,128,3,248,73,127,20,232,4,128,5,104,9,128,6,216,200,126,23,240,3,128,27,136,131,129, 
  9,72,8,128,10,184,7,128,11,40,71,127,31,32,3,128,13,152,6,128,43,144,2,128,15,8,70,127,19,31,0,0,0,84,0,0,0,1,0,17,1,166,155,0,0,1,15,1,166,155,0,0,17,1,247,164,0,0,1,15, 
  1,166,155,0,0,17,1,44,157,0,0,1,15,1,166,155,0,0,17,1,249,156,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,166,155,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,166,155,0,0, 
  1,19,3,0,0,0,13,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0,10,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0,9,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0, 
  8,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0,5,0,0,0,1,0, 
  17,1,166,155,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,166,155,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,166,155,0,0,1,1,2,21,1,39,0,0,0,32,9,1,0,3,0,0,0,1, 
  0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80, 
  1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,216,160,0,0,15,1,51,150,0,0,17,1,250,140,0,0, 
  1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,216,160,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1, 
  194,157,0,0,1,2,21,1,21,3,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,104,202,132,1,80,3,128,2,80,131,131,3,96,23,128,4,24,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130, 
  9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14,8,12,128,15,56,11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119, 
  0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,84,131,0,0,15,1,109,136,0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,72,131,0,0, 
  17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,84,131,0,0,15,1,109,136,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,84,131, 
  0,0,17,1,28,39,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,102,64,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,102,64,0,0, 
  15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1, 
  34,47,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,22,47,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24, 
  0,0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,19,0,0,0,52, 
  0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0, 
  0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0, 
  17,1,84,131,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0, 
  0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,84,131,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,6,0,0,0,21,0,0,0,3,0,1,4,19,3,0,0,0,12, 
  0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0, 
  0,0,24,0,0,0,1,0,17,1,84,131,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,144,65,129,25,88,3,128,10,136,2,128,43,232,3,128,28,248,2,128,31,144,2,128,6,136, 
  2,128,7,32,66,126,19,31,0,0,0,83,0,0,0,1,0,17,1,216,160,0,0,1,15,1,216,160,0,0,17,1,104,161,0,0,1,1,15,1,216,160,0,0,17,1,250,140,0,0,1,19,10,0,0,0,32,0,0,0, 
  2,0,1,19,7,0,0,0,22,0,0,0,1,0,17,1,216,160,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,216,160,0,0,1,2,21,1,119,0,0,0,176,8,1,0,5,0,0,0,2,0,0,0,24,88, 
  2,128,1,48,1,128,2,48,129,128,7,120,3,128,42,56,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,39,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0, 
  0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,39,150,0,0,17,1,250,140,0,0,1,4,17,1,224,161,0,0,1,2,21,1,22,3,0,0,201,246,0,0,22,0,0,0,4,0,0,0,16,104, 
  202,132,1,80,3,128,2,80,131,131,3,104,23,128,4,32,22,128,5,176,21,128,22,152,8,128,7,224,148,130,8,16,148,130,9,200,210,130,10,128,209,130,11,56,16,128,12,152,14,128,13,80,13,128,14,8,12,128,15,56, 
  11,128,18,152,9,128,23,152,7,128,24,208,6,128,32,168,5,128,41,216,4,128,42,88,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,251,145,0,0,15,1,109,136, 
  0,0,15,1,121,136,0,0,17,1,91,228,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,72,131,0,0,17,1,241,64,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,251,145,0,0,15, 
  1,109,136,0,0,15,1,229,64,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,251,145,0,0,17,1,114,64,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,102,64, 
  0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,102,64,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0, 
  15,1,90,64,0,0,17,1,190,55,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,90,64,0,0,17,1,34,47,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,22,47,0,0,17,1, 
  50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,26, 
  0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1, 
  0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1, 
  4,19,9,0,0,0,27,0,0,0,1,0,19,26,0,0,0,70,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,26,0,0,0,70,0, 
  0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0,0,15,1,10,47,0,0,17,1,249,167,0,0,1,4,15,1,251,145,0,0,15,1,109,136,0, 
  0,15,1,90,64,0,0,17,1,6,36,0,0,1,4,19,6,0,0,0,20,0,0,0,4,0,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0, 
  1,0,17,1,251,145,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,26,0,0,0,68,0,0,0,1,0,19,8,0,0,0,24,0,0,0,1,0,17,1,251,145,0,0,1,2,21,1,75,0,0,0,15,10,1, 
  0,4,0,0,0,2,0,0,0,34,184,1,128,1,176,1,128,2,176,129,127,35,16,1,128,4,15,1,78,165,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,4,15,1,78,165,0,0,15,1,8,31,0,0,17, 
  1,19,30,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17, 
  1,78,165,0,0,1,15,1,78,165,0,0,17,1,141,165,0,0,1,1,2,21,1,63,0,0,0,15,10,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,240,1,128,2,240,129,127,35,128,1,128,4,15,1,33,14, 
  0,0,17,1,19,30,0,0,1,4,15,1,33,14,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,33,0,0,0,2,0,14,1,19,18,0,0,0,51,0,0,0,2,0,1,21,1,149,0,0,0,78,246,0,0,6, 
  0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,123,166,0, 
  0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,123,166,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238, 
  140,0,0,17,1,133,136,0,0,1,4,17,1,194,157,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,240,66,129,25,232,3,128,10,232,2,128,43,88,2,128,28,248,1,128,31,144,1, 
  128,6,232,2,128,7,128,67,126,15,1,123,166,0,0,17,1,250,140,0,0,1,19,10,0,0,0,32,0,0,0,2,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,123,166,0,0,1,1,19,31,0,0,0,83,0, 
  0,0,1,0,17,1,123,166,0,0,1,15,1,123,166,0,0,17,1,104,161,0,0,1,19,7,0,0,0,22,0,0,0,1,0,17,1,123,166,0,0,1,2,21,1,75,0,0,0,242,10,1,0,4,0,0,0,2,0,0, 
  0,34,16,1,128,1,176,1,128,2,176,129,127,35,184,1,128,4,15,1,98,167,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,4,15,1,98,167,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11, 
  0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,98,167,0,0,1,15,1, 
  98,167,0,0,17,1,161,167,0,0,1,1,2,21,1,63,0,0,0,242,10,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,128,1,128,2,128,129,127,35,136,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0, 
  1,8,4,15,1,33,14,0,0,17,1,227,7,0,0,1,19,11,0,0,0,33,0,0,0,2,0,14,1,19,18,0,0,0,50,0,0,0,1,0,1,21,1,44,0,0,0,191,11,1,0,3,0,0,0,1,0,0,0,2, 
  240,128,128,1,240,0,128,12,248,0,128,8,4,19,9,0,0,0,29,0,0,0,2,0,1,2,19,2,0,0,0,6,0,0,0,1,0,1,21,1,102,3,0,0,244,11,1,0,22,0,0,0,4,0,0,0,16,40,78,132, 
  1,80,3,128,2,80,195,128,3,232,25,128,4,160,24,128,18,88,13,128,22,88,12,128,7,208,23,130,8,0,23,130,9,184,21,128,10,112,20,130,11,40,19,130,12,136,17,130,13,64,16,130,14,248,14,128,23,88,11,128, 
  24,216,9,128,32,176,8,128,42,48,7,128,43,232,5,128,44,160,4,128,45,88,3,128,8,4,19,44,0,0,0,122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17, 
  1,36,198,0,0,1,4,19,44,0,0,0,121,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19, 
  32,0,0,0,87,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,36,198,0,0,15,1,12, 
  198,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,36,198,0,0,15,1,12,198,0,0,15,1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57, 
  0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,36,198,0,0,15,1,12,198,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1,4,15,1,36,198,0,0,15,1,12,198,0,0,15,1,244,197,0,0, 
  15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,36,198,0,0,15,1,12,198,0,0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,36,198,0,0,15,1,12,198,0,0,15,1, 
  232,197,0,0,17,1,39,189,0,0,1,4,15,1,36,198,0,0,15,1,12,198,0,0,15,1,232,197,0,0,17,1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0, 
  19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4, 
  19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,9,0,0,0,28,0,0, 
  0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0, 
  38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,15,1,36,198,0, 
  0,15,1,12,198,0,0,15,1,90,180,0,0,17,1,249,167,0,0,1,4,15,1,36,198,0,0,15,1,12,198,0,0,15,1,232,197,0,0,17,1,153,171,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32, 
  0,0,0,85,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,14,0,0,0,38,0,0,0,1, 
  0,17,1,36,198,0,0,1,2,21,1,119,3,0,0,58,13,1,0,23,0,0,0,4,0,0,0,16,72,142,132,1,112,3,128,2,112,67,131,3,112,26,128,4,40,25,128,5,192,24,128,22,120,12,128,7,240,87,130,8, 
  32,87,130,9,216,21,128,10,144,84,130,11,72,83,130,12,168,81,130,13,96,80,130,14,24,15,128,18,120,13,128,23,120,11,128,24,248,9,128,32,208,8,128,42,80,7,128,43,8,6,128,44,192,4,128,45,120,3,128,8, 
  4,19,44,0,0,0,122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,44,0,0,0,121,0,0,0,1,0,19,32,0,0,0,87,0, 
  0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175, 
  0,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,29,175,0,0,15,1,17,175,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0, 
  0,1,0,15,1,29,175,0,0,15,1,17,175,0,0,15,1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,29,175,0,0,15,1,17, 
  175,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1,4,15,1,29,175,0,0,15,1,17,175,0,0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,29,175,0,0,15,1,17,175,0, 
  0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,29,175,0,0,15,1,17,175,0,0,15,1,232,197,0,0,17,1,39,189,0,0,1,4,15,1,29,175,0,0,15,1,17,175,0,0,15, 
  1,232,197,0,0,17,1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,9,0,0,0,30, 
  0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,32,0, 
  0,0,85,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0, 
  17,1,29,175,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0, 
  19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,15,1,29,175,0,0,15,1,17,175,0,0,15,1,90,180,0,0,17,1,249,167,0,0,1,4,15,1,29,175, 
  0,0,15,1,17,175,0,0,15,1,232,197,0,0,17,1,153,171,0,0,1,4,19,45,0,0,0,128,0,0,0,2,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,48,0, 
  0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,2,19,48,0, 
  0,0,133,0,0,0,1,0,1,21,0,77,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,32,136,132,131,9,72,9,128,10,184,8,128,3,216,73,129,20,8,71,130,45,216,2,128,22,120,6,128,23,232,5,129,11, 
  40,72,128,19,152,71,128,27,128,133,128,31,24,5,128,43,248,3,128,44,104,3,128,48,112,2,128,15,1,29,175,0,0,17,1,77,177,0,0,1,19,32,0,0,0,90,0,0,0,1,0,17,1,29,175,0,0,1,19,32, 
  0,0,0,87,0,0,0,1,0,17,1,29,175,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,29,175,0,0,1,19,48,0,0,0,133,0,0,0,1,0,17,1,29,175,0,0,1,15,1,29,175,0,0,17,1, 
  107,176,0,0,1,15,1,29,175,0,0,17,1,165,64,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,29,175,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,29,175,0,0,1,19,3,0,0,0,14, 
  0,0,0,1,0,17,1,29,175,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,29,175,0,0,1,19,32,0,0,0,89,0,0,0,1,0,17,1,29,175,0,0,1,19,32,0,0,0,88,0,0,0,1,0,17, 
  1,29,175,0,0,1,19,32,0,0,0,86,0,0,0,1,0,17,1,29,175,0,0,1,19,32,0,0,0,85,0,0,0,1,0,17,1,29,175,0,0,1,2,21,1,75,0,0,0,18,14,1,0,4,0,0,0,2,0,0, 
  0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,194,176,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,194,176,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11, 
  0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,194,176,0,0,1,15,1, 
  194,176,0,0,17,1,1,177,0,0,1,1,2,21,1,63,0,0,0,18,14,1,0,4,0,0,0,2,0,0,0,34,136,1,128,1,128,1,128,2,128,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0, 
  1,8,4,15,1,33,14,0,0,17,1,19,30,0,0,1,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,0,3,0,0,58,13,1,0,23,0,0,0,4,0,0,0,16,152,140,132,1,112,3,128,2,112,67,131,3, 
  232,22,128,4,208,21,128,5,96,21,128,22,40,11,128,7,192,84,130,8,32,84,130,9,8,19,128,10,240,81,130,11,216,80,130,12,104,79,130,13,80,78,130,14,56,13,128,18,248,11,128,23,88,10,128,24,8,9,128,32, 
  16,8,128,42,192,6,128,43,168,5,128,44,144,4,128,45,120,3,128,8,4,19,44,0,0,0,122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,4,19,44,0,0, 
  0,121,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,48,0,0,0,134,0, 
  0,0,2,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,78,180,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15, 
  1,78,180,0,0,15,1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,78,180,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1, 
  4,15,1,78,180,0,0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,78,180,0,0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,78,180,0,0,15, 
  1,232,197,0,0,17,1,39,189,0,0,1,4,15,1,78,180,0,0,15,1,232,197,0,0,17,1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0, 
  134,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13, 
  0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,134,0,0,0, 
  2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1, 
  0,19,48,0,0,0,134,0,0,0,2,0,1,4,15,1,78,180,0,0,15,1,90,180,0,0,17,1,249,167,0,0,1,4,15,1,78,180,0,0,15,1,232,197,0,0,17,1,153,171,0,0,1,4,19,45,0,0,0,127, 
  0,0,0,3,0,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85, 
  0,0,0,1,0,19,48,0,0,0,134,0,0,0,2,0,1,2,19,48,0,0,0,134,0,0,0,2,0,1,19,32,0,0,0,86,0,0,0,1,0,1,21,1,119,3,0,0,246,14,1,0,23,0,0,0,4,0,0,0, 
  16,176,142,132,1,112,3,129,2,112,67,131,3,112,26,128,4,40,25,128,17,72,14,128,22,120,12,128,7,88,88,130,8,136,87,130,9,64,22,128,10,248,84,130,11,176,83,130,12,16,82,130,13,200,80,130,14,128,15,128, 
  18,120,13,128,23,120,11,128,24,248,9,128,32,208,8,128,42,80,7,128,43,8,6,128,44,192,4,128,45,120,3,128,8,4,19,44,0,0,0,122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,47,0,0, 
  0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,44,0,0,0,121,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,44,0,0, 
  0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15, 
  1,234,183,0,0,15,1,222,183,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,234,183,0,0,15,1,222,183,0,0,15,1,0,198,0,0,17,1,165,64,0,0, 
  1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,234,183,0,0,15,1,222,183,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1,4,15,1,234,183,0,0,15,1,222,183, 
  0,0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,234,183,0,0,15,1,222,183,0,0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,234,183,0,0, 
  15,1,222,183,0,0,15,1,232,197,0,0,17,1,39,189,0,0,1,4,19,45,0,0,0,126,0,0,0,2,0,1,4,15,1,234,183,0,0,15,1,222,183,0,0,15,1,232,197,0,0,17,1,102,180,0,0,1,4,19, 
  9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,32,0,0,0,86,0,0,0, 
  1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,47,0,0,0,131, 
  0,0,0,1,0,17,1,234,183,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,9,0,0,0,27, 
  0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0, 
  0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,15,1,234,183,0,0,15,1,222,183,0,0,15,1,90,180,0,0,17,1,249,167,0,0,1,4,15,1,234,183,0,0,15,1,222,183,0,0,15,1,232,197,0,0, 
  17,1,153,171,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,32,0,0,0,85,0,0,0,1,0,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,2,19,47,0,0,0,131,0,0,0,1,0,1,21,0,77,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0, 
  32,136,4,128,9,72,9,128,10,184,8,128,3,216,73,129,20,8,71,130,45,216,2,128,22,120,6,128,23,232,5,129,11,40,72,128,19,152,71,128,27,128,133,128,31,24,197,128,43,248,3,128,44,104,3,128,47,112,2,128, 
  15,1,234,183,0,0,17,1,26,186,0,0,1,19,32,0,0,0,90,0,0,0,1,0,17,1,234,183,0,0,1,19,32,0,0,0,87,0,0,0,1,0,17,1,234,183,0,0,1,19,31,0,0,0,84,0,0,0,1,0, 
  17,1,234,183,0,0,1,19,47,0,0,0,131,0,0,0,1,0,17,1,234,183,0,0,1,15,1,234,183,0,0,17,1,56,185,0,0,1,15,1,234,183,0,0,17,1,165,64,0,0,1,19,31,0,0,0,83,0,0,0, 
  1,0,17,1,234,183,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,234,183,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,234,183,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,234,183, 
  0,0,1,19,32,0,0,0,89,0,0,0,1,0,17,1,234,183,0,0,1,19,32,0,0,0,88,0,0,0,1,0,17,1,234,183,0,0,1,19,32,0,0,0,86,0,0,0,1,0,17,1,234,183,0,0,1,19,32,0, 
  0,0,85,0,0,0,1,0,17,1,234,183,0,0,1,2,21,1,75,0,0,0,206,15,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,176,1,128,2,176,129,127,35,184,1,128,4,15,1,143,185,0,0,15,1,8, 
  31,0,0,17,1,19,30,0,0,1,8,4,15,1,143,185,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,143,185,0,0,1,15,1,143,185,0,0,17,1,206,185,0,0,1,1,2,21,1,63,0,0,0,206,15,1,0,4,0,0,0, 
  2,0,0,0,34,136,1,128,1,128,1,128,2,128,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,8,4,15,1,33,14,0,0,17,1,19,30,0,0,1,19,11,0,0,0,33,0,0,0,2,0, 
  14,1,21,1,0,3,0,0,246,14,1,0,23,0,0,0,4,0,0,0,16,8,141,132,1,112,3,129,2,112,67,131,3,232,22,128,4,208,21,128,17,152,12,128,22,40,11,128,7,48,85,130,8,144,84,130,9,120,19,128, 
  10,96,82,130,11,72,81,130,12,216,79,130,13,192,78,130,14,168,13,128,18,248,11,128,23,88,10,128,24,8,9,128,32,16,8,128,42,192,6,128,43,168,5,128,44,144,4,128,45,120,3,128,8,4,19,44,0,0,0,122, 
  0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,44,0,0,0,121,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,47,0,0,0,132,0,0,0, 
  2,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1, 
  0,15,1,27,189,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,27,189,0,0,15,1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0, 
  0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,27,189,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1,4,15,1,27,189,0,0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4, 
  15,1,27,189,0,0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,27,189,0,0,15,1,232,197,0,0,17,1,39,189,0,0,1,4,19,45,0,0,0,125,0,0,0,3,0,14,1,4, 
  15,1,27,189,0,0,15,1,232,197,0,0,17,1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,9,0,0,0, 
  30,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,32,0,0,0,85,0,0, 
  0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0, 
  1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1, 
  4,15,1,27,189,0,0,15,1,90,180,0,0,17,1,249,167,0,0,1,4,15,1,27,189,0,0,15,1,232,197,0,0,17,1,153,171,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0,0, 
  0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,47,0,0,0,132,0,0,0,2,0,1,2,19,47,0,0,0,132,0,0,0, 
  2,0,1,21,1,119,3,0,0,178,16,1,0,23,0,0,0,4,0,0,0,16,176,142,132,1,112,3,128,2,112,195,128,3,112,26,131,4,40,25,128,18,224,13,128,22,120,12,128,7,88,88,130,8,136,87,130,9,64,22, 
  128,10,248,84,130,11,176,83,130,12,16,82,130,13,200,80,130,14,128,15,128,19,120,13,128,23,120,11,128,24,248,9,128,32,208,8,128,42,80,7,128,43,8,6,128,44,192,4,128,45,120,3,128,8,4,19,44,0,0,0, 
  122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,44,0,0,0,121,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,46, 
  0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,43, 
  0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,171,192,0,0,15,1,159,192,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,171, 
  192,0,0,15,1,159,192,0,0,15,1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,171,192,0,0,15,1,159,192,0,0,15,1,24, 
  198,0,0,17,1,140,7,0,0,1,4,15,1,171,192,0,0,15,1,159,192,0,0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,171,192,0,0,15,1,159,192,0,0,15,1,244,197,0, 
  0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,19,45,0,0,0,124,0,0,0,2,0,1,4,15,1,171,192,0,0,15,1,159,192,0,0,15,1,232,197,0,0,17,1,39,189,0,0,1,4,15,1,171,192,0,0, 
  15,1,159,192,0,0,15,1,232,197,0,0,17,1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1, 
  4,19,9,0,0,0,30,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0, 
  0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0, 
  0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,19,9,0,0, 
  0,26,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,4,15,1,171,192,0,0,15,1,159,192,0,0,15,1,90,180,0,0,17,1,249,167,0, 
  0,1,4,15,1,171,192,0,0,15,1,159,192,0,0,15,1,232,197,0,0,17,1,153,171,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,46,0,0,0,129,0,0,0, 
  1,0,17,1,171,192,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,2,19,46,0,0,0,129,0,0,0, 
  1,0,1,21,0,77,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,32,136,4,128,9,72,9,128,10,184,8,128,3,216,73,129,20,8,71,130,45,216,2,128,22,120,6,130,23,232,5,129,11,40,72,128,19,152,71, 
  128,27,128,133,128,31,24,5,128,43,248,3,128,44,104,3,128,46,112,2,128,15,1,171,192,0,0,17,1,219,194,0,0,1,19,32,0,0,0,90,0,0,0,1,0,17,1,171,192,0,0,1,19,32,0,0,0,87,0,0, 
  0,1,0,17,1,171,192,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,171,192,0,0,1,19,46,0,0,0,129,0,0,0,1,0,17,1,171,192,0,0,1,15,1,171,192,0,0,17,1,249,193,0,0,1,15, 
  1,171,192,0,0,17,1,165,64,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,171,192,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,171,192,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17, 
  1,171,192,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,171,192,0,0,1,19,32,0,0,0,89,0,0,0,1,0,17,1,171,192,0,0,1,19,32,0,0,0,88,0,0,0,1,0,17,1,171,192,0,0,1, 
  19,32,0,0,0,86,0,0,0,1,0,17,1,171,192,0,0,1,19,32,0,0,0,85,0,0,0,1,0,17,1,171,192,0,0,1,2,21,1,75,0,0,0,138,17,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1, 
  80,2,128,2,80,130,127,35,176,1,128,4,15,1,80,194,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,80,194,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0, 
  0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,80,194,0,0,1,15,1,80,194,0,0,17,1, 
  143,194,0,0,1,1,2,21,1,63,0,0,0,138,17,1,0,4,0,0,0,2,0,0,0,34,136,1,128,1,128,1,128,2,128,129,127,35,16,1,128,4,15,1,33,14,0,0,17,1,227,7,0,0,1,8,4,15,1,33, 
  14,0,0,17,1,19,30,0,0,1,19,11,0,0,0,33,0,0,0,2,0,14,1,21,1,0,3,0,0,178,16,1,0,23,0,0,0,4,0,0,0,16,8,141,132,1,112,3,128,2,112,195,128,3,232,22,131,4,208,21, 
  128,18,104,12,128,22,40,11,128,7,48,85,130,8,144,84,130,9,120,19,128,10,96,82,130,11,72,81,130,12,216,79,130,13,192,78,130,14,168,13,128,19,248,11,128,23,88,10,128,24,8,9,128,32,16,8,128,42,192,6, 
  128,43,168,5,128,44,144,4,128,45,120,3,128,8,4,19,44,0,0,0,122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,44,0,0,0,121,0,0,0,1, 
  0,19,32,0,0,0,87,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4, 
  19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,220,197,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,220,197,0,0,15, 
  1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,220,197,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1,4,15,1,220,197,0, 
  0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,220,197,0,0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,19,45,0,0,0,123,0,0,0,3,0,14,1, 
  4,15,1,220,197,0,0,15,1,232,197,0,0,17,1,39,189,0,0,1,4,15,1,220,197,0,0,15,1,232,197,0,0,17,1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0, 
  0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0, 
  1,0,19,3,0,0,0,13,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19, 
  46,0,0,0,130,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,32, 
  0,0,0,86,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,15,1,220,197,0,0,15,1,90,180,0,0,17,1,249,167,0,0,1,4,15,1,220,197,0,0,15,1,232,197,0,0,17,1,153,171,0,0, 
  1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,46,0,0,0,130,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19, 
  46,0,0,0,130,0,0,0,2,0,1,2,19,46,0,0,0,130,0,0,0,2,0,1,19,32,0,0,0,90,0,0,0,1,0,1,19,32,0,0,0,85,0,0,0,1,0,1,19,32,0,0,0,88,0,0,0,1,0,1, 
  19,14,0,0,0,38,0,0,0,1,0,1,19,32,0,0,0,89,0,0,0,1,0,1,21,0,82,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,32,64,4,128,19,80,7,128,27,56,197,128,3,0,138,127,20,192, 
  6,128,43,176,3,128,22,48,6,128,23,160,5,128,45,144,2,128,9,112,9,128,10,224,8,128,11,80,200,125,44,32,3,128,13,72,200,126,14,224,7,128,31,208,4,128,19,32,0,0,0,90,0,0,0,1,0,17,1,36, 
  198,0,0,1,19,32,0,0,0,87,0,0,0,1,0,17,1,36,198,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,36,198,0,0,1,19,14,0,0,0,38,0,0,0,1,0,17,1,36,198,0,0,1,15,1, 
  36,198,0,0,17,1,132,202,0,0,1,15,1,36,198,0,0,17,1,165,64,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,36,198,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,36,198,0,0,1, 
  19,3,0,0,0,14,0,0,0,1,0,17,1,36,198,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,36,198,0,0,1,15,1,36,198,0,0,17,1,119,199,0,0,1,1,19,32,0,0,0,89,0,0,0,1, 
  0,17,1,36,198,0,0,1,19,32,0,0,0,88,0,0,0,1,0,17,1,36,198,0,0,1,19,32,0,0,0,86,0,0,0,1,0,17,1,36,198,0,0,1,19,32,0,0,0,85,0,0,0,1,0,17,1,36,198,0, 
  0,1,2,21,1,0,3,0,0,58,13,1,0,23,0,0,0,4,0,0,0,16,152,140,132,1,112,3,128,2,112,67,131,3,232,22,128,4,208,21,128,5,96,21,128,22,40,11,128,7,192,84,130,8,32,84,130,9,8,19, 
  128,10,240,81,130,11,216,80,130,12,104,79,130,13,80,78,130,14,56,13,128,18,248,11,128,23,88,10,128,24,8,9,128,32,16,8,128,42,192,6,128,43,168,5,128,44,144,4,128,45,120,3,128,8,4,19,44,0,0,0, 
  122,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,19,44,0,0,0,121,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,14,0,0,0,39,0,0, 
  0,2,0,1,4,19,44,0,0,0,120,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0, 
  1,0,15,1,120,202,0,0,15,1,24,198,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,120,202,0,0,15,1,0,198,0,0,17,1,165,64,0,0,1,4,19,23,0,0,0,57,0, 
  0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,120,202,0,0,15,1,24,198,0,0,17,1,140,7,0,0,1,4,15,1,120,202,0,0,15,1,244,197,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1, 
  4,15,1,120,202,0,0,15,1,244,197,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,120,202,0,0,15,1,232,197,0,0,17,1,39,189,0,0,1,4,15,1,120,202,0,0,15,1,232,197,0,0,17, 
  1,102,180,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,32,0,0,0,86,0, 
  0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2, 
  0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0, 
  19,14,0,0,0,39,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,32,0,0,0,86,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,15,1,120,202,0,0,15,1,90,180,0,0, 
  17,1,249,167,0,0,1,4,15,1,120,202,0,0,15,1,232,197,0,0,17,1,153,171,0,0,1,4,19,13,0,0,0,37,0,0,0,3,0,14,1,4,19,3,0,0,0,12,0,0,0,1,0,19,32,0,0,0,85,0, 
  0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,32,0,0,0,85,0,0,0,1,0,19,14,0,0,0,39,0,0,0,2,0,1,2,19,14,0,0,0,39,0,0, 
  0,2,0,1,21,1,75,0,0,0,18,14,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,194,176,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15, 
  1,194,176,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,19,2,0,0,0,9,0,0,0,1,0,1,21,1,235,2,0,0,110,18,1,0,21,0,0,0,4,0,0, 
  0,16,128,201,132,1,48,3,129,2,48,131,131,3,16,22,128,4,200,20,128,17,24,9,128,22,72,7,128,7,248,147,130,8,40,147,130,9,224,17,128,10,152,144,130,11,80,15,128,12,176,13,128,13,104,12,128,14,32,11, 
  128,15,80,10,128,18,72,8,128,23,72,6,128,24,128,5,128,32,184,4,128,42,56,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,44,207,0,0,15,1,129,213,0, 
  0,15,1,17,235,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,44,207,0,0,17,1,6,206,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,44,207,0,0,17,1,211, 
  205,0,0,1,4,15,1,44,207,0,0,15,1,129,213,0,0,15,1,234,226,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,44,207,0,0,15,1,129,213,0,0,15,1,234,226,0,0,15,1,218,224,0, 
  0,17,1,63,224,0,0,1,4,15,1,44,207,0,0,15,1,129,213,0,0,15,1,51,224,0,0,17,1,141,213,0,0,1,4,19,15,0,0,0,43,0,0,0,2,0,1,4,15,1,44,207,0,0,15,1,129,213,0,0, 
  15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,44,207,0,0,15,1,129,213,0,0,15,1,219,202,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0, 
  1,0,19,17,0,0,0,48,0,0,0,1,0,17,1,44,207,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,17,0,0,0,48,0,0,0,1,0,17,1,44,207,0,0, 
  1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,17,0,0,0,48,0,0,0,1,0,17,1,44,207,0,0,1,4,19,9,0,0,0,28, 
  0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,17,0,0,0,48,0,0,0,1,0,17,1,44,207,0,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,17,0, 
  0,0,48,0,0,0,1,0,17,1,44,207,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,17,0,0,0,48,0,0,0,1,0,17,1,44,207,0,0,1,4,15,1,44, 
  207,0,0,15,1,129,213,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15,1,44,207,0,0,15,1,129,213,0,0,15,1,51,224,0,0,17,1,240,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0, 
  19,2,0,0,0,3,0,0,0,1,0,19,17,0,0,0,48,0,0,0,1,0,17,1,44,207,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,17,0,0,0,48,0,0, 
  0,1,0,17,1,44,207,0,0,1,2,21,1,39,0,0,0,47,19,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,6,248,0,128,8,4,17,1,15,6,0,0,1,19,31,0,0,0,83,0,0,0,1, 
  0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0, 
  0,0,84,0,0,0,1,0,15,1,156,206,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,156,206,0,0,15,1,51,150, 
  0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,240,32,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,232,67,129,25,144,1,128,10,32, 
  2,128,43,88,3,128,28,248,2,128,31,144,2,128,6,32,2,128,7,40,66,126,19,7,0,0,0,22,0,0,0,1,0,17,1,156,206,0,0,1,1,15,1,156,206,0,0,17,1,108,142,0,0,1,15,1,156,206,0,0, 
  17,1,250,140,0,0,1,19,10,0,0,0,32,0,0,0,2,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,156,206,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,156,206,0,0,1,2,21,0,116,1, 
  0,0,255,255,255,255,17,0,0,0,4,0,0,0,19,152,5,128,17,40,6,128,2,16,11,128,3,128,74,127,20,8,5,128,5,240,9,128,6,96,137,128,23,16,4,128,22,120,4,128,9,208,8,128,10,64,8,128,11,176, 
  71,128,27,168,3,129,13,32,7,128,31,64,3,128,15,144,198,127,43,176,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,44,207,0,0,1,15,1,44,207,0,0,17,1,159,212,0,0,1,15,1,44,207,0,0,17, 
  1,121,211,0,0,1,15,1,44,207,0,0,17,1,70,211,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,44,207,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,44,207,0,0,1,19,3,0,0,0, 
  13,0,0,0,1,0,17,1,44,207,0,0,1,15,1,44,207,0,0,17,1,161,208,0,0,1,19,2,0,0,0,10,0,0,0,1,0,17,1,44,207,0,0,1,19,2,0,0,0,9,0,0,0,1,0,17,1,44,207,0, 
  0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,44,207,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,44,207,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,44,207,0,0,1,19,2,0,0, 
  0,5,0,0,0,1,0,17,1,44,207,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,44,207,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,44,207,0,0,1,19,17,0,0,0,48,0,0,0,1, 
  0,17,1,44,207,0,0,1,2,21,1,152,2,0,0,110,18,1,0,21,0,0,0,4,0,0,0,16,40,201,132,1,48,3,129,2,48,131,131,3,168,19,128,4,144,18,128,17,184,8,128,22,72,7,128,7,240,145,130,8, 
  80,145,130,9,56,16,128,10,32,143,130,11,8,14,128,12,152,12,128,13,128,11,128,14,104,10,128,15,200,9,128,18,24,8,128,23,120,6,128,24,128,5,128,32,136,4,128,42,56,3,128,8,4,19,43,0,0,0,119,0, 
  0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,58,211,0,0,15,1,17,235,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,58,211,0,0,15,1,166,155,0,0,17,1, 
  128,154,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,58,211,0,0,15,1,166,155,0,0,17,1,77,154,0,0,1,4,15,1,58,211,0,0,15,1,234,226,0,0,15,1,246,226,0,0,17,1,230,224,0, 
  0,1,4,15,1,58,211,0,0,15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,58,211,0,0,15,1,51,224,0,0,17,1,141,213,0,0,1,4,19,15,0,0,0,42,0,0,0,3,0, 
  14,1,4,15,1,58,211,0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,58,211,0,0,15,1,219,202,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6, 
  0,0,0,1,0,19,17,0,0,0,49,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,17,0,0,0,49,0,0,0,2,0,1,4,19,19,0,0,0,52,0, 
  0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,17,0,0,0,49,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1, 
  0,19,17,0,0,0,49,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,17,0,0,0,49,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0, 
  19,2,0,0,0,6,0,0,0,1,0,19,17,0,0,0,49,0,0,0,2,0,1,4,15,1,58,211,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15,1,58,211,0,0,15,1,51,224,0,0,17,1,240,2, 
  0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,17,0,0,0,49,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1, 
  0,19,17,0,0,0,49,0,0,0,2,0,1,2,19,17,0,0,0,49,0,0,0,2,0,1,21,1,39,0,0,0,47,19,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15, 
  6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8, 
  4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,15,212,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83, 
  0,0,0,1,0,15,1,15,212,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,194,157,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,23,88,67,129,25,96,2,128,10,144,1,128,43,232,3,128,28,152,1,128,31,240,2,128,6,144,1,128,7,248,65,126,1,19,10,0,0,0,32,0,0,0,2,0,1,15,1,15,212,0,0,17,1,104,161, 
  0,0,1,19,7,0,0,0,22,0,0,0,1,0,17,1,15,212,0,0,1,15,1,15,212,0,0,17,1,250,140,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,15,212,0,0,1,19,31,0,0,0,84,0,0, 
  0,1,0,17,1,15,212,0,0,1,2,21,1,75,0,0,0,8,20,1,0,4,0,0,0,2,0,0,0,34,184,1,128,1,176,1,128,2,176,129,127,35,16,1,128,4,15,1,246,212,0,0,15,1,8,31,0,0,17,1, 
  227,7,0,0,1,8,4,15,1,246,212,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128, 
  11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,246,212,0,0,1,15,1,246,212,0,0,17,1,53,213,0,0,1,1,2,21,1,63,0,0,0,8,20,1,0,4,0,0,0,2,0,0,0,34, 
  16,1,128,1,128,1,128,2,128,129,127,35,136,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,4,15,1,33,14,0,0,17,1,227,7,0,0,1,19,11,0,0,0,33,0,0,0,2,0,14,1,19,17,0, 
  0,0,48,0,0,0,1,0,1,21,1,235,2,0,0,213,20,1,0,21,0,0,0,4,0,0,0,16,128,201,132,1,48,3,128,2,48,195,128,3,16,86,131,4,200,20,128,18,176,8,128,22,72,7,128,7,248,147,130,8, 
  40,147,130,9,224,17,128,10,152,144,130,11,80,15,128,12,176,13,128,13,104,12,128,14,32,11,128,15,80,10,128,19,72,8,128,23,72,6,128,24,128,5,128,32,184,4,128,42,56,3,128,8,4,19,43,0,0,0,119,0, 
  0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,210,217,0,0,15,1,39,224,0,0,15,1,17,235,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,210,217,0,0,17,1, 
  172,216,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,210,217,0,0,17,1,121,216,0,0,1,4,15,1,210,217,0,0,15,1,39,224,0,0,15,1,234,226,0,0,15,1,246,226,0,0,17,1,230,224,0, 
  0,1,4,15,1,210,217,0,0,15,1,39,224,0,0,15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,19,15,0,0,0,41,0,0,0,2,0,1,4,15,1,210,217,0,0,15,1,39,224,0,0, 
  15,1,51,224,0,0,17,1,141,213,0,0,1,4,15,1,210,217,0,0,15,1,39,224,0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,210,217,0,0,15,1,39,224,0,0,15,1,219,202,0,0,17,1, 
  50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,2, 
  0,0,0,6,0,0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1, 
  0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1, 
  4,19,9,0,0,0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,2,0,0,0,6,0, 
  0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,4,15,1,210,217,0,0,15,1,39,224,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15,1,210,217,0,0,15,1,39,224,0, 
  0,15,1,51,224,0,0,17,1,240,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,4,19,3,0,0, 
  0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,2,21,1,39,0,0,0,150,21,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1, 
  48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104, 
  4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,66,217,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57, 
  0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,66,217,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,240,32,0,0,1,2,21,0, 
  143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,24,67,129,25,248,1,128,10,144,1,128,43,136,2,128,28,152,1,128,31,168,3,128,6,144,1,128,7,16,68,126,1,19,10,0,0,0,32,0,0,0,2,0, 
  1,19,7,0,0,0,22,0,0,0,1,0,17,1,66,217,0,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,66,217,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,66,217,0,0,1,15,1,66,217,0, 
  0,17,1,250,140,0,0,1,15,1,66,217,0,0,17,1,108,142,0,0,1,2,21,0,116,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,16,40,6,128,19,152,5,128,2,16,11,128,3,128,138,127,20,8,5,128, 
  5,240,9,128,6,96,137,128,23,16,4,128,22,120,4,128,9,208,8,128,10,64,8,128,11,176,71,128,27,168,3,129,13,32,7,128,31,64,3,128,15,144,198,127,43,176,2,128,19,31,0,0,0,84,0,0,0,1,0,17, 
  1,210,217,0,0,1,15,1,210,217,0,0,17,1,69,223,0,0,1,15,1,210,217,0,0,17,1,31,222,0,0,1,15,1,210,217,0,0,17,1,236,221,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,210,217, 
  0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,210,217,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,210,217,0,0,1,15,1,210,217,0,0,17,1,71,219,0,0,1,19,2,0,0,0,10,0,0, 
  0,1,0,17,1,210,217,0,0,1,19,2,0,0,0,9,0,0,0,1,0,17,1,210,217,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,210,217,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,210, 
  217,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,210,217,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,210,217,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,210,217,0,0,1,19,2, 
  0,0,0,3,0,0,0,1,0,17,1,210,217,0,0,1,19,16,0,0,0,46,0,0,0,1,0,17,1,210,217,0,0,1,2,21,1,152,2,0,0,213,20,1,0,21,0,0,0,4,0,0,0,16,40,201,132,1,48,3, 
  128,2,48,195,128,3,168,83,131,4,144,18,128,18,136,8,128,22,72,7,128,7,240,145,130,8,80,145,130,9,56,16,128,10,32,143,130,11,8,14,128,12,152,12,128,13,128,11,128,14,104,10,128,15,200,9,128,19,24,8, 
  128,23,120,6,128,24,128,5,128,32,136,4,128,42,56,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,224,221,0,0,15,1,17,235,0,0,17,1,91,228,0,0,1, 
  4,19,27,0,0,0,76,0,0,0,1,0,15,1,224,221,0,0,15,1,166,155,0,0,17,1,128,154,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,15,1,224,221,0,0,15,1,166,155,0,0,17,1,77,154,0, 
  0,1,4,15,1,224,221,0,0,15,1,234,226,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,224,221,0,0,15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,19,15,0,0,0, 
  40,0,0,0,3,0,14,1,4,15,1,224,221,0,0,15,1,51,224,0,0,17,1,141,213,0,0,1,4,15,1,224,221,0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4,15,1,224,221,0,0,15,1,219,202,0, 
  0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,16,0,0,0,47,0,0,0,2,0,1,4,19,9,0,0,0,30,0,0,0,1,0,19,2,0,0,0, 
  6,0,0,0,1,0,19,16,0,0,0,47,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,16,0,0,0,47,0,0, 
  0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,16,0,0,0,47,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0,1,0,19,2,0,0,0,6,0,0,0, 
  1,0,19,16,0,0,0,47,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,16,0,0,0,47,0,0,0,2,0,1,4,15,1,224,221,0,0,15,1,38,168, 
  0,0,17,1,249,167,0,0,1,4,15,1,224,221,0,0,15,1,51,224,0,0,17,1,240,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,16,0,0,0,47,0,0, 
  0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,16,0,0,0,47,0,0,0,2,0,1,2,19,16,0,0,0,47,0,0,0,2,0,1,21,1,39,0,0,0,150,21, 
  1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0,0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0, 
  0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,181,222,0,0,15,1,51,150,0, 
  0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,181,222,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133, 
  136,0,0,1,4,17,1,194,157,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,232,67,129,25,88,3,128,10,248,1,128,43,96,2,128,28,0,2,128,31,144,1,128,6,248,1,128,7, 
  240,66,126,15,1,181,222,0,0,17,1,250,140,0,0,1,1,19,10,0,0,0,32,0,0,0,2,0,1,19,31,0,0,0,84,0,0,0,1,0,17,1,181,222,0,0,1,15,1,181,222,0,0,17,1,104,161,0,0,1, 
  19,7,0,0,0,22,0,0,0,1,0,17,1,181,222,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,181,222,0,0,1,2,21,1,75,0,0,0,111,22,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1, 
  176,1,128,2,176,129,127,35,184,1,128,4,15,1,156,223,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,4,15,1,156,223,0,0,15,1,8,31,0,0,17,1,227,7,0,0,1,19,11,0,0,0,34,0,0, 
  0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0,0,35,0,0,0,1,0,17,1,156,223,0,0,1,15,1,156,223,0,0,17,1, 
  219,223,0,0,1,1,2,21,1,63,0,0,0,111,22,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,128,1,128,2,128,129,127,35,136,1,128,4,15,1,33,14,0,0,17,1,19,30,0,0,1,8,4,15,1,33, 
  14,0,0,17,1,227,7,0,0,1,19,11,0,0,0,33,0,0,0,2,0,14,1,19,16,0,0,0,46,0,0,0,1,0,1,19,2,0,0,0,10,0,0,0,1,0,1,21,1,50,0,0,0,145,240,0,0,3,0,0, 
  0,1,0,0,0,2,240,128,128,1,240,0,128,20,248,0,128,8,4,19,21,0,0,0,54,0,0,0,1,0,17,1,114,224,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,20,56,1,128, 
  21,208,0,128,15,1,114,224,0,0,17,1,155,224,0,0,1,1,2,21,1,62,0,0,0,239,240,0,0,4,0,0,0,2,0,0,0,20,136,1,128,1,16,129,128,2,16,1,128,21,24,1,128,8,4,19,20,0,0,0, 
  53,0,0,0,3,0,14,1,4,19,21,0,0,0,55,0,0,0,2,0,1,2,19,3,0,0,0,14,0,0,0,1,0,1,21,1,201,0,0,0,201,239,0,0,7,0,0,0,2,0,0,0,12,88,69,129,1,112,129,128, 
  2,112,129,128,21,184,4,128,22,24,132,128,24,200,2,128,42,120,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,176,225,0,0,15,1,78,32,0,0,17,1,91,228, 
  0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,176,225,0,0,15,1,78,32,0,0,17,1,140,7,0,0,1,4,15,1,176,225,0,0,15,1,128,7,0,0,17,1, 
  63,224,0,0,1,4,15,1,176,225,0,0,15,1,116,7,0,0,17,1,217,6,0,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,24,0,0,0,63,0,0,0,1,0,17,1,176,225,0,0,1,2,21,0,189,0, 
  0,0,255,255,255,255,9,0,0,0,3,0,0,0,24,56,3,128,33,64,2,128,19,200,132,129,11,88,197,127,20,56,4,128,31,208,2,128,22,48,4,128,23,160,131,127,43,176,1,128,19,31,0,0,0,84,0,0,0,1, 
  0,17,1,176,225,0,0,1,19,24,0,0,0,66,0,0,0,1,0,17,1,176,225,0,0,1,15,1,176,225,0,0,17,1,95,26,0,0,1,15,1,176,225,0,0,17,1,110,226,0,0,1,19,31,0,0,0,83,0,0, 
  0,1,0,17,1,176,225,0,0,1,1,19,24,0,0,0,65,0,0,0,1,0,17,1,176,225,0,0,1,19,24,0,0,0,63,0,0,0,1,0,17,1,176,225,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1, 
  176,225,0,0,1,2,21,7,96,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,47,144,1,128,45,208,193,128,42,80,194,128,43,16,66,127,93,144,2,128,94,80,1,128,4,17,1,57,28,0,0,1,4,17,1,182, 
  26,0,0,1,4,17,1,220,24,0,0,1,4,17,1,77,16,0,0,1,4,17,1,231,9,0,0,1,4,19,22,0,0,0,56,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,208,0,128,8,2,19,2,0,0,0,3,0,0,0,1,0,1,19,3,0,0,0,15,0,0,0,1,0,1,21,1,39,0,0,0,60,23,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128, 
  6,248,0,128,8,4,17,1,15,6,0,0,1,19,31,0,0,0,83,0,0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24, 
  168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0,84,0,0,0,1,0,15,1,203,227,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0, 
  1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,203,227,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,240,32,0,0,1,2,21,0,143,0,0, 
  0,255,255,255,255,8,0,0,0,3,0,0,0,23,0,66,129,25,240,2,128,10,248,1,128,43,232,3,128,28,144,2,128,31,128,3,128,6,248,1,128,7,144,65,126,15,1,203,227,0,0,17,1,108,142,0,0,1,1,19, 
  31,0,0,0,83,0,0,0,1,0,17,1,203,227,0,0,1,19,10,0,0,0,32,0,0,0,2,0,1,19,7,0,0,0,22,0,0,0,1,0,17,1,203,227,0,0,1,15,1,203,227,0,0,17,1,250,140,0,0,1, 
  19,31,0,0,0,84,0,0,0,1,0,17,1,203,227,0,0,1,2,21,1,75,0,0,0,74,241,0,0,4,0,0,0,2,0,0,0,34,176,1,128,1,80,2,128,2,80,130,127,35,16,1,128,4,15,1,125,30,0,0, 
  15,1,8,31,0,0,17,1,227,7,0,0,1,4,15,1,125,30,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,121,1,0,0,255,255,255,255,18,0,0,0, 
  4,0,0,0,0,192,11,128,1,88,11,128,2,200,10,128,3,56,74,129,20,40,5,128,5,168,9,128,6,24,137,129,23,48,4,128,19,184,5,128,9,136,8,128,10,248,7,128,11,104,199,128,22,152,4,128,13,216,6,128, 
  27,200,195,128,15,72,70,128,31,96,3,128,43,208,2,128,19,31,0,0,0,84,0,0,0,1,0,17,1,178,228,0,0,1,15,1,178,228,0,0,17,1,35,234,0,0,1,15,1,178,228,0,0,17,1,253,232,0,0,1, 
  15,1,178,228,0,0,17,1,202,232,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,178,228,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,178,228,0,0,1,19,3,0,0,0,13,0,0,0,1,0, 
  17,1,178,228,0,0,1,19,2,0,0,0,10,0,0,0,1,0,17,1,178,228,0,0,1,19,2,0,0,0,9,0,0,0,1,0,17,1,178,228,0,0,1,19,2,0,0,0,8,0,0,0,1,0,17,1,178,228,0,0, 
  1,19,2,0,0,0,7,0,0,0,1,0,17,1,178,228,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,178,228,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1,178,228,0,0,1,19,2,0,0,0, 
  4,0,0,0,1,0,17,1,178,228,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,178,228,0,0,1,19,1,0,0,0,1,0,0,0,1,0,17,1,178,228,0,0,1,15,1,178,228,0,0,17,1,44,230,0, 
  0,1,1,2,21,1,134,2,0,0,30,235,0,0,20,0,0,0,4,0,0,0,16,152,136,132,1,16,3,128,2,16,195,128,3,24,19,128,4,0,18,128,18,248,7,128,22,40,7,128,7,96,81,130,8,192,80,130,9,168, 
  15,128,10,144,78,130,11,120,13,128,12,8,12,128,13,240,10,128,14,216,9,128,15,56,9,128,23,88,6,128,24,96,5,128,32,104,4,128,42,24,3,128,8,4,19,43,0,0,0,119,0,0,0,1,0,19,31,0,0,0, 
  84,0,0,0,1,0,15,1,190,232,0,0,15,1,17,235,0,0,17,1,91,228,0,0,1,4,19,27,0,0,0,76,0,0,0,1,0,15,1,190,232,0,0,15,1,166,155,0,0,17,1,128,154,0,0,1,4,19,23,0, 
  0,0,57,0,0,0,1,0,15,1,190,232,0,0,15,1,166,155,0,0,17,1,77,154,0,0,1,4,15,1,190,232,0,0,15,1,234,226,0,0,15,1,246,226,0,0,17,1,230,224,0,0,1,4,15,1,190,232,0,0, 
  15,1,234,226,0,0,15,1,218,224,0,0,17,1,63,224,0,0,1,4,15,1,190,232,0,0,15,1,51,224,0,0,17,1,141,213,0,0,1,4,15,1,190,232,0,0,15,1,51,224,0,0,17,1,231,202,0,0,1,4, 
  15,1,190,232,0,0,15,1,219,202,0,0,17,1,50,168,0,0,1,4,19,9,0,0,0,31,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,9,0,0,0, 
  30,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,19,0,0,0,52,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0, 
  0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,9,0,0,0,28,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,9,0,0,0,27,0,0,0, 
  1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,9,0,0,0,26,0,0,0,1,0,19,2,0,0,0,6,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1, 
  4,15,1,190,232,0,0,15,1,38,168,0,0,17,1,249,167,0,0,1,4,15,1,190,232,0,0,15,1,51,224,0,0,17,1,240,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0, 
  0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,19,0,0,0,0,0,0,0,0,1, 
  0,1,19,1,0,0,0,2,0,0,0,2,0,1,21,1,39,0,0,0,60,23,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,6,240,0,128,4,17,1,15,6,0,0,1,8,19,31,0,0,0,83,0, 
  0,0,1,0,1,21,1,149,0,0,0,78,246,0,0,6,0,0,0,2,0,0,0,16,248,3,129,1,80,1,128,2,80,193,128,7,104,4,128,24,168,2,128,42,88,1,128,8,4,19,43,0,0,0,119,0,0,0,1,0, 
  19,31,0,0,0,84,0,0,0,1,0,15,1,147,233,0,0,15,1,51,150,0,0,17,1,250,140,0,0,1,4,19,23,0,0,0,57,0,0,0,1,0,19,31,0,0,0,83,0,0,0,1,0,15,1,147,233,0,0,15, 
  1,51,150,0,0,17,1,250,140,0,0,1,4,15,1,238,140,0,0,17,1,133,136,0,0,1,4,17,1,194,157,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,23,232,67,129,25,88,3, 
  128,10,232,2,128,43,144,1,128,28,136,2,128,31,240,2,128,6,232,2,128,7,32,66,126,19,31,0,0,0,84,0,0,0,1,0,17,1,147,233,0,0,1,15,1,147,233,0,0,17,1,104,161,0,0,1,19,10,0,0, 
  0,32,0,0,0,2,0,1,1,15,1,147,233,0,0,17,1,250,140,0,0,1,19,7,0,0,0,22,0,0,0,1,0,17,1,147,233,0,0,1,19,31,0,0,0,83,0,0,0,1,0,17,1,147,233,0,0,1,2,21, 
  1,75,0,0,0,10,24,1,0,4,0,0,0,2,0,0,0,34,16,1,128,1,80,2,128,2,80,130,127,35,176,1,128,4,15,1,122,234,0,0,15,1,8,31,0,0,17,1,19,30,0,0,1,4,15,1,122,234,0,0, 
  15,1,8,31,0,0,17,1,227,7,0,0,1,8,19,11,0,0,0,34,0,0,0,1,0,1,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,128,129,128,11,232,1,128,30,240,0,128,19,12,0,0, 
  0,35,0,0,0,1,0,17,1,122,234,0,0,1,15,1,122,234,0,0,17,1,185,234,0,0,1,1,2,21,1,63,0,0,0,10,24,1,0,4,0,0,0,2,0,0,0,34,136,1,128,1,16,1,128,2,16,129,127,35, 
  24,1,128,8,4,15,1,33,14,0,0,17,1,227,7,0,0,1,4,15,1,33,14,0,0,17,1,19,30,0,0,1,19,11,0,0,0,33,0,0,0,2,0,14,1,19,1,0,0,0,1,0,0,0,1,0,1,19,2,0, 
  0,0,8,0,0,0,1,0,1,13,21,4,103,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,16,66,128,64,184,2,128,91,128,2,129,35,248,194,127,92,208,1,128,45,144,1,128,123,72,2,128,95,144,1,128, 
  3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255, 
  255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237, 
  0,0,1,3,17,1,105,237,0,0,1,2,18,3,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,26,236,0,0,1,21,2,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,26,236,0,0,1,1,18,32,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,26,236,0,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,26,236,0,0,1,1,18,8,0,0,0,21,4,96,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,59, 
  32,2,129,45,88,2,128,34,200,2,128,43,144,66,127,91,232,65,128,95,168,65,128,123,112,1,128,3,18,15,0,0,0,1,3,17,1,232,236,0,0,1,3,18,23,0,0,0,1,3,18,11,0,0,0,1,3,18,10,0, 
  0,0,1,3,18,9,0,0,0,1,3,18,22,0,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,232,236,0,0,1,1,18,24,0,0,0,21,4,34,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,232,236,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,232,236,0,0, 
  1,1,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,32,8,65,128,110,208,0,128,3,18,14,0,0,0,1,3,18,13,0,0,0,1,24,3,17,1,99,237,0,0,1,18,4,0,0,0,1,18,3,0, 
  0,0,1,18,42,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,111,237,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,111,237,0,0,1,1,18,12,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,185,237,0,0,1,1,18,2,0,0,0,1,18, 
  1,0,0,0,1,21,4,114,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,104,66,128,64,16,3,128,91,216,2,129,35,80,195,127,92,40,2,128,45,176,193,128,123,160,2,128,95,176,1,128,125,240,1,128,3, 
  17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21, 
  2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0, 
  1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,40,131,128,61,168,66,130,64,208,3,128,35,16,68,129,92,232,2,128,45,48,2,127, 
  46,240,1,128,95,48,2,128,91,152,67,128,123,96,3,128,125,112,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3, 
  18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112, 
  1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,18,3,0,0,0, 
  21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,3,18,6,0,0,0,1,1,18,34,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,91,176,0,128,3,18,35, 
  0,0,0,1,1,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,64,136,1,128,45,16,1,128,34,80,1,128,95,16,1,128,3,17,1,111,237,0,0,1,3,18,21,0,0,0,1,3,17,1,69,240,0, 
  0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1, 
  3,17,1,111,237,0,0,1,2,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,34,48,1,128,45,240,64,128,95,240,0,128,3,17,1,232,236,0,0,1,3,18,22,0,0,0,1,21,2,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,232,236,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,3,17,1,225,240,0,0,1,21,2,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,233,240,0,0,1,24,3,18,20,0,0,0,1,18,20,0,0,0,1,21, 
  4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,225,240,0,0,1,3,18,21,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,16,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,233,240,0,0,1,21,4,245,0,0,0,255,255,255,255,20,0,0,0,4,0,0,0,32,112,135,128,59,40,197,128,64,232,4,128, 
  35,48,7,128,91,176,132,131,92,112,4,128,93,56,196,130,94,0,4,128,40,248,6,128,41,192,6,128,42,136,6,128,43,80,134,125,44,24,70,126,45,216,69,126,46,152,69,126,47,96,69,128,95,192,3,128,109,128,131,128, 
  123,72,3,128,125,16,3,128,3,18,5,0,0,0,1,3,18,7,0,0,0,1,3,17,1,228,242,0,0,1,3,17,1,111,237,0,0,1,3,18,29,0,0,0,1,3,18,19,0,0,0,1,3,17,1,50,237,0,0,1, 
  3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,18,47,0,0,0,1,3,18,28,0,0,0,1,3,17,1,166,239,0,0,1,3,17,1,154,242,0,0,1,3,18,33,0,0,0,1,3,18,26,0,0,0,1,3, 
  18,25,0,0,0,1,3,18,17,0,0,0,1,3,18,16,0,0,0,1,3,17,1,212,235,0,0,1,3,18,44,0,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1, 
  128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0,1,3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1, 
  105,237,0,0,1,2,18,27,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,111,237,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,237,0,0,1,1,18,42,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,97,48,1,128,3,17,1,111,237, 
  0,0,1,3,17,1,58,243,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,237,0,0,1,1,18,42,0,0,0,21,4,46,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,111,237,0,0,1,3,17,1,144,243,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3, 
  208,0,128,3,17,1,111,237,0,0,1,1,18,42,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,48,1,128,3,17,1,111,237,0,0,1,3,17,1,230,243, 
  0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,237,0,0,1,1,18,42,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,104,240,0,128,45,48,65,128,95,48,1,128,3,17,1,60,244,0,0,1,3,17,1,111,237,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,237, 
  0,0,1,1,18,41,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,111,237,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,111,237,0,0,1,1,18,45,0,0,0,1,18,43,0,0,0,1,21,4,96,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,46,24,66,129,45,88,66,129,42,200,130,127, 
  43,144,66,128,47,224,1,128,94,168,1,128,125,112,1,128,3,18,5,0,0,0,1,3,18,29,0,0,0,1,3,18,28,0,0,0,1,3,17,1,166,239,0,0,1,3,18,27,0,0,0,1,3,18,26,0,0,0,1,3, 
  18,25,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,2,21,4,96,0,0,0,255,255,255,255,7, 
  0,0,0,2,0,0,0,46,24,130,129,45,88,2,129,42,200,130,127,43,144,66,128,47,224,1,128,93,168,1,128,94,112,1,128,3,18,29,0,0,0,1,3,18,19,0,0,0,1,3,18,28,0,0,0,1,3,17,1,166, 
  239,0,0,1,3,18,27,0,0,0,1,3,18,26,0,0,0,1,3,18,25,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,227,237,0,0,1,3, 
  17,1,221,237,0,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,64,48,1,128,45,240,64,128,95,240,0,128,3,17,1,111,237,0,0,1,3,17,1,13,246,0,0,1,21,2,54,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,111,237,0,0,1,2,21,4,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,232,236,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,232,236,0,0,1,2,21,4,68,0,0,0, 
  255,255,255,255,5,0,0,0,2,0,0,0,40,112,129,128,45,48,1,128,64,168,1,128,95,48,65,128,123,232,1,128,3,17,1,111,237,0,0,1,3,18,16,0,0,0,1,3,17,1,13,246,0,0,1,3,18,7,0,0, 
  0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,111,237,0,0,1,2,21,4,126, 
  0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,200,66,128,64,112,3,128,91,56,131,129,35,176,195,127,92,136,2,128,45,16,66,128,109,208,193,128,95,16,2,128,123,0,3,128,125,80,2,128,3,17,1,150,247, 
  0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235, 
  0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1, 
  185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,18,42,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,97,48,1,128,3,17,1, 
  111,237,0,0,1,3,17,1,58,243,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,237,0,0,1,1,21,4,150,0,0,0,255,255,255,255,12,0, 
  0,0,3,0,0,0,40,136,131,128,61,200,2,130,64,48,4,128,35,112,68,129,92,8,3,128,45,80,2,127,46,16,2,128,95,80,2,128,91,248,131,128,109,72,131,128,123,192,3,128,125,144,2,128,3,17,1,166,239,0, 
  0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0, 
  0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227, 
  237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,40,67,128,64, 
  208,3,128,91,152,195,129,35,16,196,127,92,168,2,128,45,48,194,128,46,240,1,128,95,48,2,128,109,232,130,128,123,96,3,128,125,112,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0, 
  0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0, 
  0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1, 
  111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,183,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,32,68,130,41,40,3,128,59,144,4,130,35,64,197,127,92,160,3,128,45,176,194,128,46,112,2,128, 
  95,176,2,128,61,96,195,128,64,0,5,128,91,200,196,128,93,128,69,128,109,224,131,128,123,88,4,128,125,240,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,18,17,0,0, 
  0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,47,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0, 
  0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237, 
  0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,171,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,192,3,130,41,8, 
  3,128,59,48,196,129,35,224,196,127,92,64,3,128,45,144,66,129,46,80,2,128,95,144,2,128,64,160,4,128,91,104,196,128,93,32,69,128,109,128,131,128,123,248,3,128,125,208,2,128,3,17,1,166,239,0,0,1,3,17, 
  1,111,237,0,0,1,3,18,5,0,0,0,1,3,18,17,0,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,47,0,0,0,1,3,18,18, 
  0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1, 
  128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,126,0,0,0,255,255,255,255,10,0,0,0, 
  3,0,0,0,40,200,130,128,41,80,2,128,64,112,3,128,35,176,195,128,92,136,2,128,45,16,194,128,91,56,195,128,95,16,2,128,109,208,1,128,123,0,3,128,3,17,1,150,247,0,0,1,3,17,1,111,237,0,0,1, 
  3,18,17,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255, 
  255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0, 
  0,1,3,17,1,105,237,0,0,1,2,21,4,150,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,136,3,130,41,144,2,128,61,200,2,130,35,112,132,129,92,8,3,128,45,80,66,127,46,16,2,128,95,80,2, 
  128,64,48,4,128,91,248,131,128,109,72,3,128,123,192,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,17,0,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228, 
  242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4, 
  176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2, 
  21,4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,40,131,128,41,112,2,128,64,208,3,128,35,16,68,129,92,168,2,128,45,48,2,129,46,240,1,128,95,48,2,128,91,152,131,128,109,232,2,128,123,96, 
  3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,17,0,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0, 
  0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227, 
  237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,126,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,144,66,128,64, 
  56,3,128,91,0,195,129,35,120,195,127,92,80,2,128,45,16,66,128,93,184,131,128,95,16,2,128,109,208,1,128,123,200,2,128,3,17,1,150,247,0,0,1,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3, 
  18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0, 
  0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0, 
  1,2,21,4,150,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,80,131,128,61,144,2,130,64,248,3,128,35,56,68,129,92,208,2,128,45,80,2,127,46,16,2,128,95,80,2,128,91,192,195,128,93,120,68,128, 
  109,16,3,128,123,136,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0, 
  0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48, 
  130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,138,0,0,0,255,255,255, 
  255,11,0,0,0,3,0,0,0,40,240,66,128,64,152,3,128,91,96,3,130,35,216,195,127,92,112,2,128,45,48,194,128,46,240,1,128,95,48,2,128,93,24,68,128,109,176,2,128,123,40,3,128,3,17,1,166,239,0,0, 
  1,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0, 
  0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237, 
  0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,68,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,168,1,129,45,48,1,128,34,112,1,128,95,48, 
  1,128,64,224,1,128,3,17,1,111,237,0,0,1,3,18,21,0,0,0,1,3,18,16,0,0,0,1,3,17,1,69,240,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80, 
  1,128,6,16,1,128,3,208,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0, 
  0,64,80,1,128,45,144,1,128,34,208,1,128,95,16,1,128,3,17,1,203,1,1,0,1,3,17,1,69,240,0,0,1,3,17,1,111,237,0,0,1,3,18,21,0,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0, 
  0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,2,18,46,0,0,0, 
  21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,111,237,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128, 
  3,17,1,111,237,0,0,1,1,21,4,107,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,94,144,1,128,41,32,3,128,42,232,2,128,43,176,2,128,44,120,2,128,45,64,2,128,46,0,130,126,47,200,1,128,3, 
  18,29,0,0,0,1,3,18,28,0,0,0,1,3,17,1,166,239,0,0,1,3,18,27,0,0,0,1,3,18,33,0,0,0,1,3,18,26,0,0,0,1,3,18,25,0,0,0,1,3,18,17,0,0,0,1,21,2,42,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,2,21,4,90,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,40,32,2, 
  129,45,112,65,129,34,232,1,128,95,112,129,128,64,88,2,128,123,152,2,128,125,176,1,128,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,18,21,0,0,0,1,3,18,16,0,0,0,1,3,17,1,69,240,0, 
  0,1,3,18,7,0,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3, 
  17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,2,21,4,137,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,232,130,128,59,88,67,129,64,200,3,128,35,8,132,127,92,168,2,128,45,48,194,128,91,144, 
  195,128,95,48,2,128,109,240,129,128,123,32,3,128,125,112,2,128,3,17,1,150,247,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0, 
  0,0,1,3,18,47,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2, 
  48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,161,0,0,0,255,255, 
  255,255,13,0,0,0,3,0,0,0,40,168,3,130,59,24,4,130,61,232,2,130,35,200,132,127,92,40,3,128,45,112,66,127,46,48,2,128,95,112,2,128,64,136,4,128,91,80,132,128,109,104,131,128,123,224,3,128,125,176, 
  2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0, 
  0,0,1,3,18,47,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2, 
  48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,149,0,0,0,255,255, 
  255,255,12,0,0,0,3,0,0,0,40,72,131,128,59,184,195,129,64,40,4,128,35,104,132,127,92,200,2,128,45,80,2,129,46,16,2,128,95,80,2,128,91,240,131,128,109,8,131,128,123,128,3,128,125,144,2,128,3,17, 
  1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237,0,0,1,3,17,1,228,242,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,47,0,0,0,1,3,18, 
  18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3, 
  17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,96,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,46,24, 
  66,129,45,88,2,128,42,200,130,127,43,144,66,128,47,224,129,128,94,168,1,128,123,112,1,128,3,18,7,0,0,0,1,3,18,29,0,0,0,1,3,18,28,0,0,0,1,3,17,1,166,239,0,0,1,3,18,27,0,0, 
  0,1,3,18,26,0,0,0,1,3,18,25,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,2,21, 
  4,68,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,64,224,1,128,41,112,193,128,34,168,1,128,95,48,1,128,45,48,1,128,3,17,1,111,237,0,0,1,3,18,17,0,0,0,1,3,18,21,0,0,0,1,3, 
  17,1,69,240,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1, 
  185,237,0,0,1,3,17,1,111,237,0,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,110,176,1,128,105,240,1,128,102,48,130,127,111,112,65,128,115,48,1,128,3,17,1,115,8,1,0,1, 
  3,17,1,54,8,1,0,1,3,17,1,249,7,1,0,1,3,17,1,188,7,1,0,1,3,17,1,127,7,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17, 
  1,227,237,0,0,1,3,17,1,221,237,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,158,7,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,116,176,0,128,3,18,39,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,219,7,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,116,176,0,128,3,18,40,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,24,8,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,109,176,0,128,3,18,37,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,17,1,85,8,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,106,176,0,128,3,18,36,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,146,8,1,0,1,2,21,4,29,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,114,176,0,128,3,18,38,0,0,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,64,80,1,128,45,16,1,128,123,144,1,128,95,16,193,127,3,17,1,111,237,0, 
  0,1,3,17,1,13,246,0,0,1,3,18,7,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237, 
  0,0,1,3,17,1,111,237,0,0,1,2,21,4,160,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,160,3,130,41,232,2,128,61,32,3,130,35,136,132,129,92,96,3,128,45,112,66,127,46,48,2,128,95,112, 
  2,128,64,72,4,128,91,16,132,128,93,200,132,128,123,216,3,128,125,176,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,18,17,0,0,0,1,3,17,1,131,239,0,0,1, 
  3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255, 
  255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0, 
  0,1,3,17,1,105,237,0,0,1,2,21,4,148,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,64,131,128,41,200,2,128,64,232,3,128,35,40,68,129,92,0,3,128,45,80,2,129,46,16,2,128,95,80,2, 
  128,91,176,131,128,93,104,132,128,123,120,3,128,125,144,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,18,17,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0, 
  0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1, 
  128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4, 
  126,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,200,66,128,64,112,3,128,91,56,131,129,35,176,195,127,92,136,2,128,45,16,2,129,46,208,1,128,95,16,2,128,123,0,3,128,125,80,2,128,3,17,1,166, 
  239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212, 
  235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17, 
  1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1, 
  3,18,2,0,0,0,1,3,17,1,185,237,0,0,1,2,21,4,114,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,32,48,66,128,40,104,66,128,64,216,2,128,35,80,195,128,92,240,1,128,45,176,1,128,91,24, 
  131,128,95,176,1,128,123,160,2,128,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,18,44,0,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,17,1,193,12,1,0,1,3,18,18,0,0, 
  0,1,3,17,1,212,235,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1,128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0,1, 
  3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,18,8,0,0,0,21,4,85,0,0,0,255,255,255,255,6,0,0,0, 
  2,0,0,0,59,200,1,129,45,0,2,128,34,112,2,128,43,56,66,127,91,144,65,128,95,80,1,128,3,17,1,232,236,0,0,1,3,18,23,0,0,0,1,3,18,11,0,0,0,1,3,18,10,0,0,0,1,3,18,9, 
  0,0,0,1,3,18,22,0,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,232,236,0,0,1,1,21,4,125,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0, 
  32,136,66,128,40,192,66,128,64,48,3,128,35,168,195,128,92,72,2,128,45,208,1,129,91,112,131,128,95,208,1,128,123,248,2,128,125,16,2,128,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237, 
  0,0,1,3,18,44,0,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,17,1,193,12,1,0,1,3,18,18,0,0,0,1,3,17,1,212,235,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0, 
  0,2,0,0,0,4,16,2,128,5,208,1,128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0,1,3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0, 
  1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,137,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,32,232,66,128,40,32,67,128,64,144,3,128,35,8,68,129,92,168,2,128,45,48,66,129, 
  46,240,1,128,95,48,2,128,91,208,67,128,123,88,3,128,125,112,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,5,0,0,0,1,3,17,1,50,237,0,0,1,3,18,44,0,0,0,1,3,18, 
  16,0,0,0,1,3,18,7,0,0,0,1,3,17,1,193,12,1,0,1,3,18,18,0,0,0,1,3,17,1,212,235,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1, 
  128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0,1,3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1, 
  105,237,0,0,1,2,21,4,125,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,32,136,130,128,41,16,2,128,40,192,2,129,35,168,67,129,92,72,2,128,45,208,1,128,64,48,3,128,95,208,1,128,91,112,67,128, 
  123,248,2,128,3,17,1,111,237,0,0,1,3,18,17,0,0,0,1,3,17,1,50,237,0,0,1,3,18,44,0,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,17,1,193,12,1,0,1,3,18,18,0, 
  0,0,1,3,17,1,212,235,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1,128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0, 
  1,3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,137,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0, 
  32,232,130,128,41,112,2,128,40,32,131,129,35,8,132,129,92,168,2,128,45,48,2,128,46,240,1,128,95,48,2,128,64,144,3,128,91,208,67,128,123,88,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1, 
  3,18,17,0,0,0,1,3,17,1,50,237,0,0,1,3,18,44,0,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,17,1,193,12,1,0,1,3,18,18,0,0,0,1,3,17,1,212,235,0,0,1,21, 
  2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1,128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0,1,3,17,1,227,237,0,0,1,3,17,1, 
  134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,125,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,32,80,66,128,40,136,66,128,64,248,2,128, 
  35,112,195,128,92,16,2,128,45,208,193,128,91,56,195,128,95,208,1,128,93,176,3,128,123,192,2,128,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,18,44,0,0,0,1,3,18,16,0,0,0,1,3,18, 
  7,0,0,0,1,3,17,1,193,12,1,0,1,3,18,18,0,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1, 
  128,2,144,130,128,3,80,130,128,6,144,1,128,7,80,1,128,3,17,1,140,244,0,0,1,3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1, 
  105,237,0,0,1,2,21,4,137,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,32,176,66,128,40,232,66,128,64,88,3,128,35,208,67,129,92,112,2,128,45,48,2,129,46,240,1,128,95,48,2,128,91,152,131,128, 
  93,16,4,128,123,32,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,18,44,0,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,17,1,193,12,1,0, 
  1,3,18,18,0,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,4,16,2,128,5,208,1,128,2,144,130,128,3,80,130,128,6,144,1, 
  128,7,80,1,128,3,17,1,140,244,0,0,1,3,17,1,227,237,0,0,1,3,17,1,134,244,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,114,0,0,0, 
  255,255,255,255,9,0,0,0,3,0,0,0,40,104,130,128,41,240,1,128,64,16,3,128,35,80,195,128,92,40,2,128,45,176,1,128,91,216,130,128,95,176,1,128,123,160,2,128,3,17,1,111,237,0,0,1,3,18,17,0, 
  0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17, 
  1,105,237,0,0,1,2,21,4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,40,3,130,41,112,2,128,61,168,2,128,35,16,132,129,92,232,2,128,45,48,66,127,46,240,1,128,95,48,2,128,64,208,3, 
  128,91,152,67,128,123,96,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18,17,0,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0, 
  0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6, 
  48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,126,0,0,0,255,255,255,255,10,0,0,0,3,0, 
  0,0,40,200,130,128,41,80,2,128,64,112,3,128,35,176,67,129,92,136,2,128,45,16,2,128,46,208,1,128,95,16,2,128,91,56,67,128,123,0,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,18, 
  17,0,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1, 
  3,17,1,105,237,0,0,1,2,21,4,114,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,48,66,128,64,216,2,128,91,160,130,129,35,24,195,127,92,240,1,128,45,176,65,128,93,88,3,128,95,176,1,128,123, 
  104,2,128,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0, 
  0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1, 
  185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,240,130,128,61,112,2,130,64,152,3,128,35,216,67,129,92,176,2,128, 
  45,48,2,127,46,240,1,128,95,48,2,128,91,96,131,128,93,24,4,128,123,40,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0, 
  0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18,19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176, 
  1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21, 
  4,126,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,144,66,128,64,56,3,128,91,0,195,129,35,120,195,127,92,80,2,128,45,16,194,128,46,208,1,128,95,16,2,128,93,184,3,128,123,200,2,128,3,17,1, 
  166,239,0,0,1,3,17,1,111,237,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,3,18, 
  19,0,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3, 
  17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,127,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,208,130,128,61,80,2,128,64,120,3,128,35,184,67,129,92,144, 
  2,128,45,16,2,127,46,208,1,128,95,16,2,128,91,64,67,128,123,8,3,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1,3,17,1,131,239,0,0,1,3,17,1,50,237,0,0,1,3,18,16,0,0,0, 
  1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130, 
  128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237,0,0,1,2,21,4,115,0,0,0,255,255,255,255, 
  9,0,0,0,3,0,0,0,40,112,66,128,64,24,3,128,91,224,130,129,35,88,195,127,92,48,2,128,45,240,1,128,46,176,1,128,95,240,1,128,123,168,2,128,3,17,1,166,239,0,0,1,3,17,1,111,237,0,0,1, 
  3,17,1,50,237,0,0,1,3,18,16,0,0,0,1,3,18,7,0,0,0,1,3,18,18,0,0,0,1,3,17,1,100,236,0,0,1,3,17,1,212,235,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,227,237,0,0,1,3,17,1,221,237,0,0,1,3,17,1,185,237,0,0,1,3,17,1,111,237,0,0,1,3,17,1,105,237, 
  0,0,1,2, 
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