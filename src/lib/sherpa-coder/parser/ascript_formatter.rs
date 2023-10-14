
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
  Dedent(Box<Dedent>),
  NewLine(Box<NewLine>),
  Match(Box<Match>),
  Literal(Box<Literal>),
  StringType(Box<StringType>),
  Param(Box<Param>),
  ObjType(Box<ObjType>),
  Call(Box<Call>),
  Expression(Box<Expression>),
  Text(Box<Text>),
  Funct(Box<Funct>),
  Space(Box<Space>),
  Mul(Box<Mul>),
  Div(Box<Div>),
  IntType(Box<IntType>),
  FBlock(Box<FBlock>),
  Add(Box<Add>),
  Obj(Box<Obj>),
  Sub(Box<Sub>),
  SBlock(Box<SBlock>),
  MatchArm(Box<MatchArm>),
  NumType(Box<NumType>),
  Num(Box<Num>),
  For(Box<For>),
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
  Dedent,
  NewLine,
  Match,
  Literal,
  StringType,
  Param,
  ObjType,
  Call,
  Expression,
  Text,
  Funct,
  Space,
  Mul,
  Div,
  IntType,
  FBlock,
  Add,
  Obj,
  Sub,
  SBlock,
  MatchArm,
  NumType,
  Num,
  For,
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
      ASTNode::Call(node) => node.tok.clone(),
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
      ASTNode::Dedent(..) => ASTNodeType::Dedent,
      ASTNode::NewLine(..) => ASTNodeType::NewLine,
      ASTNode::Match(..) => ASTNodeType::Match,
      ASTNode::Literal(..) => ASTNodeType::Literal,
      ASTNode::StringType(..) => ASTNodeType::StringType,
      ASTNode::Param(..) => ASTNodeType::Param,
      ASTNode::ObjType(..) => ASTNodeType::ObjType,
      ASTNode::Call(..) => ASTNodeType::Call,
      ASTNode::Expression(..) => ASTNodeType::Expression,
      ASTNode::Text(..) => ASTNodeType::Text,
      ASTNode::Funct(..) => ASTNodeType::Funct,
      ASTNode::Space(..) => ASTNodeType::Space,
      ASTNode::Mul(..) => ASTNodeType::Mul,
      ASTNode::Div(..) => ASTNodeType::Div,
      ASTNode::IntType(..) => ASTNodeType::IntType,
      ASTNode::FBlock(..) => ASTNodeType::FBlock,
      ASTNode::Add(..) => ASTNodeType::Add,
      ASTNode::Obj(..) => ASTNodeType::Obj,
      ASTNode::Sub(..) => ASTNodeType::Sub,
      ASTNode::SBlock(..) => ASTNodeType::SBlock,
      ASTNode::MatchArm(..) => ASTNodeType::MatchArm,
      ASTNode::NumType(..) => ASTNodeType::NumType,
      ASTNode::Num(..) => ASTNodeType::Num,
      ASTNode::For(..) => ASTNodeType::For,
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
      Dedent(node) => node.hash(hasher),
      NewLine(node) => node.hash(hasher),
      Match(node) => node.hash(hasher),
      Literal(node) => node.hash(hasher),
      StringType(node) => node.hash(hasher),
      Param(node) => node.hash(hasher),
      ObjType(node) => node.hash(hasher),
      Call(node) => node.hash(hasher),
      Expression(node) => node.hash(hasher),
      Text(node) => node.hash(hasher),
      Funct(node) => node.hash(hasher),
      Space(node) => node.hash(hasher),
      Mul(node) => node.hash(hasher),
      Div(node) => node.hash(hasher),
      IntType(node) => node.hash(hasher),
      FBlock(node) => node.hash(hasher),
      Add(node) => node.hash(hasher),
      Obj(node) => node.hash(hasher),
      Sub(node) => node.hash(hasher),
      SBlock(node) => node.hash(hasher),
      MatchArm(node) => node.hash(hasher),
      NumType(node) => node.hash(hasher),
      Num(node) => node.hash(hasher),
      For(node) => node.hash(hasher),
      
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
}

impl Indent{
  
  pub fn new ()-> Self {
    
    Self{
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
pub struct Dedent{
}

impl Dedent{
  
  pub fn new ()-> Self {
    
    Self{
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
}

impl NewLine{
  
  pub fn new ()-> Self {
    
    Self{
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
  pub matches:Vec<Box<MatchArm>>, 
  pub obj:Box<Obj>, 
}

impl Match{
  
  pub fn new (matches: Vec<Box<MatchArm>>, obj: Box<Obj>)-> Self {
    
    Self{
      matches,
      obj,
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
    
    for val in &self.matches{
      val.hash(hasher);
    }
    self.obj.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Literal{
  pub val:String, 
}

impl Literal{
  
  pub fn new (val: String)-> Self {
    
    Self{
      val,
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
}

impl StringType{
  
  pub fn new ()-> Self {
    
    Self{
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
  pub name:String, 
  pub ty:String, 
}

impl Param{
  
  pub fn new (name: String, ty: String)-> Self {
    
    Self{
      name,
      ty,
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
}

impl ObjType{
  
  pub fn new ()-> Self {
    
    Self{
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
}

impl Expression{
  
  pub fn new (val: ASTNode)-> Self {
    
    Self{
      val,
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
pub struct Text{
  pub val:String, 
}

impl Text{
  
  pub fn new (val: String)-> Self {
    
    Self{
      val,
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
}

impl Funct{
  
  pub fn new (content: Vec<ASTNode>, name: String, params: Vec<Box<Param>>)-> Self {
    
    Self{
      content,
      name,
      params,
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
}

impl Space{
  
  pub fn new (count: u32)-> Self {
    
    Self{
      count,
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
pub struct Mul{
  pub l:ASTNode, 
  pub r:ASTNode, 
}

impl Mul{
  
  pub fn new (l: ASTNode, r: ASTNode)-> Self {
    
    Self{
      l,
      r,
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
}

impl Div{
  
  pub fn new (l: ASTNode, r: ASTNode)-> Self {
    
    Self{
      l,
      r,
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
}

impl IntType{
  
  pub fn new ()-> Self {
    
    Self{
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
pub struct FBlock{
  pub content:Vec<ASTNode>, 
  pub ty:String, 
}

impl FBlock{
  
  pub fn new (content: Vec<ASTNode>, ty: String)-> Self {
    
    Self{
      content,
      ty,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::FBlock
  }
}

impl ASTNode{
  
  pub fn to_FBlock (self)-> Box::<FBlock> {
    
    match self{
      Self::FBlock(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_FBlock (&self)-> Option<&FBlock> {
    
    match self{
      Self::FBlock(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_FBlock_mut (&mut self)-> Option<&mut FBlock> {
    
    match self{
      Self::FBlock(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for FBlock{
  
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
pub struct Add{
  pub l:ASTNode, 
  pub r:ASTNode, 
}

impl Add{
  
  pub fn new (l: ASTNode, r: ASTNode)-> Self {
    
    Self{
      l,
      r,
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
  pub name:String, 
  pub path:Vec<String>, 
}

impl Obj{
  
  pub fn new (name: String, path: Vec<String>)-> Self {
    
    Self{
      name,
      path,
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
    self.name.hash(hasher);
    self.path.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Sub{
  pub l:ASTNode, 
  pub r:ASTNode, 
}

impl Sub{
  
  pub fn new (l: ASTNode, r: ASTNode)-> Self {
    
    Self{
      l,
      r,
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
pub struct SBlock{
  pub content:Option<ASTNode>, 
  pub ty:String, 
}

impl SBlock{
  
  pub fn new (content: Option<ASTNode>, ty: String)-> Self {
    
    Self{
      content,
      ty,
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
    self.content.hash(hasher);
    self.ty.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct MatchArm{
  pub content:Vec<ASTNode>, 
  pub default:bool, 
  pub match_expr:Option<ASTNode>, 
}

impl MatchArm{
  
  pub fn new (content: Vec<ASTNode>, default: bool, match_expr: Option<ASTNode>)-> Self {
    
    Self{
      content,
      default,
      match_expr,
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
}

impl NumType{
  
  pub fn new ()-> Self {
    
    Self{
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
}

impl Num{
  
  pub fn new (val: String)-> Self {
    
    Self{
      val,
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

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct For{
  pub content:Vec<ASTNode>, 
  pub local:Token, 
  pub object:Box<Obj>, 
}

impl For{
  
  pub fn new (content: Vec<ASTNode>, local: Token, object: Box<Obj>)-> Self {
    
    Self{
      content,
      local,
      object,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::For
  }
}

impl ASTNode{
  
  pub fn to_For (self)-> Box::<For> {
    
    match self{
      Self::For(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_For (&self)-> Option<&For> {
    
    match self{
      Self::For(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_For_mut (&mut self)-> Option<&mut For> {
    
    match self{
      Self::For(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for For{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.content{
      val.hash(hasher);
    }
    self.local.to_string().replace(" ", "").replace("\n", "").hash(hasher);
    self.object.hash(hasher);
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


/* for */
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


/* block::<t_SBlock, script_statement> */
fn reducer_009 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( tk:(c:id+) | c:sym ) 
                                    :ast { t_Text, val: str(tok) } */
fn reducer_010 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_token(unsafe{&mut *_ctx_}.get_reader_mut());
  let tok_rule_0 = tok_rule_0.to_string();
  let var_2_0 = Text::new(
    tok_rule_0,
  );
  slots.assign(0, AstSlot(ASTNode::Text(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( tk:(c:id+) | c:sym ) 
                                    :ast { t_Text, val: str(tok) } */
fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_token(unsafe{&mut *_ctx_}.get_reader_mut());
  let tok_rule_0 = tok_rule_0.to_string();
  let var_2_0 = Text::new(
    tok_rule_0,
  );
  slots.assign(0, AstSlot(ASTNode::Text(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" "match" ) 
                                    :ast { t_Text, val: str(tok<1>) } */
fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.trim(1, 0);
  let tok_rule_0 = tok_rule_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_2_0 = Text::new(
    tok_rule_0,
  );
  slots.assign(0, AstSlot(ASTNode::Text(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* num */
fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
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


/* tk:(c:id+) */
fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4 } */
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
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4 } */
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
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4 } */
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
  );
  slots.assign(0, AstSlot(ASTNode::Funct(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name param* '{' function_statement* "}"{kw}
                                    
                                    :ast { t_Funct, name: str($1), params:$2, content: $4 } */
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


/* tk:( "@" "+"{:9999})          :ast { t_Indent } */
fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Indent::new();
  slots.assign(0, AstSlot(ASTNode::Indent(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" "-"{:9999})          :ast { t_Dedent } */
fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Dedent::new();
  slots.assign(0, AstSlot(ASTNode::Dedent(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "@" tk:(c:num+)               :ast { t_Space, count: u32($2) } */
fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_3_0 = Space::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Space(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" " "{:9999})         :ast { t_Space } */
fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Space::new(
    0,
  );
  slots.assign(0, AstSlot(ASTNode::Space(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( "\\" "n"{:9999})         :ast { t_NewLine } */
fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NewLine::new();
  slots.assign(0, AstSlot(ASTNode::NewLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* fn_name args              :ast { t_Call, name: str($1), args: $2, tok } */
fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "#for" id ":" object "{" function_statement+ "}"
                                    :ast { t_For, local: $2, object: $4, content: $6 } */
fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  slots.take(4);
  let AstSlot (ref_5, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_6, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_6;
  let obj_5_0 = ref_5.into_nodes();
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_3_2 = ref_3;
  let obj_3_2 = obj_3_2.to_Obj();
  let var_8_0 = For::new(
    obj_5_0,
    tok_1_1,
    obj_3_2,
  );
  slots.assign(0, AstSlot(ASTNode::For(Box::new(var_8_0)), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* id ("." var :ast str($2) )*            
                                    :ast { t_Obj, name: str($1), path:$2 } */
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_1_1 = ref_1.into_strings();
  let var_3_0 = Obj::new(
    tok_0_0,
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Obj(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* id ("." var :ast str($2) )*            
                                    :ast { t_Obj, name: str($1), path:$2 } */
fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_2_0 = Obj::new(
    tok_0_0,
    Default::default(),
  );
  slots.assign(0, AstSlot(ASTNode::Obj(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "." var :ast str($2) */
fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* ("." var :ast str($2) ) */
fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_string();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ("." var :ast str($2) )* */
fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1.to_string();
  let mut obj_0_0 = ref_0.into_strings();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    Some(obj_1_0),
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    None,
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    Some(obj_1_0),
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    None,
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = SBlock::new(
    Some(obj_1_0),
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = SBlock::new(
    None,
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::SBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* tk:( c:num+ ) 
                                    :ast { t_Num, val: str(tok) } */
fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_token(unsafe{&mut *_ctx_}.get_reader_mut());
  let tok_rule_0 = tok_rule_0.to_string();
  let var_2_0 = Num::new(
    tok_rule_0,
  );
  slots.assign(0, AstSlot(ASTNode::Num(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "{{"  c:any+  "}}"     :ast { t_Literal, val: str($2) } */
fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_tokens();
  let obj_1_0 = (obj_1_0.first().unwrap() + obj_1_0.last().unwrap()).to_string();
  let var_4_0 = Literal::new(
    obj_1_0,
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


/* "[["  expr  "]]"              :ast { t_Expression, val: $2 } */
fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let var_4_0 = Expression::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Expression(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* id ":" type              
                                    :ast { t_Param, name: str($1), ty: str($3) } */
fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_2_1 = __tok_rng_2;
  let tok_2_1 = tok_2_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = Param::new(
    tok_0_0,
    tok_2_1,
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


/* for */
fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* block::<t_FBlock, function_statement+> */
fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:( "#" ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) */
fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "(" expr(*",") ")" 
                                    :ast { [$2] } */
fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "(" expr(*",") ")" 
                                    :ast { [$2] } */
fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = vec![];;
  slots.assign(0, AstSlot(ASTNode::NODES(obj_3_0), __rule_rng__, TokenRange::default()));
}


/* expr */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expr(*",") */
fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:( "@" ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:( ( c:id | "_" | '-' ) ( c:id | "_" | '-' | c:num )* ) */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expr "+"{1} expr{1}           :ast { t_Add, l:$1, r:$3 } */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Add::new(
    obj_0_0,
    obj_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Add(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "-"{1} expr{1}           :ast { t_Sub, l:$1, r:$3 } */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Sub::new(
    obj_0_0,
    obj_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Sub(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "*"{3} expr{3}           :ast { t_Mul, l:$1, r:$3 } */
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Mul::new(
    obj_0_0,
    obj_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Mul(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "/"{2} expr{2}           :ast { t_Div, l:$1, r:$3 } */
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = Div::new(
    obj_0_0,
    obj_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Div(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* num */
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* object */
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "obj"                         :ast { t_ObjType } */
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = ObjType::new();
  slots.assign(0, AstSlot(ASTNode::ObjType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "num"                         :ast { t_NumType } */
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = NumType::new();
  slots.assign(0, AstSlot(ASTNode::NumType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "int"                         :ast { t_IntType } */
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = IntType::new();
  slots.assign(0, AstSlot(ASTNode::IntType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "str"                         :ast { t_StringType } */
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = StringType::new();
  slots.assign(0, AstSlot(ASTNode::StringType(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "match" object "{"  match_arm*  "}" 
                                    :ast { t_Match, obj: $2, matches:$4 } */
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_3_0 = ref_3.into_nodes();
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Obj();
  let var_6_0 = Match::new(
    obj_3_0.into_iter().map(|v|match v { ASTNode::MatchArm(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "match" object "{"  match_arm*  "}" 
                                    :ast { t_Match, obj: $2, matches:$4 } */
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Obj();
  let var_5_0 = Match::new(
    vec![],
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* match_arm */
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* match_arm* */
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = FBlock::new(
    obj_1_0,
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::FBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "[" Content? "]"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = FBlock::new(
    vec![],
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::FBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = FBlock::new(
    obj_1_0,
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::FBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "(" Content? ")"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = FBlock::new(
    vec![],
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::FBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_4_0 = FBlock::new(
    obj_1_0,
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::FBlock(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" Content? "}"            :ast { t_T, ty: str($1), content: $2 } */
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_0_1 = __tok_rng_0;
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = FBlock::new(
    vec![],
    tok_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::FBlock(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* function_statement+ */
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* expr "{" ( function_statement )+{2} "}"
                                    :ast { t_MatchArm, match_expr: $1, content: $3 } */
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( function_statement )+{2} ";"? "}"
                                    :ast { t_MatchArm, default: true, content: $2 } */
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( function_statement )+{2} ";"? "}"
                                    :ast { t_MatchArm, default: true, content: $2 } */
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
  );
  slots.assign(0, AstSlot(ASTNode::MatchArm(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( function_statement ) */
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( function_statement )+ */
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* function_statement */
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( function_statement ) */
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( function_statement )+ */
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 102]
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
    "for",
    "for_list",
    "object",
    "object_group",
    "object_list_1",
    "block_template_17066716807271665512",
    "num",
    "literal",
    "literal_list",
    "expression",
    "param",
    "function_statement",
    "fn_name",
    "args",
    "args_list",
    "id",
    "var",
    "expr",
    "type",
    "match",
    "match_list",
    "block_template_4651019715736341218",
    "block_template_4651019715736341218_list",
    "block_template_4651019715736341218_list_1",
    "block_template_4651019715736341218_list_2",
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
    r####"tk:nonterm"####,
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
    r####" : "####,
    r####" #for "####,
    r####" . "####,
    r####" ( "####,
    r####" ) "####,
    r####" [ "####,
    r####" ] "####,
    r####"nonterm"####,
    r####"c:any"####,
    r####" {{ "####,
    r####" }} "####,
    r####" [[ "####,
    r####" ]] "####,
    r####"tk:nonterm"####,
    r####" , "####,
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

pub static bytecode: [u8; 27514] = [
  0,211,200,197,210,208,193,2,15,1,79,90,0,0,17,1,21,0,0,0,1,21,1,178,2,0,0,80,90,0,0,19,0,0,0,4,0,0,0,17,184,7,128,1,240,194,127,2,240,2,128,3,72,212,130,4,0,19,128,5, 
  184,17,128,6,232,16,128,23,232,5,128,8,24,16,128,9,208,206,129,10,136,13,128,11,232,139,129,12,160,10,128,13,88,73,129,19,232,6,128,15,136,8,128,25,232,4,128,27,32,4,128,29,248,2,128,8,4,19,25,0, 
  0,0,64,0,0,0,1,0,15,1,201,85,0,0,15,1,55,90,0,0,15,1,67,90,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,201,85,0,0,17,1,220,83,0,0,1,4,15, 
  1,201,85,0,0,15,1,55,90,0,0,15,1,196,83,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,201,85,0,0,15,1,55,90,0,0,15,1,196,83,0,0,15,1,101,82,0,0,17,1,202,81,0, 
  0,1,4,15,1,201,85,0,0,15,1,55,90,0,0,15,1,190,81,0,0,17,1,61,79,0,0,1,4,15,1,201,85,0,0,15,1,55,90,0,0,15,1,190,81,0,0,17,1,188,76,0,0,1,4,15,1,201,85,0, 
  0,15,1,55,90,0,0,15,1,176,76,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85,0,0, 
  1,4,19,8,0,0,0,29,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13, 
  0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0, 
  0,0,1,0,0,0,1,0,17,1,201,85,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85,0,0,1,4,15,1,201, 
  85,0,0,15,1,55,90,0,0,15,1,20,69,0,0,17,1,231,68,0,0,1,4,15,1,201,85,0,0,15,1,55,90,0,0,15,1,190,81,0,0,17,1,200,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0, 
  19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0, 
  0,1,0,17,1,201,85,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85,0,0,1,2,21,1,65,2,0,0,119,94, 
  0,0,20,0,0,0,4,0,0,0,17,72,7,128,1,16,195,127,2,16,3,128,3,24,209,130,4,40,16,128,5,56,15,128,6,152,14,128,7,48,78,130,8,144,13,128,9,160,12,130,10,176,11,128,11,104,202,129,12,120, 
  9,128,13,136,136,129,19,168,6,128,15,232,7,128,23,216,5,128,25,8,5,128,27,16,4,128,29,24,3,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,168,68,0,0,15,1,67,90,0,0,17,1,157,84,0, 
  0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,168,68,0,0,15,1,209,66,0,0,17,1,10,5,0,0,1,4,15,1,168,68,0,0,15,1,196,83,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4, 
  15,1,168,68,0,0,15,1,196,83,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,168,68,0,0,15,1,190,81,0,0,17,1,61,79,0,0,1,4,15,1,168,68,0,0,15,1,190,81,0,0,17,1, 
  188,76,0,0,1,4,15,1,168,68,0,0,15,1,176,76,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,168,68,0,0,1,4,19,8,0,0, 
  0,29,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,168,68,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17, 
  1,168,68,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,168,68,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17, 
  1,168,68,0,0,1,4,15,1,168,68,0,0,15,1,20,69,0,0,17,1,231,68,0,0,1,4,19,15,0,0,0,45,0,0,0,2,0,1,4,15,1,168,68,0,0,15,1,190,81,0,0,17,1,200,2,0,0,1,4, 
  19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,168,68,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,168,68,0,0,1,4, 
  19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,168,68,0,0,1,2,21,1,92,0,0,0,42,95,0,0,5,0,0,0,2,0,0,0,6,160,2,128,1,48,129,128,2,48,129,127, 
  17,48,66,128,29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,96,59,0,0,15,1,197,66,0,0,17,1,154,58,0,0,1,4,15,1,142,58,0,0,17,1,99,55,0,0,1,4,17,1,103,5,0, 
  0,1,2,21,1,237,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,40,9,128,1,48,195,127,2,48,3,128,3,32,214,130,4,216,20,128,5,144,19,128,6,192,18,128,7,88,82,130,8,136,17,131,9,64,16, 
  130,10,248,14,128,11,88,205,129,12,16,12,128,13,200,138,129,19,88,8,128,15,248,9,128,23,88,7,128,25,88,6,128,27,48,5,128,29,8,4,128,40,56,3,128,8,4,15,1,127,51,0,0,15,1,75,55,0,0,15, 
  1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,127,51,0,0,15,1,75,55,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1, 
  0,15,1,127,51,0,0,15,1,75,55,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,127,51,0,0,15,1,75,55,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15, 
  1,127,51,0,0,15,1,75,55,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,127,51,0,0,15,1,75,55,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,127, 
  51,0,0,15,1,75,55,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,127,51,0,0,15,1,75,55,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0, 
  19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,24,0,0, 
  0,1,0,17,1,127,51,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0, 
  0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0, 
  53,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0,0,1,4,15,1,127,51,0,0,15,1,75,55,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,5,0,0,0,21,0,0,0, 
  3,0,1,4,15,1,127,51,0,0,15,1,75,55,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0, 
  0,1,0,17,1,127,51,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0,0,1,4,19,3,0,0,0,10,0,0, 
  0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0,0,1,2,21,1,237,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,40,9,128,1,48,195,127,2, 
  48,3,128,3,32,214,130,4,216,20,128,5,144,19,128,6,192,18,128,7,88,82,130,8,136,17,131,9,64,16,130,10,248,14,128,11,88,205,129,12,16,12,128,13,200,138,129,19,88,8,128,15,248,9,128,23,88,7,128,25, 
  88,6,128,27,48,5,128,29,8,4,128,40,56,3,128,8,4,15,1,67,11,0,0,15,1,10,15,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,67,11,0,0, 
  15,1,10,15,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,67,11,0,0,15,1,10,15,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,67, 
  11,0,0,15,1,10,15,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,67,11,0,0,15,1,10,15,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1, 
  4,15,1,67,11,0,0,15,1,10,15,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,67,11,0,0,15,1,10,15,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,67,11,0,0,15, 
  1,10,15,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,4, 
  19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0, 
  0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0, 
  91,0,0,0,1,0,17,1,67,11,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,4,15,1,67,11,0, 
  0,15,1,10,15,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,31,0,0,0,86,0,0,0,2,0,1,4,15,1,67,11,0,0,15,1,10,15,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4, 
  19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0, 
  0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0, 
  0,1,2,21,0,55,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,152,8,130,9,8,8,130,10,120,199,130,3,40,201,129,12,232,6,128,21,168,196,129,22,64,4,128,31,184,2,128,16,88,6,128,17,200,133, 
  128,19,56,5,128,25,216,3,128,29,72,3,128,34,80,2,128,15,1,67,11,0,0,17,1,123,12,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,67,11,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17, 
  1,67,11,0,0,1,15,1,67,11,0,0,17,1,157,84,0,0,1,15,1,67,11,0,0,17,1,200,28,0,0,1,19,34,0,0,0,91,0,0,0,1,0,17,1,67,11,0,0,1,19,3,0,0,0,15,0,0,0,1, 
  0,17,1,67,11,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,67,11,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,67,11,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,67,11,0, 
  0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,67,11,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,67,11,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,67,11,0,0,1,19,21,0,0, 
  0,52,0,0,0,1,0,17,1,67,11,0,0,1,2,21,1,130,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,8,8,128,1,48,195,127,2,48,3,128,3,248,210,130,4,224,17,128,5,200,16,128,6,40,16, 
  128,7,184,79,130,8,24,15,131,9,0,14,130,10,232,12,128,11,120,203,129,12,96,10,128,13,72,137,129,19,104,7,128,15,168,8,128,23,152,6,128,25,200,5,128,27,208,4,128,29,216,3,128,40,56,3,128,8,4,15, 
  1,254,14,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,254,14,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1, 
  0,15,1,254,14,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,254,14,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,254,14,0,0,15,1,188,28,0,0,15, 
  1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,254,14,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,254,14,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,254,14,0,0,15, 
  1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19, 
  21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0, 
  0,92,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0, 
  53,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,4,15,1,254,14,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,31,0,0,0,85,0,0,0,3,0,14,1,4,15,1,254,14,0,0,15, 
  1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19, 
  21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,34,0,0,0,92,0,0,0,2,0,1,2,19,34, 
  0,0,0,92,0,0,0,2,0,1,19,34,0,0,0,91,0,0,0,1,0,1,19,21,0,0,0,53,0,0,0,1,0,1,19,21,0,0,0,57,0,0,0,1,0,1,21,1,237,2,0,0,181,97,0,0,21,0,0,0, 
  4,0,0,0,17,144,9,128,1,48,195,127,2,48,3,131,3,32,86,131,4,216,20,128,5,144,19,128,6,192,18,128,23,88,7,128,8,240,17,131,9,168,16,130,10,96,15,128,11,192,205,129,12,120,12,128,13,48,139,129, 
  18,40,9,128,15,96,10,128,19,88,8,128,25,88,6,128,27,48,5,128,29,8,4,128,40,56,3,128,8,4,15,1,28,18,0,0,15,1,227,21,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0, 
  0,64,0,0,0,1,0,15,1,28,18,0,0,15,1,227,21,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,28,18,0,0,15,1,227,21,0,0,15,1,8,29, 
  0,0,17,1,200,28,0,0,1,4,15,1,28,18,0,0,15,1,227,21,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,28,18,0,0,15,1,227,21,0,0,15,1,188,28,0,0, 
  15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,28,18,0,0,15,1,227,21,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,19,31,0,0,0,84,0,0,0,2,0,1,4,15,1,28,18,0,0,15, 
  1,227,21,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,28,18,0,0,15,1,227,21,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0, 
  0,53,0,0,0,1,0,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,89,0,0,0,1,0,17, 
  1,28,18,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,4,19, 
  8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0, 
  1,0,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,4,15,1,28,18,0,0,15,1,227,21,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,15,1,28,18,0,0,15,1,227,21,0,0,15, 
  1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,4,19,3,0,0,0,11, 
  0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0, 
  0,0,89,0,0,0,1,0,17,1,28,18,0,0,1,2,21,0,55,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,152,8,130,9,8,8,130,10,120,7,128,3,40,201,129,12,232,6,128,21,168,196,129,22,64, 
  4,128,31,184,2,128,16,88,6,128,17,200,133,128,19,56,5,128,25,216,131,128,29,72,3,128,33,80,2,128,15,1,28,18,0,0,17,1,84,19,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,28,18,0,0, 
  1,19,21,0,0,0,55,0,0,0,1,0,17,1,28,18,0,0,1,15,1,28,18,0,0,17,1,157,84,0,0,1,15,1,28,18,0,0,17,1,200,28,0,0,1,19,33,0,0,0,89,0,0,0,1,0,17,1,28,18, 
  0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,28,18,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,28,18,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,28,18,0,0,1,19,21,0, 
  0,0,56,0,0,0,1,0,17,1,28,18,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,28,18,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,28,18,0,0,1,19,21,0,0,0,53,0,0,0, 
  1,0,17,1,28,18,0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,28,18,0,0,1,2,21,1,130,2,0,0,181,97,0,0,21,0,0,0,4,0,0,0,17,120,8,128,1,48,195,127,2,48,3,131,3,248, 
  82,131,4,224,17,128,5,200,16,128,6,40,16,128,23,152,6,128,8,136,15,131,9,112,14,130,10,88,13,128,11,232,203,129,12,208,10,128,13,184,137,129,18,8,8,128,15,24,9,128,19,104,7,128,25,200,5,128,27,208, 
  4,128,29,216,3,128,40,56,3,128,8,4,15,1,215,21,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,215,21,0,0,15,1,20,29,0,0,17,1,157,84,0, 
  0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,215,21,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,215,21,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4, 
  15,1,215,21,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,215,21,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,19,31,0,0,0,83,0,0,0,3,0,14,1,4, 
  15,1,215,21,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,215,21,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0, 
  1,0,19,33,0,0,0,90,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,90,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1, 
  0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,90,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33, 
  0,0,0,90,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,33,0,0,0,90,0,0,0,2,0,1,4,15,1,215,21,0,0,15,1,22,15,0,0,17,1, 
  231,68,0,0,1,4,15,1,215,21,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,90,0,0,0,2,0,1, 
  4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33,0,0,0,90,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,33, 
  0,0,0,90,0,0,0,2,0,1,2,19,33,0,0,0,90,0,0,0,2,0,1,19,33,0,0,0,89,0,0,0,1,0,1,21,1,237,2,0,0,116,98,0,0,21,0,0,0,4,0,0,0,17,144,9,128,1,48,195, 
  127,2,48,3,128,3,32,214,130,4,216,20,131,5,144,19,128,6,192,18,128,23,88,7,128,8,240,17,131,9,168,16,130,10,96,15,128,11,192,205,129,12,120,12,128,13,48,139,129,19,192,8,128,15,96,10,128,20,88,8, 
  128,25,88,6,128,27,48,5,128,29,8,4,128,40,56,3,128,8,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,221,24, 
  0,0,15,1,164,28,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,221,24,0,0,15,1,164,28,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15, 
  1,221,24,0,0,15,1,164,28,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0, 
  0,1,4,19,31,0,0,0,82,0,0,0,2,0,1,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,176,28,0,0, 
  17,1,46,15,0,0,1,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0, 
  0,87,0,0,0,1,0,17,1,221,24,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,17,1,221,24,0,0,1,4,19,16,0,0, 
  0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,17,1,221,24,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19, 
  21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,17,1,221,24,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,87,0,0,0, 
  1,0,17,1,221,24,0,0,1,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,15,1,221,24,0,0,15,1,164,28,0,0,15,1,176,28,0,0,17,1,85,8,0,0, 
  1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,17,1,221,24,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52, 
  0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,17,1,221,24,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,87,0,0,0,1,0,17,1,221, 
  24,0,0,1,2,21,0,55,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,152,8,130,9,8,8,130,10,120,7,128,3,40,201,129,12,232,6,128,21,168,196,129,22,64,4,128,31,184,2,128,16,88,70,129,17, 
  200,133,128,19,56,5,128,25,216,3,128,29,72,3,128,32,80,2,128,15,1,221,24,0,0,17,1,21,26,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,221,24,0,0,1,19,21,0,0,0,55,0,0,0,1, 
  0,17,1,221,24,0,0,1,15,1,221,24,0,0,17,1,157,84,0,0,1,15,1,221,24,0,0,17,1,200,28,0,0,1,19,32,0,0,0,87,0,0,0,1,0,17,1,221,24,0,0,1,19,3,0,0,0,15,0,0, 
  0,1,0,17,1,221,24,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,221,24,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,221,24,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,221, 
  24,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,221,24,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,221,24,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,221,24,0,0,1,19,21, 
  0,0,0,52,0,0,0,1,0,17,1,221,24,0,0,1,2,21,1,130,2,0,0,116,98,0,0,21,0,0,0,4,0,0,0,17,120,8,128,1,48,195,127,2,48,3,128,3,248,210,130,4,224,17,131,5,200,16,128,6, 
  40,16,128,23,152,6,128,8,136,15,131,9,112,14,130,10,88,13,128,11,232,203,129,12,208,10,128,13,184,137,129,19,216,7,128,15,24,9,128,20,104,7,128,25,200,5,128,27,208,4,128,29,216,3,128,40,56,3,128,8, 
  4,15,1,152,28,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,152,28,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0, 
  0,1,0,15,1,152,28,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,152,28,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,152,28,0,0,15,1,188,28,0, 
  0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,19,31,0,0,0,81,0,0,0,3,0,14,1,4,15,1,152,28,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,152,28,0,0,15,1,176,28,0, 
  0,17,1,46,15,0,0,1,4,15,1,152,28,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0, 
  2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1, 
  0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,4, 
  19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,4,15,1,152,28,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,15,1,152,28,0, 
  0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1, 
  0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,32,0,0,0,88,0,0,0,2,0,1,2, 
  19,32,0,0,0,88,0,0,0,2,0,1,19,32,0,0,0,87,0,0,0,1,0,1,19,21,0,0,0,58,0,0,0,1,0,1,19,21,0,0,0,52,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,40,176,0,128,4,15,1,142,58,0,0,17,1,99,55,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,21,0,0,0,54,0,0, 
  0,1,0,1,19,21,0,0,0,56,0,0,0,1,0,1,21,1,56,0,0,0,51,99,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,29,248,0,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15, 
  1,89,29,0,0,17,1,157,84,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,17,1,147,29,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,2,21,1,161,0,0,0,124,99,0,0,7,0,0,0,2,0,0,0,6,104,4,128,1,112,65,129,2,112,129,127,7,0,68,128,11,16,67,128,23,112,2,128,29,120,1,128,8,4,19, 
  25,0,0,0,64,0,0,0,1,0,15,1,78,37,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,78,37,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1, 
  0,19,27,0,0,0,70,0,0,0,1,0,17,1,78,37,0,0,1,4,19,29,0,0,0,78,0,0,0,4,0,1,4,15,1,78,37,0,0,15,1,54,37,0,0,17,1,53,30,0,0,1,2,21,1,220,2,0,0,232, 
  99,0,0,20,0,0,0,4,0,0,0,17,8,9,128,1,16,195,127,2,16,3,128,3,152,213,130,4,80,20,128,5,8,19,128,6,56,18,128,23,56,7,128,8,104,209,130,9,32,208,129,10,216,14,128,11,56,141,129,12, 
  240,11,128,13,168,74,129,19,56,8,128,15,216,9,128,25,56,6,128,27,16,5,128,29,232,3,128,40,24,3,128,8,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19, 
  25,0,0,0,64,0,0,0,1,0,15,1,18,33,0,0,15,1,42,37,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,18,33,0,0,15,1,42,37,0,0,15, 
  1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,188, 
  28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,176,28,0, 
  0,17,1,46,15,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0, 
  0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,4,19,16,0, 
  0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0, 
  19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,100,0,0, 
  0,1,0,17,1,18,33,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,15,1,18,33,0,0,15,1,42,37,0,0,15,1,176,28,0,0,17,1,85,8,0, 
  0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0, 
  52,0,0,0,1,0,19,39,0,0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,100,0,0,0,1,0,17,1, 
  18,33,0,0,1,2,21,0,60,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,8,192,8,130,9,48,8,130,10,160,7,128,3,80,201,129,12,16,7,128,21,208,196,129,22,104,4,128,31,224,194,129,16,128,6,128, 
  17,240,133,128,19,96,197,128,25,0,4,128,29,112,3,128,35,216,2,128,39,112,2,128,15,1,18,33,0,0,17,1,79,34,0,0,1,1,19,21,0,0,0,58,0,0,0,1,0,17,1,18,33,0,0,1,19,21,0,0, 
  0,55,0,0,0,1,0,17,1,18,33,0,0,1,15,1,18,33,0,0,17,1,157,84,0,0,1,15,1,18,33,0,0,17,1,200,28,0,0,1,19,39,0,0,0,100,0,0,0,1,0,17,1,18,33,0,0,1,19,3, 
  0,0,0,15,0,0,0,1,0,17,1,18,33,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,18,33,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,18,33,0,0,1,19,21,0,0,0,56,0,0, 
  0,1,0,17,1,18,33,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,18,33,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,18,33,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,18, 
  33,0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,18,33,0,0,1,2,21,1,142,2,0,0,156,100,0,0,22,0,0,0,4,0,0,0,17,104,8,128,1,80,195,127,2,80,3,128,3,88,211,130,4,64,18, 
  128,5,40,17,128,6,136,16,128,7,24,80,130,8,120,15,131,9,96,14,130,10,72,13,128,11,216,203,129,12,192,10,128,13,168,137,129,19,200,7,128,15,8,9,128,23,248,6,128,25,40,6,129,27,48,5,128,29,56,4, 
  128,40,152,3,128,41,88,3,128,8,4,17,1,234,36,0,0,1,4,15,1,222,36,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,222,36,0,0,15,1,20,29, 
  0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,222,36,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,222,36,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17, 
  1,113,82,0,0,1,4,15,1,222,36,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,222,36,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,222,36,0,0,15, 
  1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,222,36,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0, 
  101,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,101,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13, 
  0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,101,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,101,0,0,0, 
  2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,39,0,0,0,101,0,0,0,2,0,1,4,15,1,222,36,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19, 
  35,0,0,0,95,0,0,0,3,0,14,1,4,15,1,222,36,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0, 
  101,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,39,0,0,0,101,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52, 
  0,0,0,1,0,19,39,0,0,0,101,0,0,0,2,0,1,2,19,39,0,0,0,101,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,35,0,0,0,94,0, 
  0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,39,0,0,0,100,0,0,0,1,0,1,19,30,0,0,0,79,0,0,0,1,0,1,19,27, 
  0,0,0,72,0,0,0,1,0,1,21,0,162,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,16,240,3,128,17,96,67,128,25,248,2,128,27,144,2,129,12,128,4,128,29,136,2,128,30,32,2,128,35,144,1,128, 
  19,30,0,0,0,79,0,0,0,1,0,17,1,78,37,0,0,1,15,1,78,37,0,0,17,1,60,50,0,0,1,1,15,1,78,37,0,0,17,1,241,37,0,0,1,15,1,78,37,0,0,17,1,157,84,0,0,1,19,27, 
  0,0,0,72,0,0,0,1,0,17,1,78,37,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,78,37,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,78,37,0,0,1,2,21,7,78,0,0,0,255, 
  255,255,255,5,0,0,0,2,0,0,0,47,48,1,129,45,112,1,128,42,240,1,128,43,176,65,127,123,48,2,128,4,17,1,250,48,0,0,1,4,17,1,160,47,0,0,1,4,17,1,70,46,0,0,1,4,17,1,16,45, 
  0,0,1,4,17,1,91,38,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,220,2,0,0,232,99,0,0,20,0,0,0,4,0,0,0,17,8,9, 
  128,1,16,195,127,2,16,3,128,3,152,213,130,4,80,20,128,5,8,19,128,6,56,18,128,23,56,7,128,8,104,209,130,9,32,208,129,10,216,14,128,11,56,141,129,12,240,11,128,13,168,74,129,19,56,8,128,15,216,9, 
  128,25,56,6,128,27,16,5,128,29,232,3,128,40,24,3,128,8,4,15,1,56,41,0,0,15,1,4,45,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,56,41, 
  0,0,15,1,4,45,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,56,41,0,0,15,1,4,45,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15, 
  1,56,41,0,0,15,1,4,45,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,56,41,0,0,15,1,4,45,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0, 
  0,1,4,15,1,56,41,0,0,15,1,4,45,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,56,41,0,0,15,1,4,45,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,56,41,0, 
  0,15,1,4,45,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0, 
  1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13, 
  0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0, 
  0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,4,15,1,56, 
  41,0,0,15,1,4,45,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,15,1,56,41,0,0,15,1,4,45,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0, 
  19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,97,0,0, 
  0,1,0,17,1,56,41,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,2,21,0,60,1,0,0,255,255, 
  255,255,15,0,0,0,3,0,0,0,8,192,8,130,9,48,8,130,10,160,7,128,3,80,201,129,12,16,7,128,21,208,196,129,22,104,4,128,31,224,2,128,16,128,6,128,17,240,133,128,19,96,197,128,25,0,4,128,29,112, 
  131,128,35,216,2,128,37,112,2,128,15,1,56,41,0,0,17,1,117,42,0,0,1,1,19,21,0,0,0,58,0,0,0,1,0,17,1,56,41,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,56,41,0,0,1, 
  15,1,56,41,0,0,17,1,157,84,0,0,1,15,1,56,41,0,0,17,1,200,28,0,0,1,19,37,0,0,0,97,0,0,0,1,0,17,1,56,41,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,56,41,0, 
  0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,56,41,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,56,41,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,56,41,0,0,1,19,21,0,0, 
  0,57,0,0,0,1,0,17,1,56,41,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,56,41,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,56,41,0,0,1,19,21,0,0,0,52,0,0,0,1, 
  0,17,1,56,41,0,0,1,2,21,1,130,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,8,8,128,1,48,195,127,2,48,3,128,3,248,210,130,4,224,17,128,5,200,16,128,6,40,16,128,7,184,79,130,8, 
  24,15,131,9,0,14,130,10,232,12,128,11,120,203,129,12,96,10,128,13,72,137,129,19,104,7,128,15,168,8,128,23,152,6,128,25,200,5,128,27,208,4,128,29,216,3,128,40,56,3,128,8,4,15,1,248,44,0,0,15, 
  1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,248,44,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,248,44,0, 
  0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,248,44,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,248,44,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17, 
  1,202,81,0,0,1,4,15,1,248,44,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,248,44,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,248,44,0,0,15,1,34,15,0,0,17, 
  1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,98,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0, 
  0,0,1,0,19,37,0,0,0,98,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,98,0,0,0,2, 
  0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,37,0,0,0,98,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0, 
  19,37,0,0,0,98,0,0,0,2,0,1,4,15,1,248,44,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,35,0,0,0,93,0,0,0,4,0,14,1,4,15,1,248,44,0,0,15,1,176,28,0,0,17, 
  1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,98,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0, 
  0,0,1,0,19,37,0,0,0,98,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,37,0,0,0,98,0,0,0,2,0,1,2,19,37,0,0,0,98,0,0, 
  0,2,0,1,19,37,0,0,0,97,0,0,0,1,0,1,21,1,120,0,0,0,102,101,0,0,5,0,0,0,2,0,0,0,23,48,2,128,1,48,193,128,2,48,1,128,11,208,66,127,29,56,1,128,8,4,19,25,0,0, 
  0,64,0,0,0,1,0,15,1,137,45,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,137,45,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27, 
  0,0,0,70,0,0,0,1,0,17,1,137,45,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,12,32,131,128,17,0,194,128,16,144,2,128,27,48,1,128,25,152,1,128,15,1,137,45,0, 
  0,17,1,0,46,0,0,1,15,1,137,45,0,0,17,1,157,84,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,137,45,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,137,45,0,0,1,19,27,0, 
  0,0,71,0,0,0,1,0,17,1,137,45,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,4,17,1,16,45,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,68,0,0,0,3,0,14,1,21,1,120,0,0,0,102,101,0,0,5,0,0,0,2,0,0,0,23,48,2,128,1,48,193,128,2,48,1,128,11,208,66,127, 
  29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,191,46,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,191,46,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0, 
  0,0,46,0,0,0,1,0,19,27,0,0,0,70,0,0,0,1,0,17,1,191,46,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,12,32,131,128,17,0,194,128,16,144,2,128,27,48,1, 
  128,25,152,1,128,15,1,191,46,0,0,17,1,54,47,0,0,1,15,1,191,46,0,0,17,1,157,84,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,191,46,0,0,1,19,27,0,0,0,70,0,0,0,1,0, 
  17,1,191,46,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,191,46,0,0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,144,1,128,45,208,1,128,42,80,1,128,43,16,65,127, 
  4,17,1,70,46,0,0,1,4,17,1,16,45,0,0,1,4,17,1,250,48,0,0,1,4,17,1,160,47,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  27,0,0,0,66,0,0,0,3,0,14,1,21,1,120,0,0,0,102,101,0,0,5,0,0,0,2,0,0,0,23,48,2,128,1,48,193,128,2,48,1,128,11,208,66,127,29,56,1,128,8,4,19,25,0,0,0,64,0,0, 
  0,1,0,15,1,25,48,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,25,48,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,70, 
  0,0,0,1,0,17,1,25,48,0,0,1,2,21,0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,12,32,131,128,17,0,194,128,16,144,2,128,27,48,1,128,25,152,1,128,15,1,25,48,0,0,17,1,144, 
  48,0,0,1,15,1,25,48,0,0,17,1,157,84,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,25,48,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,25,48,0,0,1,19,27,0,0,0,71,0, 
  0,0,1,0,17,1,25,48,0,0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,144,1,128,45,16,1,128,42,208,1,128,43,80,65,127,4,17,1,160,47,0,0,1,4,17,1,70,46,0, 
  0,1,4,17,1,250,48,0,0,1,4,17,1,16,45,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,67,0,0,0,3,0,14,1,21,1, 
  120,0,0,0,102,101,0,0,5,0,0,0,2,0,0,0,23,48,2,128,1,48,193,128,2,48,1,128,11,208,66,127,29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,115,49,0,0,15,1,115,51,0, 
  0,17,1,157,84,0,0,1,4,15,1,115,49,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,70,0,0,0,1,0,17,1,115,49,0,0,1,2,21, 
  0,118,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,12,32,131,128,17,0,194,128,16,144,2,128,27,48,1,128,25,152,1,128,15,1,115,49,0,0,17,1,234,49,0,0,1,15,1,115,49,0,0,17,1,157,84, 
  0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,115,49,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,115,49,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,115,49,0,0,1,2,21,7, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,208,0,128,47,16,1,128,4,17,1,16,45,0,0,1,4,17,1,250,48,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,19,27,0,0,0,69,0,0,0,3,0,14,1,21,1,174,0,0,0,124,99,0,0,7,0,0,0,2,0,0,0,6,0,5,128,1,112,65,129,2,112,129,127,7,144,68,128,11,112,67,128,23,160, 
  2,128,29,120,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,235,50,0,0,15,1,247,50,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,235,50,0,0,15,1,247,50,0,0,15,1,66, 
  37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,70,0,0,0,1,0,15,1,235,50,0,0,17,1,247,50,0,0,1,4,19,29,0,0,0,77,0,0,0,5,0,14,1, 
  4,15,1,235,50,0,0,17,1,53,30,0,0,1,2,19,30,0,0,0,80,0,0,0,2,0,1,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,72,131,128,17,40,194,128,16,184,2,128,27,88,129, 
  128,25,192,1,128,35,80,1,128,1,15,1,247,50,0,0,17,1,241,37,0,0,1,15,1,247,50,0,0,17,1,157,84,0,0,1,19,27,0,0,0,72,0,0,0,1,0,17,1,247,50,0,0,1,19,27,0,0,0,70, 
  0,0,0,1,0,17,1,247,50,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,247,50,0,0,1,2,19,27,0,0,0,71,0,0,0,1,0,1,21,0,60,1,0,0,255,255,255,255,15,0,0,0,3,0,0, 
  0,8,80,8,130,9,192,7,130,10,48,7,128,3,80,201,129,12,160,6,128,5,72,137,129,22,248,3,128,7,224,200,129,16,16,6,128,17,128,197,128,19,240,4,128,21,96,132,128,25,144,3,128,29,0,3,128,31,112,2, 
  128,19,21,0,0,0,58,0,0,0,1,0,17,1,127,51,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,127,51,0,0,1,15,1,127,51,0,0,17,1,157,84,0,0,1,15,1,127,51,0,0,17,1,200,28, 
  0,0,1,19,7,0,0,0,24,0,0,0,1,0,17,1,127,51,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,127,51,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,127,51,0,0,1,19,3,0, 
  0,0,13,0,0,0,1,0,17,1,127,51,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,127,51,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,127,51,0,0,1,19,21,0,0,0,54,0,0,0, 
  1,0,17,1,127,51,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,127,51,0,0,1,15,1,127,51,0,0,17,1,188,52,0,0,1,1,19,21,0,0,0,52,0,0,0,1,0,17,1,127,51,0,0,1,2, 
  21,1,130,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,8,8,128,1,48,195,127,2,48,3,128,3,248,210,130,4,224,17,128,5,200,16,128,6,40,16,128,7,184,79,130,8,24,15,131,9,0,14,130,10,232, 
  12,128,11,120,203,129,12,96,10,128,13,72,137,129,19,104,7,128,15,168,8,128,23,152,6,128,25,200,5,128,27,208,4,128,29,216,3,128,40,56,3,128,8,4,15,1,63,55,0,0,15,1,87,55,0,0,17,1,32,29, 
  0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,63,55,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,63,55,0,0,15,1,8,29,0,0,17,1, 
  200,28,0,0,1,4,15,1,63,55,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,63,55,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1, 
  63,55,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,63,55,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,63,55,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8, 
  0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0, 
  25,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,8,0,0,0,27, 
  0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0,0, 
  2,0,1,4,15,1,63,55,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,5,0,0,0,19,0,0,0,4,0,14,1,4,15,1,63,55,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3, 
  0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0, 
  25,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,2,19,7,0,0,0,25,0,0,0,2,0,1,19,7,0,0,0, 
  24,0,0,0,1,0,1,19,21,0,0,0,55,0,0,0,1,0,1,21,1,137,0,0,0,229,101,0,0,6,0,0,0,2,0,0,0,18,240,2,128,1,80,1,129,2,80,129,127,11,88,67,128,23,80,2,128,29,88,1, 
  128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,237,55,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,237,55,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,23,0,0,0,61, 
  0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,70,0,0,0,1,0,17,1,237,55,0,0,1,2,21,0,140,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,208,131,128,17, 
  176,2,129,16,64,131,128,23,168,194,128,24,64,2,128,25,216,1,128,27,112,1,128,15,1,237,55,0,0,17,1,37,58,0,0,1,15,1,237,55,0,0,17,1,157,84,0,0,1,15,1,237,55,0,0,17,1,122,56,0, 
  0,1,1,19,27,0,0,0,72,0,0,0,1,0,17,1,237,55,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,237,55,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,237,55,0,0,1,2,21,7, 
  48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,208,0,128,41,16,1,128,4,17,1,198,56,0,0,1,4,19,23,0,0,0,60,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,120,0,0,0,102,101,0,0,5,0,0,0,2,0,0,0,23,48,2,128,1,48,193,128,2,48,1,128,11,208,66,127,29,56,1,128,8,4,19,25,0,0,0,64, 
  0,0,0,1,0,15,1,63,57,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,63,57,0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0, 
  0,70,0,0,0,1,0,17,1,63,57,0,0,1,2,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,72,131,128,17,40,2,129,16,184,130,128,27,80,1,128,24,32,2,128,25,184,1,128,15,1,63, 
  57,0,0,17,1,187,57,0,0,1,15,1,63,57,0,0,17,1,157,84,0,0,1,1,19,27,0,0,0,72,0,0,0,1,0,17,1,63,57,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,63,57,0,0,1, 
  19,27,0,0,0,71,0,0,0,1,0,17,1,63,57,0,0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,144,1,128,45,16,1,128,42,208,1,128,43,80,65,127,4,17,1,160,47,0,0, 
  1,4,17,1,70,46,0,0,1,4,17,1,250,48,0,0,1,4,17,1,16,45,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,63,0,0, 
  0,3,0,14,1,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,80,1,128,45,208,1,128,42,16,1,128,43,144,65,127,4,17,1,16,45,0,0,1,4,17,1,250,48,0,0,1,4,17,1,70,46, 
  0,0,1,4,17,1,160,47,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,62,0,0,0,1,0,1,19,9,0,0,0,31,0,0,0,2, 
  0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,212,58,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2, 
  21,1,139,0,0,0,81,102,0,0,6,0,0,0,2,0,0,0,36,152,3,128,1,80,193,128,2,80,193,128,39,88,1,128,37,216,2,128,38,24,2,128,8,4,19,28,0,0,0,75,0,0,0,1,0,19,20,0,0,0, 
  51,0,0,0,3,0,1,4,19,28,0,0,0,76,0,0,0,1,0,19,20,0,0,0,51,0,0,0,3,0,1,4,19,28,0,0,0,74,0,0,0,1,0,19,20,0,0,0,51,0,0,0,3,0,1,4,19,28,0,0, 
  0,73,0,0,0,1,0,19,20,0,0,0,51,0,0,0,3,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,184,1,128,5,176,193,128,6,72,2,128,23,80,1,128,9,176,65,128,25,176, 
  2,128,19,9,0,0,0,31,0,0,0,2,0,1,1,19,6,0,0,0,22,0,0,0,1,0,17,1,96,59,0,0,1,15,1,96,59,0,0,17,1,196,59,0,0,1,15,1,96,59,0,0,17,1,154,58,0,0,1,2, 
  21,1,68,0,0,0,178,103,0,0,4,0,0,0,2,0,0,0,6,224,1,128,1,16,129,128,2,16,129,127,29,24,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,185,66,0,0,17,1,154,58,0,0,1, 
  4,17,1,9,60,0,0,1,2,21,1,238,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,40,9,128,1,48,195,127,2,48,3,128,3,40,214,130,4,224,20,128,5,152,19,128,6,200,18,128,7,88,82,130,8, 
  136,17,131,9,64,16,130,10,248,14,128,11,88,205,129,12,16,12,128,13,200,138,129,19,88,8,128,15,248,9,128,23,88,7,128,25,88,6,128,27,48,5,128,29,8,4,128,40,56,3,128,8,4,15,1,248,62,0,0,15, 
  1,75,55,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,248,62,0,0,15,1,75,55,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0, 
  0,59,0,0,0,1,0,15,1,248,62,0,0,15,1,75,55,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113, 
  82,0,0,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,176,28,0,0,17,1,239,21,0, 
  0,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0, 
  30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7, 
  0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1, 
  0,17,1,248,62,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,4,19,8,0,0,0,26,0,0,0,1, 
  0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,5,0, 
  0,0,20,0,0,0,4,0,14,1,4,15,1,248,62,0,0,15,1,75,55,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19, 
  7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,4,19, 
  3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,2,21,0,60,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,8,80, 
  8,130,9,192,7,130,10,48,7,128,3,80,201,129,12,160,6,128,5,72,137,129,22,248,3,128,7,224,200,129,16,16,6,128,17,128,197,128,19,240,4,128,21,96,132,128,25,144,3,128,29,0,3,128,31,112,2,128,19,21, 
  0,0,0,58,0,0,0,1,0,17,1,248,62,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,248,62,0,0,1,15,1,248,62,0,0,17,1,157,84,0,0,1,15,1,248,62,0,0,17,1,200,28,0,0,1, 
  19,7,0,0,0,24,0,0,0,1,0,17,1,248,62,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,248,62,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,248,62,0,0,1,19,3,0,0,0,13, 
  0,0,0,1,0,17,1,248,62,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,248,62,0,0,1,19,21,0,0,0,57,0,0,0,1,0,17,1,248,62,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17, 
  1,248,62,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,248,62,0,0,1,15,1,248,62,0,0,17,1,53,64,0,0,1,1,19,21,0,0,0,52,0,0,0,1,0,17,1,248,62,0,0,1,2,21,1,131, 
  2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,8,8,128,1,48,195,127,2,48,3,128,3,0,211,130,4,232,17,128,5,208,16,128,6,48,16,128,7,184,79,130,8,24,15,131,9,0,14,130,10,232,12,128,11, 
  120,203,129,12,96,10,128,13,72,137,129,19,104,7,128,15,168,8,128,23,152,6,128,25,200,5,128,27,208,4,128,29,216,3,128,40,56,3,128,8,4,15,1,63,55,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1, 
  4,19,25,0,0,0,64,0,0,0,1,0,15,1,63,55,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,63,55,0,0,15,1,8,29,0,0,17,1,200,28,0, 
  0,1,4,15,1,63,55,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,63,55,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,63,55,0, 
  0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,63,55,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,63,55,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0, 
  30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0, 
  0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0, 
  1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1, 
  4,15,1,63,55,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,5,0,0,0,18,0,0,0,5,0,14,14,1,4,15,1,63,55,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0, 
  0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0, 
  0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,7,0,0,0,25,0,0,0,2,0,1,2,19,6,0,0,0,23,0,0,0,2,0,1,19,6,0,0,0,22,0, 
  0,0,1,0,1,21,0,21,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,8,240,70,130,9,96,70,130,2,160,136,129,3,16,8,130,12,64,5,128,5,128,7,128,22,152,2,128,15,176,4,128,10,208,5,128,16, 
  32,4,128,17,144,131,128,19,0,3,128,25,48,2,128,15,1,209,66,0,0,17,1,157,84,0,0,1,15,1,209,66,0,0,17,1,231,67,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,209,66,0,0,1,19, 
  3,0,0,0,14,0,0,0,1,0,17,1,209,66,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,209,66,0,0,1,19,2,0,0,0,9,0,0,0,1,0,17,1,209,66,0,0,1,19,2,0,0,0,8,0, 
  0,0,1,0,17,1,209,66,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,209,66,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,209,66,0,0,1,19,2,0,0,0,5,0,0,0,1,0,17,1, 
  209,66,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,209,66,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,209,66,0,0,1,1,2,21,1,92,0,0,0,42,95,0,0,5,0,0,0,2,0,0, 
  0,6,160,2,128,1,48,129,128,2,48,129,127,17,48,66,128,29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,68,68,0,0,15,1,197,66,0,0,17,1,154,58,0,0,1,4,15,1,142,58,0,0, 
  17,1,99,55,0,0,1,4,17,1,103,5,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,80,1,128,5,224,193,128,6,72,2,128,23,232,1,128,9,224,65,128,25,176,2,128,19,6, 
  0,0,0,22,0,0,0,1,0,17,1,68,68,0,0,1,1,19,9,0,0,0,31,0,0,0,2,0,1,15,1,68,68,0,0,17,1,196,59,0,0,1,15,1,68,68,0,0,17,1,154,58,0,0,1,2,21,7,35,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,15,0,0,0,44,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21, 
  1,44,0,0,0,6,104,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,11,248,0,128,8,4,19,8,0,0,0,28,0,0,0,2,0,1,2,19,2,0,0,0,5,0,0,0,1,0,1,21,1,50,0, 
  0,0,51,99,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,29,248,0,128,8,4,19,25,0,0,0,64,0,0,0,1,0,17,1,83,69,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,58,176,0,128,4,17,1,141,69,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,56,0,0,0,51,99,0,0,3,0,0,0,1, 
  0,0,0,2,240,0,128,1,240,64,128,29,248,0,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,198,69,0,0,17,1,157,84,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  123,176,0,128,4,17,1,0,70,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,220,2,0,0,232,99,0,0,20,0,0,0,4,0,0,0,17,8, 
  9,128,1,16,195,127,2,16,3,128,3,152,213,130,4,80,20,128,5,8,19,128,6,56,18,128,23,56,7,128,8,104,209,130,9,32,208,129,10,216,14,128,11,56,141,129,12,240,11,128,13,168,74,129,19,56,8,128,15,216, 
  9,128,25,56,6,128,27,16,5,128,29,232,3,128,40,24,3,128,8,4,15,1,221,72,0,0,15,1,164,76,0,0,15,1,87,55,0,0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,221, 
  72,0,0,15,1,164,76,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,221,72,0,0,15,1,164,76,0,0,15,1,8,29,0,0,17,1,200,28,0,0,1,4, 
  15,1,221,72,0,0,15,1,164,76,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,221,72,0,0,15,1,164,76,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81, 
  0,0,1,4,15,1,221,72,0,0,15,1,164,76,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,221,72,0,0,15,1,164,76,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,221,72, 
  0,0,15,1,164,76,0,0,15,1,34,15,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0, 
  0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0, 
  13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11, 
  0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,4,15,1, 
  221,72,0,0,15,1,164,76,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,15,1,221,72,0,0,15,1,164,76,0,0,15,1,176,28,0,0,17,1,85,8,0,0,1,4,19,3,0,0,0,12,0,0,0,1, 
  0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,33,0, 
  0,0,1,0,17,1,221,72,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,2,21,0,55,1,0,0,255, 
  255,255,255,14,0,0,0,3,0,0,0,8,152,72,130,9,8,72,130,10,120,7,128,3,40,73,129,12,128,6,128,21,64,4,130,22,216,3,128,31,80,2,128,11,16,199,128,16,240,5,128,17,96,133,128,19,208,4,128,25, 
  112,3,128,29,224,2,128,19,21,0,0,0,58,0,0,0,1,0,17,1,221,72,0,0,1,19,21,0,0,0,55,0,0,0,1,0,17,1,221,72,0,0,1,15,1,221,72,0,0,17,1,157,84,0,0,1,15,1,221,72, 
  0,0,17,1,200,28,0,0,1,19,11,0,0,0,33,0,0,0,1,0,17,1,221,72,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,221,72,0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,221,72, 
  0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,221,72,0,0,1,19,21,0,0,0,56,0,0,0,1,0,17,1,221,72,0,0,1,15,1,221,72,0,0,17,1,21,74,0,0,1,19,21,0,0,0,57,0,0, 
  0,1,0,17,1,221,72,0,0,1,19,21,0,0,0,54,0,0,0,1,0,17,1,221,72,0,0,1,19,21,0,0,0,53,0,0,0,1,0,17,1,221,72,0,0,1,19,21,0,0,0,52,0,0,0,1,0,17,1,221, 
  72,0,0,1,2,21,1,130,2,0,0,202,95,0,0,21,0,0,0,4,0,0,0,17,8,8,128,1,48,195,127,2,48,3,128,3,248,210,130,4,224,17,128,5,200,16,128,6,40,16,128,7,184,79,130,8,24,15,131,9, 
  0,14,130,10,232,12,128,11,120,203,129,12,96,10,128,13,72,137,129,19,104,7,128,15,168,8,128,23,152,6,128,25,200,5,128,27,208,4,128,29,216,3,128,40,56,3,128,8,4,15,1,152,76,0,0,15,1,87,55,0, 
  0,17,1,32,29,0,0,1,4,19,25,0,0,0,64,0,0,0,1,0,15,1,152,76,0,0,15,1,20,29,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,152,76,0,0,15,1,8, 
  29,0,0,17,1,200,28,0,0,1,4,15,1,152,76,0,0,15,1,188,28,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,152,76,0,0,15,1,188,28,0,0,15,1,101,82,0,0,17,1,202,81,0, 
  0,1,4,15,1,152,76,0,0,15,1,176,28,0,0,17,1,239,21,0,0,1,4,15,1,152,76,0,0,15,1,176,28,0,0,17,1,46,15,0,0,1,4,15,1,152,76,0,0,15,1,34,15,0,0,17,1,32,69,0, 
  0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11,0,0,0,34,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0, 
  19,11,0,0,0,34,0,0,0,2,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,34,0,0,0,2,0,1,4,19, 
  8,0,0,0,27,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11,0,0,0,34,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,21,0,0,0,53,0,0,0,1,0,19,11,0,0, 
  0,34,0,0,0,2,0,1,4,15,1,152,76,0,0,15,1,22,15,0,0,17,1,231,68,0,0,1,4,19,10,0,0,0,32,0,0,0,7,0,14,1,4,15,1,152,76,0,0,15,1,176,28,0,0,17,1,85,8,0, 
  0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,34,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0, 
  19,11,0,0,0,34,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,21,0,0,0,52,0,0,0,1,0,19,11,0,0,0,34,0,0,0,2,0,1,2,19,11,0,0,0,34,0,0,0,2,0,1, 
  19,11,0,0,0,33,0,0,0,1,0,1,19,2,0,0,0,7,0,0,0,1,0,1,21,1,65,2,0,0,59,104,0,0,20,0,0,0,4,0,0,0,17,176,7,128,1,16,195,127,2,16,3,131,3,24,81,131,4,40, 
  16,128,5,56,15,128,6,152,14,128,23,216,5,128,8,248,13,128,9,8,13,130,10,24,12,128,11,208,202,129,12,224,9,128,13,240,136,129,18,72,7,128,15,80,8,128,19,168,6,128,25,8,5,128,27,16,4,128,29,24, 
  3,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,254,78,0,0,15,1,67,90,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,254,78,0,0,15,1,209,66,0,0,17,1, 
  10,5,0,0,1,4,15,1,254,78,0,0,15,1,196,83,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,254,78,0,0,15,1,196,83,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1, 
  254,78,0,0,15,1,190,81,0,0,17,1,61,79,0,0,1,4,19,15,0,0,0,43,0,0,0,2,0,1,4,15,1,254,78,0,0,15,1,190,81,0,0,17,1,188,76,0,0,1,4,15,1,254,78,0,0,15,1,176, 
  76,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,254,78,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,2,0,0,0,5,0,0, 
  0,1,0,17,1,254,78,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,254,78,0,0,1,4,19,8,0,0,0,27,0,0, 
  0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,254,78,0,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,254,78,0,0,1,4,15,1,254,78,0,0,15,1, 
  20,69,0,0,17,1,231,68,0,0,1,4,15,1,254,78,0,0,15,1,190,81,0,0,17,1,200,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,254,78,0,0, 
  1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,254,78,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,254,78,0,0, 
  1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,15,0,0,0,42,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  208,0,128,8,2,21,1,65,2,0,0,238,104,0,0,20,0,0,0,4,0,0,0,17,176,7,128,1,16,195,127,2,16,3,128,3,24,209,130,4,40,16,131,5,56,15,128,6,152,14,128,23,216,5,128,8,248,13,128,9, 
  8,13,130,10,24,12,128,11,208,202,129,12,224,9,128,13,240,136,129,19,16,7,128,15,80,8,128,20,168,6,128,25,8,5,128,27,16,4,128,29,24,3,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,127,81, 
  0,0,15,1,67,90,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,127,81,0,0,15,1,209,66,0,0,17,1,10,5,0,0,1,4,15,1,127,81,0,0,15,1,196,83,0,0,15, 
  1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,127,81,0,0,15,1,196,83,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,19,15,0,0,0,41,0,0,0,2,0,1,4,15,1,127,81,0,0,15,1, 
  190,81,0,0,17,1,61,79,0,0,1,4,15,1,127,81,0,0,15,1,190,81,0,0,17,1,188,76,0,0,1,4,15,1,127,81,0,0,15,1,176,76,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0, 
  0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,127,81,0,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,127,81,0,0,1,4,19,16,0,0,0,46,0,0, 
  0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,127,81,0,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,127,81,0, 
  0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,17,1,127,81,0,0,1,4,15,1,127,81,0,0,15,1,20,69,0,0,17,1,231,68,0,0,1,4,15,1,127,81,0,0,15, 
  1,190,81,0,0,17,1,200,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,127,81,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3, 
  0,0,0,1,0,17,1,127,81,0,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,17,1,127,81,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,93,176,0,128,4,19,15,0,0,0,40,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,2,0,0,0,9,0,0,0,1,0,1,21,1, 
  50,0,0,0,161,105,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,22,248,0,128,8,4,19,18,0,0,0,48,0,0,0,1,0,17,1,253,81,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,18,208,0,128,17,56,1,128,15,1,253,81,0,0,17,1,38,82,0,0,1,1,2,21,1,62,0,0,0,208,105,0,0,4,0,0,0,2,0,0,0,24,24,1,128,1,16,1,128,2,16,65,128, 
  22,136,1,128,8,4,19,17,0,0,0,47,0,0,0,3,0,14,1,4,19,18,0,0,0,49,0,0,0,2,0,1,2,19,3,0,0,0,14,0,0,0,1,0,1,21,1,120,0,0,0,102,101,0,0,5,0,0,0,2, 
  0,0,0,23,48,2,128,1,48,193,128,2,48,1,128,11,208,66,127,29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,234,82,0,0,15,1,115,51,0,0,17,1,157,84,0,0,1,4,15,1,234,82, 
  0,0,15,1,66,37,0,0,17,1,202,81,0,0,1,4,19,16,0,0,0,46,0,0,0,1,0,19,27,0,0,0,70,0,0,0,1,0,17,1,234,82,0,0,1,2,21,0,123,0,0,0,255,255,255,255,6,0,0,0, 
  2,0,0,0,12,72,131,128,17,40,194,128,16,184,2,128,19,32,130,128,25,184,1,128,27,80,1,128,15,1,234,82,0,0,17,1,102,83,0,0,1,15,1,234,82,0,0,17,1,157,84,0,0,1,1,19,27,0,0,0, 
  72,0,0,0,1,0,17,1,234,82,0,0,1,19,27,0,0,0,70,0,0,0,1,0,17,1,234,82,0,0,1,19,27,0,0,0,71,0,0,0,1,0,17,1,234,82,0,0,1,2,21,1,93,0,0,0,68,106,0,0, 
  7,0,0,0,2,0,0,0,32,248,1,128,1,112,1,129,2,112,129,128,31,56,2,128,26,120,130,128,33,184,1,128,34,120,1,128,8,4,17,1,250,48,0,0,1,4,17,1,160,47,0,0,1,4,17,1,70,46,0,0, 
  1,4,17,1,16,45,0,0,1,4,19,19,0,0,0,50,0,0,0,3,0,14,1,2,19,2,0,0,0,3,0,0,0,1,0,1,19,3,0,0,0,15,0,0,0,1,0,1,21,1,92,0,0,0,42,95,0,0,5,0, 
  0,0,2,0,0,0,6,160,2,128,1,48,129,128,2,48,129,127,17,48,66,128,29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,57,84,0,0,15,1,197,66,0,0,17,1,154,58,0,0,1,4,15, 
  1,142,58,0,0,17,1,99,55,0,0,1,4,17,1,103,5,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,32,2,128,5,80,193,128,6,88,1,128,23,192,1,128,9,80,65,128,25, 
  176,2,128,1,15,1,57,84,0,0,17,1,196,59,0,0,1,19,9,0,0,0,31,0,0,0,2,0,1,19,6,0,0,0,22,0,0,0,1,0,17,1,57,84,0,0,1,15,1,57,84,0,0,17,1,154,58,0,0,1, 
  2,21,7,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,15,1,38,85,0,0,15,1,189,85,0,0,17,1,238,84,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,19,12,0,0,0,36,0,0,0,1,0,1,21,1,55,0,0,0,215,106,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,30,248,0,128,8,4,19,26,0,0,0,65, 
  0,0,0,1,0,19,13,0,0,0,37,0,0,0,2,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,12,232,129,128,13,88,1,128,14,240,0,128,15,1,38,85,0,0,17,1,101,85,0,0, 
  1,19,14,0,0,0,38,0,0,0,1,0,17,1,38,85,0,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,15,1,177,85,0,0,17,1,238,84,0,0,1,21,9,27, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,12,0,0,0,35,0,0,0,2,0,14,1,19,14,0,0,0,39,0,0,0,2,0,1,19,14,0,0,0,38,0,0,0,1,0, 
  1,21,0,60,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,0,216,9,130,1,112,9,130,2,224,8,130,3,80,136,130,12,128,5,128,5,192,7,128,22,216,2,128,15,240,4,128,8,48,199,128,9,160,198,128,10, 
  16,6,128,16,96,4,128,17,208,131,128,19,64,3,128,25,112,2,128,15,1,201,85,0,0,17,1,157,84,0,0,1,15,1,201,85,0,0,17,1,118,89,0,0,1,19,3,0,0,0,15,0,0,0,1,0,17,1,201,85, 
  0,0,1,19,3,0,0,0,14,0,0,0,1,0,17,1,201,85,0,0,1,19,3,0,0,0,13,0,0,0,1,0,17,1,201,85,0,0,1,19,2,0,0,0,9,0,0,0,1,0,17,1,201,85,0,0,1,19,2,0, 
  0,0,8,0,0,0,1,0,17,1,201,85,0,0,1,19,2,0,0,0,7,0,0,0,1,0,17,1,201,85,0,0,1,19,2,0,0,0,6,0,0,0,1,0,17,1,201,85,0,0,1,19,2,0,0,0,5,0,0,0, 
  1,0,17,1,201,85,0,0,1,19,2,0,0,0,4,0,0,0,1,0,17,1,201,85,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,201,85,0,0,1,19,1,0,0,0,1,0,0,0,1,0,17,1,201,85, 
  0,0,1,15,1,201,85,0,0,17,1,6,87,0,0,1,1,2,21,1,88,2,0,0,80,90,0,0,19,0,0,0,4,0,0,0,17,40,7,128,1,240,194,127,2,240,2,128,3,168,209,130,4,144,16,128,5,120,15,128, 
  6,216,14,128,23,184,5,128,8,56,14,128,9,32,205,129,10,8,12,128,11,152,138,129,12,128,9,128,13,104,72,129,19,136,6,128,15,200,7,128,25,232,4,128,27,240,3,128,29,248,2,128,8,4,19,25,0,0,0,64, 
  0,0,0,1,0,15,1,106,89,0,0,15,1,67,90,0,0,17,1,157,84,0,0,1,4,19,22,0,0,0,59,0,0,0,1,0,15,1,106,89,0,0,15,1,209,66,0,0,17,1,10,5,0,0,1,4,15,1,106,89, 
  0,0,15,1,196,83,0,0,15,1,208,83,0,0,17,1,113,82,0,0,1,4,15,1,106,89,0,0,15,1,196,83,0,0,15,1,101,82,0,0,17,1,202,81,0,0,1,4,15,1,106,89,0,0,15,1,190,81,0,0, 
  17,1,61,79,0,0,1,4,15,1,106,89,0,0,15,1,190,81,0,0,17,1,188,76,0,0,1,4,15,1,106,89,0,0,15,1,176,76,0,0,17,1,32,69,0,0,1,4,19,8,0,0,0,30,0,0,0,1,0,19, 
  2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,29,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,16, 
  0,0,0,46,0,0,0,1,0,19,3,0,0,0,13,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,27,0,0,0,1,0,19,2,0,0,0, 
  5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,8,0,0,0,26,0,0,0,1,0,19,2,0,0,0,5,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,15,1,106,89,0,0, 
  15,1,20,69,0,0,17,1,231,68,0,0,1,4,15,1,106,89,0,0,15,1,190,81,0,0,17,1,200,2,0,0,1,4,19,3,0,0,0,12,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0, 
  0,2,0,0,0,2,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,4,19,3,0,0,0,10,0,0,0,1,0,19,2,0,0,0, 
  3,0,0,0,1,0,19,1,0,0,0,2,0,0,0,2,0,1,19,0,0,0,0,0,0,0,0,1,0,1,19,1,0,0,0,2,0,0,0,2,0,1,21,1,92,0,0,0,42,95,0,0,5,0,0,0,2,0,0,0, 
  6,160,2,128,1,48,129,128,2,48,129,127,17,48,66,128,29,56,1,128,8,4,19,25,0,0,0,64,0,0,0,1,0,15,1,211,89,0,0,15,1,197,66,0,0,17,1,154,58,0,0,1,4,15,1,142,58,0,0,17, 
  1,99,55,0,0,1,4,17,1,103,5,0,0,1,2,21,0,99,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,20,40,2,128,5,184,193,128,6,192,1,128,23,184,2,128,9,184,65,128,25,80,1,128,15,1,211, 
  89,0,0,17,1,154,58,0,0,1,1,15,1,211,89,0,0,17,1,196,59,0,0,1,19,6,0,0,0,22,0,0,0,1,0,17,1,211,89,0,0,1,19,9,0,0,0,31,0,0,0,2,0,1,2,19,1,0,0,0, 
  1,0,0,0,1,0,1,19,2,0,0,0,8,0,0,0,1,0,1,13,21,4,89,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,80,66,128,64,16,194,128,91,208,193,128,35,136,194,127,92,144,1,128,123,80, 
  1,128,3,17,1,250,93,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5, 
  0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3, 
  17,1,29,94,0,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,102,240,0,128,45,48,65,128,95,48,1,128,3,17,1,69,91,0,0,1,3,17,1,59,92,0,0,1,21,2,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,59,92,0,0,1,2,18,27,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,111,240,0,128, 
  3,17,1,155,91,0,0,1,3,17,1,59,92,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,59,92,0,0,1,1,18,27,0,0,0,21,4,46,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,48,1,128,45,240,64,128,95,240,0,128,3,17,1,59,92,0,0,1,3,17,1,241,91,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,59,92,0,0,1,1,18,15,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,59,92,0,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,59,92,0,0,1,1,18,27,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208, 
  192,127,3,17,1,59,92,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,59,92,0,0,1,1,18,8,0,0,0,21,4,52,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,45,48,129,128,43,104,193,127,95,240,0,128,3,17,1,221,92,0,0,1,3,18,10,0,0,0,1,3,18,9,0,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  3,176,0,128,3,17,1,221,92,0,0,1,1,18,29,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,221,92,0,0,1,21,2,34,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,221,92,0,0,1,1,18,19,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,91,176,0,128,3,18,25,0,0,0, 
  1,1,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,32,104,129,128,109,40,1,128,110,240,0,128,3,18,13,0,0,0,1,3,17,1,127,93,0,0,1,3,18,12,0,0,0,1,2,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,158,93,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,189,93,0,0,1,2,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,220,93,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,5,0,0,0,1,2,18,6, 
  0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,3,18,23,0,0,0,1,1,18,4,0,0,0,1,18,3,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,3,176,0,128,3,17,1,35,94,0,0,1,1,18,11,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,71,94,0,0,1,1,18,2,0,0,0,1,18,1,0, 
  0,0,1,21,4,100,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,40,168,130,128,125,112,1,128,64,104,194,128,35,224,66,128,91,40,130,128,92,232,1,128,123,168,1,128,3,18,7,0,0,0,1,3,17,1,250, 
  93,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0, 
  0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3,17,1,29,94,0,0, 
  1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,104,129,128,123,240,0,128,64,40,1,128,3,18,6,0,0,0,1,3,17,1,137,95,0,0,1,3,18,17,0,0,0,1,21,2,42,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45, 
  208,192,127,3,17,1,221,92,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,221,92,0,0,1,2,21,4,112,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0, 
  40,8,67,128,64,200,2,128,91,136,2,129,35,64,195,127,92,72,2,128,109,8,130,128,123,200,1,128,125,144,1,128,3,18,7,0,0,0,1,3,17,1,250,93,0,0,1,3,17,1,137,96,0,0,1,3,17,1,74,93, 
  0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128, 
  2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3,17,1,29,94,0,0,1,2,18,3,0,0,0,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,203,96,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,35,94,0,0,1,1,18, 
  3,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,13,97,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,35, 
  94,0,0,1,1,18,3,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,79,97,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176, 
  0,128,3,17,1,35,94,0,0,1,1,18,3,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,145,97,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,3,176,0,128,3,17,1,35,94,0,0,1,1,18,40,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,35,94,0,0,1,1,21,4,112,0,0,0,255, 
  255,255,255,8,0,0,0,3,0,0,0,40,8,131,128,41,208,2,128,64,144,2,128,35,64,195,128,92,16,2,128,109,208,1,128,91,80,66,128,123,144,1,128,3,17,1,250,93,0,0,1,3,17,1,137,96,0,0,1,3, 
  17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,18,0,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3,17,1,29,94, 
  0,0,1,2,21,4,112,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,8,67,128,64,200,2,128,91,136,66,129,35,64,195,127,92,72,2,128,93,16,66,128,109,208,1,128,123,144,1,128,3,17,1,250,93,0, 
  0,1,3,17,1,137,96,0,0,1,3,18,20,0,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0, 
  0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17, 
  1,35,94,0,0,1,3,17,1,29,94,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,64,176,0,128,3,17,1,137,95,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,16,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,64,104,1,128,123,40,65,128,125,240,0,128,3,18, 
  7,0,0,0,1,3,17,1,250,93,0,0,1,3,17,1,137,95,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,113,94,0,0,1,3, 
  17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,2,21,4,101,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,40,176,130,128,109,176,1,128,64,112,194,128,35,232,66,128,91,48,130,128,92,240,1,128,123,112, 
  1,128,3,17,1,250,93,0,0,1,3,17,1,137,96,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3, 
  17,1,35,94,0,0,1,3,17,1,29,94,0,0,1,2,21,4,123,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,96,131,128,59,40,67,129,64,232,2,128,35,152,131,127,92,104,2,128,109,40,194,128,91,168, 
  66,128,123,232,1,128,125,176,1,128,3,18,7,0,0,0,1,3,17,1,250,93,0,0,1,3,17,1,137,96,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0,1,3,18,41, 
  0,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1, 
  113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3,17,1,29,94,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,64,16,1,128, 
  123,208,0,128,3,17,1,199,101,0,0,1,3,17,1,137,95,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,113,94,0,0,1,3,17, 
  1,107,94,0,0,1,3,17,1,71,94,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,3,18,23,0,0,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,64,48,1,128,41,112,65,128,123,240,0,128,3,17,1,199,101,0,0,1,3,17,1,137,95,0,0,1,3,18,18,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128, 
  5,48,1,128,6,240,0,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,115,16,1,128,105,208,1,128,110, 
  144,1,128,111,80,65,127,3,17,1,117,103,0,0,1,3,17,1,56,103,0,0,1,3,17,1,251,102,0,0,1,3,17,1,190,102,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,16,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,221,102,0,0,1,2,21,4,29,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,39,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,26,103,0,0,1,2,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,37,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,17,1,87,103,0,0,1,2,21,4,29,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,106,176,0,128,3,18,36,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,148,103,0,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,18,38,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,64,8,1,128,123,208,0,128,3,18,6,0,0,0,1,3, 
  17,1,137,95,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,2,21,2,52,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,71,94,0,0,1,2,21,4,100,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,40,168,130,128,41,112,2,128,64,48,194,128,35,224,66,128,91,240,129,128,92,176,1,128,123,112,1,128,3,17,1,250,93,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3,17,1,133,92,0,0, 
  1,3,18,18,0,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1, 
  128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3,17,1,29,94,0,0,1,2,21,4,100,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0, 
  40,168,130,128,93,176,1,128,64,104,194,128,35,224,66,128,91,40,130,128,92,232,1,128,123,112,1,128,3,17,1,250,93,0,0,1,3,18,20,0,0,0,1,3,17,1,74,93,0,0,1,3,17,1,39,93,0,0,1,3, 
  17,1,133,92,0,0,1,3,18,17,0,0,0,1,3,17,1,248,90,0,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128, 
  3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,71,94,0,0,1,3,17,1,35,94,0,0,1,3,17,1,29,94,0,0,1,2,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,18,22,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,3,17,1,32,106,0,0,1,21,2, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,3,17,1,62,106,0,0,1,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,125,176,0,128,3,18,24,0,0,0,1,2,18,22,0,0,0,1,21,4,74,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,47,112,1,128,45,168,193,128,42,24,2,128,43,224,65,127,93,48, 
  1,128,3,17,1,185,106,0,0,1,3,18,34,0,0,0,1,3,18,33,0,0,0,1,3,18,32,0,0,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,16,1,128,3,17,1,113,94,0,0,1,3,17,1,107,94,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,93,176,0,128,3,18,26,0,0,0,1,2,21,4,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,48,107,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,113,94, 
  0,0,1,3,17,1,107,94,0,0,1,3,17,1,48,107,0,0,1,2,18,30,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,48,107,0,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,48,107,0,0,1,1, 
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