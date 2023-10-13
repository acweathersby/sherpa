
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



impl State{
  /// Create a [State] node from a `String` input.
  
  pub fn from_string (input: String)-> Result<Box<State>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ir_from(reader)
  }
}

impl State{
  /// Create a [State] node from a `String` input.
  
  pub fn from_str (input: &str)-> Result<Box<State>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ir_from(reader)
  }
}

impl Grammar{
  /// Create a [Grammar] node from a `String` input.
  
  pub fn from_string (input: String)-> Result<Box<Grammar>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::grammar_from(reader)
  }
}

impl Grammar{
  /// Create a [Grammar] node from a `String` input.
  
  pub fn from_str (input: &str)-> Result<Box<Grammar>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::grammar_from(reader)
  }
}

impl AST_Struct{
  /// Create a [AST_Struct] node from a `String` input.
  
  pub fn from_string (input: String)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ast_struct_from(reader)
  }
}

impl AST_Struct{
  /// Create a [AST_Struct] node from a `String` input.
  
  pub fn from_str (input: &str)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ast_struct_from(reader)
  }
}

pub trait ASTParse<T>{
  fn ir_from(input:T) -> Result<Box<State>, SherpaParseError>;
  fn escaped_from(input:T) -> Result<Vec<String>, SherpaParseError>;
  fn grammar_from(input:T) -> Result<Box<Grammar>, SherpaParseError>;
  fn type_eval_from(input:T) -> Result<ASTNode, SherpaParseError>;
  fn ast_expression_from(input:T) -> Result<ASTNode, SherpaParseError>;
  fn ast_struct_from(input:T) -> Result<Box<AST_Struct>, SherpaParseError>;
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
  Reset(Box<Reset>),
  Grouped_Rules(Box<Grouped_Rules>),
  AST_NamedReference(Box<AST_NamedReference>),
  Fork(Box<Fork>),
  Reduce(Box<Reduce>),
  SetTokenLen(Box<SetTokenLen>),
  EOFSymbol(Box<EOFSymbol>),
  CFRules(Box<CFRules>),
  AST_Add(Box<AST_Add>),
  Precedence(Box<Precedence>),
  NonTermMatch(Box<NonTermMatch>),
  TokenGroupRules(Box<TokenGroupRules>),
  SetTokenId(Box<SetTokenId>),
  TerminalToken(Box<TerminalToken>),
  Pop(Box<Pop>),
  AST_Vector(Box<AST_Vector>),
  FailHint(Box<FailHint>),
  Grammar(Box<Grammar>),
  AppendRules(Box<AppendRules>),
  TemplateSym(Box<TemplateSym>),
  AST_U64(Box<AST_U64>),
  DEFINED_TYPE_IDENT(Box<DEFINED_TYPE_IDENT>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol>),
  Pass(Box<Pass>),
  Import(Box<Import>),
  Init(Box<Init>),
  AST_IndexReference(Box<AST_IndexReference>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol>),
  AST_Member(Box<AST_Member>),
  AST_I32(Box<AST_I32>),
  Goto(Box<Goto>),
  AST_I8(Box<AST_I8>),
  TemplateASTType(Box<TemplateASTType>),
  AST_F32(Box<AST_F32>),
  Gotos(Box<Gotos>),
  TerminalMatches(Box<TerminalMatches>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol>),
  AST_Token(Box<AST_Token>),
  Shift(Box<Shift>),
  Export(Box<Export>),
  Name(Box<Name>),
  AST_BOOL(Box<AST_BOOL>),
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol>),
  Fail(Box<Fail>),
  AST_Statements(Box<AST_Statements>),
  TemplateRules(Box<TemplateRules>),
  AST_U8(Box<AST_U8>),
  Ignore(Box<Ignore>),
  AST_STRING(Box<AST_STRING>),
  AST_U32(Box<AST_U32>),
  State(Box<State>),
  List_Rules(Box<List_Rules>),
  AST_I64(Box<AST_I64>),
  DefaultMatch(Box<DefaultMatch>),
  SetLine(Box<SetLine>),
  AST_U16(Box<AST_U16>),
  PegRules(Box<PegRules>),
  AST_STRUCT_TEMPLATE_NAME(Box<AST_STRUCT_TEMPLATE_NAME>),
  NotEmptySet(Box<NotEmptySet>),
  Accept(Box<Accept>),
  ClassSymbol(Box<ClassSymbol>),
  ReduceRaw(Box<ReduceRaw>),
  Statement(Box<Statement>),
  AST_F64(Box<AST_F64>),
  AST_Map(Box<AST_Map>),
  Range(Box<Range>),
  IntMatch(Box<IntMatch>),
  AST_I16(Box<AST_I16>),
  Push(Box<Push>),
  AST_NUMBER(Box<AST_NUMBER>),
  AST_Struct(Box<AST_Struct>),
  Peek(Box<Peek>),
  Matches(Box<Matches>),
  ProductionMatches(Box<ProductionMatches>),
  TermMatch(Box<TermMatch>),
  DEFINED_TYPE_NUM(Box<DEFINED_TYPE_NUM>),
  Rule(Box<Rule>),
  AnnotatedSymbol(Box<AnnotatedSymbol>),
  AST_Property(Box<AST_Property>),
  Ascript(Box<Ascript>),
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
  Reset,
  Grouped_Rules,
  AST_NamedReference,
  Fork,
  Reduce,
  SetTokenLen,
  EOFSymbol,
  CFRules,
  AST_Add,
  Precedence,
  NonTermMatch,
  TokenGroupRules,
  SetTokenId,
  TerminalToken,
  Pop,
  AST_Vector,
  FailHint,
  Grammar,
  AppendRules,
  TemplateSym,
  AST_U64,
  DEFINED_TYPE_IDENT,
  NonTerminal_Symbol,
  Pass,
  Import,
  Init,
  AST_IndexReference,
  Template_NonTerminal_Symbol,
  AST_Member,
  AST_I32,
  Goto,
  AST_I8,
  TemplateASTType,
  AST_F32,
  Gotos,
  TerminalMatches,
  NonTerminal_Import_Symbol,
  AST_Token,
  Shift,
  Export,
  Name,
  AST_BOOL,
  NonTerminal_Terminal_Symbol,
  Fail,
  AST_Statements,
  TemplateRules,
  AST_U8,
  Ignore,
  AST_STRING,
  AST_U32,
  State,
  List_Rules,
  AST_I64,
  DefaultMatch,
  SetLine,
  AST_U16,
  PegRules,
  AST_STRUCT_TEMPLATE_NAME,
  NotEmptySet,
  Accept,
  ClassSymbol,
  ReduceRaw,
  Statement,
  AST_F64,
  AST_Map,
  Range,
  IntMatch,
  AST_I16,
  Push,
  AST_NUMBER,
  AST_Struct,
  Peek,
  Matches,
  ProductionMatches,
  TermMatch,
  DEFINED_TYPE_NUM,
  Rule,
  AnnotatedSymbol,
  AST_Property,
  Ascript,
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
      ASTNode::Reset(node) => node.tok.clone(),
      ASTNode::Grouped_Rules(node) => node.tok.clone(),
      ASTNode::AST_NamedReference(node) => node.tok.clone(),
      ASTNode::Fork(node) => node.tok.clone(),
      ASTNode::Reduce(node) => node.tok.clone(),
      ASTNode::EOFSymbol(node) => node.tok.clone(),
      ASTNode::CFRules(node) => node.tok.clone(),
      ASTNode::AST_Add(node) => node.tok.clone(),
      ASTNode::TokenGroupRules(node) => node.tok.clone(),
      ASTNode::SetTokenId(node) => node.tok.clone(),
      ASTNode::TerminalToken(node) => node.tok.clone(),
      ASTNode::Pop(node) => node.tok.clone(),
      ASTNode::AST_Vector(node) => node.tok.clone(),
      ASTNode::Grammar(node) => node.tok.clone(),
      ASTNode::AppendRules(node) => node.tok.clone(),
      ASTNode::AST_U64(node) => node.tok.clone(),
      ASTNode::NonTerminal_Symbol(node) => node.tok.clone(),
      ASTNode::Pass(node) => node.tok.clone(),
      ASTNode::Import(node) => node.tok.clone(),
      ASTNode::AST_IndexReference(node) => node.tok.clone(),
      ASTNode::Template_NonTerminal_Symbol(node) => node.tok.clone(),
      ASTNode::AST_I32(node) => node.tok.clone(),
      ASTNode::Goto(node) => node.tok.clone(),
      ASTNode::AST_I8(node) => node.tok.clone(),
      ASTNode::AST_F32(node) => node.tok.clone(),
      ASTNode::NonTerminal_Import_Symbol(node) => node.tok.clone(),
      ASTNode::Shift(node) => node.tok.clone(),
      ASTNode::AST_BOOL(node) => node.tok.clone(),
      ASTNode::NonTerminal_Terminal_Symbol(node) => node.tok.clone(),
      ASTNode::Fail(node) => node.tok.clone(),
      ASTNode::AST_Statements(node) => node.tok.clone(),
      ASTNode::TemplateRules(node) => node.tok.clone(),
      ASTNode::AST_U8(node) => node.tok.clone(),
      ASTNode::AST_STRING(node) => node.tok.clone(),
      ASTNode::AST_U32(node) => node.tok.clone(),
      ASTNode::State(node) => node.tok.clone(),
      ASTNode::List_Rules(node) => node.tok.clone(),
      ASTNode::AST_I64(node) => node.tok.clone(),
      ASTNode::SetLine(node) => node.tok.clone(),
      ASTNode::AST_U16(node) => node.tok.clone(),
      ASTNode::PegRules(node) => node.tok.clone(),
      ASTNode::NotEmptySet(node) => node.tok.clone(),
      ASTNode::Accept(node) => node.tok.clone(),
      ASTNode::ClassSymbol(node) => node.tok.clone(),
      ASTNode::ReduceRaw(node) => node.tok.clone(),
      ASTNode::AST_F64(node) => node.tok.clone(),
      ASTNode::AST_Map(node) => node.tok.clone(),
      ASTNode::AST_I16(node) => node.tok.clone(),
      ASTNode::Push(node) => node.tok.clone(),
      ASTNode::AST_Struct(node) => node.tok.clone(),
      ASTNode::Peek(node) => node.tok.clone(),
      ASTNode::Matches(node) => node.tok.clone(),
      ASTNode::Rule(node) => node.tok.clone(),
      ASTNode::AnnotatedSymbol(node) => node.tok.clone(),
      ASTNode::AST_Property(node) => node.tok.clone(),
      ASTNode::Ascript(node) => node.tok.clone(),
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
      ASTNode::Reset(..) => ASTNodeType::Reset,
      ASTNode::Grouped_Rules(..) => ASTNodeType::Grouped_Rules,
      ASTNode::AST_NamedReference(..) => ASTNodeType::AST_NamedReference,
      ASTNode::Fork(..) => ASTNodeType::Fork,
      ASTNode::Reduce(..) => ASTNodeType::Reduce,
      ASTNode::SetTokenLen(..) => ASTNodeType::SetTokenLen,
      ASTNode::EOFSymbol(..) => ASTNodeType::EOFSymbol,
      ASTNode::CFRules(..) => ASTNodeType::CFRules,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::Precedence(..) => ASTNodeType::Precedence,
      ASTNode::NonTermMatch(..) => ASTNodeType::NonTermMatch,
      ASTNode::TokenGroupRules(..) => ASTNodeType::TokenGroupRules,
      ASTNode::SetTokenId(..) => ASTNodeType::SetTokenId,
      ASTNode::TerminalToken(..) => ASTNodeType::TerminalToken,
      ASTNode::Pop(..) => ASTNodeType::Pop,
      ASTNode::AST_Vector(..) => ASTNodeType::AST_Vector,
      ASTNode::FailHint(..) => ASTNodeType::FailHint,
      ASTNode::Grammar(..) => ASTNodeType::Grammar,
      ASTNode::AppendRules(..) => ASTNodeType::AppendRules,
      ASTNode::TemplateSym(..) => ASTNodeType::TemplateSym,
      ASTNode::AST_U64(..) => ASTNodeType::AST_U64,
      ASTNode::DEFINED_TYPE_IDENT(..) => ASTNodeType::DEFINED_TYPE_IDENT,
      ASTNode::NonTerminal_Symbol(..) => ASTNodeType::NonTerminal_Symbol,
      ASTNode::Pass(..) => ASTNodeType::Pass,
      ASTNode::Import(..) => ASTNodeType::Import,
      ASTNode::Init(..) => ASTNodeType::Init,
      ASTNode::AST_IndexReference(..) => ASTNodeType::AST_IndexReference,
      ASTNode::Template_NonTerminal_Symbol(..) => ASTNodeType::Template_NonTerminal_Symbol,
      ASTNode::AST_Member(..) => ASTNodeType::AST_Member,
      ASTNode::AST_I32(..) => ASTNodeType::AST_I32,
      ASTNode::Goto(..) => ASTNodeType::Goto,
      ASTNode::AST_I8(..) => ASTNodeType::AST_I8,
      ASTNode::TemplateASTType(..) => ASTNodeType::TemplateASTType,
      ASTNode::AST_F32(..) => ASTNodeType::AST_F32,
      ASTNode::Gotos(..) => ASTNodeType::Gotos,
      ASTNode::TerminalMatches(..) => ASTNodeType::TerminalMatches,
      ASTNode::NonTerminal_Import_Symbol(..) => ASTNodeType::NonTerminal_Import_Symbol,
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Shift(..) => ASTNodeType::Shift,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::AST_BOOL(..) => ASTNodeType::AST_BOOL,
      ASTNode::NonTerminal_Terminal_Symbol(..) => ASTNodeType::NonTerminal_Terminal_Symbol,
      ASTNode::Fail(..) => ASTNodeType::Fail,
      ASTNode::AST_Statements(..) => ASTNodeType::AST_Statements,
      ASTNode::TemplateRules(..) => ASTNodeType::TemplateRules,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_STRING(..) => ASTNodeType::AST_STRING,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::State(..) => ASTNodeType::State,
      ASTNode::List_Rules(..) => ASTNodeType::List_Rules,
      ASTNode::AST_I64(..) => ASTNodeType::AST_I64,
      ASTNode::DefaultMatch(..) => ASTNodeType::DefaultMatch,
      ASTNode::SetLine(..) => ASTNodeType::SetLine,
      ASTNode::AST_U16(..) => ASTNodeType::AST_U16,
      ASTNode::PegRules(..) => ASTNodeType::PegRules,
      ASTNode::AST_STRUCT_TEMPLATE_NAME(..) => ASTNodeType::AST_STRUCT_TEMPLATE_NAME,
      ASTNode::NotEmptySet(..) => ASTNodeType::NotEmptySet,
      ASTNode::Accept(..) => ASTNodeType::Accept,
      ASTNode::ClassSymbol(..) => ASTNodeType::ClassSymbol,
      ASTNode::ReduceRaw(..) => ASTNodeType::ReduceRaw,
      ASTNode::Statement(..) => ASTNodeType::Statement,
      ASTNode::AST_F64(..) => ASTNodeType::AST_F64,
      ASTNode::AST_Map(..) => ASTNodeType::AST_Map,
      ASTNode::Range(..) => ASTNodeType::Range,
      ASTNode::IntMatch(..) => ASTNodeType::IntMatch,
      ASTNode::AST_I16(..) => ASTNodeType::AST_I16,
      ASTNode::Push(..) => ASTNodeType::Push,
      ASTNode::AST_NUMBER(..) => ASTNodeType::AST_NUMBER,
      ASTNode::AST_Struct(..) => ASTNodeType::AST_Struct,
      ASTNode::Peek(..) => ASTNodeType::Peek,
      ASTNode::Matches(..) => ASTNodeType::Matches,
      ASTNode::ProductionMatches(..) => ASTNodeType::ProductionMatches,
      ASTNode::TermMatch(..) => ASTNodeType::TermMatch,
      ASTNode::DEFINED_TYPE_NUM(..) => ASTNodeType::DEFINED_TYPE_NUM,
      ASTNode::Rule(..) => ASTNodeType::Rule,
      ASTNode::AnnotatedSymbol(..) => ASTNodeType::AnnotatedSymbol,
      ASTNode::AST_Property(..) => ASTNodeType::AST_Property,
      ASTNode::Ascript(..) => ASTNodeType::Ascript,
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
      Reset(node) => node.hash(hasher),
      Grouped_Rules(node) => node.hash(hasher),
      AST_NamedReference(node) => node.hash(hasher),
      Fork(node) => node.hash(hasher),
      Reduce(node) => node.hash(hasher),
      SetTokenLen(node) => node.hash(hasher),
      EOFSymbol(node) => node.hash(hasher),
      CFRules(node) => node.hash(hasher),
      AST_Add(node) => node.hash(hasher),
      Precedence(node) => node.hash(hasher),
      NonTermMatch(node) => node.hash(hasher),
      TokenGroupRules(node) => node.hash(hasher),
      SetTokenId(node) => node.hash(hasher),
      TerminalToken(node) => node.hash(hasher),
      Pop(node) => node.hash(hasher),
      AST_Vector(node) => node.hash(hasher),
      FailHint(node) => node.hash(hasher),
      Grammar(node) => node.hash(hasher),
      AppendRules(node) => node.hash(hasher),
      TemplateSym(node) => node.hash(hasher),
      AST_U64(node) => node.hash(hasher),
      DEFINED_TYPE_IDENT(node) => node.hash(hasher),
      NonTerminal_Symbol(node) => node.hash(hasher),
      Pass(node) => node.hash(hasher),
      Import(node) => node.hash(hasher),
      Init(node) => node.hash(hasher),
      AST_IndexReference(node) => node.hash(hasher),
      Template_NonTerminal_Symbol(node) => node.hash(hasher),
      AST_Member(node) => node.hash(hasher),
      AST_I32(node) => node.hash(hasher),
      Goto(node) => node.hash(hasher),
      AST_I8(node) => node.hash(hasher),
      TemplateASTType(node) => node.hash(hasher),
      AST_F32(node) => node.hash(hasher),
      Gotos(node) => node.hash(hasher),
      TerminalMatches(node) => node.hash(hasher),
      NonTerminal_Import_Symbol(node) => node.hash(hasher),
      AST_Token(node) => node.hash(hasher),
      Shift(node) => node.hash(hasher),
      Export(node) => node.hash(hasher),
      Name(node) => node.hash(hasher),
      AST_BOOL(node) => node.hash(hasher),
      NonTerminal_Terminal_Symbol(node) => node.hash(hasher),
      Fail(node) => node.hash(hasher),
      AST_Statements(node) => node.hash(hasher),
      TemplateRules(node) => node.hash(hasher),
      AST_U8(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      AST_STRING(node) => node.hash(hasher),
      AST_U32(node) => node.hash(hasher),
      State(node) => node.hash(hasher),
      List_Rules(node) => node.hash(hasher),
      AST_I64(node) => node.hash(hasher),
      DefaultMatch(node) => node.hash(hasher),
      SetLine(node) => node.hash(hasher),
      AST_U16(node) => node.hash(hasher),
      PegRules(node) => node.hash(hasher),
      AST_STRUCT_TEMPLATE_NAME(node) => node.hash(hasher),
      NotEmptySet(node) => node.hash(hasher),
      Accept(node) => node.hash(hasher),
      ClassSymbol(node) => node.hash(hasher),
      ReduceRaw(node) => node.hash(hasher),
      Statement(node) => node.hash(hasher),
      AST_F64(node) => node.hash(hasher),
      AST_Map(node) => node.hash(hasher),
      Range(node) => node.hash(hasher),
      IntMatch(node) => node.hash(hasher),
      AST_I16(node) => node.hash(hasher),
      Push(node) => node.hash(hasher),
      AST_NUMBER(node) => node.hash(hasher),
      AST_Struct(node) => node.hash(hasher),
      Peek(node) => node.hash(hasher),
      Matches(node) => node.hash(hasher),
      ProductionMatches(node) => node.hash(hasher),
      TermMatch(node) => node.hash(hasher),
      DEFINED_TYPE_NUM(node) => node.hash(hasher),
      Rule(node) => node.hash(hasher),
      AnnotatedSymbol(node) => node.hash(hasher),
      AST_Property(node) => node.hash(hasher),
      Ascript(node) => node.hash(hasher),
      
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
pub struct Reset{
  pub ptr_type:String, 
  pub tok: Token, 
}

impl Reset{
  
  pub fn new (ptr_type: String, tok: Token)-> Self {
    
    Self{
      ptr_type,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Reset
  }
}

impl ASTNode{
  
  pub fn to_Reset (self)-> Box::<Reset> {
    
    match self{
      Self::Reset(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Reset (&self)-> Option<&Reset> {
    
    match self{
      Self::Reset(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Reset_mut (&mut self)-> Option<&mut Reset> {
    
    match self{
      Self::Reset(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Reset{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ptr_type.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Grouped_Rules{
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl Grouped_Rules{
  
  pub fn new (rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Grouped_Rules
  }
}

impl ASTNode{
  
  pub fn to_Grouped_Rules (self)-> Box::<Grouped_Rules> {
    
    match self{
      Self::Grouped_Rules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Grouped_Rules (&self)-> Option<&Grouped_Rules> {
    
    match self{
      Self::Grouped_Rules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Grouped_Rules_mut (&mut self)-> Option<&mut Grouped_Rules> {
    
    match self{
      Self::Grouped_Rules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Grouped_Rules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_NamedReference{
  pub value:String, 
  pub tok: Token, 
}

impl AST_NamedReference{
  
  pub fn new (value: String, tok: Token)-> Self {
    
    Self{
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_NamedReference
  }
}

impl ASTNode{
  
  pub fn to_AST_NamedReference (self)-> Box::<AST_NamedReference> {
    
    match self{
      Self::AST_NamedReference(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_NamedReference (&self)-> Option<&AST_NamedReference> {
    
    match self{
      Self::AST_NamedReference(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_NamedReference_mut (&mut self)-> Option<&mut AST_NamedReference> {
    
    match self{
      Self::AST_NamedReference(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_NamedReference{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Fork{
  pub paths:Vec<Box<Goto>>, 
  pub tok: Token, 
}

impl Fork{
  
  pub fn new (paths: Vec<Box<Goto>>, tok: Token)-> Self {
    
    Self{
      paths,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Fork
  }
}

impl ASTNode{
  
  pub fn to_Fork (self)-> Box::<Fork> {
    
    match self{
      Self::Fork(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Fork (&self)-> Option<&Fork> {
    
    match self{
      Self::Fork(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Fork_mut (&mut self)-> Option<&mut Fork> {
    
    match self{
      Self::Fork(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Fork{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.paths{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Reduce{
  pub ast:Option<ASTNode>, 
  pub len:u32, 
  pub nonterminal:ASTNode, 
  pub tok: Token, 
}

impl Reduce{
  
  pub fn new (ast: Option<ASTNode>, len: u32, nonterminal: ASTNode, tok: Token)-> Self {
    
    Self{
      ast,
      len,
      nonterminal,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Reduce
  }
}

impl ASTNode{
  
  pub fn to_Reduce (self)-> Box::<Reduce> {
    
    match self{
      Self::Reduce(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Reduce (&self)-> Option<&Reduce> {
    
    match self{
      Self::Reduce(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Reduce_mut (&mut self)-> Option<&mut Reduce> {
    
    match self{
      Self::Reduce(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Reduce{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ast.hash(hasher);
    self.len.hash(hasher);
    self.nonterminal.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SetTokenLen{
  pub id:u32, 
}

impl SetTokenLen{
  
  pub fn new (id: u32)-> Self {
    
    Self{
      id,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SetTokenLen
  }
}

impl ASTNode{
  
  pub fn to_SetTokenLen (self)-> Box::<SetTokenLen> {
    
    match self{
      Self::SetTokenLen(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_SetTokenLen (&self)-> Option<&SetTokenLen> {
    
    match self{
      Self::SetTokenLen(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SetTokenLen_mut (&mut self)-> Option<&mut SetTokenLen> {
    
    match self{
      Self::SetTokenLen(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SetTokenLen{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct EOFSymbol{
  pub tok: Token, 
}

impl EOFSymbol{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::EOFSymbol
  }
}

impl ASTNode{
  
  pub fn to_EOFSymbol (self)-> Box::<EOFSymbol> {
    
    match self{
      Self::EOFSymbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_EOFSymbol (&self)-> Option<&EOFSymbol> {
    
    match self{
      Self::EOFSymbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_EOFSymbol_mut (&mut self)-> Option<&mut EOFSymbol> {
    
    match self{
      Self::EOFSymbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for EOFSymbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct CFRules{
  pub name_sym:Box<NonTerminal_Symbol>, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl CFRules{
  
  pub fn new (name_sym: Box<NonTerminal_Symbol>, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::CFRules
  }
}

impl ASTNode{
  
  pub fn to_CFRules (self)-> Box::<CFRules> {
    
    match self{
      Self::CFRules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_CFRules (&self)-> Option<&CFRules> {
    
    match self{
      Self::CFRules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_CFRules_mut (&mut self)-> Option<&mut CFRules> {
    
    match self{
      Self::CFRules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for CFRules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name_sym.hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Add{
  pub left:ASTNode, 
  pub right:ASTNode, 
  pub tok: Token, 
}

impl AST_Add{
  
  pub fn new (left: ASTNode, right: ASTNode, tok: Token)-> Self {
    
    Self{
      left,
      right,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Add
  }
}

impl ASTNode{
  
  pub fn to_AST_Add (self)-> Box::<AST_Add> {
    
    match self{
      Self::AST_Add(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Add (&self)-> Option<&AST_Add> {
    
    match self{
      Self::AST_Add(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Add_mut (&mut self)-> Option<&mut AST_Add> {
    
    match self{
      Self::AST_Add(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Add{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Precedence{
  pub is_keyword:bool, 
  pub kot_prec:u32, 
  pub sym_prec:u32, 
}

impl Precedence{
  
  pub fn new (is_keyword: bool, kot_prec: u32, sym_prec: u32)-> Self {
    
    Self{
      is_keyword,
      kot_prec,
      sym_prec,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Precedence
  }
}

impl ASTNode{
  
  pub fn to_Precedence (self)-> Box::<Precedence> {
    
    match self{
      Self::Precedence(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Precedence (&self)-> Option<&Precedence> {
    
    match self{
      Self::Precedence(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Precedence_mut (&mut self)-> Option<&mut Precedence> {
    
    match self{
      Self::Precedence(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Precedence{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_keyword.hash(hasher);
    self.kot_prec.hash(hasher);
    self.sym_prec.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTermMatch{
  pub statement:Box<Statement>, 
  pub sym:ASTNode, 
}

impl NonTermMatch{
  
  pub fn new (statement: Box<Statement>, sym: ASTNode)-> Self {
    
    Self{
      statement,
      sym,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NonTermMatch
  }
}

impl ASTNode{
  
  pub fn to_NonTermMatch (self)-> Box::<NonTermMatch> {
    
    match self{
      Self::NonTermMatch(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NonTermMatch (&self)-> Option<&NonTermMatch> {
    
    match self{
      Self::NonTermMatch(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NonTermMatch_mut (&mut self)-> Option<&mut NonTermMatch> {
    
    match self{
      Self::NonTermMatch(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NonTermMatch{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.statement.hash(hasher);
    self.sym.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TokenGroupRules{
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl TokenGroupRules{
  
  pub fn new (rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TokenGroupRules
  }
}

impl ASTNode{
  
  pub fn to_TokenGroupRules (self)-> Box::<TokenGroupRules> {
    
    match self{
      Self::TokenGroupRules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TokenGroupRules (&self)-> Option<&TokenGroupRules> {
    
    match self{
      Self::TokenGroupRules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TokenGroupRules_mut (&mut self)-> Option<&mut TokenGroupRules> {
    
    match self{
      Self::TokenGroupRules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TokenGroupRules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SetTokenId{
  pub id:u32, 
  pub tok: Token, 
}

impl SetTokenId{
  
  pub fn new (id: u32, tok: Token)-> Self {
    
    Self{
      id,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SetTokenId
  }
}

impl ASTNode{
  
  pub fn to_SetTokenId (self)-> Box::<SetTokenId> {
    
    match self{
      Self::SetTokenId(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_SetTokenId (&self)-> Option<&SetTokenId> {
    
    match self{
      Self::SetTokenId(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SetTokenId_mut (&mut self)-> Option<&mut SetTokenId> {
    
    match self{
      Self::SetTokenId(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SetTokenId{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TerminalToken{
  pub is_exclusive:bool, 
  pub val:String, 
  pub tok: Token, 
}

impl TerminalToken{
  
  pub fn new (is_exclusive: bool, val: String, tok: Token)-> Self {
    
    Self{
      is_exclusive,
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TerminalToken
  }
}

impl ASTNode{
  
  pub fn to_TerminalToken (self)-> Box::<TerminalToken> {
    
    match self{
      Self::TerminalToken(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TerminalToken (&self)-> Option<&TerminalToken> {
    
    match self{
      Self::TerminalToken(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TerminalToken_mut (&mut self)-> Option<&mut TerminalToken> {
    
    match self{
      Self::TerminalToken(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TerminalToken{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_exclusive.hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Pop{
  pub count:u32, 
  pub tok: Token, 
}

impl Pop{
  
  pub fn new (count: u32, tok: Token)-> Self {
    
    Self{
      count,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Pop
  }
}

impl ASTNode{
  
  pub fn to_Pop (self)-> Box::<Pop> {
    
    match self{
      Self::Pop(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Pop (&self)-> Option<&Pop> {
    
    match self{
      Self::Pop(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Pop_mut (&mut self)-> Option<&mut Pop> {
    
    match self{
      Self::Pop(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Pop{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.count.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Vector{
  pub initializer:Vec<ASTNode>, 
  pub tok: Token, 
}

impl AST_Vector{
  
  pub fn new (initializer: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Vector
  }
}

impl ASTNode{
  
  pub fn to_AST_Vector (self)-> Box::<AST_Vector> {
    
    match self{
      Self::AST_Vector(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Vector (&self)-> Option<&AST_Vector> {
    
    match self{
      Self::AST_Vector(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Vector_mut (&mut self)-> Option<&mut AST_Vector> {
    
    match self{
      Self::AST_Vector(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Vector{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.initializer{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct FailHint{
  pub message:String, 
}

impl FailHint{
  
  pub fn new (message: String)-> Self {
    
    Self{
      message,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::FailHint
  }
}

impl ASTNode{
  
  pub fn to_FailHint (self)-> Box::<FailHint> {
    
    match self{
      Self::FailHint(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_FailHint (&self)-> Option<&FailHint> {
    
    match self{
      Self::FailHint(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_FailHint_mut (&mut self)-> Option<&mut FailHint> {
    
    match self{
      Self::FailHint(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for FailHint{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.message.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Grammar{
  pub preamble:Vec<ASTNode>, 
  pub rules:Vec<ASTNode>, 
  pub tok: Token, 
}

impl Grammar{
  
  pub fn new (preamble: Vec<ASTNode>, rules: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      preamble,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Grammar
  }
}

impl ASTNode{
  
  pub fn to_Grammar (self)-> Box::<Grammar> {
    
    match self{
      Self::Grammar(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Grammar (&self)-> Option<&Grammar> {
    
    match self{
      Self::Grammar(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Grammar_mut (&mut self)-> Option<&mut Grammar> {
    
    match self{
      Self::Grammar(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Grammar{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.preamble{
      val.hash(hasher);
    }
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AppendRules{
  pub name_sym:ASTNode, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl AppendRules{
  
  pub fn new (name_sym: ASTNode, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AppendRules
  }
}

impl ASTNode{
  
  pub fn to_AppendRules (self)-> Box::<AppendRules> {
    
    match self{
      Self::AppendRules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AppendRules (&self)-> Option<&AppendRules> {
    
    match self{
      Self::AppendRules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AppendRules_mut (&mut self)-> Option<&mut AppendRules> {
    
    match self{
      Self::AppendRules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AppendRules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name_sym.hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TemplateSym{
  pub val:String, 
}

impl TemplateSym{
  
  pub fn new (val: String)-> Self {
    
    Self{
      val,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TemplateSym
  }
}

impl ASTNode{
  
  pub fn to_TemplateSym (self)-> Box::<TemplateSym> {
    
    match self{
      Self::TemplateSym(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TemplateSym (&self)-> Option<&TemplateSym> {
    
    match self{
      Self::TemplateSym(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TemplateSym_mut (&mut self)-> Option<&mut TemplateSym> {
    
    match self{
      Self::TemplateSym(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TemplateSym{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_U64{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_U64{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_U64
  }
}

impl ASTNode{
  
  pub fn to_AST_U64 (self)-> Box::<AST_U64> {
    
    match self{
      Self::AST_U64(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_U64 (&self)-> Option<&AST_U64> {
    
    match self{
      Self::AST_U64(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_U64_mut (&mut self)-> Option<&mut AST_U64> {
    
    match self{
      Self::AST_U64(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_U64{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DEFINED_TYPE_IDENT{
}

impl DEFINED_TYPE_IDENT{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::DEFINED_TYPE_IDENT
  }
}

impl ASTNode{
  
  pub fn to_DEFINED_TYPE_IDENT (self)-> Box::<DEFINED_TYPE_IDENT> {
    
    match self{
      Self::DEFINED_TYPE_IDENT(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_DEFINED_TYPE_IDENT (&self)-> Option<&DEFINED_TYPE_IDENT> {
    
    match self{
      Self::DEFINED_TYPE_IDENT(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_DEFINED_TYPE_IDENT_mut (&mut self)-> Option<&mut DEFINED_TYPE_IDENT> {
    
    match self{
      Self::DEFINED_TYPE_IDENT(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for DEFINED_TYPE_IDENT{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTerminal_Symbol{
  pub name:String, 
  pub tok: Token, 
}

impl NonTerminal_Symbol{
  
  pub fn new (name: String, tok: Token)-> Self {
    
    Self{
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NonTerminal_Symbol
  }
}

impl ASTNode{
  
  pub fn to_NonTerminal_Symbol (self)-> Box::<NonTerminal_Symbol> {
    
    match self{
      Self::NonTerminal_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NonTerminal_Symbol (&self)-> Option<&NonTerminal_Symbol> {
    
    match self{
      Self::NonTerminal_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NonTerminal_Symbol_mut (&mut self)-> Option<&mut NonTerminal_Symbol> {
    
    match self{
      Self::NonTerminal_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NonTerminal_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Pass{
  pub tok: Token, 
}

impl Pass{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Pass
  }
}

impl ASTNode{
  
  pub fn to_Pass (self)-> Box::<Pass> {
    
    match self{
      Self::Pass(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Pass (&self)-> Option<&Pass> {
    
    match self{
      Self::Pass(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Pass_mut (&mut self)-> Option<&mut Pass> {
    
    match self{
      Self::Pass(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Pass{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Import{
  pub reference:String, 
  pub uri:String, 
  pub tok: Token, 
}

impl Import{
  
  pub fn new (reference: String, uri: String, tok: Token)-> Self {
    
    Self{
      reference,
      uri,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Import
  }
}

impl ASTNode{
  
  pub fn to_Import (self)-> Box::<Import> {
    
    match self{
      Self::Import(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Import (&self)-> Option<&Import> {
    
    match self{
      Self::Import(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Import_mut (&mut self)-> Option<&mut Import> {
    
    match self{
      Self::Import(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Import{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.reference.hash(hasher);
    self.uri.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Init{
  pub expression:ASTNode, 
}

impl Init{
  
  pub fn new (expression: ASTNode)-> Self {
    
    Self{
      expression,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Init
  }
}

impl ASTNode{
  
  pub fn to_Init (self)-> Box::<Init> {
    
    match self{
      Self::Init(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Init (&self)-> Option<&Init> {
    
    match self{
      Self::Init(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Init_mut (&mut self)-> Option<&mut Init> {
    
    match self{
      Self::Init(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Init{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expression.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_IndexReference{
  pub value:i64, 
  pub tok: Token, 
}

impl AST_IndexReference{
  
  pub fn new (value: i64, tok: Token)-> Self {
    
    Self{
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_IndexReference
  }
}

impl ASTNode{
  
  pub fn to_AST_IndexReference (self)-> Box::<AST_IndexReference> {
    
    match self{
      Self::AST_IndexReference(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_IndexReference (&self)-> Option<&AST_IndexReference> {
    
    match self{
      Self::AST_IndexReference(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_IndexReference_mut (&mut self)-> Option<&mut AST_IndexReference> {
    
    match self{
      Self::AST_IndexReference(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_IndexReference{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Template_NonTerminal_Symbol{
  pub name:ASTNode, 
  pub template_args:Vec<ASTNode>, 
  pub tok: Token, 
}

impl Template_NonTerminal_Symbol{
  
  pub fn new (name: ASTNode, template_args: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      name,
      template_args,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Template_NonTerminal_Symbol
  }
}

impl ASTNode{
  
  pub fn to_Template_NonTerminal_Symbol (self)-> Box::<Template_NonTerminal_Symbol> {
    
    match self{
      Self::Template_NonTerminal_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Template_NonTerminal_Symbol (&self)-> Option<&Template_NonTerminal_Symbol> {
    
    match self{
      Self::Template_NonTerminal_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Template_NonTerminal_Symbol_mut (&mut self)-> Option<&mut Template_NonTerminal_Symbol> {
    
    match self{
      Self::Template_NonTerminal_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Template_NonTerminal_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
    
    for val in &self.template_args{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Member{
  pub property:Token, 
  pub reference:ASTNode, 
}

impl AST_Member{
  
  pub fn new (property: Token, reference: ASTNode)-> Self {
    
    Self{
      property,
      reference,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Member
  }
}

impl ASTNode{
  
  pub fn to_AST_Member (self)-> Box::<AST_Member> {
    
    match self{
      Self::AST_Member(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Member (&self)-> Option<&AST_Member> {
    
    match self{
      Self::AST_Member(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Member_mut (&mut self)-> Option<&mut AST_Member> {
    
    match self{
      Self::AST_Member(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Member{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.property.to_string().replace(" ", "").replace("\n", "").hash(hasher);
    self.reference.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_I32{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_I32{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_I32
  }
}

impl ASTNode{
  
  pub fn to_AST_I32 (self)-> Box::<AST_I32> {
    
    match self{
      Self::AST_I32(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_I32 (&self)-> Option<&AST_I32> {
    
    match self{
      Self::AST_I32(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_I32_mut (&mut self)-> Option<&mut AST_I32> {
    
    match self{
      Self::AST_I32(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_I32{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Goto{
  pub name:String, 
  pub nonterminal:ASTNode, 
  pub tok: Token, 
}

impl Goto{
  
  pub fn new (name: String, nonterminal: ASTNode, tok: Token)-> Self {
    
    Self{
      name,
      nonterminal,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Goto
  }
}

impl ASTNode{
  
  pub fn to_Goto (self)-> Box::<Goto> {
    
    match self{
      Self::Goto(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Goto (&self)-> Option<&Goto> {
    
    match self{
      Self::Goto(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Goto_mut (&mut self)-> Option<&mut Goto> {
    
    match self{
      Self::Goto(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Goto{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
    self.nonterminal.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_I8{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_I8{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_I8
  }
}

impl ASTNode{
  
  pub fn to_AST_I8 (self)-> Box::<AST_I8> {
    
    match self{
      Self::AST_I8(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_I8 (&self)-> Option<&AST_I8> {
    
    match self{
      Self::AST_I8(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_I8_mut (&mut self)-> Option<&mut AST_I8> {
    
    match self{
      Self::AST_I8(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_I8{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TemplateASTType{
  pub val:String, 
}

impl TemplateASTType{
  
  pub fn new (val: String)-> Self {
    
    Self{
      val,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TemplateASTType
  }
}

impl ASTNode{
  
  pub fn to_TemplateASTType (self)-> Box::<TemplateASTType> {
    
    match self{
      Self::TemplateASTType(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TemplateASTType (&self)-> Option<&TemplateASTType> {
    
    match self{
      Self::TemplateASTType(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TemplateASTType_mut (&mut self)-> Option<&mut TemplateASTType> {
    
    match self{
      Self::TemplateASTType(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TemplateASTType{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_F32{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_F32{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_F32
  }
}

impl ASTNode{
  
  pub fn to_AST_F32 (self)-> Box::<AST_F32> {
    
    match self{
      Self::AST_F32(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_F32 (&self)-> Option<&AST_F32> {
    
    match self{
      Self::AST_F32(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_F32_mut (&mut self)-> Option<&mut AST_F32> {
    
    match self{
      Self::AST_F32(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_F32{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Gotos{
  pub fork:Option<Box<Fork>>, 
  pub goto:Option<Box<Goto>>, 
  pub pushes:Vec<Box<Push>>, 
}

impl Gotos{
  
  pub fn new (fork: Option<Box<Fork>>, goto: Option<Box<Goto>>, pushes: Vec<Box<Push>>)-> Self {
    
    Self{
      fork,
      goto,
      pushes,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Gotos
  }
}

impl ASTNode{
  
  pub fn to_Gotos (self)-> Box::<Gotos> {
    
    match self{
      Self::Gotos(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Gotos (&self)-> Option<&Gotos> {
    
    match self{
      Self::Gotos(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Gotos_mut (&mut self)-> Option<&mut Gotos> {
    
    match self{
      Self::Gotos(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Gotos{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.fork.hash(hasher);
    self.goto.hash(hasher);
    
    for val in &self.pushes{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TerminalMatches{
  pub matches:Vec<ASTNode>, 
}

impl TerminalMatches{
  
  pub fn new (matches: Vec<ASTNode>)-> Self {
    
    Self{
      matches,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TerminalMatches
  }
}

impl ASTNode{
  
  pub fn to_TerminalMatches (self)-> Box::<TerminalMatches> {
    
    match self{
      Self::TerminalMatches(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TerminalMatches (&self)-> Option<&TerminalMatches> {
    
    match self{
      Self::TerminalMatches(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TerminalMatches_mut (&mut self)-> Option<&mut TerminalMatches> {
    
    match self{
      Self::TerminalMatches(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TerminalMatches{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.matches{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTerminal_Import_Symbol{
  pub module:String, 
  pub name:String, 
  pub tok: Token, 
}

impl NonTerminal_Import_Symbol{
  
  pub fn new (module: String, name: String, tok: Token)-> Self {
    
    Self{
      module,
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NonTerminal_Import_Symbol
  }
}

impl ASTNode{
  
  pub fn to_NonTerminal_Import_Symbol (self)-> Box::<NonTerminal_Import_Symbol> {
    
    match self{
      Self::NonTerminal_Import_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NonTerminal_Import_Symbol (&self)-> Option<&NonTerminal_Import_Symbol> {
    
    match self{
      Self::NonTerminal_Import_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NonTerminal_Import_Symbol_mut (&mut self)-> Option<&mut NonTerminal_Import_Symbol> {
    
    match self{
      Self::NonTerminal_Import_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NonTerminal_Import_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.module.hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Token{
  pub range:Option<Box<Range>>, 
}

impl AST_Token{
  
  pub fn new (range: Option<Box<Range>>)-> Self {
    
    Self{
      range,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Token
  }
}

impl ASTNode{
  
  pub fn to_AST_Token (self)-> Box::<AST_Token> {
    
    match self{
      Self::AST_Token(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Token (&self)-> Option<&AST_Token> {
    
    match self{
      Self::AST_Token(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Token_mut (&mut self)-> Option<&mut AST_Token> {
    
    match self{
      Self::AST_Token(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Token{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.range.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Shift{
  pub ptr_type:String, 
  pub skip:bool, 
  pub tok: Token, 
}

impl Shift{
  
  pub fn new (ptr_type: String, skip: bool, tok: Token)-> Self {
    
    Self{
      ptr_type,
      skip,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Shift
  }
}

impl ASTNode{
  
  pub fn to_Shift (self)-> Box::<Shift> {
    
    match self{
      Self::Shift(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Shift (&self)-> Option<&Shift> {
    
    match self{
      Self::Shift(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Shift_mut (&mut self)-> Option<&mut Shift> {
    
    match self{
      Self::Shift(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Shift{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ptr_type.hash(hasher);
    self.skip.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Export{
  pub nonterminal:ASTNode, 
  pub reference:String, 
}

impl Export{
  
  pub fn new (nonterminal: ASTNode, reference: String)-> Self {
    
    Self{
      nonterminal,
      reference,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Export
  }
}

impl ASTNode{
  
  pub fn to_Export (self)-> Box::<Export> {
    
    match self{
      Self::Export(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Export (&self)-> Option<&Export> {
    
    match self{
      Self::Export(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Export_mut (&mut self)-> Option<&mut Export> {
    
    match self{
      Self::Export(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Export{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.nonterminal.hash(hasher);
    self.reference.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Name{
  pub name:String, 
}

impl Name{
  
  pub fn new (name: String)-> Self {
    
    Self{
      name,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Name
  }
}

impl ASTNode{
  
  pub fn to_Name (self)-> Box::<Name> {
    
    match self{
      Self::Name(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Name (&self)-> Option<&Name> {
    
    match self{
      Self::Name(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Name_mut (&mut self)-> Option<&mut Name> {
    
    match self{
      Self::Name(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Name{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_BOOL{
  pub initializer:Option<Box<Init>>, 
  pub value:bool, 
  pub tok: Token, 
}

impl AST_BOOL{
  
  pub fn new (initializer: Option<Box<Init>>, value: bool, tok: Token)-> Self {
    
    Self{
      initializer,
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_BOOL
  }
}

impl ASTNode{
  
  pub fn to_AST_BOOL (self)-> Box::<AST_BOOL> {
    
    match self{
      Self::AST_BOOL(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_BOOL (&self)-> Option<&AST_BOOL> {
    
    match self{
      Self::AST_BOOL(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_BOOL_mut (&mut self)-> Option<&mut AST_BOOL> {
    
    match self{
      Self::AST_BOOL(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_BOOL{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTerminal_Terminal_Symbol{
  pub nonterminal:ASTNode, 
  pub tok: Token, 
}

impl NonTerminal_Terminal_Symbol{
  
  pub fn new (nonterminal: ASTNode, tok: Token)-> Self {
    
    Self{
      nonterminal,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NonTerminal_Terminal_Symbol
  }
}

impl ASTNode{
  
  pub fn to_NonTerminal_Terminal_Symbol (self)-> Box::<NonTerminal_Terminal_Symbol> {
    
    match self{
      Self::NonTerminal_Terminal_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NonTerminal_Terminal_Symbol (&self)-> Option<&NonTerminal_Terminal_Symbol> {
    
    match self{
      Self::NonTerminal_Terminal_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NonTerminal_Terminal_Symbol_mut (&mut self)-> Option<&mut NonTerminal_Terminal_Symbol> {
    
    match self{
      Self::NonTerminal_Terminal_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NonTerminal_Terminal_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.nonterminal.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Fail{
  pub tok: Token, 
}

impl Fail{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Fail
  }
}

impl ASTNode{
  
  pub fn to_Fail (self)-> Box::<Fail> {
    
    match self{
      Self::Fail(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Fail (&self)-> Option<&Fail> {
    
    match self{
      Self::Fail(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Fail_mut (&mut self)-> Option<&mut Fail> {
    
    match self{
      Self::Fail(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Fail{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Statements{
  pub statements:Vec<ASTNode>, 
  pub tok: Token, 
}

impl AST_Statements{
  
  pub fn new (statements: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      statements,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Statements
  }
}

impl ASTNode{
  
  pub fn to_AST_Statements (self)-> Box::<AST_Statements> {
    
    match self{
      Self::AST_Statements(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Statements (&self)-> Option<&AST_Statements> {
    
    match self{
      Self::AST_Statements(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Statements_mut (&mut self)-> Option<&mut AST_Statements> {
    
    match self{
      Self::AST_Statements(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Statements{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.statements{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TemplateRules{
  pub name_sym:Box<NonTerminal_Symbol>, 
  pub rules:Vec<Box<Rule>>, 
  pub template_params:Vec<ASTNode>, 
  pub tok: Token, 
}

impl TemplateRules{
  
  pub fn new (name_sym: Box<NonTerminal_Symbol>, rules: Vec<Box<Rule>>, template_params: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      template_params,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TemplateRules
  }
}

impl ASTNode{
  
  pub fn to_TemplateRules (self)-> Box::<TemplateRules> {
    
    match self{
      Self::TemplateRules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TemplateRules (&self)-> Option<&TemplateRules> {
    
    match self{
      Self::TemplateRules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TemplateRules_mut (&mut self)-> Option<&mut TemplateRules> {
    
    match self{
      Self::TemplateRules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TemplateRules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name_sym.hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
    
    for val in &self.template_params{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_U8{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_U8{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_U8
  }
}

impl ASTNode{
  
  pub fn to_AST_U8 (self)-> Box::<AST_U8> {
    
    match self{
      Self::AST_U8(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_U8 (&self)-> Option<&AST_U8> {
    
    match self{
      Self::AST_U8(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_U8_mut (&mut self)-> Option<&mut AST_U8> {
    
    match self{
      Self::AST_U8(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_U8{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Ignore{
  pub symbols:Vec<ASTNode>, 
}

impl Ignore{
  
  pub fn new (symbols: Vec<ASTNode>)-> Self {
    
    Self{
      symbols,
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
    
    for val in &self.symbols{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_STRING{
  pub initializer:Option<Box<Init>>, 
  pub value:String, 
  pub tok: Token, 
}

impl AST_STRING{
  
  pub fn new (initializer: Option<Box<Init>>, value: String, tok: Token)-> Self {
    
    Self{
      initializer,
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_STRING
  }
}

impl ASTNode{
  
  pub fn to_AST_STRING (self)-> Box::<AST_STRING> {
    
    match self{
      Self::AST_STRING(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_STRING (&self)-> Option<&AST_STRING> {
    
    match self{
      Self::AST_STRING(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_STRING_mut (&mut self)-> Option<&mut AST_STRING> {
    
    match self{
      Self::AST_STRING(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_STRING{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_U32{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_U32{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_U32
  }
}

impl ASTNode{
  
  pub fn to_AST_U32 (self)-> Box::<AST_U32> {
    
    match self{
      Self::AST_U32(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_U32 (&self)-> Option<&AST_U32> {
    
    match self{
      Self::AST_U32(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_U32_mut (&mut self)-> Option<&mut AST_U32> {
    
    match self{
      Self::AST_U32(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_U32{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct State{
  pub catches:bool, 
  pub id:Box<NonTerminal_Symbol>, 
  pub statement:Box<Statement>, 
  pub tok: Token, 
}

impl State{
  
  pub fn new (catches: bool, id: Box<NonTerminal_Symbol>, statement: Box<Statement>, tok: Token)-> Self {
    
    Self{
      catches,
      id,
      statement,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::State
  }
}

impl ASTNode{
  
  pub fn to_State (self)-> Box::<State> {
    
    match self{
      Self::State(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_State (&self)-> Option<&State> {
    
    match self{
      Self::State(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_State_mut (&mut self)-> Option<&mut State> {
    
    match self{
      Self::State(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for State{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.catches.hash(hasher);
    self.id.hash(hasher);
    self.statement.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct List_Rules{
  pub optional:bool, 
  pub symbol:ASTNode, 
  pub terminal_symbol:Option<ASTNode>, 
  pub tok: Token, 
}

impl List_Rules{
  
  pub fn new (optional: bool, symbol: ASTNode, terminal_symbol: Option<ASTNode>, tok: Token)-> Self {
    
    Self{
      optional,
      symbol,
      terminal_symbol,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::List_Rules
  }
}

impl ASTNode{
  
  pub fn to_List_Rules (self)-> Box::<List_Rules> {
    
    match self{
      Self::List_Rules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_List_Rules (&self)-> Option<&List_Rules> {
    
    match self{
      Self::List_Rules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_List_Rules_mut (&mut self)-> Option<&mut List_Rules> {
    
    match self{
      Self::List_Rules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for List_Rules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.optional.hash(hasher);
    self.symbol.hash(hasher);
    self.terminal_symbol.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_I64{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_I64{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_I64
  }
}

impl ASTNode{
  
  pub fn to_AST_I64 (self)-> Box::<AST_I64> {
    
    match self{
      Self::AST_I64(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_I64 (&self)-> Option<&AST_I64> {
    
    match self{
      Self::AST_I64(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_I64_mut (&mut self)-> Option<&mut AST_I64> {
    
    match self{
      Self::AST_I64(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_I64{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DefaultMatch{
  pub statement:Box<Statement>, 
}

impl DefaultMatch{
  
  pub fn new (statement: Box<Statement>)-> Self {
    
    Self{
      statement,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::DefaultMatch
  }
}

impl ASTNode{
  
  pub fn to_DefaultMatch (self)-> Box::<DefaultMatch> {
    
    match self{
      Self::DefaultMatch(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_DefaultMatch (&self)-> Option<&DefaultMatch> {
    
    match self{
      Self::DefaultMatch(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_DefaultMatch_mut (&mut self)-> Option<&mut DefaultMatch> {
    
    match self{
      Self::DefaultMatch(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for DefaultMatch{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.statement.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SetLine{
  pub tok: Token, 
}

impl SetLine{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SetLine
  }
}

impl ASTNode{
  
  pub fn to_SetLine (self)-> Box::<SetLine> {
    
    match self{
      Self::SetLine(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_SetLine (&self)-> Option<&SetLine> {
    
    match self{
      Self::SetLine(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SetLine_mut (&mut self)-> Option<&mut SetLine> {
    
    match self{
      Self::SetLine(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SetLine{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_U16{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_U16{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_U16
  }
}

impl ASTNode{
  
  pub fn to_AST_U16 (self)-> Box::<AST_U16> {
    
    match self{
      Self::AST_U16(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_U16 (&self)-> Option<&AST_U16> {
    
    match self{
      Self::AST_U16(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_U16_mut (&mut self)-> Option<&mut AST_U16> {
    
    match self{
      Self::AST_U16(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_U16{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct PegRules{
  pub name_sym:Box<NonTerminal_Symbol>, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl PegRules{
  
  pub fn new (name_sym: Box<NonTerminal_Symbol>, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::PegRules
  }
}

impl ASTNode{
  
  pub fn to_PegRules (self)-> Box::<PegRules> {
    
    match self{
      Self::PegRules(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_PegRules (&self)-> Option<&PegRules> {
    
    match self{
      Self::PegRules(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_PegRules_mut (&mut self)-> Option<&mut PegRules> {
    
    match self{
      Self::PegRules(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for PegRules{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name_sym.hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_STRUCT_TEMPLATE_NAME{
  pub typ:String, 
}

impl AST_STRUCT_TEMPLATE_NAME{
  
  pub fn new (typ: String)-> Self {
    
    Self{
      typ,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_STRUCT_TEMPLATE_NAME
  }
}

impl ASTNode{
  
  pub fn to_AST_STRUCT_TEMPLATE_NAME (self)-> Box::<AST_STRUCT_TEMPLATE_NAME> {
    
    match self{
      Self::AST_STRUCT_TEMPLATE_NAME(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_STRUCT_TEMPLATE_NAME (&self)-> Option<&AST_STRUCT_TEMPLATE_NAME> {
    
    match self{
      Self::AST_STRUCT_TEMPLATE_NAME(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_STRUCT_TEMPLATE_NAME_mut (&mut self)-> Option<&mut AST_STRUCT_TEMPLATE_NAME> {
    
    match self{
      Self::AST_STRUCT_TEMPLATE_NAME(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_STRUCT_TEMPLATE_NAME{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.typ.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NotEmptySet{
  pub symbols:Vec<ASTNode>, 
  pub unordered:bool, 
  pub tok: Token, 
}

impl NotEmptySet{
  
  pub fn new (symbols: Vec<ASTNode>, unordered: bool, tok: Token)-> Self {
    
    Self{
      symbols,
      unordered,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::NotEmptySet
  }
}

impl ASTNode{
  
  pub fn to_NotEmptySet (self)-> Box::<NotEmptySet> {
    
    match self{
      Self::NotEmptySet(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_NotEmptySet (&self)-> Option<&NotEmptySet> {
    
    match self{
      Self::NotEmptySet(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_NotEmptySet_mut (&mut self)-> Option<&mut NotEmptySet> {
    
    match self{
      Self::NotEmptySet(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for NotEmptySet{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.symbols{
      val.hash(hasher);
    }
    self.unordered.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Accept{
  pub tok: Token, 
}

impl Accept{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Accept
  }
}

impl ASTNode{
  
  pub fn to_Accept (self)-> Box::<Accept> {
    
    match self{
      Self::Accept(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Accept (&self)-> Option<&Accept> {
    
    match self{
      Self::Accept(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Accept_mut (&mut self)-> Option<&mut Accept> {
    
    match self{
      Self::Accept(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Accept{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ClassSymbol{
  pub val:String, 
  pub tok: Token, 
}

impl ClassSymbol{
  
  pub fn new (val: String, tok: Token)-> Self {
    
    Self{
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::ClassSymbol
  }
}

impl ASTNode{
  
  pub fn to_ClassSymbol (self)-> Box::<ClassSymbol> {
    
    match self{
      Self::ClassSymbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_ClassSymbol (&self)-> Option<&ClassSymbol> {
    
    match self{
      Self::ClassSymbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_ClassSymbol_mut (&mut self)-> Option<&mut ClassSymbol> {
    
    match self{
      Self::ClassSymbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for ClassSymbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ReduceRaw{
  pub len:u32, 
  pub nonterminal_id:u32, 
  pub rule_id:u32, 
  pub tok: Token, 
}

impl ReduceRaw{
  
  pub fn new (len: u32, nonterminal_id: u32, rule_id: u32, tok: Token)-> Self {
    
    Self{
      len,
      nonterminal_id,
      rule_id,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::ReduceRaw
  }
}

impl ASTNode{
  
  pub fn to_ReduceRaw (self)-> Box::<ReduceRaw> {
    
    match self{
      Self::ReduceRaw(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_ReduceRaw (&self)-> Option<&ReduceRaw> {
    
    match self{
      Self::ReduceRaw(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_ReduceRaw_mut (&mut self)-> Option<&mut ReduceRaw> {
    
    match self{
      Self::ReduceRaw(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for ReduceRaw{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.len.hash(hasher);
    self.nonterminal_id.hash(hasher);
    self.rule_id.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Statement{
  pub branch:Option<ASTNode>, 
  pub non_branch:Vec<ASTNode>, 
  pub pop:Option<Box<Pop>>, 
  pub transitive:Option<ASTNode>, 
}

impl Statement{
  
  pub fn new (branch: Option<ASTNode>, non_branch: Vec<ASTNode>, pop: Option<Box<Pop>>, transitive: Option<ASTNode>)-> Self {
    
    Self{
      branch,
      non_branch,
      pop,
      transitive,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Statement
  }
}

impl ASTNode{
  
  pub fn to_Statement (self)-> Box::<Statement> {
    
    match self{
      Self::Statement(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Statement (&self)-> Option<&Statement> {
    
    match self{
      Self::Statement(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Statement_mut (&mut self)-> Option<&mut Statement> {
    
    match self{
      Self::Statement(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Statement{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.branch.hash(hasher);
    
    for val in &self.non_branch{
      val.hash(hasher);
    }
    self.pop.hash(hasher);
    self.transitive.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_F64{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_F64{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_F64
  }
}

impl ASTNode{
  
  pub fn to_AST_F64 (self)-> Box::<AST_F64> {
    
    match self{
      Self::AST_F64(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_F64 (&self)-> Option<&AST_F64> {
    
    match self{
      Self::AST_F64(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_F64_mut (&mut self)-> Option<&mut AST_F64> {
    
    match self{
      Self::AST_F64(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_F64{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Map{
  pub key:ASTNode, 
  pub val:ASTNode, 
  pub tok: Token, 
}

impl AST_Map{
  
  pub fn new (key: ASTNode, val: ASTNode, tok: Token)-> Self {
    
    Self{
      key,
      val,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Map
  }
}

impl ASTNode{
  
  pub fn to_AST_Map (self)-> Box::<AST_Map> {
    
    match self{
      Self::AST_Map(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Map (&self)-> Option<&AST_Map> {
    
    match self{
      Self::AST_Map(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Map_mut (&mut self)-> Option<&mut AST_Map> {
    
    match self{
      Self::AST_Map(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Map{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.key.hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Range{
  pub end_trim:i32, 
  pub start_trim:i32, 
}

impl Range{
  
  pub fn new (end_trim: i32, start_trim: i32)-> Self {
    
    Self{
      end_trim,
      start_trim,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Range
  }
}

impl ASTNode{
  
  pub fn to_Range (self)-> Box::<Range> {
    
    match self{
      Self::Range(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Range (&self)-> Option<&Range> {
    
    match self{
      Self::Range(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Range_mut (&mut self)-> Option<&mut Range> {
    
    match self{
      Self::Range(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Range{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.end_trim.hash(hasher);
    self.start_trim.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct IntMatch{
  pub statement:Box<Statement>, 
  pub vals:Vec<u64>, 
}

impl IntMatch{
  
  pub fn new (statement: Box<Statement>, vals: Vec<u64>)-> Self {
    
    Self{
      statement,
      vals,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::IntMatch
  }
}

impl ASTNode{
  
  pub fn to_IntMatch (self)-> Box::<IntMatch> {
    
    match self{
      Self::IntMatch(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_IntMatch (&self)-> Option<&IntMatch> {
    
    match self{
      Self::IntMatch(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_IntMatch_mut (&mut self)-> Option<&mut IntMatch> {
    
    match self{
      Self::IntMatch(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for IntMatch{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.statement.hash(hasher);
    self.vals.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_I16{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_I16{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_I16
  }
}

impl ASTNode{
  
  pub fn to_AST_I16 (self)-> Box::<AST_I16> {
    
    match self{
      Self::AST_I16(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_I16 (&self)-> Option<&AST_I16> {
    
    match self{
      Self::AST_I16(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_I16_mut (&mut self)-> Option<&mut AST_I16> {
    
    match self{
      Self::AST_I16(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_I16{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Push{
  pub name:String, 
  pub nonterminal:ASTNode, 
  pub tok: Token, 
}

impl Push{
  
  pub fn new (name: String, nonterminal: ASTNode, tok: Token)-> Self {
    
    Self{
      name,
      nonterminal,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Push
  }
}

impl ASTNode{
  
  pub fn to_Push (self)-> Box::<Push> {
    
    match self{
      Self::Push(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Push (&self)-> Option<&Push> {
    
    match self{
      Self::Push(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Push_mut (&mut self)-> Option<&mut Push> {
    
    match self{
      Self::Push(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Push{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
    self.nonterminal.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_NUMBER{
  pub value:f64, 
}

impl AST_NUMBER{
  
  pub fn new (value: f64)-> Self {
    
    Self{
      value,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_NUMBER
  }
}

impl ASTNode{
  
  pub fn to_AST_NUMBER (self)-> Box::<AST_NUMBER> {
    
    match self{
      Self::AST_NUMBER(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_NUMBER (&self)-> Option<&AST_NUMBER> {
    
    match self{
      Self::AST_NUMBER(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_NUMBER_mut (&mut self)-> Option<&mut AST_NUMBER> {
    
    match self{
      Self::AST_NUMBER(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_NUMBER{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.to_le_bytes().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Struct{
  pub props:Vec<ASTNode>, 
  pub typ:String, 
  pub tok: Token, 
}

impl AST_Struct{
  
  pub fn new (props: Vec<ASTNode>, typ: String, tok: Token)-> Self {
    
    Self{
      props,
      typ,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Struct
  }
}

impl ASTNode{
  
  pub fn to_AST_Struct (self)-> Box::<AST_Struct> {
    
    match self{
      Self::AST_Struct(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Struct (&self)-> Option<&AST_Struct> {
    
    match self{
      Self::AST_Struct(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Struct_mut (&mut self)-> Option<&mut AST_Struct> {
    
    match self{
      Self::AST_Struct(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Struct{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.props{
      val.hash(hasher);
    }
    self.typ.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Peek{
  pub ptr_type:String, 
  pub skip:bool, 
  pub tok: Token, 
}

impl Peek{
  
  pub fn new (ptr_type: String, skip: bool, tok: Token)-> Self {
    
    Self{
      ptr_type,
      skip,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Peek
  }
}

impl ASTNode{
  
  pub fn to_Peek (self)-> Box::<Peek> {
    
    match self{
      Self::Peek(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Peek (&self)-> Option<&Peek> {
    
    match self{
      Self::Peek(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Peek_mut (&mut self)-> Option<&mut Peek> {
    
    match self{
      Self::Peek(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Peek{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ptr_type.hash(hasher);
    self.skip.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Matches{
  pub matches:Vec<ASTNode>, 
  pub mode:String, 
  pub scanner:String, 
  pub tok: Token, 
}

impl Matches{
  
  pub fn new (matches: Vec<ASTNode>, mode: String, scanner: String, tok: Token)-> Self {
    
    Self{
      matches,
      mode,
      scanner,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Matches
  }
}

impl ASTNode{
  
  pub fn to_Matches (self)-> Box::<Matches> {
    
    match self{
      Self::Matches(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Matches (&self)-> Option<&Matches> {
    
    match self{
      Self::Matches(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Matches_mut (&mut self)-> Option<&mut Matches> {
    
    match self{
      Self::Matches(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Matches{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.matches{
      val.hash(hasher);
    }
    self.mode.hash(hasher);
    self.scanner.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ProductionMatches{
  pub matches:Vec<ASTNode>, 
}

impl ProductionMatches{
  
  pub fn new (matches: Vec<ASTNode>)-> Self {
    
    Self{
      matches,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::ProductionMatches
  }
}

impl ASTNode{
  
  pub fn to_ProductionMatches (self)-> Box::<ProductionMatches> {
    
    match self{
      Self::ProductionMatches(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_ProductionMatches (&self)-> Option<&ProductionMatches> {
    
    match self{
      Self::ProductionMatches(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_ProductionMatches_mut (&mut self)-> Option<&mut ProductionMatches> {
    
    match self{
      Self::ProductionMatches(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for ProductionMatches{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.matches{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TermMatch{
  pub statement:Box<Statement>, 
  pub sym:ASTNode, 
}

impl TermMatch{
  
  pub fn new (statement: Box<Statement>, sym: ASTNode)-> Self {
    
    Self{
      statement,
      sym,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TermMatch
  }
}

impl ASTNode{
  
  pub fn to_TermMatch (self)-> Box::<TermMatch> {
    
    match self{
      Self::TermMatch(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TermMatch (&self)-> Option<&TermMatch> {
    
    match self{
      Self::TermMatch(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TermMatch_mut (&mut self)-> Option<&mut TermMatch> {
    
    match self{
      Self::TermMatch(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TermMatch{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.statement.hash(hasher);
    self.sym.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct DEFINED_TYPE_NUM{
}

impl DEFINED_TYPE_NUM{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::DEFINED_TYPE_NUM
  }
}

impl ASTNode{
  
  pub fn to_DEFINED_TYPE_NUM (self)-> Box::<DEFINED_TYPE_NUM> {
    
    match self{
      Self::DEFINED_TYPE_NUM(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_DEFINED_TYPE_NUM (&self)-> Option<&DEFINED_TYPE_NUM> {
    
    match self{
      Self::DEFINED_TYPE_NUM(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_DEFINED_TYPE_NUM_mut (&mut self)-> Option<&mut DEFINED_TYPE_NUM> {
    
    match self{
      Self::DEFINED_TYPE_NUM(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for DEFINED_TYPE_NUM{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Rule{
  pub ast:Option<Box<Ascript>>, 
  pub symbols:Vec<ASTNode>, 
  pub tok: Token, 
}

impl Rule{
  
  pub fn new (ast: Option<Box<Ascript>>, symbols: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      ast,
      symbols,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Rule
  }
}

impl ASTNode{
  
  pub fn to_Rule (self)-> Box::<Rule> {
    
    match self{
      Self::Rule(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Rule (&self)-> Option<&Rule> {
    
    match self{
      Self::Rule(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Rule_mut (&mut self)-> Option<&mut Rule> {
    
    match self{
      Self::Rule(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Rule{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ast.hash(hasher);
    
    for val in &self.symbols{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AnnotatedSymbol{
  pub is_optional:bool, 
  pub precedence:Option<Box<Precedence>>, 
  pub reference:String, 
  pub symbol:ASTNode, 
  pub tok: Token, 
}

impl AnnotatedSymbol{
  
  pub fn new (is_optional: bool, precedence: Option<Box<Precedence>>, reference: String, symbol: ASTNode, tok: Token)-> Self {
    
    Self{
      is_optional,
      precedence,
      reference,
      symbol,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AnnotatedSymbol
  }
}

impl ASTNode{
  
  pub fn to_AnnotatedSymbol (self)-> Box::<AnnotatedSymbol> {
    
    match self{
      Self::AnnotatedSymbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AnnotatedSymbol (&self)-> Option<&AnnotatedSymbol> {
    
    match self{
      Self::AnnotatedSymbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AnnotatedSymbol_mut (&mut self)-> Option<&mut AnnotatedSymbol> {
    
    match self{
      Self::AnnotatedSymbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AnnotatedSymbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_optional.hash(hasher);
    self.precedence.hash(hasher);
    self.reference.hash(hasher);
    self.symbol.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AST_Property{
  pub id:String, 
  pub named_reference:String, 
  pub value:Option<ASTNode>, 
  pub tok: Token, 
}

impl AST_Property{
  
  pub fn new (id: String, named_reference: String, value: Option<ASTNode>, tok: Token)-> Self {
    
    Self{
      id,
      named_reference,
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Property
  }
}

impl ASTNode{
  
  pub fn to_AST_Property (self)-> Box::<AST_Property> {
    
    match self{
      Self::AST_Property(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Property (&self)-> Option<&AST_Property> {
    
    match self{
      Self::AST_Property(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Property_mut (&mut self)-> Option<&mut AST_Property> {
    
    match self{
      Self::AST_Property(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Property{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
    self.named_reference.hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Ascript{
  pub ast:ASTNode, 
  pub tok: Token, 
}

impl Ascript{
  
  pub fn new (ast: ASTNode, tok: Token)-> Self {
    
    Self{
      ast,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Ascript
  }
}

impl ASTNode{
  
  pub fn to_Ascript (self)-> Box::<Ascript> {
    
    match self{
      Self::Ascript(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Ascript (&self)-> Option<&Ascript> {
    
    match self{
      Self::Ascript(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Ascript_mut (&mut self)-> Option<&mut Ascript> {
    
    match self{
      Self::Ascript(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Ascript{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ast.hash(hasher);
  }
}


/* sym::nonterminal_symbol^id "=>" statement

        :ast { t_State, id, statement, tok } */
fn reducer_000 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_1 = ref_0;
  let obj_0_1 = obj_0_1.to_NonTerminal_Symbol();
  let obj_2_2 = ref_2;
  let obj_2_2 = obj_2_2.to_Statement();
  let var_4_0 = State::new(
    false,
    obj_0_1,
    obj_2_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::State(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* sym::nonterminal_symbol^id "=!>" statement

        :ast { t_State, catches:true, id, statement, tok } */
fn reducer_001 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_0_1 = ref_0;
  let obj_0_1 = obj_0_1.to_NonTerminal_Symbol();
  let obj_2_2 = ref_2;
  let obj_2_2 = obj_2_2.to_Statement();
  let var_5_0 = State::new(
    obj_4_0,
    obj_0_1,
    obj_2_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::State(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* ( escaped_vals :ast str($1) | escaped )(+) */
fn reducer_002 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* escaped_vals :ast str($1) */
fn reducer_003 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* escaped */
fn reducer_004 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( escaped_vals :ast str($1) | escaped ) */
fn reducer_005 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_string();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( escaped_vals :ast str($1) | escaped )(+) */
fn reducer_006 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1.to_string();
  let mut obj_0_0 = ref_0.into_strings();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* [preamble(*) ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+)]

        :ast { t_Grammar, preamble:$1, rules:$2, tok } */
fn reducer_007 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_0 = ref_0.into_nodes();
  let obj_1_1 = ref_1.into_nodes();
  let var_3_0 = Grammar::new(
    obj_0_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Grammar(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* [preamble(*) ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+)]

        :ast { t_Grammar, preamble:$1, rules:$2, tok } */
fn reducer_008 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_1 = ref_0.into_nodes();
  let var_2_0 = Grammar::new(
    vec![],
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Grammar(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* preamble */
fn reducer_009 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* preamble(*) */
fn reducer_010 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* cf_rules */
fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* peg_rules */
fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* append_rules */
fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ir::state */
fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* template_rule */
fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_020 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_021 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_022 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_023 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_024 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_025 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* id $ :ast $1 */
fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_0 = ref_0;
  slots.assign(0, AstSlot(obj_0_0, __rule_rng__, TokenRange::default()));
}


/* num $ :ast $1 */
fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_0 = ref_0;
  slots.assign(0, AstSlot(obj_0_0, __rule_rng__, TokenRange::default()));
}


/* string_convert */
fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* numeric_convert */
fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* bool_convert */
fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* vector */
fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* add */
fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* map */
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" type_identifier^t ( "," struct_prop(+",") )? '}'
        :ast { t_AST_Struct, typ: str($t), props:$3, tok } */
fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_3_0 = ref_3.into_nodes();
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_string();
  let var_6_0 = AST_Struct::new(
    obj_3_0,
    tok_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "{" type_identifier^t ( "," struct_prop(+",") )? '}'
        :ast { t_AST_Struct, typ: str($t), props:$3, tok } */
fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_string();
  let var_4_0 = AST_Struct::new(
    vec![],
    tok_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* struct_prop */
fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* struct_prop(+",") */
fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "," struct_prop(+",") */
fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* tok::id

            :ast { t_NonTerminal_Symbol, name:str($1), tok} */
fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_2_0 = NonTerminal_Symbol::new(
    tok_0_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NonTerminal_Symbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, _, _) = slots.take(4);
  slots.take(5);
  let AstSlot (ref_6, __tok_rng_6, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_6;
  let obj_6_0 = ref_6;
  let obj_2_1 = ref_2.into_nodes();
  let obj_4_2 = ref_4;
  let obj_4_2 = obj_4_2.to_Pop();
  let obj_0_3 = ref_0;
  let var_8_0 = Statement::new(
    Some(obj_6_0),
    obj_2_1,
    Some(obj_4_2),
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_8_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_4_0 = ref_4;
  let obj_2_2 = ref_2;
  let obj_2_2 = obj_2_2.to_Pop();
  let obj_0_3 = ref_0;
  let var_6_0 = Statement::new(
    Some(obj_4_0),
    vec![],
    Some(obj_2_2),
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_4_0 = ref_4;
  let obj_2_1 = ref_2.into_nodes();
  let obj_0_3 = ref_0;
  let var_6_0 = Statement::new(
    Some(obj_4_0),
    obj_2_1,
    None,
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let obj_0_3 = ref_0;
  let var_4_0 = Statement::new(
    Some(obj_2_0),
    vec![],
    None,
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_2_1 = ref_2.into_nodes();
  let obj_4_2 = ref_4;
  let obj_4_2 = obj_4_2.to_Pop();
  let obj_0_3 = ref_0;
  let var_6_0 = Statement::new(
    None,
    obj_2_1,
    Some(obj_4_2),
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_2 = ref_2;
  let obj_2_2 = obj_2_2.to_Pop();
  let obj_0_3 = ref_0;
  let var_4_0 = Statement::new(
    None,
    vec![],
    Some(obj_2_2),
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_048 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_1 = ref_2.into_nodes();
  let obj_0_3 = ref_0;
  let var_4_0 = Statement::new(
    None,
    obj_2_1,
    None,
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, pop, branch } */
fn reducer_049 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_3 = ref_0;
  let var_2_0 = Statement::new(
    None,
    vec![],
    None,
    Some(obj_0_3),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then")^non_branch
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, non_branch, branch, pop } */
fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_4_0 = ref_4;
  let obj_0_1 = ref_0.into_nodes();
  let obj_2_2 = ref_2;
  let obj_2_2 = obj_2_2.to_Pop();
  let var_6_0 = Statement::new(
    Some(obj_4_0),
    obj_0_1,
    Some(obj_2_2),
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then")^non_branch
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, non_branch, branch, pop } */
fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let obj_0_1 = ref_0.into_nodes();
  let var_4_0 = Statement::new(
    Some(obj_2_0),
    obj_0_1,
    None,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then")^non_branch
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, non_branch, branch, pop } */
fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_1 = ref_0.into_nodes();
  let obj_2_2 = ref_2;
  let obj_2_2 = obj_2_2.to_Pop();
  let var_4_0 = Statement::new(
    None,
    obj_0_1,
    Some(obj_2_2),
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then")^non_branch
     ( "then" pop^pop )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, non_branch, branch, pop } */
fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_1 = ref_0.into_nodes();
  let var_2_0 = Statement::new(
    None,
    obj_0_1,
    None,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( pop^pop "then" )?
      branch_statement^branch 

     :ast { t_Statement, branch, pop } */
fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let obj_0_2 = ref_0;
  let obj_0_2 = obj_0_2.to_Pop();
  let var_4_0 = Statement::new(
    Some(obj_2_0),
    vec![],
    Some(obj_0_2),
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* ( pop^pop "then" )?
      branch_statement^branch 

     :ast { t_Statement, branch, pop } */
fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let var_2_0 = Statement::new(
    Some(obj_0_0),
    vec![],
    None,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* pop^pop :ast { t_Statement, pop } */
fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_2 = ref_0;
  let obj_0_2 = obj_0_2.to_Pop();
  let var_2_0 = Statement::new(
    None,
    vec![],
    Some(obj_0_2),
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement */
fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then") */
fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "then" non_branch_statement(+"then")^non_branch */
fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "then" pop^pop */
fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "then" branch_statement^branch */
fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* non_branch_statement */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then") */
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


/* "then" pop^pop */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "then" branch_statement^branch */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* pop^pop "then" */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:id */
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:nl */
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sp */
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:id */
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:nl */
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sp */
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* export_clause */
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_clause */
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* name_clause */
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ignore_clause */
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<>" sym::nonterminal_symbol^n ">" rules^r

        :ast { t_CFRules, name_sym:$n, rules: $r, tok } */
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_NonTerminal_Symbol();
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = CFRules::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::CFRules(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* ":>" sym::nonterminal_symbol^n ">" rules^r

        :ast { t_PegRules, name_sym:$n, rules: $r, tok } */
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_NonTerminal_Symbol();
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = PegRules::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::PegRules(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* '+' ">" sym::nonterminal^n ">" rules^r

        :ast { t_AppendRules,  name_sym:$n, rules: $r, tok } */
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_2_0 = ref_2;
  let obj_4_1 = ref_4.into_nodes();
  let var_6_0 = AppendRules::new(
    obj_2_0,
    obj_4_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AppendRules(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "<" template_param(+",")^p ">" sym::nonterminal_symbol^n ">" rules^r

        :ast { t_TemplateRules, name_sym:$n, template_params:$p, rules: $r, tok } */
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  slots.take(4);
  let AstSlot (ref_5, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let obj_3_0 = ref_3;
  let obj_3_0 = obj_3_0.to_NonTerminal_Symbol();
  let obj_5_1 = ref_5.into_nodes();
  let obj_1_2 = ref_1.into_nodes();
  let var_7_0 = TemplateRules::new(
    obj_3_0,
    obj_5_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    obj_1_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TemplateRules(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* template_param */
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* template_param(+",") */
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:identifier  :ast { t_DEFINED_TYPE_IDENT } */
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:number     :ast { t_DEFINED_TYPE_NUM } */
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_STRING, initializer: $2, tok  } */
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_STRING::new(
    Some(obj_1_0),
    Default::default(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_STRING, initializer: $2, tok  } */
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_STRING::new(
    None,
    Default::default(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u8"  convert_initializer?
        :ast { t_AST_U8,  initializer: $2, tok  } */
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_U8::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "u8"  convert_initializer?
        :ast { t_AST_U8,  initializer: $2, tok  } */
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u16" convert_initializer?
        :ast { t_AST_U16, initializer: $2, tok  } */
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_U16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "u16" convert_initializer?
        :ast { t_AST_U16, initializer: $2, tok  } */
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u32" convert_initializer?
        :ast { t_AST_U32, initializer: $2, tok  } */
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_U32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "u32" convert_initializer?
        :ast { t_AST_U32, initializer: $2, tok  } */
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u64" convert_initializer?
        :ast { t_AST_U64, initializer: $2, tok  } */
fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_U64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "u64" convert_initializer?
        :ast { t_AST_U64, initializer: $2, tok  } */
fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i8"  convert_initializer?
        :ast { t_AST_I8,  initializer: $2, tok  } */
fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_I8::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "i8"  convert_initializer?
        :ast { t_AST_I8,  initializer: $2, tok  } */
fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i16" convert_initializer?
        :ast { t_AST_I16, initializer: $2, tok  } */
fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_I16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "i16" convert_initializer?
        :ast { t_AST_I16, initializer: $2, tok  } */
fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i32" convert_initializer?
        :ast { t_AST_I32, initializer: $2, tok  } */
fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_I32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "i32" convert_initializer?
        :ast { t_AST_I32, initializer: $2, tok  } */
fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i64" convert_initializer?
        :ast { t_AST_I64, initializer: $2, tok  } */
fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_I64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "i64" convert_initializer?
        :ast { t_AST_I64, initializer: $2, tok  } */
fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "f32" convert_initializer?
        :ast { t_AST_F32, initializer: $2, tok  } */
fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_F32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "f32" convert_initializer?
        :ast { t_AST_F32, initializer: $2, tok  } */
fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_F32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "f64" convert_initializer?
        :ast { t_AST_F64, initializer: $2, tok  } */
fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_F64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "f64" convert_initializer?
        :ast { t_AST_F64, initializer: $2, tok  } */
fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_F64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "bool" convert_initializer?
        :ast { t_AST_BOOL,  initializer: $2, tok  } */
fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_BOOL::new(
    Some(obj_1_0),
    false,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "bool" convert_initializer?
        :ast { t_AST_BOOL,  initializer: $2, tok  } */
fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_BOOL::new(
    None,
    false,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "true" 
        :ast { t_AST_BOOL, value: true } */
fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_1 = true;
  let var_3_0 = AST_BOOL::new(
    None,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "false"
        :ast { t_AST_BOOL, value: false } */
fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_1 = false;
  let var_3_0 = AST_BOOL::new(
    None,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* token::int
        :ast { t_AST_NUMBER, value:f64($1) } */
fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_f64();
  let var_2_0 = AST_NUMBER::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_NUMBER(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* token::string
        :ast { t_AST_STRING, value:str($1), tok } */
fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let var_2_0 = AST_STRING::new(
    None,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "[" expression(*",") "]"
        :ast { t_AST_Vector, initializer: $2, tok  } */
fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = AST_Vector::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "[" expression(*",") "]"
        :ast { t_AST_Vector, initializer: $2, tok  } */
fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let var_3_0 = AST_Vector::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* expression */
fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*",") */
fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_126 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Range();
  let var_3_0 = AST_Token::new(
    Some(obj_1_0),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_127 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Range();
  let var_3_0 = AST_Token::new(
    Some(obj_1_0),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Range();
  let var_3_0 = AST_Token::new(
    Some(obj_1_0),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk" */
fn reducer_132 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "tok" */
fn reducer_133 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "token" */
fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* add "+"{1} expression{0}

        :ast { t_AST_Add, left: $1, right: $3, tok } */
fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = AST_Add::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Add(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* member */
fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "map" "(" expression^k ',' expression^v ')'

        :ast { t_AST_Map, key: $k, val: $v, tok } */
fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let obj_2_0 = ref_2;
  let obj_4_1 = ref_4;
  let var_7_0 = AST_Map::new(
    obj_2_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Map(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* identifier ":" expression
        :ast { t_AST_Property, id:str($1), value:$3, tok } */
fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_2 = ref_2;
  let var_4_0 = AST_Property::new(
    tok_0_0,
    Default::default(),
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* identifier ":" struct
        :ast { t_AST_Property, id:str($1), value:$3, tok } */
fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_2 = ref_2;
  let var_4_0 = AST_Property::new(
    tok_0_0,
    Default::default(),
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* identifier
        :ast { t_AST_Property, id:str($1), named_reference: str($1), tok } */
fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_0_1 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let var_2_0 = AST_Property::new(
    tok_0_0,
    tok_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:( 't' "_"{:9999} ) identifier */
fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* tk:id_tok */
fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok } */
fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  slots.take(5);
  slots.take(6);
  let AstSlot (_, __tok_rng_7, _) = slots.take(7);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_7;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_4_1 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_1 = tok_4_1.to_u32();
  let tok_7_2 = __tok_rng_7.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_7_2 = tok_7_2.to_u32();
  let var_9_0 = ReduceRaw::new(
    tok_1_0,
    tok_4_1,
    tok_7_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ReduceRaw(Box::new(var_9_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok } */
fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  slots.take(3);
  slots.take(4);
  let AstSlot (_, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_2_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_u32();
  let tok_5_2 = __tok_rng_5.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_5_2 = tok_5_2.to_u32();
  let var_7_0 = ReduceRaw::new(
    tok_1_0,
    tok_2_1,
    tok_5_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ReduceRaw(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok } */
fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let AstSlot (_, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_4_1 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_1 = tok_4_1.to_u32();
  let tok_5_2 = __tok_rng_5.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_5_2 = tok_5_2.to_u32();
  let var_7_0 = ReduceRaw::new(
    tok_1_0,
    tok_4_1,
    tok_5_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ReduceRaw(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok } */
fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_2_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_u32();
  let tok_3_2 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_2 = tok_3_2.to_u32();
  let var_5_0 = ReduceRaw::new(
    tok_1_0,
    tok_2_1,
    tok_3_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ReduceRaw(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? sym::nonterminal^nonterminal ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  nonterminal, tok } */
fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, _, _) = slots.take(4);
  slots.take(5);
  let AstSlot (ref_6, __tok_rng_6, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_6;
  let obj_6_0 = ref_6;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_u32();
  let obj_4_2 = ref_4;
  let var_8_0 = Reduce::new(
    Some(obj_6_0),
    tok_1_1,
    obj_4_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(var_8_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? sym::nonterminal^nonterminal ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  nonterminal, tok } */
fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_4_0 = ref_4;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_u32();
  let obj_2_2 = ref_2;
  let var_6_0 = Reduce::new(
    Some(obj_4_0),
    tok_1_1,
    obj_2_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? sym::nonterminal^nonterminal ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  nonterminal, tok } */
fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_u32();
  let obj_4_2 = ref_4;
  let var_6_0 = Reduce::new(
    None,
    tok_1_1,
    obj_4_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? sym::nonterminal^nonterminal ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  nonterminal, tok } */
fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_u32();
  let obj_2_2 = ref_2;
  let var_4_0 = Reduce::new(
    None,
    tok_1_1,
    obj_2_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "set-tok" tok::int

        :ast { t_SetTokenId, id: u32($2), tok } */
fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let var_3_0 = SetTokenId::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SetTokenId(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "set-tok-len" tok::int

        :ast { t_SetTokenLen, id: u32($2) } */
fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let var_3_0 = SetTokenLen::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::SetTokenLen(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "set-line"        :ast { t_SetLine, tok } */
fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = SetLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SetLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "symbols" "to" */
fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "with" "rule" */
fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "symbols" "to" */
fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast */
fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "pop" tok::int      :ast { t_Pop, count: u32($2), tok } */
fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let var_3_0 = Pop::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* match */
fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_sequence */
fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_statement */
fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "shift" "-skip"? ( "tok" | "char" )    :ast { t_Shift, ptr_type:str($3), skip:bool($2), tok } */
fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_2_0 = __tok_rng_2;
  let tok_2_0 = tok_2_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_4_1 = true;
  let var_5_0 = Shift::new(
    tok_2_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "shift" "-skip"? ( "tok" | "char" )    :ast { t_Shift, ptr_type:str($3), skip:bool($2), tok } */
fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_2_0 = __tok_rng_2;
  let tok_2_0 = tok_2_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_4_1 = true;
  let var_5_0 = Shift::new(
    tok_2_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "shift" "-skip"? ( "tok" | "char" )    :ast { t_Shift, ptr_type:str($3), skip:bool($2), tok } */
fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_1 = false;
  let var_4_0 = Shift::new(
    tok_1_0,
    obj_3_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "shift" "-skip"? ( "tok" | "char" )    :ast { t_Shift, ptr_type:str($3), skip:bool($2), tok } */
fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_1 = false;
  let var_4_0 = Shift::new(
    tok_1_0,
    obj_3_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "peek" "-skip"? ( "tok" | "char" )    :ast { t_Peek,  ptr_type:str($3), skip:bool($2), tok } */
fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_2_0 = __tok_rng_2;
  let tok_2_0 = tok_2_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_4_1 = true;
  let var_5_0 = Peek::new(
    tok_2_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "peek" "-skip"? ( "tok" | "char" )    :ast { t_Peek,  ptr_type:str($3), skip:bool($2), tok } */
fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_2_0 = __tok_rng_2;
  let tok_2_0 = tok_2_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_4_1 = true;
  let var_5_0 = Peek::new(
    tok_2_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "peek" "-skip"? ( "tok" | "char" )    :ast { t_Peek,  ptr_type:str($3), skip:bool($2), tok } */
fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_1 = false;
  let var_4_0 = Peek::new(
    tok_1_0,
    obj_3_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "peek" "-skip"? ( "tok" | "char" )    :ast { t_Peek,  ptr_type:str($3), skip:bool($2), tok } */
fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_1 = false;
  let var_4_0 = Peek::new(
    tok_1_0,
    obj_3_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "reset" ( "tok" | "char" )            :ast { t_Reset, ptr_type:str($2), tok } */
fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = Reset::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reset(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "reset" ( "tok" | "char" )            :ast { t_Reset, ptr_type:str($2), tok } */
fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = Reset::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reset(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "tok" */
fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "char" */
fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "tok" */
fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "char" */
fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "tok" */
fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "char" */
fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "EXPORT" sym::nonterminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, nonterminal:$2, reference:str($3) } */
fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let tok_3_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_1 = tok_3_1.to_string();
  let var_5_0 = Export::new(
    obj_1_0,
    tok_3_1,
  );
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "EXPORT" sym::nonterminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, nonterminal:$2, reference:str($3) } */
fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let tok_3_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_1 = tok_3_1.to_string();
  let var_5_0 = Export::new(
    obj_1_0,
    tok_3_1,
  );
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "EXPORT" sym::nonterminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, nonterminal:$2, reference:str($3) } */
fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let var_3_0 = Export::new(
    obj_1_0,
    Default::default(),
  );
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "AS" */
fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "as" */
fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( "AS" | "as" ) tok::id */
fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* ( "AS" | "as" ) tok::id */
fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "IMPORT" ( c:id | c:sym | c:num )(+) c:sp ( "AS" | "as" ) tok::id

        :ast { t_Import, uri: str($2), reference:str($5), tok } */
fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let tok_4_0 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_0 = tok_4_0.to_string();
  let obj_1_1 = ref_1.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let var_6_0 = Import::new(
    tok_4_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "IMPORT" ( c:id | c:sym | c:num )(+) c:sp ( "AS" | "as" ) tok::id

        :ast { t_Import, uri: str($2), reference:str($5), tok } */
fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let tok_4_0 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_0 = tok_4_0.to_string();
  let obj_1_1 = ref_1.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let var_6_0 = Import::new(
    tok_4_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* c:id */
fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "AS" */
fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "as" */
fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "NAME" tok::id

        :ast { t_Name, name: str($2) } */
fn reducer_199 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  let var_3_0 = Name::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Name(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "IGNORE" "{"  sym::terminal(+) "}"

        :ast { t_Ignore, symbols: $3 } */
fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2.into_nodes();
  let var_5_0 = Ignore::new(
    obj_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::Ignore(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* sym::terminal */
fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::terminal(+) */
fn reducer_202 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* rule */
fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "(" rules ")"{1}

        :ast { t_Grouped_Rules, rules:$2,  tok } */
fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = Grouped_Rules::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Grouped_Rules(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* nonterminal_symbol */
fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_nonterminal_symbol */
fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tok::id (":" "sym")?                    :ast { t_TemplateSym,     val:str($1) } */
fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_4_0 = TemplateSym::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::TemplateSym(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* tok::id (":" "sym")?                    :ast { t_TemplateSym,     val:str($1) } */
fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_2_0 = TemplateSym::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::TemplateSym(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ast::type_identifier ":" "ast"          :ast { t_TemplateASTType, val:str($1) } */
fn reducer_211 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_4_0 = TemplateASTType::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::TemplateASTType(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* ":" "sym" */
fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* '(' init_objects ")"       
        :ast { t_Init, expression: $2 } */
fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let var_4_0 = Init::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Init(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* tk:int_tok */
fn reducer_214 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:string_tok */
fn reducer_215 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<" token::int ( ","  token::int  )? ">"

        :ast { t_Range, start_trim:i32($2), end_trim:i32($3) } */
fn reducer_216 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let tok_3_0 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_0 = tok_3_0.to_i32();
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_i32();
  let var_6_0 = Range::new(
    tok_3_0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "<" token::int ( ","  token::int  )? ">"

        :ast { t_Range, start_trim:i32($2), end_trim:i32($3) } */
fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = 0 as i32;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_i32();
  let var_5_0 = Range::new(
    obj_4_0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* ","  token::int */
fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* reference */
fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* reference '.' identifier
        :ast { t_AST_Member, reference:$1, property:$3 } */
fn reducer_220 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_2_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_0_1 = ref_0;
  let var_4_0 = AST_Member::new(
    tok_2_0,
    obj_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Member(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* token::id */
fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* struct */
fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression
        :ast { t_AST_Statements, statements:[$1], tok } */
fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  let var_3_0 = AST_Statements::new(
    obj_2_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "{" expression(*";") '}'
        :ast { t_AST_Statements, statements:$2, tok } */
fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = AST_Statements::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" expression(*";") '}'
        :ast { t_AST_Statements, statements:$2, tok } */
fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let var_3_0 = AST_Statements::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* expression */
fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*";") */
fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* generic_match_block */
fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal_match_block */
fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_match_block */
fn reducer_230 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") "then" goto

            :ast { t_Gotos, pushes: $1, goto } */
fn reducer_231 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_1 = ref_2;
  let obj_2_1 = obj_2_1.to_Goto();
  let obj_0_2 = ref_0.into_nodes();
  let var_4_0 = Gotos::new(
    None,
    Some(obj_2_1),
    obj_0_2.into_iter().map(|v|match v { ASTNode::Push(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* goto
    
            :ast { t_Gotos, goto } */
fn reducer_232 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_1 = ref_0;
  let obj_0_1 = obj_0_1.to_Goto();
  let var_2_0 = Gotos::new(
    None,
    Some(obj_0_1),
    vec![],
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* fork

            :ast { t_Gotos, fork } */
fn reducer_233 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let obj_0_0 = obj_0_0.to_Fork();
  let var_2_0 = Gotos::new(
    Some(obj_0_0),
    None,
    vec![],
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* goto_push */
fn reducer_234 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") */
fn reducer_235 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "fail"          :ast { t_Fail, tok } */
fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Fail::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pass"        :ast { t_Pass, tok } */
fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Pass::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "accept"      :ast { t_Accept, tok } */
fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Accept::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk:(" rules ")"{1}

        :ast { t_TokenGroupRules, rules:$2,  tok } */
fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = TokenGroupRules::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TokenGroupRules(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token_non_terminal */
fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] )^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_0_1 = ref_0.into_nodes();
  let var_3_0 = Rule::new(
    Some(obj_1_0),
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* ( [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] )^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_1 = ref_0.into_nodes();
  let var_2_0 = Rule::new(
    None,
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol */
fn reducer_245 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* not_empty */
fn reducer_246 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_247 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_248 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(*) */
fn reducer_249 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(*) */
fn reducer_250 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] */
fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] */
fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] */
fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::id '::' tok::id

        :ast { t_NonTerminal_Import_Symbol, module:str($1), name:str($3), tok} */
fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_2_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_string();
  let var_4_0 = NonTerminal_Import_Symbol::new(
    tok_0_0,
    tok_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NonTerminal_Import_Symbol(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* member */
fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "$" token::id 
        :ast { t_AST_NamedReference, value: str($2), tok } */
fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  let var_3_0 = AST_NamedReference::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_NamedReference(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "$" token::int
        :ast { t_AST_IndexReference, value: i64($2), tok } */
fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_i64();
  let var_3_0 = AST_IndexReference::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_IndexReference(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "match" ":" tok::id^id ( ":" tok::id :ast str($2) )?^scanner ( int_match :ast [$1] | "{" ( int_match | default_match | hint )(+) "}" :ast $2  )^m

        :ast { t_Matches, mode: str($id), matches:$m, scanner, tok } */
fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_4_0 = ref_4.into_nodes();
  let tok_2_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_string();
  let obj_3_2 = ref_3.to_string();
  let var_6_0 = Matches::new(
    obj_4_0,
    tok_2_1,
    obj_3_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Matches(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "match" ":" tok::id^id ( ":" tok::id :ast str($2) )?^scanner ( int_match :ast [$1] | "{" ( int_match | default_match | hint )(+) "}" :ast $2  )^m

        :ast { t_Matches, mode: str($id), matches:$m, scanner, tok } */
fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_3_0 = ref_3.into_nodes();
  let tok_2_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_string();
  let var_5_0 = Matches::new(
    obj_3_0,
    tok_2_1,
    Default::default(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Matches(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* ":" tok::id :ast str($2) */
fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* int_match */
fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* default_match */
fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* hint */
fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* int_match :ast [$1] */
fn reducer_271 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( int_match | default_match | hint )(+) "}" :ast $2 */
fn reducer_272 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "PRODUCTION" ( nonterminal_match :ast [$1] | "{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_ProductionMatches, matches:$m } */
fn reducer_273 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_3_0 = ref_3.into_nodes();
  let var_5_0 = ProductionMatches::new(
    obj_3_0,
  );
  slots.assign(0, AstSlot(ASTNode::ProductionMatches(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* nonterminal_match */
fn reducer_274 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* hint */
fn reducer_275 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* default_match */
fn reducer_276 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_277 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_278 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_279 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_280 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_281 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_282 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* nonterminal_match :ast [$1] */
fn reducer_283 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_284 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "TERMINAL" ( terminal_match :ast [$1] | "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_TerminalMatches, matches:$m } */
fn reducer_285 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_3_0 = ref_3.into_nodes();
  let var_5_0 = TerminalMatches::new(
    obj_3_0,
  );
  slots.assign(0, AstSlot(ASTNode::TerminalMatches(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* terminal_match */
fn reducer_286 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* hint */
fn reducer_287 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* default_match */
fn reducer_288 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_289 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_290 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_291 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_292 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_293 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_294 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* terminal_match :ast [$1] */
fn reducer_295 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_296 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "push" sym::nonterminal

    :ast { t_Push, nonterminal: $2, name:str($2), tok } */
fn reducer_297 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_1_1 = ref_1;
  let var_3_0 = Push::new(
    tok_1_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Push(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "goto" sym::nonterminal

    :ast { t_Goto, nonterminal: $2, name:str($2), tok } */
fn reducer_298 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_1_1 = ref_1;
  let var_3_0 = Goto::new(
    tok_1_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Goto(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "fork" "{" ( sym::nonterminal :ast { t_Goto, nonterminal: $1, name:str($1), tok } )(+) "}"                 
                                            
            :ast { t_Fork, paths: $3, tok } */
fn reducer_299 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2.into_nodes();
  let var_5_0 = Fork::new(
    obj_2_0.into_iter().map(|v|match v { ASTNode::Goto(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Fork(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* sym::nonterminal :ast { t_Goto, nonterminal: $1, name:str($1), tok } */
fn reducer_300 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0;
  let tok_0_0 = tok_0_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_1 = ref_0;
  let var_2_0 = Goto::new(
    tok_0_0,
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Goto(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( sym::nonterminal :ast { t_Goto, nonterminal: $1, name:str($1), tok } ) */
fn reducer_301 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::nonterminal :ast { t_Goto, nonterminal: $1, name:str($1), tok } )(+) */
fn reducer_302 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::string

        :ast { t_TerminalToken, val:str(tok<1,1>), tok, is_exclusive:true } */
fn reducer_303 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = true;
  let tok_rule_1 = __rule_rng__;
  let tok_rule_1 = tok_rule_1.trim(1, 1);
  let tok_rule_1 = tok_rule_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = TerminalToken::new(
    obj_2_0,
    tok_rule_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TerminalToken(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* tok::quote

        :ast { t_TerminalToken, val:str(tok<1,1>), tok } */
fn reducer_304 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_1 = __rule_rng__;
  let tok_rule_1 = tok_rule_1.trim(1, 1);
  let tok_rule_1 = tok_rule_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_2_0 = TerminalToken::new(
    false,
    tok_rule_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TerminalToken(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk:" nonterminal

        :ast { t_NonTerminal_Terminal_Symbol, nonterminal:$2, tok } */
fn reducer_305 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let var_3_0 = NonTerminal_Terminal_Symbol::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NonTerminal_Terminal_Symbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_306 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_307 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_308 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_309 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_310 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_311 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_312 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_313 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* 'num' */
fn reducer_314 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'nl' */
fn reducer_315 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'sp' */
fn reducer_316 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'id' */
fn reducer_317 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'sym' */
fn reducer_318 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'any' */
fn reducer_319 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'tab' */
fn reducer_320 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'vtab' */
fn reducer_321 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_322 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_3_1 = ref_3;
  let obj_3_1 = obj_3_1.to_Precedence();
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_3_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_323 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_2_1 = ref_2;
  let obj_2_1 = obj_2_1.to_Precedence();
  let obj_0_3 = ref_0;
  let var_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_2_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_324 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = false;
  let obj_2_1 = ref_2;
  let obj_2_1 = obj_2_1.to_Precedence();
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_2_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_325 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = false;
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Precedence();
  let obj_0_3 = ref_0;
  let var_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    Some(obj_1_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_326 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    None,
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_327 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = true;
  let obj_0_3 = ref_0;
  let var_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    None,
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_328 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = false;
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    None,
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_329 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_2_1 = ref_2;
  let obj_2_1 = obj_2_1.to_Precedence();
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_2_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_330 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Precedence();
  let obj_0_3 = ref_0;
  let var_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_1_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_331 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_3_1 = ref_3;
  let obj_3_1 = obj_3_1.to_Precedence();
  let tok_2_2 = __tok_rng_2;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_3_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_332 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let tok_2_2 = __tok_rng_2;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    None,
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_333 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_2_1 = ref_2;
  let obj_2_1 = obj_2_1.to_Precedence();
  let tok_3_2 = __tok_rng_3;
  let tok_3_2 = tok_3_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_2_1),
    tok_3_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_334 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = false;
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Precedence();
  let tok_2_2 = __tok_rng_2;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_1_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_335 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Precedence();
  let tok_2_2 = __tok_rng_2;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_1_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_336 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_1_1 = ref_1;
  let obj_1_1 = obj_1_1.to_Precedence();
  let tok_3_2 = __tok_rng_3;
  let tok_3_2 = tok_3_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = ref_0;
  let var_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_1_1),
    tok_3_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* list */
fn reducer_337 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "[" sym::annotated_symbol(+)^s ']' "!"?^o

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_338 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1.into_nodes();
  let obj_5_1 = true;
  let var_6_0 = NotEmptySet::new(
    obj_1_0,
    obj_5_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "[" sym::annotated_symbol(+)^s ']' "!"?^o

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_339 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let obj_4_1 = false;
  let var_5_0 = NotEmptySet::new(
    obj_1_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol */
fn reducer_340 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol(+) */
fn reducer_341 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "$" :ast { t_EOFSymbol, tok } */
fn reducer_342 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast

            :ast  { t_Ascript, ast:$ast, tok } */
fn reducer_343 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let var_3_0 = Ascript::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Ascript(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "(" ( tok::int :ast u64($1) )(+"|")^vals ")" "{" statement "}"

    :ast { t_IntMatch, vals, statement } */
fn reducer_344 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let obj_4_0 = ref_4;
  let obj_4_0 = obj_4_0.to_Statement();
  let obj_1_1 = ref_1.into_u64_vec();
  let var_7_0 = IntMatch::new(
    obj_4_0,
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::IntMatch(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* tok::int :ast u64($1) */
fn reducer_345 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_u64();
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) ) */
fn reducer_346 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) )(+"|") */
fn reducer_347 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2.to_u64();
  let mut obj_0_0 = ref_0.into_u64_vec();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "default"? "{" statement "}"

    :ast { t_DefaultMatch, statement } */
fn reducer_348 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2;
  let obj_2_0 = obj_2_0.to_Statement();
  let var_5_0 = DefaultMatch::new(
    obj_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::DefaultMatch(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "default"? "{" statement "}"

    :ast { t_DefaultMatch, statement } */
fn reducer_349 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Statement();
  let var_4_0 = DefaultMatch::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::DefaultMatch(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "fail-hint" "{" tok::string^message "}"

    :ast { t_FailHint, message: str($message) } */
fn reducer_350 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let tok_2_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_0 = tok_2_0.to_string();
  let var_5_0 = FailHint::new(
    tok_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::FailHint(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "(" sym::nonterminal^sym ")" "{" statement "}"

    :ast { t_NonTermMatch, sym, statement } */
fn reducer_351 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let obj_4_0 = ref_4;
  let obj_4_0 = obj_4_0.to_Statement();
  let obj_1_1 = ref_1;
  let var_7_0 = NonTermMatch::new(
    obj_4_0,
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::NonTermMatch(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* "(" sym::terminal^sym ")" "{" statement "}"

    :ast { t_TermMatch, sym, statement } */
fn reducer_352 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_5, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_5;
  let obj_4_0 = ref_4;
  let obj_4_0 = obj_4_0.to_Statement();
  let obj_1_1 = ref_1;
  let var_7_0 = TermMatch::new(
    obj_4_0,
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::TermMatch(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* tk:quote_tok */
fn reducer_353 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* symbol '+'

            :ast { t_List_Rules, symbol:$1, tok } */
fn reducer_354 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_1 = ref_0;
  let var_3_0 = List_Rules::new(
    false,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "*"

            :ast { t_List_Rules, symbol:$1, tok, optional: true } */
fn reducer_355 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_0 = true;
  let obj_0_1 = ref_0;
  let var_4_0 = List_Rules::new(
    obj_3_0,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Rules, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_356 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_5_0 = List_Rules::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Rules, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_357 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_5_0 = List_Rules::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Rules, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_358 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_1 = ref_0;
  let var_4_0 = List_Rules::new(
    false,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(*" ( token | class )? ')'

            :ast { t_List_Rules, terminal_symbol:$3, symbol:$1, tok, optional:true } */
fn reducer_359 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_6_0 = List_Rules::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(*" ( token | class )? ')'

            :ast { t_List_Rules, terminal_symbol:$3, symbol:$1, tok, optional:true } */
fn reducer_360 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_6_0 = List_Rules::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(*" ( token | class )? ')'

            :ast { t_List_Rules, terminal_symbol:$3, symbol:$1, tok, optional:true } */
fn reducer_361 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_0_1 = ref_0;
  let var_5_0 = List_Rules::new(
    obj_4_0,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Rules(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* symbol */
fn reducer_362 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_363 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_364 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_365 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_366 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_367 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_1 = ref_2.to_u32();
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_5_0 = Precedence::new(
    false,
    obj_2_1,
    tok_1_2,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_368 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_1 = ref_1.to_u32();
  let obj_4_2 = 0 as u32;
  let var_5_0 = Precedence::new(
    false,
    obj_1_1,
    obj_4_2,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_369 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_4_0 = Precedence::new(
    false,
    0,
    tok_1_2,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_370 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_2 = 0 as u32;
  let var_4_0 = Precedence::new(
    false,
    0,
    obj_3_2,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( tk:precedence_num^prec ":" )? "kw" '}' :ast { t_Precedence, sym_prec: u32($prec), is_keyword: true } */
fn reducer_371 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_6_0 = true;
  let tok_1_2 = __tok_rng_1;
  let tok_1_2 = tok_1_2.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_7_0 = Precedence::new(
    obj_6_0,
    0,
    tok_1_2,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* "{" ( tk:precedence_num^prec ":" )? "kw" '}' :ast { t_Precedence, sym_prec: u32($prec), is_keyword: true } */
fn reducer_372 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_5_2 = 0 as u32;
  let var_6_0 = Precedence::new(
    obj_4_0,
    0,
    obj_5_2,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* ":" tk:precedence_num? :ast u32($2) */
fn reducer_373 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* ":" tk:precedence_num? :ast u32($2) */
fn reducer_374 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = 0 as u32;
  slots.assign(0, AstSlot(ASTNode::U32(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* tk:precedence_num^prec ":" */
fn reducer_375 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* nonterminal */
fn reducer_376 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* template_nonterminal */
fn reducer_377 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal */
fn reducer_378 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal "::<" template_arg(+",")^template_args ">"

            :ast { t_Template_NonTerminal_Symbol, name:$1, template_args, tok } */
fn reducer_379 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2.into_nodes();
  let var_5_0 = Template_NonTerminal_Symbol::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Template_NonTerminal_Symbol(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* template_arg */
fn reducer_380 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* template_arg(+",") */
fn reducer_381 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* list */
fn reducer_382 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ast::type_identifier 
                
            :ast { t_AST_STRUCT_TEMPLATE_NAME, typ: str(tok)  } */
fn reducer_383 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let var_2_0 = AST_STRUCT_TEMPLATE_NAME::new(
    tok_rule_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRUCT_TEMPLATE_NAME(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 384]
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
      reducer_156::<R, M, UP>,
      reducer_157::<R, M, UP>,
      reducer_158::<R, M, UP>,
      reducer_159::<R, M, UP>,
      reducer_160::<R, M, UP>,
      reducer_161::<R, M, UP>,
      reducer_162::<R, M, UP>,
      reducer_163::<R, M, UP>,
      reducer_164::<R, M, UP>,
      reducer_165::<R, M, UP>,
      reducer_166::<R, M, UP>,
      reducer_167::<R, M, UP>,
      reducer_168::<R, M, UP>,
      reducer_169::<R, M, UP>,
      reducer_170::<R, M, UP>,
      reducer_171::<R, M, UP>,
      reducer_172::<R, M, UP>,
      reducer_173::<R, M, UP>,
      reducer_174::<R, M, UP>,
      reducer_175::<R, M, UP>,
      reducer_176::<R, M, UP>,
      reducer_177::<R, M, UP>,
      reducer_178::<R, M, UP>,
      reducer_179::<R, M, UP>,
      reducer_180::<R, M, UP>,
      reducer_181::<R, M, UP>,
      reducer_182::<R, M, UP>,
      reducer_183::<R, M, UP>,
      reducer_184::<R, M, UP>,
      reducer_185::<R, M, UP>,
      reducer_186::<R, M, UP>,
      reducer_187::<R, M, UP>,
      reducer_188::<R, M, UP>,
      reducer_189::<R, M, UP>,
      reducer_190::<R, M, UP>,
      reducer_191::<R, M, UP>,
      reducer_192::<R, M, UP>,
      reducer_193::<R, M, UP>,
      reducer_194::<R, M, UP>,
      reducer_195::<R, M, UP>,
      reducer_196::<R, M, UP>,
      reducer_197::<R, M, UP>,
      reducer_198::<R, M, UP>,
      reducer_199::<R, M, UP>,
      reducer_200::<R, M, UP>,
      reducer_201::<R, M, UP>,
      reducer_202::<R, M, UP>,
      reducer_203::<R, M, UP>,
      reducer_204::<R, M, UP>,
      reducer_205::<R, M, UP>,
      reducer_206::<R, M, UP>,
      reducer_207::<R, M, UP>,
      reducer_208::<R, M, UP>,
      reducer_209::<R, M, UP>,
      reducer_210::<R, M, UP>,
      reducer_211::<R, M, UP>,
      reducer_212::<R, M, UP>,
      reducer_213::<R, M, UP>,
      reducer_214::<R, M, UP>,
      reducer_215::<R, M, UP>,
      reducer_216::<R, M, UP>,
      reducer_217::<R, M, UP>,
      reducer_218::<R, M, UP>,
      reducer_219::<R, M, UP>,
      reducer_220::<R, M, UP>,
      reducer_221::<R, M, UP>,
      reducer_222::<R, M, UP>,
      reducer_223::<R, M, UP>,
      reducer_224::<R, M, UP>,
      reducer_225::<R, M, UP>,
      reducer_226::<R, M, UP>,
      reducer_227::<R, M, UP>,
      reducer_228::<R, M, UP>,
      reducer_229::<R, M, UP>,
      reducer_230::<R, M, UP>,
      reducer_231::<R, M, UP>,
      reducer_232::<R, M, UP>,
      reducer_233::<R, M, UP>,
      reducer_234::<R, M, UP>,
      reducer_235::<R, M, UP>,
      reducer_236::<R, M, UP>,
      reducer_237::<R, M, UP>,
      reducer_238::<R, M, UP>,
      reducer_239::<R, M, UP>,
      reducer_240::<R, M, UP>,
      reducer_241::<R, M, UP>,
      reducer_242::<R, M, UP>,
      reducer_243::<R, M, UP>,
      reducer_244::<R, M, UP>,
      reducer_245::<R, M, UP>,
      reducer_246::<R, M, UP>,
      reducer_247::<R, M, UP>,
      reducer_248::<R, M, UP>,
      reducer_249::<R, M, UP>,
      reducer_250::<R, M, UP>,
      reducer_251::<R, M, UP>,
      reducer_252::<R, M, UP>,
      reducer_253::<R, M, UP>,
      reducer_254::<R, M, UP>,
      reducer_255::<R, M, UP>,
      reducer_256::<R, M, UP>,
      reducer_257::<R, M, UP>,
      reducer_258::<R, M, UP>,
      reducer_259::<R, M, UP>,
      reducer_260::<R, M, UP>,
      reducer_261::<R, M, UP>,
      reducer_262::<R, M, UP>,
      reducer_263::<R, M, UP>,
      reducer_264::<R, M, UP>,
      reducer_265::<R, M, UP>,
      reducer_266::<R, M, UP>,
      reducer_267::<R, M, UP>,
      reducer_268::<R, M, UP>,
      reducer_269::<R, M, UP>,
      reducer_270::<R, M, UP>,
      reducer_271::<R, M, UP>,
      reducer_272::<R, M, UP>,
      reducer_273::<R, M, UP>,
      reducer_274::<R, M, UP>,
      reducer_275::<R, M, UP>,
      reducer_276::<R, M, UP>,
      reducer_277::<R, M, UP>,
      reducer_278::<R, M, UP>,
      reducer_279::<R, M, UP>,
      reducer_280::<R, M, UP>,
      reducer_281::<R, M, UP>,
      reducer_282::<R, M, UP>,
      reducer_283::<R, M, UP>,
      reducer_284::<R, M, UP>,
      reducer_285::<R, M, UP>,
      reducer_286::<R, M, UP>,
      reducer_287::<R, M, UP>,
      reducer_288::<R, M, UP>,
      reducer_289::<R, M, UP>,
      reducer_290::<R, M, UP>,
      reducer_291::<R, M, UP>,
      reducer_292::<R, M, UP>,
      reducer_293::<R, M, UP>,
      reducer_294::<R, M, UP>,
      reducer_295::<R, M, UP>,
      reducer_296::<R, M, UP>,
      reducer_297::<R, M, UP>,
      reducer_298::<R, M, UP>,
      reducer_299::<R, M, UP>,
      reducer_300::<R, M, UP>,
      reducer_301::<R, M, UP>,
      reducer_302::<R, M, UP>,
      reducer_303::<R, M, UP>,
      reducer_304::<R, M, UP>,
      reducer_305::<R, M, UP>,
      reducer_306::<R, M, UP>,
      reducer_307::<R, M, UP>,
      reducer_308::<R, M, UP>,
      reducer_309::<R, M, UP>,
      reducer_310::<R, M, UP>,
      reducer_311::<R, M, UP>,
      reducer_312::<R, M, UP>,
      reducer_313::<R, M, UP>,
      reducer_314::<R, M, UP>,
      reducer_315::<R, M, UP>,
      reducer_316::<R, M, UP>,
      reducer_317::<R, M, UP>,
      reducer_318::<R, M, UP>,
      reducer_319::<R, M, UP>,
      reducer_320::<R, M, UP>,
      reducer_321::<R, M, UP>,
      reducer_322::<R, M, UP>,
      reducer_323::<R, M, UP>,
      reducer_324::<R, M, UP>,
      reducer_325::<R, M, UP>,
      reducer_326::<R, M, UP>,
      reducer_327::<R, M, UP>,
      reducer_328::<R, M, UP>,
      reducer_329::<R, M, UP>,
      reducer_330::<R, M, UP>,
      reducer_331::<R, M, UP>,
      reducer_332::<R, M, UP>,
      reducer_333::<R, M, UP>,
      reducer_334::<R, M, UP>,
      reducer_335::<R, M, UP>,
      reducer_336::<R, M, UP>,
      reducer_337::<R, M, UP>,
      reducer_338::<R, M, UP>,
      reducer_339::<R, M, UP>,
      reducer_340::<R, M, UP>,
      reducer_341::<R, M, UP>,
      reducer_342::<R, M, UP>,
      reducer_343::<R, M, UP>,
      reducer_344::<R, M, UP>,
      reducer_345::<R, M, UP>,
      reducer_346::<R, M, UP>,
      reducer_347::<R, M, UP>,
      reducer_348::<R, M, UP>,
      reducer_349::<R, M, UP>,
      reducer_350::<R, M, UP>,
      reducer_351::<R, M, UP>,
      reducer_352::<R, M, UP>,
      reducer_353::<R, M, UP>,
      reducer_354::<R, M, UP>,
      reducer_355::<R, M, UP>,
      reducer_356::<R, M, UP>,
      reducer_357::<R, M, UP>,
      reducer_358::<R, M, UP>,
      reducer_359::<R, M, UP>,
      reducer_360::<R, M, UP>,
      reducer_361::<R, M, UP>,
      reducer_362::<R, M, UP>,
      reducer_363::<R, M, UP>,
      reducer_364::<R, M, UP>,
      reducer_365::<R, M, UP>,
      reducer_366::<R, M, UP>,
      reducer_367::<R, M, UP>,
      reducer_368::<R, M, UP>,
      reducer_369::<R, M, UP>,
      reducer_370::<R, M, UP>,
      reducer_371::<R, M, UP>,
      reducer_372::<R, M, UP>,
      reducer_373::<R, M, UP>,
      reducer_374::<R, M, UP>,
      reducer_375::<R, M, UP>,
      reducer_376::<R, M, UP>,
      reducer_377::<R, M, UP>,
      reducer_378::<R, M, UP>,
      reducer_379::<R, M, UP>,
      reducer_380::<R, M, UP>,
      reducer_381::<R, M, UP>,
      reducer_382::<R, M, UP>,
      reducer_383::<R, M, UP>,
    ])
  }
}
    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<T, UserCTX, Bytecode> = sherpa_rust_runtime::deprecate::ByteCodeParser<T, UserCTX, Bytecode>;

pub mod meta{
  
  pub const nonterm_names: [&'static str;139] = [
    "state",
    "escaped_string",
    "escaped_string_group",
    "escaped_string_list_1",
    "grammar",
    "grammar_list",
    "grammar_group_1",
    "grammar_list_2",
    "def_type",
    "expression",
    "struct",
    "struct_list",
    "struct_group_1",
    "nonterminal_symbol",
    "statement",
    "statement_list",
    "statement_group_1",
    "statement_group_2",
    "statement_group_3",
    "statement_list_4",
    "statement_group_5",
    "statement_group_6",
    "statement_group_7",
    "escaped_vals",
    "escaped",
    "escaped_group",
    "preamble",
    "cf_rules",
    "peg_rules",
    "append_rules",
    "template_rule",
    "template_rule_list",
    "id",
    "num",
    "string_convert",
    "numeric_convert",
    "bool_convert",
    "literal",
    "vector",
    "vector_list",
    "token",
    "token_group",
    "add",
    "map",
    "struct_prop",
    "type_identifier",
    "id",
    "non_branch_statement",
    "non_branch_statement_group",
    "non_branch_statement_group_1",
    "non_branch_statement_group_2",
    "non_branch_statement_group_3",
    "pop",
    "branch_statement",
    "transitive_statement",
    "transitive_statement_group",
    "transitive_statement_group_1",
    "transitive_statement_group_2",
    "export_clause",
    "export_clause_group",
    "export_clause_group_1",
    "import_clause",
    "import_clause_group",
    "import_clause_list_1",
    "import_clause_group_2",
    "name_clause",
    "ignore_clause",
    "ignore_clause_list",
    "rules",
    "rules_list",
    "nonterminal",
    "template_param",
    "template_param_group",
    "convert_initializer",
    "int",
    "string",
    "range",
    "range_group",
    "member",
    "identifier",
    "body",
    "body_list",
    "match",
    "goto_sequence",
    "goto_sequence_list",
    "terminal_statement",
    "terminal",
    "rule",
    "rule_group",
    "rule_list_1",
    "rule_group_2",
    "import_nonterminal_symbol",
    "init_objects",
    "reference",
    "generic_match_block",
    "generic_match_block_group",
    "generic_match_block_group_1",
    "generic_match_block_list_2",
    "generic_match_block_group_3",
    "nonterminal_match_block",
    "nonterminal_match_block_group",
    "nonterminal_match_block_list_1",
    "nonterminal_match_block_group_2",
    "terminal_match_block",
    "terminal_match_block_group",
    "terminal_match_block_list_1",
    "terminal_match_block_group_2",
    "goto_push",
    "goto",
    "fork",
    "fork_group",
    "fork_list_1",
    "token",
    "token_non_terminal",
    "class",
    "class_group",
    "annotated_symbol",
    "not_empty",
    "not_empty_list",
    "end_of_input",
    "ast_definition",
    "int_match",
    "int_match_group",
    "int_match_list_1",
    "default_match",
    "hint",
    "nonterminal_match",
    "terminal_match",
    "quote",
    "list",
    "list_group",
    "list_group_1",
    "precedence",
    "precedence_group",
    "precedence_group_1",
    "symbol",
    "template_nonterminal",
    "template_nonterminal_list",
    "template_arg",
  ];
  
  pub const symbol_string: [&'static str;128] = [
    r####"Default"####,
    r####"c:sp"####,
    r####"c:nl"####,
    r####" => "####,
    r####" =!> "####,
    r####"nonterm"####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####"{EOF}"####,
    r####" , "####,
    r####" { "####,
    r####" } "####,
    r####"nonterm"####,
    r####" then "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####"c:id"####,
    r####"c:num"####,
    r####"c:sym"####,
    r####" \ "####,
    r####" > "####,
    r####" <> "####,
    r####"nonterm"####,
    r####" :> "####,
    r####" + "####,
    r####"nonterm"####,
    r####" < "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" str "####,
    r####"nonterm"####,
    r####" i8 "####,
    r####" u8 "####,
    r####" f32 "####,
    r####" i32 "####,
    r####" u32 "####,
    r####" f64 "####,
    r####" i64 "####,
    r####" u64 "####,
    r####" i16 "####,
    r####" u16 "####,
    r####" bool "####,
    r####" true "####,
    r####" false "####,
    r####" [ "####,
    r####" ] "####,
    r####" tk "####,
    r####" tok "####,
    r####" token "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####" ( "####,
    r####" ) "####,
    r####" map "####,
    r####" : "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" to "####,
    r####" rule "####,
    r####" with "####,
    r####" :ast "####,
    r####" reduce "####,
    r####" set-tok "####,
    r####" symbols "####,
    r####" set-line "####,
    r####" set-tok-len "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####" pop "####,
    r####" peek "####,
    r####" char "####,
    r####" -skip "####,
    r####" reset "####,
    r####" shift "####,
    r####" AS "####,
    r####" as "####,
    r####" EXPORT "####,
    r####" IMPORT "####,
    r####" NAME "####,
    r####" IGNORE "####,
    r####" | "####,
    r####" sym "####,
    r####" ast "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" . "####,
    r####" ; "####,
    r####" fail "####,
    r####" pass "####,
    r####" accept "####,
    r####" tk:( "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####" :: "####,
    r####" $ "####,
    r####" match "####,
    r####"nonterm"####,
    r####" PRODUCTION "####,
    r####" TERMINAL "####,
    r####" push "####,
    r####" goto "####,
    r####" fork "####,
    r####" tk: "####,
    r####" c: "####,
    r####" id "####,
    r####" nl "####,
    r####" sp "####,
    r####" tab "####,
    r####" num "####,
    r####" any "####,
    r####" vtab "####,
    r####" ? "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####"tk:nonterm"####,
    r####" ! "####,
    r####" default "####,
    r####" fail-hint "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####"tk:nonterm"####,
    r####" * "####,
    r####" (* "####,
    r####" (+ "####,
    r####"tk:nonterm"####,
    r####" kw "####,
    r####" ::< "####,
  ];
}

pub fn new_ir_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(8);
  parser
}

pub fn new_escaped_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(50544);
  parser
}

pub fn new_grammar_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(51363);
  parser
}

pub fn new_type_eval_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(104465);
  parser
}

pub fn new_ast_expression_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(104615);
  parser
}

pub fn new_ast_struct_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(105126);
  parser
}

pub static bytecode: [u8; 139055] = [
  0,211,200,197,210,208,193,2,15,1,111,197,0,0,17,1,21,0,0,0,1,21,1,69,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8, 
  4,19,46,0,0,0,143,0,0,0,1,0,19,13,0,0,0,41,0,0,0,1,0,17,1,91,0,0,0,1,2,21,1,59,0,0,0,49,157,1,0,6,0,0,0,2,0,0,0,4,88,1,128,1,80,1,128,2,80,129, 
  128,3,152,129,128,6,80,1,128,7,80,1,128,8,4,17,1,49,195,0,0,1,4,17,1,151,0,0,0,1,2,21,1,49,2,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66, 
  130,65,200,14,128,100,248,4,128,69,88,13,131,6,240,130,129,7,240,2,128,88,8,10,128,73,24,12,129,74,120,139,129,66,248,13,128,70,184,140,129,89,152,8,128,62,184,16,128,63,232,15,128,90,40,7,128,101,248,3, 
  128,102,248,2,128,8,4,15,1,1,195,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,1,195,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212, 
  192,0,0,17,1,117,192,0,0,1,4,15,1,1,195,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,1,195,0,0,15,1,13,195,0, 
  0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,19,0,0,0,0,0, 
  0,0,0,3,0,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,19,85,0,0,0,236,0, 
  0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,15,1,1,195,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15, 
  1,1,195,0,0,15,1,124,163,0,0,17,1,158,162,0,0,1,4,15,1,1,195,0,0,15,1,124,163,0,0,17,1,6,162,0,0,1,4,15,1,1,195,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15, 
  1,1,195,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,254,145,0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,19,0,0,0,62,0,0,0,1,0,15,1,1,195,0,0,17,1,62,146,0,0,1, 
  4,15,1,1,195,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,190,145,0,0,1,4,15,1,1,195,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,201,2,0,0,1,2,21,1,58,0,0,0,134, 
  164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,4,3,0,0,1,2,21,1,117,0,0,0,255,164, 
  1,0,8,0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144,1,128,57,112,2,128,52,56,67,128,84,152,1,128,6,144,1,128,7,144,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,2,145,0, 
  0,1,4,17,1,35,140,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,15,1,122,102,0,0,17,1,242,101,0,0,1,4,15,1,122,102,0,0,17,1,122,3,0,0,1,2,21,1,217,0,0,0,213,167,1,0, 
  13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128, 
  8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,80,95,0,0,1,4,15,1,244,95,0,0,17,1,11,95,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0, 
  17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,244,95,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,244,95,0,0,1,4, 
  15,1,244,95,0,0,17,1,125,92,0,0,1,4,15,1,244,95,0,0,17,1,84,4,0,0,1,2,21,1,176,0,0,0,37,171,1,0,11,0,0,0,3,0,0,0,104,144,2,128,1,240,193,129,2,240,1,128,91,112, 
  3,128,52,16,5,128,85,224,3,128,6,240,1,128,7,240,129,128,57,120,132,128,103,0,3,128,121,248,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,5,5,0,0,1,4,15,1,5,5,0,0,17,1,80, 
  95,0,0,1,4,15,1,5,5,0,0,17,1,11,95,0,0,1,4,15,1,5,5,0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,5,5,0,0,1,4,19,46,0,0,0,143,0,0, 
  0,1,0,17,1,5,5,0,0,1,4,15,1,5,5,0,0,17,1,122,3,0,0,1,2,21,0,72,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,112,160,6,131,113,16,134,131,114,128,5,128,86,192,199,129,116, 
  240,4,128,117,232,4,128,70,224,72,127,135,32,3,128,136,144,2,128,91,48,7,128,118,128,4,128,75,80,136,127,128,240,3,128,13,176,9,128,46,72,9,128,129,136,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1, 
  5,5,0,0,1,15,1,5,5,0,0,17,1,28,92,0,0,1,15,1,5,5,0,0,17,1,134,91,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,5,5,0,0,1,15,1,5,5,0,0,17,1,242,86,0, 
  0,1,1,19,118,0,0,0,84,1,0,0,1,0,17,1,5,5,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,5,5,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,5,5,0,0,1,19,86,0, 
  0,0,240,0,0,0,1,0,17,1,5,5,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,5,5,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,5,5,0,0,1,19,112,0,0,0,47,1,0,0, 
  1,0,17,1,5,5,0,0,1,15,1,5,5,0,0,17,1,220,6,0,0,1,15,1,5,5,0,0,17,1,78,6,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,5,5,0,0,1,2,21,1,39,0,0,0, 
  197,171,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21,1,50,0,0,0,112,175,1,0,3,0,0,0,1, 
  0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,180,6,0,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,0,128,91,208, 
  0,128,1,19,91,0,0,0,254,0,0,0,3,0,1,2,21,1,39,0,0,0,201,175,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,127,248,0,128,8,4,17,1,15,7,0,0,1,19,135,0,0,0, 
  120,1,0,0,1,0,1,21,1,186,0,0,0,234,176,1,0,10,0,0,0,3,0,0,0,56,240,4,130,1,208,65,129,2,208,1,128,91,80,3,128,52,96,5,128,85,192,3,128,57,88,196,128,103,224,2,128,104,112,2, 
  128,121,216,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,177,81,0,0,1,4,15,1,177,81,0,0,17,1,80,95,0,0,1,4,15,1,177,81,0,0,17,1,212,80,0,0,1,4,15,1,177,81,0,0, 
  17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,177,81,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,177,81,0,0,1,4,15,1,177,81,0,0,17,1,47,10,0,0,1,4, 
  15,1,177,81,0,0,17,1,202,7,0,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128, 
  7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,80,95,0,0,1,4,15, 
  1,244,95,0,0,17,1,212,80,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,244, 
  95,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,85,9,0,0,1,4,15,1,244,95,0,0,17,1,164,8,0,0,1,2,21,1,176,0,0,0,37,171, 
  1,0,11,0,0,0,3,0,0,0,104,144,2,128,1,240,193,129,2,240,1,128,91,112,3,128,52,16,5,128,85,224,3,128,6,240,1,128,7,240,129,128,57,120,132,128,103,0,3,128,121,248,1,128,8,4,19,128,0,0, 
  0,97,1,0,0,1,0,17,1,5,5,0,0,1,4,15,1,5,5,0,0,17,1,80,95,0,0,1,4,15,1,5,5,0,0,17,1,212,80,0,0,1,4,15,1,5,5,0,0,17,1,160,10,0,0,1,4,19,75,0, 
  0,0,215,0,0,0,1,0,17,1,5,5,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,5,5,0,0,1,4,15,1,5,5,0,0,17,1,202,7,0,0,1,2,21,1,217,0,0,0,213,167,1,0,13, 
  0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8, 
  4,19,128,0,0,0,97,1,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,80,95,0,0,1,4,15,1,244,95,0,0,17,1,212,80,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17, 
  1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,244,95,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,244,95,0,0,1,4,15, 
  1,244,95,0,0,17,1,85,9,0,0,1,4,15,1,244,95,0,0,17,1,164,8,0,0,1,2,21,1,50,0,0,0,112,175,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,19, 
  46,0,0,0,143,0,0,0,1,0,17,1,98,10,0,0,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,46,80,1,128,45,224,65,128,79,240,0,128,19,45,0,0,0,142,0,0,0,2,0,1, 
  19,79,0,0,0,221,0,0,0,1,0,17,1,98,10,0,0,1,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88, 
  6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,80, 
  95,0,0,1,4,15,1,84,12,0,0,17,1,212,80,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,122,11,0,0,1,4,19,75,0,0,0,215,0,0, 
  0,1,0,17,1,84,12,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,202,7,0,0,1,4,15,1,84,12,0,0,17,1,164,8,0,0,1,2,21,1, 
  217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128, 
  103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,80,95,0,0,1,4,15,1,84,12,0,0,17,1,212,80,0,0,1,4,19,119,0, 
  0,0,86,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,122,11,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,84,12,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0, 
  17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,202,7,0,0,1,4,15,1,84,12,0,0,17,1,164,8,0,0,1,2,21,0,184,1,0,0,255,255,255,255,21,0,0,0,4,0,0,0,112,240,135,132,113,96, 
  135,132,114,208,6,128,86,112,10,128,68,96,204,130,69,248,203,130,70,144,75,127,87,224,137,130,136,48,3,128,89,120,9,128,90,16,9,128,75,0,75,128,91,128,8,128,13,48,13,128,46,200,12,128,116,64,6,128,117,176, 
  5,128,119,32,197,128,128,144,4,128,129,40,4,128,135,192,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,84,12,0,0,1,15,1,84,12,0,0,17,1,115,80,0,0,1,15,1,84,12,0,0,17,1,116,79,0, 
  0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,84,12,0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,84,12,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,84,12,0,0,1,19,89,0,0, 
  0,247,0,0,0,1,0,17,1,84,12,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,84,12,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,84,12,0,0,1,19,86,0,0,0,240,0,0,0,1, 
  0,17,1,84,12,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,84,12,0,0,1,15,1,84,12,0,0,17,1,193,35,0,0,1,15,1,84,12,0,0,17,1,134,33,0,0,1,19,69,0,0,0,204,0,0, 
  0,1,0,17,1,84,12,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,84,12,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,84,12,0,0,1,15,1,84,12,0,0,17,1,83,33,0,0,1,15, 
  1,84,12,0,0,17,1,118,14,0,0,1,15,1,84,12,0,0,17,1,64,14,0,0,1,15,1,84,12,0,0,17,1,13,14,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,84,12,0,0,1,2,21,1,39, 
  0,0,0,219,177,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,1,53,0,0,0,72,179,1,0,5,0, 
  0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,53,56,1,128,8,4,19,86,0,0,0,239,0,0,0,3,0,14,1,2,21,1,47,0,0,0,156,179,1,0,5,0,0,0,2,0,0,0, 
  6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,81,56,1,128,8,4,17,1,177,14,0,0,1,19,68,0,0,0,203,0,0,0,1,0,1,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104, 
  208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1, 
  0,0,1,0,17,1,139,15,0,0,1,4,15,1,139,15,0,0,17,1,80,95,0,0,1,4,15,1,139,15,0,0,17,1,212,80,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,139,15,0,0,1,4,15, 
  1,139,15,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,139,15,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,139,15,0,0,1,4,15,1,139,15,0,0,17,1,202, 
  7,0,0,1,4,15,1,139,15,0,0,17,1,164,8,0,0,1,2,21,0,149,1,0,0,255,255,255,255,20,0,0,0,4,0,0,0,112,208,71,132,113,64,71,132,114,176,6,128,86,32,10,128,116,32,6,128,69,168,139, 
  130,70,64,75,127,87,192,73,130,136,16,3,128,89,88,9,128,90,240,8,128,75,176,74,128,91,96,8,128,13,24,12,128,46,176,11,128,117,144,5,128,119,0,197,128,128,112,4,128,129,8,4,128,135,160,3,128,19,135,0, 
  0,0,121,1,0,0,1,0,17,1,139,15,0,0,1,15,1,139,15,0,0,17,1,242,32,0,0,1,15,1,139,15,0,0,17,1,156,31,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,139,15,0,0,1,19, 
  90,0,0,0,252,0,0,0,1,0,17,1,139,15,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,139,15,0,0,1,19,89,0,0,0,247,0,0,0,1,0,17,1,139,15,0,0,1,19,86,0,0,0,242,0, 
  0,0,1,0,17,1,139,15,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,139,15,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,139,15,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1, 
  139,15,0,0,1,15,1,139,15,0,0,17,1,193,35,0,0,1,15,1,139,15,0,0,17,1,135,17,0,0,1,19,69,0,0,0,205,0,0,0,3,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,139,15,0,0, 
  1,19,112,0,0,0,47,1,0,0,1,0,17,1,139,15,0,0,1,15,1,139,15,0,0,17,1,84,17,0,0,1,1,15,1,139,15,0,0,17,1,33,17,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,139, 
  15,0,0,1,2,21,1,39,0,0,0,219,177,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21,1,39,0, 
  0,0,251,179,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120,1,0,0,1,0,1,21,1,217,0,0,0,43,181,1,0,13,0,0, 
  0,3,0,0,0,104,208,3,128,1,224,197,129,2,224,5,128,91,232,5,128,52,112,5,128,45,96,3,129,6,224,5,128,7,224,197,128,57,200,2,129,85,48,2,128,95,216,68,128,103,88,6,128,121,64,4,128,4,19,75, 
  0,0,0,215,0,0,0,1,0,17,1,108,18,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,108,18,0,0,1,4,15,1,108,18,0,0,17,1,164,8,0,0,1,4,15,1,108,18,0,0,17,1,80,95, 
  0,0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1,108,18,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,108,18,0,0,1,4,15,1,108,18,0,0,17,1,202,7,0,0,1,8,4,15,1,108, 
  18,0,0,17,1,160,10,0,0,1,4,15,1,108,18,0,0,17,1,212,80,0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,88,196,131,113,208, 
  194,131,114,32,9,128,86,224,5,128,116,112,6,128,117,48,8,128,70,56,71,127,119,96,131,130,136,232,4,128,89,192,3,128,90,192,3,128,75,160,71,128,91,176,9,128,13,144,8,128,46,208,6,128,128,200,3,128,129,64, 
  10,128,135,120,5,128,19,86,0,0,0,241,0,0,0,1,0,17,1,108,18,0,0,1,19,90,0,0,0,251,0,0,0,2,0,1,1,19,112,0,0,0,48,1,0,0,1,0,17,1,108,18,0,0,1,19,86,0,0,0, 
  240,0,0,0,1,0,17,1,108,18,0,0,1,19,135,0,0,0,121,1,0,0,1,0,17,1,108,18,0,0,1,15,1,108,18,0,0,17,1,149,28,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,108,18,0, 
  0,1,19,89,0,0,0,249,0,0,0,2,0,1,15,1,108,18,0,0,17,1,13,14,0,0,1,15,1,108,18,0,0,17,1,83,33,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,108,18,0,0,1,19,89, 
  0,0,0,250,0,0,0,2,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,108,18,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,108,18,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,108, 
  18,0,0,1,15,1,108,18,0,0,17,1,194,19,0,0,1,2,21,1,69,0,0,0,34,182,1,0,5,0,0,0,2,0,0,0,112,224,1,128,1,32,2,128,2,32,130,128,115,48,1,128,10,112,1,128,4,17,1,106, 
  26,0,0,1,4,15,1,123,25,0,0,17,1,143,24,0,0,1,4,17,1,19,20,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,1,57,0,0,0,155,183,1,0,4,0,0,0,2,0,0,0,10,80,1, 
  128,1,192,1,128,2,192,129,127,115,16,1,128,4,17,1,48,23,0,0,1,4,15,1,205,22,0,0,17,1,88,20,0,0,1,8,19,116,0,0,0,71,1,0,0,2,0,1,21,1,86,0,0,0,126,184,1,0,6,0, 
  0,0,2,0,0,0,55,216,1,128,1,80,193,128,2,80,193,128,11,72,66,127,125,152,1,128,126,88,1,128,8,4,17,1,142,22,0,0,1,4,17,1,80,21,0,0,1,4,15,1,231,20,0,0,17,1,175,20,0,0, 
  1,4,19,132,0,0,0,114,1,0,0,2,0,1,2,21,1,44,0,0,0,43,185,1,0,3,0,0,0,1,0,0,0,2,88,1,128,1,88,65,128,125,240,0,128,4,19,133,0,0,0,117,1,0,0,2,0,1,8,19, 
  133,0,0,0,118,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,132,56,1,128,133,208,0,128,15,1,231,20,0,0,17,1,16,21,0,0,1,1,2,21,7,36,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,112,1,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,53,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,125,64,1,128,4,15,1,37,22,0,0,17,1,161,21,0,0,1,4,19,132,0,0,0,113,1,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,56,0,0,0,127,185,1,0,4,0,0,0,2,0,0,0,125,80,1,128,1,184,193,127,2,184,65,128,126,16,1,128,4,17,1,229,21,0,0,1,4, 
  19,133,0,0,0,117,1,0,0,2,0,1,8,19,133,0,0,0,118,1,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,115,1,0,0,5,0,14, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,132,56,1,128,133,208,0,128,15,1,37,22,0, 
  0,17,1,78,22,0,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,111,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,116,1,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,205,22,0,0,17,1,246,22,0,0,1,1,2,21,1, 
  45,0,0,0,223,185,1,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,115,240,0,128,4,19,116,0,0,0,77,1,0,0,4,0,14,1,8,19,116,0,0,0,67,1,0,0,3,0,14,1,21,7,36,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,103,24,0,0,17,1,123,23,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  116,0,0,0,76,1,0,0,3,0,1,21,1,86,0,0,0,126,184,1,0,6,0,0,0,2,0,0,0,55,216,1,128,1,80,193,128,2,80,193,128,11,72,66,127,125,152,1,128,126,88,1,128,8,4,17,1,142,22,0, 
  0,1,4,17,1,210,23,0,0,1,4,15,1,231,20,0,0,17,1,175,20,0,0,1,4,19,132,0,0,0,114,1,0,0,2,0,1,2,21,7,53,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128, 
  125,64,1,128,4,15,1,37,22,0,0,17,1,35,24,0,0,1,4,19,132,0,0,0,113,1,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21, 
  1,56,0,0,0,127,185,1,0,4,0,0,0,2,0,0,0,125,16,1,128,1,184,193,127,2,184,65,128,126,120,1,128,4,19,133,0,0,0,117,1,0,0,2,0,1,4,17,1,229,21,0,0,1,8,19,133,0,0,0, 
  118,1,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,48,65,128,132,208,0,128,19,116,0,0,0,75,1,0,0,4,0,1,1,2,21,1,86,0,0,0,126,184,1,0,6,0,0, 
  0,2,0,0,0,55,216,1,128,1,80,193,128,2,80,193,128,11,72,66,127,125,152,1,128,126,88,1,128,8,4,17,1,142,22,0,0,1,4,17,1,230,24,0,0,1,4,15,1,231,20,0,0,17,1,175,20,0,0,1, 
  4,19,132,0,0,0,114,1,0,0,2,0,1,2,21,7,53,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,125,64,1,128,4,15,1,37,22,0,0,17,1,55,25,0,0,1,4,19,132,0,0,0, 
  113,1,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,56,0,0,0,127,185,1,0,4,0,0,0,2,0,0,0,125,16,1,128,1,120,193, 
  127,2,120,65,128,126,128,1,128,4,19,133,0,0,0,117,1,0,0,2,0,1,8,4,17,1,229,21,0,0,1,19,133,0,0,0,118,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,116,56,65,128,132,208,0,128,15,1,123,25,0,0,17,1,164,25,0,0,1,1,2,21,1,51,0,0,0,183,186,1,0,4,0,0,0,2,0,0,0,112,16,1,128,1,144,1,128,2,144,1,128,115,80,1,128,4,17, 
  1,48,26,0,0,1,4,17,1,228,25,0,0,1,8,19,116,0,0,0,69,1,0,0,2,0,14,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,116,0,0,0,79,1,0,0, 
  4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,116,0,0,0,78,1,0,0,3,0,14,1,21,1,45,0,0,0,223,185,1,0,3,0,0,0,1,0, 
  0,0,2,240,0,128,1,240,64,128,115,248,0,128,8,4,19,116,0,0,0,80,1,0,0,4,0,14,1,19,116,0,0,0,74,1,0,0,3,0,14,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  123,208,0,128,63,64,193,127,4,15,1,32,28,0,0,17,1,123,23,0,0,1,4,17,1,193,26,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,116,0, 
  0,0,72,1,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,248,27,0,0,17,1,12,27,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,19,116,0,0,0,70,1,0,0,3,0,1,21,1,86,0,0,0,126,184,1,0,6,0,0,0,2,0,0,0,55,216,1,128,1,80,193,128,2,80,193,128,11,72,66,127,125,152, 
  1,128,126,88,1,128,8,4,17,1,142,22,0,0,1,4,17,1,99,27,0,0,1,4,15,1,231,20,0,0,17,1,175,20,0,0,1,4,19,132,0,0,0,114,1,0,0,2,0,1,2,21,7,53,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,58,208,0,128,125,64,1,128,4,15,1,37,22,0,0,17,1,180,27,0,0,1,4,19,132,0,0,0,113,1,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,2,21,1,56,0,0,0,127,185,1,0,4,0,0,0,2,0,0,0,125,80,1,128,1,184,193,127,2,184,65,128,126,16,1,128,4,17,1,229,21,0,0,1,4,19,133,0,0,0, 
  117,1,0,0,2,0,1,8,19,133,0,0,0,118,1,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,48,65,128,132,208,0,128,19,116,0,0,0,66,1,0,0,4,0,1,1,2, 
  21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,32,28,0,0,17,1,73,28,0,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63, 
  176,0,128,4,19,116,0,0,0,73,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,116,0,0,0,68,1,0,0,3,0,14,1,21,1,85, 
  0,0,0,154,187,1,0,6,0,0,0,2,0,0,0,124,80,1,128,1,56,194,128,2,56,194,128,123,144,1,128,25,208,1,128,122,64,2,128,4,17,1,73,30,0,0,1,4,17,1,246,28,0,0,1,4,19,129,0,0, 
  0,98,1,0,0,2,0,1,8,4,19,129,0,0,0,99,1,0,0,2,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,1,108,0,0,0,159,188,1,0,6,0,0,0,2,0,0,0,104,240,1,128,1,80,129,128, 
  2,80,1,128,53,248,66,128,85,96,66,128,121,88,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,99,29,0,0,1,4,15,1,99,29,0,0,17,1,80,95,0,0,1,4,19,75,0,0,0,215,0,0,0, 
  1,0,17,1,99,29,0,0,1,4,19,129,0,0,0,105,1,0,0,3,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,112,48,2,129,129,48,1,128,114,200,1,128,75,152,2,128,128,56,1, 
  128,1,19,112,0,0,0,48,1,0,0,1,0,17,1,99,29,0,0,1,15,1,99,29,0,0,17,1,9,30,0,0,1,15,1,99,29,0,0,17,1,201,29,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,99, 
  29,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,103,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,104,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,108,0,0,0,159,188,1,0,6,0,0,0,2,0,0,0,104,240,1,128,1,80,129,128,2,80,1,128,53,248,66,128,85,96,66,128,121,88,1,128,8,4,19,128,0, 
  0,0,97,1,0,0,1,0,17,1,182,30,0,0,1,4,15,1,182,30,0,0,17,1,80,95,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,182,30,0,0,1,4,19,129,0,0,0,102,1,0,0,3,0, 
  1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,112,48,2,129,129,48,1,128,114,200,1,128,75,152,2,128,128,56,1,128,1,19,112,0,0,0,48,1,0,0,1,0,17,1,182,30,0,0,1,15, 
  1,182,30,0,0,17,1,92,31,0,0,1,15,1,182,30,0,0,17,1,28,31,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,182,30,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,41,176,0,128,4,19,129,0,0,0,100,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,36,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,101,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,69,0,0,0,34,182, 
  1,0,5,0,0,0,2,0,0,0,112,48,1,128,1,32,2,128,2,32,130,128,115,112,1,128,10,176,1,128,4,17,1,173,32,0,0,1,4,17,1,86,32,0,0,1,4,15,1,237,31,0,0,17,1,143,24,0,0,1, 
  8,19,116,0,0,0,81,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,237,31,0,0,17,1,22,32,0,0,1,1,2,21,1,51,0,0,0, 
  183,186,1,0,4,0,0,0,2,0,0,0,112,16,1,128,1,144,1,128,2,144,1,128,115,80,1,128,4,17,1,48,26,0,0,1,4,17,1,228,25,0,0,1,8,19,116,0,0,0,69,1,0,0,2,0,14,1,21,7, 
  48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,193,26,0,0,1,4,15,1,32,28,0,0,17,1,123,23,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,116,0,0,0,72,1,0,0,2,0,1,21,1,57,0,0,0,155,183,1,0,4,0,0,0,2,0,0,0,10,16,1,128,1,192,1,128,2,192,129,127,115,128,1,128,4, 
  15,1,205,22,0,0,17,1,88,20,0,0,1,4,17,1,48,23,0,0,1,8,19,116,0,0,0,71,1,0,0,2,0,1,21,1,85,0,0,0,154,187,1,0,6,0,0,0,2,0,0,0,124,0,2,128,1,248,193,128, 
  2,248,193,128,123,184,1,128,25,80,1,128,122,64,2,128,4,19,129,0,0,0,98,1,0,0,2,0,1,4,17,1,246,28,0,0,1,8,4,17,1,73,30,0,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,19, 
  129,0,0,0,106,1,0,0,1,0,1,21,1,39,0,0,0,251,179,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120,1,0,0,1, 
  0,1,21,1,217,0,0,0,43,181,1,0,13,0,0,0,3,0,0,0,104,80,6,128,1,192,198,129,2,192,6,128,91,16,3,128,52,160,2,128,45,24,4,129,6,192,6,128,7,192,198,128,57,136,4,129,85,32,5,128, 
  95,128,67,128,103,48,2,128,121,184,5,128,4,15,1,107,34,0,0,17,1,212,80,0,0,1,4,15,1,107,34,0,0,17,1,202,7,0,0,1,4,15,1,107,34,0,0,17,1,160,10,0,0,1,4,19,119,0,0,0, 
  86,1,0,0,1,0,17,1,107,34,0,0,1,4,15,1,107,34,0,0,17,1,164,8,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,107,34,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1, 
  107,34,0,0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1,107,34,0,0,1,4,15,1,107,34,0,0,17,1,80,95,0,0,1,8,19,90,0,0,0,253,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255, 
  255,18,0,0,0,4,0,0,0,112,24,198,131,113,96,195,131,114,136,5,128,86,0,8,128,116,40,5,128,117,200,4,128,70,240,67,127,119,160,135,130,136,208,2,128,89,88,4,128,90,88,4,128,75,168,70,128,91,136,9, 
  128,13,24,10,128,46,56,7,128,128,248,8,128,129,144,8,128,135,96,4,128,19,135,0,0,0,121,1,0,0,1,0,17,1,107,34,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,107,34,0,0,1,15,1,107, 
  34,0,0,17,1,83,33,0,0,1,1,15,1,107,34,0,0,17,1,149,28,0,0,1,19,89,0,0,0,250,0,0,0,2,0,1,19,89,0,0,0,249,0,0,0,2,0,1,19,86,0,0,0,242,0,0,0,1,0,17, 
  1,107,34,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,107,34,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,107,34,0,0,1,15,1,107,34,0,0,17,1,13,14,0,0,1,19,90,0,0,0, 
  251,0,0,0,2,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,107,34,0,0,1,15,1,107,34,0,0,17,1,194,19,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,107,34,0,0,1,19,70,0,0, 
  0,208,0,0,0,1,0,17,1,107,34,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,107,34,0,0,1,2,21,1,53,0,0,0,41,189,1,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2, 
  160,129,127,7,160,1,128,61,48,1,128,4,15,1,76,79,0,0,17,1,2,36,0,0,1,8,19,87,0,0,0,244,0,0,0,1,0,1,21,1,213,1,0,0,148,189,1,0,27,0,0,0,4,0,0,0,32,88,77,133, 
  1,240,195,131,2,240,195,131,35,8,12,128,36,152,11,133,37,40,11,133,6,240,3,131,7,240,3,131,40,216,9,128,41,104,9,128,10,56,142,130,43,96,8,128,44,200,7,128,45,88,7,128,30,200,13,128,47,232,198,130, 
  33,232,140,129,34,120,12,128,38,184,74,129,39,72,10,128,42,248,8,128,48,120,6,128,49,8,6,128,54,152,5,128,84,0,5,128,85,104,4,128,95,248,3,128,8,4,15,1,250,77,0,0,17,1,120,77,0,0,1,4, 
  19,75,0,0,0,215,0,0,0,1,0,17,1,250,77,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,250,77,0,0,1,4,15,1,250,77,0,0,17,1,7,71,0,0,1,4,15,1,250,77,0,0,17,1, 
  148,70,0,0,1,4,15,1,250,77,0,0,17,1,33,70,0,0,1,4,15,1,250,77,0,0,17,1,105,68,0,0,1,4,15,1,250,77,0,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17, 
  1,250,77,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,250,77,0,0,1,4,15,1,250,77,0,0,17,1,246,59,0,0,1,4,15,1,250,77,0,0,17,1,131,59,0,0,1,4,15,1,250,77,0,0, 
  17,1,16,59,0,0,1,4,15,1,250,77,0,0,17,1,157,58,0,0,1,4,15,1,250,77,0,0,17,1,42,58,0,0,1,4,15,1,250,77,0,0,17,1,183,57,0,0,1,4,15,1,250,77,0,0,17,1,68,57, 
  0,0,1,4,15,1,250,77,0,0,17,1,209,56,0,0,1,4,15,1,250,77,0,0,17,1,94,56,0,0,1,4,15,1,250,77,0,0,17,1,235,55,0,0,1,4,15,1,250,77,0,0,17,1,120,55,0,0,1,4, 
  15,1,250,77,0,0,17,1,251,53,0,0,1,4,15,1,250,77,0,0,17,1,216,37,0,0,1,2,21,1,222,1,0,0,27,194,1,0,26,0,0,0,4,0,0,0,32,168,205,132,1,208,195,131,2,208,195,131,35,88, 
  12,128,36,232,203,132,37,120,203,132,38,8,203,131,39,152,10,128,40,40,138,131,41,184,9,128,42,72,9,128,11,136,206,129,44,24,8,128,45,168,7,128,30,24,14,128,47,56,135,130,33,56,13,129,34,200,12,128,43,176, 
  8,128,48,200,6,128,49,88,6,128,54,232,5,128,56,120,5,128,84,224,4,128,85,72,4,128,95,216,3,128,8,4,15,1,183,39,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,183, 
  39,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,183,39,0,0,1,4,15,1,183,39,0,0,17,1,47,10,0,0,1,4,15,1,183,39,0,0,17,1,7,71,0,0,1,4,15,1,183,39,0,0,17,1, 
  148,70,0,0,1,4,15,1,183,39,0,0,17,1,33,70,0,0,1,4,15,1,183,39,0,0,17,1,105,68,0,0,1,4,15,1,183,39,0,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17, 
  1,183,39,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,183,39,0,0,1,4,15,1,183,39,0,0,17,1,246,59,0,0,1,4,15,1,183,39,0,0,17,1,131,59,0,0,1,4,15,1,183,39,0,0, 
  17,1,16,59,0,0,1,4,15,1,183,39,0,0,17,1,157,58,0,0,1,4,15,1,183,39,0,0,17,1,42,58,0,0,1,4,15,1,183,39,0,0,17,1,183,57,0,0,1,4,15,1,183,39,0,0,17,1,68,57, 
  0,0,1,4,15,1,183,39,0,0,17,1,209,56,0,0,1,4,15,1,183,39,0,0,17,1,94,56,0,0,1,4,15,1,183,39,0,0,17,1,235,55,0,0,1,4,15,1,183,39,0,0,17,1,120,55,0,0,1,4, 
  15,1,183,39,0,0,17,1,251,53,0,0,1,4,19,80,0,0,0,225,0,0,0,2,0,1,2,21,0,81,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,80,128,5,128,81,232,6,128,34,96,4,128,35,240,5, 
  128,36,176,2,128,37,0,9,128,38,240,4,128,42,136,69,129,40,208,3,128,9,224,7,128,10,128,69,127,43,64,3,129,74,144,9,128,45,128,198,128,78,112,8,128,75,80,7,128,93,32,10,128,19,9,0,0,0,30,0, 
  0,0,1,0,17,1,183,39,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,183,39,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,183,39,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1, 
  183,39,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,183,39,0,0,1,1,15,1,183,39,0,0,17,1,217,50,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,183,39,0,0,1,15,1,183,39,0, 
  0,17,1,237,44,0,0,1,15,1,183,39,0,0,17,1,191,41,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,183,39,0,0,1,19,81,0,0,0,226,0,0,0,1,0,17,1,183,39,0,0,1,19,42,0, 
  0,0,136,0,0,0,1,0,17,1,183,39,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,183,39,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,183,39,0,0,1,15,1,183,39,0,0,17,1,9, 
  41,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,78,41,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,19,78,0,0,0,219,0,0,0,1,0,1,21,1,50,0,0,0,112,175,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,129, 
  41,0,0,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,46,88,129,128,79,240,0,128,78,80,1,128,19,78,0,0,0,220,0,0,0,3,0,1,1,19,79,0,0,0,221,0,0,0,1,0,17, 
  1,129,41,0,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,16,1,128,59,208,192,127,4,17,1,11,42,0,0,1,4,19,80,0,0,0,224,0,0,0,3,0,14,1,21,9,27,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,187,1,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,248,140,132,1,144,195,131,2,144,195,131,35,168,11,128,36,56,75, 
  132,37,200,74,132,38,88,138,131,39,232,9,128,40,120,9,128,41,8,9,128,42,152,8,128,43,0,8,128,44,104,7,128,45,248,6,128,30,104,13,128,47,136,6,130,33,136,204,128,34,24,12,128,48,24,6,128,49,168,5, 
  128,54,56,5,128,84,160,4,128,85,8,4,128,95,152,3,128,8,4,15,1,199,43,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,199,43,0,0,1,4,19,74,0,0,0,214,0,0, 
  0,1,0,17,1,199,43,0,0,1,4,15,1,199,43,0,0,17,1,7,71,0,0,1,4,15,1,199,43,0,0,17,1,148,70,0,0,1,4,15,1,199,43,0,0,17,1,33,70,0,0,1,4,15,1,199,43,0,0,17, 
  1,105,68,0,0,1,4,15,1,199,43,0,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,199,43,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,199,43,0,0,1,4,15, 
  1,199,43,0,0,17,1,246,59,0,0,1,4,15,1,199,43,0,0,17,1,131,59,0,0,1,4,15,1,199,43,0,0,17,1,16,59,0,0,1,4,15,1,199,43,0,0,17,1,157,58,0,0,1,4,15,1,199,43,0, 
  0,17,1,42,58,0,0,1,4,15,1,199,43,0,0,17,1,183,57,0,0,1,4,15,1,199,43,0,0,17,1,68,57,0,0,1,4,15,1,199,43,0,0,17,1,209,56,0,0,1,4,15,1,199,43,0,0,17,1,94, 
  56,0,0,1,4,15,1,199,43,0,0,17,1,235,55,0,0,1,4,15,1,199,43,0,0,17,1,120,55,0,0,1,4,15,1,199,43,0,0,17,1,251,53,0,0,1,2,21,0,37,1,0,0,255,255,255,255,14,0,0, 
  0,3,0,0,0,40,104,5,128,9,200,200,130,34,56,72,129,35,168,71,129,36,24,7,128,37,136,6,130,38,248,69,129,42,0,133,128,43,112,132,128,74,224,3,128,75,80,3,128,78,192,2,128,81,184,2,128,93,80,2, 
  128,15,1,199,43,0,0,17,1,9,41,0,0,1,1,19,42,0,0,0,136,0,0,0,1,0,17,1,199,43,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,199,43,0,0,1,19,37,0,0,0,120,0,0,0, 
  1,0,17,1,199,43,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,199,43,0,0,1,15,1,199,43,0,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,199,43,0,0,1,19,9, 
  0,0,0,32,0,0,0,1,0,17,1,199,43,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,199,43,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,199,43,0,0,1,19,9,0,0,0,29,0,0, 
  0,1,0,17,1,199,43,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,199,43,0,0,1,19,81,0,0,0,227,0,0,0,3,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44, 
  64,1,128,125,208,0,128,4,19,10,0,0,0,37,0,0,0,3,0,14,1,4,17,1,57,45,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,104, 
  0,0,0,229,195,1,0,6,0,0,0,2,0,0,0,48,96,2,128,1,80,193,128,2,80,1,128,47,208,2,128,49,240,65,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,162,45,0,0,1,4, 
  15,1,162,45,0,0,17,1,148,70,0,0,1,4,15,1,162,45,0,0,17,1,33,70,0,0,1,4,15,1,162,45,0,0,17,1,105,68,0,0,1,2,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0, 
  40,216,66,128,44,72,2,128,10,208,131,128,11,104,131,128,46,184,1,128,79,80,1,128,15,1,162,45,0,0,17,1,57,47,0,0,1,19,79,0,0,0,221,0,0,0,1,0,17,1,162,45,0,0,1,19,11,0,0,0, 
  38,0,0,0,1,0,17,1,162,45,0,0,1,19,44,0,0,0,141,0,0,0,1,0,17,1,162,45,0,0,1,15,1,162,45,0,0,17,1,30,46,0,0,1,1,2,21,7,49,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,44,72,1,128,125,208,0,128,4,19,10,0,0,0,36,0,0,0,5,0,14,14,1,4,17,1,107,46,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,2,21,1,104,0,0,0,229,195,1,0,6,0,0,0,2,0,0,0,48,96,2,128,1,80,193,128,2,80,1,128,47,208,2,128,49,240,65,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1, 
  212,46,0,0,1,4,15,1,212,46,0,0,17,1,148,70,0,0,1,4,15,1,212,46,0,0,17,1,33,70,0,0,1,4,15,1,212,46,0,0,17,1,105,68,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,40,136,66,128,44,40,2,128,46,152,1,128,11,24,67,128,79,48,1,128,15,1,212,46,0,0,17,1,57,47,0,0,1,19,79,0,0,0,221,0,0,0,1,0,17,1,212,46,0,0,1,19,11,0, 
  0,0,39,0,0,0,3,0,1,19,44,0,0,0,141,0,0,0,1,0,17,1,212,46,0,0,1,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,126,47,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,44,0,0,0,140,0,0,0,1,0,1,21,1,205,1,0,0,66,198,1,0,25,0,0,0,4,0,0,0,32,24,205,132, 
  1,176,195,131,2,176,195,131,35,200,11,128,36,88,139,132,37,232,138,132,38,120,202,131,39,8,10,128,40,152,9,128,41,40,9,128,10,248,13,130,43,32,8,128,44,136,7,128,45,24,7,128,30,136,13,128,47,168,70,130, 
  33,168,12,129,34,56,12,128,42,184,8,128,48,56,6,128,49,200,5,128,54,88,5,128,84,192,4,128,85,40,4,128,95,184,3,128,8,4,15,1,163,49,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0, 
  0,1,0,17,1,163,49,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,163,49,0,0,1,4,15,1,163,49,0,0,17,1,7,71,0,0,1,4,15,1,163,49,0,0,17,1,148,70,0,0,1,4,15,1, 
  163,49,0,0,17,1,33,70,0,0,1,4,15,1,163,49,0,0,17,1,105,68,0,0,1,4,15,1,163,49,0,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,163,49,0,0,1,4,19, 
  37,0,0,0,118,0,0,0,1,0,17,1,163,49,0,0,1,4,15,1,163,49,0,0,17,1,246,59,0,0,1,4,15,1,163,49,0,0,17,1,131,59,0,0,1,4,15,1,163,49,0,0,17,1,16,59,0,0,1,4, 
  15,1,163,49,0,0,17,1,157,58,0,0,1,4,15,1,163,49,0,0,17,1,42,58,0,0,1,4,15,1,163,49,0,0,17,1,183,57,0,0,1,4,15,1,163,49,0,0,17,1,68,57,0,0,1,4,15,1,163,49, 
  0,0,17,1,209,56,0,0,1,4,15,1,163,49,0,0,17,1,94,56,0,0,1,4,15,1,163,49,0,0,17,1,235,55,0,0,1,4,15,1,163,49,0,0,17,1,120,55,0,0,1,4,15,1,163,49,0,0,17,1, 
  251,53,0,0,1,4,15,1,163,49,0,0,17,1,76,49,0,0,1,2,21,1,45,0,0,0,12,199,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,56,248,0,128,8,4,15,1,122,49,0,0,17,1, 
  47,10,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,10,56,1,128,45,208,0,128,15,1,122,49,0,0,17,1,237,44,0,0,1,1,2,21,0,53,1,0,0,255,255,255,255,15,0,0, 
  0,3,0,0,0,40,136,5,128,9,72,9,128,10,232,72,129,35,200,135,129,36,56,135,129,37,168,70,130,38,24,198,129,34,88,72,128,42,32,197,128,43,144,196,128,44,136,4,128,74,248,3,128,75,104,3,128,78,216,2, 
  128,93,112,2,128,15,1,163,49,0,0,17,1,9,41,0,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,163,49,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,163,49,0,0,1,19,37,0,0,0,120, 
  0,0,0,1,0,17,1,163,49,0,0,1,1,19,9,0,0,0,35,0,0,0,1,0,17,1,163,49,0,0,1,15,1,163,49,0,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,163,49,0, 
  0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,163,49,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,163,49,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,163,49,0,0,1,19,9,0,0, 
  0,29,0,0,0,1,0,17,1,163,49,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,163,49,0,0,1,19,44,0,0,0,139,0,0,0,3,0,1,19,44,0,0,0,138,0,0,0,3,0,1,2,21,7,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,17,1,30,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,34, 
  0,0,0,1,0,1,21,1,187,1,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,248,140,132,1,144,195,131,2,144,195,131,35,168,11,128,36,56,75,132,37,200,74,132,38,88,138,131,39,232,9,128,40,120,9,128, 
  41,8,9,128,42,152,8,128,43,0,8,128,44,104,7,128,45,248,6,128,30,104,13,128,47,136,6,130,33,136,204,128,34,24,12,128,48,24,6,128,49,168,5,128,54,56,5,128,84,160,4,128,85,8,4,128,95,152,3,128, 
  8,4,15,1,218,52,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,218,52,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,218,52,0,0,1,4,15,1,218,52,0,0, 
  17,1,7,71,0,0,1,4,15,1,218,52,0,0,17,1,148,70,0,0,1,4,15,1,218,52,0,0,17,1,33,70,0,0,1,4,15,1,218,52,0,0,17,1,105,68,0,0,1,4,15,1,218,52,0,0,17,1,105,60, 
  0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,218,52,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,218,52,0,0,1,4,15,1,218,52,0,0,17,1,246,59,0,0,1,4,15,1,218,52, 
  0,0,17,1,131,59,0,0,1,4,15,1,218,52,0,0,17,1,16,59,0,0,1,4,15,1,218,52,0,0,17,1,157,58,0,0,1,4,15,1,218,52,0,0,17,1,42,58,0,0,1,4,15,1,218,52,0,0,17,1, 
  183,57,0,0,1,4,15,1,218,52,0,0,17,1,68,57,0,0,1,4,15,1,218,52,0,0,17,1,209,56,0,0,1,4,15,1,218,52,0,0,17,1,94,56,0,0,1,4,15,1,218,52,0,0,17,1,235,55,0,0, 
  1,4,15,1,218,52,0,0,17,1,120,55,0,0,1,4,15,1,218,52,0,0,17,1,251,53,0,0,1,2,21,0,32,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,64,5,128,9,160,8,128,34,16,72,129, 
  35,128,71,129,36,240,6,128,37,96,198,129,38,208,69,129,42,216,132,128,43,72,132,128,74,184,3,128,75,40,3,128,78,152,2,128,93,48,2,128,15,1,218,52,0,0,17,1,9,41,0,0,1,19,42,0,0,0,136,0, 
  0,0,1,0,17,1,218,52,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,218,52,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,218,52,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1, 
  218,52,0,0,1,15,1,218,52,0,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,218,52,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,218,52,0,0,1,19,9,0,0,0,31, 
  0,0,0,1,0,17,1,218,52,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,218,52,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,218,52,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17, 
  1,218,52,0,0,1,19,42,0,0,0,135,0,0,0,3,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,80,55,0,0,17,1,70,54,0,0,1,21,9,27,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,95,0,0,0,1,0,1,21,1,99,0,0,0,115,199,1,0,6,0,0,0,2,0,0,0,48,56,2,128,1,80,193,128, 
  2,80,1,128,47,168,130,128,49,200,1,128,95,88,1,128,8,4,15,1,170,54,0,0,17,1,120,77,0,0,1,4,15,1,170,54,0,0,17,1,148,70,0,0,1,4,15,1,170,54,0,0,17,1,33,70,0,0,1,4, 
  15,1,170,54,0,0,17,1,105,68,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,152,194,128,73,144,194,128,78,0,2,128,92,152,1,128,93,48,1,128,15,1,170,54,0,0,17,1, 
  9,41,0,0,1,15,1,170,54,0,0,17,1,16,55,0,0,1,19,92,0,0,0,255,0,0,0,1,0,17,1,170,54,0,0,1,1,19,92,0,0,0,0,1,0,0,1,0,17,1,170,54,0,0,1,2,21,7,36,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,73,0,0,0,213,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2, 
  21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,48,1,128,73,208,0,128,19,34,0,0,0,94,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176, 
  0,128,4,15,1,195,55,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,105,0,0,0,1,0,1,21,0,39,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,104,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1, 
  54,56,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,97,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,96,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,169,56,0,0,17, 
  1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,113,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,112,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,28,57,0,0,17,1,70,54,0,0, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,109,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208, 
  0,128,35,48,193,127,19,35,0,0,0,108,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,143,57,0,0,17,1,70,54,0,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,101,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193, 
  127,19,35,0,0,0,100,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,2,58,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,115,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0, 
  0,114,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,117,58,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,111,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,110,0,0,0, 
  2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,232,58,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,35,0,0,0,103,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,102,0,0,0,2,0,1,1,2, 
  21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,91,59,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,19,35,0,0,0,107,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,106,0,0,0,2,0,1,1,2,21,7,36,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,206,59,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35, 
  0,0,0,99,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,73,208,0,128,35,48,193,127,19,35,0,0,0,98,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,65,60,0,0,17,1,70,54,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,36,0,0,0,117,0, 
  0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,36,48,1,128,73,208,0,128,19,36,0,0,0,116,0,0,0,2,0,1,1,2,21,1,204,1,0,0,241,199,1,0,25,0,0,0,4, 
  0,0,0,32,128,205,132,1,176,195,131,2,176,195,131,35,48,12,128,36,192,139,132,37,80,139,132,38,224,202,131,39,112,10,128,40,0,10,128,41,144,9,128,42,32,9,128,43,136,8,128,44,240,7,128,45,128,7,128,30, 
  240,13,129,47,168,70,130,33,16,13,129,34,160,12,128,46,24,7,128,48,56,6,128,49,200,5,128,54,88,5,128,84,192,4,128,85,40,4,128,95,184,3,128,8,4,15,1,3,64,0,0,17,1,120,77,0,0,1,4,19, 
  75,0,0,0,215,0,0,0,1,0,17,1,3,64,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,3,64,0,0,1,4,15,1,3,64,0,0,17,1,7,71,0,0,1,4,15,1,3,64,0,0,17,1,148, 
  70,0,0,1,4,15,1,3,64,0,0,17,1,33,70,0,0,1,4,15,1,3,64,0,0,17,1,105,68,0,0,1,4,19,38,0,0,0,123,0,0,0,2,0,1,4,15,1,3,64,0,0,17,1,54,62,0,0,1,4, 
  19,37,0,0,0,119,0,0,0,1,0,17,1,3,64,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,3,64,0,0,1,4,15,1,3,64,0,0,17,1,246,59,0,0,1,4,15,1,3,64,0,0,17,1, 
  131,59,0,0,1,4,15,1,3,64,0,0,17,1,16,59,0,0,1,4,15,1,3,64,0,0,17,1,157,58,0,0,1,4,15,1,3,64,0,0,17,1,42,58,0,0,1,4,15,1,3,64,0,0,17,1,183,57,0,0, 
  1,4,15,1,3,64,0,0,17,1,68,57,0,0,1,4,15,1,3,64,0,0,17,1,209,56,0,0,1,4,15,1,3,64,0,0,17,1,94,56,0,0,1,4,15,1,3,64,0,0,17,1,235,55,0,0,1,4,15,1, 
  3,64,0,0,17,1,120,55,0,0,1,4,15,1,3,64,0,0,17,1,251,53,0,0,1,2,21,1,204,1,0,0,241,199,1,0,25,0,0,0,4,0,0,0,32,128,205,132,1,176,195,131,2,176,195,131,35,48,12,128, 
  36,192,139,132,37,80,139,132,38,224,202,131,39,112,10,128,40,0,10,128,41,144,9,128,42,32,9,128,43,136,8,128,44,240,7,128,45,128,7,128,30,240,13,129,47,168,70,130,33,16,13,129,34,160,12,128,46,24,7,128, 
  48,56,6,128,49,200,5,128,54,88,5,128,84,192,4,128,85,40,4,128,95,184,3,128,8,4,15,1,3,64,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,3,64,0,0,1,4,19, 
  74,0,0,0,214,0,0,0,1,0,17,1,3,64,0,0,1,4,15,1,3,64,0,0,17,1,7,71,0,0,1,4,15,1,3,64,0,0,17,1,148,70,0,0,1,4,15,1,3,64,0,0,17,1,33,70,0,0,1,4, 
  15,1,3,64,0,0,17,1,105,68,0,0,1,4,19,38,0,0,0,123,0,0,0,2,0,1,4,15,1,3,64,0,0,17,1,54,62,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,3,64,0,0,1,4, 
  19,37,0,0,0,118,0,0,0,1,0,17,1,3,64,0,0,1,4,15,1,3,64,0,0,17,1,246,59,0,0,1,4,15,1,3,64,0,0,17,1,131,59,0,0,1,4,15,1,3,64,0,0,17,1,16,59,0,0,1, 
  4,15,1,3,64,0,0,17,1,157,58,0,0,1,4,15,1,3,64,0,0,17,1,42,58,0,0,1,4,15,1,3,64,0,0,17,1,183,57,0,0,1,4,15,1,3,64,0,0,17,1,68,57,0,0,1,4,15,1,3, 
  64,0,0,17,1,209,56,0,0,1,4,15,1,3,64,0,0,17,1,94,56,0,0,1,4,15,1,3,64,0,0,17,1,235,55,0,0,1,4,15,1,3,64,0,0,17,1,120,55,0,0,1,4,15,1,3,64,0,0,17, 
  1,251,53,0,0,1,2,21,0,55,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,96,5,128,9,40,9,128,34,152,136,129,35,8,136,129,36,120,7,128,37,232,6,130,38,88,134,129,39,240,5,128,42,248,132, 
  128,43,104,132,128,74,216,3,128,75,72,3,128,78,184,2,128,93,80,2,128,15,1,3,64,0,0,17,1,9,41,0,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,3,64,0,0,1,19,37,0,0,0,121,0,0, 
  0,1,0,17,1,3,64,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,3,64,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,3,64,0,0,1,15,1,3,64,0,0,17,1,217,50,0,0,1,19, 
  9,0,0,0,33,0,0,0,1,0,17,1,3,64,0,0,1,15,1,3,64,0,0,17,1,59,65,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,3,64,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17, 
  1,3,64,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,3,64,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,3,64,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,3,64,0,0,1, 
  19,39,0,0,0,124,0,0,0,1,0,17,1,3,64,0,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,1,128,93,208,0,128,4,19,38,0,0,0,122,0,0,0,3,0,14,1,4, 
  17,1,135,65,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,187,1,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,248,140,132,1,144,195, 
  131,2,144,195,131,35,168,11,128,36,56,75,132,37,200,74,132,38,88,138,131,39,232,9,128,40,120,9,128,41,8,9,128,42,152,8,128,43,0,8,128,44,104,7,128,45,248,6,128,30,104,13,128,47,136,6,130,33,136,204, 
  128,34,24,12,128,48,24,6,128,49,168,5,128,54,56,5,128,84,160,4,128,85,8,4,128,95,152,3,128,8,4,15,1,67,67,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,67,67, 
  0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,67,67,0,0,1,4,15,1,67,67,0,0,17,1,7,71,0,0,1,4,15,1,67,67,0,0,17,1,148,70,0,0,1,4,15,1,67,67,0,0,17,1,33, 
  70,0,0,1,4,15,1,67,67,0,0,17,1,105,68,0,0,1,4,15,1,67,67,0,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,67,67,0,0,1,4,19,37,0,0,0,118,0,0, 
  0,1,0,17,1,67,67,0,0,1,4,15,1,67,67,0,0,17,1,246,59,0,0,1,4,15,1,67,67,0,0,17,1,131,59,0,0,1,4,15,1,67,67,0,0,17,1,16,59,0,0,1,4,15,1,67,67,0,0,17, 
  1,157,58,0,0,1,4,15,1,67,67,0,0,17,1,42,58,0,0,1,4,15,1,67,67,0,0,17,1,183,57,0,0,1,4,15,1,67,67,0,0,17,1,68,57,0,0,1,4,15,1,67,67,0,0,17,1,209,56,0, 
  0,1,4,15,1,67,67,0,0,17,1,94,56,0,0,1,4,15,1,67,67,0,0,17,1,235,55,0,0,1,4,15,1,67,67,0,0,17,1,120,55,0,0,1,4,15,1,67,67,0,0,17,1,251,53,0,0,1,2,21, 
  0,37,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,96,5,128,9,200,8,128,34,56,136,129,35,168,135,129,36,24,7,128,37,136,6,130,38,248,133,129,39,240,5,128,42,248,132,128,43,104,132,128,74,216,3, 
  128,75,72,3,128,78,184,2,128,93,80,2,128,15,1,67,67,0,0,17,1,9,41,0,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,67,67,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,67,67,0, 
  0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,67,67,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,67,67,0,0,1,15,1,67,67,0,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0, 
  1,0,17,1,67,67,0,0,1,1,19,9,0,0,0,32,0,0,0,1,0,17,1,67,67,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,67,67,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,67, 
  67,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,67,67,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,67,67,0,0,1,19,39,0,0,0,125,0,0,0,3,0,1,2,21,7,36,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,249,69,0,0,17,1,180,68,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,40,0,0, 
  0,129,0,0,0,1,0,1,21,1,50,0,0,0,187,200,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,84,248,0,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,231,68,0,0,1,2,21, 
  0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,216,64,128,76,208,0,128,1,15,1,231,68,0,0,17,1,16,69,0,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64, 
  65,128,62,208,0,128,4,19,76,0,0,0,217,0,0,0,3,0,14,1,4,17,1,92,69,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,50,0, 
  0,0,187,200,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,84,248,0,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,143,69,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,74,216,64,128,76,208,0,128,1,15,1,143,69,0,0,17,1,184,69,0,0,1,2,21,7,37,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,19,76,0,0,0,216,0,0,0, 
  5,0,14,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,40,48,65,128,76,208,0,128,19, 
  40,0,0,0,126,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,108,70,0,0,17,1,180,68,0,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,40,0,0,0,130,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,40,48,65,128,76,208,0,128,19,40,0,0,0,127, 
  0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,223,70,0,0,17,1,180,68,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,19,40,0,0,0,131,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,40,48,65,128,76,208,0,128,19,40,0,0,0,128,0,0,0,2,0, 
  1,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,17,1,65,71,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  2,21,1,187,1,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,248,140,132,1,144,195,131,2,144,195,131,35,168,11,128,36,56,75,132,37,200,74,132,38,88,138,131,39,232,9,128,40,120,9,128,41,8,9,128,42, 
  152,8,128,43,0,8,128,44,104,7,128,45,248,6,128,30,104,13,128,47,136,6,130,33,136,204,128,34,24,12,128,48,24,6,128,49,168,5,128,54,56,5,128,84,160,4,128,85,8,4,128,95,152,3,128,8,4,15,1,253, 
  72,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,253,72,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,253,72,0,0,1,4,15,1,253,72,0,0,17,1,7,71,0, 
  0,1,4,15,1,253,72,0,0,17,1,148,70,0,0,1,4,15,1,253,72,0,0,17,1,33,70,0,0,1,4,15,1,253,72,0,0,17,1,105,68,0,0,1,4,15,1,253,72,0,0,17,1,105,60,0,0,1,4,19, 
  37,0,0,0,119,0,0,0,1,0,17,1,253,72,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,253,72,0,0,1,4,15,1,253,72,0,0,17,1,246,59,0,0,1,4,15,1,253,72,0,0,17,1,131, 
  59,0,0,1,4,15,1,253,72,0,0,17,1,16,59,0,0,1,4,15,1,253,72,0,0,17,1,157,58,0,0,1,4,15,1,253,72,0,0,17,1,42,58,0,0,1,4,15,1,253,72,0,0,17,1,183,57,0,0,1, 
  4,15,1,253,72,0,0,17,1,68,57,0,0,1,4,15,1,253,72,0,0,17,1,209,56,0,0,1,4,15,1,253,72,0,0,17,1,94,56,0,0,1,4,15,1,253,72,0,0,17,1,235,55,0,0,1,4,15,1,253, 
  72,0,0,17,1,120,55,0,0,1,4,15,1,253,72,0,0,17,1,251,53,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,64,5,128,9,160,8,128,34,16,72,129,35,128,71,129,36, 
  240,6,128,37,96,198,129,38,208,69,129,42,216,132,128,43,72,132,128,74,184,3,128,75,40,3,128,78,152,2,128,93,48,2,128,15,1,253,72,0,0,17,1,9,41,0,0,1,19,42,0,0,0,136,0,0,0,1,0,17, 
  1,253,72,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,253,72,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,253,72,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,253,72,0,0,1, 
  15,1,253,72,0,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,253,72,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,253,72,0,0,1,19,9,0,0,0,31,0,0,0,1,0, 
  17,1,253,72,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,253,72,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,253,72,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,253,72,0,0, 
  1,15,1,253,72,0,0,17,1,31,74,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,89,74,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,187,1,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,248,140,132,1,144,195,131,2,144,195,131,35,168,11,128,36,56,75,132,37,200,74,132,38,88,138,131,39, 
  232,9,128,40,120,9,128,41,8,9,128,42,152,8,128,43,0,8,128,44,104,7,128,45,248,6,128,30,104,13,128,47,136,6,130,33,136,204,128,34,24,12,128,48,24,6,128,49,168,5,128,54,56,5,128,84,160,4,128,85, 
  8,4,128,95,152,3,128,8,4,15,1,21,76,0,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,21,76,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,21,76,0,0,1, 
  4,15,1,21,76,0,0,17,1,7,71,0,0,1,4,15,1,21,76,0,0,17,1,148,70,0,0,1,4,15,1,21,76,0,0,17,1,33,70,0,0,1,4,15,1,21,76,0,0,17,1,105,68,0,0,1,4,15,1,21, 
  76,0,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,21,76,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,21,76,0,0,1,4,15,1,21,76,0,0,17,1,246,59,0, 
  0,1,4,15,1,21,76,0,0,17,1,131,59,0,0,1,4,15,1,21,76,0,0,17,1,16,59,0,0,1,4,15,1,21,76,0,0,17,1,157,58,0,0,1,4,15,1,21,76,0,0,17,1,42,58,0,0,1,4,15, 
  1,21,76,0,0,17,1,183,57,0,0,1,4,15,1,21,76,0,0,17,1,68,57,0,0,1,4,15,1,21,76,0,0,17,1,209,56,0,0,1,4,15,1,21,76,0,0,17,1,94,56,0,0,1,4,15,1,21,76,0, 
  0,17,1,235,55,0,0,1,4,15,1,21,76,0,0,17,1,120,55,0,0,1,4,15,1,21,76,0,0,17,1,251,53,0,0,1,2,21,0,33,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,64,5,128,9, 
  160,8,128,34,16,72,129,35,128,71,129,36,240,6,128,37,96,198,129,38,208,69,129,42,216,132,128,43,72,132,128,74,184,3,128,75,40,3,128,78,152,2,128,93,48,2,128,15,1,21,76,0,0,17,1,9,41,0,0,1, 
  19,42,0,0,0,136,0,0,0,1,0,17,1,21,76,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,21,76,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,21,76,0,0,1,19,9,0,0,0,35, 
  0,0,0,1,0,17,1,21,76,0,0,1,15,1,21,76,0,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,21,76,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,21,76,0,0, 
  1,19,9,0,0,0,31,0,0,0,1,0,17,1,21,76,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,21,76,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,21,76,0,0,1,19,9,0,0,0, 
  28,0,0,0,1,0,17,1,21,76,0,0,1,15,1,21,76,0,0,17,1,55,77,0,0,1,2,21,7,37,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,43,0,0,0,137,0,0,0,6, 
  0,14,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,73,0,0,0,240,200,1,0,4,0,0,0,2,0,0,0,84,24,1,128,1,16,129,128,2,16, 
  1,128,57,176,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,194,77,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,194,77,0,0,1,2,21,0,55,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,46,88,129,128,93,240,0,128,74,248,0,128,1,19,93,0,0,0,2,1,0,0,2,0,1,19,93,0,0,0,1,1,0,0,2,0,1,2,21,0,81,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0, 
  80,0,3,128,42,160,133,129,34,216,8,128,35,72,8,128,36,184,7,128,37,40,7,128,38,152,6,128,74,128,4,128,40,8,198,129,9,248,9,128,10,104,201,125,43,16,69,128,75,240,3,128,93,152,2,128,78,96,3,128, 
  120,144,2,128,1,15,1,250,77,0,0,17,1,9,41,0,0,1,19,120,0,0,0,87,1,0,0,2,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,250,77,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17, 
  1,250,77,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,250,77,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,250,77,0,0,1,15,1,250,77,0,0,17,1,217,50,0,0,1,19,9,0,0,0, 
  33,0,0,0,1,0,17,1,250,77,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,250,77,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,250,77,0,0,1,19,9,0,0,0,30,0,0,0,1,0, 
  17,1,250,77,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,250,77,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,250,77,0,0,1,19,80,0,0,0,222,0,0,0,1,0,17,1,250,77,0,0, 
  1,19,80,0,0,0,223,0,0,0,1,0,17,1,250,77,0,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,120,208,0,128,87,48,1,128,19,87,0,0,0,243,0,0,0,2,0,1,1,2, 
  21,1,69,0,0,0,34,182,1,0,5,0,0,0,2,0,0,0,112,112,1,128,1,32,2,128,2,32,130,128,115,48,1,128,10,176,1,128,4,17,1,86,32,0,0,1,4,17,1,46,80,0,0,1,4,15,1,197,79,0, 
  0,17,1,143,24,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,197,79,0,0,17,1,238,79,0,0,1, 
  1,2,21,1,51,0,0,0,183,186,1,0,4,0,0,0,2,0,0,0,112,16,1,128,1,144,1,128,2,144,1,128,115,80,1,128,4,17,1,48,26,0,0,1,4,17,1,228,25,0,0,1,8,19,116,0,0,0,69,1, 
  0,0,2,0,14,1,21,1,57,0,0,0,155,183,1,0,4,0,0,0,2,0,0,0,10,80,1,128,1,192,1,128,2,192,129,127,115,16,1,128,4,17,1,48,23,0,0,1,4,15,1,205,22,0,0,17,1,88,20,0, 
  0,1,8,19,116,0,0,0,71,1,0,0,2,0,1,21,1,85,0,0,0,154,187,1,0,6,0,0,0,2,0,0,0,124,184,1,128,1,56,194,128,2,56,194,128,123,248,1,128,25,80,1,128,122,64,2,128,4,19,129, 
  0,0,0,98,1,0,0,2,0,1,4,17,1,73,30,0,0,1,4,17,1,246,28,0,0,1,8,4,19,129,0,0,0,99,1,0,0,2,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,1,68,0,0,0,85,201, 
  1,0,4,0,0,0,2,0,0,0,52,176,1,128,1,16,129,128,2,16,1,128,57,24,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,25,81,0,0,1,4,15,1,25,81,0,0,17,1,202,7,0,0,1, 
  2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,70,200,1,128,13,144,194,128,46,40,130,127,91,56,1,128,113,48,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,25,81,0,0,1,19,113, 
  0,0,0,49,1,0,0,2,0,1,15,1,25,81,0,0,17,1,126,81,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,25,81,0,0,1,2,21,1,39,0,0,0,185,201,1,0,3,0,0,0,1,0,0,0, 
  2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,0,94,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,112,192,6,131,113,48,134,131,114, 
  160,5,128,45,208,9,128,86,224,7,128,91,80,7,128,70,0,137,127,135,24,4,128,136,136,3,128,137,32,3,128,138,144,2,128,75,112,136,126,128,16,5,128,13,96,138,125,46,104,9,128,129,128,4,128,19,137,0,0,0, 
  124,1,0,0,1,0,17,1,177,81,0,0,1,15,1,177,81,0,0,17,1,215,83,0,0,1,19,135,0,0,0,121,1,0,0,1,0,17,1,177,81,0,0,1,15,1,177,81,0,0,17,1,118,83,0,0,1,19,138,0, 
  0,0,126,1,0,0,1,0,17,1,177,81,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,177,81,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,177,81,0,0,1,19,86,0,0,0,241,0,0,0, 
  1,0,17,1,177,81,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,177,81,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,177,81,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,177,81, 
  0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,177,81,0,0,1,15,1,177,81,0,0,17,1,67,83,0,0,1,15,1,177,81,0,0,17,1,16,83,0,0,1,19,138,0,0,0,127,1,0,0,1,0,17,1, 
  177,81,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,177,81,0,0,1,2,21,1,39,0,0,0,129,202,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6, 
  0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,1,39,0,0,0,2,203,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120, 
  1,0,0,1,0,1,21,1,85,0,0,0,131,203,1,0,6,0,0,0,2,0,0,0,124,104,2,128,1,96,194,128,2,96,194,128,123,32,2,128,25,184,1,128,122,80,1,128,4,19,129,0,0,0,99,1,0,0,2,0, 
  1,4,19,129,0,0,0,98,1,0,0,2,0,1,4,17,1,246,28,0,0,1,8,4,17,1,73,30,0,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,44,64,65,128,62,208,0,128,4,19,136,0,0,0,123,1,0,0,4,0,14,1,4,17,1,35,84,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21, 
  1,186,0,0,0,234,176,1,0,10,0,0,0,3,0,0,0,56,240,4,130,1,208,65,129,2,208,1,128,91,80,3,128,52,96,5,128,85,192,3,128,57,88,196,128,103,224,2,128,104,112,2,128,121,216,1,128,8,4,19, 
  128,0,0,0,97,1,0,0,1,0,17,1,222,84,0,0,1,4,15,1,222,84,0,0,17,1,80,95,0,0,1,4,15,1,222,84,0,0,17,1,212,80,0,0,1,4,15,1,222,84,0,0,17,1,160,10,0,0,1,4, 
  19,75,0,0,0,215,0,0,0,1,0,17,1,222,84,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,222,84,0,0,1,4,15,1,222,84,0,0,17,1,47,10,0,0,1,4,15,1,222,84,0,0,17,1, 
  202,7,0,0,1,2,21,0,76,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,112,48,6,131,113,160,133,131,114,16,5,128,45,64,9,128,86,80,7,128,91,192,6,128,70,112,136,127,135,136,3,128,136,248,2,128, 
  137,240,2,128,138,144,2,128,75,224,135,126,128,128,4,128,13,208,137,125,46,216,8,128,129,240,3,128,19,137,0,0,0,125,1,0,0,3,0,1,1,19,135,0,0,0,121,1,0,0,1,0,17,1,222,84,0,0,1,15, 
  1,222,84,0,0,17,1,145,86,0,0,1,19,138,0,0,0,126,1,0,0,1,0,17,1,222,84,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,222,84,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17, 
  1,222,84,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,222,84,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,222,84,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,222,84,0,0,1, 
  19,135,0,0,0,122,1,0,0,1,0,17,1,222,84,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,222,84,0,0,1,15,1,222,84,0,0,17,1,94,86,0,0,1,15,1,222,84,0,0,17,1,43,86,0, 
  0,1,19,138,0,0,0,127,1,0,0,1,0,17,1,222,84,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,222,84,0,0,1,2,21,1,39,0,0,0,129,202,1,0,3,0,0,0,1,0,0,0,2,48,129, 
  128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21,1,39,0,0,0,2,203,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,127,248,0,128, 
  8,4,17,1,15,7,0,0,1,19,135,0,0,0,120,1,0,0,1,0,1,21,1,85,0,0,0,131,203,1,0,6,0,0,0,2,0,0,0,124,104,2,128,1,96,194,128,2,96,194,128,123,80,1,128,25,248,1,128,122, 
  144,1,128,4,17,1,246,28,0,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,4,19,129,0,0,0,98,1,0,0,2,0,1,8,4,17,1,73,30,0,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,1, 
  188,0,0,0,248,203,1,0,12,0,0,0,3,0,0,0,104,176,2,128,1,16,2,130,2,16,2,128,91,144,3,128,52,48,5,128,85,0,4,128,6,16,130,128,7,16,194,128,46,160,5,128,57,152,132,128,103,32,3,128, 
  121,24,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,241,87,0,0,1,4,15,1,241,87,0,0,17,1,80,95,0,0,1,4,15,1,241,87,0,0,17,1,212,80,0,0,1,4,15,1,241,87,0,0,17, 
  1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,241,87,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,241,87,0,0,1,4,15,1,241,87,0,0,17,1,202,7,0,0,1,4,17, 
  1,175,87,0,0,1,2,21,1,53,0,0,0,163,204,1,0,5,0,0,0,2,0,0,0,116,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,117,0,0,0,82,1,0,0,4,0,14,1, 
  19,117,0,0,0,83,1,0,0,3,0,14,1,21,0,49,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,112,232,5,131,113,88,5,131,114,200,4,128,75,152,199,129,116,104,4,128,13,248,8,128,46,144,136,128,135, 
  0,3,128,70,40,72,128,86,8,135,128,91,120,6,128,118,96,4,128,128,208,131,128,129,104,3,128,136,112,2,128,19,135,0,0,0,121,1,0,0,1,0,17,1,241,87,0,0,1,15,1,241,87,0,0,17,1,37,91,0, 
  0,1,15,1,241,87,0,0,17,1,137,89,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,241,87,0,0,1,1,19,118,0,0,0,85,1,0,0,2,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,241, 
  87,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,241,87,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,241,87,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,241,87,0,0,1,19,135, 
  0,0,0,122,1,0,0,1,0,17,1,241,87,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,241,87,0,0,1,15,1,241,87,0,0,17,1,86,89,0,0,1,15,1,241,87,0,0,17,1,35,89,0,0,1, 
  19,70,0,0,0,207,0,0,0,1,0,17,1,241,87,0,0,1,2,21,1,39,0,0,0,197,171,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13, 
  0,0,0,41,0,0,0,1,0,1,21,1,39,0,0,0,201,175,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120,1,0,0,1,0, 
  1,21,1,69,0,0,0,134,205,1,0,5,0,0,0,2,0,0,0,112,48,1,128,1,32,2,128,2,32,130,128,115,224,1,128,10,112,1,128,4,17,1,125,90,0,0,1,4,15,1,218,89,0,0,17,1,143,24,0,0, 
  1,4,17,1,106,26,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,218,89,0,0,17,1,3,90,0,0, 
  1,1,2,21,1,51,0,0,0,71,206,1,0,4,0,0,0,2,0,0,0,112,16,1,128,1,144,1,128,2,144,1,128,115,80,1,128,4,17,1,67,90,0,0,1,4,17,1,228,25,0,0,1,8,19,116,0,0,0,69, 
  1,0,0,2,0,14,1,21,1,45,0,0,0,253,206,1,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,115,240,0,128,4,19,116,0,0,0,80,1,0,0,4,0,14,1,8,19,116,0,0,0,74,1,0, 
  0,3,0,14,1,21,1,57,0,0,0,168,207,1,0,4,0,0,0,2,0,0,0,10,88,1,128,1,80,1,128,2,80,129,127,115,16,1,128,4,17,1,48,23,0,0,1,8,4,15,1,194,90,0,0,17,1,88,20,0, 
  0,1,19,116,0,0,0,71,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,194,90,0,0,17,1,235,90,0,0,1,1,2,21,1,45,0,0, 
  0,253,206,1,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,115,240,0,128,4,19,116,0,0,0,77,1,0,0,4,0,14,1,8,19,116,0,0,0,67,1,0,0,3,0,14,1,21,1,85,0,0,0,94, 
  208,1,0,6,0,0,0,2,0,0,0,124,144,1,128,1,160,194,128,2,160,194,128,123,80,1,128,25,208,1,128,122,56,2,128,4,17,1,246,28,0,0,1,4,17,1,73,30,0,0,1,4,19,129,0,0,0,98,1,0, 
  0,2,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,8,19,129,0,0,0,106,1,0,0,1,0,1,21,1,69,0,0,0,134,205,1,0,5,0,0,0,2,0,0,0,112,48,1,128,1,32,2,128,2,32,130,128, 
  115,112,1,128,10,176,1,128,4,17,1,215,91,0,0,1,4,17,1,106,26,0,0,1,4,15,1,218,89,0,0,17,1,143,24,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,1,57,0,0,0,168,207,1, 
  0,4,0,0,0,2,0,0,0,10,16,1,128,1,128,1,128,2,128,129,127,115,136,1,128,4,15,1,194,90,0,0,17,1,88,20,0,0,1,8,4,17,1,48,23,0,0,1,19,116,0,0,0,71,1,0,0,2,0,1, 
  21,1,85,0,0,0,94,208,1,0,6,0,0,0,2,0,0,0,124,96,2,128,1,160,194,128,2,160,194,128,123,32,2,128,25,80,1,128,122,184,1,128,4,19,129,0,0,0,98,1,0,0,2,0,1,4,19,129,0,0, 
  0,99,1,0,0,2,0,1,4,17,1,246,28,0,0,1,4,17,1,73,30,0,0,1,8,19,129,0,0,0,106,1,0,0,1,0,1,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1, 
  48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0, 
  17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,80,95,0,0,1,4,15,1,244,95,0,0,17,1,11,95,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0, 
  0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,244,95,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,244,95,0,0,1,4,15,1,244,95,0,0,17,1,125,92,0,0,1, 
  4,15,1,244,95,0,0,17,1,84,4,0,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2, 
  128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,80,95,0,0,1,4, 
  15,1,84,12,0,0,17,1,11,95,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,49,94,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1, 
  84,12,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,122,3,0,0,1,4,15,1,84,12,0,0,17,1,84,4,0,0,1,2,21,1,217,0,0,0,213, 
  167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121, 
  56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,80,95,0,0,1,4,15,1,84,12,0,0,17,1,11,95,0,0,1,4,19,119,0,0,0,86,1,0, 
  0,1,0,17,1,84,12,0,0,1,4,15,1,84,12,0,0,17,1,49,94,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,84,12,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,84,12,0, 
  0,1,4,15,1,84,12,0,0,17,1,122,3,0,0,1,4,15,1,84,12,0,0,17,1,84,4,0,0,1,2,21,1,68,0,0,0,85,201,1,0,4,0,0,0,2,0,0,0,52,176,1,128,1,16,129,128,2,16,1, 
  128,57,24,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,25,81,0,0,1,4,15,1,25,81,0,0,17,1,122,3,0,0,1,2,21,1,163,0,0,0,54,209,1,0,10,0,0,0,3,0,0,0,82,176, 
  68,130,1,208,193,129,2,208,129,127,107,120,3,128,108,16,3,128,109,168,2,128,110,64,2,128,111,216,1,128,105,72,4,128,106,224,3,128,8,4,19,114,0,0,0,57,1,0,0,2,0,1,4,19,114,0,0,0,55,1, 
  0,0,2,0,1,4,19,114,0,0,0,50,1,0,0,2,0,1,4,19,114,0,0,0,56,1,0,0,2,0,1,4,19,114,0,0,0,52,1,0,0,2,0,1,4,19,114,0,0,0,51,1,0,0,2,0,1,4,19,114, 
  0,0,0,53,1,0,0,2,0,1,4,19,114,0,0,0,54,1,0,0,2,0,1,2,21,0,184,1,0,0,255,255,255,255,21,0,0,0,4,0,0,0,112,240,135,132,113,96,135,132,114,208,6,128,86,112,10,128,68,96, 
  204,130,69,248,203,130,70,144,75,127,87,224,137,130,136,48,3,128,89,120,9,128,90,16,9,128,75,0,75,128,91,128,8,128,13,48,13,128,46,200,12,128,116,64,6,128,117,176,5,128,119,32,197,128,128,144,4,128,129,40, 
  4,128,135,192,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,244,95,0,0,1,15,1,244,95,0,0,17,1,145,101,0,0,1,15,1,244,95,0,0,17,1,146,100,0,0,1,19,112,0,0,0,48,1,0,0,1, 
  0,17,1,244,95,0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,244,95,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,244,95,0,0,1,19,89,0,0,0,247,0,0,0,1,0,17,1,244,95,0, 
  0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,244,95,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,244,95,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,244,95,0,0,1,19,70,0,0, 
  0,208,0,0,0,1,0,17,1,244,95,0,0,1,15,1,244,95,0,0,17,1,81,100,0,0,1,15,1,244,95,0,0,17,1,22,98,0,0,1,19,69,0,0,0,204,0,0,0,1,0,17,1,244,95,0,0,1,19,135, 
  0,0,0,122,1,0,0,1,0,17,1,244,95,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,244,95,0,0,1,15,1,244,95,0,0,17,1,227,97,0,0,1,15,1,244,95,0,0,17,1,118,14,0,0,1, 
  15,1,244,95,0,0,17,1,173,97,0,0,1,15,1,244,95,0,0,17,1,13,14,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,244,95,0,0,1,2,21,1,53,0,0,0,72,179,1,0,5,0,0,0,2, 
  0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,53,56,1,128,8,4,19,70,0,0,0,206,0,0,0,3,0,14,1,2,21,1,39,0,0,0,251,179,1,0,3,0,0,0,1,0,0,0,2,240,0, 
  128,1,240,64,128,127,248,0,128,8,4,17,1,15,7,0,0,1,19,135,0,0,0,120,1,0,0,1,0,1,21,1,217,0,0,0,43,181,1,0,13,0,0,0,3,0,0,0,104,208,3,128,1,80,198,129,2,80,6,128, 
  91,224,5,128,52,88,6,128,45,96,3,129,6,80,6,128,7,80,198,128,57,200,2,129,85,216,4,128,95,48,66,128,103,112,5,128,121,64,4,128,4,19,119,0,0,0,86,1,0,0,1,0,17,1,251,98,0,0,1,4, 
  19,46,0,0,0,143,0,0,0,1,0,17,1,251,98,0,0,1,4,15,1,251,98,0,0,17,1,164,8,0,0,1,4,15,1,251,98,0,0,17,1,80,95,0,0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1, 
  251,98,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,251,98,0,0,1,4,15,1,251,98,0,0,17,1,212,80,0,0,1,4,15,1,251,98,0,0,17,1,160,10,0,0,1,8,4,15,1,251,98,0,0, 
  17,1,202,7,0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,16,199,131,113,144,200,131,114,32,9,128,86,192,3,128,116,48,8,128,117,96,3, 
  128,70,176,73,127,119,72,134,130,136,80,5,128,89,72,5,128,90,72,5,128,75,160,71,128,91,24,10,128,13,184,4,128,46,224,5,128,128,208,2,128,129,80,4,128,135,168,6,128,19,112,0,0,0,48,1,0,0,1,0, 
  17,1,251,98,0,0,1,19,89,0,0,0,250,0,0,0,2,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,251,98,0,0,1,15,1,251,98,0,0,17,1,194,19,0,0,1,19,70,0,0,0,207,0,0,0,1, 
  0,17,1,251,98,0,0,1,1,19,135,0,0,0,121,1,0,0,1,0,17,1,251,98,0,0,1,15,1,251,98,0,0,17,1,13,14,0,0,1,19,90,0,0,0,251,0,0,0,2,0,1,15,1,251,98,0,0,17,1, 
  149,28,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,251,98,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,251,98,0,0,1,19,89,0,0,0,249,0,0,0,2,0,1,19,86,0,0,0,241,0, 
  0,0,1,0,17,1,251,98,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,251,98,0,0,1,15,1,251,98,0,0,17,1,83,33,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,251,98,0,0,1, 
  2,21,1,53,0,0,0,41,189,1,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2,160,129,127,7,160,1,128,61,48,1,128,4,15,1,76,79,0,0,17,1,2,36,0,0,1,8,19,87,0,0,0,244, 
  0,0,0,1,0,1,21,1,69,0,0,0,34,182,1,0,5,0,0,0,2,0,0,0,112,112,1,128,1,32,2,128,2,32,130,128,115,48,1,128,10,176,1,128,4,17,1,86,32,0,0,1,4,17,1,76,101,0,0,1, 
  4,15,1,227,100,0,0,17,1,143,24,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,227,100,0,0,17, 
  1,12,101,0,0,1,1,2,21,1,51,0,0,0,183,186,1,0,4,0,0,0,2,0,0,0,112,88,1,128,1,80,1,128,2,80,1,128,115,16,1,128,4,17,1,228,25,0,0,1,8,4,17,1,48,26,0,0,1,19, 
  116,0,0,0,69,1,0,0,2,0,14,1,21,1,57,0,0,0,155,183,1,0,4,0,0,0,2,0,0,0,10,16,1,128,1,128,1,128,2,128,129,127,115,136,1,128,4,15,1,205,22,0,0,17,1,88,20,0,0,1, 
  8,4,17,1,48,23,0,0,1,19,116,0,0,0,71,1,0,0,2,0,1,21,1,85,0,0,0,154,187,1,0,6,0,0,0,2,0,0,0,124,248,1,128,1,56,194,128,2,56,194,128,123,80,1,128,25,144,1,128,122, 
  64,2,128,4,17,1,246,28,0,0,1,4,19,129,0,0,0,98,1,0,0,2,0,1,4,17,1,73,30,0,0,1,8,4,19,129,0,0,0,99,1,0,0,2,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,0, 
  84,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,70,160,1,128,13,16,2,128,46,168,129,127,91,16,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,242,101,0,0,1,1,15,1,242,101,0,0,17,1,71, 
  102,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,242,101,0,0,1,2,21,1,39,0,0,0,63,211,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0, 
  0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,1,47,0,0,0,147,222,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,61,48,1,128,4,17,1,181,102,0,0,1, 
  8,19,47,0,0,0,151,0,0,0,3,0,1,21,1,65,3,0,0,148,189,1,0,27,0,0,0,4,0,0,0,32,200,87,133,1,240,195,131,2,240,195,131,35,88,21,128,36,136,20,133,37,184,19,133,6,240,3,131,7, 
  240,3,131,40,72,17,128,41,120,16,128,10,104,153,130,43,56,14,128,44,200,12,128,45,248,11,128,30,152,24,128,47,40,203,130,33,248,150,129,34,40,22,128,38,232,82,129,39,24,18,128,42,168,15,128,48,88,10,128,49, 
  136,9,128,54,184,8,128,84,240,6,128,85,40,5,128,95,248,3,128,8,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,121,138,0,0,15,1,143,139,0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19, 
  75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0,149,0,0,0,5,0,1,4,19,74,0,0, 
  0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0,149,0,0,0,5,0,1,4,15,1,97,138,0,0,15, 
  1,109,138,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,86, 
  132,0,0,17,1,186,130,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,240,128,0,0,17,1,209,125,0, 
  0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0,149,0,0,0,5,0,1,4,19,37,0,0,0,118,0,0,0,1,0, 
  19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0,149,0,0,0,5,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,197,125,0,0,17,1,110,125,0,0,1, 
  4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,180,124,0,0,1,4,15,1,97,138,0,0,15, 
  1,109,138,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98, 
  125,0,0,17,1,175,123,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,1,123,0, 
  0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,83,122,0,0,1,4,15,1,97,138,0, 
  0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,97,138,0,0,15,1,109,138,0,0,15,1,240,121,0,0,17,1,192,120,0,0,1,4,15,1,97,138,0,0,15,1,163,116,0,0,17, 
  1,247,105,0,0,1,2,21,1,36,3,0,0,27,194,1,0,26,0,0,0,4,0,0,0,32,24,215,132,1,208,195,131,2,208,195,131,35,168,20,128,36,216,211,132,37,8,211,132,38,56,210,131,39,104,17,128,40,152,144, 
  131,41,200,15,128,42,248,14,128,11,184,216,129,44,104,12,128,45,152,11,128,30,232,23,128,47,200,138,130,33,72,22,129,34,120,21,128,43,176,13,128,48,248,9,128,49,40,9,128,54,88,8,128,56,232,7,128,84,72,6, 
  128,85,168,4,128,95,216,3,128,8,4,15,1,97,114,0,0,15,1,143,139,0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0,19, 
  9,0,0,0,31,0,0,0,1,0,19,81,0,0,0,226,0,0,0,1,0,17,1,97,114,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0, 
  1,0,19,81,0,0,0,226,0,0,0,1,0,17,1,97,114,0,0,1,4,15,1,107,109,0,0,17,1,40,109,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1, 
  4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,86,132,0,0,17,1,186,130,0,0,1,4,15,1,97,114,0,0,15, 
  1,28,109,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0,0, 
  0,31,0,0,0,1,0,19,81,0,0,0,226,0,0,0,1,0,17,1,97,114,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,81,0,0,0,226,0,0,0,1,0,17, 
  1,97,114,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,197,125,0,0,17,1,110,125,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15, 
  1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,180,124,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,97,114,0,0,15,1,28, 
  109,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,175,123,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0, 
  0,17,1,88,123,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,1,123,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1, 
  4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,83,122,0,0,1,4,15,1,97,114,0,0,15,1,28,109,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,97,114,0,0,15, 
  1,28,109,0,0,15,1,240,121,0,0,17,1,192,120,0,0,1,4,19,80,0,0,0,225,0,0,0,2,0,1,2,19,81,0,0,0,226,0,0,0,1,0,1,21,1,66,0,0,0,112,175,1,0,3,0,0,0,1,0, 
  0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,19,46,0,0,0,143,0,0,0,1,0,19,79,0,0,0,221,0,0,0,1,0,19,45,0,0,0,142,0,0,0,2,0,1,2,21,7,47,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,44,56,1,128,125,208,0,128,4,19,10,0,0,0,37,0,0,0,3,0,1,4,17,1,182,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,2,21,1,163,0,0,0,229,195,1,0,6,0,0,0,2,0,0,0,48,120,3,128,1,80,193,128,2,80,1,128,47,72,4,128,49,168,66,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1, 
  0,19,79,0,0,0,221,0,0,0,1,0,15,1,141,113,0,0,15,1,85,114,0,0,17,1,102,110,0,0,1,4,15,1,141,113,0,0,15,1,85,114,0,0,15,1,90,110,0,0,17,1,136,131,0,0,1,4,15,1, 
  141,113,0,0,15,1,85,114,0,0,15,1,90,110,0,0,17,1,186,130,0,0,1,4,15,1,141,113,0,0,15,1,85,114,0,0,15,1,90,110,0,0,17,1,252,128,0,0,1,2,19,44,0,0,0,141,0,0,0,1, 
  0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,171,110,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  44,0,0,0,140,0,0,0,1,0,1,21,1,155,2,0,0,66,198,1,0,25,0,0,0,4,0,0,0,32,40,211,132,1,176,195,131,2,176,195,131,35,72,17,128,36,168,144,132,37,8,144,132,38,104,207,131,39,200,14, 
  128,40,40,14,128,41,136,13,128,10,104,20,130,43,208,11,128,44,184,10,128,45,24,10,128,30,200,19,128,47,120,73,130,33,136,18,129,34,232,17,128,42,232,12,128,48,216,8,128,49,56,8,128,54,152,7,128,84,40,6, 
  128,85,184,4,128,95,184,3,128,8,4,15,1,129,113,0,0,15,1,121,138,0,0,15,1,143,139,0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121, 
  0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,44,0,0,0,138,0,0,0,3,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0, 
  1,0,19,44,0,0,0,138,0,0,0,3,0,1,4,15,1,129,113,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,129,113,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,129,113,0, 
  0,15,1,86,132,0,0,17,1,186,130,0,0,1,4,15,1,129,113,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,129,113,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0, 
  119,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,44,0,0,0,138,0,0,0,3,0,1,4,19,37,0,0,0,118,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,44,0,0,0,138,0,0, 
  0,3,0,1,4,15,1,129,113,0,0,15,1,197,125,0,0,17,1,110,125,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,180, 
  124,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,175, 
  123,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,1,123,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,170, 
  122,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,83,122,0,0,1,4,15,1,129,113,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,129,113,0,0,15,1,240,121,0,0,17,1,192, 
  120,0,0,1,4,15,1,117,113,0,0,17,1,71,113,0,0,1,2,21,1,45,0,0,0,12,199,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,56,248,0,128,8,4,15,1,107,109,0,0,17,1,40, 
  109,0,0,1,2,19,44,0,0,0,139,0,0,0,3,0,1,19,44,0,0,0,138,0,0,0,3,0,1,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,216,66,128,44,72,2,128,10,208,131,128,11, 
  104,131,128,46,184,1,128,79,80,1,128,15,1,141,113,0,0,17,1,57,47,0,0,1,19,79,0,0,0,221,0,0,0,1,0,17,1,141,113,0,0,1,19,11,0,0,0,38,0,0,0,1,0,17,1,141,113,0,0,1, 
  19,44,0,0,0,141,0,0,0,1,0,17,1,141,113,0,0,1,15,1,141,113,0,0,17,1,9,114,0,0,1,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,1,128,125,208,0,128,4, 
  19,10,0,0,0,36,0,0,0,5,0,14,1,4,17,1,107,46,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,11,0,0,0,38,0,0,0,1,0, 
  1,21,0,81,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,80,200,4,128,81,96,5,128,34,232,6,128,35,176,2,128,36,200,5,128,37,104,9,128,38,88,6,128,42,96,68,129,40,208,4,128,9,216,8,128,10, 
  200,68,127,43,72,8,129,74,208,3,128,45,224,199,128,78,248,9,128,75,64,3,128,93,120,7,128,19,9,0,0,0,29,0,0,0,1,0,17,1,97,114,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,97,114, 
  0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,97,114,0,0,1,15,1,97,114,0,0,17,1,217,50,0,0,1,1,19,9,0,0,0,33,0,0,0,1,0,17,1,97,114,0,0,1,15,1,97,114,0,0,17, 
  1,191,41,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,97,114,0,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,97,114,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,97,114,0,0,1, 
  15,1,97,114,0,0,17,1,9,41,0,0,1,15,1,97,114,0,0,17,1,179,115,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,97,114,0,0,1,19,81,0,0,0,226,0,0,0,1,0,17,1,97,114,0, 
  0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,97,114,0,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,97,114,0,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,1, 
  128,125,208,0,128,4,19,10,0,0,0,37,0,0,0,3,0,14,1,4,17,1,255,115,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,163,0,0, 
  0,229,195,1,0,6,0,0,0,2,0,0,0,48,120,3,128,1,80,193,128,2,80,1,128,47,72,4,128,49,168,66,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,19,79,0,0,0,221,0,0,0,1, 
  0,15,1,162,45,0,0,15,1,85,114,0,0,17,1,102,110,0,0,1,4,15,1,162,45,0,0,15,1,85,114,0,0,15,1,90,110,0,0,17,1,136,131,0,0,1,4,15,1,162,45,0,0,15,1,85,114,0,0,15, 
  1,90,110,0,0,17,1,186,130,0,0,1,4,15,1,162,45,0,0,15,1,85,114,0,0,15,1,90,110,0,0,17,1,252,128,0,0,1,2,21,0,65,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,136,69, 
  131,9,120,9,128,10,232,72,129,35,200,135,129,36,56,7,128,37,168,70,130,38,24,134,129,34,88,72,128,42,32,133,128,43,144,132,128,74,0,4,128,75,112,3,128,78,224,2,128,80,216,2,128,93,112,2,128,15,1,163, 
  116,0,0,17,1,155,139,0,0,1,1,19,42,0,0,0,136,0,0,0,1,0,17,1,163,116,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,163,116,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1, 
  163,116,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,163,116,0,0,1,15,1,163,116,0,0,17,1,229,117,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,163,116,0,0,1,19,9,0,0,0,32, 
  0,0,0,1,0,17,1,163,116,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,163,116,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,163,116,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17, 
  1,163,116,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,163,116,0,0,1,19,80,0,0,0,222,0,0,0,1,0,17,1,163,116,0,0,1,19,80,0,0,0,223,0,0,0,1,0,17,1,163,116,0,0,1, 
  2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,17,1,42,118,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,9, 
  0,0,0,34,0,0,0,1,0,1,21,1,137,2,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,8,147,132,1,144,195,131,2,144,195,131,35,40,17,128,36,136,80,132,37,232,79,132,38,72,143,131,39,168,14,128, 
  40,8,14,128,41,104,13,128,42,200,12,128,43,176,11,128,44,152,10,128,45,248,9,128,30,168,19,128,47,88,9,130,33,104,210,128,34,200,17,128,48,184,8,128,49,24,8,128,54,120,7,128,84,8,6,128,85,152,4,128, 
  95,152,3,128,8,4,15,1,180,120,0,0,15,1,121,138,0,0,15,1,143,139,0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0, 
  19,9,0,0,0,31,0,0,0,1,0,19,42,0,0,0,135,0,0,0,3,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,42,0, 
  0,0,135,0,0,0,3,0,1,4,15,1,180,120,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,180,120,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,180,120,0,0,15,1,86,132, 
  0,0,17,1,186,130,0,0,1,4,15,1,180,120,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,180,120,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1, 
  0,19,9,0,0,0,31,0,0,0,1,0,19,42,0,0,0,135,0,0,0,3,0,1,4,19,37,0,0,0,118,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,42,0,0,0,135,0,0,0,3,0,1,4, 
  15,1,180,120,0,0,15,1,197,125,0,0,17,1,110,125,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,180,124,0,0,1,4, 
  15,1,180,120,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,175,123,0,0,1,4, 
  15,1,180,120,0,0,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,1,123,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1,4, 
  15,1,180,120,0,0,15,1,98,125,0,0,17,1,83,122,0,0,1,4,15,1,180,120,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,180,120,0,0,15,1,240,121,0,0,17,1,192,120,0,0,1,2, 
  19,42,0,0,0,135,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,228,121,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,95,0,0,0,1,0,1,21,1,129,0,0,0,115,199,1,0,6,0,0,0,2,0,0,0,48,200,2,128,1,80,193,128,2,80,1,128,47,104,131, 
  128,49,40,2,128,95,88,1,128,8,4,15,1,153,121,0,0,15,1,216,121,0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,15,1,153,121,0,0,15,1,141,121,0,0,17,1,136,131,0,0,1,4,15,1,153, 
  121,0,0,15,1,141,121,0,0,17,1,186,130,0,0,1,4,15,1,153,121,0,0,15,1,141,121,0,0,17,1,252,128,0,0,1,2,19,92,0,0,0,0,1,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,41,176,0,128,4,19,73,0,0,0,213,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,92,0,0,0,255,0,0, 
  0,1,0,1,19,34,0,0,0,94,0,0,0,2,0,1,19,9,0,0,0,28,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,71,122,0,0,17,1,11, 
  121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,105,0,0,0,1,0,1,19,35,0,0,0,104,0,0,0,2,0,1,21,7,36,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,158,122,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35, 
  0,0,0,97,0,0,0,1,0,1,19,35,0,0,0,96,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,245,122,0,0,17,1,11,121,0,0,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,113,0,0,0,1,0,1,19,35,0,0,0,112,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,40,176,0,128,4,15,1,76,123,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,109,0,0, 
  0,1,0,1,19,35,0,0,0,108,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,163,123,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,101,0,0,0,1,0,1,19,35,0,0,0,100,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,40,176,0,128,4,15,1,250,123,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,115,0,0,0,1,0,1,19,35, 
  0,0,0,114,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,81,124,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,111,0,0,0,1,0,1,19,35,0,0,0,110,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4, 
  15,1,168,124,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,103,0,0,0,1,0,1,19,35,0,0,0,102,0,0, 
  0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,255,124,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,19,35,0,0,0,107,0,0,0,1,0,1,19,35,0,0,0,106,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,86,125,0,0, 
  17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,99,0,0,0,1,0,1,19,35,0,0,0,98,0,0,0,2,0,1,19,9, 
  0,0,0,29,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,185,125,0,0,17,1,11,121,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,36,0,0,0,117,0,0,0,1,0,1,19,36,0,0,0,116,0,0,0,2,0,1,19,9,0,0,0,30,0,0,0,1,0,1,21,1,18,3,0,0,241,199,1,0,25, 
  0,0,0,4,0,0,0,32,240,214,132,1,176,195,131,2,176,195,131,35,128,20,128,36,176,147,132,37,224,146,132,38,16,210,131,39,64,17,128,40,112,16,128,41,160,15,128,42,208,14,128,43,136,13,128,44,64,12,128,45, 
  112,11,128,30,192,23,129,47,56,74,130,33,32,22,129,34,80,21,128,46,8,11,128,48,104,9,128,49,152,8,128,54,200,7,128,84,40,6,128,85,136,4,128,95,184,3,128,8,4,15,1,3,64,0,0,15,1,143,139,0, 
  0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,39,0,0,0,124,0,0,0,1,0,17, 
  1,3,64,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,39,0,0,0,124,0,0,0,1,0,17,1,3,64,0,0,1,4,15, 
  1,3,64,0,0,15,1,228,128,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,3,64,0,0,15,1,228, 
  128,0,0,15,1,86,132,0,0,17,1,186,130,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,19,38,0,0,0,123,0,0,0,2,0,1,4,15,1,3,64, 
  0,0,15,1,228,128,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,39,0,0,0,124,0,0,0,1,0,17,1,3,64,0, 
  0,1,4,19,37,0,0,0,118,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,39,0,0,0,124,0,0,0,1,0,17,1,3,64,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,197,125, 
  0,0,17,1,110,125,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,180,124,0,0, 
  1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,3,64,0,0, 
  15,1,228,128,0,0,15,1,98,125,0,0,17,1,175,123,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1, 
  98,125,0,0,17,1,1,123,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,83,122, 
  0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,3,64,0,0,15,1,228,128,0,0,15,1,240,121,0,0,17,1,192,120,0,0,1,2,19,39,0,0, 
  0,124,0,0,0,1,0,1,19,9,0,0,0,32,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,6,17,1,65,129,0,0,1,21,9,27,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,19,40,0,0,0,129,0,0,0,1,0,1,21,1,60,0,0,0,178,223,1,0,5,0,0,0,2,0,0,0,56,48,1,129,1,152,129,128,2,152,1,128,57, 
  48,1,128,84,160,1,128,12,19,40,0,0,0,129,0,0,0,1,0,1,10,12,17,1,126,129,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,174,130,0,0,17, 
  1,190,129,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,50,0,0,0,187,200,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128, 
  84,248,0,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,241,129,0,0,1,2,21,7,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,56,65,128,62,208,0,128,4,19,76,0,0,0,217,0,0, 
  0,3,0,1,4,17,1,60,130,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,50,0,0,0,187,200,1,0,3,0,0,0,1,0,0,0,2,240, 
  128,128,1,240,0,128,84,248,0,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,111,130,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,19,76,0,0,0,216, 
  0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,40,0,0,0,126,0,0,0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,60,176,0,128,6,17,1,255,130,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,19,40,0,0,0,130,0,0,0,1,0,1,21,1,60,0,0, 
  0,178,223,1,0,5,0,0,0,2,0,0,0,56,48,1,129,1,152,129,128,2,152,1,128,57,48,1,128,84,160,1,128,12,19,40,0,0,0,130,0,0,0,1,0,1,10,12,17,1,60,131,0,0,1,2,21,7,36,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,124,131,0,0,17,1,190,129,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2, 
  19,40,0,0,0,127,0,0,0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,6,17,1,205,131,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,208,0,128,10,19,40,0,0,0,131,0,0,0,1,0,1,21,1,60,0,0,0,178,223,1,0,5,0,0,0,2,0,0,0,56,120,1,129,1,48,129,128,2,48,1,128,57,120,1,128,84,56,1,128,10, 
  12,17,1,10,132,0,0,1,12,19,40,0,0,0,131,0,0,0,1,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,74,132,0,0,17,1,190,129,0,0,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,40,0,0,0,128,0,0,0,2,0,1,19,9,0,0,0,33,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,40,176,0,128,4,17,1,156,132,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,117,2,0,0,38,195,1,0,24,0, 
  0,0,4,0,0,0,32,104,146,132,1,144,195,131,2,144,195,131,35,136,16,128,36,232,79,132,37,72,79,132,38,168,142,131,39,8,14,128,40,104,13,128,41,200,12,128,42,40,12,128,43,56,11,128,44,72,10,128,45,168, 
  9,128,30,8,19,128,47,8,9,130,33,200,209,128,34,40,17,128,48,104,8,128,49,200,7,128,54,40,7,128,84,224,5,128,85,152,4,128,95,152,3,128,8,4,15,1,18,135,0,0,15,1,121,138,0,0,15,1,143,139, 
  0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,17,1,18,135,0,0,1,4,19,74,0, 
  0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,17,1,18,135,0,0,1,4,15,1,18,135,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1, 
  18,135,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,18,135,0,0,15,1,86,132,0,0,17,1,186,130,0,0,1,4,15,1,18,135,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1, 
  18,135,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,17,1,18,135,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,19, 
  9,0,0,0,31,0,0,0,1,0,17,1,18,135,0,0,1,4,15,1,18,135,0,0,15,1,197,125,0,0,17,1,110,125,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1, 
  18,135,0,0,15,1,98,125,0,0,17,1,180,124,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1, 
  18,135,0,0,15,1,98,125,0,0,17,1,175,123,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,1,123,0,0,1,4,15,1, 
  18,135,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,83,122,0,0,1,4,15,1,18,135,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1, 
  18,135,0,0,15,1,240,121,0,0,17,1,192,120,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,76,135,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,117,2,0,0,38,195,1,0,24,0,0,0,4,0,0,0,32,104,146,132,1,144,195,131,2,144,195,131,35,136,16,128,36,232,79,132,37,72,79,132,38,168, 
  142,131,39,8,14,128,40,104,13,128,41,200,12,128,42,40,12,128,43,56,11,128,44,72,10,128,45,168,9,128,30,8,19,128,47,8,9,130,33,200,209,128,34,40,17,128,48,104,8,128,49,200,7,128,54,40,7,128,84,224, 
  5,128,85,152,4,128,95,152,3,128,8,4,15,1,194,137,0,0,15,1,121,138,0,0,15,1,143,139,0,0,15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0, 
  121,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,17,1,194,137,0,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,17,1, 
  194,137,0,0,1,4,15,1,194,137,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,194,137,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,194,137,0,0,15,1,86,132,0,0,17,1, 
  186,130,0,0,1,4,15,1,194,137,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,194,137,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0, 
  0,0,31,0,0,0,1,0,17,1,194,137,0,0,1,4,19,37,0,0,0,118,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,17,1,194,137,0,0,1,4,15,1,194,137,0,0,15,1,197,125,0,0,17,1, 
  110,125,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,180,124,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1, 
  93,124,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,175,123,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1, 
  88,123,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,1,123,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1, 
  83,122,0,0,1,4,15,1,194,137,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,194,137,0,0,15,1,240,121,0,0,17,1,192,120,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,41,176,0,128,4,19,43,0,0,0,137,0,0,0,6,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,9,0,0,0,35,0,0,0,1,0, 
  1,21,1,83,0,0,0,240,200,1,0,4,0,0,0,2,0,0,0,84,24,1,128,1,16,129,128,2,16,1,128,57,216,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,93,0,0,0,2,1,0,0,2,0,1, 
  4,19,46,0,0,0,143,0,0,0,1,0,19,93,0,0,0,1,1,0,0,2,0,1,2,19,47,0,0,0,149,0,0,0,5,0,1,19,80,0,0,0,223,0,0,0,1,0,1,21,0,21,1,0,0,255,255,255,255,13, 
  0,0,0,3,0,0,0,40,64,5,128,9,160,8,128,34,16,72,129,35,128,71,129,36,240,6,128,37,96,198,129,38,208,69,129,42,216,132,128,43,72,132,128,74,184,3,128,75,40,3,128,78,152,2,128,93,48,2,128,15, 
  1,121,138,0,0,17,1,155,139,0,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,121,138,0,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,121,138,0,0,1,19,37,0,0,0,120,0,0,0,1,0,17, 
  1,121,138,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,121,138,0,0,1,15,1,121,138,0,0,17,1,229,117,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,121,138,0,0,1,19,9,0,0,0, 
  32,0,0,0,1,0,17,1,121,138,0,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,121,138,0,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,121,138,0,0,1,19,9,0,0,0,29,0,0,0,1,0, 
  17,1,121,138,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,121,138,0,0,1,1,2,19,42,0,0,0,136,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0, 
  128,4,17,1,224,139,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,78,0,0,0,219,0,0,0,1,0,1,21,1,66,0,0,0,112,175,1,0,3,0, 
  0,0,1,0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,19,46,0,0,0,143,0,0,0,1,0,19,79,0,0,0,221,0,0,0,1,0,19,78,0,0,0,220,0,0,0,3,0,1,2,21,1,47,0,0, 
  0,116,224,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,17,1,83,140,0,0,1,2,21,1,105,0,0,0,231,224,1,0,7,0,0,0,2,0,0, 
  0,52,216,130,129,1,112,1,129,2,112,129,128,7,112,1,128,6,112,1,128,57,16,2,128,84,120,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,70,144,0,0,1,4,19,46,0,0,0,143,0,0,0,1, 
  0,15,1,189,140,0,0,17,1,242,101,0,0,1,4,15,1,189,140,0,0,17,1,122,3,0,0,1,2,21,1,47,0,0,0,147,222,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7, 
  112,1,128,61,48,1,128,4,17,1,248,140,0,0,1,8,19,47,0,0,0,150,0,0,0,5,0,1,21,1,65,3,0,0,148,189,1,0,27,0,0,0,4,0,0,0,32,200,87,133,1,240,195,131,2,240,195,131,35,88, 
  21,128,36,136,20,133,37,184,19,133,6,240,3,131,7,240,3,131,40,72,17,128,41,120,16,128,10,104,153,130,43,56,14,128,44,200,12,128,45,248,11,128,30,152,24,128,47,40,203,130,33,248,150,129,34,40,22,128,38,232, 
  82,129,39,24,18,128,42,168,15,128,48,88,10,128,49,136,9,128,54,184,8,128,84,240,6,128,85,40,5,128,95,248,3,128,8,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,121,138,0,0,15,1,143,139,0,0, 
  15,1,155,139,0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47, 
  0,0,0,148,0,0,0,7,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0,0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0, 
  148,0,0,0,7,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,86,132,0,0,17,1,136,131,0,0,1,4, 
  15,1,58,144,0,0,15,1,109,138,0,0,15,1,86,132,0,0,17,1,186,130,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,58,144,0,0,15,1, 
  109,138,0,0,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0,148,0,0,0, 
  7,0,1,4,19,37,0,0,0,118,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,19,80,0,0,0,223,0,0,0,1,0,19,47,0,0,0,148,0,0,0,7,0,1,4,15,1,58,144,0,0,15,1,109,138, 
  0,0,15,1,197,125,0,0,17,1,110,125,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0, 
  17,1,180,124,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,6,124,0,0,1,4, 
  15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,175,123,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,58,144,0,0,15,1, 
  109,138,0,0,15,1,98,125,0,0,17,1,1,123,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,170,122,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125, 
  0,0,17,1,83,122,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,58,144,0,0,15,1,109,138,0,0,15,1,240,121,0,0,17,1,192,120,0,0, 
  1,4,15,1,58,144,0,0,15,1,163,116,0,0,17,1,247,105,0,0,1,2,19,47,0,0,0,148,0,0,0,7,0,1,21,1,75,0,0,0,99,225,1,0,6,0,0,0,2,0,0,0,60,24,66,129,1,80,1,128, 
  2,80,129,128,7,80,1,128,6,80,1,128,84,88,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,47,0,0,0,146,0,0,0,6,0,1,4,17,1,146,144,0,0,1,2,21,1,47,0,0,0,32,226,1,0, 
  5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,59,56,1,128,8,4,17,1,194,144,0,0,1,2,21,1,63,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128, 
  1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,47,0,0,0,144,0,0,0,8,0,1,2,21,1,75,0,0,0,99,225,1,0,6,0,0,0,2,0,0,0, 
  60,24,66,129,1,80,1,128,2,80,129,128,7,80,1,128,6,80,1,128,84,88,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,47,0,0,0,147,0,0,0,4,0,1,4,17,1,78,145,0,0,1,2,21,1, 
  47,0,0,0,32,226,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,59,56,1,128,8,4,17,1,126,145,0,0,1,2,21,1,63,0,0,0,134,164,1,0,5,0,0,0, 
  2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,47,0,0,0,145,0,0,0,6,0,1,2,21,1,63,0,0,0,134,164,1,0, 
  5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,47,0,0,0,152,0,0,0,2,0,1,2,21,1,63,0,0,0, 
  134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,47,0,0,0,153,0,0,0,2,0,1,2,21,0, 
  77,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,83,232,134,130,84,128,6,128,82,120,7,128,19,248,73,127,52,0,73,127,53,112,200,128,54,8,8,128,103,64,4,128,85,240,5,128,94,96,5,128,99,208,4,128, 
  107,176,3,128,108,32,3,128,109,144,2,128,14,96,202,126,47,104,9,128,19,83,0,0,0,233,0,0,0,1,0,17,1,62,146,0,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,62,146,0,0,1,19,84,0,0, 
  0,234,0,0,0,1,0,17,1,62,146,0,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,62,146,0,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,62,146,0,0,1,19,82,0,0,0,228,0,0,0,1, 
  0,17,1,62,146,0,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,62,146,0,0,1,15,1,62,146,0,0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,62,146,0,0,1,19,53,0, 
  0,0,160,0,0,0,1,0,17,1,62,146,0,0,1,15,1,62,146,0,0,17,1,184,150,0,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,62,146,0,0,1,15,1,62,146,0,0,17,1,168,160,0,0,1,19, 
  19,0,0,0,62,0,0,0,1,0,17,1,62,146,0,0,1,15,1,62,146,0,0,17,1,140,147,0,0,1,1,2,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48, 
  129,127,7,48,1,128,13,56,1,128,8,4,17,1,199,147,0,0,1,19,14,0,0,0,53,0,0,0,1,0,1,21,1,122,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,56,5,128,1,144,130,128,2,144,66, 
  130,65,48,10,128,100,56,4,128,69,80,201,129,6,144,194,129,7,144,2,128,88,56,8,128,89,32,7,128,90,8,6,128,66,192,9,128,101,104,3,128,102,152,2,128,62,96,11,128,63,240,10,128,8,4,15,1,172,150,0, 
  0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,172,150,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,172,150,0,0,15,1,25,195,0,0,15, 
  1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,172,150,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0, 
  0,162,0,0,0,1,0,19,14,0,0,0,51,0,0,0,3,0,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,51,0,0,0,3,0,1,4,19,85,0,0,0, 
  236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,51,0,0,0,3,0,1,4,15,1,78,149,0,0,17,1,104,160,0,0,1,4,15,1,66,149,0,0,17,1,254,145,0,0,1,4,19,47, 
  0,0,0,154,0,0,0,1,0,19,19,0,0,0,63,0,0,0,3,0,1,4,15,1,66,149,0,0,17,1,190,145,0,0,1,4,15,1,66,149,0,0,17,1,201,2,0,0,1,2,19,19,0,0,0,63,0,0,0,3, 
  0,1,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,137,149,0,0,1,8,19,14,0,0,0,52,0,0,0,3,0, 
  1,21,1,22,1,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,152,71,130,1,240,129,128,2,240,129,129,89,128,6,128,100,152,3,128,101,200,2,128,6,240,1,129,7,240,1,128,90,104,5,128,96,152,4,128,102, 
  248,1,128,8,4,15,1,160,150,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,160,150,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,160, 
  150,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,160,150,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0, 
  238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,50,0,0,0,5,0,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,50,0,0, 
  0,5,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,50,0,0,0,5,0,1,2,19,14,0,0,0,50,0,0,0,5,0,1,19,14,0,0,0,51,0,0, 
  0,3,0,1,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,243,150,0,0,1,8,19,14,0,0,0,49,0,0,0, 
  1,0,1,21,1,146,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,56,5,128,1,144,130,128,2,144,66,130,65,96,10,128,100,56,4,128,69,80,201,129,6,144,194,129,7,144,2,128,88,56,8,128,89,32,7, 
  128,90,8,6,128,66,192,9,128,101,104,3,128,102,152,2,128,62,240,11,128,63,80,11,128,8,4,15,1,192,159,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,192,159,0,0,15, 
  1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,192,159,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,192,159,0,0,15,1,57, 
  191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,45,0,0,0,3,0,1,4,19,85,0,0,0,237,0,0, 
  0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,45,0,0,0,3,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,45,0,0,0,3,0, 
  1,4,15,1,98,158,0,0,17,1,104,160,0,0,1,4,15,1,134,152,0,0,15,1,86,158,0,0,17,1,254,145,0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,15,0,0,0,57,0,0,0,1,0,17,1, 
  134,152,0,0,1,4,15,1,134,152,0,0,15,1,86,158,0,0,17,1,190,145,0,0,1,4,15,1,134,152,0,0,15,1,86,158,0,0,17,1,201,2,0,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0, 
  3,0,0,0,47,176,200,130,84,96,6,131,82,88,7,128,83,200,198,129,52,72,72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128, 
  109,112,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,134,152,0,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,134,152,0,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,134,152,0,0,1,19,82, 
  0,0,0,230,0,0,0,1,0,17,1,134,152,0,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,134,152,0,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,134,152,0,0,1,19,53,0,0,0,162,0,0, 
  0,1,0,17,1,134,152,0,0,1,15,1,134,152,0,0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,134,152,0,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,134,152,0,0,1,19, 
  14,0,0,0,45,0,0,0,3,0,1,15,1,134,152,0,0,17,1,243,156,0,0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,134,152,0,0,1,15,1,134,152,0,0,17,1,189,153,0,0,1,1,2,21,1,47, 
  0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,249,153,0,0,1,8,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,125, 
  1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,56,5,128,1,144,130,128,2,144,66,130,65,72,10,128,100,56,4,128,69,104,201,129,6,144,194,129,7,144,2,128,88,72,8,128,89,40,7,128,90,8,6,128,66, 
  216,9,128,101,104,3,128,102,152,2,128,62,120,11,128,63,8,11,128,8,4,15,1,230,156,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,230,156,0,0,15,1,25,195,0,0,15, 
  1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,230,156,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,230,156,0,0,15,1,57,191,0,0,15,1,69, 
  191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,44,0,0,0,5,0,14,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53, 
  0,0,0,162,0,0,0,1,0,19,14,0,0,0,44,0,0,0,5,0,14,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,44,0,0,0,5,0,14,1,4,15, 
  1,131,155,0,0,17,1,104,160,0,0,1,4,15,1,119,155,0,0,17,1,254,145,0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,15,0,0,0,58,0,0,0,3,0,1,4,15,1,119,155,0,0,17,1,190, 
  145,0,0,1,4,15,1,119,155,0,0,17,1,201,2,0,0,1,2,19,15,0,0,0,58,0,0,0,3,0,1,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129, 
  127,7,112,1,128,13,48,1,128,4,17,1,191,155,0,0,1,8,19,14,0,0,0,46,0,0,0,5,0,14,1,21,1,25,1,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,168,71,130,1,240,129,128,2,240,129, 
  129,89,136,6,128,100,152,3,128,101,200,2,128,6,240,1,129,7,240,1,128,90,104,5,128,96,152,4,128,102,248,1,128,8,4,15,1,217,156,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1, 
  4,15,1,217,156,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,217,156,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15, 
  1,217,156,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,42,0,0,0,7,0,14,1, 
  4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,42,0,0,0,7,0,14,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19, 
  14,0,0,0,42,0,0,0,7,0,14,1,2,19,14,0,0,0,42,0,0,0,7,0,14,1,19,14,0,0,0,44,0,0,0,5,0,14,1,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,48,1, 
  128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,47,157,0,0,1,19,14,0,0,0,47,0,0,0,3,0,14,1,21,1,25,1,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,168,71, 
  130,1,240,129,128,2,240,129,129,89,136,6,128,100,152,3,128,101,200,2,128,6,240,1,129,7,240,1,128,90,104,5,128,96,152,4,128,102,248,1,128,8,4,15,1,73,158,0,0,15,1,25,195,0,0,15,1,37,195,0, 
  0,17,1,224,192,0,0,1,4,15,1,73,158,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,73,158,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17, 
  1,159,191,0,0,1,4,15,1,73,158,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0, 
  43,0,0,0,5,0,14,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,43,0,0,0,5,0,14,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0, 
  0,162,0,0,0,1,0,19,14,0,0,0,43,0,0,0,5,0,14,1,2,19,14,0,0,0,43,0,0,0,5,0,14,1,19,15,0,0,0,57,0,0,0,1,0,1,21,1,47,0,0,0,209,226,1,0,5,0,0,0, 
  2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,157,158,0,0,1,8,19,14,0,0,0,47,0,0,0,3,0,1,21,1,22,1,0,0,191,228,1,0,11,0,0,0,3, 
  0,0,0,88,152,71,130,1,240,129,128,2,240,129,129,89,128,6,128,100,152,3,128,101,200,2,128,6,240,1,129,7,240,1,128,90,104,5,128,96,152,4,128,102,248,1,128,8,4,15,1,180,159,0,0,15,1,25,195,0, 
  0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,180,159,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,180,159,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15, 
  1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,180,159,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1, 
  0,19,14,0,0,0,43,0,0,0,5,0,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,43,0,0,0,5,0,1,4,19,85,0,0,0,236,0,0,0,1,0, 
  19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,43,0,0,0,5,0,1,2,19,14,0,0,0,43,0,0,0,5,0,1,19,14,0,0,0,45,0,0,0,3,0,1,21,1,47,0,0,0,111,229,1,0,5,0, 
  0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,252,159,0,0,1,2,21,1,71,0,0,0,32,230,1,0,6,0,0,0,2,0,0,0,100,200,1,128,1,80, 
  1,129,2,80,129,128,7,80,1,128,6,80,1,128,101,88,1,128,8,4,15,1,80,160,0,0,17,1,117,192,0,0,1,4,15,1,68,160,0,0,17,1,159,191,0,0,1,2,19,84,0,0,0,235,0,0,0,3,0,1, 
  19,83,0,0,0,231,0,0,0,3,0,1,19,19,0,0,0,62,0,0,0,1,0,1,21,1,63,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48, 
  1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,52,0,0,0,159,0,0,0,2,0,1,2,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112, 
  1,128,13,48,1,128,4,17,1,227,160,0,0,1,8,19,14,0,0,0,56,0,0,0,1,0,1,21,1,22,1,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,152,71,130,1,240,129,128,2,240,129,129,89,128,6, 
  128,100,152,3,128,101,200,2,128,6,240,1,129,7,240,1,128,90,104,5,128,96,152,4,128,102,248,1,128,8,4,15,1,250,161,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,250, 
  161,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,250,161,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,250,161,0, 
  0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,54,0,0,0,3,0,1,4,19,85,0,0, 
  0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,54,0,0,0,3,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,54,0, 
  0,0,3,0,1,2,19,14,0,0,0,54,0,0,0,3,0,1,21,1,81,0,0,0,160,230,1,0,7,0,0,0,2,0,0,0,48,32,130,129,1,112,1,128,2,112,129,128,7,112,129,128,6,112,1,128,71,184,1,128, 
  72,120,1,128,8,4,17,1,88,162,0,0,1,4,19,54,0,0,0,170,0,0,0,2,0,1,4,19,54,0,0,0,169,0,0,0,2,0,1,2,21,1,69,0,0,0,33,232,1,0,6,0,0,0,2,0,0,0,48,192, 
  1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,71,88,1,128,8,4,19,54,0,0,0,168,0,0,0,3,0,1,4,19,54,0,0,0,167,0,0,0,3,0,1,2,21,1,69,0,0,0,33,232,1,0, 
  6,0,0,0,2,0,0,0,48,192,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,71,88,1,128,8,4,19,54,0,0,0,172,0,0,0,2,0,1,4,19,54,0,0,0,171,0,0,0,2,0,1,2, 
  21,1,81,0,0,0,160,230,1,0,7,0,0,0,2,0,0,0,48,32,130,129,1,112,1,128,2,112,129,128,7,112,129,128,6,112,1,128,71,184,1,128,72,120,1,128,8,4,17,1,54,163,0,0,1,4,19,54,0,0, 
  0,166,0,0,0,2,0,1,4,19,54,0,0,0,165,0,0,0,2,0,1,2,21,1,69,0,0,0,33,232,1,0,6,0,0,0,2,0,0,0,48,192,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128, 
  71,88,1,128,8,4,19,54,0,0,0,164,0,0,0,3,0,1,4,19,54,0,0,0,163,0,0,0,3,0,1,2,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48, 
  129,127,7,48,1,128,13,56,1,128,8,4,17,1,183,163,0,0,1,19,14,0,0,0,49,0,0,0,1,0,1,21,1,146,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,56,5,128,1,144,130,128,2,144,66, 
  130,65,96,10,128,100,56,4,128,69,80,201,129,6,144,194,129,7,144,2,128,88,56,8,128,89,32,7,128,90,8,6,128,66,192,9,128,101,104,3,128,102,152,2,128,62,240,11,128,63,80,11,128,8,4,15,1,192,159,0, 
  0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,192,159,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,192,159,0,0,15,1,25,195,0,0,15, 
  1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,192,159,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0, 
  0,162,0,0,0,1,0,19,14,0,0,0,45,0,0,0,3,0,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,45,0,0,0,3,0,1,4,19,85,0,0,0, 
  236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,45,0,0,0,3,0,1,4,15,1,98,158,0,0,17,1,104,160,0,0,1,4,15,1,74,165,0,0,15,1,86,158,0,0,17,1,254,145, 
  0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,15,0,0,0,57,0,0,0,1,0,17,1,74,165,0,0,1,4,15,1,74,165,0,0,15,1,86,158,0,0,17,1,190,145,0,0,1,4,15,1,74,165,0,0, 
  15,1,86,158,0,0,17,1,201,2,0,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,176,200,130,84,96,6,131,82,88,7,128,83,200,198,129,52,72,72,127,53,232,199,128,14,168,201,128, 
  15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,74,165,0,0,1,19,83,0,0,0,232,0,0,0,1, 
  0,17,1,74,165,0,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,74,165,0,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,74,165,0,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,74,165,0, 
  0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,74,165,0,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,74,165,0,0,1,15,1,74,165,0,0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0, 
  1,0,17,1,74,165,0,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,74,165,0,0,1,19,14,0,0,0,45,0,0,0,3,0,1,15,1,74,165,0,0,17,1,129,166,0,0,1,19,15,0,0,0,57,0,0, 
  0,1,0,17,1,74,165,0,0,1,15,1,74,165,0,0,17,1,189,153,0,0,1,1,2,21,1,47,0,0,0,209,226,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13, 
  48,1,128,4,17,1,47,157,0,0,1,8,19,14,0,0,0,47,0,0,0,3,0,14,1,21,1,47,0,0,0,130,232,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,55, 
  56,1,128,8,4,17,1,237,166,0,0,1,2,21,1,82,0,0,0,214,232,1,0,7,0,0,0,2,0,0,0,6,112,65,129,1,112,193,128,2,112,129,127,7,112,193,128,57,248,1,128,98,184,1,128,99,120,1,128,8, 
  4,17,1,99,184,0,0,1,4,17,1,61,179,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,64,167,0,0,1,2,21,1,95,0,0,0,71,239,1,0,7,0,0,0,2,0,0,0,52,232,1,128,1,112, 
  1,128,2,112,129,128,7,112,193,128,6,112,65,128,10,136,2,128,55,120,1,128,8,4,15,1,227,178,0,0,17,1,163,178,0,0,1,4,15,1,139,178,0,0,15,1,151,178,0,0,17,1,213,174,0,0,1,4,15,1, 
  139,178,0,0,17,1,160,167,0,0,1,2,21,1,131,0,0,0,177,239,1,0,8,0,0,0,3,0,0,0,10,120,3,128,1,144,1,128,2,144,129,127,118,152,1,128,52,216,2,128,117,56,2,128,6,144,65,127,7,144, 
  1,128,8,4,15,1,188,173,0,0,15,1,201,174,0,0,17,1,28,173,0,0,1,4,15,1,188,173,0,0,15,1,16,173,0,0,17,1,136,170,0,0,1,4,15,1,188,173,0,0,15,1,124,170,0,0,17,1,213,174, 
  0,0,1,4,15,1,188,173,0,0,15,1,16,173,0,0,17,1,36,168,0,0,1,2,21,1,34,2,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66,130,65,80,14,128,100,248, 
  4,128,69,224,12,131,6,240,130,129,7,240,2,128,88,184,9,128,73,160,11,129,74,0,139,129,66,128,13,128,70,64,140,129,89,112,8,128,62,64,16,128,63,112,15,128,90,40,7,128,101,248,3,128,102,248,2,128,8,4, 
  15,1,71,170,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,71,170,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192, 
  0,0,1,4,15,1,71,170,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,71,170,0,0,15,1,13,195,0,0,15,1,57,191,0,0, 
  15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,71,170,0,0,1,4,19,85,0,0,0, 
  237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,71,170,0,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14, 
  0,0,0,55,0,0,0,1,0,17,1,71,170,0,0,1,4,15,1,71,170,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15,1,71,170,0,0,15,1,124,163,0,0,17,1,158,162,0,0,1,4,15,1,71, 
  170,0,0,15,1,124,163,0,0,17,1,6,162,0,0,1,4,15,1,71,170,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15,1,71,170,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,254,145,0, 
  0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,19,0,0,0,62,0,0,0,1,0,15,1,71,170,0,0,17,1,62,146,0,0,1,4,15,1,71,170,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,190, 
  145,0,0,1,4,15,1,71,170,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,201,2,0,0,1,2,21,1,52,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129, 
  127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,93,1,0,0,3,0,1,2,19,97,0,0,0,9,1,0,0,1,0,1,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48, 
  1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,184,170,0,0,1,2,21,1,34,2,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66,130,65,80,14,128,100,248, 
  4,128,69,224,12,131,6,240,130,129,7,240,2,128,88,184,9,128,73,160,11,129,74,0,139,129,66,128,13,128,70,64,140,129,89,112,8,128,62,64,16,128,63,112,15,128,90,40,7,128,101,248,3,128,102,248,2,128,8,4, 
  15,1,219,172,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,219,172,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192, 
  0,0,1,4,15,1,219,172,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,219,172,0,0,15,1,13,195,0,0,15,1,57,191,0,0, 
  15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,219,172,0,0,1,4,19,85,0,0,0, 
  237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,219,172,0,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14, 
  0,0,0,55,0,0,0,1,0,17,1,219,172,0,0,1,4,15,1,219,172,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15,1,219,172,0,0,15,1,124,163,0,0,17,1,158,162,0,0,1,4,15,1,219, 
  172,0,0,15,1,124,163,0,0,17,1,6,162,0,0,1,4,15,1,219,172,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15,1,219,172,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,254,145,0, 
  0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,19,0,0,0,62,0,0,0,1,0,15,1,219,172,0,0,17,1,62,146,0,0,1,4,15,1,219,172,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,190, 
  145,0,0,1,4,15,1,219,172,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,201,2,0,0,1,2,21,1,52,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129, 
  127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,92,1,0,0,4,0,1,2,19,97,0,0,0,10,1,0,0,1,0,1,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48, 
  1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,76,173,0,0,1,2,21,1,58,0,0,0,128,242,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56, 
  1,128,8,4,19,75,0,0,0,215,0,0,0,1,0,17,1,135,173,0,0,1,2,21,1,52,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1, 
  128,8,4,19,125,0,0,0,94,1,0,0,4,0,1,2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,192,1,128,97,232,130,128,98,224,2,128,121,80,66,128,125,48,1,128,19,97,0,0,0,11, 
  1,0,0,1,0,17,1,188,173,0,0,1,19,97,0,0,0,10,1,0,0,1,0,17,1,188,173,0,0,1,19,97,0,0,0,9,1,0,0,1,0,17,1,188,173,0,0,1,1,15,1,188,173,0,0,17,1,39,174,0, 
  0,1,2,21,1,125,0,0,0,213,242,1,0,9,0,0,0,3,0,0,0,10,120,3,128,1,176,1,128,2,176,129,127,11,8,3,128,52,152,2,128,117,40,2,128,6,176,129,128,7,176,1,128,118,184,1,128,8,4,15, 
  1,189,174,0,0,17,1,28,173,0,0,1,4,15,1,177,174,0,0,17,1,136,170,0,0,1,4,15,1,165,174,0,0,17,1,213,174,0,0,1,4,19,98,0,0,0,16,1,0,0,3,0,14,1,4,15,1,177,174,0, 
  0,17,1,36,168,0,0,1,2,19,97,0,0,0,12,1,0,0,2,0,1,19,97,0,0,0,13,1,0,0,2,0,1,19,97,0,0,0,14,1,0,0,2,0,1,19,97,0,0,0,11,1,0,0,1,0,1,21,1,80, 
  0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,122,0,0,0,89,1,0,0,1,0,19, 
  123,0,0,0,90,1,0,0,1,0,17,1,38,175,0,0,1,2,21,0,84,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,122,120,1,128,121,8,2,128,74,16,130,127,123,16,1,128,15,1,38,175,0,0,17,1, 
  123,175,0,0,1,19,123,0,0,0,90,1,0,0,1,0,17,1,38,175,0,0,1,1,19,122,0,0,0,89,1,0,0,1,0,17,1,38,175,0,0,1,2,21,1,59,0,0,0,156,179,1,0,6,0,0,0,2,0,0, 
  0,6,80,1,128,1,80,193,128,2,80,129,127,7,80,1,128,53,152,65,128,81,88,1,128,8,4,17,1,64,178,0,0,1,4,17,1,183,175,0,0,1,2,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0, 
  0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,231,175,0,0,1,2,21,1,34,2,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66, 
  130,65,80,14,128,100,248,4,128,69,224,12,131,6,240,130,129,7,240,2,128,88,184,9,128,73,160,11,129,74,0,139,129,66,128,13,128,70,64,140,129,89,112,8,128,62,64,16,128,63,112,15,128,90,40,7,128,101,248,3, 
  128,102,248,2,128,8,4,15,1,10,178,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,10,178,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212, 
  192,0,0,17,1,117,192,0,0,1,4,15,1,10,178,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,10,178,0,0,15,1,13,195,0, 
  0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,10,178,0,0, 
  1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,10,178,0,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162, 
  0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,10,178,0,0,1,4,15,1,10,178,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15,1,10,178,0,0,15,1,124,163,0,0,17,1,158,162, 
  0,0,1,4,15,1,10,178,0,0,15,1,124,163,0,0,17,1,6,162,0,0,1,4,15,1,10,178,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15,1,10,178,0,0,15,1,62,146,0,0,15,1,92,160, 
  0,0,17,1,254,145,0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,19,0,0,0,62,0,0,0,1,0,15,1,10,178,0,0,17,1,62,146,0,0,1,4,15,1,10,178,0,0,15,1,62,146,0,0,15,1, 
  92,160,0,0,17,1,190,145,0,0,1,4,15,1,10,178,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,201,2,0,0,1,2,21,1,53,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128, 
  1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,121,0,0,0,88,1,0,0,6,0,14,1,2,21,1,74,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48, 
  129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,19,122,0,0,0,89,1,0,0,1,0,19,123,0,0,0,91,1,0,0,3,0,1,2,19,94,0,0,0,4,1,0,0,4,0,1,19, 
  98,0,0,0,15,1,0,0,1,0,1,21,1,63,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0, 
  1,0,19,95,0,0,0,5,1,0,0,2,0,1,2,21,1,77,0,0,0,87,243,1,0,6,0,0,0,2,0,0,0,52,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,10,248,1,128,8,4,15, 
  1,49,179,0,0,15,1,151,178,0,0,17,1,213,174,0,0,1,4,15,1,49,179,0,0,17,1,160,167,0,0,1,2,19,94,0,0,0,3,1,0,0,5,0,1,21,1,77,0,0,0,87,243,1,0,6,0,0,0,2, 
  0,0,0,52,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,10,248,1,128,8,4,15,1,75,184,0,0,15,1,87,184,0,0,17,1,64,181,0,0,1,4,15,1,75,184,0,0,17,1,139,179,0, 
  0,1,2,21,1,131,0,0,0,177,239,1,0,8,0,0,0,3,0,0,0,10,120,3,128,1,144,1,128,2,144,129,127,118,152,1,128,52,216,2,128,117,56,2,128,6,144,65,127,7,144,1,128,8,4,15,1,39,180,0, 
  0,15,1,52,181,0,0,17,1,28,173,0,0,1,4,15,1,39,180,0,0,15,1,27,180,0,0,17,1,136,170,0,0,1,4,15,1,39,180,0,0,15,1,15,180,0,0,17,1,64,181,0,0,1,4,15,1,39,180,0, 
  0,15,1,27,180,0,0,17,1,36,168,0,0,1,2,19,101,0,0,0,21,1,0,0,1,0,1,19,101,0,0,0,23,1,0,0,1,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2, 
  128,101,232,130,128,102,224,130,128,125,192,1,128,126,48,1,128,19,101,0,0,0,21,1,0,0,1,0,17,1,39,180,0,0,1,19,101,0,0,0,22,1,0,0,1,0,17,1,39,180,0,0,1,19,101,0,0,0,23,1, 
  0,0,1,0,17,1,39,180,0,0,1,1,15,1,39,180,0,0,17,1,146,180,0,0,1,2,21,1,125,0,0,0,213,242,1,0,9,0,0,0,3,0,0,0,10,120,3,128,1,176,1,128,2,176,129,127,11,8,3,128, 
  52,152,2,128,117,40,2,128,6,176,129,128,7,176,1,128,118,184,1,128,8,4,15,1,40,181,0,0,17,1,28,173,0,0,1,4,15,1,28,181,0,0,17,1,136,170,0,0,1,4,15,1,16,181,0,0,17,1,64,181, 
  0,0,1,4,19,102,0,0,0,28,1,0,0,3,0,14,1,4,15,1,28,181,0,0,17,1,36,168,0,0,1,2,19,101,0,0,0,24,1,0,0,2,0,1,19,101,0,0,0,26,1,0,0,2,0,1,19,101,0,0, 
  0,25,1,0,0,2,0,1,19,101,0,0,0,22,1,0,0,1,0,1,21,1,82,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,32,2,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88, 
  1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,15,1,147,181,0,0,17,1,242,101,0,0,1,4,15,1,147,181,0,0,17,1,122,3,0,0,1,2,21,1,47,0,0,0,72,179,1,0,5,0,0,0,2,0,0, 
  0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,53,56,1,128,8,4,17,1,195,181,0,0,1,2,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129, 
  127,7,48,1,128,10,56,1,128,8,4,17,1,243,181,0,0,1,2,21,1,34,2,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66,130,65,80,14,128,100,248,4,128,69,224,12, 
  131,6,240,130,129,7,240,2,128,88,184,9,128,73,160,11,129,74,0,139,129,66,128,13,128,70,64,140,129,89,112,8,128,62,64,16,128,63,112,15,128,90,40,7,128,101,248,3,128,102,248,2,128,8,4,15,1,22,184,0, 
  0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,22,184,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15, 
  1,22,184,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,22,184,0,0,15,1,13,195,0,0,15,1,57,191,0,0,15,1,69,191,0, 
  0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,22,184,0,0,1,4,19,85,0,0,0,237,0,0,0,1, 
  0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,22,184,0,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0, 
  0,0,1,0,17,1,22,184,0,0,1,4,15,1,22,184,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15,1,22,184,0,0,15,1,124,163,0,0,17,1,158,162,0,0,1,4,15,1,22,184,0,0,15,1, 
  124,163,0,0,17,1,6,162,0,0,1,4,15,1,22,184,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15,1,22,184,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,254,145,0,0,1,4,19,47, 
  0,0,0,154,0,0,0,1,0,19,19,0,0,0,62,0,0,0,1,0,15,1,22,184,0,0,17,1,62,146,0,0,1,4,15,1,22,184,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,190,145,0,0,1,4, 
  15,1,22,184,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,201,2,0,0,1,2,21,1,52,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128, 
  11,56,1,128,8,4,19,126,0,0,0,95,1,0,0,6,0,1,2,19,99,0,0,0,17,1,0,0,4,0,1,19,102,0,0,0,27,1,0,0,1,0,1,21,1,77,0,0,0,87,243,1,0,6,0,0,0,2,0,0, 
  0,52,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,10,248,1,128,8,4,15,1,33,191,0,0,15,1,45,191,0,0,17,1,102,186,0,0,1,4,15,1,33,191,0,0,17,1,177,184,0,0,1, 
  2,21,1,131,0,0,0,177,239,1,0,8,0,0,0,3,0,0,0,10,120,3,128,1,144,1,128,2,144,129,127,118,152,1,128,52,216,2,128,117,56,2,128,6,144,65,127,7,144,1,128,8,4,15,1,77,185,0,0,15, 
  1,90,186,0,0,17,1,28,173,0,0,1,4,15,1,77,185,0,0,15,1,65,185,0,0,17,1,136,170,0,0,1,4,15,1,77,185,0,0,15,1,53,185,0,0,17,1,102,186,0,0,1,4,15,1,77,185,0,0,15, 
  1,65,185,0,0,17,1,36,168,0,0,1,2,19,105,0,0,0,33,1,0,0,1,0,1,19,105,0,0,0,35,1,0,0,1,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,105, 
  232,194,128,106,224,2,128,127,48,1,128,125,192,1,128,19,105,0,0,0,33,1,0,0,1,0,17,1,77,185,0,0,1,19,105,0,0,0,34,1,0,0,1,0,17,1,77,185,0,0,1,19,105,0,0,0,35,1,0,0, 
  1,0,17,1,77,185,0,0,1,1,15,1,77,185,0,0,17,1,184,185,0,0,1,2,21,1,125,0,0,0,213,242,1,0,9,0,0,0,3,0,0,0,10,120,3,128,1,176,1,128,2,176,129,127,11,8,3,128,52,152, 
  2,128,117,40,2,128,6,176,129,128,7,176,1,128,118,184,1,128,8,4,15,1,78,186,0,0,17,1,28,173,0,0,1,4,15,1,66,186,0,0,17,1,136,170,0,0,1,4,15,1,54,186,0,0,17,1,102,186,0,0, 
  1,4,19,106,0,0,0,40,1,0,0,3,0,14,1,4,15,1,66,186,0,0,17,1,36,168,0,0,1,2,19,105,0,0,0,36,1,0,0,2,0,1,19,105,0,0,0,38,1,0,0,2,0,1,19,105,0,0,0,37, 
  1,0,0,2,0,1,19,105,0,0,0,34,1,0,0,1,0,1,21,1,159,0,0,0,38,244,1,0,9,0,0,0,3,0,0,0,104,128,2,128,1,176,193,129,2,176,1,128,91,192,3,128,103,32,3,128,85,48,4,128, 
  6,176,1,128,7,176,65,127,121,184,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,15,1,105,188,0,0,17,1,227,187,0,0,1,4,15,1,105,188,0,0,15,1,227,187,0,0,17,1,80,95,0,0,1,4,15, 
  1,105,188,0,0,15,1,227,187,0,0,17,1,6,187,0,0,1,4,15,1,105,188,0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,15,1,105,188,0,0,17,1,227,187,0,0,1,2,21,1, 
  68,0,0,0,85,201,1,0,4,0,0,0,2,0,0,0,52,176,1,128,1,16,129,128,2,16,1,128,57,24,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,75,187,0,0,1,4,15,1,75,187,0,0,17, 
  1,122,3,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,70,200,1,128,13,144,194,128,46,40,130,127,91,56,1,128,113,48,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,75, 
  187,0,0,1,19,113,0,0,0,49,1,0,0,2,0,1,15,1,75,187,0,0,17,1,176,187,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,75,187,0,0,1,2,21,1,39,0,0,0,221,244,1,0,3,0, 
  0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,0,133,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,112,0,67, 
  129,113,112,2,128,86,144,131,128,75,152,3,128,114,224,1,128,128,80,1,128,19,112,0,0,0,48,1,0,0,1,0,17,1,227,187,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,227,187,0,0,1,19,86,0, 
  0,0,241,0,0,0,1,0,17,1,227,187,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,227,187,0,0,1,1,19,112,0,0,0,47,1,0,0,1,0,17,1,227,187,0,0,1,2,21,1,47,0,0,0,72, 
  179,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,53,56,1,128,8,4,17,1,153,188,0,0,1,2,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6, 
  48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,201,188,0,0,1,2,21,1,34,2,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66,130,65, 
  80,14,128,100,248,4,128,69,224,12,131,6,240,130,129,7,240,2,128,88,184,9,128,73,160,11,129,74,0,139,129,66,128,13,128,70,64,140,129,89,112,8,128,62,64,16,128,63,112,15,128,90,40,7,128,101,248,3,128,102, 
  248,2,128,8,4,15,1,236,190,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,236,190,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212,192,0, 
  0,17,1,117,192,0,0,1,4,15,1,236,190,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17,1,159,191,0,0,1,4,15,1,236,190,0,0,15,1,13,195,0,0,15, 
  1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,236,190,0,0,1,4, 
  19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,236,190,0,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0, 
  0,1,0,19,14,0,0,0,55,0,0,0,1,0,17,1,236,190,0,0,1,4,15,1,236,190,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15,1,236,190,0,0,15,1,124,163,0,0,17,1,158,162,0,0, 
  1,4,15,1,236,190,0,0,15,1,124,163,0,0,17,1,6,162,0,0,1,4,15,1,236,190,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15,1,236,190,0,0,15,1,62,146,0,0,15,1,92,160,0,0, 
  17,1,254,145,0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,19,0,0,0,62,0,0,0,1,0,15,1,236,190,0,0,17,1,62,146,0,0,1,4,15,1,236,190,0,0,15,1,62,146,0,0,15,1,92,160, 
  0,0,17,1,190,145,0,0,1,4,15,1,236,190,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,201,2,0,0,1,2,21,1,52,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,127,0,0,0,96,1,0,0,6,0,1,2,19,103,0,0,0,29,1,0,0,4,0,1,19,106,0,0,0,39,1,0,0,1,0,1,19,53,0,0,0,160,0, 
  0,0,1,0,1,21,0,89,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,94,48,2,128,103,16,1,128,82,192,130,127,99,160,129,127,19,82,0,0,0,230,0,0,0,1,0,17,1,69,191,0,0,1,19,82,0, 
  0,0,229,0,0,0,1,0,17,1,69,191,0,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,69,191,0,0,1,1,2,21,1,82,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,32,2,128,1,80,1, 
  129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,15,1,242,191,0,0,17,1,242,101,0,0,1,4,15,1,242,191,0,0,17,1,122,3,0,0,1,2,19,107, 
  0,0,0,41,1,0,0,2,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,84,224,2,129,109,48,1,128,107,80,2,128,83,72,195,127,108,192,1,128,19,83,0,0,0,233,0,0,0,1,0,17, 
  1,254,191,0,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,254,191,0,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,254,191,0,0,1,15,1,254,191,0,0,17,1,204,159,0,0,1,1,2,19,84,0, 
  0,0,234,0,0,0,1,0,1,21,1,82,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,32,2,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0, 
  0,0,1,0,15,1,200,192,0,0,17,1,242,101,0,0,1,4,15,1,200,192,0,0,17,1,122,3,0,0,1,2,19,108,0,0,0,42,1,0,0,2,0,1,19,83,0,0,0,232,0,0,0,1,0,1,21,1,47,0, 
  0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,16,193,0,0,1,2,21,1,94,0,0,0,182,243,1,0,6,0,0,0,2,0, 
  0,0,52,32,2,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,15,1,242,193,0,0,17,1,135,193,0,0,1,4,15,1,242,193,0,0,15, 
  1,111,193,0,0,15,1,123,193,0,0,17,1,122,3,0,0,1,2,19,111,0,0,0,45,1,0,0,1,0,1,19,110,0,0,0,44,1,0,0,1,0,1,21,1,39,0,0,0,79,246,1,0,3,0,0,0,1,0,0, 
  0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,186,193,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21,1,55,0,0,0,112,175,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128, 
  57,248,0,128,8,4,19,46,0,0,0,143,0,0,0,1,0,19,91,0,0,0,254,0,0,0,3,0,1,2,21,0,145,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,70,0,67,129,13,248,195,128,46,144,131,127, 
  91,112,194,128,109,104,2,128,110,216,1,128,111,112,1,128,15,1,242,193,0,0,17,1,132,194,0,0,1,19,111,0,0,0,45,1,0,0,1,0,17,1,242,193,0,0,1,1,19,70,0,0,0,208,0,0,0,1,0,17, 
  1,242,193,0,0,1,19,110,0,0,0,44,1,0,0,1,0,17,1,242,193,0,0,1,15,1,242,193,0,0,17,1,135,193,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,242,193,0,0,1,2,21,1,112,0, 
  0,0,202,246,1,0,7,0,0,0,2,0,0,0,52,112,2,128,1,112,65,129,2,112,129,128,7,112,129,128,6,112,1,128,11,16,3,128,57,120,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,15,1,245,194,0, 
  0,15,1,123,193,0,0,17,1,242,101,0,0,1,4,15,1,245,194,0,0,15,1,123,193,0,0,17,1,122,3,0,0,1,4,19,109,0,0,0,43,1,0,0,4,0,14,1,2,19,111,0,0,0,46,1,0,0,2,0, 
  1,19,0,0,0,0,0,0,0,0,3,0,1,19,14,0,0,0,55,0,0,0,1,0,1,19,53,0,0,0,161,0,0,0,1,0,1,19,83,0,0,0,233,0,0,0,1,0,1,21,1,49,2,0,0,206,157,1,0,19, 
  0,0,0,4,0,0,0,96,40,6,128,1,240,130,128,2,240,66,130,65,200,14,128,100,248,4,128,69,88,13,131,6,240,130,129,7,240,2,128,88,8,10,128,73,24,12,129,74,120,139,129,66,248,13,128,70,184,140,129,89, 
  152,8,128,62,184,16,128,63,232,15,128,90,40,7,128,101,248,3,128,102,248,2,128,8,4,15,1,99,197,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,37,195,0,0,17,1,224,192,0,0,1,4,15,1,99, 
  197,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,212,192,0,0,17,1,117,192,0,0,1,4,15,1,99,197,0,0,15,1,13,195,0,0,15,1,25,195,0,0,15,1,254,191,0,0,15,1,105,192,0,0,17, 
  1,159,191,0,0,1,4,15,1,99,197,0,0,15,1,13,195,0,0,15,1,57,191,0,0,15,1,69,191,0,0,17,1,189,166,0,0,1,4,19,85,0,0,0,238,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1, 
  0,19,14,0,0,0,55,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,85,0,0,0,237,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,19,0, 
  0,0,0,1,0,0,0,3,0,1,4,19,85,0,0,0,236,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,14,0,0,0,55,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,15,1,99, 
  197,0,0,15,1,124,163,0,0,17,1,228,162,0,0,1,4,15,1,99,197,0,0,15,1,124,163,0,0,17,1,158,162,0,0,1,4,15,1,99,197,0,0,15,1,124,163,0,0,17,1,6,162,0,0,1,4,15,1,99, 
  197,0,0,15,1,168,160,0,0,17,1,104,160,0,0,1,4,15,1,99,197,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,254,145,0,0,1,4,19,47,0,0,0,154,0,0,0,1,0,19,19,0,0,0,62, 
  0,0,0,1,0,15,1,99,197,0,0,17,1,62,146,0,0,1,4,15,1,99,197,0,0,15,1,62,146,0,0,15,1,92,160,0,0,17,1,190,145,0,0,1,4,15,1,99,197,0,0,15,1,62,146,0,0,15,1,92, 
  160,0,0,17,1,201,2,0,0,1,2,19,0,0,0,0,1,0,0,0,3,0,1,13,15,1,111,197,0,0,17,1,125,197,0,0,1,21,7,48,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4, 
  15,1,33,199,0,0,15,1,9,199,0,0,15,1,21,199,0,0,17,1,161,198,0,0,1,21,9,243,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,120,2,128,5,8,5,128,2,48,129,128,3,192,3,128,6, 
  80,6,128,4,19,23,0,0,0,69,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,33,199,0,0,1,4,19,23,0,0,0,67,0,0,0,1,0,19,2,0,0, 
  0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,33,199,0,0,1,4,19,23,0,0,0,68,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17, 
  1,33,199,0,0,1,4,19,23,0,0,0,70,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,33,199,0,0,1,4,19,23,0,0,0,71,0,0,0,1,0,19, 
  2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,33,199,0,0,1,2,21,9,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,152,1,128,5,104,2,128,2,48,129,128,3, 
  0,2,128,6,208,2,128,4,19,24,0,0,0,74,0,0,0,2,0,1,4,19,24,0,0,0,72,0,0,0,2,0,1,4,19,24,0,0,0,73,0,0,0,2,0,1,4,19,24,0,0,0,75,0,0,0,2,0,1,4, 
  19,24,0,0,0,76,0,0,0,2,0,1,2,19,3,0,0,0,5,0,0,0,1,0,1,19,2,0,0,0,4,0,0,0,1,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,24,48,1,128,1, 
  72,3,128,2,184,2,128,3,80,66,128,23,192,1,128,19,2,0,0,0,4,0,0,0,1,0,17,1,33,199,0,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,33,199,0,0,1,15,1,33,199,0,0,17,1,140, 
  199,0,0,1,19,3,0,0,0,5,0,0,0,1,0,17,1,33,199,0,0,1,1,2,21,7,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,151,200,0,0,15,1,21,199,0,0,17, 
  1,161,198,0,0,1,21,9,213,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,72,2,128,5,120,4,128,2,48,129,128,3,96,3,128,6,144,5,128,4,19,23,0,0,0,69,0,0,0,1,0,19,2,0,0, 
  0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,23,0,0,0,67,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,23,0,0,0, 
  68,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,23,0,0,0,70,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0, 
  0,2,0,1,4,19,23,0,0,0,71,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,19,1,0,0,0,2,0,0,0,1,0,1,19,3,0,0,0,6,0,0,0, 
  2,0,1,15,1,111,197,0,0,17,1,176,200,0,0,1,21,1,202,0,0,0,69,247,1,0,13,0,0,0,3,0,0,0,24,112,5,131,1,48,194,129,2,48,2,128,27,144,4,128,22,224,133,129,77,136,3,128,6,48, 
  130,127,7,48,2,129,25,0,69,128,57,248,3,128,78,24,3,128,79,168,2,128,80,56,2,128,8,4,15,1,54,5,1,0,17,1,127,1,1,0,1,4,15,1,54,5,1,0,17,1,28,1,1,0,1,4,15,1,54,5, 
  1,0,17,1,39,255,0,0,1,4,15,1,54,5,1,0,17,1,49,253,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,54,5,1,0,1,4,15,1,54,5,1,0,17,1,85,243,0,0,1,4,15,1,54, 
  5,1,0,17,1,193,234,0,0,1,4,15,1,54,5,1,0,17,1,40,226,0,0,1,4,15,1,54,5,1,0,17,1,123,201,0,0,1,2,21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1, 
  128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,182,201,0,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,46,240,0,128, 
  13,136,65,128,27,128,1,128,19,13,0,0,0,41,0,0,0,1,0,17,1,182,201,0,0,1,1,15,1,182,201,0,0,17,1,245,201,0,0,1,2,21,1,47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6, 
  48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,37,202,0,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91, 
  72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,213,206,0,0,1,4, 
  15,1,213,206,0,0,17,1,80,95,0,0,1,4,15,1,213,206,0,0,17,1,248,205,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,213,206,0,0,1,4,15,1,213,206,0,0,17,1,87,93,0,0,1, 
  4,19,75,0,0,0,215,0,0,0,1,0,17,1,213,206,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,213,206,0,0,1,4,15,1,213,206,0,0,17,1,122,3,0,0,1,4,15,1,213,206,0,0,17, 
  1,255,202,0,0,1,2,21,1,176,0,0,0,37,171,1,0,11,0,0,0,3,0,0,0,104,144,2,128,1,240,193,129,2,240,1,128,91,112,3,128,52,16,5,128,85,224,3,128,6,240,1,128,7,240,129,128,57,120,132, 
  128,103,0,3,128,121,248,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,176,203,0,0,1,4,15,1,176,203,0,0,17,1,80,95,0,0,1,4,15,1,176,203,0,0,17,1,11,95,0,0,1,4,15,1, 
  176,203,0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,176,203,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,176,203,0,0,1,4,15,1,176,203,0,0,17,1,122,3, 
  0,0,1,2,21,0,72,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,112,160,6,131,113,16,134,131,114,128,5,128,86,192,199,129,116,240,4,128,117,232,4,128,70,224,72,127,135,32,3,128,136,144,2,128,91,48, 
  7,128,118,128,4,128,75,80,136,127,128,240,3,128,13,176,9,128,46,72,9,128,129,136,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,176,203,0,0,1,15,1,176,203,0,0,17,1,28,92,0,0,1,15,1,176, 
  203,0,0,17,1,134,91,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,176,203,0,0,1,15,1,176,203,0,0,17,1,249,204,0,0,1,1,19,118,0,0,0,84,1,0,0,1,0,17,1,176,203,0,0,1, 
  19,86,0,0,0,242,0,0,0,1,0,17,1,176,203,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,176,203,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,176,203,0,0,1,19,70,0,0,0,208, 
  0,0,0,1,0,17,1,176,203,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,176,203,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,176,203,0,0,1,15,1,176,203,0,0,17,1,220,6,0,0, 
  1,15,1,176,203,0,0,17,1,78,6,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,176,203,0,0,1,2,21,1,188,0,0,0,248,203,1,0,12,0,0,0,3,0,0,0,104,176,2,128,1,16,2,130,2, 
  16,2,128,91,144,3,128,52,48,5,128,85,0,4,128,6,16,130,128,7,16,194,128,46,160,5,128,57,152,132,128,103,32,3,128,121,24,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,241,87,0,0,1,4, 
  15,1,241,87,0,0,17,1,80,95,0,0,1,4,15,1,241,87,0,0,17,1,212,80,0,0,1,4,15,1,241,87,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,241,87,0,0,1, 
  4,19,46,0,0,0,143,0,0,0,1,0,17,1,241,87,0,0,1,4,15,1,241,87,0,0,17,1,202,7,0,0,1,4,17,1,182,205,0,0,1,2,21,1,53,0,0,0,91,253,1,0,5,0,0,0,2,0,0,0, 
  116,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,117,0,0,0,82,1,0,0,4,0,14,1,19,117,0,0,0,83,1,0,0,3,0,14,1,21,1,68,0,0,0,85,201,1,0,4,0, 
  0,0,2,0,0,0,52,176,1,128,1,16,129,128,2,16,1,128,57,24,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,61,206,0,0,1,4,15,1,61,206,0,0,17,1,122,3,0,0,1,2,21,0,100, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,70,200,1,128,13,144,194,128,46,40,130,127,91,56,1,128,113,48,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,61,206,0,0,1,19,113,0,0,0,49, 
  1,0,0,2,0,1,15,1,61,206,0,0,17,1,162,206,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,61,206,0,0,1,2,21,1,39,0,0,0,51,254,1,0,3,0,0,0,1,0,0,0,2,240,128,128, 
  1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,0,189,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112,16,200,132,113,128,199,132,114,240,6,128,75, 
  32,11,131,68,128,12,131,69,24,12,131,70,176,139,129,87,0,202,130,136,80,3,128,89,152,9,128,90,48,9,128,27,80,13,126,86,144,10,128,13,88,13,128,46,232,12,128,91,160,8,128,116,96,6,128,117,208,5,128,119, 
  64,197,128,128,176,4,128,129,72,4,128,135,224,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,213,206,0,0,1,15,1,213,206,0,0,17,1,71,225,0,0,1,15,1,213,206,0,0,17,1,171,223,0,0,1,19, 
  112,0,0,0,48,1,0,0,1,0,17,1,213,206,0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,213,206,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,213,206,0,0,1,19,89,0,0,0,247,0, 
  0,0,1,0,17,1,213,206,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,213,206,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,213,206,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1, 
  213,206,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,213,206,0,0,1,15,1,213,206,0,0,17,1,106,223,0,0,1,15,1,213,206,0,0,17,1,4,218,0,0,1,19,69,0,0,0,204,0,0,0,1,0, 
  17,1,213,206,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,213,206,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,213,206,0,0,1,15,1,213,206,0,0,17,1,209,217,0,0,1,15,1,213,206, 
  0,0,17,1,198,208,0,0,1,19,27,0,0,0,86,0,0,0,4,0,14,1,15,1,213,206,0,0,17,1,147,208,0,0,1,1,19,70,0,0,0,207,0,0,0,1,0,17,1,213,206,0,0,1,2,21,1,39,0,0, 
  0,87,255,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21,1,47,0,0,0,93,0,2,0,5,0,0,0, 
  2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,81,48,1,128,4,17,1,1,209,0,0,1,8,19,68,0,0,0,203,0,0,0,1,0,1,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3, 
  0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0, 
  0,0,97,1,0,0,1,0,17,1,219,209,0,0,1,4,15,1,219,209,0,0,17,1,80,95,0,0,1,4,15,1,219,209,0,0,17,1,212,80,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,219,209,0, 
  0,1,4,15,1,219,209,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,219,209,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,219,209,0,0,1,4,15,1,219,209,0, 
  0,17,1,202,7,0,0,1,4,15,1,219,209,0,0,17,1,164,8,0,0,1,2,21,0,149,1,0,0,255,255,255,255,20,0,0,0,4,0,0,0,112,208,71,132,113,64,71,132,114,176,6,128,86,32,10,128,116,32,6, 
  128,69,168,139,130,70,64,75,127,87,192,73,130,136,16,3,128,89,88,9,128,90,240,8,128,75,176,74,128,91,96,8,128,13,24,12,128,46,176,11,128,117,144,5,128,119,0,197,128,128,112,4,128,129,8,4,128,135,160,3, 
  128,19,135,0,0,0,121,1,0,0,1,0,17,1,219,209,0,0,1,15,1,219,209,0,0,17,1,112,217,0,0,1,15,1,219,209,0,0,17,1,182,216,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,219,209, 
  0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,219,209,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,219,209,0,0,1,19,89,0,0,0,247,0,0,0,1,0,17,1,219,209,0,0,1,19,86,0, 
  0,0,242,0,0,0,1,0,17,1,219,209,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,219,209,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,219,209,0,0,1,19,70,0,0,0,208,0,0,0, 
  1,0,17,1,219,209,0,0,1,15,1,219,209,0,0,17,1,117,216,0,0,1,15,1,219,209,0,0,17,1,215,211,0,0,1,19,69,0,0,0,205,0,0,0,3,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1, 
  219,209,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,219,209,0,0,1,15,1,219,209,0,0,17,1,164,211,0,0,1,1,15,1,219,209,0,0,17,1,113,211,0,0,1,19,70,0,0,0,207,0,0,0,1, 
  0,17,1,219,209,0,0,1,2,21,1,39,0,0,0,240,0,2,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1, 
  21,1,39,0,0,0,234,1,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120,1,0,0,1,0,1,21,1,217,0,0,0,228,2,2, 
  0,13,0,0,0,3,0,0,0,104,88,6,128,1,80,198,129,2,80,6,128,91,176,4,128,52,160,2,128,45,16,3,129,6,80,6,128,7,80,198,128,57,184,5,129,85,32,5,128,95,128,67,128,103,48,2,128,121,24,4, 
  128,4,15,1,188,212,0,0,17,1,212,80,0,0,1,4,15,1,188,212,0,0,17,1,202,7,0,0,1,4,15,1,188,212,0,0,17,1,164,8,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,188,212,0, 
  0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1,188,212,0,0,1,4,15,1,188,212,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,188,212,0,0,1,4,19,46,0,0,0, 
  143,0,0,0,1,0,17,1,188,212,0,0,1,8,4,15,1,188,212,0,0,17,1,80,95,0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,208, 
  194,131,113,104,200,131,114,24,10,128,86,248,8,128,116,120,7,128,117,224,5,128,70,64,70,127,119,96,131,130,136,192,4,128,89,184,4,128,90,184,4,128,75,216,71,128,91,40,4,128,13,80,5,128,46,16,7,128,128,136, 
  9,128,129,168,6,128,135,192,3,128,19,86,0,0,0,240,0,0,0,1,0,17,1,188,212,0,0,1,19,90,0,0,0,251,0,0,0,2,0,1,15,1,188,212,0,0,17,1,20,216,0,0,1,19,70,0,0,0,208,0, 
  0,0,1,0,17,1,188,212,0,0,1,1,19,135,0,0,0,121,1,0,0,1,0,17,1,188,212,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,188,212,0,0,1,19,89,0,0,0,250,0,0,0,2,0,1, 
  15,1,188,212,0,0,17,1,225,215,0,0,1,15,1,188,212,0,0,17,1,69,214,0,0,1,15,1,188,212,0,0,17,1,18,214,0,0,1,19,89,0,0,0,249,0,0,0,2,0,1,19,112,0,0,0,47,1,0,0, 
  1,0,17,1,188,212,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,188,212,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,188,212,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,188,212, 
  0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,188,212,0,0,1,2,21,1,39,0,0,0,240,0,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0, 
  1,19,13,0,0,0,41,0,0,0,1,0,1,21,1,69,0,0,0,177,3,2,0,5,0,0,0,2,0,0,0,112,48,1,128,1,32,2,128,2,32,130,128,115,112,1,128,10,176,1,128,4,17,1,57,215,0,0,1,4, 
  17,1,106,26,0,0,1,4,15,1,150,214,0,0,17,1,143,24,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128, 
  15,1,150,214,0,0,17,1,191,214,0,0,1,1,2,21,1,51,0,0,0,148,4,2,0,4,0,0,0,2,0,0,0,112,88,1,128,1,80,1,128,2,80,1,128,115,16,1,128,4,17,1,228,25,0,0,1,8,4,17, 
  1,255,214,0,0,1,19,116,0,0,0,69,1,0,0,2,0,14,1,21,1,45,0,0,0,108,5,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,115,240,0,128,4,19,116,0,0,0,80,1,0,0,4, 
  0,14,1,8,19,116,0,0,0,74,1,0,0,3,0,14,1,21,1,57,0,0,0,57,6,2,0,4,0,0,0,2,0,0,0,10,80,1,128,1,192,1,128,2,192,129,127,115,16,1,128,4,17,1,48,23,0,0,1,4, 
  15,1,126,215,0,0,17,1,88,20,0,0,1,8,19,116,0,0,0,71,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,126,215,0,0,17,1, 
  167,215,0,0,1,1,2,21,1,45,0,0,0,108,5,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,115,248,0,128,8,4,19,116,0,0,0,77,1,0,0,4,0,14,1,19,116,0,0,0,67,1,0, 
  0,3,0,14,1,21,1,39,0,0,0,234,1,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120,1,0,0,1,0,1,21,1,85,0, 
  0,0,17,7,2,0,6,0,0,0,2,0,0,0,124,80,1,128,1,160,194,128,2,160,194,128,123,144,1,128,25,208,1,128,122,56,2,128,4,17,1,73,30,0,0,1,4,17,1,246,28,0,0,1,4,19,129,0,0,0, 
  98,1,0,0,2,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,8,19,129,0,0,0,106,1,0,0,1,0,1,21,1,53,0,0,0,11,8,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2, 
  160,129,127,7,160,1,128,61,48,1,128,4,15,1,76,79,0,0,17,1,2,36,0,0,1,8,19,87,0,0,0,244,0,0,0,1,0,1,21,1,69,0,0,0,177,3,2,0,5,0,0,0,2,0,0,0,112,224,1,128, 
  1,32,2,128,2,32,130,128,115,160,1,128,10,48,1,128,4,15,1,7,217,0,0,17,1,143,24,0,0,1,4,17,1,86,32,0,0,1,4,17,1,57,215,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21, 
  0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1,7,217,0,0,17,1,48,217,0,0,1,1,2,21,1,51,0,0,0,148,4,2,0,4,0,0,0,2,0,0,0,112,80, 
  1,128,1,144,1,128,2,144,1,128,115,16,1,128,4,17,1,228,25,0,0,1,4,17,1,255,214,0,0,1,8,19,116,0,0,0,69,1,0,0,2,0,14,1,21,1,85,0,0,0,17,7,2,0,6,0,0,0,2,0, 
  0,0,124,104,2,128,1,96,194,128,2,96,194,128,123,80,1,128,25,144,1,128,122,248,1,128,4,17,1,246,28,0,0,1,4,19,129,0,0,0,98,1,0,0,2,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1, 
  8,4,17,1,73,30,0,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,1,39,0,0,0,107,8,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,127,248,0,128,8,4,17,1,15,7,0,0,1, 
  19,135,0,0,0,120,1,0,0,1,0,1,21,1,212,0,0,0,167,9,2,0,13,0,0,0,3,0,0,0,104,40,5,128,1,16,195,129,2,16,3,128,91,24,3,128,52,48,6,128,45,160,2,129,6,16,3,128,7,16, 
  195,128,57,136,3,129,85,152,5,128,95,144,68,128,103,48,2,128,121,248,3,128,4,15,1,20,222,0,0,17,1,212,80,0,0,1,4,15,1,20,222,0,0,17,1,164,8,0,0,1,10,4,15,1,20,222,0,0,17,1, 
  160,10,0,0,1,6,15,1,20,222,0,0,17,1,228,218,0,0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1,20,222,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,20,222,0,0,1,4,15,1, 
  20,222,0,0,17,1,80,95,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,20,222,0,0,1,4,15,1,20,222,0,0,17,1,202,7,0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,1,76,1, 
  0,0,139,10,2,0,29,0,0,0,4,0,0,0,112,224,8,128,1,88,197,132,2,88,5,128,3,48,68,133,4,48,68,131,85,96,5,128,6,88,133,130,7,88,197,131,24,96,198,131,25,32,71,130,10,96,7,132,27,224, 
  137,130,124,32,10,128,45,152,132,129,94,160,9,128,95,224,71,131,22,96,9,128,52,32,8,128,57,160,199,129,61,96,8,128,81,224,6,128,91,160,134,129,103,24,5,128,104,216,4,128,115,160,5,128,121,32,9,128,122,32, 
  6,128,123,224,5,128,127,160,8,128,12,19,90,0,0,0,253,0,0,0,1,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,10,12,17,1,49,220,0,0,1,12,17,1,49, 
  220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1, 
  12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49, 
  220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,2,21,1,176,0,0,0,37,171,1,0,11,0,0,0,3,0,0,0,104,144,2,128,1,240,193,129,2,240,1,128,91, 
  112,3,128,52,16,5,128,85,224,3,128,6,240,1,128,7,240,129,128,57,120,132,128,103,0,3,128,121,248,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,226,220,0,0,1,4,15,1,226,220,0,0,17,1, 
  80,95,0,0,1,4,15,1,226,220,0,0,17,1,212,80,0,0,1,4,15,1,226,220,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,226,220,0,0,1,4,19,46,0,0,0,143,0, 
  0,0,1,0,17,1,226,220,0,0,1,4,15,1,226,220,0,0,17,1,202,7,0,0,1,2,21,0,49,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,112,224,5,131,89,0,135,130,114,192,4,128,75,152,199,129, 
  116,96,4,128,13,248,8,128,46,144,136,128,135,0,3,128,70,40,72,128,86,8,7,128,91,112,6,128,113,80,133,128,128,208,131,128,129,104,3,128,136,112,2,128,19,135,0,0,0,121,1,0,0,1,0,17,1,226,220,0, 
  0,1,15,1,226,220,0,0,17,1,149,28,0,0,1,15,1,226,220,0,0,17,1,194,19,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,226,220,0,0,1,19,89,0,0,0,249,0,0,0,2,0,1,19,86, 
  0,0,0,242,0,0,0,1,0,17,1,226,220,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,226,220,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,226,220,0,0,1,19,70,0,0,0,208,0,0, 
  0,1,0,17,1,226,220,0,0,1,1,19,135,0,0,0,122,1,0,0,1,0,17,1,226,220,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,226,220,0,0,1,15,1,226,220,0,0,17,1,83,33,0,0,1, 
  15,1,226,220,0,0,17,1,13,14,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,226,220,0,0,1,2,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,240,196,131,113,96,196,131,114,16, 
  6,128,86,32,8,128,116,24,9,128,117,48,7,128,70,104,67,127,119,120,137,130,136,208,3,128,89,96,3,128,90,96,3,128,75,144,71,128,91,128,5,128,13,160,6,128,46,64,10,128,128,208,2,128,129,176,8,128,135,216, 
  9,128,19,112,0,0,0,48,1,0,0,1,0,17,1,20,222,0,0,1,1,15,1,20,222,0,0,17,1,83,33,0,0,1,19,135,0,0,0,121,1,0,0,1,0,17,1,20,222,0,0,1,19,86,0,0,0,241,0,0, 
  0,1,0,17,1,20,222,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,20,222,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,20,222,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,20, 
  222,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,20,222,0,0,1,19,89,0,0,0,250,0,0,0,2,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,20,222,0,0,1,19,135,0,0,0,122,1,0, 
  0,1,0,17,1,20,222,0,0,1,15,1,20,222,0,0,17,1,194,19,0,0,1,19,89,0,0,0,249,0,0,0,2,0,1,19,90,0,0,0,251,0,0,0,2,0,1,15,1,20,222,0,0,17,1,149,28,0,0,1, 
  15,1,20,222,0,0,17,1,13,14,0,0,1,2,21,1,53,0,0,0,169,11,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2,160,129,127,7,160,1,128,61,48,1,128,4,15,1,76,79,0,0,17, 
  1,2,36,0,0,1,8,19,87,0,0,0,244,0,0,0,1,0,1,21,1,69,0,0,0,60,12,2,0,5,0,0,0,2,0,0,0,112,48,1,128,1,224,1,128,2,224,129,128,115,232,1,128,10,112,1,128,4,17,1, 
  159,224,0,0,1,4,15,1,252,223,0,0,17,1,143,24,0,0,1,8,4,17,1,86,32,0,0,1,19,116,0,0,0,81,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56, 
  65,128,132,208,0,128,15,1,252,223,0,0,17,1,37,224,0,0,1,1,2,21,1,51,0,0,0,54,13,2,0,4,0,0,0,2,0,0,0,112,16,1,128,1,144,1,128,2,144,1,128,115,80,1,128,4,17,1,101,224, 
  0,0,1,4,17,1,228,25,0,0,1,8,19,116,0,0,0,69,1,0,0,2,0,14,1,21,1,45,0,0,0,37,14,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,115,240,0,128,4,19,116,0,0, 
  0,80,1,0,0,4,0,14,1,8,19,116,0,0,0,74,1,0,0,3,0,14,1,21,1,57,0,0,0,9,15,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,128,1,128,2,128,129,127,115,136,1,128,4,15,1, 
  228,224,0,0,17,1,88,20,0,0,1,8,4,17,1,48,23,0,0,1,19,116,0,0,0,71,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,56,65,128,132,208,0,128,15,1, 
  228,224,0,0,17,1,13,225,0,0,1,1,2,21,1,45,0,0,0,37,14,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,115,240,0,128,4,19,116,0,0,0,77,1,0,0,4,0,14,1,8,19,116, 
  0,0,0,67,1,0,0,3,0,14,1,21,1,80,0,0,0,248,15,2,0,6,0,0,0,2,0,0,0,124,64,2,128,1,80,193,128,2,80,193,128,123,88,1,128,25,152,1,128,122,216,1,128,10,4,17,1,246,28,0, 
  0,1,6,17,1,163,225,0,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,4,17,1,73,30,0,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,62,176,0,128,12,19,129,0,0,0,106,1,0,0,1,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,12,17,1,233,225,0,0,1,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,19,129,0,0,0,98,1,0,0,2,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,58,0, 
  0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,99,226,0,0,1,2,21,0,62,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,28,128,129,128,13,136,1,128,46,240,0,128,19,13,0,0,0,41,0,0,0,1,0,17,1,99,226,0,0,1,1,15,1,99,226,0,0,17,1,162,226,0,0,1,2,21,1, 
  47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,210,226,0,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0, 
  3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128, 
  0,0,0,97,1,0,0,1,0,17,1,172,227,0,0,1,4,15,1,172,227,0,0,17,1,80,95,0,0,1,4,15,1,172,227,0,0,17,1,248,205,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,172,227, 
  0,0,1,4,15,1,172,227,0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,172,227,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,172,227,0,0,1,4,15,1,172,227, 
  0,0,17,1,122,3,0,0,1,4,15,1,172,227,0,0,17,1,255,202,0,0,1,2,21,0,189,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112,16,200,132,113,128,199,132,114,240,6,128,86,144,10,128,68,128, 
  12,131,69,24,12,131,70,176,75,127,87,0,202,130,136,80,3,128,89,152,9,128,90,48,9,128,75,32,11,129,28,80,13,128,13,88,13,128,46,232,12,128,91,160,8,128,116,96,6,128,117,208,5,128,119,64,197,128,128,176, 
  4,128,129,72,4,128,135,224,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,172,227,0,0,1,15,1,172,227,0,0,17,1,101,234,0,0,1,15,1,172,227,0,0,17,1,207,233,0,0,1,19,112,0,0,0,48, 
  1,0,0,1,0,17,1,172,227,0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,172,227,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,172,227,0,0,1,19,89,0,0,0,247,0,0,0,1,0,17, 
  1,172,227,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,172,227,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,172,227,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,172,227,0,0,1, 
  19,70,0,0,0,208,0,0,0,1,0,17,1,172,227,0,0,1,15,1,172,227,0,0,17,1,142,233,0,0,1,15,1,172,227,0,0,17,1,11,230,0,0,1,19,69,0,0,0,204,0,0,0,1,0,17,1,172,227,0, 
  0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,172,227,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,172,227,0,0,1,15,1,172,227,0,0,17,1,216,229,0,0,1,15,1,172,227,0,0,17,1,157, 
  229,0,0,1,19,28,0,0,0,87,0,0,0,4,0,14,1,15,1,172,227,0,0,17,1,106,229,0,0,1,1,19,70,0,0,0,207,0,0,0,1,0,17,1,172,227,0,0,1,2,21,1,39,0,0,0,87,255,1,0, 
  3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,1,47,0,0,0,93,0,2,0,5,0,0,0,2,0,0,0,6, 
  112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,81,48,1,128,4,17,1,1,209,0,0,1,8,19,68,0,0,0,203,0,0,0,1,0,1,21,1,39,0,0,0,107,8,2,0,3,0,0,0,1,0,0,0,2,48, 
  1,128,1,48,65,128,127,240,0,128,4,17,1,15,7,0,0,1,8,19,135,0,0,0,120,1,0,0,1,0,1,21,1,212,0,0,0,167,9,2,0,13,0,0,0,3,0,0,0,104,184,4,128,1,208,195,129,2,208,3, 
  128,91,96,3,128,52,192,5,128,45,72,4,129,6,208,3,128,7,208,195,128,57,216,3,129,85,48,2,128,95,200,66,128,103,48,6,128,121,40,5,128,4,19,75,0,0,0,215,0,0,0,1,0,17,1,56,232,0,0,1, 
  4,19,119,0,0,0,86,1,0,0,1,0,17,1,56,232,0,0,1,4,15,1,56,232,0,0,17,1,160,10,0,0,1,10,6,15,1,56,232,0,0,17,1,235,230,0,0,1,4,15,1,56,232,0,0,17,1,164,8,0, 
  0,1,4,15,1,56,232,0,0,17,1,80,95,0,0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1,56,232,0,0,1,4,15,1,56,232,0,0,17,1,202,7,0,0,1,4,15,1,56,232,0,0,17,1,212,80, 
  0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,1,76,1,0,0,139,10,2,0,29,0,0,0,4,0,0,0,112,224,6,128,1,88,197,132,2,88,5,128,3,48,68,133,4,48,68,131,85,96,6,128,6,88,133, 
  130,7,88,197,131,24,160,199,131,25,32,73,130,10,216,4,132,27,24,133,130,124,224,7,128,45,96,135,129,94,224,5,128,95,32,71,131,22,32,6,128,52,160,8,128,57,32,200,129,61,96,9,128,81,160,5,128,91,224,137, 
  129,103,160,6,128,104,224,8,128,115,32,10,128,121,152,4,128,122,96,5,128,123,96,8,128,127,160,9,128,12,19,90,0,0,0,253,0,0,0,1,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17, 
  1,49,220,0,0,1,10,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220, 
  0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12, 
  17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,2,21,0,85,1,0,0,255,255,255,255,18,0, 
  0,0,4,0,0,0,112,32,197,131,113,0,200,131,114,8,7,128,86,176,9,128,116,48,3,128,117,168,6,128,70,184,68,127,119,208,130,130,136,152,3,128,89,144,3,128,90,144,3,128,75,176,69,128,91,144,8,128,13,40, 
  4,128,46,64,10,128,128,32,9,128,129,152,7,128,135,64,6,128,19,90,0,0,0,251,0,0,0,2,0,1,19,89,0,0,0,249,0,0,0,2,0,1,1,19,135,0,0,0,121,1,0,0,1,0,17,1,56,232,0,0, 
  1,19,70,0,0,0,207,0,0,0,1,0,17,1,56,232,0,0,1,15,1,56,232,0,0,17,1,83,33,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,56,232,0,0,1,19,112,0,0,0,47,1,0,0,1, 
  0,17,1,56,232,0,0,1,15,1,56,232,0,0,17,1,149,28,0,0,1,19,89,0,0,0,250,0,0,0,2,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,56,232,0,0,1,15,1,56,232,0,0,17,1,194, 
  19,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,56,232,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,56,232,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,56,232,0,0,1,19,135, 
  0,0,0,122,1,0,0,1,0,17,1,56,232,0,0,1,15,1,56,232,0,0,17,1,13,14,0,0,1,2,21,1,53,0,0,0,169,11,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2,160,129,127, 
  7,160,1,128,61,48,1,128,4,15,1,76,79,0,0,17,1,2,36,0,0,1,8,19,87,0,0,0,244,0,0,0,1,0,1,21,1,69,0,0,0,60,12,2,0,5,0,0,0,2,0,0,0,112,112,1,128,1,32,2, 
  128,2,32,130,128,115,48,1,128,10,176,1,128,4,17,1,86,32,0,0,1,4,17,1,32,234,0,0,1,4,15,1,252,223,0,0,17,1,143,24,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,1,57,0, 
  0,0,9,15,2,0,4,0,0,0,2,0,0,0,10,80,1,128,1,192,1,128,2,192,129,127,115,16,1,128,4,17,1,48,23,0,0,1,4,15,1,228,224,0,0,17,1,88,20,0,0,1,8,19,116,0,0,0,71,1, 
  0,0,2,0,1,21,1,80,0,0,0,248,15,2,0,6,0,0,0,2,0,0,0,124,152,1,128,1,144,193,128,2,144,193,128,123,216,1,128,25,80,1,128,122,24,2,128,6,17,1,163,225,0,0,1,10,4,17,1,73, 
  30,0,0,1,4,17,1,246,28,0,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,1,47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1, 
  48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,241,234,0,0,1,2,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6, 
  80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,62,235,0,0,1,4,15,1,62,235,0,0,17,1,122,3,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  29,144,2,128,13,152,194,127,46,40,130,128,91,48,1,128,70,192,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,62,235,0,0,1,15,1,62,235,0,0,17,1,215,235,0,0,1,15,1,62,235,0,0,17,1,164, 
  235,0,0,1,1,19,70,0,0,0,207,0,0,0,1,0,17,1,62,235,0,0,1,2,21,1,39,0,0,0,254,16,2,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0, 
  0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21,1,47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,7,236,0, 
  0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4, 
  128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,225,236,0,0,1,4,15,1,225,236,0,0,17,1,80,95,0,0,1,4,15,1,225,236,0,0,17,1,248,205,0,0, 
  1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,225,236,0,0,1,4,15,1,225,236,0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,225,236,0,0,1,4,19,46,0,0,0,143, 
  0,0,0,1,0,17,1,225,236,0,0,1,4,15,1,225,236,0,0,17,1,122,3,0,0,1,4,15,1,225,236,0,0,17,1,255,202,0,0,1,2,21,0,189,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112, 
  16,200,132,113,128,199,132,114,240,6,128,29,80,13,128,68,128,12,131,69,24,12,131,70,176,139,129,87,0,202,130,136,80,3,128,89,152,9,128,90,48,9,128,75,32,11,129,86,144,10,128,13,88,141,125,46,232,12,128,91, 
  160,8,128,116,96,6,128,117,208,5,128,119,64,197,128,128,176,4,128,129,72,4,128,135,224,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,225,236,0,0,1,15,1,225,236,0,0,17,1,249,242,0,0,1,15,1, 
  225,236,0,0,17,1,99,242,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,225,236,0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,225,236,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1, 
  225,236,0,0,1,19,89,0,0,0,247,0,0,0,1,0,17,1,225,236,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,225,236,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,225,236,0,0,1,19, 
  86,0,0,0,240,0,0,0,1,0,17,1,225,236,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,225,236,0,0,1,15,1,225,236,0,0,17,1,34,242,0,0,1,15,1,225,236,0,0,17,1,159,238,0,0, 
  1,19,69,0,0,0,204,0,0,0,1,0,17,1,225,236,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,225,236,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,225,236,0,0,1,15,1,225,236,0, 
  0,17,1,216,229,0,0,1,15,1,225,236,0,0,17,1,198,208,0,0,1,19,29,0,0,0,88,0,0,0,5,0,14,1,15,1,225,236,0,0,17,1,147,208,0,0,1,1,19,70,0,0,0,207,0,0,0,1,0,17, 
  1,225,236,0,0,1,2,21,1,212,0,0,0,167,9,2,0,13,0,0,0,3,0,0,0,104,168,2,128,1,160,194,129,2,160,2,128,91,32,4,128,52,176,3,128,45,144,4,129,6,160,2,128,7,160,194,128,57,48,2, 
  129,85,152,5,128,95,0,69,128,103,48,6,128,121,24,3,128,6,15,1,204,240,0,0,17,1,127,239,0,0,1,10,4,15,1,204,240,0,0,17,1,80,95,0,0,1,4,19,128,0,0,0,97,1,0,0,1,0,17,1, 
  204,240,0,0,1,4,15,1,204,240,0,0,17,1,202,7,0,0,1,4,15,1,204,240,0,0,17,1,160,10,0,0,1,4,15,1,204,240,0,0,17,1,164,8,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17, 
  1,204,240,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,204,240,0,0,1,4,15,1,204,240,0,0,17,1,212,80,0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,1,76,1,0,0,139,10,2, 
  0,29,0,0,0,4,0,0,0,112,96,6,128,1,88,197,132,2,88,5,128,3,48,68,133,4,48,68,131,85,224,8,128,6,88,133,130,7,88,197,131,24,32,202,131,25,224,69,130,10,32,9,132,27,160,136,130,124,160,5, 
  128,45,216,132,129,94,96,9,128,95,32,70,131,22,224,6,128,52,24,5,128,57,96,197,129,61,96,8,128,81,224,9,128,91,160,134,129,103,152,4,128,104,160,7,128,115,224,7,128,121,96,7,128,122,32,8,128,123,32,7, 
  128,127,160,9,128,12,19,90,0,0,0,253,0,0,0,1,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,10,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12, 
  17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220, 
  0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12, 
  17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,2,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,224,197,131,113,72,197,131,114,56,7,128,86,184,4,128,116,112, 
  6,128,117,80,9,128,70,64,74,127,119,200,131,130,136,40,4,128,89,216,5,128,90,216,5,128,75,176,73,128,91,56,3,128,13,192,8,128,46,208,6,128,128,200,7,128,129,88,8,128,135,208,2,128,15,1,204,240,0,0, 
  17,1,149,28,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,204,240,0,0,1,19,90,0,0,0,251,0,0,0,2,0,1,19,135,0,0,0,121,1,0,0,1,0,17,1,204,240,0,0,1,19,135,0,0,0, 
  122,1,0,0,1,0,17,1,204,240,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,204,240,0,0,1,1,19,86,0,0,0,240,0,0,0,1,0,17,1,204,240,0,0,1,19,89,0,0,0,249,0,0,0,2, 
  0,1,15,1,204,240,0,0,17,1,13,14,0,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,204,240,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,204,240,0,0,1,15,1,204,240,0,0,17,1,194, 
  19,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,204,240,0,0,1,19,89,0,0,0,250,0,0,0,2,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,204,240,0,0,1,15,1,204,240,0,0,17,1, 
  83,33,0,0,1,2,21,1,53,0,0,0,169,11,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,61,56,1,128,8,4,15,1,76,79,0,0,17,1,2,36,0,0,1,19, 
  87,0,0,0,244,0,0,0,1,0,1,21,1,69,0,0,0,60,12,2,0,5,0,0,0,2,0,0,0,112,112,1,128,1,32,2,128,2,32,130,128,115,48,1,128,10,176,1,128,4,17,1,106,26,0,0,1,4,17,1, 
  180,242,0,0,1,4,15,1,252,223,0,0,17,1,143,24,0,0,1,8,19,116,0,0,0,81,1,0,0,1,0,1,21,1,57,0,0,0,9,15,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,192,1,128,2,192, 
  129,127,115,128,1,128,4,15,1,228,224,0,0,17,1,88,20,0,0,1,4,17,1,48,23,0,0,1,8,19,116,0,0,0,71,1,0,0,2,0,1,21,1,80,0,0,0,248,15,2,0,6,0,0,0,2,0,0,0,124, 
  152,1,128,1,80,193,128,2,80,193,128,123,88,1,128,25,64,2,128,122,216,1,128,10,4,17,1,246,28,0,0,1,4,17,1,73,30,0,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,6,17,1,163,225,0,0, 
  1,19,129,0,0,0,106,1,0,0,1,0,1,21,1,76,0,0,0,82,17,2,0,6,0,0,0,2,0,0,0,56,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0, 
  0,0,143,0,0,0,1,0,17,1,162,243,0,0,1,4,15,1,162,243,0,0,17,1,47,10,0,0,1,2,21,0,96,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,46,192,1,128,45,40,2,128,30,248,130,127, 
  31,144,66,128,71,48,1,128,19,31,0,0,0,90,0,0,0,1,0,17,1,162,243,0,0,1,15,1,162,243,0,0,17,1,193,252,0,0,1,15,1,162,243,0,0,17,1,92,252,0,0,1,15,1,162,243,0,0,17,1, 
  3,244,0,0,1,1,2,21,1,59,0,0,0,195,17,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,193,128,2,80,129,127,7,80,1,128,9,152,65,128,21,88,1,128,8,4,17,1,164,244,0,0,1,4,17, 
  1,63,244,0,0,1,2,21,1,88,0,0,0,82,17,2,0,6,0,0,0,2,0,0,0,56,32,2,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0, 
  1,0,15,1,152,244,0,0,17,1,193,252,0,0,1,4,15,1,152,244,0,0,15,1,92,252,0,0,17,1,40,109,0,0,1,2,19,31,0,0,0,91,0,0,0,3,0,1,21,1,58,0,0,0,237,154,1,0,5,0, 
  0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,223,244,0,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,30,128,129,128,13,136,1,128,46,240,0,128,19,13,0,0,0,41,0,0,0,1,0,17,1,223,244,0,0,1,1,15,1,223,244,0,0,17,1,30,245,0,0,1,2,21,1,47,0,0,0,7,253,1,0, 
  5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,78,245,0,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128, 
  1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1, 
  0,17,1,40,246,0,0,1,4,15,1,40,246,0,0,17,1,80,95,0,0,1,4,15,1,40,246,0,0,17,1,248,205,0,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,40,246,0,0,1,4,15,1,40,246, 
  0,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,40,246,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,40,246,0,0,1,4,15,1,40,246,0,0,17,1,122,3,0,0, 
  1,4,15,1,40,246,0,0,17,1,255,202,0,0,1,2,21,0,190,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112,16,200,132,113,128,199,132,114,240,6,128,46,240,12,128,68,128,12,131,69,24,12,131,70,176, 
  139,129,87,0,202,130,136,80,3,128,89,152,9,128,90,48,9,128,75,32,11,129,86,144,10,128,13,96,13,128,30,88,77,125,91,160,8,128,116,96,6,128,117,208,5,128,119,64,197,128,128,176,4,128,129,72,4,128,135,224, 
  3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,40,246,0,0,1,15,1,40,246,0,0,17,1,0,252,0,0,1,15,1,40,246,0,0,17,1,106,251,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,40, 
  246,0,0,1,19,90,0,0,0,252,0,0,0,1,0,17,1,40,246,0,0,1,19,89,0,0,0,248,0,0,0,1,0,17,1,40,246,0,0,1,19,89,0,0,0,247,0,0,0,1,0,17,1,40,246,0,0,1,19,86, 
  0,0,0,242,0,0,0,1,0,17,1,40,246,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,40,246,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,40,246,0,0,1,19,70,0,0,0,208,0,0, 
  0,1,0,17,1,40,246,0,0,1,15,1,40,246,0,0,17,1,34,242,0,0,1,15,1,40,246,0,0,17,1,231,247,0,0,1,19,69,0,0,0,204,0,0,0,1,0,17,1,40,246,0,0,1,19,135,0,0,0,122, 
  1,0,0,1,0,17,1,40,246,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,40,246,0,0,1,15,1,40,246,0,0,17,1,216,229,0,0,1,15,1,40,246,0,0,17,1,198,208,0,0,1,19,30,0,0, 
  0,89,0,0,0,6,0,14,14,1,15,1,40,246,0,0,17,1,147,208,0,0,1,1,19,70,0,0,0,207,0,0,0,1,0,17,1,40,246,0,0,1,2,21,1,212,0,0,0,167,9,2,0,13,0,0,0,3,0,0, 
  0,104,184,4,128,1,48,194,129,2,48,2,128,91,168,2,128,52,40,5,128,45,24,3,129,6,48,2,128,7,48,194,128,57,56,2,129,85,136,3,128,95,32,68,128,103,152,5,128,121,8,6,128,10,6,15,1,20,250,0, 
  0,17,1,199,248,0,0,1,4,15,1,20,250,0,0,17,1,160,10,0,0,1,4,15,1,20,250,0,0,17,1,164,8,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,20,250,0,0,1,4,19,119,0,0, 
  0,86,1,0,0,1,0,17,1,20,250,0,0,1,4,15,1,20,250,0,0,17,1,80,95,0,0,1,4,15,1,20,250,0,0,17,1,202,7,0,0,1,4,15,1,20,250,0,0,17,1,212,80,0,0,1,4,19,128,0, 
  0,0,97,1,0,0,1,0,17,1,20,250,0,0,1,19,90,0,0,0,253,0,0,0,1,0,1,21,1,76,1,0,0,139,10,2,0,29,0,0,0,4,0,0,0,112,224,8,128,1,152,197,132,2,152,5,128,3,48,68, 
  133,4,48,68,131,85,224,6,128,6,152,133,130,7,152,197,131,24,160,200,131,25,152,68,130,10,88,5,132,27,32,135,130,124,32,9,128,45,160,133,129,94,216,4,128,95,160,73,131,22,224,7,128,52,160,7,128,57,32,198, 
  129,61,24,5,128,81,96,7,128,91,224,137,129,103,32,8,128,104,96,9,128,115,32,10,128,121,96,6,128,122,160,6,128,123,96,8,128,127,224,5,128,12,19,90,0,0,0,253,0,0,0,1,0,1,12,17,1,49,220,0, 
  0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,10,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12, 
  17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220, 
  0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,12,17,1,49,220,0,0,1,2, 
  21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,24,198,131,113,24,202,131,114,32,9,128,86,56,3,128,116,56,7,128,117,200,3,128,70,152,71,127,119,40,132,130,136,136,5,128,89,128,5,128,90,128, 
  5,128,75,168,70,128,91,0,8,128,13,136,4,128,46,24,5,128,128,144,8,128,129,176,9,128,135,208,2,128,15,1,20,250,0,0,17,1,149,28,0,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,20,250,0,0, 
  1,19,89,0,0,0,250,0,0,0,2,0,1,19,90,0,0,0,251,0,0,0,2,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,20,250,0,0,1,15,1,20,250,0,0,17,1,13,14,0,0,1,1,19,135,0, 
  0,0,121,1,0,0,1,0,17,1,20,250,0,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,20,250,0,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,20,250,0,0,1,19,89,0,0,0,249,0,0,0, 
  2,0,1,15,1,20,250,0,0,17,1,83,33,0,0,1,19,70,0,0,0,208,0,0,0,1,0,17,1,20,250,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,20,250,0,0,1,19,86,0,0,0,242,0,0, 
  0,1,0,17,1,20,250,0,0,1,15,1,20,250,0,0,17,1,194,19,0,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,20,250,0,0,1,2,21,1,69,0,0,0,60,12,2,0,5,0,0,0,2,0,0,0, 
  112,232,1,128,1,224,1,128,2,224,129,128,115,48,1,128,10,112,1,128,4,17,1,106,26,0,0,1,4,15,1,252,223,0,0,17,1,143,24,0,0,1,8,4,17,1,187,251,0,0,1,19,116,0,0,0,81,1,0,0, 
  1,0,1,21,1,57,0,0,0,9,15,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,128,1,128,2,128,129,127,115,136,1,128,4,15,1,228,224,0,0,17,1,88,20,0,0,1,8,4,17,1,48,23,0,0,1, 
  19,116,0,0,0,71,1,0,0,2,0,1,21,1,80,0,0,0,248,15,2,0,6,0,0,0,2,0,0,0,124,216,1,128,1,80,193,128,2,80,193,128,123,88,1,128,25,152,1,128,122,24,2,128,10,4,17,1,246,28, 
  0,0,1,6,17,1,163,225,0,0,1,4,17,1,73,30,0,0,1,4,19,129,0,0,0,99,1,0,0,2,0,1,19,129,0,0,0,106,1,0,0,1,0,1,21,1,47,0,0,0,130,232,1,0,5,0,0,0,2,0, 
  0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,55,56,1,128,8,4,17,1,140,252,0,0,1,2,21,1,52,0,0,0,34,18,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48, 
  129,127,7,48,65,128,83,56,1,128,8,4,19,71,0,0,0,211,0,0,0,3,0,1,2,21,1,47,0,0,0,180,18,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,55, 
  48,1,128,4,17,1,252,252,0,0,1,8,19,71,0,0,0,210,0,0,0,1,0,1,21,1,52,0,0,0,30,19,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56, 
  1,128,8,4,19,71,0,0,0,209,0,0,0,3,0,1,2,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8, 
  4,19,46,0,0,0,143,0,0,0,1,0,17,1,126,253,0,0,1,4,15,1,126,253,0,0,17,1,122,3,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,58,40,2,129,13,152,2,128, 
  46,48,130,127,91,48,1,128,70,192,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,126,253,0,0,1,15,1,126,253,0,0,17,1,23,254,0,0,1,1,15,1,126,253,0,0,17,1,228,253,0,0,1,19,70,0, 
  0,0,207,0,0,0,1,0,17,1,126,253,0,0,1,2,21,1,39,0,0,0,146,19,2,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0, 
  41,0,0,0,1,0,1,21,1,59,0,0,0,115,20,2,0,6,0,0,0,2,0,0,0,76,80,1,128,1,208,1,128,2,208,129,128,7,208,129,128,6,208,1,128,75,144,1,128,4,17,1,195,254,0,0,1,4,17,1, 
  95,254,0,0,1,8,19,58,0,0,0,181,0,0,0,2,0,14,1,21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19, 
  46,0,0,0,143,0,0,0,1,0,17,1,154,254,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,64,128,58,208,0,128,1,19,58,0,0,0,179,0,0,0,4,0,14,1,2,21, 
  1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,254,254,0,0,1,2,21,0, 
  40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,64,128,58,208,0,128,1,19,58,0,0,0,180,0,0,0,4,0,14,1,2,21,1,104,0,0,0,55,21,2,0,7,0,0,0,2,0,0,0,6,112,65, 
  129,1,112,193,128,2,112,129,127,7,112,193,128,17,168,2,128,18,16,2,128,19,120,1,128,8,4,19,63,0,0,0,192,0,0,0,1,0,17,1,144,255,0,0,1,4,19,63,0,0,0,193,0,0,0,1,0,17,1,144, 
  255,0,0,1,4,19,63,0,0,0,191,0,0,0,1,0,17,1,144,255,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,63,208,0,128,61,56,193,127,15,1,144,255,0,0,17,1,185,255, 
  0,0,1,1,2,21,1,94,0,0,0,55,21,2,0,7,0,0,0,2,0,0,0,6,112,65,129,1,176,194,128,2,112,129,127,7,112,193,128,17,72,2,128,18,224,1,128,19,120,1,128,8,4,19,63,0,0,0,195,0, 
  0,0,2,0,1,4,19,63,0,0,0,196,0,0,0,2,0,1,4,19,63,0,0,0,194,0,0,0,2,0,1,4,17,1,24,0,1,0,1,2,21,1,59,0,0,0,230,21,2,0,6,0,0,0,2,0,0,0,76,88, 
  1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,75,152,1,128,8,4,17,1,184,0,1,0,1,4,17,1,84,0,1,0,1,2,21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48, 
  1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,143,0,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,0, 
  128,61,208,0,128,1,19,61,0,0,0,186,0,0,0,5,0,14,1,2,21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4, 
  19,46,0,0,0,143,0,0,0,1,0,17,1,243,0,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,0,128,61,208,0,128,1,19,61,0,0,0,187,0,0,0,5,0,14,1,2, 
  21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,87,1,1,0,1,2,21, 
  0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,0,128,65,208,0,128,1,19,65,0,0,0,199,0,0,0,2,0,1,2,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1, 
  129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,175,1,1,0,1,2,21,1,135,0,0,0,38,244,1,0,9,0,0,0,3,0,0,0,104,80,2,128,1,176,193,129,2,176,1,128,91,48,3, 
  128,103,192,2,128,85,160,3,128,6,176,1,128,7,176,65,127,121,184,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,20,3,1,0,1,4,15,1,20,3,1,0,17,1,80,95,0,0,1,4,15,1,20,3, 
  1,0,17,1,55,2,1,0,1,4,15,1,20,3,1,0,17,1,87,93,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,20,3,1,0,1,2,21,1,68,0,0,0,85,201,1,0,4,0,0,0,2,0,0, 
  0,52,176,1,128,1,16,129,128,2,16,1,128,57,24,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,124,2,1,0,1,4,15,1,124,2,1,0,17,1,122,3,0,0,1,2,21,0,100,0,0,0,255,255, 
  255,255,5,0,0,0,2,0,0,0,70,200,1,128,13,144,194,128,46,40,130,127,91,56,1,128,113,48,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,124,2,1,0,1,19,113,0,0,0,49,1,0,0,2,0, 
  1,15,1,124,2,1,0,17,1,225,2,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,124,2,1,0,1,2,21,1,39,0,0,0,131,22,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94, 
  248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1,21,0,172,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,64,195,129,113,176,2,128,66,88,197,128,67,240,68,128,75,96, 
  4,128,114,32,2,128,86,208,3,128,128,144,1,128,19,112,0,0,0,48,1,0,0,1,0,17,1,20,3,1,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,20,3,1,0,1,19,86,0,0,0,241,0,0,0,1, 
  0,17,1,20,3,1,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,20,3,1,0,1,19,67,0,0,0,201,0,0,0,1,0,17,1,20,3,1,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,20,3,1, 
  0,1,15,1,20,3,1,0,17,1,193,3,1,0,1,1,2,21,1,153,0,0,0,136,23,2,0,10,0,0,0,3,0,0,0,104,112,2,128,1,208,1,130,2,208,1,128,11,88,68,128,91,80,3,128,85,192,3,128,6, 
  208,1,128,7,208,65,128,103,224,2,128,121,216,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,160,4,1,0,1,4,15,1,160,4,1,0,17,1,80,95,0,0,1,4,15,1,160,4,1,0,17,1,91,4, 
  1,0,1,4,15,1,160,4,1,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,160,4,1,0,1,4,19,66,0,0,0,200,0,0,0,4,0,14,1,2,21,1,68,0,0,0,85,201,1, 
  0,4,0,0,0,2,0,0,0,52,176,1,128,1,16,129,128,2,16,1,128,57,24,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,124,2,1,0,1,4,15,1,124,2,1,0,17,1,202,7,0,0,1,2, 
  21,0,149,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,112,32,131,129,113,144,2,128,86,176,195,128,67,160,68,128,75,16,4,128,114,0,2,128,128,112,1,128,19,112,0,0,0,48,1,0,0,1,0,17,1,160, 
  4,1,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,160,4,1,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,160,4,1,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,160,4,1,0,1,19,67, 
  0,0,0,202,0,0,0,2,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,160,4,1,0,1,1,2,21,0,60,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,0,80,9,128,65,0,3,128,26,128,135,130, 
  27,240,6,128,4,72,73,129,5,224,200,128,30,64,69,129,7,120,8,128,13,16,136,128,28,96,6,128,29,208,197,128,46,176,4,128,58,32,132,128,61,144,3,128,66,112,2,128,19,26,0,0,0,85,0,0,0,1,0,17, 
  1,54,5,1,0,1,19,26,0,0,0,84,0,0,0,1,0,17,1,54,5,1,0,1,19,26,0,0,0,83,0,0,0,1,0,17,1,54,5,1,0,1,19,26,0,0,0,82,0,0,0,1,0,17,1,54,5,1,0,1, 
  19,13,0,0,0,41,0,0,0,1,0,17,1,54,5,1,0,1,19,7,0,0,0,20,0,0,0,1,0,17,1,54,5,1,0,1,19,7,0,0,0,18,0,0,0,1,0,17,1,54,5,1,0,1,19,7,0,0,0,17, 
  0,0,0,1,0,17,1,54,5,1,0,1,19,7,0,0,0,16,0,0,0,1,0,17,1,54,5,1,0,1,19,5,0,0,0,9,0,0,0,1,0,17,1,54,5,1,0,1,15,1,54,5,1,0,17,1,35,22,1,0, 
  1,15,1,54,5,1,0,17,1,149,21,1,0,1,15,1,54,5,1,0,17,1,115,6,1,0,1,1,19,7,0,0,0,19,0,0,0,1,0,17,1,54,5,1,0,1,2,21,1,202,0,0,0,69,247,1,0,13,0,0, 
  0,3,0,0,0,24,112,5,131,1,48,194,129,2,48,2,128,27,144,4,128,22,224,133,129,77,136,3,128,6,48,130,127,7,48,2,129,25,0,69,128,57,248,3,128,78,24,3,128,79,168,2,128,80,56,2,128,8,4,15, 
  1,241,8,1,0,17,1,57,8,1,0,1,4,15,1,241,8,1,0,17,1,28,1,1,0,1,4,15,1,241,8,1,0,17,1,39,255,0,0,1,4,15,1,241,8,1,0,17,1,62,7,1,0,1,4,19,46,0,0,0, 
  143,0,0,0,1,0,17,1,241,8,1,0,1,4,15,1,241,8,1,0,17,1,85,243,0,0,1,4,15,1,241,8,1,0,17,1,193,234,0,0,1,4,15,1,241,8,1,0,17,1,40,226,0,0,1,4,15,1,241,8, 
  1,0,17,1,123,201,0,0,1,2,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143, 
  0,0,0,1,0,17,1,139,7,1,0,1,4,15,1,139,7,1,0,17,1,202,7,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,58,40,2,129,13,152,2,128,46,48,130,127,91,48,1, 
  128,70,192,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,139,7,1,0,1,15,1,139,7,1,0,17,1,241,7,1,0,1,1,15,1,139,7,1,0,17,1,228,253,0,0,1,19,70,0,0,0,207,0,0,0,1, 
  0,17,1,139,7,1,0,1,2,21,1,59,0,0,0,115,20,2,0,6,0,0,0,2,0,0,0,76,144,1,128,1,208,1,128,2,208,129,128,7,208,129,128,6,208,1,128,75,80,1,128,4,17,1,95,254,0,0,1,4, 
  17,1,195,254,0,0,1,8,19,58,0,0,0,181,0,0,0,2,0,14,1,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8, 
  4,17,1,105,8,1,0,1,2,21,1,135,0,0,0,38,244,1,0,9,0,0,0,3,0,0,0,104,80,2,128,1,176,193,129,2,176,1,128,91,48,3,128,103,192,2,128,85,160,3,128,6,176,1,128,7,176,65,127,121, 
  184,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,20,3,1,0,1,4,15,1,20,3,1,0,17,1,80,95,0,0,1,4,15,1,20,3,1,0,17,1,91,4,1,0,1,4,15,1,20,3,1,0,17,1, 
  160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,20,3,1,0,1,2,21,0,41,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,0,216,5,128,65,184,8,128,26,104,134,130,27,8,3,128,4, 
  0,67,129,5,0,195,128,30,112,66,129,7,48,7,128,13,200,134,128,28,40,8,128,29,40,196,128,46,152,3,128,58,184,132,128,61,72,5,128,66,152,7,128,19,7,0,0,0,20,0,0,0,1,0,17,1,241,8,1,0, 
  1,1,19,7,0,0,0,16,0,0,0,1,0,17,1,241,8,1,0,1,19,13,0,0,0,41,0,0,0,1,0,17,1,241,8,1,0,1,19,7,0,0,0,18,0,0,0,1,0,17,1,241,8,1,0,1,19,26,0,0, 
  0,82,0,0,0,1,0,17,1,241,8,1,0,1,19,26,0,0,0,83,0,0,0,1,0,17,1,241,8,1,0,1,19,7,0,0,0,19,0,0,0,1,0,17,1,241,8,1,0,1,19,5,0,0,0,10,0,0,0,2, 
  0,1,15,1,241,8,1,0,17,1,35,22,1,0,1,15,1,241,8,1,0,17,1,27,10,1,0,1,19,26,0,0,0,85,0,0,0,1,0,17,1,241,8,1,0,1,19,7,0,0,0,17,0,0,0,1,0,17,1,241, 
  8,1,0,1,19,26,0,0,0,84,0,0,0,1,0,17,1,241,8,1,0,1,2,21,1,130,0,0,0,12,24,2,0,9,0,0,0,3,0,0,0,24,48,3,128,1,176,1,129,2,176,1,128,27,80,2,128,22,160,3, 
  128,25,192,194,128,6,176,129,127,7,176,1,128,57,184,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,6,21,1,0,1,4,15,1,6,21,1,0,17,1,152,18,1,0,1,4,15,1,6,21,1,0,17,1, 
  171,16,1,0,1,4,15,1,6,21,1,0,17,1,39,15,1,0,1,4,15,1,6,21,1,0,17,1,170,10,1,0,1,19,4,0,0,0,7,0,0,0,2,0,14,1,21,1,58,0,0,0,237,154,1,0,5,0,0,0, 
  2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,229,10,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,46,240,0,128,13,136,65,128,27,128,1,128,19,13,0,0,0,41,0,0,0,1,0,17,1,229,10,1,0,1,1,15,1,229,10,1,0,17,1,36,11,1,0,1,2,21,1,47,0,0,0,7,253,1,0,5,0, 
  0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,84,11,1,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48, 
  194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17, 
  1,213,206,0,0,1,4,15,1,213,206,0,0,17,1,80,95,0,0,1,4,15,1,213,206,0,0,17,1,91,4,1,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,213,206,0,0,1,4,15,1,213,206,0,0, 
  17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,213,206,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,213,206,0,0,1,4,15,1,213,206,0,0,17,1,202,7,0,0,1,4, 
  15,1,213,206,0,0,17,1,46,12,1,0,1,2,21,1,176,0,0,0,37,171,1,0,11,0,0,0,3,0,0,0,104,144,2,128,1,240,193,129,2,240,1,128,91,112,3,128,52,16,5,128,85,224,3,128,6,240,1,128, 
  7,240,129,128,57,120,132,128,103,0,3,128,121,248,1,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,223,12,1,0,1,4,15,1,223,12,1,0,17,1,80,95,0,0,1,4,15,1,223,12,1,0,17,1,212, 
  80,0,0,1,4,15,1,223,12,1,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,223,12,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,223,12,1,0,1,4,15,1,223, 
  12,1,0,17,1,202,7,0,0,1,2,21,0,72,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,112,160,6,131,113,16,134,131,114,128,5,128,86,192,199,129,116,240,4,128,117,232,4,128,70,224,72,127,135,32,3, 
  128,136,144,2,128,91,48,7,128,118,128,4,128,75,80,136,127,128,240,3,128,13,176,9,128,46,72,9,128,129,136,3,128,19,135,0,0,0,121,1,0,0,1,0,17,1,223,12,1,0,1,15,1,223,12,1,0,17,1,28, 
  92,0,0,1,15,1,223,12,1,0,17,1,134,91,0,0,1,19,112,0,0,0,48,1,0,0,1,0,17,1,223,12,1,0,1,15,1,223,12,1,0,17,1,40,14,1,0,1,1,19,118,0,0,0,84,1,0,0,1,0, 
  17,1,223,12,1,0,1,19,86,0,0,0,242,0,0,0,1,0,17,1,223,12,1,0,1,19,86,0,0,0,241,0,0,0,1,0,17,1,223,12,1,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,223,12,1,0, 
  1,19,70,0,0,0,208,0,0,0,1,0,17,1,223,12,1,0,1,19,135,0,0,0,122,1,0,0,1,0,17,1,223,12,1,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,223,12,1,0,1,15,1,223,12,1, 
  0,17,1,220,6,0,0,1,15,1,223,12,1,0,17,1,78,6,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,223,12,1,0,1,2,21,1,188,0,0,0,248,203,1,0,12,0,0,0,3,0,0,0,104,176, 
  2,128,1,16,2,130,2,16,2,128,91,144,3,128,52,48,5,128,85,0,4,128,6,16,130,128,7,16,194,128,46,160,5,128,57,152,132,128,103,32,3,128,121,24,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17, 
  1,241,87,0,0,1,4,15,1,241,87,0,0,17,1,80,95,0,0,1,4,15,1,241,87,0,0,17,1,212,80,0,0,1,4,15,1,241,87,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0, 
  17,1,241,87,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,241,87,0,0,1,4,15,1,241,87,0,0,17,1,202,7,0,0,1,4,17,1,229,14,1,0,1,2,21,1,53,0,0,0,148,24,2,0,5, 
  0,0,0,2,0,0,0,116,48,1,128,1,160,1,128,2,160,129,128,7,160,1,128,6,160,1,128,4,19,117,0,0,0,82,1,0,0,4,0,14,1,8,19,117,0,0,0,83,1,0,0,3,0,14,1,21,1,58,0,0, 
  0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,98,15,1,0,1,2,21,0,62,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,28,128,129,128,13,136,1,128,46,240,0,128,19,13,0,0,0,41,0,0,0,1,0,17,1,98,15,1,0,1,1,15,1,98,15,1,0,17,1,161,15,1,0,1,2,21,1,47, 
  0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,209,15,1,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3, 
  0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0, 
  0,0,97,1,0,0,1,0,17,1,172,227,0,0,1,4,15,1,172,227,0,0,17,1,80,95,0,0,1,4,15,1,172,227,0,0,17,1,91,4,1,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,172,227,0, 
  0,1,4,15,1,172,227,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,172,227,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,172,227,0,0,1,4,15,1,172,227,0, 
  0,17,1,202,7,0,0,1,4,15,1,172,227,0,0,17,1,46,12,1,0,1,2,21,1,47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1, 
  128,8,4,17,1,219,16,1,0,1,2,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0, 
  143,0,0,0,1,0,17,1,40,17,1,0,1,4,15,1,40,17,1,0,17,1,202,7,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,29,144,2,128,13,152,194,127,46,40,130,128,91,48, 
  1,128,70,192,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,40,17,1,0,1,15,1,40,17,1,0,17,1,142,17,1,0,1,15,1,40,17,1,0,17,1,164,235,0,0,1,1,19,70,0,0,0,207,0,0,0, 
  1,0,17,1,40,17,1,0,1,2,21,1,47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,190,17,1,0,1,2,21,1, 
  217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88,6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128, 
  103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,225,236,0,0,1,4,15,1,225,236,0,0,17,1,80,95,0,0,1,4,15,1,225,236,0,0,17,1,91,4,1,0,1,4,19,119,0, 
  0,0,86,1,0,0,1,0,17,1,225,236,0,0,1,4,15,1,225,236,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,225,236,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0, 
  17,1,225,236,0,0,1,4,15,1,225,236,0,0,17,1,202,7,0,0,1,4,15,1,225,236,0,0,17,1,46,12,1,0,1,2,21,1,76,0,0,0,82,17,2,0,6,0,0,0,2,0,0,0,56,240,1,128,1,80, 
  1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,229,18,1,0,1,4,15,1,229,18,1,0,17,1,47,10,0,0,1,2,21,0,96,0,0,0,255, 
  255,255,255,5,0,0,0,2,0,0,0,46,192,1,128,45,40,2,128,30,248,130,127,31,144,66,128,71,48,1,128,19,31,0,0,0,90,0,0,0,1,0,17,1,229,18,1,0,1,15,1,229,18,1,0,17,1,193,252,0, 
  0,1,15,1,229,18,1,0,17,1,92,252,0,0,1,15,1,229,18,1,0,17,1,70,19,1,0,1,1,2,21,1,59,0,0,0,195,17,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,193,128,2,80,129,127, 
  7,80,1,128,9,152,65,128,21,88,1,128,8,4,17,1,130,19,1,0,1,4,17,1,63,244,0,0,1,2,21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127, 
  7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,189,19,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,30,128,129,128,13,136,1,128,46,240,0,128,19, 
  13,0,0,0,41,0,0,0,1,0,17,1,189,19,1,0,1,1,15,1,189,19,1,0,17,1,252,19,1,0,1,2,21,1,47,0,0,0,7,253,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48, 
  129,127,7,48,1,128,21,56,1,128,8,4,17,1,44,20,1,0,1,2,21,1,217,0,0,0,213,167,1,0,13,0,0,0,3,0,0,0,104,208,2,128,1,48,194,129,2,48,2,128,91,72,4,128,52,232,5,128,45,88, 
  6,129,6,48,2,128,7,48,194,128,57,80,5,129,85,184,4,128,95,176,67,128,103,64,3,128,121,56,2,128,8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,40,246,0,0,1,4,15,1,40,246,0,0,17,1,80, 
  95,0,0,1,4,15,1,40,246,0,0,17,1,91,4,1,0,1,4,19,119,0,0,0,86,1,0,0,1,0,17,1,40,246,0,0,1,4,15,1,40,246,0,0,17,1,160,10,0,0,1,4,19,75,0,0,0,215,0,0, 
  0,1,0,17,1,40,246,0,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,40,246,0,0,1,4,15,1,40,246,0,0,17,1,202,7,0,0,1,4,15,1,40,246,0,0,17,1,46,12,1,0,1,2,21,0, 
  142,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,0,16,4,128,29,128,2,128,46,144,1,128,27,64,3,128,28,224,2,128,13,160,3,127,30,32,2,127,7,8,4,128,19,13,0,0,0,41,0,0,0,1,0,17, 
  1,6,21,1,0,1,19,7,0,0,0,25,0,0,0,2,0,1,19,7,0,0,0,23,0,0,0,2,0,1,19,7,0,0,0,22,0,0,0,2,0,1,19,7,0,0,0,21,0,0,0,2,0,1,15,1,6,21,1,0, 
  17,1,35,22,1,0,1,1,19,7,0,0,0,24,0,0,0,2,0,1,2,21,1,130,0,0,0,12,24,2,0,9,0,0,0,3,0,0,0,24,48,3,128,1,176,1,129,2,176,1,128,27,80,2,128,22,160,3,128,25, 
  192,194,128,6,176,129,127,7,176,1,128,57,184,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,6,21,1,0,1,4,15,1,6,21,1,0,17,1,152,18,1,0,1,4,15,1,6,21,1,0,17,1,171,16, 
  1,0,1,4,15,1,6,21,1,0,17,1,39,15,1,0,1,4,15,1,6,21,1,0,17,1,170,10,1,0,1,19,4,0,0,0,8,0,0,0,1,0,1,21,1,59,0,0,0,49,157,1,0,6,0,0,0,2,0,0, 
  0,4,88,1,128,1,80,1,128,2,80,129,128,3,152,129,128,6,80,1,128,7,80,1,128,8,4,17,1,208,141,1,0,1,4,17,1,95,22,1,0,1,2,21,1,69,1,0,0,206,157,1,0,19,0,0,0,4,0,0, 
  0,96,72,4,128,1,240,130,128,2,240,66,130,65,176,8,128,100,216,3,128,69,208,7,131,6,240,130,129,7,240,2,128,88,232,5,128,73,240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5,128,62,184,9, 
  128,63,72,9,128,90,184,4,128,101,104,3,128,102,248,2,128,8,4,15,1,32,132,1,0,17,1,4,130,1,0,1,4,15,1,32,132,1,0,17,1,31,129,1,0,1,4,15,1,32,132,1,0,17,1,58,128,1,0,1, 
  4,15,1,32,132,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,32,132,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,32,132,1,0,1,4,19,85,0,0,0,236,0, 
  0,0,1,0,17,1,32,132,1,0,1,4,15,1,32,132,1,0,17,1,228,162,0,0,1,4,15,1,32,132,1,0,17,1,158,162,0,0,1,4,15,1,32,132,1,0,17,1,6,162,0,0,1,4,15,1,32,132,1,0, 
  17,1,48,42,1,0,1,4,15,1,32,132,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,32,132,1,0,1,4,15,1,32,132,1,0,17,1,106,41,1,0,1,4,15,1,32,132,1, 
  0,17,1,165,23,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0, 
  17,1,224,23,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,56,1,128,15,1,224,23,1,0,17,1,9,24,1,0,1,1,2,21,1,111,0,0,0,255,164,1,0,8, 
  0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144,1,128,57,112,2,128,52,8,67,128,84,152,1,128,6,144,1,128,7,144,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,85,33,1,0,1,4, 
  17,1,121,24,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,85,33,1,0,1,4,15,1,85,33,1,0,17,1,202,7,0,0,1,2,21,1,47,0,0,0,116,224,1,0,5,0,0,0,2,0,0,0,6, 
  48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,17,1,169,24,1,0,1,2,21,1,99,0,0,0,231,224,1,0,7,0,0,0,2,0,0,0,52,168,130,129,1,112,1,129,2,112,129,128,7, 
  112,1,128,6,112,1,128,57,16,2,128,84,120,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,13,25,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,13,25,1,0,1,4,15,1,13,25,1, 
  0,17,1,202,7,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128,46,184,130,127,47,176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0, 
  1,0,17,1,13,25,1,0,1,15,1,13,25,1,0,17,1,79,32,1,0,1,15,1,13,25,1,0,17,1,183,25,1,0,1,1,15,1,13,25,1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0, 
  17,1,13,25,1,0,1,2,21,1,39,0,0,0,131,25,2,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1,21, 
  1,47,0,0,0,216,25,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,61,48,1,128,4,17,1,244,25,1,0,1,8,19,47,0,0,0,150,0,0,0,5,0,14,14,1, 
  21,1,213,1,0,0,148,189,1,0,27,0,0,0,4,0,0,0,32,88,77,133,1,240,195,131,2,240,195,131,35,8,12,128,36,152,11,133,37,40,11,133,6,240,3,131,7,240,3,131,40,216,9,128,41,104,9,128,10,56, 
  142,130,43,96,8,128,44,200,7,128,45,88,7,128,30,200,13,128,47,232,198,130,33,232,140,129,34,120,12,128,38,184,74,129,39,72,10,128,42,248,8,128,48,120,6,128,49,8,6,128,54,152,5,128,84,0,5,128,85,104, 
  4,128,95,248,3,128,8,4,15,1,251,30,1,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,251,30,1,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,251,30,1,0,1,4, 
  15,1,251,30,1,0,17,1,7,71,0,0,1,4,15,1,251,30,1,0,17,1,148,70,0,0,1,4,15,1,251,30,1,0,17,1,33,70,0,0,1,4,15,1,251,30,1,0,17,1,105,68,0,0,1,4,15,1,251,30, 
  1,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,251,30,1,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,251,30,1,0,1,4,15,1,251,30,1,0,17,1,246,59,0,0, 
  1,4,15,1,251,30,1,0,17,1,131,59,0,0,1,4,15,1,251,30,1,0,17,1,16,59,0,0,1,4,15,1,251,30,1,0,17,1,157,58,0,0,1,4,15,1,251,30,1,0,17,1,42,58,0,0,1,4,15,1, 
  251,30,1,0,17,1,183,57,0,0,1,4,15,1,251,30,1,0,17,1,68,57,0,0,1,4,15,1,251,30,1,0,17,1,209,56,0,0,1,4,15,1,251,30,1,0,17,1,94,56,0,0,1,4,15,1,251,30,1,0, 
  17,1,235,55,0,0,1,4,15,1,251,30,1,0,17,1,120,55,0,0,1,4,15,1,251,30,1,0,17,1,251,53,0,0,1,4,15,1,251,30,1,0,17,1,202,27,1,0,1,2,21,1,222,1,0,0,27,194,1,0, 
  26,0,0,0,4,0,0,0,32,168,205,132,1,208,195,131,2,208,195,131,35,88,12,128,36,232,203,132,37,120,203,132,38,8,203,131,39,152,10,128,40,40,138,131,41,184,9,128,42,72,9,128,11,136,206,129,44,24,8,128, 
  45,168,7,128,30,24,14,128,47,56,135,130,33,56,13,129,34,200,12,128,43,176,8,128,48,200,6,128,49,88,6,128,54,232,5,128,56,120,5,128,84,224,4,128,85,72,4,128,95,216,3,128,8,4,15,1,169,29,1,0, 
  17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,169,29,1,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,169,29,1,0,1,4,15,1,169,29,1,0,17,1,47,10,0,0,1,4, 
  15,1,169,29,1,0,17,1,7,71,0,0,1,4,15,1,169,29,1,0,17,1,148,70,0,0,1,4,15,1,169,29,1,0,17,1,33,70,0,0,1,4,15,1,169,29,1,0,17,1,105,68,0,0,1,4,15,1,169,29, 
  1,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,169,29,1,0,1,4,19,37,0,0,0,118,0,0,0,1,0,17,1,169,29,1,0,1,4,15,1,169,29,1,0,17,1,246,59,0,0, 
  1,4,15,1,169,29,1,0,17,1,131,59,0,0,1,4,15,1,169,29,1,0,17,1,16,59,0,0,1,4,15,1,169,29,1,0,17,1,157,58,0,0,1,4,15,1,169,29,1,0,17,1,42,58,0,0,1,4,15,1, 
  169,29,1,0,17,1,183,57,0,0,1,4,15,1,169,29,1,0,17,1,68,57,0,0,1,4,15,1,169,29,1,0,17,1,209,56,0,0,1,4,15,1,169,29,1,0,17,1,94,56,0,0,1,4,15,1,169,29,1,0, 
  17,1,235,55,0,0,1,4,15,1,169,29,1,0,17,1,120,55,0,0,1,4,15,1,169,29,1,0,17,1,251,53,0,0,1,4,19,80,0,0,0,225,0,0,0,2,0,1,2,21,0,81,1,0,0,255,255,255,255,17, 
  0,0,0,4,0,0,0,80,224,6,128,81,208,3,128,34,64,3,128,35,72,8,128,36,232,6,128,37,104,9,128,38,232,5,128,42,120,70,129,40,216,8,128,9,176,2,128,10,224,70,127,43,56,4,129,74,248,9,128,45, 
  120,199,128,78,88,5,128,75,200,4,128,93,224,7,128,19,81,0,0,0,226,0,0,0,1,0,17,1,169,29,1,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,169,29,1,0,1,15,1,169,29,1,0,17,1,191, 
  41,0,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,169,29,1,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,169,29,1,0,1,19,42,0,0,0,136,0,0,0,1,0,17,1,169,29,1,0,1,19,9, 
  0,0,0,32,0,0,0,1,0,17,1,169,29,1,0,1,15,1,169,29,1,0,17,1,217,50,0,0,1,1,19,9,0,0,0,30,0,0,0,1,0,17,1,169,29,1,0,1,15,1,169,29,1,0,17,1,237,44,0,0, 
  1,15,1,169,29,1,0,17,1,9,41,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,169,29,1,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,169,29,1,0,1,19,9,0,0,0,31,0,0,0,1, 
  0,17,1,169,29,1,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,169,29,1,0,1,2,21,0,83,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,80,248,2,128,42,176,133,129,34,232,8,128,35,88,8, 
  128,36,200,7,128,37,56,7,128,38,168,6,128,74,136,4,128,40,24,6,128,9,8,10,128,10,120,201,125,43,32,69,128,75,248,3,128,93,144,2,128,78,104,3,128,47,24,5,128,15,1,251,30,1,0,17,1,9,41,0, 
  0,1,19,47,0,0,0,148,0,0,0,7,0,14,14,1,19,42,0,0,0,136,0,0,0,1,0,17,1,251,30,1,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,251,30,1,0,1,19,37,0,0,0,120,0,0, 
  0,1,0,17,1,251,30,1,0,1,1,19,9,0,0,0,35,0,0,0,1,0,17,1,251,30,1,0,1,15,1,251,30,1,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,251,30,1,0,1, 
  19,9,0,0,0,32,0,0,0,1,0,17,1,251,30,1,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1,251,30,1,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,251,30,1,0,1,19,9,0,0,0,29, 
  0,0,0,1,0,17,1,251,30,1,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,251,30,1,0,1,19,80,0,0,0,222,0,0,0,1,0,17,1,251,30,1,0,1,19,80,0,0,0,223,0,0,0,1,0,17, 
  1,251,30,1,0,1,2,21,1,70,0,0,0,99,225,1,0,6,0,0,0,2,0,0,0,60,240,65,129,1,80,1,128,2,80,129,128,7,80,1,128,6,80,1,128,84,88,1,128,8,4,19,74,0,0,0,214,0,0,0, 
  1,0,17,1,43,33,1,0,1,4,17,1,150,32,1,0,1,2,21,1,47,0,0,0,32,226,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,59,56,1,128,8,4,17,1, 
  198,32,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,1, 
  33,1,0,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,64,1,128,19,47,0,0,0,144,0,0,0,8,0,14,14,1,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,74,208,0,128,47,64,1,128,19,47,0,0,0,146,0,0,0,6,0,14,14,1,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128,46,184,130,127,47, 
  176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,85,33,1,0,1,15,1,85,33,1,0,17,1,100,40,1,0,1,15,1,85,33,1,0,17,1,204,33,1,0,1,1,15,1,85,33, 
  1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,85,33,1,0,1,2,21,1,47,0,0,0,216,25,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112, 
  1,128,61,48,1,128,4,17,1,9,34,1,0,1,8,19,47,0,0,0,151,0,0,0,3,0,14,14,1,21,1,213,1,0,0,148,189,1,0,27,0,0,0,4,0,0,0,32,88,77,133,1,240,195,131,2,240,195,131,35, 
  8,12,128,36,152,11,133,37,40,11,133,6,240,3,131,7,240,3,131,40,216,9,128,41,104,9,128,10,56,142,130,43,96,8,128,44,200,7,128,45,88,7,128,30,200,13,128,47,232,198,130,33,232,140,129,34,120,12,128,38, 
  184,74,129,39,72,10,128,42,248,8,128,48,120,6,128,49,8,6,128,54,152,5,128,84,0,5,128,85,104,4,128,95,248,3,128,8,4,15,1,16,39,1,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0, 
  1,0,17,1,16,39,1,0,1,4,19,74,0,0,0,214,0,0,0,1,0,17,1,16,39,1,0,1,4,15,1,16,39,1,0,17,1,7,71,0,0,1,4,15,1,16,39,1,0,17,1,148,70,0,0,1,4,15,1,16, 
  39,1,0,17,1,33,70,0,0,1,4,15,1,16,39,1,0,17,1,105,68,0,0,1,4,15,1,16,39,1,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,16,39,1,0,1,4,19,37, 
  0,0,0,118,0,0,0,1,0,17,1,16,39,1,0,1,4,15,1,16,39,1,0,17,1,246,59,0,0,1,4,15,1,16,39,1,0,17,1,131,59,0,0,1,4,15,1,16,39,1,0,17,1,16,59,0,0,1,4,15, 
  1,16,39,1,0,17,1,157,58,0,0,1,4,15,1,16,39,1,0,17,1,42,58,0,0,1,4,15,1,16,39,1,0,17,1,183,57,0,0,1,4,15,1,16,39,1,0,17,1,68,57,0,0,1,4,15,1,16,39,1, 
  0,17,1,209,56,0,0,1,4,15,1,16,39,1,0,17,1,94,56,0,0,1,4,15,1,16,39,1,0,17,1,235,55,0,0,1,4,15,1,16,39,1,0,17,1,120,55,0,0,1,4,15,1,16,39,1,0,17,1,251, 
  53,0,0,1,4,15,1,16,39,1,0,17,1,223,35,1,0,1,2,21,1,222,1,0,0,27,194,1,0,26,0,0,0,4,0,0,0,32,168,205,132,1,208,195,131,2,208,195,131,35,88,12,128,36,232,203,132,37,120,203, 
  132,38,8,203,131,39,152,10,128,40,40,138,131,41,184,9,128,42,72,9,128,11,136,206,129,44,24,8,128,45,168,7,128,30,24,14,128,47,56,135,130,33,56,13,129,34,200,12,128,43,176,8,128,48,200,6,128,49,88,6, 
  128,54,232,5,128,56,120,5,128,84,224,4,128,85,72,4,128,95,216,3,128,8,4,15,1,190,37,1,0,17,1,120,77,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,190,37,1,0,1,4,19,74,0,0, 
  0,214,0,0,0,1,0,17,1,190,37,1,0,1,4,15,1,190,37,1,0,17,1,47,10,0,0,1,4,15,1,190,37,1,0,17,1,7,71,0,0,1,4,15,1,190,37,1,0,17,1,148,70,0,0,1,4,15,1,190, 
  37,1,0,17,1,33,70,0,0,1,4,15,1,190,37,1,0,17,1,105,68,0,0,1,4,15,1,190,37,1,0,17,1,105,60,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,17,1,190,37,1,0,1,4,19,37, 
  0,0,0,118,0,0,0,1,0,17,1,190,37,1,0,1,4,15,1,190,37,1,0,17,1,246,59,0,0,1,4,15,1,190,37,1,0,17,1,131,59,0,0,1,4,15,1,190,37,1,0,17,1,16,59,0,0,1,4,15, 
  1,190,37,1,0,17,1,157,58,0,0,1,4,15,1,190,37,1,0,17,1,42,58,0,0,1,4,15,1,190,37,1,0,17,1,183,57,0,0,1,4,15,1,190,37,1,0,17,1,68,57,0,0,1,4,15,1,190,37,1, 
  0,17,1,209,56,0,0,1,4,15,1,190,37,1,0,17,1,94,56,0,0,1,4,15,1,190,37,1,0,17,1,235,55,0,0,1,4,15,1,190,37,1,0,17,1,120,55,0,0,1,4,15,1,190,37,1,0,17,1,251, 
  53,0,0,1,4,19,80,0,0,0,225,0,0,0,2,0,1,2,21,0,81,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,80,96,4,128,81,32,10,128,34,0,9,128,35,88,6,128,36,120,7,128,37,176,2,128, 
  38,8,8,128,42,240,69,129,40,232,6,128,9,96,5,128,10,96,68,127,43,208,3,129,74,64,3,128,45,104,196,128,78,208,4,128,75,144,9,128,93,152,8,128,19,9,0,0,0,31,0,0,0,1,0,17,1,190,37,1, 
  0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,190,37,1,0,1,19,9,0,0,0,35,0,0,0,1,0,17,1,190,37,1,0,1,1,15,1,190,37,1,0,17,1,237,44,0,0,1,19,42,0,0,0,136,0,0, 
  0,1,0,17,1,190,37,1,0,1,19,81,0,0,0,226,0,0,0,1,0,17,1,190,37,1,0,1,15,1,190,37,1,0,17,1,217,50,0,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,190,37,1,0,1,19, 
  9,0,0,0,33,0,0,0,1,0,17,1,190,37,1,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,190,37,1,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,190,37,1,0,1,15,1,190,37,1,0,17, 
  1,9,41,0,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,190,37,1,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,190,37,1,0,1,15,1,190,37,1,0,17,1,191,41,0,0,1,2,21,0,83,1, 
  0,0,255,255,255,255,16,0,0,0,4,0,0,0,80,248,2,128,42,176,133,129,34,232,8,128,35,88,8,128,36,200,7,128,37,56,7,128,38,168,6,128,74,136,4,128,40,24,6,128,9,8,10,128,10,120,201,125,43,32, 
  69,128,75,248,3,128,93,144,2,128,78,104,3,128,47,24,5,128,15,1,16,39,1,0,17,1,9,41,0,0,1,19,47,0,0,0,149,0,0,0,5,0,14,14,1,19,42,0,0,0,136,0,0,0,1,0,17,1,16,39, 
  1,0,1,19,37,0,0,0,121,0,0,0,1,0,17,1,16,39,1,0,1,19,37,0,0,0,120,0,0,0,1,0,17,1,16,39,1,0,1,1,19,9,0,0,0,35,0,0,0,1,0,17,1,16,39,1,0,1,15,1, 
  16,39,1,0,17,1,217,50,0,0,1,19,9,0,0,0,33,0,0,0,1,0,17,1,16,39,1,0,1,19,9,0,0,0,32,0,0,0,1,0,17,1,16,39,1,0,1,19,9,0,0,0,31,0,0,0,1,0,17,1, 
  16,39,1,0,1,19,9,0,0,0,30,0,0,0,1,0,17,1,16,39,1,0,1,19,9,0,0,0,29,0,0,0,1,0,17,1,16,39,1,0,1,19,9,0,0,0,28,0,0,0,1,0,17,1,16,39,1,0,1,19, 
  80,0,0,0,222,0,0,0,1,0,17,1,16,39,1,0,1,19,80,0,0,0,223,0,0,0,1,0,17,1,16,39,1,0,1,2,21,1,70,0,0,0,99,225,1,0,6,0,0,0,2,0,0,0,60,240,65,129,1,80, 
  1,128,2,80,129,128,7,80,1,128,6,80,1,128,84,88,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,64,41,1,0,1,4,17,1,171,40,1,0,1,2,21,1,47,0,0,0,32,226,1,0,5,0,0, 
  0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,59,56,1,128,8,4,17,1,219,40,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1, 
  128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,22,41,1,0,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,64,1,128, 
  19,47,0,0,0,145,0,0,0,6,0,14,14,1,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,64,1,128,19,47,0,0,0,147,0,0,0,4,0,14,14,1,1,2,21,1, 
  58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,165,41,1,0,1,2,21,0,39, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,48,1,128,19,47,0,0,0,152,0,0,0,2,0,1,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1, 
  48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,8,42,1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,48, 
  1,128,19,47,0,0,0,153,0,0,0,2,0,1,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0, 
  0,214,0,0,0,1,0,17,1,107,42,1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,52,48,65,128,74,208,0,128,19,52,0,0,0,159,0,0,0,2,0,1,1,2,21,1,47,0,0, 
  0,130,232,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,55,56,1,128,8,4,17,1,195,42,1,0,1,2,21,1,82,0,0,0,214,232,1,0,7,0,0,0,2,0,0, 
  0,6,112,65,129,1,112,193,128,2,112,129,127,7,112,193,128,57,248,1,128,98,184,1,128,99,120,1,128,8,4,17,1,21,114,1,0,1,4,17,1,95,99,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1, 
  22,43,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,64,128,94,208,0,128,1,15,1,22,43,1,0,17,1,63,43,1,0,1,2,21,1,89,0,0,0,71,239,1,0,7,0,0, 
  0,2,0,0,0,52,232,1,128,1,112,1,128,2,112,129,128,7,112,193,128,6,112,65,128,10,88,2,128,55,120,1,128,8,4,15,1,135,98,1,0,17,1,36,98,1,0,1,4,15,1,135,98,1,0,17,1,159,85,1, 
  0,1,4,15,1,135,98,1,0,17,1,153,43,1,0,1,2,21,1,107,0,0,0,177,239,1,0,8,0,0,0,3,0,0,0,10,232,2,128,1,144,1,128,2,144,129,127,118,152,1,128,52,120,2,128,117,8,2,128,6, 
  144,65,127,7,144,1,128,8,4,15,1,110,84,1,0,17,1,164,83,1,0,1,4,15,1,110,84,1,0,17,1,178,69,1,0,1,4,15,1,110,84,1,0,17,1,159,85,1,0,1,4,15,1,110,84,1,0,17,1,5, 
  44,1,0,1,2,21,1,69,1,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,72,4,128,1,240,130,128,2,240,66,130,65,176,8,128,100,216,3,128,69,208,7,131,6,240,130,129,7,240,2,128,88,232,5,128,73, 
  240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5,128,62,184,9,128,63,72,9,128,90,184,4,128,101,104,3,128,102,248,2,128,8,4,15,1,27,48,1,0,17,1,4,130,1,0,1,4,15,1,27,48,1, 
  0,17,1,31,129,1,0,1,4,15,1,27,48,1,0,17,1,58,128,1,0,1,4,15,1,27,48,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,27,48,1,0,1,4,19,85,0,0, 
  0,237,0,0,0,1,0,17,1,27,48,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,27,48,1,0,1,4,15,1,27,48,1,0,17,1,228,162,0,0,1,4,15,1,27,48,1,0,17,1,158,162,0,0, 
  1,4,15,1,27,48,1,0,17,1,6,162,0,0,1,4,15,1,27,48,1,0,17,1,48,42,1,0,1,4,15,1,27,48,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,27,48,1, 
  0,1,4,15,1,27,48,1,0,17,1,106,41,1,0,1,4,15,1,27,48,1,0,17,1,75,45,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129, 
  128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,134,45,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,56,1,128,15,1,134,45, 
  1,0,17,1,175,45,1,0,1,1,2,21,1,111,0,0,0,255,164,1,0,8,0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144,1,128,57,112,2,128,52,8,67,128,84,152,1,128,6,144,1,128,7,144,1, 
  128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,103,47,1,0,1,4,17,1,31,46,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,103,47,1,0,1,4,15,1,103,47,1,0,17,1,202,7,0, 
  0,1,2,21,1,47,0,0,0,116,224,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,17,1,79,46,1,0,1,2,21,1,99,0,0,0,231,224,1, 
  0,7,0,0,0,2,0,0,0,52,168,130,129,1,112,1,129,2,112,129,128,7,112,1,128,6,112,1,128,57,16,2,128,84,120,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,179,46,1,0,1,4,19,46, 
  0,0,0,143,0,0,0,1,0,17,1,179,46,1,0,1,4,15,1,179,46,1,0,17,1,202,7,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128,46,184,130, 
  127,47,176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,179,46,1,0,1,15,1,179,46,1,0,17,1,79,32,1,0,1,15,1,179,46,1,0,17,1,42,47,1,0,1,1,15,1, 
  179,46,1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,179,46,1,0,1,2,21,1,47,0,0,0,57,26,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127, 
  7,48,1,128,61,56,1,128,8,4,17,1,244,25,1,0,1,19,47,0,0,0,150,0,0,0,5,0,14,14,1,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128,46,184,130, 
  127,47,176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,103,47,1,0,1,15,1,103,47,1,0,17,1,100,40,1,0,1,15,1,103,47,1,0,17,1,222,47,1,0,1,1,15,1, 
  103,47,1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,103,47,1,0,1,2,21,1,47,0,0,0,57,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127, 
  7,112,1,128,61,48,1,128,4,17,1,9,34,1,0,1,8,19,47,0,0,0,151,0,0,0,3,0,14,14,1,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,83,16,135,130,84,168,6,128,82,160,7, 
  128,19,32,74,127,52,40,73,127,53,152,200,128,54,48,8,128,103,104,4,128,85,24,6,128,94,136,5,128,99,248,4,128,107,216,3,128,108,72,3,129,109,184,2,128,14,136,202,126,47,144,9,128,124,176,2,128,1,19,83, 
  0,0,0,233,0,0,0,1,0,17,1,27,48,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,27,48,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,27,48,1,0,1,19,82,0,0,0,230,0,0, 
  0,1,0,17,1,27,48,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,27,48,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,27,48,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,27, 
  48,1,0,1,15,1,27,48,1,0,17,1,254,68,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,27,48,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,27,48,1,0,1,15,1,27,48,1,0,17, 
  1,95,57,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,27,48,1,0,1,15,1,27,48,1,0,17,1,116,55,1,0,1,19,19,0,0,0,62,0,0,0,1,0,17,1,27,48,1,0,1,15,1,27,48,1, 
  0,17,1,176,49,1,0,1,15,1,27,48,1,0,17,1,122,49,1,0,1,2,21,1,53,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128, 
  8,4,19,124,0,0,0,93,1,0,0,3,0,14,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,235,49,1, 
  0,1,8,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194, 
  129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,251,50,1,0,17,1,4,130,1,0,1,4,15,1,251,50,1,0,17, 
  1,31,129,1,0,1,4,15,1,251,50,1,0,17,1,58,128,1,0,1,4,15,1,251,50,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,251,50,1,0,1,4,19,85,0,0,0,237, 
  0,0,0,1,0,17,1,251,50,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,251,50,1,0,1,4,15,1,251,50,1,0,17,1,48,42,1,0,1,4,15,1,251,50,1,0,17,1,205,41,1,0,1,4, 
  19,47,0,0,0,154,0,0,0,1,0,17,1,251,50,1,0,1,4,15,1,251,50,1,0,17,1,106,41,1,0,1,4,15,1,251,50,1,0,17,1,165,23,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0, 
  0,3,0,0,0,83,224,133,130,84,32,8,131,82,136,4,128,19,128,68,127,52,120,69,127,53,24,197,128,14,128,196,128,47,112,2,129,85,96,131,129,94,0,7,128,99,112,134,128,103,208,2,128,107,144,7,128,108,240,3, 
  128,109,136,8,128,19,19,0,0,0,63,0,0,0,3,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,251,50,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,251,50,1,0,1,19,83,0,0,0,232,0, 
  0,0,1,0,17,1,251,50,1,0,1,1,19,53,0,0,0,160,0,0,0,1,0,17,1,251,50,1,0,1,19,14,0,0,0,51,0,0,0,3,0,1,15,1,251,50,1,0,17,1,211,52,1,0,1,19,53,0,0,0, 
  161,0,0,0,1,0,17,1,251,50,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,251,50,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,251,50,1,0,1,19,84,0,0,0,234,0,0,0,1,0, 
  17,1,251,50,1,0,1,15,1,251,50,1,0,17,1,31,52,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,251,50,1,0,1,2,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48,1, 
  128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,79,52,1,0,1,2,21,1,71,0,0,0,32,230,1,0,6,0,0,0,2,0,0,0,100,200,1,128,1,80,1,129,2,80,129,128,7,80,1, 
  128,6,80,1,128,101,88,1,128,8,4,15,1,151,52,1,0,17,1,31,129,1,0,1,4,15,1,151,52,1,0,17,1,58,128,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,84,16,129, 
  128,107,120,1,128,108,24,1,128,83,16,129,127,1,19,83,0,0,0,231,0,0,0,3,0,1,19,84,0,0,0,235,0,0,0,3,0,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1, 
  128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,15,53,1,0,1,8,19,14,0,0,0,52,0,0,0,3,0,14,1,21,1,176,0,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,232,68, 
  130,1,240,129,128,2,240,129,129,89,80,4,128,100,216,2,128,101,104,2,128,6,240,1,129,7,240,1,128,90,184,3,128,96,72,3,128,102,248,1,128,8,4,15,1,192,53,1,0,17,1,4,130,1,0,1,4,15,1,192, 
  53,1,0,17,1,31,129,1,0,1,4,15,1,192,53,1,0,17,1,58,128,1,0,1,4,15,1,192,53,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,192,53,1,0,1,4,19,85, 
  0,0,0,237,0,0,0,1,0,17,1,192,53,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,192,53,1,0,1,2,21,0,255,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,85,112,197,130,94,224, 
  4,128,82,248,6,128,83,104,70,129,84,0,134,129,53,136,199,126,14,240,199,126,103,192,3,128,99,80,68,128,107,48,3,128,108,160,2,128,109,16,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,192,53,1,0,1, 
  19,83,0,0,0,232,0,0,0,1,0,17,1,192,53,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,192,53,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,192,53,1,0,1,19,82,0,0,0,229, 
  0,0,0,1,0,17,1,192,53,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,192,53,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,192,53,1,0,1,15,1,192,53,1,0,17,1,192,54,1,0, 
  1,19,53,0,0,0,161,0,0,0,1,0,17,1,192,53,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,192,53,1,0,1,19,14,0,0,0,50,0,0,0,5,0,14,1,1,2,21,1,47,0,0,0,111,229, 
  1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,240,54,1,0,1,2,21,1,71,0,0,0,32,230,1,0,6,0,0,0,2,0,0,0,100,200, 
  1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,101,88,1,128,8,4,15,1,56,55,1,0,17,1,31,129,1,0,1,4,15,1,56,55,1,0,17,1,58,128,1,0,1,2,21,0,59,0,0,0,255,255, 
  255,255,4,0,0,0,2,0,0,0,84,112,129,128,107,16,1,128,108,120,1,128,83,112,129,127,19,84,0,0,0,235,0,0,0,3,0,1,1,19,83,0,0,0,231,0,0,0,3,0,1,2,21,1,47,0,0,0,165,26, 
  2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,175,55,1,0,1,8,19,14,0,0,0,56,0,0,0,1,0,1,21,1,176,0,0,0,191,228,1, 
  0,11,0,0,0,3,0,0,0,88,232,68,130,1,240,129,128,2,240,129,129,89,80,4,128,100,216,2,128,101,104,2,128,6,240,1,129,7,240,1,128,90,184,3,128,96,72,3,128,102,248,1,128,8,4,15,1,96,56,1, 
  0,17,1,4,130,1,0,1,4,15,1,96,56,1,0,17,1,31,129,1,0,1,4,15,1,96,56,1,0,17,1,58,128,1,0,1,4,15,1,96,56,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0, 
  1,0,17,1,96,56,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,96,56,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,96,56,1,0,1,2,21,0,254,0,0,0,255,255,255,255,12,0, 
  0,0,3,0,0,0,85,112,197,130,94,224,4,128,82,248,6,128,83,104,70,129,84,0,134,129,53,136,199,126,14,232,199,126,103,192,3,128,99,80,68,128,107,48,3,128,108,160,2,128,109,16,2,128,19,83,0,0,0,233, 
  0,0,0,1,0,17,1,96,56,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,96,56,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,96,56,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17, 
  1,96,56,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,96,56,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,96,56,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,96,56,1,0,1, 
  15,1,96,56,1,0,17,1,192,54,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,96,56,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,96,56,1,0,1,19,14,0,0,0,54,0,0,0,3,0, 
  1,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,154,57,1,0,1,8,19,14,0,0,0,49,0,0,0,1, 
  0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128, 
  90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,198,60,1,0,17,1,4,130,1,0,1,4,15,1,198,60,1,0,17,1,31,129,1,0,1,4,15,1,198,60,1,0, 
  17,1,58,128,1,0,1,4,15,1,198,60,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,198,60,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,198,60,1,0,1,4, 
  19,85,0,0,0,236,0,0,0,1,0,17,1,198,60,1,0,1,4,15,1,198,60,1,0,17,1,48,42,1,0,1,4,15,1,198,60,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1, 
  198,60,1,0,1,4,15,1,198,60,1,0,17,1,106,41,1,0,1,4,15,1,198,60,1,0,17,1,170,58,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128, 
  2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,229,58,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,56,1,128,15, 
  1,229,58,1,0,17,1,14,59,1,0,1,1,2,21,1,111,0,0,0,255,164,1,0,8,0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144,1,128,57,112,2,128,52,8,67,128,84,152,1,128,6,144,1,128, 
  7,144,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,85,33,1,0,1,4,17,1,126,59,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,85,33,1,0,1,4,15,1,85,33,1,0,17,1, 
  202,7,0,0,1,2,21,1,47,0,0,0,116,224,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,17,1,174,59,1,0,1,2,21,1,99,0,0,0, 
  231,224,1,0,7,0,0,0,2,0,0,0,52,168,130,129,1,112,1,129,2,112,129,128,7,112,1,128,6,112,1,128,57,16,2,128,84,120,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,18,60,1,0,1, 
  4,19,46,0,0,0,143,0,0,0,1,0,17,1,18,60,1,0,1,4,15,1,18,60,1,0,17,1,202,7,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128, 
  46,184,130,127,47,176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,18,60,1,0,1,15,1,18,60,1,0,17,1,79,32,1,0,1,15,1,18,60,1,0,17,1,137,60,1,0,1, 
  1,15,1,18,60,1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,18,60,1,0,1,2,21,1,47,0,0,0,216,25,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2, 
  48,129,127,7,48,1,128,61,56,1,128,8,4,17,1,244,25,1,0,1,19,47,0,0,0,150,0,0,0,5,0,14,14,1,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,176,200,130,84,96,6,131, 
  82,88,7,128,83,200,198,129,52,72,72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2,128,19,83,0,0,0,233,0,0, 
  0,1,0,17,1,198,60,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,198,60,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,198,60,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,198, 
  60,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,198,60,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,198,60,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,198,60,1,0,1,15,1, 
  198,60,1,0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,198,60,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,198,60,1,0,1,19,14,0,0,0,45,0,0,0,3,0,1,15, 
  1,198,60,1,0,17,1,93,66,1,0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,198,60,1,0,1,15,1,198,60,1,0,17,1,253,61,1,0,1,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2, 
  0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,57,62,1,0,1,8,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4, 
  0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62, 
  8,8,128,63,152,7,128,8,4,15,1,73,63,1,0,17,1,4,130,1,0,1,4,15,1,73,63,1,0,17,1,31,129,1,0,1,4,15,1,73,63,1,0,17,1,58,128,1,0,1,4,15,1,73,63,1,0,17,1,147, 
  42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,73,63,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,73,63,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,73,63,1,0, 
  1,4,15,1,73,63,1,0,17,1,48,42,1,0,1,4,15,1,73,63,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,73,63,1,0,1,4,15,1,73,63,1,0,17,1,106,41,1, 
  0,1,4,15,1,73,63,1,0,17,1,165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,160,199,130,84,136,5,131,82,240,5,128,83,144,200,129,52,144,67,127,53,32,197,128,14, 
  248,195,128,15,248,67,126,85,144,132,129,94,0,4,128,99,112,130,128,103,128,6,128,107,0,3,128,108,16,7,128,109,0,8,128,19,82,0,0,0,229,0,0,0,1,0,17,1,73,63,1,0,1,19,84,0,0,0,234,0, 
  0,0,1,0,17,1,73,63,1,0,1,15,1,73,63,1,0,17,1,110,64,1,0,1,1,19,82,0,0,0,228,0,0,0,1,0,17,1,73,63,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,73,63,1,0, 
  1,19,14,0,0,0,44,0,0,0,5,0,14,1,15,1,73,63,1,0,17,1,31,52,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,73,63,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,73,63, 
  1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,73,63,1,0,1,19,15,0,0,0,58,0,0,0,3,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,73,63,1,0,1,19,53,0,0,0,161,0,0,0, 
  1,0,17,1,73,63,1,0,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,171,64,1,0,1,8,19,14,0, 
  0,0,46,0,0,0,5,0,14,14,1,21,1,176,0,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,232,68,130,1,240,129,128,2,240,129,129,89,80,4,128,100,216,2,128,101,104,2,128,6,240,1,129,7,240,1, 
  128,90,184,3,128,96,72,3,128,102,248,1,128,8,4,15,1,92,65,1,0,17,1,4,130,1,0,1,4,15,1,92,65,1,0,17,1,31,129,1,0,1,4,15,1,92,65,1,0,17,1,58,128,1,0,1,4,15,1,92, 
  65,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,92,65,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,92,65,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0, 
  17,1,92,65,1,0,1,2,21,0,0,1,0,0,255,255,255,255,12,0,0,0,3,0,0,0,85,112,197,130,94,224,4,128,82,248,6,128,83,104,70,129,84,0,134,129,53,136,199,126,14,248,199,126,103,192,3,128,99,80, 
  68,128,107,48,3,128,108,160,2,128,109,16,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,92,65,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,92,65,1,0,1,19,84,0,0,0,234,0,0,0,1, 
  0,17,1,92,65,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,92,65,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,92,65,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,92,65,1, 
  0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,92,65,1,0,1,15,1,92,65,1,0,17,1,254,68,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,92,65,1,0,1,19,53,0,0,0,160,0,0,0, 
  1,0,17,1,92,65,1,0,1,19,14,0,0,0,42,0,0,0,7,0,14,14,1,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13, 
  56,1,128,8,4,17,1,153,66,1,0,1,19,14,0,0,0,47,0,0,0,3,0,14,1,21,1,176,0,0,0,191,228,1,0,11,0,0,0,3,0,0,0,88,232,68,130,1,240,129,128,2,240,129,129,89,80,4,128,100, 
  216,2,128,101,104,2,128,6,240,1,129,7,240,1,128,90,184,3,128,96,72,3,128,102,248,1,128,8,4,15,1,74,67,1,0,17,1,4,130,1,0,1,4,15,1,74,67,1,0,17,1,31,129,1,0,1,4,15,1,74, 
  67,1,0,17,1,58,128,1,0,1,4,15,1,74,67,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,74,67,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,74,67,1, 
  0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,74,67,1,0,1,2,21,0,255,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,85,112,197,130,94,224,4,128,82,248,6,128,83,104,70,129,84,0,134,129, 
  53,136,199,126,14,240,199,126,103,192,3,128,99,80,68,128,107,48,3,128,108,160,2,128,109,16,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,74,67,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,74, 
  67,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,74,67,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,74,67,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,74,67,1,0,1,19,82, 
  0,0,0,228,0,0,0,1,0,17,1,74,67,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,74,67,1,0,1,15,1,74,67,1,0,17,1,74,68,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1, 
  74,67,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,74,67,1,0,1,19,14,0,0,0,43,0,0,0,5,0,14,1,1,2,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48,1,128, 
  1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,122,68,1,0,1,2,21,1,71,0,0,0,32,230,1,0,6,0,0,0,2,0,0,0,100,200,1,128,1,80,1,129,2,80,129,128,7,80,1,128, 
  6,80,1,128,101,88,1,128,8,4,15,1,194,68,1,0,17,1,31,129,1,0,1,4,15,1,194,68,1,0,17,1,58,128,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,84,16,129,128, 
  107,24,1,128,108,120,1,128,83,16,129,127,1,19,84,0,0,0,235,0,0,0,3,0,1,19,83,0,0,0,231,0,0,0,3,0,1,2,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48,1,128, 
  1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,46,69,1,0,1,2,21,1,71,0,0,0,32,230,1,0,6,0,0,0,2,0,0,0,100,200,1,128,1,80,1,129,2,80,129,128,7,80,1,128, 
  6,80,1,128,101,88,1,128,8,4,15,1,118,69,1,0,17,1,31,129,1,0,1,4,15,1,118,69,1,0,17,1,58,128,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,84,112,129,128, 
  107,120,1,128,108,16,1,128,83,112,129,127,19,83,0,0,0,231,0,0,0,3,0,1,1,19,84,0,0,0,235,0,0,0,3,0,1,2,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129, 
  1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,226,69,1,0,1,2,21,1,69,1,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,72,4,128,1,240,130,128,2,240,66,130,65,176,8,128, 
  100,216,3,128,69,208,7,131,6,240,130,129,7,240,2,128,88,232,5,128,73,240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5,128,62,184,9,128,63,72,9,128,90,184,4,128,101,104,3,128,102,248,2,128, 
  8,4,15,1,248,73,1,0,17,1,4,130,1,0,1,4,15,1,248,73,1,0,17,1,31,129,1,0,1,4,15,1,248,73,1,0,17,1,58,128,1,0,1,4,15,1,248,73,1,0,17,1,147,42,1,0,1,4,19,85, 
  0,0,0,238,0,0,0,1,0,17,1,248,73,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,248,73,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,248,73,1,0,1,4,15,1,248,73,1, 
  0,17,1,228,162,0,0,1,4,15,1,248,73,1,0,17,1,158,162,0,0,1,4,15,1,248,73,1,0,17,1,6,162,0,0,1,4,15,1,248,73,1,0,17,1,48,42,1,0,1,4,15,1,248,73,1,0,17,1,205, 
  41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,248,73,1,0,1,4,15,1,248,73,1,0,17,1,106,41,1,0,1,4,15,1,248,73,1,0,17,1,40,71,1,0,1,2,21,1,58,0,0,0,134,164, 
  1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,99,71,1,0,1,2,21,0,40,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,74,208,0,128,47,56,1,128,15,1,99,71,1,0,17,1,140,71,1,0,1,1,2,21,1,111,0,0,0,255,164,1,0,8,0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144, 
  1,128,57,112,2,128,52,8,67,128,84,152,1,128,6,144,1,128,7,144,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,68,73,1,0,1,4,17,1,252,71,1,0,1,4,19,46,0,0,0,143,0,0,0, 
  1,0,17,1,68,73,1,0,1,4,15,1,68,73,1,0,17,1,202,7,0,0,1,2,21,1,47,0,0,0,116,224,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56, 
  1,128,8,4,17,1,44,72,1,0,1,2,21,1,99,0,0,0,231,224,1,0,7,0,0,0,2,0,0,0,52,168,130,129,1,112,1,129,2,112,129,128,7,112,1,128,6,112,1,128,57,16,2,128,84,120,1,128,8,4, 
  19,74,0,0,0,214,0,0,0,1,0,17,1,144,72,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,144,72,1,0,1,4,15,1,144,72,1,0,17,1,202,7,0,0,1,2,21,0,118,0,0,0,255,255, 
  255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128,46,184,130,127,47,176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,144,72,1,0,1,15,1,144,72,1,0,17,1, 
  79,32,1,0,1,15,1,144,72,1,0,17,1,7,73,1,0,1,1,15,1,144,72,1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,144,72,1,0,1,2,21,1,47,0,0,0,57,26,2, 
  0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,61,48,1,128,4,17,1,244,25,1,0,1,8,19,47,0,0,0,150,0,0,0,5,0,14,14,1,21,0,118,0,0,0,255,255, 
  255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,32,3,128,46,184,130,127,47,176,130,128,74,224,1,128,91,80,1,128,19,70,0,0,0,208,0,0,0,1,0,17,1,68,73,1,0,1,15,1,68,73,1,0,17,1, 
  100,40,1,0,1,15,1,68,73,1,0,17,1,187,73,1,0,1,1,15,1,68,73,1,0,17,1,132,25,1,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,68,73,1,0,1,2,21,1,47,0,0,0,57,26,2, 
  0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,61,56,1,128,8,4,17,1,9,34,1,0,1,19,47,0,0,0,151,0,0,0,3,0,14,14,1,21,0,94,1,0,0,255,255, 
  255,255,17,0,0,0,4,0,0,0,83,16,135,130,84,168,6,128,82,160,7,128,19,32,74,127,52,40,73,127,53,152,200,128,54,48,8,128,103,104,4,128,85,24,6,128,94,136,5,128,99,248,4,128,107,216,3,128,108,72, 
  3,129,109,184,2,128,14,136,202,126,47,144,9,128,124,176,2,128,1,19,83,0,0,0,233,0,0,0,1,0,17,1,248,73,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,248,73,1,0,1,19,84,0,0,0, 
  234,0,0,0,1,0,17,1,248,73,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,248,73,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,248,73,1,0,1,19,82,0,0,0,228,0,0,0,1,0, 
  17,1,248,73,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,248,73,1,0,1,15,1,248,73,1,0,17,1,192,54,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,248,73,1,0,1,19,53,0,0, 
  0,160,0,0,0,1,0,17,1,248,73,1,0,1,15,1,248,73,1,0,17,1,56,78,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,248,73,1,0,1,15,1,248,73,1,0,17,1,116,55,1,0,1,19,19, 
  0,0,0,62,0,0,0,1,0,17,1,248,73,1,0,1,15,1,248,73,1,0,17,1,141,75,1,0,1,15,1,248,73,1,0,17,1,87,75,1,0,1,2,21,1,53,0,0,0,216,241,1,0,5,0,0,0,2,0,0, 
  0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,92,1,0,0,4,0,14,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1, 
  48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,200,75,1,0,1,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144, 
  130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4, 
  15,1,216,76,1,0,17,1,4,130,1,0,1,4,15,1,216,76,1,0,17,1,31,129,1,0,1,4,15,1,216,76,1,0,17,1,58,128,1,0,1,4,15,1,216,76,1,0,17,1,147,42,1,0,1,4,19,85,0,0, 
  0,238,0,0,0,1,0,17,1,216,76,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,216,76,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,216,76,1,0,1,4,15,1,216,76,1,0,17, 
  1,48,42,1,0,1,4,15,1,216,76,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,216,76,1,0,1,4,15,1,216,76,1,0,17,1,106,41,1,0,1,4,15,1,216,76,1,0, 
  17,1,165,23,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,83,128,133,130,84,160,6,131,82,152,7,128,19,0,67,127,52,8,67,127,53,144,196,128,14,0,195,128,47,184,8,129,85,112, 
  131,129,94,240,4,128,99,112,130,128,103,0,4,128,107,40,8,128,108,8,7,128,109,16,6,128,19,82,0,0,0,229,0,0,0,1,0,17,1,216,76,1,0,1,1,15,1,216,76,1,0,17,1,252,77,1,0,1,19,53, 
  0,0,0,162,0,0,0,1,0,17,1,216,76,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,216,76,1,0,1,19,14,0,0,0,51,0,0,0,3,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,216, 
  76,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,216,76,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,216,76,1,0,1,15,1,216,76,1,0,17,1,31,52,1,0,1,19,83,0,0,0,232,0, 
  0,0,1,0,17,1,216,76,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,216,76,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,216,76,1,0,1,19,19,0,0,0,63,0,0,0,3,0,1,2, 
  21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,15,53,1,0,1,8,19,14,0,0,0,52,0,0,0,3,0,14,1, 
  21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,115,78,1,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21, 
  1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4, 
  128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,131,79,1,0,17,1,4,130,1,0,1,4,15,1,131,79,1,0,17,1,31,129,1,0,1,4,15,1,131,79,1,0,17,1,58, 
  128,1,0,1,4,15,1,131,79,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,131,79,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,131,79,1,0,1,4,19,85,0, 
  0,0,236,0,0,0,1,0,17,1,131,79,1,0,1,4,15,1,131,79,1,0,17,1,48,42,1,0,1,4,15,1,131,79,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,131,79,1, 
  0,1,4,15,1,131,79,1,0,17,1,106,41,1,0,1,4,15,1,131,79,1,0,17,1,170,58,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,176,200,130,84,96,6,131,82,88,7, 
  128,83,200,198,129,52,72,72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2,128,19,83,0,0,0,233,0,0,0,1,0, 
  17,1,131,79,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,131,79,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,131,79,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,131,79,1,0, 
  1,19,82,0,0,0,229,0,0,0,1,0,17,1,131,79,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,131,79,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,131,79,1,0,1,15,1,131,79,1, 
  0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,131,79,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,131,79,1,0,1,19,14,0,0,0,45,0,0,0,3,0,1,15,1,131,79, 
  1,0,17,1,104,83,1,0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,131,79,1,0,1,15,1,131,79,1,0,17,1,186,80,1,0,1,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0, 
  6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,246,80,1,0,1,8,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0, 
  96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128, 
  63,152,7,128,8,4,15,1,6,82,1,0,17,1,4,130,1,0,1,4,15,1,6,82,1,0,17,1,31,129,1,0,1,4,15,1,6,82,1,0,17,1,58,128,1,0,1,4,15,1,6,82,1,0,17,1,147,42,1,0, 
  1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,6,82,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,6,82,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,6,82,1,0,1,4,15, 
  1,6,82,1,0,17,1,48,42,1,0,1,4,15,1,6,82,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,6,82,1,0,1,4,15,1,6,82,1,0,17,1,106,41,1,0,1,4, 
  15,1,6,82,1,0,17,1,165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,216,195,130,84,8,3,131,82,152,7,128,83,112,194,129,52,40,72,127,53,112,195,128,14,0,195,128, 
  15,0,67,126,85,200,132,129,94,8,7,128,99,88,133,128,103,232,5,128,107,120,6,128,108,56,4,128,109,144,8,128,19,53,0,0,0,161,0,0,0,1,0,17,1,6,82,1,0,1,1,15,1,6,82,1,0,17,1,31, 
  52,1,0,1,19,14,0,0,0,44,0,0,0,5,0,14,1,19,15,0,0,0,58,0,0,0,3,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,6,82,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1, 
  6,82,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,6,82,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,6,82,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,6,82,1,0,1,19, 
  82,0,0,0,228,0,0,0,1,0,17,1,6,82,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,6,82,1,0,1,15,1,6,82,1,0,17,1,43,83,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17, 
  1,6,82,1,0,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,171,64,1,0,1,19,14,0,0,0,46, 
  0,0,0,5,0,14,14,1,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,153,66,1,0,1,8,19,14,0,0,0, 
  47,0,0,0,3,0,14,1,21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,212,83,1,0,1,2,21,1,58,0, 
  0,0,128,242,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56,1,128,8,4,19,75,0,0,0,215,0,0,0,1,0,17,1,15,84,1,0,1,2,21,0,40,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,125,208,0,128,75,216,192,127,1,15,1,15,84,1,0,17,1,56,84,1,0,1,2,21,1,53,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,125,0,0,0,94,1,0,0,4,0,14,1,2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,192,1,128,97,232,130,128,98,224,2,128, 
  121,80,66,128,125,48,1,128,19,97,0,0,0,11,1,0,0,1,0,17,1,110,84,1,0,1,19,97,0,0,0,10,1,0,0,1,0,17,1,110,84,1,0,1,19,97,0,0,0,9,1,0,0,1,0,17,1,110,84,1, 
  0,1,1,15,1,110,84,1,0,17,1,217,84,1,0,1,2,21,1,125,0,0,0,213,242,1,0,9,0,0,0,3,0,0,0,10,120,3,128,1,176,1,128,2,176,129,127,11,8,3,128,52,152,2,128,117,40,2,128,6, 
  176,129,128,7,176,1,128,118,184,1,128,8,4,15,1,87,85,1,0,17,1,164,83,1,0,1,4,15,1,87,85,1,0,17,1,178,69,1,0,1,4,15,1,87,85,1,0,17,1,159,85,1,0,1,4,19,98,0,0,0, 
  16,1,0,0,3,0,14,1,4,15,1,87,85,1,0,17,1,5,44,1,0,1,2,21,0,71,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,112,1,128,97,48,66,128,121,208,65,128,125,16,1,128,19,97,0, 
  0,0,14,1,0,0,2,0,1,19,97,0,0,0,13,1,0,0,2,0,1,19,97,0,0,0,12,1,0,0,2,0,1,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1, 
  128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,218,85,1,0,1,2,21,0,84,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,122,120,1,128,121,8,2,128, 
  74,16,130,127,123,16,1,128,15,1,218,85,1,0,17,1,47,86,1,0,1,19,123,0,0,0,90,1,0,0,1,0,17,1,218,85,1,0,1,1,19,122,0,0,0,89,1,0,0,1,0,17,1,218,85,1,0,1,2,21, 
  1,59,0,0,0,156,179,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,193,128,2,80,129,127,7,80,1,128,53,152,65,128,81,88,1,128,8,4,17,1,64,178,0,0,1,4,17,1,107,86,1,0,1,2,21, 
  1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,155,86,1,0,1,2,21,1,69,1,0,0,206,157,1,0,19,0,0, 
  0,4,0,0,0,96,72,4,128,1,240,130,128,2,240,66,130,65,176,8,128,100,216,3,128,69,208,7,131,6,240,130,129,7,240,2,128,88,232,5,128,73,240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5, 
  128,62,184,9,128,63,72,9,128,90,184,4,128,101,104,3,128,102,248,2,128,8,4,15,1,181,88,1,0,17,1,4,130,1,0,1,4,15,1,181,88,1,0,17,1,31,129,1,0,1,4,15,1,181,88,1,0,17,1,58, 
  128,1,0,1,4,15,1,181,88,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,181,88,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,181,88,1,0,1,4,19,85,0, 
  0,0,236,0,0,0,1,0,17,1,181,88,1,0,1,4,15,1,181,88,1,0,17,1,228,162,0,0,1,4,15,1,181,88,1,0,17,1,158,162,0,0,1,4,15,1,181,88,1,0,17,1,6,162,0,0,1,4,15,1, 
  181,88,1,0,17,1,48,42,1,0,1,4,15,1,181,88,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,181,88,1,0,1,4,15,1,181,88,1,0,17,1,106,41,1,0,1,4,15, 
  1,181,88,1,0,17,1,225,87,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0, 
  0,0,1,0,17,1,28,88,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,56,1,128,15,1,28,88,1,0,17,1,69,88,1,0,1,1,2,21,1,111,0,0,0,255, 
  164,1,0,8,0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144,1,128,57,112,2,128,52,8,67,128,84,152,1,128,6,144,1,128,7,144,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,68,73, 
  1,0,1,4,17,1,31,46,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,68,73,1,0,1,4,15,1,68,73,1,0,17,1,202,7,0,0,1,2,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4, 
  0,0,0,83,16,7,132,84,168,6,128,82,160,7,128,19,32,74,127,52,40,73,127,53,152,200,128,54,48,8,128,103,104,4,128,85,24,6,128,121,176,2,128,94,136,5,128,107,216,3,128,108,72,3,128,109,184,2,128,14, 
  136,10,127,47,144,9,128,99,248,4,128,1,19,83,0,0,0,233,0,0,0,1,0,17,1,181,88,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,181,88,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17, 
  1,181,88,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,181,88,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,181,88,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,181,88,1,0,1, 
  19,53,0,0,0,162,0,0,0,1,0,17,1,181,88,1,0,1,15,1,181,88,1,0,17,1,74,68,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,181,88,1,0,1,19,53,0,0,0,160,0,0,0,1,0, 
  17,1,181,88,1,0,1,15,1,181,88,1,0,17,1,245,92,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,181,88,1,0,1,15,1,181,88,1,0,17,1,186,92,1,0,1,19,19,0,0,0,62,0,0,0, 
  1,0,17,1,181,88,1,0,1,15,1,181,88,1,0,17,1,75,90,1,0,1,15,1,181,88,1,0,17,1,20,90,1,0,1,2,21,1,54,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,121,0,0,0,88,1,0,0,6,0,14,14,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129, 
  127,7,112,1,128,13,48,1,128,4,17,1,134,90,1,0,1,8,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130, 
  65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,150,91,1,0, 
  17,1,4,130,1,0,1,4,15,1,150,91,1,0,17,1,31,129,1,0,1,4,15,1,150,91,1,0,17,1,58,128,1,0,1,4,15,1,150,91,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1, 
  0,17,1,150,91,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,150,91,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,150,91,1,0,1,4,15,1,150,91,1,0,17,1,48,42,1,0,1, 
  4,15,1,150,91,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,150,91,1,0,1,4,15,1,150,91,1,0,17,1,106,41,1,0,1,4,15,1,150,91,1,0,17,1,165,23,1,0, 
  1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,83,0,135,130,84,144,3,131,82,128,5,128,19,248,67,127,52,32,72,127,53,32,197,128,14,248,195,128,47,160,6,129,85,0,132,129,94,144,4,128, 
  99,16,134,128,103,136,8,128,107,144,7,128,108,112,2,128,109,0,3,128,19,83,0,0,0,232,0,0,0,1,0,17,1,150,91,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,150,91,1,0,1,15,1,150,91, 
  1,0,17,1,31,52,1,0,1,1,19,53,0,0,0,162,0,0,0,1,0,17,1,150,91,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,150,91,1,0,1,19,14,0,0,0,51,0,0,0,3,0,1,19,53, 
  0,0,0,160,0,0,0,1,0,17,1,150,91,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,150,91,1,0,1,19,19,0,0,0,63,0,0,0,3,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,150, 
  91,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,150,91,1,0,1,15,1,150,91,1,0,17,1,211,52,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,150,91,1,0,1,2,21,1,47,0,0,0, 
  165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,175,55,1,0,1,8,19,14,0,0,0,56,0,0,0,1,0,1,21,1,47,0,0,0,165, 
  26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,48,93,1,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21,1,15,1,0,0,142,227, 
  1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8, 
  3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,64,94,1,0,17,1,4,130,1,0,1,4,15,1,64,94,1,0,17,1,31,129,1,0,1,4,15,1,64,94,1,0,17,1,58,128,1,0,1,4,15,1, 
  64,94,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,64,94,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,64,94,1,0,1,4,19,85,0,0,0,236,0,0,0,1, 
  0,17,1,64,94,1,0,1,4,15,1,64,94,1,0,17,1,48,42,1,0,1,4,15,1,64,94,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,64,94,1,0,1,4,15,1,64,94, 
  1,0,17,1,106,41,1,0,1,4,15,1,64,94,1,0,17,1,170,58,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,176,200,130,84,96,6,131,82,88,7,128,83,200,198,129,52,72, 
  72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,64,94,1,0,1, 
  19,83,0,0,0,232,0,0,0,1,0,17,1,64,94,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,64,94,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,64,94,1,0,1,19,82,0,0,0,229, 
  0,0,0,1,0,17,1,64,94,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,64,94,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,64,94,1,0,1,15,1,64,94,1,0,17,1,204,159,0,0, 
  1,19,53,0,0,0,161,0,0,0,1,0,17,1,64,94,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,64,94,1,0,1,19,14,0,0,0,45,0,0,0,3,0,1,15,1,64,94,1,0,17,1,232,97,1, 
  0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,64,94,1,0,1,15,1,64,94,1,0,17,1,119,95,1,0,1,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193, 
  128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,179,95,1,0,1,8,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130, 
  128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15, 
  1,195,96,1,0,17,1,4,130,1,0,1,4,15,1,195,96,1,0,17,1,31,129,1,0,1,4,15,1,195,96,1,0,17,1,58,128,1,0,1,4,15,1,195,96,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0, 
  238,0,0,0,1,0,17,1,195,96,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,195,96,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,195,96,1,0,1,4,15,1,195,96,1,0,17,1, 
  48,42,1,0,1,4,15,1,195,96,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,195,96,1,0,1,4,15,1,195,96,1,0,17,1,106,41,1,0,1,4,15,1,195,96,1,0,17, 
  1,165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,144,195,130,84,192,7,131,82,160,6,128,83,48,199,129,52,136,68,127,53,184,200,128,14,240,195,128,15,240,67,126,85,240,132, 
  129,94,128,5,128,99,248,131,128,103,112,2,128,107,16,6,128,108,40,8,128,109,0,3,128,19,82,0,0,0,230,0,0,0,1,0,17,1,195,96,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,195,96,1,0, 
  1,19,15,0,0,0,58,0,0,0,3,0,1,1,19,82,0,0,0,229,0,0,0,1,0,17,1,195,96,1,0,1,15,1,195,96,1,0,17,1,43,83,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,195,96, 
  1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,195,96,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,195,96,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,195,96,1,0,1,19,53,0, 
  0,0,161,0,0,0,1,0,17,1,195,96,1,0,1,15,1,195,96,1,0,17,1,31,52,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,195,96,1,0,1,19,14,0,0,0,44,0,0,0,5,0,14,1,2, 
  21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,153,66,1,0,1,8,19,14,0,0,0,47,0,0,0,3,0,14,1, 
  21,1,58,0,0,0,237,154,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,95,98,1,0,1,2,21, 
  0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,46,216,0,128,95,208,0,128,1,19,95,0,0,0,5,1,0,0,2,0,1,2,21,0,79,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,98,160,1, 
  128,121,16,1,128,94,112,130,127,95,8,2,128,19,98,0,0,0,15,1,0,0,1,0,17,1,135,98,1,0,1,19,94,0,0,0,4,1,0,0,4,0,14,1,15,1,135,98,1,0,17,1,215,98,1,0,1,1,2,21, 
  1,71,0,0,0,87,243,1,0,6,0,0,0,2,0,0,0,52,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,10,200,1,128,8,4,15,1,31,99,1,0,17,1,159,85,1,0,1,4,15,1,31, 
  99,1,0,17,1,153,43,1,0,1,2,21,0,63,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,94,240,129,128,121,240,0,128,98,128,1,128,19,98,0,0,0,15,1,0,0,1,0,17,1,31,99,1,0,1,19, 
  94,0,0,0,3,1,0,0,5,0,14,14,1,1,2,21,1,71,0,0,0,87,243,1,0,6,0,0,0,2,0,0,0,52,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,10,200,1,128,8,4,15, 
  1,215,113,1,0,17,1,68,101,1,0,1,4,15,1,215,113,1,0,17,1,167,99,1,0,1,2,21,1,107,0,0,0,177,239,1,0,8,0,0,0,3,0,0,0,10,232,2,128,1,144,1,128,2,144,129,127,118,152,1, 
  128,52,120,2,128,117,8,2,128,6,144,65,127,7,144,1,128,8,4,15,1,19,100,1,0,17,1,164,83,1,0,1,4,15,1,19,100,1,0,17,1,178,69,1,0,1,4,15,1,19,100,1,0,17,1,68,101,1,0,1, 
  4,15,1,19,100,1,0,17,1,5,44,1,0,1,2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,101,232,130,128,102,224,130,128,125,192,1,128,126,48,1,128,19,101,0,0,0,21,1, 
  0,0,1,0,17,1,19,100,1,0,1,19,101,0,0,0,22,1,0,0,1,0,17,1,19,100,1,0,1,19,101,0,0,0,23,1,0,0,1,0,17,1,19,100,1,0,1,1,15,1,19,100,1,0,17,1,126,100,1,0, 
  1,2,21,1,125,0,0,0,213,242,1,0,9,0,0,0,3,0,0,0,10,120,3,128,1,176,1,128,2,176,129,127,11,8,3,128,52,152,2,128,117,40,2,128,6,176,129,128,7,176,1,128,118,184,1,128,8,4,15,1, 
  252,100,1,0,17,1,164,83,1,0,1,4,15,1,252,100,1,0,17,1,178,69,1,0,1,4,15,1,252,100,1,0,17,1,68,101,1,0,1,4,19,102,0,0,0,28,1,0,0,3,0,14,1,4,15,1,252,100,1,0, 
  17,1,5,44,1,0,1,2,21,0,71,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,208,1,128,101,48,130,128,126,16,1,128,125,112,1,128,19,101,0,0,0,24,1,0,0,2,0,1,19,101,0,0,0,25, 
  1,0,0,2,0,1,19,101,0,0,0,26,1,0,0,2,0,1,1,2,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88, 
  1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,145,101,1,0,1,4,15,1,145,101,1,0,17,1,202,7,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,70,200,1,129,13, 
  152,2,128,46,48,130,127,91,56,1,128,126,48,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,145,101,1,0,1,15,1,145,101,1,0,17,1,42,102,1,0,1,15,1,145,101,1,0,17,1,247,101,1,0,1, 
  19,70,0,0,0,207,0,0,0,1,0,17,1,145,101,1,0,1,2,21,1,39,0,0,0,5,27,2,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13, 
  0,0,0,41,0,0,0,1,0,1,21,1,47,0,0,0,72,179,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,53,56,1,128,8,4,17,1,90,102,1,0,1,2,21,1, 
  47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,138,102,1,0,1,2,21,1,69,1,0,0,206,157,1,0,19,0,0,0, 
  4,0,0,0,96,72,4,128,1,240,130,128,2,240,66,130,65,176,8,128,100,216,3,128,69,208,7,131,6,240,130,129,7,240,2,128,88,232,5,128,73,240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5,128, 
  62,184,9,128,63,72,9,128,90,184,4,128,101,104,3,128,102,248,2,128,8,4,15,1,164,104,1,0,17,1,4,130,1,0,1,4,15,1,164,104,1,0,17,1,31,129,1,0,1,4,15,1,164,104,1,0,17,1,58,128, 
  1,0,1,4,15,1,164,104,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,164,104,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,164,104,1,0,1,4,19,85,0,0, 
  0,236,0,0,0,1,0,17,1,164,104,1,0,1,4,15,1,164,104,1,0,17,1,228,162,0,0,1,4,15,1,164,104,1,0,17,1,158,162,0,0,1,4,15,1,164,104,1,0,17,1,6,162,0,0,1,4,15,1,164, 
  104,1,0,17,1,48,42,1,0,1,4,15,1,164,104,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,164,104,1,0,1,4,15,1,164,104,1,0,17,1,106,41,1,0,1,4,15,1, 
  164,104,1,0,17,1,208,103,1,0,1,2,21,1,58,0,0,0,134,164,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,214,0,0, 
  0,1,0,17,1,11,104,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,74,208,0,128,47,56,1,128,15,1,11,104,1,0,17,1,52,104,1,0,1,1,2,21,1,111,0,0,0,255,164, 
  1,0,8,0,0,0,3,0,0,0,64,48,2,128,1,144,129,128,2,144,1,128,57,112,2,128,52,8,67,128,84,152,1,128,6,144,1,128,7,144,1,128,8,4,19,74,0,0,0,214,0,0,0,1,0,17,1,103,47,1, 
  0,1,4,17,1,252,71,1,0,1,4,19,46,0,0,0,143,0,0,0,1,0,17,1,103,47,1,0,1,4,15,1,103,47,1,0,17,1,202,7,0,0,1,2,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0, 
  0,0,83,16,135,130,84,168,6,128,82,160,7,128,19,32,74,127,52,40,73,127,53,152,200,128,54,48,8,128,103,104,4,128,85,24,6,128,94,136,197,129,99,248,4,128,107,216,3,128,108,72,3,128,109,184,2,128,14,136, 
  202,126,47,144,9,128,126,176,2,128,1,19,83,0,0,0,233,0,0,0,1,0,17,1,164,104,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,164,104,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1, 
  164,104,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,164,104,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,164,104,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,164,104,1,0,1,19, 
  53,0,0,0,162,0,0,0,1,0,17,1,164,104,1,0,1,15,1,164,104,1,0,17,1,74,68,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,164,104,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17, 
  1,164,104,1,0,1,15,1,164,104,1,0,17,1,228,108,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,164,104,1,0,1,15,1,164,104,1,0,17,1,169,108,1,0,1,19,19,0,0,0,62,0,0,0,1, 
  0,17,1,164,104,1,0,1,15,1,164,104,1,0,17,1,58,106,1,0,1,15,1,164,104,1,0,17,1,3,106,1,0,1,2,21,1,54,0,0,0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1, 
  128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,126,0,0,0,95,1,0,0,6,0,14,14,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127, 
  7,112,1,128,13,48,1,128,4,17,1,117,106,1,0,1,8,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65, 
  0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,133,107,1,0,17, 
  1,4,130,1,0,1,4,15,1,133,107,1,0,17,1,31,129,1,0,1,4,15,1,133,107,1,0,17,1,58,128,1,0,1,4,15,1,133,107,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0, 
  17,1,133,107,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,133,107,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,133,107,1,0,1,4,15,1,133,107,1,0,17,1,48,42,1,0,1,4, 
  15,1,133,107,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,133,107,1,0,1,4,15,1,133,107,1,0,17,1,106,41,1,0,1,4,15,1,133,107,1,0,17,1,165,23,1,0,1, 
  2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,83,248,131,130,84,32,8,131,82,224,5,128,19,136,68,127,52,112,66,127,53,128,197,128,14,136,196,128,47,32,5,129,85,112,134,129,94,104,3,128,99, 
  0,135,128,103,144,4,128,107,144,7,128,108,136,8,128,109,216,2,128,15,1,133,107,1,0,17,1,211,52,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,133,107,1,0,1,19,82,0,0,0,228,0,0,0,1, 
  0,17,1,133,107,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,133,107,1,0,1,1,19,82,0,0,0,230,0,0,0,1,0,17,1,133,107,1,0,1,19,19,0,0,0,63,0,0,0,3,0,1,19,14,0, 
  0,0,51,0,0,0,3,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,133,107,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,133,107,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,133,107, 
  1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,133,107,1,0,1,15,1,133,107,1,0,17,1,31,52,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,133,107,1,0,1,2,21,1,47,0,0,0,165, 
  26,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,175,55,1,0,1,19,14,0,0,0,56,0,0,0,1,0,1,21,1,47,0,0,0,165,26, 
  2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,31,109,1,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1, 
  0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3, 
  128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,47,110,1,0,17,1,4,130,1,0,1,4,15,1,47,110,1,0,17,1,31,129,1,0,1,4,15,1,47,110,1,0,17,1,58,128,1,0,1,4,15,1,47, 
  110,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,47,110,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,47,110,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0, 
  17,1,47,110,1,0,1,4,15,1,47,110,1,0,17,1,48,42,1,0,1,4,15,1,47,110,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,47,110,1,0,1,4,15,1,47,110,1, 
  0,17,1,106,41,1,0,1,4,15,1,47,110,1,0,17,1,170,58,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,176,200,130,84,96,6,131,82,88,7,128,83,200,198,129,52,72,72, 
  127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,47,110,1,0,1,19, 
  83,0,0,0,232,0,0,0,1,0,17,1,47,110,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,47,110,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,47,110,1,0,1,19,82,0,0,0,229,0, 
  0,0,1,0,17,1,47,110,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,47,110,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,47,110,1,0,1,15,1,47,110,1,0,17,1,204,159,0,0,1, 
  19,53,0,0,0,161,0,0,0,1,0,17,1,47,110,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,47,110,1,0,1,19,14,0,0,0,45,0,0,0,3,0,1,15,1,47,110,1,0,17,1,93,66,1,0, 
  1,19,15,0,0,0,57,0,0,0,1,0,17,1,47,110,1,0,1,15,1,47,110,1,0,17,1,102,111,1,0,1,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128, 
  2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,162,111,1,0,1,8,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128, 
  2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1, 
  178,112,1,0,17,1,4,130,1,0,1,4,15,1,178,112,1,0,17,1,31,129,1,0,1,4,15,1,178,112,1,0,17,1,58,128,1,0,1,4,15,1,178,112,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238, 
  0,0,0,1,0,17,1,178,112,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,178,112,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,178,112,1,0,1,4,15,1,178,112,1,0,17,1,48, 
  42,1,0,1,4,15,1,178,112,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,178,112,1,0,1,4,15,1,178,112,1,0,17,1,106,41,1,0,1,4,15,1,178,112,1,0,17,1, 
  165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,64,198,130,84,160,6,131,82,0,8,128,83,144,200,129,52,216,69,127,53,152,199,128,14,32,196,128,15,32,68,126,85,0,131,129, 
  94,184,4,128,99,72,133,128,103,8,7,128,107,40,4,128,108,144,3,128,109,112,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,178,112,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,178,112,1,0,1, 
  19,83,0,0,0,232,0,0,0,1,0,17,1,178,112,1,0,1,1,19,84,0,0,0,234,0,0,0,1,0,17,1,178,112,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,178,112,1,0,1,19,82,0,0,0, 
  229,0,0,0,1,0,17,1,178,112,1,0,1,15,1,178,112,1,0,17,1,43,83,1,0,1,19,15,0,0,0,58,0,0,0,3,0,1,15,1,178,112,1,0,17,1,31,52,1,0,1,19,82,0,0,0,230,0,0,0, 
  1,0,17,1,178,112,1,0,1,19,14,0,0,0,44,0,0,0,5,0,14,1,19,53,0,0,0,160,0,0,0,1,0,17,1,178,112,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,178,112,1,0,1,2,21, 
  0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,102,128,129,128,99,224,1,128,126,240,0,128,19,102,0,0,0,27,1,0,0,1,0,17,1,215,113,1,0,1,19,99,0,0,0,17,1,0,0,4,0,1,1, 
  2,21,1,71,0,0,0,87,243,1,0,6,0,0,0,2,0,0,0,52,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,10,200,1,128,8,4,15,1,252,127,1,0,17,1,250,115,1,0,1,4,15, 
  1,252,127,1,0,17,1,93,114,1,0,1,2,21,1,107,0,0,0,177,239,1,0,8,0,0,0,3,0,0,0,10,232,2,128,1,144,1,128,2,144,129,127,118,152,1,128,52,120,2,128,117,8,2,128,6,144,65,127,7, 
  144,1,128,8,4,15,1,201,114,1,0,17,1,164,83,1,0,1,4,15,1,201,114,1,0,17,1,178,69,1,0,1,4,15,1,201,114,1,0,17,1,250,115,1,0,1,4,15,1,201,114,1,0,17,1,5,44,1,0,1, 
  2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,105,232,194,128,106,224,2,128,127,48,1,128,125,192,1,128,19,105,0,0,0,33,1,0,0,1,0,17,1,201,114,1,0,1,19,105,0, 
  0,0,34,1,0,0,1,0,17,1,201,114,1,0,1,19,105,0,0,0,35,1,0,0,1,0,17,1,201,114,1,0,1,1,15,1,201,114,1,0,17,1,52,115,1,0,1,2,21,1,125,0,0,0,213,242,1,0,9,0, 
  0,0,3,0,0,0,10,120,3,128,1,176,1,128,2,176,129,127,11,8,3,128,52,152,2,128,117,40,2,128,6,176,129,128,7,176,1,128,118,184,1,128,8,4,15,1,178,115,1,0,17,1,164,83,1,0,1,4,15,1, 
  178,115,1,0,17,1,178,69,1,0,1,4,15,1,178,115,1,0,17,1,250,115,1,0,1,4,19,106,0,0,0,40,1,0,0,3,0,14,1,4,15,1,178,115,1,0,17,1,5,44,1,0,1,2,21,0,71,0,0,0, 
  255,255,255,255,4,0,0,0,2,0,0,0,124,208,1,128,105,48,66,128,125,112,1,128,127,16,1,128,19,105,0,0,0,36,1,0,0,2,0,1,19,105,0,0,0,37,1,0,0,2,0,1,19,105,0,0,0,38,1,0, 
  0,2,0,1,1,2,21,1,135,0,0,0,38,244,1,0,9,0,0,0,3,0,0,0,104,80,2,128,1,176,193,129,2,176,1,128,91,48,3,128,103,192,2,128,85,160,3,128,6,176,1,128,7,176,65,127,121,184,1,128, 
  8,4,19,128,0,0,0,97,1,0,0,1,0,17,1,199,116,1,0,1,4,15,1,199,116,1,0,17,1,80,95,0,0,1,4,15,1,199,116,1,0,17,1,130,116,1,0,1,4,15,1,199,116,1,0,17,1,160,10,0, 
  0,1,4,19,75,0,0,0,215,0,0,0,1,0,17,1,199,116,1,0,1,2,21,1,68,0,0,0,85,201,1,0,4,0,0,0,2,0,0,0,52,176,1,128,1,16,129,128,2,16,1,128,57,24,1,128,8,4,19,46, 
  0,0,0,143,0,0,0,1,0,17,1,61,206,0,0,1,4,15,1,61,206,0,0,17,1,202,7,0,0,1,2,21,0,150,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,112,40,131,129,113,152,2,128,86,184,131, 
  128,75,32,132,128,114,8,2,128,127,0,2,128,128,112,1,128,19,112,0,0,0,48,1,0,0,1,0,17,1,199,116,1,0,1,1,19,86,0,0,0,242,0,0,0,1,0,17,1,199,116,1,0,1,19,86,0,0,0,241, 
  0,0,0,1,0,17,1,199,116,1,0,1,19,86,0,0,0,240,0,0,0,1,0,17,1,199,116,1,0,1,15,1,199,116,1,0,17,1,94,117,1,0,1,19,112,0,0,0,47,1,0,0,1,0,17,1,199,116,1,0, 
  1,2,21,1,47,0,0,0,72,179,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,53,56,1,128,8,4,17,1,142,117,1,0,1,2,21,1,47,0,0,0,44,242,1,0, 
  5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,190,117,1,0,1,2,21,1,69,1,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,72,4,128, 
  1,240,130,128,2,240,66,130,65,176,8,128,100,216,3,128,69,208,7,131,6,240,130,129,7,240,2,128,88,232,5,128,73,240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5,128,62,184,9,128,63,72,9,128, 
  90,184,4,128,101,104,3,128,102,248,2,128,8,4,15,1,4,119,1,0,17,1,4,130,1,0,1,4,15,1,4,119,1,0,17,1,31,129,1,0,1,4,15,1,4,119,1,0,17,1,58,128,1,0,1,4,15,1,4,119, 
  1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,4,119,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,4,119,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17, 
  1,4,119,1,0,1,4,15,1,4,119,1,0,17,1,228,162,0,0,1,4,15,1,4,119,1,0,17,1,158,162,0,0,1,4,15,1,4,119,1,0,17,1,6,162,0,0,1,4,15,1,4,119,1,0,17,1,48,42,1, 
  0,1,4,15,1,4,119,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,4,119,1,0,1,4,15,1,4,119,1,0,17,1,106,41,1,0,1,4,15,1,4,119,1,0,17,1,208,103, 
  1,0,1,2,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,83,16,135,130,84,168,6,128,82,160,7,128,19,32,74,127,52,40,73,127,53,152,200,128,54,48,8,128,103,104,4,128,85,24,6,128,94,136, 
  5,128,99,248,4,128,107,216,3,128,108,72,3,128,109,184,2,128,14,136,202,126,47,144,73,128,127,176,2,128,1,19,83,0,0,0,233,0,0,0,1,0,17,1,4,119,1,0,1,19,83,0,0,0,232,0,0,0,1,0, 
  17,1,4,119,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,4,119,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,4,119,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,4,119,1,0, 
  1,19,82,0,0,0,228,0,0,0,1,0,17,1,4,119,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,4,119,1,0,1,15,1,4,119,1,0,17,1,31,52,1,0,1,19,53,0,0,0,161,0,0,0,1, 
  0,17,1,4,119,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,4,119,1,0,1,15,1,4,119,1,0,17,1,9,123,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,4,119,1,0,1,15,1,4, 
  119,1,0,17,1,116,55,1,0,1,19,19,0,0,0,62,0,0,0,1,0,17,1,4,119,1,0,1,15,1,4,119,1,0,17,1,154,120,1,0,1,15,1,4,119,1,0,17,1,99,120,1,0,1,2,21,1,54,0,0, 
  0,216,241,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,127,0,0,0,96,1,0,0,6,0,14,14,1,2,21,1,47,0,0,0,165,26,2,0, 
  5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,213,120,1,0,1,8,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16, 
  0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102, 
  152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,229,121,1,0,17,1,4,130,1,0,1,4,15,1,229,121,1,0,17,1,31,129,1,0,1,4,15,1,229,121,1,0,17,1,58,128,1,0,1,4,15,1,229,121,1, 
  0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,229,121,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,229,121,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1, 
  229,121,1,0,1,4,15,1,229,121,1,0,17,1,48,42,1,0,1,4,15,1,229,121,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,229,121,1,0,1,4,15,1,229,121,1,0,17, 
  1,106,41,1,0,1,4,15,1,229,121,1,0,17,1,165,23,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,83,248,135,130,84,0,3,131,82,248,3,128,19,136,68,127,52,32,69,127,53, 
  8,199,128,14,136,196,128,47,24,6,129,85,104,131,129,94,104,7,128,99,136,133,128,103,112,2,128,107,136,8,128,108,120,6,128,109,144,4,128,19,82,0,0,0,230,0,0,0,1,0,17,1,229,121,1,0,1,15,1,229, 
  121,1,0,17,1,31,52,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,229,121,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,229,121,1,0,1,1,19,83,0,0,0,233,0,0,0,1,0,17,1, 
  229,121,1,0,1,15,1,229,121,1,0,17,1,211,52,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,229,121,1,0,1,19,19,0,0,0,63,0,0,0,3,0,1,19,83,0,0,0,232,0,0,0,1,0,17, 
  1,229,121,1,0,1,19,14,0,0,0,51,0,0,0,3,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,229,121,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,229,121,1,0,1,19,84,0,0,0,234, 
  0,0,0,1,0,17,1,229,121,1,0,1,2,21,1,47,0,0,0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,68,123,1,0,1,8, 
  19,14,0,0,0,49,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144, 
  2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,84,124,1,0,17,1,4,130,1,0,1,4,15,1,84,124,1,0,17,1,31,129, 
  1,0,1,4,15,1,84,124,1,0,17,1,58,128,1,0,1,4,15,1,84,124,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,84,124,1,0,1,4,19,85,0,0,0,237,0,0,0, 
  1,0,17,1,84,124,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,84,124,1,0,1,4,15,1,84,124,1,0,17,1,48,42,1,0,1,4,15,1,84,124,1,0,17,1,205,41,1,0,1,4,19,47,0, 
  0,0,154,0,0,0,1,0,17,1,84,124,1,0,1,4,15,1,84,124,1,0,17,1,106,41,1,0,1,4,15,1,84,124,1,0,17,1,170,58,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0, 
  0,0,47,176,200,130,84,96,6,131,82,88,7,128,83,200,198,129,52,72,72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112, 
  2,128,19,83,0,0,0,233,0,0,0,1,0,17,1,84,124,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,84,124,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,84,124,1,0,1,19,82,0,0, 
  0,230,0,0,0,1,0,17,1,84,124,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,84,124,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,84,124,1,0,1,19,53,0,0,0,162,0,0,0,1, 
  0,17,1,84,124,1,0,1,15,1,84,124,1,0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,84,124,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,84,124,1,0,1,19,14,0, 
  0,0,45,0,0,0,3,0,1,15,1,84,124,1,0,17,1,104,83,1,0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,84,124,1,0,1,15,1,84,124,1,0,17,1,139,125,1,0,1,1,2,21,1,47,0,0, 
  0,165,26,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,199,125,1,0,1,8,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0, 
  0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6, 
  128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,215,126,1,0,17,1,4,130,1,0,1,4,15,1,215,126,1,0,17,1,31,129,1,0,1,4,15,1,215,126,1,0,17,1,58,128,1,0,1, 
  4,15,1,215,126,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,215,126,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,215,126,1,0,1,4,19,85,0,0,0,236,0, 
  0,0,1,0,17,1,215,126,1,0,1,4,15,1,215,126,1,0,17,1,48,42,1,0,1,4,15,1,215,126,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,215,126,1,0,1,4,15, 
  1,215,126,1,0,17,1,106,41,1,0,1,4,15,1,215,126,1,0,17,1,165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,0,196,130,84,144,3,131,82,120,6,128,83,0,200, 
  129,52,8,71,127,53,16,198,128,14,248,195,128,15,248,67,126,85,240,132,129,94,96,4,128,99,144,136,128,103,128,5,128,107,112,2,128,108,112,7,128,109,0,3,128,19,84,0,0,0,234,0,0,0,1,0,17,1,215,126, 
  1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,215,126,1,0,1,15,1,215,126,1,0,17,1,31,52,1,0,1,1,19,15,0,0,0,58,0,0,0,3,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1, 
  215,126,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,215,126,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,215,126,1,0,1,19,14,0,0,0,44,0,0,0,5,0,14,1,19,53,0,0,0,160, 
  0,0,0,1,0,17,1,215,126,1,0,1,15,1,215,126,1,0,17,1,43,83,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,215,126,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,215,126,1,0, 
  1,19,82,0,0,0,229,0,0,0,1,0,17,1,215,126,1,0,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,106,128,1,128,103,224,65,128,127,240,0,128,19,106,0,0,0,39,1,0,0,1, 
  0,17,1,252,127,1,0,1,19,103,0,0,0,29,1,0,0,4,0,1,1,2,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128, 
  57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,135,128,1,0,1,4,15,1,135,128,1,0,17,1,202,7,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,70,200,1, 
  128,13,144,2,128,46,40,130,127,91,56,65,128,107,48,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,135,128,1,0,1,19,107,0,0,0,41,1,0,0,2,0,1,15,1,135,128,1,0,17,1,236,128,1,0, 
  1,19,70,0,0,0,207,0,0,0,1,0,17,1,135,128,1,0,1,2,21,1,39,0,0,0,89,27,2,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19, 
  13,0,0,0,41,0,0,0,1,0,1,21,1,76,0,0,0,182,243,1,0,6,0,0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0, 
  143,0,0,0,1,0,17,1,108,129,1,0,1,4,15,1,108,129,1,0,17,1,202,7,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,108,48,1,128,13,144,2,128,46,40,130,128,91,56, 
  1,128,70,200,1,128,1,19,70,0,0,0,208,0,0,0,1,0,17,1,108,129,1,0,1,19,108,0,0,0,42,1,0,0,2,0,1,15,1,108,129,1,0,17,1,209,129,1,0,1,19,70,0,0,0,207,0,0,0,1, 
  0,17,1,108,129,1,0,1,2,21,1,39,0,0,0,174,27,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,94,248,0,128,8,4,17,1,129,6,0,0,1,19,13,0,0,0,41,0,0,0,1,0,1, 
  21,1,47,0,0,0,44,242,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,52,130,1,0,1,2,21,1,76,0,0,0,182,243,1,0,6,0, 
  0,0,2,0,0,0,52,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,57,88,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,129,130,1,0,1,4,15,1,129,130,1,0,17,1,202, 
  7,0,0,1,2,21,0,145,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,70,0,67,129,13,248,195,128,46,144,131,127,91,112,194,128,109,104,2,128,110,216,1,128,111,112,1,128,15,1,129,130,1,0,17,1,19, 
  131,1,0,1,19,111,0,0,0,45,1,0,0,1,0,17,1,129,130,1,0,1,1,19,70,0,0,0,208,0,0,0,1,0,17,1,129,130,1,0,1,19,110,0,0,0,44,1,0,0,1,0,17,1,129,130,1,0,1,15, 
  1,129,130,1,0,17,1,135,193,0,0,1,19,70,0,0,0,207,0,0,0,1,0,17,1,129,130,1,0,1,2,21,1,94,0,0,0,202,246,1,0,7,0,0,0,2,0,0,0,52,16,2,128,1,112,65,129,2,112,129, 
  128,7,112,129,128,6,112,1,128,11,128,2,128,57,120,1,128,8,4,19,46,0,0,0,143,0,0,0,1,0,17,1,114,131,1,0,1,4,15,1,114,131,1,0,17,1,202,7,0,0,1,4,19,109,0,0,0,43,1,0, 
  0,4,0,14,1,2,21,0,122,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,70,72,2,129,13,64,3,128,46,216,130,127,91,184,129,128,110,88,1,128,111,80,1,128,1,19,111,0,0,0,46,1,0,0,2,0, 
  1,19,70,0,0,0,208,0,0,0,1,0,17,1,114,131,1,0,1,19,110,0,0,0,44,1,0,0,1,0,17,1,114,131,1,0,1,15,1,114,131,1,0,17,1,237,131,1,0,1,19,70,0,0,0,207,0,0,0,1, 
  0,17,1,114,131,1,0,1,2,21,1,39,0,0,0,79,246,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,94,240,0,128,4,17,1,129,6,0,0,1,8,19,13,0,0,0,41,0,0,0,1,0,1, 
  21,0,93,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,0,224,10,128,83,8,199,131,82,152,7,128,19,24,138,127,52,32,9,129,53,144,8,129,54,40,8,128,103,96,4,128,84,160,6,128,85,16,6,128,94,128, 
  5,128,107,208,3,128,108,64,3,128,109,176,2,128,14,128,10,127,47,136,9,128,99,240,4,128,19,83,0,0,0,233,0,0,0,1,0,17,1,32,132,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,32,132,1, 
  0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,32,132,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,32,132,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,32,132,1,0,1,19,82,0,0, 
  0,228,0,0,0,1,0,17,1,32,132,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,32,132,1,0,1,15,1,32,132,1,0,17,1,31,52,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,32,132, 
  1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,32,132,1,0,1,15,1,32,132,1,0,17,1,100,136,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,32,132,1,0,1,15,1,32,132,1,0,17,1, 
  41,136,1,0,1,19,19,0,0,0,62,0,0,0,1,0,17,1,32,132,1,0,1,15,1,32,132,1,0,17,1,126,133,1,0,1,19,0,0,0,0,0,0,0,0,3,0,1,1,2,21,1,47,0,0,0,247,27,2,0, 
  5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,185,133,1,0,1,8,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16, 
  0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102, 
  152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,201,134,1,0,17,1,4,130,1,0,1,4,15,1,201,134,1,0,17,1,31,129,1,0,1,4,15,1,201,134,1,0,17,1,58,128,1,0,1,4,15,1,201,134,1, 
  0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,201,134,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,201,134,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1, 
  201,134,1,0,1,4,15,1,201,134,1,0,17,1,48,42,1,0,1,4,15,1,201,134,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,201,134,1,0,1,4,15,1,201,134,1,0,17, 
  1,106,41,1,0,1,4,15,1,201,134,1,0,17,1,165,23,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,83,104,135,130,84,136,4,131,82,96,3,128,19,240,67,127,52,128,69,127,53, 
  232,197,128,14,240,195,128,47,112,2,129,85,248,131,129,94,248,7,128,99,216,134,128,103,208,2,128,107,136,8,128,108,72,6,128,109,240,4,128,19,19,0,0,0,63,0,0,0,3,0,1,19,82,0,0,0,230,0,0,0, 
  1,0,17,1,201,134,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,201,134,1,0,1,1,19,53,0,0,0,162,0,0,0,1,0,17,1,201,134,1,0,1,15,1,201,134,1,0,17,1,31,52,1,0,1,19, 
  83,0,0,0,233,0,0,0,1,0,17,1,201,134,1,0,1,15,1,201,134,1,0,17,1,237,135,1,0,1,19,14,0,0,0,51,0,0,0,3,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,201,134,1,0,1, 
  19,82,0,0,0,229,0,0,0,1,0,17,1,201,134,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,201,134,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,201,134,1,0,1,19,84,0,0,0,234, 
  0,0,0,1,0,17,1,201,134,1,0,1,2,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,15,53,1,0,1, 
  19,14,0,0,0,52,0,0,0,3,0,14,1,21,1,47,0,0,0,247,27,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,175,55,1,0,1, 
  19,14,0,0,0,56,0,0,0,1,0,1,21,1,47,0,0,0,247,27,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,159,136,1,0,1,8,19, 
  14,0,0,0,49,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2, 
  128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,175,137,1,0,17,1,4,130,1,0,1,4,15,1,175,137,1,0,17,1,31,129,1, 
  0,1,4,15,1,175,137,1,0,17,1,58,128,1,0,1,4,15,1,175,137,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,175,137,1,0,1,4,19,85,0,0,0,237,0,0,0,1, 
  0,17,1,175,137,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,175,137,1,0,1,4,15,1,175,137,1,0,17,1,48,42,1,0,1,4,15,1,175,137,1,0,17,1,205,41,1,0,1,4,19,47,0,0, 
  0,154,0,0,0,1,0,17,1,175,137,1,0,1,4,15,1,175,137,1,0,17,1,106,41,1,0,1,4,15,1,175,137,1,0,17,1,170,58,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0, 
  0,47,176,200,130,84,96,6,131,82,88,7,128,83,200,198,129,52,72,72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2, 
  128,19,83,0,0,0,233,0,0,0,1,0,17,1,175,137,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,175,137,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,175,137,1,0,1,19,82,0,0,0, 
  230,0,0,0,1,0,17,1,175,137,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,175,137,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,175,137,1,0,1,19,53,0,0,0,162,0,0,0,1,0, 
  17,1,175,137,1,0,1,15,1,175,137,1,0,17,1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,175,137,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,175,137,1,0,1,19,14,0,0, 
  0,45,0,0,0,3,0,1,15,1,175,137,1,0,17,1,148,141,1,0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,175,137,1,0,1,15,1,175,137,1,0,17,1,230,138,1,0,1,1,2,21,1,47,0,0,0, 
  111,229,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,34,139,1,0,1,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0,0, 
  142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128, 
  101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,50,140,1,0,17,1,4,130,1,0,1,4,15,1,50,140,1,0,17,1,31,129,1,0,1,4,15,1,50,140,1,0,17,1,58,128,1,0,1,4, 
  15,1,50,140,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,50,140,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,50,140,1,0,1,4,19,85,0,0,0,236,0,0, 
  0,1,0,17,1,50,140,1,0,1,4,15,1,50,140,1,0,17,1,48,42,1,0,1,4,15,1,50,140,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,50,140,1,0,1,4,15,1, 
  50,140,1,0,17,1,106,41,1,0,1,4,15,1,50,140,1,0,17,1,165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,24,198,130,84,144,3,131,82,0,3,128,83,8,199,129, 
  52,152,71,127,53,32,197,128,14,24,197,128,15,24,69,126,85,248,131,129,94,112,2,128,99,0,136,128,103,120,6,128,107,144,8,128,108,136,5,128,109,136,4,128,19,82,0,0,0,228,0,0,0,1,0,17,1,50,140,1, 
  0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,50,140,1,0,1,15,1,50,140,1,0,17,1,31,52,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,50,140,1,0,1,19,83,0,0,0,233,0,0,0, 
  1,0,17,1,50,140,1,0,1,1,19,14,0,0,0,44,0,0,0,5,0,14,1,19,83,0,0,0,232,0,0,0,1,0,17,1,50,140,1,0,1,19,15,0,0,0,58,0,0,0,3,0,1,19,82,0,0,0,230,0, 
  0,0,1,0,17,1,50,140,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,50,140,1,0,1,15,1,50,140,1,0,17,1,87,141,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,50,140,1,0,1, 
  19,84,0,0,0,234,0,0,0,1,0,17,1,50,140,1,0,1,2,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17, 
  1,171,64,1,0,1,19,14,0,0,0,46,0,0,0,5,0,14,14,1,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4, 
  17,1,153,66,1,0,1,19,14,0,0,0,47,0,0,0,3,0,14,1,21,1,69,1,0,0,206,157,1,0,19,0,0,0,4,0,0,0,96,72,4,128,1,240,130,128,2,240,66,130,65,176,8,128,100,216,3,128,69,208, 
  7,131,6,240,130,129,7,240,2,128,88,232,5,128,73,240,6,129,74,128,134,129,66,64,8,128,70,96,135,129,89,80,5,128,62,184,9,128,63,72,9,128,90,184,4,128,101,104,3,128,102,248,2,128,8,4,15,1,22,143, 
  1,0,17,1,4,130,1,0,1,4,15,1,22,143,1,0,17,1,31,129,1,0,1,4,15,1,22,143,1,0,17,1,58,128,1,0,1,4,15,1,22,143,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0, 
  0,1,0,17,1,22,143,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,22,143,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,22,143,1,0,1,4,15,1,22,143,1,0,17,1,228,162,0, 
  0,1,4,15,1,22,143,1,0,17,1,158,162,0,0,1,4,15,1,22,143,1,0,17,1,6,162,0,0,1,4,15,1,22,143,1,0,17,1,48,42,1,0,1,4,15,1,22,143,1,0,17,1,205,41,1,0,1,4,19, 
  47,0,0,0,154,0,0,0,1,0,17,1,22,143,1,0,1,4,15,1,22,143,1,0,17,1,106,41,1,0,1,4,15,1,22,143,1,0,17,1,170,58,1,0,1,2,21,0,93,1,0,0,255,255,255,255,17,0,0,0, 
  4,0,0,0,0,224,10,128,83,8,199,131,82,152,7,128,19,24,138,127,52,32,9,129,53,144,8,129,54,40,8,128,103,96,4,128,84,160,6,128,85,16,6,128,94,128,5,128,107,208,3,128,108,64,3,128,109,176,2,128, 
  14,128,10,127,47,136,9,128,99,240,4,128,19,83,0,0,0,233,0,0,0,1,0,17,1,22,143,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,22,143,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17, 
  1,22,143,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,22,143,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,22,143,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,22,143,1,0,1, 
  19,53,0,0,0,162,0,0,0,1,0,17,1,22,143,1,0,1,15,1,22,143,1,0,17,1,31,52,1,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,22,143,1,0,1,19,53,0,0,0,160,0,0,0,1,0, 
  17,1,22,143,1,0,1,15,1,22,143,1,0,17,1,30,147,1,0,1,19,14,0,0,0,55,0,0,0,1,0,17,1,22,143,1,0,1,15,1,22,143,1,0,17,1,227,146,1,0,1,19,19,0,0,0,62,0,0,0, 
  1,0,17,1,22,143,1,0,1,15,1,22,143,1,0,17,1,116,144,1,0,1,19,0,0,0,0,1,0,0,0,3,0,1,1,2,21,1,47,0,0,0,247,27,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112, 
  193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,175,144,1,0,1,8,19,14,0,0,0,53,0,0,0,1,0,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130, 
  128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15, 
  1,191,145,1,0,17,1,4,130,1,0,1,4,15,1,191,145,1,0,17,1,31,129,1,0,1,4,15,1,191,145,1,0,17,1,58,128,1,0,1,4,15,1,191,145,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0, 
  238,0,0,0,1,0,17,1,191,145,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,191,145,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,191,145,1,0,1,4,15,1,191,145,1,0,17,1, 
  48,42,1,0,1,4,15,1,191,145,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,191,145,1,0,1,4,15,1,191,145,1,0,17,1,106,41,1,0,1,4,15,1,191,145,1,0,17, 
  1,165,23,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,83,120,133,130,84,32,8,131,82,152,6,128,19,112,66,127,52,184,71,127,53,136,196,128,14,112,194,128,47,8,3,129,85,104,131, 
  129,94,136,8,128,99,248,131,128,103,8,6,128,107,232,4,128,108,120,2,128,109,40,7,128,1,19,83,0,0,0,232,0,0,0,1,0,17,1,191,145,1,0,1,19,19,0,0,0,63,0,0,0,3,0,1,19,53,0,0, 
  0,162,0,0,0,1,0,17,1,191,145,1,0,1,19,82,0,0,0,229,0,0,0,1,0,17,1,191,145,1,0,1,19,14,0,0,0,51,0,0,0,3,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,191,145,1, 
  0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,191,145,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,191,145,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,191,145,1,0,1,19,83,0,0, 
  0,233,0,0,0,1,0,17,1,191,145,1,0,1,15,1,191,145,1,0,17,1,237,135,1,0,1,15,1,191,145,1,0,17,1,31,52,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,191,145,1,0,1,2,21, 
  1,47,0,0,0,247,27,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,175,55,1,0,1,8,19,14,0,0,0,56,0,0,0,1,0,1,21,1, 
  47,0,0,0,247,27,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,89,147,1,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21,1,15, 
  1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232,3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66, 
  144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152,7,128,8,4,15,1,105,148,1,0,17,1,4,130,1,0,1,4,15,1,105,148,1,0,17,1,31,129,1,0,1,4,15,1,105,148,1,0,17,1,58,128,1, 
  0,1,4,15,1,105,148,1,0,17,1,147,42,1,0,1,4,19,85,0,0,0,238,0,0,0,1,0,17,1,105,148,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,105,148,1,0,1,4,19,85,0,0,0, 
  236,0,0,0,1,0,17,1,105,148,1,0,1,4,15,1,105,148,1,0,17,1,48,42,1,0,1,4,15,1,105,148,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,105,148,1,0,1, 
  4,15,1,105,148,1,0,17,1,106,41,1,0,1,4,15,1,105,148,1,0,17,1,170,58,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,176,200,130,84,96,6,131,82,88,7,128,83, 
  200,198,129,52,72,72,127,53,232,199,128,14,168,201,128,15,64,73,126,85,208,133,129,94,64,5,128,99,176,132,128,103,32,4,128,107,144,3,128,108,0,3,128,109,112,2,128,19,83,0,0,0,233,0,0,0,1,0,17,1, 
  105,148,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,105,148,1,0,1,19,84,0,0,0,234,0,0,0,1,0,17,1,105,148,1,0,1,19,82,0,0,0,230,0,0,0,1,0,17,1,105,148,1,0,1,19, 
  82,0,0,0,229,0,0,0,1,0,17,1,105,148,1,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,105,148,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,105,148,1,0,1,15,1,105,148,1,0,17, 
  1,204,159,0,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,105,148,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,105,148,1,0,1,19,14,0,0,0,45,0,0,0,3,0,1,15,1,105,148,1,0, 
  17,1,148,141,1,0,1,19,15,0,0,0,57,0,0,0,1,0,17,1,105,148,1,0,1,15,1,105,148,1,0,17,1,160,149,1,0,1,1,2,21,1,47,0,0,0,111,229,1,0,5,0,0,0,2,0,0,0,6,48, 
  1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,220,149,1,0,1,19,14,0,0,0,48,0,0,0,3,0,14,1,21,1,15,1,0,0,142,227,1,0,16,0,0,0,4,0,0,0,96,232, 
  3,128,1,144,130,128,2,144,66,130,65,0,7,128,100,120,3,128,69,32,198,129,6,144,194,129,7,144,2,128,88,136,5,128,89,240,4,128,90,88,4,128,66,144,6,128,101,8,3,128,102,152,2,128,62,8,8,128,63,152, 
  7,128,8,4,15,1,236,150,1,0,17,1,4,130,1,0,1,4,15,1,236,150,1,0,17,1,31,129,1,0,1,4,15,1,236,150,1,0,17,1,58,128,1,0,1,4,15,1,236,150,1,0,17,1,147,42,1,0,1,4, 
  19,85,0,0,0,238,0,0,0,1,0,17,1,236,150,1,0,1,4,19,85,0,0,0,237,0,0,0,1,0,17,1,236,150,1,0,1,4,19,85,0,0,0,236,0,0,0,1,0,17,1,236,150,1,0,1,4,15,1,236, 
  150,1,0,17,1,48,42,1,0,1,4,15,1,236,150,1,0,17,1,205,41,1,0,1,4,19,47,0,0,0,154,0,0,0,1,0,17,1,236,150,1,0,1,4,15,1,236,150,1,0,17,1,106,41,1,0,1,4,15,1, 
  236,150,1,0,17,1,165,23,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,47,200,199,130,84,96,7,131,82,176,5,128,83,112,195,129,52,184,72,127,53,8,195,128,14,0,195,128,15,0, 
  67,126,85,64,134,129,94,40,8,128,99,32,133,128,103,208,6,128,107,112,2,128,108,0,4,128,109,144,4,128,19,84,0,0,0,234,0,0,0,1,0,17,1,236,150,1,0,1,1,19,14,0,0,0,44,0,0,0,5,0, 
  14,1,19,53,0,0,0,161,0,0,0,1,0,17,1,236,150,1,0,1,19,83,0,0,0,232,0,0,0,1,0,17,1,236,150,1,0,1,19,83,0,0,0,233,0,0,0,1,0,17,1,236,150,1,0,1,19,82,0,0, 
  0,229,0,0,0,1,0,17,1,236,150,1,0,1,19,53,0,0,0,160,0,0,0,1,0,17,1,236,150,1,0,1,19,53,0,0,0,162,0,0,0,1,0,17,1,236,150,1,0,1,19,82,0,0,0,230,0,0,0,1, 
  0,17,1,236,150,1,0,1,15,1,236,150,1,0,17,1,31,52,1,0,1,19,15,0,0,0,58,0,0,0,3,0,1,19,82,0,0,0,228,0,0,0,1,0,17,1,236,150,1,0,1,15,1,236,150,1,0,17,1,87, 
  141,1,0,1,2,15,1,111,197,0,0,17,1,30,152,1,0,1,21,1,64,0,0,0,139,28,2,0,2,0,0,0,1,0,0,0,28,104,1,128,29,208,0,128,4,19,33,0,0,0,93,0,0,0,1,0,17,1,131,152, 
  1,0,1,4,19,32,0,0,0,92,0,0,0,1,0,17,1,95,152,1,0,1,2,21,1,35,0,0,0,17,31,2,0,1,0,0,0,0,0,0,0,8,176,0,128,4,19,8,0,0,0,26,0,0,0,2,0,1,2,21, 
  1,35,0,0,0,17,31,2,0,1,0,0,0,0,0,0,0,8,176,0,128,4,19,8,0,0,0,27,0,0,0,2,0,1,2,15,1,111,197,0,0,17,1,180,152,1,0,1,21,1,241,1,0,0,38,195,1,0,24,0, 
  0,0,4,0,0,0,32,168,142,132,1,144,195,131,2,144,195,131,35,88,13,128,36,232,76,132,37,120,76,132,38,8,140,131,39,152,11,128,40,40,11,128,41,184,10,128,42,72,10,128,43,136,9,128,44,200,8,128,45,88, 
  8,128,30,24,15,128,47,232,7,130,33,56,206,128,34,200,13,128,48,120,7,128,49,8,7,128,54,152,6,128,84,128,5,128,85,104,4,128,95,152,3,128,8,4,15,1,121,138,0,0,15,1,143,139,0,0,15,1,155,139, 
  0,0,17,1,13,138,0,0,1,4,19,75,0,0,0,215,0,0,0,1,0,19,37,0,0,0,121,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,1,4,19,74,0,0,0,214,0,0,0,1,0,19,37,0,0, 
  0,120,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,1,4,15,1,1,138,0,0,17,1,98,132,0,0,1,4,15,1,86,132,0,0,17,1,136,131,0,0,1,4,15,1,86,132,0,0,17,1,186,130,0,0, 
  1,4,15,1,86,132,0,0,17,1,252,128,0,0,1,4,15,1,240,128,0,0,17,1,209,125,0,0,1,4,19,37,0,0,0,119,0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,1,4,19,37,0,0,0,118, 
  0,0,0,1,0,19,9,0,0,0,31,0,0,0,1,0,1,4,15,1,197,125,0,0,17,1,110,125,0,0,1,4,15,1,98,125,0,0,17,1,11,125,0,0,1,4,15,1,98,125,0,0,17,1,180,124,0,0,1,4, 
  15,1,98,125,0,0,17,1,93,124,0,0,1,4,15,1,98,125,0,0,17,1,6,124,0,0,1,4,15,1,98,125,0,0,17,1,175,123,0,0,1,4,15,1,98,125,0,0,17,1,88,123,0,0,1,4,15,1,98,125, 
  0,0,17,1,1,123,0,0,1,4,15,1,98,125,0,0,17,1,170,122,0,0,1,4,15,1,98,125,0,0,17,1,83,122,0,0,1,4,15,1,98,125,0,0,17,1,252,121,0,0,1,4,15,1,240,121,0,0,17,1, 
  192,120,0,0,1,2,15,1,111,197,0,0,17,1,179,154,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,17,1,71,113,0,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47,240,128,128,45,48,193,127,95,48,1,128,3,17,1,82,155,1,0,1,3,17,1,219,156, 
  1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,165,156,1,0,1,3,17,1,125,155,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42, 
  176,0,128,3,17,1,210,155,1,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,80,156,1,0,1,3,17,1, 
  80,156,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,8,1,128,47,208,0,128,3,18,7,0,0,0,1,3,17,1,50,156,1,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0, 
  0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,80,156,1,0,1,3,17,1,80,156,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  47,176,0,128,3,18,7,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,50,156,1,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,80,156,1,0,1,3,17,1,80,156,1,0,1,2,21,2,53,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5, 
  112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,165,156,1,0,1,3,18,6,0,0,0,1,2,18,57,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208, 
  192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,2,0,0,0,1,18,1,0,0,0,1,21,4,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,61,208,0,128,47,16,193,127,3,17,1,134,157,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,208,0,128,33,8,1,128,3,18,3,0,0,0,1,3,17,1,176,157, 
  1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,4,0,0,0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128, 
  114,208,1,128,115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,124,162,1,0,1,3,17,1,153,161,1,0,1,3,17,1,129,160,1,0,1,3,17,1,6,160,1,0,1,3,17,1,170, 
  159,1,0,1,3,17,1,5,159,1,0,1,3,17,1,107,158,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1, 
  0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,138,158,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  99,176,0,128,3,17,1,169,158,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,200,158,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,112,176,0,128,3,17,1,231,158,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,90,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,111,208,0,128,97,16,193,127,3,17,1,109,159,1,0,1,3,17,1,48,159,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,79,159,1,0,1,2,21, 
  4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,18,88,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,17,1,140,159,1,0,1,2, 
  21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,18,102,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,201,159,1,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,232,159,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,18,101,0,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,37,160,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,68,160, 
  1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,99,160,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,96, 
  0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,101,144,129,128,97,208,193,127,117,16,1,128,111,80,1,128,3,17,1,92,161,1,0,1,3,17,1,62,161,1,0,1,3,17,1,1,161, 
  1,0,1,3,17,1,196,160,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,227,160,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,115,176,0,128,3,18,89,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,32,161,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,107,176,0,128,3,18,70,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,69,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,115,176,0,128,3,17,1,123,161,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,100,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,101,176,0,128,3,17,1,184,161,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,100,16,1,128,115,208,0,128,3,17,1,63,162,1,0,1,3,17,1,227,161,1,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,2,162,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,33,162,1,0,1, 
  2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,62,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,94,162,1,0, 
  1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,73,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,104,208,0,128,101,16,1,128,3,17, 
  1,42,164,1,0,1,3,17,1,167,162,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,198,162,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,45,176,0,128,3,17,1,229,162,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,65,128,116,208,0,128,3,17,1,108,163,1,0,1,3,17,1,16,163,1,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,47,163,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,78,163,1,0, 
  1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,65,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,139,163,1, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,170,163,1,0,1,2,18,63,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0, 
  128,3,17,1,206,163,1,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,237,163,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101, 
  176,0,128,3,17,1,12,164,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,66,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  105,176,0,128,3,17,1,73,164,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,104,164,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,116,176,0,128,3,18,74,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,82,155,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,2,18,84,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,4,176,0,128,3,17,1,219,164,1,0,1,1,21,4,69,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,240,1,128,45,112,1,128,95,112,129,128,47,48,193,127,115,176,1,128,3,17,1,82,155,1,0, 
  1,3,17,1,219,156,1,0,1,3,17,1,135,165,1,0,1,3,18,52,0,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17, 
  1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240, 
  192,127,121,48,1,128,3,17,1,219,156,1,0,1,3,17,1,221,165,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0, 
  0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,109,240,0,128,3,17,1,51,166,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,98,240,0,128,45,48,65,128,95,48,1,128,3,17, 
  1,137,166,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,48,1,128,3,17,1,219,156,1,0,1,3,17,1,223,166,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,108,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17, 
  1,53,167,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,95,240,128,128,45,240,192,127,115,48,1,128,3,17,1,219,156,1,0,1,3,17,1,139,167,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17, 
  1,219,156,1,0,1,1,18,64,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,21,4,127,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,8,3,128,47,208,65,129,34,128,3,128,91,192,67,129,36,80,66,129,45, 
  16,2,128,95,16,2,128,39,136,130,126,99,200,2,128,116,64,3,128,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,58,170,1,0,1,3,17,1,229,169,1,0,1,3,18, 
  52,0,0,0,1,3,17,1,22,169,1,0,1,3,17,1,139,168,1,0,1,3,18,45,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17, 
  1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,235,168,1,0,1,3,18,85, 
  0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,139,168,1,0,1,3,17,1,139,168,1,0,1,2,21, 
  2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,139,168,1,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,95,240,128,128,45,240,192,127,107,48,1,128,3,17,1,219,156,1,0,1,3,17,1,108,169,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,194,169,1,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,103,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0, 
  128,3,18,91,0,0,0,1,1,18,57,0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,40,65,128,95,40,1,128,3,18,104,0,0,0,1,3,17,1,219,156,1,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128, 
  3,17,1,154,170,1,0,1,3,18,121,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,197,170,1,0, 
  1,3,17,1,197,170,1,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,197,170,1,0,1,2,21,4,41,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,154,170,1,0,1,3,18,121,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128, 
  2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,197,170,1,0,1,3,17,1,197,170,1,0,1,2,21,4,105,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,144,2,128,47,144,65,129,34,8,3,128,99, 
  80,2,128,116,200,2,128,45,208,1,128,95,208,1,128,39,16,130,126,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,17,1,58,170,1,0,1,3,17,1,229,169,1,0,1,3,18,52,0,0,0,1,3,17, 
  1,22,169,1,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1, 
  3,17,1,219,156,1,0,1,2,21,4,173,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,144,2,128,42,64,195,129,34,120,195,127,43,208,66,130,116,80,2,128,45,248,67,129,94,184,3,128,39,40,133,128,58, 
  112,4,128,63,56,132,128,93,176,4,128,95,248,3,128,99,232,68,128,123,8,3,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1, 
  3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,112,0,0,0,1,3,17,1,148,173,1,0,1,3,18,46,0,0,0,1,3,17,1,229,169,1,0,1,3,17,1,169,172,1, 
  0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,41, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,9,173,1,0,1,3,18,121,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128, 
  5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,52,173,1,0,1,3,17,1,52,173,1,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2, 
  16,193,127,3,16,1,128,3,17,1,52,173,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,9,173,1,0,1,3,18,121,0,0,0,1,21,2,54, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,52,173,1,0,1,3,17,1,52,173,1,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,179,173,1,0,1,2,18,94,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,3,18,127,0,0,0,1,1,21,4, 
  34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,23,174,1,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,23,174,1, 
  0,1,2,18,115,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,23,174,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,23,174,1,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,193,174,1,0,1,3,18,85,0,0,0,1,21,2, 
  54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,97,174,1,0,1,3,17,1,97,174,1,0,1,2,21,2,42,0,0,0,255, 
  255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,97,174,1,0,1,2,18,52,0,0,0,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42, 
  8,1,128,43,208,0,128,3,18,124,0,0,0,1,3,18,123,0,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,48,1,128,3,17,1, 
  219,156,1,0,1,3,17,1,108,169,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,21,4,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1, 
  3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,173,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,144,2,128,42,64,195,129,34,120,195,127,43,208,66,130,116,80,2,128,45,248,67,129,94, 
  184,3,128,39,40,133,128,58,56,4,128,63,120,132,128,93,176,4,128,95,248,3,128,99,232,68,128,123,8,3,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0, 
  1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,17,1,173,176,1,0,1,3,18,112,0,0,0,1,3,18,46,0,0,0,1,3,17,1,229,169,1, 
  0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219, 
  156,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,204,176,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,3,18, 
  127,0,0,0,1,2,21,4,93,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,40,112,130,129,45,176,1,128,34,168,2,128,39,240,65,128,95,176,65,128,99,48,2,128,116,112,1,128,3,17,1,126,177,1,0,1, 
  3,17,1,219,156,1,0,1,3,17,1,58,170,1,0,1,3,17,1,229,169,1,0,1,3,18,52,0,0,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129, 
  128,45,104,193,127,107,240,0,128,3,17,1,108,169,1,0,1,3,18,56,0,0,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17, 
  1,219,156,1,0,1,1,21,4,206,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,120,5,128,91,248,197,131,34,216,3,128,99,184,5,128,36,152,132,128,95,88,4,128,116,176,2,128,39,48,6,128,40,240,2, 
  128,41,64,5,128,42,160,131,125,43,48,131,125,124,8,5,128,45,88,4,128,94,24,4,128,63,208,132,125,123,104,3,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0, 
  0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0, 
  0,1,3,17,1,224,178,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5, 
  48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17,1,11,179,1, 
  0,1,3,17,1,179,173,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,42,179,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  116,176,0,128,3,18,61,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,47,208,0,128,41,16,193,127,3,17,1,82,155,1,0,1,3,18,53,0,0,0,1,21,2,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,124,240,0,128,41,104, 
  65,128,47,40,1,128,3,18,81,0,0,0,1,3,17,1,82,155,1,0,1,3,18,53,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1, 
  0,1,3,17,1,37,157,1,0,1,2,21,4,206,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,8,5,128,91,248,197,131,34,216,3,128,99,184,5,128,36,152,132,128,95,88,4,128,116,176,2,128,39,48,6, 
  128,40,240,2,128,41,128,5,128,42,160,131,125,43,48,131,125,124,72,5,128,45,88,4,128,94,24,4,128,63,208,132,125,123,104,3,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1, 
  3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,17,1,0,181,1,0,1, 
  3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3, 
  112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17, 
  1,11,179,1,0,1,3,17,1,204,176,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,104,3,128,41,24,4,128,34,144,132,129,91,208,196,129,36,176,194,129,45,112,2,128,47,48,194, 
  128,39,232,194,127,58,80,4,128,95,112,2,128,99,40,3,128,116,160,67,128,124,224,3,128,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,58,170,1,0,1,3,17,1,229, 
  169,1,0,1,3,18,52,0,0,0,1,3,17,1,22,169,1,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,139,168,1,0,1,3,18,45,0,0,0,1,21,2,54,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,11,179,1,0,1,2,21,4,183,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,24,4,128,41,136,4,128,34,232,130,129,91,64,5,130,36,104,3,130, 
  45,40,3,128,94,216,3,128,39,120,133,128,58,192,4,128,63,160,67,128,95,40,3,128,99,0,133,128,116,112,130,128,123,176,2,128,124,80,4,128,3,17,1,26,175,1,0,1,3,18,10,0,0,0,1,3,17,1,97,174, 
  1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,3,182,1, 
  0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157, 
  1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,81,183,1,0,1,21,2,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,81,183,1,0,1,2,18,115,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,81, 
  183,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,81,183,1,0,1,1,21,4,172,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,192,3, 
  128,41,48,4,128,34,200,130,129,91,232,196,129,36,72,195,129,45,8,3,128,94,128,3,128,39,32,133,128,58,104,4,128,95,8,3,128,99,168,132,128,116,80,130,128,123,144,2,128,124,248,3,128,3,17,1,26,175,1,0, 
  1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1, 
  3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1, 
  128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,1,128,107,40,65,128,125,240,0,128,3,18,11,0, 
  0,0,1,3,17,1,233,184,1,0,1,3,18,55,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37, 
  157,1,0,1,3,17,1,7,185,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,119,176,0,128,3,18,126,0,0,0,1,2,18,125,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,4,176,0,128,3,17,1,7,185,1,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,3,18,11,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,7,185,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,125,208,0,128,107,8,193,127,3,18,11,0,0,0,1,3,17,1,233,184,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1, 
  0,1,3,17,1,37,157,1,0,1,3,17,1,7,185,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,104,3,128,41,216,3,128,34,112,130,129,91,144,196,129,36,240,194,129,45,176,2, 
  128,94,40,3,128,39,200,132,128,58,16,4,128,95,176,2,128,99,80,4,128,116,48,66,128,124,160,3,128,3,17,1,26,175,1,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1, 
  3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1, 
  21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,172,0,0, 
  0,255,255,255,255,14,0,0,0,3,0,0,0,40,192,3,128,41,48,4,128,34,144,130,129,91,232,4,130,36,16,3,130,45,208,2,128,94,128,3,128,39,32,133,128,58,104,4,128,63,72,67,128,95,208,2,128,99,168,4, 
  128,116,80,66,128,124,248,3,128,3,17,1,26,175,1,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0, 
  0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,206,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0, 
  58,120,5,128,91,248,197,131,34,216,3,128,99,184,5,128,36,152,132,128,95,88,4,128,116,176,2,128,39,48,6,128,40,240,2,128,41,64,5,128,42,160,131,125,43,48,131,125,124,8,5,128,45,88,4,128,94,24,4,128, 
  63,208,132,125,123,104,3,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0, 
  1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1, 
  3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1, 
  0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,99,16,1,128,41,80,1,128,34,200,1,128,39,136,65,127,3,17,1,11,189,1,0,1,3,18,53,0,0,0,1,3,17,1,58,170,1,0,1, 
  3,17,1,139,168,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,29,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,58,176,0,128,3,18,104,0,0,0,1,2,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,16,1,128,41,200,1,128,58,72,1,128,47,136,1,128,3,18,81,0,0, 
  0,1,3,17,1,3,182,1,0,1,3,17,1,82,155,1,0,1,3,18,53,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3, 
  17,1,37,157,1,0,1,2,21,4,159,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,98,200,3,128,105,72,3,128,34,184,132,127,91,8,68,129,36,128,68,129,109,8,67,129,102,136,3,128,47,64,4,128,115,200, 
  194,128,116,136,2,128,117,72,2,128,123,16,2,128,3,18,10,0,0,0,1,3,17,1,127,193,1,0,1,3,17,1,171,192,1,0,1,3,17,1,110,192,1,0,1,3,17,1,49,192,1,0,1,3,17,1,149,191,1,0, 
  1,3,17,1,198,190,1,0,1,3,17,1,106,190,1,0,1,3,18,45,0,0,0,1,3,17,1,82,155,1,0,1,3,18,95,0,0,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  111,176,0,128,3,17,1,137,190,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,168,190,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,108,176,0,128,3,18,42,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,54,48,1,128,51,112,65,128,97,240,0,128,3,17,1,57,191,1,0,1,3,17,1,27,191,1,0,1, 
  3,17,1,253,190,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,34,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0, 
  128,3,18,37,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,88,191,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176, 
  0,128,3,17,1,119,191,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,44,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56, 
  16,1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,32,0,0,0,1,3,17,1,19,192,1,0,1,3,17,1,245,191,1,0,1,3,17,1,215,191,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,54,176,0,128,3,18,40,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,35,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,52,176,0,128,3,18,38,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,80,192,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,112,176,0,128,3,18,54,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,141,192,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,114,176,0,128,3,18,30,0,0,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,240,0,128,107,112,65,128,111,48,1,128,3,17,1,66,193,1,0,1,3,17,1, 
  225,192,1,0,1,3,18,47,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,0,193,1,0,1,2,18,48,0,0,0,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,101,176,0,128,3,17,1,36,193,1,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,49,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,97,193,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,43,0,0,0,1,2,21,4,65,0,0,0,255,255,255, 
  255,4,0,0,0,2,0,0,0,56,16,1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,33,0,0,0,1,3,17,1,253,193,1,0,1,3,17,1,223,193,1,0,1,3,17,1,193,193,1,0,1,2,21,4,29, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,3,18,41,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,36,0,0,0,1,2,21,4,29, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,39,0,0,0,1,2,21,4,147,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,98,168,3,128,105,40,3,128,34,88,132,127,91,232,3, 
  129,36,32,4,129,109,232,2,129,102,104,3,128,115,168,2,128,116,104,2,128,117,40,66,128,125,240,1,128,3,18,11,0,0,0,1,3,17,1,127,193,1,0,1,3,17,1,229,194,1,0,1,3,17,1,110,192,1,0,1, 
  3,17,1,49,192,1,0,1,3,17,1,149,191,1,0,1,3,17,1,198,190,1,0,1,3,17,1,106,190,1,0,1,3,18,45,0,0,0,1,3,18,95,0,0,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,2,21,4,64,0,0,0,255,255,255,255,4, 
  0,0,0,2,0,0,0,107,144,65,128,111,80,1,128,114,16,1,128,95,200,65,127,3,17,1,66,193,1,0,1,3,17,1,225,192,1,0,1,3,18,47,0,0,0,1,3,18,56,0,0,0,1,2,21,4,136,0,0,0, 
  255,255,255,255,10,0,0,0,3,0,0,0,98,80,3,128,105,208,2,128,34,0,132,127,91,144,3,129,36,200,3,129,109,144,2,129,102,16,3,128,115,80,2,128,116,16,2,128,117,208,1,128,3,17,1,127,193,1,0,1, 
  3,17,1,171,192,1,0,1,3,17,1,110,192,1,0,1,3,17,1,49,192,1,0,1,3,17,1,149,191,1,0,1,3,17,1,198,190,1,0,1,3,17,1,106,190,1,0,1,3,18,45,0,0,0,1,3,18,95,0,0, 
  0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219, 
  164,1,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,74,196,1,0,1,21,2,54,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,58,0,0,0,255,255,255, 
  255,4,0,0,0,2,0,0,0,107,144,129,128,45,16,1,128,111,80,1,128,95,16,65,127,3,17,1,219,156,1,0,1,3,17,1,246,196,1,0,1,3,17,1,172,196,1,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,47,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156, 
  1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,95,48,129,128,45,48,193,127,107,240,0,128,3,17,1,76,197,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156, 
  1,0,1,1,18,48,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,219,156,1,0,1,3,17,1,162,197,1,0,1,21,2,34,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,110,48,1,128,45,240,64, 
  128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,248,197,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,49,0,0, 
  0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,219,156,1,0,1,1,21,4,147,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,98,168,3,128,105,40,3,128,34,88,132,127,91,232,3,129,36,32,4,129,109,232,2,129,102,104,3,128,115,168,194,128, 
  116,104,2,128,117,40,2,128,123,240,1,128,3,18,10,0,0,0,1,3,17,1,127,193,1,0,1,3,17,1,171,192,1,0,1,3,17,1,110,192,1,0,1,3,17,1,49,192,1,0,1,3,17,1,149,191,1,0,1,3, 
  17,1,198,190,1,0,1,3,17,1,106,190,1,0,1,3,18,45,0,0,0,1,3,18,95,0,0,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5, 
  48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,85,199,1, 
  0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,95,176,0,128,3,18,56,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,36,16,65,128,116,208,0,128,3,17,1,199,199,1,0,1,3,18,95,0,0,0,1,21,2,42,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,111,208,0, 
  128,107,16,193,127,3,17,1,225,192,1,0,1,3,18,47,0,0,0,1,2,21,4,147,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,98,112,3,128,105,240,2,128,34,88,132,127,91,232,67,129,36,32,68,129,93, 
  176,131,128,102,48,3,128,109,176,194,128,115,112,2,128,116,48,2,128,117,240,1,128,3,17,1,127,193,1,0,1,3,17,1,171,192,1,0,1,3,17,1,110,192,1,0,1,3,17,1,49,192,1,0,1,3,17,1,149,191, 
  1,0,1,3,17,1,198,190,1,0,1,3,17,1,106,190,1,0,1,3,18,46,0,0,0,1,3,18,45,0,0,0,1,3,18,95,0,0,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,2,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,219,164,1,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208, 
  192,127,3,17,1,219,156,1,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1, 
  3,17,1,219,164,1,0,1,3,17,1,219,156,1,0,1,2,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,18,52,0,0, 
  0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,86, 
  0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,112,66,128,44,200,1,128,42,56,130,128,43,0,2,128,58,136,65,128,62,80,1,128,3,18,21,0,0,0,1,3,17,1,99,202,1,0,1,3,18,9,0,0,0, 
  1,3,18,25,0,0,0,1,3,18,122,0,0,0,1,3,17,1,58,202,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1, 
  37,157,1,0,1,2,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,8,1,128,43,208,0,128,3,18,124,0,0,0,1,3,18,123,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,58,176,0,128,3,18,94,0,0,0,1,2,21,4,86,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,112,66,128,44,200,1,128,42,56,130,128,43,0,2,128,58,136,65,128,62,80,1,128,3, 
  18,21,0,0,0,1,3,17,1,148,173,1,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,122,0,0,0,1,3,17,1,58,202,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,86,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,112,66,128,44,200,1,128,42,56,130,128,43,0,2,128, 
  58,136,65,128,62,80,1,128,3,18,21,0,0,0,1,3,17,1,173,176,1,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,122,0,0,0,1,3,17,1,58,202,1,0,1,21,2,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,74,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,16,66,128,44,104,1, 
  128,42,216,129,128,43,160,1,128,62,48,1,128,3,18,21,0,0,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,122,0,0,0,1,3,17,1,58,202,1,0,1,21,2,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,176,2,128,47,176,193,129,34,96, 
  3,128,99,112,2,128,116,232,2,128,45,240,65,128,93,40,3,128,39,48,130,126,95,240,1,128,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,17,1,58,170,1,0,1,3,17,1,229,169,1,0,1,3,18, 
  52,0,0,0,1,3,17,1,22,169,1,0,1,3,18,46,0,0,0,1,3,17,1,139,168,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17, 
  1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,172,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,136,3,128,33,192,67,129,34,208,194,129,91,232,4,130,36,80,3, 
  130,45,16,3,128,41,48,4,128,39,32,69,128,47,144,130,128,58,104,4,128,95,16,3,128,99,168,4,128,116,80,66,128,124,248,3,128,3,17,1,26,175,1,0,1,3,17,1,82,155,1,0,1,3,17,1,97,174,1,0, 
  1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,52,0,0,0,1,3,18,116,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1, 
  3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0, 
  1,3,17,1,219,156,1,0,1,2,21,4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,96,3,128,63,232,2,130,34,104,2,128,99,208,195,129,116,240,1,128,45,168,194,128,94,32,3,128,39,16,132,126, 
  93,152,3,128,95,168,2,128,123,48,2,128,3,17,1,26,175,1,0,1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,112,0,0,0,1,3,17,1,16,183,1,0,1,3,18, 
  52,0,0,0,1,3,18,46,0,0,0,1,3,17,1,229,169,1,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17, 
  1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,127,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,8,3,128,63,144,2,130,34,16,2,128,99,120,3,128,116,208,1, 
  128,45,80,194,128,94,200,2,128,39,184,131,126,93,64,3,128,95,80,2,128,3,17,1,26,175,1,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,112,0,0,0,1,3,17,1,16,183,1,0,1, 
  3,18,52,0,0,0,1,3,18,46,0,0,0,1,3,17,1,229,169,1,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128, 
  3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,176,2,128,93,232,2,128,34,240,1,128,99,32,3,128,116, 
  176,1,128,45,48,2,127,94,112,2,128,39,96,67,128,95,48,2,128,3,17,1,26,175,1,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18, 
  46,0,0,0,1,3,17,1,229,169,1,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3, 
  17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,127,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,8,3,128,93,64,3,128,34,72,2,128,99,120,131,129,116,208,1,128,45,136,2,127,94,200, 
  2,128,39,184,67,128,95,136,2,128,123,16,2,128,3,17,1,26,175,1,0,1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1, 
  3,18,46,0,0,0,1,3,17,1,229,169,1,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0, 
  1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,112,2,128,42,32,3,128,34,88,195,127,43,176,2,130,116,48,2,128,45,216,3,129, 
  94,152,3,128,39,200,68,128,63,24,132,128,93,80,4,128,95,216,3,128,99,136,68,128,123,232,2,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18, 
  122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,112,0,0,0,1,3,18,46,0,0,0,1,3,17,1,229,169,1,0,1,3,17,1,169,172,1,0,1,21, 
  2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,90,0,0,0, 
  255,255,255,255,6,0,0,0,2,0,0,0,116,144,1,128,97,144,194,128,110,16,194,128,115,208,1,128,105,80,2,128,118,80,1,128,3,17,1,227,210,1,0,1,3,17,1,166,210,1,0,1,3,17,1,94,210,1,0,1, 
  3,17,1,22,210,1,0,1,3,17,1,248,209,1,0,1,3,17,1,187,209,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17, 
  1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,218,209,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128, 
  3,18,110,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,18,105,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,1,128, 
  117,208,0,128,3,17,1,64,210,1,0,1,3,18,106,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,109,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,112,16,1,128,121,208,0,128,3,17,1,136,210,1,0,1,3,18,107,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,82,0,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,197,210,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,18,108,0,0, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,2,211,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,33, 
  211,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,18,111,0,0,0,1,2,21,4,66,1,0,0,255,255,255,255,27,0,0,0,4,0,0,0,58,200,5,128,65,56,71, 
  133,34,72,5,128,99,88,9,128,36,0,199,132,69,136,6,128,60,136,197,132,39,208,9,128,40,96,4,128,41,168,200,129,42,16,133,125,43,160,196,129,44,240,131,126,45,72,134,129,62,200,198,128,63,120,135,129,73,240,7, 
  128,78,176,199,128,91,152,137,129,93,224,200,129,94,8,6,128,95,72,6,128,97,24,9,128,116,104,8,128,123,216,4,128,124,48,8,128,125,40,4,128,3,18,9,0,0,0,1,3,18,11,0,0,0,1,3,17,1,236,174, 
  1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,58,222,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219, 
  156,1,0,1,3,17,1,66,220,1,0,1,3,18,21,0,0,0,1,3,18,95,0,0,0,1,3,17,1,162,219,1,0,1,3,18,112,0,0,0,1,3,17,1,86,218,1,0,1,3,17,1,176,214,1,0,1,3,18,81, 
  0,0,0,1,3,17,1,88,213,1,0,1,3,18,53,0,0,0,1,3,18,46,0,0,0,1,3,17,1,184,212,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,46, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,48,1,128,3,17,1,219,156,1,0,1,3,17,1,14,213,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,76,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,104,80,1,128,45, 
  16,1,128,107,144,1,128,95,16,193,127,3,17,1,219,156,1,0,1,3,17,1,186,213,1,0,1,3,17,1,108,169,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,219,156,1,0,1,3,17,1,16,214,1,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,110, 
  240,0,128,45,48,65,128,95,48,1,128,3,17,1,102,214,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0, 
  1,1,18,13,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,77,144,1,128,45,16,193,127,95,16,1,128,71,80,193,127,3,17,1,219,156, 
  1,0,1,3,17,1,180,216,1,0,1,3,17,1,18,215,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21, 
  4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,104,215,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,190,215, 
  1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,20,216,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3, 
  208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,106,216, 
  1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,78,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,10,217,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0, 
  1,3,17,1,96,217,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,182,217,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,12,218,1,0,1,3,17,1,219,156,1,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,80,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,172,218,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3, 
  17,1,2,219,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,69,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,88,219,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,219,156,1,0,1,1,18,79,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,83,240,128,128,45,48,193,127,95,48,1,128,3,17,1, 
  248,219,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,75,0,0,0,21,4,34,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0, 
  1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,152,220,1,0,1,21,2,34,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95, 
  48,1,128,3,17,1,238,220,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21, 
  4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,68,221,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,154,221, 
  1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,240,221,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3, 
  208,0,128,3,17,1,219,156,1,0,1,1,18,77,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,129,128,97,240,0,128,62,48,1,128,3,17,1,11, 
  179,1,0,1,3,18,24,0,0,0,1,3,17,1,179,173,1,0,1,2,18,27,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,22,0,0,0,1,1,21,4,104,0,0, 
  0,255,255,255,255,8,0,0,0,3,0,0,0,95,128,2,128,116,0,3,128,58,192,2,128,43,200,1,128,60,64,66,127,45,128,66,128,125,144,1,128,47,0,66,126,3,18,11,0,0,0,1,3,18,25,0,0,0,1,3, 
  17,1,82,155,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,17,1,136,223,1,0,1,3,17,1,50,223,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,104,48,1, 
  128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,186,213,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1, 
  21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,16,1,128,97,208,0,128,3,17,1,11,179,1,0,1,3,18,24,0,0,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,35,224,1,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208, 
  1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208, 
  0,128,45,8,193,127,3,18,56,0,0,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,21,4,42,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,201,224,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,18,58,0,0,0,1,2,21,4,57,0,0,0,255,255,255, 
  255,4,0,0,0,2,0,0,0,40,144,1,128,45,80,1,128,95,80,1,128,47,16,193,127,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,18,52,0,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0, 
  0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,3,17,1,219,156,1,0,1,2,21,4,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,119,208,0,128,47,16,193,127,3,17,1,196,225,1,0,1,3,17,1,82,155,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5, 
  48,1,128,6,240,0,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,164,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,227,225,1, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,2,226,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,60,0, 
  0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,114,208,0,128,47,16,1,128,3,17,1,117,226,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,148,226,1,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,179,226,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,59,0,0,0,1, 
  2,21,4,104,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,192,2,128,116,0,3,128,58,0,2,128,43,200,1,128,60,128,66,127,45,192,66,128,125,144,1,128,47,64,66,126,3,18,11,0,0,0,1,3,18, 
  25,0,0,0,1,3,17,1,112,227,1,0,1,3,17,1,82,155,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,17,1,50,223,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176, 
  0,128,3,18,24,0,0,0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127, 
  3,17,1,160,228,1,0,1,3,17,1,98,228,1,0,1,3,17,1,43,228,1,0,1,3,17,1,6,160,1,0,1,3,17,1,170,159,1,0,1,3,17,1,5,159,1,0,1,3,17,1,107,158,1,0,1,3,17,1,82, 
  155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,111,48,129,128,97,112,193,127,117,240,0,128,3,17,1,92,161,1,0,1,3,17,1,62,161,1,0,1,3,17,1,196,160,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  101,176,0,128,3,17,1,129,228,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,227,161,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,101,176,0,128,3,17,1,167,162,1,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,112,80,1,128,97,80,2,129,102,16,2,128,47,144,66,128,103,208,1,128,109,144,1,128,3,17,1, 
  68,229,1,0,1,3,17,1,6,160,1,0,1,3,17,1,170,159,1,0,1,3,17,1,5,159,1,0,1,3,17,1,107,158,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,117,208,0,128,97,16,193,127,3,17,1,92,161,1, 
  0,1,3,17,1,196,160,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,196,229,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17, 
  1,227,229,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,2,230,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128, 
  3,18,13,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,112,240,0,128,47,112,65,128,103,48,1,128,3,17,1,129,230,1,0,1,3,17,1,170,159,1,0,1,3,17,1,82,155,1, 
  0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,117,176,0,128,3,17,1,92,161,1,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116,16,1,128,45,208,1,128,99,80,1,128,47,144,193,127,3,17,1,228,231,1,0,1,3,17, 
  1,136,231,1,0,1,3,17,1,82,155,1,0,1,3,17,1,13,231,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37, 
  157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,44,231,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17, 
  1,75,231,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,106,231,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128, 
  3,18,72,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,167,231,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0, 
  128,3,17,1,198,231,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,18,71,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176, 
  0,128,3,17,1,3,232,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,18,48,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116, 
  240,0,128,47,112,65,128,99,48,1,128,3,17,1,228,231,1,0,1,3,17,1,136,231,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1, 
  128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,47,8,1,128,3,18,55,0,0,0,1,3,17,1,82,155,1,0,1,21, 
  2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  80,240,129,128,45,112,1,128,84,176,1,128,47,48,65,128,95,112,1,128,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,17,1,163,236,1,0,1,3,17,1,83,233,1,0,1,21,2,54,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,169,233,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128, 
  3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,255, 
  233,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,68,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,85,234,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219, 
  156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,85,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,171,234,1,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,67,240,128,128,45,48, 
  193,127,95,48,1,128,3,17,1,1,235,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0, 
  0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,87,235,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,48,129,128,45,240,192,127,95,240,0,128,3,17, 
  1,219,156,1,0,1,3,17,1,173,235,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,3,236,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17, 
  1,89,236,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,98,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0, 
  21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,249,236,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219, 
  156,1,0,1,3,17,1,79,237,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1,128,3,17,1,165,237,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128, 
  3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,251, 
  237,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,81,238,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219, 
  156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,167,238,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,76,48,1,128,45,240, 
  64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,253,238,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,99,0, 
  0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208, 
  0,128,3,17,1,219,156,1,0,1,1,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,192,1,128,123,16,1,128,58,72,1,128,47,128,129,127,3,18,10,0,0,0,1,3,18,55,0,0,0,1,3, 
  17,1,82,155,1,0,1,3,18,52,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,76, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,40,66,128,100,168,1,128,102,104,1,128,47,232,65,128,123,48,1,128,3,18,10,0,0,0,1,3,17,1,225,240,1,0,1,3,17,1,40,240,1,0,1,3,17, 
  1,82,155,1,0,1,3,18,52,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,71,240,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,102,240,1,0,1,2,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,133,240,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,164,240,1,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,195,240,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,117,0,0,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,0,241,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,31,241,1, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,62,241,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,93, 
  241,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,124,241,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17, 
  1,155,241,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,186,241,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128, 
  3,18,118,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,208,0,128,47,8,193,127,3,18,11,0,0,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,47,8,193,127,3,18, 
  10,0,0,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,42,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,1,128,47,208,0,128,3,17,1,82,155,1,0,1,3,17,1,139,168,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,128,2,129,125,80,1,128,102,192,1,128,47,64,130,128,100,0,2,128,123, 
  136,1,128,3,18,11,0,0,0,1,3,18,10,0,0,0,1,3,17,1,225,240,1,0,1,3,17,1,40,240,1,0,1,3,17,1,82,155,1,0,1,3,18,52,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,104,1,128,47,40,65,128,123,240,0, 
  128,3,18,10,0,0,0,1,3,17,1,82,155,1,0,1,3,18,52,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1, 
  37,157,1,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,144,1,128,45,80,1,128,95,80,1,128,47,16,193,127,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,18,52,0, 
  0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4, 
  78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,116,48,1,128,47,176,193,128,34,48,2,128,39,240,129,127,99,112,1,128,3,17,1,159,244,1,0,1,3,17,1,11,189,1,0,1,3,17,1,82,155,1,0,1, 
  3,17,1,58,170,1,0,1,3,17,1,139,168,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,190,244,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,194,169,1,0,1, 
  2,21,4,6,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,58,88,6,128,60,40,197,132,34,232,4,128,99,120,7,128,36,32,134,131,91,184,135,131,93,64,199,131,39,240,7,128,40,0,4,128,41,8,7,128,42, 
  176,132,125,43,64,132,126,44,144,67,125,45,168,69,126,62,232,133,128,63,152,134,128,94,104,5,128,95,168,5,128,116,80,3,128,123,120,4,128,124,208,6,128,125,200,3,128,3,17,1,26,175,1,0,1,3,18,9,0,0, 
  0,1,3,18,11,0,0,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,214,173,1, 
  0,1,3,17,1,219,156,1,0,1,3,18,21,0,0,0,1,3,18,95,0,0,0,1,3,17,1,26,246,1,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,18,46,0,0,0,1, 
  3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0, 
  1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,129,128,97,240,0,128,62,48,1,128,3,17,1,11,179,1,0,1,3,18,24,0, 
  0,0,1,3,18,94,0,0,0,1,2,21,4,68,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,168,1,128,45,104,193,128,58,224,1,128,95,104,1,128,125,48,1,128,3,18,11,0,0,0,1,3,17,1,219, 
  156,1,0,1,3,18,52,0,0,0,1,3,17,1,99,202,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1, 
  37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,68,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,232,1,128,45,168,193,128,95,168,1,128,47,104,193,127,125,48,1,128,3,18,11,0,0,0,1,3, 
  17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,18,52,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1, 
  3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,69,104,3,128,73,168,2,128,58,232,1,128,43,176,1,128,60,232,2,128,45,104,194,126,78, 
  40,3,128,47,40,66,128,95,104,2,128,3,18,25,0,0,0,1,3,17,1,112,227,1,0,1,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,17,1,245,249,1,0,1,3,17,1,112,222,1,0,1,3,17, 
  1,73,249,1,0,1,3,17,1,241,247,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1, 
  3,17,1,219,156,1,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,71,248,1,0,1, 
  21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,48, 
  1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,157,248,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1, 
  1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,243,248,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240, 
  0,128,3,17,1,219,156,1,0,1,3,17,1,154,221,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4, 
  46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,159,249,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1,128,3,17,1,2,219,1, 
  0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,77,16,1,128,45,80,193,127,95,80,1,128,71,144,193,127,3,17,1,175,251,1,0,1,3,17,1,219,156,1,0,1,3,17,1,87,250,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17, 
  1,219,156,1,0,1,3,17,1,173,250,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,3,251,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17, 
  1,89,251,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,69,48,129,128,45,240,192,127,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,12,218,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17, 
  1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,5,252,1,0,1,3,17,1,219,156,1,0,1,21,2, 
  34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128, 
  45,48,193,127,95,48,1,128,3,17,1,91,252,1,0,1,3,17,1,219,156,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18, 
  57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,219,156,1,0,1,3,17,1,177,252,1,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128,95,240,0,128, 
  3,17,1,219,156,1,0,1,3,17,1,106,216,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,219,156,1,0,1,1,21,4,41,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,62,208,0,128,47,8,1,128,3,18,21,0,0,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17, 
  1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,104,3,128,33,160,3,128,34,176,130,129,91,144,196,129,36,48,195,129,45,240,2,128,47,112,194, 
  128,39,200,196,127,58,16,4,128,95,240,2,128,99,80,4,128,116,48,66,128,124,216,3,128,3,17,1,26,175,1,0,1,3,17,1,82,155,1,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,95, 
  0,0,0,1,3,18,52,0,0,0,1,3,18,116,0,0,0,1,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,195,0,0,0,255,255,255, 
  255,16,0,0,0,4,0,0,0,58,16,3,128,91,160,5,130,34,248,3,128,99,96,5,128,36,184,132,128,95,120,4,128,116,144,2,128,39,216,5,128,40,208,2,128,123,136,3,128,42,192,131,125,43,80,131,125,124,40,5, 
  128,45,120,4,128,94,56,4,128,63,240,132,125,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,17,1,45,255,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17, 
  1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17, 
  1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1, 
  2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17,1,11,179,1,0,1,3,18,94,0,0,0,1,2,21,4,207,0,0,0,255,255,255,255,17,0,0,0,4,0,0, 
  0,58,88,4,128,91,0,6,130,34,216,3,128,99,192,5,128,36,24,133,128,95,216,4,128,116,176,2,128,39,56,6,128,40,240,2,128,123,104,3,128,42,160,131,125,43,48,131,125,60,24,4,129,45,216,4,128,94,152,4, 
  128,63,80,133,125,124,136,5,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1, 
  0,1,3,17,1,58,222,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0, 
  0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219, 
  156,1,0,1,2,21,4,92,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,60,40,130,129,45,104,2,128,58,168,1,128,43,112,65,128,47,232,65,128,95,104,2,128,124,168,2,128,3,18,25,0,0,0,1,3,17, 
  1,112,227,1,0,1,3,17,1,82,155,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,81,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3, 
  112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,58,32,5,128,91,160,5,130,34,184, 
  3,128,99,96,5,128,36,120,132,128,95,56,4,128,116,144,2,128,39,216,5,128,40,208,2,128,123,72,3,128,42,128,131,125,43,16,131,125,124,232,4,128,45,56,4,128,94,248,3,128,63,176,132,125,3,17,1,26,175,1, 
  0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0, 
  0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,17,1,224,178,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0, 
  58,232,4,128,91,160,5,130,34,184,3,128,99,96,5,128,36,120,132,128,95,56,4,128,116,144,2,128,39,216,5,128,40,208,2,128,123,72,3,128,42,128,131,125,43,16,131,125,124,40,5,128,45,56,4,128,94,248,3,128, 
  63,176,132,125,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1, 
  219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,17,1,0,181,1,0,1,3,18,81,0,0,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,150,0,0,0,255,255, 
  255,255,12,0,0,0,3,0,0,0,40,72,3,128,47,16,194,129,34,56,4,129,91,120,132,129,36,144,130,129,45,80,2,128,58,248,3,128,39,200,130,126,95,80,2,128,99,8,3,128,116,128,67,128,124,192,3,128,3,17, 
  1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,58,170,1,0,1,3,17,1,229,169,1,0,1,3,18,52,0,0,0,1,3,17,1,22,169,1,0,1,3,18,81,0,0,0,1,3, 
  17,1,3,182,1,0,1,3,17,1,139,168,1,0,1,3,18,45,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1, 
  3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,172,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,248,3,128,58,104,4,128,34,200,194,127,91,232,196,129,36,72,195,129,45,8,3,128,94, 
  184,3,128,39,32,69,128,63,128,67,128,95,8,3,128,99,168,132,128,116,80,130,128,123,144,2,128,124,48,4,128,3,17,1,26,175,1,0,1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1, 
  0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0, 
  1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156, 
  1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,160,3,128,58,16,4,128,34,112,194,127,91,144,196,129,36,240,194,129,45,176,2,128,94,96,3,128,39,200,68,128,63,40,67,128,95,176, 
  2,128,99,80,4,128,116,48,66,128,124,216,3,128,3,17,1,26,175,1,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,17,1,16,183,1,0,1, 
  3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,150,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,72, 
  3,128,58,184,3,128,34,80,194,127,91,56,132,129,36,208,130,129,45,144,2,128,94,8,3,128,39,112,68,128,95,144,2,128,99,248,3,128,116,16,66,128,124,128,3,128,3,17,1,26,175,1,0,1,3,17,1,97,174,1, 
  0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0, 
  0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219, 
  156,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,160,3,128,58,16,4,128,34,168,194,127,91,144,132,129,36,40,131,129,45,232,2,128,94,96,3,128,39,200,68,128,95,232,2,128,99, 
  80,132,128,116,48,130,128,123,112,2,128,124,216,3,128,3,17,1,26,175,1,0,1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,16,183,1,0, 
  1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,58, 
  32,5,128,91,160,5,130,34,184,3,128,99,96,5,128,36,120,132,128,95,56,4,128,116,144,2,128,39,216,5,128,40,208,2,128,123,72,3,128,42,128,131,125,43,16,131,125,124,232,4,128,45,56,4,128,94,248,3,128,63, 
  176,132,125,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219, 
  156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,53,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,58,40,129,128,47,104,1,128,124,240,0,128,3,18,81,0,0,0,1,3,17,1,3,182,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,207,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,216,4,128,91,0,6,130,34,216,3,128,99,192,5, 
  128,36,24,133,128,95,152,4,128,116,176,2,128,39,56,6,128,40,240,2,128,123,104,3,128,42,160,131,125,43,48,131,125,60,24,4,129,45,152,4,128,94,88,4,128,63,80,133,125,124,136,5,128,3,17,1,26,175,1,0, 
  1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1, 
  0,1,3,17,1,113,9,2,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,53,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,58,104,129,128,97,240,0,128,62,48,1,128,3,17,1,11,179,1,0,1,3,18,24,0,0,0,1,3,17,1,204,176,1,0,1,2,21,4,173,0,0,0,255,255,255,255,14,0,0,0,3,0,0, 
  0,40,192,3,128,47,136,66,130,34,112,4,129,43,80,130,129,36,72,3,129,45,8,3,128,58,48,4,128,39,128,131,126,60,200,2,129,91,48,133,128,95,8,3,128,99,176,4,128,116,240,68,128,124,248,3,128,3,18,25, 
  0,0,0,1,3,17,1,82,155,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,58,170,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1, 
  136,223,1,0,1,3,17,1,139,168,1,0,1,3,17,1,229,169,1,0,1,3,17,1,26,175,1,0,1,3,18,45,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112, 
  65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,231,0,0,0,255,255,255,255,19,0,0,0,4,0,0,0,58,24,5,128,61,152,4,128,34,88,4, 
  128,99,128,6,128,36,216,5,131,63,16,6,129,91,192,198,130,39,248,6,128,40,48,3,128,95,152,5,128,42,32,132,125,43,112,195,126,60,216,132,129,45,152,5,125,94,88,5,128,47,168,131,125,116,240,2,128,123,232,3, 
  128,124,72,6,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,17,1,82,155,1,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17, 
  1,134,157,1,0,1,3,17,1,112,222,1,0,1,3,17,1,58,222,1,0,1,3,17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3, 
  17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1, 
  3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,92,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,60,232,129,129,45,40,2,128,58,104,2,128,43,112,65,128,47,168,65,128,95,40,2,128,124, 
  168,2,128,3,18,25,0,0,0,1,3,17,1,82,155,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,17,1,136,223,1,0,1,3,18,81,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0, 
  0,0,91,160,133,129,95,192,3,128,34,64,3,128,99,96,5,128,36,0,68,128,116,144,2,128,123,8,3,128,39,216,5,128,40,176,4,128,124,232,4,128,58,32,5,128,43,208,66,125,60,128,67,127,45,192,3,128,94,112, 
  4,128,63,56,132,124,3,17,1,26,175,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3, 
  18,112,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,136,223,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21, 
  2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,184,0,0,0, 
  255,255,255,255,15,0,0,0,3,0,0,0,40,88,4,128,58,200,4,128,34,232,194,127,43,176,194,129,36,168,3,129,45,104,3,128,94,24,4,128,39,128,133,128,60,40,67,129,63,224,131,128,91,72,133,128,95,104,3,128, 
  99,8,5,128,116,112,66,128,124,144,4,128,3,17,1,26,175,1,0,1,3,18,25,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18, 
  112,0,0,0,1,3,17,1,16,183,1,0,1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,136,223,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,173,0,0,0,255, 
  255,255,255,14,0,0,0,3,0,0,0,40,0,4,128,58,112,4,128,34,200,194,127,43,144,130,129,36,136,3,129,45,72,3,128,94,192,3,128,39,40,197,128,60,8,3,129,91,240,132,128,95,72,3,128,99,176,4,128,116, 
  80,66,128,124,56,4,128,3,17,1,26,175,1,0,1,3,18,25,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,16,183,1,0, 
  1,3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,136,223,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,184,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40, 
  88,4,128,58,200,4,128,34,32,195,127,43,176,130,129,36,224,3,129,45,160,3,128,94,24,4,128,39,128,197,128,60,96,3,129,91,72,133,128,95,160,3,128,99,8,133,128,116,112,130,128,123,232,2,128,124,144,4,128,3, 
  17,1,26,175,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,17,1,16,183,1,0,1, 
  3,18,52,0,0,0,1,3,18,81,0,0,0,1,3,17,1,136,223,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,207,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,128, 
  5,128,91,0,6,130,34,216,3,128,99,192,5,128,36,216,132,128,95,152,4,128,116,176,2,128,39,56,6,128,40,240,2,128,123,104,3,128,42,160,131,125,43,48,131,125,60,24,4,129,45,152,4,128,94,88,4,128,63,16, 
  133,125,124,72,5,128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3, 
  17,1,214,173,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,17,1,136,223,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3, 
  17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0, 
  1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,8,65,128,62,208,0,128,3,18,21,0,0,0,1,3,17,1,99,202,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116,144,1,128,45,80,1,128,95,80,1,128,47,16,193,127, 
  3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,17,1,35,224,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1, 
  0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,44,104,129,128,47,40,1,128,62,240,0,128,3,18,21,0,0,0,1,3,17,1,82, 
  155,1,0,1,3,18,9,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,42,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,97,208,0,128,47,16,193,127,3,17,1,119,18,2,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16, 
  1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,150,18,2,0,1,2,21,4,29,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,116,176,0,128,3,18,83,0,0,0,1,2,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,44,192,1,128,62,16,1,128,58,72,193,127,47,128,1,128,3,18,21,0,0,0, 
  1,3,18,55,0,0,0,1,3,17,1,82,155,1,0,1,3,18,9,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1, 
  37,157,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,115,208,0,128,47,16,193,127,3,17,1,115,19,2,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128,3,17,1,136,210,1,0, 
  1,2,21,4,129,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,69,200,2,128,65,8,195,129,58,8,2,128,43,208,1,128,60,72,2,128,45,136,194,126,78,72,3,128,95,136,2,128,73,136,67,128,97,200,3,128, 
  3,18,25,0,0,0,1,3,17,1,74,20,2,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,17,1,66,220,1,0,1,3,17,1,162,219,1,0,1,3,17,1,86,218,1,0,1,3,17,1,176,214, 
  1,0,1,3,17,1,184,212,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1, 
  219,156,1,0,1,2,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,8,65,128,62,208,0,128,3,18,24,0,0,0,1,3,18,94,0,0,0,1,2,21,4,141,0,0,0,255,255,255,255,11,0,0, 
  0,3,0,0,0,69,40,3,128,65,104,195,129,58,40,2,128,43,240,1,128,60,168,2,128,45,232,194,126,78,168,3,128,47,104,130,128,73,232,131,128,95,232,2,128,97,40,4,128,3,18,25,0,0,0,1,3,17,1,112, 
  227,1,0,1,3,17,1,82,155,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,17,1,66,220,1,0,1,3,17,1,162,219,1,0,1,3,17,1,86,218,1,0,1,3,17,1,176,214,1,0,1, 
  3,17,1,184,212,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,164,21,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2, 
  48,130,128,3,240,1,128,6,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,224,21,2,0,1,3,17,1,218,21,2,0,1,3,17,1,212,21,2,0,1,2,18,19,0,0,0,21,4,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,165,156,1,0,1,3,17,1,125,155,1,0,1,1,18,19,0,0,0,1,18,17,0,0,0,1,18,18,0,0,0,1,21,4, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,47,112,193,127,97,240,0,128,3,17,1,101,22,2,0,1,3,17,1,71,22,2,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,83,176,0,128,3,18,75, 
  0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,18,76,0,0,0,1,2,21,4,206,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,48,3,128,91,248,197, 
  131,34,24,4,128,99,184,5,128,36,216,132,128,95,152,4,128,116,176,2,128,39,48,6,128,40,240,2,128,41,128,5,128,42,224,131,125,43,112,131,125,124,72,5,128,45,152,4,128,94,88,4,128,63,16,133,125,123,168,3, 
  128,3,17,1,26,175,1,0,1,3,17,1,236,174,1,0,1,3,17,1,45,255,1,0,1,3,18,25,0,0,0,1,3,18,10,0,0,0,1,3,18,122,0,0,0,1,3,17,1,97,174,1,0,1,3,17,1,214,173,1, 
  0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,112,0,0,0,1,3,18,81,0,0,0,1,3,18,53,0,0,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0, 
  1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,89,0, 
  0,0,255,255,255,255,6,0,0,0,2,0,0,0,116,136,1,128,125,80,1,128,34,136,2,128,39,72,66,128,47,8,66,128,99,200,1,128,3,18,11,0,0,0,1,3,17,1,159,244,1,0,1,3,17,1,11,189,1,0, 
  1,3,17,1,82,155,1,0,1,3,17,1,58,170,1,0,1,3,17,1,139,168,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3, 
  17,1,37,157,1,0,1,2,21,4,81,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,60,72,2,128,45,8,2,128,58,136,1,128,43,80,65,128,47,200,65,128,95,8,2,128,3,18,25,0,0,0,1,3,17,1, 
  112,227,1,0,1,3,17,1,82,155,1,0,1,3,17,1,219,156,1,0,1,3,17,1,112,222,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3, 
  17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,184,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,32,4,128,33,88,4,128,34,40,131,129,43,176,194,129,36,232, 
  67,129,45,168,3,128,47,232,66,129,39,128,197,127,58,200,4,128,60,104,3,129,91,72,133,128,95,168,3,128,99,8,5,128,116,112,66,128,124,144,4,128,3,17,1,26,175,1,0,1,3,18,25,0,0,0,1,3,17,1, 
  82,155,1,0,1,3,17,1,97,174,1,0,1,3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,18,95,0,0,0,1,3,18,52,0,0,0,1,3,18,116,0,0,0,1,3,18,81,0,0,0,1,3,17,1, 
  136,223,1,0,1,3,17,1,229,169,1,0,1,3,18,45,0,0,0,1,3,17,1,169,172,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17, 
  1,43,157,1,0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,65,128,116,208,0,128,3,17,1,196,229,1,0,1,3,17,1, 
  45,255,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,58,48,129,128,47,112,1,128,116,240,0,128,3,17,1,196,229,1,0,1,3,17,1,3,182,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116,72,1,128,125,16,1,128,58,136,1,128,47,200,1,128,3, 
  18,11,0,0,0,1,3,17,1,196,229,1,0,1,3,17,1,3,182,1,0,1,3,17,1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43, 
  157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,40,1,128,47,104,65,128,125,240,0,128,3,18,11,0,0,0,1,3,17,1,196,229,1,0,1,3,17, 
  1,82,155,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,58,208,0,128,41,16,1,128,3,17,1,99,202,1,0,1,3,18,53,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43, 
  157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,65,128,116,208,0,128,3,17,1,196,229,1,0,1,3,17,1,99,202,1,0,1,21,2,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128, 
  3,17,1,99,202,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,43,157,1,0,1,3,17,1,37,157,1,0,1,2,21,4,93,0,0,0,255,255,255, 
  255,7,0,0,0,2,0,0,0,60,40,130,129,45,104,2,128,58,168,1,128,43,112,65,128,47,232,65,128,95,104,2,128,116,168,2,128,3,18,25,0,0,0,1,3,17,1,112,227,1,0,1,3,17,1,82,155,1,0,1, 
  3,17,1,112,222,1,0,1,3,17,1,219,156,1,0,1,3,17,1,50,223,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,43,157,1, 
  0,1,3,17,1,37,157,1,0,1,3,17,1,219,156,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,212,28,2,0,1,21,2,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,237,30,2,0,1,3,17,1,5,30,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,212,28,2, 
  0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,29,29,2,0,1,3,17,1,29,29,2,0,1,2,18,28,0,0,0,21,4,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,103,29,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,103,29,2,0,1,1,18, 
  28,0,0,0,21,4,38,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,18,28,0,0,0,17,1,180,29,2,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,17,1,249,29,2,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,103,29,2,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,103,29,2,0,1,2,18,28,0,0,0,17,1,180,29,2,0,1,18,28,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,3,17,1,79,30,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,30,2,0,1,1,18,28,0,0,0,21,4,38,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,18,28,0,0,0,17,1,168,30,2,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128, 
  17,1,156,30,2,0,1,2,18,28,0,0,0,17,1,168,30,2,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,30,2,0,1,21,2,34,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,30,2,0,1,2,18,29,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,237, 
  30,2,0,1,1,21,5,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,1,176,0,128,3,18,8,0,0,0,1,2, 
];

pub mod ast{
  impl AstObject for ASTNode {}
  type ASTSlot = (ASTNode, TokenRange, TokenRange);
  use super::*; 
  type Node = ASTNode;
  
  pub fn ir_from<'a> (mut reader: UTF8StringReader)-> Result<Box<State>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(8);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_State();
    Ok(obj_0_0)
  }
  
  pub fn escaped_from<'a> (mut reader: UTF8StringReader)-> Result<Vec<String>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(50544);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0.into_strings();
    Ok(obj_0_0)
  }
  
  pub fn grammar_from<'a> (mut reader: UTF8StringReader)-> Result<Box<Grammar>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(51363);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_Grammar();
    Ok(obj_0_0)
  }
  
  pub fn type_eval_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(104465);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_expression_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(104615);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_struct_from<'a> (mut reader: UTF8StringReader)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(105126);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_AST_Struct();
    Ok(obj_0_0)
  }
}