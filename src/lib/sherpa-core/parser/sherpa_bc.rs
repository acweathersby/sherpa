
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
  pub kot_prec:u32, 
  pub sym_prec:u32, 
}

impl Precedence{
  
  pub fn new (kot_prec: u32, sym_prec: u32)-> Self {
    
    Self{
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
  pub popped_state:u32, 
  pub tok: Token, 
}

impl Pop{
  
  pub fn new (popped_state: u32, tok: Token)-> Self {
    
    Self{
      popped_state,
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
    self.popped_state.hash(hasher);
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
  pub template_params:Vec<String>, 
  pub tok: Token, 
}

impl TemplateRules{
  
  pub fn new (name_sym: Box<NonTerminal_Symbol>, rules: Vec<Box<Rule>>, template_params: Vec<String>, tok: Token)-> Self {
    
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
    self.template_params.hash(hasher);
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
  pub value:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_STRING{
  
  pub fn new (value: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
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
  pub transitive:Option<ASTNode>, 
}

impl Statement{
  
  pub fn new (branch: Option<ASTNode>, non_branch: Vec<ASTNode>, transitive: Option<ASTNode>)-> Self {
    
    Self{
      branch,
      non_branch,
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
  pub typ:Token, 
  pub tok: Token, 
}

impl AST_Struct{
  
  pub fn new (props: Vec<ASTNode>, typ: Token, tok: Token)-> Self {
    
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
    self.typ.to_string().replace(" ", "").replace("\n", "").hash(hasher);
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


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule ) */
fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_rules | peg_rules | append_rules | ir::state | template_rule )(+) */
fn reducer_020 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* id $ :ast $1 */
fn reducer_021 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_0 = ref_0;
  slots.assign(0, AstSlot(obj_0_0, __rule_rng__, TokenRange::default()));
}


/* num $ :ast $1 */
fn reducer_022 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_0_0 = ref_0;
  slots.assign(0, AstSlot(obj_0_0, __rule_rng__, TokenRange::default()));
}


/* string_convert */
fn reducer_023 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* numeric_convert */
fn reducer_024 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* bool_convert */
fn reducer_025 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* vector */
fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* add */
fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* map */
fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" type_identifier^t ( "," struct_prop(+",") )? '}'
        :ast { t_AST_Struct, typ:$t, props:$3, tok } */
fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, _, _) = slots.take(3);
  let AstSlot (_, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_3_0 = ref_3.into_nodes();
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let var_6_0 = AST_Struct::new(
    obj_3_0,
    tok_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "{" type_identifier^t ( "," struct_prop(+",") )? '}'
        :ast { t_AST_Struct, typ:$t, props:$3, tok } */
fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let var_4_0 = AST_Struct::new(
    vec![],
    tok_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* struct_prop */
fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* struct_prop(+",") */
fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::id

            :ast { t_NonTerminal_Symbol, name:str($1), tok} */
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, branch } */
fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (ref_4, __tok_rng_4, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_4;
  let obj_4_0 = ref_4;
  let obj_2_1 = ref_2.into_nodes();
  let obj_0_2 = ref_0;
  let var_6_0 = Statement::new(
    Some(obj_4_0),
    obj_2_1,
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, branch } */
fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let obj_0_2 = ref_0;
  let var_4_0 = Statement::new(
    Some(obj_2_0),
    vec![],
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, branch } */
fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_1 = ref_2.into_nodes();
  let obj_0_2 = ref_0;
  let var_4_0 = Statement::new(
    None,
    obj_2_1,
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* transitive_statement^transitive 
     ( "then" non_branch_statement(+"then")^non_branch )?
     ( "then" branch_statement^branch )?

     :ast { t_Statement, transitive, non_branch, branch } */
fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_2 = ref_0;
  let var_2_0 = Statement::new(
    None,
    vec![],
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then")^non_branch
     ( "then" branch_statement^branch )?

     :ast { t_Statement, non_branch, branch } */
fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then")^non_branch
     ( "then" branch_statement^branch )?

     :ast { t_Statement, non_branch, branch } */
fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_1 = ref_0.into_nodes();
  let var_2_0 = Statement::new(
    None,
    obj_0_1,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* branch_statement^branch 

     :ast { t_Statement, branch } */
fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let var_2_0 = Statement::new(
    Some(obj_0_0),
    vec![],
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement */
fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then") */
fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement */
fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then") */
fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:id */
fn reducer_048 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_049 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:nl */
fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sp */
fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} :ast str($1) */
fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0;
  let tok_0_0 = tok_0_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* export_clause */
fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_clause */
fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* name_clause */
fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ignore_clause */
fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<>" sym::nonterminal_symbol^n ">" rules^r

        :ast { t_CFRules, name_sym:$n, rules: $r, tok } */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "+>" sym::nonterminal^n ">" rules^r

        :ast { t_AppendRules,  name_sym:$n, rules: $r, tok } */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = AppendRules::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AppendRules(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "<" ( tok::id :ast str($1) )(+",")^p ">" sym::nonterminal_symbol^n ">" rules^r

        :ast { t_TemplateRules, name_sym:$n, template_params:$p, rules: $r, tok } */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
  let obj_1_2 = ref_1.into_strings();
  let var_7_0 = TemplateRules::new(
    obj_3_0,
    obj_5_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    obj_1_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TemplateRules(Box::new(var_7_0)), __rule_rng__, TokenRange::default()));
}


/* tok::id :ast str($1) */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::id :ast str($1) ) */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_string();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::id :ast str($1) )(+",") */
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2.to_string();
  let mut obj_0_0 = ref_0.into_strings();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tk:identifier  :ast { t_DEFINED_TYPE_IDENT } */
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:number     :ast { t_DEFINED_TYPE_NUM } */
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_STRING, value: $2, tok  } */
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_STRING::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_STRING, value: $2, tok  } */
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_STRING::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u8"  convert_initializer?
        :ast { t_AST_U8,  initializer: $2, tok  } */
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_f64();
  let var_2_0 = AST_NUMBER::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_NUMBER(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "[" expression(*",") "]"
        :ast { t_AST_Vector, initializer: $2, tok  } */
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*",") */
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* add "+" expression

        :ast { t_AST_Add, left: $1, right: $3, tok } */
fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "map" "(" expression^k ',' expression^v ')'

        :ast { t_AST_Map, key: $k, val: $v, tok } */
fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "t_" identifier */
fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* tk:id_tok */
fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok } */
fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_126 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_127 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = SetLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SetLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pop" tok::int?    :ast { t_Pop, popped_state: u32($2), tok } */
fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "pop" tok::int?    :ast { t_Pop, popped_state: u32($2), tok } */
fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = 0 as u32;
  let var_3_0 = Pop::new(
    obj_2_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "shift" "-skip"? ( "tok" | "char" )    :ast { t_Shift, ptr_type:str($3), skip:bool($2), tok } */
fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_132 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_133 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* match */
fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_sequence */
fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_statement */
fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "EXPORT" sym::nonterminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, nonterminal:$2, reference:str($3) } */
fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "IMPORT" ( c:id | c:sym | c:num )(+) c:sp ( "AS" | "as" ) tok::id

        :ast { t_Import, uri: str($2), reference:str($5), tok } */
fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( c:id | c:sym | c:num ) */
fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "NAME" tok::id

        :ast { t_Name, name: str($2) } */
fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::terminal(+) */
fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* rule */
fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_nonterminal_symbol */
fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* '(' init_objects ")"       
        :ast { t_Init, expression: $2 } */
fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<" token::int ( ","  token::int  )? ">"

        :ast { t_Range, start_trim:i32($2), end_trim:i32($3) } */
fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* reference */
fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* reference '.' identifier
        :ast { t_AST_Member, reference:$1, property:$3 } */
fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* struct */
fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression
        :ast { t_AST_Statements, statements:[$1], tok } */
fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*";") */
fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal_match_block */
fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_match_block */
fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") "then" goto

            :ast { t_Gotos, pushes: $1, goto } */
fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") */
fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Fail::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pass"        :ast { t_Pass, tok } */
fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Pass::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "accept"      :ast { t_Accept, tok } */
fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Accept::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk:(" rules ")"{1}

        :ast { t_TokenGroupRules, rules:$2,  tok } */
fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token_non_terminal */
fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* (( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi])^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* (( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi])^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+) */
fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+) */
fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi] */
fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi] */
fn reducer_199 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::id '::' tok::id

        :ast { t_NonTerminal_Import_Symbol, module:str($1), name:str($3), tok} */
fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_202 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "$" token::id 
        :ast { t_AST_NamedReference, value: str($2), tok } */
fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_211 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* int_match :ast [$1] */
fn reducer_214 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( int_match | default_match | hint )(+) "}" :ast $2 */
fn reducer_215 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "PRODUCTION" ( nonterminal_match :ast [$1] | "{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_ProductionMatches, matches:$m } */
fn reducer_216 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( nonterminal_match | hint | default_match ) */
fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_220 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* nonterminal_match :ast [$1] */
fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "TERMINAL" ( terminal_match :ast [$1] | "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_TerminalMatches, matches:$m } */
fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( terminal_match | hint | default_match ) */
fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_230 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_231 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* terminal_match :ast [$1] */
fn reducer_232 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_233 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "push" sym::nonterminal

    :ast { t_Push, nonterminal: $2, name:str($2), tok } */
fn reducer_234 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_235 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::nonterminal :ast { t_Goto, nonterminal: $1, name:str($1), tok } )(+) */
fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_245 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_246 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_247 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_248 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_249 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_250 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "[" sym::annotated_symbol(+)^s ']' "!"?^o

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol(+) */
fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "$" :ast { t_EOFSymbol, tok } */
fn reducer_271 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast

            :ast  { t_Ascript, ast:$ast, tok } */
fn reducer_272 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_273 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_274 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_u64();
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) ) */
fn reducer_275 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) )(+"|") */
fn reducer_276 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_277 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_278 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_279 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_280 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_281 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:string_tok */
fn reducer_282 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:quote_tok */
fn reducer_283 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* symbol "+" 

            :ast { t_List_Rules, symbol:$1, tok } */
fn reducer_284 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_285 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_286 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_287 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_288 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_289 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_290 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_291 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_292 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_293 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_2_0 = ref_2.to_u32();
  let tok_1_1 = __tok_rng_1;
  let tok_1_1 = tok_1_1.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_5_0 = Precedence::new(
    obj_2_0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_294 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.to_u32();
  let obj_4_1 = 0 as u32;
  let var_5_0 = Precedence::new(
    obj_1_0,
    obj_4_1,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_295 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_1 = __tok_rng_1;
  let tok_1_1 = tok_1_1.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_4_0 = Precedence::new(
    0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_296 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_3_1 = 0 as u32;
  let var_4_0 = Precedence::new(
    0,
    obj_3_1,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* ":" tk:precedence_num? :ast u32($2) */
fn reducer_297 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* ":" tk:precedence_num? :ast u32($2) */
fn reducer_298 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = 0 as u32;
  slots.assign(0, AstSlot(ASTNode::U32(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* nonterminal */
fn reducer_299 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* template_nonterminal */
fn reducer_300 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal */
fn reducer_301 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal "::<" ( nonterminal  )(+",")^template_args ">"

            :ast { t_Template_NonTerminal_Symbol, name:$1, template_args, tok} */
fn reducer_302 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( nonterminal  ) */
fn reducer_303 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal  )(+",") */
fn reducer_304 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 305]
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
    ])
  }
}
    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<T, UserCTX, Bytecode> = sherpa_rust_runtime::deprecate::ByteCodeParser<T, UserCTX, Bytecode>;

pub mod meta{
  
  pub const nonterm_names: [&'static str;106] = [
    "state",
    "escaped_string",
    "escaped_string_group",
    "escaped_string_list_1",
    "grammar",
    "grammar_list",
    "grammar_list_1",
    "def_type",
    "expression",
    "struct",
    "struct_list",
    "nonterminal_symbol",
    "statement",
    "statement_list",
    "statement_list_1",
    "escaped_vals",
    "escaped",
    "preamble",
    "cf_rules",
    "peg_rules",
    "append_rules",
    "template_rule",
    "template_rule_group",
    "template_rule_list_1",
    "id",
    "num",
    "string_convert",
    "numeric_convert",
    "bool_convert",
    "literal",
    "vector",
    "vector_list",
    "token",
    "add",
    "map",
    "struct_prop",
    "type_identifier",
    "id",
    "non_branch_statement",
    "transitive_statement",
    "branch_statement",
    "export_clause",
    "import_clause",
    "import_clause_list",
    "name_clause",
    "ignore_clause",
    "ignore_clause_list",
    "rules",
    "rules_list",
    "nonterminal",
    "convert_initializer",
    "int",
    "range",
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
    "rule_list",
    "rule_group_1",
    "import_nonterminal_symbol",
    "init_objects",
    "reference",
    "generic_match_block",
    "generic_match_block_group",
    "generic_match_block_list_1",
    "generic_match_block_group_2",
    "nonterminal_match_block",
    "nonterminal_match_block_list",
    "nonterminal_match_block_group_1",
    "terminal_match_block",
    "terminal_match_block_list",
    "terminal_match_block_group_1",
    "goto_push",
    "goto",
    "fork",
    "fork_group",
    "fork_list_1",
    "token",
    "token_non_terminal",
    "class",
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
    "string",
    "quote",
    "list",
    "precedence",
    "precedence_group",
    "symbol",
    "template_nonterminal",
    "template_nonterminal_list",
  ];
  
  pub const symbol_string: [&'static str;126] = [
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
    r####"c:id"####,
    r####"c:num"####,
    r####"c:sym"####,
    r####" \ "####,
    r####" > "####,
    r####" <> "####,
    r####"nonterm"####,
    r####" :> "####,
    r####" +> "####,
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
    r####" + "####,
    r####" ( "####,
    r####" ) "####,
    r####" map "####,
    r####"nonterm"####,
    r####" : "####,
    r####" t_ "####,
    r####"tk:nonterm"####,
    r####" to "####,
    r####" pop "####,
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
    r####" sym "####,
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
    r####"tk:nonterm"####,
    r####" * "####,
    r####" (* "####,
    r####" (+ "####,
    r####"tk:nonterm"####,
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
  parser.init_parser(36004);
  parser
}

pub fn new_grammar_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(36776);
  parser
}

pub fn new_type_eval_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(42190);
  parser
}

pub fn new_ast_expression_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(42368);
  parser
}

pub fn new_ast_struct_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(42805);
  parser
}

pub static bytecode: [u8; 74045] = [
  0,211,200,197,210,208,193,2,15,1,163,140,0,0,17,1,21,0,0,0,1,21,1,59,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8, 
  4,15,1,93,0,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,2,19,37,0,0,0,116,0,0,0,1,0,1,21,1,59,0,0,0,207,169,0,0,6,0,0,0,2,0,0,0,4,88,1,128,1,80,1,128,2, 
  80,129,128,3,152,129,128,6,80,1,128,7,80,1,128,8,4,17,1,153,138,0,0,1,4,17,1,153,0,0,0,1,2,21,1,241,1,0,0,122,170,0,0,19,0,0,0,4,0,0,0,64,24,13,128,1,240,194,131,2, 
  240,130,128,67,120,203,131,66,72,76,131,85,200,8,128,6,240,130,128,7,240,194,129,70,216,10,129,73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128,93,40,6,128,87,40,7,128,63,232,13,128,97,248,4,128,98, 
  248,3,128,99,248,2,128,8,4,15,1,105,138,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,105,138,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15, 
  1,82,136,0,0,17,1,242,135,0,0,1,4,15,1,105,138,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,105,138,0,0,15,1,117, 
  138,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,105,138,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,105,138,0,0,15,1,117,138,0, 
  0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,105,138,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,105,138,0,0,15,1,228,109,0,0,17,1,48,109,0,0,1, 
  4,15,1,105,138,0,0,15,1,228,109,0,0,17,1,220,108,0,0,1,4,15,1,105,138,0,0,15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1,105,138,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17, 
  1,57,100,0,0,1,4,15,1,105,138,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,45,100,0,0,1,4,15,1,105,138,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,235,99,0,0,1,4,15, 
  1,105,138,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,228,2,0,0,1,4,15,1,105,138,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,139,2,0,0,1,2,21,1,53,0,0,0,148,177,0, 
  0,5,0,0,0,2,0,0,0,6,160,1,129,1,160,1,128,2,160,129,127,7,160,1,128,82,48,1,128,4,15,1,216,2,0,0,17,1,204,2,0,0,1,8,19,38,0,0,0,129,0,0,0,1,0,1,19,51,0,0, 
  0,165,0,0,0,1,0,1,19,38,0,0,0,128,0,0,0,2,0,1,21,1,53,0,0,0,117,180,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4, 
  15,1,26,3,0,0,17,1,204,2,0,0,1,2,21,1,113,0,0,0,202,180,0,0,8,0,0,0,3,0,0,0,57,72,2,129,1,144,193,127,2,144,193,128,51,24,3,128,65,8,2,128,82,152,1,128,6,144,1,128, 
  7,144,1,128,8,4,15,1,43,99,0,0,17,1,204,2,0,0,1,4,17,1,202,94,0,0,1,4,15,1,65,91,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,65,91,0,0, 
  17,1,140,3,0,0,1,2,21,1,145,1,0,0,167,183,0,0,12,0,0,0,3,0,0,0,88,184,200,130,1,16,194,129,2,16,2,128,51,232,10,128,44,184,75,129,101,152,5,128,6,16,2,128,7,16,194,128,57,24, 
  10,128,100,40,7,128,119,216,3,128,120,24,2,128,8,4,15,1,235,26,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1, 
  53,91,0,0,17,1,223,26,0,0,1,4,15,1,235,26,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0, 
  17,1,199,26,0,0,1,4,15,1,235,26,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1, 
  235,26,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,235,26,0,0,15,1,39,27,0,0, 
  15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,235,26,0,0,15,1,39,27,0,0,15,1,122,23,0,0,17,1,81,0,0,0,1,4,15,1, 
  235,26,0,0,15,1,39,27,0,0,15,1,142,22,0,0,17,1,140,3,0,0,1,4,15,1,235,26,0,0,15,1,39,27,0,0,15,1,130,22,0,0,17,1,30,5,0,0,1,2,21,1,73,1,0,0,76,189,0,0, 
  11,0,0,0,3,0,0,0,88,216,135,130,1,240,193,129,2,240,1,128,51,168,9,128,100,120,6,128,101,24,5,128,6,240,1,128,7,240,129,128,57,8,9,128,119,136,3,128,120,248,1,128,8,4,15,1,195,8,0,0, 
  15,1,118,22,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,195,8,0,0,15,1,118,22,0,0,15,1,96,88, 
  0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,195,8,0,0,15,1,118,22,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1, 
  29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,195,8,0,0,15,1,118,22,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0, 
  1,4,15,1,195,8,0,0,15,1,118,22,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,195,8,0,0,15,1,144,8,0,0,17,1,81,0,0,0,1,4, 
  15,1,195,8,0,0,15,1,104,6,0,0,17,1,140,3,0,0,1,2,21,1,39,0,0,0,237,189,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,125,240,0,128,4,17,1,155,6,0,0,1,8,19, 
  103,0,0,0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0,0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,243,6,0,0,15,1,35,8,0,0,17, 
  1,81,0,0,0,1,4,15,1,243,6,0,0,15,1,231,6,0,0,17,1,140,3,0,0,1,2,19,105,0,0,0,47,1,0,0,1,0,1,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1, 
  128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105,80,1,128,15,1,243,6,0,0,17,1,120,7,0,0,1,1,15,1,243,6,0,0,17,1,108,7,0,0,1,15,1,243,6,0,0,17,1,231,6,0,0, 
  1,15,1,243,6,0,0,17,1,35,8,0,0,1,15,1,243,6,0,0,17,1,96,7,0,0,1,2,19,49,0,0,0,162,0,0,0,1,0,1,19,49,0,0,0,163,0,0,0,1,0,1,21,7,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,44,16,65,128,62,208,0,128,4,17,1,22,8,0,0,1,4,17,1,190,7,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,2,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0,0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,232,1,128,8,4,15,1,10,8,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0, 
  0,0,1,4,15,1,10,8,0,0,17,1,140,3,0,0,1,2,19,105,0,0,0,48,1,0,0,3,0,1,19,104,0,0,0,46,1,0,0,4,0,14,1,21,1,39,0,0,0,26,197,0,0,3,0,0,0,1,0,0, 
  0,2,48,1,128,1,48,65,128,91,240,0,128,4,17,1,86,8,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,45,0,0,0,172,197,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128, 
  57,248,0,128,8,4,15,1,132,8,0,0,17,1,81,0,0,0,1,2,19,65,0,0,0,200,0,0,0,3,0,1,21,1,39,0,0,0,5,198,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,91,248, 
  0,128,8,4,17,1,86,8,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,0,22,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,65,168,6,128,49,120,199,127,98,48,4,128,83,64,198,129,84,216,5, 
  130,37,224,7,129,86,8,5,128,87,0,197,129,88,152,196,129,85,112,5,128,99,200,3,128,11,72,8,128,100,96,3,128,61,16,7,128,103,248,2,128,104,144,2,128,15,1,195,8,0,0,17,1,106,22,0,0,1,15,1, 
  195,8,0,0,17,1,247,19,0,0,1,15,1,195,8,0,0,17,1,136,15,0,0,1,15,1,195,8,0,0,17,1,53,91,0,0,1,15,1,195,8,0,0,17,1,211,26,0,0,1,15,1,195,8,0,0,17,1,249,10, 
  0,0,1,1,15,1,195,8,0,0,17,1,118,22,0,0,1,15,1,195,8,0,0,17,1,187,26,0,0,1,15,1,195,8,0,0,17,1,211,25,0,0,1,15,1,195,8,0,0,17,1,41,91,0,0,1,15,1,195,8, 
  0,0,17,1,108,7,0,0,1,15,1,195,8,0,0,17,1,29,91,0,0,1,15,1,195,8,0,0,17,1,13,10,0,0,1,15,1,195,8,0,0,17,1,218,9,0,0,1,15,1,195,8,0,0,17,1,96,7,0,0, 
  1,2,21,1,39,0,0,0,5,198,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,91,240,0,128,4,17,1,86,8,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,39,0,0,0,237, 
  189,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,125,240,0,128,4,17,1,64,10,0,0,1,8,19,103,0,0,0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0, 
  0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,140,10,0,0,15,1,35,8,0,0,17,1,81,0,0,0,1,4,15,1,140,10,0,0,15,1,231,6,0,0,17,1,140,3,0,0,1,2, 
  21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105,80,1,128,15,1,140,10,0,0,17,1,120,7,0,0,1,1,15,1,140,10, 
  0,0,17,1,108,7,0,0,1,15,1,140,10,0,0,17,1,231,6,0,0,1,15,1,140,10,0,0,17,1,35,8,0,0,1,15,1,140,10,0,0,17,1,96,7,0,0,1,2,21,1,91,1,0,0,49,199,0,0,12, 
  0,0,0,3,0,0,0,88,56,199,130,1,16,194,129,2,16,2,128,51,104,9,128,100,8,6,128,45,152,10,129,6,16,2,128,7,16,194,128,57,56,8,128,101,216,4,128,119,120,3,128,120,24,2,128,8,4,15,1,124, 
  15,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,124,15,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15, 
  1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,124,15,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0, 
  0,1,4,15,1,124,15,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,124,15,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15, 
  1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,124,15,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,138,13,0,0,15,1,73,15,0,0,17,1,81,0,0,0,1,4,15,1,124,15,0,0,15,1,96, 
  88,0,0,15,1,242,89,0,0,15,1,138,13,0,0,15,1,158,12,0,0,17,1,140,3,0,0,1,4,17,1,85,12,0,0,1,2,21,1,47,0,0,0,222,199,0,0,5,0,0,0,2,0,0,0,6,112,1,129,1, 
  112,1,128,2,112,129,127,7,112,1,128,114,48,1,128,4,17,1,145,12,0,0,1,8,19,87,0,0,0,12,1,0,0,3,0,14,1,19,87,0,0,0,11,1,0,0,4,0,14,1,21,1,39,0,0,0,102,201,0,0, 
  3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,125,240,0,128,4,17,1,209,12,0,0,1,8,19,103,0,0,0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0,0,0,57, 
  24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,29,13,0,0,15,1,35,8,0,0,17,1,81,0,0,0,1,4,15,1,29,13,0,0,15,1,231,6,0,0,17,1,140,3,0,0,1,2,21,0,108, 
  0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105,80,1,128,15,1,29,13,0,0,17,1,120,7,0,0,1,1,15,1,29,13,0,0,17, 
  1,108,7,0,0,1,15,1,29,13,0,0,17,1,231,6,0,0,1,15,1,29,13,0,0,17,1,35,8,0,0,1,15,1,29,13,0,0,17,1,96,7,0,0,1,2,21,0,210,0,0,0,255,255,255,255,12,0,0,0, 
  3,0,0,0,104,16,2,128,49,88,197,129,98,232,2,128,11,40,134,129,84,184,3,128,37,192,69,128,61,240,4,129,103,120,2,128,65,136,4,128,83,32,132,128,85,80,3,128,99,128,2,128,15,1,138,13,0,0,17,1, 
  106,22,0,0,1,1,15,1,138,13,0,0,17,1,53,91,0,0,1,15,1,138,13,0,0,17,1,211,26,0,0,1,15,1,138,13,0,0,17,1,187,26,0,0,1,15,1,138,13,0,0,17,1,211,25,0,0,1,15,1, 
  138,13,0,0,17,1,41,91,0,0,1,15,1,138,13,0,0,17,1,108,7,0,0,1,15,1,138,13,0,0,17,1,29,91,0,0,1,15,1,138,13,0,0,17,1,93,14,0,0,1,15,1,138,13,0,0,17,1,73,15, 
  0,0,1,15,1,138,13,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,102,201,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,125,248,0,128,8,4,17,1,144,14,0,0,1,19,103,0,0, 
  0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0,0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,220,14,0,0,15,1,35,8,0,0,17,1,81,0, 
  0,0,1,4,15,1,220,14,0,0,15,1,231,6,0,0,17,1,140,3,0,0,1,2,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192, 
  65,128,105,80,1,128,15,1,220,14,0,0,17,1,120,7,0,0,1,1,15,1,220,14,0,0,17,1,108,7,0,0,1,15,1,220,14,0,0,17,1,231,6,0,0,1,15,1,220,14,0,0,17,1,35,8,0,0,1,15, 
  1,220,14,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,230,202,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,91,248,0,128,8,4,17,1,86,8,0,0,1,19,11,0,0,0,35,0,0, 
  0,1,0,1,19,88,0,0,0,14,1,0,0,2,0,1,21,1,69,0,0,0,66,204,0,0,5,0,0,0,2,0,0,0,10,184,193,128,1,112,193,128,2,112,129,127,110,48,1,128,113,120,1,128,4,17,1,28,19,0, 
  0,1,8,4,17,1,29,18,0,0,1,4,15,1,78,17,0,0,17,1,217,15,0,0,1,19,86,0,0,0,10,1,0,0,1,0,1,21,1,69,0,0,0,146,205,0,0,5,0,0,0,2,0,0,0,124,56,1,128,1, 
  48,1,128,2,48,1,128,11,232,65,128,55,120,1,128,8,4,17,1,176,16,0,0,1,4,15,1,106,16,0,0,17,1,43,16,0,0,1,4,17,1,31,16,0,0,1,2,19,101,0,0,0,40,1,0,0,2,0,1,21, 
  1,39,0,0,0,29,206,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,124,248,0,128,8,4,17,1,94,16,0,0,1,19,102,0,0,0,42,1,0,0,1,0,1,19,102,0,0,0,41,1,0,0,2, 
  0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,17,1,164,16,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2, 
  19,101,0,0,0,38,1,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,125,64,1,128,4,15,1,8,17,0,0,17,1,43,16,0,0,1,4,17,1,252,16,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,101,0,0,0,39,1,0,0,3,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176, 
  0,128,4,17,1,66,17,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,101,0,0,0,37,1,0,0,4,0,1,21,1,51,0,0,0,114,206,0,0, 
  4,0,0,0,2,0,0,0,110,88,1,128,1,16,129,128,2,16,129,127,113,24,1,128,8,4,17,1,204,17,0,0,1,4,17,1,141,17,0,0,1,19,86,0,0,0,254,0,0,0,2,0,1,21,1,39,0,0,0,43, 
  207,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,113,248,0,128,8,4,17,1,192,17,0,0,1,19,86,0,0,0,3,1,0,0,3,0,1,19,86,0,0,0,9,1,0,0,4,0,1,21,7,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,17,1,17,18,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,7,1, 
  0,0,3,0,1,19,86,0,0,0,8,1,0,0,4,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,197,18,0,0,1,4,15,1,116,18,0,0,17,1, 
  217,15,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,1,1,0,0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,63,176,0,128,4,17,1,185,18,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,253,0,0,0,3,0,1,19,86,0,0,0,2,1, 
  0,0,4,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,16,19,0,0,17,1,217,15,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,86,0,0,0,255,0,0,0,3,0,1,19,86,0,0,0,251,0,0,0,4,0,1,21,1,57,0,0,0,216,207,0,0,4,0,0,0,2,0,0,0,10,88,1,128,1,80,129,128,2,80, 
  129,127,113,16,1,128,4,17,1,160,19,0,0,1,8,4,15,1,97,19,0,0,17,1,217,15,0,0,1,19,86,0,0,0,0,1,0,0,2,0,1,21,1,39,0,0,0,43,207,0,0,3,0,0,0,1,0,0,0,2, 
  48,1,128,1,48,65,128,113,240,0,128,4,17,1,148,19,0,0,1,8,19,86,0,0,0,252,0,0,0,3,0,1,19,86,0,0,0,6,1,0,0,4,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,123,176,0,128,4,15,1,235,19,0,0,17,1,217,15,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,5,1,0,0,3,0,1,19, 
  86,0,0,0,4,1,0,0,4,0,1,21,1,75,0,0,0,145,208,0,0,6,0,0,0,2,0,0,0,50,216,65,129,1,144,193,128,2,144,129,127,123,152,1,128,121,24,2,128,122,80,1,128,4,17,1,104,21,0,0, 
  1,8,4,17,1,102,20,0,0,1,4,17,1,90,20,0,0,1,4,17,1,78,20,0,0,1,19,100,0,0,0,36,1,0,0,1,0,1,19,100,0,0,0,29,1,0,0,2,0,1,19,100,0,0,0,28,1,0,0,2, 
  0,1,21,1,105,0,0,0,110,209,0,0,6,0,0,0,2,0,0,0,52,8,67,129,1,80,193,128,2,80,1,128,119,248,1,128,101,152,2,128,120,88,1,128,8,4,15,1,34,21,0,0,15,1,53,91,0,0,17,1, 
  223,26,0,0,1,4,15,1,34,21,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,220,20,0,0,17,1,223,25,0,0,1,4,17,1,208,20,0,0,1,2,19,100,0,0,0,32,1,0,0,3,0,1, 
  21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,17,1,22,21,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,100, 
  0,0,0,31,1,0,0,4,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,17,1,92,21,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,2,19,100,0,0,0,30,1,0,0,4,0,1,21,1,105,0,0,0,110,209,0,0,6,0,0,0,2,0,0,0,52,8,67,129,1,80,193,128,2,80,1,128,119,248,1,128,101,152,2,128,120,88, 
  1,128,8,4,15,1,36,22,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,36,22,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,222,21,0,0,17,1,223,25,0,0,1,4,17,1, 
  210,21,0,0,1,2,19,100,0,0,0,35,1,0,0,3,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,17,1,24,22,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,100,0,0,0,34,1,0,0,4,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,17,1,94,22,0,0,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,100,0,0,0,33,1,0,0,4,0,1,19,103,0,0,0,44,1,0,0,1,0,1,19,88,0,0,0,13,1,0,0,1, 
  0,1,19,63,0,0,0,195,0,0,0,1,0,1,21,1,39,0,0,0,250,209,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,125,240,0,128,4,17,1,193,22,0,0,1,8,19,103,0,0,0,43,1, 
  0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0,0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,13,23,0,0,15,1,35,8,0,0,17,1,81,0,0,0,1, 
  4,15,1,13,23,0,0,15,1,231,6,0,0,17,1,140,3,0,0,1,2,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105, 
  80,1,128,15,1,13,23,0,0,17,1,120,7,0,0,1,1,15,1,13,23,0,0,17,1,108,7,0,0,1,15,1,13,23,0,0,17,1,231,6,0,0,1,15,1,13,23,0,0,17,1,35,8,0,0,1,15,1,13,23, 
  0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,19,211,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,91,248,0,128,8,4,17,1,86,8,0,0,1,19,11,0,0,0,35,0,0,0,1,0, 
  1,21,1,145,1,0,0,167,183,0,0,12,0,0,0,3,0,0,0,88,184,200,130,1,16,194,129,2,16,2,128,51,232,10,128,44,184,75,129,101,152,5,128,6,16,2,128,7,16,194,128,57,24,10,128,100,40,7,128,119, 
  216,3,128,120,24,2,128,8,4,15,1,63,25,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223, 
  26,0,0,1,4,15,1,63,25,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1, 
  4,15,1,63,25,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,63,25,0,0,15,1,39, 
  27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,63,25,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15, 
  1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,63,25,0,0,15,1,39,27,0,0,15,1,122,23,0,0,17,1,81,0,0,0,1,4,15,1,63,25,0,0,15,1,39, 
  27,0,0,15,1,142,22,0,0,17,1,140,3,0,0,1,4,15,1,63,25,0,0,15,1,39,27,0,0,15,1,130,22,0,0,17,1,30,5,0,0,1,2,21,1,47,0,0,0,44,212,0,0,5,0,0,0,2,0,0, 
  0,52,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,111,25,0,0,1,2,19,61,0,0,0,188,0,0,0,3,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0,0, 
  0,57,24,1,128,1,16,193,127,2,16,1,128,51,232,1,128,8,4,15,1,199,25,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,199,25,0,0,17,1,140,3,0,0,1,2,19, 
  84,0,0,0,242,0,0,0,2,0,1,19,61,0,0,0,190,0,0,0,1,0,1,21,1,123,0,0,0,129,212,0,0,10,0,0,0,3,0,0,0,104,24,3,128,1,208,193,129,2,208,193,129,107,88,2,128,108,24,2, 
  128,109,216,1,128,102,152,3,128,103,88,3,128,105,216,2,128,106,152,2,128,8,4,17,1,175,26,0,0,1,4,17,1,163,26,0,0,1,4,17,1,151,26,0,0,1,4,17,1,139,26,0,0,1,4,17,1,127,26,0, 
  0,1,4,17,1,115,26,0,0,1,4,17,1,103,26,0,0,1,4,17,1,91,26,0,0,1,2,19,85,0,0,0,246,0,0,0,2,0,1,19,85,0,0,0,244,0,0,0,2,0,1,19,85,0,0,0,245,0,0,0, 
  2,0,1,19,85,0,0,0,249,0,0,0,2,0,1,19,85,0,0,0,243,0,0,0,2,0,1,19,85,0,0,0,247,0,0,0,2,0,1,19,85,0,0,0,248,0,0,0,2,0,1,19,85,0,0,0,250,0,0,0, 
  2,0,1,19,61,0,0,0,191,0,0,0,1,0,1,19,98,0,0,0,26,1,0,0,1,0,1,19,83,0,0,0,240,0,0,0,1,0,1,19,99,0,0,0,27,1,0,0,1,0,1,21,1,47,0,0,0,44,212,0, 
  0,5,0,0,0,2,0,0,0,52,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,27,27,0,0,1,2,19,49,0,0,0,161,0,0,0,3,0,1,21,0,90,1,0,0,255,255,255, 
  255,20,0,0,0,4,0,0,0,48,144,137,130,49,40,201,130,98,176,4,128,83,184,134,131,84,80,134,131,37,0,202,130,86,128,5,128,87,24,5,131,104,16,3,128,63,240,7,128,64,136,7,128,11,104,10,128,65,32,7, 
  128,61,192,8,128,62,88,8,128,47,248,137,126,85,232,5,128,99,72,4,128,100,224,3,128,103,120,3,128,15,1,39,27,0,0,17,1,106,22,0,0,1,15,1,39,27,0,0,17,1,253,87,0,0,1,15,1,39,27,0, 
  0,17,1,40,87,0,0,1,15,1,39,27,0,0,17,1,53,91,0,0,1,15,1,39,27,0,0,17,1,211,26,0,0,1,15,1,39,27,0,0,17,1,130,22,0,0,1,15,1,39,27,0,0,17,1,84,88,0,0,1, 
  15,1,39,27,0,0,17,1,187,26,0,0,1,15,1,39,27,0,0,17,1,211,25,0,0,1,15,1,39,27,0,0,17,1,41,91,0,0,1,15,1,39,27,0,0,17,1,108,7,0,0,1,15,1,39,27,0,0,17,1, 
  106,42,0,0,1,15,1,39,27,0,0,17,1,206,40,0,0,1,15,1,39,27,0,0,17,1,194,40,0,0,1,15,1,39,27,0,0,17,1,29,91,0,0,1,15,1,39,27,0,0,17,1,214,39,0,0,1,15,1,39, 
  27,0,0,17,1,181,28,0,0,1,1,15,1,39,27,0,0,17,1,130,28,0,0,1,15,1,39,27,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,19,211,0,0,3,0,0,0,1,0,0,0,2,240,0,128, 
  1,240,64,128,91,248,0,128,8,4,17,1,86,8,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,47,0,0,0,194,214,0,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7, 
  112,1,128,81,48,1,128,4,17,1,240,28,0,0,1,8,19,47,0,0,0,158,0,0,0,1,0,1,21,1,193,1,0,0,167,183,0,0,12,0,0,0,3,0,0,0,88,120,201,130,1,16,194,129,2,16,2,128,51,8, 
  12,128,44,8,77,129,101,248,5,128,6,16,2,128,7,16,194,128,57,8,11,128,100,184,7,128,119,8,4,128,120,24,2,128,8,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1,84,88,0,0, 
  15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1,84,88, 
  0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1, 
  84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1,84,88,0,0, 
  15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1,84,88,0,0,15,1,96,88, 
  0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1,158,31,0,0,17,1,81,0,0,0,1,4,15,1,209,31,0,0, 
  15,1,106,42,0,0,15,1,221,31,0,0,15,1,178,30,0,0,17,1,140,3,0,0,1,4,15,1,209,31,0,0,15,1,106,42,0,0,15,1,221,31,0,0,15,1,130,22,0,0,17,1,30,5,0,0,1,2,21,1, 
  39,0,0,0,250,209,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,125,248,0,128,8,4,17,1,229,30,0,0,1,19,103,0,0,0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4, 
  0,0,0,2,0,0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,49,31,0,0,15,1,35,8,0,0,17,1,81,0,0,0,1,4,15,1,49,31,0,0,15,1,231,6,0,0,17,1,140, 
  3,0,0,1,2,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105,80,1,128,15,1,49,31,0,0,17,1,120,7,0,0,1, 
  1,15,1,49,31,0,0,17,1,108,7,0,0,1,15,1,49,31,0,0,17,1,231,6,0,0,1,15,1,49,31,0,0,17,1,35,8,0,0,1,15,1,49,31,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0, 
  19,211,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,91,240,0,128,4,17,1,86,8,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,19,48,0,0,0,160,0,0,0,3,0,1,21,0,39, 
  1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,64,40,7,128,49,0,8,130,98,80,4,128,83,88,70,130,84,240,133,130,37,104,72,129,86,32,5,128,87,184,68,130,104,176,2,128,65,192,6,128,85,136,5,128,11, 
  208,8,128,99,232,3,128,61,152,7,128,100,128,3,128,63,48,7,128,103,24,3,128,15,1,221,31,0,0,17,1,106,22,0,0,1,15,1,221,31,0,0,17,1,171,38,0,0,1,15,1,221,31,0,0,17,1,25,37,0, 
  0,1,15,1,221,31,0,0,17,1,53,91,0,0,1,15,1,221,31,0,0,17,1,211,26,0,0,1,15,1,221,31,0,0,17,1,130,22,0,0,1,15,1,221,31,0,0,17,1,84,88,0,0,1,15,1,221,31,0,0, 
  17,1,187,26,0,0,1,15,1,221,31,0,0,17,1,211,25,0,0,1,15,1,221,31,0,0,17,1,41,91,0,0,1,15,1,221,31,0,0,17,1,108,7,0,0,1,1,15,1,221,31,0,0,17,1,241,33,0,0,1, 
  15,1,221,31,0,0,17,1,29,91,0,0,1,15,1,221,31,0,0,17,1,5,33,0,0,1,15,1,221,31,0,0,17,1,130,28,0,0,1,15,1,221,31,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,250, 
  209,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,125,248,0,128,8,4,17,1,56,33,0,0,1,19,103,0,0,0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4,0,0,0,2,0, 
  0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,132,33,0,0,15,1,35,8,0,0,17,1,81,0,0,0,1,4,15,1,132,33,0,0,15,1,231,6,0,0,17,1,140,3,0,0,1,2, 
  21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105,80,1,128,15,1,132,33,0,0,17,1,120,7,0,0,1,1,15,1,132,33, 
  0,0,17,1,108,7,0,0,1,15,1,132,33,0,0,17,1,231,6,0,0,1,15,1,132,33,0,0,17,1,35,8,0,0,1,15,1,132,33,0,0,17,1,96,7,0,0,1,2,21,1,85,1,0,0,99,215,0,0,13, 
  0,0,0,3,0,0,0,88,72,7,131,1,64,196,129,2,64,4,128,51,72,4,128,44,120,69,129,101,120,9,128,6,64,4,128,7,64,4,129,57,144,3,128,92,208,67,128,100,72,8,128,119,232,5,128,120,48,2,128,4, 
  15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,6,17,1,118,35,0,0,1,4,15,1,106,35,0,0,17,1, 
  94,35,0,0,1,10,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,138,13,0,0,15,1,158,12,0,0,17,1,140,3,0,0,1,4,15,1,82,35,0,0,17,1,30,5,0,0,1,4,15, 
  1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0, 
  0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,13,37,0,0,15, 
  1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,19,64,0,0,0,199,0,0,0,1,0,1,19,63,0,0,0,197,0,0,0,2,0,1,19,89,0,0,0,15, 
  1,0,0,1,0,1,19,64,0,0,0,198,0,0,0,2,0,1,21,1,51,0,0,0,174,216,0,0,6,0,0,0,2,0,0,0,4,80,1,128,1,144,1,128,2,144,129,128,3,80,129,128,6,144,1,128,7,144,1,128, 
  12,17,1,1,37,0,0,1,10,12,17,1,177,35,0,0,1,21,1,79,1,0,0,76,189,0,0,11,0,0,0,3,0,0,0,88,24,135,130,1,240,193,129,2,240,1,128,51,72,9,128,100,232,5,128,101,184,4,128,6, 
  240,1,128,7,240,129,128,57,24,8,128,119,88,3,128,120,248,1,128,8,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223, 
  26,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0, 
  0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123, 
  25,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,138,13,0, 
  0,15,1,73,15,0,0,17,1,81,0,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,138,13,0,0,15,1,158,12,0,0,17,1,140,3,0,0,1,2,19,64,0,0,0,199,0, 
  0,0,1,0,1,19,63,0,0,0,196,0,0,0,2,0,1,21,1,69,0,0,0,223,217,0,0,5,0,0,0,2,0,0,0,10,56,193,128,1,48,193,128,2,48,129,127,110,232,1,128,113,168,1,128,8,4,15,1,57, 
  38,0,0,17,1,217,15,0,0,1,4,17,1,226,37,0,0,1,4,17,1,106,37,0,0,1,19,86,0,0,0,10,1,0,0,1,0,1,21,1,57,0,0,0,236,218,0,0,4,0,0,0,2,0,0,0,10,16,1,128, 
  1,128,129,128,2,128,129,127,113,136,1,128,4,15,1,175,37,0,0,17,1,217,15,0,0,1,8,4,17,1,160,19,0,0,1,19,86,0,0,0,0,1,0,0,2,0,1,21,1,39,0,0,0,237,219,0,0,3,0,0, 
  0,1,0,0,0,2,240,0,128,1,240,64,128,113,248,0,128,8,4,17,1,148,19,0,0,1,19,86,0,0,0,252,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128, 
  63,64,193,127,4,15,1,116,18,0,0,17,1,217,15,0,0,1,4,17,1,197,18,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,1,1, 
  0,0,2,0,1,21,1,51,0,0,0,226,220,0,0,4,0,0,0,2,0,0,0,110,24,1,128,1,16,129,128,2,16,129,127,113,88,1,128,8,4,17,1,120,38,0,0,1,4,17,1,204,17,0,0,1,19,86,0,0, 
  0,254,0,0,0,2,0,1,21,1,39,0,0,0,237,219,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,113,240,0,128,4,17,1,192,17,0,0,1,8,19,86,0,0,0,3,1,0,0,3,0,1,21, 
  1,75,0,0,0,227,221,0,0,6,0,0,0,2,0,0,0,50,152,65,129,1,80,193,128,2,80,129,127,123,88,1,128,121,216,1,128,122,24,2,128,8,4,17,1,108,39,0,0,1,4,17,1,90,20,0,0,1,4,17, 
  1,78,20,0,0,1,4,17,1,2,39,0,0,1,19,100,0,0,0,36,1,0,0,1,0,1,21,1,105,0,0,0,110,209,0,0,6,0,0,0,2,0,0,0,52,8,67,129,1,80,193,128,2,80,1,128,119,248,1,128, 
  101,152,2,128,120,88,1,128,8,4,15,1,36,22,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,36,22,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,222,21,0,0,17,1,223,25, 
  0,0,1,4,17,1,210,21,0,0,1,2,21,1,105,0,0,0,110,209,0,0,6,0,0,0,2,0,0,0,52,8,67,129,1,80,193,128,2,80,1,128,119,248,1,128,101,152,2,128,120,88,1,128,8,4,15,1,34,21, 
  0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,34,21,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,220,20,0,0,17,1,223,25,0,0,1,4,17,1,208,20,0,0,1,2,21,1, 
  39,0,0,0,250,209,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,125,240,0,128,4,17,1,9,40,0,0,1,8,19,103,0,0,0,43,1,0,0,1,0,1,21,1,75,0,0,0,181,196,0,0,4, 
  0,0,0,2,0,0,0,57,24,1,128,1,16,193,127,2,16,1,128,51,184,1,128,8,4,15,1,85,40,0,0,15,1,35,8,0,0,17,1,81,0,0,0,1,4,15,1,85,40,0,0,15,1,231,6,0,0,17,1,140, 
  3,0,0,1,2,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,184,1,128,37,144,66,128,49,40,130,128,11,248,2,128,65,192,65,128,105,80,1,128,15,1,85,40,0,0,17,1,120,7,0,0,1, 
  1,15,1,85,40,0,0,17,1,108,7,0,0,1,15,1,85,40,0,0,17,1,231,6,0,0,1,15,1,85,40,0,0,17,1,35,8,0,0,1,15,1,85,40,0,0,17,1,96,7,0,0,1,2,19,48,0,0,0,159, 
  0,0,0,1,0,1,21,1,85,1,0,0,99,215,0,0,13,0,0,0,3,0,0,0,88,72,7,131,1,64,196,129,2,64,4,128,51,72,4,128,44,120,69,129,101,120,9,128,6,64,4,128,7,64,4,129,57,0,4,128, 
  92,144,67,128,100,72,8,128,119,232,5,128,120,48,2,128,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1, 
  4,15,1,106,35,0,0,17,1,94,35,0,0,1,6,17,1,47,42,0,0,1,10,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,138,13,0,0,15,1,158,12,0,0,17,1,140,3,0,0, 
  1,4,15,1,82,35,0,0,17,1,30,5,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4, 
  15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25, 
  0,0,17,1,123,25,0,0,1,4,15,1,13,37,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,19,64,0,0,0,199,0,0,0,1,0,1,21, 
  1,51,0,0,0,174,216,0,0,6,0,0,0,2,0,0,0,4,80,1,128,1,144,1,128,2,144,129,128,3,80,129,128,6,144,1,128,7,144,1,128,12,17,1,1,37,0,0,1,10,12,17,1,177,35,0,0,1,21,1, 
  53,0,0,0,252,222,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,15,1,28,87,0,0,17,1,171,42,0,0,1,19,62,0,0,0,193,0,0,0, 
  1,0,1,21,1,195,2,0,0,200,223,0,0,26,0,0,0,4,0,0,0,32,8,147,133,1,208,195,131,2,208,195,131,35,152,16,128,36,200,15,128,37,248,142,132,6,208,3,131,7,208,3,131,40,136,12,128,41,184,11, 
  128,10,120,149,130,43,24,10,128,44,72,73,131,29,168,20,128,46,120,8,128,31,216,147,129,33,56,18,128,34,104,209,129,38,40,14,128,39,88,13,128,42,232,10,128,47,168,7,128,48,216,6,128,53,8,6,128,82,8,5, 
  128,92,216,3,128,8,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,179,85,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,43, 
  85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,13,80,0, 
  0,17,1,60,79,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1, 
  4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,155,85,0,0,15, 
  1,167,85,0,0,15,1,43,85,0,0,17,1,199,69,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88, 
  69,0,0,17,1,1,69,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,83,68,0, 
  0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1,155,85,0, 
  0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15, 
  1,88,69,0,0,17,1,160,66,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,242, 
  65,0,0,1,4,15,1,155,85,0,0,15,1,167,85,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,4,15,1,155,85,0,0,15,1,36,63,0,0,17,1,111,45,0,0,1,2,21,1,181,2,0,0,189,228,0, 
  0,25,0,0,0,4,0,0,0,32,248,18,133,1,176,195,131,2,176,195,131,35,136,16,128,36,184,15,128,37,232,14,132,38,24,14,128,39,72,13,128,40,120,140,131,41,168,11,128,42,216,10,128,11,104,213,129,44,56,9, 
  131,29,152,20,128,46,104,8,128,31,200,19,129,33,40,18,128,34,88,145,129,43,8,10,128,47,152,7,128,48,200,6,128,53,248,5,128,56,136,5,128,82,136,4,128,92,184,3,128,8,4,15,1,235,54,0,0,15,1,203, 
  86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,125,48,0,0,17,1,49,48,0, 
  0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,235,54,0, 
  0,15,1,223,54,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15, 
  1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,43,85,0,0,17,1,199, 
  69,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,235, 
  54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0, 
  0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17, 
  1,78,67,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,160,66,0,0,1,4,15, 
  1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,235,54,0,0,15,1,223,54,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,235,54,0,0,15,1,223, 
  54,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,4,17,1,37,48,0,0,1,2,19,55,0,0,0,174,0,0,0,2,0,1,21,1,51,0,0,0,172,197,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1, 
  240,64,128,57,248,0,128,8,4,15,1,101,48,0,0,15,1,113,48,0,0,17,1,81,0,0,0,1,2,19,36,0,0,0,115,0,0,0,2,0,1,19,54,0,0,0,170,0,0,0,1,0,1,21,7,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,44,16,1,128,125,208,0,128,4,17,1,211,54,0,0,1,4,17,1,195,48,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,2,21,1,153,0,0,0,199,229,0,0,6,0,0,0,2,0,0,0,48,88,2,128,1,80,1,129,2,80,129,128,47,40,3,128,46,248,3,128,57,88,1,128,8,4,15,1,105,49,0,0,15,1,183,50,0,0,15, 
  1,195,50,0,0,15,1,113,48,0,0,17,1,81,0,0,0,1,4,15,1,105,49,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,60,79,0,0,1,4,15,1,105,49,0,0,15,1,183,50,0,0,15,1,93, 
  49,0,0,17,1,107,78,0,0,1,4,15,1,105,49,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,166,76,0,0,1,2,19,35,0,0,0,114,0,0,0,1,0,1,21,0,108,0,0,0,255,255,255,255,6, 
  0,0,0,2,0,0,0,32,136,2,128,9,88,195,128,10,240,194,128,35,32,2,128,37,184,1,128,54,80,1,128,15,1,105,49,0,0,17,1,195,50,0,0,1,15,1,105,49,0,0,17,1,113,48,0,0,1,15,1,105, 
  49,0,0,17,1,183,50,0,0,1,15,1,105,49,0,0,17,1,93,49,0,0,1,15,1,105,49,0,0,17,1,214,49,0,0,1,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,16,1,128, 
  125,208,0,128,4,17,1,170,50,0,0,1,4,17,1,28,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,129,0,0,0,199,229,0,0,6,0, 
  0,0,2,0,0,0,48,40,2,128,1,80,1,129,2,80,129,128,47,200,2,128,46,104,3,128,57,88,1,128,8,4,15,1,158,50,0,0,15,1,195,50,0,0,15,1,113,48,0,0,17,1,81,0,0,0,1,4,15,1, 
  158,50,0,0,15,1,93,49,0,0,17,1,60,79,0,0,1,4,15,1,158,50,0,0,15,1,93,49,0,0,17,1,107,78,0,0,1,4,15,1,158,50,0,0,15,1,93,49,0,0,17,1,166,76,0,0,1,2,19,10, 
  0,0,0,34,0,0,0,3,0,1,19,9,0,0,0,31,0,0,0,5,0,14,1,19,10,0,0,0,33,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1, 
  8,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,113,0,0,0,1,0,1,21,1,55,2,0,0,36,232,0,0,24,0,0,0,4,0, 
  0,0,32,104,15,133,1,144,195,131,2,144,195,131,35,136,13,128,36,232,12,128,37,72,12,132,38,168,11,128,39,8,11,128,40,104,10,128,41,200,9,128,10,72,17,130,43,136,8,128,44,232,199,130,29,168,16,128,46,72, 
  7,128,31,8,16,129,33,200,14,128,34,40,78,129,42,40,9,128,47,168,6,128,48,8,6,128,53,104,5,128,82,152,4,128,92,152,3,128,8,4,15,1,199,54,0,0,15,1,179,85,0,0,15,1,203,86,0,0,15,1, 
  215,86,0,0,17,1,67,85,0,0,1,4,15,1,199,54,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,199,54,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1, 
  199,54,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,199,54,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,199,54,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1, 
  199,54,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,199,54,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,199,54,0,0,15,1,43,85,0,0,17,1,199,69,0,0,1,4,15,1, 
  199,54,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1, 
  199,54,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1, 
  199,54,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,160,66,0,0,1,4,15,1, 
  199,54,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,199,54,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,199,54,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,4,15,1, 
  187,54,0,0,17,1,64,53,0,0,1,2,21,1,45,0,0,0,229,232,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,56,248,0,128,8,4,15,1,110,53,0,0,17,1,49,48,0,0,1,2,21,7, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,16,1,128,125,208,0,128,4,17,1,211,54,0,0,1,4,17,1,180,53,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,2,21,1,153,0,0,0,199,229,0,0,6,0,0,0,2,0,0,0,48,88,2,128,1,80,1,129,2,80,129,128,47,40,3,128,46,248,3,128,57,88,1,128,8,4,15,1,78,54,0,0,15,1, 
  183,50,0,0,15,1,195,50,0,0,15,1,113,48,0,0,17,1,81,0,0,0,1,4,15,1,78,54,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,60,79,0,0,1,4,15,1,78,54,0,0,15,1,183,50, 
  0,0,15,1,93,49,0,0,17,1,107,78,0,0,1,4,15,1,78,54,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,166,76,0,0,1,2,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0, 
  32,136,2,128,9,88,195,128,10,240,194,128,35,32,2,128,37,184,1,128,54,80,1,128,15,1,78,54,0,0,17,1,195,50,0,0,1,15,1,78,54,0,0,17,1,113,48,0,0,1,15,1,78,54,0,0,17,1,183,50, 
  0,0,1,15,1,78,54,0,0,17,1,93,49,0,0,1,15,1,78,54,0,0,17,1,214,49,0,0,1,1,2,19,35,0,0,0,112,0,0,0,3,0,1,19,35,0,0,0,111,0,0,0,3,0,1,19,9,0,0,0, 
  32,0,0,0,3,0,1,19,56,0,0,0,175,0,0,0,1,0,1,21,0,9,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,32,248,2,128,33,64,6,128,34,56,4,128,51,224,7,131,36,16,7,128,53,144,2, 
  128,56,8,5,128,55,48,4,128,8,96,131,127,9,48,4,128,26,120,7,128,27,160,4,128,28,216,5,128,29,168,6,128,30,112,5,128,67,200,3,128,15,1,235,54,0,0,17,1,203,86,0,0,1,15,1,235,54,0,0, 
  17,1,13,80,0,0,1,15,1,235,54,0,0,17,1,223,54,0,0,1,15,1,235,54,0,0,17,1,159,62,0,0,1,1,15,1,235,54,0,0,17,1,31,85,0,0,1,15,1,235,54,0,0,17,1,88,69,0,0,1, 
  15,1,235,54,0,0,17,1,26,60,0,0,1,15,1,235,54,0,0,17,1,154,76,0,0,1,15,1,235,54,0,0,17,1,187,69,0,0,1,15,1,235,54,0,0,17,1,163,57,0,0,1,15,1,235,54,0,0,17,1, 
  43,85,0,0,1,15,1,235,54,0,0,17,1,245,55,0,0,1,15,1,235,54,0,0,17,1,230,65,0,0,1,15,1,235,54,0,0,17,1,55,85,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,44,16,1,128,125,208,0,128,4,17,1,150,57,0,0,1,4,17,1,59,56,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,153,0,0, 
  0,199,229,0,0,6,0,0,0,2,0,0,0,48,88,2,128,1,80,1,129,2,80,129,128,47,40,3,128,46,248,3,128,57,88,1,128,8,4,15,1,213,56,0,0,15,1,183,50,0,0,15,1,195,50,0,0,15,1,113, 
  48,0,0,17,1,81,0,0,0,1,4,15,1,213,56,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,60,79,0,0,1,4,15,1,213,56,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,107,78,0, 
  0,1,4,15,1,213,56,0,0,15,1,183,50,0,0,15,1,93,49,0,0,17,1,166,76,0,0,1,2,21,0,108,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,32,136,2,128,9,88,195,128,10,240,194,128,35, 
  32,2,128,37,184,1,128,54,80,1,128,15,1,213,56,0,0,17,1,195,50,0,0,1,15,1,213,56,0,0,17,1,113,48,0,0,1,15,1,213,56,0,0,17,1,183,50,0,0,1,15,1,213,56,0,0,17,1,93,49, 
  0,0,1,15,1,213,56,0,0,17,1,66,57,0,0,1,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,16,1,128,125,208,0,128,4,17,1,136,57,0,0,1,4,17,1,28,50,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,9,0,0,0,31,0,0,0,5,0,14,14,1,19,9,0,0,0,32,0,0,0,3,0,14,1,21,7,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,17,1,232,57,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,8,0,0,0,29,0,0, 
  0,1,0,1,21,1,37,2,0,0,77,233,0,0,23,0,0,0,4,0,0,0,32,72,207,132,1,112,195,131,2,112,195,131,35,104,13,128,36,200,12,128,37,40,204,131,38,136,11,128,39,232,10,128,40,72,10,128,41,168, 
  9,128,42,8,9,128,43,104,8,128,44,200,135,130,29,136,16,128,46,40,7,128,31,232,207,128,33,168,14,128,34,8,14,129,47,136,6,128,48,232,5,128,53,72,5,128,82,120,4,128,92,120,3,128,8,4,15,1,14,60, 
  0,0,15,1,179,85,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,14,60,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,14,60,0,0, 
  15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,14,60,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,14,60,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,14,60,0,0, 
  15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1,14,60,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,14,60,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,14,60,0,0, 
  15,1,43,85,0,0,17,1,199,69,0,0,1,4,15,1,14,60,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,14,60,0,0, 
  15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,14,60,0,0, 
  15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,14,60,0,0, 
  15,1,88,69,0,0,17,1,160,66,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,14,60,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,14,60,0,0, 
  15,1,230,65,0,0,17,1,175,64,0,0,1,2,19,33,0,0,0,108,0,0,0,3,0,1,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,16,1,128,59,208,192,127,4,17,1,109,60,0,0,1, 
  4,17,1,96,60,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,55,0,0,0,173,0,0,0,3,0,14,1,21,1,37,2,0,0,77,233,0,0,23, 
  0,0,0,4,0,0,0,32,72,207,132,1,112,195,131,2,112,195,131,35,104,13,128,36,200,12,128,37,40,204,131,38,136,11,128,39,232,10,128,40,72,10,128,41,168,9,128,42,8,9,128,43,104,8,128,44,200,135,130,29, 
  136,16,128,46,40,7,128,31,232,207,128,33,168,14,128,34,8,14,129,47,136,6,128,48,232,5,128,53,72,5,128,82,120,4,128,92,120,3,128,8,4,15,1,147,62,0,0,15,1,179,85,0,0,15,1,203,86,0,0,15, 
  1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,147,62,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,147,62,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15, 
  1,147,62,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,147,62,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,147,62,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15, 
  1,147,62,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,147,62,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,147,62,0,0,15,1,43,85,0,0,17,1,199,69,0,0,1,4,15, 
  1,147,62,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15, 
  1,147,62,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15, 
  1,147,62,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,160,66,0,0,1,4,15, 
  1,147,62,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,147,62,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,147,62,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,2,19, 
  56,0,0,0,176,0,0,0,3,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,228,62,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,53,0,0,0,168,0,0,0,1,0,1,21,1,51,0,0,0,172,197,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,15,1,24,63,0,0,15,1, 
  113,48,0,0,17,1,81,0,0,0,1,2,19,53,0,0,0,169,0,0,0,3,0,1,21,0,244,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,8,56,7,130,9,208,6,130,26,104,6,130,27,0,6,130,28,152, 
  5,128,29,48,197,129,30,200,4,128,55,184,2,128,32,96,4,128,33,248,3,128,34,144,3,128,51,40,131,128,53,192,2,128,67,80,2,128,15,1,36,63,0,0,17,1,106,64,0,0,1,1,15,1,36,63,0,0,17,1, 
  203,86,0,0,1,15,1,36,63,0,0,17,1,55,85,0,0,1,15,1,36,63,0,0,17,1,31,85,0,0,1,15,1,36,63,0,0,17,1,37,64,0,0,1,15,1,36,63,0,0,17,1,13,80,0,0,1,15,1,36, 
  63,0,0,17,1,154,76,0,0,1,15,1,36,63,0,0,17,1,43,85,0,0,1,15,1,36,63,0,0,17,1,187,69,0,0,1,15,1,36,63,0,0,17,1,88,69,0,0,1,15,1,36,63,0,0,17,1,230,65,0, 
  0,1,15,1,36,63,0,0,17,1,25,64,0,0,1,15,1,36,63,0,0,17,1,167,85,0,0,1,2,19,55,0,0,0,171,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43, 
  176,0,128,4,17,1,232,57,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,8,0,0,0,29,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,228,62,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,53,0,0,0,168,0,0,0,1,0,1,21, 
  7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,218,65,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,19,26,0,0,0,72,0,0,0,1,0,1,21,1,129,0,0,0,2,234,0,0,6,0,0,0,2,0,0,0,48,40,66,129,1,80,1,128,2,80,129,128,47,200,2,128,46,104,3,128,92,88,1,128,8,4,15,1, 
  136,65,0,0,15,1,206,65,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,136,65,0,0,15,1,124,65,0,0,17,1,60,79,0,0,1,4,15,1,136,65,0,0,15,1,124,65,0,0,17,1,107,78, 
  0,0,1,4,15,1,136,65,0,0,15,1,124,65,0,0,17,1,166,76,0,0,1,2,19,66,0,0,0,202,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,17, 
  1,194,65,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,50,0,0,0,164,0,0,0,3,0,1,19,66,0,0,0,201,0,0,0,1,0,1,19,26, 
  0,0,0,71,0,0,0,2,0,1,19,8,0,0,0,23,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,61,66,0,0,17,1,250,64,0,0,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,82,0,0,0,1,0,1,19,27,0,0,0,81,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,40,176,0,128,4,15,1,148,66,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,74,0,0, 
  0,1,0,1,19,27,0,0,0,73,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,235,66,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,90,0,0,0,1,0,1,19,27,0,0,0,89,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,40,176,0,128,4,15,1,66,67,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,86,0,0,0,1,0,1,19,27, 
  0,0,0,85,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,153,67,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,78,0,0,0,1,0,1,19,27,0,0,0,77,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4, 
  15,1,240,67,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,92,0,0,0,1,0,1,19,27,0,0,0,91,0,0, 
  0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,71,68,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,19,27,0,0,0,88,0,0,0,1,0,1,19,27,0,0,0,87,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,158,68,0,0, 
  17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,80,0,0,0,1,0,1,19,27,0,0,0,79,0,0,0,2,0,1,21,7, 
  36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,245,68,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,19,27,0,0,0,84,0,0,0,1,0,1,19,27,0,0,0,83,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,76,69,0,0,17,1,250,64,0,0, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,27,0,0,0,76,0,0,0,1,0,1,19,27,0,0,0,75,0,0,0,2,0,1,19,8,0,0,0,24,0,0, 
  0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,175,69,0,0,17,1,250,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,19,28,0,0,0,94,0,0,0,1,0,1,19,28,0,0,0,93,0,0,0,2,0,1,19,8,0,0,0,25,0,0,0,1,0,1,19,29,0,0,0,95,0,0,0,1,0,1,19,29,0,0,0, 
  96,0,0,0,1,0,1,21,1,163,2,0,0,130,234,0,0,24,0,0,0,4,0,0,0,32,168,18,133,1,144,195,131,2,144,195,131,35,56,16,128,36,104,15,128,37,152,14,132,38,200,13,128,39,248,12,128,40,40,12, 
  128,41,88,11,128,42,136,10,128,43,184,9,128,44,232,200,130,29,72,84,129,46,216,7,128,31,120,19,129,33,216,17,128,34,8,81,129,45,168,8,128,47,8,7,128,48,56,6,128,53,104,5,128,82,104,4,128,92,152,3, 
  128,8,4,15,1,155,72,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1, 
  4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,155,72,0,0,15, 
  1,143,72,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,17,1,131,72,0,0,1,4,15,1,155,72,0,0,15, 
  1,143,72,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,43, 
  85,0,0,17,1,199,69,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,1,69,0, 
  0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,155,72,0, 
  0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15, 
  1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,160, 
  66,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,155,72,0,0,15,1,143,72,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,155, 
  72,0,0,15,1,143,72,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,2,19,30,0,0,0,99,0,0,0,2,0,1,19,31,0,0,0,100,0,0,0,1,0,1,21,0,239,0,0,0,255,255,255,255,13,0,0, 
  0,3,0,0,0,8,16,7,130,33,208,3,128,26,168,198,129,27,64,198,129,28,216,5,128,29,112,133,129,30,8,5,128,31,160,4,128,32,56,4,128,34,104,3,128,51,0,131,128,53,152,2,128,67,48,2,128,15,1,155, 
  72,0,0,17,1,85,76,0,0,1,15,1,155,72,0,0,17,1,203,86,0,0,1,15,1,155,72,0,0,17,1,55,85,0,0,1,15,1,155,72,0,0,17,1,31,85,0,0,1,15,1,155,72,0,0,17,1,16,76,0, 
  0,1,15,1,155,72,0,0,17,1,13,80,0,0,1,15,1,155,72,0,0,17,1,139,73,0,0,1,15,1,155,72,0,0,17,1,154,76,0,0,1,15,1,155,72,0,0,17,1,43,85,0,0,1,15,1,155,72,0,0, 
  17,1,187,69,0,0,1,15,1,155,72,0,0,17,1,88,69,0,0,1,15,1,155,72,0,0,17,1,230,65,0,0,1,15,1,155,72,0,0,17,1,143,72,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,44,16,1,128,93,208,0,128,4,17,1,3,76,0,0,1,4,17,1,209,73,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,37, 
  2,0,0,77,233,0,0,23,0,0,0,4,0,0,0,32,72,207,132,1,112,195,131,2,112,195,131,35,104,13,128,36,200,12,128,37,40,204,131,38,136,11,128,39,232,10,128,40,72,10,128,41,168,9,128,42,8,9,128,43, 
  104,8,128,44,200,135,130,29,136,16,128,46,40,7,128,31,232,207,128,33,168,14,128,34,8,14,129,47,136,6,128,48,232,5,128,53,72,5,128,82,120,4,128,92,120,3,128,8,4,15,1,247,75,0,0,15,1,179,85,0, 
  0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,247,75,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,247,75,0,0,15,1,31,85,0,0,17, 
  1,25,80,0,0,1,4,15,1,247,75,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,247,75,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,247,75,0,0,15,1,13,80,0,0,17, 
  1,166,76,0,0,1,4,15,1,247,75,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,247,75,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,247,75,0,0,15,1,43,85,0,0,17, 
  1,199,69,0,0,1,4,15,1,247,75,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17, 
  1,170,68,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17, 
  1,165,67,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17, 
  1,160,66,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,247,75,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,247,75,0,0,15,1,230,65,0,0,17, 
  1,175,64,0,0,1,2,19,31,0,0,0,101,0,0,0,3,0,1,19,30,0,0,0,98,0,0,0,3,0,14,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,17,1,232,57,0, 
  0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,8,0,0,0,29,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46, 
  176,0,128,4,17,1,228,62,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,53,0,0,0,168,0,0,0,1,0,1,19,8,0,0,0,27,0,0,0,1, 
  0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,6,17,1,235,76,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,19, 
  32,0,0,0,105,0,0,0,1,0,1,21,1,51,0,0,0,67,235,0,0,4,0,0,0,2,0,0,0,57,88,1,128,1,16,193,127,2,16,65,128,82,24,1,128,10,12,17,1,43,77,0,0,1,12,17,1,31,77,0, 
  0,1,2,19,32,0,0,0,105,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,95,78,0,0,17,1,107,77,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,45,0,0,0,168,235,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,82,248,0,128,8,4,15,1,153,77,0,0,17,1,204, 
  2,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,16,65,128,62,208,0,128,4,17,1,83,78,0,0,1,4,17,1,223,77,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,45,0,0,0,168,235,0,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,82,248,0,128,8,4,15,1,13,78,0,0,17,1,204,2,0,0,1, 
  2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,17,1,71,78,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19, 
  52,0,0,0,166,0,0,0,5,0,1,19,52,0,0,0,167,0,0,0,3,0,1,19,32,0,0,0,102,0,0,0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,6,17,1, 
  176,78,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,19,32,0,0,0,106,0,0,0,1,0,1,21,1,51,0,0,0,67,235,0,0,4,0,0,0,2,0, 
  0,0,57,88,1,128,1,16,193,127,2,16,65,128,82,24,1,128,10,12,17,1,240,78,0,0,1,12,17,1,228,78,0,0,1,2,19,32,0,0,0,106,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,60,176,0,128,4,15,1,48,79,0,0,17,1,107,77,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,32,0,0,0,103,0,0, 
  0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,6,17,1,129,79,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  10,19,32,0,0,0,107,0,0,0,1,0,1,21,1,51,0,0,0,67,235,0,0,4,0,0,0,2,0,0,0,57,88,1,128,1,16,193,127,2,16,65,128,82,24,1,128,10,12,17,1,193,79,0,0,1,12,17,1,181, 
  79,0,0,1,2,19,32,0,0,0,107,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,1,80,0,0,17,1,107,77,0,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,32,0,0,0,104,0,0,0,2,0,1,19,8,0,0,0,28,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,40,176,0,128,4,17,1,83,80,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,37,2,0,0,77,233,0,0,23,0,0,0,4,0,0, 
  0,32,72,207,132,1,112,195,131,2,112,195,131,35,104,13,128,36,200,12,128,37,40,204,131,38,136,11,128,39,232,10,128,40,72,10,128,41,168,9,128,42,8,9,128,43,104,8,128,44,200,135,130,29,136,16,128,46,40,7, 
  128,31,232,207,128,33,168,14,128,34,8,14,129,47,136,6,128,48,232,5,128,53,72,5,128,82,120,4,128,92,120,3,128,8,4,15,1,121,82,0,0,15,1,179,85,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17, 
  1,67,85,0,0,1,4,15,1,121,82,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,121,82,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,121,82,0,0,15, 
  1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,121,82,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,121,82,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1,121,82,0,0,15, 
  1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,121,82,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,121,82,0,0,15,1,43,85,0,0,17,1,199,69,0,0,1,4,15,1,121,82,0,0,15, 
  1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,121,82,0,0,15, 
  1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1,121,82,0,0,15, 
  1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,160,66,0,0,1,4,15,1,121,82,0,0,15, 
  1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,121,82,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,121,82,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,2,21,7,30,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,179,82,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,37,2,0,0,77,233,0, 
  0,23,0,0,0,4,0,0,0,32,72,207,132,1,112,195,131,2,112,195,131,35,104,13,128,36,200,12,128,37,40,204,131,38,136,11,128,39,232,10,128,40,72,10,128,41,168,9,128,42,8,9,128,43,104,8,128,44,200,135, 
  130,29,136,16,128,46,40,7,128,31,232,207,128,33,168,14,128,34,8,14,129,47,136,6,128,48,232,5,128,53,72,5,128,82,120,4,128,92,120,3,128,8,4,15,1,217,84,0,0,15,1,179,85,0,0,15,1,203,86,0, 
  0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,217,84,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,217,84,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1, 
  4,15,1,217,84,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,217,84,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,217,84,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1, 
  4,15,1,217,84,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,217,84,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,217,84,0,0,15,1,43,85,0,0,17,1,199,69,0,0,1, 
  4,15,1,217,84,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1, 
  4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1, 
  4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,160,66,0,0,1, 
  4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,217,84,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,217,84,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1, 
  2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,17,1,19,85,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19, 
  34,0,0,0,110,0,0,0,6,0,1,19,8,0,0,0,30,0,0,0,1,0,1,19,8,0,0,0,26,0,0,0,1,0,1,19,29,0,0,0,97,0,0,0,1,0,1,21,1,63,0,0,0,67,235,0,0,4,0,0, 
  0,2,0,0,0,57,136,1,128,1,16,193,127,2,16,65,128,82,24,1,128,8,4,15,1,143,85,0,0,17,1,204,2,0,0,1,4,15,1,131,85,0,0,17,1,81,0,0,0,1,2,19,67,0,0,0,203,0,0,0, 
  2,0,1,19,67,0,0,0,204,0,0,0,2,0,1,19,90,0,0,0,16,1,0,0,2,0,1,19,55,0,0,0,172,0,0,0,1,0,1,21,0,210,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,8,136,198, 
  129,33,176,3,128,26,32,134,129,27,184,133,129,28,80,5,128,29,232,68,129,30,128,4,128,32,24,4,128,34,72,3,128,51,224,130,128,53,120,2,128,67,16,2,128,15,1,179,85,0,0,17,1,215,86,0,0,1,15,1, 
  179,85,0,0,17,1,203,86,0,0,1,15,1,179,85,0,0,17,1,55,85,0,0,1,15,1,179,85,0,0,17,1,31,85,0,0,1,15,1,179,85,0,0,17,1,134,86,0,0,1,15,1,179,85,0,0,17,1,13,80, 
  0,0,1,15,1,179,85,0,0,17,1,154,76,0,0,1,15,1,179,85,0,0,17,1,43,85,0,0,1,15,1,179,85,0,0,17,1,187,69,0,0,1,15,1,179,85,0,0,17,1,88,69,0,0,1,15,1,179,85,0, 
  0,17,1,230,65,0,0,1,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,17,1,232,57,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,19,8,0,0,0,29,0,0,0,1,0,1,19,33,0,0,0,109,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,228,62,0,0, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,53,0,0,0,168,0,0,0,1,0,1,19,62,0,0,0,192,0,0,0,2,0,1,21,1,69,0,0,0,223,217, 
  0,0,5,0,0,0,2,0,0,0,10,48,193,128,1,160,193,128,2,160,129,127,110,168,1,128,113,232,1,128,4,15,1,190,87,0,0,17,1,217,15,0,0,1,8,4,17,1,121,87,0,0,1,4,17,1,226,37,0,0, 
  1,19,86,0,0,0,10,1,0,0,1,0,1,21,1,57,0,0,0,236,218,0,0,4,0,0,0,2,0,0,0,10,88,1,128,1,80,129,128,2,80,129,127,113,16,1,128,4,17,1,160,19,0,0,1,8,4,15,1,175, 
  37,0,0,17,1,217,15,0,0,1,19,86,0,0,0,0,1,0,0,2,0,1,21,1,51,0,0,0,226,220,0,0,4,0,0,0,2,0,0,0,110,80,1,128,1,144,129,128,2,144,129,127,113,16,1,128,4,17,1,204, 
  17,0,0,1,4,17,1,120,38,0,0,1,8,19,86,0,0,0,254,0,0,0,2,0,1,21,1,75,0,0,0,227,221,0,0,6,0,0,0,2,0,0,0,50,216,65,129,1,144,193,128,2,144,129,127,123,152,1,128,121, 
  24,2,128,122,80,1,128,4,17,1,2,39,0,0,1,8,4,17,1,108,39,0,0,1,4,17,1,90,20,0,0,1,4,17,1,78,20,0,0,1,19,100,0,0,0,36,1,0,0,1,0,1,19,63,0,0,0,194,0,0, 
  0,1,0,1,21,1,69,0,0,0,223,235,0,0,5,0,0,0,2,0,0,0,10,184,193,128,1,176,193,128,2,176,129,127,110,112,1,128,113,48,1,128,4,17,1,155,89,0,0,1,4,17,1,35,89,0,0,1,8,4, 
  15,1,177,88,0,0,17,1,217,15,0,0,1,19,86,0,0,0,10,1,0,0,1,0,1,21,1,51,0,0,0,248,236,0,0,4,0,0,0,2,0,0,0,110,88,1,128,1,16,129,128,2,16,129,127,113,24,1,128,8, 
  4,17,1,204,17,0,0,1,4,17,1,240,88,0,0,1,19,86,0,0,0,254,0,0,0,2,0,1,21,1,39,0,0,0,5,238,0,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,113,248,0,128,8,4, 
  17,1,192,17,0,0,1,19,86,0,0,0,3,1,0,0,3,0,1,21,1,57,0,0,0,6,239,0,0,4,0,0,0,2,0,0,0,10,16,1,128,1,128,129,128,2,128,129,127,113,136,1,128,4,15,1,104,89,0,0, 
  17,1,217,15,0,0,1,8,4,17,1,160,19,0,0,1,19,86,0,0,0,0,1,0,0,2,0,1,21,1,39,0,0,0,5,238,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,113,240,0,128,4,17, 
  1,148,19,0,0,1,8,19,86,0,0,0,252,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,116,18,0,0,17,1,217,15,0,0,1,4, 
  17,1,197,18,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,1,1,0,0,2,0,1,21,1,75,0,0,0,19,240,0,0,6,0,0,0, 
  2,0,0,0,50,24,66,129,1,16,194,128,2,16,130,127,123,144,1,128,121,80,1,128,122,208,1,128,4,17,1,78,20,0,0,1,4,17,1,179,90,0,0,1,4,17,1,73,90,0,0,1,8,4,17,1,90,20,0,0, 
  1,19,100,0,0,0,36,1,0,0,1,0,1,21,1,105,0,0,0,110,209,0,0,6,0,0,0,2,0,0,0,52,8,67,129,1,80,193,128,2,80,1,128,119,248,1,128,101,152,2,128,120,88,1,128,8,4,15,1,36, 
  22,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,36,22,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,222,21,0,0,17,1,223,25,0,0,1,4,17,1,210,21,0,0,1,2,21, 
  1,105,0,0,0,110,209,0,0,6,0,0,0,2,0,0,0,52,8,67,129,1,80,193,128,2,80,1,128,119,248,1,128,101,152,2,128,120,88,1,128,8,4,15,1,34,21,0,0,15,1,53,91,0,0,17,1,223,26,0, 
  0,1,4,15,1,34,21,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,220,20,0,0,17,1,223,25,0,0,1,4,17,1,208,20,0,0,1,2,19,103,0,0,0,45,1,0,0,1,0,1,19,61,0, 
  0,0,189,0,0,0,1,0,1,19,83,0,0,0,241,0,0,0,1,0,1,21,1,47,0,0,0,56,241,0,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,4, 
  17,1,124,91,0,0,1,8,19,38,0,0,0,124,0,0,0,3,0,1,21,1,195,2,0,0,200,223,0,0,26,0,0,0,4,0,0,0,32,8,147,133,1,208,195,131,2,208,195,131,35,152,16,128,36,200,15,128,37,248, 
  142,132,6,208,3,131,7,208,3,131,40,136,12,128,41,184,11,128,10,120,149,130,43,24,10,128,44,72,73,131,29,168,20,128,46,120,8,128,31,216,147,129,33,56,18,128,34,104,209,129,38,40,14,128,39,88,13,128,42,232, 
  10,128,47,168,7,128,48,216,6,128,53,8,6,128,82,8,5,128,92,216,3,128,8,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,179,85,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0, 
  1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4, 
  15,1,64,94,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,64,94,0,0,15,1, 
  167,85,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,43,85, 
  0,0,17,1,211,69,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,43,85,0,0,17,1,199,69,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,187,69,0,0,17,1,100,69,0,0, 
  1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,1,69,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,64,94,0,0, 
  15,1,167,85,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1, 
  88,69,0,0,17,1,165,67,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,247,66, 
  0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,160,66,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,64,94, 
  0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,64,94,0,0,15,1,167,85,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,4,15,1,64,94,0,0,15,1,36,63,0,0, 
  17,1,111,45,0,0,1,2,19,38,0,0,0,122,0,0,0,5,0,1,21,0,74,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,49,120,129,128,37,128,193,127,65,16,1,128,11,232,1,128,15,1,76,94,0,0, 
  17,1,108,7,0,0,1,1,15,1,76,94,0,0,17,1,151,94,0,0,1,15,1,76,94,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,217,241,0,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65, 
  128,91,240,0,128,4,17,1,86,8,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,47,0,0,0,234,251,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128, 
  58,56,1,128,8,4,17,1,250,94,0,0,1,2,21,1,101,0,0,0,100,252,0,0,7,0,0,0,2,0,0,0,6,112,129,129,1,112,1,129,2,112,129,127,7,112,65,128,51,184,2,128,57,232,1,128,82,120,1,128, 
  8,4,15,1,107,98,0,0,17,1,204,2,0,0,1,4,15,1,96,95,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,96,95,0,0,17,1,140,3,0,0,1,2,21,1,47,0, 
  0,0,56,241,0,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,4,17,1,155,95,0,0,1,8,19,38,0,0,0,123,0,0,0,5,0,1,21,1,195,2,0, 
  0,200,223,0,0,26,0,0,0,4,0,0,0,32,8,147,133,1,208,195,131,2,208,195,131,35,152,16,128,36,200,15,128,37,248,142,132,6,208,3,131,7,208,3,131,40,136,12,128,41,184,11,128,10,120,149,130,43,24,10, 
  128,44,72,73,131,29,168,20,128,46,120,8,128,31,216,147,129,33,56,18,128,34,104,209,129,38,40,14,128,39,88,13,128,42,232,10,128,47,168,7,128,48,216,6,128,53,8,6,128,82,8,5,128,92,216,3,128,8,4,15, 
  1,95,98,0,0,15,1,167,85,0,0,15,1,179,85,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,43,85,0,0,15,1,55,85,0, 
  0,17,1,204,2,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,60,79,0,0,1, 
  4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1,95,98,0,0,15, 
  1,167,85,0,0,15,1,154,76,0,0,17,1,223,69,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,43, 
  85,0,0,17,1,199,69,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,1,69,0, 
  0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,95,98,0, 
  0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,165,67,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15, 
  1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,160, 
  66,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,95,98,0,0,15,1,167,85,0,0,15,1,88,69,0,0,17,1,242,65,0,0,1,4,15,1,95, 
  98,0,0,15,1,167,85,0,0,15,1,230,65,0,0,17,1,175,64,0,0,1,4,15,1,95,98,0,0,15,1,36,63,0,0,17,1,111,45,0,0,1,2,19,38,0,0,0,121,0,0,0,7,0,1,21,1,65,0,0, 
  0,225,252,0,0,6,0,0,0,2,0,0,0,6,80,65,129,1,80,193,128,2,80,129,127,7,80,1,128,61,200,1,128,82,88,1,128,8,4,15,1,31,99,0,0,17,1,204,2,0,0,1,4,17,1,173,98,0,0,1, 
  2,21,1,47,0,0,0,165,253,0,0,5,0,0,0,2,0,0,0,60,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,221,98,0,0,1,2,21,1,53,0,0,0,117,180,0,0,5, 
  0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,15,1,19,99,0,0,17,1,204,2,0,0,1,2,19,38,0,0,0,117,0,0,0,8,0,1,19,38,0,0,0, 
  119,0,0,0,6,0,1,21,1,65,0,0,0,225,252,0,0,6,0,0,0,2,0,0,0,6,80,65,129,1,80,193,128,2,80,129,127,7,80,1,128,61,200,1,128,82,88,1,128,8,4,15,1,223,99,0,0,17,1,204, 
  2,0,0,1,4,17,1,109,99,0,0,1,2,21,1,47,0,0,0,165,253,0,0,5,0,0,0,2,0,0,0,60,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,157,99,0,0,1, 
  2,21,1,53,0,0,0,117,180,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,15,1,211,99,0,0,17,1,204,2,0,0,1,2,19,38,0,0,0, 
  118,0,0,0,6,0,1,19,38,0,0,0,120,0,0,0,4,0,1,21,1,53,0,0,0,117,180,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,15, 
  1,33,100,0,0,17,1,204,2,0,0,1,2,19,38,0,0,0,125,0,0,0,2,0,1,19,38,0,0,0,127,0,0,0,1,0,1,21,1,53,0,0,0,117,180,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1, 
  48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,15,1,111,100,0,0,17,1,204,2,0,0,1,2,19,38,0,0,0,126,0,0,0,2,0,1,21,0,5,1,0,0,255,255,255,255,15,0,0,0,3,0,0, 
  0,40,128,134,130,57,24,6,128,58,176,5,128,59,72,5,130,12,32,8,129,38,80,199,129,14,184,199,127,39,232,134,129,60,224,68,128,68,120,4,128,72,16,4,129,75,168,3,128,78,64,3,128,79,216,2,128,80,112,2, 
  128,15,1,123,100,0,0,17,1,141,138,0,0,1,15,1,123,100,0,0,17,1,82,136,0,0,1,15,1,123,100,0,0,17,1,230,135,0,0,1,15,1,123,100,0,0,17,1,16,108,0,0,1,15,1,123,100,0,0,17, 
  1,4,108,0,0,1,15,1,123,100,0,0,17,1,248,107,0,0,1,15,1,123,100,0,0,17,1,154,112,0,0,1,15,1,123,100,0,0,17,1,104,107,0,0,1,15,1,123,100,0,0,17,1,129,138,0,0,1,15,1, 
  123,100,0,0,17,1,211,134,0,0,1,15,1,123,100,0,0,17,1,117,138,0,0,1,15,1,123,100,0,0,17,1,24,103,0,0,1,15,1,123,100,0,0,17,1,28,108,0,0,1,15,1,123,100,0,0,17,1,129,101, 
  0,0,1,1,2,21,1,47,0,0,0,93,254,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,188,101,0,0,1,19,12,0,0,0,41,0,0, 
  0,1,0,1,21,1,67,1,0,0,84,255,0,0,16,0,0,0,4,0,0,0,64,200,8,128,1,144,66,130,2,144,130,128,67,232,199,130,66,88,8,130,85,72,7,128,6,144,130,128,7,144,130,128,86,168,6,128,87,8, 
  6,128,97,56,4,128,59,168,9,128,98,104,3,128,93,56,5,128,99,152,2,128,63,56,9,128,8,4,15,1,12,103,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,12,103,0,0, 
  15,1,129,138,0,0,15,1,82,136,0,0,17,1,242,135,0,0,1,4,15,1,12,103,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,12,103,0,0,15,1, 
  211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,12,103,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,12,103,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1, 
  12,103,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,0,103,0,0,17,1,57,100,0,0,1,4,15,1,0,103,0,0,17,1,45,100,0,0,1,4,15,1,0,103,0,0,17,1,235,99,0,0,1,4, 
  15,1,0,103,0,0,17,1,228,2,0,0,1,4,15,1,0,103,0,0,17,1,139,2,0,0,1,2,19,14,0,0,0,46,0,0,0,3,0,1,19,12,0,0,0,40,0,0,0,3,0,1,21,1,47,0,0,0,93,254, 
  0,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,83,103,0,0,1,8,19,12,0,0,0,39,0,0,0,1,0,1,21,1,97,1,0,0,84,255,0, 
  0,16,0,0,0,4,0,0,0,64,40,9,128,1,144,66,130,2,144,130,128,67,232,199,130,66,136,8,130,85,72,7,128,6,144,130,128,7,144,130,128,86,168,6,128,87,8,6,128,97,56,4,128,59,104,10,128,98,104,3, 
  128,93,56,5,128,99,152,2,128,63,200,9,128,8,4,15,1,92,107,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,92,107,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17, 
  1,242,135,0,0,1,4,15,1,92,107,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,92,107,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166, 
  112,0,0,1,4,15,1,92,107,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,92,107,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,92,107,0,0,15,1,154,112,0,0,17,1,118, 
  112,0,0,1,4,15,1,181,104,0,0,15,1,80,107,0,0,17,1,57,100,0,0,1,4,15,1,181,104,0,0,15,1,80,107,0,0,17,1,45,100,0,0,1,4,15,1,181,104,0,0,15,1,80,107,0,0,17,1,235, 
  99,0,0,1,4,15,1,181,104,0,0,15,1,80,107,0,0,17,1,228,2,0,0,1,4,15,1,181,104,0,0,15,1,80,107,0,0,17,1,139,2,0,0,1,2,21,0,244,0,0,0,255,255,255,255,14,0,0,0,3, 
  0,0,0,40,96,134,130,57,248,5,128,58,144,5,128,59,40,5,130,12,152,7,129,13,48,7,128,38,200,134,129,79,184,2,128,60,192,68,128,68,88,4,128,72,240,195,128,75,136,3,128,78,32,3,128,80,80,2,128,15, 
  1,181,104,0,0,17,1,141,138,0,0,1,15,1,181,104,0,0,17,1,82,136,0,0,1,15,1,181,104,0,0,17,1,230,135,0,0,1,15,1,181,104,0,0,17,1,16,108,0,0,1,15,1,181,104,0,0,17,1,4, 
  108,0,0,1,15,1,181,104,0,0,17,1,248,107,0,0,1,15,1,181,104,0,0,17,1,154,112,0,0,1,15,1,181,104,0,0,17,1,104,107,0,0,1,15,1,181,104,0,0,17,1,129,138,0,0,1,15,1,181,104, 
  0,0,17,1,211,134,0,0,1,15,1,181,104,0,0,17,1,67,107,0,0,1,15,1,181,104,0,0,17,1,80,107,0,0,1,15,1,181,104,0,0,17,1,170,105,0,0,1,1,2,21,1,47,0,0,0,93,254,0,0, 
  5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,230,105,0,0,1,8,19,12,0,0,0,38,0,0,0,3,0,14,1,21,1,67,1,0,0,84,255,0,0, 
  16,0,0,0,4,0,0,0,64,200,8,128,1,144,66,130,2,144,130,128,67,232,199,130,66,88,8,130,85,72,7,128,6,144,130,128,7,144,130,128,86,168,6,128,87,8,6,128,97,56,4,128,59,168,9,128,98,104,3,128, 
  93,56,5,128,99,152,2,128,63,56,9,128,8,4,15,1,54,107,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,54,107,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17,1, 
  242,135,0,0,1,4,15,1,54,107,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,54,107,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112, 
  0,0,1,4,15,1,54,107,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,54,107,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,54,107,0,0,15,1,154,112,0,0,17,1,118,112, 
  0,0,1,4,15,1,42,107,0,0,17,1,57,100,0,0,1,4,15,1,42,107,0,0,17,1,45,100,0,0,1,4,15,1,42,107,0,0,17,1,235,99,0,0,1,4,15,1,42,107,0,0,17,1,228,2,0,0,1,4, 
  15,1,42,107,0,0,17,1,139,2,0,0,1,2,19,13,0,0,0,44,0,0,0,3,0,1,19,12,0,0,0,36,0,0,0,5,0,14,1,19,12,0,0,0,37,0,0,0,3,0,14,1,19,13,0,0,0,43,0,0, 
  0,1,0,1,19,12,0,0,0,37,0,0,0,3,0,1,21,1,47,0,0,0,133,0,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,152,107, 
  0,0,1,2,21,1,71,0,0,0,61,1,1,0,6,0,0,0,2,0,0,0,6,80,65,129,1,80,193,128,2,80,129,127,7,80,1,128,97,200,1,128,98,88,1,128,8,4,15,1,236,107,0,0,17,1,242,135,0,0, 
  1,4,15,1,224,107,0,0,17,1,42,135,0,0,1,2,19,59,0,0,0,184,0,0,0,3,0,1,19,58,0,0,0,180,0,0,0,3,0,1,19,57,0,0,0,177,0,0,0,1,0,1,19,57,0,0,0,178,0,0, 
  0,1,0,1,19,57,0,0,0,179,0,0,0,1,0,1,19,14,0,0,0,45,0,0,0,1,0,1,21,1,71,0,0,0,189,1,1,0,7,0,0,0,2,0,0,0,72,120,1,128,1,112,1,128,2,112,129,128,7,112, 
  129,128,6,112,1,128,47,248,65,128,71,184,1,128,8,4,17,1,136,108,0,0,1,4,17,1,124,108,0,0,1,4,17,1,112,108,0,0,1,2,19,39,0,0,0,135,0,0,0,2,0,1,19,39,0,0,0,137,0,0, 
  0,2,0,1,21,1,59,0,0,0,83,3,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,128,2,80,129,127,7,80,65,128,47,152,65,128,71,88,1,128,8,4,17,1,208,108,0,0,1,4,17,1,196,108, 
  0,0,1,2,19,39,0,0,0,134,0,0,0,3,0,1,19,39,0,0,0,136,0,0,0,3,0,1,21,1,59,0,0,0,83,3,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,128,2,80,129,127,7,80, 
  65,128,47,152,65,128,71,88,1,128,8,4,17,1,36,109,0,0,1,4,17,1,24,109,0,0,1,2,19,39,0,0,0,138,0,0,0,2,0,1,19,39,0,0,0,139,0,0,0,2,0,1,21,1,71,0,0,0,189,1, 
  1,0,7,0,0,0,2,0,0,0,72,120,1,128,1,112,1,128,2,112,129,128,7,112,129,128,6,112,1,128,47,248,65,128,71,184,1,128,8,4,17,1,144,109,0,0,1,4,17,1,132,109,0,0,1,4,17,1,120,109, 
  0,0,1,2,19,39,0,0,0,131,0,0,0,2,0,1,19,39,0,0,0,133,0,0,0,2,0,1,21,1,59,0,0,0,83,3,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,128,2,80,129,127,7,80, 
  65,128,47,152,65,128,71,88,1,128,8,4,17,1,216,109,0,0,1,4,17,1,204,109,0,0,1,2,19,39,0,0,0,130,0,0,0,3,0,1,19,39,0,0,0,132,0,0,0,3,0,1,21,1,47,0,0,0,93,254, 
  0,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,31,110,0,0,1,8,19,12,0,0,0,39,0,0,0,1,0,1,21,1,97,1,0,0,84,255,0, 
  0,16,0,0,0,4,0,0,0,64,40,9,128,1,144,66,130,2,144,130,128,67,232,199,130,66,136,8,130,85,72,7,128,6,144,130,128,7,144,130,128,86,168,6,128,87,8,6,128,97,56,4,128,59,104,10,128,98,104,3, 
  128,93,56,5,128,99,152,2,128,63,200,9,128,8,4,15,1,92,107,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,92,107,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17, 
  1,242,135,0,0,1,4,15,1,92,107,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,92,107,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166, 
  112,0,0,1,4,15,1,92,107,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,92,107,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,92,107,0,0,15,1,154,112,0,0,17,1,118, 
  112,0,0,1,4,15,1,129,111,0,0,15,1,80,107,0,0,17,1,57,100,0,0,1,4,15,1,129,111,0,0,15,1,80,107,0,0,17,1,45,100,0,0,1,4,15,1,129,111,0,0,15,1,80,107,0,0,17,1,235, 
  99,0,0,1,4,15,1,129,111,0,0,15,1,80,107,0,0,17,1,228,2,0,0,1,4,15,1,129,111,0,0,15,1,80,107,0,0,17,1,139,2,0,0,1,2,21,0,244,0,0,0,255,255,255,255,14,0,0,0,3, 
  0,0,0,40,96,134,130,57,248,5,128,58,144,5,128,59,40,5,130,12,152,7,129,13,48,7,128,38,200,134,129,79,184,2,128,60,192,68,128,68,88,4,128,72,240,195,128,75,136,3,128,78,32,3,128,80,80,2,128,15, 
  1,129,111,0,0,17,1,141,138,0,0,1,15,1,129,111,0,0,17,1,82,136,0,0,1,15,1,129,111,0,0,17,1,230,135,0,0,1,15,1,129,111,0,0,17,1,16,108,0,0,1,15,1,129,111,0,0,17,1,4, 
  108,0,0,1,15,1,129,111,0,0,17,1,248,107,0,0,1,15,1,129,111,0,0,17,1,154,112,0,0,1,15,1,129,111,0,0,17,1,104,107,0,0,1,15,1,129,111,0,0,17,1,129,138,0,0,1,15,1,129,111, 
  0,0,17,1,211,134,0,0,1,15,1,129,111,0,0,17,1,67,107,0,0,1,15,1,129,111,0,0,17,1,80,107,0,0,1,15,1,129,111,0,0,17,1,170,105,0,0,1,1,2,19,60,0,0,0,185,0,0,0,1, 
  0,1,19,60,0,0,0,186,0,0,0,1,0,1,19,60,0,0,0,187,0,0,0,1,0,1,19,40,0,0,0,142,0,0,0,1,0,1,21,1,47,0,0,0,180,3,1,0,5,0,0,0,2,0,0,0,6,48,1,128, 
  1,48,1,128,2,48,129,127,7,48,65,128,55,56,1,128,8,4,17,1,214,112,0,0,1,2,21,1,77,0,0,0,9,4,1,0,7,0,0,0,2,0,0,0,96,120,1,128,1,112,1,129,2,112,129,128,7,112,193,128, 
  6,112,1,128,57,248,1,128,95,184,1,128,8,4,17,1,144,129,0,0,1,4,17,1,155,124,0,0,1,4,15,1,36,113,0,0,17,1,81,0,0,0,1,2,21,1,95,0,0,0,122,10,1,0,7,0,0,0,2,0, 
  0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,129,128,10,136,2,128,51,232,65,128,55,120,1,128,8,4,15,1,65,124,0,0,17,1,255,123,0,0,1,4,15,1,231,123,0,0,15,1,243,123,0,0,17,1, 
  95,120,0,0,1,4,15,1,231,123,0,0,17,1,132,113,0,0,1,2,21,1,131,0,0,0,231,10,1,0,8,0,0,0,3,0,0,0,10,120,3,128,1,144,1,128,2,144,129,127,51,216,130,128,116,152,1,128,115,56, 
  2,128,6,144,1,128,7,144,1,128,8,4,15,1,78,119,0,0,15,1,83,120,0,0,17,1,172,118,0,0,1,4,15,1,78,119,0,0,15,1,160,118,0,0,17,1,66,116,0,0,1,4,15,1,78,119,0,0,15,1, 
  54,116,0,0,17,1,95,120,0,0,1,4,15,1,78,119,0,0,15,1,160,118,0,0,17,1,8,114,0,0,1,2,21,1,241,1,0,0,122,170,0,0,19,0,0,0,4,0,0,0,64,24,13,128,1,240,194,131,2,240, 
  130,128,67,120,203,131,66,72,76,131,85,200,8,128,6,240,130,128,7,240,194,129,70,216,10,129,73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128,93,40,6,128,87,40,7,128,63,232,13,128,97,248,4,128,98,248, 
  3,128,99,248,2,128,8,4,15,1,250,115,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,250,115,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1, 
  82,136,0,0,17,1,242,135,0,0,1,4,15,1,250,115,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,250,115,0,0,15,1,117,138, 
  0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,250,115,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,250,115,0,0,15,1,117,138,0,0, 
  15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,250,115,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,250,115,0,0,15,1,228,109,0,0,17,1,48,109,0,0,1,4, 
  15,1,250,115,0,0,15,1,228,109,0,0,17,1,220,108,0,0,1,4,15,1,250,115,0,0,15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1,250,115,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1, 
  57,100,0,0,1,4,15,1,250,115,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,45,100,0,0,1,4,15,1,250,115,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,235,99,0,0,1,4,15,1, 
  250,115,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,228,2,0,0,1,4,15,1,250,115,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,139,2,0,0,1,2,21,1,47,0,0,0,30,13,1,0, 
  5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,42,116,0,0,1,2,19,94,0,0,0,22,1,0,0,3,0,1,19,70,0,0,0,208,0,0,0,1, 
  0,1,21,1,47,0,0,0,115,13,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,114,116,0,0,1,2,21,1,241,1,0,0,122,170,0,0, 
  19,0,0,0,4,0,0,0,64,24,13,128,1,240,194,131,2,240,130,128,67,120,203,131,66,72,76,131,85,200,8,128,6,240,130,128,7,240,194,129,70,216,10,129,73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128, 
  93,40,6,128,87,40,7,128,63,232,13,128,97,248,4,128,98,248,3,128,99,248,2,128,8,4,15,1,100,118,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1, 
  100,118,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17,1,242,135,0,0,1,4,15,1,100,118,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0, 
  17,1,42,135,0,0,1,4,15,1,100,118,0,0,15,1,117,138,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,100,118,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1, 
  142,112,0,0,1,4,15,1,100,118,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,100,118,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1, 
  100,118,0,0,15,1,228,109,0,0,17,1,48,109,0,0,1,4,15,1,100,118,0,0,15,1,228,109,0,0,17,1,220,108,0,0,1,4,15,1,100,118,0,0,15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1, 
  100,118,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,57,100,0,0,1,4,15,1,100,118,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,45,100,0,0,1,4,15,1,100,118,0,0,15,1,123,100, 
  0,0,15,1,28,108,0,0,17,1,235,99,0,0,1,4,15,1,100,118,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,228,2,0,0,1,4,15,1,100,118,0,0,15,1,123,100,0,0,15,1,28,108,0,0, 
  17,1,139,2,0,0,1,2,21,1,47,0,0,0,30,13,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,148,118,0,0,1,2,19,94,0,0, 
  0,21,1,0,0,4,0,1,19,70,0,0,0,209,0,0,0,1,0,1,21,1,47,0,0,0,115,13,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4, 
  17,1,220,118,0,0,1,2,21,1,53,0,0,0,200,13,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,119,56,1,128,8,4,15,1,18,119,0,0,17,1,199,26,0,0, 
  1,2,21,1,47,0,0,0,30,13,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,66,119,0,0,1,2,19,95,0,0,0,23,1,0,0,4, 
  0,1,21,0,91,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,91,0,2,129,94,152,1,128,70,112,194,127,71,104,66,127,95,48,1,128,15,1,78,119,0,0,17,1,83,120,0,0,1,15,1,78,119,0,0,17, 
  1,160,118,0,0,1,15,1,78,119,0,0,17,1,54,116,0,0,1,1,15,1,78,119,0,0,17,1,170,119,0,0,1,2,21,1,119,0,0,0,29,14,1,0,9,0,0,0,3,0,0,0,10,72,3,128,1,176,1,128, 
  2,176,129,127,11,8,131,128,116,184,1,128,51,152,194,128,6,176,1,128,7,176,1,128,115,40,2,128,8,4,15,1,71,120,0,0,17,1,172,118,0,0,1,4,15,1,59,120,0,0,17,1,66,116,0,0,1,4,15,1, 
  47,120,0,0,17,1,95,120,0,0,1,4,17,1,34,120,0,0,1,4,15,1,59,120,0,0,17,1,8,114,0,0,1,2,19,71,0,0,0,215,0,0,0,3,0,14,1,19,70,0,0,0,211,0,0,0,2,0,1,19, 
  70,0,0,0,212,0,0,0,2,0,1,19,70,0,0,0,213,0,0,0,2,0,1,19,70,0,0,0,210,0,0,0,1,0,1,21,1,65,0,0,0,117,180,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1, 
  128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,15,1,161,120,0,0,15,1,207,123,0,0,15,1,219,123,0,0,17,1,204,2,0,0,1,2,21,0,74,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,92, 
  120,1,128,93,16,1,128,91,224,1,128,51,232,193,127,15,1,161,120,0,0,17,1,236,120,0,0,1,15,1,161,120,0,0,17,1,207,123,0,0,1,1,15,1,161,120,0,0,17,1,219,123,0,0,1,2,21,1,59,0, 
  0,0,162,14,1,0,6,0,0,0,2,0,0,0,52,152,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,81,88,1,128,8,4,17,1,135,123,0,0,1,4,17,1,40,121,0,0,1,2,21,1,47,0, 
  0,0,115,13,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,88,121,0,0,1,2,21,1,241,1,0,0,122,170,0,0,19,0,0,0,4,0, 
  0,0,64,24,13,128,1,240,194,131,2,240,130,128,67,120,203,131,66,72,76,131,85,200,8,128,6,240,130,128,7,240,194,129,70,216,10,129,73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128,93,40,6,128,87,40, 
  7,128,63,232,13,128,97,248,4,128,98,248,3,128,99,248,2,128,8,4,15,1,74,123,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,74,123,0,0,15,1, 
  117,138,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17,1,242,135,0,0,1,4,15,1,74,123,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0, 
  1,4,15,1,74,123,0,0,15,1,117,138,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,74,123,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4, 
  15,1,74,123,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,74,123,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,74,123,0,0,15,1, 
  228,109,0,0,17,1,48,109,0,0,1,4,15,1,74,123,0,0,15,1,228,109,0,0,17,1,220,108,0,0,1,4,15,1,74,123,0,0,15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1,74,123,0,0,15,1, 
  123,100,0,0,15,1,28,108,0,0,17,1,57,100,0,0,1,4,15,1,74,123,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,45,100,0,0,1,4,15,1,74,123,0,0,15,1,123,100,0,0,15,1,28,108, 
  0,0,17,1,235,99,0,0,1,4,15,1,74,123,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,228,2,0,0,1,4,15,1,74,123,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,139,2,0,0, 
  1,2,21,1,47,0,0,0,30,13,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,122,123,0,0,1,2,19,91,0,0,0,17,1,0,0,6, 
  0,14,1,21,1,59,0,0,0,117,180,0,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,15,1,195,123,0,0,15,1,219,123,0,0,17,1,204,2,0, 
  0,1,2,19,93,0,0,0,20,1,0,0,3,0,1,19,93,0,0,0,19,1,0,0,1,0,1,19,92,0,0,0,18,1,0,0,1,0,1,19,68,0,0,0,206,0,0,0,4,0,1,19,71,0,0,0,214,0,0,0, 
  1,0,1,21,1,53,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,53,124,0,0,17,1,81,0,0,0,1,2,19,69,0, 
  0,0,207,0,0,0,2,0,1,21,1,77,0,0,0,3,15,1,0,6,0,0,0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,248,1,128,51,88,1,128,8,4,15,1,143,124,0,0,15, 
  1,243,123,0,0,17,1,95,120,0,0,1,4,15,1,143,124,0,0,17,1,132,113,0,0,1,2,19,68,0,0,0,205,0,0,0,5,0,1,21,1,77,0,0,0,3,15,1,0,6,0,0,0,2,0,0,0,6,80,1, 
  129,1,80,1,128,2,80,129,127,7,80,129,128,10,248,1,128,51,88,1,128,8,4,15,1,120,129,0,0,15,1,132,129,0,0,17,1,150,126,0,0,1,4,15,1,120,129,0,0,17,1,233,124,0,0,1,2,21,1,131, 
  0,0,0,231,10,1,0,8,0,0,0,3,0,0,0,10,120,3,128,1,144,1,128,2,144,129,127,51,216,130,128,116,152,1,128,115,56,2,128,6,144,1,128,7,144,1,128,8,4,15,1,133,125,0,0,15,1,138,126,0, 
  0,17,1,172,118,0,0,1,4,15,1,133,125,0,0,15,1,121,125,0,0,17,1,66,116,0,0,1,4,15,1,133,125,0,0,15,1,109,125,0,0,17,1,150,126,0,0,1,4,15,1,133,125,0,0,15,1,121,125,0, 
  0,17,1,8,114,0,0,1,2,19,73,0,0,0,217,0,0,0,1,0,1,19,73,0,0,0,219,0,0,0,1,0,1,21,0,91,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,96,48,1,128,73,112,2,128,74, 
  104,130,128,95,152,1,128,94,0,2,128,15,1,133,125,0,0,17,1,109,125,0,0,1,15,1,133,125,0,0,17,1,138,126,0,0,1,15,1,133,125,0,0,17,1,121,125,0,0,1,1,15,1,133,125,0,0,17,1,225, 
  125,0,0,1,2,21,1,119,0,0,0,29,14,1,0,9,0,0,0,3,0,0,0,10,72,3,128,1,176,1,128,2,176,129,127,11,8,131,128,116,184,1,128,51,152,194,128,6,176,1,128,7,176,1,128,115,40,2,128,8, 
  4,15,1,126,126,0,0,17,1,172,118,0,0,1,4,15,1,114,126,0,0,17,1,66,116,0,0,1,4,15,1,102,126,0,0,17,1,150,126,0,0,1,4,17,1,89,126,0,0,1,4,15,1,114,126,0,0,17,1,8, 
  114,0,0,1,2,19,74,0,0,0,224,0,0,0,3,0,14,1,19,73,0,0,0,220,0,0,0,2,0,1,19,73,0,0,0,222,0,0,0,2,0,1,19,73,0,0,0,221,0,0,0,2,0,1,19,73,0,0,0,218, 
  0,0,0,1,0,1,21,1,83,0,0,0,100,15,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,51,40,2,128,57,88,1,128,8,4,15,1,234,126,0,0,15,1,76,94, 
  0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,234,126,0,0,17,1,140,3,0,0,1,2,21,1,47,0,0,0,213,15,1,0,5,0,0,0,2,0,0,0,52,56,1,128,1,48,1,128,2,48,129,128, 
  7,48,1,128,6,48,1,128,8,4,17,1,26,127,0,0,1,2,21,1,47,0,0,0,115,13,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1, 
  74,127,0,0,1,2,21,1,241,1,0,0,122,170,0,0,19,0,0,0,4,0,0,0,64,24,13,128,1,240,194,131,2,240,130,128,67,120,203,131,66,72,76,131,85,200,8,128,6,240,130,128,7,240,194,129,70,216,10,129, 
  73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128,93,40,6,128,87,40,7,128,63,232,13,128,97,248,4,128,98,248,3,128,99,248,2,128,8,4,15,1,60,129,0,0,15,1,117,138,0,0,15,1,129,138,0,0, 
  15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,60,129,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17,1,242,135,0,0,1,4,15,1,60,129,0,0,15,1,117,138,0,0,15,1, 
  129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,60,129,0,0,15,1,117,138,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,60,129, 
  0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,60,129,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,60,129,0,0,15,1,117,138,0,0, 
  15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,60,129,0,0,15,1,228,109,0,0,17,1,48,109,0,0,1,4,15,1,60,129,0,0,15,1,228,109,0,0,17,1,220,108,0,0,1,4,15,1,60,129,0,0, 
  15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1,60,129,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,57,100,0,0,1,4,15,1,60,129,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1, 
  45,100,0,0,1,4,15,1,60,129,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,235,99,0,0,1,4,15,1,60,129,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,228,2,0,0,1,4,15,1, 
  60,129,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,139,2,0,0,1,2,21,1,47,0,0,0,30,13,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56, 
  1,128,8,4,17,1,108,129,0,0,1,2,19,96,0,0,0,24,1,0,0,6,0,1,19,72,0,0,0,216,0,0,0,4,0,1,19,74,0,0,0,223,0,0,0,1,0,1,21,1,77,0,0,0,3,15,1,0,6,0, 
  0,0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,248,1,128,51,88,1,128,8,4,15,1,187,134,0,0,15,1,199,134,0,0,17,1,139,131,0,0,1,4,15,1,187,134,0,0,17,1, 
  222,129,0,0,1,2,21,1,131,0,0,0,231,10,1,0,8,0,0,0,3,0,0,0,10,120,3,128,1,144,1,128,2,144,129,127,51,216,130,128,116,152,1,128,115,56,2,128,6,144,1,128,7,144,1,128,8,4,15,1, 
  122,130,0,0,15,1,127,131,0,0,17,1,172,118,0,0,1,4,15,1,122,130,0,0,15,1,110,130,0,0,17,1,66,116,0,0,1,4,15,1,122,130,0,0,15,1,98,130,0,0,17,1,139,131,0,0,1,4,15,1, 
  122,130,0,0,15,1,110,130,0,0,17,1,8,114,0,0,1,2,19,76,0,0,0,226,0,0,0,1,0,1,19,76,0,0,0,228,0,0,0,1,0,1,21,0,91,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  76,112,2,128,77,104,194,128,94,0,2,128,95,152,1,128,97,48,1,128,15,1,122,130,0,0,17,1,98,130,0,0,1,15,1,122,130,0,0,17,1,127,131,0,0,1,15,1,122,130,0,0,17,1,110,130,0,0,1,1, 
  15,1,122,130,0,0,17,1,214,130,0,0,1,2,21,1,119,0,0,0,29,14,1,0,9,0,0,0,3,0,0,0,10,72,3,128,1,176,1,128,2,176,129,127,11,8,131,128,116,184,1,128,51,152,194,128,6,176,1,128, 
  7,176,1,128,115,40,2,128,8,4,15,1,115,131,0,0,17,1,172,118,0,0,1,4,15,1,103,131,0,0,17,1,66,116,0,0,1,4,15,1,91,131,0,0,17,1,139,131,0,0,1,4,17,1,78,131,0,0,1,4, 
  15,1,103,131,0,0,17,1,8,114,0,0,1,2,19,77,0,0,0,233,0,0,0,3,0,14,1,19,76,0,0,0,229,0,0,0,2,0,1,19,76,0,0,0,231,0,0,0,2,0,1,19,76,0,0,0,230,0,0,0, 
  2,0,1,19,76,0,0,0,227,0,0,0,1,0,1,21,1,161,0,0,0,42,16,1,0,9,0,0,0,3,0,0,0,88,152,4,130,1,176,1,128,2,176,1,128,119,136,2,128,100,248,3,128,101,88,3,128,6,176,1, 
  128,7,176,1,127,120,184,1,128,8,4,15,1,45,132,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,45,132,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0, 
  0,1,4,15,1,45,132,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,45,132,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,45,132,0,0,17,1,173,23,0,0,1,2,21,1,47, 
  0,0,0,213,15,1,0,5,0,0,0,2,0,0,0,52,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,93,132,0,0,1,2,21,1,47,0,0,0,115,13,1,0,5,0,0,0,2, 
  0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,141,132,0,0,1,2,21,1,241,1,0,0,122,170,0,0,19,0,0,0,4,0,0,0,64,24,13,128,1,240,194,131,2, 
  240,130,128,67,120,203,131,66,72,76,131,85,200,8,128,6,240,130,128,7,240,194,129,70,216,10,129,73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128,93,40,6,128,87,40,7,128,63,232,13,128,97,248,4,128,98, 
  248,3,128,99,248,2,128,8,4,15,1,127,134,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,127,134,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15, 
  1,82,136,0,0,17,1,242,135,0,0,1,4,15,1,127,134,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,127,134,0,0,15,1,117, 
  138,0,0,15,1,211,134,0,0,15,1,223,134,0,0,17,1,166,112,0,0,1,4,15,1,127,134,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,127,134,0,0,15,1,117,138,0, 
  0,15,1,154,112,0,0,17,1,130,112,0,0,1,4,15,1,127,134,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,127,134,0,0,15,1,228,109,0,0,17,1,48,109,0,0,1, 
  4,15,1,127,134,0,0,15,1,228,109,0,0,17,1,220,108,0,0,1,4,15,1,127,134,0,0,15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1,127,134,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17, 
  1,57,100,0,0,1,4,15,1,127,134,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,45,100,0,0,1,4,15,1,127,134,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,235,99,0,0,1,4,15, 
  1,127,134,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,228,2,0,0,1,4,15,1,127,134,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,139,2,0,0,1,2,21,1,47,0,0,0,30,13,1, 
  0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,175,134,0,0,1,2,19,97,0,0,0,25,1,0,0,6,0,1,19,75,0,0,0,225,0,0,0, 
  4,0,1,19,77,0,0,0,232,0,0,0,1,0,1,19,40,0,0,0,140,0,0,0,1,0,1,21,0,74,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,68,224,129,128,57,72,2,128,72,120,1,128,75,16,1, 
  128,15,1,223,134,0,0,17,1,16,108,0,0,1,15,1,223,134,0,0,17,1,4,108,0,0,1,15,1,223,134,0,0,17,1,248,107,0,0,1,1,2,21,1,83,0,0,0,100,15,1,0,6,0,0,0,2,0,0,0, 
  6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,51,40,2,128,57,88,1,128,8,4,15,1,126,135,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,126,135,0,0,17,1, 
  140,3,0,0,1,2,19,78,0,0,0,234,0,0,0,2,0,1,21,0,91,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,80,48,1,128,78,0,2,128,58,208,194,127,59,104,66,128,79,152,1,128,15,1,138,135, 
  0,0,17,1,141,138,0,0,1,15,1,138,135,0,0,17,1,82,136,0,0,1,15,1,138,135,0,0,17,1,230,135,0,0,1,15,1,138,135,0,0,17,1,104,107,0,0,1,1,2,19,59,0,0,0,183,0,0,0,1, 
  0,1,21,1,83,0,0,0,100,15,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,51,40,2,128,57,88,1,128,8,4,15,1,70,136,0,0,15,1,76,94,0,0,15,1, 
  151,94,0,0,17,1,81,0,0,0,1,4,15,1,70,136,0,0,17,1,140,3,0,0,1,2,19,79,0,0,0,235,0,0,0,2,0,1,19,58,0,0,0,181,0,0,0,1,0,1,21,1,47,0,0,0,115,13,1,0, 
  5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,142,136,0,0,1,2,21,1,89,0,0,0,100,15,1,0,6,0,0,0,2,0,0,0,6,80,1,128, 
  1,80,1,129,2,80,129,127,7,80,65,128,51,248,1,128,57,88,1,128,8,4,15,1,0,137,0,0,15,1,54,138,0,0,17,1,81,0,0,0,1,4,15,1,0,137,0,0,15,1,232,136,0,0,15,1,244,136,0,0, 
  17,1,140,3,0,0,1,2,19,82,0,0,0,238,0,0,0,1,0,1,19,81,0,0,0,237,0,0,0,1,0,1,21,0,125,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,80,64,2,128,37,24,195,128,82,112, 
  1,128,11,128,3,128,49,176,66,128,65,72,66,128,81,216,1,128,15,1,0,137,0,0,17,1,177,137,0,0,1,15,1,0,137,0,0,17,1,232,136,0,0,1,1,15,1,0,137,0,0,17,1,108,7,0,0,1,15,1, 
  0,137,0,0,17,1,244,136,0,0,1,15,1,0,137,0,0,17,1,126,137,0,0,1,15,1,0,137,0,0,17,1,96,7,0,0,1,2,21,1,39,0,0,0,225,16,1,0,3,0,0,0,1,0,0,0,2,240,0,128, 
  1,240,64,128,91,248,0,128,8,4,17,1,86,8,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,107,0,0,0,94,17,1,0,7,0,0,0,2,0,0,0,6,112,1,128,1,112,65,129,2,112,129,127,7, 
  112,65,128,11,24,67,128,51,120,2,128,57,120,1,128,8,4,15,1,42,138,0,0,15,1,244,136,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,42,138,0,0,15,1,244,136,0, 
  0,17,1,140,3,0,0,1,4,17,1,29,138,0,0,1,2,19,80,0,0,0,236,0,0,0,4,0,14,1,19,82,0,0,0,239,0,0,0,2,0,1,21,1,39,0,0,0,225,16,1,0,3,0,0,0,1,0,0,0, 
  2,48,1,128,1,48,65,128,91,240,0,128,4,17,1,86,8,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,19,0,0,0,0,0,0,0,0,3,0,1,19,12,0,0,0,42,0,0,0,1,0,1,19,40,0, 
  0,0,141,0,0,0,1,0,1,19,58,0,0,0,182,0,0,0,1,0,1,21,1,241,1,0,0,122,170,0,0,19,0,0,0,4,0,0,0,64,24,13,128,1,240,194,131,2,240,130,128,67,120,203,131,66,72,76,131,85, 
  200,8,128,6,240,130,128,7,240,194,129,70,216,10,129,73,56,10,128,74,152,9,128,59,184,14,128,86,248,7,128,93,40,6,128,87,40,7,128,63,232,13,128,97,248,4,128,98,248,3,128,99,248,2,128,8,4,15,1,139, 
  140,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,141,138,0,0,17,1,94,136,0,0,1,4,15,1,139,140,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,82,136,0,0,17,1,242,135,0,0,1, 
  4,15,1,139,140,0,0,15,1,117,138,0,0,15,1,129,138,0,0,15,1,138,135,0,0,15,1,230,135,0,0,17,1,42,135,0,0,1,4,15,1,139,140,0,0,15,1,117,138,0,0,15,1,211,134,0,0,15,1,223, 
  134,0,0,17,1,166,112,0,0,1,4,15,1,139,140,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,142,112,0,0,1,4,15,1,139,140,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,130,112,0, 
  0,1,4,15,1,139,140,0,0,15,1,117,138,0,0,15,1,154,112,0,0,17,1,118,112,0,0,1,4,15,1,139,140,0,0,15,1,228,109,0,0,17,1,48,109,0,0,1,4,15,1,139,140,0,0,15,1,228,109,0, 
  0,17,1,220,108,0,0,1,4,15,1,139,140,0,0,15,1,228,109,0,0,17,1,40,108,0,0,1,4,15,1,139,140,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,57,100,0,0,1,4,15,1,139,140,0, 
  0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,45,100,0,0,1,4,15,1,139,140,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,235,99,0,0,1,4,15,1,139,140,0,0,15,1,123,100,0,0,15, 
  1,28,108,0,0,17,1,228,2,0,0,1,4,15,1,139,140,0,0,15,1,123,100,0,0,15,1,28,108,0,0,17,1,139,2,0,0,1,2,19,0,0,0,0,1,0,0,0,3,0,1,19,11,0,0,0,35,0,0,0, 
  1,0,1,13,15,1,163,140,0,0,17,1,177,140,0,0,1,21,7,48,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,104,142,0,0,15,1,144,143,0,0,15,1,32,142,0,0,17,1, 
  138,141,0,0,1,21,9,168,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,160,3,128,2,48,129,128,3,208,2,128,6,112,4,128,4,15,1,104,142,0,0,15,1,144,143,0,0,15,1,156,143, 
  0,0,17,1,92,142,0,0,1,4,15,1,104,142,0,0,15,1,144,143,0,0,15,1,156,143,0,0,17,1,80,142,0,0,1,4,15,1,104,142,0,0,15,1,144,143,0,0,15,1,156,143,0,0,17,1,68,142,0,0, 
  1,4,15,1,104,142,0,0,15,1,144,143,0,0,15,1,156,143,0,0,17,1,56,142,0,0,1,4,15,1,104,142,0,0,15,1,144,143,0,0,15,1,156,143,0,0,17,1,44,142,0,0,1,2,21,9,78,0,0,0, 
  255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,240,1,128,2,112,129,128,3,48,2,128,6,48,1,128,4,17,1,20,142,0,0,1,4,17,1,8,142,0,0,1,4,17,1,252,141,0,0,1,4,17,1,240, 
  141,0,0,1,4,17,1,228,141,0,0,1,19,16,0,0,0,57,0,0,0,1,0,1,19,16,0,0,0,53,0,0,0,2,0,1,19,16,0,0,0,55,0,0,0,2,0,1,19,16,0,0,0,52,0,0,0,2,0,1, 
  19,16,0,0,0,54,0,0,0,2,0,1,19,16,0,0,0,56,0,0,0,2,0,1,19,2,0,0,0,4,0,0,0,1,0,1,19,15,0,0,0,51,0,0,0,1,0,1,19,15,0,0,0,50,0,0,0,1,0,1, 
  19,15,0,0,0,48,0,0,0,1,0,1,19,15,0,0,0,47,0,0,0,1,0,1,19,15,0,0,0,49,0,0,0,1,0,1,21,0,91,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,16,48,1,128,1,208, 
  2,128,2,104,2,128,3,0,66,128,15,152,1,128,15,1,104,142,0,0,17,1,32,142,0,0,1,15,1,104,142,0,0,17,1,156,143,0,0,1,15,1,104,142,0,0,17,1,196,142,0,0,1,15,1,104,142,0,0,17, 
  1,144,143,0,0,1,1,2,21,7,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,132,143,0,0,15,1,32,142,0,0,17,1,138,141,0,0,1,21,9,138,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,4,208,1,128,5,16,3,128,2,48,129,128,3,112,2,128,6,176,3,128,4,15,1,132,143,0,0,15,1,156,143,0,0,17,1,92,142,0,0,1,4,15,1,132,143,0,0,15,1,156,143,0, 
  0,17,1,80,142,0,0,1,4,15,1,132,143,0,0,15,1,156,143,0,0,17,1,68,142,0,0,1,4,15,1,132,143,0,0,15,1,156,143,0,0,17,1,56,142,0,0,1,4,15,1,132,143,0,0,15,1,156,143,0, 
  0,17,1,44,142,0,0,1,19,1,0,0,0,2,0,0,0,1,0,1,19,3,0,0,0,6,0,0,0,2,0,1,19,3,0,0,0,5,0,0,0,1,0,1,19,2,0,0,0,3,0,0,0,1,0,1,15,1,163,140, 
  0,0,17,1,181,143,0,0,1,21,1,31,1,0,0,219,17,1,0,13,0,0,0,3,0,0,0,24,24,7,131,1,48,194,129,2,48,130,128,23,184,7,130,26,120,6,128,21,88,8,129,6,48,2,129,7,48,2,127,57, 
  120,5,128,77,168,4,128,78,216,3,128,79,8,3,128,80,56,2,128,8,4,15,1,47,160,0,0,15,1,182,164,0,0,15,1,194,164,0,0,17,1,221,157,0,0,1,4,15,1,47,160,0,0,15,1,182,164,0,0,15, 
  1,209,157,0,0,17,1,143,157,0,0,1,4,15,1,47,160,0,0,15,1,182,164,0,0,15,1,131,157,0,0,17,1,166,155,0,0,1,4,15,1,47,160,0,0,15,1,182,164,0,0,15,1,154,155,0,0,17,1,123, 
  154,0,0,1,4,15,1,47,160,0,0,15,1,111,154,0,0,15,1,93,0,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,4,15,1,47,160,0,0,15,1,99,154,0,0,17,1,47,151,0,0,1,4,15,1,47, 
  160,0,0,15,1,35,151,0,0,17,1,1,149,0,0,1,4,15,1,47,160,0,0,15,1,245,148,0,0,17,1,235,146,0,0,1,4,15,1,47,160,0,0,15,1,223,146,0,0,17,1,213,144,0,0,1,2,21,1,59, 
  0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,17,145,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,2,21,1,47, 
  0,0,0,114,25,1,0,5,0,0,0,2,0,0,0,20,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,65,145,0,0,1,2,21,1,145,1,0,0,167,183,0,0,12,0,0,0,3, 
  0,0,0,88,184,200,130,1,16,194,129,2,16,2,128,51,232,10,128,44,184,75,129,101,152,5,128,6,16,2,128,7,16,194,128,57,24,10,128,100,40,7,128,119,216,3,128,120,24,2,128,8,4,15,1,211,146,0,0,15, 
  1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,211,146,0,0,15,1,39,27,0, 
  0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,211,146,0,0,15,1,39,27,0,0,15,1,84, 
  88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,211,146,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15, 
  1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,211,146,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0, 
  0,17,1,173,23,0,0,1,4,15,1,211,146,0,0,15,1,39,27,0,0,15,1,122,23,0,0,17,1,81,0,0,0,1,4,15,1,211,146,0,0,15,1,39,27,0,0,15,1,142,22,0,0,17,1,140,3,0,0,1, 
  4,15,1,211,146,0,0,15,1,39,27,0,0,15,1,130,22,0,0,17,1,30,5,0,0,1,2,19,18,0,0,0,62,0,0,0,4,0,1,19,6,0,0,0,11,0,0,0,1,0,1,21,1,59,0,0,0,124,167,0, 
  0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,39,147,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,2,21,1,47,0,0,0,114,25,1, 
  0,5,0,0,0,2,0,0,0,20,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,87,147,0,0,1,2,21,1,145,1,0,0,167,183,0,0,12,0,0,0,3,0,0,0,88,184,200, 
  130,1,16,194,129,2,16,2,128,51,232,10,128,44,184,75,129,101,152,5,128,6,16,2,128,7,16,194,128,57,24,10,128,100,40,7,128,119,216,3,128,120,24,2,128,8,4,15,1,233,148,0,0,15,1,39,27,0,0,15, 
  1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,233,148,0,0,15,1,39,27,0,0,15,1,84,88,0, 
  0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,233,148,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96, 
  88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,233,148,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15, 
  1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,233,148,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0, 
  0,1,4,15,1,233,148,0,0,15,1,39,27,0,0,15,1,122,23,0,0,17,1,81,0,0,0,1,4,15,1,233,148,0,0,15,1,39,27,0,0,15,1,142,22,0,0,17,1,140,3,0,0,1,4,15,1,233,148,0, 
  0,15,1,39,27,0,0,15,1,130,22,0,0,17,1,30,5,0,0,1,2,19,19,0,0,0,63,0,0,0,4,0,1,19,6,0,0,0,12,0,0,0,1,0,1,21,1,83,0,0,0,100,15,1,0,6,0,0,0,2, 
  0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,51,40,2,128,57,88,1,128,8,4,15,1,85,149,0,0,15,1,76,94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,85,149,0, 
  0,17,1,140,3,0,0,1,2,21,1,47,0,0,0,114,25,1,0,5,0,0,0,2,0,0,0,20,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,133,149,0,0,1,2,21,1,145, 
  1,0,0,167,183,0,0,12,0,0,0,3,0,0,0,88,184,200,130,1,16,194,129,2,16,2,128,51,232,10,128,44,184,75,129,101,152,5,128,6,16,2,128,7,16,194,128,57,24,10,128,100,40,7,128,119,216,3,128,120, 
  24,2,128,8,4,15,1,23,151,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1, 
  4,15,1,23,151,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,23, 
  151,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,23,151,0,0,15,1,39,27,0,0,15, 
  1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,23,151,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0, 
  0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,23,151,0,0,15,1,39,27,0,0,15,1,122,23,0,0,17,1,81,0,0,0,1,4,15,1,23,151,0,0,15,1,39,27,0,0,15, 
  1,142,22,0,0,17,1,140,3,0,0,1,4,15,1,23,151,0,0,15,1,39,27,0,0,15,1,130,22,0,0,17,1,30,5,0,0,1,2,19,20,0,0,0,64,0,0,0,4,0,1,19,6,0,0,0,13,0,0,0, 
  1,0,1,21,1,65,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,113,151,0,0,15,1,75,154,0,0,15,1,87,154,0, 
  0,17,1,81,0,0,0,1,2,21,0,74,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,37,16,1,128,21,72,194,127,22,224,1,128,23,120,1,128,15,1,113,151,0,0,17,1,87,154,0,0,1,15,1,113,151, 
  0,0,17,1,188,151,0,0,1,15,1,113,151,0,0,17,1,75,154,0,0,1,1,2,21,1,59,0,0,0,199,25,1,0,6,0,0,0,2,0,0,0,20,88,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80, 
  1,128,9,152,1,128,8,4,17,1,64,152,0,0,1,4,17,1,248,151,0,0,1,2,21,1,59,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56, 
  1,128,8,4,15,1,52,152,0,0,15,1,87,154,0,0,17,1,81,0,0,0,1,2,19,23,0,0,0,68,0,0,0,3,0,1,21,1,59,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,124,152,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,2,21,1,47,0,0,0,114,25,1,0,5,0,0,0,2,0,0,0,20,56,1,128,1,48, 
  1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,172,152,0,0,1,2,21,1,145,1,0,0,167,183,0,0,12,0,0,0,3,0,0,0,88,184,200,130,1,16,194,129,2,16,2,128,51,232,10,128,44,184, 
  75,129,101,152,5,128,6,16,2,128,7,16,194,128,57,24,10,128,100,40,7,128,119,216,3,128,120,24,2,128,8,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89, 
  0,0,15,1,29,91,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1, 
  29,91,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0, 
  15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,15,1,211,25,0,0,17,1,123,25, 
  0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,84,88,0,0,15,1,96,88,0,0,15,1,242,89,0,0,15,1,29,91,0,0,17,1,173,23,0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0, 
  15,1,122,23,0,0,17,1,81,0,0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,142,22,0,0,17,1,140,3,0,0,1,4,15,1,62,154,0,0,15,1,39,27,0,0,15,1,130,22,0,0,17,1, 
  30,5,0,0,1,2,19,21,0,0,0,65,0,0,0,6,0,14,1,19,23,0,0,0,67,0,0,0,1,0,1,19,22,0,0,0,66,0,0,0,1,0,1,19,6,0,0,0,15,0,0,0,1,0,1,19,6,0,0,0, 
  14,0,0,0,1,0,1,21,1,83,0,0,0,100,15,1,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,51,40,2,128,57,88,1,128,8,4,15,1,207,154,0,0,15,1,76, 
  94,0,0,15,1,151,94,0,0,17,1,81,0,0,0,1,4,15,1,207,154,0,0,17,1,140,3,0,0,1,2,21,1,59,0,0,0,40,26,1,0,6,0,0,0,2,0,0,0,76,80,1,128,1,144,1,128,2,144,129, 
  128,7,144,129,128,6,144,1,128,75,152,1,128,4,17,1,88,155,0,0,1,8,4,17,1,22,155,0,0,1,19,41,0,0,0,145,0,0,0,2,0,1,21,1,53,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0, 
  6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,76,155,0,0,17,1,81,0,0,0,1,2,19,41,0,0,0,143,0,0,0,4,0,1,21,1,53,0,0,0,124,167,0,0,5,0, 
  0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,142,155,0,0,17,1,81,0,0,0,1,2,19,41,0,0,0,144,0,0,0,4,0,1,19,17,0,0,0,58, 
  0,0,0,1,0,1,21,1,89,0,0,0,237,26,1,0,7,0,0,0,2,0,0,0,16,88,2,128,1,112,1,129,2,112,129,128,7,112,1,128,6,112,129,128,17,232,1,128,18,120,1,128,8,4,15,1,36,156,0,0, 
  17,1,24,156,0,0,1,4,15,1,36,156,0,0,17,1,12,156,0,0,1,4,15,1,36,156,0,0,17,1,0,156,0,0,1,2,19,43,0,0,0,148,0,0,0,1,0,1,19,43,0,0,0,150,0,0,0,1,0,1, 
  19,43,0,0,0,149,0,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,56,1,128,43,208,0,128,15,1,36,156,0,0,17,1,77,156,0,0,1,1,2,21,1,79,0,0,0,156, 
  27,1,0,7,0,0,0,2,0,0,0,16,248,1,128,1,56,2,129,2,112,129,128,7,112,1,128,6,112,129,128,17,184,1,128,18,120,1,128,8,4,17,1,119,157,0,0,1,4,17,1,107,157,0,0,1,4,17,1,95, 
  157,0,0,1,4,17,1,157,156,0,0,1,2,21,1,59,0,0,0,9,28,1,0,6,0,0,0,2,0,0,0,76,88,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,75,152,1,128,8,4,17,1,28, 
  157,0,0,1,4,17,1,217,156,0,0,1,2,21,1,53,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,15,157,0,0,17, 
  1,81,0,0,0,1,2,19,42,0,0,0,146,0,0,0,5,0,14,1,21,1,53,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4, 
  15,1,82,157,0,0,17,1,81,0,0,0,1,2,19,42,0,0,0,147,0,0,0,5,0,14,1,19,43,0,0,0,151,0,0,0,2,0,1,19,43,0,0,0,153,0,0,0,2,0,1,19,43,0,0,0,152,0,0,0, 
  2,0,1,19,17,0,0,0,59,0,0,0,1,0,1,21,1,53,0,0,0,124,167,0,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,57,56,1,128,8,4,15,1,197,157,0, 
  0,17,1,81,0,0,0,1,2,19,44,0,0,0,154,0,0,0,2,0,1,19,17,0,0,0,60,0,0,0,1,0,1,21,1,47,0,0,0,115,13,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2, 
  48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,13,158,0,0,1,2,21,1,191,0,0,0,42,16,1,0,9,0,0,0,3,0,0,0,88,88,5,130,1,176,1,128,2,176,1,128,119,184,2,128,100,136,4,128,101, 
  184,3,128,6,176,1,128,7,176,1,127,120,184,1,128,8,4,15,1,205,158,0,0,15,1,35,160,0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,205,158,0,0,15,1,35,160,0, 
  0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,205,158,0,0,15,1,35,160,0,0,15,1,187,26,0,0,17,1,223,25,0,0,1,4,15,1,205,158,0,0,15,1,35,160,0,0,15, 
  1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,205,158,0,0,15,1,35,160,0,0,17,1,173,23,0,0,1,2,21,0,142,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,61,152,67,128,85,96,2,128,98, 
  248,1,128,83,48,3,129,84,200,2,128,45,104,196,126,46,0,4,128,99,144,1,128,15,1,205,158,0,0,17,1,53,91,0,0,1,15,1,205,158,0,0,17,1,211,26,0,0,1,15,1,205,158,0,0,17,1,187,26,0, 
  0,1,15,1,205,158,0,0,17,1,211,25,0,0,1,15,1,205,158,0,0,17,1,41,91,0,0,1,15,1,205,158,0,0,17,1,35,160,0,0,1,15,1,205,158,0,0,17,1,92,159,0,0,1,1,2,21,1,173,0, 
  0,0,180,28,1,0,10,0,0,0,3,0,0,0,88,184,68,130,1,208,1,128,2,208,1,128,11,40,5,128,100,24,4,128,101,120,3,128,6,208,1,128,7,208,65,128,119,168,2,128,120,216,1,128,8,4,15,1,23,160, 
  0,0,15,1,41,91,0,0,15,1,53,91,0,0,17,1,223,26,0,0,1,4,15,1,23,160,0,0,15,1,41,91,0,0,15,1,211,26,0,0,17,1,199,26,0,0,1,4,15,1,23,160,0,0,15,1,187,26,0,0, 
  17,1,223,25,0,0,1,4,15,1,23,160,0,0,15,1,211,25,0,0,17,1,123,25,0,0,1,4,15,1,23,160,0,0,17,1,173,23,0,0,1,4,17,1,10,160,0,0,1,2,19,45,0,0,0,155,0,0,0,4, 
  0,14,1,19,46,0,0,0,157,0,0,0,2,0,1,19,46,0,0,0,156,0,0,0,1,0,1,21,0,5,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,0,192,7,128,17,24,134,130,18,176,133,130,11,128,6, 
  129,4,184,7,129,5,80,7,129,6,232,6,128,19,72,5,128,20,224,68,129,21,120,68,128,37,16,4,129,41,168,3,128,42,64,3,128,44,216,2,128,45,112,2,128,15,1,47,160,0,0,17,1,194,164,0,0,1,15,1, 
  47,160,0,0,17,1,209,157,0,0,1,15,1,47,160,0,0,17,1,131,157,0,0,1,15,1,47,160,0,0,17,1,154,155,0,0,1,15,1,47,160,0,0,17,1,151,140,0,0,1,15,1,47,160,0,0,17,1,99,154, 
  0,0,1,15,1,47,160,0,0,17,1,35,151,0,0,1,15,1,47,160,0,0,17,1,245,148,0,0,1,15,1,47,160,0,0,17,1,223,146,0,0,1,15,1,47,160,0,0,17,1,182,164,0,0,1,15,1,47,160,0, 
  0,17,1,93,0,0,0,1,15,1,47,160,0,0,17,1,33,164,0,0,1,15,1,47,160,0,0,17,1,53,161,0,0,1,1,15,1,47,160,0,0,17,1,111,154,0,0,1,2,21,1,7,1,0,0,219,17,1,0,13, 
  0,0,0,3,0,0,0,24,88,6,131,1,48,194,129,2,48,130,128,23,248,6,130,26,184,5,128,21,152,7,129,6,48,2,129,7,48,2,127,57,184,4,128,77,24,4,128,78,120,3,128,79,216,2,128,80,56,2,128,8, 
  4,15,1,21,164,0,0,15,1,194,164,0,0,17,1,221,157,0,0,1,4,15,1,21,164,0,0,15,1,209,157,0,0,17,1,143,157,0,0,1,4,15,1,21,164,0,0,15,1,131,157,0,0,17,1,166,155,0,0,1, 
  4,15,1,21,164,0,0,15,1,154,155,0,0,17,1,123,154,0,0,1,4,15,1,61,162,0,0,15,1,111,154,0,0,15,1,93,0,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,4,15,1,61,162,0,0,15, 
  1,99,154,0,0,17,1,47,151,0,0,1,4,15,1,61,162,0,0,15,1,35,151,0,0,17,1,1,149,0,0,1,4,15,1,61,162,0,0,15,1,245,148,0,0,17,1,235,146,0,0,1,4,15,1,61,162,0,0,15, 
  1,223,146,0,0,17,1,213,144,0,0,1,2,21,0,248,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,0,16,4,128,17,80,133,130,18,112,130,130,11,240,6,129,4,120,4,129,5,120,4,129,6,168,3,128,19, 
  32,6,128,20,88,71,129,21,232,68,128,37,64,3,129,41,136,6,128,42,216,2,128,44,128,4,128,45,184,5,128,15,1,61,162,0,0,17,1,223,146,0,0,1,15,1,61,162,0,0,17,1,131,157,0,0,1,15,1,61, 
  162,0,0,17,1,151,140,0,0,1,15,1,61,162,0,0,17,1,67,163,0,0,1,15,1,61,162,0,0,17,1,111,154,0,0,1,1,15,1,61,162,0,0,17,1,209,157,0,0,1,15,1,61,162,0,0,17,1,99,154, 
  0,0,1,15,1,61,162,0,0,17,1,54,163,0,0,1,15,1,61,162,0,0,17,1,194,164,0,0,1,15,1,61,162,0,0,17,1,245,148,0,0,1,15,1,61,162,0,0,17,1,154,155,0,0,1,15,1,61,162,0, 
  0,17,1,93,0,0,0,1,15,1,61,162,0,0,17,1,35,151,0,0,1,2,19,5,0,0,0,10,0,0,0,2,0,14,1,21,1,137,0,0,0,57,29,1,0,9,0,0,0,3,0,0,0,24,248,2,128,1,176,193, 
  129,2,176,129,128,23,104,3,128,26,136,2,128,21,216,3,128,6,176,1,128,7,176,1,127,57,184,1,128,8,4,15,1,9,164,0,0,15,1,93,0,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,4,15,1,253, 
  163,0,0,17,1,47,151,0,0,1,4,15,1,241,163,0,0,17,1,1,149,0,0,1,4,15,1,229,163,0,0,17,1,235,146,0,0,1,4,15,1,217,163,0,0,17,1,213,144,0,0,1,19,4,0,0,0,7,0,0, 
  0,2,0,14,1,19,6,0,0,0,16,0,0,0,2,0,1,19,6,0,0,0,17,0,0,0,2,0,1,19,6,0,0,0,18,0,0,0,2,0,1,19,6,0,0,0,20,0,0,0,2,0,1,19,6,0,0,0,19,0, 
  0,0,2,0,1,19,5,0,0,0,10,0,0,0,2,0,1,21,1,137,0,0,0,57,29,1,0,9,0,0,0,3,0,0,0,24,248,2,128,1,176,193,129,2,176,129,128,23,104,3,128,26,136,2,128,21,216,3,128,6, 
  176,1,128,7,176,1,127,57,184,1,128,8,4,15,1,9,164,0,0,15,1,93,0,0,0,15,1,151,140,0,0,17,1,81,0,0,0,1,4,15,1,253,163,0,0,17,1,47,151,0,0,1,4,15,1,241,163,0,0,17, 
  1,1,149,0,0,1,4,15,1,229,163,0,0,17,1,235,146,0,0,1,4,15,1,217,163,0,0,17,1,213,144,0,0,1,19,4,0,0,0,8,0,0,0,1,0,1,19,5,0,0,0,9,0,0,0,1,0,1,19,17, 
  0,0,0,61,0,0,0,1,0,1,15,1,163,140,0,0,17,1,219,164,0,0,1,21,1,54,0,0,0,194,29,1,0,2,0,0,0,1,0,0,0,28,208,0,128,27,64,1,128,4,15,1,85,165,0,0,17,1,73,165, 
  0,0,1,4,15,1,30,165,0,0,17,1,18,165,0,0,1,2,19,24,0,0,0,69,0,0,0,1,0,1,21,1,30,0,0,0,24,33,1,0,1,0,0,0,0,0,0,0,8,176,0,128,4,17,1,61,165,0,0,1, 
  2,19,7,0,0,0,21,0,0,0,2,0,1,19,25,0,0,0,70,0,0,0,1,0,1,21,1,30,0,0,0,24,33,1,0,1,0,0,0,0,0,0,0,8,176,0,128,4,17,1,116,165,0,0,1,2,19,7,0,0, 
  0,22,0,0,0,2,0,1,15,1,163,140,0,0,17,1,141,165,0,0,1,21,1,167,1,0,0,77,233,0,0,23,0,0,0,4,0,0,0,32,232,203,132,1,112,195,131,2,112,195,131,35,152,10,128,36,40,10,128,37, 
  184,201,131,38,72,9,128,39,216,8,128,40,104,8,128,41,248,7,128,42,136,7,128,43,24,7,128,44,168,134,130,29,200,12,128,46,56,6,128,31,88,204,128,33,120,11,128,34,8,11,129,47,200,5,128,48,88,5,128,53, 
  232,4,128,82,72,4,128,92,120,3,128,8,4,15,1,179,85,0,0,15,1,203,86,0,0,15,1,215,86,0,0,17,1,67,85,0,0,1,4,15,1,43,85,0,0,15,1,55,85,0,0,17,1,204,2,0,0,1,4,15, 
  1,31,85,0,0,17,1,25,80,0,0,1,4,15,1,13,80,0,0,17,1,60,79,0,0,1,4,15,1,13,80,0,0,17,1,107,78,0,0,1,4,15,1,13,80,0,0,17,1,166,76,0,0,1,4,15,1,154,76,0, 
  0,17,1,223,69,0,0,1,4,15,1,43,85,0,0,17,1,211,69,0,0,1,4,15,1,43,85,0,0,17,1,199,69,0,0,1,4,15,1,187,69,0,0,17,1,100,69,0,0,1,4,15,1,88,69,0,0,17,1,1, 
  69,0,0,1,4,15,1,88,69,0,0,17,1,170,68,0,0,1,4,15,1,88,69,0,0,17,1,83,68,0,0,1,4,15,1,88,69,0,0,17,1,252,67,0,0,1,4,15,1,88,69,0,0,17,1,165,67,0,0,1, 
  4,15,1,88,69,0,0,17,1,78,67,0,0,1,4,15,1,88,69,0,0,17,1,247,66,0,0,1,4,15,1,88,69,0,0,17,1,160,66,0,0,1,4,15,1,88,69,0,0,17,1,73,66,0,0,1,4,15,1,88, 
  69,0,0,17,1,242,65,0,0,1,4,15,1,230,65,0,0,17,1,175,64,0,0,1,2,15,1,163,140,0,0,17,1,66,167,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128, 
  4,17,1,64,53,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47,240,128,128,45,48, 
  193,127,95,48,1,128,3,17,1,225,167,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1, 
  3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,60,169,0,0,1,3,17,1,12,168,0,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,97,168,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128, 
  3,48,1,128,6,48,1,128,3,17,1,231,168,0,0,1,3,17,1,231,168,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,225,168,0,0,1,3, 
  17,1,194,168,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,231,168,0,0,1,3,17,1,231,168,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,225,168,0,0,1,2,18,7,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0, 
  128,3,17,1,194,168,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,231,168,0,0,1,3,17,1,231,168, 
  0,0,1,2,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,115,169,0,0,1,3,17,1,60,169,0,0,1,2,18, 
  6,0,0,0,1,18,57,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,2,0,0,0,1,18,1,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,61,208,0,128,47,16,193,127,3,17,1, 
  36,170,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,42,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,208,0,128,33,16,1,128,3,17,1,116,170,0,0,1,3,17,1,79,170,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0, 
  128,3,17,1,110,170,0,0,1,2,18,4,0,0,0,1,18,3,0,0,0,1,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144,2,128, 
  109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,117,175,0,0,1,3,17,1,132,174,0,0,1,3,17,1,80,173,0,0,1,3,17,1,206,172,0,0,1,3,17,1,107,172,0,0,1,3,17,1,184,171,0,0,1, 
  3,17,1,23,171,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,54,171,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,85,171,0,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,116,171,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,17,1,147,171,0, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,178,171,0,0,1,2,18,87,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,111,208, 
  0,128,97,16,193,127,3,17,1,39,172,0,0,1,3,17,1,227,171,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,2,172,0,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,33,172,0,0,1,2,18,85,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,17,1,70,172,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,101,172,0,0,1,2,18,99,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0, 
  128,3,17,1,138,172,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,169,172,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111, 
  176,0,128,3,17,1,200,172,0,0,1,2,18,98,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,237,172,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,116,176,0,128,3,17,1,12,173,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,43,173,0,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,74,173,0,0,1,2,18,93,0,0,0,1,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,101,144,129,128,97,208,193,127,117,16,1,128,111,80, 
  1,128,3,17,1,64,174,0,0,1,3,17,1,27,174,0,0,1,3,17,1,215,173,0,0,1,3,17,1,147,173,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1, 
  178,173,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,209,173,0,0,1,2,18,86,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,101,176,0,128,3,17,1,246,173,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,21,174,0,0,1,2,18,70,0,0,0,1,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,112,176,0,128,3,17,1,58,174,0,0,1,2,18,59,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,95,174,0,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,126,174,0,0,1,2,18,97,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17, 
  1,163,174,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,100,16,1,128,115,208,0,128,3,17,1,49,175,0,0,1,3,17,1,206,174,0,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,237,174,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,12,175,0,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,43,175,0,0,1,2,18,63,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,80,175,0,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,111,175,0,0,1,2,18,73,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,104,208,0,128,101, 
  16,1,128,3,17,1,49,177,0,0,1,3,17,1,160,175,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,191,175,0,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,222,175,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,65,128,116,208,0,128,3,17,1,108,176,0,0,1,3,17,1,9, 
  176,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,40,176,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17, 
  1,71,176,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,102,176,0,0,1,2,18,66,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,111,176,0,128,3,17,1,139,176,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,170,176,0,0,1,2,18,64,0,0,0,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,206,176,0,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,237,176,0,0,1,2,21,4,30,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,12,177,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,43,177,0,0,1,2,18,67,0,0,0, 
  1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,80,177,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,111,177,0, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,142,177,0,0,1,2,18,74,0,0,0,1,21,4,106,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,144, 
  2,128,116,16,2,128,58,208,2,128,43,16,3,128,60,80,66,127,45,144,66,128,125,144,1,128,47,208,65,126,3,17,1,111,180,0,0,1,3,17,1,225,167,0,0,1,3,17,1,35,179,0,0,1,3,17,1,139,178,0, 
  0,1,3,17,1,181,178,0,0,1,3,17,1,102,178,0,0,1,3,17,1,65,178,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128, 
  3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,3,17,1,181,178,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,17,1,96,178, 
  0,0,1,2,18,24,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,17,1,133,178,0,0,1,2,18,23,0,0,0,1,18,26,0,0,0,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,17,1,175,178,0,0,1,1,18,21,0,0,0,1,18,57,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127, 
  3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,82,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,4,176,0,128,3,17,1,255,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,181,178,0, 
  0,1,3,17,1,121,179,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,181,178,0,0,1,3,17,1,207,179,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208, 
  0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,110,240,0,128,45,48,65,128,95,48,1,128,3,17,1,37,180,0,0,1,3,17,1,181,178,0, 
  0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,13,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,11,0,0,0,1,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,225,167,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1, 
  201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,48,1,128,45,240,1,128,95,240,129,128,47,112,193,127,115,176,1,128, 
  3,17,1,161,183,0,0,1,3,17,1,225,167,0,0,1,3,17,1,83,181,0,0,1,3,17,1,121,169,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16, 
  1,128,3,208,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,3,17,1,121,169,0,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,95,48,129,128,45,48,193,127,121,240,0,128,3,17,1,169,181,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121, 
  169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,109,240,0,128,3,17,1,255,181,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,98,240,0,128,45,48, 
  65,128,95,48,1,128,3,17,1,85,182,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0, 
  0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,111,240,0,128,3,17,1,171,182,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,108,240,0,128,45,48,65,128,95,48,1,128,3,17, 
  1,1,183,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,115,240,0,128,3,17,1,87,183,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,65,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,51,0,0,0,1,21,4,118,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,240,1,128,47,48,66, 
  129,34,112,2,128,91,240,66,129,116,48,3,128,45,112,3,128,95,112,3,128,39,176,129,126,99,176,2,128,3,17,1,108,187,0,0,1,3,17,1,161,183,0,0,1,3,17,1,225,167,0,0,1,3,17,1,140,185,0,0, 
  1,3,17,1,48,185,0,0,1,3,17,1,42,185,0,0,1,3,17,1,84,184,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5, 
  48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,107, 
  240,0,128,3,17,1,170,184,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21, 
  4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,48,65,128,95,48,1,128,3,17,1,0,185,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,100,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,3,17,1,36,185,0,0,1,1,18,88,0,0, 
  0,1,18,44,0,0,0,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,48,65,128,95,48,1,128,3,17,1,134,185,0,0,1,3,17,1,121,169,0,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,101,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16, 
  65,128,92,208,0,128,3,17,1,243,185,0,0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128, 
  3,17,1,11,187,0,0,1,3,17,1,11,187,0,0,1,2,18,119,0,0,0,1,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1, 
  30,186,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,127,186,0,0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,170,186,0,0,1,3,17,1,170,186,0,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0, 
  0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,11,187,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,127,186,0,0,1, 
  3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,170,186,0,0,1,3,17,1,170,186,0, 
  0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,127,186,0,0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,170,186,0,0,1,3,17,1,170,186,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208, 
  0,128,39,16,1,128,3,17,1,211,187,0,0,1,3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128, 
  3,17,1,235,188,0,0,1,3,17,1,235,188,0,0,1,2,18,120,0,0,0,1,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1, 
  254,187,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,95,188,0,0,1,3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,138,188,0,0,1,3,17,1,138,188,0,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0, 
  0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,235,188,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,95,188,0,0,1, 
  3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,138,188,0,0,1,3,17,1,138,188,0, 
  0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,95,188,0,0,1,3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,138,188,0,0,1,3,17,1,138,188,0,0,1,2,21,4,106,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,208, 
  1,128,47,16,66,129,34,80,2,128,99,144,2,128,116,208,2,128,45,16,3,128,95,16,3,128,39,144,129,126,3,17,1,108,187,0,0,1,3,17,1,161,183,0,0,1,3,17,1,225,167,0,0,1,3,17,1,140,185,0, 
  0,1,3,17,1,48,185,0,0,1,3,17,1,84,184,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201, 
  169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,178,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,80,5,128,42,144,196,129,34,208,194,127,43,16,69,130,116,144,3,128,45, 
  80,68,129,94,16,4,128,39,16,131,128,58,208,4,128,63,144,130,128,93,80,2,128,95,80,4,128,99,80,67,128,123,208,3,128,3,17,1,175,196,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3, 
  17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,98,191,0,0,1,3,17,1,181,178,0,0,1,3,17,1,92,191,0,0,1,3,17,1,24,191, 
  0,0,1,3,17,1,18,191,0,0,1,3,17,1,214,190,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1, 
  195,169,0,0,1,3,17,1,181,178,0,0,1,2,18,51,0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,43,208,0,128,3,17,1,12,191,0,0,1,3,17,1,6,191,0,0, 
  1,1,18,122,0,0,0,1,18,123,0,0,0,1,18,50,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,55,191,0,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,60,176,0,128,3,17,1,86,191,0,0,1,2,18,125,0,0,0,1,18,121,0,0,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127, 
  3,17,1,163,191,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,163,191,0,0,1,2,18,113,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,95,208,0,128,45,208,192,127,3,17,1,163,191,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,163,191,0,0,1,1,18,10,0,0,0,1,18, 
  57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,48,1,128,3,17,1,181,178,0,0,1,3,17,1,73,192,0,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,48,65,128,95,48,1,128, 
  3,17,1,0,185,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,48,1,128,45,240,64,128,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,134,185,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,86,193,0,0,1,3,17,1,205,187,0,0,1,21, 
  2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,110,194,0,0,1,3,17,1,110,194,0,0,1,2,21,2,42,0,0,0, 
  255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,129,193,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39, 
  16,1,128,3,17,1,226,193,0,0,1,3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1, 
  13,194,0,0,1,3,17,1,13,194,0,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,110,194,0,0,1,2,21,4,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,226,193,0,0,1,3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1, 
  128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,13,194,0,0,1,3,17,1,13,194,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128, 
  3,17,1,226,193,0,0,1,3,17,1,205,187,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,13,194,0, 
  0,1,3,17,1,13,194,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,48,195,0,0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255, 
  255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,72,196,0,0,1,3,17,1,72,196,0,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0, 
  0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,91,195,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1, 
  188,195,0,0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,231,195,0,0,1,3, 
  17,1,231,195,0,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,72,196,0,0,1,2,21,4,42,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,188,195,0,0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2, 
  112,129,128,3,112,1,128,6,112,1,128,3,17,1,231,195,0,0,1,3,17,1,231,195,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,188,195,0, 
  0,1,3,17,1,237,185,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,231,195,0,0,1,3,17,1,231, 
  195,0,0,1,2,18,110,0,0,0,1,18,45,0,0,0,1,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,240,0,128,45,48,65,128,95,48,1,128,3,17,1,161,183,0,0,1,3,17,1,121,169, 
  0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,44,112,65,128,58,48,65,128,62,240,0,128,3,17,1,166,197,0,0,1,3,17,1,129,197,0,0,1,3,17,1,123,197,0,0,1,21,2,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,18,9,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58, 
  176,0,128,3,17,1,160,197,0,0,1,2,18,91,0,0,0,1,18,20,0,0,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,178,0,0,0,255, 
  255,255,255,14,0,0,0,3,0,0,0,40,80,5,128,42,144,196,129,34,208,194,127,43,16,69,130,116,144,3,128,45,80,68,129,94,16,4,128,39,16,131,128,58,208,4,128,63,144,130,128,93,80,2,128,95,80,4,128,99, 
  80,67,128,123,208,3,128,3,17,1,175,196,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191, 
  0,0,1,3,17,1,98,191,0,0,1,3,17,1,181,178,0,0,1,3,17,1,92,191,0,0,1,3,17,1,238,198,0,0,1,3,17,1,18,191,0,0,1,3,17,1,214,190,0,0,1,21,2,54,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,58,176,0,128,3,17,1,13,199,0,0,1,2,18,91,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,3,17,1,86,191,0,0,1,1,21,4,118,0,0,0,255, 
  255,255,255,9,0,0,0,3,0,0,0,40,48,2,128,47,112,194,129,34,176,2,128,99,240,2,128,116,48,3,128,45,112,67,128,93,176,1,128,39,240,129,126,95,112,3,128,3,17,1,175,196,0,0,1,3,17,1,108,187, 
  0,0,1,3,17,1,161,183,0,0,1,3,17,1,225,167,0,0,1,3,17,1,140,185,0,0,1,3,17,1,48,185,0,0,1,3,17,1,84,184,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,202,0,0,0,255,255,255,255,16,0,0,0, 
  4,0,0,0,91,144,5,128,33,144,4,128,34,208,2,128,99,144,3,128,36,16,132,128,95,16,5,128,116,208,3,128,39,16,3,128,40,144,2,128,41,80,5,128,58,80,4,128,43,208,69,125,60,208,132,128,45,16,5,128, 
  124,16,6,128,47,80,131,125,3,17,1,161,183,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,225,167,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,96, 
  201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,235,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1, 
  3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0, 
  0,1,2,18,81,0,0,0,1,18,52,0,0,0,1,18,114,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,16,1,128,97,208,0,128,3,17,1,28,201,0,0,1,3,17,1,133,178, 
  0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,59,201,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1, 
  90,201,0,0,1,2,18,62,0,0,0,1,18,92,0,0,0,1,21,4,238,0,0,0,255,255,255,255,19,0,0,0,4,0,0,0,58,112,6,128,91,48,6,132,34,112,3,128,99,240,3,128,36,176,4,131,93,240,2,128, 
  95,112,5,128,39,176,3,128,40,240,6,128,41,176,5,128,42,240,133,125,43,176,134,125,60,48,133,129,45,112,5,126,94,240,4,128,63,48,195,125,116,48,4,128,123,112,4,128,124,48,7,128,3,17,1,175,196,0,0,1, 
  3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,98, 
  191,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,92,191,0,0,1,3,17,1,42,185,0,0,1,3,17,1,175,202,0,0,1,3,17,1,139,202,0,0,1, 
  3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0, 
  0,1,3,17,1,181,178,0,0,1,2,18,50,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,17,1,96,178,0,0,1,1,21,4,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,58,112,129,128,97,240,0,128,62,48,1,128,3,17,1,28,201,0,0,1,3,17,1,133,178,0,0,1,3,17,1,55,191,0,0,1,2,21,4,238,0,0,0,255,255,255,255,19,0,0,0,4,0,0,0, 
  58,176,5,128,91,112,6,132,34,112,3,128,99,240,3,128,36,176,4,131,93,240,2,128,95,112,5,128,39,176,3,128,40,240,6,128,41,240,5,128,42,48,134,125,43,176,134,125,60,48,133,129,45,112,5,126,94,240,4,128, 
  63,48,195,125,116,48,4,128,123,112,4,128,124,48,7,128,3,17,1,175,196,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243, 
  191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,98,191,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,11,204,0,0,1,3,17,1,229,200,0,0,1, 
  3,17,1,92,191,0,0,1,3,17,1,42,185,0,0,1,3,17,1,139,202,0,0,1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240, 
  0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,112,129,128,97,240,0, 
  128,62,48,1,128,3,17,1,28,201,0,0,1,3,17,1,133,178,0,0,1,3,17,1,13,199,0,0,1,2,21,4,142,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,48,2,128,63,112,2,130,34,176,2,128, 
  99,112,195,129,116,176,3,128,45,48,196,128,94,48,3,128,39,240,130,126,93,240,1,128,95,48,4,128,123,240,3,128,3,17,1,175,196,0,0,1,3,17,1,161,183,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207, 
  194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,181,178,0,0,1,21,2,54,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,72,205,0,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,72,205,0,0,1,2,18,113,0,0,0,21,4,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,72,205,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1, 
  72,205,0,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,125,208,0,128,3,17,1,111,180,0,0,1,3,17,1,243,205,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,249,205,0,0,1,2,18,55,0,0,0,1,18,124,0,0,0,21,2,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,249,205,0,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,3,17,1,111,180,0,0,1,21,2,54,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,249,205,0,0,1,2,21,4,130,0,0,0,255,255,255,255, 
  10,0,0,0,3,0,0,0,40,16,2,128,63,80,2,130,34,144,2,128,99,80,3,128,116,144,3,128,45,208,195,128,94,16,3,128,39,208,130,126,93,208,1,128,95,208,3,128,3,17,1,175,196,0,0,1,3,17,1,161, 
  183,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,181,178,0,0,1, 
  21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,118,0,0, 
  0,255,255,255,255,9,0,0,0,3,0,0,0,40,240,1,128,93,176,1,128,34,48,2,128,99,240,2,128,116,48,3,128,45,112,3,127,94,176,2,128,39,112,66,128,95,112,3,128,3,17,1,175,196,0,0,1,3,17,1, 
  161,183,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,181,178,0,0,1,21,2,54,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,130,0,0,0,255,255,255,255,10,0, 
  0,0,3,0,0,0,40,16,2,128,93,208,1,128,34,80,2,128,99,16,131,129,116,80,3,128,45,208,3,127,94,208,2,128,39,144,66,128,95,208,3,128,123,144,3,128,3,17,1,175,196,0,0,1,3,17,1,161,183,0, 
  0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,181,178,0,0,1,21,2, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,166,0,0,0,255, 
  255,255,255,13,0,0,0,3,0,0,0,40,240,4,128,42,112,4,128,34,176,194,127,43,176,4,130,116,112,3,128,45,48,4,129,94,240,3,128,39,240,66,128,63,112,130,128,93,48,2,128,95,48,4,128,99,48,67,128,123, 
  176,3,128,3,17,1,175,196,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3, 
  17,1,98,191,0,0,1,3,17,1,181,178,0,0,1,3,17,1,92,191,0,0,1,3,17,1,18,191,0,0,1,3,17,1,214,190,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,99,16,1,128,41,80,1,128, 
  34,208,1,128,39,144,65,127,3,17,1,219,209,0,0,1,3,17,1,229,200,0,0,1,3,17,1,108,187,0,0,1,3,17,1,140,185,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,134,185,0,0,1,2,21,4,226,0,0,0, 
  255,255,255,255,18,0,0,0,4,0,0,0,58,16,6,128,91,208,197,131,34,16,3,128,99,144,3,128,36,80,132,128,95,16,5,128,116,208,3,128,39,80,3,128,40,144,6,128,41,80,5,128,42,144,133,125,43,80,134,125, 
  60,208,68,129,45,16,5,128,94,144,4,128,63,208,130,125,123,16,4,128,124,208,6,128,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243, 
  191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,98,191,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,92,191,0,0,1, 
  3,17,1,42,185,0,0,1,3,17,1,175,202,0,0,1,3,17,1,139,202,0,0,1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240, 
  0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,226,0,0,0,255,255,255,255,18,0,0,0,4,0,0,0,58,80,5,128,91,16,198, 
  131,34,16,3,128,99,144,3,128,36,80,132,128,95,16,5,128,116,208,3,128,39,80,3,128,40,144,6,128,41,144,5,128,42,208,133,125,43,80,134,125,60,208,68,129,45,16,5,128,94,144,4,128,63,208,130,125,123,16,4, 
  128,124,208,6,128,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0, 
  1,3,17,1,98,191,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,11,204,0,0,1,3,17,1,229,200,0,0,1,3,17,1,92,191,0,0,1,3,17,1,42,185,0,0,1,3,17,1, 
  139,202,0,0,1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3, 
  17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,47,208,0,128,41,16,193,127,3,17,1,225,167,0,0,1,3,17,1,229,200,0,0,1,21,2, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,116, 
  144,1,128,97,144,194,128,110,16,194,128,115,208,1,128,105,80,2,128,118,80,1,128,3,17,1,95,214,0,0,1,3,17,1,27,214,0,0,1,3,17,1,197,213,0,0,1,3,17,1,111,213,0,0,1,3,17,1,74,213, 
  0,0,1,3,17,1,6,213,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,37,213,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128,3,17,1,68,213,0,0,1,2,18,108,0,0, 
  0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,105,213,0,0,1,2,18,102,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,1, 
  128,117,208,0,128,3,17,1,160,213,0,0,1,3,17,1,154,213,0,0,1,2,18,103,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,17,1,191,213,0,0,1,2,18, 
  106,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,16,1,128,121,208,0,128,3,17,1,246,213,0,0,1,3,17,1,240,213,0,0,1,2,18,104,0,0,0,1,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,17,1,21,214,0,0,1,2,18,107,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,58,214,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,17,1,89,214,0,0,1,2,18,105,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0, 
  128,3,17,1,126,214,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,157,214,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98, 
  176,0,128,3,17,1,188,214,0,0,1,2,18,109,0,0,0,1,21,4,106,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,16,2,128,41,80,2,128,58,144,2,128,43,208,2,128,60,208,129,128,45,16,2,128, 
  124,16,3,128,47,144,65,126,3,17,1,225,167,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,102,178,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223, 
  200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21, 
  4,190,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,176,2,128,41,240,4,128,34,112,131,129,43,112,197,129,36,240,67,129,45,176,4,128,47,48,67,129,39,112,194,127,58,48,4,128,60,112,4,129,91,48,133, 
  128,95,176,4,128,99,176,3,128,116,240,66,128,124,176,5,128,3,17,1,108,187,0,0,1,3,17,1,161,183,0,0,1,3,17,1,88,216,0,0,1,3,17,1,225,167,0,0,1,3,17,1,140,185,0,0,1,3,17,1, 
  159,192,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0, 
  1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178, 
  0,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,48,1,128,3,17,1,181,178,0,0,1,3,17,1,73,192,0,0,1,21,2,34,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,21,4,250,0,0,0,255,255,255,255,20,0,0,0,4,0,0,0,58,16,6,128,61,208,3,128,34,80,3,128, 
  99,80,4,128,36,16,69,131,63,16,195,130,91,208,6,131,39,144,3,128,40,80,7,128,41,80,6,128,42,144,134,125,43,16,199,126,60,144,197,129,45,208,5,125,94,80,5,128,47,16,132,125,95,208,5,128,116,144,4,128, 
  123,208,4,128,124,144,7,128,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,36,170,0,0,1,3,17,1,225,167,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243, 
  191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,98,191,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,11,204,0,0,1,3,17,1,229,200,0,0,1, 
  3,17,1,92,191,0,0,1,3,17,1,42,185,0,0,1,3,17,1,139,202,0,0,1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240, 
  0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,214,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,91,240,133,129,95,112,5, 
  128,34,48,3,128,99,240,3,128,36,176,68,128,116,48,4,128,123,112,4,128,39,112,3,128,40,176,2,128,41,176,5,128,58,240,4,128,43,48,70,125,60,48,5,129,45,112,5,128,94,176,3,128,63,240,130,124,124,112,6, 
  128,3,17,1,161,183,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1, 
  237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0, 
  1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178, 
  0,0,1,2,21,4,202,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,91,144,69,129,116,208,3,128,34,208,2,128,99,144,3,128,36,80,68,127,123,16,4,128,124,16,6,128,39,16,3,128,40,144,2,128,41,80, 
  5,128,58,144,4,128,43,208,69,125,60,208,132,126,45,16,5,128,94,80,3,128,95,16,5,128,3,17,1,161,183,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17, 
  1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0, 
  0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201, 
  169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,190,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,112,2,128,41,240,4,128,34,176,130,129,43,112,197,129,36,240,67,129,45, 
  176,4,128,94,48,3,128,39,240,2,129,58,48,4,128,60,112,4,129,91,48,133,128,95,176,4,128,99,112,3,128,116,176,67,128,124,176,5,128,3,17,1,161,183,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192, 
  0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3, 
  17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1, 
  128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,202,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,91,144,5,128,95,16,5,128,34,16,3,128,99,208,3,128, 
  36,80,68,128,116,16,4,128,124,16,6,128,39,80,3,128,40,144,2,128,41,80,5,128,58,144,4,128,43,208,69,125,60,208,132,126,45,16,5,128,94,144,3,128,63,208,130,124,3,17,1,161,183,0,0,1,3,17,1,169, 
  196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1, 
  3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,226,0,0,0,255,255,255,255,18,0,0,0,4,0,0, 
  0,58,208,4,128,91,16,198,131,34,16,3,128,99,144,3,128,36,80,132,128,95,80,5,128,116,208,3,128,39,80,3,128,40,144,6,128,41,144,5,128,42,208,133,125,43,80,134,125,60,16,69,129,45,80,5,128,94,144,4, 
  128,63,208,130,125,123,16,4,128,124,208,6,128,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0, 
  1,3,17,1,96,201,0,0,1,3,17,1,98,191,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,92,191,0,0,1,3,17,1, 
  42,185,0,0,1,3,17,1,139,202,0,0,1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3, 
  17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,106,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,16,2,128,41,80,2,128,58,144,2,128,43,208,2,128,60,208, 
  129,128,45,16,2,128,124,16,3,128,47,144,65,126,3,17,1,225,167,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,157,223,0,0,1,3,17,1,65,178,0, 
  0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181, 
  178,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,16,1,128,97,208,0,128,3,17,1,28,201,0,0,1,3,17,1,133,178,0,0,1,2,21,4,150,0,0,0,255,255,255,255,11,0, 
  0,0,3,0,0,0,115,176,130,130,105,48,3,128,98,176,3,128,91,240,67,127,36,112,4,129,109,240,2,129,102,112,3,128,47,48,4,128,116,112,2,128,117,48,2,128,123,240,1,128,3,17,1,237,191,0,0,1,3,17, 
  1,5,228,0,0,1,3,17,1,28,227,0,0,1,3,17,1,216,226,0,0,1,3,17,1,148,226,0,0,1,3,17,1,220,225,0,0,1,3,17,1,248,224,0,0,1,3,17,1,149,224,0,0,1,3,17,1,42,185,0, 
  0,1,3,17,1,225,167,0,0,1,3,17,1,96,201,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195, 
  169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,180,224,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,111,176,0,128,3,17,1,211,224,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,242,224,0,0,1,2,18,41,0,0,0,1,21,4,54,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,54,48,1,128,51,112,65,128,97,240,0,128,3,17,1,121,225,0,0,1,3,17,1,84,225,0,0,1,3,17,1,47,225,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,50,176,0,128,3,17,1,78,225,0,0,1,2,18,33,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,17,1,115,225,0,0,1,2,18,36,0,0,0, 
  1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,152,225,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,183,225,0, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,214,225,0,0,1,2,18,43,0,0,0,1,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56,16, 
  1,128,49,208,1,128,54,80,1,128,51,144,1,128,3,17,1,142,226,0,0,1,3,17,1,105,226,0,0,1,3,17,1,68,226,0,0,1,3,17,1,31,226,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,54,176,0,128,3,17,1,62,226,0,0,1,2,18,39,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,17,1,99,226,0,0,1,2,18,34,0,0,0, 
  1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,17,1,136,226,0,0,1,2,18,37,0,0,0,1,18,31,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,97,176,0,128,3,17,1,179,226,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,17,1,210,226,0,0,1,2,18,53,0,0,0,1,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,247,226,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,17,1,22,227,0,0,1,2,18,29,0,0,0,1, 
  21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,240,0,128,107,112,65,128,111,48,1,128,3,17,1,193,227,0,0,1,3,17,1,89,227,0,0,1,3,17,1,83,227,0,0,1,2,18,46,0,0,0, 
  1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,120,227,0,0,1,2,18,47,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3, 
  17,1,156,227,0,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,187,227,0,0,1,2,18,48,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,117,176,0,128,3,17,1,224,227,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,255,227,0,0,1,2,18,42,0,0,0,1,21,4,66,0,0,0,255, 
  255,255,255,4,0,0,0,2,0,0,0,56,16,1,128,49,208,1,128,54,80,1,128,51,144,1,128,3,17,1,183,228,0,0,1,3,17,1,146,228,0,0,1,3,17,1,109,228,0,0,1,3,17,1,72,228,0,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,3,17,1,103,228,0,0,1,2,18,40,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3, 
  17,1,140,228,0,0,1,2,18,35,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,17,1,177,228,0,0,1,2,18,38,0,0,0,1,18,32,0,0,0,1,21,4,138, 
  0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,115,144,2,128,105,16,3,128,98,144,3,128,91,208,67,127,36,16,196,128,109,208,194,128,102,80,3,128,116,80,2,128,117,16,66,128,125,208,1,128,3,17,1,111,180, 
  0,0,1,3,17,1,5,228,0,0,1,3,17,1,126,229,0,0,1,3,17,1,216,226,0,0,1,3,17,1,148,226,0,0,1,3,17,1,220,225,0,0,1,3,17,1,248,224,0,0,1,3,17,1,149,224,0,0,1,3, 
  17,1,42,185,0,0,1,3,17,1,96,201,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0, 
  1,3,17,1,255,178,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,144,65,128,111,80,1,128,114,16,1,128,95,208,65,127,3,17,1,193,227,0,0,1,3,17,1,89,227,0,0,1, 
  3,17,1,83,227,0,0,1,3,17,1,193,229,0,0,1,2,18,56,0,0,0,1,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,240,0,128,45,48,65,128,95,48,1,128,3,17,1,44,230,0,0, 
  1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169, 
  0,0,1,2,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,80,129,128,45,144,1,128,111,16,1,128,95,144,65,127,3,17,1,216,230,0,0,1,3,17,1,142,230,0,0,1,3, 
  17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,46,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0, 
  0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,107,240,0,128,3,17,1,46,231,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,47,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,101,240,0,128,3,17,1, 
  132,231,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,110,240,0,128,45,48,65,128,95,48,1,128,3,17,1,218,231,0,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0, 
  128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,48,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,21,4,138,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,115,144,66,130,105,16,3,128,98,144,3,128,91,208, 
  67,127,36,16,196,128,109,208,194,128,102,80,3,128,116,80,2,128,117,16,2,128,123,208,1,128,3,17,1,237,191,0,0,1,3,17,1,5,228,0,0,1,3,17,1,28,227,0,0,1,3,17,1,216,226,0,0,1,3,17, 
  1,148,226,0,0,1,3,17,1,220,225,0,0,1,3,17,1,248,224,0,0,1,3,17,1,149,224,0,0,1,3,17,1,42,185,0,0,1,3,17,1,96,201,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116, 
  176,0,128,3,17,1,46,233,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,193,229,0,0,1,2,21,4,126,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,115,48,2,128,105,176,2,128,98,48,3,128,91,112,67,127,36, 
  176,195,128,109,112,194,128,102,240,2,128,116,240,1,128,117,176,1,128,3,17,1,5,228,0,0,1,3,17,1,28,227,0,0,1,3,17,1,216,226,0,0,1,3,17,1,148,226,0,0,1,3,17,1,220,225,0,0,1,3, 
  17,1,248,224,0,0,1,3,17,1,149,224,0,0,1,3,17,1,42,185,0,0,1,3,17,1,96,201,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0, 
  128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,36,16,65,128,116,208,0,128,3,17,1,87,234,0,0,1, 
  3,17,1,96,201,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,42,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,111,208,0,128,107,16,193,127,3,17,1,89,227,0,0,1,3,17,1,83,227,0,0,1,2,21,4,138,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,109,144,66,130,105,208,2,128, 
  98,80,3,128,91,208,3,129,36,16,4,129,93,144,195,126,102,16,3,128,115,80,2,128,116,16,2,128,117,208,1,128,3,17,1,5,228,0,0,1,3,17,1,28,227,0,0,1,3,17,1,216,226,0,0,1,3,17,1,148, 
  226,0,0,1,3,17,1,220,225,0,0,1,3,17,1,248,224,0,0,1,3,17,1,149,224,0,0,1,3,17,1,175,196,0,0,1,3,17,1,42,185,0,0,1,3,17,1,96,201,0,0,1,21,2,54,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,201,169, 
  0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,3,17,1,121,169,0,0,1,2,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17, 
  1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,226,0,0,0,255,255,255,255,18,0,0,0,4,0,0,0,91,80,6,132,93,208,2,128,34,144,3,128,99,80,4,128,36,16,133, 
  128,95,208,5,128,116,144,4,128,39,208,3,128,40,16,3,128,41,16,6,128,58,80,5,128,43,144,70,125,60,144,69,129,45,208,5,125,94,16,4,128,63,80,131,125,123,208,4,128,124,208,6,128,3,17,1,175,196,0,0, 
  1,3,17,1,161,183,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1, 
  237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0, 
  1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178, 
  0,0,1,2,21,4,214,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,91,240,5,128,93,176,2,128,34,112,3,128,99,48,4,128,36,176,132,128,95,112,5,128,116,112,4,128,39,176,3,128,40,240,2,128,41,176, 
  5,128,58,240,4,128,43,48,70,125,60,48,5,129,45,112,5,125,94,240,3,128,63,48,131,125,124,112,6,128,3,17,1,175,196,0,0,1,3,17,1,161,183,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0, 
  0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17, 
  1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128, 
  3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,202,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,91,144,5,128,93,144,2,128,34, 
  16,3,128,99,208,3,128,36,80,68,128,116,16,4,128,124,16,6,128,39,80,3,128,40,208,2,128,41,80,5,128,58,144,4,128,43,208,69,125,60,208,132,126,45,16,5,125,94,144,3,128,95,16,5,128,3,17,1,175,196, 
  0,0,1,3,17,1,161,183,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,96,201,0,0,1,3, 
  17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,214,0,0,0,255,255,255,255, 
  17,0,0,0,4,0,0,0,91,240,133,129,93,176,2,128,34,48,3,128,99,240,3,128,36,176,68,128,116,48,4,128,123,112,4,128,39,112,3,128,40,240,2,128,41,176,5,128,58,240,4,128,43,48,70,125,60,48,5,129, 
  45,112,5,125,94,176,3,128,95,112,5,128,124,112,6,128,3,17,1,175,196,0,0,1,3,17,1,161,183,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,7,205,0,0,1,3,17,1,159, 
  192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1, 
  3,17,1,42,185,0,0,1,3,17,1,65,178,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0, 
  0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,238,0,0,0,255,255,255,255,19,0,0,0,4,0,0,0,58,48,5,128,91,112,6,132,34,112,3,128,99,240,3,128,36,176,4,131,93,240,2, 
  128,95,176,5,128,39,176,3,128,40,240,6,128,41,240,5,128,42,48,134,125,43,176,134,125,60,112,133,129,45,176,5,126,94,240,4,128,63,48,195,125,116,48,4,128,123,112,4,128,124,48,7,128,3,17,1,175,196,0,0, 
  1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,159,192,0,0,1,3,17,1,243,191,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1, 
  98,191,0,0,1,3,17,1,241,200,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,229,200,0,0,1,3,17,1,92,191,0,0,1,3,17,1,42,185,0,0,1,3,17,1,139,202,0,0, 
  1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169, 
  0,0,1,3,17,1,181,178,0,0,1,2,21,4,106,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,144,2,128,116,16,2,128,58,208,2,128,43,16,3,128,60,80,66,127,45,144,66,128,125,144,1,128,47,208, 
  65,126,3,17,1,111,180,0,0,1,3,17,1,225,167,0,0,1,3,17,1,35,179,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,157,223,0,0,1,3,17,1,65,178,0,0,1,21,2, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,78,1,0,0,255, 
  255,255,255,27,0,0,0,4,0,0,0,58,48,8,128,65,176,69,133,34,176,4,128,99,240,5,128,36,112,198,132,69,240,6,128,60,176,199,132,39,240,4,128,40,240,9,128,41,112,200,129,42,176,136,125,43,176,201,129,44, 
  48,133,126,45,240,135,129,62,112,197,128,63,112,132,129,73,112,7,128,78,48,199,128,91,240,136,129,93,240,195,129,94,176,6,128,95,240,7,128,97,48,9,128,116,112,9,128,123,48,6,128,124,48,10,128,125,48,4,128,3, 
  17,1,175,196,0,0,1,3,17,1,111,180,0,0,1,3,17,1,169,196,0,0,1,3,17,1,207,194,0,0,1,3,17,1,245,192,0,0,1,3,17,1,123,197,0,0,1,3,17,1,166,197,0,0,1,3,17,1,74,251, 
  0,0,1,3,17,1,159,192,0,0,1,3,17,1,237,191,0,0,1,3,17,1,96,201,0,0,1,3,17,1,98,191,0,0,1,3,17,1,82,249,0,0,1,3,17,1,6,248,0,0,1,3,17,1,96,244,0,0,1,3, 
  17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,11,204,0,0,1,3,17,1,229,200,0,0,1,3,17,1,92,191,0,0,1,3,17,1,42,185,0,0,1,3,17,1,192,243,0,0,1,3,17,1,94,243, 
  0,0,1,3,17,1,139,202,0,0,1,3,17,1,214,190,0,0,1,3,17,1,223,200,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1, 
  201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,104,144,1,128,45,16,1,128,107,80,1,128,95,16,193, 
  127,3,17,1,181,178,0,0,1,3,17,1,73,192,0,0,1,3,17,1,121,179,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1, 
  18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,48,1,128,3,17,1,181,178,0,0,1,3,17,1,22,244,0,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,76,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1, 
  181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2, 
  0,0,0,77,80,1,128,45,144,193,127,95,144,1,128,71,16,193,127,3,17,1,100,246,0,0,1,3,17,1,194,244,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,48,1,128,45,240,64,128,95,240,0,128,3,17,1,181,178,0,0, 
  1,3,17,1,24,245,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,110,245,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,196,245,0,0,1,3,17,1,181,178,0,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84, 
  240,0,128,45,48,65,128,95,48,1,128,3,17,1,26,246,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0, 
  1,1,18,78,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,186,246,0,0,1,3, 
  17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,16,247,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,102,247,0,0,1,3,17,1,181,178,0,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128, 
  128,45,48,193,127,95,48,1,128,3,17,1,188,247,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1, 
  18,80,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0, 
  128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,92,248,0,0,1,3,17,1, 
  181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,77,48,129,128,45,240,192,127,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,178,248,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1, 
  181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,48,129,128,45,240,192,127,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,8,249,0,0,1,21,2,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,79,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45, 
  208,192,127,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,88,240,0,128,45,48,65,128,95,48,1,128,3,17,1,168,249,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3, 
  208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,254,249,0,0,1,3,17,1,181,178, 
  0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,84,250,0,0,1,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178, 
  0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,170,250,0,0,1,21,2,34,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64, 
  128,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,0,251,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,77,0,0, 
  0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,181,178,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,83,240,128,128,45,48,193,127,95,48,1,128,3,17,1,160,251,0,0,1,3,17,1,181,178,0,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,18,75,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,17,1,181,178,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,21,4,42,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,63,252,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17, 
  1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,94,252,0,0,1,2,18,58,0,0,0,1,21,4,58,0,0,0,255,255, 
  255,255,4,0,0,0,2,0,0,0,40,16,1,128,45,144,1,128,95,144,1,128,47,80,193,127,3,17,1,161,183,0,0,1,3,17,1,225,167,0,0,1,3,17,1,121,169,0,0,1,21,2,66,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,3,17,1,121,169,0,0,1,2,21,4,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,119,208,0,128,47,16,193,127,3,17,1,66,253,0,0,1,3,17,1,225,167,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129, 
  128,5,48,1,128,6,240,0,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,255,178,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,97, 
  253,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,128,253,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17, 
  1,159,253,0,0,1,2,18,61,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,114,208,0,128,47,16,1,128,3,17,1,250,253,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128, 
  3,17,1,25,254,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,56,254,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176, 
  0,128,3,17,1,87,254,0,0,1,2,18,60,0,0,0,1,21,4,106,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,144,2,128,116,80,2,128,58,208,2,128,43,16,3,128,60,16,66,127,45,144,66,128,125, 
  144,1,128,47,208,65,126,3,17,1,111,180,0,0,1,3,17,1,225,167,0,0,1,3,17,1,139,178,0,0,1,3,17,1,254,254,0,0,1,3,17,1,181,178,0,0,1,3,17,1,102,178,0,0,1,3,17,1,65,178, 
  0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,18,57, 
  0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,181,178,0,0,1,3,17,1,121,179,0,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,181,178,0,0,1,1,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144, 
  2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,102,0,1,0,1,3,17,1,40,0,1,0,1,3,17,1,241,255,0,0,1,3,17,1,206,172,0,0,1,3,17,1,107,172,0,0,1,3,17,1,184,171,0, 
  0,1,3,17,1,23,171,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1, 
  2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,111,48,129,128,97,112,193,127,117,240,0,128,3,17,1,64,174,0,0,1,3,17,1,27,174,0,0,1,3,17,1,147,173,0,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,71,0,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,206,174,0,0,1,2,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,160,175,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,218,0, 
  1,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,249,0,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,24,1,1,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,55,1,1,0,1,2,18,13,0,0,0,1,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,112,240,0,128,47,112,65,128,103, 
  48,1,128,3,17,1,158,1,1,0,1,3,17,1,107,172,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0, 
  1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,64,174,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116, 
  16,1,128,45,208,1,128,99,80,1,128,47,144,193,127,3,17,1,15,3,1,0,1,3,17,1,172,2,1,0,1,3,17,1,225,167,0,0,1,3,17,1,42,2,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,73,2,1,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,104,2,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,135,2,1, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,17,1,166,2,1,0,1,2,18,72,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176, 
  0,128,3,17,1,203,2,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,234,2,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  114,176,0,128,3,17,1,9,3,1,0,1,2,18,71,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,46,3,1,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,77,3,1,0,1,2,18,47,0,0,0,1,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,240,0,128,47,112,65,128,99,48,1,128,3,17,1, 
  15,3,1,0,1,3,17,1,172,2,1,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169, 
  0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,47,16,1,128,3,17,1,243,205,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,80,48,129,128,45,240,1,128,84,112,1,128,47, 
  176,65,128,95,240,1,128,3,17,1,42,7,1,0,1,3,17,1,134,4,1,0,1,3,17,1,225,167,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128, 
  128,45,48,193,127,95,48,1,128,3,17,1,220,4,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1, 
  18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,50,5,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1, 
  128,3,17,1,136,5,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,240,128,128,45,48,193,127,95,48,1,128,3,17,1,222,5,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,52,6,1,0, 
  1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,138,6,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,76,240,0,128,45,48,65,128,95,48,1,128,3,17,1,224,6,1,0,1,3,17,1,121,169,0,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,96,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,128,7,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,214,7,1,0,1,3, 
  17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,68,240,0,128,45,48,65,128,95,48,1,128,3,17,1,44,8,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,85,240,128,128,45,48,193,127,95,48,1,128,3,17,1,130,8,1,0,1,3,17,1,121,169,0,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,67,240,128, 
  128,45,48,193,127,95,48,1,128,3,17,1,216,8,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1, 
  18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,46,9,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,240,128,128,45,48,193,127,95,48,1, 
  128,3,17,1,132,9,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,218,9,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,48,10,1,0, 
  1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,95,0,0,0,21,4,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,21,4, 
  66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,208,1,128,123,16,1,128,58,80,1,128,47,144,129,127,3,17,1,237,191,0,0,1,3,17,1,243,205,0,0,1,3,17,1,225,167,0,0,1,3,17,1,161, 
  183,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0, 
  0,2,0,0,0,40,48,66,128,100,176,1,128,102,112,1,128,47,240,65,128,123,48,1,128,3,17,1,237,191,0,0,1,3,17,1,32,12,1,0,1,3,17,1,96,11,1,0,1,3,17,1,225,167,0,0,1,3,17,1, 
  161,183,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,101,176,0,128,3,17,1,127,11,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,158,11,1,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,189,11,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,220,11,1,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,251,11,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,26,12,1,0,1,2,18,115,0,0,0,1, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,63,12,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,94,12,1,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,125,12,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,156,12, 
  1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,187,12,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1, 
  218,12,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,249,12,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3, 
  17,1,24,13,1,0,1,2,18,116,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,208,0,128,47,16,193,127,3,17,1,111,180,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0, 
  128,47,16,193,127,3,17,1,237,191,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169, 
  0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,1,128,47,208,0,128,3,17,1,225,167,0,0,1,3,17,1,140,185,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,144,2,129,125,80,1,128,102,208,1,128,47, 
  80,130,128,100,16,2,128,123,144,1,128,3,17,1,111,180,0,0,1,3,17,1,237,191,0,0,1,3,17,1,32,12,1,0,1,3,17,1,96,11,1,0,1,3,17,1,225,167,0,0,1,3,17,1,161,183,0,0,1,21, 
  2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  124,240,0,128,41,112,65,128,47,48,1,128,3,17,1,223,200,0,0,1,3,17,1,225,167,0,0,1,3,17,1,229,200,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16, 
  1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,112,1,128,47,48,65,128,123,240,0,128,3,17,1,237,191,0,0,1,3,17,1, 
  225,167,0,0,1,3,17,1,161,183,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,58,0, 
  0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,16,1,128,45,144,1,128,95,144,1,128,47,80,193,127,3,17,1,161,183,0,0,1,3,17,1,225,167,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,47,208,0,128,41,16,193,127,3,17,1,225,167,0,0,1,3,17,1,229,200,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1, 
  201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,116,48,1,128,47,176,193,128,34,48,2,128,39,240,129,127,99,112,1,128,3,17,1,163,16,1,0,1, 
  3,17,1,219,209,0,0,1,3,17,1,225,167,0,0,1,3,17,1,108,187,0,0,1,3,17,1,140,185,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17, 
  1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,194,16,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,58,176,0,128,3,17,1,0,185,0,0,1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,48,1,128,45,240,193,128,58,176,1,128,95,240,1,128,125,112,1,128,3,17,1,161,183, 
  0,0,1,3,17,1,111,180,0,0,1,3,17,1,129,197,0,0,1,3,17,1,181,178,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1, 
  201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,181,178,0,0,1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,48,1,128,45,240,193,128,95,240,1,128,47,176,193,127,125,112,1,128, 
  3,17,1,161,183,0,0,1,3,17,1,111,180,0,0,1,3,17,1,225,167,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48, 
  1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,118,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,69,176,1,128,73,176,2,128,58,240,2,128,43,48,3, 
  128,60,112,2,128,45,112,195,126,78,48,2,128,47,240,65,128,95,112,3,128,3,17,1,122,23,1,0,1,3,17,1,225,167,0,0,1,3,17,1,46,22,1,0,1,3,17,1,139,178,0,0,1,3,17,1,136,18,1,0, 
  1,3,17,1,102,178,0,0,1,3,17,1,65,178,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169, 
  0,0,1,3,17,1,195,169,0,0,1,3,17,1,121,169,0,0,1,2,18,57,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,77,80,1,128,45,144,193,127,95,144,1,128,71,16,193,127,3, 
  17,1,140,20,1,0,1,3,17,1,234,18,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57, 
  0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,64,19,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3, 
  17,1,150,19,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,236,19,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,66,20,1,0,1,3, 
  17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,78,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0, 
  0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,226,20,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1, 
  56,21,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,142,21,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0, 
  128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,228,21,1,0,1,3,17,1, 
  121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,80,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21, 
  4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,132,22,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1,128,3,17,1,218,22, 
  1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,48,23,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3, 
  208,0,128,3,17,1,121,169,0,0,1,1,18,79,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88,240,0,128,45,48,65,128,95,48,1, 
  128,3,17,1,208,23,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,38,24,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,124,24,1,0, 
  1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,210,24,1,0,1,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0, 
  128,3,17,1,121,169,0,0,1,1,18,57,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,40,25,1,0,1,3,17,1,121,169,0,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,18,77,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,17,1,121,169,0,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,121,169,0,0,1,1,21,4,42,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,62,208,0,128,47,16,1,128,3,17,1,166,197,0,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17, 
  1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,44,112,129,128,47,48,1,128,62,240,0,128,3,17,1,166,197,0,0,1,3,17,1,225,167,0,0, 
  1,3,17,1,123,197,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,142,0,0,0,255,255, 
  255,255,11,0,0,0,3,0,0,0,69,112,2,128,65,48,194,129,58,240,3,128,43,48,4,128,60,48,3,128,45,112,195,126,78,176,2,128,47,240,129,128,73,240,130,128,95,112,3,128,97,176,3,128,3,17,1,225,167,0, 
  0,1,3,17,1,74,251,0,0,1,3,17,1,82,249,0,0,1,3,17,1,6,248,0,0,1,3,17,1,96,244,0,0,1,3,17,1,139,178,0,0,1,3,17,1,181,178,0,0,1,3,17,1,192,243,0,0,1,3,17, 
  1,102,178,0,0,1,3,17,1,65,178,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1, 
  3,17,1,181,178,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,90,27,1,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1, 
  128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,150,27,1,0,1,3,17,1,144,27,1,0,1,3,17,1,138,27,1,0,1,2,18,18, 
  0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,60,169,0,0,1,3,17,1,12,168,0,0,1,1,18,18,0,0,0,1,18,16,0,0,0,1,18,17, 
  0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,90,27,1,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128, 
  2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,3,17,1,150,27,1,0,1,3,17,1,144,27,1,0,1,3,17,1,138,27,1,0,1,2,21,4,54,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,47,112,193,127,97,240,0,128,3,17,1,143,28,1,0,1,3,17,1,106,28,1,0,1,3,17,1,225,167,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,83,176,0,128,3,17,1,137,28,1,0,1, 
  2,18,75,0,0,0,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,174,28,1,0,1,2,18,76,0,0,0,1,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0, 
  0,0,116,144,1,128,125,80,1,128,34,144,2,128,39,80,66,128,47,16,66,128,99,208,1,128,3,17,1,111,180,0,0,1,3,17,1,163,16,1,0,1,3,17,1,219,209,0,0,1,3,17,1,225,167,0,0,1,3,17, 
  1,108,187,0,0,1,3,17,1,140,185,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,201,169,0,0,1,3,17,1,195,169,0,0,1,2,21,4,82, 
  0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,60,144,1,128,45,80,2,128,58,208,1,128,43,16,66,128,47,80,65,128,95,80,2,128,3,17,1,225,167,0,0,1,3,17,1,139,178,0,0,1,3,17,1,102,178, 
  0,0,1,3,17,1,65,178,0,0,1,3,17,1,121,169,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,201,169,0,0,1,3,17,1, 
  195,169,0,0,1,3,17,1,121,169,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,11,30,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,4,208,0,128,3,16,1,128,3,17,1,244,32,1,0,1,3,17,1,201,31,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,11,30,1,0,1,21,2,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,127,31,1,0,1,3,17,1,84,30,1,0,1,2,18,27,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,208,192,127,3,17,1,158,30,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,158,30,1,0,1,1,18,27,0,0,0,21, 
  4,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,17,1,230,30,1,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,17,1, 
  230,30,1,0,1,2,18,27,0,0,0,17,1,242,30,1,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,55,31,1,0,1,21,2,34,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,55,31,1,0,1,2,18,27,0,0,0,21,4,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,17,1, 
  230,30,1,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,17,1,230,30,1,0,1,2,18,27,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,95,208,0,128,45,208,192,127,3,17,1,158,30,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,158,30,1,0,1,1,18,27,0,0,0,21,4, 
  34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,19,32,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17, 
  1,19,32,1,0,1,1,18,27,0,0,0,21,4,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,17,1,91,32,1,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,17,1,91,32,1,0,1,2,18,27,0,0,0,17,1,103,32,1,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1, 
  172,32,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,172,32,1,0,1,2,18,27,0,0,0,21,4,33,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,208,192,127,17,1,91,32,1,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,17,1,91,32,1,0,1,2,18,28,0,0,0,21,2,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,244,32,1,0,1,1,21,5,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,1,176,0,128,3,17,1,55,33,1,0,1,2,18, 
  8,0,0,0,1, 
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
    parser.init_parser(36004);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0.into_strings();
    Ok(obj_0_0)
  }
  
  pub fn grammar_from<'a> (mut reader: UTF8StringReader)-> Result<Box<Grammar>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(36776);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_Grammar();
    Ok(obj_0_0)
  }
  
  pub fn type_eval_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(42190);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_expression_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(42368);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_struct_from<'a> (mut reader: UTF8StringReader)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(42805);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_AST_Struct();
    Ok(obj_0_0)
  }
}