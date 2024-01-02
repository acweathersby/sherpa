
/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.1-beta2
/// - **SOURCE**: /home/work/projects/lib_radlr/grammars/v2_0_0/grammar.sg
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
use radlr_rust_runtime::{
  llvm_parser::*,
  types::{ast::*, Token, TokenRange}, deprecate::*,
};



impl State{
  /// Create a [State] node from a `String` input.
  
  pub fn from_string (input: String)-> Result<Box<State>, RadlrParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ir_from(reader)
  }
}

impl State{
  /// Create a [State] node from a `String` input.
  
  pub fn from_str (input: &str)-> Result<Box<State>, RadlrParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ir_from(reader)
  }
}

impl Grammar{
  /// Create a [Grammar] node from a `String` input.
  
  pub fn from_string (input: String)-> Result<Box<Grammar>, RadlrParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::grammar_from(reader)
  }
}

impl Grammar{
  /// Create a [Grammar] node from a `String` input.
  
  pub fn from_str (input: &str)-> Result<Box<Grammar>, RadlrParseError> {
    let reader = UTF8StringReader::from(input);
    ast::grammar_from(reader)
  }
}

impl AST_Struct{
  /// Create a [AST_Struct] node from a `String` input.
  
  pub fn from_string (input: String)-> Result<Box<AST_Struct>, RadlrParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ast_struct_from(reader)
  }
}

impl AST_Struct{
  /// Create a [AST_Struct] node from a `String` input.
  
  pub fn from_str (input: &str)-> Result<Box<AST_Struct>, RadlrParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ast_struct_from(reader)
  }
}

pub trait ASTParse<T>{
  fn ir_from(input:T) -> Result<Box<State>, RadlrParseError>;
  fn escaped_from(input:T) -> Result<Vec<String>, RadlrParseError>;
  fn grammar_from(input:T) -> Result<Box<Grammar>, RadlrParseError>;
  fn type_eval_from(input:T) -> Result<ASTNode, RadlrParseError>;
  fn ast_expression_from(input:T) -> Result<ASTNode, RadlrParseError>;
  fn ast_struct_from(input:T) -> Result<Box<AST_Struct>, RadlrParseError>;
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

#[derive(Clone, Debug)]
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
  AST_Statement(Box<AST_Statement>),
  AST_Add(Box<AST_Add>),
  Precedence(Box<Precedence>),
  NonTermMatch(Box<NonTermMatch>),
  TokenGroupRules(Box<TokenGroupRules>),
  SetTokenId(Box<SetTokenId>),
  TerminalToken(Box<TerminalToken>),
  AST_Bool(Box<AST_Bool>),
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
  AST_Pow(Box<AST_Pow>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol>),
  AST_Member(Box<AST_Member>),
  AST_F16(Box<AST_F16>),
  AST_I32(Box<AST_I32>),
  Goto(Box<Goto>),
  AST_I8(Box<AST_I8>),
  TemplateASTType(Box<TemplateASTType>),
  AST_F32(Box<AST_F32>),
  Gotos(Box<Gotos>),
  TerminalMatches(Box<TerminalMatches>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol>),
  AST_Mod(Box<AST_Mod>),
  AST_Token(Box<AST_Token>),
  Shift(Box<Shift>),
  Export(Box<Export>),
  Name(Box<Name>),
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol>),
  Fail(Box<Fail>),
  AST_NumberLiteral(Box<AST_NumberLiteral>),
  TemplateRules(Box<TemplateRules>),
  AST_U8(Box<AST_U8>),
  AST_U128(Box<AST_U128>),
  Ignore(Box<Ignore>),
  AST_U32(Box<AST_U32>),
  State(Box<State>),
  AST_BoolLiteral(Box<AST_BoolLiteral>),
  List_Rules(Box<List_Rules>),
  AST_Neg(Box<AST_Neg>),
  AST_String(Box<AST_String>),
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
  AST_Struct(Box<AST_Struct>),
  Peek(Box<Peek>),
  Matches(Box<Matches>),
  AST_Mul(Box<AST_Mul>),
  ProductionMatches(Box<ProductionMatches>),
  TermMatch(Box<TermMatch>),
  DEFINED_TYPE_NUM(Box<DEFINED_TYPE_NUM>),
  AST_Div(Box<AST_Div>),
  Rule(Box<Rule>),
  AST_Sub(Box<AST_Sub>),
  AnnotatedSymbol(Box<AnnotatedSymbol>),
  AST_Property(Box<AST_Property>),
  Ascript(Box<Ascript>),
  AST_F128(Box<AST_F128>),
  AST_StringLiteral(Box<AST_StringLiteral>),
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
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
  AST_Statement,
  AST_Add,
  Precedence,
  NonTermMatch,
  TokenGroupRules,
  SetTokenId,
  TerminalToken,
  AST_Bool,
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
  AST_Pow,
  Template_NonTerminal_Symbol,
  AST_Member,
  AST_F16,
  AST_I32,
  Goto,
  AST_I8,
  TemplateASTType,
  AST_F32,
  Gotos,
  TerminalMatches,
  NonTerminal_Import_Symbol,
  AST_Mod,
  AST_Token,
  Shift,
  Export,
  Name,
  NonTerminal_Terminal_Symbol,
  Fail,
  AST_NumberLiteral,
  TemplateRules,
  AST_U8,
  AST_U128,
  Ignore,
  AST_U32,
  State,
  AST_BoolLiteral,
  List_Rules,
  AST_Neg,
  AST_String,
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
  AST_Struct,
  Peek,
  Matches,
  AST_Mul,
  ProductionMatches,
  TermMatch,
  DEFINED_TYPE_NUM,
  AST_Div,
  Rule,
  AST_Sub,
  AnnotatedSymbol,
  AST_Property,
  Ascript,
  AST_F128,
  AST_StringLiteral,
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
      ASTNode::AST_Statement(node) => node.tok.clone(),
      ASTNode::AST_Add(node) => node.tok.clone(),
      ASTNode::TokenGroupRules(node) => node.tok.clone(),
      ASTNode::SetTokenId(node) => node.tok.clone(),
      ASTNode::TerminalToken(node) => node.tok.clone(),
      ASTNode::AST_Bool(node) => node.tok.clone(),
      ASTNode::Pop(node) => node.tok.clone(),
      ASTNode::AST_Vector(node) => node.tok.clone(),
      ASTNode::Grammar(node) => node.tok.clone(),
      ASTNode::AppendRules(node) => node.tok.clone(),
      ASTNode::AST_U64(node) => node.tok.clone(),
      ASTNode::NonTerminal_Symbol(node) => node.tok.clone(),
      ASTNode::Pass(node) => node.tok.clone(),
      ASTNode::Import(node) => node.tok.clone(),
      ASTNode::AST_IndexReference(node) => node.tok.clone(),
      ASTNode::AST_Pow(node) => node.tok.clone(),
      ASTNode::Template_NonTerminal_Symbol(node) => node.tok.clone(),
      ASTNode::AST_F16(node) => node.tok.clone(),
      ASTNode::AST_I32(node) => node.tok.clone(),
      ASTNode::Goto(node) => node.tok.clone(),
      ASTNode::AST_I8(node) => node.tok.clone(),
      ASTNode::AST_F32(node) => node.tok.clone(),
      ASTNode::NonTerminal_Import_Symbol(node) => node.tok.clone(),
      ASTNode::AST_Mod(node) => node.tok.clone(),
      ASTNode::Shift(node) => node.tok.clone(),
      ASTNode::NonTerminal_Terminal_Symbol(node) => node.tok.clone(),
      ASTNode::Fail(node) => node.tok.clone(),
      ASTNode::TemplateRules(node) => node.tok.clone(),
      ASTNode::AST_U8(node) => node.tok.clone(),
      ASTNode::AST_U128(node) => node.tok.clone(),
      ASTNode::AST_U32(node) => node.tok.clone(),
      ASTNode::State(node) => node.tok.clone(),
      ASTNode::List_Rules(node) => node.tok.clone(),
      ASTNode::AST_Neg(node) => node.tok.clone(),
      ASTNode::AST_String(node) => node.tok.clone(),
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
      ASTNode::AST_Mul(node) => node.tok.clone(),
      ASTNode::AST_Div(node) => node.tok.clone(),
      ASTNode::Rule(node) => node.tok.clone(),
      ASTNode::AST_Sub(node) => node.tok.clone(),
      ASTNode::AnnotatedSymbol(node) => node.tok.clone(),
      ASTNode::AST_Property(node) => node.tok.clone(),
      ASTNode::Ascript(node) => node.tok.clone(),
      ASTNode::AST_F128(node) => node.tok.clone(),
      ASTNode::AST_StringLiteral(node) => node.tok.clone(),
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
      ASTNode::AST_Statement(..) => ASTNodeType::AST_Statement,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::Precedence(..) => ASTNodeType::Precedence,
      ASTNode::NonTermMatch(..) => ASTNodeType::NonTermMatch,
      ASTNode::TokenGroupRules(..) => ASTNodeType::TokenGroupRules,
      ASTNode::SetTokenId(..) => ASTNodeType::SetTokenId,
      ASTNode::TerminalToken(..) => ASTNodeType::TerminalToken,
      ASTNode::AST_Bool(..) => ASTNodeType::AST_Bool,
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
      ASTNode::AST_Pow(..) => ASTNodeType::AST_Pow,
      ASTNode::Template_NonTerminal_Symbol(..) => ASTNodeType::Template_NonTerminal_Symbol,
      ASTNode::AST_Member(..) => ASTNodeType::AST_Member,
      ASTNode::AST_F16(..) => ASTNodeType::AST_F16,
      ASTNode::AST_I32(..) => ASTNodeType::AST_I32,
      ASTNode::Goto(..) => ASTNodeType::Goto,
      ASTNode::AST_I8(..) => ASTNodeType::AST_I8,
      ASTNode::TemplateASTType(..) => ASTNodeType::TemplateASTType,
      ASTNode::AST_F32(..) => ASTNodeType::AST_F32,
      ASTNode::Gotos(..) => ASTNodeType::Gotos,
      ASTNode::TerminalMatches(..) => ASTNodeType::TerminalMatches,
      ASTNode::NonTerminal_Import_Symbol(..) => ASTNodeType::NonTerminal_Import_Symbol,
      ASTNode::AST_Mod(..) => ASTNodeType::AST_Mod,
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Shift(..) => ASTNodeType::Shift,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::NonTerminal_Terminal_Symbol(..) => ASTNodeType::NonTerminal_Terminal_Symbol,
      ASTNode::Fail(..) => ASTNodeType::Fail,
      ASTNode::AST_NumberLiteral(..) => ASTNodeType::AST_NumberLiteral,
      ASTNode::TemplateRules(..) => ASTNodeType::TemplateRules,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::AST_U128(..) => ASTNodeType::AST_U128,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::State(..) => ASTNodeType::State,
      ASTNode::AST_BoolLiteral(..) => ASTNodeType::AST_BoolLiteral,
      ASTNode::List_Rules(..) => ASTNodeType::List_Rules,
      ASTNode::AST_Neg(..) => ASTNodeType::AST_Neg,
      ASTNode::AST_String(..) => ASTNodeType::AST_String,
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
      ASTNode::AST_Struct(..) => ASTNodeType::AST_Struct,
      ASTNode::Peek(..) => ASTNodeType::Peek,
      ASTNode::Matches(..) => ASTNodeType::Matches,
      ASTNode::AST_Mul(..) => ASTNodeType::AST_Mul,
      ASTNode::ProductionMatches(..) => ASTNodeType::ProductionMatches,
      ASTNode::TermMatch(..) => ASTNodeType::TermMatch,
      ASTNode::DEFINED_TYPE_NUM(..) => ASTNodeType::DEFINED_TYPE_NUM,
      ASTNode::AST_Div(..) => ASTNodeType::AST_Div,
      ASTNode::Rule(..) => ASTNodeType::Rule,
      ASTNode::AST_Sub(..) => ASTNodeType::AST_Sub,
      ASTNode::AnnotatedSymbol(..) => ASTNodeType::AnnotatedSymbol,
      ASTNode::AST_Property(..) => ASTNodeType::AST_Property,
      ASTNode::Ascript(..) => ASTNodeType::Ascript,
      ASTNode::AST_F128(..) => ASTNodeType::AST_F128,
      ASTNode::AST_StringLiteral(..) => ASTNodeType::AST_StringLiteral,
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
      AST_Statement(node) => node.hash(hasher),
      AST_Add(node) => node.hash(hasher),
      Precedence(node) => node.hash(hasher),
      NonTermMatch(node) => node.hash(hasher),
      TokenGroupRules(node) => node.hash(hasher),
      SetTokenId(node) => node.hash(hasher),
      TerminalToken(node) => node.hash(hasher),
      AST_Bool(node) => node.hash(hasher),
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
      AST_Pow(node) => node.hash(hasher),
      Template_NonTerminal_Symbol(node) => node.hash(hasher),
      AST_Member(node) => node.hash(hasher),
      AST_F16(node) => node.hash(hasher),
      AST_I32(node) => node.hash(hasher),
      Goto(node) => node.hash(hasher),
      AST_I8(node) => node.hash(hasher),
      TemplateASTType(node) => node.hash(hasher),
      AST_F32(node) => node.hash(hasher),
      Gotos(node) => node.hash(hasher),
      TerminalMatches(node) => node.hash(hasher),
      NonTerminal_Import_Symbol(node) => node.hash(hasher),
      AST_Mod(node) => node.hash(hasher),
      AST_Token(node) => node.hash(hasher),
      Shift(node) => node.hash(hasher),
      Export(node) => node.hash(hasher),
      Name(node) => node.hash(hasher),
      NonTerminal_Terminal_Symbol(node) => node.hash(hasher),
      Fail(node) => node.hash(hasher),
      AST_NumberLiteral(node) => node.hash(hasher),
      TemplateRules(node) => node.hash(hasher),
      AST_U8(node) => node.hash(hasher),
      AST_U128(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      AST_U32(node) => node.hash(hasher),
      State(node) => node.hash(hasher),
      AST_BoolLiteral(node) => node.hash(hasher),
      List_Rules(node) => node.hash(hasher),
      AST_Neg(node) => node.hash(hasher),
      AST_String(node) => node.hash(hasher),
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
      AST_Struct(node) => node.hash(hasher),
      Peek(node) => node.hash(hasher),
      Matches(node) => node.hash(hasher),
      AST_Mul(node) => node.hash(hasher),
      ProductionMatches(node) => node.hash(hasher),
      TermMatch(node) => node.hash(hasher),
      DEFINED_TYPE_NUM(node) => node.hash(hasher),
      AST_Div(node) => node.hash(hasher),
      Rule(node) => node.hash(hasher),
      AST_Sub(node) => node.hash(hasher),
      AnnotatedSymbol(node) => node.hash(hasher),
      AST_Property(node) => node.hash(hasher),
      Ascript(node) => node.hash(hasher),
      AST_F128(node) => node.hash(hasher),
      AST_StringLiteral(node) => node.hash(hasher),
      
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Statement{
  pub expression:ASTNode, 
  pub tok: Token, 
}

impl AST_Statement{
  
  pub fn new (expression: ASTNode, tok: Token)-> Self {
    
    Self{
      expression,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Statement
  }
}

impl ASTNode{
  
  pub fn to_AST_Statement (self)-> Box::<AST_Statement> {
    
    match self{
      Self::AST_Statement(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Statement (&self)-> Option<&AST_Statement> {
    
    match self{
      Self::AST_Statement(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Statement_mut (&mut self)-> Option<&mut AST_Statement> {
    
    match self{
      Self::AST_Statement(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Statement{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expression.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Bool{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_Bool{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Bool
  }
}

impl ASTNode{
  
  pub fn to_AST_Bool (self)-> Box::<AST_Bool> {
    
    match self{
      Self::AST_Bool(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Bool (&self)-> Option<&AST_Bool> {
    
    match self{
      Self::AST_Bool(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Bool_mut (&mut self)-> Option<&mut AST_Bool> {
    
    match self{
      Self::AST_Bool(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Bool{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Pow{
  pub left:ASTNode, 
  pub right:ASTNode, 
  pub tok: Token, 
}

impl AST_Pow{
  
  pub fn new (left: ASTNode, right: ASTNode, tok: Token)-> Self {
    
    Self{
      left,
      right,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Pow
  }
}

impl ASTNode{
  
  pub fn to_AST_Pow (self)-> Box::<AST_Pow> {
    
    match self{
      Self::AST_Pow(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Pow (&self)-> Option<&AST_Pow> {
    
    match self{
      Self::AST_Pow(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Pow_mut (&mut self)-> Option<&mut AST_Pow> {
    
    match self{
      Self::AST_Pow(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Pow{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_F16{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_F16{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_F16
  }
}

impl ASTNode{
  
  pub fn to_AST_F16 (self)-> Box::<AST_F16> {
    
    match self{
      Self::AST_F16(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_F16 (&self)-> Option<&AST_F16> {
    
    match self{
      Self::AST_F16(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_F16_mut (&mut self)-> Option<&mut AST_F16> {
    
    match self{
      Self::AST_F16(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_F16{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Mod{
  pub left:ASTNode, 
  pub right:ASTNode, 
  pub tok: Token, 
}

impl AST_Mod{
  
  pub fn new (left: ASTNode, right: ASTNode, tok: Token)-> Self {
    
    Self{
      left,
      right,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Mod
  }
}

impl ASTNode{
  
  pub fn to_AST_Mod (self)-> Box::<AST_Mod> {
    
    match self{
      Self::AST_Mod(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Mod (&self)-> Option<&AST_Mod> {
    
    match self{
      Self::AST_Mod(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Mod_mut (&mut self)-> Option<&mut AST_Mod> {
    
    match self{
      Self::AST_Mod(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Mod{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_NumberLiteral{
  pub value:f64, 
}

impl AST_NumberLiteral{
  
  pub fn new (value: f64)-> Self {
    
    Self{
      value,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_NumberLiteral
  }
}

impl ASTNode{
  
  pub fn to_AST_NumberLiteral (self)-> Box::<AST_NumberLiteral> {
    
    match self{
      Self::AST_NumberLiteral(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_NumberLiteral (&self)-> Option<&AST_NumberLiteral> {
    
    match self{
      Self::AST_NumberLiteral(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_NumberLiteral_mut (&mut self)-> Option<&mut AST_NumberLiteral> {
    
    match self{
      Self::AST_NumberLiteral(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_NumberLiteral{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.to_le_bytes().hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_U128{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_U128{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_U128
  }
}

impl ASTNode{
  
  pub fn to_AST_U128 (self)-> Box::<AST_U128> {
    
    match self{
      Self::AST_U128(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_U128 (&self)-> Option<&AST_U128> {
    
    match self{
      Self::AST_U128(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_U128_mut (&mut self)-> Option<&mut AST_U128> {
    
    match self{
      Self::AST_U128(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_U128{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_BoolLiteral{
  pub value:bool, 
}

impl AST_BoolLiteral{
  
  pub fn new (value: bool)-> Self {
    
    Self{
      value,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_BoolLiteral
  }
}

impl ASTNode{
  
  pub fn to_AST_BoolLiteral (self)-> Box::<AST_BoolLiteral> {
    
    match self{
      Self::AST_BoolLiteral(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_BoolLiteral (&self)-> Option<&AST_BoolLiteral> {
    
    match self{
      Self::AST_BoolLiteral(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_BoolLiteral_mut (&mut self)-> Option<&mut AST_BoolLiteral> {
    
    match self{
      Self::AST_BoolLiteral(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_BoolLiteral{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Neg{
  pub expr:ASTNode, 
  pub tok: Token, 
}

impl AST_Neg{
  
  pub fn new (expr: ASTNode, tok: Token)-> Self {
    
    Self{
      expr,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Neg
  }
}

impl ASTNode{
  
  pub fn to_AST_Neg (self)-> Box::<AST_Neg> {
    
    match self{
      Self::AST_Neg(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Neg (&self)-> Option<&AST_Neg> {
    
    match self{
      Self::AST_Neg(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Neg_mut (&mut self)-> Option<&mut AST_Neg> {
    
    match self{
      Self::AST_Neg(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Neg{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expr.hash(hasher);
  }
}

#[derive(Clone, Debug)]
pub struct AST_String{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_String{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_String
  }
}

impl ASTNode{
  
  pub fn to_AST_String (self)-> Box::<AST_String> {
    
    match self{
      Self::AST_String(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_String (&self)-> Option<&AST_String> {
    
    match self{
      Self::AST_String(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_String_mut (&mut self)-> Option<&mut AST_String> {
    
    match self{
      Self::AST_String(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_String{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct NotEmptySet{
  pub allow_empty:bool, 
  pub symbols:Vec<ASTNode>, 
  pub unordered:bool, 
  pub tok: Token, 
}

impl NotEmptySet{
  
  pub fn new (allow_empty: bool, symbols: Vec<ASTNode>, unordered: bool, tok: Token)-> Self {
    
    Self{
      allow_empty,
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
    self.allow_empty.hash(hasher);
    
    for val in &self.symbols{
      val.hash(hasher);
    }
    self.unordered.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Mul{
  pub left:ASTNode, 
  pub right:ASTNode, 
  pub tok: Token, 
}

impl AST_Mul{
  
  pub fn new (left: ASTNode, right: ASTNode, tok: Token)-> Self {
    
    Self{
      left,
      right,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Mul
  }
}

impl ASTNode{
  
  pub fn to_AST_Mul (self)-> Box::<AST_Mul> {
    
    match self{
      Self::AST_Mul(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Mul (&self)-> Option<&AST_Mul> {
    
    match self{
      Self::AST_Mul(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Mul_mut (&mut self)-> Option<&mut AST_Mul> {
    
    match self{
      Self::AST_Mul(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Mul{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Div{
  pub left:ASTNode, 
  pub right:ASTNode, 
  pub tok: Token, 
}

impl AST_Div{
  
  pub fn new (left: ASTNode, right: ASTNode, tok: Token)-> Self {
    
    Self{
      left,
      right,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Div
  }
}

impl ASTNode{
  
  pub fn to_AST_Div (self)-> Box::<AST_Div> {
    
    match self{
      Self::AST_Div(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Div (&self)-> Option<&AST_Div> {
    
    match self{
      Self::AST_Div(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Div_mut (&mut self)-> Option<&mut AST_Div> {
    
    match self{
      Self::AST_Div(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Div{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_Sub{
  pub left:ASTNode, 
  pub right:ASTNode, 
  pub tok: Token, 
}

impl AST_Sub{
  
  pub fn new (left: ASTNode, right: ASTNode, tok: Token)-> Self {
    
    Self{
      left,
      right,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_Sub
  }
}

impl ASTNode{
  
  pub fn to_AST_Sub (self)-> Box::<AST_Sub> {
    
    match self{
      Self::AST_Sub(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_Sub (&self)-> Option<&AST_Sub> {
    
    match self{
      Self::AST_Sub(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_Sub_mut (&mut self)-> Option<&mut AST_Sub> {
    
    match self{
      Self::AST_Sub(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_Sub{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AST_F128{
  pub initializer:Option<Box<Init>>, 
  pub tok: Token, 
}

impl AST_F128{
  
  pub fn new (initializer: Option<Box<Init>>, tok: Token)-> Self {
    
    Self{
      initializer,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_F128
  }
}

impl ASTNode{
  
  pub fn to_AST_F128 (self)-> Box::<AST_F128> {
    
    match self{
      Self::AST_F128(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_F128 (&self)-> Option<&AST_F128> {
    
    match self{
      Self::AST_F128(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_F128_mut (&mut self)-> Option<&mut AST_F128> {
    
    match self{
      Self::AST_F128(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_F128{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Clone, Debug)]
pub struct AST_StringLiteral{
  pub value:String, 
  pub tok: Token, 
}

impl AST_StringLiteral{
  
  pub fn new (value: String, tok: Token)-> Self {
    
    Self{
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_StringLiteral
  }
}

impl ASTNode{
  
  pub fn to_AST_StringLiteral (self)-> Box::<AST_StringLiteral> {
    
    match self{
      Self::AST_StringLiteral(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AST_StringLiteral (&self)-> Option<&AST_StringLiteral> {
    
    match self{
      Self::AST_StringLiteral(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_StringLiteral_mut (&mut self)-> Option<&mut AST_StringLiteral> {
    
    match self{
      Self::AST_StringLiteral(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_StringLiteral{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
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


/* expr */
fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" type_identifier^t ( "," struct_prop(+",") )? '}'
        :ast { t_AST_Struct, typ: str($t), props:$3, tok } */
fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* struct_prop(+",") */
fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* tok::id

            :ast { t_NonTerminal_Symbol, name:str($1), tok} */
fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_048 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_049 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then") */
fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "then" pop^pop */
fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "then" branch_statement^branch */
fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* non_branch_statement */
fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* non_branch_statement(+"then") */
fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "then" branch_statement^branch */
fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* pop^pop "then" */
fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:id */
fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:nl */
fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sp */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* "\\"{:9999} ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:id */
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:nl */
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sp */
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* export_clause */
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_clause */
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* name_clause */
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ignore_clause */
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<>" sym::nonterminal_symbol^n ">" rules^r

        :ast { t_CFRules, name_sym:$n, rules: $r, tok } */
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* template_param(+",") */
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:number     :ast { t_DEFINED_TYPE_NUM } */
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* expr "+"{2} expr{1} :ast { t_AST_Add, left: $1, right: $3, tok } */
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* expr "-"{2} expr{1} :ast { t_AST_Sub, left: $1, right: $3, tok } */
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = AST_Sub::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Sub(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "/"{4} expr{3} :ast { t_AST_Div, left: $1, right: $3, tok } */
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = AST_Div::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Div(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "*"{4} expr{3} :ast { t_AST_Mul, left: $1, right: $3, tok } */
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = AST_Mul::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Mul(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "%"{4} expr{3} :ast { t_AST_Mod, left: $1, right: $3, tok } */
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = AST_Mod::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Mod(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* expr "^"{6} expr{5} :ast { t_AST_Pow, left: $1, right: $3, tok } */
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_0 = ref_0;
  let obj_2_1 = ref_2;
  let var_4_0 = AST_Pow::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Pow(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* "-"{8} expr{7} :ast { t_AST_Neg, expr: $2, tok } */
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let var_3_0 = AST_Neg::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Neg(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "(" expr^expr ")"        :ast $expr */
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  slots.assign(0, AstSlot(obj_1_0, __rule_rng__, TokenRange::default()));
}


/* term{9} */
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* identifier ":" expr
        :ast { t_AST_Property, id:str($1), value:$3, tok } */
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:( 't' "_"{:9999} ) identifier */
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* tk:id_tok */
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok } */
fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = SetLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SetLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "symbols" "to" */
fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "with" "rule" */
fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "symbols" "to" */
fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast */
fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "pop" tok::int      :ast { t_Pop, count: u32($2), tok } */
fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_sequence */
fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_statement */
fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "shift" "-skip"? ( "tok" | "char" )    :ast { t_Shift, ptr_type:str($3), skip:bool($2), tok } */
fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_126 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_127 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "char" */
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


/* "char" */
fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "tok" */
fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "char" */
fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "EXPORT" sym::nonterminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, nonterminal:$2, reference:str($3) } */
fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "as" */
fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( "AS" | "as" ) tok::id */
fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* ( "AS" | "as" ) tok::id */
fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "IMPORT" ( c:id | c:sym | c:num )(+) c:sp ( "AS" | "as" ) tok::id

        :ast { t_Import, uri: str($2), reference:str($5), tok } */
fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:sym */
fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* c:num */
fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
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


/* ( c:id | c:sym | c:num ) */
fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
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


/* ( c:id | c:sym | c:num )(+) */
fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "AS" */
fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "as" */
fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "NAME" tok::id

        :ast { t_Name, name: str($2) } */
fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "IGNORE" "{"  sym::terminal(*) "}"

        :ast { t_Ignore, symbols: $3 } */
fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "IGNORE" "{"  sym::terminal(*) "}"

        :ast { t_Ignore, symbols: $3 } */
fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let var_4_0 = Ignore::new(
    vec![],
  );
  slots.assign(0, AstSlot(ASTNode::Ignore(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* sym::terminal */
fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::terminal(*) */
fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* rule */
fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_nonterminal_symbol */
fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tok::id (":" "sym")?                    :ast { t_TemplateSym,     val:str($1) } */
fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* member */
fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* map */
fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* vector */
fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* string */
fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* bool */
fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* number */
fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token::id */
fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk" */
fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "tok" */
fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "token" */
fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* struct */
fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression
        :ast { t_AST_Statement, expression:$1, tok } */
fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let var_2_0 = AST_Statement::new(
    obj_0_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statement(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "{" expression '}'
        :ast { t_AST_Statement, expression:$2, tok } */
fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1;
  let var_4_0 = AST_Statement::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statement(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* tk:int_tok */
fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* generic_match_block */
fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal_match_block */
fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_match_block */
fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") "then" goto

            :ast { t_Gotos, pushes: $1, goto } */
fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_199 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") */
fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Fail::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pass"        :ast { t_Pass, tok } */
fn reducer_202 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Pass::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "accept"      :ast { t_Accept, tok } */
fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Accept::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk:(" rules ")"{1}

        :ast { t_TokenGroupRules, rules:$2,  tok } */
fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token_non_terminal */
fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] )^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* not_empty */
fn reducer_211 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(*) */
fn reducer_214 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(*) */
fn reducer_215 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] */
fn reducer_216 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] */
fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* [ ( sym::annotated_symbol | not_empty )(*)^s sym::end_of_input?^eoi ] :ast [$s, $eoi] */
fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::id '::' tok::id

        :ast { t_NonTerminal_Import_Symbol, module:str($1), name:str($3), tok} */
fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* trimmed_reference */
fn reducer_220 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* reference "." identifier
        :ast { t_AST_Member, reference:$1, property:$3 } */
fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "map" "(" expr^k ',' expr^v ')'

        :ast { t_AST_Map, key: $k, val: $v, tok } */
fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "[" expr(*",") "]"
        :ast { t_AST_Vector, initializer: $2, tok } */
fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "[" expr(*",") "]"
        :ast { t_AST_Vector, initializer: $2, tok } */
fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let var_3_0 = AST_Vector::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* expr */
fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expr(*",") */
fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_String, initializer: $2, tok } */
fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_String::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_String(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_String, initializer: $2, tok } */
fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_String::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_String(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "bool" convert_initializer?
        :ast { t_AST_Bool,  initializer: $2, tok } */
fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_Bool::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Bool(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "bool" convert_initializer?
        :ast { t_AST_Bool,  initializer: $2, tok } */
fn reducer_230 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Bool::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Bool(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u8"  convert_initializer?
        :ast { t_AST_U8,  initializer: $2, tok } */
fn reducer_231 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_U8,  initializer: $2, tok } */
fn reducer_232 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u16" convert_initializer?
        :ast { t_AST_U16, initializer: $2, tok } */
fn reducer_233 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_U16, initializer: $2, tok } */
fn reducer_234 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u32" convert_initializer?
        :ast { t_AST_U32, initializer: $2, tok } */
fn reducer_235 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_U32, initializer: $2, tok } */
fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u64" convert_initializer?
        :ast { t_AST_U64, initializer: $2, tok } */
fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_U64, initializer: $2, tok } */
fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "u128" convert_initializer?
        :ast { t_AST_U128, initializer: $2, tok } */
fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_U128::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U128(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "u128" convert_initializer?
        :ast { t_AST_U128, initializer: $2, tok } */
fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_U128::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U128(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i8"  convert_initializer?
        :ast { t_AST_I8,  initializer: $2, tok } */
fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_I8,  initializer: $2, tok } */
fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i16" convert_initializer?
        :ast { t_AST_I16, initializer: $2, tok } */
fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_I16, initializer: $2, tok } */
fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i32" convert_initializer?
        :ast { t_AST_I32, initializer: $2, tok } */
fn reducer_245 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_I32, initializer: $2, tok } */
fn reducer_246 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i64" convert_initializer?
        :ast { t_AST_I64, initializer: $2, tok } */
fn reducer_247 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_I64, initializer: $2, tok } */
fn reducer_248 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "i128" convert_initializer?
        :ast { t_AST_I64, initializer: $2, tok } */
fn reducer_249 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "i128" convert_initializer?
        :ast { t_AST_I64, initializer: $2, tok } */
fn reducer_250 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_I64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "f16" convert_initializer?
        :ast { t_AST_F16, initializer: $2, tok } */
fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_F16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F16(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "f16" convert_initializer?
        :ast { t_AST_F16, initializer: $2, tok } */
fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_F16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F16(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "f32" convert_initializer?
        :ast { t_AST_F32, initializer: $2, tok } */
fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_F32, initializer: $2, tok } */
fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_F32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "f64" convert_initializer?
        :ast { t_AST_F64, initializer: $2, tok } */
fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        :ast { t_AST_F64, initializer: $2, tok } */
fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_F64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "f128" convert_initializer?
        :ast { t_AST_F128, initializer: $2, tok } */
fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Init();
  let var_3_0 = AST_F128::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F128(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "f128" convert_initializer?
        :ast { t_AST_F128, initializer: $2, tok } */
fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_F128::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F128(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "true" 
        :ast { t_AST_BoolLiteral, value: true } */
fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = true;
  let var_3_0 = AST_BoolLiteral::new(
    obj_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_BoolLiteral(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "false"
        :ast { t_AST_BoolLiteral, value: false } */
fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = false;
  let var_3_0 = AST_BoolLiteral::new(
    obj_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_BoolLiteral(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* token::num
        :ast { t_AST_NumberLiteral, value: f64($1) } */
fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_f64();
  let var_2_0 = AST_NumberLiteral::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_NumberLiteral(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* token::string
        :ast { t_AST_StringLiteral, value:str($1), tok } */
fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_2_0 = AST_StringLiteral::new(
    tok_0_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_StringLiteral(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "<" token::int ( ","  token::int  )? ">"
        :ast { t_Range, start_trim:i32($2), end_trim:i32($3) } */
fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* "match" ":" tok::id^id ( ":" tok::id :ast str($2) )?^scanner ( int_match :ast [$1] | "{" ( int_match | default_match | hint )(+) "}" :ast $2  )^m

        :ast { t_Matches, mode: str($id), matches:$m, scanner, tok } */
fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* int_match */
fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* default_match */
fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* hint */
fn reducer_271 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_272 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_273 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_274 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_275 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_276 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_277 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* int_match :ast [$1] */
fn reducer_278 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( int_match | default_match | hint )(+) "}" :ast $2 */
fn reducer_279 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "PRODUCTION" ( nonterminal_match :ast [$1] | "{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_ProductionMatches, matches:$m } */
fn reducer_280 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_281 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* hint */
fn reducer_282 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* default_match */
fn reducer_283 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_284 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_285 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match ) */
fn reducer_286 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_287 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_288 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( nonterminal_match | hint | default_match )(+) */
fn reducer_289 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* nonterminal_match :ast [$1] */
fn reducer_290 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_291 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "TERMINAL" ( terminal_match :ast [$1] | "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_TerminalMatches, matches:$m } */
fn reducer_292 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_293 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* hint */
fn reducer_294 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* default_match */
fn reducer_295 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_296 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_297 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_298 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_299 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_300 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_301 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* terminal_match :ast [$1] */
fn reducer_302 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_303 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "push" sym::nonterminal

    :ast { t_Push, nonterminal: $2, name:str($2), tok } */
fn reducer_304 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_305 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_306 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_307 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_308 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::nonterminal :ast { t_Goto, nonterminal: $1, name:str($1), tok } )(+) */
fn reducer_309 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_310 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_311 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_312 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_314 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_315 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_316 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_317 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_318 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_319 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_320 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'vtab' | tk:(c:id+) )

        :ast { t_ClassSymbol, val:str($2),  tok } */
fn reducer_321 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_322 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'nl' */
fn reducer_323 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'sp' */
fn reducer_324 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'id' */
fn reducer_325 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'sym' */
fn reducer_326 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'any' */
fn reducer_327 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'tab' */
fn reducer_328 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* 'vtab' */
fn reducer_329 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:(c:id+) */
fn reducer_330 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* list^s [tk:reference?^r "?" ?^o  precedence?^p ]!

        :ast { t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  } */
fn reducer_331 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_332 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_333 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_334 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_335 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_336 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_337 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_338 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_339 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_340 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_341 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_342 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_343 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_344 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_345 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_346 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "[" not_empty_set^s ']' "!"?^o  

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_347 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_1 = ref_1.into_nodes();
  let obj_5_2 = true;
  let var_6_0 = NotEmptySet::new(
    false,
    obj_1_1,
    obj_5_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "[" not_empty_set^s ']' "!"?^o  

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_348 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_1 = ref_1.into_nodes();
  let obj_4_2 = false;
  let var_5_0 = NotEmptySet::new(
    false,
    obj_1_1,
    obj_4_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "[" not_empty_set^s "]!?"^o

        :ast { t_NotEmptySet, unordered: bool($o), allow_empty: bool($o), symbols:$s, tok } */
fn reducer_349 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_1_1 = ref_1.into_nodes();
  let obj_5_2 = true;
  let var_6_0 = NotEmptySet::new(
    obj_4_0,
    obj_1_1,
    obj_5_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* "$" :ast { t_EOFSymbol, tok } */
fn reducer_350 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast

            :ast  { t_Ascript, ast:$ast, tok } */
fn reducer_351 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* reference */
fn reducer_352 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "$" token::id 
        :ast { t_AST_NamedReference, value: str($2), tok } */
fn reducer_353 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_354 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" init_objects ")"       
        :ast { t_Init, expression: $2 } */
fn reducer_355 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tk:num_tok */
fn reducer_356 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:string_tok */
fn reducer_357 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "(" ( tok::int :ast u64($1) )(+"|")^vals ")" "{" statement "}"

    :ast { t_IntMatch, vals, statement } */
fn reducer_358 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_359 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_u64();
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) ) */
fn reducer_360 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) )(+"|") */
fn reducer_361 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_362 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_363 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_364 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_365 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_366 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_367 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* symbol '+'

            :ast { t_List_Rules, symbol:$1, tok } */
fn reducer_368 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_369 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_370 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_371 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_372 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_373 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_374 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_375 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_376 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_377 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_378 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_379 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_380 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num? ( ":" tk:precedence_num? :ast u32($2) )? '}' :ast { t_Precedence, sym_prec: u32($2), kot_prec: $3 } */
fn reducer_381 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_382 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_383 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_384 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_385 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_386 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_387 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* ":" tk:precedence_num? :ast u32($2) */
fn reducer_388 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_2_0 = 0 as u32;
  slots.assign(0, AstSlot(ASTNode::U32(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* tk:precedence_num^prec ":" */
fn reducer_389 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol(+)^s */
fn reducer_390 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol */
fn reducer_391 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol(+) */
fn reducer_392 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* member */
fn reducer_393 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_394 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* literal */
fn reducer_395 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal */
fn reducer_396 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* template_nonterminal */
fn reducer_397 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal */
fn reducer_398 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* nonterminal "::<" template_arg(+",")^template_args ">"

            :ast { t_Template_NonTerminal_Symbol, name:$1, template_args, tok } */
fn reducer_399 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_400 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* template_arg(+",") */
fn reducer_401 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_402 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* ast::type_identifier 
                
            :ast { t_AST_STRUCT_TEMPLATE_NAME, typ: str(tok)  } */
fn reducer_403 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_rule_0 = __rule_rng__;
  let tok_rule_0 = tok_rule_0.to_token(unsafe{&mut *_ctx_}.get_reader_mut());
  let tok_rule_0 = tok_rule_0.to_string();
  let var_2_0 = AST_STRUCT_TEMPLATE_NAME::new(
    tok_rule_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRUCT_TEMPLATE_NAME(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 404]
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
      reducer_384::<R, M, UP>,
      reducer_385::<R, M, UP>,
      reducer_386::<R, M, UP>,
      reducer_387::<R, M, UP>,
      reducer_388::<R, M, UP>,
      reducer_389::<R, M, UP>,
      reducer_390::<R, M, UP>,
      reducer_391::<R, M, UP>,
      reducer_392::<R, M, UP>,
      reducer_393::<R, M, UP>,
      reducer_394::<R, M, UP>,
      reducer_395::<R, M, UP>,
      reducer_396::<R, M, UP>,
      reducer_397::<R, M, UP>,
      reducer_398::<R, M, UP>,
      reducer_399::<R, M, UP>,
      reducer_400::<R, M, UP>,
      reducer_401::<R, M, UP>,
      reducer_402::<R, M, UP>,
      reducer_403::<R, M, UP>,
    ])
  }
}
    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<T, UserCTX, Bytecode> = radlr_rust_runtime::deprecate::ByteCodeParser<T, UserCTX, Bytecode>;

pub mod meta{
  
  pub const nonterm_names: [&'static str;142] = [
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
    "expr",
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
    "term",
    "identifier",
    "token",
    "token_group",
    "body",
    "int",
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
    "member",
    "map",
    "vector",
    "vector_list",
    "string",
    "bool",
    "number",
    "literal",
    "range",
    "range_group",
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
    "end_of_input",
    "ast_definition",
    "trimmed_reference",
    "reference",
    "convert_initializer",
    "num",
    "string",
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
    "not_empty_set",
    "not_empty_set_list",
    "init_objects",
    "symbol",
    "template_nonterminal",
    "template_nonterminal_list",
    "template_arg",
  ];
  
  pub const symbol_string: [&'static str;140] = [
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
    r####" % "####,
    r####" ( "####,
    r####" ) "####,
    r####" * "####,
    r####" - "####,
    r####" / "####,
    r####" ^ "####,
    r####"nonterm"####,
    r####"nonterm"####,
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
    r####" tok "####,
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
    r####" tk "####,
    r####" token "####,
    r####"nonterm"####,
    r####"tk:nonterm"####,
    r####" fail "####,
    r####" pass "####,
    r####" accept "####,
    r####" tk:( "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####" :: "####,
    r####" . "####,
    r####" map "####,
    r####" [ "####,
    r####" ] "####,
    r####" str "####,
    r####"nonterm"####,
    r####" bool "####,
    r####" i8 "####,
    r####" u8 "####,
    r####" f32 "####,
    r####" i32 "####,
    r####" u32 "####,
    r####" f64 "####,
    r####" i64 "####,
    r####" u64 "####,
    r####" f16 "####,
    r####" i16 "####,
    r####" u16 "####,
    r####" f128 "####,
    r####" i128 "####,
    r####" u128 "####,
    r####" true "####,
    r####" false "####,
    r####" match "####,
    r####"nonterm"####,
    r####" PRODUCTION "####,
    r####" TERMINAL "####,
    r####" push "####,
    r####" goto "####,
    r####" fork "####,
    r####" tk: "####,
    r####"tk:nonterm"####,
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
    r####" ]!? "####,
    r####"nonterm"####,
    r####" $ "####,
    r####"tk:nonterm"####,
    r####"tk:nonterm"####,
    r####" default "####,
    r####" fail-hint "####,
    r####"nonterm"####,
    r####"nonterm"####,
    r####"tk:nonterm"####,
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
  parser.init_parser(67796);
  parser
}

pub fn new_grammar_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(68615);
  parser
}

pub fn new_type_eval_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(122669);
  parser
}

pub fn new_ast_expression_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(122819);
  parser
}

pub fn new_ast_struct_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<T, UserCTX, &'static [u8]> {
  let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);
  parser.init_parser(123662);
  parser
}

pub static bytecode: [u8; 160129] = [
  0,211,200,197,210,208,193,2,15,1,211,8,1,0,17,1,21,0,0,0,1,21,1,69,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8, 
  4,19,37,0,0,0,101,0,0,0,1,0,19,13,0,0,0,34,0,0,0,1,0,17,1,91,0,0,0,1,2,21,1,59,0,0,0,153,229,1,0,6,0,0,0,2,0,0,0,4,88,1,128,1,80,1,128,2,80,129, 
  128,3,152,129,128,6,80,1,128,7,80,1,128,8,4,17,1,149,6,1,0,1,4,17,1,151,0,0,0,1,2,21,1,49,2,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,200,14,128,1,240,194,127,2,240,66, 
  128,50,248,13,128,55,184,76,131,53,88,13,128,6,240,2,128,7,240,66,127,74,152,8,128,73,8,10,128,58,24,140,127,59,120,75,129,108,248,3,128,109,248,2,128,46,184,16,128,47,232,15,128,75,40,135,128,103,40,6, 
  128,107,248,4,128,8,4,15,1,101,6,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,101,6,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56, 
  4,1,0,17,1,217,3,1,0,1,4,15,1,101,6,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,101,6,1,0,15,1,113,6,1, 
  0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,19,0,0,0,0,0, 
  0,0,0,3,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,19,73,0,0,0,201,0, 
  0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,15,1,101,6,1,0,15,1,224,230,0,0,17,1,72,230,0,0,1,4,15, 
  1,101,6,1,0,15,1,224,230,0,0,17,1,2,230,0,0,1,4,15,1,101,6,1,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,101,6,1,0,15,1,47,229,0,0,17,1,239,228,0,0,1,4,15, 
  1,101,6,1,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0,0,0,1,0,15,1,101,6,1,0,17,1,103,213,0,0,1, 
  4,15,1,101,6,1,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,101,6,1,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,201,2,0,0,1,2,21,1,58,0,0,0,238, 
  236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,4,3,0,0,1,2,21,1,117,0,0,0,103,237, 
  1,0,8,0,0,0,3,0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,56,3,128,41,112,2,128,72,152,1,128,6,144,1,128,7,144,1,127,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,43,212,0, 
  0,1,4,17,1,4,206,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,15,1,60,133,0,0,17,1,180,132,0,0,1,4,15,1,60,133,0,0,17,1,122,3,0,0,1,2,21,1,217,0,0,0,61,240,1,0, 
  13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128, 
  8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,139,127,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,139,127,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,139,127,0,0,1,4,15, 
  1,139,127,0,0,17,1,214,126,0,0,1,4,15,1,139,127,0,0,17,1,145,126,0,0,1,4,15,1,139,127,0,0,17,1,224,125,0,0,1,4,15,1,139,127,0,0,17,1,46,5,0,0,1,4,19,37,0,0,0, 
  101,0,0,0,1,0,17,1,139,127,0,0,1,4,15,1,139,127,0,0,17,1,84,4,0,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88, 
  198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,139,127,0,0,1,4,19, 
  120,0,0,0,101,1,0,0,1,0,17,1,139,127,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,139,127,0,0,1,4,15,1,139,127,0,0,17,1,214,126,0,0,1,4,15,1,139,127,0,0,17,1,145, 
  126,0,0,1,4,15,1,139,127,0,0,17,1,224,125,0,0,1,4,15,1,139,127,0,0,17,1,46,5,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,139,127,0,0,1,4,15,1,139,127,0,0,17,1, 
  84,4,0,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128, 
  110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,226,6,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,226,6,0,0,1,4,19,114,0,0, 
  0,94,1,0,0,1,0,17,1,226,6,0,0,1,4,15,1,226,6,0,0,17,1,214,126,0,0,1,4,15,1,226,6,0,0,17,1,145,126,0,0,1,4,15,1,226,6,0,0,17,1,224,125,0,0,1,4,15,1,226, 
  6,0,0,17,1,8,6,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,226,6,0,0,1,4,15,1,226,6,0,0,17,1,122,3,0,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0, 
  0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0, 
  0,111,1,0,0,1,0,17,1,226,6,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,226,6,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,226,6,0,0,1,4,15,1,226,6,0,0,17, 
  1,214,126,0,0,1,4,15,1,226,6,0,0,17,1,145,126,0,0,1,4,15,1,226,6,0,0,17,1,224,125,0,0,1,4,15,1,226,6,0,0,17,1,8,6,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0, 
  17,1,226,6,0,0,1,4,15,1,226,6,0,0,17,1,122,3,0,0,1,2,21,0,184,1,0,0,255,255,255,255,21,0,0,0,4,0,0,0,112,208,70,132,113,64,70,132,114,176,5,128,61,144,203,128,75,112,10,132, 
  37,200,12,128,77,8,202,128,108,128,8,128,120,32,5,128,109,240,7,128,74,0,75,130,59,96,76,126,60,248,203,126,13,48,141,125,78,160,137,128,79,16,9,128,110,96,7,128,128,144,4,128,129,40,4,128,138,192,3,128, 
  139,48,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,226,6,0,0,1,15,1,226,6,0,0,17,1,127,125,0,0,1,15,1,226,6,0,0,17,1,128,124,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17, 
  1,226,6,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,226,6,0,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,226,6,0,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,226,6,0,0,1, 
  19,77,0,0,0,212,0,0,0,1,0,17,1,226,6,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,226,6,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,226,6,0,0,1,19,74,0,0,0,205, 
  0,0,0,1,0,17,1,226,6,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,226,6,0,0,1,15,1,226,6,0,0,17,1,63,124,0,0,1,15,1,226,6,0,0,17,1,4,122,0,0,1,19,60,0,0, 
  0,163,0,0,0,1,0,17,1,226,6,0,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,226,6,0,0,1,15,1,226,6,0,0,17,1,209,121,0,0,1,15,1,226,6,0,0,17,1,95,9,0,0,1,15,1, 
  226,6,0,0,17,1,41,9,0,0,1,15,1,226,6,0,0,17,1,155,8,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,226,6,0,0,1,2,21,1,39,0,0,0,141,243,1,0,3,0,0,0,1,0,0, 
  0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,50,0,0,0,55,248,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128, 
  41,248,0,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,1,9,0,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,79,208,0,128,37,216,192,127,1,19,79,0,0,0,219,0,0, 
  0,3,0,1,2,21,1,53,0,0,0,144,248,1,0,5,0,0,0,2,0,0,0,32,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,74,0,0,0,204,0,0,0,3,0,14,1,2,21, 
  1,47,0,0,0,228,248,1,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,66,48,1,128,4,17,1,154,9,0,0,1,8,19,59,0,0,0,162,0,0,0,1,0,1,21,1, 
  217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128, 
  129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,194,37,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,194,37,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17, 
  1,194,37,0,0,1,4,15,1,194,37,0,0,17,1,214,126,0,0,1,4,15,1,194,37,0,0,17,1,229,36,0,0,1,4,15,1,194,37,0,0,17,1,220,13,0,0,1,4,15,1,194,37,0,0,17,1,40,12,0, 
  0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,194,37,0,0,1,4,15,1,194,37,0,0,17,1,116,10,0,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48, 
  2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17, 
  1,139,127,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,139,127,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,139,127,0,0,1,4,15,1,139,127,0,0,17,1,214,126,0,0,1,4,15, 
  1,139,127,0,0,17,1,229,36,0,0,1,4,15,1,139,127,0,0,17,1,220,13,0,0,1,4,15,1,139,127,0,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,139,127,0,0,1,4, 
  15,1,139,127,0,0,17,1,78,11,0,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128, 
  7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,139,127,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,139,127, 
  0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,139,127,0,0,1,4,15,1,139,127,0,0,17,1,214,126,0,0,1,4,15,1,139,127,0,0,17,1,229,36,0,0,1,4,15,1,139,127,0,0,17,1,220, 
  13,0,0,1,4,15,1,139,127,0,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,139,127,0,0,1,4,15,1,139,127,0,0,17,1,78,11,0,0,1,2,21,1,217,0,0,0,61,240, 
  1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56, 
  2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,226,6,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,226,6,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,226,6,0,0,1, 
  4,15,1,226,6,0,0,17,1,214,126,0,0,1,4,15,1,226,6,0,0,17,1,229,36,0,0,1,4,15,1,226,6,0,0,17,1,220,13,0,0,1,4,15,1,226,6,0,0,17,1,2,13,0,0,1,4,19,37,0, 
  0,0,101,0,0,0,1,0,17,1,226,6,0,0,1,4,15,1,226,6,0,0,17,1,116,10,0,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129, 
  31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,226,6,0,0,1, 
  4,19,120,0,0,0,101,1,0,0,1,0,17,1,226,6,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,226,6,0,0,1,4,15,1,226,6,0,0,17,1,214,126,0,0,1,4,15,1,226,6,0,0,17, 
  1,229,36,0,0,1,4,15,1,226,6,0,0,17,1,220,13,0,0,1,4,15,1,226,6,0,0,17,1,2,13,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,226,6,0,0,1,4,15,1,226,6,0,0, 
  17,1,116,10,0,0,1,2,21,1,176,0,0,0,67,249,1,0,11,0,0,0,3,0,0,0,112,40,3,128,1,240,1,129,2,240,1,128,31,16,5,128,76,8,4,128,41,120,4,129,6,240,129,128,7,240,1,127,110,152, 
  131,128,129,144,2,128,134,248,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,141,14,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,141,14,0,0,1,4,15,1,141,14,0,0,17,1,214,126, 
  0,0,1,4,15,1,141,14,0,0,17,1,229,36,0,0,1,4,15,1,141,14,0,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,141,14,0,0,1,4,15,1,141,14,0,0,17,1,116, 
  10,0,0,1,2,21,0,89,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,112,8,6,129,113,0,70,129,61,104,73,128,109,40,7,128,128,224,4,128,37,208,9,128,129,120,4,128,135,16,4,128,120,112,69,128,136, 
  168,3,128,74,216,136,129,139,176,2,128,108,184,7,128,13,56,74,125,110,152,6,128,79,72,8,128,138,64,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,141,14,0,0,1,15,1,141,14,0,0,17,1,132,36,0, 
  0,1,15,1,141,14,0,0,17,1,121,32,0,0,1,15,1,141,14,0,0,17,1,245,31,0,0,1,15,1,141,14,0,0,17,1,250,24,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,141,14,0,0,1,19, 
  108,0,0,0,54,1,0,0,1,0,17,1,141,14,0,0,1,1,19,136,0,0,0,135,1,0,0,1,0,17,1,141,14,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,141,14,0,0,1,19,74,0,0,0,206, 
  0,0,0,1,0,17,1,141,14,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,141,14,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,141,14,0,0,1,19,138,0,0,0,142,1,0,0,1,0,17, 
  1,141,14,0,0,1,15,1,141,14,0,0,17,1,26,16,0,0,1,15,1,141,14,0,0,17,1,231,15,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,141,14,0,0,1,2,21,1,39,0,0,0,227,249,1, 
  0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8,0,0,1,19,13,0,0,0,34,0,0,0,1,0,1,21,1,39,0,0,0,41,251,1,0,3,0,0,0,1,0,0,0, 
  2,240,0,128,1,240,64,128,139,248,0,128,8,4,17,1,77,16,0,0,1,19,138,0,0,0,140,1,0,0,1,0,1,21,1,186,0,0,0,75,252,1,0,10,0,0,0,3,0,0,0,40,240,68,129,1,208,129,128,2, 
  208,1,128,41,88,68,129,76,232,3,128,112,8,3,128,110,120,195,128,31,96,5,128,129,112,2,128,134,216,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,121,17,0,0,1,4,19,120,0,0,0,101,1,0, 
  0,1,0,17,1,121,17,0,0,1,4,15,1,121,17,0,0,17,1,214,126,0,0,1,4,15,1,121,17,0,0,17,1,229,36,0,0,1,4,15,1,121,17,0,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0, 
  0,0,1,0,17,1,121,17,0,0,1,4,15,1,121,17,0,0,17,1,8,17,0,0,1,4,15,1,121,17,0,0,17,1,116,10,0,0,1,2,21,1,50,0,0,0,55,248,1,0,3,0,0,0,1,0,0,0,2,240, 
  0,128,1,240,64,128,41,248,0,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,59,17,0,0,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,36,224,1,128,37,80,65,128,65,240,0, 
  128,19,36,0,0,0,100,0,0,0,2,0,1,19,65,0,0,0,179,0,0,0,1,0,17,1,59,17,0,0,1,1,2,21,0,94,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,128,16,5,128,129,128,4,128,61, 
  0,73,128,109,192,134,129,36,208,9,128,37,104,9,128,138,24,4,128,140,32,3,128,120,160,5,128,141,144,2,128,74,112,8,127,139,136,3,128,108,80,199,126,13,96,74,125,110,48,6,128,79,224,7,128,19,140,0,0,0, 
  144,1,0,0,1,0,17,1,121,17,0,0,1,15,1,121,17,0,0,17,1,69,22,0,0,1,19,138,0,0,0,141,1,0,0,1,0,17,1,121,17,0,0,1,15,1,121,17,0,0,17,1,62,19,0,0,1,19,141,0, 
  0,0,146,1,0,0,1,0,17,1,121,17,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,121,17,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,121,17,0,0,1,19,74,0,0,0,207,0,0,0, 
  1,0,17,1,121,17,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,121,17,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,121,17,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,121,17, 
  0,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,121,17,0,0,1,15,1,121,17,0,0,17,1,11,19,0,0,1,15,1,121,17,0,0,17,1,216,18,0,0,1,19,141,0,0,0,147,1,0,0,1,0,17,1, 
  121,17,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,121,17,0,0,1,2,21,1,39,0,0,0,60,253,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8, 
  0,0,1,19,13,0,0,0,34,0,0,0,1,0,1,21,1,39,0,0,0,230,253,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,139,240,0,128,4,17,1,77,16,0,0,1,8,19,138,0,0,0,140, 
  1,0,0,1,0,1,21,1,85,0,0,0,103,254,1,0,6,0,0,0,2,0,0,0,136,96,2,128,1,160,194,128,2,160,2,128,135,80,1,128,25,144,65,128,33,248,1,128,4,17,1,242,20,0,0,1,4,19,129,0, 
  0,0,112,1,0,0,2,0,1,4,19,129,0,0,0,113,1,0,0,2,0,1,4,17,1,159,19,0,0,1,8,19,129,0,0,0,120,1,0,0,1,0,1,21,1,108,0,0,0,220,254,1,0,6,0,0,0,2,0,0, 
  0,32,248,194,128,1,80,193,128,2,80,193,128,112,136,2,128,129,240,1,128,134,88,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,12,20,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,12, 
  20,0,0,1,4,15,1,12,20,0,0,17,1,214,126,0,0,1,4,19,129,0,0,0,116,1,0,0,3,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,108,192,194,128,129,48,1,128,110,88, 
  2,128,120,200,65,128,128,56,1,128,1,19,108,0,0,0,55,1,0,0,1,0,17,1,12,20,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,12,20,0,0,1,15,1,12,20,0,0,17,1,178,20,0,0,1, 
  15,1,12,20,0,0,17,1,114,20,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,114,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,115,1,0,0,4,0,14,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,108,0,0,0,220,254,1,0,6,0,0,0,2,0,0,0,32,248,194,128,1,80,193,128,2,80,193,128,112,136,2,128,129,240,1,128, 
  134,88,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,95,21,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,95,21,0,0,1,4,15,1,95,21,0,0,17,1,214,126,0,0,1,4,19,129, 
  0,0,0,119,1,0,0,3,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,108,192,194,128,129,48,1,128,110,88,2,128,120,200,65,128,128,56,1,128,1,19,108,0,0,0,55,1,0,0,1, 
  0,17,1,95,21,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,95,21,0,0,1,15,1,95,21,0,0,17,1,5,22,0,0,1,15,1,95,21,0,0,17,1,197,21,0,0,1,2,21,7,36,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,117,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,36, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,129,0,0,0,118,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,65,128,62,208,0,128,4,19,139,0,0,0,143,1,0,0,4,0,14,1,4,17,1,145,22,0,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,186,0,0,0,75,252,1,0,10,0,0,0,3,0,0,0,40,240,68,129,1,208,129,128,2,208,1,128,41,88,68,129,76,232,3,128,112,8,3,128,110, 
  120,195,128,31,96,5,128,129,112,2,128,134,216,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,76,23,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,76,23,0,0,1,4,15,1,76,23,0, 
  0,17,1,214,126,0,0,1,4,15,1,76,23,0,0,17,1,229,36,0,0,1,4,15,1,76,23,0,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,76,23,0,0,1,4,15,1,76,23, 
  0,0,17,1,8,17,0,0,1,4,15,1,76,23,0,0,17,1,116,10,0,0,1,2,21,0,76,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,128,128,4,128,129,240,3,128,61,112,72,128,109,48,134,129,36,64, 
  9,128,37,216,8,128,138,136,3,128,140,240,2,128,120,16,5,128,141,144,2,128,74,224,7,127,139,248,2,128,108,192,198,126,13,208,73,125,110,160,5,128,79,80,7,128,19,140,0,0,0,145,1,0,0,3,0,1,1,19, 
  138,0,0,0,141,1,0,0,1,0,17,1,76,23,0,0,1,15,1,76,23,0,0,17,1,153,24,0,0,1,19,141,0,0,0,146,1,0,0,1,0,17,1,76,23,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17, 
  1,76,23,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,76,23,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,76,23,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,76,23,0,0,1, 
  19,74,0,0,0,205,0,0,0,1,0,17,1,76,23,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,76,23,0,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,76,23,0,0,1,15,1,76,23,0,0, 
  17,1,11,19,0,0,1,15,1,76,23,0,0,17,1,216,18,0,0,1,19,141,0,0,0,147,1,0,0,1,0,17,1,76,23,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,76,23,0,0,1,2,21,1,85, 
  0,0,0,103,254,1,0,6,0,0,0,2,0,0,0,136,32,2,128,1,160,194,128,2,160,2,128,135,96,2,128,25,80,65,128,33,184,1,128,4,19,129,0,0,0,112,1,0,0,2,0,1,4,19,129,0,0,0,113,1, 
  0,0,2,0,1,4,17,1,159,19,0,0,1,4,17,1,242,20,0,0,1,8,19,129,0,0,0,120,1,0,0,1,0,1,21,1,69,0,0,0,102,255,1,0,5,0,0,0,2,0,0,0,120,48,1,128,1,176,1,128, 
  2,176,129,128,123,112,1,128,10,184,1,128,4,17,1,218,30,0,0,1,4,17,1,175,28,0,0,1,8,4,15,1,192,27,0,0,17,1,75,25,0,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,1,86,0,0, 
  0,179,0,2,0,6,0,0,0,2,0,0,0,39,216,1,128,1,80,193,128,2,80,193,128,11,72,66,127,137,152,1,128,138,88,1,128,8,4,17,1,129,27,0,0,1,4,17,1,67,26,0,0,1,4,15,1,218,25,0, 
  0,17,1,162,25,0,0,1,4,19,132,0,0,0,128,1,0,0,2,0,1,2,21,1,44,0,0,0,96,1,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,137,248,0,128,8,4,19,133,0,0,0,131, 
  1,0,0,2,0,1,19,133,0,0,0,132,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,132,56,1,128,133,208,0,128,15,1,218,25,0,0,17,1,3,26,0,0,1,1,2,21, 
  7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,126,1,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,2,21,7,53,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,125,64,1,128,4,15,1,24,27,0,0,17,1,148,26,0,0,1,4,19,132,0,0,0,127,1,0,0,3,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,56,0,0,0,180,1,2,0,4,0,0,0,2,0,0,0,137,88,1,128,1,80,193,127,2,80,65,128,138,16,1,128,4,17, 
  1,216,26,0,0,1,8,4,19,133,0,0,0,131,1,0,0,2,0,1,19,133,0,0,0,132,1,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0, 
  129,1,0,0,5,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,132,56,1,128,133,208, 
  0,128,15,1,24,27,0,0,17,1,65,27,0,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,125,1,0,0,4,0,14,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,132,0,0,0,130,1,0,0,3,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,192,27,0,0,17,1,233,27, 
  0,0,1,1,2,21,1,51,0,0,0,20,2,2,0,4,0,0,0,2,0,0,0,120,80,1,128,1,144,1,128,2,144,1,128,123,16,1,128,4,17,1,99,28,0,0,1,4,17,1,41,28,0,0,1,8,19,112,0,0, 
  0,78,1,0,0,2,0,14,1,21,1,45,0,0,0,203,2,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,123,240,0,128,4,19,112,0,0,0,89,1,0,0,4,0,14,1,8,19,112,0,0,0,83, 
  1,0,0,3,0,14,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,112,0,0,0,88,1,0,0,4,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,19,112,0,0,0,87,1,0,0,3,0,14,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,101,30,0,0,17,1,121,29, 
  0,0,1,4,17,1,6,29,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,112,0,0,0,81,1,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,81,29,0,0,17,1,75,25,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,112,0,0,0,79,1, 
  0,0,3,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,48,65,128,132,208,0,128,19,112,0,0,0,75,1,0,0,4,0,1,1,2,21,1,86,0,0,0,179,0,2,0,6,0,0,0,2, 
  0,0,0,39,216,1,128,1,80,193,128,2,80,193,128,11,72,66,127,137,152,1,128,138,88,1,128,8,4,17,1,129,27,0,0,1,4,17,1,208,29,0,0,1,4,15,1,218,25,0,0,17,1,162,25,0,0,1,4,19, 
  132,0,0,0,128,1,0,0,2,0,1,2,21,7,53,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,125,64,1,128,4,15,1,24,27,0,0,17,1,33,30,0,0,1,4,19,132,0,0,0,127,1, 
  0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,56,0,0,0,180,1,2,0,4,0,0,0,2,0,0,0,137,88,1,128,1,16,193,127,2, 
  16,65,128,138,24,1,128,8,4,17,1,216,26,0,0,1,4,19,133,0,0,0,131,1,0,0,2,0,1,19,133,0,0,0,132,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112, 
  56,65,128,132,208,0,128,15,1,101,30,0,0,17,1,142,30,0,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,112,0,0,0,82,1,0,0,4,0,14,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,112,0,0,0,77,1,0,0,3,0,14,1,21,1,57,0,0,0,119,3,2,0,4,0,0,0,2,0,0,0,10,80,1,128, 
  1,192,1,128,2,192,129,127,123,16,1,128,4,17,1,130,31,0,0,1,4,15,1,31,31,0,0,17,1,75,25,0,0,1,8,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,31,31,0,0,17,1,72,31,0,0,1,1,2,21,1,45,0,0,0,203,2,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,123,240,0,128,4,19, 
  112,0,0,0,86,1,0,0,4,0,14,1,8,19,112,0,0,0,76,1,0,0,3,0,14,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,205,31,0,0,17,1,75,25,0, 
  0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,112,0,0,0,85,1,0,0,3,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112, 
  48,65,128,132,208,0,128,19,112,0,0,0,84,1,0,0,4,0,1,1,2,21,1,65,0,0,0,46,4,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,83,200,1,128,125, 
  88,1,128,8,4,19,113,0,0,0,93,1,0,0,3,0,14,1,4,17,1,55,32,0,0,1,2,21,1,53,0,0,0,131,4,2,0,5,0,0,0,2,0,0,0,124,56,1,128,1,48,1,128,2,48,129,128,7,48,1, 
  128,6,48,1,128,8,4,19,113,0,0,0,91,1,0,0,4,0,14,1,19,113,0,0,0,92,1,0,0,3,0,14,1,21,1,176,0,0,0,133,5,2,0,11,0,0,0,3,0,0,0,112,32,3,128,1,0,4,129,2, 
  0,4,128,31,16,5,128,76,144,3,128,41,136,2,129,6,0,132,128,7,0,4,127,110,8,132,128,129,120,4,128,134,240,1,128,4,19,128,0,0,0,111,1,0,0,1,0,17,1,53,33,0,0,1,4,19,37,0,0,0, 
  101,0,0,0,1,0,17,1,53,33,0,0,1,4,15,1,53,33,0,0,17,1,214,126,0,0,1,4,15,1,53,33,0,0,17,1,40,12,0,0,1,8,4,15,1,53,33,0,0,17,1,229,36,0,0,1,4,19,120,0, 
  0,0,101,1,0,0,1,0,17,1,53,33,0,0,1,4,15,1,53,33,0,0,17,1,116,10,0,0,1,19,135,0,0,0,134,1,0,0,1,0,1,21,0,49,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,112, 
  248,196,130,129,112,3,128,74,152,7,131,139,112,2,128,108,120,6,128,13,248,200,128,110,88,5,128,79,8,7,128,37,144,72,128,61,40,72,128,109,232,5,128,120,104,68,128,128,216,67,128,136,104,3,128,138,0,3,128,19, 
  138,0,0,0,141,1,0,0,1,0,17,1,53,33,0,0,1,15,1,53,33,0,0,17,1,35,36,0,0,1,1,15,1,53,33,0,0,17,1,205,34,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,53,33,0, 
  0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,53,33,0,0,1,19,136,0,0,0,136,1,0,0,2,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,53,33,0,0,1,19,74,0,0,0,206,0,0,0,1, 
  0,17,1,53,33,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,53,33,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,53,33,0,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,53,33,0, 
  0,1,15,1,53,33,0,0,17,1,154,34,0,0,1,15,1,53,33,0,0,17,1,103,34,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,53,33,0,0,1,2,21,1,39,0,0,0,227,249,1,0,3,0,0, 
  0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,39,0,0,0,41,251,1,0,3,0,0,0,1,0,0,0,2,48,1,128, 
  1,48,65,128,139,240,0,128,4,17,1,77,16,0,0,1,8,19,138,0,0,0,140,1,0,0,1,0,1,21,1,69,0,0,0,102,255,1,0,5,0,0,0,2,0,0,0,120,160,1,128,1,224,1,128,2,224,129,128,123, 
  232,1,128,10,48,1,128,4,15,1,186,35,0,0,17,1,75,25,0,0,1,4,17,1,117,35,0,0,1,8,4,17,1,30,35,0,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,7,48,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,6,29,0,0,1,4,15,1,101,30,0,0,17,1,121,29,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,112,0,0,0,81,1,0,0,2,0,1,21,1,57,0,0,0,119,3,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,192,1,128,2,192,129,127,123,128,1,128,4,15,1,31,31,0,0,17,1, 
  75,25,0,0,1,4,17,1,130,31,0,0,1,8,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,186,35,0,0,17,1, 
  227,35,0,0,1,1,2,21,1,51,0,0,0,20,2,2,0,4,0,0,0,2,0,0,0,120,88,1,128,1,80,1,128,2,80,1,128,123,16,1,128,4,17,1,99,28,0,0,1,8,4,17,1,41,28,0,0,1,19,112, 
  0,0,0,78,1,0,0,2,0,14,1,21,1,85,0,0,0,49,6,2,0,6,0,0,0,2,0,0,0,136,104,2,128,1,96,194,128,2,96,2,128,135,184,1,128,25,80,65,128,33,248,1,128,4,19,129,0,0,0,112, 
  1,0,0,2,0,1,4,17,1,242,20,0,0,1,4,19,129,0,0,0,113,1,0,0,2,0,1,8,4,17,1,159,19,0,0,1,19,129,0,0,0,120,1,0,0,1,0,1,21,1,85,0,0,0,49,6,2,0,6,0, 
  0,0,2,0,0,0,136,96,2,128,1,160,194,128,2,160,2,128,135,80,1,128,25,144,65,128,33,248,1,128,4,17,1,242,20,0,0,1,4,19,129,0,0,0,112,1,0,0,2,0,1,4,19,129,0,0,0,113,1,0, 
  0,2,0,1,4,17,1,159,19,0,0,1,8,19,129,0,0,0,120,1,0,0,1,0,1,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0,0,0,41,24,1,128,1,16,193,127,2,16,1,128,31,176,1,128,8, 
  4,19,37,0,0,0,101,0,0,0,1,0,17,1,42,37,0,0,1,4,15,1,42,37,0,0,17,1,116,10,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,37,40,130,128,13,144,194,127, 
  61,200,129,128,79,56,1,128,109,48,1,128,1,19,61,0,0,0,167,0,0,0,1,0,17,1,42,37,0,0,1,19,109,0,0,0,56,1,0,0,2,0,1,15,1,42,37,0,0,17,1,143,37,0,0,1,19,61,0,0, 
  0,166,0,0,0,1,0,17,1,42,37,0,0,1,2,21,1,39,0,0,0,110,7,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34, 
  0,0,0,1,0,1,21,0,149,1,0,0,255,255,255,255,20,0,0,0,4,0,0,0,112,176,6,132,113,32,6,132,114,144,5,128,61,64,75,128,77,232,201,128,37,176,11,128,108,96,8,128,109,208,7,128,120,0,5,128, 
  110,64,7,128,74,176,10,130,75,80,10,130,60,168,139,126,13,24,140,125,78,128,201,126,79,240,8,128,128,112,4,128,129,8,4,128,138,160,3,128,139,16,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,194,37,0, 
  0,1,15,1,194,37,0,0,17,1,112,121,0,0,1,15,1,194,37,0,0,17,1,182,120,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,194,37,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,194, 
  37,0,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,194,37,0,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,194,37,0,0,1,19,77,0,0,0,212,0,0,0,1,0,17,1,194,37,0,0,1,19,74, 
  0,0,0,207,0,0,0,1,0,17,1,194,37,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,194,37,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,194,37,0,0,1,19,61,0,0,0,167,0,0, 
  0,1,0,17,1,194,37,0,0,1,15,1,194,37,0,0,17,1,246,43,0,0,1,15,1,194,37,0,0,17,1,139,39,0,0,1,19,60,0,0,0,164,0,0,0,3,0,1,19,138,0,0,0,142,1,0,0,1,0,17, 
  1,194,37,0,0,1,15,1,194,37,0,0,17,1,88,39,0,0,1,1,15,1,194,37,0,0,17,1,155,8,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,194,37,0,0,1,2,21,1,39,0,0,0,13,8, 
  2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,139,248,0,128,8,4,17,1,77,16,0,0,1,19,138,0,0,0,140,1,0,0,1,0,1,21,1,217,0,0,0,61,9,2,0,13,0,0,0,3,0,0, 
  0,112,160,2,128,1,144,5,129,2,144,133,129,31,168,195,129,76,32,5,128,41,136,132,129,6,144,197,128,7,144,5,127,82,48,2,128,110,24,196,128,127,48,6,128,129,152,5,128,134,16,3,128,4,15,1,112,40,0,0, 
  17,1,220,13,0,0,1,4,15,1,112,40,0,0,17,1,214,126,0,0,1,4,19,128,0,0,0,111,1,0,0,1,0,17,1,112,40,0,0,1,4,15,1,112,40,0,0,17,1,116,10,0,0,1,4,15,1,112,40,0, 
  0,17,1,229,36,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,112,40,0,0,1,4,15,1,112,40,0,0,17,1,40,12,0,0,1,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,112,40,0,0, 
  1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,112,40,0,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,80,73,130,113,232,197,131,114,104, 
  7,128,61,200,71,128,77,48,132,128,37,176,9,128,109,48,8,128,110,192,8,128,120,216,6,128,128,88,5,128,74,200,196,129,139,72,6,128,108,160,3,128,13,24,138,125,78,48,68,126,79,56,4,128,129,208,2,128,138,56, 
  3,128,15,1,112,40,0,0,17,1,90,42,0,0,1,15,1,112,40,0,0,17,1,249,41,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,112,40,0,0,1,1,19,61,0,0,0,167,0,0,0,1,0,17,1, 
  112,40,0,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,112,40,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,112,40,0,0,1,19,77,0,0,0,215,0,0,0,2,0,1,19,138,0,0,0,141,1, 
  0,0,1,0,17,1,112,40,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,112,40,0,0,1,19,78,0,0,0,216,0,0,0,2,0,1,15,1,112,40,0,0,17,1,209,121,0,0,1,19,74,0,0,0,206, 
  0,0,0,1,0,17,1,112,40,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,112,40,0,0,1,19,77,0,0,0,214,0,0,0,2,0,1,15,1,112,40,0,0,17,1,198,41,0,0,1,19,61,0,0,0, 
  166,0,0,0,1,0,17,1,112,40,0,0,1,2,21,1,39,0,0,0,141,243,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8,0,0,1,19,13,0,0,0,34,0, 
  0,0,1,0,1,21,1,85,0,0,0,21,10,2,0,6,0,0,0,2,0,0,0,136,248,1,128,1,160,194,128,2,160,2,128,135,184,1,128,25,56,66,128,33,80,1,128,4,19,129,0,0,0,113,1,0,0,2,0,1, 
  4,17,1,242,20,0,0,1,4,17,1,159,19,0,0,1,4,19,129,0,0,0,112,1,0,0,2,0,1,8,19,129,0,0,0,120,1,0,0,1,0,1,21,1,69,0,0,0,26,11,2,0,5,0,0,0,2,0,0,0, 
  120,224,1,128,1,32,2,128,2,32,130,128,123,160,1,128,10,48,1,128,4,15,1,83,43,0,0,17,1,75,25,0,0,1,4,17,1,30,35,0,0,1,4,17,1,171,42,0,0,1,8,19,112,0,0,0,90,1,0,0, 
  1,0,1,21,1,57,0,0,0,8,12,2,0,4,0,0,0,2,0,0,0,10,88,1,128,1,80,1,128,2,80,129,127,123,16,1,128,4,17,1,130,31,0,0,1,8,4,15,1,240,42,0,0,17,1,75,25,0,0,1, 
  19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,240,42,0,0,17,1,25,43,0,0,1,1,2,21,1,45,0,0,0,235, 
  12,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,123,240,0,128,4,19,112,0,0,0,86,1,0,0,4,0,14,1,8,19,112,0,0,0,76,1,0,0,3,0,14,1,21,0,40,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,83,43,0,0,17,1,124,43,0,0,1,1,2,21,1,51,0,0,0,195,13,2,0,4,0,0,0,2,0,0,0,120,16,1,128,1,144,1,128,2,144, 
  1,128,123,80,1,128,4,17,1,188,43,0,0,1,4,17,1,99,28,0,0,1,8,19,112,0,0,0,78,1,0,0,2,0,14,1,21,1,45,0,0,0,235,12,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96, 
  65,128,123,240,0,128,4,19,112,0,0,0,89,1,0,0,4,0,14,1,8,19,112,0,0,0,83,1,0,0,3,0,14,1,21,1,53,0,0,0,166,14,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128, 
  2,48,129,127,7,48,1,128,45,56,1,128,8,4,15,1,142,120,0,0,17,1,55,44,0,0,1,19,75,0,0,0,209,0,0,0,1,0,1,21,1,11,2,0,0,17,15,2,0,30,0,0,0,4,0,0,0,96,232,8, 
  135,1,80,4,132,2,80,132,131,99,152,7,128,84,184,77,133,101,144,6,128,6,80,68,131,7,80,68,131,88,104,12,128,89,248,11,128,10,232,207,130,91,24,11,128,92,168,10,128,93,56,10,128,94,200,9,128,31,120,207, 
  129,34,8,143,128,81,152,142,129,82,40,142,129,86,72,205,129,87,216,12,128,90,136,11,128,95,88,73,129,97,120,136,129,98,8,8,128,100,40,7,128,102,248,5,128,127,136,5,128,128,240,4,128,129,88,4,128,8,4,19, 
  120,0,0,0,101,1,0,0,1,0,17,1,165,118,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,165,118,0,0,1,4,15,1,165,118,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0, 
  1,0,17,1,165,118,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,165,118,0,0,1,4,15,1,165,118,0,0,17,1,176,117,0,0,1,4,15,1,165,118,0,0,17,1,61,117,0,0,1,4,15,1,165, 
  118,0,0,17,1,202,116,0,0,1,4,15,1,165,118,0,0,17,1,87,116,0,0,1,4,15,1,165,118,0,0,17,1,228,115,0,0,1,4,15,1,165,118,0,0,17,1,113,115,0,0,1,4,15,1,165,118,0,0,17, 
  1,254,114,0,0,1,4,15,1,165,118,0,0,17,1,139,114,0,0,1,4,15,1,165,118,0,0,17,1,24,114,0,0,1,4,15,1,165,118,0,0,17,1,165,113,0,0,1,4,15,1,165,118,0,0,17,1,50,113,0, 
  0,1,4,15,1,165,118,0,0,17,1,191,112,0,0,1,4,15,1,165,118,0,0,17,1,76,112,0,0,1,4,15,1,165,118,0,0,17,1,217,111,0,0,1,4,15,1,165,118,0,0,17,1,102,111,0,0,1,4,15, 
  1,165,118,0,0,17,1,53,109,0,0,1,4,15,1,165,118,0,0,17,1,138,99,0,0,1,4,15,1,165,118,0,0,17,1,19,92,0,0,1,4,15,1,165,118,0,0,17,1,9,87,0,0,1,4,15,1,165,118,0, 
  0,17,1,118,81,0,0,1,4,15,1,165,118,0,0,17,1,67,46,0,0,1,2,21,1,3,2,0,0,83,22,2,0,28,0,0,0,4,0,0,0,96,168,136,134,1,16,4,132,2,16,132,131,99,88,7,128,84,120,205, 
  132,101,80,6,128,86,8,141,132,87,152,12,128,40,200,206,130,89,184,11,128,90,72,11,128,91,216,10,128,92,104,10,128,93,248,9,128,94,136,9,128,31,168,79,129,34,56,143,128,81,88,14,129,82,232,13,129,88,40,12, 
  128,95,24,73,129,97,56,136,129,98,200,7,128,100,232,6,128,102,184,5,128,127,72,5,128,128,176,4,128,129,24,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,71,48,0,0,1,4,19,119,0,0,0,100, 
  1,0,0,1,0,17,1,71,48,0,0,1,4,15,1,71,48,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,71,48,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,71, 
  48,0,0,1,4,15,1,71,48,0,0,17,1,176,117,0,0,1,4,15,1,71,48,0,0,17,1,61,117,0,0,1,4,15,1,71,48,0,0,17,1,202,116,0,0,1,4,15,1,71,48,0,0,17,1,87,116,0,0,1, 
  4,15,1,71,48,0,0,17,1,228,115,0,0,1,4,15,1,71,48,0,0,17,1,113,115,0,0,1,4,15,1,71,48,0,0,17,1,254,114,0,0,1,4,15,1,71,48,0,0,17,1,139,114,0,0,1,4,15,1,71, 
  48,0,0,17,1,24,114,0,0,1,4,15,1,71,48,0,0,17,1,165,113,0,0,1,4,15,1,71,48,0,0,17,1,50,113,0,0,1,4,15,1,71,48,0,0,17,1,191,112,0,0,1,4,15,1,71,48,0,0,17, 
  1,76,112,0,0,1,4,15,1,71,48,0,0,17,1,217,111,0,0,1,4,15,1,71,48,0,0,17,1,102,111,0,0,1,4,15,1,71,48,0,0,17,1,53,109,0,0,1,4,15,1,71,48,0,0,17,1,138,99,0, 
  0,1,4,15,1,71,48,0,0,17,1,19,92,0,0,1,4,15,1,71,48,0,0,17,1,8,17,0,0,1,4,15,1,71,48,0,0,17,1,9,87,0,0,1,4,15,1,71,48,0,0,17,1,118,81,0,0,1,2,21, 
  0,81,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,64,240,197,130,81,120,7,128,34,128,134,130,68,208,131,130,36,152,200,127,85,248,132,130,86,216,3,128,87,8,72,130,120,176,2,128,9,144,9,128,10,208,3, 
  128,80,248,9,128,82,64,3,128,84,0,73,128,116,232,6,128,117,136,5,128,119,104,4,128,19,87,0,0,0,6,1,0,0,1,0,17,1,71,48,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,71,48,0,0, 
  1,1,19,64,0,0,0,177,0,0,0,1,0,17,1,71,48,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,71,48,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,71,48,0,0,1,15,1,71,48, 
  0,0,17,1,192,80,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,71,48,0,0,1,15,1,71,48,0,0,17,1,63,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,71,48,0,0,1,19,64, 
  0,0,0,173,0,0,0,1,0,17,1,71,48,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,71,48,0,0,1,15,1,71,48,0,0,17,1,217,49,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1, 
  71,48,0,0,1,15,1,71,48,0,0,17,1,153,49,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,71,48,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4, 
  19,68,0,0,0,191,0,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44, 
  64,1,128,125,208,0,128,4,19,10,0,0,0,30,0,0,0,3,0,14,1,4,17,1,37,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,104, 
  0,0,0,95,23,2,0,6,0,0,0,2,0,0,0,41,168,2,129,1,80,193,127,2,80,65,128,54,56,130,128,69,200,1,128,70,88,1,128,8,4,15,1,44,53,0,0,17,1,185,52,0,0,1,4,15,1,44,53,0, 
  0,17,1,70,52,0,0,1,4,15,1,44,53,0,0,17,1,142,50,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,44,53,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  60,176,0,128,4,15,1,30,52,0,0,17,1,217,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,66,0,0,0,184,0,0,0,1,0,1,21,1,50, 
  0,0,0,188,25,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,72,248,0,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,12,51,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,88,208,0,128,69,216,0,128,1,15,1,12,51,0,0,17,1,53,51,0,0,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,65,128,62,208,0,128,4,19,88,0,0, 
  0,8,1,0,0,3,0,14,1,4,17,1,129,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,50,0,0,0,188,25,2,0,3,0,0,0,1, 
  0,0,0,2,240,128,128,1,240,0,128,72,248,0,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,180,51,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,88,208,0,128,69,216, 
  0,128,1,15,1,180,51,0,0,17,1,221,51,0,0,1,2,21,7,37,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,19,88,0,0,0,7,1,0,0,5,0,14,14,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,66,48,65,128,88,208,0,128,19,66,0,0,0,181,0,0,0,2,0,1, 
  1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,145,52,0,0,17,1,217,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,66,0,0,0,183,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,66,48,65,128,88,208,0,128,19,66,0,0,0,180,0,0,0,2,0,1,1,2,21,7,36, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,4,53,0,0,17,1,217,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  19,66,0,0,0,185,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,66,48,65,128,88,208,0,128,19,66,0,0,0,182,0,0,0,2,0,1,1,2,21,0,123,0,0,0,255,255, 
  255,255,6,0,0,0,2,0,0,0,35,216,2,128,37,72,194,128,10,208,195,128,11,104,67,127,65,224,1,128,66,80,1,128,19,35,0,0,0,99,0,0,0,1,0,17,1,44,53,0,0,1,15,1,44,53,0,0,17,1, 
  195,54,0,0,1,19,65,0,0,0,179,0,0,0,1,0,17,1,44,53,0,0,1,19,11,0,0,0,31,0,0,0,1,0,17,1,44,53,0,0,1,15,1,44,53,0,0,17,1,168,53,0,0,1,1,2,21,7,49,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,72,1,128,125,208,0,128,4,19,10,0,0,0,29,0,0,0,5,0,14,14,1,4,17,1,245,53,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,104,0,0,0,95,23,2,0,6,0,0,0,2,0,0,0,41,168,2,129,1,80,193,127,2,80,65,128,54,56,130,128,69,200,1,128,70,88,1,128,8,4,15,1,94, 
  54,0,0,17,1,185,52,0,0,1,4,15,1,94,54,0,0,17,1,70,52,0,0,1,4,15,1,94,54,0,0,17,1,142,50,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,94,54,0,0,1,2,21,0, 
  100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,35,184,2,128,37,40,194,128,66,48,1,128,11,24,67,127,65,192,1,128,19,35,0,0,0,99,0,0,0,1,0,17,1,94,54,0,0,1,15,1,94,54,0,0, 
  17,1,195,54,0,0,1,19,65,0,0,0,179,0,0,0,1,0,17,1,94,54,0,0,1,19,11,0,0,0,32,0,0,0,3,0,1,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0, 
  128,4,17,1,8,55,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,98,0,0,0,1,0,1,21,1,3,2,0,0,241,25,2,0,28,0, 
  0,0,4,0,0,0,96,168,136,134,1,16,4,132,2,16,132,131,99,88,7,128,84,120,205,132,101,80,6,128,86,8,141,132,87,152,12,128,88,40,12,128,89,184,11,128,10,168,79,130,91,216,10,128,92,104,10,128,93,248, 
  9,128,94,136,9,128,31,56,79,129,34,200,142,128,81,88,14,129,82,232,13,129,90,72,11,128,95,24,73,129,97,56,136,129,98,200,7,128,100,232,6,128,102,184,5,128,127,72,5,128,128,176,4,128,129,24,4,128,8,4, 
  19,120,0,0,0,101,1,0,0,1,0,17,1,99,57,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,99,57,0,0,1,4,15,1,99,57,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0, 
  0,1,0,17,1,99,57,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,99,57,0,0,1,4,15,1,99,57,0,0,17,1,176,117,0,0,1,4,15,1,99,57,0,0,17,1,61,117,0,0,1,4,15,1, 
  99,57,0,0,17,1,202,116,0,0,1,4,15,1,99,57,0,0,17,1,87,116,0,0,1,4,15,1,99,57,0,0,17,1,228,115,0,0,1,4,15,1,99,57,0,0,17,1,113,115,0,0,1,4,15,1,99,57,0,0, 
  17,1,254,114,0,0,1,4,15,1,99,57,0,0,17,1,139,114,0,0,1,4,15,1,99,57,0,0,17,1,24,114,0,0,1,4,15,1,99,57,0,0,17,1,165,113,0,0,1,4,15,1,99,57,0,0,17,1,50,113, 
  0,0,1,4,15,1,99,57,0,0,17,1,191,112,0,0,1,4,15,1,99,57,0,0,17,1,76,112,0,0,1,4,15,1,99,57,0,0,17,1,217,111,0,0,1,4,15,1,99,57,0,0,17,1,102,111,0,0,1,4, 
  15,1,99,57,0,0,17,1,53,109,0,0,1,4,15,1,99,57,0,0,17,1,138,99,0,0,1,4,15,1,99,57,0,0,17,1,19,92,0,0,1,4,15,1,99,57,0,0,17,1,9,87,0,0,1,4,15,1,99,57, 
  0,0,17,1,118,81,0,0,1,4,15,1,99,57,0,0,17,1,12,57,0,0,1,2,21,1,45,0,0,0,222,26,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,40,248,0,128,8,4,15,1,58,57, 
  0,0,17,1,8,17,0,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,10,56,65,128,36,208,0,128,15,1,58,57,0,0,17,1,217,49,0,0,1,1,2,21,0,59,1,0,0,255,255,255, 
  255,15,0,0,0,3,0,0,0,64,120,72,130,81,88,7,128,10,120,137,129,35,8,9,128,84,56,198,129,85,168,197,129,86,24,5,128,87,136,132,129,34,16,137,128,80,232,71,129,82,200,6,128,116,248,3,128,117,144,3, 
  128,119,0,3,128,120,112,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,99,57,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,99,57,0,0,1,15,1,99,57,0,0,17,1,192,80,0,0,1,19,80, 
  0,0,0,220,0,0,0,1,0,17,1,99,57,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,99,57,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,99,57,0,0,1,19,64,0,0,0,176,0,0, 
  0,1,0,17,1,99,57,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,99,57,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,99,57,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,99, 
  57,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,99,57,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,99,57,0,0,1,1,15,1,99,57,0,0,17,1,159,58,0,0,1,19,35,0,0,0,97, 
  0,0,0,3,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,80,2,128,37,144,193,127,42,16,194,128,43,144,66,128,47,208,1,128,94,80,1,128,4,17,1,224,76,0,0,1,4,17,1, 
  93,73,0,0,1,4,17,1,218,69,0,0,1,4,17,1,87,66,0,0,1,4,17,1,188,62,0,0,1,4,17,1,33,59,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  208,0,128,8,19,35,0,0,0,96,0,0,0,3,0,14,1,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86, 
  232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98, 
  168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,19,61,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,19,61,0, 
  0,1,4,15,1,19,61,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,19,61,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,19,61,0,0,1,4,15,1,19,61,0, 
  0,17,1,176,117,0,0,1,4,15,1,19,61,0,0,17,1,61,117,0,0,1,4,15,1,19,61,0,0,17,1,202,116,0,0,1,4,15,1,19,61,0,0,17,1,87,116,0,0,1,4,15,1,19,61,0,0,17,1,228, 
  115,0,0,1,4,15,1,19,61,0,0,17,1,113,115,0,0,1,4,15,1,19,61,0,0,17,1,254,114,0,0,1,4,15,1,19,61,0,0,17,1,139,114,0,0,1,4,15,1,19,61,0,0,17,1,24,114,0,0,1, 
  4,15,1,19,61,0,0,17,1,165,113,0,0,1,4,15,1,19,61,0,0,17,1,50,113,0,0,1,4,15,1,19,61,0,0,17,1,191,112,0,0,1,4,15,1,19,61,0,0,17,1,76,112,0,0,1,4,15,1,19, 
  61,0,0,17,1,217,111,0,0,1,4,15,1,19,61,0,0,17,1,102,111,0,0,1,4,15,1,19,61,0,0,17,1,53,109,0,0,1,4,15,1,19,61,0,0,17,1,138,99,0,0,1,4,15,1,19,61,0,0,17, 
  1,19,92,0,0,1,4,15,1,19,61,0,0,17,1,9,87,0,0,1,4,15,1,19,61,0,0,17,1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7, 
  128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,19,61, 
  0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,19,61,0,0,1,15,1,19,61,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,19,61,0,0,1,19,64,0,0,0,178,0,0, 
  0,1,0,17,1,19,61,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,19,61,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,19,61,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,19, 
  61,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,19,61,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,19,61,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,19,61,0,0,1,19,34, 
  0,0,0,95,0,0,0,1,0,17,1,19,61,0,0,1,15,1,19,61,0,0,17,1,58,62,0,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,144,2,128,37,144,193,127,42,80,194,128, 
  43,208,65,128,47,16,2,128,94,80,1,128,4,17,1,224,76,0,0,1,4,17,1,93,73,0,0,1,4,17,1,33,59,0,0,1,4,17,1,218,69,0,0,1,4,17,1,87,66,0,0,1,4,17,1,188,62,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,87,0,0,0,3,0,14,1,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136, 
  72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24, 
  15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17, 
  1,174,64,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,174,64,0,0,1,4,15,1,174,64,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,174,64,0,0,1,4,19, 
  87,0,0,0,3,1,0,0,1,0,17,1,174,64,0,0,1,4,15,1,174,64,0,0,17,1,176,117,0,0,1,4,15,1,174,64,0,0,17,1,61,117,0,0,1,4,15,1,174,64,0,0,17,1,202,116,0,0,1,4, 
  15,1,174,64,0,0,17,1,87,116,0,0,1,4,15,1,174,64,0,0,17,1,228,115,0,0,1,4,15,1,174,64,0,0,17,1,113,115,0,0,1,4,15,1,174,64,0,0,17,1,254,114,0,0,1,4,15,1,174,64, 
  0,0,17,1,139,114,0,0,1,4,15,1,174,64,0,0,17,1,24,114,0,0,1,4,15,1,174,64,0,0,17,1,165,113,0,0,1,4,15,1,174,64,0,0,17,1,50,113,0,0,1,4,15,1,174,64,0,0,17,1, 
  191,112,0,0,1,4,15,1,174,64,0,0,17,1,76,112,0,0,1,4,15,1,174,64,0,0,17,1,217,111,0,0,1,4,15,1,174,64,0,0,17,1,102,111,0,0,1,4,15,1,174,64,0,0,17,1,53,109,0,0, 
  1,4,15,1,174,64,0,0,17,1,138,99,0,0,1,4,15,1,174,64,0,0,17,1,19,92,0,0,1,4,15,1,174,64,0,0,17,1,9,87,0,0,1,4,15,1,174,64,0,0,17,1,118,81,0,0,1,2,21,0, 
  38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128, 
  119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,174,64,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,174,64,0,0,1,15,1,174,64,0,0,17,1,192,80,0,0,1,19,80,0, 
  0,0,220,0,0,0,1,0,17,1,174,64,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,174,64,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,174,64,0,0,1,19,64,0,0,0,176,0,0,0, 
  1,0,17,1,174,64,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,174,64,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,174,64,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,174,64, 
  0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,174,64,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,174,64,0,0,1,15,1,174,64,0,0,17,1,213,65,0,0,1,2,21,7,90,0,0,0,255, 
  255,255,255,6,0,0,0,2,0,0,0,45,80,1,128,37,144,193,127,42,80,194,128,43,208,65,128,47,16,2,128,94,144,2,128,4,17,1,188,62,0,0,1,4,17,1,93,73,0,0,1,4,17,1,33,59,0,0,1,4, 
  17,1,218,69,0,0,1,4,17,1,87,66,0,0,1,4,17,1,224,76,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,88,0,0,0,3, 
  0,14,1,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11, 
  128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5, 
  128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,73,68,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,73,68,0,0,1,4,15,1,73,68,0,0,17,1,35,118,0, 
  0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,73,68,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,73,68,0,0,1,4,15,1,73,68,0,0,17,1,176,117,0,0,1,4,15,1,73,68,0, 
  0,17,1,61,117,0,0,1,4,15,1,73,68,0,0,17,1,202,116,0,0,1,4,15,1,73,68,0,0,17,1,87,116,0,0,1,4,15,1,73,68,0,0,17,1,228,115,0,0,1,4,15,1,73,68,0,0,17,1,113, 
  115,0,0,1,4,15,1,73,68,0,0,17,1,254,114,0,0,1,4,15,1,73,68,0,0,17,1,139,114,0,0,1,4,15,1,73,68,0,0,17,1,24,114,0,0,1,4,15,1,73,68,0,0,17,1,165,113,0,0,1, 
  4,15,1,73,68,0,0,17,1,50,113,0,0,1,4,15,1,73,68,0,0,17,1,191,112,0,0,1,4,15,1,73,68,0,0,17,1,76,112,0,0,1,4,15,1,73,68,0,0,17,1,217,111,0,0,1,4,15,1,73, 
  68,0,0,17,1,102,111,0,0,1,4,15,1,73,68,0,0,17,1,53,109,0,0,1,4,15,1,73,68,0,0,17,1,138,99,0,0,1,4,15,1,73,68,0,0,17,1,19,92,0,0,1,4,15,1,73,68,0,0,17, 
  1,9,87,0,0,1,4,15,1,73,68,0,0,17,1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85, 
  104,69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,73,68,0,0,1,19,87,0,0,0,5,1,0,0,1,0, 
  17,1,73,68,0,0,1,15,1,73,68,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,73,68,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,73,68,0,0,1,19,64,0,0, 
  0,177,0,0,0,1,0,17,1,73,68,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,73,68,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,73,68,0,0,1,19,64,0,0,0,174,0,0,0,1, 
  0,17,1,73,68,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,73,68,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,73,68,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,73,68,0, 
  0,1,15,1,73,68,0,0,17,1,112,69,0,0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,94,144,1,128,37,208,1,128,42,80,129,127,47,16,1,128,4,17,1,218,69,0,0,1,4,17, 
  1,87,66,0,0,1,4,17,1,224,76,0,0,1,4,17,1,93,73,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,90,0,0,0,3,0, 
  14,1,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128, 
  90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128, 
  128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,204,71,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,204,71,0,0,1,4,15,1,204,71,0,0,17,1,35,118,0,0, 
  1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,204,71,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,204,71,0,0,1,4,15,1,204,71,0,0,17,1,176,117,0,0,1,4,15,1,204,71,0,0, 
  17,1,61,117,0,0,1,4,15,1,204,71,0,0,17,1,202,116,0,0,1,4,15,1,204,71,0,0,17,1,87,116,0,0,1,4,15,1,204,71,0,0,17,1,228,115,0,0,1,4,15,1,204,71,0,0,17,1,113,115, 
  0,0,1,4,15,1,204,71,0,0,17,1,254,114,0,0,1,4,15,1,204,71,0,0,17,1,139,114,0,0,1,4,15,1,204,71,0,0,17,1,24,114,0,0,1,4,15,1,204,71,0,0,17,1,165,113,0,0,1,4, 
  15,1,204,71,0,0,17,1,50,113,0,0,1,4,15,1,204,71,0,0,17,1,191,112,0,0,1,4,15,1,204,71,0,0,17,1,76,112,0,0,1,4,15,1,204,71,0,0,17,1,217,111,0,0,1,4,15,1,204,71, 
  0,0,17,1,102,111,0,0,1,4,15,1,204,71,0,0,17,1,53,109,0,0,1,4,15,1,204,71,0,0,17,1,138,99,0,0,1,4,15,1,204,71,0,0,17,1,19,92,0,0,1,4,15,1,204,71,0,0,17,1, 
  9,87,0,0,1,4,15,1,204,71,0,0,17,1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104, 
  69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,204,71,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17, 
  1,204,71,0,0,1,15,1,204,71,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,204,71,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,204,71,0,0,1,19,64,0,0,0, 
  177,0,0,0,1,0,17,1,204,71,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,204,71,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,204,71,0,0,1,19,64,0,0,0,174,0,0,0,1,0, 
  17,1,204,71,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,204,71,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,204,71,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,204,71,0,0, 
  1,15,1,204,71,0,0,17,1,243,72,0,0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,94,80,1,128,37,208,1,128,42,16,129,127,47,144,1,128,4,17,1,87,66,0,0,1,4,17,1, 
  224,76,0,0,1,4,17,1,218,69,0,0,1,4,17,1,93,73,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,89,0,0,0,3,0,14, 
  1,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90, 
  40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128, 
  144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,79,75,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,79,75,0,0,1,4,15,1,79,75,0,0,17,1,35,118,0,0,1, 
  4,19,87,0,0,0,4,1,0,0,1,0,17,1,79,75,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,79,75,0,0,1,4,15,1,79,75,0,0,17,1,176,117,0,0,1,4,15,1,79,75,0,0,17, 
  1,61,117,0,0,1,4,15,1,79,75,0,0,17,1,202,116,0,0,1,4,15,1,79,75,0,0,17,1,87,116,0,0,1,4,15,1,79,75,0,0,17,1,228,115,0,0,1,4,15,1,79,75,0,0,17,1,113,115,0, 
  0,1,4,15,1,79,75,0,0,17,1,254,114,0,0,1,4,15,1,79,75,0,0,17,1,139,114,0,0,1,4,15,1,79,75,0,0,17,1,24,114,0,0,1,4,15,1,79,75,0,0,17,1,165,113,0,0,1,4,15, 
  1,79,75,0,0,17,1,50,113,0,0,1,4,15,1,79,75,0,0,17,1,191,112,0,0,1,4,15,1,79,75,0,0,17,1,76,112,0,0,1,4,15,1,79,75,0,0,17,1,217,111,0,0,1,4,15,1,79,75,0, 
  0,17,1,102,111,0,0,1,4,15,1,79,75,0,0,17,1,53,109,0,0,1,4,15,1,79,75,0,0,17,1,138,99,0,0,1,4,15,1,79,75,0,0,17,1,19,92,0,0,1,4,15,1,79,75,0,0,17,1,9, 
  87,0,0,1,4,15,1,79,75,0,0,17,1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69, 
  129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,79,75,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1, 
  79,75,0,0,1,15,1,79,75,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,79,75,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,79,75,0,0,1,19,64,0,0,0,177, 
  0,0,0,1,0,17,1,79,75,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,79,75,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,79,75,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17, 
  1,79,75,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,79,75,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,79,75,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,79,75,0,0,1, 
  15,1,79,75,0,0,17,1,118,76,0,0,1,2,21,7,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,94,16,1,128,37,80,1,128,42,208,129,127,47,144,1,128,4,17,1,224,76,0,0,1,4,17,1,93, 
  73,0,0,1,4,17,1,218,69,0,0,1,4,17,1,87,66,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,91,0,0,0,3,0,14,1, 
  21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40, 
  11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144, 
  4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,210,78,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,210,78,0,0,1,4,15,1,210,78,0,0,17,1,35,118,0,0,1,4, 
  19,87,0,0,0,4,1,0,0,1,0,17,1,210,78,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,210,78,0,0,1,4,15,1,210,78,0,0,17,1,176,117,0,0,1,4,15,1,210,78,0,0,17,1, 
  61,117,0,0,1,4,15,1,210,78,0,0,17,1,202,116,0,0,1,4,15,1,210,78,0,0,17,1,87,116,0,0,1,4,15,1,210,78,0,0,17,1,228,115,0,0,1,4,15,1,210,78,0,0,17,1,113,115,0,0, 
  1,4,15,1,210,78,0,0,17,1,254,114,0,0,1,4,15,1,210,78,0,0,17,1,139,114,0,0,1,4,15,1,210,78,0,0,17,1,24,114,0,0,1,4,15,1,210,78,0,0,17,1,165,113,0,0,1,4,15,1, 
  210,78,0,0,17,1,50,113,0,0,1,4,15,1,210,78,0,0,17,1,191,112,0,0,1,4,15,1,210,78,0,0,17,1,76,112,0,0,1,4,15,1,210,78,0,0,17,1,217,111,0,0,1,4,15,1,210,78,0,0, 
  17,1,102,111,0,0,1,4,15,1,210,78,0,0,17,1,53,109,0,0,1,4,15,1,210,78,0,0,17,1,138,99,0,0,1,4,15,1,210,78,0,0,17,1,19,92,0,0,1,4,15,1,210,78,0,0,17,1,9,87, 
  0,0,1,4,15,1,210,78,0,0,17,1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129, 
  86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,210,78,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,210, 
  78,0,0,1,15,1,210,78,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,210,78,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,210,78,0,0,1,19,64,0,0,0,177,0, 
  0,0,1,0,17,1,210,78,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,210,78,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,210,78,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1, 
  210,78,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,210,78,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,210,78,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,210,78,0,0,1,15, 
  1,210,78,0,0,17,1,249,79,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,94,176,0,128,4,17,1,224,76,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,19,34,0,0,0,92,0,0,0,3,0,14,1,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,16,2,128,37,80,194,127,42,144,194,128,43,80,65,128,47,208,1, 
  128,94,144,1,128,4,17,1,33,59,0,0,1,4,17,1,224,76,0,0,1,4,17,1,218,69,0,0,1,4,17,1,188,62,0,0,1,4,17,1,93,73,0,0,1,4,17,1,87,66,0,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,28,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,5,81,0, 
  0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,116,0,0,0,96,1,0,0,1,0,1,21,1,50,0,0,0,55,248,1,0,3,0,0,0,1,0,0,0,2, 
  240,0,128,1,240,64,128,41,248,0,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,56,81,0,0,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,37,88,65,128,65,248, 
  0,128,1,19,80,0,0,0,221,0,0,0,3,0,1,19,65,0,0,0,179,0,0,0,1,0,17,1,56,81,0,0,1,2,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132, 
  2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128, 
  81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,90,85,0,0,1, 
  4,19,119,0,0,0,100,1,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,90,85,0,0,1,4,19,87,0,0,0,3,1, 
  0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,17,1,176,117,0,0,1,4,15,1,90,85,0,0,17,1,61,117,0,0,1,4,15,1,90,85,0,0,17,1,202,116,0,0,1,4,15,1,90,85,0,0, 
  17,1,87,116,0,0,1,4,15,1,90,85,0,0,17,1,228,115,0,0,1,4,15,1,90,85,0,0,17,1,113,115,0,0,1,4,15,1,90,85,0,0,17,1,254,114,0,0,1,4,15,1,90,85,0,0,17,1,139,114, 
  0,0,1,4,15,1,90,85,0,0,17,1,24,114,0,0,1,4,15,1,90,85,0,0,17,1,165,113,0,0,1,4,15,1,90,85,0,0,17,1,50,113,0,0,1,4,15,1,90,85,0,0,17,1,191,112,0,0,1,4, 
  15,1,90,85,0,0,17,1,76,112,0,0,1,4,15,1,90,85,0,0,17,1,217,111,0,0,1,4,15,1,90,85,0,0,17,1,102,111,0,0,1,4,15,1,90,85,0,0,17,1,53,109,0,0,1,4,15,1,90,85, 
  0,0,17,1,138,99,0,0,1,4,15,1,90,85,0,0,17,1,19,92,0,0,1,4,15,1,90,85,0,0,17,1,9,87,0,0,1,4,15,1,90,85,0,0,17,1,104,83,0,0,1,2,21,1,241,1,0,0,69,27, 
  2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72, 
  10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4, 
  19,120,0,0,0,101,1,0,0,1,0,17,1,90,85,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0, 
  0,1,0,17,1,90,85,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,17,1,176,117,0,0,1,4,15,1,90,85,0,0,17,1,61,117,0,0,1,4,15,1, 
  90,85,0,0,17,1,202,116,0,0,1,4,15,1,90,85,0,0,17,1,87,116,0,0,1,4,15,1,90,85,0,0,17,1,228,115,0,0,1,4,15,1,90,85,0,0,17,1,113,115,0,0,1,4,15,1,90,85,0,0, 
  17,1,254,114,0,0,1,4,15,1,90,85,0,0,17,1,139,114,0,0,1,4,15,1,90,85,0,0,17,1,24,114,0,0,1,4,15,1,90,85,0,0,17,1,165,113,0,0,1,4,15,1,90,85,0,0,17,1,50,113, 
  0,0,1,4,15,1,90,85,0,0,17,1,191,112,0,0,1,4,15,1,90,85,0,0,17,1,76,112,0,0,1,4,15,1,90,85,0,0,17,1,217,111,0,0,1,4,15,1,90,85,0,0,17,1,102,111,0,0,1,4, 
  15,1,90,85,0,0,17,1,53,109,0,0,1,4,15,1,90,85,0,0,17,1,138,99,0,0,1,4,15,1,90,85,0,0,17,1,19,92,0,0,1,4,15,1,90,85,0,0,17,1,9,87,0,0,1,4,15,1,90,85, 
  0,0,17,1,104,83,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129, 
  82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,90,85,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,90,85,0,0,1,15,1,90,85, 
  0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,90,85,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,90,85,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,90,85, 
  0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,90,85,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,90,85,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,90,85,0,0,1,19,64,0, 
  0,0,173,0,0,0,1,0,17,1,90,85,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,90,85,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,15,1,90,85,0,0,17,1,129, 
  86,0,0,1,2,21,7,108,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,41,112,2,129,37,224,194,127,42,48,2,129,43,32,131,128,45,240,1,128,47,176,1,128,94,112,1,128,4,17,1,224,76,0,0,1,4, 
  17,1,218,69,0,0,1,4,17,1,188,62,0,0,1,4,17,1,87,66,0,0,1,4,19,34,0,0,0,94,0,0,0,3,0,14,1,4,17,1,93,73,0,0,1,4,17,1,33,59,0,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101, 
  48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97, 
  24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,237,90,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17, 
  1,237,90,0,0,1,4,15,1,237,90,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,237,90,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,237,90,0,0,1,4,15, 
  1,237,90,0,0,17,1,176,117,0,0,1,4,15,1,237,90,0,0,17,1,61,117,0,0,1,4,15,1,237,90,0,0,17,1,202,116,0,0,1,4,15,1,237,90,0,0,17,1,87,116,0,0,1,4,15,1,237,90,0, 
  0,17,1,228,115,0,0,1,4,15,1,237,90,0,0,17,1,113,115,0,0,1,4,15,1,237,90,0,0,17,1,254,114,0,0,1,4,15,1,237,90,0,0,17,1,139,114,0,0,1,4,15,1,237,90,0,0,17,1,24, 
  114,0,0,1,4,15,1,237,90,0,0,17,1,165,113,0,0,1,4,15,1,237,90,0,0,17,1,50,113,0,0,1,4,15,1,237,90,0,0,17,1,191,112,0,0,1,4,15,1,237,90,0,0,17,1,76,112,0,0,1, 
  4,15,1,237,90,0,0,17,1,217,111,0,0,1,4,15,1,237,90,0,0,17,1,102,111,0,0,1,4,15,1,237,90,0,0,17,1,53,109,0,0,1,4,15,1,237,90,0,0,17,1,138,99,0,0,1,4,15,1,237, 
  90,0,0,17,1,19,92,0,0,1,4,15,1,237,90,0,0,17,1,251,88,0,0,1,4,15,1,237,90,0,0,17,1,118,81,0,0,1,2,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72, 
  134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15, 
  129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1, 
  237,90,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,237,90,0,0,1,4,15,1,237,90,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,237,90,0,0,1,4,19,87, 
  0,0,0,3,1,0,0,1,0,17,1,237,90,0,0,1,4,15,1,237,90,0,0,17,1,176,117,0,0,1,4,15,1,237,90,0,0,17,1,61,117,0,0,1,4,15,1,237,90,0,0,17,1,202,116,0,0,1,4,15, 
  1,237,90,0,0,17,1,87,116,0,0,1,4,15,1,237,90,0,0,17,1,228,115,0,0,1,4,15,1,237,90,0,0,17,1,113,115,0,0,1,4,15,1,237,90,0,0,17,1,254,114,0,0,1,4,15,1,237,90,0, 
  0,17,1,139,114,0,0,1,4,15,1,237,90,0,0,17,1,24,114,0,0,1,4,15,1,237,90,0,0,17,1,165,113,0,0,1,4,15,1,237,90,0,0,17,1,50,113,0,0,1,4,15,1,237,90,0,0,17,1,191, 
  112,0,0,1,4,15,1,237,90,0,0,17,1,76,112,0,0,1,4,15,1,237,90,0,0,17,1,217,111,0,0,1,4,15,1,237,90,0,0,17,1,102,111,0,0,1,4,15,1,237,90,0,0,17,1,53,109,0,0,1, 
  4,15,1,237,90,0,0,17,1,138,99,0,0,1,4,15,1,237,90,0,0,17,1,19,92,0,0,1,4,15,1,237,90,0,0,17,1,251,88,0,0,1,4,15,1,237,90,0,0,17,1,118,81,0,0,1,2,21,0,37, 
  1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119, 
  192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,237,90,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,237,90,0,0,1,15,1,237,90,0,0,17,1,192,80,0,0,1,19,80,0,0, 
  0,220,0,0,0,1,0,17,1,237,90,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,237,90,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,237,90,0,0,1,19,64,0,0,0,176,0,0,0,1, 
  0,17,1,237,90,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,237,90,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,237,90,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,237,90,0, 
  0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,237,90,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0,0,1,19,34,0,0,0,93,0,0,0,2,0,1,2,21,7,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,40,176,0,128,4,17,1,77,92,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,241,1,0,0,69,27,2,0,27, 
  0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93, 
  216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0, 
  0,0,101,1,0,0,1,0,17,1,63,94,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,63,94,0,0,1,4,15,1,63,94,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0, 
  17,1,63,94,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,63,94,0,0,1,4,15,1,63,94,0,0,17,1,176,117,0,0,1,4,15,1,63,94,0,0,17,1,61,117,0,0,1,4,15,1,63,94,0, 
  0,17,1,202,116,0,0,1,4,15,1,63,94,0,0,17,1,87,116,0,0,1,4,15,1,63,94,0,0,17,1,228,115,0,0,1,4,15,1,63,94,0,0,17,1,113,115,0,0,1,4,15,1,63,94,0,0,17,1,254, 
  114,0,0,1,4,15,1,63,94,0,0,17,1,139,114,0,0,1,4,15,1,63,94,0,0,17,1,24,114,0,0,1,4,15,1,63,94,0,0,17,1,165,113,0,0,1,4,15,1,63,94,0,0,17,1,50,113,0,0,1, 
  4,15,1,63,94,0,0,17,1,191,112,0,0,1,4,15,1,63,94,0,0,17,1,76,112,0,0,1,4,15,1,63,94,0,0,17,1,217,111,0,0,1,4,15,1,63,94,0,0,17,1,102,111,0,0,1,4,15,1,63, 
  94,0,0,17,1,53,109,0,0,1,4,15,1,63,94,0,0,17,1,138,99,0,0,1,4,15,1,63,94,0,0,17,1,19,92,0,0,1,4,15,1,63,94,0,0,17,1,9,87,0,0,1,4,15,1,63,94,0,0,17, 
  1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129,82,136,6, 
  128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,63,94,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,63,94,0,0,1,15,1,63,94,0,0,17, 
  1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,63,94,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,63,94,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,63,94,0,0,1, 
  19,64,0,0,0,176,0,0,0,1,0,17,1,63,94,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,63,94,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,63,94,0,0,1,19,64,0,0,0,173, 
  0,0,0,1,0,17,1,63,94,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,63,94,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,63,94,0,0,1,15,1,63,94,0,0,17,1,102,95,0,0, 
  1,2,21,7,102,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,44,240,2,128,37,112,194,128,42,48,2,129,43,176,130,128,45,240,1,128,47,176,1,128,94,112,1,128,4,17,1,224,76,0,0,1,4,17,1,218, 
  69,0,0,1,4,17,1,188,62,0,0,1,4,17,1,87,66,0,0,1,4,17,1,93,73,0,0,1,4,17,1,33,59,0,0,1,4,17,1,232,95,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,2,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99,56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120, 
  12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82,200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200, 
  6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,218,97,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,218,97,0,0,1,4,15,1, 
  218,97,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,218,97,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,218,97,0,0,1,4,15,1,218,97,0,0,17,1,176,117, 
  0,0,1,4,15,1,218,97,0,0,17,1,61,117,0,0,1,4,15,1,218,97,0,0,17,1,202,116,0,0,1,4,15,1,218,97,0,0,17,1,87,116,0,0,1,4,15,1,218,97,0,0,17,1,228,115,0,0,1,4, 
  15,1,218,97,0,0,17,1,113,115,0,0,1,4,15,1,218,97,0,0,17,1,254,114,0,0,1,4,15,1,218,97,0,0,17,1,139,114,0,0,1,4,15,1,218,97,0,0,17,1,24,114,0,0,1,4,15,1,218,97, 
  0,0,17,1,165,113,0,0,1,4,15,1,218,97,0,0,17,1,50,113,0,0,1,4,15,1,218,97,0,0,17,1,191,112,0,0,1,4,15,1,218,97,0,0,17,1,76,112,0,0,1,4,15,1,218,97,0,0,17,1, 
  217,111,0,0,1,4,15,1,218,97,0,0,17,1,102,111,0,0,1,4,15,1,218,97,0,0,17,1,53,109,0,0,1,4,15,1,218,97,0,0,17,1,138,99,0,0,1,4,15,1,218,97,0,0,17,1,19,92,0,0, 
  1,4,15,1,218,97,0,0,17,1,9,87,0,0,1,4,15,1,218,97,0,0,17,1,118,81,0,0,1,2,21,0,38,1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129, 
  80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119,192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,218,97,0,0,1,19,87, 
  0,0,0,5,1,0,0,1,0,17,1,218,97,0,0,1,15,1,218,97,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,218,97,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1, 
  218,97,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,218,97,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,218,97,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,218,97,0,0,1,19, 
  64,0,0,0,174,0,0,0,1,0,17,1,218,97,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,218,97,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,218,97,0,0,1,19,34,0,0,0,95,0, 
  0,0,1,0,17,1,218,97,0,0,1,15,1,218,97,0,0,17,1,1,99,0,0,1,2,21,7,109,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,41,112,2,129,37,232,194,127,42,48,2,129,43,40,131,128,45, 
  240,1,128,47,176,1,128,94,112,1,128,4,17,1,224,76,0,0,1,4,17,1,218,69,0,0,1,4,17,1,188,62,0,0,1,4,17,1,87,66,0,0,1,4,19,81,0,0,0,222,0,0,0,6,0,14,14,1,4,17, 
  1,93,73,0,0,1,4,17,1,33,59,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,2,2,0,0,39,28,2,0,28,0,0,0,4,0,0,0, 
  96,168,136,134,1,16,4,132,2,16,132,131,83,232,205,132,84,120,205,132,101,80,6,128,86,8,141,132,87,152,12,128,88,40,12,128,89,184,11,128,90,72,11,128,91,216,10,128,92,104,10,128,93,248,9,128,94,136,9,128, 
  31,160,15,129,34,48,143,128,81,192,206,128,82,80,206,128,95,24,137,129,97,56,200,129,98,200,7,128,99,88,7,128,100,232,6,128,102,184,5,128,127,72,5,128,128,176,4,128,129,24,4,128,8,4,19,120,0,0,0,101, 
  1,0,0,1,0,17,1,144,103,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,144,103,0,0,1,4,15,1,144,103,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,144, 
  103,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,144,103,0,0,1,4,15,1,144,103,0,0,17,1,176,117,0,0,1,4,15,1,144,103,0,0,17,1,61,117,0,0,1,4,15,1,144,103,0,0,17,1, 
  202,116,0,0,1,4,15,1,144,103,0,0,17,1,87,116,0,0,1,4,15,1,144,103,0,0,17,1,228,115,0,0,1,4,15,1,144,103,0,0,17,1,113,115,0,0,1,4,15,1,144,103,0,0,17,1,254,114,0,0, 
  1,4,15,1,144,103,0,0,17,1,139,114,0,0,1,4,15,1,144,103,0,0,17,1,24,114,0,0,1,4,15,1,144,103,0,0,17,1,165,113,0,0,1,4,15,1,144,103,0,0,17,1,50,113,0,0,1,4,15,1, 
  144,103,0,0,17,1,191,112,0,0,1,4,15,1,144,103,0,0,17,1,76,112,0,0,1,4,15,1,144,103,0,0,17,1,217,111,0,0,1,4,15,1,144,103,0,0,17,1,102,111,0,0,1,4,15,1,144,103,0,0, 
  17,1,53,109,0,0,1,4,19,82,0,0,0,224,0,0,0,2,0,1,4,15,1,144,103,0,0,17,1,141,101,0,0,1,4,15,1,144,103,0,0,17,1,19,92,0,0,1,4,15,1,144,103,0,0,17,1,9,87,0, 
  0,1,4,15,1,144,103,0,0,17,1,118,81,0,0,1,2,21,1,2,2,0,0,39,28,2,0,28,0,0,0,4,0,0,0,96,168,136,134,1,16,4,132,2,16,132,131,83,232,205,132,84,120,205,132,101,80,6,128,86, 
  8,141,132,87,152,12,128,88,40,12,128,89,184,11,128,90,72,11,128,91,216,10,128,92,104,10,128,93,248,9,128,94,136,9,128,31,160,15,129,34,48,143,128,81,192,206,128,82,80,206,128,95,24,137,129,97,56,200,129,98, 
  200,7,128,99,88,7,128,100,232,6,128,102,184,5,128,127,72,5,128,128,176,4,128,129,24,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,144,103,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17, 
  1,144,103,0,0,1,4,15,1,144,103,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,144,103,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,144,103,0,0,1,4,15, 
  1,144,103,0,0,17,1,176,117,0,0,1,4,15,1,144,103,0,0,17,1,61,117,0,0,1,4,15,1,144,103,0,0,17,1,202,116,0,0,1,4,15,1,144,103,0,0,17,1,87,116,0,0,1,4,15,1,144,103,0, 
  0,17,1,228,115,0,0,1,4,15,1,144,103,0,0,17,1,113,115,0,0,1,4,15,1,144,103,0,0,17,1,254,114,0,0,1,4,15,1,144,103,0,0,17,1,139,114,0,0,1,4,15,1,144,103,0,0,17,1,24, 
  114,0,0,1,4,15,1,144,103,0,0,17,1,165,113,0,0,1,4,15,1,144,103,0,0,17,1,50,113,0,0,1,4,15,1,144,103,0,0,17,1,191,112,0,0,1,4,15,1,144,103,0,0,17,1,76,112,0,0,1, 
  4,15,1,144,103,0,0,17,1,217,111,0,0,1,4,15,1,144,103,0,0,17,1,102,111,0,0,1,4,15,1,144,103,0,0,17,1,53,109,0,0,1,4,19,82,0,0,0,224,0,0,0,2,0,1,4,15,1,144,103, 
  0,0,17,1,141,101,0,0,1,4,15,1,144,103,0,0,17,1,19,92,0,0,1,4,15,1,144,103,0,0,17,1,9,87,0,0,1,4,15,1,144,103,0,0,17,1,118,81,0,0,1,2,21,0,55,1,0,0,255,255, 
  255,255,14,0,0,0,3,0,0,0,64,192,8,130,81,160,7,128,34,80,201,129,83,168,6,128,84,24,134,129,85,136,133,129,86,248,4,128,87,104,68,129,80,48,72,129,82,16,7,128,116,216,3,128,117,112,3,128,119,224, 
  2,128,120,80,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,144,103,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,144,103,0,0,1,15,1,144,103,0,0,17,1,192,80,0,0,1,19,80,0,0,0, 
  220,0,0,0,1,0,17,1,144,103,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,144,103,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,144,103,0,0,1,19,64,0,0,0,176,0,0,0,1,0, 
  17,1,144,103,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,144,103,0,0,1,15,1,144,103,0,0,17,1,73,105,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,144,103,0,0,1,19,64,0,0, 
  0,173,0,0,0,1,0,17,1,144,103,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,144,103,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,144,103,0,0,1,15,1,144,103,0,0,17,1,200,104, 
  0,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,16,2,128,37,144,193,127,42,80,194,128,43,208,65,128,47,144,2,128,94,80,1,128,4,17,1,224,76,0,0,1,4,17,1,93,73,0, 
  0,1,4,17,1,33,59,0,0,1,4,17,1,188,62,0,0,1,4,17,1,87,66,0,0,1,4,17,1,218,69,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,19,83,0,0,0,225,0,0,0,1,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,1,128,93,208,0,128,4,19,82,0,0,0,223,0,0,0,3,0,14,1,4,17,1,149,105,0,0, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,241,1,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,136,72,134,1,240,3,132,2,240,131,131,99, 
  56,7,128,84,88,141,132,101,48,6,128,86,232,76,132,87,120,12,128,88,8,12,128,89,152,11,128,90,40,11,128,91,184,10,128,92,72,10,128,93,216,9,128,94,104,9,128,31,24,15,129,34,168,142,128,81,56,206,128,82, 
  200,205,128,95,248,72,129,97,24,136,129,98,168,7,128,100,200,6,128,102,152,5,128,127,40,5,128,128,144,4,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,135,107,0,0,1,4,19,119,0,0, 
  0,100,1,0,0,1,0,17,1,135,107,0,0,1,4,15,1,135,107,0,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,135,107,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17, 
  1,135,107,0,0,1,4,15,1,135,107,0,0,17,1,176,117,0,0,1,4,15,1,135,107,0,0,17,1,61,117,0,0,1,4,15,1,135,107,0,0,17,1,202,116,0,0,1,4,15,1,135,107,0,0,17,1,87,116,0, 
  0,1,4,15,1,135,107,0,0,17,1,228,115,0,0,1,4,15,1,135,107,0,0,17,1,113,115,0,0,1,4,15,1,135,107,0,0,17,1,254,114,0,0,1,4,15,1,135,107,0,0,17,1,139,114,0,0,1,4,15, 
  1,135,107,0,0,17,1,24,114,0,0,1,4,15,1,135,107,0,0,17,1,165,113,0,0,1,4,15,1,135,107,0,0,17,1,50,113,0,0,1,4,15,1,135,107,0,0,17,1,191,112,0,0,1,4,15,1,135,107,0, 
  0,17,1,76,112,0,0,1,4,15,1,135,107,0,0,17,1,217,111,0,0,1,4,15,1,135,107,0,0,17,1,102,111,0,0,1,4,15,1,135,107,0,0,17,1,53,109,0,0,1,4,15,1,135,107,0,0,17,1,138, 
  99,0,0,1,4,15,1,135,107,0,0,17,1,19,92,0,0,1,4,15,1,135,107,0,0,17,1,9,87,0,0,1,4,15,1,135,107,0,0,17,1,118,81,0,0,1,2,21,0,43,1,0,0,255,255,255,255,14,0,0, 
  0,3,0,0,0,64,96,8,130,81,64,7,128,34,240,200,129,83,168,6,128,84,24,134,129,85,136,133,129,86,248,4,128,87,104,68,129,80,208,71,129,82,176,6,128,116,216,3,128,117,112,3,128,119,224,2,128,120,80,2, 
  128,19,87,0,0,0,6,1,0,0,1,0,17,1,135,107,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,135,107,0,0,1,15,1,135,107,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1, 
  0,17,1,135,107,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,135,107,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,135,107,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,135,107,0, 
  0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,135,107,0,0,1,1,19,64,0,0,0,174,0,0,0,1,0,17,1,135,107,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,135,107,0,0,1,19,64,0, 
  0,0,172,0,0,0,1,0,17,1,135,107,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,135,107,0,0,1,15,1,135,107,0,0,17,1,179,108,0,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0, 
  0,2,0,0,0,45,144,2,128,37,208,193,127,42,80,193,128,43,16,66,128,47,144,1,128,94,80,2,128,4,17,1,87,66,0,0,1,4,17,1,218,69,0,0,1,4,17,1,93,73,0,0,1,4,17,1,33,59,0,0, 
  1,4,17,1,224,76,0,0,1,4,17,1,188,62,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,83,0,0,0,226,0,0,0,3,0,14,1,21,7,36, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,62,111,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  19,84,0,0,0,228,0,0,0,1,0,1,21,1,191,0,0,0,20,29,2,0,10,0,0,0,3,0,0,0,128,112,2,128,1,208,1,130,2,208,1,128,70,168,68,129,101,16,4,128,69,24,197,127,54,136,69,127,127,8, 
  3,128,102,120,3,128,129,216,1,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,64,110,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,64,110,0,0,1,4,15,1,64,110,0,0,17,1,35,118, 
  0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,64,110,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,64,110,0,0,1,4,15,1,64,110,0,0,17,1,185,52,0,0,1,4,15,1,64,110, 
  0,0,17,1,70,52,0,0,1,4,15,1,64,110,0,0,17,1,142,50,0,0,1,2,21,0,189,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,80,200,4,130,137,176,1,128,66,88,5,128,119,168,2,128,116,168, 
  3,128,117,64,3,128,118,56,3,128,87,56,4,127,120,24,2,128,15,1,64,110,0,0,17,1,254,110,0,0,1,19,87,0,0,0,6,1,0,0,1,0,17,1,64,110,0,0,1,19,87,0,0,0,5,1,0,0,1,0, 
  17,1,64,110,0,0,1,1,15,1,64,110,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,64,110,0,0,1,19,137,0,0,0,139,1,0,0,1,0,17,1,64,110,0,0,1,19,137,0, 
  0,0,137,1,0,0,1,0,17,1,64,110,0,0,1,19,137,0,0,0,138,1,0,0,1,0,17,1,64,110,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,118,0, 
  0,0,99,1,0,0,3,0,14,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,84,48,65,128, 
  118,208,0,128,19,84,0,0,0,227,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,177,111,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,85,0,0,0,230,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,118,208,0,128,85,48,1,128,19, 
  85,0,0,0,229,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,36,112,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,242,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,241, 
  0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,151,112,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,232,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,231,0,0,0,2,0, 
  1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,10,113,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,208,0,128,8,19,86,0,0,0,254,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,253,0,0,0,2,0,1,1,2,21,7, 
  36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,125,113,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,19,86,0,0,0,246,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,245,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,240,113,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0, 
  0,236,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,235,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,40,176,0,128,4,15,1,99,114,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,0,1,0,0, 
  1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,255,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,40,176,0,128,4,15,1,214,114,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,248,0,0,0,1,0,1,21,0, 
  39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,247,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128, 
  4,15,1,73,115,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,238,0,0,0,1,0,1,21,0,39,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,237,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,188,115, 
  0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,252,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,251,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,47,116,0,0,17,1,128, 
  109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,244,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,86,48,65,128,118,208,0,128,19,86,0,0,0,243,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,162,116,0,0,17,1,128,109,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,234,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128, 
  118,208,0,128,19,86,0,0,0,233,0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,21,117,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,2,1,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19, 
  86,0,0,0,1,1,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,136,117,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,250,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,249, 
  0,0,0,2,0,1,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,251,117,0,0,17,1,128,109,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,240,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,86,48,65,128,118,208,0,128,19,86,0,0,0,239,0,0,0,2,0, 
  1,1,2,21,1,73,0,0,0,90,30,2,0,4,0,0,0,2,0,0,0,72,24,1,128,1,16,129,128,2,16,1,128,41,176,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,109,118,0,0,1,4,19,37, 
  0,0,0,101,0,0,0,1,0,17,1,109,118,0,0,1,2,21,0,55,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,248,128,128,37,88,193,127,117,240,0,128,1,19,117,0,0,0,98,1,0,0,2,0,1, 
  19,117,0,0,0,97,1,0,0,2,0,1,2,21,0,103,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,64,32,201,130,81,160,7,128,34,176,137,130,115,200,4,128,68,192,72,130,85,240,133,130,86,96,5,128,87, 
  208,68,130,120,176,2,128,9,168,10,128,10,24,10,128,80,48,8,128,82,16,7,128,84,128,70,128,116,56,4,128,117,208,3,128,119,64,3,128,19,87,0,0,0,6,1,0,0,1,0,17,1,165,118,0,0,1,19,87,0, 
  0,0,5,1,0,0,1,0,17,1,165,118,0,0,1,15,1,165,118,0,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,165,118,0,0,1,1,19,64,0,0,0,178,0,0,0,1,0,17,1, 
  165,118,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,165,118,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,165,118,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,165,118,0,0,1,19, 
  64,0,0,0,174,0,0,0,1,0,17,1,165,118,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,165,118,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,165,118,0,0,1,19,115,0,0,0,95,1, 
  0,0,2,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,165,118,0,0,1,15,1,165,118,0,0,17,1,13,120,0,0,1,19,68,0,0,0,189,0,0,0,1,0,17,1,165,118,0,0,1,19,68,0,0,0,190, 
  0,0,0,1,0,17,1,165,118,0,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,144,1,128,37,80,193,127,42,80,194,128,43,208,65,128,47,144,2,128,94,16,2,128,4,17,1,93,73, 
  0,0,1,4,17,1,188,62,0,0,1,4,17,1,33,59,0,0,1,4,17,1,224,76,0,0,1,4,17,1,87,66,0,0,1,4,17,1,218,69,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,28,0,0,0,1,0,1,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,115,208,0,128,75,48,193,127,19,75,0,0,0,208,0,0,0,2,0,1, 
  1,2,21,1,69,0,0,0,26,11,2,0,5,0,0,0,2,0,0,0,120,232,1,128,1,224,1,128,2,224,129,128,123,48,1,128,10,112,1,128,4,17,1,30,35,0,0,1,4,15,1,7,121,0,0,17,1,75,25,0, 
  0,1,8,4,17,1,171,42,0,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,7,121,0,0,17,1,48,121,0, 
  0,1,1,2,21,1,51,0,0,0,195,13,2,0,4,0,0,0,2,0,0,0,120,80,1,128,1,144,1,128,2,144,1,128,123,16,1,128,4,17,1,99,28,0,0,1,4,17,1,188,43,0,0,1,8,19,112,0,0,0, 
  78,1,0,0,2,0,14,1,21,1,85,0,0,0,21,10,2,0,6,0,0,0,2,0,0,0,136,184,1,128,1,160,194,128,2,160,2,128,135,248,1,128,25,56,66,128,33,80,1,128,4,19,129,0,0,0,113,1,0,0, 
  2,0,1,4,17,1,159,19,0,0,1,4,17,1,242,20,0,0,1,4,19,129,0,0,0,112,1,0,0,2,0,1,8,19,129,0,0,0,120,1,0,0,1,0,1,21,1,39,0,0,0,13,8,2,0,3,0,0,0,1, 
  0,0,0,2,48,1,128,1,48,65,128,139,240,0,128,4,17,1,77,16,0,0,1,8,19,138,0,0,0,140,1,0,0,1,0,1,21,1,217,0,0,0,61,9,2,0,13,0,0,0,3,0,0,0,112,56,3,128,1,184, 
  5,129,2,184,133,129,31,200,194,129,76,72,5,128,41,24,132,129,6,184,197,128,7,184,5,127,82,88,6,128,110,168,195,128,127,176,4,128,129,48,2,128,134,192,5,128,4,19,120,0,0,0,101,1,0,0,1,0,17,1, 
  233,122,0,0,1,4,15,1,233,122,0,0,17,1,116,10,0,0,1,4,15,1,233,122,0,0,17,1,214,126,0,0,1,4,15,1,233,122,0,0,17,1,229,36,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17, 
  1,233,122,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,233,122,0,0,1,4,15,1,233,122,0,0,17,1,40,12,0,0,1,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,233,122,0,0,1,4, 
  15,1,233,122,0,0,17,1,220,13,0,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,80,70,130,113,160,199,131,114,176,6,128,61,96,68,128,77, 
  56,131,128,37,32,9,128,109,24,10,128,110,16,7,128,120,64,3,128,128,136,9,128,74,144,200,129,139,208,3,128,108,200,4,128,13,0,136,125,78,56,67,126,79,192,5,128,129,88,5,128,138,208,2,128,15,1,233,122,0, 
  0,17,1,249,41,0,0,1,1,19,108,0,0,0,54,1,0,0,1,0,17,1,233,122,0,0,1,19,138,0,0,0,141,1,0,0,1,0,17,1,233,122,0,0,1,15,1,233,122,0,0,17,1,209,121,0,0,1,19,74, 
  0,0,0,205,0,0,0,1,0,17,1,233,122,0,0,1,15,1,233,122,0,0,17,1,90,42,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,233,122,0,0,1,19,77,0,0,0,214,0,0,0,2,0,1,19, 
  78,0,0,0,216,0,0,0,2,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,233,122,0,0,1,19,77,0,0,0,215,0,0,0,2,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,233,122,0,0,1,19, 
  138,0,0,0,142,1,0,0,1,0,17,1,233,122,0,0,1,15,1,233,122,0,0,17,1,198,41,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,233,122,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17, 
  1,233,122,0,0,1,2,21,1,53,0,0,0,166,14,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2,160,129,127,7,160,1,128,45,48,1,128,4,15,1,142,120,0,0,17,1,55,44,0,0,1,8, 
  19,75,0,0,0,209,0,0,0,1,0,1,21,1,69,0,0,0,26,11,2,0,5,0,0,0,2,0,0,0,120,48,1,128,1,176,1,128,2,176,129,128,123,112,1,128,10,184,1,128,4,17,1,58,125,0,0,1,4,17, 
  1,30,35,0,0,1,8,4,15,1,209,124,0,0,17,1,75,25,0,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15, 
  1,209,124,0,0,17,1,250,124,0,0,1,1,2,21,1,51,0,0,0,195,13,2,0,4,0,0,0,2,0,0,0,120,80,1,128,1,144,1,128,2,144,1,128,123,16,1,128,4,17,1,99,28,0,0,1,4,17,1,188, 
  43,0,0,1,8,19,112,0,0,0,78,1,0,0,2,0,14,1,21,1,57,0,0,0,8,12,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,192,1,128,2,192,129,127,123,128,1,128,4,15,1,240,42,0,0,17, 
  1,75,25,0,0,1,4,17,1,130,31,0,0,1,8,19,112,0,0,0,80,1,0,0,2,0,1,21,1,85,0,0,0,21,10,2,0,6,0,0,0,2,0,0,0,136,184,1,128,1,96,194,128,2,96,2,128,135,104,2, 
  128,25,248,65,128,33,80,1,128,4,19,129,0,0,0,113,1,0,0,2,0,1,4,17,1,159,19,0,0,1,4,19,129,0,0,0,112,1,0,0,2,0,1,8,4,17,1,242,20,0,0,1,19,129,0,0,0,120,1,0, 
  0,1,0,1,21,1,176,0,0,0,67,249,1,0,11,0,0,0,3,0,0,0,112,40,3,128,1,240,1,129,2,240,1,128,31,16,5,128,76,8,4,128,41,120,4,129,6,240,129,128,7,240,1,127,110,152,131,128,129,144, 
  2,128,134,248,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,141,14,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,141,14,0,0,1,4,15,1,141,14,0,0,17,1,214,126,0,0,1,4, 
  15,1,141,14,0,0,17,1,145,126,0,0,1,4,15,1,141,14,0,0,17,1,46,5,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,141,14,0,0,1,4,15,1,141,14,0,0,17,1,122,3,0,0,1, 
  2,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0,0,0,41,24,1,128,1,16,193,127,2,16,1,128,31,176,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,42,37,0,0,1,4,15,1,42,37, 
  0,0,17,1,122,3,0,0,1,2,21,1,180,0,0,0,191,30,2,0,11,0,0,0,3,0,0,0,113,104,4,128,1,240,193,127,2,240,129,129,67,56,133,129,116,48,3,128,117,200,2,128,118,96,2,128,111,208,196,128, 
  114,0,4,128,115,152,3,128,119,248,1,128,8,4,19,110,0,0,0,64,1,0,0,2,0,1,4,19,110,0,0,0,62,1,0,0,2,0,1,4,19,110,0,0,0,57,1,0,0,2,0,1,4,19,110,0,0,0,63,1, 
  0,0,2,0,1,4,19,110,0,0,0,59,1,0,0,2,0,1,4,19,110,0,0,0,58,1,0,0,2,0,1,4,19,110,0,0,0,60,1,0,0,2,0,1,4,19,110,0,0,0,65,1,0,0,2,0,1,4,19,110, 
  0,0,0,61,1,0,0,2,0,1,2,21,0,184,1,0,0,255,255,255,255,21,0,0,0,4,0,0,0,112,208,70,132,113,64,70,132,114,176,5,128,61,144,203,128,75,112,10,132,37,200,12,128,77,8,202,128,108,128,8, 
  128,120,32,5,128,109,240,7,128,74,0,75,130,59,96,76,126,60,248,203,126,13,48,141,125,78,160,137,128,79,16,9,128,110,96,7,128,128,144,4,128,129,40,4,128,138,192,3,128,139,48,3,128,19,138,0,0,0,141,1, 
  0,0,1,0,17,1,139,127,0,0,1,15,1,139,127,0,0,17,1,249,41,0,0,1,15,1,139,127,0,0,17,1,181,131,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,139,127,0,0,1,19,108,0,0,0, 
  54,1,0,0,1,0,17,1,139,127,0,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,139,127,0,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,139,127,0,0,1,19,77,0,0,0,212,0,0,0,1,0, 
  17,1,139,127,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,139,127,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,139,127,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,139,127,0,0, 
  1,19,61,0,0,0,167,0,0,0,1,0,17,1,139,127,0,0,1,15,1,139,127,0,0,17,1,63,124,0,0,1,15,1,139,127,0,0,17,1,122,129,0,0,1,19,60,0,0,0,163,0,0,0,1,0,17,1,139,127, 
  0,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,139,127,0,0,1,15,1,139,127,0,0,17,1,209,121,0,0,1,15,1,139,127,0,0,17,1,95,9,0,0,1,15,1,139,127,0,0,17,1,68,129,0,0,1, 
  15,1,139,127,0,0,17,1,155,8,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,139,127,0,0,1,2,21,1,53,0,0,0,144,248,1,0,5,0,0,0,2,0,0,0,32,56,1,128,1,48,1,128,2,48, 
  129,128,7,48,1,128,6,48,1,128,8,4,19,61,0,0,0,165,0,0,0,3,0,14,1,2,21,1,217,0,0,0,61,9,2,0,13,0,0,0,3,0,0,0,112,64,4,128,1,80,6,129,2,80,134,129,31,72,197,129, 
  76,88,6,128,41,56,131,129,6,80,198,128,7,80,6,127,82,208,3,128,110,200,194,128,127,48,2,128,129,176,4,128,134,184,5,128,4,19,114,0,0,0,94,1,0,0,1,0,17,1,95,130,0,0,1,4,15,1,95,130, 
  0,0,17,1,229,36,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,95,130,0,0,1,4,15,1,95,130,0,0,17,1,220,13,0,0,1,4,15,1,95,130,0,0,17,1,214,126,0,0,1,4,19,120,0, 
  0,0,101,1,0,0,1,0,17,1,95,130,0,0,1,4,15,1,95,130,0,0,17,1,116,10,0,0,1,4,19,128,0,0,0,111,1,0,0,1,0,17,1,95,130,0,0,1,8,4,15,1,95,130,0,0,17,1,40,12, 
  0,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,184,68,130,113,72,202,131,114,96,3,128,61,80,68,128,77,24,133,128,37,24,6,128,109,184,9, 
  128,110,160,7,128,120,192,3,128,128,40,9,128,74,16,199,129,139,128,6,128,108,32,5,128,13,208,130,125,78,24,69,126,79,48,8,128,129,176,5,128,138,192,8,128,19,61,0,0,0,166,0,0,0,1,0,17,1,95,130, 
  0,0,1,19,78,0,0,0,216,0,0,0,2,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,95,130,0,0,1,15,1,95,130,0,0,17,1,209,121,0,0,1,19,77,0,0,0,214,0,0,0,2,0,1,1,19, 
  74,0,0,0,205,0,0,0,1,0,17,1,95,130,0,0,1,15,1,95,130,0,0,17,1,90,42,0,0,1,15,1,95,130,0,0,17,1,198,41,0,0,1,19,138,0,0,0,141,1,0,0,1,0,17,1,95,130,0,0, 
  1,19,138,0,0,0,142,1,0,0,1,0,17,1,95,130,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,95,130,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,95,130,0,0,1,15,1,95,130,0, 
  0,17,1,249,41,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,95,130,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,95,130,0,0,1,19,77,0,0,0,215,0,0,0,2,0,1,2,21,1,69, 
  0,0,0,26,11,2,0,5,0,0,0,2,0,0,0,120,232,1,128,1,224,1,128,2,224,129,128,123,48,1,128,10,112,1,128,4,17,1,175,28,0,0,1,4,15,1,75,132,0,0,17,1,75,25,0,0,1,8,4,17, 
  1,6,132,0,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,1,57,0,0,0,8,12,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,192,1,128,2,192,129,127,123,128,1,128,4,15,1,240,42,0,0,17, 
  1,75,25,0,0,1,4,17,1,130,31,0,0,1,8,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,75,132,0,0,17, 
  1,116,132,0,0,1,1,2,21,1,51,0,0,0,195,13,2,0,4,0,0,0,2,0,0,0,120,16,1,128,1,80,1,128,2,80,1,128,123,88,1,128,4,17,1,188,43,0,0,1,8,4,17,1,99,28,0,0,1,19, 
  112,0,0,0,78,1,0,0,2,0,14,1,21,0,84,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,37,168,129,128,13,16,194,127,61,160,1,128,79,16,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,180, 
  132,0,0,1,1,15,1,180,132,0,0,17,1,9,133,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,180,132,0,0,1,2,21,1,39,0,0,0,196,35,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1, 
  48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,47,0,0,0,25,47,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112, 
  1,128,45,48,1,128,4,17,1,119,133,0,0,1,8,19,38,0,0,0,109,0,0,0,3,0,1,21,1,137,4,0,0,17,15,2,0,30,0,0,0,4,0,0,0,96,104,17,135,1,80,4,132,2,80,132,131,99,216,13, 
  128,84,120,94,133,101,0,11,128,6,80,68,131,7,80,68,131,88,232,26,128,89,184,25,128,10,168,227,130,91,88,23,128,92,40,22,128,93,248,20,128,94,200,19,128,31,216,226,129,34,8,162,128,81,216,160,129,82,168,159, 
  129,86,72,221,129,87,24,28,128,90,136,24,128,95,152,82,129,97,56,144,129,98,8,15,128,100,168,12,128,102,88,9,128,127,88,8,128,128,88,6,128,129,88,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87, 
  0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,236,205,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1,4,19,119,0,0,0,100,1,0,0, 
  1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,236,205,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1,4,15,1,236,205,0, 
  0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0, 
  15,1,236,205,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,236,205,0,0,15, 
  1,248,205,0,0,17,1,63,204,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,236,205,0,0,15,1,248, 
  205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,70,202,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63, 
  204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1, 
  4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,236,205,0, 
  0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87, 
  203,0,0,17,1,142,199,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0, 
  0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137, 
  198,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0, 
  0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15, 
  1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,17,1,197,177,0, 
  0,1,4,15,1,236,205,0,0,15,1,248,205,0,0,15,1,63,204,0,0,17,1,73,171,0,0,1,4,15,1,236,205,0,0,15,1,252,149,0,0,17,1,1,138,0,0,1,2,21,1,229,3,0,0,83,22,2,0,28, 
  0,0,0,4,0,0,0,96,120,143,134,1,16,4,132,2,16,132,131,99,120,12,128,84,120,218,132,101,0,10,128,86,120,153,132,87,120,24,128,40,120,221,130,89,120,22,128,90,120,21,128,91,120,20,128,92,120,19,128,93, 
  120,18,128,94,120,17,128,31,136,94,129,34,232,157,128,81,120,28,129,82,120,27,129,88,120,23,128,95,120,80,129,97,120,142,129,98,120,13,128,100,120,11,128,102,136,8,128,127,184,7,128,128,232,5,128,129,24,4,128,8, 
  4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,189,149,0,0,17,1,63,204,0,0,1,4,19, 
  119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,189,149,0,0,17,1,63,204,0,0,1,4,15,1,189, 
  149,0,0,15,1,63,204,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,189,149, 
  0,0,17,1,63,204,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,189,149,0,0,17,1,63,204,0,0,1,4,15,1,189, 
  149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1, 
  4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239, 
  201,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0, 
  0,17,1,65,201,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15, 
  1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0, 
  0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,189,149,0,0,15, 
  1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,189, 
  149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1, 
  4,15,1,189,149,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,42,142,0,0,17,1,231,141,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,17, 
  1,197,177,0,0,1,4,15,1,189,149,0,0,15,1,63,204,0,0,17,1,73,171,0,0,1,2,21,1,66,0,0,0,55,248,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,41,248,0,128,8,4,19, 
  37,0,0,0,101,0,0,0,1,0,19,65,0,0,0,179,0,0,0,1,0,19,36,0,0,0,100,0,0,0,2,0,1,2,21,7,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,56,1,128,125,208,0,128, 
  4,19,10,0,0,0,30,0,0,0,3,0,1,4,17,1,117,142,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,163,0,0,0,95,23,2,0,6, 
  0,0,0,2,0,0,0,41,200,3,129,1,80,193,127,2,80,65,128,54,248,130,128,69,40,2,128,70,88,1,128,8,4,15,1,221,148,0,0,15,1,165,149,0,0,15,1,177,149,0,0,17,1,134,148,0,0,1,4,15, 
  1,221,148,0,0,15,1,165,149,0,0,15,1,177,149,0,0,17,1,47,148,0,0,1,4,15,1,221,148,0,0,15,1,165,149,0,0,15,1,177,149,0,0,17,1,232,146,0,0,1,4,19,37,0,0,0,101,0,0,0, 
  1,0,19,65,0,0,0,179,0,0,0,1,0,15,1,221,148,0,0,15,1,165,149,0,0,17,1,25,143,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,94,143, 
  0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,35,0,0,0,98,0,0,0,1,0,1,21,1,79,3,0,0,241,25,2,0,28,0,0,0,4,0,0,0, 
  96,200,141,134,1,16,4,132,2,16,132,131,99,88,11,128,84,184,214,132,101,64,9,128,86,232,149,132,87,24,21,128,88,72,20,128,89,120,19,128,10,8,90,130,91,216,17,128,92,8,17,128,93,56,16,128,94,104,15,128, 
  31,152,89,129,34,40,153,128,81,88,24,129,82,136,23,129,90,168,18,128,95,152,78,129,97,248,140,129,98,40,12,128,100,136,10,128,102,248,7,128,127,88,7,128,128,184,5,128,129,24,4,128,8,4,19,120,0,0,0,101, 
  1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,99,57,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0, 
  0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,99,57,0,0,1,4,15,1,99,57,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87, 
  0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,99,57,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1, 
  0,19,34,0,0,0,95,0,0,0,1,0,17,1,99,57,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1, 
  87,203,0,0,17,1,157,202,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201, 
  0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,99,57, 
  0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0, 
  15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1, 
  142,199,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1, 
  99,57,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,99,57,0,0,15,1,75,203, 
  0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,99,57,0,0,15,1,75,203,0,0,15,1,160,192,0,0, 
  17,1,65,184,0,0,1,4,15,1,99,57,0,0,17,1,197,177,0,0,1,4,15,1,99,57,0,0,17,1,73,171,0,0,1,4,15,1,220,146,0,0,17,1,174,146,0,0,1,2,21,1,45,0,0,0,222,26,2,0, 
  3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,40,248,0,128,8,4,15,1,42,142,0,0,17,1,231,141,0,0,1,2,19,35,0,0,0,97,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,60,176,0,128,4,15,1,35,148,0,0,17,1,51,147,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,66,0,0,0,184,0,0,0, 
  1,0,1,21,1,50,0,0,0,188,25,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,72,248,0,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,102,147,0,0,1,2,21,7,47,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,44,56,65,128,62,208,0,128,4,19,88,0,0,0,8,1,0,0,3,0,1,4,17,1,177,147,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,2,21,1,50,0,0,0,188,25,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,72,248,0,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,228,147,0,0,1,2, 
  21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,19,88,0,0,0,7,1,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,2,19,66,0,0,0,181,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,122,148,0,0,17,1,51,147,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,66,0,0,0,183,0,0,0,1,0,1,19,66,0,0,0,180,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  60,176,0,128,4,15,1,209,148,0,0,17,1,51,147,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,66,0,0,0,185,0,0,0,1,0,1,19,66,0, 
  0,0,182,0,0,0,2,0,1,21,0,123,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,35,216,2,128,37,72,194,128,10,208,195,128,11,104,67,127,65,224,1,128,66,80,1,128,19,35,0,0,0,99,0,0,0, 
  1,0,17,1,221,148,0,0,1,15,1,221,148,0,0,17,1,195,54,0,0,1,19,65,0,0,0,179,0,0,0,1,0,17,1,221,148,0,0,1,19,11,0,0,0,31,0,0,0,1,0,17,1,221,148,0,0,1,15,1, 
  221,148,0,0,17,1,89,149,0,0,1,1,2,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,64,1,128,125,208,0,128,4,19,10,0,0,0,29,0,0,0,5,0,14,1,4,17,1,245,53,0,0, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,11,0,0,0,31,0,0,0,1,0,1,19,35,0,0,0,99,0,0,0,1,0,1,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,68,0,0,0,191,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,87,1, 
  0,0,255,255,255,255,16,0,0,0,4,0,0,0,64,160,200,128,81,120,7,128,34,48,73,130,80,8,8,128,68,152,8,130,85,200,69,130,86,56,5,128,87,168,4,130,120,144,2,128,9,40,10,128,10,152,9,128,82,232, 
  6,128,84,88,70,128,116,24,4,128,117,176,3,128,119,32,3,128,19,87,0,0,0,6,1,0,0,1,0,17,1,252,149,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,252,149,0,0,1,15,1,252,149,0,0, 
  17,1,183,203,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,252,149,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,252,149,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,252,149,0,0, 
  1,19,64,0,0,0,176,0,0,0,1,0,17,1,252,149,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,252,149,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,252,149,0,0,1,19,64,0,0,0, 
  173,0,0,0,1,0,17,1,252,149,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,252,149,0,0,1,1,19,34,0,0,0,95,0,0,0,1,0,17,1,252,149,0,0,1,15,1,252,149,0,0,17,1,84,151, 
  0,0,1,19,68,0,0,0,189,0,0,0,1,0,17,1,252,149,0,0,1,19,68,0,0,0,190,0,0,0,1,0,17,1,252,149,0,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,208, 
  1,128,37,80,194,127,42,80,193,128,43,144,65,128,47,16,2,128,94,144,2,128,4,17,1,11,168,0,0,1,4,17,1,205,164,0,0,1,4,17,1,143,161,0,0,1,4,17,1,81,158,0,0,1,4,17,1,19,155,0, 
  0,1,4,17,1,213,151,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,28,0,0,0,1,0,1,21,1,61,3,0,0,69,27,2,0,27, 
  0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93, 
  24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0, 
  0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,210,78,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0, 
  19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,210,78,0,0,1,4,15,1,210,78,0,0,15,1,192,80,0,0,17,1,99,203,0,0,1, 
  4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,210,78,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0, 
  0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,210,78,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0, 
  0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,239,201,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15, 
  1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,210,78,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0, 
  0,17,1,142,199,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1, 
  4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,210,78,0,0,15, 
  1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,210,78,0,0,15,1,75,203,0,0,15,1,160, 
  192,0,0,17,1,65,184,0,0,1,4,15,1,210,78,0,0,17,1,9,87,0,0,1,4,15,1,210,78,0,0,17,1,118,81,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77, 
  134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25, 
  129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87, 
  0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,79,75,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1, 
  0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,79,75,0,0,1,4,15,1,79,75,0,0,15,1,192,80,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0, 
  1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,79,75,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95, 
  0,0,0,1,0,17,1,79,75,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157, 
  202,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,79, 
  75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0, 
  0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,60,200,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15, 
  1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,79,75,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,26,198,0, 
  0,17,1,7,196,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,79,75,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1, 
  4,15,1,79,75,0,0,17,1,9,87,0,0,1,4,15,1,79,75,0,0,17,1,118,81,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99, 
  56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82, 
  104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19, 
  64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,204,71,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0, 
  1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,204,71,0,0,1,4,15,1,204,71,0,0,15,1,192,80,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0, 
  0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,204,71,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,204,71,0, 
  0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,204,71,0, 
  0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15, 
  1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234, 
  200,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,204, 
  71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0, 
  0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,137,198,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15, 
  1,204,71,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,204,71,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,204,71,0,0,17,1,9, 
  87,0,0,1,4,15,1,204,71,0,0,17,1,118,81,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9, 
  128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140, 
  129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0, 
  19,34,0,0,0,95,0,0,0,1,0,17,1,174,64,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0, 
  0,1,0,17,1,174,64,0,0,1,4,15,1,174,64,0,0,15,1,192,80,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0, 
  0,0,1,0,17,1,174,64,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,174,64,0,0,1,4,15,1,174,64,0,0,15, 
  1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87, 
  203,0,0,17,1,70,202,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0, 
  0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,174,64,0, 
  0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15, 
  1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55, 
  199,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,174, 
  64,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0, 
  0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,174,64,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,174,64,0,0,17,1,9,87,0,0,1,4,15,1,174,64,0, 
  0,17,1,118,81,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88, 
  40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102, 
  216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1, 
  0,17,1,19,61,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,19,61,0,0,1, 
  4,15,1,19,61,0,0,15,1,192,80,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,19,61,0,0, 
  1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,19,61,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0, 
  0,17,1,244,202,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1, 
  4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,19,61,0,0,15, 
  1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87, 
  203,0,0,17,1,147,200,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0, 
  0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,19,61,0, 
  0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15, 
  1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172, 
  192,0,0,1,4,15,1,19,61,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,19,61,0,0,17,1,9,87,0,0,1,4,15,1,19,61,0,0,17,1,118,81,0,0,1,2,21, 
  1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18, 
  128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5, 
  128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,73,68,0,0,1,4,19, 
  119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,73,68,0,0,1,4,15,1,73,68,0,0,15,1,192, 
  80,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,73,68,0,0,1,4,19,87,0,0,0,3,1,0, 
  0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,73,68,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15, 
  1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,73,68,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0, 
  0,17,1,65,201,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1, 
  4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,73,68,0,0,15, 
  1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87, 
  203,0,0,17,1,224,198,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0, 
  0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,73,68,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,73,68,0, 
  0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,73,68,0,0,17,1,9,87,0,0,1,4,15,1,73,68,0,0,17,1,118,81,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27, 
  0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93, 
  24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0, 
  0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0, 
  19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1, 
  4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0, 
  0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0, 
  0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,239,201,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15, 
  1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,90,85,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0, 
  0,17,1,142,199,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1, 
  4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,90,85,0,0,15, 
  1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,160, 
  192,0,0,17,1,65,184,0,0,1,4,15,1,90,85,0,0,17,1,197,177,0,0,1,4,15,1,90,85,0,0,17,1,135,174,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77, 
  134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25, 
  129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87, 
  0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1, 
  0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0, 
  1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,90,85,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95, 
  0,0,0,1,0,17,1,90,85,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157, 
  202,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,90, 
  85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0, 
  0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,60,200,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15, 
  1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,90,85,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,26,198,0, 
  0,17,1,7,196,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,90,85,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1, 
  4,15,1,90,85,0,0,17,1,197,177,0,0,1,4,15,1,90,85,0,0,17,1,135,174,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99, 
  56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82, 
  104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19, 
  64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0, 
  1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0,0,1,4,15,1,237,90,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0, 
  0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0, 
  0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,237,90,0, 
  0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15, 
  1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234, 
  200,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,237, 
  90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0, 
  0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,137,198,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15, 
  1,237,90,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,237,90,0,0,17,1,3, 
  181,0,0,1,4,15,1,237,90,0,0,17,1,73,171,0,0,1,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9, 
  128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140, 
  129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0, 
  19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0, 
  0,1,0,17,1,237,90,0,0,1,4,15,1,237,90,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0, 
  0,0,1,0,17,1,237,90,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,237,90,0,0,1,4,15,1,237,90,0,0,15, 
  1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87, 
  203,0,0,17,1,70,202,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0, 
  0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,237,90,0, 
  0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15, 
  1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55, 
  199,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,237, 
  90,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0, 
  0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,237,90,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,237,90,0,0,17,1,3,181,0,0,1,4,15,1,237,90,0, 
  0,17,1,73,171,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,17,1,123,184,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,208,0,128,8,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20, 
  128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7, 
  128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17, 
  1,185,187,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,185,187,0,0,1,4,15, 
  1,185,187,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,185,187,0,0,1,4, 
  19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,185,187,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,244,202,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15, 
  1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,185,187,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0, 
  0,17,1,147,200,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1, 
  4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,185,187,0,0,15, 
  1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,125, 
  198,0,0,17,1,38,198,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0, 
  0,1,4,15,1,185,187,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,185,187,0,0,17,1,197,177,0,0,1,4,15,1,185,187,0,0,17,1,73,171,0,0,1,2,21,0,38, 
  1,0,0,255,255,255,255,13,0,0,0,3,0,0,0,64,56,200,128,81,24,7,128,34,200,136,129,80,168,71,130,84,248,69,129,85,104,69,129,86,216,4,128,87,72,4,129,82,136,6,128,116,184,3,128,117,80,3,128,119, 
  192,2,128,120,48,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,185,187,0,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,185,187,0,0,1,15,1,185,187,0,0,17,1,192,80,0,0,1,19,80,0,0, 
  0,220,0,0,0,1,0,17,1,185,187,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,185,187,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,185,187,0,0,1,19,64,0,0,0,176,0,0,0,1, 
  0,17,1,185,187,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,185,187,0,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,185,187,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,185,187,0, 
  0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,185,187,0,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,185,187,0,0,1,15,1,185,187,0,0,17,1,224,188,0,0,1,2,21,7,102,0,0,0,255,255, 
  255,255,7,0,0,0,2,0,0,0,44,240,2,128,37,112,194,128,42,48,2,129,43,176,130,128,45,240,1,128,47,176,1,128,94,112,1,128,4,17,1,224,76,0,0,1,4,17,1,218,69,0,0,1,4,17,1,188,62,0, 
  0,1,4,17,1,87,66,0,0,1,4,17,1,93,73,0,0,1,4,17,1,33,59,0,0,1,4,17,1,98,189,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,2,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248,20,128,88,40,20,128,89,88,19,128, 
  90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104,10,128,102,216,7,128,127,56,7,128, 
  128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,218,97,0,0, 
  1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,218,97,0,0,1,4,15,1,218,97,0,0, 
  15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,218,97,0,0,1,4,19,87,0,0,0, 
  3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,218,97,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0, 
  1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,218,97,0,0, 
  15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1, 
  87,203,0,0,17,1,65,201,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200, 
  0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,218,97, 
  0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0, 
  15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1, 
  38,198,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,218,97,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1, 
  218,97,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,218,97,0,0,17,1,197,177,0,0,1,4,15,1,218,97,0,0,17,1,73,171,0,0,1,2,19,64,0,0,0,173,0,0, 
  0,1,0,1,21,1,78,3,0,0,39,28,2,0,28,0,0,0,4,0,0,0,96,200,141,134,1,16,4,132,2,16,132,131,83,136,215,132,84,184,214,132,101,64,9,128,86,232,149,132,87,24,21,128,88,72,20,128,89,120, 
  19,128,90,168,18,128,91,216,17,128,92,8,17,128,93,56,16,128,94,104,15,128,31,0,26,129,34,144,153,128,81,192,216,128,82,240,215,128,95,152,142,129,97,248,204,129,98,40,12,128,99,88,11,128,100,136,10,128,102,248, 
  7,128,127,88,7,128,128,184,5,128,129,24,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0, 
  17,1,144,103,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,144,103,0,0,1,4, 
  15,1,144,103,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,144,103,0,0,1, 
  4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,144,103,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0, 
  17,1,244,202,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202,0,0,1,4, 
  15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,144,103,0,0,15,1, 
  75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203, 
  0,0,17,1,147,200,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0, 
  1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,144,103,0,0, 
  15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1, 
  125,198,0,0,17,1,38,198,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,19,82,0,0,0,224,0,0,0,2,0,1,4,15,1,144,103,0,0,15,1,75, 
  203,0,0,15,1,251,195,0,0,17,1,172,192,0,0,1,4,15,1,144,103,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,144,103,0,0,17,1,197,177,0,0,1,4,15,1,144, 
  103,0,0,17,1,73,171,0,0,1,2,19,64,0,0,0,174,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,14,198,0,0,17,1,82,196,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,84,0,0,0,228,0,0,0,1,0,1,21,1,37,1,0,0,20,29,2,0,10,0,0,0,3,0,0,0,128,32,3,128, 
  1,208,1,130,2,208,1,128,70,72,71,129,101,88,6,128,69,232,199,127,54,136,72,127,127,104,4,128,102,104,5,128,129,216,1,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0, 
  19,137,0,0,0,139,1,0,0,1,0,17,1,207,197,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,137,0,0,0,139,1,0,0,1,0,17,1,207,197,0,0,1,4, 
  15,1,207,197,0,0,15,1,132,197,0,0,15,1,144,197,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,137,0,0,0,139,1,0,0,1,0,17,1,207,197,0,0, 
  1,4,19,87,0,0,0,3,1,0,0,1,0,19,137,0,0,0,139,1,0,0,1,0,17,1,207,197,0,0,1,4,15,1,207,197,0,0,15,1,120,197,0,0,17,1,134,148,0,0,1,4,15,1,207,197,0,0,15,1, 
  120,197,0,0,17,1,47,148,0,0,1,4,15,1,207,197,0,0,15,1,120,197,0,0,17,1,232,146,0,0,1,2,19,137,0,0,0,138,1,0,0,1,0,1,19,137,0,0,0,137,1,0,0,1,0,1,21,0,62,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,232,129,128,117,240,0,128,116,88,1,128,15,1,144,197,0,0,17,1,183,203,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,144,197,0,0,1,1,2,21, 
  7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,118,0,0,0,99,1,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,2,19,84,0,0,0,227,0,0,0,2,0,1,19,64,0,0,0,175,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,113,198,0,0,17,1,82,196,0, 
  0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,85,0,0,0,230,0,0,0,1,0,1,19,85,0,0,0,229,0,0,0,2,0,1,19,64,0,0,0,176,0, 
  0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,212,198,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,86,0,0,0,242,0,0,0,1,0,1,19,86,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,43,199,0, 
  0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,232,0,0,0,1,0,1,19,86,0,0,0,231,0,0,0,2,0,1,21, 
  7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,130,199,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,19,86,0,0,0,254,0,0,0,1,0,1,19,86,0,0,0,253,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,217,199,0,0,17,1,82,196,0, 
  0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,246,0,0,0,1,0,1,19,86,0,0,0,245,0,0,0,2,0,1,21,7,36,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,48,200,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0, 
  0,236,0,0,0,1,0,1,19,86,0,0,0,235,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,135,200,0,0,17,1,82,196,0,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,0,1,0,0,1,0,1,19,86,0,0,0,255,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,40,176,0,128,4,15,1,222,200,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,248,0,0,0,1, 
  0,1,19,86,0,0,0,247,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,53,201,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,238,0,0,0,1,0,1,19,86,0,0,0,237,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40, 
  176,0,128,4,15,1,140,201,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,252,0,0,0,1,0,1,19,86,0,0, 
  0,251,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,227,201,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,244,0,0,0,1,0,1,19,86,0,0,0,243,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1, 
  58,202,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,234,0,0,0,1,0,1,19,86,0,0,0,233,0,0,0,2, 
  0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,145,202,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,86,0,0,0,2,1,0,0,1,0,1,19,86,0,0,0,1,1,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,232,202,0,0,17,1, 
  82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,86,0,0,0,250,0,0,0,1,0,1,19,86,0,0,0,249,0,0,0,2,0,1,21,7,36,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,63,203,0,0,17,1,82,196,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  86,0,0,0,240,0,0,0,1,0,1,19,86,0,0,0,239,0,0,0,2,0,1,19,34,0,0,0,95,0,0,0,1,0,1,19,64,0,0,0,177,0,0,0,1,0,1,21,1,83,0,0,0,90,30,2,0,4,0,0, 
  0,2,0,0,0,72,24,1,128,1,16,129,128,2,16,1,128,41,216,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,19,117,0,0,0,98,1,0,0,2,0,1,4,19,37,0,0,0,101,0,0,0,1,0,19,117, 
  0,0,0,97,1,0,0,2,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,252,203,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,116,0,0,0,96,1,0,0,1,0,1,21,1,66,0,0,0,55,248,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,41,248,0,128,8,4,19,37,0,0,0,101,0,0, 
  0,1,0,19,65,0,0,0,179,0,0,0,1,0,19,80,0,0,0,221,0,0,0,3,0,1,2,21,0,43,1,0,0,255,255,255,255,14,0,0,0,3,0,0,0,64,88,200,128,9,80,201,129,34,232,200,129,80,200,135, 
  130,84,24,134,129,85,136,133,129,86,248,4,128,87,104,68,129,81,56,7,128,82,168,6,128,116,216,3,128,117,112,3,128,119,224,2,128,120,80,2,128,19,87,0,0,0,6,1,0,0,1,0,17,1,63,204,0,0,1,19, 
  87,0,0,0,5,1,0,0,1,0,17,1,63,204,0,0,1,15,1,63,204,0,0,17,1,183,203,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,63,204,0,0,1,19,64,0,0,0,178,0,0,0,1,0,17, 
  1,63,204,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,63,204,0,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,63,204,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,63,204,0,0,1, 
  19,64,0,0,0,174,0,0,0,1,0,17,1,63,204,0,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,63,204,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,63,204,0,0,1,19,34,0,0,0,95, 
  0,0,0,1,0,17,1,63,204,0,0,1,15,1,63,204,0,0,17,1,107,205,0,0,1,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,144,1,128,37,208,193,127,42,80,193,128,43,80,66, 
  128,47,144,2,128,94,16,2,128,4,17,1,11,168,0,0,1,4,17,1,143,161,0,0,1,4,17,1,19,155,0,0,1,4,17,1,213,151,0,0,1,4,17,1,205,164,0,0,1,4,17,1,81,158,0,0,1,21,9,27, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,28,0,0,0,1,0,1,19,38,0,0,0,107,0,0,0,5,0,1,19,68,0,0,0,190,0,0,0,1,0,1, 
  21,1,47,0,0,0,56,48,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,42,56,1,128,8,4,17,1,52,206,0,0,1,2,21,1,105,0,0,0,171,48,2,0,7,0, 
  0,0,2,0,0,0,72,120,1,128,1,112,65,129,2,112,129,128,7,112,129,128,6,112,1,128,31,216,2,128,41,16,2,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,111,211,0,0,1,4,19,37,0,0,0, 
  101,0,0,0,1,0,15,1,158,206,0,0,17,1,180,132,0,0,1,4,15,1,158,206,0,0,17,1,122,3,0,0,1,2,21,1,47,0,0,0,25,47,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128, 
  2,112,129,127,7,112,1,128,45,48,1,128,4,17,1,217,206,0,0,1,8,19,38,0,0,0,108,0,0,0,5,0,1,21,1,137,4,0,0,17,15,2,0,30,0,0,0,4,0,0,0,96,104,17,135,1,80,4,132,2, 
  80,132,131,99,216,13,128,84,120,94,133,101,0,11,128,6,80,68,131,7,80,68,131,88,232,26,128,89,184,25,128,10,168,227,130,91,88,23,128,92,40,22,128,93,248,20,128,94,200,19,128,31,216,226,129,34,8,162,128,81, 
  216,160,129,82,168,159,129,86,72,221,129,87,24,28,128,90,136,24,128,95,152,82,129,97,56,144,129,98,8,15,128,100,168,12,128,102,88,9,128,127,88,8,128,128,88,6,128,129,88,4,128,8,4,19,120,0,0,0,101,1, 
  0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,99,211,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1,4,19,119,0, 
  0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15,1,99,211,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1, 
  4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0, 
  95,0,0,0,1,0,15,1,99,211,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,15, 
  1,99,211,0,0,15,1,248,205,0,0,17,1,63,204,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,244,202,0,0,1,4,15,1,99, 
  211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15, 
  1,87,203,0,0,17,1,70,202,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,99,211,0,0,15,1,248, 
  205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17, 
  1,65,201,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63, 
  204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1, 
  4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,229,199,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75, 
  203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1,99,211,0, 
  0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87, 
  203,0,0,17,1,137,198,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0, 
  0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,251,195,0,0,17,1,172, 
  192,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0, 
  0,17,1,197,177,0,0,1,4,15,1,99,211,0,0,15,1,248,205,0,0,15,1,63,204,0,0,17,1,73,171,0,0,1,4,15,1,99,211,0,0,15,1,252,149,0,0,17,1,1,138,0,0,1,2,19,38,0,0,0, 
  106,0,0,0,7,0,1,21,1,75,0,0,0,39,49,2,0,6,0,0,0,2,0,0,0,44,24,66,129,1,80,1,128,2,80,129,128,7,80,1,128,6,80,1,128,72,88,1,128,8,4,19,69,0,0,0,192,0,0,0, 
  1,0,19,38,0,0,0,104,0,0,0,6,0,1,4,17,1,187,211,0,0,1,2,21,1,47,0,0,0,228,49,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,43,56,1, 
  128,8,4,17,1,235,211,0,0,1,2,21,1,63,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0, 
  1,0,19,38,0,0,0,102,0,0,0,8,0,1,2,21,1,75,0,0,0,39,49,2,0,6,0,0,0,2,0,0,0,44,24,66,129,1,80,1,128,2,80,129,128,7,80,1,128,6,80,1,128,72,88,1,128,8,4,19, 
  69,0,0,0,192,0,0,0,1,0,19,38,0,0,0,105,0,0,0,4,0,1,4,17,1,119,212,0,0,1,2,21,1,47,0,0,0,228,49,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129, 
  127,7,48,65,128,43,56,1,128,8,4,17,1,167,212,0,0,1,2,21,1,63,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19, 
  69,0,0,0,192,0,0,0,1,0,19,38,0,0,0,103,0,0,0,6,0,1,2,21,1,63,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1, 
  128,8,4,19,69,0,0,0,192,0,0,0,1,0,19,38,0,0,0,110,0,0,0,2,0,1,2,21,1,63,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1, 
  128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,19,38,0,0,0,111,0,0,0,2,0,1,2,21,0,77,1,0,0,255,255,255,255,16,0,0,0,4,0,0,0,70,120,7,128,99,64,4,128,103,176,3, 
  128,19,248,137,127,104,32,3,128,105,144,2,128,38,104,137,126,71,232,198,126,72,128,6,127,73,240,5,127,90,96,5,128,43,0,9,128,44,112,8,128,45,8,8,128,14,96,10,128,95,208,4,128,19,71,0,0,0,198,0, 
  0,0,1,0,17,1,103,213,0,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,103,213,0,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,103,213,0,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1, 
  103,213,0,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,103,213,0,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,103,213,0,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,103,213,0,0,1,15, 
  1,103,213,0,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,103,213,0,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,103,213,0,0,1,15,1,103,213,0,0,17,1,63,219,0,0, 
  1,19,14,0,0,0,48,0,0,0,1,0,17,1,103,213,0,0,1,15,1,103,213,0,0,17,1,225,217,0,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,103,213,0,0,1,15,1,103,213,0,0,17,1,181,214, 
  0,0,1,1,2,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,240,214,0,0,1,8,19,14,0,0,0,46,0,0, 
  0,1,0,1,21,1,122,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,48,10,128,1,144,194,127,2,144,66,128,50,192,9,128,103,56,5,128,53,80,9,128,6,144,2,128,7,144,66,127,107,56,4,128,73,56, 
  8,128,74,32,7,128,75,8,70,127,108,104,3,128,109,152,2,128,46,96,11,128,47,240,10,128,8,4,15,1,213,217,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,213,217,0,0, 
  15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,213,217,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,213,217,0,0,15,1, 
  157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,44,0,0,0,3,0,1,4,19,73,0,0,0,202,0, 
  0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,44,0,0,0,3,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,44,0,0,0,3, 
  0,1,4,15,1,119,216,0,0,17,1,239,228,0,0,1,4,15,1,107,216,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,56,0,0,0,3,0,1,4,15,1,107,216,0, 
  0,17,1,231,212,0,0,1,4,15,1,107,216,0,0,17,1,201,2,0,0,1,2,19,19,0,0,0,56,0,0,0,3,0,1,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193, 
  128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,178,216,0,0,1,8,19,14,0,0,0,45,0,0,0,3,0,1,21,1,22,1,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,152,7,128,1,240,193,127, 
  2,240,129,129,75,104,197,129,108,200,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,128,6,128,103,152,4,128,107,152,3,128,8,4,15,1,201,217,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4, 
  1,0,1,4,15,1,201,217,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,201,217,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0, 
  1,4,15,1,201,217,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,43,0,0,0,5, 
  0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,43,0,0,0,5,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0, 
  19,14,0,0,0,43,0,0,0,5,0,1,2,19,14,0,0,0,43,0,0,0,5,0,1,19,14,0,0,0,44,0,0,0,3,0,1,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1, 
  112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,28,218,0,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21,1,22,1,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,152,7,128,1,240, 
  193,127,2,240,129,129,75,104,197,129,108,200,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,128,6,128,103,152,4,128,107,152,3,128,8,4,15,1,51,219,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1, 
  68,4,1,0,1,4,15,1,51,219,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,51,219,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3, 
  1,0,1,4,15,1,51,219,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,47,0,0, 
  0,3,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,47,0,0,0,3,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0, 
  1,0,19,14,0,0,0,47,0,0,0,3,0,1,2,19,14,0,0,0,47,0,0,0,3,0,1,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1, 
  128,13,48,1,128,4,17,1,122,219,0,0,1,8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,146,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,96,10,128,1,144,194,127,2,144,66,128,50,192,9,128, 
  103,56,5,128,53,80,9,128,6,144,2,128,7,144,66,127,107,56,4,128,73,56,8,128,74,32,7,128,75,8,70,127,108,104,3,128,109,152,2,128,46,240,11,128,47,80,11,128,8,4,15,1,71,228,0,0,15,1,125,6, 
  1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,71,228,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,71,228,0,0,15,1,125,6,1,0,15,1,98,3,1,0, 
  15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,71,228,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0, 
  1,0,19,14,0,0,0,38,0,0,0,3,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,38,0,0,0,3,0,1,4,19,73,0,0,0,201,0,0,0,1, 
  0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,38,0,0,0,3,0,1,4,15,1,233,226,0,0,17,1,239,228,0,0,1,4,15,1,13,221,0,0,15,1,221,226,0,0,17,1,39,213,0,0,1,4,19, 
  38,0,0,0,112,0,0,0,1,0,19,15,0,0,0,50,0,0,0,1,0,17,1,13,221,0,0,1,4,15,1,13,221,0,0,15,1,221,226,0,0,17,1,231,212,0,0,1,4,15,1,13,221,0,0,15,1,221,226,0, 
  0,17,1,201,2,0,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70, 
  88,7,128,71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,13,221,0,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,13,221, 
  0,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,13,221,0,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,13,221,0,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,13,221,0,0,1,19,70,0, 
  0,0,193,0,0,0,1,0,17,1,13,221,0,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,13,221,0,0,1,15,1,13,221,0,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,13, 
  221,0,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,13,221,0,0,1,19,14,0,0,0,38,0,0,0,3,0,1,15,1,13,221,0,0,17,1,122,225,0,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1, 
  13,221,0,0,1,15,1,13,221,0,0,17,1,68,222,0,0,1,1,2,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17, 
  1,128,222,0,0,1,8,19,14,0,0,0,41,0,0,0,3,0,14,1,21,1,125,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,72,10,128,1,144,194,127,2,144,66,128,50,216,9,128,103,56,5,128,53,104, 
  9,128,6,144,2,128,7,144,66,127,107,56,4,128,73,72,8,128,74,40,7,128,75,8,70,127,108,104,3,128,109,152,2,128,46,120,11,128,47,8,11,128,8,4,15,1,109,225,0,0,15,1,125,6,1,0,15,1,137,6, 
  1,0,17,1,68,4,1,0,1,4,15,1,109,225,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,109,225,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0, 
  17,1,3,3,1,0,1,4,15,1,109,225,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0, 
  0,37,0,0,0,5,0,14,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,37,0,0,0,5,0,14,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0, 
  0,0,120,0,0,0,1,0,19,14,0,0,0,37,0,0,0,5,0,14,1,4,15,1,10,224,0,0,17,1,239,228,0,0,1,4,15,1,254,223,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1, 
  0,19,15,0,0,0,51,0,0,0,3,0,1,4,15,1,254,223,0,0,17,1,231,212,0,0,1,4,15,1,254,223,0,0,17,1,201,2,0,0,1,2,19,15,0,0,0,51,0,0,0,3,0,1,21,1,47,0,0,0, 
  149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,70,224,0,0,1,8,19,14,0,0,0,39,0,0,0,5,0,14,1,21,1,25,1,0,0, 
  131,52,2,0,11,0,0,0,3,0,0,0,73,168,7,128,1,240,193,127,2,240,129,129,75,104,197,129,108,200,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,136,6,128,103,152,4,128,107,152,3,128,8,4,15,1, 
  96,225,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,96,225,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,96,225,0,0,15,1,125,6, 
  1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,96,225,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19, 
  44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,35,0,0,0,7,0,14,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,35,0,0,0,7,0,14,1,4, 
  19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,35,0,0,0,7,0,14,1,2,19,14,0,0,0,35,0,0,0,7,0,14,1,19,14,0,0,0,37,0,0,0,5,0, 
  14,1,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,182,225,0,0,1,8,19,14,0,0,0,40,0,0,0,3,0, 
  14,1,21,1,25,1,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,168,7,128,1,240,193,127,2,240,129,129,75,104,197,129,108,200,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,136,6,128,103,152,4,128, 
  107,152,3,128,8,4,15,1,208,226,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,208,226,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1, 
  208,226,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,208,226,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0, 
  0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,36,0,0,0,5,0,14,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,36, 
  0,0,0,5,0,14,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,36,0,0,0,5,0,14,1,2,19,14,0,0,0,36,0,0,0,5,0,14,1,19,15,0, 
  0,0,50,0,0,0,1,0,1,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,36,227,0,0,1,19,14,0,0, 
  0,40,0,0,0,3,0,1,21,1,22,1,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,152,7,128,1,240,193,127,2,240,129,129,75,104,197,129,108,200,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,128, 
  6,128,103,152,4,128,107,152,3,128,8,4,15,1,59,228,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,59,228,0,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3, 
  1,0,1,4,15,1,59,228,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,59,228,0,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0, 
  1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,36,0,0,0,5,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19, 
  14,0,0,0,36,0,0,0,5,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,36,0,0,0,5,0,1,2,19,14,0,0,0,36,0,0,0,5,0,1,19, 
  14,0,0,0,38,0,0,0,3,0,1,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,131,228,0,0,1,2,21, 
  1,71,0,0,0,228,53,2,0,6,0,0,0,2,0,0,0,108,88,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,107,200,1,128,8,4,15,1,215,228,0,0,17,1,217,3,1,0,1,4,15,1,203, 
  228,0,0,17,1,3,3,1,0,1,2,19,72,0,0,0,200,0,0,0,3,0,1,19,71,0,0,0,196,0,0,0,3,0,1,19,19,0,0,0,55,0,0,0,1,0,1,21,1,63,0,0,0,238,236,1,0,5,0,0, 
  0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,19,43,0,0,0,117,0,0,0,2,0,1,2,21,1,47,0,0,0,149,50,2, 
  0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,28,218,0,0,1,19,14,0,0,0,49,0,0,0,1,0,1,21,1,81,0,0,0,100,54,2,0, 
  7,0,0,0,2,0,0,0,56,184,1,128,1,112,65,129,2,112,129,128,7,112,1,128,6,112,65,128,54,32,2,128,57,120,1,128,8,4,17,1,188,229,0,0,1,4,19,45,0,0,0,128,0,0,0,2,0,1,4,19, 
  45,0,0,0,127,0,0,0,2,0,1,2,21,1,69,0,0,0,229,55,2,0,6,0,0,0,2,0,0,0,56,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,54,192,1,128,8,4,19,45,0,0, 
  0,126,0,0,0,3,0,1,4,19,45,0,0,0,125,0,0,0,3,0,1,2,21,1,69,0,0,0,229,55,2,0,6,0,0,0,2,0,0,0,56,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128, 
  54,192,1,128,8,4,19,45,0,0,0,130,0,0,0,2,0,1,4,19,45,0,0,0,129,0,0,0,2,0,1,2,21,1,81,0,0,0,100,54,2,0,7,0,0,0,2,0,0,0,56,184,1,128,1,112,65,129,2,112, 
  129,128,7,112,1,128,6,112,65,128,54,32,2,128,57,120,1,128,8,4,17,1,154,230,0,0,1,4,19,45,0,0,0,124,0,0,0,2,0,1,4,19,45,0,0,0,123,0,0,0,2,0,1,2,21,1,69,0,0,0, 
  229,55,2,0,6,0,0,0,2,0,0,0,56,88,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,54,192,1,128,8,4,19,45,0,0,0,122,0,0,0,3,0,1,4,19,45,0,0,0,121,0,0,0, 
  3,0,1,2,21,1,47,0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,27,231,0,0,1,19,14,0,0,0,42,0,0,0, 
  1,0,1,21,1,146,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,96,10,128,1,144,194,127,2,144,66,128,50,192,9,128,103,56,5,128,53,80,9,128,6,144,2,128,7,144,66,127,107,56,4,128,73,56,8, 
  128,74,32,7,128,75,8,70,127,108,104,3,128,109,152,2,128,46,240,11,128,47,80,11,128,8,4,15,1,71,228,0,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,71,228,0,0,15, 
  1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,71,228,0,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,71,228,0,0,15,1,157, 
  2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,38,0,0,0,3,0,1,4,19,73,0,0,0,202,0,0, 
  0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,38,0,0,0,3,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,38,0,0,0,3,0, 
  1,4,15,1,233,226,0,0,17,1,239,228,0,0,1,4,15,1,174,232,0,0,15,1,221,226,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,15,0,0,0,50,0,0,0,1,0,17,1, 
  174,232,0,0,1,4,15,1,174,232,0,0,15,1,221,226,0,0,17,1,231,212,0,0,1,4,15,1,174,232,0,0,15,1,221,226,0,0,17,1,201,2,0,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0, 
  3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128,71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128, 
  105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,174,232,0,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,174,232,0,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,174,232,0,0,1,19,70, 
  0,0,0,195,0,0,0,1,0,17,1,174,232,0,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,174,232,0,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,174,232,0,0,1,19,44,0,0,0,120,0,0, 
  0,1,0,17,1,174,232,0,0,1,15,1,174,232,0,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,174,232,0,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,174,232,0,0,1,19, 
  14,0,0,0,38,0,0,0,3,0,1,15,1,174,232,0,0,17,1,229,233,0,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,174,232,0,0,1,15,1,174,232,0,0,17,1,68,222,0,0,1,1,2,21,1,47, 
  0,0,0,149,50,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,182,225,0,0,1,8,19,14,0,0,0,40,0,0,0,3,0,14,1,21,1,47, 
  0,0,0,70,56,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,39,56,1,128,8,4,17,1,81,234,0,0,1,2,21,1,82,0,0,0,154,56,2,0,7,0,0,0,2, 
  0,0,0,6,112,129,129,1,112,193,128,2,112,129,127,7,112,1,128,41,248,65,128,105,184,1,128,106,120,1,128,8,4,17,1,199,251,0,0,1,4,17,1,161,246,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0, 
  17,1,164,234,0,0,1,2,21,1,95,0,0,0,11,63,2,0,7,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,129,128,10,136,2,128,31,232,65,128,39,120,1,128,8,4,15,1,71,246, 
  0,0,17,1,7,246,0,0,1,4,15,1,239,245,0,0,15,1,251,245,0,0,17,1,57,242,0,0,1,4,15,1,239,245,0,0,17,1,4,235,0,0,1,2,21,1,131,0,0,0,117,63,2,0,8,0,0,0,3,0, 
  0,0,10,120,67,129,1,144,1,128,2,144,129,127,131,152,1,128,31,216,2,128,130,56,2,128,6,144,1,128,7,144,65,127,8,4,15,1,32,241,0,0,15,1,45,242,0,0,17,1,128,240,0,0,1,4,15,1,32,241, 
  0,0,15,1,116,240,0,0,17,1,236,237,0,0,1,4,15,1,32,241,0,0,15,1,224,237,0,0,17,1,57,242,0,0,1,4,15,1,32,241,0,0,15,1,116,240,0,0,17,1,136,235,0,0,1,2,21,1,34,2, 
  0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,80,14,128,1,240,194,127,2,240,66,128,50,128,13,128,55,64,76,131,53,224,12,128,6,240,2,128,7,240,66,127,74,112,8,128,73,184,9,128,58,160,139,127,59,0, 
  75,129,108,248,3,128,109,248,2,128,46,64,16,128,47,112,15,128,75,40,135,128,103,40,6,128,107,248,4,128,8,4,15,1,171,237,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4, 
  1,0,1,4,15,1,171,237,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,171,237,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,98,3,1,0, 
  15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,171,237,0,0,15,1,113,6,1,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0, 
  0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,171,237,0,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0, 
  17,1,171,237,0,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,171,237,0,0,1,4,15,1,171,237,0,0,15,1,224,230,0, 
  0,17,1,72,230,0,0,1,4,15,1,171,237,0,0,15,1,224,230,0,0,17,1,2,230,0,0,1,4,15,1,171,237,0,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,171,237,0,0,15,1,47,229,0, 
  0,17,1,239,228,0,0,1,4,15,1,171,237,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0,0,0,1,0,15,1,171, 
  237,0,0,17,1,103,213,0,0,1,4,15,1,171,237,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,171,237,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,201,2,0, 
  0,1,2,21,1,52,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,107,1,0,0,3,0,1,2,19,93,0,0, 
  0,16,1,0,0,1,0,1,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,28,238,0,0,1,2,21,1,34,2, 
  0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,80,14,128,1,240,194,127,2,240,66,128,50,128,13,128,55,64,76,131,53,224,12,128,6,240,2,128,7,240,66,127,74,112,8,128,73,184,9,128,58,160,139,127,59,0, 
  75,129,108,248,3,128,109,248,2,128,46,64,16,128,47,112,15,128,75,40,135,128,103,40,6,128,107,248,4,128,8,4,15,1,63,240,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4, 
  1,0,1,4,15,1,63,240,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,63,240,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,98,3,1,0, 
  15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,63,240,0,0,15,1,113,6,1,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0, 
  0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,63,240,0,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0, 
  17,1,63,240,0,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,63,240,0,0,1,4,15,1,63,240,0,0,15,1,224,230,0, 
  0,17,1,72,230,0,0,1,4,15,1,63,240,0,0,15,1,224,230,0,0,17,1,2,230,0,0,1,4,15,1,63,240,0,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,63,240,0,0,15,1,47,229,0, 
  0,17,1,239,228,0,0,1,4,15,1,63,240,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0,0,0,1,0,15,1,63, 
  240,0,0,17,1,103,213,0,0,1,4,15,1,63,240,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,63,240,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,201,2,0, 
  0,1,2,21,1,52,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,106,1,0,0,4,0,1,2,19,93,0,0, 
  0,17,1,0,0,1,0,1,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,176,240,0,0,1,2,21,1,58,0, 
  0,0,68,66,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,129,56,1,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,235,240,0,0,1,2,21,1,52,0,0, 
  0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,125,0,0,0,108,1,0,0,4,0,1,2,21,0,106,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,124,192,1,128,93,232,130,128,94,224,2,128,121,80,66,128,125,48,1,128,19,93,0,0,0,18,1,0,0,1,0,17,1,32,241,0,0,1,19,93,0,0,0,17,1,0,0,1,0,17,1,32,241,0, 
  0,1,19,93,0,0,0,16,1,0,0,1,0,17,1,32,241,0,0,1,1,15,1,32,241,0,0,17,1,139,241,0,0,1,2,21,1,125,0,0,0,153,66,2,0,9,0,0,0,3,0,0,0,10,120,67,129,1,176,1, 
  128,2,176,129,127,11,8,67,129,31,152,2,128,130,40,2,128,6,176,1,128,7,176,65,127,131,184,1,128,8,4,15,1,33,242,0,0,17,1,128,240,0,0,1,4,15,1,21,242,0,0,17,1,236,237,0,0,1,4,15, 
  1,9,242,0,0,17,1,57,242,0,0,1,4,19,94,0,0,0,23,1,0,0,3,0,14,1,4,15,1,21,242,0,0,17,1,136,235,0,0,1,2,19,93,0,0,0,19,1,0,0,2,0,1,19,93,0,0,0,20,1, 
  0,0,2,0,1,19,93,0,0,0,21,1,0,0,2,0,1,19,93,0,0,0,18,1,0,0,1,0,1,21,1,80,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7, 
  48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,19,122,0,0,0,103,1,0,0,1,0,19,123,0,0,0,104,1,0,0,1,0,17,1,138,242,0,0,1,2,21,0,84,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,121,8,2,128,69,16,194,127,122,120,1,128,123,16,1,128,15,1,138,242,0,0,17,1,223,242,0,0,1,19,123,0,0,0,104,1,0,0,1,0,17,1,138,242,0,0,1,1,19,122,0,0, 
  0,103,1,0,0,1,0,17,1,138,242,0,0,1,2,21,1,59,0,0,0,228,248,1,0,6,0,0,0,2,0,0,0,32,152,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,66,88,1,128,8,4,17, 
  1,164,245,0,0,1,4,17,1,27,243,0,0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,75,243,0, 
  0,1,2,21,1,34,2,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,80,14,128,1,240,194,127,2,240,66,128,50,128,13,128,55,64,76,131,53,224,12,128,6,240,2,128,7,240,66,127,74,112,8,128,73,184,9, 
  128,58,160,139,127,59,0,75,129,108,248,3,128,109,248,2,128,46,64,16,128,47,112,15,128,75,40,135,128,103,40,6,128,107,248,4,128,8,4,15,1,110,245,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137, 
  6,1,0,17,1,68,4,1,0,1,4,15,1,110,245,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,110,245,0,0,15,1,113,6,1,0,15,1,125,6,1, 
  0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,110,245,0,0,15,1,113,6,1,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0, 
  0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,110,245,0,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0, 
  0,48,0,0,0,1,0,17,1,110,245,0,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,110,245,0,0,1,4,15,1,110,245, 
  0,0,15,1,224,230,0,0,17,1,72,230,0,0,1,4,15,1,110,245,0,0,15,1,224,230,0,0,17,1,2,230,0,0,1,4,15,1,110,245,0,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,110,245, 
  0,0,15,1,47,229,0,0,17,1,239,228,0,0,1,4,15,1,110,245,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0, 
  0,0,1,0,15,1,110,245,0,0,17,1,103,213,0,0,1,4,15,1,110,245,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,110,245,0,0,15,1,103,213,0,0,15,1,227,228, 
  0,0,17,1,201,2,0,0,1,2,21,1,53,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,121,0,0,0,102,1,0,0,6, 
  0,14,1,2,21,1,74,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,19,122,0,0,0, 
  103,1,0,0,1,0,19,123,0,0,0,105,1,0,0,3,0,1,2,19,90,0,0,0,11,1,0,0,4,0,1,19,94,0,0,0,22,1,0,0,1,0,1,21,1,63,0,0,0,85,227,1,0,5,0,0,0,2,0,0, 
  0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,19,91,0,0,0,12,1,0,0,2,0,1,2,21,1,77,0,0,0,27,67,2,0,6,0,0, 
  0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,248,1,128,31,88,1,128,8,4,15,1,149,246,0,0,15,1,251,245,0,0,17,1,57,242,0,0,1,4,15,1,149,246,0,0,17,1,4, 
  235,0,0,1,2,19,90,0,0,0,10,1,0,0,5,0,1,21,1,77,0,0,0,27,67,2,0,6,0,0,0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,248,1,128,31,88,1,128,8, 
  4,15,1,175,251,0,0,15,1,187,251,0,0,17,1,164,248,0,0,1,4,15,1,175,251,0,0,17,1,239,246,0,0,1,2,21,1,131,0,0,0,117,63,2,0,8,0,0,0,3,0,0,0,10,120,67,129,1,144,1, 
  128,2,144,129,127,131,152,1,128,31,216,2,128,130,56,2,128,6,144,1,128,7,144,65,127,8,4,15,1,139,247,0,0,15,1,152,248,0,0,17,1,128,240,0,0,1,4,15,1,139,247,0,0,15,1,127,247,0,0,17, 
  1,236,237,0,0,1,4,15,1,139,247,0,0,15,1,115,247,0,0,17,1,164,248,0,0,1,4,15,1,139,247,0,0,15,1,127,247,0,0,17,1,136,235,0,0,1,2,19,97,0,0,0,28,1,0,0,1,0,1,19, 
  97,0,0,0,30,1,0,0,1,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,97,232,130,128,98,224,130,128,125,192,1,128,126,48,1,128,19,97,0,0,0,28,1,0,0,1,0, 
  17,1,139,247,0,0,1,19,97,0,0,0,29,1,0,0,1,0,17,1,139,247,0,0,1,19,97,0,0,0,30,1,0,0,1,0,17,1,139,247,0,0,1,1,15,1,139,247,0,0,17,1,246,247,0,0,1,2,21,1, 
  125,0,0,0,153,66,2,0,9,0,0,0,3,0,0,0,10,120,67,129,1,176,1,128,2,176,129,127,11,8,67,129,31,152,2,128,130,40,2,128,6,176,1,128,7,176,65,127,131,184,1,128,8,4,15,1,140,248,0,0, 
  17,1,128,240,0,0,1,4,15,1,128,248,0,0,17,1,236,237,0,0,1,4,15,1,116,248,0,0,17,1,164,248,0,0,1,4,19,98,0,0,0,35,1,0,0,3,0,14,1,4,15,1,128,248,0,0,17,1,136,235, 
  0,0,1,2,19,97,0,0,0,31,1,0,0,2,0,1,19,97,0,0,0,33,1,0,0,2,0,1,19,97,0,0,0,32,1,0,0,2,0,1,19,97,0,0,0,29,1,0,0,1,0,1,21,1,82,0,0,0,122,67, 
  2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,32,2,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,15,1,247,248,0,0,17,1,180,132,0,0,1, 
  4,15,1,247,248,0,0,17,1,122,3,0,0,1,2,21,1,47,0,0,0,144,248,1,0,5,0,0,0,2,0,0,0,32,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,39,249,0, 
  0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,87,249,0,0,1,2,21,1,34,2,0,0,54,230,1, 
  0,19,0,0,0,4,0,0,0,49,80,14,128,1,240,194,127,2,240,66,128,50,128,13,128,55,64,76,131,53,224,12,128,6,240,2,128,7,240,66,127,74,112,8,128,73,184,9,128,58,160,139,127,59,0,75,129,108,248,3, 
  128,109,248,2,128,46,64,16,128,47,112,15,128,75,40,135,128,103,40,6,128,107,248,4,128,8,4,15,1,122,251,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15, 
  1,122,251,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,122,251,0,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1, 
  0,17,1,3,3,1,0,1,4,15,1,122,251,0,0,15,1,113,6,1,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0, 
  0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,122,251,0,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,122,251,0, 
  0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,122,251,0,0,1,4,15,1,122,251,0,0,15,1,224,230,0,0,17,1,72,230, 
  0,0,1,4,15,1,122,251,0,0,15,1,224,230,0,0,17,1,2,230,0,0,1,4,15,1,122,251,0,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,122,251,0,0,15,1,47,229,0,0,17,1,239,228, 
  0,0,1,4,15,1,122,251,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0,0,0,1,0,15,1,122,251,0,0,17,1, 
  103,213,0,0,1,4,15,1,122,251,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,122,251,0,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,201,2,0,0,1,2,21,1, 
  52,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,126,0,0,0,109,1,0,0,6,0,1,2,19,95,0,0,0,24,1,0,0, 
  4,0,1,19,98,0,0,0,34,1,0,0,1,0,1,21,1,77,0,0,0,27,67,2,0,6,0,0,0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,248,1,128,31,88,1,128,8,4,15, 
  1,133,2,1,0,15,1,145,2,1,0,17,1,202,253,0,0,1,4,15,1,133,2,1,0,17,1,21,252,0,0,1,2,21,1,131,0,0,0,117,63,2,0,8,0,0,0,3,0,0,0,10,120,67,129,1,144,1,128,2, 
  144,129,127,131,152,1,128,31,216,2,128,130,56,2,128,6,144,1,128,7,144,65,127,8,4,15,1,177,252,0,0,15,1,190,253,0,0,17,1,128,240,0,0,1,4,15,1,177,252,0,0,15,1,165,252,0,0,17,1,236, 
  237,0,0,1,4,15,1,177,252,0,0,15,1,153,252,0,0,17,1,202,253,0,0,1,4,15,1,177,252,0,0,15,1,165,252,0,0,17,1,136,235,0,0,1,2,19,101,0,0,0,40,1,0,0,1,0,1,19,101,0, 
  0,0,42,1,0,0,1,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,101,232,194,128,102,224,2,128,127,48,1,128,125,192,1,128,19,101,0,0,0,40,1,0,0,1,0,17,1, 
  177,252,0,0,1,19,101,0,0,0,41,1,0,0,1,0,17,1,177,252,0,0,1,19,101,0,0,0,42,1,0,0,1,0,17,1,177,252,0,0,1,1,15,1,177,252,0,0,17,1,28,253,0,0,1,2,21,1,125,0, 
  0,0,153,66,2,0,9,0,0,0,3,0,0,0,10,120,67,129,1,176,1,128,2,176,129,127,11,8,67,129,31,152,2,128,130,40,2,128,6,176,1,128,7,176,65,127,131,184,1,128,8,4,15,1,178,253,0,0,17,1, 
  128,240,0,0,1,4,15,1,166,253,0,0,17,1,236,237,0,0,1,4,15,1,154,253,0,0,17,1,202,253,0,0,1,4,19,102,0,0,0,47,1,0,0,3,0,14,1,4,15,1,166,253,0,0,17,1,136,235,0,0, 
  1,2,19,101,0,0,0,43,1,0,0,2,0,1,19,101,0,0,0,45,1,0,0,2,0,1,19,101,0,0,0,44,1,0,0,2,0,1,19,101,0,0,0,41,1,0,0,1,0,1,21,1,159,0,0,0,234,67,2,0, 
  9,0,0,0,3,0,0,0,112,72,3,128,1,176,1,129,2,176,1,128,110,232,67,129,76,136,4,128,129,128,2,128,6,176,65,127,7,176,1,128,134,184,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,15,1,205, 
  255,0,0,17,1,71,255,0,0,1,4,19,120,0,0,0,101,1,0,0,1,0,15,1,205,255,0,0,17,1,71,255,0,0,1,4,15,1,205,255,0,0,15,1,71,255,0,0,17,1,214,126,0,0,1,4,15,1,205,255, 
  0,0,15,1,71,255,0,0,17,1,106,254,0,0,1,4,15,1,205,255,0,0,17,1,46,5,0,0,1,2,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0,0,0,41,24,1,128,1,16,193,127,2,16,1,128, 
  31,176,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,175,254,0,0,1,4,15,1,175,254,0,0,17,1,122,3,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,37,40,130, 
  128,13,144,194,127,61,200,129,128,79,56,1,128,109,48,1,128,1,19,61,0,0,0,167,0,0,0,1,0,17,1,175,254,0,0,1,19,109,0,0,0,56,1,0,0,2,0,1,15,1,175,254,0,0,17,1,20,255,0,0, 
  1,19,61,0,0,0,166,0,0,0,1,0,17,1,175,254,0,0,1,2,21,1,39,0,0,0,161,68,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19, 
  13,0,0,0,34,0,0,0,1,0,1,21,0,133,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,108,144,3,129,109,0,3,128,74,32,68,128,110,112,2,128,120,224,65,128,128,80,1,128,19,108,0,0,0,55,1, 
  0,0,1,0,17,1,71,255,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,71,255,0,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,71,255,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1, 
  71,255,0,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,71,255,0,0,1,1,2,21,1,47,0,0,0,144,248,1,0,5,0,0,0,2,0,0,0,32,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6, 
  48,1,128,8,4,17,1,253,255,0,0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,45,0,1,0,1, 
  2,21,1,34,2,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,80,14,128,1,240,194,127,2,240,66,128,50,128,13,128,55,64,76,131,53,224,12,128,6,240,2,128,7,240,66,127,74,112,8,128,73,184,9,128,58, 
  160,139,127,59,0,75,129,108,248,3,128,109,248,2,128,46,64,16,128,47,112,15,128,75,40,135,128,103,40,6,128,107,248,4,128,8,4,15,1,80,2,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137,6,1, 
  0,17,1,68,4,1,0,1,4,15,1,80,2,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1,4,15,1,80,2,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15, 
  1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,80,2,1,0,15,1,113,6,1,0,15,1,157,2,1,0,15,1,169,2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0, 
  1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,80,2,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48, 
  0,0,0,1,0,17,1,80,2,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,17,1,80,2,1,0,1,4,15,1,80,2,1,0, 
  15,1,224,230,0,0,17,1,72,230,0,0,1,4,15,1,80,2,1,0,15,1,224,230,0,0,17,1,2,230,0,0,1,4,15,1,80,2,1,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,80,2,1,0, 
  15,1,47,229,0,0,17,1,239,228,0,0,1,4,15,1,80,2,1,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0,0,0, 
  1,0,15,1,80,2,1,0,17,1,103,213,0,0,1,4,15,1,80,2,1,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,80,2,1,0,15,1,103,213,0,0,15,1,227,228,0,0, 
  17,1,201,2,0,0,1,2,21,1,52,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,127,0,0,0,110,1,0,0,6,0,1, 
  2,19,99,0,0,0,36,1,0,0,4,0,1,19,102,0,0,0,46,1,0,0,1,0,1,19,44,0,0,0,118,0,0,0,1,0,1,21,0,89,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,90,48,2,128,99, 
  16,1,128,70,192,130,127,95,160,129,127,19,70,0,0,0,195,0,0,0,1,0,17,1,169,2,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,169,2,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1, 
  169,2,1,0,1,1,2,21,1,82,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,32,2,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0, 
  1,0,15,1,86,3,1,0,17,1,180,132,0,0,1,4,15,1,86,3,1,0,17,1,122,3,0,0,1,2,19,103,0,0,0,48,1,0,0,2,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  72,224,2,129,105,48,1,128,103,80,2,128,71,72,195,127,104,192,1,128,19,71,0,0,0,198,0,0,0,1,0,17,1,98,3,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,98,3,1,0,1,19,72,0,0, 
  0,199,0,0,0,1,0,17,1,98,3,1,0,1,15,1,98,3,1,0,17,1,83,228,0,0,1,1,2,19,72,0,0,0,199,0,0,0,1,0,1,21,1,82,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6, 
  80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,32,2,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,15,1,44,4,1,0,17,1,180,132,0,0,1,4,15,1,44,4,1,0,17,1,122,3, 
  0,0,1,2,19,104,0,0,0,49,1,0,0,2,0,1,19,71,0,0,0,197,0,0,0,1,0,1,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48, 
  1,128,10,56,1,128,8,4,17,1,116,4,1,0,1,2,21,1,94,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,32,2,128,41,88,1,128,8,4, 
  19,37,0,0,0,101,0,0,0,1,0,15,1,86,5,1,0,17,1,235,4,1,0,1,4,15,1,86,5,1,0,15,1,211,4,1,0,15,1,223,4,1,0,17,1,122,3,0,0,1,2,19,107,0,0,0,52,1,0,0, 
  1,0,1,19,106,0,0,0,51,1,0,0,1,0,1,21,1,39,0,0,0,20,70,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,30,5,1,0,1,8,19,13,0,0,0,34, 
  0,0,0,1,0,1,21,1,55,0,0,0,55,248,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,41,248,0,128,8,4,19,37,0,0,0,101,0,0,0,1,0,19,79,0,0,0,219,0,0,0,3,0, 
  1,2,21,0,145,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,37,144,3,129,13,248,195,127,106,216,1,128,79,112,194,128,61,0,67,128,105,104,2,128,107,112,1,128,15,1,86,5,1,0,17,1,232,5,1,0, 
  1,19,107,0,0,0,52,1,0,0,1,0,17,1,86,5,1,0,1,1,19,61,0,0,0,167,0,0,0,1,0,17,1,86,5,1,0,1,19,106,0,0,0,51,1,0,0,1,0,17,1,86,5,1,0,1,15,1,86,5, 
  1,0,17,1,235,4,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,86,5,1,0,1,2,21,1,112,0,0,0,143,70,2,0,7,0,0,0,2,0,0,0,6,112,1,128,1,112,65,129,2,112,129,127,7,112, 
  65,128,11,16,67,128,31,112,2,128,41,120,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,15,1,89,6,1,0,15,1,223,4,1,0,17,1,180,132,0,0,1,4,15,1,89,6,1,0,15,1,223,4,1,0,17, 
  1,122,3,0,0,1,4,19,105,0,0,0,50,1,0,0,4,0,14,1,2,19,107,0,0,0,53,1,0,0,2,0,1,19,0,0,0,0,0,0,0,0,3,0,1,19,14,0,0,0,48,0,0,0,1,0,1,19,44,0, 
  0,0,119,0,0,0,1,0,1,19,71,0,0,0,198,0,0,0,1,0,1,21,1,49,2,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,200,14,128,1,240,194,127,2,240,66,128,50,248,13,128,55,184,76,131,53, 
  88,13,128,6,240,2,128,7,240,66,127,74,152,8,128,73,8,10,128,58,24,140,127,59,120,75,129,108,248,3,128,109,248,2,128,46,184,16,128,47,232,15,128,75,40,135,128,103,40,6,128,107,248,4,128,8,4,15,1,199, 
  8,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,137,6,1,0,17,1,68,4,1,0,1,4,15,1,199,8,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,56,4,1,0,17,1,217,3,1,0,1, 
  4,15,1,199,8,1,0,15,1,113,6,1,0,15,1,125,6,1,0,15,1,98,3,1,0,15,1,205,3,1,0,17,1,3,3,1,0,1,4,15,1,199,8,1,0,15,1,113,6,1,0,15,1,157,2,1,0,15,1,169, 
  2,1,0,17,1,33,234,0,0,1,4,19,73,0,0,0,203,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,73,0, 
  0,0,202,0,0,0,1,0,19,44,0,0,0,120,0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,73,0,0,0,201,0,0,0,1,0,19,44,0,0,0,120, 
  0,0,0,1,0,19,14,0,0,0,48,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,15,1,199,8,1,0,15,1,224,230,0,0,17,1,72,230,0,0,1,4,15,1,199,8,1,0,15,1,224,230,0, 
  0,17,1,2,230,0,0,1,4,15,1,199,8,1,0,15,1,224,230,0,0,17,1,106,229,0,0,1,4,15,1,199,8,1,0,15,1,47,229,0,0,17,1,239,228,0,0,1,4,15,1,199,8,1,0,15,1,103,213,0, 
  0,15,1,227,228,0,0,17,1,39,213,0,0,1,4,19,38,0,0,0,112,0,0,0,1,0,19,19,0,0,0,55,0,0,0,1,0,15,1,199,8,1,0,17,1,103,213,0,0,1,4,15,1,199,8,1,0,15,1,103, 
  213,0,0,15,1,227,228,0,0,17,1,231,212,0,0,1,4,15,1,199,8,1,0,15,1,103,213,0,0,15,1,227,228,0,0,17,1,201,2,0,0,1,2,19,0,0,0,0,1,0,0,0,3,0,1,13,15,1,211,8, 
  1,0,17,1,225,8,1,0,1,21,7,48,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,133,10,1,0,15,1,109,10,1,0,15,1,121,10,1,0,17,1,5,10,1,0,1,21,9,243, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,120,2,128,5,8,5,128,2,48,129,128,3,192,3,128,6,80,6,128,4,19,23,0,0,0,62,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3, 
  0,0,0,5,0,0,0,1,0,17,1,133,10,1,0,1,4,19,23,0,0,0,60,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,133,10,1,0,1,4,19,23, 
  0,0,0,61,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,133,10,1,0,1,4,19,23,0,0,0,63,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1, 
  0,19,3,0,0,0,5,0,0,0,1,0,17,1,133,10,1,0,1,4,19,23,0,0,0,64,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,17,1,133,10,1,0,1, 
  2,21,9,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,152,1,128,5,104,2,128,2,48,129,128,3,0,2,128,6,208,2,128,4,19,24,0,0,0,67,0,0,0,2,0,1,4,19,24,0,0,0,65,0, 
  0,0,2,0,1,4,19,24,0,0,0,66,0,0,0,2,0,1,4,19,24,0,0,0,68,0,0,0,2,0,1,4,19,24,0,0,0,69,0,0,0,2,0,1,2,19,3,0,0,0,5,0,0,0,1,0,1,19,2,0, 
  0,0,4,0,0,0,1,0,1,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,24,48,1,128,1,72,3,128,2,184,2,128,3,80,66,128,23,192,1,128,19,2,0,0,0,4,0,0,0,1,0,17,1, 
  133,10,1,0,1,19,2,0,0,0,3,0,0,0,1,0,17,1,133,10,1,0,1,15,1,133,10,1,0,17,1,240,10,1,0,1,19,3,0,0,0,5,0,0,0,1,0,17,1,133,10,1,0,1,1,2,21,7,42,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,251,11,1,0,15,1,121,10,1,0,17,1,5,10,1,0,1,21,9,213,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,72,2,128, 
  5,120,4,128,2,48,129,128,3,96,3,128,6,144,5,128,4,19,23,0,0,0,62,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,23,0,0,0,60,0,0, 
  0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,23,0,0,0,61,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0, 
  1,4,19,23,0,0,0,63,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,23,0,0,0,64,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19, 
  3,0,0,0,6,0,0,0,2,0,1,19,1,0,0,0,2,0,0,0,1,0,1,19,3,0,0,0,6,0,0,0,2,0,1,15,1,211,8,1,0,17,1,20,12,1,0,1,21,1,202,0,0,0,10,71,2,0,13,0, 
  0,0,3,0,0,0,24,112,197,130,1,48,2,129,2,48,2,128,27,144,4,128,22,224,69,129,25,0,197,128,6,48,130,127,7,48,194,128,41,248,3,129,62,136,3,128,63,24,3,128,64,168,2,128,65,56,2,128,8,4, 
  15,1,205,72,1,0,17,1,5,69,1,0,1,4,15,1,205,72,1,0,17,1,162,68,1,0,1,4,15,1,205,72,1,0,17,1,173,66,1,0,1,4,15,1,205,72,1,0,17,1,183,64,1,0,1,4,19,37,0,0, 
  0,101,0,0,0,1,0,17,1,205,72,1,0,1,4,15,1,205,72,1,0,17,1,63,54,1,0,1,4,15,1,205,72,1,0,17,1,66,45,1,0,1,4,15,1,205,72,1,0,17,1,34,37,1,0,1,4,15,1,205, 
  72,1,0,17,1,223,12,1,0,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0, 
  1,0,17,1,26,13,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,27,128,129,128,13,136,193,127,37,240,0,128,19,13,0,0,0,34,0,0,0,1,0,17,1,26,13,1,0,1,1,15, 
  1,26,13,1,0,17,1,89,13,1,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,137,13,1,0,1, 
  2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127, 
  104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,207,17,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,207,17,1,0,1,4,19,114,0,0,0,94,1,0,0, 
  1,0,17,1,207,17,1,0,1,4,15,1,207,17,1,0,17,1,214,126,0,0,1,4,15,1,207,17,1,0,17,1,242,16,1,0,1,4,15,1,207,17,1,0,17,1,99,14,1,0,1,4,15,1,207,17,1,0,17,1, 
  46,5,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,207,17,1,0,1,4,15,1,207,17,1,0,17,1,122,3,0,0,1,2,21,1,176,0,0,0,67,249,1,0,11,0,0,0,3,0,0,0,112,40,3, 
  128,1,240,1,129,2,240,1,128,31,16,5,128,76,8,4,128,41,120,4,129,6,240,129,128,7,240,1,127,110,152,131,128,129,144,2,128,134,248,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,20,15,1,0, 
  1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,20,15,1,0,1,4,15,1,20,15,1,0,17,1,214,126,0,0,1,4,15,1,20,15,1,0,17,1,145,126,0,0,1,4,15,1,20,15,1,0,17,1,46,5,0, 
  0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,20,15,1,0,1,4,15,1,20,15,1,0,17,1,122,3,0,0,1,2,21,0,89,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,112,8,6,129,113,0, 
  70,129,61,104,73,128,109,40,7,128,128,224,4,128,37,208,9,128,129,120,4,128,135,16,4,128,120,112,69,128,136,168,3,128,74,216,136,129,139,176,2,128,108,184,7,128,13,56,74,125,110,152,6,128,79,72,8,128,138,64, 
  3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,20,15,1,0,1,15,1,20,15,1,0,17,1,132,36,0,0,1,15,1,20,15,1,0,17,1,121,32,0,0,1,15,1,20,15,1,0,17,1,110,16,1,0,1,15, 
  1,20,15,1,0,17,1,250,24,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,20,15,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,20,15,1,0,1,1,19,136,0,0,0,135,1,0,0,1,0, 
  17,1,20,15,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,20,15,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,20,15,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,20,15,1,0, 
  1,19,61,0,0,0,167,0,0,0,1,0,17,1,20,15,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,20,15,1,0,1,15,1,20,15,1,0,17,1,26,16,0,0,1,15,1,20,15,1,0,17,1,231,15, 
  0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,20,15,1,0,1,2,21,1,65,0,0,0,46,4,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,83,200,1,128, 
  125,88,1,128,8,4,19,113,0,0,0,93,1,0,0,3,0,14,1,4,17,1,176,16,1,0,1,2,21,1,53,0,0,0,244,78,2,0,5,0,0,0,2,0,0,0,124,48,1,128,1,160,1,128,2,160,129,128,7,160, 
  1,128,6,160,1,128,4,19,113,0,0,0,91,1,0,0,4,0,14,1,8,19,113,0,0,0,92,1,0,0,3,0,14,1,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0,0,0,41,24,1,128,1,16,193,127, 
  2,16,1,128,31,176,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,55,17,1,0,1,4,15,1,55,17,1,0,17,1,122,3,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0, 
  0,37,40,130,128,13,144,194,127,61,200,129,128,79,56,1,128,109,48,1,128,1,19,61,0,0,0,167,0,0,0,1,0,17,1,55,17,1,0,1,19,109,0,0,0,56,1,0,0,2,0,1,15,1,55,17,1,0,17,1, 
  156,17,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,55,17,1,0,1,2,21,1,39,0,0,0,204,79,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0, 
  0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,0,189,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112,240,134,132,113,96,134,132,114,208,5,128,59,128,204,128,61,176,203,128,37,232,12,128,75,144,202, 
  131,77,40,74,130,120,64,5,128,108,160,8,128,74,32,139,130,27,80,13,126,60,24,76,127,13,88,205,125,78,192,201,128,79,48,9,128,109,16,8,128,110,128,7,128,128,176,4,128,129,72,4,128,138,224,3,128,139,80,3, 
  128,19,138,0,0,0,141,1,0,0,1,0,17,1,207,17,1,0,1,15,1,207,17,1,0,17,1,65,36,1,0,1,15,1,207,17,1,0,17,1,165,34,1,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,207,17, 
  1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,207,17,1,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,207,17,1,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,207,17,1,0,1,19,77,0, 
  0,0,212,0,0,0,1,0,17,1,207,17,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,207,17,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,207,17,1,0,1,19,74,0,0,0,205,0,0,0, 
  1,0,17,1,207,17,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,207,17,1,0,1,15,1,207,17,1,0,17,1,100,34,1,0,1,15,1,207,17,1,0,17,1,254,28,1,0,1,19,60,0,0,0,163,0, 
  0,0,1,0,17,1,207,17,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,207,17,1,0,1,15,1,207,17,1,0,17,1,203,28,1,0,1,15,1,207,17,1,0,17,1,192,19,1,0,1,19,27,0,0,0, 
  79,0,0,0,4,0,14,1,15,1,207,17,1,0,17,1,141,19,1,0,1,1,19,61,0,0,0,166,0,0,0,1,0,17,1,207,17,1,0,1,2,21,1,39,0,0,0,240,80,2,0,3,0,0,0,1,0,0,0,2, 
  48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,47,0,0,0,246,81,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112, 
  129,127,7,112,1,128,66,48,1,128,4,17,1,251,19,1,0,1,8,19,59,0,0,0,162,0,0,0,1,0,1,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130, 
  129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,213,20,1,0, 
  1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,213,20,1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,213,20,1,0,1,4,15,1,213,20,1,0,17,1,214,126,0,0,1,4,15,1,213,20,1,0, 
  17,1,229,36,0,0,1,4,15,1,213,20,1,0,17,1,220,13,0,0,1,4,15,1,213,20,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,213,20,1,0,1,4,15,1,213,20,1, 
  0,17,1,116,10,0,0,1,2,21,0,149,1,0,0,255,255,255,255,20,0,0,0,4,0,0,0,112,176,6,132,113,32,6,132,114,144,5,128,61,64,75,128,77,232,201,128,37,176,11,128,108,96,8,128,109,208,7,128,120, 
  0,5,128,110,64,7,128,74,176,10,130,75,80,10,130,60,168,139,126,13,24,140,125,78,128,201,126,79,240,8,128,128,112,4,128,129,8,4,128,138,160,3,128,139,16,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1, 
  213,20,1,0,1,15,1,213,20,1,0,17,1,106,28,1,0,1,15,1,213,20,1,0,17,1,176,27,1,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,213,20,1,0,1,19,108,0,0,0,54,1,0,0,1,0, 
  17,1,213,20,1,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,213,20,1,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,213,20,1,0,1,19,77,0,0,0,212,0,0,0,1,0,17,1,213,20,1,0, 
  1,19,74,0,0,0,207,0,0,0,1,0,17,1,213,20,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,213,20,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,213,20,1,0,1,19,61,0,0,0, 
  167,0,0,0,1,0,17,1,213,20,1,0,1,15,1,213,20,1,0,17,1,111,27,1,0,1,15,1,213,20,1,0,17,1,209,22,1,0,1,19,60,0,0,0,164,0,0,0,3,0,1,19,138,0,0,0,142,1,0,0, 
  1,0,17,1,213,20,1,0,1,15,1,213,20,1,0,17,1,158,22,1,0,1,1,15,1,213,20,1,0,17,1,107,22,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,213,20,1,0,1,2,21,1,39,0,0, 
  0,137,82,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,39,0,0,0,131,83,2,0,3,0,0,0, 
  1,0,0,0,2,240,0,128,1,240,64,128,139,248,0,128,8,4,17,1,77,16,0,0,1,19,138,0,0,0,140,1,0,0,1,0,1,21,1,217,0,0,0,125,84,2,0,13,0,0,0,3,0,0,0,112,208,3,128,1, 
  72,5,129,2,72,133,129,31,232,197,129,76,200,2,128,41,56,131,129,6,72,197,128,7,72,5,127,82,88,6,128,110,216,196,128,127,64,4,128,129,80,5,128,134,48,2,128,4,19,128,0,0,0,111,1,0,0,1,0,17, 
  1,182,23,1,0,1,4,15,1,182,23,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,182,23,1,0,1,4,15,1,182,23,1,0,17,1,214,126,0,0,1,4,19,114,0,0,0,94, 
  1,0,0,1,0,17,1,182,23,1,0,1,4,15,1,182,23,1,0,17,1,229,36,0,0,1,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,182,23,1,0,1,4,15,1,182,23,1,0,17,1,116,10,0,0,1, 
  4,15,1,182,23,1,0,17,1,220,13,0,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,240,68,130,113,72,198,131,114,16,7,128,61,96,67,128, 
  77,88,132,128,37,64,10,128,109,200,3,128,110,208,2,128,120,144,8,128,128,32,9,128,74,176,201,129,139,96,4,128,108,112,7,128,13,80,133,125,78,88,68,126,79,0,8,128,129,224,5,128,138,168,6,128,19,74,0,0, 
  0,207,0,0,0,1,0,17,1,182,23,1,0,1,15,1,182,23,1,0,17,1,60,27,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,182,23,1,0,1,1,19,138,0,0,0,141,1,0,0,1,0,17,1,182, 
  23,1,0,1,19,77,0,0,0,214,0,0,0,2,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,182,23,1,0,1,15,1,182,23,1,0,17,1,160,25,1,0,1,19,77,0,0,0,215,0,0,0,2,0,1,15, 
  1,182,23,1,0,17,1,63,25,1,0,1,19,78,0,0,0,216,0,0,0,2,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,182,23,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,182,23,1,0,1, 
  19,108,0,0,0,54,1,0,0,1,0,17,1,182,23,1,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,182,23,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,182,23,1,0,1,15,1,182,23,1,0, 
  17,1,12,25,1,0,1,2,21,1,39,0,0,0,137,82,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8,0,0,1,19,13,0,0,0,34,0,0,0,1,0,1,21, 
  1,85,0,0,0,74,85,2,0,6,0,0,0,2,0,0,0,136,144,1,128,1,160,194,128,2,160,2,128,135,80,1,128,25,208,65,128,33,56,2,128,4,17,1,242,20,0,0,1,4,17,1,159,19,0,0,1,4,19,129, 
  0,0,0,112,1,0,0,2,0,1,4,19,129,0,0,0,113,1,0,0,2,0,1,8,19,129,0,0,0,120,1,0,0,1,0,1,21,1,69,0,0,0,68,86,2,0,5,0,0,0,2,0,0,0,120,48,1,128,1,32, 
  2,128,2,32,130,128,123,112,1,128,10,176,1,128,4,17,1,148,26,1,0,1,4,17,1,175,28,0,0,1,4,15,1,241,25,1,0,17,1,75,25,0,0,1,8,19,112,0,0,0,90,1,0,0,1,0,1,21,0,40, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,241,25,1,0,17,1,26,26,1,0,1,1,2,21,1,51,0,0,0,39,87,2,0,4,0,0,0,2,0,0,0,120,88,1,128, 
  1,80,1,128,2,80,1,128,123,16,1,128,4,17,1,99,28,0,0,1,8,4,17,1,90,26,1,0,1,19,112,0,0,0,78,1,0,0,2,0,14,1,21,1,45,0,0,0,255,87,2,0,3,0,0,0,1,0,0,0, 
  2,96,1,128,1,96,65,128,123,240,0,128,4,19,112,0,0,0,89,1,0,0,4,0,14,1,8,19,112,0,0,0,83,1,0,0,3,0,14,1,21,1,57,0,0,0,204,88,2,0,4,0,0,0,2,0,0,0,10,16, 
  1,128,1,128,1,128,2,128,129,127,123,136,1,128,4,15,1,217,26,1,0,17,1,75,25,0,0,1,8,4,17,1,130,31,0,0,1,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,217,26,1,0,17,1,2,27,1,0,1,1,2,21,1,45,0,0,0,255,87,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,123,240,0,128, 
  4,19,112,0,0,0,86,1,0,0,4,0,14,1,8,19,112,0,0,0,76,1,0,0,3,0,14,1,21,1,39,0,0,0,131,83,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,139,240,0,128,4,17, 
  1,77,16,0,0,1,8,19,138,0,0,0,140,1,0,0,1,0,1,21,1,53,0,0,0,164,89,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2,160,129,127,7,160,1,128,45,48,1,128,4,15,1, 
  142,120,0,0,17,1,55,44,0,0,1,8,19,75,0,0,0,209,0,0,0,1,0,1,21,1,69,0,0,0,68,86,2,0,5,0,0,0,2,0,0,0,120,112,1,128,1,32,2,128,2,32,130,128,123,48,1,128,10,176, 
  1,128,4,17,1,175,28,0,0,1,4,17,1,148,26,1,0,1,4,15,1,1,28,1,0,17,1,75,25,0,0,1,8,19,112,0,0,0,90,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,112,56,65,128,132,208,0,128,15,1,1,28,1,0,17,1,42,28,1,0,1,1,2,21,1,51,0,0,0,39,87,2,0,4,0,0,0,2,0,0,0,120,80,1,128,1,144,1,128,2,144,1,128,123,16,1,128, 
  4,17,1,99,28,0,0,1,4,17,1,90,26,1,0,1,8,19,112,0,0,0,78,1,0,0,2,0,14,1,21,1,85,0,0,0,74,85,2,0,6,0,0,0,2,0,0,0,136,80,1,128,1,160,194,128,2,160,2,128, 
  135,96,2,128,25,144,65,128,33,248,1,128,4,17,1,159,19,0,0,1,4,19,129,0,0,0,112,1,0,0,2,0,1,4,19,129,0,0,0,113,1,0,0,2,0,1,4,17,1,242,20,0,0,1,8,19,129,0,0,0, 
  120,1,0,0,1,0,1,21,1,39,0,0,0,4,90,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,139,240,0,128,4,17,1,77,16,0,0,1,8,19,138,0,0,0,140,1,0,0,1,0,1,21,1, 
  212,0,0,0,64,91,2,0,13,0,0,0,3,0,0,0,112,152,5,128,1,200,2,129,2,200,130,129,31,72,196,129,76,216,3,128,41,104,131,129,6,200,194,128,7,200,2,127,82,184,4,128,110,40,197,128,127,8,6,128, 
  129,208,2,128,134,48,2,128,4,19,128,0,0,0,111,1,0,0,1,0,17,1,14,33,1,0,1,10,4,19,120,0,0,0,101,1,0,0,1,0,17,1,14,33,1,0,1,6,15,1,14,33,1,0,17,1,222,29,1,0, 
  1,4,15,1,14,33,1,0,17,1,40,12,0,0,1,4,15,1,14,33,1,0,17,1,116,10,0,0,1,4,15,1,14,33,1,0,17,1,220,13,0,0,1,4,15,1,14,33,1,0,17,1,229,36,0,0,1,4,15,1, 
  14,33,1,0,17,1,214,126,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,14,33,1,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,1,76,1,0,0,36,92,2,0,29,0,0,0,4,0,0,0, 
  112,160,8,128,1,240,196,131,2,240,4,132,3,56,7,128,4,56,7,128,22,160,7,133,6,240,196,127,7,240,196,132,24,48,68,131,25,56,5,130,10,112,4,128,27,120,198,130,76,184,6,128,45,32,10,128,110,248,6,128, 
  31,248,5,129,33,96,9,130,41,32,9,128,66,120,133,128,79,32,8,129,82,160,9,128,120,224,135,129,123,224,137,129,127,96,8,128,129,248,4,128,134,184,5,128,135,56,6,128,136,176,4,128,139,224,8,128,12,17,1,43, 
  31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,10,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0, 
  1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,19,78,0,0,0,218,0,0,0,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1, 
  0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17, 
  1,43,31,1,0,1,12,17,1,43,31,1,0,1,2,21,1,176,0,0,0,67,249,1,0,11,0,0,0,3,0,0,0,112,40,3,128,1,240,1,129,2,240,1,128,31,16,5,128,76,8,4,128,41,120,4,129,6,240,129, 
  128,7,240,1,127,110,152,131,128,129,144,2,128,134,248,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,220,31,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,220,31,1,0,1,4,15,1,220, 
  31,1,0,17,1,214,126,0,0,1,4,15,1,220,31,1,0,17,1,229,36,0,0,1,4,15,1,220,31,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,220,31,1,0,1,4,15,1, 
  220,31,1,0,17,1,116,10,0,0,1,2,21,0,49,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,112,240,4,131,129,104,3,128,74,152,7,131,139,112,2,128,108,112,6,128,13,248,200,128,110,80,5,128,79,0, 
  7,128,37,144,72,128,61,40,72,128,77,144,71,128,109,224,5,128,120,96,68,128,128,208,3,128,138,0,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,220,31,1,0,1,15,1,220,31,1,0,17,1,249,41,0,0, 
  1,15,1,220,31,1,0,17,1,90,42,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,220,31,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,220,31,1,0,1,19,77,0,0,0,214,0,0,0,2, 
  0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,220,31,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,220,31,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,220,31,1,0,1,19,61,0,0, 
  0,167,0,0,0,1,0,17,1,220,31,1,0,1,1,19,138,0,0,0,142,1,0,0,1,0,17,1,220,31,1,0,1,15,1,220,31,1,0,17,1,209,121,0,0,1,15,1,220,31,1,0,17,1,198,41,0,0,1,19, 
  61,0,0,0,166,0,0,0,1,0,17,1,220,31,1,0,1,2,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,48,68,130,113,208,194,131,114,152,8,128,61,144,68,128,77,192,131,128,37,56,7,128, 
  109,48,3,128,110,136,5,128,120,8,8,128,128,248,4,128,74,24,198,129,139,248,8,128,108,136,9,128,13,168,134,125,78,192,67,126,79,24,10,128,129,200,3,128,138,160,7,128,19,77,0,0,0,215,0,0,0,2,0,1, 
  19,74,0,0,0,206,0,0,0,1,0,17,1,14,33,1,0,1,1,15,1,14,33,1,0,17,1,90,42,0,0,1,19,77,0,0,0,214,0,0,0,2,0,1,15,1,14,33,1,0,17,1,209,121,0,0,1,19,108,0, 
  0,0,55,1,0,0,1,0,17,1,14,33,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,14,33,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,14,33,1,0,1,19,61,0,0,0,166,0,0,0, 
  1,0,17,1,14,33,1,0,1,15,1,14,33,1,0,17,1,198,41,0,0,1,15,1,14,33,1,0,17,1,249,41,0,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,14,33,1,0,1,19,78,0,0,0,216,0, 
  0,0,2,0,1,19,138,0,0,0,141,1,0,0,1,0,17,1,14,33,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,14,33,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,14,33,1,0,1,2, 
  21,1,53,0,0,0,66,93,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,193,128,2,160,129,127,7,160,1,128,45,48,1,128,4,15,1,142,120,0,0,17,1,55,44,0,0,1,8,19,75,0,0,0,209,0, 
  0,0,1,0,1,21,1,69,0,0,0,213,93,2,0,5,0,0,0,2,0,0,0,120,112,1,128,1,32,2,128,2,32,130,128,123,48,1,128,10,176,1,128,4,17,1,30,35,0,0,1,4,17,1,153,35,1,0,1,4, 
  15,1,246,34,1,0,17,1,75,25,0,0,1,8,19,112,0,0,0,90,1,0,0,1,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,246,34,1,0,17,1, 
  31,35,1,0,1,1,2,21,1,51,0,0,0,207,94,2,0,4,0,0,0,2,0,0,0,120,16,1,128,1,144,1,128,2,144,1,128,123,80,1,128,4,17,1,95,35,1,0,1,4,17,1,99,28,0,0,1,8,19,112, 
  0,0,0,78,1,0,0,2,0,14,1,21,1,45,0,0,0,190,95,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,123,240,0,128,4,19,112,0,0,0,89,1,0,0,4,0,14,1,8,19,112,0,0, 
  0,83,1,0,0,3,0,14,1,21,1,57,0,0,0,162,96,2,0,4,0,0,0,2,0,0,0,10,80,1,128,1,192,1,128,2,192,129,127,123,16,1,128,4,17,1,130,31,0,0,1,4,15,1,222,35,1,0,17,1, 
  75,25,0,0,1,8,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,222,35,1,0,17,1,7,36,1,0,1,1,2,21, 
  1,45,0,0,0,190,95,2,0,3,0,0,0,1,0,0,0,2,96,1,128,1,96,65,128,123,240,0,128,4,19,112,0,0,0,86,1,0,0,4,0,14,1,8,19,112,0,0,0,76,1,0,0,3,0,14,1,21,1,80, 
  0,0,0,145,97,2,0,6,0,0,0,2,0,0,0,136,152,1,128,1,144,193,128,2,144,1,128,135,216,1,128,25,80,65,128,33,24,2,128,6,17,1,157,36,1,0,1,10,4,17,1,159,19,0,0,1,4,17,1,242, 
  20,0,0,1,4,19,129,0,0,0,113,1,0,0,2,0,1,19,129,0,0,0,120,1,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,12,19,129,0,0,0,120,1,0, 
  0,1,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,12,17,1,227,36,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0, 
  128,4,19,129,0,0,0,112,1,0,0,2,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0, 
  6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,93,37,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,28, 
  128,1,128,13,136,65,128,37,240,0,128,19,13,0,0,0,34,0,0,0,1,0,17,1,93,37,1,0,1,1,15,1,93,37,1,0,17,1,156,37,1,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0, 
  0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,204,37,1,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48, 
  130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,166,38,1, 
  0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,166,38,1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,166,38,1,0,1,4,15,1,166,38,1,0,17,1,214,126,0,0,1,4,15,1,166,38,1, 
  0,17,1,242,16,1,0,1,4,15,1,166,38,1,0,17,1,99,14,1,0,1,4,15,1,166,38,1,0,17,1,46,5,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,166,38,1,0,1,4,15,1,166,38, 
  1,0,17,1,122,3,0,0,1,2,21,0,189,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112,240,134,132,113,96,134,132,114,208,5,128,60,24,140,129,61,176,203,128,37,232,12,128,75,144,202,131,77,40,74,130, 
  120,64,5,128,108,160,8,128,74,32,139,130,59,128,204,126,28,80,205,125,13,88,205,125,78,192,201,128,79,48,9,128,109,16,8,128,110,128,7,128,128,176,4,128,129,72,4,128,138,224,3,128,139,80,3,128,19,138,0,0, 
  0,141,1,0,0,1,0,17,1,166,38,1,0,1,15,1,166,38,1,0,17,1,230,44,1,0,1,15,1,166,38,1,0,17,1,231,43,1,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,166,38,1,0,1,19,108, 
  0,0,0,54,1,0,0,1,0,17,1,166,38,1,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,166,38,1,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,166,38,1,0,1,19,77,0,0,0,212,0,0, 
  0,1,0,17,1,166,38,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,166,38,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,166,38,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,166, 
  38,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,166,38,1,0,1,15,1,166,38,1,0,17,1,100,34,1,0,1,15,1,166,38,1,0,17,1,100,40,1,0,1,19,60,0,0,0,163,0,0,0,1,0,17, 
  1,166,38,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,166,38,1,0,1,15,1,166,38,1,0,17,1,203,28,1,0,1,15,1,166,38,1,0,17,1,192,19,1,0,1,19,28,0,0,0,80,0,0,0,4, 
  0,14,1,15,1,166,38,1,0,17,1,141,19,1,0,1,1,19,61,0,0,0,166,0,0,0,1,0,17,1,166,38,1,0,1,2,21,1,212,0,0,0,64,91,2,0,13,0,0,0,3,0,0,0,112,152,5,128,1,56, 
  3,129,2,56,131,129,31,40,197,129,76,216,3,128,41,48,130,129,6,56,195,128,7,56,3,127,82,184,4,128,110,72,196,128,127,8,6,128,129,64,3,128,134,160,2,128,6,15,1,145,42,1,0,17,1,68,41,1,0,1, 
  4,19,128,0,0,0,111,1,0,0,1,0,17,1,145,42,1,0,1,10,4,19,120,0,0,0,101,1,0,0,1,0,17,1,145,42,1,0,1,4,15,1,145,42,1,0,17,1,40,12,0,0,1,4,15,1,145,42,1,0, 
  17,1,229,36,0,0,1,4,15,1,145,42,1,0,17,1,220,13,0,0,1,4,15,1,145,42,1,0,17,1,116,10,0,0,1,4,15,1,145,42,1,0,17,1,214,126,0,0,1,4,19,114,0,0,0,94,1,0,0,1, 
  0,17,1,145,42,1,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,1,76,1,0,0,36,92,2,0,29,0,0,0,4,0,0,0,112,56,7,128,1,48,197,131,2,48,5,132,3,120,7,128,4,120,7,128,22,32, 
  9,133,6,48,197,127,7,48,197,132,24,120,69,131,25,184,6,130,10,56,5,128,27,224,199,130,76,184,5,128,45,176,4,128,110,248,5,128,31,160,8,129,33,96,9,130,41,96,8,128,66,112,132,128,79,48,4,129,82,224, 
  9,128,120,160,137,129,123,32,138,129,127,120,6,128,129,32,8,128,134,248,6,128,135,240,4,128,136,224,8,128,139,56,6,128,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17, 
  1,43,31,1,0,1,10,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31, 
  1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,19,78,0,0,0,218,0,0,0,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43, 
  31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,2,21,0,85,1,0,0,255, 
  255,255,255,18,0,0,0,4,0,0,0,112,48,72,130,113,104,199,131,114,240,3,128,61,80,69,128,77,72,133,128,37,200,7,128,109,184,5,128,110,96,3,128,120,216,6,128,128,144,8,128,74,176,201,129,139,72,6,128,108, 
  80,4,128,13,208,130,125,78,72,69,126,79,32,9,128,129,64,10,128,138,224,4,128,19,61,0,0,0,166,0,0,0,1,0,17,1,145,42,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,145,42,1,0,1,19, 
  78,0,0,0,216,0,0,0,2,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,145,42,1,0,1,15,1,145,42,1,0,17,1,249,41,0,0,1,1,15,1,145,42,1,0,17,1,209,121,0,0,1,19,74,0,0, 
  0,206,0,0,0,1,0,17,1,145,42,1,0,1,19,138,0,0,0,141,1,0,0,1,0,17,1,145,42,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,145,42,1,0,1,19,77,0,0,0,215,0,0,0,2, 
  0,1,15,1,145,42,1,0,17,1,198,41,0,0,1,19,77,0,0,0,214,0,0,0,2,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,145,42,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,145,42, 
  1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,145,42,1,0,1,15,1,145,42,1,0,17,1,90,42,0,0,1,2,21,1,69,0,0,0,213,93,2,0,5,0,0,0,2,0,0,0,120,48,1,128,1,32,2, 
  128,2,32,130,128,123,112,1,128,10,176,1,128,4,17,1,161,44,1,0,1,4,17,1,30,35,0,0,1,4,15,1,56,44,1,0,17,1,75,25,0,0,1,8,19,112,0,0,0,90,1,0,0,1,0,1,21,0,40,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,56,44,1,0,17,1,97,44,1,0,1,1,2,21,1,51,0,0,0,207,94,2,0,4,0,0,0,2,0,0,0,120,80,1,128,1, 
  144,1,128,2,144,1,128,123,16,1,128,4,17,1,99,28,0,0,1,4,17,1,95,35,1,0,1,8,19,112,0,0,0,78,1,0,0,2,0,14,1,21,1,57,0,0,0,162,96,2,0,4,0,0,0,2,0,0,0,10, 
  88,1,128,1,80,1,128,2,80,129,127,123,16,1,128,4,17,1,130,31,0,0,1,8,4,15,1,222,35,1,0,17,1,75,25,0,0,1,19,112,0,0,0,80,1,0,0,2,0,1,21,1,80,0,0,0,145,97,2,0, 
  6,0,0,0,2,0,0,0,136,152,1,128,1,144,193,128,2,144,1,128,135,216,1,128,25,80,65,128,33,24,2,128,6,17,1,157,36,1,0,1,10,4,17,1,159,19,0,0,1,4,17,1,242,20,0,0,1,4,19,129, 
  0,0,0,113,1,0,0,2,0,1,19,129,0,0,0,120,1,0,0,1,0,1,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128, 
  8,4,17,1,114,45,1,0,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0,101, 
  0,0,0,1,0,17,1,191,45,1,0,1,4,15,1,191,45,1,0,17,1,122,3,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,29,144,130,128,13,152,194,127,37,40,130,128,79,48,1, 
  128,61,192,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,191,45,1,0,1,15,1,191,45,1,0,17,1,88,46,1,0,1,15,1,191,45,1,0,17,1,37,46,1,0,1,1,19,61,0,0,0,166,0,0,0,1, 
  0,17,1,191,45,1,0,1,2,21,1,39,0,0,0,151,98,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1, 
  21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,136,46,1,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0, 
  0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4, 
  19,128,0,0,0,111,1,0,0,1,0,17,1,98,47,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,98,47,1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,98,47,1,0,1,4,15,1,98, 
  47,1,0,17,1,214,126,0,0,1,4,15,1,98,47,1,0,17,1,242,16,1,0,1,4,15,1,98,47,1,0,17,1,99,14,1,0,1,4,15,1,98,47,1,0,17,1,46,5,0,0,1,4,19,37,0,0,0,101,0, 
  0,0,1,0,17,1,98,47,1,0,1,4,15,1,98,47,1,0,17,1,122,3,0,0,1,2,21,0,189,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,112,240,134,132,113,96,134,132,114,208,5,128,29,80,77,128, 
  61,176,203,128,37,232,12,128,75,144,202,131,77,40,74,130,120,64,5,128,108,160,8,128,74,32,139,130,59,128,204,126,60,24,76,127,13,88,141,125,78,192,201,128,79,48,9,128,109,16,8,128,110,128,7,128,128,176,4,128, 
  129,72,4,128,138,224,3,128,139,80,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,98,47,1,0,1,15,1,98,47,1,0,17,1,227,53,1,0,1,15,1,98,47,1,0,17,1,228,52,1,0,1,19,108,0,0, 
  0,55,1,0,0,1,0,17,1,98,47,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,98,47,1,0,1,19,78,0,0,0,217,0,0,0,1,0,17,1,98,47,1,0,1,19,77,0,0,0,213,0,0,0,1, 
  0,17,1,98,47,1,0,1,19,77,0,0,0,212,0,0,0,1,0,17,1,98,47,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,98,47,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,98,47,1, 
  0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,98,47,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,98,47,1,0,1,15,1,98,47,1,0,17,1,163,52,1,0,1,15,1,98,47,1,0,17,1,32, 
  49,1,0,1,19,60,0,0,0,163,0,0,0,1,0,17,1,98,47,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,98,47,1,0,1,15,1,98,47,1,0,17,1,203,28,1,0,1,15,1,98,47,1,0,17, 
  1,192,19,1,0,1,19,29,0,0,0,81,0,0,0,5,0,14,1,15,1,98,47,1,0,17,1,141,19,1,0,1,1,19,61,0,0,0,166,0,0,0,1,0,17,1,98,47,1,0,1,2,21,1,212,0,0,0,64,91, 
  2,0,13,0,0,0,3,0,0,0,112,184,4,128,1,48,2,129,2,48,130,129,31,152,197,129,76,208,2,128,41,176,131,129,6,48,194,128,7,48,2,127,82,40,5,128,110,64,195,128,127,8,6,128,129,56,2,128,134,32, 
  4,128,10,4,19,120,0,0,0,101,1,0,0,1,0,17,1,77,51,1,0,1,4,15,1,77,51,1,0,17,1,40,12,0,0,1,4,15,1,77,51,1,0,17,1,229,36,0,0,1,6,15,1,77,51,1,0,17,1,0, 
  50,1,0,1,4,19,128,0,0,0,111,1,0,0,1,0,17,1,77,51,1,0,1,4,15,1,77,51,1,0,17,1,214,126,0,0,1,4,15,1,77,51,1,0,17,1,220,13,0,0,1,4,15,1,77,51,1,0,17,1, 
  116,10,0,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,77,51,1,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,1,76,1,0,0,36,92,2,0,29,0,0,0,4,0,0,0,112,248,5,128,1,112, 
  197,131,2,112,5,132,3,120,7,128,4,120,7,128,22,48,4,133,6,112,197,127,7,112,197,132,24,56,71,131,25,224,8,130,10,96,8,128,27,48,197,130,76,248,6,128,45,224,9,128,110,120,5,128,31,160,8,129,33,32, 
  9,130,41,112,4,128,66,160,137,128,79,224,7,129,82,176,4,128,120,32,138,129,123,56,134,129,127,184,5,128,129,240,4,128,134,120,6,128,135,32,8,128,136,96,9,128,139,184,6,128,12,17,1,43,31,1,0,1,12,17, 
  1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,10,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31, 
  1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,19,78,0,0,0,218,0,0,0,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43, 
  31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1, 
  12,17,1,43,31,1,0,1,2,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,40,69,130,113,200,196,131,114,184,9,128,61,24,70,128,77,200,131,128,37,208,2,128,109,128,6,128,110,136,5,128,120, 
  16,7,128,128,152,8,128,74,208,195,129,139,56,3,128,108,24,10,128,13,40,137,125,78,200,67,126,79,8,8,128,129,96,4,128,138,160,7,128,15,1,77,51,1,0,17,1,198,41,0,0,1,19,138,0,0,0,141,1,0, 
  0,1,0,17,1,77,51,1,0,1,1,19,138,0,0,0,142,1,0,0,1,0,17,1,77,51,1,0,1,15,1,77,51,1,0,17,1,90,42,0,0,1,19,77,0,0,0,215,0,0,0,2,0,1,19,77,0,0,0,214, 
  0,0,0,2,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,77,51,1,0,1,15,1,77,51,1,0,17,1,209,121,0,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,77,51,1,0,1,19,108,0,0,0, 
  54,1,0,0,1,0,17,1,77,51,1,0,1,15,1,77,51,1,0,17,1,249,41,0,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,77,51,1,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,77,51,1, 
  0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,77,51,1,0,1,19,78,0,0,0,216,0,0,0,2,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,77,51,1,0,1,2,21,1,53,0,0,0,66,93,2, 
  0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,45,56,1,128,8,4,15,1,142,120,0,0,17,1,55,44,0,0,1,19,75,0,0,0,209,0,0,0,1,0,1,21,1,69,0, 
  0,0,213,93,2,0,5,0,0,0,2,0,0,0,120,232,1,128,1,224,1,128,2,224,129,128,123,48,1,128,10,112,1,128,4,17,1,30,35,0,0,1,4,15,1,122,53,1,0,17,1,75,25,0,0,1,8,4,17,1, 
  53,53,1,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,1,57,0,0,0,162,96,2,0,4,0,0,0,2,0,0,0,10,16,1,128,1,128,1,128,2,128,129,127,123,136,1,128,4,15,1,222,35,1,0,17,1, 
  75,25,0,0,1,8,4,17,1,130,31,0,0,1,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15,1,122,53,1,0,17,1, 
  163,53,1,0,1,1,2,21,1,51,0,0,0,207,94,2,0,4,0,0,0,2,0,0,0,120,80,1,128,1,144,1,128,2,144,1,128,123,16,1,128,4,17,1,99,28,0,0,1,4,17,1,95,35,1,0,1,8,19,112, 
  0,0,0,78,1,0,0,2,0,14,1,21,1,80,0,0,0,145,97,2,0,6,0,0,0,2,0,0,0,136,144,1,128,1,208,193,128,2,208,1,128,135,80,1,128,25,64,66,128,33,216,1,128,4,17,1,242,20,0,0, 
  1,4,17,1,159,19,0,0,1,10,4,19,129,0,0,0,113,1,0,0,2,0,1,6,17,1,157,36,1,0,1,19,129,0,0,0,120,1,0,0,1,0,1,21,1,76,0,0,0,235,98,2,0,6,0,0,0,2,0,0, 
  0,40,240,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,140,54,1,0,1,4,15,1,140,54,1,0,17,1,8,17,0,0,1,2, 
  21,0,96,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,36,40,2,128,37,192,1,128,30,248,130,128,31,144,2,128,62,48,1,128,19,31,0,0,0,83,0,0,0,1,0,17,1,140,54,1,0,1,15,1,140,54, 
  1,0,17,1,71,64,1,0,1,15,1,140,54,1,0,17,1,226,63,1,0,1,15,1,140,54,1,0,17,1,237,54,1,0,1,1,2,21,1,59,0,0,0,173,99,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1, 
  80,193,128,2,80,129,127,7,80,1,128,9,152,65,128,21,88,1,128,8,4,17,1,142,55,1,0,1,4,17,1,41,55,1,0,1,2,21,1,88,0,0,0,235,98,2,0,6,0,0,0,2,0,0,0,40,32,2,128,1, 
  80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,15,1,130,55,1,0,17,1,71,64,1,0,1,4,15,1,130,55,1,0,15,1,226,63,1,0,17,1, 
  231,141,0,0,1,2,19,31,0,0,0,84,0,0,0,3,0,1,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37, 
  0,0,0,101,0,0,0,1,0,17,1,201,55,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,30,128,1,128,13,136,65,128,37,240,0,128,19,13,0,0,0,34,0,0,0,1,0,17,1, 
  201,55,1,0,1,1,15,1,201,55,1,0,17,1,8,56,1,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4, 
  17,1,56,56,1,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224, 
  4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,18,57,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,18,57,1,0,1,4,19,114, 
  0,0,0,94,1,0,0,1,0,17,1,18,57,1,0,1,4,15,1,18,57,1,0,17,1,214,126,0,0,1,4,15,1,18,57,1,0,17,1,242,16,1,0,1,4,15,1,18,57,1,0,17,1,99,14,1,0,1,4,15, 
  1,18,57,1,0,17,1,46,5,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,18,57,1,0,1,4,15,1,18,57,1,0,17,1,122,3,0,0,1,2,21,0,190,1,0,0,255,255,255,255,22,0,0,0, 
  4,0,0,0,112,240,134,132,113,96,134,132,114,208,5,128,61,176,203,128,75,144,74,132,37,240,12,128,77,40,138,130,78,192,137,130,120,64,5,128,108,160,8,128,74,32,139,130,59,128,76,126,60,24,76,127,13,96,141,125, 
  30,88,77,126,79,48,9,128,109,16,8,128,110,128,7,128,128,176,4,128,129,72,4,128,138,224,3,128,139,80,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,18,57,1,0,1,15,1,18,57,1,0,17,1,134,63, 
  1,0,1,15,1,18,57,1,0,17,1,135,62,1,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,18,57,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,18,57,1,0,1,19,78,0,0,0,217,0,0, 
  0,1,0,17,1,18,57,1,0,1,19,77,0,0,0,213,0,0,0,1,0,17,1,18,57,1,0,1,19,77,0,0,0,212,0,0,0,1,0,17,1,18,57,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,18, 
  57,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,18,57,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,18,57,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,18,57,1,0,1,15,1, 
  18,57,1,0,17,1,100,34,1,0,1,15,1,18,57,1,0,17,1,4,59,1,0,1,19,60,0,0,0,163,0,0,0,1,0,17,1,18,57,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,18,57,1,0,1, 
  15,1,18,57,1,0,17,1,203,28,1,0,1,15,1,18,57,1,0,17,1,192,19,1,0,1,19,30,0,0,0,82,0,0,0,6,0,14,14,1,15,1,18,57,1,0,17,1,209,58,1,0,1,1,19,61,0,0,0,166, 
  0,0,0,1,0,17,1,18,57,1,0,1,2,21,1,39,0,0,0,240,80,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8,0,0,1,19,13,0,0,0,34,0,0, 
  0,1,0,1,21,1,212,0,0,0,64,91,2,0,13,0,0,0,3,0,0,0,112,192,5,128,1,208,3,129,2,208,131,129,31,72,196,129,76,184,4,128,41,48,130,129,6,208,195,128,7,208,3,127,82,216,3,128,110,48, 
  198,128,127,40,5,128,129,160,2,128,134,56,3,128,6,15,1,49,61,1,0,17,1,228,59,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,49,61,1,0,1,4,19,128,0,0,0,111,1,0,0,1,0,17, 
  1,49,61,1,0,1,10,4,15,1,49,61,1,0,17,1,220,13,0,0,1,4,15,1,49,61,1,0,17,1,116,10,0,0,1,4,15,1,49,61,1,0,17,1,40,12,0,0,1,4,19,114,0,0,0,94,1,0,0,1, 
  0,17,1,49,61,1,0,1,4,15,1,49,61,1,0,17,1,214,126,0,0,1,4,15,1,49,61,1,0,17,1,229,36,0,0,1,19,78,0,0,0,218,0,0,0,1,0,1,21,1,76,1,0,0,36,92,2,0,29,0, 
  0,0,4,0,0,0,112,56,7,128,1,48,197,131,2,48,5,132,3,184,7,128,4,184,7,128,22,160,9,133,6,48,197,127,7,48,197,132,24,56,69,131,25,32,10,130,10,120,5,128,27,248,198,130,76,96,9,128,45,160, 
  8,128,110,224,8,128,31,32,9,129,33,240,4,130,41,224,9,128,66,56,134,128,79,48,4,129,82,120,6,128,120,176,132,129,123,32,136,129,127,120,7,128,129,96,8,128,134,112,4,128,135,248,5,128,136,184,5,128,139,184, 
  6,128,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,10,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12, 
  17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,19,78,0,0, 
  0,218,0,0,0,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43, 
  31,1,0,1,12,17,1,43,31,1,0,1,12,17,1,43,31,1,0,1,2,21,0,85,1,0,0,255,255,255,255,18,0,0,0,4,0,0,0,112,72,74,130,113,48,196,131,114,96,3,128,61,176,69,128,77,40,132,128,37, 
  192,3,128,109,184,9,128,110,32,5,128,120,152,8,128,128,144,4,128,74,40,201,129,139,8,8,128,108,208,2,128,13,120,135,125,78,40,68,126,79,128,6,128,129,24,6,128,138,16,7,128,19,74,0,0,0,205,0,0,0, 
  1,0,17,1,49,61,1,0,1,19,78,0,0,0,216,0,0,0,2,0,1,15,1,49,61,1,0,17,1,198,41,0,0,1,1,19,77,0,0,0,215,0,0,0,2,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1, 
  49,61,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,49,61,1,0,1,15,1,49,61,1,0,17,1,209,121,0,0,1,15,1,49,61,1,0,17,1,90,42,0,0,1,19,61,0,0,0,167,0,0,0,1,0, 
  17,1,49,61,1,0,1,15,1,49,61,1,0,17,1,249,41,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,49,61,1,0,1,19,138,0,0,0,141,1,0,0,1,0,17,1,49,61,1,0,1,19,108,0,0, 
  0,54,1,0,0,1,0,17,1,49,61,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,49,61,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,49,61,1,0,1,19,77,0,0,0,214,0,0,0,2, 
  0,1,2,21,1,69,0,0,0,213,93,2,0,5,0,0,0,2,0,0,0,120,232,1,128,1,224,1,128,2,224,129,128,123,48,1,128,10,112,1,128,4,17,1,30,35,0,0,1,4,15,1,29,63,1,0,17,1,75,25, 
  0,0,1,8,4,17,1,216,62,1,0,1,19,112,0,0,0,90,1,0,0,1,0,1,21,1,57,0,0,0,162,96,2,0,4,0,0,0,2,0,0,0,10,80,1,128,1,192,1,128,2,192,129,127,123,16,1,128,4,17, 
  1,130,31,0,0,1,4,15,1,222,35,1,0,17,1,75,25,0,0,1,8,19,112,0,0,0,80,1,0,0,2,0,1,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,56,65,128,132,208,0,128,15, 
  1,29,63,1,0,17,1,70,63,1,0,1,1,2,21,1,51,0,0,0,207,94,2,0,4,0,0,0,2,0,0,0,120,16,1,128,1,80,1,128,2,80,1,128,123,88,1,128,4,17,1,95,35,1,0,1,8,4,17,1, 
  99,28,0,0,1,19,112,0,0,0,78,1,0,0,2,0,14,1,21,1,80,0,0,0,145,97,2,0,6,0,0,0,2,0,0,0,136,216,1,128,1,144,193,128,2,144,1,128,135,80,1,128,25,152,65,128,33,24,2,128, 
  4,17,1,242,20,0,0,1,10,6,17,1,157,36,1,0,1,4,17,1,159,19,0,0,1,4,19,129,0,0,0,113,1,0,0,2,0,1,19,129,0,0,0,120,1,0,0,1,0,1,21,1,47,0,0,0,70,56,2,0, 
  5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,39,56,1,128,8,4,17,1,18,64,1,0,1,2,21,1,52,0,0,0,12,100,2,0,5,0,0,0,2,0,0,0,68,56,1,128, 
  1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,62,0,0,0,170,0,0,0,3,0,1,2,21,1,47,0,0,0,158,100,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129, 
  127,7,48,65,128,39,56,1,128,8,4,17,1,130,64,1,0,1,19,62,0,0,0,169,0,0,0,1,0,1,21,1,52,0,0,0,8,101,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127, 
  7,48,65,128,67,56,1,128,8,4,19,62,0,0,0,168,0,0,0,3,0,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,240,1, 
  128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,4,65,1,0,1,4,15,1,4,65,1,0,17,1,122,3,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,37,48, 
  130,128,13,152,194,127,49,40,130,128,79,48,1,128,61,192,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,4,65,1,0,1,15,1,4,65,1,0,17,1,157,65,1,0,1,1,15,1,4,65,1,0,17,1,106,65, 
  1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,4,65,1,0,1,2,21,1,39,0,0,0,154,101,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1, 
  8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,59,0,0,0,123,102,2,0,6,0,0,0,2,0,0,0,60,152,1,128,1,144,1,129,2,144,129,128,7,144,1,128,6,144,1,128,61,80,1,128,4,17,1,73,66, 
  1,0,1,8,4,17,1,229,65,1,0,1,19,49,0,0,0,139,0,0,0,2,0,14,1,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41, 
  56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,32,66,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,49,208,0,128,37,216,192,127,1,19,49,0,0,0,137,0,0,0, 
  4,0,14,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,132,66, 
  1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,49,208,0,128,37,216,192,127,1,19,49,0,0,0,138,0,0,0,4,0,14,1,2,21,1,104,0,0,0,63,103,2,0,7,0,0,0,2, 
  0,0,0,6,112,65,129,1,112,193,128,2,112,129,127,7,112,193,128,17,168,2,128,18,16,2,128,19,120,1,128,8,4,19,54,0,0,0,150,0,0,0,1,0,17,1,22,67,1,0,1,4,19,54,0,0,0,151,0,0, 
  0,1,0,17,1,22,67,1,0,1,4,19,54,0,0,0,149,0,0,0,1,0,17,1,22,67,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,52,56,65,128,54,208,0,128,15,1,22,67, 
  1,0,17,1,63,67,1,0,1,1,2,21,1,94,0,0,0,63,103,2,0,7,0,0,0,2,0,0,0,6,112,65,129,1,176,194,128,2,112,129,127,7,112,193,128,17,72,2,128,18,224,1,128,19,120,1,128,8,4,19, 
  54,0,0,0,153,0,0,0,2,0,1,4,19,54,0,0,0,154,0,0,0,2,0,1,4,19,54,0,0,0,152,0,0,0,2,0,1,4,17,1,158,67,1,0,1,2,21,1,59,0,0,0,238,103,2,0,6,0,0,0, 
  2,0,0,0,60,152,1,128,1,80,1,129,2,80,129,128,7,80,1,128,6,80,1,128,61,88,1,128,8,4,17,1,62,68,1,0,1,4,17,1,218,67,1,0,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0, 
  2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,21,68,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,52,208,0,128,37,216,0,128,1,19,52,0,0,0,144,0,0,0,5,0,14,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128, 
  41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,121,68,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,52,208,0,128,37,216,0,128,1,19,52,0,0,0,145,0,0, 
  0,5,0,14,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,221, 
  68,1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,56,208,0,128,37,216,0,128,1,19,56,0,0,0,157,0,0,0,2,0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2, 
  0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,53,69,1,0,1,2,21,1,152,0,0,0,139,104,2,0,10,0,0,0,3,0,0,0,112,8,3,128,1,208,193,129,2, 
  208,1,128,11,88,4,128,76,232,3,128,110,120,3,129,6,208,193,127,7,208,1,128,129,112,2,128,134,216,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,171,70,1,0,1,4,19,120,0,0,0,101,1,0, 
  0,1,0,17,1,171,70,1,0,1,4,15,1,171,70,1,0,17,1,214,126,0,0,1,4,15,1,171,70,1,0,17,1,206,69,1,0,1,4,15,1,171,70,1,0,17,1,46,5,0,0,1,4,19,57,0,0,0,159,0, 
  0,0,3,0,1,2,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0,0,0,41,24,1,128,1,16,193,127,2,16,1,128,31,176,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,19,70,1,0,1, 
  4,15,1,19,70,1,0,17,1,122,3,0,0,1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,37,40,130,128,13,144,194,127,61,200,129,128,79,56,1,128,109,48,1,128,1,19,61,0,0,0,167, 
  0,0,0,1,0,17,1,19,70,1,0,1,19,109,0,0,0,56,1,0,0,2,0,1,15,1,19,70,1,0,17,1,120,70,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,19,70,1,0,1,2,21,1,39,0, 
  0,0,15,105,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,0,172,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,120,32,194,129,57,88,5,128,58,240,68,128,74,96,4,128,108,208,3,128,109,64,3,128,110,176,2,128,128,144,1,128,19,108,0,0,0,55,1,0,0,1,0,17,1,171,70,1,0,1,19,108,0,0,0, 
  54,1,0,0,1,0,17,1,171,70,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,171,70,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,171,70,1,0,1,19,74,0,0,0,205,0,0,0,1,0, 
  17,1,171,70,1,0,1,19,58,0,0,0,160,0,0,0,1,0,17,1,171,70,1,0,1,15,1,171,70,1,0,17,1,88,71,1,0,1,1,2,21,1,153,0,0,0,139,104,2,0,10,0,0,0,3,0,0,0,112,8, 
  3,128,1,208,193,129,2,208,1,128,11,88,4,128,76,232,3,128,110,120,3,129,6,208,193,127,7,208,1,128,129,112,2,128,134,216,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,55,72,1,0,1,4,19, 
  120,0,0,0,101,1,0,0,1,0,17,1,55,72,1,0,1,4,15,1,55,72,1,0,17,1,214,126,0,0,1,4,15,1,55,72,1,0,17,1,242,71,1,0,1,4,15,1,55,72,1,0,17,1,40,12,0,0,1,4, 
  19,57,0,0,0,158,0,0,0,4,0,14,1,2,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0,0,0,41,24,1,128,1,16,193,127,2,16,1,128,31,176,1,128,8,4,19,37,0,0,0,101,0,0,0,1, 
  0,17,1,19,70,1,0,1,4,15,1,19,70,1,0,17,1,116,10,0,0,1,2,21,0,149,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,108,176,67,129,109,32,3,128,58,160,68,128,74,64,68,128,110,144,2, 
  128,120,0,66,128,128,112,1,128,19,108,0,0,0,55,1,0,0,1,0,17,1,55,72,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,55,72,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,55,72, 
  1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,55,72,1,0,1,19,74,0,0,0,205,0,0,0,1,0,17,1,55,72,1,0,1,19,58,0,0,0,161,0,0,0,2,0,1,1,2,21,0,60,1,0,0,255, 
  255,255,255,15,0,0,0,3,0,0,0,0,80,73,131,49,32,68,131,26,128,7,128,27,240,6,128,4,72,73,129,5,224,200,128,30,64,5,128,7,120,8,128,13,16,136,128,28,96,198,128,29,208,69,128,37,176,4,128,52, 
  144,3,128,56,0,3,128,57,112,2,128,19,26,0,0,0,78,0,0,0,1,0,17,1,205,72,1,0,1,19,26,0,0,0,77,0,0,0,1,0,17,1,205,72,1,0,1,19,26,0,0,0,76,0,0,0,1,0,17,1, 
  205,72,1,0,1,19,26,0,0,0,75,0,0,0,1,0,17,1,205,72,1,0,1,19,13,0,0,0,34,0,0,0,1,0,17,1,205,72,1,0,1,19,7,0,0,0,20,0,0,0,1,0,17,1,205,72,1,0,1,19, 
  7,0,0,0,18,0,0,0,1,0,17,1,205,72,1,0,1,19,7,0,0,0,17,0,0,0,1,0,17,1,205,72,1,0,1,19,7,0,0,0,16,0,0,0,1,0,17,1,205,72,1,0,1,19,5,0,0,0,9,0, 
  0,0,1,0,17,1,205,72,1,0,1,15,1,205,72,1,0,17,1,97,89,1,0,1,15,1,205,72,1,0,17,1,211,88,1,0,1,15,1,205,72,1,0,17,1,10,74,1,0,1,1,19,7,0,0,0,19,0,0,0, 
  1,0,17,1,205,72,1,0,1,2,21,1,202,0,0,0,10,71,2,0,13,0,0,0,3,0,0,0,24,112,197,130,1,48,2,129,2,48,2,128,27,144,4,128,22,224,69,129,25,0,197,128,6,48,130,127,7,48,194,128, 
  41,248,3,129,62,136,3,128,63,24,3,128,64,168,2,128,65,56,2,128,8,4,15,1,153,76,1,0,17,1,208,75,1,0,1,4,15,1,153,76,1,0,17,1,162,68,1,0,1,4,15,1,153,76,1,0,17,1,173,66, 
  1,0,1,4,15,1,153,76,1,0,17,1,213,74,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,153,76,1,0,1,4,15,1,153,76,1,0,17,1,63,54,1,0,1,4,15,1,153,76,1,0,17,1,66, 
  45,1,0,1,4,15,1,153,76,1,0,17,1,34,37,1,0,1,4,15,1,153,76,1,0,17,1,223,12,1,0,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2, 
  80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,34,75,1,0,1,4,15,1,34,75,1,0,17,1,116,10,0,0,1,2,21,0,101,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,37,48,130,128,13,152,194,127,49,40,130,128,79,48,1,128,61,192,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,34,75,1,0,1,15,1,34,75,1,0,17,1,136,75,1,0,1,1, 
  15,1,34,75,1,0,17,1,106,65,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,34,75,1,0,1,2,21,1,59,0,0,0,123,102,2,0,6,0,0,0,2,0,0,0,60,80,1,128,1,144,1,129,2,144, 
  129,128,7,144,1,128,6,144,1,128,61,152,1,128,4,17,1,229,65,1,0,1,8,4,17,1,73,66,1,0,1,19,49,0,0,0,139,0,0,0,2,0,14,1,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0, 
  0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,0,76,1,0,1,2,21,1,152,0,0,0,139,104,2,0,10,0,0,0,3,0,0,0,112,8,3,128,1,208,193,129,2,208, 
  1,128,11,88,4,128,76,232,3,128,110,120,3,129,6,208,193,127,7,208,1,128,129,112,2,128,134,216,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,171,70,1,0,1,4,19,120,0,0,0,101,1,0,0, 
  1,0,17,1,171,70,1,0,1,4,15,1,171,70,1,0,17,1,214,126,0,0,1,4,15,1,171,70,1,0,17,1,242,71,1,0,1,4,15,1,171,70,1,0,17,1,40,12,0,0,1,4,19,57,0,0,0,159,0,0, 
  0,3,0,1,2,21,0,41,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,0,80,72,131,49,144,68,131,26,32,5,128,27,16,6,128,4,136,68,129,5,136,196,128,30,48,7,128,7,144,3,128,13,224,136,128,28, 
  192,199,128,29,0,67,128,37,248,3,128,52,128,5,128,56,112,2,128,57,160,6,128,19,26,0,0,0,77,0,0,0,1,0,17,1,153,76,1,0,1,19,7,0,0,0,18,0,0,0,1,0,17,1,153,76,1,0,1,15, 
  1,153,76,1,0,17,1,195,77,1,0,1,19,13,0,0,0,34,0,0,0,1,0,17,1,153,76,1,0,1,1,19,26,0,0,0,75,0,0,0,1,0,17,1,153,76,1,0,1,19,5,0,0,0,10,0,0,0,2,0, 
  1,19,26,0,0,0,76,0,0,0,1,0,17,1,153,76,1,0,1,19,7,0,0,0,16,0,0,0,1,0,17,1,153,76,1,0,1,19,26,0,0,0,78,0,0,0,1,0,17,1,153,76,1,0,1,19,7,0,0,0, 
  20,0,0,0,1,0,17,1,153,76,1,0,1,19,7,0,0,0,17,0,0,0,1,0,17,1,153,76,1,0,1,19,7,0,0,0,19,0,0,0,1,0,17,1,153,76,1,0,1,15,1,153,76,1,0,17,1,97,89,1, 
  0,1,2,21,1,130,0,0,0,20,106,2,0,9,0,0,0,3,0,0,0,24,48,3,128,1,176,1,129,2,176,1,128,27,80,2,128,22,160,3,128,25,192,194,128,6,176,129,127,7,176,1,128,41,184,1,128,8,4,19, 
  37,0,0,0,101,0,0,0,1,0,17,1,68,88,1,0,1,4,15,1,68,88,1,0,17,1,214,85,1,0,1,4,15,1,68,88,1,0,17,1,233,83,1,0,1,4,15,1,68,88,1,0,17,1,101,82,1,0,1,4, 
  15,1,68,88,1,0,17,1,82,78,1,0,1,19,4,0,0,0,7,0,0,0,2,0,14,1,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128, 
  41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,141,78,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,27,128,129,128,13,136,193,127,37,240,0,128,19,13,0,0,0, 
  34,0,0,0,1,0,17,1,141,78,1,0,1,1,15,1,141,78,1,0,17,1,204,78,1,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48, 
  1,128,21,56,1,128,8,4,17,1,252,78,1,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48, 
  194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,207,17,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1, 
  207,17,1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,207,17,1,0,1,4,15,1,207,17,1,0,17,1,214,126,0,0,1,4,15,1,207,17,1,0,17,1,242,71,1,0,1,4,15,1,207,17,1,0,17, 
  1,214,79,1,0,1,4,15,1,207,17,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,207,17,1,0,1,4,15,1,207,17,1,0,17,1,116,10,0,0,1,2,21,1,176,0,0,0, 
  67,249,1,0,11,0,0,0,3,0,0,0,112,40,3,128,1,240,1,129,2,240,1,128,31,16,5,128,76,8,4,128,41,120,4,129,6,240,129,128,7,240,1,127,110,152,131,128,129,144,2,128,134,248,1,128,8,4,19,128, 
  0,0,0,111,1,0,0,1,0,17,1,135,80,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,135,80,1,0,1,4,15,1,135,80,1,0,17,1,214,126,0,0,1,4,15,1,135,80,1,0,17,1,229,36, 
  0,0,1,4,15,1,135,80,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,135,80,1,0,1,4,15,1,135,80,1,0,17,1,116,10,0,0,1,2,21,0,89,1,0,0,255,255,255, 
  255,17,0,0,0,4,0,0,0,112,8,6,129,113,0,70,129,61,104,73,128,109,40,7,128,128,224,4,128,37,208,9,128,129,120,4,128,135,16,4,128,120,112,69,128,136,168,3,128,74,216,136,129,139,176,2,128,108,184,7, 
  128,13,56,74,125,110,152,6,128,79,72,8,128,138,64,3,128,19,138,0,0,0,141,1,0,0,1,0,17,1,135,80,1,0,1,15,1,135,80,1,0,17,1,132,36,0,0,1,15,1,135,80,1,0,17,1,121,32,0,0, 
  1,15,1,135,80,1,0,17,1,225,81,1,0,1,15,1,135,80,1,0,17,1,250,24,0,0,1,19,108,0,0,0,55,1,0,0,1,0,17,1,135,80,1,0,1,19,108,0,0,0,54,1,0,0,1,0,17,1,135,80, 
  1,0,1,1,19,136,0,0,0,135,1,0,0,1,0,17,1,135,80,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,135,80,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,135,80,1,0,1,19,74, 
  0,0,0,205,0,0,0,1,0,17,1,135,80,1,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,135,80,1,0,1,19,138,0,0,0,142,1,0,0,1,0,17,1,135,80,1,0,1,15,1,135,80,1,0,17,1, 
  26,16,0,0,1,15,1,135,80,1,0,17,1,231,15,0,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,135,80,1,0,1,2,21,1,65,0,0,0,46,4,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1, 
  80,1,129,2,80,129,127,7,80,65,128,83,200,1,128,125,88,1,128,8,4,19,113,0,0,0,93,1,0,0,3,0,14,1,4,17,1,35,82,1,0,1,2,21,1,53,0,0,0,156,106,2,0,5,0,0,0,2,0,0, 
  0,124,48,1,128,1,160,1,128,2,160,129,128,7,160,1,128,6,160,1,128,4,19,113,0,0,0,91,1,0,0,4,0,14,1,8,19,113,0,0,0,92,1,0,0,3,0,14,1,21,1,58,0,0,0,85,227,1,0,5, 
  0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,160,82,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,28,128,1,128,13,136,65,128,37,240,0,128,19,13,0,0,0,34,0,0,0,1,0,17,1,160,82,1,0,1,1,15,1,160,82,1,0,17,1,223,82,1,0,1,2,21,1,47,0,0,0,160,78,2, 
  0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,15,83,1,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4, 
  128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0, 
  1,0,17,1,166,38,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,166,38,1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,166,38,1,0,1,4,15,1,166,38,1,0,17,1,214,126,0,0, 
  1,4,15,1,166,38,1,0,17,1,242,71,1,0,1,4,15,1,166,38,1,0,17,1,214,79,1,0,1,4,15,1,166,38,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,166,38,1, 
  0,1,4,15,1,166,38,1,0,17,1,116,10,0,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,25, 
  84,1,0,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0, 
  17,1,102,84,1,0,1,4,15,1,102,84,1,0,17,1,116,10,0,0,1,2,21,0,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,29,144,130,128,13,152,194,127,37,40,130,128,79,48,1,128,61,192,1,128, 
  19,61,0,0,0,167,0,0,0,1,0,17,1,102,84,1,0,1,15,1,102,84,1,0,17,1,204,84,1,0,1,15,1,102,84,1,0,17,1,37,46,1,0,1,1,19,61,0,0,0,166,0,0,0,1,0,17,1,102,84, 
  1,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,21,56,1,128,8,4,17,1,252,84,1,0,1,2,21,1,217,0,0,0,61,240, 
  1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128,7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56, 
  2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,98,47,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,98,47,1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,98,47,1,0,1, 
  4,15,1,98,47,1,0,17,1,214,126,0,0,1,4,15,1,98,47,1,0,17,1,242,71,1,0,1,4,15,1,98,47,1,0,17,1,214,79,1,0,1,4,15,1,98,47,1,0,17,1,40,12,0,0,1,4,19,37,0, 
  0,0,101,0,0,0,1,0,17,1,98,47,1,0,1,4,15,1,98,47,1,0,17,1,116,10,0,0,1,2,21,1,76,0,0,0,235,98,2,0,6,0,0,0,2,0,0,0,40,240,1,128,1,80,1,129,2,80,129,128, 
  7,80,1,128,6,80,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,35,86,1,0,1,4,15,1,35,86,1,0,17,1,8,17,0,0,1,2,21,0,96,0,0,0,255,255,255,255,5,0,0, 
  0,2,0,0,0,36,40,2,128,37,192,1,128,30,248,130,128,31,144,2,128,62,48,1,128,19,31,0,0,0,83,0,0,0,1,0,17,1,35,86,1,0,1,15,1,35,86,1,0,17,1,71,64,1,0,1,15,1,35,86, 
  1,0,17,1,226,63,1,0,1,15,1,35,86,1,0,17,1,132,86,1,0,1,1,2,21,1,59,0,0,0,173,99,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,193,128,2,80,129,127,7,80,1,128,9,152, 
  65,128,21,88,1,128,8,4,17,1,192,86,1,0,1,4,17,1,41,55,1,0,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56, 
  1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,251,86,1,0,1,2,21,0,62,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,30,128,1,128,13,136,65,128,37,240,0,128,19,13,0,0,0,34,0, 
  0,0,1,0,17,1,251,86,1,0,1,1,15,1,251,86,1,0,17,1,58,87,1,0,1,2,21,1,47,0,0,0,160,78,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128, 
  21,56,1,128,8,4,17,1,106,87,1,0,1,2,21,1,217,0,0,0,61,240,1,0,13,0,0,0,3,0,0,0,112,0,4,128,1,48,2,129,2,48,130,129,31,88,198,129,76,80,5,128,41,192,133,129,6,48,194,128, 
  7,48,2,127,82,224,4,128,110,112,196,128,127,104,3,128,129,208,2,128,134,56,2,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,18,57,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,18,57, 
  1,0,1,4,19,114,0,0,0,94,1,0,0,1,0,17,1,18,57,1,0,1,4,15,1,18,57,1,0,17,1,214,126,0,0,1,4,15,1,18,57,1,0,17,1,242,71,1,0,1,4,15,1,18,57,1,0,17,1,214, 
  79,1,0,1,4,15,1,18,57,1,0,17,1,40,12,0,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,18,57,1,0,1,4,15,1,18,57,1,0,17,1,116,10,0,0,1,2,21,0,142,0,0,0,255,255, 
  255,255,8,0,0,0,3,0,0,0,0,16,4,128,29,128,66,128,37,144,1,128,27,64,3,128,28,224,2,128,13,160,3,127,30,32,2,128,7,8,4,128,19,13,0,0,0,34,0,0,0,1,0,17,1,68,88,1,0,1, 
  19,7,0,0,0,25,0,0,0,2,0,1,19,7,0,0,0,23,0,0,0,2,0,1,19,7,0,0,0,22,0,0,0,2,0,1,19,7,0,0,0,21,0,0,0,2,0,1,15,1,68,88,1,0,17,1,97,89,1,0, 
  1,1,19,7,0,0,0,24,0,0,0,2,0,1,2,21,1,130,0,0,0,20,106,2,0,9,0,0,0,3,0,0,0,24,48,3,128,1,176,1,129,2,176,1,128,27,80,2,128,22,160,3,128,25,192,194,128,6,176,129, 
  127,7,176,1,128,41,184,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,68,88,1,0,1,4,15,1,68,88,1,0,17,1,214,85,1,0,1,4,15,1,68,88,1,0,17,1,233,83,1,0,1,4,15,1, 
  68,88,1,0,17,1,101,82,1,0,1,4,15,1,68,88,1,0,17,1,82,78,1,0,1,19,4,0,0,0,8,0,0,0,1,0,1,21,1,59,0,0,0,153,229,1,0,6,0,0,0,2,0,0,0,4,88,1,128,1, 
  80,1,128,2,80,129,128,3,152,129,128,6,80,1,128,7,80,1,128,8,4,17,1,115,212,1,0,1,4,17,1,157,89,1,0,1,2,21,1,69,1,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,176,8,128,1, 
  240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7,128,6,240,2,128,7,240,66,127,74,80,5,128,73,232,5,128,58,240,134,127,59,128,70,129,108,104,3,128,109,248,2,128,46,184,9,128,47,72,9,128,75, 
  184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,195,202,1,0,17,1,167,200,1,0,1,4,15,1,195,202,1,0,17,1,194,199,1,0,1,4,15,1,195,202,1,0,17,1,221,198,1,0,1,4,15,1,195,202,1, 
  0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,195,202,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,195,202,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1, 
  195,202,1,0,1,4,15,1,195,202,1,0,17,1,72,230,0,0,1,4,15,1,195,202,1,0,17,1,2,230,0,0,1,4,15,1,195,202,1,0,17,1,106,229,0,0,1,4,15,1,195,202,1,0,17,1,133,111,1,0, 
  1,4,15,1,195,202,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,195,202,1,0,1,4,15,1,195,202,1,0,17,1,191,110,1,0,1,4,15,1,195,202,1,0,17,1,227,90,1, 
  0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,30,91,1,0, 
  1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,56,1,128,69,208,0,128,15,1,30,91,1,0,17,1,71,91,1,0,1,1,2,21,1,111,0,0,0,103,237,1,0,8,0,0,0,3,0,0, 
  0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41,112,2,128,72,152,1,128,6,144,1,128,7,144,1,127,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,133,101,1,0,1,4,17,1,183,91,1,0, 
  1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,133,101,1,0,1,4,15,1,133,101,1,0,17,1,116,10,0,0,1,2,21,1,47,0,0,0,56,48,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1, 
  128,2,48,129,127,7,48,1,128,42,56,1,128,8,4,17,1,231,91,1,0,1,2,21,1,99,0,0,0,171,48,2,0,7,0,0,0,2,0,0,0,72,120,1,128,1,112,65,129,2,112,129,128,7,112,129,128,6,112,1, 
  128,31,168,2,128,41,16,2,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,75,92,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,75,92,1,0,1,4,15,1,75,92,1,0,17,1,116,10,0, 
  0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,75,92, 
  1,0,1,15,1,75,92,1,0,17,1,127,100,1,0,1,15,1,75,92,1,0,17,1,245,92,1,0,1,1,15,1,75,92,1,0,17,1,194,92,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,75,92,1,0, 
  1,2,21,1,39,0,0,0,139,107,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,47,0,0,0,224, 
  107,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1,50,93,1,0,1,8,19,38,0,0,0,108,0,0,0,5,0,14,14,1,21,1,11,2,0,0, 
  17,15,2,0,30,0,0,0,4,0,0,0,96,232,8,135,1,80,4,132,2,80,132,131,99,152,7,128,84,184,77,133,101,144,6,128,6,80,68,131,7,80,68,131,88,104,12,128,89,248,11,128,10,232,207,130,91,24,11,128, 
  92,168,10,128,93,56,10,128,94,200,9,128,31,120,207,129,34,8,143,128,81,152,142,129,82,40,142,129,86,72,205,129,87,216,12,128,90,136,11,128,95,88,73,129,97,120,136,129,98,8,8,128,100,40,7,128,102,248,5,128, 
  127,136,5,128,128,240,4,128,129,88,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,148,98,1,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,148,98,1,0,1,4,15,1,148,98,1,0,17,1, 
  35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,148,98,1,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,148,98,1,0,1,4,15,1,148,98,1,0,17,1,176,117,0,0,1,4,15,1, 
  148,98,1,0,17,1,61,117,0,0,1,4,15,1,148,98,1,0,17,1,202,116,0,0,1,4,15,1,148,98,1,0,17,1,87,116,0,0,1,4,15,1,148,98,1,0,17,1,228,115,0,0,1,4,15,1,148,98,1,0, 
  17,1,113,115,0,0,1,4,15,1,148,98,1,0,17,1,254,114,0,0,1,4,15,1,148,98,1,0,17,1,139,114,0,0,1,4,15,1,148,98,1,0,17,1,24,114,0,0,1,4,15,1,148,98,1,0,17,1,165,113, 
  0,0,1,4,15,1,148,98,1,0,17,1,50,113,0,0,1,4,15,1,148,98,1,0,17,1,191,112,0,0,1,4,15,1,148,98,1,0,17,1,76,112,0,0,1,4,15,1,148,98,1,0,17,1,217,111,0,0,1,4, 
  15,1,148,98,1,0,17,1,102,111,0,0,1,4,15,1,148,98,1,0,17,1,53,109,0,0,1,4,15,1,148,98,1,0,17,1,138,99,0,0,1,4,15,1,148,98,1,0,17,1,19,92,0,0,1,4,15,1,148,98, 
  1,0,17,1,9,87,0,0,1,4,15,1,148,98,1,0,17,1,118,81,0,0,1,4,15,1,148,98,1,0,17,1,62,95,1,0,1,2,21,1,3,2,0,0,83,22,2,0,28,0,0,0,4,0,0,0,96,168,136,134, 
  1,16,4,132,2,16,132,131,99,88,7,128,84,120,205,132,101,80,6,128,86,8,141,132,87,152,12,128,40,200,206,130,89,184,11,128,90,72,11,128,91,216,10,128,92,104,10,128,93,248,9,128,94,136,9,128,31,168,79,129, 
  34,56,143,128,81,88,14,129,82,232,13,129,88,40,12,128,95,24,73,129,97,56,136,129,98,200,7,128,100,232,6,128,102,184,5,128,127,72,5,128,128,176,4,128,129,24,4,128,8,4,19,120,0,0,0,101,1,0,0,1, 
  0,17,1,66,97,1,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,66,97,1,0,1,4,15,1,66,97,1,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,66,97,1,0,1, 
  4,19,87,0,0,0,3,1,0,0,1,0,17,1,66,97,1,0,1,4,15,1,66,97,1,0,17,1,176,117,0,0,1,4,15,1,66,97,1,0,17,1,61,117,0,0,1,4,15,1,66,97,1,0,17,1,202,116,0,0, 
  1,4,15,1,66,97,1,0,17,1,87,116,0,0,1,4,15,1,66,97,1,0,17,1,228,115,0,0,1,4,15,1,66,97,1,0,17,1,113,115,0,0,1,4,15,1,66,97,1,0,17,1,254,114,0,0,1,4,15,1, 
  66,97,1,0,17,1,139,114,0,0,1,4,15,1,66,97,1,0,17,1,24,114,0,0,1,4,15,1,66,97,1,0,17,1,165,113,0,0,1,4,15,1,66,97,1,0,17,1,50,113,0,0,1,4,15,1,66,97,1,0, 
  17,1,191,112,0,0,1,4,15,1,66,97,1,0,17,1,76,112,0,0,1,4,15,1,66,97,1,0,17,1,217,111,0,0,1,4,15,1,66,97,1,0,17,1,102,111,0,0,1,4,15,1,66,97,1,0,17,1,53,109, 
  0,0,1,4,15,1,66,97,1,0,17,1,138,99,0,0,1,4,15,1,66,97,1,0,17,1,19,92,0,0,1,4,15,1,66,97,1,0,17,1,8,17,0,0,1,4,15,1,66,97,1,0,17,1,9,87,0,0,1,4, 
  15,1,66,97,1,0,17,1,118,81,0,0,1,2,21,0,81,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,64,0,201,130,81,208,3,128,34,88,134,130,68,96,132,130,36,248,196,127,85,176,130,130,86,64,3,128, 
  87,112,72,130,120,104,4,128,9,32,10,128,10,96,4,128,80,224,7,128,82,144,9,128,84,200,69,128,116,80,7,128,117,96,5,128,119,192,6,128,19,64,0,0,0,176,0,0,0,1,0,17,1,66,97,1,0,1,19,64, 
  0,0,0,177,0,0,0,1,0,17,1,66,97,1,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,66,97,1,0,1,1,19,87,0,0,0,6,1,0,0,1,0,17,1,66,97,1,0,1,15,1,66,97,1,0,17, 
  1,217,49,0,0,1,15,1,66,97,1,0,17,1,192,80,0,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,66,97,1,0,1,15,1,66,97,1,0,17,1,63,80,0,0,1,19,87,0,0,0,5,1,0,0,1, 
  0,17,1,66,97,1,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,66,97,1,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,66,97,1,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,66,97,1, 
  0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,66,97,1,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,66,97,1,0,1,15,1,66,97,1,0,17,1,153,49,0,0,1,2,21,0,105,1,0,0,255,255, 
  255,255,17,0,0,0,4,0,0,0,64,40,201,128,81,152,7,128,34,192,73,130,80,40,8,128,68,184,8,130,85,232,133,130,38,184,201,129,87,200,68,130,120,176,2,128,9,184,10,128,10,40,10,128,82,8,7,128,84,120, 
  134,128,86,88,5,128,116,56,4,128,117,208,3,128,119,64,3,128,19,87,0,0,0,6,1,0,0,1,0,17,1,148,98,1,0,1,19,87,0,0,0,5,1,0,0,1,0,17,1,148,98,1,0,1,15,1,148,98,1,0, 
  17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,148,98,1,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,148,98,1,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,148,98,1,0, 
  1,19,64,0,0,0,176,0,0,0,1,0,17,1,148,98,1,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,148,98,1,0,1,19,64,0,0,0,174,0,0,0,1,0,17,1,148,98,1,0,1,19,64,0,0,0, 
  173,0,0,0,1,0,17,1,148,98,1,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,148,98,1,0,1,19,38,0,0,0,106,0,0,0,7,0,14,14,1,19,34,0,0,0,95,0,0,0,1,0,17,1,148,98, 
  1,0,1,1,15,1,148,98,1,0,17,1,254,99,1,0,1,19,68,0,0,0,189,0,0,0,1,0,17,1,148,98,1,0,1,19,68,0,0,0,190,0,0,0,1,0,17,1,148,98,1,0,1,2,21,7,90,0,0,0, 
  255,255,255,255,6,0,0,0,2,0,0,0,45,80,1,128,37,208,193,127,42,16,194,128,43,144,65,128,47,80,2,128,94,144,2,128,4,17,1,188,62,0,0,1,4,17,1,33,59,0,0,1,4,17,1,93,73,0,0,1, 
  4,17,1,87,66,0,0,1,4,17,1,218,69,0,0,1,4,17,1,224,76,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,28,0,0,0, 
  1,0,1,21,1,70,0,0,0,39,49,2,0,6,0,0,0,2,0,0,0,44,240,65,129,1,80,1,128,2,80,129,128,7,80,1,128,6,80,1,128,72,88,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1, 
  91,101,1,0,1,4,17,1,198,100,1,0,1,2,21,1,47,0,0,0,228,49,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,43,56,1,128,8,4,17,1,246,100,1,0, 
  1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,49,101,1,0,1, 
  2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,64,1,128,69,208,0,128,19,38,0,0,0,102,0,0,0,8,0,14,14,1,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,38,64,1,128,69,208,0,128,19,38,0,0,0,104,0,0,0,6,0,14,14,1,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61, 
  72,66,128,69,224,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,133,101,1,0,1,15,1,133,101,1,0,17,1,185,109,1,0,1,15,1,133,101,1,0,17,1,47,102,1,0,1,1,15,1,133,101,1,0,17,1, 
  252,101,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,133,101,1,0,1,2,21,1,39,0,0,0,139,107,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8, 
  0,0,1,19,13,0,0,0,34,0,0,0,1,0,1,21,1,47,0,0,0,224,107,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1,108,102,1,0, 
  1,8,19,38,0,0,0,109,0,0,0,3,0,14,14,1,21,1,11,2,0,0,17,15,2,0,30,0,0,0,4,0,0,0,96,232,8,135,1,80,4,132,2,80,132,131,99,152,7,128,84,184,77,133,101,144,6,128,6,80, 
  68,131,7,80,68,131,88,104,12,128,89,248,11,128,10,232,207,130,91,24,11,128,92,168,10,128,93,56,10,128,94,200,9,128,31,120,207,129,34,8,143,128,81,152,142,129,82,40,142,129,86,72,205,129,87,216,12,128,90,136, 
  11,128,95,88,73,129,97,120,136,129,98,8,8,128,100,40,7,128,102,248,5,128,127,136,5,128,128,240,4,128,129,88,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,206,107,1,0,1,4,19,119,0,0,0, 
  100,1,0,0,1,0,17,1,206,107,1,0,1,4,15,1,206,107,1,0,17,1,35,118,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,206,107,1,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1, 
  206,107,1,0,1,4,15,1,206,107,1,0,17,1,176,117,0,0,1,4,15,1,206,107,1,0,17,1,61,117,0,0,1,4,15,1,206,107,1,0,17,1,202,116,0,0,1,4,15,1,206,107,1,0,17,1,87,116,0,0, 
  1,4,15,1,206,107,1,0,17,1,228,115,0,0,1,4,15,1,206,107,1,0,17,1,113,115,0,0,1,4,15,1,206,107,1,0,17,1,254,114,0,0,1,4,15,1,206,107,1,0,17,1,139,114,0,0,1,4,15,1, 
  206,107,1,0,17,1,24,114,0,0,1,4,15,1,206,107,1,0,17,1,165,113,0,0,1,4,15,1,206,107,1,0,17,1,50,113,0,0,1,4,15,1,206,107,1,0,17,1,191,112,0,0,1,4,15,1,206,107,1,0, 
  17,1,76,112,0,0,1,4,15,1,206,107,1,0,17,1,217,111,0,0,1,4,15,1,206,107,1,0,17,1,102,111,0,0,1,4,15,1,206,107,1,0,17,1,53,109,0,0,1,4,15,1,206,107,1,0,17,1,138,99, 
  0,0,1,4,15,1,206,107,1,0,17,1,19,92,0,0,1,4,15,1,206,107,1,0,17,1,9,87,0,0,1,4,15,1,206,107,1,0,17,1,118,81,0,0,1,4,15,1,206,107,1,0,17,1,120,104,1,0,1,2, 
  21,1,3,2,0,0,83,22,2,0,28,0,0,0,4,0,0,0,96,168,136,134,1,16,4,132,2,16,132,131,99,88,7,128,84,120,205,132,101,80,6,128,86,8,141,132,87,152,12,128,40,200,206,130,89,184,11,128,90,72, 
  11,128,91,216,10,128,92,104,10,128,93,248,9,128,94,136,9,128,31,168,79,129,34,56,143,128,81,88,14,129,82,232,13,129,88,40,12,128,95,24,73,129,97,56,136,129,98,200,7,128,100,232,6,128,102,184,5,128,127,72, 
  5,128,128,176,4,128,129,24,4,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,124,106,1,0,1,4,19,119,0,0,0,100,1,0,0,1,0,17,1,124,106,1,0,1,4,15,1,124,106,1,0,17,1,35,118, 
  0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,17,1,124,106,1,0,1,4,19,87,0,0,0,3,1,0,0,1,0,17,1,124,106,1,0,1,4,15,1,124,106,1,0,17,1,176,117,0,0,1,4,15,1,124,106, 
  1,0,17,1,61,117,0,0,1,4,15,1,124,106,1,0,17,1,202,116,0,0,1,4,15,1,124,106,1,0,17,1,87,116,0,0,1,4,15,1,124,106,1,0,17,1,228,115,0,0,1,4,15,1,124,106,1,0,17,1, 
  113,115,0,0,1,4,15,1,124,106,1,0,17,1,254,114,0,0,1,4,15,1,124,106,1,0,17,1,139,114,0,0,1,4,15,1,124,106,1,0,17,1,24,114,0,0,1,4,15,1,124,106,1,0,17,1,165,113,0,0, 
  1,4,15,1,124,106,1,0,17,1,50,113,0,0,1,4,15,1,124,106,1,0,17,1,191,112,0,0,1,4,15,1,124,106,1,0,17,1,76,112,0,0,1,4,15,1,124,106,1,0,17,1,217,111,0,0,1,4,15,1, 
  124,106,1,0,17,1,102,111,0,0,1,4,15,1,124,106,1,0,17,1,53,109,0,0,1,4,15,1,124,106,1,0,17,1,138,99,0,0,1,4,15,1,124,106,1,0,17,1,19,92,0,0,1,4,15,1,124,106,1,0, 
  17,1,8,17,0,0,1,4,15,1,124,106,1,0,17,1,9,87,0,0,1,4,15,1,124,106,1,0,17,1,118,81,0,0,1,2,21,0,81,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,64,248,196,130,81,208, 
  3,128,34,144,137,130,68,96,132,130,36,40,201,127,85,64,131,130,86,128,6,128,87,152,72,130,120,104,4,128,9,24,6,128,10,96,4,128,80,120,7,128,82,248,9,128,84,8,72,128,116,136,5,128,117,16,7,128,119,176, 
  2,128,19,87,0,0,0,5,1,0,0,1,0,17,1,124,106,1,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,124,106,1,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,124,106,1,0,1,1,19,87,0, 
  0,0,6,1,0,0,1,0,17,1,124,106,1,0,1,19,34,0,0,0,95,0,0,0,1,0,17,1,124,106,1,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,124,106,1,0,1,15,1,124,106,1,0,17,1,153, 
  49,0,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,124,106,1,0,1,15,1,124,106,1,0,17,1,192,80,0,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,124,106,1,0,1,19,64,0,0,0,175,0, 
  0,0,1,0,17,1,124,106,1,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1,124,106,1,0,1,15,1,124,106,1,0,17,1,217,49,0,0,1,15,1,124,106,1,0,17,1,63,80,0,0,1,19,64,0,0,0, 
  174,0,0,0,1,0,17,1,124,106,1,0,1,2,21,0,105,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,64,40,201,128,81,152,7,128,34,192,73,130,80,40,8,128,68,184,8,130,85,232,133,130,38,184,201,129, 
  87,200,68,130,120,176,2,128,9,184,10,128,10,40,10,128,82,8,7,128,84,120,134,128,86,88,5,128,116,56,4,128,117,208,3,128,119,64,3,128,19,87,0,0,0,6,1,0,0,1,0,17,1,206,107,1,0,1,19,87, 
  0,0,0,5,1,0,0,1,0,17,1,206,107,1,0,1,15,1,206,107,1,0,17,1,192,80,0,0,1,19,80,0,0,0,220,0,0,0,1,0,17,1,206,107,1,0,1,19,64,0,0,0,178,0,0,0,1,0,17,1, 
  206,107,1,0,1,19,64,0,0,0,177,0,0,0,1,0,17,1,206,107,1,0,1,19,64,0,0,0,176,0,0,0,1,0,17,1,206,107,1,0,1,19,64,0,0,0,175,0,0,0,1,0,17,1,206,107,1,0,1,19, 
  64,0,0,0,174,0,0,0,1,0,17,1,206,107,1,0,1,19,64,0,0,0,173,0,0,0,1,0,17,1,206,107,1,0,1,19,64,0,0,0,172,0,0,0,1,0,17,1,206,107,1,0,1,19,38,0,0,0,107,0, 
  0,0,5,0,14,14,1,19,34,0,0,0,95,0,0,0,1,0,17,1,206,107,1,0,1,1,15,1,206,107,1,0,17,1,56,109,1,0,1,19,68,0,0,0,189,0,0,0,1,0,17,1,206,107,1,0,1,19,68,0, 
  0,0,190,0,0,0,1,0,17,1,206,107,1,0,1,2,21,7,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,45,80,1,128,37,144,194,127,42,208,193,128,43,144,65,128,47,16,2,128,94,80,2,128,4,17, 
  1,188,62,0,0,1,4,17,1,33,59,0,0,1,4,17,1,87,66,0,0,1,4,17,1,218,69,0,0,1,4,17,1,224,76,0,0,1,4,17,1,93,73,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,9,0,0,0,28,0,0,0,1,0,1,21,1,70,0,0,0,39,49,2,0,6,0,0,0,2,0,0,0,44,240,65,129,1,80,1,128,2,80,129,128,7,80,1,128,6, 
  80,1,128,72,88,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,149,110,1,0,1,4,17,1,0,110,1,0,1,2,21,1,47,0,0,0,228,49,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  1,128,2,48,129,127,7,48,65,128,43,56,1,128,8,4,17,1,48,110,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48, 
  1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,107,110,1,0,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,64,1,128,69,208,0,128,19,38,0,0,0,103,0,0,0,6,0, 
  14,14,1,1,2,21,0,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,64,1,128,69,208,0,128,19,38,0,0,0,105,0,0,0,4,0,14,14,1,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0, 
  0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,250,110,1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,38,48,1,128,69,208,0,128,19,38,0,0,0,110,0,0,0,2,0,1,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128, 
  6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,93,111,1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,48,1,128,69,208,0,128,19,38,0,0,0,111,0,0,0, 
  2,0,1,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,192,111, 
  1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,69,208,0,128,43,48,193,127,19,43,0,0,0,117,0,0,0,2,0,1,1,2,21,1,47,0,0,0,70,56,2,0,5,0,0,0,2,0, 
  0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,39,56,1,128,8,4,17,1,24,112,1,0,1,2,21,1,82,0,0,0,154,56,2,0,7,0,0,0,2,0,0,0,6,112,129,129,1,112,193,128,2,112, 
  129,127,7,112,1,128,41,248,65,128,105,184,1,128,106,120,1,128,8,4,17,1,63,184,1,0,1,4,17,1,196,169,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,107,112,1,0,1,2,21,0,40,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,90,208,0,128,37,216,0,128,1,15,1,107,112,1,0,17,1,148,112,1,0,1,2,21,1,89,0,0,0,11,63,2,0,7,0,0,0,2,0,0,0,6,112,1,129,1,112, 
  1,128,2,112,129,127,7,112,129,128,10,88,2,128,31,232,65,128,39,120,1,128,8,4,15,1,236,168,1,0,17,1,137,168,1,0,1,4,15,1,236,168,1,0,17,1,51,155,1,0,1,4,15,1,236,168,1,0,17,1, 
  238,112,1,0,1,2,21,1,107,0,0,0,117,63,2,0,8,0,0,0,3,0,0,0,10,232,66,129,1,144,1,128,2,144,129,127,131,152,1,128,31,120,2,128,130,8,2,128,6,144,1,128,7,144,65,127,8,4,15,1, 
  2,154,1,0,17,1,56,153,1,0,1,4,15,1,2,154,1,0,17,1,251,139,1,0,1,4,15,1,2,154,1,0,17,1,51,155,1,0,1,4,15,1,2,154,1,0,17,1,90,113,1,0,1,2,21,1,69,1,0,0, 
  54,230,1,0,19,0,0,0,4,0,0,0,49,176,8,128,1,240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7,128,6,240,2,128,7,240,66,127,74,80,5,128,73,232,5,128,58,240,134,127,59,128,70,129, 
  108,104,3,128,109,248,2,128,46,184,9,128,47,72,9,128,75,184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,112,117,1,0,17,1,167,200,1,0,1,4,15,1,112,117,1,0,17,1,194,199,1,0,1,4,15,1, 
  112,117,1,0,17,1,221,198,1,0,1,4,15,1,112,117,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,112,117,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,112,117, 
  1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,112,117,1,0,1,4,15,1,112,117,1,0,17,1,72,230,0,0,1,4,15,1,112,117,1,0,17,1,2,230,0,0,1,4,15,1,112,117,1,0,17,1,106, 
  229,0,0,1,4,15,1,112,117,1,0,17,1,133,111,1,0,1,4,15,1,112,117,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,112,117,1,0,1,4,15,1,112,117,1,0,17,1, 
  191,110,1,0,1,4,15,1,112,117,1,0,17,1,160,114,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4, 
  19,69,0,0,0,192,0,0,0,1,0,17,1,219,114,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,56,1,128,69,208,0,128,15,1,219,114,1,0,17,1,4,115,1,0,1,1,2, 
  21,1,111,0,0,0,103,237,1,0,8,0,0,0,3,0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41,112,2,128,72,152,1,128,6,144,1,128,7,144,1,127,8,4,19,69,0,0,0,192,0,0, 
  0,1,0,17,1,188,116,1,0,1,4,17,1,116,115,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,188,116,1,0,1,4,15,1,188,116,1,0,17,1,116,10,0,0,1,2,21,1,47,0,0,0,56,48, 
  2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,42,56,1,128,8,4,17,1,164,115,1,0,1,2,21,1,99,0,0,0,171,48,2,0,7,0,0,0,2,0,0,0,72,120, 
  1,128,1,112,65,129,2,112,129,128,7,112,129,128,6,112,1,128,31,168,2,128,41,16,2,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,8,116,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1, 
  8,116,1,0,1,4,15,1,8,116,1,0,17,1,116,10,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224, 
  1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,8,116,1,0,1,15,1,8,116,1,0,17,1,127,100,1,0,1,15,1,8,116,1,0,17,1,127,116,1,0,1,1,15,1,8,116,1,0,17,1,194,92,1,0,1, 
  19,61,0,0,0,166,0,0,0,1,0,17,1,8,116,1,0,1,2,21,1,47,0,0,0,65,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1, 
  50,93,1,0,1,8,19,38,0,0,0,108,0,0,0,5,0,14,14,1,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224, 
  1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,188,116,1,0,1,15,1,188,116,1,0,17,1,185,109,1,0,1,15,1,188,116,1,0,17,1,51,117,1,0,1,1,15,1,188,116,1,0,17,1,252,101,1,0,1, 
  19,61,0,0,0,166,0,0,0,1,0,17,1,188,116,1,0,1,2,21,1,47,0,0,0,65,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1, 
  108,102,1,0,1,8,19,38,0,0,0,109,0,0,0,3,0,14,14,1,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,70,160,7,128,99,104,4,128,103,216,3,128,19,32,138,127,104,72,3,128,105,184, 
  2,128,38,144,137,126,71,16,199,126,72,168,6,127,73,24,6,127,90,136,5,128,43,40,9,128,44,152,8,129,45,48,8,128,14,136,10,128,95,248,4,128,124,176,2,128,1,19,71,0,0,0,198,0,0,0,1,0,17,1, 
  112,117,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,112,117,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,112,117,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,112,117,1,0,1,19, 
  70,0,0,0,194,0,0,0,1,0,17,1,112,117,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,112,117,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,112,117,1,0,1,15,1,112,117,1,0,17, 
  1,71,139,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,112,117,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,112,117,1,0,1,15,1,112,117,1,0,17,1,180,126,1,0,1,19,14,0,0,0, 
  48,0,0,0,1,0,17,1,112,117,1,0,1,15,1,112,117,1,0,17,1,21,124,1,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,112,117,1,0,1,15,1,112,117,1,0,17,1,5,119,1,0,1,15,1,112, 
  117,1,0,17,1,207,118,1,0,1,2,21,1,53,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,107,1,0,0, 
  3,0,14,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,64,119,1,0,1,8,19,14,0,0,0,46,0,0, 
  0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136, 
  5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,80,120,1,0,17,1,167,200,1,0,1,4,15,1,80,120,1,0,17,1,194,199,1,0,1,4,15,1,80,120, 
  1,0,17,1,221,198,1,0,1,4,15,1,80,120,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,80,120,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,80,120,1,0, 
  1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,80,120,1,0,1,4,15,1,80,120,1,0,17,1,133,111,1,0,1,4,15,1,80,120,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0, 
  17,1,80,120,1,0,1,4,15,1,80,120,1,0,17,1,191,110,1,0,1,4,15,1,80,120,1,0,17,1,227,90,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,208,66,131,73,240, 
  68,131,90,192,7,128,19,200,67,129,44,112,2,128,38,80,8,129,14,200,195,127,71,160,198,128,43,176,200,128,70,128,5,128,95,96,132,128,99,16,6,128,103,56,3,128,104,208,3,128,105,48,7,128,19,14,0,0,0,44, 
  0,0,0,3,0,1,15,1,80,120,1,0,17,1,71,139,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,80,120,1,0,1,1,19,71,0,0,0,197,0,0,0,1,0,17,1,80,120,1,0,1,19,70,0,0, 
  0,194,0,0,0,1,0,17,1,80,120,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,80,120,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,80,120,1,0,1,19,70,0,0,0,195,0,0,0,1, 
  0,17,1,80,120,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,80,120,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,80,120,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,80,120,1, 
  0,1,19,19,0,0,0,56,0,0,0,3,0,1,15,1,80,120,1,0,17,1,116,121,1,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112, 
  1,128,13,48,1,128,4,17,1,176,121,1,0,1,8,19,14,0,0,0,45,0,0,0,3,0,14,1,21,1,176,0,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,232,4,128,1,240,193,127,2,240,129,129,75,184, 
  195,129,108,104,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,80,4,128,103,72,3,128,107,216,2,128,8,4,15,1,97,122,1,0,17,1,167,200,1,0,1,4,15,1,97,122,1,0,17,1,194,199,1,0,1,4, 
  15,1,97,122,1,0,17,1,221,198,1,0,1,4,15,1,97,122,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,97,122,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1, 
  97,122,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,97,122,1,0,1,2,21,0,255,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,72,0,134,130,73,112,133,130,90,224,4,128,99,192,3,128,44, 
  136,7,128,70,248,6,128,14,240,199,127,71,104,70,128,95,80,68,128,103,48,3,128,104,160,2,128,105,16,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,97,122,1,0,1,19,71,0,0,0,197,0,0,0,1,0, 
  17,1,97,122,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,97,122,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,97,122,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,97,122,1,0, 
  1,19,70,0,0,0,193,0,0,0,1,0,17,1,97,122,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,97,122,1,0,1,15,1,97,122,1,0,17,1,97,123,1,0,1,19,44,0,0,0,119,0,0,0,1, 
  0,17,1,97,122,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,97,122,1,0,1,19,14,0,0,0,43,0,0,0,5,0,14,1,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6, 
  48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,145,123,1,0,1,2,21,1,71,0,0,0,228,53,2,0,6,0,0,0,2,0,0,0,108,88,1,128,1,80,1,128,2,80,129,128,7, 
  80,129,128,6,80,1,128,107,200,1,128,8,4,15,1,217,123,1,0,17,1,194,199,1,0,1,4,15,1,217,123,1,0,17,1,221,198,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,72, 
  16,129,128,103,120,1,128,104,24,1,128,71,16,129,127,1,19,71,0,0,0,196,0,0,0,3,0,1,19,72,0,0,0,200,0,0,0,3,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6, 
  112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,80,124,1,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21,1,176,0,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,232, 
  4,128,1,240,193,127,2,240,129,129,75,184,195,129,108,104,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,80,4,128,103,72,3,128,107,216,2,128,8,4,15,1,1,125,1,0,17,1,167,200,1,0,1,4,15,1, 
  1,125,1,0,17,1,194,199,1,0,1,4,15,1,1,125,1,0,17,1,221,198,1,0,1,4,15,1,1,125,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,1,125,1,0,1,4,19, 
  73,0,0,0,202,0,0,0,1,0,17,1,1,125,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,1,125,1,0,1,2,21,0,254,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,72,0,134,130,73, 
  112,133,130,90,224,4,128,99,192,3,128,44,136,7,128,70,248,6,128,14,232,199,127,71,104,70,128,95,80,68,128,103,48,3,128,104,160,2,128,105,16,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,1,125,1,0, 
  1,19,71,0,0,0,197,0,0,0,1,0,17,1,1,125,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,1,125,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,1,125,1,0,1,19,70,0,0,0, 
  194,0,0,0,1,0,17,1,1,125,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,1,125,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,1,125,1,0,1,15,1,1,125,1,0,17,1,0,126,1, 
  0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,1,125,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,1,125,1,0,1,19,14,0,0,0,47,0,0,0,3,0,1,1,2,21,1,47,0,0,0,51,53, 
  2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,48,126,1,0,1,2,21,1,71,0,0,0,228,53,2,0,6,0,0,0,2,0,0,0,108,88, 
  1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,107,200,1,128,8,4,15,1,120,126,1,0,17,1,194,199,1,0,1,4,15,1,120,126,1,0,17,1,221,198,1,0,1,2,21,0,59,0,0,0,255,255, 
  255,255,4,0,0,0,2,0,0,0,72,16,129,128,103,24,1,128,104,120,1,128,71,16,129,127,1,19,72,0,0,0,200,0,0,0,3,0,1,19,71,0,0,0,196,0,0,0,3,0,1,2,21,1,47,0,0,0,173,108, 
  2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,239,126,1,0,1,8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2, 
  0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3, 
  128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,135,129,1,0,17,1,167,200,1,0,1,4,15,1,135,129,1,0,17,1,194,199,1,0,1,4,15,1,135,129,1,0,17,1,221,198,1,0,1,4,15,1,135, 
  129,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,135,129,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,135,129,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0, 
  17,1,135,129,1,0,1,4,15,1,135,129,1,0,17,1,133,111,1,0,1,4,15,1,135,129,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,135,129,1,0,1,4,15,1,135,129,1, 
  0,17,1,191,110,1,0,1,4,15,1,135,129,1,0,17,1,255,127,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1, 
  128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,58,128,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,56,1,128,69,208,0,128,15,1,58,128,1,0,17,1,99,128,1,0, 
  1,1,2,21,1,111,0,0,0,103,237,1,0,8,0,0,0,3,0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41,112,2,128,72,152,1,128,6,144,1,128,7,144,1,127,8,4,19,69,0,0,0, 
  192,0,0,0,1,0,17,1,211,128,1,0,1,4,17,1,183,91,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,211,128,1,0,1,4,15,1,211,128,1,0,17,1,116,10,0,0,1,2,21,0,118,0,0, 
  0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,211,128,1,0,1,15,1,211,128,1, 
  0,17,1,185,109,1,0,1,15,1,211,128,1,0,17,1,74,129,1,0,1,1,15,1,211,128,1,0,17,1,252,101,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,211,128,1,0,1,2,21,1,47,0,0,0, 
  224,107,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1,108,102,1,0,1,8,19,38,0,0,0,109,0,0,0,3,0,14,14,1,21,0,54,1,0, 
  0,255,255,255,255,15,0,0,0,3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128,71,200,70,128,95,176,132,128,99,32,4, 
  128,103,144,3,128,104,0,3,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,135,129,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,135,129,1,0,1,19,72,0,0,0,199,0,0,0,1,0, 
  17,1,135,129,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,135,129,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,135,129,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,135,129,1,0, 
  1,19,44,0,0,0,120,0,0,0,1,0,17,1,135,129,1,0,1,15,1,135,129,1,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,135,129,1,0,1,19,44,0,0,0,118,0,0,0,1, 
  0,17,1,135,129,1,0,1,19,14,0,0,0,38,0,0,0,3,0,1,15,1,135,129,1,0,17,1,90,137,1,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,135,129,1,0,1,15,1,135,129,1,0,17,1,190, 
  130,1,0,1,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,250,130,1,0,1,8,19,14,0,0,0,41,0, 
  0,0,3,0,14,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128, 
  73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,146,133,1,0,17,1,167,200,1,0,1,4,15,1,146,133,1,0,17,1,194,199,1,0,1,4,15,1, 
  146,133,1,0,17,1,221,198,1,0,1,4,15,1,146,133,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,146,133,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,146,133, 
  1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,146,133,1,0,1,4,15,1,146,133,1,0,17,1,133,111,1,0,1,4,15,1,146,133,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0, 
  1,0,17,1,146,133,1,0,1,4,15,1,146,133,1,0,17,1,191,110,1,0,1,4,15,1,146,133,1,0,17,1,10,132,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128, 
  1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,69,132,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,56,1,128,69, 
  208,0,128,15,1,69,132,1,0,17,1,110,132,1,0,1,1,2,21,1,111,0,0,0,103,237,1,0,8,0,0,0,3,0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41,112,2,128,72,152,1,128, 
  6,144,1,128,7,144,1,127,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,222,132,1,0,1,4,17,1,183,91,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,222,132,1,0,1,4,15,1,222,132, 
  1,0,17,1,116,10,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224,1,128,19,61,0,0,0,167,0,0, 
  0,1,0,17,1,222,132,1,0,1,15,1,222,132,1,0,17,1,185,109,1,0,1,15,1,222,132,1,0,17,1,85,133,1,0,1,1,15,1,222,132,1,0,17,1,252,101,1,0,1,19,61,0,0,0,166,0,0,0,1, 
  0,17,1,222,132,1,0,1,2,21,1,47,0,0,0,224,107,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,45,56,1,128,8,4,17,1,108,102,1,0,1,19,38,0,0, 
  0,109,0,0,0,3,0,14,14,1,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,8,67,131,73,232,69,131,90,112,7,128,43,120,6,130,44,240,4,128,38,112,195,128,14,0,195,127,15,0,131,128, 
  70,208,3,128,71,96,68,128,95,88,133,128,99,0,8,128,103,112,2,128,104,224,6,128,105,144,8,128,19,72,0,0,0,199,0,0,0,1,0,17,1,146,133,1,0,1,1,15,1,146,133,1,0,17,1,166,136,1,0,1, 
  19,15,0,0,0,51,0,0,0,3,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,146,133,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,146,133,1,0,1,19,14,0,0,0,37,0,0,0,5,0,14, 
  1,19,70,0,0,0,194,0,0,0,1,0,17,1,146,133,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,146,133,1,0,1,15,1,146,133,1,0,17,1,183,134,1,0,1,19,71,0,0,0,197,0,0,0,1, 
  0,17,1,146,133,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,146,133,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,146,133,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,146,133,1, 
  0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,244,134,1,0,1,8,19,14,0,0,0,39,0,0,0,5, 
  0,14,14,1,21,1,176,0,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,232,4,128,1,240,193,127,2,240,129,129,75,184,195,129,108,104,2,128,109,248,1,128,6,240,1,128,7,240,129,128,74,80,4,128,103,72, 
  3,128,107,216,2,128,8,4,15,1,165,135,1,0,17,1,167,200,1,0,1,4,15,1,165,135,1,0,17,1,194,199,1,0,1,4,15,1,165,135,1,0,17,1,221,198,1,0,1,4,15,1,165,135,1,0,17,1,232,111, 
  1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,165,135,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,165,135,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,165,135,1,0,1, 
  2,21,0,0,1,0,0,255,255,255,255,12,0,0,0,3,0,0,0,72,0,134,130,73,112,133,130,90,224,4,128,99,192,3,128,44,136,7,128,70,248,6,128,14,248,199,127,71,104,70,128,95,80,68,128,103,48,3,128,104, 
  160,2,128,105,16,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,165,135,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,165,135,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,165,135,1,0, 
  1,19,70,0,0,0,195,0,0,0,1,0,17,1,165,135,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,165,135,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,165,135,1,0,1,19,44,0,0,0, 
  120,0,0,0,1,0,17,1,165,135,1,0,1,15,1,165,135,1,0,17,1,0,126,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,165,135,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,165,135,1, 
  0,1,19,14,0,0,0,35,0,0,0,7,0,14,14,1,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1, 
  214,136,1,0,1,2,21,1,71,0,0,0,228,53,2,0,6,0,0,0,2,0,0,0,108,88,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,107,200,1,128,8,4,15,1,30,137,1,0,17,1,194,199, 
  1,0,1,4,15,1,30,137,1,0,17,1,221,198,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,72,112,129,128,103,120,1,128,104,16,1,128,71,112,129,127,19,71,0,0,0,196,0,0, 
  0,3,0,1,1,19,72,0,0,0,200,0,0,0,3,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,150, 
  137,1,0,1,8,19,14,0,0,0,40,0,0,0,3,0,14,1,21,1,176,0,0,0,131,52,2,0,11,0,0,0,3,0,0,0,73,232,4,128,1,240,193,127,2,240,129,129,75,184,195,129,108,104,2,128,109,248,1,128, 
  6,240,1,128,7,240,129,128,74,80,4,128,103,72,3,128,107,216,2,128,8,4,15,1,71,138,1,0,17,1,167,200,1,0,1,4,15,1,71,138,1,0,17,1,194,199,1,0,1,4,15,1,71,138,1,0,17,1,221,198, 
  1,0,1,4,15,1,71,138,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,71,138,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,71,138,1,0,1,4,19,73,0,0, 
  0,201,0,0,0,1,0,17,1,71,138,1,0,1,2,21,0,255,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,72,0,134,130,73,112,133,130,90,224,4,128,99,192,3,128,44,136,7,128,70,248,6,128,14,240,199, 
  127,71,104,70,128,95,80,68,128,103,48,3,128,104,160,2,128,105,16,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,71,138,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,71,138,1,0,1,19,72,0, 
  0,0,199,0,0,0,1,0,17,1,71,138,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,71,138,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,71,138,1,0,1,19,70,0,0,0,193,0,0,0, 
  1,0,17,1,71,138,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,71,138,1,0,1,15,1,71,138,1,0,17,1,0,126,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,71,138,1,0,1,19,44, 
  0,0,0,118,0,0,0,1,0,17,1,71,138,1,0,1,19,14,0,0,0,36,0,0,0,5,0,14,1,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129, 
  127,7,48,1,128,13,56,1,128,8,4,17,1,119,139,1,0,1,2,21,1,71,0,0,0,228,53,2,0,6,0,0,0,2,0,0,0,108,88,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,107,200,1, 
  128,8,4,15,1,191,139,1,0,17,1,194,199,1,0,1,4,15,1,191,139,1,0,17,1,221,198,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,72,112,129,128,103,16,1,128,104,120,1, 
  128,71,112,129,127,19,72,0,0,0,200,0,0,0,3,0,1,1,19,71,0,0,0,196,0,0,0,3,0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129, 
  127,7,48,1,128,10,56,1,128,8,4,17,1,43,140,1,0,1,2,21,1,69,1,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,176,8,128,1,240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7, 
  128,6,240,2,128,7,240,66,127,74,80,5,128,73,232,5,128,58,240,134,127,59,128,70,129,108,104,3,128,109,248,2,128,46,184,9,128,47,72,9,128,75,184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,65,144,1, 
  0,17,1,167,200,1,0,1,4,15,1,65,144,1,0,17,1,194,199,1,0,1,4,15,1,65,144,1,0,17,1,221,198,1,0,1,4,15,1,65,144,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0, 
  1,0,17,1,65,144,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,65,144,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,65,144,1,0,1,4,15,1,65,144,1,0,17,1,72,230,0,0, 
  1,4,15,1,65,144,1,0,17,1,2,230,0,0,1,4,15,1,65,144,1,0,17,1,106,229,0,0,1,4,15,1,65,144,1,0,17,1,133,111,1,0,1,4,15,1,65,144,1,0,17,1,34,111,1,0,1,4,19,38, 
  0,0,0,112,0,0,0,1,0,17,1,65,144,1,0,1,4,15,1,65,144,1,0,17,1,191,110,1,0,1,4,15,1,65,144,1,0,17,1,113,141,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2, 
  0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,172,141,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,38,56,1,128,69,208,0,128,15,1,172,141,1,0,17,1,213,141,1,0,1,1,2,21,1,111,0,0,0,103,237,1,0,8,0,0,0,3,0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41, 
  112,2,128,72,152,1,128,6,144,1,128,7,144,1,127,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,141,143,1,0,1,4,17,1,69,142,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,141,143,1, 
  0,1,4,15,1,141,143,1,0,17,1,116,10,0,0,1,2,21,1,47,0,0,0,56,48,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,42,56,1,128,8,4,17,1,117, 
  142,1,0,1,2,21,1,99,0,0,0,171,48,2,0,7,0,0,0,2,0,0,0,72,120,1,128,1,112,65,129,2,112,129,128,7,112,129,128,6,112,1,128,31,168,2,128,41,16,2,128,8,4,19,69,0,0,0,192,0, 
  0,0,1,0,17,1,217,142,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,217,142,1,0,1,4,15,1,217,142,1,0,17,1,116,10,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2, 
  0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,217,142,1,0,1,15,1,217,142,1,0,17,1,127,100,1,0,1,15,1, 
  217,142,1,0,17,1,80,143,1,0,1,1,15,1,217,142,1,0,17,1,194,92,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,217,142,1,0,1,2,21,1,47,0,0,0,65,108,2,0,5,0,0,0,2,0, 
  0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1,50,93,1,0,1,8,19,38,0,0,0,108,0,0,0,5,0,14,14,1,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2, 
  0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1,141,143,1,0,1,15,1,141,143,1,0,17,1,185,109,1,0,1,15,1, 
  141,143,1,0,17,1,4,144,1,0,1,1,15,1,141,143,1,0,17,1,252,101,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,141,143,1,0,1,2,21,1,47,0,0,0,65,108,2,0,5,0,0,0,2,0, 
  0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,45,48,1,128,4,17,1,108,102,1,0,1,8,19,38,0,0,0,109,0,0,0,3,0,14,14,1,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4, 
  0,0,0,70,160,7,128,99,104,4,128,103,216,3,128,19,32,138,127,104,72,3,128,105,184,2,128,38,144,137,126,71,16,199,126,72,168,6,127,73,24,6,127,90,136,5,128,43,40,9,128,44,152,8,129,45,48,8,128,14, 
  136,10,128,95,248,4,128,124,176,2,128,1,19,71,0,0,0,198,0,0,0,1,0,17,1,65,144,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,65,144,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17, 
  1,65,144,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,65,144,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,65,144,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,65,144,1,0,1, 
  19,44,0,0,0,120,0,0,0,1,0,17,1,65,144,1,0,1,15,1,65,144,1,0,17,1,0,126,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,65,144,1,0,1,19,44,0,0,0,118,0,0,0,1,0, 
  17,1,65,144,1,0,1,15,1,65,144,1,0,17,1,69,148,1,0,1,19,14,0,0,0,48,0,0,0,1,0,17,1,65,144,1,0,1,15,1,65,144,1,0,17,1,21,124,1,0,1,19,19,0,0,0,55,0,0,0, 
  1,0,17,1,65,144,1,0,1,15,1,65,144,1,0,17,1,214,145,1,0,1,15,1,65,144,1,0,17,1,160,145,1,0,1,2,21,1,53,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,124,0,0,0,106,1,0,0,4,0,14,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127, 
  7,112,1,128,13,48,1,128,4,17,1,17,146,1,0,1,8,19,14,0,0,0,46,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50, 
  144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,33,147,1,0,17, 
  1,167,200,1,0,1,4,15,1,33,147,1,0,17,1,194,199,1,0,1,4,15,1,33,147,1,0,17,1,221,198,1,0,1,4,15,1,33,147,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0, 
  17,1,33,147,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,33,147,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,33,147,1,0,1,4,15,1,33,147,1,0,17,1,133,111,1,0,1,4, 
  15,1,33,147,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,33,147,1,0,1,4,15,1,33,147,1,0,17,1,191,110,1,0,1,4,15,1,33,147,1,0,17,1,227,90,1,0,1, 
  2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,176,72,131,73,168,69,131,90,184,7,128,19,112,66,129,44,56,6,128,38,8,3,129,14,112,194,127,71,104,195,128,43,72,200,128,70,24,5,128,95, 
  40,135,128,99,120,2,128,103,248,3,128,104,136,4,128,105,152,6,128,1,19,70,0,0,0,195,0,0,0,1,0,17,1,33,147,1,0,1,19,19,0,0,0,56,0,0,0,3,0,1,19,44,0,0,0,119,0,0,0,1, 
  0,17,1,33,147,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,33,147,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,33,147,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,33,147,1, 
  0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,33,147,1,0,1,19,14,0,0,0,44,0,0,0,3,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,33,147,1,0,1,19,70,0,0,0,194,0,0,0,1, 
  0,17,1,33,147,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,33,147,1,0,1,15,1,33,147,1,0,17,1,116,121,1,0,1,15,1,33,147,1,0,17,1,71,139,1,0,1,2,21,1,47,0,0,0,173, 
  108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,128,148,1,0,1,8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51, 
  2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8, 
  3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,144,149,1,0,17,1,167,200,1,0,1,4,15,1,144,149,1,0,17,1,194,199,1,0,1,4,15,1,144,149,1,0,17,1,221,198,1,0,1,4,15,1, 
  144,149,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,144,149,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,144,149,1,0,1,4,19,73,0,0,0,201,0,0,0,1, 
  0,17,1,144,149,1,0,1,4,15,1,144,149,1,0,17,1,133,111,1,0,1,4,15,1,144,149,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,144,149,1,0,1,4,15,1,144,149, 
  1,0,17,1,191,110,1,0,1,4,15,1,144,149,1,0,17,1,255,127,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232, 
  7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128,71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,144,149,1,0,1, 
  19,71,0,0,0,197,0,0,0,1,0,17,1,144,149,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,144,149,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,144,149,1,0,1,19,70,0,0,0,194, 
  0,0,0,1,0,17,1,144,149,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,144,149,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,144,149,1,0,1,15,1,144,149,1,0,17,1,83,228,0,0, 
  1,19,44,0,0,0,119,0,0,0,1,0,17,1,144,149,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,144,149,1,0,1,19,14,0,0,0,38,0,0,0,3,0,1,15,1,144,149,1,0,17,1,90,137,1, 
  0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,144,149,1,0,1,15,1,144,149,1,0,17,1,199,150,1,0,1,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193, 
  128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,3,151,1,0,1,8,19,14,0,0,0,41,0,0,0,3,0,14,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194, 
  127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15, 
  1,19,152,1,0,17,1,167,200,1,0,1,4,15,1,19,152,1,0,17,1,194,199,1,0,1,4,15,1,19,152,1,0,17,1,221,198,1,0,1,4,15,1,19,152,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0, 
  203,0,0,0,1,0,17,1,19,152,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,19,152,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,19,152,1,0,1,4,15,1,19,152,1,0,17,1, 
  133,111,1,0,1,4,15,1,19,152,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,19,152,1,0,1,4,15,1,19,152,1,0,17,1,191,110,1,0,1,4,15,1,19,152,1,0,17, 
  1,10,132,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,0,67,131,73,232,69,131,90,144,8,128,43,40,8,130,44,200,3,128,38,104,195,128,14,48,196,127,15,48,132,128,70,56,4, 
  128,71,152,71,128,95,88,133,128,99,8,7,128,103,120,6,128,104,200,4,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,19,152,1,0,1,15,1,19,152,1,0,17,1,166,136,1,0,1,19,15,0,0, 
  0,51,0,0,0,3,0,1,19,14,0,0,0,37,0,0,0,5,0,14,1,1,19,44,0,0,0,118,0,0,0,1,0,17,1,19,152,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,19,152,1,0,1,19,70, 
  0,0,0,194,0,0,0,1,0,17,1,19,152,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,19,152,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,19,152,1,0,1,19,70,0,0,0,195,0,0, 
  0,1,0,17,1,19,152,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,19,152,1,0,1,15,1,19,152,1,0,17,1,183,134,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,19,152,1,0,1,2, 
  21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,104,153,1,0,1,2,21,1,58,0,0,0,68,66,2,0,5,0, 
  0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,129,56,1,128,8,4,19,120,0,0,0,101,1,0,0,1,0,17,1,163,153,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,120,216,0,128,125,208,0,128,1,15,1,163,153,1,0,17,1,204,153,1,0,1,2,21,1,53,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48, 
  65,128,11,56,1,128,8,4,19,125,0,0,0,108,1,0,0,4,0,14,1,2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,192,1,128,93,232,130,128,94,224,2,128,121,80,66,128,125,48,1,128, 
  19,93,0,0,0,18,1,0,0,1,0,17,1,2,154,1,0,1,19,93,0,0,0,17,1,0,0,1,0,17,1,2,154,1,0,1,19,93,0,0,0,16,1,0,0,1,0,17,1,2,154,1,0,1,1,15,1,2,154,1, 
  0,17,1,109,154,1,0,1,2,21,1,125,0,0,0,153,66,2,0,9,0,0,0,3,0,0,0,10,120,67,129,1,176,1,128,2,176,129,127,11,8,67,129,31,152,2,128,130,40,2,128,6,176,1,128,7,176,65,127,131, 
  184,1,128,8,4,15,1,235,154,1,0,17,1,56,153,1,0,1,4,15,1,235,154,1,0,17,1,251,139,1,0,1,4,15,1,235,154,1,0,17,1,51,155,1,0,1,4,19,94,0,0,0,23,1,0,0,3,0,14,1, 
  4,15,1,235,154,1,0,17,1,90,113,1,0,1,2,21,0,71,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,112,1,128,93,48,66,128,121,208,65,128,125,16,1,128,19,93,0,0,0,21,1,0,0,2,0, 
  1,19,93,0,0,0,20,1,0,0,2,0,1,19,93,0,0,0,19,1,0,0,2,0,1,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1, 
  128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,110,155,1,0,1,2,21,0,84,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,121,8,2,128,69,16,194,127,122,120,1,128,123,16,1,128, 
  15,1,110,155,1,0,17,1,195,155,1,0,1,19,123,0,0,0,104,1,0,0,1,0,17,1,110,155,1,0,1,1,19,122,0,0,0,103,1,0,0,1,0,17,1,110,155,1,0,1,2,21,1,59,0,0,0,228,248,1, 
  0,6,0,0,0,2,0,0,0,32,152,1,128,1,80,1,128,2,80,129,128,7,80,1,128,6,80,65,128,66,88,1,128,8,4,17,1,164,245,0,0,1,4,17,1,255,155,1,0,1,2,21,1,47,0,0,0,240,65,2, 
  0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,47,156,1,0,1,2,21,1,69,1,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,176,8, 
  128,1,240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7,128,6,240,2,128,7,240,66,127,74,80,5,128,73,232,5,128,58,240,134,127,59,128,70,129,108,104,3,128,109,248,2,128,46,184,9,128,47,72,9, 
  128,75,184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,145,159,1,0,17,1,167,200,1,0,1,4,15,1,145,159,1,0,17,1,194,199,1,0,1,4,15,1,145,159,1,0,17,1,221,198,1,0,1,4,15,1,145, 
  159,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,145,159,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,145,159,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0, 
  17,1,145,159,1,0,1,4,15,1,145,159,1,0,17,1,72,230,0,0,1,4,15,1,145,159,1,0,17,1,2,230,0,0,1,4,15,1,145,159,1,0,17,1,106,229,0,0,1,4,15,1,145,159,1,0,17,1,133,111, 
  1,0,1,4,15,1,145,159,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,145,159,1,0,1,4,15,1,145,159,1,0,17,1,191,110,1,0,1,4,15,1,145,159,1,0,17,1,117, 
  157,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,176,157, 
  1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,56,1,128,69,208,0,128,15,1,176,157,1,0,17,1,217,157,1,0,1,1,2,21,1,111,0,0,0,103,237,1,0,8,0,0,0,3, 
  0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41,112,2,128,72,152,1,128,6,144,1,128,7,144,1,127,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,141,143,1,0,1,4,17,1,73,158, 
  1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,141,143,1,0,1,4,15,1,141,143,1,0,17,1,116,10,0,0,1,2,21,1,47,0,0,0,56,48,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1, 
  48,1,128,2,48,129,127,7,48,1,128,42,56,1,128,8,4,17,1,121,158,1,0,1,2,21,1,99,0,0,0,171,48,2,0,7,0,0,0,2,0,0,0,72,120,1,128,1,112,65,129,2,112,129,128,7,112,129,128,6, 
  112,1,128,31,168,2,128,41,16,2,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,221,158,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,221,158,1,0,1,4,15,1,221,158,1,0,17,1,116, 
  10,0,0,1,2,21,0,118,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,184,2,129,13,32,195,127,38,176,2,128,79,80,1,128,61,72,66,128,69,224,1,128,19,61,0,0,0,167,0,0,0,1,0,17,1, 
  221,158,1,0,1,15,1,221,158,1,0,17,1,127,100,1,0,1,15,1,221,158,1,0,17,1,84,159,1,0,1,1,15,1,221,158,1,0,17,1,194,92,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,221,158, 
  1,0,1,2,21,1,47,0,0,0,65,108,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,45,56,1,128,8,4,17,1,50,93,1,0,1,19,38,0,0,0,108,0,0,0, 
  5,0,14,14,1,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,70,160,7,128,99,104,4,128,103,216,3,128,19,32,138,127,104,72,3,128,105,184,194,130,38,144,137,126,71,16,199,126,72,168,6,127,73, 
  24,6,127,90,136,5,128,43,40,9,128,44,152,8,128,45,48,8,128,14,136,10,128,95,248,4,128,121,176,2,128,1,19,71,0,0,0,198,0,0,0,1,0,17,1,145,159,1,0,1,19,71,0,0,0,197,0,0,0,1, 
  0,17,1,145,159,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,145,159,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,145,159,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,145,159,1, 
  0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,145,159,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,145,159,1,0,1,15,1,145,159,1,0,17,1,0,126,1,0,1,19,44,0,0,0,119,0,0,0, 
  1,0,17,1,145,159,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,145,159,1,0,1,15,1,145,159,1,0,17,1,150,163,1,0,1,19,14,0,0,0,48,0,0,0,1,0,17,1,145,159,1,0,1,15,1, 
  145,159,1,0,17,1,21,124,1,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,145,159,1,0,1,15,1,145,159,1,0,17,1,39,161,1,0,1,15,1,145,159,1,0,17,1,240,160,1,0,1,2,21,1,54,0, 
  0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,121,0,0,0,102,1,0,0,6,0,14,14,1,2,21,1,47,0,0,0,173,108,2, 
  0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,98,161,1,0,1,8,19,14,0,0,0,46,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0, 
  16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128, 
  109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,114,162,1,0,17,1,167,200,1,0,1,4,15,1,114,162,1,0,17,1,194,199,1,0,1,4,15,1,114,162,1,0,17,1,221,198,1,0,1,4,15,1,114,162, 
  1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,114,162,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,114,162,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17, 
  1,114,162,1,0,1,4,15,1,114,162,1,0,17,1,133,111,1,0,1,4,15,1,114,162,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,114,162,1,0,1,4,15,1,114,162,1,0, 
  17,1,191,110,1,0,1,4,15,1,114,162,1,0,17,1,227,90,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,80,72,131,73,152,67,131,90,0,3,128,19,144,67,129,44,184,8,128, 
  38,40,4,129,14,144,195,127,71,200,198,128,43,232,199,128,70,136,4,128,95,168,133,128,99,56,6,128,103,24,5,128,104,112,2,128,105,88,7,128,19,71,0,0,0,197,0,0,0,1,0,17,1,114,162,1,0,1,19,70, 
  0,0,0,193,0,0,0,1,0,17,1,114,162,1,0,1,1,19,44,0,0,0,120,0,0,0,1,0,17,1,114,162,1,0,1,19,19,0,0,0,56,0,0,0,3,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1, 
  114,162,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,114,162,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,114,162,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,114,162,1,0,1,19, 
  44,0,0,0,119,0,0,0,1,0,17,1,114,162,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,114,162,1,0,1,15,1,114,162,1,0,17,1,116,121,1,0,1,15,1,114,162,1,0,17,1,71,139,1,0, 
  1,19,14,0,0,0,44,0,0,0,3,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,209,163,1,0,1, 
  8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7, 
  144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,225,164,1,0,17,1,167,200,1,0,1,4,15,1,225,164,1,0,17,1,194, 
  199,1,0,1,4,15,1,225,164,1,0,17,1,221,198,1,0,1,4,15,1,225,164,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,225,164,1,0,1,4,19,73,0,0,0,202,0,0, 
  0,1,0,17,1,225,164,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,225,164,1,0,1,4,15,1,225,164,1,0,17,1,133,111,1,0,1,4,15,1,225,164,1,0,17,1,34,111,1,0,1,4,19,38, 
  0,0,0,112,0,0,0,1,0,17,1,225,164,1,0,1,4,15,1,225,164,1,0,17,1,191,110,1,0,1,4,15,1,225,164,1,0,17,1,255,127,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3, 
  0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128,71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128,105, 
  112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,225,164,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,225,164,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,225,164,1,0,1,19,70,0, 
  0,0,195,0,0,0,1,0,17,1,225,164,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,225,164,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,225,164,1,0,1,19,44,0,0,0,120,0,0,0, 
  1,0,17,1,225,164,1,0,1,15,1,225,164,1,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,225,164,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,225,164,1,0,1,19,14, 
  0,0,0,38,0,0,0,3,0,1,15,1,225,164,1,0,17,1,90,137,1,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,225,164,1,0,1,15,1,225,164,1,0,17,1,24,166,1,0,1,1,2,21,1,47,0, 
  0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,84,166,1,0,1,19,14,0,0,0,41,0,0,0,3,0,14,1,21,1,15,1, 
  0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88, 
  68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,100,167,1,0,17,1,167,200,1,0,1,4,15,1,100,167,1,0,17,1,194,199,1,0,1,4,15,1,100,167,1,0,17,1,221,198,1,0, 
  1,4,15,1,100,167,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,100,167,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,100,167,1,0,1,4,19,73,0,0,0,201, 
  0,0,0,1,0,17,1,100,167,1,0,1,4,15,1,100,167,1,0,17,1,133,111,1,0,1,4,15,1,100,167,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,100,167,1,0,1,4, 
  15,1,100,167,1,0,17,1,191,110,1,0,1,4,15,1,100,167,1,0,17,1,10,132,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,192,71,131,73,16,70,131,90,144,8,128,43,40, 
  8,130,44,0,3,128,38,32,197,128,14,248,195,127,15,248,131,128,70,0,4,128,71,128,69,128,95,144,132,128,99,112,2,128,103,48,7,128,104,104,3,128,105,160,6,128,19,70,0,0,0,195,0,0,0,1,0,17,1,100, 
  167,1,0,1,19,14,0,0,0,37,0,0,0,5,0,14,1,19,71,0,0,0,197,0,0,0,1,0,17,1,100,167,1,0,1,1,19,44,0,0,0,118,0,0,0,1,0,17,1,100,167,1,0,1,19,70,0,0,0,194, 
  0,0,0,1,0,17,1,100,167,1,0,1,19,15,0,0,0,51,0,0,0,3,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,100,167,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,100,167,1,0,1, 
  19,71,0,0,0,198,0,0,0,1,0,17,1,100,167,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,100,167,1,0,1,15,1,100,167,1,0,17,1,166,136,1,0,1,15,1,100,167,1,0,17,1,183,134,1, 
  0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,100,167,1,0,1,2,21,1,58,0,0,0,85,227,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,41,56,1,128,8, 
  4,19,37,0,0,0,101,0,0,0,1,0,17,1,196,168,1,0,1,2,21,0,39,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,91,208,0,128,37,216,192,127,1,19,91,0,0,0,12,1,0,0,2,0,1,2, 
  21,0,79,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,94,160,1,128,121,16,1,128,90,112,130,127,91,8,2,128,19,94,0,0,0,22,1,0,0,1,0,17,1,236,168,1,0,1,19,90,0,0,0,11,1,0, 
  0,4,0,14,1,15,1,236,168,1,0,17,1,60,169,1,0,1,1,2,21,1,71,0,0,0,27,67,2,0,6,0,0,0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,200,1,128,31,88, 
  1,128,8,4,15,1,132,169,1,0,17,1,51,155,1,0,1,4,15,1,132,169,1,0,17,1,238,112,1,0,1,2,21,0,63,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,90,240,129,128,121,240,0,128,94,128, 
  1,128,19,94,0,0,0,22,1,0,0,1,0,17,1,132,169,1,0,1,19,90,0,0,0,10,1,0,0,5,0,14,14,1,1,2,21,1,71,0,0,0,27,67,2,0,6,0,0,0,2,0,0,0,6,80,1,129,1,80, 
  1,128,2,80,129,127,7,80,129,128,10,200,1,128,31,88,1,128,8,4,15,1,1,184,1,0,17,1,169,171,1,0,1,4,15,1,1,184,1,0,17,1,12,170,1,0,1,2,21,1,107,0,0,0,117,63,2,0,8,0, 
  0,0,3,0,0,0,10,232,66,129,1,144,1,128,2,144,129,127,131,152,1,128,31,120,2,128,130,8,2,128,6,144,1,128,7,144,65,127,8,4,15,1,120,170,1,0,17,1,56,153,1,0,1,4,15,1,120,170,1,0, 
  17,1,251,139,1,0,1,4,15,1,120,170,1,0,17,1,169,171,1,0,1,4,15,1,120,170,1,0,17,1,90,113,1,0,1,2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,97,232, 
  130,128,98,224,130,128,125,192,1,128,126,48,1,128,19,97,0,0,0,28,1,0,0,1,0,17,1,120,170,1,0,1,19,97,0,0,0,29,1,0,0,1,0,17,1,120,170,1,0,1,19,97,0,0,0,30,1,0,0,1, 
  0,17,1,120,170,1,0,1,1,15,1,120,170,1,0,17,1,227,170,1,0,1,2,21,1,125,0,0,0,153,66,2,0,9,0,0,0,3,0,0,0,10,120,67,129,1,176,1,128,2,176,129,127,11,8,67,129,31,152,2, 
  128,130,40,2,128,6,176,1,128,7,176,65,127,131,184,1,128,8,4,15,1,97,171,1,0,17,1,56,153,1,0,1,4,15,1,97,171,1,0,17,1,251,139,1,0,1,4,15,1,97,171,1,0,17,1,169,171,1,0,1, 
  4,19,98,0,0,0,35,1,0,0,3,0,14,1,4,15,1,97,171,1,0,17,1,90,113,1,0,1,2,21,0,71,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,208,1,128,97,48,130,128,126,16,1,128,125, 
  112,1,128,19,97,0,0,0,31,1,0,0,2,0,1,19,97,0,0,0,32,1,0,0,2,0,1,19,97,0,0,0,33,1,0,0,2,0,1,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6, 
  80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,246,171,1,0,1,4,15,1,246,171,1,0,17,1,116,10,0,0,1,2,21,0, 
  101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,37,48,2,129,13,152,194,127,126,48,1,128,79,56,1,128,61,200,1,128,1,19,61,0,0,0,167,0,0,0,1,0,17,1,246,171,1,0,1,15,1,246,171,1, 
  0,17,1,143,172,1,0,1,15,1,246,171,1,0,17,1,92,172,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,246,171,1,0,1,2,21,1,39,0,0,0,13,109,2,0,3,0,0,0,1,0,0,0,2,240, 
  0,128,1,240,64,128,79,248,0,128,8,4,17,1,206,8,0,0,1,19,13,0,0,0,34,0,0,0,1,0,1,21,1,47,0,0,0,144,248,1,0,5,0,0,0,2,0,0,0,32,56,1,128,1,48,1,128,2,48,129, 
  128,7,48,1,128,6,48,1,128,8,4,17,1,191,172,1,0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17, 
  1,239,172,1,0,1,2,21,1,69,1,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,176,8,128,1,240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7,128,6,240,2,128,7,240,66,127,74,80,5, 
  128,73,232,5,128,58,240,134,127,59,128,70,129,108,104,3,128,109,248,2,128,46,184,9,128,47,72,9,128,75,184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,9,175,1,0,17,1,167,200,1,0,1,4,15,1,9, 
  175,1,0,17,1,194,199,1,0,1,4,15,1,9,175,1,0,17,1,221,198,1,0,1,4,15,1,9,175,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,9,175,1,0,1,4,19,73, 
  0,0,0,202,0,0,0,1,0,17,1,9,175,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,9,175,1,0,1,4,15,1,9,175,1,0,17,1,72,230,0,0,1,4,15,1,9,175,1,0,17,1,2,230, 
  0,0,1,4,15,1,9,175,1,0,17,1,106,229,0,0,1,4,15,1,9,175,1,0,17,1,133,111,1,0,1,4,15,1,9,175,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,9, 
  175,1,0,1,4,15,1,9,175,1,0,17,1,191,110,1,0,1,4,15,1,9,175,1,0,17,1,53,174,1,0,1,2,21,1,58,0,0,0,238,236,1,0,5,0,0,0,2,0,0,0,72,56,1,128,1,48,1,128,2, 
  48,129,128,7,48,1,128,6,48,1,128,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,112,174,1,0,1,2,21,0,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,38,56,1,128,69,208,0,128,15,1, 
  112,174,1,0,17,1,153,174,1,0,1,1,2,21,1,111,0,0,0,103,237,1,0,8,0,0,0,3,0,0,0,48,48,66,129,1,144,193,128,2,144,1,128,31,8,3,128,41,112,2,128,72,152,1,128,6,144,1,128,7, 
  144,1,127,8,4,19,69,0,0,0,192,0,0,0,1,0,17,1,141,143,1,0,1,4,17,1,116,115,1,0,1,4,19,37,0,0,0,101,0,0,0,1,0,17,1,141,143,1,0,1,4,15,1,141,143,1,0,17,1,116, 
  10,0,0,1,2,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,70,160,7,128,99,104,4,128,103,216,3,128,19,32,138,127,104,72,3,128,105,184,2,128,38,144,137,126,71,16,199,126,72,168,6,127,73, 
  24,6,127,90,136,5,128,43,40,9,128,44,152,8,128,45,48,8,128,14,136,138,128,95,248,4,128,126,176,2,128,1,19,71,0,0,0,198,0,0,0,1,0,17,1,9,175,1,0,1,19,71,0,0,0,197,0,0,0,1, 
  0,17,1,9,175,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,9,175,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,9,175,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,9,175,1, 
  0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,9,175,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,9,175,1,0,1,15,1,9,175,1,0,17,1,166,136,1,0,1,19,44,0,0,0,119,0,0,0, 
  1,0,17,1,9,175,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,9,175,1,0,1,15,1,9,175,1,0,17,1,14,179,1,0,1,19,14,0,0,0,48,0,0,0,1,0,17,1,9,175,1,0,1,15,1, 
  9,175,1,0,17,1,21,124,1,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,9,175,1,0,1,15,1,9,175,1,0,17,1,159,176,1,0,1,15,1,9,175,1,0,17,1,104,176,1,0,1,2,21,1,54,0, 
  0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,126,0,0,0,109,1,0,0,6,0,14,14,1,2,21,1,47,0,0,0,173,108,2, 
  0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,218,176,1,0,1,8,19,14,0,0,0,46,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0, 
  16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128, 
  109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,234,177,1,0,17,1,167,200,1,0,1,4,15,1,234,177,1,0,17,1,194,199,1,0,1,4,15,1,234,177,1,0,17,1,221,198,1,0,1,4,15,1,234,177, 
  1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,234,177,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,234,177,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17, 
  1,234,177,1,0,1,4,15,1,234,177,1,0,17,1,133,111,1,0,1,4,15,1,234,177,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,234,177,1,0,1,4,15,1,234,177,1,0, 
  17,1,191,110,1,0,1,4,15,1,234,177,1,0,17,1,227,90,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,0,67,131,73,96,68,131,90,136,8,128,19,200,67,129,44,104,3,128, 
  38,40,8,129,14,200,195,127,71,112,194,128,43,192,199,128,70,208,3,128,95,16,134,128,99,128,5,128,103,160,6,128,104,240,4,128,105,48,7,128,19,44,0,0,0,119,0,0,0,1,0,17,1,234,177,1,0,1,15,1, 
  234,177,1,0,17,1,71,139,1,0,1,19,14,0,0,0,44,0,0,0,3,0,1,1,19,44,0,0,0,118,0,0,0,1,0,17,1,234,177,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,234,177,1,0,1, 
  19,71,0,0,0,197,0,0,0,1,0,17,1,234,177,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,234,177,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,234,177,1,0,1,19,72,0,0,0,199, 
  0,0,0,1,0,17,1,234,177,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,234,177,1,0,1,15,1,234,177,1,0,17,1,116,121,1,0,1,19,19,0,0,0,56,0,0,0,3,0,1,19,70,0,0,0, 
  193,0,0,0,1,0,17,1,234,177,1,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,73,179,1,0,1, 
  8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7, 
  144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,89,180,1,0,17,1,167,200,1,0,1,4,15,1,89,180,1,0,17,1,194, 
  199,1,0,1,4,15,1,89,180,1,0,17,1,221,198,1,0,1,4,15,1,89,180,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,89,180,1,0,1,4,19,73,0,0,0,202,0,0, 
  0,1,0,17,1,89,180,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,89,180,1,0,1,4,15,1,89,180,1,0,17,1,133,111,1,0,1,4,15,1,89,180,1,0,17,1,34,111,1,0,1,4,19,38, 
  0,0,0,112,0,0,0,1,0,17,1,89,180,1,0,1,4,15,1,89,180,1,0,17,1,191,110,1,0,1,4,15,1,89,180,1,0,17,1,255,127,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3, 
  0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128,71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128,105, 
  112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,89,180,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,89,180,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,89,180,1,0,1,19,70,0, 
  0,0,195,0,0,0,1,0,17,1,89,180,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,89,180,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,89,180,1,0,1,19,44,0,0,0,120,0,0,0, 
  1,0,17,1,89,180,1,0,1,15,1,89,180,1,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,89,180,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,89,180,1,0,1,19,14, 
  0,0,0,38,0,0,0,3,0,1,15,1,89,180,1,0,17,1,90,137,1,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,89,180,1,0,1,15,1,89,180,1,0,17,1,144,181,1,0,1,1,2,21,1,47,0, 
  0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,204,181,1,0,1,8,19,14,0,0,0,41,0,0,0,3,0,14,1,21,1,15,1, 
  0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88, 
  68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,220,182,1,0,17,1,167,200,1,0,1,4,15,1,220,182,1,0,17,1,194,199,1,0,1,4,15,1,220,182,1,0,17,1,221,198,1,0, 
  1,4,15,1,220,182,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,220,182,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,220,182,1,0,1,4,19,73,0,0,0,201, 
  0,0,0,1,0,17,1,220,182,1,0,1,4,15,1,220,182,1,0,17,1,133,111,1,0,1,4,15,1,220,182,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,220,182,1,0,1,4, 
  15,1,220,182,1,0,17,1,191,110,1,0,1,4,15,1,220,182,1,0,17,1,10,132,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,192,71,131,73,240,68,131,90,240,3,128,43,136, 
  4,130,44,184,8,128,38,144,195,128,14,128,196,127,15,128,132,128,70,112,2,128,71,160,70,128,95,128,133,128,99,16,6,128,103,40,8,128,104,0,3,128,105,48,7,128,19,44,0,0,0,118,0,0,0,1,0,17,1,220, 
  182,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,220,182,1,0,1,19,15,0,0,0,51,0,0,0,3,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,220,182,1,0,1,1,15,1,220,182,1,0,17, 
  1,183,134,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,220,182,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,220,182,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,220,182,1,0,1, 
  19,44,0,0,0,119,0,0,0,1,0,17,1,220,182,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,220,182,1,0,1,15,1,220,182,1,0,17,1,166,136,1,0,1,19,72,0,0,0,199,0,0,0,1,0, 
  17,1,220,182,1,0,1,19,14,0,0,0,37,0,0,0,5,0,14,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,98,128,129,128,95,224,1,128,126,240,0,128,19,98,0,0,0,34,1,0,0, 
  1,0,17,1,1,184,1,0,1,19,95,0,0,0,24,1,0,0,4,0,1,1,2,21,1,71,0,0,0,27,67,2,0,6,0,0,0,2,0,0,0,6,80,1,129,1,80,1,128,2,80,129,127,7,80,129,128,10,200,1, 
  128,31,88,1,128,8,4,15,1,159,198,1,0,17,1,36,186,1,0,1,4,15,1,159,198,1,0,17,1,135,184,1,0,1,2,21,1,107,0,0,0,117,63,2,0,8,0,0,0,3,0,0,0,10,232,66,129,1,144,1, 
  128,2,144,129,127,131,152,1,128,31,120,2,128,130,8,2,128,6,144,1,128,7,144,65,127,8,4,15,1,243,184,1,0,17,1,56,153,1,0,1,4,15,1,243,184,1,0,17,1,251,139,1,0,1,4,15,1,243,184,1, 
  0,17,1,36,186,1,0,1,4,15,1,243,184,1,0,17,1,90,113,1,0,1,2,21,0,106,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,124,80,2,128,101,232,194,128,102,224,2,128,127,48,1,128,125,192,1, 
  128,19,101,0,0,0,40,1,0,0,1,0,17,1,243,184,1,0,1,19,101,0,0,0,41,1,0,0,1,0,17,1,243,184,1,0,1,19,101,0,0,0,42,1,0,0,1,0,17,1,243,184,1,0,1,1,15,1,243,184, 
  1,0,17,1,94,185,1,0,1,2,21,1,125,0,0,0,153,66,2,0,9,0,0,0,3,0,0,0,10,120,67,129,1,176,1,128,2,176,129,127,11,8,67,129,31,152,2,128,130,40,2,128,6,176,1,128,7,176,65,127, 
  131,184,1,128,8,4,15,1,220,185,1,0,17,1,56,153,1,0,1,4,15,1,220,185,1,0,17,1,251,139,1,0,1,4,15,1,220,185,1,0,17,1,36,186,1,0,1,4,19,102,0,0,0,47,1,0,0,3,0,14, 
  1,4,15,1,220,185,1,0,17,1,90,113,1,0,1,2,21,0,71,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,208,1,128,101,48,66,128,125,112,1,128,127,16,1,128,19,101,0,0,0,43,1,0,0,2, 
  0,1,19,101,0,0,0,44,1,0,0,2,0,1,19,101,0,0,0,45,1,0,0,2,0,1,1,2,21,1,135,0,0,0,234,67,2,0,9,0,0,0,3,0,0,0,112,232,2,128,1,176,1,129,2,176,1,128,110,88, 
  67,129,76,200,3,128,129,80,2,128,6,176,65,127,7,176,1,128,134,184,1,128,8,4,19,128,0,0,0,111,1,0,0,1,0,17,1,241,186,1,0,1,4,19,120,0,0,0,101,1,0,0,1,0,17,1,241,186,1,0, 
  1,4,15,1,241,186,1,0,17,1,214,126,0,0,1,4,15,1,241,186,1,0,17,1,172,186,1,0,1,4,15,1,241,186,1,0,17,1,40,12,0,0,1,2,21,1,68,0,0,0,10,7,2,0,4,0,0,0,2,0, 
  0,0,41,24,1,128,1,16,193,127,2,16,1,128,31,176,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,55,17,1,0,1,4,15,1,55,17,1,0,17,1,116,10,0,0,1,2,21,0,150,0,0,0,255, 
  255,255,255,7,0,0,0,2,0,0,0,108,184,67,129,109,40,3,128,74,72,132,128,127,0,2,128,110,152,2,128,120,8,66,128,128,112,1,128,19,108,0,0,0,55,1,0,0,1,0,17,1,241,186,1,0,1,1,19,108, 
  0,0,0,54,1,0,0,1,0,17,1,241,186,1,0,1,19,74,0,0,0,207,0,0,0,1,0,17,1,241,186,1,0,1,19,74,0,0,0,206,0,0,0,1,0,17,1,241,186,1,0,1,19,74,0,0,0,205,0,0, 
  0,1,0,17,1,241,186,1,0,1,15,1,241,186,1,0,17,1,136,187,1,0,1,2,21,1,47,0,0,0,144,248,1,0,5,0,0,0,2,0,0,0,32,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48, 
  1,128,8,4,17,1,184,187,1,0,1,2,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1,128,8,4,17,1,232,187,1,0,1,2, 
  21,1,69,1,0,0,54,230,1,0,19,0,0,0,4,0,0,0,49,176,8,128,1,240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7,128,6,240,2,128,7,240,66,127,74,80,5,128,73,232,5,128,58,240, 
  134,127,59,128,70,129,108,104,3,128,109,248,2,128,46,184,9,128,47,72,9,128,75,184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,46,189,1,0,17,1,167,200,1,0,1,4,15,1,46,189,1,0,17,1,194,199, 
  1,0,1,4,15,1,46,189,1,0,17,1,221,198,1,0,1,4,15,1,46,189,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,46,189,1,0,1,4,19,73,0,0,0,202,0,0,0, 
  1,0,17,1,46,189,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,46,189,1,0,1,4,15,1,46,189,1,0,17,1,72,230,0,0,1,4,15,1,46,189,1,0,17,1,2,230,0,0,1,4,15,1,46, 
  189,1,0,17,1,106,229,0,0,1,4,15,1,46,189,1,0,17,1,133,111,1,0,1,4,15,1,46,189,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,46,189,1,0,1,4,15,1, 
  46,189,1,0,17,1,191,110,1,0,1,4,15,1,46,189,1,0,17,1,53,174,1,0,1,2,21,0,94,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,70,160,7,128,99,104,4,128,103,216,3,128,19,32,138,127, 
  104,72,3,128,105,184,2,128,38,144,137,126,71,16,199,126,72,168,6,127,73,24,6,127,90,136,5,128,43,40,9,128,44,152,8,128,45,48,8,128,14,136,10,128,95,248,68,128,127,176,2,128,1,19,71,0,0,0,198,0, 
  0,0,1,0,17,1,46,189,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,46,189,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,46,189,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1, 
  46,189,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,46,189,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,46,189,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,46,189,1,0,1,15, 
  1,46,189,1,0,17,1,97,123,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,46,189,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,46,189,1,0,1,15,1,46,189,1,0,17,1,51,193,1,0, 
  1,19,14,0,0,0,48,0,0,0,1,0,17,1,46,189,1,0,1,15,1,46,189,1,0,17,1,21,124,1,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,46,189,1,0,1,15,1,46,189,1,0,17,1,196,190, 
  1,0,1,15,1,46,189,1,0,17,1,141,190,1,0,1,2,21,1,54,0,0,0,156,65,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,19,127,0, 
  0,0,110,1,0,0,6,0,14,14,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,255,190,1,0,1,8,19, 
  14,0,0,0,46,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66, 
  127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,15,192,1,0,17,1,167,200,1,0,1,4,15,1,15,192,1,0,17,1,194,199,1, 
  0,1,4,15,1,15,192,1,0,17,1,221,198,1,0,1,4,15,1,15,192,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,15,192,1,0,1,4,19,73,0,0,0,202,0,0,0,1, 
  0,17,1,15,192,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,15,192,1,0,1,4,15,1,15,192,1,0,17,1,133,111,1,0,1,4,15,1,15,192,1,0,17,1,34,111,1,0,1,4,19,38,0,0, 
  0,112,0,0,0,1,0,17,1,15,192,1,0,1,4,15,1,15,192,1,0,17,1,191,110,1,0,1,4,15,1,15,192,1,0,17,1,227,90,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0, 
  0,72,176,69,131,73,136,68,131,90,144,3,128,19,168,69,129,44,120,6,128,38,24,6,129,14,168,197,127,71,248,199,128,43,32,196,128,70,216,6,128,95,24,133,128,99,136,8,128,103,112,2,128,104,104,7,128,105,0,3, 
  128,19,72,0,0,0,199,0,0,0,1,0,17,1,15,192,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,15,192,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,15,192,1,0,1,15,1,15,192,1, 
  0,17,1,116,121,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,15,192,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,15,192,1,0,1,1,15,1,15,192,1,0,17,1,71,139,1,0,1,19,19, 
  0,0,0,56,0,0,0,3,0,1,19,14,0,0,0,44,0,0,0,3,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,15,192,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,15,192,1,0,1,19,44, 
  0,0,0,119,0,0,0,1,0,17,1,15,192,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,15,192,1,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193, 
  128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,110,193,1,0,1,8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127, 
  2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1, 
  126,194,1,0,17,1,167,200,1,0,1,4,15,1,126,194,1,0,17,1,194,199,1,0,1,4,15,1,126,194,1,0,17,1,221,198,1,0,1,4,15,1,126,194,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203, 
  0,0,0,1,0,17,1,126,194,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,126,194,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,126,194,1,0,1,4,15,1,126,194,1,0,17,1,133, 
  111,1,0,1,4,15,1,126,194,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,126,194,1,0,1,4,15,1,126,194,1,0,17,1,191,110,1,0,1,4,15,1,126,194,1,0,17,1, 
  255,127,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128, 
  71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,126,194,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,126,194,1,0,1, 
  19,72,0,0,0,199,0,0,0,1,0,17,1,126,194,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,126,194,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,126,194,1,0,1,19,70,0,0,0,193, 
  0,0,0,1,0,17,1,126,194,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,126,194,1,0,1,15,1,126,194,1,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,126,194,1,0, 
  1,19,44,0,0,0,118,0,0,0,1,0,17,1,126,194,1,0,1,19,14,0,0,0,38,0,0,0,3,0,1,15,1,126,194,1,0,17,1,99,198,1,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,126,194,1, 
  0,1,15,1,126,194,1,0,17,1,181,195,1,0,1,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,241,195, 
  1,0,1,8,19,14,0,0,0,41,0,0,0,3,0,14,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6, 
  144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,1,197,1,0,17,1,167,200,1,0,1,4,15,1,1,197,1, 
  0,17,1,194,199,1,0,1,4,15,1,1,197,1,0,17,1,221,198,1,0,1,4,15,1,1,197,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,1,197,1,0,1,4,19,73,0,0, 
  0,202,0,0,0,1,0,17,1,1,197,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,1,197,1,0,1,4,15,1,1,197,1,0,17,1,133,111,1,0,1,4,15,1,1,197,1,0,17,1,34,111,1,0, 
  1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,1,197,1,0,1,4,15,1,1,197,1,0,17,1,191,110,1,0,1,4,15,1,1,197,1,0,17,1,10,132,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15, 
  0,0,0,3,0,0,0,72,112,67,131,73,168,70,131,90,144,8,128,43,120,2,130,44,56,7,128,38,48,200,128,14,112,194,127,15,112,130,128,70,160,7,128,71,224,66,128,95,24,134,128,99,248,4,128,103,136,5,128,104, 
  104,4,128,105,216,3,128,1,15,1,1,197,1,0,17,1,38,198,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,1,197,1,0,1,15,1,1,197,1,0,17,1,166,136,1,0,1,19,71,0,0,0,198,0,0, 
  0,1,0,17,1,1,197,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,1,197,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,1,197,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,1, 
  197,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,1,197,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,1,197,1,0,1,19,14,0,0,0,37,0,0,0,5,0,14,1,19,44,0,0,0,118,0, 
  0,0,1,0,17,1,1,197,1,0,1,19,15,0,0,0,51,0,0,0,3,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,1,197,1,0,1,2,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0,0, 
  6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,244,134,1,0,1,19,14,0,0,0,39,0,0,0,5,0,14,14,1,21,1,47,0,0,0,173,108,2,0,5,0,0,0,2,0,0, 
  0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,150,137,1,0,1,8,19,14,0,0,0,40,0,0,0,3,0,14,1,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,102,128,1,128,99,224,65,128,127,240,0,128,19,102,0,0,0,46,1,0,0,1,0,17,1,159,198,1,0,1,19,99,0,0,0,36,1,0,0,4,0,1,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2, 
  0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,42,199,1,0,1,4,15,1,42,199,1,0,17,1,116,10,0,0, 
  1,2,21,0,100,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,37,40,130,128,13,144,194,127,61,200,1,128,79,56,65,128,103,48,1,128,1,19,61,0,0,0,167,0,0,0,1,0,17,1,42,199,1,0,1,19, 
  103,0,0,0,48,1,0,0,2,0,1,15,1,42,199,1,0,17,1,143,199,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,42,199,1,0,1,2,21,1,39,0,0,0,97,109,2,0,3,0,0,0,1,0,0, 
  0,2,48,1,128,1,48,65,128,79,240,0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129, 
  2,80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,15,200,1,0,1,4,15,1,15,200,1,0,17,1,116,10,0,0,1,2,21,0,100,0,0,0,255,255,255, 
  255,5,0,0,0,2,0,0,0,104,48,1,128,13,144,66,128,37,40,130,128,79,56,1,128,61,200,1,128,1,19,61,0,0,0,167,0,0,0,1,0,17,1,15,200,1,0,1,19,104,0,0,0,49,1,0,0,2,0,1, 
  15,1,15,200,1,0,17,1,116,200,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,15,200,1,0,1,2,21,1,39,0,0,0,182,109,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,79,248, 
  0,128,8,4,17,1,206,8,0,0,1,19,13,0,0,0,34,0,0,0,1,0,1,21,1,47,0,0,0,240,65,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,10,56,1, 
  128,8,4,17,1,215,200,1,0,1,2,21,1,76,0,0,0,122,67,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,31,240,1,128,41,88,1,128,8,4,19,37,0,0,0, 
  101,0,0,0,1,0,17,1,36,201,1,0,1,4,15,1,36,201,1,0,17,1,116,10,0,0,1,2,21,0,145,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,37,144,3,129,13,248,195,127,106,216,1,128,79,112, 
  194,128,61,0,67,128,105,104,2,128,107,112,1,128,15,1,36,201,1,0,17,1,182,201,1,0,1,19,107,0,0,0,52,1,0,0,1,0,17,1,36,201,1,0,1,1,19,61,0,0,0,167,0,0,0,1,0,17,1,36, 
  201,1,0,1,19,106,0,0,0,51,1,0,0,1,0,17,1,36,201,1,0,1,15,1,36,201,1,0,17,1,235,4,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,36,201,1,0,1,2,21,1,94,0,0,0, 
  143,70,2,0,7,0,0,0,2,0,0,0,6,112,1,128,1,112,65,129,2,112,129,127,7,112,65,128,11,128,66,128,31,16,2,128,41,120,1,128,8,4,19,37,0,0,0,101,0,0,0,1,0,17,1,21,202,1,0,1, 
  4,15,1,21,202,1,0,17,1,116,10,0,0,1,4,19,105,0,0,0,50,1,0,0,4,0,14,1,2,21,0,122,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,37,216,2,129,13,64,195,127,106,88,1,128,79, 
  184,129,128,61,72,2,128,107,80,1,128,1,19,107,0,0,0,53,1,0,0,2,0,1,19,61,0,0,0,167,0,0,0,1,0,17,1,21,202,1,0,1,19,106,0,0,0,51,1,0,0,1,0,17,1,21,202,1,0,1, 
  15,1,21,202,1,0,17,1,144,202,1,0,1,19,61,0,0,0,166,0,0,0,1,0,17,1,21,202,1,0,1,2,21,1,39,0,0,0,20,70,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,79,240, 
  0,128,4,17,1,206,8,0,0,1,8,19,13,0,0,0,34,0,0,0,1,0,1,21,0,93,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,0,224,10,128,70,152,7,128,99,96,4,128,19,24,202,127,103,208,3, 
  128,104,64,3,128,38,136,201,126,71,8,71,127,72,160,70,127,73,16,198,129,90,128,5,128,43,32,9,128,44,144,8,128,45,40,8,128,14,128,10,128,95,240,4,128,105,176,2,128,19,71,0,0,0,198,0,0,0,1,0, 
  17,1,195,202,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,195,202,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,195,202,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,195,202,1,0, 
  1,19,70,0,0,0,194,0,0,0,1,0,17,1,195,202,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,195,202,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,195,202,1,0,1,15,1,195,202,1, 
  0,17,1,97,123,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,195,202,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,195,202,1,0,1,15,1,195,202,1,0,17,1,7,207,1,0,1,19,14,0, 
  0,0,48,0,0,0,1,0,17,1,195,202,1,0,1,15,1,195,202,1,0,17,1,204,206,1,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,195,202,1,0,1,15,1,195,202,1,0,17,1,33,204,1,0,1,19, 
  0,0,0,0,0,0,0,0,3,0,1,1,2,21,1,47,0,0,0,255,109,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,92,204,1,0,1,8, 
  19,14,0,0,0,46,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144, 
  66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,108,205,1,0,17,1,167,200,1,0,1,4,15,1,108,205,1,0,17,1,194,199, 
  1,0,1,4,15,1,108,205,1,0,17,1,221,198,1,0,1,4,15,1,108,205,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,108,205,1,0,1,4,19,73,0,0,0,202,0,0,0, 
  1,0,17,1,108,205,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,108,205,1,0,1,4,15,1,108,205,1,0,17,1,133,111,1,0,1,4,15,1,108,205,1,0,17,1,34,111,1,0,1,4,19,38,0, 
  0,0,112,0,0,0,1,0,17,1,108,205,1,0,1,4,15,1,108,205,1,0,17,1,191,110,1,0,1,4,15,1,108,205,1,0,17,1,227,90,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0, 
  0,0,72,176,72,131,73,40,68,131,90,0,3,128,19,32,68,129,44,80,8,128,38,240,7,129,14,32,196,127,71,208,198,128,43,104,198,128,70,184,4,128,95,72,133,128,99,216,5,128,103,144,3,128,104,96,7,128,105,112, 
  2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,108,205,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,108,205,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,108,205,1,0,1,1,19,44,0, 
  0,0,120,0,0,0,1,0,17,1,108,205,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,108,205,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,108,205,1,0,1,19,70,0,0,0,195,0,0,0, 
  1,0,17,1,108,205,1,0,1,15,1,108,205,1,0,17,1,144,206,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,108,205,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,108,205,1,0,1,19,19, 
  0,0,0,56,0,0,0,3,0,1,19,14,0,0,0,44,0,0,0,3,0,1,15,1,108,205,1,0,17,1,71,139,1,0,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48, 
  193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,176,121,1,0,1,19,14,0,0,0,45,0,0,0,3,0,14,1,21,1,47,0,0,0,255,109,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112, 
  193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,80,124,1,0,1,8,19,14,0,0,0,49,0,0,0,1,0,1,21,1,47,0,0,0,255,109,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193, 
  128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,66,207,1,0,1,8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127, 
  2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1, 
  82,208,1,0,17,1,167,200,1,0,1,4,15,1,82,208,1,0,17,1,194,199,1,0,1,4,15,1,82,208,1,0,17,1,221,198,1,0,1,4,15,1,82,208,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203, 
  0,0,0,1,0,17,1,82,208,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,82,208,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,82,208,1,0,1,4,15,1,82,208,1,0,17,1,133, 
  111,1,0,1,4,15,1,82,208,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,82,208,1,0,1,4,15,1,82,208,1,0,17,1,191,110,1,0,1,4,15,1,82,208,1,0,17,1, 
  255,127,1,0,1,2,21,0,54,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128, 
  71,200,70,128,95,176,132,128,99,32,4,128,103,144,3,128,104,0,3,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,82,208,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,82,208,1,0,1, 
  19,72,0,0,0,199,0,0,0,1,0,17,1,82,208,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,82,208,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,82,208,1,0,1,19,70,0,0,0,193, 
  0,0,0,1,0,17,1,82,208,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,82,208,1,0,1,15,1,82,208,1,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,82,208,1,0, 
  1,19,44,0,0,0,118,0,0,0,1,0,17,1,82,208,1,0,1,19,14,0,0,0,38,0,0,0,3,0,1,15,1,82,208,1,0,17,1,55,212,1,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,82,208,1, 
  0,1,15,1,82,208,1,0,17,1,137,209,1,0,1,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,197, 
  209,1,0,1,19,14,0,0,0,41,0,0,0,3,0,14,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6, 
  144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,213,210,1,0,17,1,167,200,1,0,1,4,15,1,213,210,1, 
  0,17,1,194,199,1,0,1,4,15,1,213,210,1,0,17,1,221,198,1,0,1,4,15,1,213,210,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,213,210,1,0,1,4,19,73,0,0, 
  0,202,0,0,0,1,0,17,1,213,210,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,213,210,1,0,1,4,15,1,213,210,1,0,17,1,133,111,1,0,1,4,15,1,213,210,1,0,17,1,34,111,1,0, 
  1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,213,210,1,0,1,4,15,1,213,210,1,0,17,1,191,110,1,0,1,4,15,1,213,210,1,0,17,1,10,132,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15, 
  0,0,0,3,0,0,0,72,240,67,131,73,96,68,131,90,120,6,128,43,16,6,130,44,184,8,128,38,112,194,128,14,88,196,127,15,88,132,128,70,208,2,128,71,40,72,128,95,8,135,128,99,128,5,128,103,152,7,128,104, 
  96,3,128,105,240,4,128,19,15,0,0,0,51,0,0,0,3,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,213,210,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,213,210,1,0,1,15,1,213,210,1, 
  0,17,1,166,136,1,0,1,1,19,44,0,0,0,120,0,0,0,1,0,17,1,213,210,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,213,210,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,213,210, 
  1,0,1,15,1,213,210,1,0,17,1,250,211,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,213,210,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,213,210,1,0,1,19,72,0,0,0,199,0,0, 
  0,1,0,17,1,213,210,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,213,210,1,0,1,19,14,0,0,0,37,0,0,0,5,0,14,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0, 
  6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,244,134,1,0,1,19,14,0,0,0,39,0,0,0,5,0,14,14,1,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0, 
  0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,150,137,1,0,1,19,14,0,0,0,40,0,0,0,3,0,14,1,21,1,69,1,0,0,54,230,1,0,19,0,0,0,4,0,0, 
  0,49,176,8,128,1,240,194,127,2,240,66,128,50,64,8,128,55,96,71,131,53,208,7,128,6,240,2,128,7,240,66,127,74,80,5,128,73,232,5,128,58,240,134,127,59,128,70,129,108,104,3,128,109,248,2,128,46,184,9, 
  128,47,72,9,128,75,184,132,128,103,72,4,128,107,216,3,128,8,4,15,1,185,213,1,0,17,1,167,200,1,0,1,4,15,1,185,213,1,0,17,1,194,199,1,0,1,4,15,1,185,213,1,0,17,1,221,198,1,0,1, 
  4,15,1,185,213,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,185,213,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,185,213,1,0,1,4,19,73,0,0,0,201,0, 
  0,0,1,0,17,1,185,213,1,0,1,4,15,1,185,213,1,0,17,1,72,230,0,0,1,4,15,1,185,213,1,0,17,1,2,230,0,0,1,4,15,1,185,213,1,0,17,1,106,229,0,0,1,4,15,1,185,213,1,0, 
  17,1,133,111,1,0,1,4,15,1,185,213,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,185,213,1,0,1,4,15,1,185,213,1,0,17,1,191,110,1,0,1,4,15,1,185,213,1, 
  0,17,1,227,90,1,0,1,2,21,0,93,1,0,0,255,255,255,255,17,0,0,0,4,0,0,0,0,224,10,128,70,152,7,128,99,96,4,128,19,24,202,127,103,208,3,128,104,64,3,128,38,136,201,126,71,8,71,127,72, 
  160,70,127,73,16,198,129,90,128,5,128,43,32,9,128,44,144,8,128,45,40,8,128,14,128,10,128,95,240,4,128,105,176,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,185,213,1,0,1,19,71,0,0,0,197,0, 
  0,0,1,0,17,1,185,213,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,185,213,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,185,213,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1, 
  185,213,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,185,213,1,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,185,213,1,0,1,15,1,185,213,1,0,17,1,121,222,1,0,1,19,44,0,0,0,119, 
  0,0,0,1,0,17,1,185,213,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,185,213,1,0,1,15,1,185,213,1,0,17,1,134,217,1,0,1,19,14,0,0,0,48,0,0,0,1,0,17,1,185,213,1,0, 
  1,15,1,185,213,1,0,17,1,204,206,1,0,1,19,19,0,0,0,55,0,0,0,1,0,17,1,185,213,1,0,1,15,1,185,213,1,0,17,1,23,215,1,0,1,19,0,0,0,0,1,0,0,0,3,0,1,1,2,21, 
  1,47,0,0,0,255,109,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,82,215,1,0,1,19,14,0,0,0,46,0,0,0,1,0,1,21,1, 
  15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128, 
  75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,98,216,1,0,17,1,167,200,1,0,1,4,15,1,98,216,1,0,17,1,194,199,1,0,1,4,15,1,98,216,1,0,17,1,221,198, 
  1,0,1,4,15,1,98,216,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,98,216,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,98,216,1,0,1,4,19,73,0,0, 
  0,201,0,0,0,1,0,17,1,98,216,1,0,1,4,15,1,98,216,1,0,17,1,133,111,1,0,1,4,15,1,98,216,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,98,216,1,0, 
  1,4,15,1,98,216,1,0,17,1,191,110,1,0,1,4,15,1,98,216,1,0,17,1,227,90,1,0,1,2,21,0,35,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,32,69,131,73,232,69,131,90,120,6,128, 
  19,176,68,129,44,136,5,128,38,40,8,129,14,176,196,127,71,144,195,128,43,184,196,128,70,8,7,128,95,32,132,128,99,136,8,128,103,112,2,128,104,0,3,128,105,152,7,128,19,72,0,0,0,199,0,0,0,1,0,17, 
  1,98,216,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,98,216,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,98,216,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,98,216,1,0,1, 
  1,15,1,98,216,1,0,17,1,144,206,1,0,1,15,1,98,216,1,0,17,1,71,139,1,0,1,19,14,0,0,0,44,0,0,0,3,0,1,19,44,0,0,0,120,0,0,0,1,0,17,1,98,216,1,0,1,19,70,0, 
  0,0,193,0,0,0,1,0,17,1,98,216,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,98,216,1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,98,216,1,0,1,19,19,0,0,0,56,0,0,0, 
  3,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,98,216,1,0,1,2,21,1,47,0,0,0,255,109,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128, 
  4,17,1,193,217,1,0,1,8,19,14,0,0,0,42,0,0,0,1,0,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53, 
  32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136,5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,209,218,1,0,17,1,167,200,1,0,1,4,15, 
  1,209,218,1,0,17,1,194,199,1,0,1,4,15,1,209,218,1,0,17,1,221,198,1,0,1,4,15,1,209,218,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,209,218,1,0,1,4, 
  19,73,0,0,0,202,0,0,0,1,0,17,1,209,218,1,0,1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,209,218,1,0,1,4,15,1,209,218,1,0,17,1,133,111,1,0,1,4,15,1,209,218,1,0,17,1, 
  34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0,17,1,209,218,1,0,1,4,15,1,209,218,1,0,17,1,191,110,1,0,1,4,15,1,209,218,1,0,17,1,255,127,1,0,1,2,21,0,54,1,0,0,255, 
  255,255,255,15,0,0,0,3,0,0,0,72,96,70,131,73,208,69,131,90,64,5,128,43,72,8,130,44,232,7,128,38,176,200,128,14,168,201,127,15,64,137,128,70,88,7,128,71,200,70,128,95,176,132,128,99,32,4,128,103, 
  144,3,128,104,0,3,128,105,112,2,128,19,71,0,0,0,198,0,0,0,1,0,17,1,209,218,1,0,1,19,71,0,0,0,197,0,0,0,1,0,17,1,209,218,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1, 
  209,218,1,0,1,19,70,0,0,0,195,0,0,0,1,0,17,1,209,218,1,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,209,218,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,209,218,1,0,1,19, 
  44,0,0,0,120,0,0,0,1,0,17,1,209,218,1,0,1,15,1,209,218,1,0,17,1,83,228,0,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,209,218,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17, 
  1,209,218,1,0,1,19,14,0,0,0,38,0,0,0,3,0,1,15,1,209,218,1,0,17,1,55,212,1,0,1,19,15,0,0,0,50,0,0,0,1,0,17,1,209,218,1,0,1,15,1,209,218,1,0,17,1,8,220,1, 
  0,1,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,68,220,1,0,1,19,14,0,0,0,41,0,0,0, 
  3,0,14,1,21,1,15,1,0,0,82,51,2,0,16,0,0,0,4,0,0,0,49,0,7,128,1,144,194,127,2,144,66,128,50,144,6,128,103,232,3,128,53,32,6,128,6,144,2,128,7,144,66,127,107,120,3,128,73,136, 
  5,128,74,240,4,128,75,88,68,127,108,8,3,128,109,152,2,128,46,8,8,128,47,152,7,128,8,4,15,1,84,221,1,0,17,1,167,200,1,0,1,4,15,1,84,221,1,0,17,1,194,199,1,0,1,4,15,1,84,221, 
  1,0,17,1,221,198,1,0,1,4,15,1,84,221,1,0,17,1,232,111,1,0,1,4,19,73,0,0,0,203,0,0,0,1,0,17,1,84,221,1,0,1,4,19,73,0,0,0,202,0,0,0,1,0,17,1,84,221,1,0, 
  1,4,19,73,0,0,0,201,0,0,0,1,0,17,1,84,221,1,0,1,4,15,1,84,221,1,0,17,1,133,111,1,0,1,4,15,1,84,221,1,0,17,1,34,111,1,0,1,4,19,38,0,0,0,112,0,0,0,1,0, 
  17,1,84,221,1,0,1,4,15,1,84,221,1,0,17,1,191,110,1,0,1,4,15,1,84,221,1,0,17,1,10,132,1,0,1,2,21,0,36,1,0,0,255,255,255,255,15,0,0,0,3,0,0,0,72,248,67,131,73,0, 
  72,131,90,232,5,128,43,128,5,130,44,8,7,128,38,112,194,128,14,240,195,127,15,240,131,128,70,96,3,128,71,144,72,128,95,208,130,128,99,112,7,128,103,240,4,128,104,96,4,128,105,120,6,128,19,15,0,0,0,51, 
  0,0,0,3,0,1,19,70,0,0,0,194,0,0,0,1,0,17,1,84,221,1,0,1,19,44,0,0,0,118,0,0,0,1,0,17,1,84,221,1,0,1,1,15,1,84,221,1,0,17,1,166,136,1,0,1,19,71,0,0, 
  0,197,0,0,0,1,0,17,1,84,221,1,0,1,19,72,0,0,0,199,0,0,0,1,0,17,1,84,221,1,0,1,15,1,84,221,1,0,17,1,250,211,1,0,1,19,70,0,0,0,193,0,0,0,1,0,17,1,84,221, 
  1,0,1,19,71,0,0,0,198,0,0,0,1,0,17,1,84,221,1,0,1,19,14,0,0,0,37,0,0,0,5,0,14,1,19,70,0,0,0,195,0,0,0,1,0,17,1,84,221,1,0,1,19,44,0,0,0,120,0,0, 
  0,1,0,17,1,84,221,1,0,1,19,44,0,0,0,119,0,0,0,1,0,17,1,84,221,1,0,1,2,21,1,47,0,0,0,51,53,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7, 
  48,1,128,13,56,1,128,8,4,17,1,169,222,1,0,1,2,21,1,71,0,0,0,228,53,2,0,6,0,0,0,2,0,0,0,108,88,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,107,200,1,128,8, 
  4,15,1,241,222,1,0,17,1,194,199,1,0,1,4,15,1,241,222,1,0,17,1,221,198,1,0,1,2,21,0,59,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,72,208,129,128,103,112,1,128,104,16,1,128,71, 
  208,129,127,19,71,0,0,0,196,0,0,0,3,0,1,19,72,0,0,0,200,0,0,0,3,0,1,1,2,15,1,211,8,1,0,17,1,58,223,1,0,1,21,1,64,0,0,0,147,110,2,0,2,0,0,0,1,0,0,0, 
  28,104,1,128,29,208,0,128,4,19,33,0,0,0,86,0,0,0,1,0,17,1,159,223,1,0,1,4,19,32,0,0,0,85,0,0,0,1,0,17,1,123,223,1,0,1,2,21,1,35,0,0,0,99,113,2,0,1,0,0, 
  0,0,0,0,0,8,176,0,128,4,19,8,0,0,0,26,0,0,0,2,0,1,2,21,1,35,0,0,0,99,113,2,0,1,0,0,0,0,0,0,0,8,176,0,128,4,19,8,0,0,0,27,0,0,0,2,0,1,2,15, 
  1,211,8,1,0,17,1,208,223,1,0,1,21,1,61,3,0,0,69,27,2,0,27,0,0,0,4,0,0,0,96,168,77,134,1,240,3,132,2,240,131,131,99,56,11,128,84,152,150,132,101,32,9,128,86,200,85,132,87,248, 
  20,128,88,40,20,128,89,88,19,128,90,136,18,128,91,184,17,128,92,232,16,128,93,24,16,128,94,72,15,128,31,120,25,129,34,8,153,128,81,56,216,128,82,104,215,128,95,120,78,129,97,216,140,129,98,8,12,128,100,104, 
  10,128,102,216,7,128,127,56,7,128,128,152,5,128,129,248,3,128,8,4,19,120,0,0,0,101,1,0,0,1,0,19,87,0,0,0,6,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0, 
  0,0,1,0,17,1,63,204,0,0,1,4,19,119,0,0,0,100,1,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,63,204, 
  0,0,1,4,15,1,63,204,0,0,15,1,183,203,0,0,17,1,99,203,0,0,1,4,19,87,0,0,0,4,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,63, 
  204,0,0,1,4,19,87,0,0,0,3,1,0,0,1,0,19,64,0,0,0,178,0,0,0,1,0,19,34,0,0,0,95,0,0,0,1,0,17,1,63,204,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1, 
  87,203,0,0,17,1,244,202,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,157,202,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,70,202, 
  0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,239,201,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,152,201,0,0,1,4,15,1,63,204, 
  0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,65,201,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,234,200,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0, 
  15,1,87,203,0,0,17,1,147,200,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,60,200,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1, 
  229,199,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,142,199,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,55,199,0,0,1,4,15,1, 
  63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,224,198,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,87,203,0,0,17,1,137,198,0,0,1,4,15,1,63,204,0,0,15,1,75,203, 
  0,0,15,1,125,198,0,0,17,1,38,198,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,26,198,0,0,17,1,7,196,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,251,195,0,0, 
  17,1,172,192,0,0,1,4,15,1,63,204,0,0,15,1,75,203,0,0,15,1,160,192,0,0,17,1,65,184,0,0,1,4,15,1,63,204,0,0,17,1,197,177,0,0,1,4,15,1,63,204,0,0,17,1,73,171,0,0, 
  1,2,15,1,211,8,1,0,17,1,27,227,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,17,1,174,146,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47,48,129,128,45,240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,198,227,1,0,1,21, 
  2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,18,2,0,0,0,1, 
  18,1,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,25,229,1,0,1,3,17,1,241,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,42,176,0,128,3,17,1,70,228,1,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1, 
  196,228,1,0,1,3,17,1,196,228,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,8,1,128,47,208,0,128,3,18,7,0,0,0,1,3,17,1,166,228,1,0,1,21,2,54,0,0, 
  0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,196,228,1,0,1,3,17,1,196,228,1,0,1,2,21,4,29,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,47,176,0,128,3,18,7,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,166,228,1,0,1,21,2,54,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,196,228,1,0,1,3,17,1,196,228,1,0,1,2,21,2,53,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,25,229,1,0,1,3,18,6,0,0,0,1,2,18,41,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,4,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,61,208,0,128,47,16,193,127,3,17,1,238,229,1,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1, 
  128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,208,0,128,33,8,1,128,3,18,3,0,0,0,1,3,17,1,24,230,1,0,1,2, 
  21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,4,0,0,0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128, 
  115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,228,234,1,0,1,3,17,1,1,234,1,0,1,3,17,1,233,232,1,0,1,3,17,1,110,232,1,0,1,3,17,1,18,232,1,0,1, 
  3,17,1,109,231,1,0,1,3,17,1,211,230,1,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17, 
  1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,242,230,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128, 
  3,17,1,17,231,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,48,231,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176, 
  0,128,3,17,1,79,231,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,75,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,111, 
  208,0,128,97,16,193,127,3,17,1,213,231,1,0,1,3,17,1,152,231,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,183,231,1,0,1,2,21,4,29,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,18,73,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,17,1,244,231,1,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,18,109,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,49,232,1,0,1,2,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,80,232,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,18,108,0,0,0,1,2,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,141,232,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,172,232,1,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,203,232,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,103,0,0,0,1, 
  2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,101,144,129,128,97,208,193,127,117,16,1,128,111,80,1,128,3,17,1,196,233,1,0,1,3,17,1,166,233,1,0,1,3,17,1,105,233,1,0,1,3, 
  17,1,44,233,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,75,233,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0, 
  128,3,18,74,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,136,233,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176, 
  0,128,3,18,55,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,53,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176, 
  0,128,3,17,1,227,233,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,107,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101, 
  176,0,128,3,17,1,32,234,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,100,16,1,128,115,208,0,128,3,17,1,167,234,1,0,1,3,17,1,75,234,1,0,1,2,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,106,234,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,137,234,1,0,1,2,21,4,29, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,46,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,198,234,1,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,58,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,104,208,0,128,101,16,1,128,3,17,1,146,236,1, 
  0,1,3,17,1,15,235,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,46,235,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  45,176,0,128,3,17,1,77,235,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,65,128,116,208,0,128,3,17,1,212,235,1,0,1,3,17,1,120,235,1,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,151,235,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,182,235,1,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,49,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,243,235,1,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,18,236,1,0,1,2,18,47,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1, 
  54,236,1,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,85,236,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3, 
  17,1,116,236,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,50,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128, 
  3,17,1,177,236,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,208,236,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176, 
  0,128,3,18,59,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,198,227,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112, 
  129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,67,237,1,0,1,2,18,72,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176, 
  0,128,3,17,1,67,237,1,0,1,1,21,4,69,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,176,1,128,45,112,1,128,95,112,129,128,47,232,193,127,115,48,1,128,3,17,1,239,237,1,0,1,3,17,1, 
  79,229,1,0,1,3,18,31,0,0,0,1,3,17,1,198,227,1,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,192,227,1, 
  0,1,3,17,1,186,227,1,0,1,3,17,1,67,237,1,0,1,3,17,1,79,229,1,0,1,2,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,121,240, 
  0,128,3,17,1,69,238,1,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4, 
  46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,109,48,1,128,3,17,1,79,229,1,0,1,3,17,1,155,238,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,98,48,1,128,45,240,64,128,95,240,0,128,3,17,1,79,229,1, 
  0,1,3,17,1,241,238,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,111,240,0,128,3,17,1,71,239,1,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208, 
  0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,108,240,0,128,45,48,65,128,95,48,1,128,3,17,1,157,239,1,0,1,3,17,1,79,229,1, 
  0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  95,48,129,128,45,48,193,127,115,240,0,128,3,17,1,243,239,1,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1, 
  0,1,1,18,48,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,4,127,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,0,3,128,47,120,67,129,34,16,2,128,91,80,66,129,36,200,66,129,45,136,2,128,95, 
  136,2,128,39,56,131,126,99,184,3,128,116,208,1,128,3,17,1,190,242,1,0,1,3,17,1,51,242,1,0,1,3,18,82,0,0,0,1,3,17,1,79,229,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1, 
  3,17,1,72,241,1,0,1,3,17,1,198,227,1,0,1,3,17,1,243,240,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1, 
  0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,18,41,0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,40,65,128,95,40,1,128,3,18,112,0,0,0, 
  1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,92,208,0,128,39,16,1,128,3,17,1,168,241,1,0,1,3,18,134,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6, 
  48,1,128,3,17,1,211,241,1,0,1,3,17,1,211,241,1,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,211,241,1, 
  0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,168,241,1,0,1,3,18,134,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0, 
  0,0,4,112,1,128,5,48,1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,211,241,1,0,1,3,17,1,211,241,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65, 
  128,92,208,0,128,3,17,1,147,242,1,0,1,3,18,129,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17, 
  1,51,242,1,0,1,3,17,1,51,242,1,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,51,242,1,0,1,2,18,41, 
  0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,107,240,0,128,3,17,1,20,243,1,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,48,1,128,45,240,64,128,95,240,0,128,3, 
  17,1,79,229,1,0,1,3,17,1,106,243,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,110,0,0,0,21,4,29,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,3,18,76,0,0,0,1,1,21,4,206,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,192,4,128,91,24,196,131,34,160,3,128,99,0,5,128,36, 
  184,133,128,95,48,6,128,116,120,5,128,39,240,2,128,40,176,2,128,41,80,4,128,42,64,133,125,43,104,131,125,124,136,4,128,45,48,6,128,94,240,5,128,63,48,131,125,123,224,3,128,3,17,1,9,248,1,0,1,3, 
  17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1, 
  104,246,1,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,211,244,1,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,211,244,1,0,1,2,18,123,0,0,0,21,4,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,211,244,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,211, 
  244,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,107,240,0,128,3,17,1,115,245,1,0,1,3,17,1,30,246,1,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,48, 
  65,128,95,48,1,128,3,17,1,106,243,1,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0, 
  0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,40,65,128,95,40,1,128,3,18,112,0,0,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208, 
  0,128,3,17,1,182,246,1,0,1,3,17,1,147,246,1,0,1,2,18,79,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,3,18,139,0,0,0,1,1,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,213,246,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,45,0,0,0,1,2,21,4,41,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,65,128,92,208,0,128,3,17,1,83,247,1,0,1,3,18,129,0,0,0,1,21,2,54,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,48, 
  1,128,2,112,129,128,3,112,1,128,6,112,1,128,3,17,1,243,246,1,0,1,3,17,1,243,246,1,0,1,2,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193, 
  127,3,16,1,128,3,17,1,243,246,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,16,1,128,3,17,1,222,247,1,0,1,3,18,134,0,0,0,1,21,2,54,0,0, 
  0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,112,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,126,247,1,0,1,3,17,1,126,247,1,0,1,2,21,2,42,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,126,247,1,0,1,2,18,31,0,0,0,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,8,1,128, 
  43,208,0,128,3,18,136,0,0,0,1,3,18,135,0,0,0,1,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,54,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,47,208,0,128,41,16,193,127,3,17,1,198,227,1,0,1,3,18,32,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227, 
  1,0,1,3,17,1,186,227,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,124,240,0,128,41,104,65,128,47,40,1,128,3,18,66,0,0,0,1,3,17,1,198,227,1,0,1,3,18,32, 
  0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,105,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,40,80,2,128,47,200,66,129,34,208,1,128,99,8,3,128,116,144,1,128,45,16,2,128,95,16,2,128,39,136,130,126,3,17,1,190,242,1,0,1,3,17,1,51,242,1,0,1,3,17,1,79,229,1,0, 
  1,3,18,31,0,0,0,1,3,17,1,72,241,1,0,1,3,17,1,198,227,1,0,1,3,17,1,243,240,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48, 
  1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,174,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,80,2,128,42,120,196,129,34,64,195,127,43,8,67, 
  130,116,176,4,128,45,48,69,129,94,240,4,128,39,144,130,128,58,128,3,128,63,208,130,128,93,248,3,128,95,48,5,128,99,56,68,128,123,192,3,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120, 
  0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,17,1,10,251,1,0,1,3,18,10,0,0,0,1,3,17,1,200,250,1,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1, 
  29,245,1,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3, 
  17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,18,83,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,33,176,0,128,3,17,1,236,250,1,0,1,1,21,4,29,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,63,176,0,128,3,18,125,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,147,246,1,0,1,2,21,4,174,0,0,0,255, 
  255,255,255,14,0,0,0,3,0,0,0,40,80,2,128,42,120,196,129,34,64,195,127,43,8,67,130,116,176,4,128,45,48,69,129,94,240,4,128,39,144,130,128,58,184,3,128,63,208,130,128,93,248,3,128,95,48,5,128,99, 
  56,68,128,123,128,3,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,14,252,1,0,1, 
  3,17,1,200,250,1,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  58,176,0,128,3,17,1,45,252,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,3,18,139,0,0,0,1,2,21,4,93,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,40,48,130,129,45,240,1,128,34,176,1,128,39,104,66,128,95,240,65,128,99,168,2,128,116,112,1,128,3,17,1,223,252,1,0,1,3,17,1,51,242,1,0,1,3,17,1,79,229,1,0,1,3,18,31,0,0,0,1, 
  3,17,1,72,241,1,0,1,3,17,1,243,240,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1, 
  0,1,3,17,1,79,229,1,0,1,2,18,41,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,104,193,127,107,240,0,128,3,17,1,20,243,1,0,1,3,18,40,0,0,0, 
  1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,4,86,0,0,0,255,255,255,255,6,0,0,0,2,0, 
  0,0,40,112,66,128,44,200,1,128,42,56,130,128,43,0,2,128,58,136,65,128,62,80,1,128,3,18,21,0,0,0,1,3,17,1,10,251,1,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,33,0,0, 
  0,1,3,17,1,189,253,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,40,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,42,8,1,128,43,208,0,128,3,18,136,0,0,0,1,3,18,135,0,0,0,1,2,21,4,86,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,112,66,128,44,200,1,128, 
  42,56,130,128,43,0,2,128,58,136,65,128,62,80,1,128,3,18,21,0,0,0,1,3,17,1,14,252,1,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,33,0,0,0,1,3,17,1,189,253,1,0,1, 
  21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,74,0,0,0,255,255,255,255,5,0,0,0,2,0,0, 
  0,40,16,66,128,44,104,1,128,42,216,129,128,43,160,1,128,62,48,1,128,3,18,21,0,0,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,33,0,0,0,1,3,17,1,189,253,1,0,1,21,2,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,99,16, 
  1,128,41,80,1,128,34,200,1,128,39,136,65,127,3,17,1,72,255,1,0,1,3,18,32,0,0,0,1,3,17,1,72,241,1,0,1,3,17,1,51,242,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,18,112,0,0,0,1,2,21,4, 
  139,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,224,3,128,63,48,2,130,34,104,2,128,99,96,195,129,116,160,3,128,45,24,196,128,94,224,2,128,39,240,129,126,93,32,3,128,95,24,4,128,123,168,2,128, 
  3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1,3,17,1,200,250,1,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1, 
  0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227, 
  1,0,1,3,17,1,30,246,1,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,105,0,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,3,176,0,128,3,17,1,105,0,2,0,1,2,18,123,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,105,0,2,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,105,0,2,0,1,1,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,1,128,107,40,65,128,125,240,0, 
  128,3,18,11,0,0,0,1,3,17,1,30,1,2,0,1,3,18,39,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0, 
  1,3,17,1,186,227,1,0,1,3,17,1,60,1,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,119,176,0,128,3,18,138,0,0,0,1,2,18,137,0,0,0,21,2,30,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,60,1,2,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,3,18,11,0,0,0,1,21,2,54,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,60,1,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,125,208,0,128,107,8,193,127,3,18,11,0,0,0,1,3,17,1,30,1,2,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3, 
  17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,60,1,2,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,136,3,128,63,16,2,130,34,72,2,128,99,8,3,128,116,72, 
  3,128,45,192,195,128,94,136,2,128,39,208,129,126,93,200,2,128,95,192,3,128,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,17,1,243,246,1,0,1,3,17,1,40,0,2,0,1,3,17,1,200,250,1,0, 
  1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48, 
  1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,48,3,128,93,112,2,128,34,240,1,128,99,176,2, 
  128,116,240,2,128,45,104,3,127,94,48,2,128,39,176,65,128,95,104,3,128,3,17,1,126,247,1,0,1,3,17,1,243,246,1,0,1,3,17,1,40,0,2,0,1,3,17,1,200,250,1,0,1,3,17,1,201,245,1,0, 
  1,3,17,1,29,245,1,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1, 
  0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,128,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,136,3,128,93,200,2,128,34,16,2,128,99,8,131,129,116,72,3,128,45,192,3, 
  127,94,136,2,128,39,208,65,128,95,192,3,128,123,80,2,128,3,17,1,126,247,1,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1,3,17,1,200,250,1,0,1,3,17,1,201, 
  245,1,0,1,3,17,1,29,245,1,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1, 
  192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,93,208,0,128,47,16,193,127,3,17,1,200,250,1,0,1,3,17,1,198, 
  227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,172,0,0,0,255,255,255,255,14,0,0, 
  0,3,0,0,0,40,112,4,128,33,40,69,129,34,208,194,129,91,16,3,130,36,56,4,130,45,232,4,128,41,72,3,128,39,80,66,128,47,168,132,128,58,144,2,128,95,232,4,128,99,184,3,128,116,248,67,128,124,128,3, 
  128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,17,1,243,246,1,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1, 
  0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,3,18,124,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,182, 
  246,1,0,1,2,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,176,2,128,47,40,195,129,34,240,1,128,99,104,3,128,116,176,1,128,45,112,66,128,93,48,2,128,39,232,130,126,95,112,2,128,3, 
  17,1,190,242,1,0,1,3,17,1,51,242,1,0,1,3,17,1,200,250,1,0,1,3,17,1,79,229,1,0,1,3,18,31,0,0,0,1,3,17,1,72,241,1,0,1,3,17,1,198,227,1,0,1,3,17,1,243,240,1, 
  0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,162, 
  0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,48,2,128,42,24,4,128,34,32,195,127,43,232,2,130,116,80,4,128,45,208,4,129,94,144,4,128,39,112,66,128,63,176,130,128,93,152,3,128,95,208,4,128,99, 
  216,67,128,123,96,3,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,200,250,1,0,1, 
  3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,48,1,128,45,240,64,128, 
  95,240,0,128,3,17,1,79,229,1,0,1,3,18,31,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1, 
  186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,86,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,112,66,128,44,200,1,128,42,56,130,128,43,0,2,128,58,136,65,128,62,80,1,128,3,18,21,0, 
  0,0,1,3,17,1,239,7,2,0,1,3,18,9,0,0,0,1,3,18,25,0,0,0,1,3,18,33,0,0,0,1,3,17,1,189,253,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,18,79,0,0,0,1,2,21,4,206,0,0,0,255, 
  255,255,255,17,0,0,0,4,0,0,0,58,192,4,128,91,24,196,131,34,160,3,128,99,0,5,128,36,184,133,128,95,48,6,128,116,120,5,128,39,240,2,128,40,176,2,128,41,80,4,128,42,64,133,125,43,104,131,125,124, 
  136,4,128,45,48,6,128,94,240,5,128,63,48,131,125,123,224,3,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10, 
  0,0,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,18,9,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0, 
  0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1, 
  186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17,1,182,246,1,0,1,3,17,1,45,252,1,0,1,2,21,4,161, 
  0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,16,4,128,41,40,3,128,34,176,130,129,91,240,194,129,36,216,195,129,45,152,3,128,47,136,196,128,39,72,196,127,58,112,2,128,95,152,3,128,99,200,4,128,116, 
  48,66,128,124,96,3,128,3,17,1,190,242,1,0,1,3,17,1,102,5,2,0,1,3,17,1,51,242,1,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,79,229,1,0,1, 
  3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,72,241,1,0,1,3,17,1,198,227,1,0,1,3,17,1,243,240,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128, 
  3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,206,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,48,3,128,91,88,196,131,34, 
  224,3,128,99,0,5,128,36,184,133,128,95,48,6,128,116,120,5,128,39,240,2,128,40,176,2,128,41,144,4,128,42,64,133,125,43,168,131,125,124,200,4,128,45,48,6,128,94,240,5,128,63,112,131,125,123,32,4,128,3, 
  17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18,82,0,0,0,1,3, 
  18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21, 
  2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,183,0,0,0, 
  255,255,255,255,15,0,0,0,3,0,0,0,40,64,5,128,41,24,4,128,34,40,131,129,91,224,3,130,36,8,5,130,45,120,5,128,94,160,3,128,39,112,130,128,58,176,2,128,63,240,66,128,95,120,5,128,99,136,132,128, 
  116,200,132,128,123,104,3,128,124,80,4,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,18,120,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1,3,18, 
  82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,172,0,0,0,255,255, 
  255,255,14,0,0,0,3,0,0,0,40,232,4,128,41,192,3,128,34,208,130,129,91,136,195,129,36,176,196,129,45,32,5,128,94,72,3,128,39,80,130,128,58,144,2,128,95,32,5,128,99,48,132,128,116,112,132,128,123,16, 
  3,128,124,248,3,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3, 
  18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,144,4, 
  128,41,104,3,128,34,176,130,129,91,48,195,129,36,88,196,129,45,200,4,128,94,240,2,128,39,48,130,128,58,112,2,128,95,200,4,128,99,216,3,128,116,24,68,128,124,160,3,128,3,17,1,126,247,1,0,1,3,17,1, 
  102,5,2,0,1,3,17,1,243,246,1,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18, 
  127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17, 
  1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,172,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,232,4,128,41,192,3,128,34,8,131,129,91,136,3,130,36,176,4,130,45,32,5,128,94,72,3, 
  128,39,80,130,128,58,144,2,128,63,208,66,128,95,32,5,128,99,48,4,128,116,112,68,128,124,248,3,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,18,120,0,0,0,1,3,17,1,243,246,1,0,1, 
  3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3, 
  17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0, 
  1,2,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,16,1,128,41,200,1,128,58,72,1,128,47,136,1,128,3,18,66,0,0,0,1,3,17,1,102,5,2,0,1,3,17,1,198,227,1,0,1,3, 
  18,32,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,194,0,0,0,255,255,255,255,15, 
  0,0,0,3,0,0,0,40,96,5,128,105,168,3,128,34,208,197,129,43,32,69,129,36,152,5,130,45,224,68,129,102,232,3,128,47,160,4,128,91,104,196,128,98,40,4,128,109,104,195,128,115,40,195,128,116,232,2,128,117, 
  168,2,128,123,112,2,128,3,18,10,0,0,0,1,3,17,1,104,19,2,0,1,3,17,1,12,19,2,0,1,3,17,1,207,18,2,0,1,3,17,1,146,18,2,0,1,3,17,1,204,17,2,0,1,3,17,1,169,16,2, 
  0,1,3,17,1,77,16,2,0,1,3,18,82,0,0,0,1,3,17,1,198,227,1,0,1,3,17,1,41,16,2,0,1,3,17,1,10,16,2,0,1,3,18,31,0,0,0,1,3,18,127,0,0,0,1,3,17,1,51,242, 
  1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,46,20,2,0,1,2,21,2, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,46,20,2,0,1,2,18,34,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,46, 
  20,2,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,108,16,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17, 
  1,139,16,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,18,86,0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,97,16,1,128,49, 
  208,193,127,54,80,1,128,51,144,1,128,3,17,1,112,17,2,0,1,3,17,1,82,17,2,0,1,3,17,1,52,17,2,0,1,3,17,1,236,16,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,50,8,65,128,54,208,0,128,3,18,95,0,0,0,1,3,17,1,22,17,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,56,176,0,128,3,18,98,0,0,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,89,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,92,0,0,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,143,17,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,174,17,2,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,102,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56,16,1,128,49,200,1,128,54,72,1,128,51,136, 
  1,128,3,18,87,0,0,0,1,3,17,1,116,18,2,0,1,3,17,1,86,18,2,0,1,3,17,1,14,18,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,50,8,65,128,54,208,0,128, 
  3,18,96,0,0,0,1,3,17,1,56,18,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,56,176,0,128,3,18,99,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,50,176,0,128,3,18,90,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,93,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,97,176,0,128,3,17,1,177,18,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,81,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,116,176,0,128,3,17,1,238,18,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,18,84,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,114,176,0,128,3,17,1,43,19,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,74,19,2,0,1,2,21,4,29,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,101,176,0,128,3,18,101,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56,16,1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,88,0,0,0, 
  1,3,17,1,16,20,2,0,1,3,17,1,242,19,2,0,1,3,17,1,170,19,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,50,8,65,128,54,208,0,128,3,18,97,0,0,0,1,3, 
  17,1,212,19,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,56,176,0,128,3,18,100,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128, 
  3,18,91,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,94,0,0,0,1,2,18,128,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,46,240,0,128,69,48,65,128,101,48,1,128,3,17,1,129,21,2,0,1,3,17,1,253,20,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,128,20,2,0,1,2, 
  18,128,0,0,0,21,4,42,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,46,240,0,128,69,240,64,128,101,240,0,128,18,128,0,0,0,17,1,206,20,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,4,176,0,128,3,17,1,128,20,2,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,46,240,0,128,69,48,65,128,101,48,1,128,3,17,1,129,21,2,0,1,3,17,1,253, 
  20,2,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,45,208,0,128,43,208,192,127,3,17,1,98,21,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0, 
  128,3,17,1,62,21,2,0,1,2,18,128,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,62,21,2,0,1,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,4,176,0,128,3,17,1,62,21,2,0,1,2,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,160,21,2,0,1,2,18,128,0,0,0,21,4,34,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,101,208,0,128,69,208,192,127,3,17,1,253,20,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,230,21,2,0,1,2,18,128, 
  0,0,0,21,4,38,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,101,208,0,128,69,208,192,127,18,128,0,0,0,17,1,48,22,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4, 
  176,0,128,3,17,1,230,21,2,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,101,208,0,128,69,208,192,127,3,17,1,253,20,2,0,1,2,21,4,171,0,0,0,255,255,255,255,13,0,0, 
  0,3,0,0,0,40,168,4,128,105,48,3,128,34,24,133,129,43,104,4,129,36,224,196,129,45,40,4,129,102,112,3,128,91,240,195,128,98,176,3,128,109,240,194,128,115,176,2,128,116,112,2,128,117,48,2,128,3,17,1, 
  104,19,2,0,1,3,17,1,53,23,2,0,1,3,17,1,207,18,2,0,1,3,17,1,146,18,2,0,1,3,17,1,204,17,2,0,1,3,17,1,169,16,2,0,1,3,17,1,77,16,2,0,1,3,18,82,0,0,0,1, 
  3,17,1,41,16,2,0,1,3,17,1,10,16,2,0,1,3,18,31,0,0,0,1,3,18,127,0,0,0,1,3,17,1,51,242,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128, 
  5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,46,20,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,114,208,0,128,95,16,1,128,3, 
  17,1,43,19,2,0,1,3,18,40,0,0,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,240,0,128,45,48,65,128,95,48,1,128,3,17,1,196,23,2,0,1,3,17,1,79,229,1,0, 
  1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,18,41,0,0, 
  0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,16,129,128,45,80,1,128,111,144,1,128,95,80,65,127,3,17,1,114,25,2,0,1,3,17,1,79,229,1,0,1,3,17,1,38,24,2,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128, 
  128,45,240,192,127,107,48,1,128,3,17,1,79,229,1,0,1,3,17,1,124,24,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1, 
  18,54,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,79,229,1,0,1,3,17,1,210,24,2,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,110,240,0,128,45,48,65,128,95,48,1, 
  128,3,17,1,40,25,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,70,0,0,0,21,4,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1, 
  79,229,1,0,1,1,18,69,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2, 
  0,0,0,1,3,17,1,67,237,1,0,1,2,21,4,182,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,0,5,128,105,136,3,128,34,112,133,129,43,192,4,129,36,56,197,129,45,128,4,129,102,200,3,128,91, 
  72,196,128,98,8,4,128,109,72,195,128,115,8,195,128,116,200,2,128,117,136,2,128,123,80,2,128,3,18,10,0,0,0,1,3,17,1,104,19,2,0,1,3,17,1,12,19,2,0,1,3,17,1,207,18,2,0,1,3,17, 
  1,146,18,2,0,1,3,17,1,204,17,2,0,1,3,17,1,169,16,2,0,1,3,17,1,77,16,2,0,1,3,18,82,0,0,0,1,3,17,1,41,16,2,0,1,3,17,1,10,16,2,0,1,3,18,31,0,0,0,1, 
  3,18,127,0,0,0,1,3,17,1,51,242,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0, 
  1,3,17,1,46,20,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,39,27,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,18,40,0,0,0,1,2,21,4,171,0,0,0,255, 
  255,255,255,13,0,0,0,3,0,0,0,40,168,4,128,105,48,3,128,34,24,133,129,43,104,4,129,36,224,196,129,45,40,4,129,102,112,3,128,91,240,195,128,98,176,3,128,109,240,194,128,115,176,2,128,116,112,2,128,117, 
  48,2,128,3,17,1,104,19,2,0,1,3,17,1,12,19,2,0,1,3,17,1,207,18,2,0,1,3,17,1,146,18,2,0,1,3,17,1,204,17,2,0,1,3,17,1,169,16,2,0,1,3,17,1,77,16,2,0,1,3, 
  18,82,0,0,0,1,3,17,1,41,16,2,0,1,3,17,1,10,16,2,0,1,3,18,31,0,0,0,1,3,18,127,0,0,0,1,3,17,1,51,242,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,46,20,2,0,1,2,21,4,182,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,0,5, 
  128,105,80,3,128,34,112,197,129,43,192,4,129,36,56,5,130,45,128,196,128,102,144,3,128,91,72,4,129,93,16,132,128,98,208,3,128,109,16,195,128,115,208,2,128,116,144,2,128,117,80,2,128,3,17,1,104,19,2,0, 
  1,3,17,1,12,19,2,0,1,3,17,1,207,18,2,0,1,3,17,1,146,18,2,0,1,3,17,1,204,17,2,0,1,3,17,1,169,16,2,0,1,3,17,1,77,16,2,0,1,3,18,83,0,0,0,1,3,18,82,0, 
  0,0,1,3,17,1,41,16,2,0,1,3,17,1,10,16,2,0,1,3,18,31,0,0,0,1,3,18,127,0,0,0,1,3,17,1,51,242,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4, 
  112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,46,20,2,0,1,2,21,4,89,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,36,80,66,129,45,208, 
  1,128,34,136,130,128,43,16,2,128,102,144,1,128,116,80,1,128,3,17,1,195,29,2,0,1,3,17,1,164,29,2,0,1,3,17,1,10,16,2,0,1,3,17,1,10,16,2,0,1,3,18,127,0,0,0,1,3,17,1, 
  51,242,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,46,20,2,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,112,17,2,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,240,0,128,107,112,65,128,111,48,1, 
  128,3,17,1,43,19,2,0,1,3,17,1,249,29,2,0,1,3,18,69,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,24,30,2,0,1,2,18,54,0,0, 
  0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,60,30,2,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,70,0,0,0, 
  1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80, 
  1,128,6,16,1,128,3,208,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,67,237,1,0,1,3,17,1,79,229,1,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0, 
  0,116,144,1,128,97,144,194,128,110,16,194,128,115,208,1,128,105,80,2,128,118,80,1,128,3,17,1,182,34,2,0,1,3,17,1,14,34,2,0,1,3,17,1,54,33,2,0,1,3,17,1,94,32,2,0,1,3,17,1, 
  248,31,2,0,1,3,17,1,80,31,2,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3, 
  17,1,160,35,2,0,1,2,18,111,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,146,31,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128,3,17,1,212,31,2,0,1,21,2,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,118,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0, 
  0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,58,32,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0, 
  1,1,18,113,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,108,16,1,128,117,208,0,128,3,17,1,208,32,2,0,1,3,17,1,172,32,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,114,0,0, 
  0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3, 
  17,1,18,33,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,117,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,16,1,128,121,208,0,128,3,17,1,168,33,2,0,1,3,17,1,132,33,2,0, 
  1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,115,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3, 
  17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,17,1,234,33,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,67,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,80,34,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0, 
  0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,17,1,146,34,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0, 
  1,1,18,116,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,116,176,0,128,3,17,1,248,34,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,97,176,0,128,3,17,1,58,35,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,17,1,124,35,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,119,0,0, 
  0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,160,35,2,0,1,1,18,111,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3, 
  17,1,160,35,2,0,1,1,21,4,67,1,0,0,255,255,255,255,27,0,0,0,4,0,0,0,58,72,6,128,65,152,73,133,34,208,5,128,99,104,8,128,36,224,200,132,69,168,4,128,60,40,200,132,39,104,4,128,40,240, 
  3,128,41,192,198,129,42,168,136,125,43,152,197,129,44,48,132,126,45,216,137,129,62,176,199,128,63,96,133,129,73,232,7,128,78,112,199,128,91,136,134,129,93,248,198,129,94,24,9,128,95,216,9,128,97,88,9,128,116,32, 
  5,128,123,16,6,128,124,56,7,128,125,232,4,128,3,17,1,9,248,1,0,1,3,18,9,0,0,0,1,3,17,1,126,247,1,0,1,3,17,1,33,45,2,0,1,3,18,11,0,0,0,1,3,17,1,201,43,2,0,1, 
  3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,147,43,2,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,17,1,200,250,1,0,1,3, 
  18,66,0,0,0,1,3,17,1,71,42,2,0,1,3,18,21,0,0,0,1,3,17,1,161,38,2,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,18,127,0,0,0,1,3, 
  17,1,146,244,1,0,1,3,17,1,222,37,2,0,1,3,17,1,62,37,2,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1, 
  128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,83,240,128,128,45,48,193,127,95,48,1, 
  128,3,17,1,148,37,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,60,0,0,0,21,4,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1, 
  30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,48,1,128,3,17,1,30,246,1,0,1,3,17,1,52,38,2,0,1,21,2,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,61,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45, 
  208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,27,0,0,0,21,4,29,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,22,0,0,0,1,1,18,41,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,77,16,1,128,45,144,193,127,95,144,1,128,71,80,193,127, 
  3,17,1,165,40,2,0,1,3,17,1,3,39,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18, 
  41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,89,39,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128, 
  3,17,1,175,39,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,5,40,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,91,40,2,0,1, 
  3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,65,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0, 
  0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,251,40,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17, 
  1,81,41,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,167,41,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,253,41,2,0,1,3,17, 
  1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,63,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0, 
  21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,157,42,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1,128,3,17,1,243, 
  42,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,73,43,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128, 
  3,208,0,128,3,17,1,30,246,1,0,1,1,18,64,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,129,128,97,240,0,128,62,48,1,128,3,17,1, 
  182,246,1,0,1,3,18,24,0,0,0,1,3,17,1,147,246,1,0,1,2,18,41,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,104,80,1,128,45,144,1,128,107,16,1,128,95,144,193,127, 
  3,17,1,115,245,1,0,1,3,17,1,43,44,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18, 
  41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,101,240,0,128,3,17,1,129,44,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,110,240,0,128,45,48,65,128,95,48,1,128, 
  3,17,1,215,44,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,13,0,0,0,21,4,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30, 
  246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88,240,0,128,45,48,65,128,95,48,1,128,3,17,1,119,45,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48, 
  65,128,95,48,1,128,3,17,1,205,45,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0, 
  0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,35,46,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17, 
  1,121,46,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,207,46,2,0,1,3,17,1,30,246,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,18,62,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,30,246,1,0,1,21,2,34,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,21,4,104,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,0,3,128,116,128,2,128,58,200,1,128,43, 
  8,2,128,60,64,66,127,45,0,67,128,125,144,1,128,47,192,66,126,3,18,11,0,0,0,1,3,17,1,14,48,2,0,1,3,18,25,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,184,47,2,0,1,3,17,1, 
  198,227,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3, 
  17,1,30,246,1,0,1,2,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,104,240,0,128,45,48,65,128,95,48,1,128,3,17,1,43,44,2,0,1,3,17,1,30,246,1,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,30,246,1,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,16,1,128,97,208,0,128, 
  3,17,1,182,246,1,0,1,3,18,24,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,141,48,2,0,1,3,17,1,198,227,1,0,1,21,2, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111, 
  176,0,128,3,18,42,0,0,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,80,1,128,45,16,1,128,95,16,1,128,47,136,193,127,3,17,1,79,229,1,0,1,3,18,31,0,0,0,1, 
  3,17,1,198,227,1,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,5,80,1,128,6,16,1,128,3,208,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17, 
  1,67,237,1,0,1,3,17,1,79,229,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,119,208,0,128,47,16,193,127,3,17,1,136,49,2,0,1,3,17,1,198,227,1,0,1,21,2,54, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,112,129,128,5,48,1,128,6,240,0,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,67,237,1,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,167,49,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,198,49,2,0,1,2,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,44,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,114,208,0,128,47,16,1,128,3,17,1,57,50,2,0,1,3,17, 
  1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,117,176,0,128,3,17,1,88,50,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,119,50,2,0,1,2,21,4,29,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,43,0,0,0,1,2,21,4,104,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,95,0,3,128,116,128,2,128,58,0,2,128,43,200,1,128,60,64,66,127,45, 
  0,67,128,125,144,1,128,47,192,66,126,3,18,11,0,0,0,1,3,18,25,0,0,0,1,3,17,1,52,51,2,0,1,3,17,1,126,38,2,0,1,3,17,1,184,47,2,0,1,3,17,1,198,227,1,0,1,3,17,1, 
  30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2, 
  21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,24,0,0,0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128, 
  115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,100,52,2,0,1,3,17,1,38,52,2,0,1,3,17,1,239,51,2,0,1,3,17,1,110,232,1,0,1,3,17,1,18,232,1,0,1, 
  3,17,1,109,231,1,0,1,3,17,1,211,230,1,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17, 
  1,186,227,1,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,111,48,129,128,97,112,193,127,117,240,0,128,3,17,1,196,233,1,0,1,3,17,1,166,233,1,0,1,3,17,1,44,233,1,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,69,52,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,75,234, 
  1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,15,235,1,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,112,80,1,128,97,80,2, 
  129,102,16,2,128,47,144,66,128,103,208,1,128,109,144,1,128,3,17,1,8,53,2,0,1,3,17,1,110,232,1,0,1,3,17,1,18,232,1,0,1,3,17,1,109,231,1,0,1,3,17,1,211,230,1,0,1,3,17,1, 
  198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,117,208,0,128,97,16,193,127,3,17,1,196,233,1,0,1,3,17,1,44,233,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1, 
  136,53,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,167,53,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,198,53,2,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,13,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,112,240,0,128,47,112,65,128,103,48,1,128,3,17, 
  1,69,54,2,0,1,3,17,1,18,232,1,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186, 
  227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,196,233,1,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116,16,1,128,45,208, 
  1,128,99,80,1,128,47,144,193,127,3,17,1,168,55,2,0,1,3,17,1,76,55,2,0,1,3,17,1,198,227,1,0,1,3,17,1,209,54,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,240,54,2,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,15,55,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,46,55,2,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,57,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,107,55,2,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,138,55,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,18,56,0,0,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,199,55,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,18,54,0,0,0,1, 
  2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,240,0,128,47,112,65,128,99,48,1,128,3,17,1,168,55,2,0,1,3,17,1,76,55,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128, 
  47,8,1,128,3,18,39,0,0,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0, 
  1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,80,48,129,128,45,112,1,128,84,176,1,128,47,240,65,128,95,112,1,128,3,17,1,187,59,2,0,1,3,17,1,79,229,1,0,1,3,17,1,23, 
  57,2,0,1,3,17,1,198,227,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17, 
  1,79,229,1,0,1,2,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,109,57,2,0,1,3,17,1,79,229,1,0,1,21,2, 
  34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128, 
  45,240,64,128,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,195,57,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18, 
  41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,48,129,128,45,240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,25,58,2,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,240,128,128,45,48,193,127,95,48,1,128, 
  3,17,1,111,58,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,197,58,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,27,59,2,0,1, 
  3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,76,240,0,128,45,48,65,128,95,48,1,128,3,17,1,113,59,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128, 
  3,17,1,79,229,1,0,1,1,18,106,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17, 
  1,79,229,1,0,1,3,17,1,17,60,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,103,60,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,68,48,1,128,45,240,64,128,95,240,0,128,3,17,1,79,229,1,0,1,3,17, 
  1,189,60,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,85,48,129,128,45,240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,19,61,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17, 
  1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,67,240,128,128,45,48,193,127,95,48,1,128,3,17,1,105,61,2,0,1,3,17,1,79,229,1,0,1,21,2, 
  34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128, 
  45,48,65,128,95,48,1,128,3,17,1,191,61,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18, 
  41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,240,128,128,45,48,193,127,95,48,1,128,3,17,1,21,62,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128, 
  3,17,1,107,62,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,193,62,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,105,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,192,1,128,123,16,1,128,58,72,1, 
  128,47,128,129,127,3,18,10,0,0,0,1,3,18,39,0,0,0,1,3,17,1,198,227,1,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128, 
  3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,76,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,40,66,128,100,168,1,128,102,104,1,128,47,232,65,128,123,48,1,128,3,18,10,0,0, 
  0,1,3,17,1,165,64,2,0,1,3,17,1,236,63,2,0,1,3,17,1,198,227,1,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3, 
  17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,11,64,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,102,176,0,128,3,17,1,42,64,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,73,64,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,117,176,0,128,3,17,1,104,64,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,135,64,2,0,1,2,21,4,29,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,130,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,196,64,2,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,227,64,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,2,65,2,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,33,65,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,64,65,2,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,95,65,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,126,65,2,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,131,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,208,0,128,47,8,193,127,3,18,11,0,0,0, 
  1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,41,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,123,208,0,128,47,8,193,127,3,18,10,0,0,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3, 
  17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,1,128,47,208,0,128,3,17,1,198,227,1,0,1,3,17,1,51,242,1,0,1,21,2, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40, 
  128,2,129,125,80,1,128,102,192,1,128,47,64,130,128,100,0,2,128,123,136,1,128,3,18,11,0,0,0,1,3,18,10,0,0,0,1,3,17,1,165,64,2,0,1,3,17,1,236,63,2,0,1,3,17,1,198,227,1,0, 
  1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,52,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,40,104,1,128,47,40,65,128,123,240,0,128,3,18,10,0,0,0,1,3,17,1,198,227,1,0,1,3,18,31,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,80,1,128,45,16,1,128,95,16,1,128,47,136,193,127, 
  3,17,1,79,229,1,0,1,3,18,31,0,0,0,1,3,17,1,198,227,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0, 
  1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,116,48,1,128,47,176,193,128,34,48,2,128,39,240,129,127,99,112,1,128,3,17,1,99, 
  68,2,0,1,3,17,1,72,255,1,0,1,3,17,1,198,227,1,0,1,3,17,1,72,241,1,0,1,3,17,1,51,242,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16, 
  1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,130,68,2,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,106,243,1,0,1,2,21,4,7,1,0,0,255,255,255,255,22,0,0,0,4,0,0,0,58,16,6,128,60,136,198,132,34,176,4,128,99,200,6,128,36,128,135,131,91, 
  40,133,131,93,152,197,131,39,200,3,128,40,80,3,128,41,96,5,128,42,8,135,125,43,120,132,126,44,144,67,125,45,248,71,126,62,80,134,128,63,64,132,128,94,184,7,128,95,248,7,128,116,64,7,128,123,240,4,128,124, 
  216,5,128,125,8,4,128,3,17,1,9,248,1,0,1,3,18,9,0,0,0,1,3,17,1,126,247,1,0,1,3,18,11,0,0,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3, 
  18,10,0,0,0,1,3,18,82,0,0,0,1,3,18,32,0,0,0,1,3,17,1,200,250,1,0,1,3,18,66,0,0,0,1,3,17,1,223,69,2,0,1,3,18,21,0,0,0,1,3,17,1,126,38,2,0,1,3,17, 
  1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,129, 
  128,97,240,0,128,62,48,1,128,3,17,1,182,246,1,0,1,3,18,24,0,0,0,1,3,18,79,0,0,0,1,2,21,4,68,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,104,1,128,45,224,193,128,58,160, 
  1,128,95,224,1,128,125,48,1,128,3,18,11,0,0,0,1,3,18,31,0,0,0,1,3,17,1,239,7,2,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240, 
  0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,68,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,168,1,128,45,104,193, 
  128,95,104,1,128,47,224,193,127,125,48,1,128,3,18,11,0,0,0,1,3,17,1,79,229,1,0,1,3,18,31,0,0,0,1,3,17,1,198,227,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,69,240,1,128, 
  73,104,3,128,58,104,2,128,43,48,2,128,60,168,2,128,45,232,194,126,78,176,1,128,47,40,67,128,95,232,2,128,3,17,1,84,77,2,0,1,3,17,1,92,75,2,0,1,3,18,25,0,0,0,1,3,17,1,52,51, 
  2,0,1,3,17,1,126,38,2,0,1,3,17,1,79,229,1,0,1,3,17,1,198,227,1,0,1,3,17,1,182,71,2,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65, 
  128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,18,41,0,0,0,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,77,16,1,128,45,144,193, 
  127,95,144,1,128,71,80,193,127,3,17,1,186,73,2,0,1,3,17,1,24,72,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1,110,72,2,0,1,3,17,1,79,229,1,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129, 
  128,45,240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,196,72,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1, 
  18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,26,73,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1, 
  128,3,17,1,112,73,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,65,0,0,0,21,4,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1, 
  79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48,1,128,3,17,1,16,74,2,0,1,3,17,1,79,229,1,0,1,21,2,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45, 
  240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,102,74,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41, 
  0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,188,74,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128,95,240,0,128,3, 
  17,1,79,229,1,0,1,3,17,1,18,75,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,63,0,0,0,21,4,34,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229, 
  1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88,48,1,128,45,240,64,128,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,178,75,2,0,1,21,2,34,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65, 
  128,95,48,1,128,3,17,1,8,76,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0, 
  0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,79,229,1,0,1,3,17,1,94,76,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1, 
  79,229,1,0,1,3,17,1,180,76,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128,95,48,1,128,3,17,1,10,77,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0, 
  128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,62,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,45,240,192,127,95, 
  240,0,128,3,17,1,79,229,1,0,1,3,17,1,170,77,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21, 
  4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1,128,3,17,1,0,78,2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,41,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,86,78, 
  2,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,18,64,0,0,0,21,4,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1, 
  21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,208,0,128,47,8,1,128,3,18,21,0,0,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,24,4,128,33,208,4,128,34,176,130,129,91,240,194,129,36,224, 
  195,129,45,144,4,128,47,80,196,128,39,48,194,127,58,112,2,128,95,144,4,128,99,96,3,128,116,160,67,128,124,40,3,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,17,1,243,246,1,0,1,3,18, 
  82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,3,18, 
  124,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2, 
  21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,58,216,5,128,91,248,3,130,34,128,3,128,99,104,4,128,36,32,133,128,95,152,5,128,116,224,4,128,39,208,2,128,40,144,2,128,123,192,3,128,42,168, 
  132,125,43,72,131,125,124,48,4,128,45,152,5,128,94,88,5,128,63,16,131,125,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1, 
  3,18,10,0,0,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3, 
  17,1,30,246,1,0,1,3,17,1,198,80,2,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0, 
  1,3,17,1,30,246,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17,1,182,246,1,0,1,3,18,79,0,0,0,1,2,21,4,207,0,0,0,255,255, 
  255,255,17,0,0,0,4,0,0,0,58,24,4,128,91,88,4,130,34,160,3,128,99,8,5,128,36,192,133,128,95,56,6,128,116,128,5,128,39,240,2,128,40,176,2,128,123,224,3,128,42,72,133,125,43,104,131,125,60,200, 
  4,129,45,56,6,128,94,248,5,128,63,48,131,125,124,144,4,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0, 
  0,0,1,3,17,1,147,43,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0, 
  0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1, 
  186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,92,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,60,32,130,129,45,160,2,128,58,168,1,128,43,112,65,128,47,96,66,128,95,160,2,128,124,232,1,128, 
  3,18,25,0,0,0,1,3,17,1,52,51,2,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,58, 
  104,4,128,91,248,3,130,34,128,3,128,99,168,4,128,36,96,133,128,95,216,5,128,116,32,5,128,39,208,2,128,40,144,2,128,123,192,3,128,42,232,132,125,43,72,131,125,124,48,4,128,45,216,5,128,94,152,5,128,63, 
  16,131,125,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18,82,0,0,0,1,3,18,66,0,0, 
  0,1,3,17,1,104,246,1,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,195,0,0,0,255,255,255, 
  255,16,0,0,0,4,0,0,0,58,104,4,128,91,248,3,130,34,128,3,128,99,168,4,128,36,96,133,128,95,216,5,128,116,32,5,128,39,208,2,128,40,144,2,128,123,192,3,128,42,232,132,125,43,72,131,125,124,48,4, 
  128,45,216,5,128,94,152,5,128,63,16,131,125,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18, 
  82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,18,9,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17, 
  1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1, 
  2,21,4,150,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,184,3,128,47,48,196,129,34,144,2,129,91,208,130,129,36,128,131,129,45,64,3,128,58,80,2,128,39,240,131,126,95,64,3,128,99,112,4,128,116, 
  16,66,128,124,8,3,128,3,17,1,190,242,1,0,1,3,17,1,102,5,2,0,1,3,17,1,51,242,1,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,79,229,1,0,1,3,18,127,0,0,0,1, 
  3,18,31,0,0,0,1,3,17,1,72,241,1,0,1,3,17,1,198,227,1,0,1,3,17,1,243,240,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1, 
  128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,195,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,58,16,3,128,91,56,4,130,34,192,3,128,99,168,4,128, 
  36,96,133,128,95,216,5,128,116,32,5,128,39,208,2,128,40,144,2,128,123,0,4,128,42,232,132,125,43,136,131,125,124,112,4,128,45,216,5,128,94,152,5,128,63,80,131,125,3,17,1,9,248,1,0,1,3,17,1,126, 
  247,1,0,1,3,17,1,102,5,2,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245, 
  1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6, 
  240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,172,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,232,4,128,58,144, 
  2,128,34,8,195,127,91,192,195,129,36,176,196,129,45,32,5,128,94,128,3,128,39,80,66,128,63,208,66,128,95,32,5,128,99,48,132,128,116,112,132,128,123,72,3,128,124,248,3,128,3,17,1,126,247,1,0,1,3,17, 
  1,102,5,2,0,1,3,18,120,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17, 
  1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17, 
  1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,144,4,128,58,112,2,128,34,232,194,127,91,104,195,129,36,88,196, 
  129,45,200,4,128,94,40,3,128,39,48,66,128,63,176,66,128,95,200,4,128,99,216,3,128,116,24,68,128,124,160,3,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,18,120,0,0,0,1,3,17,1,243, 
  246,1,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,30, 
  246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21, 
  4,150,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,40,56,4,128,58,80,2,128,34,144,194,127,91,16,131,129,36,0,132,129,45,112,4,128,94,208,2,128,39,16,66,128,95,112,4,128,99,128,3,128,116,192,67, 
  128,124,72,3,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,17,1,243,246,1,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3, 
  17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3, 
  17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,144,4,128,58,112,2,128,34,176,194,127,91,104,131,129,36,88, 
  132,129,45,200,4,128,94,40,3,128,39,48,66,128,95,200,4,128,99,216,131,128,116,24,132,128,123,240,2,128,124,160,3,128,3,17,1,126,247,1,0,1,3,17,1,102,5,2,0,1,3,17,1,243,246,1,0,1,3,18, 
  10,0,0,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,18,31,0,0,0,1,3,17,1, 
  30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2, 
  21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,40,129,128,47,104,1,128,124,240,0,128,3,18,66,0,0,0,1,3,17,1,102,5,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,207,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,58,248,5,128,91,24, 
  4,130,34,160,3,128,99,200,4,128,36,128,133,128,95,56,6,128,116,64,5,128,39,240,2,128,40,176,2,128,123,224,3,128,42,8,133,125,43,104,131,125,60,136,4,129,45,56,6,128,94,184,5,128,63,48,131,125,124,80, 
  4,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0, 
  1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,10,91,2,0,1,3,17,1,30,246, 
  1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4, 
  53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,104,129,128,97,240,0,128,62,48,1,128,3,17,1,182,246,1,0,1,3,18,24,0,0,0,1,3,17,1,45,252,1,0,1,2,21,4,173,0,0,0,255,255, 
  255,255,14,0,0,0,3,0,0,0,40,112,4,128,47,232,68,130,34,136,2,129,43,80,130,129,36,248,3,129,45,40,5,128,58,48,4,128,39,168,132,126,60,56,3,129,91,200,130,128,95,40,5,128,99,184,3,128,116,120, 
  67,128,124,0,3,128,3,18,25,0,0,0,1,3,17,1,51,242,1,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,29,245,1,0,1,3,17,1,201,245,1,0,1,3, 
  18,127,0,0,0,1,3,17,1,14,48,2,0,1,3,18,31,0,0,0,1,3,17,1,72,241,1,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,231,0,0,0,255,255,255,255,19,0,0,0,4,0,0,0,58,152, 
  4,128,61,48,3,128,34,32,4,128,99,136,5,128,36,64,6,131,63,176,3,129,91,216,196,130,39,112,3,128,40,240,2,128,95,248,6,128,42,200,133,125,43,232,195,126,60,72,133,129,45,248,6,125,94,120,6,128,47,184, 
  134,125,116,0,6,128,123,96,4,128,124,16,5,128,3,17,1,9,248,1,0,1,3,17,1,238,229,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1, 
  3,18,10,0,0,0,1,3,17,1,147,43,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1, 
  3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1, 
  128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,92,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,60,32,130,129,45,160,2,128,58,112,1,128,43,176,65,128, 
  47,96,66,128,95,160,2,128,124,232,1,128,3,17,1,14,48,2,0,1,3,18,25,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,21,2, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,195,0,0,0,255, 
  255,255,255,16,0,0,0,4,0,0,0,91,248,131,129,95,216,5,128,34,64,3,128,99,168,4,128,36,40,69,128,116,232,4,128,123,128,3,128,39,144,2,128,40,160,5,128,124,48,4,128,58,96,5,128,43,8,67,125,60, 
  104,68,127,45,216,5,128,94,184,3,128,63,208,130,124,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1, 
  3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,14,48,2,0,1,3,18,31,0,0,0,1, 
  3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1, 
  0,1,2,21,4,184,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,72,5,128,58,8,5,128,34,32,195,127,43,232,194,129,36,208,4,129,45,128,5,128,94,96,3,128,39,112,130,128,60,16,68,129,63,176,130, 
  128,91,160,131,128,95,128,5,128,99,80,4,128,116,144,68,128,124,216,3,128,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,17,1,40,0,2,0,1,3, 
  18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,14,48,2,0,1,3,18,31,0,0,0,1,3, 
  17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0, 
  1,2,21,4,173,0,0,0,255,255,255,255,14,0,0,0,3,0,0,0,40,240,4,128,58,176,4,128,34,200,194,127,43,144,130,129,36,120,4,129,45,40,5,128,94,8,3,128,39,80,194,128,60,184,3,129,91,72,131,128, 
  95,40,5,128,99,248,3,128,116,56,68,128,124,128,3,128,3,17,1,126,247,1,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0, 
  1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,14,48,2,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,184,0,0,0,255,255,255,255, 
  15,0,0,0,3,0,0,0,40,72,5,128,58,8,5,128,34,232,194,127,43,176,130,129,36,208,4,129,45,128,5,128,94,96,3,128,39,112,194,128,60,16,4,129,91,160,131,128,95,128,5,128,99,80,132,128,116,144,132,128, 
  123,40,3,128,124,216,3,128,3,17,1,126,247,1,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,17,1,40,0,2,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1, 
  3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,14,48,2,0,1,3,18,31,0,0,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,207,0,0,0,255,255,255,255,17, 
  0,0,0,4,0,0,0,58,248,5,128,91,24,4,130,34,160,3,128,99,200,4,128,36,128,133,128,95,56,6,128,116,64,5,128,39,240,2,128,40,176,2,128,123,224,3,128,42,8,133,125,43,104,131,125,60,136,4,129,45, 
  56,6,128,94,184,5,128,63,48,131,125,124,80,4,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1, 
  3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1, 
  3,17,1,14,48,2,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1, 
  0,1,3,17,1,30,246,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,8,65,128,62,208,0,128,3,18,21,0,0,0,1,3,17,1,239,7,2,0,1,21,2,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,58,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116,16,1,128,45,80,1, 
  128,95,80,1,128,47,144,193,127,3,17,1,92,99,2,0,1,3,17,1,79,229,1,0,1,3,17,1,198,227,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5, 
  48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,18,41,0,0,0,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,8,193,127,3, 
  18,40,0,0,0,1,3,17,1,79,229,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,79,229,1,0,1,1,21,4,52,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,44,104,129,128,47,40,1,128,62,240,0,128,3,18,21,0,0,0,1,3,17,1,198,227,1,0,1,3,18,9,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,97,208,0,128,47,16,193,127,3,17,1,97,100,2,0,1,3,17, 
  1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,115,176,0,128,3,17,1,128,100,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,68,0,0,0,1,2,21,4,63,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,44,192,1,128,62,16,1,128,58,72,193,127,47,128,1,128,3,18,21,0,0,0,1,3,18,39,0,0,0,1,3,17,1,198,227,1,0,1,3,18,9,0,0,0,1,21,2,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,115,208,0,128,47,16, 
  193,127,3,17,1,93,101,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128,3,17,1,124,101,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,67,0,0,0, 
  1,2,21,4,129,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,69,208,1,128,65,136,195,129,58,72,3,128,43,16,2,128,60,200,2,128,45,200,195,126,78,72,2,128,95,200,3,128,73,136,66,128,97,8,3,128, 
  3,17,1,33,45,2,0,1,3,18,25,0,0,0,1,3,17,1,71,42,2,0,1,3,17,1,161,38,2,0,1,3,17,1,126,38,2,0,1,3,17,1,222,37,2,0,1,3,17,1,82,102,2,0,1,3,17,1,62,37, 
  2,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1, 
  30,246,1,0,1,2,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,8,65,128,62,208,0,128,3,18,24,0,0,0,1,3,18,79,0,0,0,1,2,21,4,141,0,0,0,255,255,255,255,11,0,0, 
  0,3,0,0,0,69,240,1,128,65,232,195,129,58,104,2,128,43,48,2,128,60,40,3,128,45,40,196,126,78,168,2,128,47,104,131,128,73,232,130,128,95,40,4,128,97,168,3,128,3,17,1,33,45,2,0,1,3,18,25, 
  0,0,0,1,3,17,1,52,51,2,0,1,3,17,1,71,42,2,0,1,3,17,1,161,38,2,0,1,3,17,1,126,38,2,0,1,3,17,1,198,227,1,0,1,3,17,1,222,37,2,0,1,3,17,1,62,37,2,0,1, 
  3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,172,103,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2, 
  48,130,128,3,240,1,128,6,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,232,103,2,0,1,3,17,1,226,103,2,0,1,3,17,1,220,103,2,0,1,2,18,19,0,0,0,21,4,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,25,229,1,0,1,3,17,1,241,227,1,0,1,1,18,19,0,0,0,1,18,17,0,0,0,1,18,18,0,0,0,1,21,4, 
  54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,47,112,193,127,97,240,0,128,3,17,1,109,104,2,0,1,3,17,1,79,104,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,83,176,0,128,3,18,60, 
  0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,18,61,0,0,0,1,2,21,4,89,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,116,136,1,128,125,80,1, 
  128,34,136,2,128,39,72,66,128,47,8,66,128,99,200,1,128,3,18,11,0,0,0,1,3,17,1,99,68,2,0,1,3,17,1,72,255,1,0,1,3,17,1,198,227,1,0,1,3,17,1,72,241,1,0,1,3,17,1,51, 
  242,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,206,0,0,0,255,255,255,255,17,0,0, 
  0,4,0,0,0,58,48,6,128,91,24,196,131,34,160,3,128,99,192,4,128,36,120,133,128,95,240,5,128,116,56,5,128,39,240,2,128,40,176,2,128,41,80,4,128,42,0,133,125,43,104,131,125,124,136,4,128,45,240,5, 
  128,94,176,5,128,63,48,131,125,123,224,3,128,3,17,1,9,248,1,0,1,3,17,1,126,247,1,0,1,3,18,120,0,0,0,1,3,18,25,0,0,0,1,3,17,1,243,246,1,0,1,3,18,10,0,0,0,1,3,18, 
  82,0,0,0,1,3,18,32,0,0,0,1,3,18,66,0,0,0,1,3,17,1,201,245,1,0,1,3,18,33,0,0,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,146,244,1,0,1,3,17,1, 
  30,246,1,0,1,3,17,1,198,80,2,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3, 
  17,1,30,246,1,0,1,2,21,4,81,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,60,200,1,128,45,8,2,128,58,136,1,128,43,80,65,128,47,72,66,128,95,8,2,128,3,18,25,0,0,0,1,3,17,1, 
  52,51,2,0,1,3,17,1,126,38,2,0,1,3,17,1,79,229,1,0,1,3,17,1,198,227,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3, 
  17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,79,229,1,0,1,2,21,4,184,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,208,4,128,33,136,5,128,34,232,130,129,43,176,194,129,36,88, 
  68,129,45,72,5,128,47,8,69,129,39,112,194,127,58,144,4,128,60,152,3,129,91,40,131,128,95,72,5,128,99,216,3,128,116,24,68,128,124,96,3,128,3,17,1,126,247,1,0,1,3,18,25,0,0,0,1,3,17,1, 
  243,246,1,0,1,3,18,82,0,0,0,1,3,18,66,0,0,0,1,3,17,1,126,38,2,0,1,3,17,1,201,245,1,0,1,3,17,1,29,245,1,0,1,3,18,127,0,0,0,1,3,17,1,14,48,2,0,1,3,18, 
  31,0,0,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,3,18,124,0,0,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17, 
  1,192,227,1,0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,65,128,116,208,0,128,3,17,1,136,53,2,0,1,3,17,1, 
  198,80,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,58,48,129,128,47,112,1,128,116,240,0,128,3,17,1,136,53,2,0,1,3,17,1,102,5,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,116,72,1,128,125,16,1,128,58,136,1,128,47,200,1,128,3, 
  18,11,0,0,0,1,3,17,1,136,53,2,0,1,3,17,1,102,5,2,0,1,3,17,1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192, 
  227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,40,1,128,47,104,65,128,125,240,0,128,3,18,11,0,0,0,1,3,17,1,136,53,2,0,1,3,17, 
  1,198,227,1,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,58,208,0,128,41,16,1,128,3,17,1,239,7,2,0,1,3,18,32,0,0,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192, 
  227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,65,128,116,208,0,128,3,17,1,136,53,2,0,1,3,17,1,239,7,2,0,1,21,2,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128, 
  3,17,1,239,7,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,16,1,128,3,17,1,192,227,1,0,1,3,17,1,186,227,1,0,1,2,21,4,93,0,0,0,255,255,255, 
  255,7,0,0,0,2,0,0,0,60,232,129,129,45,168,2,128,58,168,1,128,43,112,65,128,47,104,66,128,95,168,2,128,116,40,2,128,3,18,25,0,0,0,1,3,17,1,52,51,2,0,1,3,17,1,126,38,2,0,1, 
  3,17,1,184,47,2,0,1,3,17,1,198,227,1,0,1,3,17,1,30,246,1,0,1,21,2,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,112,65,128,5,48,1,128,3,17,1,192,227,1, 
  0,1,3,17,1,186,227,1,0,1,3,17,1,30,246,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,220,110,2,0,1,21,2,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,63,113,2,0,1,3,17,1,87,112,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,220,110,2, 
  0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,13,112,2,0,1,3,17,1,37,111,2,0,1,2,18,28,0,0,0,21,4,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,111,111,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,111,2,0,1,1,18, 
  28,0,0,0,21,4,38,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,18,28,0,0,0,17,1,200,111,2,0,1,21,2,33,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,17,1,188,111,2,0,1,2,18,28,0,0,0,17,1,200,111,2,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,111,111,2, 
  0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,111,2,0,1,2,18,28,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,3,17,1,111,111,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,111,111,2,0,1,1,18,28,0,0,0,21,4,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,161,112,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,161, 
  112,2,0,1,1,18,28,0,0,0,21,4,38,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,18,28,0,0,0,17,1,250,112,2,0,1,21,2,33,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,17,1,238,112,2,0,1,2,18,28,0,0,0,17,1,250,112,2,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127, 
  3,17,1,161,112,2,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,161,112,2,0,1,2,18,29,0,0,0,21,2,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,4,176,0,128,3,17,1,63,113,2,0,1,1,21,5,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,1,176,0,128,3,18,8,0,0,0,1,2, 
];

pub mod ast{
  impl AstObject for ASTNode {}
  type ASTSlot = (ASTNode, TokenRange, TokenRange);
  use super::*; 
  type Node = ASTNode;
  
  pub fn ir_from<'a> (mut reader: UTF8StringReader)-> Result<Box<State>, RadlrParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(8);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_State();
    Ok(obj_0_0)
  }
  
  pub fn escaped_from<'a> (mut reader: UTF8StringReader)-> Result<Vec<String>, RadlrParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(67796);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0.into_strings();
    Ok(obj_0_0)
  }
  
  pub fn grammar_from<'a> (mut reader: UTF8StringReader)-> Result<Box<Grammar>, RadlrParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(68615);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_Grammar();
    Ok(obj_0_0)
  }
  
  pub fn type_eval_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, RadlrParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(122669);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_expression_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, RadlrParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(122819);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_struct_from<'a> (mut reader: UTF8StringReader)-> Result<Box<AST_Struct>, RadlrParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(123662);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_AST_Struct();
    Ok(obj_0_0)
  }
}