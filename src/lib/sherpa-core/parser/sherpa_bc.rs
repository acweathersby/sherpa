
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
  types::{ast::*, *},
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
  AST_NamedReference(Box<AST_NamedReference>),
  Reduce(Box<Reduce>),
  SetTokenLen(Box<SetTokenLen>),
  PrattProduction(Box<PrattProduction>),
  EOFSymbol(Box<EOFSymbol>),
  PegProduction(Box<PegProduction>),
  AST_Add(Box<AST_Add>),
  Precedence(Box<Precedence>),
  NonTermMatch(Box<NonTermMatch>),
  GroupProduction(Box<GroupProduction>),
  List_Production(Box<List_Production>),
  SetTokenId(Box<SetTokenId>),
  TerminalToken(Box<TerminalToken>),
  Pop(Box<Pop>),
  AST_Vector(Box<AST_Vector>),
  FailHint(Box<FailHint>),
  Production_Symbol(Box<Production_Symbol>),
  Grammar(Box<Grammar>),
  AST_U64(Box<AST_U64>),
  DEFINED_TYPE_IDENT(Box<DEFINED_TYPE_IDENT>),
  Pass(Box<Pass>),
  Import(Box<Import>),
  Init(Box<Init>),
  CFProduction(Box<CFProduction>),
  AST_IndexReference(Box<AST_IndexReference>),
  AST_Member(Box<AST_Member>),
  Production_Import_Symbol(Box<Production_Import_Symbol>),
  AST_I32(Box<AST_I32>),
  Goto(Box<Goto>),
  AST_I8(Box<AST_I8>),
  AST_F32(Box<AST_F32>),
  Gotos(Box<Gotos>),
  TerminalMatches(Box<TerminalMatches>),
  AST_Token(Box<AST_Token>),
  Shift(Box<Shift>),
  Export(Box<Export>),
  Name(Box<Name>),
  AST_BOOL(Box<AST_BOOL>),
  Fail(Box<Fail>),
  AST_Statements(Box<AST_Statements>),
  AST_U8(Box<AST_U8>),
  Ignore(Box<Ignore>),
  AST_STRING(Box<AST_STRING>),
  Skip(Box<Skip>),
  AST_U32(Box<AST_U32>),
  State(Box<State>),
  AST_I64(Box<AST_I64>),
  PeekSkip(Box<PeekSkip>),
  DefaultMatch(Box<DefaultMatch>),
  SetLine(Box<SetLine>),
  AST_U16(Box<AST_U16>),
  NotEmptySet(Box<NotEmptySet>),
  Accept(Box<Accept>),
  ClassSymbol(Box<ClassSymbol>),
  ReduceRaw(Box<ReduceRaw>),
  Statement(Box<Statement>),
  AST_F64(Box<AST_F64>),
  TokenGroupProduction(Box<TokenGroupProduction>),
  AST_Map(Box<AST_Map>),
  Range(Box<Range>),
  IntMatch(Box<IntMatch>),
  AST_I16(Box<AST_I16>),
  Scan(Box<Scan>),
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
  AppendProduction(Box<AppendProduction>),
  Production_Terminal_Symbol(Box<Production_Terminal_Symbol>),
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
  AST_NamedReference,
  Reduce,
  SetTokenLen,
  PrattProduction,
  EOFSymbol,
  PegProduction,
  AST_Add,
  Precedence,
  NonTermMatch,
  GroupProduction,
  List_Production,
  SetTokenId,
  TerminalToken,
  Pop,
  AST_Vector,
  FailHint,
  Production_Symbol,
  Grammar,
  AST_U64,
  DEFINED_TYPE_IDENT,
  Pass,
  Import,
  Init,
  CFProduction,
  AST_IndexReference,
  AST_Member,
  Production_Import_Symbol,
  AST_I32,
  Goto,
  AST_I8,
  AST_F32,
  Gotos,
  TerminalMatches,
  AST_Token,
  Shift,
  Export,
  Name,
  AST_BOOL,
  Fail,
  AST_Statements,
  AST_U8,
  Ignore,
  AST_STRING,
  Skip,
  AST_U32,
  State,
  AST_I64,
  PeekSkip,
  DefaultMatch,
  SetLine,
  AST_U16,
  NotEmptySet,
  Accept,
  ClassSymbol,
  ReduceRaw,
  Statement,
  AST_F64,
  TokenGroupProduction,
  AST_Map,
  Range,
  IntMatch,
  AST_I16,
  Scan,
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
  AppendProduction,
  Production_Terminal_Symbol,
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
      ASTNode::AST_NamedReference(node) => node.tok.clone(),
      ASTNode::Reduce(node) => node.tok.clone(),
      ASTNode::PrattProduction(node) => node.tok.clone(),
      ASTNode::EOFSymbol(node) => node.tok.clone(),
      ASTNode::PegProduction(node) => node.tok.clone(),
      ASTNode::AST_Add(node) => node.tok.clone(),
      ASTNode::GroupProduction(node) => node.tok.clone(),
      ASTNode::List_Production(node) => node.tok.clone(),
      ASTNode::SetTokenId(node) => node.tok.clone(),
      ASTNode::TerminalToken(node) => node.tok.clone(),
      ASTNode::Pop(node) => node.tok.clone(),
      ASTNode::AST_Vector(node) => node.tok.clone(),
      ASTNode::Production_Symbol(node) => node.tok.clone(),
      ASTNode::Grammar(node) => node.tok.clone(),
      ASTNode::AST_U64(node) => node.tok.clone(),
      ASTNode::Pass(node) => node.tok.clone(),
      ASTNode::Import(node) => node.tok.clone(),
      ASTNode::CFProduction(node) => node.tok.clone(),
      ASTNode::AST_IndexReference(node) => node.tok.clone(),
      ASTNode::Production_Import_Symbol(node) => node.tok.clone(),
      ASTNode::AST_I32(node) => node.tok.clone(),
      ASTNode::Goto(node) => node.tok.clone(),
      ASTNode::AST_I8(node) => node.tok.clone(),
      ASTNode::AST_F32(node) => node.tok.clone(),
      ASTNode::Shift(node) => node.tok.clone(),
      ASTNode::AST_BOOL(node) => node.tok.clone(),
      ASTNode::Fail(node) => node.tok.clone(),
      ASTNode::AST_Statements(node) => node.tok.clone(),
      ASTNode::AST_U8(node) => node.tok.clone(),
      ASTNode::AST_STRING(node) => node.tok.clone(),
      ASTNode::Skip(node) => node.tok.clone(),
      ASTNode::AST_U32(node) => node.tok.clone(),
      ASTNode::State(node) => node.tok.clone(),
      ASTNode::AST_I64(node) => node.tok.clone(),
      ASTNode::PeekSkip(node) => node.tok.clone(),
      ASTNode::SetLine(node) => node.tok.clone(),
      ASTNode::AST_U16(node) => node.tok.clone(),
      ASTNode::NotEmptySet(node) => node.tok.clone(),
      ASTNode::Accept(node) => node.tok.clone(),
      ASTNode::ClassSymbol(node) => node.tok.clone(),
      ASTNode::ReduceRaw(node) => node.tok.clone(),
      ASTNode::AST_F64(node) => node.tok.clone(),
      ASTNode::TokenGroupProduction(node) => node.tok.clone(),
      ASTNode::AST_Map(node) => node.tok.clone(),
      ASTNode::AST_I16(node) => node.tok.clone(),
      ASTNode::Scan(node) => node.tok.clone(),
      ASTNode::Push(node) => node.tok.clone(),
      ASTNode::AST_Struct(node) => node.tok.clone(),
      ASTNode::Peek(node) => node.tok.clone(),
      ASTNode::Matches(node) => node.tok.clone(),
      ASTNode::Rule(node) => node.tok.clone(),
      ASTNode::AnnotatedSymbol(node) => node.tok.clone(),
      ASTNode::AST_Property(node) => node.tok.clone(),
      ASTNode::Ascript(node) => node.tok.clone(),
      ASTNode::AppendProduction(node) => node.tok.clone(),
      ASTNode::Production_Terminal_Symbol(node) => node.tok.clone(),
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
      ASTNode::AST_NamedReference(..) => ASTNodeType::AST_NamedReference,
      ASTNode::Reduce(..) => ASTNodeType::Reduce,
      ASTNode::SetTokenLen(..) => ASTNodeType::SetTokenLen,
      ASTNode::PrattProduction(..) => ASTNodeType::PrattProduction,
      ASTNode::EOFSymbol(..) => ASTNodeType::EOFSymbol,
      ASTNode::PegProduction(..) => ASTNodeType::PegProduction,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::Precedence(..) => ASTNodeType::Precedence,
      ASTNode::NonTermMatch(..) => ASTNodeType::NonTermMatch,
      ASTNode::GroupProduction(..) => ASTNodeType::GroupProduction,
      ASTNode::List_Production(..) => ASTNodeType::List_Production,
      ASTNode::SetTokenId(..) => ASTNodeType::SetTokenId,
      ASTNode::TerminalToken(..) => ASTNodeType::TerminalToken,
      ASTNode::Pop(..) => ASTNodeType::Pop,
      ASTNode::AST_Vector(..) => ASTNodeType::AST_Vector,
      ASTNode::FailHint(..) => ASTNodeType::FailHint,
      ASTNode::Production_Symbol(..) => ASTNodeType::Production_Symbol,
      ASTNode::Grammar(..) => ASTNodeType::Grammar,
      ASTNode::AST_U64(..) => ASTNodeType::AST_U64,
      ASTNode::DEFINED_TYPE_IDENT(..) => ASTNodeType::DEFINED_TYPE_IDENT,
      ASTNode::Pass(..) => ASTNodeType::Pass,
      ASTNode::Import(..) => ASTNodeType::Import,
      ASTNode::Init(..) => ASTNodeType::Init,
      ASTNode::CFProduction(..) => ASTNodeType::CFProduction,
      ASTNode::AST_IndexReference(..) => ASTNodeType::AST_IndexReference,
      ASTNode::AST_Member(..) => ASTNodeType::AST_Member,
      ASTNode::Production_Import_Symbol(..) => ASTNodeType::Production_Import_Symbol,
      ASTNode::AST_I32(..) => ASTNodeType::AST_I32,
      ASTNode::Goto(..) => ASTNodeType::Goto,
      ASTNode::AST_I8(..) => ASTNodeType::AST_I8,
      ASTNode::AST_F32(..) => ASTNodeType::AST_F32,
      ASTNode::Gotos(..) => ASTNodeType::Gotos,
      ASTNode::TerminalMatches(..) => ASTNodeType::TerminalMatches,
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Shift(..) => ASTNodeType::Shift,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::AST_BOOL(..) => ASTNodeType::AST_BOOL,
      ASTNode::Fail(..) => ASTNodeType::Fail,
      ASTNode::AST_Statements(..) => ASTNodeType::AST_Statements,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_STRING(..) => ASTNodeType::AST_STRING,
      ASTNode::Skip(..) => ASTNodeType::Skip,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::State(..) => ASTNodeType::State,
      ASTNode::AST_I64(..) => ASTNodeType::AST_I64,
      ASTNode::PeekSkip(..) => ASTNodeType::PeekSkip,
      ASTNode::DefaultMatch(..) => ASTNodeType::DefaultMatch,
      ASTNode::SetLine(..) => ASTNodeType::SetLine,
      ASTNode::AST_U16(..) => ASTNodeType::AST_U16,
      ASTNode::NotEmptySet(..) => ASTNodeType::NotEmptySet,
      ASTNode::Accept(..) => ASTNodeType::Accept,
      ASTNode::ClassSymbol(..) => ASTNodeType::ClassSymbol,
      ASTNode::ReduceRaw(..) => ASTNodeType::ReduceRaw,
      ASTNode::Statement(..) => ASTNodeType::Statement,
      ASTNode::AST_F64(..) => ASTNodeType::AST_F64,
      ASTNode::TokenGroupProduction(..) => ASTNodeType::TokenGroupProduction,
      ASTNode::AST_Map(..) => ASTNodeType::AST_Map,
      ASTNode::Range(..) => ASTNodeType::Range,
      ASTNode::IntMatch(..) => ASTNodeType::IntMatch,
      ASTNode::AST_I16(..) => ASTNodeType::AST_I16,
      ASTNode::Scan(..) => ASTNodeType::Scan,
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
      ASTNode::AppendProduction(..) => ASTNodeType::AppendProduction,
      ASTNode::Production_Terminal_Symbol(..) => ASTNodeType::Production_Terminal_Symbol,
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
      AST_NamedReference(node) => node.hash(hasher),
      Reduce(node) => node.hash(hasher),
      SetTokenLen(node) => node.hash(hasher),
      PrattProduction(node) => node.hash(hasher),
      EOFSymbol(node) => node.hash(hasher),
      PegProduction(node) => node.hash(hasher),
      AST_Add(node) => node.hash(hasher),
      Precedence(node) => node.hash(hasher),
      NonTermMatch(node) => node.hash(hasher),
      GroupProduction(node) => node.hash(hasher),
      List_Production(node) => node.hash(hasher),
      SetTokenId(node) => node.hash(hasher),
      TerminalToken(node) => node.hash(hasher),
      Pop(node) => node.hash(hasher),
      AST_Vector(node) => node.hash(hasher),
      FailHint(node) => node.hash(hasher),
      Production_Symbol(node) => node.hash(hasher),
      Grammar(node) => node.hash(hasher),
      AST_U64(node) => node.hash(hasher),
      DEFINED_TYPE_IDENT(node) => node.hash(hasher),
      Pass(node) => node.hash(hasher),
      Import(node) => node.hash(hasher),
      Init(node) => node.hash(hasher),
      CFProduction(node) => node.hash(hasher),
      AST_IndexReference(node) => node.hash(hasher),
      AST_Member(node) => node.hash(hasher),
      Production_Import_Symbol(node) => node.hash(hasher),
      AST_I32(node) => node.hash(hasher),
      Goto(node) => node.hash(hasher),
      AST_I8(node) => node.hash(hasher),
      AST_F32(node) => node.hash(hasher),
      Gotos(node) => node.hash(hasher),
      TerminalMatches(node) => node.hash(hasher),
      AST_Token(node) => node.hash(hasher),
      Shift(node) => node.hash(hasher),
      Export(node) => node.hash(hasher),
      Name(node) => node.hash(hasher),
      AST_BOOL(node) => node.hash(hasher),
      Fail(node) => node.hash(hasher),
      AST_Statements(node) => node.hash(hasher),
      AST_U8(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      AST_STRING(node) => node.hash(hasher),
      Skip(node) => node.hash(hasher),
      AST_U32(node) => node.hash(hasher),
      State(node) => node.hash(hasher),
      AST_I64(node) => node.hash(hasher),
      PeekSkip(node) => node.hash(hasher),
      DefaultMatch(node) => node.hash(hasher),
      SetLine(node) => node.hash(hasher),
      AST_U16(node) => node.hash(hasher),
      NotEmptySet(node) => node.hash(hasher),
      Accept(node) => node.hash(hasher),
      ClassSymbol(node) => node.hash(hasher),
      ReduceRaw(node) => node.hash(hasher),
      Statement(node) => node.hash(hasher),
      AST_F64(node) => node.hash(hasher),
      TokenGroupProduction(node) => node.hash(hasher),
      AST_Map(node) => node.hash(hasher),
      Range(node) => node.hash(hasher),
      IntMatch(node) => node.hash(hasher),
      AST_I16(node) => node.hash(hasher),
      Scan(node) => node.hash(hasher),
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
      AppendProduction(node) => node.hash(hasher),
      Production_Terminal_Symbol(node) => node.hash(hasher),
      
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
  pub tok: Token, 
}

impl Reset{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
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
pub struct Reduce{
  pub ast:Option<ASTNode>, 
  pub len:u32, 
  pub prod:ASTNode, 
  pub tok: Token, 
}

impl Reduce{
  
  pub fn new (ast: Option<ASTNode>, len: u32, prod: ASTNode, tok: Token)-> Self {
    
    Self{
      ast,
      len,
      prod,
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
    self.prod.hash(hasher);
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
pub struct PrattProduction{
  pub name_sym:Box<Production_Symbol>, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl PrattProduction{
  
  pub fn new (name_sym: Box<Production_Symbol>, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::PrattProduction
  }
}

impl ASTNode{
  
  pub fn to_PrattProduction (self)-> Box::<PrattProduction> {
    
    match self{
      Self::PrattProduction(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_PrattProduction (&self)-> Option<&PrattProduction> {
    
    match self{
      Self::PrattProduction(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_PrattProduction_mut (&mut self)-> Option<&mut PrattProduction> {
    
    match self{
      Self::PrattProduction(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for PrattProduction{
  
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
pub struct PegProduction{
  pub name_sym:Box<Production_Symbol>, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl PegProduction{
  
  pub fn new (name_sym: Box<Production_Symbol>, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::PegProduction
  }
}

impl ASTNode{
  
  pub fn to_PegProduction (self)-> Box::<PegProduction> {
    
    match self{
      Self::PegProduction(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_PegProduction (&self)-> Option<&PegProduction> {
    
    match self{
      Self::PegProduction(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_PegProduction_mut (&mut self)-> Option<&mut PegProduction> {
    
    match self{
      Self::PegProduction(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for PegProduction{
  
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
  pub val:u32, 
}

impl Precedence{
  
  pub fn new (val: u32)-> Self {
    
    Self{
      val,
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
    self.val.hash(hasher);
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
pub struct GroupProduction{
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl GroupProduction{
  
  pub fn new (rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::GroupProduction
  }
}

impl ASTNode{
  
  pub fn to_GroupProduction (self)-> Box::<GroupProduction> {
    
    match self{
      Self::GroupProduction(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_GroupProduction (&self)-> Option<&GroupProduction> {
    
    match self{
      Self::GroupProduction(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_GroupProduction_mut (&mut self)-> Option<&mut GroupProduction> {
    
    match self{
      Self::GroupProduction(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for GroupProduction{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct List_Production{
  pub optional:bool, 
  pub symbol:ASTNode, 
  pub terminal_symbol:Option<ASTNode>, 
  pub tok: Token, 
}

impl List_Production{
  
  pub fn new (optional: bool, symbol: ASTNode, terminal_symbol: Option<ASTNode>, tok: Token)-> Self {
    
    Self{
      optional,
      symbol,
      terminal_symbol,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::List_Production
  }
}

impl ASTNode{
  
  pub fn to_List_Production (self)-> Box::<List_Production> {
    
    match self{
      Self::List_Production(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_List_Production (&self)-> Option<&List_Production> {
    
    match self{
      Self::List_Production(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_List_Production_mut (&mut self)-> Option<&mut List_Production> {
    
    match self{
      Self::List_Production(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for List_Production{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.optional.hash(hasher);
    self.symbol.hash(hasher);
    self.terminal_symbol.hash(hasher);
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
  pub tok: Token, 
}

impl Pop{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
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
pub struct Production_Symbol{
  pub name:String, 
  pub tok: Token, 
}

impl Production_Symbol{
  
  pub fn new (name: String, tok: Token)-> Self {
    
    Self{
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Production_Symbol
  }
}

impl ASTNode{
  
  pub fn to_Production_Symbol (self)-> Box::<Production_Symbol> {
    
    match self{
      Self::Production_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Production_Symbol (&self)-> Option<&Production_Symbol> {
    
    match self{
      Self::Production_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Production_Symbol_mut (&mut self)-> Option<&mut Production_Symbol> {
    
    match self{
      Self::Production_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Production_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Grammar{
  pub preamble:Vec<ASTNode>, 
  pub productions:Vec<ASTNode>, 
  pub tok: Token, 
}

impl Grammar{
  
  pub fn new (preamble: Vec<ASTNode>, productions: Vec<ASTNode>, tok: Token)-> Self {
    
    Self{
      preamble,
      productions,
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
    
    for val in &self.productions{
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
pub struct CFProduction{
  pub name_sym:Box<Production_Symbol>, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl CFProduction{
  
  pub fn new (name_sym: Box<Production_Symbol>, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::CFProduction
  }
}

impl ASTNode{
  
  pub fn to_CFProduction (self)-> Box::<CFProduction> {
    
    match self{
      Self::CFProduction(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_CFProduction (&self)-> Option<&CFProduction> {
    
    match self{
      Self::CFProduction(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_CFProduction_mut (&mut self)-> Option<&mut CFProduction> {
    
    match self{
      Self::CFProduction(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for CFProduction{
  
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
pub struct Production_Import_Symbol{
  pub module:String, 
  pub name:String, 
  pub tok: Token, 
}

impl Production_Import_Symbol{
  
  pub fn new (module: String, name: String, tok: Token)-> Self {
    
    Self{
      module,
      name,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Production_Import_Symbol
  }
}

impl ASTNode{
  
  pub fn to_Production_Import_Symbol (self)-> Box::<Production_Import_Symbol> {
    
    match self{
      Self::Production_Import_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Production_Import_Symbol (&self)-> Option<&Production_Import_Symbol> {
    
    match self{
      Self::Production_Import_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Production_Import_Symbol_mut (&mut self)-> Option<&mut Production_Import_Symbol> {
    
    match self{
      Self::Production_Import_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Production_Import_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.module.hash(hasher);
    self.name.hash(hasher);
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
  pub prod:ASTNode, 
  pub tok: Token, 
}

impl Goto{
  
  pub fn new (name: String, prod: ASTNode, tok: Token)-> Self {
    
    Self{
      name,
      prod,
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
    self.prod.hash(hasher);
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
  pub goto:Box<Goto>, 
  pub pushes:Vec<Box<Push>>, 
}

impl Gotos{
  
  pub fn new (goto: Box<Goto>, pushes: Vec<Box<Push>>)-> Self {
    
    Self{
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
  pub tok: Token, 
}

impl Shift{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
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
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Export{
  pub production:ASTNode, 
  pub reference:String, 
}

impl Export{
  
  pub fn new (production: ASTNode, reference: String)-> Self {
    
    Self{
      production,
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
    self.production.hash(hasher);
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
pub struct Skip{
  pub tok: Token, 
}

impl Skip{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Skip
  }
}

impl ASTNode{
  
  pub fn to_Skip (self)-> Box::<Skip> {
    
    match self{
      Self::Skip(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Skip (&self)-> Option<&Skip> {
    
    match self{
      Self::Skip(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Skip_mut (&mut self)-> Option<&mut Skip> {
    
    match self{
      Self::Skip(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Skip{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
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
  pub id:Box<Production_Symbol>, 
  pub statement:Box<Statement>, 
  pub tok: Token, 
}

impl State{
  
  pub fn new (catches: bool, id: Box<Production_Symbol>, statement: Box<Statement>, tok: Token)-> Self {
    
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
pub struct PeekSkip{
  pub tok: Token, 
}

impl PeekSkip{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::PeekSkip
  }
}

impl ASTNode{
  
  pub fn to_PeekSkip (self)-> Box::<PeekSkip> {
    
    match self{
      Self::PeekSkip(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_PeekSkip (&self)-> Option<&PeekSkip> {
    
    match self{
      Self::PeekSkip(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_PeekSkip_mut (&mut self)-> Option<&mut PeekSkip> {
    
    match self{
      Self::PeekSkip(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for PeekSkip{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
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
  pub prod_id:u32, 
  pub rule_id:u32, 
  pub tok: Token, 
}

impl ReduceRaw{
  
  pub fn new (len: u32, prod_id: u32, rule_id: u32, tok: Token)-> Self {
    
    Self{
      len,
      prod_id,
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
    self.prod_id.hash(hasher);
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
pub struct TokenGroupProduction{
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl TokenGroupProduction{
  
  pub fn new (rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TokenGroupProduction
  }
}

impl ASTNode{
  
  pub fn to_TokenGroupProduction (self)-> Box::<TokenGroupProduction> {
    
    match self{
      Self::TokenGroupProduction(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_TokenGroupProduction (&self)-> Option<&TokenGroupProduction> {
    
    match self{
      Self::TokenGroupProduction(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_TokenGroupProduction_mut (&mut self)-> Option<&mut TokenGroupProduction> {
    
    match self{
      Self::TokenGroupProduction(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for TokenGroupProduction{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
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
pub struct Scan{
  pub tok: Token, 
}

impl Scan{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Scan
  }
}

impl ASTNode{
  
  pub fn to_Scan (self)-> Box::<Scan> {
    
    match self{
      Self::Scan(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Scan (&self)-> Option<&Scan> {
    
    match self{
      Self::Scan(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Scan_mut (&mut self)-> Option<&mut Scan> {
    
    match self{
      Self::Scan(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Scan{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Push{
  pub name:String, 
  pub prod:ASTNode, 
  pub tok: Token, 
}

impl Push{
  
  pub fn new (name: String, prod: ASTNode, tok: Token)-> Self {
    
    Self{
      name,
      prod,
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
    self.prod.hash(hasher);
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
  pub tok: Token, 
}

impl Peek{
  
  pub fn new (tok: Token)-> Self {
    
    Self{
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

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct AppendProduction{
  pub name_sym:ASTNode, 
  pub rules:Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl AppendProduction{
  
  pub fn new (name_sym: ASTNode, rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      name_sym,
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AppendProduction
  }
}

impl ASTNode{
  
  pub fn to_AppendProduction (self)-> Box::<AppendProduction> {
    
    match self{
      Self::AppendProduction(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_AppendProduction (&self)-> Option<&AppendProduction> {
    
    match self{
      Self::AppendProduction(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AppendProduction_mut (&mut self)-> Option<&mut AppendProduction> {
    
    match self{
      Self::AppendProduction(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AppendProduction{
  
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
pub struct Production_Terminal_Symbol{
  pub production:ASTNode, 
  pub tok: Token, 
}

impl Production_Terminal_Symbol{
  
  pub fn new (production: ASTNode, tok: Token)-> Self {
    
    Self{
      production,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Production_Terminal_Symbol
  }
}

impl ASTNode{
  
  pub fn to_Production_Terminal_Symbol (self)-> Box::<Production_Terminal_Symbol> {
    
    match self{
      Self::Production_Terminal_Symbol(val) => val,
      _ => panic!()
    }
  }
  
  pub fn as_Production_Terminal_Symbol (&self)-> Option<&Production_Terminal_Symbol> {
    
    match self{
      Self::Production_Terminal_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Production_Terminal_Symbol_mut (&mut self)-> Option<&mut Production_Terminal_Symbol> {
    
    match self{
      Self::Production_Terminal_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Production_Terminal_Symbol{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.production.hash(hasher);
  }
}


/* sym::production_symbol^id "=>" statement

        :ast { t_State, id, statement, tok } */
fn reducer_000 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_1 = ref_0;
  let obj_0_1 = obj_0_1.to_Production_Symbol();
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


/* sym::production_symbol^id "=!>" statement

        :ast { t_State, catches:true, id, statement, tok } */
fn reducer_001 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_0_1 = ref_0;
  let obj_0_1 = obj_0_1.to_Production_Symbol();
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


/* preamble(*) ( cf_production | pratt_production | peg_production | append_production | ir::state )(+)

        :ast { t_Grammar, preamble:$1, productions:$2, tok } */
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


/* preamble(*) ( cf_production | pratt_production | peg_production | append_production | ir::state )(+)

        :ast { t_Grammar, preamble:$1, productions:$2, tok } */
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


/* ( cf_production | pratt_production | peg_production | append_production | ir::state ) */
fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state ) */
fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state ) */
fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state ) */
fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state ) */
fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state )(+) */
fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state )(+) */
fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state )(+) */
fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state )(+) */
fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( cf_production | pratt_production | peg_production | append_production | ir::state )(+) */
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

            :ast { t_Production_Symbol , c_Symbol, name:str($1), tok} */
fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let var_2_0 = Production_Symbol::new(
    tok_0_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Production_Symbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
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


/* '\\' ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* '\\' ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* '\\' ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* '\\' ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* '\\' ( c:num | c:id | c:sym | c:nl | c:sp ) :ast str($2) */
fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* '\\' :ast str($1) */
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


/* "<>" sym::production_symbol^n ">" rules^r

        :ast { t_CFProduction, name_sym:$n, rules: $r, tok } */
fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Production_Symbol();
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = CFProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::CFProduction(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "#>" sym::production_symbol^n ">" rules^r

        :ast { t_PrattProduction, name_sym:$n, rules: $r, tok } */
fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Production_Symbol();
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = PrattProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::PrattProduction(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* ":>" sym::production_symbol^n ">" rules^r

        :ast { t_PegProduction, name_sym:$n, rules: $r, tok } */
fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_1_0 = obj_1_0.to_Production_Symbol();
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = PegProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::PegProduction(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* "+>" sym::non_terminal^n ">" rules^r

        :ast { t_AppendProduction,  name_sym:$n, rules: $r, tok } */
fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (ref_3, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_1_0 = ref_1;
  let obj_3_1 = ref_3.into_nodes();
  let var_5_0 = AppendProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AppendProduction(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* tk:identifier  :ast { t_DEFINED_TYPE_IDENT } */
fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* tk:number     :ast { t_DEFINED_TYPE_NUM } */
fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "str" convert_initializer?
        :ast { t_AST_STRING, value: $2, tok  } */
fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*",") */
fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ( "tk" | "tok" | "token" ) range?
        :ast { t_AST_Token, range: $2 } */
fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* add "+" expression

        :ast { t_AST_Add, left: $1, right: $3, tok } */
fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "map" "(" expression^k ',' expression^v ')'

        :ast { t_AST_Map, key: $k, val: $v, tok } */
fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "t_" identifier */
fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  slots.assign(0, AstSlot(ref_1, __rule_rng__, TokenRange::default()));
}


/* tk:id_tok */
fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "reduce" tok::int ( "symbols" "to" )? tok::int ( "with" "rule" )? tok::int^int
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), prod_id: u32($4), tok } */
fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), prod_id: u32($4), tok } */
fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), prod_id: u32($4), tok } */
fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
        
        :ast { t_ReduceRaw, len: u32($2), rule_id: u32($int), prod_id: u32($4), tok } */
fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "reduce" tok::int ( "symbols" "to" )? sym::non_terminal^prod ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  prod, tok } */
fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "reduce" tok::int ( "symbols" "to" )? sym::non_terminal^prod ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  prod, tok } */
fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "reduce" tok::int ( "symbols" "to" )? sym::non_terminal^prod ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  prod, tok } */
fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "reduce" tok::int ( "symbols" "to" )? sym::non_terminal^prod ( ":ast" ast::body^ast )?
        
        :ast { t_Reduce, len: u32($2), ast,  prod, tok } */
fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = SetLine::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::SetLine(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "peek"          :ast { t_Peek, tok } */
fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Peek::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "peek-skip"   :ast { t_PeekSkip, tok } */
fn reducer_126 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = PeekSkip::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::PeekSkip(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "shift"       :ast { t_Shift, tok } */
fn reducer_127 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Shift::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "skip"        :ast { t_Skip, tok } */
fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Skip::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Skip(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pop"         :ast { t_Pop, tok } */
fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Pop::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "scan"        :ast { t_Scan, tok } */
fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Scan::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Scan(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "reset"       :ast { t_Reset, tok } */
fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Reset::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Reset(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* match */
fn reducer_132 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_sequence */
fn reducer_133 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_statement */
fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "EXPORT" sym::non_terminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, production:$2, reference:str($3) } */
fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "EXPORT" sym::non_terminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, production:$2, reference:str($3) } */
fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "EXPORT" sym::non_terminal (( "AS" | "as" ) tok::id)?

        :ast { t_Export, production:$2, reference:str($3) } */
fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "IMPORT" ( c:id | c:sym )(+) c:sp ( "AS" | "as" ) tok::id

        :ast { t_Import, uri: str($2), reference:str($5), tok } */
fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "IMPORT" ( c:id | c:sym )(+) c:sp ( "AS" | "as" ) tok::id

        :ast { t_Import, uri: str($2), reference:str($5), tok } */
fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( c:id | c:sym ) */
fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym ) */
fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym )(+) */
fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym )(+) */
fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::terminal(+) */
fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* rule */
fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

        :ast { t_GroupProduction, rules:$2,  tok } */
fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = GroupProduction::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::GroupProduction(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* production_symbol */
fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_production_symbol */
fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* '(' init_objects ")"       
        :ast { t_Init, expression: $2 } */
fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<" token::int ( ","  token::int  )? ">"

        :ast { t_Range, start_trim:i32($2), end_trim:i32($3) } */
fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_i32();
  let var_4_0 = Range::new(
    0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* reference */
fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* reference '.' identifier
        :ast { t_AST_Member, reference:$1, property:$3 } */
fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* struct */
fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression
        :ast { t_AST_Statements, statements:[$1], tok } */
fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*";") */
fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* production_match_block */
fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_match_block */
fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") "then" goto

            :ast { t_Gotos, pushes: $1, goto } */
fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_2_0 = ref_2;
  let obj_2_0 = obj_2_0.to_Goto();
  let obj_0_1 = ref_0.into_nodes();
  let var_4_0 = Gotos::new(
    obj_2_0,
    obj_0_1.into_iter().map(|v|match v { ASTNode::Push(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* goto

            :ast { t_Gotos, goto } */
fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let obj_0_0 = obj_0_0.to_Goto();
  let var_2_0 = Gotos::new(
    obj_0_0,
    vec![],
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* goto_push */
fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") */
fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Fail::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pass"        :ast { t_Pass, tok } */
fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Pass::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "accept"      :ast { t_Accept, tok } */
fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Accept::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk:(" rules ")"{1}

        :ast { t_TokenGroupProduction, rules:$2,  tok } */
fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  let var_4_0 = TokenGroupProduction::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TokenGroupProduction(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token_non_terminal */
fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* (( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi])^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+) */
fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+) */
fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi] */
fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi] */
fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::id '::' tok::id

        :ast { t_Production_Import_Symbol , c_Symbol , module:str($1), name:str($3), tok} */
fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_2_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_string();
  let var_4_0 = Production_Import_Symbol::new(
    tok_0_0,
    tok_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Production_Import_Symbol(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* member */
fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "$" token::id 
        :ast { t_AST_NamedReference, value: str($2), tok } */
fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_199 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_202 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* int_match :ast [$1] */
fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( int_match | default_match | hint )(+) "}" :ast $2 */
fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "PRODUCTION" ( non_terminal_match :ast [$1] | "{" ( non_terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_ProductionMatches, matches:$m } */
fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* ( non_terminal_match | hint | default_match ) */
fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match ) */
fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match ) */
fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match )(+) */
fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match )(+) */
fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match )(+) */
fn reducer_211 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* non_terminal_match :ast [$1] */
fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( non_terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "TERMINAL" ( terminal_match :ast [$1] | "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_TerminalMatches, matches:$m } */
fn reducer_214 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_215 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_216 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_220 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* terminal_match :ast [$1] */
fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "push" sym::non_terminal

    :ast { t_Push, prod: $2, name:str($2), tok } */
fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "goto" sym::non_terminal

    :ast { t_Goto, prod: $2, name:str($2), tok } */
fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* tok::string

        :ast { t_TerminalToken, c_Symbol , val:str(tok<1,1>), tok, is_exclusive:true } */
fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

        :ast { t_TerminalToken , c_Symbol , val:str(tok<1,1>), tok } */
fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "tk:" non_terminal

        :ast { t_Production_Terminal_Symbol , c_Symbol , production:$2, tok } */
fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let var_3_0 = Production_Terminal_Symbol::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Production_Terminal_Symbol(Box::new(var_3_0)), __rule_rng__, TokenRange::default()));
}


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_230 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_231 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_232 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_233 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_234 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_235 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_245 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_246 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_247 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_248 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_249 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_250 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "[" sym::annotated_symbol(+)^s ']' "!"?^o

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol(+) */
fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "$" :ast { t_EOFSymbol, c_Symbol , tok } */
fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast

            :ast  { t_Ascript, ast:$ast, tok } */
fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_u64();
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) ) */
fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) )(+"|") */
fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "(" sym::non_terminal^sym ")" "{" statement "}"

    :ast { t_NonTermMatch, sym, statement } */
fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:quote_tok */
fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_5_0 = List_Production::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_5_0 = List_Production::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_271 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_0_1 = ref_0;
  let var_4_0 = List_Production::new(
    false,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(*" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok, optional:true } */
fn reducer_272 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_6_0 = List_Production::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(*" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok, optional:true } */
fn reducer_273 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (ref_2, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_3, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_3;
  let obj_5_0 = true;
  let obj_0_1 = ref_0;
  let obj_2_2 = ref_2;
  let var_6_0 = List_Production::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(var_6_0)), __rule_rng__, TokenRange::default()));
}


/* symbol "(*" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok, optional:true } */
fn reducer_274 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_4_0 = true;
  let obj_0_1 = ref_0;
  let var_5_0 = List_Production::new(
    obj_4_0,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(var_5_0)), __rule_rng__, TokenRange::default()));
}


/* symbol */
fn reducer_275 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num '}' :ast { t_Precedence, val: u32($2) } */
fn reducer_276 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let tok_1_0 = __tok_rng_1;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let var_4_0 = Precedence::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(var_4_0)), __rule_rng__, TokenRange::default()));
}


/* non_terminal */
fn reducer_277 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal */
fn reducer_278 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 279]
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
    ])
  }
}
    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<'a, T, UserCTX> = sherpa_rust_runtime::bytecode::ByteCodeParser<'a, T, UserCTX>;

pub mod meta{
  
  pub const production_names: [&'static str;98] = [
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
    "production_symbol",
    "statement",
    "statement_list",
    "statement_list_1",
    "escaped_vals",
    "escaped",
    "preamble",
    "cf_production",
    "pratt_production",
    "peg_production",
    "append_production",
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
    "non_terminal",
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
    "import_production_symbol",
    "init_objects",
    "reference",
    "generic_match_block",
    "generic_match_block_group",
    "generic_match_block_list_1",
    "generic_match_block_group_2",
    "production_match_block",
    "production_match_block_list",
    "production_match_block_group_1",
    "terminal_match_block",
    "terminal_match_block_list",
    "terminal_match_block_group_1",
    "goto_push",
    "goto",
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
    "non_terminal_match",
    "terminal_match",
    "string",
    "quote",
    "list",
    "precedence",
    "symbol",
  ];
  
  pub const symbol_string: [&'static str;130] = [
    r####"Default"####,
    r####"c:sp"####,
    r####"c:nl"####,
    r####"[=>]{1}"####,
    r####"[=!>]{1}"####,
    r####"non_term"####,
    r####"tk:non_term"####,
    r####"tk:non_term"####,
    r####"{EOF}"####,
    r####"[}]"####,
    r####"[,]{1}"####,
    r####"[{]{1}"####,
    r####"non_term"####,
    r####"[then]{1}"####,
    r####"non_term"####,
    r####"non_term"####,
    r####"c:id"####,
    r####"c:num"####,
    r####"c:sym"####,
    r####"[\]"####,
    r####"[>]{1}"####,
    r####"[<>]{1}"####,
    r####"non_term"####,
    r####"[#>]{1}"####,
    r####"[:>]{1}"####,
    r####"[+>]{1}"####,
    r####"non_term"####,
    r####"tk:non_term"####,
    r####"tk:non_term"####,
    r####"[str]{1}"####,
    r####"non_term"####,
    r####"[i8]{1}"####,
    r####"[u8]{1}"####,
    r####"[f32]{1}"####,
    r####"[i32]{1}"####,
    r####"[u32]{1}"####,
    r####"[f64]{1}"####,
    r####"[i64]{1}"####,
    r####"[u64]{1}"####,
    r####"[i16]{1}"####,
    r####"[u16]{1}"####,
    r####"[bool]{1}"####,
    r####"[true]{1}"####,
    r####"[false]{1}"####,
    r####"[[]{1}"####,
    r####"[]]{1}"####,
    r####"[tk]{1}"####,
    r####"[tok]{1}"####,
    r####"[token]{1}"####,
    r####"non_term"####,
    r####"[+]{1}"####,
    r####"[)]"####,
    r####"[,]"####,
    r####"[(]{1}"####,
    r####"[map]{1}"####,
    r####"non_term"####,
    r####"[:]{1}"####,
    r####"[t_]{1}"####,
    r####"tk:non_term"####,
    r####"[to]{1}"####,
    r####"[rule]{1}"####,
    r####"[with]{1}"####,
    r####"[:ast]{1}"####,
    r####"[reduce]{1}"####,
    r####"[set-tok]{1}"####,
    r####"[symbols]{1}"####,
    r####"[set-line]{1}"####,
    r####"[set-tok-len]{1}"####,
    r####"non_term"####,
    r####"non_term"####,
    r####"[pop]{1}"####,
    r####"[peek]{1}"####,
    r####"[scan]{1}"####,
    r####"[skip]{1}"####,
    r####"[reset]{1}"####,
    r####"[shift]{1}"####,
    r####"[peek-skip]{1}"####,
    r####"[AS]{1}"####,
    r####"[as]{1}"####,
    r####"[EXPORT]{1}"####,
    r####"[IMPORT]{1}"####,
    r####"[NAME]{1}"####,
    r####"[}]{1}"####,
    r####"[IGNORE]{1}"####,
    r####"[|]{1}"####,
    r####"[)]{1}"####,
    r####"[(]"####,
    r####"tk:non_term"####,
    r####"[<]{1}"####,
    r####"[.]"####,
    r####"[;]{1}"####,
    r####"[fail]{1}"####,
    r####"[pass]{1}"####,
    r####"[accept]{1}"####,
    r####"[tk:(]{1}"####,
    r####"non_term"####,
    r####"non_term"####,
    r####"[::]"####,
    r####"[$]{1}"####,
    r####"[match]{1}"####,
    r####"non_term"####,
    r####"[PRODUCTION]{1}"####,
    r####"[TERMINAL]{1}"####,
    r####"[push]{1}"####,
    r####"[goto]{1}"####,
    r####"[tk:]{1}"####,
    r####"[id]"####,
    r####"[nl]"####,
    r####"[sp]"####,
    r####"[tab]"####,
    r####"[num]"####,
    r####"[sym]"####,
    r####"[any]"####,
    r####"[htab]"####,
    r####"[c:]{1}"####,
    r####"[?]"####,
    r####"non_term"####,
    r####"non_term"####,
    r####"tk:non_term"####,
    r####"[!]"####,
    r####"[]]"####,
    r####"[default]"####,
    r####"[fail-hint]{1}"####,
    r####"non_term"####,
    r####"non_term"####,
    r####"tk:non_term"####,
    r####"tk:non_term"####,
    r####"[(*]{1}"####,
    r####"[(+]{1}"####,
    r####"tk:non_term"####,
  ];
}

pub fn new_ir_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(8);
  parser
}

pub fn new_escaped_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(92104);
  parser
}

pub fn new_grammar_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(93016);
  parser
}

pub fn new_type_eval_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(148022);
  parser
}

pub fn new_ast_expression_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(148172);
  parser
}

pub fn new_ast_struct_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(148650);
  parser
}

pub static bytecode: [u8; 183891] = [
  0,211,200,197,210,208,193,2,15,1,199,103,1,0,17,1,21,0,0,0,1,21,1,69,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,32,2,129,1,32,2,128,2,32,130,127,7,32,2,128,58,48,1,128,4, 
  19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,91,0,0,0,1,8,2,21,1,59,0,0,0,130,81,2,0,6,0,0,0,2,0,0,0,4,80,1,128,1,144,1,128,2,144,129, 
  128,3,152,129,128,6,144,1,128,7,144,1,128,4,17,1,160,100,1,0,1,8,4,17,1,151,0,0,0,1,2,21,1,132,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,48,6,128,1,248,11,128,2,248,139, 
  128,67,144,209,131,66,96,13,128,70,248,3,128,6,248,203,127,7,248,203,129,72,88,19,131,73,144,18,128,74,48,3,128,75,248,72,129,76,48,75,129,93,192,9,128,71,48,72,129,63,48,5,128,91,32,16,128,92,176,14, 
  128,99,192,4,128,103,0,12,128,104,48,7,128,4,19,37,0,0,0,131,0,0,0,1,0,15,1,148,100,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,148,100,1,0,17,1,161,94, 
  1,0,1,4,15,1,11,94,1,0,17,1,62,92,1,0,1,4,15,1,148,100,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,206,34,1,0,1,4,15,1,148,100,1,0,15,1,154,51, 
  1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,142,34,1,0,1,4,15,1,148,100,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,4,19,37,0,0,0,125, 
  0,0,0,1,0,15,1,148,100,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,148,100,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0, 
  134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,148,100,1,0,17,1,161,94,1,0,1,8,4,15,1,148,100, 
  1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0, 
  1,0,15,1,148,100,1,0,15,1,154,51,1,0,17,1,65,90,1,0,1,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0, 
  0,0,0,0,3,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,15,1,148,100,1,0, 
  15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,28,3,0,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,148,100,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,130,0,0,0, 
  1,0,15,1,148,100,1,0,17,1,161,94,1,0,1,2,21,1,63,0,0,0,188,89,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,87,56,1,128,8,4,19,49,0,0, 
  0,155,0,0,0,1,0,19,36,0,0,0,123,0,0,0,2,0,1,2,21,1,88,0,0,0,125,90,2,0,6,0,0,0,2,0,0,0,6,24,66,129,1,24,194,128,2,24,130,127,7,24,2,128,53,32,2,128,58,80, 
  1,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,107,31,1,0,17,1,0,31,1,0,1,8,4,15,1,188,30,1,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21,1,162,2,0,0,243,90,2,0,12, 
  0,0,0,3,0,0,0,58,192,76,130,1,136,205,129,2,136,141,127,94,112,10,130,44,16,2,128,53,16,80,129,6,136,77,127,7,136,13,128,105,144,18,128,114,144,13,128,125,32,7,128,126,208,3,128,4,15,1,135,156, 
  0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,19,94,0,0,0,12,1,0, 
  0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,135,156,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0, 
  15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,135,156,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1, 
  146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,135,156,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0, 
  15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,167,8,0,0,17,1,60,8,0, 
  0,1,8,4,15,1,135,156,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,135,156,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0, 
  15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,135,156,0,0,15,1,188,156,0,0,15,1,200,156, 
  0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6, 
  0,0,1,2,21,1,80,0,0,0,82,106,2,0,4,0,0,0,2,0,0,0,53,16,1,128,1,120,194,127,2,120,66,128,58,176,1,128,4,15,1,104,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19, 
  35,0,0,0,113,0,0,0,1,0,15,1,20,7,0,0,17,1,169,6,0,0,1,8,2,21,1,39,0,0,0,188,106,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,220,6, 
  0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58,240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,63, 
  0,0,0,189,0,0,0,3,0,1,8,2,21,0,83,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,168,65,128,63,24,65,128,79,16,1,128,11,8,66,127,1,19,47,0,0,0,153,0,0,0,1,0,17,1, 
  20,7,0,0,1,19,79,0,0,0,227,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,20,7,0,0,1,2,19,79,0,0,0,227,0,0,0,2,0,1,19,59,0,0,0,179,0,0,0,1,0,1, 
  19,97,0,0,0,21,1,0,0,1,0,1,21,1,163,0,0,0,128,107,2,0,10,0,0,0,3,0,0,0,112,120,3,128,1,112,3,130,2,112,131,129,107,160,2,128,108,56,2,128,109,208,1,128,110,176,4,128,111,8, 
  3,128,106,224,3,128,113,72,4,128,4,19,80,0,0,0,234,0,0,0,2,0,1,4,19,80,0,0,0,230,0,0,0,2,0,1,4,19,80,0,0,0,229,0,0,0,2,0,1,4,19,80,0,0,0,232,0,0,0,2, 
  0,1,8,4,19,80,0,0,0,233,0,0,0,2,0,1,4,19,80,0,0,0,231,0,0,0,2,0,1,4,19,80,0,0,0,235,0,0,0,2,0,1,4,19,80,0,0,0,228,0,0,0,2,0,1,2,19,59,0,0, 
  0,180,0,0,0,1,0,1,21,1,39,0,0,0,188,106,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,111,8,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21, 
  1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,224,0,0, 
  0,255,255,255,255,11,0,0,0,3,0,0,0,61,72,4,128,81,192,66,130,62,224,3,128,11,112,6,128,60,176,4,128,45,56,198,126,46,208,5,127,47,64,69,128,63,80,67,128,95,88,2,128,97,240,1,128,15,1,167, 
  8,0,0,17,1,86,78,0,0,1,15,1,167,8,0,0,17,1,208,74,0,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,167,8,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,167,8,0,0,1,15, 
  1,167,8,0,0,17,1,14,33,0,0,1,15,1,167,8,0,0,17,1,233,12,0,0,1,19,46,0,0,0,149,0,0,0,1,0,17,1,167,8,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,167,8,0,0, 
  1,15,1,167,8,0,0,17,1,136,9,0,0,1,17,1,135,156,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,167,8,0,0,1,2,21,1,47,0,0,0,135,109,2,0,5,0,0,0,2,0,0,0,84,48, 
  1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,12,17,1,196,9,0,0,1,10,12,19,45,0,0,0,148,0,0,0,1,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,56, 
  1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,244,9,0,0,1,2,21,1,36,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,104,78,130,1,176,201,129,2,176,137,127,94,16, 
  2,130,44,128,8,128,53,144,70,129,6,176,73,127,7,176,9,128,105,120,12,128,114,48,15,128,125,184,9,128,126,208,3,128,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15, 
  1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15, 
  1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1, 
  4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0, 
  1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0, 
  17,1,82,114,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7, 
  0,0,17,1,88,6,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,25,12,0,0,17,1,60,8,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15, 
  1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81, 
  160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61,40,4,128,46,128,133,126,47,240,196,126,95,56,2,128,97,208,1,128,15,1,25,12,0,0,17,1,86,78,0,0,1,15,1,25,12,0,0,17,1,208,74,0,0, 
  1,19,61,0,0,0,183,0,0,0,1,0,17,1,25,12,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,25,12,0,0,1,15,1,25,12,0,0,17,1,14,33,0,0,1,15,1,25,12,0,0,17,1,233,12, 
  0,0,1,19,46,0,0,0,150,0,0,0,3,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,25,12,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,25,12,0,0,1,2,19,46,0,0,0,150,0, 
  0,0,3,0,1,21,1,143,0,0,0,54,110,2,0,13,0,0,0,3,0,0,0,58,56,4,130,1,176,3,130,2,176,131,127,94,48,67,130,44,112,2,128,53,48,130,129,6,176,67,127,7,176,3,128,98,176,130,128,105, 
  248,3,128,114,184,3,128,125,240,2,128,126,112,3,128,12,17,1,229,30,0,0,1,12,17,1,188,28,0,0,1,12,17,1,147,26,0,0,1,12,17,1,106,24,0,0,1,12,17,1,65,22,0,0,1,12,17,1,24,20, 
  0,0,1,10,12,17,1,239,17,0,0,1,12,17,1,198,15,0,0,1,12,17,1,133,13,0,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58, 
  240,2,130,1,40,4,130,2,40,132,127,94,96,69,130,44,184,3,128,53,144,137,129,6,40,68,127,7,40,4,128,98,48,130,128,105,96,8,128,114,48,4,128,125,96,6,128,126,192,10,128,4,19,84,0,0,0,0,1,0, 
  0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,54,15,0,0,17,1,60,8,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,8,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1, 
  204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116, 
  0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,19,61,0,0,0,185,0,0,0,2,0,1,19,61,0,0,0,186,0,0,0,2,0,1,21,0,143,0,0,0, 
  255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,176,194,128,95,128,3,128,11,232,3,128,97,16,3,128,61,120,3,128,62,120,3,128,47,32,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,54,15,0, 
  0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,54,15,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,54,15,0,0,17,1,86,78,0,0,1,1,15,1,54,15,0,0,17,1,208,74,0,0,1,19, 
  47,0,0,0,152,0,0,0,1,0,17,1,54,15,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,8,130,1,96,3,130,2,96,131,127,94,152,68,130,44,240,2,128,53,144,137,129, 
  6,96,67,127,7,96,3,128,98,48,130,128,105,152,7,128,114,104,3,128,125,152,5,128,126,192,10,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17, 
  1,101,119,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4, 
  19,35,0,0,0,113,0,0,0,1,0,15,1,95,17,0,0,17,1,60,8,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0, 
  0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17, 
  1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,130,128,81,136,194,128,95,128,3,128,11,248,1,128,97,144,1,128,61,232,2,128,62,232,2,128,47,232,67,126,15,1,95, 
  17,0,0,17,1,86,78,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,95,17,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,95,17,0,0,1,15, 
  1,95,17,0,0,17,1,208,74,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,95,17,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,3,130,1,40,4,130,2,40,132, 
  127,94,96,69,130,44,240,2,128,53,144,137,129,6,40,68,127,7,40,4,128,98,48,130,128,105,96,8,128,114,48,4,128,125,96,6,128,126,192,10,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0, 
  0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,136,19,0,0,17,1,60,8,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0, 
  0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7, 
  0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0, 
  15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,32,130,128,81,176,194,128,95,168,3,128,11,24,3,128,97,16,4,128,61,16, 
  3,128,62,16,3,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1,136,19,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,136,19,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,19, 
  47,0,0,0,152,0,0,0,1,0,17,1,136,19,0,0,1,15,1,136,19,0,0,17,1,208,74,0,0,1,15,1,136,19,0,0,17,1,86,78,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0, 
  0,0,58,48,2,130,1,40,4,130,2,40,132,127,94,96,69,130,44,184,3,128,53,144,137,129,6,40,68,127,7,40,4,128,98,248,130,128,105,96,8,128,114,48,4,128,125,96,6,128,126,192,10,128,4,19,35,0,0,0, 
  113,0,0,0,1,0,15,1,177,21,0,0,17,1,60,8,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,8,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15, 
  1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0, 
  1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,130,128,81,248,193,128,95, 
  232,2,128,11,232,3,128,97,144,1,128,61,224,3,128,62,224,3,128,47,80,67,126,15,1,177,21,0,0,17,1,86,78,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17, 
  1,177,21,0,0,1,15,1,177,21,0,0,17,1,208,74,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,177,21,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,177,21,0,0,1,2,21,1,152, 
  1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,152,5,130,1,96,3,130,2,96,131,127,94,152,68,130,44,240,2,128,53,144,137,129,6,96,67,127,7,96,3,128,98,48,130,128,105,96,8,128,114,104,3,128,125, 
  96,6,128,126,192,10,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0, 
  0,0,113,0,0,0,1,0,15,1,218,23,0,0,17,1,60,8,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0, 
  22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6, 
  0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0, 
  0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0, 
  3,0,0,0,63,88,131,128,81,136,194,128,95,232,2,128,11,232,3,128,97,144,1,128,61,80,3,128,62,80,3,128,47,248,65,126,15,1,218,23,0,0,17,1,86,78,0,0,1,19,97,0,0,0,21,1,0,0,1,0, 
  17,1,218,23,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,218,23,0,0,17,1,208,74,0,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,218,23,0,0,1,19,47,0,0,0,152,0,0,0, 
  1,0,17,1,218,23,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,104,3,130,1,96,3,130,2,96,131,127,94,96,69,130,44,240,2,128,53,144,137,129,6,96,67,127,7,96,3,128, 
  98,48,130,128,105,96,8,128,114,48,4,128,125,96,6,128,126,192,10,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,8,4, 
  19,35,0,0,0,113,0,0,0,1,0,15,1,3,26,0,0,17,1,60,8,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0, 
  0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0, 
  12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21, 
  0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,131,128,81,24,195,128,95,16,4,128,11,144,1,128,97,32,2,128,61,120,3,128,62,120,3,128,47,136,66,126,19,47,0,0,0,152,0,0,0,1,0, 
  17,1,3,26,0,0,1,15,1,3,26,0,0,17,1,86,78,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,3,26,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,19,47,0,0,0,153,0,0,0, 
  1,0,17,1,3,26,0,0,1,15,1,3,26,0,0,17,1,208,74,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,152,5,130,1,96,3,130,2,96,131,127,94,152,68,130,44,240,2, 
  128,53,144,137,129,6,96,67,127,7,96,3,128,98,48,130,128,105,96,8,128,114,104,3,128,125,96,6,128,126,192,10,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1, 
  42,15,0,0,17,1,101,119,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,44,28,0,0,17,1,60,8,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19, 
  78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0, 
  17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,248,129,128,81,168,195,128,95,144,1,128,11,24,3,128,97,16,4,128,61,8,4,128,62,8,4,128,47,136, 
  66,126,15,1,44,28,0,0,17,1,208,74,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,44,28,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,44,28,0,0,1,19,47,0,0,0,152,0,0,0, 
  1,0,17,1,44,28,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,15,1,44,28,0,0,17,1,86,78,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,104,3,130,1,96, 
  3,130,2,96,131,127,94,96,69,130,44,240,2,128,53,144,137,129,6,96,67,127,7,96,3,128,98,48,130,128,105,96,8,128,114,48,4,128,125,96,6,128,126,192,10,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62, 
  0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,85,30,0,0,17,1,60,8,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1, 
  4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82, 
  114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22, 
  1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,32,194,128,95,128,3,128,11,128,2,128,97, 
  16,3,128,61,120,3,128,62,120,3,128,47,232,67,126,19,47,0,0,0,153,0,0,0,1,0,17,1,85,30,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,85,30, 
  0,0,1,15,1,85,30,0,0,17,1,86,78,0,0,1,1,15,1,85,30,0,0,17,1,208,74,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,85,30,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13, 
  0,0,0,3,0,0,0,58,96,3,130,1,40,4,130,2,40,132,127,94,96,69,130,44,240,2,128,53,144,137,129,6,40,68,127,7,40,4,128,98,48,130,128,105,96,8,128,114,48,4,128,125,96,6,128,126,192,10,128,4, 
  19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,126,32,0,0,17,1,60,8,0, 
  0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128, 
  81,168,195,128,95,8,4,128,11,24,3,128,97,176,2,128,61,112,4,128,62,112,4,128,47,32,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,126,32,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,126, 
  32,0,0,1,15,1,126,32,0,0,17,1,86,78,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,126,32,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,126,32,0,0,17,1,208,74,0,0,1, 
  1,2,21,1,47,0,0,0,44,112,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,74,33,0,0,1,10,12,19,60,0,0,0,182,0,0,0,1, 
  0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,160,1,129,1,160,1,128,2,160,129,127,7,160,1,128,62,48,1,128,4,15,1,196,74,0,0,17,1,128,33,0,0,1,8,2,21,1,4,3, 
  0,0,232,112,2,0,26,0,0,0,4,0,0,0,32,96,145,133,1,72,206,131,2,72,206,131,35,104,6,128,36,152,5,128,37,224,22,128,6,72,14,131,7,72,14,131,40,56,7,128,41,64,21,128,42,80,14,128,11,176, 
  87,130,44,120,13,128,29,192,15,128,46,168,12,128,31,8,139,129,33,216,11,128,34,16,22,130,38,160,83,129,39,144,80,129,43,48,18,128,47,216,8,128,48,112,20,128,54,8,8,128,87,208,3,128,98,168,9,128,4,19, 
  49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,4,15,1,160,74, 
  0,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0, 
  15,1,184,74,0,0,17,1,155,73,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,50,68,0,0,17,1, 
  219,67,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,160,74,0,0,15,1,172,74, 
  0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,50,68,0,0, 
  17,1,55,62,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,8,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0, 
  0,0,162,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15, 
  1,184,74,0,0,17,1,166,54,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1, 
  0,19,53,0,0,0,162,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4,15,1,160,74,0,0,15,1,172, 
  74,0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,184,74,0, 
  0,17,1,247,51,0,0,1,4,15,1,160,74,0,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,137,50,0,0,17,1,133,36,0,0,1,2,21,1,124,3,0,0,102,117,2,0,25, 
  0,0,0,4,0,0,0,32,32,8,133,1,8,212,131,2,8,212,131,35,144,14,128,36,32,6,128,37,144,17,128,38,32,201,131,39,16,20,132,40,16,24,128,9,40,77,130,42,176,11,128,43,144,18,128,44,224,26,128,29, 
  16,23,128,46,176,3,128,31,16,21,129,33,144,13,128,34,16,214,129,41,32,5,129,47,144,15,128,48,144,16,128,54,32,7,128,57,176,4,128,87,16,25,128,98,32,10,128,4,15,1,41,47,0,0,15,1,104,47,0,0, 
  15,1,125,50,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,69,40,0,0,17,1,2,40,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,165,52,0,0,17,1, 
  78,52,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,143,73, 
  0,0,17,1,62,68,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0, 
  15,1,184,74,0,0,17,1,248,53,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63, 
  0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,165,0,0,0,1,0,15,1,41,47,0,0,17,1,104,47,0,0,1,4,19,53,0,0,0,164,0,0,0, 
  2,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0, 
  17,1,242,73,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1, 
  50,68,0,0,17,1,177,52,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0, 
  26,0,0,0,1,0,19,54,0,0,0,165,0,0,0,1,0,15,1,41,47,0,0,17,1,104,47,0,0,1,8,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,166,54, 
  0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0, 
  17,1,247,51,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1, 
  184,74,0,0,17,1,155,73,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,165,0,0,0,1,0,15,1,41,47, 
  0,0,17,1,104,47,0,0,1,4,15,1,41,47,0,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,2,21,1,66,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0, 
  2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,160,0,0,0,1,0,19,34,0,0,0,112,0,0,0,2,0,1,2,21,7,47,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,44,56,1,128,125,208,0,128,4,19,9,0,0,0,32,0,0,0,3,0,1,4,17,1,144,40,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,2,21,1,187,0,0,0,104,118,2,0,6,0,0,0,2,0,0,0,48,216,2,128,1,208,2,128,2,208,130,128,47,216,4,128,46,216,67,128,58,80,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,52, 
  0,0,0,160,0,0,0,1,0,15,1,153,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,17,1,88,41,0,0,1,8,4,15,1,153,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0, 
  17,1,177,52,0,0,1,4,15,1,153,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1,55,62,0,0,1,4,15,1,153,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1, 
  76,41,0,0,17,1,219,67,0,0,1,2,19,33,0,0,0,111,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,157,41,0,0,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,33,0,0,0,110,0,0,0,1,0,1,21,1,111,2,0,0,214,119,2,0,24,0,0,0,4,0,0,0,32,160,5,133,1,72,204,131,2, 
  72,204,131,35,192,12,128,36,160,14,128,37,64,15,128,38,144,195,131,39,0,206,131,40,96,13,128,41,128,16,128,42,240,9,128,11,80,204,129,44,216,18,128,29,8,11,128,46,64,6,128,31,168,11,129,33,224,6,128,34, 
  128,135,129,43,192,17,128,47,32,17,128,48,80,9,128,54,224,15,128,87,48,4,128,98,32,8,128,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19, 
  27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,33,0,0,0,108,0,0,0,3,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,141,45,0,0, 
  15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,141,45,0,0, 
  15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,141,45,0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,4,19,27,0,0,0,92,0,0, 
  0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,33,0,0,0,108,0,0,0,3,0,1,4,15,1,141,45,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17, 
  1,229,62,0,0,1,8,4,15,1,129,45,0,0,17,1,13,44,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,155,73,0,0, 
  1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,166,54,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,141,45,0,0,15,1,184,74,0,0,17,1,199,50,0,0, 
  1,4,15,1,141,45,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,141,45,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,141,45,0,0,15,1,50,68,0,0,17,1,219,67,0,0, 
  1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,33,0,0,0,108,0,0,0,3,0,1,4,15,1,141,45,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,2,21,1,45, 
  0,0,0,151,120,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,15,1,59,44,0,0,17,1,2,40,0,0,1,2,21,7,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,44,208,0,128,125,16,1,128,4,17,1,134,44,0,0,1,4,19,9,0,0,0,32,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1, 
  187,0,0,0,104,118,2,0,6,0,0,0,2,0,0,0,48,216,3,128,1,208,3,128,2,208,131,128,47,208,2,128,46,216,68,128,58,80,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,160,0,0, 
  0,1,0,15,1,66,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,17,1,88,41,0,0,1,4,15,1,66,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1,219,67,0,0,1, 
  8,4,15,1,66,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1,177,52,0,0,1,4,15,1,66,45,0,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1, 
  55,62,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,9,0,0,0,31,0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,2,19,33,0,0,0,109,0,0,0,3,0,1,19,33,0,0,0,108,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,9,0,0, 
  0,31,0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,10,176,0,128,15,1, 
  216,45,0,0,17,1,252,45,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,16,1,128,125,208,0,128,12,17,1,27,47,0,0,1,12,17,1,73,46,0,0,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,12,17,1,27,47,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,131,46,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,139,0,0,0,104,118,2,0,6,0,0,0,2,0,0,0,48,24,3,128,1,240,1,128,2,240,129,128,47,184,3, 
  128,46,80,65,128,58,248,1,128,4,15,1,15,47,0,0,15,1,76,41,0,0,17,1,55,62,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,160,0,0,0,1,0,15,1,15,47,0,0,17, 
  1,88,41,0,0,1,4,15,1,15,47,0,0,15,1,76,41,0,0,17,1,177,52,0,0,1,4,15,1,15,47,0,0,15,1,76,41,0,0,17,1,219,67,0,0,1,2,19,10,0,0,0,34,0,0,0,3,0,1,14, 
  1,19,10,0,0,0,33,0,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,53,0,0,0,163,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,15,1,104,47,0,0,17,1,140,47,0,0,1,2,21,7,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,125,16,1,128,59,208,192,127,12,17,1,217,47,0,0,1,12,17,1,27,47,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  10,12,17,1,27,47,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,59,176,0,128,4,17,1,19,48,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,208,0,128,8,2,21,1,93,2,0,0,252,120,2,0,23,0,0,0,4,0,0,0,32,112,195,132,1,0,200,131,2,0,200,131,35,8,17,128,36,168,17,128,37,96,7,128,38,64,140,131,39,168,136,131,40,32,6, 
  128,41,160,11,128,42,136,10,128,43,176,14,128,44,104,16,128,29,16,14,128,46,72,18,128,31,232,201,128,33,192,6,128,34,72,73,129,47,128,5,128,48,200,15,128,54,8,8,128,87,16,4,128,98,224,12,128,4,15,1, 
  113,50,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,166,0,0,0, 
  3,0,1,4,15,1,113,50,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,142,62, 
  0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,8,4,15,1,113,50,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,166, 
  54,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0, 
  0,26,0,0,0,1,0,19,54,0,0,0,166,0,0,0,3,0,1,4,15,1,113,50,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4, 
  15,1,113,50,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,113,50,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,19,27, 
  0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,166,0,0,0,3,0,1,4,15,1,113,50,0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,4,15,1,113,50,0,0,15, 
  1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,113,50,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,113,50,0,0,15, 
  1,50,68,0,0,17,1,55,62,0,0,1,2,19,54,0,0,0,166,0,0,0,3,0,1,19,54,0,0,0,165,0,0,0,1,0,1,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,53,248,128,128,9, 
  88,193,127,85,240,0,128,1,19,85,0,0,0,1,1,0,0,2,0,1,19,53,0,0,0,161,0,0,0,1,0,17,1,137,50,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0, 
  128,4,15,1,235,51,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,85,0,0,0,1,0,1,21,1,129,0,0,0, 
  178,121,2,0,6,0,0,0,2,0,0,0,48,200,2,128,1,32,2,128,2,32,130,128,47,40,2,128,46,104,67,128,98,80,1,128,4,15,1,160,51,0,0,15,1,223,51,0,0,15,1,83,67,0,0,17,1,60,63,0, 
  0,1,8,4,15,1,160,51,0,0,15,1,148,51,0,0,17,1,219,67,0,0,1,4,15,1,160,51,0,0,15,1,148,51,0,0,17,1,177,52,0,0,1,4,15,1,160,51,0,0,15,1,148,51,0,0,17,1,55,62, 
  0,0,1,2,19,64,0,0,0,191,0,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,48,0,0,0,154,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,64,0,0,0,190,0,0,0,1,0,1,19,25,0,0,0,84,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,40,176,0,128,4,15,1,66,52,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,83,0,0,0,1,0,1,19,25, 
  0,0,0,82,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,153,52,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,26,0,0,0,91,0,0,0,1,0,1,19,26,0,0,0,90,0,0,0,2,0,1,19,8,0,0,0,25,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,60,176,0,128,4,15,1,236,53,0,0,17,1,252,52,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,30,0,0,0,104,0,0, 
  0,1,0,1,21,1,50,0,0,0,46,122,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,87,248,0,128,8,4,19,49,0,0,0,155,0,0,0,1,0,17,1,47,53,0,0,1,2,21,7,47,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,44,208,64,128,62,16,1,128,4,17,1,122,53,0,0,1,4,19,50,0,0,0,157,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,208,0,128,8,2,21,1,50,0,0,0,46,122,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,87,248,0,128,8,4,19,49,0,0,0,155,0,0,0,1,0,17,1,173,53,0,0,1, 
  2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,19,50,0,0,0,156,0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,2,19,30,0,0,0,101,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,67,54,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,77,0,0,0,1,0,1,19,25,0,0,0,76,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,40,176,0,128,4,15,1,154,54,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,71,0,0,0,1,0,1,19,25, 
  0,0,0,70,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,241,54,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,81,0,0,0,1,0,1,19,25,0,0,0,80,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4, 
  15,1,72,55,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,24,0,0,0,69,0,0,0,1,0,1,19,24,0,0,0,68,0,0, 
  0,2,0,1,19,8,0,0,0,23,0,0,0,1,0,1,21,1,106,3,0,0,105,122,2,0,24,0,0,0,4,0,0,0,32,72,18,133,1,200,207,131,2,200,207,131,35,248,3,128,36,72,17,128,37,72,20,128,38,200, 
  203,131,39,200,206,131,40,80,26,128,41,80,25,128,42,208,15,128,43,72,21,128,44,248,4,128,29,200,74,129,46,200,12,128,31,200,9,129,33,200,13,128,34,192,150,129,45,144,3,128,47,248,6,128,48,248,5,128,54,72, 
  19,128,87,248,7,128,98,192,23,128,4,19,28,0,0,0,96,0,0,0,2,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,203,58, 
  0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,4, 
  15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26, 
  0,0,0,1,0,19,29,0,0,0,97,0,0,0,1,0,15,1,203,58,0,0,17,1,10,59,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,229,62,0,0, 
  1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1, 
  248,53,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74, 
  0,0,17,1,142,62,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,166,54,0,0,1,8,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26, 
  0,0,0,1,0,19,29,0,0,0,97,0,0,0,1,0,15,1,203,58,0,0,17,1,10,59,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,73,74,0,0, 
  1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,143,73,0,0,17,1, 
  62,68,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0, 
  19,29,0,0,0,97,0,0,0,1,0,15,1,203,58,0,0,17,1,10,59,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,203, 
  58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15, 
  1,31,62,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,203,58,0,0,15,1,10,59,0,0,15,1,31,62,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,2,21,7,35,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,93,176,0,128,4,19,28,0,0,0,95,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,35,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,29,176,0,128,15,1,10,59,0,0,17,1,46,59,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,16,1,128,93,208,0,128,12,17,1,27, 
  47,0,0,1,12,17,1,123,59,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,12,17,1,27,47,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,44,176,0,128,4,17,1,181,59,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,93,2,0,0,252,120,2,0,23,0,0,0,4, 
  0,0,0,32,72,210,132,1,88,203,131,2,88,203,131,35,152,7,128,36,112,3,128,37,64,5,128,38,56,144,131,39,120,137,131,40,184,10,128,41,56,8,128,42,96,11,128,43,128,6,128,44,152,15,128,29,88,14,128,46, 
  248,14,128,31,24,205,128,33,120,12,128,34,216,72,129,47,184,13,128,48,224,5,128,54,24,10,128,87,216,16,128,98,16,4,128,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,19,62, 
  0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,19,62,0,0, 
  15,1,50,68,0,0,17,1,177,52,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,29,0,0,0,98,0,0,0,3,0,1,4,15,1,19,62,0,0,15,1,184,74,0, 
  0,17,1,242,73,0,0,1,4,15,1,19,62,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,19,62,0,0,15,1,184,74,0, 
  0,17,1,166,54,0,0,1,4,15,1,19,62,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,8,4,19,27,0,0,0,92,0,0,0,1, 
  0,19,8,0,0,0,26,0,0,0,1,0,19,29,0,0,0,98,0,0,0,3,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,229, 
  62,0,0,1,4,15,1,19,62,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,19,62,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,19,62,0,0,15,1,50,68,0,0,17,1,55, 
  62,0,0,1,4,15,1,19,62,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0, 
  0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,29,0,0,0,98,0,0,0,3,0,1,4,15,1,19,62,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,2,19,29,0,0,0,98,0,0,0, 
  3,0,1,19,29,0,0,0,97,0,0,0,1,0,1,19,8,0,0,0,27,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,130,62,0,0,17,1,252,52, 
  0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,30,0,0,0,102,0,0,0,1,0,1,19,30,0,0,0,99,0,0,0,2,0,1,21,7,36,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,217,62,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0, 
  0,0,87,0,0,0,1,0,1,19,25,0,0,0,86,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,48,63,0,0,17,1,18,51,0,0,1,21,9,27, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,79,0,0,0,1,0,1,19,25,0,0,0,78,0,0,0,2,0,1,21,1,83,0,0,0,42,123,2,0,4,0, 
  0,0,2,0,0,0,58,216,1,128,1,16,1,128,2,16,129,127,87,24,1,128,8,4,19,49,0,0,0,155,0,0,0,1,0,19,65,0,0,0,193,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,19, 
  65,0,0,0,192,0,0,0,2,0,1,2,19,8,0,0,0,29,0,0,0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,31,176,0,128,15,1,156,63,0,0,17,1,192,63,0,0,1,2, 
  21,1,219,0,0,0,154,123,2,0,18,0,0,0,4,0,0,0,25,16,4,128,1,16,5,128,2,16,69,130,51,144,4,128,52,144,195,130,21,88,197,130,45,208,4,128,23,152,6,128,24,208,2,128,9,80,195,125,10,80, 
  132,128,50,88,198,128,58,216,69,129,13,152,69,126,82,24,6,128,84,16,3,128,85,24,5,128,90,208,3,128,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0, 
  0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,10,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12, 
  17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,163,64,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,43,176,0,128,4,17,1,221,64,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,93,2,0,0,252,120,2,0,23,0,0,0,4, 
  0,0,0,32,32,199,132,1,152,204,131,2,152,204,131,35,0,9,128,36,248,11,128,37,112,3,128,38,192,135,131,39,72,146,131,40,96,8,128,41,128,14,128,42,32,15,128,43,224,10,128,44,176,4,128,29,160,12,128,46, 
  224,13,128,31,64,205,128,33,64,10,128,34,168,81,129,47,16,4,128,48,160,9,128,54,128,6,128,87,56,16,128,98,80,5,128,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,59,67, 
  0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,59,67,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,59,67,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0, 
  15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,59,67,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,59,67,0,0, 
  15,1,184,74,0,0,17,1,248,53,0,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,59,67,0,0, 
  15,1,50,68,0,0,17,1,177,52,0,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,31,0,0, 
  0,105,0,0,0,3,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,8,4,15,1,59,67,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,59,67,0,0,15,1,184,74, 
  0,0,17,1,229,62,0,0,1,4,15,1,59,67,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,59,67,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,19,27,0,0,0,92,0,0,0,1, 
  0,19,8,0,0,0,26,0,0,0,1,0,19,31,0,0,0,105,0,0,0,3,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,31, 
  0,0,0,105,0,0,0,3,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,59,67,0,0,15,1,184,74,0,0,17,1,166,54,0,0,1,2,19,31,0,0,0,105,0,0,0, 
  3,0,1,19,31,0,0,0,106,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,152,67,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,19,51,0,0,0,158,0,0,0,1,0,1,21,1,66,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,8,130,128,1,8,2,128,58,240,0,128,4,19,35,0,0,0, 
  113,0,0,0,1,0,19,52,0,0,0,160,0,0,0,1,0,19,51,0,0,0,159,0,0,0,3,0,1,8,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,38,68,0,0, 
  17,1,252,52,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,30,0,0,0,103,0,0,0,1,0,1,19,30,0,0,0,100,0,0,0,2,0,1,19,8, 
  0,0,0,28,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,17,1,120,68,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,2,21,1,78,2,0,0,252,120,2,0,23,0,0,0,4,0,0,0,32,248,202,132,1,112,200,131,2,112,200,131,35,56,12,128,36,48,7,128,37,24,9,128,38,216,140,131,39,152,139,131,40,184, 
  14,128,41,144,6,128,42,160,5,128,43,176,4,128,44,88,10,128,29,16,4,128,46,24,14,128,31,208,199,128,33,184,9,128,34,120,72,129,47,120,13,128,48,160,16,128,54,112,3,128,87,88,15,128,98,64,17,128,4,15, 
  1,199,70,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,199,70,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1, 
  0,17,1,199,70,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,199,70,0,0,1,4,15,1,199,70,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15, 
  1,199,70,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,199,70,0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,8,4,15,1,199,70,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4, 
  15,1,199,70,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,199,70,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,199,70,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4, 
  15,1,199,70,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,199,70,0,0,15,1,184,74,0,0,17,1,166,54,0,0,1,4,15,1,199,70,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4, 
  15,1,199,70,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4,15,1,199,70,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,199,70,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4, 
  15,1,199,70,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,199,70,0,0,1, 
  4,15,1,199,70,0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,4,15,1,199,70,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,2,21, 
  7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,1,71,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,78, 
  2,0,0,252,120,2,0,23,0,0,0,4,0,0,0,32,176,202,132,1,8,202,131,2,8,202,131,35,144,6,128,36,80,11,128,37,176,4,128,38,48,135,131,39,240,143,131,40,128,13,128,41,80,15,128,42,240,11,128,43, 
  24,9,128,44,240,5,128,29,48,17,128,46,16,4,128,31,208,209,128,33,112,3,128,34,80,69,129,47,144,16,128,48,224,12,128,54,16,10,128,87,208,7,128,98,32,14,128,4,15,1,80,73,0,0,15,1,184,74,0,0, 
  17,1,142,62,0,0,1,4,15,1,80,73,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0, 
  17,1,247,51,0,0,1,4,15,1,80,73,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0, 
  17,1,248,53,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,80,73,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0, 
  19,8,0,0,0,26,0,0,0,1,0,17,1,80,73,0,0,1,8,4,15,1,80,73,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4, 
  15,1,80,73,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,80,73,0,0,1,4,15,1,80,73,0,0,15,1,50,68, 
  0,0,17,1,177,52,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,4,15,1,80,73,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0, 
  17,1,60,63,0,0,1,4,15,1,80,73,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0,17,1,166,54,0,0,1,4,15,1,80,73,0,0,15,1,50,68,0,0, 
  17,1,219,67,0,0,1,4,15,1,80,73,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,80,73,0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,41,176,0,128,4,19,32,0,0,0,107,0,0,0,6,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,8,0,0,0,30,0,0,0, 
  1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,230,73,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,208,0,128,8,19,25,0,0,0,73,0,0,0,1,0,1,19,25,0,0,0,72,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,61,74,0,0,17, 
  1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,75,0,0,0,1,0,1,19,25,0,0,0,74,0,0,0,2,0,1,21,7,36, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,148,74,0,0,17,1,18,51,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  19,25,0,0,0,89,0,0,0,1,0,1,19,25,0,0,0,88,0,0,0,2,0,1,19,85,0,0,0,1,1,0,0,2,0,1,19,53,0,0,0,162,0,0,0,1,0,1,19,8,0,0,0,24,0,0,0,1,0,1, 
  19,60,0,0,0,181,0,0,0,2,0,1,21,1,63,0,0,0,171,126,2,0,5,0,0,0,2,0,0,0,115,112,1,128,1,176,1,128,2,176,129,128,11,184,65,127,118,48,1,128,12,17,1,28,75,0,0,1,12,17, 
  1,28,75,0,0,1,10,12,17,1,28,75,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,32,2,128,2,32,130,128,11,176, 
  65,127,118,112,1,128,4,17,1,130,77,0,0,1,4,17,1,138,76,0,0,1,4,15,1,201,75,0,0,17,1,98,75,0,0,1,8,2,21,1,39,0,0,0,76,130,2,0,3,0,0,0,1,0,0,0,2,48,1,128, 
  1,48,65,128,129,240,0,128,4,17,1,138,75,0,0,1,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,96,0,0,0,20,1,0,0,3,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,51,0,0,0,209,130,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,144,1,128,2,144,129,127,115,80,1,128,4,17,1,64,76, 
  0,0,1,4,17,1,8,76,0,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,1,44,0,0,0,218,131,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0, 
  0,250,0,0,0,4,0,1,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127, 
  4,17,1,43,77,0,0,1,4,15,1,225,76,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0, 
  1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,19,81,0,0,0,236,0,0,0,4,0,1,21,1,57,0,0,0,124,132,2,0,4,0,0,0,2,0,0,0, 
  118,16,1,128,1,80,1,128,2,80,129,127,11,88,1,128,4,17,1,255,77,0,0,1,8,4,15,1,199,77,0,0,17,1,98,75,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,218,131,2, 
  0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3, 
  0,1,19,81,0,0,0,245,0,0,0,4,0,1,21,1,51,0,0,0,41,133,2,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,12,17,1,150,78,0,0,1,10,12,17,1, 
  150,78,0,0,1,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,4,17,1,203,79,0,0,1, 
  8,4,17,1,202,78,0,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,160,67,129,1,184,193,128,2,184,129,127,51,80,1,128,125,192,1,128,126,176,2,128,4,19,95,0,0,0,18,1, 
  0,0,3,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,140,79,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17, 
  1,140,79,0,0,1,4,15,1,77,79,0,0,17,1,140,7,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,168,66,129,1,8,196,128,2,8,132,127,51,80,1, 
  128,125,24,3,128,126,184,1,128,4,19,95,0,0,0,15,1,0,0,3,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,141,80,0,0,1,4,15,1,78,80,0,0,17, 
  1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,141,80,0,0,1,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128, 
  4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41, 
  176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,162,2,0,0,243,90,2,0,12,0,0,0,3,0, 
  0,0,58,32,76,130,1,232,204,129,2,232,140,127,94,240,12,130,44,96,10,128,53,144,82,129,6,232,76,127,7,232,12,128,105,96,5,128,114,224,7,128,125,64,15,128,126,16,2,128,4,19,94,0,0,0,12,1,0,0, 
  1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,17,114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15, 
  1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,17,114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0, 
  0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,17, 
  114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,17,114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15, 
  1,176,30,1,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,218,83,0,0,17,1,111,83,0,0,1,8,4,15,1,17,114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250, 
  245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1, 
  0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,17,114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0, 
  0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,17,114,0,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250, 
  245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21, 
  1,39,0,0,0,38,135,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,162,83,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0, 
  3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,229,0,0,0,255,255,255,255,12,0,0,0, 
  3,0,0,0,59,96,5,128,81,224,130,130,61,104,4,128,11,152,70,127,60,208,4,128,45,96,70,127,46,248,133,128,47,104,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16,2,128,15,1,218,83,0,0,17,1, 
  155,111,0,0,1,15,1,218,83,0,0,17,1,148,108,0,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,218,83,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,218,83,0,0,1,15,1,218,83,0,0, 
  17,1,34,108,0,0,1,15,1,218,83,0,0,17,1,21,88,0,0,1,19,46,0,0,0,149,0,0,0,1,0,17,1,218,83,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,218,83,0,0,1,15,1,218, 
  83,0,0,17,1,192,84,0,0,1,17,1,17,114,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,218,83,0,0,1,2,21,1,47,0,0,0,135,109,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48, 
  1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,252,84,0,0,1,12,19,45,0,0,0,148,0,0,0,1,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48, 
  1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,44,85,0,0,1,2,21,1,36,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,16,66,130,1,168,202,129,2,168,138,127,94,176,10,130,44,184, 
  6,128,53,216,66,129,6,168,74,127,7,168,10,128,105,112,12,128,114,200,4,128,125,232,7,128,126,96,14,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,81,87,0,0,17,1,111,83,0,0,1,4,15,1,221,12, 
  0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1, 
  221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4, 
  15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,8,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0, 
  0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88, 
  6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0, 
  0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63, 
  48,131,129,11,136,5,128,60,144,4,128,61,40,4,128,46,128,133,126,47,240,196,126,95,56,2,128,97,208,1,128,15,1,81,87,0,0,17,1,155,111,0,0,1,15,1,81,87,0,0,17,1,148,108,0,0,1,19,61,0, 
  0,0,183,0,0,0,1,0,17,1,81,87,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,81,87,0,0,1,15,1,81,87,0,0,17,1,34,108,0,0,1,15,1,81,87,0,0,17,1,21,88,0,0,1,19, 
  46,0,0,0,150,0,0,0,3,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,81,87,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,81,87,0,0,1,2,21,1,143,0,0,0,54,110,2,0,13, 
  0,0,0,3,0,0,0,58,112,3,130,1,48,4,130,2,48,132,127,94,48,67,130,44,112,2,128,53,56,132,129,6,48,68,127,7,48,4,128,98,240,130,128,105,240,3,128,114,176,2,128,125,48,2,128,126,176,3,128,12, 
  17,1,249,105,0,0,1,12,17,1,208,103,0,0,1,12,17,1,167,101,0,0,1,12,17,1,126,99,0,0,1,12,17,1,85,97,0,0,1,12,17,1,44,95,0,0,1,12,17,1,3,93,0,0,1,12,17,1,218,90, 
  0,0,1,10,12,17,1,177,88,0,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,32,8,130,2,32,136,127,94,40,72,130,44, 
  88,10,128,53,200,138,129,6,32,72,127,7,32,8,128,98,96,133,128,105,48,2,128,114,40,9,128,125,96,3,128,126,32,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0, 
  15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0, 
  0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42, 
  15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0, 
  15,1,74,90,0,0,17,1,111,83,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,240,1,128,11,240,2,128,97,128,3,128,61,232,2,128,62,232,2,128, 
  47,88,66,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,74,90,0,0,17,1,148,108,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,74,90,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17, 
  1,74,90,0,0,1,15,1,74,90,0,0,17,1,155,111,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,74,90,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130, 
  1,32,8,130,2,32,136,127,94,40,72,130,44,88,10,128,53,200,138,129,6,32,72,127,7,32,8,128,98,96,133,128,105,48,2,128,114,40,9,128,125,96,3,128,126,32,6,128,4,15,1,30,15,0,0,15,1,159,116,0, 
  0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0, 
  19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0, 
  12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8, 
  0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0, 
  1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,115,92,0,0,17,1,111,83,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,224,131,128,81,144,193,128,95,240,1,128,11,88,2, 
  128,97,120,3,128,61,112,4,128,62,112,4,128,47,232,66,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,115,92,0,0,17,1,148,108,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,115,92,0,0,1, 
  19,97,0,0,0,21,1,0,0,1,0,17,1,115,92,0,0,1,15,1,115,92,0,0,17,1,155,111,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,115,92,0,0,1,1,2,21,1,152,1,0,0,112,111,2, 
  0,13,0,0,0,3,0,0,0,58,40,8,130,1,32,8,130,2,32,136,127,94,240,72,130,44,32,11,128,53,144,139,129,6,32,72,127,7,32,8,128,98,96,133,128,105,48,2,128,114,240,9,128,125,96,3,128,126,32,6, 
  128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0, 
  1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0, 
  187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,156,94,0,0,17,1,111,83,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0, 
  1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240, 
  129,128,81,144,193,128,95,16,4,128,11,24,3,128,97,168,3,128,61,16,3,128,62,16,3,128,47,128,66,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,156,94,0,0,1, 
  19,97,0,0,0,21,1,0,0,1,0,17,1,156,94,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,156,94,0,0,1,15,1,156,94,0,0,17,1,155,111,0,0,1,15,1,156,94,0,0,17,1,148,108, 
  0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,5,130,1,232,8,130,2,232,136,127,94,240,72,130,44,32,11,128,53,144,139,129,6,232,72,127,7,232,8,128,98,40,134,128,105,48, 
  2,128,114,240,9,128,125,96,3,128,126,232,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1, 
  0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0, 
  0,0,113,0,0,0,1,0,15,1,197,96,0,0,17,1,111,83,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0, 
  0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1, 
  4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21,0,143,0,0,0,255, 
  255,255,255,8,0,0,0,3,0,0,0,63,232,130,128,81,144,193,128,95,16,4,128,11,120,3,128,97,128,2,128,61,8,4,128,62,8,4,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0, 
  21,1,0,0,1,0,17,1,197,96,0,0,1,15,1,197,96,0,0,17,1,155,111,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,197,96,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,197,96,0, 
  0,1,1,15,1,197,96,0,0,17,1,148,108,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,40,9,130,1,32,8,130,2,32,136,127,94,40,72,130,44,32,11,128,53,144,139,129,6, 
  32,72,127,7,32,8,128,98,96,133,128,105,48,2,128,114,240,9,128,125,96,3,128,126,32,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1, 
  88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116, 
  0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,238,98,0,0,17,1,111,83,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0, 
  15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1, 
  181,3,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,96,130,128,81,144,193,128,95,240,1,128,11,128,3,128,97,16,4,128,61,88,2,128,62,88,2,128,47,240,66,126,19,61,0,0, 
  0,185,0,0,0,2,0,1,15,1,238,98,0,0,17,1,148,108,0,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,238,98,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,238,98,0,0,1,19,47, 
  0,0,0,152,0,0,0,1,0,17,1,238,98,0,0,1,15,1,238,98,0,0,17,1,155,111,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,40,8,130,1,32,8,130,2,32,136,127, 
  94,240,72,130,44,32,11,128,53,144,139,129,6,32,72,127,7,32,8,128,98,96,133,128,105,48,2,128,114,240,9,128,125,96,3,128,126,32,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0, 
  0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78, 
  0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,23,101,0,0,17,1,111,83,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0, 
  0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,129,128,81,144,193,128,95,128,2,128,11,232,3,128,97,232,2,128,61,80,3, 
  128,62,80,3,128,47,88,67,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,23,101,0,0,1,15,1,23,101,0,0,17,1,148,108,0,0,1,15,1,23,101,0,0,17,1, 
  155,111,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,23,101,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,23,101,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0, 
  0,58,96,5,130,1,232,8,130,2,232,136,127,94,240,72,130,44,32,11,128,53,144,139,129,6,232,72,127,7,232,8,128,98,40,134,128,105,48,2,128,114,240,9,128,125,96,3,128,126,232,6,128,4,15,1,30,15,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178, 
  0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,64,103,0,0,17,1,111,83,0,0,1, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97, 
  0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0, 
  1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,192,130,128,81,248,193,128,95,88, 
  2,128,11,232,3,128,97,144,1,128,61,80,3,128,62,80,3,128,47,88,67,126,15,1,64,103,0,0,17,1,155,111,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,64,103,0,0,17,1,148,108,0,0,1, 
  19,47,0,0,0,153,0,0,0,1,0,17,1,64,103,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,64,103,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,64,103,0,0,1,2,21,1,152,1, 
  0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,40,9,130,1,32,8,130,2,32,136,127,94,40,72,130,44,32,11,128,53,144,139,129,6,32,72,127,7,32,8,128,98,96,133,128,105,48,2,128,114,240,9,128,125,96, 
  3,128,126,32,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0, 
  0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0, 
  19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30, 
  15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1, 
  0,15,1,105,105,0,0,17,1,111,83,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17, 
  1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3, 
  0,0,0,63,80,131,128,81,144,193,128,95,232,2,128,11,240,1,128,97,128,2,128,61,112,4,128,62,112,4,128,47,224,67,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1, 
  105,105,0,0,1,15,1,105,105,0,0,17,1,155,111,0,0,1,15,1,105,105,0,0,17,1,148,108,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,105,105,0,0,1,19,97,0,0,0,21,1,0,0,1,0, 
  17,1,105,105,0,0,1,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,32,8,130,2,32,136,127,94,40,72,130,44,88,10,128,53,200,138,129,6,32,72,127,7,32,8,128,98, 
  96,133,128,105,48,2,128,114,40,9,128,125,96,3,128,126,32,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,93, 
  0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0, 
  1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0, 
  0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,146,107,0,0,17,1,111,83,0,0,1,2,21,0, 
  143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,128,2,128,11,232,2,128,97,128,3,128,61,120,3,128,62,120,3,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1, 
  19,97,0,0,0,21,1,0,0,1,0,17,1,146,107,0,0,1,15,1,146,107,0,0,17,1,148,108,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,146,107,0,0,1,1,15,1,146,107,0,0,17,1,155,111, 
  0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,146,107,0,0,1,2,21,1,47,0,0,0,44,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128, 
  10,12,17,1,94,108,0,0,1,12,19,60,0,0,0,182,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,160,1,129,1,160,1,128,2,160,129,127,7,160,1,128,62,48,1,128, 
  4,15,1,196,74,0,0,17,1,128,33,0,0,1,8,2,21,1,63,0,0,0,171,126,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,184,65,127,118,120,1,128,12,17,1,224,108,0, 
  0,1,10,12,17,1,224,108,0,0,1,12,17,1,224,108,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,232,1,128,1,224,1,128,2,224, 
  129,128,11,48,65,127,118,160,1,128,4,15,1,218,110,0,0,17,1,98,75,0,0,1,4,17,1,238,109,0,0,1,8,4,17,1,38,109,0,0,1,2,21,1,57,0,0,0,124,132,2,0,4,0,0,0,2,0,0,0, 
  118,128,1,128,1,192,1,128,2,192,129,127,11,16,1,128,4,15,1,182,109,0,0,17,1,98,75,0,0,1,4,17,1,107,109,0,0,1,8,19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246, 
  0,0,0,3,0,1,21,1,44,0,0,0,218,131,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,247,0,0,0,4,0,1,8,19,81,0,0,0,237,0,0,0,3, 
  0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,144,110,0,0,17,1,98,75,0,0,1,4,17,1,69,110,0,0,1,21,9,27,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1, 
  98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,1, 
  51,0,0,0,209,130,2,0,4,0,0,0,2,0,0,0,118,24,1,128,1,16,1,128,2,16,129,127,115,88,1,128,8,4,17,1,81,111,0,0,1,4,17,1,25,111,0,0,1,19,81,0,0,0,239,0,0,0,2,0, 
  1,21,1,44,0,0,0,218,131,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,250,0,0,0,4,0,1,8,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  81,0,0,0,248,0,0,0,3,0,1,21,1,51,0,0,0,41,133,2,0,4,0,0,0,2,0,0,0,128,80,1,128,1,144,1,128,2,144,1,128,127,16,1,128,12,17,1,219,111,0,0,1,12,17,1,219,111,0,0, 
  1,10,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,24,1,128,1,16,1,128,2,16,1,128,127,88,1,128,8,4,17,1,16,113,0,0,1,4,17, 
  1,15,112,0,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,160,67,129,1,168,194,128,2,168,130,127,51,80,1,128,125,176,2,128,126,184,1,128,4,19,95,0,0,0,18,1,0,0,3, 
  0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,209,112,0,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,209,112, 
  0,0,1,4,15,1,146,112,0,0,17,1,140,7,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,64,66,129,1,24,195,128,2,24,131,127,51,176,2,128,125,32, 
  3,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,210,113,0,0,1,4,15,1,147,113,0,0,17,1,140,7,0,0,1,4,19,95,0,0,0,15,1,0,0, 
  3,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,210,113,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95, 
  0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128, 
  4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,52,0,0,0,253,135,2,0,5,0,0,0,2,0,0,0,6, 
  48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56,1,128,8,4,19,59,0,0,0,177,0,0,0,3,0,1,2,19,97,0,0,0,22,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0, 
  2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,4,17,1,146,115,0,0,1,8,4,17,1,145,114,0,0,1,19,95,0,0,0,19,1,0,0,1,0,1,21,1,130,0,0,0,188,134,2,0,6, 
  0,0,0,2,0,0,0,114,64,66,129,1,176,194,128,2,176,130,127,51,168,3,128,125,80,1,128,126,184,2,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,83,115,0,0, 
  1,4,15,1,20,115,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,83,115,0,0,1,4,19,95,0,0,0,18,1,0,0,3,0,1,2, 
  21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,48,67,129,1,160,195,128,2,160,131,127,51,168,3,128,125,64,2,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0, 
  19,78,0,0,0,226,0,0,0,1,0,17,1,84,116,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,84,116,0,0,1,4,15,1,21,116,0,0,17,1,140,7,0, 
  0,1,8,4,19,95,0,0,0,15,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,61,0,0,0,183,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,176,1, 
  128,2,176,129,128,11,184,65,127,118,112,1,128,4,17,1,157,118,0,0,1,4,17,1,177,117,0,0,1,8,4,15,1,240,116,0,0,17,1,98,75,0,0,1,19,81,0,0,0,251,0,0,0,1,0,1,21,1,51,0, 
  0,0,79,136,2,0,4,0,0,0,2,0,0,0,118,24,1,128,1,16,1,128,2,16,129,127,115,88,1,128,8,4,17,1,103,117,0,0,1,4,17,1,47,117,0,0,1,19,81,0,0,0,239,0,0,0,2,0,1,21, 
  1,44,0,0,0,161,136,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,250,0,0,0,4,0,1,8,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0, 
  0,0,248,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,83,118,0,0,17,1,98,75,0,0,1,4,17,1,8,118,0,0,1,21,9,27, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15, 
  1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0, 
  0,0,3,0,1,21,1,57,0,0,0,232,136,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,80,1,128,2,80,129,127,11,88,1,128,4,17,1,26,119,0,0,1,8,4,15,1,226,118,0,0,17,1,98,75,0, 
  0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,161,136,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81, 
  0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,178,1,0,0,58,137,2,0,11,0,0,0,3,0,0,0,58,80,4,130,1,40,203,128,2,40,139,127,94,168,198,129,105, 
  8,8,128,53,24,5,129,6,40,75,127,7,40,11,128,114,152,9,128,125,48,11,128,126,240,1,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1, 
  0,19,97,0,0,0,22,1,0,0,1,0,15,1,252,129,0,0,15,1,44,130,0,0,15,1,123,156,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,131,121,0, 
  0,17,1,24,121,0,0,1,4,15,1,252,129,0,0,15,1,44,130,0,0,15,1,123,156,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15, 
  1,252,129,0,0,15,1,44,130,0,0,15,1,123,156,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,252,129,0,0,15,1,44,130,0,0,15,1,123,156,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,252,129,0,0,15,1,44,130,0,0,15,1,123,156,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0, 
  0,0,22,1,0,0,1,0,15,1,252,129,0,0,15,1,44,130,0,0,15,1,123,156,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,1,39,0,0,0,224,137,2,0,3,0,0,0,1,0,0,0,2,240, 
  0,128,1,240,64,128,97,248,0,128,8,4,17,1,75,121,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58,240,0, 
  128,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,8,2,21,0,162,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,96,67,129,81,208,66,129,82,200,2,128,11,128,68, 
  128,83,96,2,128,95,248,1,128,97,144,1,128,47,240,67,126,15,1,131,121,0,0,17,1,134,127,0,0,1,15,1,131,121,0,0,17,1,127,124,0,0,1,15,1,131,121,0,0,17,1,38,122,0,0,1,1,19,83,0, 
  0,0,254,0,0,0,1,0,17,1,131,121,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,131,121,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,131,121,0,0,1,19,47,0,0,0,152,0,0,0, 
  1,0,17,1,131,121,0,0,1,2,21,1,124,1,0,0,108,138,2,0,12,0,0,0,3,0,0,0,120,64,8,128,1,168,202,129,2,168,74,128,58,176,136,129,94,16,194,129,53,16,67,129,6,168,138,127,7,168,10,128, 
  105,176,10,128,114,120,9,128,125,64,4,128,126,64,6,128,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0, 
  0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0, 
  19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,227,123,0,0,17,1,163,123,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0, 
  15,1,227,123,0,0,17,1,24,121,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,115,124,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,2,21,1,52,0,0,0,29,139,2,0,5,0,0,0,2,0,0,0,6,152,1,128,1,152,1,128,2,152,129, 
  127,7,152,65,128,119,48,1,128,4,19,82,0,0,0,252,0,0,0,4,0,1,8,19,82,0,0,0,253,0,0,0,3,0,1,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,67,129,81,136,67, 
  129,82,136,2,128,11,144,66,128,83,136,2,128,95,32,2,128,97,32,3,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1,227,123,0,0,1,15,1,227,123,0,0,17,1,127,124,0,0,1,1,19,47,0, 
  0,0,152,0,0,0,1,0,17,1,227,123,0,0,1,15,1,227,123,0,0,17,1,134,127,0,0,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,227,123,0,0,1,2,19, 
  83,0,0,0,255,0,0,0,2,0,1,21,1,63,0,0,0,111,139,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,176,1,128,2,176,129,128,11,112,65,127,118,184,1,128,12,17,1,203,124,0,0,1,12,17,1, 
  203,124,0,0,1,10,12,17,1,203,124,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,232,1,128,1,224,1,128,2,224,129,128,11,48,65, 
  127,118,160,1,128,4,15,1,197,126,0,0,17,1,98,75,0,0,1,4,17,1,217,125,0,0,1,8,4,17,1,17,125,0,0,1,2,21,1,57,0,0,0,54,140,2,0,4,0,0,0,2,0,0,0,118,128,1,128,1, 
  192,1,128,2,192,129,127,11,16,1,128,4,15,1,161,125,0,0,17,1,98,75,0,0,1,4,17,1,86,125,0,0,1,8,19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0, 
  1,21,1,44,0,0,0,182,140,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,247,0,0,0,4,0,1,8,19,81,0,0,0,237,0,0,0,3,0,1,21,7,48, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,123,126,0,0,17,1,98,75,0,0,1,4,17,1,48,126,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0, 
  128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,1,51,0,0,0,43, 
  141,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,80,1,128,2,80,129,127,115,88,1,128,4,17,1,60,127,0,0,1,8,4,17,1,4,127,0,0,1,19,81,0,0,0,239,0,0,0,2,0,1,21,1,44,0, 
  0,0,182,140,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248, 
  0,0,0,3,0,1,21,1,51,0,0,0,171,141,2,0,4,0,0,0,2,0,0,0,128,16,1,128,1,144,1,128,2,144,1,128,127,80,1,128,12,17,1,198,127,0,0,1,12,17,1,198,127,0,0,1,10,12,19,95, 
  0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,88,1,128,1,80,1,128,2,80,1,128,127,16,1,128,4,17,1,251,128,0,0,1,8,4,17,1,250,127,0,0, 
  1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,184,65,129,1,24,195,128,2,24,131,127,51,80,1,128,125,40,2,128,126,32,3,128,4,19,95,0,0,0,15,1,0,0,3,0,1,4,15,1, 
  188,128,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,125,128,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226, 
  0,0,0,1,0,17,1,125,128,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,176,66,129,1,64,194,128,2,64,130,127,51,72,2,128,125,32,3,128,126,80,1, 
  128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,189,129,0,0,1,8,4,19,95,0,0,0,18,1,0,0,3,0,1,4,15,1,126,129,0,0,17,1,140,7,0,0,1,4, 
  19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,189,129,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1, 
  0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0, 
  0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,47,0,0,0,115,142,2,0,5,0,0,0,2,0,0,0,120,56,1,128,1,48, 
  1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,163,123,0,0,1,2,21,0,157,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,200,131,128,81,168,66,129,63,56,131,128,11,88,68,127,83,64, 
  66,128,95,216,1,128,97,112,1,128,15,1,44,130,0,0,17,1,5,154,0,0,1,15,1,44,130,0,0,17,1,254,150,0,0,1,15,1,44,130,0,0,17,1,202,130,0,0,1,19,83,0,0,0,254,0,0,0,1,0, 
  17,1,44,130,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,44,130,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,44,130,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,44,130,0,0, 
  1,2,21,1,131,0,0,0,108,138,2,0,12,0,0,0,3,0,0,0,120,216,3,128,1,16,195,129,2,16,67,128,58,24,131,129,94,80,194,129,53,16,66,129,6,16,131,127,7,16,3,128,105,208,2,128,114,152,3,128, 
  125,144,2,128,126,88,3,128,12,17,1,7,149,0,0,1,12,17,1,16,147,0,0,1,12,17,1,25,145,0,0,1,12,17,1,34,143,0,0,1,10,12,17,1,43,141,0,0,1,12,17,1,52,139,0,0,1,12,17,1, 
  85,131,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,1,106,1,0,0,58,137,2,0,11,0,0,0,3,0,0,0,58,128,6,130,1,72,199,128,2,72,135,127,94,32,195,129,105,32,4,128,53, 
  80,5,129,6,72,71,127,7,72,7,128,114,240,1,128,125,80,9,128,126,80,7,128,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0, 
  1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1, 
  116,7,0,0,17,1,88,6,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1, 
  0,15,1,43,133,0,0,17,1,192,132,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0, 
  15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1, 
  0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,1,39,0,0,0,197,142,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,243,132,0, 
  0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,63, 
  0,0,0,189,0,0,0,3,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,43, 
  133,0,0,17,1,190,136,0,0,1,15,1,43,133,0,0,17,1,183,133,0,0,1,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,43,133,0,0,1,19,97,0,0,0,21, 
  1,0,0,1,0,17,1,43,133,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,43,133,0,0,1,2,21,1,63,0,0,0,111,139,2,0,5,0,0,0,2,0,0,0,115,184,1,128,1,112,1,128,2,112,129, 
  128,11,120,65,127,118,48,1,128,12,17,1,3,134,0,0,1,10,12,17,1,3,134,0,0,1,12,17,1,3,134,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0, 
  0,2,0,0,0,115,48,1,128,1,224,1,128,2,224,129,128,11,112,65,127,118,232,1,128,4,17,1,246,135,0,0,1,4,15,1,53,135,0,0,17,1,98,75,0,0,1,8,4,17,1,73,134,0,0,1,2,21,7,48, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,235,134,0,0,17,1,98,75,0,0,1,4,17,1,160,134,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0, 
  128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,1,51,0,0,0,70, 
  143,2,0,4,0,0,0,2,0,0,0,118,80,1,128,1,144,1,128,2,144,129,127,115,16,1,128,4,17,1,190,135,0,0,1,4,17,1,116,135,0,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,7,35,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81, 
  0,0,0,248,0,0,0,3,0,1,21,1,44,0,0,0,187,143,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1,19,81,0,0,0,244, 
  0,0,0,3,0,1,21,1,57,0,0,0,37,144,2,0,4,0,0,0,2,0,0,0,118,136,1,128,1,128,1,128,2,128,129,127,11,16,1,128,4,15,1,134,136,0,0,17,1,98,75,0,0,1,8,4,17,1,59,136, 
  0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,44,0,0,0,187,143,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128, 
  8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0,0,3,0,1,21,1,51,0,0,0,171,141,2,0,4,0,0,0,2,0,0,0,128,88,1,128,1,80,1,128,2,80,1,128,127,16,1,128, 
  12,17,1,254,136,0,0,1,10,12,17,1,254,136,0,0,1,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,88,1,128,1,16,1,128,2,16,1,128, 
  127,24,1,128,8,4,17,1,51,138,0,0,1,4,17,1,50,137,0,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,160,67,129,1,80,193,128,2,80,129,127,51,88,1,128,125,192,1,128, 
  126,176,2,128,8,4,19,95,0,0,0,15,1,0,0,3,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,244,137,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0, 
  19,78,0,0,0,226,0,0,0,1,0,17,1,244,137,0,0,1,4,15,1,181,137,0,0,17,1,140,7,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0, 
  0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19, 
  95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,80,65, 
  129,1,176,194,128,2,176,130,127,51,168,3,128,125,192,1,128,126,184,2,128,4,15,1,245,138,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1, 
  182,138,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,182,138,0,0,1,4,19,95,0,0,0,18,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,106,1, 
  0,0,58,137,2,0,11,0,0,0,3,0,0,0,58,136,10,130,1,128,198,128,2,128,134,127,94,32,195,129,105,32,4,128,53,80,5,129,6,128,70,127,7,128,6,128,114,240,1,128,125,136,8,128,126,136,6,128,4,15, 
  1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,17,1,204,80,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,159,140,0,0,17,1,192,132,0,0,1,2,21, 
  0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,159,140,0,0,17,1,190,136,0,0,1,15,1, 
  159,140,0,0,17,1,183,133,0,0,1,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,159,140,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,159,140,0,0,1, 
  19,47,0,0,0,152,0,0,0,1,0,17,1,159,140,0,0,1,2,21,1,106,1,0,0,58,137,2,0,11,0,0,0,3,0,0,0,58,128,6,130,1,72,199,128,2,72,135,127,94,32,195,129,105,32,4,128,53,80,5, 
  129,6,72,71,127,7,72,7,128,114,240,1,128,125,80,9,128,126,80,7,128,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4, 
  15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7, 
  0,0,17,1,88,6,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15, 
  1,150,142,0,0,17,1,192,132,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128, 
  95,216,1,128,97,112,1,128,15,1,150,142,0,0,17,1,190,136,0,0,1,15,1,150,142,0,0,17,1,183,133,0,0,1,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1, 
  150,142,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,150,142,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,150,142,0,0,1,2,21,1,106,1,0,0,58,137,2,0,11,0,0,0,3,0,0,0, 
  58,32,4,130,1,72,199,128,2,72,135,127,94,32,195,129,105,232,4,128,53,24,6,129,6,72,71,127,7,72,7,128,114,240,1,128,125,80,9,128,126,80,7,128,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0, 
  113,0,0,0,1,0,15,1,141,144,0,0,17,1,192,132,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1, 
  115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19, 
  59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2, 
  0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,141,144,0,0,17,1,190,136,0,0,1,15,1,141,144,0,0,17,1,183,133,0,0,1,1,19,83, 
  0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,141,144,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,141,144,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,141, 
  144,0,0,1,2,21,1,106,1,0,0,58,137,2,0,11,0,0,0,3,0,0,0,58,136,10,130,1,128,198,128,2,128,134,127,94,32,195,129,105,32,4,128,53,80,5,129,6,128,70,127,7,128,6,128,114,240,1,128,125, 
  136,8,128,126,136,6,128,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,115,124, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1, 
  0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,132,146,0,0, 
  17,1,192,132,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,132,146,0,0, 
  17,1,190,136,0,0,1,15,1,132,146,0,0,17,1,183,133,0,0,1,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,132,146,0,0,1,19,97,0,0,0,21,1,0,0, 
  1,0,17,1,132,146,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,132,146,0,0,1,2,21,1,106,1,0,0,58,137,2,0,11,0,0,0,3,0,0,0,58,80,5,130,1,72,199,128,2,72,135,127,94,32, 
  195,129,105,32,4,128,53,24,6,129,6,72,71,127,7,72,7,128,114,240,1,128,125,80,9,128,126,80,7,128,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0, 
  0,17,1,140,7,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,123,148,0,0,17,1,192,132,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0, 
  19,97,0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130, 
  128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,123,148,0,0,17,1,190,136,0,0,1,15,1,123,148,0,0,17,1,183,133,0,0,1,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0, 
  0,153,0,0,0,1,0,17,1,123,148,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,123,148,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,123,148,0,0,1,2,21,1,106,1,0,0,58,137,2, 
  0,11,0,0,0,3,0,0,0,58,136,10,130,1,128,198,128,2,128,134,127,94,32,195,129,105,32,4,128,53,80,5,129,6,128,70,127,7,128,6,128,114,240,1,128,125,136,8,128,126,136,6,128,4,15,1,115,124,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80, 
  0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,115,124,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22, 
  1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97, 
  0,0,0,22,1,0,0,1,0,15,1,115,124,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,114,150,0,0,17,1,192,132,0,0,1,2,21,0,139,0,0,0, 
  255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,114,150,0,0,17,1,190,136,0,0,1,15,1,114,150,0,0,17, 
  1,183,133,0,0,1,1,19,83,0,0,0,255,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,114,150,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,114,150,0,0,1,19,47,0,0,0, 
  152,0,0,0,1,0,17,1,114,150,0,0,1,2,21,1,63,0,0,0,111,139,2,0,5,0,0,0,2,0,0,0,115,176,1,128,1,240,1,128,2,240,129,128,11,112,65,127,118,48,1,128,12,17,1,74,151,0,0,1, 
  12,17,1,74,151,0,0,1,12,17,1,74,151,0,0,1,10,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128, 
  11,184,65,127,118,120,1,128,4,17,1,61,153,0,0,1,8,4,17,1,81,152,0,0,1,4,15,1,144,151,0,0,17,1,98,75,0,0,1,2,21,1,51,0,0,0,70,143,2,0,4,0,0,0,2,0,0,0,118,16, 
  1,128,1,80,1,128,2,80,129,127,115,88,1,128,4,17,1,7,152,0,0,1,8,4,17,1,207,151,0,0,1,19,81,0,0,0,239,0,0,0,2,0,1,21,1,44,0,0,0,187,143,2,0,3,0,0,0,1,0,0, 
  0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,250,0,0,0,4,0,1,8,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0, 
  128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,7,48,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,243,152,0,0,17,1,98,75,0,0,1,4,17,1,168,152,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81, 
  0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,1,57,0,0,0,37,144,2,0,4, 
  0,0,0,2,0,0,0,118,24,1,128,1,16,1,128,2,16,129,127,11,88,1,128,8,4,17,1,186,153,0,0,1,4,15,1,130,153,0,0,17,1,98,75,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1, 
  44,0,0,0,187,143,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0, 
  0,0,246,0,0,0,3,0,1,21,1,51,0,0,0,171,141,2,0,4,0,0,0,2,0,0,0,128,16,1,128,1,144,1,128,2,144,1,128,127,80,1,128,12,17,1,69,154,0,0,1,12,17,1,69,154,0,0,1,10, 
  12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,80,1,128,1,144,1,128,2,144,1,128,127,16,1,128,4,17,1,122,155,0,0,1,4,17,1,121,154, 
  0,0,1,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,48,67,129,1,160,195,128,2,160,131,127,51,168,3,128,125,80,1,128,126,64,2,128,4,19,93,0,0,0,11,1,0,0,1,0,19, 
  78,0,0,0,225,0,0,0,1,0,17,1,59,155,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,59,155,0,0,1,4,15,1,252,154,0,0,17,1,140,7,0,0, 
  1,8,4,19,95,0,0,0,15,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,80,65,129,1,192,193,128,2,192,129,127,51,168,3,128,125,184,2,128, 
  126,200,1,128,4,15,1,60,156,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,253,155,0,0,1,4,19,93,0,0,0,11,1,0,0,1, 
  0,19,78,0,0,0,225,0,0,0,1,0,17,1,253,155,0,0,1,4,19,95,0,0,0,18,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0, 
  0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19, 
  95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,83,0,0,0,254,0,0,0,1,0,1,21,1,52,0,0,0,253,135,2, 
  0,5,0,0,0,2,0,0,0,6,152,1,128,1,152,193,128,2,152,129,127,7,152,1,128,85,48,1,128,4,19,47,0,0,0,151,0,0,0,3,0,1,8,2,19,45,0,0,0,148,0,0,0,1,0,1,21,0,213,0, 
  0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,24,6,128,60,144,4,128,61,40,4,128,46,176,133,126,47,32,197,126,95,56,2,128,97,208,1,128,15,1,200,156,0,0, 
  17,1,132,243,0,0,1,15,1,200,156,0,0,17,1,125,240,0,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,200,156,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,200,156,0,0,1,15,1,200,156, 
  0,0,17,1,11,240,0,0,1,15,1,200,156,0,0,17,1,152,203,0,0,1,19,46,0,0,0,149,0,0,0,1,0,17,1,200,156,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,200,156,0,0,1,15,1, 
  200,156,0,0,17,1,158,157,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,200,156,0,0,1,2,21,1,119,0,0,0,154,144,2,0,11,0,0,0,3,0,0,0,24,184,2,128,1,176,194,129,2,176,194,129, 
  23,56,3,128,84,240,1,128,21,112,66,129,6,176,2,128,7,176,2,127,25,120,3,128,58,248,2,128,85,48,2,128,12,17,1,29,158,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,10,12,17,1, 
  27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,48,1,128,1, 
  112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,77,158,0,0,1,8,2,21,1,36,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,88,80,130,1,96,205,129,2,96,141,127,94,152,14,130,44, 
  104,13,128,53,192,70,129,6,96,77,127,7,96,13,128,105,176,8,128,114,16,2,128,125,0,4,128,126,160,10,128,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0, 
  1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3, 
  0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1, 
  88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246, 
  0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15, 
  1,176,30,1,0,17,1,101,119,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,221,160,0,0,17,1,114,160,0,0,1,2,21,1,39,0,0,0,105,145,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128, 
  97,248,0,128,8,4,17,1,165,160,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58,240,0,128,4,19,35,0,0, 
  0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,8,2,21,0,195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61, 
  40,4,128,46,128,133,126,47,240,196,126,95,56,2,128,97,208,1,128,15,1,221,160,0,0,17,1,34,201,0,0,1,15,1,221,160,0,0,17,1,27,198,0,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,221,160, 
  0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,221,160,0,0,1,15,1,221,160,0,0,17,1,169,197,0,0,1,15,1,221,160,0,0,17,1,161,161,0,0,1,19,46,0,0,0,150,0,0,0,3,0,1,19, 
  97,0,0,0,21,1,0,0,1,0,17,1,221,160,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,221,160,0,0,1,2,21,1,143,0,0,0,11,146,2,0,13,0,0,0,3,0,0,0,58,112,3,130,1, 
  240,3,130,2,240,131,127,94,176,67,130,44,56,4,128,53,176,130,129,6,240,67,127,7,240,3,128,98,240,130,128,105,48,2,128,114,248,3,128,125,112,2,128,126,48,3,128,12,17,1,128,195,0,0,1,12,17,1,87,193, 
  0,0,1,12,17,1,46,191,0,0,1,12,17,1,5,189,0,0,1,12,17,1,220,186,0,0,1,6,17,1,184,168,0,0,1,12,17,1,143,166,0,0,1,10,12,17,1,102,164,0,0,1,12,17,1,61,162,0,0,1, 
  12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,8,130,1,40,9,130,2,40,137,127,94,96,69,130,44,48,2,128,53,96,138,129,6,40,73,127,7, 
  40,9,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0, 
  0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,214,163,0,0,17, 
  1,114,160,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0, 
  1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,128,3,128,11,240,1,128,97,24,3,128,61,16,3,128,62,16,3,128,47,128,66,126,19,61,0,0,0,185,0,0, 
  0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,214,163,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,214,163,0,0,1,1,15,1,214,163,0,0,17,1,34,201,0,0,1,15,1,214,163,0,0, 
  17,1,27,198,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,214,163,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,40,9,130,2,40,137,127,94,40,70,130, 
  44,248,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,104,133,128,105,48,9,128,114,144,11,128,125,104,3,128,126,40,7,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,255,165,0,0,17,1,114,160,0,0, 
  1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0, 
  15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0, 
  15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,130,128,81,144,193,128,95,128,3,128,11,88,2,128,97,240,1,128,61,232,2,128,62,232,2, 
  128,47,232,67,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,255,165,0,0,17,1,34,201,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,255,165,0,0,1,1,19,47,0,0,0,153,0,0,0,1,0, 
  17,1,255,165,0,0,1,15,1,255,165,0,0,17,1,27,198,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,255,165,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,8, 
  130,1,40,9,130,2,40,137,127,94,96,69,130,44,48,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119, 
  0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0, 
  17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80, 
  0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0, 
  17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,40,168,0,0,17,1,114,160,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1, 
  116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116, 
  0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,130,128,81,144,193,128,95,240,1,128,11,120, 
  3,128,97,16,4,128,61,8,4,128,62,8,4,128,47,88,66,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,40,168,0,0,17,1,27,198,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,40,168,0,0, 
  1,19,47,0,0,0,153,0,0,0,1,0,17,1,40,168,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,40,168,0,0,1,1,15,1,40,168,0,0,17,1,34,201,0,0,1,2,21,1,212,0,0,0,55,147, 
  2,0,19,0,0,0,4,0,0,0,128,216,4,128,1,24,6,131,2,24,134,131,3,112,195,131,4,112,3,129,53,24,4,128,6,24,6,128,7,24,6,128,84,48,3,128,105,240,2,128,94,216,5,128,11,88,4,128,44,96, 
  6,128,97,216,3,128,62,152,4,127,127,24,5,128,98,88,69,128,114,32,6,128,115,152,5,128,12,17,1,128,195,0,0,1,12,17,1,179,184,0,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,12,17,1,138,182, 
  0,0,1,12,17,1,46,191,0,0,1,12,17,1,97,180,0,0,1,12,17,1,56,178,0,0,1,12,17,1,15,176,0,0,1,12,17,1,230,173,0,0,1,12,17,1,5,189,0,0,1,12,17,1,189,171,0,0,1,12, 
  17,1,143,166,0,0,1,10,12,17,1,102,164,0,0,1,12,17,1,61,162,0,0,1,12,17,1,148,169,0,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,8,130,1,40,9,130,2,40, 
  137,127,94,96,69,130,44,48,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0, 
  0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0, 
  0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1, 
  4,19,35,0,0,0,113,0,0,0,1,0,15,1,45,171,0,0,17,1,114,160,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88, 
  6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,32,194,128,95,160,3,128,11,16,3,128,97,16,4,128,61, 
  8,4,128,62,8,4,128,47,128,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,45,171,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,45,171,0,0,1,19, 
  47,0,0,0,152,0,0,0,1,0,17,1,45,171,0,0,1,15,1,45,171,0,0,17,1,27,198,0,0,1,1,15,1,45,171,0,0,17,1,34,201,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3, 
  0,0,0,58,160,4,130,1,40,9,130,2,40,137,127,94,40,70,130,44,48,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,104,133,128,105,48,9,128,114,144,11,128,125,160,2,128,126,40,7,128,4,15,1,42,15, 
  0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0, 
  15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,86,173,0,0,17,1,114,160,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2, 
  0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,120,131,128,81,144,193,128, 
  95,8,4,128,11,240,1,128,97,16,3,128,61,112,4,128,62,112,4,128,47,128,66,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,86,173,0,0,1,19,97,0,0,0,21, 
  1,0,0,1,0,17,1,86,173,0,0,1,15,1,86,173,0,0,17,1,34,201,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,86,173,0,0,1,15,1,86,173,0,0,17,1,27,198,0,0,1,1,2,21,1, 
  152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,8,130,1,40,9,130,2,40,137,127,94,96,69,130,44,48,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,160,132,128,105,48,9,128,114,144,11,128, 
  125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,127,175,0,0,17,1,114,160,0,0,1,8,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1, 
  0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,63,240,129,128,81,144,193,128,95,128,2,128,11,232,3,128,97,232,2,128,61,80,3,128,62,80,3,128,47,88,67,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0, 
  17,1,127,175,0,0,1,15,1,127,175,0,0,17,1,27,198,0,0,1,15,1,127,175,0,0,17,1,34,201,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,127,175,0,0,1,19,47,0,0,0,152,0,0, 
  0,1,0,17,1,127,175,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,104,8,130,1,96,8,130,2,96,136,127,94,96,69,130,44,48,2,128,53,96,138,129,6,96,72,127,7,96,8, 
  128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59, 
  0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0, 
  2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59, 
  0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,168,177,0,0,17,1, 
  114,160,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2, 
  21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,32,194,128,95,128,3,128,11,144,1,128,97,16,3,128,61,120,3,128,62,120,3,128,47,128,66,126,19,47,0,0,0,152,0,0,0,1, 
  0,17,1,168,177,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,168,177,0,0,1,15,1,168,177,0,0,17,1,34,201,0,0,1,1,15,1,168,177,0,0,17,1, 
  27,198,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,168,177,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,160,4,130,1,40,9,130,2,40,137,127,94,40,70,130,44,48, 
  2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,104,133,128,105,48,9,128,114,144,11,128,125,160,2,128,126,40,7,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0, 
  19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0, 
  0,0,1,0,15,1,209,179,0,0,17,1,114,160,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15, 
  1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,129,128,81,144,193,128,95,16,3,128,11,232,3,128,97,120,3,128,61,224,3,128,62,224,3,128,47, 
  128,66,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,209,179,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,209,179,0,0,1,15,1,209,179,0,0,17,1,27, 
  198,0,0,1,15,1,209,179,0,0,17,1,34,201,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,209,179,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,104,8,130,1, 
  96,8,130,2,96,136,127,94,96,69,130,44,48,2,128,53,96,138,129,6,96,72,127,7,96,8,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0, 
  1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0, 
  1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,250,181,0,0,17,1,114,160,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7, 
  0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,32,194,128,95,16,3,128,11,128,3,128, 
  97,16,4,128,61,120,3,128,62,120,3,128,47,128,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,250,181,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,250, 
  181,0,0,1,15,1,250,181,0,0,17,1,27,198,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,250,181,0,0,1,15,1,250,181,0,0,17,1,34,201,0,0,1,2,21,1,152,1,0,0,112,111,2,0, 
  13,0,0,0,3,0,0,0,58,200,10,130,1,96,8,130,2,96,136,127,94,96,69,130,44,48,2,128,53,152,137,129,6,96,72,127,7,96,8,128,98,160,132,128,105,104,8,128,114,144,11,128,125,160,2,128,126,96,6,128, 
  4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15, 
  1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15, 
  1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1, 
  30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,35,184,0,0,17,1,114,160,0,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,130, 
  128,81,144,193,128,95,16,4,128,11,24,3,128,97,168,3,128,61,16,3,128,62,16,3,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,35,184,0,0,1,19, 
  47,0,0,0,153,0,0,0,1,0,17,1,35,184,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,35,184,0,0,1,15,1,35,184,0,0,17,1,34,201,0,0,1,15,1,35,184,0,0,17,1,27,198,0, 
  0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,160,4,130,1,40,9,130,2,40,137,127,94,40,70,130,44,48,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,104,133,128,105,48,9, 
  128,114,144,11,128,125,160,2,128,126,40,7,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1, 
  0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,76,186,0,0,17,1,114,160,0,0,1,4,19,84,0, 
  0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1, 
  0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1, 
  30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0, 
  15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255, 
  255,255,8,0,0,0,3,0,0,0,63,80,131,128,81,144,193,128,95,240,1,128,11,88,2,128,97,232,2,128,61,224,3,128,62,224,3,128,47,232,67,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,76,186,0,0, 
  17,1,27,198,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,76,186,0,0,1,15,1,76,186,0,0,17,1,34,201,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,76,186,0,0,1,1,19,97,0, 
  0,0,21,1,0,0,1,0,17,1,76,186,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,104,8,130,1,96,8,130,2,96,136,127,94,96,69,130,44,48,2,128,53,96,138,129,6,96, 
  72,127,7,96,8,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0, 
  0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,117, 
  188,0,0,17,1,114,160,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0, 
  0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140, 
  7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,130,128,81,144,193,128,95,16,4,128,11,120,3,128,97,240,1,128,61,8,4,128,62,8,4,128,47,232,66,126,19,61,0,0,0, 
  185,0,0,0,2,0,1,15,1,117,188,0,0,17,1,34,201,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,117,188,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,117,188,0,0,1,19,47,0,0, 
  0,152,0,0,0,1,0,17,1,117,188,0,0,1,1,15,1,117,188,0,0,17,1,27,198,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,96,8,130,2,96,136,127,94, 
  96,69,130,44,48,2,128,53,152,137,129,6,96,72,127,7,96,8,128,98,160,132,128,105,104,8,128,114,200,10,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11, 
  1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84, 
  0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12, 
  1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0, 
  0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0, 
  0,0,1,0,15,1,158,190,0,0,17,1,114,160,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,130,128,81,144,193,128,95,232,2,128,11,88,3,128,97,240,1,128,61,80,3,128, 
  62,80,3,128,47,232,67,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,158,190,0,0,17,1,34,201,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,158,190,0,0,1,15,1,158,190,0,0,17,1,27, 
  198,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,158,190,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,158,190,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0, 
  58,96,8,130,1,40,9,130,2,40,137,127,94,96,69,130,44,48,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2,128,126,96,6,128,4,15,1,42,15,0,0,17, 
  1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159, 
  116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17, 
  1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159, 
  116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,199,192,0,0,17,1,114,160,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,130,128,81,248,193,128,95,144,1, 
  128,11,120,3,128,97,8,4,128,61,112,4,128,62,112,4,128,47,88,66,126,15,1,199,192,0,0,17,1,27,198,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,199, 
  192,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,199,192,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,199,192,0,0,1,15,1,199,192,0,0,17,1,34,201,0,0,1,1,2,21,1,152,1,0, 
  0,112,111,2,0,13,0,0,0,3,0,0,0,58,104,8,130,1,96,8,130,2,96,136,127,94,96,69,130,44,48,2,128,53,96,138,129,6,96,72,127,7,96,8,128,98,160,132,128,105,48,9,128,114,144,11,128,125,160,2, 
  128,126,96,6,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1, 
  0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1, 
  0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,240,194,0,0,17,1,114,160,0,0,1,4,15,1,30,15,0,0,15,1,159,116, 
  0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1, 
  181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0, 
  0,0,63,240,129,128,81,144,193,128,95,24,3,128,11,128,2,128,97,128,3,128,61,16,3,128,62,16,3,128,47,232,67,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,240, 
  194,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,240,194,0,0,1,1,15,1,240,194,0,0,17,1,27,198,0,0,1,15,1,240,194,0,0,17,1,34,201,0,0,1,19,97,0,0,0,21,1,0,0,1,0, 
  17,1,240,194,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,40,9,130,2,40,137,127,94,40,70,130,44,248,2,128,53,96,138,129,6,40,73,127,7,40,9,128,98,104, 
  133,128,105,48,9,128,114,144,11,128,125,104,3,128,126,40,7,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,25,197,0,0,17,1,114,160,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93, 
  0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0, 
  1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,94, 
  0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0, 
  1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143, 
  0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,129,128,81,144,193,128,95,128,2,128,11,232,3,128,97,232,2,128,61,80,3,128,62,80,3,128,47,88,67,126,19,61,0,0,0,185,0,0,0,2,0,1,19, 
  47,0,0,0,153,0,0,0,1,0,17,1,25,197,0,0,1,15,1,25,197,0,0,17,1,27,198,0,0,1,15,1,25,197,0,0,17,1,34,201,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,25,197,0, 
  0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,25,197,0,0,1,2,21,1,47,0,0,0,241,147,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12, 
  17,1,229,197,0,0,1,10,12,19,60,0,0,0,182,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8, 
  4,15,1,196,74,0,0,17,1,128,33,0,0,1,2,21,1,63,0,0,0,162,148,2,0,5,0,0,0,2,0,0,0,115,184,1,128,1,176,1,128,2,176,129,128,11,48,65,127,118,112,1,128,12,17,1,103,198,0,0, 
  1,12,17,1,103,198,0,0,1,10,12,17,1,103,198,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,232,1,128,1,160,1,128,2,160,129, 
  128,11,48,65,127,118,168,1,128,4,15,1,97,200,0,0,17,1,98,75,0,0,1,8,4,17,1,117,199,0,0,1,4,17,1,173,198,0,0,1,2,21,1,57,0,0,0,186,149,2,0,4,0,0,0,2,0,0,0,118, 
  16,1,128,1,80,1,128,2,80,129,127,11,88,1,128,4,17,1,42,199,0,0,1,8,4,15,1,242,198,0,0,17,1,98,75,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,92,150,2,0, 
  3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0, 
  1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,22,200,0,0,1,4,15,1,204,199,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0, 
  4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,1,51, 
  0,0,0,243,150,2,0,4,0,0,0,2,0,0,0,118,88,1,128,1,16,1,128,2,16,129,127,115,24,1,128,8,4,17,1,234,200,0,0,1,4,17,1,160,200,0,0,1,19,81,0,0,0,239,0,0,0,2,0,1, 
  21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0, 
  128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1,19,81, 
  0,0,0,244,0,0,0,3,0,1,21,1,51,0,0,0,149,151,2,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,12,17,1,98,201,0,0,1,10,12,17,1,98,201,0,0, 
  1,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,88,1,128,1,80,1,128,2,80,1,128,127,16,1,128,4,17,1,151,202,0,0,1,8,4,17,1, 
  150,201,0,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,64,66,129,1,176,194,128,2,176,130,127,51,168,3,128,125,184,2,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0, 
  19,78,0,0,0,226,0,0,0,1,0,17,1,88,202,0,0,1,4,15,1,25,202,0,0,17,1,140,7,0,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,88,202, 
  0,0,1,4,19,95,0,0,0,15,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,80,65,129,1,160,195,128,2,160,131,127,51,168,3,128,125,176,2, 
  128,126,192,1,128,4,15,1,89,203,0,0,17,1,140,7,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,26,203,0,0,1,4,19,93,0,0,0,11,1,0,0,1, 
  0,19,78,0,0,0,225,0,0,0,1,0,17,1,26,203,0,0,1,8,4,19,95,0,0,0,18,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0, 
  0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4, 
  19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,143,0,0,0,11,146,2,0,13,0,0,0,3,0,0,0,58,56, 
  4,130,1,112,3,130,2,112,131,127,94,176,66,130,44,48,3,128,53,248,131,129,6,112,67,127,7,112,3,128,98,240,130,128,105,184,3,128,114,48,2,128,125,112,2,128,126,120,3,128,12,17,1,226,237,0,0,1,12,17, 
  1,185,235,0,0,1,12,17,1,144,233,0,0,1,12,17,1,103,231,0,0,1,12,17,1,62,229,0,0,1,10,12,17,1,21,227,0,0,1,12,17,1,236,224,0,0,1,12,17,1,195,222,0,0,1,6,17,1,52,204, 
  0,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,212,0,0,0,55,147,2,0,19,0,0,0,4,0,0,0,128,224,5,128,1,216,5,131,2,216,133,131,3,112,195,131,4,112,3,129,53,96,6,128,6,216, 
  5,128,7,216,5,128,84,88,4,128,105,32,6,128,94,152,4,128,11,24,5,128,44,88,5,128,97,48,3,128,62,152,5,127,127,24,4,128,98,216,68,128,114,216,3,128,115,240,2,128,12,17,1,154,220,0,0,1,12,17, 
  1,113,218,0,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,12,17,1,226,237,0,0,1,12,17,1,31,214,0,0,1,12,17,1,246,211,0,0,1,12,17,1,144,233,0,0,1,12,17,1,103,231,0,0,1,12, 
  17,1,205,209,0,0,1,12,17,1,62,229,0,0,1,12,17,1,164,207,0,0,1,10,12,17,1,16,205,0,0,1,12,17,1,236,224,0,0,1,12,17,1,195,222,0,0,1,12,17,1,72,216,0,0,1,21,1,152,1, 
  0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,88,6,130,1,80,6,130,2,80,134,127,94,32,68,130,44,32,7,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,144, 
  7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1, 
  0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,20,207,0,0,17,1,169,206,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19, 
  93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0, 
  0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17, 
  1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,1,39,0,0,0,105,145,2,0,3,0,0,0,1, 
  0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,220,206,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176, 
  1,128,58,240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,16,131,128,81,32,194,128,95,160, 
  3,128,11,128,2,128,97,16,4,128,61,8,4,128,62,8,4,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1,20,207,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0, 
  0,1,0,17,1,20,207,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,20,207,0,0,1,15,1,20,207,0,0,17,1,125,240,0,0,1,1,15,1,20,207,0,0,17,1,132,243,0,0,1,2,21,1,152,1, 
  0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,4,130,1,24,7,130,2,24,135,127,94,232,68,130,44,32,7,128,53,240,130,129,6,24,71,127,7,24,7,128,98,48,130,128,105,232,5,128,114,144,11,128,125,144, 
  7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1, 
  0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,61,209,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1, 
  204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19, 
  93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0, 
  0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17, 
  1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3, 
  0,0,0,63,128,130,128,81,32,194,128,95,16,3,128,11,128,3,128,97,16,4,128,61,120,3,128,62,120,3,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1,61,209,0,0,1,19,61,0,0,0,185,0, 
  0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,61,209,0,0,1,15,1,61,209,0,0,17,1,125,240,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,61,209,0,0,1,15,1,61,209,0, 
  0,17,1,132,243,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,4,130,1,24,7,130,2,24,135,127,94,232,68,130,44,32,7,128,53,240,130,129,6,24,71,127,7,24,7,128,98, 
  48,130,128,105,232,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,102,211,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1, 
  42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15, 
  0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0, 
  15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0, 
  143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,240,1,128,11,88,2,128,97,232,2,128,61,80,3,128,62,80,3,128,47,88,67,126,19,61,0,0,0,185,0,0,0,2,0,1, 
  15,1,102,211,0,0,17,1,125,240,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,102,211,0,0,1,15,1,102,211,0,0,17,1,132,243,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,102,211, 
  0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,102,211,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,88,6,130,1,80,6,130,2,80,134,127,94,32,68,130,44,32,7,128, 
  53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17, 
  1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,143, 
  213,0,0,17,1,169,206,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97, 
  0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0, 
  1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48, 
  8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,32,194,128,95,16,3,128,11,128,3,128,97,16,4,128,61,120,3,128,62,120,3,128,47,128,66, 
  126,19,47,0,0,0,153,0,0,0,1,0,17,1,143,213,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,143,213,0,0,1,15,1,143,213,0,0,17,1,125,240,0, 
  0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,143,213,0,0,1,15,1,143,213,0,0,17,1,132,243,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,10,130,1,80,6, 
  130,2,80,134,127,94,32,68,130,44,88,6,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,200,6,128,126,200,8,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0, 
  0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15, 
  1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30, 
  15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1, 
  0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,184,215,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,130,128,81,248,193,128,95,16,4,128,11,120,3,128,97,144, 
  1,128,61,8,4,128,62,8,4,128,47,232,66,126,15,1,184,215,0,0,17,1,132,243,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,184,215,0,0,1,19,97,0, 
  0,0,21,1,0,0,1,0,17,1,184,215,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,184,215,0,0,1,1,15,1,184,215,0,0,17,1,125,240,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0, 
  0,0,3,0,0,0,58,200,6,130,1,80,6,130,2,80,134,127,94,32,68,130,44,88,6,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116, 
  7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,225,217,0,0,17,1,169,206,0,0,1,4,19,93,0,0,0,11,1,0,0, 
  1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0, 
  12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,130,128,81, 
  136,194,128,95,16,4,128,11,144,1,128,97,32,2,128,61,8,4,128,62,8,4,128,47,120,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,225,217,0,0,1,15,1,225,217,0,0,17,1,132,243,0,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,225,217,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,225,217,0,0,1,1,15,1,225,217,0,0,17,1,125,240,0,0,1, 
  2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,88,6,130,1,80,6,130,2,80,134,127,94,32,68,130,44,32,7,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114, 
  144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0, 
  15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,10,220,0,0,17,1,169,206,0,0,1,4,15,1,42,15,0,0,17,1,101,119, 
  0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0, 
  17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255, 
  8,0,0,0,3,0,0,0,63,16,131,128,81,32,194,128,95,16,4,128,11,144,1,128,97,160,3,128,61,8,4,128,62,8,4,128,47,128,66,126,19,47,0,0,0,152,0,0,0,1,0,17,1,10,220,0,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,10,220,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,10,220,0,0,1,15,1,10,220,0,0,17,1,132,243,0,0,1,1, 
  15,1,10,220,0,0,17,1,125,240,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,4,130,1,24,7,130,2,24,135,127,94,232,68,130,44,32,7,128,53,240,130,129,6,24,71,127, 
  7,24,7,128,98,48,130,128,105,232,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0, 
  0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,51,222,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0, 
  1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1, 
  0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22, 
  1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0, 
  0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,80,131,128,81,32,194,128,95,232,2,128,11,232,3,128,97,128,2,128,61,224,3,128,62,224,3,128,47,144,65,126,19,97,0,0,0,21,1, 
  0,0,1,0,17,1,51,222,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,51,222,0,0,17,1,132,243,0,0,1,15,1,51,222,0,0,17,1,125,240,0,0,1,19,47,0,0,0,153,0,0,0,1,0, 
  17,1,51,222,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,51,222,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,5,130,1,24,7,130,2,24,135,127,94,32,68, 
  130,44,32,7,128,53,240,130,129,6,24,71,127,7,24,7,128,98,48,130,128,105,232,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0, 
  1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,92,224,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,136,194,128,95,144,1,128,11,232,2,128,97,120,3,128,61,224,3,128,62,224, 
  3,128,47,248,65,126,15,1,92,224,0,0,17,1,125,240,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,92,224,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0, 
  17,1,92,224,0,0,1,15,1,92,224,0,0,17,1,132,243,0,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,92,224,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200, 
  10,130,1,80,6,130,2,80,134,127,94,32,68,130,44,88,6,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,200,6,128,126,200,8,128,4,19,84,0,0,0,0,1,0,0, 
  1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0, 
  0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0, 
  22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,133,226,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,130,128,81,248,193,128,95,16,4,128,11, 
  240,2,128,97,144,1,128,61,232,2,128,62,232,2,128,47,128,67,126,15,1,133,226,0,0,17,1,132,243,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,133,226,0, 
  0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,133,226,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,133,226,0,0,1,15,1,133,226,0,0,17,1,125,240,0,0,1,2,21,1,152,1,0,0,112, 
  111,2,0,13,0,0,0,3,0,0,0,58,200,10,130,1,80,6,130,2,80,134,127,94,32,68,130,44,88,6,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,200,6,128,126, 
  200,8,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1, 
  181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,174,228,0,0,17,1,169,206, 
  0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0, 
  63,200,130,128,81,96,194,128,95,248,1,128,11,88,3,128,97,144,1,128,61,192,2,128,62,192,2,128,47,232,67,126,15,1,174,228,0,0,17,1,132,243,0,0,1,15,1,174,228,0,0,17,1,125,240,0,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,174,228,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,174,228,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1, 
  174,228,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,4,130,1,24,7,130,2,24,135,127,94,232,68,130,44,32,7,128,53,240,130,129,6,24,71,127,7,24,7,128,98,48,130,128, 
  105,232,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,215,230,0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0, 
  0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15, 
  1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30, 
  15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0, 
  0,255,255,255,255,8,0,0,0,3,0,0,0,63,24,131,128,81,144,193,128,95,16,4,128,11,128,2,128,97,168,3,128,61,16,3,128,62,16,3,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0, 
  0,0,21,1,0,0,1,0,17,1,215,230,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,215,230,0,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,215,230,0,0,1,15,1,215,230,0,0,17,1, 
  132,243,0,0,1,15,1,215,230,0,0,17,1,125,240,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,88,6,130,1,80,6,130,2,80,134,127,94,32,68,130,44,32,7,128,53,240,130, 
  129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80, 
  0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,0,233,0,0, 
  17,1,169,206,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0, 
  22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0, 
  17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,240,1,128,11,88,3,128,97,232,2,128,61,80,3,128,62,80,3,128,47,88,66,126,19,61, 
  0,0,0,185,0,0,0,2,0,1,15,1,0,233,0,0,17,1,125,240,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,0,233,0,0,1,15,1,0,233,0,0,17,1,132,243,0,0,1,1,19,47,0,0,0, 
  152,0,0,0,1,0,17,1,0,233,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,0,233,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,4,130,1,24,7,130,2,24, 
  135,127,94,232,68,130,44,32,7,128,53,240,130,129,6,24,71,127,7,24,7,128,98,48,130,128,105,232,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187, 
  0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,41,235, 
  0,0,17,1,169,206,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1, 
  0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,176,194,128,95,160,3,128,11,16,3,128,97,16,4,128,61, 
  8,4,128,62,8,4,128,47,32,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,41,235,0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,41,235,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19, 
  47,0,0,0,152,0,0,0,1,0,17,1,41,235,0,0,1,15,1,41,235,0,0,17,1,125,240,0,0,1,1,15,1,41,235,0,0,17,1,132,243,0,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3, 
  0,0,0,58,48,2,130,1,24,7,130,2,24,135,127,94,232,68,130,44,32,7,128,53,184,131,129,6,24,71,127,7,24,7,128,98,248,130,128,105,232,5,128,114,144,11,128,125,144,7,128,126,144,9,128,4,19,35,0,0, 
  0,113,0,0,0,1,0,15,1,82,237,0,0,17,1,169,206,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19, 
  78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0, 
  0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,80,131,128,81,136,194,128, 
  95,232,2,128,11,232,3,128,97,32,2,128,61,224,3,128,62,224,3,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1,82,237,0,0,1,15,1,82,237,0,0,17,1,132,243,0,0,1,19,61,0,0,0, 
  185,0,0,0,2,0,1,15,1,82,237,0,0,17,1,125,240,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,82,237,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,82,237,0,0,1,2,21,1, 
  152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,88,6,130,1,80,6,130,2,80,134,127,94,32,68,130,44,32,7,128,53,240,130,129,6,80,70,127,7,80,6,128,98,48,130,128,105,32,5,128,114,144,11,128, 
  125,144,7,128,126,144,9,128,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200, 
  30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,123,239,0,0,17,1,169,206,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1, 
  4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82, 
  114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0, 
  0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,63,144,129,128,81,32,194,128,95,232,2,128,11,88,3,128,97,128,2,128,61,80,3,128,62,80,3,128,47,232,67,126,19,47,0,0,0,153,0,0,0,1,0,17,1,123,239,0,0,1,19,61,0,0,0, 
  185,0,0,0,2,0,1,15,1,123,239,0,0,17,1,132,243,0,0,1,15,1,123,239,0,0,17,1,125,240,0,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,123,239,0,0,1,19,97,0,0,0,21,1,0, 
  0,1,0,17,1,123,239,0,0,1,2,21,1,47,0,0,0,241,147,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,10,12,17,1,71,240,0,0,1,12,19, 
  60,0,0,0,182,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,15,1,196,74,0,0,17,1,128, 
  33,0,0,1,2,21,1,63,0,0,0,162,148,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,120,65,127,118,184,1,128,12,17,1,201,240,0,0,1,10,12,17,1,201,240,0,0,1, 
  12,17,1,201,240,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,120,1,128,1,112,1,128,2,112,129,128,11,184,65,127,118,48,1,128,4, 
  17,1,152,242,0,0,1,8,4,17,1,208,241,0,0,1,4,15,1,15,241,0,0,17,1,98,75,0,0,1,2,21,1,51,0,0,0,243,150,2,0,4,0,0,0,2,0,0,0,118,88,1,128,1,16,1,128,2,16,129, 
  127,115,24,1,128,8,4,17,1,152,241,0,0,1,4,17,1,78,241,0,0,1,19,81,0,0,0,239,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0, 
  0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0, 
  0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1,19,81,0,0,0,244,0,0,0,3,0,1,21,1,57,0,0,0,186,149,2,0,4,0,0,0,2,0, 
  0,0,118,128,1,128,1,192,1,128,2,192,129,127,11,16,1,128,4,15,1,96,242,0,0,17,1,98,75,0,0,1,4,17,1,21,242,0,0,1,8,19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0, 
  0,246,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0, 
  0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,57,243,0,0,1,4,15,1,239,242,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243, 
  0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1, 
  21,1,51,0,0,0,149,151,2,0,4,0,0,0,2,0,0,0,128,24,1,128,1,16,1,128,2,16,1,128,127,88,1,128,10,12,17,1,196,243,0,0,1,12,17,1,196,243,0,0,1,12,19,95,0,0,0,19,1,0, 
  0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,4,17,1,249,244,0,0,1,8,4,17,1,248,243,0,0,1,2,21,1,130,0, 
  0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,56,67,129,1,48,195,128,2,48,131,127,51,168,3,128,125,64,2,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1, 
  0,17,1,186,244,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,186,244,0,0,1,8,4,15,1,123,244,0,0,17,1,140,7,0,0,1,4,19,95,0,0,0,18, 
  1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,168,66,129,1,24,195,128,2,24,131,127,51,64,2,128,125,32,3,128,126,80,1,128,4,19,94,0,0, 
  0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,187,245,0,0,1,4,19,95,0,0,0,15,1,0,0,3,0,1,4,15,1,124,245,0,0,17,1,140,7,0,0,1,8,4,19,93,0,0,0,11, 
  1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,187,245,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4, 
  0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,46,0,0,0,149,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0, 
  6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,15,1,196,74,0,0,17,1,128,33,0,0,1,19,60,0,0,0,182,0,0,0,1,0,1,21,1,63,0,0,0,174,152,2,0,5,0,0, 
  0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,98,56,1,128,8,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,19,62,0,0,0,188,0,0,0,1, 
  0,1,21,0,157,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,200,131,128,61,96,195,128,63,208,194,128,11,88,68,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,146,246,0,0,17,1,58,28,1,0, 
  1,15,1,146,246,0,0,17,1,51,25,1,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,146,246,0,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,146,246,0,0,1,15,1,146,246,0,0,17,1,48,247, 
  0,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,146,246,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,146,246,0,0,1,2,21,1,227,0,0,0,11,146,2,0,20,0,0,0,4,0,0,0,23,16, 
  4,128,1,208,5,128,2,208,133,131,53,16,3,130,84,152,6,128,21,216,134,127,6,208,5,128,7,208,69,126,24,80,4,128,25,16,5,130,58,80,3,128,85,144,4,128,44,80,5,128,125,208,4,128,62,144,67,128,94,216, 
  5,129,98,208,131,128,105,88,6,128,114,24,6,128,126,144,5,128,12,17,1,42,23,1,0,1,6,17,1,57,10,1,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17, 
  1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,48,8,1,0,1,12,17,1,27,47,0,0,1,12,17,1,39,6,1,0,1,12,17,1,30,4,1,0,1,10,12,17,1,21,2,1,0,1,12,17,1,12,0, 
  1,0,1,12,17,1,27,248,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,16,66,130,1,120,198, 
  129,2,120,134,127,94,224,8,130,44,216,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,3,250,0,0,17, 
  1,152,249,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19, 
  94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0, 
  0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0, 
  0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,1,39,0, 
  0,0,0,153,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,97,248,0,128,8,4,17,1,203,249,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0, 
  0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,3,250,0,0,17,1,150,253,0,0,1,15,1,3,250,0,0,17,1,143,250,0,0,1,19,61,0,0,0, 
  185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,3,250,0,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,3,250,0,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,3,250,0, 
  0,1,2,21,1,63,0,0,0,162,148,2,0,5,0,0,0,2,0,0,0,115,184,1,128,1,176,1,128,2,176,129,128,11,48,65,127,118,112,1,128,12,17,1,219,250,0,0,1,12,17,1,219,250,0,0,1,10,12,17, 
  1,219,250,0,0,1,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,112,1,128,1,176,1,128,2,176,129,128,11,184,65,127,118,48,1,128,4,17,1, 
  170,252,0,0,1,4,17,1,226,251,0,0,1,8,4,15,1,33,251,0,0,17,1,98,75,0,0,1,2,21,1,51,0,0,0,140,153,2,0,4,0,0,0,2,0,0,0,118,88,1,128,1,16,1,128,2,16,129,127,115, 
  24,1,128,8,4,17,1,170,251,0,0,1,4,17,1,96,251,0,0,1,19,81,0,0,0,239,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0, 
  249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,1,44,0,0,0,12,154,2,0,3,0,0,0, 
  1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1,19,81,0,0,0,244,0,0,0,3,0,1,21,1,57,0,0,0,129,154,2,0,4,0,0,0,2,0,0,0, 
  118,136,1,128,1,16,1,128,2,16,129,127,11,24,1,128,8,4,15,1,114,252,0,0,17,1,98,75,0,0,1,4,17,1,39,252,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246, 
  0,0,0,3,0,1,21,1,44,0,0,0,12,154,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0,0,3, 
  0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,75,253,0,0,1,4,15,1,1,253,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0, 
  0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,1, 
  51,0,0,0,149,151,2,0,4,0,0,0,2,0,0,0,128,88,1,128,1,16,1,128,2,16,1,128,127,24,1,128,10,12,17,1,214,253,0,0,1,12,17,1,214,253,0,0,1,12,19,95,0,0,0,19,1,0,0,1, 
  0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,24,1,128,1,16,1,128,2,16,1,128,127,88,1,128,8,4,17,1,11,255,0,0,1,4,17,1,10,254,0,0,1,2,21,1,130,0,0,0, 
  188,134,2,0,6,0,0,0,2,0,0,0,114,176,66,129,1,168,194,128,2,168,130,127,51,64,2,128,125,32,3,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17, 
  1,204,254,0,0,1,4,19,95,0,0,0,18,1,0,0,3,0,1,8,4,15,1,141,254,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,204, 
  254,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,56,67,129,1,64,194,128,2,64,130,127,51,168,3,128,125,80,1,128,126,72,2,128,4,19,93,0,0,0,11, 
  1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,205,255,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,205,255,0,0,1,4,15,1,142,255,0, 
  0,17,1,140,7,0,0,1,4,19,95,0,0,0,15,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,128,66,130,1,120,198,129,2,120,134,127,94,224, 
  8,130,44,16,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0, 
  15,1,137,1,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0, 
  0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15, 
  1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0, 
  0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7, 
  0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,137,1,1,0,17,1,150,253,0,0,1,15,1,137,1,1,0,17,1,143,250,0,0, 
  1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,137,1,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,137,1,1,0,1,19,47,0,0,0,152,0,0,0,1, 
  0,17,1,137,1,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,24,73,130,1,176,197,129,2,176,133,127,94,24,8,130,44,16,2,128,53,184,69,129,6,176,69,127,7,176,5,128,105, 
  128,2,128,114,232,6,128,125,224,9,128,126,176,3,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0, 
  17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1, 
  4,19,35,0,0,0,113,0,0,0,1,0,15,1,146,3,1,0,17,1,152,249,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128, 
  11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,146,3,1,0,17,1,150,253,0,0,1,15,1,146,3,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153, 
  0,0,0,1,0,17,1,146,3,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,146,3,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,146,3,1,0,1,2,21,1,124,1,0,0,243,90,2,0, 
  12,0,0,0,3,0,0,0,58,128,66,130,1,120,198,129,2,120,134,127,94,224,8,130,44,16,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,15,1,42, 
  15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,155,5,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0, 
  15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17, 
  1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159, 
  116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,155, 
  5,1,0,17,1,150,253,0,0,1,15,1,155,5,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,155,5,1,0,1,1,19,97,0,0,0,21, 
  1,0,0,1,0,17,1,155,5,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,155,5,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,24,73,130,1,176,197,129,2,176,133, 
  127,94,24,8,130,44,16,2,128,53,184,69,129,6,176,69,127,7,176,5,128,105,128,2,128,114,232,6,128,125,224,9,128,126,176,3,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15, 
  1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,164,7,1,0,17,1,152,249,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19, 
  78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255, 
  255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,164,7,1,0,17,1,150,253,0,0,1,15,1,164,7,1,0,17,1,143, 
  250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,164,7,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,164,7,1,0,1,19,47,0,0,0,152,0, 
  0,0,1,0,17,1,164,7,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,184,69,130,1,176,197,129,2,176,133,127,94,224,8,130,44,16,2,128,53,128,70,129,6,176,69,127,7,176, 
  5,128,105,128,2,128,114,176,7,128,125,224,9,128,126,176,3,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116, 
  7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0, 
  0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,173,9,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0, 
  1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63, 
  160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,173,9,1,0,17,1,150,253,0,0,1,15,1,173,9,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0, 
  0,0,153,0,0,0,1,0,17,1,173,9,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,173,9,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,173,9,1,0,1,2,21,1,179,0,0,0,1, 
  155,2,0,16,0,0,0,4,0,0,0,128,80,3,128,1,80,196,129,2,80,4,130,3,88,133,130,4,152,4,128,53,16,3,128,6,80,4,128,7,80,4,128,97,208,3,128,105,24,5,128,114,216,4,128,11,208,2,128,44, 
  16,4,128,115,144,2,128,94,88,4,128,127,144,3,128,12,17,1,33,21,1,0,1,12,17,1,24,19,1,0,1,12,17,1,42,23,1,0,1,12,17,1,15,17,1,0,1,12,17,1,6,15,1,0,1,12,17,1,253,12, 
  1,0,1,12,17,1,39,6,1,0,1,10,12,17,1,21,2,1,0,1,12,17,1,27,47,0,0,1,12,17,1,12,0,1,0,1,12,17,1,27,248,0,0,1,12,17,1,27,47,0,0,1,12,17,1,244,10,1,0,1, 
  21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,128,66,130,1,120,198,129,2,120,134,127,94,224,8,130,44,16,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224, 
  9,128,126,120,4,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,113,12,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0, 
  0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17, 
  1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1, 
  0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95, 
  216,1,128,97,112,1,128,15,1,113,12,1,0,17,1,150,253,0,0,1,15,1,113,12,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,113,12, 
  1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,113,12,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,113,12,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58, 
  128,66,130,1,120,198,129,2,120,134,127,94,224,8,130,44,16,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,15,1,42,15,0,0,17,1,101,119,0,0, 
  1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,122,14,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88, 
  6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0, 
  0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93, 
  0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0, 
  1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,122,14,1,0,17,1,150,253,0,0, 
  1,15,1,122,14,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,122,14,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,122,14, 
  1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,122,14,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,16,66,130,1,120,198,129,2,120,134,127,94,224,8,130,44,216,2,128, 
  53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,131,16,1,0,17,1,152,249,0,0,1,4,15,1,42,15,0,0, 
  17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0, 
  0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17, 
  1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1, 
  0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,131,16,1,0,17,1,150,253,0,0,1,15,1,131,16,1,0,17,1,143,250,0,0,1,19,61,0,0,0, 
  185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,131,16,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,131,16,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,131,16,1, 
  0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,24,73,130,1,176,197,129,2,176,133,127,94,24,8,130,44,16,2,128,53,184,69,129,6,176,69,127,7,176,5,128,105,128,2,128,114,232,6, 
  128,125,224,9,128,126,176,3,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0, 
  1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35,0,0,0, 
  113,0,0,0,1,0,15,1,140,18,1,0,17,1,152,249,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1, 
  0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64, 
  130,128,95,216,1,128,97,112,1,128,15,1,140,18,1,0,17,1,150,253,0,0,1,15,1,140,18,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17, 
  1,140,18,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,140,18,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,140,18,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0, 
  0,0,58,128,66,130,1,120,198,129,2,120,134,127,94,224,8,130,44,16,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,15,1,42,15,0,0,17,1,101, 
  119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,149,20,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0, 
  17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1, 
  4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82, 
  114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,149,20,1,0,17,1,150, 
  253,0,0,1,15,1,149,20,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,149,20,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17, 
  1,149,20,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,149,20,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,128,66,130,1,120,198,129,2,120,134,127,94,224,8,130,44, 
  16,2,128,53,128,70,129,6,120,70,127,7,120,6,128,105,72,3,128,114,176,7,128,125,224,9,128,126,120,4,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,158, 
  22,1,0,17,1,152,249,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0, 
  19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8, 
  0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0, 
  0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0, 
  2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67,127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,158,22,1,0,17,1,150,253,0,0,1,15,1,158,22,1,0,17,1,143,250,0,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,158,22,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,158,22,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1, 
  158,22,1,0,1,2,21,1,124,1,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,24,73,130,1,176,197,129,2,176,133,127,94,24,8,130,44,16,2,128,53,184,69,129,6,176,69,127,7,176,5,128,105,128,2,128, 
  114,232,6,128,125,224,9,128,126,176,3,128,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88, 
  6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0, 
  0,17,1,82,114,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,35, 
  0,0,0,113,0,0,0,1,0,15,1,167,24,1,0,17,1,152,249,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,61,48,195,128,63,160,194,128,11,200,67, 
  127,81,64,130,128,95,216,1,128,97,112,1,128,15,1,167,24,1,0,17,1,150,253,0,0,1,15,1,167,24,1,0,17,1,143,250,0,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0, 
  1,0,17,1,167,24,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,167,24,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,167,24,1,0,1,2,21,1,63,0,0,0,162,148,2,0,5,0,0, 
  0,2,0,0,0,115,48,1,128,1,176,1,128,2,176,129,128,11,184,65,127,118,112,1,128,12,17,1,127,25,1,0,1,12,17,1,127,25,1,0,1,10,12,17,1,127,25,1,0,1,12,19,81,0,0,0,251,0,0,0, 
  1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,184,65,127,118,120,1,128,4,17,1,114,27,1,0,1,8,4,17,1,134,26,1,0,1,4,15, 
  1,197,25,1,0,17,1,98,75,0,0,1,2,21,1,51,0,0,0,140,153,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,144,1,128,2,144,129,127,115,80,1,128,4,17,1,60,26,1,0,1,4,17,1,4,26, 
  1,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,1,44,0,0,0,12,154,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1, 
  19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,39,27,1,0,1, 
  4,15,1,221,26,1,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0, 
  238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,1,57,0,0,0,129,154,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,80,1,128,2,80,129,127,11,88,1,128,4,17,1,239, 
  27,1,0,1,8,4,15,1,183,27,1,0,17,1,98,75,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,12,154,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0, 
  128,4,19,81,0,0,0,247,0,0,0,4,0,1,8,19,81,0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75, 
  0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,51,0,0,0,149,151,2,0,4,0,0,0,2,0,0,0, 
  128,16,1,128,1,144,1,128,2,144,1,128,127,80,1,128,12,17,1,122,28,1,0,1,12,17,1,122,28,1,0,1,10,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0, 
  2,0,0,0,128,80,1,128,1,144,1,128,2,144,1,128,127,16,1,128,4,17,1,175,29,1,0,1,4,17,1,174,28,1,0,1,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,184,65,129, 
  1,24,195,128,2,24,131,127,51,80,1,128,125,40,2,128,126,32,3,128,4,19,95,0,0,0,15,1,0,0,3,0,1,4,15,1,112,29,1,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19, 
  78,0,0,0,225,0,0,0,1,0,17,1,49,29,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,49,29,1,0,1,2,21,7,35,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0, 
  0,188,134,2,0,6,0,0,0,2,0,0,0,114,176,66,129,1,64,194,128,2,64,130,127,51,72,2,128,125,32,3,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0, 
  17,1,113,30,1,0,1,8,4,19,95,0,0,0,18,1,0,0,3,0,1,4,15,1,50,30,1,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1, 
  113,30,1,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,2,19,61,0,0,0,184,0,0,0,1,0,1,19,76,0,0,0,223,0,0,0,2,0,1,21,0,55,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47,80,129,128,11,88, 
  193,127,63,240,0,128,19,47,0,0,0,153,0,0,0,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,1,2,21,1,39,0,0,0,188,106,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240, 
  0,128,4,17,1,51,31,1,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0, 
  113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,83,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,76,16,1,128,47,168,65,128,63,24,1,128,11,8,130,127,1,19,47,0,0,0,153, 
  0,0,0,1,0,17,1,107,31,1,0,1,19,76,0,0,0,223,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,107,31,1,0,1,2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0, 
  0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,239,31,1,0,1,8,2,21,1,53,0,0,0,84,156,2,0,5,0,0,0,2,0,0,0,104,56,1,128,1,48,1,128,2,48,129, 
  128,7,48,1,128,6,48,1,128,8,4,15,1,37,32,1,0,17,1,70,33,1,0,1,2,19,56,0,0,0,170,0,0,0,3,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,57,176,0,128,15, 
  1,49,32,1,0,17,1,85,32,1,0,1,2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,6,17,1,140,32,1,0,1,10, 
  12,17,1,27,47,0,0,1,21,1,59,0,0,0,167,156,2,0,6,0,0,0,2,0,0,0,104,152,1,128,1,80,1,128,2,80,129,128,7,80,129,128,6,80,1,128,103,88,1,128,10,12,17,1,200,32,1,0,1,12, 
  17,1,27,47,0,0,1,2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,248,32,1,0,1,2,21,1,53,0, 
  0,0,37,157,2,0,5,0,0,0,2,0,0,0,6,160,1,128,1,160,1,128,2,160,129,127,7,160,65,128,103,48,1,128,4,15,1,46,33,1,0,17,1,92,3,0,0,1,8,2,19,57,0,0,0,173,0,0,0,3, 
  0,1,19,57,0,0,0,172,0,0,0,1,0,1,21,1,88,0,0,0,125,90,2,0,6,0,0,0,2,0,0,0,6,80,65,129,1,80,193,128,2,80,129,127,7,80,1,128,53,88,1,128,58,248,1,128,8,4,15,1, 
  94,34,1,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,10,34,1,0,17,1,159,33,1,0,1,2,21,1,39,0,0,0,188,106,2,0,3,0,0,0,1,0,0, 
  0,2,240,0,128,1,240,64,128,97,248,0,128,8,4,17,1,210,33,1,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128, 
  58,240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,8,2,21,0,83,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,168,129,128,77,16,1,128,63,24,1,128, 
  11,8,66,127,1,19,47,0,0,0,153,0,0,0,1,0,17,1,10,34,1,0,1,19,77,0,0,0,224,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,10,34,1,0,1,2,19,77,0,0,0,224, 
  0,0,0,2,0,1,19,12,0,0,0,42,0,0,0,1,0,1,19,38,0,0,0,133,0,0,0,1,0,1,19,56,0,0,0,171,0,0,0,1,0,1,21,1,63,0,0,0,188,89,2,0,5,0,0,0,2,0,0,0, 
  6,240,1,128,1,240,1,128,2,240,129,127,7,240,65,128,87,48,1,128,4,19,49,0,0,0,155,0,0,0,1,0,19,36,0,0,0,122,0,0,0,2,0,1,8,2,21,1,58,0,0,0,188,89,2,0,5,0,0,0, 
  2,0,0,0,6,200,1,128,1,200,1,128,2,200,129,127,7,200,65,128,87,48,1,128,4,19,49,0,0,0,155,0,0,0,1,0,17,1,9,35,1,0,1,8,2,21,1,129,0,0,0,120,157,2,0,8,0,0,0,3, 
  0,0,0,58,64,3,128,1,152,130,128,2,152,130,127,65,40,2,128,87,144,1,128,53,160,2,128,6,152,2,128,7,152,66,127,4,19,49,0,0,0,155,0,0,0,1,0,17,1,222,50,1,0,1,4,15,1,35,50,1, 
  0,17,1,130,44,1,0,1,8,4,15,1,246,35,1,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,35,50,1,0,17,1,139,35,1,0,1,2,21,1,39,0,0, 
  0,154,160,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,97,248,0,128,8,4,17,1,190,35,1,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0, 
  1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,1,47,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0, 
  6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,17,1,49,36,1,0,1,19,36,0,0,0,121,0,0,0,3,0,1,21,1,4,3,0,0,232,112,2,0,26,0,0,0,4,0,0,0,32, 
  216,136,133,1,152,207,131,2,152,207,131,35,56,7,128,36,160,15,128,37,176,21,128,6,152,15,131,7,152,15,131,40,128,22,128,41,8,8,128,42,112,16,128,11,120,74,130,44,168,9,128,29,232,10,128,46,224,17,128,31, 
  104,134,129,33,248,13,128,34,16,20,130,38,80,87,129,39,184,75,129,43,136,12,128,47,224,20,128,48,152,5,128,54,200,14,128,87,208,3,128,98,176,18,128,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0, 
  94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,36,0,0,0,119,0,0,0,5,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,50,68,0,0,17, 
  1,177,52,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15, 
  1,118,44,1,0,15,1,172,74,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,118,44,1,0,15,1,172, 
  74,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,56,44,1,0,17,1,54,39,1,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,118, 
  44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,166,54,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,36,0,0, 
  0,119,0,0,0,5,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1, 
  8,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1, 
  0,19,36,0,0,0,119,0,0,0,5,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,144,63,0,0,15,1, 
  156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,118,44,1,0,15,1,172,74, 
  0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0, 
  17,1,155,73,0,0,1,4,15,1,118,44,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,2,21,1,124,3,0,0,102,117,2,0,25,0,0,0,4,0,0,0,32,224,25,133,1,120,208,131, 
  2,120,208,131,35,112,23,128,36,248,20,128,37,248,17,128,38,168,203,131,39,112,24,132,40,24,4,128,9,176,67,130,42,128,16,128,43,248,21,128,44,168,10,128,29,24,7,128,46,168,12,128,31,248,18,129,33,224,26,128, 
  34,24,198,129,41,248,19,129,47,120,15,128,48,168,9,128,54,24,5,128,57,112,25,128,87,168,13,128,98,24,8,128,4,19,53,0,0,0,164,0,0,0,2,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1, 
  125,50,0,0,15,1,184,74,0,0,17,1,155,73,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,249,43,1,0,15,1,104,47, 
  0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,249,43,1,0, 
  15,1,104,47,0,0,15,1,125,50,0,0,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50, 
  0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0, 
  15,1,125,50,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,19,49,0,0,0,155,0,0, 
  0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,165,0,0,0,1,0,15,1,249,43,1,0,17,1,104,47,0,0,1,4,15,1,249,43,1,0,15,1,104,47, 
  0,0,15,1,125,50,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,8,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,165,0,0,0,1,0,15,1,249,43, 
  1,0,17,1,104,47,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0, 
  15,1,184,74,0,0,17,1,229,62,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1, 
  125,50,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,165,0,0,0,1,0,15,1,249,43,1,0,17,1,104, 
  47,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0, 
  0,17,1,166,54,0,0,1,4,15,1,179,42,1,0,17,1,2,40,0,0,1,4,15,1,249,43,1,0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,249,43,1, 
  0,15,1,104,47,0,0,15,1,125,50,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,2,21,7,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,44,56,1,128,125,208,0,128,4,19,9,0,0,0,32, 
  0,0,0,3,0,1,4,17,1,254,42,1,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,187,0,0,0,104,118,2,0,6,0,0,0,2,0,0,0, 
  48,80,1,128,1,80,3,128,2,80,131,128,47,216,4,128,46,80,66,128,58,88,3,128,4,15,1,186,43,1,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1,177,52,0,0,1,4,15,1,186, 
  43,1,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1,55,62,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,160,0,0,0,1,0,15,1,186,43,1,0,15,1, 
  216,45,0,0,15,1,29,47,0,0,17,1,88,41,0,0,1,4,15,1,186,43,1,0,15,1,216,45,0,0,15,1,29,47,0,0,15,1,76,41,0,0,17,1,219,67,0,0,1,2,21,7,35,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,125,176,0,128,4,19,9,0,0,0,31,0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,53,0,0,0,163,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,61,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,36,80,1,128,9,88,65,128,53,240,0,128,19,36,0,0,0,119,0,0,0,5,0,1,1,19,53,0,0,0,161,0,0,0,1,0,17,1,56,44,1,0,1,2,19,36, 
  0,0,0,119,0,0,0,5,0,1,21,1,47,0,0,0,225,160,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,59,48,1,128,4,17,1,178,44,1,0,1,8,2,21,1, 
  111,0,0,0,82,161,2,0,7,0,0,0,2,0,0,0,6,56,66,129,1,56,194,128,2,56,130,127,7,56,194,128,53,64,2,128,58,112,1,128,87,224,2,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,104,49, 
  1,0,17,1,139,35,1,0,1,8,4,15,1,222,45,1,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,17,1,34,45,1,0,1,2,21,1,75,0,0,0,217,161,2,0, 
  6,0,0,0,2,0,0,0,6,16,2,128,1,16,194,128,2,16,130,127,7,16,130,128,61,24,2,128,87,80,1,128,4,19,49,0,0,0,155,0,0,0,1,0,19,36,0,0,0,116,0,0,0,6,0,1,8,4,17,1, 
  110,45,1,0,1,2,21,1,47,0,0,0,153,162,2,0,5,0,0,0,2,0,0,0,60,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,158,45,1,0,1,8,2,21,1,63,0,0,0, 
  188,89,2,0,5,0,0,0,2,0,0,0,6,240,1,128,1,240,1,128,2,240,129,127,7,240,65,128,87,48,1,128,4,19,49,0,0,0,155,0,0,0,1,0,19,36,0,0,0,114,0,0,0,8,0,1,8,2,21,1, 
  47,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,17,1,25,46,1,0,1,19,36,0,0,0,120,0,0,0,5,0,1,21,1,4, 
  3,0,0,232,112,2,0,26,0,0,0,4,0,0,0,32,224,137,133,1,0,209,131,2,0,209,131,35,64,21,128,36,64,8,128,37,192,14,128,6,0,17,131,7,0,17,131,40,176,10,128,41,160,19,128,42,144,15,128,11, 
  16,86,130,44,128,11,128,29,32,13,128,46,112,20,128,31,80,140,129,33,128,22,128,34,64,5,130,38,240,77,129,39,16,73,129,43,208,3,128,47,208,18,128,48,80,23,128,54,16,6,128,87,8,17,128,98,224,6,128,4, 
  19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,36,0,0,0,118,0,0,0,7,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1, 
  184,74,0,0,17,1,247,51,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,144,63,0,0,15,1,156,63, 
  0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0, 
  15,1,184,74,0,0,17,1,166,54,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1, 
  155,73,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,229,62,0,0,1,4,15,1, 
  92,49,1,0,15,1,172,74,0,0,15,1,84,55,0,0,17,1,253,54,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,248,53,0,0,1,4,15,1,92,49,1,0,15,1,172,74, 
  0,0,15,1,184,74,0,0,17,1,199,50,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,36,0,0,0,118,0,0,0,7,0, 
  1,8,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,162,0,0,0,1,0,19,36,0,0,0,118,0,0,0,7,0,1,4, 
  15,1,92,49,1,0,15,1,172,74,0,0,15,1,50,68,0,0,17,1,219,67,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,165,52,0,0,17,1,78,52,0,0,1,4,15,1,92,49,1,0,15,1, 
  172,74,0,0,15,1,50,68,0,0,17,1,55,62,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,30,49,1,0,17,1,54,39,1,0,1,4,15,1, 
  92,49,1,0,15,1,172,74,0,0,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,92,49,1,0,15,1,172,74,0,0,15,1,50,68,0,0,17,1,177,52,0,0,1,2,21,0,61,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,36,80,1,128,9,88,65,128,53,240,0,128,19,36,0,0,0,118,0,0,0,7,0,1,1,19,53,0,0,0,161,0,0,0,1,0,17,1,30,49,1,0,1,2,19,36,0,0,0,118,0,0, 
  0,7,0,1,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,36,216,1,128,47,160,65,128,63,16,1,128,11,224,129,127,19,47,0,0,0,153,0,0,0,1,0,17,1,104,49,1,0,1,17,1,183,49, 
  1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,104,49,1,0,1,2,21,1,47,0,0,0,72,163,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1, 
  128,12,17,1,243,49,1,0,1,10,12,19,36,0,0,0,120,0,0,0,5,0,1,21,1,47,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1, 
  128,8,4,17,1,25,46,1,0,1,2,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,36,216,1,128,47,160,65,128,63,16,1,128,11,224,129,127,19,47,0,0,0,153,0,0,0,1,0,17,1,35,50, 
  1,0,1,17,1,114,50,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,35,50,1,0,1,2,21,1,47,0,0,0,72,163,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127, 
  7,112,1,128,62,48,1,128,12,17,1,174,50,1,0,1,10,12,19,36,0,0,0,121,0,0,0,3,0,1,21,1,47,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127, 
  7,48,1,128,62,56,1,128,8,4,17,1,49,36,1,0,1,2,21,1,75,0,0,0,217,161,2,0,6,0,0,0,2,0,0,0,6,144,1,128,1,144,193,128,2,144,129,127,7,144,129,128,61,80,1,128,87,152,1,128, 
  4,17,1,42,51,1,0,1,8,4,19,49,0,0,0,155,0,0,0,1,0,19,36,0,0,0,117,0,0,0,4,0,1,2,21,1,47,0,0,0,153,162,2,0,5,0,0,0,2,0,0,0,60,56,1,128,1,48,1,128, 
  2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,90,51,1,0,1,2,21,1,63,0,0,0,188,89,2,0,5,0,0,0,2,0,0,0,6,240,1,128,1,240,1,128,2,240,129,127,7,240,65,128,87,48,1,128, 
  4,19,49,0,0,0,155,0,0,0,1,0,19,36,0,0,0,115,0,0,0,6,0,1,8,2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128, 
  13,48,1,128,4,17,1,213,51,1,0,1,8,19,12,0,0,0,41,0,0,0,1,0,1,21,1,242,0,0,0,250,163,2,0,10,0,0,0,3,0,0,0,104,192,6,128,1,160,5,128,2,160,5,128,91,208,65,129,92, 
  24,4,128,93,168,5,128,6,160,5,128,7,160,133,128,99,48,5,128,103,232,2,128,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,40,0,0,0,3,0,1,4,15, 
  1,53,90,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19, 
  12,0,0,0,40,0,0,0,3,0,1,4,15,1,181,89,1,0,17,1,200,52,1,0,1,8,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,40,0,0,0,3,0, 
  1,4,15,1,53,90,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,2,21,1,47,0,0,0,168,164,2,0,5,0,0,0,2,0,0,0,56,56,1,128,1,48,1,128,2,48,129,128,7,48, 
  1,128,6,48,1,128,8,4,17,1,248,52,1,0,1,2,21,1,82,0,0,0,250,164,2,0,7,0,0,0,2,0,0,0,6,176,1,129,1,176,1,129,2,176,129,127,7,176,1,128,58,184,129,128,101,80,2,128,102,112, 
  1,128,4,17,1,103,89,1,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,17,1,185,88,1,0,1,4,17,1,75,53,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1, 
  128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,173,88,1,0,17,1,157,86,1,0,1,8,4,15,1,173,88,1,0,15,1,145,86,1,0,17,1,153,53,1,0,1,2,21,1,88, 
  0,0,0,125,90,2,0,6,0,0,0,2,0,0,0,6,184,66,129,1,184,194,128,2,184,130,127,7,184,2,128,53,80,1,128,58,240,1,128,4,15,1,172,54,1,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4, 
  19,35,0,0,0,113,0,0,0,1,0,15,1,93,54,1,0,17,1,242,53,1,0,1,8,2,21,1,39,0,0,0,144,172,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,37, 
  54,1,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0, 
  19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,168,65,128,63,24,65,128,91,16,1,128,11,224,65,127,1,19,47,0,0,0,153,0,0,0,1,0,17, 
  1,93,54,1,0,1,17,1,172,54,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,93,54,1,0,1,2,21,1,47,0,0,0,253,135,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112, 
  129,127,7,112,1,128,85,48,1,128,4,17,1,220,54,1,0,1,8,2,21,1,47,0,0,0,226,172,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4, 
  17,1,12,55,1,0,1,2,21,1,117,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,248,13,128,1,240,13,128,2,240,141,128,67,208,199,131,66,192,15,128,70,48,3,128,6,240,205,127,7,240,205,129,72,208, 
  8,131,73,248,14,128,74,248,3,128,75,40,77,129,76,128,81,129,93,152,10,128,71,8,71,129,63,152,9,128,91,224,11,128,92,192,5,128,99,16,17,128,103,72,18,128,104,192,4,128,4,19,37,0,0,0,129,0,0,0, 
  1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,4,15,1,92,86,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15, 
  1,130,34,1,0,17,1,70,33,1,0,1,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,92,86,1,0,1,4,19,37,0,0,0,125, 
  0,0,0,1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,4,15,1,92,86,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,28,3,0,0,1,4,19,37,0,0,0,130,0,0,0, 
  1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,4,15,1,92,86,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,206,34,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19, 
  38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,92,86,1,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0, 
  1,0,17,1,92,86,1,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,8,4,15,1,92,86,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0, 
  17,1,142,34,1,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,92,86,1, 
  0,15,1,154,51,1,0,17,1,65,90,1,0,1,4,15,1,203,85,1,0,17,1,130,57,1,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,92,86,1,0,17,1,161,94,1,0,1,4,15,1,92,86,1,0, 
  15,1,106,34,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,2,21,1,47,0,0,0,168,164,2,0,5,0,0,0,2,0,0,0,56,56,1,128, 
  1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,178,57,1,0,1,2,21,1,82,0,0,0,250,164,2,0,7,0,0,0,2,0,0,0,6,136,2,129,1,136,2,129,2,136,130,127,7,136,2,128, 
  58,112,129,128,101,72,2,128,102,8,2,128,4,19,35,0,0,0,113,0,0,0,1,0,17,1,29,85,1,0,1,4,17,1,83,58,1,0,1,4,17,1,5,58,1,0,1,8,2,21,1,77,0,0,0,51,172,2,0,6, 
  0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,11,88,1,128,53,200,1,128,8,4,15,1,173,88,1,0,17,1,157,86,1,0,1,4,15,1,173,88,1,0,15,1,145,86,1,0,17, 
  1,153,53,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,240,1,128,1,240,1,129,2,240,129,127,7,240,65,128,11,248,1,128,53,80,1,128,4,15,1,5,85,1,0,15,1,17,85, 
  1,0,17,1,220,78,1,0,1,8,4,15,1,5,85,1,0,17,1,161,58,1,0,1,2,21,1,155,0,0,0,52,173,2,0,8,0,0,0,3,0,0,0,121,56,3,128,1,48,195,127,2,48,131,128,11,144,1,128,122, 
  96,2,128,53,8,4,128,6,48,3,128,7,48,3,128,4,15,1,128,77,1,0,15,1,181,77,1,0,15,1,208,78,1,0,17,1,68,74,1,0,1,4,15,1,128,77,1,0,15,1,181,77,1,0,15,1,56,74,1,0, 
  17,1,152,73,1,0,1,8,4,15,1,128,77,1,0,15,1,181,77,1,0,15,1,208,78,1,0,17,1,73,59,1,0,1,4,15,1,128,77,1,0,15,1,181,77,1,0,15,1,61,59,1,0,17,1,220,78,1,0,1, 
  2,19,74,0,0,0,215,0,0,0,1,0,1,21,1,47,0,0,0,226,172,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,121,59,1,0,1, 
  2,21,1,117,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,160,10,128,1,160,11,128,2,160,139,128,67,72,208,131,66,48,3,128,70,56,6,128,6,160,203,127,7,160,203,129,72,112,13,131,73,168,12,128,74, 
  16,9,128,75,0,71,129,76,216,73,129,93,200,7,128,71,56,78,129,63,168,11,128,91,240,4,128,92,0,15,128,99,128,4,128,103,72,18,128,104,72,17,128,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0, 
  45,0,0,0,1,0,15,1,99,73,1,0,15,1,154,51,1,0,17,1,65,90,1,0,1,4,15,1,210,72,1,0,17,1,239,61,1,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0, 
  1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,99,73,1,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,99,73,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,99, 
  73,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,99,73,1,0,1,4,19,37,0,0,0,131,0,0, 
  0,1,0,15,1,99,73,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,99,73,1,0,17,1,161,94,1,0,1,4,15,1,99,73,1,0,15,1,154,51,1,0,15,1,65,90,1,0, 
  15,1,50,92,1,0,17,1,142,34,1,0,1,8,4,15,1,99,73,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,206,34,1,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1, 
  99,73,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,99,73,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,99,73,1,0,17,1,161,94,1,0, 
  1,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,99,73,1,0,1,4,15,1,99,73,1,0,15,1,154,51,1,0,15,1,65,90,1, 
  0,15,1,50,92,1,0,17,1,28,3,0,0,1,4,15,1,99,73,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,4,15,1,99,73,1,0,15,1,106,34,1,0,15, 
  1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,2,21,1,47,0,0,0,168,164,2,0,5,0,0,0,2,0,0,0,56,48,1,128,1,112,1,128,2,112,129, 
  128,7,112,1,128,6,112,1,128,4,17,1,31,62,1,0,1,8,2,21,1,82,0,0,0,250,164,2,0,7,0,0,0,2,0,0,0,6,136,2,129,1,136,2,129,2,136,130,127,7,136,2,128,58,112,129,128,101,72,2, 
  128,102,8,2,128,4,19,35,0,0,0,113,0,0,0,1,0,17,1,14,63,1,0,1,4,17,1,192,62,1,0,1,4,17,1,114,62,1,0,1,8,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0, 
  6,96,2,128,1,96,2,129,2,96,130,127,7,96,66,128,11,80,1,128,53,192,1,128,4,15,1,173,88,1,0,17,1,157,86,1,0,1,4,15,1,173,88,1,0,15,1,145,86,1,0,17,1,153,53,1,0,1,8,2, 
  21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,11,248,1,128,53,88,1,128,8,4,15,1,5,85,1,0,15,1,17,85,1,0,17,1,220,78, 
  1,0,1,4,15,1,5,85,1,0,17,1,161,58,1,0,1,2,21,1,95,0,0,0,89,175,2,0,7,0,0,0,2,0,0,0,56,136,2,128,1,224,65,129,2,224,129,128,7,224,129,128,6,224,1,128,11,112,1,128, 
  53,232,1,128,4,15,1,198,72,1,0,17,1,182,70,1,0,1,8,4,15,1,198,72,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,4,15,1,174,63,1,0,17,1,110,63,1,0,1,2,21,1,63,0,0,0, 
  241,68,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,67,0,0,0,196,0,0,0,2,0,1,2,21,1, 
  77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1,128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,252,63,1,0,17,1,182,70,1,0,1,8,4,15,1,252,63, 
  1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,2,19,66,0,0,0,194,0,0,0,5,0,1,21,1,86,0,0,0,188,89,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48, 
  65,128,87,56,1,128,8,4,19,49,0,0,0,155,0,0,0,1,0,19,87,0,0,0,3,1,0,0,1,0,19,88,0,0,0,4,1,0,0,1,0,15,1,65,65,1,0,17,1,95,64,1,0,1,2,21,0,35,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,88,176,0,128,15,1,95,64,1,0,17,1,131,64,1,0,1,2,21,1,59,0,0,0,135,109,2,0,6,0,0,0,2,0,0,0,84,152,1,128,1,80,1,129,2,80,129, 
  128,7,80,1,128,6,80,1,128,85,88,1,128,10,12,17,1,27,47,0,0,1,12,17,1,198,64,1,0,1,12,17,1,27,47,0,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,56,1,128, 
  1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,246,64,1,0,1,2,21,1,74,0,0,0,188,89,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128, 
  87,56,1,128,8,4,19,49,0,0,0,155,0,0,0,1,0,19,87,0,0,0,3,1,0,0,1,0,19,88,0,0,0,5,1,0,0,3,0,1,2,21,1,47,0,0,0,253,135,2,0,5,0,0,0,2,0,0,0,6, 
  112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,85,48,1,128,4,17,1,113,65,1,0,1,8,2,21,1,47,0,0,0,226,172,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7, 
  48,65,128,11,56,1,128,8,4,17,1,161,65,1,0,1,2,21,1,117,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,104,9,128,1,176,12,128,2,176,140,128,67,160,199,131,66,184,14,128,70,24,18,128,6, 
  176,204,127,7,176,204,129,72,184,12,131,73,128,13,128,74,144,5,128,75,224,82,129,76,8,80,129,93,88,6,128,71,160,72,129,63,144,4,128,91,208,16,128,92,104,10,128,99,72,14,128,103,48,3,128,104,176,11,128,4, 
  15,1,117,70,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,4,15,1,117,70,1,0,15,1,154,51,1,0,15,1,65,90, 
  1,0,15,1,50,92,1,0,17,1,206,34,1,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0, 
  0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,117,70,1,0,1,4,15,1,117,70,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,28,3,0,0,1,4,19,37,0,0,0,125, 
  0,0,0,1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,4,15,1,117,70,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,142,34,1,0,1,4,19,58,0,0,0,175,0,0,0, 
  1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,117,70,1,0,1,4,15,1,117,70,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33, 
  1,0,1,8,4,19,37,0,0,0,130,0,0,0,1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,4,15,1,228,69,1, 
  0,17,1,23,68,1,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,117,70,1,0,15,1,154,51,1,0,17,1,65,90,1,0,1,4,19,37,0,0,0,126,0,0,0, 
  1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,117,70,1,0,1,4,19,37,0, 
  0,0,129,0,0,0,1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,117,70,1,0,17,1,161,94,1,0,1,2,21,1,47,0,0,0,168,164,2,0,5,0,0, 
  0,2,0,0,0,56,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,71,68,1,0,1,2,21,1,82,0,0,0,250,164,2,0,7,0,0,0,2,0,0,0,6,176,1,129,1,176,1, 
  129,2,176,129,127,7,176,1,128,58,184,129,128,101,80,2,128,102,112,1,128,4,17,1,150,69,1,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,17,1,232,68,1,0,1,4,17,1,154,68,1,0,1,2,21,1, 
  77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1,128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,173,88,1,0,17,1,157,86,1,0,1,8,4,15,1,173,88, 
  1,0,15,1,145,86,1,0,17,1,153,53,1,0,1,2,21,1,95,0,0,0,89,175,2,0,7,0,0,0,2,0,0,0,56,16,2,128,1,128,66,129,2,128,130,128,7,128,130,128,6,128,2,128,11,136,2,128,53,112, 
  1,128,4,15,1,198,72,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,4,15,1,72,69,1,0,17,1,110,63,1,0,1,8,4,15,1,198,72,1,0,17,1,182,70,1,0,1,2,21,1,77,0,0,0,51,172, 
  2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,11,88,1,128,53,200,1,128,8,4,15,1,252,63,1,0,17,1,182,70,1,0,1,4,15,1,252,63,1,0,15,1,170,70, 
  1,0,17,1,8,64,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,240,1,128,1,240,1,129,2,240,129,127,7,240,65,128,11,248,1,128,53,80,1,128,4,15,1,5,85,1,0,15, 
  1,17,85,1,0,17,1,220,78,1,0,1,8,4,15,1,5,85,1,0,17,1,161,58,1,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4,128,73,120,1,128,38,184,131,128,55,40, 
  3,128,66,152,66,128,70,8,66,128,86,112,1,128,1,19,55,0,0,0,169,0,0,0,1,0,17,1,228,69,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,228,69,1,0,1,19,55,0,0,0,167,0,0,0, 
  1,0,17,1,228,69,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,228,69,1,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,228,69,1,0,1,17,1,117,70,1,0,1,2,21,1,52,0,0,0,193, 
  175,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,86,0,0,0,2,1,0,0,6,0,1,2,19,69,0,0,0,203,0,0,0,1,0,1,21,1, 
  155,0,0,0,52,173,2,0,8,0,0,0,3,0,0,0,121,96,2,128,1,48,195,127,2,48,131,128,11,8,4,128,122,144,1,128,53,56,3,128,6,48,3,128,7,48,3,128,4,15,1,106,71,1,0,15,1,159,71,1, 
  0,15,1,186,72,1,0,17,1,152,73,1,0,1,4,15,1,106,71,1,0,15,1,159,71,1,0,15,1,94,71,1,0,17,1,73,59,1,0,1,8,4,15,1,106,71,1,0,15,1,159,71,1,0,15,1,82,71,1,0, 
  17,1,8,64,1,0,1,4,15,1,106,71,1,0,15,1,159,71,1,0,15,1,94,71,1,0,17,1,68,74,1,0,1,2,19,68,0,0,0,197,0,0,0,1,0,1,19,68,0,0,0,198,0,0,0,1,0,1,21,1, 
  52,0,0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,69,0,0,0,204,0,0,0,3,0,1,2,21,0,35,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,68,176,0,128,15,1,159,71,1,0,17,1,195,71,1,0,1,2,21,1,95,0,0,0,19,176,2,0,9,0,0,0,3,0,0,0,82,176,1,130,1,176,194,128,2,176,130,127,11,240,1, 
  128,121,48,2,128,53,184,2,128,6,176,2,128,7,176,2,128,122,112,2,128,12,17,1,27,47,0,0,1,12,17,1,42,72,1,0,1,12,17,1,42,72,1,0,1,12,17,1,42,72,1,0,1,10,12,17,1,42,72,1, 
  0,1,12,17,1,27,47,0,0,1,21,1,107,0,0,0,52,173,2,0,8,0,0,0,3,0,0,0,121,232,2,128,1,0,194,127,2,0,130,128,11,120,2,128,122,144,1,128,53,8,2,128,6,0,2,128,7,0,2,128, 
  4,15,1,174,72,1,0,17,1,152,73,1,0,1,8,4,15,1,162,72,1,0,17,1,8,64,1,0,1,4,15,1,150,72,1,0,17,1,68,74,1,0,1,4,15,1,150,72,1,0,17,1,73,59,1,0,1,2,19,68, 
  0,0,0,201,0,0,0,2,0,1,19,68,0,0,0,200,0,0,0,2,0,1,19,68,0,0,0,202,0,0,0,2,0,1,19,68,0,0,0,199,0,0,0,1,0,1,19,66,0,0,0,195,0,0,0,4,0,1,21,0, 
  144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4,128,73,120,65,129,38,184,131,128,55,40,3,128,66,152,66,128,70,8,2,128,89,112,1,128,1,19,55,0,0,0,169,0,0,0,1,0,17,1,210,72, 
  1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,210,72,1,0,1,19,55,0,0,0,167,0,0,0,1,0,17,1,210,72,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,210,72,1,0,1,19,12,0, 
  0,0,42,0,0,0,1,0,17,1,210,72,1,0,1,17,1,99,73,1,0,1,2,21,1,52,0,0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1, 
  128,8,4,19,89,0,0,0,6,1,0,0,4,0,1,2,21,1,47,0,0,0,226,172,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,11,48,1,128,4,17,1,200,73,1, 
  0,1,8,2,21,1,58,0,0,0,147,176,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,125,56,1,128,8,4,19,93,0,0,0,11,1,0,0,1,0,17,1,3,74,1, 
  0,1,2,21,1,52,0,0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,90,0,0,0,8,1,0,0,4,0,1,8,2,19,74,0,0, 
  0,216,0,0,0,1,0,1,21,1,117,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,104,10,128,1,104,12,128,2,104,140,128,67,168,210,131,66,144,16,128,70,224,17,128,6,104,204,127,7,104,204,129,72,184, 
  13,131,73,200,15,128,74,160,6,128,75,144,68,129,76,160,73,129,93,128,14,128,71,104,72,129,63,104,11,128,91,88,5,128,92,112,12,128,99,48,9,128,103,48,3,128,104,104,7,128,4,15,1,75,77,1,0,15,1,106, 
  34,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,75,77,1,0,17,1,161,94,1,0,1,4, 
  19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,75,77,1,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,75,77,1,0,17,1, 
  161,94,1,0,1,4,15,1,75,77,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,75,77,1,0,17,1,161,94,1, 
  0,1,4,15,1,186,76,1,0,17,1,239,61,1,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,75,77,1,0,17,1,161,94,1,0,1,4,15,1,75,77,1,0,15,1,154,51,1,0,15,1,65,90,1,0, 
  15,1,50,92,1,0,17,1,142,34,1,0,1,4,15,1,75,77,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,206,34,1,0,1,8,4,19,58,0,0,0,175,0,0,0,1,0,19,38, 
  0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,75,77,1,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,75,77,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,176,0, 
  0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,75,77,1,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,75,77,1,0,17,1,161,94,1,0,1,4,19, 
  36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,75,77,1,0,15,1,154,51,1,0,17,1,65,90,1,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,75,77,1,0,17,1, 
  161,94,1,0,1,4,15,1,75,77,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,28,3,0,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4,128, 
  73,120,65,129,38,184,131,128,55,40,3,128,66,152,66,128,70,8,2,128,89,112,1,128,1,19,55,0,0,0,169,0,0,0,1,0,17,1,186,76,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,186,76,1,0, 
  1,19,55,0,0,0,167,0,0,0,1,0,17,1,186,76,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,186,76,1,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,186,76,1,0,1,17,1,75,77,1, 
  0,1,2,21,1,52,0,0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,89,0,0,0,7,1,0,0,3,0,1,2,21,1,52,0, 
  0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,75,0,0,0,222,0,0,0,3,0,1,2,21,0,35,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,74,176,0,128,15,1,181,77,1,0,17,1,217,77,1,0,1,2,21,1,95,0,0,0,19,176,2,0,9,0,0,0,3,0,0,0,82,176,1,130,1,176,194,128,2,176,130,127,11,240,1,128,121, 
  112,2,128,53,184,2,128,6,176,2,128,7,176,2,128,122,48,2,128,12,17,1,27,47,0,0,1,12,17,1,64,78,1,0,1,12,17,1,64,78,1,0,1,12,17,1,64,78,1,0,1,10,12,17,1,64,78,1,0,1, 
  12,17,1,27,47,0,0,1,21,1,107,0,0,0,52,173,2,0,8,0,0,0,3,0,0,0,121,232,2,128,1,0,194,127,2,0,130,128,11,8,2,128,122,120,2,128,53,144,1,128,6,0,2,128,7,0,2,128,4,15, 
  1,196,78,1,0,17,1,220,78,1,0,1,8,4,15,1,184,78,1,0,17,1,68,74,1,0,1,4,15,1,172,78,1,0,17,1,152,73,1,0,1,4,15,1,184,78,1,0,17,1,73,59,1,0,1,2,19,74,0,0, 
  0,219,0,0,0,2,0,1,19,74,0,0,0,220,0,0,0,2,0,1,19,74,0,0,0,218,0,0,0,2,0,1,19,74,0,0,0,217,0,0,0,1,0,1,21,1,191,0,0,0,230,176,2,0,9,0,0,0,3,0, 
  0,0,94,176,1,130,1,176,132,128,2,176,132,128,105,88,5,128,114,184,4,128,125,104,3,128,6,176,132,126,7,176,4,128,126,32,2,128,4,15,1,156,79,1,0,17,1,204,80,0,0,1,4,19,94,0,0,0,12,1, 
  0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,17,1,156,79,1,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,17,1,156,79,1,0,1,8,4,15,1,156,79,1,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,156,79,1,0,15,1,116,7,0,0,17,1,88,6,0,0,1,2,21,1,47,0, 
  0,0,253,135,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56,1,128,8,4,17,1,204,79,1,0,1,2,21,1,47,0,0,0,226,172,2,0,5,0,0,0,2,0, 
  0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,11,48,1,128,4,17,1,252,79,1,0,1,8,2,21,1,117,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,168,18,128,1,200,14,128,2,200, 
  142,128,67,248,198,131,66,176,12,128,70,232,11,128,6,200,206,127,7,200,206,129,72,32,11,131,73,248,8,128,74,24,16,128,75,48,67,129,76,224,81,129,93,248,3,128,71,0,78,129,63,224,16,128,91,64,5,128,92,208, 
  14,128,99,136,6,128,103,192,9,128,104,248,7,128,4,19,37,0,0,0,127,0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0, 
  1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,208,84,1,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,208,84,1,0, 
  1,4,15,1,63,84,1,0,17,1,114,82,1,0,1,4,15,1,208,84,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,28,3,0,0,1,4,15,1,208,84,1,0,15,1,106,34,1,0, 
  15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,4,15,1,208,84,1,0,15,1,106,34,1,0,15,1,118, 
  34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,129, 
  0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,208,84,1,0,15,1,154,51,1,0,17,1,65,90,1,0,1, 
  4,19,37,0,0,0,125,0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,8,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0, 
  17,1,208,84,1,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,4,15,1,208,84,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,206, 
  34,1,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,208,84,1,0,17,1,161,94,1,0,1,4,15,1,208,84,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,142,34,1,0, 
  1,2,21,1,47,0,0,0,168,164,2,0,5,0,0,0,2,0,0,0,56,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,162,82,1,0,1,8,2,21,1,82,0,0,0,250,164,2,0, 
  7,0,0,0,2,0,0,0,6,8,2,129,1,8,2,129,2,8,130,127,7,8,2,128,58,112,129,128,101,80,2,128,102,16,2,128,4,19,35,0,0,0,113,0,0,0,1,0,17,1,145,83,1,0,1,8,4,17,1,67, 
  83,1,0,1,4,17,1,245,82,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1,128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,173,88, 
  1,0,17,1,157,86,1,0,1,8,4,15,1,173,88,1,0,15,1,145,86,1,0,17,1,153,53,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129, 
  127,7,80,65,128,11,248,1,128,53,88,1,128,8,4,15,1,5,85,1,0,15,1,17,85,1,0,17,1,220,78,1,0,1,4,15,1,5,85,1,0,17,1,161,58,1,0,1,2,21,1,95,0,0,0,89,175,2,0,7, 
  0,0,0,2,0,0,0,56,136,2,128,1,16,66,129,2,16,130,128,7,16,130,128,6,16,2,128,11,24,2,128,53,112,1,128,4,15,1,198,72,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,8,4,15,1,198, 
  72,1,0,17,1,182,70,1,0,1,4,15,1,241,83,1,0,17,1,110,63,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,240,1,128,1,240,1,129,2,240,129,127,7,240,65,128,11, 
  248,1,128,53,80,1,128,4,15,1,252,63,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,8,4,15,1,252,63,1,0,17,1,182,70,1,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,12,72,132,129,73,120,1,128,38,184,131,128,55,40,3,128,66,152,66,128,70,8,2,128,92,112,1,128,1,19,55,0,0,0,169,0,0,0,1,0,17,1,63,84,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17, 
  1,63,84,1,0,1,19,55,0,0,0,167,0,0,0,1,0,17,1,63,84,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,63,84,1,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,63,84,1,0,1, 
  17,1,208,84,1,0,1,2,21,1,52,0,0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,92,0,0,0,10,1,0,0,6,0,1, 
  2,19,73,0,0,0,214,0,0,0,4,0,1,19,75,0,0,0,221,0,0,0,1,0,1,21,1,95,0,0,0,89,175,2,0,7,0,0,0,2,0,0,0,56,136,2,128,1,128,66,129,2,128,130,128,7,128,130,128,6, 
  128,2,128,11,16,2,128,53,112,1,128,4,15,1,198,72,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,4,15,1,198,72,1,0,17,1,182,70,1,0,1,8,4,15,1,125,85,1,0,17,1,110,63,1,0,1, 
  2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1,128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,252,63,1,0,17,1,182,70,1,0,1,8,4,15, 
  1,252,63,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4,128,73,120,1,128,38,184,131,128,55,40,195,128,66,152,66,128,70,8,2, 
  128,91,112,1,128,1,19,55,0,0,0,169,0,0,0,1,0,17,1,203,85,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,203,85,1,0,1,19,55,0,0,0,167,0,0,0,1,0,17,1,203,85,1,0,1, 
  19,38,0,0,0,132,0,0,0,1,0,17,1,203,85,1,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,203,85,1,0,1,17,1,92,86,1,0,1,2,21,1,52,0,0,0,193,175,2,0,5,0,0,0,2,0, 
  0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,91,0,0,0,9,1,0,0,6,0,1,8,2,19,72,0,0,0,212,0,0,0,1,0,1,21,1,155,0,0,0,52,173,2,0,8, 
  0,0,0,3,0,0,0,121,104,2,128,1,96,194,127,2,96,130,128,11,144,1,128,122,56,3,128,53,8,4,128,6,96,2,128,7,96,2,128,4,15,1,81,87,1,0,15,1,134,87,1,0,15,1,161,88,1,0,17,1, 
  68,74,1,0,1,8,4,15,1,81,87,1,0,15,1,134,87,1,0,15,1,161,88,1,0,17,1,73,59,1,0,1,4,15,1,81,87,1,0,15,1,134,87,1,0,15,1,69,87,1,0,17,1,152,73,1,0,1,4,15, 
  1,81,87,1,0,15,1,134,87,1,0,15,1,57,87,1,0,17,1,153,53,1,0,1,2,19,71,0,0,0,206,0,0,0,1,0,1,19,71,0,0,0,207,0,0,0,1,0,1,21,1,52,0,0,0,193,175,2,0,5, 
  0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,72,0,0,0,213,0,0,0,3,0,1,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  71,176,0,128,15,1,134,87,1,0,17,1,170,87,1,0,1,2,21,1,95,0,0,0,19,176,2,0,9,0,0,0,3,0,0,0,82,176,1,130,1,176,194,128,2,176,130,127,11,240,1,128,121,184,2,128,53,48,2,128, 
  6,176,2,128,7,176,2,128,122,112,2,128,12,17,1,27,47,0,0,1,12,17,1,17,88,1,0,1,12,17,1,17,88,1,0,1,12,17,1,17,88,1,0,1,10,12,17,1,17,88,1,0,1,12,17,1,27,47,0,0, 
  1,21,1,107,0,0,0,52,173,2,0,8,0,0,0,3,0,0,0,121,120,2,128,1,112,194,127,2,112,130,128,11,0,2,128,122,232,2,128,53,144,1,128,6,112,2,128,7,112,2,128,4,15,1,149,88,1,0,17,1, 
  153,53,1,0,1,4,15,1,137,88,1,0,17,1,68,74,1,0,1,8,4,15,1,137,88,1,0,17,1,73,59,1,0,1,4,15,1,125,88,1,0,17,1,152,73,1,0,1,2,19,71,0,0,0,210,0,0,0,2,0, 
  1,19,71,0,0,0,211,0,0,0,2,0,1,19,71,0,0,0,209,0,0,0,2,0,1,19,71,0,0,0,208,0,0,0,1,0,1,19,70,0,0,0,205,0,0,0,4,0,1,21,1,95,0,0,0,89,175,2,0,7, 
  0,0,0,2,0,0,0,56,136,2,128,1,128,66,129,2,128,130,128,7,128,130,128,6,128,2,128,11,112,1,128,53,224,1,128,4,15,1,198,72,1,0,17,1,182,70,1,0,1,4,15,1,198,72,1,0,15,1,170,70, 
  1,0,17,1,8,64,1,0,1,8,4,15,1,25,89,1,0,17,1,110,63,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,240,1,128,1,240,1,129,2,240,129,127,7,240,65,128,11, 
  248,1,128,53,80,1,128,4,15,1,252,63,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,8,4,15,1,252,63,1,0,17,1,182,70,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0, 
  0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,11,88,1,128,53,200,1,128,8,4,15,1,5,85,1,0,17,1,161,58,1,0,1,4,15,1,5,85,1,0,15,1,17,85,1,0,17,1,220,78,1,0,1, 
  2,21,0,127,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,240,3,128,73,80,1,128,38,144,131,128,55,0,3,128,66,112,66,128,70,224,1,128,19,55,0,0,0,169,0,0,0,1,0,17,1,181,89,1,0, 
  1,19,55,0,0,0,168,0,0,0,1,0,17,1,181,89,1,0,1,19,55,0,0,0,167,0,0,0,1,0,17,1,181,89,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,181,89,1,0,1,19,12,0,0,0, 
  40,0,0,0,3,0,1,1,2,19,12,0,0,0,40,0,0,0,3,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,14,176,0,128,15,1,65,90,1,0,17,1,101,90,1,0,1,2,21,1,119, 
  0,0,0,93,177,2,0,11,0,0,0,3,0,0,0,24,48,3,128,1,112,195,129,2,112,195,129,21,240,1,128,23,48,2,128,13,120,131,127,6,112,3,128,7,112,67,127,25,176,2,128,58,240,66,128,82,112,2,128,12, 
  17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,10,6,17,1,228,90,1,0,1,12,17,1,27, 
  47,0,0,1,21,1,155,0,0,0,15,178,2,0,14,0,0,0,3,0,0,0,64,88,68,131,1,16,4,128,2,16,196,129,67,144,195,129,92,24,4,128,93,80,3,128,6,16,4,128,7,16,68,128,63,144,2,129,66,152, 
  4,128,91,16,67,128,99,208,2,128,103,208,3,128,104,80,2,128,12,17,1,27,47,0,0,1,12,17,1,128,91,1,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17, 
  1,128,91,1,0,1,12,17,1,27,47,0,0,1,10,12,17,1,27,47,0,0,1,12,17,1,128,91,1,0,1,12,17,1,128,91,1,0,1,2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,48, 
  1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,176,91,1,0,1,2,21,1,117,0,0,0,7,179,2,0,8,0,0,0,3,0,0,0,64,0,2,128,1,112,2,128,2,112,194,128,67,56, 
  3,128,63,144,1,128,66,120,2,128,6,112,2,128,7,112,66,127,4,15,1,38,92,1,0,17,1,206,34,1,0,1,4,15,1,38,92,1,0,17,1,142,34,1,0,1,8,4,19,36,0,0,0,124,0,0,0,1,0,19, 
  14,0,0,0,46,0,0,0,3,0,1,4,15,1,38,92,1,0,17,1,28,3,0,0,1,2,19,14,0,0,0,46,0,0,0,3,0,1,19,14,0,0,0,45,0,0,0,1,0,1,21,1,47,0,0,0,168,164,2,0, 
  5,0,0,0,2,0,0,0,56,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,110,92,1,0,1,8,2,21,1,82,0,0,0,250,164,2,0,7,0,0,0,2,0,0,0,6,112,1,129, 
  1,112,1,129,2,112,129,127,7,112,1,128,58,248,129,128,101,184,1,128,102,120,1,128,8,4,17,1,189,93,1,0,1,4,17,1,111,93,1,0,1,4,19,35,0,0,0,113,0,0,0,1,0,17,1,193,92,1,0,1, 
  2,21,1,95,0,0,0,89,175,2,0,7,0,0,0,2,0,0,0,56,112,1,128,1,80,66,129,2,80,130,128,7,80,130,128,6,80,2,128,11,224,1,128,53,88,2,128,4,15,1,33,93,1,0,17,1,110,63,1,0, 
  1,4,15,1,198,72,1,0,17,1,182,70,1,0,1,8,4,15,1,198,72,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1,128,1, 
  192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,252,63,1,0,17,1,182,70,1,0,1,8,4,15,1,252,63,1,0,15,1,170,70,1,0,17,1,8,64,1,0,1,2,21,1,77,0,0, 
  0,51,172,2,0,6,0,0,0,2,0,0,0,6,192,1,128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,173,88,1,0,17,1,157,86,1,0,1,8,4,15,1,173,88,1,0,15, 
  1,145,86,1,0,17,1,153,53,1,0,1,2,21,1,77,0,0,0,51,172,2,0,6,0,0,0,2,0,0,0,6,80,1,128,1,80,1,129,2,80,129,127,7,80,65,128,11,88,1,128,53,200,1,128,8,4,15,1,5, 
  85,1,0,17,1,161,58,1,0,1,4,15,1,5,85,1,0,15,1,17,85,1,0,17,1,220,78,1,0,1,2,21,0,149,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,0,160,4,129,73,112,1,128,38,176,195, 
  128,55,32,3,128,12,64,4,128,66,144,66,128,70,0,2,128,19,55,0,0,0,169,0,0,0,1,0,17,1,11,94,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,11,94,1,0,1,19,55,0,0,0,167,0, 
  0,0,1,0,17,1,11,94,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,11,94,1,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,11,94,1,0,1,19,0,0,0,0,0,0,0,0,3,0,1,1, 
  2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,220,94,1,0,1,19,12,0,0,0,39,0,0,0,1,0,1, 
  21,1,116,1,0,0,15,178,2,0,14,0,0,0,3,0,0,0,64,104,67,131,1,80,7,128,2,80,199,129,67,80,197,129,92,24,10,128,93,56,4,128,6,80,7,128,7,80,71,128,63,88,7,129,66,40,8,128,91,80, 
  66,128,99,48,11,128,103,32,6,128,104,72,9,128,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,37,0,0,0,3,0,1,4,15,1,221,96,1,0,15,1,151,98, 
  1,0,15,1,136,100,1,0,17,1,142,34,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,37,0,0,0,3,0,1,4,15,1,221,96,1,0,15,1,151, 
  98,1,0,15,1,136,100,1,0,17,1,28,3,0,0,1,4,15,1,209,96,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,8,4,15,1,221,96, 
  1,0,15,1,151,98,1,0,15,1,136,100,1,0,17,1,206,34,1,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,13,0,0,0,43,0,0,0,1,0,15,1,221,96,1,0,17,1,151,98,1,0,1,4,15,1, 
  209,96,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,37,0,0,0,3,0,1,4,15, 
  1,81,96,1,0,17,1,200,52,1,0,1,2,21,0,127,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,240,3,128,73,80,1,128,38,144,131,128,55,0,3,128,66,112,66,128,70,224,1,128,19,55,0,0,0, 
  169,0,0,0,1,0,17,1,81,96,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,81,96,1,0,1,19,55,0,0,0,167,0,0,0,1,0,17,1,81,96,1,0,1,19,38,0,0,0,132,0,0,0,1,0, 
  17,1,81,96,1,0,1,19,12,0,0,0,37,0,0,0,3,0,1,1,2,19,12,0,0,0,37,0,0,0,3,0,1,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2, 
  48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,24,97,1,0,1,19,12,0,0,0,38,0,0,0,3,0,1,21,1,242,0,0,0,250,163,2,0,10,0,0,0,3,0,0,0,104,232,2,128,1,184,3,128,2,184, 
  3,128,91,96,69,129,92,208,1,128,93,120,6,128,6,184,3,128,7,184,131,128,99,240,4,128,103,192,3,128,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,36,0, 
  0,0,5,0,1,4,15,1,139,98,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,8,4,15,1,139,98,1,0,15,1,118,34,1,0,15,1,191,31,1,0,15,1,49,32,1,0,15,1,58, 
  33,1,0,17,1,92,3,0,0,1,4,15,1,11,98,1,0,17,1,200,52,1,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,36,0,0,0,5,0,1,4, 
  19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,36,0,0,0,5,0,1,2,21,0,127,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,240,3,128,73,80,1, 
  128,38,144,131,128,55,0,3,128,66,112,66,128,70,224,1,128,19,55,0,0,0,169,0,0,0,1,0,17,1,11,98,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,11,98,1,0,1,19,55,0,0,0,167,0, 
  0,0,1,0,17,1,11,98,1,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,11,98,1,0,1,19,12,0,0,0,36,0,0,0,5,0,1,1,2,19,12,0,0,0,36,0,0,0,5,0,1,21,0,35,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,13,176,0,128,15,1,151,98,1,0,17,1,187,98,1,0,1,2,21,1,119,0,0,0,93,177,2,0,11,0,0,0,3,0,0,0,24,56,3,128,1,176,194,129,2,176,194, 
  129,21,112,2,128,23,120,3,128,13,184,130,127,6,176,2,128,7,176,66,127,25,248,2,128,58,240,65,128,82,48,2,128,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,10,6,17, 
  1,58,99,1,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,1,155,0,0,0,15,178,2,0,14,0,0,0,3,0,0,0,64,88,68,131, 
  1,208,3,128,2,208,195,129,67,216,195,129,92,144,2,128,93,80,3,128,6,208,3,128,7,208,67,128,63,144,3,129,66,16,3,128,91,208,66,128,99,24,4,128,103,152,4,128,104,80,2,128,12,17,1,27,47,0,0,1, 
  12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17,1,214,99,1,0,1,12,17,1,27,47,0,0,1,12,17,1,214,99,1,0,1,10,12,17,1,214,99,1,0,1,12,17,1,27,47,0,0,1,12,17,1, 
  214,99,1,0,1,12,17,1,27,47,0,0,1,2,21,1,47,0,0,0,165,155,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,6,100,1,0,1, 
  8,2,21,1,117,0,0,0,7,179,2,0,8,0,0,0,3,0,0,0,64,56,3,128,1,48,3,128,2,48,195,128,67,0,2,128,63,144,1,128,66,112,2,128,6,48,3,128,7,48,67,127,4,15,1,124,100,1,0,17, 
  1,206,34,1,0,1,4,15,1,124,100,1,0,17,1,28,3,0,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,13,0,0,0,44,0,0,0,3,0,1,8,4,15,1,124,100,1,0,17,1,142,34,1,0,1,2, 
  19,13,0,0,0,44,0,0,0,3,0,1,19,13,0,0,0,43,0,0,0,1,0,1,19,0,0,0,0,0,0,0,0,3,0,1,21,1,132,2,0,0,29,82,2,0,21,0,0,0,4,0,0,0,64,192,17,128,1,152, 
  13,128,2,152,141,128,67,160,205,131,66,72,7,128,70,152,8,128,6,152,205,127,7,152,205,129,72,248,16,131,73,208,10,128,74,104,15,128,75,160,78,129,76,48,67,129,93,248,3,128,71,48,80,129,63,152,11,128,91,96, 
  9,128,92,216,5,128,99,104,5,128,103,192,18,128,104,152,12,128,4,19,37,0,0,0,126,0,0,0,1,0,15,1,187,103,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0, 
  134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,15,1,37,103,1,0,17,1,62,92,1,0,1,4,19,58,0,0,0,175,0,0,0,1,0,19,38,0,0,0, 
  134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,187,103,1,0,15,1, 
  154,51,1,0,17,1,65,90,1,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,187,103,1,0,17,1,161,94,1,0,1,4,19,58,0,0,0,174,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19, 
  12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,187,103,1,0,17,1,161,94,1,0,1,4,15,1,187,103,1,0,15,1,154,51,1,0, 
  15,1,65,90,1,0,15,1,50,92,1,0,17,1,206,34,1,0,1,4,15,1,187,103,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,130,34,1,0,17,1,70,33,1,0,1,8,4,15,1,187,103,1,0,15, 
  1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,28,3,0,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,187,103,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,131,0,0,0,1, 
  0,15,1,187,103,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,187,103,1,0,17,1,161,94,1,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,187,103,1,0,17,1,161, 
  94,1,0,1,4,15,1,187,103,1,0,15,1,154,51,1,0,15,1,65,90,1,0,15,1,50,92,1,0,17,1,142,34,1,0,1,4,15,1,187,103,1,0,15,1,106,34,1,0,15,1,118,34,1,0,15,1,191,31,1, 
  0,15,1,49,32,1,0,15,1,58,33,1,0,17,1,92,3,0,0,1,2,21,0,149,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,0,160,4,129,73,112,1,128,38,176,195,128,55,32,3,128,12,64,4,128,66, 
  144,66,128,70,0,2,128,19,55,0,0,0,169,0,0,0,1,0,17,1,37,103,1,0,1,19,55,0,0,0,168,0,0,0,1,0,17,1,37,103,1,0,1,19,55,0,0,0,167,0,0,0,1,0,17,1,37,103,1,0, 
  1,19,38,0,0,0,132,0,0,0,1,0,17,1,37,103,1,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,37,103,1,0,1,19,0,0,0,0,1,0,0,0,3,0,1,1,2,19,0,0,0,0,1,0,0,0, 
  3,0,1,13,15,1,199,103,1,0,17,1,213,103,1,0,1,21,7,54,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,76,107,1,0,15,1,168,105,1,0,15,1,144,105,1,0,15,1, 
  156,105,1,0,17,1,29,105,1,0,1,21,9,17,1,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,168,2,128,5,152,5,128,2,48,129,128,3,32,4,128,6,16,7,128,4,19,15,0,0,0,49,0,0,0,1, 
  0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,76,107,1,0,17,1,168,105,1,0,1,4,19,15,0,0,0,47,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19, 
  3,0,0,0,5,0,0,0,1,0,15,1,76,107,1,0,17,1,168,105,1,0,1,4,19,15,0,0,0,48,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,76, 
  107,1,0,17,1,168,105,1,0,1,4,19,15,0,0,0,50,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,76,107,1,0,17,1,168,105,1,0,1,4,19,15, 
  0,0,0,51,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,76,107,1,0,17,1,168,105,1,0,1,2,21,9,103,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,152,1,128,5,104,2,128,2,48,129,128,3,0,2,128,6,208,2,128,4,19,16,0,0,0,54,0,0,0,2,0,1,4,19,16,0,0,0,52,0,0,0,2,0,1,4,19,16,0,0,0,53,0,0,0,2, 
  0,1,4,19,16,0,0,0,55,0,0,0,2,0,1,4,19,16,0,0,0,56,0,0,0,2,0,1,19,16,0,0,0,57,0,0,0,1,0,1,19,3,0,0,0,5,0,0,0,1,0,1,19,2,0,0,0,4,0,0, 
  0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,15,1,168,105,1,0,17,1,204,105,1,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176, 
  0,128,12,17,1,64,106,1,0,1,21,9,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,112,1,128,5,240,1,128,2,48,129,128,3,176,1,128,6,48,2,128,12,17,1,64,106,1,0,1,12,17,1,64, 
  106,1,0,1,12,17,1,64,106,1,0,1,12,17,1,64,106,1,0,1,12,17,1,64,106,1,0,1,12,17,1,27,47,0,0,1,21,7,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15, 
  1,64,107,1,0,15,1,156,105,1,0,17,1,29,105,1,0,1,21,9,213,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,72,2,128,5,120,4,128,2,48,129,128,3,96,3,128,6,144,5,128,4,19,15,0, 
  0,0,49,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,15,0,0,0,47,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6, 
  0,0,0,2,0,1,4,19,15,0,0,0,48,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,15,0,0,0,50,0,0,0,1,0,19,2,0,0,0,3,0, 
  0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,15,0,0,0,51,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,2,19,3,0,0,0,6,0,0, 
  0,2,0,1,19,1,0,0,0,2,0,0,0,1,0,1,15,1,199,103,1,0,17,1,101,107,1,0,1,21,1,95,1,0,0,102,179,2,0,13,0,0,0,3,0,0,0,24,40,202,130,1,0,197,129,2,0,197,129,83, 
  48,2,128,23,88,135,129,21,8,5,128,6,0,5,128,7,0,69,127,25,48,4,129,58,216,5,128,79,40,8,128,80,48,3,128,81,40,9,128,4,15,1,247,63,2,0,15,1,211,64,2,0,15,1,30,66,2,0,15,1, 
  42,66,2,0,17,1,19,61,2,0,1,4,15,1,247,63,2,0,15,1,211,64,2,0,15,1,30,66,2,0,15,1,7,61,2,0,17,1,12,59,2,0,1,4,15,1,153,57,2,0,15,1,165,57,2,0,15,1,0,59, 
  2,0,17,1,212,6,2,0,1,8,4,15,1,153,57,2,0,15,1,165,57,2,0,15,1,200,6,2,0,17,1,101,212,1,0,1,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,15, 
  1,153,57,2,0,15,1,165,57,2,0,15,1,89,212,1,0,17,1,91,0,0,0,1,4,15,1,153,57,2,0,15,1,165,57,2,0,15,1,77,212,1,0,17,1,234,161,1,0,1,4,15,1,247,63,2,0,15,1,211, 
  64,2,0,15,1,30,66,2,0,15,1,222,161,1,0,17,1,128,159,1,0,1,4,15,1,247,63,2,0,15,1,211,64,2,0,15,1,30,66,2,0,15,1,116,159,1,0,17,1,52,159,1,0,1,4,15,1,153,57,2, 
  0,15,1,165,57,2,0,15,1,40,159,1,0,17,1,197,108,1,0,1,2,21,1,69,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,32,2,129,1,32,2,128,2,32,130,127,7,32,2,128,58,48,1,128,4, 
  19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,11,109,1,0,1,8,2,21,1,47,0,0,0,141,187,2,0,5,0,0,0,2,0,0,0,20,56,1,128,1,48,1,128,2,48,129, 
  128,7,48,1,128,6,48,1,128,8,4,17,1,59,109,1,0,1,2,21,1,162,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,8,80,130,1,0,208,129,2,0,144,127,94,96,5,130,44,80,19,128,53,176,71, 
  129,6,0,80,127,7,0,16,128,105,208,16,128,114,128,13,128,125,48,10,128,126,16,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,4,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246, 
  0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0, 
  0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0, 
  15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246, 
  0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0, 
  0,0,1,0,15,1,73,112,1,0,17,1,222,111,1,0,1,4,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0, 
  15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,28,159,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245, 
  0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,2,21,1,39,0,0,0,105,145,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128, 
  97,240,0,128,4,17,1,17,112,1,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0, 
  0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,60,208,4,128,81,224,130,130,61,104,4,128,11,192,6,128,20,184,6,127,45, 
  88,70,127,46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16,2,128,15,1,73,112,1,0,17,1,166,156,1,0,1,15,1,73,112,1,0,17,1,159,153,1,0,1,19,61,0,0,0,183,0, 
  0,0,1,0,17,1,73,112,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,73,112,1,0,1,15,1,73,112,1,0,17,1,45,153,1,0,1,15,1,73,112,1,0,17,1,145,152,1,0,1,19,46,0,0,0, 
  149,0,0,0,1,0,17,1,73,112,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,73,112,1,0,1,15,1,73,112,1,0,17,1,52,113,1,0,1,19,20,0,0,0,64,0,0,0,4,0,1,1,19,47,0, 
  0,0,152,0,0,0,1,0,17,1,73,112,1,0,1,2,21,1,47,0,0,0,223,187,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,112,113, 
  1,0,1,12,19,45,0,0,0,148,0,0,0,1,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,160,113, 
  1,0,1,2,21,1,36,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,88,80,130,1,160,204,129,2,160,140,127,94,168,12,130,44,192,6,128,53,104,78,129,6,160,76,127,7,160,12,128,105,176,10,128,114,208, 
  4,128,125,16,2,128,126,240,7,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0, 
  0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146, 
  246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15, 
  1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0, 
  0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15, 
  1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,15,1,221,12,0,0,15,1,6,246, 
  0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1, 
  71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15, 
  1,197,115,1,0,17,1,222,111,1,0,1,2,21,0,195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61,40,4,128,46,128,133,126,47, 
  240,196,126,95,56,2,128,97,208,1,128,15,1,197,115,1,0,17,1,166,156,1,0,1,15,1,197,115,1,0,17,1,159,153,1,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,197,115,1,0,1,19,47,0,0,0, 
  153,0,0,0,1,0,17,1,197,115,1,0,1,15,1,197,115,1,0,17,1,45,153,1,0,1,15,1,197,115,1,0,17,1,137,116,1,0,1,19,46,0,0,0,150,0,0,0,3,0,1,19,97,0,0,0,21,1,0,0, 
  1,0,17,1,197,115,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,197,115,1,0,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,248,3,130,1,112,3,130,2,112,131,127,94, 
  48,67,130,44,184,3,128,53,56,132,129,6,112,67,127,7,112,3,128,98,176,130,128,105,48,2,128,114,120,3,128,125,240,2,128,126,112,2,128,12,17,1,104,150,1,0,1,12,17,1,63,148,1,0,1,12,17,1,22,146, 
  1,0,1,12,17,1,237,143,1,0,1,12,17,1,196,141,1,0,1,10,12,17,1,155,139,1,0,1,12,17,1,114,137,1,0,1,6,17,1,78,119,1,0,1,12,17,1,37,117,1,0,1,12,19,62,0,0,0,188,0, 
  0,0,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,192,5,130,1,136,6,130,2,136,134,127,94,144,72,130,44,80,12,128,53,48,130,129,6,136,70,127,7,136,6,128,98,144,139,128,105, 
  96,3,128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8, 
  0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,190,118,1,0,17,1,222,111,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59, 
  0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15, 
  0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0, 
  255,255,255,255,8,0,0,0,3,0,0,0,63,240,129,128,81,144,193,128,95,16,4,128,11,128,2,128,97,16,3,128,61,120,3,128,62,120,3,128,47,128,67,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0, 
  0,153,0,0,0,1,0,17,1,190,118,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,190,118,1,0,1,15,1,190,118,1,0,17,1,166,156,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,190, 
  118,1,0,1,15,1,190,118,1,0,17,1,159,153,1,0,1,2,21,1,212,0,0,0,55,147,2,0,19,0,0,0,4,0,0,0,128,152,4,128,1,88,5,131,2,88,133,131,3,240,194,131,4,240,2,129,53,224,5,128, 
  6,88,5,128,7,88,5,128,84,24,5,128,105,88,3,128,94,216,4,128,11,152,3,128,44,160,5,128,97,32,6,128,62,216,3,127,127,96,6,128,98,24,68,128,114,96,5,128,115,88,4,128,12,19,62,0,0,0,188,0, 
  0,0,1,0,1,12,17,1,104,150,1,0,1,12,17,1,32,133,1,0,1,12,17,1,247,130,1,0,1,12,17,1,22,146,1,0,1,12,17,1,206,128,1,0,1,12,17,1,165,126,1,0,1,12,17,1,196,141,1,0, 
  1,12,17,1,124,124,1,0,1,10,12,17,1,155,139,1,0,1,12,17,1,114,137,1,0,1,12,17,1,37,117,1,0,1,12,17,1,83,122,1,0,1,12,17,1,42,120,1,0,1,12,17,1,73,135,1,0,1,21,1, 
  152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,192,5,130,1,136,6,130,2,136,134,127,94,144,72,130,44,80,12,128,53,48,130,129,6,136,70,127,7,136,6,128,98,144,139,128,105,96,3,128,114,144,4,128, 
  125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0, 
  0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,195,121,1,0,17,1,222,111,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0, 
  1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204, 
  80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0, 
  0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,63,128,131,128,81,144,193,128,95,240,1,128,11,232,2,128,97,16,4,128,61,120,3,128,62,120,3,128,47,88,66,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,195,121,1,0,17,1,159,153,1, 
  0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,195,121,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,195,121,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,195,121,1,0,1,15,1,195, 
  121,1,0,17,1,166,156,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,11,130,1,192,5,130,2,192,133,127,94,200,71,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5, 
  128,98,200,138,128,105,96,3,128,114,144,4,128,125,200,8,128,126,200,5,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1, 
  0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1, 
  0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0, 
  0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,236,123,1,0,17,1,222,111,1,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2, 
  21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,131,128,81,144,193,128,95,16,4,128,11,240,2,128,97,240,1,128,61,232,2,128,62,232,2,128,47,88,66,126,19,61,0,0,0,185,0,0,0,2, 
  0,1,15,1,236,123,1,0,17,1,166,156,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,236,123,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,236,123,1,0,1,19,47,0,0,0,153,0,0, 
  0,1,0,17,1,236,123,1,0,1,15,1,236,123,1,0,17,1,159,153,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,7,130,1,192,5,130,2,192,133,127,94,144,72,130,44,80, 
  12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,200,5,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15, 
  1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,21,126,1,0,17,1,222,111,1,0,1,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15, 
  1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,32,194,128,95,16,3,128,11,120,3,128,97,16,4,128,61,8,4,128,62,8,4,128,47, 
  128,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,21,126,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,21,126,1,0,1,15,1,21,126,1,0,17,1,159, 
  153,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,21,126,1,0,1,1,15,1,21,126,1,0,17,1,166,156,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,5,130,1, 
  192,5,130,2,192,133,127,94,144,72,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6, 
  0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,62,128,1,0, 
  17,1,222,111,1,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1, 
  159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0, 
  225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,32,194,128,95,128,2,128,11,144,1,128, 
  97,232,2,128,61,80,3,128,62,80,3,128,47,88,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,62,128,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,62,128,1,0,17,1,159,153,1,0,1,15, 
  1,62,128,1,0,17,1,166,156,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,62,128,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,62,128,1,0,1,2,21,1,152,1,0,0,112,111,2,0, 
  13,0,0,0,3,0,0,0,58,200,7,130,1,192,5,130,2,192,133,127,94,144,72,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,200,5,128, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0, 
  0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4, 
  19,35,0,0,0,113,0,0,0,1,0,15,1,103,130,1,0,17,1,222,111,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0, 
  0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,130, 
  128,81,144,193,128,95,16,3,128,11,128,3,128,97,16,4,128,61,120,3,128,62,120,3,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,103,130,1,0,1,19, 
  47,0,0,0,153,0,0,0,1,0,17,1,103,130,1,0,1,15,1,103,130,1,0,17,1,159,153,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,103,130,1,0,1,15,1,103,130,1,0,17,1,166,156,1, 
  0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,5,130,1,192,5,130,2,192,133,127,94,144,72,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,144,139,128,105,96,3, 
  128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0, 
  17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,144,132,1,0,17,1,222,111,1,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0, 
  15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255, 
  255,255,8,0,0,0,3,0,0,0,63,128,131,128,81,144,193,128,95,16,4,128,11,232,2,128,97,240,1,128,61,120,3,128,62,120,3,128,47,88,66,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,144,132,1,0, 
  17,1,166,156,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,144,132,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,144,132,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,144,132,1, 
  0,1,15,1,144,132,1,0,17,1,159,153,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,192,5,130,1,136,6,130,2,136,134,127,94,144,72,130,44,80,12,128,53,48,130,129,6,136, 
  70,127,7,136,6,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181, 
  3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,185,134,1,0,17,1,222,111,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78, 
  0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101, 
  119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,131,128,81,144,193,128,95,240,2,128,11,232,3,128,97,128,2,128,61,232,2,128,62,232,2,128,47,240,65,126,19,61,0,0,0, 
  185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,185,134,1,0,1,15,1,185,134,1,0,17,1,166,156,1,0,1,1,15,1,185,134,1,0,17,1,159,153,1,0,1,19,47,0,0,0,153,0,0, 
  0,1,0,17,1,185,134,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,185,134,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,7,130,1,192,5,130,2,192,133,127,94, 
  144,72,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,200,5,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,226,136,1,0,17,1,222,111, 
  1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59, 
  0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0, 
  2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,80,131,128,81,144,193,128,95,232,2,128,11,88,2,128,97,240,1,128,61,224,3,128, 
  62,224,3,128,47,232,67,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,226,136,1,0,17,1,166,156,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,226,136,1,0,1,15,1,226,136,1,0,17,1,159, 
  153,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,226,136,1,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,226,136,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0, 
  58,192,5,130,1,136,6,130,2,136,134,127,94,144,72,130,44,80,12,128,53,48,130,129,6,136,70,127,7,136,6,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0, 
  0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1, 
  11,139,1,0,17,1,222,111,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30, 
  15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0, 
  19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1, 
  0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,129,128,81,144,193,128,95,16,4, 
  128,11,128,3,128,97,16,3,128,61,120,3,128,62,120,3,128,47,128,66,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,11,139,1,0,1,19,97,0,0,0,21,1,0,0, 
  1,0,17,1,11,139,1,0,1,15,1,11,139,1,0,17,1,166,156,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,11,139,1,0,1,15,1,11,139,1,0,17,1,159,153,1,0,1,2,21,1,152,1,0, 
  0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,11,130,1,192,5,130,2,192,133,127,94,200,71,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,200,138,128,105,96,3,128,114,144,4,128,125,200,8, 
  128,126,200,5,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8, 
  4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82, 
  114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19, 
  59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0, 
  0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,52,141,1,0,17,1,222,111,1,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0, 
  0,0,63,88,130,128,81,248,193,128,95,144,1,128,11,120,3,128,97,16,4,128,61,8,4,128,62,8,4,128,47,232,66,126,15,1,52,141,1,0,17,1,159,153,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19, 
  47,0,0,0,153,0,0,0,1,0,17,1,52,141,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,52,141,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,52,141,1,0,1,1,15,1,52,141,1,0, 
  17,1,166,156,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,3,130,1,136,6,130,2,136,134,127,94,144,72,130,44,80,12,128,53,48,130,129,6,136,70,127,7,136,6,128,98,144, 
  139,128,105,40,4,128,114,88,5,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0, 
  0,0,113,0,0,0,1,0,15,1,93,143,1,0,17,1,222,111,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1, 
  0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15, 
  1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143, 
  0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,130,128,81,144,193,128,95,240,1,128,11,240,2,128,97,16,4,128,61,232,2,128,62,232,2,128,47,128,67,126,19,61,0,0,0,185,0,0,0,2,0,1,15, 
  1,93,143,1,0,17,1,159,153,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,93,143,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,93,143,1,0,1,19,97,0,0,0,21,1,0,0,1,0, 
  17,1,93,143,1,0,1,15,1,93,143,1,0,17,1,166,156,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,11,130,1,192,5,130,2,192,133,127,94,200,71,130,44,80,12,128,53, 
  48,130,129,6,192,69,127,7,192,5,128,98,200,138,128,105,96,3,128,114,144,4,128,125,200,8,128,126,200,5,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30, 
  1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0, 
  19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0, 
  0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17, 
  1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,134,145,1,0,17,1,222,111,1,0,1,4,15,1,42,15, 
  0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,130,128,81,144,193,128,95,128,2,128,11,128,3,128,97,16,4,128,61,120,3,128,62,120,3,128,47,240,65,126, 
  19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,134,145,1,0,1,15,1,134,145,1,0,17,1,159,153,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,134,145,1,0, 
  1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,134,145,1,0,1,15,1,134,145,1,0,17,1,166,156,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,5,130,1,192,5,130, 
  2,192,133,127,94,144,72,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,175,147,1,0,17,1,222, 
  111,1,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0, 
  0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0, 
  0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,240,1,128,11,192,2,128,97,88,2, 
  128,61,224,3,128,62,224,3,128,47,80,67,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,175,147,1,0,17,1,159,153,1,0,1,15,1,175,147,1,0,17,1,166,156,1,0,1,19,47,0,0,0,152,0,0,0, 
  1,0,17,1,175,147,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,175,147,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,175,147,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0, 
  0,3,0,0,0,58,192,5,130,1,136,6,130,2,136,134,127,94,144,72,130,44,80,12,128,53,48,130,129,6,136,70,127,7,136,6,128,98,144,139,128,105,96,3,128,114,144,4,128,125,144,9,128,126,144,6,128,4,15,1, 
  30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0, 
  15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,216,149,1,0,17,1,222,111,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11, 
  1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84, 
  0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,130,128,81,144, 
  193,128,95,128,3,128,11,232,3,128,97,128,2,128,61,232,2,128,62,232,2,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,216,149,1,0,1,15,1,216,149, 
  1,0,17,1,166,156,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,216,149,1,0,1,15,1,216,149,1,0,17,1,159,153,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,216,149,1,0,1,2, 
  21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,11,130,1,192,5,130,2,192,133,127,94,200,71,130,44,80,12,128,53,48,130,129,6,192,69,127,7,192,5,128,98,200,138,128,105,96,3,128,114,144, 
  4,128,125,200,8,128,126,200,5,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0, 
  0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140, 
  7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116, 
  0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0, 
  0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0, 
  0,0,187,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,1,152,1,0,17,1,222,111,1,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8, 
  0,0,0,3,0,0,0,63,144,129,128,81,136,194,128,95,32,2,128,11,232,3,128,97,232,2,128,61,224,3,128,62,224,3,128,47,80,67,126,19,47,0,0,0,153,0,0,0,1,0,17,1,1,152,1,0,1,15,1,1, 
  152,1,0,17,1,159,153,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,1,152,1,0,17,1,166,156,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,1,152,1,0,1,1,19,47,0,0,0,152, 
  0,0,0,1,0,17,1,1,152,1,0,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,112,2,130,1,176,3,130,2,176,131,127,94,112,67,130,44,248,3,128,53,56,132,129,6,176,67,127,7, 
  176,3,128,98,240,130,128,105,48,2,128,114,184,3,128,125,48,3,128,126,176,2,128,12,17,1,104,150,1,0,1,6,17,1,78,119,1,0,1,12,17,1,63,148,1,0,1,12,17,1,22,146,1,0,1,12,17,1,237,143, 
  1,0,1,12,17,1,196,141,1,0,1,10,12,17,1,155,139,1,0,1,12,17,1,114,137,1,0,1,12,17,1,37,117,1,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,47,0,0,0,124,189,2,0,5, 
  0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,105,153,1,0,1,10,12,19,60,0,0,0,182,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5, 
  0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,15,1,196,74,0,0,17,1,128,33,0,0,1,2,21,1,63,0,0,0,34,190,2,0,5,0,0,0,2,0,0, 
  0,115,112,1,128,1,240,1,128,2,240,129,128,11,176,65,127,118,48,1,128,12,17,1,235,153,1,0,1,12,17,1,235,153,1,0,1,12,17,1,235,153,1,0,1,10,12,19,81,0,0,0,251,0,0,0,1,0,1,21, 
  1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,232,1,128,1,160,1,128,2,160,129,128,11,48,65,127,118,168,1,128,4,15,1,229,155,1,0,17,1,98,75,0,0,1,8,4,17,1,249,154,1,0,1, 
  4,17,1,49,154,1,0,1,2,21,1,57,0,0,0,186,149,2,0,4,0,0,0,2,0,0,0,118,128,1,128,1,192,1,128,2,192,129,127,11,16,1,128,4,15,1,193,154,1,0,17,1,98,75,0,0,1,4,17,1, 
  118,154,1,0,1,8,19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118, 
  240,0,128,4,19,81,0,0,0,247,0,0,0,4,0,1,8,19,81,0,0,0,237,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,154,155, 
  1,0,1,4,15,1,80,155,1,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,35,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81, 
  0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,1,51,0,0,0,243,150,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,144,1,128,2,144,129,127,115,80,1,128,4, 
  17,1,92,156,1,0,1,4,17,1,36,156,1,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4, 
  19,81,0,0,0,250,0,0,0,4,0,1,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,1,51,0,0,0,47,191,2,0,4,0,0,0,2,0,0,0,128,88,1,128, 
  1,16,1,128,2,16,1,128,127,24,1,128,10,12,17,1,230,156,1,0,1,12,17,1,230,156,1,0,1,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0, 
  128,24,1,128,1,16,1,128,2,16,1,128,127,88,1,128,8,4,17,1,27,158,1,0,1,4,17,1,26,157,1,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,184,65,129,1,40,194,128, 
  2,40,130,127,51,80,1,128,125,32,3,128,126,48,2,128,4,19,95,0,0,0,18,1,0,0,3,0,1,4,15,1,220,157,1,0,17,1,140,7,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0, 
  0,226,0,0,0,1,0,17,1,157,157,1,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,157,157,1,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2, 
  0,6,0,0,0,2,0,0,0,114,160,67,129,1,152,195,128,2,152,131,127,51,48,3,128,125,80,1,128,126,64,2,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,221,158, 
  1,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,221,158,1,0,1,4,19,95,0,0,0,15,1,0,0,3,0,1,8,4,15,1,158,158,1,0,17,1,140,7,0,0, 
  1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,2,19,20,0,0,0,64,0,0,0,4,0,1,19,6,0,0,0,13,0,0,0,1,0,1,21,1,63,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,240,1,129,1,240,1,128,2,240, 
  129,127,7,240,1,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,42,0,0,0,144,0,0,0,2,0,1,8,2,19,17,0,0,0,60,0,0,0,1,0,1,21,1,88,0,0,0,125,90,2,0,6,0, 
  0,0,2,0,0,0,6,240,65,129,1,240,193,128,2,240,129,127,7,240,1,128,53,80,1,128,58,248,1,128,4,15,1,151,161,1,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,68,160,1,0,17,1,217,159,1,0,1,2,21,1,39,0,0,0,61,192,2,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,97,248,0,128,8,4,17,1,12,160,1,0,1,19,11,0,0, 
  0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58,240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3, 
  0,1,8,2,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,39,216,65,128,47,160,65,128,63,16,1,128,11,224,65,127,19,47,0,0,0,153,0,0,0,1,0,17,1,68,160,1,0,1,17,1,147,160, 
  1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,68,160,1,0,1,2,21,1,59,0,0,0,216,192,2,0,6,0,0,0,2,0,0,0,6,144,65,129,1,144,193,128,2,144,129,127,7,144,1,128,77,152,1, 
  128,78,80,1,128,12,17,1,219,160,1,0,1,10,12,17,1,219,160,1,0,1,12,19,39,0,0,0,137,0,0,0,2,0,1,21,1,59,0,0,0,165,194,2,0,6,0,0,0,2,0,0,0,6,144,65,129,1,144,193, 
  128,2,144,129,127,7,144,1,128,77,152,1,128,78,80,1,128,4,17,1,87,161,1,0,1,8,4,17,1,23,161,1,0,1,2,21,1,63,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1, 
  128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,39,0,0,0,135,0,0,0,4,0,1,2,21,1,63,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,48,1, 
  129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,39,0,0,0,136,0,0,0,4,0,1,2,21,1,59,0,0,0,165,194,2,0,6,0,0,0,2,0,0, 
  0,6,144,65,129,1,144,193,128,2,144,129,127,7,144,1,128,77,152,1,128,78,80,1,128,4,17,1,87,161,1,0,1,8,4,17,1,23,161,1,0,1,19,39,0,0,0,137,0,0,0,2,0,1,19,17,0,0,0,58, 
  0,0,0,1,0,1,21,1,69,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0, 
  0,0,35,0,0,0,1,0,17,1,48,162,1,0,1,2,21,1,47,0,0,0,141,187,2,0,5,0,0,0,2,0,0,0,20,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,96,162,1, 
  0,1,8,2,21,1,162,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,248,81,130,1,240,209,129,2,240,145,127,94,192,18,130,44,48,11,128,53,176,72,129,6,240,81,127,7,240,17,128,105,240,12,128,114,112, 
  15,128,125,16,2,128,126,96,5,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,65,212,1, 
  0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0, 
  0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,65,212,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15, 
  1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,65,212,1,0,15,1,188,156,0,0,15,1,200,156,0, 
  0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0, 
  0,1,4,15,1,65,212,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,15, 
  1,65,212,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,65,212,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0, 
  0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,110,165,1,0,17,1,3, 
  165,1,0,1,4,15,1,65,212,1,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,2,21,1,39,0,0,0,105,145,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,54,165,1,0,1,8,19,11, 
  0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58,240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0, 
  0,3,0,1,8,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,19,184,6,128,81,224,130,130,61,104,4,128,11,192,70,127,60,208,4,128,45,88,70,127,46,240,133,128,47,96,133,128,62,0,4,128, 
  63,112,67,128,95,120,2,128,97,16,2,128,15,1,110,165,1,0,17,1,203,209,1,0,1,15,1,110,165,1,0,17,1,196,206,1,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,110,165,1,0,1,19,47,0,0, 
  0,153,0,0,0,1,0,17,1,110,165,1,0,1,15,1,110,165,1,0,17,1,82,206,1,0,1,15,1,110,165,1,0,17,1,182,205,1,0,1,19,46,0,0,0,149,0,0,0,1,0,17,1,110,165,1,0,1,19,97, 
  0,0,0,21,1,0,0,1,0,17,1,110,165,1,0,1,15,1,110,165,1,0,17,1,89,166,1,0,1,19,19,0,0,0,63,0,0,0,4,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,110,165,1,0,1, 
  2,21,1,47,0,0,0,223,187,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,149,166,1,0,1,12,19,45,0,0,0,148,0,0,0,1,0, 
  1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,197,166,1,0,1,2,21,1,36,2,0,0,243,90,2,0,12, 
  0,0,0,3,0,0,0,58,208,68,130,1,72,201,129,2,72,137,127,94,152,5,130,44,16,12,128,53,88,71,129,6,72,73,127,7,72,9,128,105,64,13,128,114,48,15,128,125,80,9,128,126,16,2,128,4,19,94,0,0, 
  0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1, 
  146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,234,168,1,0,17,1,3,165,1,0,1,4,15,1,221,12,0,0,15,1,6,246,0, 
  0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71, 
  246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19, 
  78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116, 
  0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,15,1,221,12,0,0, 
  15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,221,12, 
  0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0, 
  195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61,40,4,128,46,128,133,126,47,240,196,126,95,56,2,128,97,208,1,128,15,1,234,168, 
  1,0,17,1,203,209,1,0,1,15,1,234,168,1,0,17,1,196,206,1,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,234,168,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,234,168,1,0,1,15,1, 
  234,168,1,0,17,1,82,206,1,0,1,15,1,234,168,1,0,17,1,174,169,1,0,1,19,46,0,0,0,150,0,0,0,3,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,234,168,1,0,1,1,19,47,0,0,0, 
  152,0,0,0,1,0,17,1,234,168,1,0,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,56,4,130,1,240,3,130,2,240,131,127,94,112,67,130,44,176,3,128,53,112,130,129,6,240,67,127, 
  7,240,3,128,98,48,131,128,105,240,2,128,114,176,2,128,125,48,2,128,126,248,3,128,12,17,1,141,203,1,0,1,12,17,1,100,201,1,0,1,12,17,1,59,199,1,0,1,12,17,1,18,197,1,0,1,12,17,1,233, 
  194,1,0,1,12,17,1,192,192,1,0,1,12,17,1,151,190,1,0,1,10,12,17,1,110,188,1,0,1,6,17,1,74,170,1,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,212,0,0,0,55,147,2,0, 
  19,0,0,0,4,0,0,0,128,96,6,128,1,88,6,131,2,88,134,131,3,48,195,131,4,48,3,129,53,216,3,128,6,88,6,128,7,88,6,128,84,24,5,128,105,152,4,128,94,216,5,128,11,152,3,128,44,24,6,128, 
  97,240,2,128,62,152,5,127,127,88,4,128,98,88,69,128,114,24,4,128,115,216,4,128,12,17,1,69,186,1,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,12,17,1,28,184,1,0,1,12,17,1,100,201,1,0, 
  1,12,17,1,59,199,1,0,1,12,17,1,243,181,1,0,1,12,17,1,18,197,1,0,1,12,17,1,202,179,1,0,1,12,17,1,161,177,1,0,1,12,17,1,233,194,1,0,1,12,17,1,120,175,1,0,1,12,17,1, 
  192,192,1,0,1,12,17,1,151,190,1,0,1,10,12,17,1,79,173,1,0,1,12,17,1,38,171,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,240,11,130,2,240,139,127, 
  94,32,72,130,44,32,9,128,53,144,137,129,6,240,75,127,7,240,11,128,98,48,132,128,105,192,10,128,114,240,4,128,125,32,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0, 
  0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78, 
  0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0, 
  0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113, 
  0,0,0,1,0,15,1,191,172,1,0,17,1,3,165,1,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,136,194,128,95,128,3,128,11,232,2,128,97,32,2,128,61,120,3, 
  128,62,120,3,128,47,232,67,126,19,47,0,0,0,153,0,0,0,1,0,17,1,191,172,1,0,1,15,1,191,172,1,0,17,1,203,209,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0, 
  0,1,0,17,1,191,172,1,0,1,1,15,1,191,172,1,0,17,1,196,206,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,191,172,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0, 
  0,58,48,2,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,248,132,128,105,136,11,128,114,184,5,128,125,232,6,128,126,248,2,128,4,19,35,0,0,0,113, 
  0,0,0,1,0,15,1,232,174,1,0,17,1,3,165,1,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0, 
  0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97, 
  0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1, 
  4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,129,128,81,144,193,128,95,16, 
  4,128,11,16,3,128,97,168,3,128,61,160,3,128,62,160,3,128,47,128,66,126,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,232,174,1,0,1,19,97,0,0,0,21,1,0, 
  0,1,0,17,1,232,174,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,232,174,1,0,1,1,15,1,232,174,1,0,17,1,203,209,1,0,1,15,1,232,174,1,0,17,1,196,206,1,0,1,2,21,1,152,1, 
  0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,248,132,128,105,136,11,128,114,184,5,128,125,232, 
  6,128,126,248,2,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,17,177,1,0,17,1,3,165,1,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178, 
  0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1, 
  181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3, 
  0,0,0,63,80,131,128,81,32,194,128,95,232,2,128,11,144,1,128,97,128,2,128,61,224,3,128,62,224,3,128,47,232,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,17,177,1,0,1,19,61,0,0,0,185,0, 
  0,0,2,0,1,15,1,17,177,1,0,17,1,203,209,1,0,1,15,1,17,177,1,0,17,1,196,206,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,17,177,1,0,1,1,19,97,0,0,0,21,1,0,0,1, 
  0,17,1,17,177,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,6,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98, 
  48,132,128,105,136,11,128,114,240,4,128,125,232,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1, 
  0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,58,179,1,0,17,1,3,165,1,0,1,4,19,93,0,0,0,11,1,0,0, 
  1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15, 
  1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0, 
  143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,176,194,128,95,128,3,128,11,32,2,128,97,16,3,128,61,120,3,128,62,120,3,128,47,232,67,126,19,47,0,0,0,153,0,0,0,1,0,17, 
  1,58,179,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,58,179,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,58,179,1,0,17,1,203,209,1,0,1,1,15,1,58,179,1,0,17,1,196,206, 
  1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,58,179,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,192,10,130,1,184,12,130,2,184,140,127,94,32,72,130,44,32,9,128, 
  53,144,137,129,6,184,76,127,7,184,12,128,98,48,132,128,105,136,11,128,114,240,4,128,125,32,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0, 
  178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1, 
  0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17, 
  1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,99,181,1,0,17,1,3,165,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7, 
  0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,96,130,128,81,248,193,128,95,144,1,128,11,240,2,128,97,16,4,128,61,88,2,128,62,88,2,128,47,128,67, 
  126,15,1,99,181,1,0,17,1,196,206,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,99,181,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,99,181, 
  1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,99,181,1,0,1,15,1,99,181,1,0,17,1,203,209,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,8,130,1,184,12, 
  130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,48,132,128,105,136,11,128,114,240,4,128,125,32,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0, 
  0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1, 
  0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0, 
  0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0, 
  0,113,0,0,0,1,0,15,1,140,183,1,0,17,1,3,165,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17, 
  1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,131,128,81,144,193,128,95,128,2,128,11,232,2,128,97,16, 
  4,128,61,120,3,128,62,120,3,128,47,240,65,126,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,140,183,1,0,1,15,1,140,183,1,0,17,1,196,206,1,0,1,19,47,0, 
  0,0,152,0,0,0,1,0,17,1,140,183,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,140,183,1,0,1,15,1,140,183,1,0,17,1,203,209,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0, 
  0,0,3,0,0,0,58,32,6,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,48,132,128,105,136,11,128,114,240,4,128,125,232,6,128,126,48,2,128,4,19, 
  94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0, 
  0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140, 
  7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,181,185,1,0,17,1,3,165,1,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1, 
  204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1, 
  30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81, 
  32,194,128,95,128,2,128,11,144,1,128,97,232,2,128,61,224,3,128,62,224,3,128,47,80,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,181,185,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,181, 
  185,1,0,17,1,196,206,1,0,1,15,1,181,185,1,0,17,1,203,209,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,181,185,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,181,185,1,0,1, 
  2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,240,11,130,2,240,139,127,94,32,72,130,44,32,9,128,53,144,137,129,6,240,75,127,7,240,11,128,98,48,132,128,105,192,10,128,114, 
  240,4,128,125,32,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15, 
  0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0, 
  17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,222,187,1,0,17,1,3,165,1,0,1,2,21,0,143,0,0,0,255,255,255,255, 
  8,0,0,0,3,0,0,0,63,96,130,128,81,240,194,128,95,248,1,128,11,88,3,128,97,144,1,128,61,80,3,128,62,80,3,128,47,232,67,126,15,1,222,187,1,0,17,1,203,209,1,0,1,15,1,222,187,1,0,17, 
  1,196,206,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,222,187,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,222,187,1,0,1,19,97,0,0,0, 
  21,1,0,0,1,0,17,1,222,187,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,6,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127, 
  7,184,12,128,98,48,132,128,105,136,11,128,114,240,4,128,125,232,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97, 
  0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,7,190,1,0,17,1,3,165,1,0,1,4,19,93,0,0, 
  0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0, 
  1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,131,128,81,144,193,128,95,136,2,128,11,240,2,128,97,16,4,128,61,128,2,128,62,128,2,128,47,240,65,126,19,61,0,0,0,185,0, 
  0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,7,190,1,0,1,1,15,1,7,190,1,0,17,1,196,206,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,7,190,1,0,1,19,47,0,0,0, 
  153,0,0,0,1,0,17,1,7,190,1,0,1,15,1,7,190,1,0,17,1,203,209,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,240,4,130,1,184,12,130,2,184,140,127,94,232,72, 
  130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,48,132,128,105,136,11,128,114,184,5,128,125,232,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0, 
  0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,48,192,1,0,17,1,3,165,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0, 
  0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15, 
  1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,130,128,81,32,194,128,95,16,4,128,11,144,1,128,97,16,3,128,61,8,4,128,62,8, 
  4,128,47,120,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,48,192,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,48,192,1,0,1,15,1,48,192,1,0, 
  17,1,203,209,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,48,192,1,0,1,1,15,1,48,192,1,0,17,1,196,206,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248, 
  11,130,1,240,11,130,2,240,139,127,94,32,72,130,44,32,9,128,53,144,137,129,6,240,75,127,7,240,11,128,98,48,132,128,105,192,10,128,114,240,4,128,125,32,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0, 
  1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0, 
  0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0, 
  0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0, 
  0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,89,194,1,0,17,1,3,165,1,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,136,194,128,95,32,2,128,11, 
  120,3,128,97,16,4,128,61,8,4,128,62,8,4,128,47,232,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,89,194,1,0,1,15,1,89,194,1,0,17,1,196,206,1,0,1,19,61,0,0,0,185,0,0,0,2, 
  0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,89,194,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,89,194,1,0,1,1,15,1,89,194,1,0,17,1,203,209,1,0,1,2,21,1,152,1,0,0,112, 
  111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,240,11,130,2,240,139,127,94,32,72,130,44,32,9,128,53,144,137,129,6,240,75,127,7,240,11,128,98,48,132,128,105,192,10,128,114,240,4,128,125,32,6,128,126, 
  48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0, 
  17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8, 
  0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0, 
  15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,130,196,1,0,17,1,3,165,1,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0, 
  63,232,131,128,81,24,195,128,95,32,2,128,11,144,1,128,97,120,3,128,61,224,3,128,62,224,3,128,47,136,66,126,19,47,0,0,0,152,0,0,0,1,0,17,1,130,196,1,0,1,15,1,130,196,1,0,17,1,196,206, 
  1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,130,196,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,130,196,1,0,17,1,203,209,1,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1, 
  130,196,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,240,11,130,2,240,139,127,94,32,72,130,44,32,9,128,53,144,137,129,6,240,75,127,7,240,11,128,98,48,132,128, 
  105,192,10,128,114,240,4,128,125,32,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1, 
  0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15, 
  1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,171,198,1,0,17,1,3,165,1,0,1,2,21,0,143,0,0, 
  0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,130,128,81,32,194,128,95,128,2,128,11,144,1,128,97,120,3,128,61,224,3,128,62,224,3,128,47,232,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,171,198, 
  1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,171,198,1,0,17,1,196,206,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,171,198,1,0,1,15,1,171,198,1,0,17,1,203,209,1,0,1,1, 
  19,97,0,0,0,21,1,0,0,1,0,17,1,171,198,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138, 
  129,6,184,76,127,7,184,12,128,98,248,132,128,105,136,11,128,114,184,5,128,125,232,6,128,126,248,2,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,212,200,1,0,17,1,3,165,1,0,1,4,19,94,0,0,0, 
  12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1, 
  4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82, 
  114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17, 
  1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,131,128,81,240,194,128,95,136,2,128,11,144,1,128,97,32,2,128,61,80,3,128,62,80,3,128,47,232,67,126,19,47, 
  0,0,0,152,0,0,0,1,0,17,1,212,200,1,0,1,15,1,212,200,1,0,17,1,203,209,1,0,1,15,1,212,200,1,0,17,1,196,206,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,19,47,0,0,0, 
  153,0,0,0,1,0,17,1,212,200,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,212,200,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,6,130,1,184,12,130,2,184, 
  140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,48,132,128,105,136,11,128,114,240,4,128,125,232,6,128,126,48,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226, 
  0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62, 
  0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0, 
  15,1,253,202,1,0,17,1,3,165,1,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,42,15,0,0,17,1,101,119, 
  0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,120,131,128,81,176,194,128,95,16,3,128,11,32,2,128,97,16,4,128,61, 
  8,4,128,62,8,4,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1,253,202,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,253,202,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15, 
  1,253,202,1,0,17,1,196,206,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,253,202,1,0,1,1,15,1,253,202,1,0,17,1,203,209,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3, 
  0,0,0,58,32,6,130,1,184,12,130,2,184,140,127,94,232,72,130,44,232,9,128,53,88,138,129,6,184,76,127,7,184,12,128,98,48,132,128,105,136,11,128,114,240,4,128,125,232,6,128,126,48,2,128,4,19,94,0,0, 
  0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4, 
  19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0, 
  1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,38,205,1,0,17,1,3,165,1,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0, 
  19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0, 
  0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,248,193,128, 
  95,144,1,128,11,88,2,128,97,240,2,128,61,232,2,128,62,232,2,128,47,88,67,126,15,1,38,205,1,0,17,1,196,206,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0, 
  17,1,38,205,1,0,1,1,15,1,38,205,1,0,17,1,203,209,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,38,205,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,38,205,1,0,1,2,21,1, 
  143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,56,4,130,1,240,3,130,2,240,131,127,94,112,67,130,44,176,3,128,53,112,130,129,6,240,67,127,7,240,3,128,98,48,131,128,105,240,2,128,114,176,2,128, 
  125,48,2,128,126,248,3,128,12,17,1,141,203,1,0,1,12,17,1,100,201,1,0,1,12,17,1,59,199,1,0,1,12,17,1,18,197,1,0,1,12,17,1,233,194,1,0,1,12,17,1,192,192,1,0,1,12,17,1,151, 
  190,1,0,1,10,12,17,1,110,188,1,0,1,6,17,1,74,170,1,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,47,0,0,0,124,189,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128, 
  2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,142,206,1,0,1,10,12,19,60,0,0,0,182,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128, 
  2,48,129,127,7,48,1,128,62,56,1,128,8,4,15,1,196,74,0,0,17,1,128,33,0,0,1,2,21,1,63,0,0,0,34,190,2,0,5,0,0,0,2,0,0,0,115,176,1,128,1,240,1,128,2,240,129,128,11,112, 
  65,127,118,48,1,128,12,17,1,16,207,1,0,1,12,17,1,16,207,1,0,1,12,17,1,16,207,1,0,1,10,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0, 
  0,0,115,48,1,128,1,32,2,128,2,32,130,128,11,176,65,127,118,112,1,128,4,17,1,3,209,1,0,1,4,17,1,23,208,1,0,1,4,15,1,86,207,1,0,17,1,98,75,0,0,1,8,2,21,1,51,0,0,0, 
  243,150,2,0,4,0,0,0,2,0,0,0,118,80,1,128,1,144,1,128,2,144,129,127,115,16,1,128,4,17,1,223,207,1,0,1,4,17,1,149,207,1,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,7,35, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  81,0,0,0,248,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,250,0,0,0,4,0,1,19,81,0,0,0, 
  244,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,4,17,1,184,208,1,0,1,4,15,1,110,208,1,0,17,1,98,75,0,0,1,21,9,27,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0, 
  0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0, 
  3,0,1,21,1,57,0,0,0,186,149,2,0,4,0,0,0,2,0,0,0,118,136,1,128,1,16,1,128,2,16,129,127,11,24,1,128,8,4,15,1,147,209,1,0,17,1,98,75,0,0,1,4,17,1,72,209,1,0,1, 
  19,81,0,0,0,241,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19, 
  81,0,0,0,247,0,0,0,4,0,1,19,81,0,0,0,237,0,0,0,3,0,1,21,1,51,0,0,0,47,191,2,0,4,0,0,0,2,0,0,0,128,24,1,128,1,16,1,128,2,16,1,128,127,88,1,128,10,12,17, 
  1,11,210,1,0,1,12,17,1,11,210,1,0,1,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,88,1,128,1,80,1,128,2,80,1,128,127,16,1, 
  128,4,17,1,64,211,1,0,1,8,4,17,1,63,210,1,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,160,67,129,1,168,194,128,2,168,130,127,51,80,1,128,125,184,1,128,126,176,2, 
  128,4,19,95,0,0,0,15,1,0,0,3,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,1,211,1,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0, 
  0,0,226,0,0,0,1,0,17,1,1,211,1,0,1,4,15,1,194,210,1,0,17,1,140,7,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1, 
  0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0, 
  0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,176,66,129,1,168, 
  194,128,2,168,130,127,51,80,1,128,125,184,1,128,126,32,3,128,4,19,95,0,0,0,18,1,0,0,3,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,2,212,1,0, 
  1,8,4,15,1,195,211,1,0,17,1,140,7,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,2,212,1,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,19,0,0,0,63,0, 
  0,0,4,0,1,19,6,0,0,0,12,0,0,0,1,0,1,19,6,0,0,0,15,0,0,0,1,0,1,21,1,69,0,0,0,241,68,2,0,5,0,0,0,2,0,0,0,6,32,2,129,1,32,2,128,2,32,130,127,7, 
  32,2,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,171,212,1,0,1,8,2,21,1,47,0,0,0,141,187,2,0,5,0,0,0,2,0,0,0,20,56,1, 
  128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,219,212,1,0,1,2,21,1,162,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,232,76,130,1,144,196,129,2,144,132,127,94,176,13, 
  130,44,80,19,128,53,232,71,129,6,144,68,127,7,144,4,128,105,16,2,128,114,104,10,128,125,152,4,128,126,0,16,128,4,15,1,188,6,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1, 
  6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,93,0,0,0, 
  11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,188,6,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250, 
  245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,188,6,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15, 
  1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1, 
  4,15,1,188,6,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,233,215,1,0,17,1,126,215,1,0,1,4,15,1,188,6,2,0,15,1,188,156,0,0,15,1, 
  200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4, 
  19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,188,6,2,0,15,1,188,156,0,0,15,1,200,156, 
  0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,188,6,2,0,15,1,188,156,0,0,15,1, 
  200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,2,21,1,39,0,0,0,105,145,2,0,3,0,0,0,1,0,0,0, 
  2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,177,215,1,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58, 
  240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,8,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,61,104,4,128,81,224,130,130,18,184,6,128,11, 
  192,6,128,60,208,4,128,45,88,198,126,46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16,2,128,15,1,233,215,1,0,17,1,70,4,2,0,1,15,1,233,215,1,0,17,1,63,1,2,0, 
  1,19,61,0,0,0,183,0,0,0,1,0,17,1,233,215,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,233,215,1,0,1,15,1,233,215,1,0,17,1,205,0,2,0,1,15,1,233,215,1,0,17,1,49,0, 
  2,0,1,19,46,0,0,0,149,0,0,0,1,0,17,1,233,215,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,233,215,1,0,1,15,1,233,215,1,0,17,1,212,216,1,0,1,19,18,0,0,0,62,0,0, 
  0,4,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,233,215,1,0,1,2,21,1,47,0,0,0,223,187,2,0,5,0,0,0,2,0,0,0,84,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112, 
  1,128,12,17,1,16,217,1,0,1,10,12,19,45,0,0,0,148,0,0,0,1,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48, 
  1,128,8,4,17,1,64,217,1,0,1,2,21,1,36,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,176,74,130,1,104,205,129,2,104,141,127,94,112,13,130,44,128,9,128,53,144,71,129,6,104,77,127,7,104, 
  13,128,105,48,15,128,114,120,11,128,125,16,2,128,126,208,4,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0, 
  0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78, 
  0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0, 
  0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128, 
  7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,19,35,0,0,0, 
  113,0,0,0,1,0,15,1,101,219,1,0,17,1,126,215,1,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,2,21,0,195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61, 
  40,4,128,46,128,133,126,47,240,196,126,95,56,2,128,97,208,1,128,15,1,101,219,1,0,17,1,70,4,2,0,1,15,1,101,219,1,0,17,1,63,1,2,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,101,219, 
  1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,101,219,1,0,1,15,1,101,219,1,0,17,1,205,0,2,0,1,15,1,101,219,1,0,17,1,41,220,1,0,1,19,46,0,0,0,150,0,0,0,3,0,1,19, 
  97,0,0,0,21,1,0,0,1,0,17,1,101,219,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,101,219,1,0,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,56,3,130,1, 
  48,3,130,2,48,131,127,94,248,67,130,44,184,3,128,53,112,130,129,6,48,67,127,7,48,3,128,98,176,130,128,105,56,4,128,114,48,2,128,125,120,3,128,126,240,2,128,12,17,1,8,254,1,0,1,12,17,1,223,251, 
  1,0,1,12,17,1,182,249,1,0,1,12,17,1,141,247,1,0,1,10,6,17,1,105,229,1,0,1,12,17,1,64,227,1,0,1,12,17,1,23,225,1,0,1,12,17,1,238,222,1,0,1,12,17,1,197,220,1,0,1, 
  12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,192,10,130,2,192,138,127,94,144,72,130,44,96,7,128,53,144,137,129,6,192,74,127,7, 
  192,10,128,98,208,135,128,105,48,2,128,114,200,10,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0, 
  1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116, 
  0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,94,222,1,0,17,1,126,215,1,0, 
  1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,248,129,128,81,24,196,128,95,144,1,128,11,136,3,128,97,24,3,128,61,128,3,128,62,128,3,128,47,136,66,126,15,1,94,222,1,0,17,1, 
  63,1,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,94,222,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,94,222,1,0,1,15,1,94,222,1,0,17,1,70,4,2,0,1,1,19,47,0,0,0, 
  152,0,0,0,1,0,17,1,94,222,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,208,7,130,1,136,11,130,2,136,139,127,94,88,73,130, 
  44,96,7,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152,136,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15, 
  1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0, 
  0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,135,224,1,0,17,1,126,215,1,0, 
  1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1, 
  30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,136,195,128,95,144,2,128,11,248,2,128,97,32,2,128,61,136,2,128,62,136,2, 
  128,47,232,67,126,19,47,0,0,0,153,0,0,0,1,0,17,1,135,224,1,0,1,15,1,135,224,1,0,17,1,70,4,2,0,1,1,15,1,135,224,1,0,17,1,63,1,2,0,1,19,47,0,0,0,152,0,0,0,1, 
  0,17,1,135,224,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,135,224,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,3, 
  130,1,136,11,130,2,136,139,127,94,88,73,130,44,40,8,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152,136,128,105,48,2,128,114,144,11,128,125,40,6,128,126,40,4,128,4,15,1,30,15,0,0,15,1,159,116, 
  0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,176,226,1,0,17,1,126,215,1,0,1,4,19,94,0,0,0,12,1, 
  0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0, 
  0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1, 
  4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116, 
  0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,130,128,81,24,196,128,95,144,1,128,11,248, 
  1,128,97,176,3,128,61,168,3,128,62,168,3,128,47,24,67,126,15,1,176,226,1,0,17,1,63,1,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,176,226,1,0,1,19,47,0,0,0,153,0,0,0,1,0, 
  17,1,176,226,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,176,226,1,0,1,1,15,1,176,226,1,0,17,1,70,4,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,2,21,1,152,1,0,0,112,111, 
  2,0,13,0,0,0,3,0,0,0,58,208,7,130,1,136,11,130,2,136,139,127,94,88,73,130,44,96,7,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152,136,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96, 
  3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0, 
  0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0, 
  0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,217,228,1,0,17,1,126,215,1,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0, 
  1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63, 
  232,131,128,81,32,195,128,95,176,2,128,11,144,1,128,97,128,3,128,61,24,3,128,62,24,3,128,47,32,66,126,19,47,0,0,0,152,0,0,0,1,0,17,1,217,228,1,0,1,19,97,0,0,0,21,1,0,0,1,0, 
  17,1,217,228,1,0,1,15,1,217,228,1,0,17,1,63,1,2,0,1,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,217,228,1,0,17,1,70,4,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,217, 
  228,1,0,1,2,21,1,212,0,0,0,55,147,2,0,19,0,0,0,4,0,0,0,128,88,4,128,1,152,5,131,2,152,133,131,3,112,195,131,4,112,3,129,53,216,4,128,6,152,5,128,7,152,5,128,84,160,5,128,105, 
  96,6,128,94,32,6,128,11,48,3,128,44,224,5,128,97,24,4,128,62,152,4,127,127,88,5,128,98,24,69,128,114,216,3,128,115,240,2,128,12,17,1,100,245,1,0,1,12,17,1,59,243,1,0,1,12,19,62,0,0, 
  0,188,0,0,0,1,0,1,12,17,1,8,254,1,0,1,12,17,1,18,241,1,0,1,12,17,1,233,238,1,0,1,12,17,1,192,236,1,0,1,12,17,1,223,251,1,0,1,12,17,1,182,249,1,0,1,12,17,1,151, 
  234,1,0,1,10,12,17,1,110,232,1,0,1,12,17,1,23,225,1,0,1,12,17,1,238,222,1,0,1,12,17,1,197,220,1,0,1,12,17,1,69,230,1,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3, 
  0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94,144,72,130,44,96,7,128,53,144,137,129,6,192,74,127,7,192,10,128,98,208,135,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96,3,128,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,222,231,1,0,17,1,126,215,1,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,176,195,128, 
  95,32,2,128,11,24,3,128,97,16,4,128,61,168,3,128,62,168,3,128,47,136,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,222,231,1,0,1,15,1,222,231,1,0,17,1,63,1,2,0,1,19,97,0,0,0, 
  21,1,0,0,1,0,17,1,222,231,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,222,231,1,0,1,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,222,231,1,0,17,1,70,4,2,0,1,2,21,1, 
  152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,192,10,130,2,192,138,127,94,144,72,130,44,96,7,128,53,144,137,129,6,192,74,127,7,192,10,128,98,208,135,128,105,48,2,128,114,200,10,128, 
  125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78, 
  0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0, 
  1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0, 
  0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204, 
  80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,7,234,1,0,17,1,126,215,1,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,63,144,129,128,81,136,195,128,95,32,2,128,11,240,2,128,97,136,2,128,61,128,3,128,62,128,3,128,47,232,67,126,19,47,0,0,0,153,0,0,0,1,0,17,1,7,234,1,0,1,15,1,7,234,1, 
  0,17,1,63,1,2,0,1,15,1,7,234,1,0,17,1,70,4,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,7,234,1,0,1,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0, 
  0,1,0,17,1,7,234,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,136,11,130,2,136,139,127,94,88,73,130,44,40,8,128,53,88,138,129,6,136,75,127,7,136,11, 
  128,98,152,136,128,105,248,2,128,114,144,11,128,125,40,6,128,126,40,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,48,236,1,0,17,1,126,215,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97, 
  0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0, 
  1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62, 
  0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2, 
  21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,32,130,128,81,176,195,128,95,176,2,128,11,144,1,128,97,16,4,128,61,24,3,128,62,24,3,128,47,32,67,126,19,47,0,0,0,152,0,0,0,1, 
  0,17,1,48,236,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,48,236,1,0,1,15,1,48,236,1,0,17,1,63,1,2,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,48,236,1,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,15,1,48,236,1,0,17,1,70,4,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,136,11,130,2,136,139,127,94,88,73,130,44,40, 
  8,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152,136,128,105,248,2,128,114,144,11,128,125,40,6,128,126,40,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,89,238,1,0,17,1,126,215,1,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0, 
  19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0, 
  0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4, 
  19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15, 
  1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,248,194,128,95,40,2,128,11,144,1,128,97,144,2,128,61,32,2,128,62,32,2,128,47, 
  88,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,89,238,1,0,1,1,15,1,89,238,1,0,17,1,63,1,2,0,1,15,1,89,238,1,0,17,1,70,4,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1, 
  19,97,0,0,0,21,1,0,0,1,0,17,1,89,238,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,89,238,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1, 
  192,10,130,2,192,138,127,94,144,72,130,44,96,7,128,53,144,137,129,6,192,74,127,7,192,10,128,98,208,135,128,105,48,2,128,114,200,10,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0, 
  15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1, 
  4,19,35,0,0,0,113,0,0,0,1,0,15,1,130,240,1,0,17,1,126,215,1,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,130,128,81,24,196,128,95,176,3,128,11,144,1,128, 
  97,32,2,128,61,168,3,128,62,168,3,128,47,24,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,130,240,1,0,1,15,1,130,240,1,0,17,1,70,4,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1, 
  130,240,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,130,240,1,0,1,1,15,1,130,240,1,0,17,1,63,1,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,2,21,1,152,1,0,0,112,111,2,0, 
  13,0,0,0,3,0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94,144,72,130,44,96,7,128,53,144,137,129,6,192,74,127,7,192,10,128,98,208,135,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96,3,128, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1, 
  0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225, 
  0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,171,242,1,0,17,1,126,215,1,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,32,131, 
  128,81,176,195,128,95,16,4,128,11,144,1,128,97,176,2,128,61,24,3,128,62,24,3,128,47,32,66,126,19,47,0,0,0,152,0,0,0,1,0,17,1,171,242,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1, 
  171,242,1,0,1,15,1,171,242,1,0,17,1,70,4,2,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,171,242,1,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,171,242,1,0,17,1,63,1,2, 
  0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,144,9,130,1,136,11,130,2,136,139,127,94,144,72,130,44,96,7,128,53,88,138,129,6,136,75,127,7,136,11,128,98,208,135,128,105,48,2, 
  128,114,144,11,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0, 
  0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0, 
  0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4, 
  15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,17,1,204,80,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,212,244,1,0,17,1,126,215,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15, 
  1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255, 
  255,255,8,0,0,0,3,0,0,0,63,32,130,128,81,32,195,128,95,16,4,128,11,144,1,128,97,176,2,128,61,24,3,128,62,24,3,128,47,128,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,212,244,1,0,1, 
  19,47,0,0,0,153,0,0,0,1,0,17,1,212,244,1,0,1,15,1,212,244,1,0,17,1,70,4,2,0,1,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,212,244,1, 
  0,1,15,1,212,244,1,0,17,1,63,1,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94,144,72,130,44,96,7,128,53,144,137,129,6,192, 
  74,127,7,192,10,128,98,208,135,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88, 
  6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0, 
  0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15, 
  1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1, 
  8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,253,246,1,0,17,1,126,215,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140, 
  7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,248,130,128,81,136,195,128,95,144,1,128,11,232,3,128,97,0,2,128,61,248,1,128,62,248,1,128,47,104,66,126,15,1,253,246,1, 
  0,17,1,63,1,2,0,1,1,15,1,253,246,1,0,17,1,70,4,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,253,246,1,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,253,246,1,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,253,246,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94, 
  144,72,130,44,96,7,128,53,144,137,129,6,192,74,127,7,192,10,128,98,208,135,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0, 
  22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0, 
  2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0, 
  15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,38,249,1,0,17,1,126,215,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,248,129,128,81,32,195,128,95,144,1,128,11,144,2,128,97,128,3,128,61,136,2,128, 
  62,136,2,128,47,232,67,126,15,1,38,249,1,0,17,1,63,1,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,38,249,1,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,38,249,1,0,1,19,61, 
  0,0,0,185,0,0,0,2,0,1,15,1,38,249,1,0,17,1,70,4,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,38,249,1,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0, 
  58,96,7,130,1,136,11,130,2,136,139,127,94,88,73,130,44,40,8,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152,136,128,105,48,2,128,114,144,11,128,125,96,5,128,126,96,3,128,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,79,251,1,0,17,1,126,215, 
  1,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,130,128,81,24,196,128,95,176,3, 
  128,11,24,3,128,97,144,1,128,61,168,3,128,62,168,3,128,47,248,65,126,15,1,79,251,1,0,17,1,70,4,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,79,251,1,0,1,19,47,0,0,0,153,0,0, 
  0,1,0,17,1,79,251,1,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,79,251,1,0,1,1,15,1,79,251,1,0,17,1,63,1,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,2,21,1,152,1,0, 
  0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,136,11,130,2,136,139,127,94,88,73,130,44,40,8,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152,136,128,105,248,2,128,114,144,11,128,125,40,6, 
  128,126,40,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,120,253,1,0,17,1,126,215,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0, 
  0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15, 
  1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30, 
  15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181, 
  3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0, 
  0,0,63,144,129,128,81,24,196,128,95,64,3,128,11,32,2,128,97,176,3,128,61,168,3,128,62,168,3,128,47,176,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,120,253,1,0,1,19,47,0,0,0,152,0,0, 
  0,1,0,17,1,120,253,1,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,120,253,1,0,1,15,1,120,253,1,0,17,1,63,1,2,0,1,1,15,1,120,253,1,0,17,1,70,4,2,0,1,19,61,0,0,0, 
  185,0,0,0,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,136,11,130,2,136,139,127,94,88,73,130,44,40,8,128,53,88,138,129,6,136,75,127,7,136,11,128,98,152, 
  136,128,105,248,2,128,114,144,11,128,125,40,6,128,126,40,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,161,255,1,0,17,1,126,215,1,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114, 
  0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0, 
  22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19, 
  97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0, 
  187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1, 
  128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,2,21,0,143, 
  0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,130,128,81,24,196,128,95,24,3,128,11,128,3,128,97,32,2,128,61,16,4,128,62,16,4,128,47,144,65,126,19,97,0,0,0,21,1,0,0,1,0,17,1, 
  161,255,1,0,1,15,1,161,255,1,0,17,1,70,4,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,161,255,1,0,1,15,1,161,255,1,0,17,1,63,1,2,0,1,19,47,0,0,0,152,0,0,0,1,0, 
  17,1,161,255,1,0,1,1,19,61,0,0,0,185,0,0,0,2,0,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,112,3,130,2,112,131,127,94,248,67,130,44,184,3,128,53, 
  176,130,129,6,112,67,127,7,112,3,128,98,240,130,128,105,56,4,128,114,112,2,128,125,120,3,128,126,48,3,128,6,17,1,105,229,1,0,1,12,17,1,8,254,1,0,1,12,17,1,223,251,1,0,1,12,17,1,182,249, 
  1,0,1,12,17,1,141,247,1,0,1,10,12,17,1,64,227,1,0,1,12,17,1,23,225,1,0,1,12,17,1,238,222,1,0,1,12,17,1,197,220,1,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,47, 
  0,0,0,124,189,2,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,9,1,2,0,1,10,12,19,60,0,0,0,182,0,0,0,1,0,1,21,1,53, 
  0,0,0,149,112,2,0,5,0,0,0,2,0,0,0,6,160,1,129,1,160,1,128,2,160,129,127,7,160,1,128,62,48,1,128,4,15,1,196,74,0,0,17,1,128,33,0,0,1,8,2,21,1,63,0,0,0,34,190,2, 
  0,5,0,0,0,2,0,0,0,115,48,1,128,1,240,1,128,2,240,129,128,11,176,65,127,118,112,1,128,12,17,1,139,1,2,0,1,12,17,1,139,1,2,0,1,12,17,1,139,1,2,0,1,10,12,19,81,0,0,0, 
  251,0,0,0,1,0,1,21,1,69,0,0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,232,1,128,1,112,1,128,2,112,129,128,11,120,65,127,118,48,1,128,4,17,1,90,3,2,0,1,8,4,15,1,153,2,2, 
  0,17,1,98,75,0,0,1,4,17,1,209,1,2,0,1,2,21,1,57,0,0,0,186,149,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1,80,1,128,2,80,129,127,11,88,1,128,4,17,1,78,2,2,0,1,8, 
  4,15,1,22,2,2,0,17,1,98,75,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0, 
  0,0,247,0,0,0,4,0,1,8,19,81,0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,51,0,0,0,243,150,2,0,4,0,0,0,2,0,0,0,118,16,1,128,1, 
  144,1,128,2,144,129,127,115,80,1,128,4,17,1,16,3,2,0,1,4,17,1,216,2,2,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,88, 
  129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,250,0,0,0,4,0,1,8,19,81,0,0,0,244,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19, 
  81,0,0,0,249,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,252,3,2,0,17,1,98,75,0,0,1,4,17,1,177,3,2,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,81,0,0,0,242,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0, 
  243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,1,51,0,0,0,47,191,2,0,4,0,0,0, 
  2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,12,17,1,134,4,2,0,1,10,12,17,1,134,4,2,0,1,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0, 
  4,0,0,0,2,0,0,0,128,88,1,128,1,80,1,128,2,80,1,128,127,16,1,128,4,17,1,187,5,2,0,1,8,4,17,1,186,4,2,0,1,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0, 
  114,64,66,129,1,176,194,128,2,176,130,127,51,184,2,128,125,80,1,128,126,32,3,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,124,5,2,0,1,4,15,1,61,5,2, 
  0,17,1,140,7,0,0,1,8,4,19,95,0,0,0,15,1,0,0,3,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,124,5,2,0,1,2,21,7,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21, 
  1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,64,66,129,1,160,195,128,2,160,131,127,51,168,3,128,125,80,1,128,126,176,2,128,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0, 
  0,0,1,0,17,1,125,6,2,0,1,4,15,1,62,6,2,0,17,1,140,7,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,125,6,2,0,1,8,4,19,95,0, 
  0,0,18,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,18,0,0,0,62,0,0,0,4,0,1,19,6,0,0,0,11,0,0,0,1,0,1,21,1,88,0,0,0,125,90,2,0,6,0,0,0,2,0,0,0,6,184, 
  66,129,1,184,194,128,2,184,130,127,7,184,2,128,53,80,1,128,58,240,1,128,4,15,1,231,7,2,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,152,7,2,0, 
  17,1,45,7,2,0,1,8,2,21,1,39,0,0,0,4,195,2,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,96,7,2,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1, 
  21,1,55,0,0,0,33,107,2,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,63,0,0,0,189,0,0,0,3,0,1,2,21,0,78,0, 
  0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,160,129,128,21,216,1,128,63,16,1,128,11,224,65,127,19,47,0,0,0,153,0,0,0,1,0,17,1,152,7,2,0,1,17,1,231,7,2,0,1,1,19,47,0,0, 
  0,152,0,0,0,1,0,17,1,152,7,2,0,1,2,21,1,47,0,0,0,141,187,2,0,5,0,0,0,2,0,0,0,20,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,23,8,2,0, 
  1,8,2,21,1,162,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,32,71,130,1,8,213,129,2,8,149,127,94,232,12,130,44,96,5,128,53,232,71,129,6,8,85,127,7,8,21,128,105,104,10,128,114,56,15, 
  128,125,184,17,128,126,16,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,141,57,2,0, 
  15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,141,57, 
  2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30,1,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,186,10,2,0,17,1,45,7,2,0,1,4,15,1,141,57,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15, 
  1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,141,57,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0, 
  0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,141, 
  57,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,17,1,204,80,0,0,1,4,15,1,141,57,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15, 
  1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,141,57,2,0,15,1,188,156,0,0,15,1,200,156,0,0,15,1,250,245,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147, 
  116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,8,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,45,88,134,128,81,224,130,130,61,104,4,128,11,192,6,128,60,208,4,128,21,184,198,126, 
  46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16,2,128,15,1,186,10,2,0,17,1,23,55,2,0,1,15,1,186,10,2,0,17,1,16,52,2,0,1,19,61,0,0,0,183,0,0,0,1, 
  0,17,1,186,10,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,186,10,2,0,1,15,1,186,10,2,0,17,1,158,51,2,0,1,15,1,186,10,2,0,17,1,2,51,2,0,1,19,46,0,0,0,149,0,0, 
  0,1,0,17,1,186,10,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,186,10,2,0,1,15,1,186,10,2,0,17,1,165,11,2,0,1,19,21,0,0,0,65,0,0,0,4,0,1,1,19,47,0,0,0,152, 
  0,0,0,1,0,17,1,186,10,2,0,1,2,21,1,47,0,0,0,223,187,2,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,225,11,2,0,1, 
  12,19,45,0,0,0,148,0,0,0,1,0,1,21,1,47,0,0,0,228,109,2,0,5,0,0,0,2,0,0,0,84,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,17,12,2,0,1,8, 
  2,21,1,36,2,0,0,243,90,2,0,12,0,0,0,3,0,0,0,58,72,75,130,1,128,200,129,2,128,136,127,94,208,4,130,44,240,15,128,53,16,76,129,6,128,72,127,7,128,8,128,105,0,14,128,114,144,6,128,125, 
  136,8,128,126,16,2,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1, 
  6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0, 
  15,1,147,116,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0, 
  0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1,159,116,0,0,17,1,82,114,0, 
  0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,54,14,2,0,17,1,45,7,2,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,147,116,0,0, 
  15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,15,1,221,12,0,0,15,1,6,246,0,0,15,1,71,246,0,0,15,1,146,246,0,0,15,1,176,30, 
  1,0,17,1,101,119,0,0,1,2,21,0,195,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61,40,4,128,46,128,133,126,47,240,196,126, 
  95,56,2,128,97,208,1,128,15,1,54,14,2,0,17,1,23,55,2,0,1,15,1,54,14,2,0,17,1,16,52,2,0,1,19,61,0,0,0,183,0,0,0,1,0,17,1,54,14,2,0,1,19,47,0,0,0,153,0,0, 
  0,1,0,17,1,54,14,2,0,1,15,1,54,14,2,0,17,1,158,51,2,0,1,15,1,54,14,2,0,17,1,250,14,2,0,1,19,46,0,0,0,150,0,0,0,3,0,1,19,97,0,0,0,21,1,0,0,1,0,17, 
  1,54,14,2,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,54,14,2,0,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,48,3,130,1,176,3,130,2,176,131,127,94,112,67,130, 
  44,48,2,128,53,184,131,129,6,176,67,127,7,176,3,128,98,176,130,128,105,56,4,128,114,240,2,128,125,112,2,128,126,248,3,128,12,17,1,217,48,2,0,1,12,17,1,176,46,2,0,1,12,17,1,135,44,2,0,1, 
  12,17,1,94,42,2,0,1,6,17,1,58,24,2,0,1,12,17,1,17,22,2,0,1,10,12,17,1,232,19,2,0,1,12,17,1,191,17,2,0,1,12,17,1,150,15,2,0,1,12,19,62,0,0,0,188,0,0,0,1, 
  0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,8,130,1,128,8,130,2,128,136,127,94,80,73,130,44,80,12,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128, 
  114,80,7,128,125,80,10,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0, 
  1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0, 
  0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,47,17,2,0,17,1,45,7,2,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0, 
  178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255, 
  255,8,0,0,0,3,0,0,0,63,16,131,128,81,32,194,128,95,16,4,128,11,144,1,128,97,168,3,128,61,160,3,128,62,160,3,128,47,128,66,126,19,47,0,0,0,152,0,0,0,1,0,17,1,47,17,2,0,1,19, 
  61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,47,17,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,47,17,2,0,1,1,15,1,47,17,2,0,17,1,23,55,2,0, 
  1,15,1,47,17,2,0,17,1,16,52,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,6,130,1,72,9,130,2,72,137,127,94,80,73,130,44,80,12,128,53,232,134,129,6,72,73, 
  127,7,72,9,128,98,96,133,128,105,48,2,128,114,24,8,128,125,80,10,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6, 
  0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0, 
  17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,88,19,2,0,17,1,45,7,2,0,1,4,15,1,30, 
  15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15, 
  1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0, 
  0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119, 
  0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,120,131,128,81,24,195,128,95,32,2,128,11,136,2,128,97,16,4,128,61,8,4,128,62,8,4,128,47,144,65,126,19,97,0,0,0,21, 
  1,0,0,1,0,17,1,88,19,2,0,1,15,1,88,19,2,0,17,1,16,52,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,88,19,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0, 
  153,0,0,0,1,0,17,1,88,19,2,0,1,1,15,1,88,19,2,0,17,1,23,55,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,128,8,130,2,128,136,127,94,136, 
  72,130,44,136,11,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,136,9,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1, 
  0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1, 
  8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0, 
  0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,129,21,2,0,17,1,45,7,2,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,32,194,128,95,128,3,128,11,232,3,128,97,128,2,128,61,120,3,128,62, 
  120,3,128,47,232,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,129,21,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,129,21,2,0,17,1,23,55,2,0,1,19,97,0,0,0,21,1,0,0,1, 
  0,17,1,129,21,2,0,1,1,15,1,129,21,2,0,17,1,16,52,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,129,21,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58, 
  136,11,130,1,128,8,130,2,128,136,127,94,136,72,130,44,80,12,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,136,9,128,126,96,3,128,4,15,1,30,15,0,0,15,1, 
  159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0, 
  0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1, 
  30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0, 
  15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78, 
  0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0, 
  1,0,15,1,170,23,2,0,17,1,45,7,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,80,131,128,81,240,194,128,95,248,1,128, 
  11,224,3,128,97,144,1,128,61,112,4,128,62,112,4,128,47,96,66,126,15,1,170,23,2,0,17,1,23,55,2,0,1,15,1,170,23,2,0,17,1,16,52,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,170, 
  23,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,170,23,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,170,23,2,0,1,1,2,21,1,212,0,0,0, 
  55,147,2,0,19,0,0,0,4,0,0,0,128,224,5,128,1,24,5,131,2,24,133,131,3,112,195,131,4,112,3,129,53,32,5,128,6,24,5,128,7,24,5,128,84,96,6,128,105,32,6,128,94,216,4,128,11,88,4,128, 
  44,48,3,128,97,96,5,128,62,160,5,127,127,152,4,128,98,216,67,128,114,24,4,128,115,240,2,128,12,17,1,12,38,2,0,1,12,17,1,217,48,2,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,12,17,1, 
  135,44,2,0,1,12,17,1,94,42,2,0,1,12,17,1,227,35,2,0,1,12,17,1,186,33,2,0,1,12,17,1,17,22,2,0,1,10,12,17,1,232,19,2,0,1,12,17,1,145,31,2,0,1,12,17,1,104,29,2, 
  0,1,12,17,1,63,27,2,0,1,12,17,1,150,15,2,0,1,12,17,1,22,25,2,0,1,12,17,1,53,40,2,0,1,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,11,130,1,128,8,130, 
  2,128,136,127,94,136,72,130,44,80,12,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,136,9,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82, 
  114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0, 
  0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17, 
  1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0, 
  1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,175,26,2, 
  0,17,1,45,7,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,144,193,128,95,128,3,128,11,232,2,128,97,240,1, 
  128,61,120,3,128,62,120,3,128,47,88,66,126,19,61,0,0,0,185,0,0,0,2,0,1,15,1,175,26,2,0,17,1,23,55,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,175,26,2,0,1,19,47,0,0, 
  0,152,0,0,0,1,0,17,1,175,26,2,0,1,1,15,1,175,26,2,0,17,1,16,52,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,175,26,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0, 
  0,3,0,0,0,58,48,2,130,1,72,9,130,2,72,137,127,94,80,73,130,44,80,12,128,53,232,134,129,6,72,73,127,7,72,9,128,98,40,134,128,105,248,2,128,114,24,8,128,125,80,10,128,126,40,4,128,4,19,35, 
  0,0,0,113,0,0,0,1,0,15,1,216,28,2,0,17,1,45,7,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1, 
  4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82, 
  114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17, 
  1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1, 
  82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0, 
  1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,136, 
  194,128,95,32,2,128,11,232,3,128,97,128,3,128,61,120,3,128,62,120,3,128,47,232,66,126,19,47,0,0,0,153,0,0,0,1,0,17,1,216,28,2,0,1,15,1,216,28,2,0,17,1,16,52,2,0,1,19,61,0, 
  0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,216,28,2,0,1,1,15,1,216,28,2,0,17,1,23,55,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,216,28,2,0,1,2, 
  21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,96,3,130,1,72,9,130,2,72,137,127,94,80,73,130,44,80,12,128,53,232,134,129,6,72,73,127,7,72,9,128,98,40,134,128,105,48,2,128,114,24, 
  8,128,125,80,10,128,126,40,4,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0, 
  15,1,1,31,2,0,17,1,45,7,2,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1, 
  30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0, 
  15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15, 
  1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0, 
  0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8, 
  0,0,0,3,0,0,0,63,248,129,128,81,136,194,128,95,144,1,128,11,128,3,128,97,16,4,128,61,120,3,128,62,120,3,128,47,232,66,126,15,1,1,31,2,0,17,1,16,52,2,0,1,19,47,0,0,0,153,0,0, 
  0,1,0,17,1,1,31,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,1,31,2,0,1,1,19,47,0,0,0,152,0,0,0,1,0,17,1,1,31,2,0,1,15, 
  1,1,31,2,0,17,1,23,55,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,11,130,1,128,8,130,2,128,136,127,94,136,72,130,44,80,12,128,53,32,134,129,6,128,72,127,7, 
  128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,136,9,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0, 
  1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0, 
  17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0, 
  0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,42,33,2,0,17,1,45,7,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0, 
  1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,176,194,128,95,128,3,128,11,32,2,128,97,24,3,128,61,16,3,128,62,16,3,128,47,144,65,126,19,97,0,0,0,21,1,0, 
  0,1,0,17,1,42,33,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,42,33,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,1,15,1,42,33,2,0,17,1,23,55,2,0,1,15,1,42,33,2,0, 
  17,1,16,52,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,42,33,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,128,8,130,1,72,9,130,2,72,137,127,94,80,73,130, 
  44,80,12,128,53,32,134,129,6,72,73,127,7,72,9,128,98,96,133,128,105,48,2,128,114,80,7,128,125,80,10,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0, 
  0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15, 
  1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0, 
  0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19, 
  35,0,0,0,113,0,0,0,1,0,15,1,83,35,2,0,17,1,45,7,2,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0, 
  0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1, 
  4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,240,194,128,95,144,1,128,11,248,1,128,97,136,2,128,61,224,3,128,62,224,3, 
  128,47,80,67,126,15,1,83,35,2,0,17,1,16,52,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,83,35,2,0,1,15,1,83,35,2,0,17,1,23,55,2,0,1,19,61,0,0,0,185,0,0,0,2,0, 
  1,19,97,0,0,0,21,1,0,0,1,0,17,1,83,35,2,0,1,1,19,47,0,0,0,153,0,0,0,1,0,17,1,83,35,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,136,8, 
  130,1,128,8,130,2,128,136,127,94,80,73,130,44,80,12,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,80,10,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116, 
  0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1, 
  0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1, 
  48,8,0,0,17,1,140,7,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,124,37,2,0,17,1,45,7,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0, 
  15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,120,131,128,81,248,193,128,95,144,1,128,11,88, 
  2,128,97,16,4,128,61,8,4,128,62,8,4,128,47,232,66,126,15,1,124,37,2,0,17,1,16,52,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,124,37,2,0, 
  1,19,97,0,0,0,21,1,0,0,1,0,17,1,124,37,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,124,37,2,0,1,1,15,1,124,37,2,0,17,1,23,55,2,0,1,2,21,1,152,1,0,0,112,111, 
  2,0,13,0,0,0,3,0,0,0,58,248,11,130,1,128,8,130,2,128,136,127,94,136,72,130,44,136,11,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,136,9,128,126,96, 
  3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0, 
  0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0, 
  0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15, 
  1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93, 
  0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0, 
  1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,165,39,2,0,17,1,45,7,2,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63, 
  232,130,128,81,248,193,128,95,144,1,128,11,232,3,128,97,128,3,128,61,120,3,128,62,120,3,128,47,88,66,126,15,1,165,39,2,0,17,1,16,52,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0, 
  0,21,1,0,0,1,0,17,1,165,39,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,165,39,2,0,1,1,15,1,165,39,2,0,17,1,23,55,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,165, 
  39,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,80,7,130,1,72,9,130,2,72,137,127,94,80,73,130,44,80,12,128,53,32,134,129,6,72,73,127,7,72,9,128,98,96,133,128,105, 
  48,2,128,114,24,8,128,125,80,10,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12, 
  1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84, 
  0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4, 
  19,35,0,0,0,113,0,0,0,1,0,15,1,206,41,2,0,17,1,45,7,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0, 
  0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59, 
  0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0, 
  255,255,255,255,8,0,0,0,3,0,0,0,63,248,129,128,81,24,195,128,95,144,1,128,11,224,3,128,97,120,3,128,61,112,4,128,62,112,4,128,47,136,66,126,15,1,206,41,2,0,17,1,16,52,2,0,1,19,47,0, 
  0,0,153,0,0,0,1,0,17,1,206,41,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,206,41,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,15,1,206,41,2,0,17,1,23,55,2,0,1,19,47, 
  0,0,0,152,0,0,0,1,0,17,1,206,41,2,0,1,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,32,6,130,1,72,9,130,2,72,137,127,94,80,73,130,44,80,12,128,53,232,134,129, 
  6,72,73,127,7,72,9,128,98,96,133,128,105,48,2,128,114,24,8,128,125,80,10,128,126,96,3,128,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17, 
  1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159, 
  116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,247,43,2,0,17,1,45,7,2,0,1,4, 
  15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114, 
  0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0, 
  19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17, 
  1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,224,131,128,81,136,194,128,95,120,3,128,11,144,1,128,97,32,2,128,61,112,4,128,62,112,4,128,47,232,66,126,19,47,0, 
  0,0,152,0,0,0,1,0,17,1,247,43,2,0,1,15,1,247,43,2,0,17,1,23,55,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,247,43,2,0,1,15,1, 
  247,43,2,0,17,1,16,52,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,247,43,2,0,1,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,72,9,130,2,72,137, 
  127,94,80,73,130,44,80,12,128,53,232,134,129,6,72,73,127,7,72,9,128,98,40,134,128,105,248,2,128,114,24,8,128,125,80,10,128,126,40,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,32,46,2,0,17, 
  1,45,7,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0, 
  0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0, 
  19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159, 
  116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0, 
  1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1, 
  82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,192,130,128,81,96,194,128,95,248,1,128,11,232,3,128,97,144,1,128,61,80, 
  3,128,62,80,3,128,47,88,67,126,15,1,32,46,2,0,17,1,23,55,2,0,1,15,1,32,46,2,0,17,1,16,52,2,0,1,19,61,0,0,0,185,0,0,0,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17, 
  1,32,46,2,0,1,1,19,97,0,0,0,21,1,0,0,1,0,17,1,32,46,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,32,46,2,0,1,2,21,1,152,1,0,0,112,111,2,0,13,0,0,0,3,0, 
  0,0,58,136,11,130,1,128,8,130,2,128,136,127,94,136,72,130,44,80,12,128,53,32,134,129,6,128,72,127,7,128,8,128,98,96,133,128,105,48,2,128,114,80,7,128,125,136,9,128,126,96,3,128,4,15,1,30,15,0, 
  0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0, 
  178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1, 
  4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128,7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70, 
  114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1, 
  0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,35,0,0,0,113, 
  0,0,0,1,0,15,1,73,48,2,0,17,1,45,7,2,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,32,130,128,81,176,194,128,95, 
  16,4,128,11,144,1,128,97,160,3,128,61,8,4,128,62,8,4,128,47,16,67,126,19,47,0,0,0,152,0,0,0,1,0,17,1,73,48,2,0,1,19,47,0,0,0,153,0,0,0,1,0,17,1,73,48,2,0,1,19, 
  61,0,0,0,185,0,0,0,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,73,48,2,0,1,15,1,73,48,2,0,17,1,23,55,2,0,1,1,15,1,73,48,2,0,17,1,16,52,2,0,1,2,21,1,152, 
  1,0,0,112,111,2,0,13,0,0,0,3,0,0,0,58,48,2,130,1,72,9,130,2,72,137,127,94,80,73,130,44,80,12,128,53,232,134,129,6,72,73,127,7,72,9,128,98,40,134,128,105,248,2,128,114,24,8,128,125, 
  80,10,128,126,40,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,114,50,2,0,17,1,45,7,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,116, 
  7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0, 
  0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,62,0,0,0,187,0,0,0,2,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,128, 
  7,0,0,15,1,200,30,1,0,17,1,181,3,0,0,1,4,15,1,30,15,0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,15,1,48,8,0,0,17,1,140,7,0,0,1,8,4,15,1,30,15, 
  0,0,15,1,159,116,0,0,15,1,82,114,0,0,15,1,70,114,0,0,17,1,204,80,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1, 
  0,19,97,0,0,0,22,1,0,0,1,0,15,1,30,15,0,0,15,1,159,116,0,0,17,1,82,114,0,0,1,4,15,1,42,15,0,0,17,1,101,119,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0, 
  3,0,0,0,63,88,130,128,81,248,193,128,95,144,1,128,11,120,3,128,97,8,4,128,61,112,4,128,62,112,4,128,47,232,66,126,15,1,114,50,2,0,17,1,16,52,2,0,1,19,61,0,0,0,185,0,0,0,2,0, 
  1,19,47,0,0,0,153,0,0,0,1,0,17,1,114,50,2,0,1,19,97,0,0,0,21,1,0,0,1,0,17,1,114,50,2,0,1,19,47,0,0,0,152,0,0,0,1,0,17,1,114,50,2,0,1,15,1,114,50,2, 
  0,17,1,23,55,2,0,1,1,2,21,1,143,0,0,0,133,188,2,0,13,0,0,0,3,0,0,0,58,120,3,130,1,112,3,130,2,112,131,127,94,48,67,130,44,48,2,128,53,184,131,129,6,112,67,127,7,112,3,128, 
  98,176,130,128,105,56,4,128,114,240,2,128,125,112,2,128,126,248,3,128,12,17,1,217,48,2,0,1,12,17,1,176,46,2,0,1,12,17,1,135,44,2,0,1,12,17,1,94,42,2,0,1,12,17,1,17,22,2,0,1, 
  10,6,17,1,58,24,2,0,1,12,17,1,232,19,2,0,1,12,17,1,191,17,2,0,1,12,17,1,150,15,2,0,1,12,19,62,0,0,0,188,0,0,0,1,0,1,21,1,47,0,0,0,124,189,2,0,5,0,0,0, 
  2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,10,12,17,1,218,51,2,0,1,12,19,60,0,0,0,182,0,0,0,1,0,1,21,1,53,0,0,0,149,112,2,0,5,0,0,0, 
  2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,15,1,196,74,0,0,17,1,128,33,0,0,1,2,21,1,63,0,0,0,34,190,2,0,5,0,0,0,2,0,0,0,115,112, 
  1,128,1,240,1,128,2,240,129,128,11,176,65,127,118,48,1,128,12,17,1,92,52,2,0,1,12,17,1,92,52,2,0,1,12,17,1,92,52,2,0,1,10,12,19,81,0,0,0,251,0,0,0,1,0,1,21,1,69,0, 
  0,0,239,129,2,0,5,0,0,0,2,0,0,0,115,232,1,128,1,112,1,128,2,112,129,128,11,120,65,127,118,48,1,128,4,17,1,43,54,2,0,1,8,4,15,1,106,53,2,0,17,1,98,75,0,0,1,4,17,1, 
  162,52,2,0,1,2,21,1,57,0,0,0,186,149,2,0,4,0,0,0,2,0,0,0,118,24,1,128,1,16,1,128,2,16,129,127,11,88,1,128,8,4,17,1,31,53,2,0,1,4,15,1,231,52,2,0,17,1,98,75, 
  0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0,0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,247,0,0,0,4,0,1,8,19, 
  81,0,0,0,237,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,74,78,0,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,246,0,0,0,3,0,1,21,1,51,0,0,0,243,150,2,0,4,0,0,0,2,0,0,0,118,80,1,128,1,144,1,128,2,144,129,127,115,16,1,128, 
  4,17,1,243,53,2,0,1,4,17,1,169,53,2,0,1,8,19,81,0,0,0,239,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,249,0,0, 
  0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,21,1,44,0,0,0,92,150,2,0,3,0,0,0,1,0,0, 
  0,2,88,129,128,1,88,1,128,118,240,0,128,4,19,81,0,0,0,250,0,0,0,4,0,1,8,19,81,0,0,0,244,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1, 
  128,63,208,192,127,4,17,1,204,54,2,0,1,4,15,1,130,54,2,0,17,1,98,75,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242, 
  0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,243,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,81,0,0,0,238,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,118,77,0,0,17,1,98,75,0,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,1,51,0,0,0,47,191,2,0,4,0,0,0,2,0,0,0,128,80,1,128,1,144,1, 
  128,2,144,1,128,127,16,1,128,12,17,1,87,55,2,0,1,12,17,1,87,55,2,0,1,10,12,19,95,0,0,0,19,1,0,0,1,0,1,21,1,51,0,0,0,76,134,2,0,4,0,0,0,2,0,0,0,128,80,1, 
  128,1,144,1,128,2,144,1,128,127,16,1,128,4,17,1,140,56,2,0,1,4,17,1,139,55,2,0,1,8,2,21,1,130,0,0,0,188,134,2,0,6,0,0,0,2,0,0,0,114,56,67,129,1,48,195,128,2,48,131, 
  127,51,168,3,128,125,64,2,128,126,80,1,128,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,77,56,2,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0, 
  225,0,0,0,1,0,17,1,77,56,2,0,1,8,4,15,1,14,56,2,0,17,1,140,7,0,0,1,4,19,95,0,0,0,15,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  41,176,0,128,4,19,95,0,0,0,14,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,41,176,0,128,4,19,95,0,0,0,13,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,188,134,2,0,6,0, 
  0,0,2,0,0,0,114,184,65,129,1,24,195,128,2,24,131,127,51,80,1,128,125,32,3,128,126,40,2,128,4,19,95,0,0,0,18,1,0,0,3,0,1,4,15,1,78,57,2,0,17,1,140,7,0,0,1,4,19,94, 
  0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,17,1,15,57,2,0,1,8,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,17,1,15,57,2,0,1,2,21, 
  7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128, 
  8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,17,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  208,0,128,8,2,19,21,0,0,0,65,0,0,0,4,0,1,19,4,0,0,0,8,0,0,0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,6,176,0,128,15,1,165,57,2,0,17,1,201, 
  57,2,0,1,2,21,1,95,0,0,0,86,195,2,0,9,0,0,0,3,0,0,0,24,120,2,128,1,112,194,128,2,112,130,129,23,176,1,128,25,48,2,128,21,184,2,128,6,112,2,128,7,112,2,127,58,240,1,128,12, 
  17,1,48,58,2,0,1,12,17,1,48,58,2,0,1,12,17,1,48,58,2,0,1,10,12,17,1,48,58,2,0,1,12,17,1,48,58,2,0,1,12,17,1,27,47,0,0,1,21,1,147,0,0,0,86,195,2,0,9,0, 
  0,0,3,0,0,0,24,184,3,128,1,144,194,128,2,144,130,129,23,40,4,128,25,32,2,128,21,176,1,128,6,144,2,128,7,144,2,127,58,152,2,128,4,15,1,244,58,2,0,17,1,101,212,1,0,1,4,15,1,232, 
  58,2,0,17,1,212,6,2,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,15,1,220,58,2,0,17,1,91,0,0,0,1,4,15,1,208,58,2,0,17,1,197,108,1,0, 
  1,4,15,1,196,58,2,0,17,1,234,161,1,0,1,2,19,6,0,0,0,17,0,0,0,2,0,1,19,6,0,0,0,18,0,0,0,2,0,1,19,6,0,0,0,20,0,0,0,2,0,1,19,6,0,0,0,19,0,0, 
  0,2,0,1,19,6,0,0,0,16,0,0,0,2,0,1,19,6,0,0,0,14,0,0,0,1,0,1,21,1,93,0,0,0,241,195,2,0,6,0,0,0,2,0,0,0,16,80,1,128,1,224,2,128,2,224,130,128,7,224, 
  2,128,6,224,66,128,18,24,2,128,4,19,41,0,0,0,140,0,0,0,1,0,15,1,31,60,2,0,17,1,106,59,2,0,1,4,19,41,0,0,0,141,0,0,0,1,0,15,1,31,60,2,0,17,1,106,59,2,0,1, 
  8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,15,1,106,59,2,0,17,1,142,59,2,0,1,2,21,1,67,0,0,0,241,195,2,0,6,0,0,0,2,0,0,0,16,152,1,128, 
  1,216,1,128,2,144,129,128,7,144,1,128,6,144,65,128,18,80,1,128,12,17,1,217,59,2,0,1,10,12,17,1,217,59,2,0,1,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,1,69,0,0,0,241, 
  195,2,0,6,0,0,0,2,0,0,0,16,80,1,128,1,32,2,128,2,32,130,128,7,32,2,128,6,32,66,128,18,184,1,128,4,19,41,0,0,0,142,0,0,0,2,0,1,4,19,41,0,0,0,143,0,0,0,2,0, 
  1,8,2,21,1,43,0,0,0,126,196,2,0,4,0,0,0,2,0,0,0,6,16,1,128,1,24,1,128,2,16,129,127,7,16,1,128,8,4,17,1,75,60,2,0,1,2,21,1,59,0,0,0,165,194,2,0,6,0,0, 
  0,2,0,0,0,6,144,65,129,1,144,193,128,2,144,129,127,7,144,1,128,77,152,1,128,78,80,1,128,4,17,1,199,60,2,0,1,8,4,17,1,135,60,2,0,1,2,21,1,63,0,0,0,241,68,2,0,5,0,0, 
  0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,40,0,0,0,138,0,0,0,5,0,1,2,21,1,63,0,0,0,241,68,2, 
  0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,40,0,0,0,139,0,0,0,5,0,1,2,19,17,0,0,0, 
  59,0,0,0,1,0,1,21,1,47,0,0,0,226,172,2,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,11,48,1,128,4,17,1,67,61,2,0,1,8,2,21,1,5,1,0, 
  0,230,176,2,0,9,0,0,0,3,0,0,0,94,128,4,130,1,80,134,128,2,80,134,128,105,80,5,128,114,176,1,128,125,176,2,128,6,80,134,126,7,80,6,128,126,88,6,128,4,15,1,73,62,2,0,15,1,126,62, 
  2,0,15,1,235,63,2,0,15,1,48,8,0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,44,0,0,0, 
  146,0,0,0,1,0,15,1,73,62,2,0,17,1,126,62,2,0,1,4,15,1,73,62,2,0,15,1,126,62,2,0,15,1,235,63,2,0,17,1,204,80,0,0,1,4,15,1,73,62,2,0,15,1,126,62,2,0,15,1, 
  235,63,2,0,15,1,116,7,0,0,17,1,88,6,0,0,1,8,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,44,0,0,0,146,0,0, 
  0,1,0,15,1,73,62,2,0,17,1,126,62,2,0,1,2,21,1,52,0,0,0,193,175,2,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,43,0,0, 
  0,145,0,0,0,4,0,1,8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,15,1,126,62,2,0,17,1,162,62,2,0,1,2,21,1,107,0,0,0,197,196,2,0,10,0,0,0, 
  3,0,0,0,82,208,1,130,1,208,194,128,2,208,130,127,94,16,130,129,105,216,2,128,125,144,2,128,6,208,66,127,7,208,2,128,114,24,3,128,126,80,2,128,12,17,1,27,47,0,0,1,12,17,1,21,63,2,0,1, 
  12,17,1,21,63,2,0,1,12,17,1,21,63,2,0,1,10,12,17,1,21,63,2,0,1,12,17,1,21,63,2,0,1,12,17,1,27,47,0,0,1,21,1,201,0,0,0,230,176,2,0,9,0,0,0,3,0,0,0,94, 
  176,1,130,1,48,132,128,2,48,132,128,105,56,4,128,114,32,2,128,125,192,2,128,6,48,132,126,7,48,4,128,126,216,4,128,4,15,1,223,63,2,0,17,1,204,80,0,0,1,4,15,1,223,63,2,0,15,1,48,8, 
  0,0,17,1,140,7,0,0,1,4,19,93,0,0,0,11,1,0,0,1,0,19,78,0,0,0,225,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,44,0,0,0,147,0,0,0,2,0,1,8,4,15,1,223, 
  63,2,0,15,1,116,7,0,0,17,1,88,6,0,0,1,4,19,94,0,0,0,12,1,0,0,1,0,19,78,0,0,0,226,0,0,0,1,0,19,59,0,0,0,178,0,0,0,1,0,19,44,0,0,0,147,0,0,0,2, 
  0,1,2,19,44,0,0,0,147,0,0,0,2,0,1,19,44,0,0,0,146,0,0,0,1,0,1,21,1,207,0,0,0,86,195,2,0,9,0,0,0,3,0,0,0,24,168,5,128,1,32,196,128,2,32,132,129,23,128,2, 
  128,25,176,1,128,21,80,3,128,6,32,4,128,7,32,4,127,58,40,4,128,4,15,1,199,64,2,0,15,1,165,57,2,0,15,1,0,59,2,0,17,1,212,6,2,0,1,4,15,1,199,64,2,0,15,1,165,57,2,0, 
  15,1,77,212,1,0,17,1,234,161,1,0,1,4,15,1,199,64,2,0,15,1,165,57,2,0,15,1,200,6,2,0,17,1,101,212,1,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0, 
  0,1,0,15,1,199,64,2,0,15,1,165,57,2,0,15,1,89,212,1,0,17,1,91,0,0,0,1,4,15,1,199,64,2,0,15,1,165,57,2,0,15,1,40,159,1,0,17,1,197,108,1,0,1,2,19,4,0,0,0, 
  7,0,0,0,2,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,5,176,0,128,15,1,211,64,2,0,17,1,247,64,2,0,1,2,21,1,143,0,0,0,102,179,2,0,13,0,0,0,3,0,0, 
  0,24,240,194,130,1,176,195,129,2,176,195,129,83,112,2,128,23,184,131,129,21,112,3,128,6,176,3,128,7,176,67,127,25,56,4,129,58,248,3,128,79,48,3,128,80,48,2,128,81,176,2,128,12,17,1,142,65,2,0, 
  1,12,17,1,142,65,2,0,1,12,17,1,142,65,2,0,1,12,17,1,27,47,0,0,1,12,17,1,142,65,2,0,1,12,17,1,27,47,0,0,1,10,12,17,1,27,47,0,0,1,12,17,1,27,47,0,0,1,12,17, 
  1,27,47,0,0,1,12,17,1,27,47,0,0,1,21,1,131,0,0,0,71,197,2,0,8,0,0,0,3,0,0,0,80,216,2,128,1,48,2,129,2,48,2,128,83,120,3,128,79,56,2,128,81,144,1,128,6,48,2,128, 
  7,48,66,127,4,15,1,18,66,2,0,15,1,116,159,1,0,17,1,52,159,1,0,1,8,4,15,1,18,66,2,0,15,1,222,161,1,0,17,1,128,159,1,0,1,4,15,1,18,66,2,0,15,1,7,61,2,0,17,1, 
  12,59,2,0,1,4,15,1,18,66,2,0,15,1,42,66,2,0,17,1,19,61,2,0,1,2,19,5,0,0,0,10,0,0,0,2,0,1,19,5,0,0,0,9,0,0,0,1,0,1,19,17,0,0,0,61,0,0,0,1, 
  0,1,15,1,199,103,1,0,17,1,67,66,2,0,1,21,1,64,0,0,0,201,199,2,0,2,0,0,0,1,0,0,0,28,208,0,128,27,104,1,128,4,19,23,0,0,0,67,0,0,0,1,0,17,1,168,66,2,0,1, 
  4,19,22,0,0,0,66,0,0,0,1,0,17,1,132,66,2,0,1,2,21,1,35,0,0,0,53,206,2,0,1,0,0,0,0,0,0,0,8,176,0,128,4,19,7,0,0,0,21,0,0,0,2,0,1,2,21,1,35,0, 
  0,0,53,206,2,0,1,0,0,0,0,0,0,0,8,176,0,128,4,19,7,0,0,0,22,0,0,0,2,0,1,2,15,1,199,103,1,0,17,1,217,66,2,0,1,21,1,208,1,0,0,252,120,2,0,23,0,0,0,4, 
  0,0,0,32,112,202,132,1,248,201,131,2,248,201,131,35,24,9,128,36,56,8,128,37,0,10,128,38,16,142,131,39,152,134,131,40,168,8,128,41,8,7,128,42,112,3,128,43,120,7,128,44,40,6,128,29,160,4,128,46, 
  48,13,128,31,80,203,128,33,160,13,128,34,48,68,129,47,136,9,128,48,224,10,128,54,192,12,128,87,16,5,128,98,192,11,128,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,1,4, 
  15,1,184,74,0,0,17,1,247,51,0,0,1,4,15,1,84,55,0,0,17,1,253,54,0,0,1,4,19,49,0,0,0,155,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1, 
  0,1,4,15,1,43,62,0,0,17,1,96,55,0,0,1,4,15,1,184,74,0,0,17,1,166,54,0,0,1,4,15,1,165,52,0,0,17,1,78,52,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0, 
  0,26,0,0,0,1,0,1,4,15,1,184,74,0,0,17,1,73,74,0,0,1,4,15,1,184,74,0,0,17,1,155,73,0,0,1,4,15,1,184,74,0,0,17,1,242,73,0,0,1,4,15,1,50,68,0,0,17,1,219, 
  67,0,0,1,8,4,15,1,184,74,0,0,17,1,199,50,0,0,1,4,15,1,184,74,0,0,17,1,79,54,0,0,1,4,15,1,50,68,0,0,17,1,177,52,0,0,1,4,15,1,184,74,0,0,17,1,229,62,0,0, 
  1,4,15,1,144,63,0,0,15,1,156,63,0,0,15,1,71,67,0,0,15,1,83,67,0,0,17,1,60,63,0,0,1,4,15,1,143,73,0,0,17,1,62,68,0,0,1,4,15,1,50,68,0,0,17,1,55,62,0,0, 
  1,4,15,1,184,74,0,0,17,1,142,62,0,0,1,4,15,1,184,74,0,0,17,1,248,53,0,0,1,2,15,1,199,103,1,0,17,1,183,68,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,123,176,0,128,4,17,1,13,44,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47, 
  48,129,128,45,112,193,127,95,240,0,128,3,17,1,162,80,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65, 
  128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3, 
  15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17, 
  1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,55,70,2,0,1, 
  3,15,1,225,70,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,140,70,2,0,1,15,1,225,70,2,0,17, 
  1,140,70,2,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,16,193,127,3,17,1,55,70,2,0,1,3,17,1,55,70,2,0,1,21,2,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,55,70,2,0,1,3,17,1,55,70,2,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,16,193,127,3,17, 
  1,55,70,2,0,1,3,17,1,55,70,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,55,70,2,0,1,3,17,1,55,70,2,0,1,2,1,21,4, 
  54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,55,70,2,0,1,3,15,1,225,70,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,140,70,2,0,1,15,1,225,70,2,0,17,1,140,70,2,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,16,193,127,3,17,1,226,70,2,0,1,3,17,1,204,69,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,13,72,2,0, 
  1,3,17,1,162,71,2,0,1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,55,70,2,0,1,3,15,1,225,70,2,0,17,1,55, 
  70,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,140,70,2,0,1,15,1,225,70,2,0,17,1,140,70,2,0,1,1,21,4,54, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,55,70,2,0,1,3,15,1,225,70,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,140,70,2,0,1,15,1,225,70,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,1,21,4,42,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,111,78,2,0,1,3,17,1,169,72,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,47,73, 
  2,0,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,80,78,2,0,17,1,134,75,2,0,1,15,1,80,78,2,0, 
  17,1,134,75,2,0,1,15,1,80,78,2,0,17,1,134,75,2,0,1,15,1,80,78,2,0,17,1,134,75,2,0,1,15,1,80,78,2,0,17,1,134,75,2,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,229,73,2,0,1,3,17,1,199,73,2,0,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3, 
  104,2,128,6,48,1,128,15,1,97,75,2,0,17,1,172,74,2,0,1,15,1,97,75,2,0,17,1,172,74,2,0,1,15,1,97,75,2,0,17,1,172,74,2,0,1,15,1,97,75,2,0,17,1,172,74,2,0,1,15, 
  1,97,75,2,0,17,1,172,74,2,0,1,17,1,80,78,2,0,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,18,7,0,0,0,1,2,18,7,0,0,0,21,2,78,0,0,0, 
  255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,57,74,2,0,1,3,17,1,57,74,2,0,1,3,17,1,57,74,2,0,1,3,17,1,57, 
  74,2,0,1,3,17,1,57,74,2,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,199,73,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0, 
  0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,57,74,2,0,1,3,17,1,57,74,2,0,1,3,17,1,57,74,2,0,1,3,17,1,57,74,2,0,1,3,17,1,57,74,2,0, 
  1,17,1,80,78,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,251,74,2,0,1,3,17,1,251,74,2, 
  0,1,3,17,1,251,74,2,0,1,3,17,1,251,74,2,0,1,3,17,1,251,74,2,0,1,2,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,78,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,251,74,2,0,1,3,17,1,251,74,2,0,1,3,17,1,251,74,2,0,1,3,17,1,251,74,2,0,1, 
  3,17,1,251,74,2,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,199,73,2,0,1,17,1,80,78,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0, 
  2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,209,77,2,0,1,3,17,1,82,77,2,0,1,3,17,1,211,76,2,0,1,3,17,1,84,76,2,0,1,3,17,1,213, 
  75,2,0,1,2,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104, 
  2,128,6,48,1,128,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1, 
  225,70,2,0,17,1,172,74,2,0,1,1,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1, 
  128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1, 
  172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,1,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15, 
  1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,1,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,103,0,0,0,255,255,255,255,5, 
  0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17, 
  1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,1,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,103,0, 
  0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1, 
  15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,15,1,225,70,2,0,17,1,172,74,2,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176, 
  0,128,3,17,1,199,73,2,0,1,2,21,2,101,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,216,1,128,5,160,1,128,2,184,130,128,3,72,2,128,6,48,1,128,3,15,1,132,80,2,0,17,1,41,80, 
  2,0,1,3,18,6,0,0,0,1,3,15,1,132,80,2,0,17,1,206,79,2,0,1,3,15,1,132,80,2,0,17,1,115,79,2,0,1,3,15,1,132,80,2,0,17,1,213,78,2,0,1,2,21,2,90,0,0,0,255, 
  255,255,255,4,0,0,0,2,0,0,0,4,240,1,128,6,128,1,128,2,16,193,127,3,96,2,128,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70, 
  2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,144,1,128,6,16,1,128,2,208,193,127,3,80,1,128,3,17, 
  1,48,79,2,0,1,3,17,1,48,79,2,0,1,3,17,1,48,79,2,0,1,3,17,1,48,79,2,0,1,1,21,2,90,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,128,1,128,6,96,2,128,2,16,193, 
  127,3,240,1,128,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0, 
  1,1,21,2,90,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,240,1,128,6,96,2,128,2,128,193,127,3,16,1,128,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48, 
  79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,1,21,2,90,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,128,1,128,6,16,1,128,2, 
  96,194,127,3,240,1,128,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79,2,0,1,3,15,1,225,70,2,0,17,1,48,79, 
  2,0,1,1,21,2,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,5,176,0,128,3,18,6,0,0,0,1,2,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0, 
  128,45,64,193,127,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15, 
  1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1, 
  120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77, 
  71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,61,208,0,128,47,16,193,127,3,17,1,213,81,2,0,1,3,17,1,126,72,2,0,1, 
  21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62, 
  208,0,128,33,8,1,128,3,18,3,0,0,0,1,3,17,1,255,81,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,4,0,0,0,1,2,21,4,114,0,0,0,255, 
  255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,32,87,2,0,1,3,17,1,61,86,2,0,1,3, 
  17,1,133,84,2,0,1,3,17,1,10,84,2,0,1,3,17,1,174,83,2,0,1,3,17,1,82,83,2,0,1,3,17,1,184,82,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,215,82,2,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,246,82,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,21,83,2,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,17,1,52,83,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,93,0,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,113,83,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,144,83, 
  2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,18,91,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,205, 
  83,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,236,83,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,18, 
  104,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,41,84,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3, 
  17,1,72,84,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,103,84,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0, 
  128,3,18,99,0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,101,144,129,128,97,208,193,127,117,16,1,128,111,80,1,128,3,17,1,0,86,2,0,1,3,17,1,226,85,2,0,1,3, 
  17,1,5,85,2,0,1,3,17,1,200,84,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,231,84,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,115,176,0,128,3,18,92,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,36,85,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,107,176,0,128,3,17,1,67,85,2,0,1,2,18,71,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,103,85,2,0,1,1,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,134,85,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,165,85,2,0,1,2,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,196,85,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,76,0,0,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,70,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,31,86,2,0,1,2,21, 
  4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,103,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,92,86,2,0,1,2, 
  21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,100,16,1,128,115,208,0,128,3,17,1,227,86,2,0,1,3,17,1,135,86,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,117,176,0,128,3,17,1,166,86,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,197,86,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,101,176,0,128,3,18,63,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,2,87,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,116,176,0,128,3,18,74,0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,104,80,1,128,101,144,1,128,107,16,1,128,99,208,193,127,3,17,1,127,89,2,0,1,3,17, 
  1,35,89,2,0,1,3,17,1,160,87,2,0,1,3,17,1,99,87,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,130,87,2,0,1,2,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,72,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,191,87,2,0,1,2,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,222,87,2,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,65,128,116,208,0,128,3,17,1,101,88,2,0,1, 
  3,17,1,9,88,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,40,88,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176, 
  0,128,3,17,1,71,88,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,66,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111, 
  176,0,128,3,17,1,132,88,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,163,88,2,0,1,2,18,64,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,45,176,0,128,3,17,1,199,88,2,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,230,88,2,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,5,89,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,67,0,0,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,66,89,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,97,89,2,0,1,2,21,4,29,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,75,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,158,89,2,0,1,2,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,73,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,126,72,2,0,1,21,2,57,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,2,21,2,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,51,90,2,0,1,2,21,2,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,15,1,225,70,2,0,17,1,88,90,2,0,1,1, 
  21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,88,90,2,0,1,1,18,87,0,0,0,1,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,208,1,128,45, 
  144,1,128,95,16,1,128,47,80,193,127,3,17,1,162,80,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,124,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,40,3,128,47,168,66, 
  129,34,160,3,128,91,112,66,129,116,176,1,128,45,232,2,128,95,48,2,128,39,96,131,126,99,240,1,128,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,18,44,0,0,0,1, 
  3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0, 
  128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15, 
  1,41,98,2,0,15,1,44,94,2,0,17,1,232,93,2,0,1,3,17,1,66,92,2,0,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128, 
  6,48,1,128,15,1,41,98,2,0,17,1,195,94,2,0,1,15,1,41,98,2,0,17,1,195,94,2,0,1,15,1,41,98,2,0,17,1,195,94,2,0,1,15,1,41,98,2,0,17,1,195,94,2,0,1,15,1,41,98, 
  2,0,17,1,195,94,2,0,1,2,18,125,0,0,0,21,4,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,3,15,1,196,93,2,0,15,1,217,92,2,0,17,1,232,93,2,0,1,21,2,103, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,196,93,2,0,17,1,81,93,2,0,1,15,1,196,93,2,0,17,1,81,93,2,0, 
  1,15,1,196,93,2,0,17,1,81,93,2,0,1,15,1,196,93,2,0,17,1,81,93,2,0,1,15,1,196,93,2,0,17,1,81,93,2,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34, 
  64,65,128,92,208,0,128,3,15,1,217,92,2,0,17,1,232,93,2,0,1,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128, 
  3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,1,21,4,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92, 
  176,0,128,3,15,1,217,92,2,0,17,1,232,93,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,217,92, 
  2,0,1,3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,3,17,1,217,92,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,34,176,0,128,3,18, 
  125,0,0,0,1,17,1,41,98,2,0,1,21,2,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,80,1,128,6,16,1,128,2,208,193,127,3,144,1,128,3,17,1,43,94,2,0,1,3,17,1,43,94,2, 
  0,1,3,17,1,43,94,2,0,1,3,17,1,43,94,2,0,1,2,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,225,70,2,0,15,1,217,92,2,0,17, 
  1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1, 
  225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,1,21,4,36,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,92,176,0,128,3,15,1,44,94,2,0,17,1,232,93,2,0,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1, 
  128,6,48,1,128,3,17,1,146,97,2,0,1,3,17,1,251,96,2,0,1,3,17,1,100,96,2,0,1,3,17,1,205,95,2,0,1,3,17,1,54,95,2,0,1,2,21,4,47,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,225,70,2,0,15,1,217,92,2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2, 
  208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93, 
  2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,225,70,2,0,15,1,217,92,2,0,17,1,232,93, 
  2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2, 
  0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,1,21,4,47,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,225,70,2,0,15,1,217,92,2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1, 
  128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1, 
  81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,225,70,2,0,15,1,217,92,2,0,17,1, 
  232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225, 
  70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,1,21,4,47,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,225,70,2,0,15,1,217,92,2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5, 
  152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,15,1,225,70,2,0, 
  17,1,81,93,2,0,1,15,1,225,70,2,0,17,1,81,93,2,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,34,176,0,128,3,18,125,0,0,0,1,2,21,4,54,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,158,104,2,0,15,1,161,100,2,0,17,1,232,93,2,0,1,3,17,1,229,98,2,0,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2, 
  0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,158,104,2,0,17,1,56,101,2,0,1,15,1,158,104,2,0,17,1,56,101,2,0,1,15,1,158,104,2,0,17,1,56,101,2, 
  0,1,15,1,158,104,2,0,17,1,56,101,2,0,1,15,1,158,104,2,0,17,1,56,101,2,0,1,2,18,126,0,0,0,21,4,53,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128, 
  3,15,1,125,100,2,0,15,1,146,99,2,0,17,1,232,93,2,0,1,3,18,126,0,0,0,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2, 
  128,6,48,1,128,15,1,125,100,2,0,17,1,10,100,2,0,1,15,1,125,100,2,0,17,1,10,100,2,0,1,15,1,125,100,2,0,17,1,10,100,2,0,1,15,1,125,100,2,0,17,1,10,100,2,0,1,15,1,125, 
  100,2,0,17,1,10,100,2,0,1,18,126,0,0,0,17,1,158,104,2,0,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,64,1,128,3,15,1,146,99,2,0,17,1,232,93,2, 
  0,1,1,21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,146,99,2,0,1,3,17,1,146,99,2,0,1,3,17,1, 
  146,99,2,0,1,3,17,1,146,99,2,0,1,3,17,1,146,99,2,0,1,1,21,4,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,3,15,1,146,99,2,0,17,1,232,93,2,0,1,21,2, 
  78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,146,99,2,0,1,3,17,1,146,99,2,0,1,3,17,1,146,99,2,0,1, 
  3,17,1,146,99,2,0,1,3,17,1,146,99,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,39,176,0,128,3,18,126,0,0,0,1,17,1,158,104,2,0,1,21,4,47,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,225,70,2,0,15,1,146,99,2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0, 
  2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225, 
  70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,1,21,4,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,3,15,1,161,100,2,0,17,1,232,93,2,0,1, 
  21,2,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,176,1,128,5,112,1,128,2,48,130,128,3,240,1,128,6,48,1,128,3,17,1,7,104,2,0,1,3,17,1,112,103,2,0,1,3,17,1,217,102,2, 
  0,1,3,17,1,66,102,2,0,1,3,17,1,171,101,2,0,1,2,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,225,70,2,0,15,1,146,99,2,0,17,1, 
  232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225, 
  70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,1,21,4,47,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,225,70,2,0,15,1,146,99,2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5, 
  152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0, 
  17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,225,70,2,0,15,1,146,99,2,0, 
  17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,10,100,2,0,1,15, 
  1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,1,21,4,47,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,225,70,2,0,15,1,146,99,2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2, 
  128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70, 
  2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,225,70,2,0,15,1,146,99, 
  2,0,17,1,232,93,2,0,1,1,21,2,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,0,2,128,5,152,1,128,2,208,130,128,3,104,2,128,6,48,1,128,15,1,225,70,2,0,17,1,10,100,2,0, 
  1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,15,1,225,70,2,0,17,1,10,100,2,0,1,1,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,39,176,0,128,3,18,126,0,0,0,1,2,18,58,0,0,0,21,4,65,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,96,1,128,45,152,65,128,95,240,0,128,3, 
  15,1,120,72,2,0,17,1,226,70,2,0,1,3,18,114,0,0,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128, 
  15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,160,193,127,107,240, 
  0,128,3,17,1,179,105,2,0,1,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128, 
  3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,96,1,128,45,160, 
  65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,17,1,47,106,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,105,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176, 
  0,128,3,18,94,0,0,0,1,1,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,112,1,128,45,48,65,128,95,240,0,128,3,17,1,162,80,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0, 
  0,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,3,107,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3, 
  18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,18,97,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45, 
  16,193,127,3,17,1,162,80,2,0,1,3,17,1,92,69,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0, 
  0,0,1,3,17,1,18,81,2,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,104,80,66,129,97,144,194,128,110,208,1,128,115,144,1,128,105,16,2,128,116,80,1,128,3,17,1,74,109,2, 
  0,1,3,17,1,2,109,2,0,1,3,17,1,186,108,2,0,1,3,17,1,156,108,2,0,1,3,17,1,64,108,2,0,1,3,17,1,3,108,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,34,108,2,0,1,2,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128,3,18,112,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,95,108,2,0,1,2,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,126,108,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,18,113,0,0,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,18,106,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,1,128,117,208,0,128,3,17,1,228,108,2,0,1, 
  3,18,107,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,110,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,16,1,128, 
  121,208,0,128,3,17,1,44,109,2,0,1,3,18,108,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,111,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,105,109,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,18,109,0,0,0,1,2,21,4,52,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,124,240,0,128,41,104,65,128,47,40,1,128,3,18,84,0,0,0,1,3,17,1,126,72,2,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,124,208,0,128,47,8,1,128,3,18,84,0,0,0,1,3,17,1, 
  126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,169,0,0,0,255,255,255,255,13,0,0,0, 
  3,0,0,0,40,88,4,128,41,32,4,128,34,8,133,129,91,40,195,129,36,208,196,129,45,224,3,128,47,160,195,128,39,144,196,127,58,96,3,128,95,232,2,128,99,168,2,128,116,104,66,128,124,48,2,128,3,18,84,0, 
  0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,18,44,0,0,0,1,3,17,1,20,111,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18, 
  85,0,0,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65, 
  128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,51,111,2,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,82,111,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,62,0,0,0,1,2, 
  21,4,135,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,72,3,128,47,200,66,129,34,248,3,128,91,144,66,129,36,192,67,129,45,8,3,128,95,80,2,128,39,128,131,126,99,16,2,128,116,208,1,128,3,17, 
  1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,18,44,0,0,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1, 
  3,18,98,0,0,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3, 
  17,1,18,81,2,0,1,2,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,16,1,128,41,200,1,128,58,72,1,128,47,136,1,128,3,18,84,0,0,0,1,3,17,1,20,111,2,0,1,3,17,1, 
  126,72,2,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,47,16,1,128,3,17,1,20,111,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1, 
  128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,147,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,115,168,130,130,105,40,3,128,98,168,3,128,91,232,67,127,36,96,4,129,109,232,2,129,102,104, 
  3,128,47,32,4,128,116,104,2,128,117,40,2,128,123,240,1,128,3,18,11,0,0,0,1,3,17,1,202,116,2,0,1,3,17,1,246,115,2,0,1,3,17,1,185,115,2,0,1,3,17,1,124,115,2,0,1,3,17,1, 
  224,114,2,0,1,3,17,1,17,114,2,0,1,3,17,1,181,113,2,0,1,3,18,44,0,0,0,1,3,17,1,126,72,2,0,1,3,18,98,0,0,0,1,21,2,57,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111, 
  176,0,128,3,17,1,212,113,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,243,113,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,108,176,0,128,3,18,41,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,54,48,1,128,51,112,65,128,97,240,0,128,3,17,1,132,114,2,0,1,3,17,1,102,114,2,0,1,3, 
  17,1,72,114,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,33,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128, 
  3,18,36,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,163,114,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0, 
  128,3,17,1,194,114,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,43,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56,16, 
  1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,31,0,0,0,1,3,17,1,94,115,2,0,1,3,17,1,64,115,2,0,1,3,17,1,34,115,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,54,176,0,128,3,18,39,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,34,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,52,176,0,128,3,18,37,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,155,115,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,112,176,0,128,3,18,54,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,216,115,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,114,176,0,128,3,18,29,0,0,0,1,2,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,240,0,128,107,112,65,128,111,48,1,128,3,17,1,141,116,2,0,1,3,17,1,44, 
  116,2,0,1,3,18,46,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,75,116,2,0,1,2,18,47,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,101,176,0,128,3,17,1,111,116,2,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,48,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,117,176,0,128,3,17,1,172,116,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,42,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,56,16,1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,32,0,0,0,1,3,17,1,72,117,2,0,1,3,17,1,42,117,2,0,1,3,17,1,12,117,2,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,3,18,40,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,35,0,0,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,38,0,0,0,1,2,21,4,135,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,115,136,2,128,105,8,3,128,98,136,3,128,91,200,67,127, 
  36,0,196,128,109,200,194,128,102,72,3,128,116,72,2,128,117,8,66,128,125,208,1,128,3,18,9,0,0,0,1,3,17,1,202,116,2,0,1,3,17,1,39,118,2,0,1,3,17,1,185,115,2,0,1,3,17,1,124,115, 
  2,0,1,3,17,1,224,114,2,0,1,3,17,1,17,114,2,0,1,3,17,1,181,113,2,0,1,3,18,44,0,0,0,1,3,18,98,0,0,0,1,21,2,57,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4, 
  96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,2,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,144,65, 
  128,111,80,1,128,114,16,1,128,95,200,65,127,3,17,1,141,116,2,0,1,3,17,1,44,116,2,0,1,3,18,46,0,0,0,1,3,18,57,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,116,240,0,128,45,112,65,128,95,48,1,128,3,17,1,211,118,2,0,1,3,17,1,162,80,2,0,1,3,17,1,92,69,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128, 
  3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,18,58,0,0,0,21,4,77,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,80,129,128,45,248, 
  1,128,111,16,1,128,95,136,65,127,3,17,1,90,119,2,0,1,3,18,46,0,0,0,1,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,95,48,129,128,45,160,193,127,107,240,0,128,3,17,1,75,116,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,21,4,135,0,0,0,255,255,255,255, 
  10,0,0,0,3,0,0,0,115,136,66,130,105,8,3,128,98,136,3,128,91,200,67,127,36,0,196,128,109,200,194,128,102,72,3,128,116,72,2,128,117,8,2,128,123,208,1,128,3,18,11,0,0,0,1,3,17,1,202,116, 
  2,0,1,3,17,1,246,115,2,0,1,3,17,1,185,115,2,0,1,3,17,1,124,115,2,0,1,3,17,1,224,114,2,0,1,3,17,1,17,114,2,0,1,3,17,1,181,113,2,0,1,3,18,44,0,0,0,1,3,18, 
  98,0,0,0,1,21,2,57,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,222,120,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3, 
  18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,18,57,0,0,0,1,2,21,4,124,0,0,0,255,255,255,255,9,0,0,0,3,0, 
  0,0,115,48,2,128,105,176,2,128,98,48,3,128,91,112,67,127,36,168,195,128,109,112,194,128,102,240,2,128,116,240,1,128,117,176,1,128,3,17,1,202,116,2,0,1,3,17,1,246,115,2,0,1,3,17,1,185,115,2, 
  0,1,3,17,1,124,115,2,0,1,3,17,1,224,114,2,0,1,3,17,1,17,114,2,0,1,3,17,1,181,113,2,0,1,3,18,44,0,0,0,1,3,18,98,0,0,0,1,21,2,57,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,36,16,65,128,116,208,0,128,3,17,1,4,122,2,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1, 
  3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,111,208,0,128,107,16,193,127,3,17,1,44,116,2,0,1,3,18,46,0,0,0,1,2,21,2,58,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,119,90,2,0,17,1,51,90,2,0,1,2,21,4,135,0,0,0,255,255,255,255,10, 
  0,0,0,3,0,0,0,109,144,66,130,105,208,2,128,98,80,3,128,91,200,3,129,36,0,4,129,93,144,195,126,102,16,3,128,115,80,2,128,116,16,2,128,117,208,1,128,3,17,1,202,116,2,0,1,3,17,1,246,115, 
  2,0,1,3,17,1,185,115,2,0,1,3,17,1,124,115,2,0,1,3,17,1,224,114,2,0,1,3,17,1,17,114,2,0,1,3,17,1,181,113,2,0,1,3,18,45,0,0,0,1,3,18,44,0,0,0,1,3,18,98, 
  0,0,0,1,21,2,57,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0, 
  1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,16,193,127,3,17,1,162,80,2,0,1,3,17,1,92,69,2,0,1,21,2,69,0,0,0,255,255,255,255,4,0,0,0,2,0, 
  0,0,4,128,1,128,5,72,1,128,6,16,1,128,3,232,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,3,17,1,18,81,2,0,1,2,21,4,168,0,0,0, 
  255,255,255,255,13,0,0,0,3,0,0,0,43,136,132,129,41,200,4,128,58,208,3,128,35,0,69,127,44,80,4,129,45,16,4,129,59,152,3,128,95,224,2,128,60,88,131,128,93,32,195,128,116,160,66,128,124,104,2,128, 
  125,48,2,128,3,18,82,0,0,0,1,3,18,84,0,0,0,1,3,17,1,244,124,2,0,1,3,17,1,162,80,2,0,1,3,18,45,0,0,0,1,3,17,1,214,124,2,0,1,3,18,90,0,0,0,1,3,17,1,184, 
  124,2,0,1,3,17,1,92,69,2,0,1,3,18,10,0,0,0,1,3,17,1,149,124,2,0,1,3,18,85,0,0,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,23, 
  0,0,0,1,2,18,50,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,25,0,0,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176, 
  0,128,3,18,24,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,21,0,0,0,1,2,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,104,240,0,128,45,160,65,128,95,48,1,128,3,17,1,112,125,2,0,1,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,95,48,129,128,45,160,193,127,101,240,0,128,3,17,1,236,125,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,110,240,0,128,45,104,65,128,95,40,1,128,3,18,13,0,0,0,1,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,16,193,127, 
  3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,21, 
  4,191,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,8,5,128,41,208,4,128,34,184,133,129,91,224,3,130,36,128,5,130,45,144,4,128,94,160,3,128,39,64,133,128,58,80,4,128,63,24,68,128,95,96,3, 
  128,99,32,131,128,116,224,130,128,123,168,2,128,124,112,2,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127, 
  2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,20,111,2,0,1,3,17,1,92,69,2,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0, 
  0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2, 
  0,1,2,21,4,66,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,112,193,127,3,15,1,233,129,2,0,15,1,225,70,2,0,17,1,112,128,2,0,1,3,15,1,233,129,2,0,15,1,225,70, 
  2,0,17,1,5,128,2,0,1,21,2,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,15,1,233,129,2,0,17,1,219,128,2,0,1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,226,70,2,0,1,3,15,1,225,70,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0, 
  128,3,56,1,128,15,1,225,70,2,0,17,1,77,71,2,0,1,15,1,225,70,2,0,17,1,77,71,2,0,1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15, 
  1,225,70,2,0,17,1,226,70,2,0,1,3,15,1,225,70,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1, 
  77,71,2,0,1,15,1,225,70,2,0,17,1,77,71,2,0,1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,112,128,2,0,1,3, 
  15,1,225,70,2,0,17,1,5,128,2,0,1,21,2,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,15,1,225,70,2,0,17,1,53,129,2,0,1,2,21,4,42,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,95,208,0,128,45,16,193,127,3,17,1,112,128,2,0,1,3,17,1,5,128,2,0,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,17,1,126,129,2,0, 
  1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,225,70,2,0,17,1,226,70,2,0,1,3,15,1,225,70,2,0,17,1,204,69,2,0,1,21,2,52,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,77,71,2,0,1,15,1,225,70,2,0,17,1,77,71,2,0,1,1,18,118,0,0,0,1,21,4,52,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,94,40,1,128,63,104,65,128,123,240,0,128,3,18,11,0,0,0,1,3,17,1,159,127,2,0,1,3,18,115,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18, 
  1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,203,130,2,0,17,1,135,130,2,0,1,2,21,2,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,15,1,225,70,2,0,17,1,172,130, 
  2,0,1,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17,1,172,130,2,0,1,1,18,129,0,0,0,1,21,4,132,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40, 
  176,3,128,41,120,3,128,58,56,3,128,91,200,130,128,36,232,3,129,99,72,2,128,94,136,2,128,63,0,3,128,116,8,66,128,124,208,1,128,3,18,84,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2, 
  0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,20,111,2,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,18,114,0, 
  0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,187,131,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,17,1, 
  47,106,2,0,1,2,21,4,121,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,88,3,128,41,32,3,128,58,224,2,128,91,168,130,128,36,144,195,128,99,40,2,128,94,104,2,128,116,232,65,128,124,176,1,128, 
  3,18,84,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,17,1,20,111,2,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1, 
  3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,132,0,0,0,255,255,255,255,10,0, 
  0,0,3,0,0,0,40,176,3,128,41,120,3,128,58,56,3,128,91,0,131,128,36,232,195,128,99,128,194,128,94,192,2,128,116,64,130,128,123,8,2,128,124,208,1,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1, 
  3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,17,1,20,111,2,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1, 
  21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,192,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40, 
  8,5,128,41,208,4,128,34,192,133,129,91,224,3,130,36,136,5,130,45,144,4,128,94,160,3,128,39,72,133,128,58,80,4,128,63,24,68,128,95,96,3,128,99,32,131,128,116,224,130,128,123,168,2,128,124,112,2,128,3, 
  18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3, 
  17,1,20,111,2,0,1,3,17,1,92,69,2,0,1,3,18,85,0,0,0,1,3,17,1,30,134,2,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,18,53,0,0,0,21,4,40,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,42,8,1,128,43,208,0,128,3,18,128,0,0,0,1,3,18,127,0,0,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,3,17,1,147,134,2, 
  0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,42,8,1,128,43,208,0,128,3,18,128,0,0,0,1,3,18,127,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,99,16,1,128,41,80,1,128,34,200,1,128,39,136,65,127,3,17, 
  1,126,131,2,0,1,3,18,51,0,0,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0, 
  0,1,3,18,2,0,0,0,1,2,21,4,132,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,168,3,128,41,112,3,128,58,48,3,128,91,192,130,128,36,232,131,128,99,128,194,128,116,64,194,128,63,248,2,128, 
  123,8,2,128,124,208,1,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,211,135,2,0,1, 
  3,18,85,0,0,0,1,3,17,1,30,134,2,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0, 
  0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17,1,51,111,2,0,1,3,18,97,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,47,208,0,128,41,16,193,127,3,17,1,126,72,2,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3, 
  18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,94,208,0,128,63,16,1,128,3,17,1,159,127,2,0,1,3,18,115,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,94,176,0,128,3,17,1,159,127,2,0,1,21, 
  2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,94,8, 
  1,128,123,208,0,128,3,18,11,0,0,0,1,3,17,1,159,127,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0, 
  1,2,21,4,113,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,208,2,128,47,80,66,129,34,72,3,128,99,208,1,128,116,144,1,128,45,144,2,128,95,16,2,128,39,8,131,126,3,17,1,55,105,2,0,1, 
  3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,99,0,0,0,255,255,255,255,7,0, 
  0,0,2,0,0,0,40,216,66,129,93,40,2,128,58,152,2,128,63,96,66,128,99,232,129,128,116,168,1,128,123,112,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,18,120, 
  0,0,0,1,3,18,115,0,0,0,1,3,17,1,3,107,2,0,1,3,17,1,30,134,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1, 
  3,18,2,0,0,0,1,2,21,4,124,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,40,3,128,47,168,194,129,34,160,3,128,99,240,1,128,116,176,1,128,45,232,66,128,93,112,2,128,39,96,131,126,95,48, 
  2,128,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,18,120,0,0,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,3,17,1,71, 
  98,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18, 
  81,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,47,208,0,128,33,16,193,127,3,17,1,126,72,2,0,1,3,18,119,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,146,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,216,3,128,63,96,3,130,34,80,4,128,99,104,194, 
  129,116,40,2,128,45,152,195,128,94,232,2,128,39,16,132,126,93,40,3,128,95,168,2,128,123,240,1,128,3,18,11,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1, 
  3,17,1,159,127,2,0,1,3,18,120,0,0,0,1,3,18,115,0,0,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0, 
  2,0,0,0,40,128,2,129,93,72,2,128,94,8,2,128,99,200,129,128,116,136,1,128,123,80,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3, 
  18,120,0,0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,76,0,0,0, 
  255,255,255,255,5,0,0,0,2,0,0,0,40,40,2,129,93,240,1,128,94,176,1,128,99,112,1,128,116,48,1,128,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,120,0, 
  0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87,0,0,0,255,255,255, 
  255,6,0,0,0,2,0,0,0,40,128,66,129,93,16,2,128,94,208,1,128,63,72,66,128,99,144,1,128,116,80,1,128,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,120, 
  0,0,0,1,3,18,115,0,0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21, 
  4,147,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,216,3,128,63,96,3,130,34,88,4,128,99,104,194,129,116,40,2,128,45,152,195,128,94,232,2,128,39,24,132,126,93,40,3,128,95,168,2,128,123,240,1, 
  128,3,18,11,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127,2,0,1,3,18,120,0,0,0,1,3,18,115,0,0,0,1,3,17,1,92,69,2, 
  0,1,3,17,1,30,134,2,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0, 
  0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,93,208,0,128,47,8,193,127,3,18,120,0,0,0,1,3,17,1,126,72,2,0,1, 
  21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,88,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40, 
  128,2,129,99,200,1,129,58,64,2,128,63,8,130,127,116,136,1,128,123,80,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,18,115,0,0,0,1,3,17,1,3,107,2,0, 
  1,3,17,1,30,134,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,76,0,0,0,255,255,255,255, 
  5,0,0,0,2,0,0,0,40,40,2,129,99,112,1,128,94,176,1,128,63,240,129,127,116,48,1,128,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,115,0,0,0,1,3, 
  18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0, 
  0,2,0,0,0,40,208,65,128,116,16,1,128,94,144,1,128,99,80,1,128,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,76,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,40,66,128,116,104,1,128,94, 
  232,1,128,99,168,65,128,123,48,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,124,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,40,3,128,41,104,3,128,58,104,2,128, 
  35,160,67,127,60,40,2,129,45,232,2,128,95,232,1,128,47,168,194,127,124,176,1,128,3,18,84,0,0,0,1,3,17,1,162,80,2,0,1,3,17,1,214,124,2,0,1,3,17,1,184,124,2,0,1,3,17,1,126,72, 
  2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,85,0,0,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128, 
  5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,25,0,0,0,1,2,21,4,121, 
  0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,80,3,128,99,96,66,129,58,16,3,128,91,160,130,127,36,144,67,128,116,32,194,128,123,232,1,128,63,216,2,128,124,176,1,128,3,18,84,0,0,0,1,3,18, 
  11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,211,135,2,0,1,3,17,1,30,134,2,0,1,3,18,98,0,0,0,1,21,2, 
  40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,205,0,0,0,255,255,255,255,16,0,0,0,4,0,0,0,91,136,3, 
  128,95,72,3,128,34,40,6,128,35,232,133,128,36,176,133,128,99,8,3,128,116,200,2,128,39,112,5,128,40,56,5,128,41,0,5,128,58,0,4,128,43,192,68,125,60,192,131,128,45,128,4,128,124,144,2,128,47,64,132, 
  124,3,18,84,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,18,44,0,0,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147,2,0,1,3,17,1,126,72, 
  2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,119,124,2,0,1,3,17,1,164, 
  91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,41, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,16,1,128,97,208,0,128,3,17,1,51,111,2,0,1,3,18,24,0,0,0,1,2,21,4,145,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,16,4, 
  128,63,24,3,128,58,144,3,128,91,224,194,128,36,80,4,129,61,80,3,128,99,160,194,128,47,208,131,126,116,96,130,128,123,40,2,128,124,240,1,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,156,131, 
  2,0,1,3,17,1,126,131,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,213,81,2,0,1,3,17,1,211,135,2,0,1,3,17,1,126,72,2,0,1,3,17,1,30,134,2,0,1,3,18,98, 
  0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,124,0,0,0,255,255,255,255,9,0,0,0,3, 
  0,0,0,43,40,3,128,41,104,3,128,58,104,2,128,35,160,67,127,60,40,2,129,45,232,2,128,95,232,1,128,47,168,194,127,124,176,1,128,3,18,84,0,0,0,1,3,17,1,162,80,2,0,1,3,17,1,214,124,2, 
  0,1,3,17,1,13,147,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,85,0,0,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3, 
  0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,227,0,0,0,255,255,255,255,18,0,0,0,4,0,0,0, 
  91,64,4,132,95,192,3,128,34,216,6,128,35,152,134,128,36,96,134,128,99,128,3,128,116,64,3,128,39,32,6,128,40,232,5,128,41,176,5,128,58,240,4,128,43,112,69,125,60,176,68,129,45,48,5,128,94,0,4,128, 
  63,120,132,124,123,8,3,128,124,208,2,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127,2,0,1,3,18, 
  44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,17, 
  1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,119,124,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3, 
  18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,121,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,88,3,128,99,96,130,129,58,24,3,128,91,224,130,127,36,144,67,128, 
  116,32,194,128,94,160,2,128,123,232,1,128,124,176,1,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0, 
  1,3,17,1,20,111,2,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0, 
  0,0,1,2,21,4,110,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,0,3,128,99,8,2,128,58,192,2,128,91,136,130,127,36,56,67,128,116,200,129,128,94,72,2,128,124,144,1,128,3,18,84,0,0,0, 
  1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,17,1,20,111,2,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,121,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,88,3,128,99,40,2, 
  128,58,24,3,128,91,168,130,127,36,144,67,128,116,232,193,128,94,104,2,128,63,224,2,128,124,176,1,128,3,18,84,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1, 
  3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,20,111,2,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8, 
  1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,228,0,0,0,255,255,255,255,18,0,0,0,4,0,0,0,91,64,4,132,95,192,3,128,34,224,6,128,35,160,134,128,36,104,134,128,99,128,3,128,116, 
  64,3,128,39,40,6,128,40,232,5,128,41,176,5,128,58,240,4,128,43,112,69,125,60,176,68,129,45,48,5,128,94,0,4,128,63,120,132,124,123,8,3,128,124,208,2,128,3,18,84,0,0,0,1,3,18,11,0,0,0, 
  1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147, 
  2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,85,0,0,0,1,3,17,1,30,134,2,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,119,124,2,0,1,3,17,1, 
  164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4, 
  41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,36,16,1,128,47,208,0,128,3,17,1,126,72,2,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,99,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,40,216,66,129,91,40,194,128,58,152,2,128,63,96,130,127,99,232,129,128,116,168, 
  1,128,123,112,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,3,107,2,0,1,3,17,1,30,134,2,0,1,21, 
  2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,128, 
  66,129,91,16,194,128,94,208,1,128,63,72,130,127,99,144,1,128,116,80,1,128,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1, 
  3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,76,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,40,40,2,129,99,112,1,128,94,176,1,128,91,240,129,127,116,48,1,128,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,18,53, 
  0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0,2, 
  0,0,0,40,128,2,129,99,200,1,129,94,8,2,128,91,72,130,127,116,136,1,128,123,80,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,159,127,2,0,1,3,18, 
  44,0,0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,123,0,0,0,255, 
  255,255,255,9,0,0,0,3,0,0,0,40,152,3,128,63,160,2,128,58,24,3,128,91,104,194,128,116,232,1,128,61,216,2,128,99,40,130,128,47,88,131,126,123,176,1,128,3,18,11,0,0,0,1,3,17,1,156,131,2, 
  0,1,3,17,1,126,131,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,213,81,2,0,1,3,17,1,3,107,2,0,1,3,17,1,126,72,2,0,1,3,17,1,30,134,2,0,1,21,2,40,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47, 
  16,1,128,3,17,1,248,155,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,23,156,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,54,156,2,0, 
  1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,13,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,103,208,0,128,47,16,193,127,3,17, 
  1,174,83,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,54,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,112,240,0,128,47,112,65,128,103,48,1,128,3,17,1,6,157,2,0,1,3,17,1,174,83,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,0,86,2,0,1, 
  2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,208,0,128,47,16,1,128,3,17,1,6,157,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,77,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,48,2,128,45,240,1,128,95,112,129,128,47,176,193,127,115,48, 
  1,128,3,17,1,11,158,2,0,1,3,17,1,162,80,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,21,2,69,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4, 
  128,1,128,5,72,1,128,6,16,1,128,3,232,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,3,17,1,18,81,2,0,1,2,18,58,0,0,0,21,4,66,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,160,193,127,121,240,0,128,3,17,1,135,158,2,0,1,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2, 
  0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0, 
  21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,160,193,127,109,240,0,128,3,17,1,3,159,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,15,1,120,72,2,0,17, 
  1,55,70,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18, 
  58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,98,240,0,128,45,112,65,128,95,48,1,128,3,17,1,105,159,2,0,1,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,21,2, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,95,48,129,128,45,112,193,127,111,240,0,128,3,17,1,207,159,2,0,1,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208, 
  0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,108,240,0,128,45,112,65,128,95,48,1,128,3,17, 
  1,53,160,2,0,1,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81, 
  126,2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,40,129,128,45,104,193,127,115,240,0,128,3,18,65,0,0,0,1,3,17,1,81,126,2,0,1,3,17,1,81,126, 
  2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,58,176,0,128,3,17,1,211,135,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,52,161,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,18,59,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255, 
  4,0,0,0,2,0,0,0,40,208,1,128,45,144,1,128,95,16,1,128,47,80,193,127,3,17,1,162,80,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,18,53,0,0,0,1,21,2,69,0,0, 
  0,255,255,255,255,4,0,0,0,2,0,0,0,4,128,1,128,5,72,1,128,6,16,1,128,3,232,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,3,17,1,18, 
  81,2,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,119,208,0,128,47,16,193,127,3,17,1,61,162,2,0,1,3,17,1,126,72,2,0,1,21,2,57,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,15,1,119,90,2,0,17,1,20,90,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,105,176,0,128,3,17,1,92,162,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,123,162,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,104,176,0,128,3,18,61,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,114,208,0,128,47,16,1,128,3,17,1,236,162,2,0,1,3,17,1,126,72,2,0,1,21, 
  2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176, 
  0,128,3,17,1,11,163,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,42,163,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  101,176,0,128,3,18,60,0,0,0,1,2,21,4,125,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,104,3,128,95,40,2,128,58,168,2,128,35,168,67,127,60,104,130,128,45,40,195,128,116,232,1,128,47,232, 
  130,126,125,176,1,128,3,18,82,0,0,0,1,3,17,1,244,124,2,0,1,3,17,1,162,80,2,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0, 
  1,3,17,1,75,145,2,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0, 
  1,3,17,1,18,81,2,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,112,80,1,128,97,80,2,129,102,16,2,128,47,144,66,128,103,208,1,128,109,144,1,128,3,17,1,125,164,2,0,1, 
  3,17,1,10,84,2,0,1,3,17,1,174,83,2,0,1,3,17,1,82,83,2,0,1,3,17,1,184,82,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,117,208,0,128,97,16,193,127,3,17,1,0,86,2,0,1,3,17,1,200,84, 
  2,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,47,8,1,128,3,18,56,0,0,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,80,176,129,128,45,48,2,128,84,112,1,128,47,240,65,128, 
  95,48,1,128,3,17,1,162,80,2,0,1,3,17,1,62,169,2,0,1,3,17,1,125,165,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  82,96,1,128,45,160,65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,17,1,249,165,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,79,96,129,128,45,160,193,127,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,17,1,117,166,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,68,48,1,128,45,112,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,219,166,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,85,48,129,128,45,112,193,127,95,240, 
  0,128,3,17,1,81,126,2,0,1,3,17,1,65,167,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1, 
  3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,67,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,167,167,2,0,1, 
  3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,112,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,13,168,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,48,129,128, 
  45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,115,168,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17, 
  1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17, 
  1,217,168,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0, 
  0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,48,1,128,45,104,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,18,101,0,0,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,69,96,129,128,45,160,193,127,95,240,0,128,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,17,1,186,169,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,82,96,1,128,45,160,65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,17,1,54,170,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,77,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,156,170,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,48,129,128,45,112,193,127,95, 
  240,0,128,3,17,1,81,126,2,0,1,3,17,1,2,171,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0, 
  1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,48,1,128,45,112,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,104,171,2,0, 
  1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,206,171,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,76,48,1, 
  128,45,104,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,18,102,0,0,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17, 
  1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,104,1,128,47,40,65,128,123,240,0,128,3,18,11,0,0,0,1,3,17,1,126,72,2,0,1, 
  3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,58,208,0,128,41,16,1,128,3,17,1,3,107,2,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0, 
  0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,47,8,193,127,3,18,11,0,0,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,76,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,40,66,128,100,168,1,128,102,104, 
  1,128,47,232,65,128,123,48,1,128,3,18,11,0,0,0,1,3,17,1,98,174,2,0,1,3,17,1,169,173,2,0,1,3,17,1,126,72,2,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,200,173,2,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,231,173,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,6,174,2,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,37,174,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,68,174, 
  2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,121,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,129, 
  174,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,160,174,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17, 
  1,191,174,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,222,174,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128, 
  3,17,1,253,174,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,28,175,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176, 
  0,128,3,17,1,59,175,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,122,0,0,0,1,2,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40, 
  192,1,128,123,16,1,128,58,72,1,128,47,128,129,127,3,18,11,0,0,0,1,3,18,56,0,0,0,1,3,17,1,126,72,2,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,208,0,128,47,8,193,127,3,18,82,0,0,0,1,3,17, 
  1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0, 
  0,2,0,0,0,40,128,2,129,125,80,1,128,102,192,1,128,47,64,130,128,100,0,2,128,123,136,1,128,3,18,82,0,0,0,1,3,18,11,0,0,0,1,3,17,1,98,174,2,0,1,3,17,1,169,173,2,0,1,3, 
  17,1,126,72,2,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,1,128,47,208,0,128,3,17,1,126,72,2,0,1,3,17,1,164,91,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,116,48,1,128,47,176,193,128,34,48,2,128,39,240,129,127,99,112,1,128,3,17,1,156, 
  131,2,0,1,3,17,1,126,131,2,0,1,3,17,1,126,72,2,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8, 
  1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,125,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,104,3,128,95,40,2,128,58,168,2,128,35,168,67,127,60,104,130,128,45,40,195,128,116, 
  232,1,128,47,232,130,126,125,176,1,128,3,18,82,0,0,0,1,3,17,1,244,124,2,0,1,3,17,1,162,80,2,0,1,3,17,1,214,124,2,0,1,3,17,1,184,124,2,0,1,3,17,1,126,72,2,0,1,3,17, 
  1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3, 
  18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144,2,128,109,80,2,128,102,208,2, 
  128,47,80,67,127,3,17,1,232,178,2,0,1,3,17,1,170,178,2,0,1,3,17,1,125,164,2,0,1,3,17,1,10,84,2,0,1,3,17,1,174,83,2,0,1,3,17,1,82,83,2,0,1,3,17,1,184,82,2,0, 
  1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,201,178,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,135,86,2,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,160,87,2,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,48,1,128,47,112,65,128,115,240,0,128,3,17,1,232,178,2,0, 
  1,3,17,1,170,178,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4, 
  138,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,43,208,3,128,73,80,2,128,58,16,3,128,35,16,68,127,60,208,2,128,45,144,195,128,78,16,2,128,47,80,131,128,69,144,2,128,95,208,1,128,3,17,1,162, 
  80,2,0,1,3,17,1,48,186,2,0,1,3,17,1,78,182,2,0,1,3,17,1,37,180,2,0,1,3,17,1,214,124,2,0,1,3,17,1,184,124,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1, 
  3,17,1,75,145,2,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1, 
  3,17,1,18,81,2,0,1,2,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88,96,1,128,45,160,65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,17, 
  1,161,180,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15, 
  1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,96,1,128,45,160,65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2, 
  0,1,3,17,1,29,181,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70, 
  2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1, 
  3,17,1,131,181,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18, 
  58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,112,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,233,181,2,0,1,3,17,1,81,126,2,0,1,21,2, 
  42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,84,48,1,128,45,104,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,18,79,0,0,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0, 
  128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,77,128,1,128,45,0,194,127,95,16,1,128,71,192,193, 
  127,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,17,1,131,184,2,0,1,3,17,1,214,182,2,0,1,3,15,1,120,72,2,0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0, 
  0,78,96,1,128,45,160,65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,17,1,82,183,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,79,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,184,183,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4, 
  208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128,45,112,65,128,95,240,0,128,3, 
  17,1,81,126,2,0,1,3,17,1,30,184,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1, 
  81,126,2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,48,129,128,45,104,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,18,83,0,0,0,1,3,17,1,81, 
  126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,66,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,80,96,1,128,45,160,65,128,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,17,1,255,184,2,0,1,3,15,1,120,72,2,0,17,1,55,70,2,0,1,21,2, 
  52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70,2,0,1,1,18,58,0,0,0,21,4,54,0, 
  0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,48,129,128,45,112,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,101,185,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,48,1,128, 
  45,112,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,17,1,203,185,2,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17, 
  1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,48,1,128,45,104,65,128,95,240,0,128,3,17,1,81,126,2,0,1,3,18, 
  80,0,0,0,1,3,17,1,81,126,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,18,58,0,0, 
  0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,96,129,128,45,160,193,127,95,240,0,128,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,17,1,172,186,2,0,1,3,15,1,120,72,2,0, 
  17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1, 
  18,58,0,0,0,21,4,66,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,96,129,128,45,160,193,127,95,240,0,128,3,15,1,120,72,2,0,17,1,55,70,2,0,1,3,17,1,40,187,2,0,1,3,15,1, 
  120,72,2,0,17,1,55,70,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,140,70,2,0,1,15,1,120,72,2,0,17,1,140,70, 
  2,0,1,1,18,58,0,0,0,21,4,53,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,48,129,128,45,104,193,127,95,240,0,128,3,17,1,81,126,2,0,1,3,18,81,0,0,0,1,3,17,1,81,126,2, 
  0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,81,126,2,0,1,3,17,1,81,126,2,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,62,208,0,128,47,8,1,128,3,18,20,0,0,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3, 
  18,2,0,0,0,1,2,21,4,113,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,43,8,3,128,95,200,1,128,58,72,2,128,35,72,67,127,60,8,130,128,45,200,2,128,124,144,1,128,47,136,130,126,3,18,84, 
  0,0,0,1,3,17,1,162,80,2,0,1,3,17,1,214,124,2,0,1,3,17,1,184,124,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,17,1,119,124,2,0,1, 
  21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,194,0,0,0,255, 
  255,255,255,15,0,0,0,3,0,0,0,40,224,4,128,43,160,68,130,34,208,133,129,35,144,133,127,36,88,69,129,45,96,4,128,47,32,68,129,39,24,197,127,58,224,3,128,60,160,3,129,91,104,131,128,95,40,3,128,99, 
  232,2,128,116,168,66,128,124,112,2,128,3,18,84,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,18,44,0,0,0,1,3,17,1,214,124,2,0,1,3,17,1, 
  13,147,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,119,124,2,0,1,3, 
  17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2, 
  21,4,113,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,43,8,3,128,95,200,1,128,58,72,2,128,35,72,67,127,60,8,130,128,45,200,2,128,124,144,1,128,47,136,130,126,3,18,84,0,0,0,1,3,17,1, 
  162,80,2,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,216,0,0,0,255,255,255,255,17,0,0,0, 
  4,0,0,0,91,32,68,130,95,160,3,128,34,128,6,128,35,64,134,128,36,8,134,128,99,96,3,128,116,32,3,128,39,200,5,128,40,144,5,128,123,232,2,128,58,208,4,128,43,80,69,125,60,144,4,129,45,16,5,128, 
  94,224,3,128,63,88,132,124,124,176,2,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,55,105,2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127,2,0,1,3,18, 
  44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,18,53,0,0,0,1,3,17,1,71,98,2,0,1,3, 
  18,98,0,0,0,1,3,17,1,119,124,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3, 
  18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,217,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,91,32,68,130,95,160,3,128,34,136,6,128,35,72,134,128,36,16,134,128,99,96,3,128,116,32,3, 
  128,39,208,5,128,40,144,5,128,123,232,2,128,58,208,4,128,43,80,69,125,60,144,4,129,45,16,5,128,94,224,3,128,63,88,132,124,124,176,2,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,55,105, 
  2,0,1,3,17,1,188,104,2,0,1,3,17,1,162,80,2,0,1,3,17,1,159,127,2,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,214,124,2,0,1,3,17,1,13,147,2,0,1,3,17,1, 
  92,69,2,0,1,3,17,1,75,145,2,0,1,3,17,1,30,134,2,0,1,3,17,1,71,98,2,0,1,3,18,98,0,0,0,1,3,17,1,119,124,2,0,1,3,17,1,164,91,2,0,1,21,2,52,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1, 
  0,0,0,58,112,1,128,65,48,65,128,97,240,0,128,3,17,1,186,192,2,0,1,3,17,1,156,192,2,0,1,3,17,1,3,107,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,83,176,0,128,3,18,77,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,115,176,0,128,3,18,78,0,0,0,1,2,21,4,162,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,43,144,4,128,65,80,3,130,58,208,3,128,35,208,68,127,60,144,3,128,45,80, 
  196,128,78,144,2,128,47,16,196,128,69,16,3,128,73,208,130,128,95,80,2,128,97,16,2,128,3,17,1,42,194,2,0,1,3,17,1,162,80,2,0,1,3,17,1,48,186,2,0,1,3,17,1,78,182,2,0,1,3,17, 
  1,37,180,2,0,1,3,17,1,175,193,2,0,1,3,17,1,214,124,2,0,1,3,17,1,184,124,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,17,1,119,124,2, 
  0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,18,58,0,0,0, 
  21,4,65,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,83,96,129,128,45,152,193,127,95,240,0,128,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,18,77,0,0,0,1,3,15,1,120,72,2,0,17,1, 
  204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1,1,18,58, 
  0,0,0,21,4,65,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,40,129,128,45,152,193,127,115,240,0,128,3,18,78,0,0,0,1,3,15,1,120,72,2,0,17,1,226,70,2,0,1,3,15,1,120,72,2, 
  0,17,1,204,69,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,120,72,2,0,17,1,77,71,2,0,1,15,1,120,72,2,0,17,1,77,71,2,0,1, 
  1,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,47,112,193,127,97,240,0,128,3,17,1,186,192,2,0,1,3,17,1,156,192,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,8,65,128,62,208, 
  0,128,3,18,20,0,0,0,1,3,17,1,3,107,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4, 
  102,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,60,176,1,128,45,112,2,128,58,240,1,128,35,240,66,128,43,176,66,128,47,48,66,128,95,112,1,128,3,17,1,162,80,2,0,1,3,17,1,214,124,2,0,1, 
  3,17,1,184,124,2,0,1,3,17,1,126,72,2,0,1,3,17,1,92,69,2,0,1,3,17,1,75,145,2,0,1,3,17,1,119,124,2,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240, 
  0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,18,81,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,78,196, 
  2,0,1,21,2,62,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,6,16,1,128,5,72,1,128,2,184,129,127,3,128,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,18,16,0,0,0,1,3,18, 
  18,0,0,0,1,2,18,18,0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,111,78,2,0,1,3,17,1,169,72,2,0,1,1,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2, 
  0,0,0,1,2,21,4,89,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,116,136,1,128,125,80,1,128,34,136,2,128,39,72,66,128,47,8,66,128,99,200,1,128,3,18,82,0,0,0,1,3,17,1,156,131,2, 
  0,1,3,17,1,126,131,2,0,1,3,17,1,126,72,2,0,1,3,17,1,71,98,2,0,1,3,17,1,164,91,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128, 
  3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,73,80,1,128,69,144,193,127,78,16,1,128,47,208,1,128,3,17,1,109,199,2,0,1,3,17,1, 
  76,198,2,0,1,3,17,1,178,197,2,0,1,3,17,1,126,72,2,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0, 
  1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,88,176,0,128,3,17,1,209,197,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,80,176,0,128,3,17,1,240,197, 
  2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,79,176,0,128,3,17,1,15,198,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,82,176,0,128,3,17,1, 
  46,198,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,84,176,0,128,3,18,79,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,77,208,0,128,71,16, 
  193,127,3,17,1,242,198,2,0,1,3,17,1,119,198,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,78,176,0,128,3,17,1,150,198,2,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,79,176,0,128,3,17,1,181,198,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,82,176,0,128,3,17,1,212,198,2,0,1,2,21,4,29,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,69,176,0,128,3,18,83,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,80,176,0,128,3,17,1,17,199,2,0,1,2,21,4,30,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,79,176,0,128,3,17,1,48,199,2,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,82,176,0,128,3,17,1,79,199,2,0,1,2,21,4,29,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,84,176,0,128,3,18,80,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,65,176,0,128,3,17,1,140,199,2,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,77,176,0,128,3,17,1,171,199,2,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,69,176,0,128,3,18,81,0,0,0,1,2,21,4,36, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,15,1,97,200,2,0,17,1,29,200,2,0,1,21,2,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15, 
  1,47,206,2,0,17,1,204,205,2,0,1,3,17,1,152,203,2,0,1,2,21,4,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,15,1,225,70,2,0,17,1,66,200,2,0,1,1,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,66,200,2,0,1,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,16,1,128,3,17,1,28,203, 
  2,0,1,3,17,1,140,200,2,0,1,2,18,27,0,0,0,21,4,66,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,112,193,127,3,15,1,22,203,2,0,15,1,9,201,2,0,17,1,8,201, 
  2,0,1,3,15,1,22,203,2,0,15,1,9,201,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,22,203,2,0,17,1,171,202,2, 
  0,1,15,1,22,203,2,0,17,1,171,202,2,0,1,1,1,21,4,66,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,112,193,127,3,15,1,225,70,2,0,15,1,128,201,2,0,17,1,8,201, 
  2,0,1,3,15,1,225,70,2,0,15,1,128,201,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,64,202,2, 
  0,1,15,1,225,70,2,0,17,1,64,202,2,0,1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,128,201,2,0,17,1,8,201,2,0,1,3,15,1,128, 
  201,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,128,201,2,0,17,1,235,201,2,0,1,15,1,128,201,2,0,17,1,235,201,2, 
  0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,16,193,127,3,17,1,8,201,2,0,1,3,17,1,8,201,2,0,1,21,2,42,0,0,0,255,255,255,255,2,0,0,0,1, 
  0,0,0,4,208,0,128,3,16,1,128,3,17,1,8,201,2,0,1,3,17,1,8,201,2,0,1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,128,201,2, 
  0,17,1,8,201,2,0,1,3,15,1,128,201,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,128,201,2,0,17,1,235,201,2,0, 
  1,15,1,128,201,2,0,17,1,235,201,2,0,1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,9,201,2,0,17,1,8,201,2,0,1,3,15,1,9,201, 
  2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,9,201,2,0,17,1,235,201,2,0,1,15,1,9,201,2,0,17,1,235,201,2,0, 
  1,2,18,27,0,0,0,1,18,27,0,0,0,21,4,66,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,112,193,127,3,15,1,22,203,2,0,15,1,9,201,2,0,17,1,8,201,2,0,1,3, 
  15,1,22,203,2,0,15,1,9,201,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,22,203,2,0,17,1,171,202,2,0,1,15,1, 
  22,203,2,0,17,1,171,202,2,0,1,1,18,27,0,0,0,21,4,66,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,112,193,127,3,15,1,22,203,2,0,15,1,20,204,2,0,17,1,8,201, 
  2,0,1,3,15,1,22,203,2,0,15,1,20,204,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,22,203,2,0,17,1,97,205,2, 
  0,1,15,1,22,203,2,0,17,1,97,205,2,0,1,1,21,4,66,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,112,193,127,3,15,1,225,70,2,0,15,1,139,204,2,0,17,1,8,201,2, 
  0,1,3,15,1,225,70,2,0,15,1,139,204,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,225,70,2,0,17,1,246,204,2,0, 
  1,15,1,225,70,2,0,17,1,246,204,2,0,1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,139,204,2,0,17,1,8,201,2,0,1,3,15,1,139,204, 
  2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,139,204,2,0,17,1,235,201,2,0,1,15,1,139,204,2,0,17,1,235,201,2,0, 
  1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,139,204,2,0,17,1,8,201,2,0,1,3,15,1,139,204,2,0,17,1,8,201,2,0,1,21,2,52,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,56,1,128,15,1,139,204,2,0,17,1,235,201,2,0,1,15,1,139,204,2,0,17,1,235,201,2,0,1,2,21,4,54,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,95,208,0,128,45,64,193,127,3,15,1,20,204,2,0,17,1,8,201,2,0,1,3,15,1,20,204,2,0,17,1,8,201,2,0,1,21,2,52,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,4,208,0,128,3,56,1,128,15,1,20,204,2,0,17,1,235,201,2,0,1,15,1,20,204,2,0,17,1,235,201,2,0,1,2,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17, 
  1,235,205,2,0,1,2,21,2,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,15,1,225,70,2,0,17,1,16,206,2,0,1,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,4,176,0,128,3,17,1,16,206,2,0,1,1,18,28,0,0,0,1,21,5,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,1,176,0,128,3,18,8,0,0,0,1,2, 
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
    parser.init_parser(92104);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0.into_strings();
    Ok(obj_0_0)
  }
  
  pub fn grammar_from<'a> (mut reader: UTF8StringReader)-> Result<Box<Grammar>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(93016);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_Grammar();
    Ok(obj_0_0)
  }
  
  pub fn type_eval_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(148022);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_expression_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(148172);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_struct_from<'a> (mut reader: UTF8StringReader)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(148650);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_AST_Struct();
    Ok(obj_0_0)
  }
}