
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


/* "IMPORT" ( c:id | c:sym | c:num )(+) c:sp ( "AS" | "as" ) tok::id

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


/* "IMPORT" ( c:id | c:sym | c:num )(+) c:sp ( "AS" | "as" ) tok::id

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


/* ( c:id | c:sym | c:num ) */
fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num ) */
fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = ref_0.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( c:id | c:sym | c:num )(+) */
fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::terminal(+) */
fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* rule */
fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* rule(+"|") */
fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* import_production_symbol */
fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* '(' init_objects ")"       
        :ast { t_Init, expression: $2 } */
fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "<" token::int ( ","  token::int  )? ">"

        :ast { t_Range, start_trim:i32($2), end_trim:i32($3) } */
fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* reference '.' identifier
        :ast { t_AST_Member, reference:$1, property:$3 } */
fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* struct */
fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* expression
        :ast { t_AST_Statements, statements:[$1], tok } */
fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* expression(*";") */
fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* production_match_block */
fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal_match_block */
fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") "then" goto

            :ast { t_Gotos, pushes: $1, goto } */
fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* goto_push(+"then") */
fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Fail::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "pass"        :ast { t_Pass, tok } */
fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Pass::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "accept"      :ast { t_Accept, tok } */
fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = Accept::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* "tk:(" rules ")"{1}

        :ast { t_TokenGroupProduction, rules:$2,  tok } */
fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token_non_terminal */
fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* class */
fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* (( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi])^s 
    
    ast_definition?^a

        :ast { t_Rule, symbols:$s, ast:$a, tok } */
fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty ) */
fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+) */
fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+) */
fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi] */
fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( sym::annotated_symbol | not_empty )(+)^s sym::end_of_input?^eoi :ast [$s, $eoi] */
fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* tok::id '::' tok::id

        :ast { t_Production_Import_Symbol , c_Symbol , module:str($1), name:str($3), tok} */
fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* token */
fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "$" token::id 
        :ast { t_AST_NamedReference, value: str($2), tok } */
fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (_, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let tok_1_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
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


/* ( int_match | default_match | hint ) */
fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint ) */
fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
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


/* ( int_match | default_match | hint )(+) */
fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( int_match | default_match | hint )(+) */
fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* int_match :ast [$1] */
fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( int_match | default_match | hint )(+) "}" :ast $2 */
fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "PRODUCTION" ( non_terminal_match :ast [$1] | "{" ( non_terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_ProductionMatches, matches:$m } */
fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match ) */
fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match ) */
fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
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


/* ( non_terminal_match | hint | default_match )(+) */
fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( non_terminal_match | hint | default_match )(+) */
fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* non_terminal_match :ast [$1] */
fn reducer_214 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( non_terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_215 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "match" ":" "TERMINAL" ( terminal_match :ast [$1] | "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 )^m

        :ast { t_TerminalMatches, matches:$m } */
fn reducer_216 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match ) */
fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
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


/* ( terminal_match | hint | default_match )(+) */
fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* ( terminal_match | hint | default_match )(+) */
fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* terminal_match :ast [$1] */
fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* "{" ( terminal_match | hint | default_match )(+) "}" :ast $2 */
fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_2, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_2;
  let obj_1_0 = ref_1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}


/* "push" sym::non_terminal

    :ast { t_Push, prod: $2, name:str($2), tok } */
fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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


/* "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' | 'tab' | 'htab' )

        :ast { t_ClassSymbol, c_Symbol , val:str($2),  tok } */
fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_245 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_246 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_247 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_248 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_249 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_250 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "[" sym::annotated_symbol(+)^s ']' "!"?^o

        :ast { t_NotEmptySet, unordered: bool($o), symbols:$s, tok } */
fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* sym::annotated_symbol(+) */
fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let AstSlot (ref_1, __tok_rng_1, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_0 + __tok_rng_1;
  let obj_1_0 = ref_1;
  let mut obj_0_0 = ref_0.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}


/* "$" :ast { t_EOFSymbol, c_Symbol , tok } */
fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let var_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(var_2_0)), __rule_rng__, TokenRange::default()));
}


/* ":ast" ast::body^ast

            :ast  { t_Ascript, ast:$ast, tok } */
fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let tok_0_0 = __tok_rng_0.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_u64();
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) ) */
fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  let obj_0_0 = ref_0.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}


/* ( tok::int :ast u64($1) )(+"|") */
fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* tk:quote_tok */
fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* symbol "(+" ( token | class )? ')'

            :ast { t_List_Production, c_Symbol, terminal_symbol:$3, symbol:$1, tok } */
fn reducer_271 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_272 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_273 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_274 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_275 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_276 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_277 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* "{" tk:precedence_num '}' :ast { t_Precedence, val: u32($2) } */
fn reducer_278 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
fn reducer_279 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}


/* terminal */
fn reducer_280 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (ref_0, __tok_rng_0, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_0;
  slots.assign(0, AstSlot(ref_0, __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 281]
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
  parser.init_parser(56546);
  parser
}

pub fn new_grammar_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(57426);
  parser
}

pub fn new_type_eval_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(70010);
  parser
}

pub fn new_ast_expression_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(70160);
  parser
}

pub fn new_ast_struct_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(70638);
  parser
}

pub static bytecode: [u8; 97192] = [
  0,211,200,197,210,208,193,2,15,1,225,220,0,0,17,1,21,0,0,0,1,21,1,69,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,32,2,129,1,32,2,128,2,32,130,127,7,32,2,128,58,48,1,128,4, 
  19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,91,0,0,0,1,8,2,21,1,59,0,0,0,210,24,1,0,6,0,0,0,2,0,0,0,4,80,1,128,1,144,1,128,2,144,129, 
  128,3,152,129,128,6,144,1,128,7,144,1,128,4,17,1,186,217,0,0,1,8,4,17,1,151,0,0,0,1,2,21,1,132,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,144,12,128,1,144,13,128,2,144,141, 
  128,67,88,201,131,66,152,17,128,70,144,8,128,6,144,205,127,7,144,205,129,72,88,6,131,73,200,11,128,74,8,15,128,75,208,79,129,76,144,69,129,93,32,7,128,71,88,83,129,63,152,16,128,91,88,10,128,92,152,13, 
  128,99,232,18,128,103,48,3,128,104,144,4,128,4,15,1,15,216,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,15,1, 
  15,216,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,15,216,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0, 
  130,0,0,0,1,0,15,1,15,216,0,0,17,1,38,209,0,0,1,4,19,58,0,0,0,178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,0,0, 
  0,0,3,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,15,216,0,0,17,1,38,209,0,0,1,4,15,1,15,216,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,183,205,0, 
  0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,19,37,0,0,0,128,0,0,0,1,0, 
  15,1,15,216,0,0,17,1,38,209,0,0,1,4,15,1,15,216,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1,8,4,19,58,0,0,0,177,0,0,0,1,0,19,38, 
  0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,0,0,0,0,3,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,15,216,0,0,17,1,38,209,0,0,1,4,19,37, 
  0,0,0,127,0,0,0,1,0,15,1,15,216,0,0,17,1,38,209,0,0,1,4,15,1,15,216,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,24,194,0,0,1,4,19,36,0,0,0, 
  124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,15,216,0,0,15,1,247,205,0,0,17,1,177,207,0,0,1,4,15,1,130,193,0,0,17,1,28,3,0,0,1,4,19,37,0,0,0,125,0,0,0, 
  1,0,15,1,15,216,0,0,17,1,38,209,0,0,1,2,21,1,47,0,0,0,12,33,1,0,5,0,0,0,2,0,0,0,56,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,76,3, 
  0,0,1,2,21,1,82,0,0,0,94,33,1,0,7,0,0,0,2,0,0,0,6,8,2,129,1,8,2,129,2,8,130,127,7,8,2,128,58,112,129,128,101,80,2,128,102,16,2,128,4,19,35,0,0,0,113,0,0,0, 
  1,0,17,1,187,185,0,0,1,8,4,17,1,13,179,0,0,1,4,17,1,159,3,0,0,1,2,21,1,77,0,0,0,131,39,1,0,6,0,0,0,2,0,0,0,6,240,1,128,1,240,1,129,2,240,129,127,7,240,65, 
  128,11,248,1,128,53,80,1,128,4,15,1,245,178,0,0,15,1,1,179,0,0,17,1,35,13,0,0,1,8,4,15,1,245,178,0,0,17,1,237,3,0,0,1,2,21,1,155,0,0,0,224,39,1,0,8,0,0,0,3, 
  0,0,0,121,56,3,128,1,144,193,127,2,144,129,128,11,104,2,128,122,152,1,128,53,8,4,128,6,144,1,128,7,144,1,128,8,4,15,1,233,11,0,0,15,1,30,12,0,0,15,1,23,13,0,0,17,1,73,11,0, 
  0,1,4,15,1,233,11,0,0,15,1,30,12,0,0,15,1,61,11,0,0,17,1,1,8,0,0,1,4,15,1,233,11,0,0,15,1,30,12,0,0,15,1,61,11,0,0,17,1,149,4,0,0,1,4,15,1,233,11,0, 
  0,15,1,30,12,0,0,15,1,137,4,0,0,17,1,35,13,0,0,1,2,19,71,0,0,0,208,0,0,0,1,0,1,21,1,47,0,0,0,5,42,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2, 
  48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,197,4,0,0,1,2,21,1,117,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,112,12,128,1,48,11,128,2,48,139,128,67,48,201,131,66,144,17,128,70, 
  160,6,128,6,48,203,127,7,48,203,129,72,48,3,131,73,56,11,128,74,104,7,128,75,112,77,129,76,200,80,129,93,128,15,128,71,224,82,129,63,48,8,128,91,248,3,128,92,56,14,128,99,0,12,128,103,64,5,128,104, 
  48,10,128,4,19,37,0,0,0,130,0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0, 
  1,0,17,1,204,7,0,0,1,4,15,1,204,7,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,19,37,0,0,0,129, 
  0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,4,15,1,204,7,0,0,15,1,247,205,0,0,15,1,177,207, 
  0,0,15,1,26,209,0,0,17,1,24,194,0,0,1,4,15,1,204,7,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,183,205,0,0,1,4,15,1,204,7,0,0,15,1,27,216,0,0, 
  15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,8,4,19,37,0,0,0,128,0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,4,15,1,59,7,0,0,17,1,28,3,0,0,1,4, 
  15,1,204,7,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,4,19,58,0, 
  0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,204,7,0,0,1,4,19,58,0,0,0,178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0, 
  19,12,0,0,0,42,0,0,0,1,0,17,1,204,7,0,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0, 
  45,0,0,0,1,0,15,1,204,7,0,0,15,1,247,205,0,0,17,1,177,207,0,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,204,7,0,0,17,1,38,209,0,0,1,2,21,0,144,0,0,0,255,255,255, 
  255,7,0,0,0,2,0,0,0,12,72,4,128,73,120,65,129,38,184,131,128,55,40,3,128,66,152,66,128,70,8,2,128,89,112,1,128,1,19,55,0,0,0,171,0,0,0,1,0,17,1,59,7,0,0,1,19,55,0,0, 
  0,170,0,0,0,1,0,17,1,59,7,0,0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,59,7,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,59,7,0,0,1,19,12,0,0,0,42,0,0,0,1, 
  0,17,1,59,7,0,0,1,17,1,204,7,0,0,1,2,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,89,0,0, 
  0,8,1,0,0,4,0,1,2,21,1,117,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,24,16,128,1,48,6,128,2,48,134,128,67,168,210,131,66,200,7,128,70,224,17,128,6,48,198,127,7,48,198,129,72, 
  40,13,131,73,24,17,128,74,56,6,128,75,80,79,129,76,24,75,129,93,48,3,128,71,0,71,129,63,24,10,128,91,232,4,128,92,224,11,128,99,120,4,128,103,240,13,128,104,24,9,128,4,19,58,0,0,0,178,0,0, 
  0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,8,11,0,0,1,4,15,1,119,10,0,0,17,1,28,3,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38, 
  0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,8,11,0,0,1,8,4,19,37,0,0,0,131,0,0,0,1,0,15,1,8,11,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,125, 
  0,0,0,1,0,15,1,8,11,0,0,17,1,38,209,0,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,8,11,0,0,15,1,247,205,0,0,17,1,177,207,0,0,1, 
  4,15,1,8,11,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,15,1,8,11,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,24, 
  194,0,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,8,11,0,0,17,1,38,209,0,0,1,4,19,58,0,0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0, 
  0,1,0,17,1,8,11,0,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,8,11,0,0,17,1,38,209,0,0,1,4,15,1,8,11,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,51,216,0,0, 
  15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,8,11,0,0,17,1,38,209,0,0,1,4,15,1,8,11,0,0,15,1,247,205,0,0,15,1,177, 
  207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,8,11,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,8,11,0,0,17, 
  1,38,209,0,0,1,4,15,1,8,11,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,183,205,0,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4, 
  128,73,120,65,129,38,184,131,128,55,40,3,128,66,152,66,128,70,8,2,128,89,112,1,128,1,19,55,0,0,0,171,0,0,0,1,0,17,1,119,10,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1,119,10,0, 
  0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,119,10,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,119,10,0,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,119,10,0,0,1,17,1,8,11, 
  0,0,1,2,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,89,0,0,0,9,1,0,0,3,0,1,8,2,19,71,0, 
  0,0,210,0,0,0,1,0,1,21,1,47,0,0,0,5,42,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,121,11,0,0,1,2,21,1,58, 
  0,0,0,169,42,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,125,56,1,128,8,4,19,93,0,0,0,13,1,0,0,1,0,17,1,180,11,0,0,1,2,21,1,52,0, 
  0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,90,0,0,0,10,1,0,0,4,0,1,8,2,21,1,52,0,0,0,87,42,1,0,5, 
  0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,72,0,0,0,215,0,0,0,3,0,1,8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  71,176,0,128,15,1,30,12,0,0,17,1,66,12,0,0,1,2,21,1,59,0,0,0,209,44,1,0,8,0,0,0,3,0,0,0,121,144,1,128,1,208,193,127,2,208,129,128,11,144,1,128,122,144,1,128,53,144,1,128, 
  6,208,1,128,7,208,1,128,12,17,1,135,12,0,0,1,10,12,17,1,133,12,0,0,1,14,1,21,1,107,0,0,0,224,39,1,0,8,0,0,0,3,0,0,0,121,232,2,128,1,112,194,127,2,112,130,128,11,144,1, 
  128,122,0,2,128,53,120,2,128,6,112,2,128,7,112,2,128,4,15,1,11,13,0,0,17,1,1,8,0,0,1,4,15,1,255,12,0,0,17,1,73,11,0,0,1,8,4,15,1,243,12,0,0,17,1,35,13,0,0,1, 
  4,15,1,11,13,0,0,17,1,149,4,0,0,1,2,19,71,0,0,0,211,0,0,0,2,0,1,19,71,0,0,0,212,0,0,0,2,0,1,19,71,0,0,0,213,0,0,0,2,0,1,19,71,0,0,0,209,0,0,0, 
  1,0,1,21,1,88,0,0,0,81,45,1,0,6,0,0,0,2,0,0,0,6,24,66,129,1,24,194,128,2,24,130,127,7,24,2,128,53,32,2,128,58,80,1,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,166, 
  178,0,0,17,1,115,178,0,0,1,8,4,15,1,159,174,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,162,2,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,248,81,130,1,160,201,129,2,160, 
  137,127,94,192,18,130,44,96,5,128,53,168,73,129,6,160,73,127,7,160,9,128,105,32,7,128,114,120,15,128,125,16,2,128,126,40,12,128,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1, 
  0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,165,115,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15, 
  1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,165,115,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0, 
  0,15,1,111,159,0,0,15,1,90,115,0,0,17,1,115,91,0,0,1,4,15,1,165,115,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111, 
  159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,165,115,0,0,15,1,218,115,0,0,15,1,230,115,0,0, 
  15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0, 
  1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,165,115,0,0,15,1,218,115,0,0,15,1, 
  230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,165,115,0,0,15,1,218,115,0,0, 
  15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0, 
  17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,13,89,0,0,17,1,115,178,0,0,1,4,15,1,165,115,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227, 
  158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,2,21,1,162,2,0,0,191,45,1,0,12,0,0, 
  0,3,0,0,0,58,136,82,130,1,224,204,129,2,224,140,127,94,232,12,130,44,80,19,128,53,144,68,129,6,224,76,127,7,224,12,128,105,96,10,128,114,16,2,128,125,16,7,128,126,56,15,128,4,15,1,216,88,0,0, 
  15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0, 
  15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,216,88,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174, 
  0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0, 
  0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,216,88,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0, 
  15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,216,88,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159, 
  0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,216,88,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15, 
  1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0, 
  14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,216,88,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215, 
  158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,194,18,0,0,17,1, 
  115,178,0,0,1,4,15,1,216,88,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,90,115,0,0,17,1,115,91,0,0, 
  1,2,21,0,229,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,59,96,5,128,81,224,130,130,61,104,4,128,11,152,70,127,60,208,4,128,45,96,70,127,46,248,133,128,47,104,133,128,62,0,4,128,63,112,67,128, 
  95,120,2,128,97,16,2,128,15,1,194,18,0,0,17,1,106,86,0,0,1,15,1,194,18,0,0,17,1,244,82,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,194,18,0,0,1,19,47,0,0,0,155,0,0, 
  0,1,0,17,1,194,18,0,0,1,15,1,194,18,0,0,17,1,46,43,0,0,1,15,1,194,18,0,0,17,1,9,23,0,0,1,19,46,0,0,0,151,0,0,0,1,0,17,1,194,18,0,0,1,1,19,97,0,0,0, 
  23,1,0,0,1,0,17,1,194,18,0,0,1,15,1,194,18,0,0,17,1,168,19,0,0,1,17,1,216,88,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,194,18,0,0,1,2,21,1,47,0,0,0,139,49, 
  1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,228,19,0,0,1,12,19,45,0,0,0,150,0,0,0,1,0,1,21,1,47,0,0,0,232,49, 
  1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,20,20,0,0,1,2,21,1,36,2,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,88, 
  80,130,1,208,203,129,2,208,139,127,94,152,14,130,44,160,10,128,53,0,68,129,6,208,75,127,7,208,11,128,105,176,8,128,114,16,2,128,125,216,11,128,126,240,5,128,4,15,1,253,22,0,0,15,1,227,158,0,0,15, 
  1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,253,22,0,0,15,1,227,158,0, 
  0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,94,0,0,0,14,1,0,0, 
  1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,253,22,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15, 
  1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,253,22,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0, 
  0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,253,22,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,90,115,0,0,17,1,115,91,0,0,1,8,4, 
  19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,253,22,0,0,15,1,227,158,0,0,15,1,36,159, 
  0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,253,22,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1, 
  78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,57,22,0,0,17,1,115,178,0,0,1,2,21,0,195,0,0,0,255,255,255,255,10, 
  0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129,11,136,5,128,60,144,4,128,61,40,4,128,46,128,133,126,47,240,196,126,95,56,2,128,97,208,1,128,15,1,57,22,0,0,17,1,106,86,0,0,1, 
  15,1,57,22,0,0,17,1,244,82,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,57,22,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,57,22,0,0,1,15,1,57,22,0,0,17,1,46,43,0, 
  0,1,15,1,57,22,0,0,17,1,9,23,0,0,1,19,46,0,0,0,152,0,0,0,3,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,57,22,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,57, 
  22,0,0,1,2,19,46,0,0,0,152,0,0,0,3,0,1,21,1,143,0,0,0,58,50,1,0,13,0,0,0,3,0,0,0,58,184,3,130,1,48,3,130,2,48,131,127,94,248,67,130,44,176,2,128,53,48,130,129,6, 
  48,67,127,7,48,3,128,98,112,130,128,105,240,2,128,114,56,3,128,125,56,4,128,126,120,3,128,12,17,1,5,41,0,0,1,12,17,1,220,38,0,0,1,12,17,1,179,36,0,0,1,12,17,1,138,34,0,0,1,10, 
  12,17,1,97,32,0,0,1,12,17,1,56,30,0,0,1,12,17,1,15,28,0,0,1,12,17,1,230,25,0,0,1,12,17,1,165,23,0,0,1,12,19,62,0,0,0,190,0,0,0,1,0,1,21,1,152,1,0,0,108, 
  51,1,0,13,0,0,0,3,0,0,0,58,240,2,130,1,136,11,130,2,136,139,127,94,184,67,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126, 
  184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,86,25,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15, 
  1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97, 
  0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17, 
  1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78, 
  174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0, 
  0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,19,61,0,0,0,188,0,0,0,2,0,1,19,61,0,0,0,187, 
  0,0,0,2,0,1,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,130,128,81,176,195,128,95,144,1,128,11,0,2,128,97,16,4,128,61,248,1,128,62,248,1,128,47,32,67,126,15,1,86,25, 
  0,0,17,1,244,82,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,86,25,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,86,25,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,86, 
  25,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,86,25,0,0,17,1,106,86,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,2,130,1,136,11,130,2,136,139,127, 
  94,184,67,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0, 
  0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,127,27,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0, 
  1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1, 
  102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0, 
  227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15, 
  1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,130,128,81,176,195,128,95,40,2,128,11,152,1,128,97,16,4,128,61,144,1, 
  128,62,144,1,128,47,32,67,126,1,19,47,0,0,0,154,0,0,0,1,0,17,1,127,27,0,0,1,15,1,127,27,0,0,17,1,244,82,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,127,27,0,0,1,19, 
  97,0,0,0,23,1,0,0,1,0,17,1,127,27,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,127,27,0,0,17,1,106,86,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0, 
  0,58,200,10,130,1,192,10,130,2,192,138,127,94,240,66,130,44,80,10,128,53,144,139,129,6,192,74,127,7,192,10,128,98,48,130,128,105,240,5,128,114,32,9,128,125,32,7,128,126,240,3,128,4,19,84,0,0,0,2, 
  1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1, 
  0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0, 
  15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180, 
  0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0, 
  15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,168,29,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0, 
  15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,32,130,128,81,24,196,128,95,184, 
  2,128,11,144,1,128,97,176,3,128,61,176,2,128,62,176,2,128,47,32,67,126,19,47,0,0,0,154,0,0,0,1,0,17,1,168,29,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,168,29,0,0,1,1,15, 
  1,168,29,0,0,17,1,244,82,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,168,29,0,0,1,15,1,168,29,0,0,17,1,106,86,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1, 
  0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94,240,66,130,44,80,10,128,53,144,139,129,6,192,74,127,7,192,10,128,98,48,130,128,105,240,5,128,114,32,9,128,125,32, 
  7,128,126,240,3,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0, 
  0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17, 
  1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0, 
  0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0, 
  0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,209,31,0,0,17, 
  1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3, 
  0,0,0,63,144,129,128,81,136,195,128,95,32,3,128,11,32,2,128,97,184,2,128,61,176,2,128,62,176,2,128,47,232,67,126,19,47,0,0,0,155,0,0,0,1,0,17,1,209,31,0,0,1,19,47,0,0,0,154,0, 
  0,0,1,0,17,1,209,31,0,0,1,1,15,1,209,31,0,0,17,1,106,86,0,0,1,15,1,209,31,0,0,17,1,244,82,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,97,0,0,0,23,1,0,0,1, 
  0,17,1,209,31,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,80,10,130,1,136,11,130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98, 
  48,130,128,105,240,5,128,114,32,9,128,125,32,7,128,126,240,3,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115, 
  0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0, 
  15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93, 
  0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0, 
  1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,250,33,0,0,17,1,115, 
  178,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0, 
  143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,32,195,128,95,40,2,128,11,144,1,128,97,128,3,128,61,32,2,128,62,32,2,128,47,144,66,126,19,47,0,0,0,154,0,0,0,1,0,17, 
  1,250,33,0,0,1,1,15,1,250,33,0,0,17,1,244,82,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,250,33,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,250,33,0,0,17,1,106,86, 
  0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,250,33,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,2,130,1,136,11,130,2,136,139,127,94,184,67,130,44,24,11,128, 
  53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,35,0, 
  0,0,113,0,0,0,1,0,15,1,35,36,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14, 
  1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1, 
  74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59, 
  0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1, 
  91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59, 
  178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,40,130,128,81,32,195,128,95,184,2,128,11,128,3,128,97,16,4,128,61,144,1,128,62,144,1,128,47,152,65, 
  126,1,19,97,0,0,0,23,1,0,0,1,0,17,1,35,36,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,35,36,0,0,1,15,1,35,36,0,0,17,1,244,82,0,0,1,19,61,0,0,0,187,0,0,0, 
  2,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,35,36,0,0,1,15,1,35,36,0,0,17,1,106,86,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,3,130,1,136,11, 
  130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0, 
  0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,76,38,0,0,17, 
  1,115,178,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78, 
  174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0, 
  19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15, 
  1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,104,130,128,81,136,195,128,95,248,1,128,11,248,2,128,97,144, 
  1,128,61,96,2,128,62,96,2,128,47,232,67,126,15,1,76,38,0,0,17,1,106,86,0,0,1,15,1,76,38,0,0,17,1,244,82,0,0,1,1,19,47,0,0,0,155,0,0,0,1,0,17,1,76,38,0,0,1,19, 
  47,0,0,0,154,0,0,0,1,0,17,1,76,38,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,76,38,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0, 
  0,0,3,0,0,0,58,240,3,130,1,136,11,130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19, 
  84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0, 
  113,0,0,0,1,0,15,1,117,40,0,0,17,1,115,178,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1, 
  0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0, 
  1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1, 
  102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15, 
  1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,0,130,128,81, 
  136,195,128,95,144,2,128,11,232,3,128,97,152,1,128,61,144,1,128,62,144,1,128,47,248,66,126,1,15,1,117,40,0,0,17,1,106,86,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,117,40,0,0,1,15, 
  1,117,40,0,0,17,1,244,82,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,117,40,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,117,40,0,0,1, 
  2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,2,130,1,136,11,130,2,136,139,127,94,184,67,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114, 
  232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,158,42,0,0,17,1,115,178,0,0,1, 
  4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0, 
  180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0, 
  0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15, 
  1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62, 
  25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255, 
  8,0,0,0,3,0,0,0,63,248,130,128,81,136,195,128,95,40,2,128,11,232,3,128,97,144,2,128,61,144,1,128,62,144,1,128,47,152,65,126,1,19,97,0,0,0,23,1,0,0,1,0,17,1,158,42,0,0,1,15, 
  1,158,42,0,0,17,1,244,82,0,0,1,15,1,158,42,0,0,17,1,106,86,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,158,42,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,47,0,0,0, 
  154,0,0,0,1,0,17,1,158,42,0,0,1,2,21,1,47,0,0,0,32,52,1,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,106,43,0,0,1, 
  10,12,19,60,0,0,0,184,0,0,0,1,0,1,21,1,53,0,0,0,137,52,1,0,5,0,0,0,2,0,0,0,6,160,1,129,1,160,1,128,2,160,129,127,7,160,1,128,62,48,1,128,4,15,1,232,82,0,0,17, 
  1,160,43,0,0,1,8,2,21,1,4,3,0,0,220,52,1,0,26,0,0,0,4,0,0,0,32,232,145,133,1,96,205,131,2,96,205,131,35,32,10,128,36,216,15,128,37,80,9,128,6,96,13,131,7,96,13,131,40,192, 
  11,128,41,40,20,128,42,112,5,128,11,168,80,130,44,144,12,128,29,160,4,128,46,240,10,128,31,128,136,129,33,176,7,128,34,24,17,130,38,56,78,129,39,208,67,129,43,184,18,128,47,224,6,128,48,8,15,128,54,104, 
  13,128,87,248,20,128,98,192,22,128,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,97,82,0,0,17,1,10,82,0, 
  0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,85,0,0,0,3,1,0,0,2,0,1,4,15,1,196,82,0,0,15,1,208,82,0, 
  0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17, 
  1,249,80,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,15, 
  1,196,82,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,196,82,0,0,15,1,208, 
  82,0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,8,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82, 
  0,0,17,1,18,67,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,116,65,0,0, 
  1,4,15,1,54,65,0,0,17,1,122,52,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,220,82,0,0, 
  17,1,204,51,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,85,0,0,0,3,1,0,0,2,0,1,4,15,1,196,82,0,0, 
  15,1,208,82,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0, 
  0,0,1,0,19,85,0,0,0,3,1,0,0,2,0,1,4,15,1,196,82,0,0,15,1,208,82,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1, 
  2,21,1,83,0,0,0,165,57,1,0,4,0,0,0,2,0,0,0,58,216,1,128,1,208,1,128,2,208,129,127,87,16,1,128,4,19,49,0,0,0,157,0,0,0,1,0,19,65,0,0,0,195,0,0,0,2,0,1,8, 
  4,19,35,0,0,0,113,0,0,0,1,0,19,65,0,0,0,194,0,0,0,2,0,1,2,19,8,0,0,0,29,0,0,0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,31,176,0,128,15, 
  1,5,47,0,0,17,1,41,47,0,0,1,2,21,1,39,0,0,0,14,58,1,0,3,0,0,0,1,0,0,0,2,48,129,128,1,48,1,128,50,240,0,128,12,17,1,88,47,0,0,1,10,12,17,1,133,12,0,0,1, 
  21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,43,176,0,128,4,17,1,146,47,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1, 
  93,2,0,0,135,60,1,0,23,0,0,0,4,0,0,0,32,48,209,132,1,64,201,131,2,64,201,131,35,112,3,128,36,16,14,128,37,144,16,128,38,208,140,131,39,176,142,131,40,240,15,128,41,96,7,128,42,208,17,128, 
  43,72,9,128,44,0,11,128,29,80,15,128,46,192,6,128,31,32,198,128,33,112,13,128,34,96,74,129,47,160,8,128,48,128,5,128,54,0,8,128,87,16,4,128,98,160,11,128,4,15,1,240,49,0,0,15,1,220,82,0, 
  0,17,1,75,80,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,31,0,0,0,105,0,0,0,3,0,1,4,15,1,240,49,0, 
  0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,240,49,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,240,49,0, 
  0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,240,49,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,240,49,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,8,4,19,27,0,0, 
  0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,31,0,0,0,105,0,0,0,3,0,1,4,15,1,240,49,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,240,49,0,0,15,1,145, 
  79,0,0,17,1,198,72,0,0,1,4,15,1,240,49,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0, 
  0,17,1,18,67,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0, 
  0,17,1,109,82,0,0,1,4,15,1,240,49,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0, 
  0,17,1,162,80,0,0,1,4,15,1,240,49,0,0,15,1,220,82,0,0,17,1,204,51,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,31,0,0,0,105,0,0,0, 
  3,0,1,2,19,31,0,0,0,105,0,0,0,3,0,1,19,31,0,0,0,106,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,46,176,0,128,4,17,1,77,50,0,0,1,21,9, 
  27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,51,0,0,0,160,0,0,0,1,0,1,21,1,66,0,0,0,62,61,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1, 
  240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,162,0,0,0,1,0,19,51,0,0,0,161,0,0,0,3,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,40,176,0,128,4,15,1,180,51,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,26,0,0,0,91,0,0,0,1,0,1,21, 
  1,129,0,0,0,149,61,1,0,6,0,0,0,2,0,0,0,48,32,2,128,1,96,3,128,2,96,131,128,47,104,3,128,46,192,66,128,98,80,1,128,4,15,1,105,51,0,0,15,1,168,51,0,0,15,1,8,50,0,0, 
  17,1,165,46,0,0,1,4,15,1,105,51,0,0,15,1,93,51,0,0,17,1,203,65,0,0,1,4,15,1,105,51,0,0,15,1,93,51,0,0,17,1,244,79,0,0,1,8,4,15,1,105,51,0,0,15,1,93,51,0, 
  0,17,1,167,81,0,0,1,2,19,64,0,0,0,193,0,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,48,0,0,0,156,0,0,0,3,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,64,0,0,0,192,0,0,0,1,0,1,19,26,0,0,0,90,0,0,0,2,0,1,19,8,0,0,0,25,0,0,0,1,0,1, 
  21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,23,52,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,19,25,0,0,0,71,0,0,0,1,0,1,19,25,0,0,0,70,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,110,52,0,0,17,1,219,50, 
  0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,83,0,0,0,1,0,1,19,25,0,0,0,82,0,0,0,2,0,1,21,1,124,3,0,0, 
  17,62,1,0,25,0,0,0,4,0,0,0,32,176,6,133,1,96,208,131,2,96,208,131,35,24,10,128,36,24,9,128,37,24,8,128,38,224,217,131,39,112,20,132,40,176,3,128,9,176,71,130,42,248,17,128,43,24,11,128, 
  44,112,19,128,29,224,26,128,46,224,22,128,31,176,4,129,33,224,23,128,34,112,213,129,41,224,24,129,47,144,13,128,48,144,12,128,54,176,5,128,57,112,22,128,87,144,14,128,98,104,16,128,4,15,1,214,61,0,0,15, 
  1,21,62,0,0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,214, 
  61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,204,51,0,0,1, 
  4,19,53,0,0,0,166,0,0,0,2,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1, 
  42,65,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,19,27,0,0,0,93,0,0,0,1, 
  0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,167,0,0,0,1,0,15,1,214,61,0,0,17,1,21,62,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,254,81,0, 
  0,17,1,203,65,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0, 
  0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,167,0,0,0,1,0,15,1,214,61,0,0,17,1,21,62,0,0,1,8,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1, 
  249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,167,0,0, 
  0,1,0,15,1,214,61,0,0,17,1,21,62,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0, 
  0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,58,56,0,0,17, 
  1,247,55,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,220, 
  82,0,0,17,1,80,81,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0, 
  0,15,1,220,82,0,0,17,1,18,67,0,0,1,4,15,1,214,61,0,0,15,1,21,62,0,0,15,1,42,65,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,2,21,1,66,0,0,0,62,61,1,0,3,0,0, 
  0,1,0,0,0,2,8,130,128,1,8,2,128,58,240,0,128,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,162,0,0,0,1,0,19,34,0,0,0,112,0,0,0,2,0,1,8,2,21,7,47,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,44,56,1,128,125,208,0,128,4,19,9,0,0,0,32,0,0,0,3,0,1,4,17,1,133,56,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,2,21,1,187,0,0,0,20,63,1,0,6,0,0,0,2,0,0,0,48,208,2,128,1,208,5,128,2,208,133,128,47,208,4,128,46,208,67,128,58,80,1,128,4,19,35,0,0,0,113,0,0, 
  0,1,0,19,52,0,0,0,162,0,0,0,1,0,15,1,72,60,0,0,15,1,135,60,0,0,15,1,202,61,0,0,17,1,77,57,0,0,1,4,15,1,72,60,0,0,15,1,135,60,0,0,15,1,202,61,0,0,15,1, 
  65,57,0,0,17,1,203,65,0,0,1,4,15,1,72,60,0,0,15,1,135,60,0,0,15,1,202,61,0,0,15,1,65,57,0,0,17,1,244,79,0,0,1,4,15,1,72,60,0,0,15,1,135,60,0,0,15,1,202,61, 
  0,0,15,1,65,57,0,0,17,1,167,81,0,0,1,8,2,19,33,0,0,0,111,0,0,0,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,4,17,1,146,57,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,33,0,0,0,110,0,0,0,1,0,1,21,1,111,2,0,0,84,64,1,0,24,0,0,0,4,0,0,0,32,208,4,133, 
  1,128,203,131,2,128,203,131,35,16,6,128,36,8,16,128,37,224,10,128,38,240,199,131,39,112,197,131,40,48,4,128,41,192,17,128,42,168,16,128,11,144,200,129,44,104,15,128,29,160,9,128,46,64,10,128,31,88,13,129, 
  33,144,3,128,34,136,139,129,43,96,18,128,47,176,6,128,48,0,9,128,54,80,7,128,87,248,13,128,98,40,12,128,4,15,1,60,60,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,60,60,0,0,15, 
  1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,60,60,0,0,15,1,220,82,0,0,17,1,204,51,0,0,1,4,15,1,60,60,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,60,60,0,0,15, 
  1,220,82,0,0,17,1,75,80,0,0,1,4,15,1,60,60,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,60,60,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,60,60,0,0,15, 
  1,220,82,0,0,17,1,18,67,0,0,1,4,15,1,48,60,0,0,17,1,2,60,0,0,1,4,15,1,60,60,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,60,60,0,0,15,1,97,82,0,0,17, 
  1,10,82,0,0,1,4,15,1,60,60,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,60,60,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,8,4,15,1,60,60,0,0,15,1,220,82,0,0, 
  17,1,35,52,0,0,1,4,15,1,60,60,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,60,60,0,0,15,1,220,82,0,0,17,1, 
  249,80,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,33,0,0,0,108,0,0,0,3,0,1,4,15,1,60,60,0,0,15,1, 
  145,79,0,0,17,1,198,72,0,0,1,4,15,1,60,60,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,33,0,0,0,108, 
  0,0,0,3,0,1,4,15,1,60,60,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,33,0,0,0,108,0,0,0,3,0, 
  1,2,21,1,45,0,0,0,22,65,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,57,248,0,128,8,4,15,1,58,56,0,0,17,1,247,55,0,0,1,2,19,33,0,0,0,109,0,0,0,3,0,1, 
  19,33,0,0,0,108,0,0,0,3,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,9,0,0,0,31,0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,10,176,0,128,15,1,135,60,0,0,17,1,171,60,0,0,1,2,21,7,42,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,44,208,0,128,125,16,1,128,12,17,1,248,60,0,0,1,12,17,1,133,12,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10, 
  12,17,1,133,12,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,50,61,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,2,21,1,139,0,0,0,20,63,1,0,6,0,0,0,2,0,0,0,48,184,3,128,1,80,1,128,2,80,129,128,47,248,1,128,46,88,65,128,58,152,2,128,8,4,15,1,190,61,0,0,15,1,65,57, 
  0,0,17,1,244,79,0,0,1,4,15,1,190,61,0,0,15,1,65,57,0,0,17,1,167,81,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,19,52,0,0,0,162,0,0,0,1,0,15,1,190,61,0,0,17,1, 
  77,57,0,0,1,4,15,1,190,61,0,0,15,1,65,57,0,0,17,1,203,65,0,0,1,2,19,10,0,0,0,34,0,0,0,3,0,1,19,10,0,0,0,33,0,0,0,1,0,1,21,7,35,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,125,176,0,128,4,19,53,0,0,0,165,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,0,35,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,15,1,21,62,0,0,17,1,57,62,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,125,16,1,128,59,208,192,127,12,17,1,134,62, 
  0,0,1,12,17,1,133,12,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,12,17,1,133,12,0,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,59,176,0,128,4,17,1,192,62,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,93,2,0,0,135,60,1,0,23,0,0,0,4,0, 
  0,0,32,48,209,132,1,216,204,131,2,216,204,131,35,88,10,128,36,176,14,128,37,144,6,128,38,176,132,131,39,224,140,131,40,240,15,128,41,16,4,128,42,48,7,128,43,208,17,128,44,152,11,128,29,56,12,128,46,112, 
  3,128,31,240,197,128,33,80,15,128,34,248,74,129,47,80,5,128,48,144,16,128,54,72,8,128,87,232,8,128,98,128,13,128,4,15,1,30,65,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,30,65,0, 
  0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,18,67,0,0,1,4,15,1,30,65,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,30,65,0, 
  0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0, 
  0,0,168,0,0,0,3,0,1,4,15,1,30,65,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0, 
  0,1,0,19,54,0,0,0,168,0,0,0,3,0,1,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,30,65, 
  0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,4,15,1,30,65,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,8,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,30, 
  65,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,30,65,0, 
  0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,30,65,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,30,65,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,30,65,0, 
  0,15,1,220,82,0,0,17,1,204,51,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,54,0,0,0,168,0,0,0,3,0,1,2,19,54,0,0,0,168,0,0,0,3, 
  0,1,19,54,0,0,0,167,0,0,0,1,0,1,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,53,248,128,128,9,88,193,127,85,240,0,128,1,19,85,0,0,0,3,1,0,0,2,0,1,19,53,0, 
  0,0,163,0,0,0,1,0,17,1,54,65,0,0,1,2,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,191,65,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,89,0,0,0,1,0,1,19,25,0,0,0,88,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,60,176,0,128,4,15,1,6,67,0,0,17,1,22,66,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,30,0,0,0,104,0,0,0,1,0,1,21,1, 
  50,0,0,0,123,65,1,0,3,0,0,0,1,0,0,0,2,136,1,128,1,136,65,128,87,240,0,128,4,19,49,0,0,0,157,0,0,0,1,0,17,1,73,66,0,0,1,8,2,21,7,47,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,44,56,65,128,62,208,0,128,4,19,50,0,0,0,159,0,0,0,3,0,1,4,17,1,148,66,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,2,21,1,50,0,0,0,123,65,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,87,248,0,128,8,4,19,49,0,0,0,157,0,0,0,1,0,17,1,199,66,0,0,1,2,21,7,35,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,4,19,50,0,0,0,158,0,0,0,5,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,30, 
  0,0,0,101,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,93,67,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,77,0,0,0,1,0,1,19,25,0,0,0,76,0,0,0,2,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4, 
  17,1,163,67,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,78,2,0,0,135,60,1,0,23,0,0,0,4,0,0,0,32,240,207,132,1,24,205, 
  131,2,24,205,131,35,232,10,128,36,120,12,128,37,208,17,128,38,104,136,131,39,192,141,131,40,96,14,128,41,16,4,128,42,0,15,128,43,136,11,128,44,48,17,128,29,72,10,128,46,200,7,128,31,112,195,128,33,144,16, 
  128,34,40,71,129,47,32,13,128,48,8,9,128,54,168,9,128,87,176,4,128,98,248,5,128,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,242,69,0,0,15,1,192,51,0,0,17,1, 
  144,50,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,242,69,0,0,1,4,15,1,242,69,0,0,15,1,249,46,0,0,15, 
  1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,242,69,0,0,15,1,254,81,0,0,17,1,244, 
  79,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,18,67,0,0,1,4,15,1,242,69,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,242,69,0,0,15,1,186,72,0,0,17,1,105, 
  67,0,0,1,4,15,1,242,69,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0, 
  0,26,0,0,0,1,0,17,1,242,69,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,8,4,15,1,242,69,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,242,69, 
  0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1, 
  242,69,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,204,51,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,242,69,0,0,15,1,145,79,0,0,17,1, 
  198,72,0,0,1,4,15,1,242,69,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,44,70,0,0,1,21,9,27,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,78,2,0,0,135,60,1,0,23,0,0,0,4,0,0,0,32,16,202,132,1,8,202,131,2,8,202,131,35,48,17,128,36,112, 
  3,128,37,152,6,128,38,224,140,131,39,144,144,131,40,16,4,128,41,248,5,128,42,80,11,128,43,216,7,128,44,56,7,128,29,176,10,128,46,208,17,128,31,200,200,128,33,128,13,128,34,32,78,129,47,64,12,128,48,104, 
  9,128,54,240,15,128,87,176,4,128,98,192,14,128,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,19,49,0,0,0, 
  157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,123,72,0,0,1,4,15,1,123,72,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,123,72, 
  0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,4,15,1,123,72,0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1, 
  123,72,0,0,1,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,123,72,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1,8,4,15,1,123,72,0,0,15,1,220,82,0,0,17, 
  1,204,51,0,0,1,4,15,1,123,72,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,17,1,123,72,0,0,1,4,15,1,123, 
  72,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,18,67,0,0,1,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,123, 
  72,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,123,72,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,123,72,0, 
  0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,123,72,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,15,1,123,72,0, 
  0,15,1,254,81,0,0,17,1,244,79,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,32,0,0,0,107,0,0,0,6,0,1,21,9,27,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,19,8,0,0,0,30,0,0,0,1,0,1,21,1,106,3,0,0,182,65,1,0,24,0,0,0,4,0,0,0,32,216,20,133,1,96,201,131,2,96,201,131, 
  35,224,13,128,36,224,12,128,37,216,22,128,38,104,201,131,39,144,195,131,40,224,14,128,41,104,10,128,42,104,11,128,43,216,25,128,44,216,24,128,29,216,87,129,46,224,16,128,31,144,5,129,33,144,4,128,34,144,134,129, 
  45,112,19,128,47,224,15,128,48,216,19,128,54,216,21,128,87,144,7,128,98,224,17,128,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,49, 
  76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1, 
  4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0, 
  26,0,0,0,1,0,19,29,0,0,0,97,0,0,0,1,0,15,1,49,76,0,0,17,1,112,76,0,0,1,8,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,18,67, 
  0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,29, 
  0,0,0,97,0,0,0,1,0,15,1,49,76,0,0,17,1,112,76,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,49,76,0, 
  0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15, 
  1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,254,81,0,0,17,1,244,79,0, 
  0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,19,28,0,0,0,96,0, 
  0,0,2,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,220,82, 
  0,0,17,1,204,51,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0, 
  15,1,220,82,0,0,17,1,162,80,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1,133,79,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,4,15,1,49,76,0,0,15,1,112,76,0,0,15,1, 
  133,79,0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,29,0,0,0,97,0,0,0,1,0,15,1,49,76,0,0,17,1,112, 
  76,0,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,93,176,0,128,4,19,28,0,0,0,95,0,0,0,3,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,208,0,128,8,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,29,176,0,128,15,1,112,76,0,0,17,1,148,76,0,0,1,2,21,7,42,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,44,16,1,128,93,208,0,128,12,17,1,133,12,0,0,1,12,17,1,225,76,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,10,12,17,1,133,12,0,0, 
  1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,4,17,1,27,77,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21, 
  1,93,2,0,0,135,60,1,0,23,0,0,0,4,0,0,0,32,152,207,132,1,0,201,131,2,0,201,131,35,160,12,128,36,64,13,128,37,80,5,128,38,56,144,131,39,72,146,131,40,16,4,128,41,48,7,128,42,128,14, 
  128,43,232,10,128,44,72,10,128,29,8,9,128,46,176,4,128,31,224,205,128,33,240,5,128,34,144,70,129,47,0,12,128,48,112,3,128,54,168,9,128,87,216,16,128,98,208,7,128,4,15,1,121,79,0,0,15,1,254,81, 
  0,0,17,1,203,65,0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,121,79,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,121,79,0,0,15,1,220,82, 
  0,0,17,1,162,80,0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,121,79,0,0,15,1,192,51, 
  0,0,17,1,144,50,0,0,1,4,15,1,121,79,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,8,4,15,1,121,79,0,0,15,1,97,82,0, 
  0,17,1,10,82,0,0,1,4,15,1,121,79,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,121,79,0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0, 
  19,8,0,0,0,26,0,0,0,1,0,19,29,0,0,0,98,0,0,0,3,0,1,4,15,1,121,79,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,75,80, 
  0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0, 
  26,0,0,0,1,0,19,29,0,0,0,98,0,0,0,3,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,204,51,0,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,18,67,0,0,1,4,19, 
  49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,29,0,0,0,98,0,0,0,3,0,1,4,15,1,121,79,0,0,15,1,220,82,0,0,17,1,109, 
  82,0,0,1,2,19,29,0,0,0,98,0,0,0,3,0,1,19,29,0,0,0,97,0,0,0,1,0,1,19,8,0,0,0,27,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40, 
  176,0,128,4,15,1,232,79,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,73,0,0,0,1,0,1,19,25,0,0, 
  0,72,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,63,80,0,0,17,1,22,66,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,208,0,128,8,19,30,0,0,0,102,0,0,0,1,0,1,19,30,0,0,0,99,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1, 
  150,80,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,75,0,0,0,1,0,1,19,25,0,0,0,74,0,0,0,2, 
  0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,237,80,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,25,0,0,0,85,0,0,0,1,0,1,19,25,0,0,0,84,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,68,81,0,0,17,1, 
  219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,79,0,0,0,1,0,1,19,25,0,0,0,78,0,0,0,2,0,1,21,7,36,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,155,81,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19, 
  25,0,0,0,87,0,0,0,1,0,1,19,25,0,0,0,86,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,60,176,0,128,4,15,1,242,81,0,0,17,1,22,66,0,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,30,0,0,0,103,0,0,0,1,0,1,19,30,0,0,0,100,0,0,0,2,0,1,19,8,0,0,0,28,0,0,0,1, 
  0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40,176,0,128,4,15,1,85,82,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128, 
  5,208,0,128,8,19,24,0,0,0,69,0,0,0,1,0,1,19,24,0,0,0,68,0,0,0,2,0,1,19,8,0,0,0,23,0,0,0,1,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,40, 
  176,0,128,4,15,1,184,82,0,0,17,1,219,50,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,25,0,0,0,81,0,0,0,1,0,1,19,25,0,0, 
  0,80,0,0,0,2,0,1,19,85,0,0,0,3,1,0,0,2,0,1,19,53,0,0,0,164,0,0,0,1,0,1,19,8,0,0,0,24,0,0,0,1,0,1,19,60,0,0,0,183,0,0,0,2,0,1,21,1,47,0, 
  0,0,120,66,1,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,48,65,127,118,48,1,128,12,17,1,48,83,0,0,1,10,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,69,0, 
  0,0,195,67,1,0,5,0,0,0,2,0,0,0,115,232,1,128,1,48,1,128,2,48,129,128,11,120,65,127,118,56,1,128,8,4,17,1,114,85,0,0,1,4,15,1,177,84,0,0,17,1,74,84,0,0,1,4,17,1, 
  118,83,0,0,1,2,21,1,57,0,0,0,32,68,1,0,4,0,0,0,2,0,0,0,118,128,1,128,1,192,1,128,2,192,129,127,11,16,1,128,4,15,1,18,84,0,0,17,1,74,84,0,0,1,4,17,1,187,83,0, 
  0,1,8,19,81,0,0,0,243,0,0,0,2,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,6,84,0,0,17,1,74,84,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,248,0,0,0,3,0,1,19,81,0,0,0,247,0,0,0,4,0,1,21,1,44,0,0,0,41,69,1,0,3,0,0,0,1,0,0,0, 
  2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,249,0,0,0,4,0,1,19,81,0,0,0,239,0,0,0,3,0,1,21,1,39,0,0,0,203,69,1,0,3,0,0,0,1,0,0,0,2,48,1,128, 
  1,48,65,128,129,240,0,128,4,17,1,114,84,0,0,1,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,125,176,0,128,4,19,96,0,0,0,22,1,0,0,3,0,1,21,9,27,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,51,0,0,0,80,70,1,0,4,0,0,0,2,0,0,0,118,88,1,128,1,80,1,128,2,80,129,127,115,16,1,128,4,17,1,58,85, 
  0,0,1,8,4,17,1,240,84,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,251,0,0,0,4,0,1,21, 
  9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,250,0,0,0,3,0,1,21,1,44,0,0,0,41,69,1,0,3,0,0,0,1,0,0,0,2,240,128,128, 
  1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,252,0,0,0,4,0,1,19,81,0,0,0,246,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127, 
  4,17,1,19,86,0,0,1,4,15,1,201,85,0,0,17,1,74,84,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,244,0,0,0,2,0, 
  1,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,63,176,0,128,4,19,81,0,0,0,245,0,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,19,81,0,0,0,240,0,0,0,3,0,1,21,7,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,15,1,94,86,0,0,17,1,74,84,0,0,1,21,9,27,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,242,0,0,0,3,0,1,19,81,0,0,0,238,0,0,0,4,0,1,21,1,43,0,0,0,253,70,1,0,4,0,0,0,2,0,0,0, 
  128,16,1,128,1,80,1,128,2,80,1,128,127,16,1,128,12,17,1,162,86,0,0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,21,1,51,0,0,0,24,72,1,0,4,0,0,0,2,0,0,0,128,24,1,128, 
  1,16,1,128,2,16,1,128,127,88,1,128,8,4,17,1,215,87,0,0,1,4,17,1,214,86,0,0,1,2,21,1,130,0,0,0,136,72,1,0,6,0,0,0,2,0,0,0,114,80,65,129,1,160,195,128,2,160,131,127, 
  51,168,3,128,125,192,1,128,126,176,2,128,4,15,1,152,87,0,0,17,1,238,89,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,17,1,89,87,0,0,1,4,19,94,0, 
  0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,17,1,89,87,0,0,1,8,4,19,95,0,0,0,20,1,0,0,3,0,1,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41, 
  176,0,128,4,19,95,0,0,0,18,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,41,176,0,128,4,19,95,0,0,0,19,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,1,130,0,0,0,136,72,1,0,6,0,0, 
  0,2,0,0,0,114,176,66,129,1,64,194,128,2,64,130,127,51,72,2,128,125,32,3,128,126,80,1,128,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,17,1,153,88,0,0,1,8, 
  4,19,95,0,0,0,17,1,0,0,3,0,1,4,15,1,90,88,0,0,17,1,238,89,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,17,1,153,88,0,0,1,2,21,7, 
  35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,16,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8, 
  2,21,7,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,41,176,0,128,4,19,95,0,0,0,15,1,0,0,4,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208, 
  0,128,8,2,21,1,52,0,0,0,242,72,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56,1,128,8,4,19,59,0,0,0,179,0,0,0,3,0,1,2,21,0,224, 
  0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,61,72,4,128,81,192,66,130,62,224,3,128,11,112,6,128,60,176,4,128,45,56,198,126,46,208,5,127,47,64,69,128,63,80,67,128,95,88,2,128,97,240,1,128,15, 
  1,13,89,0,0,17,1,106,86,0,0,1,15,1,13,89,0,0,17,1,244,82,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,13,89,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,13,89,0,0, 
  1,15,1,13,89,0,0,17,1,46,43,0,0,1,15,1,13,89,0,0,17,1,9,23,0,0,1,19,46,0,0,0,151,0,0,0,1,0,17,1,13,89,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,13,89, 
  0,0,1,15,1,13,89,0,0,17,1,168,19,0,0,1,17,1,165,115,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,13,89,0,0,1,2,21,1,163,0,0,0,68,73,1,0,10,0,0,0,3,0,0,0, 
  112,168,2,128,1,160,2,130,2,160,130,129,107,120,3,128,108,208,1,128,109,56,2,128,110,224,3,128,111,16,3,128,106,176,4,128,113,72,4,128,4,19,80,0,0,0,232,0,0,0,2,0,1,4,19,80,0,0,0,236, 
  0,0,0,2,0,1,8,4,19,80,0,0,0,235,0,0,0,2,0,1,4,19,80,0,0,0,234,0,0,0,2,0,1,4,19,80,0,0,0,231,0,0,0,2,0,1,4,19,80,0,0,0,230,0,0,0,2,0,1,4, 
  19,80,0,0,0,237,0,0,0,2,0,1,4,19,80,0,0,0,233,0,0,0,2,0,1,2,19,59,0,0,0,182,0,0,0,1,0,1,19,97,0,0,0,23,1,0,0,1,0,1,21,1,80,0,0,0,75,75,1,0, 
  4,0,0,0,2,0,0,0,53,16,1,128,1,120,194,127,2,120,66,128,58,176,1,128,4,15,1,79,91,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,251,90, 
  0,0,17,1,115,178,0,0,1,8,2,21,0,83,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,168,65,128,63,24,65,128,79,16,1,128,11,8,66,127,1,19,47,0,0,0,155,0,0,0,1,0,17,1,251, 
  90,0,0,1,19,79,0,0,0,229,0,0,0,2,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,251,90,0,0,1,2,19,79,0,0,0,229,0,0,0,2,0,1,19,97,0,0,0,24,1,0,0,1,0,1,19, 
  59,0,0,0,181,0,0,0,1,0,1,21,1,178,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,216,9,130,1,112,199,128,2,112,135,127,94,160,202,129,105,0,12,128,53,224,5,129,6,112,71,127,7,112,7, 
  128,114,80,4,128,125,120,7,128,126,240,1,128,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1, 
  204,99,0,0,15,1,252,99,0,0,15,1,78,115,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,204,99,0,0,15,1,252,99,0,0,15,1,78,115,0,0,15,1,78,174,0,0,15,1,102,115,0,0, 
  15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,204,99,0,0,15,1,252,99,0,0,15,1,78,115,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178, 
  0,0,17,1,124,13,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,204,99,0, 
  0,15,1,252,99,0,0,15,1,78,115,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,145,93,0,0,17,1,38,93,0,0,1,4,15,1,204,99,0,0,15,1, 
  252,99,0,0,15,1,78,115,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,204,99,0,0,15,1,252,99,0,0,15,1,78,115,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,2,21,1,39,0,0,0,75,76,1,0,3,0,0,0,1,0,0,0,2,240,0,128,1,240,64,128,97,248,0,128,8,4,17,1, 
  89,93,0,0,1,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,62,61,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,58,248,0,128,8,4,19,35,0,0,0,113,0,0,0,1,0, 
  19,63,0,0,0,191,0,0,0,3,0,1,2,21,0,162,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,96,67,129,81,208,66,129,82,200,2,128,11,128,68,128,83,96,2,128,95,248,1,128,97,144,1,128,47, 
  240,67,126,15,1,145,93,0,0,17,1,90,98,0,0,1,15,1,145,93,0,0,17,1,141,96,0,0,1,15,1,145,93,0,0,17,1,52,94,0,0,1,1,19,83,0,0,0,0,1,0,0,1,0,17,1,145,93,0,0, 
  1,19,47,0,0,0,155,0,0,0,1,0,17,1,145,93,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,145,93,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,145,93,0,0,1,2,21,1,124,1, 
  0,0,0,77,1,0,12,0,0,0,3,0,0,0,120,72,4,128,1,16,195,129,2,16,67,128,58,24,135,129,94,16,194,129,53,24,67,129,6,16,131,127,7,16,3,128,105,184,4,128,114,232,5,128,125,224,9,128,126,224, 
  7,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,8,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0, 
  15,1,59,178,0,0,17,1,124,13,0,0,1,4,15,1,241,95,0,0,17,1,177,95,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1, 
  170,90,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,241,95,0, 
  0,17,1,38,93,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15, 
  1,78,174,0,0,17,1,102,115,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129, 
  96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,2,21,1,52,0,0,0,169,77,1,0,5,0,0,0,2,0,0,0,6,152,1,128,1,152,1,128,2,152,129,127,7,152,65,128,119,48,1,128,4,19,82,0,0, 
  0,254,0,0,0,4,0,1,8,19,82,0,0,0,255,0,0,0,3,0,1,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,67,129,81,144,66,129,82,144,1,128,11,240,66,128,83,144,1,128,95, 
  128,3,128,97,40,2,128,47,152,65,126,1,19,97,0,0,0,23,1,0,0,1,0,17,1,241,95,0,0,1,15,1,241,95,0,0,17,1,90,98,0,0,1,19,83,0,0,0,1,1,0,0,2,0,1,19,47,0,0,0, 
  154,0,0,0,1,0,17,1,241,95,0,0,1,15,1,241,95,0,0,17,1,141,96,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,241,95,0,0,1,2,19,83,0,0,0,1,1,0,0,2,0,1,21,1,47, 
  0,0,0,251,77,1,0,5,0,0,0,2,0,0,0,115,56,1,128,1,48,1,128,2,48,129,128,11,56,65,127,118,56,1,128,10,12,17,1,201,96,0,0,1,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,69, 
  0,0,0,195,67,1,0,5,0,0,0,2,0,0,0,115,232,1,128,1,224,1,128,2,224,129,128,11,48,65,127,118,160,1,128,4,15,1,227,97,0,0,17,1,74,84,0,0,1,4,17,1,140,97,0,0,1,8,4,17, 
  1,15,97,0,0,1,2,21,1,57,0,0,0,186,78,1,0,4,0,0,0,2,0,0,0,118,16,1,128,1,80,1,128,2,80,129,127,11,88,1,128,4,17,1,187,83,0,0,1,8,4,15,1,84,97,0,0,17,1,74, 
  84,0,0,1,19,81,0,0,0,243,0,0,0,2,0,1,21,1,44,0,0,0,69,79,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,249,0,0,0,4,0,1, 
  19,81,0,0,0,239,0,0,0,3,0,1,21,7,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,201,85,0,0,17,1,74,84,0,0,1,4,17,1,19,86,0,0,1, 
  21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,19,81,0,0,0,244,0,0,0,2,0,1,21,1,51,0,0,0,197,79,1,0,4,0,0,0,2,0,0,0,118,16,1, 
  128,1,144,1,128,2,144,129,127,115,80,1,128,4,17,1,240,84,0,0,1,4,17,1,34,98,0,0,1,8,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,69,79,1,0,3,0,0,0,1,0,0,0, 
  2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,252,0,0,0,4,0,1,19,81,0,0,0,246,0,0,0,3,0,1,21,1,43,0,0,0,80,80,1,0,4,0,0,0,2,0,0,0,128,16,1,128, 
  1,80,1,128,2,80,1,128,127,16,1,128,12,17,1,146,98,0,0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,21,1,51,0,0,0,24,72,1,0,4,0,0,0,2,0,0,0,128,88,1,128,1,80,1,128, 
  2,80,1,128,127,16,1,128,4,17,1,73,99,0,0,1,8,4,17,1,198,98,0,0,1,2,21,1,130,0,0,0,136,72,1,0,6,0,0,0,2,0,0,0,114,160,67,129,1,152,195,128,2,152,131,127,51,48,3,128, 
  125,80,1,128,126,64,2,128,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,17,1,153,88,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1, 
  0,17,1,153,88,0,0,1,4,19,95,0,0,0,17,1,0,0,3,0,1,8,4,15,1,90,88,0,0,17,1,238,89,0,0,1,2,21,1,130,0,0,0,136,72,1,0,6,0,0,0,2,0,0,0,114,176,66,129,1, 
  184,193,128,2,184,129,127,51,80,1,128,125,32,3,128,126,192,1,128,4,19,95,0,0,0,20,1,0,0,3,0,1,8,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,17,1,89,87, 
  0,0,1,4,15,1,152,87,0,0,17,1,238,89,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,17,1,89,87,0,0,1,2,21,1,47,0,0,0,16,81,1,0,5,0, 
  0,0,2,0,0,0,120,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,177,95,0,0,1,2,21,0,157,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,200,131,128,81,168, 
  66,129,63,56,131,128,11,88,68,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,252,99,0,0,17,1,90,98,0,0,1,15,1,252,99,0,0,17,1,141,96,0,0,1,15,1,252,99,0,0,17,1,154,100,0,0, 
  1,19,83,0,0,0,0,1,0,0,1,0,17,1,252,99,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,252,99,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,252,99,0,0,1,19,47,0,0,0, 
  154,0,0,0,1,0,17,1,252,99,0,0,1,2,21,1,119,0,0,0,0,77,1,0,11,0,0,0,3,0,0,0,58,248,2,130,1,48,194,128,2,48,130,127,94,56,195,129,105,184,2,128,53,120,3,129,6,48,66,127, 
  7,48,2,128,114,240,1,128,125,120,2,128,126,56,2,128,12,17,1,87,113,0,0,1,10,12,17,1,96,111,0,0,1,12,17,1,105,109,0,0,1,12,17,1,114,107,0,0,1,12,17,1,123,105,0,0,1,12,17,1, 
  132,103,0,0,1,12,17,1,25,101,0,0,1,12,17,1,133,12,0,0,1,21,1,106,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,136,6,130,1,80,197,128,2,80,133,127,94,32,195,129,105,32,4,128,53, 
  88,5,129,6,80,69,127,7,80,5,128,114,240,1,128,125,80,7,128,126,80,9,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0, 
  1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1, 
  103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,35,0,0,0,113,0,0,0, 
  1,0,15,1,132,102,0,0,17,1,115,178,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0, 
  15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1, 
  0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64, 
  66,128,95,216,1,128,97,112,1,128,15,1,132,102,0,0,17,1,76,103,0,0,1,15,1,132,102,0,0,17,1,16,103,0,0,1,1,19,83,0,0,0,1,1,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0, 
  17,1,132,102,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,132,102,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,132,102,0,0,1,2,21,1,47,0,0,0,251,77,1,0,5,0,0,0,2,0, 
  0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,48,65,127,118,48,1,128,12,17,1,48,83,0,0,1,10,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,43,0,0,0,80,80,1,0,4,0,0,0,2,0, 
  0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,16,1,128,12,17,1,162,86,0,0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,21,1,106,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,32, 
  4,130,1,24,198,128,2,24,134,127,94,32,195,129,105,232,4,128,53,32,6,129,6,24,70,127,7,24,6,128,114,240,1,128,125,80,7,128,126,80,9,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0, 
  0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0, 
  0,0,1,0,15,1,239,104,0,0,17,1,115,178,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,129, 
  96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0, 
  0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1, 
  0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,239,104,0,0,17,1,76,103,0,0,1,15,1,239,104,0,0,17,1,16,103,0,0,1,1,19,83,0,0, 
  0,1,1,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,239,104,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,239,104,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,239,104,0, 
  0,1,2,21,1,106,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,136,6,130,1,80,197,128,2,80,133,127,94,32,195,129,105,32,4,128,53,88,5,129,6,80,69,127,7,80,5,128,114,240,1,128,125,80,7, 
  128,126,80,9,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115, 
  0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,129,96,0, 
  0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,230,106,0,0,17,1,115,178,0,0,1,4,19,93, 
  0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0, 
  1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1, 
  102,115,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,230,106,0,0,17,1, 
  76,103,0,0,1,15,1,230,106,0,0,17,1,16,103,0,0,1,1,19,83,0,0,0,1,1,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,230,106,0,0,1,19,97,0,0,0,23,1,0,0,1,0, 
  17,1,230,106,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,230,106,0,0,1,2,21,1,106,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,80,5,130,1,24,198,128,2,24,134,127,94,32,195,129, 
  105,32,4,128,53,32,6,129,6,24,70,127,7,24,6,128,114,240,1,128,125,80,7,128,126,80,9,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17, 
  1,238,89,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91, 
  91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,221,108,0,0,17,1,115,178,0,0,1,8,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0, 
  0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24, 
  1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97, 
  0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11, 
  200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,221,108,0,0,17,1,76,103,0,0,1,15,1,221,108,0,0,17,1,16,103,0,0,1,1,19,83,0,0,0,1,1,0,0,2,0,1,19,47,0,0,0,155, 
  0,0,0,1,0,17,1,221,108,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,221,108,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,221,108,0,0,1,2,21,1,106,1,0,0,173,75,1,0,11, 
  0,0,0,3,0,0,0,58,136,10,130,1,80,197,128,2,80,133,127,94,32,195,129,105,32,4,128,53,88,5,129,6,80,69,127,7,80,5,128,114,240,1,128,125,136,6,128,126,136,8,128,4,15,1,129,96,0,0,15,1, 
  78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0, 
  1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15, 
  1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0, 
  0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0, 
  0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,212,110,0,0,17,1,115,178,0,0,1,2,21,0,139,0,0,0,255,255, 
  255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,212,110,0,0,17,1,76,103,0,0,1,15,1,212,110,0,0,17,1,16, 
  103,0,0,1,1,19,83,0,0,0,1,1,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,212,110,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,212,110,0,0,1,19,47,0,0,0,154,0, 
  0,0,1,0,17,1,212,110,0,0,1,2,21,1,106,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,136,6,130,1,80,197,128,2,80,133,127,94,32,195,129,105,32,4,128,53,88,5,129,6,80,69,127,7,80, 
  5,128,114,240,1,128,125,80,7,128,126,80,9,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,129,96,0,0,15, 
  1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0, 
  0,1,8,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,203,112,0,0,17, 
  1,115,178,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78, 
  174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0, 
  0,15,1,78,174,0,0,17,1,102,115,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128,81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1, 
  128,15,1,203,112,0,0,17,1,76,103,0,0,1,15,1,203,112,0,0,17,1,16,103,0,0,1,1,19,83,0,0,0,1,1,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,203,112,0,0,1,19,97, 
  0,0,0,23,1,0,0,1,0,17,1,203,112,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,203,112,0,0,1,2,21,1,106,1,0,0,173,75,1,0,11,0,0,0,3,0,0,0,58,136,6,130,1,80,197, 
  128,2,80,133,127,94,32,195,129,105,32,4,128,53,88,5,129,6,80,69,127,7,80,5,128,114,240,1,128,125,80,7,128,126,80,9,128,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91, 
  0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,129,96,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,129,96,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124, 
  13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,194,114,0,0,17,1,115,178,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0, 
  0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0, 
  0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,129,96,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,2,21,0,139,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,56,131,128, 
  81,72,66,129,63,168,130,128,11,200,67,127,83,64,66,128,95,216,1,128,97,112,1,128,15,1,194,114,0,0,17,1,76,103,0,0,1,15,1,194,114,0,0,17,1,16,103,0,0,1,1,19,83,0,0,0,1,1,0,0, 
  2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,194,114,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,194,114,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,194,114,0,0,1,2,19,83, 
  0,0,0,0,1,0,0,1,0,1,19,61,0,0,0,186,0,0,0,1,0,1,21,1,51,0,0,0,24,72,1,0,4,0,0,0,2,0,0,0,128,24,1,128,1,16,1,128,2,16,1,128,127,88,1,128,8,4,17,1, 
  215,87,0,0,1,4,17,1,214,86,0,0,1,19,95,0,0,0,21,1,0,0,1,0,1,21,1,52,0,0,0,242,72,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85, 
  56,1,128,8,4,19,47,0,0,0,153,0,0,0,3,0,1,2,19,45,0,0,0,150,0,0,0,1,0,1,21,0,213,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,62,192,3,128,81,160,2,130,63,48,131,129, 
  11,24,6,128,60,144,4,128,61,40,4,128,46,176,133,126,47,32,197,126,95,56,2,128,97,208,1,128,15,1,230,115,0,0,17,1,159,158,0,0,1,15,1,230,115,0,0,17,1,99,158,0,0,1,19,61,0,0,0,185, 
  0,0,0,1,0,17,1,230,115,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,230,115,0,0,1,15,1,230,115,0,0,17,1,39,158,0,0,1,15,1,230,115,0,0,17,1,243,116,0,0,1,19,46,0,0, 
  0,151,0,0,0,1,0,17,1,230,115,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,230,115,0,0,1,15,1,230,115,0,0,17,1,188,116,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,230,115, 
  0,0,1,2,21,1,47,0,0,0,98,81,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,228,19,0,0,1,12,17,1,133,12,0,0,1,21, 
  1,143,0,0,0,41,82,1,0,13,0,0,0,3,0,0,0,58,248,3,130,1,176,2,130,2,176,130,127,94,48,66,130,44,184,2,128,53,120,131,129,6,176,66,127,7,176,2,128,98,56,132,128,105,56,3,128,114,248,2, 
  128,125,112,2,128,126,184,3,128,12,17,1,142,156,0,0,1,12,17,1,101,154,0,0,1,10,12,17,1,204,152,0,0,1,12,17,1,51,151,0,0,1,12,17,1,154,149,0,0,1,12,17,1,1,148,0,0,1,12,17, 
  1,216,145,0,0,1,6,17,1,184,119,0,0,1,12,17,1,143,117,0,0,1,12,19,62,0,0,0,190,0,0,0,1,0,1,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,0,8,130,1,200,8, 
  130,2,200,136,127,94,208,69,130,44,96,3,128,53,144,139,129,6,200,72,127,7,200,8,128,98,208,136,128,105,48,2,128,114,208,6,128,125,144,9,128,126,208,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0, 
  19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0, 
  15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,40,119,0,0,17,1,38,93,0,0,1,8,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0, 
  0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,136,195,128,95,144,2,128,11,248,1,128,97,144, 
  1,128,61,136,2,128,62,136,2,128,47,248,66,126,15,1,40,119,0,0,17,1,159,158,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,40,119,0,0,1,1,15,1,40,119,0,0,17,1,99,158,0,0,1,19, 
  97,0,0,0,23,1,0,0,1,0,17,1,40,119,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,40,119,0,0,1,2,21,1,188,0,0,0,77,83,1,0,17,0, 
  0,0,4,0,0,0,128,176,2,128,1,176,195,129,2,176,3,130,3,120,69,131,4,120,5,128,53,120,4,128,6,176,3,128,7,176,3,128,97,48,3,128,105,240,2,128,98,56,196,128,11,248,4,128,44,112,3,128,114,56, 
  5,128,94,184,4,128,127,184,3,128,115,248,3,128,12,17,1,175,143,0,0,1,12,17,1,134,141,0,0,1,12,17,1,93,139,0,0,1,12,17,1,52,137,0,0,1,10,12,17,1,226,132,0,0,1,12,17,1,185,130, 
  0,0,1,12,17,1,32,129,0,0,1,12,17,1,247,126,0,0,1,12,17,1,206,124,0,0,1,12,17,1,165,122,0,0,1,12,17,1,124,120,0,0,1,12,19,62,0,0,0,190,0,0,0,1,0,1,12,17,1,11, 
  135,0,0,1,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,144,6,130,1,136,9,130,2,136,137,127,94,88,71,130,44,32,4,128,53,144,139,129,6,136,73,127,7,136,9,128,98,48,130,128,105,240, 
  2,128,114,88,8,128,125,144,9,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91, 
  91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0, 
  0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,21,122,0,0,17,1,38,93,0,0,1,4, 
  15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90, 
  0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0, 
  0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255, 
  255,255,255,8,0,0,0,3,0,0,0,63,184,130,128,81,176,195,128,95,72,3,128,11,40,2,128,97,16,4,128,61,32,2,128,62,32,2,128,47,144,65,126,19,97,0,0,0,23,1,0,0,1,0,17,1,21,122,0,0, 
  1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,21,122,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,21,122,0,0,1,15,1,21,122,0,0,17,1,99,158,0,0,1,19,61,0,0,0,187,0,0,0, 
  2,0,1,15,1,21,122,0,0,17,1,159,158,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,8,130,2,192,136,127,94,144,70,130,44,32,4,128,53,144,139,129,6, 
  192,72,127,7,192,8,128,98,48,130,128,105,240,2,128,114,144,7,128,125,200,8,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1, 
  78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0, 
  228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19, 
  93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0, 
  0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,62,124,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1, 
  124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,152,129,128,81,184,194,128,95,128,3,128,11,40,2,128,97,24,3,128,61,144,1,128,62,144,1,128,47,232,67,126,1,19,47,0, 
  0,0,155,0,0,0,1,0,17,1,62,124,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,62,124,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,62,124,0,0,17,1,159,158,0,0,1,15,1, 
  62,124,0,0,17,1,99,158,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,62,124,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,8,130,2,192,136,127, 
  94,144,70,130,44,32,4,128,53,144,139,129,6,192,72,127,7,192,8,128,98,48,130,128,105,240,2,128,114,144,7,128,125,200,8,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0, 
  0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0, 
  0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1, 
  4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146, 
  90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25, 
  0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,103,126,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15, 
  1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,131,128,81,24,196,128,95,40,2,128,11,144,2,128,97,32,3,128,61,144,1, 
  128,62,144,1,128,47,152,65,126,1,19,97,0,0,0,23,1,0,0,1,0,17,1,103,126,0,0,1,15,1,103,126,0,0,17,1,99,158,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,103,126,0,0,1,15, 
  1,103,126,0,0,17,1,159,158,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,103,126,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0, 
  0,58,200,8,130,1,192,8,130,2,192,136,127,94,144,70,130,44,32,4,128,53,144,139,129,6,192,72,127,7,192,8,128,98,48,130,128,105,240,2,128,114,144,7,128,125,144,9,128,126,144,4,128,4,19,84,0,0,0,2, 
  1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25, 
  0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0, 
  15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,144,128,0,0,17,1,38,93,0,0,1,4,19,93,0,0,0,13,1,0,0,1, 
  0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0, 
  15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,152,129,128,81,176,195,128,95,40, 
  2,128,11,32,3,128,97,16,4,128,61,144,1,128,62,144,1,128,47,144,66,126,1,19,47,0,0,0,155,0,0,0,1,0,17,1,144,128,0,0,1,15,1,144,128,0,0,17,1,99,158,0,0,1,19,97,0,0,0,23, 
  1,0,0,1,0,17,1,144,128,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,144,128,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,144,128,0,0,17,1,159,158,0,0,1,2,21,1,152,1, 
  0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,192,8,130,1,136,9,130,2,136,137,127,94,144,70,130,44,32,4,128,53,144,139,129,6,136,73,127,7,136,9,128,98,48,130,128,105,240,2,128,114,144,7,128,125,144, 
  9,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0, 
  0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0, 
  0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15, 
  1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,40,119,0,0,17,1,38,93,0,0, 
  1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17, 
  1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3, 
  0,0,0,58,200,10,130,1,192,8,130,2,192,136,127,94,144,70,130,44,32,4,128,53,144,139,129,6,192,72,127,7,192,8,128,98,48,130,128,105,240,2,128,114,144,7,128,125,200,8,128,126,144,4,128,4,19,84,0,0, 
  0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1, 
  62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25, 
  0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0, 
  19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,82,132,0,0,17,1,38,93,0,0,1,4,15,1,74,25, 
  0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,144,129,128,81,176,195,128, 
  95,72,3,128,11,184,2,128,97,16,4,128,61,32,2,128,62,32,2,128,47,40,66,126,19,47,0,0,0,155,0,0,0,1,0,17,1,82,132,0,0,1,1,19,97,0,0,0,23,1,0,0,1,0,17,1,82,132,0,0, 
  1,19,47,0,0,0,154,0,0,0,1,0,17,1,82,132,0,0,1,15,1,82,132,0,0,17,1,99,158,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,82,132,0,0,17,1,159,158,0,0,1,2,21,1, 
  152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,8,130,1,192,8,130,2,192,136,127,94,144,70,130,44,32,4,128,53,144,139,129,6,192,72,127,7,192,8,128,98,48,130,128,105,240,2,128,114,144,7,128, 
  125,144,9,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103, 
  91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97, 
  0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1, 
  4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,123,134,0,0,17,1,38, 
  93,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0, 
  0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,63,232,131,128,81,136,195,128,95,32,2,128,11,144,1,128,97,144,2,128,61,136,2,128,62,136,2,128,47,248,66,126,19,47,0,0,0,154,0,0,0,1,0,17,1,123,134,0,0,1,15,1,123,134,0, 
  0,17,1,99,158,0,0,1,1,15,1,123,134,0,0,17,1,159,158,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,123,134,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,47,0,0,0,155,0,0, 
  0,1,0,17,1,123,134,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,8,130,2,192,136,127,94,144,70,130,44,32,4,128,53,144,139,129,6,192,72,127,7,192,8, 
  128,98,48,130,128,105,240,2,128,114,144,7,128,125,200,8,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0, 
  19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0, 
  15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1, 
  0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0, 
  0,0,113,0,0,0,1,0,15,1,164,136,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2, 
  21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,152,129,128,81,184,194,128,95,16,4,128,11,24,3,128,97,168,3,128,61,144,1,128,62,144,1,128,47,40,66,126,1,19,47,0,0,0,155,0,0,0, 
  1,0,17,1,164,136,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,164,136,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,164,136,0,0,1,15,1,164, 
  136,0,0,17,1,159,158,0,0,1,15,1,164,136,0,0,17,1,99,158,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,2,130,1,136,9,130,2,136,137,127,94,88,71,130,44,232, 
  4,128,53,144,139,129,6,136,73,127,7,136,9,128,98,48,130,128,105,184,3,128,114,88,8,128,125,144,9,128,126,88,5,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19, 
  35,0,0,0,113,0,0,0,1,0,15,1,205,138,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0, 
  1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0, 
  15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1, 
  78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0, 
  0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15, 
  1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,32,131,128,81,24,196,128,95,176,3,128,11,0,2,128,97,152,1,128,61,144,1,128,62,144,1,128,47, 
  144,66,126,1,15,1,205,138,0,0,17,1,159,158,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,205,138,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,205,138,0,0,1,19,47,0,0,0,155,0, 
  0,0,1,0,17,1,205,138,0,0,1,15,1,205,138,0,0,17,1,99,158,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1, 
  192,8,130,2,192,136,127,94,144,70,130,44,32,4,128,53,144,139,129,6,192,72,127,7,192,8,128,98,48,130,128,105,240,2,128,114,144,7,128,125,200,8,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19, 
  62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91, 
  0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0, 
  17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1, 
  91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0, 
  0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,246,140,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,131,128,81,144,194,128,95,240,2,128,11,144,1,128, 
  97,40,2,128,61,32,2,128,62,32,2,128,47,232,67,126,19,47,0,0,0,154,0,0,0,1,0,17,1,246,140,0,0,1,1,15,1,246,140,0,0,17,1,159,158,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1, 
  15,1,246,140,0,0,17,1,99,158,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,246,140,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,246,140,0,0,1,2,21,1,152,1,0,0,108,51,1,0, 
  13,0,0,0,3,0,0,0,58,144,4,130,1,136,9,130,2,136,137,127,94,88,71,130,44,32,4,128,53,144,139,129,6,136,73,127,7,136,9,128,98,48,130,128,105,240,2,128,114,88,8,128,125,144,9,128,126,88,5,128, 
  4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0, 
  0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,31,143,0,0,17,1,38,93,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0, 
  0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0, 
  0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1, 
  4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,248,130, 
  128,81,24,196,128,95,144,2,128,11,136,3,128,97,32,2,128,61,136,2,128,62,136,2,128,47,144,65,126,19,97,0,0,0,23,1,0,0,1,0,17,1,31,143,0,0,1,15,1,31,143,0,0,17,1,159,158,0,0,1, 
  1,15,1,31,143,0,0,17,1,99,158,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,31,143,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,31,143,0,0,1,19,61,0,0,0,187,0,0,0,2, 
  0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,144,7,130,1,136,9,130,2,136,137,127,94,144,70,130,44,32,4,128,53,144,139,129,6,136,73,127,7,136,9,128,98,48,130,128,105,240,2, 
  128,114,88,8,128,125,144,9,128,126,144,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91, 
  0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0, 
  0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1, 
  31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,72,145,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0, 
  0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0, 
  15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255, 
  255,255,8,0,0,0,3,0,0,0,63,144,130,128,81,24,196,128,95,32,2,128,11,136,3,128,97,32,3,128,61,136,2,128,62,136,2,128,47,144,65,126,19,97,0,0,0,23,1,0,0,1,0,17,1,72,145,0,0,1, 
  15,1,72,145,0,0,17,1,99,158,0,0,1,1,19,47,0,0,0,155,0,0,0,1,0,17,1,72,145,0,0,1,15,1,72,145,0,0,17,1,159,158,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,72,145, 
  0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,0,8,130,2,0,136,127,94,208,69,130,44,96,3,128,53,144,139,129,6,0, 
  72,127,7,0,8,128,98,8,136,128,105,48,2,128,114,208,6,128,125,200,8,128,126,208,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170, 
  90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0, 
  0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0, 
  0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93, 
  0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0, 
  1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,113,147,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124, 
  13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,40,130,128,81,24,196,128,95,184,2,128,11,144,1,128,97,176,3,128,61,32,2,128,62,32,2,128,47,32,67,126,19,47,0,0,0, 
  154,0,0,0,1,0,17,1,113,147,0,0,1,1,19,47,0,0,0,155,0,0,0,1,0,17,1,113,147,0,0,1,15,1,113,147,0,0,17,1,99,158,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,113,147, 
  0,0,1,15,1,113,147,0,0,17,1,159,158,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,8,8,130,1,0,8,130,2,0,136,127,94, 
  208,69,130,44,96,3,128,53,144,139,129,6,0,72,127,7,0,8,128,98,208,136,128,105,48,2,128,114,208,6,128,125,144,9,128,126,208,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1, 
  91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180, 
  0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0, 
  17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1, 
  144,128,0,0,17,1,38,93,0,0,1,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59, 
  0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1, 
  158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,208,3,130,1,200,8,130,2,200,136,127,94,152,70,130,44,96,3,128,53,144,139,129, 
  6,200,72,127,7,200,8,128,98,208,136,128,105,48,2,128,114,152,7,128,125,144,9,128,126,152,4,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17, 
  1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,31,143,0,0,17,1,38,93,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0, 
  0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174, 
  0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8, 
  4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97, 
  0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17, 
  1,124,13,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,208,5,130,1,200,8,130,2,200,136,127,94,152,70,130,44,96,3,128,53,144,139,129,6,200,72,127,7,200,8,128,98,208,136, 
  128,105,48,2,128,114,152,7,128,125,144,9,128,126,208,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25, 
  0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0, 
  15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,21,122,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91, 
  91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,84,0,0,0,2,1,0,0,1, 
  0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1, 
  74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,152,1, 
  0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,48,2,130,1,200,8,130,2,200,136,127,94,152,70,130,44,40,4,128,53,144,139,129,6,200,72,127,7,200,8,128,98,208,136,128,105,248,2,128,114,152,7,128,125,144, 
  9,128,126,152,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,205,138,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91, 
  0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0, 
  0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4, 
  15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2, 
  0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17, 
  1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3, 
  0,0,0,58,200,10,130,1,0,8,130,2,0,136,127,94,208,69,130,44,96,3,128,53,144,139,129,6,0,72,127,7,0,8,128,98,8,136,128,105,48,2,128,114,208,6,128,125,200,8,128,126,208,3,128,4,15,1,74,25, 
  0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19, 
  78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1, 
  78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0, 
  1,8,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0, 
  19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,254,155,0,0,17,1,38,93,0,0,1,4,15,1,74,25, 
  0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,88,131,128,81,0,194,128, 
  95,152,1,128,11,232,3,128,97,96,2,128,61,144,1,128,62,144,1,128,47,200,66,126,1,15,1,254,155,0,0,17,1,99,158,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,254,155,0,0,17,1,159,158, 
  0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,254,155,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,254,155,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,254,155,0,0,1,2,21,1, 
  152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,0,8,130,2,0,136,127,94,208,69,130,44,96,3,128,53,144,139,129,6,0,72,127,7,0,8,128,98,8,136,128,105,48,2,128,114,208,6,128, 
  125,200,8,128,126,208,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1, 
  4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102, 
  115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0, 
  0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0, 
  1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,103,126,0, 
  0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,47,0,0,0,252,83,1,0,5,0,0, 
  0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,106,43,0,0,1,10,12,19,60,0,0,0,184,0,0,0,1,0,1,21,1,47,0,0,0,165,84,1,0,5,0,0, 
  0,2,0,0,0,115,56,1,128,1,48,1,128,2,48,129,128,11,56,65,127,118,56,1,128,10,12,17,1,201,96,0,0,1,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,43,0,0,0,181,85,1,0,4,0,0, 
  0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,16,1,128,12,17,1,146,98,0,0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,19,46,0,0,0,151,0,0,0,1,0,1,21,1,53,0,0, 
  0,137,52,1,0,5,0,0,0,2,0,0,0,6,160,1,129,1,160,1,128,2,160,129,127,7,160,1,128,62,48,1,128,4,15,1,232,82,0,0,17,1,160,43,0,0,1,8,19,60,0,0,0,184,0,0,0,1,0,1, 
  21,1,63,0,0,0,198,86,1,0,5,0,0,0,2,0,0,0,6,240,1,129,1,240,1,128,2,240,129,127,7,240,1,128,98,48,1,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2, 
  0,1,8,19,62,0,0,0,190,0,0,0,1,0,1,21,0,157,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,47,200,131,128,61,96,195,128,63,208,194,128,11,88,68,127,81,64,130,128,95,216,1,128,97,112,1, 
  128,15,1,111,159,0,0,17,1,208,172,0,0,1,15,1,111,159,0,0,17,1,3,171,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,111,159,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,111,159, 
  0,0,1,15,1,111,159,0,0,17,1,13,160,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,111,159,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,111,159,0,0,1,2,21,1,131,0,0,0,41, 
  82,1,0,12,0,0,0,3,0,0,0,58,216,67,130,1,144,195,129,2,144,131,127,94,16,3,130,44,80,2,128,53,152,67,129,6,144,67,127,7,144,3,128,105,144,2,128,114,208,2,128,125,16,2,128,126,80,3,128,12, 
  17,1,134,169,0,0,1,12,17,1,9,168,0,0,1,12,17,1,140,166,0,0,1,12,17,1,15,165,0,0,1,12,17,1,146,163,0,0,1,12,17,1,21,162,0,0,1,10,12,17,1,152,160,0,0,1,6,17,1,184, 
  119,0,0,1,12,17,1,133,12,0,0,1,21,1,124,1,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,232,71,130,1,224,199,129,2,224,135,127,94,176,5,130,44,64,3,128,53,176,74,129,6,224,71,127,7,224, 
  7,128,105,16,2,128,114,176,6,128,125,176,8,128,126,176,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62, 
  25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0, 
  0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15, 
  1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,144,128,0,0,17,1,38,93,0,0,1,4,19,93,0,0,0,13,1,0,0, 
  1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0, 
  0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,124,1,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,232,73,130,1,224,199,129,2, 
  224,135,127,94,176,5,130,44,64,3,128,53,176,74,129,6,224,71,127,7,224,7,128,105,16,2,128,114,176,6,128,125,232,7,128,126,176,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1, 
  91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180, 
  0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0, 
  17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78, 
  0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0, 
  1,0,15,1,113,147,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,124,1,0,0, 
  191,45,1,0,12,0,0,0,3,0,0,0,58,232,73,130,1,224,199,129,2,224,135,127,94,176,5,130,44,64,3,128,53,176,74,129,6,224,71,127,7,224,7,128,105,16,2,128,114,176,6,128,125,232,7,128,126,176,3,128, 
  4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1, 
  0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74, 
  25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17, 
  1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1, 
  78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,103,126,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0, 
  0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,124,1,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,176,69,130,1,168,200,129,2,168,136,127,94,120,6,130,44,64,3,128,53,176,74,129,6,168,72, 
  127,7,168,8,128,105,16,2,128,114,120,7,128,125,176,8,128,126,176,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4, 
  15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1, 
  74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,21,122,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0, 
  0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13, 
  1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1, 
  74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,124,1,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,176,67,130,1,168, 
  200,129,2,168,136,127,94,120,6,130,44,64,3,128,53,176,74,129,6,168,72,127,7,168,8,128,105,16,2,128,114,120,7,128,125,176,8,128,126,120,4,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0, 
  0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,31,143,0,0,17,1,38,93,0,0,1,4, 
  19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115, 
  0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0, 
  15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15, 
  1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,124, 
  1,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,16,66,130,1,168,200,129,2,168,136,127,94,120,6,130,44,8,4,128,53,176,74,129,6,168,72,127,7,168,8,128,105,216,2,128,114,120,7,128,125,176,8,128,126, 
  120,4,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,205,138,0,0,17,1,38,93,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17, 
  1,170,90,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24, 
  1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74, 
  25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59, 
  0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1, 
  158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,124,1,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,232,73,130,1,224,199,129,2,224,135,127,94,176,5,130,44,64,3,128,53,176,74,129, 
  6,224,71,127,7,224,7,128,105,16,2,128,114,176,6,128,125,232,7,128,126,176,3,128,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0, 
  0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1, 
  0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,15,1,74,25,0,0,15, 
  1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180, 
  0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,254,155,0,0,17,1,38,93,0,0,1, 
  4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,1,47,0,0,0,165,84,1,0,5,0,0,0,2,0,0,0,115,56,1, 
  128,1,48,1,128,2,48,129,128,11,56,65,127,118,56,1,128,10,12,17,1,63,171,0,0,1,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,69,0,0,0,195,67,1,0,5,0,0,0,2,0,0,0,115,120,1, 
  128,1,112,1,128,2,112,129,128,11,184,65,127,118,48,1,128,4,17,1,121,172,0,0,1,8,4,17,1,252,171,0,0,1,4,15,1,133,171,0,0,17,1,74,84,0,0,1,2,21,1,51,0,0,0,24,87,1,0,4, 
  0,0,0,2,0,0,0,118,88,1,128,1,80,1,128,2,80,129,127,115,16,1,128,4,17,1,196,171,0,0,1,8,4,17,1,240,84,0,0,1,19,81,0,0,0,241,0,0,0,2,0,1,21,1,44,0,0,0,152,87, 
  1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,252,0,0,0,4,0,1,19,81,0,0,0,246,0,0,0,3,0,1,21,1,57,0,0,0,13,88,1,0,4,0, 
  0,0,2,0,0,0,118,136,1,128,1,16,1,128,2,16,129,127,11,24,1,128,8,4,15,1,65,172,0,0,17,1,74,84,0,0,1,4,17,1,187,83,0,0,1,19,81,0,0,0,243,0,0,0,2,0,1,21,1,44, 
  0,0,0,152,87,1,0,3,0,0,0,1,0,0,0,2,240,128,128,1,240,0,128,118,248,0,128,8,4,19,81,0,0,0,249,0,0,0,4,0,1,19,81,0,0,0,239,0,0,0,3,0,1,21,7,48,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,63,64,193,127,4,15,1,201,85,0,0,17,1,74,84,0,0,1,4,17,1,19,86,0,0,1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,208,0,128,8,19,81,0,0,0,244,0,0,0,2,0,1,21,1,43,0,0,0,181,85,1,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,16,1,128,12,17,1,8,173,0, 
  0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,21,1,51,0,0,0,24,72,1,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,88,1,128,4,17,1,191,173,0,0,1,8,4, 
  17,1,60,173,0,0,1,2,21,1,130,0,0,0,136,72,1,0,6,0,0,0,2,0,0,0,114,56,67,129,1,48,195,128,2,48,131,127,51,168,3,128,125,64,2,128,126,80,1,128,4,19,94,0,0,0,14,1,0,0, 
  1,0,19,78,0,0,0,228,0,0,0,1,0,17,1,89,87,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,17,1,89,87,0,0,1,8,4,15,1,152,87,0,0,17,1, 
  238,89,0,0,1,4,19,95,0,0,0,20,1,0,0,3,0,1,2,21,1,130,0,0,0,136,72,1,0,6,0,0,0,2,0,0,0,114,72,66,129,1,64,194,128,2,64,130,127,51,184,2,128,125,32,3,128,126,80,1, 
  128,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,17,1,153,88,0,0,1,8,4,15,1,90,88,0,0,17,1,238,89,0,0,1,4,19,95,0,0,0,17,1,0,0,3,0,1,4, 
  19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,17,1,153,88,0,0,1,2,19,61,0,0,0,185,0,0,0,1,0,1,21,1,69,0,0,0,195,67,1,0,5,0,0,0,2,0,0,0, 
  115,232,1,128,1,48,1,128,2,48,129,128,11,120,65,127,118,56,1,128,8,4,17,1,114,85,0,0,1,4,15,1,177,84,0,0,17,1,74,84,0,0,1,4,17,1,118,83,0,0,1,19,81,0,0,0,253,0,0,0, 
  1,0,1,21,1,47,0,0,0,242,72,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56,1,128,8,4,17,1,207,174,0,0,1,2,21,1,47,0,0,0,5,42,1, 
  0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,11,48,1,128,4,17,1,255,174,0,0,1,8,2,21,1,117,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,224,17, 
  128,1,160,10,128,2,160,138,128,67,216,200,131,66,168,10,128,70,64,14,128,6,160,202,127,7,160,202,129,72,224,18,131,73,8,15,128,74,216,9,128,75,208,79,129,76,144,68,129,93,144,7,128,71,88,70,129,63,88,5, 
  128,91,152,16,128,92,248,12,128,99,32,7,128,103,48,3,128,104,248,11,128,4,15,1,6,178,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1, 
  86,215,0,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,6,178,0,0,17,1,38,209,0,0,1,4,15,1,6,178,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,24,194,0, 
  0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,6,178,0,0,17,1,38,209,0,0,1,4,15,1,117,177,0,0,17,1,28,3,0,0,1,4,19,58,0,0,0,178,0,0,0,1,0,19,38,0,0,0,134,0, 
  0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,6,178,0,0,1,4,15,1,6,178,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,183,205,0,0,1,4,19,37,0,0,0, 
  131,0,0,0,1,0,15,1,6,178,0,0,17,1,38,209,0,0,1,8,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,6,178,0,0,15,1,247,205,0,0,17,1,177,207,0, 
  0,1,4,15,1,6,178,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,19,58,0,0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0, 
  0,0,42,0,0,0,1,0,17,1,6,178,0,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,6,178,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,6,178,0,0,17,1, 
  38,209,0,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,6,178,0,0,17,1,38,209,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0, 
  0,0,1,0,17,1,6,178,0,0,1,4,15,1,6,178,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,6,178,0, 
  0,17,1,38,209,0,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4,128,73,120,1,128,38,184,131,128,55,40,195,128,66,152,66,128,70,8,2,128,91,112,1,128,1,19,55,0,0, 
  0,171,0,0,0,1,0,17,1,117,177,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1,117,177,0,0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,117,177,0,0,1,19,38,0,0,0,132,0,0,0,1, 
  0,17,1,117,177,0,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,117,177,0,0,1,17,1,6,178,0,0,1,2,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128, 
  2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,91,0,0,0,11,1,0,0,6,0,1,2,21,0,55,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47,80,129,128,11,88,193,127,63,240,0,128,19,47,0, 
  0,0,155,0,0,0,1,0,1,1,19,47,0,0,0,154,0,0,0,1,0,1,2,21,1,39,0,0,0,141,88,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128,4,17,1,89,93,0,0, 
  1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,168,65,128,63,24,65,128,91,16,1,128,11,224,65,127,1,19,47,0,0,0,155,0,0,0,1,0, 
  17,1,166,178,0,0,1,17,1,159,174,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,166,178,0,0,1,2,19,70,0,0,0,207,0,0,0,4,0,1,19,72,0,0,0,214,0,0,0,1,0,1,21,1,77, 
  0,0,0,131,39,1,0,6,0,0,0,2,0,0,0,6,192,1,128,1,192,1,129,2,192,129,127,7,192,65,128,11,80,1,128,53,200,1,128,4,15,1,175,185,0,0,17,1,195,183,0,0,1,8,4,15,1,175,185,0, 
  0,15,1,183,183,0,0,17,1,91,179,0,0,1,2,21,1,191,0,0,0,100,89,1,0,9,0,0,0,3,0,0,0,94,136,5,130,1,224,132,128,2,224,132,128,105,64,4,128,114,232,4,128,125,176,1,128,6,224,132, 
  126,7,224,4,128,126,248,2,128,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,17,1,27,180,0,0,1,4,19,94,0,0,0,14,1,0,0, 
  1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,17,1,27,180,0,0,1,4,15,1,27,180,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,8,4,15,1,27,180,0,0,15, 
  1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,27,180,0,0,17,1,31,16,0,0,1,2,21,1,47,0,0,0,242,72,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1, 
  128,85,56,1,128,8,4,17,1,75,180,0,0,1,2,21,1,47,0,0,0,5,42,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,11,48,1,128,4,17,1,123,180,0,0, 
  1,8,2,21,1,117,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,224,11,128,1,16,11,128,2,16,139,128,67,200,200,131,66,120,7,128,70,128,17,128,6,16,203,127,7,16,203,129,72,160,3,131,73,240,15, 
  128,74,24,11,128,75,184,80,129,76,176,70,129,93,104,5,128,71,40,79,129,63,224,12,128,91,224,13,128,92,200,9,128,99,48,3,128,103,72,18,128,104,104,4,128,4,15,1,38,183,0,0,17,1,28,3,0,0,1,4, 
  19,37,0,0,0,130,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0,0,1,4,15,1,241,182,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,19,58,0, 
  0,0,178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,241,182,0,0,1,4,19,37,0,0,0,126,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0, 
  0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,241,182,0,0,15,1,247,205,0,0,17,1,177,207,0,0,1,4,15,1,241,182,0,0,15,1,247,205,0,0,15,1,177, 
  207,0,0,15,1,26,209,0,0,17,1,183,205,0,0,1,4,19,58,0,0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,241,182,0,0,1,8,4,19, 
  37,0,0,0,131,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0,0,1,4,15,1,241,182,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1,4,15,1,241,182, 
  0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,24,194,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1, 
  0,17,1,241,182,0,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0,0,1,4,19, 
  37,0,0,0,127,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,129,0,0,0,1,0,15,1,241,182,0,0,17,1,38,209,0,0,1,4,15,1,241,182,0,0,15,1,27,216,0, 
  0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,2,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2, 
  152,129,127,7,152,1,128,82,48,1,128,4,19,92,0,0,0,12,1,0,0,6,0,1,8,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,132,129,73,120,1,128,38,184,131,128,55,40,3,128, 
  66,152,66,128,70,8,2,128,92,112,1,128,1,19,55,0,0,0,171,0,0,0,1,0,17,1,38,183,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1,38,183,0,0,1,19,55,0,0,0,169,0,0,0,1,0, 
  17,1,38,183,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,38,183,0,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,38,183,0,0,1,17,1,241,182,0,0,1,2,19,75,0,0,0,223,0,0,0, 
  1,0,1,21,1,155,0,0,0,224,39,1,0,8,0,0,0,3,0,0,0,121,8,4,128,1,48,195,127,2,48,131,128,11,96,2,128,122,144,1,128,53,56,3,128,6,48,3,128,7,48,3,128,4,15,1,119,184,0,0, 
  15,1,172,184,0,0,15,1,163,185,0,0,17,1,73,11,0,0,1,4,15,1,119,184,0,0,15,1,172,184,0,0,15,1,107,184,0,0,17,1,1,8,0,0,1,8,4,15,1,119,184,0,0,15,1,172,184,0,0,15, 
  1,95,184,0,0,17,1,91,179,0,0,1,4,15,1,119,184,0,0,15,1,172,184,0,0,15,1,107,184,0,0,17,1,149,4,0,0,1,2,19,74,0,0,0,217,0,0,0,1,0,1,19,74,0,0,0,219,0,0,0, 
  1,0,1,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1,128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,75,0,0,0,224,0,0,0,3,0,1,8,2,21,0,35,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,74,176,0,128,15,1,172,184,0,0,17,1,208,184,0,0,1,2,21,1,59,0,0,0,209,44,1,0,8,0,0,0,3,0,0,0,121,144,1,128,1,208,193,127,2,208, 
  129,128,11,144,1,128,122,144,1,128,53,144,1,128,6,208,1,128,7,208,1,128,12,17,1,19,185,0,0,1,10,12,17,1,133,12,0,0,1,21,1,107,0,0,0,224,39,1,0,8,0,0,0,3,0,0,0,121,144,1, 
  128,1,224,194,127,2,224,130,128,11,0,2,128,122,112,2,128,53,232,2,128,6,224,2,128,7,224,2,128,4,15,1,151,185,0,0,17,1,149,4,0,0,1,4,15,1,151,185,0,0,17,1,1,8,0,0,1,4,15,1, 
  139,185,0,0,17,1,73,11,0,0,1,8,4,15,1,127,185,0,0,17,1,91,179,0,0,1,2,19,74,0,0,0,220,0,0,0,2,0,1,19,74,0,0,0,221,0,0,0,2,0,1,19,74,0,0,0,222,0,0,0, 
  2,0,1,19,74,0,0,0,218,0,0,0,1,0,1,19,73,0,0,0,216,0,0,0,4,0,1,21,1,95,0,0,0,219,89,1,0,7,0,0,0,2,0,0,0,56,112,1,128,1,80,66,129,2,80,130,128,7,80,130, 
  128,6,80,2,128,11,224,1,128,53,88,2,128,4,15,1,40,193,0,0,17,1,232,192,0,0,1,4,15,1,220,192,0,0,17,1,240,190,0,0,1,8,4,15,1,220,192,0,0,15,1,228,190,0,0,17,1,27,186,0, 
  0,1,2,21,1,86,0,0,0,67,90,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,87,56,1,128,8,4,19,49,0,0,0,157,0,0,0,1,0,19,87,0,0,0,5, 
  1,0,0,1,0,19,88,0,0,0,6,1,0,0,1,0,15,1,72,187,0,0,17,1,114,186,0,0,1,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,88,176,0,128,15,1,114,186,0,0,17,1, 
  150,186,0,0,1,2,21,1,47,0,0,0,139,49,1,0,5,0,0,0,2,0,0,0,84,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,12,17,1,205,186,0,0,1,10,12,17,1,133,12,0,0, 
  1,21,1,47,0,0,0,232,49,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,253,186,0,0,1,2,21,1,74,0,0,0,67,90,1,0,5, 
  0,0,0,2,0,0,0,6,72,2,128,1,72,2,128,2,72,130,127,7,72,66,128,87,48,1,128,4,19,49,0,0,0,157,0,0,0,1,0,19,87,0,0,0,5,1,0,0,1,0,19,88,0,0,0,7,1,0,0,3, 
  0,1,8,2,21,1,47,0,0,0,242,72,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,85,56,1,128,8,4,17,1,120,187,0,0,1,2,21,1,47,0,0,0,5,42, 
  1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,11,56,1,128,8,4,17,1,168,187,0,0,1,2,21,1,117,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,64, 
  5,128,1,176,11,128,2,176,139,128,67,184,204,131,66,152,9,128,70,64,6,128,6,176,203,127,7,176,203,129,72,8,7,131,73,232,10,128,74,120,4,128,75,24,79,129,76,208,71,129,93,96,18,128,71,224,79,129,63,184, 
  11,128,91,24,17,128,92,48,3,128,99,168,16,128,103,184,13,128,104,152,8,128,4,19,58,0,0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,175,190, 
  0,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,175,190,0,0,17,1,38,209,0,0,1,4,15,1,175,190,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1, 
  4,19,37,0,0,0,129,0,0,0,1,0,15,1,175,190,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,130,0,0,0,1,0,15,1,175,190,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,126,0,0,0, 
  1,0,15,1,175,190,0,0,17,1,38,209,0,0,1,4,15,1,175,190,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19, 
  14,0,0,0,45,0,0,0,1,0,15,1,175,190,0,0,15,1,247,205,0,0,17,1,177,207,0,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,175,190,0,0,17,1,38,209,0,0,1,8,4,15,1,175,190, 
  0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,24,194,0,0,1,4,15,1,175,190,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,183,205,0,0,1,4, 
  15,1,175,190,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,19,37,0,0,0,127,0,0,0,1,0,15,1,175,190,0, 
  0,17,1,38,209,0,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,175,190,0,0,17,1,38,209,0,0,1,4,15,1,30,190,0,0,17,1,28,3,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19, 
  38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,175,190,0,0,1,4,19,58,0,0,0,178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0, 
  1,0,17,1,175,190,0,0,1,2,21,0,144,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,12,72,4,128,73,120,1,128,38,184,131,128,55,40,3,128,66,152,66,128,70,8,66,128,86,112,1,128,1,19,55,0, 
  0,0,171,0,0,0,1,0,17,1,30,190,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1,30,190,0,0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,30,190,0,0,1,19,38,0,0,0,132,0,0,0, 
  1,0,17,1,30,190,0,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,30,190,0,0,1,17,1,175,190,0,0,1,2,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,152,1,129,1,152,1, 
  128,2,152,129,127,7,152,1,128,82,48,1,128,4,19,86,0,0,0,4,1,0,0,6,0,1,8,2,19,69,0,0,0,205,0,0,0,1,0,1,21,1,155,0,0,0,224,39,1,0,8,0,0,0,3,0,0,0,121,96, 
  2,128,1,208,196,127,2,208,132,128,11,0,4,128,122,48,3,128,53,144,1,128,6,208,4,128,7,208,4,128,4,15,1,164,191,0,0,15,1,217,191,0,0,15,1,208,192,0,0,17,1,27,186,0,0,1,4,15,1,164, 
  191,0,0,15,1,217,191,0,0,15,1,152,191,0,0,17,1,149,4,0,0,1,4,15,1,164,191,0,0,15,1,217,191,0,0,15,1,140,191,0,0,17,1,73,11,0,0,1,4,15,1,164,191,0,0,15,1,217,191,0, 
  0,15,1,152,191,0,0,17,1,1,8,0,0,1,8,2,19,68,0,0,0,201,0,0,0,1,0,1,19,68,0,0,0,200,0,0,0,1,0,1,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,48, 
  1,129,1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,69,0,0,0,206,0,0,0,3,0,1,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,68,176,0,128,15,1,217,191,0, 
  0,17,1,253,191,0,0,1,2,21,1,59,0,0,0,209,44,1,0,8,0,0,0,3,0,0,0,121,144,1,128,1,208,193,127,2,208,129,128,11,144,1,128,122,144,1,128,53,144,1,128,6,208,1,128,7,208,1,128,12, 
  17,1,64,192,0,0,1,10,12,17,1,133,12,0,0,1,21,1,107,0,0,0,224,39,1,0,8,0,0,0,3,0,0,0,121,144,1,128,1,224,194,127,2,224,130,128,11,112,2,128,122,232,2,128,53,0,2,128,6,224, 
  2,128,7,224,2,128,4,15,1,196,192,0,0,17,1,149,4,0,0,1,4,15,1,184,192,0,0,17,1,27,186,0,0,1,4,15,1,196,192,0,0,17,1,1,8,0,0,1,8,4,15,1,172,192,0,0,17,1,73,11, 
  0,0,1,2,19,68,0,0,0,204,0,0,0,2,0,1,19,68,0,0,0,202,0,0,0,2,0,1,19,68,0,0,0,203,0,0,0,2,0,1,19,68,0,0,0,199,0,0,0,1,0,1,19,66,0,0,0,197,0,0, 
  0,4,0,1,21,1,63,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,240,1,129,1,240,1,128,2,240,129,127,7,240,1,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,67,0,0,0,198, 
  0,0,0,2,0,1,8,2,21,1,77,0,0,0,131,39,1,0,6,0,0,0,2,0,0,0,6,96,2,128,1,96,2,129,2,96,130,127,7,96,66,128,11,240,1,128,53,80,1,128,4,15,1,118,193,0,0,15,1,228, 
  190,0,0,17,1,27,186,0,0,1,4,15,1,118,193,0,0,17,1,240,190,0,0,1,8,2,19,66,0,0,0,196,0,0,0,5,0,1,21,0,149,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,0,160,4,129, 
  73,112,1,128,38,176,195,128,55,32,3,128,12,64,4,128,66,144,66,128,70,0,2,128,19,55,0,0,0,171,0,0,0,1,0,17,1,130,193,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1,130,193,0,0,1, 
  19,55,0,0,0,169,0,0,0,1,0,17,1,130,193,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,130,193,0,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,130,193,0,0,1,19,0,0,0,0,0, 
  0,0,0,3,0,1,1,2,21,1,58,0,0,0,67,90,1,0,5,0,0,0,2,0,0,0,6,200,1,128,1,200,1,128,2,200,129,127,7,200,65,128,87,48,1,128,4,19,49,0,0,0,157,0,0,0,1,0,17,1, 
  83,194,0,0,1,8,2,21,1,129,0,0,0,156,90,1,0,8,0,0,0,3,0,0,0,58,64,3,128,1,200,130,128,2,200,130,127,65,208,2,128,87,48,2,128,53,144,1,128,6,200,2,128,7,200,66,127,4,15,1, 
  60,205,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,17,1,128,204,0,0,1,8,4,15,1,118,200,0,0,17,1,213,194,0,0,1,4,19,35,0,0,0,113,0,0, 
  0,1,0,15,1,118,200,0,0,17,1,38,93,0,0,1,2,21,1,47,0,0,0,81,93,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,1,128,2,48,129,127,7,48,65,128,59,56,1,128,8,4,17,1,5, 
  195,0,0,1,2,21,1,111,0,0,0,194,93,1,0,7,0,0,0,2,0,0,0,6,112,65,129,1,112,193,128,2,112,129,127,7,112,193,128,53,16,2,128,58,176,2,128,87,120,1,128,8,4,19,49,0,0,0,157,0, 
  0,0,1,0,17,1,186,199,0,0,1,4,15,1,127,199,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,117,195,0,0,17,1,38,93,0,0,1,2,21,0,78, 
  0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,36,216,1,128,47,160,65,128,63,16,1,128,11,224,129,127,19,47,0,0,0,155,0,0,0,1,0,17,1,117,195,0,0,1,17,1,196,195,0,0,1,1,19,47,0, 
  0,0,154,0,0,0,1,0,17,1,117,195,0,0,1,2,21,1,47,0,0,0,66,94,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,10,12,17,1,0,196, 
  0,0,1,12,19,36,0,0,0,120,0,0,0,5,0,1,21,1,47,0,0,0,137,52,1,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,4,17,1,48,196,0, 
  0,1,8,2,21,1,4,3,0,0,220,52,1,0,26,0,0,0,4,0,0,0,32,48,142,133,1,136,204,131,2,136,204,131,35,232,10,128,36,208,3,128,37,96,13,128,6,136,12,131,7,136,12,131,40,48,17,128,41,64, 
  20,128,42,0,18,128,11,120,74,130,44,64,6,128,29,16,21,128,46,112,5,128,31,112,147,129,33,224,21,128,34,216,8,130,38,184,75,129,39,144,76,129,43,176,22,128,47,168,9,128,48,96,16,128,54,160,4,128,87,16, 
  7,128,98,0,15,128,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15, 
  1,115,199,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,145,79,0,0,17,1,198,72,0,0,1,4,19,49,0,0,0,157,0,0,0, 
  1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,36,0,0,0,118,0,0,0,7,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0, 
  15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,53,199,0,0,17,1,122,52,0,0,1,4,15,1,115,199,0,0, 
  15,1,208,82,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,18,67,0,0,1,8,4,15,1,115,199,0,0,15,1,208,82,0,0,15, 
  1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,204, 
  51,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0, 
  0,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0, 
  0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,36,0,0,0,118,0,0,0,7,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,115,199,0,0,15, 
  1,208,82,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,4,15,1,115,199,0,0,15,1,208,82,0,0,15,1,220, 
  82,0,0,17,1,80,81,0,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,36,0,0,0,118,0,0,0,7,0,1,2,21,0,61, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,36,80,1,128,9,88,65,128,53,240,0,128,19,36,0,0,0,118,0,0,0,7,0,1,1,19,53,0,0,0,163,0,0,0,1,0,17,1,53,199,0,0,1,2,19, 
  36,0,0,0,118,0,0,0,7,0,1,21,1,47,0,0,0,137,52,1,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,4,17,1,48,196,0,0,1,8,19,36, 
  0,0,0,120,0,0,0,5,0,1,21,1,75,0,0,0,236,94,1,0,6,0,0,0,2,0,0,0,6,80,2,128,1,80,194,128,2,80,130,127,7,80,130,128,61,80,1,128,87,144,1,128,4,17,1,6,200,0,0,1, 
  4,19,49,0,0,0,157,0,0,0,1,0,19,36,0,0,0,116,0,0,0,6,0,1,8,2,21,1,47,0,0,0,173,95,1,0,5,0,0,0,2,0,0,0,60,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128, 
  6,112,1,128,4,17,1,54,200,0,0,1,8,2,21,1,63,0,0,0,67,90,1,0,5,0,0,0,2,0,0,0,6,240,1,128,1,240,1,128,2,240,129,127,7,240,65,128,87,48,1,128,4,19,49,0,0,0,157,0, 
  0,0,1,0,19,36,0,0,0,114,0,0,0,8,0,1,8,2,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,36,216,1,128,47,160,65,128,63,16,1,128,11,224,129,127,19,47,0,0,0,155,0,0, 
  0,1,0,17,1,118,200,0,0,1,17,1,197,200,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,118,200,0,0,1,2,21,1,47,0,0,0,66,94,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1, 
  48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,10,12,17,1,1,201,0,0,1,12,19,36,0,0,0,121,0,0,0,3,0,1,21,1,47,0,0,0,137,52,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1, 
  48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,17,1,49,201,0,0,1,2,21,1,4,3,0,0,220,52,1,0,26,0,0,0,4,0,0,0,32,120,138,133,1,72,206,131,2,72,206,131,35,208,18,128,36, 
  216,8,128,37,224,20,128,6,72,14,131,7,72,14,131,40,80,23,128,41,208,3,128,42,32,15,128,11,160,83,130,44,64,6,128,29,128,22,128,46,80,14,128,31,0,146,129,33,112,5,128,34,176,21,130,38,16,84,129,39, 
  160,68,129,43,144,16,128,47,72,11,128,48,168,9,128,54,120,13,128,87,16,7,128,98,24,12,128,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,116,204,0,0, 
  15,1,208,82,0,0,15,1,220,82,0,0,17,1,109,82,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1, 
  145,79,0,0,17,1,198,72,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,36,0,0, 
  0,119,0,0,0,5,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,116,65,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,203,65,0,0,1, 
  4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,204,51,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,116,204,0,0,15, 
  1,208,82,0,0,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,186,72,0,0,17,1,105,67,0, 
  0,1,8,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,254,81,0,0,17,1,244,79,0,0,1,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0, 
  0,1,0,19,36,0,0,0,119,0,0,0,5,0,1,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,19,53,0,0,0,164,0,0,0,1,0,19,36,0,0,0,119,0,0,0,5,0, 
  1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,75,80,0,0,1,4,15,1,54,204,0,0, 
  17,1,122,52,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,18,67,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,162,80,0,0,1,4, 
  15,1,116,204,0,0,15,1,208,82,0,0,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,116,204,0,0,15,1,208,82,0,0,15,1,97,82,0,0,17,1,10,82,0,0,1,4,15,1,116,204,0,0,15,1, 
  208,82,0,0,15,1,220,82,0,0,17,1,157,79,0,0,1,2,21,0,61,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,36,80,1,128,9,88,65,128,53,240,0,128,19,36,0,0,0,119,0,0,0,5,0,1, 
  1,19,53,0,0,0,163,0,0,0,1,0,17,1,54,204,0,0,1,2,19,36,0,0,0,119,0,0,0,5,0,1,21,1,75,0,0,0,236,94,1,0,6,0,0,0,2,0,0,0,6,80,2,128,1,80,194,128,2,80, 
  130,127,7,80,130,128,61,80,1,128,87,144,1,128,4,17,1,204,204,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,36,0,0,0,117,0,0,0,4,0,1,8,2,21,1,47,0,0,0,173,95,1,0,5,0, 
  0,0,2,0,0,0,60,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,252,204,0,0,1,8,2,21,1,63,0,0,0,67,90,1,0,5,0,0,0,2,0,0,0,6,240,1,128,1,240, 
  1,128,2,240,129,127,7,240,65,128,87,48,1,128,4,19,49,0,0,0,157,0,0,0,1,0,19,36,0,0,0,115,0,0,0,6,0,1,8,2,21,1,47,0,0,0,137,52,1,0,5,0,0,0,2,0,0,0,6,48, 
  1,129,1,48,1,128,2,48,129,127,7,48,1,128,62,56,1,128,8,4,17,1,49,201,0,0,1,19,36,0,0,0,121,0,0,0,3,0,1,21,1,63,0,0,0,67,90,1,0,5,0,0,0,2,0,0,0,6,48,1, 
  128,1,48,1,128,2,48,129,127,7,48,65,128,87,56,1,128,8,4,19,49,0,0,0,157,0,0,0,1,0,19,36,0,0,0,122,0,0,0,2,0,1,2,21,1,63,0,0,0,67,90,1,0,5,0,0,0,2,0,0, 
  0,6,240,1,128,1,240,1,128,2,240,129,127,7,240,65,128,87,48,1,128,4,19,49,0,0,0,157,0,0,0,1,0,19,36,0,0,0,123,0,0,0,2,0,1,8,2,21,1,47,0,0,0,92,96,1,0,5,0,0, 
  0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,50,206,0,0,1,19,12,0,0,0,41,0,0,0,1,0,1,21,1,242,0,0,0,11,97,1,0,10,0,0,0, 
  3,0,0,0,104,168,5,128,1,64,2,128,2,64,2,128,91,120,70,129,92,72,2,128,93,96,3,128,6,64,2,128,7,64,130,128,99,208,1,128,103,120,4,128,4,15,1,49,207,0,0,17,1,28,3,0,0,1,8,4, 
  19,58,0,0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,40,0,0,0,3,0,1,4,19,58,0,0,0,178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0, 
  0,0,40,0,0,0,3,0,1,4,15,1,37,207,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,15,1,37,207,0,0,15,1,39,216,0,0, 
  15,1,74,215,0,0,17,1,145,214,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,40,0,0,0,3,0,1,2,19,12,0,0,0,40,0,0,0,3,0, 
  1,21,0,127,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,240,3,128,73,80,1,128,38,144,131,128,55,0,3,128,66,112,66,128,70,224,1,128,19,55,0,0,0,171,0,0,0,1,0,17,1,49,207,0,0, 
  1,19,55,0,0,0,170,0,0,0,1,0,17,1,49,207,0,0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,49,207,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,49,207,0,0,1,19,12,0,0,0, 
  40,0,0,0,3,0,1,1,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,14,176,0,128,15,1,177,207,0,0,17,1,213,207,0,0,1,2,21,1,47,0,0,0,185,97,1,0,5,0,0,0,2, 
  0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,6,17,1,12,208,0,0,1,10,12,17,1,133,12,0,0,1,21,1,91,0,0,0,99,98,1,0,14,0,0,0,3,0,0,0,64,88, 
  66,131,1,80,2,128,2,80,194,129,67,88,194,129,92,152,2,128,93,152,2,128,6,80,2,128,7,80,66,128,63,88,2,129,66,88,2,128,91,152,66,128,99,152,2,128,103,152,2,128,104,152,2,128,10,12,17,1,104,208, 
  0,0,1,12,17,1,133,12,0,0,1,2,21,1,47,0,0,0,92,96,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,152,208,0,0,1,8,2, 
  21,1,117,0,0,0,91,99,1,0,8,0,0,0,3,0,0,0,64,56,3,128,1,192,2,128,2,192,194,128,67,200,2,128,63,80,2,128,66,144,1,128,6,192,2,128,7,192,66,127,4,19,36,0,0,0,124,0,0,0, 
  1,0,19,14,0,0,0,46,0,0,0,3,0,1,4,15,1,14,209,0,0,17,1,24,194,0,0,1,8,4,15,1,14,209,0,0,17,1,183,205,0,0,1,4,15,1,14,209,0,0,17,1,119,205,0,0,1,2,19,14, 
  0,0,0,46,0,0,0,3,0,1,19,14,0,0,0,45,0,0,0,1,0,1,21,1,47,0,0,0,92,96,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128, 
  4,17,1,97,209,0,0,1,8,19,12,0,0,0,39,0,0,0,1,0,1,21,1,116,1,0,0,99,98,1,0,14,0,0,0,3,0,0,0,64,184,73,131,1,32,3,128,2,32,195,129,67,80,194,129,92,160,8,128,93, 
  72,4,128,6,32,3,128,7,32,67,128,63,48,6,129,66,40,3,128,91,136,74,128,99,48,8,128,103,0,7,128,104,96,5,128,4,15,1,98,211,0,0,15,1,28,213,0,0,15,1,133,214,0,0,17,1,183,205,0,0, 
  1,8,4,19,36,0,0,0,124,0,0,0,1,0,19,13,0,0,0,43,0,0,0,1,0,15,1,98,211,0,0,17,1,28,213,0,0,1,4,19,58,0,0,0,178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1, 
  0,19,12,0,0,0,37,0,0,0,3,0,1,4,15,1,86,211,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,15,1,98,211,0,0,15,1,28,213,0,0,15,1,133,214,0,0,17,1, 
  24,194,0,0,1,4,15,1,86,211,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,15,1,214,210,0,0,17,1,28,3,0,0,1,4,19,58, 
  0,0,0,177,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,37,0,0,0,3,0,1,4,15,1,98,211,0,0,15,1,28,213,0,0,15,1,133,214,0,0,17,1,119,205,0,0,1,4,19, 
  58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,37,0,0,0,3,0,1,2,21,0,127,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,240,3,128,73,80,1,128, 
  38,144,131,128,55,0,3,128,66,112,66,128,70,224,1,128,19,55,0,0,0,171,0,0,0,1,0,17,1,214,210,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1,214,210,0,0,1,19,55,0,0,0,169,0,0, 
  0,1,0,17,1,214,210,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,214,210,0,0,1,19,12,0,0,0,37,0,0,0,3,0,1,1,2,19,12,0,0,0,37,0,0,0,3,0,1,21,1,47,0,0,0, 
  92,96,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,4,17,1,157,211,0,0,1,8,19,12,0,0,0,38,0,0,0,3,0,1,21,1,242,0,0,0,11, 
  97,1,0,10,0,0,0,3,0,0,0,104,232,2,128,1,208,4,128,2,208,4,128,91,120,70,129,92,208,1,128,93,184,3,128,6,208,4,128,7,208,132,128,99,216,4,128,103,72,5,128,4,19,58,0,0,0,177,0,0, 
  0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,36,0,0,0,5,0,1,4,15,1,16,213,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,4,19,58,0,0,0,178,0, 
  0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,36,0,0,0,5,0,1,8,4,15,1,144,212,0,0,17,1,28,3,0,0,1,4,15,1,16,213,0,0,15,1,39,216,0,0,15,1,51,216,0, 
  0,15,1,165,216,0,0,15,1,174,217,0,0,17,1,86,215,0,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,36,0,0,0,5,0,1,2,21,0,127,0, 
  0,0,255,255,255,255,6,0,0,0,2,0,0,0,12,240,3,128,73,80,1,128,38,144,131,128,55,0,3,128,66,112,66,128,70,224,1,128,19,55,0,0,0,171,0,0,0,1,0,17,1,144,212,0,0,1,19,55,0,0, 
  0,170,0,0,0,1,0,17,1,144,212,0,0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,144,212,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,144,212,0,0,1,19,12,0,0,0,36,0,0,0,5, 
  0,1,1,2,19,12,0,0,0,36,0,0,0,5,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,13,176,0,128,15,1,28,213,0,0,17,1,64,213,0,0,1,2,21,1,47,0,0,0,185,97, 
  1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1,128,13,48,1,128,6,17,1,119,213,0,0,1,10,12,17,1,133,12,0,0,1,21,1,91,0,0,0,99,98,1,0,14,0,0, 
  0,3,0,0,0,64,88,66,131,1,80,2,128,2,80,194,129,67,88,194,129,92,152,2,128,93,152,2,128,6,80,2,128,7,80,66,128,63,88,2,129,66,88,2,128,91,152,66,128,99,152,2,128,103,152,2,128,104,152,2, 
  128,10,12,17,1,211,213,0,0,1,12,17,1,133,12,0,0,1,2,21,1,47,0,0,0,92,96,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17, 
  1,3,214,0,0,1,2,21,1,117,0,0,0,91,99,1,0,8,0,0,0,3,0,0,0,64,120,2,128,1,0,2,128,2,0,194,128,67,144,1,128,63,8,2,128,66,232,2,128,6,0,2,128,7,0,66,127,4,15,1, 
  121,214,0,0,17,1,183,205,0,0,1,8,4,15,1,121,214,0,0,17,1,24,194,0,0,1,4,15,1,121,214,0,0,17,1,119,205,0,0,1,4,19,36,0,0,0,124,0,0,0,1,0,19,13,0,0,0,44,0,0, 
  0,3,0,1,2,19,13,0,0,0,44,0,0,0,3,0,1,19,13,0,0,0,43,0,0,0,1,0,1,21,1,88,0,0,0,81,45,1,0,6,0,0,0,2,0,0,0,6,184,66,129,1,184,194,128,2,184,130,127,7, 
  184,2,128,53,80,1,128,58,240,1,128,4,15,1,62,215,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,234,214,0,0,17,1,115,178,0,0,1,8,2,21,0, 
  83,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,168,129,128,77,16,1,128,63,24,1,128,11,8,66,127,1,19,47,0,0,0,155,0,0,0,1,0,17,1,234,214,0,0,1,19,77,0,0,0,226,0,0,0, 
  2,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,234,214,0,0,1,2,19,77,0,0,0,226,0,0,0,2,0,1,19,56,0,0,0,173,0,0,0,1,0,1,21,1,88,0,0,0,81,45,1,0,6,0,0,0, 
  2,0,0,0,6,24,66,129,1,24,194,128,2,24,130,127,7,24,2,128,53,32,2,128,58,80,1,128,4,19,35,0,0,0,113,0,0,0,1,0,15,1,187,215,0,0,17,1,115,178,0,0,1,8,4,15,1,175,215,0, 
  0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,19,76,0,0,0,225,0,0,0,2,0,1,21,0,83,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,76,16,1,128,47,168,65,128,63,24,1,128,11,8,130, 
  127,1,19,47,0,0,0,155,0,0,0,1,0,17,1,187,215,0,0,1,19,76,0,0,0,225,0,0,0,2,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,187,215,0,0,1,2,19,0,0,0,0,0,0,0,0, 
  3,0,1,19,12,0,0,0,42,0,0,0,1,0,1,19,38,0,0,0,133,0,0,0,1,0,1,21,1,47,0,0,0,92,96,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,193,128,2,112,129,127,7,112,1, 
  128,13,48,1,128,4,17,1,99,216,0,0,1,8,2,21,1,53,0,0,0,186,99,1,0,5,0,0,0,2,0,0,0,104,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,8,4,15,1,153,216,0, 
  0,17,1,145,214,0,0,1,2,19,56,0,0,0,172,0,0,0,3,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,57,176,0,128,15,1,165,216,0,0,17,1,201,216,0,0,1,2,21,1,47, 
  0,0,0,92,96,1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,10,6,17,1,0,217,0,0,1,12,17,1,133,12,0,0,1,21,1,59,0,0,0,13,100, 
  1,0,6,0,0,0,2,0,0,0,104,80,1,128,1,144,1,128,2,144,129,128,7,144,129,128,6,144,1,128,103,152,1,128,12,17,1,133,12,0,0,1,10,12,17,1,60,217,0,0,1,2,21,1,47,0,0,0,92,96, 
  1,0,5,0,0,0,2,0,0,0,6,48,1,128,1,48,193,128,2,48,129,127,7,48,1,128,13,56,1,128,8,4,17,1,108,217,0,0,1,2,21,1,53,0,0,0,139,100,1,0,5,0,0,0,2,0,0,0,6,160, 
  1,128,1,160,1,128,2,160,129,127,7,160,65,128,103,48,1,128,4,15,1,162,217,0,0,17,1,86,215,0,0,1,8,2,19,57,0,0,0,175,0,0,0,3,0,1,19,57,0,0,0,174,0,0,0,1,0,1,21,1, 
  132,2,0,0,109,25,1,0,21,0,0,0,4,0,0,0,64,224,13,128,1,80,8,128,2,80,136,128,67,248,196,131,66,88,11,128,70,48,3,128,6,80,200,127,7,80,200,129,72,88,8,131,73,248,5,128,74,144,10,128, 
  75,24,77,129,76,136,71,129,93,224,14,128,71,192,70,129,63,248,3,128,91,80,16,128,92,32,9,128,99,168,12,128,103,192,17,128,104,32,19,128,4,19,37,0,0,0,129,0,0,0,1,0,15,1,213,220,0,0,17,1, 
  38,209,0,0,1,4,15,1,213,220,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,24,194,0,0,1,4,15,1,213,220,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209, 
  0,0,17,1,183,205,0,0,1,4,19,37,0,0,0,128,0,0,0,1,0,15,1,213,220,0,0,17,1,38,209,0,0,1,4,19,37,0,0,0,125,0,0,0,1,0,15,1,213,220,0,0,17,1,38,209,0,0,1,4, 
  19,37,0,0,0,126,0,0,0,1,0,15,1,213,220,0,0,17,1,38,209,0,0,1,8,4,19,37,0,0,0,130,0,0,0,1,0,15,1,213,220,0,0,17,1,38,209,0,0,1,4,19,58,0,0,0,177,0,0,0, 
  1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,37,0,0,0,131,0,0,0,1,0,15,1,213,220,0,0,17,1,38,209,0,0, 
  1,4,19,36,0,0,0,124,0,0,0,1,0,19,14,0,0,0,45,0,0,0,1,0,15,1,213,220,0,0,15,1,247,205,0,0,17,1,177,207,0,0,1,4,15,1,63,220,0,0,17,1,28,3,0,0,1,4,19,37, 
  0,0,0,127,0,0,0,1,0,15,1,213,220,0,0,17,1,38,209,0,0,1,4,15,1,213,220,0,0,15,1,247,205,0,0,15,1,177,207,0,0,15,1,26,209,0,0,17,1,119,205,0,0,1,4,19,58,0,0,0, 
  178,0,0,0,1,0,19,38,0,0,0,134,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,19,58,0,0,0,176,0,0,0,1,0,19,38,0,0,0,134,0,0, 
  0,1,0,19,12,0,0,0,42,0,0,0,1,0,19,0,0,0,0,1,0,0,0,3,0,1,4,15,1,213,220,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,51,216,0,0,15,1,165,216,0,0,15,1,174, 
  217,0,0,17,1,86,215,0,0,1,4,15,1,213,220,0,0,15,1,27,216,0,0,15,1,39,216,0,0,15,1,74,215,0,0,17,1,145,214,0,0,1,2,21,0,149,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,0,160,4,129,73,112,1,128,38,176,195,128,55,32,3,128,12,64,4,128,66,144,66,128,70,0,2,128,19,55,0,0,0,171,0,0,0,1,0,17,1,63,220,0,0,1,19,55,0,0,0,170,0,0,0,1,0,17,1, 
  63,220,0,0,1,19,55,0,0,0,169,0,0,0,1,0,17,1,63,220,0,0,1,19,38,0,0,0,132,0,0,0,1,0,17,1,63,220,0,0,1,19,12,0,0,0,42,0,0,0,1,0,17,1,63,220,0,0,1,19, 
  0,0,0,0,1,0,0,0,3,0,1,1,2,19,0,0,0,0,1,0,0,0,3,0,1,13,15,1,225,220,0,0,17,1,239,220,0,0,1,21,7,54,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0, 
  128,4,15,1,70,224,0,0,15,1,194,222,0,0,15,1,170,222,0,0,15,1,182,222,0,0,17,1,55,222,0,0,1,21,9,17,1,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,168,2,128,5,152,5,128,2, 
  48,129,128,3,32,4,128,6,16,7,128,4,19,15,0,0,0,49,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,70,224,0,0,17,1,194,222,0,0,1,4,19, 
  15,0,0,0,47,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,70,224,0,0,17,1,194,222,0,0,1,4,19,15,0,0,0,48,0,0,0,1,0,19,2,0, 
  0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,70,224,0,0,17,1,194,222,0,0,1,4,19,15,0,0,0,50,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0, 
  5,0,0,0,1,0,15,1,70,224,0,0,17,1,194,222,0,0,1,4,19,15,0,0,0,51,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,5,0,0,0,1,0,15,1,70,224,0,0,17, 
  1,194,222,0,0,1,2,21,9,103,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,152,1,128,5,104,2,128,2,48,129,128,3,0,2,128,6,208,2,128,4,19,16,0,0,0,54,0,0,0,2,0,1,4,19, 
  16,0,0,0,52,0,0,0,2,0,1,4,19,16,0,0,0,53,0,0,0,2,0,1,4,19,16,0,0,0,55,0,0,0,2,0,1,4,19,16,0,0,0,56,0,0,0,2,0,1,19,16,0,0,0,57,0,0,0,1, 
  0,1,19,3,0,0,0,5,0,0,0,1,0,1,19,2,0,0,0,4,0,0,0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,15,1,194,222,0,0,17,1,230,222,0,0, 
  1,2,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,12,17,1,58,223,0,0,1,21,9,46,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48, 
  129,128,3,48,1,128,6,48,1,128,12,17,1,58,223,0,0,1,12,17,1,133,12,0,0,1,21,7,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,4,15,1,58,224,0,0,15,1,182,222,0, 
  0,17,1,55,222,0,0,1,21,9,213,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,72,2,128,5,120,4,128,2,48,129,128,3,96,3,128,6,144,5,128,4,19,15,0,0,0,49,0,0,0,1,0,19,2, 
  0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,15,0,0,0,47,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,15,0, 
  0,0,48,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,4,19,15,0,0,0,50,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6, 
  0,0,0,2,0,1,4,19,15,0,0,0,51,0,0,0,1,0,19,2,0,0,0,3,0,0,0,1,0,19,3,0,0,0,6,0,0,0,2,0,1,2,19,3,0,0,0,6,0,0,0,2,0,1,19,1,0,0,0,2, 
  0,0,0,1,0,1,15,1,225,220,0,0,17,1,95,224,0,0,1,21,1,95,1,0,0,222,100,1,0,13,0,0,0,3,0,0,0,24,0,197,130,1,80,200,129,2,80,200,129,83,0,4,128,23,48,131,129,21,88,9, 
  128,6,80,8,128,7,80,72,127,25,40,10,129,58,208,5,128,79,80,7,128,80,48,2,128,81,88,8,128,4,15,1,143,15,1,0,15,1,107,16,1,0,15,1,98,17,1,0,15,1,110,17,1,0,17,1,118,13,1,0, 
  1,4,15,1,35,12,1,0,15,1,47,12,1,0,15,1,106,13,1,0,17,1,19,8,1,0,1,4,15,1,143,15,1,0,15,1,107,16,1,0,15,1,98,17,1,0,15,1,7,8,1,0,17,1,79,5,1,0,1,4, 
  15,1,35,12,1,0,15,1,47,12,1,0,15,1,67,5,1,0,17,1,51,1,1,0,1,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,15,1,35,12,1,0,15,1,47,12,1,0, 
  15,1,39,1,1,0,17,1,91,0,0,0,1,4,15,1,143,15,1,0,15,1,107,16,1,0,15,1,98,17,1,0,15,1,27,1,1,0,17,1,197,254,0,0,1,8,4,15,1,143,15,1,0,15,1,107,16,1,0,15, 
  1,98,17,1,0,15,1,185,254,0,0,17,1,121,254,0,0,1,4,15,1,35,12,1,0,15,1,47,12,1,0,15,1,109,254,0,0,17,1,93,250,0,0,1,4,15,1,35,12,1,0,15,1,47,12,1,0,15,1,81, 
  250,0,0,17,1,191,225,0,0,1,2,21,1,88,0,0,0,81,45,1,0,6,0,0,0,2,0,0,0,6,240,65,129,1,240,193,128,2,240,129,127,7,240,1,128,53,80,1,128,58,248,1,128,4,15,1,103,226,0,0, 
  15,1,59,178,0,0,17,1,124,13,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,24,226,0,0,17,1,115,178,0,0,1,2,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,47,160, 
  129,128,21,216,1,128,63,16,1,128,11,224,65,127,19,47,0,0,0,155,0,0,0,1,0,17,1,24,226,0,0,1,17,1,103,226,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,24,226,0,0,1,2,21, 
  1,47,0,0,0,209,107,1,0,5,0,0,0,2,0,0,0,20,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,151,226,0,0,1,8,2,21,1,162,2,0,0,191,45,1,0,12,0,0, 
  0,3,0,0,0,58,136,82,130,1,48,203,129,2,48,139,127,94,56,11,130,44,80,19,128,53,16,66,129,6,48,75,127,7,48,11,128,105,136,13,128,114,8,16,128,125,144,4,128,126,224,7,128,4,15,1,69,250,0,0, 
  15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0, 
  15,1,59,178,0,0,17,1,124,13,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1, 
  69,250,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4, 
  19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,69,250,0,0,15,1,218,115,0,0,15,1,230,115, 
  0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,8,4,15,1,69,250,0,0,15,1,218,115,0,0,15, 
  1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1, 
  4,15,1,69,250,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0, 
  0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,69,250,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111, 
  159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,58,229,0,0,17,1, 
  115,178,0,0,1,4,15,1,69,250,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,90,115,0,0,17,1,115,91,0,0, 
  1,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,45,88,134,128,81,224,130,130,61,104,4,128,11,192,6,128,60,208,4,128,21,184,198,126,46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128, 
  95,120,2,128,97,16,2,128,15,1,58,229,0,0,17,1,13,250,0,0,1,15,1,58,229,0,0,17,1,209,249,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,58,229,0,0,1,19,47,0,0,0,155,0,0, 
  0,1,0,17,1,58,229,0,0,1,15,1,58,229,0,0,17,1,149,249,0,0,1,15,1,58,229,0,0,17,1,97,230,0,0,1,19,46,0,0,0,151,0,0,0,1,0,17,1,58,229,0,0,1,19,97,0,0,0,23, 
  1,0,0,1,0,17,1,58,229,0,0,1,15,1,58,229,0,0,17,1,37,230,0,0,1,19,21,0,0,0,65,0,0,0,4,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,58,229,0,0,1,2,21,1,47, 
  0,0,0,35,108,1,0,5,0,0,0,2,0,0,0,84,56,1,128,1,48,1,128,2,48,129,128,7,48,1,128,6,48,1,128,10,12,17,1,228,19,0,0,1,12,19,45,0,0,0,150,0,0,0,1,0,1,21,1,143, 
  0,0,0,193,108,1,0,13,0,0,0,3,0,0,0,58,120,3,130,1,48,3,130,2,48,131,127,94,248,67,130,44,176,2,128,53,48,130,129,6,48,67,127,7,48,3,128,98,112,130,128,105,240,2,128,114,56,3,128,125, 
  56,4,128,126,184,3,128,12,17,1,5,41,0,0,1,12,17,1,220,38,0,0,1,12,17,1,179,36,0,0,1,12,17,1,138,34,0,0,1,10,12,17,1,97,32,0,0,1,6,17,1,253,230,0,0,1,12,17,1,56, 
  30,0,0,1,12,17,1,230,25,0,0,1,12,17,1,165,23,0,0,1,12,19,62,0,0,0,190,0,0,0,1,0,1,21,1,212,0,0,0,176,109,1,0,19,0,0,0,4,0,0,0,128,56,5,128,1,176,4,131,2, 
  176,132,131,3,56,198,131,4,56,6,129,53,240,2,128,6,176,4,128,7,176,4,128,84,112,3,128,105,112,4,128,94,248,5,128,11,120,5,128,44,48,4,128,97,240,3,128,62,184,5,127,127,48,3,128,98,176,67,128,114, 
  248,4,128,115,184,4,128,12,17,1,5,41,0,0,1,12,17,1,108,247,0,0,1,12,17,1,67,245,0,0,1,12,17,1,220,38,0,0,1,12,17,1,241,240,0,0,1,12,17,1,179,36,0,0,1,12,17,1,138,34, 
  0,0,1,10,12,17,1,200,238,0,0,1,12,17,1,97,32,0,0,1,12,17,1,159,236,0,0,1,12,17,1,118,234,0,0,1,12,17,1,217,231,0,0,1,12,17,1,230,25,0,0,1,12,19,62,0,0,0,190,0, 
  0,0,1,0,1,12,17,1,26,243,0,0,1,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,32,9,130,1,136,11,130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7, 
  136,11,128,98,48,130,128,105,240,5,128,114,232,9,128,125,32,7,128,126,240,3,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1, 
  0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0, 
  1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1, 
  102,115,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,114,233,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0, 
  0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0, 
  1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,40,130,128,81,72,195,128,95,16,4,128,11,184,2,128,97,168,3,128,61,32,2,128,62,32,2,128,47,144,65,126,19,97,0,0,0,23,1,0, 
  0,1,0,17,1,114,233,0,0,1,1,19,47,0,0,0,155,0,0,0,1,0,17,1,114,233,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,114,233,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15, 
  1,114,233,0,0,17,1,62,234,0,0,1,15,1,114,233,0,0,17,1,2,234,0,0,1,2,21,1,47,0,0,0,165,84,1,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,48,65,127, 
  118,48,1,128,12,17,1,48,83,0,0,1,10,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,43,0,0,0,181,85,1,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,16,1,128, 
  12,17,1,162,86,0,0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94,240,66,130,44,80,10,128, 
  53,144,139,129,6,192,74,127,7,192,10,128,98,48,130,128,105,240,5,128,114,32,9,128,125,32,7,128,126,240,3,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74, 
  25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0, 
  1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103, 
  91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0, 
  0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17, 
  1,115,91,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,15,236,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59, 
  178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,131,128,81,24,196,128,95,32,3,128,11,144,2,128,97,144,1,128,61,248,1,128,62,248,1,128,47,0,66, 
  126,15,1,15,236,0,0,17,1,62,234,0,0,1,1,19,97,0,0,0,23,1,0,0,1,0,17,1,15,236,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,15,236,0,0,1,15,1,15,236,0,0,17,1,2, 
  234,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,15,236,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,3,130,1,136,11, 
  130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0, 
  0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,56,238,0,0,17, 
  1,115,178,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78, 
  174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0, 
  19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15, 
  1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1, 
  102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,40,130,128,81,176,195,128,95,72,3,128,11,144,1,128,97,16, 
  4,128,61,32,2,128,62,32,2,128,47,184,66,126,19,47,0,0,0,154,0,0,0,1,0,17,1,56,238,0,0,1,1,19,47,0,0,0,155,0,0,0,1,0,17,1,56,238,0,0,1,19,97,0,0,0,23,1,0,0, 
  1,0,17,1,56,238,0,0,1,15,1,56,238,0,0,17,1,2,234,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,15,1,56,238,0,0,17,1,62,234,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0, 
  0,0,3,0,0,0,58,240,5,130,1,136,11,130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,240,3,128,4,19, 
  84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0, 
  14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19, 
  35,0,0,0,113,0,0,0,1,0,15,1,97,240,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0, 
  1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1, 
  102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15, 
  1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,128,131,128,81, 
  32,195,128,95,184,2,128,11,40,2,128,97,16,4,128,61,144,1,128,62,144,1,128,47,152,65,126,1,19,97,0,0,0,23,1,0,0,1,0,17,1,97,240,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,97, 
  240,0,0,1,15,1,97,240,0,0,17,1,2,234,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,97,240,0,0,1,15,1,97,240,0,0,17,1,62,234,0,0,1, 
  2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,200,10,130,1,192,10,130,2,192,138,127,94,240,66,130,44,80,10,128,53,144,139,129,6,192,74,127,7,192,10,128,98,48,130,128,105,240,5,128,114, 
  32,9,128,125,32,7,128,126,240,3,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0, 
  17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1, 
  78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1, 
  0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0, 
  15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1, 
  138,242,0,0,17,1,115,178,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255, 
  8,0,0,0,3,0,0,0,63,96,130,128,81,24,196,128,95,144,1,128,11,248,2,128,97,248,1,128,61,240,2,128,62,240,2,128,47,136,67,126,15,1,138,242,0,0,17,1,2,234,0,0,1,15,1,138,242,0,0,17, 
  1,62,234,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,138,242,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,138,242,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,138,242,0,0, 
  1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,3,130,1,136,11,130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127, 
  7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0, 
  0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,179,244,0,0,17,1,115,178,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0, 
  0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174, 
  0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1, 
  0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90, 
  0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0, 
  0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,232,131,128,81,136,195,128,95,152,1,128,11,248,2,128,97,0,2,128,61,144,1,128,62,144,1,128,47,104,66,126,1,15,1,179,244,0,0, 
  17,1,2,234,0,0,1,15,1,179,244,0,0,17,1,62,234,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,179,244,0,0,1,19,47,0,0,0,154,0,0,0,1,0,17,1,179,244,0,0,1,19,61,0,0, 
  0,187,0,0,0,2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,179,244,0,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,240,3,130,1,136,11,130,2,136,139,127,94,240,66, 
  130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,184,6,128,114,232,9,128,125,232,7,128,126,184,4,128,4,19,84,0,0,0,2,1,0,0,1,0,19,62,0,0,0,189,0,0,0,2,0, 
  1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,220,246,0,0,17,1,115,178,0,0,1,4,19, 
  94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0, 
  0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0, 
  0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102, 
  115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90, 
  0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,136,131,128,81,24,196,128,95,144,1,128,11,248,2,128,97,0,2,128,61,248,1,128,62,248, 
  1,128,47,104,66,126,15,1,220,246,0,0,17,1,2,234,0,0,1,1,15,1,220,246,0,0,17,1,62,234,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,220,246,0,0,1,19,47,0,0,0,154,0,0,0, 
  1,0,17,1,220,246,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,220,246,0,0,1,19,61,0,0,0,187,0,0,0,2,0,1,2,21,1,152,1,0,0,108,51,1,0,13,0,0,0,3,0,0,0,58,80, 
  10,130,1,136,11,130,2,136,139,127,94,240,66,130,44,24,11,128,53,144,139,129,6,136,75,127,7,136,11,128,98,48,130,128,105,240,5,128,114,32,9,128,125,32,7,128,126,240,3,128,4,19,84,0,0,0,2,1,0,0, 
  1,0,19,62,0,0,0,189,0,0,0,2,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78, 
  0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78, 
  174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0, 
  1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,74,25,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,74,25,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146, 
  90,0,0,17,1,238,89,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,5,249,0,0,17,1,115,178,0,0,1,4,15,1,62,25,0,0,17,1,115,91,0,0,1,8,4,15,1,74,25,0,0,15,1,78, 
  174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,2,21,0,143,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,63,240,130,128,81,144,194,128,95,144,1,128,11, 
  0,2,128,97,16,4,128,61,248,1,128,62,248,1,128,47,128,67,126,15,1,5,249,0,0,17,1,2,234,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,5,249,0,0,1,19,61,0,0,0,187,0,0,0, 
  2,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,5,249,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,5,249,0,0,1,15,1,5,249,0,0,17,1,62,234,0,0,1,2,21,1,47,0,0,0,106, 
  110,1,0,5,0,0,0,2,0,0,0,6,112,1,129,1,112,1,128,2,112,129,127,7,112,1,128,62,48,1,128,12,17,1,106,43,0,0,1,10,12,19,60,0,0,0,184,0,0,0,1,0,1,21,1,47,0,0,0,8, 
  111,1,0,5,0,0,0,2,0,0,0,115,48,1,128,1,112,1,128,2,112,129,128,11,48,65,127,118,48,1,128,12,17,1,48,83,0,0,1,10,12,19,81,0,0,0,253,0,0,0,1,0,1,21,1,43,0,0,0,13, 
  112,1,0,4,0,0,0,2,0,0,0,128,16,1,128,1,80,1,128,2,80,1,128,127,16,1,128,12,17,1,162,86,0,0,1,10,12,19,95,0,0,0,21,1,0,0,1,0,1,19,21,0,0,0,65,0,0,0,4,0, 
  1,19,6,0,0,0,14,0,0,0,1,0,1,21,1,69,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,32,2,129,1,32,2,128,2,32,130,127,7,32,2,128,58,48,1,128,4,19,35,0,0,0,113,0,0, 
  0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,163,250,0,0,1,8,2,21,1,47,0,0,0,209,107,1,0,5,0,0,0,2,0,0,0,20,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1, 
  128,4,17,1,211,250,0,0,1,8,2,21,1,162,2,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,200,81,130,1,80,198,129,2,80,134,127,94,120,15,130,44,144,4,128,53,16,66,129,6,80,70,127,7,80,6, 
  128,105,144,18,128,114,248,12,128,125,88,6,128,126,168,9,128,4,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0, 
  15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158, 
  0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,90,115,0,0,17,1,115,91,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0, 
  0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0, 
  0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24, 
  1,0,0,1,0,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17, 
  1,102,115,0,0,1,4,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0, 
  0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36, 
  159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,118,253,0,0,17,1, 
  115,178,0,0,1,4,15,1,97,254,0,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0, 
  15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,61,104,4,128,81,224,130,130,18,184,6,128,11,192,6,128, 
  60,208,4,128,45,88,198,126,46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16,2,128,15,1,118,253,0,0,17,1,13,250,0,0,1,15,1,118,253,0,0,17,1,209,249,0,0,1,19,61, 
  0,0,0,185,0,0,0,1,0,17,1,118,253,0,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,118,253,0,0,1,15,1,118,253,0,0,17,1,149,249,0,0,1,15,1,118,253,0,0,17,1,97,230,0,0,1, 
  19,46,0,0,0,151,0,0,0,1,0,17,1,118,253,0,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,118,253,0,0,1,15,1,118,253,0,0,17,1,37,230,0,0,1,19,18,0,0,0,62,0,0,0,4,0, 
  1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,118,253,0,0,1,2,19,18,0,0,0,62,0,0,0,4,0,1,19,6,0,0,0,11,0,0,0,1,0,1,21,1,63,0,0,0,53,20,1,0,5,0,0,0,2, 
  0,0,0,6,240,1,129,1,240,1,128,2,240,129,127,7,240,1,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,42,0,0,0,146,0,0,0,2,0,1,8,2,19,17,0,0,0,60,0,0,0,1,0, 
  1,21,1,88,0,0,0,81,45,1,0,6,0,0,0,2,0,0,0,6,80,65,129,1,80,193,128,2,80,129,127,7,80,1,128,53,88,1,128,58,248,1,128,8,4,15,1,212,0,1,0,15,1,59,178,0,0,17,1,124, 
  13,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,137,255,0,0,17,1,30,255,0,0,1,2,21,1,39,0,0,0,19,113,1,0,3,0,0,0,1,0,0,0,2,48,1,128,1,48,65,128,97,240,0,128, 
  4,17,1,81,255,0,0,1,8,19,11,0,0,0,35,0,0,0,1,0,1,21,1,55,0,0,0,62,61,1,0,3,0,0,0,1,0,0,0,2,176,129,128,1,176,1,128,58,240,0,128,4,19,35,0,0,0,113,0,0, 
  0,1,0,19,63,0,0,0,191,0,0,0,3,0,1,8,2,21,0,78,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,39,216,65,128,47,160,65,128,63,16,1,128,11,224,65,127,19,47,0,0,0,155,0,0,0, 
  1,0,17,1,137,255,0,0,1,17,1,216,255,0,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,137,255,0,0,1,2,21,1,51,0,0,0,159,113,1,0,6,0,0,0,2,0,0,0,6,80,65,129,1,80, 
  193,128,2,80,129,127,7,80,1,128,77,88,1,128,78,88,1,128,10,12,17,1,24,0,1,0,1,12,19,39,0,0,0,137,0,0,0,2,0,1,21,1,59,0,0,0,76,115,1,0,6,0,0,0,2,0,0,0,6,144, 
  65,129,1,144,193,128,2,144,129,127,7,144,1,128,77,80,1,128,78,152,1,128,4,17,1,148,0,1,0,1,8,4,17,1,84,0,1,0,1,2,21,1,63,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,240, 
  1,129,1,240,1,128,2,240,129,127,7,240,1,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,39,0,0,0,136,0,0,0,4,0,1,8,2,21,1,63,0,0,0,53,20,1,0,5,0,0,0,2,0, 
  0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,39,0,0,0,135,0,0,0,4,0,1,2,21,1,59,0,0,0,76,115,1,0,6,0, 
  0,0,2,0,0,0,6,144,65,129,1,144,193,128,2,144,129,127,7,144,1,128,77,80,1,128,78,152,1,128,4,17,1,148,0,1,0,1,8,4,17,1,84,0,1,0,1,19,39,0,0,0,137,0,0,0,2,0,1,19, 
  17,0,0,0,58,0,0,0,1,0,1,19,6,0,0,0,15,0,0,0,1,0,1,21,1,69,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1,128,2,48,129,127,7,48,1,128,58,56,1, 
  128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,121,1,1,0,1,2,21,1,47,0,0,0,209,107,1,0,5,0,0,0,2,0,0,0,20,56,1,128,1,48,1,128,2, 
  48,129,128,7,48,1,128,6,48,1,128,8,4,17,1,169,1,1,0,1,2,21,1,162,2,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58,96,73,130,1,120,205,129,2,120,141,127,94,16,7,130,44,208,16,128,53, 
  144,68,129,6,120,77,127,7,120,13,128,105,144,18,128,114,16,2,128,125,128,13,128,126,40,10,128,4,15,1,55,5,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1, 
  36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,55,5,1,0,15,1,218,115,0,0, 
  15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90,0,0,15,1,59,178,0,0, 
  17,1,124,13,0,0,1,4,15,1,55,5,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174, 
  0,0,15,1,102,115,0,0,15,1,91,91,0,0,17,1,31,16,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,15,1,76,4,1,0,17,1,115,178,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78, 
  0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,55,5,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0, 
  0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0, 
  0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,55,5,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0, 
  15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,55,5,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159, 
  0,0,15,1,90,115,0,0,17,1,115,91,0,0,1,4,15,1,55,5,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1, 
  66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,2,21,0,234,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,60,208,4,128,81,224, 
  130,130,61,104,4,128,11,192,6,128,20,184,6,127,45,88,70,127,46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16,2,128,15,1,76,4,1,0,17,1,13,250,0,0,1,15,1,76,4,1, 
  0,17,1,209,249,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,76,4,1,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,76,4,1,0,1,15,1,76,4,1,0,17,1,149,249,0,0,1,15,1,76, 
  4,1,0,17,1,97,230,0,0,1,19,46,0,0,0,151,0,0,0,1,0,17,1,76,4,1,0,1,19,97,0,0,0,23,1,0,0,1,0,17,1,76,4,1,0,1,15,1,76,4,1,0,17,1,37,230,0,0,1,19, 
  20,0,0,0,64,0,0,0,4,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,76,4,1,0,1,2,19,20,0,0,0,64,0,0,0,4,0,1,19,6,0,0,0,13,0,0,0,1,0,1,21,1,47,0,0, 
  0,5,42,1,0,5,0,0,0,2,0,0,0,6,112,1,128,1,112,1,128,2,112,129,127,7,112,65,128,11,48,1,128,4,17,1,127,5,1,0,1,8,2,21,1,5,1,0,0,100,89,1,0,9,0,0,0,3,0,0, 
  0,94,88,7,130,1,80,134,128,2,80,134,128,105,88,6,128,114,128,3,128,125,128,4,128,6,80,134,126,7,80,6,128,126,176,1,128,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0, 
  19,59,0,0,0,180,0,0,0,1,0,19,44,0,0,0,148,0,0,0,1,0,15,1,210,7,1,0,17,1,145,6,1,0,1,4,15,1,210,7,1,0,15,1,145,6,1,0,15,1,133,6,1,0,15,1,146,90,0,0, 
  17,1,238,89,0,0,1,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,44,0,0,0,148,0,0,0,1,0,15,1,210,7,1,0,17,1, 
  145,6,1,0,1,8,4,15,1,210,7,1,0,15,1,145,6,1,0,15,1,133,6,1,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,210,7,1,0,15,1,145,6,1,0,15,1,133,6,1,0,17,1,31, 
  16,0,0,1,2,19,44,0,0,0,148,0,0,0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,44,176,0,128,15,1,145,6,1,0,17,1,181,6,1,0,1,2,21,1,63,0,0,0,231, 
  115,1,0,9,0,0,0,3,0,0,0,94,184,1,130,1,176,129,128,2,176,129,128,105,184,1,128,114,184,1,128,125,184,1,128,6,176,129,126,7,176,1,128,126,184,1,128,10,12,17,1,252,6,1,0,1,12,17,1,133, 
  12,0,0,1,21,1,201,0,0,0,100,89,1,0,9,0,0,0,3,0,0,0,94,216,5,130,1,96,132,128,2,96,132,128,105,176,1,128,114,80,2,128,125,104,4,128,6,96,132,126,7,96,4,128,126,240,2,128,4,15, 
  1,198,7,1,0,15,1,103,91,0,0,17,1,170,90,0,0,1,4,15,1,198,7,1,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1, 
  0,19,59,0,0,0,180,0,0,0,1,0,19,44,0,0,0,149,0,0,0,2,0,1,8,4,19,93,0,0,0,13,1,0,0,1,0,19,78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19, 
  44,0,0,0,149,0,0,0,2,0,1,4,15,1,198,7,1,0,17,1,31,16,0,0,1,2,19,44,0,0,0,149,0,0,0,2,0,1,21,1,52,0,0,0,87,42,1,0,5,0,0,0,2,0,0,0,6,48,1,129, 
  1,48,1,128,2,48,129,127,7,48,1,128,82,56,1,128,8,4,19,43,0,0,0,147,0,0,0,4,0,1,2,19,17,0,0,0,61,0,0,0,1,0,1,21,1,69,0,0,0,53,20,1,0,5,0,0,0,2,0,0, 
  0,6,32,2,129,1,32,2,128,2,32,130,127,7,32,2,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,17,1,89,8,1,0,1,8,2,21,1,47,0,0,0,209, 
  107,1,0,5,0,0,0,2,0,0,0,20,48,1,128,1,112,1,128,2,112,129,128,7,112,1,128,6,112,1,128,4,17,1,137,8,1,0,1,8,2,21,1,162,2,0,0,191,45,1,0,12,0,0,0,3,0,0,0,58, 
  40,71,130,1,32,199,129,2,32,135,127,94,240,7,130,44,96,5,128,53,144,77,129,6,32,71,127,7,32,7,128,105,144,18,128,114,16,16,128,125,16,2,128,126,64,10,128,4,19,93,0,0,0,13,1,0,0,1,0,19, 
  78,0,0,0,227,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,23,12,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158, 
  0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,23,12,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1, 
  227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,90,115,0,0,17,1,115,91,0,0,1,8,4,19,35,0,0,0,113,0,0,0,1,0,15,1,44,11,1,0,17,1,115,178,0,0,1,4,15,1,23,12, 
  1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91, 
  0,0,17,1,31,16,0,0,1,4,19,94,0,0,0,14,1,0,0,1,0,19,78,0,0,0,228,0,0,0,1,0,19,59,0,0,0,180,0,0,0,1,0,19,97,0,0,0,24,1,0,0,1,0,15,1,23,12,1,0, 
  15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,17,1,102,115,0,0,1,4,15,1,23,12, 
  1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,158,90, 
  0,0,15,1,59,178,0,0,17,1,124,13,0,0,1,4,15,1,23,12,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0,15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1, 
  66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,146,90,0,0,17,1,238,89,0,0,1,4,15,1,23,12,1,0,15,1,218,115,0,0,15,1,230,115,0,0,15,1,215,158,0,0, 
  15,1,227,158,0,0,15,1,36,159,0,0,15,1,111,159,0,0,15,1,66,174,0,0,15,1,78,174,0,0,15,1,102,115,0,0,15,1,91,91,0,0,15,1,103,91,0,0,17,1,170,90,0,0,1,2,21,0,234,0, 
  0,0,255,255,255,255,12,0,0,0,3,0,0,0,19,184,6,128,81,224,130,130,61,104,4,128,11,192,70,127,60,208,4,128,45,88,70,127,46,240,133,128,47,96,133,128,62,0,4,128,63,112,67,128,95,120,2,128,97,16, 
  2,128,15,1,44,11,1,0,17,1,13,250,0,0,1,15,1,44,11,1,0,17,1,209,249,0,0,1,19,61,0,0,0,185,0,0,0,1,0,17,1,44,11,1,0,1,19,47,0,0,0,155,0,0,0,1,0,17,1,44, 
  11,1,0,1,15,1,44,11,1,0,17,1,149,249,0,0,1,15,1,44,11,1,0,17,1,97,230,0,0,1,19,46,0,0,0,151,0,0,0,1,0,17,1,44,11,1,0,1,19,97,0,0,0,23,1,0,0,1,0,17, 
  1,44,11,1,0,1,15,1,44,11,1,0,17,1,37,230,0,0,1,19,19,0,0,0,63,0,0,0,4,0,1,1,19,47,0,0,0,154,0,0,0,1,0,17,1,44,11,1,0,1,2,19,19,0,0,0,63,0,0,0, 
  4,0,1,19,4,0,0,0,8,0,0,0,1,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,6,176,0,128,15,1,47,12,1,0,17,1,83,12,1,0,1,2,21,1,63,0,0,0,105,116,1, 
  0,9,0,0,0,3,0,0,0,24,184,1,128,1,176,193,128,2,176,129,129,23,184,1,128,25,184,1,128,21,184,1,128,6,176,1,128,7,176,1,127,58,184,1,128,10,12,17,1,154,12,1,0,1,12,17,1,133,12,0, 
  0,1,21,1,147,0,0,0,105,116,1,0,9,0,0,0,3,0,0,0,24,176,1,128,1,144,194,128,2,144,130,129,23,40,4,128,25,152,2,128,21,32,2,128,6,144,2,128,7,144,2,127,58,8,3,128,4,15,1,94, 
  13,1,0,17,1,51,1,1,0,1,4,15,1,82,13,1,0,17,1,93,250,0,0,1,8,4,15,1,70,13,1,0,17,1,191,225,0,0,1,4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0, 
  1,0,15,1,58,13,1,0,17,1,91,0,0,0,1,4,15,1,46,13,1,0,17,1,19,8,1,0,1,2,19,6,0,0,0,17,0,0,0,2,0,1,19,6,0,0,0,20,0,0,0,2,0,1,19,6,0,0,0,19, 
  0,0,0,2,0,1,19,6,0,0,0,16,0,0,0,2,0,1,19,6,0,0,0,18,0,0,0,2,0,1,19,6,0,0,0,12,0,0,0,1,0,1,21,1,122,0,0,0,252,116,1,0,7,0,0,0,2,0,0,0, 
  16,64,2,128,1,56,2,129,2,56,130,128,7,56,2,128,6,56,130,128,17,112,1,128,18,8,3,128,4,19,41,0,0,0,142,0,0,0,1,0,15,1,167,14,1,0,17,1,241,13,1,0,1,8,4,19,41,0,0,0, 
  140,0,0,0,1,0,15,1,167,14,1,0,17,1,241,13,1,0,1,4,19,41,0,0,0,141,0,0,0,1,0,15,1,167,14,1,0,17,1,241,13,1,0,1,2,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,41,176,0,128,15,1,241,13,1,0,17,1,21,14,1,0,1,2,21,1,51,0,0,0,252,116,1,0,6,0,0,0,2,0,0,0,16,80,1,128,17,80,1,128,2,144,129,128,7,144,1,128,6,144,65,128,18, 
  80,1,128,12,17,1,80,14,1,0,1,10,12,17,1,133,12,0,0,1,21,1,86,0,0,0,252,116,1,0,7,0,0,0,2,0,0,0,16,72,2,128,1,216,1,129,2,216,129,128,7,216,1,128,6,216,129,128,17,112, 
  1,128,18,224,1,128,4,19,41,0,0,0,145,0,0,0,2,0,1,8,4,19,41,0,0,0,144,0,0,0,2,0,1,4,19,41,0,0,0,143,0,0,0,2,0,1,2,21,1,43,0,0,0,148,117,1,0,4,0,0, 
  0,2,0,0,0,6,80,1,128,1,16,1,128,2,80,129,127,7,80,1,128,4,17,1,211,14,1,0,1,8,2,21,1,59,0,0,0,76,115,1,0,6,0,0,0,2,0,0,0,6,208,65,129,1,208,193,128,2,208,129, 
  127,7,208,1,128,77,144,1,128,78,80,1,128,4,17,1,79,15,1,0,1,4,17,1,15,15,1,0,1,8,2,21,1,63,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,240,1,129,1,240,1,128,2,240,129, 
  127,7,240,1,128,58,48,1,128,4,19,35,0,0,0,113,0,0,0,1,0,19,40,0,0,0,138,0,0,0,5,0,1,8,2,21,1,63,0,0,0,53,20,1,0,5,0,0,0,2,0,0,0,6,48,1,129,1,48,1, 
  128,2,48,129,127,7,48,1,128,58,56,1,128,8,4,19,35,0,0,0,113,0,0,0,1,0,19,40,0,0,0,139,0,0,0,5,0,1,2,21,1,207,0,0,0,105,116,1,0,9,0,0,0,3,0,0,0,24,168,5, 
  128,1,176,193,128,2,176,129,129,23,216,4,128,25,184,1,128,21,8,4,128,6,176,1,128,7,176,1,127,58,136,2,128,8,4,15,1,95,16,1,0,15,1,47,12,1,0,15,1,81,250,0,0,17,1,191,225,0,0,1, 
  4,19,35,0,0,0,113,0,0,0,1,0,19,11,0,0,0,35,0,0,0,1,0,15,1,95,16,1,0,15,1,47,12,1,0,15,1,39,1,1,0,17,1,91,0,0,0,1,4,15,1,95,16,1,0,15,1,47,12,1, 
  0,15,1,109,254,0,0,17,1,93,250,0,0,1,4,15,1,95,16,1,0,15,1,47,12,1,0,15,1,106,13,1,0,17,1,19,8,1,0,1,4,15,1,95,16,1,0,15,1,47,12,1,0,15,1,67,5,1,0,17, 
  1,51,1,1,0,1,2,19,4,0,0,0,7,0,0,0,2,0,1,21,0,35,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,5,176,0,128,15,1,107,16,1,0,17,1,143,16,1,0,1,2,21,1,59,0,0, 
  0,222,100,1,0,8,0,0,0,3,0,0,0,80,152,1,128,1,144,1,129,2,144,1,128,83,152,1,128,79,152,1,128,81,152,1,128,6,144,1,128,7,144,65,127,10,12,17,1,210,16,1,0,1,12,17,1,133,12,0, 
  0,1,21,1,131,0,0,0,219,117,1,0,8,0,0,0,3,0,0,0,80,144,1,128,1,208,2,129,2,208,2,128,83,48,2,128,79,120,3,128,81,216,2,128,6,208,2,128,7,208,66,127,4,15,1,86,17,1,0,15, 
  1,110,17,1,0,17,1,118,13,1,0,1,4,15,1,86,17,1,0,15,1,7,8,1,0,17,1,79,5,1,0,1,8,4,15,1,86,17,1,0,15,1,185,254,0,0,17,1,121,254,0,0,1,4,15,1,86,17,1,0, 
  15,1,27,1,1,0,17,1,197,254,0,0,1,2,19,5,0,0,0,10,0,0,0,2,0,1,19,5,0,0,0,9,0,0,0,1,0,1,19,17,0,0,0,59,0,0,0,1,0,1,15,1,225,220,0,0,17,1,135,17, 
  1,0,1,21,1,64,0,0,0,93,120,1,0,2,0,0,0,1,0,0,0,28,208,0,128,27,104,1,128,4,19,23,0,0,0,67,0,0,0,1,0,17,1,236,17,1,0,1,4,19,22,0,0,0,66,0,0,0,1,0, 
  17,1,200,17,1,0,1,2,21,1,35,0,0,0,138,123,1,0,1,0,0,0,0,0,0,0,8,176,0,128,4,19,7,0,0,0,21,0,0,0,2,0,1,2,21,1,35,0,0,0,138,123,1,0,1,0,0,0,0,0, 
  0,0,8,176,0,128,4,19,7,0,0,0,22,0,0,0,2,0,1,2,15,1,225,220,0,0,17,1,29,18,1,0,1,21,1,208,1,0,0,135,60,1,0,23,0,0,0,4,0,0,0,32,16,198,132,1,88,201,131,2, 
  88,201,131,35,128,6,128,36,48,4,128,37,160,13,128,38,232,136,131,39,224,139,131,40,0,11,128,41,144,10,128,42,96,9,128,43,112,3,128,44,16,14,128,29,8,8,128,46,32,10,128,31,160,196,128,33,192,12,128,34, 
  48,77,129,47,80,12,128,48,120,8,128,54,112,11,128,87,240,6,128,98,16,5,128,4,19,27,0,0,0,93,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,1,4,15,1,220,82,0,0,17,1,116,65,0,0, 
  1,4,15,1,220,82,0,0,17,1,249,80,0,0,1,4,15,1,249,46,0,0,15,1,5,47,0,0,15,1,252,49,0,0,15,1,8,50,0,0,17,1,165,46,0,0,1,4,15,1,220,82,0,0,17,1,204,51,0,0, 
  1,4,15,1,220,82,0,0,17,1,75,80,0,0,1,4,19,49,0,0,0,157,0,0,0,1,0,19,27,0,0,0,94,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,1,4,15,1,97,82,0,0,17,1,10, 
  82,0,0,1,4,15,1,254,81,0,0,17,1,203,65,0,0,1,4,15,1,220,82,0,0,17,1,18,67,0,0,1,8,4,19,27,0,0,0,92,0,0,0,1,0,19,8,0,0,0,26,0,0,0,1,0,1,4,15,1, 
  254,81,0,0,17,1,244,79,0,0,1,4,15,1,192,51,0,0,17,1,144,50,0,0,1,4,15,1,220,82,0,0,17,1,157,79,0,0,1,4,15,1,186,72,0,0,17,1,105,67,0,0,1,4,15,1,220,82,0,0, 
  17,1,109,82,0,0,1,4,15,1,254,81,0,0,17,1,167,81,0,0,1,4,15,1,220,82,0,0,17,1,80,81,0,0,1,4,15,1,220,82,0,0,17,1,35,52,0,0,1,4,15,1,220,82,0,0,17,1,162,80, 
  0,0,1,4,15,1,145,79,0,0,17,1,198,72,0,0,1,2,15,1,225,220,0,0,17,1,251,19,1,0,1,21,7,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,123,176,0,128,4,17,1,2,60,0,0, 
  1,21,9,27,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,208,0,128,8,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,47,48,129,128,45,240,192,127,95,240,0,128,3, 
  17,1,124,23,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3, 
  17,1,124,23,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,198,22,1,0,1,3,17,1,195,20,1,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,22,21,1,0,1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3, 
  15,1,167,22,1,0,17,1,90,22,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,147,21,1,0,1,3,17,1,117,21,1,0,1,21,2,52,0, 
  0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,167,22,1,0,17,1,20,22,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,47,176,0,128,3,18,7,0,0,0,1,2,18,7,0,0,0,21,2,46,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48, 
  1,128,3,17,1,199,21,1,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,117,21,1,0,1,21,2,46,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4, 
  48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,199,21,1,0,1,2,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,1,21,2,46,0,0,0,255,255,255, 
  255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,20,22,1,0,1,1,21,4,23,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128, 
  1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,166,22,1,0,17,1,20,22,1,0,1,1,1,21,4,30,0,0, 
  0,255,255,255,255,1,0,0,0,0,0,0,0,42,176,0,128,3,17,1,117,21,1,0,1,2,21,2,59,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,160,1,128,2,48,129,128,3,48,1,128, 
  6,48,1,128,3,15,1,94,23,1,0,17,1,2,23,1,0,1,3,18,6,0,0,0,1,2,21,2,48,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128, 
  3,15,1,166,22,1,0,17,1,51,23,1,0,1,1,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,51,23,1,0,1,1,21,2, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,5,176,0,128,3,18,6,0,0,0,1,2,18,58,0,0,0,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3, 
  15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0, 
  17,1,224,23,1,0,1,1,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127, 
  3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,95,208,0,128,45,208,192,127,3,15,1,166,22,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,166,22,1,0,17,1,49,24, 
  1,0,1,1,18,58,0,0,0,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,61,208,0,128,47,16,193,127,3,17,1,37,25,1,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,208,0,128,33,8,1,128,3, 
  18,3,0,0,0,1,3,17,1,79,25,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,4,0,0,0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3, 
  0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,112,30,1,0,1,3,17,1,141,29,1,0,1,3,17,1,213,27,1,0,1,3, 
  17,1,90,27,1,0,1,3,17,1,254,26,1,0,1,3,17,1,162,26,1,0,1,3,17,1,8,26,1,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0, 
  128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,39,26,1,0,1,2,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,70,26,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,101,26,1,0,1,2,21,4,30,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,17,1,132,26,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,93,0,0,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,193,26,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,224,26,1,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,18,91,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,29,27,1,0,1,2,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,60,27,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,18,104,0,0,0,1,2,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,121,27,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,152,27,1,0,1,2, 
  21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,183,27,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,99,0,0,0,1, 
  2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,101,144,129,128,97,208,193,127,117,16,1,128,111,80,1,128,3,17,1,80,29,1,0,1,3,17,1,50,29,1,0,1,3,17,1,85,28,1,0,1,3, 
  17,1,24,28,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,55,28,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0, 
  128,3,18,92,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,116,28,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176, 
  0,128,3,17,1,147,28,1,0,1,2,18,71,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,183,28,1,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,115,176,0,128,3,17,1,214,28,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,245,28,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,105,176,0,128,3,17,1,20,29,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,76,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,112,176,0,128,3,18,70,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,111,29,1,0,1,2,21,4,29,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,103,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,172,29,1,0,1,2,21,4,42,0,0,0,255,255, 
  255,255,2,0,0,0,1,0,0,0,100,16,1,128,115,208,0,128,3,17,1,51,30,1,0,1,3,17,1,215,29,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1, 
  246,29,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,99,176,0,128,3,17,1,21,30,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3, 
  18,63,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,82,30,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128, 
  3,18,74,0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,104,80,1,128,101,144,1,128,107,16,1,128,99,208,193,127,3,17,1,207,32,1,0,1,3,17,1,115,32,1,0,1,3,17, 
  1,240,30,1,0,1,3,17,1,179,30,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,210,30,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,110,176,0,128,3,18,72,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,15,31,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,45,176,0,128,3,17,1,46,31,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,65,128,116,208,0,128,3,17,1,181,31,1,0,1,3,17,1,89,31,1,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,120,31,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,151,31,1, 
  0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,66,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,212,31, 
  1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,243,31,1,0,1,2,18,64,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176, 
  0,128,3,17,1,23,32,1,0,1,1,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,54,32,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  101,176,0,128,3,17,1,85,32,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,67,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,105,176,0,128,3,17,1,146,32,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,177,32,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,116,176,0,128,3,18,75,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,238,32,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0, 
  0,0,0,0,112,176,0,128,3,18,73,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,47,8,1,128,3,18,56,0,0,0,1,3,17,1,152,20,1,0,1,21,2,40, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,70,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,80,176,129,128, 
  45,112,1,128,84,48,1,128,47,240,65,128,95,112,1,128,3,17,1,4,37,1,0,1,3,17,1,124,23,1,0,1,3,17,1,217,33,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,73,34,1,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128, 
  3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128, 
  45,48,193,127,95,48,1,128,3,17,1,171,34,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204, 
  24,1,0,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,68,240,0,128,45,48,65,128,95,48,1,128,3,17,1,1,35,1,0,1,3,17,1,49,24,1, 
  0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0, 
  85,240,128,128,45,48,193,127,95,48,1,128,3,17,1,87,35,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1, 
  0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,67,240,128,128,45,48,193,127,95,48,1,128,3,17,1,173,35,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0, 
  255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,48,65,128, 
  95,48,1,128,3,17,1,3,36,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0, 
  21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,240,128,128,45,48,193,127,95,48,1,128,3,17,1,89,36,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,175, 
  36,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,45,0,0,0,255,255, 
  255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,40,65,128,95,40,1,128,3,18,101,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3, 
  208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,48,193,127,95,48,1,128,3,17,1,116,37,1,0,1,3,15,1,204,24, 
  1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23, 
  1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1,128,3,17,1,214,37,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0, 
  1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,77,240,128,128,45,48,193,127,95,48,1,128,3,17,1,44,38,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,73,240,128,128,45,48,193,127,95,48,1,128,3,17,1,130,38,1,0,1,3,17,1,49,24,1,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0, 
  128,45,48,65,128,95,48,1,128,3,17,1,216,38,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1, 
  18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,46,39,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,76,240,0,128,45,40,65,128,95,40,1, 
  128,3,18,102,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,21,4,52,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,40,104,1,128,47,40,65,128,123,240,0,128,3,18,11,0,0,0,1,3,17,1,152,20,1,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0, 
  0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,76,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,40,66,128,100,168,1,128,102,104,1,128,47,232,65,128,123,48, 
  1,128,3,18,11,0,0,0,1,3,17,1,14,41,1,0,1,3,17,1,85,40,1,0,1,3,17,1,152,20,1,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208, 
  0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,116,40,1,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,102,176,0,128,3,17,1,147,40,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,178,40,1,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,209,40,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,240,40,1,0,1,2,21,4,29,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,121,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,45,41,1,0,1,2,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,76,41,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,107,41,1,0,1,2,21, 
  4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,45,176,0,128,3,17,1,138,41,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,17,1,169,41,1,0,1, 
  2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,200,41,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,231,41,1, 
  0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,122,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,123,208,0,128,47,8,193,127,3, 
  18,11,0,0,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,125,208,0,128,47,8,193,127,3,18,82,0,0,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8, 
  1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,16,1,128,47,208,0,128,3,17,1,152,20,1,0,1,3,17,1,252,42,1,0,1,21, 
  2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,112, 
  65,128,92,208,0,128,3,15,1,179,44,1,0,15,1,79,44,1,0,17,1,35,44,1,0,1,3,17,1,103,43,1,0,1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128, 
  2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,179,44,1,0,17,1,79,44,1,0,1,2,18,125,0,0,0,21,4,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,92,176,0,128,3,15,1,179,44,1, 
  0,15,1,203,43,1,0,17,1,35,44,1,0,1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,179,44,1,0,17, 
  1,203,43,1,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,64,65,128,92,208,0,128,3,15,1,203,43,1,0,17,1,35,44,1,0,1,1,21,2,46,0,0,0,255,255,255,255,5,0, 
  0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,203,43,1,0,1,1,21,2,42,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1, 
  128,2,16,193,127,3,16,1,128,3,17,1,78,44,1,0,1,2,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,34,112,65,128,92,208,0,128,3,15,1,166,22,1,0,15,1,203,43,1,0,17,1, 
  35,44,1,0,1,1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,166,22,1,0,17,1,203,43,1,0,1,1,21, 
  4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,34,176,0,128,3,18,125,0,0,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,128,2,129,125,80,1,128,102,192,1,128,47, 
  64,130,128,100,0,2,128,123,136,1,128,3,18,82,0,0,0,1,3,18,11,0,0,0,1,3,17,1,14,41,1,0,1,3,17,1,85,40,1,0,1,3,17,1,152,20,1,0,1,3,18,53,0,0,0,1,21,2,40,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,16,1,128,45, 
  72,1,128,95,72,1,128,47,136,193,127,3,18,53,0,0,0,1,3,17,1,124,23,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128, 
  5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,176,1,128,47,96,67,129,34,224,2,128,91,232,65, 
  129,116,32,3,128,45,32,2,128,95,32,2,128,39,96,130,126,99,160,2,128,3,18,53,0,0,0,1,3,18,44,0,0,0,1,3,17,1,124,23,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3, 
  17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1, 
  3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,107,240,0,128,3,17,1,216,46,1,0,1,3, 
  15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0, 
  17,1,224,23,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,48,65,128,95,48,1,128,3,17,1,58,47,1,0,1,3,15,1,204,24,1,0,17,1, 
  49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1,18,105,0,0,0,21,4,29,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,40,176,0,128,3,18,94,0,0,0,1,1,18,58,0,0,0,21,4,51,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,58,240,0,128,45,40,65,128,95,40,1,128,3,18,114,0, 
  0,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1, 
  204,24,1,0,17,1,224,23,1,0,1,1,21,4,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,109,49,1,0,15,1,9,49,1,0,17,1,35,44,1,0,1,3,17, 
  1,55,48,1,0,1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,109,49,1,0,17,1,9,49,1,0,1,2,18, 
  126,0,0,0,21,4,53,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,109,49,1,0,15,1,177,48,1,0,17,1,35,44,1,0,1,3,18,126,0,0,0,1,21,2,52, 
  0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,109,49,1,0,17,1,177,48,1,0,1,18,126,0,0,0,17,1,109,49,1,0, 
  1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,64,1,128,3,15,1,177,48,1,0,17,1,35,44,1,0,1,1,21,2,46,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0, 
  4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,17,1,177,48,1,0,1,1,21,4,47,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,92,208,0,128,39,112,1,128,3,15,1,166,22, 
  1,0,15,1,177,48,1,0,17,1,35,44,1,0,1,1,21,2,52,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,4,48,1,128,5,48,1,128,2,48,129,128,3,48,1,128,6,48,1,128,3,15,1,166,22,1, 
  0,17,1,177,48,1,0,1,1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,39,176,0,128,3,18,126,0,0,0,1,2,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,124,240,0, 
  128,41,104,65,128,47,40,1,128,3,18,84,0,0,0,1,3,17,1,152,20,1,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1, 
  0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,124,208,0,128,47,8,1,128,3,18,84,0,0,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255, 
  255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,161,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,40,104,2,128,41,80,3,128, 
  34,72,132,129,91,24,195,129,36,48,194,129,45,136,3,128,47,200,196,128,39,200,195,127,58,216,2,128,95,136,3,128,99,8,4,128,116,136,68,128,124,160,2,128,3,18,98,0,0,0,1,3,18,53,0,0,0,1,3,18, 
  84,0,0,0,1,3,17,1,16,51,1,0,1,3,18,44,0,0,0,1,3,18,85,0,0,0,1,3,17,1,124,23,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3, 
  17,1,104,46,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3, 
  17,1,124,23,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,47,51,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0, 
  128,3,17,1,78,51,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,18,62,0,0,0,1,2,21,4,127,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,8, 
  2,128,47,184,67,129,34,56,3,128,91,64,66,129,36,208,65,129,45,120,2,128,95,120,2,128,39,184,130,126,99,248,2,128,116,120,3,128,3,18,98,0,0,0,1,3,18,53,0,0,0,1,3,18,44,0,0,0,1,3, 
  17,1,124,23,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,124,16, 
  1,128,41,200,1,128,58,72,1,128,47,136,1,128,3,18,84,0,0,0,1,3,17,1,16,51,1,0,1,3,17,1,152,20,1,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0, 
  0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,208,0,128,47,16,1,128,3,17,1,16,51,1,0,1,3, 
  17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,147,0,0,0,255,255,255,255,11,0, 
  0,0,3,0,0,0,115,168,130,130,105,40,3,128,98,168,3,128,91,232,67,127,36,96,4,129,109,232,2,129,102,104,3,128,47,32,4,128,116,104,2,128,117,40,2,128,123,240,1,128,3,18,11,0,0,0,1,3,17,1, 
  191,56,1,0,1,3,17,1,235,55,1,0,1,3,17,1,174,55,1,0,1,3,17,1,113,55,1,0,1,3,17,1,213,54,1,0,1,3,17,1,6,54,1,0,1,3,17,1,170,53,1,0,1,3,18,44,0,0,0,1, 
  3,17,1,152,20,1,0,1,3,18,98,0,0,0,1,21,2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3, 
  15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,111,176,0,128,3,17,1,201,53,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,111,176,0,128,3,17,1,232,53,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,18,41,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,54,48,1,128,51,112,65,128,97,240,0,128,3,17,1,121,54,1,0,1,3,17,1,91,54,1,0,1,3,17,1,61,54,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50, 
  176,0,128,3,18,33,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,36,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108, 
  176,0,128,3,17,1,152,54,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,17,1,183,54,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0, 
  0,101,176,0,128,3,18,43,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56,16,1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,31,0,0,0,1,3,17,1,83,55,1, 
  0,1,3,17,1,53,55,1,0,1,3,17,1,23,55,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,3,18,39,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,50,176,0,128,3,18,34,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,37,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,97,176,0,128,3,17,1,144,55,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,112,176,0,128,3,18,54,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,205,55,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,114,176,0,128,3,18,29,0,0,0,1,2,21,4,53,0,0,0,255,255,255, 
  255,3,0,0,0,1,0,0,0,114,240,0,128,107,112,65,128,111,48,1,128,3,17,1,130,56,1,0,1,3,17,1,33,56,1,0,1,3,18,46,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,107,176,0,128,3,17,1,64,56,1,0,1,2,18,47,0,0,0,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,100,56,1,0,1,1,21,4,29,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,18,48,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,161,56,1,0,1,2,21,4,29,0,0,0,255, 
  255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,42,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,56,16,1,128,49,200,1,128,54,72,1,128,51,136,1,128,3,18,32, 
  0,0,0,1,3,17,1,61,57,1,0,1,3,17,1,31,57,1,0,1,3,17,1,1,57,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,54,176,0,128,3,18,40,0,0,0,1,2,21, 
  4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,50,176,0,128,3,18,35,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,52,176,0,128,3,18,38,0,0,0,1,2,21, 
  2,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,15,1,166,22,1,0,17,1,128,57,1,0,1,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,17, 
  1,128,57,1,0,1,1,18,87,0,0,0,1,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,124,23,1,0,1,21,2,70,0,0,0,255,255,255,255,4,0,0, 
  0,2,0,0,0,4,128,1,128,5,72,1,128,6,16,1,128,3,240,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,3,17,1,124,23,1,0,1,2,21,4, 
  160,0,0,0,255,255,255,255,13,0,0,0,3,0,0,0,43,192,132,129,41,16,3,128,58,200,3,128,35,8,68,127,44,48,2,129,45,136,3,129,59,216,2,128,95,136,3,128,60,72,131,128,93,136,196,128,116,72,68,128, 
  124,104,2,128,125,160,2,128,3,18,10,0,0,0,1,3,18,84,0,0,0,1,3,18,82,0,0,0,1,3,18,90,0,0,0,1,3,18,85,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,124,23,1,0,1,3, 
  17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,6,59,1,0,1,3,18,45,0,0,0,1,3,17,1,227,58,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128, 
  3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,18,50,0,0,0,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18, 
  25,0,0,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,104,240,0,128,45,48,65,128,95,48,1,128,3,17,1,118,59,1,0,1,3,15,1,204,24,1,0,17,1,123,24, 
  1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23,1,0,1,1,18,58, 
  0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,101,240,0,128,3,17,1,216,59,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,110, 
  240,0,128,45,40,65,128,95,40,1,128,3,18,13,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1, 
  1,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,23,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,24,0,0,0,1, 
  2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,21,0,0,0,1,2,21,4,124,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,115,48,2,128,105,176,2,128,98,48,3, 
  128,91,112,67,127,36,168,195,128,109,112,194,128,102,240,2,128,116,240,1,128,117,176,1,128,3,17,1,191,56,1,0,1,3,17,1,235,55,1,0,1,3,17,1,174,55,1,0,1,3,17,1,113,55,1,0,1,3,17,1, 
  213,54,1,0,1,3,17,1,6,54,1,0,1,3,17,1,170,53,1,0,1,3,18,44,0,0,0,1,3,18,98,0,0,0,1,21,2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1, 
  128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127, 
  3,17,1,124,23,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1, 
  2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,36,16,65,128,116,208,0,128,3,17,1,231,61,1,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,111,208,0,128,107,16,193,127,3,17,1,33,56,1,0,1,3,18,46, 
  0,0,0,1,2,21,4,135,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,115,136,2,128,105,8,3,128,98,136,3,128,91,200,67,127,36,0,196,128,109,200,194,128,102,72,3,128,116,72,2,128,117,8,66,128,125, 
  208,1,128,3,18,9,0,0,0,1,3,17,1,191,56,1,0,1,3,17,1,211,62,1,0,1,3,17,1,174,55,1,0,1,3,17,1,113,55,1,0,1,3,17,1,213,54,1,0,1,3,17,1,6,54,1,0,1,3,17, 
  1,170,53,1,0,1,3,18,44,0,0,0,1,3,18,98,0,0,0,1,21,2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2, 
  0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,144,65,128,111,80,1,128,114,16,1,128,95,200,65,127,3,17,1,130,56,1,0, 
  1,3,17,1,33,56,1,0,1,3,18,46,0,0,0,1,3,18,57,0,0,0,1,2,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,124,23,1, 
  0,1,3,17,1,119,63,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1, 
  0,1,2,18,58,0,0,0,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,107,80,129,128,45,136,1,128,111,16,1,128,95,136,65,127,3,17,1,242,63,1,0,1,3,18,46,0,0,0,1,3,15,1, 
  204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1, 
  224,23,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,107,240,0,128,3,17,1,64,56,1,0,1,3,15,1,204,24,1,0,17,1,49,24, 
  1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1,21,4,135,0,0,0,255,255,255,255,10,0,0,0,3,0, 
  0,0,115,136,66,130,105,8,3,128,98,136,3,128,91,200,67,127,36,0,196,128,109,200,194,128,102,72,3,128,116,72,2,128,117,8,2,128,123,208,1,128,3,18,11,0,0,0,1,3,17,1,191,56,1,0,1,3,17,1, 
  235,55,1,0,1,3,17,1,174,55,1,0,1,3,17,1,113,55,1,0,1,3,17,1,213,54,1,0,1,3,17,1,6,54,1,0,1,3,17,1,170,53,1,0,1,3,18,44,0,0,0,1,3,18,98,0,0,0,1,21, 
  2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4, 
  30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,93,65,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0, 
  1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,18,57,0,0,0,1,2,21,2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129, 
  128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,135,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,109,144,66,130, 
  105,208,2,128,98,80,3,128,91,200,3,129,36,0,4,129,93,144,195,126,102,16,3,128,115,80,2,128,116,16,2,128,117,208,1,128,3,17,1,191,56,1,0,1,3,17,1,235,55,1,0,1,3,17,1,174,55,1,0,1, 
  3,17,1,113,55,1,0,1,3,17,1,213,54,1,0,1,3,17,1,6,54,1,0,1,3,17,1,170,53,1,0,1,3,18,45,0,0,0,1,3,18,44,0,0,0,1,3,18,98,0,0,0,1,21,2,58,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,183,0,0,0,255,255, 
  255,255,15,0,0,0,3,0,0,0,40,168,2,128,41,144,3,128,34,56,133,129,91,88,3,130,36,112,2,130,45,200,3,128,94,184,4,128,39,120,132,128,58,24,3,128,63,8,68,128,95,200,3,128,99,248,132,128,116,120, 
  133,128,123,64,4,128,124,224,2,128,3,18,98,0,0,0,1,3,18,53,0,0,0,1,3,18,84,0,0,0,1,3,17,1,16,51,1,0,1,3,18,44,0,0,0,1,3,18,85,0,0,0,1,3,17,1,124,23,1,0, 
  1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0, 
  0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,46,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,15,1,189,67,1,0,15,1,166,22,1,0,17,1,124,23,1,0,1,21,2,42,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,3,176,0,128,3,15,1,189, 
  67,1,0,15,1,166,22,1,0,17,1,124,23,1,0,1,2,18,118,0,0,0,1,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,94,40,1,128,63,104,65,128,123,240,0,128,3,18,11,0,0,0,1, 
  3,17,1,100,67,1,0,1,3,18,115,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,132,0, 
  0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,176,3,128,41,120,3,128,58,56,3,128,91,0,131,128,36,232,195,128,99,128,194,128,94,192,2,128,116,64,130,128,123,8,2,128,124,208,1,128,3,18,84,0,0,0, 
  1,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,17,1,16,51,1,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0, 
  1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,58,176,0,128,3,18,114,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,107,176,0,128,3,17,1,10,69,1,0,1,2,21,4,30,0,0,0,255,255,255,255, 
  1,0,0,0,0,0,0,0,58,176,0,128,3,17,1,58,47,1,0,1,2,21,4,121,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,40,88,3,128,41,32,3,128,58,224,2,128,91,168,130,128,36,144,195,128,99, 
  40,2,128,94,104,2,128,116,232,65,128,124,176,1,128,3,18,84,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,17,1,16,51,1,0, 
  1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0, 
  0,1,2,21,2,58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,74,70,1,0,17,1,6,70,1,0, 
  1,2,21,2,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,15,1,166,22,1,0,17,1,43,70,1,0,1,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0, 
  128,3,17,1,43,70,1,0,1,1,18,129,0,0,0,1,21,4,132,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,176,3,128,41,120,3,128,58,56,3,128,91,200,130,128,36,232,3,129,99,72,2,128,94,136, 
  2,128,63,0,3,128,116,8,66,128,124,208,1,128,3,18,84,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3, 
  17,1,16,51,1,0,1,3,18,85,0,0,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0, 
  1,3,18,2,0,0,0,1,2,21,4,184,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,64,4,128,41,88,3,128,34,64,133,129,91,32,3,130,36,112,2,130,45,144,3,128,94,192,4,128,39,128,132,128,58, 
  224,2,128,63,208,67,128,95,144,3,128,99,0,133,128,116,128,133,128,123,8,4,128,124,168,2,128,3,18,98,0,0,0,1,3,18,84,0,0,0,1,3,17,1,16,51,1,0,1,3,18,44,0,0,0,1,3,18,85,0, 
  0,0,1,3,17,1,124,23,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,234,71,1,0,1,3,17,1,204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0,1,3,17,1, 
  252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1, 
  124,23,1,0,1,2,18,53,0,0,0,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,8,1,128,43,208,0,128,3,18,128,0,0,0,1,3,18,127,0,0,0,1,1,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,40,176,0,128,3,17,1,95,72,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0, 
  0,1,2,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,8,1,128,43,208,0,128,3,18,128,0,0,0,1,3,18,127,0,0,0,1,2,21,4,65,0,0,0,255,255,255,255,4,0,0,0,2,0, 
  0,0,99,16,1,128,41,80,1,128,34,200,1,128,39,136,65,127,3,17,1,205,68,1,0,1,3,18,51,0,0,0,1,3,17,1,204,47,1,0,1,3,17,1,252,42,1,0,1,21,2,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,47,208,0,128,41,16,193,127,3,17,1,152, 
  20,1,0,1,3,18,85,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,90,0,0,0,255,255, 
  255,255,6,0,0,0,2,0,0,0,104,80,66,129,97,144,194,128,110,208,1,128,115,144,1,128,105,16,2,128,116,80,1,128,3,17,1,14,75,1,0,1,3,17,1,198,74,1,0,1,3,17,1,126,74,1,0,1,3,17, 
  1,96,74,1,0,1,3,17,1,4,74,1,0,1,3,17,1,199,73,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0, 
  0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,110,176,0,128,3,17,1,230,73,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,121,176,0,128,3,18,112,0, 
  0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,35,74,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1, 
  66,74,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,18,113,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,18, 
  106,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,108,16,1,128,117,208,0,128,3,17,1,168,74,1,0,1,3,18,107,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,109,176,0,128,3,18,110,0,0,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,16,1,128,121,208,0,128,3,17,1,240,74,1,0,1,3,18,108,0,0,0,1,2, 
  21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,109,176,0,128,3,18,111,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,97,176,0,128,3,17,1,45,75,1,0,1, 
  2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,98,176,0,128,3,18,109,0,0,0,1,2,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,40,240,0,128,45,40,65,128,95,40,1, 
  128,3,18,53,0,0,0,1,3,17,1,124,23,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1, 
  3,17,1,124,23,1,0,1,2,21,4,105,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,40,144,1,128,47,8,67,129,34,136,2,128,99,72,2,128,116,200,2,128,45,200,1,128,95,200,1,128,39,8,130,126,3, 
  18,53,0,0,0,1,3,17,1,124,23,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0, 
  255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,110,0,0,0,255,255,255,255,8,0,0, 
  0,3,0,0,0,40,248,2,128,99,8,66,129,58,184,2,128,91,72,130,127,36,56,67,128,116,200,1,128,123,144,1,128,63,128,2,128,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1, 
  3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,226,76,1,0,1,3,17,1,234,71,1,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5, 
  8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,58,176,0,128,3,18,97,0,0,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0, 
  0,0,3,0,0,0,40,176,1,128,47,96,195,129,34,224,2,128,99,160,2,128,116,32,3,128,45,32,66,128,93,232,1,128,39,96,130,126,95,32,2,128,3,18,53,0,0,0,1,3,18,120,0,0,0,1,3,17,1,124, 
  23,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0, 
  0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,47,208,0,128,33, 
  16,193,127,3,17,1,152,20,1,0,1,3,18,119,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21, 
  4,138,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,240,1,128,63,160,2,130,34,208,3,128,99,144,195,129,116,16,4,128,45,96,194,128,94,80,3,128,39,16,131,126,93,40,2,128,95,96,2,128,123,216,2, 
  128,3,18,53,0,0,0,1,3,18,120,0,0,0,1,3,17,1,124,23,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0, 
  1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0, 
  1,3,17,1,124,23,1,0,1,2,21,4,98,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,36,216,66,128,40,160,2,129,94,40,2,128,91,104,66,128,99,232,129,128,116,168,1,128,123,112,1,128,3,18,11,0, 
  0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,36,128,66,128,40,72,2,129,94,208,1,128,91, 
  16,66,128,99,144,1,128,116,80,1,128,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,98,0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,36,216,66,128,40, 
  160,66,129,94,240,1,128,63,104,66,128,91,48,66,128,99,176,1,128,116,112,1,128,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0, 
  1,3,18,53,0,0,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,139,0, 
  0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,216,2,128,63,104,2,130,34,216,3,128,99,152,195,129,116,24,4,128,45,40,194,128,94,88,3,128,39,24,131,126,93,240,1,128,95,40,2,128,123,160,2,128,3,18, 
  120,0,0,0,1,3,17,1,124,23,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,234,71,1,0,1,3,17,1,204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0,1,3, 
  17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3, 
  17,1,124,23,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,93,208,0,128,47,8,193,127,3,18,120,0,0,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,96,2,128,41,232,1,128,58,224,2,128, 
  35,32,67,127,60,32,2,129,45,160,2,128,95,160,2,128,47,96,195,127,124,176,1,128,3,18,84,0,0,0,1,3,18,85,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1, 
  0,1,3,17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0, 
  0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,62,176,0,128,3,18,25,0,0,0,1,2,21,4,197,0,0,0,255,255,255,255,16, 
  0,0,0,4,0,0,0,91,56,3,128,95,40,4,128,34,104,5,128,35,168,132,128,36,144,130,128,99,40,5,128,116,168,5,128,39,232,4,128,40,200,2,128,41,112,3,128,58,104,4,128,43,232,67,125,60,168,131,128,45, 
  40,4,128,124,0,3,128,47,232,133,124,3,18,98,0,0,0,1,3,18,53,0,0,0,1,3,18,84,0,0,0,1,3,18,44,0,0,0,1,3,18,85,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1, 
  0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,3,17, 
  1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21, 
  4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,16,1,128,97,208,0,128,3,17,1,47,51,1,0,1,3,18,24,0,0,0,1,2,21,4,134,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40, 
  184,3,128,63,192,2,128,58,56,3,128,91,136,194,128,36,248,3,129,61,248,2,128,99,72,194,128,47,120,131,126,116,8,2,128,123,208,1,128,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1, 
  0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,37,25,1,0,1,3,17,1,226,76,1,0,1,3,17,1,152,20,1,0,1,3,17,1,234,71,1,0,1,3,18,98,0,0,0,1,21,2,40,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,116,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,96,2,128,41,232, 
  1,128,58,224,2,128,35,32,67,127,60,32,2,129,45,160,2,128,95,160,2,128,47,96,195,127,124,176,1,128,3,18,84,0,0,0,1,3,18,85,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1, 
  3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40, 
  1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,219,0,0,0,255,255,255,255,18,0,0,0,4,0,0,0,91,120,3,132,95,104,4,128,34,88,6,128,35,232,132,128,36, 
  208,130,128,99,24,6,128,116,152,6,128,39,152,5,128,40,8,3,128,41,176,3,128,58,168,4,128,43,40,68,125,60,232,67,129,45,104,4,128,94,216,5,128,63,40,133,124,123,96,5,128,124,64,3,128,3,18,98,0,0, 
  0,1,3,18,53,0,0,0,1,3,18,84,0,0,0,1,3,18,44,0,0,0,1,3,18,85,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1, 
  0,1,3,17,1,45,60,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104, 
  46,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,220, 
  0,0,0,255,255,255,255,18,0,0,0,4,0,0,0,91,64,3,132,95,48,4,128,34,96,6,128,35,176,132,128,36,208,130,128,99,32,6,128,116,160,6,128,39,160,5,128,40,96,5,128,41,120,3,128,58,112,4,128,43, 
  240,67,125,60,176,67,129,45,48,4,128,94,224,5,128,63,240,132,124,123,40,5,128,124,8,3,128,3,18,98,0,0,0,1,3,18,84,0,0,0,1,3,18,44,0,0,0,1,3,18,85,0,0,0,1,3,17,1,105,60, 
  1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,234,71,1,0,1,3,17,1, 
  204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3, 
  96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,36,16,1,128,47,208,0,128,3,17,1,152, 
  20,1,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87,0,0,0,255,255, 
  255,255,6,0,0,0,2,0,0,0,40,128,66,129,91,16,194,128,94,208,1,128,63,72,130,127,99,144,1,128,116,80,1,128,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1,0,1,3,18, 
  44,0,0,0,1,3,18,115,0,0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2, 
  21,4,76,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,40,40,2,129,99,112,1,128,94,176,1,128,91,240,129,127,116,48,1,128,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,100,67,1, 
  0,1,3,18,44,0,0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,87, 
  0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,40,128,2,129,99,200,1,129,94,8,2,128,91,72,130,127,116,136,1,128,123,80,1,128,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1, 
  0,1,3,17,1,100,67,1,0,1,3,18,44,0,0,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2, 
  0,0,0,1,2,21,4,132,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,40,168,3,128,41,112,3,128,58,48,3,128,91,192,130,128,36,232,131,128,99,128,194,128,116,64,194,128,63,248,2,128,123,8,2,128,124, 
  208,1,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,18,44,0,0,0,1,3,18,115,0,0,0,1,3,17,1,58,89,1,0,1,3,18,85,0,0, 
  0,1,3,17,1,234,71,1,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4, 
  41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,58,16,1,128,97,208,0,128,3,17,1,47,51,1,0,1,3,18,97,0,0,0,1,2,21,4,78,0,0,0,255,255,255,255,5,0,0,0,2,0,0,0,116,48, 
  1,128,47,176,193,128,34,48,2,128,39,240,129,127,99,112,1,128,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,152,20,1,0,1,3,17,1,204,47,1,0,1,3,17,1,252,42,1,0,1,21,2, 
  40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,63,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,192,1, 
  128,123,16,1,128,58,72,1,128,47,128,129,127,3,18,11,0,0,0,1,3,18,56,0,0,0,1,3,17,1,152,20,1,0,1,3,18,53,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,152,20,1,0,1,21,2,58,0,0,0,255, 
  255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,69,0,0,0,255,255, 
  255,255,5,0,0,0,2,0,0,0,40,48,1,128,45,168,1,128,95,168,129,128,47,232,193,127,115,104,1,128,3,18,53,0,0,0,1,3,17,1,40,91,1,0,1,3,17,1,124,23,1,0,1,3,17,1,152,20,1,0, 
  1,21,2,70,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,128,1,128,5,72,1,128,6,16,1,128,3,240,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57, 
  1,0,1,3,17,1,124,23,1,0,1,2,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,121,240,0,128,3,17,1,152,91,1,0,1,3,15,1,204,24, 
  1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23, 
  1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,109,240,0,128,3,17,1,250,91,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0, 
  1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0, 
  0,1,0,0,0,98,240,0,128,45,48,65,128,95,48,1,128,3,17,1,80,92,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3, 
  17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,48,129,128,45,48,193,127,111,240,0,128,3,17,1,166,92,1,0,1,3,17,1,49,24,1,0,1,21, 
  2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,108,240,0, 
  128,45,48,65,128,95,48,1,128,3,17,1,252,92,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1, 
  18,58,0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,40,129,128,45,40,193,127,115,240,0,128,3,18,65,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255, 
  2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,164,93,1,0,1,3, 
  17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,111,176,0,128,3,18,59,0,0,0,1,2,21,4,57,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,40,16,1,128,45,72,1,128,95,72,1,128,47,136,193,127,3,18,53,0,0,0,1,3, 
  17,1,124,23,1,0,1,3,17,1,152,20,1,0,1,21,2,70,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,4,128,1,128,5,72,1,128,6,16,1,128,3,240,1,128,3,18,1,0,0,0,1,3,18,2,0, 
  0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,3,17,1,124,23,1,0,1,2,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,40,2,128,95,104,2,128,58,168,2,128,35,232,66,127, 
  60,232,129,128,45,104,194,128,116,40,3,128,47,104,131,126,125,176,1,128,3,18,82,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3, 
  17,1,45,60,1,0,1,3,17,1,6,59,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1, 
  3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,119,208,0,128,47,16,193,127,3,17,1,81,95,1,0,1,3,17,1,152,20,1,0,1,21,2, 
  58,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,4,96,129,128,5,40,1,128,6,240,0,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,15,1,159,57,1,0,17,1,91,57,1,0,1,2,21,4,30, 
  0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,105,176,0,128,3,17,1,112,95,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,116,176,0,128,3,17,1,143,95,1,0,1,2,21, 
  4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,104,176,0,128,3,18,61,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,114,208,0,128,47,16,1,128,3,17,1,0,96, 
  1,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255, 
  255,255,1,0,0,0,0,0,0,0,117,176,0,128,3,17,1,31,96,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,108,176,0,128,3,17,1,62,96,1,0,1,2,21,4,29,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,18,60,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,116,208,0,128,47,16,1,128,3,17,1,175,96,1,0,1,3,17, 
  1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,104,176,0,128,3,17,1,206,96,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128,3,17,1,237,96,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,110,176,0,128,3,18,13,0,0,0,1,2,21,4,90,0,0,0,255,255,255,255,6,0,0,0,2,0,0,0,112,80,1,128,97,80,2,129,102,16,2,128,47,144,66,128,103,208,1,128,109,144,1, 
  128,3,17,1,142,97,1,0,1,3,17,1,90,27,1,0,1,3,17,1,254,26,1,0,1,3,17,1,162,26,1,0,1,3,17,1,8,26,1,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2, 
  0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,117,208,0,128,97,16,193,127,3,17,1,80, 
  29,1,0,1,3,17,1,24,28,1,0,1,2,21,4,117,0,0,0,255,255,255,255,9,0,0,0,3,0,0,0,43,40,2,128,95,104,2,128,58,168,2,128,35,232,66,127,60,232,129,128,45,104,194,128,116,40,3,128,47, 
  104,131,126,125,176,1,128,3,18,82,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,6,59,1, 
  0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1, 
  0,1,2,21,4,114,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,112,16,2,128,97,16,3,128,114,208,1,128,115,144,1,128,103,144,2,128,109,80,2,128,102,208,2,128,47,80,67,127,3,17,1,60,99,1,0, 
  1,3,17,1,254,98,1,0,1,3,17,1,142,97,1,0,1,3,17,1,90,27,1,0,1,3,17,1,254,26,1,0,1,3,17,1,162,26,1,0,1,3,17,1,8,26,1,0,1,3,17,1,152,20,1,0,1,21,2,40, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176,0,128, 
  3,17,1,29,99,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,100,176,0,128,3,17,1,215,29,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,101,176, 
  0,128,3,17,1,240,30,1,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,114,48,1,128,47,112,65,128,115,240,0,128,3,17,1,60,99,1,0,1,3,17,1,254,98,1,0,1,3,17,1, 
  152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0, 
  1,0,0,0,103,208,0,128,47,16,193,127,3,17,1,254,26,1,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0, 
  1,3,18,2,0,0,0,1,2,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,112,240,0,128,47,112,65,128,103,48,1,128,3,17,1,108,100,1,0,1,3,17,1,254,26,1,0,1,3,17,1,152,20, 
  1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,117,176,0,128,3,17,1,80,29,1,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,112,208,0,128,47,16,1,128,3,17,1,108,100,1,0,1,3,17,1,152,20,1,0,1,21,2,40, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,130,0,0,0,255,255,255,255,10,0,0,0,3,0,0,0,43,80,2,128, 
  73,208,1,128,58,80,3,128,35,144,67,127,60,16,2,128,45,144,194,128,78,16,3,128,47,208,131,128,69,208,2,128,95,144,2,128,3,17,1,143,104,1,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1, 
  3,17,1,124,23,1,0,1,3,17,1,188,102,1,0,1,3,17,1,149,101,1,0,1,3,17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0, 
  0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0, 
  1,0,0,0,65,240,128,128,45,48,193,127,95,48,1,128,3,17,1,5,102,1,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128, 
  3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,77,240,128,128, 
  45,48,193,127,95,48,1,128,3,17,1,103,102,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204, 
  24,1,0,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,40,193,127,95,40,1,128,3,18,81,0,0,0,1,3,17,1,49,24,1,0, 
  1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,88, 
  240,0,128,45,48,65,128,95,48,1,128,3,17,1,44,103,1,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3, 
  15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95, 
  48,1,128,3,17,1,142,103,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1, 
  49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,228,103,1,0,1,3,17,1,49,24,1,0,1,21,2,34, 
  0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45, 
  48,65,128,95,48,1,128,3,17,1,58,104,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58, 
  0,0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,40,65,128,95,40,1,128,3,18,79,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,64,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,77,80,1,128,45,144,193,127,95,144,1,128,71,16, 
  193,127,3,17,1,110,106,1,0,1,3,17,1,11,105,1,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15, 
  1,204,24,1,0,17,1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,80,240,0,128,45,48,65,128,95,48, 
  1,128,3,17,1,109,105,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49, 
  24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,195,105,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0, 
  0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48, 
  65,128,95,48,1,128,3,17,1,25,106,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0, 
  0,0,21,4,45,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,84,240,0,128,45,40,65,128,95,40,1,128,3,18,80,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,78,240,0,128,45,48,65,128,95,48,1,128,3,17,1, 
  208,106,1,0,1,3,15,1,204,24,1,0,17,1,49,24,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,204,24,1,0,17,1,49,24,1,0,1,1, 
  18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,79,240,128,128,45,48,193,127,95,48,1,128,3,17,1,38,107,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255, 
  255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,46,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,82,240,0,128,45,48,65,128,95,48,1, 
  128,3,17,1,124,107,1,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,18,58,0,0,0,21,4,45, 
  0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,69,240,128,128,45,40,193,127,95,40,1,128,3,18,83,0,0,0,1,3,17,1,49,24,1,0,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  4,208,0,128,3,208,0,128,3,17,1,49,24,1,0,1,1,21,4,41,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,62,208,0,128,47,8,1,128,3,18,20,0,0,0,1,3,17,1,152,20,1,0,1,21,2, 
  40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,105,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,43,8,2, 
  128,95,72,2,128,58,136,2,128,35,200,66,127,60,200,129,128,45,72,2,128,124,144,1,128,47,8,131,126,3,18,84,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1, 
  3,17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0, 
  1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,186,0,0,0,255,255,255,255,15,0,0,0,3,0,0,0,40,168,2,128,43,144,67,130,34,16,133,129,35,80,132,127,36,112,66,129,45,208,3,128,47, 
  144,69,129,39,144,196,127,58,16,4,128,60,80,3,129,91,24,131,128,95,208,3,128,99,208,4,128,116,80,69,128,124,224,2,128,3,18,98,0,0,0,1,3,18,53,0,0,0,1,3,18,84,0,0,0,1,3,18,44,0, 
  0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3,17,1,204,47,1,0,1,3,17,1,93,47,1,0,1,3, 
  17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1, 
  3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,145,0,0,0,255,255,255,255,11,0,0,0,3,0,0,0,40,16,4,128,63,24,3,128,58,144,3,128,91,224,194,128,36,80,4,129,61,80,3,128,99,160, 
  194,128,47,208,131,126,116,96,130,128,123,40,2,128,124,240,1,128,3,18,84,0,0,0,1,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,18,44,0,0,0,1,3,18,115,0,0, 
  0,1,3,17,1,37,25,1,0,1,3,17,1,58,89,1,0,1,3,17,1,152,20,1,0,1,3,17,1,234,71,1,0,1,3,18,98,0,0,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6, 
  208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,105,0,0,0,255,255,255,255,8,0,0,0,3,0,0,0,43,8,2,128,95,72,2,128,58,136,2,128,35,200,66,127,60,200,129,128, 
  45,72,2,128,124,144,1,128,47,8,131,126,3,18,84,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3, 
  17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2, 
  21,4,208,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,91,88,67,130,95,16,4,128,34,0,6,128,35,144,132,128,36,176,130,128,99,192,5,128,116,64,6,128,39,64,5,128,40,232,2,128,123,8,5,128,58,80, 
  4,128,43,208,67,125,60,144,3,129,45,16,4,128,94,128,5,128,63,208,132,124,124,32,3,128,3,18,98,0,0,0,1,3,18,53,0,0,0,1,3,18,84,0,0,0,1,3,18,44,0,0,0,1,3,17,1,105,60,1, 
  0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0,0,1,3,17,1,204,47,1,0,1,3,17,1,100, 
  67,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18, 
  1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,209,0,0,0,255,255,255,255,17,0,0,0,4,0,0,0,91,32,67,130,95,216,3,128,34,8,6,128,35,88,132,128,36,176,130,128,99, 
  200,5,128,116,72,6,128,39,72,5,128,40,8,5,128,123,208,4,128,58,24,4,128,43,152,67,125,60,88,3,129,45,216,3,128,94,136,5,128,63,152,132,124,124,232,2,128,3,18,98,0,0,0,1,3,18,84,0,0,0, 
  1,3,18,44,0,0,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,35,83,1,0,1,3,17,1,45,60,1,0,1,3,18,115,0,0,0,1,3,18,11,0,0, 
  0,1,3,17,1,234,71,1,0,1,3,17,1,204,47,1,0,1,3,17,1,100,67,1,0,1,3,17,1,93,47,1,0,1,3,17,1,252,42,1,0,1,3,17,1,104,46,1,0,1,21,2,52,0,0,0,255,255,255,255, 
  3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,99,0,0,0,255,255,255,255,7,0,0,0,2,0,0, 
  0,40,216,66,129,91,40,194,128,58,152,2,128,63,96,130,127,99,232,129,128,116,168,1,128,123,112,1,128,3,18,11,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,18,44,0,0,0,1,3, 
  18,115,0,0,0,1,3,17,1,226,76,1,0,1,3,17,1,234,71,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0, 
  0,1,2,21,4,154,0,0,0,255,255,255,255,12,0,0,0,3,0,0,0,43,208,2,128,65,16,2,130,58,208,3,128,35,16,68,127,60,144,2,128,45,16,195,128,78,144,3,128,47,144,196,128,69,80,3,128,73,80,130, 
  128,95,16,3,128,97,80,4,128,3,17,1,221,114,1,0,1,3,17,1,143,104,1,0,1,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3,17,1,124,23,1,0,1,3,17,1,188,102,1,0,1,3,17,1, 
  149,101,1,0,1,3,17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,110,114,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3, 
  96,65,128,5,40,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,18,58,0,0,0,21,4,51,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,95,40,129,128,45,40,193, 
  127,115,240,0,128,3,18,78,0,0,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17, 
  1,123,24,1,0,1,3,15,1,204,24,1,0,17,1,224,23,1,0,1,1,18,58,0,0,0,21,4,51,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,83,240,128,128,45,40,193,127,95,40,1,128,3,18,77,0, 
  0,0,1,3,15,1,204,24,1,0,17,1,123,24,1,0,1,21,2,54,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,204,24,1,0,17,1,123,24,1,0,1,3,15,1, 
  204,24,1,0,17,1,224,23,1,0,1,1,21,4,54,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,65,48,129,128,47,112,193,127,97,240,0,128,3,17,1,201,115,1,0,1,3,17,1,171,115,1,0,1,3,17, 
  1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,83,176,0,128,3,18,77,0,0,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,115,176,0,128,3,18,78,0,0,0,1,2,21,4,89,0,0,0,255,255,255,255,6,0,0, 
  0,2,0,0,0,116,136,1,128,125,80,1,128,34,136,2,128,39,72,66,128,47,8,66,128,99,200,1,128,3,18,82,0,0,0,1,3,17,1,235,68,1,0,1,3,17,1,205,68,1,0,1,3,17,1,152,20,1,0,1, 
  3,17,1,204,47,1,0,1,3,17,1,252,42,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,94, 
  0,0,0,255,255,255,255,7,0,0,0,2,0,0,0,60,112,1,128,45,240,1,128,58,48,2,128,35,112,66,128,43,176,65,128,47,176,66,128,95,240,1,128,3,17,1,105,60,1,0,1,3,17,1,11,82,1,0,1,3, 
  17,1,124,23,1,0,1,3,17,1,75,60,1,0,1,3,17,1,45,60,1,0,1,3,17,1,152,20,1,0,1,21,2,52,0,0,0,255,255,255,255,3,0,0,0,1,0,0,0,6,240,0,128,3,96,65,128,5,40,1, 
  128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,17,1,124,23,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,100,117,1,0,1,21,2,73,0,0,0, 
  255,255,255,255,5,0,0,0,2,0,0,0,4,160,1,128,5,104,1,128,2,16,130,128,3,216,1,128,6,48,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,3,18,17,0,0,0,1,3,18,16,0,0,0,1, 
  3,18,18,0,0,0,1,2,18,18,0,0,0,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,42,16,1,128,47,208,0,128,3,17,1,198,22,1,0,1,3,17,1,195,20,1,0,1,1,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,47,176,0,128,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3, 
  18,2,0,0,0,1,2,21,4,66,0,0,0,255,255,255,255,4,0,0,0,2,0,0,0,73,80,1,128,69,144,193,127,78,16,1,128,47,208,1,128,3,17,1,1,120,1,0,1,3,17,1,224,118,1,0,1,3,17,1, 
  70,118,1,0,1,3,17,1,152,20,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,6,208,0,128,5,8,1,128,3,18,1,0,0,0,1,3,18,2,0,0,0,1,2,21,4,30,0,0,0, 
  255,255,255,255,1,0,0,0,0,0,0,0,88,176,0,128,3,17,1,101,118,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,80,176,0,128,3,17,1,132,118,1,0,1,2,21,4,30,0, 
  0,0,255,255,255,255,1,0,0,0,0,0,0,0,79,176,0,128,3,17,1,163,118,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,82,176,0,128,3,17,1,194,118,1,0,1,2,21,4, 
  29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,84,176,0,128,3,18,79,0,0,0,1,2,21,4,42,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,77,208,0,128,71,16,193,127,3,17,1,134,119,1, 
  0,1,3,17,1,11,119,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,78,176,0,128,3,17,1,42,119,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0, 
  79,176,0,128,3,17,1,73,119,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,82,176,0,128,3,17,1,104,119,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0, 
  0,0,69,176,0,128,3,18,83,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,80,176,0,128,3,17,1,165,119,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,79,176,0,128,3,17,1,196,119,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,82,176,0,128,3,17,1,227,119,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0, 
  0,0,0,0,0,84,176,0,128,3,18,80,0,0,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,65,176,0,128,3,17,1,32,120,1,0,1,2,21,4,30,0,0,0,255,255,255,255,1,0, 
  0,0,0,0,0,0,77,176,0,128,3,17,1,63,120,1,0,1,2,21,4,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,69,176,0,128,3,18,81,0,0,0,1,2,21,4,36,0,0,0,255,255,255,255,1, 
  0,0,0,0,0,0,0,95,176,0,128,3,15,1,246,120,1,0,17,1,178,120,1,0,1,21,2,48,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,64,1,128,3,15,1,132,123,1,0,17,1, 
  64,123,1,0,1,3,17,1,48,122,1,0,1,2,21,4,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,95,176,0,128,3,15,1,166,22,1,0,17,1,215,120,1,0,1,1,21,4,30,0,0,0,255,255,255, 
  255,1,0,0,0,0,0,0,0,95,176,0,128,3,17,1,215,120,1,0,1,1,21,2,34,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,17,1,25,121,1,0,1,2,18,27,0, 
  0,0,21,4,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,15,1,124,121,1,0,15,1,130,121,1,0,17,1,123,121,1,0,1,21,2,46,0,0,0,255,255,255,255,2,0, 
  0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,124,121,1,0,15,1,130,121,1,0,17,1,123,121,1,0,1,1,1,18,27,0,0,0,1,21,4,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,3,15,1,166,22,1,0,15,1,223,121,1,0,17,1,123,121,1,0,1,21,2,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,166,22,1, 
  0,15,1,223,121,1,0,17,1,123,121,1,0,1,1,21,4,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,15,1,223,121,1,0,17,1,123,121,1,0,1,21,2,40,0,0, 
  0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,223,121,1,0,17,1,123,121,1,0,1,1,18,27,0,0,0,21,4,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,15,1,124,121,1,0,15,1,146,122,1,0,17,1,123,121,1,0,1,21,2,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,124,121,1,0, 
  15,1,146,122,1,0,17,1,123,121,1,0,1,1,21,4,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,15,1,166,22,1,0,15,1,239,122,1,0,17,1,123,121,1,0,1, 
  21,2,46,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,166,22,1,0,15,1,239,122,1,0,17,1,123,121,1,0,1,1,21,4,40,0,0,0,255,255,255,255,2,0,0, 
  0,1,0,0,0,95,208,0,128,45,208,192,127,3,15,1,239,122,1,0,17,1,123,121,1,0,1,21,2,40,0,0,0,255,255,255,255,2,0,0,0,1,0,0,0,4,208,0,128,3,208,0,128,3,15,1,239,122,1,0, 
  17,1,123,121,1,0,1,1,21,2,36,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,4,176,0,128,3,15,1,166,22,1,0,17,1,101,123,1,0,1,1,21,2,30,0,0,0,255,255,255,255,1,0,0,0,0, 
  0,0,0,4,176,0,128,3,17,1,101,123,1,0,1,1,18,28,0,0,0,1,21,5,29,0,0,0,255,255,255,255,1,0,0,0,0,0,0,0,1,176,0,128,3,18,8,0,0,0,1,2, 
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
    parser.init_parser(56546);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0.into_strings();
    Ok(obj_0_0)
  }
  
  pub fn grammar_from<'a> (mut reader: UTF8StringReader)-> Result<Box<Grammar>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(57426);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_Grammar();
    Ok(obj_0_0)
  }
  
  pub fn type_eval_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(70010);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_expression_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(70160);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    Ok(obj_0_0)
  }
  
  pub fn ast_struct_from<'a> (mut reader: UTF8StringReader)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(70638);
    let AstSlot (ref_0, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = ref_0;
    let obj_0_0 = obj_0_0.to_AST_Struct();
    Ok(obj_0_0)
  }
}