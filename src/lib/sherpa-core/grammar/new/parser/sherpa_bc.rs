
/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta1
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
/// of this software and associated documentation files (the "Software"), to deal
/// in the Software without restriction, including without limitation the rights
/// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
/// copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
/// 
/// The above copyright notice and this permission notice shall be included in all
/// copies or substantial portions of the Software.
/// 
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
/// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
/// SOFTWARE.
///

use std::hash::Hash;
use sherpa_runtime::types::{ast::*, *};
use sherpa_runtime::llvm_parser::*;



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

pub trait ASTParse<T>{
  fn grammar_from(input:T) -> Result<Box<Grammar>, SherpaParseError>;
  fn ast_struct_from(input:T) -> Result<Box<AST_Struct>, SherpaParseError>;
  fn ast_expression_from(input:T) -> Result<ASTNode, SherpaParseError>;
  fn ir_from(input:T) -> Result<Box<State>, SherpaParseError>;
  fn type_eval_from(input:T) -> Result<ASTNode, SherpaParseError>;
  fn escaped_from(input:T) -> Result<Vec<String>, SherpaParseError>;
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

#[derive(Debug, Clone)]
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
  AST_NamedReference(Box<AST_NamedReference>),
  Reduce(Box<Reduce>),
  PrattProduction(Box<PrattProduction>),
  EOFSymbol(Box<EOFSymbol>),
  PegProduction(Box<PegProduction>),
  AST_Add(Box<AST_Add>),
  Match(Box<Match>),
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
  AST_Token(Box<AST_Token>),
  Shift(Box<Shift>),
  Export(Box<Export>),
  Name(Box<Name>),
  AST_BOOL(Box<AST_BOOL>),
  Fail(Box<Fail>),
  AST_Statements(Box<AST_Statements>),
  AST_U8(Box<AST_U8>),
  Assert(Box<Assert>),
  Ignore(Box<Ignore>),
  AST_STRING(Box<AST_STRING>),
  Skip(Box<Skip>),
  AST_U32(Box<AST_U32>),
  State(Box<State>),
  AST_I64(Box<AST_I64>),
  DefaultMatch(Box<DefaultMatch>),
  AST_U16(Box<AST_U16>),
  NotEmptySet(Box<NotEmptySet>),
  Accept(Box<Accept>),
  ClassSymbol(Box<ClassSymbol>),
  Statement(Box<Statement>),
  AST_F64(Box<AST_F64>),
  TokenGroupProduction(Box<TokenGroupProduction>),
  AST_Map(Box<AST_Map>),
  Range(Box<Range>),
  AST_I16(Box<AST_I16>),
  Scan(Box<Scan>),
  Push(Box<Push>),
  AST_NUMBER(Box<AST_NUMBER>),
  AST_Struct(Box<AST_Struct>),
  Peek(Box<Peek>),
  TermMatch(Box<TermMatch>),
  DEFINED_TYPE_NUM(Box<DEFINED_TYPE_NUM>),
  SetVal(Box<SetVal>),
  Rule(Box<Rule>),
  AnnotatedSymbol(Box<AnnotatedSymbol>),
  AST_Property(Box<AST_Property>),
  Ascript(Box<Ascript>),
  AppendProduction(Box<AppendProduction>),
  Production_Terminal_Symbol(Box<Production_Terminal_Symbol>),
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
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
  AST_NamedReference,
  Reduce,
  PrattProduction,
  EOFSymbol,
  PegProduction,
  AST_Add,
  Match,
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
  AST_Token,
  Shift,
  Export,
  Name,
  AST_BOOL,
  Fail,
  AST_Statements,
  AST_U8,
  Assert,
  Ignore,
  AST_STRING,
  Skip,
  AST_U32,
  State,
  AST_I64,
  DefaultMatch,
  AST_U16,
  NotEmptySet,
  Accept,
  ClassSymbol,
  Statement,
  AST_F64,
  TokenGroupProduction,
  AST_Map,
  Range,
  AST_I16,
  Scan,
  Push,
  AST_NUMBER,
  AST_Struct,
  Peek,
  TermMatch,
  DEFINED_TYPE_NUM,
  SetVal,
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
      ASTNode::AST_NamedReference(node) => node.tok.clone(),
      ASTNode::PrattProduction(node) => node.tok.clone(),
      ASTNode::EOFSymbol(node) => node.tok.clone(),
      ASTNode::PegProduction(node) => node.tok.clone(),
      ASTNode::AST_Add(node) => node.tok.clone(),
      ASTNode::GroupProduction(node) => node.tok.clone(),
      ASTNode::List_Production(node) => node.tok.clone(),
      ASTNode::TerminalToken(node) => node.tok.clone(),
      ASTNode::AST_Vector(node) => node.tok.clone(),
      ASTNode::Production_Symbol(node) => node.tok.clone(),
      ASTNode::Grammar(node) => node.tok.clone(),
      ASTNode::AST_U64(node) => node.tok.clone(),
      ASTNode::Import(node) => node.tok.clone(),
      ASTNode::CFProduction(node) => node.tok.clone(),
      ASTNode::AST_IndexReference(node) => node.tok.clone(),
      ASTNode::Production_Import_Symbol(node) => node.tok.clone(),
      ASTNode::AST_I32(node) => node.tok.clone(),
      ASTNode::AST_I8(node) => node.tok.clone(),
      ASTNode::AST_F32(node) => node.tok.clone(),
      ASTNode::AST_BOOL(node) => node.tok.clone(),
      ASTNode::AST_Statements(node) => node.tok.clone(),
      ASTNode::AST_U8(node) => node.tok.clone(),
      ASTNode::AST_STRING(node) => node.tok.clone(),
      ASTNode::AST_U32(node) => node.tok.clone(),
      ASTNode::State(node) => node.tok.clone(),
      ASTNode::AST_I64(node) => node.tok.clone(),
      ASTNode::AST_U16(node) => node.tok.clone(),
      ASTNode::NotEmptySet(node) => node.tok.clone(),
      ASTNode::ClassSymbol(node) => node.tok.clone(),
      ASTNode::AST_F64(node) => node.tok.clone(),
      ASTNode::TokenGroupProduction(node) => node.tok.clone(),
      ASTNode::AST_Map(node) => node.tok.clone(),
      ASTNode::AST_I16(node) => node.tok.clone(),
      ASTNode::AST_Struct(node) => node.tok.clone(),
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
      ASTNode::AST_NamedReference(..) => ASTNodeType::AST_NamedReference,
      ASTNode::Reduce(..) => ASTNodeType::Reduce,
      ASTNode::PrattProduction(..) => ASTNodeType::PrattProduction,
      ASTNode::EOFSymbol(..) => ASTNodeType::EOFSymbol,
      ASTNode::PegProduction(..) => ASTNodeType::PegProduction,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::Match(..) => ASTNodeType::Match,
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
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Shift(..) => ASTNodeType::Shift,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::AST_BOOL(..) => ASTNodeType::AST_BOOL,
      ASTNode::Fail(..) => ASTNodeType::Fail,
      ASTNode::AST_Statements(..) => ASTNodeType::AST_Statements,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::Assert(..) => ASTNodeType::Assert,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_STRING(..) => ASTNodeType::AST_STRING,
      ASTNode::Skip(..) => ASTNodeType::Skip,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::State(..) => ASTNodeType::State,
      ASTNode::AST_I64(..) => ASTNodeType::AST_I64,
      ASTNode::DefaultMatch(..) => ASTNodeType::DefaultMatch,
      ASTNode::AST_U16(..) => ASTNodeType::AST_U16,
      ASTNode::NotEmptySet(..) => ASTNodeType::NotEmptySet,
      ASTNode::Accept(..) => ASTNodeType::Accept,
      ASTNode::ClassSymbol(..) => ASTNodeType::ClassSymbol,
      ASTNode::Statement(..) => ASTNodeType::Statement,
      ASTNode::AST_F64(..) => ASTNodeType::AST_F64,
      ASTNode::TokenGroupProduction(..) => ASTNodeType::TokenGroupProduction,
      ASTNode::AST_Map(..) => ASTNodeType::AST_Map,
      ASTNode::Range(..) => ASTNodeType::Range,
      ASTNode::AST_I16(..) => ASTNodeType::AST_I16,
      ASTNode::Scan(..) => ASTNodeType::Scan,
      ASTNode::Push(..) => ASTNodeType::Push,
      ASTNode::AST_NUMBER(..) => ASTNodeType::AST_NUMBER,
      ASTNode::AST_Struct(..) => ASTNodeType::AST_Struct,
      ASTNode::Peek(..) => ASTNodeType::Peek,
      ASTNode::TermMatch(..) => ASTNodeType::TermMatch,
      ASTNode::DEFINED_TYPE_NUM(..) => ASTNodeType::DEFINED_TYPE_NUM,
      ASTNode::SetVal(..) => ASTNodeType::SetVal,
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
      AST_NamedReference(node) => node.hash(hasher),
      Reduce(node) => node.hash(hasher),
      PrattProduction(node) => node.hash(hasher),
      EOFSymbol(node) => node.hash(hasher),
      PegProduction(node) => node.hash(hasher),
      AST_Add(node) => node.hash(hasher),
      Match(node) => node.hash(hasher),
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
      AST_Token(node) => node.hash(hasher),
      Shift(node) => node.hash(hasher),
      Export(node) => node.hash(hasher),
      Name(node) => node.hash(hasher),
      AST_BOOL(node) => node.hash(hasher),
      Fail(node) => node.hash(hasher),
      AST_Statements(node) => node.hash(hasher),
      AST_U8(node) => node.hash(hasher),
      Assert(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      AST_STRING(node) => node.hash(hasher),
      Skip(node) => node.hash(hasher),
      AST_U32(node) => node.hash(hasher),
      State(node) => node.hash(hasher),
      AST_I64(node) => node.hash(hasher),
      DefaultMatch(node) => node.hash(hasher),
      AST_U16(node) => node.hash(hasher),
      NotEmptySet(node) => node.hash(hasher),
      Accept(node) => node.hash(hasher),
      ClassSymbol(node) => node.hash(hasher),
      Statement(node) => node.hash(hasher),
      AST_F64(node) => node.hash(hasher),
      TokenGroupProduction(node) => node.hash(hasher),
      AST_Map(node) => node.hash(hasher),
      Range(node) => node.hash(hasher),
      AST_I16(node) => node.hash(hasher),
      Scan(node) => node.hash(hasher),
      Push(node) => node.hash(hasher),
      AST_NUMBER(node) => node.hash(hasher),
      AST_Struct(node) => node.hash(hasher),
      Peek(node) => node.hash(hasher),
      TermMatch(node) => node.hash(hasher),
      DEFINED_TYPE_NUM(node) => node.hash(hasher),
      SetVal(node) => node.hash(hasher),
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

#[derive(Debug, Clone)]
pub struct AST_NamedReference{
  pub value: String, 
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
  
  #[track_caller]
  pub fn to_AST_NamedReference (self)-> Box::<AST_NamedReference> {
    
    match self{
      Self::AST_NamedReference(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_NamedReference", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Reduce{
  pub len: u32, 
  pub prod_id: u32, 
  pub rule_id: u32, 
}

impl Reduce{
  
  pub fn new (len: u32, prod_id: u32, rule_id: u32)-> Self {
    
    Self{
      len,
      prod_id,
      rule_id,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Reduce
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Reduce (self)-> Box::<Reduce> {
    
    match self{
      Self::Reduce(val) => val,
      _ => panic!("Type {:?} cannot be converted to Reduce", self.get_type())
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
    self.len.hash(hasher);
    self.prod_id.hash(hasher);
    self.rule_id.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct PrattProduction{
  pub name_sym: Box<Production_Symbol>, 
  pub rules: Vec<Box<Rule>>, 
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
  
  #[track_caller]
  pub fn to_PrattProduction (self)-> Box::<PrattProduction> {
    
    match self{
      Self::PrattProduction(val) => val,
      _ => panic!("Type {:?} cannot be converted to PrattProduction", self.get_type())
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

#[derive(Debug, Clone)]
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
  
  #[track_caller]
  pub fn to_EOFSymbol (self)-> Box::<EOFSymbol> {
    
    match self{
      Self::EOFSymbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to EOFSymbol", self.get_type())
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

#[derive(Debug, Clone)]
pub struct PegProduction{
  pub name_sym: Box<Production_Symbol>, 
  pub rules: Vec<Box<Rule>>, 
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
  
  #[track_caller]
  pub fn to_PegProduction (self)-> Box::<PegProduction> {
    
    match self{
      Self::PegProduction(val) => val,
      _ => panic!("Type {:?} cannot be converted to PegProduction", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Add{
  pub left: ASTNode, 
  pub right: ASTNode, 
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
  
  #[track_caller]
  pub fn to_AST_Add (self)-> Box::<AST_Add> {
    
    match self{
      Self::AST_Add(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Add", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Match{
  pub matches: Vec<ASTNode>, 
  pub mode: String, 
}

impl Match{
  
  pub fn new (matches: Vec<ASTNode>, mode: String)-> Self {
    
    Self{
      matches,
      mode,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Match
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Match (self)-> Box::<Match> {
    
    match self{
      Self::Match(val) => val,
      _ => panic!("Type {:?} cannot be converted to Match", self.get_type())
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
    self.mode.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Precedence{
  pub val: u32, 
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
  
  #[track_caller]
  pub fn to_Precedence (self)-> Box::<Precedence> {
    
    match self{
      Self::Precedence(val) => val,
      _ => panic!("Type {:?} cannot be converted to Precedence", self.get_type())
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

#[derive(Debug, Clone)]
pub struct NonTermMatch{
  pub statement: Box<Statement>, 
  pub sym: ASTNode, 
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
  
  #[track_caller]
  pub fn to_NonTermMatch (self)-> Box::<NonTermMatch> {
    
    match self{
      Self::NonTermMatch(val) => val,
      _ => panic!("Type {:?} cannot be converted to NonTermMatch", self.get_type())
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

#[derive(Debug, Clone)]
pub struct GroupProduction{
  pub rules: Vec<Box<Rule>>, 
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
  
  #[track_caller]
  pub fn to_GroupProduction (self)-> Box::<GroupProduction> {
    
    match self{
      Self::GroupProduction(val) => val,
      _ => panic!("Type {:?} cannot be converted to GroupProduction", self.get_type())
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

#[derive(Debug, Clone)]
pub struct List_Production{
  pub optional: bool, 
  pub symbol: ASTNode, 
  pub terminal_symbol: Option<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_List_Production (self)-> Box::<List_Production> {
    
    match self{
      Self::List_Production(val) => val,
      _ => panic!("Type {:?} cannot be converted to List_Production", self.get_type())
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

#[derive(Debug, Clone)]
pub struct SetTokenId{
  pub id: u32, 
}

impl SetTokenId{
  
  pub fn new (id: u32)-> Self {
    
    Self{
      id,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SetTokenId
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_SetTokenId (self)-> Box::<SetTokenId> {
    
    match self{
      Self::SetTokenId(val) => val,
      _ => panic!("Type {:?} cannot be converted to SetTokenId", self.get_type())
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

#[derive(Debug, Clone)]
pub struct TerminalToken{
  pub is_exclusive: bool, 
  pub val: String, 
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
  
  #[track_caller]
  pub fn to_TerminalToken (self)-> Box::<TerminalToken> {
    
    match self{
      Self::TerminalToken(val) => val,
      _ => panic!("Type {:?} cannot be converted to TerminalToken", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Pop{
}

impl Pop{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Pop
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Pop (self)-> Box::<Pop> {
    
    match self{
      Self::Pop(val) => val,
      _ => panic!("Type {:?} cannot be converted to Pop", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Vector{
  pub initializer: Vec<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_AST_Vector (self)-> Box::<AST_Vector> {
    
    match self{
      Self::AST_Vector(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Vector", self.get_type())
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

#[derive(Debug, Clone)]
pub struct FailHint{
  pub message: String, 
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
  
  #[track_caller]
  pub fn to_FailHint (self)-> Box::<FailHint> {
    
    match self{
      Self::FailHint(val) => val,
      _ => panic!("Type {:?} cannot be converted to FailHint", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Production_Symbol{
  pub name: String, 
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
  
  #[track_caller]
  pub fn to_Production_Symbol (self)-> Box::<Production_Symbol> {
    
    match self{
      Self::Production_Symbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production_Symbol", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Grammar{
  pub preamble: Vec<ASTNode>, 
  pub productions: Vec<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_Grammar (self)-> Box::<Grammar> {
    
    match self{
      Self::Grammar(val) => val,
      _ => panic!("Type {:?} cannot be converted to Grammar", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_U64{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_U64 (self)-> Box::<AST_U64> {
    
    match self{
      Self::AST_U64(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U64", self.get_type())
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

#[derive(Debug, Clone)]
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
  
  #[track_caller]
  pub fn to_DEFINED_TYPE_IDENT (self)-> Box::<DEFINED_TYPE_IDENT> {
    
    match self{
      Self::DEFINED_TYPE_IDENT(val) => val,
      _ => panic!("Type {:?} cannot be converted to DEFINED_TYPE_IDENT", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Pass{
}

impl Pass{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Pass
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Pass (self)-> Box::<Pass> {
    
    match self{
      Self::Pass(val) => val,
      _ => panic!("Type {:?} cannot be converted to Pass", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Import{
  pub reference: String, 
  pub uri: String, 
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
  
  #[track_caller]
  pub fn to_Import (self)-> Box::<Import> {
    
    match self{
      Self::Import(val) => val,
      _ => panic!("Type {:?} cannot be converted to Import", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Init{
  pub expression: ASTNode, 
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
  
  #[track_caller]
  pub fn to_Init (self)-> Box::<Init> {
    
    match self{
      Self::Init(val) => val,
      _ => panic!("Type {:?} cannot be converted to Init", self.get_type())
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

#[derive(Debug, Clone)]
pub struct CFProduction{
  pub name_sym: Box<Production_Symbol>, 
  pub rules: Vec<Box<Rule>>, 
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
  
  #[track_caller]
  pub fn to_CFProduction (self)-> Box::<CFProduction> {
    
    match self{
      Self::CFProduction(val) => val,
      _ => panic!("Type {:?} cannot be converted to CFProduction", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_IndexReference{
  pub value: i64, 
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
  
  #[track_caller]
  pub fn to_AST_IndexReference (self)-> Box::<AST_IndexReference> {
    
    match self{
      Self::AST_IndexReference(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_IndexReference", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Member{
  pub property: Token, 
  pub reference: ASTNode, 
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
  
  #[track_caller]
  pub fn to_AST_Member (self)-> Box::<AST_Member> {
    
    match self{
      Self::AST_Member(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Member", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Production_Import_Symbol{
  pub module: String, 
  pub name: String, 
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
  
  #[track_caller]
  pub fn to_Production_Import_Symbol (self)-> Box::<Production_Import_Symbol> {
    
    match self{
      Self::Production_Import_Symbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production_Import_Symbol", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_I32{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_I32 (self)-> Box::<AST_I32> {
    
    match self{
      Self::AST_I32(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I32", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Goto{
  pub prod: ASTNode, 
}

impl Goto{
  
  pub fn new (prod: ASTNode)-> Self {
    
    Self{
      prod,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Goto
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Goto (self)-> Box::<Goto> {
    
    match self{
      Self::Goto(val) => val,
      _ => panic!("Type {:?} cannot be converted to Goto", self.get_type())
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
    self.prod.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_I8{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_I8 (self)-> Box::<AST_I8> {
    
    match self{
      Self::AST_I8(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I8", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_F32{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_F32 (self)-> Box::<AST_F32> {
    
    match self{
      Self::AST_F32(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_F32", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Gotos{
  pub goto: Box<Goto>, 
  pub pushes: Vec<Box<Push>>, 
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
  
  #[track_caller]
  pub fn to_Gotos (self)-> Box::<Gotos> {
    
    match self{
      Self::Gotos(val) => val,
      _ => panic!("Type {:?} cannot be converted to Gotos", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Token{
  pub range: Option<Box<Range>>, 
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
  
  #[track_caller]
  pub fn to_AST_Token (self)-> Box::<AST_Token> {
    
    match self{
      Self::AST_Token(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Token", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Shift{
}

impl Shift{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Shift
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Shift (self)-> Box::<Shift> {
    
    match self{
      Self::Shift(val) => val,
      _ => panic!("Type {:?} cannot be converted to Shift", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Export{
  pub production: ASTNode, 
  pub reference: String, 
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
  
  #[track_caller]
  pub fn to_Export (self)-> Box::<Export> {
    
    match self{
      Self::Export(val) => val,
      _ => panic!("Type {:?} cannot be converted to Export", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Name{
  pub name: String, 
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
  
  #[track_caller]
  pub fn to_Name (self)-> Box::<Name> {
    
    match self{
      Self::Name(val) => val,
      _ => panic!("Type {:?} cannot be converted to Name", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_BOOL{
  pub initializer: Option<Box<Init>>, 
  pub value: bool, 
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
  
  #[track_caller]
  pub fn to_AST_BOOL (self)-> Box::<AST_BOOL> {
    
    match self{
      Self::AST_BOOL(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_BOOL", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Fail{
}

impl Fail{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Fail
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Fail (self)-> Box::<Fail> {
    
    match self{
      Self::Fail(val) => val,
      _ => panic!("Type {:?} cannot be converted to Fail", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Statements{
  pub statements: Vec<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_AST_Statements (self)-> Box::<AST_Statements> {
    
    match self{
      Self::AST_Statements(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Statements", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_U8{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_U8 (self)-> Box::<AST_U8> {
    
    match self{
      Self::AST_U8(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U8", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Assert{
}

impl Assert{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Assert
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Assert (self)-> Box::<Assert> {
    
    match self{
      Self::Assert(val) => val,
      _ => panic!("Type {:?} cannot be converted to Assert", self.get_type())
    }
  }
  
  pub fn as_Assert (&self)-> Option<&Assert> {
    
    match self{
      Self::Assert(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Assert_mut (&mut self)-> Option<&mut Assert> {
    
    match self{
      Self::Assert(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Assert{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Ignore{
  pub symbols: Vec<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_Ignore (self)-> Box::<Ignore> {
    
    match self{
      Self::Ignore(val) => val,
      _ => panic!("Type {:?} cannot be converted to Ignore", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_STRING{
  pub value: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_STRING (self)-> Box::<AST_STRING> {
    
    match self{
      Self::AST_STRING(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_STRING", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Skip{
}

impl Skip{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Skip
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Skip (self)-> Box::<Skip> {
    
    match self{
      Self::Skip(val) => val,
      _ => panic!("Type {:?} cannot be converted to Skip", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_U32{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_U32 (self)-> Box::<AST_U32> {
    
    match self{
      Self::AST_U32(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U32", self.get_type())
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

#[derive(Debug, Clone)]
pub struct State{
  pub catches: bool, 
  pub id: Box<Production_Symbol>, 
  pub statement: Box<Statement>, 
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
  
  #[track_caller]
  pub fn to_State (self)-> Box::<State> {
    
    match self{
      Self::State(val) => val,
      _ => panic!("Type {:?} cannot be converted to State", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_I64{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_I64 (self)-> Box::<AST_I64> {
    
    match self{
      Self::AST_I64(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I64", self.get_type())
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

#[derive(Debug, Clone)]
pub struct DefaultMatch{
  pub statement: Box<Statement>, 
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
  
  #[track_caller]
  pub fn to_DefaultMatch (self)-> Box::<DefaultMatch> {
    
    match self{
      Self::DefaultMatch(val) => val,
      _ => panic!("Type {:?} cannot be converted to DefaultMatch", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_U16{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_U16 (self)-> Box::<AST_U16> {
    
    match self{
      Self::AST_U16(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U16", self.get_type())
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

#[derive(Debug, Clone)]
pub struct NotEmptySet{
  pub symbols: Vec<ASTNode>, 
  pub unordered: bool, 
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
  
  #[track_caller]
  pub fn to_NotEmptySet (self)-> Box::<NotEmptySet> {
    
    match self{
      Self::NotEmptySet(val) => val,
      _ => panic!("Type {:?} cannot be converted to NotEmptySet", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Accept{
}

impl Accept{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Accept
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Accept (self)-> Box::<Accept> {
    
    match self{
      Self::Accept(val) => val,
      _ => panic!("Type {:?} cannot be converted to Accept", self.get_type())
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

#[derive(Debug, Clone)]
pub struct ClassSymbol{
  pub val: String, 
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
  
  #[track_caller]
  pub fn to_ClassSymbol (self)-> Box::<ClassSymbol> {
    
    match self{
      Self::ClassSymbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to ClassSymbol", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Statement{
  pub branch: Option<ASTNode>, 
  pub non_branch: Vec<ASTNode>, 
  pub transitive: Option<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_Statement (self)-> Box::<Statement> {
    
    match self{
      Self::Statement(val) => val,
      _ => panic!("Type {:?} cannot be converted to Statement", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_F64{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_F64 (self)-> Box::<AST_F64> {
    
    match self{
      Self::AST_F64(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_F64", self.get_type())
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

#[derive(Debug, Clone)]
pub struct TokenGroupProduction{
  pub rules: Vec<Box<Rule>>, 
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
  
  #[track_caller]
  pub fn to_TokenGroupProduction (self)-> Box::<TokenGroupProduction> {
    
    match self{
      Self::TokenGroupProduction(val) => val,
      _ => panic!("Type {:?} cannot be converted to TokenGroupProduction", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Map{
  pub key: ASTNode, 
  pub val: ASTNode, 
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
  
  #[track_caller]
  pub fn to_AST_Map (self)-> Box::<AST_Map> {
    
    match self{
      Self::AST_Map(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Map", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Range{
  pub end_trim: i32, 
  pub start_trim: i32, 
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
  
  #[track_caller]
  pub fn to_Range (self)-> Box::<Range> {
    
    match self{
      Self::Range(val) => val,
      _ => panic!("Type {:?} cannot be converted to Range", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_I16{
  pub initializer: Option<Box<Init>>, 
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
  
  #[track_caller]
  pub fn to_AST_I16 (self)-> Box::<AST_I16> {
    
    match self{
      Self::AST_I16(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I16", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Scan{
}

impl Scan{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Scan
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Scan (self)-> Box::<Scan> {
    
    match self{
      Self::Scan(val) => val,
      _ => panic!("Type {:?} cannot be converted to Scan", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Push{
  pub prod: ASTNode, 
}

impl Push{
  
  pub fn new (prod: ASTNode)-> Self {
    
    Self{
      prod,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Push
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Push (self)-> Box::<Push> {
    
    match self{
      Self::Push(val) => val,
      _ => panic!("Type {:?} cannot be converted to Push", self.get_type())
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
    self.prod.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_NUMBER{
  pub value: f64, 
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
  
  #[track_caller]
  pub fn to_AST_NUMBER (self)-> Box::<AST_NUMBER> {
    
    match self{
      Self::AST_NUMBER(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_NUMBER", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Struct{
  pub props: Vec<ASTNode>, 
  pub typ: Token, 
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
  
  #[track_caller]
  pub fn to_AST_Struct (self)-> Box::<AST_Struct> {
    
    match self{
      Self::AST_Struct(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Struct", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Peek{
}

impl Peek{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Peek
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Peek (self)-> Box::<Peek> {
    
    match self{
      Self::Peek(val) => val,
      _ => panic!("Type {:?} cannot be converted to Peek", self.get_type())
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

#[derive(Debug, Clone)]
pub struct TermMatch{
  pub statement: Box<Statement>, 
  pub sym: Option<ASTNode>, 
  pub vals: Vec<u64>, 
}

impl TermMatch{
  
  pub fn new (statement: Box<Statement>, sym: Option<ASTNode>, vals: Vec<u64>)-> Self {
    
    Self{
      statement,
      sym,
      vals,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::TermMatch
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_TermMatch (self)-> Box::<TermMatch> {
    
    match self{
      Self::TermMatch(val) => val,
      _ => panic!("Type {:?} cannot be converted to TermMatch", self.get_type())
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
    self.vals.hash(hasher);
  }
}

#[derive(Debug, Clone)]
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
  
  #[track_caller]
  pub fn to_DEFINED_TYPE_NUM (self)-> Box::<DEFINED_TYPE_NUM> {
    
    match self{
      Self::DEFINED_TYPE_NUM(val) => val,
      _ => panic!("Type {:?} cannot be converted to DEFINED_TYPE_NUM", self.get_type())
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

#[derive(Debug, Clone)]
pub struct SetVal{
  pub id: String, 
  pub val: u32, 
}

impl SetVal{
  
  pub fn new (id: String, val: u32)-> Self {
    
    Self{
      id,
      val,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SetVal
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_SetVal (self)-> Box::<SetVal> {
    
    match self{
      Self::SetVal(val) => val,
      _ => panic!("Type {:?} cannot be converted to SetVal", self.get_type())
    }
  }
  
  pub fn as_SetVal (&self)-> Option<&SetVal> {
    
    match self{
      Self::SetVal(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SetVal_mut (&mut self)-> Option<&mut SetVal> {
    
    match self{
      Self::SetVal(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SetVal{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Rule{
  pub ast: Option<Box<Ascript>>, 
  pub symbols: Vec<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_Rule (self)-> Box::<Rule> {
    
    match self{
      Self::Rule(val) => val,
      _ => panic!("Type {:?} cannot be converted to Rule", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AnnotatedSymbol{
  pub is_optional: bool, 
  pub precedence: Option<Box<Precedence>>, 
  pub reference: String, 
  pub symbol: ASTNode, 
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
  
  #[track_caller]
  pub fn to_AnnotatedSymbol (self)-> Box::<AnnotatedSymbol> {
    
    match self{
      Self::AnnotatedSymbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to AnnotatedSymbol", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AST_Property{
  pub id: String, 
  pub named_reference: String, 
  pub value: Option<ASTNode>, 
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
  
  #[track_caller]
  pub fn to_AST_Property (self)-> Box::<AST_Property> {
    
    match self{
      Self::AST_Property(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Property", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Ascript{
  pub ast: ASTNode, 
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
  
  #[track_caller]
  pub fn to_Ascript (self)-> Box::<Ascript> {
    
    match self{
      Self::Ascript(val) => val,
      _ => panic!("Type {:?} cannot be converted to Ascript", self.get_type())
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

#[derive(Debug, Clone)]
pub struct AppendProduction{
  pub name_sym: ASTNode, 
  pub rules: Vec<Box<Rule>>, 
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
  
  #[track_caller]
  pub fn to_AppendProduction (self)-> Box::<AppendProduction> {
    
    match self{
      Self::AppendProduction(val) => val,
      _ => panic!("Type {:?} cannot be converted to AppendProduction", self.get_type())
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

#[derive(Debug, Clone)]
pub struct Production_Terminal_Symbol{
  pub production: ASTNode, 
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
  
  #[track_caller]
  pub fn to_Production_Terminal_Symbol (self)-> Box::<Production_Terminal_Symbol> {
    
    match self{
      Self::Production_Terminal_Symbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production_Terminal_Symbol", self.get_type())
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

fn reducer_000 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_001 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_002 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_2 = obj3;
  let obj_4_0 = AST_Property::new(
    tok_0_0,
    Default::default(),
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_003 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_2 = obj3;
  let obj_4_0 = AST_Property::new(
    tok_0_0,
    Default::default(),
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_004 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_0_1 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let obj_2_0 = AST_Property::new(
    tok_0_0,
    tok_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_005 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_006 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Peek::new();
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_007 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Shift::new();
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_008 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Skip::new();
  slots.assign(0, AstSlot(ASTNode::Skip(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_009 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Pop::new();
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_010 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Scan::new();
  slots.assign(0, AstSlot(ASTNode::Scan(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_0_0 = obj1.into_nodes();
  let obj_1_1 = obj2.into_nodes();
  let obj_3_0 = Grammar::new(
    obj_0_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Grammar(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_1 = obj1.into_nodes();
  let obj_2_0 = Grammar::new(
    vec![],
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Grammar(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Production_Terminal_Symbol::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Production_Terminal_Symbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_020 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_021 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_022 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let obj_1_1 = obj2.into_nodes();
  let obj_0_2 = obj1;
  let obj_4_0 = Statement::new(
    Some(obj_2_0),
    obj_1_1,
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_023 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_0_1 = obj1.into_nodes();
  let obj_3_0 = Statement::new(
    Some(obj_1_0),
    obj_0_1,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_024 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_0_2 = obj1;
  let obj_3_0 = Statement::new(
    Some(obj_1_0),
    vec![],
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_025 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let obj_2_0 = Statement::new(
    Some(obj_0_0),
    vec![],
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_1 = obj2.into_nodes();
  let obj_0_2 = obj1;
  let obj_3_0 = Statement::new(
    None,
    obj_1_1,
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_1 = obj1.into_nodes();
  let obj_2_0 = Statement::new(
    None,
    obj_0_1,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_2 = obj1;
  let obj_2_0 = Statement::new(
    None,
    vec![],
    Some(obj_0_2),
  );
  slots.assign(0, AstSlot(ASTNode::Statement(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let obj_3_1 = obj4.into_nodes();
  let obj_5_0 = AppendProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AppendProduction(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_0 = Production_Symbol::new(
    tok_0_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Production_Symbol(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_0 = obj3;
  let obj_4_1 = obj5;
  let obj_7_0 = AST_Map::new(
    obj_2_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Map(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_2_1 = __tok_rng_3;
  let tok_2_1 = tok_2_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_5_0 = Match::new(
    vec![],
    tok_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_040 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_041 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_4_0 = Init::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Init(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_0 = obj1;
  let obj_2_1 = obj3;
  let obj_4_0 = AST_Add::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Add(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_1 = true;
  let obj_3_0 = AST_BOOL::new(
    None,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_1 = false;
  let obj_3_0 = AST_BOOL::new(
    None,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_f64();
  let obj_2_0 = AST_NUMBER::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_NUMBER(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_048 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_049 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = true;
  let tok_0_1 = __rule_rng__;
  let tok_0_1 = tok_0_1.trim(1, 1);
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = TerminalToken::new(
    obj_2_0,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TerminalToken(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_1 = __rule_rng__;
  let tok_0_1 = tok_0_1.trim(1, 1);
  let tok_0_1 = tok_0_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_2_0 = TerminalToken::new(
    false,
    tok_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TerminalToken(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_STRING::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_STRING::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_3_0 = obj4.into_nodes();
  let tok_2_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_string();
  let obj_5_0 = Match::new(
    obj_3_0,
    tok_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_2_1 = __tok_rng_3;
  let tok_2_1 = tok_2_1.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_5_0 = Match::new(
    vec![],
    tok_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::Match(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_BOOL::new(
    Some(obj_1_0),
    false,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_BOOL::new(
    None,
    false,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_078 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_079 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_080 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_0 = obj5;
  let obj_4_0 = obj_4_0.to_Statement();
  let obj_1_2 = obj2.into_u64_vec();
  let obj_7_0 = TermMatch::new(
    obj_4_0,
    None,
    obj_1_2,
  );
  slots.assign(0, AstSlot(ASTNode::TermMatch(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = TokenGroupProduction::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::TokenGroupProduction(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  let obj_3_0 = AST_Statements::new(
    obj_2_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = AST_Statements::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = AST_Statements::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let tok_3_1 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_1 = tok_3_1.to_string();
  let obj_5_0 = Export::new(
    obj_1_0,
    tok_3_1,
  );
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let tok_3_1 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_1 = tok_3_1.to_string();
  let obj_5_0 = Export::new(
    obj_1_0,
    tok_3_1,
  );
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Export::new(
    obj_1_0,
    Default::default(),
  );
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Production_Symbol();
  let obj_3_1 = obj4.into_nodes();
  let obj_5_0 = PrattProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::PrattProduction(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_3_0 = obj4.into_nodes();
  let tok_1_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_6_0 = AST_Struct::new(
    obj_3_0,
    tok_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_4_0 = AST_Struct::new(
    vec![],
    tok_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<u64>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_2_0 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_0_1 = obj1;
  let obj_4_0 = AST_Member::new(
    tok_2_0,
    obj_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Member(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_0 = obj5;
  let obj_4_0 = obj_4_0.to_Statement();
  let obj_1_1 = obj2;
  let obj_7_0 = NonTermMatch::new(
    obj_4_0,
    obj_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::NonTermMatch(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_126 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_127 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Production_Symbol();
  let obj_3_1 = obj4.into_nodes();
  let obj_5_0 = CFProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::CFProduction(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Push::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Push(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  let obj_4_0 = Precedence::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Precedence(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Precedence();
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_3_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_132 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Precedence();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_2_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_133 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = false;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Precedence();
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_2_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = false;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Precedence();
  let obj_0_3 = obj1;
  let obj_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    Some(obj_1_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    None,
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = true;
  let obj_0_3 = obj1;
  let obj_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    None,
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = false;
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    None,
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Precedence();
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_2_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Precedence();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_1_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Precedence();
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_3_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    None,
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Precedence();
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_2_1),
    tok_3_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = false;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Precedence();
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_1_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Precedence();
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_1_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Precedence();
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_1_1),
    tok_3_2,
    obj_0_3,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  let obj_3_0 = Name::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Name(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Production_Symbol();
  let obj_3_1 = obj4.into_nodes();
  let obj_5_0 = PegProduction::new(
    obj_1_0,
    obj_3_1.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::PegProduction(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<u64>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U8::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I8::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_F32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_F32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_F64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_F64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Ascript::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Ascript(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3.into_nodes();
  let obj_5_0 = Ignore::new(
    obj_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::Ignore(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_2_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_string();
  let obj_4_0 = Production_Import_Symbol::new(
    tok_0_0,
    tok_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Production_Import_Symbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_1 = obj1;
  let obj_0_1 = obj_0_1.to_Production_Symbol();
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Statement();
  let obj_4_0 = State::new(
    false,
    obj_0_1,
    obj_2_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::State(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_0_1 = obj1;
  let obj_0_1 = obj_0_1.to_Production_Symbol();
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Statement();
  let obj_5_0 = State::new(
    obj_4_0,
    obj_0_1,
    obj_2_2,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::State(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Fail::new();
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Pass::new();
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Accept::new();
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2.into_nodes();
  let obj_5_1 = true;
  let obj_6_0 = NotEmptySet::new(
    obj_1_0,
    obj_5_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_1 = false;
  let obj_5_0 = NotEmptySet::new(
    obj_1_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::NotEmptySet(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Goto::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::Goto(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_0 = obj5;
  let obj_4_0 = obj_4_0.to_Statement();
  let obj_1_1 = obj2;
  let obj_7_0 = TermMatch::new(
    obj_4_0,
    Some(obj_1_1),
    Default::default(),
  );
  slots.assign(0, AstSlot(ASTNode::TermMatch(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Range();
  let obj_3_0 = AST_Token::new(
    Some(obj_1_0),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Range();
  let obj_3_0 = AST_Token::new(
    Some(obj_1_0),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Range();
  let obj_3_0 = AST_Token::new(
    Some(obj_1_0),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = AST_Vector::new(
    obj_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = AST_Vector::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_199 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  let obj_3_0 = AST_NamedReference::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_NamedReference(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_i64();
  let obj_3_0 = AST_IndexReference::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_IndexReference(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_202 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_4_0 = __tok_rng_5.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_0 = tok_4_0.to_string();
  let obj_1_1 = obj2.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let obj_6_0 = Import::new(
    tok_4_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_4_0 = __tok_rng_5.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_0 = tok_4_0.to_string();
  let obj_1_1 = obj2.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let obj_6_0 = Import::new(
    tok_4_0,
    obj_1_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_0_1 = obj1;
  let obj_2_2 = obj3;
  let obj_5_0 = List_Production::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_0_1 = obj1;
  let obj_2_2 = obj3;
  let obj_5_0 = List_Production::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_1 = obj1;
  let obj_4_0 = List_Production::new(
    false,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_0_1 = obj1;
  let obj_2_2 = obj3;
  let obj_6_0 = List_Production::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_211 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_0_1 = obj1;
  let obj_2_2 = obj3;
  let obj_6_0 = List_Production::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_0_1 = obj1;
  let obj_5_0 = List_Production::new(
    obj_4_0,
    obj_0_1,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_214 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2.to_string();
  let mut obj_0_0 = obj1.into_strings();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_215 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.to_string();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_216 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_220 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = GroupProduction::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::GroupProduction(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3.to_u64();
  let mut obj_0_0 = obj1.into_u64_vec();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_230 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_231 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_232 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_233 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_234 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_235 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_3_0 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_0 = tok_3_0.to_i32();
  let tok_1_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_i32();
  let obj_6_0 = Range::new(
    tok_3_0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_1 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_1 = tok_1_1.to_i32();
  let obj_4_0 = Range::new(
    0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_2_0 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_0 = tok_2_0.to_string();
  let obj_5_0 = FailHint::new(
    tok_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::FailHint(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Statement();
  let obj_5_0 = DefaultMatch::new(
    obj_2_0,
  );
  slots.assign(0, AstSlot(ASTNode::DefaultMatch(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Statement();
  let obj_4_0 = DefaultMatch::new(
    obj_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::DefaultMatch(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_245 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_246 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_247 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_248 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_249 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_250 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Goto();
  let obj_0_1 = obj1.into_nodes();
  let obj_3_0 = Gotos::new(
    obj_1_0,
    obj_0_1.into_iter().map(|v|match v { ASTNode::Push(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let obj_0_0 = obj_0_0.to_Goto();
  let obj_2_0 = Gotos::new(
    obj_0_0,
    vec![],
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  slots.take(5);
  slots.take(6);
  let AstSlot (_, __tok_rng_8, _) = slots.take(7);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_8;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_4_1 = __tok_rng_5.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_1 = tok_4_1.to_u32();
  let tok_7_2 = __tok_rng_8.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_7_2 = tok_7_2.to_u32();
  let obj_9_0 = Reduce::new(
    tok_1_0,
    tok_4_1,
    tok_7_2,
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_9_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  slots.take(3);
  slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_2_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_u32();
  let tok_5_2 = __tok_rng_6.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_5_2 = tok_5_2.to_u32();
  let obj_7_0 = Reduce::new(
    tok_1_0,
    tok_2_1,
    tok_5_2,
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_4_1 = __tok_rng_5.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_4_1 = tok_4_1.to_u32();
  let tok_5_2 = __tok_rng_6.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_5_2 = tok_5_2.to_u32();
  let obj_7_0 = Reduce::new(
    tok_1_0,
    tok_4_1,
    tok_5_2,
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let tok_2_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_u32();
  let tok_3_2 = __tok_rng_4.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_3_2 = tok_3_2.to_u32();
  let obj_5_0 = Reduce::new(
    tok_1_0,
    tok_2_1,
    tok_3_2,
  );
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_u32();
  let obj_3_0 = SetTokenId::new(
    tok_1_0,
  );
  slots.assign(0, AstSlot(ASTNode::SetTokenId(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  let tok_2_1 = __tok_rng_3.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_2_1 = tok_2_1.to_u32();
  let obj_4_0 = SetVal::new(
    tok_1_0,
    tok_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::SetVal(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Assert::new();
  slots.assign(0, AstSlot(ASTNode::Assert(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_0_1 = obj1.into_nodes();
  let obj_3_0 = Rule::new(
    Some(obj_1_0),
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_1 = obj1.into_nodes();
  let obj_2_0 = Rule::new(
    None,
    obj_0_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 271]
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
    ])
  }
}

    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader + std::fmt::Debug {}

impl<T: ByteReader + MutByteReader + UTF8Reader + std::fmt::Debug> Reader for T {}

pub type Parser<'a, T, UserCTX> = sherpa_runtime::bytecode_parser::ByteCodeParser<'a, T, UserCTX>;

pub mod meta{
  
  pub const production_names: [&'static str;98] = [
    "defined_type_eval::escaped_string_list_1_group_0",
    "ascript::struct_prop",
    "ir::transitive_statement",
    "token::quote",
    "sherpa::rules_list_1",
    "sherpa::grammar_list_1",
    "sherpa::grammar",
    "sherpa_symbol::token_non_terminal",
    "defined_type_eval::id",
    "ascript::init_objects",
    "ir::statement",
    "sherpa::append_production",
    "sherpa::rules",
    "ir::match",
    "sherpa_symbol::production_symbol",
    "defined_type_eval::escaped_string_list_1_group_1",
    "ascript::map",
    "ir::production_match_block",
    "sherpa::not_empty_list_1",
    "ascript::convert_initializer",
    "ascript::add",
    "token::int",
    "ascript::literal",
    "sherpa::rule_group_0",
    "ir::generic_match_block_group_3_list_1",
    "sherpa::ignore_clause_list_1",
    "sherpa_symbol::class",
    "sherpa_symbol::token",
    "ascript::string_convert",
    "ir::generic_match_block",
    "ir::terminal_match_block",
    "sherpa::import_clause_list_1",
    "ascript::bool_convert",
    "sherpa_symbol::symbol",
    "ascript::vector_list_1",
    "ir::int_match",
    "sherpa_symbol::terminal",
    "ascript::body",
    "ir::production_match_block_group_3",
    "sherpa::export_clause",
    "sherpa::pratt_production",
    "ir::statement_list_1",
    "ascript::body_list_1",
    "token::string",
    "ascript::struct",
    "ir::goto_sequence_list_1",
    "ir::int_match_list_1_group_0",
    "defined_type_eval::escaped_vals",
    "ascript::type_identifier",
    "defined_type_eval::num",
    "defined_type_eval::escaped",
    "ascript::member",
    "ir::non_terminal_match",
    "ir::production_match_block_group_3_list_1",
    "sherpa::cf_production",
    "ir::goto_push",
    "sherpa_symbol::precedence",
    "sherpa_symbol::annotated_symbol",
    "sherpa::name_clause",
    "sherpa::peg_production",
    "ir::int_match_list_1_group_2",
    "ascript::numeric_convert",
    "ascript::identifier",
    "sherpa::ast_definition",
    "sherpa::ignore_clause",
    "sherpa_symbol::import_production_symbol",
    "ir::state",
    "ir::terminal_statement",
    "sherpa::not_empty",
    "ir::goto",
    "token::id",
    "sherpa::rule_group_0_list_1",
    "ir::terminal_match",
    "ascript::token",
    "ascript::vector",
    "sherpa::preamble",
    "ascript::reference",
    "ir::statement_group_2",
    "sherpa::import_clause",
    "sherpa_symbol::list",
    "defined_type_eval::escaped_string_list_1",
    "ir::generic_match_block_group_3",
    "sherpa_symbol::non_terminal",
    "ir::int_match_list_1",
    "sherpa::grammar_list_2",
    "defined_type_eval::def_type",
    "ir::terminal_match_block_group_3",
    "ascript::range",
    "ir::hint",
    "ir::default_match",
    "ascript::struct_list_1",
    "ir::terminal_match_block_group_3_list_1",
    "ir::goto_sequence",
    "defined_type_eval::escaped_string",
    "ir::non_branch_statement",
    "sherpa::rule",
    "ascript::expression",
    "sherpa_symbol::end_of_input",
  ];
  
  pub const symbol_string: [&'static str;113] = [
    r####"tk:id_rest"####,
    r####"'tok'"####,
    r####"'num'"####,
    r####"'nl'"####,
    r####"'id'"####,
    r####"'htab'"####,
    r####"'tab'"####,
    r####"'sp'"####,
    r####"'any'"####,
    r####"'sym'"####,
    r####"'.'"####,
    r####"'_'"####,
    r####"')'"####,
    r####"'::'"####,
    r####"']'"####,
    r####"'}'"####,
    r####"','"####,
    r####"'('"####,
    r####"'-'"####,
    r####""u16""####,
    r####""i32""####,
    r####""i8""####,
    r####""set-tok""####,
    r####""true""####,
    r####""u32""####,
    r####""reduce""####,
    r####""t_""####,
    r####""str""####,
    r####""symbols""####,
    r####""default""####,
    r####""EXPORT""####,
    r####""i16""####,
    r####""TERMINAL""####,
    r####""peek""####,
    r####""assert""####,
    r####""shift""####,
    r####""with""####,
    r####""skip""####,
    r####""f64""####,
    r####""false""####,
    r####""push""####,
    r####""to""####,
    r####""goto""####,
    r####""fail-hint""####,
    r####""bool""####,
    r####""scan""####,
    r####""rule""####,
    r####""fail""####,
    r####""IGNORE""####,
    r####""NAME""####,
    r####""u64""####,
    r####""map""####,
    r####""i64""####,
    r####""u8""####,
    r####""pass""####,
    r####""PRODUCTION""####,
    r####""pop""####,
    r####""f32""####,
    r####""tk""####,
    r####""AS""####,
    r####""IMPORT""####,
    r####""token""####,
    r####""accept""####,
    r####""set""####,
    r####""match""####,
    r####""as""####,
    r####""//""####,
    r####""?""####,
    r####""tk:""####,
    r####""{""####,
    r####"":>""####,
    r####""_""####,
    r####"")""####,
    r####""!""####,
    r####""'""####,
    r####""]""####,
    r####"":""####,
    r####""}""####,
    r####"",""####,
    r####"":ast""####,
    r####""+""####,
    r####""/*""####,
    r####""|""####,
    r####""tk:(""####,
    r####"""""####,
    r####""$""####,
    r####""<""####,
    r####""(""####,
    r####""=>""####,
    r####""=!>""####,
    r####""c:""####,
    r####""#>""####,
    r####""(*""####,
    r####""<>""####,
    r####""\""####,
    r####""-""####,
    r####""*/""####,
    r####""^""####,
    r####"";""####,
    r####"">""####,
    r####""(+""####,
    r####""+>""####,
    r####""[""####,
    r####"tk:precedence_num"####,
    r####"tk:int_tok"####,
    r####"tk:identifier"####,
    r####"tk:quote_tok"####,
    r####"tk:string_tok"####,
    r####"tk:block"####,
    r####"tk:line"####,
    r####"tk:reference"####,
    r####"tk:number"####,
    r####"tk:id_tok"####,
  ];
}

pub fn new_grammar_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(60);
  parser
}

pub fn new_ast_struct_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(21);
  parser
}

pub fn new_ast_expression_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(8);
  parser
}

pub fn new_ir_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(73);
  parser
}

pub fn new_type_eval_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(34);
  parser
}

pub fn new_escaped_parser<'a, T: Reader, UserCTX> (reader: &'a mut T)-> Parser<'a, T, UserCTX> {
  let mut parser = Parser::new(reader, &bytecode);
  parser.init_parser(47);
  parser
}

pub static bytecode: [u8; 152783] = [
  0,211,200,197,210,208,193,2,15,1,86,0,0,0,17,1,137,0,0,0,1,15,1,86,0,0,0,17,1,83,2,0,0,1,15,1,141,2,0,0,17,1,142,2,0,0,1,15,1,141,2,0,0,17,1,207,2,0,0,1, 
  15,1,23,4,0,0,17,1,59,4,0,0,1,15,1,23,4,0,0,17,1,155,5,0,0,1,20,5,23,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,22,0,0,0,13,20,2,27,0,0,0,0,0,0,0,2, 
  0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,13,21,1,201,1,0,0,50,229,1,0,23,0,0,0,4,0,0,0,16,112,3,128,65,224,8,128,34,224,195,131,35,80,196,131,36,192,196,131,5,64,142,130,6, 
  64,206,129,39,240,197,131,72,160,10,128,73,16,11,128,42,96,6,128,59,112,8,128,76,128,11,128,38,48,197,128,46,208,6,128,53,64,135,129,54,176,7,128,66,80,9,128,67,192,9,128,68,48,74,128,100,240,11,128,117, 
  184,12,128,119,40,13,128,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,56,6,0,0,4,17,1,68,6,0,0,1,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,56,6,0,0,4,17,1,218,6, 
  0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,1,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,56,6,0, 
  0,4,17,1,199,7,0,0,1,15,1,56,6,0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,1,15,1,93,8,0,0,4,17,1,105,8,0,0, 
  1,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,56, 
  6,0,0,4,17,1,219,9,0,0,1,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,188,10,0,0,15,1,200,10,0,0,15,1,236,10,0,0,17,1,248, 
  10,0,0,1,15,1,56,11,0,0,4,17,1,68,11,0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,1,8,2,20,4,30,0,0, 
  0,0,0,0,0,1,0,0,0,123,0,0,0,22,0,0,0,5,17,1,168,14,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,13,21,1,64,0,0,0, 
  233,229,1,0,2,0,0,0,1,0,0,0,120,208,64,128,126,104,1,128,4,19,8,0,0,0,19,0,0,0,1,0,17,1,214,14,0,0,1,4,19,49,0,0,0,113,0,0,0,1,0,17,1,249,14,0,0,1,2,20, 
  4,54,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,15,1,28,15,0,0,15,1,40,15,0,0,15,1,76,15,0,0,15,1,88,15,0,0,5,17,1,100,15,0,0,1,20,2,17,1,0,0,0, 
  0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,85,0,0,0,132,0,0,0,179,0,0,0,226,0,0,0,15,1,28,15,0,0,5,19,47,0,0,0,109,0,0,0,1,0,19,0,0,0,0,0,0,0,0,1,0, 
  19,80,0,0,0,215,0,0,0,1,0,17,1,40,15,0,0,1,15,1,28,15,0,0,5,19,47,0,0,0,108,0,0,0,1,0,19,0,0,0,0,0,0,0,0,1,0,19,80,0,0,0,215,0,0,0,1,0,17,1, 
  40,15,0,0,1,15,1,28,15,0,0,5,19,47,0,0,0,107,0,0,0,1,0,19,0,0,0,0,0,0,0,0,1,0,19,80,0,0,0,215,0,0,0,1,0,17,1,40,15,0,0,1,15,1,28,15,0,0,5,19, 
  47,0,0,0,110,0,0,0,1,0,19,0,0,0,0,0,0,0,0,1,0,19,80,0,0,0,215,0,0,0,1,0,17,1,40,15,0,0,1,15,1,28,15,0,0,5,19,47,0,0,0,111,0,0,0,1,0,19,0,0, 
  0,0,0,0,0,0,1,0,19,80,0,0,0,215,0,0,0,1,0,17,1,40,15,0,0,1,2,21,1,35,0,0,0,62,230,1,0,4,0,0,0,2,0,0,0,124,16,1,128,5,16,1,128,6,16,1,128,123,16,1, 
  128,8,13,21,1,95,1,0,0,115,230,1,0,13,0,0,0,3,0,0,0,64,48,4,128,45,48,194,129,106,0,7,128,75,48,197,129,108,208,71,129,5,240,10,127,6,240,10,128,63,48,67,129,85,48,6,128,116,160,136, 
  128,123,240,10,128,124,240,10,128,127,112,9,128,15,1,204,15,0,0,15,1,156,16,0,0,15,1,192,16,0,0,15,1,204,16,0,0,4,17,1,216,16,0,0,1,15,1,204,15,0,0,15,1,156,16,0,0,15,1,192, 
  16,0,0,15,1,49,17,0,0,4,17,1,61,17,0,0,1,15,1,204,15,0,0,15,1,156,16,0,0,15,1,192,16,0,0,15,1,109,17,0,0,4,17,1,121,17,0,0,1,15,1,204,15,0,0,15,1,156,16,0, 
  0,15,1,192,16,0,0,15,1,185,17,0,0,4,17,1,197,17,0,0,1,15,1,35,18,0,0,15,1,47,18,0,0,15,1,83,18,0,0,4,17,1,95,18,0,0,1,15,1,35,18,0,0,15,1,47,18,0,0,15, 
  1,165,18,0,0,4,17,1,177,18,0,0,1,15,1,35,18,0,0,15,1,47,18,0,0,15,1,247,18,0,0,4,17,1,3,19,0,0,1,15,1,35,18,0,0,15,1,47,18,0,0,15,1,73,19,0,0,4,17,1, 
  85,19,0,0,1,15,1,35,18,0,0,15,1,47,18,0,0,15,1,174,19,0,0,4,19,70,0,0,0,182,0,0,0,1,0,19,14,0,0,0,34,0,0,0,1,0,17,1,186,19,0,0,1,8,2,21,1,69,0,0, 
  0,42,231,1,0,5,0,0,0,2,0,0,0,124,32,2,128,5,32,2,128,6,32,2,128,123,32,66,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,14,0,0,0,34,0,0,0,1,0,17,1,186,19, 
  0,0,1,8,2,19,96,0,0,0,11,1,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,60,0,0,0,22,0,0,0,15,1,246,19,0,0,5,17,1,2,20,0,0,1,20,2,27,0,0,0,0, 
  0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,73,0,0,0,192,0,0,0,1,0,1,19,96,0,0,0,7,1,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0, 
  0,0,22,0,0,0,15,1,53,20,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,153,0,0,0,1,0,1,20, 
  4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,188,20,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0, 
  0,9,19,61,0,0,0,163,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,200,20,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0, 
  2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,159,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,212,20,0,0,5,17,1, 
  65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,155,0,0,0,1,0,1,19,96,0,0,0,6,1,0,0,1,0,1,20,4,36,0, 
  0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,224,20,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19, 
  28,0,0,0,69,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,236,20,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0, 
  0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,161,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,248,20,0,0,5,17,1,65,20,0, 
  0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,169,0,0,0,1,0,1,19,96,0,0,0,8,1,0,0,1,0,1,20,4,36,0,0,0,0, 
  0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,4,21,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,32,0,0, 
  0,77,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,16,21,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0, 
  0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,157,0,0,0,1,0,1,19,96,0,0,0,13,1,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,5,17,1, 
  28,21,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,87,23,0, 
  0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,165,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0, 
  0,0,40,0,0,0,22,0,0,0,15,1,99,23,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,61,0,0,0,151,0,0,0, 
  1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,111,23,0,0,5,17,1,65,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0, 
  0,26,0,0,0,9,19,61,0,0,0,167,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,60,0,0,0,22,0,0,0,15,1,123,23,0,0,5,17,1,2,20,0,0,1,20,2,27,0,0,0, 
  0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,73,0,0,0,191,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,60,0,0,0,22,0,0,0,15,1,135,23,0, 
  0,5,17,1,2,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,73,0,0,0,193,0,0,0,1,0,1,19,96,0,0,0,12,1,0,0,1,0,1, 
  20,0,35,0,0,0,0,0,0,0,1,0,0,0,20,0,0,0,22,0,0,0,15,1,200,10,0,0,17,1,147,23,0,0,1,2,19,20,0,0,0,43,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0, 
  0,0,36,0,0,0,22,0,0,0,15,1,202,23,0,0,5,17,1,15,24,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,96,0,0,0,10,1,0, 
  0,1,0,1,21,1,99,3,0,0,6,233,1,0,24,0,0,0,4,0,0,0,16,144,3,128,65,128,15,128,34,144,196,131,35,144,197,131,36,144,198,131,5,16,155,130,6,16,219,129,39,8,9,132,72,128,19,128,73,128, 
  20,128,42,8,138,130,59,128,14,128,76,128,21,128,38,144,199,128,46,8,11,128,53,8,204,129,54,8,13,128,66,128,16,128,67,128,17,128,68,128,146,128,90,128,22,128,100,232,22,128,117,64,24,128,119,64,25,128,15,1, 
  99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,68,6,0, 
  0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17, 
  1,218,6,0,0,1,15,1,99,24,0,0,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,34,0,0,0,81,0,0,0,1,0,17,1,162,24,0,0,1,15,1,99,24,0,0,15, 
  1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,99, 
  24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,18,8,0,0, 
  1,15,1,99,24,0,0,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,34,0,0,0,81,0,0,0,1,0,17,1,162,24,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0, 
  15,1,198,24,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,99,24,0,0,15,1, 
  162,24,0,0,15,1,198,24,0,0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,99,24, 
  0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,6,0,0,4,17,1,219,9,0,0,1, 
  15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,225,5,0,0,4,17,1,113, 
  10,0,0,1,4,19,74,0,0,0,195,0,0,0,2,0,1,15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,188,10,0,0,15,1,200,10,0,0,15,1,236,10,0,0,17,1,248,10,0,0,1, 
  15,1,99,24,0,0,15,1,162,24,0,0,15,1,198,24,0,0,15,1,56,11,0,0,4,17,1,68,11,0,0,1,15,1,99,24,0,0,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1, 
  0,19,96,0,0,0,9,1,0,0,1,0,19,34,0,0,0,81,0,0,0,1,0,17,1,162,24,0,0,1,8,2,21,1,45,0,0,0,200,233,1,0,3,0,0,0,1,0,0,0,6,96,1,128,5,96,65,128,41,240, 
  0,128,15,1,210,24,0,0,4,17,1,29,25,0,0,1,8,2,20,5,34,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,22,0,0,0,19,85,0,0,0,233,0,0,0,1,0,1,2,20,5,34,0,0,0,0, 
  0,0,0,1,0,0,0,1,0,0,0,22,0,0,0,19,85,0,0,0,234,0,0,0,1,0,1,2,19,93,0,0,0,252,0,0,0,1,0,1,20,0,35,0,0,0,0,0,0,0,1,0,0,0,80,0,0,0,22,0, 
  0,0,15,1,40,15,0,0,17,1,96,25,0,0,1,2,19,80,0,0,0,215,0,0,0,1,0,1,19,0,0,0,0,1,0,0,0,1,0,1,20,2,103,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0, 
  0,0,51,0,0,0,64,0,0,0,77,0,0,0,90,0,0,0,5,19,50,0,0,0,116,0,0,0,2,0,1,5,19,50,0,0,0,115,0,0,0,2,0,1,5,19,50,0,0,0,114,0,0,0,2,0,1,5,19,50, 
  0,0,0,117,0,0,0,2,0,1,5,19,50,0,0,0,118,0,0,0,2,0,1,2,21,1,207,0,0,0,12,237,1,0,9,0,0,0,3,0,0,0,85,176,1,128,116,32,196,129,106,128,2,128,123,112,6,128,108,80, 
  67,127,5,112,198,126,6,112,6,128,127,240,4,128,124,112,6,128,15,1,180,25,0,0,15,1,47,18,0,0,15,1,83,18,0,0,4,17,1,95,18,0,0,1,15,1,180,25,0,0,15,1,47,18,0,0,15,1,165,18, 
  0,0,4,17,1,177,18,0,0,1,15,1,180,25,0,0,15,1,47,18,0,0,15,1,247,18,0,0,4,17,1,3,19,0,0,1,15,1,180,25,0,0,15,1,47,18,0,0,15,1,73,19,0,0,4,17,1,85,19,0, 
  0,1,15,1,180,25,0,0,15,1,47,18,0,0,15,1,174,19,0,0,4,19,70,0,0,0,182,0,0,0,1,0,19,14,0,0,0,34,0,0,0,1,0,17,1,186,19,0,0,1,8,2,20,0,35,0,0,0,0,0, 
  0,0,1,0,0,0,5,0,0,0,22,0,0,0,15,1,156,16,0,0,17,1,192,25,0,0,1,2,19,5,0,0,0,15,0,0,0,1,0,1,19,75,0,0,0,196,0,0,0,1,0,1,21,1,88,0,0,0,159,237, 
  1,0,6,0,0,0,2,0,0,0,124,184,2,128,5,184,2,128,6,184,130,128,123,184,130,128,102,80,1,128,127,240,1,128,15,1,3,26,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,37,29,0,0, 
  4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,19,75,0,0,0,199,0,0,0,1,0,1,21,1,47,0,0,0,13,238,1,0,5,0,0,0,2,0,0,0,84,48,1,129,5,112,1,128,6, 
  112,1,128,123,112,1,128,124,112,1,128,4,17,1,175,29,0,0,1,8,2,19,75,0,0,0,198,0,0,0,1,0,1,21,1,63,0,0,0,42,231,1,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6, 
  240,1,128,123,240,65,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,58,0,0,0,147,0,0,0,2,0,1,8,2,19,75,0,0,0,197,0,0,0,1,0,1,21,1,93,0,0,0,77,238,1,0,6, 
  0,0,0,2,0,0,0,124,224,2,128,5,224,2,128,2,80,129,128,3,24,130,128,6,224,2,128,123,224,2,128,15,1,181,30,0,0,4,19,31,0,0,0,75,0,0,0,1,0,17,1,225,30,0,0,1,15,1,181,30, 
  0,0,4,19,31,0,0,0,74,0,0,0,1,0,17,1,225,30,0,0,1,8,2,19,6,0,0,0,17,0,0,0,1,0,1,20,0,35,0,0,0,0,0,0,0,1,0,0,0,84,0,0,0,22,0,0,0,15,1,47, 
  18,0,0,17,1,5,31,0,0,1,2,19,84,0,0,0,230,0,0,0,1,0,1,21,1,69,0,0,0,42,231,1,0,5,0,0,0,2,0,0,0,124,32,2,128,5,32,2,128,6,32,2,128,123,32,66,128,127,48,1, 
  128,4,19,70,0,0,0,182,0,0,0,1,0,19,14,0,0,0,34,0,0,0,1,0,17,1,76,31,0,0,1,8,2,19,84,0,0,0,229,0,0,0,1,0,1,21,1,69,0,0,0,42,231,1,0,5,0,0,0,2, 
  0,0,0,124,32,2,128,5,32,2,128,6,32,2,128,123,32,66,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,14,0,0,0,34,0,0,0,1,0,17,1,124,31,0,0,1,8,2,19,84,0,0,0, 
  228,0,0,0,1,0,1,21,1,69,0,0,0,42,231,1,0,5,0,0,0,2,0,0,0,124,32,2,128,5,32,2,128,6,32,2,128,123,32,66,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,14,0, 
  0,0,34,0,0,0,1,0,17,1,172,31,0,0,1,8,2,19,84,0,0,0,231,0,0,0,1,0,1,21,1,88,0,0,0,159,237,1,0,6,0,0,0,2,0,0,0,124,184,2,128,5,184,2,128,6,184,130,128,123, 
  184,130,128,102,80,1,128,127,240,1,128,15,1,220,31,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,12,32,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,19,84, 
  0,0,0,232,0,0,0,1,0,1,21,1,59,0,0,0,170,238,1,0,6,0,0,0,2,0,0,0,104,144,65,129,5,208,1,128,6,208,1,128,103,80,65,128,123,208,1,128,124,208,1,128,4,17,1,91,32,0,0,1, 
  4,17,1,166,34,0,0,1,8,2,19,73,0,0,0,189,0,0,0,2,0,1,21,1,50,0,0,0,179,240,1,0,3,0,0,0,1,0,0,0,6,136,1,128,5,136,65,128,119,240,0,128,4,19,21,0,0,0,44,0, 
  0,0,1,0,17,1,241,36,0,0,1,8,2,19,61,0,0,0,152,0,0,0,2,0,1,21,1,122,0,0,0,0,241,1,0,6,0,0,0,2,0,0,0,16,80,1,129,5,200,131,128,6,200,3,128,73,240,1,128,76, 
  144,66,128,100,48,3,128,15,1,60,37,0,0,15,1,123,37,0,0,4,17,1,237,5,0,0,1,15,1,60,37,0,0,15,1,123,37,0,0,4,17,1,38,10,0,0,1,15,1,60,37,0,0,15,1,123,37,0,0,4, 
  17,1,113,10,0,0,1,15,1,60,37,0,0,15,1,135,37,0,0,17,1,248,10,0,0,1,8,2,19,61,0,0,0,162,0,0,0,2,0,1,19,61,0,0,0,158,0,0,0,2,0,1,19,61,0,0,0,154,0,0, 
  0,2,0,1,19,28,0,0,0,68,0,0,0,2,0,1,19,61,0,0,0,160,0,0,0,2,0,1,19,61,0,0,0,168,0,0,0,2,0,1,19,32,0,0,0,76,0,0,0,2,0,1,19,61,0,0,0,156,0,0, 
  0,2,0,1,21,1,58,2,0,0,50,229,1,0,23,0,0,0,4,0,0,0,16,112,3,128,65,240,10,128,34,16,196,131,35,176,196,131,36,80,197,131,5,200,145,130,6,200,209,129,39,224,198,131,72,112,13,128,73,16, 
  14,128,42,128,7,128,59,80,10,128,76,176,14,128,38,240,197,128,46,32,8,128,53,192,136,129,54,96,9,128,66,144,11,128,67,48,12,128,68,208,76,128,100,80,15,128,117,232,15,128,119,128,16,128,15,1,147,37,0,0, 
  15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,68,6,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,147,37,0,0, 
  15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,17,1,147,37,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,37, 
  7,0,0,1,15,1,147,37,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,18, 
  8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,17,1,147,37,0,0,1,15,1,147,37,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,147,37,0,0, 
  15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,147,37,0,0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,147,37,0,0, 
  15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,147,37,0,0,15,1,56,6,0,0,4,17,1,219,9,0,0,1,15,1,147,37,0,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,147,37,0,0, 
  15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,147,37,0,0,15,1,188,10,0,0,17,1,205,37,0,0,1,15,1,147,37,0,0,15,1,56,11,0,0,17,1,25,38,0,0,1,4,19,21,0,0,0,44,0, 
  0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,17,1,147,37,0,0,1,8,2,19,61,0,0,0,164,0,0,0,2,0,1,19,61,0,0,0,150,0,0,0,2,0,1,19, 
  61,0,0,0,166,0,0,0,2,0,1,19,73,0,0,0,188,0,0,0,2,0,1,19,73,0,0,0,190,0,0,0,2,0,1,21,1,47,0,0,0,64,241,1,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1, 
  128,6,112,1,128,95,48,65,128,123,112,1,128,12,17,1,83,38,0,0,1,10,12,17,1,141,38,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,46,0,0,0,22,0,0,0,5,17,1,143,38,0,0,1, 
  20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,51,0,0,0,119,0,0,0,1,0,1,21,1,83,0,0,0,21,242,1,0,4,0,0,0,2,0,0,0,127,208,1, 
  128,5,144,2,128,6,144,2,128,119,16,65,127,4,19,21,0,0,0,44,0,0,0,1,0,19,76,0,0,0,201,0,0,0,2,0,1,4,19,70,0,0,0,182,0,0,0,1,0,19,76,0,0,0,200,0,0,0,2,0, 
  1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,93,0,0,0,22,0,0,0,5,19,74,0,0,0,194,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0, 
  26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,34,0,0,0,22,0,0,0,15,1,162,24,0,0,17,1,210,38,0,0,1,2,19,34,0,0,0,81,0,0,0,1,0,1,21,4,47,0,0,0, 
  0,0,0,0,2,0,0,0,1,0,0,0,44,56,1,128,125,208,0,128,5,19,44,0,0,0,103,0,0,0,3,0,1,5,17,1,31,39,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26, 
  0,0,0,26,0,0,0,9,2,21,1,66,0,0,0,156,242,1,0,3,0,0,0,1,0,0,0,6,8,2,128,5,8,66,128,127,240,0,128,4,19,70,0,0,0,182,0,0,0,1,0,19,62,0,0,0,170,0,0,0, 
  1,0,19,48,0,0,0,112,0,0,0,2,0,1,8,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,12,17,1,219,39,0,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0, 
  2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,12,17,1,219,39,0,0,1,12,17,1,141,38,0,0,1,19,6,0,0,0,16,0,0,0,2,0,1,21,1,59,0,0,0,115,230, 
  1,0,8,0,0,0,3,0,0,0,64,144,1,128,45,144,1,128,123,208,1,128,75,144,193,127,124,208,1,128,5,208,1,127,6,208,1,128,63,144,1,128,12,17,1,219,40,0,0,1,10,12,17,1,141,38,0,0,1,21, 
  1,59,0,0,0,121,246,1,0,6,0,0,0,2,0,0,0,80,144,65,129,5,208,1,128,6,208,129,128,123,208,1,128,74,80,1,128,124,208,1,128,4,17,1,95,41,0,0,1,4,17,1,159,41,0,0,1,8,19,39, 
  0,0,0,95,0,0,0,2,0,1,21,0,55,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,14,240,128,128,65,80,1,128,82,176,1,128,19,82,0,0,0,218,0,0,0,1,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,1,1,2,21,1,162,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,224,6,128,105,96,9,130,98,144,4,130,83,16,2,130,124,8,21,128,5,8,213,128,6,8,149,126,127,64,20,128,117,224,11,128, 
  121,160,13,128,122,240,16,128,123,8,21,128,15,1,223,41,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,223,41,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43, 
  0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,223,41,0,0,15,1,30,42,0,0,15,1, 
  42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,223,41,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,223,41,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43, 
  0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,223,41,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1, 
  152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1, 
  0,17,1,147,44,0,0,1,15,1,223,41,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44, 
  0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,89,50,0,0, 
  4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,0,78,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,82,56,2,128,65,168,1,128,14,16,129,127,39,160,1,128,19,82,0,0,0, 
  218,0,0,0,1,0,17,1,37,29,0,0,1,1,19,82,0,0,0,219,0,0,0,1,0,17,1,37,29,0,0,1,17,1,109,51,0,0,1,2,21,1,47,0,0,0,111,247,1,0,5,0,0,0,2,0,0,0,28,48, 
  1,129,5,112,1,128,6,112,1,128,123,112,1,128,124,112,1,128,4,17,1,173,51,0,0,1,8,19,14,0,0,0,34,0,0,0,1,0,1,21,1,5,1,0,0,93,248,1,0,9,0,0,0,3,0,0,0,121,128,4, 
  128,105,128,195,127,98,176,66,129,83,176,65,129,124,32,8,128,5,32,8,128,6,32,8,128,122,80,6,128,123,32,8,128,15,1,229,51,0,0,15,1,26,52,0,0,15,1,62,52,0,0,15,1,222,44,0,0,4,17,1, 
  234,44,0,0,1,15,1,229,51,0,0,15,1,26,52,0,0,15,1,62,52,0,0,4,17,1,59,45,0,0,1,15,1,229,51,0,0,15,1,26,52,0,0,15,1,62,52,0,0,15,1,234,47,0,0,4,17,1,246,47, 
  0,0,1,15,1,229,51,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,25,0,0,0,57,0,0,0,1,0,17,1,26,52,0,0, 
  1,15,1,229,51,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,25,0,0,0,57,0,0,0,1,0,17,1,26,52,0,0,1,8, 
  2,21,1,43,0,0,0,62,230,1,0,4,0,0,0,2,0,0,0,124,80,1,128,5,80,1,128,6,16,1,128,123,80,1,128,4,17,1,74,52,0,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,31, 
  0,0,0,22,0,0,0,15,1,225,30,0,0,17,1,134,52,0,0,1,2,21,1,63,0,0,0,12,237,1,0,9,0,0,0,3,0,0,0,85,176,1,128,116,176,193,129,106,176,1,128,123,240,1,128,108,176,65,127,5, 
  240,193,126,6,240,1,128,127,176,1,128,124,240,1,128,12,17,1,189,52,0,0,1,10,12,17,1,141,38,0,0,1,21,1,47,0,0,0,242,248,1,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112, 
  129,128,123,112,1,128,114,48,1,128,4,17,1,81,53,0,0,1,8,2,21,1,47,0,0,0,242,248,1,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112,129,128,123,112,1,128,114,48,1,128,4,17, 
  1,244,55,0,0,1,8,2,21,1,47,0,0,0,242,248,1,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112,129,128,123,112,1,128,114,48,1,128,4,17,1,151,58,0,0,1,8,2,21,1,47,0, 
  0,0,242,248,1,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112,129,128,123,112,1,128,114,48,1,128,4,17,1,58,61,0,0,1,8,2,21,0,78,0,0,0,0,0,0,0,4,0,0,0,2,0, 
  0,0,82,56,2,128,65,168,1,128,14,24,129,127,11,16,1,128,1,19,82,0,0,0,218,0,0,0,1,0,17,1,12,32,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,12,32,0,0,1,17,1,220,31,0, 
  0,1,2,21,1,74,2,0,0,92,249,1,0,19,0,0,0,4,0,0,0,48,240,4,128,49,184,5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,72,146,127,6,72,18,128,55,152,72,130,40,240,3,128,57,248,9, 
  128,69,48,13,128,123,72,18,128,60,248,138,129,77,104,15,128,62,192,203,128,79,216,17,128,71,160,14,128,78,216,16,128,124,72,18,128,15,1,221,63,0,0,15,1,233,63,0,0,15,1,231,64,0,0,15,1,11,65,0, 
  0,4,17,1,23,65,0,0,1,15,1,221,63,0,0,15,1,87,65,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,221,63,0,0,4,19,2,0,0,0,6,0,0,0,1,0,17, 
  1,144,66,0,0,1,15,1,221,63,0,0,15,1,16,68,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,221,63,0,0,4,19,2,0,0,0, 
  7,0,0,0,1,0,17,1,14,69,0,0,1,15,1,221,63,0,0,4,19,2,0,0,0,8,0,0,0,1,0,17,1,142,70,0,0,1,15,1,221,63,0,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,38,72, 
  0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,221,63,0,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,15,1,221,63,0,0, 
  4,19,2,0,0,0,10,0,0,0,1,0,17,1,74,73,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,19,66,0,0,0,174,0, 
  0,0,3,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,19,66,0,0,0,174,0,0,0,3,0,1,15,1,221,63,0,0,4,19,2, 
  0,0,0,9,0,0,0,1,0,17,1,202,74,0,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,19,66,0,0,0,174,0,0,0,3, 
  0,1,15,1,221,63,0,0,15,1,74,76,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,131,77,0,0,17,1,25,78,0,0,1,8,2,21,1,74,2,0,0,92,249,1,0, 
  19,0,0,0,4,0,0,0,48,240,4,128,49,184,5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,72,146,127,6,72,18,128,55,152,72,130,40,240,3,128,57,248,9,128,69,48,13,128,123,72,18,128,60,248,138,129, 
  77,104,15,128,62,192,203,128,79,216,17,128,71,160,14,128,78,216,16,128,124,72,18,128,15,1,73,78,0,0,15,1,233,63,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,73,78, 
  0,0,15,1,87,65,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,73,78,0,0,4,19,2,0,0,0,6,0,0,0,1,0,17,1,144,66,0,0,1,15,1,73,78,0,0,15, 
  1,16,68,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,73,78,0,0,4,19,2,0,0,0,7,0,0,0,1,0,17,1,14,69,0,0,1, 
  15,1,73,78,0,0,4,19,2,0,0,0,8,0,0,0,1,0,17,1,142,70,0,0,1,15,1,73,78,0,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0, 
  0,4,17,1,140,72,0,0,1,15,1,73,78,0,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,15,1,73,78,0,0,4,19,2,0,0,0,10,0,0,0,1,0,17, 
  1,74,73,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,19,66,0,0,0,175,0,0,0,3,0,1,4,19,67,0,0,0,177,0, 
  0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,19,66,0,0,0,175,0,0,0,3,0,1,15,1,73,78,0,0,4,19,2,0,0,0,9,0,0,0,1,0,17,1,202,74, 
  0,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,19,66,0,0,0,175,0,0,0,3,0,1,15,1,73,78,0,0,15,1,74,76,0, 
  0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,85,78,0,0,17,1,25,78,0,0,1,8,2,21,4,47,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,44,208,64,128,62, 
  16,1,128,5,17,1,235,78,0,0,1,5,19,87,0,0,0,238,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0, 
  0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,19,0,0,0,41,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,9,0,0,0, 
  21,0,0,0,1,0,1,19,9,0,0,0,20,0,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,44,0,0,0,22,0,0,0,5,17,1,30,79,0,0,1,20,2,27,0,0,0,0,0,0,0,2, 
  0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,48,0,0,0,0,0,0,0,1,0,0,0,36,0,0,0,22,0,0,0,15,1,200,10,0,0,15,1,236,10,0,0,15,1,202,23,0,0,5,17,1, 
  15,24,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,91,0,0,0,22,0,0,0,5,17,1,68,11, 
  0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,43,0,0,0,22,0,0,0,5,17,1,89,81,0,0, 
  1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,14,1,21,1,66,0,0,0,156,242,1,0,3,0,0,0,1,0,0,0,6,8,2,128,5,8,66,128,127,240,0, 
  128,4,19,70,0,0,0,182,0,0,0,1,0,19,62,0,0,0,170,0,0,0,1,0,19,51,0,0,0,120,0,0,0,3,0,1,8,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,44,16,1,128, 
  93,208,0,128,12,17,1,141,38,0,0,1,12,17,1,163,83,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12,17,1,141,38,0,0,1,21,1,187,0,0, 
  0,177,250,1,0,6,0,0,0,2,0,0,0,16,80,65,129,5,208,197,128,6,208,5,128,127,80,4,128,73,80,2,128,76,80,3,128,15,1,221,83,0,0,15,1,28,84,0,0,15,1,64,84,0,0,15,1,76,84,0, 
  0,4,17,1,237,5,0,0,1,15,1,221,83,0,0,15,1,28,84,0,0,15,1,64,84,0,0,15,1,76,84,0,0,4,17,1,38,10,0,0,1,15,1,221,83,0,0,15,1,28,84,0,0,15,1,64,84,0,0,15, 
  1,76,84,0,0,4,17,1,113,10,0,0,1,15,1,221,83,0,0,15,1,28,84,0,0,15,1,64,84,0,0,4,19,70,0,0,0,182,0,0,0,1,0,19,62,0,0,0,170,0,0,0,1,0,17,1,88,84,0,0, 
  1,8,2,20,4,42,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,15,1,157,84,0,0,15,1,169,84,0,0,5,17,1,100,15,0,0,1,20,2,213,0,0,0,0,0,0,0,5,0,0,0,2, 
  0,0,0,38,0,0,0,73,0,0,0,108,0,0,0,143,0,0,0,178,0,0,0,5,19,47,0,0,0,109,0,0,0,1,0,19,15,0,0,0,35,0,0,0,1,0,19,80,0,0,0,214,0,0,0,2,0,1,5,19, 
  47,0,0,0,108,0,0,0,1,0,19,15,0,0,0,35,0,0,0,1,0,19,80,0,0,0,214,0,0,0,2,0,1,5,19,47,0,0,0,107,0,0,0,1,0,19,15,0,0,0,35,0,0,0,1,0,19,80,0,0, 
  0,214,0,0,0,2,0,1,5,19,47,0,0,0,110,0,0,0,1,0,19,15,0,0,0,35,0,0,0,1,0,19,80,0,0,0,214,0,0,0,2,0,1,5,19,47,0,0,0,111,0,0,0,1,0,19,15,0,0,0, 
  35,0,0,0,1,0,19,80,0,0,0,214,0,0,0,2,0,1,2,21,1,131,0,0,0,181,254,1,0,8,0,0,0,3,0,0,0,64,208,2,128,45,144,1,128,123,16,4,128,75,112,195,127,124,16,4,128,5,16,4, 
  127,6,16,4,128,63,48,2,128,15,1,181,84,0,0,15,1,204,16,0,0,4,17,1,216,16,0,0,1,15,1,181,84,0,0,15,1,49,17,0,0,4,17,1,61,17,0,0,1,15,1,181,84,0,0,15,1,109,17,0, 
  0,4,17,1,121,17,0,0,1,15,1,181,84,0,0,15,1,185,17,0,0,4,17,1,197,17,0,0,1,8,2,21,1,63,0,0,0,42,231,1,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6,240,1, 
  128,123,240,65,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,39,0,0,0,93,0,0,0,4,0,1,8,2,21,1,63,0,0,0,42,231,1,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1, 
  128,6,240,1,128,123,240,65,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,39,0,0,0,94,0,0,0,4,0,1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0, 
  0,5,19,82,0,0,0,220,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,12,0,0,0,30,0,0,0,1,0,1,21,0,213,0,0,0, 
  0,0,0,0,10,0,0,0,3,0,0,0,57,152,195,128,33,48,195,127,82,136,5,128,65,40,4,128,4,208,1,128,71,184,196,128,14,56,2,128,23,200,130,127,79,32,69,128,95,24,6,128,15,1,42,42,0,0,17,1, 
  193,84,0,0,1,19,82,0,0,0,218,0,0,0,1,0,17,1,42,42,0,0,1,15,1,42,42,0,0,17,1,248,84,0,0,1,15,1,42,42,0,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0, 
  17,1,42,42,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,42,42,0,0,1,15,1,42,42,0,0,17,1,116,85,0,0,1,15,1,42,42,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,42,42,0,0,1,19,4,0,0,0,13,0,0,0,1,0,17,1,42,42,0,0,1,2,19,4,0,0,0,13,0,0,0,1,0,1,21,1,53,0,0,0,179,0,2,0,5,0,0,0,2,0,0,0,124,160, 
  1,128,5,160,1,128,6,160,129,128,123,160,1,128,94,48,1,128,15,1,84,86,0,0,4,17,1,96,86,0,0,1,8,19,95,0,0,0,5,1,0,0,1,0,1,21,1,63,0,0,0,244,0,2,0,5,0,0,0,2, 
  0,0,0,100,48,1,129,5,240,1,128,6,240,1,128,123,240,1,128,124,240,1,128,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,8,19,23,0,0,0,49,0,0,0,1,0,1, 
  21,0,157,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,136,131,128,65,248,2,128,79,240,3,128,82,88,4,128,19,82,0,0,0,218,0,0,0,1,0,17,1,152, 
  43,0,0,1,15,1,152,43,0,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,152,43,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,152,43,0,0,1,15,1,152,43,0,0,17, 
  1,94,89,0,0,1,15,1,152,43,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,152,43,0,0,1,2,19,71,0,0,0,185,0,0,0,1,0,1,21,1,69,0,0,0,52,1,2,0, 
  5,0,0,0,2,0,0,0,84,112,1,128,5,32,194,128,6,32,66,128,82,48,1,128,125,224,1,128,4,17,1,233,89,0,0,1,15,1,46,90,0,0,4,17,1,109,90,0,0,1,4,17,1,149,90,0,0,1,8,19, 
  57,0,0,0,146,0,0,0,1,0,1,21,1,51,0,0,0,127,1,2,0,4,0,0,0,2,0,0,0,115,80,1,128,5,144,1,128,6,144,1,128,107,16,65,127,4,17,1,236,90,0,0,1,4,17,1,111,91,0,0, 
  1,8,19,79,0,0,0,213,0,0,0,1,0,1,19,33,0,0,0,79,0,0,0,1,0,1,19,36,0,0,0,84,0,0,0,1,0,1,21,1,80,0,0,0,180,1,2,0,4,0,0,0,2,0,0,0,102,16,1,128, 
  5,120,2,128,6,120,130,127,127,176,1,128,15,1,242,91,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,254,91,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21, 
  1,162,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,224,6,128,105,96,9,130,98,144,4,130,83,16,2,130,124,8,21,128,5,8,213,128,6,8,149,126,127,64,20,128,117,224,11,128,121,160,13,128,122,240,16, 
  128,123,8,21,128,15,1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15, 
  1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0, 
  0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0, 
  43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15, 
  1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0, 
  0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54, 
  44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0, 
  1,15,1,82,92,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0, 
  0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,145,92,0,0,4,19,70,0,0,0,182, 
  0,0,0,1,0,17,1,116,29,0,0,1,8,2,19,33,0,0,0,78,0,0,0,1,0,1,19,36,0,0,0,85,0,0,0,1,0,1,21,1,163,0,0,0,22,2,2,0,10,0,0,0,3,0,0,0,24,168,4,128, 
  17,208,1,128,18,56,2,128,19,160,2,128,20,8,3,128,5,16,197,128,6,16,197,128,23,64,4,128,21,112,3,128,22,216,3,128,4,19,26,0,0,0,58,0,0,0,2,0,1,4,19,26,0,0,0,59,0,0,0,2, 
  0,1,4,19,26,0,0,0,61,0,0,0,2,0,1,4,19,26,0,0,0,65,0,0,0,2,0,1,4,19,26,0,0,0,64,0,0,0,2,0,1,4,19,26,0,0,0,60,0,0,0,2,0,1,4,19,26,0,0,0, 
  63,0,0,0,2,0,1,4,19,26,0,0,0,62,0,0,0,2,0,1,8,2,19,71,0,0,0,186,0,0,0,1,0,1,21,1,178,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,224,4,128,105,112,198,129, 
  98,128,195,129,83,240,193,129,124,136,13,128,5,136,13,128,6,136,141,126,127,192,12,128,121,0,8,128,122,96,10,128,123,136,13,128,15,1,119,93,0,0,15,1,167,93,0,0,15,1,69,94,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,119,93,0,0,15,1,167,93,0,0,15,1,69,94,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44, 
  0,0,4,17,1,59,45,0,0,1,15,1,119,93,0,0,15,1,167,93,0,0,15,1,69,94,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1, 
  15,1,119,93,0,0,15,1,167,93,0,0,15,1,69,94,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,119,93,0,0,15,1,167,93, 
  0,0,15,1,69,94,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,119,93,0,0,15,1,167,93,0,0,15,1,69,94,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,81,94,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,224,0,0,0,0,0,0,0,11, 
  0,0,0,3,0,0,0,12,88,2,128,33,136,131,128,82,224,5,128,57,240,131,128,4,240,1,127,65,128,4,128,14,144,2,128,23,32,67,128,71,16,69,128,79,120,69,128,95,112,6,128,15,1,89,50,0,0,17,1,244, 
  94,0,0,1,17,1,223,41,0,0,1,19,82,0,0,0,218,0,0,0,1,0,17,1,89,50,0,0,1,15,1,89,50,0,0,17,1,248,84,0,0,1,15,1,89,50,0,0,17,1,52,85,0,0,1,19,71,0,0,0, 
  185,0,0,0,1,0,17,1,89,50,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,89,50,0,0,1,15,1,89,50,0,0,17,1,48,95,0,0,1,15,1,89,50,0,0,17,1,16,86,0,0,1,19,33,0, 
  0,0,78,0,0,0,1,0,17,1,89,50,0,0,1,19,4,0,0,0,13,0,0,0,1,0,17,1,89,50,0,0,1,2,21,1,39,0,0,0,37,3,2,0,3,0,0,0,1,0,0,0,6,48,129,128,5,48,1,128, 
  28,240,0,128,4,17,1,173,51,0,0,1,8,19,14,0,0,0,34,0,0,0,1,0,1,21,1,51,0,0,0,90,3,2,0,6,0,0,0,2,0,0,0,80,80,65,129,5,144,1,128,6,144,129,128,123,144,1,128,74, 
  80,1,128,124,144,1,128,12,17,1,204,95,0,0,1,10,12,19,39,0,0,0,95,0,0,0,2,0,1,21,1,55,0,0,0,156,242,1,0,3,0,0,0,1,0,0,0,6,176,1,128,5,176,65,128,127,240,0,128,4, 
  19,70,0,0,0,182,0,0,0,1,0,19,65,0,0,0,173,0,0,0,3,0,1,8,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124, 
  152,1,128,4,19,64,0,0,0,172,0,0,0,4,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,25,0,0,0,22,0,0,0,15,1,26,52,0,0,17,1,8,96,0,0,1,2,19,25,0,0,0,57, 
  0,0,0,1,0,1,21,1,59,0,0,0,121,246,1,0,6,0,0,0,2,0,0,0,80,144,65,129,5,208,1,128,6,208,129,128,123,208,1,128,74,80,1,128,124,208,1,128,4,17,1,79,96,0,0,1,4,17,1,143, 
  96,0,0,1,8,2,21,1,47,0,0,0,77,238,1,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,2,48,1,128,3,48,65,128,123,112,1,128,12,17,1,207,96,0,0,1,10,12,17,1,141,38,0,0, 
  1,21,1,147,0,0,0,12,237,1,0,9,0,0,0,3,0,0,0,85,176,1,128,116,0,195,129,106,32,2,128,123,144,4,128,108,144,66,127,5,144,196,126,6,144,4,128,127,112,3,128,124,144,4,128,15,1,21,97,0, 
  0,4,17,1,95,18,0,0,1,15,1,33,97,0,0,4,17,1,177,18,0,0,1,15,1,45,97,0,0,4,17,1,3,19,0,0,1,15,1,57,97,0,0,4,17,1,85,19,0,0,1,15,1,69,97,0,0,4,19,70, 
  0,0,0,182,0,0,0,1,0,19,14,0,0,0,34,0,0,0,1,0,17,1,186,19,0,0,1,8,2,21,1,162,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,224,6,128,105,96,9,130,98,144,4,130,83, 
  16,2,130,124,8,21,128,5,8,213,128,6,8,149,126,127,64,20,128,117,224,11,128,121,160,13,128,122,240,16,128,123,8,21,128,15,1,81,97,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15, 
  1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,81,97,0, 
  0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,15,1,81,97,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,81,97,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15, 
  1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,81,97,0,0,15,1,30,42,0, 
  0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,81,97,0,0,15,1,30,42,0,0,15,1,42, 
  42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0, 
  1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,81,97,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0, 
  0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,93,97,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,162,2,0,0,198,246,1,0,12,0,0,0,3,0, 
  0,0,102,224,6,128,105,96,9,130,98,144,4,130,83,16,2,130,124,8,21,128,5,8,213,128,6,8,149,126,127,64,20,128,117,224,11,128,121,160,13,128,122,240,16,128,123,8,21,128,15,1,72,98,0,0,15,1,30,42, 
  0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,72,98,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,72,98,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0, 
  15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,72,98,0,0,15,1,30,42,0,0,15,1,42,42, 
  0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246, 
  47,0,0,1,15,1,72,98,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1, 
  15,1,72,98,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0, 
  11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,72,98,0,0,15,1,30,42,0,0,15,1, 
  42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0, 
  0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,84,98,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21, 
  1,162,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,224,6,128,105,96,9,130,98,144,4,130,83,16,2,130,124,8,21,128,5,8,213,128,6,8,149,126,127,64,20,128,117,224,11,128,121,160,13,128,122,240,16, 
  128,123,8,21,128,15,1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15, 
  1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0, 
  0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0, 
  43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15, 
  1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0, 
  0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54, 
  44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0, 
  1,15,1,63,99,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0, 
  0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,75,99,0,0,4,19,70,0,0,0,182, 
  0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,162,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,224,6,128,105,96,9,130,98,144,4,130,83,16,2,130,124,8,21,128,5,8,213,128,6,8,149,126, 
  127,64,20,128,117,224,11,128,121,160,13,128,122,240,16,128,123,8,21,128,15,1,54,100,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43, 
  0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,54,100,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1, 
  0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,54,100,0,0, 
  15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0, 
  4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,54,100,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,54,100,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1, 
  12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,54,100,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0, 
  15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33, 
  0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,54,100,0,0,15,1,30,42,0,0,15,1,42,42,0,0,15,1,0,43,0,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1, 
  54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,66,100,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,19,66,0,0,0,174,0,0,0,3,0,1,21,1,242,0,0,0,162,6,2,0,10,0,0,0,3,0,0,0,62, 
  208,3,128,57,0,3,128,69,232,132,129,123,136,7,128,124,136,7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,45,101,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92, 
  72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,45,101,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0, 
  204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,19,67,0,0,0,178, 
  0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,15,1,57,101,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,27,0,0,0,1,0,1,20,0,35,0,0, 
  0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,15,1,231,64,0,0,17,1,185,101,0,0,1,2,19,41,0,0,0,98,0,0,0,1,0,1,21,1,63,0,0,0,19,7,2,0,5,0,0,0,2,0,0, 
  0,124,240,1,128,5,240,1,128,6,240,1,128,119,48,65,128,123,240,1,128,4,19,21,0,0,0,44,0,0,0,1,0,19,94,0,0,0,1,1,0,0,2,0,1,8,2,21,1,242,0,0,0,162,6,2,0,10,0,0, 
  0,3,0,0,0,62,208,3,128,57,0,3,128,69,232,132,129,123,136,7,128,124,136,7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,45,101,0,0,15,1,26,72,0,0,15,1,38, 
  72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,45,101,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1, 
  0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4, 
  19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,15,1,252,101,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,27,0,0,0,1,0, 
  1,21,1,58,0,0,0,19,7,2,0,5,0,0,0,2,0,0,0,124,200,1,128,5,200,1,128,6,200,1,128,119,48,65,128,123,200,1,128,4,19,21,0,0,0,44,0,0,0,1,0,17,1,124,102,0,0,1,8,2, 
  21,1,116,1,0,0,108,7,2,0,14,0,0,0,3,0,0,0,40,32,3,128,49,240,195,129,37,80,2,130,123,152,11,128,124,152,11,128,5,152,75,127,6,152,203,128,55,16,133,129,57,64,6,128,62,16,199,128,69,40, 
  72,128,77,64,9,128,78,88,10,128,79,40,11,128,15,1,195,102,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,193,103,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17, 
  1,85,66,0,0,1,15,1,191,104,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,38,72,0,0, 
  15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77, 
  0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19,67,0, 
  0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,15,1,201,105,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1, 
  199,106,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,28,0,0,0,1,0,1,21,1,242,0,0,0,162,6,2,0,10,0,0,0,3,0,0,0,62,208,3,128,57,0,3,128,69,232,132,129,123,136,7,128,124,136, 
  7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,45,101,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1, 
  15,1,45,101,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0, 
  1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19, 
  10,0,0,0,23,0,0,0,2,0,1,4,15,1,71,107,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,27,0,0,0,1,0,1,21,1,116,1,0,0,108,7,2,0,14,0,0,0,3,0,0,0,40,32,3,128, 
  49,240,195,129,37,80,2,130,123,152,11,128,124,152,11,128,5,152,75,127,6,152,203,128,55,16,133,129,57,64,6,128,62,16,199,128,69,40,72,128,77,64,9,128,78,88,10,128,79,40,11,128,15,1,195,102,0,0,15,1, 
  231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,193,103,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,191,104,0,0,4,19,94,0,0,0,3,1,0,0, 
  1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1, 
  189,105,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4, 
  19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0, 
  0,0,24,0,0,0,2,0,1,15,1,201,105,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,199,107,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,28,0,0,0,1, 
  0,1,21,1,116,1,0,0,108,7,2,0,14,0,0,0,3,0,0,0,40,32,3,128,49,240,195,129,37,80,2,130,123,152,11,128,124,152,11,128,5,152,75,127,6,152,203,128,55,16,133,129,57,64,6,128,62,16,199,128, 
  69,40,72,128,77,64,9,128,78,88,10,128,79,40,11,128,15,1,195,102,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,193,103,0,0,15,1,231,64,0,0,15,1,11,65,0,0, 
  4,17,1,85,66,0,0,1,15,1,191,104,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,38,72, 
  0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0, 
  19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19, 
  67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,15,1,201,105,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4, 
  15,1,71,108,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,28,0,0,0,1,0,1,19,10,0,0,0,25,0,0,0,1,0,1,19,77,0,0,0,203,0,0,0,1,0,1,21,1,53,0,0,0,245,7,2,0, 
  5,0,0,0,2,0,0,0,124,160,1,128,5,160,193,128,6,160,1,128,123,160,1,128,57,48,1,128,15,1,199,108,0,0,4,17,1,241,72,0,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,45,0, 
  0,0,22,0,0,0,15,1,92,72,0,0,17,1,211,108,0,0,1,2,19,45,0,0,0,105,0,0,0,1,0,1,21,1,88,0,0,0,159,237,1,0,6,0,0,0,2,0,0,0,124,184,2,128,5,184,2,128,6,184, 
  130,128,123,184,130,128,102,80,1,128,127,240,1,128,15,1,10,109,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,22,109,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8, 
  2,19,92,0,0,0,251,0,0,0,1,0,1,21,1,88,0,0,0,159,237,1,0,6,0,0,0,2,0,0,0,124,184,2,128,5,184,2,128,6,184,130,128,123,184,130,128,102,80,1,128,127,240,1,128,15,1,106,109,0, 
  0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,118,109,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,116,1,0,0,108,7,2,0,14,0,0,0,3,0,0,0, 
  40,32,3,128,49,240,195,129,37,80,2,130,123,152,11,128,124,152,11,128,5,152,75,127,6,152,203,128,55,16,133,129,57,64,6,128,62,16,199,128,69,40,72,128,77,64,9,128,78,88,10,128,79,40,11,128,15,1,195,102, 
  0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,193,103,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,191,104,0,0,4,19,94,0,0,0, 
  3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0, 
  0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0, 
  2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1, 
  0,19,10,0,0,0,24,0,0,0,2,0,1,15,1,201,105,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,202,109,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,28, 
  0,0,0,1,0,1,21,1,116,1,0,0,108,7,2,0,14,0,0,0,3,0,0,0,40,32,3,128,49,240,195,129,37,80,2,130,123,152,11,128,124,152,11,128,5,152,75,127,6,152,203,128,55,16,133,129,57,64,6,128, 
  62,16,199,128,69,40,72,128,77,64,9,128,78,88,10,128,79,40,11,128,15,1,195,102,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,193,103,0,0,15,1,231,64,0,0,15,1, 
  11,65,0,0,4,17,1,85,66,0,0,1,15,1,191,104,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0, 
  15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,189,105,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0, 
  0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2, 
  0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,24,0,0,0,2,0,1,15,1,201,105,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77, 
  0,0,1,4,15,1,74,110,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,28,0,0,0,1,0,1,21,1,242,0,0,0,162,6,2,0,10,0,0,0,3,0,0,0,62,208,3,128,57,0,3,128,69,232,132,129, 
  123,136,7,128,124,136,7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,45,101,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17, 
  1,140,72,0,0,1,15,1,45,101,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0, 
  23,0,0,0,2,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204, 
  0,0,0,1,0,19,10,0,0,0,23,0,0,0,2,0,1,4,15,1,202,110,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,27,0,0,0,1,0,1,21,1,58,0,0,0,42,231,1,0,5,0,0,0,2,0, 
  0,0,124,200,1,128,5,200,1,128,6,200,1,128,123,200,65,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,17,1,74,111,0,0,1,8,2,21,0,149,0,0,0,0,0,0,0,7,0,0,0,2,0,0, 
  0,17,96,194,128,13,208,193,127,10,112,129,128,29,240,194,128,30,128,67,128,66,16,4,128,77,24,4,128,19,66,0,0,0,174,0,0,0,3,0,1,19,77,0,0,0,202,0,0,0,1,0,17,1,131,77,0,0,1,19, 
  13,0,0,0,32,0,0,0,1,0,17,1,131,77,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,131,77,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,131,77,0,0,1,1,19,10,0,0,0,25, 
  0,0,0,1,0,17,1,131,77,0,0,1,2,21,1,47,0,0,0,54,8,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112,1,128,91,48,65,128,123,112,1,128,4,17,1,138,111,0,0,1,8, 
  2,19,66,0,0,0,175,0,0,0,3,0,1,21,0,149,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,17,96,194,128,13,208,193,127,10,112,129,128,29,240,194,128,30,128,67,128,66,16,4,128,77,24,4,128,19, 
  66,0,0,0,175,0,0,0,3,0,1,19,77,0,0,0,202,0,0,0,1,0,17,1,85,78,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,85,78,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1, 
  85,78,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,85,78,0,0,1,1,19,10,0,0,0,25,0,0,0,1,0,17,1,85,78,0,0,1,2,21,1,50,0,0,0,179,240,1,0,3,0,0,0,1,0,0, 
  0,6,136,1,128,5,136,65,128,119,240,0,128,4,19,21,0,0,0,44,0,0,0,1,0,17,1,221,111,0,0,1,8,2,21,1,58,2,0,0,50,229,1,0,23,0,0,0,4,0,0,0,16,112,3,128,65,240,10,128, 
  34,16,196,131,35,176,196,131,36,80,197,131,5,200,145,130,6,200,209,129,39,224,198,131,72,112,13,128,73,16,14,128,42,128,7,128,59,80,10,128,76,176,14,128,38,240,197,128,46,32,8,128,53,192,136,129,54,96,9,128, 
  66,144,11,128,67,48,12,128,68,208,76,128,100,80,15,128,117,232,15,128,119,128,16,128,15,1,28,112,0,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,68, 
  6,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0, 
  0,9,1,0,0,1,0,17,1,28,112,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,28,112,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,28,112,0,0, 
  15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,17,1,28, 
  112,0,0,1,15,1,28,112,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,28,112,0,0,15,1,255,8,0,0,4,17,1,11, 
  9,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,28,112,0,0,15,1,56,6,0,0,4,17,1,219, 
  9,0,0,1,15,1,28,112,0,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,28,112,0,0,15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,28,112,0,0,15,1,188,10,0,0,17,1,205,37, 
  0,0,1,15,1,28,112,0,0,15,1,56,11,0,0,17,1,25,38,0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,17,1,28,112, 
  0,0,1,8,2,21,1,73,2,0,0,50,229,1,0,23,0,0,0,4,0,0,0,16,112,3,128,65,64,11,128,34,16,196,131,35,176,196,131,36,80,197,131,5,64,146,130,6,64,210,129,39,8,199,131,72,192,13,128,73, 
  96,14,128,42,168,7,128,59,160,10,128,76,0,15,128,38,240,197,128,46,72,8,128,53,232,136,129,54,136,9,128,66,224,11,128,67,128,12,128,68,32,77,128,100,160,15,128,117,56,16,128,119,208,16,128,15,1,91,112,0, 
  0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1,68,6,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,91,112,0, 
  0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,20,0,0,0,42,0,0,0,3,0,1,15,1,91,112,0,0,15,1,56,6, 
  0,0,4,17,1,37,7,0,0,1,15,1,91,112,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,91,112,0,0,15,1,56,6, 
  0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,20,0,0,0,42,0,0,0,3,0,1,15,1,91,112,0,0,15,1,93,8,0,0,4,17,1, 
  105,8,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,91,112,0,0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1, 
  69,9,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,91,112,0,0,15,1,56,6,0,0,4,17,1,219,9,0,0,1,15,1,91,112,0,0,15,1,225,5,0,0,4,17,1, 
  38,10,0,0,1,15,1,91,112,0,0,15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,91,112,0,0,15,1,188,10,0,0,17,1,205,37,0,0,1,15,1,91,112,0,0,15,1,56,11,0,0,17,1,25,38, 
  0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,20,0,0,0,42,0,0,0,3,0,1,8,2,20,4,30,0,0,0,0,0,0, 
  0,1,0,0,0,44,0,0,0,22,0,0,0,5,17,1,103,112,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1, 
  0,0,0,125,0,0,0,22,0,0,0,5,19,44,0,0,0,102,0,0,0,5,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0, 
  0,0,1,0,0,0,90,0,0,0,22,0,0,0,15,1,28,84,0,0,17,1,177,114,0,0,1,2,19,90,0,0,0,243,0,0,0,1,0,1,19,1,0,0,0,5,0,0,0,1,0,1,20,4,30,0,0,0,0,0, 
  0,0,1,0,0,0,58,0,0,0,22,0,0,0,5,17,1,254,114,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,1,0,0,0,4,0,0,0,1,0, 
  1,19,80,0,0,0,214,0,0,0,2,0,1,19,15,0,0,0,36,0,0,0,1,0,1,19,5,0,0,0,14,0,0,0,2,0,1,21,1,47,0,0,0,17,14,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5, 
  112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17,1,90,117,0,0,1,10,12,17,1,141,38,0,0,1,21,1,47,0,0,0,186,14,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112, 
  129,128,123,112,1,128,94,48,1,128,12,17,1,138,117,0,0,1,10,12,19,95,0,0,0,5,1,0,0,1,0,1,21,1,51,0,0,0,88,15,2,0,6,0,0,0,2,0,0,0,124,144,1,128,5,144,1,128,6,144, 
  1,128,107,80,65,128,115,80,65,128,123,144,1,128,12,17,1,192,117,0,0,1,10,12,19,79,0,0,0,213,0,0,0,1,0,1,21,1,143,0,0,0,106,16,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48, 
  3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112,132,126,127,48,4,128,117,112,3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17,1,244,117,0,0,1,12,17,1,141,119,0, 
  0,1,12,17,1,38,121,0,0,1,12,17,1,191,122,0,0,1,12,17,1,88,124,0,0,1,12,17,1,241,125,0,0,1,12,17,1,138,127,0,0,1,12,17,1,35,129,0,0,1,6,17,1,188,130,0,0,1,10,12, 
  19,23,0,0,0,49,0,0,0,1,0,1,21,1,55,0,0,0,100,17,2,0,7,0,0,0,2,0,0,0,84,112,65,129,5,176,65,129,6,176,129,128,123,176,1,128,82,112,1,128,124,176,1,128,125,112,1,128,12,17, 
  1,152,131,0,0,1,10,12,19,57,0,0,0,146,0,0,0,1,0,1,19,95,0,0,0,4,1,0,0,2,0,1,21,1,253,2,0,0,78,18,2,0,26,0,0,0,4,0,0,0,16,208,3,128,65,0,14,128,34,160, 
  196,131,35,112,197,131,36,64,198,131,5,224,151,130,6,224,215,129,39,128,8,132,72,64,17,128,73,16,18,128,42,80,9,128,59,48,77,131,76,224,82,131,38,16,199,128,46,32,10,128,53,240,202,129,54,192,11,128,66,208, 
  14,128,67,160,15,128,68,112,80,128,84,176,83,128,100,32,20,128,117,72,21,128,119,24,22,128,123,224,23,128,124,224,23,128,15,1,222,131,0,0,15,1,234,131,0,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1, 
  15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,68,6,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,222,131,0,0,15,1, 
  234,131,0,0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,37,0,0,0,88,0,0,0,1,0,19,63,0,0,0,171,0,0, 
  0,2,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,222,131, 
  0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0, 
  19,96,0,0,0,9,1,0,0,1,0,19,37,0,0,0,88,0,0,0,1,0,19,63,0,0,0,171,0,0,0,2,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1, 
  15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,222,131,0,0,15,1, 
  234,131,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,56,6, 
  0,0,4,17,1,219,9,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,225,5,0,0,4,17,1,113,10,0, 
  0,1,4,15,1,246,131,0,0,17,1,52,132,0,0,1,15,1,222,131,0,0,15,1,234,131,0,0,15,1,188,10,0,0,15,1,200,10,0,0,15,1,236,10,0,0,17,1,248,10,0,0,1,15,1,222,131,0,0,15, 
  1,234,131,0,0,15,1,56,11,0,0,4,17,1,68,11,0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,37,0,0,0,88,0, 
  0,0,1,0,19,63,0,0,0,171,0,0,0,2,0,1,8,2,21,1,131,0,0,0,106,16,2,0,12,0,0,0,3,0,0,0,102,144,2,128,105,208,2,130,98,80,2,130,83,16,2,130,124,16,4,128,5,16,196,128, 
  6,16,132,126,127,208,3,128,117,16,3,128,121,80,3,128,122,144,3,128,123,16,4,128,12,17,1,170,135,0,0,1,12,17,1,39,137,0,0,1,12,17,1,164,138,0,0,1,12,17,1,33,140,0,0,1,12,17,1,158, 
  141,0,0,1,12,17,1,27,143,0,0,1,12,17,1,152,144,0,0,1,6,17,1,21,146,0,0,1,10,12,17,1,141,38,0,0,1,21,1,57,0,0,0,28,19,2,0,4,0,0,0,2,0,0,0,84,16,1,128,5, 
  192,129,128,6,192,1,128,125,128,1,128,15,1,200,146,0,0,4,17,1,109,90,0,0,1,4,17,1,0,147,0,0,1,8,19,57,0,0,0,136,0,0,0,2,0,1,21,1,51,0,0,0,92,19,2,0,4,0,0,0, 
  2,0,0,0,82,16,1,128,5,144,129,128,6,144,129,127,125,80,1,128,4,17,1,75,147,0,0,1,4,17,1,131,147,0,0,1,8,19,57,0,0,0,134,0,0,0,2,0,1,21,1,39,0,0,0,156,19,2,0,3, 
  0,0,0,1,0,0,0,6,48,129,128,5,48,1,128,118,240,0,128,4,17,1,205,147,0,0,1,8,2,21,4,48,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,123,16,1,128,63,208,192,127,5,17,1,12,148, 
  0,0,1,15,1,87,148,0,0,5,17,1,109,90,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,57,0,0,0,137,0,0,0,2,0,1,21,1,130,0, 
  0,0,18,20,2,0,6,0,0,0,2,0,0,0,105,184,1,129,5,8,196,127,6,8,196,128,27,80,1,128,121,40,2,128,122,24,3,128,4,19,79,0,0,0,212,0,0,0,3,0,1,15,1,161,148,0,0,4,17,1, 
  246,47,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,17,1,224,148,0,0,1,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,17,1, 
  224,148,0,0,1,8,2,21,1,130,0,0,0,18,20,2,0,6,0,0,0,2,0,0,0,105,184,1,129,5,8,196,127,6,8,196,128,27,80,1,128,121,40,2,128,122,24,3,128,4,19,79,0,0,0,209,0,0,0,3, 
  0,1,15,1,31,149,0,0,4,17,1,246,47,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,17,1,94,149,0,0,1,4,19,43,0,0,0,101,0,0,0,1,0,19,27, 
  0,0,0,66,0,0,0,1,0,17,1,94,149,0,0,1,8,2,19,7,0,0,0,18,0,0,0,2,0,1,21,0,83,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,82,56,2,128,65,168,1,128,14,24,129,127, 
  7,16,1,128,1,19,82,0,0,0,218,0,0,0,1,0,17,1,254,91,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,254,91,0,0,1,19,7,0,0,0,18,0,0,0,2,0,1,2,20,4,35,0,0,0, 
  0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,36,0,0,0,86,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,229, 
  0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,12,120,194,128,33,168,3,129,82,8,6,128,36,16,4,128,4,16,2,127,57,24,196,128,14,176,2,128,23,64,131,128,65,168,4,128,71,56,69,128,79,160,69,128,95, 
  152,6,128,15,1,145,92,0,0,17,1,157,149,0,0,1,17,1,82,92,0,0,1,19,82,0,0,0,218,0,0,0,1,0,17,1,145,92,0,0,1,15,1,145,92,0,0,17,1,248,84,0,0,1,15,1,145,92,0,0, 
  17,1,52,85,0,0,1,1,19,71,0,0,0,185,0,0,0,1,0,17,1,145,92,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,145,92,0,0,1,15,1,145,92,0,0,17,1,217,149,0,0,1,15,1,145, 
  92,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,145,92,0,0,1,19,4,0,0,0,13,0,0,0,1,0,17,1,145,92,0,0,1,2,21,1,47,0,0,0,57,21,2,0,5,0,0, 
  0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,29,48,1,128,4,17,1,117,150,0,0,1,8,2,21,0,157,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,104,194, 
  128,14,112,129,127,79,240,3,128,57,208,66,128,65,96,3,128,82,88,4,128,19,82,0,0,0,218,0,0,0,1,0,17,1,167,93,0,0,1,15,1,167,93,0,0,17,1,181,150,0,0,1,15,1,167,93,0,0,17,1, 
  52,85,0,0,1,19,18,0,0,0,40,0,0,0,1,0,17,1,167,93,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,167,93,0,0,1,15,1,167,93,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78, 
  0,0,0,1,0,17,1,167,93,0,0,1,2,19,18,0,0,0,40,0,0,0,1,0,1,21,0,162,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,240,194,128,33,136,194,127,18,32,194,128,65,128,3,128,68, 
  16,4,128,82,128,4,128,14,144,1,128,79,24,4,128,19,82,0,0,0,218,0,0,0,1,0,17,1,81,94,0,0,1,15,1,81,94,0,0,17,1,52,151,0,0,1,15,1,81,94,0,0,17,1,52,85,0,0,1,19, 
  18,0,0,0,40,0,0,0,1,0,17,1,81,94,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,81,94,0,0,1,1,15,1,81,94,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0, 
  17,1,81,94,0,0,1,2,21,1,47,0,0,0,121,21,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17,1,177,152,0,0,1,10,12,19,12,0,0, 
  0,30,0,0,0,1,0,1,21,1,143,0,0,0,196,21,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48,3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112,132,126,127,48,4,128,117,112, 
  3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17,1,225,152,0,0,1,12,17,1,122,154,0,0,1,12,17,1,19,156,0,0,1,12,17,1,172,157,0,0,1,12,17,1,69,159,0,0,1,12,17, 
  1,222,160,0,0,1,12,17,1,119,162,0,0,1,12,17,1,16,164,0,0,1,12,17,1,169,165,0,0,1,10,12,19,23,0,0,0,49,0,0,0,1,0,1,21,1,59,0,0,0,121,246,1,0,6,0,0,0,2,0, 
  0,0,80,144,65,129,5,208,1,128,6,208,129,128,123,208,1,128,74,80,1,128,124,208,1,128,4,17,1,95,41,0,0,1,4,17,1,159,41,0,0,1,8,2,21,1,63,0,0,0,17,24,2,0,9,0,0,0,3,0, 
  0,0,121,176,1,128,105,176,193,127,98,176,65,129,83,176,65,129,124,240,1,128,5,240,1,128,6,240,1,128,122,176,1,128,123,240,1,128,12,17,1,66,167,0,0,1,10,12,17,1,141,38,0,0,1,21,1,63,0,0, 
  0,42,231,1,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6,240,1,128,123,240,65,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,78,0,0,0,205,0,0,0,5,0,1,8,2,21, 
  1,63,0,0,0,42,231,1,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6,240,1,128,123,240,65,128,127,48,1,128,4,19,70,0,0,0,182,0,0,0,1,0,19,78,0,0,0,206,0,0,0,5,0, 
  1,8,2,21,1,69,0,0,0,77,238,1,0,6,0,0,0,2,0,0,0,124,32,2,128,5,32,2,128,2,80,129,128,3,184,129,128,6,32,2,128,123,32,2,128,4,19,31,0,0,0,73,0,0,0,2,0,1,4,19, 
  31,0,0,0,72,0,0,0,2,0,1,8,2,19,84,0,0,0,225,0,0,0,2,0,1,19,84,0,0,0,224,0,0,0,2,0,1,19,84,0,0,0,223,0,0,0,2,0,1,19,84,0,0,0,226,0,0,0,2,0, 
  1,19,84,0,0,0,227,0,0,0,2,0,1,19,59,0,0,0,148,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,12,120,2,128,33,208,3,129,82,48,6,128,59,200,4,128,4, 
  16,2,127,57,56,196,128,14,216,2,128,23,104,131,128,65,208,4,128,71,96,69,128,79,200,69,128,95,192,6,128,15,1,93,97,0,0,17,1,12,168,0,0,1,19,59,0,0,0,148,0,0,0,4,0,1,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,93,97,0,0,1,15,1,93,97,0,0,17,1,248,84,0,0,1,15,1,93,97,0,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,93,97,0,0,1,1,19, 
  82,0,0,0,219,0,0,0,1,0,17,1,93,97,0,0,1,15,1,93,97,0,0,17,1,72,168,0,0,1,15,1,93,97,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,93,97,0,0, 
  1,19,4,0,0,0,13,0,0,0,1,0,17,1,93,97,0,0,1,2,19,40,0,0,0,96,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,40,56,4,128,33,208,3,129,82,48, 
  6,128,12,120,2,128,4,16,194,127,57,64,196,128,14,216,2,128,23,104,131,128,65,208,4,128,71,96,69,128,79,200,69,128,95,192,6,128,15,1,84,98,0,0,17,1,228,168,0,0,1,19,40,0,0,0,96,0,0,0, 
  4,0,1,19,82,0,0,0,218,0,0,0,1,0,17,1,84,98,0,0,1,15,1,84,98,0,0,17,1,248,84,0,0,1,15,1,84,98,0,0,17,1,52,85,0,0,1,1,19,71,0,0,0,185,0,0,0,1,0,17, 
  1,84,98,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,84,98,0,0,1,15,1,84,98,0,0,17,1,32,169,0,0,1,15,1,84,98,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1, 
  0,17,1,84,98,0,0,1,19,4,0,0,0,13,0,0,0,1,0,17,1,84,98,0,0,1,2,19,54,0,0,0,128,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,12,120,2, 
  128,33,208,3,129,82,48,6,128,54,56,4,128,4,16,2,127,57,64,196,128,14,216,66,127,23,104,131,128,65,208,4,128,71,96,69,128,79,200,69,128,95,192,6,128,15,1,75,99,0,0,17,1,188,169,0,0,1,19,54, 
  0,0,0,128,0,0,0,4,0,1,19,82,0,0,0,218,0,0,0,1,0,17,1,75,99,0,0,1,15,1,75,99,0,0,17,1,248,84,0,0,1,15,1,75,99,0,0,17,1,52,85,0,0,1,1,19,71,0,0,0, 
  185,0,0,0,1,0,17,1,75,99,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,75,99,0,0,1,15,1,75,99,0,0,17,1,248,169,0,0,1,15,1,75,99,0,0,17,1,16,86,0,0,1,19,33,0, 
  0,0,78,0,0,0,1,0,17,1,75,99,0,0,1,19,4,0,0,0,13,0,0,0,1,0,17,1,75,99,0,0,1,2,19,11,0,0,0,29,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0, 
  3,0,0,0,12,128,2,128,33,216,3,129,82,48,6,128,11,120,2,128,4,16,2,127,57,64,196,128,14,224,2,128,23,112,131,128,65,208,4,128,71,96,69,128,79,200,69,128,95,192,6,128,15,1,66,100,0,0,17,1, 
  148,170,0,0,1,1,19,11,0,0,0,29,0,0,0,4,0,1,19,82,0,0,0,218,0,0,0,1,0,17,1,66,100,0,0,1,15,1,66,100,0,0,17,1,248,84,0,0,1,15,1,66,100,0,0,17,1,52,85,0, 
  0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,66,100,0,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,66,100,0,0,1,15,1,66,100,0,0,17,1,208,170,0,0,1,15,1,66,100,0,0,17,1,16, 
  86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,66,100,0,0,1,19,4,0,0,0,13,0,0,0,1,0,17,1,66,100,0,0,1,2,19,10,0,0,0,23,0,0,0,2,0,1,21,0,127,0,0,0,0, 
  0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,57,101,0,0,1,19,13,0,0,0,32, 
  0,0,0,1,0,17,1,57,101,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,57,101,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,57,101,0,0,1,19,10,0,0,0,23,0,0,0,2,0,1, 
  2,21,1,59,0,0,0,124,26,2,0,8,0,0,0,3,0,0,0,40,144,1,128,49,144,1,128,37,144,1,128,123,208,1,128,124,208,1,128,5,208,65,127,6,208,65,128,78,144,1,128,12,17,1,108,171,0,0,1,10, 
  12,17,1,141,38,0,0,1,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0, 
  1,0,17,1,252,101,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,252,101,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,252,101,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,252,101, 
  0,0,1,19,10,0,0,0,23,0,0,0,2,0,1,2,21,1,70,0,0,0,110,27,2,0,6,0,0,0,2,0,0,0,124,40,2,128,5,40,2,128,6,40,2,128,43,80,65,128,119,144,65,128,123,40,2,128,4,17, 
  1,226,171,0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,17,1,18,172,0,0,1,8,2,21,1,242,0,0,0,162,6,2,0,10,0,0,0,3,0,0,0,62,208,3,128,57,0,3,128,69,232,132,129,123,136,7, 
  128,124,136,7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,94,172,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72, 
  0,0,1,15,1,94,172,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0, 
  0,3,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0, 
  1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,15,1,106,172,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,26,0,0,0,2,0,1,21,1,242,0,0,0,162,6,2,0,10,0,0,0,3,0,0,0,62, 
  208,3,128,57,0,3,128,69,232,132,129,123,136,7,128,124,136,7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,94,172,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92, 
  72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,94,172,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0, 
  204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,19,67,0,0,0,178, 
  0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,15,1,234,172,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,26,0,0,0,2,0,1,21,1,242,0,0, 
  0,162,6,2,0,10,0,0,0,3,0,0,0,62,208,3,128,57,0,3,128,69,232,132,129,123,136,7,128,124,136,7,128,5,136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,94,172,0,0,15, 
  1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,94,172,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67, 
  0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0, 
  22,0,0,0,3,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,15,1,106,173,0,0,17,1,25,78,0,0,1,8,19,10,0, 
  0,0,26,0,0,0,2,0,1,19,10,0,0,0,24,0,0,0,2,0,1,21,1,242,0,0,0,162,6,2,0,10,0,0,0,3,0,0,0,62,208,3,128,57,0,3,128,69,232,132,129,123,136,7,128,124,136,7,128,5, 
  136,71,127,6,136,135,126,55,208,129,128,77,0,6,128,79,24,7,128,15,1,94,172,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,94, 
  172,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,19, 
  67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,22,0,0,0,3,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0, 
  0,22,0,0,0,3,0,1,4,15,1,234,173,0,0,17,1,25,78,0,0,1,8,19,10,0,0,0,26,0,0,0,2,0,1,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193, 
  127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,199,106,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,199,106,0,0,1,19,13,0,0,0,31, 
  0,0,0,1,0,17,1,199,106,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,199,106,0,0,1,19,10,0,0,0,24,0,0,0,2,0,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0, 
  0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,71,107,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,71,107,0, 
  0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,71,107,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,71,107,0,0,1,19,10,0,0,0,23,0,0,0,2,0,1,2,21,0,127,0,0,0,0,0,0, 
  0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,199,107,0,0,1,19,13,0,0,0,32,0,0, 
  0,1,0,17,1,199,107,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,199,107,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,199,107,0,0,1,19,10,0,0,0,24,0,0,0,2,0,1,2,21, 
  0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,71,108,0,0,1, 
  19,13,0,0,0,32,0,0,0,1,0,17,1,71,108,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,71,108,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,71,108,0,0,1,19,10,0,0,0,24, 
  0,0,0,2,0,1,2,19,92,0,0,0,250,0,0,0,2,0,1,21,1,47,0,0,0,242,27,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112,1,128,55,48,65,128,123,112,1,128,12,17,1, 
  106,174,0,0,1,10,12,17,1,141,38,0,0,1,19,55,0,0,0,129,0,0,0,2,0,1,21,0,83,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,82,56,2,128,65,168,1,128,14,16,129,127,55,160,1,128, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,22,109,0,0,1,1,19,82,0,0,0,219,0,0,0,1,0,17,1,22,109,0,0,1,19,55,0,0,0,129,0,0,0,2,0,1,2,19,69,0,0,0,181,0,0,0,2, 
  0,1,21,0,83,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,69,48,2,128,65,160,193,127,14,16,65,128,82,56,2,128,19,82,0,0,0,218,0,0,0,1,0,17,1,118,109,0,0,1,19,82,0,0,0,219, 
  0,0,0,1,0,17,1,118,109,0,0,1,1,19,69,0,0,0,181,0,0,0,2,0,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128, 
  30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,202,109,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,202,109,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,202,109, 
  0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,202,109,0,0,1,19,10,0,0,0,24,0,0,0,2,0,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127, 
  10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,74,110,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,74,110,0,0,1,19,13,0,0,0,31,0, 
  0,0,1,0,17,1,74,110,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,74,110,0,0,1,19,10,0,0,0,24,0,0,0,2,0,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0, 
  17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,202,110,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,202,110,0,0, 
  1,19,13,0,0,0,31,0,0,0,1,0,17,1,202,110,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,202,110,0,0,1,19,10,0,0,0,23,0,0,0,2,0,1,2,21,1,63,0,0,0,19,7,2,0, 
  5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6,240,1,128,119,48,65,128,123,240,1,128,4,19,21,0,0,0,44,0,0,0,1,0,19,94,0,0,0,2,1,0,0,3,0,1,8,2,21,1,82,0,0,0, 
  63,28,2,0,7,0,0,0,2,0,0,0,124,136,2,128,5,136,2,128,6,136,130,128,47,112,129,128,70,176,1,128,123,136,66,128,127,240,1,128,4,17,1,160,174,0,0,1,4,17,1,238,174,0,0,1,4,19,70,0, 
  0,0,182,0,0,0,1,0,17,1,60,175,0,0,1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,5,19,87,0,0,0,237,0,0,0,5,0,1,20,2,27,0,0,0,0,0, 
  0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,16,0,0,0,37,0,0,0,6,0,1,20,2,27,0,0, 
  0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,20,0,0,0,42,0,0,0,3,0,1,21,1,73,2,0,0,50,229,1,0,23,0,0,0,4,0,0,0,16,112,3,128,65,64,11, 
  128,34,16,196,131,35,176,196,131,36,80,197,131,5,64,146,130,6,64,210,129,39,8,199,131,72,192,13,128,73,96,14,128,42,168,7,128,59,160,10,128,76,0,15,128,38,240,197,128,46,72,8,128,53,232,136,129,54,136,9, 
  128,66,224,11,128,67,128,12,128,68,32,77,128,100,160,15,128,117,56,16,128,119,208,16,128,15,1,138,175,0,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1, 
  68,6,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0, 
  0,0,9,1,0,0,1,0,19,34,0,0,0,80,0,0,0,3,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,138,175,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1, 
  15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0, 
  0,1,0,19,34,0,0,0,80,0,0,0,3,0,1,15,1,138,175,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,138,175,0, 
  0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,138,175,0,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,138,175,0, 
  0,15,1,56,6,0,0,4,17,1,219,9,0,0,1,15,1,138,175,0,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,138,175,0,0,15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,138,175,0, 
  0,15,1,188,10,0,0,17,1,205,37,0,0,1,15,1,138,175,0,0,15,1,56,11,0,0,17,1,25,38,0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0, 
  0,9,1,0,0,1,0,19,34,0,0,0,80,0,0,0,3,0,1,8,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,44,16,1,128,125,208,0,128,12,17,1,141,38,0,0,1,12,17,1,150,175, 
  0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12,17,1,141,38,0,0,1,21,1,91,2,0,0,74,29,2,0,24,0,0,0,4,0,0,0,16,144,3,128, 
  65,96,11,128,34,48,196,131,35,208,196,131,36,112,197,131,5,208,146,130,6,208,210,129,39,40,7,132,72,224,13,128,73,128,14,128,42,200,7,128,59,192,10,128,76,32,15,128,38,16,198,128,46,104,8,128,53,8,201,129, 
  54,168,9,128,66,0,12,128,67,160,12,128,68,64,77,128,84,192,79,128,100,48,16,128,117,200,16,128,119,96,17,128,15,1,208,175,0,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,208,175,0,0,15,1, 
  56,6,0,0,4,17,1,68,6,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0, 
  0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,1,0,0,0,2,0,0,0,3,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,208,175,0,0,15,1,112,7,0,0,4, 
  17,1,124,7,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19, 
  96,0,0,0,9,1,0,0,1,0,19,1,0,0,0,2,0,0,0,3,0,1,15,1,208,175,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,180,8,0, 
  0,1,15,1,208,175,0,0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,144,9,0, 
  0,1,15,1,208,175,0,0,15,1,56,6,0,0,4,17,1,219,9,0,0,1,15,1,208,175,0,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,208,175,0,0,15,1,225,5,0,0,4,17,1,113,10,0, 
  0,1,15,1,220,175,0,0,4,17,1,168,14,0,0,1,15,1,208,175,0,0,15,1,188,10,0,0,17,1,205,37,0,0,1,15,1,208,175,0,0,15,1,56,11,0,0,17,1,25,38,0,0,1,4,19,21,0,0,0, 
  44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,1,0,0,0,2,0,0,0,3,0,1,8,2,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0, 
  124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,4,17,1,232,175,0,0,1,8,2,21,1,53,0,0,0,179,0,2,0,5,0,0,0,2,0,0,0,124,160,1,128,5,160,1,128,6,160,129,128, 
  123,160,1,128,94,48,1,128,15,1,84,86,0,0,4,17,1,96,86,0,0,1,8,2,21,1,51,0,0,0,127,1,2,0,4,0,0,0,2,0,0,0,115,80,1,128,5,144,1,128,6,144,1,128,107,16,65,127,4,17, 
  1,236,90,0,0,1,4,17,1,111,91,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184, 
  140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234, 
  44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44, 
  0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67, 
  0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27, 
  0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,37,178,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29, 
  0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121, 
  240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0, 
  0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1, 
  246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83, 
  0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36, 
  0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,181,178,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0, 
  52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128, 
  124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47, 
  0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0, 
  4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79, 
  0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33, 
  0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,69,179,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0, 
  0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45, 
  0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1, 
  130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13, 
  178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1, 
  147,44,0,0,1,15,1,213,179,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96, 
  3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1, 
  0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19, 
  3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,101,180,0,0, 
  4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5, 
  184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0, 
  0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0, 
  0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19, 
  27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0, 
  0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,245,180,0,0,4,19,70,0,0,0,182,0,0,0,1, 
  0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128, 
  117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47, 
  0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0, 
  0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,133,181,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21, 
  1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9, 
  128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15, 
  1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15, 
  1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0, 
  0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,21,182,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,212,0,0,0,232,35,2,0,19,0, 
  0,0,4,0,0,0,98,112,4,128,97,48,4,128,82,48,131,127,83,112,3,131,84,176,131,129,5,152,198,130,6,152,198,129,103,48,5,128,104,48,5,128,105,152,5,128,100,176,4,128,107,216,133,129,28,240,130,129,102,240, 
  4,128,94,240,3,128,115,24,6,128,117,88,6,128,123,152,6,128,124,152,6,128,12,17,1,165,182,0,0,1,12,17,1,62,184,0,0,1,12,17,1,244,117,0,0,1,12,17,1,215,185,0,0,1,12,17,1,112,187,0, 
  0,1,12,17,1,9,189,0,0,1,12,17,1,141,119,0,0,1,12,17,1,38,121,0,0,1,12,17,1,191,122,0,0,1,12,19,23,0,0,0,49,0,0,0,1,0,1,12,17,1,88,124,0,0,1,12,17,1,162,190, 
  0,0,1,12,17,1,59,192,0,0,1,12,17,1,241,125,0,0,1,10,12,17,1,212,193,0,0,1,21,1,69,0,0,0,52,1,2,0,5,0,0,0,2,0,0,0,84,112,1,128,5,32,194,128,6,32,66,128,82,48, 
  1,128,125,224,1,128,4,17,1,109,195,0,0,1,15,1,186,195,0,0,4,17,1,109,90,0,0,1,4,17,1,1,196,0,0,1,8,2,19,63,0,0,0,171,0,0,0,2,0,1,19,37,0,0,0,88,0,0,0,1, 
  0,1,21,0,61,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,44,80,1,128,37,240,64,128,63,224,1,128,19,63,0,0,0,171,0,0,0,2,0,1,19,37,0,0,0,87,0,0,0,1,0,17,1,246,131,0, 
  0,1,1,2,21,1,117,3,0,0,226,36,2,0,25,0,0,0,4,0,0,0,16,176,3,128,65,120,16,128,34,24,5,132,35,24,6,132,36,24,7,132,5,160,219,130,6,160,219,129,39,144,73,132,72,120,20,128,41,144, 
  10,131,42,0,11,128,59,120,15,128,76,120,22,128,38,24,8,129,30,176,68,128,46,0,12,128,53,0,205,129,54,0,14,128,66,120,17,128,67,120,18,128,68,120,147,128,73,120,21,128,100,120,23,128,117,208,24,128,119,208, 
  25,128,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,4,19,37,0,0,0,90,0,0,0,2,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15, 
  1,177,196,0,0,15,1,56,6,0,0,4,17,1,68,6,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,78,196,0,0,15,1,141, 
  196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,15,1,78,196,0,0,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,42,0,0,0,100,0,0, 
  0,1,0,17,1,141,196,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,210,24,0,0,4,17,1,29,25,0,0,1,15,1,78,196, 
  0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,199,7,0,0,1, 
  15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,18,8,0,0,1,15,1,78,196,0,0,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1, 
  0,19,42,0,0,0,100,0,0,0,1,0,17,1,141,196,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,78,196,0,0,15,1,141, 
  196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,255,8,0,0,4,17,1,11,9,0,0,1,15,1,78,196,0, 
  0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15, 
  1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,6,0,0,4,17,1,219,9,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,225,5,0,0,4,17,1,38,10, 
  0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,188,10,0,0,15, 
  1,200,10,0,0,15,1,236,10,0,0,17,1,248,10,0,0,1,15,1,78,196,0,0,15,1,141,196,0,0,15,1,177,196,0,0,15,1,56,11,0,0,4,17,1,68,11,0,0,1,15,1,78,196,0,0,4,19,21,0, 
  0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,42,0,0,0,100,0,0,0,1,0,17,1,141,196,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0, 
  12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17, 
  1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,189,196,0,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11, 
  128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1, 
  130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13, 
  178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1, 
  147,44,0,0,1,15,1,73,197,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64, 
  3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44, 
  0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246, 
  47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0, 
  0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,213,197,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198, 
  246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15, 
  1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0, 
  0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0, 
  0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,97,198,0,0,4,19,70,0,0,0,182, 
  0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126, 
  127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0, 
  0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147, 
  44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1, 
  0,17,1,147,44,0,0,1,15,1,237,198,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5, 
  130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4, 
  17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0, 
  0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,121,199,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1, 
  0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216, 
  11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1, 
  210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0, 
  11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19, 
  43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,5,200,0,0,4,19,70,0, 
  0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,171,0,0,0,164,37,2,0,16,0,0,0,4,0,0,0,98,144,3,128,102,208,3,128,82,208,130,127,83,16,195,129,84,80,3,128,5,80,5,130,6, 
  80,197,126,103,16,4,128,104,16,4,128,105,80,4,128,115,208,4,128,107,144,196,128,28,144,194,128,117,16,5,128,123,80,5,128,124,80,5,128,12,17,1,145,200,0,0,1,12,17,1,14,202,0,0,1,12,17,1,170,135, 
  0,0,1,12,17,1,139,203,0,0,1,12,17,1,39,137,0,0,1,12,17,1,164,138,0,0,1,12,17,1,141,38,0,0,1,12,17,1,33,140,0,0,1,12,17,1,8,205,0,0,1,12,17,1,133,206,0,0,1,12, 
  17,1,158,141,0,0,1,10,12,17,1,2,208,0,0,1,21,1,44,0,0,0,136,38,2,0,3,0,0,0,1,0,0,0,6,88,1,128,5,88,65,128,125,240,0,128,4,19,57,0,0,0,142,0,0,0,4,0,1,8, 
  19,57,0,0,0,132,0,0,0,3,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,123,0,0,0,22,0,0,0,15,1,127,209,0,0,5,17,1,109,90,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0, 
  0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,57,0,0,0,141,0,0,0,3,0,1,21,1,44,0,0,0,136,38,2,0,3,0,0,0,1,0,0,0,6,88,1,128,5,88,65,128,125,240,0,128,4,19,57, 
  0,0,0,145,0,0,0,4,0,1,8,19,57,0,0,0,139,0,0,0,3,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,63,0,0,0,22,0,0,0,5,19,57,0,0,0,144,0,0,0,4,0,1,20,2, 
  27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,57,0,0,0,143,0,0,0,3,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5, 
  19,56,0,0,0,130,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,123,0,0,0,22,0, 
  0,0,15,1,139,209,0,0,5,17,1,109,90,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,57,0,0,0,135,0,0,0,3,0,1,20,4,35,0,0, 
  0,0,0,0,0,1,0,0,0,63,0,0,0,22,0,0,0,5,19,57,0,0,0,138,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,57,0, 
  0,0,133,0,0,0,3,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,79,0,0,0,211,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0, 
  0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,79,0,0,0,210,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0, 
  0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,79,0,0,0,208,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0, 
  2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,79,0,0,0,207,0,0,0,4,0,1,20,2,27,0,0,0,0, 
  0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,47,0,0,0,121,21,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12, 
  17,1,151,209,0,0,1,10,12,19,12,0,0,0,30,0,0,0,1,0,1,21,1,143,0,0,0,196,21,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48,3,130,98,112,2,130,83,48,2,130,100,176,2,130,5, 
  112,196,128,6,112,132,126,127,48,4,128,117,112,3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17,1,199,209,0,0,1,12,17,1,96,211,0,0,1,12,17,1,249,212,0,0,1,12,17,1,146,214, 
  0,0,1,12,17,1,43,216,0,0,1,12,17,1,196,217,0,0,1,12,17,1,93,219,0,0,1,12,17,1,246,220,0,0,1,12,17,1,143,222,0,0,1,10,12,19,23,0,0,0,49,0,0,0,1,0,1,21,1,52, 
  0,0,0,127,39,2,0,5,0,0,0,2,0,0,0,88,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1,128,4,19,68,0,0,0,179,0,0,0,4,0,1,8,19,68,0,0,0,180,0,0,0,3,0, 
  1,21,1,119,0,0,0,191,39,2,0,11,0,0,0,3,0,0,0,102,112,2,128,105,176,194,129,98,48,194,129,83,240,193,129,124,176,3,128,5,176,3,128,6,176,131,126,127,112,3,128,121,240,2,128,122,48,3,128,123, 
  176,3,128,12,17,1,40,224,0,0,1,12,17,1,147,225,0,0,1,12,17,1,254,226,0,0,1,12,17,1,105,228,0,0,1,12,17,1,212,229,0,0,1,12,17,1,63,231,0,0,1,12,17,1,170,232,0,0,1,10, 
  12,17,1,141,38,0,0,1,21,1,124,1,0,0,191,39,2,0,12,0,0,0,3,0,0,0,29,16,2,128,105,224,5,130,98,176,3,130,83,128,2,130,124,216,11,128,5,216,203,126,6,216,139,128,127,16,11,128,102,176, 
  4,128,121,16,7,128,122,16,9,128,123,216,11,128,4,15,1,21,234,0,0,17,1,117,150,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17, 
  1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47, 
  0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,165,234,0,0, 
  15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1, 
  165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,21,234,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123, 
  112,1,128,97,48,1,128,4,17,1,177,234,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6, 
  184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1, 
  234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0, 
  67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19, 
  27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,214,236,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58, 
  51,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,102,237,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,246,237,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,134,238,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,22,239,0, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,166,239,0,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,58,51,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,54,240,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,198,240,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13, 
  0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1, 
  74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,86,241,0,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,1,201,0,0,0,93,248,1,0,9,0,0,0,3,0,0,0,121,96,3,128, 
  105,192,194,127,98,80,66,129,83,176,65,129,124,64,6,128,5,64,6,128,6,64,6,128,122,208,4,128,123,64,6,128,15,1,230,241,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,230,241,0,0,4,17, 
  1,59,45,0,0,1,15,1,230,241,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,25,0,0,0,56,0,0,0,2,0,1,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,25,0,0,0,56,0,0,0,2,0,1,8,2, 
  21,1,47,0,0,0,226,40,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17,1,242,241,0,0,1,10,12,19,12,0,0,0,30,0,0,0,1,0,1, 
  21,1,143,0,0,0,128,41,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48,3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112,132,126,127,48,4,128,117,112,3,128,121,176,3,128,122,240, 
  3,128,123,112,4,128,124,112,4,128,12,17,1,34,242,0,0,1,12,17,1,187,243,0,0,1,12,17,1,84,245,0,0,1,12,17,1,237,246,0,0,1,12,17,1,134,248,0,0,1,12,17,1,31,250,0,0,1,12,17, 
  1,184,251,0,0,1,12,17,1,81,253,0,0,1,6,17,1,234,254,0,0,1,10,12,19,23,0,0,0,49,0,0,0,1,0,1,21,1,47,0,0,0,226,40,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112, 
  193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17,1,198,255,0,0,1,10,12,19,12,0,0,0,30,0,0,0,1,0,1,21,1,143,0,0,0,128,41,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48, 
  3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112,132,126,127,48,4,128,117,112,3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17,1,246,255,0,0,1,12,17,1,143,1,1, 
  0,1,12,17,1,40,3,1,0,1,12,17,1,193,4,1,0,1,12,17,1,90,6,1,0,1,12,17,1,243,7,1,0,1,12,17,1,140,9,1,0,1,12,17,1,37,11,1,0,1,6,17,1,190,12,1,0,1,10,12, 
  19,23,0,0,0,49,0,0,0,1,0,1,21,1,47,0,0,0,226,40,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17,1,154,13,1,0,1,10,12, 
  19,12,0,0,0,30,0,0,0,1,0,1,21,1,143,0,0,0,128,41,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48,3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112,132,126,127,48, 
  4,128,117,112,3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17,1,202,13,1,0,1,12,17,1,99,15,1,0,1,12,17,1,252,16,1,0,1,12,17,1,149,18,1,0,1,12,17,1,46,20,1, 
  0,1,12,17,1,199,21,1,0,1,12,17,1,96,23,1,0,1,12,17,1,249,24,1,0,1,6,17,1,146,26,1,0,1,10,12,19,23,0,0,0,49,0,0,0,1,0,1,21,1,47,0,0,0,226,40,2,0,5,0, 
  0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17,1,110,27,1,0,1,10,12,19,12,0,0,0,30,0,0,0,1,0,1,21,1,143,0,0,0,128,41,2,0,13,0, 
  0,0,3,0,0,0,102,240,2,128,105,48,3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112,132,126,127,48,4,128,117,112,3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17, 
  1,158,27,1,0,1,12,17,1,55,29,1,0,1,12,17,1,208,30,1,0,1,12,17,1,105,32,1,0,1,12,17,1,2,34,1,0,1,12,17,1,155,35,1,0,1,12,17,1,52,37,1,0,1,12,17,1,205,38,1, 
  0,1,6,17,1,102,40,1,0,1,10,12,19,23,0,0,0,49,0,0,0,1,0,1,21,1,117,0,0,0,198,46,2,0,8,0,0,0,3,0,0,0,40,0,2,128,49,112,2,128,37,144,1,128,123,160,3,128,124,160, 
  3,128,5,160,67,127,6,160,67,128,78,48,3,128,15,1,66,41,1,0,4,17,1,23,65,0,0,1,15,1,66,41,1,0,4,17,1,85,66,0,0,1,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,97, 
  0,0,0,2,0,1,15,1,66,41,1,0,4,17,1,72,77,0,0,1,8,2,21,1,47,0,0,0,62,47,2,0,5,0,0,0,2,0,0,0,56,48,1,129,5,112,1,128,6,112,1,128,123,112,1,128,124,112,1,128, 
  4,17,1,78,41,1,0,1,8,2,21,1,75,0,0,0,127,47,2,0,6,0,0,0,2,0,0,0,124,80,2,128,5,80,2,128,6,80,2,128,51,80,65,128,119,144,65,128,123,80,2,128,4,17,1,137,41,1,0,1, 
  4,19,21,0,0,0,44,0,0,0,1,0,19,94,0,0,0,0,1,0,0,4,0,1,8,2,19,10,0,0,0,22,0,0,0,3,0,1,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128, 
  13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,106,172,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,106,172,0,0,1,19,13,0, 
  0,0,31,0,0,0,1,0,17,1,106,172,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,106,172,0,0,1,19,10,0,0,0,22,0,0,0,3,0,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0, 
  2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,234,172,0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1, 
  234,172,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,234,172,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,234,172,0,0,1,19,10,0,0,0,22,0,0,0,3,0,1,2,21,0,127,0,0,0, 
  0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,106,173,0,0,1,19,13,0,0,0, 
  32,0,0,0,1,0,17,1,106,173,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,106,173,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,106,173,0,0,1,19,10,0,0,0,22,0,0,0,3,0, 
  1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,17,232,193,128,13,88,193,127,10,80,129,128,29,120,130,128,30,8,3,128,77,152,3,128,1,19,77,0,0,0,202,0,0,0,1,0,17,1,234,173, 
  0,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,234,173,0,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,234,173,0,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,234,173,0,0,1,19,10,0, 
  0,0,22,0,0,0,3,0,1,2,21,1,53,0,0,0,3,48,2,0,5,0,0,0,2,0,0,0,124,160,1,128,5,160,1,128,6,160,1,128,55,48,65,128,123,160,1,128,15,1,185,41,1,0,4,17,1,140,72,0, 
  0,1,8,2,21,1,77,0,0,0,8,49,2,0,6,0,0,0,2,0,0,0,84,80,65,129,5,96,2,128,6,96,130,128,123,96,2,128,102,192,1,128,124,96,2,128,15,1,197,41,1,0,4,17,1,209,41,1,0,1, 
  15,1,197,41,1,0,15,1,109,42,1,0,4,17,1,121,42,1,0,1,8,2,21,1,77,0,0,0,8,49,2,0,6,0,0,0,2,0,0,0,84,80,65,129,5,96,2,128,6,96,130,128,123,96,2,128,102,192,1,128, 
  124,96,2,128,15,1,57,43,1,0,4,17,1,69,43,1,0,1,15,1,57,43,1,0,15,1,225,43,1,0,4,17,1,237,43,1,0,1,8,2,21,1,77,0,0,0,8,49,2,0,6,0,0,0,2,0,0,0,84,80, 
  65,129,5,96,2,128,6,96,130,128,123,96,2,128,102,192,1,128,124,96,2,128,15,1,70,44,1,0,4,17,1,82,44,1,0,1,15,1,70,44,1,0,15,1,238,44,1,0,4,17,1,250,44,1,0,1,8,2,19,34, 
  0,0,0,80,0,0,0,3,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,44,0,0,0,22,0,0,0,5,17,1,70,45,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0, 
  0,0,26,0,0,0,9,2,19,1,0,0,0,2,0,0,0,3,0,1,19,1,0,0,0,3,0,0,0,3,0,1,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0, 
  4,130,83,16,2,130,124,24,17,128,5,24,209,128,6,24,145,126,127,80,16,128,117,160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43, 
  0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1, 
  152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0, 
  15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43, 
  0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1, 
  152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0, 
  11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1, 
  77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0, 
  0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,222,45,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,19,71,0,0,0,183,0,0,0,2,0,1,19,71,0,0,0,184,0, 
  0,0,2,0,1,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0, 
  218,0,0,0,1,0,17,1,37,178,0,0,1,1,15,1,37,178,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,37,178,0,0,1,15,1,37, 
  178,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,37,178,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65, 
  240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,181,178,0,0,1,1,15,1,181,178,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0, 
  2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,181,178,0,0,1,15,1,181,178,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,181,178,0,0,1,2,21,0,143,0,0,0,0, 
  0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,69,179,0,0, 
  1,1,15,1,69,179,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,69,179,0,0,1,15,1,69,179,0,0,17,1,16,86,0,0,1,19,33, 
  0,0,0,78,0,0,0,1,0,17,1,69,179,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14, 
  144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,213,179,0,0,1,1,15,1,213,179,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0, 
  1,0,17,1,213,179,0,0,1,15,1,213,179,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,213,179,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57, 
  144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,101,180,0,0,1,1,15,1,101,180,0,0,17,1,52,85, 
  0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,101,180,0,0,1,15,1,101,180,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,101, 
  180,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0, 
  218,0,0,0,1,0,17,1,245,180,0,0,1,1,15,1,245,180,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,245,180,0,0,1,15,1,245, 
  180,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,245,180,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65, 
  240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,133,181,0,0,1,1,15,1,133,181,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0, 
  2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,133,181,0,0,1,15,1,133,181,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,133,181,0,0,1,2,21,0,143,0,0,0,0, 
  0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,21,182,0,0, 
  1,1,15,1,21,182,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,21,182,0,0,1,15,1,21,182,0,0,17,1,16,86,0,0,1,19,33, 
  0,0,0,78,0,0,0,1,0,17,1,21,182,0,0,1,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6, 
  184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1, 
  234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0, 
  67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19, 
  27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,162,46,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116, 
  29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,50,47,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,194,47,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,82,48,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,226,48,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,114,49,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,2,50,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,146,50,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,65,0,0,0,108,53,2,0,6, 
  0,0,0,2,0,0,0,84,80,1,129,5,0,2,129,6,0,2,128,123,0,2,128,124,0,2,128,125,192,1,128,15,1,34,51,1,0,4,17,1,109,90,0,0,1,4,17,1,98,51,1,0,1,8,19,57,0,0,0,136, 
  0,0,0,2,0,1,21,1,59,0,0,0,89,54,2,0,6,0,0,0,2,0,0,0,124,208,1,128,5,208,1,129,6,208,129,128,123,208,1,128,82,80,1,128,125,144,1,128,4,17,1,163,51,1,0,1,4,17,1,227, 
  51,1,0,1,8,19,57,0,0,0,134,0,0,0,2,0,1,21,1,65,0,0,0,70,55,2,0,6,0,0,0,2,0,0,0,84,144,65,129,5,0,2,128,6,0,130,128,123,0,2,128,82,80,1,128,124,0,2,128,4, 
  17,1,35,52,1,0,1,15,1,100,52,1,0,4,17,1,109,90,0,0,1,8,19,57,0,0,0,137,0,0,0,2,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,37,0, 
  0,0,89,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,15, 
  1,141,196,0,0,17,1,164,52,1,0,1,2,19,42,0,0,0,100,0,0,0,1,0,1,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65, 
  200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,189,196,0,0,1,15,1,189,196,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219, 
  0,0,0,1,0,17,1,189,196,0,0,1,1,15,1,189,196,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,189,196,0,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2, 
  0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,73,197,0,0,1,15,1,73,197,0,0,17,1,52,85,0, 
  0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,73,197,0,0,1,1,15,1,73,197,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,73, 
  197,0,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0, 
  1,0,17,1,213,197,0,0,1,15,1,213,197,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,213,197,0,0,1,1,15,1,213,197,0,0,17, 
  1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,213,197,0,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65, 
  200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,97,198,0,0,1,15,1,97,198,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219, 
  0,0,0,1,0,17,1,97,198,0,0,1,1,15,1,97,198,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,97,198,0,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2, 
  0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,237,198,0,0,1,15,1,237,198,0,0,17,1,52,85,0, 
  0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,237,198,0,0,1,1,15,1,237,198,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,237, 
  198,0,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0, 
  1,0,17,1,121,199,0,0,1,15,1,121,199,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,121,199,0,0,1,1,15,1,121,199,0,0,17, 
  1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,121,199,0,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65, 
  200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,5,200,0,0,1,15,1,5,200,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219, 
  0,0,0,1,0,17,1,5,200,0,0,1,1,15,1,5,200,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,5,200,0,0,1,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3, 
  0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0, 
  67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19, 
  27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,241,52,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116, 
  29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128, 
  121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1, 
  15,1,125,53,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2, 
  130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222, 
  44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15, 
  1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0, 
  0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,9,54,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0, 
  0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27, 
  0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0, 
  1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,149,54,1,0,4,19,70,0,0,0,182,0,0,0,1,0, 
  17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130,83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117, 
  160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44, 
  0,0,1,15,1,33,55,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,124,1,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,64,4,128,105,112,5,130,98,64,3,130, 
  83,16,2,130,124,216,11,128,5,216,203,128,6,216,139,126,127,16,11,128,117,160,6,128,121,16,7,128,122,16,9,128,123,216,11,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0, 
  15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1, 
  147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0, 
  0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0, 
  1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,173,55,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,19,57,0,0,0,140,0,0,0, 
  4,0,1,19,57,0,0,0,131,0,0,0,4,0,1,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,4,17,1,57,56,1,0, 
  1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7, 
  128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15, 
  1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47, 
  0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0, 
  0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0, 
  0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,94,58,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35, 
  2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184, 
  12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1, 
  210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0, 
  4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17, 
  1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0, 
  0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0, 
  0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,238,58,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102, 
  32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0, 
  1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44, 
  0,0,1,15,1,126,59,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130, 
  83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1, 
  210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0, 
  1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0, 
  0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,14,60,1,0,4,19, 
  70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204, 
  128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4, 
  17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0, 
  2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0, 
  0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1, 
  0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,158,60,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17, 
  1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128, 
  7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0, 
  4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0, 
  0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1, 
  0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,46,61,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152, 
  1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123, 
  184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147, 
  44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25, 
  178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0, 
  0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1, 
  0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,190,61,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0, 
  3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17, 
  1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0, 
  0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147, 
  44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1, 
  0,17,1,147,44,0,0,1,15,1,78,62,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6, 
  130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147, 
  44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0, 
  0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13, 
  178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0, 
  0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,222, 
  62,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,32,4,128,105,80,197,129,98,32,195,129,83,240,193,129,124,72, 
  11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234, 
  44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0, 
  4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,165,234,0,0,15,1, 
  66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,165,234, 
  0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1, 
  15,1,110,63,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,32,4,128,105,80,197,129,98,32,195,129,83,240,193, 
  129,124,72,11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4, 
  17,1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,165,234,0, 
  0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15, 
  1,165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44, 
  0,0,1,15,1,250,63,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,32,4,128,105,80,197,129,98,32,195,129, 
  83,240,193,129,124,72,11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1, 
  165,234,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,134,64,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,32,4,128,105,80,197,129,98, 
  32,195,129,83,240,193,129,124,72,11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15, 
  1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147, 
  44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0, 
  1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1, 
  147,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0, 
  1,0,17,1,147,44,0,0,1,15,1,18,65,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,32,4,128,105,80, 
  197,129,98,32,195,129,83,240,193,129,124,72,11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44, 
  0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246, 
  47,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1, 
  0,17,1,147,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79, 
  0,0,0,1,0,17,1,147,44,0,0,1,15,1,158,65,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0,102,32,4, 
  128,105,80,197,129,98,32,195,129,83,240,193,129,124,72,11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4, 
  17,1,246,47,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0, 
  0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,42,66,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,106,1,0,0,135,2,2,0,11,0,0,0,3,0,0,0, 
  102,32,4,128,105,80,197,129,98,32,195,129,83,240,193,129,124,72,11,128,5,72,11,128,6,72,139,126,127,128,10,128,121,128,6,128,122,128,8,128,123,72,11,128,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,165,234,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47, 
  0,0,4,17,1,246,47,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0, 
  0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,165,234,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,182,66,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3, 
  0,0,0,57,144,194,128,33,40,194,127,18,32,194,128,65,240,2,128,68,32,2,128,82,232,3,128,14,144,1,128,79,128,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,21,234,0,0,1,1,15,1,21,234,0,0, 
  17,1,52,85,0,0,1,19,18,0,0,0,39,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,21,234,0,0,1,15,1,21,234,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1, 
  0,17,1,21,234,0,0,1,2,19,18,0,0,0,39,0,0,0,2,0,1,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0,4,130,83,16,2,130,124,24,17,128,5, 
  24,209,128,6,24,145,126,127,80,16,128,117,160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0, 
  0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4, 
  17,1,166,48,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0, 
  67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15, 
  1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44, 
  0,0,1,15,1,66,67,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,58,51,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128, 
  65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,214,236,0,0,1,1,15,1,214,236,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,214,236,0,0,1,15,1,214,236,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,214,236,0,0,1,2,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,102,237,0, 
  0,1,1,15,1,102,237,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,102,237,0,0,1,15,1,102,237,0,0,17,1,16,86,0,0,1,19, 
  33,0,0,0,78,0,0,0,1,0,17,1,102,237,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,246,237,0,0,1,1,15,1,246,237,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,246,237,0,0,1,15,1,246,237,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,246,237,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,134,238,0,0,1,1,15,1,134,238,0,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,134,238,0,0,1,15,1,134,238,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  134,238,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,22,239,0,0,1,1,15,1,22,239,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,22,239,0,0,1,15,1, 
  22,239,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,22,239,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128, 
  65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,166,239,0,0,1,1,15,1,166,239,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,166,239,0,0,1,15,1,166,239,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,166,239,0,0,1,2,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,54,240,0, 
  0,1,1,15,1,54,240,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,54,240,0,0,1,15,1,54,240,0,0,17,1,16,86,0,0,1,19, 
  33,0,0,0,78,0,0,0,1,0,17,1,54,240,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,198,240,0,0,1,1,15,1,198,240,0,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,198,240,0,0,1,15,1,198,240,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,198,240,0,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,86,241,0,0,1,1,15,1,86,241,0,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,86,241,0,0,1,15,1,86,241,0,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  86,241,0,0,1,2,19,25,0,0,0,56,0,0,0,2,0,1,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,4,17,1,6, 
  68,1,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,43,70,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,187,70,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,75,71,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,219,71,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,107,72,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,251,72,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,139,73,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13, 
  0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1, 
  74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,27,74,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,212,0,0,0,232,35,2,0,19,0,0,0,4,0,0,0,98,112,4,128, 
  97,48,4,128,82,48,131,127,83,112,3,131,84,176,131,129,5,152,198,130,6,152,198,129,103,48,5,128,104,48,5,128,105,152,5,128,100,176,4,128,107,216,133,129,28,240,130,129,102,240,4,128,94,240,3,128,115,24,6,128, 
  117,88,6,128,123,152,6,128,124,152,6,128,12,17,1,171,74,1,0,1,12,17,1,68,76,1,0,1,12,17,1,34,242,0,0,1,12,17,1,221,77,1,0,1,12,17,1,118,79,1,0,1,12,17,1,15,81,1,0,1, 
  12,17,1,187,243,0,0,1,12,17,1,84,245,0,0,1,12,17,1,237,246,0,0,1,12,19,23,0,0,0,49,0,0,0,1,0,1,12,17,1,134,248,0,0,1,12,17,1,168,82,1,0,1,12,17,1,65,84,1,0, 
  1,12,17,1,31,250,0,0,1,10,12,17,1,218,85,1,0,1,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,4,17,1,115, 
  87,1,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,152,89,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,40,90,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,184,90,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,72,91,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,216,91,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,104,92,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,248,92,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13, 
  0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1, 
  74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,136,93,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,212,0,0,0,232,35,2,0,19,0,0,0,4,0,0,0,98,112,4,128, 
  97,48,4,128,82,48,131,127,83,112,3,131,84,176,131,129,5,152,198,130,6,152,198,129,103,48,5,128,104,48,5,128,105,152,5,128,100,176,4,128,107,216,133,129,28,240,130,129,102,240,4,128,94,240,3,128,115,24,6,128, 
  117,88,6,128,123,152,6,128,124,152,6,128,12,17,1,24,94,1,0,1,12,17,1,177,95,1,0,1,12,17,1,246,255,0,0,1,12,17,1,74,97,1,0,1,12,17,1,227,98,1,0,1,12,17,1,124,100,1,0,1, 
  12,17,1,143,1,1,0,1,12,17,1,40,3,1,0,1,12,17,1,193,4,1,0,1,12,19,23,0,0,0,49,0,0,0,1,0,1,12,17,1,90,6,1,0,1,12,17,1,21,102,1,0,1,12,17,1,174,103,1,0, 
  1,12,17,1,243,7,1,0,1,10,12,17,1,71,105,1,0,1,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,4,17,1,224, 
  106,1,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,5,109,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,149,109,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,37,110,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,181,110,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,69,111,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,213,111,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,101,112,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13, 
  0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1, 
  74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,245,112,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,212,0,0,0,232,35,2,0,19,0,0,0,4,0,0,0,98,112,4,128, 
  97,48,4,128,82,48,131,127,83,112,3,131,84,176,131,129,5,152,198,130,6,152,198,129,103,48,5,128,104,48,5,128,105,152,5,128,100,176,4,128,107,216,133,129,28,240,130,129,102,240,4,128,94,240,3,128,115,24,6,128, 
  117,88,6,128,123,152,6,128,124,152,6,128,12,17,1,133,113,1,0,1,12,17,1,30,115,1,0,1,12,17,1,202,13,1,0,1,12,17,1,183,116,1,0,1,12,17,1,80,118,1,0,1,12,17,1,233,119,1,0,1, 
  12,17,1,99,15,1,0,1,12,17,1,252,16,1,0,1,12,17,1,149,18,1,0,1,12,19,23,0,0,0,49,0,0,0,1,0,1,12,17,1,46,20,1,0,1,12,17,1,130,121,1,0,1,12,17,1,27,123,1,0, 
  1,12,17,1,199,21,1,0,1,10,12,17,1,180,124,1,0,1,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,4,17,1,77, 
  126,1,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,114,128,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,2,129,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,146,129,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,34,130,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,178,130,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,66,131,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,131,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13, 
  0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1, 
  74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,98,132,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,212,0,0,0,232,35,2,0,19,0,0,0,4,0,0,0,98,112,4,128, 
  97,48,4,128,82,48,131,127,83,112,3,131,84,176,131,129,5,152,198,130,6,152,198,129,103,48,5,128,104,48,5,128,105,152,5,128,100,176,4,128,107,216,133,129,28,240,130,129,102,240,4,128,94,240,3,128,115,24,6,128, 
  117,88,6,128,123,152,6,128,124,152,6,128,12,17,1,242,132,1,0,1,12,17,1,139,134,1,0,1,12,17,1,158,27,1,0,1,12,17,1,36,136,1,0,1,12,17,1,189,137,1,0,1,12,17,1,86,139,1,0,1, 
  12,17,1,55,29,1,0,1,12,17,1,208,30,1,0,1,12,17,1,105,32,1,0,1,12,19,23,0,0,0,49,0,0,0,1,0,1,12,17,1,2,34,1,0,1,12,17,1,239,140,1,0,1,12,17,1,136,142,1,0, 
  1,12,17,1,155,35,1,0,1,10,12,17,1,33,144,1,0,1,19,41,0,0,0,97,0,0,0,2,0,1,21,1,58,0,0,0,19,7,2,0,5,0,0,0,2,0,0,0,124,200,1,128,5,200,1,128,6,200,1,128, 
  119,48,65,128,123,200,1,128,4,19,21,0,0,0,44,0,0,0,1,0,17,1,186,145,1,0,1,8,2,21,1,47,0,0,0,29,61,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123, 
  112,1,128,61,48,1,128,4,17,1,6,146,1,0,1,8,2,19,45,0,0,0,104,0,0,0,2,0,1,19,30,0,0,0,71,0,0,0,4,0,1,21,1,155,0,0,0,34,62,2,0,8,0,0,0,3,0,0,0,84, 
  48,195,129,102,0,4,128,58,96,2,128,123,208,4,128,44,144,1,127,5,208,4,128,6,208,196,126,124,208,4,128,15,1,70,146,1,0,15,1,123,146,1,0,15,1,159,146,1,0,4,17,1,171,146,1,0,1,15,1,70, 
  146,1,0,15,1,123,146,1,0,15,1,219,146,1,0,4,17,1,231,146,1,0,1,15,1,70,146,1,0,15,1,123,146,1,0,15,1,159,146,1,0,4,17,1,23,147,1,0,1,15,1,70,146,1,0,15,1,123,146,1, 
  0,15,1,83,149,1,0,4,17,1,121,42,1,0,1,8,2,19,86,0,0,0,235,0,0,0,1,0,1,21,1,191,0,0,0,93,248,1,0,9,0,0,0,3,0,0,0,121,96,3,128,105,192,194,127,98,80,66,129,83, 
  176,65,129,124,240,5,128,5,240,5,128,6,240,5,128,122,168,4,128,123,240,5,128,15,1,95,149,1,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,95,149,1,0,4,17,1,59,45,0,0,1,15,1,95, 
  149,1,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,17,1,95,149,1,0,1,4,19, 
  43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,17,1,95,149,1,0,1,8,2,19,17,0,0,0,38,0,0,0,4,0,1,21,1,155,0,0,0,34, 
  62,2,0,8,0,0,0,3,0,0,0,84,48,195,129,102,0,4,128,58,96,2,128,123,208,4,128,44,144,1,127,5,208,4,128,6,208,196,126,124,208,4,128,15,1,143,149,1,0,15,1,196,149,1,0,15,1,232,149,1, 
  0,4,17,1,171,146,1,0,1,15,1,143,149,1,0,15,1,196,149,1,0,15,1,244,149,1,0,4,17,1,231,146,1,0,1,15,1,143,149,1,0,15,1,196,149,1,0,15,1,232,149,1,0,4,17,1,23,147,1,0, 
  1,15,1,143,149,1,0,15,1,196,149,1,0,15,1,0,150,1,0,4,17,1,237,43,1,0,1,8,2,19,38,0,0,0,91,0,0,0,1,0,1,21,1,88,0,0,0,159,237,1,0,6,0,0,0,2,0,0,0,124, 
  184,2,128,5,184,2,128,6,184,130,128,123,184,130,128,102,80,1,128,127,240,1,128,15,1,12,150,1,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,60,150,1,0,4,19,70,0,0,0,182,0,0,0,1, 
  0,17,1,116,29,0,0,1,8,2,19,29,0,0,0,70,0,0,0,4,0,1,21,1,155,0,0,0,34,62,2,0,8,0,0,0,3,0,0,0,84,48,195,129,102,0,4,128,58,96,2,128,123,208,4,128,44,144,1,127, 
  5,208,4,128,6,208,196,126,124,208,4,128,15,1,139,150,1,0,15,1,192,150,1,0,15,1,228,150,1,0,4,17,1,171,146,1,0,1,15,1,139,150,1,0,15,1,192,150,1,0,15,1,240,150,1,0,4,17,1,231, 
  146,1,0,1,15,1,139,150,1,0,15,1,192,150,1,0,15,1,228,150,1,0,4,17,1,23,147,1,0,1,15,1,139,150,1,0,15,1,192,150,1,0,15,1,252,150,1,0,4,17,1,250,44,1,0,1,8,2,19,81, 
  0,0,0,216,0,0,0,1,0,1,21,1,75,0,0,0,133,62,2,0,5,0,0,0,2,0,0,0,4,48,1,129,5,80,2,128,6,80,2,128,123,80,2,128,124,80,2,128,15,1,8,151,1,0,4,19,46,0,0,0, 
  106,0,0,0,1,0,19,83,0,0,0,222,0,0,0,1,0,17,1,56,151,1,0,1,8,2,21,1,139,0,0,0,177,250,1,0,6,0,0,0,2,0,0,0,16,80,65,129,5,80,196,128,6,80,4,128,127,48,3,128, 
  73,240,1,128,76,144,2,128,15,1,92,151,1,0,15,1,76,84,0,0,4,17,1,237,5,0,0,1,15,1,92,151,1,0,15,1,76,84,0,0,4,17,1,38,10,0,0,1,15,1,92,151,1,0,15,1,76,84,0,0, 
  4,17,1,113,10,0,0,1,15,1,92,151,1,0,4,19,70,0,0,0,182,0,0,0,1,0,19,62,0,0,0,170,0,0,0,1,0,17,1,88,84,0,0,1,8,2,19,4,0,0,0,12,0,0,0,3,0,1,21,0, 
  195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127,82,40,5,128,65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128,23,104,130,127,79,192,68,128,95,184,5,128,1,19,82,0, 
  0,0,218,0,0,0,1,0,17,1,222,45,1,0,1,15,1,222,45,1,0,17,1,248,84,0,0,1,15,1,222,45,1,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,222,45,1,0,1,19, 
  82,0,0,0,219,0,0,0,1,0,17,1,222,45,1,0,1,15,1,222,45,1,0,17,1,104,151,1,0,1,15,1,222,45,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,222,45,1,0, 
  1,19,4,0,0,0,12,0,0,0,3,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128, 
  23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,162,46,1,0,1,1,15,1,162,46,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17, 
  1,162,46,1,0,1,15,1,162,46,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,162,46,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128, 
  33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,50,47,1,0,1,1,15,1,50,47,1,0,17,1,52,85,0,0,1, 
  19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,50,47,1,0,1,15,1,50,47,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,50,47,1,0, 
  1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0, 
  0,1,0,17,1,194,47,1,0,1,1,15,1,194,47,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,194,47,1,0,1,15,1,194,47,1,0, 
  17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,194,47,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128, 
  71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,82,48,1,0,1,1,15,1,82,48,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1, 
  19,82,0,0,0,219,0,0,0,1,0,17,1,82,48,1,0,1,15,1,82,48,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,82,48,1,0,1,2,21,0,143,0,0,0,0,0,0,0, 
  8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,226,48,1,0,1,1,15, 
  1,226,48,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,226,48,1,0,1,15,1,226,48,1,0,17,1,16,86,0,0,1,19,33,0,0,0, 
  78,0,0,0,1,0,17,1,226,48,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128, 
  23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,114,49,1,0,1,1,15,1,114,49,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17, 
  1,114,49,1,0,1,15,1,114,49,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,114,49,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128, 
  33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,2,50,1,0,1,1,15,1,2,50,1,0,17,1,52,85,0,0,1, 
  19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,2,50,1,0,1,15,1,2,50,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,2,50,1,0, 
  1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0, 
  0,1,0,17,1,146,50,1,0,1,1,15,1,146,50,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,146,50,1,0,1,15,1,146,50,1,0, 
  17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,146,50,1,0,1,2,21,1,52,0,0,0,232,63,2,0,5,0,0,0,2,0,0,0,124,152,1,128,5,152,193,128,6,152,1,128,123,152,1,128, 
  125,48,1,128,4,19,57,0,0,0,142,0,0,0,4,0,1,8,19,57,0,0,0,132,0,0,0,3,0,1,21,1,53,0,0,0,146,64,2,0,5,0,0,0,2,0,0,0,84,48,1,129,5,160,1,128,6,160,1,128, 
  123,160,1,128,124,160,1,128,15,1,127,209,0,0,4,17,1,109,90,0,0,1,8,19,57,0,0,0,141,0,0,0,3,0,1,21,1,52,0,0,0,70,65,2,0,5,0,0,0,2,0,0,0,124,152,1,128,5,152,193, 
  128,6,152,1,128,123,152,1,128,125,48,1,128,4,19,57,0,0,0,145,0,0,0,4,0,1,8,19,57,0,0,0,139,0,0,0,3,0,1,21,1,52,0,0,0,29,66,2,0,5,0,0,0,2,0,0,0,124,152,1, 
  128,5,152,1,128,6,152,129,128,123,152,1,128,82,48,1,128,4,19,57,0,0,0,144,0,0,0,4,0,1,8,19,57,0,0,0,143,0,0,0,3,0,1,21,1,53,0,0,0,243,66,2,0,5,0,0,0,2,0,0, 
  0,84,48,1,129,5,160,1,128,6,160,1,128,123,160,1,128,124,160,1,128,15,1,139,209,0,0,4,17,1,109,90,0,0,1,8,19,57,0,0,0,135,0,0,0,3,0,1,21,1,52,0,0,0,29,66,2,0,5,0, 
  0,0,2,0,0,0,124,152,1,128,5,152,1,128,6,152,129,128,123,152,1,128,82,48,1,128,4,19,57,0,0,0,138,0,0,0,4,0,1,8,19,57,0,0,0,133,0,0,0,3,0,1,21,4,42,0,0,0,0,0, 
  0,0,2,0,0,0,1,0,0,0,125,208,0,128,59,16,193,127,12,17,1,141,38,0,0,1,12,17,1,4,152,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0, 
  11,12,17,1,141,38,0,0,1,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0, 
  218,0,0,0,1,0,17,1,241,52,1,0,1,15,1,241,52,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,241,52,1,0,1,1,15,1,241, 
  52,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,241,52,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71, 
  88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,125,53,1,0,1,15,1,125,53,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82, 
  0,0,0,219,0,0,0,1,0,17,1,125,53,1,0,1,1,15,1,125,53,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,125,53,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7, 
  0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,9,54,1,0,1,15,1,9,54,1,0,17, 
  1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,9,54,1,0,1,1,15,1,9,54,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1, 
  0,17,1,9,54,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0, 
  218,0,0,0,1,0,17,1,149,54,1,0,1,15,1,149,54,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,149,54,1,0,1,1,15,1,149, 
  54,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,149,54,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71, 
  88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,33,55,1,0,1,15,1,33,55,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82, 
  0,0,0,219,0,0,0,1,0,17,1,33,55,1,0,1,1,15,1,33,55,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,33,55,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7, 
  0,0,0,2,0,0,0,57,104,2,129,33,0,194,127,14,112,1,129,71,88,131,128,65,200,2,128,79,96,3,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,173,55,1,0,1,15,1,173,55,1,0,17, 
  1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,173,55,1,0,1,1,15,1,173,55,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1, 
  0,17,1,173,55,1,0,1,2,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0,4,130,83,16,2,130,124,24,17,128,5,24,209,128,6,24,145,126,127,80,16,128,117, 
  160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0, 
  0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,210,45,1, 
  0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83, 
  0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4, 
  19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,62,152,1,0,4,19,70, 
  0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,94,58,1,0,1,1,15,1,94,58,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,94,58,1,0,1,15,1,94,58,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,94,58,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,238,58,1,0,1,1,15,1,238,58,1,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,238,58,1,0,1,15,1,238,58,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  238,58,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,126,59,1,0,1,1,15,1,126,59,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,126,59,1,0,1,15,1, 
  126,59,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,126,59,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128, 
  65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,14,60,1,0,1,1,15,1,14,60,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,14,60,1,0,1,15,1,14,60,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,14,60,1,0,1,2,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,158,60,1, 
  0,1,1,15,1,158,60,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,158,60,1,0,1,15,1,158,60,1,0,17,1,16,86,0,0,1,19, 
  33,0,0,0,78,0,0,0,1,0,17,1,158,60,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,46,61,1,0,1,1,15,1,46,61,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,46,61,1,0,1,15,1,46,61,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,46,61,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,190,61,1,0,1,1,15,1,190,61,1,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,190,61,1,0,1,15,1,190,61,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  190,61,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,78,62,1,0,1,1,15,1,78,62,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,78,62,1,0,1,15,1, 
  78,62,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,78,62,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128, 
  65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,222,62,1,0,1,1,15,1,222,62,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,222,62,1,0,1,15,1,222,62,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,222,62,1,0,1,2,21,0,139,0,0,0, 
  0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128,14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,110,63,1,0,1,1,15, 
  1,110,63,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,110,63,1,0,1,15,1,110,63,1,0,17,1,16,86,0,0,1,19,33,0,0,0, 
  78,0,0,0,1,0,17,1,110,63,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128,14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,250,63,1,0,1,1,15,1,250,63,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,250,63,1, 
  0,1,15,1,250,63,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,250,63,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128, 
  14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,134,64,1,0,1,1,15,1,134,64,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,134,64,1,0,1,15,1,134,64,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,134,64,1,0,1,2,21,0,139,0,0,0, 
  0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128,14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,18,65,1,0,1,1,15, 
  1,18,65,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,18,65,1,0,1,15,1,18,65,1,0,17,1,16,86,0,0,1,19,33,0,0,0, 
  78,0,0,0,1,0,17,1,18,65,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128,14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,158,65,1,0,1,1,15,1,158,65,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,158,65,1, 
  0,1,15,1,158,65,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,158,65,1,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128, 
  14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,42,66,1,0,1,1,15,1,42,66,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,42,66,1,0,1,15,1,42,66,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,42,66,1,0,1,2,21,0,139,0,0,0, 
  0,0,0,0,7,0,0,0,2,0,0,0,18,0,130,129,33,8,194,128,14,112,129,127,79,96,3,128,57,112,66,128,65,208,2,128,82,200,3,128,19,82,0,0,0,218,0,0,0,1,0,17,1,182,66,1,0,1,1,15, 
  1,182,66,1,0,17,1,52,85,0,0,1,19,18,0,0,0,39,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,182,66,1,0,1,15,1,182,66,1,0,17,1,16,86,0,0,1,19,33,0,0,0, 
  78,0,0,0,1,0,17,1,182,66,1,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127,82,40,5,128,65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128, 
  23,104,130,127,79,192,68,128,95,184,5,128,1,19,82,0,0,0,218,0,0,0,1,0,17,1,66,67,1,0,1,15,1,66,67,1,0,17,1,248,84,0,0,1,15,1,66,67,1,0,17,1,52,85,0,0,1,19,71,0, 
  0,0,185,0,0,0,1,0,17,1,66,67,1,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,66,67,1,0,1,15,1,66,67,1,0,17,1,48,95,0,0,1,15,1,66,67,1,0,17,1,16,86,0,0,1,19, 
  33,0,0,0,78,0,0,0,1,0,17,1,66,67,1,0,1,19,4,0,0,0,12,0,0,0,3,0,1,2,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0,4,130, 
  83,16,2,130,124,24,17,128,5,24,209,128,6,24,145,126,127,80,16,128,117,160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0, 
  15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43, 
  0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1, 
  54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0, 
  15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43, 
  0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0, 
  0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43, 
  0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79, 
  0,0,0,1,0,17,1,147,44,0,0,1,15,1,2,153,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194, 
  128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,43,70,1,0,1,1,15,1,43,70,1,0,17,1,52,85,0,0, 
  1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,43,70,1,0,1,15,1,43,70,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,43,70,1, 
  0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0, 
  0,0,1,0,17,1,187,70,1,0,1,1,15,1,187,70,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,187,70,1,0,1,15,1,187,70,1, 
  0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,187,70,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2, 
  128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,75,71,1,0,1,1,15,1,75,71,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0, 
  1,19,82,0,0,0,219,0,0,0,1,0,17,1,75,71,1,0,1,15,1,75,71,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,75,71,1,0,1,2,21,0,143,0,0,0,0,0,0, 
  0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,219,71,1,0,1,1, 
  15,1,219,71,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,219,71,1,0,1,15,1,219,71,1,0,17,1,16,86,0,0,1,19,33,0,0, 
  0,78,0,0,0,1,0,17,1,219,71,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1, 
  128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,107,72,1,0,1,1,15,1,107,72,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0, 
  17,1,107,72,1,0,1,15,1,107,72,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,107,72,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194, 
  128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,251,72,1,0,1,1,15,1,251,72,1,0,17,1,52,85,0,0, 
  1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,251,72,1,0,1,15,1,251,72,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,251,72,1, 
  0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0, 
  0,0,1,0,17,1,139,73,1,0,1,1,15,1,139,73,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,139,73,1,0,1,15,1,139,73,1, 
  0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,139,73,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2, 
  128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,27,74,1,0,1,1,15,1,27,74,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0, 
  1,19,82,0,0,0,219,0,0,0,1,0,17,1,27,74,1,0,1,15,1,27,74,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,27,74,1,0,1,2,21,1,152,1,0,0,52,35,2, 
  0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12, 
  128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4, 
  15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1, 
  166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0, 
  1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,198,153,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32, 
  5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1, 
  4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,86,154,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83, 
  48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1, 
  0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0, 
  0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,230,154,1,0,4,19,70, 
  0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128, 
  6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17, 
  1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1, 
  210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0, 
  0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0, 
  19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,118,155,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1, 
  116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7, 
  128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4, 
  17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0, 
  0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,6,156,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1, 
  0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184, 
  12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1, 
  222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178, 
  0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0, 
  0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,150,156,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3, 
  0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1, 
  59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0, 
  17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,38,157,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130, 
  98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0, 
  14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,182,157, 
  1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0,4,130,83,16,2,130,124,24,17, 
  128,5,24,209,128,6,24,145,126,127,80,16,128,117,160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66, 
  44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0, 
  0,4,17,1,166,48,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0, 
  0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0, 
  0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1, 
  147,44,0,0,1,15,1,70,158,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232, 
  3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,152,89,1,0,1,1,15,1,152,89,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183, 
  0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,152,89,1,0,1,15,1,152,89,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,152,89,1,0,1,2,21,0,143,0, 
  0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,40, 
  90,1,0,1,1,15,1,40,90,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,40,90,1,0,1,15,1,40,90,1,0,17,1,16,86,0,0, 
  1,19,33,0,0,0,78,0,0,0,1,0,17,1,40,90,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128, 
  3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,184,90,1,0,1,1,15,1,184,90,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219, 
  0,0,0,1,0,17,1,184,90,1,0,1,15,1,184,90,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,184,90,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0, 
  0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,72,91,1,0,1,1,15,1,72,91,1,0,17, 
  1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,72,91,1,0,1,15,1,72,91,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0, 
  17,1,72,91,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82, 
  0,0,0,218,0,0,0,1,0,17,1,216,91,1,0,1,1,15,1,216,91,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,216,91,1,0,1, 
  15,1,216,91,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,216,91,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232, 
  3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,104,92,1,0,1,1,15,1,104,92,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183, 
  0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,104,92,1,0,1,15,1,104,92,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,104,92,1,0,1,2,21,0,143,0, 
  0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,248, 
  92,1,0,1,1,15,1,248,92,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,248,92,1,0,1,15,1,248,92,1,0,17,1,16,86,0,0, 
  1,19,33,0,0,0,78,0,0,0,1,0,17,1,248,92,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128, 
  3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,136,93,1,0,1,1,15,1,136,93,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219, 
  0,0,0,1,0,17,1,136,93,1,0,1,15,1,136,93,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,136,93,1,0,1,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,10,159,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,154,159,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,42,160,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,186,160,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240, 
  9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1, 
  15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0, 
  19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,74,161,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13, 
  0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1, 
  74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0, 
  17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,218,161,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128, 
  105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19, 
  97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1, 
  15,1,106,162,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2, 
  130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19, 
  23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0, 
  0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11, 
  0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43, 
  0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,250,162,1,0,4,19,70,0,0, 
  0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0,4,130,83,16,2,130,124,24,17,128,5,24,209,128,6,24, 
  145,126,127,80,16,128,117,160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1, 
  147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1, 
  147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0, 
  0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1, 
  0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0, 
  15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1, 
  138,163,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71, 
  32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,5,109,1,0,1,1,15,1,5,109,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19, 
  82,0,0,0,219,0,0,0,1,0,17,1,5,109,1,0,1,15,1,5,109,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,5,109,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,149,109,1,0,1,1,15,1, 
  149,109,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,149,109,1,0,1,15,1,149,109,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78, 
  0,0,0,1,0,17,1,149,109,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23, 
  32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,37,110,1,0,1,1,15,1,37,110,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1, 
  37,110,1,0,1,15,1,37,110,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,37,110,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33, 
  40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,181,110,1,0,1,1,15,1,181,110,1,0,17,1,52,85,0,0,1,19, 
  71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,181,110,1,0,1,15,1,181,110,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,181,110,1,0,1, 
  2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0, 
  1,0,17,1,69,111,1,0,1,1,15,1,69,111,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,69,111,1,0,1,15,1,69,111,1,0,17, 
  1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,69,111,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71, 
  32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,213,111,1,0,1,1,15,1,213,111,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19, 
  82,0,0,0,219,0,0,0,1,0,17,1,213,111,1,0,1,15,1,213,111,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,213,111,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,101,112,1,0,1,1,15,1, 
  101,112,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,101,112,1,0,1,15,1,101,112,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78, 
  0,0,0,1,0,17,1,101,112,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23, 
  32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,245,112,1,0,1,1,15,1,245,112,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1, 
  245,112,1,0,1,15,1,245,112,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,245,112,1,0,1,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105, 
  80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15, 
  1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97, 
  0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15, 
  1,78,164,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130, 
  100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0, 
  15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23, 
  0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0, 
  0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0, 
  0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,222,164,1,0,4,19,70,0,0,0, 
  182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140, 
  126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0, 
  0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,110,165,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0, 
  0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240, 
  7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246, 
  47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0, 
  0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,254,165,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52, 
  35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124, 
  184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0, 
  0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4, 
  17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0, 
  0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,142,166,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0, 
  102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0, 
  0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130, 
  26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147, 
  44,0,0,1,15,1,30,167,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3, 
  130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0, 
  0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3, 
  0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0, 
  0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,174,167,1,0,4, 
  19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184, 
  204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0, 
  4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0, 
  0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27, 
  0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0, 
  1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,62,168,1,0,4,19,70,0,0,0,182,0,0,0,1,0, 
  17,1,116,29,0,0,1,8,2,21,1,36,2,0,0,198,246,1,0,12,0,0,0,3,0,0,0,102,192,5,128,105,176,7,130,98,0,4,130,83,16,2,130,124,24,17,128,5,24,209,128,6,24,145,126,127,80,16,128,117, 
  160,9,128,121,208,10,128,122,144,13,128,123,24,17,128,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,4,17,1,59,45,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0, 
  0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,154,48,0,0,4,17,1,166,48,0,0,1,15,1,210,45,1, 
  0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83, 
  0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,45,1,0,15,1,12,43,0,0,15,1,77,43,0,0,15,1,152,43,0,0,15,1,54,44,0,0,15,1,66,44,0,0,4, 
  19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,206,168,1,0,4,19,70, 
  0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,114,128,1,0,1,1,15,1,114,128,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,114,128,1,0,1,15,1,114,128,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,114,128,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,2,129,1,0,1,1,15,1,2,129,1,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,2,129,1,0,1,15,1,2,129,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  2,129,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,146,129,1,0,1,1,15,1,146,129,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,146,129,1,0,1,15,1, 
  146,129,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,146,129,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128, 
  65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,34,130,1,0,1,1,15,1,34,130,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,34,130,1,0,1,15,1,34,130,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,34,130,1,0,1,2,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,178,130,1, 
  0,1,1,15,1,178,130,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,178,130,1,0,1,15,1,178,130,1,0,17,1,16,86,0,0,1,19, 
  33,0,0,0,78,0,0,0,1,0,17,1,178,130,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,66,131,1,0,1,1,15,1,66,131,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,66,131,1,0,1,15,1,66,131,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,66,131,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,210,131,1,0,1,1,15,1,210,131,1,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,210,131,1,0,1,15,1,210,131,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  210,131,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,98,132,1,0,1,1,15,1,98,132,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,98,132,1,0,1,15,1, 
  98,132,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,98,132,1,0,1,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130, 
  83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1, 
  210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0, 
  1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0, 
  0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,146,169,1,0,4,19, 
  70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204, 
  128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4, 
  17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0, 
  2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0, 
  0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1, 
  0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,34,170,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17, 
  1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128, 
  7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0, 
  4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0, 
  0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1, 
  0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,178,170,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152, 
  1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123, 
  184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147, 
  44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25, 
  178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0, 
  0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1, 
  0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,66,171,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0, 
  3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178, 
  0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17, 
  1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0, 
  0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147, 
  44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1, 
  0,17,1,147,44,0,0,1,15,1,210,171,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6, 
  130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147, 
  44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0, 
  0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13, 
  178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0, 
  0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,98, 
  172,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96, 
  4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1, 
  222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0, 
  0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1, 
  147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0, 
  1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0, 
  101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,242,172,1,0,4,19,70,0,0,0,182,0, 
  0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127, 
  240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13, 
  178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15, 
  1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0, 
  1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0, 
  66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,130,173,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1, 
  8,2,21,1,75,0,0,0,127,47,2,0,6,0,0,0,2,0,0,0,124,80,2,128,5,80,2,128,6,80,2,128,51,80,65,128,119,144,65,128,123,80,2,128,4,17,1,18,174,1,0,1,4,19,21,0,0,0,44,0, 
  0,0,1,0,19,94,0,0,0,255,0,0,0,6,0,1,8,2,21,1,63,0,0,0,19,7,2,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6,240,1,128,119,48,65,128,123,240,1,128,4,19,21,0, 
  0,0,44,0,0,0,1,0,19,94,0,0,0,254,0,0,0,6,0,1,8,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1,128, 
  4,19,86,0,0,0,236,0,0,0,3,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,91,0,0,0,22,0,0,0,15,1,123,146,1,0,17,1,66,174,1,0,1,2,19,91,0,0,0,249,0,0,0, 
  1,0,1,21,1,47,0,0,0,13,238,1,0,5,0,0,0,2,0,0,0,84,48,1,129,5,112,1,128,6,112,1,128,123,112,1,128,124,112,1,128,4,17,1,133,174,1,0,1,8,2,19,91,0,0,0,248,0,0,0, 
  1,0,1,21,1,47,0,0,0,13,238,1,0,5,0,0,0,2,0,0,0,84,48,1,129,5,112,1,128,6,112,1,128,123,112,1,128,124,112,1,128,4,17,1,193,176,1,0,1,8,2,21,1,59,2,0,0,92,249,1, 
  0,19,0,0,0,4,0,0,0,48,240,4,128,49,184,5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,208,145,127,6,208,17,128,55,152,72,130,40,240,3,128,57,248,9,128,69,8,13,128,123,208,17,128,60,248,138, 
  129,77,24,15,128,62,192,203,128,79,96,17,128,71,80,14,128,78,96,16,128,124,208,17,128,15,1,252,176,1,0,15,1,233,63,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,252, 
  176,1,0,15,1,87,65,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,252,176,1,0,4,19,2,0,0,0,6,0,0,0,1,0,17,1,144,66,0,0,1,15,1,252,176,1,0, 
  15,1,16,68,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,252,176,1,0,4,19,2,0,0,0,7,0,0,0,1,0,17,1,14,69,0,0, 
  1,15,1,252,176,1,0,4,19,2,0,0,0,8,0,0,0,1,0,17,1,142,70,0,0,1,15,1,252,176,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72, 
  0,0,4,17,1,140,72,0,0,1,15,1,252,176,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,15,1,252,176,1,0,4,19,2,0,0,0,10,0,0,0,1,0, 
  17,1,74,73,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,252,176,1,0,1,4,19,67,0,0,0,177,0,0,0,1,0, 
  19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,252,176,1,0,1,15,1,252,176,1,0,4,19,2,0,0,0,9,0,0,0,1,0,17,1,202,74,0,0,1,4,19,67,0,0,0, 
  178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,252,176,1,0,1,15,1,252,176,1,0,15,1,74,76,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4, 
  17,1,72,77,0,0,1,4,15,1,49,177,1,0,17,1,25,78,0,0,1,8,2,19,91,0,0,0,247,0,0,0,1,0,1,21,1,47,0,0,0,82,72,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1, 
  128,6,112,1,128,87,48,65,128,123,112,1,128,4,17,1,194,177,1,0,1,8,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1, 
  128,4,19,38,0,0,0,92,0,0,0,3,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,53,0,0,0,22,0,0,0,15,1,196,149,1,0,17,1,242,177,1,0,1,2,19,53,0,0,0,127,0,0, 
  0,1,0,1,19,53,0,0,0,126,0,0,0,1,0,1,19,53,0,0,0,125,0,0,0,1,0,1,21,1,47,0,0,0,82,72,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,1,128,6,112,1,128,87,48, 
  65,128,123,112,1,128,4,17,1,53,178,1,0,1,8,2,21,0,78,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,52,160,1,128,65,168,1,128,14,16,65,128,82,56,2,128,19,82,0,0,0,218,0,0,0,1, 
  0,17,1,60,150,1,0,1,1,19,82,0,0,0,219,0,0,0,1,0,17,1,60,150,1,0,1,17,1,12,150,1,0,1,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1, 
  128,6,152,1,128,123,152,1,128,124,152,1,128,4,19,81,0,0,0,217,0,0,0,3,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,24,0,0,0,22,0,0,0,15,1,192,150,1,0,17,1,101,178, 
  1,0,1,2,19,24,0,0,0,54,0,0,0,1,0,1,19,24,0,0,0,55,0,0,0,1,0,1,19,24,0,0,0,53,0,0,0,1,0,1,21,1,47,0,0,0,82,72,2,0,5,0,0,0,2,0,0,0,124,112, 
  1,128,5,112,1,128,6,112,1,128,87,48,65,128,123,112,1,128,4,17,1,168,178,1,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,83,0,0,0,22,0,0,0,15,1,56,151,1,0,17,1,216,178, 
  1,0,1,2,19,90,0,0,0,242,0,0,0,3,0,1,21,1,143,0,0,0,106,16,2,0,13,0,0,0,3,0,0,0,102,240,2,128,105,48,3,130,98,112,2,130,83,48,2,130,100,176,2,130,5,112,196,128,6,112, 
  132,126,127,48,4,128,117,112,3,128,121,176,3,128,122,240,3,128,123,112,4,128,124,112,4,128,12,17,1,15,179,1,0,1,12,17,1,168,180,1,0,1,12,17,1,65,182,1,0,1,12,17,1,218,183,1,0,1,12,17, 
  1,115,185,1,0,1,12,17,1,12,187,1,0,1,12,17,1,165,188,1,0,1,12,17,1,62,190,1,0,1,6,17,1,215,191,1,0,1,10,12,19,23,0,0,0,49,0,0,0,1,0,1,20,4,30,0,0,0,0,0, 
  0,0,1,0,0,0,59,0,0,0,22,0,0,0,5,17,1,179,192,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,195,0,0,0,0,0,0,0, 
  10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127,82,40,5,128,65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128,23,104,130,127,79,192,68,128,95,184,5,128,1,19,82,0,0,0,218,0,0,0,1,0, 
  17,1,62,152,1,0,1,15,1,62,152,1,0,17,1,248,84,0,0,1,15,1,62,152,1,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,62,152,1,0,1,19,82,0,0,0,219,0,0,0, 
  1,0,17,1,62,152,1,0,1,15,1,62,152,1,0,17,1,217,149,0,0,1,15,1,62,152,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,62,152,1,0,1,19,4,0,0,0,12,0, 
  0,0,3,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127,82,40,5,128,65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128,23,104,130,127,79,192,68,128, 
  95,184,5,128,1,19,82,0,0,0,218,0,0,0,1,0,17,1,2,153,1,0,1,15,1,2,153,1,0,17,1,248,84,0,0,1,15,1,2,153,1,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0, 
  17,1,2,153,1,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,2,153,1,0,1,15,1,2,153,1,0,17,1,72,168,0,0,1,15,1,2,153,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,2,153,1,0,1,19,4,0,0,0,12,0,0,0,3,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128, 
  79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,198,153,1,0,1,1,15,1,198,153,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0, 
  0,219,0,0,0,1,0,17,1,198,153,1,0,1,15,1,198,153,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,198,153,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,86,154,1,0,1,1,15,1,86,154,1, 
  0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,86,154,1,0,1,15,1,86,154,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,86,154,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,230,154,1,0,1,1,15,1,230,154,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,230,154,1, 
  0,1,15,1,230,154,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,230,154,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127, 
  82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,118,155,1,0,1,1,15,1,118,155,1,0,17,1,52,85,0,0,1,19,71,0,0, 
  0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,118,155,1,0,1,15,1,118,155,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,118,155,1,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17, 
  1,6,156,1,0,1,1,15,1,6,156,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,6,156,1,0,1,15,1,6,156,1,0,17,1,16,86, 
  0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,6,156,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128, 
  79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,150,156,1,0,1,1,15,1,150,156,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0, 
  0,219,0,0,0,1,0,17,1,150,156,1,0,1,15,1,150,156,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,150,156,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,38,157,1,0,1,1,15,1,38,157,1, 
  0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,38,157,1,0,1,15,1,38,157,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,38,157,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,182,157,1,0,1,1,15,1,182,157,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,182,157,1, 
  0,1,15,1,182,157,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,182,157,1,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127, 
  82,40,5,128,65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128,23,104,130,127,79,192,68,128,95,184,5,128,1,19,82,0,0,0,218,0,0,0,1,0,17,1,70,158,1,0,1,15,1,70,158,1,0,17,1,248, 
  84,0,0,1,15,1,70,158,1,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,70,158,1,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,70,158,1,0,1,15,1,70,158,1,0,17, 
  1,32,169,0,0,1,15,1,70,158,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,70,158,1,0,1,19,4,0,0,0,12,0,0,0,3,0,1,2,21,0,143,0,0,0,0,0,0,0, 
  8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,10,159,1,0,1,1,15, 
  1,10,159,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,10,159,1,0,1,15,1,10,159,1,0,17,1,16,86,0,0,1,19,33,0,0,0, 
  78,0,0,0,1,0,17,1,10,159,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128, 
  23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,154,159,1,0,1,1,15,1,154,159,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17, 
  1,154,159,1,0,1,15,1,154,159,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,154,159,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128, 
  33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,42,160,1,0,1,1,15,1,42,160,1,0,17,1,52,85,0,0,1, 
  19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,42,160,1,0,1,15,1,42,160,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,42,160,1,0, 
  1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0, 
  0,1,0,17,1,186,160,1,0,1,1,15,1,186,160,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,186,160,1,0,1,15,1,186,160,1,0, 
  17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,186,160,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128, 
  71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,74,161,1,0,1,1,15,1,74,161,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1, 
  19,82,0,0,0,219,0,0,0,1,0,17,1,74,161,1,0,1,15,1,74,161,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,74,161,1,0,1,2,21,0,143,0,0,0,0,0,0,0, 
  8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,218,161,1,0,1,1,15, 
  1,218,161,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,218,161,1,0,1,15,1,218,161,1,0,17,1,16,86,0,0,1,19,33,0,0,0, 
  78,0,0,0,1,0,17,1,218,161,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128, 
  23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,106,162,1,0,1,1,15,1,106,162,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17, 
  1,106,162,1,0,1,15,1,106,162,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,106,162,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128, 
  33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,250,162,1,0,1,1,15,1,250,162,1,0,17,1,52,85,0,0,1, 
  19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,250,162,1,0,1,15,1,250,162,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,250,162,1,0, 
  1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127,82,40,5,128,65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128,23,104,130,127,79,192,68,128,95,184,5,128, 
  1,19,82,0,0,0,218,0,0,0,1,0,17,1,138,163,1,0,1,15,1,138,163,1,0,17,1,248,84,0,0,1,15,1,138,163,1,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,138,163, 
  1,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,138,163,1,0,1,15,1,138,163,1,0,17,1,248,169,0,0,1,15,1,138,163,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  138,163,1,0,1,19,4,0,0,0,12,0,0,0,3,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,78,164,1,0,1,1,15,1,78,164,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,78,164,1,0,1,15,1,78,164,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,78,164,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,222,164,1,0,1,1,15,1,222,164,1,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,222,164,1,0,1,15,1,222,164,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  222,164,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,110,165,1,0,1,1,15,1,110,165,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,110,165,1,0,1,15,1, 
  110,165,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,110,165,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128, 
  65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,254,165,1,0,1,1,15,1,254,165,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0, 
  0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,254,165,1,0,1,15,1,254,165,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,254,165,1,0,1,2,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,142,166,1, 
  0,1,1,15,1,142,166,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,142,166,1,0,1,15,1,142,166,1,0,17,1,16,86,0,0,1,19, 
  33,0,0,0,78,0,0,0,1,0,17,1,142,166,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128, 
  14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,30,167,1,0,1,1,15,1,30,167,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0, 
  0,1,0,17,1,30,167,1,0,1,15,1,30,167,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,30,167,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,174,167,1,0,1,1,15,1,174,167,1,0,17,1,52, 
  85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,174,167,1,0,1,15,1,174,167,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1, 
  174,167,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0, 
  0,218,0,0,0,1,0,17,1,62,168,1,0,1,1,15,1,62,168,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,62,168,1,0,1,15,1, 
  62,168,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,62,168,1,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,57,56,195,128,33,208,194,127,82,40,5,128, 
  65,200,3,128,4,208,1,128,71,88,196,128,14,216,1,128,23,104,130,127,79,192,68,128,95,184,5,128,1,19,82,0,0,0,218,0,0,0,1,0,17,1,206,168,1,0,1,15,1,206,168,1,0,17,1,248,84,0,0,1, 
  15,1,206,168,1,0,17,1,52,85,0,0,1,19,71,0,0,0,185,0,0,0,1,0,17,1,206,168,1,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,206,168,1,0,1,15,1,206,168,1,0,17,1,208,170,0, 
  0,1,15,1,206,168,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,206,168,1,0,1,19,4,0,0,0,12,0,0,0,3,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,146,169,1,0,1,1,15,1,146,169,1, 
  0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,146,169,1,0,1,15,1,146,169,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,146,169,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,34,170,1,0,1,1,15,1,34,170,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,34,170,1, 
  0,1,15,1,34,170,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,34,170,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127, 
  82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,178,170,1,0,1,1,15,1,178,170,1,0,17,1,52,85,0,0,1,19,71,0,0, 
  0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,178,170,1,0,1,15,1,178,170,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,178,170,1,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17, 
  1,66,171,1,0,1,1,15,1,66,171,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,66,171,1,0,1,15,1,66,171,1,0,17,1,16,86, 
  0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,66,171,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128, 
  79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,210,171,1,0,1,1,15,1,210,171,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0, 
  0,219,0,0,0,1,0,17,1,210,171,1,0,1,15,1,210,171,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,210,171,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,98,172,1,0,1,1,15,1,98,172,1, 
  0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,98,172,1,0,1,15,1,98,172,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,98,172,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,242,172,1,0,1,1,15,1,242,172,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,242,172,1, 
  0,1,15,1,242,172,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,242,172,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127, 
  82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,130,173,1,0,1,1,15,1,130,173,1,0,17,1,52,85,0,0,1,19,71,0,0, 
  0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,130,173,1,0,1,15,1,130,173,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,130,173,1,0,1,2,21,1, 
  47,0,0,0,29,61,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,61,48,1,128,4,17,1,253,194,1,0,1,8,2,21,1,59,0,0,0,212,76,2,0,8,0,0,0, 
  3,0,0,0,84,144,193,129,102,144,1,128,58,144,1,128,123,208,1,128,44,144,1,127,5,208,1,128,6,208,193,126,124,208,1,128,12,17,1,61,195,1,0,1,10,12,17,1,141,38,0,0,1,21,1,59,2,0,0,92, 
  249,1,0,19,0,0,0,4,0,0,0,48,240,4,128,49,184,5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,208,145,127,6,208,17,128,55,152,72,130,40,240,3,128,57,248,9,128,69,8,13,128,123,208,17,128,60, 
  248,138,129,77,24,15,128,62,192,203,128,79,96,17,128,71,80,14,128,78,96,16,128,124,208,17,128,15,1,169,195,1,0,15,1,233,63,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15, 
  1,169,195,1,0,15,1,87,65,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,169,195,1,0,4,19,2,0,0,0,6,0,0,0,1,0,17,1,144,66,0,0,1,15,1,169,195, 
  1,0,15,1,16,68,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,169,195,1,0,4,19,2,0,0,0,7,0,0,0,1,0,17,1,14,69, 
  0,0,1,15,1,169,195,1,0,4,19,2,0,0,0,8,0,0,0,1,0,17,1,142,70,0,0,1,15,1,169,195,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1, 
  128,72,0,0,4,17,1,140,72,0,0,1,15,1,169,195,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,15,1,169,195,1,0,4,19,2,0,0,0,10,0,0,0, 
  1,0,17,1,74,73,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,169,195,1,0,1,4,19,67,0,0,0,177,0,0,0, 
  1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,169,195,1,0,1,15,1,169,195,1,0,4,19,2,0,0,0,9,0,0,0,1,0,17,1,202,74,0,0,1,4,19,67,0, 
  0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,169,195,1,0,1,15,1,169,195,1,0,15,1,74,76,0,0,15,1,231,64,0,0,15,1,11,65,0, 
  0,4,17,1,72,77,0,0,1,4,15,1,222,195,1,0,17,1,25,78,0,0,1,8,2,21,1,58,0,0,0,66,77,2,0,5,0,0,0,2,0,0,0,124,200,1,128,5,200,1,128,6,200,129,128,123,200,1,128,122, 
  48,1,128,4,19,43,0,0,0,101,0,0,0,1,0,17,1,111,196,1,0,1,8,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152, 
  1,128,4,19,89,0,0,0,241,0,0,0,3,0,1,8,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,17,56,194,128,13,168,193,127,10,112,129,128,29,200,130,128,30,88,3,128,77,232,67,128,89, 
  120,4,128,17,1,252,176,1,0,1,19,77,0,0,0,202,0,0,0,1,0,17,1,49,177,1,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,49,177,1,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,49, 
  177,1,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,49,177,1,0,1,19,10,0,0,0,25,0,0,0,1,0,17,1,49,177,1,0,1,1,2,21,1,47,0,0,0,13,238,1,0,5,0,0,0,2,0,0,0, 
  84,48,1,129,5,112,1,128,6,112,1,128,123,112,1,128,124,112,1,128,4,17,1,164,196,1,0,1,8,2,21,1,59,0,0,0,212,76,2,0,8,0,0,0,3,0,0,0,84,144,193,129,102,144,1,128,58,144,1,128, 
  123,208,1,128,44,144,1,127,5,208,1,128,6,208,193,126,124,208,1,128,12,17,1,224,198,1,0,1,10,12,17,1,141,38,0,0,1,21,1,47,0,0,0,13,238,1,0,5,0,0,0,2,0,0,0,84,48,1,129,5, 
  112,1,128,6,112,1,128,123,112,1,128,124,112,1,128,4,17,1,76,199,1,0,1,8,2,21,1,59,0,0,0,212,76,2,0,8,0,0,0,3,0,0,0,84,144,193,129,102,144,1,128,58,144,1,128,123,208,1,128,44, 
  144,1,127,5,208,1,128,6,208,193,126,124,208,1,128,12,17,1,136,201,1,0,1,10,12,17,1,141,38,0,0,1,21,1,47,0,0,0,13,238,1,0,5,0,0,0,2,0,0,0,84,48,1,129,5,112,1,128,6,112, 
  1,128,123,112,1,128,124,112,1,128,4,17,1,244,201,1,0,1,8,2,21,1,47,0,0,0,121,21,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112,1,128,97,48,1,128,12,17, 
  1,48,204,1,0,1,10,12,17,1,141,38,0,0,1,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140, 
  126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44, 
  0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0, 
  0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0, 
  0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,96,204,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0, 
  0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240, 
  7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0, 
  15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246, 
  47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0, 
  0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0, 
  0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,240,204,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52, 
  35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124, 
  184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0, 
  0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4, 
  17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0, 
  0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0, 
  0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,128,205,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0, 
  102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1, 
  66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0, 
  0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130, 
  26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178, 
  0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1, 
  15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147, 
  44,0,0,1,15,1,16,206,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3, 
  130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15, 
  1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0, 
  0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3, 
  0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0, 
  0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,160,206,1,0,4, 
  19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184, 
  204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0, 
  4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0, 
  0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27, 
  0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0, 
  1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,48,207,1,0,4,19,70,0,0,0,182,0,0,0,1,0, 
  17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117, 
  128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13, 
  178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15, 
  1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0, 
  0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36, 
  0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0, 
  1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,192,207,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1, 
  152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128, 
  123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1, 
  147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0, 
  15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1, 
  25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33, 
  0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0, 
  1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,80,208,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,212,0,0,0,232,35,2,0,19,0,0, 
  0,4,0,0,0,98,112,4,128,97,48,4,128,82,48,131,127,83,112,3,131,84,176,131,129,5,152,198,130,6,152,198,129,103,48,5,128,104,48,5,128,105,152,5,128,100,176,4,128,107,216,133,129,28,240,130,129,102,240,4, 
  128,94,240,3,128,115,24,6,128,117,88,6,128,123,152,6,128,124,152,6,128,12,17,1,224,208,1,0,1,12,17,1,121,210,1,0,1,12,17,1,15,179,1,0,1,12,17,1,18,212,1,0,1,12,17,1,171,213,1,0, 
  1,12,17,1,68,215,1,0,1,12,17,1,168,180,1,0,1,12,17,1,65,182,1,0,1,12,17,1,218,183,1,0,1,12,19,23,0,0,0,49,0,0,0,1,0,1,12,17,1,115,185,1,0,1,12,17,1,221,216,1, 
  0,1,12,17,1,118,218,1,0,1,12,17,1,12,187,1,0,1,10,12,17,1,15,220,1,0,1,21,1,73,2,0,0,50,229,1,0,23,0,0,0,4,0,0,0,16,112,3,128,65,64,11,128,34,16,196,131,35,176,196, 
  131,36,80,197,131,5,64,146,130,6,64,210,129,39,8,199,131,72,192,13,128,73,96,14,128,42,168,7,128,59,160,10,128,76,0,15,128,38,240,197,128,46,72,8,128,53,232,136,129,54,136,9,128,66,224,11,128,67,128,12, 
  128,68,32,77,128,100,160,15,128,117,56,16,128,119,208,16,128,15,1,168,221,1,0,15,1,225,5,0,0,4,17,1,237,5,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,68,6,0,0,1,15,1,168, 
  221,1,0,15,1,56,6,0,0,4,17,1,143,6,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,218,6,0,0,1,4,19,22,0,0,0,45,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0, 
  19,42,0,0,0,99,0,0,0,3,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,37,7,0,0,1,15,1,168,221,1,0,15,1,112,7,0,0,4,17,1,124,7,0,0,1,15,1,168,221,1,0,15,1, 
  56,6,0,0,4,17,1,199,7,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,18,8,0,0,1,4,19,22,0,0,0,46,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19,42,0,0,0, 
  99,0,0,0,3,0,1,15,1,168,221,1,0,15,1,93,8,0,0,4,17,1,105,8,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,180,8,0,0,1,15,1,168,221,1,0,15,1,255,8,0,0,4, 
  17,1,11,9,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,69,9,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4,17,1,144,9,0,0,1,15,1,168,221,1,0,15,1,56,6,0,0,4, 
  17,1,219,9,0,0,1,15,1,168,221,1,0,15,1,225,5,0,0,4,17,1,38,10,0,0,1,15,1,168,221,1,0,15,1,225,5,0,0,4,17,1,113,10,0,0,1,15,1,168,221,1,0,15,1,188,10,0,0,17, 
  1,205,37,0,0,1,15,1,168,221,1,0,15,1,56,11,0,0,17,1,25,38,0,0,1,4,19,21,0,0,0,44,0,0,0,1,0,19,22,0,0,0,47,0,0,0,1,0,19,96,0,0,0,9,1,0,0,1,0,19, 
  42,0,0,0,99,0,0,0,3,0,1,8,2,21,1,63,0,0,0,19,7,2,0,5,0,0,0,2,0,0,0,124,240,1,128,5,240,1,128,6,240,1,128,119,48,65,128,123,240,1,128,4,19,21,0,0,0,44,0,0, 
  0,1,0,19,94,0,0,0,253,0,0,0,8,0,1,8,2,21,1,107,0,0,0,34,62,2,0,8,0,0,0,3,0,0,0,84,112,194,129,102,224,2,128,58,0,2,128,123,80,3,128,44,144,1,127,5,80,3,128,6, 
  80,195,126,124,80,3,128,15,1,180,221,1,0,4,17,1,171,146,1,0,1,15,1,192,221,1,0,4,17,1,231,146,1,0,1,15,1,180,221,1,0,4,17,1,23,147,1,0,1,15,1,204,221,1,0,4,17,1,121,42, 
  1,0,1,8,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1,128,4,19,89,0,0,0,240,0,0,0,4,0,1,8,2,21,0, 
  144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,17,56,194,128,13,168,193,127,10,112,129,128,29,200,130,128,30,88,3,128,77,232,67,128,89,120,4,128,17,1,169,195,1,0,1,19,77,0,0,0,202,0,0,0, 
  1,0,17,1,222,195,1,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,222,195,1,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,222,195,1,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,222,195, 
  1,0,1,19,10,0,0,0,25,0,0,0,1,0,17,1,222,195,1,0,1,1,2,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1, 
  128,4,19,88,0,0,0,239,0,0,0,4,0,1,8,2,21,1,59,2,0,0,92,249,1,0,19,0,0,0,4,0,0,0,48,240,4,128,49,184,5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,208,145,127,6,208, 
  17,128,55,152,72,130,40,240,3,128,57,248,9,128,69,8,13,128,123,208,17,128,60,248,138,129,77,24,15,128,62,192,203,128,79,96,17,128,71,80,14,128,78,96,16,128,124,208,17,128,15,1,216,221,1,0,15,1,233,63, 
  0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,216,221,1,0,15,1,87,65,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,216,221,1,0, 
  4,19,2,0,0,0,6,0,0,0,1,0,17,1,144,66,0,0,1,15,1,216,221,1,0,15,1,16,68,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0, 
  1,15,1,216,221,1,0,4,19,2,0,0,0,7,0,0,0,1,0,17,1,14,69,0,0,1,15,1,216,221,1,0,4,19,2,0,0,0,8,0,0,0,1,0,17,1,142,70,0,0,1,15,1,216,221,1,0,15,1,14, 
  72,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,216,221,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4, 
  17,1,241,72,0,0,1,15,1,216,221,1,0,4,19,2,0,0,0,10,0,0,0,1,0,17,1,74,73,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0, 
  25,0,0,0,1,0,17,1,216,221,1,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,216,221,1,0,1,15,1,216,221,1,0, 
  4,19,2,0,0,0,9,0,0,0,1,0,17,1,202,74,0,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,216,221,1,0,1, 
  15,1,216,221,1,0,15,1,74,76,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,13,222,1,0,17,1,25,78,0,0,1,8,2,21,1,107,0,0,0,34,62,2,0,8,0, 
  0,0,3,0,0,0,84,112,194,129,102,224,2,128,58,0,2,128,123,80,3,128,44,144,1,127,5,80,3,128,6,80,195,126,124,80,3,128,15,1,158,222,1,0,4,17,1,171,146,1,0,1,15,1,170,222,1,0,4,17, 
  1,231,146,1,0,1,15,1,158,222,1,0,4,17,1,23,147,1,0,1,15,1,182,222,1,0,4,17,1,237,43,1,0,1,8,2,21,1,59,2,0,0,92,249,1,0,19,0,0,0,4,0,0,0,48,240,4,128,49,184, 
  5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,208,145,127,6,208,17,128,55,152,72,130,40,240,3,128,57,248,9,128,69,8,13,128,123,208,17,128,60,248,138,129,77,24,15,128,62,192,203,128,79,96,17,128,71,80, 
  14,128,78,96,16,128,124,208,17,128,15,1,194,222,1,0,15,1,233,63,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0,0,1,15,1,194,222,1,0,15,1,87,65,0,0,15,1,231,64,0,0, 
  15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,194,222,1,0,4,19,2,0,0,0,6,0,0,0,1,0,17,1,144,66,0,0,1,15,1,194,222,1,0,15,1,16,68,0,0,4,19,94,0,0,0,3,1,0, 
  0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,194,222,1,0,4,19,2,0,0,0,7,0,0,0,1,0,17,1,14,69,0,0,1,15,1,194,222,1,0,4,19,2,0,0,0,8,0, 
  0,0,1,0,17,1,142,70,0,0,1,15,1,194,222,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0,0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,194,222,1, 
  0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,15,1,194,222,1,0,4,19,2,0,0,0,10,0,0,0,1,0,17,1,74,73,0,0,1,4,19,67,0,0,0,176,0, 
  0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,194,222,1,0,1,4,19,67,0,0,0,177,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0, 
  0,25,0,0,0,1,0,17,1,194,222,1,0,1,15,1,194,222,1,0,4,19,2,0,0,0,9,0,0,0,1,0,17,1,202,74,0,0,1,4,19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0, 
  1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,194,222,1,0,1,15,1,194,222,1,0,15,1,74,76,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,247,222,1,0,17, 
  1,25,78,0,0,1,8,2,21,1,107,0,0,0,34,62,2,0,8,0,0,0,3,0,0,0,84,112,194,129,102,224,2,128,58,0,2,128,123,80,3,128,44,144,1,127,5,80,3,128,6,80,195,126,124,80,3,128,15,1, 
  136,223,1,0,4,17,1,171,146,1,0,1,15,1,148,223,1,0,4,17,1,231,146,1,0,1,15,1,136,223,1,0,4,17,1,23,147,1,0,1,15,1,160,223,1,0,4,17,1,250,44,1,0,1,8,2,21,1,59,2, 
  0,0,92,249,1,0,19,0,0,0,4,0,0,0,48,240,4,128,49,184,5,128,50,8,7,128,37,240,194,129,52,208,7,128,5,208,145,127,6,208,17,128,55,152,72,130,40,240,3,128,57,248,9,128,69,8,13,128,123,208, 
  17,128,60,248,138,129,77,24,15,128,62,192,203,128,79,96,17,128,71,80,14,128,78,96,16,128,124,208,17,128,15,1,172,223,1,0,15,1,233,63,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,23,65,0, 
  0,1,15,1,172,223,1,0,15,1,87,65,0,0,15,1,231,64,0,0,15,1,11,65,0,0,4,17,1,85,66,0,0,1,15,1,172,223,1,0,4,19,2,0,0,0,6,0,0,0,1,0,17,1,144,66,0,0,1,15, 
  1,172,223,1,0,15,1,16,68,0,0,4,19,94,0,0,0,3,1,0,0,1,0,19,41,0,0,0,98,0,0,0,1,0,17,1,231,64,0,0,1,15,1,172,223,1,0,4,19,2,0,0,0,7,0,0,0,1,0,17, 
  1,14,69,0,0,1,15,1,172,223,1,0,4,19,2,0,0,0,8,0,0,0,1,0,17,1,142,70,0,0,1,15,1,172,223,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,38,72,0,0,15,1,92,72,0, 
  0,15,1,128,72,0,0,4,17,1,140,72,0,0,1,15,1,172,223,1,0,15,1,14,72,0,0,15,1,26,72,0,0,15,1,229,72,0,0,4,17,1,241,72,0,0,1,15,1,172,223,1,0,4,19,2,0,0,0,10, 
  0,0,0,1,0,17,1,74,73,0,0,1,4,19,67,0,0,0,176,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,172,223,1,0,1,4,19,67,0,0,0,177, 
  0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,172,223,1,0,1,15,1,172,223,1,0,4,19,2,0,0,0,9,0,0,0,1,0,17,1,202,74,0,0,1,4, 
  19,67,0,0,0,178,0,0,0,1,0,19,77,0,0,0,204,0,0,0,1,0,19,10,0,0,0,25,0,0,0,1,0,17,1,172,223,1,0,1,15,1,172,223,1,0,15,1,74,76,0,0,15,1,231,64,0,0,15,1, 
  11,65,0,0,4,17,1,72,77,0,0,1,4,15,1,225,223,1,0,17,1,25,78,0,0,1,8,2,21,1,47,0,0,0,202,34,2,0,5,0,0,0,2,0,0,0,124,112,1,128,5,112,193,128,6,112,1,128,123,112, 
  1,128,97,48,1,128,4,17,1,114,224,1,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144, 
  1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,96,204,1,0,1,1,15,1,96,204,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1, 
  0,17,1,96,204,1,0,1,15,1,96,204,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,96,204,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144, 
  194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,240,204,1,0,1,1,15,1,240,204,1,0,17,1,52,85,0, 
  0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,240,204,1,0,1,15,1,240,204,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,240,204, 
  1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218, 
  0,0,0,1,0,17,1,128,205,1,0,1,1,15,1,128,205,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,128,205,1,0,1,15,1,128,205, 
  1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,128,205,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240, 
  2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,16,206,1,0,1,1,15,1,16,206,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2, 
  0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,16,206,1,0,1,15,1,16,206,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,16,206,1,0,1,2,21,0,143,0,0,0,0,0, 
  0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,160,206,1,0,1, 
  1,15,1,160,206,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,160,206,1,0,1,15,1,160,206,1,0,17,1,16,86,0,0,1,19,33,0, 
  0,0,78,0,0,0,1,0,17,1,160,206,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144, 
  1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,48,207,1,0,1,1,15,1,48,207,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1, 
  0,17,1,48,207,1,0,1,15,1,48,207,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,48,207,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144, 
  194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,192,207,1,0,1,1,15,1,192,207,1,0,17,1,52,85,0, 
  0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,192,207,1,0,1,15,1,192,207,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,192,207, 
  1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218, 
  0,0,0,1,0,17,1,80,208,1,0,1,1,15,1,80,208,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,80,208,1,0,1,15,1,80,208, 
  1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,80,208,1,0,1,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48, 
  2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44, 
  0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0, 
  19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0, 
  11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19, 
  43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,178,224,1,0,4,19,70,0, 
  0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6, 
  184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1, 
  234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0, 
  1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210, 
  44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0, 
  67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19, 
  27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,66,225,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116, 
  29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128, 
  121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44, 
  0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17, 
  1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0, 
  83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19, 
  36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,210,225,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0, 
  0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12, 
  128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222, 
  47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0, 
  0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0, 
  79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19, 
  33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,98,226,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0, 
  0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0, 
  15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59, 
  45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17, 
  1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1, 
  13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0, 
  0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17, 
  1,147,44,0,0,1,15,1,242,226,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98, 
  96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0, 
  0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14, 
  1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4, 
  19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66, 
  44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,130,227,1, 
  0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130, 
  5,184,204,128,6,184,140,126,127,240,11,128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44, 
  0,0,4,17,1,234,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48, 
  0,0,0,2,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44, 
  0,0,15,1,210,44,0,0,15,1,234,47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0, 
  19,27,0,0,0,67,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0, 
  0,0,1,0,19,27,0,0,0,66,0,0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,18,228,1,0,4,19,70,0,0,0,182,0,0,0, 
  1,0,17,1,116,29,0,0,1,8,2,21,1,152,1,0,0,52,35,2,0,13,0,0,0,3,0,0,0,102,32,5,128,105,80,6,130,98,96,3,130,83,48,2,130,100,96,4,130,5,184,204,128,6,184,140,126,127,240,11, 
  128,117,128,7,128,121,240,7,128,122,240,9,128,123,184,12,128,124,184,12,128,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,222,44,0,0,4,17,1,234,44,0,0,1,15, 
  1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,4,17,1,59,45,0,0,1,4,19,97,0,0,0,14,1,0,0,1,0,19,23,0,0,0,48,0,0,0,2,0,1,15,1,13,178,0, 
  0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,222,47,0,0,4,15,1,74,26,0,0,17,1,130,26,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,15,1,147,44,0,0,15,1,210,44,0,0,15,1,234, 
  47,0,0,4,17,1,246,47,0,0,1,15,1,25,178,0,0,4,17,1,166,48,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,3,0,0,0,11,0,0,0,1,0,19,27,0,0,0,67,0,0,0,1,0, 
  19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,13,178,0,0,15,1,66,44,0,0,4,19,43,0,0,0,101,0,0,0,1,0,19,27,0,0,0,66,0, 
  0,0,1,0,19,36,0,0,0,83,0,0,0,1,0,19,33,0,0,0,79,0,0,0,1,0,17,1,147,44,0,0,1,15,1,162,228,1,0,4,19,70,0,0,0,182,0,0,0,1,0,17,1,116,29,0,0,1,8,2, 
  19,42,0,0,0,99,0,0,0,3,0,1,19,91,0,0,0,246,0,0,0,2,0,1,19,91,0,0,0,245,0,0,0,2,0,1,19,91,0,0,0,244,0,0,0,2,0,1,21,1,52,0,0,0,23,5,2,0,5,0, 
  0,0,2,0,0,0,92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1,128,4,19,72,0,0,0,187,0,0,0,6,0,1,8,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,72, 
  232,3,128,13,168,129,128,10,112,193,128,17,56,66,128,29,200,130,128,30,88,3,128,77,240,3,128,17,1,216,221,1,0,1,19,77,0,0,0,202,0,0,0,1,0,17,1,13,222,1,0,1,19,13,0,0,0,32,0,0, 
  0,1,0,17,1,13,222,1,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,13,222,1,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,13,222,1,0,1,1,19,10,0,0,0,25,0,0,0,1,0,17,1, 
  13,222,1,0,1,2,19,53,0,0,0,124,0,0,0,2,0,1,19,53,0,0,0,123,0,0,0,2,0,1,19,53,0,0,0,122,0,0,0,2,0,1,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0, 
  92,48,1,129,5,152,1,128,6,152,1,128,123,152,1,128,124,152,1,128,4,19,52,0,0,0,121,0,0,0,6,0,1,8,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,52,232,3,128,13,168,129, 
  128,10,112,193,128,17,56,66,128,29,200,130,128,30,88,3,128,77,240,3,128,17,1,194,222,1,0,1,19,77,0,0,0,202,0,0,0,1,0,17,1,247,222,1,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,247, 
  222,1,0,1,19,13,0,0,0,31,0,0,0,1,0,17,1,247,222,1,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,247,222,1,0,1,1,19,10,0,0,0,25,0,0,0,1,0,17,1,247,222,1,0,1,2, 
  19,24,0,0,0,51,0,0,0,2,0,1,19,24,0,0,0,52,0,0,0,2,0,1,19,24,0,0,0,50,0,0,0,2,0,1,21,1,52,0,0,0,23,5,2,0,5,0,0,0,2,0,0,0,92,48,1,129,5,152, 
  1,128,6,152,1,128,123,152,1,128,124,152,1,128,4,19,35,0,0,0,82,0,0,0,6,0,1,8,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,17,56,2,129,13,168,193,127,10,112,193,128,35, 
  232,3,128,29,200,130,128,30,88,3,128,77,240,3,128,17,1,172,223,1,0,1,19,77,0,0,0,202,0,0,0,1,0,17,1,225,223,1,0,1,19,13,0,0,0,32,0,0,0,1,0,17,1,225,223,1,0,1,19,13, 
  0,0,0,31,0,0,0,1,0,17,1,225,223,1,0,1,19,13,0,0,0,33,0,0,0,1,0,17,1,225,223,1,0,1,1,19,10,0,0,0,25,0,0,0,1,0,17,1,225,223,1,0,1,2,21,1,63,0,0,0, 
  133,62,2,0,5,0,0,0,2,0,0,0,4,48,1,129,5,240,1,128,6,240,1,128,123,240,1,128,124,240,1,128,4,19,60,0,0,0,149,0,0,0,1,0,19,83,0,0,0,221,0,0,0,3,0,1,8,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17, 
  1,178,224,1,0,1,1,15,1,178,224,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,178,224,1,0,1,15,1,178,224,1,0,17,1,16,86, 
  0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,178,224,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128, 
  79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,66,225,1,0,1,1,15,1,66,225,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0, 
  0,219,0,0,0,1,0,17,1,66,225,1,0,1,15,1,66,225,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,66,225,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,210,225,1,0,1,1,15,1,210,225,1, 
  0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,210,225,1,0,1,15,1,210,225,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,210,225,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127, 
  19,82,0,0,0,218,0,0,0,1,0,17,1,98,226,1,0,1,1,15,1,98,226,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,98,226,1, 
  0,1,15,1,98,226,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,98,226,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127, 
  82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,242,226,1,0,1,1,15,1,242,226,1,0,17,1,52,85,0,0,1,19,71,0,0, 
  0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,242,226,1,0,1,15,1,242,226,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,242,226,1,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17, 
  1,130,227,1,0,1,1,15,1,130,227,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,130,227,1,0,1,15,1,130,227,1,0,17,1,16,86, 
  0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,130,227,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128, 
  79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,18,228,1,0,1,1,15,1,18,228,1,0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0, 
  0,219,0,0,0,1,0,17,1,18,228,1,0,1,15,1,18,228,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0,1,0,17,1,18,228,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,57,144,194,128,33,40,194,127,82,232,3,128,65,240,2,128,71,32,66,128,79,128,3,128,14,144,1,128,23,32,66,127,19,82,0,0,0,218,0,0,0,1,0,17,1,162,228,1,0,1,1,15,1,162,228,1, 
  0,17,1,52,85,0,0,1,19,71,0,0,0,183,0,0,0,2,0,1,19,82,0,0,0,219,0,0,0,1,0,17,1,162,228,1,0,1,15,1,162,228,1,0,17,1,16,86,0,0,1,19,33,0,0,0,78,0,0,0, 
  1,0,17,1,162,228,1,0,1,2,21,4,146,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,2,128,105,80,3,128,10,240,65,129,91,152,66,129,36,96,66,129,109,144,67,129,102,16,3,128,98,208,2,128, 
  115,208,3,128,116,16,4,128,117,80,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,100,0,0,0,1,3,18,117,0,0,0,1,3,17,1,184,231,1,0,1,3,17,1,215,231,1,0,1,3,17,1,14, 
  232,1,0,1,3,17,1,80,232,1,0,1,3,17,1,111,232,1,0,1,3,17,1,142,232,1,0,1,3,17,1,196,232,1,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1, 
  141,231,1,0,3,17,1,147,231,1,0,1,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,15,1,126,234,1,0,3,17,1,161,234,1,0,1,20,2,48,0,0,0,0,0,0,0,2, 
  0,0,0,3,0,0,0,26,0,0,0,34,0,0,0,3,17,1,253,233,1,0,1,15,1,83,234,1,0,3,17,1,89,234,1,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128, 
  47,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,2,21,4,152,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,73,64,4,128,10,16,194, 
  129,35,128,66,129,60,192,3,128,45,0,67,129,78,128,4,128,47,64,3,129,43,192,2,128,58,128,3,128,69,0,4,128,95,0,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3, 
  17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1,0,1,3,17,1,188,235,1,0,1,3,17,1,218,235,1,0,1,3,17,1,60,236,1,0,1,3,17,1,170,236, 
  1,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,68,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,128,45,160,1,128, 
  10,48,1,128,47,224,65,128,95,160,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0, 
  22,0,0,0,3,17,1,12,235,1,0,1,2,18,119,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,235,238,1,0,3,17,1,236,238,1,0,1,1,20,4,30,0, 
  0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,11,239,1,0,1,2,21,4,54,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,54,48,1,128,51,240,64,128,97,112,1,128,3,17,1, 
  42,239,1,0,1,3,17,1,72,239,1,0,1,3,17,1,102,239,1,0,1,2,21,4,65,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,56,208,1,128,49,16,1,128,54,144,1,128,51,80,1,128,3,17,1,133, 
  239,1,0,1,3,17,1,163,239,1,0,1,3,17,1,193,239,1,0,1,3,18,36,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,223,239,1,0,1,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,253,239,1,0,1,2,21,4,53,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,104,1,128,107,240,64,128,111,40,1,128, 
  3,18,73,0,0,0,1,3,17,1,27,240,1,0,1,3,17,1,58,240,1,0,1,2,21,4,65,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,56,208,1,128,49,16,1,128,54,144,1,128,51,80,1,128,3,17, 
  1,89,240,1,0,1,3,17,1,119,240,1,0,1,3,17,1,149,240,1,0,1,3,18,68,0,0,0,1,2,21,4,157,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,105,168,3,128,10,16,66,129, 
  91,184,130,129,36,128,130,129,93,240,194,128,102,104,3,128,98,40,3,128,109,232,195,128,115,40,4,128,116,104,4,128,117,168,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,100,0,0,0,1,3,18,117, 
  0,0,0,1,3,18,90,0,0,0,1,3,17,1,184,231,1,0,1,3,17,1,215,231,1,0,1,3,17,1,14,232,1,0,1,3,17,1,80,232,1,0,1,3,17,1,111,232,1,0,1,3,17,1,142,232,1,0,1,3, 
  17,1,196,232,1,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141,231,1,0,3,17,1,147,231,1,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0, 
  0,0,10,240,64,128,32,40,65,128,116,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,126,242,1,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45, 
  208,192,127,15,1,243,242,1,0,3,17,1,249,242,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,243,242,1,0,3,17,1,249,242,1,0,1,18,120,0, 
  0,0,1,18,126,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,235,238,1,0,3,17,1,74,243,1,0,1,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0, 
  3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,105,243,1,0,1,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,15,1,235,238,1,0,3,17,1,191,243,1,0,1,20,2,27, 
  0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,1,1,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,42,208,0,128,47,16,1,128,3,17,1,249,243,1,0,1,3, 
  17,1,88,244,1,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,154,244,1,0,3,17,1,160,244,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0, 
  0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,106,0, 
  0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,116,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,85,0, 
  0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,108,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,88,96,1,128,45,240,64,128, 
  95,240,0,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,241,244,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17, 
  1,160,244,1,0,1,18,127,0,0,0,1,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,77,192,1,128,45,16,193,127,95,16,1,128,71,128,193,127,15,1,154,244,1,0,3,17,1,160,244,1,0,1, 
  3,17,1,83,245,1,0,1,3,17,1,181,245,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0, 
  0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,65,96,129,128,45,240,192,127,95,240,0,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,23,246,1,0,1,20,2,40,0,0,0, 
  0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,116,0,0,0,0,0,0,0,9,0,0,0,3,0,0,0,32,232, 
  1,128,43,96,2,128,10,176,1,129,35,32,130,127,60,96,3,128,45,160,2,128,58,32,3,128,47,224,66,128,95,160,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,17,1,128, 
  235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1,0,1,3,17,1,188,235,1,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17, 
  1,12,235,1,0,1,2,21,4,79,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,1,129,45,248,1,128,10,80,1,128,47,56,130,128,40,192,1,128,95,248,1,128,3,18,5,0,0,0,1,3,18,6,0, 
  0,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,63, 
  0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,123,192,1,128,10,16,1,128,47,128,129,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,18,84,0,0,0,1, 
  2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128,47,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,194,248,1,0,1,20,2,40,0,0,0,0, 
  0,0,0,2,0,0,0,2,0,0,0,26,0,0,0,33,0,0,0,3,18,2,0,0,0,1,3,18,3,0,0,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,61,192,1,128, 
  10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,50,249,1,0,1,2,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0, 
  0,0,3,17,1,236,238,1,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,229,249,1,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,50,0,0,0, 
  22,0,0,0,3,18,72,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,52,0,0,0,22,0,0,0,3,18,53,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0, 
  22,0,0,0,3,17,1,3,250,1,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,54,0,0,0,22,0,0,0,3,18,46,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,50,0,0, 
  0,22,0,0,0,3,18,35,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,52,0,0,0,22,0,0,0,3,18,67,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,112,0,0, 
  0,22,0,0,0,3,18,66,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,3,18,42,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,107,0,0, 
  0,22,0,0,0,3,17,1,34,250,1,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,117,0,0,0,22,0,0,0,3,17,1,70,250,1,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,54, 
  0,0,0,22,0,0,0,3,18,34,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,50,0,0,0,22,0,0,0,3,18,39,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,52, 
  0,0,0,22,0,0,0,3,18,65,0,0,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,10,208,64,128,32,8,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,20,2,36,0,0, 
  0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141,231,1,0,3,17,1,147,231,1,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,36,128,129,128,10,16, 
  1,128,116,184,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,100,0,0,0,1,3,17,1,100,250,1,0,1,2,21,4,182,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,2,128,41,32, 
  3,128,10,112,130,129,35,224,194,128,44,152,131,129,45,208,131,129,43,88,195,128,47,16,68,129,58,80,4,128,59,144,4,128,60,200,196,128,93,8,197,128,95,208,3,128,124,64,5,128,125,120,5,128,3,18,5,0,0,0, 
  1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,18,87,0,0,0,1,3,17,1,142,250,1,0,1,3,18,93,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1, 
  0,1,3,18,113,0,0,0,1,3,17,1,188,235,1,0,1,3,18,90,0,0,0,1,3,18,97,0,0,0,1,3,18,30,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0, 
  3,17,1,12,235,1,0,1,2,21,4,56,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,45,128,1,128,10,16,1,128,95,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1, 
  12,235,1,0,1,20,2,48,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,34,0,0,0,3,17,1,12,235,1,0,1,15,1,141,231,1,0,3,17,1,147,231,1,0,1,2,20,4,29,0,0,0, 
  0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,3,18,41,0,0,0,1,2,21,4,56,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,45,128,1,128,10,16,1,128,95,128,1,128,3,18, 
  5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,12,235,1,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,18,120,0,0,0,1,21,4,40, 
  0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,235,238,1,0,3,17,1,20,251,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26, 
  0,0,0,15,1,235,238,1,0,3,17,1,20,251,1,0,1,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,3,17,1,74,243,1,0,1,1,21,4,40,0,0,0,0,0,0,0,2, 
  0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,243,242,1,0,3,17,1,89,251,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,243,242,1, 
  0,3,17,1,89,251,1,0,1,18,120,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,3,17,1,191,243,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,1,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,15,1,27,252,1,0,3,17,1,33,252,1,0,1,20,2,52,0,0,0,0,0,0,0,5, 
  0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,170,251,1,0,3,17,1,207,251,1,0,1,17,1,58,252,1,0,1,20,2,59,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,52,0,0,0,38,0,0,0,15,1,141,252,1,0,3,17,1,171,252,1,0,1,3,18,124,0,0,0,1,17,1,220,252,1,0,1,18,127,0,0,0,1, 
  21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,235,238,1,0,3,17,1,24,253,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0, 
  0,0,26,0,0,0,15,1,235,238,1,0,3,17,1,24,253,1,0,1,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,80,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1, 
  24,253,1,0,1,3,17,1,93,253,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21, 
  4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,78,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,179,253,1,0,1,20,2,40,0,0,0,0,0,0, 
  0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,80,96,1,128,45, 
  240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,9,254,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1, 
  0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,77,96,129,128,45,240,192,127,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3, 
  17,1,95,254,1,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,76,0,0,0,0, 
  0,0,0,5,0,0,0,2,0,0,0,32,104,1,128,65,224,193,128,10,48,1,128,47,160,1,128,97,32,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,14,255,1,0, 
  1,3,17,1,44,255,1,0,1,2,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,130,129,34,96,2,128,10,240,193,127,91,152,195,129,116,16,4,128,45,24,3,128,40,224,2,128,39,160,66,128, 
  47,88,67,128,95,24,3,128,99,208,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1, 
  225,234,1,0,1,3,18,117,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21, 
  4,207,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,2,128,58,152,4,128,34,32,3,128,99,136,5,128,36,96,67,129,63,216,68,128,95,24,4,128,39,152,3,128,40,216,3,128,116,200,5,128,10,176,194, 
  125,91,16,69,129,124,64,6,128,45,24,4,128,94,72,5,128,47,88,132,125,123,8,6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1, 
  0,1,3,17,1,41,4,2,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,87,4,2,0,1,3,18,82,0,0,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240, 
  255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,100, 
  0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,32,168,129,129,34,224,1,128,10,112,193,127,39,32,66,128,47,96,66,128,99,160,2,128,116,224,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1, 
  74,255,1,0,1,3,17,1,157,255,1,0,1,3,17,1,225,234,1,0,1,3,17,1,218,4,2,0,1,3,17,1,248,4,2,0,1,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,42,208,0,128, 
  47,16,1,128,3,17,1,249,243,1,0,1,3,17,1,88,244,1,0,1,18,2,0,0,0,1,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,62,192,1,128,10,16,193,127,47,128,1,128, 
  3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,18,114,0,0,0,1,2,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,62,16,1,128,33,208,0,128,3,17,1,87, 
  5,2,0,1,3,18,103,0,0,0,1,2,21,4,136,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,2,130,97,128,2,128,10,208,193,129,115,0,4,128,103,0,3,128,109,64,3,128,102,192,2,128,47,64, 
  66,127,112,128,3,128,114,192,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,117,5,2,0,1,3,17,1,160,5,2,0,1,3,17,1,191,5,2,0,1,3,17,1,222, 
  5,2,0,1,3,17,1,253,5,2,0,1,3,17,1,64,6,2,0,1,3,17,1,95,6,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,18,59,0,0,0,1,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,118,8,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,148,8,2,0,1, 
  18,16,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,18,38,0,0,0,1,2,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,111,8,1,128,107,208, 
  192,127,3,18,73,0,0,0,1,3,17,1,27,240,1,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,116,0,0,0,1,18,95,0,0,0,1,21,4,68,0,0,0,0, 
  0,0,0,5,0,0,0,2,0,0,0,32,104,1,129,45,160,1,128,10,48,1,128,95,160,1,128,116,224,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,178,8,2,0, 
  1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17, 
  1,20,251,1,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,20,251,1,0,1,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,15,1,235,238,1,0,3,17,1,32,9,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,235,238,1,0,3,17,1,32,9,2,0, 
  1,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,15,1,27,252,1,0,3,17,1,33,252,1,0,1,2,20,4,23,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0, 
  0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,235,238,1,0,3,17,1,101,9,2,0,1,1,18,123,0,0,0, 
  1,20,4,24,0,0,0,0,0,0,0,1,0,0,0,47,0,0,0,22,0,0,0,3,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,3,17,1,171,9,2,0,1,20,2,52,0, 
  0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,170,251,1,0,3,17,1,207,251,1,0,1,2,20,2,29,0,0,0,0,0,0,0,1, 
  0,0,0,5,0,0,0,22,0,0,0,3,18,124,0,0,0,1,2,21,2,48,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,15,1,235,238,1,0,3, 
  17,1,21,10,2,0,1,1,20,2,59,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,52,0,0,0,38,0,0,0,15,1,141,252,1,0,3,17,1,171,252,1,0,1, 
  3,18,124,0,0,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,24,253,1,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,3,17,1,24,253,1,0,1,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,138,10, 
  2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0, 
  0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,224,10,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10, 
  2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,54,11,2,0,1,20,2,34,0,0, 
  0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,69,48,129,128,45,240,192, 
  127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,140,11,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0, 
  1,21,4,88,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,1,128,69,0,194,128,10,80,193,128,47,192,1,128,73,64,2,128,78,128,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1, 
  225,234,1,0,1,3,17,1,180,11,2,0,1,3,17,1,211,11,2,0,1,3,17,1,254,11,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,83,0,0,0,22,0,0,0,3,18,74,0,0,0,1,2, 
  20,4,29,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,18,80,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,147,12,2,0,1, 
  20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,29,12,2,0,3,17,1,59,12,2,0,1,2,20,4,30,0,0,0,0, 
  0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,92,13,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0, 
  0,15,1,230,12,2,0,3,17,1,4,13,2,0,1,2,21,4,51,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,58,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1, 
  3,18,105,0,0,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0, 
  0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,96,1,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,175,13,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,58,192,1,128,10,16,193, 
  127,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,47,18,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,36,128, 
  1,128,10,16,1,128,47,184,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,100,0,0,0,1,3,17,1,225,234,1,0,1,2,21,4,74,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104, 
  1,128,94,216,1,128,10,48,193,127,63,160,65,128,123,24,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,82,0,0,0,1,3,17,1,129,4,2,0,1,3,18,84,0,0,0,1,2,21,4,52,0,0, 
  0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,40,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,233,19,2,0,1,2,21,4,67,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,32,104,1,129,45,216,1,128,10,48,1,128,95,216,1,128,40,160,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,20,2,30,0,0, 
  0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,112,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,32,200,65,129,97,0,130,129,10,144,1,128,115,0,3,128, 
  116,64,3,128,104,64,2,128,110,192,2,128,105,128,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,106,20,2,0,1,3,17,1,137,20,2,0,1,3,17,1,168,20,2,0,1,3,17,1,198,20,2, 
  0,1,3,17,1,240,20,2,0,1,3,17,1,26,21,2,0,1,2,21,4,127,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,130,129,34,64,2,128,10,208,193,127,99,120,3,128,116,184,3,128,45,248,2, 
  128,40,192,2,128,39,128,66,128,47,56,67,128,95,248,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1, 
  0,1,3,17,1,225,234,1,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,52, 
  0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,58,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,154,22,2,0,1,2,21,4,176,0,0,0,0,0,0,0, 
  14,0,0,0,3,0,0,0,32,136,2,128,65,64,132,130,10,80,194,129,35,192,66,129,60,0,4,128,45,64,67,129,78,0,5,128,47,128,67,129,43,0,3,128,58,192,3,128,69,128,4,128,73,192,132,128,95,64,3,128, 
  97,64,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1,0,1,3,17, 
  1,188,235,1,0,1,3,17,1,184,22,2,0,1,3,17,1,218,235,1,0,1,3,17,1,60,236,1,0,1,3,17,1,170,236,1,0,1,3,17,1,26,23,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0, 
  3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,20,4,40,0,0,0,0,0,0,0,2,0,0,0,42,0,0,0,26,0,0,0,33,0,0,0,3,18,107,0,0,0,1,3,18,115,0,0,0,1,18,102,0, 
  0,0,1,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,58,208,0,128,97,8,1,128,3,18,28,0,0,0,1,3,17,1,124,23,2,0,1,2,21,4,46,0,0,0,0,0,0,0,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,208,192,127,15,1,155,23,2,0,15,1,235,238,1,0,3,17,1,161,23,2,0,1,20,2,42,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,15,1,155,23,2,0,15, 
  1,235,238,1,0,3,17,1,161,23,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,3,18,105,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,107,0, 
  0,0,22,0,0,0,3,17,1,242,23,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,125,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0, 
  0,0,1,3,17,1,225,234,1,0,1,3,18,92,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,104,0,0,0,1,2,21,4,42,0,0,0,0,0,0,0,2, 
  0,0,0,1,0,0,0,115,16,1,128,99,208,192,127,3,17,1,129,24,2,0,1,3,17,1,160,24,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,191,24,2, 
  0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,222,24,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,253, 
  24,2,0,1,2,21,4,66,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,101,80,129,128,97,16,193,127,117,208,1,128,111,144,1,128,3,17,1,28,25,2,0,1,3,17,1,59,25,2,0,1,3,17,1,90,25, 
  2,0,1,3,17,1,120,25,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,151,25,2,0,1,2,21,4,66,0,0,0,0,0,0,0,4,0,0,0,2,0,0, 
  0,104,144,1,128,101,80,1,128,107,208,1,128,99,16,193,127,3,17,1,182,25,2,0,1,3,17,1,213,25,2,0,1,3,17,1,244,25,2,0,1,3,17,1,19,26,2,0,1,2,21,4,112,0,0,0,0,0,0,0, 
  8,0,0,0,3,0,0,0,32,200,1,129,97,64,2,128,10,144,1,128,103,192,2,128,112,64,3,128,109,0,3,128,102,128,2,128,47,0,2,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1, 
  0,1,3,17,1,50,26,2,0,1,3,17,1,160,5,2,0,1,3,17,1,191,5,2,0,1,3,17,1,222,5,2,0,1,3,17,1,81,26,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0, 
  0,10,240,128,128,47,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141, 
  231,1,0,3,17,1,147,231,1,0,1,2,21,4,136,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,2,130,97,128,2,128,10,208,193,129,115,0,4,128,103,0,3,128,109,64,3,128,102,192,2,128,47,64, 
  66,127,112,128,3,128,114,192,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,117,5,2,0,1,3,17,1,160,5,2,0,1,3,17,1,191,5,2,0,1,3,17,1,222, 
  5,2,0,1,3,17,1,81,26,2,0,1,3,17,1,64,6,2,0,1,3,17,1,211,27,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,103,192,1,128,10,16,1,128,47, 
  128,129,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,191,5,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,58,192,1,128, 
  10,16,193,127,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,18,91,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0, 
  3,18,54,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,18,76,0,0,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,107,128,129,128, 
  45,16,1,128,111,192,1,128,95,16,65,127,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,186,28,2,0,1,3,17,1,232,28,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,32,9,2, 
  0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,32,9,2,0,1,1,20,4,23,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,1, 
  20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,101,9,2,0,1,1,21,4,47,0,0,0,0,0,0,0,2,0,0, 
  0,1,0,0,0,42,208,0,128,47,64,1,128,15,1,27,252,1,0,3,17,1,33,252,1,0,1,3,18,123,0,0,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,38,0,0,0,15,1,12,30,2,0,3,17,1,101,9,2,0,1,17,1,170,251,1,0,1,21,2,42,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2, 
  16,193,127,3,16,1,128,3,17,1,21,10,2,0,1,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,20,2,34,0,0,0,0,0,0,0, 
  2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128, 
  3,17,1,64,10,2,0,1,3,17,1,19,30,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0, 
  0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,105,30,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1, 
  3,17,1,191,30,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0, 
  0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,64,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,88,0,0,0,22,0,0,0,3,17,1,21,31,2,0,1,2,21, 
  4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,77,16,1,128,71,208,192,127,3,17,1,52,31,2,0,1,3,17,1,83,31,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,65,0,0,0, 
  22,0,0,0,3,17,1,114,31,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,34,0,0,0,22,0,0,0,3,18,122,0,0,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0, 
  0,34,208,64,128,92,216,0,128,1,3,17,1,227,31,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,235,238, 
  1,0,3,17,1,145,31,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,154,32,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38, 
  0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,54,32,2,0,1,17,1,237,32,2,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,39,0,0,0,22,0,0,0,3,18,121,0, 
  0,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,216,0,128,39,208,0,128,1,3,17,1,110,33,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0, 
  0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,235,238,1,0,3,17,1,28,33,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,37,34,2, 
  0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,193,33,2,0,1,17,1,120,34,2,0,1,21,4,52,0,0, 
  0,0,0,0,0,3,0,0,0,1,0,0,0,58,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,167,34,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0, 
  0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,2,128,41,160,2,128,10, 
  240,129,129,35,96,194,128,60,216,131,129,45,24,3,128,43,216,2,128,47,88,131,128,58,152,3,128,95,24,3,128,124,24,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,18,87, 
  0,0,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1,0,1,3,17,1,188,235,1,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,127,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,2,128,43,128,2,128,10,208,1,129,35,64,130,127,60,128,67,129, 
  45,192,2,128,58,64,3,128,47,0,67,128,95,192,2,128,124,192,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17, 
  1,225,234,1,0,1,3,17,1,10,35,2,0,1,3,17,1,188,235,1,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2, 
  21,4,243,0,0,0,0,0,0,0,20,0,0,0,4,0,0,0,32,72,3,128,58,120,5,128,34,128,3,128,35,192,67,131,36,0,68,131,63,248,5,129,91,48,6,131,39,56,4,128,40,120,4,128,95,248,4,128,10,16, 
  195,125,43,184,196,126,60,184,197,129,45,248,4,128,94,104,6,128,47,56,133,125,99,168,6,128,116,232,6,128,123,40,7,128,124,96,7,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1, 
  3,17,1,98,235,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,17,1,41,4,2,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,10,35, 
  2,0,1,3,17,1,188,235,1,0,1,3,18,82,0,0,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,3,18,97,0, 
  0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,219,0,0,0,0,0,0,0,18,0,0,0,4,0,0,0,32,8,3,128,58,104,5,128, 
  34,64,3,128,35,128,195,130,36,192,3,131,91,232,5,128,95,232,4,128,39,248,3,128,40,56,4,128,41,112,4,128,10,208,194,125,43,168,132,126,60,168,69,129,45,232,4,128,99,32,6,128,47,40,197,125,116,96,6,128, 
  124,160,6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,98,235,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,18,87,0, 
  0,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,10,35,2,0,1,3,17,1,188,235,1,0,1,3,18,117,0,0,0,1,3,17,1,240,255,1,0,1,3,17, 
  1,81,0,2,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,172,0,0,0,0,0,0,0,14,0,0,0,3,0, 
  0,0,32,136,2,130,34,192,2,128,10,80,194,127,99,168,132,130,116,232,4,128,45,120,131,129,94,104,4,128,39,0,131,128,40,64,3,128,47,184,67,128,63,248,131,128,93,48,4,128,95,120,3,128,123,40,5,128,3,18, 
  5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,18,82,0,0,0,1,3,18, 
  29,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17, 
  1,12,235,1,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,124,23,2,0,1,2,21,4,169,0,0,0,0,0,0,0,13,0,0,0,3,0,0,0,32,104,2,128, 
  105,208,3,128,10,48,130,129,91,24,131,129,36,160,130,129,109,16,132,129,102,144,3,128,47,216,2,128,98,80,3,128,115,80,196,128,116,144,4,128,117,208,4,128,123,16,5,128,3,18,5,0,0,0,1,3,18,6,0,0, 
  0,1,3,18,100,0,0,0,1,3,17,1,225,234,1,0,1,3,18,117,0,0,0,1,3,17,1,184,231,1,0,1,3,17,1,215,231,1,0,1,3,17,1,14,232,1,0,1,3,17,1,80,232,1,0,1,3,17,1,111, 
  232,1,0,1,3,17,1,142,232,1,0,1,3,17,1,196,232,1,0,1,3,18,84,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141,231,1,0,3,17,1,147,231, 
  1,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,94,128,1,128,10,16,193,127,123,192,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,129,4,2,0,1, 
  3,18,84,0,0,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,94,184,1,128,10,16,193,127,63,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,82,0, 
  0,0,1,3,17,1,129,4,2,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,10,208,64,128,32,8,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,20,2,36,0,0,0,0,0, 
  0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,189,38,2,0,3,17,1,195,38,2,0,1,2,20,4,40,0,0,0,0,0,0,0,2,0,0,0,42,0,0,0,26,0,0,0,33,0,0,0,3,18,107,0,0, 
  0,1,3,18,115,0,0,0,1,2,21,4,87,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,1,128,41,64,2,128,10,80,129,128,39,0,130,128,34,192,1,128,99,120,2,128,3,18,5,0,0,0,1,3, 
  18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,27,0,0,0,1,3,17,1,218,4,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0, 
  3,17,1,232,38,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,6,39,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,100,0,0,0,22,0, 
  0,0,3,18,19,0,0,0,1,2,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,108,208,0,128,117,8,1,128,3,18,18,0,0,0,1,3,17,1,37,39,2,0,1,2,21,4,41,0,0,0,0,0, 
  0,0,2,0,0,0,1,0,0,0,112,208,0,128,121,8,1,128,3,18,22,0,0,0,1,3,17,1,67,39,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,97, 
  39,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,93,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0, 
  1,3,18,29,0,0,0,1,2,21,4,74,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,129,41,160,1,128,10,48,1,128,47,216,1,128,124,24,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0, 
  1,3,18,87,0,0,0,1,3,17,1,225,234,1,0,1,3,18,97,0,0,0,1,2,21,4,183,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,2,130,41,208,3,128,10,112,2,129,91,200,68,130,36,32, 
  67,130,45,8,4,128,34,224,2,129,39,88,131,128,40,152,3,128,47,72,132,128,58,136,4,128,95,8,4,128,99,0,5,128,116,64,69,128,124,128,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74, 
  255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,18,87,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,47,18,2,0,1,3,18,117, 
  0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,20,4, 
  29,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,3,18,28,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,83,96,129,128,45,240,192,127,95,240,0,128,15,1, 
  154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,104,40,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1, 
  18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,150,40,2,0,1,20,2, 
  40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0, 
  0,0,22,0,0,0,3,17,1,196,40,2,0,1,2,18,125,0,0,0,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,235,238,1,0,3,17,1,160,244,1,0, 
  1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,235,238,1,0,3,17,1,160,244,1,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0, 
  22,0,0,0,3,17,1,167,34,2,0,1,2,21,4,111,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,32,200,1,128,34,0,2,128,10,144,193,127,99,192,2,128,116,0,3,128,125,64,3,128,47,128,2,128,39, 
  64,194,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,17,1,225,234,1,0,1,3,17,1,218,4,2,0,1,3,17,1,248,4,2,0,1,3,18,92, 
  0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,111,42,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17, 
  1,142,42,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22,0,0,0,3,17,1,173,42,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0, 
  3,17,1,203,42,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,233,42,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0, 
  0,0,3,17,1,8,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,38,43,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,112,0,0,0, 
  22,0,0,0,3,18,71,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,68,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,100,0,0, 
  0,22,0,0,0,3,17,1,98,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,129,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116, 
  0,0,0,22,0,0,0,3,17,1,159,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22,0,0,0,3,17,1,195,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0, 
  0,105,0,0,0,22,0,0,0,3,17,1,226,43,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,129,24,2,0,1,2,21,4,42,0,0,0,0,0,0,0,2, 
  0,0,0,1,0,0,0,117,16,1,128,97,208,192,127,3,17,1,28,25,2,0,1,3,17,1,120,25,2,0,1,2,21,4,211,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,66,130,97,160,4,128,114,224, 
  5,128,35,32,195,130,58,32,4,128,95,160,3,128,102,224,4,128,103,32,5,128,109,96,5,130,112,160,5,128,10,176,130,126,43,96,3,128,60,96,4,128,45,160,195,126,115,32,6,128,47,224,131,125,125,96,6,128,3,18, 
  5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1,0,1,3,17,1,188,235,1,0,1, 
  3,17,1,0,44,2,0,1,3,17,1,110,44,2,0,1,3,17,1,208,44,2,0,1,3,17,1,50,45,2,0,1,3,17,1,148,45,2,0,1,3,17,1,2,46,2,0,1,3,17,1,100,46,2,0,1,3,18,92,0, 
  0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,115,192,1,128, 
  10,16,1,128,47,128,129,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,31,47,2,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0, 
  15,1,141,231,1,0,3,17,1,147,231,1,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,213,25,2,0,1,2,21,4,76,0,0,0,0,0,0,0,5,0,0,0, 
  2,0,0,0,32,104,1,129,103,224,1,128,10,48,1,128,47,160,129,127,112,32,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,191,5,2,0,1,3,17,1,228,47,2, 
  0,1,2,21,4,92,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,32,168,1,129,45,224,1,128,10,112,1,128,47,32,194,128,80,96,66,128,84,160,2,128,95,224,1,128,3,18,5,0,0,0,1,3,18,6,0, 
  0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,68,48,2,0,1,3,17,1,166,48,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1, 
  12,235,1,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,73,0,0,0,1,21,4,52,0,0,0,0,0, 
  0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,96,1,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,83,49,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0, 
  0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,157,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,105,112,3,128,10,16,66,129, 
  91,184,66,129,36,128,66,129,109,176,67,129,102,48,3,128,98,240,2,128,115,240,195,128,116,48,4,128,117,112,4,128,123,176,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,100,0,0,0,1,3,18,117, 
  0,0,0,1,3,17,1,184,231,1,0,1,3,17,1,215,231,1,0,1,3,17,1,14,232,1,0,1,3,17,1,80,232,1,0,1,3,17,1,111,232,1,0,1,3,17,1,142,232,1,0,1,3,17,1,196,232,1,0,1, 
  3,18,84,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141,231,1,0,3,17,1,147,231,1,0,1,2,17,1,170,251,1,0,1,21,4,46,0,0,0,0,0,0, 
  0,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,169,49,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26, 
  0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,69,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,209,49, 
  2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0, 
  0,84,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,249,49,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10, 
  2,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,80,0,0,0,22,0,0,0,3,17,1,33,50,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,78,0,0,0,22,0, 
  0,0,3,17,1,64,50,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,80,0,0,0,22,0,0,0,3,17,1,95,50,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,77,0,0,0, 
  22,0,0,0,3,17,1,126,50,2,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,34,208,64,128,92,216,0,128,1,3,17,1,156,50,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,145,31,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1, 
  227,31,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,239,50,2,0,1,17,1,65,51,2,0,1,21,4, 
  41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,34,208,64,128,92,8,1,128,3,18,122,0,0,0,1,3,17,1,154,32,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0, 
  0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,112,51,2,0,3,17,1,145,31,2,0,1,17,1,29,12,2,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0, 
  3,17,1,154,32,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,119,51,2,0,1,17,1,213,51,2,0, 
  1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,54,32,2,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0, 
  0,0,1,0,0,0,92,216,0,128,39,208,0,128,1,3,17,1,4,52,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0, 
  0,3,17,1,28,33,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,110,33,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0, 
  0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,87,52,2,0,1,17,1,169,52,2,0,1,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,8,1,128,39,208,0,128,3, 
  18,121,0,0,0,1,3,17,1,37,34,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,216,52,2,0,3,17, 
  1,28,33,2,0,1,17,1,230,12,2,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,37,34,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0, 
  0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,223,52,2,0,1,17,1,61,53,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0, 
  0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,193,33,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,3,18,98,0,0,0,1,18,83,0,0,0,1,21,4, 
  63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,124,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,18,97,0,0,0, 
  1,2,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,62,208,0,128,97,8,1,128,3,18,85,0,0,0,1,3,17,1,124,23,2,0,1,2,21,4,149,0,0,0,0,0,0,0,12,0,0,0,3,0, 
  0,0,32,72,130,129,34,128,2,128,10,16,194,127,91,240,195,129,36,192,194,129,45,112,3,128,40,56,3,128,39,248,66,128,47,176,67,128,95,112,3,128,99,40,4,128,116,104,4,128,3,18,5,0,0,0,1,3,18,6, 
  0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,18,117,0,0,0,1,3,17,1, 
  240,255,1,0,1,3,17,1,81,0,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,219,0,0,0,0,0,0,0,18,0,0,0,4,0, 
  0,0,32,8,3,128,58,184,4,128,34,64,3,128,99,232,5,128,36,128,3,131,61,248,4,128,63,56,197,128,39,184,3,128,40,248,3,128,95,56,4,128,10,208,194,125,91,112,133,129,124,160,6,128,45,56,4,126,94,168, 
  5,128,47,120,196,125,116,40,6,128,123,104,6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,17,1,41,4,2,0,1,3, 
  17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,87,4,2,0,1,3,17,1,50,249,1,0,1,3,18,82,0,0,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0, 
  1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,157,0,0,0, 
  0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,105,112,3,128,10,16,66,129,91,184,66,129,36,128,66,129,109,176,67,129,102,48,3,128,98,240,2,128,115,240,3,128,116,48,4,128,117,112,68,128,125,176,4,128, 
  3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,100,0,0,0,1,3,18,117,0,0,0,1,3,17,1,184,231,1,0,1,3,17,1,215,231,1,0,1,3,17,1,14,232,1,0,1,3,17,1,80,232,1,0,1, 
  3,17,1,111,232,1,0,1,3,17,1,250,55,2,0,1,3,17,1,196,232,1,0,1,3,18,30,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141,231,1,0,3, 
  17,1,147,231,1,0,1,2,21,4,197,0,0,0,0,0,0,0,16,0,0,0,4,0,0,0,32,200,2,128,58,64,4,128,34,0,3,128,99,112,5,128,116,176,5,128,61,128,4,128,63,192,196,128,39,64,3,128,40,128, 
  3,128,95,192,3,128,10,144,194,125,91,248,68,128,123,240,5,128,45,192,3,126,94,48,5,128,47,0,196,125,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1, 
  3,17,1,41,4,2,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,154,22,2,0,1,3,17,1,50,249,1,0,1,3,18,82,0,0,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2, 
  0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,52,0, 
  0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,94,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,129,4,2,0,1,2,18,118,0,0,0,1,20,2,36,0,0, 
  0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,235,238,1,0,3,17,1,59,56,2,0,1,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,121,0,0,0,22,0,0,0,3,18,23,0,0,0, 
  1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,90,56,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,109,0,0,0,22,0,0,0,3,18,17,0,0, 
  0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,109,0,0,0,22,0,0,0,3,18,24,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,3,18,21,0,0, 
  0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,33,128,1,128,10,16,1,128,47,184,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,88,0,0,0,1,3,17, 
  1,225,234,1,0,1,2,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,130,129,34,96,2,128,10,240,193,127,99,208,3,128,116,16,4,128,45,24,3,129,40,224,2,128,39,160,66,128,47,88,131, 
  128,93,152,3,128,95,24,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1, 
  0,1,3,18,29,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,40,0, 
  0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,74,0,0,0,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,80,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,94,0,0,0,1,2,21,4, 
  127,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,2,128,43,128,2,128,10,208,1,129,35,64,130,127,60,128,67,129,45,192,2,128,58,64,3,128,47,0,67,128,95,192,2,128,124,192,3,128,3,18,5,0, 
  0,0,1,3,18,6,0,0,0,1,3,17,1,98,235,1,0,1,3,17,1,128,235,1,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,158,235,1,0,1,3,17,1,188,235,1,0,1,3,18, 
  97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,208,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,2,128,58,16, 
  5,128,34,32,3,128,35,96,131,129,36,160,131,130,91,144,5,128,95,144,4,128,39,216,3,128,40,24,4,128,99,200,5,128,10,176,194,125,43,80,132,126,60,80,5,129,45,144,4,128,116,8,6,128,47,208,196,125,124,72, 
  6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,98,235,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,128,235,1, 
  0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,10,35,2,0,1,3,17,1,188,235,1,0,1,3,18,117,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,97, 
  0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1, 
  120,56,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,151,56,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3, 
  18,62,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,18,57,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3, 
  17,1,182,56,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,18,69,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,107,0,0,0,22,0,0,0, 
  3,18,48,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0,3,18,55,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,117,0,0,0,22,0,0,0, 
  3,17,1,212,56,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,18,60,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,45,0,0,0,22,0,0, 
  0,3,17,1,243,56,2,0,1,18,78,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,102,0,0,0,22,0,0,0,3,17,1,18,57,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0, 
  112,0,0,0,22,0,0,0,3,18,52,0,0,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,99,128,129,128,45,16,1,128,115,192,1,128,95,16,65,127,15,1,154,244,1,0,3,17,1,160, 
  244,1,0,1,3,17,1,48,57,2,0,1,3,17,1,146,57,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1, 
  18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,97,96,1,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,244,57,2,0,1,20,2, 
  40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0, 
  0,0,95,240,128,128,45,240,192,127,111,96,1,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,86,58,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0, 
  0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,97,96,1,128,15,1,154,244,1,0,3,17, 
  1,160,244,1,0,1,3,17,1,184,58,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1, 
  21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,97,128,129,128,45,16,193,127,117,192,1,128,95,16,1,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,26,59,2,0,1,3,17,1,124, 
  59,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0, 
  3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,96,1,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,222,59,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,96,1,128,15,1, 
  154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,64,60,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1, 
  18,127,0,0,0,1,21,4,88,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,1,128,97,0,2,128,10,80,129,128,47,192,129,128,114,64,2,128,115,128,2,128,3,18,5,0,0,0,1,3,18,6,0,0, 
  0,1,3,17,1,225,234,1,0,1,3,17,1,162,60,2,0,1,3,17,1,64,6,2,0,1,3,17,1,211,27,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,121,0,0,0,22,0,0,0,3,17,1, 
  193,60,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,116,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1, 
  0,1,3,17,1,224,60,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,119,192,1,128,10,16,1,128,47,128,129,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3, 
  17,1,225,234,1,0,1,3,17,1,254,60,2,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,141,231,1,0,3,17,1,147,231,1,0,1,2,20,4,30,0,0,0,0,0, 
  0,0,1,0,0,0,117,0,0,0,22,0,0,0,3,17,1,120,25,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,112,192,1,128,10,16,1,128,47,128,1,128,3,18,5, 
  0,0,0,1,3,18,6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,228,47,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,96,1,128,45,240,64,128,95,240,0,128,15,1, 
  154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,94,61,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1, 
  18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,69,96,129,128,45,240,192,127,95,240,0,128,15,1,154,244,1,0,3,17,1,160,244,1,0,1,3,17,1,192,61,2,0,1,20,2, 
  40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,160,244,1,0,1,18,127,0,0,0,1,21,4,74,0,0,0,0,0,0,0,5,0,0,0,2,0, 
  0,0,32,104,65,128,40,160,1,128,10,48,1,128,47,216,65,128,123,24,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,102,0,0,0,1,3,17,1,225,234,1,0,1,3,18,84,0,0,0,1,2,21, 
  4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,64,10,2,0,1,3,17,1,215,62,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,16,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18, 
  45,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,63,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1, 
  0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,75,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,79,0,0,0,22,0,0,0,3,17,1,45,63,2,0,1,2,20,4,30,0, 
  0,0,0,0,0,0,1,0,0,0,79,0,0,0,22,0,0,0,3,17,1,76,63,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,79,0,0,0,22,0,0,0,3,17,1,107,63,2,0,1,2,20,4, 
  29,0,0,0,0,0,0,0,1,0,0,0,69,0,0,0,22,0,0,0,3,18,64,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,156,50,2,0,1,20,2, 
  46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,145,31,2,0,1,17,1,138,63,2,0,1,21,4,35,0,0,0,0,0,0, 
  0,2,0,0,0,1,0,0,0,34,208,64,128,92,216,0,128,1,3,17,1,227,31,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0, 
  38,0,0,0,3,17,1,239,50,2,0,1,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,239,50,2,0,1,2, 
  17,1,29,12,2,0,1,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,34,208,64,128,92,8,1,128,3,18,122,0,0,0,1,3,17,1,154,32,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,119,51,2,0,1,17,1,29,12,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38, 
  0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,119,51,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,4,52,2,0,1,20,2, 
  46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,28,33,2,0,1,17,1,185,63,2,0,1,21,4,35,0,0,0,0,0,0, 
  0,2,0,0,0,1,0,0,0,92,216,0,128,39,208,0,128,1,3,17,1,110,33,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0, 
  38,0,0,0,3,17,1,87,52,2,0,1,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,87,52,2,0,1,2, 
  17,1,230,12,2,0,1,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,8,1,128,39,208,0,128,3,18,121,0,0,0,1,3,17,1,37,34,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,223,52,2,0,1,17,1,230,12,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38, 
  0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,223,52,2,0,1,2,21,4,206,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,2,128,58,200,4,128,34,32,3,128,99,128, 
  5,128,36,96,131,128,95,72,4,128,116,192,5,128,39,152,3,128,40,216,3,128,41,16,4,128,10,176,194,125,91,8,69,129,124,56,6,128,45,72,4,128,94,64,5,128,47,136,132,125,123,0,6,128,3,18,5,0,0,0, 
  1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,18,87,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0, 
  1,3,17,1,47,18,2,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0, 
  0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,206,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,2,128,58,200,4,128,34,32,3,128,99,184,5,128,36, 
  96,3,131,63,8,69,128,95,72,4,128,39,152,3,128,40,216,3,128,41,16,4,128,10,176,194,125,91,64,5,128,124,56,6,128,45,72,4,128,94,120,5,128,47,136,132,125,116,248,5,128,3,18,5,0,0,0,1,3,18, 
  6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,18,87,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17, 
  1,47,18,2,0,1,3,18,82,0,0,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,149,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,130,129,34,128,2,128,10,16,194,127,99,240,3,130,116,48,4,128, 
  45,56,3,128,40,0,3,128,39,192,66,128,47,120,67,128,63,184,67,128,95,56,3,128,123,112,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18, 
  102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,18,82,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,107,72,65,128,111,128,1,128,114,192,1,128,95,16,65,127,3,18,41,0, 
  0,0,1,3,18,73,0,0,0,1,3,17,1,27,240,1,0,1,3,17,1,58,240,1,0,1,2,20,2,30,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,3,17,1,59,56,2,0,1,1,20,4, 
  29,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,3,18,20,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,112,0,0,0,22,0,0,0,3,17,1,178,67,2,0,1,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,3,17,1,208,67,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0,3,18,79,0,0,0,1,2, 
  20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,238,67,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,12,68,2,0, 
  1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,50,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,96, 
  1,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,43,68,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24, 
  253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,129,68,2, 
  0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0, 
  0,0,1,0,0,0,95,240,128,128,45,240,192,127,105,96,1,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,215,68,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0, 
  0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244, 
  1,0,3,17,1,24,253,1,0,1,3,17,1,45,69,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127, 
  0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,131,69,2,0,1,20,2,40,0, 
  0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0, 
  95,240,128,128,45,240,192,127,115,96,1,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,217,69,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0, 
  15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1,128,15,1,154,244,1,0,3,17,1,24, 
  253,1,0,1,3,17,1,47,70,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4, 
  52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,100,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,133,70,2,0,1,20,2,40,0,0,0,0,0,0,0, 
  2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,96,1,128,45,240, 
  64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,219,70,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0, 
  3,17,1,24,253,1,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,160,24,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,109, 
  0,0,0,22,0,0,0,3,17,1,11,71,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,18,56,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0, 
  105,0,0,0,22,0,0,0,3,17,1,42,71,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,114,192,1,128,10,16,193,127,47,128,1,128,3,18,5,0,0,0,1,3,18, 
  6,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,73,71,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,96,129,128,45,240,192,127,95,240,0,128,15,1,154,244,1,0,3,17, 
  1,24,253,1,0,1,3,17,1,104,71,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1, 
  21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,96,1,128,45,240,64,128,95,240,0,128,15,1,154,244,1,0,3,17,1,24,253,1,0,1,3,17,1,190,71,2,0,1,20,2,40,0,0,0,0,0, 
  0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,154,244,1,0,3,17,1,24,253,1,0,1,18,127,0,0,0,1,21,4,98,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,32,168,65,128, 
  40,224,193,128,10,112,193,128,47,24,194,128,100,88,2,128,102,152,2,128,123,216,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,102,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,20,72,2,0, 
  1,3,17,1,51,72,2,0,1,3,18,84,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128,47,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0, 
  1,3,17,1,225,234,1,0,1,20,2,29,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,3,18,4,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,110,48,1, 
  128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,146,72,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18, 
  127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,82,0,0,0,22,0,0,0,3,17,1,186,72,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,82,0,0,0,22,0,0,0,3,17, 
  1,216,72,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,82,0,0,0,22,0,0,0,3,17,1,246,72,2,0,1,2,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,145,31,2,0,1,2,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38, 
  0,0,0,3,17,1,28,33,2,0,1,2,21,4,139,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,2,130,34,96,2,128,10,240,193,127,99,216,3,128,116,24,4,128,45,24,3,128,94,152,3,128,39,160, 
  130,128,40,224,2,128,47,88,67,128,95,24,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3, 
  17,1,225,234,1,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0, 
  1,2,21,4,149,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,130,129,34,128,2,128,10,16,194,127,99,240,3,130,116,48,4,128,45,56,3,129,40,0,3,128,39,192,66,128,47,120,131,128,93,184,3,128, 
  95,56,3,128,123,112,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0, 
  1,3,18,29,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0, 
  1,2,21,4,184,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,2,130,34,224,66,130,10,112,194,127,91,144,68,130,36,32,67,130,45,208,3,128,94,200,4,128,39,88,131,128,40,152,3,128,47,16,132,128, 
  58,80,4,128,95,208,3,128,99,8,5,128,116,72,69,128,124,136,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102, 
  0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,47,18,2,0,1,3,18,117,0,0,0,1,3,17,1,129,4,2,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3, 
  18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,21,4,183,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,130,129,34, 
  224,2,130,10,112,194,127,91,200,68,130,36,32,67,130,45,208,3,128,40,152,3,128,39,88,67,128,47,16,132,128,58,80,4,128,63,144,68,128,95,208,3,128,99,0,5,128,116,64,69,128,124,128,5,128,3,18,5,0,0, 
  0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18,102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,17,1,47,18, 
  2,0,1,3,18,82,0,0,0,1,3,18,117,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,97,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0, 
  0,0,3,17,1,12,235,1,0,1,2,21,4,160,0,0,0,0,0,0,0,13,0,0,0,3,0,0,0,32,104,130,129,34,160,2,128,10,48,194,127,91,16,196,129,36,224,194,129,45,144,3,128,40,88,3,128,39,24,67, 
  128,47,208,67,128,95,144,3,128,99,72,132,128,116,136,4,128,123,200,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,255,1,0,1,3,18,100,0,0,0,1,3,17,1,157,255,1,0,1,3,18, 
  102,0,0,0,1,3,17,1,12,235,1,0,1,3,17,1,225,234,1,0,1,3,18,117,0,0,0,1,3,17,1,240,255,1,0,1,3,17,1,81,0,2,0,1,3,18,84,0,0,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,12,235,1,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,77,0,0,0,1,2,20,4,29,0,0,0,0,0, 
  0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,49,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,18,40,0,0,0,1,2,20,4,30,0,0,0,0,0, 
  0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,20,73,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,64,10,2,0, 
  1,3,17,1,50,73,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3, 
  0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,64,10,2,0,1,3,17,1,136,73,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0, 
  0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,108,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,222,73,2,0, 
  1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95, 
  240,128,128,45,240,192,127,111,48,1,128,3,17,1,64,10,2,0,1,3,17,1,6,74,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0, 
  1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,48,1,128,3,17,1,64,10,2,0,1,3,17,1,46,74,2,0,1,20,2,34,0,0,0,0, 
  0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115, 
  48,1,128,3,17,1,64,10,2,0,1,3,17,1,132,74,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21, 
  4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,172,74,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,117,48,1,128,3,17,1,64,10, 
  2,0,1,3,17,1,212,74,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,42,0,0,0,0,0,0, 
  0,2,0,0,0,1,0,0,0,95,16,1,128,45,208,192,127,3,17,1,42,75,2,0,1,3,17,1,64,10,2,0,1,18,78,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0, 
  0,3,17,1,142,75,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,173,75,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,117,0,0,0,22, 
  0,0,0,3,17,1,203,75,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,68,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,234,75,2,0,1,20,2, 
  34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,77,48,129,128, 
  45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,64,76,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127, 
  0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,150,76,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1, 
  181,76,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,41,128,1,128,10,16,1,128,47,184,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,87,0,0,0, 
  1,3,17,1,225,234,1,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,76,0,0,0,1,20,4,29,0,0,0,0,0,0,0, 
  1,0,0,0,84,0,0,0,22,0,0,0,3,18,45,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,69,0,0,0,22,0,0,0,3,18,63,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0, 
  1,0,0,0,84,0,0,0,22,0,0,0,3,18,75,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,107,0,0,0,22,0,0,0,3,18,37,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0, 
  3,0,0,0,1,0,0,0,112,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,131,77,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0, 
  0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,217,77,2, 
  0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,62,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,57,0,0, 
  0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,47,78,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0, 
  0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2, 
  0,1,18,69,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,55,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0, 
  0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,48,1,128,3,17,1,64,10,2,0,1,3,17,1,87,78,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0, 
  3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,50,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,80,1,128,45,240,64,128,95,240,0,128,18,127,0,0,0,17,1,173,78,2,0,1,3,17,1,185, 
  78,2,0,1,20,2,38,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,18,127,0,0,0,17,1,173,78,2,0,1,18,127,0,0,0,17,1,173,78,2,0,1,20,4,30,0,0,0, 
  0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,10,79,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0,3,18,51,0,0,0,1,2,20,4,30,0,0, 
  0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,17,1,41,79,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,85,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64, 
  10,2,0,1,3,17,1,71,79,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0, 
  0,0,3,0,0,0,1,0,0,0,73,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,157,79,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0, 
  26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,102,0,0,0,22,0,0,0,3,17,1,243,79,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1, 
  0,0,0,105,0,0,0,22,0,0,0,3,17,1,18,80,2,0,1,2,21,4,109,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,32,200,65,128,40,0,2,128,10,144,1,128,123,248,2,128,100,120,2,128,125,48, 
  3,128,102,184,2,128,47,56,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,102,0,0,0,1,3,17,1,225,234,1,0,1,3,17,1,20,72,2,0,1,3,17,1,51,72,2,0,1,3,18,84,0,0, 
  0,1,3,18,92,0,0,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,34,128,1,128,10,16,193,127,47,192,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17, 
  1,74,255,1,0,1,3,17,1,225,234,1,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,49,80,2,0, 
  1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116, 
  48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,89,80,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0, 
  1,18,127,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,79,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0, 
  0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,64,10,2,0,1,3,17,1,129,80,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3, 
  17,1,64,10,2,0,1,18,127,0,0,0,1,18,127,0,0,0,17,1,169,80,2,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,48,1,128,3,17,1,64,10, 
  2,0,1,3,17,1,238,80,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0, 
  108,0,0,0,22,0,0,0,3,17,1,63,81,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,18,61,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0, 
  0,1,0,0,0,67,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,93,81,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3, 
  17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,179,81,2,0,1,20, 
  2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0, 
  0,3,17,1,9,82,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,17,1,40,82,2,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,77,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,49,0,0,0, 
  1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,40,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95, 
  208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,2,21,4,46,0,0,0,0,0,0,0, 
  3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,48,1,128,3,17,1,64,10,2,0,1,3,17,1,71,82,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0, 
  0,0,3,17,1,64,10,2,0,1,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,18,43,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,84, 
  48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,111,82,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0, 
  1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,65,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,197,82,2,0,1,20,2,34,0,0,0,0, 
  0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,117,0,0,0,22,0,0,0,3,17,1,27,83, 
  2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,45,0,0,0,22,0,0,0,3,17,1,58,83,2,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192, 
  127,3,17,1,64,10,2,0,1,18,37,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,73,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,89,83,2,0, 
  1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,76, 
  48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,175,83,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0, 
  1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,17,1,215,83,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0, 
  3,17,1,245,83,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,20,84,2,0,1,20,2,34,0,0, 
  0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192, 
  127,3,17,1,64,10,2,0,1,18,47,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,44,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105, 
  0,0,0,22,0,0,0,3,17,1,106,84,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17,1,64,10,2,0,1,3,17,1,137,84,2, 
  0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,64,10,2,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0, 
  22,0,0,0,3,17,1,177,84,2,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,64,10,2,0,1,18,70,0,0,0,1,20,4,29,0,0,0,0, 
  0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,58,0,0,0,1,2, 
];

pub mod ast{
  impl AstObject for ASTNode {}
  type ASTSlot = (ASTNode, TokenRange, TokenRange);
  use super::*; 
  type Node = ASTNode;
  
  pub fn grammar_from<'a> (mut reader: UTF8StringReader)-> Result<Box<Grammar>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(60);
    let AstSlot (obj1, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = obj1;
    let obj_0_0 = obj_0_0.to_Grammar();
    Ok(obj_0_0)
  }
  
  pub fn ast_struct_from<'a> (mut reader: UTF8StringReader)-> Result<Box<AST_Struct>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(21);
    let AstSlot (obj1, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = obj1;
    let obj_0_0 = obj_0_0.to_AST_Struct();
    Ok(obj_0_0)
  }
  
  pub fn ast_expression_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(8);
    let AstSlot (obj1, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = obj1;
    Ok(obj_0_0)
  }
  
  pub fn ir_from<'a> (mut reader: UTF8StringReader)-> Result<Box<State>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(73);
    let AstSlot (obj1, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = obj1;
    let obj_0_0 = obj_0_0.to_State();
    Ok(obj_0_0)
  }
  
  pub fn type_eval_from<'a> (mut reader: UTF8StringReader)-> Result<ASTNode, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(34);
    let AstSlot (obj1, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = obj1;
    Ok(obj_0_0)
  }
  
  pub fn escaped_from<'a> (mut reader: UTF8StringReader)-> Result<Vec<String>, SherpaParseError> {
    let reduce_functions = ReduceFunctions::<_, u32, true>::new();
    let mut parser = Parser::new(&mut reader, &bytecode);
    parser.init_parser(47);
    let AstSlot (obj1, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;
    let obj_0_0 = obj1.into_strings();
    Ok(obj_0_0)
  }
}