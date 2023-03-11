
/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta1
/// - **SOURCE**: /home/work/projects/lib_sherpa/source/grammar/v2_0_0/grammar.sg
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
  Token_Group_Production(Box<Token_Group_Production>),
  PrattProduction(Box<PrattProduction>),
  EOFSymbol(Box<EOFSymbol>),
  Assign(Box<Assign>),
  PegProduction(Box<PegProduction>),
  AST_Add(Box<AST_Add>),
  Match(Box<Match>),
  Precedence(Box<Precedence>),
  NonTermMatch(Box<NonTermMatch>),
  Syntax(Box<Syntax>),
  SyntaxField(Box<SyntaxField>),
  List_Production(Box<List_Production>),
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
  RGBA(Box<RGBA>),
  AST_Member(Box<AST_Member>),
  Production_Import_Symbol(Box<Production_Import_Symbol>),
  AST_I32(Box<AST_I32>),
  AST_I8(Box<AST_I8>),
  AST_F32(Box<AST_F32>),
  Gotos(Box<Gotos>),
  AST_Token(Box<AST_Token>),
  Shift(Box<Shift>),
  Export(Box<Export>),
  Name(Box<Name>),
  AST_BOOL(Box<AST_BOOL>),
  Group_Production(Box<Group_Production>),
  Fail(Box<Fail>),
  AST_Statements(Box<AST_Statements>),
  AST_U8(Box<AST_U8>),
  Assert(Box<Assert>),
  Ignore(Box<Ignore>),
  AST_STRING(Box<AST_STRING>),
  Skip(Box<Skip>),
  AST_U32(Box<AST_U32>),
  State(Box<State>),
  AST_ClassId(Box<AST_ClassId>),
  AST_I64(Box<AST_I64>),
  DefaultMatch(Box<DefaultMatch>),
  AST_U16(Box<AST_U16>),
  Accept(Box<Accept>),
  ClassSymbol(Box<ClassSymbol>),
  Statement(Box<Statement>),
  AST_F64(Box<AST_F64>),
  AST_Map(Box<AST_Map>),
  AnyGroup(Box<AnyGroup>),
  Range(Box<Range>),
  AST_I16(Box<AST_I16>),
  Scan(Box<Scan>),
  AST_NUMBER(Box<AST_NUMBER>),
  AST_Struct(Box<AST_Struct>),
  Peek(Box<Peek>),
  TermMatch(Box<TermMatch>),
  DEFINED_TYPE_NUM(Box<DEFINED_TYPE_NUM>),
  SyntaxSpec(Box<SyntaxSpec>),
  SetVal(Box<SetVal>),
  Rule(Box<Rule>),
  AnnotatedSymbol(Box<AnnotatedSymbol>),
  AST_Property(Box<AST_Property>),
  Ascript(Box<Ascript>),
  AppendProduction(Box<AppendProduction>),
  Production_Terminal_Symbol(Box<Production_Terminal_Symbol>),
  Recovery(Box<Recovery>),
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
  Token_Group_Production,
  PrattProduction,
  EOFSymbol,
  Assign,
  PegProduction,
  AST_Add,
  Match,
  Precedence,
  NonTermMatch,
  Syntax,
  SyntaxField,
  List_Production,
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
  RGBA,
  AST_Member,
  Production_Import_Symbol,
  AST_I32,
  AST_I8,
  AST_F32,
  Gotos,
  AST_Token,
  Shift,
  Export,
  Name,
  AST_BOOL,
  Group_Production,
  Fail,
  AST_Statements,
  AST_U8,
  Assert,
  Ignore,
  AST_STRING,
  Skip,
  AST_U32,
  State,
  AST_ClassId,
  AST_I64,
  DefaultMatch,
  AST_U16,
  Accept,
  ClassSymbol,
  Statement,
  AST_F64,
  AST_Map,
  AnyGroup,
  Range,
  AST_I16,
  Scan,
  AST_NUMBER,
  AST_Struct,
  Peek,
  TermMatch,
  DEFINED_TYPE_NUM,
  SyntaxSpec,
  SetVal,
  Rule,
  AnnotatedSymbol,
  AST_Property,
  Ascript,
  AppendProduction,
  Production_Terminal_Symbol,
  Recovery,
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
      ASTNode::Token_Group_Production(node) => node.tok.clone(),
      ASTNode::PrattProduction(node) => node.tok.clone(),
      ASTNode::EOFSymbol(node) => node.tok.clone(),
      ASTNode::PegProduction(node) => node.tok.clone(),
      ASTNode::AST_Add(node) => node.tok.clone(),
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
      ASTNode::Group_Production(node) => node.tok.clone(),
      ASTNode::AST_Statements(node) => node.tok.clone(),
      ASTNode::AST_U8(node) => node.tok.clone(),
      ASTNode::AST_STRING(node) => node.tok.clone(),
      ASTNode::AST_U32(node) => node.tok.clone(),
      ASTNode::State(node) => node.tok.clone(),
      ASTNode::AST_ClassId(node) => node.tok.clone(),
      ASTNode::AST_I64(node) => node.tok.clone(),
      ASTNode::AST_U16(node) => node.tok.clone(),
      ASTNode::ClassSymbol(node) => node.tok.clone(),
      ASTNode::AST_F64(node) => node.tok.clone(),
      ASTNode::AST_Map(node) => node.tok.clone(),
      ASTNode::AnyGroup(node) => node.tok.clone(),
      ASTNode::AST_I16(node) => node.tok.clone(),
      ASTNode::AST_Struct(node) => node.tok.clone(),
      ASTNode::Rule(node) => node.tok.clone(),
      ASTNode::AnnotatedSymbol(node) => node.tok.clone(),
      ASTNode::AST_Property(node) => node.tok.clone(),
      ASTNode::Ascript(node) => node.tok.clone(),
      ASTNode::AppendProduction(node) => node.tok.clone(),
      ASTNode::Production_Terminal_Symbol(node) => node.tok.clone(),
      ASTNode::Recovery(node) => node.tok.clone(),
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
      ASTNode::Token_Group_Production(..) => ASTNodeType::Token_Group_Production,
      ASTNode::PrattProduction(..) => ASTNodeType::PrattProduction,
      ASTNode::EOFSymbol(..) => ASTNodeType::EOFSymbol,
      ASTNode::Assign(..) => ASTNodeType::Assign,
      ASTNode::PegProduction(..) => ASTNodeType::PegProduction,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::Match(..) => ASTNodeType::Match,
      ASTNode::Precedence(..) => ASTNodeType::Precedence,
      ASTNode::NonTermMatch(..) => ASTNodeType::NonTermMatch,
      ASTNode::Syntax(..) => ASTNodeType::Syntax,
      ASTNode::SyntaxField(..) => ASTNodeType::SyntaxField,
      ASTNode::List_Production(..) => ASTNodeType::List_Production,
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
      ASTNode::RGBA(..) => ASTNodeType::RGBA,
      ASTNode::AST_Member(..) => ASTNodeType::AST_Member,
      ASTNode::Production_Import_Symbol(..) => ASTNodeType::Production_Import_Symbol,
      ASTNode::AST_I32(..) => ASTNodeType::AST_I32,
      ASTNode::AST_I8(..) => ASTNodeType::AST_I8,
      ASTNode::AST_F32(..) => ASTNodeType::AST_F32,
      ASTNode::Gotos(..) => ASTNodeType::Gotos,
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Shift(..) => ASTNodeType::Shift,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::AST_BOOL(..) => ASTNodeType::AST_BOOL,
      ASTNode::Group_Production(..) => ASTNodeType::Group_Production,
      ASTNode::Fail(..) => ASTNodeType::Fail,
      ASTNode::AST_Statements(..) => ASTNodeType::AST_Statements,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::Assert(..) => ASTNodeType::Assert,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_STRING(..) => ASTNodeType::AST_STRING,
      ASTNode::Skip(..) => ASTNodeType::Skip,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::State(..) => ASTNodeType::State,
      ASTNode::AST_ClassId(..) => ASTNodeType::AST_ClassId,
      ASTNode::AST_I64(..) => ASTNodeType::AST_I64,
      ASTNode::DefaultMatch(..) => ASTNodeType::DefaultMatch,
      ASTNode::AST_U16(..) => ASTNodeType::AST_U16,
      ASTNode::Accept(..) => ASTNodeType::Accept,
      ASTNode::ClassSymbol(..) => ASTNodeType::ClassSymbol,
      ASTNode::Statement(..) => ASTNodeType::Statement,
      ASTNode::AST_F64(..) => ASTNodeType::AST_F64,
      ASTNode::AST_Map(..) => ASTNodeType::AST_Map,
      ASTNode::AnyGroup(..) => ASTNodeType::AnyGroup,
      ASTNode::Range(..) => ASTNodeType::Range,
      ASTNode::AST_I16(..) => ASTNodeType::AST_I16,
      ASTNode::Scan(..) => ASTNodeType::Scan,
      ASTNode::AST_NUMBER(..) => ASTNodeType::AST_NUMBER,
      ASTNode::AST_Struct(..) => ASTNodeType::AST_Struct,
      ASTNode::Peek(..) => ASTNodeType::Peek,
      ASTNode::TermMatch(..) => ASTNodeType::TermMatch,
      ASTNode::DEFINED_TYPE_NUM(..) => ASTNodeType::DEFINED_TYPE_NUM,
      ASTNode::SyntaxSpec(..) => ASTNodeType::SyntaxSpec,
      ASTNode::SetVal(..) => ASTNodeType::SetVal,
      ASTNode::Rule(..) => ASTNodeType::Rule,
      ASTNode::AnnotatedSymbol(..) => ASTNodeType::AnnotatedSymbol,
      ASTNode::AST_Property(..) => ASTNodeType::AST_Property,
      ASTNode::Ascript(..) => ASTNodeType::Ascript,
      ASTNode::AppendProduction(..) => ASTNodeType::AppendProduction,
      ASTNode::Production_Terminal_Symbol(..) => ASTNodeType::Production_Terminal_Symbol,
      ASTNode::Recovery(..) => ASTNodeType::Recovery,
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
      Token_Group_Production(node) => node.hash(hasher),
      PrattProduction(node) => node.hash(hasher),
      EOFSymbol(node) => node.hash(hasher),
      Assign(node) => node.hash(hasher),
      PegProduction(node) => node.hash(hasher),
      AST_Add(node) => node.hash(hasher),
      Match(node) => node.hash(hasher),
      Precedence(node) => node.hash(hasher),
      NonTermMatch(node) => node.hash(hasher),
      Syntax(node) => node.hash(hasher),
      SyntaxField(node) => node.hash(hasher),
      List_Production(node) => node.hash(hasher),
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
      RGBA(node) => node.hash(hasher),
      AST_Member(node) => node.hash(hasher),
      Production_Import_Symbol(node) => node.hash(hasher),
      AST_I32(node) => node.hash(hasher),
      AST_I8(node) => node.hash(hasher),
      AST_F32(node) => node.hash(hasher),
      Gotos(node) => node.hash(hasher),
      AST_Token(node) => node.hash(hasher),
      Shift(node) => node.hash(hasher),
      Export(node) => node.hash(hasher),
      Name(node) => node.hash(hasher),
      AST_BOOL(node) => node.hash(hasher),
      Group_Production(node) => node.hash(hasher),
      Fail(node) => node.hash(hasher),
      AST_Statements(node) => node.hash(hasher),
      AST_U8(node) => node.hash(hasher),
      Assert(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      AST_STRING(node) => node.hash(hasher),
      Skip(node) => node.hash(hasher),
      AST_U32(node) => node.hash(hasher),
      State(node) => node.hash(hasher),
      AST_ClassId(node) => node.hash(hasher),
      AST_I64(node) => node.hash(hasher),
      DefaultMatch(node) => node.hash(hasher),
      AST_U16(node) => node.hash(hasher),
      Accept(node) => node.hash(hasher),
      ClassSymbol(node) => node.hash(hasher),
      Statement(node) => node.hash(hasher),
      AST_F64(node) => node.hash(hasher),
      AST_Map(node) => node.hash(hasher),
      AnyGroup(node) => node.hash(hasher),
      Range(node) => node.hash(hasher),
      AST_I16(node) => node.hash(hasher),
      Scan(node) => node.hash(hasher),
      AST_NUMBER(node) => node.hash(hasher),
      AST_Struct(node) => node.hash(hasher),
      Peek(node) => node.hash(hasher),
      TermMatch(node) => node.hash(hasher),
      DEFINED_TYPE_NUM(node) => node.hash(hasher),
      SyntaxSpec(node) => node.hash(hasher),
      SetVal(node) => node.hash(hasher),
      Rule(node) => node.hash(hasher),
      AnnotatedSymbol(node) => node.hash(hasher),
      AST_Property(node) => node.hash(hasher),
      Ascript(node) => node.hash(hasher),
      AppendProduction(node) => node.hash(hasher),
      Production_Terminal_Symbol(node) => node.hash(hasher),
      Recovery(node) => node.hash(hasher),
      
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
}

impl Reduce{
  
  pub fn new ()-> Self {
    
    Self{
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
  }
}

#[derive(Debug, Clone)]
pub struct Token_Group_Production{
  pub rules: Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl Token_Group_Production{
  
  pub fn new (rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Token_Group_Production
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Token_Group_Production (self)-> Box::<Token_Group_Production> {
    
    match self{
      Self::Token_Group_Production(val) => val,
      _ => panic!("Type {:?} cannot be converted to Token_Group_Production", self.get_type())
    }
  }
  
  pub fn as_Token_Group_Production (&self)-> Option<&Token_Group_Production> {
    
    match self{
      Self::Token_Group_Production(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Token_Group_Production_mut (&mut self)-> Option<&mut Token_Group_Production> {
    
    match self{
      Self::Token_Group_Production(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Token_Group_Production{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
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
pub struct Assign{
}

impl Assign{
  
  pub fn new ()-> Self {
    
    Self{
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Assign
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Assign (self)-> Box::<Assign> {
    
    match self{
      Self::Assign(val) => val,
      _ => panic!("Type {:?} cannot be converted to Assign", self.get_type())
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
pub struct Syntax{
  pub specs: Vec<Box<SyntaxField>>, 
}

impl Syntax{
  
  pub fn new (specs: Vec<Box<SyntaxField>>)-> Self {
    
    Self{
      specs,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Syntax
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Syntax (self)-> Box::<Syntax> {
    
    match self{
      Self::Syntax(val) => val,
      _ => panic!("Type {:?} cannot be converted to Syntax", self.get_type())
    }
  }
  
  pub fn as_Syntax (&self)-> Option<&Syntax> {
    
    match self{
      Self::Syntax(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Syntax_mut (&mut self)-> Option<&mut Syntax> {
    
    match self{
      Self::Syntax(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Syntax{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.specs{
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct SyntaxField{
  pub reference: ASTNode, 
  pub spec: Box<SyntaxSpec>, 
}

impl SyntaxField{
  
  pub fn new (reference: ASTNode, spec: Box<SyntaxSpec>)-> Self {
    
    Self{
      reference,
      spec,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SyntaxField
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_SyntaxField (self)-> Box::<SyntaxField> {
    
    match self{
      Self::SyntaxField(val) => val,
      _ => panic!("Type {:?} cannot be converted to SyntaxField", self.get_type())
    }
  }
  
  pub fn as_SyntaxField (&self)-> Option<&SyntaxField> {
    
    match self{
      Self::SyntaxField(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SyntaxField_mut (&mut self)-> Option<&mut SyntaxField> {
    
    match self{
      Self::SyntaxField(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SyntaxField{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.reference.hash(hasher);
    self.spec.hash(hasher);
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
pub struct RGBA{
  pub a: u32, 
  pub b: u32, 
  pub g: u32, 
  pub r: u32, 
}

impl RGBA{
  
  pub fn new (a: u32, b: u32, g: u32, r: u32)-> Self {
    
    Self{
      a,
      b,
      g,
      r,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::RGBA
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_RGBA (self)-> Box::<RGBA> {
    
    match self{
      Self::RGBA(val) => val,
      _ => panic!("Type {:?} cannot be converted to RGBA", self.get_type())
    }
  }
  
  pub fn as_RGBA (&self)-> Option<&RGBA> {
    
    match self{
      Self::RGBA(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_RGBA_mut (&mut self)-> Option<&mut RGBA> {
    
    match self{
      Self::RGBA(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for RGBA{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.a.hash(hasher);
    self.b.hash(hasher);
    self.g.hash(hasher);
    self.r.hash(hasher);
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
  pub goto: ASTNode, 
  pub pushes: Vec<ASTNode>, 
}

impl Gotos{
  
  pub fn new (goto: ASTNode, pushes: Vec<ASTNode>)-> Self {
    
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
pub struct Group_Production{
  pub rules: Vec<Box<Rule>>, 
  pub tok: Token, 
}

impl Group_Production{
  
  pub fn new (rules: Vec<Box<Rule>>, tok: Token)-> Self {
    
    Self{
      rules,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Group_Production
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Group_Production (self)-> Box::<Group_Production> {
    
    match self{
      Self::Group_Production(val) => val,
      _ => panic!("Type {:?} cannot be converted to Group_Production", self.get_type())
    }
  }
  
  pub fn as_Group_Production (&self)-> Option<&Group_Production> {
    
    match self{
      Self::Group_Production(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Group_Production_mut (&mut self)-> Option<&mut Group_Production> {
    
    match self{
      Self::Group_Production(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Group_Production{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.rules{
      val.hash(hasher);
    }
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
  pub id: Box<Production_Symbol>, 
  pub statement: Box<Statement>, 
  pub tok: Token, 
}

impl State{
  
  pub fn new (id: Box<Production_Symbol>, statement: Box<Statement>, tok: Token)-> Self {
    
    Self{
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
    self.id.hash(hasher);
    self.statement.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_ClassId{
  pub value: String, 
  pub tok: Token, 
}

impl AST_ClassId{
  
  pub fn new (value: String, tok: Token)-> Self {
    
    Self{
      value,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AST_ClassId
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_AST_ClassId (self)-> Box::<AST_ClassId> {
    
    match self{
      Self::AST_ClassId(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_ClassId", self.get_type())
    }
  }
  
  pub fn as_AST_ClassId (&self)-> Option<&AST_ClassId> {
    
    match self{
      Self::AST_ClassId(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AST_ClassId_mut (&mut self)-> Option<&mut AST_ClassId> {
    
    match self{
      Self::AST_ClassId(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AST_ClassId{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
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
pub struct AnyGroup{
  pub symbols: Vec<ASTNode>, 
  pub unordered: bool, 
  pub tok: Token, 
}

impl AnyGroup{
  
  pub fn new (symbols: Vec<ASTNode>, unordered: bool, tok: Token)-> Self {
    
    Self{
      symbols,
      unordered,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::AnyGroup
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_AnyGroup (self)-> Box::<AnyGroup> {
    
    match self{
      Self::AnyGroup(val) => val,
      _ => panic!("Type {:?} cannot be converted to AnyGroup", self.get_type())
    }
  }
  
  pub fn as_AnyGroup (&self)-> Option<&AnyGroup> {
    
    match self{
      Self::AnyGroup(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_AnyGroup_mut (&mut self)-> Option<&mut AnyGroup> {
    
    match self{
      Self::AnyGroup(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for AnyGroup{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    
    for val in &self.symbols{
      val.hash(hasher);
    }
    self.unordered.hash(hasher);
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
pub struct SyntaxSpec{
  pub id: Token, 
  pub rgb: Option<Box<RGBA>>, 
}

impl SyntaxSpec{
  
  pub fn new (id: Token, rgb: Option<Box<RGBA>>)-> Self {
    
    Self{
      id,
      rgb,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::SyntaxSpec
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_SyntaxSpec (self)-> Box::<SyntaxSpec> {
    
    match self{
      Self::SyntaxSpec(val) => val,
      _ => panic!("Type {:?} cannot be converted to SyntaxSpec", self.get_type())
    }
  }
  
  pub fn as_SyntaxSpec (&self)-> Option<&SyntaxSpec> {
    
    match self{
      Self::SyntaxSpec(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_SyntaxSpec_mut (&mut self)-> Option<&mut SyntaxSpec> {
    
    match self{
      Self::SyntaxSpec(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for SyntaxSpec{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.to_string().replace(" ", "").replace("\n", "").hash(hasher);
    self.rgb.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct SetVal{
}

impl SetVal{
  
  pub fn new ()-> Self {
    
    Self{
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
  }
}

#[derive(Debug, Clone)]
pub struct Rule{
  pub ast: Option<Box<Ascript>>, 
  pub recover_definition: Option<Box<Recovery>>, 
  pub symbols: Vec<ASTNode>, 
  pub syntax_definition: Option<Box<Syntax>>, 
  pub tok: Token, 
}

impl Rule{
  
  pub fn new (ast: Option<Box<Ascript>>, recover_definition: Option<Box<Recovery>>, symbols: Vec<ASTNode>, syntax_definition: Option<Box<Syntax>>, tok: Token)-> Self {
    
    Self{
      ast,
      recover_definition,
      symbols,
      syntax_definition,
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
    self.recover_definition.hash(hasher);
    
    for val in &self.symbols{
      val.hash(hasher);
    }
    self.syntax_definition.hash(hasher);
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

#[derive(Debug, Clone)]
pub struct Recovery{
  pub state: Box<State>, 
  pub tok: Token, 
}

impl Recovery{
  
  pub fn new (state: Box<State>, tok: Token)-> Self {
    
    Self{
      state,
      tok,
    }
  }
  
  pub fn get_type (&self)-> ASTNodeType {
    ASTNodeType::Recovery
  }
}

impl ASTNode{
  
  #[track_caller]
  pub fn to_Recovery (self)-> Box::<Recovery> {
    
    match self{
      Self::Recovery(val) => val,
      _ => panic!("Type {:?} cannot be converted to Recovery", self.get_type())
    }
  }
  
  pub fn as_Recovery (&self)-> Option<&Recovery> {
    
    match self{
      Self::Recovery(val) => Some(val.as_ref()),
      _ => None
    }
  }
  
  pub fn as_Recovery_mut (&mut self)-> Option<&mut Recovery> {
    
    match self{
      Self::Recovery(val) => Some(val.as_mut()),
      _ => None
    }
  }
}

impl Hash for Recovery{
  
  fn hash<H: std::hash::Hasher> (&self,hasher: &mut H) {
    self.get_type().hash(hasher);
    self.state.hash(hasher);
  }
}

fn reducer_000 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_001 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_002 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_State();
  let obj_5_0 = Recovery::new(
    obj_2_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Recovery(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_003 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_004 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_005 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_006 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_007 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_008 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_009 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_010 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_011 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_012 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I8::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_013 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_014 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I16::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_015 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_016 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_017 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_018 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_019 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_020 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_F32::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_021 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_022 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_F64::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_023 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_024 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_025 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_026 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_027 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_028 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_029 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_030 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_031 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_032 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_033 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_034 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_0 = AST_ClassId::new(
    tok_0_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_ClassId(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_035 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_036 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_037 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_038 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_039 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
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

fn reducer_042 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_043 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_044 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_045 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_046 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_047 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
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
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_050 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_051 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_052 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_053 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_054 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_055 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_056 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_057 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_058 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_059 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_060 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_061 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_062 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_063 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_064 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_065 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_066 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = Group_Production::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Group_Production(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_067 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_068 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_069 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_070 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_071 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_072 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_073 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_074 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_075 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_076 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_077 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
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
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  let obj_3_0 = AST_NamedReference::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_NamedReference(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_081 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i64>(unsafe{&*_ctx_}.get_str());
  let obj_3_0 = AST_IndexReference::new(
    tok_1_0,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_IndexReference(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_082 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_083 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_084 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_085 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(
    obj_5_0,
    obj_4_1,
    obj_3_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_086 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_1 = obj5.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_7_0 = RGBA::new(
    0,
    obj_4_1,
    obj_3_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_087 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(
    obj_4_0,
    obj_5_1,
    obj_3_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_088 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(
    obj_5_0,
    obj_3_1,
    obj_4_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_089 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_3_1 = obj4.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_7_0 = RGBA::new(
    0,
    obj_3_1,
    obj_4_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_090 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(
    obj_4_0,
    obj_3_1,
    obj_5_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_091 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(
    obj_3_0,
    obj_5_1,
    obj_4_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_092 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(
    obj_3_0,
    obj_4_1,
    obj_5_2,
    obj_2_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_093 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(
    obj_5_0,
    obj_4_1,
    obj_2_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_094 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_1 = obj5.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_7_0 = RGBA::new(
    0,
    obj_4_1,
    obj_2_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_095 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(
    obj_4_0,
    obj_5_1,
    obj_2_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_096 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(
    obj_5_0,
    obj_3_1,
    obj_2_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_097 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_3_1 = obj4.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_7_0 = RGBA::new(
    0,
    obj_3_1,
    obj_2_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_098 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(
    obj_4_0,
    obj_3_1,
    obj_2_2,
    obj_5_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_099 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(
    obj_3_0,
    obj_5_1,
    obj_2_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_100 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(
    obj_3_0,
    obj_4_1,
    obj_2_2,
    obj_5_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_101 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(
    obj_5_0,
    obj_2_1,
    obj_4_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_102 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_1 = obj3.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_7_0 = RGBA::new(
    0,
    obj_2_1,
    obj_4_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_103 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(
    obj_4_0,
    obj_2_1,
    obj_5_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_104 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(
    obj_5_0,
    obj_2_1,
    obj_3_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_105 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_1 = obj3.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_7_0 = RGBA::new(
    0,
    obj_2_1,
    obj_3_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_106 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(
    obj_4_0,
    obj_2_1,
    obj_3_2,
    obj_5_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_107 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(
    obj_3_0,
    obj_2_1,
    obj_5_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_108 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(
    obj_3_0,
    obj_2_1,
    obj_4_2,
    obj_5_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_109 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(
    obj_2_0,
    obj_5_1,
    obj_4_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_110 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(
    obj_2_0,
    obj_4_1,
    obj_5_2,
    obj_3_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_111 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(
    obj_2_0,
    obj_5_1,
    obj_3_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_112 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(
    obj_2_0,
    obj_4_1,
    obj_3_2,
    obj_5_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_113 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(
    obj_2_0,
    obj_3_1,
    obj_5_2,
    obj_4_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_114 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, _, _) = slots.take(4);
  let AstSlot (obj6, _, _) = slots.take(5);
  let AstSlot (_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(
    obj_2_0,
    obj_3_1,
    obj_4_2,
    obj_5_3,
  );
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_115 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_116 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_117 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_118 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = Token_Group_Production::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Token_Group_Production(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_119 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_120 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_121 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_122 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_123 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_124 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_125 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
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

fn reducer_128 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_129 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_130 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_131 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_132 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_133 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_134 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_135 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_136 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_137 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_138 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Fail::new();
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_139 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Pass::new();
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_140 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Accept::new();
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_141 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_142 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_143 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_144 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_145 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_146 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_147 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = AST_Vector::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_148 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_149 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_150 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_151 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_152 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_153 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_154 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Reduce::new();
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_155 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Assign::new();
  slots.assign(0, AstSlot(ASTNode::Assign(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_156 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = SetVal::new();
  slots.assign(0, AstSlot(ASTNode::SetVal(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_157 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Assert::new();
  slots.assign(0, AstSlot(ASTNode::Assert(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_158 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_159 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_0_1 = obj1.into_nodes();
  let obj_3_0 = Gotos::new(
    obj_1_0,
    obj_0_1,
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_160 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let obj_2_0 = Gotos::new(
    obj_0_0,
    vec![],
  );
  slots.assign(0, AstSlot(ASTNode::Gotos(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_161 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_162 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_163 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_164 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_165 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_166 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(
    None,
  );
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_167 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_168 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_169 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let AstSlot (_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_3_0 = __tok_rng_4;
  let tok_3_0 = tok_3_0.parse::<i32>(unsafe{&*_ctx_}.get_str());
  let tok_1_1 = __tok_rng_2;
  let tok_1_1 = tok_1_1.parse::<i32>(unsafe{&*_ctx_}.get_str());
  let obj_6_0 = Range::new(
    tok_3_0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_170 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_1 = __tok_rng_2;
  let tok_1_1 = tok_1_1.parse::<i32>(unsafe{&*_ctx_}.get_str());
  let obj_4_0 = Range::new(
    0,
    tok_1_1,
  );
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_171 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Peek::new();
  slots.assign(0, AstSlot(ASTNode::Peek(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_172 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Shift::new();
  slots.assign(0, AstSlot(ASTNode::Shift(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_173 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Skip::new();
  slots.assign(0, AstSlot(ASTNode::Skip(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_174 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Pop::new();
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_175 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Scan::new();
  slots.assign(0, AstSlot(ASTNode::Scan(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_176 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_177 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_178 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_179 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_180 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_181 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_182 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_0 = obj1;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_SyntaxSpec();
  let obj_4_0 = SyntaxField::new(
    obj_0_0,
    obj_2_1,
  );
  slots.assign(0, AstSlot(ASTNode::SyntaxField(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_183 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_184 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_185 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_186 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = AST_Statements::new(
    vec![],
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_187 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_188 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_189 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_190 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_191 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_192 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_193 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_194 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_195 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_196 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_197 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_198 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_199 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_200 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_201 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_202 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_203 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_204 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_205 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_206 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_207 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_208 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_209 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_210 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_211 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_212 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_213 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
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
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<u64>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_217 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_218 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_219 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_220 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = Syntax::new(
    obj_1_0.into_iter().map(|v|match v { ASTNode::SyntaxField(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Syntax(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_221 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_222 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3.to_u64();
  let mut obj_0_0 = obj1.into_u64_vec();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_223 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.to_u64();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::U64Vec(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_224 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_225 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_226 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_227 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_228 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_229 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_230 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_231 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
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
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_0 = obj1;
  let obj_0_0 = obj_0_0.to_Production_Symbol();
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Statement();
  let obj_4_0 = State::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::State(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_236 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_237 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_238 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_239 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_240 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_241 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_242 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_243 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe{&*_ctx_}.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_244 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
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
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_251 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_252 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3.into_nodes();
  let obj_5_1 = true;
  let obj_6_0 = AnyGroup::new(
    obj_2_0,
    obj_5_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnyGroup(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_253 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_1 = false;
  let obj_5_0 = AnyGroup::new(
    obj_1_0,
    obj_4_1,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AnyGroup(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_254 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_255 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_256 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_257 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_258 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_259 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_260 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_261 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_262 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_263 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_264 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_265 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, _, _) = slots.take(3);
  let AstSlot (obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_4_1 = obj5;
  let obj_4_1 = obj_4_1.to_Recovery();
  let obj_1_2 = obj2.into_nodes();
  let obj_3_3 = obj4;
  let obj_3_3 = obj_3_3.to_Syntax();
  let obj_6_0 = Rule::new(
    Some(obj_2_0),
    Some(obj_4_1),
    obj_1_2,
    Some(obj_3_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_266 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Recovery();
  let obj_0_2 = obj1.into_nodes();
  let obj_2_3 = obj3;
  let obj_2_3 = obj_2_3.to_Syntax();
  let obj_5_0 = Rule::new(
    Some(obj_1_0),
    Some(obj_3_1),
    obj_0_2,
    Some(obj_2_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_267 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Recovery();
  let obj_1_2 = obj2.into_nodes();
  let obj_2_3 = obj3;
  let obj_2_3 = obj_2_3.to_Syntax();
  let obj_5_0 = Rule::new(
    None,
    Some(obj_3_1),
    obj_1_2,
    Some(obj_2_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_268 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Recovery();
  let obj_0_2 = obj1.into_nodes();
  let obj_1_3 = obj2;
  let obj_1_3 = obj_1_3.to_Syntax();
  let obj_4_0 = Rule::new(
    None,
    Some(obj_2_1),
    obj_0_2,
    Some(obj_1_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_269 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Recovery();
  let obj_1_2 = obj2.into_nodes();
  let obj_5_0 = Rule::new(
    Some(obj_2_0),
    Some(obj_3_1),
    obj_1_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_270 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Recovery();
  let obj_0_2 = obj1.into_nodes();
  let obj_4_0 = Rule::new(
    Some(obj_1_0),
    Some(obj_2_1),
    obj_0_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_271 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Recovery();
  let obj_1_2 = obj2.into_nodes();
  let obj_4_0 = Rule::new(
    None,
    Some(obj_2_1),
    obj_1_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_272 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Recovery();
  let obj_0_2 = obj1.into_nodes();
  let obj_3_0 = Rule::new(
    None,
    Some(obj_1_1),
    obj_0_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_273 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, _, _) = slots.take(2);
  let AstSlot (obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_1_2 = obj2.into_nodes();
  let obj_3_3 = obj4;
  let obj_3_3 = obj_3_3.to_Syntax();
  let obj_5_0 = Rule::new(
    Some(obj_2_0),
    None,
    obj_1_2,
    Some(obj_3_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_274 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_0_2 = obj1.into_nodes();
  let obj_2_3 = obj3;
  let obj_2_3 = obj_2_3.to_Syntax();
  let obj_4_0 = Rule::new(
    Some(obj_1_0),
    None,
    obj_0_2,
    Some(obj_2_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_275 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_2 = obj2.into_nodes();
  let obj_2_3 = obj3;
  let obj_2_3 = obj_2_3.to_Syntax();
  let obj_4_0 = Rule::new(
    None,
    None,
    obj_1_2,
    Some(obj_2_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_276 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_0_2 = obj1.into_nodes();
  let obj_1_3 = obj2;
  let obj_1_3 = obj_1_3.to_Syntax();
  let obj_3_0 = Rule::new(
    None,
    None,
    obj_0_2,
    Some(obj_1_3),
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_277 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_1_2 = obj2.into_nodes();
  let obj_4_0 = Rule::new(
    Some(obj_2_0),
    None,
    obj_1_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_278 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_0_2 = obj1.into_nodes();
  let obj_3_0 = Rule::new(
    Some(obj_1_0),
    None,
    obj_0_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_279 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_2 = obj2.into_nodes();
  let obj_3_0 = Rule::new(
    None,
    None,
    obj_1_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_280 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_2 = obj1.into_nodes();
  let obj_2_0 = Rule::new(
    None,
    None,
    obj_0_2,
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_281 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_282 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_283 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_284 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<f64>(unsafe{&*_ctx_}.get_str());
  let obj_2_0 = AST_NUMBER::new(
    tok_0_0,
  );
  slots.assign(0, AstSlot(ASTNode::AST_NUMBER(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_285 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_286 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_287 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_288 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_289 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_290 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_291 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_292 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_293 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_294 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_295 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_296 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_297 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_298 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_299 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_300 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_301 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_302 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_303 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_304 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_305 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_306 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_STRING::new(
    None,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_307 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<u64>(unsafe{&*_ctx_}.get_str());
  slots.assign(0, AstSlot(ASTNode::U64(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_308 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_309 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_310 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_311 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_RGBA();
  let obj_3_0 = SyntaxSpec::new(
    tok_0_0,
    Some(obj_1_1),
  );
  slots.assign(0, AstSlot(ASTNode::SyntaxSpec(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_312 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_1 = obj1;
  let obj_0_1 = obj_0_1.to_RGBA();
  let obj_2_0 = SyntaxSpec::new(
    Default::default(),
    Some(obj_0_1),
  );
  slots.assign(0, AstSlot(ASTNode::SyntaxSpec(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_313 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe{&mut*_ctx_}.get_reader_mut());
  let obj_2_0 = SyntaxSpec::new(
    tok_0_0,
    None,
  );
  slots.assign(0, AstSlot(ASTNode::SyntaxSpec(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_314 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_315 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_316 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, _, _) = slots.take(1);
  let AstSlot (_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_317 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_318 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_319 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_320 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_321 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
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

fn reducer_322 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_BOOL::new(
    None,
    false,
    __rule_rng__.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()),
  );
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_323 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let AstSlot (obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_324 <R: Reader + UTF8Reader, M, const UP: bool> (_ctx_: *mut ParseContext<R, M>,slots: &AstStackSlice<AstSlot<ASTNode>, UP>) {
  let AstSlot (_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(0, AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 325]
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
    ])
  }
}

    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader + std::fmt::Debug {}

impl<T: ByteReader + MutByteReader + UTF8Reader + std::fmt::Debug> Reader for T {}

pub type Parser<'a, T, UserCTX> = sherpa_runtime::bytecode_parser::ByteCodeParser<'a, T, UserCTX>;

pub mod meta{
  
  pub const production_names: [&'static str;109] = [
    "ir::production_match_block_group_3",
    "sherpa::recover_definition",
    "ascript::numeric_convert",
    "syntax::red",
    "defined_type_eval::escaped_vals",
    "ir::terminal_match_block_group_3",
    "ascript::struct_prop",
    "sherpa::ast_definition",
    "sherpa::name_clause",
    "defined_type_eval::escaped_string",
    "sherpa::rules_list_1",
    "sherpa::peg_production",
    "sherpa_symbol::production_symbol",
    "ascript::struct",
    "sherpa::import_clause",
    "sherpa::rule_group_1_list_1",
    "syntax::alpha",
    "ir::generic_match_block",
    "ir::match",
    "token::id",
    "sherpa_symbol::end_of_input",
    "ir::statement_group_2",
    "sherpa::append_production",
    "sherpa_symbol::symbol",
    "sherpa_symbol::non_terminal",
    "sherpa::ignore_clause_list_1",
    "ir::terminal_match",
    "ir::terminal_match_block",
    "ascript::convert_initializer",
    "ascript::expression",
    "ascript::reference",
    "ascript::add",
    "ir::goto",
    "syntax::color",
    "sherpa_symbol::terminal",
    "ir::generic_match_block_group_3_list_1",
    "ascript::body_list_1",
    "sherpa::export_clause",
    "sherpa_symbol::class",
    "ir::terminal_statement",
    "sherpa_symbol::token",
    "defined_type_eval::escaped_string_list_1_group_1",
    "ir::int_match",
    "ascript::vector",
    "ir::terminal_match_block_group_3_list_1",
    "ir::non_branch_statement",
    "sherpa::rules",
    "ir::goto_sequence",
    "ascript::token",
    "ir::default_match",
    "ascript::range",
    "ir::transitive_statement",
    "sherpa::import_clause_list_1",
    "ascript::identifier",
    "defined_type_eval::id",
    "syntax::field",
    "ascript::body",
    "ascript::map",
    "ir::production_match_block",
    "sherpa::grammar_list_1",
    "sherpa_symbol::import_production_symbol",
    "ir::statement",
    "sherpa::preamble",
    "ir::hint",
    "sherpa::grammar_list_2",
    "defined_type_eval::escaped_string_list_1",
    "ir::int_match_list_1_group_0",
    "ascript::class_identifier",
    "syntax::declaration_list_1",
    "syntax::declaration",
    "token::quote",
    "ir::int_match_list_1",
    "sherpa_symbol::list",
    "ascript::struct_list_1",
    "defined_type_eval::def_type",
    "ir::state",
    "sherpa::any_group_list_1",
    "sherpa::pratt_production",
    "defined_type_eval::escaped",
    "sherpa::ignore_clause",
    "ir::production_match_block_group_3_list_1",
    "ir::goto_push",
    "sherpa::any_group",
    "sherpa::grammar",
    "token::string",
    "ir::statement_list_1",
    "sherpa_symbol::precedence",
    "syntax::green",
    "ascript::vector_list_1",
    "syntax::blue",
    "ascript::type_identifier",
    "sherpa::rule",
    "sherpa_symbol::token_non_terminal",
    "ascript::literal",
    "ascript::member",
    "sherpa::rule_group_1",
    "sherpa_symbol::annotated_symbol",
    "ascript::string_convert",
    "ir::int_match_list_1_group_2",
    "sherpa::cf_production",
    "defined_type_eval::escaped_string_list_1_group_0",
    "syntax::syntax_spec",
    "ir::non_terminal_match",
    "ir::generic_match_block_group_3",
    "ir::goto_sequence_list_1",
    "ascript::init_objects",
    "ascript::bool_convert",
    "sherpa::syntax_definition",
    "defined_type_eval::num",
  ];
  
  pub const symbol_string: [&'static str;121] = [
    r####"tk:integer_list_1"####,
    r####"'tok'"####,
    r####"'num'"####,
    r####"'nl'"####,
    r####"'id'"####,
    r####"'htab'"####,
    r####"'tab'"####,
    r####"'sp'"####,
    r####"'any'"####,
    r####"'sym'"####,
    r####"'{'"####,
    r####"'.'"####,
    r####"'_'"####,
    r####"')'"####,
    r####"'::'"####,
    r####"']'"####,
    r####"':'"####,
    r####"'}'"####,
    r####"','"####,
    r####"'('"####,
    r####"'-'"####,
    r####""u16""####,
    r####""unordered""####,
    r####""i32""####,
    r####""assign""####,
    r####""i8""####,
    r####""true""####,
    r####""u32""####,
    r####""pop-goto""####,
    r####""reduce""####,
    r####""t_""####,
    r####""str""####,
    r####""rgb""####,
    r####""default""####,
    r####""EXPORT""####,
    r####""i16""####,
    r####""TERMINAL""####,
    r####""peek""####,
    r####""push-goto""####,
    r####""assert""####,
    r####""shift""####,
    r####""skip""####,
    r####""a""####,
    r####""f64""####,
    r####""false""####,
    r####""goto""####,
    r####""r""####,
    r####""fail-hint""####,
    r####""bool""####,
    r####""scan""####,
    r####""g""####,
    r####""fail""####,
    r####""IGNORE""####,
    r####""NAME""####,
    r####""u64""####,
    r####""map""####,
    r####""i64""####,
    r####""u8""####,
    r####""pass""####,
    r####""PRODUCTION""####,
    r####""f32""####,
    r####""tk""####,
    r####""AS""####,
    r####""IMPORT""####,
    r####""token""####,
    r####""accept""####,
    r####""b""####,
    r####""c_""####,
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
    r####"":syn""####,
    r####"""""####,
    r####""$""####,
    r####""<""####,
    r####""(""####,
    r####""=>""####,
    r####"":rec""####,
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
    r####"tk:quote_tok"####,
    r####"tk:identifier_syms"####,
    r####"tk:identifier"####,
    r####"tk:block"####,
    r####"tk:number"####,
    r####"tk:integer"####,
    r####"tk:identifier"####,
    r####"tk:line"####,
    r####"tk:reference"####,
    r####"tk:id_tok"####,
    r####"tk:string_tok"####,
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

pub static bytecode: [u8; 179685] = [
  0,211,200,197,210,208,193,2,15,1,86,0,0,0,17,1,137,0,0,0,1,15,1,86,0,0,0,17,1,72,2,0,0,1,15,1,130,2,0,0,17,1,131,2,0,0,1,15,1,130,2,0,0,17,1,196,2,0,0,1, 
  15,1,12,4,0,0,17,1,48,4,0,0,1,15,1,144,5,0,0,17,1,195,5,0,0,1,20,5,23,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,22,0,0,0,13,20,2,27,0,0,0,0,0,0,0,2, 
  0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,13,21,1,190,1,0,0,144,68,2,0,23,0,0,0,4,0,0,0,16,112,3,128,38,80,196,131,50,208,6,133,58,64,71,132,36,224,3,128,5,232,13,130,6, 
  232,205,126,71,192,9,128,40,192,68,130,41,48,5,128,42,240,69,126,59,176,199,129,76,16,11,128,69,224,8,128,46,96,6,128,63,112,8,129,70,80,9,128,72,48,10,128,75,160,202,128,79,128,11,128,106,240,11,128,123, 
  184,12,128,130,40,13,128,15,1,1,6,0,0,4,17,1,13,6,0,0,1,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,88,6,0,0,4,17,1,175,6,0,0,1,15,1,88,6,0,0,4,17,1,250,6, 
  0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,1,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,88,6,0, 
  0,4,17,1,231,7,0,0,1,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,1,15,1,125,8,0,0,4,17,1,137,8,0,0, 
  1,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,88,6,0,0,4,17,1,176,9,0,0,1,15,1,88, 
  6,0,0,4,17,1,251,9,0,0,1,15,1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,1,6,0,0,4,17,1,145,10,0,0,1,15,1,220,10,0,0,15,1,232,10,0,0,15,1,12,11,0,0,17,1,24, 
  11,0,0,1,15,1,88,11,0,0,4,17,1,100,11,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,1,8,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,123,0, 
  0,0,22,0,0,0,5,17,1,189,14,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,13,21,1,64,0,0,0,71,69,2,0,2,0,0,0,1,0,0, 
  0,129,104,1,128,127,208,192,127,4,19,54,0,0,0,181,0,0,0,1,0,17,1,235,14,0,0,1,4,19,108,0,0,0,68,1,0,0,1,0,17,1,14,15,0,0,1,2,20,4,54,0,0,0,0,0,0,0,1,0, 
  0,0,92,0,0,0,22,0,0,0,15,1,49,15,0,0,15,1,61,15,0,0,15,1,97,15,0,0,15,1,109,15,0,0,5,17,1,121,15,0,0,1,20,2,17,1,0,0,0,0,0,0,5,0,0,0,2,0,0,0, 
  38,0,0,0,85,0,0,0,132,0,0,0,179,0,0,0,226,0,0,0,15,1,49,15,0,0,5,19,4,0,0,0,26,0,0,0,1,0,19,100,0,0,0,53,1,0,0,1,0,19,65,0,0,0,215,0,0,0,1,0, 
  17,1,61,15,0,0,1,15,1,49,15,0,0,5,19,4,0,0,0,25,0,0,0,1,0,19,100,0,0,0,53,1,0,0,1,0,19,65,0,0,0,215,0,0,0,1,0,17,1,61,15,0,0,1,15,1,49,15,0,0, 
  5,19,4,0,0,0,24,0,0,0,1,0,19,100,0,0,0,53,1,0,0,1,0,19,65,0,0,0,215,0,0,0,1,0,17,1,61,15,0,0,1,15,1,49,15,0,0,5,19,4,0,0,0,27,0,0,0,1,0,19, 
  100,0,0,0,53,1,0,0,1,0,19,65,0,0,0,215,0,0,0,1,0,17,1,61,15,0,0,1,15,1,49,15,0,0,5,19,4,0,0,0,28,0,0,0,1,0,19,100,0,0,0,53,1,0,0,1,0,19,65,0, 
  0,0,215,0,0,0,1,0,17,1,61,15,0,0,1,2,21,1,35,0,0,0,156,69,2,0,4,0,0,0,2,0,0,0,128,16,193,128,5,16,1,128,6,16,1,128,132,16,1,128,8,13,21,1,95,1,0,0,209,69, 
  2,0,13,0,0,0,3,0,0,0,112,0,135,130,49,48,2,128,90,48,134,129,67,48,3,128,68,48,196,129,5,240,10,128,6,240,74,128,78,48,69,129,114,208,71,128,122,160,8,128,128,240,10,128,132,240,10,128,134,112, 
  9,128,15,1,225,15,0,0,15,1,177,16,0,0,15,1,213,16,0,0,15,1,225,16,0,0,4,17,1,237,16,0,0,1,15,1,225,15,0,0,15,1,177,16,0,0,15,1,213,16,0,0,15,1,70,17,0,0,4,17, 
  1,82,17,0,0,1,15,1,225,15,0,0,15,1,177,16,0,0,15,1,213,16,0,0,15,1,130,17,0,0,4,17,1,142,17,0,0,1,15,1,225,15,0,0,15,1,177,16,0,0,15,1,213,16,0,0,15,1,206,17, 
  0,0,4,17,1,218,17,0,0,1,15,1,56,18,0,0,15,1,68,18,0,0,15,1,104,18,0,0,4,17,1,116,18,0,0,1,15,1,56,18,0,0,15,1,68,18,0,0,15,1,186,18,0,0,4,17,1,198,18,0, 
  0,1,15,1,56,18,0,0,15,1,68,18,0,0,15,1,12,19,0,0,4,17,1,24,19,0,0,1,15,1,56,18,0,0,15,1,68,18,0,0,15,1,94,19,0,0,4,17,1,106,19,0,0,1,15,1,56,18,0,0, 
  15,1,68,18,0,0,15,1,195,19,0,0,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,207,19,0,0,1,8,2,20,5,23,0,0,0,0,0,0,0,1,0,0,0,1,0, 
  0,0,22,0,0,0,13,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,13,21,1,61,0,0,0,136,70,2,0,3,0,0,0,1,0,0,0,6,224,129,128,5,224,1, 
  128,134,240,0,128,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,207,19,0,0,1,8,2,19,29,0,0,0,77,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1, 
  0,0,0,60,0,0,0,22,0,0,0,15,1,247,19,0,0,5,17,1,3,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,48,0,0,0,165,0,0, 
  0,1,0,1,19,29,0,0,0,73,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,43,20,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0, 
  0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,6,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,178,20,0,0,5, 
  17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,16,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0, 
  40,0,0,0,22,0,0,0,15,1,190,20,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,12,0,0,0,1,0, 
  1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,202,20,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26, 
  0,0,0,9,19,2,0,0,0,8,0,0,0,1,0,1,19,29,0,0,0,72,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,214,20,0,0,5,17,1,55, 
  20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,97,0,0,0,50,1,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0, 
  0,22,0,0,0,15,1,226,20,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,14,0,0,0,1,0,1,20,4, 
  36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,238,20,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0, 
  9,19,2,0,0,0,22,0,0,0,1,0,1,19,29,0,0,0,74,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,250,20,0,0,5,17,1,55,20,0,0, 
  1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,106,0,0,0,66,1,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0, 
  0,0,15,1,6,21,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,10,0,0,0,1,0,1,19,29,0,0,0, 
  79,0,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,5,17,1,18,21,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26, 
  0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,66,23,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26, 
  0,0,0,26,0,0,0,9,19,2,0,0,0,18,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,78,23,0,0,5,17,1,55,20,0,0,1,20,2,27,0, 
  0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,4,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,15,1,90, 
  23,0,0,5,17,1,55,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,2,0,0,0,20,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0, 
  1,0,0,0,60,0,0,0,22,0,0,0,15,1,102,23,0,0,5,17,1,3,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,48,0,0,0,164,0, 
  0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,60,0,0,0,22,0,0,0,15,1,114,23,0,0,5,17,1,3,20,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26, 
  0,0,0,26,0,0,0,9,19,48,0,0,0,166,0,0,0,1,0,1,19,29,0,0,0,78,0,0,0,1,0,1,20,0,35,0,0,0,0,0,0,0,1,0,0,0,31,0,0,0,22,0,0,0,15,1,232,10,0,0, 
  17,1,126,23,0,0,1,2,19,31,0,0,0,83,0,0,0,1,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,36,0,0,0,22,0,0,0,15,1,181,23,0,0,5,17,1,250,23,0,0,1,20,2,27,0, 
  0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,29,0,0,0,76,0,0,0,1,0,1,21,1,88,3,0,0,88,72,2,0,24,0,0,0,4,0,0,0,16,144,3,128,38,144, 
  197,131,50,8,75,133,58,8,140,132,36,144,4,128,5,184,26,130,6,184,218,126,71,128,17,128,40,144,70,130,41,144,7,128,42,8,73,126,59,8,205,129,76,128,20,128,69,128,15,128,46,8,10,128,63,128,14,129,70,128, 
  16,128,72,128,18,128,75,128,19,129,79,128,85,128,95,128,22,128,106,232,22,128,123,64,24,128,130,64,25,128,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,1,6,0,0,4,17,1,13,6,0, 
  0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17, 
  1,175,6,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,15,1,56,24,0,0,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0, 
  0,75,0,0,0,1,0,19,88,0,0,0,6,1,0,0,1,0,17,1,119,24,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,56, 
  24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0, 
  1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,15,1,56,24,0,0,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0, 
  1,0,19,88,0,0,0,6,1,0,0,1,0,17,1,119,24,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,56,24,0,0,15,1, 
  119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,56,24, 
  0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,176,9,0,0,1, 
  15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,1,6,0,0,4,17,1,70, 
  10,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,1,6,0,0,4,17,1,145,10,0,0,1,4,19,43,0,0,0,147,0,0,0,2,0,1,15,1,56,24,0,0,15,1,119,24,0, 
  0,15,1,155,24,0,0,15,1,220,10,0,0,15,1,232,10,0,0,15,1,12,11,0,0,17,1,24,11,0,0,1,15,1,56,24,0,0,15,1,119,24,0,0,15,1,155,24,0,0,15,1,88,11,0,0,4,17,1,100, 
  11,0,0,1,15,1,56,24,0,0,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,88,0,0,0,6,1,0,0,1,0,17,1,119,24,0,0,1,8,2,21,1,45,0,0,0,26, 
  73,2,0,3,0,0,0,1,0,0,0,6,96,1,128,5,96,65,128,45,240,0,128,15,1,167,24,0,0,4,17,1,242,24,0,0,1,8,2,20,5,34,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,22,0,0, 
  0,19,74,0,0,0,233,0,0,0,1,0,1,2,20,5,34,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,22,0,0,0,19,74,0,0,0,234,0,0,0,1,0,1,2,19,9,0,0,0,38,0,0,0,1,0, 
  1,20,0,35,0,0,0,0,0,0,0,1,0,0,0,65,0,0,0,22,0,0,0,15,1,61,15,0,0,17,1,42,25,0,0,1,2,19,65,0,0,0,215,0,0,0,1,0,1,19,100,0,0,0,54,1,0,0,1,0, 
  1,20,2,103,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,51,0,0,0,64,0,0,0,77,0,0,0,90,0,0,0,5,19,78,0,0,0,241,0,0,0,2,0,1,5,19,78,0,0,0,240,0, 
  0,0,2,0,1,5,19,78,0,0,0,239,0,0,0,2,0,1,5,19,78,0,0,0,242,0,0,0,2,0,1,5,19,78,0,0,0,243,0,0,0,2,0,1,2,21,1,207,0,0,0,94,76,2,0,9,0,0,0,3, 
  0,0,0,112,128,194,129,114,80,131,128,90,176,193,127,122,32,4,128,132,112,6,128,5,112,6,128,6,112,134,128,128,112,6,128,134,240,4,128,15,1,126,25,0,0,15,1,68,18,0,0,15,1,104,18,0,0,4,17,1, 
  116,18,0,0,1,15,1,126,25,0,0,15,1,68,18,0,0,15,1,186,18,0,0,4,17,1,198,18,0,0,1,15,1,126,25,0,0,15,1,68,18,0,0,15,1,12,19,0,0,4,17,1,24,19,0,0,1,15,1,126, 
  25,0,0,15,1,68,18,0,0,15,1,94,19,0,0,4,17,1,106,19,0,0,1,15,1,126,25,0,0,15,1,68,18,0,0,15,1,195,19,0,0,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0, 
  0,0,1,0,17,1,207,19,0,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,59,0,0,0,22,0,0,0,15,1,177,16,0,0,17,1,138,25,0,0,1,2,19,59,0,0,0,190,0,0,0,1,0, 
  1,19,62,0,0,0,199,0,0,0,1,0,1,21,1,88,0,0,0,241,76,2,0,6,0,0,0,2,0,0,0,108,80,193,128,5,184,2,128,6,184,194,128,128,184,66,128,132,184,2,128,134,240,1,128,15,1,205,25,0, 
  0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,25,29,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,8,2,19,62,0,0,0,202,0,0,0,1,0,1,21,1,47,0,0,0, 
  95,77,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,129,128,6,112,1,128,89,48,1,128,132,112,1,128,4,17,1,163,29,0,0,1,8,2,19,62,0,0,0,201,0,0,0,1,0,1,21,1,63,0,0,0, 
  159,77,2,0,5,0,0,0,2,0,0,0,128,240,193,128,5,240,1,128,6,240,129,128,132,240,1,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,8,0,0,0,37,0,0,0,2,0,1,8,2,19,62, 
  0,0,0,200,0,0,0,1,0,1,21,1,93,0,0,0,2,78,2,0,6,0,0,0,2,0,0,0,128,224,66,129,5,224,2,128,2,80,129,128,3,24,2,128,6,224,2,128,132,224,2,128,15,1,169,30,0,0,4,19, 
  52,0,0,0,179,0,0,0,1,0,17,1,213,30,0,0,1,15,1,169,30,0,0,4,19,52,0,0,0,178,0,0,0,1,0,17,1,213,30,0,0,1,8,2,19,83,0,0,0,255,0,0,0,1,0,1,20,0,35,0, 
  0,0,0,0,0,0,1,0,0,0,64,0,0,0,22,0,0,0,15,1,68,18,0,0,17,1,249,30,0,0,1,2,19,64,0,0,0,211,0,0,0,1,0,1,21,1,69,0,0,0,159,77,2,0,5,0,0,0,2,0, 
  0,0,128,32,194,128,5,32,2,128,6,32,130,128,132,32,2,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,64,31,0,0,1,8,2,19,64,0,0,0,210, 
  0,0,0,1,0,1,21,1,69,0,0,0,159,77,2,0,5,0,0,0,2,0,0,0,128,32,194,128,5,32,2,128,6,32,130,128,132,32,2,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0, 
  0,42,0,0,0,1,0,17,1,112,31,0,0,1,8,2,19,64,0,0,0,209,0,0,0,1,0,1,21,1,69,0,0,0,159,77,2,0,5,0,0,0,2,0,0,0,128,32,194,128,5,32,2,128,6,32,130,128,132,32, 
  2,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,160,31,0,0,1,8,2,19,64,0,0,0,212,0,0,0,1,0,1,21,1,88,0,0,0,241,76,2,0, 
  6,0,0,0,2,0,0,0,108,80,193,128,5,184,2,128,6,184,194,128,128,184,66,128,132,184,2,128,134,240,1,128,15,1,208,31,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,0,32,0,0,4,19, 
  19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,8,2,19,64,0,0,0,213,0,0,0,1,0,1,21,1,39,0,0,0,95,78,2,0,3,0,0,0,1,0,0,0,6,48,1,128,5,48,65,128,109,240,0, 
  128,4,17,1,79,32,0,0,1,8,2,19,48,0,0,0,162,0,0,0,2,0,1,21,1,39,0,0,0,92,80,2,0,3,0,0,0,1,0,0,0,6,48,129,128,5,48,1,128,130,240,0,128,4,17,1,176,34,0,0, 
  1,8,2,19,2,0,0,0,5,0,0,0,2,0,1,21,1,122,0,0,0,169,80,2,0,6,0,0,0,2,0,0,0,16,80,1,129,5,200,3,128,6,200,195,128,79,144,2,128,76,240,1,128,106,48,3,128,15,1,251, 
  34,0,0,15,1,58,35,0,0,4,17,1,13,6,0,0,1,15,1,251,34,0,0,15,1,58,35,0,0,4,17,1,70,10,0,0,1,15,1,251,34,0,0,15,1,58,35,0,0,4,17,1,145,10,0,0,1,15,1,251, 
  34,0,0,15,1,70,35,0,0,17,1,24,11,0,0,1,8,2,19,2,0,0,0,15,0,0,0,2,0,1,19,2,0,0,0,11,0,0,0,2,0,1,19,2,0,0,0,7,0,0,0,2,0,1,19,97,0,0,0,49, 
  1,0,0,2,0,1,19,2,0,0,0,13,0,0,0,2,0,1,19,2,0,0,0,21,0,0,0,2,0,1,19,106,0,0,0,65,1,0,0,2,0,1,19,2,0,0,0,9,0,0,0,2,0,1,21,1,47,2,0,0, 
  144,68,2,0,23,0,0,0,4,0,0,0,16,112,3,128,38,176,196,131,50,32,8,133,58,192,72,132,36,16,4,128,5,112,17,130,6,112,209,126,71,48,12,128,40,80,69,130,41,240,5,128,42,224,70,126,59,96,201,129, 
  76,16,14,128,69,240,10,128,46,128,7,128,63,80,10,129,70,144,11,128,72,208,12,128,75,112,205,128,79,176,14,128,106,80,15,128,123,232,15,128,130,128,16,128,15,1,82,35,0,0,15,1,1,6,0,0,4,17,1,13, 
  6,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,175,6,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,250, 
  6,0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,17,1,82,35,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,82,35,0,0, 
  15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0, 
  27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,17,1,82,35,0,0,1,15,1,82,35,0,0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,212, 
  8,0,0,1,15,1,82,35,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,176, 
  9,0,0,1,15,1,82,35,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,82,35,0,0,15,1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,82,35,0,0,15,1,1,6,0,0,4,17,1,145, 
  10,0,0,1,15,1,82,35,0,0,15,1,220,10,0,0,17,1,140,35,0,0,1,15,1,82,35,0,0,15,1,88,11,0,0,17,1,216,35,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75, 
  0,0,0,1,0,17,1,82,35,0,0,1,8,2,19,2,0,0,0,17,0,0,0,2,0,1,19,2,0,0,0,3,0,0,0,2,0,1,19,2,0,0,0,19,0,0,0,2,0,1,19,48,0,0,0,161,0,0,0,2, 
  0,1,19,48,0,0,0,163,0,0,0,2,0,1,21,1,47,0,0,0,233,80,2,0,5,0,0,0,2,0,0,0,100,48,193,128,5,112,1,128,6,112,1,128,128,112,65,128,132,112,1,128,12,17,1,18,36,0,0,1, 
  10,12,17,1,76,36,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,46,0,0,0,22,0,0,0,5,17,1,78,36,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0, 
  0,26,0,0,0,9,19,94,0,0,0,29,1,0,0,1,0,1,21,1,61,0,0,0,190,81,2,0,4,0,0,0,2,0,0,0,130,16,1,128,5,224,1,128,6,224,129,127,131,120,1,128,4,19,30,0,0,0,81,0, 
  0,0,2,0,1,4,19,30,0,0,0,80,0,0,0,2,0,1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,93,0,0,0,22,0,0,0,5,19,43,0,0,0,146,0,0,0,3,0,1,20,2,27,0,0, 
  0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,88,0,0,0,22,0,0,0,15,1,119,24,0,0,17,1,134,36,0,0,1,2,19, 
  88,0,0,0,6,1,0,0,1,0,1,21,4,47,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,44,56,1,128,125,208,0,128,5,19,13,0,0,0,44,0,0,0,3,0,1,5,17,1,211,36,0,0,1,20,2, 
  27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,55,0,0,0,81,82,2,0,3,0,0,0,1,0,0,0,6,176,129,128,5,176,1,128,126,240,0,128,4,19,53,0, 
  0,0,180,0,0,0,1,0,19,90,0,0,0,8,1,0,0,2,0,1,8,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,12,17,1,168,37,0,0,1,20,2,46,0,0,0,0,0, 
  0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,12,17,1,168,37,0,0,1,12,17,1,76,36,0,0,1,19,83,0,0,0,254,0,0,0,2,0,1,21,1, 
  59,0,0,0,209,69,2,0,8,0,0,0,3,0,0,0,128,208,1,128,49,144,1,128,78,144,1,128,67,144,1,128,68,144,193,128,5,208,1,128,6,208,1,127,132,208,1,128,12,17,1,168,38,0,0,1,10,12,17,1, 
  76,36,0,0,1,21,1,59,0,0,0,46,86,2,0,6,0,0,0,2,0,0,0,128,208,65,129,5,208,129,128,6,208,1,128,77,80,65,128,85,144,1,128,132,208,1,128,4,17,1,44,39,0,0,1,4,17,1,108,39, 
  0,0,1,8,19,37,0,0,0,129,0,0,0,2,0,1,21,0,55,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,12,240,64,128,24,80,65,128,60,88,1,128,19,24,0,0,0,64,0,0,0,1,0,1,1,19, 
  24,0,0,0,65,0,0,0,1,0,1,2,21,1,204,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,176,196,129,111,176,138,130,123,48,13,128,108,48,136,129,5,88,22,127,6,88,86,129,103,224, 
  197,126,125,240,14,128,128,88,22,128,132,88,22,128,134,64,18,128,135,8,19,128,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1, 
  137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0, 
  15,1,205,40,0,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41, 
  0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1, 
  217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,172,39,0,0, 
  15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49, 
  0,0,4,17,1,106,49,0,0,1,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1, 
  51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,47,51, 
  0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,172,39,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137, 
  41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0, 
  17,1,132,42,0,0,1,8,2,21,0,78,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,12,16,129,128,37,216,1,128,24,160,65,128,60,224,1,128,19,24,0,0,0,64,0,0,0,1,0,17,1,25,29,0,0, 
  1,17,1,67,52,0,0,1,1,19,24,0,0,0,65,0,0,0,1,0,17,1,25,29,0,0,1,2,21,1,47,0,0,0,47,87,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,129,128,6,112,1,128,29,48, 
  1,128,132,112,1,128,4,17,1,131,52,0,0,1,8,19,12,0,0,0,42,0,0,0,1,0,1,21,1,5,1,0,0,18,88,2,0,9,0,0,0,3,0,0,0,88,176,193,128,111,128,195,129,125,128,4,128,128,32,8, 
  128,132,32,8,128,5,32,72,127,6,32,8,128,103,176,130,126,135,80,6,128,15,1,187,52,0,0,15,1,240,52,0,0,15,1,20,53,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,187,52,0,0,15, 
  1,240,52,0,0,15,1,20,53,0,0,4,17,1,213,45,0,0,1,15,1,187,52,0,0,15,1,240,52,0,0,15,1,20,53,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,187,52,0,0,4,19,70, 
  0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,25,0,0,0,68,0,0,0,1,0,17,1,240,52,0,0,1,15,1,187,52,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,25,0,0,0,68,0,0,0,1,0,17,1,240,52,0,0,1,8,2,21,1,43,0,0,0,156,69,2,0,4, 
  0,0,0,2,0,0,0,128,80,193,128,5,80,1,128,6,16,1,128,132,80,1,128,4,17,1,32,53,0,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,52,0,0,0,22,0,0,0,15,1,213,30,0, 
  0,17,1,92,53,0,0,1,2,21,1,63,0,0,0,94,76,2,0,9,0,0,0,3,0,0,0,112,176,193,129,114,176,129,128,90,176,193,127,122,176,1,128,132,240,1,128,5,240,1,128,6,240,129,128,128,240,1,128,134, 
  176,1,128,12,17,1,147,53,0,0,1,10,12,17,1,76,36,0,0,1,21,1,47,0,0,0,167,88,2,0,5,0,0,0,2,0,0,0,120,48,193,128,5,112,1,128,6,112,1,128,128,112,65,128,132,112,1,128,4,17, 
  1,39,54,0,0,1,8,2,21,1,47,0,0,0,167,88,2,0,5,0,0,0,2,0,0,0,120,48,193,128,5,112,1,128,6,112,1,128,128,112,65,128,132,112,1,128,4,17,1,244,56,0,0,1,8,2,21,1,47,0, 
  0,0,167,88,2,0,5,0,0,0,2,0,0,0,120,48,193,128,5,112,1,128,6,112,1,128,128,112,65,128,132,112,1,128,4,17,1,193,59,0,0,1,8,2,21,1,47,0,0,0,167,88,2,0,5,0,0,0,2,0, 
  0,0,120,48,193,128,5,112,1,128,6,112,1,128,128,112,65,128,132,112,1,128,4,17,1,142,62,0,0,1,8,2,21,0,78,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,12,16,65,128,24,168,129,128,22,160, 
  1,128,60,224,1,128,19,24,0,0,0,64,0,0,0,1,0,17,1,0,32,0,0,1,1,17,1,208,31,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,0,32,0,0,1,2,21,1,96,2,0,0,5,89,2, 
  0,17,0,0,0,4,0,0,0,64,32,204,131,53,224,6,128,66,232,12,128,83,56,17,128,52,24,6,131,5,248,18,127,6,248,18,129,39,176,130,129,56,88,10,128,73,88,14,128,54,64,8,128,43,0,4,128,44,200,132, 
  128,55,144,9,128,60,32,11,128,80,200,15,128,84,136,18,128,15,1,91,65,0,0,15,1,103,65,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15, 
  1,91,65,0,0,4,19,51,0,0,0,174,0,0,0,1,0,17,1,129,66,0,0,1,15,1,91,65,0,0,15,1,23,68,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17, 
  1,93,66,0,0,1,15,1,91,65,0,0,4,19,51,0,0,0,171,0,0,0,1,0,17,1,13,69,0,0,1,15,1,91,65,0,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0, 
  0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,91,65,0,0,15,1,106,71,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,91, 
  65,0,0,4,19,51,0,0,0,172,0,0,0,1,0,17,1,96,72,0,0,1,15,1,91,65,0,0,4,19,51,0,0,0,173,0,0,0,1,0,17,1,246,73,0,0,1,15,1,91,65,0,0,15,1,163,70,0,0,15, 
  1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,15,1,91,65,0,0,4,19,51,0,0,0,175,0,0,0,1,0,17,1,233,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0, 
  0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,19,75,0,0,0,235,0,0,0,3,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0, 
  0,0,1,0,19,75,0,0,0,235,0,0,0,3,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,19,75,0,0,0,235,0,0,0,3, 
  0,1,15,1,91,65,0,0,15,1,127,77,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,117,78,0,0,17,1,11,79,0,0,1,8,2, 
  21,4,47,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,44,208,64,128,62,16,1,128,5,17,1,69,79,0,0,1,5,19,50,0,0,0,170,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0, 
  0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,28,0,0,0,71,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0, 
  2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,105,0,0,0,64,1,0,0,1,0,1,19,105,0,0,0,63,1,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,44,0,0,0, 
  22,0,0,0,5,17,1,109,79,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,48,0,0,0,0,0,0,0,1,0,0,0,36,0,0,0,22,0, 
  0,0,15,1,232,10,0,0,15,1,12,11,0,0,15,1,181,23,0,0,5,17,1,250,23,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,30,0, 
  0,0,0,0,0,0,1,0,0,0,91,0,0,0,22,0,0,0,5,17,1,100,11,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,30,0,0,0, 
  0,0,0,0,1,0,0,0,43,0,0,0,22,0,0,0,5,17,1,157,81,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,14,1,21,1,55,0,0,0, 
  81,82,2,0,3,0,0,0,1,0,0,0,6,176,129,128,5,176,1,128,126,240,0,128,4,19,53,0,0,0,180,0,0,0,1,0,19,94,0,0,0,30,1,0,0,3,0,1,8,2,21,4,42,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,44,16,1,128,93,208,0,128,12,17,1,76,36,0,0,1,12,17,1,220,83,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12, 
  17,1,76,36,0,0,1,21,1,212,0,0,0,219,90,2,0,7,0,0,0,2,0,0,0,16,112,1,129,5,152,6,128,6,152,198,128,79,112,3,128,76,112,2,128,82,112,68,128,126,112,5,128,15,1,22,84,0,0,15, 
  1,85,84,0,0,15,1,121,84,0,0,15,1,133,84,0,0,4,17,1,13,6,0,0,1,15,1,22,84,0,0,15,1,85,84,0,0,15,1,121,84,0,0,15,1,133,84,0,0,4,17,1,70,10,0,0,1,15,1,22, 
  84,0,0,15,1,85,84,0,0,15,1,121,84,0,0,15,1,133,84,0,0,4,17,1,145,10,0,0,1,15,1,22,84,0,0,15,1,85,84,0,0,15,1,121,84,0,0,15,1,145,84,0,0,4,17,1,157,84,0,0, 
  1,15,1,22,84,0,0,15,1,85,84,0,0,15,1,121,84,0,0,4,19,53,0,0,0,180,0,0,0,1,0,17,1,213,84,0,0,1,8,2,20,4,42,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0, 
  0,0,15,1,26,85,0,0,15,1,38,85,0,0,5,17,1,121,15,0,0,1,20,2,213,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,73,0,0,0,108,0,0,0,143,0,0,0,178,0,0,0, 
  5,19,4,0,0,0,26,0,0,0,1,0,19,41,0,0,0,143,0,0,0,1,0,19,65,0,0,0,214,0,0,0,2,0,1,5,19,4,0,0,0,25,0,0,0,1,0,19,41,0,0,0,143,0,0,0,1,0,19,65, 
  0,0,0,214,0,0,0,2,0,1,5,19,4,0,0,0,24,0,0,0,1,0,19,41,0,0,0,143,0,0,0,1,0,19,65,0,0,0,214,0,0,0,2,0,1,5,19,4,0,0,0,27,0,0,0,1,0,19,41,0, 
  0,0,143,0,0,0,1,0,19,65,0,0,0,214,0,0,0,2,0,1,5,19,4,0,0,0,28,0,0,0,1,0,19,41,0,0,0,143,0,0,0,1,0,19,65,0,0,0,214,0,0,0,2,0,1,2,21,1,131,0, 
  0,0,65,95,2,0,8,0,0,0,3,0,0,0,128,16,4,128,49,144,1,128,78,112,3,128,67,48,2,128,68,208,194,128,5,16,4,128,6,16,4,127,132,16,4,128,15,1,50,85,0,0,15,1,225,16,0,0,4,17, 
  1,237,16,0,0,1,15,1,50,85,0,0,15,1,70,17,0,0,4,17,1,82,17,0,0,1,15,1,50,85,0,0,15,1,130,17,0,0,4,17,1,142,17,0,0,1,15,1,50,85,0,0,15,1,206,17,0,0,4,17, 
  1,218,17,0,0,1,8,2,21,1,63,0,0,0,159,77,2,0,5,0,0,0,2,0,0,0,128,240,193,128,5,240,1,128,6,240,129,128,132,240,1,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,37, 
  0,0,0,127,0,0,0,4,0,1,8,2,21,1,63,0,0,0,159,77,2,0,5,0,0,0,2,0,0,0,128,240,193,128,5,240,1,128,6,240,129,128,132,240,1,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0, 
  1,0,19,37,0,0,0,128,0,0,0,4,0,1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,24,0,0,0,66,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0, 
  0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,46,0,0,0,158,0,0,0,1,0,1,21,0,213,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,24,152,131,129,23,48,195,129,10,208,1, 
  128,91,32,5,128,12,56,66,128,60,40,4,128,72,184,196,128,15,200,130,126,95,176,5,128,96,24,6,128,15,1,247,39,0,0,17,1,62,85,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,247,39,0,0,1, 
  15,1,247,39,0,0,17,1,117,85,0,0,1,15,1,247,39,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,247,39,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,247,39,0, 
  0,1,15,1,247,39,0,0,17,1,81,86,0,0,1,19,10,0,0,0,40,0,0,0,1,0,17,1,247,39,0,0,1,15,1,247,39,0,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,247, 
  39,0,0,1,2,19,10,0,0,0,40,0,0,0,1,0,1,21,1,89,0,0,0,63,97,2,0,7,0,0,0,2,0,0,0,104,224,65,129,5,192,2,128,6,192,130,128,99,112,1,128,110,80,2,128,128,192,66,128,132, 
  192,2,128,15,1,217,86,0,0,4,17,1,44,87,0,0,1,15,1,31,90,0,0,4,17,1,96,90,0,0,1,15,1,150,90,0,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,24,1,0,0,1,0,1,21,1, 
  63,0,0,0,128,97,2,0,5,0,0,0,2,0,0,0,128,240,1,129,5,240,1,128,6,240,65,128,106,48,1,128,132,240,1,128,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1, 
  8,19,95,0,0,0,32,1,0,0,1,0,1,21,0,157,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,104,2,128,24,208,130,128,15,0,130,127,60,96,67,128,72,240,67,128,96,88,4,128,19, 
  24,0,0,0,64,0,0,0,1,0,17,1,137,41,0,0,1,15,1,137,41,0,0,17,1,210,90,0,0,1,15,1,137,41,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,137,41,0,0, 
  1,19,24,0,0,0,65,0,0,0,1,0,17,1,137,41,0,0,1,15,1,137,41,0,0,17,1,81,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,137,41,0,0,1,2,19,15,0,0,0,49,0,0,0, 
  1,0,1,21,1,69,0,0,0,192,97,2,0,5,0,0,0,2,0,0,0,89,112,1,129,5,32,194,127,6,32,2,128,87,48,1,128,133,224,1,128,4,17,1,93,91,0,0,1,15,1,162,91,0,0,4,17,1,225,91, 
  0,0,1,4,17,1,9,92,0,0,1,8,19,96,0,0,0,48,1,0,0,1,0,1,21,1,51,0,0,0,11,98,2,0,4,0,0,0,2,0,0,0,113,16,193,128,5,144,193,127,6,144,1,128,121,80,1,128,4,17, 
  1,96,92,0,0,1,4,17,1,227,92,0,0,1,8,19,72,0,0,0,230,0,0,0,1,0,1,19,23,0,0,0,63,0,0,0,1,0,1,19,34,0,0,0,116,0,0,0,1,0,1,21,1,80,0,0,0,64,98,2, 
  0,4,0,0,0,2,0,0,0,108,16,1,128,5,120,2,128,6,120,66,128,134,176,1,128,15,1,102,93,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,114,93,0,0,4,19,19,0,0,0,56,0,0, 
  0,1,0,17,1,16,52,0,0,1,8,2,21,0,173,0,0,0,0,0,0,0,9,0,0,0,3,0,0,0,24,16,67,129,23,168,66,129,60,160,3,128,91,152,4,128,12,176,129,127,72,48,196,128,95,160,4,128,15,64, 
  130,126,96,216,4,128,19,24,0,0,0,64,0,0,0,1,0,17,1,44,43,0,0,1,15,1,44,43,0,0,17,1,198,93,0,0,1,15,1,44,43,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1, 
  0,17,1,44,43,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,44,43,0,0,1,15,1,44,43,0,0,17,1,81,86,0,0,1,1,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1, 
  44,43,0,0,1,2,21,1,250,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,32,135,130,125,224,9,128,123,224,8,128,108,96,69,129,5,200,79,127,6,200,15,129,103,208,131,126,128,200,15,128, 
  132,200,15,128,134,112,12,128,135,56,13,128,15,1,98,94,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0, 
  4,17,1,219,42,0,0,1,15,1,98,94,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1, 
  98,94,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,98,94,0,0, 
  15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,98,94,0,0,15,1,62,41, 
  0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,98,94,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,199,94,0,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,98,94,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0, 
  1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,204,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,176,196,129,111, 
  176,138,130,123,48,13,128,108,48,136,129,5,88,22,127,6,88,86,129,103,224,197,126,125,240,14,128,128,88,22,128,132,88,22,128,134,64,18,128,135,8,19,128,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0, 
  0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42, 
  0,0,1,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15, 
  1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,117,95,0, 
  0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0, 
  0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39, 
  42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15, 
  1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0, 
  0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19, 
  23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,180,95,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,117,95,0,0,15,1,235,39,0,0,15,1,247,39,0,0, 
  15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,19,23,0,0,0,62,0,0,0,1,0,1,19,34,0,0,0,117,0,0,0,1,0,1,21,1,163,0,0,0, 
  75,99,2,0,10,0,0,0,3,0,0,0,24,168,4,128,17,208,1,128,18,56,2,128,19,160,2,128,20,8,3,128,5,16,197,128,6,16,197,128,23,64,4,128,21,112,3,128,22,216,3,128,4,19,38,0,0,0,130,0, 
  0,0,2,0,1,4,19,38,0,0,0,131,0,0,0,2,0,1,4,19,38,0,0,0,133,0,0,0,2,0,1,4,19,38,0,0,0,137,0,0,0,2,0,1,4,19,38,0,0,0,136,0,0,0,2,0,1,4,19,38, 
  0,0,0,132,0,0,0,2,0,1,4,19,38,0,0,0,135,0,0,0,2,0,1,4,19,38,0,0,0,134,0,0,0,2,0,1,8,2,19,15,0,0,0,50,0,0,0,1,0,1,21,1,196,1,0,0,188,99,2,0, 
  12,0,0,0,3,0,0,0,88,128,2,130,37,16,130,128,111,0,71,130,125,144,8,128,108,112,69,129,5,24,14,127,6,24,14,129,103,16,196,126,128,24,14,128,132,24,14,128,134,240,10,128,135,184,11,128,4,15,1,154, 
  96,0,0,17,1,61,97,0,0,1,15,1,240,98,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1, 
  15,1,240,98,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,240,98,0,0,15,1,37,99,0,0,15,1,195,99, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,240,98,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,240,98,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0, 
  19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,154,96,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1, 
  104,29,0,0,1,15,1,240,98,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,224,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,24,184,3,130,23,80,3,130,10,240,1,128,91,120,5,128,12,88,66, 
  128,60,128,4,128,46,72,4,128,15,232,130,126,72,16,133,128,95,8,6,128,96,112,6,128,15,1,47,51,0,0,17,1,207,99,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,47,51,0,0,1,15,1,47,51, 
  0,0,17,1,11,100,0,0,1,15,1,47,51,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,47,51,0,0,1,17,1,172,39,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1, 
  47,51,0,0,1,15,1,47,51,0,0,17,1,81,86,0,0,1,19,10,0,0,0,40,0,0,0,1,0,17,1,47,51,0,0,1,15,1,47,51,0,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0, 
  17,1,47,51,0,0,1,2,21,1,39,0,0,0,102,100,2,0,3,0,0,0,1,0,0,0,6,48,1,128,5,48,65,128,29,240,0,128,4,17,1,131,52,0,0,1,8,19,12,0,0,0,42,0,0,0,1,0,1,21, 
  1,51,0,0,0,167,100,2,0,6,0,0,0,2,0,0,0,128,144,65,129,5,144,129,128,6,144,1,128,77,80,65,128,85,80,1,128,132,144,1,128,12,17,1,167,100,0,0,1,10,12,19,37,0,0,0,129,0,0,0, 
  2,0,1,21,1,55,0,0,0,136,70,2,0,3,0,0,0,1,0,0,0,6,176,129,128,5,176,1,128,134,240,0,128,4,19,19,0,0,0,56,0,0,0,1,0,19,60,0,0,0,191,0,0,0,3,0,1,8,2,21, 
  1,52,0,0,0,88,102,2,0,5,0,0,0,2,0,0,0,128,152,1,129,5,152,129,128,6,152,1,128,97,48,1,128,132,152,1,128,4,19,79,0,0,0,244,0,0,0,4,0,1,8,2,20,0,35,0,0,0,0,0, 
  0,0,1,0,0,0,25,0,0,0,22,0,0,0,15,1,240,52,0,0,17,1,227,100,0,0,1,2,19,25,0,0,0,68,0,0,0,1,0,1,21,1,59,0,0,0,46,86,2,0,6,0,0,0,2,0,0,0,128,208, 
  65,129,5,208,129,128,6,208,1,128,77,80,65,128,85,144,1,128,132,208,1,128,4,17,1,42,101,0,0,1,4,17,1,106,101,0,0,1,8,2,21,1,47,0,0,0,2,78,2,0,5,0,0,0,2,0,0,0,128,112, 
  1,129,5,112,1,128,2,48,1,128,3,48,1,128,132,112,1,128,12,17,1,170,101,0,0,1,10,12,17,1,76,36,0,0,1,21,1,147,0,0,0,94,76,2,0,9,0,0,0,3,0,0,0,112,32,194,129,114,144,130, 
  128,90,176,193,127,122,0,3,128,132,144,4,128,5,144,4,128,6,144,132,128,128,144,4,128,134,112,3,128,15,1,240,101,0,0,4,17,1,116,18,0,0,1,15,1,252,101,0,0,4,17,1,198,18,0,0,1,15,1,8, 
  102,0,0,4,17,1,24,19,0,0,1,15,1,20,102,0,0,4,17,1,106,19,0,0,1,15,1,32,102,0,0,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0,0,0,1,0,17,1,207,19,0,0, 
  1,8,2,21,1,204,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,176,196,129,111,176,138,130,123,48,13,128,108,48,136,129,5,88,22,127,6,88,86,129,103,224,197,126,125,240,14,128,128,88,22, 
  128,132,88,22,128,134,64,18,128,135,8,19,128,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,4,15,1, 
  44,43,0,0,17,1,218,43,0,0,1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0, 
  0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247, 
  39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0, 
  1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0, 
  0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,56,102,0,0,4,19,19,0,0,0,56, 
  0,0,0,1,0,17,1,104,29,0,0,1,15,1,44,102,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0, 
  15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2, 
  21,1,204,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,176,196,129,111,176,138,130,123,48,13,128,108,48,136,129,5,88,22,127,6,88,86,129,103,224,197,126,125,240,14,128,128,88,22,128,132,88, 
  22,128,134,64,18,128,135,8,19,128,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,4,15,1,44,43,0, 
  0,17,1,218,43,0,0,1,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1, 
  137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0, 
  15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0, 
  0,1,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1, 
  35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,47,103,0,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,35,103,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51, 
  42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,204, 
  2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,176,196,129,111,176,138,130,123,48,13,128,108,48,136,129,5,88,22,127,6,88,86,129,103,224,197,126,125,240,14,128,128,88,22,128,132,88,22,128,134, 
  64,18,128,135,8,19,128,15,1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,4,15,1,44,43,0,0,17,1, 
  218,43,0,0,1,15,1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0, 
  0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205, 
  40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15, 
  1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,26,104,0, 
  0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1, 
  0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,38,104,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17, 
  1,104,29,0,0,1,15,1,26,104,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,204,2,0,0, 
  123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,176,196,129,111,176,138,130,123,48,13,128,108,48,136,129,5,88,22,127,6,88,86,129,103,224,197,126,125,240,14,128,128,88,22,128,132,88,22,128,134,64,18,128, 
  135,8,19,128,15,1,17,105,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,17,105,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,4,15,1,44,43,0,0,17,1,218,43,0, 
  0,1,15,1,17,105,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,17,105,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1, 
  39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,17,105,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0, 
  15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,17,105, 
  0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,17,105,0,0,15,1, 
  235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40, 
  0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,29,105,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29, 
  0,0,1,15,1,17,105,0,0,15,1,235,39,0,0,15,1,247,39,0,0,15,1,205,40,0,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84, 
  0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,19,75,0,0,0,235,0,0,0, 
  3,0,1,21,1,234,0,0,0,197,103,2,0,8,0,0,0,3,0,0,0,80,192,5,128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7,128,84,216,6,128,15,1,8,106,0,0,15, 
  1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,8,106,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39, 
  0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0, 
  193,0,0,0,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,15,1,20,106,0,0,17,1,11,79,0,0,1,8,19,61,0, 
  0,0,197,0,0,0,1,0,1,20,0,35,0,0,0,0,0,0,0,1,0,0,0,85,0,0,0,22,0,0,0,15,1,93,66,0,0,17,1,148,106,0,0,1,2,21,1,138,1,0,0,42,104,2,0,12,0,0,0,3, 
  0,0,0,80,160,9,128,73,136,8,128,66,112,7,128,83,184,10,128,44,48,131,129,5,72,204,128,6,72,204,128,39,16,2,128,53,80,4,128,54,128,5,128,60,160,70,128,84,216,11,128,15,1,215,106,0,0,4,19,45, 
  0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,205,107,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66, 
  0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,207,108,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19, 
  85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0, 
  60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,140, 
  0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,15,1,197,109,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93, 
  66,0,0,1,4,15,1,187,110,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,198,0,0,0,1,0,1,21,1,234,0,0,0,197,103,2,0,8,0,0,0,3,0,0,0,80,192,5,128,73,168,4,128,66,144,3, 
  128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7,128,84,216,6,128,15,1,8,106,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15, 
  1,8,106,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1, 
  4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61, 
  0,0,0,193,0,0,0,2,0,1,4,15,1,59,111,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,197,0,0,0,1,0,1,21,1,138,1,0,0,42,104,2,0,12,0,0,0,3,0,0,0,80,160,9,128,73, 
  136,8,128,66,112,7,128,83,184,10,128,44,48,131,129,5,72,204,128,6,72,204,128,39,16,2,128,53,80,4,128,54,128,5,128,60,160,70,128,84,216,11,128,15,1,215,106,0,0,4,19,45,0,0,0,155,0,0,0,1, 
  0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,205,107,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,195,108,0, 
  0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,207,108,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0, 
  1,0,17,1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61, 
  0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0, 
  0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,15,1,197,109,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,187, 
  111,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,198,0,0,0,1,0,1,19,61,0,0,0,195,0,0,0,1,0,1,19,21,0,0,0,59,0,0,0,1,0,1,21,1,45,0,0,0,167,104,2,0,3,0,0, 
  0,1,0,0,0,6,96,129,128,5,96,1,128,60,240,0,128,15,1,59,112,0,0,4,17,1,152,75,0,0,1,8,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0,15,1,233,70,0, 
  0,17,1,71,112,0,0,1,2,19,104,0,0,0,62,1,0,0,1,0,1,21,1,80,0,0,0,64,98,2,0,4,0,0,0,2,0,0,0,108,16,1,128,5,120,2,128,6,120,66,128,134,176,1,128,15,1,118,112,0, 
  0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,130,112,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,8,2,21,1,234,0,0,0,197,103,2,0,8,0,0,0,3,0,0,0, 
  80,192,5,128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7,128,84,216,6,128,15,1,8,106,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71, 
  0,0,4,17,1,25,71,0,0,1,15,1,8,106,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19, 
  61,0,0,0,193,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21, 
  0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,15,1,214,112,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,197,0,0,0,1,0,1,21,1,138,1,0,0,42,104,2,0,12,0, 
  0,0,3,0,0,0,80,160,9,128,73,136,8,128,66,112,7,128,83,184,10,128,44,48,131,129,5,72,204,128,6,72,204,128,39,16,2,128,53,80,4,128,54,128,5,128,60,160,70,128,84,216,11,128,15,1,215,106,0,0, 
  4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,205,107,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17, 
  1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,207,108,0,0,4,19,45,0,0,0,157,0,0,0, 
  1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21, 
  0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0, 
  0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,15,1,197,109,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0, 
  17,1,93,66,0,0,1,4,15,1,86,113,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,198,0,0,0,1,0,1,21,1,138,1,0,0,42,104,2,0,12,0,0,0,3,0,0,0,80,160,9,128,73,136,8,128, 
  66,112,7,128,83,184,10,128,44,48,131,129,5,72,204,128,6,72,204,128,39,16,2,128,53,80,4,128,54,128,5,128,60,160,70,128,84,216,11,128,15,1,215,106,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85, 
  0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,205,107,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,195,108,0,0,15,1, 
  175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,207,108,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17, 
  1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0, 
  194,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60, 
  0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,15,1,197,109,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,214,113,0,0, 
  17,1,11,79,0,0,1,8,19,61,0,0,0,198,0,0,0,1,0,1,19,47,0,0,0,160,0,0,0,1,0,1,21,1,80,0,0,0,64,98,2,0,4,0,0,0,2,0,0,0,108,16,1,128,5,120,2,128,6,120, 
  66,128,134,176,1,128,15,1,86,114,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,98,114,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,8,2,21,1,138,1,0,0,42, 
  104,2,0,12,0,0,0,3,0,0,0,80,160,9,128,73,136,8,128,66,112,7,128,83,184,10,128,44,48,131,129,5,72,204,128,6,72,204,128,39,16,2,128,53,80,4,128,54,128,5,128,60,160,70,128,84,216,11,128,15, 
  1,215,106,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,205,107,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1, 
  0,0,1,0,17,1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,207,108,0,0,4,19,45,0,0, 
  0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,195,108,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0, 
  0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0, 
  1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,194,0,0,0,2,0,1,15,1,197,109,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2, 
  1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,182,114,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,198,0,0,0,1,0,1,21,1,234,0,0,0,197,103,2,0,8,0,0,0,3,0,0,0,80,192,5, 
  128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7,128,84,216,6,128,15,1,8,106,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4, 
  17,1,25,71,0,0,1,15,1,8,106,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0, 
  0,193,0,0,0,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0, 
  60,0,0,0,1,0,19,61,0,0,0,193,0,0,0,2,0,1,4,15,1,54,115,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,197,0,0,0,1,0,1,21,0,149,0,0,0,0,0,0,0,7,0,0,0,2, 
  0,0,0,21,144,66,129,17,112,193,127,18,0,130,128,27,32,195,128,58,176,3,128,61,64,4,128,75,160,4,128,19,18,0,0,0,53,0,0,0,1,0,17,1,117,78,0,0,1,19,21,0,0,0,58,0,0,0,1,0, 
  17,1,117,78,0,0,1,19,61,0,0,0,195,0,0,0,1,0,17,1,117,78,0,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,117,78,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,117,78,0,0, 
  1,19,75,0,0,0,235,0,0,0,3,0,1,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,5,17,1,182,115,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5, 
  0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,39,0,0,0,92,80,2,0,3,0,0,0,1,0,0,0,6,48,129,128,5,48,1,128,130,240,0,128,4,17,1,1,116,0,0,1,8,2,21,1,47,2,0,0,144, 
  68,2,0,23,0,0,0,4,0,0,0,16,112,3,128,38,176,196,131,50,32,8,133,58,192,72,132,36,16,4,128,5,112,17,130,6,112,209,126,71,48,12,128,40,80,69,130,41,240,5,128,42,224,70,126,59,96,201,129,76, 
  16,14,128,69,240,10,128,46,128,7,128,63,80,10,129,70,144,11,128,72,208,12,128,75,112,205,128,79,176,14,128,106,80,15,128,123,232,15,128,130,128,16,128,15,1,64,116,0,0,15,1,1,6,0,0,4,17,1,13,6, 
  0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,175,6,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,250,6, 
  0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,17,1,64,116,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,64,116,0,0,15, 
  1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27, 
  1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,17,1,64,116,0,0,1,15,1,64,116,0,0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,212,8, 
  0,0,1,15,1,64,116,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,176,9, 
  0,0,1,15,1,64,116,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,64,116,0,0,15,1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,64,116,0,0,15,1,1,6,0,0,4,17,1,145,10, 
  0,0,1,15,1,64,116,0,0,15,1,220,10,0,0,17,1,140,35,0,0,1,15,1,64,116,0,0,15,1,88,11,0,0,17,1,216,35,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0, 
  0,0,1,0,17,1,64,116,0,0,1,8,2,21,1,62,2,0,0,144,68,2,0,23,0,0,0,4,0,0,0,16,112,3,128,38,176,196,131,50,72,8,133,58,232,72,132,36,16,4,128,5,232,17,130,6,232,209,126,71, 
  128,12,128,40,80,69,130,41,240,5,128,42,8,71,126,59,136,201,129,76,96,14,128,69,64,11,128,46,168,7,128,63,160,10,129,70,224,11,128,72,32,13,128,75,192,205,128,79,0,15,128,106,160,15,128,123,56,16,128,130, 
  208,16,128,15,1,127,116,0,0,15,1,1,6,0,0,4,17,1,13,6,0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,175,6, 
  0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,31,0,0,0,82,0,0,0,3,0,1,15,1, 
  127,116,0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,127,116,0,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1, 
  127,116,0,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,31,0,0,0,82,0,0,0,3,0,1,15,1,127,116,0,0,15, 
  1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,127,116,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,127,116,0,0,15, 
  1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,176,9,0,0,1,15,1,127,116,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,127,116,0,0,15, 
  1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,127,116,0,0,15,1,1,6,0,0,4,17,1,145,10,0,0,1,15,1,127,116,0,0,15,1,220,10,0,0,17,1,140,35,0,0,1,15,1,127,116,0,0,15,1, 
  88,11,0,0,17,1,216,35,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,31,0,0,0,82,0,0,0,3,0,1,8,2,20,4,30,0,0,0,0,0,0,0,1,0, 
  0,0,44,0,0,0,22,0,0,0,5,17,1,139,116,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0, 
  125,0,0,0,22,0,0,0,5,19,13,0,0,0,43,0,0,0,5,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1, 
  0,0,0,73,0,0,0,22,0,0,0,15,1,85,84,0,0,17,1,202,118,0,0,1,2,19,73,0,0,0,232,0,0,0,1,0,1,19,6,0,0,0,35,0,0,0,1,0,1,19,6,0,0,0,34,0,0,0,1,0, 
  1,21,1,55,0,0,0,81,82,2,0,3,0,0,0,1,0,0,0,6,176,129,128,5,176,1,128,126,240,0,128,4,19,53,0,0,0,180,0,0,0,1,0,19,67,0,0,0,217,0,0,0,2,0,1,8,2,20,4,30, 
  0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,5,17,1,23,119,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,6,0,0,0,33, 
  0,0,0,1,0,1,19,65,0,0,0,214,0,0,0,2,0,1,19,41,0,0,0,144,0,0,0,1,0,1,19,59,0,0,0,189,0,0,0,2,0,1,21,1,47,0,0,0,92,111,2,0,5,0,0,0,2,0,0,0, 
  128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,104,121,0,0,1,10,12,17,1,76,36,0,0,1,21,1,143,0,0,0,5,112,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111, 
  48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1,152,121,0,0,1,12,17,1,49,123, 
  0,0,1,12,17,1,202,124,0,0,1,12,17,1,99,126,0,0,1,12,17,1,252,127,0,0,1,12,17,1,149,129,0,0,1,12,17,1,46,131,0,0,1,6,17,1,199,132,0,0,1,12,17,1,183,133,0,0,1,10, 
  12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,51,0,0,0,255,112,2,0,6,0,0,0,2,0,0,0,128,144,65,129,5,144,129,128,6,144,1,128,113,80,65,128,121,80,1,128,132,144,1,128,12,17,1,80,135, 
  0,0,1,10,12,19,72,0,0,0,230,0,0,0,1,0,1,21,1,55,0,0,0,28,114,2,0,7,0,0,0,2,0,0,0,128,176,65,129,5,176,193,128,6,176,1,128,87,112,1,128,89,112,129,128,132,176,1,128,133, 
  112,1,128,12,17,1,132,135,0,0,1,10,12,19,96,0,0,0,48,1,0,0,1,0,1,21,1,55,0,0,0,56,115,2,0,7,0,0,0,2,0,0,0,104,112,65,129,5,176,1,128,6,176,129,128,99,112,1,128,110, 
  112,1,128,128,176,65,128,132,176,1,128,12,17,1,202,135,0,0,1,10,12,19,91,0,0,0,24,1,0,0,1,0,1,21,1,71,0,0,0,24,116,2,0,6,0,0,0,2,0,0,0,104,80,1,129,5,48,2,128,6, 
  48,66,128,110,192,1,128,128,48,66,128,132,48,2,128,15,1,36,136,0,0,4,17,1,96,90,0,0,1,15,1,101,136,0,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,22,1,0,0,2,0,1,21,1,242,2, 
  0,0,100,116,2,0,26,0,0,0,4,0,0,0,16,208,195,133,38,112,197,131,50,32,138,133,58,240,138,132,36,160,68,133,5,136,23,130,6,136,215,126,71,160,15,128,40,64,70,130,41,16,199,130,42,128,72,126,59,192, 
  203,129,76,16,18,128,69,0,14,128,46,80,9,128,63,48,13,129,70,208,14,128,72,112,16,128,75,64,17,129,79,224,18,128,89,176,19,128,106,32,20,128,123,72,21,128,128,136,23,128,130,24,22,128,132,136,23,128,15,1, 
  113,136,0,0,15,1,125,136,0,0,15,1,1,6,0,0,4,17,1,13,6,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,113,136,0,0,15,1,125,136, 
  0,0,15,1,88,6,0,0,4,17,1,175,6,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75, 
  0,0,0,1,0,19,56,0,0,0,184,0,0,0,1,0,19,7,0,0,0,36,0,0,0,2,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,113,136,0,0, 
  15,1,125,136,0,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1, 
  88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,56,0,0,0,184,0,0,0,1,0,19,7,0,0,0,36,0,0,0,2,0,1,15,1, 
  113,136,0,0,15,1,125,136,0,0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,113,136,0,0,15,1,125,136, 
  0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0, 
  4,17,1,176,9,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,1,6,0,0,4,17,1,70,10,0,0,1, 
  15,1,113,136,0,0,15,1,125,136,0,0,15,1,1,6,0,0,4,17,1,145,10,0,0,1,4,15,1,137,136,0,0,17,1,199,136,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,220,10,0,0,15,1, 
  232,10,0,0,15,1,12,11,0,0,17,1,24,11,0,0,1,15,1,113,136,0,0,15,1,125,136,0,0,15,1,88,11,0,0,4,17,1,100,11,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0, 
  75,0,0,0,1,0,19,56,0,0,0,184,0,0,0,1,0,19,7,0,0,0,36,0,0,0,2,0,1,8,2,21,1,53,0,0,0,50,117,2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160,1,128,6,160,65, 
  128,110,48,1,128,132,160,1,128,15,1,50,140,0,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,20,1,0,0,2,0,1,21,1,53,0,0,0,115,117,2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160, 
  129,128,6,160,1,128,25,48,1,128,132,160,1,128,15,1,62,140,0,0,4,17,1,74,140,0,0,1,8,2,19,91,0,0,0,16,1,0,0,2,0,1,21,1,47,0,0,0,95,77,2,0,5,0,0,0,2,0,0,0, 
  128,112,1,129,5,112,129,128,6,112,1,128,89,48,1,128,132,112,1,128,4,17,1,156,140,0,0,1,8,2,21,1,131,0,0,0,5,112,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,208,130,130,125,80,3,128, 
  123,16,3,128,108,144,66,129,5,16,68,127,6,16,4,129,103,80,130,126,128,16,4,128,132,16,4,128,134,144,3,128,135,208,3,128,12,17,1,232,140,0,0,1,12,17,1,101,142,0,0,1,12,17,1,226,143,0,0,1, 
  12,17,1,95,145,0,0,1,12,17,1,220,146,0,0,1,12,17,1,89,148,0,0,1,6,17,1,214,149,0,0,1,12,17,1,133,150,0,0,1,10,12,17,1,76,36,0,0,1,21,1,57,0,0,0,179,117,2,0,4, 
  0,0,0,2,0,0,0,89,16,193,128,5,192,193,127,6,192,1,128,133,128,1,128,15,1,2,152,0,0,4,17,1,225,91,0,0,1,4,17,1,58,152,0,0,1,8,19,96,0,0,0,38,1,0,0,2,0,1,21,1, 
  51,0,0,0,243,117,2,0,4,0,0,0,2,0,0,0,133,80,1,128,5,144,193,127,6,144,1,128,87,16,1,128,4,17,1,133,152,0,0,1,4,17,1,189,152,0,0,1,8,19,96,0,0,0,36,1,0,0,2,0, 
  1,21,1,39,0,0,0,51,118,2,0,3,0,0,0,1,0,0,0,6,48,129,128,5,48,1,128,124,240,0,128,4,17,1,7,153,0,0,1,8,2,21,4,48,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,123, 
  16,1,128,63,208,192,127,5,17,1,70,153,0,0,1,15,1,145,153,0,0,5,17,1,225,91,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,96,0,0, 
  0,39,1,0,0,2,0,1,21,1,130,0,0,0,169,118,2,0,6,0,0,0,2,0,0,0,28,80,1,128,5,8,196,128,6,8,4,128,111,184,129,128,125,40,2,128,135,24,3,128,4,19,72,0,0,0,229,0,0,0, 
  3,0,1,15,1,219,153,0,0,4,17,1,186,48,0,0,1,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,17,1,26,154,0,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  40,0,0,0,141,0,0,0,1,0,17,1,26,154,0,0,1,8,2,21,1,130,0,0,0,169,118,2,0,6,0,0,0,2,0,0,0,28,80,1,128,5,8,196,128,6,8,4,128,111,184,129,128,125,40,2,128,135,24,3, 
  128,4,19,72,0,0,0,226,0,0,0,3,0,1,15,1,89,154,0,0,4,17,1,186,48,0,0,1,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,17,1,152,154,0,0,1,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,17,1,152,154,0,0,1,8,2,19,92,0,0,0,25,1,0,0,2,0,1,21,0,83,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0, 
  12,16,65,128,24,160,65,128,60,0,66,128,92,144,2,128,19,24,0,0,0,64,0,0,0,1,0,17,1,114,93,0,0,1,19,92,0,0,0,25,1,0,0,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,114, 
  93,0,0,1,1,2,21,1,143,0,0,0,5,112,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128, 
  128,112,4,128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1,215,154,0,0,1,12,17,1,112,156,0,0,1,12,17,1,9,158,0,0,1,12,17,1,162,159,0,0,1,12,17,1,59,161,0,0,1,12,17,1,212, 
  162,0,0,1,12,17,1,109,164,0,0,1,6,17,1,6,166,0,0,1,12,17,1,234,166,0,0,1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,89,0,0,0,63,97,2,0,7,0,0,0,2,0,0,0, 
  104,224,65,129,5,192,2,128,6,192,130,128,99,112,1,128,110,80,2,128,128,192,66,128,132,192,2,128,15,1,131,168,0,0,4,17,1,44,87,0,0,1,15,1,214,168,0,0,4,17,1,96,90,0,0,1,15,1,23,169, 
  0,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,23,1,0,0,2,0,1,21,0,173,0,0,0,0,0,0,0,9,0,0,0,3,0,0,0,24,16,67,129,23,168,66,129,60,160,3,128,91,152,4,128,12,176,129, 
  127,72,48,196,128,95,160,4,128,15,64,130,126,96,216,4,128,19,24,0,0,0,64,0,0,0,1,0,17,1,199,94,0,0,1,15,1,199,94,0,0,17,1,198,93,0,0,1,15,1,199,94,0,0,17,1,17,86,0,0, 
  1,19,23,0,0,0,62,0,0,0,1,0,17,1,199,94,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,199,94,0,0,1,15,1,199,94,0,0,17,1,81,86,0,0,1,1,17,1,35,169,0,0,1,19,15, 
  0,0,0,49,0,0,0,1,0,17,1,199,94,0,0,1,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,34,0,0,0,118,0,0,0,3,0,1,20,2,27,0,0,0,0,0, 
  0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,229,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,24,216,67,130,23,112,67,130,10,16,194,128,91,160,5,128,12,120,2,129,34,104, 
  4,128,46,112,4,128,15,8,131,126,60,168,4,128,72,56,133,128,95,48,6,128,96,152,6,128,15,1,180,95,0,0,17,1,103,169,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,180,95,0,0,1,15,1,180, 
  95,0,0,17,1,163,169,0,0,1,15,1,180,95,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,180,95,0,0,1,1,17,1,117,95,0,0,1,19,24,0,0,0,65,0,0,0,1,0, 
  17,1,180,95,0,0,1,15,1,180,95,0,0,17,1,81,86,0,0,1,19,10,0,0,0,40,0,0,0,1,0,17,1,180,95,0,0,1,15,1,180,95,0,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0, 
  1,0,17,1,180,95,0,0,1,2,21,0,162,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,136,194,128,60,24,3,129,82,120,4,128,72,168,195,128,12,144,65,127,76,16,4,128,96,128,4,128,23,32,2,128, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,154,96,0,0,1,15,1,154,96,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,154,96,0,0,1,19,24,0,0,0,65,0,0,0,1,0, 
  17,1,154,96,0,0,1,15,1,154,96,0,0,17,1,81,86,0,0,1,15,1,154,96,0,0,17,1,63,170,0,0,1,1,19,76,0,0,0,237,0,0,0,1,0,17,1,154,96,0,0,1,2,21,1,178,1,0,0,50, 
  120,2,0,11,0,0,0,3,0,0,0,88,240,193,128,111,112,70,130,125,0,8,128,128,136,13,128,108,224,4,129,5,136,77,127,6,136,205,128,103,128,131,126,132,136,13,128,134,96,10,128,135,40,11,128,15,1,193,171,0, 
  0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,193,171,0,0,15,1,37,99,0,0,15,1,195, 
  99,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,193,171,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,193,171,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0, 
  0,4,17,1,186,48,0,0,1,15,1,193,171,0,0,15,1,37,99,0,0,15,1,195,99,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0, 
  0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,246,171,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,193,171,0,0,15,1,37,99, 
  0,0,15,1,195,99,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17, 
  1,132,42,0,0,1,8,2,21,1,52,0,0,0,208,120,2,0,5,0,0,0,2,0,0,0,128,152,1,129,5,152,1,128,6,152,65,128,30,48,1,128,132,152,1,128,4,19,82,0,0,0,253,0,0,0,3,0,1,8, 
  2,21,0,157,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240,67,128,96,88,4,128,19,24,0,0,0,64,0,0,0,1,0,17,1, 
  37,99,0,0,1,15,1,37,99,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,37,99,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,37,99,0,0,1,15,1,37,99,0,0, 
  17,1,81,86,0,0,1,15,1,37,99,0,0,17,1,153,172,0,0,1,19,76,0,0,0,237,0,0,0,1,0,17,1,37,99,0,0,1,2,19,76,0,0,0,237,0,0,0,1,0,1,21,1,47,0,0,0,16,121,2, 
  0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,24,173,0,0,1,10,12,19,46,0,0,0,158,0,0,0,1,0,1,21,1,143,0,0,0,91,121,2, 
  0,13,0,0,0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4, 
  128,12,17,1,72,173,0,0,1,12,17,1,225,174,0,0,1,12,17,1,122,176,0,0,1,12,17,1,19,178,0,0,1,12,17,1,172,179,0,0,1,12,17,1,69,181,0,0,1,12,17,1,222,182,0,0,1,12,17,1, 
  119,184,0,0,1,12,17,1,16,186,0,0,1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,59,0,0,0,46,86,2,0,6,0,0,0,2,0,0,0,128,208,65,129,5,208,129,128,6,208,1,128,77,80,65, 
  128,85,144,1,128,132,208,1,128,4,17,1,44,39,0,0,1,4,17,1,108,39,0,0,1,8,2,21,1,63,0,0,0,107,123,2,0,9,0,0,0,3,0,0,0,88,176,193,128,111,176,193,129,125,176,1,128,128,240,1, 
  128,132,240,1,128,5,240,65,127,6,240,1,128,103,176,129,126,135,176,1,128,12,17,1,169,187,0,0,1,10,12,17,1,76,36,0,0,1,21,1,63,0,0,0,159,77,2,0,5,0,0,0,2,0,0,0,128,240,193,128, 
  5,240,1,128,6,240,129,128,132,240,1,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,14,0,0,0,45,0,0,0,5,0,1,8,2,21,1,63,0,0,0,159,77,2,0,5,0,0,0,2,0,0,0, 
  128,240,193,128,5,240,1,128,6,240,129,128,132,240,1,128,134,48,1,128,4,19,19,0,0,0,56,0,0,0,1,0,19,14,0,0,0,46,0,0,0,5,0,1,8,2,21,1,69,0,0,0,2,78,2,0,6,0,0,0, 
  2,0,0,0,128,32,66,129,5,32,2,128,2,80,129,128,3,184,1,128,6,32,2,128,132,32,2,128,4,19,52,0,0,0,177,0,0,0,2,0,1,4,19,52,0,0,0,176,0,0,0,2,0,1,8,2,19,64,0,0, 
  0,206,0,0,0,2,0,1,19,64,0,0,0,205,0,0,0,2,0,1,19,64,0,0,0,204,0,0,0,2,0,1,19,64,0,0,0,207,0,0,0,2,0,1,19,64,0,0,0,208,0,0,0,2,0,1,19,11,0,0, 
  0,41,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,24,224,3,130,23,120,67,130,10,16,2,128,11,120,130,129,12,128,66,128,60,208,4,128,46,112,4,128,15,16,131,126,72,96, 
  197,128,91,200,5,128,95,88,6,128,96,192,6,128,15,1,56,102,0,0,17,1,115,188,0,0,1,1,19,24,0,0,0,64,0,0,0,1,0,17,1,56,102,0,0,1,15,1,56,102,0,0,17,1,175,188,0,0,1,15, 
  1,56,102,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,56,102,0,0,1,19,11,0,0,0,41,0,0,0,4,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,56,102,0,0,1, 
  15,1,56,102,0,0,17,1,81,86,0,0,1,19,10,0,0,0,40,0,0,0,1,0,17,1,56,102,0,0,1,15,1,56,102,0,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,56,102,0, 
  0,1,2,19,77,0,0,0,238,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,24,216,67,130,23,112,67,130,10,16,2,128,91,200,5,128,12,120,2,129,77,192,5,128,46,104,4, 
  128,15,8,131,126,60,200,4,128,72,88,133,128,95,88,6,128,96,192,6,128,15,1,47,103,0,0,17,1,75,189,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,47,103,0,0,1,15,1,47,103,0,0,17,1, 
  135,189,0,0,1,15,1,47,103,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,47,103,0,0,1,19,77,0,0,0,238,0,0,0,4,0,1,19,24,0,0,0,65,0,0,0,1,0,17, 
  1,47,103,0,0,1,15,1,47,103,0,0,17,1,81,86,0,0,1,1,19,10,0,0,0,40,0,0,0,1,0,17,1,47,103,0,0,1,15,1,47,103,0,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0, 
  1,0,17,1,47,103,0,0,1,2,19,99,0,0,0,52,1,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,24,216,3,130,23,112,3,130,10,16,2,128,91,192,5,130,12,120,66,128, 
  60,200,4,128,46,104,4,128,15,8,131,126,72,88,133,128,95,80,6,128,96,184,6,128,99,72,7,128,15,1,38,104,0,0,17,1,35,190,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17,1,38,104,0,0,1,15, 
  1,38,104,0,0,17,1,95,190,0,0,1,15,1,38,104,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,38,104,0,0,1,19,99,0,0,0,52,1,0,0,4,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,38,104,0,0,1,15,1,38,104,0,0,17,1,81,86,0,0,1,19,10,0,0,0,40,0,0,0,1,0,17,1,38,104,0,0,1,15,1,38,104,0,0,17,1,149,86,0,0,1,19,15,0, 
  0,0,49,0,0,0,1,0,17,1,38,104,0,0,1,1,2,19,22,0,0,0,61,0,0,0,4,0,1,21,0,234,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,24,224,67,130,23,120,67,130,10,16,2,128,91, 
  200,5,128,12,120,2,129,46,112,4,128,22,112,195,127,15,8,131,126,60,208,4,128,72,96,133,128,95,88,6,128,96,192,6,128,15,1,29,105,0,0,17,1,251,190,0,0,1,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,29,105,0,0,1,15,1,29,105,0,0,17,1,55,191,0,0,1,1,15,1,29,105,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,29,105,0,0,1,19,22,0,0,0,61,0,0,0, 
  4,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,29,105,0,0,1,15,1,29,105,0,0,17,1,81,86,0,0,1,19,10,0,0,0,40,0,0,0,1,0,17,1,29,105,0,0,1,15,1,29,105,0,0,17,1, 
  149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,29,105,0,0,1,2,19,61,0,0,0,193,0,0,0,2,0,1,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80, 
  193,127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,20,106,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,20,106,0,0,1,19,61,0,0,0,193, 
  0,0,0,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,20,106,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,20,106,0,0,1,1,2,21,1,59,0,0,0,214,125,2,0,8,0,0,0,3,0, 
  0,0,128,208,1,128,54,144,1,128,132,208,1,128,83,144,1,128,44,144,129,127,5,208,1,128,6,208,193,126,39,144,1,128,12,17,1,211,191,0,0,1,10,12,17,1,76,36,0,0,1,21,1,234,0,0,0,197,103,2, 
  0,8,0,0,0,3,0,0,0,80,192,5,128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7,128,84,216,6,128,15,1,95,192,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15, 
  1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,95,192,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0, 
  0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0, 
  0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,15,1,107,192,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,196,0,0,0,2,0,1,21,1,234, 
  0,0,0,197,103,2,0,8,0,0,0,3,0,0,0,80,192,5,128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7,128,84,216,6,128,15,1,95,192,0,0,15,1,175,70,0,0,15, 
  1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,95,192,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0, 
  0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0, 
  1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,15,1,235,192,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,196,0,0,0, 
  2,0,1,19,61,0,0,0,194,0,0,0,2,0,1,21,1,234,0,0,0,197,103,2,0,8,0,0,0,3,0,0,0,80,192,5,128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5,72,135,127,6,72,7, 
  128,84,216,6,128,15,1,95,192,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,95,192,0,0,15,1,175,70,0,0,15,1,140,75,0, 
  0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0, 
  0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,15,1,107,193,0, 
  0,17,1,11,79,0,0,1,8,19,61,0,0,0,196,0,0,0,2,0,1,21,1,234,0,0,0,197,103,2,0,8,0,0,0,3,0,0,0,80,192,5,128,73,168,4,128,66,144,3,128,53,144,1,128,60,192,194,128,5, 
  72,135,127,6,72,7,128,84,216,6,128,15,1,95,192,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,95,192,0,0,15,1,175,70,0, 
  0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0,0,139,0,0,0, 
  1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,192,0,0,0,3,0,1, 
  4,15,1,235,193,0,0,17,1,11,79,0,0,1,8,19,61,0,0,0,196,0,0,0,2,0,1,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2, 
  128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,187,110,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,187,110,0,0,1,19,61,0,0,0,194,0,0,0,2,0,1,19,18,0, 
  0,0,55,0,0,0,1,0,17,1,187,110,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,187,110,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193, 
  127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,59,111,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,59,111,0,0,1,19,61,0,0,0,193,0, 
  0,0,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,59,111,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,59,111,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0, 
  0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,187,111,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,187,111,0,0, 
  1,19,61,0,0,0,194,0,0,0,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,187,111,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,187,111,0,0,1,1,2,19,47,0,0,0,159,0,0,0, 
  2,0,1,21,1,39,0,0,0,231,126,2,0,3,0,0,0,1,0,0,0,6,48,1,128,5,48,65,128,53,240,0,128,12,17,1,107,194,0,0,1,10,12,17,1,76,36,0,0,1,19,81,0,0,0,251,0,0,0,2, 
  0,1,21,0,83,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,12,16,129,128,81,144,2,128,24,160,65,128,60,0,2,128,19,24,0,0,0,64,0,0,0,1,0,17,1,130,112,0,0,1,19,81,0,0,0,251, 
  0,0,0,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,130,112,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128, 
  58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,214,112,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,214,112,0,0,1,19,61,0,0,0,193,0,0,0,2,0,1,19,18,0,0, 
  0,55,0,0,0,1,0,17,1,214,112,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,214,112,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127, 
  18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,86,113,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,86,113,0,0,1,19,61,0,0,0,194,0,0, 
  0,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,86,113,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,86,113,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0, 
  21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,214,113,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,214,113,0,0,1, 
  19,61,0,0,0,194,0,0,0,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,214,113,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,214,113,0,0,1,1,2,19,32,0,0,0,84,0,0,0,2, 
  0,1,21,0,83,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,12,16,65,128,24,160,65,128,32,0,66,128,60,8,2,128,19,24,0,0,0,64,0,0,0,1,0,17,1,98,114,0,0,1,19,32,0,0,0,84, 
  0,0,0,2,0,1,1,19,24,0,0,0,65,0,0,0,1,0,17,1,98,114,0,0,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128, 
  58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,182,114,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,182,114,0,0,1,19,61,0,0,0,194,0,0,0,2,0,1,19,18,0,0, 
  0,55,0,0,0,1,0,17,1,182,114,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,182,114,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127, 
  18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,54,115,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,54,115,0,0,1,19,61,0,0,0,193,0,0, 
  0,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,54,115,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,54,115,0,0,1,1,2,21,1,74,0,0,0,40,127,2,0,5,0,0,0,2,0,0,0, 
  74,112,1,129,5,72,2,128,6,72,130,127,51,48,1,128,134,176,1,128,4,17,1,153,194,0,0,1,4,17,1,241,194,0,0,1,4,19,19,0,0,0,56,0,0,0,1,0,17,1,73,195,0,0,1,8,2,20,4,35, 
  0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,5,19,50,0,0,0,169,0,0,0,5,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2, 
  20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,57,0,0,0,187,0,0,0,6,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0, 
  0,9,2,19,31,0,0,0,82,0,0,0,3,0,1,21,1,62,2,0,0,144,68,2,0,23,0,0,0,4,0,0,0,16,112,3,128,38,176,196,131,50,72,8,133,58,232,72,132,36,16,4,128,5,232,17,130,6,232,209, 
  126,71,128,12,128,40,80,69,130,41,240,5,128,42,8,71,126,59,136,201,129,76,96,14,128,69,64,11,128,46,168,7,128,63,160,10,129,70,224,11,128,72,32,13,128,75,192,205,128,79,0,15,128,106,160,15,128,123,56,16, 
  128,130,208,16,128,15,1,161,195,0,0,15,1,1,6,0,0,4,17,1,13,6,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1, 
  175,6,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,88,0,0,0,5,1,0,0,3,0,1, 
  15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,161,195,0,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1, 
  15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,88,0,0,0,5,1,0,0,3,0,1,15,1,161,195,0, 
  0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,161,195,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,161,195,0, 
  0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,176,9,0,0,1,15,1,161,195,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,161,195,0, 
  0,15,1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,161,195,0,0,15,1,1,6,0,0,4,17,1,145,10,0,0,1,15,1,161,195,0,0,15,1,220,10,0,0,17,1,140,35,0,0,1,15,1,161,195,0,0, 
  15,1,88,11,0,0,17,1,216,35,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,88,0,0,0,5,1,0,0,3,0,1,8,2,21,4,42,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,44,16,1,128,125,208,0,128,12,17,1,76,36,0,0,1,12,17,1,173,195,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12, 
  17,1,76,36,0,0,1,21,1,80,2,0,0,41,128,2,0,24,0,0,0,4,0,0,0,16,144,3,128,38,208,196,131,50,104,72,133,58,8,137,132,36,48,4,128,5,120,18,130,6,120,210,126,71,160,12,128,40,112,69, 
  130,41,16,198,130,42,40,71,126,59,168,201,129,76,128,14,128,69,96,11,128,46,200,7,128,63,192,10,129,70,0,12,128,72,64,13,128,75,224,13,129,79,32,15,128,89,192,15,128,106,48,16,128,123,200,16,128,130,96,17, 
  128,15,1,231,195,0,0,15,1,1,6,0,0,4,17,1,13,6,0,0,1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,175,6,0,0, 
  1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,6,0,0,0,31,0,0,0,3,0,1,15,1,231,195, 
  0,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,231,195,0,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1,231,195, 
  0,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,6,0,0,0,31,0,0,0,3,0,1,15,1,231,195,0,0,15,1,125, 
  8,0,0,4,17,1,137,8,0,0,1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,231,195,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,231,195,0,0,15,1,88, 
  6,0,0,4,17,1,101,9,0,0,1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,176,9,0,0,1,15,1,231,195,0,0,15,1,88,6,0,0,4,17,1,251,9,0,0,1,15,1,231,195,0,0,15,1,1, 
  6,0,0,4,17,1,70,10,0,0,1,15,1,231,195,0,0,15,1,1,6,0,0,4,17,1,145,10,0,0,1,15,1,243,195,0,0,4,17,1,189,14,0,0,1,15,1,231,195,0,0,15,1,220,10,0,0,17,1,140, 
  35,0,0,1,15,1,231,195,0,0,15,1,88,11,0,0,17,1,216,35,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,6,0,0,0,31,0,0,0,3,0,1,8,2, 
  21,1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,4,17,1,255,195,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0, 
  0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20, 
  26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17, 
  1,132,42,0,0,1,15,1,84,198,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0, 
  141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111, 
  80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20, 
  0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,228,198,0, 
  0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0, 
  115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128, 
  108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95, 
  0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,116,199,0,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0, 
  63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76, 
  129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0, 
  0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,4,200,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0, 
  0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184, 
  12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186, 
  48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0, 
  0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,148,200,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4, 
  19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43, 
  134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135, 
  184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0, 
  0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4, 
  17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0, 
  0,0,1,0,17,1,132,42,0,0,1,15,1,36,201,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0, 
  19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0, 
  88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0, 
  0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76, 
  26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1, 
  15,1,180,201,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0, 
  19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,232,0,0,0,223,134,2,0,20,0,0,0,4,0,0,0,128,56,7,128,113,120,6,128,102,80,4, 
  128,99,16,4,128,132,56,7,128,5,56,7,128,6,56,7,127,87,80,67,130,88,144,67,130,89,208,131,130,106,16,5,128,123,248,6,128,108,80,5,128,29,16,67,129,110,248,5,128,111,56,6,128,103,144,4,128,104,208,4, 
  128,109,144,5,128,121,184,6,128,12,17,1,68,202,0,0,1,12,17,1,221,203,0,0,1,12,17,1,152,121,0,0,1,12,17,1,118,205,0,0,1,12,17,1,15,207,0,0,1,12,17,1,168,208,0,0,1,12,17,1, 
  49,123,0,0,1,12,17,1,65,210,0,0,1,12,17,1,202,124,0,0,1,12,17,1,99,126,0,0,1,12,19,95,0,0,0,32,1,0,0,1,0,1,12,17,1,218,211,0,0,1,12,17,1,252,127,0,0,1,12,17, 
  1,115,213,0,0,1,12,17,1,12,215,0,0,1,12,17,1,149,129,0,0,1,10,12,17,1,165,216,0,0,1,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4, 
  128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0, 
  0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70, 
  0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,62,218,0,0,4,19,19,0,0, 
  0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,51,0,0,0,11,98,2,0,4,0,0,0,2,0,0,0,113,16,193,128,5,144,193,127,6,144,1,128,121,80,1,128,4,17,1,96,92,0, 
  0,1,4,17,1,227,92,0,0,1,8,2,21,1,69,0,0,0,192,97,2,0,5,0,0,0,2,0,0,0,89,112,1,129,5,32,194,127,6,32,2,128,87,48,1,128,133,224,1,128,4,17,1,206,218,0,0,1,15,1, 
  27,219,0,0,4,17,1,225,91,0,0,1,4,17,1,98,219,0,0,1,8,2,21,1,89,0,0,0,63,97,2,0,7,0,0,0,2,0,0,0,104,224,65,129,5,192,2,128,6,192,130,128,99,112,1,128,110,80,2,128, 
  128,192,66,128,132,192,2,128,15,1,217,86,0,0,4,17,1,44,87,0,0,1,15,1,31,90,0,0,4,17,1,96,90,0,0,1,15,1,150,90,0,0,4,17,1,162,90,0,0,1,8,2,21,1,53,0,0,0,35,136, 
  2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160,1,128,6,160,65,128,110,48,1,128,132,160,1,128,15,1,175,219,0,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,18,1,0,0,3,0,1,19,91,0, 
  0,0,14,1,0,0,3,0,1,19,7,0,0,0,36,0,0,0,2,0,1,19,56,0,0,0,184,0,0,0,1,0,1,21,0,61,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,56,136,1,128,7,240,64,128,13, 
  248,0,128,1,19,56,0,0,0,183,0,0,0,1,0,17,1,137,136,0,0,1,19,7,0,0,0,36,0,0,0,2,0,1,2,21,1,106,3,0,0,111,136,2,0,25,0,0,0,4,0,0,0,16,176,67,128,32,176,4, 
  128,50,0,140,133,38,24,198,131,36,24,5,128,5,72,27,131,6,72,91,127,71,120,18,128,40,24,199,130,41,24,8,128,42,144,137,129,59,0,78,130,76,120,21,128,45,144,10,128,46,0,11,128,63,120,143,129,58,0,141, 
  129,69,120,16,128,70,120,17,128,72,120,19,128,75,120,212,128,79,120,22,128,106,120,23,128,123,208,24,128,130,208,25,128,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,1,6,0,0,4,17,1, 
  13,6,0,0,1,4,19,56,0,0,0,186,0,0,0,2,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1,15,1,187,219,0,0,15,1,250,219, 
  0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,175,6,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,15,1,187,219,0,0, 
  4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,36,0,0,0,126,0,0,0,1,0,17,1,250,219,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15, 
  1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,167,24,0,0,4,17,1,242,24,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15, 
  1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,50,8, 
  0,0,1,15,1,187,219,0,0,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,36,0,0,0,126,0,0,0,1,0,17,1,250,219,0,0,1,15,1,187,219,0,0,15,1,250,219, 
  0,0,15,1,30,220,0,0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,187,219,0,0, 
  15,1,250,219,0,0,15,1,30,220,0,0,15,1,31,9,0,0,4,17,1,43,9,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1, 
  187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,176,9,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,88,6,0,0,4,17,1,251,9,0, 
  0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,1,6,0,0,4,17, 
  1,145,10,0,0,1,15,1,187,219,0,0,15,1,250,219,0,0,15,1,30,220,0,0,15,1,220,10,0,0,15,1,232,10,0,0,15,1,12,11,0,0,17,1,24,11,0,0,1,15,1,187,219,0,0,15,1,250,219,0, 
  0,15,1,30,220,0,0,15,1,88,11,0,0,4,17,1,100,11,0,0,1,15,1,187,219,0,0,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,36,0,0,0,126,0,0,0,1, 
  0,17,1,250,219,0,0,1,8,2,19,91,0,0,0,12,1,0,0,3,0,1,19,107,0,0,0,67,1,0,0,2,0,1,20,4,54,0,0,0,0,0,0,0,1,0,0,0,36,0,0,0,22,0,0,0,15,1,42,220, 
  0,0,15,1,105,220,0,0,15,1,141,220,0,0,15,1,153,220,0,0,5,17,1,250,23,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,75,0, 
  0,0,159,77,2,0,5,0,0,0,2,0,0,0,128,80,194,128,5,80,2,128,6,80,130,128,132,80,2,128,134,48,1,128,15,1,211,220,0,0,4,19,19,0,0,0,56,0,0,0,1,0,19,12,0,0,0,42,0,0, 
  0,1,0,17,1,207,19,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64, 
  131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17, 
  1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0, 
  0,1,15,1,8,221,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0, 
  1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125, 
  16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195, 
  42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,148,221,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0, 
  162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,32,222,0,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0, 
  63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11, 
  129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26, 
  0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,15,1,172,222,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112, 
  133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0, 
  4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0, 
  0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,56,223,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124, 
  1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135, 
  216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0, 
  0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,196,223,0,0,4,19,19,0,0,0,56, 
  0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23, 
  0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,167,0,0,0,80,137,2,0,15,0,0,0,3,0,0,0,88,240,66,131,89,48,131,130,29,112,194,129,123,240,4,128,108,176,131,130,5,48,69,127, 
  6,48,5,128,87,176,66,128,103,112,131,128,109,240,3,128,111,48,4,128,113,112,68,128,121,176,4,128,128,48,5,128,132,48,5,128,12,17,1,80,224,0,0,1,12,17,1,205,225,0,0,1,12,17,1,232,140,0,0,1, 
  12,17,1,74,227,0,0,1,12,17,1,101,142,0,0,1,12,17,1,226,143,0,0,1,12,17,1,76,36,0,0,1,12,17,1,95,145,0,0,1,12,17,1,199,228,0,0,1,12,17,1,68,230,0,0,1,12,17,1,220, 
  146,0,0,1,10,12,17,1,193,231,0,0,1,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103, 
  64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0, 
  17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,15,1,62,233,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,44,0,0,0,52,138,2,0,3,0,0,0,1,0,0,0,6,88,1,128,5,88,65,128, 
  133,240,0,128,4,19,96,0,0,0,44,1,0,0,4,0,1,8,19,96,0,0,0,34,1,0,0,3,0,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,123,0,0,0,22,0,0,0,15,1,202,233,0,0,5,17, 
  1,225,91,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,96,0,0,0,43,1,0,0,3,0,1,21,1,44,0,0,0,52,138,2,0,3,0,0,0,1, 
  0,0,0,6,88,1,128,5,88,65,128,133,240,0,128,4,19,96,0,0,0,47,1,0,0,4,0,1,8,19,96,0,0,0,41,1,0,0,3,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,63,0,0,0,22, 
  0,0,0,5,19,96,0,0,0,46,1,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,96,0,0,0,45,1,0,0,3,0,1,20,4,35,0,0, 
  0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,86,0,0,0,3,1,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4, 
  36,0,0,0,0,0,0,0,1,0,0,0,123,0,0,0,22,0,0,0,15,1,214,233,0,0,5,17,1,225,91,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0, 
  9,19,96,0,0,0,37,1,0,0,3,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,63,0,0,0,22,0,0,0,5,19,96,0,0,0,40,1,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0, 
  0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,19,96,0,0,0,35,1,0,0,3,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,72,0,0,0,228,0,0,0,4, 
  0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,72,0,0,0,227,0, 
  0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,72,0,0, 
  0,225,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19, 
  72,0,0,0,224,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66, 
  130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4, 
  19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,226, 
  233,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0, 
  0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128, 
  7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0, 
  19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0, 
  221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,114,234,0,0,4,19,19,0,0,0,56,0, 
  0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0, 
  0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6, 
  184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1, 
  219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195, 
  42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0, 
  142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,2,235,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132, 
  42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128, 
  128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17, 
  1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0, 
  115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,146,235,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0, 
  0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9, 
  128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162, 
  48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0, 
  0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0, 
  63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,34,236,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0, 
  1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0, 
  0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213, 
  45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17, 
  1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0, 
  0,1,15,1,178,236,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0, 
  1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106, 
  96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57, 
  0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4, 
  19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,66,237,0,0,4,19,19, 
  0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0, 
  1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,220,0,0,0,148,138,2,0,19,0,0,0,4,0,0,0,128,216,6,128,113,24,6,128,103,48,4,128,99,240,3,128,132,216,6,128, 
  5,216,6,128,6,216,6,128,87,48,195,126,88,112,3,130,89,176,67,130,106,176,4,128,123,152,6,128,108,240,4,128,29,240,2,129,110,152,5,128,111,216,5,128,104,112,4,128,109,48,5,128,121,88,6,128,12,17,1,210, 
  237,0,0,1,12,17,1,107,239,0,0,1,12,17,1,215,154,0,0,1,12,17,1,4,241,0,0,1,12,17,1,157,242,0,0,1,12,17,1,112,156,0,0,1,12,17,1,54,244,0,0,1,12,17,1,9,158,0,0,1, 
  12,17,1,162,159,0,0,1,12,19,95,0,0,0,32,1,0,0,1,0,1,12,17,1,207,245,0,0,1,12,17,1,59,161,0,0,1,12,17,1,104,247,0,0,1,12,17,1,1,249,0,0,1,12,17,1,212,162,0,0, 
  1,10,12,17,1,154,250,0,0,1,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126, 
  125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48, 
  0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,51,252,0,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21, 
  1,71,0,0,0,131,139,2,0,6,0,0,0,2,0,0,0,104,80,1,129,5,48,2,128,6,48,66,128,110,192,1,128,128,48,66,128,132,48,2,128,15,1,195,252,0,0,4,17,1,96,90,0,0,1,15,1,4,253,0, 
  0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,21,1,0,0,3,0,1,21,1,53,0,0,0,50,117,2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160,1,128,6,160,65,128,110,48,1,128,132,160,1,128, 
  15,1,16,253,0,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,19,1,0,0,3,0,1,19,91,0,0,0,15,1,0,0,3,0,1,21,1,55,0,0,0,56,115,2,0,7,0,0,0,2,0,0,0,104,112,65, 
  129,5,176,1,128,6,176,129,128,99,112,1,128,110,112,1,128,128,176,65,128,132,176,1,128,12,17,1,28,253,0,0,1,10,12,19,91,0,0,0,23,1,0,0,2,0,1,21,1,47,0,0,0,16,121,2,0,5,0,0, 
  0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,118,253,0,0,1,10,12,19,46,0,0,0,158,0,0,0,1,0,1,21,1,143,0,0,0,91,121,2,0,13,0,0, 
  0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1, 
  166,253,0,0,1,12,17,1,63,255,0,0,1,12,17,1,216,0,1,0,1,12,17,1,113,2,1,0,1,12,17,1,10,4,1,0,1,12,17,1,163,5,1,0,1,12,17,1,60,7,1,0,1,12,17,1,213,8,1,0, 
  1,12,17,1,110,10,1,0,1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,129,1,0,0,189,140,2,0,12,0,0,0,3,0,0,0,88,168,2,130,30,16,66,130,111,8,70,130,125,56,7,128,108,216,68, 
  129,5,0,140,127,6,0,204,126,103,216,195,126,128,0,12,128,132,0,12,128,134,56,9,128,135,0,10,128,4,19,82,0,0,0,253,0,0,0,3,0,17,1,7,12,1,0,1,15,1,151,12,1,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1, 
  151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,7,12,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,52,0,0,0,208,120,2,0,5, 
  0,0,0,2,0,0,0,128,152,1,129,5,152,1,128,6,152,65,128,30,48,1,128,132,152,1,128,4,19,82,0,0,0,252,0,0,0,4,0,1,8,2,21,0,162,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  24,136,194,128,60,24,3,129,82,120,4,128,72,168,195,128,12,144,65,127,76,16,4,128,96,128,4,128,23,32,2,128,19,24,0,0,0,64,0,0,0,1,0,17,1,246,171,0,0,1,15,1,246,171,0,0,17,1,17,86, 
  0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,246,171,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,246,171,0,0,1,15,1,246,171,0,0,17,1,81,86,0,0,1,15,1,246,171,0,0,17,1, 
  163,12,1,0,1,1,19,76,0,0,0,237,0,0,0,1,0,17,1,246,171,0,0,1,2,21,1,119,0,0,0,189,140,2,0,11,0,0,0,3,0,0,0,88,240,193,128,111,176,66,130,125,240,2,128,128,176,3,128,108, 
  112,2,129,5,176,67,127,6,176,195,128,103,48,130,126,132,176,3,128,134,48,3,128,135,112,3,128,12,17,1,37,14,1,0,1,12,17,1,144,15,1,0,1,12,17,1,251,16,1,0,1,12,17,1,102,18,1,0,1,12, 
  17,1,209,19,1,0,1,12,17,1,60,21,1,0,1,12,17,1,167,22,1,0,1,10,12,17,1,76,36,0,0,1,21,1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112, 
  65,128,102,48,1,128,132,112,1,128,4,17,1,18,24,1,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184, 
  204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0, 
  4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0, 
  0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40, 
  0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,79,26,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0, 
  17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125, 
  240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0, 
  0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34, 
  0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,223,26,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1, 
  152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128, 
  134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1, 
  72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23, 
  0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,111,27,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0, 
  1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0, 
  0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4, 
  17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26, 
  0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,15,1,255,27,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80, 
  198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0, 
  0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,143,28,1,0, 
  4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108, 
  32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15, 
  1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0, 
  0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0, 
  0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,31,29,1,0,4,19,19,0,0,0,56,0,0,0,1, 
  0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129, 
  103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,175,29,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12, 
  128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48, 
  0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,63,30,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134, 
  2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184, 
  10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0, 
  4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17, 
  1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0, 
  0,1,0,17,1,132,42,0,0,1,15,1,207,30,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,201,0,0,0,18,88,2,0,9,0,0,0,3,0,0,0,88, 
  176,193,128,111,192,194,129,125,96,3,128,128,64,6,128,132,64,6,128,5,64,70,127,6,64,6,128,103,80,130,126,135,208,4,128,15,1,95,31,1,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,95,31,1, 
  0,4,17,1,213,45,0,0,1,15,1,95,31,1,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,25,0,0,0,67,0,0,0,2,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,25,0,0,0,67,0,0,0,2,0, 
  1,8,2,21,1,47,0,0,0,194,141,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,107,31,1,0,1,10,12,19,46,0,0,0,158,0,0,0, 
  1,0,1,21,1,143,0,0,0,96,142,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4, 
  128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1,155,31,1,0,1,12,17,1,52,33,1,0,1,12,17,1,205,34,1,0,1,12,17,1,102,36,1,0,1,12,17,1,255,37,1,0,1,12,17,1,152,39,1,0, 
  1,12,17,1,49,41,1,0,1,6,17,1,202,42,1,0,1,12,17,1,186,43,1,0,1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,47,0,0,0,194,141,2,0,5,0,0,0,2,0,0,0,128,112,1, 
  129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,83,45,1,0,1,10,12,19,46,0,0,0,158,0,0,0,1,0,1,21,1,143,0,0,0,96,142,2,0,13,0,0,0,3,0,0,0,88,48,66, 
  130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1,131,45,1,0,1,12,17,1, 
  28,47,1,0,1,12,17,1,181,48,1,0,1,12,17,1,78,50,1,0,1,12,17,1,231,51,1,0,1,12,17,1,128,53,1,0,1,12,17,1,25,55,1,0,1,6,17,1,178,56,1,0,1,12,17,1,162,57,1,0, 
  1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,47,0,0,0,194,141,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,59,59,1,0, 
  1,10,12,19,46,0,0,0,158,0,0,0,1,0,1,21,1,143,0,0,0,96,142,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68, 
  129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1,107,59,1,0,1,12,17,1,4,61,1,0,1,12,17,1,157,62,1,0,1,12,17,1,54,64,1,0,1,12,17,1, 
  207,65,1,0,1,12,17,1,104,67,1,0,1,12,17,1,1,69,1,0,1,6,17,1,154,70,1,0,1,12,17,1,138,71,1,0,1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,47,0,0,0,194,141,2, 
  0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,12,17,1,35,73,1,0,1,10,12,19,46,0,0,0,158,0,0,0,1,0,1,21,1,143,0,0,0,96,142,2, 
  0,13,0,0,0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128,108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4, 
  128,12,17,1,83,73,1,0,1,12,17,1,236,74,1,0,1,12,17,1,133,76,1,0,1,12,17,1,30,78,1,0,1,12,17,1,183,79,1,0,1,12,17,1,80,81,1,0,1,12,17,1,233,82,1,0,1,6,17,1, 
  130,84,1,0,1,12,17,1,114,85,1,0,1,10,12,19,95,0,0,0,32,1,0,0,1,0,1,21,1,139,0,0,0,174,147,2,0,6,0,0,0,2,0,0,0,44,16,2,128,5,80,4,128,6,80,132,128,39,80,129, 
  128,54,208,2,128,83,144,3,128,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,4,19,45, 
  0,0,0,157,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,1,1,0,0,2,0,1,8,2,19,61,0,0,0,192,0,0,0,3,0,1,21, 
  0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,107,192,0,0,1,19, 
  21,0,0,0,58,0,0,0,1,0,17,1,107,192,0,0,1,19,61,0,0,0,192,0,0,0,3,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,107,192,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1, 
  107,192,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0, 
  17,1,235,192,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,235,192,0,0,1,19,61,0,0,0,192,0,0,0,3,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,235,192,0,0,1,19,18,0,0,0, 
  54,0,0,0,1,0,17,1,235,192,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128,58,96,3,128,61,240,3,128,19,18,0, 
  0,0,53,0,0,0,1,0,17,1,107,193,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,107,193,0,0,1,19,61,0,0,0,192,0,0,0,3,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,107,193, 
  0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,107,193,0,0,1,1,2,21,0,127,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,21,112,66,129,17,80,193,127,18,224,129,128,27,208,2,128,58,96,3, 
  128,61,240,3,128,19,18,0,0,0,53,0,0,0,1,0,17,1,235,193,0,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,235,193,0,0,1,19,61,0,0,0,192,0,0,0,3,0,1,19,18,0,0,0,55,0, 
  0,0,1,0,17,1,235,193,0,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,235,193,0,0,1,1,2,21,1,45,0,0,0,26,148,2,0,3,0,0,0,1,0,0,0,6,96,1,128,5,96,65,128,53,240,0, 
  128,15,1,11,87,1,0,4,17,1,25,71,0,0,1,8,2,21,4,60,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,40,64,1,128,123,208,0,128,15,1,23,87,1,0,5,17,1,35,87,1,0,1,15,1,23, 
  87,1,0,15,1,182,87,1,0,5,17,1,194,87,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,60,0,0,0,0,0,0,0,2,0,0,0,1, 
  0,0,0,40,64,1,128,123,208,0,128,15,1,122,88,1,0,5,17,1,134,88,1,0,1,15,1,122,88,1,0,15,1,25,89,1,0,5,17,1,37,89,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5, 
  0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,60,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,40,64,1,128,123,208,0,128,15,1,118,89,1,0,5,17,1,130,89,1,0,1,15,1,118,89,1,0,15, 
  1,21,90,1,0,5,17,1,33,90,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,88,0,0,0,5,1,0,0,3,0,1,20,4,30,0,0,0,0, 
  0,0,0,1,0,0,0,44,0,0,0,22,0,0,0,5,17,1,101,90,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,6,0,0,0,31,0,0,0, 
  3,0,1,19,6,0,0,0,32,0,0,0,3,0,1,21,1,60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216,81, 
  129,103,192,196,126,125,144,11,128,128,216,17,128,132,216,17,128,134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15, 
  1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0, 
  0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62, 
  41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15, 
  1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0, 
  0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,10,91,1,0,4, 
  19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0, 
  0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,19,15,0,0,0,47,0,0,0,2,0,1, 
  19,15,0,0,0,48,0,0,0,2,0,1,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32, 
  130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,84,198,0,0,1,1,15,1,84,198,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,84,198,0,0,1,19,24,0,0,0,65,0,0, 
  0,1,0,17,1,84,198,0,0,1,15,1,84,198,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40, 
  2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,228,198,0,0,1,1,15,1,228,198,0,0,17,1,17,86,0,0,1,19,23, 
  0,0,0,62,0,0,0,1,0,17,1,228,198,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,228,198,0,0,1,15,1,228,198,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2, 
  21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1, 
  0,17,1,116,199,0,0,1,1,15,1,116,199,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,116,199,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,116,199,0,0,1,15,1, 
  116,199,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144, 
  129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,4,200,0,0,1,1,15,1,4,200,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,4, 
  200,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,4,200,0,0,1,15,1,4,200,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0, 
  0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,148,200,0,0,1,1,15,1,148, 
  200,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,148,200,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,148,200,0,0,1,15,1,148,200,0,0,17,1,81,86,0,0,1,19, 
  15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32, 
  130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,36,201,0,0,1,1,15,1,36,201,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,36,201,0,0,1,19,24,0,0,0,65,0,0, 
  0,1,0,17,1,36,201,0,0,1,15,1,36,201,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40, 
  2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,180,201,0,0,1,1,15,1,180,201,0,0,17,1,17,86,0,0,1,19,23, 
  0,0,0,62,0,0,0,1,0,17,1,180,201,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,180,201,0,0,1,15,1,180,201,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2, 
  21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184, 
  12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1, 
  15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,206,91,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13, 
  0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1, 
  20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0, 
  17,1,132,42,0,0,1,15,1,94,92,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0, 
  0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130, 
  111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19, 
  20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,238,92, 
  1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0, 
  0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7, 
  128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19, 
  95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221, 
  0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,126,93,1,0,4,19,19,0,0,0,56,0,0, 
  0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184, 
  76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219, 
  42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,14,94,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128, 
  184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,158,94,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0, 
  43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128, 
  135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48, 
  0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0, 
  4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,15,1,46,95,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1, 
  0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0, 
  0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45, 
  0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1, 
  76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,15,1,190,95,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1, 
  0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96, 
  4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0, 
  0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,78,96,1,0,4,19,19,0, 
  0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1, 
  0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5, 
  184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0, 
  0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1, 
  0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19, 
  40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,222,96,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104, 
  29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,62,218,0,0,1,1,15,1,62,218,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,62,218,0,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,62,218,0,0,1,15,1,62,218,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,65,0,0,0,184,153,2,0,6,0,0,0,2,0,0,0,128,0,2,129,5,0,130,128, 
  6,0,2,128,89,80,129,128,132,0,2,128,133,192,1,128,15,1,110,97,1,0,4,17,1,225,91,0,0,1,4,17,1,174,97,1,0,1,8,19,96,0,0,0,38,1,0,0,2,0,1,21,1,59,0,0,0,109,154,2, 
  0,6,0,0,0,2,0,0,0,128,208,1,129,5,208,1,129,6,208,1,128,87,80,1,128,132,208,1,128,133,144,1,128,4,17,1,239,97,1,0,1,4,17,1,47,98,1,0,1,8,19,96,0,0,0,36,1,0,0,2, 
  0,1,21,1,65,0,0,0,79,155,2,0,6,0,0,0,2,0,0,0,128,0,66,129,5,0,194,128,6,0,2,128,87,80,1,128,89,144,1,128,132,0,2,128,4,17,1,111,98,1,0,1,15,1,176,98,1,0,4,17, 
  1,225,91,0,0,1,8,19,96,0,0,0,39,1,0,0,2,0,1,19,91,0,0,0,10,1,0,0,4,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,56,0,0,0,185, 
  0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,36,0,0,0,22,0,0,0,15,1,250,219, 
  0,0,17,1,240,98,1,0,1,2,19,36,0,0,0,126,0,0,0,1,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,69,0,0,0,220,0,0,0,3,0,1,20,2,27, 
  0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,68,0,0,0,22,0,0,0,15,1,105,220,0,0,17,1,61,99,1,0,1, 
  2,19,68,0,0,0,219,0,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,5,17,1,138,99,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0, 
  0,26,0,0,0,26,0,0,0,9,2,21,1,52,0,0,0,88,102,2,0,5,0,0,0,2,0,0,0,128,152,1,129,5,152,129,128,6,152,1,128,97,48,1,128,132,152,1,128,4,19,1,0,0,0,2,0,0,0,4, 
  0,1,8,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1, 
  0,17,1,8,221,0,0,1,1,15,1,8,221,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,8,221,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,8,221,0,0,1,15,1, 
  8,221,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0, 
  67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,148,221,0,0,1,1,15,1,148,221,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,148,221,0,0,1, 
  19,24,0,0,0,65,0,0,0,1,0,17,1,148,221,0,0,1,15,1,148,221,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0, 
  0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,32,222,0,0,1,1,15,1,32,222,0,0,17,1,17,86,0, 
  0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,32,222,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,32,222,0,0,1,15,1,32,222,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0, 
  2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1, 
  0,17,1,172,222,0,0,1,1,15,1,172,222,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,172,222,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,172,222,0,0,1,15,1, 
  172,222,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0, 
  67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,56,223,0,0,1,1,15,1,56,223,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,56,223,0,0,1, 
  19,24,0,0,0,65,0,0,0,1,0,17,1,56,223,0,0,1,15,1,56,223,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0, 
  0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,196,223,0,0,1,1,15,1,196,223,0,0,17,1,17,86,0, 
  0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,196,223,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,196,223,0,0,1,15,1,196,223,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0, 
  2,0,1,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216, 
  11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,219,99,1,0, 
  4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108, 
  64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0, 
  0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72, 
  198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0, 
  0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,103,100,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1, 
  0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0, 
  3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0, 
  0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,243,100,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11, 
  128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,127, 
  101,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0, 
  0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12,0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160, 
  6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1, 
  207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1, 
  15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,11,102,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,124,1,0,0,162,98,2,0,12, 
  0,0,0,3,0,0,0,88,16,2,130,111,112,133,130,125,16,7,128,123,160,6,128,108,64,68,129,5,216,75,127,6,216,11,129,103,64,131,126,128,216,11,128,132,216,11,128,134,16,9,128,135,216,9,128,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1, 
  213,45,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19, 
  40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,151,102,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104, 
  29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,8,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0, 
  0,64,0,0,0,1,0,17,1,62,233,0,0,1,1,15,1,62,233,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,62,233,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,62, 
  233,0,0,1,15,1,62,233,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,19,96,0,0,0,42,1,0,0,4,0,1,19,96,0,0,0,33,1,0,0,4,0,1,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,226,233,0, 
  0,1,1,15,1,226,233,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,226,233,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,226,233,0,0,1,15,1,226,233,0,0,17,1, 
  81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128, 
  96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,114,234,0,0,1,1,15,1,114,234,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,114,234,0,0,1,19,24, 
  0,0,0,65,0,0,0,1,0,17,1,114,234,0,0,1,15,1,114,234,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,2,235,0,0,1,1,15,1,2,235,0,0,17,1,17, 
  86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,2,235,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,2,235,0,0,1,15,1,2,235,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0, 
  0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0, 
  0,64,0,0,0,1,0,17,1,146,235,0,0,1,1,15,1,146,235,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,146,235,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,146, 
  235,0,0,1,15,1,146,235,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128, 
  72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,34,236,0,0,1,1,15,1,34,236,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0, 
  0,1,0,17,1,34,236,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,34,236,0,0,1,15,1,34,236,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0, 
  0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,178,236,0, 
  0,1,1,15,1,178,236,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,178,236,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,178,236,0,0,1,15,1,178,236,0,0,17,1, 
  81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128, 
  96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,66,237,0,0,1,1,15,1,66,237,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,66,237,0,0,1,19,24, 
  0,0,0,65,0,0,0,1,0,17,1,66,237,0,0,1,15,1,66,237,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0, 
  88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0, 
  0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76, 
  26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1, 
  15,1,35,103,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0, 
  19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4, 
  128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0, 
  0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70, 
  0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,179,103,1,0,4,19,19,0,0, 
  0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184, 
  204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0, 
  4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0, 
  0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40, 
  0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,67,104,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0, 
  17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125, 
  240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0, 
  0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34, 
  0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,211,104,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1, 
  152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128, 
  134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1, 
  72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23, 
  0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,99,105,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0, 
  1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0, 
  0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4, 
  17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26, 
  0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,15,1,243,105,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80, 
  198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0, 
  0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,131,106,1,0, 
  4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108, 
  32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15, 
  1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0, 
  0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0, 
  0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,19,107,1,0,4,19,19,0,0,0,56,0,0,0,1, 
  0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129, 
  103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,163,107,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0, 
  0,0,1,0,17,1,51,252,0,0,1,1,15,1,51,252,0,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,51,252,0,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,51,252,0,0, 
  1,15,1,51,252,0,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,53,0,0,0,50,117,2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160,1,128,6,160,65,128,110,48,1, 
  128,132,160,1,128,15,1,51,108,1,0,4,17,1,162,90,0,0,1,8,19,91,0,0,0,17,1,0,0,4,0,1,19,91,0,0,0,13,1,0,0,4,0,1,19,91,0,0,0,11,1,0,0,4,0,1,21,1,89,0, 
  0,0,63,97,2,0,7,0,0,0,2,0,0,0,104,224,65,129,5,192,2,128,6,192,130,128,99,112,1,128,110,80,2,128,128,192,66,128,132,192,2,128,15,1,131,168,0,0,4,17,1,44,87,0,0,1,15,1,214,168, 
  0,0,4,17,1,96,90,0,0,1,15,1,23,169,0,0,4,17,1,162,90,0,0,1,8,2,21,1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128, 
  132,112,1,128,4,17,1,63,108,1,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129, 
  103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,124,110,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12, 
  128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48, 
  0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,12,111,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134, 
  2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184, 
  10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0, 
  4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17, 
  1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0, 
  0,1,0,17,1,132,42,0,0,1,15,1,156,111,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88, 
  48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0, 
  1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15, 
  1,44,112,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128, 
  123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0, 
  1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0, 
  0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,188,112,1,0,4,19,19,0,0,0, 
  56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19, 
  23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204, 
  128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4, 
  17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0, 
  2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0, 
  0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,76,113,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17, 
  1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240, 
  7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0, 
  4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0, 
  0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,220,113,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152, 
  1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134, 
  240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72, 
  198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0, 
  0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,108,114,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1, 
  0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0, 
  3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17, 
  1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0, 
  0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132, 
  42,0,0,1,15,1,252,114,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0, 
  0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,136,194,128,60,24,3, 
  129,82,16,4,128,72,168,195,128,12,144,65,127,76,16,4,128,96,24,4,128,23,32,2,128,19,24,0,0,0,64,0,0,0,1,0,17,1,7,12,1,0,1,15,1,7,12,1,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,7,12,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,7,12,1,0,1,15,1,7,12,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2,19, 
  76,0,0,0,236,0,0,0,2,0,1,21,1,129,1,0,0,189,140,2,0,12,0,0,0,3,0,0,0,88,168,2,130,30,16,66,130,111,8,70,130,125,56,7,128,108,216,68,129,5,0,140,127,6,0,204,126,103,216,195, 
  126,128,0,12,128,132,0,12,128,134,56,9,128,135,0,10,128,4,19,82,0,0,0,252,0,0,0,4,0,17,1,140,115,1,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0, 
  0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17, 
  1,132,42,0,0,1,15,1,140,115,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0, 
  141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0,0,50,120,2,0,11,0,0,0,3,0,0,0,88,240,193,128,111, 
  80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195, 
  42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,151,12,1,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0, 
  1,0,17,1,132,42,0,0,1,15,1,28,116,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40, 
  0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0,0,50,120,2,0,11,0,0,0,3,0,0,0,88,240, 
  193,128,111,80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,151,12,1,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0, 
  4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,15,1,168,116,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1, 
  0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0,0,50,120,2,0,11,0,0,0,3,0,0, 
  0,88,240,193,128,111,80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,151,12,1, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174, 
  48,0,0,4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0, 
  0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,52,117,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1, 
  0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0,0,50,120,2,0,11,0,0,0, 
  3,0,0,0,88,240,193,128,111,80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151,12,1,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1, 
  151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,192,117,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0,0,50,120,2,0,11, 
  0,0,0,3,0,0,0,88,240,193,128,111,80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151,12,1,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0, 
  1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195, 
  42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,76,118,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0,0,50,120, 
  2,0,11,0,0,0,3,0,0,0,88,240,193,128,111,80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151,12,1,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213, 
  45,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0, 
  115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,216,118,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1,51,42,0, 
  0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,106,1,0, 
  0,50,120,2,0,11,0,0,0,3,0,0,0,88,240,193,128,111,80,69,130,125,128,6,128,128,72,11,128,108,32,4,129,5,72,75,127,6,72,203,128,103,32,131,126,132,72,11,128,134,128,8,128,135,72,9,128,15,1,151, 
  12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4, 
  17,1,213,45,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,151,12,1,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34, 
  0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,100,119,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,151,12,1,0,15,1, 
  51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1, 
  60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216,81,129,103,192,196,126,125,144,11,128,128,216,17,128,132,216,17,128, 
  134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42, 
  0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0, 
  4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0, 
  0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,240,119,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0, 
  1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0, 
  19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3, 
  128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,79,26,1,0,1,1,15,1,79,26,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0, 
  0,0,1,0,17,1,79,26,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,79,26,1,0,1,15,1,79,26,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0, 
  0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,223,26, 
  1,0,1,1,15,1,223,26,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,223,26,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,223,26,1,0,1,15,1,223,26,1,0,17, 
  1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2, 
  128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,111,27,1,0,1,1,15,1,111,27,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,111,27,1,0,1,19, 
  24,0,0,0,65,0,0,0,1,0,17,1,111,27,1,0,1,15,1,111,27,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0, 
  0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,255,27,1,0,1,1,15,1,255,27,1,0,17,1, 
  17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,255,27,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,255,27,1,0,1,15,1,255,27,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47, 
  0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0, 
  0,0,64,0,0,0,1,0,17,1,143,28,1,0,1,1,15,1,143,28,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,143,28,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1, 
  143,28,1,0,1,15,1,143,28,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3, 
  128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,31,29,1,0,1,1,15,1,31,29,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0, 
  0,0,1,0,17,1,31,29,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,31,29,1,0,1,15,1,31,29,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0, 
  0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,175,29, 
  1,0,1,1,15,1,175,29,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,175,29,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,175,29,1,0,1,15,1,175,29,1,0,17, 
  1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2, 
  128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,63,30,1,0,1,1,15,1,63,30,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,63,30,1,0,1,19, 
  24,0,0,0,65,0,0,0,1,0,17,1,63,30,1,0,1,15,1,63,30,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0, 
  0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,207,30,1,0,1,1,15,1,207,30,1,0,17,1, 
  17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,207,30,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,207,30,1,0,1,15,1,207,30,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47, 
  0,0,0,2,0,1,2,19,25,0,0,0,67,0,0,0,2,0,1,21,1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,4,17,1, 
  180,120,1,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7, 
  128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4, 
  17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0, 
  0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,241,122,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1, 
  0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240, 
  9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198, 
  0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,129,123,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0, 
  0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3, 
  0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1, 
  213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0, 
  17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,15,1,17,124,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130, 
  106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0, 
  57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,161,124,1,0,4,19, 
  19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133, 
  129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207, 
  42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0, 
  31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1, 
  0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,49,125,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17, 
  1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0, 
  0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96, 
  131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1, 
  174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1, 
  0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,193,125,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8, 
  2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132, 
  184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0, 
  1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1, 
  0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,81,126,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0, 
  0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,232,0,0,0,223,134,2,0, 
  20,0,0,0,4,0,0,0,128,56,7,128,113,120,6,128,102,80,4,128,99,16,4,128,132,56,7,128,5,56,7,128,6,56,7,127,87,80,67,130,88,144,67,130,89,208,131,130,106,16,5,128,123,248,6,128,108,80,5,128, 
  29,16,67,129,110,248,5,128,111,56,6,128,103,144,4,128,104,208,4,128,109,144,5,128,121,184,6,128,12,17,1,225,126,1,0,1,12,17,1,122,128,1,0,1,12,17,1,155,31,1,0,1,12,17,1,19,130,1,0,1, 
  12,17,1,172,131,1,0,1,12,17,1,69,133,1,0,1,12,17,1,52,33,1,0,1,12,17,1,222,134,1,0,1,12,17,1,205,34,1,0,1,12,17,1,102,36,1,0,1,12,19,95,0,0,0,32,1,0,0,1,0, 
  1,12,17,1,119,136,1,0,1,12,17,1,255,37,1,0,1,12,17,1,16,138,1,0,1,12,17,1,169,139,1,0,1,12,17,1,152,39,1,0,1,10,12,17,1,66,141,1,0,1,21,1,152,1,0,0,43,134,2,0, 
  13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15, 
  1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106, 
  49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,15,1,219,142,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0, 
  0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1, 
  129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,4,17,1,107,143,1,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7, 
  128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19, 
  95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221, 
  0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,168,145,1,0,4,19,19,0,0,0,56,0,0, 
  0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184, 
  76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219, 
  42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,56,146,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128, 
  184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,200,146,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0, 
  43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128, 
  135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48, 
  0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0, 
  4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,15,1,88,147,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1, 
  0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0, 
  0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45, 
  0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1, 
  76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,15,1,232,147,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1, 
  0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96, 
  4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0, 
  0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,120,148,1,0,4,19,19,0, 
  0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1, 
  0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5, 
  184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0, 
  0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1, 
  0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19, 
  40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,8,149,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104, 
  29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,8,2,21,1,232,0,0,0,223,134,2,0,20,0,0,0,4,0,0,0,128,56,7,128,113,120,6,128,102,80,4,128,99,16,4,128,132,56,7,128,5,56,7,128,6,56,7,127,87,80,67,130, 
  88,144,67,130,89,208,131,130,106,16,5,128,123,248,6,128,108,80,5,128,29,16,67,129,110,248,5,128,111,56,6,128,103,144,4,128,104,208,4,128,109,144,5,128,121,184,6,128,12,17,1,152,149,1,0,1,12,17,1,49, 
  151,1,0,1,12,17,1,131,45,1,0,1,12,17,1,202,152,1,0,1,12,17,1,99,154,1,0,1,12,17,1,252,155,1,0,1,12,17,1,28,47,1,0,1,12,17,1,149,157,1,0,1,12,17,1,181,48,1,0,1, 
  12,17,1,78,50,1,0,1,12,19,95,0,0,0,32,1,0,0,1,0,1,12,17,1,46,159,1,0,1,12,17,1,231,51,1,0,1,12,17,1,199,160,1,0,1,12,17,1,96,162,1,0,1,12,17,1,128,53,1,0, 
  1,10,12,17,1,249,163,1,0,1,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126, 
  125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48, 
  0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,146,165,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21, 
  1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,4,17,1,34,166,1,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0, 
  0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4, 
  17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26, 
  0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,15,1,95,168,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80, 
  198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0, 
  0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,239,168,1,0, 
  4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108, 
  32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15, 
  1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0, 
  0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0, 
  0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,127,169,1,0,4,19,19,0,0,0,56,0,0,0,1, 
  0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129, 
  103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,15,170,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12, 
  128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48, 
  0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,159,170,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134, 
  2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184, 
  10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0, 
  4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17, 
  1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0, 
  0,1,0,17,1,132,42,0,0,1,15,1,47,171,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88, 
  48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0, 
  1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15, 
  1,191,171,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,232,0,0,0,223,134,2,0,20,0,0,0,4,0,0,0,128,56,7,128,113,120,6,128,102,80,4,128, 
  99,16,4,128,132,56,7,128,5,56,7,128,6,56,7,127,87,80,67,130,88,144,67,130,89,208,131,130,106,16,5,128,123,248,6,128,108,80,5,128,29,16,67,129,110,248,5,128,111,56,6,128,103,144,4,128,104,208,4,128, 
  109,144,5,128,121,184,6,128,12,17,1,79,172,1,0,1,12,17,1,232,173,1,0,1,12,17,1,107,59,1,0,1,12,17,1,129,175,1,0,1,12,17,1,26,177,1,0,1,12,17,1,179,178,1,0,1,12,17,1,4, 
  61,1,0,1,12,17,1,76,180,1,0,1,12,17,1,157,62,1,0,1,12,17,1,54,64,1,0,1,12,19,95,0,0,0,32,1,0,0,1,0,1,12,17,1,229,181,1,0,1,12,17,1,207,65,1,0,1,12,17,1, 
  126,183,1,0,1,12,17,1,23,185,1,0,1,12,17,1,104,67,1,0,1,10,12,17,1,176,186,1,0,1,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128, 
  123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0, 
  1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0, 
  0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,73,188,1,0,4,19,19,0,0,0, 
  56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19, 
  23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,47,0,0,0,169,133,2,0,5,0,0,0,2,0,0,0,128,112,1,129,5,112,1,128,6,112,65,128,102,48,1,128,132,112,1,128,4,17,1, 
  217,188,1,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7, 
  128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4, 
  17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0, 
  0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,22,191,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1, 
  0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240, 
  9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198, 
  0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,166,191,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0, 
  0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3, 
  0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1, 
  213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0, 
  17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,15,1,54,192,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130, 
  106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0, 
  57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,198,192,1,0,4,19, 
  19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133, 
  129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207, 
  42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0, 
  31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1, 
  0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,86,193,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17, 
  1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0, 
  0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96, 
  131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1, 
  174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1, 
  0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,230,193,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8, 
  2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132, 
  184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0, 
  1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1, 
  0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,118,194,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0, 
  0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,232,0,0,0,223,134,2,0, 
  20,0,0,0,4,0,0,0,128,56,7,128,113,120,6,128,102,80,4,128,99,16,4,128,132,56,7,128,5,56,7,128,6,56,7,127,87,80,67,130,88,144,67,130,89,208,131,130,106,16,5,128,123,248,6,128,108,80,5,128, 
  29,16,67,129,110,248,5,128,111,56,6,128,103,144,4,128,104,208,4,128,109,144,5,128,121,184,6,128,12,17,1,6,195,1,0,1,12,17,1,159,196,1,0,1,12,17,1,83,73,1,0,1,12,17,1,56,198,1,0,1, 
  12,17,1,209,199,1,0,1,12,17,1,106,201,1,0,1,12,17,1,236,74,1,0,1,12,17,1,3,203,1,0,1,12,17,1,133,76,1,0,1,12,17,1,30,78,1,0,1,12,19,95,0,0,0,32,1,0,0,1,0, 
  1,12,17,1,156,204,1,0,1,12,17,1,183,79,1,0,1,12,17,1,53,206,1,0,1,12,17,1,206,207,1,0,1,12,17,1,80,81,1,0,1,10,12,17,1,103,209,1,0,1,21,1,152,1,0,0,43,134,2,0, 
  13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15, 
  1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106, 
  49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,15,1,0,211,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0, 
  0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,19,104,0,0,0,61,1,0,0,2,0,1,19,27,0,0,0,70,0,0,0, 
  4,0,1,21,1,146,0,0,0,120,162,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5,136,196,128,6,136,68,128,62,32,2,128,89,240,2,128,108,192,3,128,15,1,144,211,1,0,15,1,207,211,1,0,15,1,243, 
  211,1,0,4,17,1,255,211,1,0,1,15,1,144,211,1,0,15,1,207,211,1,0,15,1,57,212,1,0,4,17,1,69,212,1,0,1,15,1,144,211,1,0,15,1,207,211,1,0,15,1,243,211,1,0,4,17,1,127,212, 
  1,0,1,15,1,144,211,1,0,15,1,207,211,1,0,15,1,209,214,1,0,17,1,221,214,1,0,1,8,2,19,5,0,0,0,29,0,0,0,1,0,1,21,1,183,0,0,0,207,162,2,0,7,0,0,0,2,0,0,0, 
  88,112,1,128,5,176,5,129,6,176,5,128,103,16,66,128,111,128,130,128,125,32,3,128,135,104,4,128,15,1,23,215,1,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,23,215,1,0,4,17,1,213,45,0, 
  0,1,15,1,23,215,1,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,17,1,23,215, 
  1,0,1,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,17,1,23,215,1,0,1,8,2,19,58,0,0,0,188,0,0,0,4,0,1,21,1, 
  146,0,0,0,120,162,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5,136,196,128,6,136,68,128,62,32,2,128,89,240,2,128,108,192,3,128,15,1,81,215,1,0,15,1,144,215,1,0,15,1,180,215,1,0,4,17, 
  1,255,211,1,0,1,15,1,81,215,1,0,15,1,144,215,1,0,15,1,192,215,1,0,4,17,1,69,212,1,0,1,15,1,81,215,1,0,15,1,144,215,1,0,15,1,180,215,1,0,4,17,1,127,212,1,0,1,15,1, 
  81,215,1,0,15,1,144,215,1,0,15,1,204,215,1,0,17,1,216,215,1,0,1,8,2,19,0,0,0,0,0,0,0,0,1,0,1,21,1,80,0,0,0,64,98,2,0,4,0,0,0,2,0,0,0,108,16,1,128,5, 
  120,2,128,6,120,66,128,134,176,1,128,15,1,18,216,1,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,76,216,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,16,52,0,0,1,8,2,19,17, 
  0,0,0,52,0,0,0,4,0,1,21,1,146,0,0,0,120,162,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5,136,196,128,6,136,68,128,62,32,2,128,89,240,2,128,108,192,3,128,15,1,155,216,1,0,15,1, 
  218,216,1,0,15,1,254,216,1,0,4,17,1,255,211,1,0,1,15,1,155,216,1,0,15,1,218,216,1,0,15,1,10,217,1,0,4,17,1,69,212,1,0,1,15,1,155,216,1,0,15,1,218,216,1,0,15,1,254,216, 
  1,0,4,17,1,127,212,1,0,1,15,1,155,216,1,0,15,1,218,216,1,0,15,1,22,217,1,0,17,1,34,217,1,0,1,8,2,19,103,0,0,0,59,1,0,0,1,0,1,20,2,67,0,0,0,0,0,0,0,3, 
  0,0,0,4,0,0,0,30,0,0,0,66,0,0,0,66,0,0,0,15,1,92,217,1,0,5,19,66,0,0,0,216,0,0,0,1,0,19,71,0,0,0,223,0,0,0,1,0,17,1,150,217,1,0,1,9,2,21,1,152, 
  0,0,0,219,90,2,0,7,0,0,0,2,0,0,0,16,112,1,129,5,184,4,128,6,184,196,128,79,176,2,128,76,16,2,128,82,80,67,128,126,240,3,128,15,1,186,217,1,0,15,1,133,84,0,0,4,17,1,13,6, 
  0,0,1,15,1,186,217,1,0,15,1,133,84,0,0,4,17,1,70,10,0,0,1,15,1,186,217,1,0,15,1,133,84,0,0,4,17,1,145,10,0,0,1,15,1,186,217,1,0,15,1,145,84,0,0,4,17,1,157,84, 
  0,0,1,15,1,186,217,1,0,4,19,53,0,0,0,180,0,0,0,1,0,17,1,213,84,0,0,1,8,2,19,10,0,0,0,39,0,0,0,3,0,1,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0, 
  24,56,131,129,23,208,194,129,10,208,1,128,91,192,4,128,12,216,65,128,60,200,3,128,72,88,196,128,15,104,130,126,95,32,5,128,96,136,5,128,1,19,24,0,0,0,64,0,0,0,1,0,17,1,10,91,1,0,1,15, 
  1,10,91,1,0,17,1,198,217,1,0,1,15,1,10,91,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,10,91,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,10,91,1,0, 
  1,15,1,10,91,1,0,17,1,81,86,0,0,1,19,10,0,0,0,39,0,0,0,3,0,1,15,1,10,91,1,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,10,91,1,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,206,91,1,0,1,1,15,1,206,91,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,206,91,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,206,91,1,0,1,15,1,206,91, 
  1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127, 
  95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,94,92,1,0,1,1,15,1,94,92,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,94,92,1, 
  0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,94,92,1,0,1,15,1,94,92,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,238,92,1,0,1,1,15,1,238,92,1, 
  0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,238,92,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,238,92,1,0,1,15,1,238,92,1,0,17,1,81,86,0,0,1,19,15,0, 
  0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,126,93,1,0,1,1,15,1,126,93,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,126,93,1,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,126,93,1,0,1,15,1,126,93,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,14,94,1,0,1,1,15,1,14,94,1,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,14,94,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,14,94,1,0,1,15,1,14,94,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,158,94,1,0,1,1,15,1,158,94,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,158,94,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,158,94,1,0,1,15,1,158,94, 
  1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127, 
  95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,46,95,1,0,1,1,15,1,46,95,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,46,95,1, 
  0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,46,95,1,0,1,15,1,46,95,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,190,95,1,0,1,1,15,1,190,95,1, 
  0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,190,95,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,190,95,1,0,1,15,1,190,95,1,0,17,1,81,86,0,0,1,19,15,0, 
  0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,78,96,1,0,1,1,15,1,78,96,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,78,96,1,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,78,96,1,0,1,15,1,78,96,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,222,96,1,0,1,1,15,1,222,96,1,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,222,96,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,222,96,1,0,1,15,1,222,96,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1, 
  52,0,0,0,56,164,2,0,5,0,0,0,2,0,0,0,128,152,193,128,5,152,193,128,6,152,1,128,132,152,1,128,133,48,1,128,4,19,96,0,0,0,44,1,0,0,4,0,1,8,19,96,0,0,0,34,1,0,0,3, 
  0,1,21,1,53,0,0,0,237,164,2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160,129,128,6,160,1,128,89,48,1,128,132,160,1,128,15,1,202,233,0,0,4,17,1,225,91,0,0,1,8,19,96,0,0,0, 
  43,1,0,0,3,0,1,21,1,52,0,0,0,161,165,2,0,5,0,0,0,2,0,0,0,128,152,193,128,5,152,193,128,6,152,1,128,132,152,1,128,133,48,1,128,4,19,96,0,0,0,47,1,0,0,4,0,1,8,19, 
  96,0,0,0,41,1,0,0,3,0,1,21,1,52,0,0,0,120,166,2,0,5,0,0,0,2,0,0,0,128,152,1,129,5,152,1,128,6,152,1,128,87,48,1,128,132,152,1,128,4,19,96,0,0,0,46,1,0,0,4, 
  0,1,8,19,96,0,0,0,45,1,0,0,3,0,1,21,1,53,0,0,0,78,167,2,0,5,0,0,0,2,0,0,0,128,160,1,129,5,160,129,128,6,160,1,128,89,48,1,128,132,160,1,128,15,1,214,233,0,0,4, 
  17,1,225,91,0,0,1,8,19,96,0,0,0,37,1,0,0,3,0,1,21,1,52,0,0,0,36,168,2,0,5,0,0,0,2,0,0,0,128,152,1,129,5,152,1,128,6,152,1,128,87,48,1,128,132,152,1,128,4,19, 
  96,0,0,0,40,1,0,0,4,0,1,8,19,96,0,0,0,35,1,0,0,3,0,1,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,125,208,0,128,59,16,193,127,12,17,1,76,36,0,0,1,12,17, 
  1,98,218,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12,17,1,76,36,0,0,1,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,44, 
  16,1,128,125,208,0,128,12,17,1,76,36,0,0,1,12,17,1,156,218,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12,17,1,76,36,0,0,1,21,1, 
  80,0,0,0,239,168,2,0,4,0,0,0,2,0,0,0,126,176,1,128,5,120,2,128,6,120,130,127,47,16,1,128,15,1,214,218,1,0,15,1,226,218,1,0,4,17,1,238,218,1,0,1,15,1,214,218,1,0,4,19, 
  53,0,0,0,180,0,0,0,1,0,17,1,40,219,1,0,1,8,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67, 
  128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,219,99,1,0,1,1,15,1,219,99,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,219,99,1,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,219,99,1,0,1,15,1,219,99,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129, 
  128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,103,100,1,0,1,1,15,1,103,100,1,0,17,1,17,86,0,0,1,19,23,0, 
  0,0,62,0,0,0,1,0,17,1,103,100,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,103,100,1,0,1,15,1,103,100,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21, 
  0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,243,100, 
  1,0,1,1,15,1,243,100,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,243,100,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,243,100,1,0,1,15,1,243,100,1,0,17, 
  1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67, 
  128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,127,101,1,0,1,1,15,1,127,101,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,127,101,1,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,127,101,1,0,1,15,1,127,101,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129, 
  128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,11,102,1,0,1,1,15,1,11,102,1,0,17,1,17,86,0,0,1,19,23,0, 
  0,0,62,0,0,0,1,0,17,1,11,102,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,11,102,1,0,1,15,1,11,102,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21, 
  0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,129,128,23,8,2,128,24,112,130,128,15,0,130,127,60,0,67,128,72,144,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,151,102, 
  1,0,1,1,15,1,151,102,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,151,102,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,151,102,1,0,1,15,1,151,102,1,0,17, 
  1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2, 
  128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,35,103,1,0,1,1,15,1,35,103,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,35,103,1,0,1,19, 
  24,0,0,0,65,0,0,0,1,0,17,1,35,103,1,0,1,15,1,35,103,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0, 
  0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,179,103,1,0,1,1,15,1,179,103,1,0,17,1, 
  17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,179,103,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,179,103,1,0,1,15,1,179,103,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47, 
  0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0, 
  0,0,64,0,0,0,1,0,17,1,67,104,1,0,1,1,15,1,67,104,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,67,104,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1, 
  67,104,1,0,1,15,1,67,104,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3, 
  128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,211,104,1,0,1,1,15,1,211,104,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0, 
  0,0,1,0,17,1,211,104,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,211,104,1,0,1,15,1,211,104,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0, 
  0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,99,105, 
  1,0,1,1,15,1,99,105,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,99,105,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,99,105,1,0,1,15,1,99,105,1,0,17, 
  1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2, 
  128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,243,105,1,0,1,1,15,1,243,105,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,243,105,1,0,1,19, 
  24,0,0,0,65,0,0,0,1,0,17,1,243,105,1,0,1,15,1,243,105,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0, 
  0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,131,106,1,0,1,1,15,1,131,106,1,0,17,1, 
  17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,131,106,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,131,106,1,0,1,15,1,131,106,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47, 
  0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0, 
  0,0,64,0,0,0,1,0,17,1,19,107,1,0,1,1,15,1,19,107,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,19,107,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1, 
  19,107,1,0,1,15,1,19,107,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3, 
  128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,163,107,1,0,1,1,15,1,163,107,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0, 
  0,0,1,0,17,1,163,107,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,163,107,1,0,1,15,1,163,107,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,19,91,0,0,0, 
  9,1,0,0,5,0,1,21,1,60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216,81,129,103,192,196,126,125,144,11, 
  128,128,216,17,128,132,216,17,128,134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137, 
  41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15, 
  1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0, 
  0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137, 
  41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221, 
  0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,97,219,1,0,4,19,19,0,0,0,56,0,0, 
  0,1,0,17,1,104,29,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40, 
  0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144, 
  194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,124,110,1,0,1,1,15,1,124,110,1,0,17,1,17,86,0, 
  0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,124,110,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,124,110,1,0,1,15,1,124,110,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0, 
  2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64, 
  0,0,0,1,0,17,1,12,111,1,0,1,1,15,1,12,111,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,12,111,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,12,111,1, 
  0,1,15,1,12,111,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176, 
  195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,156,111,1,0,1,1,15,1,156,111,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1, 
  0,17,1,156,111,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,156,111,1,0,1,15,1,156,111,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0, 
  0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,44,112,1,0,1, 
  1,15,1,44,112,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,44,112,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,44,112,1,0,1,15,1,44,112,1,0,17,1,81,86, 
  0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24, 
  4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,188,112,1,0,1,1,15,1,188,112,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,188,112,1,0,1,19,24,0,0, 
  0,65,0,0,0,1,0,17,1,188,112,1,0,1,15,1,188,112,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144, 
  194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,76,113,1,0,1,1,15,1,76,113,1,0,17,1,17,86,0, 
  0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,76,113,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,76,113,1,0,1,15,1,76,113,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0, 
  2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64, 
  0,0,0,1,0,17,1,220,113,1,0,1,1,15,1,220,113,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,220,113,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,220,113,1, 
  0,1,15,1,220,113,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176, 
  195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,108,114,1,0,1,1,15,1,108,114,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1, 
  0,17,1,108,114,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,108,114,1,0,1,15,1,108,114,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0, 
  0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,252,114,1,0,1, 
  1,15,1,252,114,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,252,114,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,252,114,1,0,1,15,1,252,114,1,0,17,1,81,86, 
  0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,136,194,128,60,24,3,129,82,16,4,128,72,168,195,128,12,144,65,127,76,16,4,128,96,24, 
  4,128,23,32,2,128,19,24,0,0,0,64,0,0,0,1,0,17,1,140,115,1,0,1,15,1,140,115,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,140,115,1,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,140,115,1,0,1,15,1,140,115,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112, 
  65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,28,116,1,0,1,15,1,28,116,1,0,17,1,17,86,0,0,1,19,23,0, 
  0,0,62,0,0,0,1,0,17,1,28,116,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,28,116,1,0,1,15,1,28,116,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2, 
  21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,168, 
  116,1,0,1,15,1,168,116,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,168,116,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,168,116,1,0,1,15,1,168,116,1,0,17, 
  1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240, 
  67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,52,117,1,0,1,15,1,52,117,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,52,117,1,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,52,117,1,0,1,15,1,52,117,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112, 
  65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,192,117,1,0,1,15,1,192,117,1,0,17,1,17,86,0,0,1,19,23,0, 
  0,0,62,0,0,0,1,0,17,1,192,117,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,192,117,1,0,1,15,1,192,117,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2, 
  21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,76, 
  118,1,0,1,15,1,76,118,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,76,118,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,76,118,1,0,1,15,1,76,118,1,0,17, 
  1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112,65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240, 
  67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,216,118,1,0,1,15,1,216,118,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,216,118,1,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,216,118,1,0,1,15,1,216,118,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2,21,0,139,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,12,112, 
  65,128,24,104,66,128,60,248,130,128,23,0,2,128,72,136,67,128,76,240,67,128,96,248,3,128,19,24,0,0,0,64,0,0,0,1,0,17,1,100,119,1,0,1,15,1,100,119,1,0,17,1,17,86,0,0,1,19,23,0, 
  0,0,62,0,0,0,1,0,17,1,100,119,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,100,119,1,0,1,15,1,100,119,1,0,17,1,81,86,0,0,1,1,19,76,0,0,0,236,0,0,0,2,0,1,2, 
  21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,24,56,131,129,23,208,194,129,10,208,1,128,91,192,4,128,12,216,65,128,60,200,3,128,72,88,196,128,15,104,130,126,95,32,5,128,96,136,5,128,1,19, 
  24,0,0,0,64,0,0,0,1,0,17,1,240,119,1,0,1,15,1,240,119,1,0,17,1,11,100,0,0,1,15,1,240,119,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,240,119,1,0, 
  1,19,24,0,0,0,65,0,0,0,1,0,17,1,240,119,1,0,1,15,1,240,119,1,0,17,1,81,86,0,0,1,19,10,0,0,0,39,0,0,0,3,0,1,15,1,240,119,1,0,17,1,149,86,0,0,1,19,15,0, 
  0,0,49,0,0,0,1,0,17,1,240,119,1,0,1,2,21,1,60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216, 
  81,129,103,192,196,126,125,144,11,128,128,216,17,128,132,216,17,128,134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0, 
  15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41, 
  0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1, 
  62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0, 
  15,1,62,41,0,0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42, 
  0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,37,220,1,0, 
  4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,241,122,1,0,1,1,15,1, 
  241,122,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,241,122,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,241,122,1,0,1,15,1,241,122,1,0,17,1,81,86,0,0,1, 
  19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15, 
  32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,129,123,1,0,1,1,15,1,129,123,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,129,123,1,0,1,19,24,0,0,0,65,0, 
  0,0,1,0,17,1,129,123,1,0,1,15,1,129,123,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23, 
  40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,17,124,1,0,1,1,15,1,17,124,1,0,17,1,17,86,0,0,1,19, 
  23,0,0,0,62,0,0,0,1,0,17,1,17,124,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,17,124,1,0,1,15,1,17,124,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1, 
  2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0, 
  1,0,17,1,161,124,1,0,1,1,15,1,161,124,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,161,124,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,161,124,1,0,1,15, 
  1,161,124,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12, 
  144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,49,125,1,0,1,1,15,1,49,125,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1, 
  49,125,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,49,125,1,0,1,15,1,49,125,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,193,125,1,0,1,1,15,1, 
  193,125,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,193,125,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,193,125,1,0,1,15,1,193,125,1,0,17,1,81,86,0,0,1, 
  19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15, 
  32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,81,126,1,0,1,1,15,1,81,126,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,81,126,1,0,1,19,24,0,0,0,65,0, 
  0,0,1,0,17,1,81,126,1,0,1,15,1,81,126,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111, 
  80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20, 
  0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,233,220,1, 
  0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0, 
  115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128, 
  108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95, 
  0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,121,221,1,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0, 
  63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76, 
  129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0, 
  0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,9,222,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0, 
  0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184, 
  12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186, 
  48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0, 
  0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,153,222,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4, 
  19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43, 
  134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135, 
  184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0, 
  0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4, 
  17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0, 
  0,0,1,0,17,1,132,42,0,0,1,15,1,41,223,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0, 
  19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0, 
  88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0, 
  0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76, 
  26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1, 
  15,1,185,223,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0, 
  19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4, 
  128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0, 
  0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70, 
  0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,73,224,1,0,4,19,19,0,0, 
  0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184, 
  204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0, 
  4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0, 
  0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40, 
  0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,217,224,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0, 
  17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125, 
  240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0, 
  0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34, 
  0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,105,225,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1, 
  152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128, 
  134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1, 
  72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23, 
  0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,249,225,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0, 
  1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0, 
  0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,219,142,1,0,1,1,15,1,219,142, 
  1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,219,142,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,219,142,1,0,1,15,1,219,142,1,0,17,1,81,86,0,0,1,19,15, 
  0,0,0,47,0,0,0,2,0,1,2,21,1,60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216,81,129,103,192,196, 
  126,125,144,11,128,128,216,17,128,132,216,17,128,134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132, 
  42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0, 
  0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137, 
  41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15, 
  1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0, 
  0,15,1,137,41,0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70, 
  0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,137,226,1,0,4,19,19,0,0, 
  0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0, 
  1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0, 
  0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,168,145,1,0,1,1,15,1,168,145,1,0,17, 
  1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,168,145,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,168,145,1,0,1,15,1,168,145,1,0,17,1,81,86,0,0,1,19,15,0,0,0, 
  47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24, 
  0,0,0,64,0,0,0,1,0,17,1,56,146,1,0,1,1,15,1,56,146,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,56,146,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17, 
  1,56,146,1,0,1,15,1,56,146,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32, 
  3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,200,146,1,0,1,1,15,1,200,146,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62, 
  0,0,0,1,0,17,1,200,146,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,200,146,1,0,1,15,1,200,146,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0, 
  0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,88, 
  147,1,0,1,1,15,1,88,147,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,88,147,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,88,147,1,0,1,15,1,88,147,1,0, 
  17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32, 
  2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,232,147,1,0,1,1,15,1,232,147,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,232,147,1,0,1, 
  19,24,0,0,0,65,0,0,0,1,0,17,1,232,147,1,0,1,15,1,232,147,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0, 
  0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,120,148,1,0,1,1,15,1,120,148,1,0,17, 
  1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,120,148,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,120,148,1,0,1,15,1,120,148,1,0,17,1,81,86,0,0,1,19,15,0,0,0, 
  47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24, 
  0,0,0,64,0,0,0,1,0,17,1,8,149,1,0,1,1,15,1,8,149,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,8,149,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17, 
  1,8,149,1,0,1,15,1,8,149,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96, 
  4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0, 
  0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,77,227,1,0,4,19,19,0, 
  0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1, 
  0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5, 
  184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0, 
  0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1, 
  0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19, 
  40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,221,227,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104, 
  29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126, 
  125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48, 
  0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,109,228,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21, 
  1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12, 
  128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15, 
  1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19, 
  23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,253,228,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0, 
  0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0, 
  0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20, 
  26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17, 
  1,132,42,0,0,1,15,1,141,229,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0, 
  141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111, 
  80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20, 
  0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,29,230,1, 
  0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0, 
  115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128, 
  108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95, 
  0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,173,230,1,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0, 
  63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76, 
  129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0, 
  0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,61,231,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0, 
  0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184, 
  12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186, 
  48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0, 
  0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,205,231,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4, 
  19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43, 
  134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135, 
  184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15, 
  1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0, 
  0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4, 
  17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0, 
  0,0,1,0,17,1,132,42,0,0,1,15,1,93,232,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0, 
  19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0, 
  24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,146,165,1,0,1,1,15,1,146,165,1,0,17,1,17, 
  86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,146,165,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,146,165,1,0,1,15,1,146,165,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0, 
  0,0,2,0,1,2,21,1,60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216,81,129,103,192,196,126,125,144,11,128, 
  128,216,17,128,132,216,17,128,134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41, 
  0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1, 
  39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0, 
  15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41, 
  0,0,15,1,94,49,0,0,4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0, 
  0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,237,232,1,0,4,19,19,0,0,0,56,0,0,0, 
  1,0,17,1,104,29,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0, 
  0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194, 
  128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,95,168,1,0,1,1,15,1,95,168,1,0,17,1,17,86,0,0, 
  1,19,23,0,0,0,62,0,0,0,1,0,17,1,95,168,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,95,168,1,0,1,15,1,95,168,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2, 
  0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0, 
  0,0,1,0,17,1,239,168,1,0,1,1,15,1,239,168,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,239,168,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,239,168,1,0, 
  1,15,1,239,168,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195, 
  128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,127,169,1,0,1,1,15,1,127,169,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0, 
  17,1,127,169,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,127,169,1,0,1,15,1,127,169,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0, 
  0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,15,170,1,0,1,1, 
  15,1,15,170,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,15,170,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,15,170,1,0,1,15,1,15,170,1,0,17,1,81,86,0, 
  0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4, 
  128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,159,170,1,0,1,1,15,1,159,170,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,159,170,1,0,1,19,24,0,0,0, 
  65,0,0,0,1,0,17,1,159,170,1,0,1,15,1,159,170,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194, 
  128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,47,171,1,0,1,1,15,1,47,171,1,0,17,1,17,86,0,0, 
  1,19,23,0,0,0,62,0,0,0,1,0,17,1,47,171,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,47,171,1,0,1,15,1,47,171,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2, 
  0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0, 
  0,0,1,0,17,1,191,171,1,0,1,1,15,1,191,171,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,191,171,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,191,171,1,0, 
  1,15,1,191,171,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7, 
  128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19, 
  95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221, 
  0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,177,233,1,0,4,19,19,0,0,0,56,0,0, 
  0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184, 
  76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219, 
  42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,65,234,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128, 
  184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,209,234,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0, 
  43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128, 
  135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48, 
  0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0, 
  4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,15,1,97,235,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1, 
  0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0, 
  0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45, 
  0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1, 
  76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,15,1,241,235,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1, 
  0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96, 
  4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0, 
  0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,129,236,1,0,4,19,19,0, 
  0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1, 
  0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5, 
  184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0, 
  0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1, 
  0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19, 
  40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,17,237,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104, 
  29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1, 
  0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126, 
  125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48, 
  0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,161,237,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21, 
  1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12, 
  128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0, 
  0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15, 
  1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19, 
  23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,49,238,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0, 
  0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0, 
  0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20, 
  26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17, 
  1,132,42,0,0,1,15,1,193,238,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0, 
  141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23, 
  40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,73,188,1,0,1,1,15,1,73,188,1,0,17,1,17,86,0,0,1,19, 
  23,0,0,0,62,0,0,0,1,0,17,1,73,188,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,73,188,1,0,1,15,1,73,188,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1, 
  2,21,1,60,2,0,0,123,86,2,0,13,0,0,0,3,0,0,0,88,48,66,130,93,32,196,129,111,112,136,130,123,96,10,128,108,128,134,129,5,216,17,127,6,216,81,129,103,192,196,126,125,144,11,128,128,216,17,128,132, 
  216,17,128,134,80,14,128,135,24,15,128,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15, 
  1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,254,90,1,0,4,15,1,44,43,0,0,17,1,218,43,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39, 
  42,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,94, 
  49,0,0,4,17,1,106,49,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19, 
  40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,81,239,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104, 
  29,0,0,1,15,1,254,90,1,0,15,1,217,40,0,0,15,1,62,41,0,0,15,1,137,41,0,0,15,1,39,42,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,22,191,1,0,1,1,15,1,22,191,1,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,22,191,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,22,191,1,0,1,15,1,22,191,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,166,191,1,0,1,1,15,1,166,191,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,166,191,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,166,191,1,0,1,15,1,166,191, 
  1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127, 
  95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,54,192,1,0,1,1,15,1,54,192,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,54,192,1, 
  0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,54,192,1,0,1,15,1,54,192,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,198,192,1,0,1,1,15,1,198,192,1, 
  0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,198,192,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,198,192,1,0,1,15,1,198,192,1,0,17,1,81,86,0,0,1,19,15,0, 
  0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,86,193,1,0,1,1,15,1,86,193,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,86,193,1,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,86,193,1,0,1,15,1,86,193,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,230,193,1,0,1,1,15,1,230,193,1,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,230,193,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,230,193,1,0,1,15,1,230,193,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,118,194,1,0,1,1,15,1,118,194,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,118,194,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,118,194,1,0,1,15,1,118,194, 
  1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129, 
  5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42, 
  0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31, 
  1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0, 
  19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,21,240,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1, 
  104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0, 
  1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131, 
  126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174, 
  48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0, 
  19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,165,240,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2, 
  21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184, 
  12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1, 
  15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0, 
  19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,53,241,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0, 
  0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13, 
  0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15, 
  1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1, 
  20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0, 
  17,1,132,42,0,0,1,15,1,197,241,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0, 
  0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130, 
  111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0, 
  15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19, 
  20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1, 
  51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,85,242, 
  1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0, 
  0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7, 
  128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0, 
  0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19, 
  95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221, 
  0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,229,242,1,0,4,19,19,0,0,0,56,0,0, 
  0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184, 
  76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219, 
  42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1, 
  15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42, 
  0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,117,243,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128, 
  184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0, 
  0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1, 
  186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,5,244,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0, 
  4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0, 
  43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128, 
  135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48, 
  0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0, 
  4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,15,1,149,244,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1, 
  0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0, 
  0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15, 
  1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45, 
  0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1, 
  76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,15,1,37,245,1,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1, 
  0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32, 
  3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,0,211,1,0,1,1,15,1,0,211,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62, 
  0,0,0,1,0,17,1,0,211,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,0,211,1,0,1,15,1,0,211,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,20,4,35,0, 
  0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,5,0,0,0,30,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20, 
  0,35,0,0,0,0,0,0,0,1,0,0,0,44,0,0,0,22,0,0,0,15,1,207,211,1,0,17,1,181,245,1,0,1,2,19,44,0,0,0,153,0,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0, 
  0,123,0,0,0,22,0,0,0,5,17,1,240,245,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,44,0,0,0,152,0,0,0,1,0,1,20,4,30, 
  0,0,0,0,0,0,0,1,0,0,0,123,0,0,0,22,0,0,0,5,17,1,66,248,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,81,2,0, 
  0,5,89,2,0,17,0,0,0,4,0,0,0,64,32,204,131,53,224,6,128,66,232,12,128,83,192,16,128,52,24,6,131,5,128,18,127,6,128,18,129,39,176,130,129,56,88,10,128,73,48,14,128,54,64,8,128,43,0,4, 
  128,44,200,132,128,55,144,9,128,60,32,11,128,80,120,15,128,84,16,18,128,15,1,117,248,1,0,15,1,103,65,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66, 
  0,0,1,15,1,117,248,1,0,4,19,51,0,0,0,174,0,0,0,1,0,17,1,129,66,0,0,1,15,1,117,248,1,0,15,1,23,68,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0, 
  0,1,0,17,1,93,66,0,0,1,15,1,117,248,1,0,4,19,51,0,0,0,171,0,0,0,1,0,17,1,13,69,0,0,1,15,1,117,248,1,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15, 
  1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,117,248,1,0,15,1,106,71,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0, 
  1,15,1,117,248,1,0,4,19,51,0,0,0,172,0,0,0,1,0,17,1,96,72,0,0,1,15,1,117,248,1,0,4,19,51,0,0,0,173,0,0,0,1,0,17,1,246,73,0,0,1,15,1,117,248,1,0,15,1,163, 
  70,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,15,1,117,248,1,0,4,19,51,0,0,0,175,0,0,0,1,0,17,1,233,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0, 
  19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,117,248,1,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0, 
  0,1,0,17,1,117,248,1,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,117,248,1,0,1,15,1,117,248,1,0,15,1,127, 
  77,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,180,248,1,0,17,1,11,79,0,0,1,8,2,19,44,0,0,0,151,0,0,0,1,0, 
  1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,5,17,1,194,87,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,17,1,69,249,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35, 
  0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,0,0,0,0,1,0,0,0,3,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2, 
  20,0,35,0,0,0,0,0,0,0,1,0,0,0,80,0,0,0,22,0,0,0,15,1,144,215,1,0,17,1,127,249,1,0,1,2,19,80,0,0,0,250,0,0,0,1,0,1,19,80,0,0,0,249,0,0,0,1,0,1, 
  19,80,0,0,0,248,0,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,5,17,1,37,89,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0, 
  26,0,0,0,26,0,0,0,9,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,17,1,186,249,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0, 
  0,0,26,0,0,0,9,2,21,0,78,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,12,16,65,128,24,160,129,128,102,104,2,128,60,216,1,128,19,24,0,0,0,64,0,0,0,1,0,17,1,76,216,1,0,1, 
  17,1,18,216,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,76,216,1,0,1,1,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,103,0,0,0,60,1,0,0,3, 
  0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,35,0,0,0,22,0,0,0,15,1,218,216,1,0,17,1, 
  244,249,1,0,1,2,19,35,0,0,0,123,0,0,0,1,0,1,19,35,0,0,0,124,0,0,0,1,0,1,19,35,0,0,0,122,0,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0, 
  22,0,0,0,5,17,1,33,90,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0, 
  0,0,5,17,1,47,250,1,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,0,35,0,0,0,0,0,0,0,1,0,0,0,71,0,0,0,22,0,0,0, 
  15,1,150,217,1,0,17,1,105,250,1,0,1,2,19,73,0,0,0,231,0,0,0,3,0,1,21,1,143,0,0,0,5,112,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,48,195,130,106,176,2,128,123,112,3,128, 
  108,240,130,129,5,112,196,128,6,112,68,129,103,112,130,126,125,176,3,128,128,112,4,128,132,112,4,128,134,240,3,128,135,48,4,128,12,17,1,182,250,1,0,1,12,17,1,79,252,1,0,1,12,17,1,232,253,1,0,1, 
  12,17,1,129,255,1,0,1,12,17,1,26,1,2,0,1,12,17,1,179,2,2,0,1,12,17,1,76,4,2,0,1,6,17,1,229,5,2,0,1,12,17,1,213,6,2,0,1,10,12,19,95,0,0,0,32,1,0,0,1, 
  0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,59,0,0,0,22,0,0,0,5,17,1,110,8,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2, 
  20,4,30,0,0,0,0,0,0,0,1,0,0,0,44,0,0,0,22,0,0,0,5,17,1,173,10,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,19,55, 
  0,0,0,182,0,0,0,3,0,1,19,101,0,0,0,56,1,0,0,1,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,5,17,1,243,10,2,0,1,20,2,27,0,0,0,0,0, 
  0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,45,0,0,0,232,174,2,0,3,0,0,0,1,0,0,0,6,96,1,128,5,96,65,128,47,240,0,128,15,1,105,11,2,0,4,17,1,238, 
  218,1,0,1,8,19,101,0,0,0,57,1,0,0,1,0,1,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,24,56,131,129,23,208,194,129,10,208,1,128,91,192,4,128,12,216,65,128,60,200,3,128,72, 
  88,196,128,15,104,130,126,95,32,5,128,96,136,5,128,1,19,24,0,0,0,64,0,0,0,1,0,17,1,97,219,1,0,1,15,1,97,219,1,0,17,1,163,169,0,0,1,15,1,97,219,1,0,17,1,17,86,0,0,1, 
  19,23,0,0,0,62,0,0,0,1,0,17,1,97,219,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,97,219,1,0,1,15,1,97,219,1,0,17,1,81,86,0,0,1,19,10,0,0,0,39,0,0,0,3,0, 
  1,15,1,97,219,1,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,97,219,1,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,24,56,131,129,23,208,194,129,10, 
  208,1,128,91,192,4,128,12,216,65,128,60,200,3,128,72,88,196,128,15,104,130,126,95,32,5,128,96,136,5,128,1,19,24,0,0,0,64,0,0,0,1,0,17,1,37,220,1,0,1,15,1,37,220,1,0,17,1,175,188, 
  0,0,1,15,1,37,220,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,37,220,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,37,220,1,0,1,15,1,37,220,1,0,17,1, 
  81,86,0,0,1,19,10,0,0,0,39,0,0,0,3,0,1,15,1,37,220,1,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,37,220,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,233,220,1,0,1,1,15,1, 
  233,220,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,233,220,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,233,220,1,0,1,15,1,233,220,1,0,17,1,81,86,0,0,1, 
  19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15, 
  32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,121,221,1,0,1,1,15,1,121,221,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,121,221,1,0,1,19,24,0,0,0,65,0, 
  0,0,1,0,17,1,121,221,1,0,1,15,1,121,221,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23, 
  40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,9,222,1,0,1,1,15,1,9,222,1,0,17,1,17,86,0,0,1,19, 
  23,0,0,0,62,0,0,0,1,0,17,1,9,222,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,9,222,1,0,1,15,1,9,222,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1, 
  2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0, 
  1,0,17,1,153,222,1,0,1,1,15,1,153,222,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,153,222,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,153,222,1,0,1,15, 
  1,153,222,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12, 
  144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,41,223,1,0,1,1,15,1,41,223,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1, 
  41,223,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,41,223,1,0,1,15,1,41,223,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,185,223,1,0,1,1,15,1, 
  185,223,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,185,223,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,185,223,1,0,1,15,1,185,223,1,0,17,1,81,86,0,0,1, 
  19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15, 
  32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,73,224,1,0,1,1,15,1,73,224,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,73,224,1,0,1,19,24,0,0,0,65,0, 
  0,0,1,0,17,1,73,224,1,0,1,15,1,73,224,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23, 
  40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,217,224,1,0,1,1,15,1,217,224,1,0,17,1,17,86,0,0,1,19, 
  23,0,0,0,62,0,0,0,1,0,17,1,217,224,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,217,224,1,0,1,15,1,217,224,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1, 
  2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0, 
  1,0,17,1,105,225,1,0,1,1,15,1,105,225,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,105,225,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,105,225,1,0,1,15, 
  1,105,225,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12, 
  144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,249,225,1,0,1,1,15,1,249,225,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1, 
  249,225,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,249,225,1,0,1,15,1,249,225,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,195,0,0,0,0,0,0,0,10, 
  0,0,0,3,0,0,0,24,56,131,129,23,208,194,129,10,208,1,128,91,192,4,128,12,216,65,128,60,200,3,128,72,88,196,128,15,104,130,126,95,32,5,128,96,136,5,128,1,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,137,226,1,0,1,15,1,137,226,1,0,17,1,135,189,0,0,1,15,1,137,226,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,137,226,1,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,137,226,1,0,1,15,1,137,226,1,0,17,1,81,86,0,0,1,19,10,0,0,0,39,0,0,0,3,0,1,15,1,137,226,1,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,137, 
  226,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0, 
  64,0,0,0,1,0,17,1,77,227,1,0,1,1,15,1,77,227,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,77,227,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,77,227, 
  1,0,1,15,1,77,227,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72, 
  176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,221,227,1,0,1,1,15,1,221,227,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0, 
  1,0,17,1,221,227,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,221,227,1,0,1,15,1,221,227,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0, 
  0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,109,228,1,0, 
  1,1,15,1,109,228,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,109,228,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,109,228,1,0,1,15,1,109,228,1,0,17,1,81, 
  86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96, 
  24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,253,228,1,0,1,1,15,1,253,228,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,253,228,1,0,1,19,24,0, 
  0,0,65,0,0,0,1,0,17,1,253,228,1,0,1,15,1,253,228,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24, 
  144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,141,229,1,0,1,1,15,1,141,229,1,0,17,1,17,86, 
  0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,141,229,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,141,229,1,0,1,15,1,141,229,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0, 
  0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0, 
  64,0,0,0,1,0,17,1,29,230,1,0,1,1,15,1,29,230,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,29,230,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,29,230, 
  1,0,1,15,1,29,230,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72, 
  176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,173,230,1,0,1,1,15,1,173,230,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0, 
  1,0,17,1,173,230,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,173,230,1,0,1,15,1,173,230,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0, 
  0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,61,231,1,0, 
  1,1,15,1,61,231,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,61,231,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,61,231,1,0,1,15,1,61,231,1,0,17,1,81, 
  86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96, 
  24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,205,231,1,0,1,1,15,1,205,231,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,205,231,1,0,1,19,24,0, 
  0,0,65,0,0,0,1,0,17,1,205,231,1,0,1,15,1,205,231,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24, 
  144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,93,232,1,0,1,1,15,1,93,232,1,0,17,1,17,86, 
  0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,93,232,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,93,232,1,0,1,15,1,93,232,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0, 
  0,2,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,24,56,131,129,23,208,194,129,10,208,1,128,91,192,4,128,12,216,65,128,60,200,3,128,72,88,196,128,15,104,130,126,95,32,5,128,96, 
  136,5,128,1,19,24,0,0,0,64,0,0,0,1,0,17,1,237,232,1,0,1,15,1,237,232,1,0,17,1,95,190,0,0,1,15,1,237,232,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17, 
  1,237,232,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,237,232,1,0,1,15,1,237,232,1,0,17,1,81,86,0,0,1,19,10,0,0,0,39,0,0,0,3,0,1,15,1,237,232,1,0,17,1,149,86,0, 
  0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,237,232,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95, 
  32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,177,233,1,0,1,1,15,1,177,233,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,177,233,1,0, 
  1,19,24,0,0,0,65,0,0,0,1,0,17,1,177,233,1,0,1,15,1,177,233,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3, 
  0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,65,234,1,0,1,1,15,1,65,234,1,0, 
  17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,65,234,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,65,234,1,0,1,15,1,65,234,1,0,17,1,81,86,0,0,1,19,15,0,0, 
  0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19, 
  24,0,0,0,64,0,0,0,1,0,17,1,209,234,1,0,1,1,15,1,209,234,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,209,234,1,0,1,19,24,0,0,0,65,0,0,0,1,0, 
  17,1,209,234,1,0,1,15,1,209,234,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60, 
  32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,97,235,1,0,1,1,15,1,97,235,1,0,17,1,17,86,0,0,1,19,23,0,0,0, 
  62,0,0,0,1,0,17,1,97,235,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,97,235,1,0,1,15,1,97,235,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143, 
  0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1, 
  241,235,1,0,1,1,15,1,241,235,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,241,235,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,241,235,1,0,1,15,1,241,235,1, 
  0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95, 
  32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,129,236,1,0,1,1,15,1,129,236,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,129,236,1,0, 
  1,19,24,0,0,0,65,0,0,0,1,0,17,1,129,236,1,0,1,15,1,129,236,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3, 
  0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,17,237,1,0,1,1,15,1,17,237,1,0, 
  17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,17,237,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,17,237,1,0,1,15,1,17,237,1,0,17,1,81,86,0,0,1,19,15,0,0, 
  0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19, 
  24,0,0,0,64,0,0,0,1,0,17,1,161,237,1,0,1,1,15,1,161,237,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,161,237,1,0,1,19,24,0,0,0,65,0,0,0,1,0, 
  17,1,161,237,1,0,1,15,1,161,237,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60, 
  32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,49,238,1,0,1,1,15,1,49,238,1,0,17,1,17,86,0,0,1,19,23,0,0,0, 
  62,0,0,0,1,0,17,1,49,238,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,49,238,1,0,1,15,1,49,238,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143, 
  0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1, 
  193,238,1,0,1,1,15,1,193,238,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,193,238,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,193,238,1,0,1,15,1,193,238,1, 
  0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,195,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,24,56,131,129,23,208,194,129,10,208,1,128,91,192,4,128,12,216,65,128,60, 
  200,3,128,72,88,196,128,15,104,130,126,95,32,5,128,96,136,5,128,1,19,24,0,0,0,64,0,0,0,1,0,17,1,81,239,1,0,1,15,1,81,239,1,0,17,1,55,191,0,0,1,15,1,81,239,1,0,17,1,17, 
  86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,81,239,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,81,239,1,0,1,15,1,81,239,1,0,17,1,81,86,0,0,1,19,10,0,0,0,39,0, 
  0,0,3,0,1,15,1,81,239,1,0,17,1,149,86,0,0,1,19,15,0,0,0,49,0,0,0,1,0,17,1,81,239,1,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23, 
  40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,21,240,1,0,1,1,15,1,21,240,1,0,17,1,17,86,0,0,1,19, 
  23,0,0,0,62,0,0,0,1,0,17,1,21,240,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,21,240,1,0,1,15,1,21,240,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1, 
  2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0, 
  1,0,17,1,165,240,1,0,1,1,15,1,165,240,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,165,240,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,165,240,1,0,1,15, 
  1,165,240,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12, 
  144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,53,241,1,0,1,1,15,1,53,241,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1, 
  53,241,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,53,241,1,0,1,15,1,53,241,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,197,241,1,0,1,1,15,1, 
  197,241,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,197,241,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,197,241,1,0,1,15,1,197,241,1,0,17,1,81,86,0,0,1, 
  19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15, 
  32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,85,242,1,0,1,1,15,1,85,242,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,85,242,1,0,1,19,24,0,0,0,65,0, 
  0,0,1,0,17,1,85,242,1,0,1,15,1,85,242,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23, 
  40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,229,242,1,0,1,1,15,1,229,242,1,0,17,1,17,86,0,0,1,19, 
  23,0,0,0,62,0,0,0,1,0,17,1,229,242,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,229,242,1,0,1,15,1,229,242,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1, 
  2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0, 
  1,0,17,1,117,243,1,0,1,1,15,1,117,243,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,117,243,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,117,243,1,0,1,15, 
  1,117,243,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12, 
  144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,5,244,1,0,1,1,15,1,5,244,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1, 
  5,244,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,5,244,1,0,1,15,1,5,244,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8, 
  0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,149,244,1,0,1,1,15,1, 
  149,244,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,149,244,1,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,149,244,1,0,1,15,1,149,244,1,0,17,1,81,86,0,0,1, 
  19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15, 
  32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,37,245,1,0,1,1,15,1,37,245,1,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,37,245,1,0,1,19,24,0,0,0,65,0, 
  0,0,1,0,17,1,37,245,1,0,1,15,1,37,245,1,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,51,0,0,0,19,179,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5, 
  144,193,128,6,144,65,128,62,80,1,128,89,80,1,128,108,80,1,128,12,17,1,117,11,2,0,1,10,12,17,1,76,36,0,0,1,21,1,81,2,0,0,5,89,2,0,17,0,0,0,4,0,0,0,64,32,204,131,53,224, 
  6,128,66,232,12,128,83,192,16,128,52,24,6,131,5,128,18,127,6,128,18,129,39,176,130,129,56,88,10,128,73,48,14,128,54,64,8,128,43,0,4,128,44,200,132,128,55,144,9,128,60,32,11,128,80,120,15,128,84,16, 
  18,128,15,1,217,11,2,0,15,1,103,65,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,217,11,2,0,4,19,51,0,0,0,174,0,0,0, 
  1,0,17,1,129,66,0,0,1,15,1,217,11,2,0,15,1,23,68,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,217,11,2,0,4,19,51, 
  0,0,0,171,0,0,0,1,0,17,1,13,69,0,0,1,15,1,217,11,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1, 
  15,1,217,11,2,0,15,1,106,71,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,217,11,2,0,4,19,51,0,0,0,172,0,0,0,1,0, 
  17,1,96,72,0,0,1,15,1,217,11,2,0,4,19,51,0,0,0,173,0,0,0,1,0,17,1,246,73,0,0,1,15,1,217,11,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152, 
  75,0,0,1,15,1,217,11,2,0,4,19,51,0,0,0,175,0,0,0,1,0,17,1,233,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0, 
  0,1,0,17,1,217,11,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,217,11,2,0,1,4,19,39,0,0,0,140,0,0, 
  0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,217,11,2,0,1,15,1,217,11,2,0,15,1,127,77,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0, 
  0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,24,12,2,0,17,1,11,79,0,0,1,8,2,21,1,50,0,0,0,117,179,2,0,3,0,0,0,1,0,0,0,6,136,1,128,5,136,65,128,135,240,0,128, 
  4,19,84,0,0,0,0,1,0,0,1,0,17,1,169,12,2,0,1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,49,0,0,0,168,0,0,0,3,0,1,20,2,27,0, 
  0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,21,144,2,129,17,112,193,127,18,0,194,128,27,32,3,128,49,176, 
  131,128,58,184,3,128,61,72,4,128,19,18,0,0,0,53,0,0,0,1,0,17,1,180,248,1,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,180,248,1,0,1,19,61,0,0,0,195,0,0,0,1,0,17,1,180, 
  248,1,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,180,248,1,0,1,1,19,18,0,0,0,54,0,0,0,1,0,17,1,180,248,1,0,1,17,1,117,248,1,0,1,2,20,4,30,0,0,0,0,0,0,0,1, 
  0,0,0,123,0,0,0,22,0,0,0,5,17,1,232,12,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,51,0,0,0,19,179,2,0,6,0,0, 
  0,2,0,0,0,48,80,65,129,5,144,193,128,6,144,65,128,62,80,1,128,89,80,1,128,108,80,1,128,12,17,1,58,15,2,0,1,10,12,17,1,76,36,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0, 
  123,0,0,0,22,0,0,0,5,17,1,158,15,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,51,0,0,0,19,179,2,0,6,0,0,0,2,0, 
  0,0,48,80,65,129,5,144,193,128,6,144,65,128,62,80,1,128,89,80,1,128,108,80,1,128,12,17,1,240,17,2,0,1,10,12,17,1,76,36,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,123,0,0, 
  0,22,0,0,0,5,17,1,84,18,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,124, 
  16,1,128,41,208,0,128,12,17,1,76,36,0,0,1,12,17,1,166,20,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,11,12,17,1,76,36,0,0,1,21,1, 
  152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128, 
  134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0, 
  15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1, 
  72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23, 
  0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,224,20,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0, 
  1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0, 
  0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4, 
  17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26, 
  0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,15,1,112,21,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141, 
  0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80, 
  198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0, 
  0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,0,22,2,0, 
  4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115, 
  0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108, 
  32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15, 
  1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0, 
  0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0, 
  0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,144,22,2,0,4,19,19,0,0,0,56,0,0,0,1, 
  0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63, 
  0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129, 
  103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1, 
  60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0, 
  15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,32,23,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0, 
  1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12, 
  128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15, 
  1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48, 
  0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0, 
  0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,176,23,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19, 
  84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134, 
  2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184, 
  10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0, 
  4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17, 
  1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0, 
  0,1,0,17,1,132,42,0,0,1,15,1,64,24,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19, 
  40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,232,0,0,0,223,134,2,0,20,0,0,0,4,0,0,0,128, 
  56,7,128,113,120,6,128,102,80,4,128,99,16,4,128,132,56,7,128,5,56,7,128,6,56,7,127,87,80,67,130,88,144,67,130,89,208,131,130,106,16,5,128,123,248,6,128,108,80,5,128,29,16,67,129,110,248,5,128,111, 
  56,6,128,103,144,4,128,104,208,4,128,109,144,5,128,121,184,6,128,12,17,1,208,24,2,0,1,12,17,1,105,26,2,0,1,12,17,1,182,250,1,0,1,12,17,1,2,28,2,0,1,12,17,1,155,29,2,0,1,12, 
  17,1,52,31,2,0,1,12,17,1,79,252,1,0,1,12,17,1,205,32,2,0,1,12,17,1,232,253,1,0,1,12,17,1,129,255,1,0,1,12,19,95,0,0,0,32,1,0,0,1,0,1,12,17,1,102,34,2,0,1, 
  12,17,1,26,1,2,0,1,12,17,1,255,35,2,0,1,12,17,1,152,37,2,0,1,12,17,1,179,2,2,0,1,10,12,17,1,49,39,2,0,1,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88, 
  48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0, 
  1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26, 
  0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15, 
  1,202,40,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19, 
  34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,62,2,0,0,144,68,2,0,23,0,0,0,4,0,0,0,16,112,3,128,38,176,196,131,50,72,8,133, 
  58,232,72,132,36,16,4,128,5,232,17,130,6,232,209,126,71,128,12,128,40,80,69,130,41,240,5,128,42,8,71,126,59,136,201,129,76,96,14,128,69,64,11,128,46,168,7,128,63,160,10,129,70,224,11,128,72,32,13,128, 
  75,192,205,128,79,0,15,128,106,160,15,128,123,56,16,128,130,208,16,128,15,1,90,41,2,0,15,1,1,6,0,0,4,17,1,13,6,0,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,100,6,0,0,1, 
  15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,175,6,0,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,250,6,0,0,1,4,19,93,0,0,0,26,1,0,0,1,0,19,29,0,0,0,75,0,0, 
  0,1,0,19,36,0,0,0,125,0,0,0,3,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,69,7,0,0,1,15,1,90,41,2,0,15,1,144,7,0,0,4,17,1,156,7,0,0,1,15,1,90,41,2, 
  0,15,1,88,6,0,0,4,17,1,231,7,0,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,50,8,0,0,1,4,19,93,0,0,0,27,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,36, 
  0,0,0,125,0,0,0,3,0,1,15,1,90,41,2,0,15,1,125,8,0,0,4,17,1,137,8,0,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,212,8,0,0,1,15,1,90,41,2,0,15,1,31,9, 
  0,0,4,17,1,43,9,0,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,101,9,0,0,1,15,1,90,41,2,0,15,1,88,6,0,0,4,17,1,176,9,0,0,1,15,1,90,41,2,0,15,1,88,6, 
  0,0,4,17,1,251,9,0,0,1,15,1,90,41,2,0,15,1,1,6,0,0,4,17,1,70,10,0,0,1,15,1,90,41,2,0,15,1,1,6,0,0,4,17,1,145,10,0,0,1,15,1,90,41,2,0,15,1,220,10, 
  0,0,17,1,140,35,0,0,1,15,1,90,41,2,0,15,1,88,11,0,0,17,1,216,35,0,0,1,4,19,93,0,0,0,28,1,0,0,1,0,19,29,0,0,0,75,0,0,0,1,0,19,36,0,0,0,125,0,0,0, 
  3,0,1,8,2,20,4,42,0,0,0,0,0,0,0,1,0,0,0,36,0,0,0,22,0,0,0,15,1,102,41,2,0,15,1,153,220,0,0,5,17,1,250,23,0,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0, 
  0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,90,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,114,128,1,128,97,16,1,128,98,96,130,127,103,240,1,128,15,1,114,41,2,0,5,17,1,214,41, 
  2,0,1,15,1,3,42,2,0,5,17,1,103,42,2,0,1,15,1,148,42,2,0,5,17,1,248,42,2,0,1,15,1,37,43,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5, 
  0,0,0,26,0,0,0,26,0,0,0,9,2,19,101,0,0,0,55,1,0,0,2,0,1,21,1,99,0,0,0,120,162,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5,16,195,128,6,16,67,128,62,192,1,128,89, 
  48,2,128,108,160,2,128,15,1,182,43,2,0,4,17,1,255,211,1,0,1,15,1,194,43,2,0,4,17,1,69,212,1,0,1,15,1,182,43,2,0,4,17,1,127,212,1,0,1,15,1,206,43,2,0,4,17,1,194,87, 
  1,0,1,8,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,49,0,0,0,167,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0, 
  0,0,26,0,0,0,9,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,21,144,2,129,17,112,193,127,18,0,194,128,27,32,3,128,49,176,131,128,58,184,3,128,61,72,4,128,19,18,0,0,0,53, 
  0,0,0,1,0,17,1,24,12,2,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,24,12,2,0,1,19,61,0,0,0,195,0,0,0,1,0,17,1,24,12,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17, 
  1,24,12,2,0,1,1,19,18,0,0,0,54,0,0,0,1,0,17,1,24,12,2,0,1,17,1,217,11,2,0,1,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0,5,19,63,0,0, 
  0,203,0,0,0,4,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,81,2,0,0,5,89,2,0,17,0,0,0,4,0,0,0,64,32,204,131,53,224, 
  6,128,66,232,12,128,83,192,16,128,52,24,6,131,5,128,18,127,6,128,18,129,39,176,130,129,56,88,10,128,73,48,14,128,54,64,8,128,43,0,4,128,44,200,132,128,55,144,9,128,60,32,11,128,80,120,15,128,84,16, 
  18,128,15,1,218,43,2,0,15,1,103,65,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,218,43,2,0,4,19,51,0,0,0,174,0,0,0, 
  1,0,17,1,129,66,0,0,1,15,1,218,43,2,0,15,1,23,68,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,218,43,2,0,4,19,51, 
  0,0,0,171,0,0,0,1,0,17,1,13,69,0,0,1,15,1,218,43,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1, 
  15,1,218,43,2,0,15,1,106,71,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,218,43,2,0,4,19,51,0,0,0,172,0,0,0,1,0, 
  17,1,96,72,0,0,1,15,1,218,43,2,0,4,19,51,0,0,0,173,0,0,0,1,0,17,1,246,73,0,0,1,15,1,218,43,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152, 
  75,0,0,1,15,1,218,43,2,0,4,19,51,0,0,0,175,0,0,0,1,0,17,1,233,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0, 
  0,1,0,17,1,218,43,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,218,43,2,0,1,4,19,39,0,0,0,140,0,0, 
  0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,218,43,2,0,1,15,1,218,43,2,0,15,1,127,77,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0, 
  0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,25,44,2,0,17,1,11,79,0,0,1,8,2,21,1,99,0,0,0,120,162,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5,16,195,128,6,16,67,128, 
  62,192,1,128,89,48,2,128,108,160,2,128,15,1,170,44,2,0,4,17,1,255,211,1,0,1,15,1,182,44,2,0,4,17,1,69,212,1,0,1,15,1,170,44,2,0,4,17,1,127,212,1,0,1,15,1,194,44,2,0, 
  4,17,1,37,89,1,0,1,8,2,21,1,81,2,0,0,5,89,2,0,17,0,0,0,4,0,0,0,64,32,204,131,53,224,6,128,66,232,12,128,83,192,16,128,52,24,6,131,5,128,18,127,6,128,18,129,39,176,130,129, 
  56,88,10,128,73,48,14,128,54,64,8,128,43,0,4,128,44,200,132,128,55,144,9,128,60,32,11,128,80,120,15,128,84,16,18,128,15,1,206,44,2,0,15,1,103,65,0,0,4,19,45,0,0,0,155,0,0,0,1,0, 
  19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,206,44,2,0,4,19,51,0,0,0,174,0,0,0,1,0,17,1,129,66,0,0,1,15,1,206,44,2,0,15,1,23,68,0,0,4,19,45,0,0, 
  0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,206,44,2,0,4,19,51,0,0,0,171,0,0,0,1,0,17,1,13,69,0,0,1,15,1,206,44,2,0,15,1,163,70, 
  0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0,4,17,1,25,71,0,0,1,15,1,206,44,2,0,15,1,106,71,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85, 
  0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,206,44,2,0,4,19,51,0,0,0,172,0,0,0,1,0,17,1,96,72,0,0,1,15,1,206,44,2,0,4,19,51,0,0,0,173,0,0,0,1,0,17, 
  1,246,73,0,0,1,15,1,206,44,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,140,75,0,0,4,17,1,152,75,0,0,1,15,1,206,44,2,0,4,19,51,0,0,0,175,0,0,0,1,0,17,1,233,75, 
  0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,206,44,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0, 
  0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,206,44,2,0,1,4,19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17, 
  1,206,44,2,0,1,15,1,206,44,2,0,15,1,127,77,0,0,4,19,45,0,0,0,156,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,13,45,2,0,17,1,11,79,0, 
  0,1,8,2,21,1,99,0,0,0,120,162,2,0,6,0,0,0,2,0,0,0,48,80,65,129,5,16,195,128,6,16,67,128,62,192,1,128,89,48,2,128,108,160,2,128,15,1,158,45,2,0,4,17,1,255,211,1,0,1, 
  15,1,170,45,2,0,4,17,1,69,212,1,0,1,15,1,158,45,2,0,4,17,1,127,212,1,0,1,15,1,182,45,2,0,4,17,1,33,90,1,0,1,8,2,21,1,81,2,0,0,5,89,2,0,17,0,0,0,4,0, 
  0,0,64,32,204,131,53,224,6,128,66,232,12,128,83,192,16,128,52,24,6,131,5,128,18,127,6,128,18,129,39,176,130,129,56,88,10,128,73,48,14,128,54,64,8,128,43,0,4,128,44,200,132,128,55,144,9,128,60,32, 
  11,128,80,120,15,128,84,16,18,128,15,1,194,45,2,0,15,1,103,65,0,0,4,19,45,0,0,0,155,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,194,45,2,0,4,19, 
  51,0,0,0,174,0,0,0,1,0,17,1,129,66,0,0,1,15,1,194,45,2,0,15,1,23,68,0,0,4,19,45,0,0,0,154,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15, 
  1,194,45,2,0,4,19,51,0,0,0,171,0,0,0,1,0,17,1,13,69,0,0,1,15,1,194,45,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1,187,70,0,0,15,1,233,70,0,0,15,1,13,71,0,0, 
  4,17,1,25,71,0,0,1,15,1,194,45,2,0,15,1,106,71,0,0,4,19,45,0,0,0,157,0,0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,15,1,194,45,2,0,4,19,51,0, 
  0,0,172,0,0,0,1,0,17,1,96,72,0,0,1,15,1,194,45,2,0,4,19,51,0,0,0,173,0,0,0,1,0,17,1,246,73,0,0,1,15,1,194,45,2,0,15,1,163,70,0,0,15,1,175,70,0,0,15,1, 
  140,75,0,0,4,17,1,152,75,0,0,1,15,1,194,45,2,0,4,19,51,0,0,0,175,0,0,0,1,0,17,1,233,75,0,0,1,4,19,39,0,0,0,138,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0, 
  19,61,0,0,0,195,0,0,0,1,0,17,1,194,45,2,0,1,4,19,39,0,0,0,139,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,194,45,2,0,1,4, 
  19,39,0,0,0,140,0,0,0,1,0,19,21,0,0,0,60,0,0,0,1,0,19,61,0,0,0,195,0,0,0,1,0,17,1,194,45,2,0,1,15,1,194,45,2,0,15,1,127,77,0,0,4,19,45,0,0,0,156,0, 
  0,0,1,0,19,85,0,0,0,2,1,0,0,1,0,17,1,93,66,0,0,1,4,15,1,1,46,2,0,17,1,11,79,0,0,1,8,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,124,0,0,0,22,0,0,0, 
  5,17,1,146,46,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40, 
  2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,224,20,2,0,1,1,15,1,224,20,2,0,17,1,17,86,0,0,1,19,23, 
  0,0,0,62,0,0,0,1,0,17,1,224,20,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,224,20,2,0,1,15,1,224,20,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2, 
  21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1, 
  0,17,1,112,21,2,0,1,1,15,1,112,21,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,112,21,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,112,21,2,0,1,15,1, 
  112,21,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144, 
  129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,0,22,2,0,1,1,15,1,0,22,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,0, 
  22,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,0,22,2,0,1,15,1,0,22,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0, 
  0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,144,22,2,0,1,1,15,1,144, 
  22,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,144,22,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,144,22,2,0,1,15,1,144,22,2,0,17,1,81,86,0,0,1,19, 
  15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32, 
  130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,32,23,2,0,1,1,15,1,32,23,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,32,23,2,0,1,19,24,0,0,0,65,0,0, 
  0,1,0,17,1,32,23,2,0,1,15,1,32,23,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40, 
  2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,176,23,2,0,1,1,15,1,176,23,2,0,17,1,17,86,0,0,1,19,23, 
  0,0,0,62,0,0,0,1,0,17,1,176,23,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,176,23,2,0,1,15,1,176,23,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2, 
  21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1, 
  0,17,1,64,24,2,0,1,1,15,1,64,24,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,64,24,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,64,24,2,0,1,15,1, 
  64,24,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32, 
  133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1, 
  207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0, 
  0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0, 
  1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,202,46,2,0,4,19,19,0,0,0,56,0,0,0,1,0, 
  17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0, 
  0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103, 
  96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60, 
  198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15, 
  1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0, 
  1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,90,47,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198, 
  0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1, 
  8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128, 
  132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1, 
  132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0, 
  0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0, 
  1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,234,47,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84, 
  0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2, 
  0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10, 
  128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195, 
  42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4, 
  15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1, 
  106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0, 
  1,0,17,1,132,42,0,0,1,15,1,122,48,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40, 
  0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48, 
  66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42, 
  0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1, 
  4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0, 
  15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1, 
  10,49,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34, 
  0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123, 
  128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195, 
  42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1, 
  0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0, 
  0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,154,49,2,0,4,19,19,0,0,0,56, 
  0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23, 
  0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128, 
  6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17, 
  1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2, 
  0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0, 
  0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,42,50,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0, 
  1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1, 
  132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7, 
  128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51, 
  42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4, 
  17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0, 
  0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,186,50,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42, 
  0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1, 
  0,0,43,134,2,0,13,0,0,0,3,0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240, 
  9,128,135,184,10,128,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42, 
  0,0,15,1,195,42,0,0,4,17,1,213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1, 
  162,48,0,0,4,15,1,20,26,0,0,17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198, 
  0,0,4,17,1,106,49,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0, 
  0,63,0,0,0,1,0,17,1,132,42,0,0,1,15,1,74,51,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0, 
  0,1,0,19,40,0,0,0,141,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,1,152,1,0,0,43,134,2,0,13,0,0,0,3, 
  0,0,0,88,48,66,130,111,80,198,130,106,96,4,128,123,128,7,128,108,32,133,129,5,184,204,128,6,184,76,129,103,96,131,126,125,240,7,128,128,184,12,128,132,184,12,128,134,240,9,128,135,184,10,128,15,1,60,198,0, 
  0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,207,42,0,0,4,17,1,219,42,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,4,17,1, 
  213,45,0,0,1,4,19,20,0,0,0,57,0,0,0,1,0,19,95,0,0,0,31,1,0,0,2,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,162,48,0,0,4,15,1,20,26,0,0, 
  17,1,76,26,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,15,1,132,42,0,0,15,1,195,42,0,0,15,1,174,48,0,0,4,17,1,186,48,0,0,1,15,1,72,198,0,0,4,17,1,106,49,0,0,1,15, 
  1,60,198,0,0,15,1,51,42,0,0,4,19,70,0,0,0,221,0,0,0,1,0,19,40,0,0,0,142,0,0,0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42, 
  0,0,1,15,1,218,51,2,0,4,19,19,0,0,0,56,0,0,0,1,0,17,1,104,29,0,0,1,15,1,60,198,0,0,15,1,51,42,0,0,4,19,84,0,0,0,0,1,0,0,1,0,19,40,0,0,0,141,0,0, 
  0,1,0,19,34,0,0,0,115,0,0,0,1,0,19,23,0,0,0,63,0,0,0,1,0,17,1,132,42,0,0,1,8,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,202,40,2,0,1,1,15,1,202,40,2,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,202,40,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,202,40,2,0,1,15,1,202,40,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,19,36, 
  0,0,0,125,0,0,0,3,0,1,19,68,0,0,0,218,0,0,0,3,0,1,21,4,72,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,98,208,129,128,103,96,1,128,114,240,0,128,15,1,106,52,2,0,5,17, 
  1,103,42,2,0,1,15,1,188,52,2,0,5,17,1,248,42,2,0,1,15,1,14,53,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0, 
  9,2,21,1,44,0,0,0,92,80,2,0,3,0,0,0,1,0,0,0,6,88,129,128,5,88,1,128,130,240,0,128,4,19,16,0,0,0,51,0,0,0,2,0,1,8,2,21,4,72,0,0,0,0,0,0,0,3,0,0, 
  0,1,0,0,0,98,208,1,128,97,240,64,128,103,96,1,128,15,1,96,53,2,0,5,17,1,214,41,2,0,1,15,1,178,53,2,0,5,17,1,248,42,2,0,1,15,1,4,54,2,0,5,17,1,137,43,2,0,1,20, 
  2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,44,0,0,0,92,80,2,0,3,0,0,0,1,0,0,0,6,88,129,128,5,88,1,128,130,240,0,128,4,19,3, 
  0,0,0,23,0,0,0,2,0,1,8,2,21,4,72,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,98,208,129,128,97,240,0,128,114,96,1,128,15,1,86,54,2,0,5,17,1,214,41,2,0,1,15,1,168,54, 
  2,0,5,17,1,103,42,2,0,1,15,1,250,54,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,44,0,0,0,92,80, 
  2,0,3,0,0,0,1,0,0,0,6,88,129,128,5,88,1,128,130,240,0,128,4,19,87,0,0,0,4,1,0,0,2,0,1,8,2,21,4,72,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,96,1,128,97, 
  240,64,128,103,208,1,128,15,1,76,55,2,0,5,17,1,214,41,2,0,1,15,1,158,55,2,0,5,17,1,103,42,2,0,1,15,1,240,55,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2, 
  0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,1,44,0,0,0,92,80,2,0,3,0,0,0,1,0,0,0,6,88,129,128,5,88,1,128,130,240,0,128,4,19,89,0,0,0,7,1,0,0,2,0,1, 
  8,2,19,44,0,0,0,150,0,0,0,2,0,1,19,44,0,0,0,149,0,0,0,2,0,1,19,44,0,0,0,148,0,0,0,2,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0,22,0,0,0, 
  5,19,26,0,0,0,69,0,0,0,6,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,21, 
  144,130,129,17,112,193,127,18,0,130,128,27,40,3,128,26,32,67,128,58,184,3,128,61,72,4,128,19,18,0,0,0,53,0,0,0,1,0,17,1,25,44,2,0,1,19,21,0,0,0,58,0,0,0,1,0,17,1,25,44, 
  2,0,1,19,61,0,0,0,195,0,0,0,1,0,17,1,25,44,2,0,1,1,19,18,0,0,0,55,0,0,0,1,0,17,1,25,44,2,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,25,44,2,0,1,17,1, 
  218,43,2,0,1,2,19,80,0,0,0,247,0,0,0,2,0,1,19,80,0,0,0,246,0,0,0,2,0,1,19,80,0,0,0,245,0,0,0,2,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0,125,0,0,0, 
  22,0,0,0,5,19,102,0,0,0,58,1,0,0,6,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,144,0,0,0,0,0,0,0,7,0,0,0,2, 
  0,0,0,21,144,66,129,17,112,193,127,18,0,130,128,27,32,3,128,58,176,131,128,61,64,4,128,102,120,4,128,19,18,0,0,0,53,0,0,0,1,0,17,1,13,45,2,0,1,19,21,0,0,0,58,0,0,0,1,0, 
  17,1,13,45,2,0,1,19,61,0,0,0,195,0,0,0,1,0,17,1,13,45,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,13,45,2,0,1,19,18,0,0,0,54,0,0,0,1,0,17,1,13,45,2,0, 
  1,17,1,206,44,2,0,1,1,2,19,35,0,0,0,120,0,0,0,2,0,1,19,35,0,0,0,121,0,0,0,2,0,1,19,35,0,0,0,119,0,0,0,2,0,1,20,4,35,0,0,0,0,0,0,0,1,0,0,0, 
  125,0,0,0,22,0,0,0,5,19,42,0,0,0,145,0,0,0,6,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,0,144,0,0,0,0,0,0,0,7, 
  0,0,0,2,0,0,0,21,144,130,129,17,112,193,127,18,0,130,128,27,32,3,128,42,176,67,128,58,184,3,128,61,72,4,128,19,18,0,0,0,53,0,0,0,1,0,17,1,1,46,2,0,1,19,21,0,0,0,58,0, 
  0,0,1,0,17,1,1,46,2,0,1,19,61,0,0,0,195,0,0,0,1,0,17,1,1,46,2,0,1,19,18,0,0,0,55,0,0,0,1,0,17,1,1,46,2,0,1,1,19,18,0,0,0,54,0,0,0,1,0,17, 
  1,1,46,2,0,1,17,1,194,45,2,0,1,2,20,2,55,0,0,0,0,0,0,0,3,0,0,0,4,0,0,0,30,0,0,0,54,0,0,0,54,0,0,0,5,19,98,0,0,0,51,1,0,0,1,0,19,71,0,0, 
  0,222,0,0,0,3,0,1,9,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,202,46,2,0,1,1,15,1,202,46,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,202,46,2,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,202,46,2,0,1,15,1,202,46,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,90,47,2,0,1,1,15,1,90,47,2,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,90,47,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,90,47,2,0,1,15,1,90,47,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,234,47,2,0,1,1,15,1,234,47,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,234,47,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,234,47,2,0,1,15,1,234,47, 
  2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127, 
  95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,122,48,2,0,1,1,15,1,122,48,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,122,48,2, 
  0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,122,48,2,0,1,15,1,122,48,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,10,49,2,0,1,1,15,1,10,49,2, 
  0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,10,49,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,10,49,2,0,1,15,1,10,49,2,0,17,1,81,86,0,0,1,19,15,0, 
  0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126, 
  19,24,0,0,0,64,0,0,0,1,0,17,1,154,49,2,0,1,1,15,1,154,49,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,154,49,2,0,1,19,24,0,0,0,65,0,0,0,1, 
  0,17,1,154,49,2,0,1,15,1,154,49,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129, 
  60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,42,50,2,0,1,1,15,1,42,50,2,0,17,1,17,86,0,0,1,19,23,0,0, 
  0,62,0,0,0,1,0,17,1,42,50,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,42,50,2,0,1,15,1,42,50,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0, 
  143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17, 
  1,186,50,2,0,1,1,15,1,186,50,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,186,50,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,186,50,2,0,1,15,1,186,50, 
  2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127, 
  95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,74,51,2,0,1,1,15,1,74,51,2,0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,74,51,2, 
  0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,74,51,2,0,1,15,1,74,51,2,0,17,1,81,86,0,0,1,19,15,0,0,0,47,0,0,0,2,0,1,2,21,0,143,0,0,0,0,0,0,0,8,0,0,0, 
  3,0,0,0,24,144,194,128,23,40,2,129,60,32,3,128,72,176,195,128,12,144,129,127,95,32,2,128,96,24,4,128,15,32,130,126,19,24,0,0,0,64,0,0,0,1,0,17,1,218,51,2,0,1,1,15,1,218,51,2, 
  0,17,1,17,86,0,0,1,19,23,0,0,0,62,0,0,0,1,0,17,1,218,51,2,0,1,19,24,0,0,0,65,0,0,0,1,0,17,1,218,51,2,0,1,15,1,218,51,2,0,17,1,81,86,0,0,1,19,15,0, 
  0,0,47,0,0,0,2,0,1,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,98,64,1,128,103,208,0,128,15,1,66,56,2,0,5,17,1,248,42,2,0,1,15,1,130,56,2,0,5,17,1,137, 
  43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,98,64,65,128,114,208,0,128,15,1, 
  194,56,2,0,5,17,1,103,42,2,0,1,15,1,2,57,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0, 
  0,0,0,0,2,0,0,0,1,0,0,0,114,208,0,128,103,64,1,128,15,1,66,57,2,0,5,17,1,103,42,2,0,1,15,1,130,57,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0, 
  0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,98,64,1,128,103,208,0,128,15,1,194,57,2,0,5,17,1,248,42,2,0,1,15,1,2,58, 
  2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,54,0,0,0,0,0,0,0,2,0,0,0,97,0,0,0,26,0,0,0, 
  40,0,0,0,15,1,66,58,2,0,5,17,1,214,41,2,0,1,15,1,130,58,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2, 
  21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,103,64,1,128,97,208,192,127,15,1,211,58,2,0,5,17,1,214,41,2,0,1,15,1,19,59,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0, 
  0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,98,64,65,128,114,208,0,128,15,1,100,59,2,0,5,17,1,103,42,2, 
  0,1,15,1,164,59,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,54,0,0,0,0,0,0,0,2,0,0,0,97,0, 
  0,0,26,0,0,0,40,0,0,0,15,1,228,59,2,0,5,17,1,214,41,2,0,1,15,1,36,60,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0, 
  26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,114,64,1,128,97,208,0,128,15,1,117,60,2,0,5,17,1,214,41,2,0,1,15,1,181,60,2,0,5,17,1,103,42,2,0,1, 
  20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,114,208,0,128,103,64,1,128,15,1,6,61,2,0, 
  5,17,1,103,42,2,0,1,15,1,70,61,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,103,64,1,128,97,208,192,127,15,1,134,61,2,0,5,17,1,214,41,2,0,1,15,1,198,61,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0, 
  0,0,26,0,0,0,26,0,0,0,9,2,21,4,54,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,114,64,1,128,97,208,0,128,15,1,23,62,2,0,5,17,1,214,41,2,0,1,15,1,87,62,2,0,5,17, 
  1,103,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,15,1,168,62, 
  2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0, 
  15,1,231,62,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0, 
  22,0,0,0,15,1,38,63,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0, 
  114,0,0,0,22,0,0,0,15,1,101,63,2,0,5,17,1,103,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0, 
  1,0,0,0,103,0,0,0,22,0,0,0,15,1,164,63,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0, 
  0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,15,1,227,63,2,0,5,17,1,103,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4, 
  36,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,15,1,34,64,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0, 
  9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,15,1,97,64,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0, 
  26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,15,1,160,64,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0, 
  26,0,0,0,26,0,0,0,9,2,21,4,53,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,97,56,1,128,41,208,192,127,5,19,33,0,0,0,86,0,0,0,6,0,1,15,1,223,64,2,0,5,17,1,214,41, 
  2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,15,1,30,65,2,0,5, 
  17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,53,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,97,56,1,128,41,208,192, 
  127,5,19,33,0,0,0,89,0,0,0,6,0,1,15,1,93,65,2,0,5,17,1,214,41,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0, 
  0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,15,1,156,65,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2, 
  20,4,36,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,15,1,219,65,2,0,5,17,1,103,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0, 
  0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,15,1,26,66,2,0,5,17,1,137,43,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0, 
  0,0,26,0,0,0,9,2,21,4,53,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,97,56,1,128,41,208,192,127,5,19,33,0,0,0,94,0,0,0,6,0,1,15,1,89,66,2,0,5,17,1,214,41,2,0, 
  1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,15,1,152,66,2,0,5,17,1, 
  103,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,53,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,97,56,1,128,41,208,192,127,5, 
  19,33,0,0,0,97,0,0,0,6,0,1,15,1,215,66,2,0,5,17,1,214,41,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0, 
  0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,15,1,22,67,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4, 
  36,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,15,1,85,67,2,0,5,17,1,103,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0, 
  9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,15,1,148,67,2,0,5,17,1,248,42,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0, 
  26,0,0,0,9,2,21,4,53,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,97,56,1,128,41,208,192,127,5,19,33,0,0,0,102,0,0,0,6,0,1,15,1,211,67,2,0,5,17,1,214,41,2,0,1,20, 
  2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,15,1,18,68,2,0,5,17,1,103,42, 
  2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,21,4,53,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,97,56,1,128,41,208,192,127,5,19,33, 
  0,0,0,105,0,0,0,6,0,1,15,1,81,68,2,0,5,17,1,214,41,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0, 
  0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,109,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0, 
  0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,110,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4, 
  35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,111,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9, 
  2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,112,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0, 
  0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,113,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0, 
  0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,114,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0, 
  26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,91,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5, 
  0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,92,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0, 
  0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,87,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0, 
  0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,85,0,0,0,7,0,1,20,2,27,0,0,0, 
  0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,90,0,0,0,7,0,1,20,2,27, 
  0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,88,0,0,0,7,0,1, 
  20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,99,0,0,0, 
  7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,100, 
  0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0, 
  0,0,95,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5, 
  19,33,0,0,0,93,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0, 
  0,0,5,19,33,0,0,0,98,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0, 
  0,22,0,0,0,5,19,33,0,0,0,96,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1,0,0,0, 
  41,0,0,0,22,0,0,0,5,19,33,0,0,0,107,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0,0,0,1, 
  0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,108,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0,0,0,0, 
  0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,103,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4,35,0,0, 
  0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,101,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9,2,20,4, 
  35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,106,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0,0,0,9, 
  2,20,4,35,0,0,0,0,0,0,0,1,0,0,0,41,0,0,0,22,0,0,0,5,19,33,0,0,0,104,0,0,0,7,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,5,0,0,0,26,0,0,0,26,0, 
  0,0,9,2,21,4,146,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,2,128,105,80,3,128,10,240,65,129,91,152,66,129,36,96,66,129,109,144,67,129,102,16,3,128,98,208,2,128,115,208,3,128,116,16, 
  4,128,117,80,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,106,0,0,0,1,3,18,123,0,0,0,1,3,17,1,10,71,2,0,1,3,17,1,41,71,2,0,1,3,17,1,96,71,2,0,1,3,17, 
  1,162,71,2,0,1,3,17,1,193,71,2,0,1,3,17,1,224,71,2,0,1,3,17,1,22,72,2,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,223,70,2,0,3,17, 
  1,229,70,2,0,1,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,15,1,208,73,2,0,3,17,1,243,73,2,0,1,20,2,48,0,0,0,0,0,0,0,2,0,0,0,3,0,0, 
  0,26,0,0,0,34,0,0,0,3,17,1,79,73,2,0,1,15,1,165,73,2,0,3,17,1,171,73,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128,47,96,1,128,32,40, 
  1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,2,21,4,152,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,73,64,4,128,10,16,194,129,35,128,66,129,60, 
  192,3,128,45,0,67,129,78,128,4,128,47,64,3,129,43,192,2,128,58,128,3,128,69,0,4,128,95,0,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,17,1,210,74,2,0, 
  1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,240,74,2,0,1,3,17,1,14,75,2,0,1,3,17,1,44,75,2,0,1,3,17,1,142,75,2,0,1,3,17,1,252,75,2,0,1,20,2,30, 
  0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,56,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,45,128,1,128,10,16,1,128,95,128, 
  1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,94,74,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,18,130,0,0,0, 
  1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,148,78,2,0,3,17,1,149,78,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0, 
  3,17,1,180,78,2,0,1,2,21,4,54,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,54,48,1,128,51,240,64,128,97,112,1,128,3,17,1,211,78,2,0,1,3,17,1,241,78,2,0,1,3,17,1,15,79, 
  2,0,1,2,21,4,65,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,56,208,1,128,49,16,1,128,54,144,1,128,51,80,1,128,3,17,1,46,79,2,0,1,3,17,1,76,79,2,0,1,3,17,1,106,79,2, 
  0,1,3,18,40,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,136,79,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22, 
  0,0,0,3,17,1,166,79,2,0,1,2,21,4,53,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,104,1,128,107,240,64,128,111,40,1,128,3,18,76,0,0,0,1,3,17,1,196,79,2,0,1,3,17,1, 
  227,79,2,0,1,2,21,4,65,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,56,208,1,128,49,16,1,128,54,144,1,128,51,80,1,128,3,17,1,2,80,2,0,1,3,17,1,32,80,2,0,1,3,17,1,62, 
  80,2,0,1,3,18,72,0,0,0,1,2,21,4,157,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,105,168,3,128,10,16,66,129,91,184,130,129,36,128,130,129,93,240,194,128,102,104,3,128,98,40, 
  3,128,109,232,195,128,115,40,4,128,116,104,4,128,117,168,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,106,0,0,0,1,3,18,123,0,0,0,1,3,18,95,0,0,0,1,3,17,1,10,71,2,0, 
  1,3,17,1,41,71,2,0,1,3,17,1,96,71,2,0,1,3,17,1,162,71,2,0,1,3,17,1,193,71,2,0,1,3,17,1,224,71,2,0,1,3,17,1,22,72,2,0,1,20,2,36,0,0,0,0,0,0,0,1, 
  0,0,0,4,0,0,0,22,0,0,0,15,1,223,70,2,0,3,17,1,229,70,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,116,96,1,128,3,18,5,0, 
  0,0,1,3,18,6,0,0,0,1,3,17,1,51,82,2,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,168,82,2,0,3,17,1,174,82,2,0,1,20, 
  2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,168,82,2,0,3,17,1,174,82,2,0,1,18,127,0,0,0,1,18,129,0,0,0,1,20,2,36,0,0,0,0,0,0, 
  0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,148,78,2,0,3,17,1,255,82,2,0,1,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,30,83,2, 
  0,1,2,20,4,36,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,15,1,148,78,2,0,3,17,1,116,83,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0, 
  0,26,0,0,0,1,1,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,42,208,0,128,47,16,1,128,3,17,1,174,83,2,0,1,3,17,1,13,84,2,0,1,2,21,4,40,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,79,84,2,0,3,17,1,85,84,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84, 
  2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,112,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0, 
  62,0,0,0,22,0,0,0,3,18,122,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,90,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0, 
  62,0,0,0,22,0,0,0,3,18,114,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,88,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1, 
  3,17,1,166,84,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,64,0,0,0, 
  0,0,0,0,4,0,0,0,2,0,0,0,77,192,1,128,45,16,193,127,95,16,1,128,71,128,193,127,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,8,85,2,0,1,3,17,1,106,85,2,0,1,20,2, 
  40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0, 
  0,0,65,96,129,128,45,240,192,127,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,204,85,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0, 
  0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,116,0,0,0,0,0,0,0,9,0,0,0,3,0,0,0,32,232,1,128,43,96,2,128,10,176,1,129,35,32,130,127,60,96,3,128, 
  45,160,2,128,58,32,3,128,47,224,66,128,95,160,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2, 
  0,1,3,17,1,240,74,2,0,1,3,17,1,14,75,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,79,0,0,0,0,0,0,0,6, 
  0,0,0,2,0,0,0,32,136,1,129,45,248,1,128,10,80,1,128,47,56,130,128,40,192,1,128,95,248,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1, 
  3,17,1,51,74,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1, 
  128,123,192,1,128,10,16,1,128,47,128,129,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,18,89,0,0,0,1,2,21,4,68,0,0,0,0,0,0,0,5,0,0,0,2,0,0, 
  0,32,104,1,128,45,160,1,128,10,48,1,128,47,224,65,128,95,160,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128,47,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3, 
  18,6,0,0,0,1,3,17,1,119,88,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,2,0,0,0,26,0,0,0,33,0,0,0,3,18,2,0,0,0,1,3,18,3,0,0,0,1,2,21,4,52,0,0, 
  0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128,61,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,231,88,2,0,1,2,1,20,2,30,0,0,0,0,0,0,0,1, 
  0,0,0,4,0,0,0,22,0,0,0,3,17,1,149,78,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,130,89,2,0,1,2,20,4,29,0,0,0,0,0,0, 
  0,1,0,0,0,50,0,0,0,22,0,0,0,3,18,75,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,52,0,0,0,22,0,0,0,3,18,58,0,0,0,1,2,20,4,30,0,0,0,0,0,0, 
  0,1,0,0,0,108,0,0,0,22,0,0,0,3,17,1,160,89,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,54,0,0,0,22,0,0,0,3,18,50,0,0,0,1,2,20,4,29,0,0,0,0,0, 
  0,0,1,0,0,0,50,0,0,0,22,0,0,0,3,18,38,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,52,0,0,0,22,0,0,0,3,18,71,0,0,0,1,2,20,4,29,0,0,0,0,0, 
  0,0,1,0,0,0,112,0,0,0,22,0,0,0,3,18,70,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,3,18,46,0,0,0,1,2,20,4,30,0,0,0,0,0, 
  0,0,1,0,0,0,107,0,0,0,22,0,0,0,3,17,1,191,89,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,117,0,0,0,22,0,0,0,3,17,1,227,89,2,0,1,2,20,4,29,0,0,0, 
  0,0,0,0,1,0,0,0,54,0,0,0,22,0,0,0,3,18,36,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,50,0,0,0,22,0,0,0,3,18,42,0,0,0,1,2,20,4,29,0,0,0, 
  0,0,0,0,1,0,0,0,52,0,0,0,22,0,0,0,3,18,69,0,0,0,1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,10,208,64,128,32,8,1,128,3,18,5,0,0,0,1,3,18,6, 
  0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,223,70,2,0,3,17,1,229,70,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32, 
  72,65,128,36,128,129,128,10,16,1,128,116,184,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,106,0,0,0,1,3,17,1,1,90,2,0,1,2,21,4,182,0,0,0,0,0,0,0,15,0,0,0,3, 
  0,0,0,32,168,2,128,41,32,3,128,10,112,130,129,35,224,194,128,44,152,131,129,45,208,131,129,43,88,195,128,47,16,68,129,58,80,4,128,59,144,4,128,60,200,196,128,93,8,197,128,95,208,3,128,124,64,5,128,125, 
  120,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,18,92,0,0,0,1,3,17,1,43,90,2,0,1,3,18,98,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74, 
  2,0,1,3,17,1,78,90,2,0,1,3,18,119,0,0,0,1,3,17,1,14,75,2,0,1,3,18,95,0,0,0,1,3,18,102,0,0,0,1,3,18,32,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0, 
  0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,62,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,45,128,1,128,10,16,1,128,95,128,1,128,3,18,5,0,0,0,1,3, 
  18,6,0,0,0,1,15,1,132,90,2,0,3,17,1,138,90,2,0,1,20,2,54,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,40,0,0,0,15,1,132,90,2,0,3,17,1,138,90,2,0,1, 
  15,1,223,70,2,0,3,17,1,229,70,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,3,18,45,0,0,0,1,2,21,4,56,0,0,0,0,0,0,0,4,0,0,0,2, 
  0,0,0,32,72,1,128,45,128,1,128,10,16,1,128,95,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,91,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22, 
  0,0,0,3,17,1,74,91,2,0,1,2,18,127,0,0,0,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,148,78,2,0,3,17,1,160,91,2,0,1,20,2, 
  40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,148,78,2,0,3,17,1,160,91,2,0,1,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0, 
  0,3,17,1,255,82,2,0,1,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,168,82,2,0,3,17,1,229,91,2,0,1,20,2,40,0,0,0,0,0,0,0, 
  2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,168,82,2,0,3,17,1,229,91,2,0,1,18,127,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,95,0,0,0,22,0,0,0,3,17, 
  1,116,83,2,0,1,20,2,27,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,1,1,20,4,36,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,15,1,167,92, 
  2,0,3,17,1,173,92,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,54,92,2,0,3,17,1,91,92,2, 
  0,1,17,1,198,92,2,0,1,20,2,59,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,52,0,0,0,38,0,0,0,15,1,25,93,2,0,3,17,1,55,93,2,0, 
  1,3,18,132,0,0,0,1,17,1,104,93,2,0,1,18,134,0,0,0,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,148,78,2,0,3,17,1,164,93,2,0, 
  1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,148,78,2,0,3,17,1,164,93,2,0,1,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0, 
  80,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,233,93,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0, 
  15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,78,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164, 
  93,2,0,1,3,17,1,63,94,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4, 
  52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,80,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,149,94,2,0,1,20,2,40,0,0,0,0,0,0,0, 
  2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,77,96,129,128,45,240, 
  192,127,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,235,94,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0, 
  3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,76,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,128,65,224,193,128,10,48,1,128,47,160,1,128,97,32,2,128,3,18,5,0,0,0,1,3, 
  18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,17,1,154,95,2,0,1,3,17,1,184,95,2,0,1,2,21,4,149,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,130,33,128,2,128,10,16,2, 
  129,91,240,3,130,116,104,4,128,45,112,3,128,34,184,2,128,39,248,130,128,40,56,3,128,47,176,67,128,95,112,3,128,99,40,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,93,0,0,0,1,3,17, 
  1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1, 
  20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,196,0,0,0,0,0,0,0,16,0,0,0,4,0,0,0,32,200,2,128,58,120,4,128,34,0,3, 
  128,99,104,5,128,36,64,67,129,63,184,68,128,95,248,3,128,39,120,3,128,40,184,3,128,116,168,5,128,10,144,194,125,91,240,68,128,123,232,5,128,45,248,3,128,94,40,5,128,47,56,132,125,3,18,5,0,0,0,1, 
  3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,17,1,118,101,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,164,101,2, 
  0,1,3,18,87,0,0,0,1,3,18,123,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0, 
  0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,100,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,32,168,129,129,34,224,1,128,10,112,193,127,39,32,66,128,47,96,66,128,99,160,2,128, 
  116,224,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,17,1,51,74,2,0,1,3,17,1,27,102,2,0,1,3,17,1,57,102,2,0,1,2,21, 
  4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,42,208,0,128,47,16,1,128,3,17,1,174,83,2,0,1,3,17,1,13,84,2,0,1,18,2,0,0,0,1,21,4,63,0,0,0,0,0,0,0,4,0,0, 
  0,2,0,0,0,32,72,1,128,62,192,1,128,10,16,193,127,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,18,120,0,0,0,1,2,20,4,29,0,0,0,0,0,0, 
  0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,109,0,0,0,1,2,21,4,124,0,0,0,0,0,0,0,9,0,0,0,3,0,0,0,32,232,1,129,97,32,2,128,10,176,129,129,115,160,3,128,112,32,3,128,109, 
  224,2,128,102,96,2,128,103,160,2,128,114,96,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,152,102,2,0,1,3,17,1,195,102,2,0,1,3,17,1,226,102,2,0,1,3,17,1,1,103,2,0, 
  1,3,17,1,32,103,2,0,1,3,17,1,99,103,2,0,1,3,17,1,130,103,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,18,63,0,0,0,1,2,20,4,30,0, 
  0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,220,104,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,250,104,2,0,1,18,16,0, 
  0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,18,41,0,0,0,1,2,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,111,8,1,128,107,208,192,127,3, 
  18,76,0,0,0,1,3,17,1,196,79,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,62,0,0,0,22,0,0,0,3,18,122,0,0,0,1,18,100,0,0,0,1,21,4,53,0,0,0,0,0,0,0, 
  3,0,0,0,1,0,0,0,62,240,128,128,115,104,1,128,114,40,1,128,3,18,90,0,0,0,1,3,17,1,24,105,2,0,1,3,17,1,55,105,2,0,1,2,18,131,0,0,0,1,21,4,40,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,148,78,2,0,3,17,1,86,105,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,148,78, 
  2,0,3,17,1,86,105,2,0,1,1,21,4,80,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,65,129,45,192,1,128,10,80,1,128,95,192,65,128,99,0,2,128,116,64,2,128,3,18,5,0,0,0,1, 
  3,18,6,0,0,0,1,3,17,1,74,91,2,0,1,3,17,1,155,105,2,0,1,3,17,1,248,105,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,74,91,2,0, 
  1,2,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,101,106,2,0,3,17,1,86,105,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,15,1,101,106,2,0,3,17,1,86,105,2,0,1,18,126,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,160,91,2, 
  0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,160,91,2,0,1,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45, 
  208,192,127,15,1,148,78,2,0,3,17,1,107,106,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,148,78,2,0,3,17,1,107,106,2,0,1,1,20,4, 
  36,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,15,1,167,92,2,0,3,17,1,173,92,2,0,1,2,20,4,23,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,1,20,2, 
  52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,148,78,2,0,3,17,1,176,106,2,0,1,1,18,128,0,0,0,1,20,4,24, 
  0,0,0,0,0,0,0,1,0,0,0,47,0,0,0,22,0,0,0,3,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,42,0,0,0,22,0,0,0,3,17,1,246,106,2,0,1,20,2,52,0,0,0,0,0, 
  0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,54,92,2,0,3,17,1,91,92,2,0,1,2,20,2,29,0,0,0,0,0,0,0,1,0,0,0,5, 
  0,0,0,22,0,0,0,3,18,132,0,0,0,1,2,21,2,48,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,4,16,1,128,6,16,1,128,2,16,193,127,3,16,1,128,15,1,148,78,2,0,3,17,1,96,107, 
  2,0,1,1,20,2,59,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,52,0,0,0,38,0,0,0,15,1,25,93,2,0,3,17,1,55,93,2,0,1,3,18,132,0, 
  0,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,164,93,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0, 
  26,0,0,0,3,17,1,164,93,2,0,1,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,213,107,2,0,1,20, 
  2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,48,129, 
  128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,43,108,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18, 
  134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,129,108,2,0,1,20,2,34,0,0,0,0,0,0, 
  0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,69,48,129,128,45,240,192,127,95,240,0, 
  128,3,17,1,139,107,2,0,1,3,17,1,215,108,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,88, 
  0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,1,128,69,0,194,128,10,80,193,128,47,192,1,128,73,64,2,128,78,128,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0, 
  1,3,17,1,255,108,2,0,1,3,17,1,30,109,2,0,1,3,17,1,73,109,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,83,0,0,0,22,0,0,0,3,18,77,0,0,0,1,2,20,4,29,0, 
  0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,18,85,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,222,109,2,0,1,20,2,52,0, 
  0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,104,109,2,0,3,17,1,134,109,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1, 
  0,0,0,92,0,0,0,22,0,0,0,3,17,1,167,110,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,49, 
  110,2,0,3,17,1,79,110,2,0,1,2,21,4,51,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,58,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,18,111,0, 
  0,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3, 
  0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,96,1,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,250,110,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26, 
  0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,58,192,1,128,10,16,193,127,47,128,1, 
  128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,36,128,1,128,10,16, 
  1,128,47,184,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,106,0,0,0,1,3,17,1,51,74,2,0,1,2,21,4,74,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,128,94,216, 
  1,128,10,48,193,127,63,160,65,128,123,24,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,87,0,0,0,1,3,17,1,194,101,2,0,1,3,18,89,0,0,0,1,2,21,4,52,0,0,0,0,0,0, 
  0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,40,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,128,118,2,0,1,2,21,4,67,0,0,0,0,0,0,0,5,0,0,0,2,0, 
  0,0,32,104,1,129,45,216,1,128,10,48,1,128,95,216,1,128,40,160,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,130,129,34,96,2,128,10,240,193,127,91,152,195,129,116,16,4,128, 
  45,24,3,128,40,224,2,128,39,160,66,128,47,88,67,128,95,24,3,128,99,208,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0, 
  1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0, 
  0,0,3,17,1,94,74,2,0,1,2,21,4,112,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,32,200,65,129,97,0,130,129,10,144,1,128,115,0,3,128,116,64,3,128,104,64,2,128,110,192,2,128,105,128,2, 
  128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,1,119,2,0,1,3,17,1,32,119,2,0,1,3,17,1,63,119,2,0,1,3,17,1,93,119,2,0,1,3,17,1,135,119,2,0,1,3,17,1,177,119, 
  2,0,1,2,21,4,139,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,130,129,34,96,2,128,10,240,193,127,99,152,3,128,116,216,3,128,45,24,67,129,40,224,2,128,39,160,66,128,47,88,67,128,95,24, 
  3,128,117,24,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3, 
  17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,17,1,208,119,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,64,0,0,0, 
  0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,40,128,1,128,10,16,65,128,58,192,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,128,118,2,0,1,3,17,1,164,101,2,0,1,2,21, 
  4,176,0,0,0,0,0,0,0,14,0,0,0,3,0,0,0,32,136,2,128,65,64,132,130,10,80,194,129,35,192,66,129,60,0,4,128,45,64,67,129,78,0,5,128,47,128,67,129,43,0,3,128,58,192,3,128,69,128,4, 
  128,73,192,132,128,95,64,3,128,97,64,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3, 
  17,1,240,74,2,0,1,3,17,1,14,75,2,0,1,3,17,1,49,122,2,0,1,3,17,1,44,75,2,0,1,3,17,1,142,75,2,0,1,3,17,1,252,75,2,0,1,3,17,1,147,122,2,0,1,20,2,30,0,0, 
  0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,20,4,40,0,0,0,0,0,0,0,2,0,0,0,42,0,0,0,26,0,0,0,33,0,0,0,3,18,113,0,0,0,1,3, 
  18,121,0,0,0,1,18,108,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,3,18,29,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0, 
  95,208,0,128,45,208,192,127,15,1,245,122,2,0,15,1,148,78,2,0,3,17,1,251,122,2,0,1,20,2,42,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,15,1,245,122,2,0,15,1,148,78, 
  2,0,3,17,1,251,122,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,3,18,111,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,107,0,0,0,22, 
  0,0,0,3,17,1,76,123,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,125,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1, 
  3,17,1,51,74,2,0,1,3,18,97,0,0,0,1,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,115,16,1,128,99,208,192,127,3,17,1,219,123,2,0,1,3,17,1,250,123,2,0,1,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,25,124,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,56,124,2,0,1, 
  2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,87,124,2,0,1,2,21,4,66,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,101,80,129,128,97,16,193,127,117,208, 
  1,128,111,144,1,128,3,17,1,118,124,2,0,1,3,17,1,149,124,2,0,1,3,17,1,180,124,2,0,1,3,17,1,211,124,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0, 
  0,3,17,1,242,124,2,0,1,2,21,4,66,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,104,144,1,128,101,80,1,128,107,208,1,128,99,16,193,127,3,17,1,17,125,2,0,1,3,17,1,48,125,2,0,1, 
  3,17,1,78,125,2,0,1,3,17,1,109,125,2,0,1,2,21,4,100,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,32,168,129,129,97,224,1,129,10,112,129,128,103,96,2,128,102,32,2,128,109,160,2,128,112, 
  224,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,140,125,2,0,1,3,17,1,195,102,2,0,1,3,17,1,226,102,2,0,1,3,17,1,1,103,2,0,1,3,17,1,171,125,2,0,1,2,21,4, 
  124,0,0,0,0,0,0,0,9,0,0,0,3,0,0,0,32,232,1,129,97,32,2,128,10,176,129,129,115,160,3,128,112,32,3,128,109,224,2,128,102,96,2,128,103,160,2,128,114,96,3,128,3,18,5,0,0,0,1,3, 
  18,6,0,0,0,1,3,17,1,152,102,2,0,1,3,17,1,195,102,2,0,1,3,17,1,226,102,2,0,1,3,17,1,1,103,2,0,1,3,17,1,171,125,2,0,1,3,17,1,99,103,2,0,1,3,17,1,200,126,2, 
  0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,128,128,103,96,1,128,32,40,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,226,102,2,0,1,2,20,4,29,0, 
  0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,18,59,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,18,79,0,0,0,1,2,20,4,30,0, 
  0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,151,127,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,121,0,0,0,22,0,0,0,3,17,1,181,127,2,0,1,2,21,4, 
  34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,86,105,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17, 
  1,86,105,2,0,1,1,21,4,47,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,64,1,128,45,208,192,127,15,1,101,106,2,0,3,17,1,86,105,2,0,1,3,18,82,0,0,0,1,20,2,40,0,0,0, 
  0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,101,106,2,0,3,17,1,86,105,2,0,1,18,126,0,0,0,1,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,107,128, 
  129,128,45,16,1,128,111,184,1,128,95,16,65,127,15,1,101,106,2,0,3,17,1,86,105,2,0,1,3,18,76,0,0,0,1,3,17,1,211,127,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0, 
  0,26,0,0,0,26,0,0,0,15,1,101,106,2,0,3,17,1,86,105,2,0,1,18,126,0,0,0,1,18,126,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192, 
  127,3,17,1,107,106,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,107,106,2,0,1,1,20,4,23,0,0,0,0,0,0,0,1,0,0,0,42,0, 
  0,0,22,0,0,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,176,106,2,0,1,1,21,4,47,0,0,0, 
  0,0,0,0,2,0,0,0,1,0,0,0,42,208,0,128,47,64,1,128,15,1,167,92,2,0,3,17,1,173,92,2,0,1,3,18,128,0,0,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38, 
  0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,235,128,2,0,3,17,1,176,106,2,0,1,17,1,54,92,2,0,1,21,2,42,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,4,16, 
  1,128,6,16,1,128,2,16,193,127,3,16,1,128,3,17,1,96,107,2,0,1,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,20,2,34, 
  0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,48,1,128,45, 
  240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,242,128,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0, 
  0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,72,129,2,0,1,20,2,34,0,0,0,0,0,0,0,2, 
  0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,48,1,128,45,240,64,128,95,240,0,128,3, 
  17,1,139,107,2,0,1,3,17,1,158,129,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,34,0,0, 
  0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,68,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,88,0,0,0,22,0,0,0,3,17,1, 
  244,129,2,0,1,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,77,16,1,128,71,208,192,127,3,17,1,19,130,2,0,1,3,17,1,50,130,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1, 
  0,0,0,65,0,0,0,22,0,0,0,3,17,1,81,130,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,34,0,0,0,22,0,0,0,3,18,135,0,0,0,1,2,21,4,35,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,34,208,64,128,92,216,0,128,1,3,17,1,194,130,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38, 
  0,0,0,15,1,148,78,2,0,3,17,1,112,130,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,121,131,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,21,131,2,0,1,17,1,204,131,2,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,39,0,0,0,22, 
  0,0,0,3,18,125,0,0,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,216,0,128,39,208,0,128,1,3,17,1,77,132,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0, 
  2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,148,78,2,0,3,17,1,251,131,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0, 
  0,3,17,1,4,133,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,160,132,2,0,1,17,1,87,133,2, 
  0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,58,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,134,133,2,0,1,20,2,40,0,0,0, 
  0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40, 
  2,128,41,160,2,128,10,240,129,129,35,96,194,128,60,216,131,129,45,24,3,128,43,216,2,128,47,88,131,128,58,152,3,128,95,24,3,128,124,24,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180, 
  74,2,0,1,3,18,92,0,0,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,240,74,2,0,1,3,17,1,14,75,2,0,1,3,18,102,0,0,0,1,20,2, 
  30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,219,0,0,0,0,0,0,0,18,0,0,0,4,0,0,0,32,8,3,128,58,104,5,128,34,64,3,128,35, 
  128,195,130,36,192,3,131,91,232,5,128,95,232,4,128,39,248,3,128,40,56,4,128,41,112,4,128,10,208,194,125,43,168,132,126,60,168,69,129,45,232,4,128,99,32,6,128,47,40,197,125,116,96,6,128,124,160,6,128,3, 
  18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,180,74,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,18,92,0,0,0,1,3,17, 
  1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,233,133,2,0,1,3,17,1,14,75,2,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0, 
  1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,254,0,0,0,0,0,0,0,21,0,0,0,4,0,0,0,32,104,3, 
  128,58,208,5,128,34,160,3,128,35,224,131,131,36,32,132,131,63,80,198,130,91,136,70,131,39,88,4,128,40,152,4,128,41,216,4,128,10,48,195,125,43,16,197,126,60,16,6,130,45,80,5,128,94,192,6,128,47,144,133, 
  125,95,80,5,128,99,0,7,128,116,64,7,128,123,128,7,128,124,184,7,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,180,74,2,0,1,3,18,106,0,0,0,1,3,17, 
  1,41,96,2,0,1,3,17,1,118,101,2,0,1,3,18,92,0,0,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,233,133,2,0,1,3,17,1,14,75,2,0, 
  1,3,18,87,0,0,0,1,3,18,123,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0, 
  0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,253,0,0,0,0,0,0,0,21,0,0,0,4,0,0,0,32,104,3,128,58,200,5,128,34,160,3,128,35,224,131,131,36,32, 
  132,131,63,72,198,130,91,128,70,131,39,88,4,128,40,152,4,128,41,208,4,128,10,48,195,125,43,8,197,126,60,8,6,130,45,72,5,128,94,184,6,128,47,136,133,125,95,72,5,128,99,248,6,128,116,56,7,128,123,120, 
  7,128,124,176,7,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,180,74,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,18, 
  92,0,0,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,233,133,2,0,1,3,17,1,14,75,2,0,1,3,18,87,0,0,0,1,3,18,123,0,0,0,1,3, 
  17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3, 
  17,1,94,74,2,0,1,2,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,2,128,41,160,2,128,10,240,129,129,35,96,194,128,60,216,131,129,45,24,3,128,43,216,2,128,47,88,131,128,58,152, 
  3,128,95,24,3,128,124,24,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,18,92,0,0,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74, 
  2,0,1,3,17,1,233,133,2,0,1,3,17,1,14,75,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,54, 
  0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,48,1,128,97,240,64,128,115,112,1,128,3,17,1,217,135,2,0,1,3,17,1,24,105,2,0,1,3,17,1,55,105,2,0,1,2,21,4,75,0,0,0,0,0, 
  0,0,5,0,0,0,2,0,0,0,32,104,1,129,58,224,1,128,10,48,193,127,47,160,1,128,124,32,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,17,1,248,135,2,0,1, 
  3,18,102,0,0,0,1,2,21,4,169,0,0,0,0,0,0,0,13,0,0,0,3,0,0,0,32,104,2,128,105,208,3,128,10,48,130,129,91,24,131,129,36,160,130,129,109,16,132,129,102,144,3,128,47,216,2,128,98,80, 
  3,128,115,80,196,128,116,144,4,128,117,208,4,128,123,16,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,106,0,0,0,1,3,17,1,51,74,2,0,1,3,18,123,0,0,0,1,3,17,1,10,71,2, 
  0,1,3,17,1,41,71,2,0,1,3,17,1,96,71,2,0,1,3,17,1,162,71,2,0,1,3,17,1,193,71,2,0,1,3,17,1,224,71,2,0,1,3,17,1,22,72,2,0,1,3,18,89,0,0,0,1,20,2,36, 
  0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,223,70,2,0,3,17,1,229,70,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,58,192,1,128, 
  10,16,193,127,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,17,1,49,137,2,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1, 
  128,123,192,1,128,10,16,1,128,47,128,129,127,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,18,25,0,0,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0, 
  0,32,72,1,128,94,128,1,128,10,16,193,127,123,192,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,194,101,2,0,1,3,18,89,0,0,0,1,2,21,4,63,0,0,0,0,0,0,0,4,0,0, 
  0,2,0,0,0,32,72,1,128,94,184,1,128,10,16,193,127,63,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,87,0,0,0,1,3,17,1,194,101,2,0,1,2,21,4,40,0,0,0,0,0,0, 
  0,2,0,0,0,1,0,0,0,10,208,64,128,32,8,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,105,138,2,0,3, 
  17,1,111,138,2,0,1,2,20,4,40,0,0,0,0,0,0,0,2,0,0,0,42,0,0,0,26,0,0,0,33,0,0,0,3,18,113,0,0,0,1,3,18,121,0,0,0,1,2,21,4,87,0,0,0,0,0,0,0,6, 
  0,0,0,2,0,0,0,32,136,1,128,41,64,2,128,10,80,129,128,39,0,130,128,34,192,1,128,99,120,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0, 
  1,3,18,28,0,0,0,1,3,17,1,27,102,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,17,1,196,139,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0, 
  0,0,116,0,0,0,22,0,0,0,3,17,1,226,139,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,100,0,0,0,22,0,0,0,3,18,19,0,0,0,1,2,21,4,41,0,0,0,0,0,0,0,2, 
  0,0,0,1,0,0,0,108,208,0,128,117,8,1,128,3,18,18,0,0,0,1,3,17,1,1,140,2,0,1,2,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,112,208,0,128,121,8,1,128,3,18,22, 
  0,0,0,1,3,17,1,31,140,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,61,140,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0, 
  0,0,110,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,91,140,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0, 
  0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,127,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,130,129,34,64,2,128,10,208,193,127,99,120,3,128,116,184,3,128, 
  45,248,2,128,40,192,2,128,39,128,66,128,47,56,67,128,95,248,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1, 
  94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2, 
  21,4,63,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,1,128,93,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,18,30,0, 
  0,0,1,2,21,4,74,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,129,41,160,1,128,10,48,1,128,47,216,1,128,124,24,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,92,0, 
  0,0,1,3,17,1,51,74,2,0,1,3,18,102,0,0,0,1,2,21,4,183,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,2,130,41,208,3,128,10,112,2,129,91,200,68,130,36,32,67,130,45,8,4, 
  128,34,224,2,129,39,88,131,128,40,152,3,128,47,72,132,128,58,136,4,128,95,8,4,128,99,0,5,128,116,64,69,128,124,128,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3, 
  18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,18,92,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,123,0,0,0,1,3, 
  17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,52,0,0,0,0, 
  0,0,0,3,0,0,0,1,0,0,0,83,96,129,128,45,240,192,127,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,102,141,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1, 
  128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,148,141,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84, 
  2,0,1,18,134,0,0,0,1,18,133,0,0,0,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,148,78,2,0,3,17,1,85,84,2,0,1,20,2,40,0,0, 
  0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,148,78,2,0,3,17,1,85,84,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,58,0,0,0,22,0,0,0,3,17, 
  1,134,133,2,0,1,2,21,4,111,0,0,0,0,0,0,0,8,0,0,0,3,0,0,0,32,200,1,128,34,0,2,128,10,144,193,127,99,192,2,128,116,0,3,128,125,64,3,128,47,128,2,128,39,64,194,127,3,18,5, 
  0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,17,1,51,74,2,0,1,3,17,1,27,102,2,0,1,3,17,1,57,102,2,0,1,3,18,97,0,0,0,1,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,79,143,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,110,143,2,0,1, 
  2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22,0,0,0,3,17,1,153,143,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,183,143,2, 
  0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,213,143,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,244, 
  143,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,18,144,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,112,0,0,0,22,0,0,0,3,17, 
  1,48,144,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,79,144,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,100,0,0,0,22,0,0,0, 
  3,17,1,110,144,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,141,144,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0, 
  0,0,3,18,83,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22,0,0,0,3,17,1,171,144,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22, 
  0,0,0,3,17,1,202,144,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,219,123,2,0,1,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0, 
  0,117,16,1,128,97,208,192,127,3,17,1,118,124,2,0,1,3,17,1,211,124,2,0,1,2,21,4,211,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,66,130,97,160,4,128,114,224,5,128,35,32,195,130, 
  58,32,4,128,95,160,3,128,102,224,4,128,103,32,5,128,109,96,5,130,112,160,5,128,10,176,130,126,43,96,3,128,60,96,4,128,45,160,195,126,115,32,6,128,47,224,131,125,125,96,6,128,3,18,5,0,0,0,1,3, 
  18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,240,74,2,0,1,3,17,1,14,75,2,0,1,3,17,1,232,144,2, 
  0,1,3,17,1,86,145,2,0,1,3,17,1,184,145,2,0,1,3,17,1,26,146,2,0,1,3,17,1,124,146,2,0,1,3,17,1,234,146,2,0,1,3,17,1,76,147,2,0,1,3,18,97,0,0,0,1,20,2,30, 
  0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,48,125,2,0,1,2,21, 
  4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,32,72,65,128,112,192,1,128,10,16,1,128,103,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,226,102,2,0,1,3,17,1,251,147, 
  2,0,1,2,21,4,80,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,1,129,45,192,1,128,10,80,1,128,95,192,1,128,80,0,66,128,84,64,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1, 
  3,17,1,94,74,2,0,1,3,17,1,79,148,2,0,1,3,17,1,177,148,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,20,4,29,0,0, 
  0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,18,110,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,18,104,0,0,0,1,2,21,4,46,0,0, 
  0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,107,48,1,128,3,17,1,19,149,2,0,1,3,17,1,93,149,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26, 
  0,0,0,26,0,0,0,3,17,1,19,149,2,0,1,18,126,0,0,0,1,21,4,157,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,128,105,112,3,128,10,16,66,129,91,184,66,129,36,128,66,129,109, 
  176,67,129,102,48,3,128,98,240,2,128,115,240,195,128,116,48,4,128,117,112,4,128,123,176,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,106,0,0,0,1,3,18,123,0,0,0,1,3,17,1,10,71, 
  2,0,1,3,17,1,41,71,2,0,1,3,17,1,96,71,2,0,1,3,17,1,162,71,2,0,1,3,17,1,193,71,2,0,1,3,17,1,224,71,2,0,1,3,17,1,22,72,2,0,1,3,18,89,0,0,0,1,20,2, 
  36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,223,70,2,0,3,17,1,229,70,2,0,1,2,17,1,54,92,2,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0, 
  84,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,179,149,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2, 
  0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,69,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,219,149,2,0,1,20,2,34,0,0,0, 
  0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128, 
  95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,3,150,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1, 
  20,4,30,0,0,0,0,0,0,0,1,0,0,0,80,0,0,0,22,0,0,0,3,17,1,43,150,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,78,0,0,0,22,0,0,0,3,17,1,74,150,2,0, 
  1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,80,0,0,0,22,0,0,0,3,17,1,105,150,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,77,0,0,0,22,0,0,0,3,17,1,136,150, 
  2,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,34,208,64,128,92,216,0,128,1,3,17,1,166,150,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0, 
  0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,112,130,2,0,1,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,194,130,2,0,1,20,2,46,0, 
  0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,249,150,2,0,1,17,1,75,151,2,0,1,21,4,41,0,0,0,0,0,0,0,2, 
  0,0,0,1,0,0,0,34,208,64,128,92,8,1,128,3,18,135,0,0,0,1,3,17,1,121,131,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,15,1,122,151,2,0,3,17,1,112,130,2,0,1,17,1,104,109,2,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,121,131,2,0,1,20, 
  2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,129,151,2,0,1,17,1,223,151,2,0,1,20,2,46,0,0,0,0,0, 
  0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,21,131,2,0,1,2,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,216,0, 
  128,39,208,0,128,1,3,17,1,14,152,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,251,131,2,0,1, 
  1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,77,132,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0, 
  0,38,0,0,0,38,0,0,0,3,17,1,97,152,2,0,1,17,1,179,152,2,0,1,21,4,41,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,8,1,128,39,208,0,128,3,18,125,0,0,0,1,3,17,1, 
  4,133,2,0,1,20,2,52,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,15,1,226,152,2,0,3,17,1,251,131,2,0,1,17,1,49, 
  110,2,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,4,133,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,233,152,2,0,1,17,1,71,153,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0, 
  0,38,0,0,0,3,17,1,160,132,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,40,0,0,0,22,0,0,0,3,18,103,0,0,0,1,18,88,0,0,0,1,21,4,63,0,0,0,0,0,0,0,4, 
  0,0,0,2,0,0,0,32,72,65,128,124,192,1,128,10,16,1,128,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,18,102,0,0,0,1,2,21,4,65,0,0,0,0, 
  0,0,0,4,0,0,0,2,0,0,0,114,136,1,128,97,72,1,128,62,16,129,127,115,200,1,128,3,18,90,0,0,0,1,3,17,1,217,135,2,0,1,3,17,1,24,105,2,0,1,3,17,1,55,105,2,0,1,2,21, 
  4,149,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,130,129,34,128,2,128,10,16,194,127,91,240,195,129,36,192,194,129,45,112,3,128,40,56,3,128,39,248,66,128,47,176,67,128,95,112,3,128,99,40,4, 
  128,116,104,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1, 
  51,74,2,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21, 
  4,219,0,0,0,0,0,0,0,18,0,0,0,4,0,0,0,32,8,3,128,58,184,4,128,34,64,3,128,99,232,5,128,36,128,3,131,61,248,4,128,63,56,197,128,39,184,3,128,40,248,3,128,95,56,4,128,10,208,194, 
  125,91,112,133,129,124,160,6,128,45,56,4,126,94,168,5,128,47,120,196,125,116,40,6,128,123,104,6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17, 
  1,41,96,2,0,1,3,17,1,118,101,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,118,153,2,0,1,3,17,1,231,88,2,0,1,3,18,87,0,0,0,1,3,18,123,0,0,0,1, 
  3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0, 
  3,17,1,94,74,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,59,156,2,0,1,2,20,4,42,0,0,0,0,0,0,0,2,0,0,0,114,0,0,0,26,0, 
  0,0,34,0,0,0,3,17,1,24,105,2,0,1,3,17,1,55,105,2,0,1,2,21,4,75,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,129,58,224,1,128,10,48,193,127,47,160,1,128,124,32,2, 
  128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,17,1,49,137,2,0,1,3,18,102,0,0,0,1,2,21,4,157,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2, 
  128,105,112,3,128,10,16,66,129,91,184,66,129,36,128,66,129,109,176,67,129,102,48,3,128,98,240,2,128,115,240,3,128,116,48,4,128,117,112,68,128,125,176,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3, 
  18,106,0,0,0,1,3,18,123,0,0,0,1,3,17,1,10,71,2,0,1,3,17,1,41,71,2,0,1,3,17,1,96,71,2,0,1,3,17,1,162,71,2,0,1,3,17,1,193,71,2,0,1,3,17,1,89,156,2,0, 
  1,3,17,1,22,72,2,0,1,3,18,32,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,223,70,2,0,3,17,1,229,70,2,0,1,2,20,4,30,0,0,0,0, 
  0,0,0,1,0,0,0,114,0,0,0,22,0,0,0,3,17,1,24,105,2,0,1,2,21,4,197,0,0,0,0,0,0,0,16,0,0,0,4,0,0,0,32,200,2,128,58,64,4,128,34,0,3,128,99,112,5,128,116,176, 
  5,128,61,128,4,128,63,192,196,128,39,64,3,128,40,128,3,128,95,192,3,128,10,144,194,125,91,248,68,128,123,240,5,128,45,192,3,126,94,48,5,128,47,0,196,125,3,18,5,0,0,0,1,3,18,6,0,0,0,1, 
  3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,17,1,118,101,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,164,101,2,0,1,3,17,1,231,88,2,0,1,3,18,87,0, 
  0,0,1,3,18,123,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22, 
  0,0,0,3,17,1,94,74,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,94,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,194, 
  101,2,0,1,2,18,124,0,0,0,1,20,2,36,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,22,0,0,0,15,1,148,78,2,0,3,17,1,154,156,2,0,1,1,21,4,208,0,0,0,0,0,0,0,17,0, 
  0,0,4,0,0,0,32,232,2,128,58,152,4,128,34,32,3,128,99,200,5,128,36,96,3,130,61,216,4,128,63,24,197,128,39,152,3,128,40,216,3,128,95,24,4,128,10,176,194,125,91,80,69,129,116,8,6,128,45,24, 
  4,126,94,136,5,128,47,88,196,125,123,72,6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,17,1,118,101,2,0,1,3, 
  17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,118,153,2,0,1,3,17,1,231,88,2,0,1,3,18,87,0,0,0,1,3,18,123,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0, 
  1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0, 
  0,2,0,0,0,32,72,1,128,58,192,1,128,10,16,193,127,47,128,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,51,74,2,0,1,3,17,1,248,135,2,0,1,2,20,4,29,0,0,0,0,0, 
  0,0,1,0,0,0,121,0,0,0,22,0,0,0,3,18,23,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,185,156,2,0,1,2,20,4,29,0,0,0,0, 
  0,0,0,1,0,0,0,109,0,0,0,22,0,0,0,3,18,17,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,109,0,0,0,22,0,0,0,3,18,24,0,0,0,1,2,20,4,29,0,0,0,0, 
  0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,3,18,21,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,96,1,128,15,1,79,84,2,0,3, 
  17,1,164,93,2,0,1,3,17,1,215,156,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0, 
  1,21,4,138,0,0,0,0,0,0,0,11,0,0,0,3,0,0,0,32,40,130,129,34,96,2,128,10,240,193,127,99,208,3,128,116,16,4,128,45,24,3,129,40,224,2,128,39,160,66,128,47,88,131,128,93,152,3,128,95, 
  24,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,18,30,0, 
  0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,40,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,77,0,0,0,1,21,4,40,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208, 
  192,127,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,85,0,0,0,1,21,4,127,0,0,0,0,0,0,0,10,0,0,0,3,0,0,0,32,8,2,128,43,128,2,128,10,208,1,129,35,64,130,127,60,128,67,129, 
  45,192,2,128,58,64,3,128,47,0,67,128,95,192,2,128,124,192,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,180,74,2,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17, 
  1,51,74,2,0,1,3,17,1,240,74,2,0,1,3,17,1,14,75,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2, 
  21,4,208,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,2,128,58,16,5,128,34,32,3,128,35,96,131,129,36,160,131,130,91,144,5,128,95,144,4,128,39,216,3,128,40,24,4,128,99,200,5,128,10,176, 
  194,125,43,80,132,126,60,80,5,129,45,144,4,128,116,8,6,128,47,208,196,125,124,72,6,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,180,74,2,0,1,3,18,106,0, 
  0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,210,74,2,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,233,133,2,0,1,3,17,1,14,75,2,0,1,3,18, 
  123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,20, 
  4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,17,1,45,157,2,0,1,2,21,4,42,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,105,16,1,128,101,208,192,127,3,17,1,76, 
  157,2,0,1,3,17,1,107,157,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,18,66,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,111,0,0, 
  0,22,0,0,0,3,18,60,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,138,157,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,115,0, 
  0,0,22,0,0,0,3,18,73,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,107,0,0,0,22,0,0,0,3,18,52,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,45,0, 
  0,0,22,0,0,0,3,17,1,168,157,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0,3,17,1,199,157,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0, 
  117,0,0,0,22,0,0,0,3,17,1,230,157,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,18,64,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0, 
  0,102,0,0,0,22,0,0,0,3,17,1,5,158,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,112,0,0,0,22,0,0,0,3,18,56,0,0,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0, 
  0,0,2,0,0,0,99,128,129,128,45,16,1,128,115,192,1,128,95,16,65,127,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,35,158,2,0,1,3,17,1,133,158,2,0,1,20,2,40,0,0,0,0,0, 
  0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128, 
  45,240,192,127,97,96,1,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,231,158,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84, 
  2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,96,1,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1, 
  3,17,1,73,159,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0, 
  0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,97,96,1,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,171,159,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0, 
  3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,97,128,129,128,45,16,193,127,117,192, 
  1,128,95,16,1,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,13,160,2,0,1,3,17,1,111,160,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0, 
  0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,96,1,128,15,1,79,84,2,0,3,17, 
  1,85,84,2,0,1,3,17,1,209,160,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1, 
  21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,96,1,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,51,161,2,0,1,20,2,40,0,0,0,0,0, 
  0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,76,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,128, 
  97,160,1,128,10,48,129,128,115,32,2,128,114,224,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,149,161,2,0,1,3,17,1,99,103,2,0,1,3,17,1,200,126,2,0,1,2,20,4,30,0,0, 
  0,0,0,0,0,1,0,0,0,117,0,0,0,22,0,0,0,3,17,1,211,124,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,112,96,1,128,3,18,5,0, 
  0,0,1,3,18,6,0,0,0,1,3,17,1,251,147,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84, 
  2,0,1,3,17,1,180,161,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,52, 
  0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,69,96,129,128,45,240,192,127,95,240,0,128,15,1,79,84,2,0,3,17,1,85,84,2,0,1,3,17,1,22,162,2,0,1,20,2,40,0,0,0,0,0,0,0,2, 
  0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,85,84,2,0,1,18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192, 
  127,3,17,1,19,149,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,19,149,2,0,1,18,126,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3, 
  0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,19,149,2,0,1,3,17,1,40,163,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0, 
  0,3,17,1,19,149,2,0,1,18,16,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,49,0,0,0,1,21,4,34,0,0, 
  0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,67,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192, 
  127,3,17,1,139,107,2,0,1,18,78,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,79,0,0,0,22,0,0,0,3,17,1,125,163,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0, 
  79,0,0,0,22,0,0,0,3,17,1,156,163,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,79,0,0,0,22,0,0,0,3,17,1,187,163,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0, 
  0,0,69,0,0,0,22,0,0,0,3,18,68,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,166,150,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,112,130,2,0,1,17,1,218,163,2,0,1,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,34, 
  208,64,128,92,216,0,128,1,3,17,1,194,130,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,249,150,2, 
  0,1,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,249,150,2,0,1,2,17,1,104,109,2,0,1,21,4,41, 
  0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,34,208,64,128,92,8,1,128,3,18,135,0,0,0,1,3,17,1,121,131,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,129,151,2,0,1,17,1,104,109,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0, 
  0,38,0,0,0,38,0,0,0,3,17,1,129,151,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,92,0,0,0,22,0,0,0,3,17,1,14,152,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0, 
  0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,251,131,2,0,1,17,1,9,164,2,0,1,21,4,35,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92, 
  216,0,128,39,208,0,128,1,3,17,1,77,132,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,97,152,2, 
  0,1,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,97,152,2,0,1,2,17,1,49,110,2,0,1,21,4,41, 
  0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,92,8,1,128,39,208,0,128,3,18,125,0,0,0,1,3,17,1,4,133,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,233,152,2,0,1,17,1,49,110,2,0,1,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0, 
  0,38,0,0,0,38,0,0,0,3,17,1,233,152,2,0,1,2,21,4,65,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,114,136,1,128,97,72,1,128,58,16,129,127,115,200,1,128,3,18,29,0,0,0,1,3, 
  17,1,217,135,2,0,1,3,17,1,24,105,2,0,1,3,17,1,55,105,2,0,1,2,21,4,150,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,130,34,128,2,128,10,16,194,127,99,248,3,130,116,56, 
  4,128,45,56,3,128,94,184,3,128,39,192,130,128,40,0,3,128,47,120,67,128,95,56,3,128,123,120,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1, 
  3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,20,2,30,0,0,0, 
  0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,195,0,0,0,0,0,0,0,16,0,0,0,4,0,0,0,32,200,2,128,58,112,4,128,34,0,3,128,99,96,5,128,36, 
  64,67,129,63,176,68,128,95,240,3,128,39,120,3,128,40,184,3,128,116,160,5,128,10,144,194,125,91,232,4,128,124,224,5,128,45,240,3,128,94,32,5,128,47,48,132,125,3,18,5,0,0,0,1,3,18,6,0,0,0, 
  1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,87,0,0, 
  0,1,3,18,123,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0, 
  0,0,3,17,1,94,74,2,0,1,2,21,4,205,0,0,0,0,0,0,0,17,0,0,0,4,0,0,0,32,232,2,128,58,200,4,128,34,32,3,128,99,120,5,128,36,96,131,130,63,8,69,128,95,72,4,128,39,152,3, 
  128,40,216,3,128,41,16,4,128,10,176,194,125,91,64,69,129,124,48,6,128,45,72,4,128,116,184,5,128,47,136,132,125,123,248,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3, 
  18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,18,92,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,87,0,0,0,1,3, 
  18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17, 
  1,94,74,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,99,0,0,0,1,2,21,4,64,0,0,0,0,0,0,0,4,0,0,0,2,0,0,0,107,72,65,128,111, 
  128,1,128,114,192,1,128,95,16,65,127,3,18,45,0,0,0,1,3,18,76,0,0,0,1,3,17,1,196,79,2,0,1,3,17,1,227,79,2,0,1,2,20,2,30,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0, 
  22,0,0,0,3,17,1,154,156,2,0,1,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,3,18,20,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0, 
  0,114,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,82,169,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107, 
  2,0,1,18,134,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,112,0,0,0,22,0,0,0,3,17,1,168,169,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,114,0,0,0,22,0, 
  0,0,3,17,1,198,169,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,3,17,1,228,169,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0, 
  22,0,0,0,3,18,84,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,3,17,1,2,170,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,45,0,0, 
  0,22,0,0,0,3,17,1,33,170,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,99,0,0,0,22,0,0,0,3,17,1,64,170,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116, 
  0,0,0,22,0,0,0,3,18,55,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,96,1,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3, 
  17,1,94,170,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0, 
  0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,180,170,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,105,96,1, 
  128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,22,171,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93, 
  2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,108,171,2,0, 
  1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0, 
  0,1,0,0,0,116,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,194,171,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0, 
  0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1,128,15,1,79,84,2, 
  0,3,17,1,164,93,2,0,1,3,17,1,24,172,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0, 
  0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,96,1,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,110,172,2,0,1,20,2,40,0,0, 
  0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,100, 
  96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,196,172,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15, 
  1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93, 
  2,0,1,3,17,1,26,173,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,20,4,30, 
  0,0,0,0,0,0,0,1,0,0,0,115,0,0,0,22,0,0,0,3,17,1,250,123,2,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,79,96,129,128,45,240,192,127,95,240,0,128,15,1, 
  79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,66,173,2,0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1, 
  18,134,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,82,96,1,128,45,240,64,128,95,240,0,128,15,1,79,84,2,0,3,17,1,164,93,2,0,1,3,17,1,152,173,2,0,1,20,2, 
  40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,79,84,2,0,3,17,1,164,93,2,0,1,18,134,0,0,0,1,21,4,86,0,0,0,0,0,0,0,6,0,0,0,2,0, 
  0,0,32,136,65,128,40,192,193,128,10,80,193,128,123,120,2,128,100,248,1,128,102,56,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,108,0,0,0,1,3,17,1,238,173,2,0,1,3,17,1,13,174, 
  2,0,1,3,18,89,0,0,0,1,2,21,4,88,0,0,0,0,0,0,0,6,0,0,0,2,0,0,0,32,136,65,129,34,192,1,128,10,80,193,127,39,0,66,128,99,64,2,128,116,128,2,128,3,18,5,0,0,0,1, 
  3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,17,1,27,102,2,0,1,3,17,1,57,102,2,0,1,2,21,4,45,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,110,48, 
  1,128,45,240,64,128,95,240,0,128,3,17,1,19,149,2,0,1,3,18,79,0,0,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,19,149,2,0,1,18, 
  126,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,82,0,0,0,22,0,0,0,3,17,1,44,174,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,82,0,0,0,22,0,0,0,3,17, 
  1,74,174,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,82,0,0,0,22,0,0,0,3,17,1,104,174,2,0,1,2,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0, 
  38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,3,17,1,112,130,2,0,1,2,20,2,46,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38,0,0,0,38, 
  0,0,0,3,17,1,251,131,2,0,1,2,21,4,150,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,2,130,34,128,2,128,10,16,194,127,99,48,4,128,116,112,4,128,45,56,67,129,94,240,3,128,39,192, 
  130,128,40,0,3,128,47,120,131,128,93,184,3,128,95,56,3,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74, 
  2,0,1,3,17,1,51,74,2,0,1,3,18,30,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0, 
  22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,149,0,0,0,0,0,0,0,12,0,0,0,3,0,0,0,32,72,130,129,34,128,2,128,10,16,194,127,91,184,195,129,116,48,4,128,45,56,3,128,40,0,3,128,39, 
  192,66,128,47,120,67,128,95,56,3,128,99,240,67,128,123,112,4,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94, 
  74,2,0,1,3,17,1,51,74,2,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0, 
  22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,184,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,2,130,34,224,66,130,10,112,194,127,91,144,68,130,36,32,67,130,45,208,3,128,94,200,4,128,39, 
  88,131,128,40,152,3,128,47,16,132,128,58,80,4,128,95,208,3,128,99,8,5,128,116,72,69,128,124,136,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1, 
  3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,123,0,0,0,1,3,17,1,194,101,2,0,1,3,17,1,124,96,2, 
  0,1,3,17,1,221,96,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,183,0,0,0,0,0,0,0,15,0, 
  0,0,3,0,0,0,32,168,130,129,34,224,2,130,10,112,194,127,91,200,68,130,36,32,67,130,45,208,3,128,40,152,3,128,39,88,67,128,47,16,132,128,58,80,4,128,63,144,68,128,95,208,3,128,99,0,5,128,116,64, 
  69,128,124,128,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17, 
  1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,87,0,0,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0, 
  0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,183,0,0,0,0,0,0,0,15,0,0,0,3,0,0,0,32,168,130,129,34,224,2,130,10,112,194,127,91,144,4,130,36,32,3,130, 
  45,208,3,128,40,152,3,128,39,88,67,128,47,16,132,128,58,80,4,128,95,208,3,128,99,200,132,128,116,8,133,128,123,72,5,128,124,128,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2, 
  0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2,0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,123,0,0,0,1,3,17,1,124,96, 
  2,0,1,3,17,1,221,96,2,0,1,3,18,89,0,0,0,1,3,18,102,0,0,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,172,0, 
  0,0,0,0,0,0,14,0,0,0,3,0,0,0,32,136,130,129,34,192,2,130,10,80,194,127,91,168,68,130,36,0,67,130,45,176,3,128,40,120,3,128,39,56,67,128,47,240,131,128,58,48,4,128,63,112,68,128,95,176, 
  3,128,99,224,4,128,116,32,5,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,214,95,2,0,1,3,18,106,0,0,0,1,3,17,1,41,96,2,0,1,3,18,108,0,0,0,1,3,17,1,94,74,2, 
  0,1,3,17,1,51,74,2,0,1,3,17,1,225,115,2,0,1,3,18,87,0,0,0,1,3,18,123,0,0,0,1,3,17,1,124,96,2,0,1,3,17,1,221,96,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0, 
  0,0,3,0,0,0,22,0,0,0,3,17,1,94,74,2,0,1,2,21,4,68,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,32,104,1,128,45,160,1,128,10,48,129,128,95,160,1,128,114,224,1,128,3,18,5, 
  0,0,0,1,3,18,6,0,0,0,1,3,17,1,74,91,2,0,1,3,17,1,134,174,2,0,1,20,2,30,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,22,0,0,0,3,17,1,74,91,2,0,1,2,21,4, 
  46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,100,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,29,175,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0, 
  0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,80,0,0,0,1,2,20,4,29,0,0,0, 
  0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,54,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,18,39,0,0,0,1,2,20,4,30,0,0,0, 
  0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,17,1,115,175,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,3,17,1,146,175,2,0,1,2,20,4,29,0, 
  0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0,3,18,44,0,0,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,139, 
  107,2,0,1,3,17,1,177,175,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,58,0,0,0,0,0, 
  0,0,4,0,0,0,2,0,0,0,101,80,129,128,45,16,193,127,105,144,1,128,95,16,1,128,3,17,1,139,107,2,0,1,3,17,1,7,176,2,0,1,3,17,1,93,176,2,0,1,20,2,34,0,0,0,0,0,0,0, 
  2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,108,48,1,128,45,240,64,128,95,240,0,128, 
  3,17,1,139,107,2,0,1,3,17,1,179,176,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0, 
  0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,48,1,128,3,17,1,139,107,2,0,1,3,17,1,219,176,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,48,1,128,3,17,1,139,107,2,0,1, 
  3,17,1,3,177,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0, 
  0,0,1,0,0,0,95,240,128,128,45,240,192,127,115,48,1,128,3,17,1,139,107,2,0,1,3,17,1,89,177,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0, 
  3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,129,177,2,0,1, 
  20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240, 
  128,128,45,240,192,127,117,48,1,128,3,17,1,139,107,2,0,1,3,17,1,211,177,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1, 
  18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,83,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0, 
  1,0,0,0,68,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,41,178,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17, 
  1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,77,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,127,178,2,0,1,20,2, 
  34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,101,0,0,0,22,0,0,0, 
  3,17,1,213,178,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,244,178,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,84,0,0,0,22,0, 
  0,0,3,18,49,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,69,0,0,0,22,0,0,0,3,18,67,0,0,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,84,0,0,0,22,0, 
  0,0,3,18,78,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,103,96,1,128,15,1,101,106,2,0,3,17,1,86,105,2,0,1,3,17,1,170,179,2, 
  0,1,20,2,40,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,15,1,101,106,2,0,3,17,1,86,105,2,0,1,18,126,0,0,0,1,21,4,52,0,0,0,0,0,0,0,3,0, 
  0,0,1,0,0,0,10,240,64,128,32,40,65,128,114,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1,255,179,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95, 
  240,128,128,45,240,192,127,101,48,1,128,3,17,1,139,107,2,0,1,3,17,1,30,180,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0, 
  1,18,134,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,116,180,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0, 
  3,17,1,146,180,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,112,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,177,180,2,0,1,20,2,34,0,0, 
  0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,48,1,128,45,240,64, 
  128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,7,181,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0, 
  1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,103,48,1,128,3,17,1,139,107,2,0,1,3,17,1,93,181,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0, 
  0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0, 
  1,18,66,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,60,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0, 
  0,1,0,0,0,104,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,179,181,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3, 
  17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,73,0,0,0,1,21,4,42,0,0,0,0, 
  0,0,0,2,0,0,0,1,0,0,0,95,16,1,128,45,208,192,127,3,17,1,219,181,2,0,1,3,17,1,139,107,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0, 
  0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,99,48,1,128,3,17,1,139,107,2,0,1,3,17,1,63,182,2,0, 
  1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,85, 
  48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,149,182,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0, 
  1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,73,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,235,182,2,0,1,20,2,34,0,0,0,0, 
  0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,102,0,0,0,22,0,0,0,3,17,1,65,183, 
  2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22,0,0,0,3,17,1,96,183,2,0,1,2,21,4,97,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,32,168,1,129,125,208,2, 
  128,10,112,1,129,123,152,2,128,40,224,65,128,100,24,2,128,102,88,2,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,18,108,0,0,0,1,3,17,1,238,173,2,0,1,3,17,1,13,174,2,0,1,3,18, 
  89,0,0,0,1,3,18,97,0,0,0,1,2,21,4,52,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,10,240,64,128,32,40,65,128,34,96,1,128,3,18,5,0,0,0,1,3,18,6,0,0,0,1,3,17,1, 
  214,95,2,0,1,2,21,4,45,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,98,48,1,128,45,240,64,128,95,240,0,128,3,17,1,19,149,2,0,1,3,18,47,0,0,0,1,20,2,34,0,0,0,0,0,0, 
  0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,19,149,2,0,1,18,126,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,103,0,0,0,22,0,0,0,3,17,1,127,183,2,0, 
  1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,114,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,157,183,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0, 
  0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,18,43,0,0,0,1,2,20,4, 
  30,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,17,1,243,183,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3, 
  17,1,139,107,2,0,1,3,17,1,17,184,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0, 
  0,0,0,0,0,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,57,184,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26, 
  0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,110,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3, 
  17,1,97,184,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0, 
  0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,84,0,0,0,1,21,4,50,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,103,80,1,128,18,134,0, 
  0,0,17,1,137,184,2,0,1,3,17,1,149,184,2,0,1,20,2,38,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,18,134,0,0,0,17,1,137,184,2,0,1,18,134,0,0,0, 
  17,1,137,184,2,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,139,107,2,0,1,3,17,1,230,184,2,0,1,20,2,34,0,0,0,0, 
  0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,67,48,129,128,45,240,192,127,95, 
  240,0,128,3,17,1,139,107,2,0,1,3,17,1,14,185,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21, 
  4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,78,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,100,185,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3, 
  0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,97,0,0,0,22,0,0,0,3,17,1,186,185,2,0,1,2,20,4,30,0, 
  0,0,0,0,0,0,1,0,0,0,108,0,0,0,22,0,0,0,3,17,1,217,185,2,0,1,2,20,4,29,0,0,0,0,0,0,0,1,0,0,0,98,0,0,0,22,0,0,0,3,18,47,0,0,0,1,2,21,4,46, 
  0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,101,48,1,128,3,17,1,139,107,2,0,1,3,17,1,248,185,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0, 
  0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,111,0,0,0,22,0,0,0,3,18,53,0,0,0,1,2,21,4,34,0,0,0,0, 
  0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,80,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3, 
  17,1,139,107,2,0,1,18,54,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,39,0,0,0,1,18,134,0,0,0,17,1, 
  78,186,2,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,48,1,128,3,17,1,139,107,2,0,1,3,17,1,147,186,2,0,1,20,2,34,0,0,0,0,0,0, 
  0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1, 
  18,44,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,84,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,228,186,2,0,1,20,2,34,0,0,0,0,0, 
  0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,65,48,129,128,45,240,192,127,95,240, 
  0,128,3,17,1,139,107,2,0,1,3,17,1,58,187,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4, 
  30,0,0,0,0,0,0,0,1,0,0,0,117,0,0,0,22,0,0,0,3,17,1,144,187,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,45,0,0,0,22,0,0,0,3,17,1,175,187,2,0,1,2, 
  21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,100,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,206,187,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0, 
  3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1, 
  20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,2,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,116,48,1,128,45,240,64, 
  128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,246,187,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,1,21,4,46,0, 
  0,0,0,0,0,0,3,0,0,0,1,0,0,0,73,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,71,188,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0, 
  26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,76,48,1,128,45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1, 
  3,17,1,157,188,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0, 
  0,0,108,0,0,0,22,0,0,0,3,17,1,197,188,2,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,104,0,0,0,22,0,0,0,3,17,1,227,188,2,0,1,2,21,4,34,0,0,0,0,0,0,0, 
  2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,37,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,95,240,128,128,45,240,192,127,111,48,1,128, 
  3,17,1,139,107,2,0,1,3,17,1,2,189,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,1,21,4,46,0,0,0,0,0,0, 
  0,3,0,0,0,1,0,0,0,79,48,129,128,45,240,192,127,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,42,189,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26, 
  0,0,0,3,17,1,139,107,2,0,1,18,134,0,0,0,1,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,51,0,0,0,1,20,4,29, 
  0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,48,0,0,0,1,2,20,4,30,0,0,0,0,0,0,0,1,0,0,0,105,0,0,0,22,0,0,0,3,17,1,128,189,2,0,1,2,21,4, 
  34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192,127,3,17,1,139,107,2,0,1,18,53,0,0,0,1,21,4,46,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,78,48,1,128, 
  45,240,64,128,95,240,0,128,3,17,1,139,107,2,0,1,3,17,1,159,189,2,0,1,20,2,34,0,0,0,0,0,0,0,2,0,0,0,3,0,0,0,26,0,0,0,26,0,0,0,3,17,1,139,107,2,0,1,18,134, 
  0,0,0,1,20,4,30,0,0,0,0,0,0,0,1,0,0,0,110,0,0,0,22,0,0,0,3,17,1,199,189,2,0,1,2,21,4,34,0,0,0,0,0,0,0,2,0,0,0,1,0,0,0,95,208,0,128,45,208,192, 
  127,3,17,1,139,107,2,0,1,18,74,0,0,0,1,20,4,29,0,0,0,0,0,0,0,1,0,0,0,116,0,0,0,22,0,0,0,3,18,62,0,0,0,1,2, 
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