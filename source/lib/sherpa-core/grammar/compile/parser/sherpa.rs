use sherpa_runtime::{
  llvm_parser::*,
  types::{ast::*, *},
};
/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta1
/// - **SOURCE**: /home/work/projects/lib_sherpa/source/grammar/v1_0_0/grammar.sg
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

impl Grammar {
  /// Create a [Grammar] node from a `String` input.

  pub fn from_string(input: String) -> Result<Box<Grammar>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::grammar_from(reader)
  }
}

impl Grammar {
  /// Create a [Grammar] node from a `String` input.

  pub fn from_str(input: &str) -> Result<Box<Grammar>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::grammar_from(reader)
  }
}

impl AST_Struct {
  /// Create a [AST_Struct] node from a `String` input.

  pub fn from_string(input: String) -> Result<Box<AST_Struct>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ast_struct_from(reader)
  }
}

impl AST_Struct {
  /// Create a [AST_Struct] node from a `String` input.

  pub fn from_str(input: &str) -> Result<Box<AST_Struct>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ast_struct_from(reader)
  }
}

impl IR_STATE {
  /// Create a [IR_STATE] node from a `String` input.

  pub fn from_string(input: String) -> Result<Box<IR_STATE>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ir_from(reader)
  }
}

impl IR_STATE {
  /// Create a [IR_STATE] node from a `String` input.

  pub fn from_str(input: &str) -> Result<Box<IR_STATE>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ir_from(reader)
  }
}

pub trait ASTParse<T> {
  fn grammar_from(input: T) -> Result<Box<Grammar>, SherpaParseError>;
  fn ast_struct_from(input: T) -> Result<Box<AST_Struct>, SherpaParseError>;
  fn ast_expression_from(input: T) -> Result<ASTNode, SherpaParseError>;
  fn ir_from(input: T) -> Result<Box<IR_STATE>, SherpaParseError>;
  fn type_eval_from(input: T) -> Result<ASTNode, SherpaParseError>;
  fn escaped_from(input: T) -> Result<Vec<String>, SherpaParseError>;
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
pub enum ASTNode {
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
  PeekToken(Box<PeekToken>),
  EOFSymbol(Box<EOFSymbol>),
  DEFAULT(Box<DEFAULT>),
  PushGoto(Box<PushGoto>),
  TokenAssign(Box<TokenAssign>),
  SkipToken(Box<SkipToken>),
  AST_Add(Box<AST_Add>),
  ScanShift(Box<ScanShift>),
  ShiftToken(Box<ShiftToken>),
  Syntax(Box<Syntax>),
  SyntaxField(Box<SyntaxField>),
  HASH_NAME(Box<HASH_NAME>),
  List_Production(Box<List_Production>),
  PeekReset(Box<PeekReset>),
  Pop(Box<Pop>),
  AST_Vector(Box<AST_Vector>),
  Production_Symbol(Box<Production_Symbol>),
  SkipTokenScanless(Box<SkipTokenScanless>),
  Grammar(Box<Grammar>),
  AST_U64(Box<AST_U64>),
  DEFINED_TYPE_IDENT(Box<DEFINED_TYPE_IDENT>),
  Priority(Box<Priority>),
  Pass(Box<Pass>),
  ASSERT(Box<ASSERT>),
  Import(Box<Import>),
  Init(Box<Init>),
  AST_IndexReference(Box<AST_IndexReference>),
  RGBA(Box<RGBA>),
  AST_Member(Box<AST_Member>),
  Production_Import_Symbol(Box<Production_Import_Symbol>),
  AST_I32(Box<AST_I32>),
  Goto(Box<Goto>),
  SkipPeekToken(Box<SkipPeekToken>),
  AST_I8(Box<AST_I8>),
  AST_F32(Box<AST_F32>),
  Symbols(Box<Symbols>),
  AST_Token(Box<AST_Token>),
  Export(Box<Export>),
  Name(Box<Name>),
  AST_BOOL(Box<AST_BOOL>),
  Group_Production(Box<Group_Production>),
  Fail(Box<Fail>),
  AST_Statements(Box<AST_Statements>),
  AST_U8(Box<AST_U8>),
  Production(Box<Production>),
  ShiftTokenScanless(Box<ShiftTokenScanless>),
  PushExceptHandler(Box<PushExceptHandler>),
  Ignore(Box<Ignore>),
  AST_STRING(Box<AST_STRING>),
  AST_U32(Box<AST_U32>),
  AST_ClassId(Box<AST_ClassId>),
  IR_STATE(Box<IR_STATE>),
  AST_I64(Box<AST_I64>),
  PeekTokenScanless(Box<PeekTokenScanless>),
  AST_U16(Box<AST_U16>),
  SkipPeekTokenScanless(Box<SkipPeekTokenScanless>),
  Accept(Box<Accept>),
  ClassSymbol(Box<ClassSymbol>),
  AST_F64(Box<AST_F64>),
  AST_Map(Box<AST_Map>),
  AnyGroup(Box<AnyGroup>),
  Range(Box<Range>),
  AST_I16(Box<AST_I16>),
  AST_NUMBER(Box<AST_NUMBER>),
  AST_Struct(Box<AST_Struct>),
  DEFINED_TYPE_NUM(Box<DEFINED_TYPE_NUM>),
  SyntaxSpec(Box<SyntaxSpec>),
  Rule(Box<Rule>),
  AnnotatedSymbol(Box<AnnotatedSymbol>),
  AST_Property(Box<AST_Property>),
  Ascript(Box<Ascript>),
  Num(Box<Num>),
  Production_Terminal_Symbol(Box<Production_Terminal_Symbol>),
  Terminal(Box<Terminal>),
  Recovery(Box<Recovery>),
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum ASTNodeType {
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
  PeekToken,
  EOFSymbol,
  DEFAULT,
  PushGoto,
  TokenAssign,
  SkipToken,
  AST_Add,
  ScanShift,
  ShiftToken,
  Syntax,
  SyntaxField,
  HASH_NAME,
  List_Production,
  PeekReset,
  Pop,
  AST_Vector,
  Production_Symbol,
  SkipTokenScanless,
  Grammar,
  AST_U64,
  DEFINED_TYPE_IDENT,
  Priority,
  Pass,
  ASSERT,
  Import,
  Init,
  AST_IndexReference,
  RGBA,
  AST_Member,
  Production_Import_Symbol,
  AST_I32,
  Goto,
  SkipPeekToken,
  AST_I8,
  AST_F32,
  Symbols,
  AST_Token,
  Export,
  Name,
  AST_BOOL,
  Group_Production,
  Fail,
  AST_Statements,
  AST_U8,
  Production,
  ShiftTokenScanless,
  PushExceptHandler,
  Ignore,
  AST_STRING,
  AST_U32,
  AST_ClassId,
  IR_STATE,
  AST_I64,
  PeekTokenScanless,
  AST_U16,
  SkipPeekTokenScanless,
  Accept,
  ClassSymbol,
  AST_F64,
  AST_Map,
  AnyGroup,
  Range,
  AST_I16,
  AST_NUMBER,
  AST_Struct,
  DEFINED_TYPE_NUM,
  SyntaxSpec,
  Rule,
  AnnotatedSymbol,
  AST_Property,
  Ascript,
  Num,
  Production_Terminal_Symbol,
  Terminal,
  Recovery,
}

impl ASTNode {
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

  pub fn is_numeric(&self) -> bool {
    use ASTNode::*;
    matches!(
      self,
      F64(_) | F32(_) | I64(_) | I32(_) | I16(_) | I8(_) | U64(_) | U32(_) | U16(_) | U8(_)
    )
  }

  pub fn to_bool(&self) -> bool {
    self.to_u8() != 0
  }

  pub fn into_strings(self) -> Vec<String> {
    match self {
      ASTNode::STRINGS(strings) => strings,
      _ => Default::default(),
    }
  }

  pub fn to_string(&self) -> String {
    match self {
      ASTNode::BOOL(val) => val.to_string(),
      ASTNode::STRING(string) => string.to_owned(),
      ASTNode::TOKEN(val) => val.to_string(),
      _ => self.to_token().to_string(),
    }
  }

  pub fn to_token(&self) -> Token {
    match self {
      ASTNode::AST_NamedReference(node) => node.tok.clone(),
      ASTNode::EOFSymbol(node) => node.tok.clone(),
      ASTNode::AST_Add(node) => node.tok.clone(),
      ASTNode::List_Production(node) => node.tok.clone(),
      ASTNode::AST_Vector(node) => node.tok.clone(),
      ASTNode::Production_Symbol(node) => node.tok.clone(),
      ASTNode::Grammar(node) => node.tok.clone(),
      ASTNode::AST_U64(node) => node.tok.clone(),
      ASTNode::Import(node) => node.tok.clone(),
      ASTNode::AST_IndexReference(node) => node.tok.clone(),
      ASTNode::Production_Import_Symbol(node) => node.tok.clone(),
      ASTNode::AST_I32(node) => node.tok.clone(),
      ASTNode::AST_I8(node) => node.tok.clone(),
      ASTNode::AST_F32(node) => node.tok.clone(),
      ASTNode::AST_BOOL(node) => node.tok.clone(),
      ASTNode::Group_Production(node) => node.tok.clone(),
      ASTNode::AST_Statements(node) => node.tok.clone(),
      ASTNode::AST_U8(node) => node.tok.clone(),
      ASTNode::Production(node) => node.tok.clone(),
      ASTNode::AST_STRING(node) => node.tok.clone(),
      ASTNode::AST_U32(node) => node.tok.clone(),
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
      ASTNode::Production_Terminal_Symbol(node) => node.tok.clone(),
      ASTNode::Terminal(node) => node.tok.clone(),
      ASTNode::Recovery(node) => node.tok.clone(),
      ASTNode::TOKEN(val) => val.to_owned(),
      _ => Token::empty(),
    }
  }
}

pub trait GetASTNodeType {
  fn get_type(&self) -> ASTNodeType;
}

impl GetASTNodeType for ASTNode {
  fn get_type(&self) -> ASTNodeType {
    match self {
      ASTNode::AST_NamedReference(..) => ASTNodeType::AST_NamedReference,
      ASTNode::Reduce(..) => ASTNodeType::Reduce,
      ASTNode::PeekToken(..) => ASTNodeType::PeekToken,
      ASTNode::EOFSymbol(..) => ASTNodeType::EOFSymbol,
      ASTNode::DEFAULT(..) => ASTNodeType::DEFAULT,
      ASTNode::PushGoto(..) => ASTNodeType::PushGoto,
      ASTNode::TokenAssign(..) => ASTNodeType::TokenAssign,
      ASTNode::SkipToken(..) => ASTNodeType::SkipToken,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::ScanShift(..) => ASTNodeType::ScanShift,
      ASTNode::ShiftToken(..) => ASTNodeType::ShiftToken,
      ASTNode::Syntax(..) => ASTNodeType::Syntax,
      ASTNode::SyntaxField(..) => ASTNodeType::SyntaxField,
      ASTNode::HASH_NAME(..) => ASTNodeType::HASH_NAME,
      ASTNode::List_Production(..) => ASTNodeType::List_Production,
      ASTNode::PeekReset(..) => ASTNodeType::PeekReset,
      ASTNode::Pop(..) => ASTNodeType::Pop,
      ASTNode::AST_Vector(..) => ASTNodeType::AST_Vector,
      ASTNode::Production_Symbol(..) => ASTNodeType::Production_Symbol,
      ASTNode::SkipTokenScanless(..) => ASTNodeType::SkipTokenScanless,
      ASTNode::Grammar(..) => ASTNodeType::Grammar,
      ASTNode::AST_U64(..) => ASTNodeType::AST_U64,
      ASTNode::DEFINED_TYPE_IDENT(..) => ASTNodeType::DEFINED_TYPE_IDENT,
      ASTNode::Priority(..) => ASTNodeType::Priority,
      ASTNode::Pass(..) => ASTNodeType::Pass,
      ASTNode::ASSERT(..) => ASTNodeType::ASSERT,
      ASTNode::Import(..) => ASTNodeType::Import,
      ASTNode::Init(..) => ASTNodeType::Init,
      ASTNode::AST_IndexReference(..) => ASTNodeType::AST_IndexReference,
      ASTNode::RGBA(..) => ASTNodeType::RGBA,
      ASTNode::AST_Member(..) => ASTNodeType::AST_Member,
      ASTNode::Production_Import_Symbol(..) => ASTNodeType::Production_Import_Symbol,
      ASTNode::AST_I32(..) => ASTNodeType::AST_I32,
      ASTNode::Goto(..) => ASTNodeType::Goto,
      ASTNode::SkipPeekToken(..) => ASTNodeType::SkipPeekToken,
      ASTNode::AST_I8(..) => ASTNodeType::AST_I8,
      ASTNode::AST_F32(..) => ASTNodeType::AST_F32,
      ASTNode::Symbols(..) => ASTNodeType::Symbols,
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::AST_BOOL(..) => ASTNodeType::AST_BOOL,
      ASTNode::Group_Production(..) => ASTNodeType::Group_Production,
      ASTNode::Fail(..) => ASTNodeType::Fail,
      ASTNode::AST_Statements(..) => ASTNodeType::AST_Statements,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::Production(..) => ASTNodeType::Production,
      ASTNode::ShiftTokenScanless(..) => ASTNodeType::ShiftTokenScanless,
      ASTNode::PushExceptHandler(..) => ASTNodeType::PushExceptHandler,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_STRING(..) => ASTNodeType::AST_STRING,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::AST_ClassId(..) => ASTNodeType::AST_ClassId,
      ASTNode::IR_STATE(..) => ASTNodeType::IR_STATE,
      ASTNode::AST_I64(..) => ASTNodeType::AST_I64,
      ASTNode::PeekTokenScanless(..) => ASTNodeType::PeekTokenScanless,
      ASTNode::AST_U16(..) => ASTNodeType::AST_U16,
      ASTNode::SkipPeekTokenScanless(..) => ASTNodeType::SkipPeekTokenScanless,
      ASTNode::Accept(..) => ASTNodeType::Accept,
      ASTNode::ClassSymbol(..) => ASTNodeType::ClassSymbol,
      ASTNode::AST_F64(..) => ASTNodeType::AST_F64,
      ASTNode::AST_Map(..) => ASTNodeType::AST_Map,
      ASTNode::AnyGroup(..) => ASTNodeType::AnyGroup,
      ASTNode::Range(..) => ASTNodeType::Range,
      ASTNode::AST_I16(..) => ASTNodeType::AST_I16,
      ASTNode::AST_NUMBER(..) => ASTNodeType::AST_NUMBER,
      ASTNode::AST_Struct(..) => ASTNodeType::AST_Struct,
      ASTNode::DEFINED_TYPE_NUM(..) => ASTNodeType::DEFINED_TYPE_NUM,
      ASTNode::SyntaxSpec(..) => ASTNodeType::SyntaxSpec,
      ASTNode::Rule(..) => ASTNodeType::Rule,
      ASTNode::AnnotatedSymbol(..) => ASTNodeType::AnnotatedSymbol,
      ASTNode::AST_Property(..) => ASTNodeType::AST_Property,
      ASTNode::Ascript(..) => ASTNodeType::Ascript,
      ASTNode::Num(..) => ASTNodeType::Num,
      ASTNode::Production_Terminal_Symbol(..) => ASTNodeType::Production_Terminal_Symbol,
      ASTNode::Terminal(..) => ASTNodeType::Terminal,
      ASTNode::Recovery(..) => ASTNodeType::Recovery,
      _ => ASTNodeType::NONE,
    }
  }
}

impl Default for ASTNode {
  fn default() -> Self {
    ASTNode::NONE
  }
}

impl Hash for ASTNode {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    use ASTNode::*;

    match self {
      NONE => {}
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
      PeekToken(node) => node.hash(hasher),
      EOFSymbol(node) => node.hash(hasher),
      DEFAULT(node) => node.hash(hasher),
      PushGoto(node) => node.hash(hasher),
      TokenAssign(node) => node.hash(hasher),
      SkipToken(node) => node.hash(hasher),
      AST_Add(node) => node.hash(hasher),
      ScanShift(node) => node.hash(hasher),
      ShiftToken(node) => node.hash(hasher),
      Syntax(node) => node.hash(hasher),
      SyntaxField(node) => node.hash(hasher),
      HASH_NAME(node) => node.hash(hasher),
      List_Production(node) => node.hash(hasher),
      PeekReset(node) => node.hash(hasher),
      Pop(node) => node.hash(hasher),
      AST_Vector(node) => node.hash(hasher),
      Production_Symbol(node) => node.hash(hasher),
      SkipTokenScanless(node) => node.hash(hasher),
      Grammar(node) => node.hash(hasher),
      AST_U64(node) => node.hash(hasher),
      DEFINED_TYPE_IDENT(node) => node.hash(hasher),
      Priority(node) => node.hash(hasher),
      Pass(node) => node.hash(hasher),
      ASSERT(node) => node.hash(hasher),
      Import(node) => node.hash(hasher),
      Init(node) => node.hash(hasher),
      AST_IndexReference(node) => node.hash(hasher),
      RGBA(node) => node.hash(hasher),
      AST_Member(node) => node.hash(hasher),
      Production_Import_Symbol(node) => node.hash(hasher),
      AST_I32(node) => node.hash(hasher),
      Goto(node) => node.hash(hasher),
      SkipPeekToken(node) => node.hash(hasher),
      AST_I8(node) => node.hash(hasher),
      AST_F32(node) => node.hash(hasher),
      Symbols(node) => node.hash(hasher),
      AST_Token(node) => node.hash(hasher),
      Export(node) => node.hash(hasher),
      Name(node) => node.hash(hasher),
      AST_BOOL(node) => node.hash(hasher),
      Group_Production(node) => node.hash(hasher),
      Fail(node) => node.hash(hasher),
      AST_Statements(node) => node.hash(hasher),
      AST_U8(node) => node.hash(hasher),
      Production(node) => node.hash(hasher),
      ShiftTokenScanless(node) => node.hash(hasher),
      PushExceptHandler(node) => node.hash(hasher),
      Ignore(node) => node.hash(hasher),
      AST_STRING(node) => node.hash(hasher),
      AST_U32(node) => node.hash(hasher),
      AST_ClassId(node) => node.hash(hasher),
      IR_STATE(node) => node.hash(hasher),
      AST_I64(node) => node.hash(hasher),
      PeekTokenScanless(node) => node.hash(hasher),
      AST_U16(node) => node.hash(hasher),
      SkipPeekTokenScanless(node) => node.hash(hasher),
      Accept(node) => node.hash(hasher),
      ClassSymbol(node) => node.hash(hasher),
      AST_F64(node) => node.hash(hasher),
      AST_Map(node) => node.hash(hasher),
      AnyGroup(node) => node.hash(hasher),
      Range(node) => node.hash(hasher),
      AST_I16(node) => node.hash(hasher),
      AST_NUMBER(node) => node.hash(hasher),
      AST_Struct(node) => node.hash(hasher),
      DEFINED_TYPE_NUM(node) => node.hash(hasher),
      SyntaxSpec(node) => node.hash(hasher),
      Rule(node) => node.hash(hasher),
      AnnotatedSymbol(node) => node.hash(hasher),
      AST_Property(node) => node.hash(hasher),
      Ascript(node) => node.hash(hasher),
      Num(node) => node.hash(hasher),
      Production_Terminal_Symbol(node) => node.hash(hasher),
      Terminal(node) => node.hash(hasher),
      Recovery(node) => node.hash(hasher),

      TOKEN(tk) => {
        tk.to_string().replace(" ", "").replace("\n", "").hash(hasher);
      }

      TOKENS(tks) => {
        for tk in tks {
          tk.to_string().replace(" ", "").replace("\n", "").hash(hasher);
        }
      }

      NODES(nodes) => {
        for node in nodes {
          node.hash(hasher);
        }
      }

      F32Vec(vals) => {
        for v in vals {
          v.to_le_bytes().hash(hasher);
        }
      }

      F64Vec(vals) => {
        for v in vals {
          v.to_le_bytes().hash(hasher);
        }
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct AST_NamedReference {
  pub value: String,
  pub tok:   Token,
}

impl AST_NamedReference {
  pub fn new(value: String, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_NamedReference
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_NamedReference(self) -> Box<AST_NamedReference> {
    match self {
      Self::AST_NamedReference(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_NamedReference", self.get_type()),
    }
  }

  pub fn as_AST_NamedReference(&self) -> Option<&AST_NamedReference> {
    match self {
      Self::AST_NamedReference(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_NamedReference_mut(&mut self) -> Option<&mut AST_NamedReference> {
    match self {
      Self::AST_NamedReference(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_NamedReference {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Reduce {
  pub len:     i32,
  pub prod_id: i32,
  pub rule_id: i32,
}

impl Reduce {
  pub fn new(len: i32, prod_id: i32, rule_id: i32) -> Self {
    Self { len, prod_id, rule_id }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Reduce
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Reduce(self) -> Box<Reduce> {
    match self {
      Self::Reduce(val) => val,
      _ => panic!("Type {:?} cannot be converted to Reduce", self.get_type()),
    }
  }

  pub fn as_Reduce(&self) -> Option<&Reduce> {
    match self {
      Self::Reduce(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Reduce_mut(&mut self) -> Option<&mut Reduce> {
    match self {
      Self::Reduce(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Reduce {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.len.hash(hasher);
    self.prod_id.hash(hasher);
    self.rule_id.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct PeekToken {}

impl PeekToken {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PeekToken
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_PeekToken(self) -> Box<PeekToken> {
    match self {
      Self::PeekToken(val) => val,
      _ => panic!("Type {:?} cannot be converted to PeekToken", self.get_type()),
    }
  }

  pub fn as_PeekToken(&self) -> Option<&PeekToken> {
    match self {
      Self::PeekToken(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_PeekToken_mut(&mut self) -> Option<&mut PeekToken> {
    match self {
      Self::PeekToken(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for PeekToken {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct EOFSymbol {
  pub tok: Token,
}

impl EOFSymbol {
  pub fn new(tok: Token) -> Self {
    Self { tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::EOFSymbol
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_EOFSymbol(self) -> Box<EOFSymbol> {
    match self {
      Self::EOFSymbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to EOFSymbol", self.get_type()),
    }
  }

  pub fn as_EOFSymbol(&self) -> Option<&EOFSymbol> {
    match self {
      Self::EOFSymbol(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_EOFSymbol_mut(&mut self) -> Option<&mut EOFSymbol> {
    match self {
      Self::EOFSymbol(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for EOFSymbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct DEFAULT {
  pub instructions: Vec<ASTNode>,
}

impl DEFAULT {
  pub fn new(instructions: Vec<ASTNode>) -> Self {
    Self { instructions }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::DEFAULT
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_DEFAULT(self) -> Box<DEFAULT> {
    match self {
      Self::DEFAULT(val) => val,
      _ => panic!("Type {:?} cannot be converted to DEFAULT", self.get_type()),
    }
  }

  pub fn as_DEFAULT(&self) -> Option<&DEFAULT> {
    match self {
      Self::DEFAULT(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_DEFAULT_mut(&mut self) -> Option<&mut DEFAULT> {
    match self {
      Self::DEFAULT(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for DEFAULT {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.instructions {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct PushGoto {
  pub state: Box<HASH_NAME>,
}

impl PushGoto {
  pub fn new(state: Box<HASH_NAME>) -> Self {
    Self { state }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PushGoto
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_PushGoto(self) -> Box<PushGoto> {
    match self {
      Self::PushGoto(val) => val,
      _ => panic!("Type {:?} cannot be converted to PushGoto", self.get_type()),
    }
  }

  pub fn as_PushGoto(&self) -> Option<&PushGoto> {
    match self {
      Self::PushGoto(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_PushGoto_mut(&mut self) -> Option<&mut PushGoto> {
    match self {
      Self::PushGoto(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for PushGoto {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.state.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct TokenAssign {
  pub ids: Vec<Box<Num>>,
}

impl TokenAssign {
  pub fn new(ids: Vec<Box<Num>>) -> Self {
    Self { ids }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::TokenAssign
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_TokenAssign(self) -> Box<TokenAssign> {
    match self {
      Self::TokenAssign(val) => val,
      _ => panic!("Type {:?} cannot be converted to TokenAssign", self.get_type()),
    }
  }

  pub fn as_TokenAssign(&self) -> Option<&TokenAssign> {
    match self {
      Self::TokenAssign(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_TokenAssign_mut(&mut self) -> Option<&mut TokenAssign> {
    match self {
      Self::TokenAssign(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for TokenAssign {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.ids {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct SkipToken {}

impl SkipToken {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipToken
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_SkipToken(self) -> Box<SkipToken> {
    match self {
      Self::SkipToken(val) => val,
      _ => panic!("Type {:?} cannot be converted to SkipToken", self.get_type()),
    }
  }

  pub fn as_SkipToken(&self) -> Option<&SkipToken> {
    match self {
      Self::SkipToken(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_SkipToken_mut(&mut self) -> Option<&mut SkipToken> {
    match self {
      Self::SkipToken(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for SkipToken {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Add {
  pub left:  ASTNode,
  pub right: ASTNode,
  pub tok:   Token,
}

impl AST_Add {
  pub fn new(left: ASTNode, right: ASTNode, tok: Token) -> Self {
    Self { left, right, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Add
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Add(self) -> Box<AST_Add> {
    match self {
      Self::AST_Add(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Add", self.get_type()),
    }
  }

  pub fn as_AST_Add(&self) -> Option<&AST_Add> {
    match self {
      Self::AST_Add(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Add_mut(&mut self) -> Option<&mut AST_Add> {
    match self {
      Self::AST_Add(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Add {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct ScanShift {}

impl ScanShift {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ScanShift
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_ScanShift(self) -> Box<ScanShift> {
    match self {
      Self::ScanShift(val) => val,
      _ => panic!("Type {:?} cannot be converted to ScanShift", self.get_type()),
    }
  }

  pub fn as_ScanShift(&self) -> Option<&ScanShift> {
    match self {
      Self::ScanShift(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_ScanShift_mut(&mut self) -> Option<&mut ScanShift> {
    match self {
      Self::ScanShift(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for ScanShift {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct ShiftToken {
  pub EMPTY: bool,
}

impl ShiftToken {
  pub fn new(EMPTY: bool) -> Self {
    Self { EMPTY }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ShiftToken
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_ShiftToken(self) -> Box<ShiftToken> {
    match self {
      Self::ShiftToken(val) => val,
      _ => panic!("Type {:?} cannot be converted to ShiftToken", self.get_type()),
    }
  }

  pub fn as_ShiftToken(&self) -> Option<&ShiftToken> {
    match self {
      Self::ShiftToken(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_ShiftToken_mut(&mut self) -> Option<&mut ShiftToken> {
    match self {
      Self::ShiftToken(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for ShiftToken {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.EMPTY.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Syntax {
  pub specs: Vec<Box<SyntaxField>>,
}

impl Syntax {
  pub fn new(specs: Vec<Box<SyntaxField>>) -> Self {
    Self { specs }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Syntax
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Syntax(self) -> Box<Syntax> {
    match self {
      Self::Syntax(val) => val,
      _ => panic!("Type {:?} cannot be converted to Syntax", self.get_type()),
    }
  }

  pub fn as_Syntax(&self) -> Option<&Syntax> {
    match self {
      Self::Syntax(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Syntax_mut(&mut self) -> Option<&mut Syntax> {
    match self {
      Self::Syntax(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Syntax {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.specs {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct SyntaxField {
  pub reference: ASTNode,
  pub spec:      Box<SyntaxSpec>,
}

impl SyntaxField {
  pub fn new(reference: ASTNode, spec: Box<SyntaxSpec>) -> Self {
    Self { reference, spec }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SyntaxField
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_SyntaxField(self) -> Box<SyntaxField> {
    match self {
      Self::SyntaxField(val) => val,
      _ => panic!("Type {:?} cannot be converted to SyntaxField", self.get_type()),
    }
  }

  pub fn as_SyntaxField(&self) -> Option<&SyntaxField> {
    match self {
      Self::SyntaxField(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_SyntaxField_mut(&mut self) -> Option<&mut SyntaxField> {
    match self {
      Self::SyntaxField(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for SyntaxField {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.reference.hash(hasher);
    self.spec.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct HASH_NAME {
  pub val: String,
}

impl HASH_NAME {
  pub fn new(val: String) -> Self {
    Self { val }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::HASH_NAME
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_HASH_NAME(self) -> Box<HASH_NAME> {
    match self {
      Self::HASH_NAME(val) => val,
      _ => panic!("Type {:?} cannot be converted to HASH_NAME", self.get_type()),
    }
  }

  pub fn as_HASH_NAME(&self) -> Option<&HASH_NAME> {
    match self {
      Self::HASH_NAME(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_HASH_NAME_mut(&mut self) -> Option<&mut HASH_NAME> {
    match self {
      Self::HASH_NAME(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for HASH_NAME {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct List_Production {
  pub optional: bool,
  pub symbols: ASTNode,
  pub terminal_symbol: Option<Box<Terminal>>,
  pub tok: Token,
}

impl List_Production {
  pub fn new(
    optional: bool,
    symbols: ASTNode,
    terminal_symbol: Option<Box<Terminal>>,
    tok: Token,
  ) -> Self {
    Self { optional, symbols, terminal_symbol, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::List_Production
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_List_Production(self) -> Box<List_Production> {
    match self {
      Self::List_Production(val) => val,
      _ => panic!("Type {:?} cannot be converted to List_Production", self.get_type()),
    }
  }

  pub fn as_List_Production(&self) -> Option<&List_Production> {
    match self {
      Self::List_Production(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_List_Production_mut(&mut self) -> Option<&mut List_Production> {
    match self {
      Self::List_Production(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for List_Production {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.optional.hash(hasher);
    self.symbols.hash(hasher);
    self.terminal_symbol.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct PeekReset {}

impl PeekReset {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PeekReset
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_PeekReset(self) -> Box<PeekReset> {
    match self {
      Self::PeekReset(val) => val,
      _ => panic!("Type {:?} cannot be converted to PeekReset", self.get_type()),
    }
  }

  pub fn as_PeekReset(&self) -> Option<&PeekReset> {
    match self {
      Self::PeekReset(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_PeekReset_mut(&mut self) -> Option<&mut PeekReset> {
    match self {
      Self::PeekReset(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for PeekReset {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Pop {}

impl Pop {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Pop
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Pop(self) -> Box<Pop> {
    match self {
      Self::Pop(val) => val,
      _ => panic!("Type {:?} cannot be converted to Pop", self.get_type()),
    }
  }

  pub fn as_Pop(&self) -> Option<&Pop> {
    match self {
      Self::Pop(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Pop_mut(&mut self) -> Option<&mut Pop> {
    match self {
      Self::Pop(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Pop {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Vector {
  pub initializer: Vec<ASTNode>,
  pub tok:         Token,
}

impl AST_Vector {
  pub fn new(initializer: Vec<ASTNode>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Vector
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Vector(self) -> Box<AST_Vector> {
    match self {
      Self::AST_Vector(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Vector", self.get_type()),
    }
  }

  pub fn as_AST_Vector(&self) -> Option<&AST_Vector> {
    match self {
      Self::AST_Vector(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Vector_mut(&mut self) -> Option<&mut AST_Vector> {
    match self {
      Self::AST_Vector(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Vector {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.initializer {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct Production_Symbol {
  pub name: String,
  pub tok:  Token,
}

impl Production_Symbol {
  pub fn new(name: String, tok: Token) -> Self {
    Self { name, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production_Symbol
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Production_Symbol(self) -> Box<Production_Symbol> {
    match self {
      Self::Production_Symbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production_Symbol", self.get_type()),
    }
  }

  pub fn as_Production_Symbol(&self) -> Option<&Production_Symbol> {
    match self {
      Self::Production_Symbol(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Production_Symbol_mut(&mut self) -> Option<&mut Production_Symbol> {
    match self {
      Self::Production_Symbol(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Production_Symbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct SkipTokenScanless {}

impl SkipTokenScanless {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipTokenScanless
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_SkipTokenScanless(self) -> Box<SkipTokenScanless> {
    match self {
      Self::SkipTokenScanless(val) => val,
      _ => panic!("Type {:?} cannot be converted to SkipTokenScanless", self.get_type()),
    }
  }

  pub fn as_SkipTokenScanless(&self) -> Option<&SkipTokenScanless> {
    match self {
      Self::SkipTokenScanless(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_SkipTokenScanless_mut(&mut self) -> Option<&mut SkipTokenScanless> {
    match self {
      Self::SkipTokenScanless(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for SkipTokenScanless {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Grammar {
  pub preamble:    Vec<ASTNode>,
  pub productions: Vec<Box<Production>>,
  pub tok:         Token,
}

impl Grammar {
  pub fn new(preamble: Vec<ASTNode>, productions: Vec<Box<Production>>, tok: Token) -> Self {
    Self { preamble, productions, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Grammar
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Grammar(self) -> Box<Grammar> {
    match self {
      Self::Grammar(val) => val,
      _ => panic!("Type {:?} cannot be converted to Grammar", self.get_type()),
    }
  }

  pub fn as_Grammar(&self) -> Option<&Grammar> {
    match self {
      Self::Grammar(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Grammar_mut(&mut self) -> Option<&mut Grammar> {
    match self {
      Self::Grammar(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Grammar {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.preamble {
      val.hash(hasher);
    }

    for val in &self.productions {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct AST_U64 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_U64 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U64
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_U64(self) -> Box<AST_U64> {
    match self {
      Self::AST_U64(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U64", self.get_type()),
    }
  }

  pub fn as_AST_U64(&self) -> Option<&AST_U64> {
    match self {
      Self::AST_U64(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_U64_mut(&mut self) -> Option<&mut AST_U64> {
    match self {
      Self::AST_U64(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_U64 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct DEFINED_TYPE_IDENT {}

impl DEFINED_TYPE_IDENT {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::DEFINED_TYPE_IDENT
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_DEFINED_TYPE_IDENT(self) -> Box<DEFINED_TYPE_IDENT> {
    match self {
      Self::DEFINED_TYPE_IDENT(val) => val,
      _ => panic!("Type {:?} cannot be converted to DEFINED_TYPE_IDENT", self.get_type()),
    }
  }

  pub fn as_DEFINED_TYPE_IDENT(&self) -> Option<&DEFINED_TYPE_IDENT> {
    match self {
      Self::DEFINED_TYPE_IDENT(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_DEFINED_TYPE_IDENT_mut(&mut self) -> Option<&mut DEFINED_TYPE_IDENT> {
    match self {
      Self::DEFINED_TYPE_IDENT(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for DEFINED_TYPE_IDENT {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Priority {
  pub exclusive: bool,
  pub val:       u32,
}

impl Priority {
  pub fn new(exclusive: bool, val: u32) -> Self {
    Self { exclusive, val }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Priority
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Priority(self) -> Box<Priority> {
    match self {
      Self::Priority(val) => val,
      _ => panic!("Type {:?} cannot be converted to Priority", self.get_type()),
    }
  }

  pub fn as_Priority(&self) -> Option<&Priority> {
    match self {
      Self::Priority(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Priority_mut(&mut self) -> Option<&mut Priority> {
    match self {
      Self::Priority(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Priority {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.exclusive.hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Pass {}

impl Pass {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Pass
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Pass(self) -> Box<Pass> {
    match self {
      Self::Pass(val) => val,
      _ => panic!("Type {:?} cannot be converted to Pass", self.get_type()),
    }
  }

  pub fn as_Pass(&self) -> Option<&Pass> {
    match self {
      Self::Pass(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Pass_mut(&mut self) -> Option<&mut Pass> {
    match self {
      Self::Pass(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Pass {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct ASSERT {
  pub ids:          Box<Num>,
  pub instructions: Vec<ASTNode>,
  pub mode:         String,
}

impl ASSERT {
  pub fn new(ids: Box<Num>, instructions: Vec<ASTNode>, mode: String) -> Self {
    Self { ids, instructions, mode }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ASSERT
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_ASSERT(self) -> Box<ASSERT> {
    match self {
      Self::ASSERT(val) => val,
      _ => panic!("Type {:?} cannot be converted to ASSERT", self.get_type()),
    }
  }

  pub fn as_ASSERT(&self) -> Option<&ASSERT> {
    match self {
      Self::ASSERT(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_ASSERT_mut(&mut self) -> Option<&mut ASSERT> {
    match self {
      Self::ASSERT(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for ASSERT {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ids.hash(hasher);

    for val in &self.instructions {
      val.hash(hasher);
    }
    self.mode.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Import {
  pub reference: String,
  pub uri:       String,
  pub tok:       Token,
}

impl Import {
  pub fn new(reference: String, uri: String, tok: Token) -> Self {
    Self { reference, uri, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Import
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Import(self) -> Box<Import> {
    match self {
      Self::Import(val) => val,
      _ => panic!("Type {:?} cannot be converted to Import", self.get_type()),
    }
  }

  pub fn as_Import(&self) -> Option<&Import> {
    match self {
      Self::Import(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Import_mut(&mut self) -> Option<&mut Import> {
    match self {
      Self::Import(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Import {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.reference.hash(hasher);
    self.uri.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Init {
  pub expression: ASTNode,
}

impl Init {
  pub fn new(expression: ASTNode) -> Self {
    Self { expression }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Init
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Init(self) -> Box<Init> {
    match self {
      Self::Init(val) => val,
      _ => panic!("Type {:?} cannot be converted to Init", self.get_type()),
    }
  }

  pub fn as_Init(&self) -> Option<&Init> {
    match self {
      Self::Init(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Init_mut(&mut self) -> Option<&mut Init> {
    match self {
      Self::Init(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Init {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.expression.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_IndexReference {
  pub value: i64,
  pub tok:   Token,
}

impl AST_IndexReference {
  pub fn new(value: i64, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_IndexReference
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_IndexReference(self) -> Box<AST_IndexReference> {
    match self {
      Self::AST_IndexReference(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_IndexReference", self.get_type()),
    }
  }

  pub fn as_AST_IndexReference(&self) -> Option<&AST_IndexReference> {
    match self {
      Self::AST_IndexReference(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_IndexReference_mut(&mut self) -> Option<&mut AST_IndexReference> {
    match self {
      Self::AST_IndexReference(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_IndexReference {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct RGBA {
  pub a: u32,
  pub b: u32,
  pub g: u32,
  pub r: u32,
}

impl RGBA {
  pub fn new(a: u32, b: u32, g: u32, r: u32) -> Self {
    Self { a, b, g, r }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::RGBA
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_RGBA(self) -> Box<RGBA> {
    match self {
      Self::RGBA(val) => val,
      _ => panic!("Type {:?} cannot be converted to RGBA", self.get_type()),
    }
  }

  pub fn as_RGBA(&self) -> Option<&RGBA> {
    match self {
      Self::RGBA(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_RGBA_mut(&mut self) -> Option<&mut RGBA> {
    match self {
      Self::RGBA(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for RGBA {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.a.hash(hasher);
    self.b.hash(hasher);
    self.g.hash(hasher);
    self.r.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Member {
  pub property:  Token,
  pub reference: ASTNode,
}

impl AST_Member {
  pub fn new(property: Token, reference: ASTNode) -> Self {
    Self { property, reference }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Member
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Member(self) -> Box<AST_Member> {
    match self {
      Self::AST_Member(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Member", self.get_type()),
    }
  }

  pub fn as_AST_Member(&self) -> Option<&AST_Member> {
    match self {
      Self::AST_Member(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Member_mut(&mut self) -> Option<&mut AST_Member> {
    match self {
      Self::AST_Member(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Member {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.property.to_string().replace(" ", "").replace("\n", "").hash(hasher);
    self.reference.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Production_Import_Symbol {
  pub module: String,
  pub name:   String,
  pub tok:    Token,
}

impl Production_Import_Symbol {
  pub fn new(module: String, name: String, tok: Token) -> Self {
    Self { module, name, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production_Import_Symbol
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Production_Import_Symbol(self) -> Box<Production_Import_Symbol> {
    match self {
      Self::Production_Import_Symbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production_Import_Symbol", self.get_type()),
    }
  }

  pub fn as_Production_Import_Symbol(&self) -> Option<&Production_Import_Symbol> {
    match self {
      Self::Production_Import_Symbol(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Production_Import_Symbol_mut(&mut self) -> Option<&mut Production_Import_Symbol> {
    match self {
      Self::Production_Import_Symbol(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Production_Import_Symbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.module.hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_I32 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_I32 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I32
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_I32(self) -> Box<AST_I32> {
    match self {
      Self::AST_I32(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I32", self.get_type()),
    }
  }

  pub fn as_AST_I32(&self) -> Option<&AST_I32> {
    match self {
      Self::AST_I32(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_I32_mut(&mut self) -> Option<&mut AST_I32> {
    match self {
      Self::AST_I32(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_I32 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Goto {
  pub state: Box<HASH_NAME>,
}

impl Goto {
  pub fn new(state: Box<HASH_NAME>) -> Self {
    Self { state }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Goto
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Goto(self) -> Box<Goto> {
    match self {
      Self::Goto(val) => val,
      _ => panic!("Type {:?} cannot be converted to Goto", self.get_type()),
    }
  }

  pub fn as_Goto(&self) -> Option<&Goto> {
    match self {
      Self::Goto(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Goto_mut(&mut self) -> Option<&mut Goto> {
    match self {
      Self::Goto(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Goto {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.state.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct SkipPeekToken {}

impl SkipPeekToken {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipPeekToken
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_SkipPeekToken(self) -> Box<SkipPeekToken> {
    match self {
      Self::SkipPeekToken(val) => val,
      _ => panic!("Type {:?} cannot be converted to SkipPeekToken", self.get_type()),
    }
  }

  pub fn as_SkipPeekToken(&self) -> Option<&SkipPeekToken> {
    match self {
      Self::SkipPeekToken(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_SkipPeekToken_mut(&mut self) -> Option<&mut SkipPeekToken> {
    match self {
      Self::SkipPeekToken(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for SkipPeekToken {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_I8 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_I8 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I8
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_I8(self) -> Box<AST_I8> {
    match self {
      Self::AST_I8(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I8", self.get_type()),
    }
  }

  pub fn as_AST_I8(&self) -> Option<&AST_I8> {
    match self {
      Self::AST_I8(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_I8_mut(&mut self) -> Option<&mut AST_I8> {
    match self {
      Self::AST_I8(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_I8 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_F32 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_F32 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_F32
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_F32(self) -> Box<AST_F32> {
    match self {
      Self::AST_F32(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_F32", self.get_type()),
    }
  }

  pub fn as_AST_F32(&self) -> Option<&AST_F32> {
    match self {
      Self::AST_F32(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_F32_mut(&mut self) -> Option<&mut AST_F32> {
    match self {
      Self::AST_F32(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_F32 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Symbols {
  pub expected: Vec<Box<Num>>,
  pub skipped:  Vec<Box<Num>>,
}

impl Symbols {
  pub fn new(expected: Vec<Box<Num>>, skipped: Vec<Box<Num>>) -> Self {
    Self { expected, skipped }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Symbols
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Symbols(self) -> Box<Symbols> {
    match self {
      Self::Symbols(val) => val,
      _ => panic!("Type {:?} cannot be converted to Symbols", self.get_type()),
    }
  }

  pub fn as_Symbols(&self) -> Option<&Symbols> {
    match self {
      Self::Symbols(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Symbols_mut(&mut self) -> Option<&mut Symbols> {
    match self {
      Self::Symbols(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Symbols {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.expected {
      val.hash(hasher);
    }

    for val in &self.skipped {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct AST_Token {
  pub range: Option<Box<Range>>,
}

impl AST_Token {
  pub fn new(range: Option<Box<Range>>) -> Self {
    Self { range }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Token
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Token(self) -> Box<AST_Token> {
    match self {
      Self::AST_Token(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Token", self.get_type()),
    }
  }

  pub fn as_AST_Token(&self) -> Option<&AST_Token> {
    match self {
      Self::AST_Token(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Token_mut(&mut self) -> Option<&mut AST_Token> {
    match self {
      Self::AST_Token(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Token {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.range.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Export {
  pub production: ASTNode,
  pub reference:  Token,
}

impl Export {
  pub fn new(production: ASTNode, reference: Token) -> Self {
    Self { production, reference }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Export
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Export(self) -> Box<Export> {
    match self {
      Self::Export(val) => val,
      _ => panic!("Type {:?} cannot be converted to Export", self.get_type()),
    }
  }

  pub fn as_Export(&self) -> Option<&Export> {
    match self {
      Self::Export(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Export_mut(&mut self) -> Option<&mut Export> {
    match self {
      Self::Export(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Export {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.production.hash(hasher);
    self.reference.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Name {
  pub name: String,
}

impl Name {
  pub fn new(name: String) -> Self {
    Self { name }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Name
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Name(self) -> Box<Name> {
    match self {
      Self::Name(val) => val,
      _ => panic!("Type {:?} cannot be converted to Name", self.get_type()),
    }
  }

  pub fn as_Name(&self) -> Option<&Name> {
    match self {
      Self::Name(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Name_mut(&mut self) -> Option<&mut Name> {
    match self {
      Self::Name(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Name {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_BOOL {
  pub initializer: Option<Box<Init>>,
  pub value:       bool,
  pub tok:         Token,
}

impl AST_BOOL {
  pub fn new(initializer: Option<Box<Init>>, value: bool, tok: Token) -> Self {
    Self { initializer, value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_BOOL
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_BOOL(self) -> Box<AST_BOOL> {
    match self {
      Self::AST_BOOL(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_BOOL", self.get_type()),
    }
  }

  pub fn as_AST_BOOL(&self) -> Option<&AST_BOOL> {
    match self {
      Self::AST_BOOL(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_BOOL_mut(&mut self) -> Option<&mut AST_BOOL> {
    match self {
      Self::AST_BOOL(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_BOOL {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Group_Production {
  pub rules: Vec<Box<Rule>>,
  pub tok:   Token,
}

impl Group_Production {
  pub fn new(rules: Vec<Box<Rule>>, tok: Token) -> Self {
    Self { rules, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Group_Production
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Group_Production(self) -> Box<Group_Production> {
    match self {
      Self::Group_Production(val) => val,
      _ => panic!("Type {:?} cannot be converted to Group_Production", self.get_type()),
    }
  }

  pub fn as_Group_Production(&self) -> Option<&Group_Production> {
    match self {
      Self::Group_Production(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Group_Production_mut(&mut self) -> Option<&mut Group_Production> {
    match self {
      Self::Group_Production(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Group_Production {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.rules {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct Fail {}

impl Fail {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Fail
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Fail(self) -> Box<Fail> {
    match self {
      Self::Fail(val) => val,
      _ => panic!("Type {:?} cannot be converted to Fail", self.get_type()),
    }
  }

  pub fn as_Fail(&self) -> Option<&Fail> {
    match self {
      Self::Fail(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Fail_mut(&mut self) -> Option<&mut Fail> {
    match self {
      Self::Fail(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Fail {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Statements {
  pub statements: Vec<ASTNode>,
  pub tok:        Token,
}

impl AST_Statements {
  pub fn new(statements: Vec<ASTNode>, tok: Token) -> Self {
    Self { statements, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Statements
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Statements(self) -> Box<AST_Statements> {
    match self {
      Self::AST_Statements(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Statements", self.get_type()),
    }
  }

  pub fn as_AST_Statements(&self) -> Option<&AST_Statements> {
    match self {
      Self::AST_Statements(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Statements_mut(&mut self) -> Option<&mut AST_Statements> {
    match self {
      Self::AST_Statements(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Statements {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.statements {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct AST_U8 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_U8 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U8
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_U8(self) -> Box<AST_U8> {
    match self {
      Self::AST_U8(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U8", self.get_type()),
    }
  }

  pub fn as_AST_U8(&self) -> Option<&AST_U8> {
    match self {
      Self::AST_U8(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_U8_mut(&mut self) -> Option<&mut AST_U8> {
    match self {
      Self::AST_U8(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_U8 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Production {
  pub is_append: bool,
  pub is_lazy: bool,
  pub name: String,
  pub name_sym: ASTNode,
  pub priority: Option<Box<Priority>>,
  pub rules: Vec<Box<Rule>>,
  pub template_names: Vec<String>,
  pub tok: Token,
}

impl Production {
  pub fn new(
    is_append: bool,
    is_lazy: bool,
    name: String,
    name_sym: ASTNode,
    priority: Option<Box<Priority>>,
    rules: Vec<Box<Rule>>,
    template_names: Vec<String>,
    tok: Token,
  ) -> Self {
    Self {
      is_append,
      is_lazy,
      name,
      name_sym,
      priority,
      rules,
      template_names,
      tok,
    }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Production(self) -> Box<Production> {
    match self {
      Self::Production(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production", self.get_type()),
    }
  }

  pub fn as_Production(&self) -> Option<&Production> {
    match self {
      Self::Production(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Production_mut(&mut self) -> Option<&mut Production> {
    match self {
      Self::Production(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Production {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_append.hash(hasher);
    self.is_lazy.hash(hasher);
    self.name.hash(hasher);
    self.name_sym.hash(hasher);
    self.priority.hash(hasher);

    for val in &self.rules {
      val.hash(hasher);
    }
    self.template_names.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct ShiftTokenScanless {}

impl ShiftTokenScanless {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ShiftTokenScanless
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_ShiftTokenScanless(self) -> Box<ShiftTokenScanless> {
    match self {
      Self::ShiftTokenScanless(val) => val,
      _ => panic!("Type {:?} cannot be converted to ShiftTokenScanless", self.get_type()),
    }
  }

  pub fn as_ShiftTokenScanless(&self) -> Option<&ShiftTokenScanless> {
    match self {
      Self::ShiftTokenScanless(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_ShiftTokenScanless_mut(&mut self) -> Option<&mut ShiftTokenScanless> {
    match self {
      Self::ShiftTokenScanless(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for ShiftTokenScanless {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct PushExceptHandler {
  pub state: Box<HASH_NAME>,
}

impl PushExceptHandler {
  pub fn new(state: Box<HASH_NAME>) -> Self {
    Self { state }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PushExceptHandler
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_PushExceptHandler(self) -> Box<PushExceptHandler> {
    match self {
      Self::PushExceptHandler(val) => val,
      _ => panic!("Type {:?} cannot be converted to PushExceptHandler", self.get_type()),
    }
  }

  pub fn as_PushExceptHandler(&self) -> Option<&PushExceptHandler> {
    match self {
      Self::PushExceptHandler(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_PushExceptHandler_mut(&mut self) -> Option<&mut PushExceptHandler> {
    match self {
      Self::PushExceptHandler(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for PushExceptHandler {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.state.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Ignore {
  pub symbols: Vec<ASTNode>,
}

impl Ignore {
  pub fn new(symbols: Vec<ASTNode>) -> Self {
    Self { symbols }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Ignore
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Ignore(self) -> Box<Ignore> {
    match self {
      Self::Ignore(val) => val,
      _ => panic!("Type {:?} cannot be converted to Ignore", self.get_type()),
    }
  }

  pub fn as_Ignore(&self) -> Option<&Ignore> {
    match self {
      Self::Ignore(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Ignore_mut(&mut self) -> Option<&mut Ignore> {
    match self {
      Self::Ignore(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Ignore {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.symbols {
      val.hash(hasher);
    }
  }
}

#[derive(Debug, Clone)]
pub struct AST_STRING {
  pub value: Option<Box<Init>>,
  pub tok:   Token,
}

impl AST_STRING {
  pub fn new(value: Option<Box<Init>>, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_STRING
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_STRING(self) -> Box<AST_STRING> {
    match self {
      Self::AST_STRING(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_STRING", self.get_type()),
    }
  }

  pub fn as_AST_STRING(&self) -> Option<&AST_STRING> {
    match self {
      Self::AST_STRING(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_STRING_mut(&mut self) -> Option<&mut AST_STRING> {
    match self {
      Self::AST_STRING(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_STRING {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_U32 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_U32 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U32
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_U32(self) -> Box<AST_U32> {
    match self {
      Self::AST_U32(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U32", self.get_type()),
    }
  }

  pub fn as_AST_U32(&self) -> Option<&AST_U32> {
    match self {
      Self::AST_U32(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_U32_mut(&mut self) -> Option<&mut AST_U32> {
    match self {
      Self::AST_U32(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_U32 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_ClassId {
  pub value: String,
  pub tok:   Token,
}

impl AST_ClassId {
  pub fn new(value: String, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_ClassId
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_ClassId(self) -> Box<AST_ClassId> {
    match self {
      Self::AST_ClassId(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_ClassId", self.get_type()),
    }
  }

  pub fn as_AST_ClassId(&self) -> Option<&AST_ClassId> {
    match self {
      Self::AST_ClassId(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_ClassId_mut(&mut self) -> Option<&mut AST_ClassId> {
    match self {
      Self::AST_ClassId(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_ClassId {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct IR_STATE {
  pub except_handler: bool,
  pub id: String,
  pub instructions: Vec<ASTNode>,
  pub scanner: String,
  pub symbol_meta: Option<Box<Symbols>>,
}

impl IR_STATE {
  pub fn new(
    except_handler: bool,
    id: String,
    instructions: Vec<ASTNode>,
    scanner: String,
    symbol_meta: Option<Box<Symbols>>,
  ) -> Self {
    Self { except_handler, id, instructions, scanner, symbol_meta }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::IR_STATE
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_IR_STATE(self) -> Box<IR_STATE> {
    match self {
      Self::IR_STATE(val) => val,
      _ => panic!("Type {:?} cannot be converted to IR_STATE", self.get_type()),
    }
  }

  pub fn as_IR_STATE(&self) -> Option<&IR_STATE> {
    match self {
      Self::IR_STATE(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_IR_STATE_mut(&mut self) -> Option<&mut IR_STATE> {
    match self {
      Self::IR_STATE(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for IR_STATE {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.except_handler.hash(hasher);
    self.id.hash(hasher);

    for val in &self.instructions {
      val.hash(hasher);
    }
    self.scanner.hash(hasher);
    self.symbol_meta.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_I64 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_I64 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I64
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_I64(self) -> Box<AST_I64> {
    match self {
      Self::AST_I64(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I64", self.get_type()),
    }
  }

  pub fn as_AST_I64(&self) -> Option<&AST_I64> {
    match self {
      Self::AST_I64(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_I64_mut(&mut self) -> Option<&mut AST_I64> {
    match self {
      Self::AST_I64(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_I64 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct PeekTokenScanless {}

impl PeekTokenScanless {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PeekTokenScanless
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_PeekTokenScanless(self) -> Box<PeekTokenScanless> {
    match self {
      Self::PeekTokenScanless(val) => val,
      _ => panic!("Type {:?} cannot be converted to PeekTokenScanless", self.get_type()),
    }
  }

  pub fn as_PeekTokenScanless(&self) -> Option<&PeekTokenScanless> {
    match self {
      Self::PeekTokenScanless(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_PeekTokenScanless_mut(&mut self) -> Option<&mut PeekTokenScanless> {
    match self {
      Self::PeekTokenScanless(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for PeekTokenScanless {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_U16 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_U16 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U16
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_U16(self) -> Box<AST_U16> {
    match self {
      Self::AST_U16(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_U16", self.get_type()),
    }
  }

  pub fn as_AST_U16(&self) -> Option<&AST_U16> {
    match self {
      Self::AST_U16(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_U16_mut(&mut self) -> Option<&mut AST_U16> {
    match self {
      Self::AST_U16(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_U16 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct SkipPeekTokenScanless {}

impl SkipPeekTokenScanless {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipPeekTokenScanless
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_SkipPeekTokenScanless(self) -> Box<SkipPeekTokenScanless> {
    match self {
      Self::SkipPeekTokenScanless(val) => val,
      _ => panic!("Type {:?} cannot be converted to SkipPeekTokenScanless", self.get_type()),
    }
  }

  pub fn as_SkipPeekTokenScanless(&self) -> Option<&SkipPeekTokenScanless> {
    match self {
      Self::SkipPeekTokenScanless(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_SkipPeekTokenScanless_mut(&mut self) -> Option<&mut SkipPeekTokenScanless> {
    match self {
      Self::SkipPeekTokenScanless(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for SkipPeekTokenScanless {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Accept {}

impl Accept {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Accept
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Accept(self) -> Box<Accept> {
    match self {
      Self::Accept(val) => val,
      _ => panic!("Type {:?} cannot be converted to Accept", self.get_type()),
    }
  }

  pub fn as_Accept(&self) -> Option<&Accept> {
    match self {
      Self::Accept(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Accept_mut(&mut self) -> Option<&mut Accept> {
    match self {
      Self::Accept(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Accept {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct ClassSymbol {
  pub val: String,
  pub tok: Token,
}

impl ClassSymbol {
  pub fn new(val: String, tok: Token) -> Self {
    Self { val, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ClassSymbol
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_ClassSymbol(self) -> Box<ClassSymbol> {
    match self {
      Self::ClassSymbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to ClassSymbol", self.get_type()),
    }
  }

  pub fn as_ClassSymbol(&self) -> Option<&ClassSymbol> {
    match self {
      Self::ClassSymbol(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_ClassSymbol_mut(&mut self) -> Option<&mut ClassSymbol> {
    match self {
      Self::ClassSymbol(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for ClassSymbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_F64 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_F64 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_F64
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_F64(self) -> Box<AST_F64> {
    match self {
      Self::AST_F64(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_F64", self.get_type()),
    }
  }

  pub fn as_AST_F64(&self) -> Option<&AST_F64> {
    match self {
      Self::AST_F64(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_F64_mut(&mut self) -> Option<&mut AST_F64> {
    match self {
      Self::AST_F64(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_F64 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Map {
  pub key: ASTNode,
  pub val: ASTNode,
  pub tok: Token,
}

impl AST_Map {
  pub fn new(key: ASTNode, val: ASTNode, tok: Token) -> Self {
    Self { key, val, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Map
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Map(self) -> Box<AST_Map> {
    match self {
      Self::AST_Map(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Map", self.get_type()),
    }
  }

  pub fn as_AST_Map(&self) -> Option<&AST_Map> {
    match self {
      Self::AST_Map(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Map_mut(&mut self) -> Option<&mut AST_Map> {
    match self {
      Self::AST_Map(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Map {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.key.hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AnyGroup {
  pub symbols:   Vec<ASTNode>,
  pub unordered: bool,
  pub tok:       Token,
}

impl AnyGroup {
  pub fn new(symbols: Vec<ASTNode>, unordered: bool, tok: Token) -> Self {
    Self { symbols, unordered, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AnyGroup
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AnyGroup(self) -> Box<AnyGroup> {
    match self {
      Self::AnyGroup(val) => val,
      _ => panic!("Type {:?} cannot be converted to AnyGroup", self.get_type()),
    }
  }

  pub fn as_AnyGroup(&self) -> Option<&AnyGroup> {
    match self {
      Self::AnyGroup(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AnyGroup_mut(&mut self) -> Option<&mut AnyGroup> {
    match self {
      Self::AnyGroup(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AnyGroup {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.symbols {
      val.hash(hasher);
    }
    self.unordered.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Range {
  pub end_trim:   i32,
  pub start_trim: i32,
}

impl Range {
  pub fn new(end_trim: i32, start_trim: i32) -> Self {
    Self { end_trim, start_trim }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Range
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Range(self) -> Box<Range> {
    match self {
      Self::Range(val) => val,
      _ => panic!("Type {:?} cannot be converted to Range", self.get_type()),
    }
  }

  pub fn as_Range(&self) -> Option<&Range> {
    match self {
      Self::Range(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Range_mut(&mut self) -> Option<&mut Range> {
    match self {
      Self::Range(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Range {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.end_trim.hash(hasher);
    self.start_trim.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_I16 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}

impl AST_I16 {
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I16
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_I16(self) -> Box<AST_I16> {
    match self {
      Self::AST_I16(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_I16", self.get_type()),
    }
  }

  pub fn as_AST_I16(&self) -> Option<&AST_I16> {
    match self {
      Self::AST_I16(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_I16_mut(&mut self) -> Option<&mut AST_I16> {
    match self {
      Self::AST_I16(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_I16 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_NUMBER {
  pub value: f64,
}

impl AST_NUMBER {
  pub fn new(value: f64) -> Self {
    Self { value }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_NUMBER
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_NUMBER(self) -> Box<AST_NUMBER> {
    match self {
      Self::AST_NUMBER(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_NUMBER", self.get_type()),
    }
  }

  pub fn as_AST_NUMBER(&self) -> Option<&AST_NUMBER> {
    match self {
      Self::AST_NUMBER(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_NUMBER_mut(&mut self) -> Option<&mut AST_NUMBER> {
    match self {
      Self::AST_NUMBER(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_NUMBER {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.to_le_bytes().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Struct {
  pub props: Vec<ASTNode>,
  pub typ:   Token,
  pub tok:   Token,
}

impl AST_Struct {
  pub fn new(props: Vec<ASTNode>, typ: Token, tok: Token) -> Self {
    Self { props, typ, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Struct
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Struct(self) -> Box<AST_Struct> {
    match self {
      Self::AST_Struct(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Struct", self.get_type()),
    }
  }

  pub fn as_AST_Struct(&self) -> Option<&AST_Struct> {
    match self {
      Self::AST_Struct(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Struct_mut(&mut self) -> Option<&mut AST_Struct> {
    match self {
      Self::AST_Struct(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Struct {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);

    for val in &self.props {
      val.hash(hasher);
    }
    self.typ.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct DEFINED_TYPE_NUM {}

impl DEFINED_TYPE_NUM {
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::DEFINED_TYPE_NUM
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_DEFINED_TYPE_NUM(self) -> Box<DEFINED_TYPE_NUM> {
    match self {
      Self::DEFINED_TYPE_NUM(val) => val,
      _ => panic!("Type {:?} cannot be converted to DEFINED_TYPE_NUM", self.get_type()),
    }
  }

  pub fn as_DEFINED_TYPE_NUM(&self) -> Option<&DEFINED_TYPE_NUM> {
    match self {
      Self::DEFINED_TYPE_NUM(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_DEFINED_TYPE_NUM_mut(&mut self) -> Option<&mut DEFINED_TYPE_NUM> {
    match self {
      Self::DEFINED_TYPE_NUM(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for DEFINED_TYPE_NUM {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct SyntaxSpec {
  pub id:  Token,
  pub rgb: Option<Box<RGBA>>,
}

impl SyntaxSpec {
  pub fn new(id: Token, rgb: Option<Box<RGBA>>) -> Self {
    Self { id, rgb }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SyntaxSpec
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_SyntaxSpec(self) -> Box<SyntaxSpec> {
    match self {
      Self::SyntaxSpec(val) => val,
      _ => panic!("Type {:?} cannot be converted to SyntaxSpec", self.get_type()),
    }
  }

  pub fn as_SyntaxSpec(&self) -> Option<&SyntaxSpec> {
    match self {
      Self::SyntaxSpec(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_SyntaxSpec_mut(&mut self) -> Option<&mut SyntaxSpec> {
    match self {
      Self::SyntaxSpec(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for SyntaxSpec {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.to_string().replace(" ", "").replace("\n", "").hash(hasher);
    self.rgb.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Rule {
  pub ast_definition: Option<Box<Ascript>>,
  pub is_priority: bool,
  pub recover_definition: Option<Box<Recovery>>,
  pub symbols: Vec<ASTNode>,
  pub syntax_definition: Option<Box<Syntax>>,
  pub tok: Token,
}

impl Rule {
  pub fn new(
    ast_definition: Option<Box<Ascript>>,
    is_priority: bool,
    recover_definition: Option<Box<Recovery>>,
    symbols: Vec<ASTNode>,
    syntax_definition: Option<Box<Syntax>>,
    tok: Token,
  ) -> Self {
    Self {
      ast_definition,
      is_priority,
      recover_definition,
      symbols,
      syntax_definition,
      tok,
    }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Rule
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Rule(self) -> Box<Rule> {
    match self {
      Self::Rule(val) => val,
      _ => panic!("Type {:?} cannot be converted to Rule", self.get_type()),
    }
  }

  pub fn as_Rule(&self) -> Option<&Rule> {
    match self {
      Self::Rule(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Rule_mut(&mut self) -> Option<&mut Rule> {
    match self {
      Self::Rule(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Rule {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ast_definition.hash(hasher);
    self.is_priority.hash(hasher);
    self.recover_definition.hash(hasher);

    for val in &self.symbols {
      val.hash(hasher);
    }
    self.syntax_definition.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AnnotatedSymbol {
  pub is_optional: bool,
  pub priority:    Option<Box<Priority>>,
  pub reference:   String,
  pub symbol:      ASTNode,
  pub tok:         Token,
}

impl AnnotatedSymbol {
  pub fn new(
    is_optional: bool,
    priority: Option<Box<Priority>>,
    reference: String,
    symbol: ASTNode,
    tok: Token,
  ) -> Self {
    Self { is_optional, priority, reference, symbol, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AnnotatedSymbol
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AnnotatedSymbol(self) -> Box<AnnotatedSymbol> {
    match self {
      Self::AnnotatedSymbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to AnnotatedSymbol", self.get_type()),
    }
  }

  pub fn as_AnnotatedSymbol(&self) -> Option<&AnnotatedSymbol> {
    match self {
      Self::AnnotatedSymbol(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AnnotatedSymbol_mut(&mut self) -> Option<&mut AnnotatedSymbol> {
    match self {
      Self::AnnotatedSymbol(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AnnotatedSymbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_optional.hash(hasher);
    self.priority.hash(hasher);
    self.reference.hash(hasher);
    self.symbol.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct AST_Property {
  pub id: String,
  pub named_reference: String,
  pub value: Option<ASTNode>,
  pub tok: Token,
}

impl AST_Property {
  pub fn new(id: String, named_reference: String, value: Option<ASTNode>, tok: Token) -> Self {
    Self { id, named_reference, value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Property
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_AST_Property(self) -> Box<AST_Property> {
    match self {
      Self::AST_Property(val) => val,
      _ => panic!("Type {:?} cannot be converted to AST_Property", self.get_type()),
    }
  }

  pub fn as_AST_Property(&self) -> Option<&AST_Property> {
    match self {
      Self::AST_Property(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_AST_Property_mut(&mut self) -> Option<&mut AST_Property> {
    match self {
      Self::AST_Property(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for AST_Property {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
    self.named_reference.hash(hasher);
    self.value.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Ascript {
  pub ast: ASTNode,
  pub tok: Token,
}

impl Ascript {
  pub fn new(ast: ASTNode, tok: Token) -> Self {
    Self { ast, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Ascript
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Ascript(self) -> Box<Ascript> {
    match self {
      Self::Ascript(val) => val,
      _ => panic!("Type {:?} cannot be converted to Ascript", self.get_type()),
    }
  }

  pub fn as_Ascript(&self) -> Option<&Ascript> {
    match self {
      Self::Ascript(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Ascript_mut(&mut self) -> Option<&mut Ascript> {
    match self {
      Self::Ascript(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Ascript {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ast.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Num {
  pub val: i64,
}

impl Num {
  pub fn new(val: i64) -> Self {
    Self { val }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Num
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Num(self) -> Box<Num> {
    match self {
      Self::Num(val) => val,
      _ => panic!("Type {:?} cannot be converted to Num", self.get_type()),
    }
  }

  pub fn as_Num(&self) -> Option<&Num> {
    match self {
      Self::Num(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Num_mut(&mut self) -> Option<&mut Num> {
    match self {
      Self::Num(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Num {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Production_Terminal_Symbol {
  pub production: ASTNode,
  pub tok:        Token,
}

impl Production_Terminal_Symbol {
  pub fn new(production: ASTNode, tok: Token) -> Self {
    Self { production, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production_Terminal_Symbol
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Production_Terminal_Symbol(self) -> Box<Production_Terminal_Symbol> {
    match self {
      Self::Production_Terminal_Symbol(val) => val,
      _ => panic!("Type {:?} cannot be converted to Production_Terminal_Symbol", self.get_type()),
    }
  }

  pub fn as_Production_Terminal_Symbol(&self) -> Option<&Production_Terminal_Symbol> {
    match self {
      Self::Production_Terminal_Symbol(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Production_Terminal_Symbol_mut(&mut self) -> Option<&mut Production_Terminal_Symbol> {
    match self {
      Self::Production_Terminal_Symbol(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Production_Terminal_Symbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.production.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Terminal {
  pub is_exclusive: bool,
  pub val:          String,
  pub tok:          Token,
}

impl Terminal {
  pub fn new(is_exclusive: bool, val: String, tok: Token) -> Self {
    Self { is_exclusive, val, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Terminal
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Terminal(self) -> Box<Terminal> {
    match self {
      Self::Terminal(val) => val,
      _ => panic!("Type {:?} cannot be converted to Terminal", self.get_type()),
    }
  }

  pub fn as_Terminal(&self) -> Option<&Terminal> {
    match self {
      Self::Terminal(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Terminal_mut(&mut self) -> Option<&mut Terminal> {
    match self {
      Self::Terminal(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Terminal {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_exclusive.hash(hasher);
    self.val.hash(hasher);
  }
}

#[derive(Debug, Clone)]
pub struct Recovery {
  pub state: Box<IR_STATE>,
  pub tok:   Token,
}

impl Recovery {
  pub fn new(state: Box<IR_STATE>, tok: Token) -> Self {
    Self { state, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Recovery
  }
}

impl ASTNode {
  #[track_caller]
  pub fn to_Recovery(self) -> Box<Recovery> {
    match self {
      Self::Recovery(val) => val,
      _ => panic!("Type {:?} cannot be converted to Recovery", self.get_type()),
    }
  }

  pub fn as_Recovery(&self) -> Option<&Recovery> {
    match self {
      Self::Recovery(val) => Some(val.as_ref()),
      _ => None,
    }
  }

  pub fn as_Recovery_mut(&mut self) -> Option<&mut Recovery> {
    match self {
      Self::Recovery(val) => Some(val.as_mut()),
      _ => None,
    }
  }
}

impl Hash for Recovery {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.state.hash(hasher);
  }
}

fn reducer_000<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U8::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_001<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U8::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_002<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_U16(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_003<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U16::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_U16(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_004<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_U32(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_005<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U32::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_U32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_006<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_U64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_U64(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_007<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_U64::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_U64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_008<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I8::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_009<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I8::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_010<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I16::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_I16(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_011<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I16::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_I16(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_012<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_I32(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_013<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I32::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_I32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_014<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_I64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_I64(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_015<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_I64::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_I64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_016<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_F32::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_F32(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_017<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_F32::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_F32(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_018<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_F64::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_F64(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_019<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_F64::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_F64(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_020<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe { &*_ctx_ }.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_021<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_2_0 = __tok_rng_3;
  let tok_2_0 = tok_2_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_5_0 = HASH_NAME::new(tok_2_0);
  slots
    .assign(0, AstSlot(ASTNode::HASH_NAME(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_022<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3.into_nodes();
  let obj_5_1 = true;
  let obj_6_0 = AnyGroup::new(
    obj_2_0,
    obj_5_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AnyGroup(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_023<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_1 = false;
  let obj_5_0 = AnyGroup::new(
    obj_1_0,
    obj_4_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AnyGroup(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_024<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_025<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_026<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_027<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_028<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_029<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Ascript::new(
    obj_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::Ascript(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_030<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_2 = obj3;
  let obj_4_0 = AST_Property::new(
    tok_0_0,
    Default::default(),
    Some(obj_2_2),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Property(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_031<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_2 = obj3;
  let obj_4_0 = AST_Property::new(
    tok_0_0,
    Default::default(),
    Some(obj_2_2),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Property(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_032<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let tok_0_1 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_1 = tok_0_1.to_string();
  let obj_2_0 = AST_Property::new(
    tok_0_0,
    tok_0_1,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Property(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_033<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  let obj_2_0 = AST_ClassId::new(
    tok_0_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_ClassId(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_034<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_035<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let tok_3_1 = __tok_rng_4.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_5_0 = Export::new(obj_1_0, tok_3_1);
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_036<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let tok_3_1 = __tok_rng_4.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_5_0 = Export::new(obj_1_0, tok_3_1);
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_037<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Export::new(obj_1_0, Default::default());
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_038<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_039<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3.to_string();
  let mut obj_0_0 = obj1.into_strings();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_040<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.to_string();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_041<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_042<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_043<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_3_0 = obj4.into_nodes();
  let tok_1_1 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_6_0 = AST_Struct::new(
    obj_3_0,
    tok_1_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Struct(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_044<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_1 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_4_0 = AST_Struct::new(
    vec![],
    tok_1_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Struct(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_045<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_2_0 = obj3.into_nodes();
  let obj_4_1 = obj5.into_nodes();
  let obj_6_0 = Symbols::new(
    obj_2_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    obj_4_1
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
  );
  slots
    .assign(0, AstSlot(ASTNode::Symbols(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_046<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3.into_nodes();
  let obj_4_0 = Symbols::new(
    obj_2_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    vec![],
  );
  slots
    .assign(0, AstSlot(ASTNode::Symbols(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_047<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe { &*_ctx_ }.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_048<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_3_0 = Production_Terminal_Symbol::new(
    obj_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(
      ASTNode::Production_Terminal_Symbol(Box::new(obj_3_0)),
      __rule_rng__,
      TokenRange::default(),
    ),
  );
}

fn reducer_049<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_050<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_051<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3.into_nodes();
  let obj_5_0 = Ignore::new(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::Ignore(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_052<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<i64>(unsafe { &*_ctx_ }.get_str());
  let obj_2_0 = Num::new(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::Num(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_053<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_054<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3.into_nodes();
  let obj_4_0 = TokenAssign::new(
    obj_2_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::TokenAssign(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_055<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Pop::new();
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_056<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Pass::new();
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_057<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = SkipToken::new();
  slots
    .assign(0, AstSlot(ASTNode::SkipToken(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_058<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = SkipTokenScanless::new();
  slots.assign(
    0,
    AstSlot(ASTNode::SkipTokenScanless(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_059<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Fail::new();
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_060<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = PeekToken::new();
  slots
    .assign(0, AstSlot(ASTNode::PeekToken(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_061<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = PeekTokenScanless::new();
  slots.assign(
    0,
    AstSlot(ASTNode::PeekTokenScanless(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_062<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = SkipPeekToken::new();
  slots.assign(
    0,
    AstSlot(ASTNode::SkipPeekToken(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_063<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = SkipPeekTokenScanless::new();
  slots.assign(
    0,
    AstSlot(ASTNode::SkipPeekTokenScanless(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_064<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = PeekReset::new();
  slots
    .assign(0, AstSlot(ASTNode::PeekReset(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_065<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = true;
  let obj_4_0 = ShiftToken::new(obj_3_0);
  slots.assign(
    0,
    AstSlot(ASTNode::ShiftToken(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_066<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = false;
  let obj_3_0 = ShiftToken::new(obj_2_0);
  slots.assign(
    0,
    AstSlot(ASTNode::ShiftToken(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_067<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = ShiftTokenScanless::new();
  slots.assign(
    0,
    AstSlot(ASTNode::ShiftTokenScanless(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_068<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = ScanShift::new();
  slots
    .assign(0, AstSlot(ASTNode::ScanShift(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_069<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = Accept::new();
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_070<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_4_0 = Init::new(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::Init(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_071<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_072<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_073<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_074<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_075<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_076<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_077<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_078<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_079<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_080<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_081<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_082<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_083<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_084<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_085<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_086<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_087<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_088<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_089<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_090<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_091<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_092<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_093<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = AST_NamedReference::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_NamedReference(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_094<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i64>(unsafe { &*_ctx_ }.get_str());
  let obj_3_0 = AST_IndexReference::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_IndexReference(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_095<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_096<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_0 = obj1;
  let obj_2_1 = obj3;
  let obj_4_0 = AST_Add::new(
    obj_0_0,
    obj_2_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_Add(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_097<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_098<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(obj_5_0, obj_4_1, obj_3_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_099<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_1 = obj5.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_7_0 = RGBA::new(0, obj_4_1, obj_3_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_100<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(obj_4_0, obj_5_1, obj_3_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_101<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(obj_5_0, obj_3_1, obj_4_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_102<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_3_1 = obj4.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_7_0 = RGBA::new(0, obj_3_1, obj_4_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_103<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(obj_4_0, obj_3_1, obj_5_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_104<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(obj_3_0, obj_5_1, obj_4_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_105<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_2_3 = obj3.to_u32();
  let obj_8_0 = RGBA::new(obj_3_0, obj_4_1, obj_5_2, obj_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_106<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(obj_5_0, obj_4_1, obj_2_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_107<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_4_1 = obj5.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_7_0 = RGBA::new(0, obj_4_1, obj_2_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_108<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(obj_4_0, obj_5_1, obj_2_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_109<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(obj_5_0, obj_3_1, obj_2_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_110<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_3_1 = obj4.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_7_0 = RGBA::new(0, obj_3_1, obj_2_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_111<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(obj_4_0, obj_3_1, obj_2_2, obj_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_112<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(obj_3_0, obj_5_1, obj_2_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_113<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_2_2 = obj3.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(obj_3_0, obj_4_1, obj_2_2, obj_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_114<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(obj_5_0, obj_2_1, obj_4_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_115<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_1 = obj3.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_7_0 = RGBA::new(0, obj_2_1, obj_4_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_116<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(obj_4_0, obj_2_1, obj_5_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_117<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_5_0 = obj6.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(obj_5_0, obj_2_1, obj_3_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_118<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_1 = obj3.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_7_0 = RGBA::new(0, obj_2_1, obj_3_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_119<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_4_0 = obj5.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(obj_4_0, obj_2_1, obj_3_2, obj_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_120<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(obj_3_0, obj_2_1, obj_5_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_121<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_3_0 = obj4.to_u32();
  let obj_2_1 = obj3.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(obj_3_0, obj_2_1, obj_4_2, obj_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_122<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(obj_2_0, obj_5_1, obj_4_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_123<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_3_3 = obj4.to_u32();
  let obj_8_0 = RGBA::new(obj_2_0, obj_4_1, obj_5_2, obj_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_124<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_5_1 = obj6.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(obj_2_0, obj_5_1, obj_3_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_125<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_4_1 = obj5.to_u32();
  let obj_3_2 = obj4.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(obj_2_0, obj_4_1, obj_3_2, obj_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_126<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_5_2 = obj6.to_u32();
  let obj_4_3 = obj5.to_u32();
  let obj_8_0 = RGBA::new(obj_2_0, obj_3_1, obj_5_2, obj_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_127<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, ..) = slots.take(5);
  let AstSlot(_, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_2_0 = obj3.to_u32();
  let obj_3_1 = obj4.to_u32();
  let obj_4_2 = obj5.to_u32();
  let obj_5_3 = obj6.to_u32();
  let obj_8_0 = RGBA::new(obj_2_0, obj_3_1, obj_4_2, obj_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_128<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i64>(unsafe { &*_ctx_ }.get_str());
  let obj_4_0 = Num::new(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::Num(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_129<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_6_0 = true;
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_2_3 = obj3;
  let obj_1_4 = obj2;
  let obj_1_4 = obj_1_4.to_Priority();
  let obj_4_5 = obj5.into_nodes();
  let obj_7_0 = Production::new(
    obj_6_0,
    false,
    tok_2_2,
    obj_2_3,
    Some(obj_1_4),
    obj_4_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_130<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_1_3 = obj2;
  let obj_3_5 = obj4.into_nodes();
  let obj_6_0 = Production::new(
    obj_5_0,
    false,
    tok_1_2,
    obj_1_3,
    None,
    obj_3_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_131<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_1_0 = tok_1_0.to_string();
  let obj_3_0 = Name::new(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::Name(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_132<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_133<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_134<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_135<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_136<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_1 = __tok_rng_2;
  let tok_1_1 = tok_1_1.parse::<u32>(unsafe { &*_ctx_ }.get_str());
  let obj_4_0 = Priority::new(false, tok_1_1);
  slots
    .assign(0, AstSlot(ASTNode::Priority(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_137<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = true;
  let obj_3_0 = Priority::new(obj_2_0, 0);
  slots
    .assign(0, AstSlot(ASTNode::Priority(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_138<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_139<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_140<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_141<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = Group_Production::new(
    obj_1_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Group_Production(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_142<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_143<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_144<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = AST_Vector::new(
    obj_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Vector(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_145<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = AST_Vector::new(
    vec![],
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Vector(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_146<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_147<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_148<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_149<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_150<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Priority();
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_3_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_151<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Priority();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_2_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_152<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = false;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Priority();
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_2_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_153<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = false;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Priority();
  let obj_0_3 = obj1;
  let obj_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    Some(obj_1_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_154<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    None,
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_155<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = true;
  let obj_0_3 = obj1;
  let obj_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    None,
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_156<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = false;
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_4_0 = AnnotatedSymbol::new(
    obj_3_0,
    None,
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_157<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Priority();
  let tok_1_2 = __tok_rng_2;
  let tok_1_2 = tok_1_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_2_1),
    tok_1_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_158<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Priority();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_1_1),
    Default::default(),
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_159<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_3_1 = obj4;
  let obj_3_1 = obj_3_1.to_Priority();
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_3_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_160<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    None,
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_161<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_Priority();
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_2_1),
    tok_3_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_162<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = false;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Priority();
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_5_0 = AnnotatedSymbol::new(
    obj_4_0,
    Some(obj_1_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_163<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Priority();
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_1_1),
    tok_2_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_164<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_Priority();
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_0_3 = obj1;
  let obj_6_0 = AnnotatedSymbol::new(
    obj_5_0,
    Some(obj_1_1),
    tok_3_2,
    obj_0_3,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AnnotatedSymbol(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_165<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_166<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Range();
  let obj_3_0 = AST_Token::new(Some(obj_1_0));
  slots
    .assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_167<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Range();
  let obj_3_0 = AST_Token::new(Some(obj_1_0));
  slots
    .assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_168<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Range();
  let obj_3_0 = AST_Token::new(Some(obj_1_0));
  slots
    .assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_169<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(None);
  slots
    .assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_170<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(None);
  slots
    .assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_171<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_Token::new(None);
  slots
    .assign(0, AstSlot(ASTNode::AST_Token(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_172<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_HASH_NAME();
  let obj_4_0 = PushExceptHandler::new(obj_1_0);
  slots.assign(
    0,
    AstSlot(ASTNode::PushExceptHandler(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_173<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_174<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_175<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_176<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_177<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_178<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_179<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_180<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_0 = ClassSymbol::new(
    tok_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::ClassSymbol(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_181<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let AstSlot(_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_3_0 = __tok_rng_4;
  let tok_3_0 = tok_3_0.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_1_1 = __tok_rng_2;
  let tok_1_1 = tok_1_1.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let obj_6_0 = Range::new(tok_3_0, tok_1_1);
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_182<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_1_1 = __tok_rng_2;
  let tok_1_1 = tok_1_1.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let obj_4_0 = Range::new(0, tok_1_1);
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_183<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_2_0 = __tok_rng_3;
  let tok_2_0 = tok_2_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_184<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_HASH_NAME();
  let obj_3_0 = Goto::new(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::Goto(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_185<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_186<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_187<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_188<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_189<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_190<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Num();
  let obj_4_1 = obj5.into_nodes();
  let tok_1_2 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_1_2 = tok_1_2.to_string();
  let obj_7_0 = ASSERT::new(obj_2_0, obj_4_1, tok_1_2);
  slots.assign(0, AstSlot(ASTNode::ASSERT(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_191<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3.into_nodes();
  let obj_5_0 = DEFAULT::new(obj_2_0);
  slots
    .assign(0, AstSlot(ASTNode::DEFAULT(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_192<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = DEFINED_TYPE_IDENT::new();
  slots.assign(
    0,
    AstSlot(ASTNode::DEFINED_TYPE_IDENT(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_193<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_1_1 = obj2.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let obj_5_0 = Terminal::new(
    obj_4_0,
    obj_1_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::Terminal(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_194<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_1 = obj2.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let obj_4_0 = Terminal::new(
    false,
    obj_1_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::Terminal(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_195<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_IR_STATE();
  let obj_5_0 = Recovery::new(
    obj_2_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::Recovery(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_196<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_6_1 = true;
  let obj_4_2 = obj5;
  let obj_4_2 = obj_4_2.to_Recovery();
  let obj_1_3 = obj2.into_nodes();
  let obj_3_4 = obj4;
  let obj_3_4 = obj_3_4.to_Syntax();
  let obj_7_0 = Rule::new(
    Some(obj_2_0),
    obj_6_1,
    Some(obj_4_2),
    obj_1_3,
    Some(obj_3_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_197<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_5_1 = false;
  let obj_3_2 = obj4;
  let obj_3_2 = obj_3_2.to_Recovery();
  let obj_0_3 = obj1.into_nodes();
  let obj_2_4 = obj3;
  let obj_2_4 = obj_2_4.to_Syntax();
  let obj_6_0 = Rule::new(
    Some(obj_1_0),
    obj_5_1,
    Some(obj_3_2),
    obj_0_3,
    Some(obj_2_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_198<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_1 = true;
  let obj_3_2 = obj4;
  let obj_3_2 = obj_3_2.to_Recovery();
  let obj_1_3 = obj2.into_nodes();
  let obj_2_4 = obj3;
  let obj_2_4 = obj_2_4.to_Syntax();
  let obj_6_0 = Rule::new(
    None,
    obj_5_1,
    Some(obj_3_2),
    obj_1_3,
    Some(obj_2_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_199<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_1 = false;
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Recovery();
  let obj_0_3 = obj1.into_nodes();
  let obj_1_4 = obj2;
  let obj_1_4 = obj_1_4.to_Syntax();
  let obj_5_0 = Rule::new(
    None,
    obj_4_1,
    Some(obj_2_2),
    obj_0_3,
    Some(obj_1_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_200<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_5_1 = true;
  let obj_3_2 = obj4;
  let obj_3_2 = obj_3_2.to_Recovery();
  let obj_1_3 = obj2.into_nodes();
  let obj_6_0 = Rule::new(
    Some(obj_2_0),
    obj_5_1,
    Some(obj_3_2),
    obj_1_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_201<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_4_1 = false;
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Recovery();
  let obj_0_3 = obj1.into_nodes();
  let obj_5_0 = Rule::new(
    Some(obj_1_0),
    obj_4_1,
    Some(obj_2_2),
    obj_0_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_202<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_1 = true;
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Recovery();
  let obj_1_3 = obj2.into_nodes();
  let obj_5_0 = Rule::new(
    None,
    obj_4_1,
    Some(obj_2_2),
    obj_1_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_203<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_1 = false;
  let obj_1_2 = obj2;
  let obj_1_2 = obj_1_2.to_Recovery();
  let obj_0_3 = obj1.into_nodes();
  let obj_4_0 = Rule::new(
    None,
    obj_3_1,
    Some(obj_1_2),
    obj_0_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_204<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_5_1 = true;
  let obj_1_3 = obj2.into_nodes();
  let obj_3_4 = obj4;
  let obj_3_4 = obj_3_4.to_Syntax();
  let obj_6_0 = Rule::new(
    Some(obj_2_0),
    obj_5_1,
    None,
    obj_1_3,
    Some(obj_3_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_205<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_4_1 = false;
  let obj_0_3 = obj1.into_nodes();
  let obj_2_4 = obj3;
  let obj_2_4 = obj_2_4.to_Syntax();
  let obj_5_0 = Rule::new(
    Some(obj_1_0),
    obj_4_1,
    None,
    obj_0_3,
    Some(obj_2_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_206<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_1 = true;
  let obj_1_3 = obj2.into_nodes();
  let obj_2_4 = obj3;
  let obj_2_4 = obj_2_4.to_Syntax();
  let obj_5_0 = Rule::new(
    None,
    obj_4_1,
    None,
    obj_1_3,
    Some(obj_2_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_207<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_1 = false;
  let obj_0_3 = obj1.into_nodes();
  let obj_1_4 = obj2;
  let obj_1_4 = obj_1_4.to_Syntax();
  let obj_4_0 = Rule::new(
    None,
    obj_3_1,
    None,
    obj_0_3,
    Some(obj_1_4),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_208<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let obj_2_0 = obj_2_0.to_Ascript();
  let obj_4_1 = true;
  let obj_1_3 = obj2.into_nodes();
  let obj_5_0 = Rule::new(
    Some(obj_2_0),
    obj_4_1,
    None,
    obj_1_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_209<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Ascript();
  let obj_3_1 = false;
  let obj_0_3 = obj1.into_nodes();
  let obj_4_0 = Rule::new(
    Some(obj_1_0),
    obj_3_1,
    None,
    obj_0_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_210<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_1 = true;
  let obj_1_3 = obj2.into_nodes();
  let obj_4_0 = Rule::new(
    None,
    obj_3_1,
    None,
    obj_1_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_211<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_1 = false;
  let obj_0_3 = obj1.into_nodes();
  let obj_3_0 = Rule::new(
    None,
    obj_2_1,
    None,
    obj_0_3,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_212<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_2_0 = __tok_rng_3;
  let tok_2_0 = tok_2_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_213<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_0 = obj1;
  let obj_2_1 = obj3;
  let obj_2_1 = obj_2_1.to_SyntaxSpec();
  let obj_4_0 = SyntaxField::new(obj_0_0, obj_2_1);
  slots.assign(
    0,
    AstSlot(ASTNode::SyntaxField(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_214<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_215<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_216<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_217<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_218<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_219<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  let obj_3_0 = AST_Statements::new(
    obj_2_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Statements(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_220<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = AST_Statements::new(
    obj_1_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Statements(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_221<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = AST_Statements::new(
    vec![],
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Statements(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_222<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_223<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_224<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_225<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_226<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_227<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_2_0 = obj3;
  let obj_4_1 = obj5;
  let obj_7_0 = AST_Map::new(
    obj_2_0,
    obj_4_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_Map(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_228<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(_, __tok_rng_5, _) = slots.take(4);
  slots.take(5);
  slots.take(6);
  let AstSlot(_, __tok_rng_8, _) = slots.take(7);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_8;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_4_1 = __tok_rng_5;
  let tok_4_1 = tok_4_1.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_7_2 = __tok_rng_8;
  let tok_7_2 = tok_7_2.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let obj_9_0 = Reduce::new(tok_1_0, tok_4_1, tok_7_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_9_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_229<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  slots.take(3);
  slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_2_1 = __tok_rng_3;
  let tok_2_1 = tok_2_1.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_5_2 = __tok_rng_6;
  let tok_5_2 = tok_5_2.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let obj_7_0 = Reduce::new(tok_1_0, tok_2_1, tok_5_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_230<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(_, __tok_rng_5, _) = slots.take(4);
  let AstSlot(_, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_4_1 = __tok_rng_5;
  let tok_4_1 = tok_4_1.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_5_2 = __tok_rng_6;
  let tok_5_2 = tok_5_2.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let obj_7_0 = Reduce::new(tok_1_0, tok_4_1, tok_5_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_231<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_2_1 = __tok_rng_3;
  let tok_2_1 = tok_2_1.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.parse::<i32>(unsafe { &*_ctx_ }.get_str());
  let obj_5_0 = Reduce::new(tok_1_0, tok_2_1, tok_3_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_232<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_233<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_234<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_6_0 = true;
  let obj_0_1 = obj1.to_string();
  let obj_3_2 = obj4.into_nodes();
  let obj_1_3 = obj2.to_string();
  let obj_4_4 = obj5;
  let obj_4_4 = obj_4_4.to_Symbols();
  let obj_7_0 = IR_STATE::new(obj_6_0, obj_0_1, obj_3_2, obj_1_3, Some(obj_4_4));
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_235<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_0_1 = obj1.to_string();
  let obj_2_2 = obj3.into_nodes();
  let obj_3_4 = obj4;
  let obj_3_4 = obj_3_4.to_Symbols();
  let obj_6_0 = IR_STATE::new(obj_5_0, obj_0_1, obj_2_2, Default::default(), Some(obj_3_4));
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_236<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = false;
  let obj_0_1 = obj1.to_string();
  let obj_2_2 = obj3.into_nodes();
  let obj_1_3 = obj2.to_string();
  let obj_3_4 = obj4;
  let obj_3_4 = obj_3_4.to_Symbols();
  let obj_6_0 = IR_STATE::new(obj_5_0, obj_0_1, obj_2_2, obj_1_3, Some(obj_3_4));
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_237<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = false;
  let obj_0_1 = obj1.to_string();
  let obj_1_2 = obj2.into_nodes();
  let obj_2_4 = obj3;
  let obj_2_4 = obj_2_4.to_Symbols();
  let obj_5_0 = IR_STATE::new(obj_4_0, obj_0_1, obj_1_2, Default::default(), Some(obj_2_4));
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_238<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_0_1 = obj1.to_string();
  let obj_3_2 = obj4.into_nodes();
  let obj_1_3 = obj2.to_string();
  let obj_6_0 = IR_STATE::new(obj_5_0, obj_0_1, obj_3_2, obj_1_3, None);
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_239<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_0_1 = obj1.to_string();
  let obj_2_2 = obj3.into_nodes();
  let obj_5_0 = IR_STATE::new(obj_4_0, obj_0_1, obj_2_2, Default::default(), None);
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_240<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = false;
  let obj_0_1 = obj1.to_string();
  let obj_2_2 = obj3.into_nodes();
  let obj_1_3 = obj2.to_string();
  let obj_5_0 = IR_STATE::new(obj_4_0, obj_0_1, obj_2_2, obj_1_3, None);
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_241<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_3_0 = false;
  let obj_0_1 = obj1.to_string();
  let obj_1_2 = obj2.into_nodes();
  let obj_4_0 = IR_STATE::new(obj_3_0, obj_0_1, obj_1_2, Default::default(), None);
  slots
    .assign(0, AstSlot(ASTNode::IR_STATE(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_242<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_2_0 = Production_Symbol::new(
    tok_0_0,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production_Symbol(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_243<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_244<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  slots.take(4);
  let AstSlot(obj6, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_0_0 = obj1;
  let mut obj_1_0 = obj2.into_nodes();
  let mut obj_3_0 = obj4.into_nodes();
  let obj_5_0 = obj6;
  let mut obj_7_0 = vec![];
  obj_7_0.push(obj_0_0);
  obj_7_0.append(&mut obj_1_0);
  obj_7_0.append(&mut obj_3_0);
  obj_7_0.push(obj_5_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_7_0), __rule_rng__, TokenRange::default()));
}

fn reducer_245<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let mut obj_2_0 = obj3.into_nodes();
  let obj_4_0 = obj5;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.append(&mut obj_2_0);
  obj_0_0.push(obj_4_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_246<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_0_0 = obj1;
  let mut obj_1_0 = obj2.into_nodes();
  let obj_3_0 = obj4;
  let mut obj_5_0 = vec![];
  obj_5_0.push(obj_0_0);
  obj_5_0.append(&mut obj_1_0);
  obj_5_0.push(obj_3_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_5_0), __rule_rng__, TokenRange::default()));
}

fn reducer_247<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_248<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_0_0 = obj1;
  let mut obj_1_0 = obj2.into_nodes();
  let mut obj_3_0 = obj4.into_nodes();
  let mut obj_5_0 = vec![];
  obj_5_0.push(obj_0_0);
  obj_5_0.append(&mut obj_1_0);
  obj_5_0.append(&mut obj_3_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_5_0), __rule_rng__, TokenRange::default()));
}

fn reducer_249<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let mut obj_2_0 = obj3.into_nodes();
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.append(&mut obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_250<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_0_0 = obj1;
  let mut obj_1_0 = obj2.into_nodes();
  let mut obj_3_0 = vec![];
  obj_3_0.push(obj_0_0);
  obj_3_0.append(&mut obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_3_0), __rule_rng__, TokenRange::default()));
}

fn reducer_251<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_252<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_253<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_254<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_0_0 = obj1.into_nodes();
  let obj_1_1 = obj2.into_nodes();
  let obj_3_0 = Grammar::new(
    obj_0_0,
    obj_1_1
      .into_iter()
      .map(|v| match v {
        ASTNode::Production(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::Grammar(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_255<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_1 = obj1.into_nodes();
  let obj_2_0 = Grammar::new(
    vec![],
    obj_0_1
      .into_iter()
      .map(|v| match v {
        ASTNode::Production(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::Grammar(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_256<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_257<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_258<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2.to_string();
  let mut obj_0_0 = obj1.into_strings();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_259<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1.to_string();
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_260<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_261<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_262<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_263<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_1_0 = obj2.into_nodes();
  let obj_4_0 = Syntax::new(
    obj_1_0
      .into_iter()
      .map(|v| match v {
        ASTNode::SyntaxField(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Syntax(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_264<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_265<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_266<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(obj5, ..) = slots.take(4);
  let AstSlot(obj6, __tok_rng_6, _) = slots.take(5);
  slots.take(6);
  let AstSlot(obj8, __tok_rng_8, _) = slots.take(7);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_8;
  let obj_9_1 = true;
  let tok_5_2 = __tok_rng_6;
  let tok_5_2 = tok_5_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_5_3 = obj6;
  let obj_4_4 = obj5;
  let obj_4_4 = obj_4_4.to_Priority();
  let obj_7_5 = obj8.into_nodes();
  let obj_1_6 = obj2.into_strings();
  let obj_10_0 = Production::new(
    false,
    obj_9_1,
    tok_5_2,
    obj_5_3,
    Some(obj_4_4),
    obj_7_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    obj_1_6,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_10_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_267<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  slots.take(5);
  let AstSlot(obj7, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_8_1 = true;
  let tok_4_2 = __tok_rng_5;
  let tok_4_2 = tok_4_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_4_3 = obj5;
  let obj_3_4 = obj4;
  let obj_3_4 = obj_3_4.to_Priority();
  let obj_6_5 = obj7.into_nodes();
  let obj_9_0 = Production::new(
    false,
    obj_8_1,
    tok_4_2,
    obj_4_3,
    Some(obj_3_4),
    obj_6_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_9_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_268<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, ..) = slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  slots.take(5);
  let AstSlot(obj7, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_8_1 = false;
  let tok_4_2 = __tok_rng_5;
  let tok_4_2 = tok_4_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_4_3 = obj5;
  let obj_3_4 = obj4;
  let obj_3_4 = obj_3_4.to_Priority();
  let obj_6_5 = obj7.into_nodes();
  let obj_1_6 = obj2.into_strings();
  let obj_9_0 = Production::new(
    false,
    obj_8_1,
    tok_4_2,
    obj_4_3,
    Some(obj_3_4),
    obj_6_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    obj_1_6,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_9_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_269<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  slots.take(4);
  let AstSlot(obj6, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_7_1 = false;
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_3 = obj4;
  let obj_2_4 = obj3;
  let obj_2_4 = obj_2_4.to_Priority();
  let obj_5_5 = obj6.into_nodes();
  let obj_8_0 = Production::new(
    false,
    obj_7_1,
    tok_3_2,
    obj_3_3,
    Some(obj_2_4),
    obj_5_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_270<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  slots.take(5);
  let AstSlot(obj7, __tok_rng_7, _) = slots.take(6);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_7;
  let obj_8_1 = true;
  let tok_4_2 = __tok_rng_5;
  let tok_4_2 = tok_4_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_4_3 = obj5;
  let obj_6_5 = obj7.into_nodes();
  let obj_1_6 = obj2.into_strings();
  let obj_9_0 = Production::new(
    false,
    obj_8_1,
    tok_4_2,
    obj_4_3,
    None,
    obj_6_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    obj_1_6,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_9_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_271<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  slots.take(4);
  let AstSlot(obj6, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_7_1 = true;
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_3 = obj4;
  let obj_5_5 = obj6.into_nodes();
  let obj_8_0 = Production::new(
    false,
    obj_7_1,
    tok_3_2,
    obj_3_3,
    None,
    obj_5_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_272<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(obj4, __tok_rng_4, _) = slots.take(3);
  slots.take(4);
  let AstSlot(obj6, __tok_rng_6, _) = slots.take(5);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_6;
  let obj_7_1 = false;
  let tok_3_2 = __tok_rng_4;
  let tok_3_2 = tok_3_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_3_3 = obj4;
  let obj_5_5 = obj6.into_nodes();
  let obj_1_6 = obj2.into_strings();
  let obj_8_0 = Production::new(
    false,
    obj_7_1,
    tok_3_2,
    obj_3_3,
    None,
    obj_5_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    obj_1_6,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_8_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_273<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  slots.take(3);
  let AstSlot(obj5, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let obj_6_1 = false;
  let tok_2_2 = __tok_rng_3;
  let tok_2_2 = tok_2_2.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_2_3 = obj3;
  let obj_4_5 = obj5.into_nodes();
  let obj_7_0 = Production::new(
    false,
    obj_6_1,
    tok_2_2,
    obj_2_3,
    None,
    obj_4_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::Production(Box::new(obj_7_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_274<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_275<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_276<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_277<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_278<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_279<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_HASH_NAME();
  let obj_3_0 = PushGoto::new(obj_1_0);
  slots
    .assign(0, AstSlot(ASTNode::PushGoto(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_280<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_2_0 = __rule_rng__.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_2_0 = tok_2_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_281<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_282<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_283<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_284<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_285<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_286<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_287<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_288<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_289<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_290<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_291<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_292<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_293<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_294<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_295<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_296<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_297<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_298<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe { &*_ctx_ }.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_299<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_2_0 = obj3;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_300<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_301<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2;
  let tok_1_0 = tok_1_0.parse::<u32>(unsafe { &*_ctx_ }.get_str());
  slots.assign(0, AstSlot(ASTNode::U32(tok_1_0), __rule_rng__, TokenRange::default()));
}

fn reducer_302<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  slots.assign(0, AstSlot(obj2, __rule_rng__, TokenRange::default()));
}

fn reducer_303<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_304<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_305<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_306<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_307<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_308<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_309<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_310<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_1 = true;
  let obj_3_0 = AST_BOOL::new(
    None,
    obj_2_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_311<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_1 = false;
  let obj_3_0 = AST_BOOL::new(
    None,
    obj_2_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_312<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.parse::<f64>(unsafe { &*_ctx_ }.get_str());
  let obj_2_0 = AST_NUMBER::new(tok_0_0);
  slots.assign(
    0,
    AstSlot(ASTNode::AST_NUMBER(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_313<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_314<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_2_0 = __tok_rng_3.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_0_1 = obj1;
  let obj_4_0 = AST_Member::new(tok_2_0, obj_0_1);
  slots.assign(
    0,
    AstSlot(ASTNode::AST_Member(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_315<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_316<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_317<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_318<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_319<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(_, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_1_0 = __tok_rng_2.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_0_0 = obj1.into_tokens();
  obj_0_0.push(tok_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_320<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_321<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_322<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_323<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_324<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let mut obj_2_0 = vec![];
  obj_2_0.push(tok_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_325<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_4_0 = __tok_rng_5.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_4_0 = tok_4_0.to_string();
  let obj_1_1 = obj2.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let obj_6_0 = Import::new(
    tok_4_0,
    obj_1_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_326<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(_, __tok_rng_5, _) = slots.take(4);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_5;
  let tok_4_0 = __tok_rng_5.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_4_0 = tok_4_0.to_string();
  let obj_1_1 = obj2.into_tokens();
  let obj_1_1 = (obj_1_1.first().unwrap() + obj_1_1.last().unwrap()).to_string();
  let obj_6_0 = Import::new(
    tok_4_0,
    obj_1_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_327<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = EOFSymbol::new(
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::EOFSymbol(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_328<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_STRING::new(
    Some(obj_1_0),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_STRING(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_329<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_STRING::new(
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::AST_STRING(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_330<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let tok_0_0 = tok_0_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(tok_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_331<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_332<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_1_1 = obj2;
  let obj_1_1 = obj_1_1.to_RGBA();
  let obj_3_0 = SyntaxSpec::new(tok_0_0, Some(obj_1_1));
  slots.assign(
    0,
    AstSlot(ASTNode::SyntaxSpec(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_333<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_1 = obj1;
  let obj_0_1 = obj_0_1.to_RGBA();
  let obj_2_0 = SyntaxSpec::new(Default::default(), Some(obj_0_1));
  slots.assign(
    0,
    AstSlot(ASTNode::SyntaxSpec(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_334<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let tok_0_0 = __tok_rng_1.to_token(unsafe { &mut *_ctx_ }.get_reader_mut());
  let obj_2_0 = SyntaxSpec::new(tok_0_0, None);
  slots.assign(
    0,
    AstSlot(ASTNode::SyntaxSpec(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_335<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_0_1 = obj1;
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Terminal();
  let obj_5_0 = List_Production::new(
    false,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::List_Production(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_336<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_0_1 = obj1;
  let obj_4_0 = List_Production::new(
    false,
    obj_0_1,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::List_Production(Box::new(obj_4_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_337<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(obj3, ..) = slots.take(2);
  let AstSlot(_, __tok_rng_4, _) = slots.take(3);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_4;
  let obj_5_0 = true;
  let obj_0_1 = obj1;
  let obj_2_2 = obj3;
  let obj_2_2 = obj_2_2.to_Terminal();
  let obj_6_0 = List_Production::new(
    obj_5_0,
    obj_0_1,
    Some(obj_2_2),
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::List_Production(Box::new(obj_6_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_338<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let obj_4_0 = true;
  let obj_0_1 = obj1;
  let obj_5_0 = List_Production::new(
    obj_4_0,
    obj_0_1,
    None,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(ASTNode::List_Production(Box::new(obj_5_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_339<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_340<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_341<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  slots.assign(0, AstSlot(obj1, __rule_rng__, TokenRange::default()));
}

fn reducer_342<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, __tok_rng_3, _) = slots.take(2);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_3;
  let tok_0_0 = __tok_rng_1;
  let tok_0_0 = tok_0_0.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let tok_2_1 = __tok_rng_3;
  let tok_2_1 = tok_2_1.to_slice(unsafe { &*_ctx_ }.get_str()).to_string();
  let obj_4_0 = Production_Import_Symbol::new(
    tok_0_0,
    tok_2_1,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots.assign(
    0,
    AstSlot(
      ASTNode::Production_Import_Symbol(Box::new(obj_4_0)),
      __rule_rng__,
      TokenRange::default(),
    ),
  );
}

fn reducer_343<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let obj_1_0 = obj_1_0.to_Init();
  let obj_3_0 = AST_BOOL::new(
    Some(obj_1_0),
    false,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_3_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_344<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = AST_BOOL::new(
    None,
    false,
    __rule_rng__.to_token(
      unsafe {
        {
          &mut *_ctx_
        }
      }
      .get_reader_mut(),
    ),
  );
  slots
    .assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()));
}

fn reducer_345<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(_, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_2_0 = DEFINED_TYPE_NUM::new();
  slots.assign(
    0,
    AstSlot(ASTNode::DEFINED_TYPE_NUM(Box::new(obj_2_0)), __rule_rng__, TokenRange::default()),
  );
}

fn reducer_346<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_347<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_348<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let AstSlot(obj2, __tok_rng_2, _) = slots.take(1);
  let __rule_rng__ = __tok_rng_1 + __tok_rng_2;
  let obj_1_0 = obj2;
  let mut obj_0_0 = obj1.into_nodes();
  obj_0_0.push(obj_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_0_0), __rule_rng__, TokenRange::default()));
}

fn reducer_349<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_350<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

fn reducer_351<R: Reader + UTF8Reader, M, const UP: bool>(
  _ctx_: *mut ParseContext<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>, UP>,
) {
  let AstSlot(obj1, __tok_rng_1, _) = slots.take(0);
  let __rule_rng__ = __tok_rng_1;
  let obj_0_0 = obj1;
  let mut obj_2_0 = vec![];
  obj_2_0.push(obj_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(obj_2_0), __rule_rng__, TokenRange::default()));
}

struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>(
  pub [Reducer<R, M, ASTNode, UP>; 352],
);

impl<R: Reader + UTF8Reader, M, const UP: bool> ReduceFunctions<R, M, UP> {
  pub const fn new() -> Self {
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
    ])
  }
}

#[link(name = "sherpa", kind = "static")]
extern "C" {
  fn init(ctx: *mut u8, reader: *mut u8);
  fn next(ctx: *mut u8) -> ParseActionType;
  fn prime(ctx: *mut u8, start_point: u32);
  fn drop(ctx: *mut u8);
}
pub trait Reader: ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug {}
impl<T: ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug> Reader for T {}

pub struct Parser<T: Reader, M>(ParseContext<T, M>, T);

impl<T: Reader, M> Parser<T, M> {
  /// Create a new parser context to parser the input with
  /// the grammar `sherpa
  #[inline(always)]

  fn new(mut reader: T) -> Self {
    let mut parser = Self(ParseContext::<T, M>::new_llvm(), reader);
    parser.construct_context();
    parser
  }

  /// Initialize the parser to recognize the given starting production
  /// within the input. This method is chainable.
  #[inline(always)]

  fn set_start_point(&mut self, start_point: u64) -> &mut Self {
    unsafe {
      let _ptr = &mut self.0 as *const ParseContext<T, M>;
      prime(_ptr as *mut u8, start_point as u32);
    }
    self
  }

  #[inline(always)]

  fn construct_context(&mut self) {
    unsafe {
      let _ptr = &mut self.0 as *const ParseContext<T, M>;
      let _rdr = &mut self.1 as *const T;
      init(_ptr as *mut u8, _rdr as *mut u8);
    }
  }

  #[inline(always)]

  fn destroy_context(&mut self) {
    let _ptr = &mut self.0 as *const ParseContext<T, M>;

    unsafe {
      drop(_ptr as *mut u8);
    }
  }

  pub fn new_grammar_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(0);
    ctx
  }

  pub fn new_ast_struct_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(1);
    ctx
  }

  pub fn new_ast_expression_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(2);
    ctx
  }

  pub fn new_ir_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(3);
    ctx
  }

  pub fn new_type_eval_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(4);
    ctx
  }

  pub fn new_escaped_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(5);
    ctx
  }
}

impl<T: Reader, M> Iterator for Parser<T, M> {
  type Item = ParseActionType;

  #[inline(always)]

  fn next(&mut self) -> Option<Self::Item> {
    unsafe {
      if !self.0.is_active {
        None
      } else {
        let _ptr = &mut self.0 as *const ParseContext<T, M>;
        Some(next(_ptr as *mut u8))
      }
    }
  }
}

impl<T: Reader, M> Drop for Parser<T, M> {
  fn drop(&mut self) {
    unsafe {
      self.destroy_context();
    }
  }
}

pub mod ast {
  use super::*;

  impl AstObject for ASTNode {}
  type ASTSlot = (ASTNode, TokenRange, TokenRange);

  #[link(name = "sherpa", kind = "static")]
  extern "C" {
    fn ast_parse(
      ctx: *mut u8,
      reducers: *const u8,
      shift_handler: *const u8,
      result_handler: *const u8,
    ) -> ParseResult<ASTNode>;
  }

  pub fn grammar_from<'a>(reader: UTF8StringReader) -> Result<Box<Grammar>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32, false> =
      ReduceFunctions::<UTF8StringReader, u32, false>::new();
    let mut ctx = Parser::new_grammar_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete(AstSlot(obj1, ..)) => {
        let obj_0_0 = obj1;
        let obj_0_0 = obj_0_0.to_Grammar();
        Ok(obj_0_0)
      }

      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&mut ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn ast_struct_from<'a>(
    reader: UTF8StringReader,
  ) -> Result<Box<AST_Struct>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32, false> =
      ReduceFunctions::<UTF8StringReader, u32, false>::new();
    let mut ctx = Parser::new_ast_struct_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete(AstSlot(obj1, ..)) => {
        let obj_0_0 = obj1;
        let obj_0_0 = obj_0_0.to_AST_Struct();
        Ok(obj_0_0)
      }

      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&mut ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn ast_expression_from<'a>(reader: UTF8StringReader) -> Result<ASTNode, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32, false> =
      ReduceFunctions::<UTF8StringReader, u32, false>::new();
    let mut ctx = Parser::new_ast_expression_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete(AstSlot(obj1, ..)) => {
        let obj_0_0 = obj1;
        Ok(obj_0_0)
      }

      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&mut ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn ir_from<'a>(reader: UTF8StringReader) -> Result<Box<IR_STATE>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32, false> =
      ReduceFunctions::<UTF8StringReader, u32, false>::new();
    let mut ctx = Parser::new_ir_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete(AstSlot(obj1, ..)) => {
        let obj_0_0 = obj1;
        let obj_0_0 = obj_0_0.to_IR_STATE();
        Ok(obj_0_0)
      }

      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&mut ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn type_eval_from<'a>(reader: UTF8StringReader) -> Result<ASTNode, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32, false> =
      ReduceFunctions::<UTF8StringReader, u32, false>::new();
    let mut ctx = Parser::new_type_eval_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete(AstSlot(obj1, ..)) => {
        let obj_0_0 = obj1;
        Ok(obj_0_0)
      }

      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&mut ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn escaped_from<'a>(reader: UTF8StringReader) -> Result<Vec<String>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32, false> =
      ReduceFunctions::<UTF8StringReader, u32, false>::new();
    let mut ctx = Parser::new_escaped_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete(AstSlot(obj1, ..)) => {
        let obj_0_0 = obj1.into_strings();
        Ok(obj_0_0)
      }

      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&mut ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }
}
