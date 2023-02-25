use sherpa_runtime::{
  llvm_parser::LLVMByteReader,
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
use std::{
  collections::hash_map::DefaultHasher,
  hash::{Hash, Hasher},
};

use super::parse_context_old::{ParseContextOld, Reducer};

impl Grammar {
  /// Create a [Grammar] node from a `String` input.
  pub fn from_string(input: String) -> Result<Box<Grammar>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::grammar_from(reader)
  }

  /// Create a [Grammar] node from a `&str` input.
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

  /// Create a [AST_Struct] node from a `&str` input.
  pub fn from_str(input: &str) -> Result<Box<AST_Struct>, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ast_struct_from(reader)
  }
}
impl ASTNode {
  /// Create a [Node] from a `String` input.
  pub fn parse_string_as_ast_expression(input: String) -> Result<ASTNode, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ast_expression_from(reader)
  }

  /// Create a [Node] from a `&str` input.
  pub fn parse_str_as_ast_expression(input: &str) -> Result<ASTNode, SherpaParseError> {
    let reader = UTF8StringReader::from(input);
    ast::ast_expression_from(reader)
  }
}
impl IR_STATE {
  /// Create a [IR_STATE] node from a `String` input.
  pub fn from_string(input: String) -> Result<Box<IR_STATE>, SherpaParseError> {
    let reader = UTF8StringReader::from(&input);
    ast::ir_from(reader)
  }

  /// Create a [IR_STATE] node from a `&str` input.
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

  fn into_strings(self) -> Vec<String> {
    match self {
      ASTNode::STRINGS(strings) => strings,
      _ => Default::default(),
    }
  }

  pub fn to_string(&self) -> String {
    match self {
      &ASTNode::BOOL(val) => {
        if val {
          String::from("true")
        } else {
          String::from("false")
        }
      }
      ASTNode::STRING(string) => string.to_owned(),
      ASTNode::TOKEN(val) => val.to_string(),
      _ => self.to_token().to_string(),
    }
  }

  pub fn to_token(&self) -> Token {
    match self {
      ASTNode::AST_NamedReference(node) => node.tok.clone(),
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
      ASTNode::TOKEN(val) => val.clone(),
      _ => Token::empty(),
    }
  }

  pub fn to_bool(&self) -> bool {
    self.to_u8() != 0
  }

  pub fn is_numeric(&self) -> bool {
    use ASTNode::*;
    matches!(
      self,
      F64(_) | F32(_) | I64(_) | I32(_) | I16(_) | I8(_) | U64(_) | U32(_) | U16(_) | U8(_)
    )
  }
}

impl Hash for ASTNode {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    use ASTNode::*;
    match self {
      NONE => {}
      NODES(nodes) => {
        for node in nodes {
          node.hash(hasher);
        }
      }
      STRING(string) => string.hash(hasher),
      STRINGS(strings) => strings.hash(hasher),
      F64(val) => val.to_le_bytes().hash(hasher),
      F32(val) => val.to_le_bytes().hash(hasher),
      I64(val) => val.hash(hasher),
      I32(val) => val.hash(hasher),
      I16(val) => val.hash(hasher),
      I8(val) => val.hash(hasher),
      U64(val) => val.hash(hasher),
      U32(val) => val.hash(hasher),
      U16(val) => val.hash(hasher),
      U8(val) => val.hash(hasher),
      BOOL(val) => val.hash(hasher),
      F32Vec(val) => {
        for val in val {
          val.to_le_bytes().hash(hasher)
        }
      }
      F64Vec(val) => {
        for val in val {
          val.to_le_bytes().hash(hasher)
        }
      }
      I64Vec(val) => val.hash(hasher),
      I32Vec(val) => val.hash(hasher),
      I16Vec(val) => val.hash(hasher),
      I8Vec(val) => val.hash(hasher),
      U64Vec(val) => val.hash(hasher),
      U32Vec(val) => val.hash(hasher),
      U16Vec(val) => val.hash(hasher),
      U8Vec(val) => val.hash(hasher),
      TOKEN(val) => val.to_string().replace(" ", "").replace("\n", "").hash(hasher),
      TOKENS(val) => {
        for val in val {
          val.to_string().replace(" ", "").replace("\n", "").hash(hasher);
        }
      }
      AST_NamedReference(node) => node.hash(hasher),
      Reduce(node) => node.hash(hasher),
      PeekToken(node) => node.hash(hasher),
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
      SyntaxSpec(node) => node.hash(hasher),
      Rule(node) => node.hash(hasher),
      AnnotatedSymbol(node) => node.hash(hasher),
      AST_Property(node) => node.hash(hasher),
      Ascript(node) => node.hash(hasher),
      Num(node) => node.hash(hasher),
      Production_Terminal_Symbol(node) => node.hash(hasher),
      Terminal(node) => node.hash(hasher),
      Recovery(node) => node.hash(hasher),
    }
  }
}

impl Default for ASTNode {
  fn default() -> Self {
    Self::NONE
  }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum ASTNodeType {
  Undefined,
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
pub trait GetASTNodeType {
  fn get_type(&self) -> ASTNodeType;
}
impl GetASTNodeType for ASTNode {
  fn get_type(&self) -> ASTNodeType {
    match self {
      ASTNode::AST_NamedReference(..) => ASTNodeType::AST_NamedReference,
      ASTNode::Reduce(..) => ASTNodeType::Reduce,
      ASTNode::PeekToken(..) => ASTNodeType::PeekToken,
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
#[derive(Debug, Clone)]
pub struct AST_NamedReference {
  pub value: String,
  pub tok:   Token,
}
impl AST_NamedReference {
  #[inline]
  pub fn new(value: String, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_NamedReference
  }
}
impl Hash for AST_NamedReference {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Reduce {
  pub len:     i32,
  pub prod_id: i32,
  pub rule_id: i32,
}
impl Reduce {
  #[inline]
  pub fn new(len: i32, prod_id: i32, rule_id: i32) -> Self {
    Self { len, prod_id, rule_id }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Reduce
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
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PeekToken
  }
}
impl Hash for PeekToken {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct DEFAULT {
  pub instructions: Vec<ASTNode>,
}
impl DEFAULT {
  #[inline]
  pub fn new(instructions: Vec<ASTNode>) -> Self {
    Self { instructions }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::DEFAULT
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
  #[inline]
  pub fn new(state: Box<HASH_NAME>) -> Self {
    Self { state }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PushGoto
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
  #[inline]
  pub fn new(ids: Vec<Box<Num>>) -> Self {
    Self { ids }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::TokenAssign
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
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipToken
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
  #[inline]
  pub fn new(left: ASTNode, right: ASTNode, tok: Token) -> Self {
    Self { left, right, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Add
  }
}
impl Hash for AST_Add {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.left.hash(hasher);
    self.right.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct ScanShift {}
impl ScanShift {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ScanShift
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
  #[inline]
  pub fn new(EMPTY: bool) -> Self {
    Self { EMPTY }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ShiftToken
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
  #[inline]
  pub fn new(specs: Vec<Box<SyntaxField>>) -> Self {
    Self { specs }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Syntax
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
  #[inline]
  pub fn new(reference: ASTNode, spec: Box<SyntaxSpec>) -> Self {
    Self { reference, spec }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SyntaxField
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
  #[inline]
  pub fn new(val: String) -> Self {
    Self { val }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::HASH_NAME
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
  #[inline]
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
impl Hash for List_Production {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.optional.hash(hasher);
    self.symbols.hash(hasher);
    self.terminal_symbol.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct PeekReset {}
impl PeekReset {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PeekReset
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
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Pop
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
  #[inline]
  pub fn new(initializer: Vec<ASTNode>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Vector
  }
}
impl Hash for AST_Vector {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    for val in &self.initializer {
      val.hash(hasher);
    }
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Production_Symbol {
  pub name: String,
  pub tok:  Token,
}
impl Production_Symbol {
  #[inline]
  pub fn new(name: String, tok: Token) -> Self {
    Self { name, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production_Symbol
  }
}
impl Hash for Production_Symbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.name.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct SkipTokenScanless {}
impl SkipTokenScanless {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipTokenScanless
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
  #[inline]
  pub fn new(preamble: Vec<ASTNode>, productions: Vec<Box<Production>>, tok: Token) -> Self {
    Self { preamble, productions, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Grammar
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
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_U64 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}
impl AST_U64 {
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U64
  }
}
impl Hash for AST_U64 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Priority {
  pub exclusive: bool,
  pub val:       u32,
}
impl Priority {
  #[inline]
  pub fn new(exclusive: bool, val: u32) -> Self {
    Self { exclusive, val }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Priority
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
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Pass
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
  #[inline]
  pub fn new(ids: Box<Num>, instructions: Vec<ASTNode>, mode: String) -> Self {
    Self { ids, instructions, mode }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ASSERT
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
  #[inline]
  pub fn new(reference: String, uri: String, tok: Token) -> Self {
    Self { reference, uri, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Import
  }
}
impl Hash for Import {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.reference.hash(hasher);
    self.uri.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Init {
  pub expression: ASTNode,
}
impl Init {
  #[inline]
  pub fn new(expression: ASTNode) -> Self {
    Self { expression }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Init
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
  #[inline]
  pub fn new(value: i64, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_IndexReference
  }
}
impl Hash for AST_IndexReference {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
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
  #[inline]
  pub fn new(a: u32, b: u32, g: u32, r: u32) -> Self {
    Self { a, b, g, r }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::RGBA
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
  #[inline]
  pub fn new(property: Token, reference: ASTNode) -> Self {
    Self { property, reference }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Member
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
  #[inline]
  pub fn new(module: String, name: String, tok: Token) -> Self {
    Self { module, name, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production_Import_Symbol
  }
}
impl Hash for Production_Import_Symbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.module.hash(hasher);
    self.name.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_I32 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}
impl AST_I32 {
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I32
  }
}
impl Hash for AST_I32 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Goto {
  pub state: Box<HASH_NAME>,
}
impl Goto {
  #[inline]
  pub fn new(state: Box<HASH_NAME>) -> Self {
    Self { state }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Goto
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
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipPeekToken
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
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I8
  }
}
impl Hash for AST_I8 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_F32 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}
impl AST_F32 {
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_F32
  }
}
impl Hash for AST_F32 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Symbols {
  pub expected: Vec<Box<Num>>,
  pub skipped:  Vec<Box<Num>>,
}
impl Symbols {
  #[inline]
  pub fn new(expected: Vec<Box<Num>>, skipped: Vec<Box<Num>>) -> Self {
    Self { expected, skipped }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Symbols
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
  #[inline]
  pub fn new(range: Option<Box<Range>>) -> Self {
    Self { range }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Token
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
  #[inline]
  pub fn new(production: ASTNode, reference: Token) -> Self {
    Self { production, reference }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Export
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
  #[inline]
  pub fn new(name: String) -> Self {
    Self { name }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Name
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
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, value: bool, tok: Token) -> Self {
    Self { initializer, value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_BOOL
  }
}
impl Hash for AST_BOOL {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.value.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Group_Production {
  pub rules: Vec<Box<Rule>>,
  pub tok:   Token,
}
impl Group_Production {
  #[inline]
  pub fn new(rules: Vec<Box<Rule>>, tok: Token) -> Self {
    Self { rules, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Group_Production
  }
}
impl Hash for Group_Production {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    for val in &self.rules {
      val.hash(hasher);
    }
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Fail {}
impl Fail {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Fail
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
  #[inline]
  pub fn new(statements: Vec<ASTNode>, tok: Token) -> Self {
    Self { statements, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Statements
  }
}
impl Hash for AST_Statements {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    for val in &self.statements {
      val.hash(hasher);
    }
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_U8 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}
impl AST_U8 {
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U8
  }
}
impl Hash for AST_U8 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
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
  #[inline]
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
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct ShiftTokenScanless {}
impl ShiftTokenScanless {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ShiftTokenScanless
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
  #[inline]
  pub fn new(state: Box<HASH_NAME>) -> Self {
    Self { state }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PushExceptHandler
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
  #[inline]
  pub fn new(symbols: Vec<ASTNode>) -> Self {
    Self { symbols }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Ignore
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
  #[inline]
  pub fn new(value: Option<Box<Init>>, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_STRING
  }
}
impl Hash for AST_STRING {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_U32 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}
impl AST_U32 {
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U32
  }
}
impl Hash for AST_U32 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_ClassId {
  pub value: String,
  pub tok:   Token,
}
impl AST_ClassId {
  #[inline]
  pub fn new(value: String, tok: Token) -> Self {
    Self { value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_ClassId
  }
}
impl Hash for AST_ClassId {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.value.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
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
  #[inline]
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
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I64
  }
}
impl Hash for AST_I64 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct PeekTokenScanless {}
impl PeekTokenScanless {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::PeekTokenScanless
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
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_U16
  }
}
impl Hash for AST_U16 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct SkipPeekTokenScanless {}
impl SkipPeekTokenScanless {
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SkipPeekTokenScanless
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
  #[inline]
  pub fn new() -> Self {
    Self {}
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Accept
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
  #[inline]
  pub fn new(val: String, tok: Token) -> Self {
    Self { val, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::ClassSymbol
  }
}
impl Hash for ClassSymbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.val.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_F64 {
  pub initializer: Option<Box<Init>>,
  pub tok:         Token,
}
impl AST_F64 {
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_F64
  }
}
impl Hash for AST_F64 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_Map {
  pub key: ASTNode,
  pub val: ASTNode,
  pub tok: Token,
}
impl AST_Map {
  #[inline]
  pub fn new(key: ASTNode, val: ASTNode, tok: Token) -> Self {
    Self { key, val, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Map
  }
}
impl Hash for AST_Map {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.key.hash(hasher);
    self.val.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AnyGroup {
  pub symbols:   Vec<ASTNode>,
  pub unordered: bool,
  pub tok:       Token,
}
impl AnyGroup {
  #[inline]
  pub fn new(symbols: Vec<ASTNode>, unordered: bool, tok: Token) -> Self {
    Self { symbols, unordered, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AnyGroup
  }
}
impl Hash for AnyGroup {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    for val in &self.symbols {
      val.hash(hasher);
    }
    self.unordered.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Range {
  pub end_trim:   i32,
  pub start_trim: i32,
}
impl Range {
  #[inline]
  pub fn new(end_trim: i32, start_trim: i32) -> Self {
    Self { end_trim, start_trim }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Range
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
  #[inline]
  pub fn new(initializer: Option<Box<Init>>, tok: Token) -> Self {
    Self { initializer, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_I16
  }
}
impl Hash for AST_I16 {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.initializer.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AST_NUMBER {
  pub value: f64,
}
impl AST_NUMBER {
  #[inline]
  pub fn new(value: f64) -> Self {
    Self { value }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_NUMBER
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
  #[inline]
  pub fn new(props: Vec<ASTNode>, typ: Token, tok: Token) -> Self {
    Self { props, typ, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Struct
  }
}
impl Hash for AST_Struct {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    for val in &self.props {
      val.hash(hasher);
    }
    self.typ.to_string().replace(" ", "").replace("\n", "").hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct SyntaxSpec {
  pub id:  Token,
  pub rgb: Option<Box<RGBA>>,
}
impl SyntaxSpec {
  #[inline]
  pub fn new(id: Token, rgb: Option<Box<RGBA>>) -> Self {
    Self { id, rgb }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::SyntaxSpec
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
  #[inline]
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
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct AnnotatedSymbol {
  pub is_optional: bool,
  pub prority:     Option<Box<Priority>>,
  pub reference:   String,
  pub symbol:      ASTNode,
  pub tok:         Token,
}
impl AnnotatedSymbol {
  #[inline]
  pub fn new(
    is_optional: bool,
    prority: Option<Box<Priority>>,
    reference: String,
    symbol: ASTNode,
    tok: Token,
  ) -> Self {
    Self { is_optional, prority, reference, symbol, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AnnotatedSymbol
  }
}
impl Hash for AnnotatedSymbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_optional.hash(hasher);
    self.prority.hash(hasher);
    self.reference.hash(hasher);
    self.symbol.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
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
  #[inline]
  pub fn new(id: String, named_reference: String, value: Option<ASTNode>, tok: Token) -> Self {
    Self { id, named_reference, value, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::AST_Property
  }
}
impl Hash for AST_Property {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.id.hash(hasher);
    self.named_reference.hash(hasher);
    self.value.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Ascript {
  pub ast: ASTNode,
  pub tok: Token,
}
impl Ascript {
  #[inline]
  pub fn new(ast: ASTNode, tok: Token) -> Self {
    Self { ast, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Ascript
  }
}
impl Hash for Ascript {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.ast.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Num {
  pub val: i64,
}
impl Num {
  #[inline]
  pub fn new(val: i64) -> Self {
    Self { val }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Num
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
  #[inline]
  pub fn new(production: ASTNode, tok: Token) -> Self {
    Self { production, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Production_Terminal_Symbol
  }
}
impl Hash for Production_Terminal_Symbol {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.production.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Terminal {
  pub is_exclusive: bool,
  pub val:          String,
  pub tok:          Token,
}
impl Terminal {
  #[inline]
  pub fn new(is_exclusive: bool, val: String, tok: Token) -> Self {
    Self { is_exclusive, val, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Terminal
  }
}
impl Hash for Terminal {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.is_exclusive.hash(hasher);
    self.val.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}
#[derive(Debug, Clone)]
pub struct Recovery {
  pub state: Box<IR_STATE>,
  pub tok:   Token,
}
impl Recovery {
  #[inline]
  pub fn new(state: Box<IR_STATE>, tok: Token) -> Self {
    Self { state, tok }
  }

  pub fn get_type(&self) -> ASTNodeType {
    ASTNodeType::Recovery
  }
}
impl Hash for Recovery {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
    self.get_type().hash(hasher);
    self.state.hash(hasher);
    self.tok.to_string().replace(" ", "").replace("\n", "").hash(hasher);
  }
}

fn default_fn<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  if slots.len() > 1 {
    let AstSlot(_, rng, _) = slots.take(0);
    let last = slots.take(slots.len() - 1);
    for index in 1..slots.len() - 1 {
      slots.take(index);
    }
    slots.assign(0, AstSlot(last.0, rng + last.1, TokenRange::default()));
  }
}

/*
numeric_convert => "u8" convert_initializer
*/
fn ast_fn000<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_U8::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u8"
*/
fn ast_fn001<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_U8::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U8(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u16" convert_initializer
*/
fn ast_fn002<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_U16::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u16"
*/
fn ast_fn003<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_U16::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U16(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u32" convert_initializer
*/
fn ast_fn004<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_U32::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u32"
*/
fn ast_fn005<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_U32::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U32(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u64" convert_initializer
*/
fn ast_fn006<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_U64::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "u64"
*/
fn ast_fn007<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_U64::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_U64(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i8" convert_initializer
*/
fn ast_fn008<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_I8::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i8"
*/
fn ast_fn009<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_I8::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I8(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i16" convert_initializer
*/
fn ast_fn010<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_I16::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i16"
*/
fn ast_fn011<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_I16::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I16(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i32" convert_initializer
*/
fn ast_fn012<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_I32::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i32"
*/
fn ast_fn013<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_I32::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I32(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i64" convert_initializer
*/
fn ast_fn014<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_I64::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "i64"
*/
fn ast_fn015<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_I64::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_I64(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "f32" convert_initializer
*/
fn ast_fn016<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_F32::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "f32"
*/
fn ast_fn017<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_F32::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_F32(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "f64" convert_initializer
*/
fn ast_fn018<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_F64::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => "f64"
*/
fn ast_fn019<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_F64::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_F64(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
red => "r" tk:integer
*/
fn ast_fn020<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_u32();
  slots.assign(0, AstSlot(ASTNode::U32(ref_1_0), rng, TokenRange::default()))
}

/*
state_reference => "state" '[' tk:state_hash_token ']'
*/
fn ast_fn021<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = rng2.to_token(_ctx_.get_reader());
  let ref_2_0 = ref_2_0.to_string();
  let ref_5_0 = HASH_NAME::new(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::HASH_NAME(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
any_group => "[" "unordered" any_group_list_1 ']'
*/
fn ast_fn022<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = i2.into_nodes();
  let ref_5_1 = true;
  let ref_6_0 = AnyGroup::new(ref_2_0, ref_5_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AnyGroup(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
any_group => "[" any_group_list_1 ']'
*/
fn ast_fn023<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1.into_nodes();
  let ref_4_1 = false;
  let ref_5_0 = AnyGroup::new(ref_1_0, ref_4_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AnyGroup(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
ast_definition => ":ast" body
*/
fn ast_fn024<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_3_0 = Ascript::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Ascript(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
struct_prop => identifier ":" expression
*/
fn ast_fn025<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_2 = i2;
  let ref_4_0 =
    AST_Property::new(ref_0_0, String::new(), Some(ref_2_2), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
struct_prop => identifier ":" struct
*/
fn ast_fn026<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_2 = i2;
  let ref_4_0 =
    AST_Property::new(ref_0_0, String::new(), Some(ref_2_2), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
struct_prop => identifier
*/
fn ast_fn027<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_0_1 = rng0.to_token(_ctx_.get_reader());
  let ref_0_1 = ref_0_1.to_string();
  let ref_2_0 = AST_Property::new(ref_0_0, ref_0_1, None, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Property(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
struct_prop => class_identifier
*/
fn ast_fn028<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_0 = AST_ClassId::new(ref_0_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_ClassId(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
export_clause => "EXPORT" non_terminal "AS" identifier
*/
fn ast_fn030<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_1_0 = i1;
  let ref_3_1 = rng3.to_token(_ctx_.get_reader());
  let ref_5_0 = Export::new(ref_1_0, ref_3_1);
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
export_clause => "EXPORT" non_terminal "as" identifier
*/
fn ast_fn031<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_1_0 = i1;
  let ref_3_1 = rng3.to_token(_ctx_.get_reader());
  let ref_5_0 = Export::new(ref_1_0, ref_3_1);
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
export_clause => "EXPORT" non_terminal
*/
fn ast_fn032<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_3_0 = Export::new(ref_1_0, Default::default());
  slots.assign(0, AstSlot(ASTNode::Export(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
production_list_1 => production_list_1 "," template_name
*/
fn ast_fn033<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2.to_string();
  let mut ref_0_0 = i0.into_strings();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(ref_0_0), rng, TokenRange::default()))
}

/*
production_list_1 => template_name
*/
fn ast_fn034<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0.to_string();
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::STRINGS(ref_2_0), rng, TokenRange::default()))
}

/*
struct => "{" type_identifier "," struct_list_1 '}'
*/
fn ast_fn037<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(_, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_3_0 = i3.into_nodes();
  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_6_0 = AST_Struct::new(ref_3_0, ref_1_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
struct => "{" type_identifier '}'
*/
fn ast_fn038<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_4_0 = AST_Struct::new(Default::default(), ref_1_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Struct(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
expected_symbols => 'symbols:' 'expected' token_id_list 'skipped' token_id_list
*/
fn ast_fn039<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_2_0 = i2.into_nodes();
  let ref_4_1 = i4.into_nodes();
  let ref_6_0 = Symbols::new(
    ref_2_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    ref_4_1
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Symbols(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
expected_symbols => 'symbols:' 'expected' token_id_list
*/
fn ast_fn040<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2.into_nodes();
  let ref_4_0 = Symbols::new(
    ref_2_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
  );
  slots.assign(0, AstSlot(ASTNode::Symbols(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
alpha => "a" tk:integer
*/
fn ast_fn041<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_u32();
  slots.assign(0, AstSlot(ASTNode::U32(ref_1_0), rng, TokenRange::default()))
}

/*
terminal_non_terminal => "tk:" non_terminal
*/
fn ast_fn042<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_3_0 = Production_Terminal_Symbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(
    0,
    AstSlot(ASTNode::Production_Terminal_Symbol(Box::new(ref_3_0)), rng, TokenRange::default()),
  )
}

/*
rule_list_1 => rule_list_1 annotated_symbol
*/
fn ast_fn043<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
rule_list_1 => rule_list_1 any_group
*/
fn ast_fn044<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
rule_list_1 => annotated_symbol
*/
fn ast_fn045<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
rule_list_1 => any_group
*/
fn ast_fn046<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
ignore_clause => "IGNORE" "{" ignore_clause_list_1 "}"
*/
fn ast_fn047<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = i2.into_nodes();
  let ref_5_0 = Ignore::new(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::Ignore(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
token_num => tk:integer
*/
fn ast_fn048<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_i64();
  let ref_2_0 = Num::new(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::Num(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "assign" "token" token_id_list
*/
fn ast_fn050<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2.into_nodes();
  let ref_4_0 = TokenAssign::new(
    ref_2_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Num(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::TokenAssign(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "pop"
*/
fn ast_fn051<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = Pop::new();
  slots.assign(0, AstSlot(ASTNode::Pop(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "pass"
*/
fn ast_fn052<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = Pass::new();
  slots.assign(0, AstSlot(ASTNode::Pass(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "skip-token"
*/
fn ast_fn053<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = SkipToken::new();
  slots.assign(0, AstSlot(ASTNode::SkipToken(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "skip-token-scanless"
*/
fn ast_fn054<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = SkipTokenScanless::new();
  slots
    .assign(0, AstSlot(ASTNode::SkipTokenScanless(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "fail"
*/
fn ast_fn055<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = Fail::new();
  slots.assign(0, AstSlot(ASTNode::Fail(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "peek-token"
*/
fn ast_fn056<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = PeekToken::new();
  slots.assign(0, AstSlot(ASTNode::PeekToken(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "peek-token-scanless"
*/
fn ast_fn057<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = PeekTokenScanless::new();
  slots
    .assign(0, AstSlot(ASTNode::PeekTokenScanless(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "peek-skip"
*/
fn ast_fn058<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = SkipPeekToken::new();
  slots.assign(0, AstSlot(ASTNode::SkipPeekToken(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "peek-skip-scanless"
*/
fn ast_fn059<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = SkipPeekTokenScanless::new();
  slots.assign(
    0,
    AstSlot(ASTNode::SkipPeekTokenScanless(Box::new(ref_2_0)), rng, TokenRange::default()),
  )
}

/*
sequence_instruction => "peek-reset"
*/
fn ast_fn060<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = PeekReset::new();
  slots.assign(0, AstSlot(ASTNode::PeekReset(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "shift-token" "nothing"
*/
fn ast_fn061<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = true;
  let ref_4_0 = ShiftToken::new(ref_3_0);
  slots.assign(0, AstSlot(ASTNode::ShiftToken(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "shift-token"
*/
fn ast_fn062<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = false;
  let ref_3_0 = ShiftToken::new(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::ShiftToken(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "shift-token-scanless"
*/
fn ast_fn063<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = ShiftTokenScanless::new();
  slots
    .assign(0, AstSlot(ASTNode::ShiftTokenScanless(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "scan-shift"
*/
fn ast_fn064<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = ScanShift::new();
  slots.assign(0, AstSlot(ASTNode::ScanShift(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
sequence_instruction => "accept"
*/
fn ast_fn065<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = Accept::new();
  slots.assign(0, AstSlot(ASTNode::Accept(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
convert_initializer => '(' init_objects ")"
*/
fn ast_fn066<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1;
  let ref_4_0 = Init::new(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::Init(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
terminal_list_1 => terminal_list_1 g:sym
*/
fn ast_fn075<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_1 => terminal_list_1 g:num
*/
fn ast_fn076<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_1 => g:sym
*/
fn ast_fn077<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_1 => g:num
*/
fn ast_fn078<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
rules_list_1 => rules_list_1 "|" rule
*/
fn ast_fn079<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
rules_list_1 => rule
*/
fn ast_fn080<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
instruction_sequence_list_2 => instruction_sequence_list_2 "then" push_goto_instruction
*/
fn ast_fn081<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence_list_2 => push_goto_instruction
*/
fn ast_fn082<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
reference => "$" tk:identifier
*/
fn ast_fn083<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = AST_NamedReference::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots
    .assign(0, AstSlot(ASTNode::AST_NamedReference(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
reference => "$" tk:integer
*/
fn ast_fn084<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i64();
  let ref_3_0 = AST_IndexReference::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots
    .assign(0, AstSlot(ASTNode::AST_IndexReference(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
add => add "+" expression
*/
fn ast_fn086<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_0_0 = i0;
  let ref_2_1 = i2;
  let ref_4_0 = AST_Add::new(ref_0_0, ref_2_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Add(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red green blue alpha ')'
*/
fn ast_fn088<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_5_0 = i5.to_u32();
  let ref_4_1 = i4.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_8_0 = RGBA::new(ref_5_0, ref_4_1, ref_3_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red green blue ')'
*/
fn ast_fn089<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_4_1 = i4.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_7_0 = RGBA::new(Default::default(), ref_4_1, ref_3_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red green alpha blue ')'
*/
fn ast_fn090<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_4_0 = i4.to_u32();
  let ref_5_1 = i5.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_8_0 = RGBA::new(ref_4_0, ref_5_1, ref_3_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red blue green alpha ')'
*/
fn ast_fn091<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_5_0 = i5.to_u32();
  let ref_3_1 = i3.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_8_0 = RGBA::new(ref_5_0, ref_3_1, ref_4_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red blue green ')'
*/
fn ast_fn092<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_3_1 = i3.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_7_0 = RGBA::new(Default::default(), ref_3_1, ref_4_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red blue alpha green ')'
*/
fn ast_fn093<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_4_0 = i4.to_u32();
  let ref_3_1 = i3.to_u32();
  let ref_5_2 = i5.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_8_0 = RGBA::new(ref_4_0, ref_3_1, ref_5_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red alpha green blue ')'
*/
fn ast_fn094<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_3_0 = i3.to_u32();
  let ref_5_1 = i5.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_8_0 = RGBA::new(ref_3_0, ref_5_1, ref_4_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' red alpha blue green ')'
*/
fn ast_fn095<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_3_0 = i3.to_u32();
  let ref_4_1 = i4.to_u32();
  let ref_5_2 = i5.to_u32();
  let ref_2_3 = i2.to_u32();
  let ref_8_0 = RGBA::new(ref_3_0, ref_4_1, ref_5_2, ref_2_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green red blue alpha ')'
*/
fn ast_fn096<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_5_0 = i5.to_u32();
  let ref_4_1 = i4.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_8_0 = RGBA::new(ref_5_0, ref_4_1, ref_2_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green red blue ')'
*/
fn ast_fn097<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_4_1 = i4.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_7_0 = RGBA::new(Default::default(), ref_4_1, ref_2_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green red alpha blue ')'
*/
fn ast_fn098<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_4_0 = i4.to_u32();
  let ref_5_1 = i5.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_8_0 = RGBA::new(ref_4_0, ref_5_1, ref_2_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green blue red alpha ')'
*/
fn ast_fn099<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_5_0 = i5.to_u32();
  let ref_3_1 = i3.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_8_0 = RGBA::new(ref_5_0, ref_3_1, ref_2_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green blue red ')'
*/
fn ast_fn100<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_3_1 = i3.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_7_0 = RGBA::new(Default::default(), ref_3_1, ref_2_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green blue alpha red ')'
*/
fn ast_fn101<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_4_0 = i4.to_u32();
  let ref_3_1 = i3.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_5_3 = i5.to_u32();
  let ref_8_0 = RGBA::new(ref_4_0, ref_3_1, ref_2_2, ref_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green alpha red blue ')'
*/
fn ast_fn102<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_3_0 = i3.to_u32();
  let ref_5_1 = i5.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_8_0 = RGBA::new(ref_3_0, ref_5_1, ref_2_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' green alpha blue red ')'
*/
fn ast_fn103<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_3_0 = i3.to_u32();
  let ref_4_1 = i4.to_u32();
  let ref_2_2 = i2.to_u32();
  let ref_5_3 = i5.to_u32();
  let ref_8_0 = RGBA::new(ref_3_0, ref_4_1, ref_2_2, ref_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue red green alpha ')'
*/
fn ast_fn104<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_5_0 = i5.to_u32();
  let ref_2_1 = i2.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_8_0 = RGBA::new(ref_5_0, ref_2_1, ref_4_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue red green ')'
*/
fn ast_fn105<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_2_1 = i2.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_7_0 = RGBA::new(Default::default(), ref_2_1, ref_4_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue red alpha green ')'
*/
fn ast_fn106<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_4_0 = i4.to_u32();
  let ref_2_1 = i2.to_u32();
  let ref_5_2 = i5.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_8_0 = RGBA::new(ref_4_0, ref_2_1, ref_5_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue green red alpha ')'
*/
fn ast_fn107<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_5_0 = i5.to_u32();
  let ref_2_1 = i2.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_8_0 = RGBA::new(ref_5_0, ref_2_1, ref_3_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue green red ')'
*/
fn ast_fn108<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_2_1 = i2.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_7_0 = RGBA::new(Default::default(), ref_2_1, ref_3_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue green alpha red ')'
*/
fn ast_fn109<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_4_0 = i4.to_u32();
  let ref_2_1 = i2.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_5_3 = i5.to_u32();
  let ref_8_0 = RGBA::new(ref_4_0, ref_2_1, ref_3_2, ref_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue alpha red green ')'
*/
fn ast_fn110<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_3_0 = i3.to_u32();
  let ref_2_1 = i2.to_u32();
  let ref_5_2 = i5.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_8_0 = RGBA::new(ref_3_0, ref_2_1, ref_5_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' blue alpha green red ')'
*/
fn ast_fn111<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_3_0 = i3.to_u32();
  let ref_2_1 = i2.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_5_3 = i5.to_u32();
  let ref_8_0 = RGBA::new(ref_3_0, ref_2_1, ref_4_2, ref_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' alpha red green blue ')'
*/
fn ast_fn112<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_2_0 = i2.to_u32();
  let ref_5_1 = i5.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_8_0 = RGBA::new(ref_2_0, ref_5_1, ref_4_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' alpha red blue green ')'
*/
fn ast_fn113<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_2_0 = i2.to_u32();
  let ref_4_1 = i4.to_u32();
  let ref_5_2 = i5.to_u32();
  let ref_3_3 = i3.to_u32();
  let ref_8_0 = RGBA::new(ref_2_0, ref_4_1, ref_5_2, ref_3_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' alpha green red blue ')'
*/
fn ast_fn114<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_2_0 = i2.to_u32();
  let ref_5_1 = i5.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_8_0 = RGBA::new(ref_2_0, ref_5_1, ref_3_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' alpha green blue red ')'
*/
fn ast_fn115<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_2_0 = i2.to_u32();
  let ref_4_1 = i4.to_u32();
  let ref_3_2 = i3.to_u32();
  let ref_5_3 = i5.to_u32();
  let ref_8_0 = RGBA::new(ref_2_0, ref_4_1, ref_3_2, ref_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' alpha blue red green ')'
*/
fn ast_fn116<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_2_0 = i2.to_u32();
  let ref_3_1 = i3.to_u32();
  let ref_5_2 = i5.to_u32();
  let ref_4_3 = i4.to_u32();
  let ref_8_0 = RGBA::new(ref_2_0, ref_3_1, ref_5_2, ref_4_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
color => "rgb" '(' alpha blue green red ')'
*/
fn ast_fn117<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, ..) = slots.take(5);
  let AstSlot(_, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_2_0 = i2.to_u32();
  let ref_3_1 = i3.to_u32();
  let ref_4_2 = i4.to_u32();
  let ref_5_3 = i5.to_u32();
  let ref_8_0 = RGBA::new(ref_2_0, ref_3_1, ref_4_2, ref_5_3);
  slots.assign(0, AstSlot(ASTNode::RGBA(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production_id_list => '[' tk:integer ']'
*/
fn ast_fn118<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i64();
  let ref_4_0 = Num::new(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::Num(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
append_production => "+>" priority non_terminal ">" rules
*/
fn ast_fn119<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_6_0 = true;
  let ref_2_2 = rng2.to_token(_ctx_.get_reader()).to_string();
  let ref_2_3 = i2;
  let ref_1_4 = i1;
  let ref_1_4 = if let ASTNode::Priority(obj) = ref_1_4 { obj } else { panic!("invalid node") };
  let ref_4_5 = i4.into_nodes();
  let ref_7_0 = Production::new(
    ref_6_0,
    false,
    ref_2_2,
    ref_2_3,
    Some(ref_1_4),
    ref_4_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Vec::new(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
append_production => "+>" non_terminal ">" rules
*/
fn ast_fn120<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader()).to_string();
  let ref_1_3 = i1;
  let ref_3_5 = i3.into_nodes();
  let ref_6_0 = Production::new(
    ref_5_0,
    false,
    ref_1_2,
    ref_1_3,
    Default::default(),
    ref_3_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Vec::new(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
name_clause => "NAME" identifier
*/
fn ast_fn121<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = Name::new(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::Name(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
instruction_sequence_list_3 => instruction_sequence_list_3 "then" push_goto_instruction
*/
fn ast_fn122<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence_list_3 => push_goto_instruction
*/
fn ast_fn123<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
body_list_1 => body_list_1 ";" expression
*/
fn ast_fn124<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
body_list_1 => expression
*/
fn ast_fn125<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
priority => "{" tk:priority_num '}'
*/
fn ast_fn126<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_1_1 = ref_1_1.to_u32();
  let ref_4_0 = Priority::new(false, ref_1_1);
  slots.assign(0, AstSlot(ASTNode::Priority(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
priority => "!"
*/
fn ast_fn127<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = true;
  let ref_3_0 = Priority::new(ref_2_0, 0u32);
  slots.assign(0, AstSlot(ASTNode::Priority(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
instruction_sequence_list_1 => instruction_sequence_list_1 "then" sequence_instruction
*/
fn ast_fn128<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence_list_1 => sequence_instruction
*/
fn ast_fn129<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
token_id_list => '[' token_id_list_list_1 ']'
*/
fn ast_fn130<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(ref_1_0), rng, TokenRange::default()))
}

/*
group => "(" rules ")"
*/
fn ast_fn131<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = Group_Production::new(
    ref_1_0
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Group_Production(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
vector => "[" vector_list_1 "]"
*/
fn ast_fn132<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = AST_Vector::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
vector => "[" "]"
*/
fn ast_fn133<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = AST_Vector::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Vector(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list tk:reference "?" priority
*/
fn ast_fn134<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_3_1 = i3;
  let ref_3_1 = if let ASTNode::Priority(obj) = ref_3_1 { obj } else { panic!("invalid node") };
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0,
    Some(ref_3_1),
    ref_1_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list "?" priority
*/
fn ast_fn135<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0,
    Some(ref_2_1),
    Default::default(),
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list tk:reference priority
*/
fn ast_fn136<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = false;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1 { obj } else { panic!("invalid node") };
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0,
    Some(ref_2_1),
    ref_1_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list priority
*/
fn ast_fn137<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = false;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0;
  let ref_4_0 = AnnotatedSymbol::new(
    ref_3_0,
    Some(ref_1_1),
    Default::default(),
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list tk:reference "?"
*/
fn ast_fn138<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0,
    Default::default(),
    ref_1_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list "?"
*/
fn ast_fn139<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = true;
  let ref_0_3 = i0;
  let ref_4_0 = AnnotatedSymbol::new(
    ref_3_0,
    Default::default(),
    Default::default(),
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list tk:reference
*/
fn ast_fn140<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = false;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_4_0 = AnnotatedSymbol::new(
    ref_3_0,
    Default::default(),
    ref_1_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list tk:reference priority "?"
*/
fn ast_fn141<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1 { obj } else { panic!("invalid node") };
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0,
    Some(ref_2_1),
    ref_1_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list priority "?"
*/
fn ast_fn142<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0,
    Some(ref_1_1),
    Default::default(),
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list "?" tk:reference priority
*/
fn ast_fn143<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_3_1 = i3;
  let ref_3_1 = if let ASTNode::Priority(obj) = ref_3_1 { obj } else { panic!("invalid node") };
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0,
    Some(ref_3_1),
    ref_2_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list "?" tk:reference
*/
fn ast_fn144<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0,
    Default::default(),
    ref_2_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list "?" priority tk:reference
*/
fn ast_fn145<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1 { obj } else { panic!("invalid node") };
  let ref_3_2 = rng3.to_token(_ctx_.get_reader());
  let ref_3_2 = ref_3_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0,
    Some(ref_2_1),
    ref_3_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list priority tk:reference
*/
fn ast_fn146<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = false;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1 { obj } else { panic!("invalid node") };
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0,
    Some(ref_1_1),
    ref_2_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list priority tk:reference "?"
*/
fn ast_fn147<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1 { obj } else { panic!("invalid node") };
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0,
    Some(ref_1_1),
    ref_2_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => list priority "?" tk:reference
*/
fn ast_fn148<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1 { obj } else { panic!("invalid node") };
  let ref_3_2 = rng3.to_token(_ctx_.get_reader());
  let ref_3_2 = ref_3_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0,
    Some(ref_1_1),
    ref_3_2,
    ref_0_3,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
token => "tk" range
*/
fn ast_fn150<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Range(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_Token::new(Some(ref_1_0));
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
token => 'tok' range
*/
fn ast_fn151<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Range(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_Token::new(Some(ref_1_0));
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
token => "token" range
*/
fn ast_fn152<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Range(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_Token::new(Some(ref_1_0));
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
token => "tk"
*/
fn ast_fn153<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_Token::new(Default::default());
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
token => 'tok'
*/
fn ast_fn154<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_Token::new(Default::default());
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
token => "token"
*/
fn ast_fn155<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_Token::new(Default::default());
  slots.assign(0, AstSlot(ASTNode::AST_Token(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
except_handler => "push-catch" state_reference "then"
*/
fn ast_fn156<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::HASH_NAME(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_4_0 = PushExceptHandler::new(ref_1_0);
  slots
    .assign(0, AstSlot(ASTNode::PushExceptHandler(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'num'
*/
fn ast_fn157<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'nl'
*/
fn ast_fn158<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'sp'
*/
fn ast_fn159<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'id'
*/
fn ast_fn160<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'sym'
*/
fn ast_fn161<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'any'
*/
fn ast_fn162<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'tab'
*/
fn ast_fn163<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => "c:" 'htab'
*/
fn ast_fn164<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
range => "<" tk:integer "," tk:integer ">"
*/
fn ast_fn165<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let AstSlot(_, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_3_0 = rng3.to_token(_ctx_.get_reader());
  let ref_3_0 = ref_3_0.to_i32();
  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_1_1 = ref_1_1.to_i32();
  let ref_6_0 = Range::new(ref_3_0, ref_1_1);
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
range => "<" tk:integer ">"
*/
fn ast_fn166<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_1_1 = ref_1_1.to_i32();
  let ref_4_0 = Range::new(Default::default(), ref_1_1);
  slots.assign(0, AstSlot(ASTNode::Range(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
scanner_declaration => "scanner" '[' tk:state_hash_token ']'
*/
fn ast_fn167<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = rng2.to_token(_ctx_.get_reader());
  let ref_2_0 = ref_2_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(ref_2_0), rng, TokenRange::default()))
}

/*
goto_instruction => "goto" state_reference
*/
fn ast_fn168<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::HASH_NAME(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = Goto::new(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::Goto(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
assertion_instruction => "assert" assert_class production_id_list '(' instruction_sequence ')'
*/
fn ast_fn174<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::Num(obj) = ref_2_0 { obj } else { panic!("invalid node") };
  let ref_4_1 = i4.into_nodes();
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_7_0 = ASSERT::new(ref_2_0, ref_4_1, ref_1_2);
  slots.assign(0, AstSlot(ASTNode::ASSERT(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
assertion_instruction => "default" '(' instruction_sequence ')'
*/
fn ast_fn175<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = i2.into_nodes();
  let ref_5_0 = DEFAULT::new(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::DEFAULT(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
terminal => terminal_list_1 g:sp
*/
fn ast_fn176<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_0_1 = i0.into_tokens();
  let ref_0_1 = (ref_0_1.first().unwrap() + ref_0_1.last().unwrap()).to_string();
  let ref_3_0 = Terminal::new(false, ref_0_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Terminal(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
terminal => terminal_list_1
*/
fn ast_fn177<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_1 = i0.into_tokens();
  let ref_0_1 = (ref_0_1.first().unwrap() + ref_0_1.last().unwrap()).to_string();
  let ref_2_0 = Terminal::new(false, ref_0_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Terminal(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
terminal => """ terminal_list_2 """
*/
fn ast_fn178<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_1_1 = i1.into_tokens();
  let ref_1_1 = (ref_1_1.first().unwrap() + ref_1_1.last().unwrap()).to_string();
  let ref_5_0 = Terminal::new(ref_4_0, ref_1_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Terminal(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
terminal => "'" terminal_list_3 "'"
*/
fn ast_fn179<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_1 = i1.into_tokens();
  let ref_1_1 = (ref_1_1.first().unwrap() + ref_1_1.last().unwrap()).to_string();
  let ref_4_0 = Terminal::new(false, ref_1_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Terminal(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
recover_definition => ":rec" "{" state "}"
*/
fn ast_fn180<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::IR_STATE(obj) = ref_2_0 { obj } else { panic!("invalid node") };
  let ref_5_0 = Recovery::new(ref_2_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Recovery(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 ast_definition syntax_definition recover_definition
*/
fn ast_fn181<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::Ascript(obj) = ref_2_0 { obj } else { panic!("invalid node") };
  let ref_6_1 = true;
  let ref_4_2 = i4;
  let ref_4_2 = if let ASTNode::Recovery(obj) = ref_4_2 { obj } else { panic!("invalid node") };
  let ref_1_3 = i1.into_nodes();
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Syntax(obj) = ref_3_4 { obj } else { panic!("invalid node") };
  let ref_7_0 = Rule::new(
    Some(ref_2_0),
    ref_6_1,
    Some(ref_4_2),
    ref_1_3,
    Some(ref_3_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 ast_definition syntax_definition recover_definition
*/
fn ast_fn182<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Ascript(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_5_1 = false;
  let ref_3_2 = i3;
  let ref_3_2 = if let ASTNode::Recovery(obj) = ref_3_2 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0.into_nodes();
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Syntax(obj) = ref_2_4 { obj } else { panic!("invalid node") };
  let ref_6_0 = Rule::new(
    Some(ref_1_0),
    ref_5_1,
    Some(ref_3_2),
    ref_0_3,
    Some(ref_2_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 syntax_definition recover_definition
*/
fn ast_fn183<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_1 = true;
  let ref_3_2 = i3;
  let ref_3_2 = if let ASTNode::Recovery(obj) = ref_3_2 { obj } else { panic!("invalid node") };
  let ref_1_3 = i1.into_nodes();
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Syntax(obj) = ref_2_4 { obj } else { panic!("invalid node") };
  let ref_6_0 = Rule::new(
    Default::default(),
    ref_5_1,
    Some(ref_3_2),
    ref_1_3,
    Some(ref_2_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 syntax_definition recover_definition
*/
fn ast_fn184<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_1 = false;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Recovery(obj) = ref_2_2 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0.into_nodes();
  let ref_1_4 = i1;
  let ref_1_4 = if let ASTNode::Syntax(obj) = ref_1_4 { obj } else { panic!("invalid node") };
  let ref_5_0 = Rule::new(
    Default::default(),
    ref_4_1,
    Some(ref_2_2),
    ref_0_3,
    Some(ref_1_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 ast_definition recover_definition
*/
fn ast_fn185<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::Ascript(obj) = ref_2_0 { obj } else { panic!("invalid node") };
  let ref_5_1 = true;
  let ref_3_2 = i3;
  let ref_3_2 = if let ASTNode::Recovery(obj) = ref_3_2 { obj } else { panic!("invalid node") };
  let ref_1_3 = i1.into_nodes();
  let ref_6_0 = Rule::new(
    Some(ref_2_0),
    ref_5_1,
    Some(ref_3_2),
    ref_1_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 ast_definition recover_definition
*/
fn ast_fn186<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Ascript(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_4_1 = false;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Recovery(obj) = ref_2_2 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0.into_nodes();
  let ref_5_0 = Rule::new(
    Some(ref_1_0),
    ref_4_1,
    Some(ref_2_2),
    ref_0_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 recover_definition
*/
fn ast_fn187<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_1 = true;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Recovery(obj) = ref_2_2 { obj } else { panic!("invalid node") };
  let ref_1_3 = i1.into_nodes();
  let ref_5_0 = Rule::new(
    Default::default(),
    ref_4_1,
    Some(ref_2_2),
    ref_1_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 recover_definition
*/
fn ast_fn188<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_1 = false;
  let ref_1_2 = i1;
  let ref_1_2 = if let ASTNode::Recovery(obj) = ref_1_2 { obj } else { panic!("invalid node") };
  let ref_0_3 = i0.into_nodes();
  let ref_4_0 = Rule::new(
    Default::default(),
    ref_3_1,
    Some(ref_1_2),
    ref_0_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 ast_definition syntax_definition
*/
fn ast_fn189<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::Ascript(obj) = ref_2_0 { obj } else { panic!("invalid node") };
  let ref_5_1 = true;
  let ref_1_3 = i1.into_nodes();
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Syntax(obj) = ref_3_4 { obj } else { panic!("invalid node") };
  let ref_6_0 = Rule::new(
    Some(ref_2_0),
    ref_5_1,
    Default::default(),
    ref_1_3,
    Some(ref_3_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 ast_definition syntax_definition
*/
fn ast_fn190<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Ascript(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_4_1 = false;
  let ref_0_3 = i0.into_nodes();
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Syntax(obj) = ref_2_4 { obj } else { panic!("invalid node") };
  let ref_5_0 = Rule::new(
    Some(ref_1_0),
    ref_4_1,
    Default::default(),
    ref_0_3,
    Some(ref_2_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 syntax_definition
*/
fn ast_fn191<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_1 = true;
  let ref_1_3 = i1.into_nodes();
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Syntax(obj) = ref_2_4 { obj } else { panic!("invalid node") };
  let ref_5_0 = Rule::new(
    Default::default(),
    ref_4_1,
    Default::default(),
    ref_1_3,
    Some(ref_2_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 syntax_definition
*/
fn ast_fn192<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_1 = false;
  let ref_0_3 = i0.into_nodes();
  let ref_1_4 = i1;
  let ref_1_4 = if let ASTNode::Syntax(obj) = ref_1_4 { obj } else { panic!("invalid node") };
  let ref_4_0 = Rule::new(
    Default::default(),
    ref_3_1,
    Default::default(),
    ref_0_3,
    Some(ref_1_4),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1 ast_definition
*/
fn ast_fn193<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::Ascript(obj) = ref_2_0 { obj } else { panic!("invalid node") };
  let ref_4_1 = true;
  let ref_1_3 = i1.into_nodes();
  let ref_5_0 = Rule::new(
    Some(ref_2_0),
    ref_4_1,
    Default::default(),
    ref_1_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 ast_definition
*/
fn ast_fn194<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Ascript(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_1 = false;
  let ref_0_3 = i0.into_nodes();
  let ref_4_0 = Rule::new(
    Some(ref_1_0),
    ref_3_1,
    Default::default(),
    ref_0_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => "!" rule_list_1
*/
fn ast_fn195<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_1 = true;
  let ref_1_3 = i1.into_nodes();
  let ref_4_0 = Rule::new(
    Default::default(),
    ref_3_1,
    Default::default(),
    ref_1_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1
*/
fn ast_fn196<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_1 = false;
  let ref_0_3 = i0.into_nodes();
  let ref_3_0 = Rule::new(
    Default::default(),
    ref_2_1,
    Default::default(),
    ref_0_3,
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Rule(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
state_declaration => "state" '[' tk:state_hash_token ']'
*/
fn ast_fn197<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_2_0 = rng2.to_token(_ctx_.get_reader());
  let ref_2_0 = ref_2_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(ref_2_0), rng, TokenRange::default()))
}

/*
field => reference ':' syntax_spec
*/
fn ast_fn198<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_0_0 = i0;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::SyntaxSpec(obj) = ref_2_1 { obj } else { panic!("invalid node") };
  let ref_4_0 = SyntaxField::new(ref_0_0, ref_2_1);
  slots.assign(0, AstSlot(ASTNode::SyntaxField(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
grammar_list_2 => grammar_list_2 production
*/
fn ast_fn199<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar_list_2 => grammar_list_2 append_production
*/
fn ast_fn200<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar_list_2 => production
*/
fn ast_fn201<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
grammar_list_2 => append_production
*/
fn ast_fn202<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
body => expression
*/
fn ast_fn204<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  let ref_3_0 = AST_Statements::new(ref_2_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
body => "{" body_list_1 '}'
*/
fn ast_fn205<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = AST_Statements::new(ref_1_0, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
body => "{" '}'
*/
fn ast_fn206<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = AST_Statements::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Statements(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
map => "map" "(" expression ',' expression ')'
*/
fn ast_fn212<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_2_0 = i2;
  let ref_4_1 = i4;
  let ref_7_0 = AST_Map::new(ref_2_0, ref_4_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_Map(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
reduce_instruction => "reduce" tk:integer "symbols" "to" tk:integer "with" "rule" tk:integer
*/
fn ast_fn213<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  slots.take(5);
  slots.take(6);
  let AstSlot(i7, rng7, _) = slots.take(7);
  let rng = rng0 + rng7;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i32();
  let ref_4_1 = rng4.to_token(_ctx_.get_reader());
  let ref_4_1 = ref_4_1.to_i32();
  let ref_7_2 = rng7.to_token(_ctx_.get_reader());
  let ref_7_2 = ref_7_2.to_i32();
  let ref_9_0 = Reduce::new(ref_1_0, ref_4_1, ref_7_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
reduce_instruction => "reduce" tk:integer tk:integer "with" "rule" tk:integer
*/
fn ast_fn214<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  slots.take(3);
  slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i32();
  let ref_2_1 = rng2.to_token(_ctx_.get_reader());
  let ref_2_1 = ref_2_1.to_i32();
  let ref_5_2 = rng5.to_token(_ctx_.get_reader());
  let ref_5_2 = ref_5_2.to_i32();
  let ref_7_0 = Reduce::new(ref_1_0, ref_2_1, ref_5_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
reduce_instruction => "reduce" tk:integer "symbols" "to" tk:integer tk:integer
*/
fn ast_fn215<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i32();
  let ref_4_1 = rng4.to_token(_ctx_.get_reader());
  let ref_4_1 = ref_4_1.to_i32();
  let ref_5_2 = rng5.to_token(_ctx_.get_reader());
  let ref_5_2 = ref_5_2.to_i32();
  let ref_7_0 = Reduce::new(ref_1_0, ref_4_1, ref_5_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
reduce_instruction => "reduce" tk:integer tk:integer tk:integer
*/
fn ast_fn216<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i32();
  let ref_2_1 = rng2.to_token(_ctx_.get_reader());
  let ref_2_1 = ref_2_1.to_i32();
  let ref_3_2 = rng3.to_token(_ctx_.get_reader());
  let ref_3_2 = ref_3_2.to_i32();
  let ref_5_0 = Reduce::new(ref_1_0, ref_2_1, ref_3_2);
  slots.assign(0, AstSlot(ASTNode::Reduce(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
token_id_list_list_1 => token_id_list_list_1 token_num
*/
fn ast_fn217<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
token_id_list_list_1 => token_num
*/
fn ast_fn218<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
state => state_declaration scanner_declaration "excepts" top_level_instructions expected_symbols
*/
fn ast_fn219<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_6_0 = true;
  let ref_0_1 = i0.to_string();
  let ref_3_2 = i3.into_nodes();
  let ref_1_3 = i1.to_string();
  let ref_4_4 = i4;
  let ref_4_4 = if let ASTNode::Symbols(obj) = ref_4_4 { obj } else { panic!("invalid node") };
  let ref_7_0 = IR_STATE::new(ref_6_0, ref_0_1, ref_3_2, ref_1_3, Some(ref_4_4));
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
state => state_declaration "excepts" top_level_instructions expected_symbols
*/
fn ast_fn220<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_0_1 = i0.to_string();
  let ref_2_2 = i2.into_nodes();
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Symbols(obj) = ref_3_4 { obj } else { panic!("invalid node") };
  let ref_6_0 = IR_STATE::new(ref_5_0, ref_0_1, ref_2_2, Default::default(), Some(ref_3_4));
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
state => state_declaration scanner_declaration top_level_instructions expected_symbols
*/
fn ast_fn221<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = false;
  let ref_0_1 = i0.to_string();
  let ref_2_2 = i2.into_nodes();
  let ref_1_3 = i1.to_string();
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Symbols(obj) = ref_3_4 { obj } else { panic!("invalid node") };
  let ref_6_0 = IR_STATE::new(ref_5_0, ref_0_1, ref_2_2, ref_1_3, Some(ref_3_4));
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
state => state_declaration top_level_instructions expected_symbols
*/
fn ast_fn222<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = false;
  let ref_0_1 = i0.to_string();
  let ref_1_2 = i1.into_nodes();
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Symbols(obj) = ref_2_4 { obj } else { panic!("invalid node") };
  let ref_5_0 = IR_STATE::new(ref_4_0, ref_0_1, ref_1_2, Default::default(), Some(ref_2_4));
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
state => state_declaration scanner_declaration "excepts" top_level_instructions
*/
fn ast_fn223<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_0_1 = i0.to_string();
  let ref_3_2 = i3.into_nodes();
  let ref_1_3 = i1.to_string();
  let ref_6_0 = IR_STATE::new(ref_5_0, ref_0_1, ref_3_2, ref_1_3, Default::default());
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
state => state_declaration "excepts" top_level_instructions
*/
fn ast_fn224<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_0_1 = i0.to_string();
  let ref_2_2 = i2.into_nodes();
  let ref_5_0 = IR_STATE::new(ref_4_0, ref_0_1, ref_2_2, Default::default(), Default::default());
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
state => state_declaration scanner_declaration top_level_instructions
*/
fn ast_fn225<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = false;
  let ref_0_1 = i0.to_string();
  let ref_2_2 = i2.into_nodes();
  let ref_1_3 = i1.to_string();
  let ref_5_0 = IR_STATE::new(ref_4_0, ref_0_1, ref_2_2, ref_1_3, Default::default());
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
state => state_declaration top_level_instructions
*/
fn ast_fn226<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_3_0 = false;
  let ref_0_1 = i0.to_string();
  let ref_1_2 = i1.into_nodes();
  let ref_4_0 = IR_STATE::new(ref_3_0, ref_0_1, ref_1_2, Default::default(), Default::default());
  slots.assign(0, AstSlot(ASTNode::IR_STATE(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
production_symbol => tk:identifier_syms
*/
fn ast_fn227<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_0 = Production_Symbol::new(ref_0_0, rng.to_token(_ctx_.get_reader()));
  slots
    .assign(0, AstSlot(ASTNode::Production_Symbol(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
instruction_sequence => goto_instruction
*/
fn ast_fn228<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
instruction_sequence => except_handler instruction_sequence_list_1 "then" instruction_sequence_list_2 "then" goto_instruction
*/
fn ast_fn229<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_0_0 = i0;
  let mut ref_1_0 = i1.into_nodes();
  let mut ref_3_0 = i3.into_nodes();
  let ref_5_0 = i5;
  let mut ref_7_0 = vec![];
  ref_7_0.push(ref_0_0);
  ref_7_0.append(&mut ref_1_0);
  ref_7_0.append(&mut ref_3_0);
  ref_7_0.push(ref_5_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_7_0), rng, TokenRange::default()))
}

/*
instruction_sequence => instruction_sequence_list_1 "then" instruction_sequence_list_2 "then" goto_instruction
*/
fn ast_fn230<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let mut ref_2_0 = i2.into_nodes();
  let ref_4_0 = i4;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.append(&mut ref_2_0);
  ref_0_0.push(ref_4_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence => except_handler instruction_sequence_list_1 "then" goto_instruction
*/
fn ast_fn231<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_0_0 = i0;
  let mut ref_1_0 = i1.into_nodes();
  let ref_3_0 = i3;
  let mut ref_5_0 = vec![];
  ref_5_0.push(ref_0_0);
  ref_5_0.append(&mut ref_1_0);
  ref_5_0.push(ref_3_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_5_0), rng, TokenRange::default()))
}

/*
instruction_sequence => instruction_sequence_list_1 "then" goto_instruction
*/
fn ast_fn232<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence => except_handler instruction_sequence_list_1 "then" instruction_sequence_list_2
*/
fn ast_fn233<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_0_0 = i0;
  let mut ref_1_0 = i1.into_nodes();
  let mut ref_3_0 = i3.into_nodes();
  let mut ref_5_0 = vec![];
  ref_5_0.push(ref_0_0);
  ref_5_0.append(&mut ref_1_0);
  ref_5_0.append(&mut ref_3_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_5_0), rng, TokenRange::default()))
}

/*
instruction_sequence => instruction_sequence_list_1 "then" instruction_sequence_list_2
*/
fn ast_fn234<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let mut ref_2_0 = i2.into_nodes();
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.append(&mut ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence => except_handler instruction_sequence_list_1
*/
fn ast_fn235<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_0_0 = i0;
  let mut ref_1_0 = i1.into_nodes();
  let mut ref_3_0 = vec![];
  ref_3_0.push(ref_0_0);
  ref_3_0.append(&mut ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_3_0), rng, TokenRange::default()))
}

/*
instruction_sequence => instruction_sequence_list_1
*/
fn ast_fn236<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence => instruction_sequence_list_3 "then" goto_instruction
*/
fn ast_fn237<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
instruction_sequence => instruction_sequence_list_3
*/
fn ast_fn238<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0.into_nodes();
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar => grammar_list_1 grammar_list_2
*/
fn ast_fn239<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_0_0 = i0.into_nodes();
  let ref_1_1 = i1.into_nodes();
  let ref_3_0 = Grammar::new(
    ref_0_0,
    ref_1_1
      .into_iter()
      .map(|v| match v {
        ASTNode::Production(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Grammar(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
grammar => grammar_list_2
*/
fn ast_fn240<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_1 = i0.into_nodes();
  let ref_2_0 = Grammar::new(
    Default::default(),
    ref_0_1
      .into_iter()
      .map(|v| match v {
        ASTNode::Production(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Grammar(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
top_level_instructions_list_1 => top_level_instructions_list_1 assertion_instruction
*/
fn ast_fn241<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
top_level_instructions_list_1 => assertion_instruction
*/
fn ast_fn242<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
declaration_list_1 => declaration_list_1 ',' field
*/
fn ast_fn244<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
declaration_list_1 => field
*/
fn ast_fn245<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
declaration => '{' declaration_list_1 '}'
*/
fn ast_fn246<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = Syntax::new(
    ref_1_0
      .into_iter()
      .map(|v| match v {
        ASTNode::SyntaxField(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
  );
  slots.assign(0, AstSlot(ASTNode::Syntax(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
production => "<" production_list_1 ">" "lazy" priority non_terminal ">" rules
*/
fn ast_fn249<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(i4, ..) = slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  slots.take(6);
  let AstSlot(i7, rng7, _) = slots.take(7);
  let rng = rng0 + rng7;

  let ref_9_1 = true;
  let ref_5_2 = rng5.to_token(_ctx_.get_reader()).to_string();
  let ref_5_3 = i5;
  let ref_4_4 = i4;
  let ref_4_4 = if let ASTNode::Priority(obj) = ref_4_4 { obj } else { panic!("invalid node") };
  let ref_7_5 = i7.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_10_0 = Production::new(
    false,
    ref_9_1,
    ref_5_2,
    ref_5_3,
    Some(ref_4_4),
    ref_7_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    ref_1_6,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_10_0)), rng, TokenRange::default()))
}

/*
production => "<" ">" "lazy" priority non_terminal ">" rules
*/
fn ast_fn250<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  slots.take(5);
  let AstSlot(i6, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_8_1 = true;
  let ref_4_2 = rng4.to_token(_ctx_.get_reader()).to_string();
  let ref_4_3 = i4;
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Priority(obj) = ref_3_4 { obj } else { panic!("invalid node") };
  let ref_6_5 = i6.into_nodes();
  let ref_9_0 = Production::new(
    false,
    ref_8_1,
    ref_4_2,
    ref_4_3,
    Some(ref_3_4),
    ref_6_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
production => "<" production_list_1 ">" priority non_terminal ">" rules
*/
fn ast_fn251<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, ..) = slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  slots.take(5);
  let AstSlot(i6, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_8_1 = false;
  let ref_4_2 = rng4.to_token(_ctx_.get_reader()).to_string();
  let ref_4_3 = i4;
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Priority(obj) = ref_3_4 { obj } else { panic!("invalid node") };
  let ref_6_5 = i6.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_9_0 = Production::new(
    false,
    ref_8_1,
    ref_4_2,
    ref_4_3,
    Some(ref_3_4),
    ref_6_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    ref_1_6,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
production => "<" ">" priority non_terminal ">" rules
*/
fn ast_fn252<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_7_1 = false;
  let ref_3_2 = rng3.to_token(_ctx_.get_reader()).to_string();
  let ref_3_3 = i3;
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Priority(obj) = ref_2_4 { obj } else { panic!("invalid node") };
  let ref_5_5 = i5.into_nodes();
  let ref_8_0 = Production::new(
    false,
    ref_7_1,
    ref_3_2,
    ref_3_3,
    Some(ref_2_4),
    ref_5_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production => "<" production_list_1 ">" "lazy" non_terminal ">" rules
*/
fn ast_fn253<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  slots.take(5);
  let AstSlot(i6, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;

  let ref_8_1 = true;
  let ref_4_2 = rng4.to_token(_ctx_.get_reader()).to_string();
  let ref_4_3 = i4;
  let ref_6_5 = i6.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_9_0 = Production::new(
    false,
    ref_8_1,
    ref_4_2,
    ref_4_3,
    Default::default(),
    ref_6_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    ref_1_6,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
production => "<" ">" "lazy" non_terminal ">" rules
*/
fn ast_fn254<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_7_1 = true;
  let ref_3_2 = rng3.to_token(_ctx_.get_reader()).to_string();
  let ref_3_3 = i3;
  let ref_5_5 = i5.into_nodes();
  let ref_8_0 = Production::new(
    false,
    ref_7_1,
    ref_3_2,
    ref_3_3,
    Default::default(),
    ref_5_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production => "<" production_list_1 ">" non_terminal ">" rules
*/
fn ast_fn255<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot(i3, rng3, _) = slots.take(3);
  slots.take(4);
  let AstSlot(i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;

  let ref_7_1 = false;
  let ref_3_2 = rng3.to_token(_ctx_.get_reader()).to_string();
  let ref_3_3 = i3;
  let ref_5_5 = i5.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_8_0 = Production::new(
    false,
    ref_7_1,
    ref_3_2,
    ref_3_3,
    Default::default(),
    ref_5_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    ref_1_6,
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production => "<" ">" non_terminal ">" rules
*/
fn ast_fn256<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_6_1 = false;
  let ref_2_2 = rng2.to_token(_ctx_.get_reader()).to_string();
  let ref_2_3 = i2;
  let ref_4_5 = i4.into_nodes();
  let ref_7_0 = Production::new(
    false,
    ref_6_1,
    ref_2_2,
    ref_2_3,
    Default::default(),
    ref_4_5
      .into_iter()
      .map(|v| match v {
        ASTNode::Rule(node) => node,
        _ => panic!("could not convert"),
      })
      .collect::<Vec<_>>(),
    Default::default(),
    rng.to_token(_ctx_.get_reader()),
  );
  slots.assign(0, AstSlot(ASTNode::Production(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
struct_list_1 => struct_list_1 "," struct_prop
*/
fn ast_fn258<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
struct_list_1 => struct_prop
*/
fn ast_fn259<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
push_goto_instruction => "push" state_reference
*/
fn ast_fn260<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::HASH_NAME(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = PushGoto::new(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::PushGoto(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
template_name => identifier
*/
fn ast_fn261<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = rng.to_token(_ctx_.get_reader());
  let ref_2_0 = ref_2_0.to_string();
  slots.assign(0, AstSlot(ASTNode::STRING(ref_2_0), rng, TokenRange::default()))
}

/*
grammar_list_1 => grammar_list_1 preamble
*/
fn ast_fn262<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar_list_1 => preamble
*/
fn ast_fn263<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
any_group_list_1 => any_group_list_1 annotated_symbol
*/
fn ast_fn264<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
any_group_list_1 => annotated_symbol
*/
fn ast_fn265<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => import_clause_list_1 g:id
*/
fn ast_fn266<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => import_clause_list_1 g:sym
*/
fn ast_fn267<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => g:id
*/
fn ast_fn268<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => g:sym
*/
fn ast_fn269<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
green => "g" tk:integer
*/
fn ast_fn274<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_u32();
  slots.assign(0, AstSlot(ASTNode::U32(ref_1_0), rng, TokenRange::default()))
}

/*
vector_list_1 => vector_list_1 "," expression
*/
fn ast_fn275<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
vector_list_1 => expression
*/
fn ast_fn276<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
blue => "b" tk:integer
*/
fn ast_fn277<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_u32();
  slots.assign(0, AstSlot(ASTNode::U32(ref_1_0), rng, TokenRange::default()))
}

/*
literal => "true"
*/
fn ast_fn285<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_1 = true;
  let ref_3_0 = AST_BOOL::new(None, ref_2_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
literal => "false"
*/
fn ast_fn286<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_1 = false;
  let ref_3_0 = AST_BOOL::new(None, ref_2_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
literal => tk:integer
*/
fn ast_fn287<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_f64();
  let ref_2_0 = AST_NUMBER::new(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::AST_NUMBER(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
member => reference '.' identifier
*/
fn ast_fn289<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_2_0 = rng2.to_token(_ctx_.get_reader());
  let ref_0_1 = i0;
  let ref_4_0 = AST_Member::new(ref_2_0, ref_0_1);
  slots.assign(0, AstSlot(ASTNode::AST_Member(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:id
*/
fn ast_fn290<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:sym
*/
fn ast_fn291<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:num
*/
fn ast_fn292<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:sp
*/
fn ast_fn293<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 escaped
*/
fn ast_fn294<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:id
*/
fn ast_fn295<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:sym
*/
fn ast_fn296<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:num
*/
fn ast_fn297<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:sp
*/
fn ast_fn298<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => escaped
*/
fn ast_fn299<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
import_clause => "IMPORT" import_clause_list_1 g:sp "AS" identifier
*/
fn ast_fn300<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_4_0 = rng4.to_token(_ctx_.get_reader());
  let ref_4_0 = ref_4_0.to_string();
  let ref_1_1 = i1.into_tokens();
  let ref_1_1 = (ref_1_1.first().unwrap() + ref_1_1.last().unwrap()).to_string();
  let ref_6_0 = Import::new(ref_4_0, ref_1_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
import_clause => "IMPORT" import_clause_list_1 g:sp "as" identifier
*/
fn ast_fn301<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot(i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;

  let ref_4_0 = rng4.to_token(_ctx_.get_reader());
  let ref_4_0 = ref_4_0.to_string();
  let ref_1_1 = i1.into_tokens();
  let ref_1_1 = (ref_1_1.first().unwrap() + ref_1_1.last().unwrap()).to_string();
  let ref_6_0 = Import::new(ref_4_0, ref_1_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::Import(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
string_convert => "str" convert_initializer
*/
fn ast_fn302<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_STRING::new(Some(ref_1_0), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
string_convert => "str"
*/
fn ast_fn303<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_STRING::new(Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_STRING(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
syntax_spec => identifier color
*/
fn ast_fn304<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::RGBA(obj) = ref_1_1 { obj } else { panic!("invalid node") };
  let ref_3_0 = SyntaxSpec::new(ref_0_0, Some(ref_1_1));
  slots.assign(0, AstSlot(ASTNode::SyntaxSpec(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
syntax_spec => color
*/
fn ast_fn305<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_1 = i0;
  let ref_0_1 = if let ASTNode::RGBA(obj) = ref_0_1 { obj } else { panic!("invalid node") };
  let ref_2_0 = SyntaxSpec::new(Default::default(), Some(ref_0_1));
  slots.assign(0, AstSlot(ASTNode::SyntaxSpec(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
syntax_spec => identifier
*/
fn ast_fn306<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_2_0 = SyntaxSpec::new(ref_0_0, Default::default());
  slots.assign(0, AstSlot(ASTNode::SyntaxSpec(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
list => symbol "(+" terminal ')'
*/
fn ast_fn307<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_0_1 = i0;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Terminal(obj) = ref_2_2 { obj } else { panic!("invalid node") };
  let ref_5_0 =
    List_Production::new(false, ref_0_1, Some(ref_2_2), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
list => symbol "(+" ')'
*/
fn ast_fn308<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_0_1 = i0;
  let ref_4_0 =
    List_Production::new(false, ref_0_1, Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
list => symbol "(*" terminal ')'
*/
fn ast_fn309<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, ..) = slots.take(2);
  let AstSlot(_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;

  let ref_5_0 = true;
  let ref_0_1 = i0;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Terminal(obj) = ref_2_2 { obj } else { panic!("invalid node") };
  let ref_6_0 =
    List_Production::new(ref_5_0, ref_0_1, Some(ref_2_2), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
list => symbol "(*" ')'
*/
fn ast_fn310<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_4_0 = true;
  let ref_0_1 = i0;
  let ref_5_0 =
    List_Production::new(ref_4_0, ref_0_1, Default::default(), rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::List_Production(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
import_production_symbol => tk:identifier_syms '::' tk:identifier_syms
*/
fn ast_fn314<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot(i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_1 = rng2.to_token(_ctx_.get_reader());
  let ref_2_1 = ref_2_1.to_string();
  let ref_4_0 = Production_Import_Symbol::new(ref_0_0, ref_2_1, rng.to_token(_ctx_.get_reader()));
  slots.assign(
    0,
    AstSlot(ASTNode::Production_Import_Symbol(Box::new(ref_4_0)), rng, TokenRange::default()),
  )
}

/*
bool_convert => "bool" convert_initializer
*/
fn ast_fn315<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0 { obj } else { panic!("invalid node") };
  let ref_3_0 = AST_BOOL::new(Some(ref_1_0), false, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
bool_convert => "bool"
*/
fn ast_fn316<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(_, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_2_0 = AST_BOOL::new(Default::default(), false, rng.to_token(_ctx_.get_reader()));
  slots.assign(0, AstSlot(ASTNode::AST_BOOL(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => ignore_clause_list_1 terminal_non_terminal
*/
fn ast_fn317<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => ignore_clause_list_1 terminal
*/
fn ast_fn318<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => ignore_clause_list_1 class
*/
fn ast_fn319<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => terminal_non_terminal
*/
fn ast_fn320<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => terminal
*/
fn ast_fn321<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => class
*/
fn ast_fn322<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:id
*/
fn ast_fn323<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:sym
*/
fn ast_fn324<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:num
*/
fn ast_fn325<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:sp
*/
fn ast_fn326<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 escaped
*/
fn ast_fn327<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let AstSlot(i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;

  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:id
*/
fn ast_fn328<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:sym
*/
fn ast_fn329<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:num
*/
fn ast_fn330<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:sp
*/
fn ast_fn331<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => escaped
*/
fn ast_fn332<R: Reader + UTF8Reader, M>(
  _ctx_: &ParseContextOld<R, M>,
  slots: &AstStackSlice<AstSlot<ASTNode>>,
) {
  let AstSlot(i0, rng0, _) = slots.take(0);
  let rng = rng0;

  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot(ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

struct ReduceFunctions<R: Reader + UTF8Reader, M>(pub [Reducer<R, M, ASTNode>; 333]);
impl<R: Reader + UTF8Reader, M> ReduceFunctions<R, M> {
  pub const fn new() -> Self {
    Self([
      /* 0 */ ast_fn000::<R, M>,
      /* 1 */ ast_fn001::<R, M>,
      /* 2 */ ast_fn002::<R, M>,
      /* 3 */ ast_fn003::<R, M>,
      /* 4 */ ast_fn004::<R, M>,
      /* 5 */ ast_fn005::<R, M>,
      /* 6 */ ast_fn006::<R, M>,
      /* 7 */ ast_fn007::<R, M>,
      /* 8 */ ast_fn008::<R, M>,
      /* 9 */ ast_fn009::<R, M>,
      /* 10 */ ast_fn010::<R, M>,
      /* 11 */ ast_fn011::<R, M>,
      /* 12 */ ast_fn012::<R, M>,
      /* 13 */ ast_fn013::<R, M>,
      /* 14 */ ast_fn014::<R, M>,
      /* 15 */ ast_fn015::<R, M>,
      /* 16 */ ast_fn016::<R, M>,
      /* 17 */ ast_fn017::<R, M>,
      /* 18 */ ast_fn018::<R, M>,
      /* 19 */ ast_fn019::<R, M>,
      /* 20 */ ast_fn020::<R, M>,
      /* 21 */ ast_fn021::<R, M>,
      /* 22 */ ast_fn022::<R, M>,
      /* 23 */ ast_fn023::<R, M>,
      /* 24 */ ast_fn024::<R, M>,
      /* 25 */ ast_fn025::<R, M>,
      /* 26 */ ast_fn026::<R, M>,
      /* 27 */ ast_fn027::<R, M>,
      /* 28 */ ast_fn028::<R, M>,
      /* 29 1 */ default_fn::<R, M>,
      /* 30 */ ast_fn030::<R, M>,
      /* 31 */ ast_fn031::<R, M>,
      /* 32 */ ast_fn032::<R, M>,
      /* 33 */ ast_fn033::<R, M>,
      /* 34 */ ast_fn034::<R, M>,
      /* 35 1 */ default_fn::<R, M>,
      /* 36 1 */ default_fn::<R, M>,
      /* 37 */ ast_fn037::<R, M>,
      /* 38 */ ast_fn038::<R, M>,
      /* 39 */ ast_fn039::<R, M>,
      /* 40 */ ast_fn040::<R, M>,
      /* 41 */ ast_fn041::<R, M>,
      /* 42 */ ast_fn042::<R, M>,
      /* 43 */ ast_fn043::<R, M>,
      /* 44 */ ast_fn044::<R, M>,
      /* 45 */ ast_fn045::<R, M>,
      /* 46 */ ast_fn046::<R, M>,
      /* 47 */ ast_fn047::<R, M>,
      /* 48 */ ast_fn048::<R, M>,
      /* 49 1 */ default_fn::<R, M>,
      /* 50 */ ast_fn050::<R, M>,
      /* 51 */ ast_fn051::<R, M>,
      /* 52 */ ast_fn052::<R, M>,
      /* 53 */ ast_fn053::<R, M>,
      /* 54 */ ast_fn054::<R, M>,
      /* 55 */ ast_fn055::<R, M>,
      /* 56 */ ast_fn056::<R, M>,
      /* 57 */ ast_fn057::<R, M>,
      /* 58 */ ast_fn058::<R, M>,
      /* 59 */ ast_fn059::<R, M>,
      /* 60 */ ast_fn060::<R, M>,
      /* 61 */ ast_fn061::<R, M>,
      /* 62 */ ast_fn062::<R, M>,
      /* 63 */ ast_fn063::<R, M>,
      /* 64 */ ast_fn064::<R, M>,
      /* 65 */ ast_fn065::<R, M>,
      /* 66 */ ast_fn066::<R, M>,
      /* 67 1 */ default_fn::<R, M>,
      /* 68 1 */ default_fn::<R, M>,
      /* 69 1 */ default_fn::<R, M>,
      /* 70 1 */ default_fn::<R, M>,
      /* 71 1 */ default_fn::<R, M>,
      /* 72 1 */ default_fn::<R, M>,
      /* 73 1 */ default_fn::<R, M>,
      /* 74 1 */ default_fn::<R, M>,
      /* 75 */ ast_fn075::<R, M>,
      /* 76 */ ast_fn076::<R, M>,
      /* 77 */ ast_fn077::<R, M>,
      /* 78 */ ast_fn078::<R, M>,
      /* 79 */ ast_fn079::<R, M>,
      /* 80 */ ast_fn080::<R, M>,
      /* 81 */ ast_fn081::<R, M>,
      /* 82 */ ast_fn082::<R, M>,
      /* 83 */ ast_fn083::<R, M>,
      /* 84 */ ast_fn084::<R, M>,
      /* 85 1 */ default_fn::<R, M>,
      /* 86 */ ast_fn086::<R, M>,
      /* 87 1 */ default_fn::<R, M>,
      /* 88 */ ast_fn088::<R, M>,
      /* 89 */ ast_fn089::<R, M>,
      /* 90 */ ast_fn090::<R, M>,
      /* 91 */ ast_fn091::<R, M>,
      /* 92 */ ast_fn092::<R, M>,
      /* 93 */ ast_fn093::<R, M>,
      /* 94 */ ast_fn094::<R, M>,
      /* 95 */ ast_fn095::<R, M>,
      /* 96 */ ast_fn096::<R, M>,
      /* 97 */ ast_fn097::<R, M>,
      /* 98 */ ast_fn098::<R, M>,
      /* 99 */ ast_fn099::<R, M>,
      /* 100 */ ast_fn100::<R, M>,
      /* 101 */ ast_fn101::<R, M>,
      /* 102 */ ast_fn102::<R, M>,
      /* 103 */ ast_fn103::<R, M>,
      /* 104 */ ast_fn104::<R, M>,
      /* 105 */ ast_fn105::<R, M>,
      /* 106 */ ast_fn106::<R, M>,
      /* 107 */ ast_fn107::<R, M>,
      /* 108 */ ast_fn108::<R, M>,
      /* 109 */ ast_fn109::<R, M>,
      /* 110 */ ast_fn110::<R, M>,
      /* 111 */ ast_fn111::<R, M>,
      /* 112 */ ast_fn112::<R, M>,
      /* 113 */ ast_fn113::<R, M>,
      /* 114 */ ast_fn114::<R, M>,
      /* 115 */ ast_fn115::<R, M>,
      /* 116 */ ast_fn116::<R, M>,
      /* 117 */ ast_fn117::<R, M>,
      /* 118 */ ast_fn118::<R, M>,
      /* 119 */ ast_fn119::<R, M>,
      /* 120 */ ast_fn120::<R, M>,
      /* 121 */ ast_fn121::<R, M>,
      /* 122 */ ast_fn122::<R, M>,
      /* 123 */ ast_fn123::<R, M>,
      /* 124 */ ast_fn124::<R, M>,
      /* 125 */ ast_fn125::<R, M>,
      /* 126 */ ast_fn126::<R, M>,
      /* 127 */ ast_fn127::<R, M>,
      /* 128 */ ast_fn128::<R, M>,
      /* 129 */ ast_fn129::<R, M>,
      /* 130 */ ast_fn130::<R, M>,
      /* 131 */ ast_fn131::<R, M>,
      /* 132 */ ast_fn132::<R, M>,
      /* 133 */ ast_fn133::<R, M>,
      /* 134 */ ast_fn134::<R, M>,
      /* 135 */ ast_fn135::<R, M>,
      /* 136 */ ast_fn136::<R, M>,
      /* 137 */ ast_fn137::<R, M>,
      /* 138 */ ast_fn138::<R, M>,
      /* 139 */ ast_fn139::<R, M>,
      /* 140 */ ast_fn140::<R, M>,
      /* 141 */ ast_fn141::<R, M>,
      /* 142 */ ast_fn142::<R, M>,
      /* 143 */ ast_fn143::<R, M>,
      /* 144 */ ast_fn144::<R, M>,
      /* 145 */ ast_fn145::<R, M>,
      /* 146 */ ast_fn146::<R, M>,
      /* 147 */ ast_fn147::<R, M>,
      /* 148 */ ast_fn148::<R, M>,
      /* 149 1 */ default_fn::<R, M>,
      /* 150 */ ast_fn150::<R, M>,
      /* 151 */ ast_fn151::<R, M>,
      /* 152 */ ast_fn152::<R, M>,
      /* 153 */ ast_fn153::<R, M>,
      /* 154 */ ast_fn154::<R, M>,
      /* 155 */ ast_fn155::<R, M>,
      /* 156 */ ast_fn156::<R, M>,
      /* 157 */ ast_fn157::<R, M>,
      /* 158 */ ast_fn158::<R, M>,
      /* 159 */ ast_fn159::<R, M>,
      /* 160 */ ast_fn160::<R, M>,
      /* 161 */ ast_fn161::<R, M>,
      /* 162 */ ast_fn162::<R, M>,
      /* 163 */ ast_fn163::<R, M>,
      /* 164 */ ast_fn164::<R, M>,
      /* 165 */ ast_fn165::<R, M>,
      /* 166 */ ast_fn166::<R, M>,
      /* 167 */ ast_fn167::<R, M>,
      /* 168 */ ast_fn168::<R, M>,
      /* 169 1 */ default_fn::<R, M>,
      /* 170 1 */ default_fn::<R, M>,
      /* 171 1 */ default_fn::<R, M>,
      /* 172 1 */ default_fn::<R, M>,
      /* 173 1 */ default_fn::<R, M>,
      /* 174 */ ast_fn174::<R, M>,
      /* 175 */ ast_fn175::<R, M>,
      /* 176 */ ast_fn176::<R, M>,
      /* 177 */ ast_fn177::<R, M>,
      /* 178 */ ast_fn178::<R, M>,
      /* 179 */ ast_fn179::<R, M>,
      /* 180 */ ast_fn180::<R, M>,
      /* 181 */ ast_fn181::<R, M>,
      /* 182 */ ast_fn182::<R, M>,
      /* 183 */ ast_fn183::<R, M>,
      /* 184 */ ast_fn184::<R, M>,
      /* 185 */ ast_fn185::<R, M>,
      /* 186 */ ast_fn186::<R, M>,
      /* 187 */ ast_fn187::<R, M>,
      /* 188 */ ast_fn188::<R, M>,
      /* 189 */ ast_fn189::<R, M>,
      /* 190 */ ast_fn190::<R, M>,
      /* 191 */ ast_fn191::<R, M>,
      /* 192 */ ast_fn192::<R, M>,
      /* 193 */ ast_fn193::<R, M>,
      /* 194 */ ast_fn194::<R, M>,
      /* 195 */ ast_fn195::<R, M>,
      /* 196 */ ast_fn196::<R, M>,
      /* 197 */ ast_fn197::<R, M>,
      /* 198 */ ast_fn198::<R, M>,
      /* 199 */ ast_fn199::<R, M>,
      /* 200 */ ast_fn200::<R, M>,
      /* 201 */ ast_fn201::<R, M>,
      /* 202 */ ast_fn202::<R, M>,
      /* 203 1 */ default_fn::<R, M>,
      /* 204 */ ast_fn204::<R, M>,
      /* 205 */ ast_fn205::<R, M>,
      /* 206 */ ast_fn206::<R, M>,
      /* 207 1 */ default_fn::<R, M>,
      /* 208 1 */ default_fn::<R, M>,
      /* 209 1 */ default_fn::<R, M>,
      /* 210 1 */ default_fn::<R, M>,
      /* 211 1 */ default_fn::<R, M>,
      /* 212 */ ast_fn212::<R, M>,
      /* 213 */ ast_fn213::<R, M>,
      /* 214 */ ast_fn214::<R, M>,
      /* 215 */ ast_fn215::<R, M>,
      /* 216 */ ast_fn216::<R, M>,
      /* 217 */ ast_fn217::<R, M>,
      /* 218 */ ast_fn218::<R, M>,
      /* 219 */ ast_fn219::<R, M>,
      /* 220 */ ast_fn220::<R, M>,
      /* 221 */ ast_fn221::<R, M>,
      /* 222 */ ast_fn222::<R, M>,
      /* 223 */ ast_fn223::<R, M>,
      /* 224 */ ast_fn224::<R, M>,
      /* 225 */ ast_fn225::<R, M>,
      /* 226 */ ast_fn226::<R, M>,
      /* 227 */ ast_fn227::<R, M>,
      /* 228 */ ast_fn228::<R, M>,
      /* 229 */ ast_fn229::<R, M>,
      /* 230 */ ast_fn230::<R, M>,
      /* 231 */ ast_fn231::<R, M>,
      /* 232 */ ast_fn232::<R, M>,
      /* 233 */ ast_fn233::<R, M>,
      /* 234 */ ast_fn234::<R, M>,
      /* 235 */ ast_fn235::<R, M>,
      /* 236 */ ast_fn236::<R, M>,
      /* 237 */ ast_fn237::<R, M>,
      /* 238 */ ast_fn238::<R, M>,
      /* 239 */ ast_fn239::<R, M>,
      /* 240 */ ast_fn240::<R, M>,
      /* 241 */ ast_fn241::<R, M>,
      /* 242 */ ast_fn242::<R, M>,
      /* 243 1 */ default_fn::<R, M>,
      /* 244 */ ast_fn244::<R, M>,
      /* 245 */ ast_fn245::<R, M>,
      /* 246 */ ast_fn246::<R, M>,
      /* 247 1 */ default_fn::<R, M>,
      /* 248 1 */ default_fn::<R, M>,
      /* 249 */ ast_fn249::<R, M>,
      /* 250 */ ast_fn250::<R, M>,
      /* 251 */ ast_fn251::<R, M>,
      /* 252 */ ast_fn252::<R, M>,
      /* 253 */ ast_fn253::<R, M>,
      /* 254 */ ast_fn254::<R, M>,
      /* 255 */ ast_fn255::<R, M>,
      /* 256 */ ast_fn256::<R, M>,
      /* 257 1 */ default_fn::<R, M>,
      /* 258 */ ast_fn258::<R, M>,
      /* 259 */ ast_fn259::<R, M>,
      /* 260 */ ast_fn260::<R, M>,
      /* 261 */ ast_fn261::<R, M>,
      /* 262 */ ast_fn262::<R, M>,
      /* 263 */ ast_fn263::<R, M>,
      /* 264 */ ast_fn264::<R, M>,
      /* 265 */ ast_fn265::<R, M>,
      /* 266 */ ast_fn266::<R, M>,
      /* 267 */ ast_fn267::<R, M>,
      /* 268 */ ast_fn268::<R, M>,
      /* 269 */ ast_fn269::<R, M>,
      /* 270 1 */ default_fn::<R, M>,
      /* 271 1 */ default_fn::<R, M>,
      /* 272 1 */ default_fn::<R, M>,
      /* 273 1 */ default_fn::<R, M>,
      /* 274 */ ast_fn274::<R, M>,
      /* 275 */ ast_fn275::<R, M>,
      /* 276 */ ast_fn276::<R, M>,
      /* 277 */ ast_fn277::<R, M>,
      /* 278 1 */ default_fn::<R, M>,
      /* 279 1 */ default_fn::<R, M>,
      /* 280 1 */ default_fn::<R, M>,
      /* 281 1 */ default_fn::<R, M>,
      /* 282 1 */ default_fn::<R, M>,
      /* 283 1 */ default_fn::<R, M>,
      /* 284 1 */ default_fn::<R, M>,
      /* 285 */ ast_fn285::<R, M>,
      /* 286 */ ast_fn286::<R, M>,
      /* 287 */ ast_fn287::<R, M>,
      /* 288 1 */ default_fn::<R, M>,
      /* 289 */ ast_fn289::<R, M>,
      /* 290 */ ast_fn290::<R, M>,
      /* 291 */ ast_fn291::<R, M>,
      /* 292 */ ast_fn292::<R, M>,
      /* 293 */ ast_fn293::<R, M>,
      /* 294 */ ast_fn294::<R, M>,
      /* 295 */ ast_fn295::<R, M>,
      /* 296 */ ast_fn296::<R, M>,
      /* 297 */ ast_fn297::<R, M>,
      /* 298 */ ast_fn298::<R, M>,
      /* 299 */ ast_fn299::<R, M>,
      /* 300 */ ast_fn300::<R, M>,
      /* 301 */ ast_fn301::<R, M>,
      /* 302 */ ast_fn302::<R, M>,
      /* 303 */ ast_fn303::<R, M>,
      /* 304 */ ast_fn304::<R, M>,
      /* 305 */ ast_fn305::<R, M>,
      /* 306 */ ast_fn306::<R, M>,
      /* 307 */ ast_fn307::<R, M>,
      /* 308 */ ast_fn308::<R, M>,
      /* 309 */ ast_fn309::<R, M>,
      /* 310 */ ast_fn310::<R, M>,
      /* 311 1 */ default_fn::<R, M>,
      /* 312 1 */ default_fn::<R, M>,
      /* 313 1 */ default_fn::<R, M>,
      /* 314 */ ast_fn314::<R, M>,
      /* 315 */ ast_fn315::<R, M>,
      /* 316 */ ast_fn316::<R, M>,
      /* 317 */ ast_fn317::<R, M>,
      /* 318 */ ast_fn318::<R, M>,
      /* 319 */ ast_fn319::<R, M>,
      /* 320 */ ast_fn320::<R, M>,
      /* 321 */ ast_fn321::<R, M>,
      /* 322 */ ast_fn322::<R, M>,
      /* 323 */ ast_fn323::<R, M>,
      /* 324 */ ast_fn324::<R, M>,
      /* 325 */ ast_fn325::<R, M>,
      /* 326 */ ast_fn326::<R, M>,
      /* 327 */ ast_fn327::<R, M>,
      /* 328 */ ast_fn328::<R, M>,
      /* 329 */ ast_fn329::<R, M>,
      /* 330 */ ast_fn330::<R, M>,
      /* 331 */ ast_fn331::<R, M>,
      /* 332 */ ast_fn332::<R, M>,
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

impl<T> Reader for T where T: ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug {}

pub struct Parser<T: Reader, M>(ParseContextOld<T, M>, T);

impl<T: Reader, M> Iterator for Parser<T, M> {
  type Item = ParseActionType;

  #[inline(always)]
  fn next(&mut self) -> Option<Self::Item> {
    unsafe {
      if !self.0.is_active {
        None
      } else {
        let _ptr = &mut self.0 as *const ParseContextOld<T, M>;
        Some(next(_ptr as *mut u8))
      }
    }
  }
}

impl<T: Reader, M> Parser<T, M> {
  /// Create a new parser context to parser the input with
  /// the grammar `sherpa`
  #[inline(always)]
  fn new(mut reader: T) -> Self {
    let mut parser = Self(ParseContextOld::<T, M>::new_llvm(), reader);
    parser.construct_context();
    parser
  }

  /// Initialize the parser to recognize the given starting production
  /// within the input. This method is chainable.
  #[inline(always)]
  fn set_start_point(&mut self, start_point: u64) -> &mut Self {
    unsafe {
      let _ptr = &mut self.0 as *const ParseContextOld<T, M>;
      prime(_ptr as *mut u8, start_point as u32);
    }

    self
  }

  #[inline(always)]
  fn construct_context(&mut self) {
    unsafe {
      let _ptr = &mut self.0 as *const ParseContextOld<T, M>;
      let _rdr = &mut self.1 as *const T;
      init(_ptr as *mut u8, _rdr as *mut u8);
    }
  }

  #[inline(always)]
  fn destroy_context(&mut self) {
    let _ptr = &mut self.0 as *const ParseContextOld<T, M>;
    unsafe {
      drop(_ptr as *mut u8);
    }
  }

  /// `<> grammar >
  //
  //         preamble(*) ( production | append_production )(+)
  //
  //             :ast { t_Grammar, c_Version_1_0, preamble:$1, productions:$2, tok }`
  pub fn new_grammar_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(0);
    ctx
  }

  /// `<> struct >
  //
  //     "{" type_identifier^t ( "," struct_prop(+",") )? '}'
  //         :ast { t_AST_Struct, typ:$t, props:$3, tok }`
  pub fn new_ast_struct_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(1);
    ctx
  }

  /// `<> expression >
  //
  //     string_convert
  //
  //     | numeric_convert
  //
  //     | bool_convert
  //
  //     | literal
  //
  //     | vector
  //
  //     | token
  //
  //     | add
  //
  //     | map`
  pub fn new_ast_expression_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(2);
    ctx
  }

  /// `<> state >
  //
  //         state_declaration  scanner_declaration? "excepts"? top_level_instructions expected_symbols?
  //
  //             :ast { t_IR_STATE, c_IR, c_IrState, id:$1, scanner: $2, except_handler: bool($3), instructions: $4, symbol_meta:$5 }`
  pub fn new_ir_parser(reader: T) -> Self {
    let mut ctx = Self::new(reader);
    ctx.set_start_point(3);
    ctx
  }
}

impl<T: Reader, M> Drop for Parser<T, M> {
  fn drop(&mut self) {
    self.destroy_context();
  }
}
pub mod ast {
  use crate::grammar::compile::parser::parse_context_old::{
    llvm_map_result_action_old,
    llvm_map_shift_action_old,
  };

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

  pub fn grammar_from(reader: UTF8StringReader) -> Result<Box<Grammar>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32> =
      ReduceFunctions::<UTF8StringReader, u32>::new();

    let mut ctx = Parser::new_grammar_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContextOld<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete((i0, ..)) => {
        let ref_0_0 = i0;

        let ref_0_0 =
          if let ASTNode::Grammar(obj) = ref_0_0 { obj } else { panic!("invalid node") };
        Ok(ref_0_0)
      }
      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn ast_struct_from(reader: UTF8StringReader) -> Result<Box<AST_Struct>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32> =
      ReduceFunctions::<UTF8StringReader, u32>::new();

    let mut ctx = Parser::new_ast_struct_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContextOld<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete((i0, ..)) => {
        let ref_0_0 = i0;
        let ref_0_0 =
          if let ASTNode::AST_Struct(obj) = ref_0_0 { obj } else { panic!("invalid node") };
        Ok(ref_0_0)
      }
      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn ast_expression_from(reader: UTF8StringReader) -> Result<ASTNode, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32> =
      ReduceFunctions::<UTF8StringReader, u32>::new();

    let mut ctx = Parser::new_ast_expression_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContextOld<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete((i0, ..)) => {
        let ref_0_0 = i0;
        Ok(ref_0_0)
      }
      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }

  pub fn ir_from(reader: UTF8StringReader) -> Result<Box<IR_STATE>, SherpaParseError> {
    const reduce_functions: ReduceFunctions<UTF8StringReader, u32> =
      ReduceFunctions::<UTF8StringReader, u32>::new();

    let mut ctx = Parser::new_ir_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action_old::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContextOld<UTF8StringReader, u32>;

    match unsafe { ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete((i0, node_tok, ..)) => {
        let ref_0_0 = i0;
        let ref_0_0 = if let ASTNode::IR_STATE(obj) = ref_0_0 {
          obj
        } else {
          return Err(SherpaParseError {
            inline_message: "Unexpected node".to_string(),
            last_production: 0,
            loc: node_tok.to_token(&ctx.1),
            message: "Failed to parse".to_string(),
          });
        };
        Ok(ref_0_0)
      }
      ParseResult::Error(err_tok, _) => Err(SherpaParseError {
        inline_message: "Token not recognized".to_string(),
        last_production: 0,
        loc: err_tok.to_token(&ctx.1),
        message: "Failed to parse".to_string(),
      }),
      _ => unreachable!(),
    }
  }
}
