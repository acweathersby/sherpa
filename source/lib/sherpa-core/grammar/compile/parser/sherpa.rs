/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta1
/// - **SOURCE**: /home/work/projects/lib_sherpa/source/lib/sherpa-core/test/bootstrap/grammar/grammar.hcg
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


use sherpa_runtime::types::{ast::*, *};

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

pub trait ASTParse<T>  {
  
  fn grammar_from(input:T) -> Result<Box<Grammar>, SherpaParseError>;
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
  AST_Add(Box<AST_Add>),
  List_Production(Box<List_Production>),
  AST_Vector(Box<AST_Vector>),
  Production_Symbol(Box<Production_Symbol>),
  Grammar(Box<Grammar>),
  AST_U64(Box<AST_U64>),
  Priority(Box<Priority>),
  Import(Box<Import>),
  Init(Box<Init>),
  AST_IndexReference(Box<AST_IndexReference>),
  AST_Member(Box<AST_Member>),
  Production_Import_Symbol(Box<Production_Import_Symbol>),
  AST_I32(Box<AST_I32>),
  AST_I8(Box<AST_I8>),
  AST_F32(Box<AST_F32>),
  TemplateProductionSymbol(Box<TemplateProductionSymbol>),
  AST_Token(Box<AST_Token>),
  Export(Box<Export>),
  Name(Box<Name>),
  AST_BOOL(Box<AST_BOOL>),
  Group_Production(Box<Group_Production>),
  AST_Statements(Box<AST_Statements>),
  AST_U8(Box<AST_U8>),
  Production(Box<Production>),
  Ignore(Box<Ignore>),
  AST_STRING(Box<AST_STRING>),
  AST_U32(Box<AST_U32>),
  AST_ClassId(Box<AST_ClassId>),
  AST_I64(Box<AST_I64>),
  AST_U16(Box<AST_U16>),
  ClassSymbol(Box<ClassSymbol>),
  AST_F64(Box<AST_F64>),
  AST_Map(Box<AST_Map>),
  AnyGroup(Box<AnyGroup>),
  Range(Box<Range>),
  AST_I16(Box<AST_I16>),
  AST_NUMBER(Box<AST_NUMBER>),
  AST_Struct(Box<AST_Struct>),
  Rule(Box<Rule>),
  AnnotatedSymbol(Box<AnnotatedSymbol>),
  AST_Property(Box<AST_Property>),
  Ascript(Box<Ascript>),
  Production_Terminal_Symbol(Box<Production_Terminal_Symbol>),
  Terminal(Box<Terminal>),
}
impl ASTNode {
  pub fn as_AST_NamedReference(&self) -> Option<&AST_NamedReference> {
    match self {
      Self::AST_NamedReference(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_NamedReference_mut(&mut self) -> Option<&mut AST_NamedReference> {
    match self {
      Self::AST_NamedReference(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Add(&self) -> Option<&AST_Add> {
    match self {
      Self::AST_Add(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Add_mut(&mut self) -> Option<&mut AST_Add> {
    match self {
      Self::AST_Add(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_List_Production(&self) -> Option<&List_Production> {
    match self {
      Self::List_Production(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_List_Production_mut(&mut self) -> Option<&mut List_Production> {
    match self {
      Self::List_Production(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Vector(&self) -> Option<&AST_Vector> {
    match self {
      Self::AST_Vector(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Vector_mut(&mut self) -> Option<&mut AST_Vector> {
    match self {
      Self::AST_Vector(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Production_Symbol(&self) -> Option<&Production_Symbol> {
    match self {
      Self::Production_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Production_Symbol_mut(&mut self) -> Option<&mut Production_Symbol> {
    match self {
      Self::Production_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Grammar(&self) -> Option<&Grammar> {
    match self {
      Self::Grammar(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Grammar_mut(&mut self) -> Option<&mut Grammar> {
    match self {
      Self::Grammar(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_U64(&self) -> Option<&AST_U64> {
    match self {
      Self::AST_U64(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_U64_mut(&mut self) -> Option<&mut AST_U64> {
    match self {
      Self::AST_U64(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Priority(&self) -> Option<&Priority> {
    match self {
      Self::Priority(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Priority_mut(&mut self) -> Option<&mut Priority> {
    match self {
      Self::Priority(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Import(&self) -> Option<&Import> {
    match self {
      Self::Import(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Import_mut(&mut self) -> Option<&mut Import> {
    match self {
      Self::Import(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Init(&self) -> Option<&Init> {
    match self {
      Self::Init(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Init_mut(&mut self) -> Option<&mut Init> {
    match self {
      Self::Init(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_IndexReference(&self) -> Option<&AST_IndexReference> {
    match self {
      Self::AST_IndexReference(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_IndexReference_mut(&mut self) -> Option<&mut AST_IndexReference> {
    match self {
      Self::AST_IndexReference(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Member(&self) -> Option<&AST_Member> {
    match self {
      Self::AST_Member(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Member_mut(&mut self) -> Option<&mut AST_Member> {
    match self {
      Self::AST_Member(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Production_Import_Symbol(&self) -> Option<&Production_Import_Symbol> {
    match self {
      Self::Production_Import_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Production_Import_Symbol_mut(&mut self) -> Option<&mut Production_Import_Symbol> {
    match self {
      Self::Production_Import_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_I32(&self) -> Option<&AST_I32> {
    match self {
      Self::AST_I32(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_I32_mut(&mut self) -> Option<&mut AST_I32> {
    match self {
      Self::AST_I32(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_I8(&self) -> Option<&AST_I8> {
    match self {
      Self::AST_I8(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_I8_mut(&mut self) -> Option<&mut AST_I8> {
    match self {
      Self::AST_I8(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_F32(&self) -> Option<&AST_F32> {
    match self {
      Self::AST_F32(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_F32_mut(&mut self) -> Option<&mut AST_F32> {
    match self {
      Self::AST_F32(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_TemplateProductionSymbol(&self) -> Option<&TemplateProductionSymbol> {
    match self {
      Self::TemplateProductionSymbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_TemplateProductionSymbol_mut(&mut self) -> Option<&mut TemplateProductionSymbol> {
    match self {
      Self::TemplateProductionSymbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Token(&self) -> Option<&AST_Token> {
    match self {
      Self::AST_Token(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Token_mut(&mut self) -> Option<&mut AST_Token> {
    match self {
      Self::AST_Token(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Export(&self) -> Option<&Export> {
    match self {
      Self::Export(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Export_mut(&mut self) -> Option<&mut Export> {
    match self {
      Self::Export(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Name(&self) -> Option<&Name> {
    match self {
      Self::Name(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Name_mut(&mut self) -> Option<&mut Name> {
    match self {
      Self::Name(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_BOOL(&self) -> Option<&AST_BOOL> {
    match self {
      Self::AST_BOOL(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_BOOL_mut(&mut self) -> Option<&mut AST_BOOL> {
    match self {
      Self::AST_BOOL(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Group_Production(&self) -> Option<&Group_Production> {
    match self {
      Self::Group_Production(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Group_Production_mut(&mut self) -> Option<&mut Group_Production> {
    match self {
      Self::Group_Production(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Statements(&self) -> Option<&AST_Statements> {
    match self {
      Self::AST_Statements(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Statements_mut(&mut self) -> Option<&mut AST_Statements> {
    match self {
      Self::AST_Statements(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_U8(&self) -> Option<&AST_U8> {
    match self {
      Self::AST_U8(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_U8_mut(&mut self) -> Option<&mut AST_U8> {
    match self {
      Self::AST_U8(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Production(&self) -> Option<&Production> {
    match self {
      Self::Production(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Production_mut(&mut self) -> Option<&mut Production> {
    match self {
      Self::Production(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Ignore(&self) -> Option<&Ignore> {
    match self {
      Self::Ignore(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Ignore_mut(&mut self) -> Option<&mut Ignore> {
    match self {
      Self::Ignore(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_STRING(&self) -> Option<&AST_STRING> {
    match self {
      Self::AST_STRING(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_STRING_mut(&mut self) -> Option<&mut AST_STRING> {
    match self {
      Self::AST_STRING(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_U32(&self) -> Option<&AST_U32> {
    match self {
      Self::AST_U32(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_U32_mut(&mut self) -> Option<&mut AST_U32> {
    match self {
      Self::AST_U32(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_ClassId(&self) -> Option<&AST_ClassId> {
    match self {
      Self::AST_ClassId(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_ClassId_mut(&mut self) -> Option<&mut AST_ClassId> {
    match self {
      Self::AST_ClassId(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_I64(&self) -> Option<&AST_I64> {
    match self {
      Self::AST_I64(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_I64_mut(&mut self) -> Option<&mut AST_I64> {
    match self {
      Self::AST_I64(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_U16(&self) -> Option<&AST_U16> {
    match self {
      Self::AST_U16(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_U16_mut(&mut self) -> Option<&mut AST_U16> {
    match self {
      Self::AST_U16(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_ClassSymbol(&self) -> Option<&ClassSymbol> {
    match self {
      Self::ClassSymbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_ClassSymbol_mut(&mut self) -> Option<&mut ClassSymbol> {
    match self {
      Self::ClassSymbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_F64(&self) -> Option<&AST_F64> {
    match self {
      Self::AST_F64(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_F64_mut(&mut self) -> Option<&mut AST_F64> {
    match self {
      Self::AST_F64(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Map(&self) -> Option<&AST_Map> {
    match self {
      Self::AST_Map(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Map_mut(&mut self) -> Option<&mut AST_Map> {
    match self {
      Self::AST_Map(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AnyGroup(&self) -> Option<&AnyGroup> {
    match self {
      Self::AnyGroup(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AnyGroup_mut(&mut self) -> Option<&mut AnyGroup> {
    match self {
      Self::AnyGroup(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Range(&self) -> Option<&Range> {
    match self {
      Self::Range(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Range_mut(&mut self) -> Option<&mut Range> {
    match self {
      Self::Range(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_I16(&self) -> Option<&AST_I16> {
    match self {
      Self::AST_I16(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_I16_mut(&mut self) -> Option<&mut AST_I16> {
    match self {
      Self::AST_I16(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_NUMBER(&self) -> Option<&AST_NUMBER> {
    match self {
      Self::AST_NUMBER(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_NUMBER_mut(&mut self) -> Option<&mut AST_NUMBER> {
    match self {
      Self::AST_NUMBER(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Struct(&self) -> Option<&AST_Struct> {
    match self {
      Self::AST_Struct(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Struct_mut(&mut self) -> Option<&mut AST_Struct> {
    match self {
      Self::AST_Struct(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Rule(&self) -> Option<&Rule> {
    match self {
      Self::Rule(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Rule_mut(&mut self) -> Option<&mut Rule> {
    match self {
      Self::Rule(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AnnotatedSymbol(&self) -> Option<&AnnotatedSymbol> {
    match self {
      Self::AnnotatedSymbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AnnotatedSymbol_mut(&mut self) -> Option<&mut AnnotatedSymbol> {
    match self {
      Self::AnnotatedSymbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_AST_Property(&self) -> Option<&AST_Property> {
    match self {
      Self::AST_Property(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_AST_Property_mut(&mut self) -> Option<&mut AST_Property> {
    match self {
      Self::AST_Property(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Ascript(&self) -> Option<&Ascript> {
    match self {
      Self::Ascript(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Ascript_mut(&mut self) -> Option<&mut Ascript> {
    match self {
      Self::Ascript(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Production_Terminal_Symbol(&self) -> Option<&Production_Terminal_Symbol> {
    match self {
      Self::Production_Terminal_Symbol(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Production_Terminal_Symbol_mut(&mut self) -> Option<&mut Production_Terminal_Symbol> {
    match self {
      Self::Production_Terminal_Symbol(val) => Some(val.as_mut()),
      _ => None
    }
  }
  pub fn as_Terminal(&self) -> Option<&Terminal> {
    match self {
      Self::Terminal(val) => Some(val.as_ref()),
      _ => None
    }
  }
  pub fn as_Terminal_mut(&mut self) -> Option<&mut Terminal> {
    match self {
      Self::Terminal(val) => Some(val.as_mut()),
      _ => None
    }
  }
  
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
      ASTNode::AST_NamedReference (node) => node.tok.clone()
  ,ASTNode::AST_Add (node) => node.tok.clone()
  ,ASTNode::List_Production (node) => node.tok.clone()
  ,ASTNode::AST_Vector (node) => node.tok.clone()
  ,ASTNode::Production_Symbol (node) => node.tok.clone()
  ,ASTNode::Grammar (node) => node.tok.clone()
  ,ASTNode::AST_U64 (node) => node.tok.clone()
  ,ASTNode::Import (node) => node.tok.clone()
  ,ASTNode::AST_IndexReference (node) => node.tok.clone()
  ,ASTNode::Production_Import_Symbol (node) => node.tok.clone()
  ,ASTNode::AST_I32 (node) => node.tok.clone()
  ,ASTNode::AST_I8 (node) => node.tok.clone()
  ,ASTNode::AST_F32 (node) => node.tok.clone()
  ,ASTNode::AST_BOOL (node) => node.tok.clone()
  ,ASTNode::Group_Production (node) => node.tok.clone()
  ,ASTNode::AST_Statements (node) => node.tok.clone()
  ,ASTNode::AST_U8 (node) => node.tok.clone()
  ,ASTNode::Production (node) => node.tok.clone()
  ,ASTNode::AST_STRING (node) => node.tok.clone()
  ,ASTNode::AST_U32 (node) => node.tok.clone()
  ,ASTNode::AST_ClassId (node) => node.tok.clone()
  ,ASTNode::AST_I64 (node) => node.tok.clone()
  ,ASTNode::AST_U16 (node) => node.tok.clone()
  ,ASTNode::ClassSymbol (node) => node.tok.clone()
  ,ASTNode::AST_F64 (node) => node.tok.clone()
  ,ASTNode::AST_Map (node) => node.tok.clone()
  ,ASTNode::AnyGroup (node) => node.tok.clone()
  ,ASTNode::AST_I16 (node) => node.tok.clone()
  ,ASTNode::AST_Struct (node) => node.tok.clone()
  ,ASTNode::Rule (node) => node.tok.clone()
  ,ASTNode::AnnotatedSymbol (node) => node.tok.clone()
  ,ASTNode::AST_Property (node) => node.tok.clone()
  ,ASTNode::Ascript (node) => node.tok.clone()
  ,ASTNode::Production_Terminal_Symbol (node) => node.tok.clone()
  ,ASTNode::Terminal (node) => node.tok.clone()
  ,ASTNode::TOKEN(val) => val.clone(),
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
      F64(_) | F32(_)| I64(_)| I32(_)| I16(_)| I8(_)| U64(_)| U32(_)| U16(_)| U8(_)
    )
  }
    
}

impl Default for ASTNode {
  fn default() -> Self {
    Self::NONE
  }
}
  
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
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
  AST_Add,
  List_Production,
  AST_Vector,
  Production_Symbol,
  Grammar,
  AST_U64,
  Priority,
  Import,
  Init,
  AST_IndexReference,
  AST_Member,
  Production_Import_Symbol,
  AST_I32,
  AST_I8,
  AST_F32,
  TemplateProductionSymbol,
  AST_Token,
  Export,
  Name,
  AST_BOOL,
  Group_Production,
  AST_Statements,
  AST_U8,
  Production,
  Ignore,
  AST_STRING,
  AST_U32,
  AST_ClassId,
  AST_I64,
  AST_U16,
  ClassSymbol,
  AST_F64,
  AST_Map,
  AnyGroup,
  Range,
  AST_I16,
  AST_NUMBER,
  AST_Struct,
  Rule,
  AnnotatedSymbol,
  AST_Property,
  Ascript,
  Production_Terminal_Symbol,
  Terminal,
}
pub trait GetASTNodeType { fn get_type(&self) -> ASTNodeType; }
impl GetASTNodeType for ASTNode {
  fn get_type(&self) -> ASTNodeType {
    match self{
      ASTNode::AST_NamedReference(..) => ASTNodeType::AST_NamedReference,
      ASTNode::AST_Add(..) => ASTNodeType::AST_Add,
      ASTNode::List_Production(..) => ASTNodeType::List_Production,
      ASTNode::AST_Vector(..) => ASTNodeType::AST_Vector,
      ASTNode::Production_Symbol(..) => ASTNodeType::Production_Symbol,
      ASTNode::Grammar(..) => ASTNodeType::Grammar,
      ASTNode::AST_U64(..) => ASTNodeType::AST_U64,
      ASTNode::Priority(..) => ASTNodeType::Priority,
      ASTNode::Import(..) => ASTNodeType::Import,
      ASTNode::Init(..) => ASTNodeType::Init,
      ASTNode::AST_IndexReference(..) => ASTNodeType::AST_IndexReference,
      ASTNode::AST_Member(..) => ASTNodeType::AST_Member,
      ASTNode::Production_Import_Symbol(..) => ASTNodeType::Production_Import_Symbol,
      ASTNode::AST_I32(..) => ASTNodeType::AST_I32,
      ASTNode::AST_I8(..) => ASTNodeType::AST_I8,
      ASTNode::AST_F32(..) => ASTNodeType::AST_F32,
      ASTNode::TemplateProductionSymbol(..) => ASTNodeType::TemplateProductionSymbol,
      ASTNode::AST_Token(..) => ASTNodeType::AST_Token,
      ASTNode::Export(..) => ASTNodeType::Export,
      ASTNode::Name(..) => ASTNodeType::Name,
      ASTNode::AST_BOOL(..) => ASTNodeType::AST_BOOL,
      ASTNode::Group_Production(..) => ASTNodeType::Group_Production,
      ASTNode::AST_Statements(..) => ASTNodeType::AST_Statements,
      ASTNode::AST_U8(..) => ASTNodeType::AST_U8,
      ASTNode::Production(..) => ASTNodeType::Production,
      ASTNode::Ignore(..) => ASTNodeType::Ignore,
      ASTNode::AST_STRING(..) => ASTNodeType::AST_STRING,
      ASTNode::AST_U32(..) => ASTNodeType::AST_U32,
      ASTNode::AST_ClassId(..) => ASTNodeType::AST_ClassId,
      ASTNode::AST_I64(..) => ASTNodeType::AST_I64,
      ASTNode::AST_U16(..) => ASTNodeType::AST_U16,
      ASTNode::ClassSymbol(..) => ASTNodeType::ClassSymbol,
      ASTNode::AST_F64(..) => ASTNodeType::AST_F64,
      ASTNode::AST_Map(..) => ASTNodeType::AST_Map,
      ASTNode::AnyGroup(..) => ASTNodeType::AnyGroup,
      ASTNode::Range(..) => ASTNodeType::Range,
      ASTNode::AST_I16(..) => ASTNodeType::AST_I16,
      ASTNode::AST_NUMBER(..) => ASTNodeType::AST_NUMBER,
      ASTNode::AST_Struct(..) => ASTNodeType::AST_Struct,
      ASTNode::Rule(..) => ASTNodeType::Rule,
      ASTNode::AnnotatedSymbol(..) => ASTNodeType::AnnotatedSymbol,
      ASTNode::AST_Property(..) => ASTNodeType::AST_Property,
      ASTNode::Ascript(..) => ASTNodeType::Ascript,
      ASTNode::Production_Terminal_Symbol(..) => ASTNodeType::Production_Terminal_Symbol,
      ASTNode::Terminal(..) => ASTNodeType::Terminal,
      _ => ASTNodeType::NONE,
    }
  }
}
#[derive(Debug, Clone)]
pub struct AST_NamedReference {
  pub value: String,
  pub tok: Token,
}
impl AST_NamedReference {
  #[inline]
  pub fn new (
    value:String,
    tok:Token,
  ) -> Self {
    Self{
      value,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_NamedReference }
}
#[derive(Debug, Clone)]
pub struct AST_Add {
  pub left: ASTNode,
  pub right: ASTNode,
  pub tok: Token,
}
impl AST_Add {
  #[inline]
  pub fn new (
    left:ASTNode,
    right:ASTNode,
    tok:Token,
  ) -> Self {
    Self{
      left,
      right,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Add }
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
  pub fn new (
    optional:bool,
    symbols:ASTNode,
    terminal_symbol:Option<Box<Terminal>>,
    tok:Token,
  ) -> Self {
    Self{
      optional,
      symbols,
      terminal_symbol,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::List_Production }
}
#[derive(Debug, Clone)]
pub struct AST_Vector {
  pub initializer: Vec<ASTNode>,
  pub tok: Token,
}
impl AST_Vector {
  #[inline]
  pub fn new (
    initializer:Vec<ASTNode>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Vector }
}
#[derive(Debug, Clone)]
pub struct Production_Symbol {
  pub name: String,
  pub tok: Token,
}
impl Production_Symbol {
  #[inline]
  pub fn new (
    name:String,
    tok:Token,
  ) -> Self {
    Self{
      name,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Production_Symbol }
}
#[derive(Debug, Clone)]
pub struct Grammar {
  pub preamble: Vec<ASTNode>,
  pub productions: Vec<Box<Production>>,
  pub tok: Token,
}
impl Grammar {
  #[inline]
  pub fn new (
    preamble:Vec<ASTNode>,
    productions:Vec<Box<Production>>,
    tok:Token,
  ) -> Self {
    Self{
      preamble,
      productions,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Grammar }
}
#[derive(Debug, Clone)]
pub struct AST_U64 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_U64 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_U64 }
}
#[derive(Debug, Clone)]
pub struct Priority {
  pub exclusive: bool,
  pub val: u32,
}
impl Priority {
  #[inline]
  pub fn new (
    exclusive:bool,
    val:u32,
  ) -> Self {
    Self{
      exclusive,
      val,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Priority }
}
#[derive(Debug, Clone)]
pub struct Import {
  pub reference: String,
  pub uri: String,
  pub tok: Token,
}
impl Import {
  #[inline]
  pub fn new (
    reference:String,
    uri:String,
    tok:Token,
  ) -> Self {
    Self{
      reference,
      uri,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Import }
}
#[derive(Debug, Clone)]
pub struct Init {
  pub expression: ASTNode,
}
impl Init {
  #[inline]
  pub fn new (
    expression:ASTNode,
  ) -> Self {
    Self{
      expression,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Init }
}
#[derive(Debug, Clone)]
pub struct AST_IndexReference {
  pub value: i64,
  pub tok: Token,
}
impl AST_IndexReference {
  #[inline]
  pub fn new (
    value:i64,
    tok:Token,
  ) -> Self {
    Self{
      value,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_IndexReference }
}
#[derive(Debug, Clone)]
pub struct AST_Member {
  pub property: Token,
  pub reference: ASTNode,
}
impl AST_Member {
  #[inline]
  pub fn new (
    property:Token,
    reference:ASTNode,
  ) -> Self {
    Self{
      property,
      reference,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Member }
}
#[derive(Debug, Clone)]
pub struct Production_Import_Symbol {
  pub module: String,
  pub name: String,
  pub tok: Token,
}
impl Production_Import_Symbol {
  #[inline]
  pub fn new (
    module:String,
    name:String,
    tok:Token,
  ) -> Self {
    Self{
      module,
      name,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Production_Import_Symbol }
}
#[derive(Debug, Clone)]
pub struct AST_I32 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_I32 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_I32 }
}
#[derive(Debug, Clone)]
pub struct AST_I8 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_I8 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_I8 }
}
#[derive(Debug, Clone)]
pub struct AST_F32 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_F32 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_F32 }
}
#[derive(Debug, Clone)]
pub struct TemplateProductionSymbol {
  pub prod_sym: ASTNode,
  pub template_productions: Box<Production_Symbol>,
}
impl TemplateProductionSymbol {
  #[inline]
  pub fn new (
    prod_sym:ASTNode,
    template_productions:Box<Production_Symbol>,
  ) -> Self {
    Self{
      prod_sym,
      template_productions,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::TemplateProductionSymbol }
}
#[derive(Debug, Clone)]
pub struct AST_Token {
  pub range: Option<Box<Range>>,
}
impl AST_Token {
  #[inline]
  pub fn new (
    range:Option<Box<Range>>,
  ) -> Self {
    Self{
      range,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Token }
}
#[derive(Debug, Clone)]
pub struct Export {
  pub production: ASTNode,
  pub reference: Token,
}
impl Export {
  #[inline]
  pub fn new (
    production:ASTNode,
    reference:Token,
  ) -> Self {
    Self{
      production,
      reference,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Export }
}
#[derive(Debug, Clone)]
pub struct Name {
  pub name: String,
}
impl Name {
  #[inline]
  pub fn new (
    name:String,
  ) -> Self {
    Self{
      name,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Name }
}
#[derive(Debug, Clone)]
pub struct AST_BOOL {
  pub initializer: Option<Box<Init>>,
  pub value: bool,
  pub tok: Token,
}
impl AST_BOOL {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    value:bool,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      value,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_BOOL }
}
#[derive(Debug, Clone)]
pub struct Group_Production {
  pub rules: Vec<Box<Rule>>,
  pub tok: Token,
}
impl Group_Production {
  #[inline]
  pub fn new (
    rules:Vec<Box<Rule>>,
    tok:Token,
  ) -> Self {
    Self{
      rules,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Group_Production }
}
#[derive(Debug, Clone)]
pub struct AST_Statements {
  pub statements: Vec<ASTNode>,
  pub tok: Token,
}
impl AST_Statements {
  #[inline]
  pub fn new (
    statements:Vec<ASTNode>,
    tok:Token,
  ) -> Self {
    Self{
      statements,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Statements }
}
#[derive(Debug, Clone)]
pub struct AST_U8 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_U8 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_U8 }
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
  pub fn new (
    is_append:bool,
    is_lazy:bool,
    name:String,
    name_sym:ASTNode,
    priority:Option<Box<Priority>>,
    rules:Vec<Box<Rule>>,
    template_names:Vec<String>,
    tok:Token,
  ) -> Self {
    Self{
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
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Production }
}
#[derive(Debug, Clone)]
pub struct Ignore {
  pub symbols: Vec<Box<Terminal>>,
}
impl Ignore {
  #[inline]
  pub fn new (
    symbols:Vec<Box<Terminal>>,
  ) -> Self {
    Self{
      symbols,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Ignore }
}
#[derive(Debug, Clone)]
pub struct AST_STRING {
  pub value: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_STRING {
  #[inline]
  pub fn new (
    value:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      value,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_STRING }
}
#[derive(Debug, Clone)]
pub struct AST_U32 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_U32 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_U32 }
}
#[derive(Debug, Clone)]
pub struct AST_ClassId {
  pub value: String,
  pub tok: Token,
}
impl AST_ClassId {
  #[inline]
  pub fn new (
    value:String,
    tok:Token,
  ) -> Self {
    Self{
      value,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_ClassId }
}
#[derive(Debug, Clone)]
pub struct AST_I64 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_I64 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_I64 }
}
#[derive(Debug, Clone)]
pub struct AST_U16 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_U16 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_U16 }
}
#[derive(Debug, Clone)]
pub struct ClassSymbol {
  pub val: String,
  pub tok: Token,
}
impl ClassSymbol {
  #[inline]
  pub fn new (
    val:String,
    tok:Token,
  ) -> Self {
    Self{
      val,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::ClassSymbol }
}
#[derive(Debug, Clone)]
pub struct AST_F64 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_F64 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_F64 }
}
#[derive(Debug, Clone)]
pub struct AST_Map {
  pub key: ASTNode,
  pub val: ASTNode,
  pub tok: Token,
}
impl AST_Map {
  #[inline]
  pub fn new (
    key:ASTNode,
    val:ASTNode,
    tok:Token,
  ) -> Self {
    Self{
      key,
      val,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Map }
}
#[derive(Debug, Clone)]
pub struct AnyGroup {
  pub symbols: Vec<ASTNode>,
  pub unordered: bool,
  pub tok: Token,
}
impl AnyGroup {
  #[inline]
  pub fn new (
    symbols:Vec<ASTNode>,
    unordered:bool,
    tok:Token,
  ) -> Self {
    Self{
      symbols,
      unordered,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AnyGroup }
}
#[derive(Debug, Clone)]
pub struct Range {
  pub end_trim: Vec<Token>,
  pub start_trim: Vec<Token>,
}
impl Range {
  #[inline]
  pub fn new (
    end_trim:Vec<Token>,
    start_trim:Vec<Token>,
  ) -> Self {
    Self{
      end_trim,
      start_trim,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Range }
}
#[derive(Debug, Clone)]
pub struct AST_I16 {
  pub initializer: Option<Box<Init>>,
  pub tok: Token,
}
impl AST_I16 {
  #[inline]
  pub fn new (
    initializer:Option<Box<Init>>,
    tok:Token,
  ) -> Self {
    Self{
      initializer,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_I16 }
}
#[derive(Debug, Clone)]
pub struct AST_NUMBER {
  pub value: f64,
}
impl AST_NUMBER {
  #[inline]
  pub fn new (
    value:f64,
  ) -> Self {
    Self{
      value,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_NUMBER }
}
#[derive(Debug, Clone)]
pub struct AST_Struct {
  pub props: Vec<ASTNode>,
  pub typ: Token,
  pub tok: Token,
}
impl AST_Struct {
  #[inline]
  pub fn new (
    props:Vec<ASTNode>,
    typ:Token,
    tok:Token,
  ) -> Self {
    Self{
      props,
      typ,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Struct }
}
#[derive(Debug, Clone)]
pub struct Rule {
  pub ast_definition: Option<Box<Ascript>>,
  pub is_priority: bool,
  pub symbols: Vec<ASTNode>,
  pub tok: Token,
}
impl Rule {
  #[inline]
  pub fn new (
    ast_definition:Option<Box<Ascript>>,
    is_priority:bool,
    symbols:Vec<ASTNode>,
    tok:Token,
  ) -> Self {
    Self{
      ast_definition,
      is_priority,
      symbols,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Rule }
}
#[derive(Debug, Clone)]
pub struct AnnotatedSymbol {
  pub is_optional: bool,
  pub prority: Option<Box<Priority>>,
  pub reference: String,
  pub symbol: ASTNode,
  pub tok: Token,
}
impl AnnotatedSymbol {
  #[inline]
  pub fn new (
    is_optional:bool,
    prority:Option<Box<Priority>>,
    reference:String,
    symbol:ASTNode,
    tok:Token,
  ) -> Self {
    Self{
      is_optional,
      prority,
      reference,
      symbol,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AnnotatedSymbol }
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
  pub fn new (
    id:String,
    named_reference:String,
    value:Option<ASTNode>,
    tok:Token,
  ) -> Self {
    Self{
      id,
      named_reference,
      value,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::AST_Property }
}
#[derive(Debug, Clone)]
pub struct Ascript {
  pub ast: ASTNode,
  pub tok: Token,
}
impl Ascript {
  #[inline]
  pub fn new (
    ast:ASTNode,
    tok:Token,
  ) -> Self {
    Self{
      ast,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Ascript }
}
#[derive(Debug, Clone)]
pub struct Production_Terminal_Symbol {
  pub production: ASTNode,
  pub tok: Token,
}
impl Production_Terminal_Symbol {
  #[inline]
  pub fn new (
    production:ASTNode,
    tok:Token,
  ) -> Self {
    Self{
      production,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Production_Terminal_Symbol }
}
#[derive(Debug, Clone)]
pub struct Terminal {
  pub val: String,
  pub tok: Token,
}
impl Terminal {
  #[inline]
  pub fn new (
    val:String,
    tok:Token,
  ) -> Self {
    Self{
      val,
      tok,
    }
  }
  pub fn get_type(&self) -> ASTNodeType { ASTNodeType::Terminal }
}

fn default_fn<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  if slots.len() > 1 {
    let AstSlot (_, rng, _) = slots.take(0);
    let last = slots.take(slots.len() - 1);
    for index in 1..slots.len()-1 {
      slots.take(index);
    }
    slots.assign(0, AstSlot (last.0, rng + last.1, TokenRange::default()));
  }
}

/*
vector_list_1 => vector_list_1 \, expression
*/
fn ast_fn000<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
vector_list_1 => expression
*/
fn ast_fn001<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
class => \c: \num
*/
fn ast_fn002<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => \c: \nl
*/
fn ast_fn003<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => \c: \sp
*/
fn ast_fn004<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => \c: \id
*/
fn ast_fn005<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => \c: \sym
*/
fn ast_fn006<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
class => \c: \any
*/
fn ast_fn007<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = ClassSymbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::ClassSymbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
ast_definition => \=> body
*/
fn ast_fn009<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_3_0 = Ascript::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Ascript(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
body => \{ body_list_1 \}
*/
fn ast_fn012<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = AST_Statements::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Statements(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
body => \{ \}
*/
fn ast_fn013<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_3_0 = AST_Statements::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Statements(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
any_group => \[ \unordered any_group_list_1 \]
*/
fn ast_fn014<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_2_0 = i2.into_nodes();
  let ref_5_1 = true;
  let ref_6_0 = AnyGroup::new(
    ref_2_0, ref_5_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnyGroup(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
any_group => \[ any_group_list_1 \]
*/
fn ast_fn015<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1.into_nodes();
  let ref_4_1 = false;
  let ref_5_0 = AnyGroup::new(
    ref_1_0, ref_4_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnyGroup(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
add => add \+ expression
*/
fn ast_fn016<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_0_0 = i0;
  let ref_2_1 = i2;
  let ref_4_0 = AST_Add::new(
    ref_0_0, ref_2_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Add(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
production_list_1 => production_list_1 \, template_name
*/
fn ast_fn018<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = i2.to_string();
  let mut ref_0_0 = i0.into_strings();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot (ASTNode::STRINGS(ref_0_0), rng, TokenRange::default()))
}

/*
production_list_1 => template_name
*/
fn ast_fn019<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0.to_string();
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::STRINGS(ref_2_0), rng, TokenRange::default()))
}

/*
priority => \{ tk:priority_num \}
*/
fn ast_fn020<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_1_1 = ref_1_1.to_u32();
  let ref_4_0 = Priority::new(
    false, ref_1_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Priority(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
priority => \!
*/
fn ast_fn021<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = true;
  let ref_3_0 = Priority::new(
    ref_2_0, 0u32, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Priority(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
grammar_list_2 => grammar_list_2 production
*/
fn ast_fn022<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar_list_2 => grammar_list_2 append_production
*/
fn ast_fn023<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar_list_2 => production
*/
fn ast_fn024<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
grammar_list_2 => append_production
*/
fn ast_fn025<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
non_terminal => non_terminal \< production_symbol \>
*/
fn ast_fn028<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_0_0 = i0;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Production_Symbol(obj) = ref_2_1
        { obj }
        else {panic!("invalid node")};
  let ref_5_0 = TemplateProductionSymbol::new(
    ref_0_0, ref_2_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::TemplateProductionSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
body_list_1 => body_list_1 \; expression
*/
fn ast_fn029<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
body_list_1 => expression
*/
fn ast_fn030<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
group => \( rules \)
*/
fn ast_fn031<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = Group_Production::new(
    ref_1_0.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Group_Production(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
vector => \[ vector_list_1 \]
*/
fn ast_fn032<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1.into_nodes();
  let ref_4_0 = AST_Vector::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Vector(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
vector => \[ \]
*/
fn ast_fn033<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_3_0 = AST_Vector::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Vector(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
production => \< production_list_1 \> \lazy priority non_terminal \> rules
*/
fn ast_fn041<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (i4, ..) = slots.take(4);
  let AstSlot (i5, rng5, _) = slots.take(5);
  slots.take(6);
  let AstSlot (i7, rng7, _) = slots.take(7);
  let rng = rng0 + rng7;
  
  let ref_9_1 = true;
  let ref_5_2 = rng5.to_token(_ctx_.get_reader()).to_string();
  let ref_5_3 = i5;
  let ref_4_4 = i4;
  let ref_4_4 = if let ASTNode::Priority(obj) = ref_4_4
        { obj }
        else {panic!("invalid node")};
  let ref_7_5 = i7.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_10_0 = Production::new(
    false, ref_9_1, ref_5_2, ref_5_3, Some(ref_4_4), ref_7_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), ref_1_6, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_10_0)), rng, TokenRange::default()))
}

/*
production => \< \> \lazy priority non_terminal \> rules
*/
fn ast_fn042<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot (i3, ..) = slots.take(3);
  let AstSlot (i4, rng4, _) = slots.take(4);
  slots.take(5);
  let AstSlot (i6, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;
  
  let ref_8_1 = true;
  let ref_4_2 = rng4.to_token(_ctx_.get_reader()).to_string();
  let ref_4_3 = i4;
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Priority(obj) = ref_3_4
        { obj }
        else {panic!("invalid node")};
  let ref_6_5 = i6.into_nodes();
  let ref_9_0 = Production::new(
    false, ref_8_1, ref_4_2, ref_4_3, Some(ref_3_4), ref_6_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
production => \< production_list_1 \> priority non_terminal \> rules
*/
fn ast_fn043<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, ..) = slots.take(3);
  let AstSlot (i4, rng4, _) = slots.take(4);
  slots.take(5);
  let AstSlot (i6, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;
  
  let ref_8_1 = false;
  let ref_4_2 = rng4.to_token(_ctx_.get_reader()).to_string();
  let ref_4_3 = i4;
  let ref_3_4 = i3;
  let ref_3_4 = if let ASTNode::Priority(obj) = ref_3_4
        { obj }
        else {panic!("invalid node")};
  let ref_6_5 = i6.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_9_0 = Production::new(
    false, ref_8_1, ref_4_2, ref_4_3, Some(ref_3_4), ref_6_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), ref_1_6, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
production => \< \> priority non_terminal \> rules
*/
fn ast_fn044<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  slots.take(4);
  let AstSlot (i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;
  
  let ref_7_1 = false;
  let ref_3_2 = rng3.to_token(_ctx_.get_reader()).to_string();
  let ref_3_3 = i3;
  let ref_2_4 = i2;
  let ref_2_4 = if let ASTNode::Priority(obj) = ref_2_4
        { obj }
        else {panic!("invalid node")};
  let ref_5_5 = i5.into_nodes();
  let ref_8_0 = Production::new(
    false, ref_7_1, ref_3_2, ref_3_3, Some(ref_2_4), ref_5_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production => \< production_list_1 \> \lazy non_terminal \> rules
*/
fn ast_fn045<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  slots.take(3);
  let AstSlot (i4, rng4, _) = slots.take(4);
  slots.take(5);
  let AstSlot (i6, rng6, _) = slots.take(6);
  let rng = rng0 + rng6;
  
  let ref_8_1 = true;
  let ref_4_2 = rng4.to_token(_ctx_.get_reader()).to_string();
  let ref_4_3 = i4;
  let ref_6_5 = i6.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_9_0 = Production::new(
    false, ref_8_1, ref_4_2, ref_4_3, Default::default(), ref_6_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), ref_1_6, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_9_0)), rng, TokenRange::default()))
}

/*
production => \< \> \lazy non_terminal \> rules
*/
fn ast_fn046<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  slots.take(4);
  let AstSlot (i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;
  
  let ref_7_1 = true;
  let ref_3_2 = rng3.to_token(_ctx_.get_reader()).to_string();
  let ref_3_3 = i3;
  let ref_5_5 = i5.into_nodes();
  let ref_8_0 = Production::new(
    false, ref_7_1, ref_3_2, ref_3_3, Default::default(), ref_5_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production => \< production_list_1 \> non_terminal \> rules
*/
fn ast_fn047<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  slots.take(4);
  let AstSlot (i5, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;
  
  let ref_7_1 = false;
  let ref_3_2 = rng3.to_token(_ctx_.get_reader()).to_string();
  let ref_3_3 = i3;
  let ref_5_5 = i5.into_nodes();
  let ref_1_6 = i1.into_strings();
  let ref_8_0 = Production::new(
    false, ref_7_1, ref_3_2, ref_3_3, Default::default(), ref_5_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), ref_1_6, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
production => \< \> non_terminal \> rules
*/
fn ast_fn048<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  slots.take(3);
  let AstSlot (i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;
  
  let ref_6_1 = false;
  let ref_2_2 = rng2.to_token(_ctx_.get_reader()).to_string();
  let ref_2_3 = i2;
  let ref_4_5 = i4.into_nodes();
  let ref_7_0 = Production::new(
    false, ref_6_1, ref_2_2, ref_2_3, Default::default(), ref_4_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:id
*/
fn ast_fn049<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:sym
*/
fn ast_fn050<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:num
*/
fn ast_fn051<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 g:sp
*/
fn ast_fn052<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => terminal_list_2 escaped
*/
fn ast_fn053<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:id
*/
fn ast_fn054<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:sym
*/
fn ast_fn055<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:num
*/
fn ast_fn056<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => g:sp
*/
fn ast_fn057<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_2 => escaped
*/
fn ast_fn058<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
string_convert => \str convert_initializer
*/
fn ast_fn059<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_STRING::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_STRING(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
string_convert => \str
*/
fn ast_fn060<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_STRING::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_STRING(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
terminal_non_terminal => \tk: non_terminal
*/
fn ast_fn062<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_3_0 = Production_Terminal_Symbol::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production_Terminal_Symbol(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
list => symbol \(+ terminal \)
*/
fn ast_fn063<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_0_1 = i0;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Terminal(obj) = ref_2_2
        { obj }
        else {panic!("invalid node")};
  let ref_5_0 = List_Production::new(
    false, ref_0_1, Some(ref_2_2), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::List_Production(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
list => symbol \(+ \)
*/
fn ast_fn064<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_0_1 = i0;
  let ref_4_0 = List_Production::new(
    false, ref_0_1, Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::List_Production(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
list => symbol \(* terminal \)
*/
fn ast_fn065<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_0_1 = i0;
  let ref_2_2 = i2;
  let ref_2_2 = if let ASTNode::Terminal(obj) = ref_2_2
        { obj }
        else {panic!("invalid node")};
  let ref_6_0 = List_Production::new(
    ref_5_0, ref_0_1, Some(ref_2_2), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::List_Production(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
list => symbol \(* \)
*/
fn ast_fn066<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = true;
  let ref_0_1 = i0;
  let ref_5_0 = List_Production::new(
    ref_4_0, ref_0_1, Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::List_Production(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u8 convert_initializer
*/
fn ast_fn067<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_U8::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U8(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u8
*/
fn ast_fn068<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_U8::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U8(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u16 convert_initializer
*/
fn ast_fn069<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_U16::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U16(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u16
*/
fn ast_fn070<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_U16::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U16(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u32 convert_initializer
*/
fn ast_fn071<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_U32::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U32(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u32
*/
fn ast_fn072<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_U32::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U32(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u64 convert_initializer
*/
fn ast_fn073<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_U64::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U64(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \u64
*/
fn ast_fn074<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_U64::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_U64(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i8 convert_initializer
*/
fn ast_fn075<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_I8::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I8(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i8
*/
fn ast_fn076<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_I8::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I8(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i16 convert_initializer
*/
fn ast_fn077<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_I16::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I16(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i16
*/
fn ast_fn078<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_I16::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I16(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i32 convert_initializer
*/
fn ast_fn079<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_I32::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I32(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i32
*/
fn ast_fn080<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_I32::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I32(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i64 convert_initializer
*/
fn ast_fn081<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_I64::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I64(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \i64
*/
fn ast_fn082<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_I64::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_I64(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \f32 convert_initializer
*/
fn ast_fn083<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_F32::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_F32(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \f32
*/
fn ast_fn084<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_F32::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_F32(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \f64 convert_initializer
*/
fn ast_fn085<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_F64::new(
    Some(ref_1_0), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_F64(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
numeric_convert => \f64
*/
fn ast_fn086<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_F64::new(
    Default::default(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_F64(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
grammar => grammar_list_1 grammar_list_2
*/
fn ast_fn087<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_0_0 = i0.into_nodes();
  let ref_1_1 = i1.into_nodes();
  let ref_3_0 = Grammar::new(
    ref_0_0, ref_1_1.into_iter().map(|v|match v { ASTNode::Production(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Grammar(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
grammar => grammar_list_2
*/
fn ast_fn088<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_1 = i0.into_nodes();
  let ref_2_0 = Grammar::new(
    Default::default(), ref_0_1.into_iter().map(|v|match v { ASTNode::Production(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Grammar(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
integer_list_1 => integer_list_1 g:num
*/
fn ast_fn089<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
integer_list_1 => g:num
*/
fn ast_fn090<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => import_clause_list_1 g:id
*/
fn ast_fn091<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => import_clause_list_1 g:sym
*/
fn ast_fn092<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => g:id
*/
fn ast_fn093<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
import_clause_list_1 => g:sym
*/
fn ast_fn094<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
ignore_clause => \IGNORE \{ ignore_clause_list_1 \}
*/
fn ast_fn095<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_2_0 = i2.into_nodes();
  let ref_5_0 = Ignore::new(
    ref_2_0.into_iter().map(|v|match v { ASTNode::Terminal(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Ignore(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
token => \tok range
*/
fn ast_fn096<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Range(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_Token::new(
    Some(ref_1_0), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Token(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
token => \token range
*/
fn ast_fn097<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Range(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_Token::new(
    Some(ref_1_0), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Token(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
token => \tok
*/
fn ast_fn098<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_Token::new(
    Default::default(), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Token(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
token => \token
*/
fn ast_fn099<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_Token::new(
    Default::default(), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Token(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
convert_initializer => \( init_objects \)
*/
fn ast_fn100<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1;
  let ref_4_0 = Init::new(
    ref_1_0, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Init(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule_list_1 => rule_list_1 annotated_symbol
*/
fn ast_fn101<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
rule_list_1 => rule_list_1 any_group
*/
fn ast_fn102<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
rule_list_1 => annotated_symbol
*/
fn ast_fn103<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
rule_list_1 => any_group
*/
fn ast_fn104<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
bool_convert => \bool convert_initializer
*/
fn ast_fn105<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Init(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_0 = AST_BOOL::new(
    Some(ref_1_0), false, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_BOOL(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
bool_convert => \bool
*/
fn ast_fn106<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = AST_BOOL::new(
    Default::default(), false, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_BOOL(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
import_clause => \IMPORT import_clause_list_1 \AS identifier
*/
fn ast_fn107<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_3_0 = rng3.to_token(_ctx_.get_reader());
  let ref_3_0 = ref_3_0.to_string();
  let ref_1_1 = i1.into_tokens();
  let ref_1_1 = (ref_1_1.first().unwrap() + ref_1_1.last().unwrap()).to_string();
  let ref_5_0 = Import::new(
    ref_3_0, ref_1_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Import(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
import_clause => \IMPORT import_clause_list_1 \as identifier
*/
fn ast_fn108<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_3_0 = rng3.to_token(_ctx_.get_reader());
  let ref_3_0 = ref_3_0.to_string();
  let ref_1_1 = i1.into_tokens();
  let ref_1_1 = (ref_1_1.first().unwrap() + ref_1_1.last().unwrap()).to_string();
  let ref_5_0 = Import::new(
    ref_3_0, ref_1_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Import(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
map => \map \( expression \, expression \)
*/
fn ast_fn109<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  slots.take(3);
  let AstSlot (i4, ..) = slots.take(4);
  let AstSlot (_, rng5, _) = slots.take(5);
  let rng = rng0 + rng5;
  
  let ref_2_0 = i2;
  let ref_4_1 = i4;
  let ref_7_0 = AST_Map::new(
    ref_2_0, ref_4_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Map(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
struct_prop => identifier \: expression
*/
fn ast_fn110<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_2 = i2;
  let ref_4_0 = AST_Property::new(
    ref_0_0, String::new(), Some(ref_2_2), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Property(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
struct_prop => identifier \: struct
*/
fn ast_fn111<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_2 = i2;
  let ref_4_0 = AST_Property::new(
    ref_0_0, String::new(), Some(ref_2_2), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Property(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
struct_prop => identifier
*/
fn ast_fn112<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_0_1 = rng0.to_token(_ctx_.get_reader());
  let ref_0_1 = ref_0_1.to_string();
  let ref_2_0 = AST_Property::new(
    ref_0_0, ref_0_1, None, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Property(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
struct_prop => class_identifier
*/
fn ast_fn113<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_0 = AST_ClassId::new(
    ref_0_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_ClassId(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
any_group_list_1 => any_group_list_1 annotated_symbol
*/
fn ast_fn115<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
any_group_list_1 => annotated_symbol
*/
fn ast_fn116<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
literal => \true
*/
fn ast_fn117<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_1 = true;
  let ref_3_0 = AST_BOOL::new(
    None, ref_2_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_BOOL(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
literal => \false
*/
fn ast_fn118<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_1 = false;
  let ref_3_0 = AST_BOOL::new(
    None, ref_2_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_BOOL(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
literal => tk:integer
*/
fn ast_fn119<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_f64();
  let ref_2_0 = AST_NUMBER::new(
    ref_0_0, 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_NUMBER(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol tk:reference \? priority
*/
fn ast_fn120<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_3_1 = i3;
  let ref_3_1 = if let ASTNode::Priority(obj) = ref_3_1
        { obj }
        else {panic!("invalid node")};
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0, Some(ref_3_1), ref_1_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol \? priority
*/
fn ast_fn121<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = true;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1
        { obj }
        else {panic!("invalid node")};
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0, Some(ref_2_1), Default::default(), ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol tk:reference priority
*/
fn ast_fn122<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = false;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1
        { obj }
        else {panic!("invalid node")};
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0, Some(ref_2_1), ref_1_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol priority
*/
fn ast_fn123<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_3_0 = false;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1
        { obj }
        else {panic!("invalid node")};
  let ref_0_3 = i0;
  let ref_4_0 = AnnotatedSymbol::new(
    ref_3_0, Some(ref_1_1), Default::default(), ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol tk:reference \?
*/
fn ast_fn124<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = true;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0, Default::default(), ref_1_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol \?
*/
fn ast_fn125<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_3_0 = true;
  let ref_0_3 = i0;
  let ref_4_0 = AnnotatedSymbol::new(
    ref_3_0, Default::default(), Default::default(), ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol tk:reference
*/
fn ast_fn126<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_3_0 = false;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_4_0 = AnnotatedSymbol::new(
    ref_3_0, Default::default(), ref_1_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol tk:reference priority \?
*/
fn ast_fn127<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1
        { obj }
        else {panic!("invalid node")};
  let ref_1_2 = rng1.to_token(_ctx_.get_reader());
  let ref_1_2 = ref_1_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0, Some(ref_2_1), ref_1_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol priority \?
*/
fn ast_fn128<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = true;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1
        { obj }
        else {panic!("invalid node")};
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0, Some(ref_1_1), Default::default(), ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol \? tk:reference priority
*/
fn ast_fn129<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_3_1 = i3;
  let ref_3_1 = if let ASTNode::Priority(obj) = ref_3_1
        { obj }
        else {panic!("invalid node")};
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0, Some(ref_3_1), ref_2_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol \? tk:reference
*/
fn ast_fn130<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = true;
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0, Default::default(), ref_2_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol \? priority tk:reference
*/
fn ast_fn131<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, ..) = slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_2_1 = i2;
  let ref_2_1 = if let ASTNode::Priority(obj) = ref_2_1
        { obj }
        else {panic!("invalid node")};
  let ref_3_2 = rng3.to_token(_ctx_.get_reader());
  let ref_3_2 = ref_3_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0, Some(ref_2_1), ref_3_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol priority tk:reference
*/
fn ast_fn132<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = false;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1
        { obj }
        else {panic!("invalid node")};
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_5_0 = AnnotatedSymbol::new(
    ref_4_0, Some(ref_1_1), ref_2_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol priority tk:reference \?
*/
fn ast_fn133<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let AstSlot (_, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1
        { obj }
        else {panic!("invalid node")};
  let ref_2_2 = rng2.to_token(_ctx_.get_reader());
  let ref_2_2 = ref_2_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0, Some(ref_1_1), ref_2_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
annotated_symbol => symbol priority \? tk:reference
*/
fn ast_fn134<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_1_1 = i1;
  let ref_1_1 = if let ASTNode::Priority(obj) = ref_1_1
        { obj }
        else {panic!("invalid node")};
  let ref_3_2 = rng3.to_token(_ctx_.get_reader());
  let ref_3_2 = ref_3_2.to_string();
  let ref_0_3 = i0;
  let ref_6_0 = AnnotatedSymbol::new(
    ref_5_0, Some(ref_1_1), ref_3_2, ref_0_3, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AnnotatedSymbol(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
template_name => identifier
*/
fn ast_fn137<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_0 = rng.to_token(_ctx_.get_reader());
  let ref_2_0 = ref_2_0.to_string();
  slots.assign(0, AstSlot (ASTNode::STRING(ref_2_0), rng, TokenRange::default()))
}

/*
rules_list_1 => rules_list_1 \| rule
*/
fn ast_fn139<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
rules_list_1 => rule
*/
fn ast_fn140<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
import_production_symbol => tk:identifier_syms \:: tk:identifier_syms
*/
fn ast_fn149<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_1 = rng2.to_token(_ctx_.get_reader());
  let ref_2_1 = ref_2_1.to_string();
  let ref_4_0 = Production_Import_Symbol::new(
    ref_0_0, ref_2_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production_Import_Symbol(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => \! rule_list_1 ast_definition
*/
fn ast_fn150<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = i2;
  let ref_2_0 = if let ASTNode::Ascript(obj) = ref_2_0
        { obj }
        else {panic!("invalid node")};
  let ref_4_1 = true;
  let ref_1_2 = i1.into_nodes();
  let ref_5_0 = Rule::new(
    Some(ref_2_0), ref_4_1, ref_1_2, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Rule(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1 ast_definition
*/
fn ast_fn151<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_1_0 = if let ASTNode::Ascript(obj) = ref_1_0
        { obj }
        else {panic!("invalid node")};
  let ref_3_1 = false;
  let ref_0_2 = i0.into_nodes();
  let ref_4_0 = Rule::new(
    Some(ref_1_0), ref_3_1, ref_0_2, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Rule(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => \! rule_list_1
*/
fn ast_fn152<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_3_1 = true;
  let ref_1_2 = i1.into_nodes();
  let ref_4_0 = Rule::new(
    Default::default(), ref_3_1, ref_1_2, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Rule(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
rule => rule_list_1
*/
fn ast_fn153<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_2_1 = false;
  let ref_0_2 = i0.into_nodes();
  let ref_3_0 = Rule::new(
    Default::default(), ref_2_1, ref_0_2, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Rule(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
member => reference \. identifier
*/
fn ast_fn155<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = rng2.to_token(_ctx_.get_reader());
  let ref_0_1 = i0;
  let ref_4_0 = AST_Member::new(
    ref_2_0, ref_0_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Member(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
terminal_list_1 => terminal_list_1 g:sym
*/
fn ast_fn156<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_1 => terminal_list_1 g:num
*/
fn ast_fn157<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_1 => g:sym
*/
fn ast_fn158<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_1 => g:num
*/
fn ast_fn159<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
name_clause => \NAME identifier
*/
fn ast_fn160<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = Name::new(
    ref_1_0, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Name(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
append_production => \+> priority non_terminal \> rules
*/
fn ast_fn161<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  slots.take(3);
  let AstSlot (i4, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;
  
  let ref_6_0 = true;
  let ref_7_1 = false;
  let ref_2_2 = rng2.to_token(_ctx_.get_reader()).to_string();
  let ref_2_3 = i2;
  let ref_1_4 = i1;
  let ref_1_4 = if let ASTNode::Priority(obj) = ref_1_4
        { obj }
        else {panic!("invalid node")};
  let ref_4_5 = i4.into_nodes();
  let ref_8_0 = Production::new(
    ref_6_0, ref_7_1, ref_2_2, ref_2_3, Some(ref_1_4), ref_4_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Vec::new(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_8_0)), rng, TokenRange::default()))
}

/*
append_production => priority non_terminal \> rules
*/
fn ast_fn162<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_6_1 = false;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader()).to_string();
  let ref_1_3 = i1;
  let ref_0_4 = i0;
  let ref_0_4 = if let ASTNode::Priority(obj) = ref_0_4
        { obj }
        else {panic!("invalid node")};
  let ref_3_5 = i3.into_nodes();
  let ref_7_0 = Production::new(
    ref_5_0, ref_6_1, ref_1_2, ref_1_3, Some(ref_0_4), ref_3_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Vec::new(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
append_production => \+> non_terminal \> rules
*/
fn ast_fn163<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_5_0 = true;
  let ref_6_1 = false;
  let ref_1_2 = rng1.to_token(_ctx_.get_reader()).to_string();
  let ref_1_3 = i1;
  let ref_3_5 = i3.into_nodes();
  let ref_7_0 = Production::new(
    ref_5_0, ref_6_1, ref_1_2, ref_1_3, Default::default(), ref_3_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Vec::new(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_7_0)), rng, TokenRange::default()))
}

/*
append_production => non_terminal \> rules
*/
fn ast_fn164<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_4_0 = true;
  let ref_5_1 = false;
  let ref_0_2 = rng0.to_token(_ctx_.get_reader()).to_string();
  let ref_0_3 = i0;
  let ref_2_5 = i2.into_nodes();
  let ref_6_0 = Production::new(
    ref_4_0, ref_5_1, ref_0_2, ref_0_3, Default::default(), ref_2_5.into_iter().map(|v|match v { ASTNode::Rule(node) => node, _ => panic!("could not convert")}).collect::<Vec<_>>(), Vec::new(), rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:id
*/
fn ast_fn165<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:sym
*/
fn ast_fn166<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:num
*/
fn ast_fn167<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 g:sp
*/
fn ast_fn168<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => terminal_list_3 escaped
*/
fn ast_fn169<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let mut ref_0_0 = i0.into_tokens();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_0_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:id
*/
fn ast_fn170<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:sym
*/
fn ast_fn171<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:num
*/
fn ast_fn172<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => g:sp
*/
fn ast_fn173<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal_list_3 => escaped
*/
fn ast_fn174<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::TOKENS(ref_2_0), rng, TokenRange::default()))
}

/*
terminal => terminal_list_1 g:sp
*/
fn ast_fn179<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (_, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_0_0 = i0.into_tokens();
  let ref_0_0 = (ref_0_0.first().unwrap() + ref_0_0.last().unwrap()).to_string();
  let ref_3_0 = Terminal::new(
    ref_0_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Terminal(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
terminal => terminal_list_1
*/
fn ast_fn180<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0.into_tokens();
  let ref_0_0 = (ref_0_0.first().unwrap() + ref_0_0.last().unwrap()).to_string();
  let ref_2_0 = Terminal::new(
    ref_0_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Terminal(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
terminal => \" terminal_list_2 \"
*/
fn ast_fn181<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1.into_tokens();
  let ref_1_0 = (ref_1_0.first().unwrap() + ref_1_0.last().unwrap()).to_string();
  let ref_4_0 = Terminal::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Terminal(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
terminal => \' terminal_list_3 \'
*/
fn ast_fn182<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_0 = i1.into_tokens();
  let ref_1_0 = (ref_1_0.first().unwrap() + ref_1_0.last().unwrap()).to_string();
  let ref_4_0 = Terminal::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Terminal(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
grammar_list_1 => grammar_list_1 preamble
*/
fn ast_fn183<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
grammar_list_1 => preamble
*/
fn ast_fn184<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
production_symbol => tk:identifier_syms
*/
fn ast_fn185<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = rng0.to_token(_ctx_.get_reader());
  let ref_0_0 = ref_0_0.to_string();
  let ref_2_0 = Production_Symbol::new(
    ref_0_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Production_Symbol(Box::new(ref_2_0)), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => ignore_clause_list_1 terminal
*/
fn ast_fn186<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_1_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
ignore_clause_list_1 => terminal
*/
fn ast_fn187<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
struct_list_1 => struct_list_1 \, struct_prop
*/
fn ast_fn193<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  slots.take(1);
  let AstSlot (i2, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_2_0 = i2;
  let mut ref_0_0 = i0.into_nodes();
  ref_0_0.push(ref_2_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_0_0), rng, TokenRange::default()))
}

/*
struct_list_1 => struct_prop
*/
fn ast_fn194<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (i0, rng0, _) = slots.take(0);
  let rng = rng0;
  
  let ref_0_0 = i0;
  let mut ref_2_0 = vec![];
  ref_2_0.push(ref_0_0);
  slots.assign(0, AstSlot (ASTNode::NODES(ref_2_0), rng, TokenRange::default()))
}

/*
range => \< integer \, integer \>
*/
fn ast_fn195<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, ..) = slots.take(3);
  let AstSlot (_, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;
  
  let ref_3_0 = i3.into_tokens();
  let ref_1_1 = i1.into_tokens();
  let ref_6_0 = Range::new(
    ref_3_0, ref_1_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Range(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
range => \< integer \>
*/
fn ast_fn196<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_1 = i1.into_tokens();
  let ref_4_0 = Range::new(
    Default::default(), ref_1_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Range(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
struct => \{ type_identifier \, struct_list_1 \}
*/
fn ast_fn197<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, ..) = slots.take(3);
  let AstSlot (_, rng4, _) = slots.take(4);
  let rng = rng0 + rng4;
  
  let ref_3_0 = i3.into_nodes();
  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_6_0 = AST_Struct::new(
    ref_3_0, ref_1_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Struct(Box::new(ref_6_0)), rng, TokenRange::default()))
}

/*
struct => \{ type_identifier \}
*/
fn ast_fn198<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let AstSlot (_, rng2, _) = slots.take(2);
  let rng = rng0 + rng2;
  
  let ref_1_1 = rng1.to_token(_ctx_.get_reader());
  let ref_4_0 = AST_Struct::new(
    Default::default(), ref_1_1, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_Struct(Box::new(ref_4_0)), rng, TokenRange::default()))
}

/*
reference => \$ tk:identifier
*/
fn ast_fn199<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_string();
  let ref_3_0 = AST_NamedReference::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_NamedReference(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
reference => \$ tk:integer
*/
fn ast_fn200<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = rng1.to_token(_ctx_.get_reader());
  let ref_1_0 = ref_1_0.to_i64();
  let ref_3_0 = AST_IndexReference::new(
    ref_1_0, rng.to_token(_ctx_.get_reader()), 
  
  );
  slots.assign(0, AstSlot (ASTNode::AST_IndexReference(Box::new(ref_3_0)), rng, TokenRange::default()))
}

/*
export_clause => \EXPORT non_terminal \AS identifier
*/
fn ast_fn203<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_1_0 = i1;
  let ref_3_1 = rng3.to_token(_ctx_.get_reader());
  let ref_5_0 = Export::new(
    ref_1_0, ref_3_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Export(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
export_clause => \EXPORT non_terminal \as identifier
*/
fn ast_fn204<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, ..) = slots.take(1);
  slots.take(2);
  let AstSlot (i3, rng3, _) = slots.take(3);
  let rng = rng0 + rng3;
  
  let ref_1_0 = i1;
  let ref_3_1 = rng3.to_token(_ctx_.get_reader());
  let ref_5_0 = Export::new(
    ref_1_0, ref_3_1, 
  
  );
  slots.assign(0, AstSlot (ASTNode::Export(Box::new(ref_5_0)), rng, TokenRange::default()))
}

/*
export_clause => \EXPORT non_terminal
*/
fn ast_fn205<R: Reader + UTF8Reader, M>(_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<ASTNode>>){
  let AstSlot (_, rng0, _) = slots.take(0);
  let AstSlot (i1, rng1, _) = slots.take(1);
  let rng = rng0 + rng1;
  
  let ref_1_0 = i1;
  let ref_3_0 = Export::new(
    ref_1_0, Default::default(), 
  
  );
  slots.assign(0, AstSlot (ASTNode::Export(Box::new(ref_3_0)), rng, TokenRange::default()))
}

struct ReduceFunctions<R: Reader + UTF8Reader, M>(pub [Reducer<R, M, ASTNode>; 206]);
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
/* 8 1 */ default_fn::<R, M>,
/* 9 */ ast_fn009::<R, M>,
/* 10 1 */ default_fn::<R, M>,
/* 11 1 */ default_fn::<R, M>,
/* 12 */ ast_fn012::<R, M>,
/* 13 */ ast_fn013::<R, M>,
/* 14 */ ast_fn014::<R, M>,
/* 15 */ ast_fn015::<R, M>,
/* 16 */ ast_fn016::<R, M>,
/* 17 1 */ default_fn::<R, M>,
/* 18 */ ast_fn018::<R, M>,
/* 19 */ ast_fn019::<R, M>,
/* 20 */ ast_fn020::<R, M>,
/* 21 */ ast_fn021::<R, M>,
/* 22 */ ast_fn022::<R, M>,
/* 23 */ ast_fn023::<R, M>,
/* 24 */ ast_fn024::<R, M>,
/* 25 */ ast_fn025::<R, M>,
/* 26 1 */ default_fn::<R, M>,
/* 27 1 */ default_fn::<R, M>,
/* 28 */ ast_fn028::<R, M>,
/* 29 */ ast_fn029::<R, M>,
/* 30 */ ast_fn030::<R, M>,
/* 31 */ ast_fn031::<R, M>,
/* 32 */ ast_fn032::<R, M>,
/* 33 */ ast_fn033::<R, M>,
/* 34 1 */ default_fn::<R, M>,
/* 35 1 */ default_fn::<R, M>,
/* 36 1 */ default_fn::<R, M>,
/* 37 1 */ default_fn::<R, M>,
/* 38 1 */ default_fn::<R, M>,
/* 39 1 */ default_fn::<R, M>,
/* 40 1 */ default_fn::<R, M>,
/* 41 */ ast_fn041::<R, M>,
/* 42 */ ast_fn042::<R, M>,
/* 43 */ ast_fn043::<R, M>,
/* 44 */ ast_fn044::<R, M>,
/* 45 */ ast_fn045::<R, M>,
/* 46 */ ast_fn046::<R, M>,
/* 47 */ ast_fn047::<R, M>,
/* 48 */ ast_fn048::<R, M>,
/* 49 */ ast_fn049::<R, M>,
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
/* 61 1 */ default_fn::<R, M>,
/* 62 */ ast_fn062::<R, M>,
/* 63 */ ast_fn063::<R, M>,
/* 64 */ ast_fn064::<R, M>,
/* 65 */ ast_fn065::<R, M>,
/* 66 */ ast_fn066::<R, M>,
/* 67 */ ast_fn067::<R, M>,
/* 68 */ ast_fn068::<R, M>,
/* 69 */ ast_fn069::<R, M>,
/* 70 */ ast_fn070::<R, M>,
/* 71 */ ast_fn071::<R, M>,
/* 72 */ ast_fn072::<R, M>,
/* 73 */ ast_fn073::<R, M>,
/* 74 */ ast_fn074::<R, M>,
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
/* 85 */ ast_fn085::<R, M>,
/* 86 */ ast_fn086::<R, M>,
/* 87 */ ast_fn087::<R, M>,
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
/* 114 1 */ default_fn::<R, M>,
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
/* 135 1 */ default_fn::<R, M>,
/* 136 1 */ default_fn::<R, M>,
/* 137 */ ast_fn137::<R, M>,
/* 138 1 */ default_fn::<R, M>,
/* 139 */ ast_fn139::<R, M>,
/* 140 */ ast_fn140::<R, M>,
/* 141 1 */ default_fn::<R, M>,
/* 142 1 */ default_fn::<R, M>,
/* 143 1 */ default_fn::<R, M>,
/* 144 1 */ default_fn::<R, M>,
/* 145 1 */ default_fn::<R, M>,
/* 146 1 */ default_fn::<R, M>,
/* 147 1 */ default_fn::<R, M>,
/* 148 1 */ default_fn::<R, M>,
/* 149 */ ast_fn149::<R, M>,
/* 150 */ ast_fn150::<R, M>,
/* 151 */ ast_fn151::<R, M>,
/* 152 */ ast_fn152::<R, M>,
/* 153 */ ast_fn153::<R, M>,
/* 154 1 */ default_fn::<R, M>,
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
/* 169 */ ast_fn169::<R, M>,
/* 170 */ ast_fn170::<R, M>,
/* 171 */ ast_fn171::<R, M>,
/* 172 */ ast_fn172::<R, M>,
/* 173 */ ast_fn173::<R, M>,
/* 174 */ ast_fn174::<R, M>,
/* 175 1 */ default_fn::<R, M>,
/* 176 1 */ default_fn::<R, M>,
/* 177 1 */ default_fn::<R, M>,
/* 178 1 */ default_fn::<R, M>,
/* 179 */ ast_fn179::<R, M>,
/* 180 */ ast_fn180::<R, M>,
/* 181 */ ast_fn181::<R, M>,
/* 182 */ ast_fn182::<R, M>,
/* 183 */ ast_fn183::<R, M>,
/* 184 */ ast_fn184::<R, M>,
/* 185 */ ast_fn185::<R, M>,
/* 186 */ ast_fn186::<R, M>,
/* 187 */ ast_fn187::<R, M>,
/* 188 1 */ default_fn::<R, M>,
/* 189 1 */ default_fn::<R, M>,
/* 190 1 */ default_fn::<R, M>,
/* 191 1 */ default_fn::<R, M>,
/* 192 1 */ default_fn::<R, M>,
/* 193 */ ast_fn193::<R, M>,
/* 194 */ ast_fn194::<R, M>,
/* 195 */ ast_fn195::<R, M>,
/* 196 */ ast_fn196::<R, M>,
/* 197 */ ast_fn197::<R, M>,
/* 198 */ ast_fn198::<R, M>,
/* 199 */ ast_fn199::<R, M>,
/* 200 */ ast_fn200::<R, M>,
/* 201 1 */ default_fn::<R, M>,
/* 202 1 */ default_fn::<R, M>,
/* 203 */ ast_fn203::<R, M>,
/* 204 */ ast_fn204::<R, M>,
/* 205 */ ast_fn205::<R, M>
    ])
  }
}




#[link(name = "sherpa", kind = "static" )]
extern "C" {
    fn init(ctx: *mut u8, reader: *mut u8);
    fn next(ctx: *mut u8) -> ParseActionType;
    fn prime(ctx: *mut u8, start_point: u32);
    fn drop(ctx: *mut u8);
}

pub trait Reader:
  ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug
  {}

impl<T> Reader for T
  where T: ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug 
  {}
      
pub struct Parser<T: Reader, M>(ParseContext<T, M>, T);


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


impl<T: Reader, M> Parser<T, M> {
    /// Create a new parser context to parser the input with 
    /// the grammar `sherpa`
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
      unsafe { drop(_ptr as *mut u8); }
    }
  
  /// `<> grammar > 
  // 
  //         preamble(*) ( production | append_production )(+)
  // 
  //             f:ast {{ t_Grammar, c_Version_1_0, preamble:$1, productions:$2, tok }}`
  pub fn new_grammar_parser(reader: T) -> Self{
    let mut ctx = Self::new(reader);
    ctx.set_start_point(0);
    ctx
  }
  
}

impl<T: Reader, M> Drop for Parser<T, M> {
    fn drop(&mut self) {
        self.destroy_context();
    }
}
pub mod ast  {
  use super::*; 
  
  impl AstObject for ASTNode {}
  type ASTSlot = (ASTNode, TokenRange, TokenRange);
  
  #[link(name = "sherpa", kind = "static" )]
  extern "C" {
    fn ast_parse(
      ctx: *mut u8,
      reducers: *const u8,
      shift_handler: *const u8,
      result_handler: *const u8,
    ) -> ParseResult<ASTNode>;
  }
  
  
  pub fn grammar_from(reader: UTF8StringReader)  -> Result<Box<Grammar>, SherpaParseError> { 
    
    const reduce_functions: ReduceFunctions::<UTF8StringReader, u32> = ReduceFunctions::<UTF8StringReader, u32>::new();
    
    let mut ctx = Parser::new_grammar_parser(reader);
    let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
    let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, ASTNode> as *const u8;
    let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;
    
    match unsafe{ ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {
      ParseResult::Complete((i0, _, _))  => {
        
        let ref_0_0 = i0;
        let ref_0_0 = if let ASTNode::Grammar(obj) = ref_0_0
        { obj }
        else {panic!("invalid node")};
        Ok(ref_0_0)
      }
      ParseResult::Error(..) => Err(SherpaParseError {
                  inline_message: Default::default(),
                  last_production: 0,
                  loc: Default::default(),
                  message: Default::default(),
                }),
      _ => unreachable!()
    }
  }
}