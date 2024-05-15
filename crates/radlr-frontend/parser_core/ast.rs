#![allow(unused)]
#![cfg_attr(rustfmt, rustfmt_skip)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.0-beta2
/// - **SOURCE**: UNDEFINED
///
/// #### WARNING:
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### License:
/// Copyright (c) 2020-2024 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the 'Software'), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE

use radlr_rust_runtime::parsers::ast::{Tk, Reducer, Node};
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
#[repr(C,u32)]
pub enum ASTNode<Token:Tk> {
  #[default]
  None, 
  Token(Token), 
  U32(u32), 
  U64(u64), 
  I32(i32), 
  I64(i64), 
  F64(f64), 
  String(String), 
  Bool(bool), 
  VecU64(Vec<u64>), 
  VecRule(Vec<Box<Rule<Token>>>), 
  VecPush(Vec<Box<Push<Token>>>), 
  VecGoto(Vec<Box<Goto<Token>>>), 
  VecIntMatch(Vec<Box<IntMatch>>), 
  VecNonTermMatch(Vec<Box<NonTermMatch<Token>>>), 
  VecNontermDeclaration(Vec<Box<NontermDeclaration<Token>>>), 
  VecAnnotatedSymbol(Vec<Box<AnnotatedSymbol<Token>>>), 
  VecEOFSymbol(Vec<Box<EOFSymbol<Token>>>), 
  VecTermMatch(Vec<Box<TermMatch<Token>>>), 
  /*0*/ 
  VecNon_branch_statementValues(/*0*/Vec<Non_branch_statementValues<Token>>), 
  /*2*/ 
  VecExprValues(/*2*/Vec<ExprValues<Token>>), 
  /*6*/ 
  VecTokenValues(/*6*/Vec<TokenValues<Token>>), 
  /*8*/ 
  VecModified_sequence_memberValues(/*8*/Vec<Modified_sequence_memberValues<Token>>), 
  /*12*/ 
  VecNonterm_declarations_list_1Values(/*12*/Vec<Nonterm_declarations_list_1Values<Token>>), 
  /*14*/ 
  VecPreambleValues(/*14*/Vec<PreambleValues<Token>>), 
  /*16*/ 
  Vec__TEMP___Values(/*16*/Vec<__TEMP___Values<Token>>), 
  /*18*/ 
  VecStruct_propValues(/*18*/Vec<Struct_propValues<Token>>), 
  /*21*/ 
  VecGeneric_match_block_list_2Values(/*21*/Vec<Generic_match_block_list_2Values>), 
  /*23*/ 
  VecNonterminal_match_block_list_1Values(/*23*/Vec<Nonterminal_match_block_list_1Values<Token>>), 
  /*24*/ 
  VecTerminal_match_block_list_1Values(/*24*/Vec<Terminal_match_block_list_1Values<Token>>), 
  VecToken(Vec<Token>), 
  Non_branch_statementValues(Non_branch_statementValues<Token>), 
  Transitive_statementValues(Transitive_statementValues<Token>), 
  ExprValues(ExprValues<Token>), 
  Terminal_statementValues(Terminal_statementValues<Token>), 
  NumberValues(NumberValues<Token>), 
  TokenValues(TokenValues<Token>), 
  Modified_sequence_memberValues(Modified_sequence_memberValues<Token>), 
  BodyValues(BodyValues<Token>), 
  Nonterm_declarations_list_1Values(Nonterm_declarations_list_1Values<Token>), 
  NontermValues(NontermValues<Token>), 
  PreambleValues(PreambleValues<Token>), 
  Nonterm_declarations_groupValues(Nonterm_declarations_groupValues<Token>), 
  __TEMP___Values(__TEMP___Values<Token>), 
  Struct_propValues(Struct_propValues<Token>), 
  Rule_groupValues(Rule_groupValues<Token>), 
  Generic_match_block_list_2Values(Generic_match_block_list_2Values), 
  Nonterminal_match_block_list_1Values(Nonterminal_match_block_list_1Values<Token>), 
  Terminal_match_block_list_1Values(Terminal_match_block_list_1Values<Token>), 
  Generic_match_block_group_1Values(Generic_match_block_group_1Values), 
  Nonterminal_match_block_groupValues(Nonterminal_match_block_groupValues<Token>), 
  Terminal_match_block_groupValues(Terminal_match_block_groupValues<Token>), 
  Pop(Box<Pop<Token>>), 
  Rule(Box<Rule<Token>>), 
  Push(Box<Push<Token>>), 
  Peek(Box<Peek<Token>>), 
  Fork(Box<Fork<Token>>), 
  Fail(Box<Fail<Token>>), 
  Goto(Box<Goto<Token>>), 
  Char(Box<Char<Token>>), 
  Pass(Box<Pass<Token>>), 
  Init(Box<Init<Token>>), 
  Range(Box<Range>), 
  State(Box<State<Token>>), 
  Gotos(Box<Gotos<Token>>), 
  Reset(Box<Reset<Token>>), 
  Shift(Box<Shift<Token>>), 
  AST_I8(Box<AST_I8<Token>>), 
  AST_U8(Box<AST_U8<Token>>), 
  Reduce(Box<Reduce<Token>>), 
  Accept(Box<Accept<Token>>), 
  AST_F32(Box<AST_F32<Token>>), 
  AST_I32(Box<AST_I32<Token>>), 
  AST_U32(Box<AST_U32<Token>>), 
  AST_F64(Box<AST_F64<Token>>), 
  AST_I64(Box<AST_I64<Token>>), 
  AST_U64(Box<AST_U64<Token>>), 
  AST_F16(Box<AST_F16<Token>>), 
  AST_I16(Box<AST_I16<Token>>), 
  AST_U16(Box<AST_U16<Token>>), 
  AST_Sub(Box<AST_Sub<Token>>), 
  AST_Add(Box<AST_Add<Token>>), 
  AST_Mod(Box<AST_Mod<Token>>), 
  SetLine(Box<SetLine<Token>>), 
  AST_Neg(Box<AST_Neg<Token>>), 
  AST_Mul(Box<AST_Mul<Token>>), 
  AST_Map(Box<AST_Map<Token>>), 
  Matches(Box<Matches<Token>>), 
  AST_Div(Box<AST_Div<Token>>), 
  AST_Pow(Box<AST_Pow<Token>>), 
  AST_F128(Box<AST_F128<Token>>), 
  AST_U128(Box<AST_U128<Token>>), 
  RegexEnd(RegexEnd), 
  AST_Flag(Box<AST_Flag<Token>>), 
  IntMatch(Box<IntMatch>), 
  AST_Bool(Box<AST_Bool<Token>>), 
  FailHint(Box<FailHint>), 
  AST_Property(Box<AST_Property<Token>>), 
  NonTermSymbol(Box<NonTermSymbol<Token>>), 
  RegexWordChar(RegexWordChar), 
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>), 
  AST_Member(Box<AST_Member<Token>>), 
  AST_NamedReference(Box<AST_NamedReference<Token>>), 
  RegexNewLineChar(RegexNewLineChar), 
  ReduceRaw(Box<ReduceRaw<Token>>), 
  TokenGroupRules(Box<TokenGroupRules<Token>>), 
  DefaultMatch(DefaultMatch), 
  NotEmptySet(Box<NotEmptySet<Token>>), 
  Statement(Box<Statement<Token>>), 
  AST_Token(Box<AST_Token>), 
  AST_StringLiteral(Box<AST_StringLiteral<Token>>), 
  RegexRepeatCount(Box<RegexRepeatCount<Token>>), 
  GrammarDefinition(Box<GrammarDefinition<Token>>), 
  IgnoreScope(Box<IgnoreScope<Token>>), 
  RegexRepeat(Box<RegexRepeat<Token>>), 
  REgexMatch(Box<REgexMatch<Token>>), 
  Grouped_Rules(Box<Grouped_Rules<Token>>), 
  RegexExcludeMatch(Box<RegexExcludeMatch<Token>>), 
  AST_BoolLiteral(Box<AST_BoolLiteral>), 
  RegexEscapedChar(Box<RegexEscapedChar<Token>>), 
  Precedence(Box<Precedence>), 
  NonTermMatch(Box<NonTermMatch<Token>>), 
  ClassSymbol(Box<ClassSymbol<Token>>), 
  RegexOptional(Box<RegexOptional<Token>>), 
  AST_Vector(Box<AST_Vector<Token>>), 
  ImportPreamble(Box<ImportPreamble<Token>>), 
  NontermDeclaration(Box<NontermDeclaration<Token>>), 
  RegexOptionalRepeat(Box<RegexOptionalRepeat<Token>>), 
  RegexDigitChar(RegexDigitChar), 
  SetTokenId(Box<SetTokenId<Token>>), 
  RegexSymbol(Box<RegexSymbol<Token>>), 
  AST_String(Box<AST_String<Token>>), 
  AnnotatedSymbol(Box<AnnotatedSymbol<Token>>), 
  AST_NumberLiteral(Box<AST_NumberLiteral>), 
  RegexStart(RegexStart), 
  SetTokenLen(Box<SetTokenLen>), 
  TerminalToken(Box<TerminalToken<Token>>), 
  IgnorePreamble(Box<IgnorePreamble<Token>>), 
  RegexWildCard(RegexWildCard), 
  ExportPreamble(Box<ExportPreamble<Token>>), 
  EOFSymbol(Box<EOFSymbol<Token>>), 
  RegexSpaceChar(RegexSpaceChar), 
  TokenSymbol(Box<TokenSymbol<Token>>), 
  AST_Statement(Box<AST_Statement<Token>>), 
  AST_Struct(Box<AST_Struct<Token>>), 
  List_Rules(Box<List_Rules<Token>>), 
  NonTermSymbolImportSymbol(Box<NonTermSymbolImportSymbol<Token>>), 
  ProductionMatches(Box<ProductionMatches<Token>>), 
  NamePreamble(Box<NamePreamble>), 
  AST_IndexReference(Box<AST_IndexReference<Token>>), 
  TermMatch(Box<TermMatch<Token>>), 
  RegexGroup(Box<RegexGroup<Token>>), 
  TerminalMatches(Box<TerminalMatches<Token>>), 
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>), 
}

impl<Token:Tk> ASTNode<Token>{pub fn to_token(self) -> Option<Token> {match self {ASTNode::Token(val) => Some(val),_ => None,}}}

impl<Token:Tk> ASTNode<Token>{pub fn into_U32(self) -> Option<u32> {match self {ASTNode::U32(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<u32> for ASTNode<Token>{fn from(value:u32) -> Self {Self::U32(value)}}

impl<Token:Tk> ASTNode<Token>{pub fn into_U64(self) -> Option<u64> {match self {ASTNode::U64(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<u64> for ASTNode<Token>{fn from(value:u64) -> Self {Self::U64(value)}}

impl<Token:Tk> ASTNode<Token>{pub fn into_I32(self) -> Option<i32> {match self {ASTNode::I32(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<i32> for ASTNode<Token>{fn from(value:i32) -> Self {Self::I32(value)}}

impl<Token:Tk> ASTNode<Token>{pub fn into_I64(self) -> Option<i64> {match self {ASTNode::I64(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<i64> for ASTNode<Token>{fn from(value:i64) -> Self {Self::I64(value)}}

impl<Token:Tk> ASTNode<Token>{pub fn into_F64(self) -> Option<f64> {match self {ASTNode::F64(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<f64> for ASTNode<Token>{fn from(value:f64) -> Self {Self::F64(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_String(self) -> Option<String> {match self {ASTNode::String(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<String> for ASTNode<Token>{fn from(value:String) -> Self {Self::String(value)}}

impl<Token:Tk> ASTNode<Token>{pub fn into_Bool(self) -> Option<bool> {match self {ASTNode::Bool(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<bool> for ASTNode<Token>{fn from(value:bool) -> Self {Self::Bool(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecU64(self) -> Option<Vec<u64>> {match self {ASTNode::VecU64(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<u64>> for ASTNode<Token>{fn from(value:Vec<u64>) -> Self {Self::VecU64(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecRule(self) -> Option<Vec<Box<Rule<Token>>>> {match self {ASTNode::VecRule(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<Rule<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<Rule<Token>>>) -> Self {Self::VecRule(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecPush(self) -> Option<Vec<Box<Push<Token>>>> {match self {ASTNode::VecPush(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<Push<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<Push<Token>>>) -> Self {Self::VecPush(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecGoto(self) -> Option<Vec<Box<Goto<Token>>>> {match self {ASTNode::VecGoto(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<Goto<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<Goto<Token>>>) -> Self {Self::VecGoto(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecIntMatch(self) -> Option<Vec<Box<IntMatch>>> {match self {ASTNode::VecIntMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<IntMatch>>> for ASTNode<Token>{fn from(value:Vec<Box<IntMatch>>) -> Self {Self::VecIntMatch(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecNonTermMatch(self) -> Option<Vec<Box<NonTermMatch<Token>>>> {match self {ASTNode::VecNonTermMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<NonTermMatch<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<NonTermMatch<Token>>>) -> Self {Self::VecNonTermMatch(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecNontermDeclaration(self) -> Option<Vec<Box<NontermDeclaration<Token>>>> {match self {ASTNode::VecNontermDeclaration(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<NontermDeclaration<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<NontermDeclaration<Token>>>) -> Self {Self::VecNontermDeclaration(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecAnnotatedSymbol(self) -> Option<Vec<Box<AnnotatedSymbol<Token>>>> {match self {ASTNode::VecAnnotatedSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<AnnotatedSymbol<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<AnnotatedSymbol<Token>>>) -> Self {Self::VecAnnotatedSymbol(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecEOFSymbol(self) -> Option<Vec<Box<EOFSymbol<Token>>>> {match self {ASTNode::VecEOFSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<EOFSymbol<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<EOFSymbol<Token>>>) -> Self {Self::VecEOFSymbol(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecTermMatch(self) -> Option<Vec<Box<TermMatch<Token>>>> {match self {ASTNode::VecTermMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<Box<TermMatch<Token>>>> for ASTNode<Token>{fn from(value:Vec<Box<TermMatch<Token>>>) -> Self {Self::VecTermMatch(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecNon_branch_statementValues(self) -> Option</*0*/Vec<Non_branch_statementValues<Token>>> {match self {ASTNode::VecNon_branch_statementValues(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*0*/Vec<Non_branch_statementValues<Token>>> for ASTNode<Token>{
  fn from(value:/*0*/Vec<Non_branch_statementValues<Token>>) -> Self {Self::VecNon_branch_statementValues(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecExprValues(self) -> Option</*2*/Vec<ExprValues<Token>>> {match self {ASTNode::VecExprValues(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*2*/Vec<ExprValues<Token>>> for ASTNode<Token>{fn from(value:/*2*/Vec<ExprValues<Token>>) -> Self {Self::VecExprValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecTokenValues(self) -> Option</*6*/Vec<TokenValues<Token>>> {match self {ASTNode::VecTokenValues(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*6*/Vec<TokenValues<Token>>> for ASTNode<Token>{fn from(value:/*6*/Vec<TokenValues<Token>>) -> Self {Self::VecTokenValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecModified_sequence_memberValues(self) -> Option</*8*/Vec<Modified_sequence_memberValues<Token>>> {match self {ASTNode::VecModified_sequence_memberValues(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*8*/Vec<Modified_sequence_memberValues<Token>>> for ASTNode<Token>{
  fn from(value:/*8*/Vec<Modified_sequence_memberValues<Token>>) -> Self {Self::VecModified_sequence_memberValues(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecNonterm_declarations_list_1Values(self) -> Option</*12*/Vec<Nonterm_declarations_list_1Values<Token>>> {match self {ASTNode::VecNonterm_declarations_list_1Values(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*12*/Vec<Nonterm_declarations_list_1Values<Token>>> for ASTNode<Token>{
  fn from(value:/*12*/Vec<Nonterm_declarations_list_1Values<Token>>) -> Self {Self::VecNonterm_declarations_list_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecPreambleValues(self) -> Option</*14*/Vec<PreambleValues<Token>>> {match self {ASTNode::VecPreambleValues(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*14*/Vec<PreambleValues<Token>>> for ASTNode<Token>{fn from(value:/*14*/Vec<PreambleValues<Token>>) -> Self {Self::VecPreambleValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Vec__TEMP___Values(self) -> Option</*16*/Vec<__TEMP___Values<Token>>> {match self {ASTNode::Vec__TEMP___Values(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*16*/Vec<__TEMP___Values<Token>>> for ASTNode<Token>{fn from(value:/*16*/Vec<__TEMP___Values<Token>>) -> Self {Self::Vec__TEMP___Values(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecStruct_propValues(self) -> Option</*18*/Vec<Struct_propValues<Token>>> {match self {ASTNode::VecStruct_propValues(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*18*/Vec<Struct_propValues<Token>>> for ASTNode<Token>{fn from(value:/*18*/Vec<Struct_propValues<Token>>) -> Self {Self::VecStruct_propValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecGeneric_match_block_list_2Values(self) -> Option</*21*/Vec<Generic_match_block_list_2Values>> {match self {ASTNode::VecGeneric_match_block_list_2Values(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*21*/Vec<Generic_match_block_list_2Values>> for ASTNode<Token>{
  fn from(value:/*21*/Vec<Generic_match_block_list_2Values>) -> Self {Self::VecGeneric_match_block_list_2Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecNonterminal_match_block_list_1Values(self) -> Option</*23*/Vec<Nonterminal_match_block_list_1Values<Token>>> {match self {ASTNode::VecNonterminal_match_block_list_1Values(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*23*/Vec<Nonterminal_match_block_list_1Values<Token>>> for ASTNode<Token>{
  fn from(value:/*23*/Vec<Nonterminal_match_block_list_1Values<Token>>) -> Self {Self::VecNonterminal_match_block_list_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecTerminal_match_block_list_1Values(self) -> Option</*24*/Vec<Terminal_match_block_list_1Values<Token>>> {match self {ASTNode::VecTerminal_match_block_list_1Values(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From</*24*/Vec<Terminal_match_block_list_1Values<Token>>> for ASTNode<Token>{
  fn from(value:/*24*/Vec<Terminal_match_block_list_1Values<Token>>) -> Self {Self::VecTerminal_match_block_list_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_VecToken(self) -> Option<Vec<Token>> {match self {ASTNode::VecToken(val) => Some(val),_ => None,}}
}

#[derive(Clone, Debug, Default)]
pub enum Non_branch_statementValues<Token:Tk>{
  #[default]
  None,
  Reduce(Box<Reduce<Token>>), 
  SetLine(Box<SetLine<Token>>), 
  ReduceRaw(Box<ReduceRaw<Token>>), 
  SetTokenId(Box<SetTokenId<Token>>), 
  SetTokenLen(Box<SetTokenLen>), 
}

#[derive(Clone, Debug, Default)]
pub enum Transitive_statementValues<Token:Tk>{#[default]None,Peek(Box<Peek<Token>>), Reset(Box<Reset<Token>>), Shift(Box<Shift<Token>>), }

#[derive(Clone, Debug, Default)]
pub enum ExprValues<Token:Tk>{
  #[default]
  None,
  AST_I8(Box<AST_I8<Token>>), 
  AST_U8(Box<AST_U8<Token>>), 
  AST_F32(Box<AST_F32<Token>>), 
  AST_I32(Box<AST_I32<Token>>), 
  AST_U32(Box<AST_U32<Token>>), 
  AST_F64(Box<AST_F64<Token>>), 
  AST_I64(Box<AST_I64<Token>>), 
  AST_U64(Box<AST_U64<Token>>), 
  AST_F16(Box<AST_F16<Token>>), 
  AST_I16(Box<AST_I16<Token>>), 
  AST_U16(Box<AST_U16<Token>>), 
  AST_Sub(Box<AST_Sub<Token>>), 
  AST_Add(Box<AST_Add<Token>>), 
  AST_Mod(Box<AST_Mod<Token>>), 
  AST_Neg(Box<AST_Neg<Token>>), 
  AST_Mul(Box<AST_Mul<Token>>), 
  AST_Map(Box<AST_Map<Token>>), 
  AST_Div(Box<AST_Div<Token>>), 
  AST_Pow(Box<AST_Pow<Token>>), 
  AST_F128(Box<AST_F128<Token>>), 
  AST_U128(Box<AST_U128<Token>>), 
  AST_Bool(Box<AST_Bool<Token>>), 
  AST_Member(Box<AST_Member<Token>>), 
  AST_NamedReference(Box<AST_NamedReference<Token>>), 
  AST_Token(Box<AST_Token>), 
  AST_StringLiteral(Box<AST_StringLiteral<Token>>), 
  AST_BoolLiteral(Box<AST_BoolLiteral>), 
  AST_Vector(Box<AST_Vector<Token>>), 
  AST_String(Box<AST_String<Token>>), 
  AST_NumberLiteral(Box<AST_NumberLiteral>), 
  AST_Struct(Box<AST_Struct<Token>>), 
  AST_IndexReference(Box<AST_IndexReference<Token>>), 
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Terminal_statementValues<Token:Tk>{
  #[default]
  None,
  Fail(Box<Fail<Token>>), 
  Pass(Box<Pass<Token>>), 
  Gotos(Box<Gotos<Token>>), 
  Accept(Box<Accept<Token>>), 
  Matches(Box<Matches<Token>>), 
  ProductionMatches(Box<ProductionMatches<Token>>), 
  TerminalMatches(Box<TerminalMatches<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum NumberValues<Token:Tk>{
  #[default]
  None,
  AST_I8(Box<AST_I8<Token>>), 
  AST_U8(Box<AST_U8<Token>>), 
  AST_F32(Box<AST_F32<Token>>), 
  AST_I32(Box<AST_I32<Token>>), 
  AST_U32(Box<AST_U32<Token>>), 
  AST_F64(Box<AST_F64<Token>>), 
  AST_I64(Box<AST_I64<Token>>), 
  AST_U64(Box<AST_U64<Token>>), 
  AST_F16(Box<AST_F16<Token>>), 
  AST_I16(Box<AST_I16<Token>>), 
  AST_U16(Box<AST_U16<Token>>), 
  AST_Map(Box<AST_Map<Token>>), 
  AST_F128(Box<AST_F128<Token>>), 
  AST_U128(Box<AST_U128<Token>>), 
  AST_Bool(Box<AST_Bool<Token>>), 
  AST_Member(Box<AST_Member<Token>>), 
  AST_Token(Box<AST_Token>), 
  AST_StringLiteral(Box<AST_StringLiteral<Token>>), 
  AST_BoolLiteral(Box<AST_BoolLiteral>), 
  AST_Vector(Box<AST_Vector<Token>>), 
  AST_String(Box<AST_String<Token>>), 
  AST_NumberLiteral(Box<AST_NumberLiteral>), 
}

#[derive(Clone, Debug, Default)]
pub enum TokenValues<Token:Tk>{
  #[default]
  None,
  NonTermSymbol(Box<NonTermSymbol<Token>>), 
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>), 
  TokenGroupRules(Box<TokenGroupRules<Token>>), 
  Grouped_Rules(Box<Grouped_Rules<Token>>), 
  ClassSymbol(Box<ClassSymbol<Token>>), 
  RegexSymbol(Box<RegexSymbol<Token>>), 
  TerminalToken(Box<TerminalToken<Token>>), 
  TokenSymbol(Box<TokenSymbol<Token>>), 
  List_Rules(Box<List_Rules<Token>>), 
  NonTermSymbolImportSymbol(Box<NonTermSymbolImportSymbol<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Modified_sequence_memberValues<Token:Tk>{
  #[default]
  None,
  Char(Box<Char<Token>>), 
  RegexEnd(RegexEnd), 
  RegexWordChar(RegexWordChar), 
  RegexNewLineChar(RegexNewLineChar), 
  RegexRepeatCount(Box<RegexRepeatCount<Token>>), 
  RegexRepeat(Box<RegexRepeat<Token>>), 
  REgexMatch(Box<REgexMatch<Token>>), 
  RegexExcludeMatch(Box<RegexExcludeMatch<Token>>), 
  RegexEscapedChar(Box<RegexEscapedChar<Token>>), 
  RegexOptional(Box<RegexOptional<Token>>), 
  RegexOptionalRepeat(Box<RegexOptionalRepeat<Token>>), 
  RegexDigitChar(RegexDigitChar), 
  RegexStart(RegexStart), 
  RegexWildCard(RegexWildCard), 
  RegexSpaceChar(RegexSpaceChar), 
  RegexGroup(Box<RegexGroup<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum BodyValues<Token:Tk>{
  #[default]
  None,
  AST_Flag(Box<AST_Flag<Token>>), 
  AST_Statement(Box<AST_Statement<Token>>), 
  AST_Struct(Box<AST_Struct<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Nonterm_declarations_list_1Values<Token:Tk>{
  #[default]
  None,
  IgnoreScope(Box<IgnoreScope<Token>>), 
  NontermDeclaration(Box<NontermDeclaration<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum NontermValues<Token:Tk>{
  #[default]
  None,
  NonTermSymbol(Box<NonTermSymbol<Token>>), 
  TokenGroupRules(Box<TokenGroupRules<Token>>), 
  Grouped_Rules(Box<Grouped_Rules<Token>>), 
  List_Rules(Box<List_Rules<Token>>), 
  NonTermSymbolImportSymbol(Box<NonTermSymbolImportSymbol<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum PreambleValues<Token:Tk>{
  #[default]
  None,
  ImportPreamble(Box<ImportPreamble<Token>>), 
  IgnorePreamble(Box<IgnorePreamble<Token>>), 
  ExportPreamble(Box<ExportPreamble<Token>>), 
  NamePreamble(Box<NamePreamble>), 
}

#[derive(Clone, Debug, Default)]
pub enum Nonterm_declarations_groupValues<Token:Tk>{
  #[default]
  None,
  IgnoreScope(Box<IgnoreScope<Token>>), 
  NontermDeclaration(Box<NontermDeclaration<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum __TEMP___Values<Token:Tk>{
  #[default]
  None,
  NotEmptySet(Box<NotEmptySet<Token>>), 
  AnnotatedSymbol(Box<AnnotatedSymbol<Token>>), 
  EOFSymbol(Box<EOFSymbol<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Struct_propValues<Token:Tk>{#[default]None,AST_Property(Box<AST_Property<Token>>), AST_Token(Box<AST_Token>), }

#[derive(Clone, Debug, Default)]
pub enum Rule_groupValues<Token:Tk>{#[default]None,NotEmptySet(Box<NotEmptySet<Token>>), AnnotatedSymbol(Box<AnnotatedSymbol<Token>>), }

#[derive(Clone, Debug, Default)]
pub enum Generic_match_block_list_2Values{#[default]None,IntMatch(Box<IntMatch>), FailHint(Box<FailHint>), DefaultMatch(DefaultMatch), }

#[derive(Clone, Debug, Default)]
pub enum Nonterminal_match_block_list_1Values<Token:Tk>{
  #[default]
  None,
  FailHint(Box<FailHint>), 
  DefaultMatch(DefaultMatch), 
  NonTermMatch(Box<NonTermMatch<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Terminal_match_block_list_1Values<Token:Tk>{
  #[default]
  None,
  FailHint(Box<FailHint>), 
  DefaultMatch(DefaultMatch), 
  TermMatch(Box<TermMatch<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Generic_match_block_group_1Values{#[default]None,IntMatch(Box<IntMatch>), FailHint(Box<FailHint>), DefaultMatch(DefaultMatch), }

#[derive(Clone, Debug, Default)]
pub enum Nonterminal_match_block_groupValues<Token:Tk>{
  #[default]
  None,
  FailHint(Box<FailHint>), 
  DefaultMatch(DefaultMatch), 
  NonTermMatch(Box<NonTermMatch<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum Terminal_match_block_groupValues<Token:Tk>{
  #[default]
  None,
  FailHint(Box<FailHint>), 
  DefaultMatch(DefaultMatch), 
  TermMatch(Box<TermMatch<Token>>), 
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Non_branch_statementValues(self) -> Option<Non_branch_statementValues<Token>> {
    match self {
      ASTNode::Non_branch_statementValues(val) => Some(val),
      ASTNode::Reduce(val) => Some(Non_branch_statementValues::Reduce(val)),
      ASTNode::SetLine(val) => Some(Non_branch_statementValues::SetLine(val)),
      ASTNode::ReduceRaw(val) => Some(Non_branch_statementValues::ReduceRaw(val)),
      ASTNode::SetTokenId(val) => Some(Non_branch_statementValues::SetTokenId(val)),
      ASTNode::SetTokenLen(val) => Some(Non_branch_statementValues::SetTokenLen(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Non_branch_statementValues<Token>> for ASTNode<Token>{fn from(value: Non_branch_statementValues<Token>) -> Self {Self::Non_branch_statementValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Transitive_statementValues(self) -> Option<Transitive_statementValues<Token>> {
    match self {
      ASTNode::Transitive_statementValues(val) => Some(val),
      ASTNode::Peek(val) => Some(Transitive_statementValues::Peek(val)),
      ASTNode::Reset(val) => Some(Transitive_statementValues::Reset(val)),
      ASTNode::Shift(val) => Some(Transitive_statementValues::Shift(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Transitive_statementValues<Token>> for ASTNode<Token>{fn from(value: Transitive_statementValues<Token>) -> Self {Self::Transitive_statementValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ExprValues(self) -> Option<ExprValues<Token>> {
    match self {
      ASTNode::ExprValues(val) => Some(val),
      ASTNode::AST_I8(val) => Some(ExprValues::AST_I8(val)),
      ASTNode::AST_U8(val) => Some(ExprValues::AST_U8(val)),
      ASTNode::AST_F32(val) => Some(ExprValues::AST_F32(val)),
      ASTNode::AST_I32(val) => Some(ExprValues::AST_I32(val)),
      ASTNode::AST_U32(val) => Some(ExprValues::AST_U32(val)),
      ASTNode::AST_F64(val) => Some(ExprValues::AST_F64(val)),
      ASTNode::AST_I64(val) => Some(ExprValues::AST_I64(val)),
      ASTNode::AST_U64(val) => Some(ExprValues::AST_U64(val)),
      ASTNode::AST_F16(val) => Some(ExprValues::AST_F16(val)),
      ASTNode::AST_I16(val) => Some(ExprValues::AST_I16(val)),
      ASTNode::AST_U16(val) => Some(ExprValues::AST_U16(val)),
      ASTNode::AST_Sub(val) => Some(ExprValues::AST_Sub(val)),
      ASTNode::AST_Add(val) => Some(ExprValues::AST_Add(val)),
      ASTNode::AST_Mod(val) => Some(ExprValues::AST_Mod(val)),
      ASTNode::AST_Neg(val) => Some(ExprValues::AST_Neg(val)),
      ASTNode::AST_Mul(val) => Some(ExprValues::AST_Mul(val)),
      ASTNode::AST_Map(val) => Some(ExprValues::AST_Map(val)),
      ASTNode::AST_Div(val) => Some(ExprValues::AST_Div(val)),
      ASTNode::AST_Pow(val) => Some(ExprValues::AST_Pow(val)),
      ASTNode::AST_F128(val) => Some(ExprValues::AST_F128(val)),
      ASTNode::AST_U128(val) => Some(ExprValues::AST_U128(val)),
      ASTNode::AST_Bool(val) => Some(ExprValues::AST_Bool(val)),
      ASTNode::AST_Member(val) => Some(ExprValues::AST_Member(val)),
      ASTNode::AST_NamedReference(val) => Some(ExprValues::AST_NamedReference(val)),
      ASTNode::AST_Token(val) => Some(ExprValues::AST_Token(val)),
      ASTNode::AST_StringLiteral(val) => Some(ExprValues::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(ExprValues::AST_BoolLiteral(val)),
      ASTNode::AST_Vector(val) => Some(ExprValues::AST_Vector(val)),
      ASTNode::AST_String(val) => Some(ExprValues::AST_String(val)),
      ASTNode::AST_NumberLiteral(val) => Some(ExprValues::AST_NumberLiteral(val)),
      ASTNode::AST_Struct(val) => Some(ExprValues::AST_Struct(val)),
      ASTNode::AST_IndexReference(val) => Some(ExprValues::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(ExprValues::AST_TrimmedReference(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<ExprValues<Token>> for ASTNode<Token>{fn from(value: ExprValues<Token>) -> Self {Self::ExprValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Terminal_statementValues(self) -> Option<Terminal_statementValues<Token>> {
    match self {
      ASTNode::Terminal_statementValues(val) => Some(val),
      ASTNode::Fail(val) => Some(Terminal_statementValues::Fail(val)),
      ASTNode::Pass(val) => Some(Terminal_statementValues::Pass(val)),
      ASTNode::Gotos(val) => Some(Terminal_statementValues::Gotos(val)),
      ASTNode::Accept(val) => Some(Terminal_statementValues::Accept(val)),
      ASTNode::Matches(val) => Some(Terminal_statementValues::Matches(val)),
      ASTNode::ProductionMatches(val) => Some(Terminal_statementValues::ProductionMatches(val)),
      ASTNode::TerminalMatches(val) => Some(Terminal_statementValues::TerminalMatches(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Terminal_statementValues<Token>> for ASTNode<Token>{fn from(value: Terminal_statementValues<Token>) -> Self {Self::Terminal_statementValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NumberValues(self) -> Option<NumberValues<Token>> {
    match self {
      ASTNode::NumberValues(val) => Some(val),
      ASTNode::AST_I8(val) => Some(NumberValues::AST_I8(val)),
      ASTNode::AST_U8(val) => Some(NumberValues::AST_U8(val)),
      ASTNode::AST_F32(val) => Some(NumberValues::AST_F32(val)),
      ASTNode::AST_I32(val) => Some(NumberValues::AST_I32(val)),
      ASTNode::AST_U32(val) => Some(NumberValues::AST_U32(val)),
      ASTNode::AST_F64(val) => Some(NumberValues::AST_F64(val)),
      ASTNode::AST_I64(val) => Some(NumberValues::AST_I64(val)),
      ASTNode::AST_U64(val) => Some(NumberValues::AST_U64(val)),
      ASTNode::AST_F16(val) => Some(NumberValues::AST_F16(val)),
      ASTNode::AST_I16(val) => Some(NumberValues::AST_I16(val)),
      ASTNode::AST_U16(val) => Some(NumberValues::AST_U16(val)),
      ASTNode::AST_Map(val) => Some(NumberValues::AST_Map(val)),
      ASTNode::AST_F128(val) => Some(NumberValues::AST_F128(val)),
      ASTNode::AST_U128(val) => Some(NumberValues::AST_U128(val)),
      ASTNode::AST_Bool(val) => Some(NumberValues::AST_Bool(val)),
      ASTNode::AST_Member(val) => Some(NumberValues::AST_Member(val)),
      ASTNode::AST_Token(val) => Some(NumberValues::AST_Token(val)),
      ASTNode::AST_StringLiteral(val) => Some(NumberValues::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(NumberValues::AST_BoolLiteral(val)),
      ASTNode::AST_Vector(val) => Some(NumberValues::AST_Vector(val)),
      ASTNode::AST_String(val) => Some(NumberValues::AST_String(val)),
      ASTNode::AST_NumberLiteral(val) => Some(NumberValues::AST_NumberLiteral(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<NumberValues<Token>> for ASTNode<Token>{fn from(value: NumberValues<Token>) -> Self {Self::NumberValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_TokenValues(self) -> Option<TokenValues<Token>> {
    match self {
      ASTNode::TokenValues(val) => Some(val),
      ASTNode::NonTermSymbol(val) => Some(TokenValues::NonTermSymbol(val)),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(TokenValues::NonTerminal_Terminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(TokenValues::TokenGroupRules(val)),
      ASTNode::Grouped_Rules(val) => Some(TokenValues::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(TokenValues::ClassSymbol(val)),
      ASTNode::RegexSymbol(val) => Some(TokenValues::RegexSymbol(val)),
      ASTNode::TerminalToken(val) => Some(TokenValues::TerminalToken(val)),
      ASTNode::TokenSymbol(val) => Some(TokenValues::TokenSymbol(val)),
      ASTNode::List_Rules(val) => Some(TokenValues::List_Rules(val)),
      ASTNode::NonTermSymbolImportSymbol(val) => Some(TokenValues::NonTermSymbolImportSymbol(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<TokenValues<Token>> for ASTNode<Token>{fn from(value: TokenValues<Token>) -> Self {Self::TokenValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Modified_sequence_memberValues(self) -> Option<Modified_sequence_memberValues<Token>> {
    match self {
      ASTNode::Modified_sequence_memberValues(val) => Some(val),
      ASTNode::Char(val) => Some(Modified_sequence_memberValues::Char(val)),
      ASTNode::RegexEnd(val) => Some(Modified_sequence_memberValues::RegexEnd(val)),
      ASTNode::RegexWordChar(val) => Some(Modified_sequence_memberValues::RegexWordChar(val)),
      ASTNode::RegexNewLineChar(val) => Some(Modified_sequence_memberValues::RegexNewLineChar(val)),
      ASTNode::RegexRepeatCount(val) => Some(Modified_sequence_memberValues::RegexRepeatCount(val)),
      ASTNode::RegexRepeat(val) => Some(Modified_sequence_memberValues::RegexRepeat(val)),
      ASTNode::REgexMatch(val) => Some(Modified_sequence_memberValues::REgexMatch(val)),
      ASTNode::RegexExcludeMatch(val) => Some(Modified_sequence_memberValues::RegexExcludeMatch(val)),
      ASTNode::RegexEscapedChar(val) => Some(Modified_sequence_memberValues::RegexEscapedChar(val)),
      ASTNode::RegexOptional(val) => Some(Modified_sequence_memberValues::RegexOptional(val)),
      ASTNode::RegexOptionalRepeat(val) => Some(Modified_sequence_memberValues::RegexOptionalRepeat(val)),
      ASTNode::RegexDigitChar(val) => Some(Modified_sequence_memberValues::RegexDigitChar(val)),
      ASTNode::RegexStart(val) => Some(Modified_sequence_memberValues::RegexStart(val)),
      ASTNode::RegexWildCard(val) => Some(Modified_sequence_memberValues::RegexWildCard(val)),
      ASTNode::RegexSpaceChar(val) => Some(Modified_sequence_memberValues::RegexSpaceChar(val)),
      ASTNode::RegexGroup(val) => Some(Modified_sequence_memberValues::RegexGroup(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Modified_sequence_memberValues<Token>> for ASTNode<Token>{
  fn from(value: Modified_sequence_memberValues<Token>) -> Self {Self::Modified_sequence_memberValues(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_BodyValues(self) -> Option<BodyValues<Token>> {
    match self {
      ASTNode::BodyValues(val) => Some(val),
      ASTNode::AST_Flag(val) => Some(BodyValues::AST_Flag(val)),
      ASTNode::AST_Statement(val) => Some(BodyValues::AST_Statement(val)),
      ASTNode::AST_Struct(val) => Some(BodyValues::AST_Struct(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<BodyValues<Token>> for ASTNode<Token>{fn from(value: BodyValues<Token>) -> Self {Self::BodyValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Nonterm_declarations_list_1Values(self) -> Option<Nonterm_declarations_list_1Values<Token>> {
    match self {
      ASTNode::Nonterm_declarations_list_1Values(val) => Some(val),
      ASTNode::IgnoreScope(val) => Some(Nonterm_declarations_list_1Values::IgnoreScope(val)),
      ASTNode::NontermDeclaration(val) => Some(Nonterm_declarations_list_1Values::NontermDeclaration(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Nonterm_declarations_list_1Values<Token>> for ASTNode<Token>{
  fn from(value: Nonterm_declarations_list_1Values<Token>) -> Self {Self::Nonterm_declarations_list_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NontermValues(self) -> Option<NontermValues<Token>> {
    match self {
      ASTNode::NontermValues(val) => Some(val),
      ASTNode::NonTermSymbol(val) => Some(NontermValues::NonTermSymbol(val)),
      ASTNode::TokenGroupRules(val) => Some(NontermValues::TokenGroupRules(val)),
      ASTNode::Grouped_Rules(val) => Some(NontermValues::Grouped_Rules(val)),
      ASTNode::List_Rules(val) => Some(NontermValues::List_Rules(val)),
      ASTNode::NonTermSymbolImportSymbol(val) => Some(NontermValues::NonTermSymbolImportSymbol(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<NontermValues<Token>> for ASTNode<Token>{fn from(value: NontermValues<Token>) -> Self {Self::NontermValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_PreambleValues(self) -> Option<PreambleValues<Token>> {
    match self {
      ASTNode::PreambleValues(val) => Some(val),
      ASTNode::ImportPreamble(val) => Some(PreambleValues::ImportPreamble(val)),
      ASTNode::IgnorePreamble(val) => Some(PreambleValues::IgnorePreamble(val)),
      ASTNode::ExportPreamble(val) => Some(PreambleValues::ExportPreamble(val)),
      ASTNode::NamePreamble(val) => Some(PreambleValues::NamePreamble(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<PreambleValues<Token>> for ASTNode<Token>{fn from(value: PreambleValues<Token>) -> Self {Self::PreambleValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Nonterm_declarations_groupValues(self) -> Option<Nonterm_declarations_groupValues<Token>> {
    match self {
      ASTNode::Nonterm_declarations_groupValues(val) => Some(val),
      ASTNode::IgnoreScope(val) => Some(Nonterm_declarations_groupValues::IgnoreScope(val)),
      ASTNode::NontermDeclaration(val) => Some(Nonterm_declarations_groupValues::NontermDeclaration(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Nonterm_declarations_groupValues<Token>> for ASTNode<Token>{
  fn from(value: Nonterm_declarations_groupValues<Token>) -> Self {Self::Nonterm_declarations_groupValues(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into___TEMP___Values(self) -> Option<__TEMP___Values<Token>> {
    match self {
      ASTNode::__TEMP___Values(val) => Some(val),
      ASTNode::NotEmptySet(val) => Some(__TEMP___Values::NotEmptySet(val)),
      ASTNode::AnnotatedSymbol(val) => Some(__TEMP___Values::AnnotatedSymbol(val)),
      ASTNode::EOFSymbol(val) => Some(__TEMP___Values::EOFSymbol(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<__TEMP___Values<Token>> for ASTNode<Token>{fn from(value: __TEMP___Values<Token>) -> Self {Self::__TEMP___Values(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Struct_propValues(self) -> Option<Struct_propValues<Token>> {
    match self {
      ASTNode::Struct_propValues(val) => Some(val),
      ASTNode::AST_Property(val) => Some(Struct_propValues::AST_Property(val)),
      ASTNode::AST_Token(val) => Some(Struct_propValues::AST_Token(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Struct_propValues<Token>> for ASTNode<Token>{fn from(value: Struct_propValues<Token>) -> Self {Self::Struct_propValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Rule_groupValues(self) -> Option<Rule_groupValues<Token>> {
    match self {
      ASTNode::Rule_groupValues(val) => Some(val),
      ASTNode::NotEmptySet(val) => Some(Rule_groupValues::NotEmptySet(val)),
      ASTNode::AnnotatedSymbol(val) => Some(Rule_groupValues::AnnotatedSymbol(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Rule_groupValues<Token>> for ASTNode<Token>{fn from(value: Rule_groupValues<Token>) -> Self {Self::Rule_groupValues(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Generic_match_block_list_2Values(self) -> Option<Generic_match_block_list_2Values> {
    match self {
      ASTNode::Generic_match_block_list_2Values(val) => Some(val),
      ASTNode::IntMatch(val) => Some(Generic_match_block_list_2Values::IntMatch(val)),
      ASTNode::FailHint(val) => Some(Generic_match_block_list_2Values::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(Generic_match_block_list_2Values::DefaultMatch(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Generic_match_block_list_2Values> for ASTNode<Token>{
  fn from(value: Generic_match_block_list_2Values) -> Self {Self::Generic_match_block_list_2Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Nonterminal_match_block_list_1Values(self) -> Option<Nonterminal_match_block_list_1Values<Token>> {
    match self {
      ASTNode::Nonterminal_match_block_list_1Values(val) => Some(val),
      ASTNode::FailHint(val) => Some(Nonterminal_match_block_list_1Values::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(Nonterminal_match_block_list_1Values::DefaultMatch(val)),
      ASTNode::NonTermMatch(val) => Some(Nonterminal_match_block_list_1Values::NonTermMatch(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Nonterminal_match_block_list_1Values<Token>> for ASTNode<Token>{
  fn from(value: Nonterminal_match_block_list_1Values<Token>) -> Self {Self::Nonterminal_match_block_list_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Terminal_match_block_list_1Values(self) -> Option<Terminal_match_block_list_1Values<Token>> {
    match self {
      ASTNode::Terminal_match_block_list_1Values(val) => Some(val),
      ASTNode::FailHint(val) => Some(Terminal_match_block_list_1Values::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(Terminal_match_block_list_1Values::DefaultMatch(val)),
      ASTNode::TermMatch(val) => Some(Terminal_match_block_list_1Values::TermMatch(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Terminal_match_block_list_1Values<Token>> for ASTNode<Token>{
  fn from(value: Terminal_match_block_list_1Values<Token>) -> Self {Self::Terminal_match_block_list_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Generic_match_block_group_1Values(self) -> Option<Generic_match_block_group_1Values> {
    match self {
      ASTNode::Generic_match_block_group_1Values(val) => Some(val),
      ASTNode::IntMatch(val) => Some(Generic_match_block_group_1Values::IntMatch(val)),
      ASTNode::FailHint(val) => Some(Generic_match_block_group_1Values::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(Generic_match_block_group_1Values::DefaultMatch(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Generic_match_block_group_1Values> for ASTNode<Token>{
  fn from(value: Generic_match_block_group_1Values) -> Self {Self::Generic_match_block_group_1Values(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Nonterminal_match_block_groupValues(self) -> Option<Nonterminal_match_block_groupValues<Token>> {
    match self {
      ASTNode::Nonterminal_match_block_groupValues(val) => Some(val),
      ASTNode::FailHint(val) => Some(Nonterminal_match_block_groupValues::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(Nonterminal_match_block_groupValues::DefaultMatch(val)),
      ASTNode::NonTermMatch(val) => Some(Nonterminal_match_block_groupValues::NonTermMatch(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Nonterminal_match_block_groupValues<Token>> for ASTNode<Token>{
  fn from(value: Nonterminal_match_block_groupValues<Token>) -> Self {Self::Nonterminal_match_block_groupValues(value)}
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Terminal_match_block_groupValues(self) -> Option<Terminal_match_block_groupValues<Token>> {
    match self {
      ASTNode::Terminal_match_block_groupValues(val) => Some(val),
      ASTNode::FailHint(val) => Some(Terminal_match_block_groupValues::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(Terminal_match_block_groupValues::DefaultMatch(val)),
      ASTNode::TermMatch(val) => Some(Terminal_match_block_groupValues::TermMatch(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<Terminal_match_block_groupValues<Token>> for ASTNode<Token>{
  fn from(value: Terminal_match_block_groupValues<Token>) -> Self {Self::Terminal_match_block_groupValues(value)}
}

impl<Token:Tk> ASTNode<Token> {
  pub fn token (&self) -> Token {
    match self {
      ASTNode::Pop(n) => {n.tok.clone()}
      ASTNode::Rule(n) => {n.tok.clone()}
      ASTNode::Push(n) => {n.tok.clone()}
      ASTNode::Peek(n) => {n.tok.clone()}
      ASTNode::Fork(n) => {n.tok.clone()}
      ASTNode::Fail(n) => {n.tok.clone()}
      ASTNode::Goto(n) => {n.tok.clone()}
      ASTNode::Pass(n) => {n.tok.clone()}
      ASTNode::State(n) => {n.tok.clone()}
      ASTNode::Reset(n) => {n.tok.clone()}
      ASTNode::Shift(n) => {n.tok.clone()}
      ASTNode::AST_I8(n) => {n.tok.clone()}
      ASTNode::AST_U8(n) => {n.tok.clone()}
      ASTNode::Reduce(n) => {n.tok.clone()}
      ASTNode::Accept(n) => {n.tok.clone()}
      ASTNode::AST_F32(n) => {n.tok.clone()}
      ASTNode::AST_I32(n) => {n.tok.clone()}
      ASTNode::AST_U32(n) => {n.tok.clone()}
      ASTNode::AST_F64(n) => {n.tok.clone()}
      ASTNode::AST_I64(n) => {n.tok.clone()}
      ASTNode::AST_U64(n) => {n.tok.clone()}
      ASTNode::AST_F16(n) => {n.tok.clone()}
      ASTNode::AST_I16(n) => {n.tok.clone()}
      ASTNode::AST_U16(n) => {n.tok.clone()}
      ASTNode::AST_Sub(n) => {n.tok.clone()}
      ASTNode::AST_Add(n) => {n.tok.clone()}
      ASTNode::AST_Mod(n) => {n.tok.clone()}
      ASTNode::SetLine(n) => {n.tok.clone()}
      ASTNode::AST_Neg(n) => {n.tok.clone()}
      ASTNode::AST_Mul(n) => {n.tok.clone()}
      ASTNode::AST_Map(n) => {n.tok.clone()}
      ASTNode::Matches(n) => {n.tok.clone()}
      ASTNode::AST_Div(n) => {n.tok.clone()}
      ASTNode::AST_Pow(n) => {n.tok.clone()}
      ASTNode::AST_F128(n) => {n.tok.clone()}
      ASTNode::AST_U128(n) => {n.tok.clone()}
      ASTNode::AST_Flag(n) => {n.tok.clone()}
      ASTNode::AST_Bool(n) => {n.tok.clone()}
      ASTNode::AST_Property(n) => {n.tok.clone()}
      ASTNode::NonTermSymbol(n) => {n.tok.clone()}
      ASTNode::NonTerminal_Terminal_Symbol(n) => {n.tok.clone()}
      ASTNode::AST_NamedReference(n) => {n.tok.clone()}
      ASTNode::ReduceRaw(n) => {n.tok.clone()}
      ASTNode::TokenGroupRules(n) => {n.tok.clone()}
      ASTNode::NotEmptySet(n) => {n.tok.clone()}
      ASTNode::AST_StringLiteral(n) => {n.tok.clone()}
      ASTNode::Grouped_Rules(n) => {n.tok.clone()}
      ASTNode::ClassSymbol(n) => {n.tok.clone()}
      ASTNode::AST_Vector(n) => {n.tok.clone()}
      ASTNode::ImportPreamble(n) => {n.tok.clone()}
      ASTNode::NontermDeclaration(n) => {n.tok.clone()}
      ASTNode::SetTokenId(n) => {n.tok.clone()}
      ASTNode::AST_String(n) => {n.tok.clone()}
      ASTNode::AnnotatedSymbol(n) => {n.tok.clone()}
      ASTNode::TerminalToken(n) => {n.tok.clone()}
      ASTNode::EOFSymbol(n) => {n.tok.clone()}
      ASTNode::TokenSymbol(n) => {n.tok.clone()}
      ASTNode::AST_Statement(n) => {n.tok.clone()}
      ASTNode::AST_Struct(n) => {n.tok.clone()}
      ASTNode::List_Rules(n) => {n.tok.clone()}
      ASTNode::NonTermSymbolImportSymbol(n) => {n.tok.clone()}
      ASTNode::AST_IndexReference(n) => {n.tok.clone()}
      ASTNode::AST_TrimmedReference(n) => {n.tok.clone()}
      ASTNode::Token(tok) => tok.clone(),_ => Default::default()
    }
  }
}

/*impl<Token:Tk> std::hash::Hash for ASTNode<Token> {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H){match self{
      ASTNode::Pop(n) => n.hash(hasher),
      ASTNode::Rule(n) => n.hash(hasher),
      ASTNode::Push(n) => n.hash(hasher),
      ASTNode::Peek(n) => n.hash(hasher),
      ASTNode::Fork(n) => n.hash(hasher),
      ASTNode::Fail(n) => n.hash(hasher),
      ASTNode::Goto(n) => n.hash(hasher),
      ASTNode::Char(n) => n.hash(hasher),
      ASTNode::Pass(n) => n.hash(hasher),
      ASTNode::Init(n) => n.hash(hasher),
      ASTNode::Range(n) => n.hash(hasher),
      ASTNode::State(n) => n.hash(hasher),
      ASTNode::Gotos(n) => n.hash(hasher),
      ASTNode::Reset(n) => n.hash(hasher),
      ASTNode::Shift(n) => n.hash(hasher),
      ASTNode::AST_I8(n) => n.hash(hasher),
      ASTNode::AST_U8(n) => n.hash(hasher),
      ASTNode::Reduce(n) => n.hash(hasher),
      ASTNode::Accept(n) => n.hash(hasher),
      ASTNode::AST_F32(n) => n.hash(hasher),
      ASTNode::AST_I32(n) => n.hash(hasher),
      ASTNode::AST_U32(n) => n.hash(hasher),
      ASTNode::AST_F64(n) => n.hash(hasher),
      ASTNode::AST_I64(n) => n.hash(hasher),
      ASTNode::AST_U64(n) => n.hash(hasher),
      ASTNode::AST_F16(n) => n.hash(hasher),
      ASTNode::AST_I16(n) => n.hash(hasher),
      ASTNode::AST_U16(n) => n.hash(hasher),
      ASTNode::AST_Sub(n) => n.hash(hasher),
      ASTNode::AST_Add(n) => n.hash(hasher),
      ASTNode::AST_Mod(n) => n.hash(hasher),
      ASTNode::SetLine(n) => n.hash(hasher),
      ASTNode::AST_Neg(n) => n.hash(hasher),
      ASTNode::AST_Mul(n) => n.hash(hasher),
      ASTNode::AST_Map(n) => n.hash(hasher),
      ASTNode::Matches(n) => n.hash(hasher),
      ASTNode::AST_Div(n) => n.hash(hasher),
      ASTNode::AST_Pow(n) => n.hash(hasher),
      ASTNode::AST_F128(n) => n.hash(hasher),
      ASTNode::AST_U128(n) => n.hash(hasher),
      ASTNode::RegexEnd(n) => n.hash(hasher),
      ASTNode::AST_Flag(n) => n.hash(hasher),
      ASTNode::IntMatch(n) => n.hash(hasher),
      ASTNode::AST_Bool(n) => n.hash(hasher),
      ASTNode::FailHint(n) => n.hash(hasher),
      ASTNode::AST_Property(n) => n.hash(hasher),
      ASTNode::NonTermSymbol(n) => n.hash(hasher),
      ASTNode::RegexWordChar(n) => n.hash(hasher),
      ASTNode::NonTerminal_Terminal_Symbol(n) => n.hash(hasher),
      ASTNode::AST_Member(n) => n.hash(hasher),
      ASTNode::AST_NamedReference(n) => n.hash(hasher),
      ASTNode::RegexNewLineChar(n) => n.hash(hasher),
      ASTNode::ReduceRaw(n) => n.hash(hasher),
      ASTNode::TokenGroupRules(n) => n.hash(hasher),
      ASTNode::DefaultMatch(n) => n.hash(hasher),
      ASTNode::NotEmptySet(n) => n.hash(hasher),
      ASTNode::Statement(n) => n.hash(hasher),
      ASTNode::AST_Token(n) => n.hash(hasher),
      ASTNode::AST_StringLiteral(n) => n.hash(hasher),
      ASTNode::RegexRepeatCount(n) => n.hash(hasher),
      ASTNode::GrammarDefinition(n) => n.hash(hasher),
      ASTNode::IgnoreScope(n) => n.hash(hasher),
      ASTNode::RegexRepeat(n) => n.hash(hasher),
      ASTNode::REgexMatch(n) => n.hash(hasher),
      ASTNode::Grouped_Rules(n) => n.hash(hasher),
      ASTNode::RegexExcludeMatch(n) => n.hash(hasher),
      ASTNode::AST_BoolLiteral(n) => n.hash(hasher),
      ASTNode::RegexEscapedChar(n) => n.hash(hasher),
      ASTNode::Precedence(n) => n.hash(hasher),
      ASTNode::NonTermMatch(n) => n.hash(hasher),
      ASTNode::ClassSymbol(n) => n.hash(hasher),
      ASTNode::RegexOptional(n) => n.hash(hasher),
      ASTNode::AST_Vector(n) => n.hash(hasher),
      ASTNode::ImportPreamble(n) => n.hash(hasher),
      ASTNode::NontermDeclaration(n) => n.hash(hasher),
      ASTNode::RegexOptionalRepeat(n) => n.hash(hasher),
      ASTNode::RegexDigitChar(n) => n.hash(hasher),
      ASTNode::SetTokenId(n) => n.hash(hasher),
      ASTNode::RegexSymbol(n) => n.hash(hasher),
      ASTNode::AST_String(n) => n.hash(hasher),
      ASTNode::AnnotatedSymbol(n) => n.hash(hasher),
      ASTNode::AST_NumberLiteral(n) => n.hash(hasher),
      ASTNode::RegexStart(n) => n.hash(hasher),
      ASTNode::SetTokenLen(n) => n.hash(hasher),
      ASTNode::TerminalToken(n) => n.hash(hasher),
      ASTNode::IgnorePreamble(n) => n.hash(hasher),
      ASTNode::RegexWildCard(n) => n.hash(hasher),
      ASTNode::ExportPreamble(n) => n.hash(hasher),
      ASTNode::EOFSymbol(n) => n.hash(hasher),
      ASTNode::RegexSpaceChar(n) => n.hash(hasher),
      ASTNode::TokenSymbol(n) => n.hash(hasher),
      ASTNode::AST_Statement(n) => n.hash(hasher),
      ASTNode::AST_Struct(n) => n.hash(hasher),
      ASTNode::List_Rules(n) => n.hash(hasher),
      ASTNode::NonTermSymbolImportSymbol(n) => n.hash(hasher),
      ASTNode::ProductionMatches(n) => n.hash(hasher),
      ASTNode::NamePreamble(n) => n.hash(hasher),
      ASTNode::AST_IndexReference(n) => n.hash(hasher),
      ASTNode::TermMatch(n) => n.hash(hasher),
      ASTNode::RegexGroup(n) => n.hash(hasher),
      ASTNode::TerminalMatches(n) => n.hash(hasher),
      ASTNode::AST_TrimmedReference(n) => n.hash(hasher),
      _=>{}
    }
  }
}*/

#[derive( Clone, Debug, Default )]
pub struct Pop<Token:Tk>{pub tok: Token,pub count: u32,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Pop(self) -> Option<Box<Pop<Token>>> {match self {ASTNode::Pop(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Pop<Token>>> for ASTNode<Token>{fn from(value: Box<Pop<Token>>) -> Self {Self::Pop(value)}}

#[derive( Clone, Debug, Default )]
pub struct Rule<Token:Tk>{pub tok: Token,pub symbols: /*17*/Vec<__TEMP___Values<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Rule(self) -> Option<Box<Rule<Token>>> {match self {ASTNode::Rule(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Rule<Token>>> for ASTNode<Token>{fn from(value: Box<Rule<Token>>) -> Self {Self::Rule(value)}}

#[derive( Clone, Debug, Default )]
pub struct Push<Token:Tk>{pub tok: Token,pub name: String,pub nonterminal: Box<NonTermSymbol<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Push(self) -> Option<Box<Push<Token>>> {match self {ASTNode::Push(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Push<Token>>> for ASTNode<Token>{fn from(value: Box<Push<Token>>) -> Self {Self::Push(value)}}

#[derive( Clone, Debug, Default )]
pub struct Peek<Token:Tk>{pub tok: Token,pub skip: bool,pub ptr_type: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Peek(self) -> Option<Box<Peek<Token>>> {match self {ASTNode::Peek(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Peek<Token>>> for ASTNode<Token>{fn from(value: Box<Peek<Token>>) -> Self {Self::Peek(value)}}

#[derive( Clone, Debug, Default )]
pub struct Fork<Token:Tk>{pub tok: Token,pub paths: Vec<Box<Goto<Token>>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Fork(self) -> Option<Box<Fork<Token>>> {match self {ASTNode::Fork(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Fork<Token>>> for ASTNode<Token>{fn from(value: Box<Fork<Token>>) -> Self {Self::Fork(value)}}

#[derive( Clone, Debug, Default )]
pub struct Fail<Token:Tk>{pub tok: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Fail(self) -> Option<Box<Fail<Token>>> {match self {ASTNode::Fail(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Fail<Token>>> for ASTNode<Token>{fn from(value: Box<Fail<Token>>) -> Self {Self::Fail(value)}}

#[derive( Clone, Debug, Default )]
pub struct Goto<Token:Tk>{pub tok: Token,pub name: String,pub nonterminal: Box<NonTermSymbol<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Goto(self) -> Option<Box<Goto<Token>>> {match self {ASTNode::Goto(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Goto<Token>>> for ASTNode<Token>{fn from(value: Box<Goto<Token>>) -> Self {Self::Goto(value)}}

#[derive( Clone, Debug, Default )]
pub struct Char<Token:Tk>{pub val: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Char(self) -> Option<Box<Char<Token>>> {match self {ASTNode::Char(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Char<Token>>> for ASTNode<Token>{fn from(value: Box<Char<Token>>) -> Self {Self::Char(value)}}

#[derive( Clone, Debug, Default )]
pub struct Pass<Token:Tk>{pub tok: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Pass(self) -> Option<Box<Pass<Token>>> {match self {ASTNode::Pass(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Pass<Token>>> for ASTNode<Token>{fn from(value: Box<Pass<Token>>) -> Self {Self::Pass(value)}}

#[derive( Clone, Debug, Default )]
pub struct Init<Token:Tk>{pub expression: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Init(self) -> Option<Box<Init<Token>>> {match self {ASTNode::Init(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Init<Token>>> for ASTNode<Token>{fn from(value: Box<Init<Token>>) -> Self {Self::Init(value)}}

#[derive( Clone, Debug, Default )]
pub struct Range{pub end_trim: i32,pub start_trim: i32,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Range(self) -> Option<Box<Range>> {match self {ASTNode::Range(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Range>> for ASTNode<Token>{fn from(value: Box<Range>) -> Self {Self::Range(value)}}

#[derive( Clone, Debug, Default )]
pub struct State<Token:Tk>{pub id: NontermValues<Token>,pub tok: Token,pub catches: bool,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_State(self) -> Option<Box<State<Token>>> {match self {ASTNode::State(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<State<Token>>> for ASTNode<Token>{fn from(value: Box<State<Token>>) -> Self {Self::State(value)}}

#[derive( Clone, Debug, Default )]
pub struct Gotos<Token:Tk>{pub pushes: Vec<Box<Push<Token>>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Gotos(self) -> Option<Box<Gotos<Token>>> {match self {ASTNode::Gotos(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Gotos<Token>>> for ASTNode<Token>{fn from(value: Box<Gotos<Token>>) -> Self {Self::Gotos(value)}}

#[derive( Clone, Debug, Default )]
pub struct Reset<Token:Tk>{pub tok: Token,pub ptr_type: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Reset(self) -> Option<Box<Reset<Token>>> {match self {ASTNode::Reset(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Reset<Token>>> for ASTNode<Token>{fn from(value: Box<Reset<Token>>) -> Self {Self::Reset(value)}}

#[derive( Clone, Debug, Default )]
pub struct Shift<Token:Tk>{pub tok: Token,pub skip: bool,pub ptr_type: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Shift(self) -> Option<Box<Shift<Token>>> {match self {ASTNode::Shift(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Shift<Token>>> for ASTNode<Token>{fn from(value: Box<Shift<Token>>) -> Self {Self::Shift(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_I8<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_I8(self) -> Option<Box<AST_I8<Token>>> {match self {ASTNode::AST_I8(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_I8<Token>>> for ASTNode<Token>{fn from(value: Box<AST_I8<Token>>) -> Self {Self::AST_I8(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_U8<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_U8(self) -> Option<Box<AST_U8<Token>>> {match self {ASTNode::AST_U8(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_U8<Token>>> for ASTNode<Token>{fn from(value: Box<AST_U8<Token>>) -> Self {Self::AST_U8(value)}}

#[derive( Clone, Debug, Default )]
pub struct Reduce<Token:Tk>{pub tok: Token,pub len: u32,pub ast: BodyValues<Token>,pub nonterminal: Box<NonTermSymbol<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Reduce(self) -> Option<Box<Reduce<Token>>> {match self {ASTNode::Reduce(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Reduce<Token>>> for ASTNode<Token>{fn from(value: Box<Reduce<Token>>) -> Self {Self::Reduce(value)}}

#[derive( Clone, Debug, Default )]
pub struct Accept<Token:Tk>{pub tok: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Accept(self) -> Option<Box<Accept<Token>>> {match self {ASTNode::Accept(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Accept<Token>>> for ASTNode<Token>{fn from(value: Box<Accept<Token>>) -> Self {Self::Accept(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_F32<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_F32(self) -> Option<Box<AST_F32<Token>>> {match self {ASTNode::AST_F32(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_F32<Token>>> for ASTNode<Token>{fn from(value: Box<AST_F32<Token>>) -> Self {Self::AST_F32(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_I32<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_I32(self) -> Option<Box<AST_I32<Token>>> {match self {ASTNode::AST_I32(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_I32<Token>>> for ASTNode<Token>{fn from(value: Box<AST_I32<Token>>) -> Self {Self::AST_I32(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_U32<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_U32(self) -> Option<Box<AST_U32<Token>>> {match self {ASTNode::AST_U32(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_U32<Token>>> for ASTNode<Token>{fn from(value: Box<AST_U32<Token>>) -> Self {Self::AST_U32(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_F64<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_F64(self) -> Option<Box<AST_F64<Token>>> {match self {ASTNode::AST_F64(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_F64<Token>>> for ASTNode<Token>{fn from(value: Box<AST_F64<Token>>) -> Self {Self::AST_F64(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_I64<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_I64(self) -> Option<Box<AST_I64<Token>>> {match self {ASTNode::AST_I64(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_I64<Token>>> for ASTNode<Token>{fn from(value: Box<AST_I64<Token>>) -> Self {Self::AST_I64(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_U64<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_U64(self) -> Option<Box<AST_U64<Token>>> {match self {ASTNode::AST_U64(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_U64<Token>>> for ASTNode<Token>{fn from(value: Box<AST_U64<Token>>) -> Self {Self::AST_U64(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_F16<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_F16(self) -> Option<Box<AST_F16<Token>>> {match self {ASTNode::AST_F16(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_F16<Token>>> for ASTNode<Token>{fn from(value: Box<AST_F16<Token>>) -> Self {Self::AST_F16(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_I16<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_I16(self) -> Option<Box<AST_I16<Token>>> {match self {ASTNode::AST_I16(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_I16<Token>>> for ASTNode<Token>{fn from(value: Box<AST_I16<Token>>) -> Self {Self::AST_I16(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_U16<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_U16(self) -> Option<Box<AST_U16<Token>>> {match self {ASTNode::AST_U16(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_U16<Token>>> for ASTNode<Token>{fn from(value: Box<AST_U16<Token>>) -> Self {Self::AST_U16(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Sub<Token:Tk>{pub tok: Token,pub left: ExprValues<Token>,pub right: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Sub(self) -> Option<Box<AST_Sub<Token>>> {match self {ASTNode::AST_Sub(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Sub<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Sub<Token>>) -> Self {Self::AST_Sub(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Add<Token:Tk>{pub tok: Token,pub left: ExprValues<Token>,pub right: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Add(self) -> Option<Box<AST_Add<Token>>> {match self {ASTNode::AST_Add(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Add<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Add<Token>>) -> Self {Self::AST_Add(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Mod<Token:Tk>{pub tok: Token,pub left: ExprValues<Token>,pub right: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Mod(self) -> Option<Box<AST_Mod<Token>>> {match self {ASTNode::AST_Mod(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Mod<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Mod<Token>>) -> Self {Self::AST_Mod(value)}}

#[derive( Clone, Debug, Default )]
pub struct SetLine<Token:Tk>{pub tok: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_SetLine(self) -> Option<Box<SetLine<Token>>> {match self {ASTNode::SetLine(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<SetLine<Token>>> for ASTNode<Token>{fn from(value: Box<SetLine<Token>>) -> Self {Self::SetLine(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Neg<Token:Tk>{pub tok: Token,pub expr: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Neg(self) -> Option<Box<AST_Neg<Token>>> {match self {ASTNode::AST_Neg(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Neg<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Neg<Token>>) -> Self {Self::AST_Neg(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Mul<Token:Tk>{pub tok: Token,pub left: ExprValues<Token>,pub right: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Mul(self) -> Option<Box<AST_Mul<Token>>> {match self {ASTNode::AST_Mul(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Mul<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Mul<Token>>) -> Self {Self::AST_Mul(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Map<Token:Tk>{pub tok: Token,pub val: ExprValues<Token>,pub key: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Map(self) -> Option<Box<AST_Map<Token>>> {match self {ASTNode::AST_Map(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Map<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Map<Token>>) -> Self {Self::AST_Map(value)}}

#[derive( Clone, Debug, Default )]
pub struct Matches<Token:Tk>{
  pub tok: Token,
  pub mode: String,
  pub scanner: String,
  pub matches: /*21*/Vec<Generic_match_block_list_2Values>,
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Matches(self) -> Option<Box<Matches<Token>>> {match self {ASTNode::Matches(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Matches<Token>>> for ASTNode<Token>{fn from(value: Box<Matches<Token>>) -> Self {Self::Matches(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Div<Token:Tk>{pub tok: Token,pub left: ExprValues<Token>,pub right: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Div(self) -> Option<Box<AST_Div<Token>>> {match self {ASTNode::AST_Div(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Div<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Div<Token>>) -> Self {Self::AST_Div(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Pow<Token:Tk>{pub tok: Token,pub left: ExprValues<Token>,pub right: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Pow(self) -> Option<Box<AST_Pow<Token>>> {match self {ASTNode::AST_Pow(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Pow<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Pow<Token>>) -> Self {Self::AST_Pow(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_F128<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_F128(self) -> Option<Box<AST_F128<Token>>> {match self {ASTNode::AST_F128(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_F128<Token>>> for ASTNode<Token>{fn from(value: Box<AST_F128<Token>>) -> Self {Self::AST_F128(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_U128<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_U128(self) -> Option<Box<AST_U128<Token>>> {match self {ASTNode::AST_U128(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_U128<Token>>> for ASTNode<Token>{fn from(value: Box<AST_U128<Token>>) -> Self {Self::AST_U128(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexEnd{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexEnd(self) -> Option<RegexEnd> {match self {ASTNode::RegexEnd(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexEnd> for ASTNode<Token>{fn from(value: RegexEnd) -> Self {Self::RegexEnd(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Flag<Token:Tk>{pub ty: String,pub tok: Token,pub val: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Flag(self) -> Option<Box<AST_Flag<Token>>> {match self {ASTNode::AST_Flag(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Flag<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Flag<Token>>) -> Self {Self::AST_Flag(value)}}

#[derive( Clone, Debug, Default )]
pub struct IntMatch{pub vals: Vec<u64>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_IntMatch(self) -> Option<Box<IntMatch>> {match self {ASTNode::IntMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<IntMatch>> for ASTNode<Token>{fn from(value: Box<IntMatch>) -> Self {Self::IntMatch(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Bool<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Bool(self) -> Option<Box<AST_Bool<Token>>> {match self {ASTNode::AST_Bool(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Bool<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Bool<Token>>) -> Self {Self::AST_Bool(value)}}

#[derive( Clone, Debug, Default )]
pub struct FailHint{pub message: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_FailHint(self) -> Option<Box<FailHint>> {match self {ASTNode::FailHint(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<FailHint>> for ASTNode<Token>{fn from(value: Box<FailHint>) -> Self {Self::FailHint(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Property<Token:Tk>{pub id: String,pub tok: Token,pub value: ExprValues<Token>,pub named_reference: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Property(self) -> Option<Box<AST_Property<Token>>> {match self {ASTNode::AST_Property(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Property<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Property<Token>>) -> Self {Self::AST_Property(value)}}

#[derive( Clone, Debug, Default )]
pub struct NonTermSymbol<Token:Tk>{pub tok: Token,pub val: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NonTermSymbol(self) -> Option<Box<NonTermSymbol<Token>>> {match self {ASTNode::NonTermSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NonTermSymbol<Token>>> for ASTNode<Token>{fn from(value: Box<NonTermSymbol<Token>>) -> Self {Self::NonTermSymbol(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexWordChar{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexWordChar(self) -> Option<RegexWordChar> {match self {ASTNode::RegexWordChar(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexWordChar> for ASTNode<Token>{fn from(value: RegexWordChar) -> Self {Self::RegexWordChar(value)}}

#[derive( Clone, Debug, Default )]
pub struct NonTerminal_Terminal_Symbol<Token:Tk>{pub tok: Token,pub nonterm: NontermValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NonTerminal_Terminal_Symbol(self) -> Option<Box<NonTerminal_Terminal_Symbol<Token>>> {match self {ASTNode::NonTerminal_Terminal_Symbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for ASTNode<Token>{
  fn from(value: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {Self::NonTerminal_Terminal_Symbol(value)}
}

#[derive( Clone, Debug, Default )]
pub struct AST_Member<Token:Tk>{pub property: Token,pub reference: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Member(self) -> Option<Box<AST_Member<Token>>> {match self {ASTNode::AST_Member(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Member<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Member<Token>>) -> Self {Self::AST_Member(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_NamedReference<Token:Tk>{pub tok: Token,pub value: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_NamedReference(self) -> Option<Box<AST_NamedReference<Token>>> {match self {ASTNode::AST_NamedReference(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_NamedReference<Token>>> for ASTNode<Token>{fn from(value: Box<AST_NamedReference<Token>>) -> Self {Self::AST_NamedReference(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexNewLineChar{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexNewLineChar(self) -> Option<RegexNewLineChar> {match self {ASTNode::RegexNewLineChar(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexNewLineChar> for ASTNode<Token>{fn from(value: RegexNewLineChar) -> Self {Self::RegexNewLineChar(value)}}

#[derive( Clone, Debug, Default )]
pub struct ReduceRaw<Token:Tk>{pub tok: Token,pub len: u32,pub rule_id: u32,pub nonterminal_id: u32,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ReduceRaw(self) -> Option<Box<ReduceRaw<Token>>> {match self {ASTNode::ReduceRaw(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<ReduceRaw<Token>>> for ASTNode<Token>{fn from(value: Box<ReduceRaw<Token>>) -> Self {Self::ReduceRaw(value)}}

#[derive( Clone, Debug, Default )]
pub struct TokenGroupRules<Token:Tk>{pub tok: Token,pub rules: Vec<Box<Rule<Token>>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_TokenGroupRules(self) -> Option<Box<TokenGroupRules<Token>>> {match self {ASTNode::TokenGroupRules(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<TokenGroupRules<Token>>> for ASTNode<Token>{fn from(value: Box<TokenGroupRules<Token>>) -> Self {Self::TokenGroupRules(value)}}

#[derive( Clone, Debug, Default )]
pub struct DefaultMatch{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_DefaultMatch(self) -> Option<DefaultMatch> {match self {ASTNode::DefaultMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<DefaultMatch> for ASTNode<Token>{fn from(value: DefaultMatch) -> Self {Self::DefaultMatch(value)}}

#[derive( Clone, Debug, Default )]
pub struct NotEmptySet<Token:Tk>{
  pub tok: Token,
  pub symbols: Vec<Box<AnnotatedSymbol<Token>>>,
  pub allow_empty: bool,
  pub unordered: bool,
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NotEmptySet(self) -> Option<Box<NotEmptySet<Token>>> {match self {ASTNode::NotEmptySet(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NotEmptySet<Token>>> for ASTNode<Token>{fn from(value: Box<NotEmptySet<Token>>) -> Self {Self::NotEmptySet(value)}}

#[derive( Clone, Debug, Default )]
pub struct Statement<Token:Tk>{
  pub pop: Box<Pop<Token>>,
  pub branch: Terminal_statementValues<Token>,
  pub non_branch: /*0*/Vec<Non_branch_statementValues<Token>>,
  pub transitive: Transitive_statementValues<Token>,
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Statement(self) -> Option<Box<Statement<Token>>> {match self {ASTNode::Statement(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Statement<Token>>> for ASTNode<Token>{fn from(value: Box<Statement<Token>>) -> Self {Self::Statement(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Token{pub range: Box<Range>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Token(self) -> Option<Box<AST_Token>> {match self {ASTNode::AST_Token(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Token>> for ASTNode<Token>{fn from(value: Box<AST_Token>) -> Self {Self::AST_Token(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_StringLiteral<Token:Tk>{pub tok: Token,pub value: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_StringLiteral(self) -> Option<Box<AST_StringLiteral<Token>>> {match self {ASTNode::AST_StringLiteral(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_StringLiteral<Token>>> for ASTNode<Token>{fn from(value: Box<AST_StringLiteral<Token>>) -> Self {Self::AST_StringLiteral(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexRepeatCount<Token:Tk>{pub val: Modified_sequence_memberValues<Token>,pub min: u32,pub max: u32,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexRepeatCount(self) -> Option<Box<RegexRepeatCount<Token>>> {match self {ASTNode::RegexRepeatCount(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexRepeatCount<Token>>> for ASTNode<Token>{fn from(value: Box<RegexRepeatCount<Token>>) -> Self {Self::RegexRepeatCount(value)}}

#[derive( Clone, Debug, Default )]
pub struct GrammarDefinition<Token:Tk>{
  pub preambles: /*14*/Vec<PreambleValues<Token>>,
  pub declarations: /*12*/Vec<Nonterm_declarations_list_1Values<Token>>,
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_GrammarDefinition(self) -> Option<Box<GrammarDefinition<Token>>> {match self {ASTNode::GrammarDefinition(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<GrammarDefinition<Token>>> for ASTNode<Token>{fn from(value: Box<GrammarDefinition<Token>>) -> Self {Self::GrammarDefinition(value)}}

#[derive( Clone, Debug, Default )]
pub struct IgnoreScope<Token:Tk>{pub clause: Box<IgnorePreamble<Token>>,pub definitions: Vec<Box<NontermDeclaration<Token>>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_IgnoreScope(self) -> Option<Box<IgnoreScope<Token>>> {match self {ASTNode::IgnoreScope(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<IgnoreScope<Token>>> for ASTNode<Token>{fn from(value: Box<IgnoreScope<Token>>) -> Self {Self::IgnoreScope(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexRepeat<Token:Tk>{pub val: Modified_sequence_memberValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexRepeat(self) -> Option<Box<RegexRepeat<Token>>> {match self {ASTNode::RegexRepeat(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexRepeat<Token>>> for ASTNode<Token>{fn from(value: Box<RegexRepeat<Token>>) -> Self {Self::RegexRepeat(value)}}

#[derive( Clone, Debug, Default )]
pub struct REgexMatch<Token:Tk>{pub vals: /*10*/Vec<Modified_sequence_memberValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_REgexMatch(self) -> Option<Box<REgexMatch<Token>>> {match self {ASTNode::REgexMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<REgexMatch<Token>>> for ASTNode<Token>{fn from(value: Box<REgexMatch<Token>>) -> Self {Self::REgexMatch(value)}}

#[derive( Clone, Debug, Default )]
pub struct Grouped_Rules<Token:Tk>{pub tok: Token,pub rules: Vec<Box<Rule<Token>>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Grouped_Rules(self) -> Option<Box<Grouped_Rules<Token>>> {match self {ASTNode::Grouped_Rules(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Grouped_Rules<Token>>> for ASTNode<Token>{fn from(value: Box<Grouped_Rules<Token>>) -> Self {Self::Grouped_Rules(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexExcludeMatch<Token:Tk>{pub vals: /*10*/Vec<Modified_sequence_memberValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexExcludeMatch(self) -> Option<Box<RegexExcludeMatch<Token>>> {match self {ASTNode::RegexExcludeMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexExcludeMatch<Token>>> for ASTNode<Token>{fn from(value: Box<RegexExcludeMatch<Token>>) -> Self {Self::RegexExcludeMatch(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_BoolLiteral{pub value: bool,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_BoolLiteral(self) -> Option<Box<AST_BoolLiteral>> {match self {ASTNode::AST_BoolLiteral(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_BoolLiteral>> for ASTNode<Token>{fn from(value: Box<AST_BoolLiteral>) -> Self {Self::AST_BoolLiteral(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexEscapedChar<Token:Tk>{pub val: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexEscapedChar(self) -> Option<Box<RegexEscapedChar<Token>>> {match self {ASTNode::RegexEscapedChar(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexEscapedChar<Token>>> for ASTNode<Token>{fn from(value: Box<RegexEscapedChar<Token>>) -> Self {Self::RegexEscapedChar(value)}}

#[derive( Clone, Debug, Default )]
pub struct Precedence{pub tok_prec: u32,pub sym_prec: u32,pub is_keyword: bool,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Precedence(self) -> Option<Box<Precedence>> {match self {ASTNode::Precedence(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Precedence>> for ASTNode<Token>{fn from(value: Box<Precedence>) -> Self {Self::Precedence(value)}}

#[derive( Clone, Debug, Default )]
pub struct NonTermMatch<Token:Tk>{pub sym: Box<NonTermSymbol<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NonTermMatch(self) -> Option<Box<NonTermMatch<Token>>> {match self {ASTNode::NonTermMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NonTermMatch<Token>>> for ASTNode<Token>{fn from(value: Box<NonTermMatch<Token>>) -> Self {Self::NonTermMatch(value)}}

#[derive( Clone, Debug, Default )]
pub struct ClassSymbol<Token:Tk>{pub tok: Token,pub val: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ClassSymbol(self) -> Option<Box<ClassSymbol<Token>>> {match self {ASTNode::ClassSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<ClassSymbol<Token>>> for ASTNode<Token>{fn from(value: Box<ClassSymbol<Token>>) -> Self {Self::ClassSymbol(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexOptional<Token:Tk>{pub val: Modified_sequence_memberValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexOptional(self) -> Option<Box<RegexOptional<Token>>> {match self {ASTNode::RegexOptional(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexOptional<Token>>> for ASTNode<Token>{fn from(value: Box<RegexOptional<Token>>) -> Self {Self::RegexOptional(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Vector<Token:Tk>{pub tok: Token,pub initializer: /*2*/Vec<ExprValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Vector(self) -> Option<Box<AST_Vector<Token>>> {match self {ASTNode::AST_Vector(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Vector<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Vector<Token>>) -> Self {Self::AST_Vector(value)}}

#[derive( Clone, Debug, Default )]
pub struct ImportPreamble<Token:Tk>{pub uri: String,pub tok: Token,pub reference: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ImportPreamble(self) -> Option<Box<ImportPreamble<Token>>> {match self {ASTNode::ImportPreamble(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<ImportPreamble<Token>>> for ASTNode<Token>{fn from(value: Box<ImportPreamble<Token>>) -> Self {Self::ImportPreamble(value)}}

#[derive( Clone, Debug, Default )]
pub struct NontermDeclaration<Token:Tk>{pub tok: Token,pub rules: Vec<Box<Rule<Token>>>,pub name_sym: Box<NonTermSymbol<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NontermDeclaration(self) -> Option<Box<NontermDeclaration<Token>>> {match self {ASTNode::NontermDeclaration(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NontermDeclaration<Token>>> for ASTNode<Token>{fn from(value: Box<NontermDeclaration<Token>>) -> Self {Self::NontermDeclaration(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexOptionalRepeat<Token:Tk>{pub val: Modified_sequence_memberValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexOptionalRepeat(self) -> Option<Box<RegexOptionalRepeat<Token>>> {match self {ASTNode::RegexOptionalRepeat(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexOptionalRepeat<Token>>> for ASTNode<Token>{fn from(value: Box<RegexOptionalRepeat<Token>>) -> Self {Self::RegexOptionalRepeat(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexDigitChar{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexDigitChar(self) -> Option<RegexDigitChar> {match self {ASTNode::RegexDigitChar(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexDigitChar> for ASTNode<Token>{fn from(value: RegexDigitChar) -> Self {Self::RegexDigitChar(value)}}

#[derive( Clone, Debug, Default )]
pub struct SetTokenId<Token:Tk>{pub id: u32,pub tok: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_SetTokenId(self) -> Option<Box<SetTokenId<Token>>> {match self {ASTNode::SetTokenId(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<SetTokenId<Token>>> for ASTNode<Token>{fn from(value: Box<SetTokenId<Token>>) -> Self {Self::SetTokenId(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexSymbol<Token:Tk>{pub val: /*8*/Vec<Modified_sequence_memberValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexSymbol(self) -> Option<Box<RegexSymbol<Token>>> {match self {ASTNode::RegexSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexSymbol<Token>>> for ASTNode<Token>{fn from(value: Box<RegexSymbol<Token>>) -> Self {Self::RegexSymbol(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_String<Token:Tk>{pub tok: Token,pub initializer: Box<Init<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_String(self) -> Option<Box<AST_String<Token>>> {match self {ASTNode::AST_String(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_String<Token>>> for ASTNode<Token>{fn from(value: Box<AST_String<Token>>) -> Self {Self::AST_String(value)}}

#[derive( Clone, Debug, Default )]
pub struct AnnotatedSymbol<Token:Tk>{
  pub tok: Token,
  pub symbol: TokenValues<Token>,
  pub precedence: Box<Precedence>,
  pub is_optional: bool,
  pub reference: String,
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AnnotatedSymbol(self) -> Option<Box<AnnotatedSymbol<Token>>> {match self {ASTNode::AnnotatedSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AnnotatedSymbol<Token>>> for ASTNode<Token>{fn from(value: Box<AnnotatedSymbol<Token>>) -> Self {Self::AnnotatedSymbol(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_NumberLiteral{pub value: f64,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_NumberLiteral(self) -> Option<Box<AST_NumberLiteral>> {match self {ASTNode::AST_NumberLiteral(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_NumberLiteral>> for ASTNode<Token>{fn from(value: Box<AST_NumberLiteral>) -> Self {Self::AST_NumberLiteral(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexStart{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexStart(self) -> Option<RegexStart> {match self {ASTNode::RegexStart(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexStart> for ASTNode<Token>{fn from(value: RegexStart) -> Self {Self::RegexStart(value)}}

#[derive( Clone, Debug, Default )]
pub struct SetTokenLen{pub id: u32,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_SetTokenLen(self) -> Option<Box<SetTokenLen>> {match self {ASTNode::SetTokenLen(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<SetTokenLen>> for ASTNode<Token>{fn from(value: Box<SetTokenLen>) -> Self {Self::SetTokenLen(value)}}

#[derive( Clone, Debug, Default )]
pub struct TerminalToken<Token:Tk>{pub tok: Token,pub val: String,pub is_exclusive: bool,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_TerminalToken(self) -> Option<Box<TerminalToken<Token>>> {match self {ASTNode::TerminalToken(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<TerminalToken<Token>>> for ASTNode<Token>{fn from(value: Box<TerminalToken<Token>>) -> Self {Self::TerminalToken(value)}}

#[derive( Clone, Debug, Default )]
pub struct IgnorePreamble<Token:Tk>{pub symbols: /*6*/Vec<TokenValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_IgnorePreamble(self) -> Option<Box<IgnorePreamble<Token>>> {match self {ASTNode::IgnorePreamble(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<IgnorePreamble<Token>>> for ASTNode<Token>{fn from(value: Box<IgnorePreamble<Token>>) -> Self {Self::IgnorePreamble(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexWildCard{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexWildCard(self) -> Option<RegexWildCard> {match self {ASTNode::RegexWildCard(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexWildCard> for ASTNode<Token>{fn from(value: RegexWildCard) -> Self {Self::RegexWildCard(value)}}

#[derive( Clone, Debug, Default )]
pub struct ExportPreamble<Token:Tk>{pub production: NontermValues<Token>,pub reference: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ExportPreamble(self) -> Option<Box<ExportPreamble<Token>>> {match self {ASTNode::ExportPreamble(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<ExportPreamble<Token>>> for ASTNode<Token>{fn from(value: Box<ExportPreamble<Token>>) -> Self {Self::ExportPreamble(value)}}

#[derive( Clone, Debug, Default )]
pub struct EOFSymbol<Token:Tk>{pub tok: Token,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_EOFSymbol(self) -> Option<Box<EOFSymbol<Token>>> {match self {ASTNode::EOFSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<EOFSymbol<Token>>> for ASTNode<Token>{fn from(value: Box<EOFSymbol<Token>>) -> Self {Self::EOFSymbol(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexSpaceChar{}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexSpaceChar(self) -> Option<RegexSpaceChar> {match self {ASTNode::RegexSpaceChar(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<RegexSpaceChar> for ASTNode<Token>{fn from(value: RegexSpaceChar) -> Self {Self::RegexSpaceChar(value)}}

#[derive( Clone, Debug, Default )]
pub struct TokenSymbol<Token:Tk>{pub tok: Token,pub val: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_TokenSymbol(self) -> Option<Box<TokenSymbol<Token>>> {match self {ASTNode::TokenSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<TokenSymbol<Token>>> for ASTNode<Token>{fn from(value: Box<TokenSymbol<Token>>) -> Self {Self::TokenSymbol(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Statement<Token:Tk>{pub tok: Token,pub expression: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Statement(self) -> Option<Box<AST_Statement<Token>>> {match self {ASTNode::AST_Statement(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Statement<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Statement<Token>>) -> Self {Self::AST_Statement(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_Struct<Token:Tk>{pub ty: String,pub tok: Token,pub props: /*18*/Vec<Struct_propValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_Struct(self) -> Option<Box<AST_Struct<Token>>> {match self {ASTNode::AST_Struct(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_Struct<Token>>> for ASTNode<Token>{fn from(value: Box<AST_Struct<Token>>) -> Self {Self::AST_Struct(value)}}

#[derive( Clone, Debug, Default )]
pub struct List_Rules<Token:Tk>{
  pub tok: Token,
  pub min: u32,
  pub max: u32,
  pub symbol: TokenValues<Token>,
  pub terminal_symbol: TokenValues<Token>,
}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_List_Rules(self) -> Option<Box<List_Rules<Token>>> {match self {ASTNode::List_Rules(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<List_Rules<Token>>> for ASTNode<Token>{fn from(value: Box<List_Rules<Token>>) -> Self {Self::List_Rules(value)}}

#[derive( Clone, Debug, Default )]
pub struct NonTermSymbolImportSymbol<Token:Tk>{pub tok: Token,pub name: String,pub module: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NonTermSymbolImportSymbol(self) -> Option<Box<NonTermSymbolImportSymbol<Token>>> {match self {ASTNode::NonTermSymbolImportSymbol(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NonTermSymbolImportSymbol<Token>>> for ASTNode<Token>{
  fn from(value: Box<NonTermSymbolImportSymbol<Token>>) -> Self {Self::NonTermSymbolImportSymbol(value)}
}

#[derive( Clone, Debug, Default )]
pub struct ProductionMatches<Token:Tk>{pub matches: /*23*/Vec<Nonterminal_match_block_list_1Values<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ProductionMatches(self) -> Option<Box<ProductionMatches<Token>>> {match self {ASTNode::ProductionMatches(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<ProductionMatches<Token>>> for ASTNode<Token>{fn from(value: Box<ProductionMatches<Token>>) -> Self {Self::ProductionMatches(value)}}

#[derive( Clone, Debug, Default )]
pub struct NamePreamble{pub name: String,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_NamePreamble(self) -> Option<Box<NamePreamble>> {match self {ASTNode::NamePreamble(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<NamePreamble>> for ASTNode<Token>{fn from(value: Box<NamePreamble>) -> Self {Self::NamePreamble(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_IndexReference<Token:Tk>{pub tok: Token,pub value: i64,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_IndexReference(self) -> Option<Box<AST_IndexReference<Token>>> {match self {ASTNode::AST_IndexReference(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_IndexReference<Token>>> for ASTNode<Token>{fn from(value: Box<AST_IndexReference<Token>>) -> Self {Self::AST_IndexReference(value)}}

#[derive( Clone, Debug, Default )]
pub struct TermMatch<Token:Tk>{pub sym: TokenValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_TermMatch(self) -> Option<Box<TermMatch<Token>>> {match self {ASTNode::TermMatch(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<TermMatch<Token>>> for ASTNode<Token>{fn from(value: Box<TermMatch<Token>>) -> Self {Self::TermMatch(value)}}

#[derive( Clone, Debug, Default )]
pub struct RegexGroup<Token:Tk>{pub val: /*8*/Vec<Modified_sequence_memberValues<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_RegexGroup(self) -> Option<Box<RegexGroup<Token>>> {match self {ASTNode::RegexGroup(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<RegexGroup<Token>>> for ASTNode<Token>{fn from(value: Box<RegexGroup<Token>>) -> Self {Self::RegexGroup(value)}}

#[derive( Clone, Debug, Default )]
pub struct TerminalMatches<Token:Tk>{pub matches: /*24*/Vec<Terminal_match_block_list_1Values<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_TerminalMatches(self) -> Option<Box<TerminalMatches<Token>>> {match self {ASTNode::TerminalMatches(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<TerminalMatches<Token>>> for ASTNode<Token>{fn from(value: Box<TerminalMatches<Token>>) -> Self {Self::TerminalMatches(value)}}

#[derive( Clone, Debug, Default )]
pub struct AST_TrimmedReference<Token:Tk>{pub tok: Token,pub range: Box<Range>,pub reference: ExprValues<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_AST_TrimmedReference(self) -> Option<Box<AST_TrimmedReference<Token>>> {match self {ASTNode::AST_TrimmedReference(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<AST_TrimmedReference<Token>>> for ASTNode<Token>{fn from(value: Box<AST_TrimmedReference<Token>>) -> Self {Self::AST_TrimmedReference(value)}}
  

fn rule_0/*ir::state*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_State().unwrap_unchecked() };
  out.into()
}

fn rule_1/*ast::body*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_BodyValues().unwrap_unchecked() };
  out.into()
}

fn rule_2/*grammar::declaration*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_GrammarDefinition().unwrap_unchecked() };
  out.into()
}

fn rule_3/*{ t_State, id, statement, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = std::mem::take(&mut nodes[0]);
  let id = unsafe{ id.into_NontermValues().unwrap_unchecked() };
  
  let tok = nterm_tok.clone();
  
  let catches = Default::default();
  
  ASTNode::State(Box::new(State{id,tok,catches,}))
}

fn rule_4/*{ t_State, catches:true, id, statement, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = std::mem::take(&mut nodes[0]);
  let id = unsafe{ id.into_NontermValues().unwrap_unchecked() };
  
  let tok = nterm_tok.clone();
  
  let catches = false;
  
  ASTNode::State(Box::new(State{id,tok,catches,}))
}

fn rule_5/*struct*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Struct().unwrap_unchecked() };
  out.into()
}

fn rule_6/*flag*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Flag().unwrap_unchecked() };
  out.into()
}

fn rule_7/*{ t_AST_Statement, expression:$1, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let expression = std::mem::take(&mut nodes[0]);
  let expression = unsafe{ expression.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Statement(Box::new(AST_Statement{tok,expression,}))
}

fn rule_8/*{ t_AST_Statement, expression:$2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let expression = std::mem::take(&mut nodes[1]);
  let expression = unsafe{ expression.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Statement(Box::new(AST_Statement{tok,expression,}))
}

fn rule_9/*{ t_GrammarDefinition, preambles: $pre, declarations:$nt }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let preambles = std::mem::take(&mut nodes[0]);
  let preambles = unsafe{ preambles.into_VecPreambleValues().unwrap_unchecked() };
  
  let declarations = std::mem::take(&mut nodes[1]);
  let declarations = unsafe{ declarations.into_VecNonterm_declarations_list_1Values().unwrap_unchecked() };
  
  ASTNode::GrammarDefinition(Box::new(GrammarDefinition{preambles,declarations,}))
}

fn rule_10/*{ t_GrammarDefinition, preambles: $pre, declarations:$nt }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let preambles = Default::default();
  
  let declarations = std::mem::take(&mut nodes[0]);
  let declarations = unsafe{ declarations.into_VecNonterm_declarations_list_1Values().unwrap_unchecked() };
  
  ASTNode::GrammarDefinition(Box::new(GrammarDefinition{preambles,declarations,}))
}

fn rule_11/*pre::preamble*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_PreambleValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_12/*pre::preamble(*)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_PreambleValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecPreambleValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_13/*nonterm_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NonTermSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_14/*import_nonterm_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NonTermSymbolImportSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_15/*{ t_Grouped_Rules, rules:$2,  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let rules = std::mem::take(&mut nodes[1]);
  let rules = unsafe{ rules.into_VecRule().unwrap_unchecked() };
  
  ASTNode::Grouped_Rules(Box::new(Grouped_Rules{tok,rules,}))
}

fn rule_16/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[4]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = std::mem::take(&mut nodes[6]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_17/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = std::mem::take(&mut nodes[4]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = Default::default();
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_18/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = std::mem::take(&mut nodes[4]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_19/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = std::mem::take(&mut nodes[2]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = Default::default();
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_20/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[4]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = Default::default();
  
  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_21/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = Default::default();
  
  let non_branch = Default::default();
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_22/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = Default::default();
  
  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_23/*{ t_Statement, transitive, non_branch, pop, branch }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = Default::default();
  
  let non_branch = Default::default();
  
  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe{ transitive.into_Transitive_statementValues().unwrap_unchecked() };
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_24/*{ t_Statement, non_branch, branch, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = std::mem::take(&mut nodes[4]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_25/*{ t_Statement, non_branch, branch, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = std::mem::take(&mut nodes[2]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_26/*{ t_Statement, non_branch, branch, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = Default::default();
  
  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_27/*{ t_Statement, non_branch, branch, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = Default::default();
  
  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe{ non_branch.into_VecNon_branch_statementValues().unwrap_unchecked() };
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_28/*{ t_Statement, branch, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[0]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = std::mem::take(&mut nodes[2]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = Default::default();
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_29/*{ t_Statement, branch, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = Default::default();
  
  let branch = std::mem::take(&mut nodes[0]);
  let branch = unsafe{ branch.into_Terminal_statementValues().unwrap_unchecked() };
  
  let non_branch = Default::default();
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_30/*{ t_Statement, pop }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pop = std::mem::take(&mut nodes[0]);
  let pop = unsafe{ pop.into_Pop().unwrap_unchecked() };
  
  let branch = Default::default();
  
  let non_branch = Default::default();
  
  let transitive = Default::default();
  
  ASTNode::Statement(Box::new(Statement{pop,branch,non_branch,transitive,}))
}

fn rule_31/*non_branch_statement*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Non_branch_statementValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_32/*non_branch_statement(+"then")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_Non_branch_statementValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNon_branch_statementValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_33/*"then" non_branch_statement(+"then")^non_branch*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_VecNon_branch_statementValues().unwrap_unchecked() };
  out.into()
}

fn rule_34/*"then" pop^pop*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_Pop().unwrap_unchecked() };
  out.into()
}

fn rule_35/*"then" branch_statement^branch*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_Terminal_statementValues().unwrap_unchecked() };
  out.into()
}

fn rule_36/*non_branch_statement*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Non_branch_statementValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_37/*non_branch_statement(+"then")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_Non_branch_statementValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNon_branch_statementValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_38/*"then" pop^pop*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_Pop().unwrap_unchecked() };
  out.into()
}

fn rule_39/*"then" branch_statement^branch*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_Terminal_statementValues().unwrap_unchecked() };
  out.into()
}

fn rule_40/*pop^pop "then"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[1].clone();ASTNode::Token(out)}

fn rule_41/*{ t_AST_Struct, ty: str($t), props:$3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let ty = nodes[1].clone();
  let ty = ty.to_token().unwrap();
  let ty = ty.to_string();
  
  let tok = nterm_tok.clone();
  
  let props = std::mem::take(&mut nodes[3]);
  let props = unsafe{ props.into_VecStruct_propValues().unwrap_unchecked() };
  
  ASTNode::AST_Struct(Box::new(AST_Struct{ty,tok,props,}))
}

fn rule_42/*{ t_AST_Struct, ty: str($t), props:$3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let ty = nodes[1].clone();
  let ty = ty.to_token().unwrap();
  let ty = ty.to_string();
  
  let tok = nterm_tok.clone();
  
  let props = Default::default();
  
  ASTNode::AST_Struct(Box::new(AST_Struct{ty,tok,props,}))
}

fn rule_43/*struct_prop*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Struct_propValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_44/*struct_prop(+",")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_Struct_propValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecStruct_propValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_45/*"," struct_prop(+",")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_VecStruct_propValues().unwrap_unchecked() };
  out.into()
}

fn rule_46/*{ t_AST_Flag, ty: str($1), val:$3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let ty = nodes[0].clone();
  let ty = ty.to_token().unwrap();
  let ty = ty.to_string();
  
  let tok = nterm_tok.clone();
  
  let val = nodes[2].clone();
  let val = val.to_token().unwrap();
  
  ASTNode::AST_Flag(Box::new(AST_Flag{ty,tok,val,}))
}

fn rule_47/*expr*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_48/*export_clause*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExportPreamble().unwrap_unchecked() };
  out.into()
}

fn rule_49/*import_clause*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ImportPreamble().unwrap_unchecked() };
  out.into()
}

fn rule_50/*name_clause*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NamePreamble().unwrap_unchecked() };
  out.into()
}

fn rule_51/*ignore_clause*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_IgnorePreamble().unwrap_unchecked() };
  out.into()
}

fn rule_52/*( ignore_scope | nt::nonterm_declaration )+*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecNonterm_declarations_list_1Values().unwrap_unchecked() };
  out.into()
}

fn rule_53/*ignore_scope*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_IgnoreScope().unwrap_unchecked() };
  out.into()
}

fn rule_54/*nt::nonterm_declaration*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NontermDeclaration().unwrap_unchecked() };
  out.into()
}

fn rule_55/*( ignore_scope | nt::nonterm_declaration )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Nonterm_declarations_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_56/*( ignore_scope | nt::nonterm_declaration )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Nonterm_declarations_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_57/*( ignore_scope | nt::nonterm_declaration )+*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Nonterm_declarations_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNonterm_declarations_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_58/*( ignore_scope | nt::nonterm_declaration )+*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Nonterm_declarations_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNonterm_declarations_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_59/*{ t_NonTermSymbol, val:str($1), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = nodes[0].clone();
  let val = val.to_token().unwrap();
  let val = val.to_string();
  
  ASTNode::NonTermSymbol(Box::new(NonTermSymbol{tok,val,}))
}

fn rule_60/*{ t_NonTermSymbolImportSymbol, module:str($1), name:str($3), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let name = nodes[2].clone();
  let name = name.to_token().unwrap();
  let name = name.to_string();
  
  let module = nodes[0].clone();
  let module = module.to_token().unwrap();
  let module = module.to_string();
  
  ASTNode::NonTermSymbolImportSymbol(Box::new(NonTermSymbolImportSymbol{tok,name,module,}))
}

fn rule_61/*rule(+"|")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecRule().unwrap_unchecked() };
  out.into()
}

fn rule_62/*rule*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Rule().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_63/*rule(+"|")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_Rule().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecRule().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_64/*{ t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let rule_id = nodes[7].clone();
  let rule_id = rule_id.to_token().unwrap();
  let rule_id: u32 = rule_id.to_string().parse().unwrap_or_default();
  
  let nonterminal_id = nodes[4].clone();
  let nonterminal_id = nonterminal_id.to_token().unwrap();
  let nonterminal_id: u32 = nonterminal_id.to_string().parse().unwrap_or_default();
  
  ASTNode::ReduceRaw(Box::new(ReduceRaw{tok,len,rule_id,nonterminal_id,}))
}

fn rule_65/*{ t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let rule_id = nodes[5].clone();
  let rule_id = rule_id.to_token().unwrap();
  let rule_id: u32 = rule_id.to_string().parse().unwrap_or_default();
  
  let nonterminal_id = nodes[2].clone();
  let nonterminal_id = nonterminal_id.to_token().unwrap();
  let nonterminal_id: u32 = nonterminal_id.to_string().parse().unwrap_or_default();
  
  ASTNode::ReduceRaw(Box::new(ReduceRaw{tok,len,rule_id,nonterminal_id,}))
}

fn rule_66/*{ t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let rule_id = nodes[5].clone();
  let rule_id = rule_id.to_token().unwrap();
  let rule_id: u32 = rule_id.to_string().parse().unwrap_or_default();
  
  let nonterminal_id = nodes[4].clone();
  let nonterminal_id = nonterminal_id.to_token().unwrap();
  let nonterminal_id: u32 = nonterminal_id.to_string().parse().unwrap_or_default();
  
  ASTNode::ReduceRaw(Box::new(ReduceRaw{tok,len,rule_id,nonterminal_id,}))
}

fn rule_67/*{ t_ReduceRaw, len: u32($2), rule_id: u32($int), nonterminal_id: u32($4), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let rule_id = nodes[3].clone();
  let rule_id = rule_id.to_token().unwrap();
  let rule_id: u32 = rule_id.to_string().parse().unwrap_or_default();
  
  let nonterminal_id = nodes[2].clone();
  let nonterminal_id = nonterminal_id.to_token().unwrap();
  let nonterminal_id: u32 = nonterminal_id.to_string().parse().unwrap_or_default();
  
  ASTNode::ReduceRaw(Box::new(ReduceRaw{tok,len,rule_id,nonterminal_id,}))
}

fn rule_68/*{ t_Reduce, len: u32($2), ast,  nonterminal, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let ast = std::mem::take(&mut nodes[6]);
  let ast = unsafe{ ast.into_BodyValues().unwrap_unchecked() };
  
  let nonterminal = std::mem::take(&mut nodes[4]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Reduce(Box::new(Reduce{tok,len,ast,nonterminal,}))
}

fn rule_69/*{ t_Reduce, len: u32($2), ast,  nonterminal, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let ast = std::mem::take(&mut nodes[4]);
  let ast = unsafe{ ast.into_BodyValues().unwrap_unchecked() };
  
  let nonterminal = std::mem::take(&mut nodes[2]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Reduce(Box::new(Reduce{tok,len,ast,nonterminal,}))
}

fn rule_70/*{ t_Reduce, len: u32($2), ast,  nonterminal, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let ast = Default::default();
  
  let nonterminal = std::mem::take(&mut nodes[4]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Reduce(Box::new(Reduce{tok,len,ast,nonterminal,}))
}

fn rule_71/*{ t_Reduce, len: u32($2), ast,  nonterminal, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();
  
  let ast = Default::default();
  
  let nonterminal = std::mem::take(&mut nodes[2]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Reduce(Box::new(Reduce{tok,len,ast,nonterminal,}))
}

fn rule_72/*{ t_SetTokenId, id: u32($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = nodes[1].clone();
  let id = id.to_token().unwrap();
  let id: u32 = id.to_string().parse().unwrap_or_default();
  
  let tok = nterm_tok.clone();
  
  ASTNode::SetTokenId(Box::new(SetTokenId{id,tok,}))
}

fn rule_73/*{ t_SetTokenLen, id: u32($2) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = nodes[1].clone();
  let id = id.to_token().unwrap();
  let id: u32 = id.to_string().parse().unwrap_or_default();
  
  ASTNode::SetTokenLen(Box::new(SetTokenLen{id,}))
}

fn rule_74/*{ t_SetLine, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let tok = nterm_tok.clone();ASTNode::SetLine(Box::new(SetLine{tok,}))}

fn rule_75/*"symbols" "to"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[1].clone();ASTNode::Token(out)}

fn rule_76/*"with" "rule"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[1].clone();ASTNode::Token(out)}

fn rule_77/*"symbols" "to"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[1].clone();ASTNode::Token(out)}

fn rule_78/*":ast" ast::body^ast*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_BodyValues().unwrap_unchecked() };
  out.into()
}

fn rule_79/*{ t_Pop, count: u32($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let count = nodes[1].clone();
  let count = count.to_token().unwrap();
  let count: u32 = count.to_string().parse().unwrap_or_default();
  
  ASTNode::Pop(Box::new(Pop{tok,count,}))
}

fn rule_80/*match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Terminal_statementValues().unwrap_unchecked() };
  out.into()
}

fn rule_81/*goto_sequence*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Gotos().unwrap_unchecked() };
  out.into()
}

fn rule_82/*terminal_statement*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Terminal_statementValues().unwrap_unchecked() };
  out.into()
}

fn rule_83/*{ t_Shift, ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = tokens[1].clone();
  let skip = skip.len()>0;
  
  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Shift(Box::new(Shift{tok,skip,ptr_type,}))
}

fn rule_84/*{ t_Shift, ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = tokens[1].clone();
  let skip = skip.len()>0;
  
  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Shift(Box::new(Shift{tok,skip,ptr_type,}))
}

fn rule_85/*{ t_Shift, ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = false;
  
  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Shift(Box::new(Shift{tok,skip,ptr_type,}))
}

fn rule_86/*{ t_Shift, ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = false;
  
  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Shift(Box::new(Shift{tok,skip,ptr_type,}))
}

fn rule_87/*{ t_Peek,  ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = tokens[1].clone();
  let skip = skip.len()>0;
  
  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Peek(Box::new(Peek{tok,skip,ptr_type,}))
}

fn rule_88/*{ t_Peek,  ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = tokens[1].clone();
  let skip = skip.len()>0;
  
  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Peek(Box::new(Peek{tok,skip,ptr_type,}))
}

fn rule_89/*{ t_Peek,  ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = false;
  
  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Peek(Box::new(Peek{tok,skip,ptr_type,}))
}

fn rule_90/*{ t_Peek,  ptr_type:str($3), skip:bool($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let skip = false;
  
  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Peek(Box::new(Peek{tok,skip,ptr_type,}))
}

fn rule_91/*{ t_Reset, ptr_type:str($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Reset(Box::new(Reset{tok,ptr_type,}))
}

fn rule_92/*{ t_Reset, ptr_type:str($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();
  
  ASTNode::Reset(Box::new(Reset{tok,ptr_type,}))
}

fn rule_93/*"tok"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_94/*"char"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_95/*"tok"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_96/*"char"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_97/*"tok"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_98/*"char"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_99/*{ t_AST_Property, id:str($1), value:$3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = nodes[0].clone();
  let id = id.to_token().unwrap();
  let id = id.to_string();
  
  let tok = nterm_tok.clone();
  
  let value = std::mem::take(&mut nodes[2]);
  let value = unsafe{ value.into_ExprValues().unwrap_unchecked() };
  
  let named_reference = Default::default();
  
  ASTNode::AST_Property(Box::new(AST_Property{id,tok,value,named_reference,}))
}

fn rule_100/*{ t_AST_Property, id:str($1), value:$3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = nodes[0].clone();
  let id = id.to_token().unwrap();
  let id = id.to_string();
  
  let tok = nterm_tok.clone();
  
  /* Struct*/let value = nodes[2].clone();
  let value = unsafe{ value.into_ExprValues().unwrap_unchecked() };
  
  let named_reference = Default::default();
  
  ASTNode::AST_Property(Box::new(AST_Property{id,tok,value,named_reference,}))
}

fn rule_101/*{ t_AST_Property, id:str($1), named_reference: str($1), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let id = nodes[0].clone();
  let id = id.to_token().unwrap();
  let id = id.to_string();
  
  let tok = nterm_tok.clone();
  
  let value = Default::default();
  
  let named_reference = nodes[0].clone();
  let named_reference = named_reference.to_token().unwrap();
  let named_reference = named_reference.to_string();
  
  ASTNode::AST_Property(Box::new(AST_Property{id,tok,value,named_reference,}))
}

fn rule_102/*token*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Token().unwrap_unchecked() };
  out.into()
}

fn rule_103/*tk:( 't' "_"{:9999} ) identifier*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_104/*tk:( 'f' "_"{:9999} ) identifier*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_105/*prim::id*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[0].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_106/*{ t_AST_Add, left: $1, right: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe{ left.into_ExprValues().unwrap_unchecked() };
  
  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe{ right.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Add(Box::new(AST_Add{tok,left,right,}))
}

fn rule_107/*{ t_AST_Sub, left: $1, right: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe{ left.into_ExprValues().unwrap_unchecked() };
  
  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe{ right.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Sub(Box::new(AST_Sub{tok,left,right,}))
}

fn rule_108/*{ t_AST_Div, left: $1, right: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe{ left.into_ExprValues().unwrap_unchecked() };
  
  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe{ right.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Div(Box::new(AST_Div{tok,left,right,}))
}

fn rule_109/*{ t_AST_Mul, left: $1, right: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe{ left.into_ExprValues().unwrap_unchecked() };
  
  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe{ right.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Mul(Box::new(AST_Mul{tok,left,right,}))
}

fn rule_110/*{ t_AST_Mod, left: $1, right: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe{ left.into_ExprValues().unwrap_unchecked() };
  
  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe{ right.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Mod(Box::new(AST_Mod{tok,left,right,}))
}

fn rule_111/*{ t_AST_Pow, left: $1, right: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe{ left.into_ExprValues().unwrap_unchecked() };
  
  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe{ right.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Pow(Box::new(AST_Pow{tok,left,right,}))
}

fn rule_112/*{ t_AST_Neg, expr: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let expr = std::mem::take(&mut nodes[1]);
  let expr = unsafe{ expr.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Neg(Box::new(AST_Neg{tok,expr,}))
}

fn rule_113/*"(" expr^expr ")"        :ast $expr*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_114/*term{9}*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_115/*{ t_ExportPreamble, production:$2, reference:$3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let production = std::mem::take(&mut nodes[1]);
  let production = unsafe{ production.into_NontermValues().unwrap_unchecked() };
  
  let reference = nodes[3].clone();
  let reference = reference.to_token().unwrap();
  
  ASTNode::ExportPreamble(Box::new(ExportPreamble{production,reference,}))
}

fn rule_116/*{ t_ExportPreamble, production:$2, reference:$3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let production = std::mem::take(&mut nodes[1]);
  let production = unsafe{ production.into_NontermValues().unwrap_unchecked() };
  
  let reference = nodes[3].clone();
  let reference = reference.to_token().unwrap();
  
  ASTNode::ExportPreamble(Box::new(ExportPreamble{production,reference,}))
}

fn rule_117/*{ t_ExportPreamble, production:$2, reference:$3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let production = std::mem::take(&mut nodes[1]);
  let production = unsafe{ production.into_NontermValues().unwrap_unchecked() };
  
  let reference = Default::default();
  
  ASTNode::ExportPreamble(Box::new(ExportPreamble{production,reference,}))
}

fn rule_118/*"AS"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_119/*"as"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_120/*( "AS" | "as" ) prim::id*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_121/*( "AS" | "as" ) prim::id*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_122/*{ t_ImportPreamble, uri: str($2), reference:str($5), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let uri = std::mem::take(&mut nodes[1]);
  let uri = unsafe{ uri.into_VecToken().unwrap_unchecked() };
  let uri = Token::from_slice(&uri).to_string();
  
  let tok = nterm_tok.clone();
  
  let reference = nodes[4].clone();
  let reference = reference.to_token().unwrap();
  let reference = reference.to_string();
  
  ASTNode::ImportPreamble(Box::new(ImportPreamble{uri,tok,reference,}))
}

fn rule_123/*{ t_ImportPreamble, uri: str($2), reference:str($5), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let uri = std::mem::take(&mut nodes[1]);
  let uri = unsafe{ uri.into_VecToken().unwrap_unchecked() };
  let uri = Token::from_slice(&uri).to_string();
  
  let tok = nterm_tok.clone();
  
  let reference = nodes[4].clone();
  let reference = reference.to_token().unwrap();
  let reference = reference.to_string();
  
  ASTNode::ImportPreamble(Box::new(ImportPreamble{uri,tok,reference,}))
}

fn rule_124/*c:id*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_125/*c:sym*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_126/*c:num*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_127/*( c:id | c:sym | c:num  )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = tokens[0].clone();
  
  let out = vec![ out_0 ];
  ASTNode::VecToken(out)
}

fn rule_128/*( c:id | c:sym | c:num  )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = tokens[0].clone();
  
  let out = vec![ out_0 ];
  ASTNode::VecToken(out)
}

fn rule_129/*( c:id | c:sym | c:num  )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = tokens[0].clone();
  
  let out = vec![ out_0 ];
  ASTNode::VecToken(out)
}

fn rule_130/*( c:id | c:sym | c:num  )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecToken().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::VecToken(out)
}

fn rule_131/*( c:id | c:sym | c:num  )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecToken().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::VecToken(out)
}

fn rule_132/*( c:id | c:sym | c:num  )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecToken().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::VecToken(out)
}

fn rule_133/*"AS"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_134/*"as"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_135/*{ t_NamePreamble, name: str($2) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let name = nodes[1].clone();
  let name = name.to_token().unwrap();
  let name = name.to_string();
  
  ASTNode::NamePreamble(Box::new(NamePreamble{name,}))
}

fn rule_136/*{ t_IgnorePreamble, symbols: $3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let symbols = std::mem::take(&mut nodes[2]);
  let symbols = unsafe{ symbols.into_VecTokenValues().unwrap_unchecked() };
  
  ASTNode::IgnorePreamble(Box::new(IgnorePreamble{symbols,}))
}

fn rule_137/*sym::terminal*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_TokenValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_138/*sym::terminal(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_TokenValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecTokenValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_139/*{ t_IgnoreScope, clause:$clause, definitions: $defs }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let clause = std::mem::take(&mut nodes[0]);
  let clause = unsafe{ clause.into_IgnorePreamble().unwrap_unchecked() };
  
  let definitions = std::mem::take(&mut nodes[2]);
  let definitions = unsafe{ definitions.into_VecNontermDeclaration().unwrap_unchecked() };
  
  ASTNode::IgnoreScope(Box::new(IgnoreScope{clause,definitions,}))
}

fn rule_140/*{ t_NontermDeclaration, name_sym:$n, rules: $r, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let rules = std::mem::take(&mut nodes[3]);
  let rules = unsafe{ rules.into_VecRule().unwrap_unchecked() };
  
  let name_sym = std::mem::take(&mut nodes[1]);
  let name_sym = unsafe{ name_sym.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::NontermDeclaration(Box::new(NontermDeclaration{tok,rules,name_sym,}))
}

fn rule_141/*tk:( ( "-" | "_" | c:id ) ( c:id | '_' | '-' | c:num )(*) )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_142/*{ t_Rule, symbols:$s, ast:$a, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbols = std::mem::take(&mut nodes[0]);
  let symbols = unsafe{ symbols.into_Vec__TEMP___Values().unwrap_unchecked() };
  
  ASTNode::Rule(Box::new(Rule{tok,symbols,}))
}

fn rule_143/*sym::annotated_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AnnotatedSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_144/*not_empty*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NotEmptySet().unwrap_unchecked() };
  out.into()
}

fn rule_145/*( sym::annotated_symbol | not_empty )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into___TEMP___Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_146/*( sym::annotated_symbol | not_empty )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into___TEMP___Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_147/*( sym::annotated_symbol | not_empty )(*)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into___TEMP___Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_Vec__TEMP___Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_148/*( sym::annotated_symbol | not_empty )(*)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into___TEMP___Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_Vec__TEMP___Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_149/*[ ( sym::annotated_symbol | not_empty )(*)^s end_of_input?^eoi ]  :ast [$s, $eoi]*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into___TEMP___Values().unwrap_unchecked() };
  let out_l_r = std::mem::take(&mut nodes[0]);
  let out_l_r = unsafe{ out_l_r.into_Vec__TEMP___Values().unwrap_unchecked() };
  /*0*/let out_l_l: /*17*/Vec<__TEMP___Values<Token>> = vec![];
  let mut out_l = out_l_l;
  out_l.extend(out_l_r);
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_150/*[ ( sym::annotated_symbol | not_empty )(*)^s end_of_input?^eoi ]  :ast [$s, $eoi]*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};/*0*/let out: Vec<Box<EOFSymbol<Token>>> = vec![];out.into()}

fn rule_151/*[ ( sym::annotated_symbol | not_empty )(*)^s end_of_input?^eoi ]  :ast [$s, $eoi]*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[0]);
  let out_r = unsafe{ out_r.into_Vec__TEMP___Values().unwrap_unchecked() };
  /*0*/let out_l: /*17*/Vec<__TEMP___Values<Token>> = vec![];
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_152/*tk:( c:num(+) )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_153/*generic_match_block*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Matches().unwrap_unchecked() };
  out.into()
}

fn rule_154/*nonterminal_match_block*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ProductionMatches().unwrap_unchecked() };
  out.into()
}

fn rule_155/*terminal_match_block*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_TerminalMatches().unwrap_unchecked() };
  out.into()
}

fn rule_156/*{ t_Gotos, pushes: $1, goto }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pushes = std::mem::take(&mut nodes[0]);
  let pushes = unsafe{ pushes.into_VecPush().unwrap_unchecked() };
  
  ASTNode::Gotos(Box::new(Gotos{pushes,}))
}

fn rule_157/*{ t_Gotos, goto }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pushes = Default::default();
  
  ASTNode::Gotos(Box::new(Gotos{pushes,}))
}

fn rule_158/*{ t_Gotos, fork }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let pushes = Default::default();
  
  ASTNode::Gotos(Box::new(Gotos{pushes,}))
}

fn rule_159/*goto_push*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Push().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_160/*goto_push(+"then")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_Push().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecPush().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_161/*{ t_Fail, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let tok = nterm_tok.clone();ASTNode::Fail(Box::new(Fail{tok,}))}

fn rule_162/*{ t_Pass, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let tok = nterm_tok.clone();ASTNode::Pass(Box::new(Pass{tok,}))}

fn rule_163/*{ t_Accept, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let tok = nterm_tok.clone();ASTNode::Accept(Box::new(Accept{tok,}))}

fn rule_164/*{ t_AST_Token, range: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe{ range.into_Range().unwrap_unchecked() };
  
  ASTNode::AST_Token(Box::new(AST_Token{range,}))
}

fn rule_165/*{ t_AST_Token, range: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe{ range.into_Range().unwrap_unchecked() };
  
  ASTNode::AST_Token(Box::new(AST_Token{range,}))
}

fn rule_166/*{ t_AST_Token, range: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe{ range.into_Range().unwrap_unchecked() };
  
  ASTNode::AST_Token(Box::new(AST_Token{range,}))
}

fn rule_167/*{ t_AST_Token, range: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let range = Default::default();
  
  ASTNode::AST_Token(Box::new(AST_Token{range,}))
}

fn rule_168/*{ t_AST_Token, range: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let range = Default::default();
  
  ASTNode::AST_Token(Box::new(AST_Token{range,}))
}

fn rule_169/*{ t_AST_Token, range: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let range = Default::default();
  
  ASTNode::AST_Token(Box::new(AST_Token{range,}))
}

fn rule_170/*"tk"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_171/*"tok"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_172/*"token"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_173/*member*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_174/*map*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Map().unwrap_unchecked() };
  out.into()
}

fn rule_175/*vector*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Vector().unwrap_unchecked() };
  out.into()
}

fn rule_176/*string*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_String().unwrap_unchecked() };
  out.into()
}

fn rule_177/*bool*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Bool().unwrap_unchecked() };
  out.into()
}

fn rule_178/*number*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NumberValues().unwrap_unchecked() };
  out.into()
}

fn rule_179/*literal*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NumberValues().unwrap_unchecked() };
  out.into()
}

fn rule_180/*token*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_TokenValues().unwrap_unchecked() };
  out.into()
}

fn rule_181/*token_non_terminal*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NonTerminal_Terminal_Symbol().unwrap_unchecked() };
  out.into()
}

fn rule_182/*class*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ClassSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_183/*{ t_TokenGroupRules, rules:$2,  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let rules = std::mem::take(&mut nodes[1]);
  let rules = unsafe{ rules.into_VecRule().unwrap_unchecked() };
  
  ASTNode::TokenGroupRules(Box::new(TokenGroupRules{tok,rules,}))
}

fn rule_184/*nonterm_declaration(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecNontermDeclaration().unwrap_unchecked() };
  out.into()
}

fn rule_185/*nonterm_declaration*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_NontermDeclaration().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_186/*nonterm_declaration(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_NontermDeclaration().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNontermDeclaration().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_187/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[3]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[1].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_188/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len()>0;
  
  let reference:String = String::default();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_189/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = false;
  
  let reference = tokens[1].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_190/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = false;
  
  let reference:String = String::default();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_191/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = Default::default();
  
  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[1].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_192/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = Default::default();
  
  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len()>0;
  
  let reference:String = String::default();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_193/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = Default::default();
  
  let is_optional = false;
  
  let reference = tokens[1].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_194/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = Default::default();
  
  let is_optional = false;
  
  let reference:String = String::default();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_195/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[3].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[1].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_196/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len()>0;
  
  let reference:String = String::default();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_197/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[3]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[2].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_198/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = Default::default();
  
  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[2].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_199/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[3].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_200/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = false;
  
  let reference = tokens[2].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_201/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[3].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[2].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_202/*{ t_AnnotatedSymbol, precedence:$p, symbol:$s, is_optional:bool($o), reference:str($r), tok  }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe{ precedence.into_Precedence().unwrap_unchecked() };
  
  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len()>0;
  
  let reference = tokens[3].clone();
  let reference = reference.to_string();
  
  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol{tok,symbol,precedence,is_optional,reference,}))
}

fn rule_203/*{ t_NotEmptySet, unordered: bool($o), symbols:$s, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbols = std::mem::take(&mut nodes[1]);
  let symbols = unsafe{ symbols.into_VecAnnotatedSymbol().unwrap_unchecked() };
  
  let allow_empty = Default::default();
  
  let unordered = tokens[3].clone();
  let unordered = unordered.len()>0;
  
  ASTNode::NotEmptySet(Box::new(NotEmptySet{tok,symbols,allow_empty,unordered,}))
}

fn rule_204/*{ t_NotEmptySet, unordered: bool($o), symbols:$s, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbols = std::mem::take(&mut nodes[1]);
  let symbols = unsafe{ symbols.into_VecAnnotatedSymbol().unwrap_unchecked() };
  
  let allow_empty = Default::default();
  
  let unordered = false;
  
  ASTNode::NotEmptySet(Box::new(NotEmptySet{tok,symbols,allow_empty,unordered,}))
}

fn rule_205/*{ t_NotEmptySet, unordered: bool($o), allow_empty: bool($o), symbols:$s, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let symbols = std::mem::take(&mut nodes[1]);
  let symbols = unsafe{ symbols.into_VecAnnotatedSymbol().unwrap_unchecked() };
  
  let allow_empty = tokens[2].clone();
  let allow_empty = allow_empty.len()>0;
  
  let unordered = tokens[2].clone();
  let unordered = unordered.len()>0;
  
  ASTNode::NotEmptySet(Box::new(NotEmptySet{tok,symbols,allow_empty,unordered,}))
}

fn rule_206/*{ t_EOFSymbol, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  ASTNode::EOFSymbol(Box::new(EOFSymbol{tok,}))
}

fn rule_207/*{ t_Matches, mode: str($id), matches:$m, scanner, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let mode = nodes[2].clone();
  let mode = mode.to_token().unwrap();
  let mode = mode.to_string();
  
  let scanner = std::mem::take(&mut nodes[3]);
  let scanner = unsafe{ scanner.into_String().unwrap_unchecked() };
  
  let matches = std::mem::take(&mut nodes[4]);
  let matches = unsafe{ matches.into_VecGeneric_match_block_list_2Values().unwrap_unchecked() };
  
  ASTNode::Matches(Box::new(Matches{tok,mode,scanner,matches,}))
}

fn rule_208/*{ t_Matches, mode: str($id), matches:$m, scanner, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let mode = nodes[2].clone();
  let mode = mode.to_token().unwrap();
  let mode = mode.to_string();
  
  let scanner = Default::default();
  
  let matches = std::mem::take(&mut nodes[3]);
  let matches = unsafe{ matches.into_VecGeneric_match_block_list_2Values().unwrap_unchecked() };
  
  ASTNode::Matches(Box::new(Matches{tok,mode,scanner,matches,}))
}

fn rule_209/*":" prim::id :ast str($2)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  let out = out.to_string();
  out.into()
}

fn rule_210/*int_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_IntMatch().unwrap_unchecked() };
  out.into()
}

fn rule_211/*default_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_DefaultMatch().unwrap_unchecked() };
  out.into()
}

fn rule_212/*hint*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_FailHint().unwrap_unchecked() };
  out.into()
}

fn rule_213/*( int_match | default_match | hint )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Generic_match_block_list_2Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_214/*( int_match | default_match | hint )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Generic_match_block_list_2Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_215/*( int_match | default_match | hint )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Generic_match_block_list_2Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_216/*( int_match | default_match | hint )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Generic_match_block_list_2Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecGeneric_match_block_list_2Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_217/*( int_match | default_match | hint )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Generic_match_block_list_2Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecGeneric_match_block_list_2Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_218/*( int_match | default_match | hint )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Generic_match_block_list_2Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecGeneric_match_block_list_2Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_219/*int_match :ast [$1]*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};/*0*/let out: Vec<Box<IntMatch>> = vec![];out.into()}

fn rule_220/*"{" ( int_match | default_match | hint )(+) "}" :ast $2*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_VecGeneric_match_block_list_2Values().unwrap_unchecked() };
  out.into()
}

fn rule_221/*{ t_ProductionMatches, matches:$m }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let matches = std::mem::take(&mut nodes[3]);
  let matches = unsafe{ matches.into_VecNonterminal_match_block_list_1Values().unwrap_unchecked() };
  
  ASTNode::ProductionMatches(Box::new(ProductionMatches{matches,}))
}

fn rule_222/*nonterminal_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NonTermMatch().unwrap_unchecked() };
  out.into()
}

fn rule_223/*hint*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_FailHint().unwrap_unchecked() };
  out.into()
}

fn rule_224/*default_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_DefaultMatch().unwrap_unchecked() };
  out.into()
}

fn rule_225/*( nonterminal_match | hint | default_match )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Nonterminal_match_block_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_226/*( nonterminal_match | hint | default_match )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Nonterminal_match_block_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_227/*( nonterminal_match | hint | default_match )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Nonterminal_match_block_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_228/*( nonterminal_match | hint | default_match )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Nonterminal_match_block_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNonterminal_match_block_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_229/*( nonterminal_match | hint | default_match )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Nonterminal_match_block_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNonterminal_match_block_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_230/*( nonterminal_match | hint | default_match )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Nonterminal_match_block_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecNonterminal_match_block_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_231/*nonterminal_match :ast [$1]*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};/*0*/let out: Vec<Box<NonTermMatch<Token>>> = vec![];out.into()}

fn rule_232/*"{" ( nonterminal_match | hint | default_match )(+) "}" :ast $2*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_VecNonterminal_match_block_list_1Values().unwrap_unchecked() };
  out.into()
}

fn rule_233/*{ t_TerminalMatches, matches:$m }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let matches = std::mem::take(&mut nodes[3]);
  let matches = unsafe{ matches.into_VecTerminal_match_block_list_1Values().unwrap_unchecked() };
  
  ASTNode::TerminalMatches(Box::new(TerminalMatches{matches,}))
}

fn rule_234/*terminal_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_TermMatch().unwrap_unchecked() };
  out.into()
}

fn rule_235/*hint*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_FailHint().unwrap_unchecked() };
  out.into()
}

fn rule_236/*default_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_DefaultMatch().unwrap_unchecked() };
  out.into()
}

fn rule_237/*( terminal_match | hint | default_match )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Terminal_match_block_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_238/*( terminal_match | hint | default_match )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Terminal_match_block_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_239/*( terminal_match | hint | default_match )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*//* Struct*/let out_0 = nodes[0].clone();
  let out_0 = unsafe{ out_0.into_Terminal_match_block_list_1Values().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_240/*( terminal_match | hint | default_match )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Terminal_match_block_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecTerminal_match_block_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_241/*( terminal_match | hint | default_match )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Terminal_match_block_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecTerminal_match_block_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_242/*( terminal_match | hint | default_match )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* Struct*/let out_r = nodes[1].clone();
  let out_r = unsafe{ out_r.into_Terminal_match_block_list_1Values().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecTerminal_match_block_list_1Values().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_243/*terminal_match :ast [$1]*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};/*0*/let out: Vec<Box<TermMatch<Token>>> = vec![];out.into()}

fn rule_244/*"{" ( terminal_match | hint | default_match )(+) "}" :ast $2*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_VecTerminal_match_block_list_1Values().unwrap_unchecked() };
  out.into()
}

fn rule_245/*{ t_Push, nonterminal: $2, name:str($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let name = std::mem::take(&mut nodes[1]);
  let name = unsafe{ name.into_NonTermSymbol().unwrap_unchecked() };
  let name:String = String::default();
  
  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Push(Box::new(Push{tok,name,nonterminal,}))
}

fn rule_246/*{ t_Goto, nonterminal: $2, name:str($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let name = std::mem::take(&mut nodes[1]);
  let name = unsafe{ name.into_NonTermSymbol().unwrap_unchecked() };
  let name:String = String::default();
  
  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Goto(Box::new(Goto{tok,name,nonterminal,}))
}

fn rule_247/*{ t_Fork, paths: $3, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let paths = std::mem::take(&mut nodes[2]);
  let paths = unsafe{ paths.into_VecGoto().unwrap_unchecked() };
  
  ASTNode::Fork(Box::new(Fork{tok,paths,}))
}

fn rule_248/*{ t_Goto, nonterminal: $1, name:str($1), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let name = std::mem::take(&mut nodes[0]);
  let name = unsafe{ name.into_NonTermSymbol().unwrap_unchecked() };
  let name:String = String::default();
  
  let nonterminal = std::mem::take(&mut nodes[0]);
  let nonterminal = unsafe{ nonterminal.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::Goto(Box::new(Goto{tok,name,nonterminal,}))
}

fn rule_249/*( sym::nonterm_symbol :ast { t_Goto, nonterminal: $1, name:str($1), tok } )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Goto().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_250/*( sym::nonterm_symbol :ast { t_Goto, nonterminal: $1, name:str($1), tok } )(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_Goto().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecGoto().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_251/*{ t_Range, start_trim:i32($2), end_trim:i32($3) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let end_trim = nodes[3].clone();
  let end_trim = end_trim.to_token().unwrap();
  let end_trim: i32 = end_trim.to_string().parse().unwrap_or_default();
  
  let start_trim = nodes[1].clone();
  let start_trim = start_trim.to_token().unwrap();
  let start_trim: i32 = start_trim.to_string().parse().unwrap_or_default();
  
  ASTNode::Range(Box::new(Range{end_trim,start_trim,}))
}

fn rule_252/*{ t_Range, start_trim:i32($2), end_trim:i32($3) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let end_trim = 0;
  
  let start_trim = nodes[1].clone();
  let start_trim = start_trim.to_token().unwrap();
  let start_trim: i32 = start_trim.to_string().parse().unwrap_or_default();
  
  ASTNode::Range(Box::new(Range{end_trim,start_trim,}))
}

fn rule_253/*","  prim::int*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_254/*trimmed_reference*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_255/*{ t_AST_Member, reference:$1, property:$3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let property = nodes[2].clone();
  let property = property.to_token().unwrap();
  
  let reference = std::mem::take(&mut nodes[0]);
  let reference = unsafe{ reference.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Member(Box::new(AST_Member{property,reference,}))
}

fn rule_256/*{ t_AST_Map, key: $k, val: $v, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = std::mem::take(&mut nodes[4]);
  let val = unsafe{ val.into_ExprValues().unwrap_unchecked() };
  
  let key = std::mem::take(&mut nodes[2]);
  let key = unsafe{ key.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Map(Box::new(AST_Map{tok,val,key,}))
}

fn rule_257/*{ t_AST_Vector, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_VecExprValues().unwrap_unchecked() };
  
  ASTNode::AST_Vector(Box::new(AST_Vector{tok,initializer,}))
}

fn rule_258/*{ t_AST_Vector, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_Vector(Box::new(AST_Vector{tok,initializer,}))
}

fn rule_259/*expr*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_ExprValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_260/*expr(*",")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_ExprValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecExprValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_261/*{ t_AST_String, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_String(Box::new(AST_String{tok,initializer,}))
}

fn rule_262/*{ t_AST_String, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_String(Box::new(AST_String{tok,initializer,}))
}

fn rule_263/*{ t_AST_Bool,  initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_Bool(Box::new(AST_Bool{tok,initializer,}))
}

fn rule_264/*{ t_AST_Bool,  initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_Bool(Box::new(AST_Bool{tok,initializer,}))
}

fn rule_265/*{ t_AST_U8,  initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_U8(Box::new(AST_U8{tok,initializer,}))
}

fn rule_266/*{ t_AST_U8,  initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_U8(Box::new(AST_U8{tok,initializer,}))
}

fn rule_267/*{ t_AST_U16, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_U16(Box::new(AST_U16{tok,initializer,}))
}

fn rule_268/*{ t_AST_U16, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_U16(Box::new(AST_U16{tok,initializer,}))
}

fn rule_269/*{ t_AST_U32, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_U32(Box::new(AST_U32{tok,initializer,}))
}

fn rule_270/*{ t_AST_U32, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_U32(Box::new(AST_U32{tok,initializer,}))
}

fn rule_271/*{ t_AST_U64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_U64(Box::new(AST_U64{tok,initializer,}))
}

fn rule_272/*{ t_AST_U64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_U64(Box::new(AST_U64{tok,initializer,}))
}

fn rule_273/*{ t_AST_U128, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_U128(Box::new(AST_U128{tok,initializer,}))
}

fn rule_274/*{ t_AST_U128, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_U128(Box::new(AST_U128{tok,initializer,}))
}

fn rule_275/*{ t_AST_I8,  initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_I8(Box::new(AST_I8{tok,initializer,}))
}

fn rule_276/*{ t_AST_I8,  initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_I8(Box::new(AST_I8{tok,initializer,}))
}

fn rule_277/*{ t_AST_I16, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_I16(Box::new(AST_I16{tok,initializer,}))
}

fn rule_278/*{ t_AST_I16, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_I16(Box::new(AST_I16{tok,initializer,}))
}

fn rule_279/*{ t_AST_I32, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_I32(Box::new(AST_I32{tok,initializer,}))
}

fn rule_280/*{ t_AST_I32, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_I32(Box::new(AST_I32{tok,initializer,}))
}

fn rule_281/*{ t_AST_I64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_I64(Box::new(AST_I64{tok,initializer,}))
}

fn rule_282/*{ t_AST_I64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_I64(Box::new(AST_I64{tok,initializer,}))
}

fn rule_283/*{ t_AST_I64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_I64(Box::new(AST_I64{tok,initializer,}))
}

fn rule_284/*{ t_AST_I64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_I64(Box::new(AST_I64{tok,initializer,}))
}

fn rule_285/*{ t_AST_F16, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_F16(Box::new(AST_F16{tok,initializer,}))
}

fn rule_286/*{ t_AST_F16, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_F16(Box::new(AST_F16{tok,initializer,}))
}

fn rule_287/*{ t_AST_F32, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_F32(Box::new(AST_F32{tok,initializer,}))
}

fn rule_288/*{ t_AST_F32, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_F32(Box::new(AST_F32{tok,initializer,}))
}

fn rule_289/*{ t_AST_F64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_F64(Box::new(AST_F64{tok,initializer,}))
}

fn rule_290/*{ t_AST_F64, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_F64(Box::new(AST_F64{tok,initializer,}))
}

fn rule_291/*{ t_AST_F128, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe{ initializer.into_Init().unwrap_unchecked() };
  
  ASTNode::AST_F128(Box::new(AST_F128{tok,initializer,}))
}

fn rule_292/*{ t_AST_F128, initializer: $2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let initializer = Default::default();
  
  ASTNode::AST_F128(Box::new(AST_F128{tok,initializer,}))
}

fn rule_293/*{ t_AST_BoolLiteral, value: true }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let value = false;
  
  ASTNode::AST_BoolLiteral(Box::new(AST_BoolLiteral{value,}))
}

fn rule_294/*{ t_AST_BoolLiteral, value: false }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let value = false;
  
  ASTNode::AST_BoolLiteral(Box::new(AST_BoolLiteral{value,}))
}

fn rule_295/*{ t_AST_NumberLiteral, value: f64($1) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let value = nodes[0].clone();
  let value = value.to_token().unwrap();
  let value: f64 = value.to_string().parse().unwrap_or_default();
  
  ASTNode::AST_NumberLiteral(Box::new(AST_NumberLiteral{value,}))
}

fn rule_296/*{ t_AST_StringLiteral, value:str($1<1,1>), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let value = nodes[0].clone();
  let value = value.to_token().unwrap();
  let value = value.trim(1,1);
  let value = value.to_string();
  
  ASTNode::AST_StringLiteral(Box::new(AST_StringLiteral{tok,value,}))
}

fn rule_297/*{ t_TerminalToken, val:str(tok<1,1>), tok, is_exclusive:true }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = nterm_tok.clone();
  let val = val.trim(1,1);
  let val = val.to_string();
  
  let is_exclusive = false;
  
  ASTNode::TerminalToken(Box::new(TerminalToken{tok,val,is_exclusive,}))
}

fn rule_298/*{ t_TokenSymbol, val:str(tok<1,1>), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = nterm_tok.clone();
  let val = val.trim(1,1);
  let val = val.to_string();
  
  ASTNode::TokenSymbol(Box::new(TokenSymbol{tok,val,}))
}

fn rule_299/*{ t_RegexSymbol, val:$1 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::RegexSymbol(Box::new(RegexSymbol{val,}))
}

fn rule_300/*{ t_NonTerminal_Terminal_Symbol, nonterm:$2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let nonterm = std::mem::take(&mut nodes[1]);
  let nonterm = unsafe{ nonterm.into_NontermValues().unwrap_unchecked() };
  
  ASTNode::NonTerminal_Terminal_Symbol(Box::new(NonTerminal_Terminal_Symbol{tok,nonterm,}))
}

fn rule_301/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_302/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_303/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_304/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_305/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_306/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_307/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_308/*{ t_ClassSymbol, val:str($1<1,0>),  tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let val = tokens[0].clone();
  let val = val.trim(1,0);
  let val = val.to_string();
  
  ASTNode::ClassSymbol(Box::new(ClassSymbol{tok,val,}))
}

fn rule_309/*'\\n'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_310/*'\\s'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_311/*'\\id'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_312/*'\\sym'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_313/*'\\any'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_314/*'\\tab'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_315/*'\\vtab'*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_316/*tk:( "\\" c:id+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_317/*{ t_List_Rules, symbol:$1, tok, min: u32(1) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = 1;
  let min =min as u32;
  
  let max = Default::default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = Default::default();
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_318/*{ t_List_Rules, symbol:$1, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = Default::default();
  
  let max = Default::default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = Default::default();
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_319/*{ t_List_Rules, terminal_symbol:$3, symbol:$1, tok, min: u32(1) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = 1;
  let min =min as u32;
  
  let max = Default::default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = std::mem::take(&mut nodes[2]);
  let terminal_symbol = unsafe{ terminal_symbol.into_TokenValues().unwrap_unchecked() };
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_320/*{ t_List_Rules, terminal_symbol:$3, symbol:$1, tok, min: u32(1) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = 1;
  let min =min as u32;
  
  let max = Default::default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = Default::default();
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_321/*{ t_List_Rules, terminal_symbol:$3, symbol:$1, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = Default::default();
  
  let max = Default::default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = std::mem::take(&mut nodes[2]);
  let terminal_symbol = unsafe{ terminal_symbol.into_TokenValues().unwrap_unchecked() };
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_322/*{ t_List_Rules, terminal_symbol:$3, symbol:$1, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = Default::default();
  
  let max = Default::default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = Default::default();
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_323/*{ t_List_Rules, terminal_symbol:$5, symbol:$1, tok, min: u32($3), max: u32($4) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = nodes[2].clone();
  let min = min.to_token().unwrap();
  let min: u32 = min.to_string().parse().unwrap_or_default();
  
  let max = nodes[4].clone();
  let max = max.to_token().unwrap();
  let max: u32 = max.to_string().parse().unwrap_or_default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = std::mem::take(&mut nodes[5]);
  let terminal_symbol = unsafe{ terminal_symbol.into_TokenValues().unwrap_unchecked() };
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_324/*{ t_List_Rules, terminal_symbol:$5, symbol:$1, tok, min: u32($3), max: u32($4) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = nodes[2].clone();
  let min = min.to_token().unwrap();
  let min: u32 = min.to_string().parse().unwrap_or_default();
  
  let max = 0;
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = std::mem::take(&mut nodes[3]);
  let terminal_symbol = unsafe{ terminal_symbol.into_TokenValues().unwrap_unchecked() };
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_325/*{ t_List_Rules, terminal_symbol:$5, symbol:$1, tok, min: u32($3), max: u32($4) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = nodes[2].clone();
  let min = min.to_token().unwrap();
  let min: u32 = min.to_string().parse().unwrap_or_default();
  
  let max = nodes[4].clone();
  let max = max.to_token().unwrap();
  let max: u32 = max.to_string().parse().unwrap_or_default();
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = Default::default();
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_326/*{ t_List_Rules, terminal_symbol:$5, symbol:$1, tok, min: u32($3), max: u32($4) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let min = nodes[2].clone();
  let min = min.to_token().unwrap();
  let min: u32 = min.to_string().parse().unwrap_or_default();
  
  let max = 0;
  
  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe{ symbol.into_TokenValues().unwrap_unchecked() };
  
  let terminal_symbol = Default::default();
  
  ASTNode::List_Rules(Box::new(List_Rules{tok,min,max,symbol,terminal_symbol,}))
}

fn rule_327/*symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_TokenValues().unwrap_unchecked() };
  out.into()
}

fn rule_328/*"," prim::int*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_329/*{ t_Precedence, sym_prec: u32($2), tok_prec: $3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok_prec = std::mem::take(&mut nodes[2]);
  let tok_prec = unsafe{ tok_prec.into_U32().unwrap_unchecked() };
  
  let sym_prec = tokens[1].clone();
  let sym_prec: u32 = sym_prec.to_string().parse().unwrap_or_default();
  
  let is_keyword = Default::default();
  
  ASTNode::Precedence(Box::new(Precedence{tok_prec,sym_prec,is_keyword,}))
}

fn rule_330/*{ t_Precedence, sym_prec: u32($2), tok_prec: $3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok_prec = std::mem::take(&mut nodes[1]);
  let tok_prec = unsafe{ tok_prec.into_U32().unwrap_unchecked() };
  
  let sym_prec = 0;
  
  let is_keyword = Default::default();
  
  ASTNode::Precedence(Box::new(Precedence{tok_prec,sym_prec,is_keyword,}))
}

fn rule_331/*{ t_Precedence, sym_prec: u32($2), tok_prec: $3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok_prec = Default::default();
  
  let sym_prec = tokens[1].clone();
  let sym_prec: u32 = sym_prec.to_string().parse().unwrap_or_default();
  
  let is_keyword = Default::default();
  
  ASTNode::Precedence(Box::new(Precedence{tok_prec,sym_prec,is_keyword,}))
}

fn rule_332/*{ t_Precedence, sym_prec: u32($2), tok_prec: $3 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok_prec = Default::default();
  
  let sym_prec = 0;
  
  let is_keyword = Default::default();
  
  ASTNode::Precedence(Box::new(Precedence{tok_prec,sym_prec,is_keyword,}))
}

fn rule_333/*{ t_Precedence, sym_prec: u32($prec), is_keyword: true }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok_prec = Default::default();
  
  let sym_prec = tokens[1].clone();
  let sym_prec: u32 = sym_prec.to_string().parse().unwrap_or_default();
  
  let is_keyword = false;
  
  ASTNode::Precedence(Box::new(Precedence{tok_prec,sym_prec,is_keyword,}))
}

fn rule_334/*{ t_Precedence, sym_prec: u32($prec), is_keyword: true }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok_prec = Default::default();
  
  let sym_prec = 0;
  
  let is_keyword = false;
  
  ASTNode::Precedence(Box::new(Precedence{tok_prec,sym_prec,is_keyword,}))
}

fn rule_335/*":" tk:precedence_num? :ast u32($2)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = tokens[1].clone();
  let out: u32 = out.to_string().parse().unwrap_or_default();
  out.into()
}

fn rule_336/*":" tk:precedence_num? :ast u32($2)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = 0;out.into()}

fn rule_337/*tk:precedence_num^prec ":"*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[1].clone();ASTNode::Token(out)}

fn rule_338/*sym::annotated_symbol(+)^s*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecAnnotatedSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_339/*sym::annotated_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_AnnotatedSymbol().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_340/*sym::annotated_symbol(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_AnnotatedSymbol().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecAnnotatedSymbol().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_341/*{ t_IntMatch, vals, statement }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let vals = std::mem::take(&mut nodes[1]);
  let vals = unsafe{ vals.into_VecU64().unwrap_unchecked() };
  
  ASTNode::IntMatch(Box::new(IntMatch{vals,}))
}

fn rule_342/*prim::int :ast u64($1)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nodes[0].clone();
  let out = out.to_token().unwrap();
  let out: u64 = out.to_string().parse().unwrap_or_default();
  out.into()
}

fn rule_343/*( prim::int :ast u64($1) )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_U64().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_344/*( prim::int :ast u64($1) )(+"|")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_U64().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecU64().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_345/*{ t_DefaultMatch, statement }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::DefaultMatch(DefaultMatch{})}

fn rule_346/*{ t_DefaultMatch, statement }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::DefaultMatch(DefaultMatch{})}

fn rule_347/*{ t_FailHint, message: str($message) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let message = nodes[2].clone();
  let message = message.to_token().unwrap();
  let message = message.to_string();
  
  ASTNode::FailHint(Box::new(FailHint{message,}))
}

fn rule_348/*{ t_NonTermMatch, sym, statement }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let sym = std::mem::take(&mut nodes[1]);
  let sym = unsafe{ sym.into_NonTermSymbol().unwrap_unchecked() };
  
  ASTNode::NonTermMatch(Box::new(NonTermMatch{sym,}))
}

fn rule_349/*{ t_TermMatch, sym, statement }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let sym = std::mem::take(&mut nodes[1]);
  let sym = unsafe{ sym.into_TokenValues().unwrap_unchecked() };
  
  ASTNode::TermMatch(Box::new(TermMatch{sym,}))
}

fn rule_350/*reference*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_351/*{ t_AST_TrimmedReference, reference:$1, range:$2, tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe{ range.into_Range().unwrap_unchecked() };
  
  let reference = std::mem::take(&mut nodes[0]);
  let reference = unsafe{ reference.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::AST_TrimmedReference(Box::new(AST_TrimmedReference{tok,range,reference,}))
}

fn rule_352/*{ t_AST_NamedReference, value: str($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let value = nodes[1].clone();
  let value = value.to_token().unwrap();
  let value = value.to_string();
  
  ASTNode::AST_NamedReference(Box::new(AST_NamedReference{tok,value,}))
}

fn rule_353/*{ t_AST_IndexReference, value: i64($2), tok }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let value = nodes[1].clone();
  let value = value.to_token().unwrap();
  let value: i64 = value.to_string().parse().unwrap_or_default();
  
  ASTNode::AST_IndexReference(Box::new(AST_IndexReference{tok,value,}))
}

fn rule_354/*{ t_Init, expression: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let expression = std::mem::take(&mut nodes[1]);
  let expression = unsafe{ expression.into_ExprValues().unwrap_unchecked() };
  
  ASTNode::Init(Box::new(Init{expression,}))
}

fn rule_355/*tk:( ( '+' | '-' )? c:num(+) ( '.' c:num(+) )? ( ( 'e' | 'E' ) ( '+' | '-' )? c:num(+) )? )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_356/*tk:( "\"" ( c:id | c:num | c:nl | c:sym | c:sp | escaped )(*) "\"" )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_357/*tk:( "'" ( c:id | c:num | c:nl | c:sym | c:sp | escaped )(*) "'" )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();ASTNode::Token(out)}

fn rule_358/*"/" regex "/"{:9999}

    :ast $2*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe{ out.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_359/*terminal*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_TokenValues().unwrap_unchecked() };
  out.into()
}

fn rule_360/*nonterm*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NontermValues().unwrap_unchecked() };
  out.into()
}

fn rule_361/*member*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_ExprValues().unwrap_unchecked() };
  out.into()
}

fn rule_362/*token*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_AST_Token().unwrap_unchecked() };
  out.into()
}

fn rule_363/*literal*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_NumberValues().unwrap_unchecked() };
  out.into()
}

fn rule_364/*sequence

    :ast $1*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_365/*modified_sequence_member(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_366/*modified_sequence_member*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_367/*modified_sequence_member(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_Modified_sequence_memberValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_368/*{ t_RegexOptional, val: $1 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::RegexOptional(Box::new(RegexOptional{val,}))
}

fn rule_369/*{ t_RegexOptionalRepeat, val: $1 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::RegexOptionalRepeat(Box::new(RegexOptionalRepeat{val,}))
}

fn rule_370/*{ t_RegexRepeat, val: $1 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::RegexRepeat(Box::new(RegexRepeat{val,}))
}

fn rule_371/*{ t_RegexRepeatCount, val: $1, min: u32($3) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let min = tokens[2].clone();
  let min: u32 = min.to_string().parse().unwrap_or_default();
  
  let max = Default::default();
  
  ASTNode::RegexRepeatCount(Box::new(RegexRepeatCount{val,min,max,}))
}

fn rule_372/*{ t_RegexRepeatCount, val: $1, min: u32($3), max: u32($5) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let min = std::mem::take(&mut nodes[2]);
  let min = unsafe{ min.into_VecToken().unwrap_unchecked() };
  let min = min.len() as u32;
  
  let max = std::mem::take(&mut nodes[4]);
  let max = unsafe{ max.into_VecToken().unwrap_unchecked() };
  let max = max.len() as u32;
  
  ASTNode::RegexRepeatCount(Box::new(RegexRepeatCount{val,min,max,}))
}

fn rule_373/*{ t_RegexRepeatCount, val: $1, min: u32($3), max: u32($5) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let min = 0;
  
  let max = std::mem::take(&mut nodes[3]);
  let max = unsafe{ max.into_VecToken().unwrap_unchecked() };
  let max = max.len() as u32;
  
  ASTNode::RegexRepeatCount(Box::new(RegexRepeatCount{val,min,max,}))
}

fn rule_374/*{ t_RegexRepeatCount, val: $1, min: u32($3), max: u32($5) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let min = std::mem::take(&mut nodes[2]);
  let min = unsafe{ min.into_VecToken().unwrap_unchecked() };
  let min = min.len() as u32;
  
  let max = 0;
  
  ASTNode::RegexRepeatCount(Box::new(RegexRepeatCount{val,min,max,}))
}

fn rule_375/*{ t_RegexRepeatCount, val: $1, min: u32($3), max: u32($5) }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[0]);
  let val = unsafe{ val.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let min = 0;
  
  let max = 0;
  
  ASTNode::RegexRepeatCount(Box::new(RegexRepeatCount{val,min,max,}))
}

fn rule_376/*sequence_member*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Modified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_377/*tk:( c:num(+) )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = tokens[0].clone();
  
  let out = vec![ out_0 ];
  ASTNode::VecToken(out)
}

fn rule_378/*tk:( c:num(+) )(*)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecToken().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::VecToken(out)
}

fn rule_379/*tk:( c:num(+) )*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = tokens[0].clone();
  
  let out = vec![ out_0 ];
  ASTNode::VecToken(out)
}

fn rule_380/*tk:( c:num(+) )(*)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecToken().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::VecToken(out)
}

fn rule_381/*characters_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Modified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_382/*{ t_RegexGroup, val: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = std::mem::take(&mut nodes[1]);
  let val = unsafe{ val.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::RegexGroup(Box::new(RegexGroup{val,}))
}

fn rule_383/*{ t_RegexStart }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexStart(RegexStart{})}

fn rule_384/*{ t_RegexEnd }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexEnd(RegexEnd{})}

fn rule_385/*excluding_match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_RegexExcludeMatch().unwrap_unchecked() };
  out.into()
}

fn rule_386/*match*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_REgexMatch().unwrap_unchecked() };
  out.into()
}

fn rule_387/*special_char*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Modified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_388/*char*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Char().unwrap_unchecked() };
  out.into()
}

fn rule_389/*sequence(+"|")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_390/*sequence*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  out.into()
}

fn rule_391/*sequence(+"|")*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_392/*{ t_RegexExcludeMatch, vals: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let vals = std::mem::take(&mut nodes[1]);
  let vals = unsafe{ vals.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::RegexExcludeMatch(Box::new(RegexExcludeMatch{vals,}))
}

fn rule_393/*characters_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_394/*characters_symbol(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_Modified_sequence_memberValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_395/*{ t_REgexMatch, vals: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let vals = std::mem::take(&mut nodes[1]);
  let vals = unsafe{ vals.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  
  ASTNode::REgexMatch(Box::new(REgexMatch{vals,}))
}

fn rule_396/*characters_symbol*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /*1*/let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_Modified_sequence_memberValues().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_397/*characters_symbol(+)*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe{ out_r.into_Modified_sequence_memberValues().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_VecModified_sequence_memberValues().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_398/*{ t_RegexNewLineChar }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexNewLineChar(RegexNewLineChar{})}

fn rule_399/*{ t_RegexSpaceChar }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexSpaceChar(RegexSpaceChar{})}

fn rule_400/*{ t_RegexWordChar }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexWordChar(RegexWordChar{})}

fn rule_401/*{ t_RegexDigitChar }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexDigitChar(RegexDigitChar{})}

fn rule_402/*{ t_RegexWildCard }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::RegexWildCard(RegexWildCard{})}

fn rule_403/*{ t_RegexEscapedChar, val: $2 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let val = tokens[1].clone();
  
  ASTNode::RegexEscapedChar(Box::new(RegexEscapedChar{val,}))
}

fn rule_404/*{ t_Char, val: $1 }*/<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let val = tokens[0].clone();ASTNode::Char(Box::new(Char{val,}))}

pub struct ReduceRules<Token:Tk>(pub [Reducer<Token,ASTNode<Token>>;405]);

impl<Token:Tk> ReduceRules<Token>{
  pub const fn new() -> Self {
    Self([
        rule_0, 
        rule_1, 
        rule_2, 
        rule_3, 
        rule_4, 
        rule_5, 
        rule_6, 
        rule_7, 
        rule_8, 
        rule_9, 
        rule_10, 
        rule_11, 
        rule_12, 
        rule_13, 
        rule_14, 
        rule_15, 
        rule_16, 
        rule_17, 
        rule_18, 
        rule_19, 
        rule_20, 
        rule_21, 
        rule_22, 
        rule_23, 
        rule_24, 
        rule_25, 
        rule_26, 
        rule_27, 
        rule_28, 
        rule_29, 
        rule_30, 
        rule_31, 
        rule_32, 
        rule_33, 
        rule_34, 
        rule_35, 
        rule_36, 
        rule_37, 
        rule_38, 
        rule_39, 
        rule_40, 
        rule_41, 
        rule_42, 
        rule_43, 
        rule_44, 
        rule_45, 
        rule_46, 
        rule_47, 
        rule_48, 
        rule_49, 
        rule_50, 
        rule_51, 
        rule_52, 
        rule_53, 
        rule_54, 
        rule_55, 
        rule_56, 
        rule_57, 
        rule_58, 
        rule_59, 
        rule_60, 
        rule_61, 
        rule_62, 
        rule_63, 
        rule_64, 
        rule_65, 
        rule_66, 
        rule_67, 
        rule_68, 
        rule_69, 
        rule_70, 
        rule_71, 
        rule_72, 
        rule_73, 
        rule_74, 
        rule_75, 
        rule_76, 
        rule_77, 
        rule_78, 
        rule_79, 
        rule_80, 
        rule_81, 
        rule_82, 
        rule_83, 
        rule_84, 
        rule_85, 
        rule_86, 
        rule_87, 
        rule_88, 
        rule_89, 
        rule_90, 
        rule_91, 
        rule_92, 
        rule_93, 
        rule_94, 
        rule_95, 
        rule_96, 
        rule_97, 
        rule_98, 
        rule_99, 
        rule_100, 
        rule_101, 
        rule_102, 
        rule_103, 
        rule_104, 
        rule_105, 
        rule_106, 
        rule_107, 
        rule_108, 
        rule_109, 
        rule_110, 
        rule_111, 
        rule_112, 
        rule_113, 
        rule_114, 
        rule_115, 
        rule_116, 
        rule_117, 
        rule_118, 
        rule_119, 
        rule_120, 
        rule_121, 
        rule_122, 
        rule_123, 
        rule_124, 
        rule_125, 
        rule_126, 
        rule_127, 
        rule_128, 
        rule_129, 
        rule_130, 
        rule_131, 
        rule_132, 
        rule_133, 
        rule_134, 
        rule_135, 
        rule_136, 
        rule_137, 
        rule_138, 
        rule_139, 
        rule_140, 
        rule_141, 
        rule_142, 
        rule_143, 
        rule_144, 
        rule_145, 
        rule_146, 
        rule_147, 
        rule_148, 
        rule_149, 
        rule_150, 
        rule_151, 
        rule_152, 
        rule_153, 
        rule_154, 
        rule_155, 
        rule_156, 
        rule_157, 
        rule_158, 
        rule_159, 
        rule_160, 
        rule_161, 
        rule_162, 
        rule_163, 
        rule_164, 
        rule_165, 
        rule_166, 
        rule_167, 
        rule_168, 
        rule_169, 
        rule_170, 
        rule_171, 
        rule_172, 
        rule_173, 
        rule_174, 
        rule_175, 
        rule_176, 
        rule_177, 
        rule_178, 
        rule_179, 
        rule_180, 
        rule_181, 
        rule_182, 
        rule_183, 
        rule_184, 
        rule_185, 
        rule_186, 
        rule_187, 
        rule_188, 
        rule_189, 
        rule_190, 
        rule_191, 
        rule_192, 
        rule_193, 
        rule_194, 
        rule_195, 
        rule_196, 
        rule_197, 
        rule_198, 
        rule_199, 
        rule_200, 
        rule_201, 
        rule_202, 
        rule_203, 
        rule_204, 
        rule_205, 
        rule_206, 
        rule_207, 
        rule_208, 
        rule_209, 
        rule_210, 
        rule_211, 
        rule_212, 
        rule_213, 
        rule_214, 
        rule_215, 
        rule_216, 
        rule_217, 
        rule_218, 
        rule_219, 
        rule_220, 
        rule_221, 
        rule_222, 
        rule_223, 
        rule_224, 
        rule_225, 
        rule_226, 
        rule_227, 
        rule_228, 
        rule_229, 
        rule_230, 
        rule_231, 
        rule_232, 
        rule_233, 
        rule_234, 
        rule_235, 
        rule_236, 
        rule_237, 
        rule_238, 
        rule_239, 
        rule_240, 
        rule_241, 
        rule_242, 
        rule_243, 
        rule_244, 
        rule_245, 
        rule_246, 
        rule_247, 
        rule_248, 
        rule_249, 
        rule_250, 
        rule_251, 
        rule_252, 
        rule_253, 
        rule_254, 
        rule_255, 
        rule_256, 
        rule_257, 
        rule_258, 
        rule_259, 
        rule_260, 
        rule_261, 
        rule_262, 
        rule_263, 
        rule_264, 
        rule_265, 
        rule_266, 
        rule_267, 
        rule_268, 
        rule_269, 
        rule_270, 
        rule_271, 
        rule_272, 
        rule_273, 
        rule_274, 
        rule_275, 
        rule_276, 
        rule_277, 
        rule_278, 
        rule_279, 
        rule_280, 
        rule_281, 
        rule_282, 
        rule_283, 
        rule_284, 
        rule_285, 
        rule_286, 
        rule_287, 
        rule_288, 
        rule_289, 
        rule_290, 
        rule_291, 
        rule_292, 
        rule_293, 
        rule_294, 
        rule_295, 
        rule_296, 
        rule_297, 
        rule_298, 
        rule_299, 
        rule_300, 
        rule_301, 
        rule_302, 
        rule_303, 
        rule_304, 
        rule_305, 
        rule_306, 
        rule_307, 
        rule_308, 
        rule_309, 
        rule_310, 
        rule_311, 
        rule_312, 
        rule_313, 
        rule_314, 
        rule_315, 
        rule_316, 
        rule_317, 
        rule_318, 
        rule_319, 
        rule_320, 
        rule_321, 
        rule_322, 
        rule_323, 
        rule_324, 
        rule_325, 
        rule_326, 
        rule_327, 
        rule_328, 
        rule_329, 
        rule_330, 
        rule_331, 
        rule_332, 
        rule_333, 
        rule_334, 
        rule_335, 
        rule_336, 
        rule_337, 
        rule_338, 
        rule_339, 
        rule_340, 
        rule_341, 
        rule_342, 
        rule_343, 
        rule_344, 
        rule_345, 
        rule_346, 
        rule_347, 
        rule_348, 
        rule_349, 
        rule_350, 
        rule_351, 
        rule_352, 
        rule_353, 
        rule_354, 
        rule_355, 
        rule_356, 
        rule_357, 
        rule_358, 
        rule_359, 
        rule_360, 
        rule_361, 
        rule_362, 
        rule_363, 
        rule_364, 
        rule_365, 
        rule_366, 
        rule_367, 
        rule_368, 
        rule_369, 
        rule_370, 
        rule_371, 
        rule_372, 
        rule_373, 
        rule_374, 
        rule_375, 
        rule_376, 
        rule_377, 
        rule_378, 
        rule_379, 
        rule_380, 
        rule_381, 
        rule_382, 
        rule_383, 
        rule_384, 
        rule_385, 
        rule_386, 
        rule_387, 
        rule_388, 
        rule_389, 
        rule_390, 
        rule_391, 
        rule_392, 
        rule_393, 
        rule_394, 
        rule_395, 
        rule_396, 
        rule_397, 
        rule_398, 
        rule_399, 
        rule_400, 
        rule_401, 
        rule_402, 
        rule_403, 
        rule_404, 
      ]
    )
  }
}

impl<Token:Tk> AsRef<[Reducer<Token, ASTNode<Token>>]> for ReduceRules<Token>{fn as_ref(&self) -> &[Reducer<Token, ASTNode<Token>>]{&self.0}}