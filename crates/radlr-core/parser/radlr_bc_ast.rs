/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.0-beta2
/// - **SOURcE**: UNDEFINED
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
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERcHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE
use radlr_rust_runtime::parsers::ast::{Node, Reducer, Tk};
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
#[repr(C, u32)]
pub enum ASTNode<Token: Tk> {
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
  vec_U64(Vec<u64>),
  vec_String(Vec<String>),
  vec_Rule(Vec<Box<Rule<Token>>>),
  vec_Push(Vec<Box<Push<Token>>>),
  vec_Goto(Vec<Box<Goto<Token>>>),
  /* 0 */
  vec_expr_Value(/* 0 */ Vec<expr_Value<Token>>),
  /* 1 */
  vec_non_branch_statement_Value(/* 1 */ Vec<non_branch_statement_Value<Token>>),
  /* 3 */
  vec_template_param_Value(/* 3 */ Vec<template_param_Value>),
  /* 11 */
  vec_grammar_group_1_Value(/* 11 */ Vec<grammar_group_1_Value<Token>>),
  /* 13 */
  vec_preamble_Value(/* 13 */ Vec<preamble_Value<Token>>),
  /* 17 */
  vec_rule_group_2_Value(/* 17 */ Vec<rule_group_2_Value<Token>>),
  /* 31 */
  vec_rule_group_Value(/* 31 */ Vec<rule_group_Value<Token>>),
  /* 34 */
  vec_generic_match_block_group_1_Value(/* 34 */ Vec<generic_match_block_group_1_Value<Token>>),
  /* 35 */
  vec_nonterminal_match_block_group_Value(/* 35 */ Vec<nonterminal_match_block_group_Value<Token>>),
  /* 36 */
  vec_terminal_match_block_group_Value(/* 36 */ Vec<terminal_match_block_group_Value<Token>>),
  /* 37 */
  vec_annotated_symbol_Value(/* 37 */ Vec<annotated_symbol_Value<Token>>),
  /* 38 */
  vec_template_arg_Value(/* 38 */ Vec<template_arg_Value<Token>>),
  /* 40 */
  vec_struct_list_Value(/* 40 */ Vec<struct_list_Value<Token>>),
  /* 42 */
  vec_ignore_clause_list_Value(/* 42 */ Vec<ignore_clause_list_Value<Token>>),
  vec_Token(Vec<Token>),
  expr_Value(expr_Value<Token>),
  non_branch_statement_Value(non_branch_statement_Value<Token>),
  transitive_statement_Value(transitive_statement_Value<Token>),
  template_param_Value(template_param_Value),
  terminal_statement_Value(terminal_statement_Value<Token>),
  number_Value(number_Value<Token>),
  literal_Value(literal_Value<Token>),
  reference_Value(reference_Value<Token>),
  body_Value(body_Value<Token>),
  nonterminal_Value(nonterminal_Value<Token>),
  grammar_group_1_Value(grammar_group_1_Value<Token>),
  def_type_Value(def_type_Value),
  preamble_Value(preamble_Value<Token>),
  branch_statement_Value(branch_statement_Value<Token>),
  symbol_Value(symbol_Value<Token>),
  rule_group_2_Value(rule_group_2_Value<Token>),
  init_objects_Value(init_objects_Value<Token>),
  term_Value(term_Value<Token>),
  match_Value(match_Value<Token>),
  list_group_Value(list_group_Value<Token>),
  rule_group_Value(rule_group_Value<Token>),
  member_Value(member_Value<Token>),
  trimmed_reference_Value(trimmed_reference_Value<Token>),
  generic_match_block_group_1_Value(generic_match_block_group_1_Value<Token>),
  nonterminal_match_block_group_Value(nonterminal_match_block_group_Value<Token>),
  terminal_match_block_group_Value(terminal_match_block_group_Value<Token>),
  annotated_symbol_Value(annotated_symbol_Value<Token>),
  template_arg_Value(template_arg_Value<Token>),
  struct_list_Value(struct_list_Value<Token>),
  ignore_clause_list_Value(ignore_clause_list_Value<Token>),
  list_Value(list_Value<Token>),
  Pop(Box<Pop<Token>>),
  Rule(Box<Rule<Token>>),
  Name(Box<Name>),
  Push(Box<Push<Token>>),
  Peek(Box<Peek<Token>>),
  Fork(Box<Fork<Token>>),
  Fail(Box<Fail<Token>>),
  Goto(Box<Goto<Token>>),
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
  Ignore(Box<Ignore<Token>>),
  Accept(Box<Accept<Token>>),
  Import(Box<Import<Token>>),
  Export(Box<Export<Token>>),
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
  Grammar(Box<Grammar<Token>>),
  Matches(Box<Matches<Token>>),
  CFRules(Box<CFRules<Token>>),
  Ascript(Box<Ascript<Token>>),
  AST_Div(Box<AST_Div<Token>>),
  AST_Pow(Box<AST_Pow<Token>>),
  AST_F128(Box<AST_F128<Token>>),
  AST_U128(Box<AST_U128<Token>>),
  AST_Flag(Box<AST_Flag<Token>>),
  IntMatch(Box<IntMatch<Token>>),
  AST_Bool(Box<AST_Bool<Token>>),
  PegRules(Box<PegRules<Token>>),
  FailHint(Box<FailHint>),
  DEFINED_TYPE_NUM(DEFINED_TYPE_NUM),
  AST_Property(Box<AST_Property<Token>>),
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  AST_Member(Box<AST_Member<Token>>),
  AST_NamedReference(Box<AST_NamedReference<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  ReduceRaw(Box<ReduceRaw<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  DefaultMatch(Box<DefaultMatch<Token>>),
  NotEmptySet(Box<NotEmptySet<Token>>),
  Statement(Box<Statement<Token>>),
  TemplateSym(Box<TemplateSym>),
  AST_Token(Box<AST_Token>),
  AST_StringLiteral(Box<AST_StringLiteral<Token>>),
  TemplateRules(Box<TemplateRules<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  AST_BoolLiteral(Box<AST_BoolLiteral>),
  Precedence(Box<Precedence>),
  NonTermMatch(Box<NonTermMatch<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  AST_Vector(Box<AST_Vector<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  AppendRules(Box<AppendRules<Token>>),
  TemplateASTType(Box<TemplateASTType>),
  SetTokenId(Box<SetTokenId<Token>>),
  AST_String(Box<AST_String<Token>>),
  AnnotatedSymbol(Box<AnnotatedSymbol<Token>>),
  AST_NumberLiteral(Box<AST_NumberLiteral>),
  SetTokenLen(Box<SetTokenLen>),
  TerminalToken(Box<TerminalToken<Token>>),
  AST_STRUCT_TEMPLATE_NAME(Box<AST_STRUCT_TEMPLATE_NAME>),
  EOFSymbol(Box<EOFSymbol<Token>>),
  AST_Statement(Box<AST_Statement<Token>>),
  AST_Struct(Box<AST_Struct<Token>>),
  DEFINED_TYPE_IDENT(DEFINED_TYPE_IDENT),
  List_Rules(Box<List_Rules<Token>>),
  ProductionMatches(Box<ProductionMatches<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
  TermMatch(Box<TermMatch<Token>>),
  TerminalMatches(Box<TerminalMatches<Token>>),
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>),
}

impl<Token: Tk> ASTNode<Token> {
  pub fn to_token(self) -> Option<Token> {
    match self {
      ASTNode::Token(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_U32(self) -> Option<u32> {
    match self {
      ASTNode::U32(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<u32> for ASTNode<Token> {
  fn from(value: u32) -> Self {
    Self::U32(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_U64(self) -> Option<u64> {
    match self {
      ASTNode::U64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<u64> for ASTNode<Token> {
  fn from(value: u64) -> Self {
    Self::U64(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_I32(self) -> Option<i32> {
    match self {
      ASTNode::I32(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<i32> for ASTNode<Token> {
  fn from(value: i32) -> Self {
    Self::I32(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_I64(self) -> Option<i64> {
    match self {
      ASTNode::I64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<i64> for ASTNode<Token> {
  fn from(value: i64) -> Self {
    Self::I64(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_F64(self) -> Option<f64> {
    match self {
      ASTNode::F64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<f64> for ASTNode<Token> {
  fn from(value: f64) -> Self {
    Self::F64(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_String(self) -> Option<String> {
    match self {
      ASTNode::String(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<String> for ASTNode<Token> {
  fn from(value: String) -> Self {
    Self::String(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Bool(self) -> Option<bool> {
    match self {
      ASTNode::Bool(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<bool> for ASTNode<Token> {
  fn from(value: bool) -> Self {
    Self::Bool(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_U64(self) -> Option<Vec<u64>> {
    match self {
      ASTNode::vec_U64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Vec<u64>> for ASTNode<Token> {
  fn from(value: Vec<u64>) -> Self {
    Self::vec_U64(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_String(self) -> Option<Vec<String>> {
    match self {
      ASTNode::vec_String(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Vec<String>> for ASTNode<Token> {
  fn from(value: Vec<String>) -> Self {
    Self::vec_String(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_Rule(self) -> Option<Vec<Box<Rule<Token>>>> {
    match self {
      ASTNode::vec_Rule(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Vec<Box<Rule<Token>>>> for ASTNode<Token> {
  fn from(value: Vec<Box<Rule<Token>>>) -> Self {
    Self::vec_Rule(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_Push(self) -> Option<Vec<Box<Push<Token>>>> {
    match self {
      ASTNode::vec_Push(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Vec<Box<Push<Token>>>> for ASTNode<Token> {
  fn from(value: Vec<Box<Push<Token>>>) -> Self {
    Self::vec_Push(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_Goto(self) -> Option<Vec<Box<Goto<Token>>>> {
    match self {
      ASTNode::vec_Goto(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Vec<Box<Goto<Token>>>> for ASTNode<Token> {
  fn from(value: Vec<Box<Goto<Token>>>) -> Self {
    Self::vec_Goto(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_expr_Value(self) -> Option</* 0 */ Vec<expr_Value<Token>>> {
    match self {ASTNode::vec_expr_Value/*0*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 0 */ Vec<expr_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 0 */ Vec<expr_Value<Token>>) -> Self {
    Self::vec_expr_Value/*0*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_non_branch_statement_Value(self) -> Option</* 1 */ Vec<non_branch_statement_Value<Token>>> {
    match self {ASTNode::vec_non_branch_statement_Value/*1*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 1 */ Vec<non_branch_statement_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 1 */ Vec<non_branch_statement_Value<Token>>) -> Self {
    Self::vec_non_branch_statement_Value/*1*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_template_param_Value(self) -> Option</* 3 */ Vec<template_param_Value>> {
    match self {ASTNode::vec_template_param_Value/*3*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 3 */ Vec<template_param_Value>> for ASTNode<Token> {
  fn from(value: /* 3 */ Vec<template_param_Value>) -> Self {
    Self::vec_template_param_Value/*3*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_grammar_group_1_Value(self) -> Option</* 11 */ Vec<grammar_group_1_Value<Token>>> {
    match self {ASTNode::vec_grammar_group_1_Value/*11*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 11 */ Vec<grammar_group_1_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 11 */ Vec<grammar_group_1_Value<Token>>) -> Self {
    Self::vec_grammar_group_1_Value/*11*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_preamble_Value(self) -> Option</* 13 */ Vec<preamble_Value<Token>>> {
    match self {ASTNode::vec_preamble_Value/*13*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 13 */ Vec<preamble_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 13 */ Vec<preamble_Value<Token>>) -> Self {
    Self::vec_preamble_Value/*13*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_rule_group_2_Value(self) -> Option</* 17 */ Vec<rule_group_2_Value<Token>>> {
    match self {ASTNode::vec_rule_group_2_Value/*17*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 17 */ Vec<rule_group_2_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 17 */ Vec<rule_group_2_Value<Token>>) -> Self {
    Self::vec_rule_group_2_Value/*17*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_rule_group_Value(self) -> Option</* 31 */ Vec<rule_group_Value<Token>>> {
    match self {ASTNode::vec_rule_group_Value/*31*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 31 */ Vec<rule_group_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 31 */ Vec<rule_group_Value<Token>>) -> Self {
    Self::vec_rule_group_Value/*31*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_generic_match_block_group_1_Value(self) -> Option</* 34 */ Vec<generic_match_block_group_1_Value<Token>>> {
    match self {ASTNode::vec_generic_match_block_group_1_Value/*34*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 34 */ Vec<generic_match_block_group_1_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 34 */ Vec<generic_match_block_group_1_Value<Token>>) -> Self {
    Self::vec_generic_match_block_group_1_Value/*34*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_nonterminal_match_block_group_Value(self) -> Option</* 35 */ Vec<nonterminal_match_block_group_Value<Token>>> {
    match self {ASTNode::vec_nonterminal_match_block_group_Value/*35*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 35 */ Vec<nonterminal_match_block_group_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 35 */ Vec<nonterminal_match_block_group_Value<Token>>) -> Self {
    Self::vec_nonterminal_match_block_group_Value/*35*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_terminal_match_block_group_Value(self) -> Option</* 36 */ Vec<terminal_match_block_group_Value<Token>>> {
    match self {ASTNode::vec_terminal_match_block_group_Value/*36*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 36 */ Vec<terminal_match_block_group_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 36 */ Vec<terminal_match_block_group_Value<Token>>) -> Self {
    Self::vec_terminal_match_block_group_Value/*36*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_annotated_symbol_Value(self) -> Option</* 37 */ Vec<annotated_symbol_Value<Token>>> {
    match self {ASTNode::vec_annotated_symbol_Value/*37*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 37 */ Vec<annotated_symbol_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 37 */ Vec<annotated_symbol_Value<Token>>) -> Self {
    Self::vec_annotated_symbol_Value/*37*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_template_arg_Value(self) -> Option</* 38 */ Vec<template_arg_Value<Token>>> {
    match self {ASTNode::vec_template_arg_Value/*38*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 38 */ Vec<template_arg_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 38 */ Vec<template_arg_Value<Token>>) -> Self {
    Self::vec_template_arg_Value/*38*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_struct_list_Value(self) -> Option</* 40 */ Vec<struct_list_Value<Token>>> {
    match self {ASTNode::vec_struct_list_Value/*40*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 40 */ Vec<struct_list_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 40 */ Vec<struct_list_Value<Token>>) -> Self {
    Self::vec_struct_list_Value/*40*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_ignore_clause_list_Value(self) -> Option</* 42 */ Vec<ignore_clause_list_Value<Token>>> {
    match self {ASTNode::vec_ignore_clause_list_Value/*42*/(val) => Some(val),_ => None,}
  }
}

impl<Token: Tk> From</* 42 */ Vec<ignore_clause_list_Value<Token>>> for ASTNode<Token> {
  fn from(value: /* 42 */ Vec<ignore_clause_list_Value<Token>>) -> Self {
    Self::vec_ignore_clause_list_Value/*42*/(value)
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_vec_Token(self) -> Option<Vec<Token>> {
    match self {
      ASTNode::vec_Token(val) => Some(val),
      _ => None,
    }
  }
}

#[derive(Clone, Debug, Default)]
pub enum expr_Value<Token: Tk> {
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
  AST_StringLiteral(Box<AST_StringLiteral<Token>>),
  AST_BoolLiteral(Box<AST_BoolLiteral>),
  AST_Vector(Box<AST_Vector<Token>>),
  AST_String(Box<AST_String<Token>>),
  AST_NumberLiteral(Box<AST_NumberLiteral>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum non_branch_statement_Value<Token: Tk> {
  #[default]
  None,
  Reduce(Box<Reduce<Token>>),
  SetLine(Box<SetLine<Token>>),
  ReduceRaw(Box<ReduceRaw<Token>>),
  SetTokenId(Box<SetTokenId<Token>>),
  SetTokenLen(Box<SetTokenLen>),
}

#[derive(Clone, Debug, Default)]
pub enum transitive_statement_Value<Token: Tk> {
  #[default]
  None,
  Peek(Box<Peek<Token>>),
  Reset(Box<Reset<Token>>),
  Shift(Box<Shift<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum template_param_Value {
  #[default]
  None,
  TemplateSym(Box<TemplateSym>),
  TemplateASTType(Box<TemplateASTType>),
}

#[derive(Clone, Debug, Default)]
pub enum terminal_statement_Value<Token: Tk> {
  #[default]
  None,
  Fail(Box<Fail<Token>>),
  Pass(Box<Pass<Token>>),
  Accept(Box<Accept<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum number_Value<Token: Tk> {
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
  AST_F128(Box<AST_F128<Token>>),
  AST_U128(Box<AST_U128<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum literal_Value<Token: Tk> {
  #[default]
  None,
  AST_StringLiteral(Box<AST_StringLiteral<Token>>),
  AST_BoolLiteral(Box<AST_BoolLiteral>),
  AST_NumberLiteral(Box<AST_NumberLiteral>),
}

#[derive(Clone, Debug, Default)]
pub enum reference_Value<Token: Tk> {
  #[default]
  None,
  AST_NamedReference(Box<AST_NamedReference<Token>>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum body_Value<Token: Tk> {
  #[default]
  None,
  AST_Flag(Box<AST_Flag<Token>>),
  AST_Statement(Box<AST_Statement<Token>>),
  AST_Struct(Box<AST_Struct<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum nonterminal_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum grammar_group_1_Value<Token: Tk> {
  #[default]
  None,
  State(Box<State<Token>>),
  CFRules(Box<CFRules<Token>>),
  PegRules(Box<PegRules<Token>>),
  TemplateRules(Box<TemplateRules<Token>>),
  AppendRules(Box<AppendRules<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum def_type_Value {
  #[default]
  None,
  DEFINED_TYPE_NUM(DEFINED_TYPE_NUM),
  DEFINED_TYPE_IDENT(DEFINED_TYPE_IDENT),
}

#[derive(Clone, Debug, Default)]
pub enum preamble_Value<Token: Tk> {
  #[default]
  None,
  Name(Box<Name>),
  Ignore(Box<Ignore<Token>>),
  Import(Box<Import<Token>>),
  Export(Box<Export<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum branch_statement_Value<Token: Tk> {
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
pub enum symbol_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum rule_group_2_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  NotEmptySet(Box<NotEmptySet<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  AnnotatedSymbol(Box<AnnotatedSymbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
  EOFSymbol(Box<EOFSymbol<Token>>),
  List_Rules(Box<List_Rules<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum init_objects_Value<Token: Tk> {
  #[default]
  None,
  AST_Member(Box<AST_Member<Token>>),
  AST_NamedReference(Box<AST_NamedReference<Token>>),
  AST_Token(Box<AST_Token>),
  AST_StringLiteral(Box<AST_StringLiteral<Token>>),
  AST_BoolLiteral(Box<AST_BoolLiteral>),
  AST_NumberLiteral(Box<AST_NumberLiteral>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum term_Value<Token: Tk> {
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
  AST_NamedReference(Box<AST_NamedReference<Token>>),
  AST_StringLiteral(Box<AST_StringLiteral<Token>>),
  AST_BoolLiteral(Box<AST_BoolLiteral>),
  AST_Vector(Box<AST_Vector<Token>>),
  AST_String(Box<AST_String<Token>>),
  AST_NumberLiteral(Box<AST_NumberLiteral>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum match_Value<Token: Tk> {
  #[default]
  None,
  Matches(Box<Matches<Token>>),
  ProductionMatches(Box<ProductionMatches<Token>>),
  TerminalMatches(Box<TerminalMatches<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum list_group_Value<Token: Tk> {
  #[default]
  None,
  ClassSymbol(Box<ClassSymbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum rule_group_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  NotEmptySet(Box<NotEmptySet<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  AnnotatedSymbol(Box<AnnotatedSymbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
  List_Rules(Box<List_Rules<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum member_Value<Token: Tk> {
  #[default]
  None,
  AST_Member(Box<AST_Member<Token>>),
  AST_NamedReference(Box<AST_NamedReference<Token>>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum trimmed_reference_Value<Token: Tk> {
  #[default]
  None,
  AST_NamedReference(Box<AST_NamedReference<Token>>),
  AST_IndexReference(Box<AST_IndexReference<Token>>),
  AST_TrimmedReference(Box<AST_TrimmedReference<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum generic_match_block_group_1_Value<Token: Tk> {
  #[default]
  None,
  IntMatch(Box<IntMatch<Token>>),
  FailHint(Box<FailHint>),
  DefaultMatch(Box<DefaultMatch<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum nonterminal_match_block_group_Value<Token: Tk> {
  #[default]
  None,
  FailHint(Box<FailHint>),
  DefaultMatch(Box<DefaultMatch<Token>>),
  NonTermMatch(Box<NonTermMatch<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum terminal_match_block_group_Value<Token: Tk> {
  #[default]
  None,
  FailHint(Box<FailHint>),
  DefaultMatch(Box<DefaultMatch<Token>>),
  TermMatch(Box<TermMatch<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum annotated_symbol_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  AnnotatedSymbol(Box<AnnotatedSymbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
  List_Rules(Box<List_Rules<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum template_arg_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
  AST_STRUCT_TEMPLATE_NAME(Box<AST_STRUCT_TEMPLATE_NAME>),
  List_Rules(Box<List_Rules<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum struct_list_Value<Token: Tk> {
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
  AST_Property(Box<AST_Property<Token>>),
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
pub enum ignore_clause_list_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
}

#[derive(Clone, Debug, Default)]
pub enum list_Value<Token: Tk> {
  #[default]
  None,
  NonTerminal_Terminal_Symbol(Box<NonTerminal_Terminal_Symbol<Token>>),
  NonTerminal_Symbol(Box<NonTerminal_Symbol<Token>>),
  TokenGroupRules(Box<TokenGroupRules<Token>>),
  Grouped_Rules(Box<Grouped_Rules<Token>>),
  ClassSymbol(Box<ClassSymbol<Token>>),
  NonTerminal_Import_Symbol(Box<NonTerminal_Import_Symbol<Token>>),
  TerminalToken(Box<TerminalToken<Token>>),
  List_Rules(Box<List_Rules<Token>>),
  Template_NonTerminal_Symbol(Box<Template_NonTerminal_Symbol<Token>>),
}

impl<Token: Tk> From<number_Value<Token>> for expr_Value<Token> {
  fn from(val: number_Value<Token>) -> expr_Value<Token> {
    match val {
      number_Value::AST_I8(val) => expr_Value::AST_I8(val),
      number_Value::AST_U8(val) => expr_Value::AST_U8(val),
      number_Value::AST_F32(val) => expr_Value::AST_F32(val),
      number_Value::AST_I32(val) => expr_Value::AST_I32(val),
      number_Value::AST_U32(val) => expr_Value::AST_U32(val),
      number_Value::AST_F64(val) => expr_Value::AST_F64(val),
      number_Value::AST_I64(val) => expr_Value::AST_I64(val),
      number_Value::AST_U64(val) => expr_Value::AST_U64(val),
      number_Value::AST_F16(val) => expr_Value::AST_F16(val),
      number_Value::AST_I16(val) => expr_Value::AST_I16(val),
      number_Value::AST_U16(val) => expr_Value::AST_U16(val),
      number_Value::AST_F128(val) => expr_Value::AST_F128(val),
      number_Value::AST_U128(val) => expr_Value::AST_U128(val),
      _ => expr_Value::None,
    }
  }
}

impl<Token: Tk> From<literal_Value<Token>> for expr_Value<Token> {
  fn from(val: literal_Value<Token>) -> expr_Value<Token> {
    match val {
      literal_Value::AST_StringLiteral(val) => expr_Value::AST_StringLiteral(val),
      literal_Value::AST_BoolLiteral(val) => expr_Value::AST_BoolLiteral(val),
      literal_Value::AST_NumberLiteral(val) => expr_Value::AST_NumberLiteral(val),
      _ => expr_Value::None,
    }
  }
}

impl<Token: Tk> From<reference_Value<Token>> for expr_Value<Token> {
  fn from(val: reference_Value<Token>) -> expr_Value<Token> {
    match val {
      reference_Value::AST_NamedReference(val) => expr_Value::AST_NamedReference(val),
      reference_Value::AST_IndexReference(val) => expr_Value::AST_IndexReference(val),
      _ => expr_Value::None,
    }
  }
}

impl<Token: Tk> From<term_Value<Token>> for expr_Value<Token> {
  fn from(val: term_Value<Token>) -> expr_Value<Token> {
    match val {
      term_Value::AST_I8(val) => expr_Value::AST_I8(val),
      term_Value::AST_U8(val) => expr_Value::AST_U8(val),
      term_Value::AST_F32(val) => expr_Value::AST_F32(val),
      term_Value::AST_I32(val) => expr_Value::AST_I32(val),
      term_Value::AST_U32(val) => expr_Value::AST_U32(val),
      term_Value::AST_F64(val) => expr_Value::AST_F64(val),
      term_Value::AST_I64(val) => expr_Value::AST_I64(val),
      term_Value::AST_U64(val) => expr_Value::AST_U64(val),
      term_Value::AST_F16(val) => expr_Value::AST_F16(val),
      term_Value::AST_I16(val) => expr_Value::AST_I16(val),
      term_Value::AST_U16(val) => expr_Value::AST_U16(val),
      term_Value::AST_Map(val) => expr_Value::AST_Map(val),
      term_Value::AST_F128(val) => expr_Value::AST_F128(val),
      term_Value::AST_U128(val) => expr_Value::AST_U128(val),
      term_Value::AST_Bool(val) => expr_Value::AST_Bool(val),
      term_Value::AST_Member(val) => expr_Value::AST_Member(val),
      term_Value::AST_NamedReference(val) => expr_Value::AST_NamedReference(val),
      term_Value::AST_StringLiteral(val) => expr_Value::AST_StringLiteral(val),
      term_Value::AST_BoolLiteral(val) => expr_Value::AST_BoolLiteral(val),
      term_Value::AST_Vector(val) => expr_Value::AST_Vector(val),
      term_Value::AST_String(val) => expr_Value::AST_String(val),
      term_Value::AST_NumberLiteral(val) => expr_Value::AST_NumberLiteral(val),
      term_Value::AST_IndexReference(val) => expr_Value::AST_IndexReference(val),
      term_Value::AST_TrimmedReference(val) => expr_Value::AST_TrimmedReference(val),
      _ => expr_Value::None,
    }
  }
}

impl<Token: Tk> From<member_Value<Token>> for expr_Value<Token> {
  fn from(val: member_Value<Token>) -> expr_Value<Token> {
    match val {
      member_Value::AST_Member(val) => expr_Value::AST_Member(val),
      member_Value::AST_NamedReference(val) => expr_Value::AST_NamedReference(val),
      member_Value::AST_IndexReference(val) => expr_Value::AST_IndexReference(val),
      member_Value::AST_TrimmedReference(val) => expr_Value::AST_TrimmedReference(val),
      _ => expr_Value::None,
    }
  }
}

impl<Token: Tk> From<trimmed_reference_Value<Token>> for expr_Value<Token> {
  fn from(val: trimmed_reference_Value<Token>) -> expr_Value<Token> {
    match val {
      trimmed_reference_Value::AST_NamedReference(val) => expr_Value::AST_NamedReference(val),
      trimmed_reference_Value::AST_IndexReference(val) => expr_Value::AST_IndexReference(val),
      trimmed_reference_Value::AST_TrimmedReference(val) => expr_Value::AST_TrimmedReference(val),
      _ => expr_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I8<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_I8<Token>>) -> Self {
    expr_Value::AST_I8(val)
  }
}

impl<Token: Tk> From<Box<AST_U8<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_U8<Token>>) -> Self {
    expr_Value::AST_U8(val)
  }
}

impl<Token: Tk> From<Box<AST_F32<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_F32<Token>>) -> Self {
    expr_Value::AST_F32(val)
  }
}

impl<Token: Tk> From<Box<AST_I32<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_I32<Token>>) -> Self {
    expr_Value::AST_I32(val)
  }
}

impl<Token: Tk> From<Box<AST_U32<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_U32<Token>>) -> Self {
    expr_Value::AST_U32(val)
  }
}

impl<Token: Tk> From<Box<AST_F64<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_F64<Token>>) -> Self {
    expr_Value::AST_F64(val)
  }
}

impl<Token: Tk> From<Box<AST_I64<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_I64<Token>>) -> Self {
    expr_Value::AST_I64(val)
  }
}

impl<Token: Tk> From<Box<AST_U64<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_U64<Token>>) -> Self {
    expr_Value::AST_U64(val)
  }
}

impl<Token: Tk> From<Box<AST_F16<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_F16<Token>>) -> Self {
    expr_Value::AST_F16(val)
  }
}

impl<Token: Tk> From<Box<AST_I16<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_I16<Token>>) -> Self {
    expr_Value::AST_I16(val)
  }
}

impl<Token: Tk> From<Box<AST_U16<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_U16<Token>>) -> Self {
    expr_Value::AST_U16(val)
  }
}

impl<Token: Tk> From<Box<AST_Sub<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Sub<Token>>) -> Self {
    expr_Value::AST_Sub(val)
  }
}

impl<Token: Tk> From<Box<AST_Add<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Add<Token>>) -> Self {
    expr_Value::AST_Add(val)
  }
}

impl<Token: Tk> From<Box<AST_Mod<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Mod<Token>>) -> Self {
    expr_Value::AST_Mod(val)
  }
}

impl<Token: Tk> From<Box<AST_Neg<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Neg<Token>>) -> Self {
    expr_Value::AST_Neg(val)
  }
}

impl<Token: Tk> From<Box<AST_Mul<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Mul<Token>>) -> Self {
    expr_Value::AST_Mul(val)
  }
}

impl<Token: Tk> From<Box<AST_Map<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Map<Token>>) -> Self {
    expr_Value::AST_Map(val)
  }
}

impl<Token: Tk> From<Box<AST_Div<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Div<Token>>) -> Self {
    expr_Value::AST_Div(val)
  }
}

impl<Token: Tk> From<Box<AST_Pow<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Pow<Token>>) -> Self {
    expr_Value::AST_Pow(val)
  }
}

impl<Token: Tk> From<Box<AST_F128<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_F128<Token>>) -> Self {
    expr_Value::AST_F128(val)
  }
}

impl<Token: Tk> From<Box<AST_U128<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_U128<Token>>) -> Self {
    expr_Value::AST_U128(val)
  }
}

impl<Token: Tk> From<Box<AST_Bool<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Bool<Token>>) -> Self {
    expr_Value::AST_Bool(val)
  }
}

impl<Token: Tk> From<Box<AST_Member<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Member<Token>>) -> Self {
    expr_Value::AST_Member(val)
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    expr_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_StringLiteral<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_StringLiteral<Token>>) -> Self {
    expr_Value::AST_StringLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_BoolLiteral>> for expr_Value<Token> {
  fn from(val: Box<AST_BoolLiteral>) -> Self {
    expr_Value::AST_BoolLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_Vector<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_Vector<Token>>) -> Self {
    expr_Value::AST_Vector(val)
  }
}

impl<Token: Tk> From<Box<AST_String<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_String<Token>>) -> Self {
    expr_Value::AST_String(val)
  }
}

impl<Token: Tk> From<Box<AST_NumberLiteral>> for expr_Value<Token> {
  fn from(val: Box<AST_NumberLiteral>) -> Self {
    expr_Value::AST_NumberLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    expr_Value::AST_IndexReference(val)
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for expr_Value<Token> {
  fn from(val: Box<AST_TrimmedReference<Token>>) -> Self {
    expr_Value::AST_TrimmedReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_expr_Value(self) -> Option<expr_Value<Token>> {
    match self {
      ASTNode::expr_Value(val) => Some(val),
      ASTNode::AST_I8(val) => Some(expr_Value::AST_I8(val)),
      ASTNode::AST_U8(val) => Some(expr_Value::AST_U8(val)),
      ASTNode::AST_F32(val) => Some(expr_Value::AST_F32(val)),
      ASTNode::AST_I32(val) => Some(expr_Value::AST_I32(val)),
      ASTNode::AST_U32(val) => Some(expr_Value::AST_U32(val)),
      ASTNode::AST_F64(val) => Some(expr_Value::AST_F64(val)),
      ASTNode::AST_I64(val) => Some(expr_Value::AST_I64(val)),
      ASTNode::AST_U64(val) => Some(expr_Value::AST_U64(val)),
      ASTNode::AST_F16(val) => Some(expr_Value::AST_F16(val)),
      ASTNode::AST_I16(val) => Some(expr_Value::AST_I16(val)),
      ASTNode::AST_U16(val) => Some(expr_Value::AST_U16(val)),
      ASTNode::AST_Sub(val) => Some(expr_Value::AST_Sub(val)),
      ASTNode::AST_Add(val) => Some(expr_Value::AST_Add(val)),
      ASTNode::AST_Mod(val) => Some(expr_Value::AST_Mod(val)),
      ASTNode::AST_Neg(val) => Some(expr_Value::AST_Neg(val)),
      ASTNode::AST_Mul(val) => Some(expr_Value::AST_Mul(val)),
      ASTNode::AST_Map(val) => Some(expr_Value::AST_Map(val)),
      ASTNode::AST_Div(val) => Some(expr_Value::AST_Div(val)),
      ASTNode::AST_Pow(val) => Some(expr_Value::AST_Pow(val)),
      ASTNode::AST_F128(val) => Some(expr_Value::AST_F128(val)),
      ASTNode::AST_U128(val) => Some(expr_Value::AST_U128(val)),
      ASTNode::AST_Bool(val) => Some(expr_Value::AST_Bool(val)),
      ASTNode::AST_Member(val) => Some(expr_Value::AST_Member(val)),
      ASTNode::AST_NamedReference(val) => Some(expr_Value::AST_NamedReference(val)),
      ASTNode::AST_StringLiteral(val) => Some(expr_Value::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(expr_Value::AST_BoolLiteral(val)),
      ASTNode::AST_Vector(val) => Some(expr_Value::AST_Vector(val)),
      ASTNode::AST_String(val) => Some(expr_Value::AST_String(val)),
      ASTNode::AST_NumberLiteral(val) => Some(expr_Value::AST_NumberLiteral(val)),
      ASTNode::AST_IndexReference(val) => Some(expr_Value::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(expr_Value::AST_TrimmedReference(val)),
      ASTNode::number_Value(val) => Some(val.into()),
      ASTNode::literal_Value(val) => Some(val.into()),
      ASTNode::reference_Value(val) => Some(val.into()),
      ASTNode::term_Value(val) => Some(val.into()),
      ASTNode::member_Value(val) => Some(val.into()),
      ASTNode::trimmed_reference_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<expr_Value<Token>> for ASTNode<Token> {
  fn from(value: expr_Value<Token>) -> Self {
    Self::expr_Value(value)
  }
}

impl<Token: Tk> expr_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_I8(val) => ASTNode::AST_I8(val),
      Self::AST_U8(val) => ASTNode::AST_U8(val),
      Self::AST_F32(val) => ASTNode::AST_F32(val),
      Self::AST_I32(val) => ASTNode::AST_I32(val),
      Self::AST_U32(val) => ASTNode::AST_U32(val),
      Self::AST_F64(val) => ASTNode::AST_F64(val),
      Self::AST_I64(val) => ASTNode::AST_I64(val),
      Self::AST_U64(val) => ASTNode::AST_U64(val),
      Self::AST_F16(val) => ASTNode::AST_F16(val),
      Self::AST_I16(val) => ASTNode::AST_I16(val),
      Self::AST_U16(val) => ASTNode::AST_U16(val),
      Self::AST_Sub(val) => ASTNode::AST_Sub(val),
      Self::AST_Add(val) => ASTNode::AST_Add(val),
      Self::AST_Mod(val) => ASTNode::AST_Mod(val),
      Self::AST_Neg(val) => ASTNode::AST_Neg(val),
      Self::AST_Mul(val) => ASTNode::AST_Mul(val),
      Self::AST_Map(val) => ASTNode::AST_Map(val),
      Self::AST_Div(val) => ASTNode::AST_Div(val),
      Self::AST_Pow(val) => ASTNode::AST_Pow(val),
      Self::AST_F128(val) => ASTNode::AST_F128(val),
      Self::AST_U128(val) => ASTNode::AST_U128(val),
      Self::AST_Bool(val) => ASTNode::AST_Bool(val),
      Self::AST_Member(val) => ASTNode::AST_Member(val),
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_StringLiteral(val) => ASTNode::AST_StringLiteral(val),
      Self::AST_BoolLiteral(val) => ASTNode::AST_BoolLiteral(val),
      Self::AST_Vector(val) => ASTNode::AST_Vector(val),
      Self::AST_String(val) => ASTNode::AST_String(val),
      Self::AST_NumberLiteral(val) => ASTNode::AST_NumberLiteral(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      Self::AST_TrimmedReference(val) => ASTNode::AST_TrimmedReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<Reduce<Token>>> for non_branch_statement_Value<Token> {
  fn from(val: Box<Reduce<Token>>) -> Self {
    non_branch_statement_Value::Reduce(val)
  }
}

impl<Token: Tk> From<Box<SetLine<Token>>> for non_branch_statement_Value<Token> {
  fn from(val: Box<SetLine<Token>>) -> Self {
    non_branch_statement_Value::SetLine(val)
  }
}

impl<Token: Tk> From<Box<ReduceRaw<Token>>> for non_branch_statement_Value<Token> {
  fn from(val: Box<ReduceRaw<Token>>) -> Self {
    non_branch_statement_Value::ReduceRaw(val)
  }
}

impl<Token: Tk> From<Box<SetTokenId<Token>>> for non_branch_statement_Value<Token> {
  fn from(val: Box<SetTokenId<Token>>) -> Self {
    non_branch_statement_Value::SetTokenId(val)
  }
}

impl<Token: Tk> From<Box<SetTokenLen>> for non_branch_statement_Value<Token> {
  fn from(val: Box<SetTokenLen>) -> Self {
    non_branch_statement_Value::SetTokenLen(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_non_branch_statement_Value(self) -> Option<non_branch_statement_Value<Token>> {
    match self {
      ASTNode::non_branch_statement_Value(val) => Some(val),
      ASTNode::Reduce(val) => Some(non_branch_statement_Value::Reduce(val)),
      ASTNode::SetLine(val) => Some(non_branch_statement_Value::SetLine(val)),
      ASTNode::ReduceRaw(val) => Some(non_branch_statement_Value::ReduceRaw(val)),
      ASTNode::SetTokenId(val) => Some(non_branch_statement_Value::SetTokenId(val)),
      ASTNode::SetTokenLen(val) => Some(non_branch_statement_Value::SetTokenLen(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<non_branch_statement_Value<Token>> for ASTNode<Token> {
  fn from(value: non_branch_statement_Value<Token>) -> Self {
    Self::non_branch_statement_Value(value)
  }
}

impl<Token: Tk> non_branch_statement_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::Reduce(val) => ASTNode::Reduce(val),
      Self::SetLine(val) => ASTNode::SetLine(val),
      Self::ReduceRaw(val) => ASTNode::ReduceRaw(val),
      Self::SetTokenId(val) => ASTNode::SetTokenId(val),
      Self::SetTokenLen(val) => ASTNode::SetTokenLen(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<Peek<Token>>> for transitive_statement_Value<Token> {
  fn from(val: Box<Peek<Token>>) -> Self {
    transitive_statement_Value::Peek(val)
  }
}

impl<Token: Tk> From<Box<Reset<Token>>> for transitive_statement_Value<Token> {
  fn from(val: Box<Reset<Token>>) -> Self {
    transitive_statement_Value::Reset(val)
  }
}

impl<Token: Tk> From<Box<Shift<Token>>> for transitive_statement_Value<Token> {
  fn from(val: Box<Shift<Token>>) -> Self {
    transitive_statement_Value::Shift(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_transitive_statement_Value(self) -> Option<transitive_statement_Value<Token>> {
    match self {
      ASTNode::transitive_statement_Value(val) => Some(val),
      ASTNode::Peek(val) => Some(transitive_statement_Value::Peek(val)),
      ASTNode::Reset(val) => Some(transitive_statement_Value::Reset(val)),
      ASTNode::Shift(val) => Some(transitive_statement_Value::Shift(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<transitive_statement_Value<Token>> for ASTNode<Token> {
  fn from(value: transitive_statement_Value<Token>) -> Self {
    Self::transitive_statement_Value(value)
  }
}

impl<Token: Tk> transitive_statement_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::Peek(val) => ASTNode::Peek(val),
      Self::Reset(val) => ASTNode::Reset(val),
      Self::Shift(val) => ASTNode::Shift(val),
      _ => ASTNode::None,
    }
  }
}

impl From<Box<TemplateSym>> for template_param_Value {
  fn from(val: Box<TemplateSym>) -> Self {
    template_param_Value::TemplateSym(val)
  }
}

impl From<Box<TemplateASTType>> for template_param_Value {
  fn from(val: Box<TemplateASTType>) -> Self {
    template_param_Value::TemplateASTType(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_template_param_Value(self) -> Option<template_param_Value> {
    match self {
      ASTNode::template_param_Value(val) => Some(val),
      ASTNode::TemplateSym(val) => Some(template_param_Value::TemplateSym(val)),
      ASTNode::TemplateASTType(val) => Some(template_param_Value::TemplateASTType(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<template_param_Value> for ASTNode<Token> {
  fn from(value: template_param_Value) -> Self {
    Self::template_param_Value(value)
  }
}

impl template_param_Value {
  pub fn to_ast<Token: Tk>(self) -> ASTNode<Token> {
    match self {
      Self::TemplateSym(val) => ASTNode::TemplateSym(val),
      Self::TemplateASTType(val) => ASTNode::TemplateASTType(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<Fail<Token>>> for terminal_statement_Value<Token> {
  fn from(val: Box<Fail<Token>>) -> Self {
    terminal_statement_Value::Fail(val)
  }
}

impl<Token: Tk> From<Box<Pass<Token>>> for terminal_statement_Value<Token> {
  fn from(val: Box<Pass<Token>>) -> Self {
    terminal_statement_Value::Pass(val)
  }
}

impl<Token: Tk> From<Box<Accept<Token>>> for terminal_statement_Value<Token> {
  fn from(val: Box<Accept<Token>>) -> Self {
    terminal_statement_Value::Accept(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_terminal_statement_Value(self) -> Option<terminal_statement_Value<Token>> {
    match self {
      ASTNode::terminal_statement_Value(val) => Some(val),
      ASTNode::Fail(val) => Some(terminal_statement_Value::Fail(val)),
      ASTNode::Pass(val) => Some(terminal_statement_Value::Pass(val)),
      ASTNode::Accept(val) => Some(terminal_statement_Value::Accept(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<terminal_statement_Value<Token>> for ASTNode<Token> {
  fn from(value: terminal_statement_Value<Token>) -> Self {
    Self::terminal_statement_Value(value)
  }
}

impl<Token: Tk> terminal_statement_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::Fail(val) => ASTNode::Fail(val),
      Self::Pass(val) => ASTNode::Pass(val),
      Self::Accept(val) => ASTNode::Accept(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I8<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_I8<Token>>) -> Self {
    number_Value::AST_I8(val)
  }
}

impl<Token: Tk> From<Box<AST_U8<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_U8<Token>>) -> Self {
    number_Value::AST_U8(val)
  }
}

impl<Token: Tk> From<Box<AST_F32<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_F32<Token>>) -> Self {
    number_Value::AST_F32(val)
  }
}

impl<Token: Tk> From<Box<AST_I32<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_I32<Token>>) -> Self {
    number_Value::AST_I32(val)
  }
}

impl<Token: Tk> From<Box<AST_U32<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_U32<Token>>) -> Self {
    number_Value::AST_U32(val)
  }
}

impl<Token: Tk> From<Box<AST_F64<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_F64<Token>>) -> Self {
    number_Value::AST_F64(val)
  }
}

impl<Token: Tk> From<Box<AST_I64<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_I64<Token>>) -> Self {
    number_Value::AST_I64(val)
  }
}

impl<Token: Tk> From<Box<AST_U64<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_U64<Token>>) -> Self {
    number_Value::AST_U64(val)
  }
}

impl<Token: Tk> From<Box<AST_F16<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_F16<Token>>) -> Self {
    number_Value::AST_F16(val)
  }
}

impl<Token: Tk> From<Box<AST_I16<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_I16<Token>>) -> Self {
    number_Value::AST_I16(val)
  }
}

impl<Token: Tk> From<Box<AST_U16<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_U16<Token>>) -> Self {
    number_Value::AST_U16(val)
  }
}

impl<Token: Tk> From<Box<AST_F128<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_F128<Token>>) -> Self {
    number_Value::AST_F128(val)
  }
}

impl<Token: Tk> From<Box<AST_U128<Token>>> for number_Value<Token> {
  fn from(val: Box<AST_U128<Token>>) -> Self {
    number_Value::AST_U128(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_number_Value(self) -> Option<number_Value<Token>> {
    match self {
      ASTNode::number_Value(val) => Some(val),
      ASTNode::AST_I8(val) => Some(number_Value::AST_I8(val)),
      ASTNode::AST_U8(val) => Some(number_Value::AST_U8(val)),
      ASTNode::AST_F32(val) => Some(number_Value::AST_F32(val)),
      ASTNode::AST_I32(val) => Some(number_Value::AST_I32(val)),
      ASTNode::AST_U32(val) => Some(number_Value::AST_U32(val)),
      ASTNode::AST_F64(val) => Some(number_Value::AST_F64(val)),
      ASTNode::AST_I64(val) => Some(number_Value::AST_I64(val)),
      ASTNode::AST_U64(val) => Some(number_Value::AST_U64(val)),
      ASTNode::AST_F16(val) => Some(number_Value::AST_F16(val)),
      ASTNode::AST_I16(val) => Some(number_Value::AST_I16(val)),
      ASTNode::AST_U16(val) => Some(number_Value::AST_U16(val)),
      ASTNode::AST_F128(val) => Some(number_Value::AST_F128(val)),
      ASTNode::AST_U128(val) => Some(number_Value::AST_U128(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<number_Value<Token>> for ASTNode<Token> {
  fn from(value: number_Value<Token>) -> Self {
    Self::number_Value(value)
  }
}

impl<Token: Tk> number_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_I8(val) => ASTNode::AST_I8(val),
      Self::AST_U8(val) => ASTNode::AST_U8(val),
      Self::AST_F32(val) => ASTNode::AST_F32(val),
      Self::AST_I32(val) => ASTNode::AST_I32(val),
      Self::AST_U32(val) => ASTNode::AST_U32(val),
      Self::AST_F64(val) => ASTNode::AST_F64(val),
      Self::AST_I64(val) => ASTNode::AST_I64(val),
      Self::AST_U64(val) => ASTNode::AST_U64(val),
      Self::AST_F16(val) => ASTNode::AST_F16(val),
      Self::AST_I16(val) => ASTNode::AST_I16(val),
      Self::AST_U16(val) => ASTNode::AST_U16(val),
      Self::AST_F128(val) => ASTNode::AST_F128(val),
      Self::AST_U128(val) => ASTNode::AST_U128(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_StringLiteral<Token>>> for literal_Value<Token> {
  fn from(val: Box<AST_StringLiteral<Token>>) -> Self {
    literal_Value::AST_StringLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_BoolLiteral>> for literal_Value<Token> {
  fn from(val: Box<AST_BoolLiteral>) -> Self {
    literal_Value::AST_BoolLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_NumberLiteral>> for literal_Value<Token> {
  fn from(val: Box<AST_NumberLiteral>) -> Self {
    literal_Value::AST_NumberLiteral(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_literal_Value(self) -> Option<literal_Value<Token>> {
    match self {
      ASTNode::literal_Value(val) => Some(val),
      ASTNode::AST_StringLiteral(val) => Some(literal_Value::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(literal_Value::AST_BoolLiteral(val)),
      ASTNode::AST_NumberLiteral(val) => Some(literal_Value::AST_NumberLiteral(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<literal_Value<Token>> for ASTNode<Token> {
  fn from(value: literal_Value<Token>) -> Self {
    Self::literal_Value(value)
  }
}

impl<Token: Tk> literal_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_StringLiteral(val) => ASTNode::AST_StringLiteral(val),
      Self::AST_BoolLiteral(val) => ASTNode::AST_BoolLiteral(val),
      Self::AST_NumberLiteral(val) => ASTNode::AST_NumberLiteral(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for reference_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    reference_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for reference_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    reference_Value::AST_IndexReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_reference_Value(self) -> Option<reference_Value<Token>> {
    match self {
      ASTNode::reference_Value(val) => Some(val),
      ASTNode::AST_NamedReference(val) => Some(reference_Value::AST_NamedReference(val)),
      ASTNode::AST_IndexReference(val) => Some(reference_Value::AST_IndexReference(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<reference_Value<Token>> for ASTNode<Token> {
  fn from(value: reference_Value<Token>) -> Self {
    Self::reference_Value(value)
  }
}

impl<Token: Tk> reference_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Flag<Token>>> for body_Value<Token> {
  fn from(val: Box<AST_Flag<Token>>) -> Self {
    body_Value::AST_Flag(val)
  }
}

impl<Token: Tk> From<Box<AST_Statement<Token>>> for body_Value<Token> {
  fn from(val: Box<AST_Statement<Token>>) -> Self {
    body_Value::AST_Statement(val)
  }
}

impl<Token: Tk> From<Box<AST_Struct<Token>>> for body_Value<Token> {
  fn from(val: Box<AST_Struct<Token>>) -> Self {
    body_Value::AST_Struct(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_body_Value(self) -> Option<body_Value<Token>> {
    match self {
      ASTNode::body_Value(val) => Some(val),
      ASTNode::AST_Flag(val) => Some(body_Value::AST_Flag(val)),
      ASTNode::AST_Statement(val) => Some(body_Value::AST_Statement(val)),
      ASTNode::AST_Struct(val) => Some(body_Value::AST_Struct(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<body_Value<Token>> for ASTNode<Token> {
  fn from(value: body_Value<Token>) -> Self {
    Self::body_Value(value)
  }
}

impl<Token: Tk> body_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_Flag(val) => ASTNode::AST_Flag(val),
      Self::AST_Statement(val) => ASTNode::AST_Statement(val),
      Self::AST_Struct(val) => ASTNode::AST_Struct(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for nonterminal_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    nonterminal_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for nonterminal_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    nonterminal_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for nonterminal_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    nonterminal_Value::NonTerminal_Import_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_nonterminal_Value(self) -> Option<nonterminal_Value<Token>> {
    match self {
      ASTNode::nonterminal_Value(val) => Some(val),
      ASTNode::NonTerminal_Symbol(val) => Some(nonterminal_Value::NonTerminal_Symbol(val)),
      ASTNode::Grouped_Rules(val) => Some(nonterminal_Value::Grouped_Rules(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(nonterminal_Value::NonTerminal_Import_Symbol(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<nonterminal_Value<Token>> for ASTNode<Token> {
  fn from(value: nonterminal_Value<Token>) -> Self {
    Self::nonterminal_Value(value)
  }
}

impl<Token: Tk> nonterminal_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<State<Token>>> for grammar_group_1_Value<Token> {
  fn from(val: Box<State<Token>>) -> Self {
    grammar_group_1_Value::State(val)
  }
}

impl<Token: Tk> From<Box<CFRules<Token>>> for grammar_group_1_Value<Token> {
  fn from(val: Box<CFRules<Token>>) -> Self {
    grammar_group_1_Value::CFRules(val)
  }
}

impl<Token: Tk> From<Box<PegRules<Token>>> for grammar_group_1_Value<Token> {
  fn from(val: Box<PegRules<Token>>) -> Self {
    grammar_group_1_Value::PegRules(val)
  }
}

impl<Token: Tk> From<Box<TemplateRules<Token>>> for grammar_group_1_Value<Token> {
  fn from(val: Box<TemplateRules<Token>>) -> Self {
    grammar_group_1_Value::TemplateRules(val)
  }
}

impl<Token: Tk> From<Box<AppendRules<Token>>> for grammar_group_1_Value<Token> {
  fn from(val: Box<AppendRules<Token>>) -> Self {
    grammar_group_1_Value::AppendRules(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_grammar_group_1_Value(self) -> Option<grammar_group_1_Value<Token>> {
    match self {
      ASTNode::grammar_group_1_Value(val) => Some(val),
      ASTNode::State(val) => Some(grammar_group_1_Value::State(val)),
      ASTNode::CFRules(val) => Some(grammar_group_1_Value::CFRules(val)),
      ASTNode::PegRules(val) => Some(grammar_group_1_Value::PegRules(val)),
      ASTNode::TemplateRules(val) => Some(grammar_group_1_Value::TemplateRules(val)),
      ASTNode::AppendRules(val) => Some(grammar_group_1_Value::AppendRules(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<grammar_group_1_Value<Token>> for ASTNode<Token> {
  fn from(value: grammar_group_1_Value<Token>) -> Self {
    Self::grammar_group_1_Value(value)
  }
}

impl<Token: Tk> grammar_group_1_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::State(val) => ASTNode::State(val),
      Self::CFRules(val) => ASTNode::CFRules(val),
      Self::PegRules(val) => ASTNode::PegRules(val),
      Self::TemplateRules(val) => ASTNode::TemplateRules(val),
      Self::AppendRules(val) => ASTNode::AppendRules(val),
      _ => ASTNode::None,
    }
  }
}

impl From<DEFINED_TYPE_NUM> for def_type_Value {
  fn from(val: DEFINED_TYPE_NUM) -> Self {
    def_type_Value::DEFINED_TYPE_NUM(val)
  }
}

impl From<DEFINED_TYPE_IDENT> for def_type_Value {
  fn from(val: DEFINED_TYPE_IDENT) -> Self {
    def_type_Value::DEFINED_TYPE_IDENT(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_def_type_Value(self) -> Option<def_type_Value> {
    match self {
      ASTNode::def_type_Value(val) => Some(val),
      ASTNode::DEFINED_TYPE_NUM(val) => Some(def_type_Value::DEFINED_TYPE_NUM(val)),
      ASTNode::DEFINED_TYPE_IDENT(val) => Some(def_type_Value::DEFINED_TYPE_IDENT(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<def_type_Value> for ASTNode<Token> {
  fn from(value: def_type_Value) -> Self {
    Self::def_type_Value(value)
  }
}

impl def_type_Value {
  pub fn to_ast<Token: Tk>(self) -> ASTNode<Token> {
    match self {
      Self::DEFINED_TYPE_NUM(val) => ASTNode::DEFINED_TYPE_NUM(val),
      Self::DEFINED_TYPE_IDENT(val) => ASTNode::DEFINED_TYPE_IDENT(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<Name>> for preamble_Value<Token> {
  fn from(val: Box<Name>) -> Self {
    preamble_Value::Name(val)
  }
}

impl<Token: Tk> From<Box<Ignore<Token>>> for preamble_Value<Token> {
  fn from(val: Box<Ignore<Token>>) -> Self {
    preamble_Value::Ignore(val)
  }
}

impl<Token: Tk> From<Box<Import<Token>>> for preamble_Value<Token> {
  fn from(val: Box<Import<Token>>) -> Self {
    preamble_Value::Import(val)
  }
}

impl<Token: Tk> From<Box<Export<Token>>> for preamble_Value<Token> {
  fn from(val: Box<Export<Token>>) -> Self {
    preamble_Value::Export(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_preamble_Value(self) -> Option<preamble_Value<Token>> {
    match self {
      ASTNode::preamble_Value(val) => Some(val),
      ASTNode::Name(val) => Some(preamble_Value::Name(val)),
      ASTNode::Ignore(val) => Some(preamble_Value::Ignore(val)),
      ASTNode::Import(val) => Some(preamble_Value::Import(val)),
      ASTNode::Export(val) => Some(preamble_Value::Export(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<preamble_Value<Token>> for ASTNode<Token> {
  fn from(value: preamble_Value<Token>) -> Self {
    Self::preamble_Value(value)
  }
}

impl<Token: Tk> preamble_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::Name(val) => ASTNode::Name(val),
      Self::Ignore(val) => ASTNode::Ignore(val),
      Self::Import(val) => ASTNode::Import(val),
      Self::Export(val) => ASTNode::Export(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<terminal_statement_Value<Token>> for branch_statement_Value<Token> {
  fn from(val: terminal_statement_Value<Token>) -> branch_statement_Value<Token> {
    match val {
      terminal_statement_Value::Fail(val) => branch_statement_Value::Fail(val),
      terminal_statement_Value::Pass(val) => branch_statement_Value::Pass(val),
      terminal_statement_Value::Accept(val) => branch_statement_Value::Accept(val),
      _ => branch_statement_Value::None,
    }
  }
}

impl<Token: Tk> From<match_Value<Token>> for branch_statement_Value<Token> {
  fn from(val: match_Value<Token>) -> branch_statement_Value<Token> {
    match val {
      match_Value::Matches(val) => branch_statement_Value::Matches(val),
      match_Value::ProductionMatches(val) => branch_statement_Value::ProductionMatches(val),
      match_Value::TerminalMatches(val) => branch_statement_Value::TerminalMatches(val),
      _ => branch_statement_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<Fail<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<Fail<Token>>) -> Self {
    branch_statement_Value::Fail(val)
  }
}

impl<Token: Tk> From<Box<Pass<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<Pass<Token>>) -> Self {
    branch_statement_Value::Pass(val)
  }
}

impl<Token: Tk> From<Box<Gotos<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<Gotos<Token>>) -> Self {
    branch_statement_Value::Gotos(val)
  }
}

impl<Token: Tk> From<Box<Accept<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<Accept<Token>>) -> Self {
    branch_statement_Value::Accept(val)
  }
}

impl<Token: Tk> From<Box<Matches<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<Matches<Token>>) -> Self {
    branch_statement_Value::Matches(val)
  }
}

impl<Token: Tk> From<Box<ProductionMatches<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<ProductionMatches<Token>>) -> Self {
    branch_statement_Value::ProductionMatches(val)
  }
}

impl<Token: Tk> From<Box<TerminalMatches<Token>>> for branch_statement_Value<Token> {
  fn from(val: Box<TerminalMatches<Token>>) -> Self {
    branch_statement_Value::TerminalMatches(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_branch_statement_Value(self) -> Option<branch_statement_Value<Token>> {
    match self {
      ASTNode::branch_statement_Value(val) => Some(val),
      ASTNode::Fail(val) => Some(branch_statement_Value::Fail(val)),
      ASTNode::Pass(val) => Some(branch_statement_Value::Pass(val)),
      ASTNode::Gotos(val) => Some(branch_statement_Value::Gotos(val)),
      ASTNode::Accept(val) => Some(branch_statement_Value::Accept(val)),
      ASTNode::Matches(val) => Some(branch_statement_Value::Matches(val)),
      ASTNode::ProductionMatches(val) => Some(branch_statement_Value::ProductionMatches(val)),
      ASTNode::TerminalMatches(val) => Some(branch_statement_Value::TerminalMatches(val)),
      ASTNode::terminal_statement_Value(val) => Some(val.into()),
      ASTNode::match_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<branch_statement_Value<Token>> for ASTNode<Token> {
  fn from(value: branch_statement_Value<Token>) -> Self {
    Self::branch_statement_Value(value)
  }
}

impl<Token: Tk> branch_statement_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::Fail(val) => ASTNode::Fail(val),
      Self::Pass(val) => ASTNode::Pass(val),
      Self::Gotos(val) => ASTNode::Gotos(val),
      Self::Accept(val) => ASTNode::Accept(val),
      Self::Matches(val) => ASTNode::Matches(val),
      Self::ProductionMatches(val) => ASTNode::ProductionMatches(val),
      Self::TerminalMatches(val) => ASTNode::TerminalMatches(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<nonterminal_Value<Token>> for symbol_Value<Token> {
  fn from(val: nonterminal_Value<Token>) -> symbol_Value<Token> {
    match val {
      nonterminal_Value::NonTerminal_Symbol(val) => symbol_Value::NonTerminal_Symbol(val),
      nonterminal_Value::Grouped_Rules(val) => symbol_Value::Grouped_Rules(val),
      nonterminal_Value::NonTerminal_Import_Symbol(val) => symbol_Value::NonTerminal_Import_Symbol(val),
      _ => symbol_Value::None,
    }
  }
}

impl<Token: Tk> From<ignore_clause_list_Value<Token>> for symbol_Value<Token> {
  fn from(val: ignore_clause_list_Value<Token>) -> symbol_Value<Token> {
    match val {
      ignore_clause_list_Value::NonTerminal_Terminal_Symbol(val) => symbol_Value::NonTerminal_Terminal_Symbol(val),
      ignore_clause_list_Value::TokenGroupRules(val) => symbol_Value::TokenGroupRules(val),
      ignore_clause_list_Value::ClassSymbol(val) => symbol_Value::ClassSymbol(val),
      ignore_clause_list_Value::TerminalToken(val) => symbol_Value::TerminalToken(val),
      _ => symbol_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for symbol_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    symbol_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for symbol_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    symbol_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for symbol_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    symbol_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for symbol_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    symbol_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for symbol_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    symbol_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for symbol_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    symbol_Value::NonTerminal_Import_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for symbol_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    symbol_Value::TerminalToken(val)
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for symbol_Value<Token> {
  fn from(val: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    symbol_Value::Template_NonTerminal_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_symbol_Value(self) -> Option<symbol_Value<Token>> {
    match self {
      ASTNode::symbol_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(symbol_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::NonTerminal_Symbol(val) => Some(symbol_Value::NonTerminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(symbol_Value::TokenGroupRules(val)),
      ASTNode::Grouped_Rules(val) => Some(symbol_Value::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(symbol_Value::ClassSymbol(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(symbol_Value::NonTerminal_Import_Symbol(val)),
      ASTNode::TerminalToken(val) => Some(symbol_Value::TerminalToken(val)),
      ASTNode::Template_NonTerminal_Symbol(val) => Some(symbol_Value::Template_NonTerminal_Symbol(val)),
      ASTNode::nonterminal_Value(val) => Some(val.into()),
      ASTNode::ignore_clause_list_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<symbol_Value<Token>> for ASTNode<Token> {
  fn from(value: symbol_Value<Token>) -> Self {
    Self::symbol_Value(value)
  }
}

impl<Token: Tk> symbol_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      Self::Template_NonTerminal_Symbol(val) => ASTNode::Template_NonTerminal_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<symbol_Value<Token>> for rule_group_2_Value<Token> {
  fn from(val: symbol_Value<Token>) -> rule_group_2_Value<Token> {
    match val {
      symbol_Value::NonTerminal_Terminal_Symbol(val) => rule_group_2_Value::NonTerminal_Terminal_Symbol(val),
      symbol_Value::NonTerminal_Symbol(val) => rule_group_2_Value::NonTerminal_Symbol(val),
      symbol_Value::TokenGroupRules(val) => rule_group_2_Value::TokenGroupRules(val),
      symbol_Value::Grouped_Rules(val) => rule_group_2_Value::Grouped_Rules(val),
      symbol_Value::ClassSymbol(val) => rule_group_2_Value::ClassSymbol(val),
      symbol_Value::NonTerminal_Import_Symbol(val) => rule_group_2_Value::NonTerminal_Import_Symbol(val),
      symbol_Value::TerminalToken(val) => rule_group_2_Value::TerminalToken(val),
      symbol_Value::Template_NonTerminal_Symbol(val) => rule_group_2_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_2_Value::None,
    }
  }
}

impl<Token: Tk> From<rule_group_Value<Token>> for rule_group_2_Value<Token> {
  fn from(val: rule_group_Value<Token>) -> rule_group_2_Value<Token> {
    match val {
      rule_group_Value::NonTerminal_Terminal_Symbol(val) => rule_group_2_Value::NonTerminal_Terminal_Symbol(val),
      rule_group_Value::NonTerminal_Symbol(val) => rule_group_2_Value::NonTerminal_Symbol(val),
      rule_group_Value::TokenGroupRules(val) => rule_group_2_Value::TokenGroupRules(val),
      rule_group_Value::NotEmptySet(val) => rule_group_2_Value::NotEmptySet(val),
      rule_group_Value::Grouped_Rules(val) => rule_group_2_Value::Grouped_Rules(val),
      rule_group_Value::ClassSymbol(val) => rule_group_2_Value::ClassSymbol(val),
      rule_group_Value::NonTerminal_Import_Symbol(val) => rule_group_2_Value::NonTerminal_Import_Symbol(val),
      rule_group_Value::AnnotatedSymbol(val) => rule_group_2_Value::AnnotatedSymbol(val),
      rule_group_Value::TerminalToken(val) => rule_group_2_Value::TerminalToken(val),
      rule_group_Value::List_Rules(val) => rule_group_2_Value::List_Rules(val),
      rule_group_Value::Template_NonTerminal_Symbol(val) => rule_group_2_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_2_Value::None,
    }
  }
}

impl<Token: Tk> From<annotated_symbol_Value<Token>> for rule_group_2_Value<Token> {
  fn from(val: annotated_symbol_Value<Token>) -> rule_group_2_Value<Token> {
    match val {
      annotated_symbol_Value::NonTerminal_Terminal_Symbol(val) => rule_group_2_Value::NonTerminal_Terminal_Symbol(val),
      annotated_symbol_Value::NonTerminal_Symbol(val) => rule_group_2_Value::NonTerminal_Symbol(val),
      annotated_symbol_Value::TokenGroupRules(val) => rule_group_2_Value::TokenGroupRules(val),
      annotated_symbol_Value::Grouped_Rules(val) => rule_group_2_Value::Grouped_Rules(val),
      annotated_symbol_Value::ClassSymbol(val) => rule_group_2_Value::ClassSymbol(val),
      annotated_symbol_Value::NonTerminal_Import_Symbol(val) => rule_group_2_Value::NonTerminal_Import_Symbol(val),
      annotated_symbol_Value::AnnotatedSymbol(val) => rule_group_2_Value::AnnotatedSymbol(val),
      annotated_symbol_Value::TerminalToken(val) => rule_group_2_Value::TerminalToken(val),
      annotated_symbol_Value::List_Rules(val) => rule_group_2_Value::List_Rules(val),
      annotated_symbol_Value::Template_NonTerminal_Symbol(val) => rule_group_2_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_2_Value::None,
    }
  }
}

impl<Token: Tk> From<list_Value<Token>> for rule_group_2_Value<Token> {
  fn from(val: list_Value<Token>) -> rule_group_2_Value<Token> {
    match val {
      list_Value::NonTerminal_Terminal_Symbol(val) => rule_group_2_Value::NonTerminal_Terminal_Symbol(val),
      list_Value::NonTerminal_Symbol(val) => rule_group_2_Value::NonTerminal_Symbol(val),
      list_Value::TokenGroupRules(val) => rule_group_2_Value::TokenGroupRules(val),
      list_Value::Grouped_Rules(val) => rule_group_2_Value::Grouped_Rules(val),
      list_Value::ClassSymbol(val) => rule_group_2_Value::ClassSymbol(val),
      list_Value::NonTerminal_Import_Symbol(val) => rule_group_2_Value::NonTerminal_Import_Symbol(val),
      list_Value::TerminalToken(val) => rule_group_2_Value::TerminalToken(val),
      list_Value::List_Rules(val) => rule_group_2_Value::List_Rules(val),
      list_Value::Template_NonTerminal_Symbol(val) => rule_group_2_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_2_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    rule_group_2_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    rule_group_2_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    rule_group_2_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<NotEmptySet<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<NotEmptySet<Token>>) -> Self {
    rule_group_2_Value::NotEmptySet(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    rule_group_2_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    rule_group_2_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    rule_group_2_Value::NonTerminal_Import_Symbol(val)
  }
}

impl<Token: Tk> From<Box<AnnotatedSymbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<AnnotatedSymbol<Token>>) -> Self {
    rule_group_2_Value::AnnotatedSymbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    rule_group_2_Value::TerminalToken(val)
  }
}

impl<Token: Tk> From<Box<EOFSymbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<EOFSymbol<Token>>) -> Self {
    rule_group_2_Value::EOFSymbol(val)
  }
}

impl<Token: Tk> From<Box<List_Rules<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<List_Rules<Token>>) -> Self {
    rule_group_2_Value::List_Rules(val)
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for rule_group_2_Value<Token> {
  fn from(val: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    rule_group_2_Value::Template_NonTerminal_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_rule_group_2_Value(self) -> Option<rule_group_2_Value<Token>> {
    match self {
      ASTNode::rule_group_2_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(rule_group_2_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::NonTerminal_Symbol(val) => Some(rule_group_2_Value::NonTerminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(rule_group_2_Value::TokenGroupRules(val)),
      ASTNode::NotEmptySet(val) => Some(rule_group_2_Value::NotEmptySet(val)),
      ASTNode::Grouped_Rules(val) => Some(rule_group_2_Value::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(rule_group_2_Value::ClassSymbol(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(rule_group_2_Value::NonTerminal_Import_Symbol(val)),
      ASTNode::AnnotatedSymbol(val) => Some(rule_group_2_Value::AnnotatedSymbol(val)),
      ASTNode::TerminalToken(val) => Some(rule_group_2_Value::TerminalToken(val)),
      ASTNode::EOFSymbol(val) => Some(rule_group_2_Value::EOFSymbol(val)),
      ASTNode::List_Rules(val) => Some(rule_group_2_Value::List_Rules(val)),
      ASTNode::Template_NonTerminal_Symbol(val) => Some(rule_group_2_Value::Template_NonTerminal_Symbol(val)),
      ASTNode::symbol_Value(val) => Some(val.into()),
      ASTNode::rule_group_Value(val) => Some(val.into()),
      ASTNode::annotated_symbol_Value(val) => Some(val.into()),
      ASTNode::list_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<rule_group_2_Value<Token>> for ASTNode<Token> {
  fn from(value: rule_group_2_Value<Token>) -> Self {
    Self::rule_group_2_Value(value)
  }
}

impl<Token: Tk> rule_group_2_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::NotEmptySet(val) => ASTNode::NotEmptySet(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      Self::AnnotatedSymbol(val) => ASTNode::AnnotatedSymbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      Self::EOFSymbol(val) => ASTNode::EOFSymbol(val),
      Self::List_Rules(val) => ASTNode::List_Rules(val),
      Self::Template_NonTerminal_Symbol(val) => ASTNode::Template_NonTerminal_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<literal_Value<Token>> for init_objects_Value<Token> {
  fn from(val: literal_Value<Token>) -> init_objects_Value<Token> {
    match val {
      literal_Value::AST_StringLiteral(val) => init_objects_Value::AST_StringLiteral(val),
      literal_Value::AST_BoolLiteral(val) => init_objects_Value::AST_BoolLiteral(val),
      literal_Value::AST_NumberLiteral(val) => init_objects_Value::AST_NumberLiteral(val),
      _ => init_objects_Value::None,
    }
  }
}

impl<Token: Tk> From<reference_Value<Token>> for init_objects_Value<Token> {
  fn from(val: reference_Value<Token>) -> init_objects_Value<Token> {
    match val {
      reference_Value::AST_NamedReference(val) => init_objects_Value::AST_NamedReference(val),
      reference_Value::AST_IndexReference(val) => init_objects_Value::AST_IndexReference(val),
      _ => init_objects_Value::None,
    }
  }
}

impl<Token: Tk> From<member_Value<Token>> for init_objects_Value<Token> {
  fn from(val: member_Value<Token>) -> init_objects_Value<Token> {
    match val {
      member_Value::AST_Member(val) => init_objects_Value::AST_Member(val),
      member_Value::AST_NamedReference(val) => init_objects_Value::AST_NamedReference(val),
      member_Value::AST_IndexReference(val) => init_objects_Value::AST_IndexReference(val),
      member_Value::AST_TrimmedReference(val) => init_objects_Value::AST_TrimmedReference(val),
      _ => init_objects_Value::None,
    }
  }
}

impl<Token: Tk> From<trimmed_reference_Value<Token>> for init_objects_Value<Token> {
  fn from(val: trimmed_reference_Value<Token>) -> init_objects_Value<Token> {
    match val {
      trimmed_reference_Value::AST_NamedReference(val) => init_objects_Value::AST_NamedReference(val),
      trimmed_reference_Value::AST_IndexReference(val) => init_objects_Value::AST_IndexReference(val),
      trimmed_reference_Value::AST_TrimmedReference(val) => init_objects_Value::AST_TrimmedReference(val),
      _ => init_objects_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Member<Token>>> for init_objects_Value<Token> {
  fn from(val: Box<AST_Member<Token>>) -> Self {
    init_objects_Value::AST_Member(val)
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for init_objects_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    init_objects_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_Token>> for init_objects_Value<Token> {
  fn from(val: Box<AST_Token>) -> Self {
    init_objects_Value::AST_Token(val)
  }
}

impl<Token: Tk> From<Box<AST_StringLiteral<Token>>> for init_objects_Value<Token> {
  fn from(val: Box<AST_StringLiteral<Token>>) -> Self {
    init_objects_Value::AST_StringLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_BoolLiteral>> for init_objects_Value<Token> {
  fn from(val: Box<AST_BoolLiteral>) -> Self {
    init_objects_Value::AST_BoolLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_NumberLiteral>> for init_objects_Value<Token> {
  fn from(val: Box<AST_NumberLiteral>) -> Self {
    init_objects_Value::AST_NumberLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for init_objects_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    init_objects_Value::AST_IndexReference(val)
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for init_objects_Value<Token> {
  fn from(val: Box<AST_TrimmedReference<Token>>) -> Self {
    init_objects_Value::AST_TrimmedReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_init_objects_Value(self) -> Option<init_objects_Value<Token>> {
    match self {
      ASTNode::init_objects_Value(val) => Some(val),
      ASTNode::AST_Member(val) => Some(init_objects_Value::AST_Member(val)),
      ASTNode::AST_NamedReference(val) => Some(init_objects_Value::AST_NamedReference(val)),
      ASTNode::AST_Token(val) => Some(init_objects_Value::AST_Token(val)),
      ASTNode::AST_StringLiteral(val) => Some(init_objects_Value::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(init_objects_Value::AST_BoolLiteral(val)),
      ASTNode::AST_NumberLiteral(val) => Some(init_objects_Value::AST_NumberLiteral(val)),
      ASTNode::AST_IndexReference(val) => Some(init_objects_Value::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(init_objects_Value::AST_TrimmedReference(val)),
      ASTNode::literal_Value(val) => Some(val.into()),
      ASTNode::reference_Value(val) => Some(val.into()),
      ASTNode::member_Value(val) => Some(val.into()),
      ASTNode::trimmed_reference_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<init_objects_Value<Token>> for ASTNode<Token> {
  fn from(value: init_objects_Value<Token>) -> Self {
    Self::init_objects_Value(value)
  }
}

impl<Token: Tk> init_objects_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_Member(val) => ASTNode::AST_Member(val),
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_Token(val) => ASTNode::AST_Token(val),
      Self::AST_StringLiteral(val) => ASTNode::AST_StringLiteral(val),
      Self::AST_BoolLiteral(val) => ASTNode::AST_BoolLiteral(val),
      Self::AST_NumberLiteral(val) => ASTNode::AST_NumberLiteral(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      Self::AST_TrimmedReference(val) => ASTNode::AST_TrimmedReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<number_Value<Token>> for term_Value<Token> {
  fn from(val: number_Value<Token>) -> term_Value<Token> {
    match val {
      number_Value::AST_I8(val) => term_Value::AST_I8(val),
      number_Value::AST_U8(val) => term_Value::AST_U8(val),
      number_Value::AST_F32(val) => term_Value::AST_F32(val),
      number_Value::AST_I32(val) => term_Value::AST_I32(val),
      number_Value::AST_U32(val) => term_Value::AST_U32(val),
      number_Value::AST_F64(val) => term_Value::AST_F64(val),
      number_Value::AST_I64(val) => term_Value::AST_I64(val),
      number_Value::AST_U64(val) => term_Value::AST_U64(val),
      number_Value::AST_F16(val) => term_Value::AST_F16(val),
      number_Value::AST_I16(val) => term_Value::AST_I16(val),
      number_Value::AST_U16(val) => term_Value::AST_U16(val),
      number_Value::AST_F128(val) => term_Value::AST_F128(val),
      number_Value::AST_U128(val) => term_Value::AST_U128(val),
      _ => term_Value::None,
    }
  }
}

impl<Token: Tk> From<literal_Value<Token>> for term_Value<Token> {
  fn from(val: literal_Value<Token>) -> term_Value<Token> {
    match val {
      literal_Value::AST_StringLiteral(val) => term_Value::AST_StringLiteral(val),
      literal_Value::AST_BoolLiteral(val) => term_Value::AST_BoolLiteral(val),
      literal_Value::AST_NumberLiteral(val) => term_Value::AST_NumberLiteral(val),
      _ => term_Value::None,
    }
  }
}

impl<Token: Tk> From<reference_Value<Token>> for term_Value<Token> {
  fn from(val: reference_Value<Token>) -> term_Value<Token> {
    match val {
      reference_Value::AST_NamedReference(val) => term_Value::AST_NamedReference(val),
      reference_Value::AST_IndexReference(val) => term_Value::AST_IndexReference(val),
      _ => term_Value::None,
    }
  }
}

impl<Token: Tk> From<member_Value<Token>> for term_Value<Token> {
  fn from(val: member_Value<Token>) -> term_Value<Token> {
    match val {
      member_Value::AST_Member(val) => term_Value::AST_Member(val),
      member_Value::AST_NamedReference(val) => term_Value::AST_NamedReference(val),
      member_Value::AST_IndexReference(val) => term_Value::AST_IndexReference(val),
      member_Value::AST_TrimmedReference(val) => term_Value::AST_TrimmedReference(val),
      _ => term_Value::None,
    }
  }
}

impl<Token: Tk> From<trimmed_reference_Value<Token>> for term_Value<Token> {
  fn from(val: trimmed_reference_Value<Token>) -> term_Value<Token> {
    match val {
      trimmed_reference_Value::AST_NamedReference(val) => term_Value::AST_NamedReference(val),
      trimmed_reference_Value::AST_IndexReference(val) => term_Value::AST_IndexReference(val),
      trimmed_reference_Value::AST_TrimmedReference(val) => term_Value::AST_TrimmedReference(val),
      _ => term_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I8<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_I8<Token>>) -> Self {
    term_Value::AST_I8(val)
  }
}

impl<Token: Tk> From<Box<AST_U8<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_U8<Token>>) -> Self {
    term_Value::AST_U8(val)
  }
}

impl<Token: Tk> From<Box<AST_F32<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_F32<Token>>) -> Self {
    term_Value::AST_F32(val)
  }
}

impl<Token: Tk> From<Box<AST_I32<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_I32<Token>>) -> Self {
    term_Value::AST_I32(val)
  }
}

impl<Token: Tk> From<Box<AST_U32<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_U32<Token>>) -> Self {
    term_Value::AST_U32(val)
  }
}

impl<Token: Tk> From<Box<AST_F64<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_F64<Token>>) -> Self {
    term_Value::AST_F64(val)
  }
}

impl<Token: Tk> From<Box<AST_I64<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_I64<Token>>) -> Self {
    term_Value::AST_I64(val)
  }
}

impl<Token: Tk> From<Box<AST_U64<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_U64<Token>>) -> Self {
    term_Value::AST_U64(val)
  }
}

impl<Token: Tk> From<Box<AST_F16<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_F16<Token>>) -> Self {
    term_Value::AST_F16(val)
  }
}

impl<Token: Tk> From<Box<AST_I16<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_I16<Token>>) -> Self {
    term_Value::AST_I16(val)
  }
}

impl<Token: Tk> From<Box<AST_U16<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_U16<Token>>) -> Self {
    term_Value::AST_U16(val)
  }
}

impl<Token: Tk> From<Box<AST_Map<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_Map<Token>>) -> Self {
    term_Value::AST_Map(val)
  }
}

impl<Token: Tk> From<Box<AST_F128<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_F128<Token>>) -> Self {
    term_Value::AST_F128(val)
  }
}

impl<Token: Tk> From<Box<AST_U128<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_U128<Token>>) -> Self {
    term_Value::AST_U128(val)
  }
}

impl<Token: Tk> From<Box<AST_Bool<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_Bool<Token>>) -> Self {
    term_Value::AST_Bool(val)
  }
}

impl<Token: Tk> From<Box<AST_Member<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_Member<Token>>) -> Self {
    term_Value::AST_Member(val)
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    term_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_StringLiteral<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_StringLiteral<Token>>) -> Self {
    term_Value::AST_StringLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_BoolLiteral>> for term_Value<Token> {
  fn from(val: Box<AST_BoolLiteral>) -> Self {
    term_Value::AST_BoolLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_Vector<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_Vector<Token>>) -> Self {
    term_Value::AST_Vector(val)
  }
}

impl<Token: Tk> From<Box<AST_String<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_String<Token>>) -> Self {
    term_Value::AST_String(val)
  }
}

impl<Token: Tk> From<Box<AST_NumberLiteral>> for term_Value<Token> {
  fn from(val: Box<AST_NumberLiteral>) -> Self {
    term_Value::AST_NumberLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    term_Value::AST_IndexReference(val)
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for term_Value<Token> {
  fn from(val: Box<AST_TrimmedReference<Token>>) -> Self {
    term_Value::AST_TrimmedReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_term_Value(self) -> Option<term_Value<Token>> {
    match self {
      ASTNode::term_Value(val) => Some(val),
      ASTNode::AST_I8(val) => Some(term_Value::AST_I8(val)),
      ASTNode::AST_U8(val) => Some(term_Value::AST_U8(val)),
      ASTNode::AST_F32(val) => Some(term_Value::AST_F32(val)),
      ASTNode::AST_I32(val) => Some(term_Value::AST_I32(val)),
      ASTNode::AST_U32(val) => Some(term_Value::AST_U32(val)),
      ASTNode::AST_F64(val) => Some(term_Value::AST_F64(val)),
      ASTNode::AST_I64(val) => Some(term_Value::AST_I64(val)),
      ASTNode::AST_U64(val) => Some(term_Value::AST_U64(val)),
      ASTNode::AST_F16(val) => Some(term_Value::AST_F16(val)),
      ASTNode::AST_I16(val) => Some(term_Value::AST_I16(val)),
      ASTNode::AST_U16(val) => Some(term_Value::AST_U16(val)),
      ASTNode::AST_Map(val) => Some(term_Value::AST_Map(val)),
      ASTNode::AST_F128(val) => Some(term_Value::AST_F128(val)),
      ASTNode::AST_U128(val) => Some(term_Value::AST_U128(val)),
      ASTNode::AST_Bool(val) => Some(term_Value::AST_Bool(val)),
      ASTNode::AST_Member(val) => Some(term_Value::AST_Member(val)),
      ASTNode::AST_NamedReference(val) => Some(term_Value::AST_NamedReference(val)),
      ASTNode::AST_StringLiteral(val) => Some(term_Value::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(term_Value::AST_BoolLiteral(val)),
      ASTNode::AST_Vector(val) => Some(term_Value::AST_Vector(val)),
      ASTNode::AST_String(val) => Some(term_Value::AST_String(val)),
      ASTNode::AST_NumberLiteral(val) => Some(term_Value::AST_NumberLiteral(val)),
      ASTNode::AST_IndexReference(val) => Some(term_Value::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(term_Value::AST_TrimmedReference(val)),
      ASTNode::number_Value(val) => Some(val.into()),
      ASTNode::literal_Value(val) => Some(val.into()),
      ASTNode::reference_Value(val) => Some(val.into()),
      ASTNode::member_Value(val) => Some(val.into()),
      ASTNode::trimmed_reference_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<term_Value<Token>> for ASTNode<Token> {
  fn from(value: term_Value<Token>) -> Self {
    Self::term_Value(value)
  }
}

impl<Token: Tk> term_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_I8(val) => ASTNode::AST_I8(val),
      Self::AST_U8(val) => ASTNode::AST_U8(val),
      Self::AST_F32(val) => ASTNode::AST_F32(val),
      Self::AST_I32(val) => ASTNode::AST_I32(val),
      Self::AST_U32(val) => ASTNode::AST_U32(val),
      Self::AST_F64(val) => ASTNode::AST_F64(val),
      Self::AST_I64(val) => ASTNode::AST_I64(val),
      Self::AST_U64(val) => ASTNode::AST_U64(val),
      Self::AST_F16(val) => ASTNode::AST_F16(val),
      Self::AST_I16(val) => ASTNode::AST_I16(val),
      Self::AST_U16(val) => ASTNode::AST_U16(val),
      Self::AST_Map(val) => ASTNode::AST_Map(val),
      Self::AST_F128(val) => ASTNode::AST_F128(val),
      Self::AST_U128(val) => ASTNode::AST_U128(val),
      Self::AST_Bool(val) => ASTNode::AST_Bool(val),
      Self::AST_Member(val) => ASTNode::AST_Member(val),
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_StringLiteral(val) => ASTNode::AST_StringLiteral(val),
      Self::AST_BoolLiteral(val) => ASTNode::AST_BoolLiteral(val),
      Self::AST_Vector(val) => ASTNode::AST_Vector(val),
      Self::AST_String(val) => ASTNode::AST_String(val),
      Self::AST_NumberLiteral(val) => ASTNode::AST_NumberLiteral(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      Self::AST_TrimmedReference(val) => ASTNode::AST_TrimmedReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<Matches<Token>>> for match_Value<Token> {
  fn from(val: Box<Matches<Token>>) -> Self {
    match_Value::Matches(val)
  }
}

impl<Token: Tk> From<Box<ProductionMatches<Token>>> for match_Value<Token> {
  fn from(val: Box<ProductionMatches<Token>>) -> Self {
    match_Value::ProductionMatches(val)
  }
}

impl<Token: Tk> From<Box<TerminalMatches<Token>>> for match_Value<Token> {
  fn from(val: Box<TerminalMatches<Token>>) -> Self {
    match_Value::TerminalMatches(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_match_Value(self) -> Option<match_Value<Token>> {
    match self {
      ASTNode::match_Value(val) => Some(val),
      ASTNode::Matches(val) => Some(match_Value::Matches(val)),
      ASTNode::ProductionMatches(val) => Some(match_Value::ProductionMatches(val)),
      ASTNode::TerminalMatches(val) => Some(match_Value::TerminalMatches(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<match_Value<Token>> for ASTNode<Token> {
  fn from(value: match_Value<Token>) -> Self {
    Self::match_Value(value)
  }
}

impl<Token: Tk> match_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::Matches(val) => ASTNode::Matches(val),
      Self::ProductionMatches(val) => ASTNode::ProductionMatches(val),
      Self::TerminalMatches(val) => ASTNode::TerminalMatches(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for list_group_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    list_group_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for list_group_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    list_group_Value::TerminalToken(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_list_group_Value(self) -> Option<list_group_Value<Token>> {
    match self {
      ASTNode::list_group_Value(val) => Some(val),
      ASTNode::ClassSymbol(val) => Some(list_group_Value::ClassSymbol(val)),
      ASTNode::TerminalToken(val) => Some(list_group_Value::TerminalToken(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<list_group_Value<Token>> for ASTNode<Token> {
  fn from(value: list_group_Value<Token>) -> Self {
    Self::list_group_Value(value)
  }
}

impl<Token: Tk> list_group_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<symbol_Value<Token>> for rule_group_Value<Token> {
  fn from(val: symbol_Value<Token>) -> rule_group_Value<Token> {
    match val {
      symbol_Value::NonTerminal_Terminal_Symbol(val) => rule_group_Value::NonTerminal_Terminal_Symbol(val),
      symbol_Value::NonTerminal_Symbol(val) => rule_group_Value::NonTerminal_Symbol(val),
      symbol_Value::TokenGroupRules(val) => rule_group_Value::TokenGroupRules(val),
      symbol_Value::Grouped_Rules(val) => rule_group_Value::Grouped_Rules(val),
      symbol_Value::ClassSymbol(val) => rule_group_Value::ClassSymbol(val),
      symbol_Value::NonTerminal_Import_Symbol(val) => rule_group_Value::NonTerminal_Import_Symbol(val),
      symbol_Value::TerminalToken(val) => rule_group_Value::TerminalToken(val),
      symbol_Value::Template_NonTerminal_Symbol(val) => rule_group_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_Value::None,
    }
  }
}

impl<Token: Tk> From<annotated_symbol_Value<Token>> for rule_group_Value<Token> {
  fn from(val: annotated_symbol_Value<Token>) -> rule_group_Value<Token> {
    match val {
      annotated_symbol_Value::NonTerminal_Terminal_Symbol(val) => rule_group_Value::NonTerminal_Terminal_Symbol(val),
      annotated_symbol_Value::NonTerminal_Symbol(val) => rule_group_Value::NonTerminal_Symbol(val),
      annotated_symbol_Value::TokenGroupRules(val) => rule_group_Value::TokenGroupRules(val),
      annotated_symbol_Value::Grouped_Rules(val) => rule_group_Value::Grouped_Rules(val),
      annotated_symbol_Value::ClassSymbol(val) => rule_group_Value::ClassSymbol(val),
      annotated_symbol_Value::NonTerminal_Import_Symbol(val) => rule_group_Value::NonTerminal_Import_Symbol(val),
      annotated_symbol_Value::AnnotatedSymbol(val) => rule_group_Value::AnnotatedSymbol(val),
      annotated_symbol_Value::TerminalToken(val) => rule_group_Value::TerminalToken(val),
      annotated_symbol_Value::List_Rules(val) => rule_group_Value::List_Rules(val),
      annotated_symbol_Value::Template_NonTerminal_Symbol(val) => rule_group_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_Value::None,
    }
  }
}

impl<Token: Tk> From<list_Value<Token>> for rule_group_Value<Token> {
  fn from(val: list_Value<Token>) -> rule_group_Value<Token> {
    match val {
      list_Value::NonTerminal_Terminal_Symbol(val) => rule_group_Value::NonTerminal_Terminal_Symbol(val),
      list_Value::NonTerminal_Symbol(val) => rule_group_Value::NonTerminal_Symbol(val),
      list_Value::TokenGroupRules(val) => rule_group_Value::TokenGroupRules(val),
      list_Value::Grouped_Rules(val) => rule_group_Value::Grouped_Rules(val),
      list_Value::ClassSymbol(val) => rule_group_Value::ClassSymbol(val),
      list_Value::NonTerminal_Import_Symbol(val) => rule_group_Value::NonTerminal_Import_Symbol(val),
      list_Value::TerminalToken(val) => rule_group_Value::TerminalToken(val),
      list_Value::List_Rules(val) => rule_group_Value::List_Rules(val),
      list_Value::Template_NonTerminal_Symbol(val) => rule_group_Value::Template_NonTerminal_Symbol(val),
      _ => rule_group_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    rule_group_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    rule_group_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    rule_group_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<NotEmptySet<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<NotEmptySet<Token>>) -> Self {
    rule_group_Value::NotEmptySet(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    rule_group_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    rule_group_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    rule_group_Value::NonTerminal_Import_Symbol(val)
  }
}

impl<Token: Tk> From<Box<AnnotatedSymbol<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<AnnotatedSymbol<Token>>) -> Self {
    rule_group_Value::AnnotatedSymbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    rule_group_Value::TerminalToken(val)
  }
}

impl<Token: Tk> From<Box<List_Rules<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<List_Rules<Token>>) -> Self {
    rule_group_Value::List_Rules(val)
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for rule_group_Value<Token> {
  fn from(val: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    rule_group_Value::Template_NonTerminal_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_rule_group_Value(self) -> Option<rule_group_Value<Token>> {
    match self {
      ASTNode::rule_group_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(rule_group_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::NonTerminal_Symbol(val) => Some(rule_group_Value::NonTerminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(rule_group_Value::TokenGroupRules(val)),
      ASTNode::NotEmptySet(val) => Some(rule_group_Value::NotEmptySet(val)),
      ASTNode::Grouped_Rules(val) => Some(rule_group_Value::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(rule_group_Value::ClassSymbol(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(rule_group_Value::NonTerminal_Import_Symbol(val)),
      ASTNode::AnnotatedSymbol(val) => Some(rule_group_Value::AnnotatedSymbol(val)),
      ASTNode::TerminalToken(val) => Some(rule_group_Value::TerminalToken(val)),
      ASTNode::List_Rules(val) => Some(rule_group_Value::List_Rules(val)),
      ASTNode::Template_NonTerminal_Symbol(val) => Some(rule_group_Value::Template_NonTerminal_Symbol(val)),
      ASTNode::symbol_Value(val) => Some(val.into()),
      ASTNode::annotated_symbol_Value(val) => Some(val.into()),
      ASTNode::list_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<rule_group_Value<Token>> for ASTNode<Token> {
  fn from(value: rule_group_Value<Token>) -> Self {
    Self::rule_group_Value(value)
  }
}

impl<Token: Tk> rule_group_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::NotEmptySet(val) => ASTNode::NotEmptySet(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      Self::AnnotatedSymbol(val) => ASTNode::AnnotatedSymbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      Self::List_Rules(val) => ASTNode::List_Rules(val),
      Self::Template_NonTerminal_Symbol(val) => ASTNode::Template_NonTerminal_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<reference_Value<Token>> for member_Value<Token> {
  fn from(val: reference_Value<Token>) -> member_Value<Token> {
    match val {
      reference_Value::AST_NamedReference(val) => member_Value::AST_NamedReference(val),
      reference_Value::AST_IndexReference(val) => member_Value::AST_IndexReference(val),
      _ => member_Value::None,
    }
  }
}

impl<Token: Tk> From<trimmed_reference_Value<Token>> for member_Value<Token> {
  fn from(val: trimmed_reference_Value<Token>) -> member_Value<Token> {
    match val {
      trimmed_reference_Value::AST_NamedReference(val) => member_Value::AST_NamedReference(val),
      trimmed_reference_Value::AST_IndexReference(val) => member_Value::AST_IndexReference(val),
      trimmed_reference_Value::AST_TrimmedReference(val) => member_Value::AST_TrimmedReference(val),
      _ => member_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Member<Token>>> for member_Value<Token> {
  fn from(val: Box<AST_Member<Token>>) -> Self {
    member_Value::AST_Member(val)
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for member_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    member_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for member_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    member_Value::AST_IndexReference(val)
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for member_Value<Token> {
  fn from(val: Box<AST_TrimmedReference<Token>>) -> Self {
    member_Value::AST_TrimmedReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_member_Value(self) -> Option<member_Value<Token>> {
    match self {
      ASTNode::member_Value(val) => Some(val),
      ASTNode::AST_Member(val) => Some(member_Value::AST_Member(val)),
      ASTNode::AST_NamedReference(val) => Some(member_Value::AST_NamedReference(val)),
      ASTNode::AST_IndexReference(val) => Some(member_Value::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(member_Value::AST_TrimmedReference(val)),
      ASTNode::reference_Value(val) => Some(val.into()),
      ASTNode::trimmed_reference_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<member_Value<Token>> for ASTNode<Token> {
  fn from(value: member_Value<Token>) -> Self {
    Self::member_Value(value)
  }
}

impl<Token: Tk> member_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_Member(val) => ASTNode::AST_Member(val),
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      Self::AST_TrimmedReference(val) => ASTNode::AST_TrimmedReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<reference_Value<Token>> for trimmed_reference_Value<Token> {
  fn from(val: reference_Value<Token>) -> trimmed_reference_Value<Token> {
    match val {
      reference_Value::AST_NamedReference(val) => trimmed_reference_Value::AST_NamedReference(val),
      reference_Value::AST_IndexReference(val) => trimmed_reference_Value::AST_IndexReference(val),
      _ => trimmed_reference_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for trimmed_reference_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    trimmed_reference_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for trimmed_reference_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    trimmed_reference_Value::AST_IndexReference(val)
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for trimmed_reference_Value<Token> {
  fn from(val: Box<AST_TrimmedReference<Token>>) -> Self {
    trimmed_reference_Value::AST_TrimmedReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_trimmed_reference_Value(self) -> Option<trimmed_reference_Value<Token>> {
    match self {
      ASTNode::trimmed_reference_Value(val) => Some(val),
      ASTNode::AST_NamedReference(val) => Some(trimmed_reference_Value::AST_NamedReference(val)),
      ASTNode::AST_IndexReference(val) => Some(trimmed_reference_Value::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(trimmed_reference_Value::AST_TrimmedReference(val)),
      ASTNode::reference_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<trimmed_reference_Value<Token>> for ASTNode<Token> {
  fn from(value: trimmed_reference_Value<Token>) -> Self {
    Self::trimmed_reference_Value(value)
  }
}

impl<Token: Tk> trimmed_reference_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      Self::AST_TrimmedReference(val) => ASTNode::AST_TrimmedReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<IntMatch<Token>>> for generic_match_block_group_1_Value<Token> {
  fn from(val: Box<IntMatch<Token>>) -> Self {
    generic_match_block_group_1_Value::IntMatch(val)
  }
}

impl<Token: Tk> From<Box<FailHint>> for generic_match_block_group_1_Value<Token> {
  fn from(val: Box<FailHint>) -> Self {
    generic_match_block_group_1_Value::FailHint(val)
  }
}

impl<Token: Tk> From<Box<DefaultMatch<Token>>> for generic_match_block_group_1_Value<Token> {
  fn from(val: Box<DefaultMatch<Token>>) -> Self {
    generic_match_block_group_1_Value::DefaultMatch(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_generic_match_block_group_1_Value(self) -> Option<generic_match_block_group_1_Value<Token>> {
    match self {
      ASTNode::generic_match_block_group_1_Value(val) => Some(val),
      ASTNode::IntMatch(val) => Some(generic_match_block_group_1_Value::IntMatch(val)),
      ASTNode::FailHint(val) => Some(generic_match_block_group_1_Value::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(generic_match_block_group_1_Value::DefaultMatch(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<generic_match_block_group_1_Value<Token>> for ASTNode<Token> {
  fn from(value: generic_match_block_group_1_Value<Token>) -> Self {
    Self::generic_match_block_group_1_Value(value)
  }
}

impl<Token: Tk> generic_match_block_group_1_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::IntMatch(val) => ASTNode::IntMatch(val),
      Self::FailHint(val) => ASTNode::FailHint(val),
      Self::DefaultMatch(val) => ASTNode::DefaultMatch(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<FailHint>> for nonterminal_match_block_group_Value<Token> {
  fn from(val: Box<FailHint>) -> Self {
    nonterminal_match_block_group_Value::FailHint(val)
  }
}

impl<Token: Tk> From<Box<DefaultMatch<Token>>> for nonterminal_match_block_group_Value<Token> {
  fn from(val: Box<DefaultMatch<Token>>) -> Self {
    nonterminal_match_block_group_Value::DefaultMatch(val)
  }
}

impl<Token: Tk> From<Box<NonTermMatch<Token>>> for nonterminal_match_block_group_Value<Token> {
  fn from(val: Box<NonTermMatch<Token>>) -> Self {
    nonterminal_match_block_group_Value::NonTermMatch(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_nonterminal_match_block_group_Value(self) -> Option<nonterminal_match_block_group_Value<Token>> {
    match self {
      ASTNode::nonterminal_match_block_group_Value(val) => Some(val),
      ASTNode::FailHint(val) => Some(nonterminal_match_block_group_Value::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(nonterminal_match_block_group_Value::DefaultMatch(val)),
      ASTNode::NonTermMatch(val) => Some(nonterminal_match_block_group_Value::NonTermMatch(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<nonterminal_match_block_group_Value<Token>> for ASTNode<Token> {
  fn from(value: nonterminal_match_block_group_Value<Token>) -> Self {
    Self::nonterminal_match_block_group_Value(value)
  }
}

impl<Token: Tk> nonterminal_match_block_group_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::FailHint(val) => ASTNode::FailHint(val),
      Self::DefaultMatch(val) => ASTNode::DefaultMatch(val),
      Self::NonTermMatch(val) => ASTNode::NonTermMatch(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<FailHint>> for terminal_match_block_group_Value<Token> {
  fn from(val: Box<FailHint>) -> Self {
    terminal_match_block_group_Value::FailHint(val)
  }
}

impl<Token: Tk> From<Box<DefaultMatch<Token>>> for terminal_match_block_group_Value<Token> {
  fn from(val: Box<DefaultMatch<Token>>) -> Self {
    terminal_match_block_group_Value::DefaultMatch(val)
  }
}

impl<Token: Tk> From<Box<TermMatch<Token>>> for terminal_match_block_group_Value<Token> {
  fn from(val: Box<TermMatch<Token>>) -> Self {
    terminal_match_block_group_Value::TermMatch(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_terminal_match_block_group_Value(self) -> Option<terminal_match_block_group_Value<Token>> {
    match self {
      ASTNode::terminal_match_block_group_Value(val) => Some(val),
      ASTNode::FailHint(val) => Some(terminal_match_block_group_Value::FailHint(val)),
      ASTNode::DefaultMatch(val) => Some(terminal_match_block_group_Value::DefaultMatch(val)),
      ASTNode::TermMatch(val) => Some(terminal_match_block_group_Value::TermMatch(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<terminal_match_block_group_Value<Token>> for ASTNode<Token> {
  fn from(value: terminal_match_block_group_Value<Token>) -> Self {
    Self::terminal_match_block_group_Value(value)
  }
}

impl<Token: Tk> terminal_match_block_group_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::FailHint(val) => ASTNode::FailHint(val),
      Self::DefaultMatch(val) => ASTNode::DefaultMatch(val),
      Self::TermMatch(val) => ASTNode::TermMatch(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<symbol_Value<Token>> for annotated_symbol_Value<Token> {
  fn from(val: symbol_Value<Token>) -> annotated_symbol_Value<Token> {
    match val {
      symbol_Value::NonTerminal_Terminal_Symbol(val) => annotated_symbol_Value::NonTerminal_Terminal_Symbol(val),
      symbol_Value::NonTerminal_Symbol(val) => annotated_symbol_Value::NonTerminal_Symbol(val),
      symbol_Value::TokenGroupRules(val) => annotated_symbol_Value::TokenGroupRules(val),
      symbol_Value::Grouped_Rules(val) => annotated_symbol_Value::Grouped_Rules(val),
      symbol_Value::ClassSymbol(val) => annotated_symbol_Value::ClassSymbol(val),
      symbol_Value::NonTerminal_Import_Symbol(val) => annotated_symbol_Value::NonTerminal_Import_Symbol(val),
      symbol_Value::TerminalToken(val) => annotated_symbol_Value::TerminalToken(val),
      symbol_Value::Template_NonTerminal_Symbol(val) => annotated_symbol_Value::Template_NonTerminal_Symbol(val),
      _ => annotated_symbol_Value::None,
    }
  }
}

impl<Token: Tk> From<list_Value<Token>> for annotated_symbol_Value<Token> {
  fn from(val: list_Value<Token>) -> annotated_symbol_Value<Token> {
    match val {
      list_Value::NonTerminal_Terminal_Symbol(val) => annotated_symbol_Value::NonTerminal_Terminal_Symbol(val),
      list_Value::NonTerminal_Symbol(val) => annotated_symbol_Value::NonTerminal_Symbol(val),
      list_Value::TokenGroupRules(val) => annotated_symbol_Value::TokenGroupRules(val),
      list_Value::Grouped_Rules(val) => annotated_symbol_Value::Grouped_Rules(val),
      list_Value::ClassSymbol(val) => annotated_symbol_Value::ClassSymbol(val),
      list_Value::NonTerminal_Import_Symbol(val) => annotated_symbol_Value::NonTerminal_Import_Symbol(val),
      list_Value::TerminalToken(val) => annotated_symbol_Value::TerminalToken(val),
      list_Value::List_Rules(val) => annotated_symbol_Value::List_Rules(val),
      list_Value::Template_NonTerminal_Symbol(val) => annotated_symbol_Value::Template_NonTerminal_Symbol(val),
      _ => annotated_symbol_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    annotated_symbol_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    annotated_symbol_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    annotated_symbol_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    annotated_symbol_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    annotated_symbol_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    annotated_symbol_Value::NonTerminal_Import_Symbol(val)
  }
}

impl<Token: Tk> From<Box<AnnotatedSymbol<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<AnnotatedSymbol<Token>>) -> Self {
    annotated_symbol_Value::AnnotatedSymbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    annotated_symbol_Value::TerminalToken(val)
  }
}

impl<Token: Tk> From<Box<List_Rules<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<List_Rules<Token>>) -> Self {
    annotated_symbol_Value::List_Rules(val)
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for annotated_symbol_Value<Token> {
  fn from(val: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    annotated_symbol_Value::Template_NonTerminal_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_annotated_symbol_Value(self) -> Option<annotated_symbol_Value<Token>> {
    match self {
      ASTNode::annotated_symbol_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(annotated_symbol_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::NonTerminal_Symbol(val) => Some(annotated_symbol_Value::NonTerminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(annotated_symbol_Value::TokenGroupRules(val)),
      ASTNode::Grouped_Rules(val) => Some(annotated_symbol_Value::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(annotated_symbol_Value::ClassSymbol(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(annotated_symbol_Value::NonTerminal_Import_Symbol(val)),
      ASTNode::AnnotatedSymbol(val) => Some(annotated_symbol_Value::AnnotatedSymbol(val)),
      ASTNode::TerminalToken(val) => Some(annotated_symbol_Value::TerminalToken(val)),
      ASTNode::List_Rules(val) => Some(annotated_symbol_Value::List_Rules(val)),
      ASTNode::Template_NonTerminal_Symbol(val) => Some(annotated_symbol_Value::Template_NonTerminal_Symbol(val)),
      ASTNode::symbol_Value(val) => Some(val.into()),
      ASTNode::list_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<annotated_symbol_Value<Token>> for ASTNode<Token> {
  fn from(value: annotated_symbol_Value<Token>) -> Self {
    Self::annotated_symbol_Value(value)
  }
}

impl<Token: Tk> annotated_symbol_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      Self::AnnotatedSymbol(val) => ASTNode::AnnotatedSymbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      Self::List_Rules(val) => ASTNode::List_Rules(val),
      Self::Template_NonTerminal_Symbol(val) => ASTNode::Template_NonTerminal_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<symbol_Value<Token>> for template_arg_Value<Token> {
  fn from(val: symbol_Value<Token>) -> template_arg_Value<Token> {
    match val {
      symbol_Value::NonTerminal_Terminal_Symbol(val) => template_arg_Value::NonTerminal_Terminal_Symbol(val),
      symbol_Value::NonTerminal_Symbol(val) => template_arg_Value::NonTerminal_Symbol(val),
      symbol_Value::TokenGroupRules(val) => template_arg_Value::TokenGroupRules(val),
      symbol_Value::Grouped_Rules(val) => template_arg_Value::Grouped_Rules(val),
      symbol_Value::ClassSymbol(val) => template_arg_Value::ClassSymbol(val),
      symbol_Value::NonTerminal_Import_Symbol(val) => template_arg_Value::NonTerminal_Import_Symbol(val),
      symbol_Value::TerminalToken(val) => template_arg_Value::TerminalToken(val),
      symbol_Value::Template_NonTerminal_Symbol(val) => template_arg_Value::Template_NonTerminal_Symbol(val),
      _ => template_arg_Value::None,
    }
  }
}

impl<Token: Tk> From<list_Value<Token>> for template_arg_Value<Token> {
  fn from(val: list_Value<Token>) -> template_arg_Value<Token> {
    match val {
      list_Value::NonTerminal_Terminal_Symbol(val) => template_arg_Value::NonTerminal_Terminal_Symbol(val),
      list_Value::NonTerminal_Symbol(val) => template_arg_Value::NonTerminal_Symbol(val),
      list_Value::TokenGroupRules(val) => template_arg_Value::TokenGroupRules(val),
      list_Value::Grouped_Rules(val) => template_arg_Value::Grouped_Rules(val),
      list_Value::ClassSymbol(val) => template_arg_Value::ClassSymbol(val),
      list_Value::NonTerminal_Import_Symbol(val) => template_arg_Value::NonTerminal_Import_Symbol(val),
      list_Value::TerminalToken(val) => template_arg_Value::TerminalToken(val),
      list_Value::List_Rules(val) => template_arg_Value::List_Rules(val),
      list_Value::Template_NonTerminal_Symbol(val) => template_arg_Value::Template_NonTerminal_Symbol(val),
      _ => template_arg_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    template_arg_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    template_arg_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    template_arg_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    template_arg_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    template_arg_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    template_arg_Value::NonTerminal_Import_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    template_arg_Value::TerminalToken(val)
  }
}

impl<Token: Tk> From<Box<AST_STRUCT_TEMPLATE_NAME>> for template_arg_Value<Token> {
  fn from(val: Box<AST_STRUCT_TEMPLATE_NAME>) -> Self {
    template_arg_Value::AST_STRUCT_TEMPLATE_NAME(val)
  }
}

impl<Token: Tk> From<Box<List_Rules<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<List_Rules<Token>>) -> Self {
    template_arg_Value::List_Rules(val)
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for template_arg_Value<Token> {
  fn from(val: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    template_arg_Value::Template_NonTerminal_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_template_arg_Value(self) -> Option<template_arg_Value<Token>> {
    match self {
      ASTNode::template_arg_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(template_arg_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::NonTerminal_Symbol(val) => Some(template_arg_Value::NonTerminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(template_arg_Value::TokenGroupRules(val)),
      ASTNode::Grouped_Rules(val) => Some(template_arg_Value::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(template_arg_Value::ClassSymbol(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(template_arg_Value::NonTerminal_Import_Symbol(val)),
      ASTNode::TerminalToken(val) => Some(template_arg_Value::TerminalToken(val)),
      ASTNode::AST_STRUCT_TEMPLATE_NAME(val) => Some(template_arg_Value::AST_STRUCT_TEMPLATE_NAME(val)),
      ASTNode::List_Rules(val) => Some(template_arg_Value::List_Rules(val)),
      ASTNode::Template_NonTerminal_Symbol(val) => Some(template_arg_Value::Template_NonTerminal_Symbol(val)),
      ASTNode::symbol_Value(val) => Some(val.into()),
      ASTNode::list_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<template_arg_Value<Token>> for ASTNode<Token> {
  fn from(value: template_arg_Value<Token>) -> Self {
    Self::template_arg_Value(value)
  }
}

impl<Token: Tk> template_arg_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      Self::AST_STRUCT_TEMPLATE_NAME(val) => ASTNode::AST_STRUCT_TEMPLATE_NAME(val),
      Self::List_Rules(val) => ASTNode::List_Rules(val),
      Self::Template_NonTerminal_Symbol(val) => ASTNode::Template_NonTerminal_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<expr_Value<Token>> for struct_list_Value<Token> {
  fn from(val: expr_Value<Token>) -> struct_list_Value<Token> {
    match val {
      expr_Value::AST_I8(val) => struct_list_Value::AST_I8(val),
      expr_Value::AST_U8(val) => struct_list_Value::AST_U8(val),
      expr_Value::AST_F32(val) => struct_list_Value::AST_F32(val),
      expr_Value::AST_I32(val) => struct_list_Value::AST_I32(val),
      expr_Value::AST_U32(val) => struct_list_Value::AST_U32(val),
      expr_Value::AST_F64(val) => struct_list_Value::AST_F64(val),
      expr_Value::AST_I64(val) => struct_list_Value::AST_I64(val),
      expr_Value::AST_U64(val) => struct_list_Value::AST_U64(val),
      expr_Value::AST_F16(val) => struct_list_Value::AST_F16(val),
      expr_Value::AST_I16(val) => struct_list_Value::AST_I16(val),
      expr_Value::AST_U16(val) => struct_list_Value::AST_U16(val),
      expr_Value::AST_Sub(val) => struct_list_Value::AST_Sub(val),
      expr_Value::AST_Add(val) => struct_list_Value::AST_Add(val),
      expr_Value::AST_Mod(val) => struct_list_Value::AST_Mod(val),
      expr_Value::AST_Neg(val) => struct_list_Value::AST_Neg(val),
      expr_Value::AST_Mul(val) => struct_list_Value::AST_Mul(val),
      expr_Value::AST_Map(val) => struct_list_Value::AST_Map(val),
      expr_Value::AST_Div(val) => struct_list_Value::AST_Div(val),
      expr_Value::AST_Pow(val) => struct_list_Value::AST_Pow(val),
      expr_Value::AST_F128(val) => struct_list_Value::AST_F128(val),
      expr_Value::AST_U128(val) => struct_list_Value::AST_U128(val),
      expr_Value::AST_Bool(val) => struct_list_Value::AST_Bool(val),
      expr_Value::AST_Member(val) => struct_list_Value::AST_Member(val),
      expr_Value::AST_NamedReference(val) => struct_list_Value::AST_NamedReference(val),
      expr_Value::AST_StringLiteral(val) => struct_list_Value::AST_StringLiteral(val),
      expr_Value::AST_BoolLiteral(val) => struct_list_Value::AST_BoolLiteral(val),
      expr_Value::AST_Vector(val) => struct_list_Value::AST_Vector(val),
      expr_Value::AST_String(val) => struct_list_Value::AST_String(val),
      expr_Value::AST_NumberLiteral(val) => struct_list_Value::AST_NumberLiteral(val),
      expr_Value::AST_IndexReference(val) => struct_list_Value::AST_IndexReference(val),
      expr_Value::AST_TrimmedReference(val) => struct_list_Value::AST_TrimmedReference(val),
      _ => struct_list_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I8<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_I8<Token>>) -> Self {
    struct_list_Value::AST_I8(val)
  }
}

impl<Token: Tk> From<Box<AST_U8<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_U8<Token>>) -> Self {
    struct_list_Value::AST_U8(val)
  }
}

impl<Token: Tk> From<Box<AST_F32<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_F32<Token>>) -> Self {
    struct_list_Value::AST_F32(val)
  }
}

impl<Token: Tk> From<Box<AST_I32<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_I32<Token>>) -> Self {
    struct_list_Value::AST_I32(val)
  }
}

impl<Token: Tk> From<Box<AST_U32<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_U32<Token>>) -> Self {
    struct_list_Value::AST_U32(val)
  }
}

impl<Token: Tk> From<Box<AST_F64<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_F64<Token>>) -> Self {
    struct_list_Value::AST_F64(val)
  }
}

impl<Token: Tk> From<Box<AST_I64<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_I64<Token>>) -> Self {
    struct_list_Value::AST_I64(val)
  }
}

impl<Token: Tk> From<Box<AST_U64<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_U64<Token>>) -> Self {
    struct_list_Value::AST_U64(val)
  }
}

impl<Token: Tk> From<Box<AST_F16<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_F16<Token>>) -> Self {
    struct_list_Value::AST_F16(val)
  }
}

impl<Token: Tk> From<Box<AST_I16<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_I16<Token>>) -> Self {
    struct_list_Value::AST_I16(val)
  }
}

impl<Token: Tk> From<Box<AST_U16<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_U16<Token>>) -> Self {
    struct_list_Value::AST_U16(val)
  }
}

impl<Token: Tk> From<Box<AST_Sub<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Sub<Token>>) -> Self {
    struct_list_Value::AST_Sub(val)
  }
}

impl<Token: Tk> From<Box<AST_Add<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Add<Token>>) -> Self {
    struct_list_Value::AST_Add(val)
  }
}

impl<Token: Tk> From<Box<AST_Mod<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Mod<Token>>) -> Self {
    struct_list_Value::AST_Mod(val)
  }
}

impl<Token: Tk> From<Box<AST_Neg<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Neg<Token>>) -> Self {
    struct_list_Value::AST_Neg(val)
  }
}

impl<Token: Tk> From<Box<AST_Mul<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Mul<Token>>) -> Self {
    struct_list_Value::AST_Mul(val)
  }
}

impl<Token: Tk> From<Box<AST_Map<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Map<Token>>) -> Self {
    struct_list_Value::AST_Map(val)
  }
}

impl<Token: Tk> From<Box<AST_Div<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Div<Token>>) -> Self {
    struct_list_Value::AST_Div(val)
  }
}

impl<Token: Tk> From<Box<AST_Pow<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Pow<Token>>) -> Self {
    struct_list_Value::AST_Pow(val)
  }
}

impl<Token: Tk> From<Box<AST_F128<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_F128<Token>>) -> Self {
    struct_list_Value::AST_F128(val)
  }
}

impl<Token: Tk> From<Box<AST_U128<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_U128<Token>>) -> Self {
    struct_list_Value::AST_U128(val)
  }
}

impl<Token: Tk> From<Box<AST_Bool<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Bool<Token>>) -> Self {
    struct_list_Value::AST_Bool(val)
  }
}

impl<Token: Tk> From<Box<AST_Property<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Property<Token>>) -> Self {
    struct_list_Value::AST_Property(val)
  }
}

impl<Token: Tk> From<Box<AST_Member<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Member<Token>>) -> Self {
    struct_list_Value::AST_Member(val)
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_NamedReference<Token>>) -> Self {
    struct_list_Value::AST_NamedReference(val)
  }
}

impl<Token: Tk> From<Box<AST_Token>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Token>) -> Self {
    struct_list_Value::AST_Token(val)
  }
}

impl<Token: Tk> From<Box<AST_StringLiteral<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_StringLiteral<Token>>) -> Self {
    struct_list_Value::AST_StringLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_BoolLiteral>> for struct_list_Value<Token> {
  fn from(val: Box<AST_BoolLiteral>) -> Self {
    struct_list_Value::AST_BoolLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_Vector<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Vector<Token>>) -> Self {
    struct_list_Value::AST_Vector(val)
  }
}

impl<Token: Tk> From<Box<AST_String<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_String<Token>>) -> Self {
    struct_list_Value::AST_String(val)
  }
}

impl<Token: Tk> From<Box<AST_NumberLiteral>> for struct_list_Value<Token> {
  fn from(val: Box<AST_NumberLiteral>) -> Self {
    struct_list_Value::AST_NumberLiteral(val)
  }
}

impl<Token: Tk> From<Box<AST_Struct<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_Struct<Token>>) -> Self {
    struct_list_Value::AST_Struct(val)
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_IndexReference<Token>>) -> Self {
    struct_list_Value::AST_IndexReference(val)
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for struct_list_Value<Token> {
  fn from(val: Box<AST_TrimmedReference<Token>>) -> Self {
    struct_list_Value::AST_TrimmedReference(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_struct_list_Value(self) -> Option<struct_list_Value<Token>> {
    match self {
      ASTNode::struct_list_Value(val) => Some(val),
      ASTNode::AST_I8(val) => Some(struct_list_Value::AST_I8(val)),
      ASTNode::AST_U8(val) => Some(struct_list_Value::AST_U8(val)),
      ASTNode::AST_F32(val) => Some(struct_list_Value::AST_F32(val)),
      ASTNode::AST_I32(val) => Some(struct_list_Value::AST_I32(val)),
      ASTNode::AST_U32(val) => Some(struct_list_Value::AST_U32(val)),
      ASTNode::AST_F64(val) => Some(struct_list_Value::AST_F64(val)),
      ASTNode::AST_I64(val) => Some(struct_list_Value::AST_I64(val)),
      ASTNode::AST_U64(val) => Some(struct_list_Value::AST_U64(val)),
      ASTNode::AST_F16(val) => Some(struct_list_Value::AST_F16(val)),
      ASTNode::AST_I16(val) => Some(struct_list_Value::AST_I16(val)),
      ASTNode::AST_U16(val) => Some(struct_list_Value::AST_U16(val)),
      ASTNode::AST_Sub(val) => Some(struct_list_Value::AST_Sub(val)),
      ASTNode::AST_Add(val) => Some(struct_list_Value::AST_Add(val)),
      ASTNode::AST_Mod(val) => Some(struct_list_Value::AST_Mod(val)),
      ASTNode::AST_Neg(val) => Some(struct_list_Value::AST_Neg(val)),
      ASTNode::AST_Mul(val) => Some(struct_list_Value::AST_Mul(val)),
      ASTNode::AST_Map(val) => Some(struct_list_Value::AST_Map(val)),
      ASTNode::AST_Div(val) => Some(struct_list_Value::AST_Div(val)),
      ASTNode::AST_Pow(val) => Some(struct_list_Value::AST_Pow(val)),
      ASTNode::AST_F128(val) => Some(struct_list_Value::AST_F128(val)),
      ASTNode::AST_U128(val) => Some(struct_list_Value::AST_U128(val)),
      ASTNode::AST_Bool(val) => Some(struct_list_Value::AST_Bool(val)),
      ASTNode::AST_Property(val) => Some(struct_list_Value::AST_Property(val)),
      ASTNode::AST_Member(val) => Some(struct_list_Value::AST_Member(val)),
      ASTNode::AST_NamedReference(val) => Some(struct_list_Value::AST_NamedReference(val)),
      ASTNode::AST_Token(val) => Some(struct_list_Value::AST_Token(val)),
      ASTNode::AST_StringLiteral(val) => Some(struct_list_Value::AST_StringLiteral(val)),
      ASTNode::AST_BoolLiteral(val) => Some(struct_list_Value::AST_BoolLiteral(val)),
      ASTNode::AST_Vector(val) => Some(struct_list_Value::AST_Vector(val)),
      ASTNode::AST_String(val) => Some(struct_list_Value::AST_String(val)),
      ASTNode::AST_NumberLiteral(val) => Some(struct_list_Value::AST_NumberLiteral(val)),
      ASTNode::AST_Struct(val) => Some(struct_list_Value::AST_Struct(val)),
      ASTNode::AST_IndexReference(val) => Some(struct_list_Value::AST_IndexReference(val)),
      ASTNode::AST_TrimmedReference(val) => Some(struct_list_Value::AST_TrimmedReference(val)),
      ASTNode::expr_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<struct_list_Value<Token>> for ASTNode<Token> {
  fn from(value: struct_list_Value<Token>) -> Self {
    Self::struct_list_Value(value)
  }
}

impl<Token: Tk> struct_list_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::AST_I8(val) => ASTNode::AST_I8(val),
      Self::AST_U8(val) => ASTNode::AST_U8(val),
      Self::AST_F32(val) => ASTNode::AST_F32(val),
      Self::AST_I32(val) => ASTNode::AST_I32(val),
      Self::AST_U32(val) => ASTNode::AST_U32(val),
      Self::AST_F64(val) => ASTNode::AST_F64(val),
      Self::AST_I64(val) => ASTNode::AST_I64(val),
      Self::AST_U64(val) => ASTNode::AST_U64(val),
      Self::AST_F16(val) => ASTNode::AST_F16(val),
      Self::AST_I16(val) => ASTNode::AST_I16(val),
      Self::AST_U16(val) => ASTNode::AST_U16(val),
      Self::AST_Sub(val) => ASTNode::AST_Sub(val),
      Self::AST_Add(val) => ASTNode::AST_Add(val),
      Self::AST_Mod(val) => ASTNode::AST_Mod(val),
      Self::AST_Neg(val) => ASTNode::AST_Neg(val),
      Self::AST_Mul(val) => ASTNode::AST_Mul(val),
      Self::AST_Map(val) => ASTNode::AST_Map(val),
      Self::AST_Div(val) => ASTNode::AST_Div(val),
      Self::AST_Pow(val) => ASTNode::AST_Pow(val),
      Self::AST_F128(val) => ASTNode::AST_F128(val),
      Self::AST_U128(val) => ASTNode::AST_U128(val),
      Self::AST_Bool(val) => ASTNode::AST_Bool(val),
      Self::AST_Property(val) => ASTNode::AST_Property(val),
      Self::AST_Member(val) => ASTNode::AST_Member(val),
      Self::AST_NamedReference(val) => ASTNode::AST_NamedReference(val),
      Self::AST_Token(val) => ASTNode::AST_Token(val),
      Self::AST_StringLiteral(val) => ASTNode::AST_StringLiteral(val),
      Self::AST_BoolLiteral(val) => ASTNode::AST_BoolLiteral(val),
      Self::AST_Vector(val) => ASTNode::AST_Vector(val),
      Self::AST_String(val) => ASTNode::AST_String(val),
      Self::AST_NumberLiteral(val) => ASTNode::AST_NumberLiteral(val),
      Self::AST_Struct(val) => ASTNode::AST_Struct(val),
      Self::AST_IndexReference(val) => ASTNode::AST_IndexReference(val),
      Self::AST_TrimmedReference(val) => ASTNode::AST_TrimmedReference(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for ignore_clause_list_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    ignore_clause_list_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for ignore_clause_list_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    ignore_clause_list_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for ignore_clause_list_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    ignore_clause_list_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for ignore_clause_list_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    ignore_clause_list_Value::TerminalToken(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_ignore_clause_list_Value(self) -> Option<ignore_clause_list_Value<Token>> {
    match self {
      ASTNode::ignore_clause_list_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(ignore_clause_list_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(ignore_clause_list_Value::TokenGroupRules(val)),
      ASTNode::ClassSymbol(val) => Some(ignore_clause_list_Value::ClassSymbol(val)),
      ASTNode::TerminalToken(val) => Some(ignore_clause_list_Value::TerminalToken(val)),
      _ => None,
    }
  }
}

impl<Token: Tk> From<ignore_clause_list_Value<Token>> for ASTNode<Token> {
  fn from(value: ignore_clause_list_Value<Token>) -> Self {
    Self::ignore_clause_list_Value(value)
  }
}

impl<Token: Tk> ignore_clause_list_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> From<symbol_Value<Token>> for list_Value<Token> {
  fn from(val: symbol_Value<Token>) -> list_Value<Token> {
    match val {
      symbol_Value::NonTerminal_Terminal_Symbol(val) => list_Value::NonTerminal_Terminal_Symbol(val),
      symbol_Value::NonTerminal_Symbol(val) => list_Value::NonTerminal_Symbol(val),
      symbol_Value::TokenGroupRules(val) => list_Value::TokenGroupRules(val),
      symbol_Value::Grouped_Rules(val) => list_Value::Grouped_Rules(val),
      symbol_Value::ClassSymbol(val) => list_Value::ClassSymbol(val),
      symbol_Value::NonTerminal_Import_Symbol(val) => list_Value::NonTerminal_Import_Symbol(val),
      symbol_Value::TerminalToken(val) => list_Value::TerminalToken(val),
      symbol_Value::Template_NonTerminal_Symbol(val) => list_Value::Template_NonTerminal_Symbol(val),
      _ => list_Value::None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for list_Value<Token> {
  fn from(val: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    list_Value::NonTerminal_Terminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for list_Value<Token> {
  fn from(val: Box<NonTerminal_Symbol<Token>>) -> Self {
    list_Value::NonTerminal_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for list_Value<Token> {
  fn from(val: Box<TokenGroupRules<Token>>) -> Self {
    list_Value::TokenGroupRules(val)
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for list_Value<Token> {
  fn from(val: Box<Grouped_Rules<Token>>) -> Self {
    list_Value::Grouped_Rules(val)
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for list_Value<Token> {
  fn from(val: Box<ClassSymbol<Token>>) -> Self {
    list_Value::ClassSymbol(val)
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for list_Value<Token> {
  fn from(val: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    list_Value::NonTerminal_Import_Symbol(val)
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for list_Value<Token> {
  fn from(val: Box<TerminalToken<Token>>) -> Self {
    list_Value::TerminalToken(val)
  }
}

impl<Token: Tk> From<Box<List_Rules<Token>>> for list_Value<Token> {
  fn from(val: Box<List_Rules<Token>>) -> Self {
    list_Value::List_Rules(val)
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for list_Value<Token> {
  fn from(val: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    list_Value::Template_NonTerminal_Symbol(val)
  }
}
impl<Token: Tk> ASTNode<Token> {
  pub fn into_list_Value(self) -> Option<list_Value<Token>> {
    match self {
      ASTNode::list_Value(val) => Some(val),
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(list_Value::NonTerminal_Terminal_Symbol(val)),
      ASTNode::NonTerminal_Symbol(val) => Some(list_Value::NonTerminal_Symbol(val)),
      ASTNode::TokenGroupRules(val) => Some(list_Value::TokenGroupRules(val)),
      ASTNode::Grouped_Rules(val) => Some(list_Value::Grouped_Rules(val)),
      ASTNode::ClassSymbol(val) => Some(list_Value::ClassSymbol(val)),
      ASTNode::NonTerminal_Import_Symbol(val) => Some(list_Value::NonTerminal_Import_Symbol(val)),
      ASTNode::TerminalToken(val) => Some(list_Value::TerminalToken(val)),
      ASTNode::List_Rules(val) => Some(list_Value::List_Rules(val)),
      ASTNode::Template_NonTerminal_Symbol(val) => Some(list_Value::Template_NonTerminal_Symbol(val)),
      ASTNode::symbol_Value(val) => Some(val.into()),
      _ => None,
    }
  }
}

impl<Token: Tk> From<list_Value<Token>> for ASTNode<Token> {
  fn from(value: list_Value<Token>) -> Self {
    Self::list_Value(value)
  }
}

impl<Token: Tk> list_Value<Token> {
  pub fn to_ast(self) -> ASTNode<Token> {
    match self {
      Self::NonTerminal_Terminal_Symbol(val) => ASTNode::NonTerminal_Terminal_Symbol(val),
      Self::NonTerminal_Symbol(val) => ASTNode::NonTerminal_Symbol(val),
      Self::TokenGroupRules(val) => ASTNode::TokenGroupRules(val),
      Self::Grouped_Rules(val) => ASTNode::Grouped_Rules(val),
      Self::ClassSymbol(val) => ASTNode::ClassSymbol(val),
      Self::NonTerminal_Import_Symbol(val) => ASTNode::NonTerminal_Import_Symbol(val),
      Self::TerminalToken(val) => ASTNode::TerminalToken(val),
      Self::List_Rules(val) => ASTNode::List_Rules(val),
      Self::Template_NonTerminal_Symbol(val) => ASTNode::Template_NonTerminal_Symbol(val),
      _ => ASTNode::None,
    }
  }
}

impl<Token: Tk> ASTNode<Token> {
  pub fn token(&self) -> Token {
    match self {
      ASTNode::Pop(n) => n.tok.clone(),
      ASTNode::Rule(n) => n.tok.clone(),
      ASTNode::Push(n) => n.tok.clone(),
      ASTNode::Peek(n) => n.tok.clone(),
      ASTNode::Fork(n) => n.tok.clone(),
      ASTNode::Fail(n) => n.tok.clone(),
      ASTNode::Goto(n) => n.tok.clone(),
      ASTNode::Pass(n) => n.tok.clone(),
      ASTNode::State(n) => n.tok.clone(),
      ASTNode::Reset(n) => n.tok.clone(),
      ASTNode::Shift(n) => n.tok.clone(),
      ASTNode::AST_I8(n) => n.tok.clone(),
      ASTNode::AST_U8(n) => n.tok.clone(),
      ASTNode::Reduce(n) => n.tok.clone(),
      ASTNode::Accept(n) => n.tok.clone(),
      ASTNode::Import(n) => n.tok.clone(),
      ASTNode::AST_F32(n) => n.tok.clone(),
      ASTNode::AST_I32(n) => n.tok.clone(),
      ASTNode::AST_U32(n) => n.tok.clone(),
      ASTNode::AST_F64(n) => n.tok.clone(),
      ASTNode::AST_I64(n) => n.tok.clone(),
      ASTNode::AST_U64(n) => n.tok.clone(),
      ASTNode::AST_F16(n) => n.tok.clone(),
      ASTNode::AST_I16(n) => n.tok.clone(),
      ASTNode::AST_U16(n) => n.tok.clone(),
      ASTNode::AST_Sub(n) => n.tok.clone(),
      ASTNode::AST_Add(n) => n.tok.clone(),
      ASTNode::AST_Mod(n) => n.tok.clone(),
      ASTNode::SetLine(n) => n.tok.clone(),
      ASTNode::AST_Neg(n) => n.tok.clone(),
      ASTNode::AST_Mul(n) => n.tok.clone(),
      ASTNode::AST_Map(n) => n.tok.clone(),
      ASTNode::Grammar(n) => n.tok.clone(),
      ASTNode::Matches(n) => n.tok.clone(),
      ASTNode::CFRules(n) => n.tok.clone(),
      ASTNode::Ascript(n) => n.tok.clone(),
      ASTNode::AST_Div(n) => n.tok.clone(),
      ASTNode::AST_Pow(n) => n.tok.clone(),
      ASTNode::AST_F128(n) => n.tok.clone(),
      ASTNode::AST_U128(n) => n.tok.clone(),
      ASTNode::AST_Flag(n) => n.tok.clone(),
      ASTNode::AST_Bool(n) => n.tok.clone(),
      ASTNode::PegRules(n) => n.tok.clone(),
      ASTNode::AST_Property(n) => n.tok.clone(),
      ASTNode::NonTerminal_Terminal_Symbol(n) => n.tok.clone(),
      ASTNode::AST_NamedReference(n) => n.tok.clone(),
      ASTNode::NonTerminal_Symbol(n) => n.tok.clone(),
      ASTNode::ReduceRaw(n) => n.tok.clone(),
      ASTNode::TokenGroupRules(n) => n.tok.clone(),
      ASTNode::NotEmptySet(n) => n.tok.clone(),
      ASTNode::AST_StringLiteral(n) => n.tok.clone(),
      ASTNode::TemplateRules(n) => n.tok.clone(),
      ASTNode::Grouped_Rules(n) => n.tok.clone(),
      ASTNode::ClassSymbol(n) => n.tok.clone(),
      ASTNode::AST_Vector(n) => n.tok.clone(),
      ASTNode::NonTerminal_Import_Symbol(n) => n.tok.clone(),
      ASTNode::AppendRules(n) => n.tok.clone(),
      ASTNode::SetTokenId(n) => n.tok.clone(),
      ASTNode::AST_String(n) => n.tok.clone(),
      ASTNode::AnnotatedSymbol(n) => n.tok.clone(),
      ASTNode::TerminalToken(n) => n.tok.clone(),
      ASTNode::EOFSymbol(n) => n.tok.clone(),
      ASTNode::AST_Statement(n) => n.tok.clone(),
      ASTNode::AST_Struct(n) => n.tok.clone(),
      ASTNode::List_Rules(n) => n.tok.clone(),
      ASTNode::Template_NonTerminal_Symbol(n) => n.tok.clone(),
      ASTNode::AST_IndexReference(n) => n.tok.clone(),
      ASTNode::AST_TrimmedReference(n) => n.tok.clone(),
      ASTNode::Token(tok) => tok.clone(),
      _ => Default::default(),
    }
  }
}

/*impl<Token:Tk> std::hash::Hash for ASTNode<Token> {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H){match self{
      ASTNode::Pop(n) => n.hash(hasher),
      ASTNode::Rule(n) => n.hash(hasher),
      ASTNode::Name(n) => n.hash(hasher),
      ASTNode::Push(n) => n.hash(hasher),
      ASTNode::Peek(n) => n.hash(hasher),
      ASTNode::Fork(n) => n.hash(hasher),
      ASTNode::Fail(n) => n.hash(hasher),
      ASTNode::Goto(n) => n.hash(hasher),
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
      ASTNode::Ignore(n) => n.hash(hasher),
      ASTNode::Accept(n) => n.hash(hasher),
      ASTNode::Import(n) => n.hash(hasher),
      ASTNode::Export(n) => n.hash(hasher),
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
      ASTNode::Grammar(n) => n.hash(hasher),
      ASTNode::Matches(n) => n.hash(hasher),
      ASTNode::CFRules(n) => n.hash(hasher),
      ASTNode::Ascript(n) => n.hash(hasher),
      ASTNode::AST_Div(n) => n.hash(hasher),
      ASTNode::AST_Pow(n) => n.hash(hasher),
      ASTNode::AST_F128(n) => n.hash(hasher),
      ASTNode::AST_U128(n) => n.hash(hasher),
      ASTNode::AST_Flag(n) => n.hash(hasher),
      ASTNode::IntMatch(n) => n.hash(hasher),
      ASTNode::AST_Bool(n) => n.hash(hasher),
      ASTNode::PegRules(n) => n.hash(hasher),
      ASTNode::FailHint(n) => n.hash(hasher),
      ASTNode::DEFINED_TYPE_NUM(n) => n.hash(hasher),
      ASTNode::AST_Property(n) => n.hash(hasher),
      ASTNode::NonTerminal_Terminal_Symbol(n) => n.hash(hasher),
      ASTNode::AST_Member(n) => n.hash(hasher),
      ASTNode::AST_NamedReference(n) => n.hash(hasher),
      ASTNode::NonTerminal_Symbol(n) => n.hash(hasher),
      ASTNode::ReduceRaw(n) => n.hash(hasher),
      ASTNode::TokenGroupRules(n) => n.hash(hasher),
      ASTNode::DefaultMatch(n) => n.hash(hasher),
      ASTNode::NotEmptySet(n) => n.hash(hasher),
      ASTNode::Statement(n) => n.hash(hasher),
      ASTNode::TemplateSym(n) => n.hash(hasher),
      ASTNode::AST_Token(n) => n.hash(hasher),
      ASTNode::AST_StringLiteral(n) => n.hash(hasher),
      ASTNode::TemplateRules(n) => n.hash(hasher),
      ASTNode::Grouped_Rules(n) => n.hash(hasher),
      ASTNode::AST_BoolLiteral(n) => n.hash(hasher),
      ASTNode::Precedence(n) => n.hash(hasher),
      ASTNode::NonTermMatch(n) => n.hash(hasher),
      ASTNode::ClassSymbol(n) => n.hash(hasher),
      ASTNode::AST_Vector(n) => n.hash(hasher),
      ASTNode::NonTerminal_Import_Symbol(n) => n.hash(hasher),
      ASTNode::AppendRules(n) => n.hash(hasher),
      ASTNode::TemplateASTType(n) => n.hash(hasher),
      ASTNode::SetTokenId(n) => n.hash(hasher),
      ASTNode::AST_String(n) => n.hash(hasher),
      ASTNode::AnnotatedSymbol(n) => n.hash(hasher),
      ASTNode::AST_NumberLiteral(n) => n.hash(hasher),
      ASTNode::SetTokenLen(n) => n.hash(hasher),
      ASTNode::TerminalToken(n) => n.hash(hasher),
      ASTNode::AST_STRUCT_TEMPLATE_NAME(n) => n.hash(hasher),
      ASTNode::EOFSymbol(n) => n.hash(hasher),
      ASTNode::AST_Statement(n) => n.hash(hasher),
      ASTNode::AST_Struct(n) => n.hash(hasher),
      ASTNode::DEFINED_TYPE_IDENT(n) => n.hash(hasher),
      ASTNode::List_Rules(n) => n.hash(hasher),
      ASTNode::ProductionMatches(n) => n.hash(hasher),
      ASTNode::Template_NonTerminal_Symbol(n) => n.hash(hasher),
      ASTNode::AST_IndexReference(n) => n.hash(hasher),
      ASTNode::TermMatch(n) => n.hash(hasher),
      ASTNode::TerminalMatches(n) => n.hash(hasher),
      ASTNode::AST_TrimmedReference(n) => n.hash(hasher),
      _=>{}
    }
  }
}*/

#[derive(Clone, Debug, Default)]
pub struct Pop<Token: Tk> {
  pub tok:   Token,
  pub count: u32,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Pop(self) -> Option<Box<Pop<Token>>> {
    match self {
      ASTNode::Pop(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Pop<Token>>> for ASTNode<Token> {
  fn from(value: Box<Pop<Token>>) -> Self {
    Self::Pop(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Rule<Token: Tk> {
  pub tok:     Token,
  pub ast:     Box<Ascript<Token>>,
  pub symbols: Vec<rule_group_2_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Rule(self) -> Option<Box<Rule<Token>>> {
    match self {
      ASTNode::Rule(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Rule<Token>>> for ASTNode<Token> {
  fn from(value: Box<Rule<Token>>) -> Self {
    Self::Rule(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Name {
  pub name: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Name(self) -> Option<Box<Name>> {
    match self {
      ASTNode::Name(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Name>> for ASTNode<Token> {
  fn from(value: Box<Name>) -> Self {
    Self::Name(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Push<Token: Tk> {
  pub tok:         Token,
  pub name:        String,
  pub nonterminal: nonterminal_Value<Token>, /* 10 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Push(self) -> Option<Box<Push<Token>>> {
    match self {
      ASTNode::Push(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Push<Token>>> for ASTNode<Token> {
  fn from(value: Box<Push<Token>>) -> Self {
    Self::Push(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Peek<Token: Tk> {
  pub tok:      Token,
  pub skip:     bool,
  pub ptr_type: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Peek(self) -> Option<Box<Peek<Token>>> {
    match self {
      ASTNode::Peek(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Peek<Token>>> for ASTNode<Token> {
  fn from(value: Box<Peek<Token>>) -> Self {
    Self::Peek(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Fork<Token: Tk> {
  pub tok:   Token,
  pub paths: Vec<Box<Goto<Token>>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Fork(self) -> Option<Box<Fork<Token>>> {
    match self {
      ASTNode::Fork(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Fork<Token>>> for ASTNode<Token> {
  fn from(value: Box<Fork<Token>>) -> Self {
    Self::Fork(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Fail<Token: Tk> {
  pub tok: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Fail(self) -> Option<Box<Fail<Token>>> {
    match self {
      ASTNode::Fail(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Fail<Token>>> for ASTNode<Token> {
  fn from(value: Box<Fail<Token>>) -> Self {
    Self::Fail(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Goto<Token: Tk> {
  pub tok:         Token,
  pub name:        String,
  pub nonterminal: nonterminal_Value<Token>, /* 10 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Goto(self) -> Option<Box<Goto<Token>>> {
    match self {
      ASTNode::Goto(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Goto<Token>>> for ASTNode<Token> {
  fn from(value: Box<Goto<Token>>) -> Self {
    Self::Goto(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Pass<Token: Tk> {
  pub tok: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Pass(self) -> Option<Box<Pass<Token>>> {
    match self {
      ASTNode::Pass(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Pass<Token>>> for ASTNode<Token> {
  fn from(value: Box<Pass<Token>>) -> Self {
    Self::Pass(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Init<Token: Tk> {
  pub expression: init_objects_Value<Token>, /* 19 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Init(self) -> Option<Box<Init<Token>>> {
    match self {
      ASTNode::Init(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Init<Token>>> for ASTNode<Token> {
  fn from(value: Box<Init<Token>>) -> Self {
    Self::Init(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Range {
  pub end_trim:   i32,
  pub start_trim: i32,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Range(self) -> Option<Box<Range>> {
    match self {
      ASTNode::Range(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Range>> for ASTNode<Token> {
  fn from(value: Box<Range>) -> Self {
    Self::Range(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct State<Token: Tk> {
  pub id:        Box<NonTerminal_Symbol<Token>>,
  pub tok:       Token,
  pub catches:   bool,
  pub statement: Box<Statement<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_State(self) -> Option<Box<State<Token>>> {
    match self {
      ASTNode::State(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<State<Token>>> for ASTNode<Token> {
  fn from(value: Box<State<Token>>) -> Self {
    Self::State(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Gotos<Token: Tk> {
  pub fork:   Box<Fork<Token>>,
  pub goto:   Box<Goto<Token>>,
  pub pushes: Vec<Box<Push<Token>>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Gotos(self) -> Option<Box<Gotos<Token>>> {
    match self {
      ASTNode::Gotos(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Gotos<Token>>> for ASTNode<Token> {
  fn from(value: Box<Gotos<Token>>) -> Self {
    Self::Gotos(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Reset<Token: Tk> {
  pub tok:      Token,
  pub ptr_type: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Reset(self) -> Option<Box<Reset<Token>>> {
    match self {
      ASTNode::Reset(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Reset<Token>>> for ASTNode<Token> {
  fn from(value: Box<Reset<Token>>) -> Self {
    Self::Reset(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Shift<Token: Tk> {
  pub tok:      Token,
  pub skip:     bool,
  pub ptr_type: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Shift(self) -> Option<Box<Shift<Token>>> {
    match self {
      ASTNode::Shift(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Shift<Token>>> for ASTNode<Token> {
  fn from(value: Box<Shift<Token>>) -> Self {
    Self::Shift(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_I8<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_I8(self) -> Option<Box<AST_I8<Token>>> {
    match self {
      ASTNode::AST_I8(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I8<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_I8<Token>>) -> Self {
    Self::AST_I8(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_U8<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_U8(self) -> Option<Box<AST_U8<Token>>> {
    match self {
      ASTNode::AST_U8(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_U8<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_U8<Token>>) -> Self {
    Self::AST_U8(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Reduce<Token: Tk> {
  pub tok:         Token,
  pub len:         u32,
  pub ast:         body_Value<Token>,        /* 9 */
  pub nonterminal: nonterminal_Value<Token>, /* 10 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Reduce(self) -> Option<Box<Reduce<Token>>> {
    match self {
      ASTNode::Reduce(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Reduce<Token>>> for ASTNode<Token> {
  fn from(value: Box<Reduce<Token>>) -> Self {
    Self::Reduce(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Ignore<Token: Tk> {
  pub symbols: Vec<ignore_clause_list_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Ignore(self) -> Option<Box<Ignore<Token>>> {
    match self {
      ASTNode::Ignore(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Ignore<Token>>> for ASTNode<Token> {
  fn from(value: Box<Ignore<Token>>) -> Self {
    Self::Ignore(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Accept<Token: Tk> {
  pub tok: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Accept(self) -> Option<Box<Accept<Token>>> {
    match self {
      ASTNode::Accept(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Accept<Token>>> for ASTNode<Token> {
  fn from(value: Box<Accept<Token>>) -> Self {
    Self::Accept(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Import<Token: Tk> {
  pub uri:       String,
  pub tok:       Token,
  pub reference: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Import(self) -> Option<Box<Import<Token>>> {
    match self {
      ASTNode::Import(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Import<Token>>> for ASTNode<Token> {
  fn from(value: Box<Import<Token>>) -> Self {
    Self::Import(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Export<Token: Tk> {
  pub nonterminal: nonterminal_Value<Token>, /* 10 */
  pub reference:   String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Export(self) -> Option<Box<Export<Token>>> {
    match self {
      ASTNode::Export(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Export<Token>>> for ASTNode<Token> {
  fn from(value: Box<Export<Token>>) -> Self {
    Self::Export(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_F32<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_F32(self) -> Option<Box<AST_F32<Token>>> {
    match self {
      ASTNode::AST_F32(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_F32<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_F32<Token>>) -> Self {
    Self::AST_F32(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_I32<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_I32(self) -> Option<Box<AST_I32<Token>>> {
    match self {
      ASTNode::AST_I32(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I32<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_I32<Token>>) -> Self {
    Self::AST_I32(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_U32<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_U32(self) -> Option<Box<AST_U32<Token>>> {
    match self {
      ASTNode::AST_U32(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_U32<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_U32<Token>>) -> Self {
    Self::AST_U32(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_F64<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_F64(self) -> Option<Box<AST_F64<Token>>> {
    match self {
      ASTNode::AST_F64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_F64<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_F64<Token>>) -> Self {
    Self::AST_F64(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_I64<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_I64(self) -> Option<Box<AST_I64<Token>>> {
    match self {
      ASTNode::AST_I64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I64<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_I64<Token>>) -> Self {
    Self::AST_I64(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_U64<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_U64(self) -> Option<Box<AST_U64<Token>>> {
    match self {
      ASTNode::AST_U64(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_U64<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_U64<Token>>) -> Self {
    Self::AST_U64(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_F16<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_F16(self) -> Option<Box<AST_F16<Token>>> {
    match self {
      ASTNode::AST_F16(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_F16<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_F16<Token>>) -> Self {
    Self::AST_F16(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_I16<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_I16(self) -> Option<Box<AST_I16<Token>>> {
    match self {
      ASTNode::AST_I16(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_I16<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_I16<Token>>) -> Self {
    Self::AST_I16(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_U16<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_U16(self) -> Option<Box<AST_U16<Token>>> {
    match self {
      ASTNode::AST_U16(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_U16<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_U16<Token>>) -> Self {
    Self::AST_U16(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Sub<Token: Tk> {
  pub tok:   Token,
  pub left:  expr_Value<Token>, /* 0 */
  pub right: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Sub(self) -> Option<Box<AST_Sub<Token>>> {
    match self {
      ASTNode::AST_Sub(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Sub<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Sub<Token>>) -> Self {
    Self::AST_Sub(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Add<Token: Tk> {
  pub tok:   Token,
  pub left:  expr_Value<Token>, /* 0 */
  pub right: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Add(self) -> Option<Box<AST_Add<Token>>> {
    match self {
      ASTNode::AST_Add(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Add<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Add<Token>>) -> Self {
    Self::AST_Add(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Mod<Token: Tk> {
  pub tok:   Token,
  pub left:  expr_Value<Token>, /* 0 */
  pub right: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Mod(self) -> Option<Box<AST_Mod<Token>>> {
    match self {
      ASTNode::AST_Mod(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Mod<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Mod<Token>>) -> Self {
    Self::AST_Mod(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct SetLine<Token: Tk> {
  pub tok: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_SetLine(self) -> Option<Box<SetLine<Token>>> {
    match self {
      ASTNode::SetLine(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<SetLine<Token>>> for ASTNode<Token> {
  fn from(value: Box<SetLine<Token>>) -> Self {
    Self::SetLine(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Neg<Token: Tk> {
  pub tok:  Token,
  pub expr: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Neg(self) -> Option<Box<AST_Neg<Token>>> {
    match self {
      ASTNode::AST_Neg(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Neg<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Neg<Token>>) -> Self {
    Self::AST_Neg(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Mul<Token: Tk> {
  pub tok:   Token,
  pub left:  expr_Value<Token>, /* 0 */
  pub right: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Mul(self) -> Option<Box<AST_Mul<Token>>> {
    match self {
      ASTNode::AST_Mul(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Mul<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Mul<Token>>) -> Self {
    Self::AST_Mul(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Map<Token: Tk> {
  pub tok: Token,
  pub val: expr_Value<Token>, /* 0 */
  pub key: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Map(self) -> Option<Box<AST_Map<Token>>> {
    match self {
      ASTNode::AST_Map(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Map<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Map<Token>>) -> Self {
    Self::AST_Map(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Grammar<Token: Tk> {
  pub tok:      Token,
  pub rules:    Vec<grammar_group_1_Value<Token>>,
  pub preamble: Vec<preamble_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Grammar(self) -> Option<Box<Grammar<Token>>> {
    match self {
      ASTNode::Grammar(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Grammar<Token>>> for ASTNode<Token> {
  fn from(value: Box<Grammar<Token>>) -> Self {
    Self::Grammar(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Matches<Token: Tk> {
  pub tok:     Token,
  pub mode:    String,
  pub scanner: String,
  pub matches: Vec<generic_match_block_group_1_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Matches(self) -> Option<Box<Matches<Token>>> {
    match self {
      ASTNode::Matches(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Matches<Token>>> for ASTNode<Token> {
  fn from(value: Box<Matches<Token>>) -> Self {
    Self::Matches(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct CFRules<Token: Tk> {
  pub tok:      Token,
  pub rules:    Vec<Box<Rule<Token>>>,
  pub name_sym: Box<NonTerminal_Symbol<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_CFRules(self) -> Option<Box<CFRules<Token>>> {
    match self {
      ASTNode::CFRules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<CFRules<Token>>> for ASTNode<Token> {
  fn from(value: Box<CFRules<Token>>) -> Self {
    Self::CFRules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Ascript<Token: Tk> {
  pub tok: Token,
  pub ast: body_Value<Token>, /* 9 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Ascript(self) -> Option<Box<Ascript<Token>>> {
    match self {
      ASTNode::Ascript(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Ascript<Token>>> for ASTNode<Token> {
  fn from(value: Box<Ascript<Token>>) -> Self {
    Self::Ascript(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Div<Token: Tk> {
  pub tok:   Token,
  pub left:  expr_Value<Token>, /* 0 */
  pub right: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Div(self) -> Option<Box<AST_Div<Token>>> {
    match self {
      ASTNode::AST_Div(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Div<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Div<Token>>) -> Self {
    Self::AST_Div(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Pow<Token: Tk> {
  pub tok:   Token,
  pub left:  expr_Value<Token>, /* 0 */
  pub right: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Pow(self) -> Option<Box<AST_Pow<Token>>> {
    match self {
      ASTNode::AST_Pow(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Pow<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Pow<Token>>) -> Self {
    Self::AST_Pow(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_F128<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_F128(self) -> Option<Box<AST_F128<Token>>> {
    match self {
      ASTNode::AST_F128(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_F128<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_F128<Token>>) -> Self {
    Self::AST_F128(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_U128<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_U128(self) -> Option<Box<AST_U128<Token>>> {
    match self {
      ASTNode::AST_U128(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_U128<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_U128<Token>>) -> Self {
    Self::AST_U128(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Flag<Token: Tk> {
  pub ty:  String,
  pub tok: Token,
  pub val: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Flag(self) -> Option<Box<AST_Flag<Token>>> {
    match self {
      ASTNode::AST_Flag(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Flag<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Flag<Token>>) -> Self {
    Self::AST_Flag(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct IntMatch<Token: Tk> {
  pub vals:      Vec<u64>,
  pub statement: Box<Statement<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_IntMatch(self) -> Option<Box<IntMatch<Token>>> {
    match self {
      ASTNode::IntMatch(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<IntMatch<Token>>> for ASTNode<Token> {
  fn from(value: Box<IntMatch<Token>>) -> Self {
    Self::IntMatch(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Bool<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Bool(self) -> Option<Box<AST_Bool<Token>>> {
    match self {
      ASTNode::AST_Bool(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Bool<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Bool<Token>>) -> Self {
    Self::AST_Bool(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct PegRules<Token: Tk> {
  pub tok:      Token,
  pub rules:    Vec<Box<Rule<Token>>>,
  pub name_sym: Box<NonTerminal_Symbol<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_PegRules(self) -> Option<Box<PegRules<Token>>> {
    match self {
      ASTNode::PegRules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<PegRules<Token>>> for ASTNode<Token> {
  fn from(value: Box<PegRules<Token>>) -> Self {
    Self::PegRules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct FailHint {
  pub message: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_FailHint(self) -> Option<Box<FailHint>> {
    match self {
      ASTNode::FailHint(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<FailHint>> for ASTNode<Token> {
  fn from(value: Box<FailHint>) -> Self {
    Self::FailHint(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct DEFINED_TYPE_NUM {}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_DEFINED_TYPE_NUM(self) -> Option<DEFINED_TYPE_NUM> {
    match self {
      ASTNode::DEFINED_TYPE_NUM(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<DEFINED_TYPE_NUM> for ASTNode<Token> {
  fn from(value: DEFINED_TYPE_NUM) -> Self {
    Self::DEFINED_TYPE_NUM(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Property<Token: Tk> {
  pub id:              String,
  pub tok:             Token,
  pub value:           struct_list_Value<Token>, /* 14 */
  pub named_reference: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Property(self) -> Option<Box<AST_Property<Token>>> {
    match self {
      ASTNode::AST_Property(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Property<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Property<Token>>) -> Self {
    Self::AST_Property(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct NonTerminal_Terminal_Symbol<Token: Tk> {
  pub tok:         Token,
  pub nonterminal: nonterminal_Value<Token>, /* 10 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_NonTerminal_Terminal_Symbol(self) -> Option<Box<NonTerminal_Terminal_Symbol<Token>>> {
    match self {
      ASTNode::NonTerminal_Terminal_Symbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Terminal_Symbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<NonTerminal_Terminal_Symbol<Token>>) -> Self {
    Self::NonTerminal_Terminal_Symbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Member<Token: Tk> {
  pub property:  Token,
  pub reference: reference_Value<Token>, /* 7 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Member(self) -> Option<Box<AST_Member<Token>>> {
    match self {
      ASTNode::AST_Member(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Member<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Member<Token>>) -> Self {
    Self::AST_Member(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_NamedReference<Token: Tk> {
  pub tok:   Token,
  pub value: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_NamedReference(self) -> Option<Box<AST_NamedReference<Token>>> {
    match self {
      ASTNode::AST_NamedReference(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_NamedReference<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_NamedReference<Token>>) -> Self {
    Self::AST_NamedReference(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct NonTerminal_Symbol<Token: Tk> {
  pub tok:  Token,
  pub name: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_NonTerminal_Symbol(self) -> Option<Box<NonTerminal_Symbol<Token>>> {
    match self {
      ASTNode::NonTerminal_Symbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Symbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<NonTerminal_Symbol<Token>>) -> Self {
    Self::NonTerminal_Symbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct ReduceRaw<Token: Tk> {
  pub tok:            Token,
  pub len:            u32,
  pub rule_id:        u32,
  pub nonterminal_id: u32,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_ReduceRaw(self) -> Option<Box<ReduceRaw<Token>>> {
    match self {
      ASTNode::ReduceRaw(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<ReduceRaw<Token>>> for ASTNode<Token> {
  fn from(value: Box<ReduceRaw<Token>>) -> Self {
    Self::ReduceRaw(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TokenGroupRules<Token: Tk> {
  pub tok:   Token,
  pub rules: Vec<Box<Rule<Token>>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TokenGroupRules(self) -> Option<Box<TokenGroupRules<Token>>> {
    match self {
      ASTNode::TokenGroupRules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TokenGroupRules<Token>>> for ASTNode<Token> {
  fn from(value: Box<TokenGroupRules<Token>>) -> Self {
    Self::TokenGroupRules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct DefaultMatch<Token: Tk> {
  pub statement: Box<Statement<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_DefaultMatch(self) -> Option<Box<DefaultMatch<Token>>> {
    match self {
      ASTNode::DefaultMatch(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<DefaultMatch<Token>>> for ASTNode<Token> {
  fn from(value: Box<DefaultMatch<Token>>) -> Self {
    Self::DefaultMatch(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct NotEmptySet<Token: Tk> {
  pub tok:         Token,
  pub symbols:     Vec<annotated_symbol_Value<Token>>,
  pub allow_empty: bool,
  pub unordered:   bool,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_NotEmptySet(self) -> Option<Box<NotEmptySet<Token>>> {
    match self {
      ASTNode::NotEmptySet(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<NotEmptySet<Token>>> for ASTNode<Token> {
  fn from(value: Box<NotEmptySet<Token>>) -> Self {
    Self::NotEmptySet(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Statement<Token: Tk> {
  pub pop:        Box<Pop<Token>>,
  pub branch:     branch_statement_Value<Token>, /* 15 */
  pub non_branch: Vec<non_branch_statement_Value<Token>>,
  pub transitive: transitive_statement_Value<Token>, /* 2 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Statement(self) -> Option<Box<Statement<Token>>> {
    match self {
      ASTNode::Statement(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Statement<Token>>> for ASTNode<Token> {
  fn from(value: Box<Statement<Token>>) -> Self {
    Self::Statement(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TemplateSym {
  pub val: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TemplateSym(self) -> Option<Box<TemplateSym>> {
    match self {
      ASTNode::TemplateSym(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TemplateSym>> for ASTNode<Token> {
  fn from(value: Box<TemplateSym>) -> Self {
    Self::TemplateSym(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Token {
  pub range: Box<Range>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Token(self) -> Option<Box<AST_Token>> {
    match self {
      ASTNode::AST_Token(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Token>> for ASTNode<Token> {
  fn from(value: Box<AST_Token>) -> Self {
    Self::AST_Token(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_StringLiteral<Token: Tk> {
  pub tok:   Token,
  pub value: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_StringLiteral(self) -> Option<Box<AST_StringLiteral<Token>>> {
    match self {
      ASTNode::AST_StringLiteral(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_StringLiteral<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_StringLiteral<Token>>) -> Self {
    Self::AST_StringLiteral(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TemplateRules<Token: Tk> {
  pub tok:             Token,
  pub rules:           Vec<Box<Rule<Token>>>,
  pub name_sym:        Box<NonTerminal_Symbol<Token>>,
  pub template_params: Vec<template_param_Value>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TemplateRules(self) -> Option<Box<TemplateRules<Token>>> {
    match self {
      ASTNode::TemplateRules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TemplateRules<Token>>> for ASTNode<Token> {
  fn from(value: Box<TemplateRules<Token>>) -> Self {
    Self::TemplateRules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Grouped_Rules<Token: Tk> {
  pub tok:   Token,
  pub rules: Vec<Box<Rule<Token>>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Grouped_Rules(self) -> Option<Box<Grouped_Rules<Token>>> {
    match self {
      ASTNode::Grouped_Rules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Grouped_Rules<Token>>> for ASTNode<Token> {
  fn from(value: Box<Grouped_Rules<Token>>) -> Self {
    Self::Grouped_Rules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_BoolLiteral {
  pub value: bool,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_BoolLiteral(self) -> Option<Box<AST_BoolLiteral>> {
    match self {
      ASTNode::AST_BoolLiteral(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_BoolLiteral>> for ASTNode<Token> {
  fn from(value: Box<AST_BoolLiteral>) -> Self {
    Self::AST_BoolLiteral(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Precedence {
  pub sym_prec:   u32,
  pub kot_prec:   u32,
  pub is_keyword: bool,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Precedence(self) -> Option<Box<Precedence>> {
    match self {
      ASTNode::Precedence(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Precedence>> for ASTNode<Token> {
  fn from(value: Box<Precedence>) -> Self {
    Self::Precedence(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct NonTermMatch<Token: Tk> {
  pub sym:       nonterminal_Value<Token>, /* 10 */
  pub statement: Box<Statement<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_NonTermMatch(self) -> Option<Box<NonTermMatch<Token>>> {
    match self {
      ASTNode::NonTermMatch(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<NonTermMatch<Token>>> for ASTNode<Token> {
  fn from(value: Box<NonTermMatch<Token>>) -> Self {
    Self::NonTermMatch(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct ClassSymbol<Token: Tk> {
  pub tok: Token,
  pub val: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_ClassSymbol(self) -> Option<Box<ClassSymbol<Token>>> {
    match self {
      ASTNode::ClassSymbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<ClassSymbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<ClassSymbol<Token>>) -> Self {
    Self::ClassSymbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Vector<Token: Tk> {
  pub tok:         Token,
  pub initializer: Vec<expr_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Vector(self) -> Option<Box<AST_Vector<Token>>> {
    match self {
      ASTNode::AST_Vector(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Vector<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Vector<Token>>) -> Self {
    Self::AST_Vector(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct NonTerminal_Import_Symbol<Token: Tk> {
  pub tok:    Token,
  pub name:   String,
  pub module: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_NonTerminal_Import_Symbol(self) -> Option<Box<NonTerminal_Import_Symbol<Token>>> {
    match self {
      ASTNode::NonTerminal_Import_Symbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<NonTerminal_Import_Symbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<NonTerminal_Import_Symbol<Token>>) -> Self {
    Self::NonTerminal_Import_Symbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AppendRules<Token: Tk> {
  pub tok:      Token,
  pub rules:    Vec<Box<Rule<Token>>>,
  pub name_sym: nonterminal_Value<Token>, /* 10 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AppendRules(self) -> Option<Box<AppendRules<Token>>> {
    match self {
      ASTNode::AppendRules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AppendRules<Token>>> for ASTNode<Token> {
  fn from(value: Box<AppendRules<Token>>) -> Self {
    Self::AppendRules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TemplateASTType {
  pub val: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TemplateASTType(self) -> Option<Box<TemplateASTType>> {
    match self {
      ASTNode::TemplateASTType(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TemplateASTType>> for ASTNode<Token> {
  fn from(value: Box<TemplateASTType>) -> Self {
    Self::TemplateASTType(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct SetTokenId<Token: Tk> {
  pub id:  u32,
  pub tok: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_SetTokenId(self) -> Option<Box<SetTokenId<Token>>> {
    match self {
      ASTNode::SetTokenId(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<SetTokenId<Token>>> for ASTNode<Token> {
  fn from(value: Box<SetTokenId<Token>>) -> Self {
    Self::SetTokenId(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_String<Token: Tk> {
  pub tok:         Token,
  pub initializer: Box<Init<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_String(self) -> Option<Box<AST_String<Token>>> {
    match self {
      ASTNode::AST_String(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_String<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_String<Token>>) -> Self {
    Self::AST_String(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AnnotatedSymbol<Token: Tk> {
  pub tok:         Token,
  pub symbol:      list_Value<Token>, /* 43 */
  pub precedence:  Box<Precedence>,
  pub is_optional: bool,
  pub reference:   String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AnnotatedSymbol(self) -> Option<Box<AnnotatedSymbol<Token>>> {
    match self {
      ASTNode::AnnotatedSymbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AnnotatedSymbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<AnnotatedSymbol<Token>>) -> Self {
    Self::AnnotatedSymbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_NumberLiteral {
  pub value: f64,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_NumberLiteral(self) -> Option<Box<AST_NumberLiteral>> {
    match self {
      ASTNode::AST_NumberLiteral(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_NumberLiteral>> for ASTNode<Token> {
  fn from(value: Box<AST_NumberLiteral>) -> Self {
    Self::AST_NumberLiteral(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct SetTokenLen {
  pub id: u32,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_SetTokenLen(self) -> Option<Box<SetTokenLen>> {
    match self {
      ASTNode::SetTokenLen(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<SetTokenLen>> for ASTNode<Token> {
  fn from(value: Box<SetTokenLen>) -> Self {
    Self::SetTokenLen(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TerminalToken<Token: Tk> {
  pub tok:          Token,
  pub val:          String,
  pub is_exclusive: bool,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TerminalToken(self) -> Option<Box<TerminalToken<Token>>> {
    match self {
      ASTNode::TerminalToken(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TerminalToken<Token>>> for ASTNode<Token> {
  fn from(value: Box<TerminalToken<Token>>) -> Self {
    Self::TerminalToken(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_STRUCT_TEMPLATE_NAME {
  pub typ: String,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_STRUCT_TEMPLATE_NAME(self) -> Option<Box<AST_STRUCT_TEMPLATE_NAME>> {
    match self {
      ASTNode::AST_STRUCT_TEMPLATE_NAME(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_STRUCT_TEMPLATE_NAME>> for ASTNode<Token> {
  fn from(value: Box<AST_STRUCT_TEMPLATE_NAME>) -> Self {
    Self::AST_STRUCT_TEMPLATE_NAME(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct EOFSymbol<Token: Tk> {
  pub tok: Token,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_EOFSymbol(self) -> Option<Box<EOFSymbol<Token>>> {
    match self {
      ASTNode::EOFSymbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<EOFSymbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<EOFSymbol<Token>>) -> Self {
    Self::EOFSymbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Statement<Token: Tk> {
  pub tok:        Token,
  pub expression: expr_Value<Token>, /* 0 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Statement(self) -> Option<Box<AST_Statement<Token>>> {
    match self {
      ASTNode::AST_Statement(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Statement<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Statement<Token>>) -> Self {
    Self::AST_Statement(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_Struct<Token: Tk> {
  pub ty:    String,
  pub tok:   Token,
  pub props: Vec<struct_list_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_Struct(self) -> Option<Box<AST_Struct<Token>>> {
    match self {
      ASTNode::AST_Struct(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_Struct<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_Struct<Token>>) -> Self {
    Self::AST_Struct(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct DEFINED_TYPE_IDENT {}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_DEFINED_TYPE_IDENT(self) -> Option<DEFINED_TYPE_IDENT> {
    match self {
      ASTNode::DEFINED_TYPE_IDENT(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<DEFINED_TYPE_IDENT> for ASTNode<Token> {
  fn from(value: DEFINED_TYPE_IDENT) -> Self {
    Self::DEFINED_TYPE_IDENT(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct List_Rules<Token: Tk> {
  pub tok:             Token,
  pub symbol:          symbol_Value<Token>, /* 16 */
  pub optional:        bool,
  pub terminal_symbol: list_Value<Token>, /* 43 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_List_Rules(self) -> Option<Box<List_Rules<Token>>> {
    match self {
      ASTNode::List_Rules(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<List_Rules<Token>>> for ASTNode<Token> {
  fn from(value: Box<List_Rules<Token>>) -> Self {
    Self::List_Rules(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct ProductionMatches<Token: Tk> {
  pub matches: Vec<nonterminal_match_block_group_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_ProductionMatches(self) -> Option<Box<ProductionMatches<Token>>> {
    match self {
      ASTNode::ProductionMatches(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<ProductionMatches<Token>>> for ASTNode<Token> {
  fn from(value: Box<ProductionMatches<Token>>) -> Self {
    Self::ProductionMatches(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct Template_NonTerminal_Symbol<Token: Tk> {
  pub tok:           Token,
  pub name:          nonterminal_Value<Token>, /* 10 */
  pub template_args: Vec<template_arg_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_Template_NonTerminal_Symbol(self) -> Option<Box<Template_NonTerminal_Symbol<Token>>> {
    match self {
      ASTNode::Template_NonTerminal_Symbol(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<Template_NonTerminal_Symbol<Token>>> for ASTNode<Token> {
  fn from(value: Box<Template_NonTerminal_Symbol<Token>>) -> Self {
    Self::Template_NonTerminal_Symbol(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_IndexReference<Token: Tk> {
  pub tok:   Token,
  pub value: i64,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_IndexReference(self) -> Option<Box<AST_IndexReference<Token>>> {
    match self {
      ASTNode::AST_IndexReference(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_IndexReference<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_IndexReference<Token>>) -> Self {
    Self::AST_IndexReference(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TermMatch<Token: Tk> {
  pub sym:       ignore_clause_list_Value<Token>, /* 28 */
  pub statement: Box<Statement<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TermMatch(self) -> Option<Box<TermMatch<Token>>> {
    match self {
      ASTNode::TermMatch(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TermMatch<Token>>> for ASTNode<Token> {
  fn from(value: Box<TermMatch<Token>>) -> Self {
    Self::TermMatch(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct TerminalMatches<Token: Tk> {
  pub matches: Vec<terminal_match_block_group_Value<Token>>,
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_TerminalMatches(self) -> Option<Box<TerminalMatches<Token>>> {
    match self {
      ASTNode::TerminalMatches(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<TerminalMatches<Token>>> for ASTNode<Token> {
  fn from(value: Box<TerminalMatches<Token>>) -> Self {
    Self::TerminalMatches(value)
  }
}

#[derive(Clone, Debug, Default)]
pub struct AST_TrimmedReference<Token: Tk> {
  pub tok:       Token,
  pub range:     Box<Range>,
  pub reference: reference_Value<Token>, /* 7 */
}

impl<Token: Tk> ASTNode<Token> {
  pub fn into_AST_TrimmedReference(self) -> Option<Box<AST_TrimmedReference<Token>>> {
    match self {
      ASTNode::AST_TrimmedReference(val) => Some(val),
      _ => None,
    }
  }
}

impl<Token: Tk> From<Box<AST_TrimmedReference<Token>>> for ASTNode<Token> {
  fn from(value: Box<AST_TrimmedReference<Token>>) -> Self {
    Self::AST_TrimmedReference(value)
  }
}

fn rule_0<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = std::mem::take(&mut nodes[0]);
  let id = unsafe { id.into_NonTerminal_Symbol().unwrap_unchecked() };

  let tok = nterm_tok.clone();

  let catches = Default::default();

  let statement = std::mem::take(&mut nodes[2]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::State(Box::new(State { id, tok, catches, statement }))
}

fn rule_1<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = std::mem::take(&mut nodes[0]);
  let id = unsafe { id.into_NonTerminal_Symbol().unwrap_unchecked() };

  let tok = nterm_tok.clone();

  let catches = true;

  let statement = std::mem::take(&mut nodes[2]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::State(Box::new(State { id, tok, catches, statement }))
}

fn rule_2<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_vec_String().unwrap_unchecked() };
  out.into()
}

fn rule_3<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[0].clone();
  let out = out.to_token().unwrap();
  let out = out.to_string();
  out.into()
}

fn rule_4<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_String().unwrap_unchecked() };
  out.into()
}

fn rule_5<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_String().unwrap_unchecked() };

  let out = vec![out_0];
  out.into()
}

fn rule_6<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_String().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_String().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_7<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[1]);
  let rules = unsafe {
    rules.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };

  let preamble = std::mem::take(&mut nodes[0]);
  let preamble = unsafe {
    preamble.into_vec_preamble_Value/*13*/().unwrap_unchecked()
  };

  ASTNode::Grammar(Box::new(Grammar { tok, rules, preamble }))
}

fn rule_8<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[0]);
  let rules = unsafe {
    rules.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };

  let preamble = Default::default();

  ASTNode::Grammar(Box::new(Grammar { tok, rules, preamble }))
}

fn rule_9<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_preamble_Value/*13*/().unwrap_unchecked()
  };

  let out = vec![out_0];
  out.into()
}

fn rule_10<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe {
    out_r.into_preamble_Value/*13*/().unwrap_unchecked()
  };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_preamble_Value/*13*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_11<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_CFRules().unwrap_unchecked() };
  out.into()
}

fn rule_12<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_PegRules().unwrap_unchecked() };
  out.into()
}

fn rule_13<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AppendRules().unwrap_unchecked() };
  out.into()
}

fn rule_14<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_State().unwrap_unchecked() };
  out.into()
}

fn rule_15<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_TemplateRules().unwrap_unchecked() };
  out.into()
}

fn rule_16<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_CFRules().unwrap_unchecked() };
  let out_0: grammar_group_1_Value<Token>/*8*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_17<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_PegRules().unwrap_unchecked() };
  let out_0: grammar_group_1_Value<Token>/*8*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_18<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_AppendRules().unwrap_unchecked() };
  let out_0: grammar_group_1_Value<Token>/*8*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_19<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_State().unwrap_unchecked() };
  let out_0: grammar_group_1_Value<Token>/*8*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_20<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_TemplateRules().unwrap_unchecked() };
  let out_0: grammar_group_1_Value<Token>/*8*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_21<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_CFRules().unwrap_unchecked() };
  let out_r: grammar_group_1_Value<Token>/*8*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_22<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_PegRules().unwrap_unchecked() };
  let out_r: grammar_group_1_Value<Token>/*8*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_23<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_AppendRules().unwrap_unchecked() };
  let out_r: grammar_group_1_Value<Token>/*8*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_24<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_State().unwrap_unchecked() };
  let out_r: grammar_group_1_Value<Token>/*8*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_25<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 8
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_TemplateRules().unwrap_unchecked() };
  let out_r: grammar_group_1_Value<Token>/*8*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_grammar_group_1_Value/*8*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_26<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 12
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_DEFINED_TYPE_IDENT().unwrap_unchecked() };
  let out: def_type_Value/*12*/= out.into();

  out.into()
}

fn rule_27<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 12
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_DEFINED_TYPE_NUM().unwrap_unchecked() };
  let out: def_type_Value/*12*/= out.into();

  out.into()
}

fn rule_28<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_expr_Value/*0*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_29<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let ty = nodes[1].clone();
  let ty = ty.to_token().unwrap();
  let ty = ty.to_string();

  let tok = nterm_tok.clone();

  let props = std::mem::take(&mut nodes[3]);
  let props = unsafe {
    props.into_vec_struct_list_Value/*40*/().unwrap_unchecked()
  };

  ASTNode::AST_Struct(Box::new(AST_Struct { ty, tok, props }))
}

fn rule_30<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let ty = nodes[1].clone();
  let ty = ty.to_token().unwrap();
  let ty = ty.to_string();

  let tok = nterm_tok.clone();

  let props = Default::default();

  ASTNode::AST_Struct(Box::new(AST_Struct { ty, tok, props }))
}

fn rule_31<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 40
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Multi
  from index: 14
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_struct_list_Value/*14*/().unwrap_unchecked()
  };
  let out_0: struct_list_Value<Token>/*40*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_32<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 40
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 14
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe {
    out_r.into_struct_list_Value/*14*/().unwrap_unchecked()
  };
  let out_r: struct_list_Value<Token>/*40*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_struct_list_Value/*40*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_33<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_vec_struct_list_Value/*40*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_34<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let name = nodes[0].clone();
  let name = name.to_token().unwrap();
  let name = name.to_string();

  ASTNode::NonTerminal_Symbol(Box::new(NonTerminal_Symbol { tok, name }))
}

fn rule_35<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[4]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = std::mem::take(&mut nodes[6]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_36<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = std::mem::take(&mut nodes[4]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = Default::default();

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_37<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = std::mem::take(&mut nodes[4]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_38<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = std::mem::take(&mut nodes[2]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = Default::default();

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_39<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[4]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = Default::default();

  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_40<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = Default::default();

  let non_branch = Default::default();

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_41<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = Default::default();

  let non_branch = std::mem::take(&mut nodes[2]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_42<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = Default::default();

  let non_branch = Default::default();

  let transitive = std::mem::take(&mut nodes[0]);
  let transitive = unsafe {
    transitive.into_transitive_statement_Value/*2*/().unwrap_unchecked()
  };

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_43<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = std::mem::take(&mut nodes[4]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_44<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = std::mem::take(&mut nodes[2]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_45<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[2]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = Default::default();

  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_46<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = Default::default();

  let non_branch = std::mem::take(&mut nodes[0]);
  let non_branch = unsafe {
    non_branch.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_47<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[0]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = std::mem::take(&mut nodes[2]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = Default::default();

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_48<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = Default::default();

  let branch = std::mem::take(&mut nodes[0]);
  let branch = unsafe {
    branch.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };

  let non_branch = Default::default();

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_49<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let pop = std::mem::take(&mut nodes[0]);
  let pop = unsafe { pop.into_Pop().unwrap_unchecked() };

  let branch = Default::default();

  let non_branch = Default::default();

  let transitive = Default::default();

  ASTNode::Statement(Box::new(Statement { pop, branch, non_branch, transitive }))
}

fn rule_50<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let out = vec![out_0];
  out.into()
}

fn rule_51<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe {
    out_r.into_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_52<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_53<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe { out.into_Pop().unwrap_unchecked() };
  out.into()
}

fn rule_54<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_55<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };

  let out = vec![out_0];
  out.into()
}

fn rule_56<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe {
    out_r.into_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_non_branch_statement_Value/*1*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_57<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe { out.into_Pop().unwrap_unchecked() };
  out.into()
}

fn rule_58<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_branch_statement_Value/*15*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_59<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  ASTNode::Token(out)
}

fn rule_60<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_61<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_62<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_63<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_64<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_65<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  let out = out.to_string();
  out.into()
}

fn rule_66<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  let out = out.to_string();
  out.into()
}

fn rule_67<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  let out = out.to_string();
  out.into()
}

fn rule_68<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  let out = out.to_string();
  out.into()
}

fn rule_69<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  let out = out.to_string();
  out.into()
}

fn rule_70<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_71<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_72<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_73<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_74<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_75<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Export().unwrap_unchecked() };
  out.into()
}

fn rule_76<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Import().unwrap_unchecked() };
  out.into()
}

fn rule_77<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Name().unwrap_unchecked() };
  out.into()
}

fn rule_78<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Ignore().unwrap_unchecked() };
  out.into()
}

fn rule_79<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[3]);
  let rules = unsafe { rules.into_vec_Rule().unwrap_unchecked() };

  let name_sym = std::mem::take(&mut nodes[1]);
  let name_sym = unsafe { name_sym.into_NonTerminal_Symbol().unwrap_unchecked() };

  ASTNode::CFRules(Box::new(CFRules { tok, rules, name_sym }))
}

fn rule_80<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[3]);
  let rules = unsafe { rules.into_vec_Rule().unwrap_unchecked() };

  let name_sym = std::mem::take(&mut nodes[1]);
  let name_sym = unsafe { name_sym.into_NonTerminal_Symbol().unwrap_unchecked() };

  ASTNode::PegRules(Box::new(PegRules { tok, rules, name_sym }))
}

fn rule_81<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[4]);
  let rules = unsafe { rules.into_vec_Rule().unwrap_unchecked() };

  let name_sym = std::mem::take(&mut nodes[2]);
  let name_sym = unsafe {
    name_sym.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::AppendRules(Box::new(AppendRules { tok, rules, name_sym }))
}

fn rule_82<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[5]);
  let rules = unsafe { rules.into_vec_Rule().unwrap_unchecked() };

  let name_sym = std::mem::take(&mut nodes[3]);
  let name_sym = unsafe { name_sym.into_NonTerminal_Symbol().unwrap_unchecked() };

  let template_params = std::mem::take(&mut nodes[1]);
  let template_params = unsafe {
    template_params.into_vec_template_param_Value/*3*/().unwrap_unchecked()
  };

  ASTNode::TemplateRules(Box::new(TemplateRules { tok, rules, name_sym, template_params }))
}

fn rule_83<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_template_param_Value/*3*/().unwrap_unchecked()
  };

  let out = vec![out_0];
  out.into()
}

fn rule_84<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe {
    out_r.into_template_param_Value/*3*/().unwrap_unchecked()
  };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_template_param_Value/*3*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_85<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  ASTNode::DEFINED_TYPE_IDENT(DEFINED_TYPE_IDENT {})
}

fn rule_86<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  ASTNode::DEFINED_TYPE_NUM(DEFINED_TYPE_NUM {})
}

fn rule_87<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe {
    left.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe {
    right.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Add(Box::new(AST_Add { tok, left, right }))
}

fn rule_88<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe {
    left.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe {
    right.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Sub(Box::new(AST_Sub { tok, left, right }))
}

fn rule_89<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe {
    left.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe {
    right.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Div(Box::new(AST_Div { tok, left, right }))
}

fn rule_90<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe {
    left.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe {
    right.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Mul(Box::new(AST_Mul { tok, left, right }))
}

fn rule_91<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe {
    left.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe {
    right.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Mod(Box::new(AST_Mod { tok, left, right }))
}

fn rule_92<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let left = std::mem::take(&mut nodes[0]);
  let left = unsafe {
    left.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let right = std::mem::take(&mut nodes[2]);
  let right = unsafe {
    right.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Pow(Box::new(AST_Pow { tok, left, right }))
}

fn rule_93<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let expr = std::mem::take(&mut nodes[1]);
  let expr = unsafe {
    expr.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Neg(Box::new(AST_Neg { tok, expr }))
}

fn rule_94<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_expr_Value/*0*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_95<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_term_Value/*20*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_96<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = nodes[0].clone();
  let id = id.to_token().unwrap();
  let id = id.to_string();

  let tok = nterm_tok.clone();

  /*to index id: 14
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Multi
  from index: 0
  from val: SymNode
  from agg:
  */
  let value = std::mem::take(&mut nodes[2]);
  let value = unsafe {
    value.into_expr_Value/*0*/().unwrap_unchecked()
  };
  let value: struct_list_Value<Token>/*14*/= value.into();

  let named_reference = Default::default();

  ASTNode::AST_Property(Box::new(AST_Property { id, tok, value, named_reference }))
}

fn rule_97<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = nodes[0].clone();
  let id = id.to_token().unwrap();
  let id = id.to_string();

  let tok = nterm_tok.clone();

  /*to index id: 14
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let value = std::mem::take(&mut nodes[2]);
  let value = unsafe { value.into_AST_Struct().unwrap_unchecked() };
  let value: struct_list_Value<Token>/*14*/= value.into();

  let named_reference = Default::default();

  ASTNode::AST_Property(Box::new(AST_Property { id, tok, value, named_reference }))
}

fn rule_98<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = nodes[0].clone();
  let id = id.to_token().unwrap();
  let id = id.to_string();

  let tok = nterm_tok.clone();

  let value = Default::default();

  let named_reference = nodes[0].clone();
  let named_reference = named_reference.to_token().unwrap();
  let named_reference = named_reference.to_string();

  ASTNode::AST_Property(Box::new(AST_Property { id, tok, value, named_reference }))
}

fn rule_99<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Token().unwrap_unchecked() };
  out.into()
}

fn rule_100<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_101<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_102<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

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

  ASTNode::ReduceRaw(Box::new(ReduceRaw { tok, len, rule_id, nonterminal_id }))
}

fn rule_103<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

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

  ASTNode::ReduceRaw(Box::new(ReduceRaw { tok, len, rule_id, nonterminal_id }))
}

fn rule_104<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

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

  ASTNode::ReduceRaw(Box::new(ReduceRaw { tok, len, rule_id, nonterminal_id }))
}

fn rule_105<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

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

  ASTNode::ReduceRaw(Box::new(ReduceRaw { tok, len, rule_id, nonterminal_id }))
}

fn rule_106<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();

  let ast = std::mem::take(&mut nodes[6]);
  let ast = unsafe {
    ast.into_body_Value/*9*/().unwrap_unchecked()
  };

  let nonterminal = std::mem::take(&mut nodes[4]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Reduce(Box::new(Reduce { tok, len, ast, nonterminal }))
}

fn rule_107<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();

  let ast = std::mem::take(&mut nodes[4]);
  let ast = unsafe {
    ast.into_body_Value/*9*/().unwrap_unchecked()
  };

  let nonterminal = std::mem::take(&mut nodes[2]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Reduce(Box::new(Reduce { tok, len, ast, nonterminal }))
}

fn rule_108<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();

  let ast = Default::default();

  let nonterminal = std::mem::take(&mut nodes[4]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Reduce(Box::new(Reduce { tok, len, ast, nonterminal }))
}

fn rule_109<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let len = nodes[1].clone();
  let len = len.to_token().unwrap();
  let len: u32 = len.to_string().parse().unwrap_or_default();

  let ast = Default::default();

  let nonterminal = std::mem::take(&mut nodes[2]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Reduce(Box::new(Reduce { tok, len, ast, nonterminal }))
}

fn rule_110<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = nodes[1].clone();
  let id = id.to_token().unwrap();
  let id: u32 = id.to_string().parse().unwrap_or_default();

  let tok = nterm_tok.clone();

  ASTNode::SetTokenId(Box::new(SetTokenId { id, tok }))
}

fn rule_111<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let id = nodes[1].clone();
  let id = id.to_token().unwrap();
  let id: u32 = id.to_string().parse().unwrap_or_default();

  ASTNode::SetTokenLen(Box::new(SetTokenLen { id }))
}

fn rule_112<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let tok = nterm_tok.clone();
  ASTNode::SetLine(Box::new(SetLine { tok }))
}

fn rule_113<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  ASTNode::Token(out)
}

fn rule_114<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  ASTNode::Token(out)
}

fn rule_115<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  ASTNode::Token(out)
}

fn rule_116<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_body_Value/*9*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_117<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let count = nodes[1].clone();
  let count = count.to_token().unwrap();
  let count: u32 = count.to_string().parse().unwrap_or_default();

  ASTNode::Pop(Box::new(Pop { tok, count }))
}

fn rule_118<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_match_Value/*23*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_119<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Gotos().unwrap_unchecked() };
  out.into()
}

fn rule_120<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_terminal_statement_Value/*4*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_121<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = tokens[1].clone();
  let skip = skip.len() > 0;

  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Shift(Box::new(Shift { tok, skip, ptr_type }))
}

fn rule_122<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = tokens[1].clone();
  let skip = skip.len() > 0;

  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Shift(Box::new(Shift { tok, skip, ptr_type }))
}

fn rule_123<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = false;

  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Shift(Box::new(Shift { tok, skip, ptr_type }))
}

fn rule_124<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = false;

  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Shift(Box::new(Shift { tok, skip, ptr_type }))
}

fn rule_125<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = tokens[1].clone();
  let skip = skip.len() > 0;

  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Peek(Box::new(Peek { tok, skip, ptr_type }))
}

fn rule_126<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = tokens[1].clone();
  let skip = skip.len() > 0;

  let ptr_type = tokens[2].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Peek(Box::new(Peek { tok, skip, ptr_type }))
}

fn rule_127<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = false;

  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Peek(Box::new(Peek { tok, skip, ptr_type }))
}

fn rule_128<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let skip = false;

  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Peek(Box::new(Peek { tok, skip, ptr_type }))
}

fn rule_129<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Reset(Box::new(Reset { tok, ptr_type }))
}

fn rule_130<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let ptr_type = tokens[1].clone();
  let ptr_type = ptr_type.to_string();

  ASTNode::Reset(Box::new(Reset { tok, ptr_type }))
}

fn rule_131<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_132<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_133<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_134<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_135<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_136<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_137<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  let reference = nodes[3].clone();
  let reference = reference.to_token().unwrap();
  let reference = reference.to_string();

  ASTNode::Export(Box::new(Export { nonterminal, reference }))
}

fn rule_138<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  let reference = nodes[3].clone();
  let reference = reference.to_token().unwrap();
  let reference = reference.to_string();

  ASTNode::Export(Box::new(Export { nonterminal, reference }))
}

fn rule_139<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  let reference: String = String::default();

  ASTNode::Export(Box::new(Export { nonterminal, reference }))
}

fn rule_140<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_141<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_142<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_143<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_144<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let uri = std::mem::take(&mut nodes[1]);
  let uri = unsafe { uri.into_vec_Token().unwrap_unchecked() };
  let uri = Token::from_slice(&uri).to_string();

  let tok = nterm_tok.clone();

  let reference = nodes[4].clone();
  let reference = reference.to_token().unwrap();
  let reference = reference.to_string();

  ASTNode::Import(Box::new(Import { uri, tok, reference }))
}

fn rule_145<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let uri = std::mem::take(&mut nodes[1]);
  let uri = unsafe { uri.into_vec_Token().unwrap_unchecked() };
  let uri = Token::from_slice(&uri).to_string();

  let tok = nterm_tok.clone();

  let reference = nodes[4].clone();
  let reference = reference.to_token().unwrap();
  let reference = reference.to_string();

  ASTNode::Import(Box::new(Import { uri, tok, reference }))
}

fn rule_146<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_147<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_148<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_149<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = tokens[0].clone();

  let out = vec![out_0];
  ASTNode::vec_Token(out)
}

fn rule_150<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = tokens[0].clone();

  let out = vec![out_0];
  ASTNode::vec_Token(out)
}

fn rule_151<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = tokens[0].clone();

  let out = vec![out_0];
  ASTNode::vec_Token(out)
}

fn rule_152<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_Token().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::vec_Token(out)
}

fn rule_153<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_Token().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::vec_Token(out)
}

fn rule_154<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = tokens[1].clone();
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_Token().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  ASTNode::vec_Token(out)
}

fn rule_155<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_156<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_157<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let name = nodes[1].clone();
  let name = name.to_token().unwrap();
  let name = name.to_string();

  ASTNode::Name(Box::new(Name { name }))
}

fn rule_158<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let symbols = std::mem::take(&mut nodes[2]);
  let symbols = unsafe {
    symbols.into_vec_ignore_clause_list_Value/*42*/().unwrap_unchecked()
  };

  ASTNode::Ignore(Box::new(Ignore { symbols }))
}

fn rule_159<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let symbols = Default::default();

  ASTNode::Ignore(Box::new(Ignore { symbols }))
}

fn rule_160<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 42
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Multi
  from index: 28
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_ignore_clause_list_Value/*28*/().unwrap_unchecked()
  };
  let out_0: ignore_clause_list_Value<Token>/*42*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_161<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 42
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 28
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe {
    out_r.into_ignore_clause_list_Value/*28*/().unwrap_unchecked()
  };
  let out_r: ignore_clause_list_Value<Token>/*42*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_ignore_clause_list_Value/*42*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_162<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_vec_Rule().unwrap_unchecked() };
  out.into()
}

fn rule_163<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_Rule().unwrap_unchecked() };

  let out = vec![out_0];
  out.into()
}

fn rule_164<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe { out_r.into_Rule().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_Rule().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_165<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[1]);
  let rules = unsafe { rules.into_vec_Rule().unwrap_unchecked() };

  ASTNode::Grouped_Rules(Box::new(Grouped_Rules { tok, rules }))
}

fn rule_166<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_NonTerminal_Symbol().unwrap_unchecked() };
  out.into()
}

fn rule_167<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_NonTerminal_Import_Symbol().unwrap_unchecked() };
  out.into()
}

fn rule_168<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let val = nodes[0].clone();
  let val = val.to_token().unwrap();
  let val = val.to_string();

  ASTNode::TemplateSym(Box::new(TemplateSym { val }))
}

fn rule_169<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let val = nodes[0].clone();
  let val = val.to_token().unwrap();
  let val = val.to_string();

  ASTNode::TemplateSym(Box::new(TemplateSym { val }))
}

fn rule_170<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let val = nodes[0].clone();
  let val = val.to_token().unwrap();
  let val = val.to_string();

  ASTNode::TemplateASTType(Box::new(TemplateASTType { val }))
}

fn rule_171<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  ASTNode::Token(out)
}

fn rule_172<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_member_Value/*32*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_173<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Map().unwrap_unchecked() };
  out.into()
}

fn rule_174<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Vector().unwrap_unchecked() };
  out.into()
}

fn rule_175<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_String().unwrap_unchecked() };
  out.into()
}

fn rule_176<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Bool().unwrap_unchecked() };
  out.into()
}

fn rule_177<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_number_Value/*5*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_178<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_literal_Value/*6*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_179<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[0].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_180<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe { range.into_Range().unwrap_unchecked() };

  ASTNode::AST_Token(Box::new(AST_Token { range }))
}

fn rule_181<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe { range.into_Range().unwrap_unchecked() };

  ASTNode::AST_Token(Box::new(AST_Token { range }))
}

fn rule_182<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe { range.into_Range().unwrap_unchecked() };

  ASTNode::AST_Token(Box::new(AST_Token { range }))
}

fn rule_183<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let range = Default::default();

  ASTNode::AST_Token(Box::new(AST_Token { range }))
}

fn rule_184<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let range = Default::default();

  ASTNode::AST_Token(Box::new(AST_Token { range }))
}

fn rule_185<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let range = Default::default();

  ASTNode::AST_Token(Box::new(AST_Token { range }))
}

fn rule_186<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_187<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_188<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_189<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Struct().unwrap_unchecked() };
  out.into()
}

fn rule_190<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Flag().unwrap_unchecked() };
  out.into()
}

fn rule_191<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let expression = std::mem::take(&mut nodes[0]);
  let expression = unsafe {
    expression.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Statement(Box::new(AST_Statement { tok, expression }))
}

fn rule_192<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let expression = std::mem::take(&mut nodes[1]);
  let expression = unsafe {
    expression.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Statement(Box::new(AST_Statement { tok, expression }))
}

fn rule_193<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_194<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Matches().unwrap_unchecked() };
  out.into()
}

fn rule_195<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_ProductionMatches().unwrap_unchecked() };
  out.into()
}

fn rule_196<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_TerminalMatches().unwrap_unchecked() };
  out.into()
}

fn rule_197<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let fork = Default::default();

  let goto = std::mem::take(&mut nodes[2]);
  let goto = unsafe { goto.into_Goto().unwrap_unchecked() };

  let pushes = std::mem::take(&mut nodes[0]);
  let pushes = unsafe { pushes.into_vec_Push().unwrap_unchecked() };

  ASTNode::Gotos(Box::new(Gotos { fork, goto, pushes }))
}

fn rule_198<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let fork = Default::default();

  let goto = std::mem::take(&mut nodes[0]);
  let goto = unsafe { goto.into_Goto().unwrap_unchecked() };

  let pushes = Default::default();

  ASTNode::Gotos(Box::new(Gotos { fork, goto, pushes }))
}

fn rule_199<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let fork = std::mem::take(&mut nodes[0]);
  let fork = unsafe { fork.into_Fork().unwrap_unchecked() };

  let goto = Default::default();

  let pushes = Default::default();

  ASTNode::Gotos(Box::new(Gotos { fork, goto, pushes }))
}

fn rule_200<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_Push().unwrap_unchecked() };

  let out = vec![out_0];
  out.into()
}

fn rule_201<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe { out_r.into_Push().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_Push().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_202<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let tok = nterm_tok.clone();
  ASTNode::Fail(Box::new(Fail { tok }))
}

fn rule_203<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let tok = nterm_tok.clone();
  ASTNode::Pass(Box::new(Pass { tok }))
}

fn rule_204<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let tok = nterm_tok.clone();
  ASTNode::Accept(Box::new(Accept { tok }))
}

fn rule_205<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let rules = std::mem::take(&mut nodes[1]);
  let rules = unsafe { rules.into_vec_Rule().unwrap_unchecked() };

  ASTNode::TokenGroupRules(Box::new(TokenGroupRules { tok, rules }))
}

fn rule_206<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_TerminalToken().unwrap_unchecked() };
  out.into()
}

fn rule_207<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_NonTerminal_Terminal_Symbol().unwrap_unchecked() };
  out.into()
}

fn rule_208<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_ClassSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_209<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let ast = std::mem::take(&mut nodes[1]);
  let ast = unsafe { ast.into_Ascript().unwrap_unchecked() };

  let symbols = std::mem::take(&mut nodes[0]);
  let symbols = unsafe {
    symbols.into_vec_rule_group_2_Value/*17*/().unwrap_unchecked()
  };

  ASTNode::Rule(Box::new(Rule { tok, ast, symbols }))
}

fn rule_210<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let ast = Default::default();

  let symbols = std::mem::take(&mut nodes[0]);
  let symbols = unsafe {
    symbols.into_vec_rule_group_2_Value/*17*/().unwrap_unchecked()
  };

  ASTNode::Rule(Box::new(Rule { tok, ast, symbols }))
}

fn rule_211<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_annotated_symbol_Value/*37*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_212<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_NotEmptySet().unwrap_unchecked() };
  out.into()
}

fn rule_213<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 18
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Multi
  from index: 37
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_annotated_symbol_Value/*37*/().unwrap_unchecked()
  };
  let out_0: rule_group_Value<Token>/*18*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_214<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 18
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_NotEmptySet().unwrap_unchecked() };
  let out_0: rule_group_Value<Token>/*18*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_215<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 18
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 37
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe {
    out_r.into_annotated_symbol_Value/*37*/().unwrap_unchecked()
  };
  let out_r: rule_group_Value<Token>/*18*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_rule_group_Value/*18*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_216<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 18
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_NotEmptySet().unwrap_unchecked() };
  let out_r: rule_group_Value<Token>/*18*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_rule_group_Value/*18*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_217<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 17
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_EOFSymbol().unwrap_unchecked() };
  let out_r: rule_group_2_Value<Token>/*17*/= out_r.into();

  /*to index id: 17
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 18
  from val: SymNode
  from agg: Vec
  */
  let out_l_r = std::mem::take(&mut nodes[0]);
  let out_l_r = unsafe {
    out_l_r.into_vec_rule_group_Value/*18*/().unwrap_unchecked()
  };
  let out_l_r: Vec<rule_group_2_Value<Token> /* 17 */> = out_l_r.into_iter().map(|v| v.into()).collect();

  /* 0 */
  let out_l_l: /*17*/Vec<rule_group_2_Value<Token>> = vec![];
  let mut out_l = out_l_l;
  out_l.extend(out_l_r);
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_218<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 17
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: VecNode
  from agg: Vec
  */
  /* 0 */
  let out: Vec<Box<EOFSymbol<Token>>> = vec![];
  let out: Vec<rule_group_2_Value<Token> /* 17 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_219<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 17
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 18
  from val: AddNode
  from agg: Vec
  */
  let out_r = std::mem::take(&mut nodes[0]);
  let out_r = unsafe {
    out_r.into_vec_rule_group_Value/*18*/().unwrap_unchecked()
  };
  /* 0 */
  let out_l: /*18*/Vec<rule_group_Value<Token>> = vec![];
  let mut out = out_l;
  out.extend(out_r);
  let out: Vec<rule_group_2_Value<Token> /* 17 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_220<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let name = nodes[2].clone();
  let name = name.to_token().unwrap();
  let name = name.to_string();

  let module = nodes[0].clone();
  let module = module.to_token().unwrap();
  let module = module.to_string();

  ASTNode::NonTerminal_Import_Symbol(Box::new(NonTerminal_Import_Symbol { tok, name, module }))
}

fn rule_221<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_trimmed_reference_Value/*33*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_222<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let property = nodes[2].clone();
  let property = property.to_token().unwrap();

  let reference = std::mem::take(&mut nodes[0]);
  let reference = unsafe {
    reference.into_reference_Value/*7*/().unwrap_unchecked()
  };

  ASTNode::AST_Member(Box::new(AST_Member { property, reference }))
}

fn rule_223<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = std::mem::take(&mut nodes[4]);
  let val = unsafe {
    val.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let key = std::mem::take(&mut nodes[2]);
  let key = unsafe {
    key.into_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Map(Box::new(AST_Map { tok, val, key }))
}

fn rule_224<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe {
    initializer.into_vec_expr_Value/*0*/().unwrap_unchecked()
  };

  ASTNode::AST_Vector(Box::new(AST_Vector { tok, initializer }))
}

fn rule_225<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_Vector(Box::new(AST_Vector { tok, initializer }))
}

fn rule_226<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_expr_Value/*0*/().unwrap_unchecked()
  };

  let out = vec![out_0];
  out.into()
}

fn rule_227<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe {
    out_r.into_expr_Value/*0*/().unwrap_unchecked()
  };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_expr_Value/*0*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_228<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_String(Box::new(AST_String { tok, initializer }))
}

fn rule_229<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_String(Box::new(AST_String { tok, initializer }))
}

fn rule_230<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_Bool(Box::new(AST_Bool { tok, initializer }))
}

fn rule_231<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_Bool(Box::new(AST_Bool { tok, initializer }))
}

fn rule_232<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_U8(Box::new(AST_U8 { tok, initializer }))
}

fn rule_233<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_U8(Box::new(AST_U8 { tok, initializer }))
}

fn rule_234<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_U16(Box::new(AST_U16 { tok, initializer }))
}

fn rule_235<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_U16(Box::new(AST_U16 { tok, initializer }))
}

fn rule_236<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_U32(Box::new(AST_U32 { tok, initializer }))
}

fn rule_237<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_U32(Box::new(AST_U32 { tok, initializer }))
}

fn rule_238<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_U64(Box::new(AST_U64 { tok, initializer }))
}

fn rule_239<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_U64(Box::new(AST_U64 { tok, initializer }))
}

fn rule_240<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_U128(Box::new(AST_U128 { tok, initializer }))
}

fn rule_241<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_U128(Box::new(AST_U128 { tok, initializer }))
}

fn rule_242<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_I8(Box::new(AST_I8 { tok, initializer }))
}

fn rule_243<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_I8(Box::new(AST_I8 { tok, initializer }))
}

fn rule_244<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_I16(Box::new(AST_I16 { tok, initializer }))
}

fn rule_245<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_I16(Box::new(AST_I16 { tok, initializer }))
}

fn rule_246<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_I32(Box::new(AST_I32 { tok, initializer }))
}

fn rule_247<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_I32(Box::new(AST_I32 { tok, initializer }))
}

fn rule_248<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_I64(Box::new(AST_I64 { tok, initializer }))
}

fn rule_249<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_I64(Box::new(AST_I64 { tok, initializer }))
}

fn rule_250<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_I64(Box::new(AST_I64 { tok, initializer }))
}

fn rule_251<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_I64(Box::new(AST_I64 { tok, initializer }))
}

fn rule_252<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_F16(Box::new(AST_F16 { tok, initializer }))
}

fn rule_253<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_F16(Box::new(AST_F16 { tok, initializer }))
}

fn rule_254<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_F32(Box::new(AST_F32 { tok, initializer }))
}

fn rule_255<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_F32(Box::new(AST_F32 { tok, initializer }))
}

fn rule_256<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_F64(Box::new(AST_F64 { tok, initializer }))
}

fn rule_257<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_F64(Box::new(AST_F64 { tok, initializer }))
}

fn rule_258<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = std::mem::take(&mut nodes[1]);
  let initializer = unsafe { initializer.into_Init().unwrap_unchecked() };

  ASTNode::AST_F128(Box::new(AST_F128 { tok, initializer }))
}

fn rule_259<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let initializer = Default::default();

  ASTNode::AST_F128(Box::new(AST_F128 { tok, initializer }))
}

fn rule_260<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let value = true;

  ASTNode::AST_BoolLiteral(Box::new(AST_BoolLiteral { value }))
}

fn rule_261<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let value = false;

  ASTNode::AST_BoolLiteral(Box::new(AST_BoolLiteral { value }))
}

fn rule_262<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let value = nodes[0].clone();
  let value = value.to_token().unwrap();
  let value: f64 = value.to_string().parse().unwrap_or_default();

  ASTNode::AST_NumberLiteral(Box::new(AST_NumberLiteral { value }))
}

fn rule_263<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let value = nodes[0].clone();
  let value = value.to_token().unwrap();
  let value = value.to_string();

  ASTNode::AST_StringLiteral(Box::new(AST_StringLiteral { tok, value }))
}

fn rule_264<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let end_trim = nodes[3].clone();
  let end_trim = end_trim.to_token().unwrap();
  let end_trim: i32 = end_trim.to_string().parse().unwrap_or_default();

  let start_trim = nodes[1].clone();
  let start_trim = start_trim.to_token().unwrap();
  let start_trim: i32 = start_trim.to_string().parse().unwrap_or_default();

  ASTNode::Range(Box::new(Range { end_trim, start_trim }))
}

fn rule_265<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let end_trim = 0;

  let start_trim = nodes[1].clone();
  let start_trim = start_trim.to_token().unwrap();
  let start_trim: i32 = start_trim.to_string().parse().unwrap_or_default();

  ASTNode::Range(Box::new(Range { end_trim, start_trim }))
}

fn rule_266<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_267<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let ty = nodes[0].clone();
  let ty = ty.to_token().unwrap();
  let ty = ty.to_string();

  let tok = nterm_tok.clone();

  let val = nodes[2].clone();
  let val = val.to_token().unwrap();

  ASTNode::AST_Flag(Box::new(AST_Flag { ty, tok, val }))
}

fn rule_268<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let mode = nodes[2].clone();
  let mode = mode.to_token().unwrap();
  let mode = mode.to_string();

  let scanner = std::mem::take(&mut nodes[3]);
  let scanner = unsafe { scanner.into_String().unwrap_unchecked() };

  let matches = std::mem::take(&mut nodes[4]);
  let matches = unsafe {
    matches.into_vec_generic_match_block_group_1_Value/*22*/().unwrap_unchecked()
  };

  ASTNode::Matches(Box::new(Matches { tok, mode, scanner, matches }))
}

fn rule_269<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let mode = nodes[2].clone();
  let mode = mode.to_token().unwrap();
  let mode = mode.to_string();

  let scanner = Default::default();

  let matches = std::mem::take(&mut nodes[3]);
  let matches = unsafe {
    matches.into_vec_generic_match_block_group_1_Value/*22*/().unwrap_unchecked()
  };

  ASTNode::Matches(Box::new(Matches { tok, mode, scanner, matches }))
}

fn rule_270<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  let out = out.to_string();
  out.into()
}

fn rule_271<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_IntMatch().unwrap_unchecked() };
  out.into()
}

fn rule_272<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_DefaultMatch().unwrap_unchecked() };
  out.into()
}

fn rule_273<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_FailHint().unwrap_unchecked() };
  out.into()
}

fn rule_274<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 21
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_IntMatch().unwrap_unchecked() };
  let out_0: generic_match_block_group_1_Value<Token>/*21*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_275<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 21
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_DefaultMatch().unwrap_unchecked() };
  let out_0: generic_match_block_group_1_Value<Token>/*21*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_276<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 21
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_FailHint().unwrap_unchecked() };
  let out_0: generic_match_block_group_1_Value<Token>/*21*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_277<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 21
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_IntMatch().unwrap_unchecked() };
  let out_r: generic_match_block_group_1_Value<Token>/*21*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_generic_match_block_group_1_Value/*21*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_278<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 21
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_DefaultMatch().unwrap_unchecked() };
  let out_r: generic_match_block_group_1_Value<Token>/*21*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_generic_match_block_group_1_Value/*21*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_279<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 21
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_FailHint().unwrap_unchecked() };
  let out_r: generic_match_block_group_1_Value<Token>/*21*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_generic_match_block_group_1_Value/*21*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_280<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 22
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: VecNode
  from agg: Vec
  */
  /* 0 */
  let out: Vec<Box<IntMatch<Token>>> = vec![];
  let out: Vec<generic_match_block_group_1_Value<Token> /* 22 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_281<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 22
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 21
  from val: SymNode
  from agg: Vec
  */
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_vec_generic_match_block_group_1_Value/*21*/().unwrap_unchecked()
  };
  let out: Vec<generic_match_block_group_1_Value<Token> /* 22 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_282<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let matches = std::mem::take(&mut nodes[3]);
  let matches = unsafe {
    matches.into_vec_nonterminal_match_block_group_Value/*25*/().unwrap_unchecked()
  };

  ASTNode::ProductionMatches(Box::new(ProductionMatches { matches }))
}

fn rule_283<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_NonTermMatch().unwrap_unchecked() };
  out.into()
}

fn rule_284<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_FailHint().unwrap_unchecked() };
  out.into()
}

fn rule_285<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_DefaultMatch().unwrap_unchecked() };
  out.into()
}

fn rule_286<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 24
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_NonTermMatch().unwrap_unchecked() };
  let out_0: nonterminal_match_block_group_Value<Token>/*24*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_287<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 24
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_FailHint().unwrap_unchecked() };
  let out_0: nonterminal_match_block_group_Value<Token>/*24*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_288<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 24
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_DefaultMatch().unwrap_unchecked() };
  let out_0: nonterminal_match_block_group_Value<Token>/*24*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_289<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 24
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_NonTermMatch().unwrap_unchecked() };
  let out_r: nonterminal_match_block_group_Value<Token>/*24*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_nonterminal_match_block_group_Value/*24*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_290<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 24
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_FailHint().unwrap_unchecked() };
  let out_r: nonterminal_match_block_group_Value<Token>/*24*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_nonterminal_match_block_group_Value/*24*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_291<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 24
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_DefaultMatch().unwrap_unchecked() };
  let out_r: nonterminal_match_block_group_Value<Token>/*24*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_nonterminal_match_block_group_Value/*24*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_292<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 25
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: VecNode
  from agg: Vec
  */
  /* 0 */
  let out: Vec<Box<NonTermMatch<Token>>> = vec![];
  let out: Vec<nonterminal_match_block_group_Value<Token> /* 25 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_293<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 25
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 24
  from val: SymNode
  from agg: Vec
  */
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_vec_nonterminal_match_block_group_Value/*24*/().unwrap_unchecked()
  };
  let out: Vec<nonterminal_match_block_group_Value<Token> /* 25 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_294<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let matches = std::mem::take(&mut nodes[3]);
  let matches = unsafe {
    matches.into_vec_terminal_match_block_group_Value/*27*/().unwrap_unchecked()
  };

  ASTNode::TerminalMatches(Box::new(TerminalMatches { matches }))
}

fn rule_295<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_TermMatch().unwrap_unchecked() };
  out.into()
}

fn rule_296<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_FailHint().unwrap_unchecked() };
  out.into()
}

fn rule_297<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_DefaultMatch().unwrap_unchecked() };
  out.into()
}

fn rule_298<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 26
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_TermMatch().unwrap_unchecked() };
  let out_0: terminal_match_block_group_Value<Token>/*26*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_299<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 26
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_FailHint().unwrap_unchecked() };
  let out_0: terminal_match_block_group_Value<Token>/*26*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_300<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 26
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_DefaultMatch().unwrap_unchecked() };
  let out_0: terminal_match_block_group_Value<Token>/*26*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_301<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 26
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_TermMatch().unwrap_unchecked() };
  let out_r: terminal_match_block_group_Value<Token>/*26*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_terminal_match_block_group_Value/*26*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_302<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 26
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_FailHint().unwrap_unchecked() };
  let out_r: terminal_match_block_group_Value<Token>/*26*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_terminal_match_block_group_Value/*26*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_303<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 26
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_DefaultMatch().unwrap_unchecked() };
  let out_r: terminal_match_block_group_Value<Token>/*26*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_terminal_match_block_group_Value/*26*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_304<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 27
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Struct
  from index: Struct
  from val: VecNode
  from agg: Vec
  */
  /* 0 */
  let out: Vec<Box<TermMatch<Token>>> = vec![];
  let out: Vec<terminal_match_block_group_Value<Token> /* 27 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_305<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 27
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 26
  from val: SymNode
  from agg: Vec
  */
  let out = std::mem::take(&mut nodes[1]);
  let out = unsafe {
    out.into_vec_terminal_match_block_group_Value/*26*/().unwrap_unchecked()
  };
  let out: Vec<terminal_match_block_group_Value<Token> /* 27 */> = out.into_iter().map(|v| v.into()).collect();

  out.into()
}

fn rule_306<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let name = std::mem::take(&mut nodes[1]);
  let name = unsafe {
    name.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };
  let name: String = String::default();

  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Push(Box::new(Push { tok, name, nonterminal }))
}

fn rule_307<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let name = nodes[1].clone();
  let name = unsafe {
    name.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };
  let name: String = String::default();

  let nonterminal = std::mem::take(&mut nodes[1]);

  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Goto(Box::new(Goto { tok, name, nonterminal }))
}

fn rule_308<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let paths = std::mem::take(&mut nodes[2]);
  let paths = unsafe { paths.into_vec_Goto().unwrap_unchecked() };

  ASTNode::Fork(Box::new(Fork { tok, paths }))
}

fn rule_309<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let name = std::mem::take(&mut nodes[0]);
  let name = unsafe {
    name.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };
  let name: String = String::default();

  let nonterminal = std::mem::take(&mut nodes[0]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::Goto(Box::new(Goto { tok, name, nonterminal }))
}

fn rule_310<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_Goto().unwrap_unchecked() };

  let out = vec![out_0];
  out.into()
}

fn rule_311<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe { out_r.into_Goto().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_Goto().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_312<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = nterm_tok.clone();
  let val = val.trim(1, 1);
  let val = val.to_string();

  let is_exclusive = true;

  ASTNode::TerminalToken(Box::new(TerminalToken { tok, val, is_exclusive }))
}

fn rule_313<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = nterm_tok.clone();
  let val = val.trim(1, 1);
  let val = val.to_string();

  let is_exclusive = Default::default();

  ASTNode::TerminalToken(Box::new(TerminalToken { tok, val, is_exclusive }))
}

fn rule_314<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let nonterminal = std::mem::take(&mut nodes[1]);
  let nonterminal = unsafe {
    nonterminal.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  ASTNode::NonTerminal_Terminal_Symbol(Box::new(NonTerminal_Terminal_Symbol { tok, nonterminal }))
}

fn rule_315<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_316<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_317<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_318<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_319<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_320<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_321<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_322<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_323<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let val = tokens[1].clone();
  let val = val.to_string();

  ASTNode::ClassSymbol(Box::new(ClassSymbol { tok, val }))
}

fn rule_324<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_325<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_326<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_327<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_328<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_329<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_330<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_331<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_332<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_333<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[3]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[1].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_334<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len() > 0;

  let reference: String = String::default();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_335<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = false;

  let reference = tokens[1].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_336<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = false;

  let reference: String = String::default();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_337<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = Default::default();

  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[1].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_338<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = Default::default();

  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len() > 0;

  let reference: String = String::default();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_339<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = Default::default();

  let is_optional = false;

  let reference = tokens[1].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_340<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[3].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[1].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_341<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len() > 0;

  let reference: String = String::default();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_342<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[3]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[2].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_343<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = Default::default();

  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[2].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_344<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[2]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[1].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[3].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_345<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = false;

  let reference = tokens[2].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_346<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[3].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[2].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_347<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_list_Value/*43*/().unwrap_unchecked()
  };

  let precedence = std::mem::take(&mut nodes[1]);
  let precedence = unsafe { precedence.into_Precedence().unwrap_unchecked() };

  let is_optional = tokens[2].clone();
  let is_optional = is_optional.len() > 0;

  let reference = tokens[3].clone();
  let reference = reference.to_string();

  ASTNode::AnnotatedSymbol(Box::new(AnnotatedSymbol { tok, symbol, precedence, is_optional, reference }))
}

fn rule_348<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_list_Value/*43*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_349<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbols = std::mem::take(&mut nodes[1]);
  let symbols = unsafe {
    symbols.into_vec_annotated_symbol_Value/*44*/().unwrap_unchecked()
  };

  let allow_empty = Default::default();

  let unordered = tokens[3].clone();
  let unordered = unordered.len() > 0;

  ASTNode::NotEmptySet(Box::new(NotEmptySet { tok, symbols, allow_empty, unordered }))
}

fn rule_350<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbols = std::mem::take(&mut nodes[1]);
  let symbols = unsafe {
    symbols.into_vec_annotated_symbol_Value/*44*/().unwrap_unchecked()
  };

  let allow_empty = Default::default();

  let unordered = false;

  ASTNode::NotEmptySet(Box::new(NotEmptySet { tok, symbols, allow_empty, unordered }))
}

fn rule_351<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbols = std::mem::take(&mut nodes[1]);
  let symbols = unsafe {
    symbols.into_vec_annotated_symbol_Value/*44*/().unwrap_unchecked()
  };

  let allow_empty = tokens[2].clone();
  let allow_empty = allow_empty.len() > 0;

  let unordered = tokens[2].clone();
  let unordered = unordered.len() > 0;

  ASTNode::NotEmptySet(Box::new(NotEmptySet { tok, symbols, allow_empty, unordered }))
}

fn rule_352<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  ASTNode::EOFSymbol(Box::new(EOFSymbol { tok }))
}

fn rule_353<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let ast = std::mem::take(&mut nodes[1]);
  let ast = unsafe {
    ast.into_body_Value/*9*/().unwrap_unchecked()
  };

  ASTNode::Ascript(Box::new(Ascript { tok, ast }))
}

fn rule_354<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_reference_Value/*7*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_355<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let range = std::mem::take(&mut nodes[1]);
  let range = unsafe { range.into_Range().unwrap_unchecked() };

  let reference = std::mem::take(&mut nodes[0]);
  let reference = unsafe {
    reference.into_reference_Value/*7*/().unwrap_unchecked()
  };

  ASTNode::AST_TrimmedReference(Box::new(AST_TrimmedReference { tok, range, reference }))
}

fn rule_356<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let value = nodes[1].clone();
  let value = value.to_token().unwrap();
  let value = value.to_string();

  ASTNode::AST_NamedReference(Box::new(AST_NamedReference { tok, value }))
}

fn rule_357<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let value = nodes[1].clone();
  let value = value.to_token().unwrap();
  let value: i64 = value.to_string().parse().unwrap_or_default();

  ASTNode::AST_IndexReference(Box::new(AST_IndexReference { tok, value }))
}

fn rule_358<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let expression = std::mem::take(&mut nodes[1]);
  let expression = unsafe {
    expression.into_init_objects_Value/*19*/().unwrap_unchecked()
  };

  ASTNode::Init(Box::new(Init { expression }))
}

fn rule_359<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_360<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_361<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[1].clone();
  let out = out.to_token().unwrap();
  ASTNode::Token(out)
}

fn rule_362<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let vals = std::mem::take(&mut nodes[1]);
  let vals = unsafe { vals.into_vec_U64().unwrap_unchecked() };

  let statement = std::mem::take(&mut nodes[4]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::IntMatch(Box::new(IntMatch { vals, statement }))
}

fn rule_363<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = nodes[0].clone();
  let out = out.to_token().unwrap();
  let out: u64 = out.to_string().parse().unwrap_or_default();
  out.into()
}

fn rule_364<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe { out_0.into_U64().unwrap_unchecked() };

  let out = vec![out_0];
  out.into()
}

fn rule_365<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe { out_r.into_U64().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe { out_l.into_vec_U64().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_366<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let statement = std::mem::take(&mut nodes[2]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::DefaultMatch(Box::new(DefaultMatch { statement }))
}

fn rule_367<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let statement = std::mem::take(&mut nodes[1]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::DefaultMatch(Box::new(DefaultMatch { statement }))
}

fn rule_368<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let message = nodes[2].clone();
  let message = message.to_token().unwrap();
  let message = message.to_string();

  ASTNode::FailHint(Box::new(FailHint { message }))
}

fn rule_369<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym = std::mem::take(&mut nodes[1]);
  let sym = unsafe {
    sym.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  let statement = std::mem::take(&mut nodes[4]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::NonTermMatch(Box::new(NonTermMatch { sym, statement }))
}

fn rule_370<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym = std::mem::take(&mut nodes[1]);
  let sym = unsafe {
    sym.into_ignore_clause_list_Value/*28*/().unwrap_unchecked()
  };

  let statement = std::mem::take(&mut nodes[4]);
  let statement = unsafe { statement.into_Statement().unwrap_unchecked() };

  ASTNode::TermMatch(Box::new(TermMatch { sym, statement }))
}

fn rule_371<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[0].clone();
  ASTNode::Token(out)
}

fn rule_372<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = Default::default();

  let terminal_symbol = Default::default();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_373<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = true;

  let terminal_symbol = Default::default();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_374<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = Default::default();

  /*to index id: 43
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let terminal_symbol = std::mem::take(&mut nodes[2]);
  let terminal_symbol = unsafe { terminal_symbol.into_TerminalToken().unwrap_unchecked() };
  let terminal_symbol: list_Value<Token>/*43*/= terminal_symbol.into();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_375<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = Default::default();

  /*to index id: 43
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let terminal_symbol = std::mem::take(&mut nodes[2]);
  let terminal_symbol = unsafe { terminal_symbol.into_ClassSymbol().unwrap_unchecked() };
  let terminal_symbol: list_Value<Token>/*43*/= terminal_symbol.into();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_376<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = Default::default();

  let terminal_symbol = Default::default();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_377<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = true;

  /*to index id: 43
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let terminal_symbol = std::mem::take(&mut nodes[2]);
  let terminal_symbol = unsafe { terminal_symbol.into_TerminalToken().unwrap_unchecked() };
  let terminal_symbol: list_Value<Token>/*43*/= terminal_symbol.into();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_378<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = true;

  /*to index id: 43
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Struct
  from index: Struct
  from val: SymNode
  from agg:
  */
  let terminal_symbol = std::mem::take(&mut nodes[2]);
  let terminal_symbol = unsafe { terminal_symbol.into_ClassSymbol().unwrap_unchecked() };
  let terminal_symbol: list_Value<Token>/*43*/= terminal_symbol.into();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_379<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let symbol = std::mem::take(&mut nodes[0]);
  let symbol = unsafe {
    symbol.into_symbol_Value/*16*/().unwrap_unchecked()
  };

  let optional = true;

  let terminal_symbol = Default::default();

  ASTNode::List_Rules(Box::new(List_Rules { tok, symbol, optional, terminal_symbol }))
}

fn rule_380<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_symbol_Value/*16*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_381<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_TerminalToken().unwrap_unchecked() };
  out.into()
}

fn rule_382<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_ClassSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_383<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_TerminalToken().unwrap_unchecked() };
  out.into()
}

fn rule_384<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_ClassSymbol().unwrap_unchecked() };
  out.into()
}

fn rule_385<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym_prec = tokens[1].clone();
  let sym_prec: u32 = sym_prec.to_string().parse().unwrap_or_default();

  let kot_prec = std::mem::take(&mut nodes[2]);
  let kot_prec = unsafe { kot_prec.into_U32().unwrap_unchecked() };

  let is_keyword = Default::default();

  ASTNode::Precedence(Box::new(Precedence { sym_prec, kot_prec, is_keyword }))
}

fn rule_386<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym_prec = 0;

  let kot_prec = std::mem::take(&mut nodes[1]);
  let kot_prec = unsafe { kot_prec.into_U32().unwrap_unchecked() };

  let is_keyword = Default::default();

  ASTNode::Precedence(Box::new(Precedence { sym_prec, kot_prec, is_keyword }))
}

fn rule_387<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym_prec = tokens[1].clone();
  let sym_prec: u32 = sym_prec.to_string().parse().unwrap_or_default();

  let kot_prec = Default::default();

  let is_keyword = Default::default();

  ASTNode::Precedence(Box::new(Precedence { sym_prec, kot_prec, is_keyword }))
}

fn rule_388<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym_prec = 0;

  let kot_prec = Default::default();

  let is_keyword = Default::default();

  ASTNode::Precedence(Box::new(Precedence { sym_prec, kot_prec, is_keyword }))
}

fn rule_389<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym_prec = tokens[1].clone();
  let sym_prec: u32 = sym_prec.to_string().parse().unwrap_or_default();

  let kot_prec = Default::default();

  let is_keyword = true;

  ASTNode::Precedence(Box::new(Precedence { sym_prec, kot_prec, is_keyword }))
}

fn rule_390<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let sym_prec = 0;

  let kot_prec = Default::default();

  let is_keyword = true;

  ASTNode::Precedence(Box::new(Precedence { sym_prec, kot_prec, is_keyword }))
}

fn rule_391<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  let out: u32 = out.to_string().parse().unwrap_or_default();
  out.into()
}

fn rule_392<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = 0;
  out.into()
}

fn rule_393<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = tokens[1].clone();
  ASTNode::Token(out)
}

fn rule_394<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_vec_annotated_symbol_Value/*39*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_395<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*1*//*to index id: 39
  to val type: AscriptType
  to val type: Multi
  to agg agg:

  from index: Multi
  from index: 37
  from val: SymNode
  from agg:
  */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_annotated_symbol_Value/*37*/().unwrap_unchecked()
  };
  let out_0: annotated_symbol_Value<Token>/*39*/= out_0.into();

  let out = vec![out_0];
  out.into()
}

fn rule_396<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /*to index id: 39
  to val type: AscriptType
  to val type: Multi
  to agg agg: Vec

  from index: Multi
  from index: 37
  from val: SymNode
  from agg:
  */
  let out_r = std::mem::take(&mut nodes[1]);
  let out_r = unsafe {
    out_r.into_annotated_symbol_Value/*37*/().unwrap_unchecked()
  };
  let out_r: annotated_symbol_Value<Token>/*39*/= out_r.into();
  let out_r = vec![out_r];

  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_annotated_symbol_Value/*39*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_397<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_member_Value/*32*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_398<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_AST_Token().unwrap_unchecked() };
  out.into()
}

fn rule_399<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_literal_Value/*6*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_400<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_401<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe { out.into_Template_NonTerminal_Symbol().unwrap_unchecked() };
  out.into()
}

fn rule_402<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_ignore_clause_list_Value/*28*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_403<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let tok = nterm_tok.clone();

  let name = std::mem::take(&mut nodes[0]);
  let name = unsafe {
    name.into_nonterminal_Value/*10*/().unwrap_unchecked()
  };

  let template_args = std::mem::take(&mut nodes[2]);
  let template_args = unsafe {
    template_args.into_vec_template_arg_Value/*38*/().unwrap_unchecked()
  };

  ASTNode::Template_NonTerminal_Symbol(Box::new(Template_NonTerminal_Symbol { tok, name, template_args }))
}

fn rule_404<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  /* 1 */
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe {
    out_0.into_template_arg_Value/*38*/().unwrap_unchecked()
  };

  let out = vec![out_0];
  out.into()
}

fn rule_405<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe {
    out_r.into_template_arg_Value/*38*/().unwrap_unchecked()
  };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe {
    out_l.into_vec_template_arg_Value/*38*/().unwrap_unchecked()
  };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_406<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe {
    out.into_list_Value/*43*/().unwrap_unchecked()
  };
  out.into()
}

fn rule_407<Token: Tk>(nodes: *mut [ASTNode<Token>], tokens: &[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes = unsafe { &mut *nodes };

  let typ = nterm_tok.clone();
  let typ = typ.to_string();

  ASTNode::AST_STRUCT_TEMPLATE_NAME(Box::new(AST_STRUCT_TEMPLATE_NAME { typ }))
}

pub struct ReduceRules<Token: Tk>(pub [Reducer<Token, ASTNode<Token>>; 408]);

impl<Token: Tk> ReduceRules<Token> {
  pub const fn new() -> Self {
    Self([
      rule_0, rule_1, rule_2, rule_3, rule_4, rule_5, rule_6, rule_7, rule_8, rule_9, rule_10, rule_11, rule_12, rule_13,
      rule_14, rule_15, rule_16, rule_17, rule_18, rule_19, rule_20, rule_21, rule_22, rule_23, rule_24, rule_25, rule_26,
      rule_27, rule_28, rule_29, rule_30, rule_31, rule_32, rule_33, rule_34, rule_35, rule_36, rule_37, rule_38, rule_39,
      rule_40, rule_41, rule_42, rule_43, rule_44, rule_45, rule_46, rule_47, rule_48, rule_49, rule_50, rule_51, rule_52,
      rule_53, rule_54, rule_55, rule_56, rule_57, rule_58, rule_59, rule_60, rule_61, rule_62, rule_63, rule_64, rule_65,
      rule_66, rule_67, rule_68, rule_69, rule_70, rule_71, rule_72, rule_73, rule_74, rule_75, rule_76, rule_77, rule_78,
      rule_79, rule_80, rule_81, rule_82, rule_83, rule_84, rule_85, rule_86, rule_87, rule_88, rule_89, rule_90, rule_91,
      rule_92, rule_93, rule_94, rule_95, rule_96, rule_97, rule_98, rule_99, rule_100, rule_101, rule_102, rule_103, rule_104,
      rule_105, rule_106, rule_107, rule_108, rule_109, rule_110, rule_111, rule_112, rule_113, rule_114, rule_115, rule_116,
      rule_117, rule_118, rule_119, rule_120, rule_121, rule_122, rule_123, rule_124, rule_125, rule_126, rule_127, rule_128,
      rule_129, rule_130, rule_131, rule_132, rule_133, rule_134, rule_135, rule_136, rule_137, rule_138, rule_139, rule_140,
      rule_141, rule_142, rule_143, rule_144, rule_145, rule_146, rule_147, rule_148, rule_149, rule_150, rule_151, rule_152,
      rule_153, rule_154, rule_155, rule_156, rule_157, rule_158, rule_159, rule_160, rule_161, rule_162, rule_163, rule_164,
      rule_165, rule_166, rule_167, rule_168, rule_169, rule_170, rule_171, rule_172, rule_173, rule_174, rule_175, rule_176,
      rule_177, rule_178, rule_179, rule_180, rule_181, rule_182, rule_183, rule_184, rule_185, rule_186, rule_187, rule_188,
      rule_189, rule_190, rule_191, rule_192, rule_193, rule_194, rule_195, rule_196, rule_197, rule_198, rule_199, rule_200,
      rule_201, rule_202, rule_203, rule_204, rule_205, rule_206, rule_207, rule_208, rule_209, rule_210, rule_211, rule_212,
      rule_213, rule_214, rule_215, rule_216, rule_217, rule_218, rule_219, rule_220, rule_221, rule_222, rule_223, rule_224,
      rule_225, rule_226, rule_227, rule_228, rule_229, rule_230, rule_231, rule_232, rule_233, rule_234, rule_235, rule_236,
      rule_237, rule_238, rule_239, rule_240, rule_241, rule_242, rule_243, rule_244, rule_245, rule_246, rule_247, rule_248,
      rule_249, rule_250, rule_251, rule_252, rule_253, rule_254, rule_255, rule_256, rule_257, rule_258, rule_259, rule_260,
      rule_261, rule_262, rule_263, rule_264, rule_265, rule_266, rule_267, rule_268, rule_269, rule_270, rule_271, rule_272,
      rule_273, rule_274, rule_275, rule_276, rule_277, rule_278, rule_279, rule_280, rule_281, rule_282, rule_283, rule_284,
      rule_285, rule_286, rule_287, rule_288, rule_289, rule_290, rule_291, rule_292, rule_293, rule_294, rule_295, rule_296,
      rule_297, rule_298, rule_299, rule_300, rule_301, rule_302, rule_303, rule_304, rule_305, rule_306, rule_307, rule_308,
      rule_309, rule_310, rule_311, rule_312, rule_313, rule_314, rule_315, rule_316, rule_317, rule_318, rule_319, rule_320,
      rule_321, rule_322, rule_323, rule_324, rule_325, rule_326, rule_327, rule_328, rule_329, rule_330, rule_331, rule_332,
      rule_333, rule_334, rule_335, rule_336, rule_337, rule_338, rule_339, rule_340, rule_341, rule_342, rule_343, rule_344,
      rule_345, rule_346, rule_347, rule_348, rule_349, rule_350, rule_351, rule_352, rule_353, rule_354, rule_355, rule_356,
      rule_357, rule_358, rule_359, rule_360, rule_361, rule_362, rule_363, rule_364, rule_365, rule_366, rule_367, rule_368,
      rule_369, rule_370, rule_371, rule_372, rule_373, rule_374, rule_375, rule_376, rule_377, rule_378, rule_379, rule_380,
      rule_381, rule_382, rule_383, rule_384, rule_385, rule_386, rule_387, rule_388, rule_389, rule_390, rule_391, rule_392,
      rule_393, rule_394, rule_395, rule_396, rule_397, rule_398, rule_399, rule_400, rule_401, rule_402, rule_403, rule_404,
      rule_405, rule_406, rule_407,
    ])
  }
}

impl<Token: Tk> AsRef<[Reducer<Token, ASTNode<Token>>]> for ReduceRules<Token> {
  fn as_ref(&self) -> &[Reducer<Token, ASTNode<Token>>] {
    &self.0
  }
}
