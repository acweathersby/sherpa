#![allow(unused)]
#![cfg_attr(rustfmt, rustfmt_skip)]
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
use std::{collections::HashMap, marker::PhantomData};

#[derive(Clone, Debug, Default)]
#[repr(C,u32)]
pub enum ASTNode<Token:Tk> {
  #[default]
  None, 
  Token(Token), 
  F64(f64), 
  String(String), 
  Bool(bool), 
  ValAnys(Vec<ValAny<Token>>), 
  StringValAny(HashMap<String,ValAny<Token>>), 
  ValAny(ValAny<Token>), 
  EntryAny(EntryAny<Token>), 
  JSON(Box<JSON<Token>>), 
  Null,
  Array(Box<Array<Token>>), 
  Object(Box<Object<Token>>), 
}

impl<Token:Tk> ASTNode<Token>{pub fn into_F64(self) -> Option<f64> {match self {ASTNode::F64(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<f64> for ASTNode<Token>{fn from(value:f64) -> Self {Self::F64(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_String(self) -> Option<String> {match self {ASTNode::String(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<String> for ASTNode<Token>{fn from(value:String) -> Self {Self::String(value)}}

impl<Token:Tk> ASTNode<Token>{pub fn into_Bool(self) -> Option<bool> {match self {ASTNode::Bool(val) => Some(val),_ => None,}}}

impl<Token:Tk> From<bool> for ASTNode<Token>{fn from(value:bool) -> Self {Self::Bool(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ValAnys(self) -> Option<Vec<ValAny<Token>>> {match self {ASTNode::ValAnys(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Vec<ValAny<Token>>> for ASTNode<Token>{fn from(value:Vec<ValAny<Token>>) -> Self {Self::ValAnys(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_StringValAny(self) -> Option<HashMap<String,ValAny<Token>>> {match self {ASTNode::StringValAny(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<HashMap<String,ValAny<Token>>> for ASTNode<Token>{fn from(value:HashMap<String,ValAny<Token>>) -> Self {Self::StringValAny(value)}}

#[derive(Clone, Debug, Default)]
pub enum ValAny<Token:Tk>{
  #[default]
  None,
  F64(f64), 
  String(String), 
  Bool(bool), 
  Null, 
  Array(Box<Array<Token>>), 
  Object(Box<Object<Token>>), 
}

#[derive(Clone, Debug, Default)]
pub enum EntryAny<Token:Tk>{#[default]None,Array(Box<Array<Token>>), Object(Box<Object<Token>>), }

impl<Token:Tk> ASTNode<Token>{
  pub fn into_ValAny(self) -> Option<ValAny<Token>> {
    match self {
      ASTNode::ValAny(val) => Some(val),
      ASTNode::F64(val) => Some(ValAny::F64(val)),
      ASTNode::String(val) => Some(ValAny::String(val)),
      ASTNode::Bool(val) => Some(ValAny::Bool(val)),
      ASTNode::Null => Some(ValAny::Null),
      ASTNode::Array(val) => Some(ValAny::Array(val)),
      ASTNode::Object(val) => Some(ValAny::Object(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<ValAny<Token>> for ASTNode<Token>{fn from(value: ValAny<Token>) -> Self {Self::ValAny(value)}}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_EntryAny(self) -> Option<EntryAny<Token>> {
    match self {
      ASTNode::EntryAny(val) => Some(val),
      ASTNode::Array(val) => Some(EntryAny::Array(val)),
      ASTNode::Object(val) => Some(EntryAny::Object(val)),
      _ => None,
    }
  }
}

impl<Token:Tk> From<EntryAny<Token>> for ASTNode<Token>{fn from(value: EntryAny<Token>) -> Self {Self::EntryAny(value)}}

impl<Token:Tk> ASTNode<Token> {
  pub fn token (&self) -> Token {
    match self {
      ASTNode::JSON(n) => {n.tok.clone()}
      ASTNode::Array(n) => {n.tok.clone()}
      ASTNode::Object(n) => {n.tok.clone()}
      ASTNode::Token(tok) => tok.clone(),_ => Default::default()
    }
  }
}

/*impl<Token:Tk> std::hash::Hash for ASTNode<Token> {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H){match self{
      ASTNode::JSON(n) => n.hash(hasher),
      ASTNode::Null(n) => n.hash(hasher),
      ASTNode::Array(n) => n.hash(hasher),
      ASTNode::Object(n) => n.hash(hasher),
      _=>{}
    }
  }
}*/

#[derive( Clone, Debug, Default )]
pub struct JSON<Token:Tk>{pub tok: Token,pub body: EntryAny<Token>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_JSON(self) -> Option<Box<JSON<Token>>> {match self {ASTNode::JSON(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<JSON<Token>>> for ASTNode<Token>{fn from(value: Box<JSON<Token>>) -> Self {Self::JSON(value)}}

#[derive( Clone, Debug, Default )]
pub struct Array<Token:Tk>{pub tok: Token,pub values: Vec<ValAny<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Array(self) -> Option<Box<Array<Token>>> {match self {ASTNode::Array(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Array<Token>>> for ASTNode<Token>{fn from(value: Box<Array<Token>>) -> Self {Self::Array(value)}}

#[derive( Clone, Debug, Default )]
pub struct Object<Token:Tk>{pub tok: Token,pub values: HashMap<String,ValAny<Token>>,}

impl<Token:Tk> ASTNode<Token>{
  pub fn into_Object(self) -> Option<Box<Object<Token>>> {match self {ASTNode::Object(val) => Some(val),_ => None,}}
}

impl<Token:Tk> From<Box<Object<Token>>> for ASTNode<Token>{fn from(value: Box<Object<Token>>) -> Self {Self::Object(value)}}
  

fn rule_0<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let body = std::mem::take(&mut nodes[0]);
  let body = unsafe{ body.into_EntryAny().unwrap_unchecked() };
  
  ASTNode::JSON(Box::new(JSON{tok,body,}))
}

fn rule_1<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Object().unwrap_unchecked() };
  out.into()
}

fn rule_2<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Array().unwrap_unchecked() };
  out.into()
}

fn rule_3<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let values = std::mem::take(&mut nodes[1]);
  let values = unsafe{ values.into_StringValAny().unwrap_unchecked() };
  
  ASTNode::Object(Box::new(Object{tok,values,}))
}

fn rule_4<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_StringValAny().unwrap_unchecked() };
  out.into()
}

fn rule_5<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_StringValAny().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_StringValAny().unwrap_unchecked() };
  let mut out = out_l;
  out.extend(out_r);
  out.into()
}

fn rule_6<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  
  let tok = nterm_tok.clone();
  
  let values = std::mem::take(&mut nodes[1]);
  let values = unsafe{ values.into_ValAnys().unwrap_unchecked() };
  
  ASTNode::Array(Box::new(Array{tok,values,}))
}

fn rule_7<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_0 = std::mem::take(&mut nodes[0]);
  let out_0 = unsafe{ out_0.into_ValAny().unwrap_unchecked() };
  
  let out = vec![ out_0 ];
  out.into()
}

fn rule_8<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_r = std::mem::take(&mut nodes[2]);
  let out_r = unsafe{ out_r.into_ValAny().unwrap_unchecked() };
  let out_l = std::mem::take(&mut nodes[0]);
  let out_l = unsafe{ out_l.into_ValAnys().unwrap_unchecked() };
  let mut out = out_l;
  out.push(out_r);
  out.into()
}

fn rule_9<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out_key = std::mem::take(&mut nodes[0]);
  let out_key = unsafe{ out_key.into_String().unwrap_unchecked() };
  let out_val = std::mem::take(&mut nodes[2]);
  let out_val = unsafe{ out_val.into_ValAny().unwrap_unchecked() };
  let mut out = HashMap::new();
  out.insert(out_key,out_val);
  out.into()
}

fn rule_10<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nterm_tok.clone();
  let out = out.trim(1,1);
  let out = out.to_string();
  out.into()
}

fn rule_11<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = tokens[0].clone();
  let out: f64 = out.to_string().parse().unwrap_or_default();
  out.into()
}

fn rule_12<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Object().unwrap_unchecked() };
  out.into()
}

fn rule_13<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = std::mem::take(&mut nodes[0]);
  let out = unsafe{ out.into_Array().unwrap_unchecked() };
  out.into()
}

fn rule_14<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = tokens[0].clone();let out = out.len()>0;out.into()}

fn rule_15<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {let nodes=unsafe{&mut*nodes};let out = false;out.into()}

fn rule_16<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {ASTNode::Null}

fn rule_17<Token:Tk>( nodes: *mut [ASTNode<Token>], tokens:&[Token], nterm_tok: Token) -> ASTNode<Token> {
  let nodes=unsafe{&mut*nodes};
  let out = nterm_tok.clone();
  let out = out.trim(1,1);
  let out = out.to_string();
  out.into()
}

pub struct ReduceRules<Token:Tk>(pub [Reducer<Token,ASTNode<Token>>;18]);

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
      ]
    )
  }
}

impl<Token:Tk> AsRef<[Reducer<Token, ASTNode<Token>>]> for ReduceRules<Token>{fn as_ref(&self) -> &[Reducer<Token, ASTNode<Token>>]{&self.0}}