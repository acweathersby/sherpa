#![allow(unused)]

use radlr_rust_runtime::parsers::ast::{Tk, Reducer, Node};


/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.0-beta2
/// - **SOURCE**: /home/work/projects/lib_radlr/src/grammar/v2_0_0/grammar.sg
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

#[derive(Clone, Debug, Default)]
#[repr(C,u32)]
pub enum AstNode<Token:Tk> {
  u64(u64),
  i64(i64),
  f64(f64),
  String(String),
  StringVec(Vec<String>),
  Nodes(Vec<AstNode<Token>>),
  Token(Token),
  #[default]
  None,
  Test(Box<Test<Token>>)
}

impl<Token:Tk> AstNode<Token> {
  pub fn token (&self) -> Token {match self {AstNode::Test(n) => {n.tok.clone()}_ => Default::default()}}
}
impl<Token:Tk> std::hash::Hash for AstNode<Token> {
  fn hash<H: std::hash::Hasher>(&self, hasher: &mut H){match self{
      AstNode::Test(n) => n.hash(hasher),
      AstNode::f64(val) => val.to_le_bytes().hash(hasher),
      AstNode::i64(val) => val.hash(hasher),
      AstNode::u64(val) => val.hash(hasher),
      _=>{}
    }
  }
}
#[derive(Clone, Debug, Hash)]
struct Test<Token:Tk>{t : String,tok: Token}
  

fn rule_0<Token:Tk>( nodes: *mut [AstNode<Token>], tokens:&[Token], nterm_tok: Token) -> AstNode<Token> {
  let nodes=unsafe{&mut*nodes};
  /* StrNode */
  /* :: tok*/
  let t = tokens[1].clone();
  let t = t.to_string();
  
  AstNode::Test(Box::new(Test{t,tok: nterm_tok}))
}


pub struct ReduceRules<Token:Tk>(
  pub [Reducer<Token, AstNode<Token>>;1]
);

impl<Token:Tk> ReduceRules<Token>{
  pub const fn new () -> Self {
    Self([ rule_0, ])
  }
}

impl<Token:Tk> AsRef<[Reducer<Token, AstNode<Token>>]> for ReduceRules<Token> {
  fn as_ref(&self) -> &[Reducer<Token, AstNode<Token>>] {
    &self.0
  }
}
