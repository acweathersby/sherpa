use std::rc::Rc;

use super::TokenRange;

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum CST {
  Terminal { leading_skipped: Vec<Skipped>, byte_len: u32, token_id: u32 },
  NonTerm { nterm: Vec<(u16, u16)>, children: Vec<(u32, Rc<CST>)> },
}

#[derive(Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Skipped {
  pub byte_len: u32,
  pub token_id: u32,
}

#[derive(Clone, Copy)]
pub enum CSTNode {
  Terminal { skipped: bool, id: u16, tok_range: TokenRange },
  NonTerminal { id: u16, rule: u16, symbols: u32, claim_skipped: bool },
}
