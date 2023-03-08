use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum CST {
  Terminal { leading_skipped: Vec<Skipped>, byte_len: u32, token_id: u32 },
  NonTerm { prod_id: Vec<(u16, u16)>, children: Vec<(u32, Rc<CST>)> },
}

#[derive(Debug, Clone, Copy)]
pub struct Skipped {
  pub byte_len: u32,
  pub token_id: u32,
}


