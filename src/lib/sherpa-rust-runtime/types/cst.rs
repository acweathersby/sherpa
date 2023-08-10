use std::rc::Rc;

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum CST {
  Terminal {
    leading_skipped: Vec<Skipped>,
    byte_len:        u32,
    token_id:        u32,
  },
  NonTerm {
    prod_id:  Vec<(u16, u16)>,
    children: Vec<(u32, Rc<CST>)>,
  },
}

#[derive(Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Skipped {
  pub byte_len: u32,
  pub token_id: u32,
}
