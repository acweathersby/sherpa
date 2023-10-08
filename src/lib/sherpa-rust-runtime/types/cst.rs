use super::*;
use std::{
  collections::{hash_map::DefaultHasher, HashMap, HashSet},
  fmt::Debug,
  hash::{Hash, Hasher},
  rc::Rc,
};

/// A hash of the node that only takes into account non-spacial values.
pub trait CSTHashes: Hash {
  fn canonical_hash<H: std::hash::Hasher>(&self, state: &mut H);
  fn dedup_hash<H: std::hash::Hasher>(&self, state: &mut H);
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum CSTErrorNodeType {
  MissingToken(u16),
  ErroneousInput,
}

#[derive(Clone, Hash)]
pub enum CSTNode {
  Skipped(Rc<TokenNode>),
  Token(ParserState, Rc<TokenNode>),
  MissingToken(u32, ParserState, Rc<TokenNode>),
  Errata(Rc<TokenNode>),
  NonTerm(Rc<NonTermNode>),
  Multi(Rc<Multi>),
}

impl CSTHashes for CSTNode {
  fn canonical_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    use CSTNode::*;
    match self {
      Errata(..) | Skipped(..) => {}
      Token(_, tok) | MissingToken(.., tok) => tok.canonical_hash(state),
      NonTerm(nt) => nt.canonical_hash(state),
      Multi(m) => m.canonical_hash(state),
    }
  }

  fn dedup_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    use CSTNode::*;
    match self {
      Errata(tok) | Skipped(tok) | Token(_, tok) | MissingToken(.., tok) => tok.dedup_hash(state),
      NonTerm(nt) => nt.dedup_hash(state),
      Multi(m) => m.dedup_hash(state),
    }
  }
}

impl CSTNode {
  pub fn offset(&self) -> u32 {
    use CSTNode::*;
    match self {
      Errata(tok) | Skipped(tok) | Token(_, tok) | MissingToken(.., tok) => tok.offset,
      NonTerm(nt) => nt.offset,
      Multi(m) => m.offset,
    }
  }

  pub fn length(&self) -> u32 {
    use CSTNode::*;
    match self {
      Errata(tok) | Skipped(tok) | Token(_, tok) | MissingToken(.., tok) => tok.val.len() as u32,
      NonTerm(nt) => nt.length,
      Multi(m) => m.length,
    }
  }
}

impl Debug for CSTNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Skipped(node) => {
        f.write_str("[skipped] ")?;
        node.fmt(f)
      }
      Self::MissingToken(.., node) => {
        f.write_str("[missing] ")?;
        node.fmt(f)
      }
      Self::Errata(node) => {
        f.write_str("[errata] ")?;
        node.fmt(f)
      }
      Self::Token(_, node) => node.fmt(f),
      Self::Multi(multi) => multi.fmt(f),
      Self::NonTerm(nt) => nt.fmt(f),
    }
  }
}

#[derive(Clone, Hash)]
pub struct NonTermNode {
  pub id:      u16,
  pub rule:    u16,
  pub offset:  u32,
  pub length:  u32,
  pub symbols: Vec<CSTNode>,
}

impl CSTHashes for NonTermNode {
  fn canonical_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.id.hash(state);
  }

  fn dedup_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.id.hash(state);
    self.symbols.iter().for_each(|s| s.dedup_hash(state))
  }
}

impl Debug for NonTermNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut hasher = DefaultHasher::new();
    self.canonical_hash(&mut hasher);

    let mut dedup = DefaultHasher::new();
    self.dedup_hash(&mut dedup);

    f.write_fmt(format_args!(
      "[nonterm: {} @ {} : {} : {}]",
      self.id,
      self as *const _ as usize,
      hasher.finish(),
      dedup.finish()
    ))?;
    let mut list = f.debug_list();
    for obj in &self.symbols {
      list.entry(obj);
    }
    list.finish()
  }
}

impl NonTermNode {
  pub fn new(id: u16, rule: u16, symbols: Vec<CSTNode>, offset: u32, length: u32) -> Rc<Self> {
    Rc::new(Self { id, rule, symbols, offset, length })
  }

  pub fn typed(id: u16, rule: u16, symbols: Vec<CSTNode>, offset: u32, length: u32) -> CSTNode {
    CSTNode::NonTerm(Self::new(id, rule, symbols, offset, length))
  }
}

#[derive(Clone, Hash)]
pub struct TokenNode {
  pub tok_id: u32,
  pub offset: u32,
  pub val:    String,
}

impl TokenNode {
  pub fn new(tok_id: u32, length: u32, offset: u32, val: String) -> Rc<Self> {
    Rc::new(Self { tok_id, offset, val })
  }

  pub fn skipped_type(tok_id: u32, length: u32, offset: u32, val: String) -> CSTNode {
    CSTNode::Skipped(Self::new(tok_id, length, offset, val))
  }

  pub fn token_type(state: ParserState, tok_id: u32, length: u32, offset: u32, val: String) -> CSTNode {
    CSTNode::Token(state, Self::new(tok_id, length, offset, val))
  }

  pub fn missing_type(state: ParserState, tok_id: u32, offset: u32, val: String, entropy: u32) -> CSTNode {
    CSTNode::MissingToken(entropy, state, Self::new(tok_id, 0, offset, val))
  }

  pub fn error_type(tok_id: u32, offset: u32, val: String) -> CSTNode {
    CSTNode::Errata(Self::new(tok_id, 0, offset, val))
  }

  pub fn length(&self) -> u32 {
    self.val.len() as u32
  }
}

impl CSTHashes for TokenNode {
  fn canonical_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.tok_id.hash(state);
  }

  fn dedup_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.tok_id.hash(state);
    self.val.hash(state);
  }
}

impl Debug for TokenNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("( {} )", self.val))
  }
}

#[derive(Clone, Hash)]
pub struct Multi {
  pub length:       u32,
  pub offset:       u32,
  pub alternatives: Vec<Alternative>,
  #[cfg(debug_assertions)]
  pub meta_label:   &'static str,
}

impl Multi {
  pub fn new(alternatives: Vec<Alternative>, meta_label: &'static str) -> Rc<Self> {
    let length = alternatives[0].length;
    let offset = alternatives[0].offset;

    #[cfg(debug_assertions)]
    {
      Rc::new(Self { alternatives, length, offset, meta_label })
    }
    #[cfg(not(debug_assertions))]
    Rc::new(Self { alternatives, length, offset })
  }

  pub fn typed(alternatives: Vec<Alternative>, meta_label: &'static str) -> CSTNode {
    CSTNode::Multi(Self::new(alternatives, meta_label))
  }
}

impl CSTHashes for Multi {
  fn canonical_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.alternatives[0].canonical_hash(state)
  }

  fn dedup_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.alternatives.iter().for_each(|s| s.dedup_hash(state))
  }
}

impl Debug for Multi {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    #[cfg(debug_assertions)]
    f.write_fmt(format_args!("[multi: {}]", self.meta_label))?;
    #[cfg(not(debug_assertions))]
    f.write_fmt(format_args!("[multi]"))?;
    let mut list = f.debug_list();
    for obj in &self.alternatives {
      list.entry(obj);
    }
    list.finish()
  }
}

#[derive(Clone, Hash)]
pub struct Alternative {
  pub length:  u32,
  pub offset:  u32,
  pub entropy: isize,
  pub symbols: Vec<CSTNode>,
}

impl CSTHashes for Alternative {
  fn canonical_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.symbols.iter().for_each(|s| s.canonical_hash(state))
  }

  fn dedup_hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.symbols.iter().for_each(|s| s.dedup_hash(state))
  }
}

impl Debug for Alternative {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut hasher = DefaultHasher::new();
    self.symbols.iter().for_each(|s| s.dedup_hash(&mut hasher));
    f.write_fmt(format_args!("[alternative [{}] : {}]", self.entropy, hasher.finish()))?;
    let mut list = f.debug_list();
    for obj in &self.symbols {
      list.entry(obj);
    }
    list.finish()
  }
}

pub struct Printer<'node, 'db> {
  node: &'node CSTNode,
  db:   &'db dyn RuntimeDatabase,
}

impl<'node, 'db> Printer<'node, 'db> {
  pub fn new<'a, 'b>(node: &'a CSTNode, db: &'b dyn RuntimeDatabase) -> Printer<'a, 'b> {
    Printer { node, db }
  }

  pub fn new_node(&self, node: &'node CSTNode) -> Self {
    Self { node, db: self.db }
  }

  pub fn print(&self) {
    use CSTNode::*;
    match self.node {
      Skipped(skipped) => {
        //print!("{}", skipped.val);
      }
      Token(state, token) => {
        print!("{}", token.val);
      }
      MissingToken(.., missing) => {
        print!("{}", missing.val);
      }
      NonTerm(non_term) => {
        for node in &non_term.symbols {
          self.new_node(node).print();
        }
      }
      Multi(multi) => {
        if let Some(first) = multi.alternatives.first() {
          for node in &first.symbols {
            self.new_node(node).print();
          }
        }
      }
      Errata { .. } => {}
    }
  }

  pub fn print_all(&self) {
    println!("{}", self._print_all().join("\n"));
  }

  fn _print_all(&self) -> Vec<String> {
    use CSTNode::*;
    match self.node {
      Skipped(skipped) => {
        vec![Default::default()]
      }
      Token(state, token) => {
        vec![token.val.clone()]
      }
      MissingToken(.., missing) => {
        vec![missing.val.clone()]
      }
      NonTerm(non_term) => {
        let mut vals: Vec<String> = vec![];
        for node in &non_term.symbols {
          vals = self
            .new_node(node)
            ._print_all()
            .into_iter()
            .flat_map(|v| {
              if vals.len() > 0 {
                vals.iter().map(|a| -> String { a.clone() + &v }).collect::<Vec<_>>()
              } else {
                vec![v]
              }
            })
            .collect();
        }
        vals
      }
      Multi(multi) => {
        let mut out_vals = vec![];
        for alt in &multi.alternatives {
          let mut vals: Vec<String> = vec![];
          for node in &alt.symbols {
            vals = self
              .new_node(node)
              ._print_all()
              .into_iter()
              .flat_map(|v| {
                if vals.len() > 0 {
                  vals.iter().map(|a| -> String { a.clone() + &v }).collect::<Vec<_>>()
                } else {
                  vec![v]
                }
              })
              .collect();
          }
          out_vals.extend(vals);
        }
        out_vals
      }
      Errata { .. } => {
        vec![Default::default()]
      }
    }
  }
}

pub trait CSTtoASTProducer<I: ParserInput, ASTNode>: ASTProducer<I, ASTNode> {
  fn cst_to_ast(&self, node: &CSTNode) -> Option<ASTBaseNode<ASTNode>> {
    use CSTNode::*;
    match node {
      Skipped(..) | Errata { .. } => None,
      Token(.., token) | MissingToken(.., token) => Some(ASTBaseNode::Token(token.clone())),
      NonTerm(non_term) => {
        let reduce_fn = self.get_reduce_functions()[non_term.rule as usize];

        let mut symbols = non_term.symbols.iter().filter_map(|n| self.cst_to_ast(n));

        Some(reduce_fn(&mut symbols))
      }
      Multi(multi) => {
        if let Some(first) = multi.alternatives.first() {
          first.symbols.iter().filter_map(|n| self.cst_to_ast(n)).next()
        } else {
          None
        }
      }
    }
  }
}
