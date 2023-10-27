use super::super::*;
use std::{cell::Cell, default, rc::Rc};

#[derive(Default)]
enum NodeRef {
  Clean(Rc<CSTNode>),
  Dirty(Box<CSTNode>),
  #[default]
  None,
}

impl NodeRef {
  pub fn as_ref(&self) -> Option<&CSTNode> {
    match self {
      NodeRef::Clean(node) => Some(node.as_ref()),
      NodeRef::Dirty(node) => Some(node.as_ref()),
      NodeRef::None => None,
    }
  }
}

pub struct EditNode {
  parent:    Option<Cell<*mut ()>>,
  node:      NodeRef,
  index:     usize,
  ref_count: Cell<usize>,
}

pub struct AltNode {
  parent: Option<Cell<*mut ()>>,
  node:   Rc<Alternative>,
}

impl EditNode {
  pub fn new(node: Rc<CSTNode>) -> Self {
    Self {
      parent:    None,
      node:      NodeRef::Clean(node),
      index:     0,
      ref_count: Cell::new(1),
    }
  }

  pub fn replace(&mut self, new_node: CSTNode) {
    self.node = NodeRef::Dirty(Box::new(new_node));
  }

  pub fn children(&self) -> Vec<EditNode> {
    let par = (self as *const _ as *mut ());
    match self.node.as_ref() {
      Some(CSTNode::NonTerm(node)) => node
        .symbols
        .iter()
        .enumerate()
        .map(|(index, node)| EditNode {
          parent: Some(Cell::new(par)),
          node: NodeRef::Clean(node.clone()),
          index,
          ref_count: Cell::new(1),
        })
        .collect(),
      _ => vec![],
    }
  }

  pub fn alts(&self) -> Vec<AltNode> {
    match self.node.as_ref() {
      Some(CSTNode::Alts(node)) => node
        .alternatives
        .iter()
        .enumerate()
        .map(|(index, node)| AltNode { node: node.clone(), parent: self.parent.clone() })
        .collect(),
      _ => vec![],
    }
  }

  pub fn best(&mut self) {
    for mut child in self.children() {
      child.best()
    }

    let mut alts = self.alts();

    for index in 0..alts.len().min(1) {
      let alt = &mut alts[index];
      self.replace(alt.node.symbols.first().unwrap().as_ref().clone());
      self.best()
    }
  }

  pub fn into_node(mut self) -> Option<Rc<CSTNode>> {
    match std::mem::take(&mut self.node) {
      NodeRef::Clean(node) => Some(node.clone()),
      NodeRef::Dirty(node) => Some(unsafe { Rc::from_raw(Box::into_raw(node)) }),
      NodeRef::None => None,
    }
  }

  fn make_dirty(&mut self) -> &mut NodeRef {
    match &self.node {
      NodeRef::Clean(node) => {
        let node = node.as_ref().clone();
        self.node = NodeRef::Dirty(Box::new(node));
      }
      _ => {}
    }
    &mut self.node
  }
}

impl Drop for EditNode {
  fn drop(&mut self) {
    if let Some(parent) = &self.parent {
      let par = unsafe { &mut *(*parent.as_ptr() as *mut EditNode) };
      match std::mem::take(&mut self.node) {
        NodeRef::Clean(_) => {}
        NodeRef::Dirty(child_node) => match par.make_dirty() {
          NodeRef::Dirty(node) => match node.as_mut() {
            CSTNode::NonTerm(node) => node.symbols[self.index] = unsafe { Rc::from_raw(Box::into_raw(child_node)) },
            _ => {}
          },
          _ => {}
        },
        NodeRef::None => match par.make_dirty() {
          NodeRef::Dirty(node) => match node.as_mut() {
            CSTNode::NonTerm(node) => {
              node.symbols.remove(self.index);
            }
            _ => {}
          },
          _ => {}
        },
      }
    }
  }
}
