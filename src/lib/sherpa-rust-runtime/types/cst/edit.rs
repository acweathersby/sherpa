use super::super::*;
use std::{mem::ManuallyDrop, rc::Rc};

#[derive(Default, Debug)]
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
#[derive(Default, Debug)]
pub struct EditNode {
  parent:    Option<ManuallyDrop<EditNodeRef>>,
  children:  Option<Vec<EditNodeRef>>,
  _ref: isize,
  node:      NodeRef,
  index:     usize,
}

#[derive(Debug)]
pub struct EditNodeRef {
  internal: *mut EditNode,
}

impl AsRef<EditNode> for EditNodeRef {
  fn as_ref(&self) -> &EditNode {
    unsafe { &*self.internal }
  }
}

impl AsMut<EditNode> for EditNodeRef {
  fn as_mut(&mut self) -> &mut EditNode {
    unsafe { &mut *self.internal }
  }
}

impl Drop for EditNodeRef {
  fn drop(&mut self) {
    //println!("!--- Dropping {:X} {:X}", self as *const _ as usize, self.internal
    // as usize);
    unsafe {
      self.as_mut().decr_count();
      if let Some(par) = &mut self.as_mut().parent {
        par.as_mut().decr_count();
      }
      self.try_drop_internal();
    }
  }
}

impl Clone for EditNodeRef {
  fn clone(&self) -> Self {
    let mut other = Self { internal: self.internal };
    other.as_mut().incr_count();
    other
  }
}

impl EditNodeRef {
  unsafe fn try_drop_internal(&self) {
    let node = &mut *self.internal;
    if node._ref <= 0 {
      let _ = Box::from_raw(self.internal);
    } else if node._ref < 0 {
      panic!("Invalid reference count");
    }
  }

  pub fn children(&self) -> Vec<EditNodeRef> {
    unsafe {
      let internal = &mut *self.internal;
      if let Some(children) = &internal.children {
        internal._ref += children.len() as isize;
        children.iter().map(|c| c.clone()).collect()
      } else if let Some(non_term) = self.as_nonterm() {
        let children = non_term
          .symbols
          .iter()
          .enumerate()
          .map(|(index, i)| {
            let mut node = EditNode::boxed(i.clone());
            node.as_mut().index = index;
            node.as_mut().parent = Some(ManuallyDrop::new(Self { internal: self.internal }));
            node
          })
          .collect::<Vec<_>>();
        internal.children = Some(children);
        self.children()
      } else {
        vec![]
      }
    }
  }

  pub fn to_node(&self) -> Option<Rc<CSTNode>> {
    unsafe {
      let internal = &mut *self.internal;
      match &internal.node {
        NodeRef::Clean(node) => Some(node.clone()),
        NodeRef::Dirty(node) => {
          let mut node = *node.clone();
          if let Some(children) = &internal.children {
            if let CSTNode::NonTerm(n) = &mut node {
              let children = children.iter().filter_map(|c| c.to_node()).collect::<Vec<_>>();
              let len = children.iter().fold(0, |v, n| v + n.len());
              n.length = len as u32;
              n.symbols = children;
            }
          }
          Some(Rc::new(node))
        }
        NodeRef::None => None,
      }
    }
  }

  pub fn replace(&mut self, nodes: impl IntoIterator<Item = CSTNode>, store: &CSTStore) {
    unsafe {
      let internal = &mut *self.internal;
      let index = internal.index;
      let mut par = internal.parent.clone();
      while let Some(mut parent) = par {
        parent.as_mut().make_dirty();
        par = parent.as_ref().parent.clone();
      }

      if let Some(parent) = internal.parent.as_mut() {
        let children = parent.as_mut().children.as_mut().unwrap_unchecked();

        let syms = children.drain(index..).skip(1).collect::<Vec<_>>();

        children.extend(nodes.into_iter().map(|n| EditNode::boxed(store.get_unique(n))));

        children.extend(syms);

        children.iter_mut().enumerate().for_each(|(index, c)| c.as_mut().index = index);

        self.as_mut().decr_count();
        self.try_drop_internal();

        self.internal = children[index].internal;

        self.as_mut().incr_count();
      } else if let Some(node) = nodes.into_iter().next() {
        self.as_mut().decr_count();
        self.try_drop_internal();

        self.internal = Box::into_raw(Box::new(EditNode::new(Rc::new(node))));

        //println!("--- Creating {:X}", self.internal as *const _ as usize);

        self.as_mut().incr_count();
      }
    }
  }

  pub fn node(&self) -> Option<&CSTNode> {
    match &self.as_ref().node {
      NodeRef::Clean(node) => Some(node),
      NodeRef::Dirty(node) => Some(node),
      NodeRef::None => None,
    }
  }

  pub fn best(&mut self, store: &CSTStore) -> &mut Self {
    for mut child in self.children() {
      child.best(store);
    }

    if let Some(alts) = self.as_alts() {
      let first = alts.alternatives.first().unwrap().symbols.iter().map(|i| (**i).clone()).next().unwrap();
      self.replace([first], store)
    }

    self
  }
}

impl NodeTraits for EditNodeRef {
  fn len(&self) -> usize {
    self.node().map(|n| n.len()).unwrap_or_default()
  }

  fn entropy(&self) -> isize {
    self.node().map(|n| n.entropy()).unwrap_or_default()
  }

  fn ty(&self) -> NodeType {
    self.node().map(|n| n.ty()).unwrap_or(NodeType::None)
  }

  fn as_alt(&self) -> Option<&Alternative> {
    self.node().and_then(|n| n.as_alt())
  }

  fn as_alts(&self) -> Option<&Alts> {
    self.node().and_then(|n| n.as_alts())
  }

  fn as_token(&self) -> Option<&TokenNode> {
    self.node().and_then(|n| n.as_token())
  }

  fn as_nonterm(&self) -> Option<&NonTermNode> {
    self.node().and_then(|n| n.as_nonterm())
  }
}

impl EditNode {
  fn new<'par>(node: Rc<CSTNode>) -> Self {
    Self {
      parent:    None,
      children:  None,
      node:      NodeRef::Clean(node),
      index:     0,
      _ref: 1,
    }
  }

  pub fn boxed<'par>(node: Rc<CSTNode>) -> EditNodeRef {
    let node = Box::into_raw(Box::new(Self::new(node)));
    //println!("--- Creating {:X}", node as *const _ as usize);
    EditNodeRef { internal: node }
  }

  fn incr_count(&mut self) {
    self._ref += 1;
  }

  fn decr_count(&mut self) {
    self._ref -= 1;
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
