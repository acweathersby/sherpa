use std::{marker::PhantomData, rc::Rc};

use crate::types::{CSTNode, CSTStore, EditNode, EntryPoint, NodeTraits, ParserError, Printer};

use super::{
  super::types::{ParserInput, ParserProducer},
  error_recovery::parse_with_recovery,
};

#[derive(Clone, Debug)]
pub enum CST {
  Terminal { leading_skipped: Vec<Skipped>, byte_len: u32, token_id: u32 },
  NonTerm { nterm: Vec<(u16, u16)>, children: Vec<(u32, Rc<CST>)> },
}

#[derive(Clone, Copy, Debug)]
pub struct Skipped {
  pub byte_len: u32,
  pub token_id: u32,
}

#[derive(Debug)]
pub struct InsertResult {
  /// The indices of nodes that are replaced. If this is empty, then the
  /// root node was replaced.
  pub path:      Vec<usize>,
  /// Nodes that replaced the existing nodes at index.
  pub new_nodes: Vec<Rc<CSTNode>>,
}

pub struct EditGraph<I: ParserInput, D: ParserProducer<I>> {
  root_node: Option<Rc<CSTNode>>,
  store:     CSTStore,
  db:        Rc<D>,
  _in:       PhantomData<I>,
}

impl<I: ParserInput, D: ParserProducer<I>> std::fmt::Debug for EditGraph<I, D> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("EditGraph");
    s.field("graph", &self.root_node);
    s.field("store", &self.store);
    s.finish()
  }
}

impl<'str_input, I: ParserInput + From<String>, D: ParserProducer<I>> EditGraph<I, D> {
  pub fn db(&self) -> &D {
    self.db.as_ref()
  }

  /// Initialize the graph with a base input string.
  pub fn parse(entry: EntryPoint, input: String, db: Rc<D>) -> Result<Self, ParserError> {
    let mut input = I::from(input);
    let store: CSTStore = CSTStore::default();
    match parse_with_recovery(&mut input, entry, db.as_ref(), &store) {
      Ok(mut candidates) => {
        if let Some((_, node)) =
          (candidates.len() >= 1).then(|| candidates.remove(0)).and_then(|mut s| s.symbols.drain(..).next())
        {
          let mut edit = EditNode::boxed(node);
          edit.best(&store);

          Ok(Self {
            root_node: Some(edit.to_node().unwrap()),
            db,
            _in: Default::default(),
            store,
          })
        } else {
          Ok(Self { root_node: None, db, _in: Default::default(), store })
        }
      }
      Err(_) => Ok(Self { root_node: None, db, _in: Default::default(), store }),
    }
  }

  /// Returns the path to a terminal node located at a given offset
  pub fn get_offset_path(&self, node: &Rc<CSTNode>, mut offset: usize) -> Option<(Vec<u32>, u32)> {
    let mut path = vec![0];
    let mut node = node.clone();

    loop {
      if let Some(nt_node) = node.as_nonterm() {
        for (index, c_node) in nt_node.symbols.iter().enumerate() {
          if offset > c_node.len() {
            offset -= c_node.len();
          } else {
            node = c_node.clone();
            path.push(index as u32);
            break;
          }
        }
      } else if let Some(_) = node.as_token() {
        return Some((path, offset as u32));
      } else {
        return None;
      }
    }
  }

  /// Inserts a string into the given node's text representation and re-parses
  /// the resulting modified text to form a new node of the same type.
  ///
  /// Several conditions must be met for a successful parse:
  /// - The node MUST be a NonTerminal.
  /// - The offset MUST  be <= to the length of node.
  /// - The parser is able to auto correct any errors encountered in the
  ///   modified text representation
  ///
  /// `None' is returned if one these conditions fails.
  pub fn patch_insert(&self, node: &CSTNode, offset: usize, input: &str) -> Option<Vec<Rc<CSTNode>>> {
    let Some(non_term) = node.as_nonterm() else { return None };

    let Self { db, .. } = self;

    let nonterm_id = non_term.id as u32;

    let entry_point = EntryPoint { nonterm_id };

    let mut input_string = Printer::new(node, false, db.as_ref()).to_string();

    if offset > input_string.len() {
      return None;
    };

    input_string.insert_str(offset, input);

    let str_len = input_string.len();

    let mut input = I::from(input_string);

    let store: CSTStore = CSTStore::default();

    match parse_with_recovery(&mut input, entry_point, db.as_ref(), &store) {
      Ok(mut candidates) => {
        if let Some(ctx) = (candidates.len() >= 1).then(|| candidates.remove(0)) {
          if ctx.ctx.sym_ptr >= str_len {
            let nodes = ctx
              .nodes()
              .map(|n| {
                let mut node = EditNode::boxed(n.clone());
                node.best(&store).to_node().unwrap()
              })
              .collect::<Vec<_>>();

            Some(nodes)
          } else {
            None
          }
        } else {
          None
        }
      }
      _ => None,
    }
  }

  /// Removes a portion of a the given node's text representation and re-parses
  /// the resulting modified text to form a new node of the same type.
  ///
  /// Several conditions must be met for a successful parse:
  /// - The node MUST be a NonTerminal.
  /// - The offset + length MUST  be <= to the length of node.
  /// - The parser is able to auto correct any errors encountered in the
  ///   modified text representation
  ///
  /// `None' is returned if one these conditions fails.
  pub fn patch_remove(&self, node: &CSTNode, offset: usize, length: usize) -> Option<Vec<Rc<CSTNode>>> {
    let Some(non_term) = node.as_nonterm() else { return None };

    let Self { db, .. } = self;

    let nonterm_id = non_term.id as u32;

    let entry_point = EntryPoint { nonterm_id };

    let input_string = Printer::new(node, false, db.as_ref()).to_string();

    if offset + length > input_string.len() {
      return None;
    };

    let (a, b) = input_string.split_at(offset);
    let (_, c) = b.split_at(length);

    let input_string = a.to_string() + c;

    let str_len = input_string.len();

    let mut input = I::from(input_string);

    let store: CSTStore = CSTStore::default();

    match parse_with_recovery(&mut input, entry_point, db.as_ref(), &store) {
      Ok(mut candidates) => {
        if let Some(ctx) = (candidates.len() >= 1).then(|| candidates.remove(0)) {
          if ctx.ctx.sym_ptr >= str_len {
            let nodes = ctx
              .nodes()
              .map(|n| {
                let mut node = EditNode::boxed(n.clone());
                node.best(&store).to_node().unwrap()
              })
              .collect::<Vec<_>>();

            Some(nodes)
          } else {
            None
          }
        } else {
          None
        }
      }
      _ => None,
    }
  }

  pub fn cst(&self) -> Option<Rc<CSTNode>> {
    self.root_node.as_ref().map(|s| s.clone())
  }
}
