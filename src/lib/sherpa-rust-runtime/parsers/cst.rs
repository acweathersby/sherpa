use std::{collections::VecDeque, marker::PhantomData, ops::Deref, rc::Rc};

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

pub struct EditGraph<I: ParserInput, D: ParserProducer<I>> {
  graph: Option<Rc<CSTNode>>,
  store: CSTStore,
  db:    Rc<D>,
  _in:   PhantomData<I>,
}

impl<I: ParserInput, D: ParserProducer<I>> std::fmt::Debug for EditGraph<I, D> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("EditGraph");
    s.field("graph", &self.graph);
    s.field("store", &self.store);
    s.finish()
  }
}

impl<'str_input, I: ParserInput + From<String>, D: ParserProducer<I>> EditGraph<I, D> {
  /// Initialize the graph with a base input string.
  pub fn parse(entry: EntryPoint, input: String, db: Rc<D>) -> Result<Self, ParserError> {
    let mut input = I::from(input);
    let store: CSTStore = CSTStore::default();
    match parse_with_recovery(&mut input, entry, db.as_ref(), &store) {
      Ok(mut candidates) => {
        if let Some((_, node)) =
          (candidates.len() >= 1).then(|| candidates.remove(0)).and_then(|mut s| s.symbols.drain(..).next())
        {
          dbg!(node.clone());
          let mut edit = EditNode::boxed(node);
          edit.best(&store);

          Ok(Self {
            graph: Some(edit.to_node().unwrap()),
            db,
            _in: Default::default(),
            store,
          })
        } else {
          Ok(Self { graph: None, db, _in: Default::default(), store })
        }
      }
      Err(_) => Ok(Self { graph: None, db, _in: Default::default(), store }),
    }
  }

  pub fn insert(&mut self, offset: usize, input: &'str_input str) -> Result<usize, ParserError> {
    // Find the node that represents the closest approximation of the area that
    // is directly effected by change set.

    let Self { db, graph: graph_main, store, .. } = self;

    let mut reduce_mode = false;

    if let Some(graph) = graph_main.as_mut() {
      let edit = EditNode::boxed(graph.clone());
      let mut queue = VecDeque::from_iter(vec![(offset, edit)]);

      // build up our graph
      while let Some((offset, mut node)) = queue.pop_back() {
        if let Some(non_term) = node.as_nonterm() {
          if !reduce_mode {
            let mut peek_offset = offset;

            // see if a better non-term candidate exists in this nodes children.
            let children = node.children();
            let len = children.len();

            for (index, child) in children.into_iter().enumerate() {
              if child.is_nonterm() {
                if peek_offset < child.len() {
                  // Repeat
                  queue.push_back((offset, node));
                  queue.push_back((peek_offset, child));
                  break;
                }
              }

              if index == len - 1 || peek_offset < child.len() {
                queue.push_back((offset, node));
                reduce_mode = true;
                break;
              }

              peek_offset -= child.len();
            }
          } else {
            let nonterm_id = non_term.id as u32;

            let entry_point = EntryPoint { nonterm_id };

            let mut input_string = Printer::new(node.node().unwrap(), false, db.as_ref()).to_string();
            input_string.insert_str(offset, input);

            let str_len = input_string.len();

            println!("---> {input_string}");
            let mut input = I::from(input_string);

            let store: CSTStore = CSTStore::default();
            match parse_with_recovery(&mut input, entry_point, db.as_ref(), &store) {
              Ok(mut candidates) => {
                if let Some(ctx) = (candidates.len() >= 1).then(|| candidates.remove(0)) {
                  if ctx.ctx.sym_ptr >= str_len {
                    node.replace(
                      ctx.nodes().map(|n| {
                        let mut node = EditNode::boxed(n.clone());
                        node.best(&store);
                        (*node.to_node().unwrap()).clone()
                      }),
                      &store,
                    );

                    if let Some((_, first)) = queue.pop_front() {
                      while queue.pop_back().is_some() {}
                      if let Some(new_node) = first.to_node() {
                        *graph_main = Some(new_node);
                      }
                      return Ok(0);
                    } else {
                      panic!("WTF")
                    }
                  } else {
                    println!("Going Up A level!")
                  }
                } else {
                  panic!("[2]");
                }
              }
              Err(_) => panic!("[3]"),
            }

            //panic!("need to compile:\n[{input_string}] \n {:#?}",
            // node.node());
          }
        }
      }
    }

    panic!("WTR");

    Err(ParserError::NoData)
  }

  pub fn delete(&mut self, offset: usize, length: usize) -> Result<usize, ParserError> {
    Err(ParserError::NoData)
  }

  pub fn cst(&self) -> Option<&CSTNode> {
    self.graph.as_ref().map(|s| s.as_ref())
  }
}

/*
pub struct CSTEditor<T: ParserInput> {
  db: Box<dyn ParserProducer<T>>,
}
use CSTNode::*;

impl<T: ParserInput> CSTEditor<T> {
  pub fn new(db: Box<dyn ParserProducer<T>>) -> Self {
    Self { db }
  }

  pub fn create_cst(&mut self, entry_name: &str, input: &mut T, source_id: u32) -> Result<CSTVec, ParseError> {
    if let Ok(entry) = self.db.get_entry_data_from_name(entry_name) {
      self.create_cst_array(entry.nonterm_id, input, source_id, 0, input.len())
    } else {
      Err(ParseError::Unexpected)
    }
  }

  fn create_cst_array(
    &mut self,
    non_terminal_id: u32,
    input: &mut T,
    source_id: u32,
    start: usize,
    end: usize,
  ) -> Result<CSTVec, ParseError> {
    let mut parser = self.db.get_parser()?;
    let mut ctx = parser.init_range(non_terminal_id, start, end)?;
    let mut cst = vec![];

    while let Some(action) = parser.next(input, &mut ctx) {
      match action {
        ParseAction::Accept { nonterminal_id, .. } => {
          if cst.len() < 1 {
            panic!(
              "Parser did not resolve CST. This is probably to to the
  originating grammar not supporting error recovery. Unable to provide a viable
  Concrete Syntax Tree structure."
            );
          } else {
            return Ok(cst);
          };
        }
        ParseAction::Error { last_nonterminal, .. } => {
          let mut last_input = ctx.current_tok();

          if last_input.len() == 0 {
            last_input.len += 1
          }
          return Err(ParseError::InputError {
            inline_message: "unrecognized input".into(),
            message: "Could not recognize input".into(),
            last_nonterminal,
            loc: last_input.to_token_from_ref(input.get_owned_ref()),
          });
        }
        ParseAction::Fork { .. } => {
          panic!("No implementation of fork resolution is available")
        }
        ParseAction::Skip { byte_length: len, token_id: id, byte_offset: off, .. } => {
          cst.push(CSTNode::Skipped { id: id as u16, source_id, len, off });
        }
        ParseAction::Shift { byte_length: len, token_id: id, byte_offset: off, .. } => {
          cst.push(CSTNode::Terminal { id: id as u16, source_id, len, off });
        }
        ParseAction::Reduce { rule_id: rule, nonterminal_id: id, symbol_count } => {
          cst.push(CSTNode::NonTerminal { rule, id, symbol_count, cache_data: 0 });
        }
        _ => panic!("Unexpected Action!"),
      }
    }
    Err(ParseError::Unexpected)
  }

  /// Takes an existing cst and a change patch and replace the effected section
  /// with the new path. This assumes the new patch is valid syntax for the
  /// node(s) that cover it.
  pub fn patch_cst_array(
    &mut self,
    current_cst: &CSTVec,
    input: &mut T,
    from: usize,
    old_len: usize,
    new_len: usize,
  ) -> Result<CSTVec, ParseError> {
    // From this range, merge in the edited data, and submit this as the data for
    // the parser. If the parser can successfully arrive (with our without errors)
    // at a non-terminal that matches the original non-terminal then we can
    // integrate the new node data with our existing CST structure.
    //
    // If there is an error with the data then we can extend our range to include
    // the next non-terminal, all the way to the root node.
    // This way of inline integration REQUIRES a parser that can support multiple
    // entrypoints for any non-terminal, so the Parser must be built with RD
    // flags, which provides the optimum variable goal parser.

    // find the first token that matches the needed range.

    let mut off = 0;
    let to = from + old_len;
    let mut start = 0;
    let mut start_off = 0;
    let mut end_off = 0;
    for (index, node) in current_cst.iter().enumerate() {
      use CSTNode::*;
      match node {
        Skipped { id, source_id, len, off } | Terminal { id, source_id, len, off } => {
          if (*off as usize) <= from && ((off + len) as usize) > from {
            start = index;
          }
          if ((off + len) as usize) >= to {
            // Found our start node. Now we need to find the nearest
            // nonterminal node that covers this node by reconstructing the
            // ast until we find our range.

            if let Some(index) = Self::find_nonterminal(index, current_cst) {
              if let Some((nterm_index, prev_index)) = Self::find_covering_nonterminal(start, index, current_cst) {
                let NonTerminal { id, .. } = current_cst[nterm_index] else { panic!() };
                let (start_off, end_off) = Self::get_size(nterm_index, current_cst);

                match self.create_cst_array(id, input, 1, start_off, input.len()) {
                  Ok(patch) => {
                    //dbg!(&patch);

                    // Apply the patch
                    let mut new_nodes = current_cst.clone();

                    new_nodes.splice(prev_index..=nterm_index, patch);

                    return Ok(new_nodes);
                  }
                  Err(err) => {
                    dbg!(err);
                    // go up a token level
                    let start_off = 0.max(start_off as i64 - 1) as usize;

                    return self.patch_cst_array(current_cst, input, start_off, end_off + 1, new_len);
                  }
                };
              }
            } else {
              return Err(ParseError::Unexpected);
            }
          }
        }
        _ => {}
      }
    }

    Err(ParseError::Unexpected)
  }

  fn find_covering_nonterminal(start_index: usize, current_nonterm_index: usize, current_cst: &CSTVec) -> Option<(usize, usize)> {
    use CSTNode::*;

    let NonTerminal { symbol_count, .. } = current_cst[current_nonterm_index] else { panic!("should be non-terminal") };

    let mut prev_index = Self::get_proceeding_symbol(current_nonterm_index, current_cst);

    let mut valid = false;

    for _ in 0..symbol_count {
      if prev_index <= start_index {
        valid = true;
      }

      prev_index = Self::get_prev(prev_index, current_cst);
    }

    if valid {
      Some((current_nonterm_index, Self::get_following_symbol(prev_index, current_cst)))
    }
    // Could not find a suitable range, seek out the next non-terminal and repeat
    // the process
    else if let Some(next_nonterminal) = Self::find_nonterminal(current_nonterm_index + 1, current_cst) {
      Self::find_covering_nonterminal(start_index, next_nonterminal, current_cst)
    } else {
      None
    }
  }

  fn get_prev(mut curr_index: usize, current_cst: &CSTVec) -> usize {
    if curr_index == 0 {
      return 0;
    }
    curr_index -= 1;
    loop {
      match current_cst[curr_index] {
        Terminal { .. } => return curr_index,
        NonTerminal { symbol_count, .. } => {
          let mut prev = curr_index - 1;
          for _ in 0..symbol_count {
            prev = Self::get_prev(prev, current_cst);
          }
          return prev;
        }
        _ => {}
      }
      if curr_index == 0 {
        break;
      }
      curr_index -= 1
    }
    0
  }

  fn get_size(mut curr_index: usize, current_cst: &CSTVec) -> (usize, usize) {
    use CSTNode::*;
    loop {
      match current_cst[curr_index] {
        Skipped { len, off, .. } | Terminal { len, off, .. } => return (off as usize, (off + len) as usize),
        NonTerminal { symbol_count, .. } => {
          let mut prev = Self::get_proceeding_symbol(curr_index, current_cst);
          let mut start = usize::MAX;
          let mut end = 0;
          for _ in 0..symbol_count {
            let (n_start, n_end) = Self::get_size(prev, current_cst);
            start = start.min(n_start);
            end = end.max(n_end);
            prev = Self::get_prev(prev, current_cst);
          }
          return (start, end);
        }
        _ => {}
      }
      if curr_index == 0 {
        break;
      }
      curr_index -= 1
    }
    (0, 0)
  }

  fn get_proceeding_symbol(mut curr_index: usize, current_cst: &CSTVec) -> usize {
    if curr_index == 1 {
      return 0;
    }

    curr_index -= 1;

    loop {
      match current_cst[curr_index] {
        NonTerminal { .. } | Terminal { .. } => return curr_index,
        _ => {}
      }
      if curr_index == 1 {
        break;
      }
      curr_index -= 1
    }
    0
  }

  fn get_following_symbol(mut curr_index: usize, current_cst: &CSTVec) -> usize {
    let top = current_cst.len() - 1;
    if curr_index == top {
      return top;
    }

    curr_index += 1;

    loop {
      match current_cst[curr_index] {
        NonTerminal { .. } | Terminal { .. } => return curr_index,
        _ => {}
      }
      if curr_index == top {
        return top;
      }
      curr_index += 1
    }
    top
  }

  fn find_nonterminal(curr_index: usize, current_cst: &CSTVec) -> Option<usize> {
    for index in curr_index..current_cst.len() {
      use CSTNode::*;
      match current_cst[index] {
        NonTerminal { .. } => return Some(index),
        _ => {}
      }
    }
    None
  }
}
 */
