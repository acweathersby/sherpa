use crate::grammar::JSBytecodeParserDB;
use js_sys::Uint32Array;
use radlr_rust_runtime::{
  parsers::{self},
  types::*,
};
use std::rc::Rc;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct EditGraph {
  graph: parsers::cst::EditGraph<StringInput, BytecodeParserDB>,
}

#[wasm_bindgen]
impl EditGraph {
  #[wasm_bindgen(constructor)]
  pub fn new(input: String, db: &JSBytecodeParserDB) -> Self {
    Self {
      graph: parsers::cst::EditGraph::parse(db.0.default_entrypoint(), input, db.0.clone()).unwrap(),
    }
  }

  pub fn patch_insert(&mut self, node: &JSCSTNode, offset: usize, text: String) -> Option<JSPatchResult> {
    self.graph.patch_insert(&node.node, offset, &text).map(|r| JSPatchResult { _result: r })
  }

  pub fn patch_remove(&mut self, node: &JSCSTNode, offset: usize, len: usize) -> Option<JSPatchResult> {
    self.graph.patch_remove(&node.node, offset, len).map(|r| JSPatchResult { _result: r })
  }

  pub fn get_offset(&self, node: &JSCSTNode, offset: usize) -> Option<Uint32Array> {
    self.graph.get_offset_path(&node.node, offset).map(|(mut path, index)| {
      path.push(index);
      Uint32Array::from(&path[..])
    })
  }

  pub fn get_root(&self) -> Option<JSCSTNode> {
    self.graph.cst().map(|node| JSCSTNode { node })
  }

  pub fn add_child(&self, par: &JSCSTNode, child: &JSCSTNode, offset: usize) -> JSCSTNode {
    let Some(NonTermNode { id, rule, length, symbols }) = par.node.as_nonterm() else { return par.clone() };

    let mut symbols = symbols.clone();

    symbols.insert(offset, child.node.clone());

    let length = *length + child.node.len() as u32;

    JSCSTNode {
      node: Rc::new(NonTermNode::typed(*id, *rule, symbols, length as usize)),
    }
  }

  pub fn remove_child(&self, par: &JSCSTNode, offset: usize) -> JSCSTNode {
    let Some(NonTermNode { id, rule, length, symbols }) = par.node.as_nonterm() else { return par.clone() };

    let mut symbols = symbols.clone();

    let child = symbols.remove(offset);

    let length = *length - child.len() as u32;

    JSCSTNode {
      node: Rc::new(NonTermNode::typed(*id, *rule, symbols, length as usize)),
    }
  }
}

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct JSCSTNode {
  node: Rc<CSTNode>,
}

#[wasm_bindgen]
impl JSCSTNode {
  pub fn get_type(&self) -> String {
    match self.node.ty() {
      NodeType::Alternative => "Alternative",
      NodeType::Errata => "Errata",
      NodeType::Alternatives => "Alternatives",
      NodeType::Missing => "Missing",
      NodeType::None => "None",
      NodeType::Nonterm => "Nonterm",
      NodeType::Skipped => "Skipped",
      NodeType::Token => "Token",
    }
    .into()
  }

  pub fn get_text(&self, graph: &EditGraph) -> String {
    if let Some(tk) = self.node.as_token() {
      match tk.ty() {
        NodeType::Missing => {
          let db = graph.graph.db();
          let id = tk.tok_id() as u32;
          db.token_id_to_str(id).unwrap_or("[missing]").into()
        }
        _ => tk.str().into(),
      }
    } else {
      "".into()
    }
  }

  pub fn child_at(&self, index: usize) -> Option<JSCSTNode> {
    if let Some(nt) = self.node.as_nonterm() {
      nt.symbols.get(index).map(|n| JSCSTNode { node: n.clone() })
    } else {
      None
    }
  }

  pub fn num_of_children(&self) -> usize {
    if let Some(nt) = self.node.as_nonterm() {
      nt.symbols.len()
    } else {
      0
    }
  }

  pub fn len(&self) -> usize {
    self.node.len()
  }
}

#[wasm_bindgen]
pub struct JSPatchResult {
  _result: Vec<Rc<CSTNode>>,
}

#[wasm_bindgen]
impl JSPatchResult {
  pub fn node_at(&self, index: usize) -> Option<JSCSTNode> {
    self._result.get(index).map(|n| JSCSTNode { node: n.clone() })
  }

  pub fn num_of_nodes(&self) -> usize {
    self._result.len()
  }
}
