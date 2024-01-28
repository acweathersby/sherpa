use crate::{
  compile::states::build_graph::graph::{GraphBuildState, GraphType, StateId, StateType},
  proxy::OrderedSet,
  types::{ItemSet, PrecedentSymbol, SharedParserDatabase},
  Item,
};
use std::{self, fmt::Debug, sync::Arc};

use super::ScannerData;

#[derive(Clone)]
pub(crate) struct GraphNode {
  pub id:          StateId,
  pub build_state: GraphBuildState,
  pub is_leaf:     bool,
  pub ty:          StateType,
  pub sym:         PrecedentSymbol,
  pub hash_id:     u64,
  pub kernel:      OrderedSet<Item>,
  pub reduce_item: Option<Item>,
  pub graph_type:  GraphType,
  pub predecessor: Option<SharedGraphNode>,
  pub symbol_set:  Option<Arc<ScannerData>>,
  pub db:          SharedParserDatabase,
  pub is_goto:     bool,
}

impl Debug for GraphNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let db = &self.db;

    let header = format!("{:?} - {:?} ", self.graph_type, self.id);

    let scanner = self
      .symbol_set
      .as_ref()
      .map(|s| s.symbols.iter().map(|s| db.sym(s.tok()).debug_string(db)).collect::<Vec<_>>().join(" | "))
      .unwrap_or_default();

    if self.is_leaf() {
      f.write_fmt(format_args!(
        "
== LEAF {:=<120}

ty         : {:?}
reduce on  : {:?}
prime pred : {:?}
items:
{}
{scanner}
{:=>128}",
        header,
        self.ty,
        self.reduce_item.map(|f| f._debug_string_w_db_(db)),
        self.predecessor.as_ref().map(|i| i.id()),
        self.kernel.iter().map(|i| i._debug_string_w_db_(db)).collect::<Vec<_>>().join("\n"),
        ""
      ))
    } else if self.is_root() {
      f.write_fmt(format_args!(
        "
## ROOT {:#<120}

items:
{}
{scanner}
{:#>128}",
        header,
        self.kernel.iter().map(|i| i._debug_string_w_db_(db)).collect::<Vec<_>>().join("\n"),
        ""
      ))
    } else {
      f.write_fmt(format_args!(
        "
-- {:-<125}

ty         : {:?}
sym        : {}
prime pred : {:?}
items:
{}
{scanner}
{:->128}",
        header,
        self.ty,
        self.sym.sym().debug_string(db),
        self.predecessor.as_ref().map(|i| i.id()),
        self.kernel.iter().map(|i| i._debug_string_w_db_(db)).collect::<Vec<_>>().join("\n"),
        ""
      ))
    }
  }
}

pub type SharedGraphNode = std::sync::Arc<GraphNode>;

impl GraphNode {
  pub fn get_root(&self) -> &GraphNode {
    if self.is_root() {
      self
    } else {
      self.predecessor.as_ref().unwrap().get_root()
    }
  }

  pub fn is_goto(&self) -> bool {
    self.is_goto
  }

  pub fn to_goto(&self) -> SharedGraphNode {
    Arc::new(Self { is_goto: true, ..self.clone() })
  }

  pub fn goal_items(&self) -> &ItemSet {
    self.get_root().kernel_items()
  }

  pub fn item_is_goal(&self, item: Item) -> bool {
    let root = self.get_root();
    root.kernel_items().iter().any(|i| i.to_canonical().to_complete() == item.to_canonical())
  }

  pub fn is_root(&self) -> bool {
    self.id.is_rootish()
  }

  pub fn kernel_items(&self) -> &OrderedSet<Item> {
    &self.kernel
  }

  pub fn id(&self) -> StateId {
    self.id
  }

  pub fn is_leaf(&self) -> bool {
    self.is_leaf
  }

  pub fn build_state(&self) -> GraphBuildState {
    self.build_state
  }

  pub fn graph_type(&self) -> GraphType {
    self.graph_type
  }

  pub fn state_type(&self) -> StateType {
    self.ty
  }

  pub fn is_scanner(&self) -> bool {
    self.graph_type == GraphType::Scanner
  }

  pub fn get_predecessor<'a>(&'a self, id: StateId) -> Option<&'a SharedGraphNode> {
    if let Some(pred) = self.predecessor.as_ref() {
      if pred.id == id {
        Some(pred)
      } else {
        pred.get_predecessor(id)
      }
    } else {
      None
    }
  }

  pub fn parent<'a>(&'a self) -> Option<&'a SharedGraphNode> {
    self.predecessor.as_ref()
  }
}
