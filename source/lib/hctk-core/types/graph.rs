//! # Transition Graph Types
use super::*;
use crate::grammar::get_closure_cached_with_state;
use bitmask_enum::bitmask;
use std::{
  collections::{BTreeMap, BTreeSet, HashMap, VecDeque},
  hash::Hash,
  ops::{Index, IndexMut},
  rc::Rc,
  sync::Arc,
  vec,
};

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct NodeId(u32);

impl NodeId {
  const GOTO_VIRTUAL_CLASS: u32 = 4000000000u32;
  pub const Invalid: Self = Self(u32::MAX);

  fn new(index: u32) -> Self {
    Self(index)
  }

  pub fn is_root(&self) -> bool {
    self.0 == 0
  }

  pub fn usize(&self) -> usize {
    self.0 as usize
  }

  pub fn to_goto_id(&self) -> Self {
    Self(self.0 | Self::GOTO_VIRTUAL_CLASS)
  }
}

impl Default for NodeId {
  fn default() -> Self {
    Self::Invalid
  }
}

impl From<NodeId> for u32 {
  fn from(value: NodeId) -> Self {
    value.0
  }
}

impl From<NodeId> for usize {
  fn from(value: NodeId) -> Self {
    value.0 as usize
  }
}

impl std::fmt::Display for NodeId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    std::fmt::Debug::fmt(&self.0, f)
  }
}

impl<T: Sized> Index<NodeId> for Vec<T> {
  type Output = T;

  #[inline(always)]
  fn index(&self, index: NodeId) -> &Self::Output {
    &self[index.0 as usize]
  }
}

impl<T: Sized> IndexMut<NodeId> for Vec<T> {
  #[inline(always)]
  fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
    &mut self[index.0 as usize]
  }
}

pub type MaybeNodeId = Option<NodeId>;

/// TODO: Isolate and build goto states.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum NodeType {
  Undefined,
  LRStart,
  RDStart,
  RAStart,
  GotoVirtual,
  Goto,
  TerminalTransition,
  PeekTransition,
  BreadcrumbTransition,
  BreadcrumbEndCompletion,
  BreadcrumbShiftCompletion,
  ProductionCall,
  Recovery,
  Complete,
  Fork,
  Pass,
  Fail,
  /// A kludge state to ensure base actions are performed
  /// in each fork.
  ForkBase,
  Shift,
}

impl Default for NodeType {
  fn default() -> Self {
    Self::Undefined
  }
}

impl std::fmt::Display for NodeType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    std::fmt::Debug::fmt(self, f)
  }
}

/// TODO: Isolate and build goto states.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum EdgeType {
  Undefined,
  // The order of `Goto` is important.
  Goto,
  // DO NOT reposition properties below this comment to above.
  Default,
  // DO NOT reposition properties below this comment to above.
  Assert,
  Peek,
  Start,
}

impl Default for EdgeType {
  fn default() -> Self {
    Self::Undefined
  }
}

impl std::fmt::Display for EdgeType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    std::fmt::Debug::fmt(self, f)
  }
}

#[bitmask]
pub enum NodeAttributes {
  EMPTY,

  /// Nodes marked as `I_PEEK_ORIGIN` are the root nodes of a
  /// peek branch.
  I_PEEK_ORIGIN,

  I_LR,
}

impl Default for NodeAttributes {
  fn default() -> Self {
    NodeAttributes::EMPTY
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub(crate) struct GraphNode {
  /// Items that represent the transition edges away from this node. In the resulting
  /// graph, any child node of this node should have an edge_symbol that matches one
  /// of the symbols in the closure of the parent's `transition_items`
  pub transition_items: Vec<Item>,
  /// Non-term items that are used to resolve GOTO
  /// and follow.
  pub goto_items: Vec<Item>,
  // pub output_items: Option<SymbolID>,
  /// The symbols that labels the edge that
  /// connects the previous state to this state.
  pub edge_symbol: SymbolID,
  pub prod_sym: Option<SymbolID>,
  pub parent: MaybeNodeId,
  /// This is set if this node is a member of a peek branch.
  /// Represents the state which normal parsing will return to
  /// when resolved leaves of the peek branch are reached.
  pub peek_goal: MaybeNodeId,
  /// The peek branch origin
  pub peek_origin: MaybeNodeId,
  /// The parent node whose closure
  /// produced the items belonging to this node.
  pub closure_parent: MaybeNodeId,
  pub proxy_parents: Vec<NodeId>,
  /// The number of symbol shifts that have occurred prior to
  /// reaching this node.
  pub shifts: u32,
  /// If this node is part of a disambiguating sequence, then
  /// this represents the positive number of shifts since the start of
  /// disambiguating that have occurred prior to reaching this node.
  /// Otherwise, this value is less than 0.
  pub peek_shifts: i32,
  pub id: NodeId,

  pub attributes: NodeAttributes,
  pub node_type:  NodeType,

  /// The type of scan action that is performed while
  /// traversing towards this node.
  pub edge_type: EdgeType,
  _impl:         u8,
}

impl GraphNode {
  /// Return a clone of the first item in the node's items array
  ///
  /// # Panics
  ///
  /// Panics if the node's items vector is empty
  pub fn first_item(&self) -> Item {
    self.transition_items.first().cloned().unwrap()
  }

  pub fn new(
    t_pack: &TransitionGraph,
    sym: SymbolID,
    parent_index: MaybeNodeId,
    items: Vec<Item>,
    node_type: NodeType,
  ) -> Self {
    let mut node = GraphNode {
      edge_symbol: sym,
      attributes: NodeAttributes::EMPTY,
      transition_items: items,
      peek_shifts: -1,
      id: NodeId::Invalid,
      node_type,
      ..Default::default()
    };

    if let Some(parent_index) = parent_index {
      let parent = &t_pack.graph_nodes[parent_index];
      node.parent = Some(parent_index);
      node.shifts = parent.shifts + 1;
      node.peek_goal = parent.peek_goal;
      node.peek_origin = parent.peek_origin;
    }

    node
  }

  pub fn is_peek_origin(&self) -> bool {
    self.is(NodeAttributes::I_PEEK_ORIGIN)
  }

  pub fn is_peek_node(&self) -> bool {
    self.peek_goal.is_some()
  }

  pub fn is_peek_goal_node(&self) -> bool {
    self.peek_origin.is_some() && !self.is_peek_node()
  }

  /// Strips state info from all items and returns the set of
  /// of unique items.
  pub fn get_unique_transition_item_set(&self) -> ItemSet {
    self.transition_items.clone().to_zero_state().to_set()
  }

  /// Returns the closure of this node's items
  #[deprecated]
  pub fn get_closure<'a>(&self, g: &'a GrammarStore) -> BTreeSet<Item> {
    self
      .transition_items
      .iter()
      .flat_map(|i| get_closure_cached_with_state(i, &g))
      .collect::<BTreeSet<_>>()
  }

  pub fn temp(origin: &GraphNode, sym: SymbolID, parent_index: usize, items: Vec<Item>) -> Self {
    GraphNode {
      edge_symbol: sym,
      attributes: NodeAttributes::EMPTY,
      proxy_parents: vec![],
      transition_items: items,
      peek_shifts: -1,
      id: NodeId::new(origin.id.0 * 100000),
      ..Default::default()
    }
  }

  #[inline(always)]
  pub fn is_orphan(&self, tpack: &TransitionGraph) -> bool {
    !self.has_parent(tpack)
  }

  #[inline(always)]
  pub fn has_parent(&self, tpack: &TransitionGraph) -> bool {
    self.parent.is_some() && self.id != NodeId::Invalid
  }

  #[inline(always)]
  pub fn is(&self, transition_type: NodeAttributes) -> bool {
    self.attributes.intersects(transition_type)
  }

  #[inline(always)]
  pub fn set_attribute(&mut self, transition_type: NodeAttributes) {
    self.attributes |= transition_type
  }

  #[inline(always)]
  pub fn unset_type(&mut self, transition_type: NodeAttributes) {
    self.attributes &= self.attributes ^ transition_type
  }

  pub fn debug_string(&self, g: &GrammarStore) -> String {
    format!(
      "\n|[{}]--->[{}]{}\n|   sym: {}\n|  edge: {}\n|  node: {}{}\n| items:[\n{}\n]",
      self.id,
      self.parent.map(|i| i.to_string()).unwrap_or(String::from("")),
      self
        .proxy_parents
        .iter()
        .map(|i| format!("\n|[{}]--->[{}]", i.to_string(), self.id))
        .collect::<Vec<_>>()
        .join(""),
      self.edge_symbol.to_string(g),
      self.edge_type.to_string(),
      self.node_type.to_string(),
      if self.is(NodeAttributes::I_LR) { " LR" } else { "" },
      self
        .transition_items
        .clone()
        //.to_origin_only_state()
        .to_set()
        .iter()
        .map(|i| "   ".to_string() + &i.debug_string(g))
        .collect::<Vec<_>>()
        .join("\n")
    )
  }

  pub fn linked_to_self(&self) -> bool {
    match self.parent {
      Some(parent_index) => {
        parent_index == self.id || self.proxy_parents.iter().any(|i| self.id == *i)
      }
      _ => false,
    }
  }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TransitionMode {
  RecursiveDescent,
  RecursiveAscent,
  LR,
}

impl Default for TransitionMode {
  fn default() -> Self {
    TransitionMode::RecursiveDescent
  }
}

pub(crate) type TPackResults = (TransitionGraph, Vec<HCError>);

#[derive(Debug, Default)]
pub(crate) struct ProcessGroup {
  pub node_index:   NodeId,
  pub items:        Vec<Item>,
  pub discriminant: Option<(SymbolID, Vec<Item>)>,
  pub depth:        usize,
}

/// Maintains a set of transition nodes and related properties that describe
/// either a complete or intermediate parse graph.
///
/// Note: Since transition functions can create Recursive Ascent as well as
/// Recursive Descent style graphs, the resulting graph may contain cycles.
#[derive(Debug, Default)]
pub(crate) struct TransitionGraph {
  pub goto_scoped_closure: Option<Rc<Box<Vec<Item>>>>,
  pub out_of_scope_closure: Option<Vec<Item>>,
  pub goto_seeds: BTreeSet<Item>,
  pub leaf_nodes: Vec<NodeId>,
  pub mode: TransitionMode,
  pub is_scanner: bool,
  pub root_prod_ids: BTreeSet<ProductionId>,
  pub peek_ids: BTreeSet<u64>,
  pub starts: BTreeSet<Item>,
  pub errors: Vec<HCError>,
  pub events: BTreeMap<u64, NodeId>,
  /// If this Graph defines a goto transition sequence,
  /// then this is true if the root goto state does not simply
  /// resolve to a pass action.
  pub non_trivial_root: bool,
  pub g: Arc<GrammarStore>,
  //
  graph_nodes: Vec<GraphNode>,
  node_pipeline: VecDeque<ProcessGroup>,
  /// Internal pipeline to drive transition tree
  /// creation.
  /// Stores indices of pruned node slots that can be reused
  empty_cache: VecDeque<NodeId>,
  /// For a givin item, points to an originating
  /// item that can used to look up it's own closure
  closure_links: HashMap<Item, Item>,
}

impl TransitionGraph {
  pub fn new(
    g: Arc<GrammarStore>,
    mode: TransitionMode,
    is_scanner: bool,
    starts: &[Item],
    root_prod_ids: BTreeSet<ProductionId>,
  ) -> Self {
    TransitionGraph {
      node_pipeline: VecDeque::with_capacity(32),
      empty_cache: VecDeque::with_capacity(16),
      mode,
      is_scanner,
      root_prod_ids,
      starts: BTreeSet::from_iter(starts.iter().map(|i| i.to_start().to_origin_only_state())),
      graph_nodes: Vec::with_capacity(256),
      g,
      ..Default::default()
    }
  }

  pub fn queue_node(&mut self, process_group: ProcessGroup) {
    self.node_pipeline.push_back(process_group)
  }

  pub fn get_first_prod_id(&self) -> Option<ProductionId> {
    self.root_prod_ids.first().cloned()
  }

  #[inline(always)]
  pub fn get_next_queued(&mut self) -> Option<ProcessGroup> {
    self.node_pipeline.pop_front()
  }

  pub fn insert_node(&mut self, mut node: GraphNode) -> NodeId {
    if let Some(slot_index) = self.empty_cache.pop_front() {
      node.id = slot_index;

      self.graph_nodes[slot_index] = node;

      slot_index
    } else {
      let id = NodeId::new(self.graph_nodes.len() as u32);

      node.id = id;

      self.graph_nodes.push(node);

      id
    }
  }

  /// Removes the edge between this node and its parent, rendering
  /// it orphaned and available for destruction / reuse
  pub fn drop_node(&mut self, node_index: &NodeId) -> MaybeNodeId {
    let parent;

    {
      let node = self.get_node_mut(*node_index);

      parent = node.parent;

      node.parent = None;
      node.peek_goal = None;
      node.peek_origin = None;
      node.id = NodeId::Invalid;
      node.transition_items.clear();
    }

    if *node_index != NodeId::Invalid {
      self.empty_cache.push_back(*node_index);
    }

    parent
  }

  /// If the node at node_index is `node.is_peek_node()`, then this
  /// returns the node's peek_goal node.
  ///
  /// # Panic
  ///
  /// Panics in debug if the node at `node_index` is dead, or
  /// if the node is not a peek node.
  #[inline(always)]
  pub fn get_goal(&self, node_index: NodeId) -> &GraphNode {
    debug_assert!(
      self.graph_nodes[node_index].id != NodeId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    debug_assert!(
      self.graph_nodes[node_index].is_peek_node(),
      "Invalid access of a non-peek node at index {}",
      node_index
    );

    self.get_node(self.graph_nodes[node_index].peek_goal.unwrap())
  }

  /// If the node at node_index is `node.is_peek_node()` or is `node.is_peek_goal()`, then this
  /// returns the node's peek_origin node.
  ///
  /// # Panic
  ///
  /// Panics in debug if the node at `node_index` is dead, or
  /// if the node is not a peek node or peek goal node.
  #[inline(always)]
  pub fn get_peek_origin(&self, node_index: NodeId) -> &GraphNode {
    let node = &self.graph_nodes[node_index];

    debug_assert!(
      node.id != NodeId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    if node.is_peek_origin() {
      node
    } else {
      debug_assert!(
        !node.is_peek_node() || !node.is_peek_goal_node(),
        "Invalid access of a non-peek node at index {}",
        node_index
      );

      self.get_peek_origin(node.peek_origin.unwrap())
    }
  }

  #[inline(always)]
  pub fn get_node(&self, node_index: NodeId) -> &GraphNode {
    debug_assert!(
      self.graph_nodes[node_index].id != NodeId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    &self.graph_nodes[node_index]
  }

  #[inline(always)]
  pub fn get_node_mut(&mut self, node_index: NodeId) -> &mut GraphNode {
    debug_assert!(
      self.graph_nodes[node_index].id != NodeId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    &mut self.graph_nodes[node_index]
  }

  pub fn nodes_iter(&self) -> core::slice::Iter<GraphNode> {
    self.graph_nodes.iter()
  }

  pub fn clean(self) -> TPackResults {
    (
      TransitionGraph {
        goto_seeds: self.goto_seeds,
        graph_nodes: self.graph_nodes,
        leaf_nodes: self.leaf_nodes,
        mode: self.mode,
        is_scanner: self.is_scanner,
        root_prod_ids: self.root_prod_ids,
        closure_links: self.closure_links,
        non_trivial_root: self.non_trivial_root,
        g: self.g,
        ..Default::default()
      },
      self.errors,
    )
  }

  pub fn get_node_len(&self) -> usize {
    self.graph_nodes.len()
  }

  pub fn write_nodes(&self) -> String {
    let mut string = String::new();
    for node in &self.graph_nodes {
      if !node.is_orphan(self) || node.id.0 == 0 {
        string += &format!("\n{}\n", node.debug_string(&self.g));
      }
    }

    string
  }

  pub fn print_nodes(&self) {
    for node in &self.graph_nodes {
      if !node.is_orphan(self) || node.id.0 == 0 {
        println!("{}\n", node.debug_string(&self.g));
      }
    }
  }
}
