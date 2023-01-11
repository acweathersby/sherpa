//! # Transition Graph Types
use super::*;
use crate::grammar::get_closure_cached_with_state;
use bitmask_enum::bitmask;
use std::{
  collections::{BTreeSet, HashMap, VecDeque},
  hash::Hash,
  ops::{Index, IndexMut},
  rc::Rc,
  sync::Arc,
};

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct NodeId(u32);

impl NodeId {
  const GOTO_VIRTUAL_CLASS: u32 = 4000000000u32;
  pub const Invalid: Self = Self(u32::MAX);

  pub(crate) fn new(index: u32) -> Self {
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

pub(crate) type MaybeNodeId = Option<NodeId>;

/// TODO: Isolate and build goto states.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub(crate) enum NodeType {
  Undefined,
  _LRStart,
  RDStart,
  RAStart,
  GotoVirtual,
  Goto,
  PeekTransition,
  BreadcrumbTransition,
  BreadcrumbEndCompletion,
  BreadcrumbShiftCompletion,
  ProductionCall,
  _Recovery,
  Complete,
  Fork,
  Pass,
  Fail,
  OutOfScopeComplete,
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
pub(crate) enum EdgeType {
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

  pub node_type: NodeType,

  /// The type of scan action that is performed while
  /// traversing towards this node.
  pub edge_type: EdgeType,
  _impl:         u8,
}

impl GraphNode {
  pub fn new(
    t_pack: &TransitionGraph,
    sym: SymbolID,
    parent_index: MaybeNodeId,
    items: Vec<Item>,
    node_type: NodeType,
  ) -> Self {
    let mut node = GraphNode {
      edge_symbol: sym,
      transition_items: items,
      peek_shifts: -1,
      id: NodeId::Invalid,
      node_type,
      ..Default::default()
    };

    if let Some(parent_index) = parent_index {
      let parent = &t_pack.nodes[parent_index];
      node.parent = Some(parent_index);
      node.shifts = parent.shifts + 1;
      node.peek_goal = parent.peek_goal;
      node.peek_origin = parent.peek_origin;
    }

    node
  }

  /// Strips state info from all items and returns the set of
  /// of unique items.
  pub fn get_unique_transition_item_set(&self) -> ItemSet {
    self.transition_items.clone().to_empty_state().to_set()
  }

  /// Returns the closure of this node's items
  pub fn get_closure<'a>(&self, g: &'a GrammarStore) -> BTreeSet<Item> {
    self
      .transition_items
      .iter()
      .flat_map(|i| get_closure_cached_with_state(i, &g))
      .collect::<BTreeSet<_>>()
  }

  #[inline(always)]
  pub fn is_orphan(&self) -> bool {
    !self.has_parent()
  }

  #[inline(always)]
  pub fn has_parent(&self) -> bool {
    self.parent.is_some() && self.id != NodeId::Invalid
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
      "",
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
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) enum TransitionMode {
  RecursiveDescent,
  RecursiveAscent,
  LR,
}

impl Default for TransitionMode {
  fn default() -> Self {
    TransitionMode::RecursiveDescent
  }
}

pub(crate) type TPackResults = (TransitionGraph, Vec<SherpaError>);

#[derive(Debug, Default)]
pub(crate) struct ProcessGroup {
  pub node_index:   NodeId,
  pub items:        Vec<Item>,
  pub discriminant: Option<(SymbolID, Vec<Item>)>,
  pub depth:        usize,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum ScanType {
  None = 0,
  ScannerProduction,
  ScannerEntry,
}

impl Default for ScanType {
  fn default() -> Self {
    Self::None
  }
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
  pub scan_type: ScanType,
  pub root_prod_ids: BTreeSet<ProductionId>,
  pub starts: BTreeSet<Item>,
  pub errors: Vec<SherpaError>,
  /// If this Graph defines a goto transition sequence,
  /// then this is true if the root goto state does not simply
  /// resolve to a pass action.
  pub non_trivial_root: bool,
  pub g: Arc<GrammarStore>,
  //
  nodes: Vec<GraphNode>,
  node_pipeline: VecDeque<ProcessGroup>,
  /// Internal pipeline to drive transition tree
  /// creation.
  /// Stores indices of pruned node slots that can be reused
  empty_cache: VecDeque<NodeId>,
  /// For a givin item, points to an originating
  /// item that can used to look up it's own closure
  closure_links: HashMap<Item, Item>,

  pub accept_items: ItemSet,
  lane_counter:     u32,
}

impl TransitionGraph {
  pub fn new(
    g: Arc<GrammarStore>,
    mode: TransitionMode,
    scan_type: ScanType,
    starts: &[Item],
    root_prod_ids: BTreeSet<ProductionId>,
  ) -> Self {
    TransitionGraph {
      node_pipeline: VecDeque::with_capacity(32),
      empty_cache: VecDeque::with_capacity(16),
      mode,
      scan_type,
      root_prod_ids,
      starts: BTreeSet::from_iter(starts.iter().map(|i| i.to_start().to_origin_only_state())),
      nodes: Vec::with_capacity(256),
      g,
      ..Default::default()
    }
  }

  pub fn is_scan(&self) -> bool {
    !(matches!(self.scan_type, ScanType::None))
  }

  pub fn item_is_goal(&self, item: Item) -> bool {
    self.accept_items.contains(&item.to_origin_only_state())
  }

  /// Increments the monotonic lane counter by `amount`, and returns
  /// the previous value of the counter.
  pub fn increment_lane(&mut self, amount: u32) -> u32 {
    let prev_val = self.lane_counter;
    self.lane_counter += amount;
    prev_val
  }

  /// If this is the root node, then this is the set of all
  /// transition items coerced into completed Items. Origin
  /// and lane info is stripped from these items. Other wise
  /// the set is empty.
  pub fn accept_items(&self) -> &ItemSet {
    &self.accept_items
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

      self.nodes[slot_index] = node;

      slot_index
    } else {
      let id = NodeId::new(self.nodes.len() as u32);

      node.id = id;

      self.nodes.push(node);

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

  #[inline(always)]
  pub fn get_node(&self, node_index: NodeId) -> &GraphNode {
    debug_assert!(
      self.nodes[node_index].id != NodeId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    &self.nodes[node_index]
  }

  #[inline(always)]
  pub fn get_node_mut(&mut self, node_index: NodeId) -> &mut GraphNode {
    debug_assert!(
      self.nodes[node_index].id != NodeId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    &mut self.nodes[node_index]
  }

  pub fn nodes_iter(&self) -> core::slice::Iter<GraphNode> {
    self.nodes.iter()
  }

  pub fn clean(self) -> TPackResults {
    (
      TransitionGraph {
        goto_seeds: self.goto_seeds,
        nodes: self.nodes,
        leaf_nodes: self.leaf_nodes,
        mode: self.mode,
        scan_type: self.scan_type,
        root_prod_ids: self.root_prod_ids,
        closure_links: self.closure_links,
        non_trivial_root: self.non_trivial_root,
        accept_items: self.accept_items,
        g: self.g,
        ..Default::default()
      },
      self.errors,
    )
  }

  pub fn _get_node_len(&self) -> usize {
    self.nodes.len()
  }

  pub fn write_nodes(&self) -> String {
    let mut string = String::new();
    for node in &self.nodes {
      if !node.is_orphan() || node.id.0 == 0 {
        string += &format!("\n{}\n", node.debug_string(&self.g));
      }
    }

    string
  }

  pub fn _print_nodes(&self) {
    for node in &self.nodes {
      if !node.is_orphan() || node.id.0 == 0 {
        eprintln!("{}\n", node.debug_string(&self.g));
      }
    }
  }
}
