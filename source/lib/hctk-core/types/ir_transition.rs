use super::*;
use crate::grammar::{get_closure_cached, get_closure_cached_with_state};
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
pub struct TGNId(u32);

impl TGNId {
  const GOTO_CLASS: u32 = 4000000000u32;
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
    Self(self.0 | Self::GOTO_CLASS)
  }
}

impl Default for TGNId {
  fn default() -> Self {
    Self::Invalid
  }
}

impl From<TGNId> for u32 {
  fn from(value: TGNId) -> Self {
    value.0
  }
}

impl From<TGNId> for usize {
  fn from(value: TGNId) -> Self {
    value.0 as usize
  }
}

impl std::fmt::Display for TGNId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.0.fmt(f)
  }
}

impl<T: Sized> Index<TGNId> for Vec<T> {
  type Output = T;

  #[inline(always)]
  fn index(&self, index: TGNId) -> &Self::Output {
    &self[index.0 as usize]
  }
}

impl<T: Sized> IndexMut<TGNId> for Vec<T> {
  #[inline(always)]
  fn index_mut(&mut self, index: TGNId) -> &mut Self::Output {
    &mut self[index.0 as usize]
  }
}

pub type TGNRef = Option<TGNId>;

#[bitmask]
pub enum TransitionStateType {
  EMPTY,
  /// Transition has occurred from
  /// the consumption of a terminal
  /// symbol. All transition should
  /// have this set except for the
  /// initial state and the goal state.
  O_SHIFT,

  /// Transition has occurred from the
  /// completion of non-terminal symbol.
  O_CALL,

  /// Transition has occurred from the
  /// accepting of a completed root item.
  O_ACCEPT,

  /// Transition shift next token, but the reader state is
  /// preserved and eventually reset to the last non-peek
  /// transition.
  O_PEEK_SHIFT,

  O_FORK,

  /// State includes items out of the scope of the current
  /// production that should be used for disambiguating states
  /// that would cause a reduction to a production ID other than
  /// the current production.
  I_OUT_OF_SCOPE,

  /// Transition has occurred from the
  /// accepting of a root item.
  I_END,

  I_GOTO_START,

  /// Indicates this node consumes its symbol as a token.
  I_SHIFT,

  I_PASS,

  I_FAIL,

  /// Nodes marked as `I_PEEK_ORIGIN` are the root nodes of a
  /// peek branch.
  I_PEEK_ORIGIN,

  /// This state is set when the nodes item has a skipped symbol
  /// that occludes another item that consumes that symbol.
  I_SKIPPED_COLLISION,

  /// Indicates the item of this node is the result of a goto on a
  /// root production.
  I_GOTO_ROOT,

  I_GOTO_LR_BRANCH,

  /// Indicates the item of this node is an end item that results
  /// from the GOTO of a production.
  I_GOTO_END,

  I_GOTO_LR,
  I_LR_START,
}

impl Default for TransitionStateType {
  fn default() -> Self {
    TransitionStateType::EMPTY
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TransitionGraphNode {
  pub trans_type: TransitionStateType,
  pub items: Vec<Item>,
  /// The symbols that labels the edge that
  /// connects the parent state to this state.
  pub edge_symbol: SymbolID,
  pub prod_sym: Option<SymbolID>,
  pub parent: TGNRef,
  /// This is set if this node is a member of a peek branch.
  /// Represents the state which normal parsing will return to
  /// when resolved leaves of the peek branch are reached.
  pub peek_goal: TGNRef,
  /// The peek branch origin
  pub peek_origin: TGNRef,
  /// The parent node whose closure
  /// produced the items belonging to this node.
  pub closure_parent: TGNRef,
  pub proxy_parents: Vec<TGNId>,
  /// The number of symbol shifts that have occurred prior to
  /// reaching this node.
  pub shifts: u32,
  /// If this node is part of a disambiguating sequence, then
  /// this represents the positive number of shifts since the start of
  /// disambiguating that have occurred prior to reaching this node.
  /// Otherwise, this value is less than 0.
  pub peek_shifts: i32,
  pub id: TGNId,
}

impl TransitionGraphNode {
  /// Return a clone of the first item in the node's items array
  ///
  /// # Panics
  ///
  /// Panics if the node's items vector is empty
  pub fn first_item(&self) -> Item {
    self.items.first().cloned().unwrap()
  }

  pub fn is_out_of_scope(&self) -> bool {
    return self.items[0].get_state().is_goto_end_origin();
  }

  pub fn new(
    t_pack: &TransitionPack,
    sym: SymbolID,
    parent_index: TGNRef,
    items: Vec<Item>,
  ) -> Self {
    let mut node = TransitionGraphNode {
      edge_symbol: sym,
      trans_type: TransitionStateType::EMPTY,
      items,
      peek_shifts: -1,
      id: TGNId::Invalid,
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
    self.is(TransitionStateType::I_PEEK_ORIGIN)
  }

  pub fn is_peek_node(&self) -> bool {
    self.peek_goal.is_some()
  }

  pub fn is_peek_goal_node(&self) -> bool {
    self.peek_origin.is_some() && !self.is_peek_node()
  }

  /// Returns the closure of this node's items
  pub fn get_closure<'a>(&self, g: &'a GrammarStore) -> BTreeSet<Item> {
    self.items.iter().flat_map(|i| get_closure_cached_with_state(i, &g)).collect::<BTreeSet<_>>()
  }

  pub fn temp(
    origin: &TransitionGraphNode,
    sym: SymbolID,
    parent_index: usize,
    items: Vec<Item>,
  ) -> Self {
    TransitionGraphNode {
      edge_symbol: sym,
      trans_type: TransitionStateType::EMPTY,
      proxy_parents: vec![],
      items,
      peek_shifts: -1,
      id: TGNId::new(origin.id.0 * 100000),
      ..Default::default()
    }
  }

  #[inline(always)]
  pub fn is_orphan(&self, tpack: &TransitionPack) -> bool {
    !self.has_parent(tpack)
  }

  #[inline(always)]
  pub fn has_parent(&self, tpack: &TransitionPack) -> bool {
    self.parent.is_some() && self.id != TGNId::Invalid
  }

  #[inline(always)]
  pub fn is(&self, transition_type: TransitionStateType) -> bool {
    self.trans_type.intersects(transition_type)
  }

  #[inline(always)]
  pub fn set_type(&mut self, transition_type: TransitionStateType) {
    self.trans_type |= transition_type
  }

  #[inline(always)]
  pub fn unset_type(&mut self, transition_type: TransitionStateType) {
    self.trans_type &= self.trans_type ^ transition_type
  }

  pub fn debug_string(&self, g: &GrammarStore) -> String {
    format!(
      "{{[{}] par:[{}] sym:{}\n    [\n{}\n    ]}}",
      self.id,
      self.parent.unwrap_or(TGNId::Invalid),
      self.edge_symbol.to_string(g),
      self
        .items
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
  GoTo,
}

impl Default for TransitionMode {
  fn default() -> Self {
    TransitionMode::RecursiveDescent
  }
}

pub type TPackResults = (TransitionPack, Vec<HCError>);

/// Maintains a set of transition nodes and related properties that describe
/// either a complete or intermediate parse graph.
///
/// Note: Since transition functions can create Recursive Ascent as well as
/// Recursive Descent style graphs, the resulting graph may contain cycles.
#[derive(Debug, Default)]
pub struct TransitionPack {
  pub goto_scoped_closure: Option<Rc<Box<Vec<Item>>>>,
  pub out_of_scope_closure: Option<Vec<Item>>,
  pub goto_seeds: BTreeSet<Item>,
  pub leaf_nodes: Vec<TGNId>,
  pub mode: TransitionMode,
  pub is_scanner: bool,
  pub root_prod_ids: BTreeSet<ProductionId>,
  pub peek_ids: BTreeSet<u64>,
  pub starts: BTreeSet<Item>,
  pub errors: Vec<HCError>,
  pub events: BTreeMap<u64, TGNId>,
  /// If this TPack defines a goto transition sequence,
  /// then this is true if root goto state does not simply
  /// resolve to a pass action.
  pub non_trivial_root: bool,
  pub g: Arc<GrammarStore>,
  //
  graph_nodes: Vec<TransitionGraphNode>,
  node_pipeline: VecDeque<(TGNId, Vec<Item>)>,
  /// Internal pipeline to drive transition tree
  /// creation.
  /// Stores indices of pruned node slots that can be reused
  empty_cache: VecDeque<TGNId>,
  /// For a givin item, points to an originating
  /// item that can used to look up it's own closure
  closure_links: HashMap<Item, Item>,
}

impl TransitionPack {
  pub fn new(
    g: Arc<GrammarStore>,
    mode: TransitionMode,
    is_scanner: bool,
    starts: &[Item],
    root_prod_ids: BTreeSet<ProductionId>,
  ) -> Self {
    TransitionPack {
      node_pipeline: VecDeque::with_capacity(32),
      empty_cache: VecDeque::with_capacity(16),
      mode,
      is_scanner,
      root_prod_ids,
      starts: BTreeSet::from_iter(starts.iter().map(|i| i.to_start().to_zero_state())),
      graph_nodes: Vec::with_capacity(256),
      g,
      ..Default::default()
    }
  }

  pub fn queue_node(&mut self, node_index: TGNId, items: Vec<Item>) {
    self.node_pipeline.push_back((node_index, items))
  }

  pub fn get_first_prod_id(&self) -> Option<ProductionId> {
    self.root_prod_ids.first().cloned()
  }

  #[inline(always)]
  pub fn get_next_queued(&mut self) -> Option<(TGNId, Vec<Item>)> {
    self.node_pipeline.pop_front()
  }

  pub fn insert_node(&mut self, mut node: TransitionGraphNode) -> TGNId {
    if let Some(slot_index) = self.empty_cache.pop_front() {
      node.id = slot_index;

      self.graph_nodes[slot_index] = node;

      slot_index
    } else {
      let id = TGNId::new(self.graph_nodes.len() as u32);

      node.id = id;

      self.graph_nodes.push(node);

      id
    }
  }

  /// Removes the edge between this node and its parent, rendering
  /// it orphaned and available for destruction / reuse
  pub fn drop_node(&mut self, node_index: &TGNId) -> TGNRef {
    let node_id;
    let parent;

    {
      let node = self.get_node_mut(*node_index);

      node_id = node.id;

      parent = node.parent;

      node.parent = None;
      node.peek_goal = None;
      node.peek_origin = None;
      node.id = TGNId::Invalid;
      node.items.clear();
    }

    if *node_index != TGNId::Invalid {
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
  pub fn get_goal(&self, node_index: TGNId) -> &TransitionGraphNode {
    debug_assert!(
      self.graph_nodes[node_index].id != TGNId::Invalid,
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
  pub fn get_peek_origin(&self, node_index: TGNId) -> &TransitionGraphNode {
    let node = &self.graph_nodes[node_index];

    debug_assert!(
      node.id != TGNId::Invalid,
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
  pub fn get_node(&self, node_index: TGNId) -> &TransitionGraphNode {
    debug_assert!(
      self.graph_nodes[node_index].id != TGNId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    &self.graph_nodes[node_index]
  }

  #[inline(always)]

  pub fn get_node_mut(&mut self, node_index: TGNId) -> &mut TransitionGraphNode {
    debug_assert!(
      self.graph_nodes[node_index].id != TGNId::Invalid,
      "Invalid access of a deleted node at index {}",
      node_index
    );

    &mut self.graph_nodes[node_index]
  }

  pub fn clear_peek_data(&mut self) {
    self.peek_ids.clear();
  }

  pub fn nodes_iter(&self) -> core::slice::Iter<TransitionGraphNode> {
    self.graph_nodes.iter()
  }

  pub fn clean(self) -> TPackResults {
    (
      TransitionPack {
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

  pub fn print_nodes(&self) {
    for node in &self.graph_nodes {
      if !node.is_orphan(self) || node.id.0 == 0 {
        println!("{}\n", node.debug_string(&self.g));
      }
    }
  }
}
