use bitmask_enum::bitmask;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::rc::Rc;
use std::vec;

use crate::grammar::get_production;

use super::GrammarStore;
use super::Item;
use super::ProductionId;
use super::SymbolID;

pub type TransitionGraphNodeId = usize;

#[bitmask]

pub enum TransitionStateType
{
  UNDEFINED,
  START,
  /// Transition has occurred from
  /// the consumption of a terminal
  /// symbol. All transition should
  /// have this set except for the
  /// initial state and the goal state.
  O_ASSERT,
  /// Transition has occurred from the
  /// completion of non-terminal symbol.
  O_PRODUCTION,
  /// Node represents a branch of one or
  /// more sub-nodes. Each sub-node should
  /// should be gated by an assert, peek, or
  /// consume verification instruction.
  MULTI,
  /// Transition has occurred from the
  /// accepting of a completed root item.
  ACCEPT,

  /// Transition has occurred from the
  /// accepting of a completed root item.
  AMBIGUOUS,

  /// State includes items out of the scope of the current
  /// production that should be used for disambiguating states
  /// that would cause a reduction to a production ID other than
  /// the current production.
  I_OUT_OF_SCOPE,
  /// Consumption of tokens is not allowed
  O_PEEK,

  I_FORK,

  /// Transition has occurred from the
  /// accepting of a root item.
  I_END,

  /// The current state represents a completed
  /// production. Used by scanner to determine
  /// when to apply token assignments
  COMPLETED,

  O_GOTO,

  LOOP,

  I_GOTO_START,

  I_DESCENT_START,

  /// Indicates this node consumes its symbol as a token.
  I_CONSUME,

  I_SCANNER,

  I_PASS,

  I_FAIL,

  I_TEST,

  I_PEEK_ROOT,

  /// This state is set when the nodes item has a skipped symbol
  /// that occludes another item that consumes that symbol.
  I_SKIPPED_COLLISION,

  I_COMPLETE,
  O_TERMINAL,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct TransitionGraphNode
{
  /// The symbols that lead to the
  /// transition to this state.
  pub sym: SymbolID,
  pub transition_type: TransitionStateType,
  pub items: Vec<Item>,
  pub parent: TransitionGraphNodeId,
  pub goal: TransitionGraphNodeId,
  pub proxy_parents: Vec<usize>,
  pub depth: u32,
  pub id: usize,
}

impl TransitionGraphNode
{
  pub const OrphanIndex: usize = usize::MAX;

  pub fn new(
    t_pack: &TransitionPack,
    sym: SymbolID,
    parent_index: usize,
    items: Vec<Item>,
  ) -> Self
  {
    let mut node = TransitionGraphNode {
      sym,
      transition_type: TransitionStateType::UNDEFINED,
      proxy_parents: vec![],
      items,
      depth: 0,
      parent: TransitionGraphNode::OrphanIndex,
      goal: TransitionGraphNode::OrphanIndex,
      id: TransitionGraphNode::OrphanIndex,
    };

    if t_pack.nodes.len() > parent_index {
      let parent = &t_pack.nodes[parent_index];

      node.parent = parent_index;

      node.depth = parent.depth + 1;

      node.goal = parent.goal;
    }

    node
  }

  pub fn temp(
    origin: &TransitionGraphNode,
    sym: SymbolID,
    parent_index: usize,
    items: Vec<Item>,
  ) -> Self
  {
    TransitionGraphNode {
      sym,
      transition_type: TransitionStateType::UNDEFINED,
      proxy_parents: vec![],
      items,
      depth: 0,
      parent: TransitionGraphNode::OrphanIndex,
      goal: TransitionGraphNode::OrphanIndex,
      id: origin.id * 100000,
    }
  }

  #[inline(always)]

  pub fn is_orphan(&self, tpack: &TransitionPack) -> bool
  {
    !self.has_parent(tpack)
  }

  #[inline(always)]
  pub fn has_parent(&self, tpack: &TransitionPack) -> bool
  {
    self.parent != Self::OrphanIndex
      && self.id != Self::OrphanIndex
      && self.parent < tpack.nodes.len()
  }

  #[inline(always)]
  pub fn is(&self, transition_type: TransitionStateType) -> bool
  {
    self.transition_type.intersects(transition_type)
  }

  #[inline(always)]
  pub fn set_type(&mut self, transition_type: TransitionStateType)
  {
    self.transition_type |= transition_type
  }

  #[inline(always)]
  pub fn unset_type(&mut self, transition_type: TransitionStateType)
  {
    self.transition_type &= self.transition_type ^ transition_type
  }
}

#[derive(PartialEq, Eq, Debug)]

pub enum TransitionMode
{
  RecursiveDescent,
  GoTo,
}

#[derive(Debug)]

pub struct TransitionPack<'a>
{
  /// A set of closures that can be referenced in peek states.
  pub scoped_closures: Vec<&'a [Item]>,
  /// For a givin item, points to an originating
  /// item that can used to look up it's own closure
  closure_links: HashMap<Item, Item>,
  pub goto_items: HashSet<Item>,
  nodes: Vec<TransitionGraphNode>,
  pub leaf_nodes: Vec<TransitionGraphNodeId>,
  pub mode: TransitionMode,
  pub is_scanner: bool,
  /// Internal pipeline to drive transition tree
  /// creation.
  node_pipeline: VecDeque<TransitionGraphNodeId>,
  ////
  /// Stores indices of pruned node slots that can be reused
  empty_cache: VecDeque<usize>,
  pub goto_scoped_closure: Option<Rc<Box<Vec<Item>>>>,
  pub root_productions: BTreeSet<ProductionId>,
  pub peek_ids: HashSet<u64>,
}

impl<'a> TransitionPack<'a>
{
  pub fn queue_node(&mut self, node_index: usize)
  {
    if node_index >= self.nodes.len() {
      panic!(
        "Cannot queue a TransitionNode that has not been added to the TransitionPack"
      )
    } else if self.get_node(node_index).id >= self.nodes.len() {
      panic!("Cannot queue a TransitionNode that has been pruned from the TransitionPack")
    } else {
      self.node_pipeline.push_back(node_index)
    }
  }

  #[inline(always)]
  pub fn get_next_queued(&mut self) -> Option<usize>
  {
    self.node_pipeline.pop_front()
  }

  pub fn new(
    grammar: &GrammarStore,
    mode: TransitionMode,
    is_scanner: bool,
    start_items: &[Item],
  ) -> Self
  {
    TransitionPack {
      scoped_closures: Vec::new(),
      closure_links: HashMap::new(),
      goto_items: HashSet::new(),
      nodes: Vec::new(),
      leaf_nodes: Vec::new(),
      node_pipeline: VecDeque::with_capacity(32),
      empty_cache: VecDeque::with_capacity(16),
      peek_ids: HashSet::new(),
      mode,
      is_scanner,
      goto_scoped_closure: None,
      root_productions: start_items
        .iter()
        .map(|i| i.get_production_id(grammar))
        .collect::<BTreeSet<_>>(),
    }
  }

  pub fn insert_node(&mut self, mut node: TransitionGraphNode) -> usize
  {
    if let Some(slot_index) = self.empty_cache.pop_front() {
      node.id = slot_index;

      self.nodes[slot_index] = node;

      slot_index
    } else {
      let id = self.nodes.len();

      node.id = id;

      self.nodes.push(node);

      id
    }
  }

  /// Removes the edge between this node and its parent, rendering
  /// it orphaned and available for destruction / reuse

  pub fn drop_node(&mut self, node_index: &usize) -> usize
  {
    

    let node_id;

    let parent;

    {
      let mut node = self.get_node_mut(*node_index);

      node_id = node.id;

      parent = node.parent;

      node.parent = TransitionGraphNode::OrphanIndex;

      node.id = TransitionGraphNode::OrphanIndex;
    }

    if node_id < self.nodes.len() {
      self.empty_cache.push_back(node_id);
    }

    parent
  }

  #[inline(always)]

  pub fn get_node(&self, node_index: usize) -> &TransitionGraphNode
  {
    if self.nodes[node_index].id == TransitionGraphNode::OrphanIndex {
      panic!("Invalid access of a deleted node at index {}", node_index)
    }

    &self.nodes[node_index]
  }

  #[inline(always)]

  pub fn get_node_mut(&mut self, node_index: usize) -> &mut TransitionGraphNode
  {
    if self.nodes[node_index].id == TransitionGraphNode::OrphanIndex {
      panic!("Invalid access of a deleted node at index {}", node_index)
    }

    &mut self.nodes[node_index]
  }

  #[inline(always)]
  pub fn get_closure_link(&self, i: &Item) -> Item
  {
    if let Some(link) = self.closure_links.get(i) {
      *link
    } else {
      Item::null(i.get_state())
    }
  }

  #[inline(always)]
  pub fn set_closure_link(&mut self, item_next: Item, item_prev: Item)
  {
    if item_next.get_state().get_group() != item_prev.get_state().get_group() {
      panic!("Incorrect linking of Items with differing states!");
    }
    if item_next != item_prev {
      self.closure_links.insert(item_next, item_prev);
    }
  }

  pub fn clear_peek_data(&mut self)
  {
    self.peek_ids.clear();
  }

  pub fn nodes_iter(&self) -> core::slice::Iter<TransitionGraphNode>
  {
    self.nodes.iter()
  }

  pub fn clean(self) -> Self
  {
    TransitionPack {
      peek_ids: HashSet::new(),
      scoped_closures: Vec::new(),
      closure_links: HashMap::new(),
      goto_items: self.goto_items,
      nodes: self.nodes,
      leaf_nodes: self.leaf_nodes,
      node_pipeline: VecDeque::new(),
      empty_cache: VecDeque::new(),
      mode: self.mode,
      is_scanner: self.is_scanner,
      goto_scoped_closure: None,
      root_productions: self.root_productions,
    }
  }

  pub fn get_node_len(&self) -> usize
  {
    self.nodes.len()
  }
}
