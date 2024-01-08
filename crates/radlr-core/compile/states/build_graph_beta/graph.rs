use crate::{
  compile::states::build_graph::graph::{GraphBuildState, GraphIdSubType, GraphType, Origin, PeekGroup, StateId, StateType},
  hash_id_value_u64,
  proxy::OrderedSet,
  types::*,
  Item,
  RadlrResult,
};
use std::{
  collections::VecDeque,
  hash::{Hash, Hasher},
  sync::{Arc, RwLock},
};

use super::items::{get_follow_internal, FollowType};

pub struct GraphNode {
  id:           StateId,
  build_state:  GraphBuildState,
  is_leaf:      bool,
  ty:           StateType,
  sym:          PrecedentSymbol,
  sym_set_id:   u64,
  hash_id:      u64,
  lookahead_id: u64,
  kernel:       OrderedSet<Item>,
  reduce_item:  Option<Item>,
  graph_type:   GraphType,
  predecessor:  Option<GraphNodeShared>,
}

pub type GraphNodeShared = std::sync::Arc<GraphNode>;

impl GraphNode {
  pub fn get_root(&self) -> &GraphNode {
    if self.id.is_root() {
      self
    } else {
      self.predecessor.as_ref().unwrap().get_root()
    }
  }

  pub fn goal_items(&self) -> &ItemSet {
    self.get_root().kernel_items()
  }

  pub fn item_is_goal(&self, item: Item) -> bool {
    let root = self.get_root();
    root.kernel_items().iter().any(|i| i.to_canonical().to_complete() == item.to_canonical())
  }

  pub fn is_root(&self) -> bool {
    self.id.is_root()
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

  /*   pub fn add_predecessor(&self, predecessor: GraphNodeShared) -> RadlrResult<()> {
    match self.predecessors.write() {
      Err(err) => Err(err.into()),
      Ok(mut predecessors) => {
        predecessors.push(predecessor);
        Ok(())
      }
    }
  } */

  pub fn get_predecessor<'a>(&'a self, id: StateId) -> Option<&'a GraphNodeShared> {
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

  pub fn parent<'a>(&'a self) -> Option<&'a GraphNodeShared> {
    self.predecessor.as_ref()
  }
}

pub struct GraphNodeBuilder {
  state_id: StateId,
  build_state: GraphBuildState,
  ty: StateType,
  graph_type: GraphType,
  is_leaf: bool,
  is_enqueued_leaf: bool,
  sym: PrecedentSymbol,
  kernel: OrderedSet<Item>,
  reduce_item: Option<Item>,
  primary_predecessor: Option<GraphNodeShared>,
  resolved: bool,
}

impl GraphNodeBuilder {
  pub fn new() -> Self {
    Self {
      graph_type: GraphType::Parser,
      state_id: StateId::root(),
      build_state: GraphBuildState::Normal,
      ty: StateType::Follow,
      is_leaf: false,
      is_enqueued_leaf: false,
      sym: Default::default(),
      kernel: Default::default(),
      primary_predecessor: Default::default(),
      reduce_item: None,
      resolved: false,
    }
  }

  pub fn set_sym(mut self, sym: PrecedentSymbol) -> Self {
    self.sym = sym;
    self
  }

  pub fn make_enqueued_leaf(mut self) -> Self {
    self.is_leaf = true;
    self.is_enqueued_leaf = true;
    self
  }

  pub fn make_leaf(mut self) -> Self {
    self.is_leaf = true;
    self
  }

  pub fn set_type(mut self, ty: StateType) -> Self {
    self.ty = ty;
    self
  }

  pub fn set_id(mut self, id: StateId) -> Self {
    self.state_id = id;
    self
  }

  pub fn set_build_state(mut self, build_state: GraphBuildState) -> Self {
    self.build_state = build_state;
    self
  }

  pub fn set_parent(mut self, parent: GraphNodeShared) -> Self {
    self.graph_type = parent.graph_type;
    self.primary_predecessor = Some(parent);
    self
  }

  pub fn set_kernel_items(mut self, items: impl Iterator<Item = Item>) -> Self {
    let GraphNodeBuilder { kernel, .. } = self;
    self.kernel = OrderedSet::from_iter(items);
    self
  }

  pub fn set_kernel_items_mut(&mut self, items: impl Iterator<Item = Item>) {
    self.kernel = OrderedSet::from_iter(items);
  }

  pub fn get_kernel_items(&self) -> &ItemSet {
    &self.kernel
  }

  pub fn set_peek_resolve_state<T: Iterator<Item = Item>>(&mut self, items: T, is_oos: bool) -> Origin {
    let peek_group = PeekGroup { items: items.collect(), is_oos };

    let index = hash_id_value_u64(&peek_group) as u32;

    let GraphNodeBuilder { state_id, .. } = self;

    todo!("set_peek_resolve_state");

    Origin::Peek(index)
  }

  pub fn add_kernel_items<T: ItemContainerIter>(mut self, items: T) -> Self {
    self.kernel.extend(items);
    self
  }

  pub fn set_reduce_item(mut self, item: Item) -> Self {
    self.reduce_item = Some(item);
    self
  }

  pub fn to_local(self, builder: &mut ConcurrentGraphBuilder) -> GraphNode {
    let GraphNodeBuilder {
      state_id,
      build_state,
      ty,
      is_leaf,
      kernel,
      primary_predecessor,
      resolved,
      is_enqueued_leaf,
      sym,
      reduce_item,
      graph_type,
    } = self;

    let mut state = GraphNode {
      graph_type,
      hash_id: 0,
      sym_set_id: 0,
      lookahead_id: 0,
      sym,
      ty,
      reduce_item,
      build_state,
      id: state_id,
      is_leaf,
      kernel,
      predecessor: primary_predecessor,
    };

    let lookahead = create_lookahead_hash(builder, &state, std::collections::hash_map::DefaultHasher::new());
    let state_hash = create_state_hash(&state, lookahead, std::collections::hash_map::DefaultHasher::new());

    state.hash_id = state_hash;
    state.lookahead_id = lookahead;
    state.id = StateId::new(state.hash_id as usize, GraphIdSubType::Regular);

    state
  }

  pub fn commit(self, builder: &mut ConcurrentGraphBuilder) -> RadlrResult<GraphNodeShared> {
    let enqueued_leaf = self.is_enqueued_leaf;
    let state = self.to_local(builder);

    let state = Arc::new(state);

    match state.ty {
      StateType::Start => {
        match builder.root_states.write() {
          Ok(mut root_state) => {
            root_state.insert(state.hash_id, (GraphType::Parser, state.clone()));
          }
          Err(err) => return Err(err.into()),
        }
        builder.enqueue_state(state.clone());
        Ok(state)
      }
      _ => {
        if state.is_leaf {
          match builder.leaf_states.write() {
            Ok(mut leaf_states) => {
              leaf_states.push(state.clone());
            }
            Err(err) => return Err(err.into()),
          }

          if enqueued_leaf {
            builder.enqueue_state(state.clone());
          }

          Ok(state)
        } else {
          builder.enqueue_state(state.clone());
          Ok(state)
        }
      }
    }
  }

  pub fn to_pending(self, builder: &mut ConcurrentGraphBuilder) {
    builder.pending.push(self);
  }
}

fn create_lookahead_hash<'a, H: std::hash::Hasher>(builder: &mut ConcurrentGraphBuilder, node: &GraphNode, mut hasher: H) -> u64 {
  match node.graph_type {
    GraphType::Parser => {
      let mode = GraphType::Parser;

      let mut symbols = OrderedSet::new();
      for item in node.kernel_items() {
        {
          let (follow, _) = get_follow_internal(builder, node, item.to_complete(), FollowType::AllItems);
          for item in follow {
            if let Some(term) = item.term_index_at_sym(mode, builder.db()) {
              symbols.insert(term);
            } else if item.is_nonterm(mode, builder.db()) {
              for item in builder.db().get_closure(&item) {
                if let Some(term) = item.term_index_at_sym(node.graph_type, builder.db()) {
                  symbols.insert(term);
                }
              }
            }
          }
        }
      }
      hasher.finish()
    }
    GraphType::Scanner => hasher.finish(),
  }
}

fn create_state_hash<'a, H: std::hash::Hasher>(state: &GraphNode, lookahead: u64, mut hasher: H) -> u64 {
  let hasher = &mut hasher;

  match state.ty {
    StateType::Peek(_) => "peek".hash(hasher),
    _ => state.ty.hash(hasher),
  };

  state.sym.hash(hasher);

  for item in state.kernel_items() {
    item.index().hash(hasher);
    item.origin.hash(hasher);
    item.from.hash(hasher);
    item.from_goto_origin.hash(hasher);
    item.goto_distance.hash(hasher);
  }

  lookahead.hash(hasher);

  hasher.finish()
}

pub struct ConcurrentGraphBuilder {
  queue: std::sync::Arc<std::sync::RwLock<VecDeque<GraphNodeShared>>>,
  root_states: std::sync::Arc<std::sync::RwLock<Map<u64, (GraphType, GraphNodeShared)>>>,
  state_nonterms: std::sync::Arc<std::sync::RwLock<Map<u64, ItemSet>>>,
  //states: std::sync::Arc<std::sync::RwLock<Map<u64, GraphNodeShared>>>,
  peek_resolve_states: std::sync::Arc<std::sync::RwLock<Map<u64, PeekGroup>>>,
  leaf_states: std::sync::Arc<std::sync::RwLock<Vec<GraphNodeShared>>>,
  predecessors: std::sync::Arc<std::sync::RwLock<Map<u64, Vec<GraphNodeShared>>>>,
  local_next: Option<GraphNodeShared>,
  symbol_sets: Map<u64, OrderedSet<SymbolId>>,
  oos_roots: Map<DBNonTermKey, GraphNodeShared>,
  oos_closures: Map<Item, GraphNodeShared>,
  state_lookups: Map<u64, GraphNodeShared>,
  db: SharedParserDatabase,
  config: ParserConfig,
  pending: Vec<GraphNodeBuilder>,
}

impl Clone for ConcurrentGraphBuilder {
  fn clone(&self) -> Self {
    Self {
      queue: self.queue.clone(),
      root_states: self.root_states.clone(),
      state_nonterms: self.state_nonterms.clone(),
      //states: self.states.clone(),
      peek_resolve_states: self.peek_resolve_states.clone(),
      leaf_states: self.leaf_states.clone(),
      local_next: self.local_next.clone(),
      db: self.db.clone(),
      predecessors: self.predecessors.clone(),
      symbol_sets: self.symbol_sets.clone(),
      config: self.config.clone(),
      oos_closures: Default::default(),
      oos_roots: Default::default(),
      state_lookups: Default::default(),
      pending: Default::default(),
    }
  }
}

impl ConcurrentGraphBuilder {
  pub fn new(db: SharedParserDatabase, config: ParserConfig) -> Self {
    ConcurrentGraphBuilder {
      db,
      config,
      predecessors: Default::default(),
      queue: Default::default(),
      root_states: Default::default(),
      state_nonterms: Default::default(),
      peek_resolve_states: Default::default(),
      leaf_states: Default::default(),
      local_next: Default::default(),
      symbol_sets: Default::default(),
      oos_roots: Default::default(),
      oos_closures: Default::default(),
      state_lookups: Default::default(),
      pending: Default::default(),
    }
  }

  pub fn set_peek_resolve_state<T: Iterator<Item = Item>>(&mut self, items: T, is_oos: bool) -> Origin {
    let peek_group = PeekGroup { items: items.collect(), is_oos };

    let index = hash_id_value_u64(&peek_group) as u32;

    match self.peek_resolve_states.write() {
      Ok(mut peek_resolve_states) => {
        peek_resolve_states.insert(index as u64, peek_group);
      }
      Err(err) => panic!("{err}"),
    }

    Origin::Peek(index)
  }

  pub fn get_peek_resolve_state<T: Iterator<Item = Item>>(&self, id: u32) -> Option<PeekGroup> {
    match self.peek_resolve_states.read() {
      Ok(peek_resolve_states) => peek_resolve_states.get(&(id as u64)).cloned(),
      Err(err) => panic!("{err}"),
    }
  }

  pub fn set_nonterm_items(&mut self, id: u64, nonterms: ItemSet) {
    match self.state_nonterms.write() {
      Ok(mut nt) => {
        nt.insert(id, nonterms);
      }
      Err(_) => panic!("queue has been poisoned"),
    }
  }

  pub fn declare_recursive_peek_error(&self) {
    todo!("Handle this");
  }

  pub fn get_pending_items(&self) -> ItemSet {
    let mut items = ItemSet::default();
    for state in &self.pending {
      if state.ty.is_peek() {
        todo!("Get peek resolve items")
      } else {
        items.extend(state.kernel.iter().cloned())
      }
    }
    items
  }

  pub fn iter_pending_states_mut(&mut self, closure: &mut dyn FnMut(&mut GraphNodeBuilder)) {
    for state in &mut self.pending {
      closure(state)
    }
  }

  pub fn process_pending(&mut self) {
    for pending in self.pending.drain(..).collect::<Vec<_>>() {
      pending.commit(self);
    }
  }

  pub fn get_peek_resolve_items(&self, id: u64) -> PeekGroup {
    match self.peek_resolve_states.read() {
      Ok(peek) => peek.get(&id).cloned().unwrap(),
      Err(_) => panic!("queue has been poisoned"),
    }
  }

  pub fn enqueue_state(&mut self, state: GraphNodeShared) {
    if self.local_next.is_none() {
      {
        self.local_next = Some(state);
      }
    } else {
      match self.queue.write() {
        Ok(mut queue) => {
          queue.push_back(state);
        }
        Err(_) => panic!("queue has been poisoned"),
      }
    }
  }

  pub fn get_local_work(&mut self) -> Option<GraphNodeShared> {
    self.local_next.take()
  }

  pub fn get_global_work(&mut self) -> Option<GraphNodeShared> {
    match self.queue.write() {
      Ok(mut queue) => queue.pop_front(),
      Err(_) => panic!("queue has been poisoned"),
    }
  }

  pub fn db(&self) -> &ParserDatabase {
    &self.db
  }

  pub fn db_rc(&self) -> SharedParserDatabase {
    self.db.clone()
  }

  pub fn config(&self) -> &ParserConfig {
    &self.config
  }

  pub fn get_state(&self, state: u64) -> Option<GraphNodeShared> {
    self.state_lookups.get(&state).cloned()
  }

  /// Creates or returns a state whose kernel items is the FOLLOW closure of the
  /// givin non-terminal, that is all items that are `_  = b A • b` for some
  /// non-terminal `A`
  pub fn get_oos_root_state(&mut self, nterm: DBNonTermKey) -> GraphNodeShared {
    if let Some(state_id) = self.oos_roots.get(&nterm) {
      state_id.clone()
    } else {
      let id = StateId::new(self.oos_roots.len(), GraphIdSubType::ExtendedClosure);

      let item_id = StateId::new(0, GraphIdSubType::ExtendedClosure);

      let closure = self
        .db()
        .nonterm_follow_items(nterm)
        .map(|i| i.to_origin(Origin::__OOS_CLOSURE__).to_origin_state(item_id))
        .filter_map(|i| i.increment());

      let state = GraphNodeBuilder::new()
        .set_id(id)
        .set_type(StateType::_OosClosure_)
        .set_build_state(GraphBuildState::Normal)
        .add_kernel_items(closure)
        .set_sym(Default::default());

      let mut state = state.to_local(self);

      state.hash_id = hash_id_value_u64(nterm);

      let state = Arc::new(state);

      self.state_lookups.insert(id.0 as u64, state.clone());
      self.oos_roots.insert(nterm, state);
      self.get_oos_root_state(nterm)
    }
  }

  pub fn get_oos_closure_state(&mut self, item: Item) -> GraphNodeShared {
    debug_assert!(item.origin_state.is_oos());

    let state = item.origin_state;

    let item = item.to_canonical().to_origin_state(state);

    if let Some(node) = self.oos_closures.get(&item) {
      node.clone()
    } else {
      let id = StateId::new(self.oos_closures.len(), GraphIdSubType::ExtendedClosure);

      let kernel = item.to_origin_state(id).to_origin(Origin::__OOS_CLOSURE__);

      let closure = kernel.closure_iter_align(kernel, self.db());

      let origin = item.origin_state.0 as u64;

      let mut state = GraphNodeBuilder::new()
        .set_id(id)
        .set_type(StateType::_OosClosure_)
        .set_build_state(GraphBuildState::Normal)
        .add_kernel_items(closure)
        .set_parent(self.get_state(origin).unwrap())
        .set_sym(Default::default())
        .to_local(self);

      state.hash_id = hash_id_value_u64(item);

      let state = Arc::new(state);

      self.state_lookups.insert(id.0 as u64, state.clone());
      self.oos_closures.insert(item, state);
      self.get_oos_closure_state(item)
    }
  }
}
