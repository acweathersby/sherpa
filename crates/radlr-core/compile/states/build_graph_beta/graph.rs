use crate::{
  compile::states::build_graph::graph::{GraphBuildState, GraphIdSubType, GraphType, Origin, PeekGroup, StateId, StateType},
  hash_id_value_u64,
  proxy::OrderedSet,
  types::*,
  Item,
};
use std::{
  collections::VecDeque,
  fmt::Debug,
  hash::{Hash, Hasher},
  sync::Arc,
};

use super::items::{get_follow_internal, FollowType};
#[derive(Debug)]
pub struct GraphNode {
  id: StateId,
  build_state: GraphBuildState,
  is_leaf: bool,
  ty: StateType,
  sym: PrecedentSymbol,
  sym_set_id: u64,
  hash_id: u64,
  lookahead_id: u64,
  pub(super) kernel: OrderedSet<Item>,
  reduce_item: Option<Item>,
  graph_type: GraphType,
  predecessor: Option<GraphNodeShared>,
}

pub type GraphNodeShared = std::sync::Arc<GraphNode>;

impl GraphNode {
  pub fn get_root(&self) -> &GraphNode {
    if self.is_root() {
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

fn get_state_symbols<'a>(builder: &mut ConcurrentGraphBuilder, node: &GraphNode) -> OrderedSet<PrecedentDBTerm> {
  let mode = node.graph_type();

  let mut symbols = OrderedSet::new();
  let db = builder.db_rc();
  let db = &db;

  for item in node.kernel_items().clone() {
    if let Some(sym) = item.precedent_db_key_at_sym(mode, db) {
      symbols.insert(sym);
    } else if item.is_nonterm(mode, db) {
      for item in db.get_closure(&item) {
        if let Some(sym) = item.precedent_db_key_at_sym(mode, db) {
          symbols.insert(sym);
        }
      }
    } else {
      let (follow, _) = get_follow_internal(builder, node, item.to_complete(), FollowType::AllItems);
      for item in follow {
        if let Some(sym) = item.precedent_db_key_at_sym(mode, db) {
          symbols.insert(sym);
        }
      }
    }
  }
  let syms = symbols.iter().map(|s| s.tok()).collect::<OrderedSet<_>>();

  symbols.extend(
    if node.ty.is_peek() {
      todo!("Handle peeked symbols")
    } else {
      node.kernel_items().iter().filter_map(|i| i.get_skipped(db)).flatten().collect::<Vec<_>>()
    }
    .into_iter()
    .filter_map(|s| {
      let id = s.tok_db_key().unwrap();
      (!syms.contains(&id)).then_some(Into::<PrecedentDBTerm>::into((id, 0, true)))
    }),
  );

  // insert skip symbols
  symbols
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
              let db = builder.db();
              println!("----LA {}", db.sym(term).debug_string(db));
              symbols.insert(term);
            } else if item.is_nonterm(mode, builder.db()) {
              for item in builder.db().get_closure(&item) {
                if let Some(term) = item.term_index_at_sym(node.graph_type, builder.db()) {
                  let db = builder.db();
                  println!("----LA {}", db.sym(term).debug_string(db));
                  symbols.insert(term);
                }
              }
            }
          }
        }
      }

      symbols.hash(&mut hasher);

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

pub enum PostNodeConstructorData {
  None,
  PeekInitialize { incomplete_items: Vec<TransitionPair>, completed_items: Vec<TransitionPair> },
}

pub type PostNodeConstructor =
  Box<dyn FnOnce(&GraphNodeShared, &mut ConcurrentGraphBuilder, PostNodeConstructorData) -> Vec<StagedNode>>;

pub type Finalizer = Box<dyn FnOnce(&mut GraphNode, &mut ConcurrentGraphBuilder, bool)>;

/// Temporary Represention of a graph node before goto transformations are
/// applied
pub struct StagedNode {
  node: GraphNode,
  /// Post processor that finalizes the configuration of this node, right before
  /// it is converted into a read-only node
  pnc_constructor: Option<PostNodeConstructor>,
  pnc_data: PostNodeConstructorData,
  finalizer: Option<Finalizer>,
  enqueued_leaf: bool,
  is_root: bool,
  allow_goto_increment: bool,
}

impl Debug for StagedNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("PreStagedNode");
    s.field("node", &self.node);
    s.finish()
  }
}

impl AsRef<GraphNode> for StagedNode {
  fn as_ref(&self) -> &GraphNode {
    &self.node
  }
}

impl StagedNode {
  pub fn new() -> Self {
    Self {
      node: GraphNode {
        build_state:  GraphBuildState::Normal,
        graph_type:   GraphType::Parser,
        hash_id:      0,
        id:           StateId::default(),
        is_leaf:      false,
        kernel:       Default::default(),
        lookahead_id: 0,
        predecessor:  None,
        reduce_item:  None,
        sym:          PrecedentSymbol::default(),
        sym_set_id:   0,
        ty:           StateType::Undefined,
      },
      allow_goto_increment: false,
      pnc_constructor: None,
      finalizer: None,
      pnc_data: PostNodeConstructorData::None,
      enqueued_leaf: false,
      is_root: false,
    }
  }

  pub fn set_reduce_item(mut self, item: Item) -> Self {
    self.node.reduce_item = Some(item);
    self
  }

  pub fn goto_inc(mut self) -> Self {
    self.allow_goto_increment = true;
    self
  }

  pub fn add_kernel_items<T: ItemContainerIter>(mut self, items: T) -> Self {
    self.node.kernel.extend(items);
    self
  }

  pub fn kernel_items(mut self, items: impl Iterator<Item = Item>) -> Self {
    self.node.kernel = OrderedSet::from_iter(items);
    self
  }

  pub fn set_kernel_items_mut(&mut self, items: impl Iterator<Item = Item>) {
    self.node.kernel = OrderedSet::from_iter(items);
  }

  pub fn build_state(mut self, build_state: GraphBuildState) -> Self {
    self.node.build_state = build_state;
    self
  }

  pub fn parent(mut self, parent: GraphNodeShared) -> Self {
    self.node.graph_type = parent.graph_type;
    self.node.predecessor = Some(parent);
    self
  }

  pub fn sym(mut self, sym: PrecedentSymbol) -> Self {
    self.node.sym = sym;
    self
  }

  pub fn make_enqueued_leaf(mut self) -> Self {
    self.node.is_leaf = true;
    self.enqueued_leaf = true;
    self
  }

  pub fn make_leaf(mut self) -> Self {
    self.node.is_leaf = true;
    self
  }

  pub fn make_root(mut self) -> Self {
    self.is_root = true;
    self
  }

  pub fn graph_ty(mut self, ty: GraphType) -> Self {
    self.node.graph_type = ty;
    self
  }

  pub fn ty(mut self, ty: StateType) -> Self {
    self.node.ty = ty;
    self
  }

  fn id(mut self, id: StateId) -> Self {
    self.node.id = id;
    self
  }

  pub fn pnc(mut self, pnc: PostNodeConstructor, pnc_data: PostNodeConstructorData) -> Self {
    debug_assert!(self.pnc_constructor.is_none(), "Expected finalizer to be None: This should only be set once");
    self.pnc_constructor = Some(pnc);
    self.pnc_data = pnc_data;
    self
  }

  pub fn finalizer(mut self, finalizer: Finalizer) -> Self {
    debug_assert!(self.pnc_constructor.is_none(), "Expected finalizer to be None: This should only be set once");
    self.finalizer = Some(finalizer);
    self
  }

  fn to_goto(self, state_id: StateId) -> StagedNode {
    Self {
      node: GraphNode {
        kernel: self
          .node
          .kernel
          .iter()
          .map(|i| if i.origin_state.0 == state_id.0 { i.as_goto_origin() } else { i.increment_goto() })
          .collect::<_>(),
        ..self.node
      },
      ..self
    }
  }

  pub fn commit(self, builder: &mut ConcurrentGraphBuilder) {
    if self.node.is_root() {
    } else {
      builder.pre_stage.push(self);
    }
  }
}

#[derive(Debug)]
pub struct ConcurrentGraphBuilder {
  queue: std::sync::Arc<std::sync::RwLock<VecDeque<GraphNodeShared>>>,
  root_states: std::sync::Arc<std::sync::RwLock<Map<u64, (GraphType, GraphNodeShared)>>>,
  state_nonterms: std::sync::Arc<std::sync::RwLock<Map<u64, ItemSet>>>,
  //states: std::sync::Arc<std::sync::RwLock<Map<u64, GraphNodeShared>>>,
  peek_resolve_states: std::sync::Arc<std::sync::RwLock<Map<u64, PeekGroup>>>,
  leaf_states: std::sync::Arc<std::sync::RwLock<Vec<GraphNodeShared>>>,
  predecessors: std::sync::Arc<std::sync::RwLock<Map<u64, Vec<GraphNodeShared>>>>,
  local_next: Option<GraphNodeShared>,
  symbol_sets: Map<u64, OrderedSet<PrecedentDBTerm>>,
  oos_roots: Map<DBNonTermKey, GraphNodeShared>,
  oos_closures: Map<Item, GraphNodeShared>,
  state_lookups: Map<u64, GraphNodeShared>,
  db: SharedParserDatabase,
  config: ParserConfig,
  pre_stage: Vec<StagedNode>,
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
      pre_stage: Default::default(),
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
      pre_stage: Default::default(),
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
    let mut items: std::collections::BTreeSet<Item> = ItemSet::default();

    for item in self.pre_stage.iter().map(|i| &i.node.kernel) {
      items.extend(item)
    }

    items
  }

  pub fn get_peek_resolve_items(&self, id: u64) -> PeekGroup {
    match self.peek_resolve_states.read() {
      Ok(peek) => peek.get(&id).cloned().unwrap(),
      Err(_) => panic!("queue has been poisoned"),
    }
  }

  pub fn pre_stage_state(&mut self, state: GraphNodeShared) {
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

  pub fn enqueue_state_for_processing_kernel(&mut self, state: GraphNodeShared) {
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

      let state = StagedNode::new()
        .id(id)
        .ty(StateType::_OosClosure_)
        .build_state(GraphBuildState::Normal)
        .kernel_items(closure)
        .sym(Default::default());

      let mut state = self.append_state_hashes(state.is_root, state.node);

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

      let mut state = StagedNode::new()
        .id(id)
        .ty(StateType::_OosClosure_)
        .build_state(GraphBuildState::Normal)
        .kernel_items(closure)
        .parent(self.get_state(origin).unwrap())
        .sym(Default::default());

      let mut state = self.append_state_hashes(state.is_root, state.node);

      state.hash_id = hash_id_value_u64(item);

      let state = Arc::new(state);

      self.state_lookups.insert(id.0 as u64, state.clone());
      self.oos_closures.insert(item, state);
      self.get_oos_closure_state(item)
    }
  }

  pub fn commit(&mut self, increment_goto: bool, state_id: StateId) {
    let mut nodes = self.pre_stage.drain(..).collect::<VecDeque<_>>();

    while let Some(StagedNode {
      node,
      pnc_constructor,
      pnc_data,
      finalizer,
      enqueued_leaf,
      is_root,
      allow_goto_increment,
    }) = nodes.pop_front()
    {
      let mut state = if increment_goto && allow_goto_increment {
        GraphNode {
          kernel: node
            .kernel
            .iter()
            .map(|i| if i.origin_state.0 == state_id.0 { i.as_goto_origin() } else { i.increment_goto() })
            .collect::<_>(),
          ..node
        }
      } else {
        node
      };

      if let Some(finalizer) = finalizer {
        finalizer(&mut state, self, increment_goto);
      }

      state = self.append_state_hashes(is_root, state);

      let key = state.hash_id;

      match self.predecessors.read() {
        Ok(preds) => {
          if preds.contains_key(&state.hash_id) {
            drop(preds);
            if let Some(parent) = state.parent() {
              match self.predecessors.write() {
                Ok(mut preds) => {
                  preds.entry(key).or_default().push(parent.clone());
                }
                Err(err) => panic!("{err}"),
              }
            }
          } else {
            drop(preds);
            match self.predecessors.write() {
              Ok(mut preds) => {
                if let Some(parent) = state.parent() {
                  preds.insert(key, vec![parent.clone()]);
                } else {
                  preds.insert(key, vec![]);
                }
              }
              Err(err) => panic!("{err}"),
            }
          }
        }
        Err(err) => panic!("{err}"),
      }

      if !state.is_scanner() {
        // Collect scanner information
        let symbols = get_state_symbols(self, &state);

        if symbols.len() > 0 {
          state.sym_set_id = hash_id_value_u64(&symbols);

          if !self.symbol_sets.contains_key(&state.sym_set_id) {
            // Build a scanner entry
            let db = self.db();

            let scanner_root = StagedNode::new()
              .kernel_items(symbols.iter().flat_map(|s| {
                ItemSet::start_items(db.token(s.tok()).nonterm_id, db).to_origin(Origin::TerminalGoal(s.tok(), s.precedence()))
              }))
              .ty(StateType::Start)
              .graph_ty(GraphType::Scanner)
              .make_root()
              .id(StateId(state.sym_set_id as usize, GraphIdSubType::Root));

            nodes.push_back(scanner_root);

            let db = self.db();
            for sym in &symbols {
              println!("---- {} {} {}", db.sym(sym.tok()).debug_string(db), sym.precedence(), sym.is_skipped())
            }

            self.symbol_sets.insert(state.sym_set_id, symbols);
          }
        }
      }

      let state = Arc::new(state);

      if let Some(finalizer) = pnc_constructor {
        nodes.extend(finalizer(&state, self, pnc_data));
      }

      match state.ty {
        StateType::Start => {
          match self.root_states.write() {
            Ok(mut root_state) => {
              root_state.insert(state.hash_id, (GraphType::Parser, state.clone()));
            }
            Err(err) => panic!("{err}"),
          }
          self.enqueue_state_for_processing_kernel(state.clone());
        }
        _ => {
          if state.is_leaf {
            match self.leaf_states.write() {
              Ok(mut leaf_states) => {
                leaf_states.push(state.clone());
              }
              Err(err) => panic!("{err}"),
            }

            if enqueued_leaf {
              self.enqueue_state_for_processing_kernel(state.clone());
            }
          } else {
            self.enqueue_state_for_processing_kernel(state.clone());
          }
        }
      }
    }
  }

  fn append_state_hashes(&mut self, is_root: bool, mut state: GraphNode) -> GraphNode {
    let lookahead =
      if is_root { 0 } else { create_lookahead_hash(self, &state, std::collections::hash_map::DefaultHasher::new()) };
    let state_hash = create_state_hash(&state, lookahead, std::collections::hash_map::DefaultHasher::new());

    state.hash_id = state_hash;
    state.lookahead_id = lookahead;
    state.id = StateId::new(state.hash_id as usize, if is_root { GraphIdSubType::Regular } else { GraphIdSubType::Root });
    state.kernel = state
      .kernel
      .into_iter()
      .map(|i| {
        if i.origin_state.is_invalid() {
          debug_assert!(!state.id.is_invalid());
          i.to_origin_state(state.id)
        } else {
          i
        }
      })
      .collect();

    state
  }
}
