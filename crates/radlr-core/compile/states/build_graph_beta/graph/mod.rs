use super::items::{get_follow_internal, FollowType};
use crate::{
  compile::states::build_graph::graph::{GraphBuildState, GraphIdSubType, GraphType, Origin, PeekGroup, StateId, StateType},
  hash_id_value_u64,
  proxy::OrderedSet,
  types::*,
  Item,
};
pub use node::*;
pub use scanner::*;
use std::{
  collections::{BTreeMap, HashSet, VecDeque},
  fmt::Debug,
  hash::{Hash, Hasher},
  sync::Arc,
};

mod node;
mod scanner;

fn get_state_symbols<'a>(builder: &mut ConcurrentGraphBuilder, node: &GraphNode) -> Option<ScannerData> {
  let mode = node.graph_type();

  let db = builder.db_rc();
  let db = &db;

  let mut scanner_data = ScannerData { hash: 0, ..Default::default() };

  for item in node.kernel_items().clone() {
    if let Some(sym) = item.precedent_db_key_at_sym(mode, db) {
      scanner_data.symbols.insert(sym);
    } else if item.is_nonterm(mode, db) {
      for item in db.get_closure(&item) {
        if let Some(sym) = item.precedent_db_key_at_sym(mode, db) {
          scanner_data.symbols.insert(sym);
        }
      }
    } else {
      let (follow, _) = get_follow_internal(builder, node, item.to_complete(), FollowType::AllItems);
      for item in follow {
        if let Some(sym) = item.precedent_db_key_at_sym(mode, db) {
          scanner_data.follow.insert(sym);
        }
      }
    }
  }
  let syms = scanner_data.symbols.iter().map(|s| s.tok()).collect::<OrderedSet<_>>();

  scanner_data.skipped.extend(
    if node.ty.is_peek() {
      todo!("Handle peeked symbols")
    } else {
      node.kernel_items().iter().filter_map(|i| i.get_skipped(db)).flatten().collect::<Vec<_>>()
    }
    .into_iter()
    .filter_map(|s| {
      let id = s.tok_db_key().unwrap();
      (!syms.contains(&id)).then_some(id)
    }),
  );

  if scanner_data.follow.is_empty() && scanner_data.symbols.is_empty() {
    None
  } else {
    let hash = hash_id_value_u64((&scanner_data.skipped, &scanner_data.symbols, &scanner_data.follow));
    scanner_data.hash = hash;
    Some(scanner_data)
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
              let db = builder.db();
              symbols.insert(term);
            } else if item.is_nonterm(mode, builder.db()) {
              for item in builder.db().get_closure(&item) {
                if let Some(term) = item.term_index_at_sym(node.graph_type, builder.db()) {
                  let db = builder.db();
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
}

pub type PostNodeConstructor =
  Box<dyn FnOnce(&SharedGraphNode, &mut ConcurrentGraphBuilder, PostNodeConstructorData) -> Vec<StagedNode>>;

pub type Finalizer = Box<dyn FnOnce(&mut GraphNode, &mut ConcurrentGraphBuilder, bool)>;

/// Temporary Represention of a graph node before goto transformations are
/// applied
pub struct StagedNode {
  node:                 GraphNode,
  /// Post processor that finalizes the configuration of this node, right before
  /// it is converted into a read-only node
  pnc_constructor:      Option<PostNodeConstructor>,
  pnc_data:             PostNodeConstructorData,
  finalizer:            Option<Finalizer>,
  enqueued_leaf:        bool,
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
  pub fn new(gb: &ConcurrentGraphBuilder) -> Self {
    Self {
      node:                 GraphNode {
        build_state: GraphBuildState::Normal,
        graph_type:  GraphType::Parser,
        hash_id:     0,
        id:          StateId::default(),
        is_leaf:     false,
        kernel:      Default::default(),
        // lookahead_id: 0,
        predecessor: None,
        reduce_item: None,
        sym:         PrecedentSymbol::default(),
        ty:          StateType::Undefined,
        symbol_set:  None,
        db:          gb.db_rc().clone(),
        is_goto:     false,
        root_data:   RootData {
          db_key:    DBNonTermKey::default(),
          is_root:   false,
          root_name: Default::default(),
        },
      },
      allow_goto_increment: false,
      pnc_constructor:      None,
      finalizer:            None,
      pnc_data:             PostNodeConstructorData::None,
      enqueued_leaf:        false,
    }
  }

  pub fn set_reduce_item(mut self, item: Item) -> Self {
    self.node.reduce_item = Some(item.index());
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

  pub fn parent(mut self, parent: SharedGraphNode) -> Self {
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

  pub fn make_root(mut self, root_name: IString, db_key: DBNonTermKey) -> Self {
    self.node.root_data = RootData { db_key, is_root: true, root_name };
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
      builder.pre_stage.push(self);
    } else {
      builder.pre_stage.push(self);
    }
  }
}

type SharedRW<T> = std::sync::Arc<std::sync::RwLock<T>>;

type RootStateData = (GraphType, SharedGraphNode, ParserConfig);

type RootStates = Map<u64, RootStateData>;

#[derive(Debug)]
pub struct ConcurrentGraphBuilder {
  queue:          SharedRW<VecDeque<(SharedGraphNode, ParserConfig)>>,
  poisoned:       SharedRW<Set<DBNonTermKey>>,
  root_states:    SharedRW<RootStates>,
  state_nonterms: SharedRW<Map<u64, ItemSet>>,
  peek_resolves:  SharedRW<Map<u64, PeekGroup>>,
  leaf_states:    SharedRW<Vec<SharedGraphNode>>,
  predecessors:   SharedRW<Map<u64, Vec<SharedGraphNode>>>,
  local_next:     Option<(SharedGraphNode, ParserConfig)>,
  symbol_sets:    Map<u64, Arc<ScannerData>>,
  oos_roots:      Map<DBNonTermKey, SharedGraphNode>,
  oos_closures:   Map<Item, SharedGraphNode>,
  state_lookups:  Map<u64, SharedGraphNode>,
  db:             SharedParserDatabase,
  pre_stage:      Vec<StagedNode>,
}

unsafe impl Send for ConcurrentGraphBuilder {}
unsafe impl Sync for ConcurrentGraphBuilder {}

impl Clone for ConcurrentGraphBuilder {
  fn clone(&self) -> Self {
    Self {
      queue:          self.queue.clone(),
      poisoned:       self.poisoned.clone(),
      root_states:    self.root_states.clone(),
      state_nonterms: self.state_nonterms.clone(),
      peek_resolves:  self.peek_resolves.clone(),
      leaf_states:    self.leaf_states.clone(),
      local_next:     self.local_next.clone(),
      db:             self.db.clone(),
      predecessors:   self.predecessors.clone(),
      symbol_sets:    self.symbol_sets.clone(),
      oos_closures:   Default::default(),
      oos_roots:      Default::default(),
      state_lookups:  Default::default(),
      pre_stage:      Default::default(),
    }
  }
}

impl ConcurrentGraphBuilder {
  pub fn new(db: SharedParserDatabase, config: ParserConfig) -> Self {
    ConcurrentGraphBuilder {
      db,
      poisoned: Default::default(),
      predecessors: Default::default(),
      queue: Default::default(),
      root_states: Default::default(),
      state_nonterms: Default::default(),
      peek_resolves: Default::default(),
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

    match self.peek_resolves.write() {
      Ok(mut peek_resolve_states) => {
        peek_resolve_states.insert(index as u64, peek_group);
      }
      Err(err) => panic!("{err}"),
    }

    Origin::Peek(index)
  }

  pub fn get_peek_resolve_state<T: Iterator<Item = Item>>(&self, id: u32) -> Option<PeekGroup> {
    match self.peek_resolves.read() {
      Ok(peek_resolve_states) => peek_resolve_states.get(&(id as u64)).cloned(),
      Err(err) => panic!("{err}"),
    }
  }

  pub fn poison_nonterminal(&mut self, nonterm_key: DBNonTermKey) -> RadlrResult<()> {
    match self.poisoned.write() {
      Ok(mut nt) => {
        nt.insert(nonterm_key);
        Ok(())
      }
      Err(err) => Err(err.into()),
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
    match self.peek_resolves.read() {
      Ok(peek) => peek.get(&id).cloned().unwrap(),
      Err(_) => panic!("queue has been poisoned"),
    }
  }

  pub fn pre_stage_state(&mut self, state: SharedGraphNode, parser_config: ParserConfig) {
    self.enqueue_state_for_processing_kernel(state, parser_config, true);
  }

  pub fn enqueue_state_for_processing_kernel(&mut self, state: SharedGraphNode, parser_config: ParserConfig, allow_local: bool) {
    if self.local_next.is_none() && allow_local {
      {
        self.local_next = Some((state, parser_config));
      }
    } else {
      match self.queue.write() {
        Ok(mut queue) => {
          queue.push_back((state, parser_config));
        }
        Err(_) => panic!("queue has been poisoned"),
      }
    }
  }

  pub fn get_local_work(&mut self) -> Option<(SharedGraphNode, ParserConfig)> {
    self.local_next.take()
  }

  pub fn get_global_work(&mut self) -> Option<(SharedGraphNode, ParserConfig)> {
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

  pub fn get_state(&self, state: u64) -> Option<SharedGraphNode> {
    self.state_lookups.get(&state).cloned()
  }

  /// Creates or returns a state whose kernel items is the FOLLOW closure of the
  /// givin non-terminal, that is all items that are `_  = b A • b` for some
  /// non-terminal `A`
  pub fn get_oos_root_state(&mut self, nterm: DBNonTermKey) -> SharedGraphNode {
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

      let pending = StagedNode::new(&self)
        .id(id)
        .ty(StateType::_OosClosure_)
        .build_state(GraphBuildState::Normal)
        .kernel_items(closure)
        .sym(Default::default());

      let mut state = self.append_state_hashes(pending.node.is_root(), pending.node);

      state.hash_id = hash_id_value_u64(nterm);

      let state = Arc::new(state);

      self.state_lookups.insert(id.0 as u64, state.clone());
      self.oos_roots.insert(nterm, state);
      self.get_oos_root_state(nterm)
    }
  }

  pub fn get_oos_closure_state(&mut self, item: Item) -> SharedGraphNode {
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

      let pending = StagedNode::new(&self)
        .id(id)
        .ty(StateType::_OosClosure_)
        .build_state(GraphBuildState::Normal)
        .kernel_items(closure)
        .parent(self.get_state(origin).unwrap())
        .sym(Default::default());

      let mut state = self.append_state_hashes(pending.node.is_root(), pending.node);

      state.hash_id = hash_id_value_u64(item);

      let state = Arc::new(state);

      self.state_lookups.insert(id.0 as u64, state.clone());
      self.oos_closures.insert(item, state);
      self.get_oos_closure_state(item)
    }
  }

  pub fn commit(
    &mut self,
    increment_goto: bool,
    pred: Option<&SharedGraphNode>,
    parser_config: &ParserConfig,
    allow_local_queueing: bool,
    force: bool,
  ) -> RadlrResult<u32> {
    let mut nodes = self.pre_stage.drain(..).collect::<VecDeque<_>>();

    let mut queued = 0;

    let pred_id = pred.map(|d| d.id).unwrap_or_default().0;

    while let Some(StagedNode {
      node,
      pnc_constructor,
      pnc_data,
      finalizer,
      enqueued_leaf,
      allow_goto_increment,
    }) = nodes.pop_front()
    {
      let mut state = if increment_goto && allow_goto_increment {
        GraphNode {
          kernel: node
            .kernel
            .iter()
            .map(|i| if i.origin_state.0 == pred_id { i.as_goto_origin() } else { i.increment_goto() })
            .collect::<_>(),
          ..node
        }
      } else {
        node
      };

      if let Some(finalizer) = finalizer {
        finalizer(&mut state, self, increment_goto);
      }

      if !state.root_data.is_root {
        state.root_data = pred.expect("Non-root states should have a predecessor").root_data;
        state.root_data.is_root = false;
      }

      state = self.append_state_hashes(state.root_data.is_root, state);

      let key = state.hash_id;

      if !state.is_scanner() {
        if let Some(scanner_data) = get_state_symbols(self, &state) {
          let sym_set_id = scanner_data.hash;

          if !self.symbol_sets.contains_key(&scanner_data.hash) {
            // Build a scanner entry
            let db = self.db();

            let start_items = scanner_data
              .symbols
              .iter()
              .chain(scanner_data.follow.iter())
              .map(|s| {
                ItemSet::start_items(db.token(s.tok()).nonterm_id, db).to_origin(Origin::TerminalGoal(s.tok(), s.precedence()))
              })
              .chain(
                scanner_data
                  .skipped
                  .iter()
                  .map(|s| ItemSet::start_items(db.token(*s).nonterm_id, db).to_origin(Origin::TerminalGoal(*s, 0))),
              )
              .flatten();

            let scanner_root = StagedNode::new(self)
              .kernel_items(start_items)
              .ty(StateType::Start)
              .graph_ty(GraphType::Scanner)
              .make_root(scanner_data.create_scanner_name(db), DBNonTermKey::default())
              .id(StateId(sym_set_id as usize, GraphIdSubType::Root));

            nodes.push_back(scanner_root);

            self.symbol_sets.insert(sym_set_id, Arc::new(scanner_data));
          }

          state.symbol_set = self.symbol_sets.get(&sym_set_id).cloned();
        }
      }

      let already_commited = match self.predecessors.read() {
        Ok(preds) => {
          if preds.contains_key(&state.hash_id) {
            drop(preds);
            if let Some(parent) = state.parent() {
              match self.predecessors.write() {
                Ok(mut preds) => {
                  preds.entry(key).or_default().push(parent.clone());
                }
                Err(err) => return Err(err.into()),
              }
            }
            true
          } else {
            drop(preds);
            if let Some(parent) = state.parent() {
              match self.predecessors.write() {
                Ok(mut preds) => {
                  preds.entry(key).or_default().push(parent.clone());
                }
                Err(err) => return Err(err.into()),
              }
            }

            false
          }
        }
        Err(err) => return Err(err.into()),
      };

      if !already_commited || force {
        let state = Arc::new(state);

        if let Some(finalizer) = pnc_constructor {
          nodes.extend(finalizer(&state, self, pnc_data));
        }

        match state.ty {
          StateType::Start => {
            match self.root_states.write() {
              Ok(mut root_state) => {
                root_state.insert(state.hash_id, (GraphType::Parser, state.clone(), *parser_config));
              }
              Err(err) => return Err(err.into()),
            }
            self.enqueue_state_for_processing_kernel(state.clone(), *parser_config, allow_local_queueing);
          }
          _ => {
            if state.is_leaf {
              match self.leaf_states.write() {
                Ok(mut leaf_states) => {
                  leaf_states.push(state.clone());
                }
                Err(err) => return Err(err.into()),
              }

              if enqueued_leaf {
                self.enqueue_state_for_processing_kernel(state.clone(), *parser_config, allow_local_queueing);
              }
            } else {
              self.enqueue_state_for_processing_kernel(state.clone(), *parser_config, allow_local_queueing);
            }
          }
        }
        queued += 1;
      }
    }

    Ok(queued)
  }

  fn append_state_hashes(&mut self, is_root: bool, mut state: GraphNode) -> GraphNode {
    let lookahead =
      if is_root { 0 } else { create_lookahead_hash(self, &state, std::collections::hash_map::DefaultHasher::new()) };
    let state_hash = create_state_hash(&state, lookahead, std::collections::hash_map::DefaultHasher::new());

    state.hash_id = state_hash;
    state.id = StateId::new(state.hash_id as usize, is_root.then_some(GraphIdSubType::Root).unwrap_or(GraphIdSubType::Regular));
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

pub struct Graphs {
  root_states:    RootStates,
  poisoned:       Set<DBNonTermKey>,
  leaf_states:    Vec<SharedGraphNode>,
  predecessors:   Map<u64, Vec<SharedGraphNode>>,
  state_nonterms: Map<u64, ItemSet>,
}

impl Debug for Graphs {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("Graphs");
    s.field("root_states", &self.root_states);
    s.field("leaf_states", &self.leaf_states);
    s.field("predecessors", &self.predecessors);
    s.finish()
  }
}

impl From<ConcurrentGraphBuilder> for Graphs {
  fn from(value: ConcurrentGraphBuilder) -> Self {
    assert!(value.queue.read().unwrap().is_empty());
    Self {
      root_states:    value.root_states.read().unwrap().clone(),
      poisoned:       value.poisoned.read().unwrap().clone(),
      leaf_states:    value.leaf_states.read().unwrap().clone(),
      predecessors:   value.predecessors.read().unwrap().clone(),
      state_nonterms: value.state_nonterms.read().unwrap().clone(),
    }
  }
}

impl Graphs {
  pub fn create_ir_precursors<'a>(&'a self) -> IrPrecursorData<'a> {
    // Collect all leaf states that are part of a valid source

    let mut successors: BTreeMap<u64, IRPrecursorGroup> = BTreeMap::new();

    let mut queue = VecDeque::from_iter(self.leaf_states.iter().cloned());

    for leaf_state in queue.iter() {
      if self.poisoned.contains(&leaf_state.root_data.db_key) {
        continue;
      }

      successors.insert(leaf_state.hash_id, IRPrecursorGroup {
        state:         leaf_state.clone(),
        successors:    Default::default(),
        non_terminals: Default::default(),
        root_name:     None,
      });
    }

    while let Some(node) = queue.pop_front() {
      if self.poisoned.contains(&node.root_data.db_key) {
        continue;
      }

      for predecessors in self.predecessors.get(&node.hash_id).into_iter() {
        for predecessor in predecessors {
          let map = successors.entry(predecessor.hash_id).or_insert(IRPrecursorGroup {
            state:         predecessor.clone(),
            successors:    BTreeMap::new(),
            non_terminals: self.state_nonterms.get(&predecessor.hash_id).cloned(),
            root_name:     predecessor.is_root().then(|| predecessor.root_data.root_name),
          });

          let node_hash = node.hash_id;
          map.successors.insert(node_hash, node.clone());
          queue.push_back(predecessor.clone());
        }
      }
    }

    IrPrecursorData { graph: self, precursors: successors }
  }
}

#[derive(Debug, Clone)]
pub struct IRPrecursorGroup {
  pub state:         SharedGraphNode,
  pub successors:    BTreeMap<u64, SharedGraphNode>,
  pub non_terminals: Option<ItemSet>,
  pub root_name:     Option<IString>,
}

#[derive(Debug)]
pub struct IrPrecursorData<'a> {
  graph:      &'a Graphs,
  precursors: BTreeMap<u64, IRPrecursorGroup>,
}

impl<'a> IrPrecursorData<'a> {
  pub fn iter<'p>(&'p self) -> GraphIterator<'p> {
    self.into()
  }
}

pub struct GraphIterator<'a> {
  precursors: &'a IrPrecursorData<'a>,
  queue:      VecDeque<u64>,
}

impl<'a: 'b, 'b> From<&'b IrPrecursorData<'a>> for GraphIterator<'b> {
  fn from(value: &'b IrPrecursorData<'a>) -> Self {
    let graph = value.graph;
    let mut seen = HashSet::new();
    let mut out_queue = VecDeque::with_capacity(value.precursors.len());
    let mut process_queue = VecDeque::from_iter(graph.root_states.iter().map(|s| s.1 .1.hash_id));

    while let Some(id) = process_queue.pop_front() {
      if seen.insert(id) {
        out_queue.push_back(id);
        if let Some(IRPrecursorGroup { successors, .. }) = value.precursors.get(&id).as_ref() {
          for successor in successors.values() {
            process_queue.push_back(successor.hash_id)
          }
        }
      }
    }

    Self { precursors: value, queue: out_queue }
  }
}

impl<'a> Iterator for GraphIterator<'a> {
  type Item = &'a IRPrecursorGroup;

  fn next(&mut self) -> Option<Self::Item> {
    let next = self.queue.pop_front();
    next.and_then(|n| self.precursors.precursors.get(&n))
  }
}
