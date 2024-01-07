use super::graph::{GraphBuildState, Origin, PeekGroup, StateId, StateType};
use crate::{hash_id_value_u64, proxy::OrderedSet, types::*, Item, RadlrResult};

struct GraphNode {
  build_state:  GraphBuildState,
  is_leaf:      bool,
  ty:           StateType,
  id:           StateId,
  symbol_set:   u64,
  hash_id:      u64,
  lookahead_id: u64,
  kernel:       OrderedSet<Item>,
  predecessors: std::sync::RwLock<Vec<GraphNodeShared>>,
}

pub type GraphNodeShared = std::sync::Arc<GraphNode>;

impl GraphNode {
  pub fn new(
    id: StateId,
    build_state: GraphBuildState,
    ty: StateType,
    is_leaf: bool,
    kernel: OrderedSet<Item>,
    primary_predecessor: Option<GraphNodeShared>,
  ) -> GraphNodeShared {
    let predecessors = if let Some(parent) = primary_predecessor {
      let mut predecessors = Vec::with_capacity(8);
      predecessors.push(parent);
      predecessors
    } else {
      vec![]
    };

    std::sync::Arc::new(GraphNode {
      build_state,
      is_leaf,
      ty,
      id,
      symbol_set: Default::default(),
      hash_id: Default::default(),
      lookahead_id: Default::default(),
      kernel,
      predecessors: std::sync::RwLock::new(predecessors),
    })
  }

  pub fn kernel_items(&self) -> &OrderedSet<Item> {
    &self.kernel
  }

  pub fn is_leaf(&self) -> bool {
    self.is_leaf
  }

  pub fn add_predecessor(&self, predecessor: GraphNodeShared) -> RadlrResult<()> {
    match self.predecessors.write() {
      Err(err) => Err(err.into()),
      Ok(mut predecessors) => {
        predecessors.push(predecessor);
        Ok(())
      }
    }
  }

  pub fn get_predecessors<'a>(&'a self) -> RadlrResult<std::sync::RwLockReadGuard<'a, Vec<GraphNodeShared>>> {
    match self.predecessors.read() {
      Err(err) => Err(err.into()),
      Ok(predecessors) => Ok(predecessors),
    }
  }

  pub fn parent<'a>(&'a self) -> RadlrResult<Option<GraphNodeShared>> {
    match self.predecessors.read() {
      Err(err) => Err(err.into()),
      Ok(predecessors) => Ok(predecessors.get(0).cloned()),
    }
  }
}

pub struct GraphNodeBuilder<'builder> {
  state_id: StateId,
  id: StateId,
  build_state: GraphBuildState,
  ty: StateType,
  is_leaf: bool,
  kernel: OrderedSet<Item>,
  primary_predecessor: Option<GraphNodeShared>,
  resolved: bool,
  builder: &'builder mut AsyncGraphBuilder,
}

impl<'builder> GraphNodeBuilder<'builder> {
  pub fn set_parent(&mut self, parent: GraphNodeShared) {
    let GraphNodeBuilder { state_id, .. } = self;
    let old_parent = self.primary_predecessor.take();
    self.primary_predecessor = Some(parent);
  }

  pub fn set_peek_resolve_state<T: Iterator<Item = Item>>(&mut self, items: T, is_oos: bool) -> Origin {
    let peek_group = PeekGroup { items: items.collect(), is_oos };

    let index = hash_id_value_u64(&peek_group) as u32;

    let GraphNodeBuilder { state_id, .. } = self;

    todo!("set_peek_resolve_state");

    Origin::Peek(index)
  }

  pub fn add_kernel_items<T: ItemContainerIter>(&mut self, items: T) {
    let GraphNodeBuilder { kernel, .. } = self;
    kernel.extend(items);
  }

  pub fn set_kernel_items<T: ItemContainerIter>(&mut self, items: T) {
    let GraphNodeBuilder { kernel, .. } = self;
    *kernel = OrderedSet::from_iter(items);
  }

  pub fn set_reduce_item(&mut self, item: Item) {
    let GraphNodeBuilder { state_id, .. } = self;
    todo!("set_reduce_item");
  }
}

pub struct AsyncGraphBuilder {
  root_states: std::sync::RwLock<Map<u64, GraphNodeShared>>,
  states: std::sync::RwLock<Map<u64, GraphNodeShared>>,
  peek_resolve_states: std::sync::RwLock<Map<u64, GraphNodeShared>>,
  leaf_states: std::sync::RwLock<Vec<GraphNodeShared>>,
  //leaf_states: std::sync::RwLock<Vec<GraphNodeShared>>,
}
