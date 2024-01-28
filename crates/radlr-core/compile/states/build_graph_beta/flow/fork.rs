#![allow(unused)]

use super::super::{
  build::{GroupedFirsts, TransitionGroup},
  graph::*,
};
use crate::{
  compile::states::build_graph::graph::{GraphBuildState, StateType},
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::{BTreeSet, HashSet};

pub(crate) fn create_fork<'graph_iter, 'follow, I: Iterator<Item = Item>>(
  gb: &'graph_iter mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  config: &ParserConfig,
  sym: PrecedentSymbol,
  items: I,
) -> RadlrResult<StagedNode> {
  debug_assert!(config.ALLOW_PEEKING && config.max_k > 1, "Peek states should not be created when peeking is not allowed or k=1");
  debug_assert!(!pred.is_scanner(), "Peeking in scanners is unnecessary and not allowed");

  Ok(
    StagedNode::new(gb)
      .build_state(GraphBuildState::Normal)
      .parent(pred.clone())
      .sym(sym)
      .ty(StateType::ForkInitiator)
      .kernel_items(items),
  )
}

pub(crate) fn handle_fork<'a, 'db: 'a>(gb: &mut ConcurrentGraphBuilder, pred: &SharedGraphNode) -> bool {
  if matches!(pred.state_type(), StateType::ForkInitiator) {
    for kernel_item in pred.kernel_items() {
      StagedNode::new(gb)
        .build_state(GraphBuildState::Normal)
        .parent(pred.clone())
        .sym(Default::default())
        .ty(StateType::ForkedState)
        .kernel_items(vec![*kernel_item].into_iter())
        .commit(gb);
    }
    true
  } else {
    false
  }
}

pub(crate) fn convert_peek_root_state_to_fork(gb: &mut ConcurrentGraphBuilder, pred: &SharedGraphNode) -> Result<(), RadlrError> {
  Ok(loop {
    todo!("Convert root peek state to fork");
    /* if matches!(pred.state_type(), StateType::Peek(INITIAL_PEEK_K)) {
      let items = state.get_peek_resolve_items().unwrap().flat_map(|(_, items)| items.items.iter()).cloned().collect::<Vec<_>>();
      let sym = state.get_symbol();
      let parent = state.get_parent();
      if gb.detach_state(state_id) {
        let mut state = create_fork(gb, sym, items.into_iter())?;
        state.set_parent(parent);
        state.to_enqueued();
      }
      break;
    } else if state_id.is_root() {
      unreachable!()
    } else {
      state_id = state.get_parent()
    } */
  })
}
