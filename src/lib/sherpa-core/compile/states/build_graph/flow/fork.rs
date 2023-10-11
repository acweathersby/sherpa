#![allow(unused)]

use super::super::{
  build::{GroupedFirsts, TransitionGroup},
  graph::*,
};
use crate::{
  compile::states::build_graph::errors::peek_not_allowed_error,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::{BTreeSet, HashSet};

use GraphBuildState::*;

pub(crate) fn create_fork<'graph_iter, 'db: 'graph_iter, 'follow, I: Iterator<Item = Item<'db>>>(
  gb: &'graph_iter mut GraphBuilder<'db>,
  sym: PrecedentSymbol,
  items: I,
) -> SherpaResult<StateBuilder<'graph_iter, 'db>> {
  debug_assert!(
    gb.config.ALLOW_PEEKING && gb.config.max_k > 1,
    "Peek states should not be created when peeking is not allowed or k=1"
  );
  debug_assert!(!gb.is_scanner(), "Peeking in scanners is unnecessary and not allowed");
  let state_id = gb.current_state_id();

  Ok(gb.create_state(Normal, sym, StateType::ForkInitiator, Some(items)))
}

pub(crate) fn handle_fork<'a, 'db: 'a>(gb: &mut GraphBuilder<'db>) -> bool {
  if matches!(gb.current_state().get_type(), StateType::ForkInitiator) {
    for kernel_item in gb.current_state().get_kernel_items().clone() {
      let mut state =
        gb.create_state::<DefaultIter>(Normal, Default::default(), StateType::ForkedState, Some(vec![kernel_item].into_iter()));
      state.to_enqueued();
    }
    true
  } else {
    false
  }
}

pub(crate) fn convert_peek_root_state_to_fork(gb: &mut GraphBuilder<'_>) -> Result<(), SherpaError> {
  gb.set_classification(ParserClassification { forks_present: true, ..Default::default() });
  let mut state_id = gb.current_state_id();
  Ok(loop {
    let state = gb.get_state(state_id);
    if matches!(state.get_type(), StateType::Peek(INITIAL_PEEK_K)) {
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
    }
  })
}
