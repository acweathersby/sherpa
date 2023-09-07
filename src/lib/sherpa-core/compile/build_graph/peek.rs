use super::graph::*;
use crate::{
  compile::build_graph::errors::peek_not_allowed_error,
  types::*,
  utils::{hash_group_btreemap, hash_group_btreemap_iter},
};
use std::collections::BTreeSet;

/// Peek needs --- collection nonterminal and terminal symbols to complete, and
/// a set of follow items for each kernel item

pub(super) fn create_peek<
  'a,
  'db: 'a,
  'follow,
  T: ItemRefContainerIter<'a, 'db> + Clone,
  Pairs: Iterator<Item = &'a FollowPair<'db>> + Sized + Clone,
>(
  gb: &mut GraphBuilder<'db>,
  sym: PrecedentSymbol,
  incomplete_items: &T,
  completed_pairs: Option<Pairs>,
  need_increment: bool,
  transition_type: StateType,
) -> SherpaResult<StateId> {
  debug_assert!(!gb.is_scanner(), "Peeking in scanners is unnecessary and not allowed");

  let mut kernel_items = Array::default();
  let ALLOW_PEEKING = gb.config.ALLOW_PEEKING;

  let existing_items: ItemSet = incomplete_items.clone().to_absolute();
  let mut state = gb.create_state(GraphState::Peek(0), sym, transition_type, Array::default());

  if let Some(completed_pairs) = completed_pairs {
    let pairs = completed_pairs.cloned().collect::<BTreeSet<_>>();

    // All items here complete the same nonterminal, so we group them all into one
    // goal index.

    let reduced_pairs = hash_group_btreemap(pairs, |_, fp| fp.completed.rule_id);

    for (_, items) in reduced_pairs {
      let follow: ItemSet = items
        .iter()
        .filter_map(|FollowPair { follow, .. }| if existing_items.contains(follow) { None } else { Some(*follow) })
        .collect();

      if !follow.is_empty() {
        let origin = state.set_peek_resolve_state(&items.iter().to_completed_set());
        for follow in follow {
          kernel_items.push(follow.to_origin(origin));
        }
      }
    }
  }

  for (_, nonterms) in hash_group_btreemap_iter::<Items, _, _, _, _>(incomplete_items.clone(), |_, i| i.is_out_of_scope()) {
    let origin = state.set_peek_resolve_state(&nonterms);

    for nonterm in &nonterms {
      kernel_items.push(nonterm.to_origin(origin));
    }
  }

  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );
  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );

  state.add_kernel_items(if need_increment { kernel_items.try_increment() } else { kernel_items });

  if ALLOW_PEEKING {
    Ok(state.to_state())
  } else {
    let state = state.to_state();
    Err(peek_not_allowed_error(gb.graph(), state))
  }
}

pub(super) fn get_kernel_items_from_peek_origin<'db, 'follow>(graph: &GraphHost<'db>, peek_origin: Origin) -> ItemSet<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_origin else {
    unreachable!("Invalid peek origin");
  };

  graph[peek_origin].get_resolve_state(peek_index)
}

pub(super) fn get_kernel_items_from_peek_item<'db>(iter: &GraphBuilder<'db>, peek_item: &Item<'db>) -> ItemSet<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
    unreachable!("Invalid peek origin");
  };

  iter.graph()[peek_origin].get_resolve_state(peek_index)
}
