#![allow(unused)]

use super::super::{
  build::{TransitionGroup, TransitionGroups},
  graph::*,
  items::peek_items_are_from_goto_state,
};
use crate::{
  compile::build_graph::errors::peek_not_allowed_error,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::BTreeSet;

use GraphState::*;

/// Peek needs --- collection nonterminal and terminal symbols to complete, and
/// a set of follow items for each kernel item

pub(crate) fn create_peek<
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

  for (_, nonterms) in hash_group_btree_iter::<Items, _, _, _, _>(incomplete_items.clone(), |_, i| i.is_out_of_scope()) {
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

fn resolve_peek<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  gb: &mut GraphBuilder<'db>,
  mut resolved: T,
  sym: PrecedentSymbol,
) -> SherpaResult<()> {
  let (index, items) = get_kernel_items_from_peek_origin(gb, resolved.next().unwrap().origin);

  gb.create_state(NormalGoto, sym, StateType::PeekEndComplete(index), items.to_vec()).enque();

  Ok(())
}

pub(crate) fn get_kernel_items_from_peek_origin<'db, 'follow>(
  gb: &mut GraphBuilder<'db>,
  peek_origin: Origin,
) -> (u64, ItemSet<'db>) {
  let Origin::Peek(peek_index, peek_origin) = peek_origin else {
    unreachable!("Invalid peek origin");
  };

  (peek_index, gb.graph()[peek_origin].get_resolve_state(peek_index))
}

pub(crate) fn get_kernel_items_from_peek_item<'db>(iter: &GraphBuilder<'db>, peek_item: &Item<'db>) -> ItemSet<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
    unreachable!("Invalid peek origin");
  };

  iter.graph()[peek_origin].get_resolve_state(peek_index)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PeekOriginType {
  Oos,
  Complete,
  Incomplete,
}

pub(crate) fn handle_peek_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut TransitionGroups<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  _default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let mut cmpl = follow_pairs.iter().to_completed_vec();
  match (follow_pairs.len(), groups.remove(&prec_sym.sym())) {
    (1, None) => {
      resolve_peek(gb, cmpl.iter(), prec_sym)?;
    }
    (_, None)
      if cmpl.iter().all_are_out_of_scope()
        || cmpl.iter().all_items_are_from_same_peek_origin()
        || peek_items_are_from_goto_state(&cmpl, gb.graph()) =>
    {
      if !cmpl.iter().all_are_out_of_scope() {
        cmpl = cmpl.into_iter().filter(|i| !i.is_out_of_scope()).collect();
      }

      resolve_peek(gb, cmpl.iter(), prec_sym)?;
    }
    // More than one completed items from peeking.
    (_, None) => {
      let mut targets = hash_group_btree_iter::<Vec<_>, _, _, _, _>(
        follow_pairs
          .iter()
          .map(|i| i.completed.origin)
          .collect::<Set<_>>()
          .into_iter()
          .map(|origin| get_kernel_items_from_peek_origin(gb, origin)),
        |_, (_, items)| {
          if items.iter().all_are_out_of_scope() {
            PeekOriginType::Oos
          } else if items.iter().all(|i| i.is_complete()) {
            PeekOriginType::Complete
          } else {
            PeekOriginType::Incomplete
          }
        },
      );

      let _oos_targets = targets.remove(&PeekOriginType::Oos);
      let _cmpl_targets = targets.remove(&PeekOriginType::Complete);
      let incpl_targets = targets.remove(&PeekOriginType::Incomplete);

      // Prefer shift.
      if incpl_targets.as_ref().is_some_and(|t| t.len() == 1) {
        let (origin_index, items) = incpl_targets.unwrap().into_iter().next().unwrap();
        gb.create_state(NormalGoto, prec_sym, StateType::PeekEndComplete(origin_index), items.to_vec()).enque();
      } else {
        // If the number of resolve states is two and one of the states is oos then
        // resolve to the none oos state.

        #[cfg(debug_assertions)]
        {
          let kernel_items =
            follow_pairs.iter().map(|fp| get_kernel_items_from_peek_item(gb, &fp.completed)).collect::<OrderedSet<_>>();
          let db = gb.db;
          crate::test::utils::write_debug_file(db, "parse_graph.tmp", gb.graph().debug_string(), true)?;
          unimplemented!(
        "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParent State\n{}\n\n",

        prec_sym.sym().debug_string(gb.db),
        gb.graph().goal_items().to_debug_string( "\n"),
        cmpl.to_debug_string("\n"),
        kernel_items.iter().map(|s| s.to_debug_string("\n")).collect::<Vec<_>>().join("\n"),
        gb.current_state().debug_string(gb.db),
        //graph.debug_string()
      );
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }

    (_, Some((_, group))) => {
      let mut combined = group.clone();
      combined.append(&mut follow_pairs.iter().to_follow_set());

      if combined.iter().all_items_are_from_same_peek_origin() {
        resolve_peek(gb, combined.iter(), prec_sym)?;
      } else {
        #[cfg(debug_assertions)]
        todo!(
      "Roll the follow states into the group and resubmit to incomplete handler function.\nincomplete:\n{}\ncomplete:\n{}\nfollow:\n{}\n \n {}",
      group.to_debug_string("\n"),
      follow_pairs.iter().to_completed_vec().to_debug_string( "\n"),
      follow_pairs.iter().to_follow_vec().to_debug_string("\n"),
      gb.graph().debug_string()
    );

        #[cfg(not(debug_assertions))]
        todo!()
      }
    }
  }
  Ok(())
}

pub(crate) fn handle_peek_incomplete_items<'nt_set, 'db: 'nt_set>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup<'db>,
  level: u16,
) -> SherpaResult<()> {
  if group.iter().all_items_are_from_same_peek_origin() {
    resolve_peek(gb, group.iter(), prec_sym)?;
  } else {
    gb.create_state(Peek(level + 1), prec_sym, StateType::Peek, group.try_increment()).enque();
  }
  SherpaResult::Ok(())
}
