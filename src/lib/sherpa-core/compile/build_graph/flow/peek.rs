#![allow(unused)]

use super::super::{
  build::{GroupedFirsts, TransitionGroup},
  graph::*,
};
use crate::{
  compile::build_graph::errors::peek_not_allowed_error,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::BTreeSet;

use GraphBuildState::*;

/// Peek needs --- collection nonterminal and terminal symbols to complete, and
/// a set of follow items for each kernel item

pub(crate) fn create_peek<'a, 'db: 'a, 'follow, Pairs: Iterator<Item = &'a TransitionPair<'db>> + Sized + Clone>(
  gb: &mut GraphBuilder<'db>,
  sym: PrecedentSymbol,
  incomplete_items: Pairs,
  completed_pairs: Option<Pairs>,
  need_increment: bool,
  transition_type: StateType,
) -> SherpaResult<StateId> {
  debug_assert!(!gb.is_scanner(), "Peeking in scanners is unnecessary and not allowed");
  let state_id = gb.state_id();
  let mut kernel_items = Array::default();
  let ALLOW_PEEKING = gb.config.ALLOW_PEEKING;

  let existing_items: ItemSet = incomplete_items.clone().to_next().to_absolute();

  let mut state = gb.create_state(GraphBuildState::Peek(0), sym, transition_type, Array::default());

  if let Some(completed_pairs) = completed_pairs {
    let pairs = completed_pairs.to_inherited(state_id).into_iter().collect::<BTreeSet<_>>();

    // All items here complete the same nonterminal, so we group them all into one
    // goal index.

    let reduced_pairs = hash_group_btreemap(pairs, |_, fp| fp.kernel.rule_id);

    for (_, items) in reduced_pairs {
      let follow: ItemSet = items
        .iter()
        .filter_map(|Follow { next: follow, .. }| if existing_items.contains(follow) { None } else { Some(*follow) })
        .collect();

      if !follow.is_empty() {
        let origin = state.set_peek_resolve_state(&items.iter().to_kernel().cloned().collect::<ItemSet>());
        for follow in follow {
          kernel_items.push(follow.to_origin(origin));
        }
      }
    }
  }

  for (_, nonterms) in hash_group_btree_iter::<Firsts, _, _, _, _>(incomplete_items.clone(), |_, i| i.is_out_of_scope()) {
    let origin = state.set_peek_resolve_state(&nonterms.iter().to_kernel().to_vec());

    for nonterm in &nonterms {
      kernel_items.push(nonterm.next.to_origin(origin));
    }
  }

  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.kernel.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );
  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.kernel.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );

  state.add_kernel_items(if need_increment { kernel_items.try_increment() } else { kernel_items });

  if ALLOW_PEEKING {
    Ok(state.to_state())
  } else {
    let state = state.to_state();
    Err(peek_not_allowed_error(gb, state))
  }
}

fn resolve_peek<'a, 'db: 'a, T: Iterator<Item = &'a TransitionPair<'db>>>(
  gb: &mut GraphBuilder<'db>,
  mut resolved: T,
  sym: PrecedentSymbol,
) -> SherpaResult<()> {
  let (index, items) = get_kernel_items_from_peek_origin(gb, resolved.next().unwrap().kernel.origin);

  gb.create_state(NormalGoto, sym, StateType::PeekEndComplete(index), items.clone().to_vec()).to_enqueued();

  Ok(())
}

pub(crate) fn get_kernel_items_from_peek_origin<'a, 'db: 'a>(
  gb: &'a mut GraphBuilder<'db>,
  peek_origin: Origin,
) -> (u64, ItemSet<'db>) {
  let Origin::Peek(peek_index, peek_origin) = peek_origin else {
    unreachable!("Invalid peek origin");
  };

  (peek_index, gb.graph()[peek_origin].get_resolve_item_set(peek_index))
}

pub(crate) fn get_kernel_items_from_peek_item<'a, 'db: 'a>(gb: &'a GraphBuilder<'db>, peek_item: &Item<'db>) -> ItemSet<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
    unreachable!("Invalid peek origin");
  };
  gb.graph()[peek_origin].get_resolve_item_set(peek_index)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PeekOriginType {
  Oos,
  Complete,
  Incomplete,
}

pub(crate) fn handle_peek_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  prec_sym: PrecedentSymbol,
  follows: Follows<'db>,
  _default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let mut cmpl = follows.iter().to_next();
  match (follows.len(), groups.remove(&prec_sym.sym())) {
    (1, None) => {
      resolve_peek(gb, follows.iter(), prec_sym)?;
    }
    (_, None) if peek_items_are_from_oos(gb, &follows) || peek_items_are_from_same_origin(gb, &follows) => {
      resolve_peek(gb, follows.iter(), prec_sym)?;
    }
    // More than one completed items from peeking.
    (_, None) => {
      // So, we could continue peeking as long as the following conditions are met:
      // -
      let mut targets = hash_group_btree_iter::<Vec<_>, _, _, _, _>(
        follows
          .iter()
          .map(|i| i.kernel.origin)
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
      let cmpl_targets = targets.remove(&PeekOriginType::Complete);
      let incpl_targets = targets.remove(&PeekOriginType::Incomplete);

      let __oos_targets_len = _oos_targets.as_ref().map(|t| t.len()).unwrap_or_default();
      let cmpl_targets_len = cmpl_targets.as_ref().map(|t| t.len()).unwrap_or_default();
      let incpl_targets_len = incpl_targets.as_ref().map(|t| t.len()).unwrap_or_default();

      // Prefer shift.
      if incpl_targets_len == 1 {
        // Single shift resolution, this is the ideal situation.
        let (origin_index, items) = incpl_targets.unwrap().into_iter().next().unwrap();
        gb.create_state(NormalGoto, prec_sym, StateType::PeekEndComplete(origin_index), items.to_vec()).to_enqueued();
      } else if incpl_targets_len > 1 {
        panic!("MULTIPLE INCOMPLETED -- Cannot resolve using peek within the limits of the grammar rules. This requires a fork");
      } else if cmpl_targets_len == 1 {
        let (origin_index, items) = cmpl_targets.unwrap().into_iter().next().unwrap();
        gb.create_state(NormalGoto, prec_sym, StateType::PeekEndComplete(origin_index), items.to_vec()).to_enqueued();
      } else if cmpl_targets_len > 1 {
        panic!("MULTIPLE COMPLETED -- Cannot resolve using peek within the limits of the grammar rules. This requires a fork");
      } else {
        panic!("OOS STATES -- This can be discarded");
        /// Only have oos states
        // If the number of resolve states is two and one of the states is oos then
        // resolve to the none oos state.

        #[cfg(debug_assertions)]
        {
          let kernel_items = follows.iter().map(|fp| get_kernel_items_from_peek_item(gb, &fp.kernel)).collect::<OrderedSet<_>>();
          let db = gb.db;
          crate::test::utils::write_debug_file(db, "parse_graph.tmp", gb.graph()._debug_string_(), true)?;
          unimplemented!(
        "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParent State\n{}\n\n",

        prec_sym.sym().debug_string(gb.db),
        gb.graph().goal_items().to_debug_string( "\n"),
        cmpl.to_debug_string("\n"),
        kernel_items.iter().map(|s| s.to_debug_string("\n")).collect::<Vec<_>>().join("\n"),
        gb.current_state()._debug_string_(gb.db),
        //graph.debug_string()
      );
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }

    (_, Some((_, group))) => {
      todo!("Resolve intermediate peek!");
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
  if group.iter().all(|i| matches!(i.kernel.origin, Origin::Peek(..))) {
    resolve_peek(gb, group.iter(), prec_sym)?;
  } else {
    gb.create_state(Peek(level + 1), prec_sym, StateType::Peek, group.iter().to_next().try_increment()).to_enqueued();
  }
  SherpaResult::Ok(())
}

fn peek_items_are_from_goto_state(cmpl: &Items, graph: &GraphHost) -> bool {
  debug_assert_eq!(
    cmpl
      .iter()
      .map(|i| {
        match i.origin {
          Origin::Peek(_, origin) => origin,
          _ => unreachable!(),
        }
      })
      .collect::<OrderedSet<_>>()
      .len(),
    1
  );
  match cmpl[0].origin {
    Origin::Peek(_, origin) => graph[origin].get_type().is_goto(),
    _ => false,
  }
}

fn peek_items_are_from_oos<'db>(gb: &GraphBuilder<'db>, follows: &Follows<'db>) -> bool {
  follows
    .iter()
    .to_kernel()
    .map(|i| match i.origin {
      Origin::Peek(key, origin) => gb.graph()[origin].get_resolve_item_set(key),
      _ => unreachable!(),
    })
    .all(|set| set.iter().next().unwrap().origin.is_out_of_scope())
}

fn peek_items_are_from_same_origin<'db>(gb: &GraphBuilder<'db>, follows: &Follows<'db>) -> bool {
  follows.iter().to_kernel().map(|i| i.origin).collect::<Set<_>>().len() == 1
}
