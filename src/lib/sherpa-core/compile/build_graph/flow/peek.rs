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
use std::collections::{BTreeSet, HashSet};

use GraphBuildState::*;

/// Peek needs --- collection nonterminal and terminal symbols to complete, and
/// a set of follow items for each kernel item

pub(crate) fn create_peek<'a, 'db: 'a, 'follow, Pairs: Iterator<Item = &'a TransitionPair<'db>> + Clone>(
  gb: &mut GraphBuilder<'db>,
  sym: PrecedentSymbol,
  incomplete_items: Pairs,
  completed_pairs: Option<Pairs>,
) -> SherpaResult<StateId> {
  debug_assert!(
    gb.config.ALLOW_PEEKING && gb.config.max_k > 1,
    "Peek states should not be created when peeking is not allowed or k=1"
  );
  debug_assert!(!gb.is_scanner(), "Peeking in scanners is unnecessary and not allowed");
  let state_id = gb.current_state_id();
  let mut kernel_items = Array::default();

  let existing_items = incomplete_items.clone().to_next().heritage();

  let mut state = gb.create_state::<DefaultIter>(Normal, sym, StateType::Peek(2), None);

  if let Some(completed_pairs) = completed_pairs {
    let pairs: BTreeSet<TransitionPair<'_>> = completed_pairs.into_iter().cloned().collect::<BTreeSet<_>>();

    // All items here complete the same nonterminal, so we group them all into one
    // goal index.

    let reduced_pairs = hash_group_btreemap(pairs, |_, fp| (fp.kernel.rule_id(), fp.kernel.origin.is_out_of_scope()));

    for ((_, is_oos), items) in reduced_pairs {
      let follow: ItemSet = items
        .iter()
        .filter_map(|Lookahead { next: follow, .. }| if existing_items.contains(&follow.into()) { None } else { Some(*follow) })
        .collect();

      if !follow.is_empty() {
        let origin = state.set_peek_resolve_state(items.iter().to_kernel().cloned(), is_oos);
        for follow in follow {
          kernel_items.push(follow.to_origin(origin));
        }
      }
    }
  }

  for (_, nonterms) in hash_group_btree_iter::<Lookaheads, _, _, _, _>(incomplete_items.clone(), |_, i| i.is_out_of_scope()) {
    let origin = state
      .set_peek_resolve_state(nonterms.iter().to_kernel().cloned(), nonterms.iter().any(|i| i.kernel.origin.is_out_of_scope()));

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

  state.add_kernel_items(kernel_items.try_increment().iter().cloned());

  Ok(state.to_state())
}

fn resolve_peek<'a, 'db: 'a, T: Iterator<Item = &'a TransitionPair<'db>>>(
  gb: &mut GraphBuilder<'db>,
  mut resolved: T,
  sym: PrecedentSymbol,
) -> SherpaResult<()> {
  let max_k = gb.current_state().get_type().peek_level() as u16;
  gb.set_classification(ParserClassification { max_k, ..Default::default() });

  let (index, PeekGroup { items, .. }) = get_kernel_items_from_peek_origin(gb, resolved.next().unwrap().kernel.origin);
  let staged = items.clone();
  gb.create_state(NormalGoto, sym, StateType::PeekEndComplete(index), Some(staged.into_iter())).to_enqueued();

  Ok(())
}

pub(crate) fn get_kernel_items_from_peek_origin<'graph, 'db: 'graph>(
  gb: &'graph GraphBuilder<'db>,
  peek_origin: Origin,
) -> (u32, &'graph PeekGroup<'db>) {
  let Origin::Peek(peek_index) = peek_origin else {
    unreachable!("Invalid peek origin");
  };

  (peek_index, gb.graph().peek_resolve_items.get(&peek_index).as_ref().unwrap())
}

pub(crate) fn get_kernel_items_from_peek_item<'graph, 'db: 'graph>(
  gb: &'graph GraphBuilder<'db>,
  peek_item: &Item<'db>,
) -> &'graph PeekGroup<'db> {
  let Origin::Peek(peek_index) = peek_item.origin else {
    unreachable!("Invalid peek origin");
  };
  gb.graph().peek_resolve_items.get(&peek_index).as_ref().unwrap()
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PeekOriginType {
  Oos,
  Complete,
  Incomplete,
}

pub(crate) fn handle_peek_complete_groups<'graph, 'db: 'graph>(
  gb: &'graph mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  prec_sym: PrecedentSymbol,
  follows: Lookaheads<'db>,
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
        |_, (_, PeekGroup { items, is_oos })| {
          if *is_oos {
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
        let (origin_index, PeekGroup { items, .. }) = incpl_targets.unwrap().into_iter().next().unwrap();
        let staged = items.clone();
        gb.create_state(NormalGoto, prec_sym, StateType::PeekEndComplete(origin_index), Some(staged.into_iter())).to_enqueued();
      } else if incpl_targets_len > 1 {
        panic!("MULTIPLE INCOMPLETED -- Cannot resolve using peek within the limits of the grammar rules. This requires a fork");
      } else if cmpl_targets_len == 1 {
        let (origin_index, PeekGroup { items, .. }) = cmpl_targets.unwrap().into_iter().next().unwrap();
        let staged = items.clone();
        gb.create_state(NormalGoto, prec_sym, StateType::PeekEndComplete(origin_index), Some(staged.into_iter())).to_enqueued();
      } else if cmpl_targets_len > 1 {
        panic!("MULTIPLE COMPLETED -- Cannot resolve using peek within the limits of the grammar rules. This requires a fork");
      } else {
        panic!("OOS STATES -- This can be discarded");
        /// Only have oos states
        // If the number of resolve states is two and one of the states is oos then
        // resolve to the none oos state.

        #[cfg(debug_assertions)]
        {
          let kernel_items =
            follows.iter().map(|fp| &get_kernel_items_from_peek_item(gb, &fp.kernel).items).collect::<OrderedSet<_>>();
          let db = gb.db;
          crate::test::utils::write_debug_file(db, "parse_graph.tmp", gb.graph()._debug_string_(), true)?;
          unimplemented!(
        "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParent State\n{}\n\n",

        prec_sym.sym().debug_string(gb.db),
        gb.graph().goal_items().to_debug_string( "\n"),
        cmpl.to_debug_string("\n"),
        kernel_items.iter().map(|s| s.to_debug_string("\n")).collect::<Vec<_>>().join("\n"),
        gb.current_state()._debug_string_(),
        //graph.debug_string()
      );
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }

    (_, Some((_, group))) => {
      todo!("(anthony): Resolve intermediate peek! Also figure out what \"intermediate peek\" means ");
    }
  }
  Ok(())
}

pub(crate) fn handle_peek_incomplete_items<'nt_set, 'db: 'nt_set>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup<'db>,
  level: u32,
) -> SherpaResult<()> {
  if peek_items_are_from_same_origin(gb, &group) {
    resolve_peek(gb, group.iter(), prec_sym)?;
  } else {
    gb.create_state(Normal, prec_sym, StateType::Peek(level + 1), Some(group.iter().to_next().try_increment().iter().cloned()))
      .to_enqueued();
  }
  SherpaResult::Ok(())
}

fn peek_items_are_from_oos<'db>(gb: &GraphBuilder<'db>, follows: &Lookaheads<'db>) -> bool {
  follows
    .iter()
    .to_kernel()
    .map(|i| match i.origin {
      Origin::Peek(key) => gb.graph().peek_resolve_items.get(&key).clone(),
      _ => unreachable!(),
    })
    .all(|group| group.is_some_and(|i| i.is_oos))
}

fn peek_items_are_from_same_origin<'db>(gb: &GraphBuilder<'db>, follows: &Lookaheads<'db>) -> bool {
  follows.iter().to_kernel().map(|i| i.origin).collect::<Set<_>>().len() == 1
}
