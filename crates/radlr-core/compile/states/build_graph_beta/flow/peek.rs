#![allow(unused)]

use super::{
  super::{
    build::{GroupedFirsts, TransitionGroup},
    graph::*,
  },
  convert_peek_root_state_to_fork,
  create_fork,
};
use crate::{
  compile::states::build_graph::graph::{DefaultIter, GraphBuildState, Origin, PeekGroup, StateId, StateType},
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::{BTreeSet, HashSet};

const INITIAL_PEEK_K: u32 = 2;

/// Peek needs --- collection nonterminal and terminal symbols to complete, and
/// a set of follow items for each kernel item

pub(crate) fn create_peek<'a, 'follow, Pairs: Iterator<Item = &'a TransitionPair> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  config: &ParserConfig,
  sym: PrecedentSymbol,
  incomplete_items: Pairs,
  completed_pairs: Option<Pairs>,
) -> RadlrResult<StagedNode> {
  debug_assert!(config.ALLOW_PEEKING && config.max_k > 1, "Peek states should not be created when peeking is not allowed or k=1");
  debug_assert!(!pred.is_scanner(), "Peeking in scanners is unnecessary and not allowed");

  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.kernel.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );

  let mut state = StagedNode::new(gb).build_state(GraphBuildState::Normal).sym(sym).ty(StateType::Peek(INITIAL_PEEK_K));

  state = state.parent(pred.clone());

  let state_id = pred.id();

  let incomplete_items = incomplete_items.into_iter().cloned().collect::<Vec<_>>();
  let completed_pairs = completed_pairs.map(|c| c.into_iter().cloned().collect::<Vec<_>>());

  let existing_items = incomplete_items.iter().to_next().heritage();

  let mut follow_sets = Vec::new();
  let mut nonterm_sets = Vec::new();

  let mut kernel_items = Array::default();

  if let Some(completed_pairs) = completed_pairs {
    let pairs: BTreeSet<TransitionPair> = completed_pairs.iter().cloned().collect::<BTreeSet<_>>();

    // All items here complete the same nonterminal, so we group them all into one
    // goal index.

    let reduced_pairs = hash_group_btreemap(pairs, |_, fp| (fp.kernel.rule_id(), fp.kernel.origin.is_out_of_scope()));

    for ((_, is_oos), items) in reduced_pairs {
      let follow: ItemSet = items
        .iter()
        .filter_map(|Lookahead { next: follow, .. }| if existing_items.contains(&follow.into()) { None } else { Some(*follow) })
        .collect();

      if !follow.is_empty() {
        for follow in &follow {
          kernel_items.push(*follow);
        }
        follow_sets.push((follow, items, is_oos));
      }
    }
  }

  for (_, nonterms) in
    hash_group_btree_iter::<Lookaheads, _, _, _, _>(incomplete_items.iter().clone(), |_, i| i.is_out_of_scope())
  {
    for nonterm in &nonterms {
      kernel_items.push(nonterm.next);
    }
    nonterm_sets.push(nonterms);
  }

  state = state.kernel_items(kernel_items.try_increment().iter().cloned());

  let mut kernel_items = Array::default();

  let is_goto_state = false;
  for (follow, items, is_oos) in follow_sets {
    let items = items.iter().to_kernel().map(|i| {
      if is_goto_state && false {
        if i.origin_state.0 == state_id.0 {
          i.as_goto_origin()
        } else {
          i.increment_goto()
        }
      } else {
        i.clone()
      }
    });
    let origin = gb.set_peek_resolve_state(items, is_oos);
    for follow in follow {
      kernel_items.push(follow.to_origin(origin));
    }
  }

  for nonterms in nonterm_sets {
    let items = nonterms.iter().to_kernel().map(|i| {
      if is_goto_state && false {
        if i.origin_state.0 == state_id.0 {
          i.as_goto_origin()
        } else {
          i.increment_goto()
        }
      } else {
        i.clone()
      }
    });

    let origin = gb.set_peek_resolve_state(items, nonterms.iter().any(|i| i.kernel.origin.is_out_of_scope()));

    for nonterm in &nonterms {
      kernel_items.push(nonterm.next.to_origin(origin));
    }
  }

  let state = state.kernel_items(kernel_items.try_increment().into_iter());

  /*   state = state.finalizer(Box::new(move |state, gb, is_goto_state: bool| {
    state.kernel = kernel_items.try_increment().iter().cloned().collect();
  })); */

  Ok(state)
}

fn build_peek_nodes(node: GraphNode, builder: &mut ConcurrentGraphBuilder) -> GraphNode {
  node
}

fn resolve_peek<'a, 'db: 'a, T: Iterator<Item = &'a TransitionPair>>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  mut resolved: T,
  sym: PrecedentSymbol,
) -> RadlrResult<()> {
  let (index, PeekGroup { items, .. }) = get_kernel_items_from_peek_origin(gb, pred, resolved.next().unwrap().kernel.origin);
  let staged = items.clone();

  StagedNode::new(gb)
    .sym(sym)
    .parent(pred.clone())
    .build_state(GraphBuildState::NormalGoto)
    .ty(StateType::PeekEndComplete(index))
    .kernel_items(staged.into_iter())
    .commit(gb);

  Ok(())
}

pub(crate) fn get_kernel_items_from_peek_origin<'graph, 'db: 'graph>(
  gb: &'graph mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  peek_origin: Origin,
) -> (u32, PeekGroup) {
  let Origin::Peek(peek_index) = peek_origin else {
    unreachable!("Invalid peek origin");
  };

  (peek_index, gb.get_peek_resolve_items(peek_index as u64))
}

pub(crate) fn get_kernel_items_from_peek_item<'graph, 'db: 'graph>(
  gb: &'graph mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  peek_item: &Item,
) -> PeekGroup {
  let Origin::Peek(peek_index) = peek_item.origin else {
    unreachable!("Invalid peek origin");
  };
  gb.get_peek_resolve_items(peek_index as u64)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PeekOriginType {
  Oos,
  Complete,
  Incomplete,
}

pub(crate) fn handle_peek_complete_groups<'graph, 'db: 'graph>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  config: &ParserConfig,
  groups: &mut GroupedFirsts,
  prec_sym: PrecedentSymbol,
  follows: Lookaheads,
  level: u32,
) -> RadlrResult<()> {
  let ____is_scan____ = pred.is_scanner();
  let mut cmpl = follows.iter().to_next();

  match (follows.len(), groups.remove(&prec_sym.sym())) {
    (1, None) => {
      resolve_peek(gb, pred, follows.iter(), prec_sym)?;
    }
    (_, None) if peek_items_are_from_oos(gb, pred, &follows) || peek_items_are_from_same_origin(gb, &follows) => {
      resolve_peek(gb, pred, follows.iter(), prec_sym)?;
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
          .map(|origin| get_kernel_items_from_peek_origin(gb, pred, origin)),
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

      match (__oos_targets_len, cmpl_targets_len, incpl_targets_len) {
        (_, 0, 1..) if config.ALLOW_LR => {
          // Can do any number of shifts
          let (origin_index, PeekGroup { items, .. }) = incpl_targets.unwrap().into_iter().next().unwrap();
          let staged = items.clone();

          StagedNode::new(gb)
            .sym(prec_sym)
            .parent(pred.clone())
            .build_state(GraphBuildState::NormalGoto)
            .ty(StateType::PeekEndComplete(origin_index))
            .kernel_items(staged.into_iter())
            .commit(gb);
        }
        (_, 0, 1) => {
          // Can do any number of shifts
          let (origin_index, PeekGroup { items, .. }) = incpl_targets.unwrap().into_iter().next().unwrap();
          let staged = items.clone();
          StagedNode::new(gb)
            .sym(prec_sym)
            .parent(pred.clone())
            .build_state(GraphBuildState::NormalGoto)
            .ty(StateType::PeekEndComplete(origin_index))
            .kernel_items(staged.into_iter())
            .commit(gb);
        }
        (_, 1, 0) => {
          let (origin_index, PeekGroup { items, .. }) = cmpl_targets.unwrap().into_iter().next().unwrap();
          let staged = items.clone();
          StagedNode::new(gb)
            .sym(prec_sym)
            .parent(pred.clone())
            .build_state(GraphBuildState::NormalGoto)
            .ty(StateType::PeekEndComplete(origin_index))
            .kernel_items(staged.into_iter())
            .commit(gb);
        }
        (1.., 0, 0) => {
          let (origin_index, PeekGroup { items, .. }) = _oos_targets.unwrap().into_iter().next().unwrap();
          let staged = items.clone();
          StagedNode::new(gb)
            .sym(prec_sym)
            .parent(pred.clone())
            .build_state(GraphBuildState::NormalGoto)
            .ty(StateType::PeekEndComplete(origin_index))
            .kernel_items(staged.into_iter())
            .commit(gb);
        }
        _ => {
          if !prec_sym.sym().is_default() {
            // Continue peeking

            StagedNode::new(gb)
              .sym(prec_sym)
              .parent(pred.clone())
              .build_state(GraphBuildState::Normal)
              .ty(StateType::Peek(level + 1))
              .kernel_items(follows.iter().map(|f| f.next.to_origin(f.kernel.origin)).try_increment().iter().cloned())
              .commit(gb);
          }
        }
      }
    }

    (_, Some((_, group))) => {
      //if the follows are not complete then we can continue peeking.

      if follows.iter().any(|f| f.is_eoi_complete()) {
        panic!("Cannot disambiguate using peek!!!");
      } else {
        // create a new peek state with the follow items.
        StagedNode::new(gb)
          .sym(prec_sym)
          .parent(pred.clone())
          .build_state(GraphBuildState::Normal)
          .ty(StateType::Peek(level + 1))
          .kernel_items(
            group
              .iter()
              .to_next()
              .cloned()
              .chain(follows.iter().map(|f| f.next.to_origin(f.kernel.origin)))
              .try_increment()
              .iter()
              .cloned(),
          )
          .commit(gb);
        //todo!("(anthony): Resolve intermediate peek! Also figure out what
        // \"intermediate peek\" means ");
      }
    }
  }
  Ok(())
}

pub(crate) fn handle_peek_incomplete_items<'nt_set, 'db: 'nt_set>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup,
  level: u32,
) -> RadlrResult<()> {
  if peek_items_are_from_same_origin(gb, &group) {
    resolve_peek(gb, pred, group.iter(), prec_sym)?;
  } else {
    StagedNode::new(gb)
      .sym(prec_sym)
      .parent(pred.clone())
      .build_state(GraphBuildState::Normal)
      .ty(StateType::Peek(level + 1))
      .kernel_items(group.iter().to_next().try_increment().iter().cloned())
      .commit(gb);
  }
  RadlrResult::Ok(())
}

fn peek_items_are_from_oos(gb: &mut ConcurrentGraphBuilder, node: &SharedGraphNode, follows: &Lookaheads) -> bool {
  follows
    .iter()
    .to_kernel()
    .map(|i| match i.origin {
      Origin::Peek(key) => gb.get_peek_resolve_items(key as u64),
      _ => unreachable!(),
    })
    .all(|group| group.is_oos)
}

fn peek_items_are_from_same_origin(gb: &ConcurrentGraphBuilder, follows: &Lookaheads) -> bool {
  follows.iter().to_kernel().map(|i| i.origin).collect::<Set<_>>().len() == 1
}
