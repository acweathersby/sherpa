#![allow(unused)]

use super::super::graph::*;
use crate::{
  compile::states::build_graph::{
    graph::{Origin, StateType},
    items::{get_follow, get_follow_internal, get_goal_items_from_completed, FollowType},
    stack_vec::StackVec,
  },
  types::*,
};
use std::{
  collections::hash_map::DefaultHasher,
  hash::{self, Hash, Hasher},
};

pub(crate) fn handle_completed_item<'follow>(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  completed: Lookaheads,
  sym: PrecedentSymbol,
) -> RadlrResult<()> {
  let ____is_scan____ = node.is_scanner();

  let first = completed[0];

  if first.kernel.origin == Origin::GoalCompleteOOS {
    StagedNode::new(gb).parent(node.clone()).ty(StateType::NonTermCompleteOOS).sym(sym).make_leaf().commit(gb);
  } else if ____is_scan____ {
    complete_scan(completed, gb, node, sym, first)
  } else {
    complete_regular(completed, gb, node, config, sym)
  }

  RadlrResult::Ok(())
}

fn complete_regular(
  completed: Vec<TransitionPair>,
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  sym: PrecedentSymbol,
) {
  let root_item = completed[0].kernel;
  let ____is_scan____ = node.is_scanner();
  let ____allow_rd____: bool = config.ALLOW_CALLS || ____is_scan____;
  let ____allow_ra____: bool = config.ALLOW_LR || ____is_scan____;
  let ____allow_fork____: bool = config.ALLOW_CONTEXT_SPLITTING && false; // Forking is disabled
  let ____allow_peek____: bool = config.ALLOW_PEEKING;

  #[cfg(debug_assertions)]
  debug_assert!(!root_item.from_goto_origin || root_item.goto_distance > 0, "{:?}", root_item);

  StagedNode::new(gb)
    .parent(node.clone())
    .ty(StateType::Reduce(root_item.rule_id(), root_item.goto_distance as usize - (root_item.from_goto_origin as usize)))
    .sym(sym)
    .make_leaf()
    .set_reduce_item(root_item)
    .kernel_items([root_item].into_iter())
    .commit(gb);
  //  }
}

fn complete_scan(
  completed: Vec<TransitionPair>,
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  sym: PrecedentSymbol,
  first: TransitionPair,
) {
  if first.kernel.origin.is_scanner_oos() {
    StagedNode::new(gb).parent(pred.clone()).make_leaf().sym(sym).ty(StateType::ScannerCompleteOOS).commit(gb);
  } else {
    let mut follow = StackVec::<512, _>::new();
    let mut completed_items = StackVec::<512, _>::new();

    for pair in completed {
      get_follow_internal(gb, pred, pair.kernel, FollowType::ScannerCompleted, &mut follow, &mut completed_items);
    }

    completed_items.sort();

    let follow_hash = create_follow_hash(completed_items.as_slice());

    let goals =
      if first.allow_assign { get_goal_items_from_completed(completed_items.as_slice(), &pred) } else { Default::default() };
    let completes_goal = !goals.is_empty();
    let is_continue = !follow.is_empty();

    let state = StagedNode::new(gb)
      .parent(pred.clone())
      .sym(sym)
      .ty(match (is_continue, goals.first().map(|d| d.origin)) {
        (true, Some(Origin::TerminalGoal(tok_id, ..))) => StateType::AssignAndFollow(tok_id),
        (false, Some(Origin::TerminalGoal(tok_id, ..))) => StateType::AssignToken(tok_id),
        (true, _) => StateType::Follow,
        (false, _) => StateType::CompleteToken,
      })
      .set_reduce_item(first.kernel)
      .set_follow_hash(follow_hash)
      .kernel_items(follow.iter().cloned());

    if is_continue {
      if completes_goal {
        state.make_enqueued_leaf()
      } else {
        state
      }
    } else {
      debug_assert!(
        !first.allow_assign || completes_goal,
        "Should complete a token goal {}",
        first.kernel._debug_string_w_db_(gb.db())
      );
      state.make_leaf()
    }
    .commit(gb);
  }
}

fn create_follow_hash(completed_items: &[Item]) -> u64 {
  let mut hasher = DefaultHasher::new();

  for item in completed_items {
    item.index().hash(&mut hasher);
    item.from.hash(&mut hasher);
    item.origin.hash(&mut hasher);
  }

  hasher.finish()
}
