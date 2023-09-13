#![allow(unused)]

use super::super::graph::*;
use crate::{
  compile::build_graph::items::{get_follow, get_goal_items_from_completed},
  types::*,
};

use GraphBuildState::*;

pub(crate) fn handle_completed_item<'db, 'follow>(
  gb: &mut GraphBuilder<'db>,
  completed: Follows<'db>,
  sym: PrecedentSymbol,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();

  let first = completed[0];

  if first.kernel.origin == Origin::GoalCompleteOOS {
    gb.create_state::<DefaultIter>(Normal, sym, StateType::NonTermCompleteOOS, None).to_leaf()
  } else if ____is_scan____ {
    complete_scan(completed, gb, sym, first)
  } else {
    complete_regular(first, gb, sym)
  }

  SherpaResult::Ok(())
}

fn complete_regular<'db>(first: TransitionPair<'db>, gb: &mut GraphBuilder<'db>, sym: PrecedentSymbol) {
  let completed_item = first.kernel;
  let ____is_scan____ = gb.is_scanner();
  let ____allow_rd____: bool = gb.config.ALLOW_RECURSIVE_DESCENT || ____is_scan____;
  let ____allow_ra____: bool = gb.config.ALLOW_LR || ____is_scan____;
  let ____allow_fork____: bool = gb.config.ALLOW_FORKING && false;
  let ____allow_peek____: bool = gb.config.ALLOW_PEEKING;

  if !gb.graph().item_is_goal(&completed_item) && !completed_item.from_goto_origin {
    let (follow, completed_items) = get_follow(gb, completed_item, true).expect("could not get follow");

    if follow.len() < 1 && completed_items.len() < 1 {
      panic!("TODO")
    } else {
      let mut state = gb.create_state(
        Normal,
        sym,
        StateType::ReduceComplete(completed_item.rule_id, completed_item.goto_distance as usize),
        Some(
          follow
            .into_iter()
            .chain(completed_items.into_iter().filter(|i| !i.is_out_of_scope() && !i.is_canonically_equal(&completed_item))),
        ),
      );
      state.set_reduce_item(completed_item);
      state.to_pending();
    }
  } else {
    let mut state = gb.create_state(
      Normal,
      sym,
      StateType::Reduce(completed_item.rule_id, completed_item.goto_distance as usize),
      Some([completed_item].into_iter()),
    );
    state.set_reduce_item(completed_item);
    state.to_leaf();
  }
}

fn complete_scan<'db>(
  completed: Vec<TransitionPair<'db>>,
  gb: &mut GraphBuilder<'db>,
  sym: PrecedentSymbol,
  first: TransitionPair<'db>,
) {
  let (follow, completed_items): (Vec<Items>, Vec<Items>) =
    completed.iter().into_iter().map(|i| get_follow(gb, i.kernel, false).expect("could not get follow")).unzip();

  let follow = follow.into_iter().flatten().collect::<Items>();
  let completed_items = completed_items.into_iter().flatten().collect::<Items>();
  let goals = get_goal_items_from_completed(&completed_items, gb.graph());
  let is_continue = !follow.is_empty();
  let is_goal = !goals.is_empty();

  let mut state = gb.create_state(
    Normal,
    sym,
    match (is_continue, goals.first().map(|d| d.origin)) {
      (true, Some(Origin::TerminalGoal(tok_id, ..))) => StateType::AssignAndFollow(tok_id),
      (false, Some(Origin::TerminalGoal(tok_id, ..))) => StateType::AssignToken(tok_id),
      (true, _) => StateType::Follow,
      (false, _) => StateType::CompleteToken,
    },
    Some(follow.iter().cloned()),
  );

  state.set_reduce_item(first.kernel);

  if is_continue {
    if is_goal {
      state.to_enqueued_leaf();
    } else {
      state.to_enqueued();
    }
  } else {
    state.to_leaf()
  }
}
