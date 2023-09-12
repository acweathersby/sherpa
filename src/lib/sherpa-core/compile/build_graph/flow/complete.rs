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
  let is_scan = gb.is_scanner();

  let first = completed[0];

  if first.kernel.origin == Origin::GoalCompleteOOS {
    gb.create_state::<DefaultIter>(Normal, sym, StateType::NonTermCompleteOOS, None).to_leaf()
  } else if is_scan {
    complete_scan(completed, gb, sym, first)
  } else {
    complete_regular(first, gb, sym)
  }

  SherpaResult::Ok(())
}

fn complete_regular<'db>(first: TransitionPair<'db>, gb: &mut GraphBuilder<'db>, sym: PrecedentSymbol) {
  let completed_item = first.kernel;
  let mut state = gb.create_state(
    Normal,
    sym,
    StateType::Reduce(completed_item.rule_id, completed_item.goto_distance as usize),
    Some([completed_item].into_iter()),
  );
  state.set_reduce_item(completed_item);
  state.to_leaf();
}

fn complete_scan<'db>(
  completed: Vec<TransitionPair<'db>>,
  gb: &mut GraphBuilder<'db>,
  sym: PrecedentSymbol,
  first: TransitionPair<'db>,
) {
  let (follow, completed_items): (Vec<Items>, Vec<Items>) = completed
    .iter()
    .to_inherited(gb.current_state_id())
    .into_iter()
    .map(|i| get_follow(gb, i.kernel).expect("could no get follow"))
    .unzip();

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
      (false, _) => StateType::Complete,
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

pub(crate) fn get_completed_item_artifacts<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  gb: &GraphBuilder<'db>,
  completed: T,
) -> SherpaResult<CompletedItemArtifacts<'db>> {
  let mut follow_pairs = OrderedSet::new();
  //let mut follow_items = ItemSet::new();
  let mut default_only_items = ItemSet::new();

  for c_i in completed {
    let (f, _) = get_follow(gb, *c_i)?;

    if f.is_empty() {
      default_only_items.insert(*c_i);
    } else {
      follow_pairs.extend(f.iter().flat_map(|i| vec![*i].iter().closure::<Vec<_>>(gb.current_state_id())).map(|i| {
        TransitionPair {
          next:   i.to_origin(c_i.origin),
          kernel: *c_i,
          prec:   i.token_precedence(),
          sym:    i.sym(),
        }
      }));
      //follow_items.append(&mut f.to_set());
    }
  }

  SherpaResult::Ok(CompletedItemArtifacts { follow_pairs, default_only: default_only_items })
}
