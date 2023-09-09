#![allow(unused)]

use super::super::graph::*;
use crate::{
  compile::build_graph::items::{get_follow, get_goal_items_from_completed},
  types::*,
};

use GraphState::*;

pub(crate) fn handle_completed_item<'db, 'follow>(
  gb: &mut GraphBuilder<'db>,
  (completed_item, completed_items): (Item<'db>, Items<'db>),
  sym: PrecedentSymbol,
) -> SherpaResult<()> {
  let is_scan = gb.is_scanner();

  // Determine if origin contains GOTOs.
  match (completed_item.origin, is_scan) {
    (Origin::GoalCompleteOOS, _) => {
      gb.create_state(Normal, sym, StateType::NonTermCompleteOOS, vec![]).to_leaf();
    }
    // Completion of parse tree may be premature
    // or item is not an acceptable completed item
    (_, true) => {
      let (follow, completed_items): (Vec<Items>, Vec<Items>) = completed_items
        .into_iter()
        .map(|i| {
          let Ok(result) = get_follow(gb, i) else { unreachable!("could not get follow data") };
          result
        })
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
        follow.clone(),
      );

      state.set_reduce_item(completed_item);

      if is_continue {
        if is_goal {
          state.enque_leaf();
        } else {
          state.enque();
        }
      } else {
        state.to_leaf()
      }
    }
    // Normal reduction state with no other actions.
    _ => {
      let mut state =
        gb.create_state(Normal, sym, StateType::Reduce(completed_item.rule_id, completed_item.goto_distance as usize), vec![]);
      state.set_reduce_item(completed_item);
      state.to_leaf();
    }
  }

  SherpaResult::Ok(())
}

pub(crate) fn get_completed_item_artifacts<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  gb: &GraphBuilder<'db>,
  completed: T,
) -> SherpaResult<CompletedItemArtifacts<'db>> {
  let mut follow_pairs = OrderedSet::new();
  let mut follow_items = ItemSet::new();
  let mut default_only_items = ItemSet::new();

  for c_i in completed {
    let (f, _) = get_follow(gb, *c_i)?;

    if f.is_empty() {
      default_only_items.insert(*c_i);
    } else {
      follow_pairs.extend(
        f.iter()
          .flat_map(|i| vec![*i].iter().closure::<Vec<_>>(gb.state_id()))
          .map(|i| FollowPair::from((*c_i, i.to_origin(c_i.origin)))),
      );
      follow_items.append(&mut f.to_set());
    }
  }

  SherpaResult::Ok(CompletedItemArtifacts { follow_items, follow_pairs, default_only: default_only_items })
}
