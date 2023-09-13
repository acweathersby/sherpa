use super::super::graph::*;
use crate::{types::*, utils::hash_group_btreemap};
use std::collections::{BTreeSet, VecDeque};

use GraphBuildState::*;

pub(crate) fn handle_nonterminal_shift<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<bool> {
  if gb.is_scanner() || gb.current_state_id().state().currently_peeking() {
    return Ok(false);
  };

  let mode = gb.get_mode();
  let db = gb.db;
  let kernel_base: ItemSet = gb.current_state().kernel_items_ref().iter().inscope_items();
  let state_id = gb.current_state_id();
  let origin = Origin::Goto(state_id);

  let mut nterm_items = kernel_base.iter().nonterm_items::<ItemSet>(mode);
  nterm_items.extend(kernel_base.iter().filter(|i| !i.is_complete()).flat_map(|i| {
    db.get_closure(i)
      .filter(move |i| i.is_nonterm(mode))
      .map(move |a| a.to_goal(i.goal).to_origin(origin).to_origin_state(state_id).to_goto_origin())
  }));

  let out_items = gb.get_pending_items();

  let parent_id = gb.current_state_id();
  let is_at_root = parent_id.is_root();

  let out_items: ItemSet<'db> = if false && parent_id.is_root() {
    out_items
  } else {
    out_items.into_iter().filter(|i| i.origin_state == parent_id && (!kernel_base.contains(i) || i.is_start())).collect()
  };

  if out_items.is_empty() || (nterm_items.len() <= 1 && nterm_items.first().is_some_and(|i| !i.is_left_recursive(mode))) {
    return Ok(false);
  }

  // Get all the nonterminal symbols that are shifted in the kernel
  let mut kernel_nterm_ids = kernel_base.iter().nonterm_items::<ItemSet>(mode).iter().nonterm_ids_at_index(mode);
  kernel_nterm_ids.extend(kernel_base.iter().nonterm_items::<ItemSet>(mode).iter().rule_nonterm_ids());

  // NonTerms that appear in to the the right side of the specifier in
  // used_nonterm_items.

  let filter_nterms = false;

  let used_nterm_items = if filter_nterms { get_used_nonterms(gb, out_items, nterm_items, &kernel_base) } else { nterm_items };

  if used_nterm_items.is_empty() {
    return Ok(false);
  }

  gb.current_state_mut().set_nonterm_items(&used_nterm_items);

  let used_nterm_groups = hash_group_btreemap(used_nterm_items, |_, t| t.nonterm_index_at_sym(mode).unwrap_or_default());

  for (nterm, items) in &used_nterm_groups {
    let are_shifting_a_goal_nonterm = is_at_root && gb.graph().goal_items().iter().rule_nonterm_ids().contains(&nterm);
    let contains_completed_kernel_items = items.iter().any(|i| kernel_base.contains(i) && i.is_penultimate());

    let mut incremented_items = items
      .iter() /* .map(|i| i.calculate_goto_distance(gb, parent_id)) */
      .try_increment();
    let nterm_shift_type = StateType::NonTerminalShiftLoop;

    // TODO(anthony): Only need to do this type of look ahead if one of the
    // following apply
    // - There is a shift reduce conflict
    // - There is a reduce - reduce conflict
    if kernel_nterm_ids.remove(&nterm) {
      let contains_left_recursive_items = items.iter().any(|i| i.is_left_recursive(mode));

      if is_at_root && contains_left_recursive_items {
        let local_nonterms = incremented_items.iter().nonterm_ids_at_index(mode);
        //let item = Item::from_rule(db.nonterm_rules(*nterm)?[0], db);
        //incremented_items.push(item.to_complete().to_origin(Origin::GoalCompleteOOS).
        // to_oos_index().to_origin_state(parent));`

        // This state completes this NonTerminal, but there is also one or more items
        // that transitions on the goal non-terminal. The trick is determining
        // whether we should complete the non-terminal or allow further processing the
        // left recursive items. This is the classic shift reduce problem, except
        // the condition to reduce is dependent on external items that we have to pull
        // into this NonTerms scope. So we dump all items that shift on this
        // non-terminal into this state. We call this Out-of-Scope items and are
        // only used to determine if we should perform a reduction or a
        // completion.

        // Collects the follow terminal items items from a non-terminal
        // TODO(anthony): Move this to DB creation.
        let mut nonterms = Queue::from_iter([*nterm]);
        let mut seen = Set::from_iter([*nterm]);
        seen.extend(local_nonterms.iter());

        let mut oos_items = ItemSet::new();

        while let Some(_nterm) = nonterms.pop_front() {
          for item in db.nonterm_follow_items(_nterm) {
            let item = item.try_increment().to_origin_state(parent_id);
            match item.get_type() {
              ItemType::Completed(_nterm) => {
                if seen.insert(_nterm) {
                  nonterms.push_back(_nterm)
                }
              }
              ItemType::NonTerminal(non_terminal) => {
                if !local_nonterms.contains(&non_terminal) {
                  oos_items.extend(
                    db.nonterm_rules(non_terminal)?
                      .iter()
                      .map(|r| Item::from_rule(*r, db))
                      .closure::<Items>(parent_id)
                      .term_items(mode),
                  );
                }
              }
              ItemType::TokenNonTerminal(..) | ItemType::Terminal(..) => {
                oos_items.insert(item);
              }
            }
          }
        }

        let canonical_incremented_items = incremented_items.iter().to_canonical::<ItemSet>();

        let oos_items = oos_items
          .iter()
          .filter(|i| !canonical_incremented_items.contains(&i.to_canonical()))
          .map(|i| i.to_oos_index().to_origin(Origin::GoalCompleteOOS).to_origin_state(parent_id));

        // Only need to create a peek state if there are GoalCompleteOOS items that
        // transition on the same symbols as the incremented items.

        incremented_items.extend(oos_items);
      }
    }

    // A State following a goto point must either end with a return to that GOTO or
    // a completion of the gotos kernel items.

    if let Some(state) = gb
      .create_state(NormalGoto, (nterm.to_sym(), 0).into(), nterm_shift_type, Some(incremented_items.into_iter().map(|i| i)))
      .to_pending()
    {
      if are_shifting_a_goal_nonterm && !contains_completed_kernel_items {
        let mut new_state = gb.create_state::<DefaultIter>(
          GraphBuildState::Leaf,
          (SymbolId::Default, 0).into(),
          StateType::NonTermCompleteOOS,
          None,
        );
        new_state.set_parent(state);
        new_state.to_leaf();
      }
    }
  }

  // The remaining non-terminals are comprised of accept items for this state.
  for nonterm_id in kernel_nterm_ids {
    gb.create_state::<DefaultIter>(GraphBuildState::Leaf, (nonterm_id.to_sym(), 0).into(), StateType::NonTerminalComplete, None)
      .to_leaf();
  }

  increment_gotos(gb);

  SherpaResult::Ok(true)
}

fn get_used_nonterms<'db>(
  gb: &GraphBuilder<'db>,
  out_items: BTreeSet<Item<'db>>,
  nterm_items: BTreeSet<Item<'db>>,
  kernel_base: &BTreeSet<Item<'db>>,
) -> BTreeSet<Item<'db>> {
  let mut used_nterm_items = ItemSet::new();

  let mut seen = OrderedSet::new();
  let mut queue = VecDeque::from_iter(out_items.iter().map(|i| i.nonterm_index()));

  while let Some(nterm) = queue.pop_front() {
    if seen.insert(nterm) {
      for item in nterm_items.iter().filter(|i| i.nonterm_index_at_sym(gb.get_mode()).unwrap() == nterm) {
        used_nterm_items.insert(*item);
        if !kernel_base.contains(item) || item.is_at_initial() {
          queue.push_back(item.nonterm_index());
        }
      }
    }
  }

  used_nterm_items
}

fn increment_gotos(gb: &mut GraphBuilder) {
  let current_id = gb.current_state_id();

  for next_state_id in gb.get_pending_states() {
    if gb.get_state(next_state_id).peek_resolve_items.len() > 0 {
      let resolve_states = gb
        .get_state(next_state_id)
        .peek_resolve_items
        .iter()
        .map(|(id, i)| {
          (
            *id,
            i.iter()
              .map(|i| if i.origin_state.0 != current_id.0 { i.increment_goto() } else { i.to_goto_origin() })
              .collect::<ItemSet>(),
          )
        })
        .collect::<OrderedMap<_, _>>();
      gb.get_state_mut(next_state_id).peek_resolve_items = resolve_states;
    } else {
      let items = gb
        .get_state_mut(next_state_id)
        .kernel_items
        .iter()
        .map(|i| if i.origin_state.0 != current_id.0 { i.increment_goto() } else { i.to_goto_origin() })
        .collect::<Items>();

      set_kernel_items(gb.get_state_mut(next_state_id), items.into_iter())
    }
  }
}
