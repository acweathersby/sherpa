use super::super::graph::*;
use crate::{types::*, utils::hash_group_btreemap};
use std::collections::{BTreeSet, VecDeque};

use GraphBuildState::*;

pub(crate) fn handle_nonterminal_shift<'a, 'db: 'a>(gb: &'a mut GraphBuilder<'db>) -> SherpaResult<bool> {
  if gb.is_scanner() || !gb.config.ALLOW_LR || gb.current_state_id().state().currently_peeking() {
    return Ok(false);
  };

  let mode = gb.get_mode();
  let db = gb.db;
  let kernel_base: ItemSet = gb.current_state().get_kernel_items().iter().inscope_items();

  let conanical: ItemSet =
    if gb.current_state_id().is_root() { kernel_base.iter().map(|i| i.to_canonical()).collect() } else { Default::default() };
  let conanical = &conanical; // Make a reference to allow its use within closures.

  let state_id = gb.current_state_id();
  let origin = Origin::Goto(state_id);

  let mut offset = kernel_base.iter().map(|i| i.lane.get_curr()).max().unwrap_or_default();
  let offset = &mut offset;

  let mut nterm_items = kernel_base.iter().nonterm_items::<ItemSet>(mode);
  nterm_items.extend(kernel_base.iter().filter(|i| !i.is_complete()).flat_map(|i| {
    let basis = i.to_origin(origin).to_origin_state(state_id);
    let closure = i
      .closure_iter_align_with_lane_split(basis)
      .filter(move |i| i.is_nonterm(mode) && !conanical.contains(&i.to_canonical()))
      .enumerate()
      .map(|(index, i)| (index > 0).then_some(i.to_goto_origin()).unwrap_or(i));
    *offset = closure.clone().map(|i| i.lane.get_curr()).max().unwrap_or_default();
    closure
  }));

  let out_items = gb.get_pending_items();

  let parent_id = gb.current_state_id();
  let is_at_root = parent_id.is_root();

  let out_items: ItemSet<'db> = if false && parent_id.is_root() {
    out_items
  } else {
    out_items.into_iter().filter(|i| i.origin_state == parent_id && (!kernel_base.contains(i) || i.is_start())).collect()
  };

  if out_items.is_empty()
  {
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

  for (target_nonterm, items) in &used_nterm_groups {
    let are_shifting_a_goal_nonterm = is_at_root && gb.graph().goal_items().iter().rule_nonterm_ids().contains(&target_nonterm);
    let contains_completed_kernel_items = items.iter().any(|i| kernel_base.contains(i) && i.is_penultimate());
    let contains_completed_items = items.iter().any(|i| i.is_penultimate());

    let mut incremented_items = items.iter().try_increment();
    let nterm_shift_type = StateType::NonTerminalShiftLoop;

    let should_include_oos = {
      let contains_left_recursive_items = items.iter().any(|i| i.is_left_recursive(mode));
      kernel_nterm_ids.remove(&target_nonterm) && is_at_root && contains_left_recursive_items && !contains_completed_items
    };

    // if there is a path to complete a kernel item, then we need to inject oos
    // lookahead items to ensure that we are not ignoring local ambiguity
    if should_include_oos {
      let local_nonterms = incremented_items.iter().nonterm_ids_at_index(mode);
      // This state completes this NonTerminal, but there is also one or more items
      // that transitions on the goal non-terminal. The trick is determining
      // whether we should complete the non-terminal or allow further processing the
      // left recursive items. This is a classic shift reduce problem, except
      // the condition to reduce is dependent on external items that we have to pull
      // into this scope. So we dump all items that shift on this
      // non-terminal into this state. We call this Out-of-Scope items and are
      // only used to determine if we should perform a reduction or a
      // completion.

      // We only need OOS items if there are no completed items after the non-terminal
      // transition. This will handle the cases of left-recursion.
      let canonical_incremented_items = incremented_items.iter().to_canonical::<ItemSet>();
      let oos_items = ItemSet::from_iter(
        db.nonterm_follow_items(*target_nonterm)
          .filter_map(|i| i.increment())
          .filter(|i| {
            i.nonterm_index_at_sym(mode) == Some(*target_nonterm)
              && i.nonterm_index() != *target_nonterm
              && !local_nonterms.contains(&i.nonterm_index())
              && !canonical_incremented_items.contains(&i.to_canonical())
          })
          .map(|i| i.to_origin(Origin::GoalCompleteOOS).to_origin_state(parent_id)),
      );

      incremented_items.extend(oos_items);
    }

    // A State following a goto point must either end with a return to that GOTO or
    // a completion of the gotos kernel items.

    if let Some(state) = gb
      .create_state(
        NormalGoto,
        (target_nonterm.to_sym(), 0).into(),
        nterm_shift_type,
        Some(incremented_items.into_iter().map(|i| i)),
      )
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

  gb.set_classification(ParserClassification { gotos_present: true, bottom_up: true, ..Default::default() });

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

  gb.iter_pending_states_mut(&|mut sb| {
    let peek_items = sb.state_ref().get_peek_resolve_items().map(|i| i.map(|(i, v)| (i, v.clone())).collect::<Vec<_>>());
    if let Some(peek_resolve_items) = peek_items {
      let old_kernel_items = sb.state_ref().get_kernel_items().clone();
      let mut new_kernel_items: Items = Default::default();
      for (v, items) in peek_resolve_items {
        let old_origin = Origin::Peek(v, sb.state_ref().id);
        let new_items = items
          .iter()
          .map(|i| if i.origin_state.0 != current_id.0 { i.increment_goto() } else { i.to_goto_origin() })
          .collect::<ItemSet>();

        let origin = sb.set_peek_resolve_state(&new_items);

        new_kernel_items.extend(old_kernel_items.iter().filter(|i| i.origin == old_origin).map(|i| i.to_origin(origin)));
      }

      sb.set_kernel_items(new_kernel_items.into_iter());
    } else {
      let items = sb
        .state_ref()
        .get_kernel_items()
        .iter()
        .map(|i| if i.origin_state.0 != current_id.0 { i.increment_goto() } else { i.to_goto_origin() })
        .collect::<Items>();

      sb.set_kernel_items(items.into_iter());
    }
  });
}
