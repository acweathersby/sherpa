use super::{
  errors::lr_disabled_error,
  flow::{
    get_completed_item_artifacts,
    handle_bread_crumb_complete_groups,
    handle_peek_complete_groups,
    handle_peek_incomplete_items,
    handle_peg_complete_groups,
    handle_regular_complete_groups,
    handle_regular_incomplete_items,
  },
  graph::*,
  items::{get_oos_follow_from_completed, merge_follow_items_into_group, merge_occluding_token_items},
};
use crate::{
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::{BTreeSet, VecDeque};

use GraphBuildState::*;

pub(crate) type TransitionGroup<'db> = (u16, Vec<TransitionPair<'db>>);
pub(crate) type GroupedFirsts<'db> = OrderedMap<SymbolId, TransitionGroup<'db>>;

pub(crate) fn handle_kernel_items(gb: &mut GraphBuilder) -> SherpaResult<()> {
  let mut groups = get_firsts(gb)?;

  let max_precedence = handle_completed_items(gb, &mut groups)?;

  let groups = handle_scanner_items(max_precedence, gb, groups)?;

  handle_incomplete_items(gb, groups)?;

  handle_goto_states(gb)?;

  Ok(())
}

// Iterate over each item's closure and collect the terminal transition symbols
// of each item. The item's are then catagorized by these nonterminal symbols.
// Completed items are catagorized by the default symbol.
fn get_firsts<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<GroupedFirsts<'db>> {
  let iter = gb.current_state().kernel_items_ref().iter().flat_map(|i| {
    i.closure_iter().term_items_iter(gb.is_scanner()).map(|t_item| -> TransitionPair { (*i, t_item, gb.get_mode()).into() })
  });

  let groups = hash_group_btree_iter::<Vec<_>, _, _, _, _>(iter, |_, first| first.sym);

  let groups: OrderedMap<SymbolId, (u16, Vec<TransitionPair<'db>>)> =
    groups.into_iter().map(|(s, g)| (s, (g.iter().map(|f| f.prec).max().unwrap_or_default(), g))).collect();

  SherpaResult::Ok(groups)
}

fn handle_goto_states<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<()> {
  let should_process_gotos =
    if !gb.is_scanner() && !gb.state_id().state().currently_peeking() { handle_nonterminal_shift(gb)? } else { false };

  if should_process_gotos && !gb.config.ALLOW_LR {
    lr_disabled_error(gb, gb.current_state().nonterm_items.clone().to_vec())?;
  }

  gb.process_pending(should_process_gotos);

  Ok(())
}

fn handle_scanner_items<'db>(
  max_precedence: u16,
  gb: &GraphBuilder<'db>,
  mut groups: GroupedFirsts<'db>,
) -> SherpaResult<GroupedFirsts<'db>> {
  if gb.is_scanner() {
    if max_precedence > CUSTOM_TOKEN_PRECEDENCE_BASELINE {
      groups = groups
        .into_iter()
        .filter_map(|(s, (p, g))| {
          if s == SymbolId::Default {
            // Completed items are an automatic pass
            Some((s, (p, g)))
          } else {
            let g = g.into_iter().filter(|i| i.prec >= max_precedence).collect::<Vec<_>>();
            if g.is_empty() {
              None
            } else {
              Some((s, (p, g)))
            }
          }
        })
        .collect();
    }

    merge_occluding_token_items(groups.clone(), &mut groups);
  }

  Ok(groups)
}

fn handle_nonterminal_shift<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<bool> {
  let mode = gb.get_mode();
  let db = gb.db;
  let kernel_base: ItemSet = gb.current_state().kernel_items_ref().iter().inscope_items();

  let mut nterm_items = kernel_base.iter().nonterm_items::<ItemSet>(mode);
  nterm_items.extend(kernel_base.iter().filter(|i| !i.is_complete()).flat_map(|i| {
    let a = i.to_owned().to_canonical();
    let b = i.to_owned();
    let state_id = gb.state_id();
    db.get_closure(i)
      .filter(move |i| i.is_nonterm(mode) && i.to_canonical() != a)
      .map(move |a| a.align(&b).to_origin_state(state_id))
  }));

  let out_items = gb.get_pending_items();

  let parent_id = gb.state_id();
  let is_at_root = parent_id.is_root();

  let out_items: ItemSet<'db> = if false && parent_id.is_root() {
    out_items
  } else {
    out_items.into_iter().filter(|i| i.origin_state == parent_id && (!kernel_base.contains(i) || i.is_start())).collect()
  };

  if out_items.is_empty() || nterm_items.is_empty() {
    return Ok(false);
  }

  // Get all the nonterminal symbols that are shifted in the kernel
  let mut kernel_nterm_ids = kernel_base.iter().nonterm_items::<ItemSet>(mode).iter().nonterm_ids_at_index(mode);
  kernel_nterm_ids.extend(kernel_base.iter().nonterm_items::<ItemSet>(mode).iter().rule_nonterm_ids());

  // NonTerms that appear in to the the right side of the specifier in
  // used_nonterm_items.
  let used_nterm_items = get_used_nonterms(gb, out_items, nterm_items, &kernel_base);

  if used_nterm_items.is_empty() {
    return Ok(false);
  }

  gb.current_state_mut().set_nonterm_items(&used_nterm_items);

  let used_nterm_groups = hash_group_btreemap(used_nterm_items, |_, t| t.nonterm_index_at_sym(mode).unwrap_or_default());

  for (nterm, items) in &used_nterm_groups {
    let are_shifting_a_goal_nonterm = is_at_root && gb.graph().goal_items().iter().rule_nonterm_ids().contains(&nterm);
    let contains_completed_kernel_items = items.iter().any(|i| kernel_base.contains(i) && i.is_penultimate());

    let mut incremented_items = items.iter().map(|i| i.calculate_goto_distance(gb, parent_id)).try_increment();
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

    if let Some(state) =
      gb.create_state(NormalGoto, (nterm.to_sym(), 0).into(), nterm_shift_type, incremented_items).to_enqueued()
    {
      if are_shifting_a_goal_nonterm && !contains_completed_kernel_items {
        let mut new_state = gb.create_state(
          GraphBuildState::Leaf,
          (SymbolId::Default, 0).into(),
          StateType::NonTermCompleteOOS,
          Default::default(),
        );
        new_state.set_parent(state);
        new_state.to_leaf();
      }
    }
  }

  // The remaining non-terminals are comprised of accept items for this state.
  for nonterm_id in kernel_nterm_ids {
    gb.create_state(GraphBuildState::Leaf, (nonterm_id.to_sym(), 0).into(), StateType::NonTerminalComplete, vec![]).to_leaf();
  }

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

  nterm_items.clone()
}

fn handle_incomplete_items<'nt_set, 'db: 'nt_set>(gb: &mut GraphBuilder<'db>, groups: GroupedFirsts<'db>) -> SherpaResult<()> {
  for (sym, group) in groups {
    let ____is_scan____ = gb.is_scanner();
    let prec_sym: PrecedentSymbol = (sym, group.0).into();

    match gb.state_id().state() {
      BreadCrumb(_level) => {
        todo!("Complete breadcrumb parsing");
      }
      PEG => {
        todo!("Complete peg parsing");
      }
      Peek(level) => handle_peek_incomplete_items(gb, prec_sym, group, level),
      _REGULAR_ => handle_regular_incomplete_items(gb, prec_sym, group),
    }?;
  }
  Ok(())
}

fn handle_completed_items<'db>(gb: &mut GraphBuilder<'db>, groups: &mut GroupedFirsts<'db>) -> SherpaResult<u16> {
  let ____is_scan____ = gb.is_scanner();
  let mut max_precedence = 0;

  if let Some(completed) = groups.remove(&SymbolId::Default) {
    max_precedence = max_precedence.max(completed.0);

    let CompletedItemArtifacts { mut follow_pairs, default_only, .. } =
      get_completed_item_artifacts(gb, completed.1.iter().map(|i| &i.kernel))?;

    if ____is_scan____ {
      //gb.add_kernel_items(follow_pairs.iter().to_next().filter(|i|
      // !i.is_complete()).cloned().collect::<Vec<_>>());
      /*       merge_occluding_token_items(
        hash_group_btree_iter::<Vec<_>, _, _, _, _>(follow_pairs.iter(), |_, item| match item.next.get_type() {
          ItemType::Terminal(sym) => sym,
          _ => SymbolId::Undefined,
        })
        .into_iter()
        .map(|(g, s)| (g, (0u16, s)))
        .collect(),
        groups,
      ); */
      /* get_oos_follow_from_completed(gb, &completed.1.iter().to_kernel().to_vec(), &mut |follow| {
        merge_follow_items_into_group(&follow, gb.state_id(), groups)
      })?; */
    } else {
      follow_pairs = follow_pairs
        .into_iter()
        .map(|pair| {
          if true {
            (pair.kernel, pair.next.to_oos_index().to_origin(Origin::GoalCompleteOOS), gb.get_mode()).into()
          } else {
            pair
          }
        })
        .collect();
    }

    let contains_in_scope_items = completed.1.iter().any(|i| !i.next.goal_is_oos());

    let default: Follows = if contains_in_scope_items {
      completed.1.iter().filter(|i| !i.is_out_of_scope()).cloned().collect()
    } else {
      completed.1.iter().map(|i| -> TransitionPair<'db> { (i.kernel, i.kernel, gb.get_mode()).into() }).collect()
    };

    handle_completed_groups(gb, groups, SymbolId::Default, default, &default_only)?;

    if !follow_pairs.is_empty() {
      // Create reduce states for follow items that have not already been covered.
      let mut completed_groups = hash_group_btree_iter(follow_pairs.into_iter(), |_, fp| match fp.next.get_type() {
        ItemType::Completed(_) => {
          unreachable!("Should be handled outside this path")
        }
        ItemType::TokenNonTerminal(_, sym) if !gb.is_scanner() => sym,
        ItemType::Terminal(sym) => sym,
        _ => SymbolId::Undefined,
      });

      completed_groups.remove(&SymbolId::Undefined);

      for (sym, follow_pairs) in completed_groups {
        handle_completed_groups(gb, groups, sym, follow_pairs, &default_only)?;
      }
    }
  }

  SherpaResult::Ok(max_precedence)
}

pub(crate) fn handle_completed_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  sym: SymbolId,
  follow_pairs: Follows<'db>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let prec_sym: PrecedentSymbol = (sym, follow_pairs.iter().max_precedence()).into();

  match gb.state_id().state() {
    GraphBuildState::PEG => handle_bread_crumb_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
    GraphBuildState::BreadCrumb(_) => handle_peg_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
    GraphBuildState::Peek(_) => handle_peek_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
    _REGULAR_ => handle_regular_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
  }
}
