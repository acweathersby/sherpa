use super::{
  call::{create_call, create_kernel_call},
  errors::{conflicting_symbols_error, create_reduce_reduce_error, lr_disabled_error},
  graph::*,
  items::{
    get_follow,
    get_goal_items_from_completed,
    get_oos_follow_from_completed,
    merge_items_into_groups,
    merge_occluding_token_items,
    peek_items_are_from_goto_state,
  },
  peek::{get_kernel_items_from_peek_item, get_kernel_items_from_peek_origin},
};
use crate::{
  compile::build_graph::peek::create_peek,
  types::*,
  utils::{hash_group_btreemap, hash_group_btreemap_iter},
};
use std::collections::{BTreeSet, HashSet, VecDeque};

use GraphState::*;

pub(crate) type TransitionGroup<'db> = (u16, ItemSet<'db>);
pub(crate) type TransitionGroups<'db> = OrderedMap<SymbolId, TransitionGroup<'db>>;

pub(crate) fn handle_kernel_items(gb: &mut GraphBuilder) -> SherpaResult<()> {
  if false
    && gb.config.ALLOW_RECURSIVE_DESCENT_CALLS
    && !gb.is_scanner()
    && gb.state_id().state() != NormalGoto
    && create_kernel_call(gb, (SymbolId::Default, 0).into()).is_some()
  {
    Ok(())
  } else {
    let mut groups = create_transition_groups(gb)?;

    let max_precedence = handle_completed_items(gb, &mut groups)?;

    let groups = handle_scanner_items(max_precedence, gb, groups)?;

    handle_incomplete_items(gb, groups)?;

    handle_goto_states(gb)?;

    Ok(())
  }
}

fn handle_goto_states<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<()> {
  let should_process_gotos =
    if !gb.is_scanner() && !gb.state_id().state().currently_peeking() { handle_nonterminal_shift(gb)? } else { false };

  if should_process_gotos && !gb.config.ALLOW_LR_RECURSIVE_ASCENT {
    lr_disabled_error(gb)?;
  }

  gb.process_pending(should_process_gotos);

  Ok(())
}

fn handle_scanner_items<'db>(
  max_precedence: u16,
  gb: &GraphBuilder<'db>,
  mut groups: TransitionGroups<'db>,
) -> SherpaResult<TransitionGroups<'db>> {
  if gb.is_scanner() {
    if max_precedence > CUSTOM_TOKEN_PRECEDENCE_BASELINE {
      groups = groups
        .into_iter()
        .filter_map(|(s, (p, g))| {
          if s == SymbolId::Default {
            // Completed items are an automatic pass
            Some((s, (p, g)))
          } else {
            let g = g.into_iter().filter(|i| i.token_precedence() >= max_precedence).collect::<BTreeSet<_>>();
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
  let db = gb.db;
  let nterm_items: ItemSet = gb.current_state().get_closure_ref()?.clone().nonterm_items().inscope_items();
  let kernel_base: ItemSet = gb.current_state().kernel_items_ref().iter().inscope_items();
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
  let mut kernel_nterm_ids = kernel_base.iter().nonterm_items::<ItemSet>().iter().nonterm_ids_at_index();
  kernel_nterm_ids.extend(kernel_base.iter().nonterm_items::<ItemSet>().iter().rule_nonterm_ids());

  // NonTerms that appear in to the the right side of the specifier in
  // used_nonterm_items.
  let used_nterm_items = get_used_nonterms(out_items, nterm_items, &kernel_base);

  if used_nterm_items.is_empty() {
    return Ok(false);
  }

  gb.current_state_mut().set_nonterm_items(&used_nterm_items);

  let used_nterm_groups = hash_group_btreemap(used_nterm_items, |_, t| t.nonterm_index_at_sym().unwrap_or_default());

  for (nterm, items) in &used_nterm_groups {
    let are_shifting_a_goal_nonterm = is_at_root && gb.graph().goal_items().iter().rule_nonterm_ids().contains(&nterm);
    let contains_completed_kernel_items = items.iter().any(|i| kernel_base.contains(i) && i.is_penultimate());

    let mut incremented_items = items.iter().map(|i| i.calculate_goto_distance(gb, parent_id)).try_increment();
    let nterm_shift_type = StateType::NonTerminalShiftLoop;

    let mut create_peek_state = false;
    // TODO(anthony): Only need to do this type of look ahead if one of the
    // following apply
    // - There is a shift reduce conflict
    // - There is a reduce - reduce conflict
    if kernel_nterm_ids.remove(&nterm) {
      let contains_left_recursive_items = items.iter().any(|i| i.is_left_recursive());

      if is_at_root && contains_left_recursive_items {
        let local_nonterms = incremented_items.iter().nonterm_ids_at_index();
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
                      .term_items(),
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

        // Compare closures and flag if any tokens in oos_items conflict with tokens in
        // incremented items
        create_peek_state = oos_items.clone().count() > 0 && {
          oos_items
            .clone()
            .terminals()
            .intersection(&incremented_items.iter().closure::<ItemSet>(parent_id).iter().terminals())
            .next()
            .is_some()
        };

        // Only need to create a peek state if there are GoalCompleteOOS items that
        // transition on the same symbols as the incremented items.

        incremented_items.extend(oos_items);
      }
    }

    // A State following a goto point must either end with a return to that GOTO or
    // a completion of the gotos kernel items.

    if let Some(state) = if create_peek_state {
      let CompletedItemArtifacts { default_only, follow_pairs, .. } =
        get_completed_item_artifacts(gb, incremented_items.iter().completed_items::<Vec<_>>().iter())?;
      let default = default_only.iter().map(|i| FollowPair::from((*i, *i))).collect::<Vec<_>>();
      let follow_pairs = follow_pairs.iter().chain(default.iter());

      let state = create_peek(
        gb,
        (nterm.to_sym(), 0).into(),
        &incremented_items.iter().incomplete_items::<Vec<_>>().iter(),
        Some(follow_pairs),
        false,
        nterm_shift_type,
      )?;

      gb.enqueue_pending_state(state)
    } else {
      gb.create_state(NormalGoto, (nterm.to_sym(), 0).into(), nterm_shift_type, incremented_items).enque()
    } {
      if are_shifting_a_goal_nonterm && !contains_completed_kernel_items {
        let mut new_state =
          gb.create_state(GraphState::Leaf, (SymbolId::Default, 0).into(), StateType::NonTermCompleteOOS, Default::default());
        new_state.set_parent(state);
        new_state.to_leaf();
      }
    }
  }

  // The remaining non-terminals are comprised of accept items for this state.
  for nonterm_id in kernel_nterm_ids {
    gb.create_state(GraphState::Leaf, (nonterm_id.to_sym(), 0).into(), StateType::NonTerminalComplete, vec![]).to_leaf();
  }

  SherpaResult::Ok(true)
}

fn get_used_nonterms<'db>(
  out_items: BTreeSet<Item<'db>>,
  nterm_items: BTreeSet<Item<'db>>,
  kernel_base: &BTreeSet<Item<'db>>,
) -> BTreeSet<Item<'db>> {
  let mut used_nterm_items = ItemSet::new();

  let mut seen = OrderedSet::new();
  let mut queue = VecDeque::from_iter(out_items.iter().map(|i| i.nonterm_index()));

  while let Some(nterm) = queue.pop_front() {
    if seen.insert(nterm) {
      for item in nterm_items.iter().filter(|i| i.nonterm_index_at_sym().unwrap() == nterm) {
        used_nterm_items.insert(*item);
        if !kernel_base.contains(item) || item.is_at_initial() {
          queue.push_back(item.nonterm_index());
        }
      }
    }
  }
  //used_nterm_items

  nterm_items.clone()
}

fn handle_incomplete_items<'nt_set, 'db: 'nt_set>(gb: &mut GraphBuilder<'db>, groups: TransitionGroups<'db>) -> SherpaResult<()> {
  for (sym, group) in groups {
    handle_incomplete_items_internal(gb, sym, group)?;
  }
  Ok(())
}

fn handle_incomplete_items_internal<'nt_set, 'db: 'nt_set>(
  gb: &mut GraphBuilder<'db>,
  sym: SymbolId,
  (prec, group): TransitionGroup<'db>,
) -> SherpaResult<()> {
  let is_scan = gb.is_scanner();
  let prec_sym = (sym, prec).into();

  if let Peek(level) = gb.state_id().state() {
    if group.iter().all_items_are_from_same_peek_origin() {
      resolve_peek(gb, group.iter(), prec_sym)?;
    } else {
      gb.create_state(Peek(level + 1), prec_sym, StateType::Peek, group.try_increment()).enque();
    }
  } else {
    let out_of_scope = group.clone().outscope_items().to_vec();
    let in_scope = group.clone().inscope_items();
    match (in_scope.len(), out_of_scope.len()) {
      (0, 1..) => {
        create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
      }
      (_, 1..) if is_scan => {
        create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
      }
      (1.., 1..) => {
        let pending_state = create_peek(
          gb,
          prec_sym,
          &group.iter(),
          None::<std::collections::btree_set::Iter<'_, FollowPair<'_>>>,
          true,
          StateType::Peek,
        )?;
        gb.add_pending(pending_state);
      }
      (1.., _) => {
        if let Some((pending_state, _)) = (gb.config.ALLOW_RECURSIVE_DESCENT_CALLS || gb.is_scanner())
          .then(|| create_call(gb, in_scope.iter(), prec_sym))
          .flatten()
        {
          gb.add_pending(pending_state);
        } else {
          let prec_sym = (sym, in_scope.iter().get_max_token_precedence()).into();
          gb.create_state(Normal, prec_sym, StateType::Shift, in_scope.try_increment()).to_pending();
        }
      }
      _ => unreachable!(),
    }
  }
  SherpaResult::Ok(())
}

fn handle_completed_items<'db>(gb: &mut GraphBuilder<'db>, groups: &mut TransitionGroups<'db>) -> SherpaResult<u16> {
  let is_scan = gb.is_scanner();
  let mut max_precedence = 0;

  if let Some(completed) = groups.remove(&SymbolId::Default) {
    max_precedence = max_precedence.max(completed.0);

    let CompletedItemArtifacts { mut follow_pairs, follow_items, default_only, .. } =
      get_completed_item_artifacts(gb, completed.1.iter())?;

    if is_scan {
      gb.add_kernel_items(follow_items);

      merge_occluding_token_items(
        hash_group_btreemap(follow_pairs.iter().map(|fp| fp.follow).collect::<ItemSet>(), |_, item| match item.get_type() {
          ItemType::Terminal(sym) => sym,
          _ => SymbolId::Undefined,
        })
        .into_iter()
        .map(|(g, s)| (g, (0, s)))
        .collect(),
        groups,
      );

      get_oos_follow_from_completed(gb, &completed.1.iter().to_vec(), &mut |follow: Items<'db>| {
        merge_items_into_groups(&follow, gb.state_id(), groups)
      })?;
    } else {
      follow_pairs = follow_pairs
        .into_iter()
        .map(|FollowPair { completed, follow }| {
          if true { (completed, follow.to_oos_index().to_origin(Origin::GoalCompleteOOS)) } else { (completed, follow) }.into()
        })
        .collect();
    }

    let contains_in_scope_items = completed.1.iter().any(|i| !i.is_out_of_scope());

    let default = if contains_in_scope_items {
      completed.1.iter().filter(|i| !i.is_out_of_scope()).map(|i| -> FollowPair<'db> { (*i, *i).into() }).collect()
    } else {
      completed.1.iter().map(|i| -> FollowPair<'db> { (*i, *i).into() }).collect()
    };

    handle_completed_groups(gb, groups, SymbolId::Default, default, &default_only)?;

    if !follow_pairs.is_empty() {
      // Create reduce states for follow items that have not already been covered.
      let mut completed_groups = hash_group_btreemap(follow_pairs.clone(), |_, fp| match fp.follow.get_type() {
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

fn handle_completed_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut TransitionGroups<'db>,
  sym: SymbolId,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let is_scan = gb.is_scanner();
  let mut cmpl = follow_pairs.iter().to_completed_vec();
  let prec = follow_pairs.iter().to_follow_vec().iter().get_max_precedence(gb.get_mode());
  let prec_sym: PrecedentSymbol = (sym, prec).into();

  if let GraphState::Peek(_) = gb.state_id().state() {
    match (follow_pairs.len(), groups.remove(&sym)) {
      (1, None) => {
        resolve_peek(gb, cmpl.iter(), prec_sym)?;
      }
      (_, None)
        if cmpl.iter().all_are_out_of_scope()
          || cmpl.iter().all_items_are_from_same_peek_origin()
          || peek_items_are_from_goto_state(&cmpl, gb.graph()) =>
      {
        if !cmpl.iter().all_are_out_of_scope() {
          cmpl = cmpl.into_iter().filter(|i| !i.is_out_of_scope()).collect();
        }

        resolve_peek(gb, cmpl.iter(), prec_sym)?;
      }
      // More than one completed items from peeking.
      (_, None) => {
        let mut targets = hash_group_btreemap_iter::<Vec<_>, _, _, _, _>(
          follow_pairs
            .iter()
            .map(|i| i.completed.origin)
            .collect::<HashSet<_>>()
            .into_iter()
            .map(|origin| get_kernel_items_from_peek_origin(gb.graph(), origin)),
          |_, items| {
            if items.iter().all_are_out_of_scope() {
              0
            } else if items.iter().all(|i| i.is_complete()) {
              1
            } else {
              2
            }
          },
        );

        let _oos_targets = targets.remove(&0);
        let _cmpl_targets = targets.remove(&1);
        let incpl_targets = targets.remove(&2);

        // Prefer shift.
        if incpl_targets.as_ref().is_some_and(|t| t.len() == 1) {
          let items = incpl_targets.unwrap().into_iter().next().unwrap();
          gb.create_state(NormalGoto, prec_sym, StateType::PeekEndComplete, items.to_vec()).enque();
        } else {
          // If the number of resolve states is two and one of the states is oos then
          // resolve to the none oos state.

          #[cfg(debug_assertions)]
          {
            let kernel_items =
              follow_pairs.iter().map(|fp| get_kernel_items_from_peek_item(gb, &fp.completed)).collect::<OrderedSet<_>>();
            let db = gb.db;
            crate::test::utils::write_debug_file(db, "parse_graph.tmp", gb.graph().debug_string(), true)?;
            unimplemented!(
            "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParent State\n{}\n\n",

            sym.debug_string(gb.db),
            gb.graph().goal_items().to_debug_string( "\n"),
            cmpl.to_debug_string("\n"),
            kernel_items.iter().map(|s| s.to_debug_string("\n")).collect::<Vec<_>>().join("\n"),
            gb.current_state().debug_string(gb.db),
            //graph.debug_string()
          );
          }
          #[cfg(not(debug_assertions))]
          unimplemented!()
        }
      }

      (_, Some((_, group))) => {
        let mut combined = group.clone();
        combined.append(&mut follow_pairs.iter().to_follow_set());

        if combined.iter().all_items_are_from_same_peek_origin() {
          resolve_peek(gb, combined.iter(), prec_sym)?;
        } else {
          #[cfg(debug_assertions)]
          todo!(
          "Roll the follow states into the group and resubmit to incomplete handler function.\nincomplete:\n{}\ncomplete:\n{}\nfollow:\n{}\n \n {}",
          group.to_debug_string("\n"),
          follow_pairs.iter().to_completed_vec().to_debug_string( "\n"),
          follow_pairs.iter().to_follow_vec().to_debug_string("\n"),
          gb.graph().debug_string()
        );

          #[cfg(not(debug_assertions))]
          todo!()
        }
      }
    }
  } else {
    // Non-Peeking States
    match (follow_pairs.len(), groups.remove(&sym)) {
      (1, None) => {
        handle_completed_item(gb, (cmpl[0], cmpl), prec_sym)?;
      }
      (2.., None) => {
        if is_scan {
          // We may be able to continue parsing using follow items, after we
          // determine whether we have symbol ambiguities.
          resolve_conflicting_tokens(gb, sym, cmpl.to_set())?;
        } else if cmpl.iter().to_absolute::<ItemSet>().len() == 1 {
          // The same non-terminal is generated from this completed item, regardless
          // of the origins. This is a valid outcome.
          handle_completed_item(gb, (cmpl[0], vec![cmpl[0]]), prec_sym)?;
        } else if cmpl.iter().all_are_out_of_scope() {
          // We are at the end of a lookahead that results in the completion of
          // some existing item.
          let item: Item<'_> = *o_to_r(cmpl.first(), "Item list is empty")?;
          handle_completed_item(gb, (item, vec![item]), prec_sym)?;
        } else {
          let unfollowed_items: Items = default_only_items.intersection(&cmpl.iter().to_set()).cloned().collect();

          match unfollowed_items.len() {
            2.. => {
              // Only an issue if there are no follow actions.
              // And even then, we could try using peek states to handle this
              // outcome. Reduce Reduce conflict
              // ------------------------------------------------------
              // Could pick a specific item to reduce based on precedence or some
              // kind of exclusive weight. Perhaps also walking back
              // up the graph to find a suitable divergent point and add a
              // fork. This would also be a good point to focus on breadcrumb
              // parsing strategies.
              return Err(create_reduce_reduce_error(gb, cmpl.to_set()));
            }
            1 => {
              let out = unfollowed_items;
              let cmpl_item = out[0];
              handle_completed_item(gb, (cmpl_item, out), prec_sym)?;
            }
            0 => {
              //There are lookahead(k=1) style states available, so we don't
              // need to generate a default state.
            }
            _ => unreachable!(),
          };
        }
      }
      (_, Some((prec, group))) => {
        if is_scan {
          let mut group = group.to_vec();
          let item = cmpl[0];
          group.append(&mut cmpl);
          handle_completed_item(gb, (item, group), prec_sym)?;
        } else if group.iter().all_are_out_of_scope() && cmpl.iter().all_are_out_of_scope() {
          cmpl.append(&mut group.to_vec());
          create_out_of_scope_complete_state(gb, cmpl, prec_sym);
        } else {
          // WE can use precedence to resolve shift reduce conflicts.  For now favor
          // shift.

          match resolve_shift_reduce_conflict(gb, group.iter(), cmpl.iter()) {
            ConflictResolution::Shift => {
              groups.insert(sym, (prec, group));
            }
            ConflictResolution::Reduce => {
              handle_completed_groups(gb, &mut Default::default(), sym, follow_pairs, &cmpl.to_set())?;
            }
            ConflictResolution::Peek => {
              let state = create_peek(gb, prec_sym, &group.iter(), Some(follow_pairs.iter()), true, StateType::Peek)?;
              gb.add_pending(state);
            }
          }
        }
      }
      (_len, _collide) => {
        #[cfg(debug_assertions)]
        unimplemented!(
          "\nNot Implemented: {:?} len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
          gb.state_id().state(),
          sym.debug_string(gb.db),
          cmpl.to_debug_string("\n"),
          gb.graph().debug_string()
        );
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }
  }

  SherpaResult::Ok(())
}

enum ConflictResolution {
  Shift,
  Reduce,
  Peek,
}

fn resolve_shift_reduce_conflict<
  'a,
  'db: 'a,
  A: Clone + ItemRefContainerIter<'a, 'db>,
  B: Clone + ItemRefContainerIter<'a, 'db>,
>(
  iter: &GraphBuilder<'db>,
  incom_items: A,
  comp1_items: B,
) -> ConflictResolution {
  let incom_nterm_set = incom_items.clone().rule_nonterm_ids();
  let compl_nterm_set = comp1_items.clone().map(|i| i.decrement().unwrap()).nonterm_ids_at_index();
  let nterm_sets_are_equal = incom_nterm_set.len() == 1 && incom_nterm_set.is_superset(&compl_nterm_set);

  if nterm_sets_are_equal && {
    // If all the shift items reduce to the same nterm and all completed items where
    // completed after shifting the same nterm, then the precedence of the shift
    // items determines whether we should shift first or reduce.
    let compl_prec = comp1_items.clone().get_max_precedence(iter.get_mode());
    let incom_prec = incom_items.clone().get_max_precedence(iter.get_mode());

    incom_prec >= compl_prec
  } {
    ConflictResolution::Shift
  } else if nterm_sets_are_equal {
    ConflictResolution::Reduce
  } else {
    ConflictResolution::Peek
  }
}

fn resolve_conflicting_tokens<'db, 'follow>(
  gb: &mut GraphBuilder<'db>,
  sym: SymbolId,
  completed_items: ItemSet<'db>,
) -> SherpaResult<()> {
  // Map items according to their symbols
  let token_precedence_groups =
    hash_group_btreemap(completed_items.clone(), |_, i| (i.origin_precedence(), i.origin.get_symbol(gb.db)));

  let base_precedence_groups = hash_group_btreemap(token_precedence_groups, |_, ((_, sym), _)| sym.base_token_precedence());

  if let Some((_, groups)) = base_precedence_groups.into_iter().rev().next() {
    let mut _completed: Option<&ItemSet> = None;

    if groups.len() == 1 {
      _completed = Some(groups.values().next().unwrap());
    } else if let Some((_, sub_group)) = groups.iter().rev().next() {
      if sub_group.len() == 1 {
        _completed = Some(&sub_group);
      }
    }

    if let Some(completed_items) = _completed {
      let cmpl_pair = (*(o_to_r(completed_items.first(), "")?), completed_items.clone().to_vec());
      handle_completed_item(gb, cmpl_pair, (sym, 0).into())
    } else {
      Err(conflicting_symbols_error(gb.graph(), groups))
    }
  } else {
    Ok(())
  }
}

fn create_out_of_scope_complete_state<'db, 'follow>(gb: &mut GraphBuilder<'db>, out_of_scope: Items<'db>, sym: PrecedentSymbol) {
  let transition_type = match (out_of_scope[0].origin, gb.is_scanner()) {
    (_, true) => StateType::ScannerCompleteOOS,
    _ => StateType::NonTermCompleteOOS,
  };
  gb.create_state(Normal, sym, transition_type, out_of_scope).to_leaf();
}

fn create_transition_groups<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<TransitionGroups<'db>> {
  let closure = gb.current_state().get_closure_ref()?;

  let mut groups = hash_group_btreemap(closure.iter().cloned().collect::<ItemSet>(), |_, item| match item.get_type() {
    ItemType::Completed(_) => SymbolId::Default,
    ItemType::Terminal(sym) => sym,
    ItemType::NonTerminal(_) => SymbolId::Undefined,
    ItemType::TokenNonTerminal(..) if gb.is_scanner() => SymbolId::Undefined,
    ItemType::TokenNonTerminal(_, sym) => sym,
  });

  groups.remove(&SymbolId::Undefined);

  let groups: OrderedMap<SymbolId, (u16, ItemSet<'db>)> = groups
    .into_iter()
    .map(|(s, g)| {
      (s, (if gb.is_scanner() { g.iter().get_max_token_precedence() } else { g.iter().get_max_token_precedence() }, g))
    })
    .collect();

  SherpaResult::Ok(groups)
}

fn get_completed_item_artifacts<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
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

fn handle_completed_item<'db, 'follow>(
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

fn resolve_peek<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  gb: &mut GraphBuilder<'db>,
  mut resolved: T,
  sym: PrecedentSymbol,
) -> SherpaResult<()> {
  let items = get_kernel_items_from_peek_item(gb, resolved.next().unwrap());

  gb.create_state(NormalGoto, sym, StateType::PeekEndComplete, items.to_vec()).enque();

  Ok(())
}
