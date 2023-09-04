use super::{
  errors::{conflicting_symbols_error, create_reduce_reduce_error, lr_disabled_error},
  items::{
    all_items_come_from_same_nonterminal_call,
    all_items_transition_on_same_nonterminal,
    get_follow,
    get_goal_items_from_completed,
    get_oos_follow_from_completed,
    merge_items_into_groups,
    merge_occluding_token_items,
    peek_items_are_from_goto_state,
  },
};
use crate::{
  compile::build_graph::errors::peek_not_allowed_error,
  journal::Journal,
  types::*,
  utils::{create_u64_hash, hash_group_btreemap},
};
use std::collections::{BTreeSet, VecDeque};

const _ALLOW_PEEKING: bool = true;

type PendingState = (GraphState, StateId);
type PendingStates = Vec<PendingState>;

use GraphState::*;

pub(crate) type TransitionGroup<'db> = (u16, ItemSet<'db>);
pub(crate) type TransitionGroups<'db> = OrderedMap<SymbolId, TransitionGroup<'db>>;
pub(crate) fn build<'follow, 'db: 'follow>(
  j: &mut Journal,
  name: IString,
  mode: GraphMode,
  kernel_items: Items<'db>,
  db: &'db ParserDatabase,
  config: ParserConfig,
) -> SherpaResult<GraphHost<'db>> {
  for item in &kernel_items {
    if item.len == 0 {
      todo!("Need to build warning for empty rules");
    }
  }

  let mut graph = GraphHost::new(db, mode, name, config);

  let root = graph.create_state((SymbolId::Default, 0).into(), StateType::Start, None, kernel_items);

  graph.enqueue_pending_state(Normal, root);

  let mut _have_errors = false;

  while let Some((graph_state, parent)) = graph.dequeue_pending_state() {
    match handle_kernel_items(&mut graph, parent, graph_state) {
      Err(err) => {
        _have_errors = true;
        j.report_mut().add_error(err)
      }
      _ => {}
    }
  }

  #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
  if !graph.is_scanner() {
    crate::test::utils::write_debug_file(db, "parse_graph.tmp", graph.debug_string(), true)?;
  } else {
    crate::test::utils::write_debug_file(db, "scanner_graph.tmp", graph.debug_string(), true)?;
  }

  j.report_mut().wrap_ok_or_return_errors(graph)
}

fn handle_kernel_items<'db>(graph: &mut GraphHost<'db>, parent: StateId, graph_state: GraphState) -> SherpaResult<()> {
  if false
    && graph.config().ALLOW_RECURSIVE_DESCENT_CALLS
    && !graph.is_scanner()
    && graph_state != NormalGoto
    && create_kernel_call(
      graph[parent].kernel_items_ref().clone().iter(),
      graph,
      graph_state,
      parent,
      (SymbolId::Default, 0).into(),
    )
    .is_some()
  {
    Ok(())
  } else {
    let mut groups = create_transition_groups(graph, parent)?;

    let max_precedence = handle_completed_items(graph, parent, graph_state, &mut groups)?;

    let groups = handle_scanner_items(max_precedence, graph, groups)?;

    let (out_items, pending_states) = handle_incomplete_items(graph, parent, graph_state, groups)?;

    handle_goto_states(graph, graph_state, parent, out_items, pending_states)?;

    Ok(())
  }
}

fn handle_goto_states<'db>(
  graph: &mut GraphHost<'db>,
  graph_state: GraphState,
  parent: StateId,
  out_items: BTreeSet<Item<'db>>,
  pending_states: Vec<(GraphState, StateId)>,
) -> SherpaResult<()> {
  match (!graph.is_scanner() && !graph_state.currently_peeking()).then(|| handle_nonterminal_shift(graph, parent, out_items)) {
    Some(Err(err)) => Err(err),
    Some(Ok(true)) => {
      //TODO(anthony): Move this into a function located in the errors file.
      if graph.config().ALLOW_LR_RECURSIVE_ASCENT == false {
        lr_disabled_error(graph, parent)?;
      }

      let db = graph.get_db();
      for (_, state) in pending_states {
        let incremented: Items =
          graph[state].kernel_items_ref().iter().map(|i| i.calculate_goto_distance(parent, graph)).collect();
        graph[state].set_kernel_items(incremented, db);
        graph.enqueue_pending_state(NormalGoto, state);
      }
      Ok(())
    }
    None | Some(Ok(false)) => {
      for (graph_state, state) in pending_states {
        graph.enqueue_pending_state(graph_state, state);
      }
      Ok(())
    }
  }
}

fn handle_scanner_items<'db>(
  max_precedence: u16,
  graph: &mut GraphHost<'db>,
  mut groups: TransitionGroups<'db>,
) -> SherpaResult<TransitionGroups<'db>> {
  if graph.is_scanner() {
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

fn handle_nonterminal_shift<'db, 'follow>(
  graph: &mut GraphHost<'db>,
  parent: StateId,
  out_items: ItemSet<'db>,
) -> SherpaResult<bool> {
  let db = graph.get_db();
  let nterm_items: ItemSet = graph[parent].get_closure_ref()?.clone().nonterm_items().inscope_items();
  let kernel_base: ItemSet = graph[parent].kernel_items_ref().iter().inscope_items();

  debug_assert!(!graph.is_scanner());

  let out_items: ItemSet<'db> = if false && parent.is_root() {
    out_items
  } else {
    out_items.into_iter().filter(|i| i.origin_state == parent && (!kernel_base.contains(i) || i.is_start())).collect()
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

  graph[parent].set_nonterm_items(&used_nterm_items);

  let used_nterm_groups = hash_group_btreemap(used_nterm_items, |_, t| t.nonterm_index_at_sym().unwrap_or_default());

  for (nterm, items) in &used_nterm_groups {
    let are_shifting_a_goal_nonterm = parent.is_root() && graph.goal_items().iter().rule_nonterm_ids().contains(&nterm);
    let contains_completed_kernel_items = items.iter().any(|i| kernel_base.contains(i) && i.is_penultimate());

    let mut incremented_items = items.iter().map(|i| i.calculate_goto_distance(parent, graph)).try_increment();
    let nterm_shift_type = StateType::NonTerminalShiftLoop;

    let mut create_peek_state = false;

    if kernel_nterm_ids.remove(&nterm) {
      let is_at_root = parent.is_root();
      let contains_left_recursive_items = items.iter().any(|i| i.is_left_recursive());

      if is_at_root && contains_left_recursive_items {
        let local_nterms = incremented_items.iter().nonterm_ids_at_index();
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
        let mut nonterminals = Queue::from_iter([*nterm]);
        let mut seen = Set::from_iter([*nterm]);
        seen.extend(local_nterms.iter());

        let mut oos_items = ItemSet::new();

        while let Some(_nterm) = nonterminals.pop_front() {
          for item in db.nonterm_follow_items(_nterm) {
            let item = item.try_increment().to_origin_state(parent);
            match item.get_type() {
              ItemType::Completed(_nterm) => {
                if seen.insert(_nterm) {
                  nonterminals.push_back(_nterm)
                }
              }
              ItemType::NonTerminal(non_terminal) => {
                if !local_nterms.contains(&non_terminal) {
                  oos_items.extend(
                    db.nonterm_rules(non_terminal)?.iter().map(|r| Item::from_rule(*r, db)).closure::<Items>(parent).term_items(),
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
          .map(|i| i.to_oos_index().to_origin(Origin::GoalCompleteOOS).to_origin_state(parent));

        // Compare closures and flag if any tokens in oos_items conflict with tokens in
        // incremented items
        create_peek_state = oos_items.clone().count() > 0 && {
          oos_items
            .clone()
            .terminals()
            .intersection(&incremented_items.iter().closure::<ItemSet>(parent).iter().terminals())
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
        get_completed_item_artifacts(graph, parent, incremented_items.iter().completed_items::<Vec<_>>().iter())?;
      let default = default_only.iter().map(|i| FollowPair::from((*i, *i))).collect::<Vec<_>>();
      let follow_pairs = follow_pairs.iter().chain(default.iter());

      create_peek(
        graph,
        (nterm.to_sym(), 0).into(),
        parent,
        &incremented_items.iter().incomplete_items::<Vec<_>>().iter(),
        Some(follow_pairs),
        false,
        nterm_shift_type,
      )?
    } else {
      let state = graph.create_state((nterm.to_sym(), 0).into(), nterm_shift_type, Some(parent), vec![]);

      graph[state].set_kernel_items(incremented_items, db);

      handle_kernel_items(graph, state, NormalGoto)?;

      Some(state)
    } {
      if are_shifting_a_goal_nonterm && !contains_completed_kernel_items {
        let state =
          graph.create_state((SymbolId::Default, 0).into(), StateType::NonTermCompleteOOS, Some(state), Default::default());
        graph.add_leaf_state(state);
      }
    }
  }

  // The remaining non-terminals are comprised of accept items for this state.
  for nonterm_id in kernel_nterm_ids {
    let state = graph.create_state((nonterm_id.to_sym(), 0).into(), StateType::NonTerminalComplete, Some(parent), vec![]);
    graph.add_leaf_state(state);
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

fn handle_incomplete_items<'nt_set, 'db: 'nt_set>(
  graph: &mut GraphHost<'db>,
  parent: StateId,
  graph_state: GraphState,
  groups: TransitionGroups<'db>,
) -> SherpaResult<(ItemSet<'db>, PendingStates)> {
  let mut out_items = ItemSet::new();
  let mut pending_states = PendingStates::new();

  for (sym, group) in groups {
    handle_incomplete_items_internal(graph, parent, sym, group, graph_state, &mut pending_states, &mut out_items)?;
  }

  SherpaResult::Ok((out_items, pending_states))
}

fn handle_incomplete_items_internal<'nt_set, 'db: 'nt_set>(
  graph: &mut GraphHost<'db>,
  parent: StateId,
  sym: SymbolId,
  (prec, group): TransitionGroup<'db>,
  graph_state: GraphState,
  pending_states: &mut PendingStates,
  out_items: &mut ItemSet<'db>,
) -> SherpaResult<()> {
  let is_scan = graph.is_scanner();
  let prec_sym = (sym, prec).into();

  match (group.len(), graph_state) {
    (1.., Peek(level)) => {
      if group.iter().all_items_are_from_same_peek_origin() {
        resolve_peek(graph, group.iter(), prec_sym, parent)?;
      } else {
        let state = graph.create_state(prec_sym, StateType::Peek, Some(parent), group.try_increment());
        graph.enqueue_pending_state(Peek(level + 1), state);
      }
    }
    (_, Normal | NormalGoto) => {
      let out_of_scope = group.clone().outscope_items().to_vec();
      let mut in_scope = group.clone().inscope_items();

      match (in_scope.len(), out_of_scope.len()) {
        (0, 1..) => {
          create_out_of_scope_complete_state(out_of_scope, graph, prec_sym, parent, is_scan);
        }
        (_, 1..) if is_scan => {
          create_out_of_scope_complete_state(out_of_scope, graph, prec_sym, parent, is_scan);
        }
        (1.., 1..) => {
          create_peek(
            graph,
            prec_sym,
            parent,
            &group.iter(),
            None::<std::collections::btree_set::Iter<'_, FollowPair<'_>>>,
            true,
            StateType::Peek,
          )?;
          out_items.append(&mut in_scope)
        }
        (1.., _) => {
          if let Some((shifted_items, pending_state)) = (graph.config().ALLOW_RECURSIVE_DESCENT_CALLS || graph.is_scanner())
            .then(|| create_call(in_scope.iter(), graph, graph_state, parent, prec_sym))
            .flatten()
          {
            out_items.append(&mut shifted_items.to_set());
            pending_states.push(pending_state);
          } else {
            let state = graph.create_state(
              (sym, in_scope.iter().get_max_token_precedence()).into(),
              StateType::Shift,
              Some(parent),
              in_scope.try_increment(),
            );

            out_items.append(&mut in_scope);
            pending_states.push((graph_state, state));
          }
        }
        _ => unreachable!(),
      }
    }
    _ => {}
  }

  SherpaResult::Ok(())
}

enum ConflictResolution {
  Shift,
  Reduce,
  Peek,
}

fn handle_completed_items<'nt_set, 'db: 'nt_set>(
  graph: &mut GraphHost<'db>,
  parent: StateId,
  graph_state: GraphState,
  groups: &mut TransitionGroups<'db>,
) -> SherpaResult<u16> {
  let is_scan = graph.is_scanner();
  let mut max_precedence = 0;

  if let Some(completed) = groups.remove(&SymbolId::Default) {
    max_precedence = max_precedence.max(completed.0);

    let CompletedItemArtifacts { follow_pairs, follow_items, default_only, .. } =
      get_completed_item_artifacts(graph, parent, completed.1.iter())?;

    let db = graph.get_db();

    if is_scan {
      graph[parent].add_kernel_items(follow_items, db);

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

      get_oos_follow_from_completed(graph, &completed.1.iter().to_vec(), &mut |follow: Items<'db>| {
        merge_items_into_groups(&follow, parent, groups)
      })?;
    }

    let contains_in_scope_items = completed.1.iter().any(|i| !i.is_out_of_scope());

    let default = if contains_in_scope_items {
      completed.1.iter().filter(|i| !i.is_out_of_scope()).map(|i| -> FollowPair<'db> { (*i, *i).into() }).collect()
    } else {
      completed.1.iter().map(|i| -> FollowPair<'db> { (*i, *i).into() }).collect()
    };

    handle_completed_groups(graph, groups, parent, graph_state, SymbolId::Default, default, &default_only)?;

    if !follow_pairs.is_empty() {
      // Create reduce states for follow items that have not already been covered.
      let mut completed_groups = hash_group_btreemap(follow_pairs.clone(), |_, fp| match fp.follow.get_type() {
        ItemType::Completed(_) => {
          unreachable!("Should be handled outside this path")
        }
        ItemType::Terminal(sym) => sym,
        _ => SymbolId::Undefined,
      });

      completed_groups.remove(&SymbolId::Undefined);

      for (sym, follow_pairs) in completed_groups {
        handle_completed_groups(graph, groups, parent, graph_state, sym, follow_pairs, &default_only)?;
      }
    }
  }

  SherpaResult::Ok(max_precedence)
}

fn handle_completed_groups<'nsp, 'db: 'nsp, 'follow>(
  graph: &mut GraphHost<'db>,
  groups: &mut TransitionGroups<'db>,
  parent: StateId,
  graph_state: GraphState,
  sym: SymbolId,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let is_scan = graph.is_scanner();
  let mut cmpl = follow_pairs.iter().to_completed_vec();
  let prec = follow_pairs.iter().to_follow_vec().iter().get_max_precedence(graph.get_mode());
  let prec_sym: PrecedentSymbol = (sym, prec).into();

  match (follow_pairs.len(), groups.remove(&sym), graph_state) {
    (1, None, Normal | NormalGoto) => {
      handle_completed_item(graph, (cmpl[0], cmpl), parent, prec_sym, graph_state)?;
    }
    (2.., None, Normal | NormalGoto) => {
      if is_scan {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(graph, parent, sym, cmpl.to_set(), graph_state)?;
      } else if cmpl.iter().to_absolute::<ItemSet>().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(graph, (cmpl[0], vec![cmpl[0]]), parent, prec_sym, graph_state)?;
      } else if cmpl.iter().all_are_out_of_scope() {
        // We are at the end of a lookahead that results in the completion of
        // some existing item.
        let item: Item<'_> = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(graph, (item, vec![item]), parent, prec_sym, graph_state)?;
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
            return Err(create_reduce_reduce_error(graph, cmpl.to_set()));
          }
          1 => {
            let out = unfollowed_items;
            let cmpl_item = out[0];
            handle_completed_item(graph, (cmpl_item, out), parent, prec_sym, graph_state)?;
          }
          0 => {
            //There are lookahead(k=1) style states available, so we don't need
            // to generate a default state.
          }
          _ => unreachable!(),
        };
      }
    }
    (_, Some((prec, group)), Normal | NormalGoto) => {
      // Create A Peek Path to handle this
      if is_scan {
        let mut group = group.to_vec();
        let item = cmpl[0];
        group.append(&mut cmpl);
        handle_completed_item(graph, (item, group), parent, prec_sym, graph_state)?;
      } else if group.iter().all_are_out_of_scope() && cmpl.iter().all_are_out_of_scope() {
        cmpl.append(&mut group.to_vec());
        create_out_of_scope_complete_state(cmpl, graph, prec_sym, parent, is_scan);
      } else {
        // WE can use precedence to resolve shift reduce conflicts.  For now favor
        // shift.

        let cardinal: ItemSet = group.iter().to_absolute();
        let unique: BTreeSet<FollowPair<'_>> =
          follow_pairs.into_iter().filter(|fp| !cardinal.contains(&&fp.follow.to_absolute())).collect::<OrderedSet<_>>();

        if !unique.is_empty() {
          match resolve_shift_reduce_conflict(graph, group.iter(), cmpl.iter()) {
            ConflictResolution::Shift => {
              groups.insert(sym, (prec, group));
            }
            ConflictResolution::Reduce => {
              handle_completed_groups(graph, &mut Default::default(), parent, graph_state, sym, unique, &cmpl.to_set())?;
            }
            ConflictResolution::Peek => {
              create_peek(graph, prec_sym, parent, &group.iter(), Some(unique.iter()), true, StateType::Peek)?;
            }
          }
        } else {
          groups.insert(sym, (prec, group));
        }
      }
    }
    (1, None, Peek(_)) => {
      resolve_peek(graph, cmpl.iter(), prec_sym, parent)?;
    }
    (_, None, Peek(_))
      if cmpl.iter().all_are_out_of_scope()
        || cmpl.iter().all_items_are_from_same_peek_origin()
        || peek_items_are_from_goto_state(&cmpl, graph) =>
    {
      if !cmpl.iter().all_are_out_of_scope() {
        cmpl = cmpl.into_iter().filter(|i| !i.is_out_of_scope()).collect();
      }

      resolve_peek(graph, cmpl.iter(), prec_sym, parent)?;
    }
    (_, None, Peek(_)) => {
      if cmpl.iter().follow_items_are_the_same() {
        // Items are likely a product of a reduce-shift conflict. We'll favor
        // the shift action as long as there is only one shift,
        // otherwise we have a shift-shift conflict. Grabbing the
        // original items.

        let kernel_states = follow_pairs
          .iter()
          .map(|fp| (get_kernel_state_from_peek_item(graph, &fp.completed), fp.completed))
          .collect::<OrderedSet<_>>();

        //let completed = kernel_items.clone().completed_items();
        let (incomplete, _): (Vec<_>, Vec<_>) = kernel_states.into_iter().partition(|(s, ..)| !s.first().unwrap().is_complete());

        if incomplete.len() == 1 {
          let (_, items) = incomplete[0];
          resolve_peek(graph, [items].iter(), prec_sym, parent)?;
        }
      } else {
        // If the number of resolve states is two and one of the states is oos then
        // resolve to the none oos state.

        #[cfg(debug_assertions)]
        {
          let kernel_states =
            follow_pairs.iter().map(|fp| get_kernel_state_from_peek_item(graph, &fp.completed)).collect::<OrderedSet<_>>();
          let db = graph.get_db();
          crate::test::utils::write_debug_file(db, "parse_graph.tmp", graph.debug_string(), true)?;
          unimplemented!(
            "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParent State\n{}\n\n",

            sym.debug_string(graph.get_db()),
            graph.goal_items().to_debug_string( "\n"),
            cmpl.to_debug_string("\n"),
            kernel_states.iter().map(|s| s.to_debug_string("\n")).collect::<Vec<_>>().join("\n"),
            graph[parent].debug_string(graph.get_db()),
            //graph.debug_string()
          );
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }
    (_, Some((_, group)), Peek(_)) => {
      let mut combined = group.clone();
      combined.append(&mut follow_pairs.iter().to_follow_set());

      if combined.iter().all_items_are_from_same_peek_origin() {
        resolve_peek(graph, combined.iter(), prec_sym, parent)?;
      } else {
        #[cfg(debug_assertions)]
        todo!(
          "Roll the follow states into the group and resubmit to incomplete handler function.\nincomplete:\n{}\ncomplete:\n{}\nfollow:\n{}\n \n {}",
          group.to_debug_string("\n"),
          follow_pairs.iter().to_completed_vec().to_debug_string( "\n"),
          follow_pairs.iter().to_follow_vec().to_debug_string("\n"),
          graph.debug_string()
        );

        #[cfg(not(debug_assertions))]
        todo!()
      }
    }
    (_len, _collide, _graph_state) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: {_graph_state:?} len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
        sym.debug_string(graph.get_db()),
        cmpl.to_debug_string("\n"),
        graph.debug_string()
      );
      #[cfg(not(debug_assertions))]
      unimplemented!()
    }
  }

  SherpaResult::Ok(())
}

fn resolve_shift_reduce_conflict<
  'a,
  'db: 'a,
  A: Clone + ItemRefContainerIter<'a, 'db>,
  B: Clone + ItemRefContainerIter<'a, 'db>,
>(
  graph: &'db GraphHost<'db>,
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
    let compl_prec = comp1_items.clone().get_max_precedence(graph.get_mode());
    let incom_prec = incom_items.clone().get_max_precedence(graph.get_mode());

    incom_prec >= compl_prec
  } {
    ConflictResolution::Shift
  } else if nterm_sets_are_equal {
    ConflictResolution::Reduce
  } else {
    ConflictResolution::Peek
  }
}

fn create_peek<
  'a,
  'db: 'a,
  'follow,
  T: ItemRefContainerIter<'a, 'db> + Clone,
  Pairs: Iterator<Item = &'a FollowPair<'db>> + Sized + Clone,
>(
  graph: &mut GraphHost<'db>,
  sym: PrecedentSymbol,
  parent: StateId,
  incomplete_items: &T,
  completed_pairs: Option<Pairs>,
  need_increment: bool,
  transition_type: StateType,
) -> SherpaResult<Option<StateId>> {
  let mut kernel_items = Array::default();

  let existing_nterms = incomplete_items.clone().rule_nonterm_ids();
  let existing_items: ItemSet = incomplete_items.clone().to_absolute();
  let state = graph.create_state(sym, transition_type, Some(parent), Array::default());

  if let Some(completed_pairs) = completed_pairs {
    let pairs = completed_pairs.cloned().collect::<BTreeSet<_>>();

    // All items here complete the same nonterminal, so we group them all into one
    // goal index.
    for (_, items) in hash_group_btreemap(pairs, |_, fp| fp.completed.nonterm_index()) {
      let follow: ItemSet = items
        .iter()
        .filter_map(|FollowPair { follow, .. }| {
          if existing_items.contains(&follow) || (follow.is_at_initial() && existing_nterms.contains(&follow.nonterm_index())) {
            None
          } else {
            Some(*follow)
          }
        })
        .collect();

      if !follow.is_empty() {
        let index = create_u64_hash(&follow);
        for follow in follow {
          kernel_items.push(follow.to_origin(Origin::Peek(index, state)));
        }

        graph[state].set_peek_resolve_state(index, items.iter().to_completed_set());
      }
    }
  }

  for (_, nonterms) in hash_group_btreemap(incomplete_items.clone().to_vec(), |_, i| i.is_out_of_scope()) {
    let index = create_u64_hash(&nonterms);
    for nonterm in &nonterms {
      kernel_items.push(nonterm.to_origin(Origin::Peek(index, state)));
    }

    graph[state].set_peek_resolve_state(index, nonterms.to_set())
  }

  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );
  debug_assert!(
    !incomplete_items.clone().any(|i| matches!(i.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );
  let db: &ParserDatabase = graph.get_db();

  graph[state].add_kernel_items(if need_increment { kernel_items.try_increment() } else { kernel_items }, db);

  if graph.config().ALLOW_PEEKING == false {
    Err(peek_not_allowed_error(graph, state))
  } else {
    Ok(graph.enqueue_pending_state(Peek(0), state))
  }
}

fn resolve_peek<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  graph: &mut GraphHost<'db>,
  mut resolved: T,
  sym: PrecedentSymbol,
  par: StateId,
) -> SherpaResult<()> {
  let items = get_kernel_state_from_peek_item(graph, resolved.next().unwrap());

  let resolve_state = graph.create_state(sym, StateType::PeekEndComplete, Some(par), items.to_vec());

  handle_kernel_items(graph, resolve_state, NormalGoto)
}

pub(super) fn get_kernel_state_from_peek_item<'db, 'follow>(graph: &GraphHost<'db>, peek_item: &Item<'db>) -> ItemSet<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
    unreachable!("Invalid peek origin");
  };

  graph[peek_origin].get_resolve_state(peek_index)
}

fn create_kernel_call<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db> + Clone>(
  group: T,
  graph: &mut GraphHost<'db>,
  graph_state: GraphState,
  parent: StateId,
  sym: PrecedentSymbol,
) -> Option<()> {
  let Some(first) = group.clone().next() else { return None };
  let db = graph.get_db();

  if let Some((_, (graph_state, state))) = create_call_internal(
    if all_items_transition_on_same_nonterminal(group.clone()) {
      let nterm = first.nonterm_index_at_sym()?;
      if !matches!(db.nonterm_recursion_type(nterm), RecursionType::LeftRecursive | RecursionType::LeftRightRecursive) {
        Some((group.to_vec(), nterm))
      } else {
        None
      }
    } else {
      None
    },
    graph,
    parent,
    sym,
    graph_state,
  ) {
    graph.enqueue_pending_state(graph_state, state);
    Some(())
  } else {
    None
  }
}

fn create_call<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db> + Clone>(
  group: T,
  graph: &mut GraphHost<'db>,
  graph_state: GraphState,
  parent: StateId,
  sym: PrecedentSymbol,
) -> Option<(Vec<Item<'db>>, PendingState)> {
  let Some(first) = group.clone().next() else { return None };
  let db = graph.get_db();

  create_call_internal(
    if all_items_come_from_same_nonterminal_call(group.clone()) {
      let nterm = first.nonterm_index();

      if !matches!(db.nonterm_recursion_type(nterm), RecursionType::LeftRecursive | RecursionType::LeftRightRecursive) {
        let Ok(items) = graph[parent].get_closure_ref().map(|i| {
          i.iter()
            .filter(|i| match i.nontermlike_index_at_sym() {
              Some(id) => id == nterm && i.nonterm_index() != nterm,
              _ => false,
            })
            .cloned()
            .collect::<Vec<_>>()
        }) else {
          return None;
        };

        // There may be a superior candidate. evaluate that.
        if let Some(pending_state) = create_call(items.iter(), graph, graph_state, parent, sym) {
          return Some(pending_state);
        }

        Some((items, nterm))
      } else {
        None
      }
    } else {
      None
    },
    graph,
    parent,
    sym,
    graph_state,
  )
}

fn create_call_internal<'db>(
  call_pack: Option<(Vec<Item<'db>>, DBNonTermKey)>,
  graph: &mut GraphHost<'db>,
  parent: StateId,
  sym: PrecedentSymbol,
  graph_state: GraphState,
) -> Option<(Vec<Item<'db>>, PendingState)> {
  match call_pack {
    Some((items, nterm_to_call)) if items.len() > 0 => {
      use StateType::*;

      let state_type = if graph.is_scanner() {
        let kernel_items = graph[parent].kernel_items_ref();
        items.iter().all(|i| kernel_items.contains(i)).then_some(KernelCall(nterm_to_call)).unwrap_or(InternalCall(nterm_to_call))
      } else {
        InternalCall(nterm_to_call)
      };

      let state = graph.create_state(sym, state_type, Some(parent), items.iter().try_increment());
      Some((items, (graph_state, state)))
    }
    _ => None,
  }
}

fn resolve_conflicting_tokens<'db, 'follow>(
  graph: &mut GraphHost<'db>,
  par: StateId,
  sym: SymbolId,
  completed_items: ItemSet<'db>,
  g_state: GraphState,
) -> SherpaResult<()> {
  // Map items according to their symbols
  let token_precedence_groups =
    hash_group_btreemap(completed_items.clone(), |_, i| (i.origin_precedence(), i.origin.get_symbol(graph.get_db())));

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
      handle_completed_item(graph, cmpl_pair, par, (sym, 0).into(), g_state)
    } else {
      Err(conflicting_symbols_error(graph, groups))
    }
  } else {
    Ok(())
  }
}

fn create_out_of_scope_complete_state<'db, 'follow>(
  out_of_scope: Items<'db>,
  graph: &mut GraphHost<'db>,
  sym: PrecedentSymbol,
  parent: StateId,
  is_scan: bool,
) {
  let transition_type = match (out_of_scope[0].origin, is_scan) {
    (_, true) => StateType::ScannerCompleteOOS,
    _ => StateType::NonTermCompleteOOS,
  };
  let state = graph.create_state(sym, transition_type, Some(parent), out_of_scope);
  graph.add_leaf_state(state);
}

fn create_transition_groups<'db, 'follow>(graph: &mut GraphHost<'db>, parent: StateId) -> SherpaResult<TransitionGroups<'db>> {
  let closure = graph[parent].get_closure_ref()?;

  let mut groups = hash_group_btreemap(closure.iter().cloned().collect::<ItemSet>(), |_, item| match item.get_type() {
    ItemType::Completed(_) => SymbolId::Default,
    ItemType::Terminal(sym) => sym,
    ItemType::NonTerminal(_) => SymbolId::Undefined,
    ItemType::TokenNonTerminal(..) if graph.is_scanner() => SymbolId::Undefined,
    ItemType::TokenNonTerminal(_, sym) => sym,
  });

  groups.remove(&SymbolId::Undefined);

  let groups: OrderedMap<SymbolId, (u16, ItemSet<'db>)> = groups
    .into_iter()
    .map(|(s, g)| {
      (s, (if graph.is_scanner() { g.iter().get_max_token_precedence() } else { g.iter().get_max_token_precedence() }, g))
    })
    .collect();

  SherpaResult::Ok(groups)
}

fn get_completed_item_artifacts<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  graph: &mut GraphHost<'db>,
  par: StateId,
  completed: T,
) -> SherpaResult<CompletedItemArtifacts<'db>> {
  let mut follow_pairs = OrderedSet::new();
  let mut follow_items = ItemSet::new();
  let mut default_only_items = ItemSet::new();

  for c_i in completed {
    let (f, ..) = get_follow(graph, *c_i)?;

    if f.is_empty() {
      default_only_items.insert(*c_i);
    } else {
      follow_pairs
        .append(&mut f.iter().closure::<ItemSet>(par).into_iter().map(|i| (*c_i, i.to_origin(c_i.origin)).into()).collect());
      follow_items.append(&mut f.to_set());
    }
  }

  SherpaResult::Ok(CompletedItemArtifacts { follow_items, follow_pairs, default_only: default_only_items })
}

fn handle_completed_item<'db, 'follow>(
  graph: &mut GraphHost<'db>,
  (completed_item, completed_items): (Item<'db>, Items<'db>),
  parent: StateId,
  sym: PrecedentSymbol,
  graph_state: GraphState,
) -> SherpaResult<()> {
  let is_scan = graph.is_scanner();

  // Determine if origin contains GOTOs.
  match (completed_item.origin, is_scan) {
    (Origin::GoalCompleteOOS, _) => {
      let state = graph.create_state(sym, StateType::NonTermCompleteOOS, Some(parent), vec![]);
      graph.add_leaf_state(state);
    }
    // Completion of parse tree may be premature
    // or item is not an acceptable completed item
    (_, true) => {
      let (follow, completed_items): (Vec<Items>, Vec<Items>) = completed_items
        .into_iter()
        .map(|i| {
          let Ok(result) = get_follow(graph, i) else { unreachable!("could not get follow data") };
          result
        })
        .unzip();
      let follow = follow.into_iter().flatten().collect::<Items>();
      let completed_items = completed_items.into_iter().flatten().collect::<Items>();

      let goals = get_goal_items_from_completed(&completed_items, graph);

      let is_continue = !follow.is_empty();
      let is_goal = !goals.is_empty();

      let state = graph.create_state(
        sym,
        match (is_continue, goals.first().map(|d| d.origin)) {
          (true, Some(Origin::TerminalGoal(tok_id, ..))) => StateType::AssignAndFollow(tok_id),
          (false, Some(Origin::TerminalGoal(tok_id, ..))) => StateType::AssignToken(tok_id),
          (true, _) => StateType::Follow,
          (false, _) => StateType::Complete,
        },
        Some(parent),
        follow.clone(),
      );
      graph[state].set_reduce_item(completed_item);

      if is_continue {
        if let Some(state) = graph.enqueue_pending_state(graph_state, state) {
          if is_goal {
            graph.add_leaf_state(state)
          }
        }
      } else {
        graph.add_leaf_state(state)
      }
    }
    // Normal reduction state with no other actions.
    _ => {
      let state = graph.create_state(
        sym,
        StateType::Reduce(completed_item.rule_id, completed_item.goto_distance as usize),
        Some(parent),
        vec![],
      );

      graph[state].set_reduce_item(completed_item);

      graph.add_leaf_state(state);
    }
  }

  SherpaResult::Ok(())
}
