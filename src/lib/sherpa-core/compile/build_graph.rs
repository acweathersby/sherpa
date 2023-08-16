use crate::{
  journal::Journal,
  types::*,
  utils::{create_u64_hash, hash_group_btreemap},
};
use core::panic;
use sherpa_rust_runtime::utf8::{get_token_class_from_codepoint, lookup_table::CodePointClass};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub(super) fn build_graph<'follow, 'db: 'follow>(
  j: &mut Journal,
  mode: GraphMode,
  kernel_items: Items<'db>,
  db: &'db ParserDatabase,
  follow: &'follow FollowSets<'db>,
) -> SherpaResult<Graph<'follow, 'db>> {
  for item in &kernel_items {
    if item.len == 0 {
      todo!("Need to build warning for empty rules");
    }
  }

  let mut graph = Graph::new(db, mode, follow);

  let root = graph.create_state(SymbolId::Default, StateType::Start, None, kernel_items);

  graph.enqueue_pending_state(GraphState::Normal, root);

  while let Some((graph_state, parent)) = graph.dequeue_pending_state() {
    handle_kernel_items(j, &mut graph, parent, graph_state)?;
  }

  j.report_mut().ok_or_convert_to_error(graph)
}

fn handle_kernel_items(
  j: &mut Journal,
  graph: &mut Graph,
  parent: StateId,
  graph_state: GraphState,
) -> SherpaResult<()> {
  // TODO: PEG stuff

  let mut groups = create_transition_groups(graph, parent)?;

  let (max_precedence) = handle_completed_items(j, graph, parent, graph_state, &mut groups)?;

  if max_precedence > 0 {
    groups = groups
      .into_iter()
      .filter_map(|(s, g)| {
        let g = g.into_iter().filter(|i| i.precedence() >= max_precedence).collect::<BTreeSet<_>>();
        if g.is_empty() {
          None
        } else {
          Some((s, g))
        }
      })
      .collect();
  }

  merge_occluding(j, graph.is_scan(), groups.clone(), &mut groups);

  let out_items = handle_incomplete_items(j, graph, parent, graph_state, groups)?;

  if graph_state != GraphState::Peek && !graph.is_scan() {
    handle_goto(j, graph, parent, out_items);
  }

  SherpaResult::Ok(())
}

fn handle_incomplete_items<'db, 'follow>(
  _j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  parent: StateId,
  graph_state: GraphState,
  groups: OrderedMap<SymbolId, ItemSet<'db>>,
) -> SherpaResult<ItemSet<'db>> {
  let mut out_items = OrderedSet::new();
  let is_scan = graph.is_scan();
  for (sym, group) in groups {
    match (group.len(), graph_state) {
      (1, GraphState::Peek) => {
        let kernel_items = get_kernel_items_from_peek_item(graph, group.first()?);

        match kernel_items[0].origin {
          _ => {
            let state = graph.create_state(sym, StateType::PeekEnd, Some(parent), kernel_items);
            graph.enqueue_pending_state(GraphState::Normal, state);
          }
        }
      }
      (2.., GraphState::Peek) => {
        if group.iter().all_items_are_from_same_peek_origin() {
          let state = graph.create_state(
            sym,
            StateType::PeekEnd,
            Some(parent),
            get_kernel_items_from_peek_item(graph, group.first()?),
          );
          graph.enqueue_pending_state(GraphState::Normal, state);
        } else {
          let state = graph.create_state(sym, StateType::Peek, Some(parent), group.try_increment());

          graph.enqueue_pending_state(graph_state, state);
        }
      }
      (_, GraphState::Normal) => {
        let out_of_scope = group.clone().outscope_items().to_vec();
        let mut in_scope = group.inscope_items();

        match (in_scope.len(), out_of_scope.len()) {
          (0, 1..) => {
            create_out_of_scope_complete_state(out_of_scope, graph, &sym, parent, is_scan);
          }
          (_, 1..) if is_scan => {
            create_out_of_scope_complete_state(out_of_scope, graph, &sym, parent, is_scan);
          }
          (1.., _) => {
            if let Some(shifted_items) =
              create_call(&in_scope.clone().to_vec(), graph, graph_state, parent, sym, is_scan)
            {
              out_items.append(&mut shifted_items.to_set());
            } else {
              let state =
                graph.create_state(sym, StateType::Shift, Some(parent), in_scope.try_increment());

              out_items.append(&mut in_scope);

              graph.enqueue_pending_state(graph_state, state);
            }
          }
          _ => unreachable!(),
        }
      }
      _ => {}
    }
  }

  SherpaResult::Ok(out_items)
}

fn merge_items_into_groups<'db>(
  follow: &Vec<Item<'db>>,
  par: StateId,
  is_scan: bool,
  groups: &mut OrderedMap<SymbolId, ItemSet<'db>>,
) {
  // Dumb symbols that could cause termination of parse into the intermediate
  // item groups
  for (sym, group) in hash_group_btreemap(
    follow
      .create_closure(is_scan, par)
      .into_iter()
      .filter(|i| i.is_term())
      .collect::<OrderedSet<_>>(),
    |_, i| i.sym(),
  ) {
    if !groups.contains_key(&sym) {
      groups.insert(sym, group);
    }
  }
}

// Inserts out of scope sentinel items into the existing
// items groups if we are in scanner mode and the item that
// was completed belongs to the parse state goal set.
fn get_oos_follow_from_completed<'db, 'follow>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  completed_items: &Items<'db>,
  handler: &mut dyn FnMut(Items<'db>),
) -> SherpaResult<()> {
  let mut out = ItemSet::new();
  for completed_item in completed_items {
    if !completed_item.is_out_of_scope() {
      let (_, completed) = get_follow(j, graph, *completed_item)?;

      let goals: ItemSet = get_goal_items_from_completed(&completed, graph);

      for goal in goals {
        let (follow, _) = get_follow(
          j,
          graph,
          goal.to_complete().to_origin(Origin::ScanCompleteOOS).to_oos_index(),
        )?;
        out.append(&mut follow.to_set());
      }
    }
  }
  if !out.is_empty() {
    handler(out.to_vec());
  }
  SherpaResult::Ok(())
}

fn handle_completed_items<'db, 'follow>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  parent: StateId,
  graph_state: GraphState,
  groups: &mut OrderedMap<SymbolId, ItemSet<'db>>,
) -> SherpaResult<u16> {
  let is_scan = graph.is_scan();

  let mut max_precedence = 0;

  if let Some(completed) = groups.remove(&SymbolId::Default) {
    max_precedence = max_precedence.max(get_max_precedence(&completed) as u16);

    let CompletedItemArtifacts { follow_pairs, follow_items, default_only, .. } =
      get_completed_item_artifacts(j, graph, parent, completed.iter())?;

    let db = graph.get_db();

    if is_scan {
      graph[parent].add_kernel_items(follow_items, is_scan, db);

      merge_occluding(
        j,
        is_scan,
        hash_group_btreemap(
          follow_pairs.iter().map(|fp| fp.follow).collect::<ItemSet>(),
          |_, item| match item.get_type() {
            ItemType::Terminal(sym) => sym,
            _ => SymbolId::Undefined,
          },
        ),
        groups,
      );

      get_oos_follow_from_completed(j, graph, &completed.iter().to_vec(), &mut |follow: Items<
        'db,
      >| {
        merge_items_into_groups(&follow, parent, is_scan, groups)
      })?;
    }

    let default = completed.iter().map(|i| -> FollowPair<'db> { (*i, *i).into() }).collect();

    handle_completed_groups(
      j,
      graph,
      groups,
      parent,
      graph_state,
      SymbolId::Default,
      default,
      &default_only,
    )?;

    if !follow_pairs.is_empty() {
      // Create reduce states for follow items that have not already been covered.
      let mut completed_groups =
        hash_group_btreemap(follow_pairs.clone(), |_, fp| match fp.follow.get_type() {
          ItemType::Completed(_) => {
            unreachable!("Should be handled outside this path")
          }
          ItemType::Terminal(sym) => sym,
          _ => SymbolId::Undefined,
        });

      completed_groups.remove(&SymbolId::Undefined);

      for (sym, follow_pairs) in completed_groups {
        handle_completed_groups(
          j,
          graph,
          groups,
          parent,
          graph_state,
          sym,
          follow_pairs,
          &default_only,
        )?;
      }
    }
  }

  SherpaResult::Ok(max_precedence)
}

fn handle_goto<'db, 'follow>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  parent: StateId,
  out_items: ItemSet<'db>,
) -> SherpaResult<()> {
  let non_terminals = graph[parent].get_closure_ref()?.clone().non_term_items();

  let kernel_base = graph[parent].kernel_items_ref().clone();
  let db = graph.get_db();

  let out_items: ItemSet<'db> = if parent.is_root() {
    out_items
  } else {
    out_items.into_iter().filter(|i| !kernel_base.contains(i) || i.is_start()).collect()
  };

  if !non_terminals.is_empty() && !out_items.is_empty() {
    let mut kernel_prod_ids = kernel_base.iter().to_production_id_set();
    let mut used_non_terms = OrderedSet::new();
    let mut seen = OrderedSet::new();
    let mut queue = VecDeque::from_iter(out_items.iter().map(|i| i.prod_index()));

    while let Some(prod_id) = queue.pop_front() {
      if seen.insert(prod_id) {
        for item in non_terminals.iter().filter(|i| i.prod_index_at_sym().unwrap() == prod_id) {
          used_non_terms.insert(*item);
          if !kernel_base.contains(item) || item.is_start() {
            queue.push_back(item.prod_index());
          }
        }
      }
    }

    if !used_non_terms.is_empty() {
      graph[parent].set_non_terminals(&used_non_terms);

      let used_goto_groups =
        hash_group_btreemap(used_non_terms, |_, t| t.prod_index_at_sym().unwrap_or_default());

      for (prod_id, items) in &used_goto_groups {
        let sym_id = prod_id.to_sym();

        let transition_type = items
          .iter()
          .any(|i| -> bool {
            let p = i.prod_index();
            let recursive = p == i.prod_index_at_sym().unwrap_or_default();
            recursive
              .then_some(i.at_start())
              .unwrap_or(!kernel_base.contains(i) && used_goto_groups.contains_key(&p))
          })
          .then_some(StateType::GotoLoop)
          .unwrap_or(StateType::KernelGoto);

        let incremented_items = items.try_increment();
        let incomplete = incremented_items.clone().incomplete_items();
        let mut completed = incremented_items.clone().completed_items();

        if kernel_prod_ids.remove(&prod_id) && parent.is_root() && completed.is_empty() {
          let item = Item::from_rule(db.prod_rules(*prod_id)?[0], db);

          completed.push(
            item
              .to_complete()
              .to_origin(Origin::GoalCompleteOOS)
              .to_oos_index()
              .to_origin_state(parent),
          );
        }

        if completed.len() > 0 && incomplete.len() > 0 {
          let CompletedItemArtifacts {
            follow_pairs: mut fp,
            mut oos_pairs,
            default_only: default,
            ..
          } = get_completed_item_artifacts(j, graph, parent, completed.iter())?;

          fp.append(&mut oos_pairs);

          if let Some(s) =
            create_peek(graph, sym_id, parent, incomplete.iter(), fp, false, transition_type)
          {
            let c_p: std::collections::BTreeSet<FollowPair<'_>> =
              completed.clone().into_iter().map(|i| (i, i).into()).collect();
            let graph_state = GraphState::Normal;
            let sym = SymbolId::Default;
            let groups = &mut Default::default();
            let s = graph.create_state(sym, StateType::PeekEnd, Some(s), completed);

            handle_completed_groups(j, graph, groups, s, graph_state, sym, c_p, &default)?;
          }
        } else {
          let state =
            graph.create_state(sym_id, transition_type, Some(parent), incremented_items.clone());
          graph.enqueue_pending_state(GraphState::Normal, state);
        }
      }

      // The remaining goto productions are accept states for for this goto.
      for prod_id in kernel_prod_ids {
        let state = graph.create_state(prod_id.to_sym(), StateType::GotoPass, Some(parent), vec![]);
        graph.add_leaf_state(state);
      }
    }
  }

  SherpaResult::Ok(())
}

fn create_peek<'a, 'db: 'a, 'follow, T: ItemContainerIter<'a, 'db>>(
  graph: &mut Graph<'follow, 'db>,
  sym: SymbolId,
  parent: StateId,
  mut incomplete_items: T,
  completed_item_pairs: OrderedSet<FollowPair<'db>>,
  need_increment: bool,
  transition_type: StateType,
) -> Option<StateId> {
  let is_scan = graph.is_scan();
  let mut kernel_items = Array::default();
  let mut resolve_items = Array::default();
  let mut incomplete_items = incomplete_items.to_vec();

  let existing_prod_ids = incomplete_items.iter().to_production_id_set();
  let existing_items = incomplete_items.clone().to_absolute().to_set();
  let state = graph.create_state(sym, transition_type, Some(parent), Array::default());

  for (_, items) in hash_group_btreemap(completed_item_pairs, |_, fp| fp.completed.rule_id) {
    // All items here complete the same rule, so we group them all into one goal
    // index.
    let follow: ItemSet = items
      .iter()
      .filter_map(|FollowPair { follow, .. }| {
        if follow.is_complete() || {
          existing_items.contains(&follow)
            || (follow.at_start() && existing_prod_ids.contains(&follow.prod_index()))
        } {
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
      graph[state].set_peek_resolve_items(index, items.iter().to_completed_set().to_vec());
    }
  }

  let index = create_u64_hash(&incomplete_items);

  kernel_items.append(
    &mut incomplete_items.iter().map(|i| i.to_origin(Origin::Peek(index, state))).collect(),
  );
  resolve_items.append(&mut incomplete_items);

  debug_assert!(
    !resolve_items.iter().any(|i| matches!(i.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );
  debug_assert!(
    !incomplete_items.iter().any(|i| matches!(i.origin, Origin::Peek(..))),
    "Peek states should not be in the resolution"
  );
  let db: &ParserDatabase = graph.get_db();

  graph[state].set_peek_resolve_items(index, resolve_items);

  graph[state].add_kernel_items(
    if need_increment { kernel_items.try_increment() } else { kernel_items },
    is_scan,
    db,
  );

  graph.enqueue_pending_state(GraphState::Peek, state)
}

fn get_kernel_items_from_peek<'db, 'follow>(
  graph: &Graph<'follow, 'db>,
  peek_item: &Item<'db>,
) -> Items<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
        panic!("Invalid peek origin");
    };

  graph[peek_origin].get_resolve_items(peek_index)
}

fn all_items_come_from_same_production_call(group: &Items) -> bool {
  group.iter().all(|i| i.at_start())
    && group.iter().map(|i| i.prod_index()).collect::<Set<_>>().len() == 1
}
fn create_call<'db, 'follow>(
  group: &Items<'db>,
  graph: &mut Graph<'follow, 'db>,
  graph_state: GraphState,
  parent: StateId,
  sym: SymbolId,
  is_scan: bool,
) -> Option<Items<'db>> {
  if all_items_come_from_same_production_call(group) {
    let prod_id = group[0].prod_index();
    let db = group[0].get_db();

    if !graph[parent].conflicting_production_call(prod_id, is_scan, db) {
      let items = graph[parent]
        .get_closure_ref()?
        .iter()
        .filter(|i| match i.prod_index_at_sym() {
          Some(id) => id == prod_id && i.prod_index() != prod_id,
          _ => false,
        })
        .cloned()
        .collect::<Vec<_>>();

      if items.len() > 0 {
        if let Some(items) = create_call(&items, graph, graph_state, parent, sym, is_scan) {
          return Some(items);
        } else {
          let kernel_items = graph[parent].kernel_items_ref();
          let kernel_item_call = items.iter().all(|i| kernel_items.contains(i));

          let state = graph.create_state(
            sym,
            if kernel_item_call {
              StateType::KernelCall(prod_id)
            } else {
              StateType::InternalCall(prod_id)
            },
            Some(parent),
            items.try_increment(),
          );

          graph.enqueue_pending_state(graph_state, state);

          return if kernel_item_call { Some(vec![]) } else { Some(items) };
        }
      }
    }
  }
  None
}

fn handle_completed_groups<'db, 'follow>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  groups: &mut OrderedMap<SymbolId, ItemSet<'db>>,
  par: StateId,
  g_state: GraphState,
  sym: SymbolId,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let is_scan = graph.is_scan();
  let mut cmpl = follow_pairs.iter().to_completed_vec();
  let db = graph.get_db();

  match (follow_pairs.len(), groups.remove(&sym), g_state) {
    (1, None, GraphState::Normal) => {
      handle_completed_item(j, graph, (cmpl[0], cmpl), par, sym, g_state)?;
    }
    (2.., None, GraphState::Normal) => {
      if is_scan {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_symbols(j, graph, par, sym, cmpl.to_set(), g_state)?;
      } else if cmpl.clone().to_set().to_absolute().len() == 1 {
        // The same production is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(j, graph, (cmpl[0], vec![cmpl[0]]), par, sym, g_state)?;
      } else if cmpl.iter().all_are_out_of_scope() {
        // We are at the end of a lookahead that results in the completion of
        // some existing item.
        let item = *cmpl.first()?;
        handle_completed_item(j, graph, (item, vec![item]), par, sym, g_state);
      } else {
        let unfollowed_items: Items =
          default_only_items.intersection(&cmpl.iter().to_set()).cloned().collect();

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
            create_reduce_reduce_error(j, graph, cmpl.to_set())?;
          }
          1 => {
            let out = unfollowed_items;
            handle_completed_item(j, graph, (out[0], out), par, sym, g_state)?;
          }
          0 => {
            //There are lookahead(k=1) style states available, so we don't need
            // to generate a default state.
          }
          _ => unreachable!(),
        };
      }
    }
    (_, Some(mut group), GraphState::Normal) => {
      // Create A Peek Path to handle this
      if is_scan {
        let mut group = group.to_vec();
        let item = cmpl[0];
        group.append(&mut cmpl);
        handle_completed_item(j, graph, (item, group), par, sym, g_state)?;
      /*     todo!(
        "Handle shift-reduce conflict in Scanner state. ------\n Reduce:\n{}\nShift:\n{} \n------",
        completed.to_debug_string(g, "\n"), group.to_debug_string(g, "\n")
      ); */
      } else if group.iter().all_are_out_of_scope() && cmpl.iter().all_are_out_of_scope() {
        cmpl.append(&mut group.to_vec());
        create_out_of_scope_complete_state(cmpl, graph, &sym, par, is_scan);
      } else {
        let cardinal = group.clone().to_absolute();
        let unique = follow_pairs
          .into_iter()
          .filter(|fp| !cardinal.contains(&&fp.follow.to_absolute()))
          .collect::<OrderedSet<_>>();

        if !unique.is_empty() {
          group.append(&mut get_set_of_occluding_items(
            j,
            &sym,
            &group,
            &groups,
            is_scan,
            graph.get_db(),
          ));
          create_peek(graph, sym, par, group.iter(), unique, true, StateType::Peek);
        }
      }
    }
    (1, None, GraphState::Peek) => {
      resolve_peek(graph, cmpl.iter(), sym, par);
    }
    (_, None, GraphState::Peek)
      if cmpl.iter().all_are_out_of_scope()
        || cmpl.iter().all_items_are_from_same_peek_origin()
        || peek_items_are_from_goto_state(&cmpl, graph) =>
    {
      if !cmpl.iter().all_are_out_of_scope()
      /* && sym.is_default() */
      {
        cmpl = cmpl.into_iter().filter(|i| !i.is_out_of_scope()).collect();
      }

      resolve_peek(graph, cmpl.iter(), sym, par);
    }
    (_, None, GraphState::Peek) => {
      if cmpl.iter().follow_items_are_the_same() {
        // Items are likely a product of a reduce-shift conflict. We'll favor
        // the shift action as long as there is only one shift,
        // otherwise we have a shift-shift conflict. Grabbing the
        // original items.
        let kernel_items = follow_pairs
          .iter()
          .flat_map(|fp| get_kernel_items_from_peek(graph, &fp.completed))
          .collect::<ItemSet>();

        //let completed = kernel_items.clone().completed_items();
        let incomplete = kernel_items.incomplete_items();

        if incomplete.len() == 1 {
          let state = graph.create_state(sym, StateType::PeekEnd, Some(par), incomplete.to_vec());
          graph.enqueue_pending_state(GraphState::Normal, state);
        }
      } else {
        let kernel_items = follow_pairs
          .iter()
          .flat_map(|fp| get_kernel_items_from_peek(graph, &fp.completed))
          .collect::<ItemSet>();
        #[cfg(debug_assertions)]
        unimplemented!(
          "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParant State\n{}\n\nGraph:\n{}",

          sym.debug_string(db),
          graph.goal_items().to_debug_string( "\n"),
          cmpl.to_debug_string("\n"),
          kernel_items.to_debug_string("\n"),
          graph[par].debug_string(graph.get_db()),
          graph.debug_string()
        );
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }
    (_, Some(group), GraphState::Peek) => {
      let mut combined = group.clone();
      combined.append(&mut follow_pairs.iter().to_follow_set());

      if combined.iter().all_items_are_from_same_peek_origin() {
        resolve_peek(graph, combined.iter(), sym, par);
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
    (len, collide, graph_state) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: {graph_state:?} len:{len} collide:{collide:?} sym:{} \n[ {} ]\n\n{}",
        sym.debug_string(db),
        cmpl.to_debug_string("\n"),
        graph.debug_string()
      );
      #[cfg(not(debug_assertions))]
      unimplemented!()
    }
  }

  SherpaResult::Ok(())
}

fn resolve_conflicting_symbols<'db, 'follow>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  par: StateId,
  sym: SymbolId,
  completed_items: ItemSet<'db>,
  g_state: GraphState,
) -> SherpaResult<()> {
  #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
  enum SymbolPriorities {
    Defined,
    Class,
  }

  // Map items according to their symbols
  let symbol_groups =
    hash_group_btreemap(completed_items.clone(), |_, i| i.origin.get_symbol(graph.get_db()));

  let priority_groups = hash_group_btreemap(symbol_groups, |_, (sym, _)| match sym {
    sym if sym.is_class() => Class,
    _ => Defined,
  });

  use SymbolPriorities::*;
  let completed: Option<&ItemSet>;

  for (priority, groups) in priority_groups {
    let max_precedence = groups.keys().map(|i| i.precedence()).max().unwrap_or_default();

    // Filter out members with lower precedences
    let groups = groups
      .into_iter()
      .filter(|(i, _)| i.precedence() >= max_precedence)
      .collect::<BTreeMap<_, _>>();

    match priority {
      Defined | Class => {
        if groups.len() > 1 {
          j.report_mut().add_error(SherpaError::SourcesError {
            id:       "conflicting-symbols",
            msg:      "Found ".to_string()
              + &groups.len().to_string()
              + " conflicting Defined symbols. Grammar is ambiguous",
            ps_msg:   Default::default(),
            severity: SherpaErrorSeverity::Critical,
            sources:  groups
              .iter()
              .map(|(sym, items)| {
                items.iter().map(|i| (i.rule().tok.clone(), Default::default(), Default::default()))
              })
              .flatten()
              .collect(),
          });
          return SherpaResult::Err(SherpaError::Text("Grammar conflicts".to_string()));
        } else {
          completed = Some(groups.values().next().unwrap());
        }
      }
      _ => completed = None,
    }

    if let Some(completed_items) = completed {
      handle_completed_item(
        j,
        graph,
        (*(completed_items.first()?), completed_items.clone().to_vec()),
        par,
        sym,
        g_state,
      );
      break;
    } else {
      panic!("Could not resolve Symbol ambiguities!")
    }
  }
  SherpaResult::Ok(())
}

fn peek_items_are_from_goto_state(cmpl: &Items, graph: &mut Graph) -> bool {
  debug_assert_eq!(
    cmpl
      .iter()
      .map(|i| {
        let Origin::Peek(_,origin) = i.origin  else {
      panic!("")
    };
        origin
      })
      .collect::<OrderedSet<_>>()
      .len(),
    1
  );
  match cmpl[0].origin {
    Origin::Peek(_, origin) => graph[origin].get_type().is_goto(),
    _ => false,
  }
}

fn create_out_of_scope_complete_state<'db, 'follow>(
  out_of_scope: Items<'db>,
  graph: &mut Graph<'follow, 'db>,
  sym: &SymbolId,
  parent: StateId,
  is_scan: bool,
) {
  let transition_type = match (out_of_scope[0].origin, is_scan) {
    (_, true) => StateType::ScannerCompleteOOS,
    _ => StateType::ProductionCompleteOOS,
  };
  let state = graph.create_state(*sym, transition_type, Some(parent), out_of_scope);
  graph.add_leaf_state(state);
}

fn merge_occluding<'db, 'follow>(
  j: &mut Journal,
  is_scan: bool,
  from_groups: OrderedMap<SymbolId, ItemSet<'db>>,
  into_groups: &mut OrderedMap<SymbolId, ItemSet<'db>>,
) {
  for (sym, group) in into_groups.iter_mut() {
    let mut occluding_items = get_set_of_occluding_items(
      j,
      sym,
      group,
      &from_groups,
      is_scan,
      group.first().unwrap().get_db(),
    );
    group.append(&mut occluding_items);
  }
}

fn get_set_of_occluding_items<'db, 'follow>(
  j: &mut Journal,
  into_sym: &SymbolId,
  into_group: &ItemSet<'db>,
  groups: &OrderedMap<SymbolId, ItemSet<'db>>,
  is_scanner: bool,
  db: &ParserDatabase,
) -> ItemSet<'db> {
  let mut occluding = ItemSet::new();
  let into_group_precedence = get_max_exclusivity(into_group);

  if (into_group_precedence >= 9999) {
    return occluding;
  }

  if is_scanner {
    for (from_sym, from_group) in groups.iter().filter(|(other_sym, _)| into_sym != *other_sym) {
      if symbols_occlude(into_sym, from_sym, db) {
        #[cfg(debug_assertions)]
        {
          j.report_mut().add_note("Symbol Group Merge", 
            format!(
            "\nDue to the ambiguous symbols [{} ≈ {}] the group [\n\n{}\n\n] will be merged into [\n\n{}\n\n]\n",
            into_sym.debug_string(db),
            from_sym.debug_string(db),
            from_group.to_debug_string("\n"),
            into_group.to_debug_string("\n")
          ));
        }
        occluding.append(&mut from_group.clone());
      }
    }
  }

  occluding
}

fn create_transition_groups<'db, 'follow>(
  graph: &mut Graph<'follow, 'db>,
  parent: StateId,
) -> SherpaResult<OrderedMap<SymbolId, ItemSet<'db>>> {
  let closure = graph[parent].get_closure_ref()?;

  let mut groups = hash_group_btreemap(closure.iter().cloned().collect::<ItemSet>(), |_, item| {
    match item.get_type() {
      ItemType::Completed(_) => SymbolId::Default,
      ItemType::Terminal(sym) => sym.to_plain(),
      ItemType::NonTerminal(_) => SymbolId::Undefined,
      ItemType::TokenNonTerminal(..) if graph.is_scan() => SymbolId::Undefined,
      ItemType::TokenNonTerminal(_, sym) => sym.to_plain(),
    }
  });

  groups.remove(&SymbolId::Undefined);

  if graph.is_scan() {
    groups = groups
      .into_iter()
      .filter_map(|(s, g)| {
        let max_exclusivity = g.iter().map(|i| i.exclusivity()).max().unwrap_or_default();

        let g = g
          .clone()
          .into_iter()
          .filter(|i| i.exclusivity() >= max_exclusivity)
          .collect::<BTreeSet<_>>();
        if g.is_empty() {
          None
        } else {
          Some((s, g))
        }
      })
      .collect();
  }

  SherpaResult::Ok(groups)
}

fn get_completed_item_artifacts<'a, 'db: 'a, 'follow, T: ItemContainerIter<'a, 'db>>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  par: StateId,
  completed: T,
) -> SherpaResult<CompletedItemArtifacts<'db>> {
  let mut oos_pairs = OrderedSet::new();
  let mut follow_pairs = OrderedSet::new();
  let mut follow_items = ItemSet::new();
  let mut default_only_items = ItemSet::new();

  for c_i in completed {
    let (f, c) = get_follow(j, graph, *c_i)?;

    if f.is_empty() {
      default_only_items.insert(*c_i);
    } else {
      follow_pairs.append(
        &mut f
          .create_closure(graph.is_scan(), par)
          .into_iter()
          .map(|i| (*c_i, i.to_origin(c_i.origin)).into())
          .collect(),
      );
      follow_items.append(&mut f.to_set());
    }

    if !c_i.is_out_of_scope() {
      let goals: ItemSet = get_goal_items_from_completed(&c, graph);

      for goal in goals {
        let (follow, _) = get_follow(
          j,
          graph,
          goal.to_complete().to_origin(Origin::ScanCompleteOOS).to_oos_index(),
        )?;
        oos_pairs.append(
          &mut follow.create_closure(false, par).into_iter().map(|i| (*c_i, i).into()).collect(),
        );
      }
    }
  }

  SherpaResult::Ok(CompletedItemArtifacts {
    follow_items,
    follow_pairs,
    default_only: default_only_items,
    oos_pairs,
  })
}

/// Returns all incomplete items that follow the given completed item,
/// and all completed items that were encountered, including the initial item.
pub(super) fn get_follow<'db, 'follow>(
  _j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  item: Item<'db>,
) -> SherpaResult<(Items<'db>, Items<'db>)> {
  if !item.is_complete() {
    return SherpaResult::Ok((vec![item], vec![]));
  }

  let mut completed = OrderedSet::new();
  let mut follow = OrderedSet::new();
  let mut queue = VecDeque::from_iter(vec![item]);

  while let Some(item) = queue.pop_front() {
    if completed.insert(item) {
      let prod_id = item.prod_index();
      let closure = if item.is_out_of_scope() {
        graph[item.origin_state]
          .get_root_closure_ref()?
          .iter()
          .filter(|i| i.is_out_of_scope() && i.prod_index_at_sym().unwrap_or_default() == prod_id)
          .map(|i| i.to_origin(item.origin))
          .collect::<Array<_>>()
      } else {
        graph[item.origin_state]
          .get_closure_ref()?
          .into_iter()
          .filter(|i| i.prod_index_at_sym().unwrap_or_default() == prod_id && i.goal == item.goal)
          .cloned()
          .collect::<Array<_>>()
      };

      if closure.len() > 0 {
        for item in closure.try_increment() {
          match item.get_type() {
            ItemType::Completed(_) => queue.push_back(item),
            _ => {
              follow.insert(item);
            }
          }
        }
      } else if !item.origin_state.is_root() {
        let parent_state = graph[item.origin_state].get_parent();
        queue.push_back(item.to_origin_state(parent_state));
      }
    }
  }

  SherpaResult::Ok((follow.to_vec(), completed.to_vec()))
}

fn get_goal_items_from_completed<'db, 'follow>(
  items: &Items<'db>,
  graph: &Graph<'follow, 'db>,
) -> ItemSet<'db> {
  items.iter().filter(|i| graph.item_is_goal(*i)).cloned().collect()
}

fn get_kernel_items_from_peek_item<'db, 'follow>(
  graph: &Graph<'follow, 'db>,
  peek_item: &Item<'db>,
) -> Items<'db> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
        panic!("Invalid peek origin");
    };

  graph[peek_origin].get_resolve_items(peek_index)
}

fn resolve_peek<'a, 'db: 'a, 'follow, T: ItemContainerIter<'a, 'db>>(
  graph: &mut Graph<'follow, 'db>,
  mut completed: T,
  sym: SymbolId,
  par: StateId,
) {
  let kernel_items = get_kernel_items_from_peek(graph, completed.next().unwrap());
  let state = graph.create_state(sym, StateType::PeekEnd, Some(par), kernel_items);
  graph.enqueue_pending_state(GraphState::Normal, state);
}

/// Compares whether symbolB occludes symbolA
/// ( produces an ambiguous parse path )
///
/// Symbols that can occlude are as follows
///
/// - `g:id` and any single identifier character.
/// - `g:num` and any single numeric character.
/// - `g:sym` and any single character thats not a numeric, identifier, space,
///   newline, or tab.

fn symbols_occlude(symA: &SymbolId, symB: &SymbolId, db: &ParserDatabase) -> bool {
  match symA {
    SymbolId::Char { char, .. } => match symB {
      SymbolId::ClassNumber { .. } => {
        (*char < 128)
          && get_token_class_from_codepoint(*char as u32) == CodePointClass::Number as u32
      }
      SymbolId::ClassIdentifier { .. } => {
        (*char < 128)
          && get_token_class_from_codepoint(*char as u32) == CodePointClass::Identifier as u32
      }
      SymbolId::ClassSymbol { .. } => {
        (*char < 128)
          && get_token_class_from_codepoint(*char as u32) == CodePointClass::Symbol as u32
      }
      SymbolId::Default => false,
      symB => *symA == *symB,
    },
    SymbolId::Codepoint { val, .. } => match symB {
      SymbolId::ClassNumber { .. } => {
        get_token_class_from_codepoint(*val) == CodePointClass::Number as u32
      }
      SymbolId::ClassIdentifier { .. } => {
        get_token_class_from_codepoint(*val) == CodePointClass::Identifier as u32
      }
      SymbolId::ClassSymbol { .. } => {
        get_token_class_from_codepoint(*val) == CodePointClass::Symbol as u32
      }
      SymbolId::Default => false,
      symB => *symA == *symB,
    },
    SymbolId::Default => false,
    symA => *symA == *symB,
  }
}

fn handle_completed_item<'db, 'follow>(
  j: &mut Journal,
  graph: &mut Graph<'follow, 'db>,
  (completed_item, completed_items): (Item<'db>, Items<'db>),
  parent: StateId,
  sym: SymbolId,
  graph_state: GraphState,
) -> SherpaResult<()> {
  let is_scan = graph.is_scan();

  // Determine if origin contains GOTOs.
  match (completed_item.origin, is_scan) {
    (Origin::GoalCompleteOOS, ..) => {
      let state = graph.create_state(sym, StateType::ProductionCompleteOOS, Some(parent), vec![]);
      graph.add_leaf_state(state);
    }
    // Completion of parse tree may be premature
    // or item is not an acceptable completed item
    (_, true) => {
      let (follow, completed_items): (Vec<Items>, Vec<Items>) =
        completed_items.into_iter().map(|i| get_follow(j, graph, i).unwrap()).unzip();
      let follow = follow.into_iter().flatten().collect::<Items>();
      let completed_items = completed_items.into_iter().flatten().collect::<Items>();

      let goals = get_goal_items_from_completed(&completed_items, graph);

      let is_continue = !follow.is_empty();
      let is_goal = !goals.is_empty();

      let state = graph.create_state(
        sym,
        match (is_continue, goals.first().map(|d| d.origin)) {
          (true, Some(Origin::TokenGoal(tok_id))) => StateType::AssignAndFollow(tok_id),
          (false, Some(Origin::TokenGoal(tok_id))) => StateType::AssignToken(tok_id),
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
      let state =
        graph.create_state(sym, StateType::Reduce(completed_item.rule_id), Some(parent), vec![]);
      graph[state].set_reduce_item(completed_item);
      graph.add_leaf_state(state);
    }
  }

  SherpaResult::Ok(())
}

fn create_reduce_reduce_error(
  j: &mut Journal,
  graph: &Graph,
  end_items: ItemSet,
) -> SherpaResult<()> {
  let db = graph.get_db();
  let goals = end_items.iter().flat_map(|i| get_goal_items(&graph, i)).collect::<OrderedSet<_>>();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "reduce-conflict",
    msg:      "Unresolvable parse conflict encountered".into(),
    ps_msg:   {
      let mut string = "Enable the following configs to use an alternative parse strategy".into();

      if !graph.is_scan() {
      } else {
        let prod = &goals.first()?.prod_index();
        let name = db.prod_guid_name_string(*prod).as_str().to_string();

        string += format!(
          "\n - Turn production <{name}> into a PEG by using one of the PEG mode specifiers:",
        )
        .as_str();
        string += "\n    [ FIRST_MATCH | LONGEST_MATCH | SHORTEST_MATCH ]";
        string += format!("\n\n    Example: <> {name} FIRST_MATCH >  ...",).as_str();
      }
      string
    },
    severity: SherpaErrorSeverity::Critical,
    sources:  Default::default(),
  });

  SherpaResult::Ok(())
}

fn get_goal_items<'db, 'follow>(graph: &'db Graph<'follow, 'db>, item: &Item<'db>) -> Items<'db> {
  match item.origin {
    Origin::TokenGoal(_) | Origin::ProdGoal(_) => {
      vec![graph[0].kernel_items_ref().clone().to_vec()[item.goal as usize]]
    }
    Origin::Peek(..) => get_kernel_items_from_peek_item(graph, item)
      .iter()
      .flat_map(|_| get_goal_items(graph, item))
      .collect(),
    _ => vec![],
  }
}

fn get_max_precedence(group: &ItemSet) -> u32 {
  group.iter().map(|i| i.precedence()).max().unwrap_or_default() as u32
}

fn get_max_exclusivity(group: &ItemSet) -> u32 {
  group.iter().map(|i| i.exclusivity()).max().unwrap_or_default() as u32
}
