use crate::{
  grammar::hash_id_value_u64,
  parser::utils::{hash_group_btreemap, symbols_occlude},
  types::{
    graph::{StateId, *},
    item::{Item, ItemContainer, ItemSet, ItemType, Items},
    *,
  },
  Journal,
  SherpaResult,
};
use core::panic;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub(crate) fn create(
  j: &mut Journal,
  kernel_items: Vec<Item>,
  mode: GraphMode,
) -> SherpaResult<Graph> {
  let mut graph = Graph::new(j.grammar()?, mode);

  let root =
    graph.create_state(SymbolID::Start, StateType::Start, None, kernel_items);

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
  let g = &(graph.grammar.clone());
  debug_assert!(parent.is_root() || !is_peg_state(graph_state));

  if parent.is_root() && is_peg_state(graph_state) {
    for item in graph[parent].get_kernel_items_vec() {
      let transition = match graph_state {
        GraphState::FirstMatch => StateType::FirstMatch,
        GraphState::LongestMatch => StateType::LongestMatch,
        GraphState::ShortestMatch => StateType::ShortestMatch,
        _ => unreachable!(),
      };
      graph[parent].set_transition_type(transition);
      let state = graph.create_state(
        Default::default(),
        StateType::Start,
        Some(parent),
        vec![item],
      );
      graph.enqueue_pending_state(GraphState::Normal, state);
    }
  } else {
    let (closure, mut groups) = create_transition_groups(graph, parent, g);

    handle_completed_items(j, graph, parent, graph_state, &mut groups)?;

    merge_occluding(groups.clone(), &mut groups, j, graph.is_scan());

    let out_items =
      handle_incomplete_items(j, graph, parent, graph_state, groups, &closure)?;

    if graph_state != GraphState::Peek && !graph.is_scan() {
      handle_goto(j, graph, parent, out_items)?;
    }
  }

  SherpaResult::Ok(())
}

fn create_transition_groups(
  graph: &mut Graph,
  parent: StateId,
  g: &std::sync::Arc<GrammarStore>,
) -> (BTreeSet<Item>, BTreeMap<SymbolID, BTreeSet<Item>>) {
  let closure = graph[parent].get_closure(g, graph.is_scan());

  let precedence = closure
    .iter()
    .filter(|i| i.is_completed())
    .fold(0, |a, i| a.max(i.get_precedence(g)));

  let mut groups = hash_group_btreemap(
    closure
      .iter()
      .filter(|i| i.get_precedence(g) >= precedence)
      .cloned()
      .collect::<ItemSet>(),
    |_, item| match item.get_type(g) {
      ItemType::ExclusiveCompleted(..) | ItemType::Completed(_) => {
        SymbolID::Default
      }
      ItemType::Terminal(sym) => sym,
      ItemType::NonTerminal(_) => SymbolID::Undefined,
      ItemType::TokenProduction(..) if graph.is_scan() => SymbolID::Undefined,
      ItemType::TokenProduction(_, sym) => sym,
    },
  );

  groups.remove(&SymbolID::Undefined);
  (closure, groups)
}

fn merge_occluding(
  from_groups: BTreeMap<SymbolID, BTreeSet<Item>>,
  into_groups: &mut BTreeMap<SymbolID, BTreeSet<Item>>,
  j: &mut Journal,
  is_scan: bool,
) {
  for (sym, group) in into_groups.iter_mut() {
    let mut occluding_items =
      get_set_of_occluding_items(j, sym, group, &from_groups, is_scan);
    group.append(&mut occluding_items);
  }
}

fn handle_incomplete_items(
  _j: &mut Journal,
  graph: &mut Graph,
  parent: StateId,
  graph_state: GraphState,
  groups: BTreeMap<SymbolID, ItemSet>,
  closure: &ItemSet,
) -> SherpaResult<ItemSet> {
  let mut out_items = BTreeSet::new();
  let is_scan = graph.is_scan();
  for (sym, mut group) in groups {
    match (group.len(), graph_state) {
      (1, GraphState::Peek) => {
        let kernel_items =
          get_kernel_items_from_peek_item(graph, *group.first()?);

        match kernel_items[0].origin {
          _ => {
            let state = graph.create_state(
              sym,
              StateType::PeekEnd,
              Some(parent),
              kernel_items,
            );
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
            get_kernel_items_from_peek_item(graph, *group.first()?),
          );
          graph.enqueue_pending_state(GraphState::Normal, state);
        } else {
          let state = graph.create_state(
            sym,
            StateType::Peek,
            Some(parent),
            group.try_increment(),
          );

          graph.enqueue_pending_state(graph_state, state);
        }
      }
      (_, GraphState::Normal) => {
        let out_of_scope =
          group.drain_filter(|i| i.is_out_of_scope()).collect::<Vec<_>>();
        let mut in_scope = group;

        match (in_scope.len(), out_of_scope.len()) {
          (0, 1..) => {
            create_out_of_scope_complete_state(
              out_of_scope,
              graph,
              &sym,
              parent,
              is_scan,
            );
          }
          (_, 1..) if is_scan => {
            create_out_of_scope_complete_state(
              out_of_scope,
              graph,
              &sym,
              parent,
              is_scan,
            );
          }
          (1.., _) => {
            if let Some(shifted_items) = create_call(
              &in_scope.clone().to_vec(),
              graph,
              graph_state,
              parent,
              &closure,
              sym,
              is_scan,
            ) {
              out_items.append(&mut shifted_items.to_set());
            } else {
              let state = graph.create_state(
                sym,
                StateType::Shift,
                Some(parent),
                in_scope.try_increment(),
              );

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

fn get_completed_item_artifacts<'a, T: ItemContainerIter<'a>>(
  j: &mut Journal,
  graph: &mut Graph,
  par: StateId,
  completed: T,
) -> SherpaResult<CompletedItemArtifacts> {
  let g = &(graph.grammar.clone());
  let mut oos_pairs = BTreeSet::new();
  let mut follow_pairs = BTreeSet::new();
  let mut follow_items = ItemSet::new();
  let mut default_only_items = ItemSet::new();

  for c_i in completed {
    let (f, c) = get_follow(j, graph, *c_i)?;

    if f.is_empty() {
      default_only_items.insert(*c_i);
    } else {
      follow_pairs.append(
        &mut get_closure(f.iter(), g, par, false)
          .into_iter()
          .map(|i| (*c_i, i.to_origin(c_i.origin)).into())
          .collect(),
      );
      follow_items.append(&mut f.to_set());
    }

    if !c_i.is_out_of_scope() {
      let goals: ItemSet = get_goal_items_from_completed(c, graph);

      for goal in goals {
        let (follow, _) = get_follow(
          j,
          graph,
          goal.to_complete().to_origin(Origin::ScanCompleteOOS).to_oos_index(),
        )?;
        oos_pairs.append(
          &mut get_closure(follow.iter(), g, par, false)
            .into_iter()
            .map(|i| (*c_i, i).into())
            .collect(),
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

fn handle_completed_items(
  j: &mut Journal,
  graph: &mut Graph,
  par: StateId,
  g_state: GraphState,
  groups: &mut BTreeMap<SymbolID, ItemSet>,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());
  let is_scan = graph.is_scan();

  if let Some(completed) = groups.remove(&SymbolID::Default) {
    let CompletedItemArtifacts {
      follow_pairs, follow_items, default_only, ..
    } = get_completed_item_artifacts(j, graph, par, completed.iter())?;

    if graph.is_scan() {
      graph[par].add_kernel_items(g, follow_items, is_scan);
    }

    if is_scan
    /* || is_breadcrumb */
    {
      // Dump follow into groups now?
      //*
      merge_occluding(
        hash_group_btreemap(
          follow_pairs.iter().map(|fp| fp.follow).collect::<ItemSet>(),
          |_, item| match item.get_type(g) {
            ItemType::Terminal(sym) => sym,
            _ => SymbolID::Undefined,
          },
        ),
        groups,
        j,
        is_scan,
      );
      // */
      get_oos_follow_from_completed(
        j,
        graph,
        &completed.iter().to_vec(),
        &mut |follow: Items| {
          merge_items_into_groups(&follow, g, par, is_scan, groups)
        },
      )?;
    }

    let default =
      completed.iter().map(|i| -> FollowPair { (*i, *i).into() }).collect();
    handle_completed_groups(
      j,
      graph,
      groups,
      par,
      g_state,
      SymbolID::Default,
      default,
      &default_only,
    )?;

    if !follow_pairs.is_empty() {
      // If not scanner and there exists any ambiguity, we must use either
      // a peek or breadcrumb resolution strategy to overcome this ambiguity.
      // If we are doing fork or PEG-like states then we don't have to make
      // these considerations.
      let mut completed_groups =
        hash_group_btreemap(follow_pairs.clone(), |_, fp| {
          match fp.follow.get_type(g) {
            ItemType::Completed(_) | ItemType::ExclusiveCompleted(..) => {
              unreachable!("Should be handled outside this path")
            }
            ItemType::Terminal(sym) => sym,
            _ => SymbolID::Undefined,
          }
        });
      completed_groups.remove(&SymbolID::Undefined);

      for (sym, follow_pairs) in completed_groups {
        handle_completed_groups(
          j,
          graph,
          groups,
          par,
          g_state,
          sym,
          follow_pairs,
          &default_only,
        )?;
      }
    }
  }

  SherpaResult::Ok(())
}

fn handle_completed_groups(
  j: &mut Journal,
  graph: &mut Graph,
  groups: &mut BTreeMap<SymbolID, ItemSet>,
  par: StateId,
  g_state: GraphState,
  sym: SymbolID,
  follow_pairs: BTreeSet<FollowPair>,
  default_only_items: &ItemSet,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());
  let is_scan = graph.is_scan();
  let mut cmpl = follow_pairs.iter().to_completed_vec();
  match (follow_pairs.len(), groups.remove(&sym), g_state) {
    (1, None, GraphState::Normal) => {
      handle_completed_item(j, graph, (cmpl[0], cmpl), par, sym, g_state)?;
    }
    (2.., None, GraphState::Normal) => {
      if is_scan {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_symbols(
          j,
          graph,
          par,
          sym,
          cmpl.to_set(),
          g_state,
        )?;
      } else if cmpl.clone().to_set().to_absolute().len() == 1 {
        // The same production is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(
          j,
          graph,
          (cmpl[0], vec![cmpl[0]]),
          par,
          sym,
          g_state,
        )?;
      } else if cmpl.iter().all_are_out_of_scope() {
        // We are at the end of a lookahead that results in the completion of
        // some existing item.
        let item = *cmpl.first()?;
        handle_completed_item(j, graph, (item, vec![item]), par, sym, g_state);
      } else {
        let unfollowed_items: Items = default_only_items
          .intersection(&cmpl.iter().to_set())
          .cloned()
          .collect();

        match unfollowed_items.len() {
          2.. => {
            // Only an issue if there are no follow actions.
            // And even then, we could try using peek states to handle this
            // outcome. Reduce Reduce conflict
            // ------------------------------------------------------
            // Could pick a specific item to reduce based on precedence or some
            // kind of exclusive weight. Perhaps also walking back
            // up the graph to find a suitable divergent point add a
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
      } else if group.iter().all_are_out_of_scope()
        && cmpl.iter().all_are_out_of_scope()
      {
        cmpl.append(&mut group.to_vec());
        create_out_of_scope_complete_state(cmpl, graph, &sym, par, is_scan);
      } else {
        let cardinal = group.clone().to_absolute();
        let unique = follow_pairs
          .into_iter()
          .filter(|fp| !cardinal.contains(&&fp.follow.to_absolute()))
          .collect::<BTreeSet<_>>();

        if !unique.is_empty() {
          group.append(&mut get_set_of_occluding_items(
            j, &sym, &group, &groups, is_scan,
          ));
          create_peek(
            graph,
            sym,
            par,
            group.iter(),
            unique,
            true,
            StateType::Peek,
          );
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
          .flat_map(|fp| get_kernel_items_from_peek_item(graph, fp.completed))
          .collect::<ItemSet>();

        //let completed = kernel_items.clone().completed_items();
        let incomplete = kernel_items.incomplete_items();

        if incomplete.len() == 1 {
          let state = graph.create_state(
            sym,
            StateType::PeekEnd,
            Some(par),
            incomplete.to_vec(),
          );
          graph.enqueue_pending_state(GraphState::Normal, state);
        }
      } else {
        let kernel_items = follow_pairs
          .iter()
          .flat_map(|fp| get_kernel_items_from_peek_item(graph, fp.completed))
          .collect::<ItemSet>();
        unimplemented!(
          "\nCompleted Peek Items On Symbol:[{}]\n \n\nAcceptItems\n{}\n\nPeekItems:\n{}\n\nKernelItems:\n{}\n\nParent State\n{}\n\nGraph:\n{}",

          sym.debug_string(g),
          graph.goal_items().to_debug_string(g, "\n"),
          cmpl.to_debug_string(g, "\n"),
          kernel_items.to_debug_string(g, "\n"),
          graph[par].debug_string(g),
          graph.__debug_string__()
        )
      }
    }
    (_, Some(group), GraphState::Peek) => {
      let mut combined = group.clone();
      combined.append(&mut follow_pairs.iter().to_follow_set());

      if combined.iter().all_items_are_from_same_peek_origin() {
        resolve_peek(graph, combined.iter(), sym, par);
      } else {
        todo!(
          "Roll the follow states into the group and resubmit to incomplete handler function.\nincomplete:\n{}\ncomplete:\n{}\nfollow:\n{}\n \n {}",
          group.to_debug_string(g, "\n"),
          follow_pairs.iter().to_completed_vec().to_debug_string(g, "\n"),
          follow_pairs.iter().to_follow_vec().to_debug_string(g, "\n"),
          graph.__debug_string__()

        );
      }
    }
    (len, collide, graph_state) => {
      unimplemented!(
        "\nNot Implemented: {graph_state:?} len:{len} collide:{collide:?} sym:{} \n[ {} ]\n\n{}",
        sym.debug_string(g),
        cmpl.to_debug_string(g, "\n"),
        graph.__debug_string__()
      )
    }
  }

  SherpaResult::Ok(())
}

fn peek_items_are_from_goto_state(cmpl: &Vec<Item>, graph: &mut Graph) -> bool {
  debug_assert_eq!(
    cmpl
      .iter()
      .map(|i| {
        let Origin::Peek(_,origin) = i.origin  else {
      panic!("")
    };
        origin
      })
      .collect::<BTreeSet<_>>()
      .len(),
    1
  );
  match cmpl[0].origin {
    Origin::Peek(_, origin) => graph[origin].get_type().is_goto(),
    _ => false,
  }
}

fn merge_items_into_groups(
  follow: &Vec<Item>,
  g: &std::sync::Arc<GrammarStore>,
  par: StateId,
  is_scan: bool,
  groups: &mut BTreeMap<SymbolID, BTreeSet<Item>>,
) {
  // Dumb symbols that could cause termination of parse into the intermediate
  // item groups
  for (sym, group) in hash_group_btreemap(
    get_closure(follow.iter(), g, par, is_scan)
      .into_iter()
      .filter(|i| i.is_term(g))
      .collect::<BTreeSet<_>>(),
    |_, i| i.get_symbol(g),
  ) {
    if !groups.contains_key(&sym) {
      groups.insert(sym, group);
    }
  }
}

fn create_out_of_scope_complete_state(
  out_of_scope: Vec<Item>,
  graph: &mut Graph,
  sym: &SymbolID,
  parent: StateId,
  is_scan: bool,
) {
  let transition_type = match (out_of_scope[0].origin, is_scan) {
    (_, true) => StateType::ScannerCompleteOOS,
    _ => StateType::ProductionCompleteOOS,
  };
  let state =
    graph.create_state(*sym, transition_type, Some(parent), out_of_scope);
  graph.add_leaf_state(state);
}

// Inserts out of scope sentinel items into the existing
// items groups if we are in scanner mode and the item that
// was completed belongs to the parse state goal set.
fn get_oos_follow_from_completed(
  j: &mut Journal,
  graph: &Graph,
  completed_items: &Items,
  handler: &mut dyn FnMut(Items),
) -> SherpaResult<()> {
  let mut out = ItemSet::new();
  for completed_item in completed_items {
    if !completed_item.is_out_of_scope() {
      let (_, completed) = get_follow(j, graph, *completed_item)?;

      let goals: ItemSet = get_goal_items_from_completed(completed, graph);

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

fn get_goal_items_from_completed(
  items: Vec<Item>,
  graph: &Graph,
) -> BTreeSet<Item> {
  items.into_iter().filter(|i| graph.item_is_goal(*i)).collect()
}

fn resolve_peek<'a, T: ItemContainerIter<'a>>(
  graph: &mut Graph,
  mut completed: T,
  sym: SymbolID,
  par: StateId,
) {
  let kernel_items =
    get_kernel_items_from_peek_item(graph, *completed.next().unwrap());
  let state =
    graph.create_state(sym, StateType::PeekEnd, Some(par), kernel_items);
  graph.enqueue_pending_state(GraphState::Normal, state);
}

fn resolve_conflicting_symbols(
  j: &mut Journal,
  graph: &mut Graph,
  par: StateId,
  sym: SymbolID,
  completed_items: ItemSet,
  g_state: GraphState,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());

  #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
  enum SymbolPriorities {
    ExclusiveDefined,
    Defined,
    Production,
    Generic,
  }

  // Map items according to their symbols
  let symbol_groups =
    hash_group_btreemap(completed_items.clone(), |_, i| i.origin.get_symbol());
  let priority_groups =
    hash_group_btreemap(symbol_groups, |_, (sym, _)| match sym {
      sym if sym.is_exclusive() => ExclusiveDefined,
      sym if sym.is_defined() => Defined,
      sym if sym.is_production() => Production,
      _ => Generic,
    });
  use SymbolPriorities::*;
  let completed: Option<&ItemSet>;

  for (priority, groups) in priority_groups {
    match priority {
      ExclusiveDefined => {
        if groups.len() > 1 {
          panic!(
            "Found {} conflicting Exclusive Defined symbols. Grammar is ambiguous",
            groups.len()
          );
        } else {
          completed = Some(groups.values().next().unwrap());
        }
      }
      Defined => {
        if groups.len() > 1 {
          panic!(
            "Found {} conflicting Defined symbols. Grammar is ambiguous",
            groups.len()
          );
        } else {
          completed = Some(groups.values().next().unwrap());
        }
      }
      Production => {
        if groups.len() > 1 {
          panic!(
            "\nFound {} conflicting Token Production symbols. Grammar is ambiguous:\n{}",
            groups.len(),
            groups
              .iter()
              .map(|(s, _)| match s {
                SymbolID::TokenProduction(.., prod_id) => {
                  g.get_production(prod_id).unwrap().loc.blame(
                    1,
                    1,
                    &format!("[ {} ] first defined here", s.debug_string(g)),
                    BlameColor::RED,
                  )
                }
                _ => String::new(),
              })
              .collect::<Vec<_>>()
              .join("\n")
          );
        } else {
          completed = Some(groups.values().next().unwrap());
        }
      }
      Generic => {
        if groups.len() > 1 {
          panic!(
            "Found {} conflicting Generic symbols. Grammar is ambiguous",
            groups.len()
          );
        } else {
          completed = Some(groups.values().next().unwrap());
        }
      }
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

fn handle_goto(
  j: &mut Journal,
  graph: &mut Graph,
  parent: StateId,
  out_items: ItemSet,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());
  let non_terminals = graph[parent].get_closure(g, false).non_term_items(g);
  let kernel_base = graph[parent].get_kernel_items();

  let out_items: ItemSet = if parent.is_root() {
    out_items
  } else {
    out_items
      .into_iter()
      .filter(|i| !kernel_base.contains(i) || i.is_start())
      .collect()
  };

  if !non_terminals.is_empty() && !out_items.is_empty() {
    let mut kernel_prod_ids = kernel_base.iter().to_production_id_set(g);
    let mut used_non_terms = BTreeSet::new();
    let mut seen = BTreeSet::new();
    let mut queue =
      VecDeque::from_iter(out_items.iter().map(|i| i.get_prod_id(g)));

    while let Some(prod_id) = queue.pop_front() {
      if seen.insert(prod_id) {
        for item in non_terminals
          .iter()
          .filter(|i| i.get_production_id_at_sym(g) == prod_id)
        {
          used_non_terms.insert(*item);
          if !kernel_base.contains(item) || item.is_start() {
            queue.push_back(item.get_prod_id(g));
          }
        }
      }
    }

    if !used_non_terms.is_empty() {
      if parent.0 == 2 {
        j.report_mut()
          .add_note("goto-seeds", out_items.to_debug_string(g, "\n\n"));
        j.report_mut()
          .add_note("goto-results", used_non_terms.to_debug_string(g, "\n\n"));
      }

      graph[parent].set_non_terminals(&used_non_terms);

      let used_goto_groups = hash_group_btreemap(used_non_terms, |_, t| {
        t.get_production_id_at_sym(g)
      });

      for (prod_id, items) in &used_goto_groups {
        let sym_id = g.get_production(&prod_id)?.sym_id;

        let transition_type = items
          .iter()
          .any(|i| -> bool {
            let p = i.get_prod_id(g);
            let recursive = p == i.get_production_id_at_sym(g);
            recursive.then_some(i.at_start()).unwrap_or(
              !kernel_base.contains(i) && used_goto_groups.contains_key(&p),
            )
          })
          .then_some(StateType::GotoLoop)
          .unwrap_or(StateType::KernelGoto);

        let incremented_items = items.try_increment();
        let incomplete = incremented_items.clone().incomplete_items();
        let mut completed = incremented_items.clone().completed_items();

        if kernel_prod_ids.remove(&prod_id)
          && parent.is_root()
          && completed.is_empty()
        {
          let item: Item =
            g.get_rule(&g.production_rules.get(prod_id)?[0])?.into();
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

          if let Some(s) = create_peek(
            graph,
            sym_id,
            parent,
            incomplete.iter(),
            fp,
            false,
            transition_type,
          ) {
            let c_p =
              completed.clone().into_iter().map(|i| (i, i).into()).collect();
            let graph_state = GraphState::Normal;
            let sym = SymbolID::Default;
            let groups = &mut Default::default();
            let s =
              graph.create_state(sym, StateType::PeekEnd, Some(s), completed);

            handle_completed_groups(
              j,
              graph,
              groups,
              s,
              graph_state,
              sym,
              c_p,
              &default,
            )?;
          }
        } else {
          let state = graph.create_state(
            sym_id,
            transition_type,
            Some(parent),
            incremented_items.clone(),
          );
          graph.enqueue_pending_state(GraphState::Normal, state);
        }
      }

      // The remaining goto productions are accept states for for this goto.
      for prod_id in kernel_prod_ids {
        let prod_sym = g.get_production(&prod_id)?.sym_id;
        let state = graph.create_state(
          prod_sym,
          StateType::GotoPass,
          Some(parent),
          vec![],
        );
        graph.add_leaf_state(state);
      }
    }
  }

  SherpaResult::Ok(())
}

fn get_goal_items(graph: &Graph, item: &Item) -> Vec<Item> {
  match item.origin {
    Origin::SymGoal(_) | Origin::ProdGoal(_) => {
      vec![graph[0].get_kernel_items_vec()[item.goal_index as usize]]
    }
    Origin::Peek(..) => get_kernel_items_from_peek_item(graph, *item)
      .iter()
      .flat_map(|_| get_goal_items(graph, item))
      .collect(),
    _ => vec![],
  }
}

fn is_peg_state(graph_state: GraphState) -> bool {
  matches!(
    graph_state,
    GraphState::LongestMatch
      | GraphState::FirstMatch
      | GraphState::ShortestMatch
  )
}

fn get_set_of_occluding_items(
  j: &mut Journal,
  into_sym: &SymbolID,
  into_group: &ItemSet,
  groups: &BTreeMap<SymbolID, ItemSet>,
  is_scanner: bool,
) -> ItemSet {
  let mut occluding = ItemSet::new();
  let g = &(j.grammar().unwrap());

  if is_scanner {
    for (from_sym, from_group) in
      groups.iter().filter(|(other_sym, _)| into_sym != *other_sym)
    {
      if symbols_occlude(into_sym, from_sym, g) {
        #[cfg(debug_assertions)]
        {
          if !j.occlusion_tracking_mode() {
            j.report_mut().add_note("Symbol Group Merge", 
            format!(
            "\nDue to the ambiguous symbols [{} ≈ {}] the group [\n\n{}\n\n] will be merged into [\n\n{}\n\n]\n",
            into_sym.debug_string(g),
            from_sym.debug_string(g),
            from_group.to_debug_string(g, "\n"),
            into_group.to_debug_string(g, "\n")
          ));
          }
        }
        occluding.append(&mut from_group.clone());
      }
    }
  }

  let precedence = into_group
    .iter()
    .filter(|i| {
      !i.is_completed() && i.increment().unwrap().get_symbol(g).is_eof()
    })
    .fold(0, |a, i| a.max(i.get_precedence(g)));

  occluding
    .iter()
    .filter(|i| i.get_precedence(g) >= precedence)
    .cloned()
    .collect::<ItemSet>()
}

fn create_peek<'a, T: ItemContainerIter<'a>>(
  graph: &mut Graph,
  sym: SymbolID,
  parent: StateId,
  mut incomplete_items: T,
  completed_item_pairs: BTreeSet<FollowPair>,
  need_increment: bool,
  transition_type: StateType,
) -> Option<StateId> {
  let g = &(graph.grammar.clone());
  let is_scan = graph.is_scan();
  let mut kernel_items = vec![];
  let mut resolve_items = vec![];
  let mut incomplete_items = incomplete_items.to_vec();

  let existing_prod_ids = incomplete_items.to_production_ids(g);
  let existing_items = incomplete_items.clone().to_absolute().to_set();
  let state = graph.create_state(sym, transition_type, Some(parent), vec![]);

  for (_, items) in
    hash_group_btreemap(completed_item_pairs, |_, fp| fp.completed.rule_id)
  {
    // All items here complete the same rule, so we group them all into one goal
    // index.
    let follow: ItemSet = items
      .iter()
      .filter_map(|FollowPair { follow, .. }| {
        if follow.is_completed() || {
          existing_items.contains(&follow)
            || (follow.at_start()
              && existing_prod_ids.contains(&follow.get_prod_id(g)))
        } {
          None
        } else {
          Some(*follow)
        }
      })
      .collect();

    if !follow.is_empty() {
      let index = hash_id_value_u64(&follow);
      for follow in follow {
        kernel_items.push(follow.to_origin(Origin::Peek(index, state)));
      }
      graph[state].set_peek_resolve_items(
        index,
        items.iter().to_completed_set().to_vec(),
      );
    }
  }

  let index = hash_id_value_u64(&incomplete_items);

  kernel_items.append(
    &mut incomplete_items
      .iter()
      .map(|i| i.to_origin(Origin::Peek(index, state)))
      .collect(),
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

  graph[state].set_peek_resolve_items(index, resolve_items);
  graph[state].set_kernel_items(
    g,
    if need_increment { kernel_items.try_increment() } else { kernel_items },
    is_scan,
  );
  graph.enqueue_pending_state(GraphState::Peek, state)
}

fn get_kernel_items_from_peek_item(
  graph: &Graph,
  peek_item: Item,
) -> Vec<Item> {
  let Origin::Peek(peek_index, peek_origin) = peek_item.origin else {
        panic!("Invalid peek origin");
    };

  graph[peek_origin].get_resolve_items(peek_index)
}

fn create_call(
  group: &Vec<Item>,
  graph: &mut Graph,
  graph_state: GraphState,
  parent: StateId,
  closure: &ItemSet,
  sym: SymbolID,
  is_scan: bool,
) -> Option<Vec<Item>> {
  let g = &(graph.grammar.clone());
  if all_items_come_from_same_production_call(group, g) {
    let prod_id = group[0].get_prod_id(g);

    if !graph[parent].conflicting_production_call(prod_id, g, is_scan) {
      let items = closure
        .iter()
        .filter(|i| {
          i.get_production_id_at_sym(g) == prod_id
            && i.get_prod_id(g) != prod_id
        })
        .cloned()
        .collect::<Vec<_>>();

      if items.len() > 0 {
        if let Some(items) =
          create_call(&items, graph, graph_state, parent, closure, sym, is_scan)
        {
          return Some(items);
        } else {
          let kernel_items = graph[parent].get_kernel_items_vec();
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

fn all_items_come_from_same_production_call(
  group: &Vec<Item>,
  g: &std::sync::Arc<GrammarStore>,
) -> bool {
  group.iter().all(|i| i.at_start())
    && group.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>().len()
      == 1
}

/// Returns all incomplete items that follow the given completed item,
/// and all completed items that were encountered, including the initial item.
pub(crate) fn get_follow(
  _j: &mut Journal,
  graph: &Graph,
  item: Item,
) -> SherpaResult<(Items, Items)> {
  let g = &(graph.grammar.clone());

  if !item.is_completed() {
    return SherpaResult::Ok((vec![item], vec![]));
  }

  let mut completed = BTreeSet::new();
  let mut follow = BTreeSet::new();
  let mut queue = VecDeque::from_iter(vec![item]);

  while let Some(item) = queue.pop_front() {
    if completed.insert(item) {
      let prod_id = item.get_prod_id(g);
      let closure = if item.is_out_of_scope() {
        graph[item.origin_state]
          .get_root_closure(g, graph.is_scan())
          .into_iter()
          .filter(|i| {
            i.is_out_of_scope() && i.get_production_id_at_sym(g) == prod_id
          })
          .map(|i| i.to_origin(item.origin))
          .collect::<Vec<_>>()
      } else {
        graph[item.origin_state]
          .get_closure(g, graph.is_scan())
          .into_iter()
          .filter(|i| {
            i.get_production_id_at_sym(g) == prod_id
              && i.goal_index == item.goal_index
          })
          .collect::<Vec<_>>()
      };

      if closure.len() > 0 {
        for item in closure.try_increment() {
          match item.get_type(g) {
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

fn handle_completed_item(
  j: &mut Journal,
  graph: &mut Graph,
  (completed_item, completed_items): (Item, Items),
  parent: StateId,
  sym: SymbolID,
  graph_state: GraphState,
) -> SherpaResult<()> {
  let is_scan = graph.is_scan();
  // Determine if origin contains GOTO.

  match (completed_item.origin, is_scan) {
    (Origin::GoalCompleteOOS, ..) => {
      let state = graph.create_state(
        sym,
        StateType::ProductionCompleteOOS,
        Some(parent),
        vec![],
      );
      graph.add_leaf_state(state);
    }
    // Completion of parse tree may be premature
    // or item is not an acceptable completed item
    (_, true) => {
      //let (follow, completed_set) = get_follow(j, graph, completed_item,
      // is_scan)?;
      let (follow, completed_items): (Vec<Items>, Vec<Items>) = completed_items
        .into_iter()
        .map(|i| get_follow(j, graph, i).unwrap())
        .unzip();
      let follow = follow.into_iter().flatten().collect::<Items>();
      let completed_items =
        completed_items.into_iter().flatten().collect::<Items>();

      let goals = get_goal_items_from_completed(completed_items, graph);

      let is_continue = !follow.is_empty();
      let is_goal = !goals.is_empty();

      let state = graph.create_state(
        sym,
        match (is_continue, goals.first().map(|d| d.origin)) {
          (true, Some(Origin::SymGoal(sym_id))) => {
            StateType::AssignAndFollow(sym_id)
          }
          (false, Some(Origin::SymGoal(sym_id))) => {
            StateType::AssignToken(sym_id)
          }
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
        StateType::Reduce(completed_item.get_rule_id()),
        Some(parent),
        vec![],
      );
      graph[state].set_reduce_item(completed_item);
      graph.add_leaf_state(state);
    }
  }

  SherpaResult::Ok(())
}

// Errors ----------------------------------------------------

fn create_reduce_reduce_error(
  j: &mut Journal,
  graph: &Graph,
  end_items: ItemSet,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());
  let goals = end_items
    .iter()
    .flat_map(|i| get_goal_items(&graph, i))
    .collect::<BTreeSet<_>>();
  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "reduce-conflict",
    msg:      "Unresolvable parse conflict encountered".into(),
    ps_msg:   {
      let mut string = "Enable the following configs to use an alternative parse strategy".into();

      if !graph.is_scan() {
      } else {
        let prod = g.get_production(&goals.first()?.get_prod_id(g))?;

        string += format!(
          "\n - Turn production <{}> into a PEG by using one of the PEG mode specifiers:",
          prod.name
        )
        .as_str();
        string += "\n    [ FIRST_MATCH | LONGEST_MATCH | SHORTEST_MATCH ]";
        string += format!("\n\n    Example: <> {} FIRST_MATCH >  ...", prod.name).as_str();
      }
      string
    },
    severity: SherpaErrorSeverity::Critical,
    sources:  end_items
      .into_iter()
      .map(|i| {
        let rule = i.get_rule(g).unwrap();
        (rule.tok.clone(), rule.g_id.path.clone(), "Found Here".into())
      })
      .collect(),
  });

  SherpaResult::Ok(())
}
