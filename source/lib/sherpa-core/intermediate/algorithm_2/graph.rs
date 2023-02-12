use regex::Match;

use crate::{
  grammar::get_production_start_items2,
  intermediate::utils::{hash_group_btreemap, symbols_occlude},
  types::{
    graph_2::{StateId, *},
    item_2::{Item, ItemContainer, ItemType},
    *,
  },
  Journal,
  SherpaResult,
};
use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};

pub(crate) fn create(
  j: &mut Journal,
  kernel_items: Vec<Item>,
  is_scanner: bool,
) -> SherpaResult<Graph> {
  let mut graph = Graph::new(j.grammar()?);

  let root = graph.create_state(SymbolID::Start, TransitionType::Start, None, kernel_items);

  graph.enqueue_pending_state(GraphState::Normal, root);

  while let Some((graph_state, parent)) = graph.dequeue_pending_state() {
    handle_kernel_items(j, &mut graph, parent, graph_state, is_scanner)?;
  }

  SherpaResult::Ok(graph)
}

fn handle_kernel_items(
  j: &mut Journal,
  graph: &mut Graph,
  parent: StateId,
  graph_state: GraphState,
  is_scanner: bool,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());
  // Get the closure for each kernel item.

  debug_assert!(parent.is_root() || !is_peg_state(graph_state));

  if parent.is_root() && is_peg_state(graph_state) {
    for item in graph[parent].get_kernel_items() {
      let transition = match graph_state {
        GraphState::FirstMatch => TransitionType::FirstMatch,
        GraphState::LongestMatch => TransitionType::LongestMatch,
        GraphState::ShortestMatch => TransitionType::ShortestMatch,
        _ => unreachable!(),
      };
      graph[parent].set_transition_type(transition);
      let state =
        graph.create_state(Default::default(), TransitionType::Start, Some(parent), vec![item]);
      graph.enqueue_pending_state(GraphState::Normal, state);
    }
  } else {
    let closure = graph[parent].get_closure(g, is_scanner);

    // Match all items together
    let mut groups = hash_group_btreemap(closure.clone(), |_, item| match item.get_type(g) {
      ItemType::Completed(_) => SymbolID::Default,
      ItemType::Terminal(sym) => sym,
      ItemType::NonTerminal(_) => SymbolID::Undefined,
      ItemType::TokenProduction(..) if is_scanner => SymbolID::Undefined,
      ItemType::TokenProduction(_, sym) => sym,
    });

    let non_terms = groups.remove(&SymbolID::Undefined);

    handle_completed_items(j, graph, parent, graph_state, &mut groups, is_scanner)?;

    let out_items =
      handle_incomplete_items(j, graph, parent, graph_state, &groups, &closure, is_scanner)?;

    if graph_state != GraphState::Peek && !is_scanner {
      add_gotos(j, graph, parent, non_terms, out_items)?;
    }
  }

  SherpaResult::Ok(())
}

fn handle_incomplete_items(
  j: &mut Journal,
  graph: &mut Graph,
  parent: StateId,
  graph_state: GraphState,
  groups: &BTreeMap<SymbolID, BTreeSet<Item>>,
  closure: &BTreeSet<Item>,
  is_scanner: bool,
) -> SherpaResult<BTreeSet<Item>> {
  let g = &(graph.grammar.clone());

  let mut out_items = BTreeSet::new();

  for (sym, group) in groups {
    let mut group = merge_occluding_groups(j, g, sym, group, &groups, is_scanner);

    match (group.len(), graph_state) {
      (1, GraphState::Peek) => {
        let kernel_items = get_kernel_items_from_peek_item(graph, *group.first()?);

        match kernel_items[0].origin {
          Origin::OutOfScope => {
            let state =
              graph.create_state(*sym, TransitionType::OutOfScopePass, Some(parent), kernel_items);
            graph.add_leaf_state(state);
          }
          _ => {
            let state = graph.create_state(*sym, TransitionType::Peek, Some(parent), kernel_items);
            graph.enqueue_pending_state(GraphState::Normal, state);
          }
        }
      }
      (2.., GraphState::Peek) => {
        let state =
          graph.create_state(*sym, TransitionType::Peek, Some(parent), group.try_increment());

        graph.enqueue_pending_state(graph_state, state);
      }
      (_, GraphState::Normal) => {
        let out_of_scope = group.drain_filter(|i| i.origin.is_out_of_scope()).collect::<Vec<_>>();
        let in_scope = group;

        match (in_scope.len(), out_of_scope.len()) {
          (0, 1..) => {
            let transition_type = match out_of_scope[0].origin {
              Origin::OutOfScope => TransitionType::OutOfScopePass,
              _ => TransitionType::OutOfScopePop,
            };
            let state = graph.create_state(*sym, transition_type, Some(parent), out_of_scope);
            graph.add_leaf_state(state);
          }
          (1.., 0..) => {
            if let Some(shifted_items) = create_call(
              &in_scope.clone().to_vec(),
              graph,
              graph_state,
              parent,
              &closure,
              *sym,
              is_scanner,
            ) {
              out_items.append(&mut shifted_items.to_set());
            } else {
              let state = graph.create_state(
                *sym,
                TransitionType::Shift,
                Some(parent),
                in_scope.try_increment(),
              );

              out_items.append(&mut graph[state].get_kernel_items().to_set());

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

fn handle_completed_items(
  j: &mut Journal,
  graph: &mut Graph,
  par: StateId,
  g_state: GraphState,
  groups: &mut BTreeMap<SymbolID, BTreeSet<Item>>,
  is_scan: bool,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());

  if let Some(completed) = groups.remove(&SymbolID::Default) {
    let num_of_completed = completed.len();
    let mut follow = Vec::new();
    for completed_item in &completed {
      let mut out: Vec<(Item, Item)> = get_follow(j, graph, *completed_item, is_scan)?
        .into_iter()
        .map(|i| (*completed_item, i))
        .collect();

      follow.append(&mut out);
    }

    // If not scanner and there exists any disambiguities, we must use either
    // a peek or breadcrumb resolution strategy to overcome this disambiguity.
    // If we are doing fork or PEG-like states then we don't have to make these
    // considerations.
    let mut completed_groups =
      hash_group_btreemap(follow.clone(), |_, (_, item)| match item.get_type(g) {
        ItemType::Completed(_) => SymbolID::Default,
        ItemType::Terminal(sym) => sym,
        _ => SymbolID::Undefined,
      });

    completed_groups.remove(&SymbolID::Undefined);

    if no_conflicts_with_completed_item(num_of_completed, &completed_groups, &groups) {
      let completed_item = *completed.first()?;

      insert_out_of_scope_items(j, graph, par, completed_item, groups, is_scan)?;

      match g_state {
        GraphState::Peek => {
          let kernel_items = get_kernel_items_from_peek_item(graph, completed_item);

          let state =
            graph.create_state(SymbolID::Default, TransitionType::Peek, Some(par), kernel_items);

          graph.enqueue_pending_state(GraphState::Normal, state);

          //Issue a default production.
        }
        GraphState::Normal => {
          handle_end_item(j, graph, completed_item, par, SymbolID::Default, g_state, is_scan)?;
        }
        _ => unreachable!("Unable to create parser for graph state {g_state:?}"),
      }
    } else {
      for (sym, items) in completed_groups {
        match (items.len(), groups.remove(&sym), g_state) {
          (1, None, GraphState::Normal) => {
            let item = items[0].0;
            handle_end_item(j, graph, item, par, sym, g_state, is_scan)?;
          }
          (_, None, GraphState::Normal)
            if completed.clone().to_set().to_cardinal().len() == 1 && !is_scan =>
          {
            handle_end_item(j, graph, *completed.first()?, par, sym, g_state, is_scan)?;
          }
          (1, None, GraphState::Peek) => {
            let kernel_items = get_kernel_items_from_peek_item(graph, items[0].0);
            let state = graph.create_state(sym, TransitionType::Peek, Some(par), kernel_items);
            graph.enqueue_pending_state(GraphState::Normal, state);
          }
          (_, Some(group), GraphState::Normal) => {
            // Create A Peek Path to handle this
            if is_scan {
              let completed = items.iter().map(|(i, _)| *i).collect::<BTreeSet<_>>();
              todo!(
                "Handle shift-reduce conflict in Scanner state. ------\n Reduce:\n{:?}\nShift:\n{:?} \n------",
                completed.to_debug_string(g, "\n"), group.to_debug_string(g, "\n")
              );
            } else {
              let group = merge_occluding_groups(j, g, &sym, &group, &groups, is_scan);
              create_peek(graph, sym, par, group, items, true);
            }
          }
          (_, None, GraphState::Normal) => {
            let completed_items = items.iter().map(|(i, _)| *i).collect::<BTreeSet<_>>();
            if is_scan {
              resolve_conflicting_symbols(j, graph, par, sym, completed_items, g_state, is_scan)?;

              /* todo!(
                "Handle reduce-reduce conflict in Scanner state. ------\n {} \n------ {}",
                completed_items.to_debug_string(g, "\n"),
                graph[0].debug_string(g)
              ) */
            } else if completed_items.iter().all(|i| i.origin.is_out_of_scope()) {
              handle_end_item(j, graph, *completed_items.first()?, par, sym, g_state, is_scan);
            } else {
              // Reduce Reduce conflict ------------------------------------------------------
              // Could pick a specific item to reduce based on precedence or some kind of exclusive
              // weight. Perhaps also walking back up the graph to find a suitable divergent point
              // add a fork. This would also be a good point to focus on breadcrumb parsing strategies.
              let end_items = items.into_iter().map(|(item, _)| item).collect::<BTreeSet<_>>();

              end_items.__print_items__(g, "reduce reduce conflicts");
              create_reduce_reduce_error(j, graph, end_items, is_scan)?;
            }
          }
          (len, collide, graph_state) => {
            unimplemented!(
              "{graph_state:?} len:{len} collide:{collide:?} sym:{} \n[ {} ]",
              sym.debug_string(g),
              items.iter().map(|(i, _)| i.debug_string(g)).collect::<Vec<_>>().join("\n")
            )
          }
        }
      }
    }
  }

  SherpaResult::Ok(())
}

fn insert_out_of_scope_items(
  j: &mut Journal,
  graph: &mut Graph,
  par: StateId,
  completed_item: Item,
  groups: &mut BTreeMap<SymbolID, BTreeSet<Item>>,
  is_scan: bool,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());

  if is_scan && graph.item_is_goal(completed_item) && !completed_item.origin.is_out_of_scope() {
    let follow = get_follow(j, graph, completed_item.to_origin(Origin::OutOfScope), is_scan)?;

    // Dumb symbols that could cause termination of parse into the intermiediate item groups
    for (sym, group) in hash_group_btreemap(
      get_closure(follow, g, par, is_scan)
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
  SherpaResult::Ok(())
}

fn resolve_conflicting_symbols(
  j: &mut Journal,
  graph: &mut Graph,
  par: StateId,
  sym: SymbolID,
  completed_items: BTreeSet<Item>,
  g_state: GraphState,
  is_scan: bool,
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
  let symbol_groups = hash_group_btreemap(completed_items.clone(), |_, i| i.origin.get_symbol());
  let priority_groups = hash_group_btreemap(symbol_groups, |_, (sym, _)| match sym {
    sym if sym.is_exclusive() => ExclusiveDefined,
    sym if sym.is_defined() => Defined,
    sym if sym.is_production() => Production,
    _ => Generic,
  });
  use SymbolPriorities::*;
  let completed: Option<&BTreeSet<Item>>;

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
          panic!("Found {} conflicting Defined symbols. Grammar is ambiguous", groups.len());
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
          panic!("Found {} conflicting Generic symbols. Grammar is ambiguous", groups.len());
        } else {
          completed = Some(groups.values().next().unwrap());
        }
      }
    }

    if let Some(completed_items) = completed {
      handle_end_item(j, graph, *completed_items.first()?, par, sym, g_state, is_scan);
      break;
    } else {
      panic!("Could not resolve Symbol ambiguities!")
    }
  }
  SherpaResult::Ok(())
}

fn add_gotos(
  j: &mut Journal,
  graph: &mut Graph,
  parent: StateId,
  non_terminals: Option<BTreeSet<Item>>,
  out_items: BTreeSet<Item>,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());

  if let Some(non_terminals) = non_terminals {
    let mut used_non_terms = BTreeSet::new();
    let mut seen = BTreeSet::new();
    let mut queue = VecDeque::from_iter(out_items.iter().map(|i| i.get_prod_id(g)));
    let mut kernel_prod_ids = graph[parent]
      .get_kernel_items()
      .into_iter()
      .map(|i| i.get_prod_id(g))
      .collect::<BTreeSet<_>>();

    while let Some(prod_id) = queue.pop_front() {
      if seen.insert(prod_id) {
        for item in non_terminals.iter().filter(|i| i.get_production_id_at_sym(g) == prod_id) {
          used_non_terms.insert(*item);
          queue.push_back(item.get_prod_id(g));
        }
      }
    }

    let goto_groups =
      hash_group_btreemap(used_non_terms.clone(), |_, t| t.get_production_id_at_sym(g));

    if !used_non_terms.is_empty() {
      for (prod_id, items) in &goto_groups {
        let should_loop = items.iter().any(|i| {
          let p = i.get_prod_id(g);
          let recursive = p == i.get_production_id_at_sym(g);
          (recursive && i.at_start()) || (!recursive && goto_groups.contains_key(&p))
        });

        let prod_sym = g.get_production(&prod_id)?.sym_id;
        let state = graph.create_state(
          prod_sym,
          if should_loop { TransitionType::GotoLoop } else { TransitionType::Goto },
          Some(parent),
          items.try_increment(),
        );

        if kernel_prod_ids.remove(&prod_id)
          && items.iter().all(|i| !i.increment().unwrap().completed())
        {
          let resolve_items = get_production_start_items2(&prod_id, g);
          if parent.is_root() {
            let mut item = resolve_items[0]
              .to_origin(Origin::GotoOutOfScope)
              .to_completed()
              .to_origin_state(parent);
            item.goal_index = OutScopeLane;
            let mut items = items.try_increment();

            items.append(&mut get_follow(j, graph, item, false)?);
            graph[state].set_kernel_items(items)
          } else {
            graph[state].set_peek_resolve_items(0, resolve_items)
          }

          if let Some(state) = graph.enqueue_pending_state(GraphState::Normal, state) {
            let vstate = graph.create_state(
              SymbolID::Default,
              TransitionType::OutOfScopePop,
              Some(state),
              vec![],
            );
            graph.add_leaf_state(vstate);
          }
        } else {
          graph.enqueue_pending_state(GraphState::Normal, state);
        }
      }

      for prod_id in kernel_prod_ids {
        let prod_sym = g.get_production(&prod_id)?.sym_id;
        let state = graph.create_state(prod_sym, TransitionType::GotoPass, Some(parent), vec![]);
        graph.add_leaf_state(state);
      }

      graph[parent].set_non_terminals(used_non_terms);
    }
  }

  SherpaResult::Ok(())
}

fn get_goal_items(graph: &Graph, item: &Item) -> Vec<Item> {
  match item.origin {
    Origin::SymGoal(_) | Origin::ProdGoal(_) => {
      vec![graph[0].get_kernel_items()[item.goal_index as usize]]
    }
    Origin::Peek(..) => get_kernel_items_from_peek_item(graph, *item)
      .iter()
      .flat_map(|i| get_goal_items(graph, item))
      .collect(),
    _ => vec![],
  }
}

fn is_peg_state(graph_state: GraphState) -> bool {
  matches!(
    graph_state,
    GraphState::LongestMatch | GraphState::FirstMatch | GraphState::ShortestMatch
  )
}

fn merge_occluding_groups(
  j: &mut Journal,
  g: &GrammarStore,
  into_sym: &SymbolID,
  into_group: &BTreeSet<Item>,
  groups: &BTreeMap<SymbolID, BTreeSet<Item>>,
  is_scanner: bool,
) -> BTreeSet<Item> {
  let mut into_group = into_group.clone();
  let g = &(j.grammar().unwrap());

  if is_scanner {
    for (from_sym, from_group) in groups.iter().filter(|(other_sym, _)| into_sym != *other_sym) {
      if symbols_occlude(into_sym, from_sym, g) {
        #[cfg(debug_assertions)]
        {
          if !j.occlusion_tracking_mode() {
            j.report_mut().add_note("Symbol Group Merge", 
            format!(
            "\nDue to the ambiguous symbols [{} ≈ {}] the peek group [\n\n{}\n\n] will be merged into [\n\n{}\n\n]\n",
            into_sym.debug_string(g),
            from_sym.debug_string(g),
            from_group.to_debug_string(g, "\n"),
            into_group.to_debug_string(g, "\n")
          ));
          }
        }
        into_group.append(&mut from_group.clone());
      }
    }
  }

  into_group
}

fn create_peek(
  graph: &mut Graph,
  sym: SymbolID,
  parent: StateId,
  incomplete_items: BTreeSet<Item>,
  completed_item_pairs: Vec<(Item, Item)>,
  need_increment: bool,
) -> Option<StateId> {
  let mut kernel_items = vec![];
  let mut resolve_items = vec![];
  let mut index = 0;
  let state = graph.create_state(sym, TransitionType::Peek, Some(parent), vec![]);

  for (end_item, items) in hash_group_btreemap(completed_item_pairs, |_, (i, _)| *i) {
    graph[state].set_peek_resolve_items(index, vec![end_item]);
    for (_, item) in items {
      kernel_items.push(item.to_origin(Origin::Peek(index, state)));
    }
    index += 1;
  }

  kernel_items.append(
    &mut incomplete_items.iter().map(|i| i.to_origin(Origin::Peek(index, state))).collect(),
  );
  resolve_items.append(&mut incomplete_items.to_vec());

  graph[state].set_peek_resolve_items(index, resolve_items);
  graph[state].set_kernel_items(if need_increment {
    kernel_items.try_increment()
  } else {
    kernel_items
  });

  let g = &(graph.grammar.clone());

  graph.enqueue_pending_state(GraphState::Peek, state)
}

fn get_kernel_items_from_peek_item(graph: &Graph, peek_item: Item) -> Vec<Item> {
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
  closure: &BTreeSet<Item>,
  sym: SymbolID,
  is_scan: bool,
) -> Option<Vec<Item>> {
  let g = &(graph.grammar.clone());
  if all_items_come_from_same_production_call(group, g) {
    let prod_id = group[0].get_prod_id(g);

    if !graph[parent].conflicting_production_call(prod_id, g, is_scan) {
      let items = closure
        .iter()
        .filter(|i| i.get_production_id_at_sym(g) == prod_id && i.get_prod_id(g) != prod_id)
        .cloned()
        .collect::<Vec<_>>();

      if items.len() > 0 {
        if let Some(state) = create_call(&items, graph, graph_state, parent, closure, sym, is_scan)
        {
          return Some(state);
        } else {
          let state =
            graph.create_state(sym, TransitionType::Call, Some(parent), items.try_increment());

          graph[state].set_call_symbol(prod_id);

          graph.enqueue_pending_state(graph_state, state);

          return Some(items.try_increment());
        }
      }
    }
  }
  None
}

fn no_conflicts_with_completed_item(
  num_of_completed: usize,
  completed_groups: &BTreeMap<SymbolID, Vec<(Item, Item)>>,
  groups: &BTreeMap<SymbolID, BTreeSet<Item>>,
) -> bool {
  num_of_completed == 1 && completed_groups.iter().all(|(sym, _)| !groups.contains_key(sym))
}

fn all_items_come_from_same_production_call(
  group: &Vec<Item>,
  g: &std::sync::Arc<GrammarStore>,
) -> bool {
  group.iter().all(|i| i.at_start())
    && group.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>().len() == 1
}

pub(crate) fn get_follow(
  _j: &mut Journal,
  graph: &Graph,
  item: Item,
  is_scan: bool,
) -> SherpaResult<Vec<Item>> {
  let mut output = BTreeSet::new();
  let g = &(graph.grammar.clone());

  if !item.completed() {
    println!("{}", item.debug_string(g));
    return SherpaResult::Ok(vec![item]);
  }

  let mut completed_items = VecDeque::from_iter(vec![item]);
  let mut seen = HashSet::new();

  while let Some(item) = completed_items.pop_front() {
    if seen.insert(item) {
      let prod_id = item.get_prod_id(g);
      let closure = if item.is_out_of_scope() {
        graph[item.origin_state]
          .get_root_closure(g, is_scan)
          .into_iter()
          .filter(|i| i.origin.is_out_of_scope() && i.get_production_id_at_sym(g) == prod_id)
          .map(|i| i.to_origin(item.origin))
          .collect::<Vec<_>>()
      } else {
        graph[item.origin_state]
          .get_closure(g, is_scan)
          .into_iter()
          .filter(|i| i.get_production_id_at_sym(g) == prod_id && i.goal_index == item.goal_index)
          .collect::<Vec<_>>()
      };

      if closure.len() > 0 {
        for item in closure.try_increment() {
          match item.get_type(g) {
            ItemType::Completed(_) => completed_items.push_back(item),
            _ => {
              output.insert(item);
            }
          }
        }
      } else if !item.origin_state.is_root() {
        let parent_state = graph[item.origin_state].get_parent();
        completed_items.push_back(item.to_origin_state(parent_state));
      } else {
        output.insert(item);
      }
    }
  }

  SherpaResult::Ok(output.to_vec())
}

fn handle_end_item(
  j: &mut Journal,
  graph: &mut Graph,
  completed_item: Item,
  parent: StateId,
  sym: SymbolID,
  graph_state: GraphState,
  is_scan: bool,
) -> SherpaResult<()> {
  // Determin if origin contains GOTO.
  let g = &(graph.grammar.clone());

  match (completed_item.origin, is_scan) {
    (Origin::GotoOutOfScope, ..) => {
      let state = graph.create_state(sym, TransitionType::OutOfScopePop, Some(parent), vec![]);
      graph.add_leaf_state(state);
    }
    // Item is out of scope
    (Origin::OutOfScope, ..) => {
      let state = graph.create_state(sym, TransitionType::OutOfScopePass, Some(parent), vec![]);
      graph.add_leaf_state(state);
    }
    // Completion of parse tree may be premature
    // or item is not an acceptable completed item
    (_, true) => {
      let follow = get_follow(j, graph, completed_item, is_scan)?;
      let is_continue = !follow.contains(&completed_item);
      let is_goal = graph.item_is_goal(completed_item);
      let state = graph.create_state(sym, TransitionType::AssignToken, Some(parent), follow);

      if is_goal {
        graph[state].set_reduce_item(completed_item);
      }

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
      let state = graph.create_state(sym, TransitionType::ReduceProduction, Some(parent), vec![]);
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
  end_items: BTreeSet<Item>,
  is_scanner: bool,
) -> SherpaResult<()> {
  let g = &(graph.grammar.clone());
  let goals = end_items.iter().flat_map(|i| get_goal_items(&graph, i)).collect::<BTreeSet<_>>();

  j.report_mut().add_error(SherpaError::SourcesError {
    id:       "reduce-conflict",
    msg:      "Unresovable parse conflict encountered".into(),
    ps_msg:   {
      let mut string = "Enable the following configs to use an alternative parse strategy".into();

      if !is_scanner {
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
        (rule.tok.clone(), rule.grammar_ref.path.clone(), "Found Here".into())
      })
      .collect(),
  });

  SherpaResult::Ok(())
}
