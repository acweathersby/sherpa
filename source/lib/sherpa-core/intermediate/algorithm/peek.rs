//! Functions for resolving a set of ambiguous Items.
use super::{
  follow::{follow_items, get_follow_items},
  LR::construct_inline_LR,
};
use crate::{
  grammar::hash_id_value_u64,
  intermediate::{
    get_out_of_scope,
    utils::{hash_group_btreemap, hash_group_vec, symbols_occlude},
  },
  journal::Journal,
  types::*,
};
use std::collections::{BTreeSet, VecDeque};
use EdgeType::*;
use NodeType::*;

pub(crate) fn peek(
  t: &mut TransitionGraph,
  j: &mut Journal,
  root_par_id: NodeId,
  peek_items: Items,
  global_depth: usize,
) -> SherpaResult<()> {
  let grammar = t.g.clone();
  let g = &grammar;
  let mut peek_ids = BTreeSet::new();

  // Split items into groups based on symbol Ids.
  let goals = hash_group_vec(peek_items.clone(), |index, i| {
    if i.is_out_of_scope() {
      SymbolID::DistinctGroup(index as u32)
    } else if i.completed() || j.config().enable_breadcrumb_parsing || t.is_scan() {
      SymbolID::DistinctGroup(index as u32)
    } else {
      i.get_symbol(g)
    }
  });

  // We now create a set of item lanes based on the symbol groups that will persist throughout
  // the peeking process.
  let initial_items = goals
    .iter()
    .enumerate()
    .flat_map(|(index, vec)| {
      let item_vec = vec.iter().into_iter().map(move |i| {
        i.to_state(i.get_state().to_origin(if i.is_out_of_scope() {
          OriginData::OutOfScope(index)
        } else {
          OriginData::GoalIndex(index)
        }))
      });
      item_vec.into_iter().map(|i| i.to_local_state())
    })
    .collect::<Vec<_>>();

  // t.get_node_mut(root_par_id).goto_items = initial_items.non_term_item_vec(g);
  // With our items now setup in lanes, we can start disambiguating
  let mut pending_items = VecDeque::from_iter(vec![(root_par_id, 0, initial_items)]);

  while let Some((par_id, peek_depth, items)) = pending_items.pop_front() {
    let mut local_peek_ids = BTreeSet::new();
    let mut EXCLUSIVE_COMPLETED = false;

    // Sort the items into groups of terminal and completed items.
    let mut closure = if peek_depth == 0 {
      let in_scope_items = items.clone().inscope_items();
      let mut in_scope = in_scope_items.closure_with_state(g);
      let out_scope = items.outscope_items().closure_with_state(g);

      let checked = in_scope.as_set().to_empty_state();
      let prod_ids = in_scope_items
        .into_iter()
        .flat_map(|i| vec![i.get_prod_id(g), i.get_production_id_at_sym(g)])
        .collect::<Vec<_>>();

      for item in out_scope {
        if !checked.contains(&item.to_empty_state())
          && !prod_ids.contains(&item.get_prod_id(g))
          && (item.is_term(g) || !prod_ids.contains(&item.get_production_id_at_sym(g)))
        {
          in_scope.insert(item);
        }
      }

      in_scope
    } else {
      items.closure_with_state(g)
    };

    let have_out_of_scope = some_items_are_out_of_scope(&closure.as_vec());
    let completed_items = closure.as_vec().completed_items();

    // Resolve completed items by getting their follow set.
    if completed_items.len() > 0 {
      insert_items_into_node(completed_items.clone(), t, par_id);

      let mut maybe_accept = vec![];
      for completed_item in completed_items {
        let follow_and_terminal_completed = get_follow_items(t, &completed_item, Some(par_id));

        maybe_accept
          .append(&mut Vec::from_linked(follow_and_terminal_completed.final_completed_items));

        insert_items_into_node(
          Vec::from_linked(follow_and_terminal_completed.intermediate_completed_items),
          t,
          par_id,
        );

        let uncompleted_items = follow_and_terminal_completed.uncompleted_items;

        if !completed_item.is_out_of_scope() && have_out_of_scope && uncompleted_items.is_empty() {
          closure.append(&mut follow_items(g, completed_item, None).as_set());
        }

        closure.append(&mut Vec::from_linked(uncompleted_items).closure_with_state(g))
      }
      // Items that are truly in the completed position, that is for a completed item
      //  `I => ... *` there is no follow item `X => ... * I ...` in the closures of
      // the nodes predecessor, are then turned into completed nodes. The catch here
      // is any conflicting completed items may be unresolvable ...

      // See note OUT_OF_SCOPE handling below for commentary on what is going on with this statement.
      let items: Items = maybe_accept.iter().filter(|i| !i.is_out_of_scope()).cloned().collect();

      match items.len() {
        1 if j.occlusion_tracking_mode() => {
          // We can skip further processing if in occlusion tracking mode
        }
        0 | (1..) if all_items_are_out_of_scope(&maybe_accept) => {
          create_out_of_scope_node(t, SymbolID::EndOfInput, maybe_accept, par_id, Default);
        }
        1 => {
          let item = convert_origins(&items, &goals)[0];
          if t.is_scan() {
            let exclusive: Vec<&Item> =
              items.iter().filter(|i| i.get_origin_sym().is_exclusive()).collect();
            EXCLUSIVE_COMPLETED = exclusive.iter().any(|i| i.get_origin_sym().is_exclusive());

            #[cfg(debug_assertions)]
            {
              if EXCLUSIVE_COMPLETED {
                j.report_mut().add_note("Exclusive short-circuit", format!(
                  "Short circuiting completion of other items due to one or more exclusive symbols being completed: [\n{}\n]",
                  exclusive.iter().map(|i| format!("    {{ {} => {} }}", 
                  i.debug_string(g), i.get_origin_sym().to_string(g))).collect::<Vec<_>>().join("\n")
              ));
              }
            }
          }

          if t.is_scan() && t.item_is_goal(item) {
            //dump out of scope items into the outgoing stream.
            let out_of_scope =
              get_out_of_scope(t, g, item.get_prod_id(g), &closure.clone().to_set())
                .incomplete_items()
                .closure(g)
                .to_state(
                  items[0]
                    .get_state()
                    .to_origin(OriginData::OutOfScope(items[0].get_origin_index())),
                );

            closure = closure.merge_unique(out_of_scope);
          }

          insert_items_into_node(items.clone(), t, par_id);

          if peek_depth == 0 {
            t.queue_node(ProcessGroup {
              node_index:   par_id,
              items:        get_goal_items(&items, &goals),
              discriminant: Some((SymbolID::Default, items)),
              depth:        global_depth,
            });
          } else if t.is_scan() || j.config().enable_breadcrumb_parsing {
            let node_index = create_and_insert_node(
              t,
              SymbolID::EndOfInput,
              vec![],
              if t.is_scan() { Complete } else { BreadcrumbEndCompletion },
              Default,
              Some(par_id),
              Some(par_id),
              vec![],
            );

            let origin = get_goal_origin(&items, &goals);

            // Submit these items to be processed.
            t.queue_node(ProcessGroup {
              node_index,
              items: items.into_iter().map(|i| i.to_origin(origin)).collect(),
              discriminant: None,
              depth: global_depth,
            });
          } else {
            let node_index = create_and_insert_node(
              t,
              SymbolID::EndOfInput,
              vec![],
              PeekTransition,
              Default,
              Some(par_id),
              Some(root_par_id),
              Vec::default(),
            );

            // Submit these items to be processed.
            t.queue_node(ProcessGroup {
              node_index,
              items: get_goal_items(&items, &goals),
              discriminant: None,
              depth: global_depth,
            });
          }
        }
        (2..) if t.is_scan() => {
          resolveConflictingSymbols(
            t,
            j,
            items
              .into_iter()
              .map(|i| match i.get_origin() {
                OriginData::GoalIndex(index) => i.to_origin(goals[index][0].get_origin()),
                _ => unreachable!(),
              })
              .collect(),
            peek_depth,
            global_depth,
            par_id,
          );
        }
        2.. => {
          items.__print_items__(g, "Conflicting items");
          t.accept_items.__print_items__(g, "GOALS");
          return SherpaResult::Err(SherpaError::grammar_err_multi_location {
            message:   "Could not resolve production. Grammar has ambiguities.".to_string(),
            locations: items
              .iter()
              .map(|i| SherpaError::grammar_err {
                message: "Test".to_string(),
                inline_message: "Test".to_string(),
                loc: match i.get_origin() {
                  OriginData::RuleId(rule_id) => t.g.get_rule(&rule_id).unwrap().tok.clone(),
                  _ => i.decrement().unwrap().get_rule_ref(g).unwrap().tok.clone(),
                },
                path: i.decrement().unwrap().get_rule_ref(g).unwrap().grammar_ref.path.clone(),
              })
              .collect(),
          });
        }
        _ => {}
      }
    }

    insert_items_into_node(convert_origins(&closure.as_vec(), &goals), t, par_id);

    if EXCLUSIVE_COMPLETED {
      continue;
    }

    // Create groups of terminal items based on their symbols
    let mut groups = hash_group_btreemap(closure.as_vec().term_items(g), |_, i| i.get_symbol(g))
      .into_iter()
      .collect::<Vec<_>>();

    // Merge groups whose symbols occlude.
    merge_occluding_token_groups(t, j, &mut groups);

    for (sym, mixed_items) in groups {
      // detect the number of distinct lanes present in the current group.
      // IF this value is 1, then we have successfully found a peek leaf that
      // is ambiguous

      // ---[OUT_OF_SCOPE handling]---------------------------------------------------------
      // Here we isolate out of scope items from our normal parse items. This leads to
      // to the transition to out-of-scope ( and thus failed states ) only occurring on
      // the first level of peek, making our RA algorithm `k=1`, at least when out of
      // scope symbols are concerned. It is feasible to maintain ambiguous states that
      // have a mixture of in-scope and out-of-scope items for more levels of peeking,
      // allowing for a high `k` value however this not the approach taken ATM. This
      // may change in the future.

      let _some_items_are_out_of_scope = some_items_are_out_of_scope(&mixed_items);

      let items = if peek_depth > 10 {
        mixed_items.iter().filter(|i| !i.is_out_of_scope()).cloned().collect()
      } else {
        if _some_items_are_out_of_scope && peek_depth == 0 {
          let group = hash_group_vec(mixed_items.clone(), |_, i| i.to_empty_state());
          if (group.len() == 1) {
            mixed_items.iter().filter(|i| !i.is_out_of_scope()).cloned().collect()
          } else {
            mixed_items.clone()
          }
        } else {
          mixed_items.clone()
        }
      };
      // -----------------------------------------------------------------------------------

      let peek_groups = hash_group_vec(items.clone(), |_, i| i.get_state().get_origin());

      match peek_groups.len() {
        1 if j.occlusion_tracking_mode() => {
          // We can skip further processing if in occlusions tracking mode
        }
        1.. if t.is_scan() && _some_items_are_out_of_scope => {
          let index = create_and_insert_node(
            t,
            sym,
            mixed_items,
            Pass,
            Assert,
            Some(par_id),
            Some(par_id),
            vec![],
          );

          t.leaf_nodes.push(index);
        }
        0 | (1..) if all_items_are_out_of_scope(&mixed_items) => {
          create_out_of_scope_node(t, sym, mixed_items, par_id, get_edge_type(j, t, peek_depth));
        }
        1 if peek_depth == 0 => {
          // Reprocess the root node (which is always par_id when depth == 0)
          // with the disambiguated items.

          t.queue_node(ProcessGroup {
            node_index:   par_id,
            items:        get_goal_items(&items, &goals),
            discriminant: Some((sym, items)),
            depth:        global_depth,
          });
        }
        1 if t.is_scan() || j.config().enable_breadcrumb_parsing => {
          if !j.config().enable_breadcrumb_parsing && t.is_scan() {
            // Check to see if we can issue a call instead of increment.
            // For that to work, all items need to be in an initial state,
            // and the production .
            if convert_to_production_call(
              t,
              j,
              par_id,
              &items,
              sym,
              &goals,
              peek_depth,
              global_depth,
            )? {
              continue;
            }
          }

          let origin = get_goal_origin(&items, &goals);
          let items: Items =
            items.try_increment().into_iter().map(|i| i.to_origin(origin)).collect();
          let node_index = create_and_insert_node(
            t,
            sym,
            vec![],
            if t.is_scan() { Shift } else { BreadcrumbShiftCompletion },
            Assert,
            Some(par_id),
            Some(par_id),
            items.as_vec().non_term_items(g),
          );
          // Continue processing the now disambiguated items.
          t.queue_node(ProcessGroup { node_index, items, discriminant: None, depth: global_depth });
        }
        1 => {
          let goal_items = get_goal_items(&items, &goals);
          let node_index = create_and_insert_node(
            t,
            sym,
            vec![],
            PeekTransition,
            get_edge_type(j, t, peek_depth),
            Some(par_id),
            Some(root_par_id),
            goal_items.as_vec().non_term_items(g),
          );
          // Submit these items to be processed.
          t.queue_node(ProcessGroup {
            node_index,
            items: goal_items.clone(),
            discriminant: None,
            depth: global_depth,
          });
        }

        2.. => {
          // We combine these items into a new node, then prepare their increments
          // for the next round.

          // TODO: We need to evaluate whether we can continue processing nodes.
          // The condition in which we can't continue are:
          // - Shift-Reduce conflicts
          let hash_id = hash_id_value_u64(items.clone().to_empty_state().to_set());
          local_peek_ids.insert(hash_id);
          if !t.is_scan() && /* t. */peek_ids.contains(&hash_id) {
            println!(
              "{} {} {}",
              hash_id_value_u64(items.clone().to_empty_state().to_set()),
              global_depth,
              peek_depth
            );
            t.accept_items.__print_items__(g, "starts");
            items.__print_items__(g, &format!("peeks {}", peek_depth));
            // Item set has been repeated
            let lr_starts = get_goal_items(&items, &goals);
            //let lr_starts = goal_items.clone().into_iter().flatten().cloned().collect::<Vec<_>>();

            // We can try to disambiguating using LR parsing:

            // create the root node for the start items
            let root_node = create_node(
              t,
              sym,
              lr_starts.clone(),
              get_node_type(j, t),
              get_edge_type(j, t, peek_depth),
              Some(par_id),
              Some(par_id),
              lr_starts.clone().non_term_items(g),
            );

            match construct_inline_LR(t, j, root_node) {
              SherpaResult::Ok(_) => {
                // Our grammar is now (RD/RAD + LR)
              }
              err if !t.is_scan() => {
                let mut start_node = par_id;
                let mut offset = peek_depth as i32;

                /*  while offset >= 2 {
                  start_node = t.get_node(start_node).parent?;
                  offset -= 1;
                } */

                lr_starts.__print_items__(g, "FAILED PEEKS");

                // Trace peeks back to roots.

                //#[cfg(debug_assertions)]
                //eprintln!("{:?}", err);

                // Otherwise, we must use a fork state to handle this situation
                let fork_node_index = create_and_insert_node(
                  t,
                  sym,
                  lr_starts.clone(),
                  Fork,
                  get_edge_type(j, t, peek_depth),
                  Some(par_id),
                  Some(par_id),
                  lr_starts.as_vec().non_term_items(g),
                );

                for goal_item in lr_starts.iter() {
                  let fork_base = create_and_insert_node(
                    t,
                    sym,
                    lr_starts.clone(),
                    ForkBase,
                    Default,
                    Some(fork_node_index),
                    Some(fork_node_index),
                    lr_starts.as_vec().non_term_items(g),
                  );

                  t.queue_node(ProcessGroup {
                    node_index:   fork_base,
                    items:        vec![*goal_item],
                    discriminant: None,
                    depth:        global_depth,
                  });
                }

                // Our grammar is now G + (RD/RAD) + LR?
              }
              _ => {
                panic!("Unable to resolve this grammar!");
              }
            }
          } else {
            if !j.config().enable_breadcrumb_parsing && (t.is_scan() || global_depth == 0) {
              // Check to see if we can issue a call instead of increment.
              // For that to work, all items need to be in an initial state,
              // and the production .
              if convert_to_production_call(
                t,
                j,
                par_id,
                &items,
                sym,
                &goals,
                peek_depth,
                global_depth,
              )? {
                continue;
              }
            }

            // Pack items into new peek node and submit their increments for
            // a new peek round
            let incremented_items = items.try_increment();

            let node_index = create_and_insert_node(
              t,
              sym,
              vec![],
              get_node_type(j, t),
              get_edge_type(j, t, peek_depth),
              Some(par_id),
              Some(par_id),
              incremented_items.as_vec().non_term_items(g),
            );

            pending_items.push_back((node_index, peek_depth + 1, incremented_items));
          }
        }
        _ => {
          unreachable!("Groups should have at least one lane")
        }
      }
    }

    peek_ids.append(&mut local_peek_ids);
  }

  SherpaResult::Ok(())
}

fn create_out_of_scope_node(
  t: &mut TransitionGraph,
  sym: SymbolID,
  mixed_items: Vec<Item>,
  par_id: NodeId,
  edge_type: EdgeType,
) {
  // This symbol belongs to a follow item of the production. In this
  // we simply fail to allow the production to complete using the fall
  // back function
  let index = create_and_insert_node(
    t,
    sym,
    mixed_items,
    if t.is_scan() { OutOfScopeComplete } else { Fail },
    edge_type,
    Some(par_id),
    Some(par_id),
    vec![],
  );
  t.leaf_nodes.push(index);
}

fn convert_to_production_call(
  t: &mut TransitionGraph,
  j: &Journal,
  par_id: NodeId,
  items: &Items,
  sym: SymbolID,
  goals: &[Items],
  depth: usize,
  global_depth: usize,
) -> SherpaResult<bool> {
  let g = &t.g.clone();

  let mut call_groups = hash_group_btreemap(items.clone(), |_, i| {
    let origin = convert_origin(i, &goals);

    if origin.at_start()
      && (depth > 0 || !t.accept_items.contains(&origin.to_completed().to_origin_only_state()))
    {
      Some(i.get_prod_id(g))
    } else {
      None
    }
  })
  .into_iter()
  .collect::<Vec<_>>();

  match (call_groups.len(), call_groups.pop()) {
    (1, Some((Some(_), items))) => {
      let mut continue_items = Items::new();

      for (_, items) in hash_group_btreemap(items.clone(), |_, i| i.get_origin()) {
        let results =
          get_follow_items(t, &convert_origin(&items[0], &goals).to_completed(), Some(par_id));
        continue_items.append(&mut results.get_all_items());
      }

      let production_sym = continue_items[0].decrement()?.get_symbol(g);

      let node_index = create_and_insert_node(
        t,
        sym,
        items.clone(),
        NodeType::ProductionCall,
        get_edge_type(j, t, depth),
        Some(par_id),
        Some(par_id),
        continue_items.as_vec().non_term_items(g),
      );
      t.get_node_mut(node_index).prod_sym = Some(production_sym);

      t.queue_node(ProcessGroup {
        node_index,
        items: continue_items.clone(),
        discriminant: None,
        depth: global_depth,
      });

      // End of the line - Let goto handle the reset.
      // Submit these items to be processed.

      if !t.is_scan() {
        t.goto_seeds.append(&mut items.non_term_items(g).to_empty_state().to_set());
      }

      SherpaResult::Ok(true)
    }
    _ => SherpaResult::Ok(false),
  }
}

fn convert_origins(pending: &Vec<Item>, goals: &Vec<Items>) -> Vec<Item> {
  pending.into_iter().map(|item| convert_origin(item, goals)).collect()
}

fn convert_origin(item: &Item, goals: &[Items]) -> Item {
  match item.get_origin() {
    OriginData::GoalIndex(index) | OriginData::OutOfScope(index) => {
      item.to_origin(goals[index][0].get_state().get_origin())
    }
    _ => {
      panic!("Should only have items with Goal Indices in this context! {:?}", item)
    }
  }
}

fn all_items_are_out_of_scope(items: &Vec<Item>) -> bool {
  items.iter().all(|i| i.is_out_of_scope())
}

fn some_items_are_out_of_scope(items: &Vec<Item>) -> bool {
  items.iter().any(|i| i.is_out_of_scope())
}

#[inline]
fn get_goal_items(items: &Vec<Item>, goals: &[Items]) -> Vec<Item> {
  get_goal_contents(items, goals).into_iter().flatten().cloned().collect()
}

#[inline]
fn get_goal_origin(items: &Vec<Item>, goals: &[Items]) -> OriginData {
  get_goal_contents(items, goals).into_iter().flatten().next().unwrap().get_origin()
}

fn get_node_type(j: &Journal, t: &TransitionGraph) -> NodeType {
  if t.is_scan() {
    NodeType::Shift
  } else if j.config().enable_breadcrumb_parsing {
    NodeType::BreadcrumbTransition
  } else {
    NodeType::PeekTransition
  }
}

fn get_edge_type(j: &Journal, t: &TransitionGraph, depth: usize) -> EdgeType {
  if depth > 0 && !t.is_scan() && !j.config().enable_breadcrumb_parsing {
    EdgeType::Peek
  } else {
    EdgeType::Assert
  }
}

fn resolveConflictingSymbols(
  t: &mut TransitionGraph,
  j: &mut Journal,
  completed_symbol_items: Items,
  peek_depth: usize,
  global_depth: usize,
  par_id: NodeId,
) {
  #[cfg(debug_assertions)]
  {
    if !j.occlusion_tracking_mode() {
      // Ensure all items have symbol origins
      debug_assert!(completed_symbol_items.iter().all(|i| {
        match i.get_origin() {
          OriginData::Symbol(_) => true,
          _ => false,
        }
      }));
      #[cfg(follow_tracking)]
      {
        eprintln!(
          "\nScan Mode: Conflicting items and their symbols:\n{}\n",
          completed_symbol_items
            .iter()
            .map(|i| match i.get_origin() {
              OriginData::Symbol(sym) =>
                format!("{{ {} => {} }}", i.debug_string(&t.g), sym.to_string(&t.g)),
              _ => String::new(),
            })
            .collect::<Vec<_>>()
            .join("\n")
        );
      }
    }
  }

  if j.occlusion_tracking_mode() {
    let symbols =
      completed_symbol_items.iter().map(|i| i.get_origin_sym()).collect::<BTreeSet<_>>();
    j.add_occlusions(symbols);
  } else {
    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
    enum SymbolPriorities {
      ExclusiveDefined,
      Defined,
      Production,
      Generic,
    }
    use SymbolPriorities::*;

    // Map items according to their symbols
    let symbol_groups =
      hash_group_btreemap(completed_symbol_items.clone(), |_, i| i.get_origin_sym());
    let priority_groups = hash_group_btreemap(symbol_groups, |_, (sym, _)| match sym {
      sym if sym.is_exclusive() => ExclusiveDefined,
      sym if sym.is_defined() => Defined,
      sym if sym.is_production() => Production,
      _ => Generic,
    });
    let complete_items: Option<&Items>;

    for (priority, groups) in priority_groups {
      match priority {
        ExclusiveDefined => {
          if groups.len() > 1 {
            panic!(
              "Found {} conflicting Exclusive Defined symbols. Grammar is ambiguous",
              groups.len()
            );
          } else {
            complete_items = Some(groups.values().next().unwrap());
          }
        }
        Defined => {
          if groups.len() > 1 {
            panic!("Found {} conflicting Defined symbols. Grammar is ambiguous", groups.len());
          } else {
            complete_items = Some(groups.values().next().unwrap());
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
                    t.g.get_production(prod_id).unwrap().loc.blame(
                      1,
                      1,
                      &format!("[ {} ] first defined here", s.to_string(&t.g)),
                      BlameColor::RED,
                    )
                  }
                  _ => String::new(),
                })
                .collect::<Vec<_>>()
                .join("\n")
            );
          } else {
            complete_items = Some(groups.values().next().unwrap());
          }
        }
        Generic => {
          if groups.len() > 1 {
            panic!("Found {} conflicting Generic symbols. Grammar is ambiguous", groups.len());
          } else {
            complete_items = Some(groups.values().next().unwrap());
          }
        }
      }

      if let Some(completed_items) = complete_items {
        if peek_depth == 0 {
          t.queue_node(ProcessGroup {
            node_index:   par_id,
            items:        completed_items.clone()[0..1].to_vec(),
            discriminant: Some((SymbolID::Default, completed_items.clone())),
            depth:        global_depth,
          });
        } else {
          let node_index = create_and_insert_node(
            t,
            SymbolID::EndOfInput,
            vec![],
            Complete,
            Default,
            Some(par_id),
            Some(par_id),
            Vec::default(),
          );
          // Submit these items to be processed.
          t.queue_node(ProcessGroup {
            node_index,
            items: completed_items.clone(),
            discriminant: None,
            depth: global_depth,
          });
        }
        return;
      } else {
        panic!("Could not resolve Symbol ambiguities!")
      }
    }
  }
}

fn get_goal_contents<'a>(items: &Items, goals: &'a [Items]) -> Vec<&'a Items> {
  hash_group_btreemap(items.clone(), |_, i| match i.get_origin() {
    OriginData::GoalIndex(index) | OriginData::OutOfScope(index) => index,
    _ => panic!("Should only have items with Goal Indices in this context!"),
  })
  .into_iter()
  .map(|(g, _)| {
    let goal_items = &goals[g];
    goal_items
  })
  .collect()
}

pub(super) fn insert_items_into_node(items: Items, t: &mut TransitionGraph, node_id: NodeId) {
  // par_node.transition_items.append(&mut items.to_origin_only_state());
  // let reduced = par_node.transition_items.clone().to_origin_only_state().to_set().to_vec();
  t.get_node_mut(node_id).transition_items.append(&mut items.clone());
  let mut nonterm_items = items.non_term_items(&t.g);
  t.get_node_mut(node_id).goto_items.append(&mut nonterm_items);
}

/// Compares the terminal symbols of node groups and merges those
/// groups whose terminal symbols occlude each other.
///
/// For instance, given a group `A` with the symbol `g:id` and an
/// other group `B` with symbol `\g`, the character `g` could be
/// accepted by either group. As long as group `A` (the "defined"
/// group) is not exclusive, we merge group `B` into `A` to into
/// account the ambiguous nature of the groups.

fn merge_occluding_token_groups(
  t: &mut TransitionGraph,
  journal: &mut Journal,
  groups: &mut [(SymbolID, Items)],
) {
  // Clone the from_group store so we are able
  // to merge its members into to_groups without
  // going fowl of the borrow checker.

  if !t.is_scan() && !journal.config().allow_occluding_symbols {
    return;
  }

  for i in 0..groups.len() {
    for j in 0..groups.len() {
      if i == j {
        continue;
      }

      let (from_sym, from_group) = &groups[i];
      let (to_sym, to_group) = &groups[j];

      let from_item = from_group[0];
      let to_item = to_group[0];

      let from_origin = from_item.get_origin();
      let to_origin = to_item.get_origin();

      // Scanner items that originate from the same symbol do not require occlusion
      // checking.
      if matches!(from_origin, OriginData::Symbol(..)) && from_origin == to_origin {
        continue;
      }

      if {
        if t.is_scan() {
          symbols_occlude(&to_sym, &from_sym, &t.g)
            && ((!from_item.is_out_of_scope()) || (from_sym.is_defined() || to_sym.is_defined()))
        } else {
          let occlusion_table = journal.get_occlusion_table();

          occlusion_table.get(to_sym).and_then(|f| f.contains(from_sym).then(|| 1)).is_some()
        }
      } {
        #[cfg(debug_assertions)]
        {
          if !journal.occlusion_tracking_mode() {
            journal.report_mut().add_note("Symbol Group Merge", 
            format!(
            "\nDue to the ambiguous symbols [{} ≈ {}] the peek group [\n\n{}\n\n] will be merged into [\n\n{}\n\n]\n",
            to_sym.to_string(&t.g),
            from_sym.to_string(&t.g),
            from_group.to_debug_string(&t.g, "\n"),
            groups[j].1.to_debug_string(&t.g, "\n")
          ));
          }
        }
        let mut clone = from_group.clone();
        groups[j].1.append(&mut clone);
      }
    }
  }
}

pub(super) fn create_node(
  t: &TransitionGraph,
  symbol: SymbolID,
  items: Items,
  node_type: NodeType,
  edge_type: EdgeType,
  parent: MaybeNodeId,
  closure_parent: MaybeNodeId,
  goto_items: Items,
) -> GraphNode {
  let mut node = GraphNode::new(t, symbol, parent, items, node_type);

  node.edge_type = edge_type;
  node.closure_parent = closure_parent;
  node.goto_items = goto_items.non_term_items(&t.g);

  node
}

pub(super) fn create_and_insert_node(
  t: &mut TransitionGraph,
  symbol: SymbolID,
  items: Items,
  node_type: NodeType,
  edge_type: EdgeType,
  parent: MaybeNodeId,
  closure_parent: MaybeNodeId,
  goto_items: Items,
) -> NodeId {
  let node =
    create_node(t, symbol, items, node_type, edge_type, parent, closure_parent, goto_items);
  t.insert_node(node)
}
