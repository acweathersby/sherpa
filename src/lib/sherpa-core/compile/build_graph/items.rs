use super::{
  build::{TransitionGroup, TransitionGroups},
  graph::*,
  peek::get_kernel_items_from_peek_item,
  symbols::symbols_occlude,
};
/// Returns all incomplete items that follow the given completed item,
use crate::{types::*, utils::hash_group_btreemap};
use std::collections::VecDeque;

/// and all completed items that were encountered, including the initial item.
pub(super) fn get_follow<'db>(gb: &GraphBuilder<'db>, item: Item<'db>) -> SherpaResult<(Items<'db>, Items<'db>)> {
  if !item.is_complete() {
    return SherpaResult::Ok((vec![item], vec![]));
  }

  let mut completed = OrderedSet::new();
  let mut follow = OrderedSet::new();
  let mut queue = VecDeque::from_iter(vec![item]);

  while let Some(item) = queue.pop_front() {
    if completed.insert(item) {
      let nterm = item.nonterm_index();
      let closure = if item.is_out_of_scope() {
        gb.graph()
          .get_db()
          .nonterm_follow_items(nterm)
          //graph[item.origin_state]
          //  .get_root_closure_ref()?
          //.iter()
          .filter(|i| /* i.is_out_of_scope() && */ i.nontermlike_index_at_sym().unwrap_or_default() == nterm)
          .map(|i| i.to_origin(item.origin).to_oos_index().to_origin_state(StateId::root()))
          .collect::<Array<_>>()
      } else {
        gb.graph()[item.origin_state]
          .get_closure_ref()?
          .into_iter()
          .filter(|i| i.nontermlike_index_at_sym().unwrap_or_default() == nterm && i.goal == item.goal)
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
        let parent_state = gb.graph()[item.origin_state].get_parent();
        queue.push_back(item.to_origin_state(parent_state));
      } else if !gb.is_scanner() && !item.is_out_of_scope() {
        let item: Item<'_> = item.to_oos_index();
        queue.push_back(item);
      }
    }
  }

  SherpaResult::Ok((follow.to_vec(), completed.to_vec()))
}

// Inserts out of scope sentinel items into the existing
// items groups if we are in scanner mode and the item that
// was completed belongs to the parse state goal set.
pub(super) fn get_oos_follow_from_completed<'db, 'follow>(
  gb: &GraphBuilder<'db>,
  completed_items: &Items<'db>,
  handler: &mut dyn FnMut(Items<'db>),
) -> SherpaResult<()> {
  let mut out = ItemSet::new();
  for completed_item in completed_items {
    if !completed_item.is_out_of_scope() {
      let (_, completed) = get_follow(gb, *completed_item)?;

      let goals: ItemSet = get_goal_items_from_completed(&completed, gb.graph());

      for goal in goals {
        let (follow, _) = get_follow(
          gb,
          goal
            .to_complete()
            .to_origin(if gb.is_scanner() { Origin::ScanCompleteOOS } else { Origin::GoalCompleteOOS })
            .to_oos_index(),
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

pub(super) fn get_goal_items_from_completed<'db, 'follow>(items: &Items<'db>, graph: &GraphHost<'db>) -> ItemSet<'db> {
  items.iter().filter(|i| graph.item_is_goal(*i)).cloned().collect()
}

pub(super) fn merge_items_into_groups<'db>(follow: &Vec<Item<'db>>, par: StateId, groups: &mut TransitionGroups<'db>) {
  // Dumb symbols that could cause termination of parse into the intermediate
  // item groups
  for (sym, group) in hash_group_btreemap(
    follow.iter().closure::<ItemSet>(par).into_iter().filter(|i| i.is_term()).collect::<OrderedSet<_>>(),
    |_, i| i.sym(),
  ) {
    if !groups.contains_key(&sym) {
      groups.insert(sym, (group.iter().get_max_symbol_precedence(), group));
    }
  }
}

pub(super) fn all_items_come_from_same_nonterminal_call<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db> + Clone>(group: T) -> bool {
  group.clone().all(|i| i.is_at_initial()) && group.map(|i| i.nonterm_index()).collect::<Set<_>>().len() == 1
}

pub(super) fn all_items_transition_on_same_nonterminal<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db> + Clone>(
  mut group: T,
) -> bool {
  let Some(first) = group.next() else { return false };

  let Some(nonterm) = first.nontermlike_index_at_sym() else { return false };

  group.all(|f| f.nontermlike_index_at_sym() == Some(nonterm))
}

pub(super) fn peek_items_are_from_goto_state(cmpl: &Items, graph: &GraphHost) -> bool {
  debug_assert_eq!(
    cmpl
      .iter()
      .map(|i| {
        match i.origin {
          Origin::Peek(_, origin) => origin,
          _ => unreachable!(),
        }
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

pub(super) fn get_goal_items<'db, 'follow>(iter: &GraphBuilder<'db>, item: &Item<'db>) -> Items<'db> {
  match item.origin {
    Origin::TerminalGoal(..) | Origin::NonTermGoal(_) => {
      vec![iter.graph()[0].kernel_items_ref().clone().to_vec()[item.goal as usize]]
    }
    Origin::Peek(..) => get_kernel_items_from_peek_item(iter, item).iter().cloned().collect(),
    _ => vec![],
  }
}

pub(super) fn merge_occluding_token_items<'db, 'follow>(
  from_groups: TransitionGroups<'db>,
  into_groups: &mut TransitionGroups<'db>,
) {
  for (sym, group) in into_groups.iter_mut() {
    let mut occluding_items = get_set_of_occluding_token_items(sym, group, &from_groups, group.1.first().unwrap().get_db());
    group.1.append(&mut occluding_items);
  }
}

pub(super) fn get_set_of_occluding_token_items<'db, 'follow>(
  into_sym: &SymbolId,
  into_group: &TransitionGroup<'db>,
  groups: &TransitionGroups<'db>,
  db: &ParserDatabase,
) -> ItemSet<'db> {
  let mut occluding = ItemSet::new();
  let into_group_precedence = into_group.0;

  if into_group_precedence >= 9999 {
    return occluding;
  }

  for (from_sym, from_group) in groups.iter().filter(|(other_sym, _)| into_sym != *other_sym) {
    if symbols_occlude(into_sym, from_sym, db) {
      /*     #[cfg(debug_assertions)]
      {
        _j.report_mut().add_note(
          "Symbol Group Merge",
          format!(
            "\nDue to the ambiguous symbols [{} ≈ {}] the group [\n\n{}\n\n] will be merged into [\n\n{}\n\n]\n",
            into_sym.debug_string(db),
            from_sym.debug_string(db),
            from_group.to_debug_string("\n"),
            into_group.to_debug_string("\n")
          ),
        );
      } */
      occluding.append(&mut from_group.1.clone());
    }
  }

  occluding
}
