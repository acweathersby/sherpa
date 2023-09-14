use super::{
  build::{GroupedFirsts, TransitionGroup},
  graph::*,
};
use crate::types::*;
use sherpa_rust_runtime::utf8::{get_token_class_from_codepoint, lookup_table::CodePointClass};
use std::collections::VecDeque;

/// Returns all terminals that follow the item. This is equivalent to extracting
/// the terminals from the closure of the item, including completed items, in
/// which the closure of all non-terminal items that transition on the item is
/// taken after performing the non-term shift
pub(crate) fn get_follow_symbols<'a, 'db: 'a>(
  gb: &'a mut GraphBuilder<'db>,
  item: Item<'db>,
) -> impl Iterator<Item = DBTermKey> + 'a {
  get_follow_internal(gb, item, false)
    .0
    .into_iter()
    .flat_map(|i| i.closure_iter().filter_map(|i| i.term_index_at_sym(gb.get_mode())))
}

/// Returns a tuple comprised of a vector of all items that follow the given
/// item, provided the given item is in a complete state, and a list of a all
/// items that are completed from directly or indirectly transitioning on the
/// nonterminal of the given item.
pub(crate) fn get_follow<'db>(
  gb: &mut GraphBuilder<'db>,
  item: Item<'db>,
  single_reduction_only: bool,
) -> (Items<'db>, Items<'db>) {
  get_follow_internal(gb, item, single_reduction_only)
}

/// Returns a tuple comprised of a vector of all items that follow the given
/// item, provided the given item is in a complete state, and a list of a all
/// items that are completed from directly or indirectly transitioning on the
/// nonterminal of the given item.
pub(crate) fn get_follow_internal<'db>(
  gb: &mut GraphBuilder<'db>,
  item: Item<'db>,
  single_reduction_only: bool,
) -> (Items<'db>, Items<'db>) {
  if !item.is_complete() {
    return (vec![item], vec![]);
  }

  let ____is_scan____ = gb.graph().graph_type == GraphType::Scanner;
  let mut completed = OrderedSet::new();
  let mut follow = OrderedSet::new();
  let mut queue = VecDeque::from_iter(vec![item]);
  let mode = gb.graph().graph_type;
  let root_nterm = item.nonterm_index();

  while let Some(c_item) = queue.pop_front() {
    if completed.insert(c_item) {
      let nterm: DBNonTermKey = c_item.nonterm_index();

      if single_reduction_only && nterm != root_nterm {
        continue;
      }

      let result = if c_item.origin_state.is_oos() {
        if c_item.origin_state.is_oos_root() {
          // Create or retrieve a new OOS state for this set of items.
          let state_id = gb.get_oos_root_state(nterm);
          let state = gb.get_state(state_id);
          let closure = state.get_kernel_items().iter().cloned();
          process_closure(closure, &mut queue, &mut follow)
        } else {
          let state = gb.get_state(c_item.origin_state);
          let closure = state
            .get_kernel_items()
            .iter()
            .filter(|i| i.nonterm_index_at_sym(mode) == Some(nterm))
            .cloned()
            .filter_map(|i| i.increment());
          process_closure(closure, &mut queue, &mut follow)
        }
      } else {
        let origin_state_id = c_item.origin_state;
        let origin = c_item.origin;
        let goal = c_item.goal;
        let state = gb.get_state(c_item.origin_state);
        let closure = state
          .get_kernel_items()
          .iter()
          .filter(|kernel| kernel.goal == goal)
          .flat_map(|kernel| kernel.closure_iter_align(kernel.to_origin_state(origin_state_id).to_origin(origin)))
          .filter(|i| i.nonterm_index_at_sym(mode) == Some(nterm))
          .filter_map(|i| i.increment());
        process_closure(closure, &mut queue, &mut follow)
      };

      if let Some(oos_queue) = result {
        for next_item in oos_queue {
          gb.get_oos_closure_state(next_item);
          follow.insert(next_item);
        }
      } else {
        if !c_item.origin_state.is_root() && !c_item.origin_state.is_oos_root() {
          let parent_state = gb.get_state(c_item.origin_state).get_parent();
          queue.push_back(c_item.to_origin_state(parent_state));
        } else if !____is_scan____ && !c_item.origin_state.is_oos_root() {
          queue.push_back(c_item.to_origin_state(StateId::oos_root()));
        }
      }
    }
  }

  (follow.to_vec(), completed.to_vec())
}

fn process_closure<'db>(
  closure: impl Iterator<Item = Item<'db>>,
  queue: &mut VecDeque<Item<'db>>,
  follow: &mut OrderedSet<Item<'db>>,
) -> Option<VecDeque<Item<'db>>> {
  let mut oos_queue = VecDeque::new();
  let mut closure_yielded_items = false;

  for item in closure {
    closure_yielded_items |= true;
    match item.get_type() {
      ItemType::Completed(_) => queue.push_back(item),
      _ => {
        if item.is_out_of_scope() {
          oos_queue.push_front(item);
        } else {
          follow.insert(item);
        };
      }
    }
  }

  closure_yielded_items.then_some(oos_queue)
}

pub(crate) fn get_completed_item_artifacts<'a, 'db: 'a, 'follow, T: ItemRefContainerIter<'a, 'db>>(
  gb: &mut GraphBuilder<'db>,
  completed: T,
) -> SherpaResult<CompletedItemArtifacts<'db>> {
  let mut follow_pairs = OrderedSet::new();
  let mut default_only_items = ItemSet::new();

  fn create_pair<'db>(k_i: Item<'db>, i: Item<'db>) -> TransitionPair<'db> {
    TransitionPair { kernel: k_i, next: i, prec: i.token_precedence(), sym: i.sym() }
  }

  for k_i in completed {
    let (f, _) = get_follow(gb, *k_i, false);

    if f.is_empty() {
      default_only_items.insert(*k_i);
    } else {
      for k_follow in f {
        if let Origin::__OOS_CLOSURE__ = k_follow.origin {
          let state = gb.get_oos_closure_state(k_follow);
          follow_pairs
            .extend(gb.get_state(state).get_kernel_items().iter().filter(|i| !i.is_complete()).map(|i| create_pair(*k_i, *i)));
        } else {
          follow_pairs.extend(
            k_follow
              .closure_iter_align(k_follow.to_origin_state(gb.current_state_id()).to_origin(k_follow.origin))
              .map(|i| create_pair(*k_i, i)),
          )
        }
      }
    }
  }

  SherpaResult::Ok(CompletedItemArtifacts { follow_pairs, default_only: default_only_items })
}

// Inserts out of scope sentinel items into the existing
// items groups if we are in scanner mode and the item that
// was completed belongs to the parse state goal set.
pub(super) fn _get_oos_follow_from_completed<'db>(
  gb: &mut GraphBuilder<'db>,
  completed_items: &Items<'db>,
  handler: &mut dyn FnMut(Follows<'db>),
) -> SherpaResult<()> {
  let mut out = OrderedSet::new();
  for completed_item in completed_items {
    if !completed_item.is_out_of_scope() {
      let (_, completed) = get_follow(gb, *completed_item, false);

      let goals: ItemSet = get_goal_items_from_completed(&completed, gb.graph());

      for goal in goals {
        let (follow, _) = get_follow(
          gb,
          goal
            .to_complete()
            .to_origin(if gb.is_scanner() { Origin::ScanCompleteOOS } else { Origin::GoalCompleteOOS })
            .to_oos_index(),
          false,
        );
        out.extend(&mut follow.into_iter().map(|f| -> Follow { (*completed_item, f, gb.get_mode()).into() }));
      }
    }
  }
  if !out.is_empty() {
    handler(out.into_iter().collect());
  }
  SherpaResult::Ok(())
}

pub(super) fn get_goal_items_from_completed<'db, 'follow>(items: &Items<'db>, graph: &GraphHost<'db>) -> ItemSet<'db> {
  items.iter().filter(|i| graph.item_is_goal(*i)).cloned().collect()
}

pub(super) fn _merge_follow_items_into_group<'db>(
  follows: &Vec<Follow<'db>>,
  _par: StateId,
  firsts_groups: &mut GroupedFirsts<'db>,
) {
  // Dumb symbols that could cause termination of parse into the intermediate
  // item groups

  for follow in follows {
    if !firsts_groups.contains_key(&follow.sym) {
      firsts_groups.insert(follow.sym, (follow.prec, vec![*follow]));
    }
  }
}

pub(super) fn merge_occluding_token_items<'db>(from_groups: GroupedFirsts<'db>, into_groups: &mut GroupedFirsts<'db>) {
  for (sym, group) in into_groups.iter_mut() {
    let occluding_items = get_set_of_occluding_token_items(sym, group, &from_groups);
    group.1.extend(occluding_items);
  }
}

pub(super) fn get_set_of_occluding_token_items<'db>(
  into_sym: &SymbolId,
  into_group: &TransitionGroup<'db>,
  groups: &GroupedFirsts<'db>,
) -> Firsts<'db> {
  let mut occluding = Firsts::new();
  let into_prec = into_group.0;

  if into_prec >= 9999 {
    return occluding;
  }

  for (from_sym, from_group) in groups.iter().filter(|(other_sym, (prec, _))| into_sym != *other_sym && into_prec <= *prec) {
    if symbols_occlude(into_sym, from_sym) {
      occluding.extend(from_group.1.iter().cloned());
    }
  }

  occluding
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
fn symbols_occlude(symA: &SymbolId, symB: &SymbolId) -> bool {
  match symA {
    SymbolId::Char { char, .. } => match symB {
      SymbolId::ClassNumber { .. } => {
        (*char < 128) && get_token_class_from_codepoint(*char as u32) == CodePointClass::Number as u32
      }
      SymbolId::ClassIdentifier { .. } => {
        (*char < 128) && get_token_class_from_codepoint(*char as u32) == CodePointClass::Identifier as u32
      }
      SymbolId::ClassSymbol { .. } => {
        (*char < 128) && get_token_class_from_codepoint(*char as u32) == CodePointClass::Symbol as u32
      }
      SymbolId::Default => false,
      symB => *symA == *symB,
    },
    SymbolId::Codepoint { val, .. } => match symB {
      SymbolId::ClassNumber { .. } => get_token_class_from_codepoint(*val) == CodePointClass::Number as u32,
      SymbolId::ClassIdentifier { .. } => get_token_class_from_codepoint(*val) == CodePointClass::Identifier as u32,
      SymbolId::ClassSymbol { .. } => get_token_class_from_codepoint(*val) == CodePointClass::Symbol as u32,
      SymbolId::Default => false,
      symB => *symA == *symB,
    },
    SymbolId::Default => false,
    symA => *symA == *symB,
  }
}
