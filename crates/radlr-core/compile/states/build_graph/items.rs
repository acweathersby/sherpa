use super::{
  build::{GroupedFirsts, TransitionGroup},
  graph::*,
};
use crate::{
  compile::states::build_graph::graph::{GraphType, Origin, StateId},
  types::*,
};
use radlr_rust_runtime::utf8::{get_token_class_from_codepoint, lookup_table::CodePointClass};
use std::collections::VecDeque;

/// Returns a tuple comprised of a vector of all items that follow the given
/// item, provided the given item is in a complete state, and a list of a all
/// items that are completed from directly or indirectly transitioning on the
/// nonterminal of the given item.
pub(crate) fn get_follow(gb: &mut ConcurrentGraphBuilder, node: &GraphNode, item: Item) -> (Items, Items) {
  get_follow_internal(gb, node, item, FollowType::AllItems)
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum FollowType {
  AllItems,
  FirstReduction,
  ScannerCompleted,
}

/// Returns a tuple comprised of a vector of all items that follow the given
/// item, provided the given item is in a complete state, and a list of a all
/// items that are completed from directly or indirectly transitioning on the
/// nonterminal of the given item.
pub(crate) fn get_follow_internal(
  gb: &mut ConcurrentGraphBuilder,
  node: &GraphNode,
  item: Item,
  caller_type: FollowType,
) -> (Items, Items) {
  if !item.is_complete() {
    return (vec![item], vec![]);
  }

  let ____is_scan____ = node.graph_type() == GraphType::Scanner;
  let mut completed = OrderedSet::new();
  let mut follow = OrderedSet::new();
  let mut oos_follow = OrderedSet::new();
  let mut queue = VecDeque::from_iter(vec![item]);
  let mode = node.graph_type();
  let db = &gb.db_rc();
  let root_nterm = item.nonterm_index(db);
  let mut seen_nonterminal_extents = OrderedSet::new();

  while let Some(c_item) = queue.pop_front() {
    if completed.insert(c_item) {
      let nterm: DBNonTermKey = c_item.nonterm_index(db);

      if caller_type == FollowType::FirstReduction && nterm != root_nterm {
        continue;
      }

      let result = if c_item.origin_state.is_oos() {
        if c_item.origin_state.is_oos_entry() {
          // Create or retrieve a new OOS state for this set of items.
          if seen_nonterminal_extents.insert(nterm) {
            let closure_state = gb.get_oos_root_state(nterm);
            let closure = closure_state.kernel_items().iter().cloned();
            process_closure(db, closure, &mut queue, &mut follow)
          } else {
            None
          }
        } else {
          let state = gb.get_oos_closure_state(c_item);

          let mut closure = state
            .kernel_items()
            .iter()
            .filter(|i| i.nonterm_index_at_sym(mode, db) == Some(nterm))
            .cloned()
            .filter_map(|i| i.increment())
            .peekable();

          if closure.peek().is_none() {
            queue.push_back(c_item.to_origin_state(StateId::extended_entry_base()));
            None
          } else {
            process_closure(db, closure, &mut queue, &mut follow)
          }
        }
      } else {
        let closure_state_id = c_item.origin_state;
        let origin = c_item.origin;
        let is_scanner_oos = origin.is_scanner_oos();

        let state = node
          .get_predecessor(c_item.origin_state)
          .expect(&format!("Node should have predecessors unless it is a root node {node:?} {}", c_item._debug_string_w_db_(db)));

        let closure = state
          .kernel_items()
          .iter()
          .filter(|k_i| c_item.is_successor_of(k_i) && (!is_scanner_oos || c_item.origin.is_scanner_oos()))
          .flat_map(|k_i| k_i.closure_iter_align(k_i.to_origin(origin).to_origin_state(closure_state_id), db))
          .filter(|i| i.nonterm_index_at_sym(mode, db) == Some(nterm))
          .filter_map(|i| i.increment());

        process_closure(db, closure, &mut queue, &mut follow)
      };

      if let Some(oos_queue) = result {
        for next_item in oos_queue {
          gb.get_oos_closure_state(next_item);
          oos_follow.insert(next_item);
        }
      } else {
        if !c_item.origin_state.is_root() && !c_item.origin_state.is_oos() {
          if let Some(parent_state) = node.get_predecessor(c_item.origin_state).unwrap().parent() {
            queue.push_back(c_item.to_origin_state(parent_state.id()));
          }
        } else if !____is_scan____ && !c_item.origin_state.is_oos_entry() {
          queue.push_back(c_item.to_origin_state(StateId::extended_entry_base()));
        }
      }
    }
  }

  (follow.into_iter().chain(oos_follow.into_iter()).collect(), completed.to_vec())
}

fn process_closure(
  db: &ParserDatabase,
  closure: impl Iterator<Item = Item>,
  queue: &mut VecDeque<Item>,
  follow: &mut OrderedSet<Item>,
) -> Option<VecDeque<Item>> {
  let mut oos_queue = VecDeque::new();
  let mut closure_yielded_items = false;

  for item in closure {
    closure_yielded_items |= true;
    match item.get_type(db) {
      ItemType::Completed(_) => queue.push_back(item),
      _ => {
        if item.is_oos() {
          oos_queue.push_front(item);
        } else {
          follow.insert(item);
        };
      }
    }
  }

  closure_yielded_items.then_some(oos_queue)
}

pub(crate) fn get_completed_item_artifacts<'a, 'follow, T: ItemRefContainerIter<'a>>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &GraphNode,
  completed: T,
) -> RadlrResult<CompletedItemArtifacts> {
  let db = &gb.db_rc();
  let mut follow_pairs = OrderedSet::new();
  let mut default_only_items = ItemSet::new();

  fn create_pair(k_i: Item, i: Item, db: &ParserDatabase) -> TransitionPair {
    TransitionPair {
      kernel:       k_i,
      next:         i,
      allow_assign: true,
      prec:         i.token_precedence(db),
      sym:          i.sym_id(db),
    }
  }

  for k_i in completed {
    if k_i.origin.is_scanner_oos() {
      // Do not create any states for OOS scanner items
      continue;
    }

    let (f, d) = get_follow_internal(gb, pred, *k_i, FollowType::AllItems);

    if f.is_empty() {
      debug_assert!(!d.is_empty());
      default_only_items.insert(*k_i);
      follow_pairs.extend([create_pair(*k_i, *k_i, db)]);
    } else {
      for k_follow in f {
        if let Origin::__OOS_CLOSURE__ = k_follow.origin {
          let state = gb.get_oos_closure_state(k_follow);
          follow_pairs.extend(state.kernel_items().iter().filter(|i| !i.is_complete()).map(|i| create_pair(*k_i, *i, db)));
        } else {
          follow_pairs.extend(k_follow.closure_iter_align(k_follow.to_origin(k_i.origin), db).map(|i| create_pair(*k_i, i, db)))
        }
      }
    }
  }

  RadlrResult::Ok(CompletedItemArtifacts { lookahead_pairs: follow_pairs, default_only: default_only_items })
}

pub(super) fn get_goal_items_from_completed<'db, 'follow>(items: &Items, node: &GraphNode) -> ItemSet {
  items.iter().filter(|i| node.item_is_goal(**i)).cloned().collect()
}

pub(super) fn merge_occluding_token_items(from_groups: GroupedFirsts, into_groups: &mut GroupedFirsts, db: &ParserDatabase) {
  for (sym, group) in into_groups.iter_mut() {
    let occluding_items = get_set_of_occluding_token_items(sym, group, &from_groups, db);
    group.1.extend(occluding_items);
  }
}

pub(super) fn get_set_of_occluding_token_items(
  into_sym: &SymbolId,
  into_group: &TransitionGroup,
  groups: &GroupedFirsts,
  _db: &ParserDatabase,
) -> Lookaheads {
  let mut occluding = Lookaheads::new();
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
