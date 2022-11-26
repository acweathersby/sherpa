//! Utility functions for the evaluation, interpretation, and
//! comprehension of items

use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::VecDeque;

use crate::types::GrammarStore;
use crate::types::Item;
use crate::types::ProductionId;
use crate::types::SymbolID;

/// Retrieve the initial items of a production. Returns vector of
/// items, one for each body belonging to the production.
#[inline]
pub fn get_production_start_items(prod_id: &ProductionId, g: &GrammarStore) -> Vec<Item> {
  g.production_bodies
    .get(prod_id)
    .unwrap()
    .iter()
    .map(|id| Item::from_body(id, g).unwrap())
    .collect()
}

/// Create the closure of a set of items.
pub fn create_closure(items: &[Item], g: &GrammarStore) -> Vec<Item> {
  let mut seen = BTreeSet::<Item>::new();

  let mut queue = VecDeque::<Item>::from_iter(items.iter().cloned());

  while let Some(item) = queue.pop_front() {
    if seen.insert(item) {
      if let SymbolID::Production(prod_id, _) = &item.get_symbol(g) {
        for item in get_production_start_items(prod_id, g) {
          queue.push_back(item)
        }
      }
    }
  }

  seen.into_iter().collect()
}

/// Retrieve the closure of an item that is cached in the grammar
/// store. Falls back to manually building the closure if it is not
/// cached. Does not modify the grammar store object.
#[inline]
pub fn get_closure_cached<'a>(item: &Item, g: &'a GrammarStore) -> &'a Vec<Item> {
  static empty_closure: Vec<Item> = vec![];
  if g.closures.get(&item.to_zero_state()).is_none() {
    &empty_closure
  } else {
    g.closures.get(&item.to_zero_state()).unwrap()
  }
}

/// Memoized form of 'get_closure_cached', which adds the Item's
/// closure to the grammar store if it is not already present.
#[inline]
pub fn get_closure_cached_mut<'a>(item: &Item, g: &'a mut GrammarStore) -> &'a Vec<Item> {
  let item = &item.to_zero_state();

  if !g.closures.contains_key(item) {
    let closure = create_closure(&[*item], g);

    g.closures.insert(*item, closure);
  }

  g.closures.get(item).unwrap()
}
