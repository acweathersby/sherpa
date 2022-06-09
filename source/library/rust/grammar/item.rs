//! Utility functions for the evaluation, interpretation, and comprehension of items
//!

use std::collections::{HashSet, VecDeque};

use crate::primitives::{GrammarStore, Item, ProductionId, SymbolID};

///
/// Retrieve the initial items of a production. Returns vector of items, one
/// for each body belonging to the production.
pub fn get_production_start_items(production: &ProductionId, grammar: &GrammarStore) -> Vec<Item> {
    grammar
        .production_bodies_table
        .get(production)
        .unwrap()
        .iter()
        .map(|id| Item::from_body(id, grammar).unwrap())
        .collect()
}

///
/// Retrieve the closure of a set of items.
pub fn get_closure(items: &[Item], grammar: &GrammarStore) -> Vec<Item> {
    let mut seen = HashSet::<Item>::new();
    let mut queue = VecDeque::<Item>::from_iter(items.iter().cloned());

    while let Some(item) = queue.pop_front() {
        if seen.insert(item) {
            match &item.get_symbol(grammar) {
                SymbolID::Production(prod_id, _) => {
                    for item in get_production_start_items(prod_id, grammar) {
                        queue.push_back(item)
                    }
                }
                _ => {}
            }
        }
    }

    seen.into_iter().collect()
}

///
/// Retrieve the closure of an item that is cached in the grammar store.
/// Falls back to manually building the closure if it is not cached. Does
/// not modify the original grammar.
pub fn get_closure_cached<'a>(item: &Item, grammar: &'a GrammarStore) -> &'a Vec<Item> {
    grammar.closures.get(&item.to_zero_state()).unwrap()
}

///
/// Memoized form of 'get_closure_cached', which adds the Item's closure to
/// the grammar store if it is not already present.
pub fn get_closure_cached_mut<'a>(item: &Item, grammar: &'a mut GrammarStore) -> &'a Vec<Item> {
    let item = &item.to_zero_state();

    if !grammar.closures.contains_key(item) {
        let closure = get_closure(&vec![*item], grammar);
        grammar.closures.insert(*item, closure);
    }

    grammar.closures.get(item).unwrap()
}
