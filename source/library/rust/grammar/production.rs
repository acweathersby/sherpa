//! Utility functions for the evaluation, interpretation, and comprehension of productions

use std::collections::{HashSet, VecDeque};

use crate::primitives::{GrammarStore, Item, ProductionId, SymbolID};

use super::{get_closure, get_closure_cached, get_production_start_items};

///
/// Evaluates whether a production is recursive. Returns
/// a tuple of booleans.
///
/// The first boolean value indicates that production is recursive.
///
/// The second boolean value indicates a production has left recursive, either
/// directly or indirectly.
fn is_production_recursive(production: ProductionId, grammar: &GrammarStore) -> (bool, bool) {
    let mut seen = HashSet::<Item>::new();
    let mut pipeline = VecDeque::from_iter(get_closure(
        get_production_start_items(&production, grammar),
        grammar,
    ));

    while let Some(item) = pipeline.pop_front() {
        if seen.insert(item) {
            if !item.is_end() {
                match item.get_symbol(grammar) {
                    SymbolID::Production(prod_id, _) => {
                        if prod_id == production {
                            return (true, item.get_offset() == 0);
                        }
                    }
                    _ => {}
                }
            }

            let new_item = item.increment().unwrap();

            match new_item.get_symbol(grammar) {
                SymbolID::Production(..) => {
                    for item in get_closure_cached(&new_item, grammar) {
                        pipeline.push_back(item);
                    }
                }
                _ => pipeline.push_back(new_item),
            }
        }
    }

    (false, false)
}
