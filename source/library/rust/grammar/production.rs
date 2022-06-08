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
pub fn is_production_recursive(production: ProductionId, grammar: &GrammarStore) -> (bool, bool) {
    let mut seen = HashSet::<Item>::new();
    let mut pipeline = VecDeque::from_iter(get_closure(
        &get_production_start_items(&production, grammar),
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
    }

    (false, false)
}

/// Used to separate a grammar's uuid name from a production's name
const UUID_NAME_DELIMITER: &str = "#:";

/// Generate a unique scanner production name givin a uuid production name
pub fn create_scanner_name(uuid_production_name: &String) -> String {
    format!("##scan__{}__", uuid_production_name)
}

/// Generate a UUID name using the grammars uuid_name and the productions
/// name (omitting local import name portion of a production)
pub fn create_production_uuid_name(grammar_uuid_name: &String, production_name: &String) -> String {
    grammar_uuid_name.to_owned() + UUID_NAME_DELIMITER + production_name
}

/// Retrieve the non-import and un-mangled name of a production
pub fn get_production_plain_name<'a>(
    production_id: &ProductionId,
    grammar: &'a GrammarStore,
) -> &'a str {
    let production = grammar.production_table.get(production_id).unwrap();
    let name = &production.name;
    let split = name.split(UUID_NAME_DELIMITER);
    split.last().unwrap()
}

/// Retrieves first the production_id of the first production
/// whose plain name matches  the query string. Returns None if no
/// production matches the query. The order of the productions is not
/// guaranteed.
pub fn get_production_by_name(name: &str, grammar: &GrammarStore) -> Option<ProductionId> {
    for production_id in grammar.production_table.keys() {
        if name == get_production_plain_name(production_id, grammar) {
            return Some(production_id.to_owned());
        }
    }
    None
}

#[cfg(test)]
mod production_utilities_tests {

    use super::*;
    use crate::debug::compile_test_grammar;

    #[test]
    fn test_get_production_plain_name() {
        let grammar = compile_test_grammar("<>billofolious_tantimum^a>\\o");
        assert_eq!(
            get_production_plain_name(grammar.production_table.keys().next().unwrap(), &grammar),
            "billofolious_tantimum"
        );
        assert_ne!(
            grammar.production_table.values().next().unwrap().name,
            "billofolious_tantimum"
        );
    }

    #[test]
    fn test_get_production_by_name() {
        let grammar = compile_test_grammar(
            "
<> Apple > \\o
<> Bad_Cakes > \\b
",
        );

        assert!(get_production_by_name("Apple", &grammar).is_some());
        assert!(get_production_by_name("Bad_Cakes", &grammar).is_some());
        assert!(get_production_by_name("Bandible", &grammar).is_none());
    }

    #[test]
    fn test_is_production_recursive() {
        let grammar = compile_test_grammar(
            "
<> A > B 
<> B > C
<> C > D
<> D > E
<> E > B A R | t:e
<> R > R A | R B O | t:r
<> O > t:o
",
        );

        let production = get_production_by_name("A", &grammar).unwrap();
        assert_eq!(is_production_recursive(production, &grammar), (true, false));
        let production = get_production_by_name("R", &grammar).unwrap();
        assert_eq!(is_production_recursive(production, &grammar), (true, true));
        let production = get_production_by_name("B", &grammar).unwrap();
        assert_eq!(is_production_recursive(production, &grammar), (true, true));
        let production = get_production_by_name("C", &grammar).unwrap();
        assert_eq!(is_production_recursive(production, &grammar), (true, true));
        let production = get_production_by_name("O", &grammar).unwrap();
        assert_eq!(
            is_production_recursive(production, &grammar),
            (false, false)
        );
    }
}
