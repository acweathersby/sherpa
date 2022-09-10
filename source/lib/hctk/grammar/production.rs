//! Utility functions for the evaluation, interpretation, and
//! comprehension of productions

use std::collections::HashSet;
use std::collections::VecDeque;

use crate::types::GrammarStore;
use crate::types::Item;
use crate::types::Production;
use crate::types::ProductionId;
use crate::types::SymbolID;

use super::create_closure;
use super::get_closure_cached;
use super::get_production_start_items;

/// Evaluates whether a production is recursive. Returns
/// a douple of booleans.
///
/// The first boolean value indicates that production is recursive.
///
/// The second boolean value indicates a production has left
/// recursive, either directly or indirectly.
pub fn is_production_recursive(production: ProductionId, g: &GrammarStore) -> (bool, bool) {
  let mut seen = HashSet::<Item>::new();

  let mut pipeline =
    VecDeque::from_iter(create_closure(&get_production_start_items(&production, g), g));

  while let Some(item) = pipeline.pop_front() {
    if seen.insert(item) && !item.is_end() {
      if let SymbolID::Production(prod_id, _) = item.get_symbol(g) {
        if prod_id == production {
          return (true, item.get_offset() == 0);
        }
      }

      let new_item = item.increment().unwrap();

      if let SymbolID::Production(..) = new_item.get_symbol(g) {
        for item in get_closure_cached(&new_item, g) {
          pipeline.push_back(*item);
        }
      } else {
        pipeline.push_back(new_item);
      }
    }
  }

  (false, false)
}

/// Used to separate a grammar's uuid name from a production's name
const GUID_NAME_DELIMITER: &str = "_GUID_";

/// Returns the [Production] that's mapped to [`production_id`](ProductionId)
/// within the [`grammar`](GrammarStore), or panics.
pub fn get_production<'a>(production_id: &ProductionId, g: &'a GrammarStore) -> &'a Production {
  g.productions.get(production_id).unwrap()
}

/// Generate a unique scanner production name givin a uuid production
/// name

pub fn create_scanner_name(uuid_production_name: &String) -> String {
  format!("scan_tok_{}__", uuid_production_name)
}

pub fn create_defined_scanner_name(uuid_production_name: &String) -> String {
  format!("scan_def_{}__", uuid_production_name)
}

/// Generate a UUID name using the grammars uuid_name and the
/// productions name (omitting local import name portion of a
/// production)

pub fn create_production_guid_name(grammar_uuid_name: &String, production_name: &String) -> String {
  grammar_uuid_name.to_owned() + GUID_NAME_DELIMITER + production_name
}

/// Retrieve the non-import and unmangled name of a [Production](Production).
pub fn get_production_plain_name<'a>(prod_id: &ProductionId, g: &'a GrammarStore) -> &'a str {
  if let Some(prod) = g.productions.get(prod_id) {
    &prod.original_name
  } else {
    ""
  }
}

/// Retrieves first the production_id of the first production
/// whose plain or guid name matches the query string.
/// Returns None if no production matches the query.
pub fn get_production_id_by_name(name: &str, g: &GrammarStore) -> Option<ProductionId> {
  for (prod_id, prod) in g.productions.iter() {
    if name == get_production_plain_name(prod_id, g) {
      return Some(prod_id.to_owned());
    }
    if name == prod.guid_name {
      return Some(prod_id.to_owned());
    }
  }

  None
}
/// Attempts to retrieve a production from the grammar with the matching name.
/// If the grammar is an aggregate of multiple grammars which define productions
/// with the same name, the production that is selected is undetermined.
pub fn get_production_by_name<'a>(name: &str, g: &'a GrammarStore) -> Option<&'a Production> {
  for production_id in g.productions.keys() {
    if name == get_production_plain_name(production_id, g) {
      return Some(g.productions.get(production_id).unwrap());
    }
  }

  None
}
/// A convenient wrapper around information used to construct parser entry points
/// based on [productions](Production).
pub struct ExportedProduction<'a> {
  /// The name assigned to the production within the
  /// export clause of a grammar.
  /// e.g. `@EXPORT production as <export_name>`
  pub export_name: &'a str,
  /// The GUID name assigned of the corresponding production.
  pub guid_name:   &'a str,
  /// The exported production.
  pub production:  &'a Production,
}

/// Returns a list of [ExportedProductions](ExportedProduction) extracted from
/// the [grammar](GrammarStore).
pub fn get_exported_productions<'a>(g: &'a GrammarStore) -> Vec<ExportedProduction<'a>> {
  g.export_names
    .iter()
    .map(|(id, name)| {
      let production = g.productions.get(id).unwrap();
      ExportedProduction { export_name: name, guid_name: &production.guid_name, production }
    })
    .collect::<Vec<_>>()
}

#[cfg(test)]

mod production_utilities_tests {

  use super::*;
  use crate::debug::compile_test_grammar;

  #[test]
  fn test_get_default_production() {
    let g = compile_test_grammar(
      "
@EXPORT start as test

<> start > \\hello \\and end

<> end > \\goodby
",
    );

    let exported_productions = get_exported_productions(&g);

    let first = exported_productions.first().unwrap();

    assert_eq!(first.production.original_name, "start");
    assert_eq!(first.export_name, "test");
  }

  #[test]

  fn test_get_production_plain_name() {
    let g = compile_test_grammar("<>billofolious_tantimum^a>\\o");

    let prod = get_production_id_by_name("billofolious_tantimum", &g).unwrap();

    assert_eq!(get_production_plain_name(&prod, &g), "billofolious_tantimum");

    assert_ne!(g.productions.get(&prod).unwrap().guid_name, "billofolious_tantimum");
  }

  #[test]

  fn test_get_production_by_name() {
    let g = compile_test_grammar(
      "
<> Apple > \\o
<> Bad_Cakes > \\b
",
    );

    assert!(get_production_id_by_name("Apple", &g).is_some());

    assert!(get_production_id_by_name("Bad_Cakes", &g).is_some());

    assert!(get_production_id_by_name("Bandible", &g).is_none());
  }

  #[test]

  fn test_is_production_recursive() {
    let g = compile_test_grammar(
      "
<> A > B 
<> B > C
<> C > D
<> D > E
<> E > B A R | \\e
<> R > R A | R B O | \\r
<> O > \\o
",
    );

    let production = get_production_id_by_name("A", &g).unwrap();

    assert_eq!(is_production_recursive(production, &g), (true, false));

    let production = get_production_id_by_name("R", &g).unwrap();

    assert_eq!(is_production_recursive(production, &g), (true, true));

    let production = get_production_id_by_name("B", &g).unwrap();

    assert_eq!(is_production_recursive(production, &g), (true, true));

    let production = get_production_id_by_name("C", &g).unwrap();

    assert_eq!(is_production_recursive(production, &g), (true, true));

    let production = get_production_id_by_name("O", &g).unwrap();

    assert_eq!(is_production_recursive(production, &g), (false, false));
  }
}
