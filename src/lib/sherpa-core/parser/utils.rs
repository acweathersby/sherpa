//! Private functions that are either used in transition.rs or state.rs, but
//! want to organize here to keep those file clean. The functions here may also
//! be general enough to be used in modules other than transition, as well.

use crate::types::{SymbolID, *};
use std::{
  collections::{btree_map::Entry, BTreeMap},
  hash::Hash,
  vec,
};

/// Constructs a Vector of Vectors, each of which contains a set items from the
/// original vector that have been grouped by the hash of a common
/// distinguisher.
#[inline]
pub fn hash_group_vec<
  T: Sized,
  R: Hash + Sized + Ord + Eq,
  E: IntoIterator<Item = T> + Extend<T> + Default,
  Function: Fn(usize, &T) -> R,
>(
  vector: E,
  hash_yielder: Function,
) -> Vec<E> {
  let mut hash_groups = BTreeMap::<R, E>::new();

  for (i, val) in vector.into_iter().enumerate() {
    match hash_groups.entry(hash_yielder(i, &val)) {
      Entry::Vacant(e) => {
        let mut new_container = E::default();
        new_container.extend(vec![val]);
        e.insert(new_container);
      }
      Entry::Occupied(mut e) => e.get_mut().extend(vec![val]),
    }
  }

  hash_groups.into_values().collect()
}

#[inline]
pub fn hash_group_btreemap<
  T: Sized,
  R: Hash + Sized + Ord + Eq,
  E: IntoIterator<Item = T> + Extend<T> + Default,
  Function: FnMut(usize, &T) -> R,
>(
  vector: E,
  mut hash_yielder: Function,
) -> BTreeMap<R, E> {
  let mut hash_groups = BTreeMap::<R, E>::new();

  for (i, val) in vector.into_iter().enumerate() {
    match hash_groups.entry(hash_yielder(i, &val)) {
      Entry::Vacant(e) => {
        let mut new_container = E::default();
        new_container.extend(vec![val]);
        e.insert(new_container);
      }
      Entry::Occupied(mut e) => e.get_mut().extend(vec![val]),
    }
  }

  hash_groups
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

pub fn symbols_occlude(
  symA: &SymbolID,
  symB: &SymbolID,
  g: &GrammarStore,
) -> bool {
  match symA {
    SymbolID::DefinedSymbol(..) => {
      match g.symbol_strings.get(symA).map(|s| s.as_str()) {
        Some("\n") => match symB {
          SymbolID::GenericNewLine => true,
          _ => false,
        },
        Some("\t") => match symB {
          SymbolID::GenericHorizontalTab => true,
          _ => false,
        },
        Some(" ") => match symB {
          SymbolID::GenericSpace => true,
          _ => false,
        },
        Some(_) => match symB {
          SymbolID::GenericSymbol => g.symbols.get(symA).unwrap().cp_len == 1,
          _ => symA == symB,
        },
        _ => symA == symB,
      }
    }
    SymbolID::DefinedIdentifier(_)
    | SymbolID::ExclusiveDefinedIdentifier(_) => {
      match g.symbol_strings.get(symA).map(|s| s.as_str()) {
        Some("_") | Some("-") => match symB {
          SymbolID::GenericSymbol => true,
          _ => false,
        },
        Some(_) => match symB {
          SymbolID::GenericIdentifier => {
            g.symbols.get(symA).unwrap().cp_len == 1
          }
          _ => false,
        },
        _ => false,
      }
    }
    SymbolID::DefinedNumeric(_) | SymbolID::ExclusiveDefinedNumeric(_) => {
      match symB {
        SymbolID::GenericNumber => g.symbols.get(symA).unwrap().cp_len == 1,
        _ => false,
      }
    }
    symA => *symA == *symB,
  }
}
