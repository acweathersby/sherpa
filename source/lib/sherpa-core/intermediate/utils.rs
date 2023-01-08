//! Private functions that are either used in transition.rs or state.rs, but want
//! to organize here to keep those file clean. The functions here may also be
//! general enough to be used in modules other than transition, as well.

use crate::{
  grammar::{
    get_closure_cached,
    get_closure_cached_with_state,
    get_production_start_items,
    get_scanner_info_from_defined,
  },
  journal::Journal,
  types::{Item, ProductionId, RecursionType, SymbolID, *},
};
use std::{
  collections::{btree_map::Entry, BTreeMap, BTreeSet, HashSet, VecDeque},
  hash::Hash,
  vec,
};

/// Remove items that would cause infinite recursion
pub(crate) fn get_valid_recursive_descent_start_items(
  starts: &[Item],
  g: &GrammarStore,
  invalid_productions: &BTreeSet<ProductionId>,
) -> (Items, Items) {
  let mut output: BTreeMap<Item, Item> = BTreeMap::new();
  let mut goto_seeds: BTreeSet<Item> = BTreeSet::new();
  let starts = starts.to_vec();

  for item in starts.incomplete_items() {
    let mut closure = get_closure_cached_with_state(&item, g).clone();
    let mut local_invalid = BTreeSet::new();

    for item in &closure {
      if invalid_productions.contains(&item.get_production_id_at_sym(g)) {
        local_invalid.insert(item.get_prod_id(g));
      }
    }

    if local_invalid.len() > 0 {
      let mut changed = true;

      while changed {
        changed = false;
        goto_seeds.append(
          &mut (closure
            .drain_filter(|item| {
              if local_invalid.contains(&item.get_production_id_at_sym(g)) {
                local_invalid.insert(item.get_prod_id(g));
                changed = true;
                true
              } else {
                false
              }
            })
            .collect()),
        );
      }

      let remaining_productions = closure
        .iter()
        .filter_map(|i| match i.is_nonterm(g) {
          true => Some(i.get_production_id_at_sym(g)),
          _ => None,
        })
        .collect::<BTreeSet<_>>();

      closure.drain_filter(|item| remaining_productions.contains(&item.get_prod_id(g)));

      for item in closure {
        if !output.contains_key(&item.to_empty_state()) {
          output.insert(item.to_empty_state(), item);
        }
      }
    } else {
      if !output.contains_key(&item.to_empty_state()) {
        output.insert(item.to_empty_state(), item);
      }
    }
  }

  (output.into_values().collect(), goto_seeds.to_vec())
}

/// Constructs a Vector of Vectors, each of which contains a set items from the
/// original vector that have been grouped by the hash of a common distinguisher.
#[inline]
pub fn hash_group_vec<
  T: Copy + Sized,
  R: Hash + Sized + Ord + Eq,
  E: IntoIterator<Item = T> + Extend<T> + Default,
  Function: Fn(usize, T) -> R,
>(
  vector: E,
  hash_yielder: Function,
) -> Vec<E> {
  let mut hash_groups = BTreeMap::<R, E>::new();

  for (i, val) in vector.into_iter().enumerate() {
    match hash_groups.entry(hash_yielder(i, val)) {
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

/// Gathers symbols from a set of items into normal and skipped sets.
pub(crate) fn get_symbols_from_items(
  item_set: BTreeSet<Item>,
  g: &GrammarStore,
  seen: Option<BTreeSet<ProductionId>>,
  mut normal_symbol_set: SymbolSet,
) -> (SymbolSet, SymbolSet, BTreeSet<ProductionId>) {
  let mut ignore_symbol_set = BTreeSet::new();
  let mut seen = seen.unwrap_or_default();

  for item in item_set {
    match item.get_symbol(g) {
      SymbolID::Production(production_id, ..) => {
        if !seen.insert(production_id) {
          continue;
        }

        let production_items =
          get_production_start_items(&production_id, g).into_iter().collect::<BTreeSet<_>>();

        let (mut norm, mut ignore, new_seen) = get_symbols_from_items(
          production_items.to_empty_state(),
          g,
          Some(seen),
          Default::default(),
        );

        seen = new_seen;

        normal_symbol_set.append(&mut norm);
        ignore_symbol_set.append(&mut ignore);
      }
      SymbolID::EndOfInput | SymbolID::Default | SymbolID::Undefined => {}
      sym => {
        normal_symbol_set.insert(sym);
      }
    }

    if let Some(ignored_symbols) = g.item_ignore_symbols.get(&item.to_empty_state()) {
      for ignored_symbol in ignored_symbols {
        ignore_symbol_set.insert(*ignored_symbol);
      }
    }
  }

  let ignore_symbol_set =
    ignore_symbol_set.difference(&normal_symbol_set).cloned().collect::<BTreeSet<_>>();

  (normal_symbol_set, ignore_symbol_set, seen)
}

/// Compares whether symbolB occludes symbolA
/// ( produces an ambiguous parse path )
///
/// Symbols that can occlude are as follows
///
/// - `g:id` and any single identifier character.
/// - `g:num` and any single numeric character.
/// - `g:sym` and any single character thats not a numeric,
///   identifier, space, newline, or tab.

pub fn symbols_occlude(symA: &SymbolID, symB: &SymbolID, g: &GrammarStore) -> bool {
  match symA {
    SymbolID::DefinedSymbol(..) => match g.symbol_strings.get(symA).map(|s| s.as_str()) {
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
    },
    SymbolID::DefinedIdentifier(_) | SymbolID::ExclusiveDefinedIdentifier(_) => {
      match g.symbol_strings.get(symA).map(|s| s.as_str()) {
        Some("_") | Some("-") => match symB {
          SymbolID::GenericSymbol => true,
          _ => false,
        },
        Some(_) => match symB {
          SymbolID::GenericIdentifier => g.symbols.get(symA).unwrap().cp_len == 1,
          _ => false,
        },
        _ => false,
      }
    }
    SymbolID::DefinedNumeric(_) | SymbolID::ExclusiveDefinedNumeric(_) => match symB {
      SymbolID::GenericNumber => g.symbols.get(symA).unwrap().cp_len == 1,
      _ => false,
    },
    symA => *symA == *symB,
  }
}

pub(crate) fn check_for_left_recursion(symbol_items: &Items, g: &GrammarStore) {
  let mut productions = HashSet::new();
  let mut seen = HashSet::new();
  let mut pipeline =
    VecDeque::from_iter(symbol_items.iter().flat_map(|i| get_closure_cached(i, g)).cloned());

  while let Some(item) = pipeline.pop_front() {
    if seen.insert(item) && !item.completed() {
      productions.insert(item.get_prod_id(g));

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

  assert!(
    productions.iter().all(|p| {
      let production = g.productions.get(p).unwrap();
      let has_left = production
        .recursion_type
        .intersects(RecursionType::LEFT_DIRECT | RecursionType::LEFT_INDIRECT);

      if has_left {
        symbol_items.__print_items__(g, "existing symbols");
        eprintln!(
          "[{}] {}",
          production.name,
          production.loc.blame(1, 1, "this production is left recursive", None),
        );
      }

      !has_left
    }),
    "Scanner productions cannot contain left recursion!"
  );
}

/// Returns a vector of items whose position is immediately after
/// a non-terminal symbol that matches one of the root ids.
pub(crate) fn get_follow_closure(g: &GrammarStore, root_ids: &BTreeSet<ProductionId>) -> Items {
  let mut pending_prods = VecDeque::<ProductionId>::new();
  let mut seen_prods = BTreeSet::<ProductionId>::new();

  for prod_id in root_ids {
    pending_prods.push_back(*prod_id);
  }

  let mut output = BTreeSet::new();

  while let Some(production_id) = pending_prods.pop_front() {
    if !seen_prods.insert(production_id) {
      continue;
    }

    let items: Items = g
      .lr_items
      .get(&production_id)
      .unwrap_or(&Vec::new())
      .iter()
      .map(|i| i.increment().unwrap())
      .collect();

    for item in items {
      if item.completed() {
        pending_prods.push_back(item.get_prod_id(g));
      }

      output.insert(item.decrement().unwrap());
    }

    seen_prods.insert(production_id);
  }

  output.into_iter().collect()
}

/// Constructs Scanner items from a set of symbols.
pub(crate) fn generate_scanner_symbol_items(symbols: SymbolSet, j: &mut Journal) -> Items {
  let g = j.grammar().unwrap();

  let items = symbols
    .iter()
    .flat_map(|s| {
      let (_, prod_id, ..) = get_scanner_info_from_defined(s, &g);
      get_production_start_items(&prod_id, &g)
        .iter()
        .map(|i| i.to_origin(crate::types::OriginData::Symbol(*s)))
        .collect::<Items>()
    })
    .collect::<Items>();

  // Ensure there are no left recursive productions within the items
  check_for_left_recursion(&items, &g);

  items
}

/// Constructs Scanner items from a set of symbols.
pub(crate) fn generate_recursive_descent_items(j: &mut Journal, prod_id: ProductionId) -> Items {
  get_production_start_items(&prod_id, &j.grammar().unwrap())
}
