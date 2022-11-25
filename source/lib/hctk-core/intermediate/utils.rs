//! Private functions that are either used in transition.rs or state.rs, but want
//! to organize here to keep those file clean. The functions here may also be
//! general enough to be used in modules other than transition, as well.

use std::collections::btree_map;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::path::PathBuf;
use std::process::id;
use std::rc::Rc;
use std::sync::Arc;
use std::vec;

use crate::debug::debug_items;
use crate::grammar::create_closure;
use crate::grammar::get_closure_cached;
use crate::grammar::get_production_start_items;
use crate::grammar::hash_id_value_u64;
use crate::types::BlameColor;
use crate::types::GrammarId;
use crate::types::GrammarStore;
use crate::types::HCResult;
use crate::types::Item;
use crate::types::ItemState;
use crate::types::OriginData;
use crate::types::ProductionId;
use crate::types::RecursionType;
use crate::types::SymbolID;
use crate::types::TPackResults;
use crate::types::TransitionGraphNode as TGN;
use crate::types::TransitionGraphNodeId;
use crate::types::TransitionMode;
use crate::types::TransitionPack as TPack;
use crate::types::TransitionStateType as TST;

/// Remove items that would cause LL branch conflicts and replace
/// with their derivatives.
///  
/// ## For Example
///
/// ```hcg
///  
/// <> A > B | C
///
/// <> C > \d
///
/// <> B > C \d
/// ```
/// Given the above grammar, the starting items for `A` are
/// `B => C \d` and `C => \d`. If treated directly as RD items
/// then the input `d d` requires us to determine if the resulting
/// Production `C` (from `C => \d *`) should apply to `B => C * \d`
/// or `A => C *`. Pure RD would either require a peek or a backtrack
/// since the Production A must call either C or B. However, if using
/// LR behavior, then we can call `C`, then use A's GOTO state:
/// ```hcg
/// A`Goto
///    1 | B     -> A
///    2 | C     -> A
///    3 | C \d  -> B
/// ```
/// to reduce `C` to `B` (as a result of the next symbol `\d` matching the conditions
/// for the transition to GOTO 3: `C * \d -> B`),
/// then `B` to `A`.
pub fn get_valid_starts(starts: &[Item], g: &GrammarStore) -> (Vec<Item>, Vec<Item>) {
  if starts.len() == 1 {
    // There's is now way the parse of single can encounter conflicts, so it is simply returned.
    (starts.to_vec(), vec![])
  } else {
    let mut goto_seeds = BTreeSet::new();

    let mut valid_term_items =
      starts.iter().flat_map(|i| get_closure_cached(i, g)).cloned().collect::<BTreeSet<_>>();

    let mut valid_non_term =
      valid_term_items.drain_filter(|i| i.is_nonterm(g)).collect::<BTreeSet<_>>();

    // Detects if a specific non-term symbol is parsed by one or more
    // unique productions. If this is the case, then we make the productions
    // invalid.
    let mut invalid_productions = valid_non_term
      .iter()
      .map(|i| (i.get_production_id_at_sym(g), i.get_prod_id(g)))
      .fold(BTreeMap::<ProductionId, BTreeSet<ProductionId>>::new(), |mut b, (sym, p)| {
        match b.entry(sym) {
          Entry::Occupied(mut e) => {
            e.get_mut().insert(p);
          }
          Entry::Vacant(e) => {
            e.insert(BTreeSet::from_iter(vec![p]));
          }
        };
        b
      })
      .into_iter()
      .filter_map(|(a, b)| if b.len() > 1 { Some(a) } else { None })
      .chain(starts.iter().map(|s| s.get_prod_id(g)))
      .collect::<BTreeSet<_>>();

    // Now we filter out the invalid productions.
    // Any production that parses a invalid non-term is itself invalid
    // We remove all items of that production from the output  (placing them instead
    // into the goto seeds set), and place the production into the invalid production set.
    // This continues until there are no new productions added to the invalid set.
    let mut changed = true;
    while changed {
      changed = false;
      goto_seeds.append(
        &mut valid_non_term
          .drain_filter(|item| {
            if invalid_productions.contains(&item.get_production_id_at_sym(g)) {
              invalid_productions.insert(item.get_prod_id(g));
              changed = true;
              true
            } else if invalid_productions.contains(&item.get_prod_id(g)) {
              true
            } else {
              false
            }
          })
          .collect(),
      );
    }

    // Now we have a nearly complete valid set of non-term items. There may be
    // some items that parse a non-term symbol whose items also exist in the
    // valid set. These items are now moved to the goto seeds set.
    let valid_productions =
      valid_non_term.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>();
    goto_seeds.append(
      &mut valid_non_term
        .drain_filter(|i| valid_productions.contains(&i.get_production_id_at_sym(g)))
        .collect(),
    );

    // Now the term items of productions being parsed by non-term items need not
    // be present in the term set. We'll remove them now.
    let parsed_productions =
      valid_non_term.iter().map(|i| i.get_production_id_at_sym(g)).collect::<BTreeSet<_>>();
    valid_term_items.drain_filter(|i| parsed_productions.contains(&i.get_prod_id(g)));

    // The union of valid non-term items and the remaining term items is our
    // start set.
    valid_term_items.append(&mut valid_non_term);

    (valid_term_items.into_iter().collect(), goto_seeds.into_iter().collect())
  }
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
  T: Copy + Sized,
  R: Hash + Sized + Ord + Eq,
  E: IntoIterator<Item = T> + Extend<T> + Default,
  Function: Fn(usize, T) -> R,
>(
  vector: E,
  hash_yielder: Function,
) -> BTreeMap<R, E> {
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

  hash_groups
}

/// Gathers symbols from a set of items into normal and skipped sets.
pub fn get_symbols_from_items(
  item_set: BTreeSet<Item>,
  g: &GrammarStore,
  seen: Option<BTreeSet<ProductionId>>,
) -> (BTreeSet<SymbolID>, BTreeSet<SymbolID>, BTreeSet<ProductionId>) {
  let mut normal_symbol_set = BTreeSet::new();
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

        let (mut norm, mut ignore, new_seen) =
          get_symbols_from_items(production_items, g, Some(seen));

        seen = new_seen;

        normal_symbol_set.append(&mut norm);
        ignore_symbol_set.append(&mut ignore);
      }
      SymbolID::EndOfFile | SymbolID::Default | SymbolID::Undefined => {}
      sym => {
        normal_symbol_set.insert(item.get_symbol(g));
      }
    }

    if let Some(ignored_symbols) = g.item_ignore_symbols.get(&item.to_zero_state()) {
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
    SymbolID::DefinedSymbol(..) | SymbolID::ExclusiveDefinedSymbol(..) => {
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
          _ => false,
        },
        _ => false,
      }
    }
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
    _ => false,
  }
}

/// True if the closure of the givin items does not include
/// the goal production on the right of the cursor
pub fn non_recursive(
  items: &[Item],
  target_prod: &BTreeSet<ProductionId>,
  g: &GrammarStore,
) -> bool {
  for item in create_closure(items, g) {
    if let SymbolID::Production(production, _) = item.get_symbol(g) {
      if target_prod.contains(&production) {
        return false;
      }
    }
  }

  true
}

pub fn check_for_left_recursion(symbol_items: &Vec<Item>, g: &GrammarStore) {
  let mut productions = HashSet::new();
  let mut seen = HashSet::new();
  let mut pipeline =
    VecDeque::from_iter(symbol_items.iter().flat_map(|i| get_closure_cached(i, g)).cloned());

  while let Some(item) = pipeline.pop_front() {
    if seen.insert(item) && !item.is_end() {
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
        println!(
          "[{}] {}",
          production.name,
          production.tok.blame(1, 1, "this production is left recursive", None),
        );
      }

      !has_left
    }),
    "Scanner productions cannot contain left recursion!"
  );
}
