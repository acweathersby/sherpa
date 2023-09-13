#![allow(unused)]

use super::{
  super::{
    build::{GroupedFirsts, TransitionGroup},
    graph::*,
  },
  create_call,
  create_peek,
  handle_completed_item,
  CreateCallResult,
};
use crate::{
  compile::build_graph::{
    build::handle_completed_groups,
    errors::{conflicting_symbols_error, lr_disabled_error, peek_not_allowed_error},
    items::{get_follow, get_follow_symbols},
  },
  journal::config,
  parser::Shift,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
pub(super) enum ShiftReduceConflictResolution {
  Shift,
  Reduce,
  Peek,
  Fork,
}

pub(super) enum ReduceReduceConflictResolution<'db> {
  Reduce(Item<'db>),
  BreadCrumb(Follows<'db>),
  Peek(Follows<'db>),
  Nothing,
}

pub(super) fn resolve_reduce_reduce_conflict<'db>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: Follows<'db>,
) -> ReduceReduceConflictResolution<'db> {
  if prec_sym.sym().is_default() {
    ReduceReduceConflictResolution::Nothing
  } else {
    ReduceReduceConflictResolution::Peek(follow_pairs)
  }
}

pub(super) fn resolve_shift_reduce_conflict<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &GraphBuilder<'db>,
  shifts: T,
  reduces: T,
) -> SherpaResult<ShiftReduceConflictResolution> {
  // gb._print_graph_();
  // gb._print_state_();
  // reduces.clone()._debug_print_("REDUCES");
  // shifts.clone()._debug_print_("SHIFTS");

  let mode = gb.get_mode();

  let compl_prec = reduces.clone().map(|i| i.kernel.decrement().unwrap().precedence(gb.get_mode())).max().unwrap_or_default();
  let incom_prec = shifts.clone().max_precedence();

  // If all the shift items reduce to the same nterm and all completed items where
  // completed after shifting the same nterm, then the precedence of the shift
  // items determines whether we should shift first or reduce.

  let incom_nterm_set: OrderedSet<DBNonTermKey> = shifts.clone().to_next().rule_nonterm_ids();
  let compl_nterm_set: OrderedSet<DBNonTermKey> = reduces.clone().map(|i| i.kernel.nonterm_index()).collect();

  if incom_nterm_set.len() == 1 && incom_nterm_set.is_superset(&compl_nterm_set) {
    //println!("precedence climbing? else!?!");
    return if incom_prec > compl_prec {
      Ok(ShiftReduceConflictResolution::Shift)
    } else {
      Ok(ShiftReduceConflictResolution::Reduce)
    };
  }
  const MAX_EVALUATION_K: usize = 8;
  /// This conflict requires some form of lookahead or fork to disambiguate,
  /// prefer peeking unless k is too large or the ambiguity is resolved outside
  /// the goal non-terminal
  match calculate_k(gb, reduces.clone(), shifts.clone(), MAX_EVALUATION_K) {
    KCalcResults::K(k) => {
      if !gb.config.ALLOW_PEEKING {
        if gb.config.ALLOW_FORKING {
          return Ok(ShiftReduceConflictResolution::Fork);
        }
        peek_not_allowed_error(
          gb,
          &[shifts.cloned().collect(), reduces.cloned().collect()],
          &format!("Either peeking or forking must be enabled to resolve ambiguity, which is k={k}"),
        )?;
      } else if k >= MAX_EVALUATION_K {
        //peek_not_allowed_error(gb, &[shifts.cloned().collect(),
        // reduces.cloned().collect()], "Max")?;
        return Ok(ShiftReduceConflictResolution::Peek);
      } else if k > gb.config.max_k {
        peek_not_allowed_error(gb, &[shifts.cloned().collect(), reduces.cloned().collect()], "")?;
      }
      Ok(ShiftReduceConflictResolution::Peek)
    }
    KCalcResults::RecursiveAt(k) => {
      // Test for classic shift reduce problem.
      let incom_nterm_set_sr: OrderedSet<Option<DBNonTermKey>> =
        shifts.clone().to_kernel().map(|i| i.decrement().and_then(|i| i.nonterm_index_at_sym(mode))).collect();
      let compl_nterm_set_sr: OrderedSet<Option<DBNonTermKey>> =
        reduces.clone().to_kernel().map(|i| i.decrement().and_then(|i| i.nonterm_index_at_sym(mode))).collect();

      if !incom_nterm_set_sr.contains(&None)
        && !compl_nterm_set_sr.contains(&None)
        && incom_nterm_set_sr.len() == 1
        && incom_nterm_set_sr.is_superset(&compl_nterm_set_sr)
      {
        if incom_prec >= compl_prec {
          return Ok(ShiftReduceConflictResolution::Shift);
        } else if incom_prec < compl_prec {
          return Ok(ShiftReduceConflictResolution::Reduce);
        }
      }
      Ok(ShiftReduceConflictResolution::Fork)
    }
    KCalcResults::LargerThanMaxLimit(k) => {
      if !gb.config.ALLOW_PEEKING {
        if gb.config.ALLOW_FORKING {
          return Ok(ShiftReduceConflictResolution::Fork);
        }
        peek_not_allowed_error(
          gb,
          &[shifts.cloned().collect(), reduces.cloned().collect()],
          &format!("Either peeking or forking must be enabled to resolve ambiguity, which requires k > {k} to resolve"),
        )?;
      }
      Ok(ShiftReduceConflictResolution::Peek)
    }
  }
}

enum KCalcResults {
  K(usize),
  /// The peek paths contains recursive items, which will requires infinite time
  /// and space to resolve.
  RecursiveAt(usize),
  /// The peek paths are looking promising, but k is going to be more than the
  /// max eval limit.
  LargerThanMaxLimit(usize),
}

fn calculate_k<'a, 'db: 'a, A: TransitionPairRefIter<'a, 'db> + Clone, B: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &GraphBuilder<'db>,
  reduces: B,
  shifts: A,
  MAX_EVALUATION_K: usize,
) -> KCalcResults {
  fn get_follow_artifacts<'db>(
    i_reduce: &Vec<Item<'db>>,
    gb: &GraphBuilder<'db>,
  ) -> (OrderedSet<Item<'db>>, OrderedSet<DBTermKey>) {
    let mut out = OrderedSet::default();
    let mut syms = OrderedSet::default();
    for item in i_reduce {
      let (follow, default) = get_follow(gb, *item, false);

      let items = follow.iter().flat_map(|i| i.closure_iter_align(*i));

      syms.extend(items.clone().filter_map(|i| i.term_index_at_sym(gb.get_mode())));
      out.extend(items);
    }
    (out, syms)
  }

  // Find max K for the conflicts to determine if we should us the peek mechanism

  let mut reduce_items: ItemSet = reduces.clone().to_next().cloned().collect();
  let mut shift_items: ItemSet = shifts.clone().to_next().cloned().collect();

  for k in 2..=(MAX_EVALUATION_K) {
    let (out, reduce_syms) = get_follow_artifacts(&reduce_items.try_increment(), gb);
    reduce_items = out;

    let (out, shift_syms) = get_follow_artifacts(&shift_items.try_increment(), gb);
    shift_items = out;

    // reduce_items._debug_print_("REDUCE ITEMS");
    // shift_items._debug_print_("SHIFT_ITEMS");

    // println!(
    //   " shift_syms: {}",
    //   shift_syms.iter().map(|s|
    // gb.db.token(*s).name.to_string(gb.db.string_store())).collect::<Vec<_>>().
    // join(" | ") );
    // println!(
    //   "reduce_syms: {}",
    //   reduce_syms.iter().map(|s|
    // gb.db.token(*s).name.to_string(gb.db.string_store())).collect::<Vec<_>>().
    // join(" | ") );

    if reduce_syms.intersection(&shift_syms).next().is_none() {
      // Have to consider items outside the domain of the goal-nonterminal, so we'll
      // have to a strategy other than peek to this conflict.
      return KCalcResults::K(k);
    }

    if shift_items.iter().filter(|i| i.is_recursive()).any(|i| reduce_items.contains(i)) {
      //println!("----\nk={k}");
      //shift_items.iter().filter(|i| i.is_recursive())._debug_print_(
      //  "RECUSIVE
      // SHIFT",
      //);
      //reduce_items.iter().filter(|i| i.is_recursive())._debug_print_("RECUSIVE
      // REDUCE"); println!("----\n");
      return KCalcResults::RecursiveAt(k);
    }
  }

  KCalcResults::LargerThanMaxLimit(MAX_EVALUATION_K)
}

pub(crate) fn resolve_conflicting_tokens<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &mut GraphBuilder<'db>,
  sym: SymbolId,
  completed: T,
) -> SherpaResult<()> {
  // Map items according to their symbols
  let token_precedence_groups = hash_group_btree_iter::<Vec<_>, _, _, _, _>(completed.clone(), |_, i| {
    (i.kernel.origin_precedence(), i.kernel.origin.get_symbol(gb.db))
  });

  let base_precedence_groups = hash_group_btreemap(token_precedence_groups, |_, ((_, sym), _)| sym.conflict_precedence());

  if let Some((_, groups)) = base_precedence_groups.into_iter().rev().next() {
    let mut _completed: Option<&Follows> = None;

    if groups.len() == 1 {
      _completed = Some(groups.values().next().unwrap());
    } else if let Some((_, sub_group)) = groups.iter().rev().next() {
      if sub_group.len() == 1 {
        _completed = Some(&sub_group);
      }
    }

    if let Some(completed_items) = _completed {
      handle_completed_item(gb, completed_items.clone(), (sym, 0).into())
    } else {
      Err(conflicting_symbols_error(gb, groups))
    }
  } else {
    Ok(())
  }
}
