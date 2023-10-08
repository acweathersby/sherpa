#![allow(unused)]

use std::collections::VecDeque;

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
  Peek(u16),
  Fork,
}

const MAX_EVAL_K: usize = 8;
pub(super) enum ReduceReduceConflictResolution<'db> {
  Reduce(Item<'db>),
  Fork(Lookaheads<'db>),
  Peek(u16, Lookaheads<'db>),
  Nothing,
}

pub(super) fn resolve_reduce_reduce_conflict<'db>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: Lookaheads<'db>,
) -> SherpaResult<ReduceReduceConflictResolution<'db>> {
  if prec_sym.sym().is_default() {
    Ok(ReduceReduceConflictResolution::Nothing)
  } else {
    match calculate_k_multi(gb, follow_pairs.iter().map(|i| [i].into_iter()).collect(), MAX_EVAL_K) {
      KCalcResults::K(k) => {
        if !gb.config.ALLOW_PEEKING || k >= MAX_EVAL_K {
          if gb.config.ALLOW_FORKING {
            return Ok(ReduceReduceConflictResolution::Fork(follow_pairs));
          }
          return peek_not_allowed_error(
            gb,
            follow_pairs.into_iter().map(|i| vec![i]).collect::<Vec<_>>().as_slice(),
            &format!("Either peeking or forking must be enabled to resolve this ambiguity, which requires a lookahead of k={k}"),
          );
        } else if k > gb.config.max_k {
          return peek_not_allowed_error(
            gb,
            follow_pairs.into_iter().map(|i| vec![i]).collect::<Vec<_>>().as_slice(),
            &format!(
              "A lookahead of k={k} is required, but lookahead cannot be greater than k={} with the current configuration",
              gb.config.max_k
            ),
          );
        }

        Ok(ReduceReduceConflictResolution::Peek(k as u16, follow_pairs))
      }
      _ => todo!("Resolve lookahead conflicts"),
    }
  }
}

pub(super) fn resolve_shift_reduce_conflict<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &mut GraphBuilder<'db>,
  shifts: T,
  reduces: T,
) -> SherpaResult<ShiftReduceConflictResolution> {
  let mode = gb.get_mode();

  let compl_prec = reduces.clone().map(|i| i.kernel.decrement().unwrap().precedence(gb.get_mode())).max().unwrap_or_default();
  let incom_prec = shifts.clone().max_precedence();

  // If all the shift items reduce to the same nterm and all completed items where
  // completed after shifting the same nterm, then the precedence of the shift
  // items determines whether we should shift first or reduce.

  let incom_nterm_set: OrderedSet<DBNonTermKey> = shifts.clone().to_next().rule_nonterm_ids();
  let compl_nterm_set: OrderedSet<DBNonTermKey> = reduces.clone().map(|i| i.kernel.nonterm_index()).collect();

  if incom_nterm_set.len() == 1 && incom_nterm_set.is_superset(&compl_nterm_set) {
    return if incom_prec >= compl_prec {
      Ok(ShiftReduceConflictResolution::Shift)
    } else {
      Ok(ShiftReduceConflictResolution::Reduce)
    };
  }
  /// This conflict requires some form of lookahead or fork to disambiguate,
  /// prefer peeking unless k is too large or the ambiguity is resolved outside
  /// the goal non-terminal
  match calculate_k(gb, reduces.clone(), shifts.clone(), MAX_EVAL_K) {
    KCalcResults::K(k) => {
      if !gb.config.ALLOW_PEEKING {
        if gb.config.ALLOW_FORKING {
          return Ok(ShiftReduceConflictResolution::Fork);
        }
        peek_not_allowed_error(
          gb,
          &[shifts.cloned().collect(), reduces.cloned().collect()],
          &format!("Either peeking or forking must be enabled to resolve this ambiguity, which requires a lookahead of k={k}"),
        )?;
      } else if k > gb.config.max_k {
        peek_not_allowed_error(gb, &[shifts.cloned().collect(), reduces.cloned().collect()], "")?;
      }
      gb.set_classification(ParserClassification { peeks_present: true, max_k: k as u16, ..Default::default() });

      Ok(ShiftReduceConflictResolution::Peek(k as u16))
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
          &format!(
            "Either peeking or forking must be enabled to resolve ambiguity, which requires a lookahead of k>={k} to resolve"
          ),
        )?;
      }

      gb.set_classification(ParserClassification { peeks_present: true, ..Default::default() });

      Ok(ShiftReduceConflictResolution::Peek(k as u16))
    }
  }
}

enum KCalcResults {
  /// Return the k if it is less or equal to then the maximum eval k
  K(usize),
  /// The peek paths contains recursive items, which will requires infinite time
  /// and space to resolve.
  RecursiveAt(usize),
  /// The peek paths are looking promising, but k is going to be more than the
  /// max eval limit.
  LargerThanMaxLimit(usize),
}

fn get_follow_artifacts<'db>(
  i_reduce: &Vec<Item<'db>>,
  gb: &mut GraphBuilder<'db>,
) -> (OrderedSet<Item<'db>>, OrderedSet<DBTermKey>) {
  let mut out = OrderedSet::default();
  let mut syms = OrderedSet::default();
  for item in i_reduce {
    let (follow, default) = get_follow(gb, *item);

    let items = follow.iter().flat_map(|i| i.closure_iter_align(*i));

    syms.extend(items.clone().filter_map(|i| i.term_index_at_sym(gb.get_mode())));

    out.extend(items);
  }
  (out, syms)
}

fn calculate_k<'a, 'db: 'a, A: TransitionPairRefIter<'a, 'db> + Clone, B: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &mut GraphBuilder<'db>,
  reduces: B,
  shifts: A,
  max_eval: usize,
) -> KCalcResults {
  // Find max K for the conflicts to determine if we should us the peek mechanism

  let mut reduce_items: ItemSet = reduces.clone().to_next().cloned().collect();
  let mut shift_items: ItemSet = shifts.clone().to_next().cloned().collect();

  for k in 2..=(max_eval) {
    let (out, reduce_syms) = get_follow_artifacts(&reduce_items.try_increment(), gb);
    reduce_items = out;

    let (out, shift_syms) = get_follow_artifacts(&shift_items.try_increment(), gb);
    shift_items = out;

    if reduce_syms.intersection(&shift_syms).next().is_none() {
      // Have to consider items outside the domain of the goal-nonterminal, so we'll
      // have to a strategy other than peek to this conflict.
      return KCalcResults::K(k);
    }

    if shift_items.iter().filter(|i| i.rule_is_recursive()).any(|i| reduce_items.contains(i)) {
      return KCalcResults::RecursiveAt(k);
    }
  }

  KCalcResults::LargerThanMaxLimit(max_eval)
}

fn calculate_k_multi<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &mut GraphBuilder<'db>,
  transition_groups: Vec<T>,
  max_eval: usize,
) -> KCalcResults {
  let mut groups = transition_groups.into_iter().map(|i| i.to_next().cloned().collect::<ItemSet>()).collect::<Vec<_>>();

  // Find max K for the conflicts to determine if we should us the peek mechanism

  for k in 2..=(max_eval) {
    let mut queue = groups.iter().map(|items| get_follow_artifacts(&items.try_increment(), gb)).collect::<Vec<_>>();

    let sym_table = queue.iter().flat_map(|(_, syms)| syms.iter()).fold(OrderedMap::<DBTermKey, u32>::new(), |mut map, sym| {
      (*map.entry(*sym).or_default()) += 1;
      map
    });

    if !sym_table.iter().any(|i| *i.1 > 1) {
      return KCalcResults::K(k);
    }

    let recursive = queue
      .iter()
      .flat_map(|((items, _))| items.iter())
      .filter_map(|i| -> Option<usize> { i.rule_is_recursive().then_some(i.rule_id().into()) })
      .fold(OrderedMap::<usize, u32>::new(), |mut map, sym| {
        (*map.entry(sym).or_default()) += 1;
        map
      });

    if recursive.iter().any(|i| *i.1 > 1) {
      return KCalcResults::RecursiveAt(k);
    }

    let sym_table = sym_table.into_iter().filter_map(|(sym, i)| (i > 1).then_some(sym)).collect::<OrderedSet<_>>();

    groups =
      queue.into_iter().filter_map(|(follow, syms)| syms.intersection(&sym_table).next().is_some().then_some(follow)).collect();

    if groups.len() < 2 {
      return KCalcResults::K(k);
    }
  }

  KCalcResults::LargerThanMaxLimit(max_eval)
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
    let mut _completed: Option<&Lookaheads> = None;

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
