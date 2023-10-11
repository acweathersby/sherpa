#![allow(unused)]

use std::{clone, collections::VecDeque};

use rayon::collections::btree_set;

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
  utils::{create_u64_hash, hash_group_btree_iter, hash_group_btreemap},
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
          if gb.config.ALLOW_CONTEXT_SPLITTING {
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

  if incom_prec > compl_prec {
    return Ok(ShiftReduceConflictResolution::Shift);
  } else if incom_prec < compl_prec {
    return Ok(ShiftReduceConflictResolution::Reduce);
  };

  let oos = reduces.clone().any(|r| r.next.is_oos());

  // If all the shift items reduce to the same nterm and all completed items where
  // completed after shifting the same nterm, then the precedence of the shift
  // items determines whether we should shift first or reduce.

  let incom_nterm_set: OrderedSet<DBNonTermKey> = shifts.clone().to_kernel().rule_nonterm_ids();
  let compl_next_nterm_set: OrderedSet<DBNonTermKey> = reduces.clone().to_next().rule_nonterm_ids();

  if incom_nterm_set.len() == 1 && incom_nterm_set.is_superset(&compl_next_nterm_set) {
    return if incom_prec >= compl_prec {
      Ok(ShiftReduceConflictResolution::Shift)
    } else {
      Ok(ShiftReduceConflictResolution::Reduce)
    };
  }

  /*   let symbols = shifts.clone().chain(reduces.clone()).map(|i| i.kernel.decrement().map(|i| i.sym_id())).collect::<Set<_>>();

  match (symbols.len(), symbols.into_iter().next().flatten()) {
    (1, Some(sym)) => {
      if !sym.is_term() {
        // All items shifted on the same non-terminal
        let shifted_left_recursion =
          shifts.clone().all(|i| i.kernel.rule_is_left_recursive(gb.get_mode()) && i.kernel.sym_index() == 1);
        let completed_are_goals = reduces.clone().all(|i| gb.graph().item_is_goal(&i.kernel));

        shifts.clone()._debug_print_("GREEDY-TRANSITION");
        reduces.clone()._debug_print_("GREEDY-LOOKAHEADS");
        if shifted_left_recursion
        /* && completed_are_goals */
        {
          // Greedy left recursion
          return Ok(ShiftReduceConflictResolution::Shift);
        }
      }
    }
    _ => {}
  } */

  /// This conflict requires some form of lookahead or fork to disambiguate,
  /// prefer peeking unless k is too large or the ambiguity is resolved outside
  /// the goal non-terminal
  match calculate_k(gb, reduces.clone(), shifts.clone(), MAX_EVAL_K) {
    KCalcResults::K(k) => {
      gb.set_classification(ParserClassification { peeks_present: true, max_k: k as u16, ..Default::default() });
      if !gb.config.ALLOW_PEEKING {
        if gb.config.ALLOW_CONTEXT_SPLITTING {
          Ok(ShiftReduceConflictResolution::Fork)
        } else {
          peek_not_allowed_error(
            gb,
            &[shifts.cloned().collect(), reduces.cloned().collect()],
            &format!("Either peeking or forking must be enabled to resolve this ambiguity, which requires a lookahead of k={k}"),
          )
        }
      } else if k > gb.config.max_k {
        peek_not_allowed_error(gb, &[shifts.cloned().collect(), reduces.cloned().collect()], "")
      } else {
        Ok(ShiftReduceConflictResolution::Peek(k as u16))
      }
    }
    KCalcResults::RecursiveAt(k) => {
      gb.set_classification(ParserClassification { forks_present: true, ..Default::default() });
      if gb.config.ALLOW_CONTEXT_SPLITTING {
        Ok(ShiftReduceConflictResolution::Fork)
      } else {
        // Non-term is invalid. If this is a root entry state then we can't construct a
        // parser for it. Otherwise, we can mark the parser for this non-term as
        // invalid, And then proceed to "collapse" our overall parser, if the LR parsing
        // is enabled.
        //
        // This involves the following:
        // let A = the non-terminal whose peek has failed.
        //
        // Dropping all the states of A; this will results in a parser that is not
        // enterable at A. Then, mark all exported non-terminals whose root
        // rules contains an A in the right side as "LR only", preventing states
        // with call type transitions from being created. Rebuild the states for
        // effected non-terminals. Repeat this process for any non-terminal, marked as
        // "LR only", that fails to generate states due to undeterministic peek.
        //
        // If during this iterative process a non-terminal is encountered that is "LR
        // only", is non-deterministic during peeking, and is a root
        // entry point for the parser, then we have failed to generate a minimum
        // acceptable parser for this grammar configuration. Report parser as failed
        // and produce relevant diagnostic messages.
        //
        // If LR style parsing is disabled, then we cannot perform this process. Report
        // parser as failed and produce relevant diagnostic messages.
        //
        peek_not_allowed_error(
          gb,
          &[shifts.cloned().collect(), reduces.cloned().collect()],
          &format!("This is undeterministic at k>={k}"),
        )
      }
    }
    KCalcResults::LargerThanMaxLimit(k) => {
      gb.set_classification(ParserClassification { peeks_present: true, ..Default::default() });
      if !gb.config.ALLOW_PEEKING {
        if gb.config.ALLOW_CONTEXT_SPLITTING {
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

      Ok(ShiftReduceConflictResolution::Peek(k as u16))
    }
    KCalcResults::Unpeekable => {
      if gb.config.ALLOW_CONTEXT_SPLITTING {
        return Ok(ShiftReduceConflictResolution::Fork);
      } else {
        peek_not_allowed_error(
          gb,
          &[shifts.cloned().collect(), reduces.cloned().collect()],
          &format!("Forking must be enabled to resolve ambiguity"),
        )
      }
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
  /// One or more items resolve to goal symbol without further means of
  /// disambiguation using peek states.
  Unpeekable,
}

fn get_conflict_follow_artifacts<'db>(
  i_reduce: &Vec<Item<'db>>,
  gb: &mut GraphBuilder<'db>,
) -> (OrderedSet<Item<'db>>, OrderedSet<DBTermKey>) {
  let mut out = OrderedSet::default();
  let mut syms = OrderedSet::default();
  let mut item_filter = Set::new();
  for item in i_reduce {
    let (follow, default) = get_follow(gb, *item);

    let items = follow.iter().flat_map(|i| i.closure_iter_align(*i));
    for item in items {
      if item_filter.insert(item.index) || !item.is_oos() {
        if let Some(sym) = item.term_index_at_sym(gb.get_mode()) {
          out.insert(item);
          syms.insert(sym);
        }
      }
    }
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

  let mut conflict_sets = Set::<_>::new();

  for k in 2..=(max_eval) {
    let (out, reduce_syms) = get_conflict_follow_artifacts(&reduce_items.try_increment(), gb);
    reduce_items = out;

    let (out, shift_syms) = get_conflict_follow_artifacts(&shift_items.try_increment(), gb);
    shift_items = out;

    if reduce_syms.len() == 0 || shift_syms.len() == 0 {
      // Ran out of symbols to compare, this indicates that we have reached goal
      // non-terminal and there are no further actions possible.
      return KCalcResults::Unpeekable;
    }

    let shift_groups = hash_group_btreemap(shift_items, |_, i| i.term_index_at_sym(gb.get_mode()).unwrap());
    let mut reduce_groups = hash_group_btreemap(reduce_items, |_, i| i.term_index_at_sym(gb.get_mode()).unwrap());

    let (s, r) = shift_groups
      .into_iter()
      .filter_map(|(sym, group)| reduce_groups.remove(&sym).map(|g| (group, g)))
      .unzip::<_, _, Vec<_>, Vec<_>>();

    if s.is_empty() {
      return KCalcResults::K(k);
    }

    shift_items = s.into_iter().flatten().collect();
    reduce_items = r.into_iter().flatten().collect();

    if !conflict_sets
      .insert(create_u64_hash(reduce_items.iter().chain(shift_items.iter()).map(|i| i.index).collect::<OrderedSet<_>>()))
    {
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
    let mut queue = groups.iter().map(|items| get_conflict_follow_artifacts(&items.try_increment(), gb)).collect::<Vec<_>>();

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

pub fn resolve_token_assign_id<I: Iterator<Item = DBTermKey>>(set_token: I, db: &ParserDatabase) -> Option<DBTermKey> {
  let token_set: std::collections::BTreeSet<DBTermKey> = set_token.collect::<OrderedSet<_>>();
  if token_set.len() == 1 {
    Some(token_set.into_iter().next().unwrap())
  } else if token_set.len() > 1 {
    if let Some((_, groups)) = hash_group_btree_iter::<Vec<_>, _, _, _, _>(
      token_set.clone().into_iter().map(|key| (key, db.token(key))),
      |_, (key, tok)| tok.sym_id.conflict_precedence(),
    )
    .into_iter()
    .rev()
    .next()
    {
      if groups.len() == 1 {
        Some(groups.into_iter().next().unwrap().0)
      } else {
        panic!("can't resolve symbols");
      }
    } else {
      panic!("can't resolve symbols");
    }
  } else {
    None
  }
}
