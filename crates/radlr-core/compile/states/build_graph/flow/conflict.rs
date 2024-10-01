#![allow(unused)]

use std::{clone, collections::VecDeque};

use super::{
  super::{
    build::{GroupedFirsts, TransitionGroup},
    graph::*,
  },
  handle_completed_item,
  CreateCallResult,
};
use crate::{
  compile::states::{
    build_graph,
    build_graph::{
      build::handle_completed_groups,
      errors::{conflicting_symbols_error, peek_not_allowed_error},
      items::{get_follow, get_follow_internal, FollowType},
      stack_vec::StackVec,
    },
    build_states::StateConstructionError,
  },
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

const MAX_EVAL_K_RR: usize = 8;
const MAX_EVAL_K_SR: usize = 64;

pub(super) enum ReduceReduceConflictResolution {
  Reduce(Item),
  Fork(Lookaheads),
  Peek(u16, Lookaheads),
  Nothing,
}

pub(super) fn resolve_reduce_reduce_conflict(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  prec_sym: PrecedentSymbol,
  follow_pairs: Lookaheads,
) -> RadlrResult<ReduceReduceConflictResolution> {
  if prec_sym.sym().is_default() {
    Ok(ReduceReduceConflictResolution::Nothing)
  } else {
    match calculate_k_multi(gb, node, follow_pairs.iter().map(|i| [i].into_iter()).collect(), MAX_EVAL_K_RR) {
      KCalcResults::K(k) => {
        if !config.ALLOW_PEEKING || k >= MAX_EVAL_K_RR {
          if config.ALLOW_CONTEXT_SPLITTING {
            return Ok(ReduceReduceConflictResolution::Fork(follow_pairs));
          }
          return peek_not_allowed_error(
            gb,
            follow_pairs.into_iter().map(|i| vec![i]).collect::<Vec<_>>().as_slice(),
            &format!("Either peeking or forking must be enabled to resolve this ambiguity, which requires a lookahead of k={k}"),
          );
        } else if k as u32 > config.max_k {
          return peek_not_allowed_error(
            gb,
            follow_pairs.into_iter().map(|i| vec![i]).collect::<Vec<_>>().as_slice(),
            &format!(
              "A lookahead of k={k} is required, but lookahead cannot be greater than k={} with the current configuration",
              config.max_k
            ),
          );
        }

        Ok(ReduceReduceConflictResolution::Peek(k as u16, follow_pairs))
      }
      _ => {
        return Ok(ReduceReduceConflictResolution::Peek(100 as u16, follow_pairs));
        #[cfg(debug_assertions)]
        {
          for (i, follow_pair) in follow_pairs.iter().enumerate() {
            let mut item = follow_pair.kernel;
            println!("-------------- \n\n");

            loop {
              println!("{}", item._debug_string_w_db_(&gb.db()));
              let origin_state_id = item.origin_state;

              if let Some(origin_state) = gb.get_state(origin_state_id.0 as u64) {
                let kernel_items = origin_state.kernel_items();

                if item.is_initial() && origin_state_id.is_root() {
                  break;
                }

                if let Some(i) = kernel_items.iter().find(|i| item.is_successor_of(i)) {
                  item = *i;
                } else {
                  break;
                }
              } else {
                break;
              }
            }

            println!("{i} {}", follow_pair._debug_string_(&gb.db()));
          }
          todo!("Resolve lookahead conflicts: \n prec_sym{prec_sym:#?}  \nfollow_pairs: {follow_pairs:#?}\n\n");
        }
        todo!("Resolve lookahead conflicts: Use debug build to print a more verbose error message.");
      }
    }
  }
}

pub(super) fn resolve_shift_reduce_conflict<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  shifts: T,
  reduces: T,
) -> RadlrResult<ShiftReduceConflictResolution> {
  let db = gb.db();
  let mode = node.graph_type();

  let compl_prec = reduces.clone().map(|i| i.kernel.decrement().unwrap().precedence(mode, db)).max().unwrap_or_default();
  let incom_prec = shifts.clone().max_precedence();

  if incom_prec > compl_prec {
    return Ok(ShiftReduceConflictResolution::Shift);
  } else if incom_prec < compl_prec {
    return Ok(ShiftReduceConflictResolution::Reduce);
  };

  // If all the shift items reduce to the same nterm and all completed items where
  // completed after shifting the same nterm, then the precedence of the shift
  // items determines whether we should shift first or reduce.

  let incom_nterm_set: OrderedSet<DBNonTermKey> = shifts.clone().to_kernel().rule_nonterm_ids(db);
  let compl_next_nterm_set: OrderedSet<DBNonTermKey> = reduces.clone().to_next().rule_nonterm_ids(db);

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
  match calculate_k(gb, node, reduces.clone(), shifts.clone(), MAX_EVAL_K_SR) {
    KCalcResults::K(k) => {
      if !config.ALLOW_PEEKING {
        if config.ALLOW_CONTEXT_SPLITTING {
          Ok(ShiftReduceConflictResolution::Fork)
        } else {
          peek_not_allowed_error(
            gb,
            &[shifts.cloned().collect(), reduces.cloned().collect()],
            &format!("Either peeking or forking must be enabled to resolve this ambiguity, which requires a lookahead of k={k}"),
          )
        }
      } else if k as u32 > config.max_k {
        peek_not_allowed_error(gb, &[shifts.cloned().collect(), reduces.cloned().collect()], "")
      } else {
        Ok(ShiftReduceConflictResolution::Peek(k as u16))
      }
    }
    KCalcResults::RecursiveAt(k) => {
      if config.ALLOW_CONTEXT_SPLITTING {
        Ok(ShiftReduceConflictResolution::Fork)
      } else {
        gb.declare_recursive_peek_error();
        Err(RadlrError::StateConstructionError(StateConstructionError::NonDeterministicPeek(
          node.get_root_shared(),
          Box::new(
            peek_not_allowed_error::<()>(
              gb,
              &[shifts.cloned().collect(), reduces.cloned().collect()],
              &format!("This is undeterministic at k>={k}"),
            )
            .err()
            .unwrap(),
          ),
        )))
      }
    }
    KCalcResults::LargerThanMaxLimit(k) => {
      if !config.ALLOW_PEEKING {
        if config.ALLOW_CONTEXT_SPLITTING {
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
      if config.ALLOW_CONTEXT_SPLITTING {
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

fn get_conflict_follow_artifacts(
  i_reduce: &Vec<Item>,
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
) -> (OrderedSet<Item>, OrderedSet<DBTermKey>) {
  let mut out = OrderedSet::default();
  let mut syms = OrderedSet::default();
  let mut item_filter = Set::new();
  for item in i_reduce {
    let mut follow = StackVec::<512, _>::new();
    let mut _default: StackVec<512, Item> = StackVec::<512, _>::new();

    get_follow_internal(gb, node, *item, FollowType::AllItems, &mut follow, &mut _default);

    for item in follow.as_slice() {
      for item in item.closure_iter_align(*item, gb.db()) {
        if item_filter.insert(item.index) || !item.is_oos() {
          if let Some(sym) = item.term_index_at_sym(node.graph_type(), gb.db()) {
            out.insert(item);
            syms.insert(sym);
          }
        }
      }
    }
  }

  (out, syms)
}

fn calculate_k<'a, A: TransitionPairRefIter<'a> + Clone, B: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  reduces: B,
  shifts: A,
  max_eval: usize,
) -> KCalcResults {
  // Find max K for the conflicts to determine if we should us the peek mechanism
  let db = &gb.db_rc();
  let mut reduce_items: ItemSet = reduces.clone().to_next().cloned().collect();
  let mut shift_items: ItemSet = shifts.clone().to_next().cloned().collect();

  let mut conflict_sets = Set::<_>::new();

  for k in 2..=(max_eval) {
    let (out, reduce_syms) = get_conflict_follow_artifacts(&reduce_items.try_increment(), gb, node);
    reduce_items = out;

    let (out, shift_syms) = get_conflict_follow_artifacts(&shift_items.try_increment(), gb, node);
    shift_items = out;

    if reduce_syms.len() == 0 || shift_syms.len() == 0 {
      // Ran out of symbols to compare, this indicates that we have reached goal
      // non-terminal and there are no further actions possible.
      return KCalcResults::Unpeekable;
    }

    let shift_groups = hash_group_btreemap(shift_items, |_, i| i.term_index_at_sym(node.graph_type(), db).unwrap());
    let mut reduce_groups = hash_group_btreemap(reduce_items, |_, i| i.term_index_at_sym(node.graph_type(), db).unwrap());

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

fn calculate_k_multi<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  transition_groups: Vec<T>,
  max_eval: usize,
) -> KCalcResults {
  let db = &gb.db_rc();
  let mut groups = transition_groups.into_iter().map(|i| i.to_next().cloned().collect::<ItemSet>()).collect::<Vec<_>>();

  // Find max K for the conflicts to determine if we should us the peek mechanism

  for k in 2..=(max_eval) {
    let mut queue =
      groups.iter().map(|items| get_conflict_follow_artifacts(&items.try_increment(), gb, node)).collect::<Vec<_>>();

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
      .filter_map(|i| -> Option<usize> { i.rule_is_recursive(db).then_some(i.rule_id().into()) })
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

pub(crate) fn resolve_conflicting_tokens<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  sym: SymbolId,
  completed: T,
) -> RadlrResult<()> {
  // Map items according to their symbols
  let token_precedence_groups = hash_group_btree_iter::<Vec<_>, _, _, _, _>(completed.clone(), |_, i| {
    (i.kernel.origin_precedence(), i.kernel.origin.get_symbol(gb.db()))
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
      handle_completed_item(gb, node, config, completed_items.clone(), (sym, 0).into())
    } else {
      Err(conflicting_symbols_error(gb, node, groups))
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
