#![allow(unused)]

use super::{
  super::{
    build::{GroupedFirsts, TransitionGroup},
    graph::*,
  },
  create_call,
  create_peek,
  handle_completed_item,
  resolve_conflicting_tokens,
  resolve_reduce_reduce_conflict,
  resolve_shift_reduce_conflict,
  CreateCallResult,
  ReduceReduceConflictResolution,
  ShiftReduceConflictResolution,
};
use crate::{
  compile::build_graph::{
    build::handle_completed_groups,
    errors::{conflicting_symbols_error, lr_disabled_error, peek_not_allowed_error},
    items::{get_follow, get_follow_symbols},
  },
  parser::Shift,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};

use GraphBuildState::*;

fn create_out_of_scope_complete_state<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(
  gb: &mut GraphBuilder<'db>,
  out_of_scope: T,
  sym: PrecedentSymbol,
) {
  if let Some(i) = out_of_scope.clone().next() {
    let transition_type = match (i.kernel.origin, gb.is_scanner()) {
      (_, true) => StateType::ScannerCompleteOOS,
      _ => StateType::NonTermCompleteOOS,
    };
    gb.create_state(Normal, sym, transition_type, Some(out_of_scope.to_kernel().cloned())).to_leaf();
  }
}

pub(crate) fn handle_regular_incomplete_items<'db>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let ____allow_rd____: bool = gb.config.ALLOW_RECURSIVE_DESCENT || ____is_scan____;
  let ____allow_lr____: bool = gb.config.ALLOW_LR || ____is_scan____;
  let ____allow_fork____: bool = gb.config.ALLOW_FORKING && false;
  let ____allow_peek____: bool = gb.config.ALLOW_PEEKING;

  let out_of_scope = group.iter().out_scope();
  let in_scope = group.iter().in_scope();

  match (in_scope.clone().count(), out_of_scope.clone().count()) {
    (0, 1..) => {
      create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
    }
    (_, 1..) if ____is_scan____ => {
      create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
    }
    (1.., 1..) => {
      if !____allow_peek____ {
        peek_not_allowed_error(gb, &[out_of_scope.cloned().collect(), in_scope.cloned().collect()], "")?;
      } else {
        let pending_state = create_peek(gb, prec_sym, group.iter(), None, true, StateType::Peek)?;
        gb.add_pending(pending_state);
      }
    }
    (len, _) => {
      if let Some(CreateCallResult { is_kernel, state_id, _transition_items }) =
        ____allow_rd____.then(|| create_call(gb, in_scope.clone(), prec_sym)).flatten()
      {
        if is_kernel {
          gb.enqueue_state(state_id);
        } else {
          if !____allow_lr____ {
            return lr_disabled_error(gb, _transition_items);
          }
          gb.add_pending(state_id);
        }
      } else {
        // If can't create call, do LR shift, or peek, or warn about k=1 conflicts.
        if group.iter().all(|p| p.is_kernel_terminal()) {
          gb.create_state(Normal, prec_sym, StateType::KernelShift, Some(group.iter().to_kernel().try_increment().into_iter()))
            .to_enqueued();
        } else if len == 1 {
          let items = in_scope.clone().to_next().try_increment();
          let kernel_item = in_scope.clone().to_kernel().next().unwrap();

          if !____allow_lr____ {
            if let Some(non_term) = kernel_item.nonterm_index_at_sym(gb.get_mode()) {
              // Check for left recursion. If present, the grammar is not LL
              if gb.db.nonterm_recursion_type(non_term).is_left_recursive() {
                return lr_disabled_error(gb, in_scope.to_kernel().cloned().collect());
              } else {
                let state = gb.create_state(Normal, prec_sym, StateType::Shift, Some(items.into_iter())).to_enqueued().unwrap();
                let mut nterm_shift_state = gb.create_state(
                  Normal,
                  (kernel_item.sym(), 0).into(),
                  StateType::ShiftFrom(state),
                  Some([kernel_item.try_increment()].into_iter()),
                );
                //nterm_shift_state.set_parent(state);
                nterm_shift_state.to_enqueued();
              }
            } else {
              gb.create_state(Normal, prec_sym, StateType::Shift, Some(items.into_iter())).to_pending();
            }
          } else {
            gb.create_state(Normal, prec_sym, StateType::Shift, Some(items.into_iter())).to_pending();
          }
        } else if ____allow_lr____ {
          let items = in_scope.to_next().try_increment();

          gb.set_classification(ParserClassification { bottom_up: true, ..Default::default() });

          gb.create_state(Normal, prec_sym, StateType::Shift, Some(items.into_iter())).to_pending();
        } else {
          /// If forking is allowed then use that. Other wise this grammar is no
          /// LL and LR and/or RD has been disabled.
          if ____allow_fork____ {
            return Err(SherpaError::Text("Forking has not been implemented".into()));
          } else if !____allow_lr____ {
            return lr_disabled_error(gb, in_scope.to_kernel().cloned().collect());
          } else {
            return Err(SherpaError::Text(
              "Not sure what happened, but this parser cannot be built with the current configuration".into(),
            ));
          }
        }
      }
    }
    _ => unreachable!(),
  }
  Ok(())
}

fn single_shift_allowed<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(in_scope: &T, gb: &mut GraphBuilder<'_>) -> bool {
  let ____allow_lr____: bool = gb.config.ALLOW_LR || gb.is_scanner();

  if ____allow_lr____ {
    gb.set_classification(ParserClassification { bottom_up: true, ..Default::default() });
  } else if let Some(non_term) = in_scope.clone().to_kernel().next().and_then(|i| i.nonterm_index_at_sym(gb.get_mode())) {
    // Check for left recursion. If present, the grammar is not LL
    if gb.db.nonterm_recursion_type(non_term).is_left_recursive() {
      return false;
    }
  }

  true
}

pub(crate) fn handle_regular_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  shift_groups: &mut GroupedFirsts<'db>,
  prec_sym: PrecedentSymbol,
  mut lookahead_pairs: Lookaheads<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let ____allow_peek____ = gb.config.ALLOW_PEEKING;
  let mut cmpl = lookahead_pairs.iter().to_next().to_vec();
  let sym = prec_sym.sym();
  // Non-Peeking States
  match (lookahead_pairs.len(), shift_groups.remove(&prec_sym.sym())) {
    (1, None) => {
      handle_completed_item(gb, lookahead_pairs, prec_sym)?;
    }
    (2.., None) => {
      // `k` must be > 0 to handle reduce reduce conflicts.
      gb.set_classification(ParserClassification { max_k: 1, ..Default::default() });

      if ____is_scan____ {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(gb, sym, lookahead_pairs.iter())?;
      } else if prec_sym.sym() == SymbolId::Default {
        if lookahead_pairs.iter().to_kernel().items_are_the_same_rule() {
          handle_completed_item(gb, lookahead_pairs, prec_sym)?;
        } else {
          todo!("(anthony) Handle default completed item conflicts (e.i. kernel goal items)")
        }
      } else if lookahead_pairs.iter().to_kernel().to_absolute::<ItemSet>().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(gb, lookahead_pairs, prec_sym)?;
      } else if lookahead_pairs.iter().all(|p| p.is_out_of_scope()) {
        let item: Item<'_> = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(gb, vec![(item, item, gb.get_mode()).into()], prec_sym)?;
      } else {
        match resolve_reduce_reduce_conflict(gb, prec_sym, lookahead_pairs)? {
          ReduceReduceConflictResolution::Nothing => {}
          ReduceReduceConflictResolution::Fork(_) => {
            gb.set_classification(ParserClassification { forks_present: true, ..Default::default() });
          }
          ReduceReduceConflictResolution::Reduce(item) => todo!("Handle reduce result from reduce-reduce conflict resolution"),
          ReduceReduceConflictResolution::Peek(max_k, follow_pairs) => {
            gb.set_classification(ParserClassification { peeks_present: true, max_k, ..Default::default() });
            let state = create_peek(gb, prec_sym, [].iter(), Some(follow_pairs.iter()), true, StateType::Peek)?;
            gb.add_pending(state);
          }
        }
      }
    }
    (_, Some((prec, mut group))) => {
      // `k` must be > 0 to handle shift reduce conflicts.
      gb.set_classification(ParserClassification { max_k: 1, ..Default::default() });

      if ____is_scan____ {
        let item: Item<'_> = cmpl[0];
        lookahead_pairs.extend(group);
        handle_completed_item(gb, lookahead_pairs, prec_sym)?;
      } else if group.iter().all(|i| i.is_out_of_scope()) && lookahead_pairs.iter().all(|i| i.is_out_of_scope()) {
        create_out_of_scope_complete_state(gb, lookahead_pairs.iter(), prec_sym);
      } else {
        match resolve_shift_reduce_conflict(gb, group.iter(), lookahead_pairs.iter())? {
          ShiftReduceConflictResolution::Shift => {
            shift_groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, &mut Default::default(), sym, lookahead_pairs)?;
          }
          ShiftReduceConflictResolution::Peek(max_k) => {
            gb.set_classification(ParserClassification { peeks_present: true, max_k, ..Default::default() });

            let state = create_peek(gb, prec_sym, group.iter(), Some(lookahead_pairs.iter()), true, StateType::Peek)?;
            gb.add_pending(state);
          }
          ShiftReduceConflictResolution::Fork => {
            gb.set_classification(ParserClassification { forks_present: true, ..Default::default() });

            return Err(SherpaError::Text("todo(anthony): Not ready for forking yet".into()));
          }
        }
      }
    }
    (_len, _collide) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: {:?} len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
        gb.current_state_id().state(),
        sym.debug_string(gb.db),
        cmpl.to_debug_string("\n"),
        gb.graph()._debug_string_()
      );
      #[cfg(not(debug_assertions))]
      unimplemented!()
    }
  }
  Ok(())
}
