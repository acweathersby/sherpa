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
  let ____allow_ra____: bool = gb.config.ALLOW_LR || ____is_scan____;
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
          if !____allow_ra____ {
            lr_disabled_error(gb, _transition_items)?;
          }
          gb.add_pending(state_id);
        }
      } else {
        // If can't create call, do LR shift, or peek, or warn about k=1 conflicts.
        if group.iter().all(|p| p.is_kernel_terminal()) {
          gb.create_state(Normal, prec_sym, StateType::KernelShift, Some(group.iter().to_kernel().try_increment().into_iter()))
            .to_enqueued();
        } else if ____allow_ra____ || len == 1 {
          let items = in_scope.to_next().try_increment();

          gb.set_classification(ParserClassification { bottom_up: true, ..Default::default() });

          gb.create_state(Normal, prec_sym, StateType::Shift, Some(items.into_iter())).to_pending();
        } else {
          if len > 1 {
            if !____allow_ra____ {
              lr_disabled_error(gb, in_scope.to_next().cloned().collect())?;
            } else if !____allow_fork____ {
              todo!("Create warning about k=1 conflicts: enable fork")
            }
          } else if !____allow_ra____ {
            lr_disabled_error(gb, in_scope.to_next().cloned().collect())?;
          } else {
            todo!("Warn about no strategy for handling items")
          }
        }
      }
    }
    _ => unreachable!(),
  }
  Ok(())
}

pub(crate) fn handle_regular_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  prec_sym: PrecedentSymbol,
  mut follow_pairs: Follows<'db>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let ____allow_peek____ = gb.config.ALLOW_PEEKING;
  let mut cmpl = follow_pairs.iter().to_next().to_vec();
  let sym = prec_sym.sym();
  // Non-Peeking States
  match (follow_pairs.len(), groups.remove(&prec_sym.sym())) {
    (1, None) => {
      handle_completed_item(gb, follow_pairs, prec_sym)?;
    }
    (2.., None) => {
      if ____is_scan____ {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(gb, sym, follow_pairs.iter())?;
      } else if follow_pairs.iter().to_kernel().to_absolute::<ItemSet>().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(gb, follow_pairs, prec_sym)?;
      } else if follow_pairs.iter().all(|p| p.is_out_of_scope()) {
        // We are at the end of a lookahead that results in the completion of
        // some existing item.
        let item: Item<'_> = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(gb, vec![(item, item, gb.get_mode()).into()], prec_sym)?;
      } else {
        if default_only_items.len() > 0 {
          todo!("(anthony) Handle default only completed items (e.i. kernel goal items)")
        }

        match resolve_reduce_reduce_conflict(gb, prec_sym, follow_pairs)? {
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
      if ____is_scan____ {
        let item: Item<'_> = cmpl[0];
        follow_pairs.extend(group);
        handle_completed_item(gb, follow_pairs, prec_sym)?;
      } else if group.iter().all(|i| i.is_out_of_scope()) && follow_pairs.iter().all(|i| i.is_out_of_scope()) {
        create_out_of_scope_complete_state(gb, follow_pairs.iter(), prec_sym);
      } else {
        // WE can use precedence to resolve shift reduce conflicts.  For now favor
        // shift.
        match resolve_shift_reduce_conflict(gb, group.iter(), follow_pairs.iter())? {
          ShiftReduceConflictResolution::Shift => {
            groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, &mut Default::default(), sym, follow_pairs, &cmpl.to_set())?;
          }
          ShiftReduceConflictResolution::Peek(max_k) => {
            gb.set_classification(ParserClassification { peeks_present: true, max_k, ..Default::default() });

            let state = create_peek(gb, prec_sym, group.iter(), Some(follow_pairs.iter()), true, StateType::Peek)?;
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
