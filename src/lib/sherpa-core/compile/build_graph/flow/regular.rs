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
    errors::{conflicting_symbols_error, lr_disabled_error},
  },
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};

use GraphBuildState::*;

enum ShiftReduceConflictResolution {
  Shift,
  Reduce,
  Peek,
}

enum ReduceReduceConflictResolution<'db> {
  Reduce(Item<'db>),
  BreadCrumb(Follows<'db>),
  Peek(Follows<'db>),
  Nothing,
}

fn resolve_reduce_reduce_conflict<'db>(
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

fn resolve_shift_reduce_conflict<
  'a,
  'db: 'a,
  A: TransitionPairRefIter<'a, 'db> + Clone,
  B: TransitionPairRefIter<'a, 'db> + Clone,
>(
  gb: &GraphBuilder<'db>,
  shifts: A,
  reduces: B,
) -> ShiftReduceConflictResolution {
  let incom_nterm_set = shifts.clone().to_kernel().rule_nonterm_ids();
  let compl_nterm_set = reduces.clone().map(|i| i.kernel.decrement().unwrap()).nonterm_ids_at_index(gb.get_mode());
  let nterm_sets_are_equal = incom_nterm_set.len() == 1 && incom_nterm_set.is_superset(&compl_nterm_set);

  if nterm_sets_are_equal && {
    // If all the shift items reduce to the same nterm and all completed items where
    // completed after shifting the same nterm, then the precedence of the shift
    // items determines whether we should shift first or reduce.
    let compl_prec = reduces.clone().map(|i| i.kernel.decrement().unwrap().precedence(gb.get_mode())).max().unwrap_or_default();
    let incom_prec = shifts.max_precedence();

    incom_prec >= compl_prec
  } {
    ShiftReduceConflictResolution::Shift
  } else if nterm_sets_are_equal {
    ShiftReduceConflictResolution::Reduce
  } else {
    ShiftReduceConflictResolution::Peek
  }
}

fn resolve_conflicting_tokens<'a, 'db: 'a, T: TransitionPairRefIter<'a, 'db> + Clone>(
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
    gb.create_state(Normal, sym, transition_type, out_of_scope.to_kernel().cloned().collect()).to_leaf();
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
  let mut in_scope = group.iter().in_scope();

  match (in_scope.clone().count(), out_of_scope.clone().count()) {
    (0, 1..) => {
      create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
    }
    (_, 1..) if ____is_scan____ => {
      create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
    }
    (1.., 1..) => {
      let pending_state = create_peek(gb, prec_sym, group.iter(), None, true, StateType::Peek)?;
      gb.add_pending(pending_state);
    }
    (len, _) => {
      if let Some(CreateCallResult { is_kernel, state_id, _transition_items }) =
        ____allow_rd____.then(|| create_call(gb, in_scope.clone(), prec_sym)).flatten()
      {
        if !____allow_ra____ {
          lr_disabled_error(gb, _transition_items)?;
        }
        gb.add_pending(state_id);
      } else {
        // If can't create call, do LR shift, or peek, or warn about k=1 conflicts.
        if group.iter().all(|p| p.is_kernel_terminal()) {
          gb.create_state(Normal, prec_sym, StateType::KernelShift, group.iter().to_kernel().try_increment()).to_pending();
        } else if ____allow_ra____ {
          let items = in_scope.to_inherited(gb.state_id()).iter().to_next().try_increment();
          gb.create_state(Normal, prec_sym, StateType::Shift, items).to_pending();
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

        match resolve_reduce_reduce_conflict(gb, prec_sym, follow_pairs) {
          ReduceReduceConflictResolution::Nothing => {}
          ReduceReduceConflictResolution::BreadCrumb(_) => {}
          ReduceReduceConflictResolution::Reduce(item) => todo!("Handle reduce result from reduce-reduce conflict resolution"),
          ReduceReduceConflictResolution::Peek(follow_pairs) => {
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
        match resolve_shift_reduce_conflict(gb, group.iter(), follow_pairs.iter()) {
          ShiftReduceConflictResolution::Shift => {
            groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, &mut Default::default(), sym, follow_pairs, &cmpl.to_set())?;
          }
          ShiftReduceConflictResolution::Peek => {
            let state = create_peek(gb, prec_sym, group.iter(), Some(follow_pairs.iter()), true, StateType::Peek)?;
            gb.add_pending(state);
          }
        }
      }
    }
    (_len, _collide) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: {:?} len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
        gb.state_id().state(),
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
