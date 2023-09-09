#![allow(unused)]

use super::{
  super::{
    build::{TransitionGroup, TransitionGroups},
    graph::*,
  },
  create_call,
  create_peek,
  handle_completed_item,
};
use crate::{
  compile::build_graph::{build::handle_completed_groups, errors::conflicting_symbols_error},
  types::*,
  utils::hash_group_btreemap,
};

use GraphState::*;

enum ShiftReduceConflictResolution {
  Shift,
  Reduce,
  Peek,
}

enum ReduceReduceConflictResolution<'db> {
  Reduce(Item<'db>),
  BreadCrumb(OrderedSet<FollowPair<'db>>),
  Peek(OrderedSet<FollowPair<'db>>),
  Nothing,
}

fn resolve_reduce_reduce_conflict<'db>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: OrderedSet<FollowPair<'db>>,
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
  A: Clone + ItemRefContainerIter<'a, 'db>,
  B: Clone + ItemRefContainerIter<'a, 'db>,
>(
  iter: &GraphBuilder<'db>,
  incom_items: A,
  comp1_items: B,
) -> ShiftReduceConflictResolution {
  let incom_nterm_set = incom_items.clone().rule_nonterm_ids();
  let compl_nterm_set = comp1_items.clone().map(|i| i.decrement().unwrap()).nonterm_ids_at_index();
  let nterm_sets_are_equal = incom_nterm_set.len() == 1 && incom_nterm_set.is_superset(&compl_nterm_set);

  if nterm_sets_are_equal && {
    // If all the shift items reduce to the same nterm and all completed items where
    // completed after shifting the same nterm, then the precedence of the shift
    // items determines whether we should shift first or reduce.
    let compl_prec = comp1_items.clone().get_max_precedence(iter.get_mode());
    let incom_prec = incom_items.clone().get_max_precedence(iter.get_mode());

    incom_prec >= compl_prec
  } {
    ShiftReduceConflictResolution::Shift
  } else if nterm_sets_are_equal {
    ShiftReduceConflictResolution::Reduce
  } else {
    ShiftReduceConflictResolution::Peek
  }
}

fn resolve_conflicting_tokens<'db, 'follow>(
  gb: &mut GraphBuilder<'db>,
  sym: SymbolId,
  completed_items: ItemSet<'db>,
) -> SherpaResult<()> {
  // Map items according to their symbols
  let token_precedence_groups =
    hash_group_btreemap(completed_items.clone(), |_, i| (i.origin_precedence(), i.origin.get_symbol(gb.db)));

  let base_precedence_groups = hash_group_btreemap(token_precedence_groups, |_, ((_, sym), _)| sym.base_token_precedence());

  if let Some((_, groups)) = base_precedence_groups.into_iter().rev().next() {
    let mut _completed: Option<&ItemSet> = None;

    if groups.len() == 1 {
      _completed = Some(groups.values().next().unwrap());
    } else if let Some((_, sub_group)) = groups.iter().rev().next() {
      if sub_group.len() == 1 {
        _completed = Some(&sub_group);
      }
    }

    if let Some(completed_items) = _completed {
      let cmpl_pair = (*(o_to_r(completed_items.first(), "")?), completed_items.clone().to_vec());
      handle_completed_item(gb, cmpl_pair, (sym, 0).into())
    } else {
      Err(conflicting_symbols_error(gb.graph(), groups))
    }
  } else {
    Ok(())
  }
}

fn create_out_of_scope_complete_state<'db, 'follow>(gb: &mut GraphBuilder<'db>, out_of_scope: Items<'db>, sym: PrecedentSymbol) {
  let transition_type = match (out_of_scope[0].origin, gb.is_scanner()) {
    (_, true) => StateType::ScannerCompleteOOS,
    _ => StateType::NonTermCompleteOOS,
  };
  gb.create_state(Normal, sym, transition_type, out_of_scope).to_leaf();
}

pub(crate) fn handle_regular_incomplete_items<'nt_set, 'db: 'nt_set>(
  gb: &mut GraphBuilder<'db>,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let out_of_scope = group.clone().outscope_items().to_vec();
  let in_scope = group.clone().inscope_items();
  match (in_scope.len(), out_of_scope.len()) {
    (0, 1..) => {
      create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
    }
    (_, 1..) if ____is_scan____ => {
      create_out_of_scope_complete_state(gb, out_of_scope, prec_sym);
    }
    (1.., 1..) => {
      let pending_state = create_peek(
        gb,
        prec_sym,
        &group.iter(),
        None::<std::collections::btree_set::Iter<'_, FollowPair<'_>>>,
        true,
        StateType::Peek,
      )?;
      gb.add_pending(pending_state);
    }
    (1.., _) => {
      if let Some((pending_state, _)) =
        (gb.config.ALLOW_RECURSIVE_DESCENT_CALLS || gb.is_scanner()).then(|| create_call(gb, in_scope.iter(), prec_sym)).flatten()
      {
        gb.add_pending(pending_state);
      } else {
        let prec_sym = (prec_sym.sym(), in_scope.iter().get_max_token_precedence()).into();
        gb.create_state(Normal, prec_sym, StateType::Shift, in_scope.try_increment()).to_pending();
      }
    }
    _ => unreachable!(),
  }
  Ok(())
}

pub(crate) fn handle_regular_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut TransitionGroups<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let mut cmpl = follow_pairs.iter().to_completed_vec();
  let sym = prec_sym.sym();
  // Non-Peeking States
  match (follow_pairs.len(), groups.remove(&prec_sym.sym())) {
    (1, None) => {
      handle_completed_item(gb, (cmpl[0], cmpl), prec_sym)?;
    }
    (2.., None) => {
      if ____is_scan____ {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(gb, sym, cmpl.to_set())?;
      } else if cmpl.iter().to_absolute::<ItemSet>().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(gb, (cmpl[0], vec![cmpl[0]]), prec_sym)?;
      } else if cmpl.iter().all_are_out_of_scope() {
        // We are at the end of a lookahead that results in the completion of
        // some existing item.
        let item: Item<'_> = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(gb, (item, vec![item]), prec_sym)?;
      } else {
        if default_only_items.len() > 0 {
          todo!("(anthony) Handle default only completed items (e.i. kernel goal items)")
        }

        match resolve_reduce_reduce_conflict(gb, prec_sym, follow_pairs) {
          ReduceReduceConflictResolution::Nothing => {}
          ReduceReduceConflictResolution::BreadCrumb(_) => {}
          ReduceReduceConflictResolution::Reduce(item) => todo!("Handle reduce result from reduce-reduce conflict resolution"),
          ReduceReduceConflictResolution::Peek(follow_pairs) => {
            let state = create_peek(gb, prec_sym, &[].iter(), Some(follow_pairs.iter()), true, StateType::Peek)?;
            gb.add_pending(state);
          }
        }
      }
    }
    (_, Some((prec, group))) => {
      if ____is_scan____ {
        let mut group = group.to_vec();
        let item = cmpl[0];
        group.append(&mut cmpl);
        handle_completed_item(gb, (item, group), prec_sym)?;
      } else if group.iter().all_are_out_of_scope() && cmpl.iter().all_are_out_of_scope() {
        cmpl.append(&mut group.to_vec());
        create_out_of_scope_complete_state(gb, cmpl, prec_sym);
      } else {
        // WE can use precedence to resolve shift reduce conflicts.  For now favor
        // shift.

        match resolve_shift_reduce_conflict(gb, group.iter(), cmpl.iter()) {
          ShiftReduceConflictResolution::Shift => {
            groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, &mut Default::default(), sym, follow_pairs, &cmpl.to_set())?;
          }
          ShiftReduceConflictResolution::Peek => {
            let state = create_peek(gb, prec_sym, &group.iter(), Some(follow_pairs.iter()), true, StateType::Peek)?;
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
        gb.graph().debug_string()
      );
      #[cfg(not(debug_assertions))]
      unimplemented!()
    }
  }
  Ok(())
}
