#![allow(unused)]

use super::{
  super::{
    build::{GroupedFirsts, TransitionGroup},
    graph::*,
  },
  create_call,
  create_fork,
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
  compile::states::{
    build_graph::{
      graph::{GraphBuildState, StateType},
      items::{get_follow, get_follow_symbols},
    },
    build_graph_beta::{
      build::handle_completed_groups,
      errors::{lr_disabled_error, peek_not_allowed_error},
    },
  },
  parser::Shift,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};

fn create_out_of_scope_complete_state<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &GraphNodeShared,
  out_of_scope: T,
  sym: PrecedentSymbol,
) {
  if let Some(i) = out_of_scope.clone().next() {
    let transition_type = match (i.kernel.origin, pred.is_scanner()) {
      (_, true) => StateType::ScannerCompleteOOS,
      _ => StateType::NonTermCompleteOOS,
    };

    StagedNode::new()
      .parent(pred.clone())
      .sym(sym)
      .ty(transition_type)
      .kernel_items(out_of_scope.to_kernel().cloned())
      .make_leaf()
      .commit(gb);
  }
}

pub(crate) fn handle_regular_incomplete_items(
  gb: &mut ConcurrentGraphBuilder,
  pred: &GraphNodeShared,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup,
) -> RadlrResult<()> {
  let config = gb.config();
  let ____is_scan____ = pred.is_scanner();
  let ____allow_rd____: bool = config.ALLOW_CALLS || ____is_scan____;
  let ____allow_lr____: bool = config.ALLOW_LR || ____is_scan____;
  let ____allow_fork____: bool = config.ALLOW_CONTEXT_SPLITTING && false;
  let ____allow_peek____: bool = config.ALLOW_PEEKING;
  let db = &gb.db_rc();

  let out_of_scope = group.iter().out_scope();
  let in_scope = group.iter().in_scope();

  match (in_scope.clone().count(), out_of_scope.clone().count()) {
    (0, 1..) => {
      create_out_of_scope_complete_state(gb, pred, out_of_scope, prec_sym);
    }
    (_, 1..) if ____is_scan____ => {
      create_out_of_scope_complete_state(gb, pred, out_of_scope, prec_sym);
    }
    (1.., 1..) => {
      if !____allow_peek____ {
        peek_not_allowed_error(gb, &[out_of_scope.cloned().collect(), in_scope.cloned().collect()], "")?;
      } else {
        create_peek(gb, pred, prec_sym, group.iter(), None)?;
      }
    }
    (len, _) => {
      if let Some(CreateCallResult { is_kernel, node, _transition_items }) =
        ____allow_rd____.then(|| create_call(gb, pred, in_scope.clone(), prec_sym)).flatten()
      {
        if is_kernel {
          node.commit(gb);
        } else {
          if !____allow_lr____ {
            return lr_disabled_error(gb, pred, _transition_items);
          }
          node.commit(gb);
        }
      } else {
        // If can't create call, do LR shift, or peek, or warn about k=1 conflicts.
        if group.iter().all(|p| p.is_kernel_terminal()) {
          StagedNode::new()
            .parent(pred.clone())
            .sym(prec_sym)
            .build_state(GraphBuildState::Normal)
            .ty(StateType::KernelShift)
            .kernel_items(group.iter().to_kernel().try_increment().into_iter())
            .commit(gb);
        } else if len == 1 {
          let items = in_scope.clone().to_next().try_increment();
          let kernel_item = in_scope.clone().to_kernel().next().unwrap();

          if !____allow_lr____ {
            if let Some(non_term) = kernel_item.nonterm_index_at_sym(pred.graph_type(), db) {
              // Check for left recursion. If present, the grammar is not LL
              if gb.db().nonterm_recursion_type(non_term).is_left_recursive() {
                return lr_disabled_error(gb, pred, in_scope.to_kernel().cloned().collect());
              } else {
                let p = pred.clone();
                let kernel_item = kernel_item.clone();
                StagedNode::new()
                  .parent(pred.clone())
                  .sym(prec_sym)
                  .build_state(GraphBuildState::Normal)
                  .ty(StateType::Shift)
                  .pnc(
                    Box::new(move |s: &std::sync::Arc<GraphNode>, builder, _| {
                      vec![StagedNode::new()
                        .parent(p)
                        .sym((kernel_item.sym_id(builder.db()), 0).into())
                        .build_state(GraphBuildState::Normal)
                        .ty(StateType::ShiftFrom(s.id()))
                        .kernel_items([kernel_item.try_increment()].into_iter())]
                    }),
                    PostNodeConstructorData::None,
                  )
                  .kernel_items(items.into_iter())
                  .commit(gb)
              }
            } else {
              StagedNode::new()
                .parent(pred.clone())
                .sym(prec_sym)
                .build_state(GraphBuildState::Normal)
                .ty(StateType::Shift)
                .kernel_items(items.into_iter())
                .goto_inc()
                .commit(gb);
            }
          } else {
            StagedNode::new()
              .sym(prec_sym)
              .build_state(GraphBuildState::Normal)
              .parent(pred.clone())
              .ty(StateType::Shift)
              .kernel_items(items.into_iter())
              .goto_inc()
              .commit(gb);
          }
        } else if ____allow_lr____ {
          let items = in_scope.to_next().try_increment();

          StagedNode::new()
            .parent(pred.clone())
            .sym(prec_sym)
            .build_state(GraphBuildState::Normal)
            .ty(StateType::Shift)
            .kernel_items(items.into_iter())
            .goto_inc()
            .commit(gb);
        } else {
          /// If forking is allowed then use that. Other wise this grammar is no
          /// LL and LR and/or RD has been disabled.
          if ____allow_fork____ {
            return Err(RadlrError::Text("Forking has not been implemented".into()));
          } else if !____allow_lr____ {
            return lr_disabled_error(gb, pred, in_scope.to_kernel().cloned().collect());
          } else {
            return Err(RadlrError::Text(
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

fn single_shift_allowed<'a, T: TransitionPairRefIter<'a> + Clone>(
  in_scope: &T,
  gb: &mut ConcurrentGraphBuilder,
  node: &GraphNodeShared,
) -> bool {
  let ____allow_lr____: bool = gb.config().ALLOW_LR || node.is_scanner();

  if ____allow_lr____ {
  } else if let Some(non_term) =
    in_scope.clone().to_kernel().next().and_then(|i| i.nonterm_index_at_sym(node.graph_type(), gb.db()))
  {
    // Check for left recursion. If present, the grammar is not LL
    if gb.db().nonterm_recursion_type(non_term).is_left_recursive() {
      return false;
    }
  }

  true
}

pub(crate) fn handle_regular_complete_groups(
  gb: &mut ConcurrentGraphBuilder,
  pred: &GraphNodeShared,
  shift_groups: &mut GroupedFirsts,
  prec_sym: PrecedentSymbol,
  mut lookahead_pairs: Lookaheads,
) -> RadlrResult<()> {
  let ____is_scan____ = pred.is_scanner();
  let ____allow_peek____ = gb.config().ALLOW_PEEKING;
  let mut cmpl = lookahead_pairs.iter().to_next().to_vec();
  let sym = prec_sym.sym();
  // Non-Peeking States
  match (lookahead_pairs.len(), shift_groups.remove(&prec_sym.sym())) {
    (1, None) => {
      handle_completed_item(gb, pred, lookahead_pairs, prec_sym)?;
    }
    (2.., None) => {
      if ____is_scan____ {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(gb, pred, sym, lookahead_pairs.iter())?;
      } else if prec_sym.sym() == SymbolId::Default {
        if lookahead_pairs.iter().to_kernel().items_are_the_same_rule() {
          handle_completed_item(gb, pred, lookahead_pairs, prec_sym)?;
        } else {
          todo!("(anthony) Handle default completed item conflicts (e.i. kernel goal items)")
        }
      } else if lookahead_pairs.iter().to_kernel().indices().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(gb, pred, lookahead_pairs, prec_sym)?;
      } else if lookahead_pairs.iter().all(|p| p.is_out_of_scope()) {
        let item: Item = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(gb, pred, vec![(item, item, pred.graph_type(), gb.db()).into()], prec_sym)?;
      } else {
        match resolve_reduce_reduce_conflict(gb, pred, prec_sym, lookahead_pairs)? {
          ReduceReduceConflictResolution::Nothing => {}
          ReduceReduceConflictResolution::Fork(lookahead_pairs) => {
            create_fork(gb, pred, prec_sym, lookahead_pairs.iter().map(|i| i.kernel))?.goto_inc().commit(gb);
          }
          ReduceReduceConflictResolution::Reduce(item) => todo!("Handle reduce result from reduce-reduce conflict resolution"),
          ReduceReduceConflictResolution::Peek(max_k, follow_pairs) => {
            create_peek(gb, pred, prec_sym, [].iter(), Some(follow_pairs.iter()))?.commit(gb);
          }
        }
      }
    }
    (_, Some((prec, mut group))) => {
      if ____is_scan____ {
        let item: Item = cmpl[0];
        lookahead_pairs.extend(group);
        handle_completed_item(gb, pred, lookahead_pairs, prec_sym)?;
      } else if group.iter().all(|i| i.is_out_of_scope()) && lookahead_pairs.iter().all(|i| i.is_out_of_scope()) {
        create_out_of_scope_complete_state(gb, pred, lookahead_pairs.iter(), prec_sym);
      } else {
        match resolve_shift_reduce_conflict(gb, pred, group.iter(), lookahead_pairs.iter())? {
          ShiftReduceConflictResolution::Shift => {
            shift_groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, pred, &mut Default::default(), sym, lookahead_pairs)?;
          }
          ShiftReduceConflictResolution::Peek(max_k) => {
            create_peek(gb, pred, prec_sym, group.iter(), Some(lookahead_pairs.iter()))?.commit(gb);
          }
          ShiftReduceConflictResolution::Fork => {
            create_fork(gb, pred, prec_sym, lookahead_pairs.into_iter().chain(group.into_iter()).map(|i| i.kernel))?
              .goto_inc()
              .commit(gb);
          }
        }
      }
    }
    (_len, _collide) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: {:?} len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
        pred.build_state(),
        sym.debug_string(gb.db()),
        cmpl.to_debug_string(gb.db(), "\n"),
        ""
      );
      #[cfg(not(debug_assertions))]
      unimplemented!()
    }
  }
  Ok(())
}
