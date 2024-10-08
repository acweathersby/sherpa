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
  compile::states::build_graph::{
    build::handle_completed_groups,
    errors::{lr_disabled_error, peek_not_allowed_error},
    graph::StateType,
    items::{get_follow_internal, FollowType},
    stack_vec::StackVec,
  },
  parser::Shift,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};

use radlr_rust_runtime::kernel;
use ParserClassification as PC;

fn create_out_of_scope_complete_state<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  out_of_scope: T,
  sym: PrecedentSymbol,
) {
  if let Some(i) = out_of_scope.clone().next() {
    let transition_type = match (i.kernel.origin, pred.is_scanner()) {
      (_, true) => StateType::ScannerCompleteOOS,
      _ => StateType::NonTermCompleteOOS,
    };

    StagedNode::new(gb)
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
  pred: &SharedGraphNode,
  config: &ParserConfig,
  prec_sym: PrecedentSymbol,
  (prec, group): TransitionGroup,
  classification: ParserClassification,
) -> RadlrResult<()> {
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
      // Don't create any states
    }
    (1.., 1..) => {
      if !____allow_peek____ {
        peek_not_allowed_error(gb, &[out_of_scope.cloned().collect(), in_scope.cloned().collect()], "")?;
      } else {
        create_peek(gb, pred, config, prec_sym, group.iter(), None)?.include_with_goto_state().commit(gb);
      }
    }
    (len, _) => {
      if ____is_scan____ && in_scope.clone().all(|i| i.kernel.origin.is_out_of_scope()) {
        return Ok(());
      }

      if let Some(CreateCallResult { is_kernel, node, _transition_items }) =
        ____allow_rd____.then(|| create_call(gb, pred, config, in_scope.clone(), prec_sym)).flatten()
      {
        if is_kernel {
          node.to_classification(PC { calls_present: true, ..classification }).commit(gb);
        } else {
          if !____allow_lr____ {
            return lr_disabled_error(gb, pred, _transition_items);
          }
          node
            .to_classification(PC { bottom_up: true, calls_present: true, ..classification })
            .include_with_goto_state()
            .commit(gb);
        }
      } else if group.iter().all(|p| p.is_kernel_terminal()) {
        StagedNode::new(gb)
          .parent(pred.clone())
          .sym(prec_sym)
          .ty(StateType::KernelShift)
          .to_classification(classification)
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

              let firsts = ItemSet::start_items(
                kernel_item.nonterm_index_at_sym(pred.graph_type(), &db).expect("should be a nonterminal"),
                &db,
              );

              let kernel_items = firsts.iter().map(|a| a.as_from_index(kernel_item.index));

              StagedNode::new(gb)
                .parent(pred.clone())
                .sym(prec_sym)
                .ty(StateType::KernelLLCall)
                .to_classification(classification)
                .pnc(
                  Box::new(move |s: &std::sync::Arc<GraphNode>, builder, _| {
                    vec![StagedNode::new(builder)
                      .parent(p)
                      .sym((kernel_item.sym_id(builder.db()), 0).into())
                      .ty(StateType::LLShiftFrom(s.id()))
                      .kernel_items([kernel_item.try_increment()].into_iter())]
                  }),
                  PostNodeConstructorData::None,
                )
                .kernel_items(kernel_items)
                .commit(gb)
            }
          } else {
            StagedNode::new(gb)
              .parent(pred.clone())
              .sym(prec_sym)
              .ty(StateType::Shift)
              .to_classification(classification)
              .kernel_items(items.into_iter())
              .include_with_goto_state()
              .commit(gb);
          }
        } else {
          StagedNode::new(gb)
            .sym(prec_sym)
            .parent(pred.clone())
            .ty(StateType::Shift)
            .to_classification(classification)
            .kernel_items(items.into_iter())
            .include_with_goto_state()
            .commit(gb);
        }
      } else if ____allow_lr____ {
        let items = in_scope.to_next().try_increment();

        StagedNode::new(gb)
          .parent(pred.clone())
          .sym(prec_sym)
          .ty(StateType::Shift)
          .kernel_items(items.into_iter())
          .include_with_goto_state()
          .to_classification(ParserClassification { bottom_up: true, ..classification })
          .commit(gb);
      } else {
        /// If forking is allowed then use that. Otherwise, this grammar is not
        /// LL, and LR and/or RD has been disabled.
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
    _ => unreachable!(),
  }
  Ok(())
}

pub(crate) fn handle_regular_complete_groups(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  config: &ParserConfig,
  shift_groups: &mut GroupedFirsts,
  prec_sym: PrecedentSymbol,
  mut lookahead_pairs: Lookaheads,
) -> RadlrResult<()> {
  let ____is_scan____ = pred.is_scanner();
  let ____allow_peek____ = config.ALLOW_PEEKING;
  let mut cmpl = lookahead_pairs.iter().to_next().to_vec();
  let sym = prec_sym.sym();
  let mut classification = ParserClassification { max_k: 1, ..Default::default() };
  // Non-Peeking States
  match (lookahead_pairs.len(), shift_groups.remove(&prec_sym.sym())) {
    (1, None) => {
      handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
    }
    (2.., None) => {
      if ____is_scan____ {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(gb, pred, config, sym, lookahead_pairs.iter())?;
      } else if prec_sym.sym() == SymbolId::Default {
        if lookahead_pairs.iter().to_kernel().items_are_the_same_rule() {
          handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
        } else {
          create_peek(gb, pred, config, prec_sym, [].iter(), Some(lookahead_pairs.iter()))?.include_with_goto_state().commit(gb);
        }
      } else if lookahead_pairs.iter().to_kernel().indices().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
      } else if lookahead_pairs.iter().all(|p| p.is_out_of_scope()) {
        let item: Item = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(gb, pred, config, vec![(item, item, pred.graph_type(), gb.db()).into()], prec_sym)?;
      } else {
        match resolve_reduce_reduce_conflict(gb, pred, config, prec_sym, lookahead_pairs)? {
          ReduceReduceConflictResolution::Nothing => {}
          ReduceReduceConflictResolution::Fork(lookahead_pairs) => {
            create_fork(gb, pred, config, prec_sym, lookahead_pairs.iter().map(|i| i.kernel))?
              .to_classification(classification | ParserClassification { forks_present: true, ..Default::default() })
              .include_with_goto_state()
              .commit(gb);
          }
          ReduceReduceConflictResolution::Reduce(item) => {
            todo!("Reduce result from a reduce-reduce  resolution")
          }
          ReduceReduceConflictResolution::Peek(max_k, follow_pairs) => {
            create_peek(gb, pred, config, prec_sym, [].iter(), Some(follow_pairs.iter()))?.include_with_goto_state().commit(gb);
          }
        }
      }
    }
    (_, Some((prec, mut group))) => {
      if ____is_scan____ {
        let item: Item = cmpl[0];
        lookahead_pairs.extend(group);
        handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
      } else if group.iter().all(|i| i.is_out_of_scope()) && lookahead_pairs.iter().all(|i| i.is_out_of_scope()) {
        create_out_of_scope_complete_state(gb, pred, lookahead_pairs.iter(), prec_sym);
      } else {
        match resolve_shift_reduce_conflict(gb, pred, config, group.iter(), lookahead_pairs.iter())? {
          ShiftReduceConflictResolution::Shift => {
            shift_groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, pred, config, &mut Default::default(), sym, lookahead_pairs)?;
          }
          ShiftReduceConflictResolution::Peek(max_k) => {
            create_peek(gb, pred, config, prec_sym, group.iter(), Some(lookahead_pairs.iter()))?
              .to_classification(classification)
              .include_with_goto_state()
              .commit(gb);
          }
          ShiftReduceConflictResolution::Fork => {
            create_fork(gb, pred, config, prec_sym, lookahead_pairs.into_iter().chain(group.into_iter()).map(|i| i.kernel))?
              .to_classification(classification | ParserClassification { forks_present: true, ..Default::default() })
              .include_with_goto_state()
              .commit(gb);
          }
        }
      }
    }
    (_len, _collide) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
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
