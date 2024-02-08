use super::super::graph::*;
use crate::{
  compile::states::build_graph::graph::{GraphBuildState, StateType},
  types::*,
};

pub struct CreateCallResult {
  /// `true` if the state is a KernelCall
  pub is_kernel:         bool,
  /// The new state that will perform the call
  pub node:              StagedNode,
  /// A list of items from the parent closure that transition on the called
  /// non-terminal.
  pub _transition_items: Items,
}

/// Attempts to make a "call" states, which jumps to the root of another
/// non-terminal parse graph. Returns an optional tuple:
/// (
///   is_ker
/// )
pub(crate) fn create_call<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  group: T,
  sym: PrecedentSymbol,
) -> Option<CreateCallResult> {
  let db = gb.db();
  let ____is_scan____ = node.is_scanner();
  let ____allow_rd____: bool = config.ALLOW_CALLS || ____is_scan____;
  let ____allow_ra____: bool = config.ALLOW_LR || ____is_scan____;

  if
  /* TODO(anthony) remove this after scan peek is implemented >>> */
  ____is_scan____ /* <<< */ || !____allow_rd____ || group.clone().any(|i| i.is_kernel_terminal()) {
    return None;
  };

  // if all kernels are on same nonterminal symbol then we can do a call, provided
  // the nonterminal is not left recursive.

  let kernel_symbol = group.clone().kernel_nonterm_sym(node.graph_type(), db);
  if kernel_symbol.len() == 1 {
    if let Some(Some(nonterm)) = kernel_symbol.first() {
      match db.nonterm_recursion_type(*nonterm) {
        RecursionType::LeftRecursive | RecursionType::LeftRightRecursive => {
          // Can't make a call on a left recursive non-terminal.
        }
        _ => {
          // Create call on the kernel items.
          let items = group.to_kernel().to_vec();

          return Some(CreateCallResult {
            is_kernel:         true,
            node:              StagedNode::new(gb)
              .build_state(GraphBuildState::Normal)
              .parent(node.clone())
              .sym(sym)
              .ty(StateType::KernelCall(*nonterm))
              .kernel_items(items.try_increment().iter().cloned()),
            _transition_items: items,
          });
        }
      }
    }
  }

  // We'll need to climb the closure graph to find the highest mutual non-terminal
  // that is not left recursive. This is only allowed if the system allows LR
  if !____allow_ra____ {
    return None;
  };

  if let Some((nonterm, items)) = climb_nonterms(gb, node, group) {
    return Some(CreateCallResult {
      is_kernel:         false,
      node:              StagedNode::new(gb)
        .build_state(GraphBuildState::Normal)
        .parent(node.clone())
        .sym(sym)
        .ty(StateType::InternalCall(nonterm))
        .to_classification(ParserClassification { calls_present: true, ..Default::default() })
        .kernel_items(items.try_increment().iter().cloned()),
      _transition_items: items,
    });
  } else {
    None
  }
}

fn climb_nonterms<'a, T: TransitionPairRefIter<'a> + Clone>(
  gb: &mut ConcurrentGraphBuilder,
  node: &GraphNode,
  group: T,
) -> Option<(DBNonTermKey, Vec<Item>)> {
  let db = gb.db();

  if all_items_come_from_same_nonterminal_call(group.clone(), db) {
    let nterm = unsafe { group.clone().next().unwrap_unchecked() }.next.nonterm_index(db);

    if matches!(db.nonterm_recursion_type(nterm), RecursionType::LeftRecursive | RecursionType::LeftRightRecursive) {
      return None;
    };

    let climbed_firsts = group
      .clone()
      .flat_map(|p| {
        p.kernel
          .closure_iter(db)
          .filter(|i| match i.nonterm_index_at_sym(node.graph_type(), db) {
            Some(id) => id == nterm && i.nonterm_index(db) != nterm,
            _ => false,
          })
          .map(|i| -> TransitionPair { (p.kernel, i.align(&p.next), node.graph_type(), db).into() })
      })
      .collect::<Vec<_>>();

    // There may be a superior candidate. evaluate that.
    if let Some(candidate) = climb_nonterms(gb, node, climbed_firsts.iter()) {
      return Some(candidate);
    }

    Some((nterm, climbed_firsts.iter().to_next().cloned().collect()))
  } else {
    None
  }
}

pub(super) fn all_items_come_from_same_nonterminal_call<'a, T: TransitionPairRefIter<'a> + Clone>(
  group: T,
  db: &ParserDatabase,
) -> bool {
  group.clone().all(|i| i.next.is_initial()) && group.map(|i| i.next.nonterm_index(db)).collect::<Set<_>>().len() == 1
}
