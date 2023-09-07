use super::{
  graph::*,
  items::{all_items_come_from_same_nonterminal_call, all_items_transition_on_same_nonterminal},
};
use crate::types::*;

use GraphState::*;

pub(super) fn create_call<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db> + Clone>(
  iter: &mut GraphBuilder<'db>,
  group: T,
  sym: PrecedentSymbol,
) -> Option<(StateId, Vec<Item<'db>>)> {
  let Some(first) = group.clone().next() else { return None };
  let db = iter.graph().get_db();

  let call_data = if all_items_come_from_same_nonterminal_call(group.clone()) {
    let nterm = first.nonterm_index();

    if !matches!(db.nonterm_recursion_type(nterm), RecursionType::LeftRecursive | RecursionType::LeftRightRecursive) {
      let Ok(items) = iter.current_state().get_closure_ref().map(|i| {
        i.iter()
          .filter(|i| match i.nontermlike_index_at_sym() {
            Some(id) => id == nterm && i.nonterm_index() != nterm,
            _ => false,
          })
          .cloned()
          .collect::<Vec<_>>()
      }) else {
        return None;
      };

      // There may be a superior candidate. evaluate that.
      if let Some(pending_state) = create_call(iter, items.iter(), sym) {
        return Some(pending_state);
      }

      Some((items, nterm))
    } else {
      None
    }
  } else {
    None
  };

  create_call_internal(iter, call_data, sym)
}

pub(super) fn create_call_internal<'db>(
  gb: &mut GraphBuilder<'db>,
  call_pack: Option<(Vec<Item<'db>>, DBNonTermKey)>,
  sym: PrecedentSymbol,
) -> Option<(StateId, Vec<Item<'db>>)> {
  match call_pack {
    Some((items, nterm_to_call)) if items.len() > 0 => {
      use StateType::*;

      let state_type = if gb.is_scanner() {
        let kernel_items = gb.current_state().kernel_items_ref();
        items.iter().all(|i| kernel_items.contains(i)).then_some(KernelCall(nterm_to_call)).unwrap_or(InternalCall(nterm_to_call))
      } else {
        InternalCall(nterm_to_call)
      };
      Some((gb.create_state(Normal, sym, state_type, items.iter().try_increment()).to_state(), items))
    }
    _ => None,
  }
}

pub(super) fn create_kernel_call<'a, 'db: 'a>(gb: &mut GraphBuilder<'db>, sym: PrecedentSymbol) -> Option<()> {
  let group = gb.current_state().kernel_items_ref().clone();
  let group = group.iter();
  let Some(first) = group.clone().next() else { return None };
  let db = gb.graph().get_db();

  if let Some((state, _)) = create_call_internal(
    gb,
    if all_items_transition_on_same_nonterminal(group.clone()) {
      let nterm = first.nonterm_index_at_sym()?;
      if !matches!(db.nonterm_recursion_type(nterm), RecursionType::LeftRecursive | RecursionType::LeftRightRecursive) {
        Some((group.to_vec(), nterm))
      } else {
        None
      }
    } else {
      None
    },
    sym,
  ) {
    gb.enqueue_pending_state(state);
    Some(())
  } else {
    None
  }
}
