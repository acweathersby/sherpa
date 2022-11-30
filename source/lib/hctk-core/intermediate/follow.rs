use crate::types::{TransitionGraph as TPack, *};
use std::{
  collections::{BTreeSet, VecDeque},
  vec,
};

/// Retrieve items following the reduction of the `root_completed_item`. This is similar to an [Earley
/// parser](https://en.wikipedia.org/wiki/Earley_parser)'s complete action, where completed productions are
/// matched against items in previous states to find new items to process.
pub(super) fn get_follow_items(
  t: &TPack,
  root_completed_item: &Item,
  prev_state_ref: MaybeNodeId,
) -> FollowItemGroups {
  let mut seen = BTreeSet::<LinkedItem>::new();
  let mut out = BTreeSet::<LinkedItem>::new();
  let mut fin_items = BTreeSet::<LinkedItem>::new();
  let grammar = t.g.clone();
  let g = &grammar;

  static empty_vec: Vec<Item> = Vec::new();
  // Starting at the top we grab the closure to the nearest
  // non-term link.

  // Stores the end item [1] and its immediate closure item [0]
  let mut completed_items = VecDeque::from_iter(vec![
    (LinkedItem { item: *root_completed_item, closure_node: prev_state_ref }),
  ]);
  while let Some(linked) = completed_items.pop_front() {
    let completed_item = linked.item.clone();
    let closure_node = linked.closure_node;

    if seen.insert(linked.clone()) {
      let prod = linked.item.get_prod_id(g);
      // Grab all productions from the closure that match the end item's
      // production.
      match {
        let (iter, prev_state) = match linked {
          LinkedItem { closure_node: Some(prev_node), .. } => {
            // Use the prev_node
            let node = t.get_node(prev_node);
            (node.goto_items.closure_with_state(&t.g).to_vec().into_iter(), node.closure_parent)
          }
          LinkedItem { item, closure_node: None, .. } => (
            if item.is_out_of_scope() {
              t.out_of_scope_closure.as_ref().unwrap_or(&empty_vec).clone().into_iter()
            } else {
              vec![].into_iter()
            },
            None,
          ),
        };
        let items = iter
          .clone()
          .filter(|i| linked.item.in_same_lane(i) && i.get_production_id_at_sym(&t.g) == prod)
          .collect::<Vec<_>>();

        (completed_item, items, prev_state, closure_node)
      } {
        (completed_item, empty, Some(prev_state), _) if empty.is_empty() => {
          completed_items
            .push_back(LinkedItem { item: completed_item, closure_node: Some(prev_state) });
        }
        (completed_item, empty, None, _) if empty.is_empty() => {
          fin_items.insert(LinkedItem { item: completed_item, closure_node: None });
        }
        (completed_item, prod_items, prev_state, current_state) => {
          let prod_items = prod_items.try_increment().to_state(completed_item.get_state());

          let have_non_end = prod_items.iter().any(|i| !i.completed());

          for item in prod_items {
            if item.completed() {
              if !have_non_end {
                completed_items.push_back(LinkedItem { item, closure_node: current_state });
                completed_items.push_back(LinkedItem { item, closure_node: prev_state });
              }
            } else {
              out.insert(LinkedItem { item, closure_node: current_state });
            }
          }
        }
      }
    }
  }

  FollowItemGroups {
    completed_items:   fin_items.into_iter().collect(),
    uncompleted_items: out.into_iter().collect(),
  }
}
