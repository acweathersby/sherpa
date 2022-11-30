use crate::types::{TransitionGraph as TPack, *};
use std::{
  collections::{BTreeSet, VecDeque},
  vec,
};

/// Retrieve items following the reduction of the `root_completed_item`. This is similar to an [Earley
/// parser](https://en.wikipedia.org/wiki/Earley_parser)'s complete action, where completed productions are
/// matched against items in previous states to find new items to process.
///
/// `lane_counter` - this is an internal monotonic value that is used to generate new lane identifiers when needed.
/// this value should at least equal `root_completed_item.state.curr_lane + 1` to ensure lanes do not overlap.
pub(super) fn get_follow_items(
  t: &mut TPack,
  root_completed_item: &Item,
  prev_state_ref: MaybeNodeId,
  mut lane_counter: u32,
) -> (FollowItemGroups, u32) {
  let mut seen = BTreeSet::<(ItemState, LinkedItem)>::new();
  let mut out = BTreeSet::<LinkedItem>::new();
  let mut fin_items = BTreeSet::<LinkedItem>::new();
  let mut intermediate = BTreeSet::<LinkedItem>::new();
  let grammar = t.g.clone();
  let g = &grammar;

  println!("\n\n---- Follow start on {} ----", root_completed_item.debug_string(g));

  lane_counter = lane_counter.max(1);

  static empty_vec: Vec<Item> = Vec::new();
  // Starting at the top we grab the closure to the nearest
  // non-term link.

  // Stores the end item [1] and its immediate closure item [0]
  let mut completed_items = VecDeque::from_iter(vec![(
    root_completed_item.get_state(),
    (LinkedItem { item: *root_completed_item, closure_node: prev_state_ref }),
  )]);
  while let Some((state, linked)) = completed_items.pop_front() {
    println!(
      "\nLooking for matches for  {} in {:?}",
      linked.item.debug_string(g),
      linked.closure_node
    );
    let completed_item = linked.item.clone();
    let current_node = linked.closure_node;

    if seen.insert((state, linked.clone())) {
      let prod = linked.item.get_prod_id(g);
      // Grab all productions from the closure that match the end item's
      // production.
      match {
        let (iter, prev_node) = match linked {
          LinkedItem { closure_node: Some(curr_node), .. } => {
            let node = t.get_node(curr_node);

            node
              .goto_items
              .closure_with_state(&t.g)
              .to_vec()
              .print_items(&t.g, &format!("\n Closure {:?}", curr_node));
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

        let closure = iter
          .clone()
          .filter(|i| {
            if i.is_null() {
              i.get_state().get_lane() == state.get_lane()
            } else {
              state.in_same_lane(&i.get_state()) && i.get_production_id_at_sym(&t.g) == prod
            }
          })
          .collect::<Vec<_>>();

        (completed_item, closure, prev_node, current_node)
      } {
        (completed_item, empty_closure, Some(prev_node), current_node)
          if empty_closure.is_empty() =>
        {
          println!("no closure for Node [{:?}] - Selecting previous node", current_node);
          completed_items.push_back((state, LinkedItem {
            item:         completed_item,
            closure_node: Some(prev_node),
          }));
        }
        (completed_item, empty_closure, None, current_node) if empty_closure.is_empty() => {
          println!("no closure for Node [{:?}] - Should be at root node.", current_node);
          // This item should match one of the root items when set to completed
          if completed_item == *root_completed_item {
            fin_items.insert(LinkedItem { item: completed_item, closure_node: None });
          }
        }
        (completed_item, mut closure, prev_node, Some(current_node)) => {
          closure.print_items(g, &format!("Node [{:?}] closure:", current_node));

          for null_item in closure.drain_filter(|i| i.is_null()) {
            completed_items.push_back((null_item.get_state(), LinkedItem {
              item:         completed_item,
              closure_node: Some(current_node),
            }));
          }

          let mut uncompleted_items =
            closure.try_increment().to_state(completed_item.to_local_state());

          let completed = uncompleted_items.drain_filter(|i| i.completed()).collect::<Items>();

          // let mut lane_increment = lane_counter - +;

          for item in &completed {
            if uncompleted_items.is_empty() {
              let state = completed_item.get_state();
              let state = state.to_lanes(lane_counter, state.get_lane());
              let item = item.to_state(state);

              t.get_node_mut(current_node).goto_items.push(Item::null(state));

              lane_counter += 1;

              completed_items
                .push_back((state, LinkedItem { item, closure_node: Some(current_node) }));

              completed_items.push_back((state, LinkedItem { item, closure_node: prev_node }));

              intermediate.insert(LinkedItem { item, closure_node: Some(current_node) });
            } else if *item != *root_completed_item {
              out.insert(LinkedItem { item: *item, closure_node: Some(current_node) });
            } else {
              // panic!("Returning the same item!")
            }
          }

          if !completed.is_empty() && uncompleted_items.is_empty() {
            // lane_counter = lane_increment;
          }

          for item in uncompleted_items {
            out.insert(LinkedItem { item, closure_node: Some(current_node) });
          }
        }
        _ => {
          unreachable!("All conditions should be covered by the above")
        }
      }
    }
  }

  Items::from_linked(fin_items.clone()).print_items(g, "Completed Final Items");
  Items::from_linked(intermediate.clone()).print_items(g, "Intermediate Items");
  Items::from_linked(out.clone()).print_items(g, "Uncompleted Items");

  println!("---- Follow end on {} ----\n\n", root_completed_item.debug_string(g));
  (
    FollowItemGroups {
      final_completed_items: fin_items.into_iter().collect(),
      intermediate_completed_items: intermediate.into_iter().collect(),
      uncompleted_items: out.into_iter().collect(),
    },
    lane_counter,
  )
}
