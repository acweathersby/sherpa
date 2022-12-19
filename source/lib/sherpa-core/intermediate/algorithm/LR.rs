use super::get_follow_items;
use crate::{
  grammar::*,
  intermediate::utils::hash_group_btreemap,
  journal::Journal,
  types::{GraphNode, NodeAttributes as TST, TransitionGraph as TPack, *},
};
use std::{
  collections::{BTreeSet, HashMap, VecDeque},
  vec,
};

pub(crate) fn construct_LR(
  j: &mut Journal,
  is_scanner: bool,
  starts: &Vec<Item>,
) -> SherpaResult<TPackResults> {
  let g = j.grammar().unwrap();

  let start_productions = BTreeSet::from_iter(starts.iter().map(|i| i.get_prod_id(&g)));

  let mut t = TPack::new(g.clone(), TransitionMode::LR, is_scanner, &starts, start_productions);

  let mut root_node = GraphNode::new(
    &t,
    SymbolID::Start,
    None,
    starts.into_iter().map(|i| i.to_empty_state()).collect(),
    NodeType::LRStart,
  );

  root_node.edge_type = EdgeType::Start;

  construct_inline_LR(&mut t, j, root_node)?;

  SherpaResult::Ok(t.clean())
}

/// Constructs nodes that uses the LR strategy for parsing
/// This means that any state may have a GOTO clause, with in Hydrocarbon
/// Case means a alternative node branch that stores all GOTO nodes,
/// whose address is then pushed first pushed to the stack before transitioning
/// to sibling nodes.
pub(super) fn construct_inline_LR(
  t: &mut TPack,
  j: &mut Journal,
  root_node: GraphNode,
) -> SherpaResult<()> {
  let mut nodes = Vec::<NodeId>::new();
  let mut leaf_items = vec![];

  let error_cleanup = |t: &mut TPack, nodes: Vec<NodeId>| {
    for node in nodes {
      t.drop_node(&node);
    }
  };

  // In this mode, we take the closure of the entry items, and create
  // transitions from each set of terminals and non-terminals to new states, which are
  // comprised of the shifted items of the previous state.
  // Non-terminal transition represent the parent states goto, and are grouped into
  // a separate state the is prefixed push to the branches of the parent state.

  let mut encountered_states = HashMap::<BTreeSet<Item>, NodeId>::new();

  let root_node_index = t.insert_node(root_node);

  let mut to_process = VecDeque::from_iter(vec![root_node_index]);
  nodes.push(root_node_index);

  let grammar = t.g.clone();
  let g = &grammar;

  while let Some(parent_index) = to_process.pop_front() {
    let mut closure = t.get_node(parent_index).get_closure(g);
    t.get_node_mut(parent_index).goto_items = closure.as_vec().non_term_items(g);

    // let terms = closure.drain_filter(|i| i.is_nonterm(g)).collect::<BTreeSet<_>>();
    let end_items = closure.drain_filter(|i| i.completed()).collect::<BTreeSet<_>>();
    let terms_and_non_terms = closure; // Non-terms become this state's GOTO

    for (symbol, group) in hash_group_btreemap(terms_and_non_terms, |_, v| v.get_symbol(&g)) {
      // Create a new transition node and add the incremented items to  it.
      let incremented_items = group.into_iter().map(|i| i.increment().unwrap()).collect::<Vec<_>>();
      let canonical_items =
        incremented_items.iter().map(|i| i.to_empty_state()).collect::<BTreeSet<_>>();

      match encountered_states.entry(canonical_items) {
        std::collections::hash_map::Entry::Occupied(e) => {
          let child_index = *e.get();
          t.get_node_mut(child_index).proxy_parents.push(parent_index);
        }
        std::collections::hash_map::Entry::Vacant(e) => {
          // TODO: If we can call into a state then we should
          match symbol {
            SymbolID::Production(..) => {
              let mut child_node =
                GraphNode::new(t, symbol, Some(parent_index), incremented_items, NodeType::Goto);
              child_node.edge_type = EdgeType::Goto;
              child_node.closure_parent = Some(parent_index);
              let child_index = t.insert_node(child_node);
              to_process.push_back(child_index);
              nodes.push(child_index);
              e.insert(child_index);
            }
            _ => {
              let mut child_node =
                GraphNode::new(t, symbol, Some(parent_index), incremented_items, NodeType::Shift);
              child_node.edge_type = EdgeType::Assert;
              child_node.closure_parent = Some(parent_index);
              let child_index = t.insert_node(child_node);
              to_process.push_back(child_index);
              nodes.push(child_index);
              e.insert(child_index);
            }
          }
        }
      }
      //
    }

    match end_items.len() {
      2.. => {
        // Get the follow for each node.
        let end_items = end_items
          .into_iter()
          .map(|i| i.to_origin_only_state())
          .collect::<ItemSet>()
          .into_iter()
          .map(|i| (i, get_follow_items(t, &i, Some(parent_index))))
          .collect::<Vec<_>>();

        if end_items.iter().all(|(i, items)| {
          items.final_completed_items.is_empty() && !items.uncompleted_items.is_empty()
        }) {
          let symbol_groups = hash_group_btreemap(
            end_items
              .iter()
              .flat_map(|(i, items)| {
                items
                  .get_uncompleted_items()
                  .iter()
                  .flat_map(|i| get_closure_cached(i, &g))
                  .collect::<BTreeSet<_>>()
                  .iter()
                  .filter_map(|u| u.is_term(&g).then(|| (i.clone(), **u)))
                  .collect::<Vec<_>>()
              })
              .collect::<Vec<_>>(),
            |_, (_, term)| term.get_symbol(&g),
          );

          if symbol_groups.iter().any(|g| g.1.len() > 1) {
            error_cleanup(t, nodes);
            return SherpaResult::Err(
              format!(
                "Could not disambiguate grammar here: \n{}",
                end_items
                  .iter()
                  .map(|i| { i.0.get_origin().blame_string(&t.g) + &i.0.blame_string(&t.g) })
                  .collect::<Vec<String>>()
                  .join("\n")
              )
              .into(),
            );
          } else {
            for (sym, mut items) in symbol_groups {
              let (end_item, _) = items.pop().unwrap();
              let index = create_end_node(t, sym, parent_index, &end_item);
              t.get_node_mut(index).set_attribute(TST::I_LR);
              t.get_node_mut(index).edge_type = EdgeType::Assert;
              leaf_items.push(index);
            }
          }
        } else {
          error_cleanup(t, nodes);
          return SherpaResult::Err(
            format!(
              "Encountered conflicting completed items:\n{}\n",
              end_items
                .into_iter()
                .map(|(i, _)| "   ".to_string()
                  + &i.blame_string(&g)
                  + "\n"
                  + &i.get_rule(&g).unwrap().tok.blame(0, 0, "Defined here", BlameColor::Red))
                .collect::<Vec<_>>()
                .join("\n")
            )
            .into(),
          );
        }
      }
      1 => {
        let index = create_end_node(
          t,
          SymbolID::Default,
          parent_index,
          &end_items.into_iter().next().unwrap(),
        );
        t.get_node_mut(index).set_attribute(TST::I_LR);
        t.get_node_mut(index).edge_type = EdgeType::Default;
        leaf_items.push(index);
      }
      _ => {}
    }
  }

  t.leaf_nodes.append(&mut leaf_items);

  SherpaResult::Ok(())
}

fn create_end_node(t: &mut TPack, sym: SymbolID, parent_index: NodeId, end_item: &Item) -> NodeId {
  let end_node = GraphNode::new(t, sym, Some(parent_index), vec![*end_item], NodeType::Complete);
  let node_index = t.insert_node(end_node);
  node_index
}
