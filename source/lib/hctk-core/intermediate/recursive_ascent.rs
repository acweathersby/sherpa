//! # Recursive Ascent Graph Compiler
//!
//! Though not a true representation of a recursive descent parser, which is simply the conversion of
//! an LR parse table into recursive functions, this compiler provides one of the most essential
//! features of LR parsers, the ability to parse grammars with left recursions. This feature is embodied
//! within by providing a primary GOTO state, in which productions that have
//! been completed are matched with branches that transition on a Production ids. Beyond that,
//! the implementation reuses much of the functionality found within the [recursive_descent], [peek],
//! and [lr] modules.
//!
//! Note: Since scanner productions are guaranteed to not have left recursion of any kind, there
//! is no need to run TokenProductions through this process.
use super::{peek::create_node, recursive_descent::process_node, utils::*};
use crate::{
  journal::Journal,
  types::{GraphNode, NodeAttributes as TST, TransitionGraph as TPack, *},
};
use std::{collections::BTreeSet, rc::Rc, vec};

pub(crate) fn construct_recursive_ascent(
  j: &mut Journal,
  goto_seeds: BTreeSet<Item>,
  root_ids: BTreeSet<ProductionId>,
) -> TPackResults {
  let g = j.grammar().unwrap();

  let mut t = TPack::new(g.clone(), TransitionMode::GoTo, false, &vec![], root_ids);

  t.goto_scoped_closure = Some(Rc::new(Box::<Vec<Item>>::new(
    (!t.is_scanner).then(|| get_follow_closure(&g, &t.root_prod_ids)).unwrap_or_default(),
  )));

  // Get closures of all items that could transition on the same production.

  let mut root_node =
    GraphNode::new(&t, SymbolID::Start, None, goto_seeds.clone().to_vec(), NodeType::RAStart);
  root_node.edge_type = EdgeType::Start;
  root_node.set_attribute(TST::I_GOTO_START);
  let parent_index = Some(t.insert_node(root_node));

  let mut unfulfilled_root = Some(*t.root_prod_ids.first().unwrap());

  for (production_id, group) in
    hash_group_btreemap(goto_seeds, |_, i| i.get_production_id_at_sym(&t.g))
  {
    let have_root_production = t.root_prod_ids.contains(&production_id);
    let mut have_end_items = false;

    let mut items: Vec<Item> = group
      .iter()
      .map(|i| {
        let stated_item = if i.completed() {
          have_end_items = true;
          i.to_state(ItemState::GOTO_END_GOAL_STATE)
        } else {
          i.increment().unwrap()
        };

        stated_item
      })
      .collect();

    let mut goto_node = create_node(
      &t,
      SymbolID::Production(production_id, GrammarId::default()),
      items.clone(),
      NodeType::Goto,
      EdgeType::Goto,
      parent_index,
      parent_index,
      items.term_item_vec(&t.g),
    );

    if have_root_production || (group.len() > 1 && have_end_items) {
      t.out_of_scope_closure =
        Some(g.lr_items.iter().flat_map(|(_, i)| i).cloned().collect::<Vec<Item>>());

      if have_root_production {
        unfulfilled_root = None;
        let mut reducible: Vec<Item> = g
          .lr_items
          .get(&production_id)
          .unwrap_or(&Vec::new())
          .iter()
          .filter(|i| !group.iter().any(|g| (*g).to_empty_state() == (**i).to_empty_state()))
          .map(|i| i.increment().unwrap().to_state(ItemState::GOTO_ROOT_END_GOAL_STATE))
          .collect();

        goto_node.transition_items.append(&mut reducible.clone());
        items.append(&mut reducible);
      }
      let items = goto_node.transition_items.clone().to_set().to_vec();
      let node_index = t.insert_node(goto_node);

      t.queue_node(ProcessGroup { node_index, items, ..Default::default() });
    } else {
      let node_index = t.insert_node(goto_node);
      t.queue_node(ProcessGroup { node_index, items, ..Default::default() });
    }
  }

  // If the root production is not covered in the goto branches
  // then create a new node that serves as an accepting state
  // if the active production id is the root.

  if let Some(production_id) = unfulfilled_root {
    let mut goto_node = GraphNode::new(
      &t,
      SymbolID::Production(production_id, GrammarId::default()),
      parent_index,
      vec![],
      NodeType::Pass,
    );
    goto_node.edge_type = EdgeType::Goto;
    goto_node.set_attribute(TST::I_PASS);
    let index = t.insert_node(goto_node);
    t.leaf_nodes.push(index);
  }

  while let Some(process_group) = t.get_next_queued() {
    process_node(&mut t, j, process_group);
  }

  t.non_trivial_root = unfulfilled_root.is_none();

  t.clean()
}
