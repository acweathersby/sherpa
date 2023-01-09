//! # Recursive Descent Graph Compiler
//!
//! Handles the parsing of recursive descent grammars.
//!
//! Initially, all grammars are considered to be recursive descent until one of several events occur:
//!
//! - The compiler encounters one or more left recursive Productions, with either direct recursion:
//!   ```hcg
//!   <> A > A ... x |  x
//!   ```
//!   or indirect recursion:
//!   ```hcg
//!   <> A > B ... x | x  (&&)  B > ... ( X > A )
//!   ```
//!   In which case, the compiler will produce an initial set of transitions that excludes
//!   all left recursive Productions. A list of all excluded and completed productions in the
//!   transition graph is maintained, from which the `recursive_ascent` module can be used to create
//!   a goto state and subsequent parse graph for a set of Productions within the closure that
//!   are `A > X ...`. This makes the resulting parser, assuming that the recursive ascent graph
//!   is constructed, RAD(&ast;).
//!  
//!
//! - The compiler encounters an ambiguity that can't be parsed by as *LL/Recursive Descent*.
//! The compiler can attempt to build an LR graph from the base of the affected Items, and if
//! it succeeds ( which occurs if no conflicts are found ), then appends the root of this graph
//! to the RD graph. This then makes the resulting parser RDLR(&ast;), and if the above situation is
//! also encountered the parser becomes RADLR(&ast;)
//!
//! - An ambiguity is encountered that cannot be resolved using a LR transition graph. In this
//! case, the compiler can make fork states that then turns the resulting parser into either
//! GRD(&ast;), GRDLR(&ast;), or GRADLR(&ast;)

use peek::insert_items_into_node;

use super::{create_and_insert_node, follow::get_follow_items, peek};
use crate::{
  intermediate::utils::get_valid_recursive_descent_start_items,
  journal::Journal,
  types::{GraphNode, TransitionGraph as TPack, *},
};
use std::{collections::BTreeSet, vec};

pub(crate) fn construct_recursive_descent(
  j: &mut Journal,
  scan_type: ScanType,
  starts: &Vec<Item>,
) -> SherpaResult<TPackResults> {
  let g = j.grammar().unwrap();
  let start_productions = BTreeSet::from_iter(starts.iter().map(|i| i.get_prod_id(&g)));
  let is_scanner = scan_type != ScanType::None;

  let (start_items, goto_seeds) = (!is_scanner)
    .then(|| get_valid_recursive_descent_start_items(starts, &g, &start_productions))
    .unwrap_or((starts.clone(), vec![]));

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note(
      "Start Items",
      start_items.iter().map(|i| i.debug_string(&g)).collect::<Vec<_>>().join("\n"),
    )
  }

  let mut t =
    TPack::new(g.clone(), TransitionMode::RecursiveDescent, scan_type, &starts, start_productions);

  t.increment_lane(1); // Lane 0 is reserved.

  let lane_items: Items = start_items
    .into_iter()
    .map(|item| {
        item.to_state(item.get_state().to_lane(t.increment_lane(1)).to_curr_lane())
    })
    .collect();

  t.goto_seeds.append(&mut goto_seeds.to_set());

  t.accept_items.append(&mut lane_items.clone().to_complete().to_origin_only_state().to_set());

  let mut root_node = GraphNode::new(&t, SymbolID::Start, None, starts.clone(), NodeType::RDStart);

  root_node.edge_type = EdgeType::Start;

  root_node.goto_items = lane_items.as_vec().non_term_items(&g);

  let root_index = t.insert_node(root_node);

  t.queue_node(ProcessGroup { node_index: root_index, items: lane_items, ..Default::default() });

  while let Some(process_group) = t.get_next_queued() {
    match process_node(&mut t, j, process_group) {
      SherpaResult::Ok(_) => {}
      SherpaResult::Err(err) => return SherpaResult::Err(err),
      SherpaResult::MultipleErrors(err) => return SherpaResult::MultipleErrors(err),
      SherpaResult::None => return SherpaResult::None,
    }
  }

  SherpaResult::Ok(t.clean())
}

/// Converts items into child nodes of the given parent node
pub(super) fn process_node(
  t: &mut TPack,
  j: &mut Journal,
  ProcessGroup { items, node_index: par_id, discriminant, depth }: ProcessGroup,
) -> SherpaResult<()> {
  match (discriminant, items.len()) {
    (Some((d_sym, d_items)), item_len) => {
      // These items have already been processed and can be transitioned base on the
      // the given symbols. This assumes all items have the same symbol
      debug_assert!(
        items.iter().map(|i| i.get_symbol(&t.g)).collect::<BTreeSet<_>>().len() == 1,
        "When provided with a discriminator symbol, all Items should have the same symbol {}",
        items.to_debug_string(&t.g, "\n")
      );

      insert_items_into_node(d_items, t, par_id);

      let sym = items[0].get_symbol(&t.g);
      match (sym, item_len) {
        (SymbolID::EndOfInput, 1) => {
          create_completed_node(j, t, items[0], par_id, depth);
        }
        (SymbolID::EndOfInput, 2..) => {
          panic!("Can't transition on multiple completed items!");
        }
        (SymbolID::Production(prod_id, _), _) => {
          let node = create_production_call(t, d_sym, prod_id, items, Some(par_id), depth);
          // Make sure we associate this action with a mandatory assertion.
          node.edge_type = EdgeType::Assert;
        }
        (..) => {
          create_terminal_node(t, &items, Some(par_id), depth + 1);
        }
      }
    }
    (None, 1) => {
      insert_items_into_node(items.clone(), t, par_id);

      let item = items[0];
      let sym = item.get_symbol(&t.g);
      match sym {
        SymbolID::EndOfInput => {
          create_completed_node(j, t, item, par_id, depth);
        }
        SymbolID::Production(prod_id, _) => {
          create_production_call(t, sym, prod_id, items, Some(par_id), depth);
        }
        _ => {
          create_terminal_node(t, &items, Some(par_id), depth + 1);
        }
      }
    }
    (None, 2..) => match peek(t, j, par_id, items, depth) {
      SherpaResult::Ok(_) => {}
      err => return err,
    },
    _ => {
      unreachable!("Should not be any other combinations produced at this point.")
    }
  }

  SherpaResult::Ok(())
}

pub(super) fn create_terminal_node(
  t: &mut TPack,
  term_items: &Vec<Item>,
  parent_index: MaybeNodeId,
  depth: usize,
) -> Vec<NodeId> {
  let items: Vec<Item> = term_items.try_increment();

  let node_index = create_and_insert_node(
    t,
    term_items[0].get_symbol(&t.g),
    vec![],
    NodeType::Shift,
    EdgeType::Assert,
    parent_index,
    parent_index,
    items.as_vec().non_term_items(&t.g),
  );

  t.queue_node(ProcessGroup { node_index, items, discriminant: None, depth: depth });

  vec![node_index]
}

pub(super) fn create_production_call<'a>(
  t: &'a mut TPack,
  sym: SymbolID,
  prod_id: ProductionId,
  nonterm_items: Vec<Item>,
  parent_index: MaybeNodeId,
  depth: usize,
) -> &'a mut GraphNode {
  let items: Vec<Item> = nonterm_items.try_increment();

  let node_index = create_and_insert_node(
    t,
    sym,
    vec![],
    NodeType::ProductionCall,
    EdgeType::Default,
    parent_index,
    parent_index,
    items.as_vec().non_term_items(&t.g),
  );

  t.get_node_mut(node_index).prod_sym = Some(SymbolID::Production(prod_id, GrammarId(0)));

  t.queue_node(ProcessGroup { node_index, items, discriminant: None, depth });

  t.get_node_mut(node_index)
}

pub(super) fn create_completed_node(
  j: &mut Journal,
  t: &mut TPack,
  item: Item,
  parent_index: NodeId,
  depth: usize,
) {
  debug_assert!(
    item.completed(),
    "Only items in the completed position should be passed to this function"
  );

  if t.is_scan() || j.config().enable_breadcrumb_parsing {
    let goal = item.get_origin_sym();

    if t.is_scan() {
      debug_assert!(
        goal != SymbolID::Undefined,
        "All completed scanner items should have OriginData::Symbol data\n the item [\n {} \n] has origin data {:?}",
        item.debug_string(&t.g),
        item.get_origin()
      );
    }
    // TODO: Replace with t.accept_items
    let matching_starts = t
      .starts
      .iter()
      .cloned()
      .filter(|s| s.get_origin_sym() == goal)
      .map(|i| i.to_origin_only_state())
      .collect::<BTreeSet<Item>>();

    if !matching_starts.contains(&item.to_start().to_origin_only_state()) {
      // All our completed items need to
      // a scanner run to exit successfully. Thus, the production of the completed state
      // is used to select the next set of items to be scanned, continuing the scan process
      // until we arrive at an end_item that belongs to the root closure.

      let scanned_items = get_follow_items(t, &item, Some(parent_index));

      #[cfg(follow_tracking)]
      {
        j.report_mut().add_note(
          "Scan Mode Item Continuation",
          format!(
            "\n Continuing processing of [{}] through these follow items [\n{}\n]\n",
            item.debug_string(&t.g),
            scanned_items.get_all_items().to_debug_string(&t.g, "\n")
          ),
        );
      }

      let follow_items = scanned_items.get_all_items();

      debug_assert!(follow_items[0] != item);

      if !follow_items.is_empty() {
        t.queue_node(ProcessGroup {
          node_index:   parent_index,
          items:        follow_items,
          discriminant: None,
          depth:        depth,
        });
        return;
      } else {
        panic!(
          "Should have something else to reduce to! [\n{}\n]\n\n",
          scanned_items.get_all_items().to_debug_string(&t.g, "\n")
        );
      }
    } else {
      #[cfg(follow_tracking)]
      {
        j.report_mut().add_note(
          "Scan Mode Item Completion",
          format!("\nCompleted item [{}] which produces [{}]", item.debug_string(&t.g), match item
            .get_origin()
          {
            OriginData::Symbol(sym) => "symbol :".to_string() + &sym.to_string(&t.g),
            OriginData::RuleId(rule_id) =>
              "Production :".to_string()
                + &t.g.get_production(&t.g.rules.get(&rule_id).unwrap().prod_id).unwrap().name,
            _ => String::new(),
          }),
        );
      }
    }
  }

  let node_index = if !t.is_scan()
    && item.len() == 1
    && t.root_prod_ids.contains(&item.decrement().unwrap().get_production_id_at_sym(&t.g))
  {
    // Need to create fail state to break out of the recursive ascent goto loop
    create_and_insert_node(
      t,
      SymbolID::EndOfInput,
      vec![item],
      NodeType::Fail,
      EdgeType::Default,
      Some(parent_index),
      None,
      vec![],
    )
  } else {
    create_and_insert_node(
      t,
      SymbolID::EndOfInput,
      vec![item],
      NodeType::Complete,
      EdgeType::Default,
      Some(parent_index),
      None,
      vec![],
    )
  };

  t.leaf_nodes.push(node_index);
}
