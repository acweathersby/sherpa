//! # Intermediate Representation Compiler
//!
//! Takes completed TransitionGraphs and builds a set of globally unique IRStates.
use super::utils::*;
use crate::{
  journal::Journal,
  types::{GraphNode, TransitionGraph, *},
  writer::code_writer::CodeWriter,
};
use std::{
  collections::{BTreeMap, BTreeSet, VecDeque},
  vec,
};

pub(super) fn construct_ir(
  j: &mut Journal,
  entry_name: &str,
  t: &TransitionGraph,
) -> SherpaResult<Vec<Box<IRState>>> {
  // At this point the graph edges are reverse of the direction we want,
  // with them being child->parent. The first task is to build a structure
  // to store the inverse relationship: parent->child
  let leaf_node_set = t.leaf_nodes.iter().collect::<BTreeSet<_>>();

  let mut output = BTreeMap::<NodeId, Box<IRState>>::new();

  let children_lut = create_parent_to_child_map(t);

  let mut nodes_pipeline = VecDeque::from_iter(t.leaf_nodes.iter().cloned());

  'outer: while let Some(node_id) = nodes_pipeline.pop_front() {
    let node = t.get_node(node_id);

    if !leaf_node_set.contains(&node_id) {
      if output.contains_key(&node_id) {
        // Already dealt with this node. No need to do any more work.
        continue;
      }

      let children_lookup = &children_lut[node_id];

      // Ensure dependencies are met.
      for (_, child) in children_lookup {
        if child.id != node_id {
          if !output.contains_key(&child.id) && !leaf_node_set.contains(&child.id) {
            // Push dependency to back on to the queue, which will cause this node
            // be pushed back into the queue after its children are processed
            continue 'outer;
          }
        }
      }

      #[cfg(debug_assertions)]
      {
        if children_lookup.is_empty() {
          panic!("Childless node [{}] is not in leaf node set!", node_id);
        }
      }

      let children = children_lookup.values().map(|v| *v).collect::<Vec<_>>();

      let states = create_state(entry_name, t, j, node, &children, &output)?;

      for state in states {
        output.insert(state.get_graph_id(), state);
      }
    }
    // Add all parent dependencies to the queue
    if let Some(parent_id) = node.parent {
      nodes_pipeline.push_back(parent_id);
      for proxy_parent in &node.proxy_parents {
        nodes_pipeline.push_back(*proxy_parent);
      }
    }
  }

  let mut hash_filter = BTreeSet::<u64>::new();

  SherpaResult::Ok(
    output.into_values().filter(|s| hash_filter.insert(s.get_hash())).collect::<Vec<_>>(),
  )
}

fn create_state(
  entry_name: &str,
  t: &TransitionGraph,
  j: &mut Journal,
  node: &GraphNode,
  children: &Vec<&GraphNode>,
  states: &BTreeMap<NodeId, Box<IRState>>,
) -> SherpaResult<Vec<Box<IRState>>> {
  let mut w = CodeWriter::new(vec![]);
  let mut is_token_assertion = false;
  let mut prefix: Option<String> = None;
  let mut postfix: Option<String> = match node.node_type {
    NodeType::RAStart | NodeType::RDStart if !t.is_scan() => {
      Some(format!(" then goto state [ {}_goto ]", entry_name))
    }
    _ => None,
  };
  let mut output = vec![];
  w.increase_indent();

  #[cfg(debug_assertions)]
  {
    // A few assertions that we need to get out of the way.
    debug_assert!(
      !node.id.is_root()
        || t.mode == TransitionMode::RecursiveDescent
        || t.mode == TransitionMode::LR
        || children.iter().all(|c| { c.edge_type == EdgeType::Goto }),
      "The children of the root goto function should all be EdgeType::Goto {}",
      children.iter().map(|c| c.debug_string(&t.g)).collect::<Vec<_>>().join("\n")
    );

    let child_edge_groups = hash_group_vec(
      children
        .iter()
        .filter(|c| c.edge_type != EdgeType::Default && c.edge_type != EdgeType::Goto)
        .collect::<Vec<_>>(),
      |_, c| c.edge_type,
    );

    debug_assert!(
      children.len() == 1 || child_edge_groups.len() <= 1,
      "All child nodes need to have the same transition edges type or have the default transition \n:{}",
     children.iter().map(|c| c.debug_string(&t.g)).collect::<Vec<_>>().join("\n")
    );

    debug_assert!(
      children.len() == 1
        || node.node_type == NodeType::Fork
        || children.iter().filter(|c| c.edge_type == EdgeType::Default).count() <= 1,
      "There should only be up to one default transition for each node \nNode:\n{}\nChildren:\n[\n{}\n]",
      node.debug_string(&t.g),
      children.iter().map(|c| c.debug_string(&t.g)).collect::<Vec<_>>().join("\n")
    );
  }

  match node.node_type {
    NodeType::Fork => {
      let mut child_hashes = vec![];

      for child in children {
        let child_state = states.get(&child.id).unwrap();
        child_hashes.push(format!(
          "state [ {} ]",
          IRState::get_state_name_from_hash(child_state.get_hash(),)
        ));
      }

      w.write_fmt(format_args!(
        "\nfork to ( {} ) to complete prod {}",
        &child_hashes.join(" "),
        t.g.get_production(&t.get_first_prod_id().unwrap()).unwrap().bytecode_id
      ))?;
    }
    _ => {
      let total_children_count = children.len();
      for (edge_type, children) in hash_group_btreemap(children.clone(), |_, c| c.edge_type) {
        match edge_type {
          EdgeType::Default if children.len() > 1 => {
            unreachable!(
              "There should only be one default edge per node \n Node: {} \n children: {{\n{}\n}}",
              node.debug_string(&t.g),
              children.iter().map(|c| c.debug_string(&t.g)).collect::<Vec<_>>().join("\n")
            );
          }
          EdgeType::Default => {
            for child in children {
              if t.is_scan() && child.node_type == NodeType::Complete {
                prefix = create_reduction_string(t, child).map(|s| format!("{} then", s));
              }

              create_branch_wrap(
                child,
                node,
                states,
                t,
                &mut w,
                prefix.as_ref(),
                postfix.as_ref(),
                if total_children_count == 1 { EdgeType::Undefined } else { child.edge_type },
              );

              /*        if t.is_scan() && child.node_type == NodeType::Complete {
                prefix = create_reduction_string(child, &t.g, true).map(|s| format!("{} then", s));
              } */
            }
          }
          EdgeType::Goto => {
            match node.node_type {
              NodeType::RAStart | NodeType::GotoVirtual => {
                for child in children {
                  create_branch_wrap(
                    child,
                    node,
                    states,
                    t,
                    &mut w,
                    prefix.as_ref(),
                    postfix.as_ref(),
                    child.edge_type,
                  );
                }
              }
              _ => {
                let mut virtual_node =
                  GraphNode::new(t, node.edge_symbol, None, vec![], NodeType::GotoVirtual);
                virtual_node.id = node.id.to_goto_id();
                let children =
                  children.into_iter().filter(|i| i.edge_type == EdgeType::Goto).collect();

                let mut state = create_state(entry_name, t, j, &virtual_node, &children, states)?;

                (*postfix.get_or_insert_with(|| String::default()))
                  .insert_str(0, &format!(" then goto state [ {}]", state[0].get_name()));

                output.append(&mut state);
                // Create a virtual function that will produce a goto state.
              }
            }
          }
          EdgeType::Assert | EdgeType::Peek => {
            is_token_assertion = matches!(edge_type, EdgeType::Assert | EdgeType::Peek);
            for child in children {
              create_branch_wrap(
                child,
                node,
                states,
                t,
                &mut w,
                prefix.as_ref(),
                postfix.as_ref(),
                child.edge_type,
              );
            }
          }
          _ => {
            unreachable!("Unexpected edge_type {:?}", edge_type);
          }
        }
      }
    }
  }

  if node.node_type == NodeType::RAStart && t.non_trivial_root {
    if let Some(root_production) = t.get_first_prod_id() {
      w.write_fmt(format_args!(
        "\n\non fail state [ {}_goto_failed ]\n    assert PRODUCTION [ {} ] ( pass )",
        entry_name,
        t.g.productions.get(&root_production).unwrap().bytecode_id
      ))?;
    }
  }

  let (normal_symbols, skip_symbols) = if t.is_scan() || node.node_type == NodeType::RAStart {
    (vec![], vec![])
  } else {
    let (normal_symbol_set, skip_symbols_set, _) = get_symbols_from_items(
      node.get_unique_transition_item_set(),
      &t.g,
      None,
      children
        .iter()
        .filter_map(|i| match i.edge_symbol {
          sym if sym.is_production() => None,
          SymbolID::EndOfInput => None,
          sym => Some(sym),
        })
        .collect(),
    );

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        w.write_fmt(format_args!("\nskip [ {} ]", symbol_id.bytecode_id(Some(&t.g)),))?;
      }
    }

    (normal_symbol_set.into_iter().collect(), skip_symbols_set.into_iter().collect())
  };

  output.push(Box::new(
    IRState {
      comment: Default::default(),
      code: unsafe { String::from_utf8_unchecked(w.into_output()) },
      name: match node.node_type {
        NodeType::RDStart => entry_name.to_string(),
        NodeType::RAStart => format!("{}_goto", entry_name),
        _ => Default::default(),
      },
      state_type: if t.is_scan() { IRStateType::Scanner } else { IRStateType::Parser },
      graph_id: node.id,
      normal_symbols,
      skip_symbols,
      ..Default::default()
    }
    .into_hashed(),
  ));

  SherpaResult::Ok(output)
}

const empty_string: String = String::new();

fn create_branch_wrap(
  child: &GraphNode,
  node: &GraphNode,
  resolved_states: &BTreeMap<NodeId, Box<IRState>>,
  t: &TransitionGraph,
  w: &mut CodeWriter<Vec<u8>>,
  prefix: Option<&String>,
  postfix: Option<&String>,
  edge_type: EdgeType,
) -> SherpaResult<()> {
  let sym = child.edge_symbol;

  let (symbol_bytecode_id, assert_class, sym_comment) = if !t.is_scan() {
    (sym.bytecode_id(Some(&t.g)), "TOKEN", sym.to_string(&t.g))
  } else {
    let (bc, class) = child.edge_symbol.shift_info(&t.g);
    (bc, class, sym.to_string(&t.g))
  };

  let postfix = postfix.filter(|_| !matches!(child.node_type, NodeType::Fail | NodeType::Pass));

  w.newline()?;
  match edge_type {
    EdgeType::Assert => {
      w.write_fmt(format_args!(
        "assert {} [ {}{} ] ( {}{}{} )",
        assert_class,
        symbol_bytecode_id,
        escaped_comment(sym_comment),
        prefix.unwrap_or(&empty_string),
        create_child_state(child, node, resolved_states, t),
        postfix.unwrap_or(&empty_string),
      ))?;
    }
    EdgeType::Peek => {
      w.write_fmt(format_args!(
        "assert peek {} [ {}{} ] ( {}{}{} )",
        assert_class,
        symbol_bytecode_id,
        escaped_comment(sym_comment),
        prefix.unwrap_or(&empty_string),
        create_child_state(child, node, resolved_states, t),
        postfix.unwrap_or(&empty_string),
      ))?;
    }
    EdgeType::Goto => {
      w.write_fmt(format_args!(
        "assert PRODUCTION [ {}{} ] ( {}{}{} )",
        symbol_bytecode_id,
        escaped_comment(sym_comment),
        prefix.unwrap_or(&empty_string),
        create_child_state(child, node, resolved_states, t),
        postfix.unwrap_or(&empty_string),
      ))?;
    }
    EdgeType::Default => {
      let string = format!(
        "{}{}{}",
        prefix.unwrap_or(&empty_string),
        create_child_state(child, node, resolved_states, t),
        postfix.unwrap_or(&empty_string),
      );

      if !string.is_empty() {
        w.write_fmt(format_args!("default ( {} )", string))?;
      }
    }
    _ => {
      w.write_fmt(format_args!(
        "{}{}{}",
        prefix.unwrap_or(&empty_string),
        create_child_state(child, node, resolved_states, t),
        postfix.unwrap_or(&empty_string),
      ))?;
    }
  }

  SherpaResult::Ok(())
}

fn create_child_state(
  child: &GraphNode,
  node: &GraphNode,
  resolved_states: &BTreeMap<NodeId, Box<IRState>>,
  t: &TransitionGraph,
) -> String {
  let state_name = get_node_state_name(child, node, resolved_states);

  match child.node_type {
    NodeType::ProductionCall => {
      if let Some(SymbolID::Production(prod_id, _)) = child.prod_sym {
        format!(
          "goto state [ {} ] then goto state [ {} ]",
          t.g.productions.get(&prod_id).unwrap().guid_name,
          state_name,
        )
      } else {
        panic!("Incorrectly defined ProductionCall node, missing prod_sym");
      }
    }
    NodeType::Complete => {
      if !t.is_scan() {
        create_reduction_string(t, child).unwrap()
      } else {
        " pass".to_string()
      }
    }
    NodeType::Shift => {
      format!("shift then goto state [ {} ]", state_name,)
    }
    NodeType::BreadcrumbShiftCompletion => {
      format!(
        "complete crumb trail [{}]{} then shift then goto state [ {} ]",
        child.transition_items[0].get_state().get_lane(),
        escaped_comment(child.transition_items[0].debug_string(&t.g)),
        state_name,
      )
    }
    NodeType::BreadcrumbEndCompletion => {
      format!(
        "complete crumb trail [{}]{} then {}",
        child.transition_items[0].get_state().get_lane(),
        escaped_comment(child.transition_items[0].debug_string(&t.g)),
        state_name,
      )
    }

    NodeType::BreadcrumbTransition => {
      let mut breadcrumb = String::new();
      let mut lane_items =
        hash_group_btreemap(node.transition_items.clone(), |_, item| item.get_state().get_lane())
          .into_iter()
          .map(|(l, item)| (l, (item, 0u32)))
          .collect::<BTreeMap<_, _>>();

      for (items, _) in lane_items.values().cloned().collect::<Vec<_>>() {
        for item in items {
          let state = item.get_state();
          match state.get_lanes() {
            (a, b) if a != b => {
              lane_items.entry(b).and_modify(|(_, b)| {
                (*b) += 1;
              });
            }
            _ => {}
          }
        }
      }

      for (_, (items, _)) in lane_items {
        let completed_items = items.completed_items();

        if !completed_items.is_empty() {
          for item in &completed_items {
            match item.get_state().get_lanes() {
              (c, p) if p != c => {
                breadcrumb += &format!("\n        crumb [ {} | map {} ] then ", c, p);
              }
              _ => {}
            }

            let reduce_string = create_rule_reduction_string_new(&t.g, item);
            breadcrumb += &format!(
              "\n        crumb [ {} | {} ] then ",
              item.get_state().get_lane(),
              reduce_string
            );
          }
        } else {
        }
        // if count == 0 {
        // breadcrumb += &format!("\n        crumb [ {} | shift ] then ", lane)
        //}
      }

      breadcrumb += &format!("\n        crumb [ 0 | shift ] then ");

      format!("{}\n        shift then goto state [ {} ]", breadcrumb, state_name,)
    }
    NodeType::PeekTransition | NodeType::Fork | NodeType::Goto => {
      format!("goto state [ {} ]", state_name,)
    }
    NodeType::Pass => "pass".to_string(),
    NodeType::Fail => "fail".to_string(),
    _ => {
      panic!("No actions defined for node_type {:?}", child.node_type);
    }
  }
}

fn create_reduction_string(t: &TransitionGraph, node: &GraphNode) -> Option<String> {
  let g = &t.g;
  match (node.transition_items.first(), t.scan_type, node.node_type == NodeType::Pass) {
    (Some(item), ScanType::None /* not scanner */, false) => {
      Some(create_rule_reduction_string(g, item))
    }
    (None, ScanType::None /* not scanner */, true /* default pass state */) => {
      Some("pass".to_string())
    }
    (Some(item), ScanType::ScannerEntry, false) => {
      match (item.get_origin(), t.item_is_goal(*item)) {
        (OriginData::Symbol(sym), true) => Some(create_token_reduction_string(g, sym)),
        (OriginData::Symbol(_), false) => None,
        _ => {
          unreachable!("All items assigned to scanner nodes should have OriginData::Symbol data");
        }
      }
    }
    _ => None,
  }
}

fn create_token_reduction_string(g: &GrammarStore, sym: SymbolID) -> String {
  format!("assign token [ {}{} ]", sym.bytecode_id(Some(g)), escaped_comment(sym.to_string(g)))
}

fn create_rule_reduction_string(g: &GrammarStore, item: &Item) -> String {
  let rule = g.rules.get(&item.get_rule_id()).unwrap();
  let production = g.productions.get(&rule.prod_id).unwrap();
  format!(
    "set prod to {} then reduce {} symbols to body {}",
    production.bytecode_id, rule.len, rule.bytecode_id,
  )
}

fn create_rule_reduction_string_new(g: &GrammarStore, item: &Item) -> String {
  let rule = g.rules.get(&item.get_rule_id()).unwrap();
  let production = g.productions.get(&rule.prod_id).unwrap();
  format!(
    "reduce {} symbols from rule {} to prod {}",
    rule.len, rule.bytecode_id, production.bytecode_id,
  )
}

fn get_node_state_name(
  child: &GraphNode,
  node: &GraphNode,
  resolved_states: &BTreeMap<NodeId, Box<IRState>>,
) -> String {
  let state_name = (child.id != node.id)
    .then(|| match resolved_states.get(&child.id) {
      Some(child_state) => child_state.get_name(),
      _ => String::from(format!("LEAF_STATE{}", child.id)),
    })
    .unwrap_or(String::from("%%%%"));
  state_name
}

fn create_parent_to_child_map<'a>(
  t: &'a TransitionGraph,
) -> Vec<BTreeMap<SymbolID, &'a GraphNode>> {
  let mut children_tables =
    t.nodes_iter().map(|_| BTreeMap::<SymbolID, &'a GraphNode>::new()).collect::<Vec<_>>();
  // Starting with the leaf nodes, construct a the reverse
  // edges of our transition graph, converting the relationship
  // child->parent to parent->child
  for child in t.nodes_iter() {
    if child.has_parent() {
      if let Some(parent_id) = child.parent.filter(|i| child.id != *i) {
        if children_tables[parent_id].insert(child.edge_symbol, child).is_some() {
          panic!("Overriding Node");
        }
      }

      for proxy_parent in &child.proxy_parents {
        if children_tables[*proxy_parent].insert(child.edge_symbol, child).is_some() {
          panic!("Overriding Node");
        }
      }
    }
  }
  children_tables
}

#[inline]
fn escaped_comment(comment_body: String) -> String {
  format!(" /* {} */", comment_body.replace("\n", "\\n").replace("*", "&ast;"))
}
