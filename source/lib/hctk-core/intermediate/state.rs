//! Methods for constructing IRStates from a grammar
use super::{
  transition::{construct_goto, construct_recursive_descent},
  utils::*,
};
use crate::{
  grammar::{get_production_start_items, get_scanner_info_from_defined, hash_id_value_u64},
  types::{
    GrammarStore,
    HCError,
    HCErrorContainer,
    HCErrorPrint,
    IRState,
    IRStateType,
    IRStateType::*,
    Item,
    OriginData,
    PeekType,
    ProductionId,
    SymbolID,
    TGNId,
    TransitionGraphNode,
    TransitionMode,
    TransitionPack,
    TransitionStateType,
  },
};
use std::{
  collections::{BTreeMap, BTreeSet, VecDeque},
  sync::Arc,
  thread,
  vec,
};

pub struct IROutput {
  pub states: Vec<Box<IRState>>,
  pub errors: Vec<HCError>,
}

/// Compiles all production within the grammar into unique IRStates
pub fn compile_states(
  g: Arc<GrammarStore>,
  num_of_threads: usize,
) -> (BTreeMap<String, Box<IRState>>, Vec<HCError>) {
  let mut deduped_states = BTreeMap::new();

  let productions_ids = g.productions.keys().cloned().collect::<Vec<_>>();

  let mut errors = vec![];

  if num_of_threads == 1 {
    let (states, mut e) = process_productions(&productions_ids, g);

    errors.append(&mut e);

    for state in states {
      deduped_states.entry(state.get_name()).or_insert(state);
    }

    if !errors.have_critical() {
      for (_, state) in &mut deduped_states {
        let string = state.to_string();
        if let Err(err) = state.compile_ast() {
          panic!("\n{} {}", err, string)
        };
      }
    }
  } else {
    let work_chunks = productions_ids
      .chunks((productions_ids.len() / (num_of_threads - 1)).max(1))
      .collect::<Vec<_>>();

    let (states, e): (Vec<_>, Vec<_>) = thread::scope(|s| {
      work_chunks
        .into_iter()
        .map(|productions| s.spawn(|| process_productions(productions, g.clone())))
        .collect::<Vec<_>>()
        .into_iter()
        .map(move |s| s.join().unwrap())
        .collect::<Vec<_>>()
    })
    .into_iter()
    .unzip();

    errors.append(&mut e.into_iter().flatten().collect());

    for state in states.into_iter().flatten() {
      deduped_states.entry(state.get_name()).or_insert(state);
    }

    let mut output_states = deduped_states.iter_mut().collect::<Vec<_>>();

    if !errors.have_critical() {
      thread::scope(|s| {
        let _ = output_states
          .chunks_mut(num_of_threads)
          .into_iter()
          .map(|chunk| {
            s.spawn(|| {
              for (_, state) in chunk {
                let string = state.to_string();
                if let Err(err) = state.compile_ast() {
                  panic!("\n{} {}", err, string)
                };
              }
            })
          })
          .collect::<Vec<_>>()
          .into_iter()
          .map(move |s| s.join().unwrap())
          .collect::<Vec<_>>();
      });
    }
  }
  (deduped_states, errors)
}

fn process_productions(
  productions: &[ProductionId],
  g: Arc<GrammarStore>,
) -> (Vec<Box<IRState>>, Vec<HCError>) {
  let mut out_states = Vec::with_capacity(1024);
  let mut scanner_names = BTreeSet::new();
  let mut state_names = BTreeSet::new();
  let mut errors = vec![];
  for prod_id in productions {
    let IROutput { errors: mut e, states } = generate_production_states(prod_id, g.clone());

    errors.append(&mut e);

    for state in states {
      if let Some(name) = state.get_scanner_state_name() {
        if scanner_names.insert(name.clone()) {
          let IROutput { errors: mut e, states } =
            generate_scanner_intro_state(state.get_scanner_symbol_set().unwrap(), g.clone());

          errors.append(&mut e);

          for state in states {
            if state_names.insert(state.get_name()) {
              out_states.push(state);
            }
          }
        }
      }
      if state_names.insert(state.get_name()) {
        out_states.push(state);
      }
    }
  }

  (out_states, errors)
}

pub fn generate_scanner_intro_state(symbols: BTreeSet<SymbolID>, g: Arc<GrammarStore>) -> IROutput {
  let symbol_items = symbols
    .iter()
    .flat_map(|s| {
      let (_, prod_id, ..) = get_scanner_info_from_defined(s, &g);
      get_production_start_items(&prod_id, &g)
        .iter()
        .map(|i| i.to_origin(crate::types::OriginData::Symbol(*s)))
        .collect::<Vec<_>>()
    })
    .collect::<Vec<_>>();

  // Ensure there are no left recursive productions within the
  // items
  check_for_left_recursion(&symbol_items, &g);

  generate_states(g, true, &symbol_items, &format!("scan_{:02X}", hash_id_value_u64(&symbols)))
}

pub fn generate_production_states(prod_id: &ProductionId, g: Arc<GrammarStore>) -> IROutput {
  let prod = g.get_production(prod_id).unwrap();
  generate_states(
    g.clone(),
    prod.is_scanner,
    &get_production_start_items(prod_id, &g)
      .into_iter()
      .map(|i| i.to_origin(OriginData::Production(*prod_id)))
      .collect::<Vec<_>>(),
    &prod.guid_name,
  )
}

fn generate_states(
  g: Arc<GrammarStore>,
  is_scanner: bool,
  start_items: &[Item],
  entry_name: &String,
) -> IROutput {
  let mut errors: Vec<HCError> = vec![];
  let mut output: Vec<Box<IRState>> = vec![];

  let root_ids = start_items.iter().map(|i| i.get_prod_id(&g)).collect::<BTreeSet<_>>();

  let (t, mut e) =
    construct_recursive_descent(g.clone(), is_scanner, &start_items, root_ids.clone());

  e.debug_print();

  errors.append(&mut e);

  let (mut states, mut e) = process_transition_nodes(&t, entry_name);

  errors.append(&mut e);
  output.append(&mut states);

  // Scanner states are guaranteed to be purely recursive descent compatible
  //  and thus they have no need for a GOTO path.
  if !is_scanner {
    let (goto_data, mut e) =
      construct_goto(g.clone(), is_scanner, &t.goto_seeds.iter().cloned().collect(), root_ids);

    errors.append(&mut e);

    if !goto_data.leaf_nodes.is_empty() {
      let (mut states, mut e) = process_transition_nodes(&goto_data, entry_name);

      errors.append(&mut e);
      output.append(&mut states);
    } else {
      output.push(create_passing_goto_state(entry_name, is_scanner));
    }
  }

  IROutput { states: output, errors: errors }
}

pub(crate) fn process_transition_nodes<'a>(
  t: &'a TransitionPack,
  entry_name: &String,
) -> (Vec<Box<IRState>>, Vec<HCError>) {
  // We start at leaf nodes and make our way down to the root.
  let leaf_node_set = t.leaf_nodes.iter().collect::<BTreeSet<_>>();

  let mut output = BTreeMap::<TGNId, Box<IRState>>::new();
  let mut errors = vec![];

  let mut children_tables = t.nodes_iter().map(|_| BTreeSet::<TGNId>::new()).collect::<Vec<_>>();

  // Starting with the leaf nodes, construct a the reverse
  // edges of our transition graph, converting the relationship
  // child->parent to parent->child

  for child in t.nodes_iter() {
    if child.has_parent(t) {
      if let Some(parent_id) = child.parent.filter(|i| child.id != *i) {
        children_tables[parent_id].insert(child.id);
      }

      for proxy_parent in &child.proxy_parents {
        if *proxy_parent != child.id {
          children_tables[*proxy_parent].insert(child.id);
        }
      }
    }
  }

  let mut nodes_pipeline = VecDeque::from_iter(t.leaf_nodes.iter().cloned());

  'outer: while let Some(node_id) = nodes_pipeline.pop_front() {
    if output.contains_key(&node_id) {
      // Already dealt with this node. No need to do any more work.
      continue;
    }

    let children_lookup = children_tables.get(node_id.usize()).unwrap();

    // Ensure dependencies are met.
    for child_id in children_lookup {
      if *child_id != node_id {
        if !output.contains_key(child_id) {
          // Push dependency to be processed, which will cause this node
          // be pushed back into the queue after it is processed
          nodes_pipeline.push_back(*child_id);

          continue 'outer;
        }
      }
    }

    let node = t.get_node(node_id);

    if !leaf_node_set.contains(&node_id) {
      if children_lookup.is_empty() {
        panic!("Childless node [{}] does is not in leaf node set!", node_id);
      }

      if node.is(TransitionStateType::I_GOTO_START | TransitionStateType::I_GOTO_LR) {
        let state = create_goto_state(node, t, &output, &children_tables, &entry_name, &mut errors);
        output.insert(state.get_graph_id(), state);
      }

      /// If able, parse the node as standard transition node, creating an entry name
      if !node.is(TransitionStateType::I_GOTO_START) {
        let state =
          create_intermediate_state(node, t, &output, &children_tables, entry_name, &mut errors);
        output.insert(state.get_graph_id(), state);
      };
    } else {
      // End states are discarded at the end, so we will only use this
      // retrieve state's hash for parent states.
      let state = create_reduce_state(node, &t.g, t.is_scanner);
      output.insert(state.get_graph_id(), state);
    }

    if let Some(parent_id) = node.parent {
      nodes_pipeline.push_back(parent_id);
      for proxy_parent in &node.proxy_parents {
        nodes_pipeline.push_back(*proxy_parent);
      }
    }
  }

  let mut hash_filter = BTreeSet::<u64>::new();

  (output.into_values().filter(|s| hash_filter.insert(s.get_hash())).collect::<Vec<_>>(), errors)
}

fn create_passing_goto_state(entry_name: &String, is_scanner: bool) -> Box<IRState> {
  Box::new(
    IRState {
      code: "pass".to_string(),
      id: get_goto_name(entry_name),
      state_type: if is_scanner { ScannerGoto } else { ProductionGoto },
      ..Default::default()
    }
    .into_hashed(),
  )
}

fn fun_name(
  node: &TransitionGraphNode,
  entry_name: &String,
  t: &TransitionPack,
) -> (String, String, u32, IRStateType) {
  if node.id.is_root() {
    (
      if t.is_scanner || node.is(TransitionStateType::I_LR_START) {
        "".to_string()
      } else {
        create_post_amble(entry_name, &t.g)
      },
      if node.is(TransitionStateType::I_GOTO_START) {
        entry_name.clone() + "_goto"
      } else {
        entry_name.clone()
      },
      1,
      match t.mode {
        TransitionMode::GoTo => {
          if t.is_scanner {
            ScannerGoto
          } else {
            ProductionGoto
          }
        }
        TransitionMode::RecursiveDescent => {
          if t.is_scanner {
            ScannerStart
          } else {
            ProductionStart
          }
        }
      },
    )
  } else {
    (String::default(), String::default(), 0, Undefined)
  }
}

fn create_goto_state(
  node: &TransitionGraphNode,
  t: &TransitionPack,
  resolved_states: &BTreeMap<TGNId, Box<IRState>>,
  children_tables: &Vec<BTreeSet<TGNId>>,
  entry_name: &String,
  errors: &mut Vec<HCError>,
) -> Box<IRState> {
  let mut children = get_children(t, node, children_tables);
  let mut strings = vec![];
  let mut comment = String::new();

  let (mut post_amble, state_name, mut stack_depth, mut state_type) = fun_name(node, entry_name, t);

  for child in children {
    let state = resolved_states.get(&child.id).unwrap();
    let symbol = child.edge_symbol;

    match &symbol {
      SymbolID::Production(production_id, _) => {
        let production_bytecode_id = t.g.productions.get(production_id).unwrap().bytecode_id;

        if child.is(TransitionStateType::I_PASS) {
          strings.push(format!(
            "assert PRODUCTION [ {} ] ( pass )",
            production_bytecode_id,
            // get_production_plain_name(production_id, grammar),
          ));
        } else {
          let state_name = state.get_name();

          strings.push(format!(
            "assert PRODUCTION [ {}  ] ( goto state [ {} ]{})",
            production_bytecode_id,
            state_name,
            // state_name,
            post_amble
          ));
        }
      }
      _ => {}
    }
  }

  if t.non_trivial_root {
    if let Some(root_production) = t.get_first_prod_id() {
      strings.push(format!(
        "
on fail state [ {}_goto_failed ]
    assert PRODUCTION [ {} ] ( pass )
",
        entry_name,
        t.g.productions.get(&root_production).unwrap().bytecode_id
      ))
    }
  }

  let code = strings.join("\n");

  if code.is_empty() {
    panic!("[GOTO] Empty state generated!");
  }

  Box::new(
    IRState {
      comment,
      code,
      id: state_name,
      graph_id: node.id.to_goto_id(),
      state_type: if t.is_scanner { ScannerGoto } else { ProductionGoto },
      ..Default::default()
    }
    .into_hashed(),
  )
}

fn create_intermediate_state(
  node: &TransitionGraphNode,
  t: &TransitionPack,
  resolved_states: &BTreeMap<TGNId, Box<IRState>>,
  children_tables: &Vec<BTreeSet<TGNId>>,
  entry_name: &String,
  errors: &mut Vec<HCError>,
) -> Box<IRState> {
  let mut strings = vec![];
  let mut comment = format!("[{}][{:?}]", node.id, node.parent);
  let mut item_set = BTreeSet::new();
  let mut peek_type: PeekType = PeekType::None;
  let mut is_token_assertion = false;
  let mut children = get_children(t, node, children_tables);

  if node.linked_to_self() {
    children.push(node);
  }

  let (mut post_amble, state_name, mut stack_depth, mut state_type) = fun_name(node, entry_name, t);

  if node.is(TransitionStateType::I_FAIL) {
    strings.push("fail".to_string());
  } else if node.is(TransitionStateType::O_FORK) {
    if state_type == Undefined {
      state_type = ForkState;
    }
    let mut child_hashes = vec![];

    // Manually create intermediate nodes to handle each initial
    // "forked-to" state that would otherwise be skipped.

    for child in &children {
      let child_state = resolved_states.get(&child.id).unwrap();
      child_hashes
        .push(format!("state [ {} ]", IRState::get_state_name_from_hash(child_state.get_hash(),)));
    }

    strings.push(format!(
      "fork to ( {} ) to complete prod {}",
      &child_hashes.join(" "),
      t.get_first_prod_id().unwrap()
    ));
  } else {
    state_type = if state_type == Undefined {
      if t.is_scanner {
        ScannerIntermediateState
      } else {
        ProductionIntermediateState
      }
    } else {
      state_type
    };

    let single_child = children.len() == 1;
    let mut max_child_depth = 0;

    /// TODO: Isolate and build goto states.
    #[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
    enum GroupType {
      Goto,
      TerminalTransition,
      ProductionCall,
      Recovery,
      Default,
    }

    use GroupType::*;

    /// Group children together to handle cases where multiple children of the same
    /// type need further refinement.
    for (group_type, group) in
      hash_group_btreemap(children.to_vec(), |_, c| match (c.prod_sym, c.edge_symbol) {
        (_, SymbolID::Production(..)) => Goto,
        (Some(_), ..) => ProductionCall,
        (None, SymbolID::Recovery) => Recovery,
        (None, SymbolID::EndOfFile) | (None, SymbolID::Default) => Default,
        _ => TerminalTransition,
      })
    {
      for child in &group {
        if *child == node {
          continue;
        }
        for item in &child.items {
          item_set.insert(*item);
        }
        comment += &format!(
          "\n{}",
          &child.items.iter().map(|i| i.debug_string(&t.g)).collect::<Vec<_>>().join("\n")
        );
      }
      match group_type {
        Goto => {
          let state_name = resolved_states.get(&node.id.to_goto_id()).unwrap().get_name();
          post_amble = format!(" then goto [{}]", state_name) + &post_amble;
        }
        Default if group.len() > 1 => {
          if t.is_scanner {
            let defined_branches = group
              .iter()
              .filter(
                |n| matches!(n.items[0].get_origin(), OriginData::Symbol(sym) if sym.is_defined()),
              )
              .collect::<Vec<_>>();

            if !defined_branches.is_empty() {
              // Use the first defined symbol. TODO - Define and use hierarchy rules to determine the best branch
              // to use as default
              let child = defined_branches[0];

              let shift = (if child.is(TransitionStateType::I_SHIFT) { "shift then " } else { "" })
                .to_string();

              let end_string = create_reduce_string(child, &t.g, t.is_scanner);

              strings.push(match single_child {
                true => format!("{}{}{}", shift, end_string, post_amble),
                false => {
                  format!("default ( {}{}{} )", shift, end_string, post_amble)
                }
              })
            } else {
              for child in group {
                for item in &child.items {
                  eprintln!("{}", item.debug_string(&t.g));
                }
              }

              // Group based on originating symbol
              panic!("Multiple DEFAULT branches found!")
            }
          } else {
            panic!("Multiple DEFAULT branches found!")
          }
        }
        _ => {
          let group_len = group.len();
          for child in group {
            let state_name = (child != node)
              .then(|| {
                let child_state = resolved_states.get(&child.id).unwrap();
                max_child_depth = max_child_depth.max(child_state.get_stack_depth() + 1);
                child_state.get_name()
              })
              .unwrap_or(String::from("%%%%"));

            match group_type {
              Default => {
                let shift =
                  (if child.is(TransitionStateType::I_SHIFT) { "shift then " } else { "" })
                    .to_string();

                strings.push(if child.is(TransitionStateType::I_FAIL) {
                  match single_child {
                    true => format!("fail"),
                    false => {
                      format!("default ( fail )")
                    }
                  }
                } else {
                  match single_child {
                    true => format!("{}goto state [ {} ]{}", shift, state_name, post_amble),
                    false => {
                      format!("default ( {}goto state [ {} ]{} )", shift, state_name, post_amble)
                    }
                  }
                });
              }
              ProductionCall => {
                // Productions
                if let SymbolID::Production(production_id, ..) =
                  child.prod_sym.unwrap_or(child.edge_symbol)
                {
                  strings.push(
                    match (
                      single_child,
                      group_len,
                      matches!(child.edge_symbol, SymbolID::Production(..)),
                    ) {
                      (false /* not a single child */, len, true) if len > 1 => {
                        unreachable!("Too many unqualified production calls!");
                      }
                      (false /* not a single child */, ..) => {
                        is_token_assertion = true;
                        let symbol_id = child.edge_symbol;
                        let assertion_type = (if child.is(TransitionStateType::O_PEEK_SHIFT) {
                          "assert peek"
                        } else {
                          "assert"
                        })
                        .to_string();

                        if child.is(TransitionStateType::O_PEEK_SHIFT) {
                          if (node.is(TransitionStateType::O_PEEK_SHIFT)) {
                            peek_type = PeekType::PeekContinue;
                          } else {
                            peek_type = PeekType::PeekStart;
                          }
                        }

                        let (symbol_bytecode_id, assert_class) = if (!t.is_scanner) {
                          (symbol_id.bytecode_id(Some(&t.g)), "TOKEN")
                        } else {
                          symbol_id.shift_type(&t.g)
                        };

                        format!(
                          "{} {} [ {} ] ( goto state [ {} ] then goto state [ {} ]{} )",
                          assertion_type,
                          assert_class,
                          symbol_bytecode_id,
                          t.g.productions.get(&production_id).unwrap().guid_name,
                          state_name,
                          post_amble
                        )
                      }
                      (true /* is a single child */, ..) => {
                        format!(
                          "goto state [ {} ] then goto state [ {} ]{}",
                          t.g.productions.get(&production_id).unwrap().guid_name,
                          state_name,
                          post_amble
                        )
                      }
                    },
                  )
                }
              }
              Recovery => {
                let child_state = resolved_states.get(&child.id).unwrap();
                let state_name = child_state.get_name();
                strings.push(format!("goto state [ {} ]{}", state_name, post_amble))
              }
              TerminalTransition | _ => {
                let symbol_id = child.edge_symbol;
                is_token_assertion = true;

                let assertion_type = (if child.is(TransitionStateType::O_PEEK_SHIFT) {
                  "assert peek"
                } else {
                  "assert"
                })
                .to_string();

                if child.is(TransitionStateType::O_PEEK_SHIFT) {
                  if (node.is(TransitionStateType::O_PEEK_SHIFT)) {
                    peek_type = PeekType::PeekContinue;
                  } else {
                    peek_type = PeekType::PeekStart;
                  }
                }

                let (symbol_id, assert_class) = if (!t.is_scanner) {
                  (symbol_id.bytecode_id(Some(&t.g)), "TOKEN")
                } else {
                  symbol_id.shift_type(&t.g)
                };

                let shift =
                  (if child.is(TransitionStateType::I_SHIFT) { "shift then " } else { "" })
                    .to_string();

                strings.push(format!(
                  "{} {} [ {} ] ( {}goto state [ {} ]{} )",
                  assertion_type, assert_class, symbol_id, shift, state_name, post_amble
                ));
              }
            }
          }
        }
      }
    }
    stack_depth += max_child_depth;
  }

  if t.is_scanner {
    let code = strings.join("\n");

    if code.is_empty() {
      errors.push(HCError::from(format!(
        "[BRANCH] Empty scanner state generated! [{}] [{}] {:?} {}",
        comment,
        t.root_prod_ids
          .iter()
          .map(|s| { t.g.get_production_plain_name(s) })
          .collect::<Vec<_>>()
          .join("   \n"),
        children_tables.get(node.id.usize()).cloned().unwrap_or_default(),
        node.debug_string(&t.g)
      )));
    }

    Box::new(
      IRState {
        comment,
        code,
        id: state_name,
        graph_id: node.id,
        state_type,
        stack_depth,
        peek_type,
        ..Default::default()
      }
      .into_hashed(),
    )
  } else {
    let (normal_symbol_set, skip_symbols_set, _) =
      get_symbols_from_items(item_set.clone(), &t.g, None);

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        strings.push(format!("skip [ {} ]", symbol_id.bytecode_id(Some(&t.g)),))
      }
    }

    let code = strings.join("\n");

    if code.is_empty() {
      errors.push(HCError::from(format!(
        "[BRANCH] Empty state generated! [{}] [{}] {:?} {}",
        comment,
        t.root_prod_ids
          .iter()
          .map(|s| { t.g.get_production_plain_name(s) })
          .collect::<Vec<_>>()
          .join("   \n"),
        children_tables.get(node.id.usize()).cloned().unwrap_or_default(),
        node.debug_string(&t.g)
      )));
    }

    Box::new(
      IRState {
        comment,
        code,
        id: state_name,
        graph_id: node.id,
        normal_symbols: normal_symbol_set.into_iter().collect(),
        skip_symbols: skip_symbols_set.into_iter().collect(),
        state_type,
        stack_depth,
        peek_type,
        ..Default::default()
      }
      .into_hashed(),
    )
  }
}

fn get_children<'a>(
  t: &'a TransitionPack,
  node: &TransitionGraphNode,
  children_tables: &Vec<BTreeSet<TGNId>>,
) -> Vec<&'a TransitionGraphNode> {
  children_tables
    .get(node.id.usize())
    .cloned()
    .unwrap_or_default()
    .iter()
    .map(|c| t.get_node(*c))
    .collect::<Vec<_>>()
}

fn create_reduce_string(node: &TransitionGraphNode, g: &GrammarStore, is_scanner: bool) -> String {
  match (node.items.first(), is_scanner, node.is(TransitionStateType::I_PASS)) {
    (None, false /* not scanner */, true /* default pass state */) => "pass".to_string(),
    (Some(item), true /* is scanner */, false) => {
      let body = g.bodies.get(&item.get_body_id()).unwrap();
      let production = g.productions.get(&body.prod_id).unwrap();
      let production_id = production.bytecode_id;
      match node.items[0].get_origin() {
        OriginData::Symbol(sym) => {
          format!(
            "assign token [ {} ] then set prod to {}",
            sym.bytecode_id(Some(g)),
            production_id
          )
        }
        _ => format!("set prod to {}", production_id),
      }
    }
    (Some(item), false /* not scanner */, false) => {
      let body = g.bodies.get(&item.get_body_id()).unwrap();
      let production = g.productions.get(&body.prod_id).unwrap();
      format!(
        "set prod to {} then reduce {} symbols to body {}",
        production.bytecode_id, body.len, body.bc_id,
      )
    }
    _ => panic!("Invalid Leaf Node"),
  }
}

fn create_reduce_state(
  node: &TransitionGraphNode,
  g: &GrammarStore,
  is_scanner: bool,
) -> Box<IRState> {
  let code = if node.is(TransitionStateType::I_OUT_OF_SCOPE) {
    "fail".to_string()
  } else {
    create_reduce_string(node, g, is_scanner)
  };

  let item_strings = node.items.iter().map(|i| i.debug_string(g)).collect::<Vec<_>>();

  if code.is_empty() {
    panic!("[REDUCE] Empty state generated!");
  }

  let comment = if node.is(TransitionStateType::I_FAIL) {
    format!("[{}] Out of production scope --- {:?}", node.id, item_strings)
  } else {
    item_strings.join("\n")
  };

  Box::new(
    IRState {
      code,
      comment,
      graph_id: node.id,
      state_type: if is_scanner { ProductionEndState } else { ScannerEndState },
      ..Default::default()
    }
    .into_hashed(),
  )
}

fn get_goto_name(entry_name: &String) -> String {
  format!("{}_goto", entry_name)
}

fn create_post_amble(entry_name: &String, g: &GrammarStore) -> String {
  format!(" then goto state [ {}_goto ]", entry_name)
}
