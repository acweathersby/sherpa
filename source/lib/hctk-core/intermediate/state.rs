//! Methods for constructing IRStates from a grammar
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::format;
use std::thread;

use super::transition::construct_goto;
use super::transition::construct_recursive_descent;
use super::transition::get_valid_starts;
use super::transition::hash_group;
use crate::grammar::create_closure;
use crate::grammar::get_closure_cached;
use crate::grammar::get_production;
use crate::grammar::get_production_plain_name;
use crate::grammar::get_production_start_items;
use crate::grammar::get_scanner_info_from_defined;
use crate::grammar::hash_id_value_u64;
use crate::types::GrammarStore;
use crate::types::IRState;
use crate::types::IRStateType;
use crate::types::IRStateType::*;
use crate::types::Item;
use crate::types::OriginData;
use crate::types::PeekType;
use crate::types::ProductionId;
use crate::types::RecursionType;
use crate::types::Symbol;
use crate::types::SymbolID;
use crate::types::TransitionGraphNode;
use crate::types::TransitionMode;
use crate::types::TransitionPack;
use crate::types::TransitionStateType;

type IROutput = Vec<Box<IRState>>;
/// Compiles all production within the grammar into unique IRStates
pub fn compile_states(g: &GrammarStore, num_of_threads: usize) -> BTreeMap<String, Box<IRState>> {
  let mut deduped_states = BTreeMap::new();

  let productions_ids = g.productions.keys().cloned().collect::<Vec<_>>();

  if num_of_threads == 1 {
    let work_chunks = productions_ids.chunks(num_of_threads).collect::<Vec<_>>();
    let mut deduped_states = BTreeMap::new();
    let mut scanner_names = BTreeSet::new();

    for production_id in &productions_ids {
      let states = generate_production_states(production_id, g);

      for state in states {
        if let Some(name) = state.get_scanner_state_name() {
          if scanner_names.insert(name) {
            for state in generate_scanner_intro_state(state.get_scanner_symbol_set().unwrap(), g) {
              deduped_states.entry(state.get_name()).or_insert(state);
            }
          }
        }
        deduped_states.entry(state.get_name()).or_insert(state);
      }
    }

    for (_, state) in &mut deduped_states {
      let string = state.to_string();
      if let Err(err) = state.compile_ast() {
        panic!("\n{} {}", err, string)
      };
    }

    deduped_states
  } else {
    let work_chunks = productions_ids
      .chunks((productions_ids.len() / (num_of_threads - 1)).max(1))
      .collect::<Vec<_>>();

    for state in {
      thread::scope(|s| {
        work_chunks
          .into_iter()
          .map(|productions| {
            s.spawn(move || {
              let mut out_states = Vec::with_capacity(1024);
              let mut scanner_names = BTreeSet::new();
              let mut state_names = BTreeSet::new();

              for production_id in productions {
                let states = generate_production_states(production_id, g);

                for state in states {
                  if let Some(name) = state.get_scanner_state_name() {
                    if scanner_names.insert(name.clone()) {
                      for state in
                        generate_scanner_intro_state(state.get_scanner_symbol_set().unwrap(), g)
                      {
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
              out_states
            })
          })
          .collect::<Vec<_>>()
          .into_iter()
          .flat_map(move |s| s.join().unwrap())
          .collect::<Vec<_>>()
      })
    } {
      deduped_states.entry(state.get_name()).or_insert(state);
    }

    let mut output_states = deduped_states.iter_mut().collect::<Vec<_>>();

    thread::scope(|s| {
      let work_chunks = output_states.chunks_mut(num_of_threads);

      work_chunks
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

    deduped_states
  }
}

pub fn generate_scanner_intro_state(symbols: BTreeSet<SymbolID>, g: &GrammarStore) -> IROutput {
  let symbol_items = symbols
    .iter()
    .flat_map(|s| {
      let (_, production_id, ..) = get_scanner_info_from_defined(s, g);
      get_production_start_items(&production_id, g)
        .iter()
        .map(|i| i.to_origin(crate::types::OriginData::Symbol(*s)))
        .collect::<Vec<_>>()
    })
    .collect::<Vec<_>>();

  // Ensure there are no left recursive productions within the
  // items
  check_for_left_recursion(&symbol_items, g);

  generate_states(
    g,
    true,
    &symbol_items,
    &format!("scan_{:02X}", hash_id_value_u64(&symbols)),
    u32::MAX,
  )
}

fn check_for_left_recursion(symbol_items: &Vec<Item>, g: &GrammarStore) {
  let mut productions = HashSet::new();
  let mut seen = HashSet::new();
  let mut pipeline =
    VecDeque::from_iter(symbol_items.iter().flat_map(|i| get_closure_cached(i, g)).cloned());

  while let Some(item) = pipeline.pop_front() {
    if seen.insert(item) && !item.is_end() {
      productions.insert(item.get_prod_id(g));

      let new_item = item.increment().unwrap();

      if let SymbolID::Production(..) = new_item.get_symbol(g) {
        for item in get_closure_cached(&new_item, g) {
          pipeline.push_back(*item);
        }
      } else {
        pipeline.push_back(new_item);
      }
    }
  }

  assert!(
    productions.iter().all(|p| {
      let production = g.productions.get(p).unwrap();
      let has_left = production
        .recursion_type
        .intersects(RecursionType::LEFT_DIRECT | RecursionType::LEFT_INDIRECT);

      if has_left {
        println!(
          "{}",
          production.original_location.blame(1, 1, "this production is left recursive", None)
        );
      }

      !has_left
    }),
    "Scanner productions cannot contain left recursion!"
  );
}

pub fn generate_production_states(production_id: &ProductionId, g: &GrammarStore) -> IROutput {
  generate_states(
    g,
    get_production(production_id, g).is_scanner,
    &get_production_start_items(production_id, g)
      .into_iter()
      .map(|i| i.to_origin(OriginData::Production(*production_id)))
      .collect::<Vec<_>>(),
    &get_production(production_id, g).guid_name,
    get_production(production_id, g).bytecode_id,
  )
}

fn generate_states(
  g: &GrammarStore,
  is_scanner: bool,
  start_items: &[Item],
  entry_name: &String,
  production_id: u32,
) -> IROutput {
  let mut output: IROutput = Vec::new();
  let root_ids = start_items.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>();
  let (start_items, goto_seeds) =
    if !is_scanner { get_valid_starts(&start_items, &g) } else { (start_items.to_vec(), vec![]) };

  let recursive_descent_data =
    construct_recursive_descent(g, is_scanner, &start_items, root_ids.clone());

  output.append(&mut process_transition_nodes(
    &recursive_descent_data,
    g,
    entry_name,
    production_id,
    false,
  ));

  // Scanner states are guaranteed to be purely recursive descent compatible
  //  and thus they have no need for a GOTO path.
  if !is_scanner {
    let (goto_data, non_trivial_root_branch) =
      construct_goto(g, is_scanner, &start_items, &goto_seeds, root_ids);

    if !goto_data.leaf_nodes.is_empty() {
      output.append(&mut process_transition_nodes(
        &goto_data,
        g,
        entry_name,
        production_id,
        non_trivial_root_branch,
      ));
    } else {
      output.push(create_passing_goto_state(entry_name, is_scanner));
    }
  }

  output
}

fn process_transition_nodes<'a>(
  t_pack: &'a TransitionPack,
  g: &GrammarStore,
  entry_name: &String,
  prod_id: u32,
  non_trivial_root_branch: bool,
) -> Vec<Box<IRState>> {
  // We start at leaf nodes and make our way down to the root.
  let number_of_nodes = t_pack.get_node_len();

  let leaf_node_set = t_pack.leaf_nodes.iter().collect::<BTreeSet<_>>();

  let mut output = BTreeMap::<usize, Box<IRState>>::new();

  let mut children_tables =
    t_pack.nodes_iter().map(|_| BTreeSet::<usize>::new()).collect::<Vec<_>>();

  // Starting with the leaf nodes, construct the reverse
  // edges of our transition graph, converting the relationship
  // child->parent to parent->child

  for child in t_pack.nodes_iter() {
    if child.has_parent(t_pack) {
      children_tables[child.parent].insert(child.id);

      for proxy_parent in &child.proxy_parents {
        children_tables[*proxy_parent].insert(child.id);
      }
    }
  }

  let mut nodes_pipeline = VecDeque::from_iter(t_pack.leaf_nodes.iter().cloned());

  'outer: while let Some(node_id) = nodes_pipeline.pop_front() {
    if output.contains_key(&node_id) {
      // Already dealt with this node. No need to do any more work.
      continue;
    }

    let children_lookup = children_tables.get(node_id).unwrap();

    // Ensure dependencies are met.
    for child_id in children_lookup {
      if !output.contains_key(child_id) {
        // Push dependency to be processed, which will cause this node
        // be pushed back into the queue after it is processed
        nodes_pipeline.push_back(*child_id);

        continue 'outer;
      }
    }

    let node = t_pack.get_node(node_id);

    if !leaf_node_set.contains(&node_id) {
      if children_lookup.is_empty() {
        panic!("Childless node [{}] does is not in leaf node set!", node_id);
      }
      for state in {
        if t_pack.mode == TransitionMode::GoTo && node.id == 0 {
          vec![create_goto_start_state(
            g,
            t_pack,
            &output,
            children_lookup,
            &t_pack.root_prod_ids,
            entry_name,
            non_trivial_root_branch,
          )]
        } else {
          create_intermediate_state(node, g, t_pack, &output, &children_tables, entry_name, prod_id)
        }
      } {
        output.insert(state.get_graph_id(), state);
      }
    } else {
      // End states are discarded at the end, so we will only use this
      // retrieve state's hash for parent states.
      let state = create_reduce_state(node, g, t_pack.is_scanner);

      output.insert(state.get_graph_id(), state);
    }

    if !node.is_orphan(t_pack) {
      nodes_pipeline.push_back(node.parent);
      for proxy_parent in &node.proxy_parents {
        nodes_pipeline.push_back(*proxy_parent);
      }
    }
  }

  let mut hash_filter = BTreeSet::<u64>::new();

  output.into_values().filter(|s| hash_filter.insert(s.get_hash())).collect::<Vec<_>>()
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

fn create_goto_start_state(
  g: &GrammarStore,
  t_pack: &TransitionPack,
  resolved_states: &BTreeMap<usize, Box<IRState>>,
  children_ids: &BTreeSet<usize>,
  root_productions: &BTreeSet<ProductionId>,
  entry_name: &String,
  non_trivial_root_branch: bool,
) -> Box<IRState> {
  let is_scanner = t_pack.is_scanner;
  let mut strings = vec![];
  let mut comment = String::new();
  let post_amble = create_post_amble(entry_name, g);

  for child_id in children_ids {
    let child = t_pack.get_node(*child_id);
    let state = resolved_states.get(&child.id).unwrap();
    let symbol = child.terminal_symbol;

    match &symbol {
      SymbolID::Production(production_id, _) => {
        let production_bytecode_id = g.productions.get(production_id).unwrap().bytecode_id;

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
      _ => {
        panic!("Child symbol types should production in root goto node")
      }
    }
  }

  if non_trivial_root_branch {
    if let Some(root_production) = root_productions.first() {
      strings.push(format!(
        "
on fail state [ {}_goto_failed ]
    assert PRODUCTION [ {} ] ( pass )
",
        entry_name,
        g.productions.get(root_production).unwrap().bytecode_id
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
      id: get_goto_name(entry_name),
      state_type: if is_scanner { ScannerGoto } else { ProductionGoto },
      ..Default::default()
    }
    .into_hashed(),
  )
}

fn create_intermediate_state(
  node: &TransitionGraphNode,
  g: &GrammarStore,
  t_pack: &TransitionPack,
  resolved_states: &BTreeMap<usize, Box<IRState>>,
  children_tables: &Vec<BTreeSet<usize>>,
  entry_name: &String,
  production_id: u32,
) -> Vec<Box<IRState>> {
  let mut strings = vec![];
  let mut comment = format!("[{}][{}]", node.id, node.parent);
  let mut item_set = BTreeSet::new();
  let mut states = vec![];
  let mut peek_type: PeekType = PeekType::None;
  let mut is_token_assertion = false;
  let is_scanner = t_pack.is_scanner;
  let mode = t_pack.mode;
  let children = children_tables
    .get(node.id)
    .cloned()
    .unwrap_or_default()
    .iter()
    .map(|c| t_pack.get_node(*c))
    .collect::<Vec<_>>();

  let (post_amble, state_name, mut stack_depth, mut state_type) = {
    if node.id == 0 {
      (
        if is_scanner { "".to_string() } else { create_post_amble(entry_name, g) },
        entry_name.clone(),
        1,
        match mode {
          TransitionMode::GoTo => {
            if is_scanner {
              ScannerGoto
            } else {
              ProductionGoto
            }
          }
          TransitionMode::RecursiveDescent => {
            if is_scanner {
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
  };

  if node.is(TransitionStateType::I_FAIL) {
    strings.push("fail".to_string());
  } else if node.is(TransitionStateType::I_FORK) {
    if state_type == Undefined {
      state_type = ForkState;
    }
    let mut child_hashes = vec![];

    // Manually create intermediate nodes to handle each initial
    // "forked-to" state that would otherwise be skipped.

    let mut origin = node;

    for child in children.iter() {
      let mut new_states = create_intermediate_state(
        &TransitionGraphNode::temp(
          child,
          SymbolID::Undefined,
          TransitionGraphNode::OrphanIndex,
          vec![],
        ),
        g,
        t_pack,
        resolved_states,
        children_tables,
        entry_name,
        production_id,
      );

      child_hashes.push(format!(
        "state [ {} ]",
        IRState::get_state_name_from_hash(new_states.last().unwrap().get_hash(),)
      ));

      states.append(&mut new_states);
    }

    strings.push(format!(
      "fork to ( {} ) to complete prod {}",
      &child_hashes.join(" "),
      production_id
    ));
  } else {
    state_type = if state_type == Undefined {
      if is_scanner {
        ScannerIntermediateState
      } else {
        ProductionIntermediateState
      }
    } else {
      state_type
    };

    let single_child = children.len() == 1;
    let mut max_child_depth = 0;

    /// Group children together to handle cases where multiple children of the same
    /// type need further refinement.
    for group in hash_group(children.to_vec(), |_, c| match c.prod_sym {
      Some(..) => 1,
      None => match c.terminal_symbol {
        SymbolID::Production(..) => 1,
        SymbolID::Recovery => 2,
        SymbolID::EndOfFile | SymbolID::Default => 3,
        _ => 5,
      },
    }) {
      for child in &group {
        let child_state = resolved_states.get(&child.id).unwrap();
        max_child_depth = max_child_depth.max(child_state.get_stack_depth());

        for item in &child.items {
          item_set.insert(*item);
        }
        comment += &format!(
          "\n{}",
          &child.items.iter().map(|i| i.debug_string(g)).collect::<Vec<_>>().join("\n")
        );
      }

      if group[0].prod_sym.is_some() || matches!(group[0].terminal_symbol, SymbolID::Production(..))
      {
        for child in &group {
          if let SymbolID::Production(production_id, ..) =
            child.prod_sym.unwrap_or(child.terminal_symbol)
          {
            let child_state = resolved_states.get(&child.id).unwrap();

            max_child_depth = max_child_depth.max(child_state.get_stack_depth() + 1);

            strings.push(
              match (
                single_child,
                group.len(),
                matches!(child.terminal_symbol, SymbolID::Production(..)),
              ) {
                (false /* not a single child */, len, true) if len > 1 => {
                  unreachable!("Too many unqualified production calls!");
                }
                (false /* not a single child */, ..) => {
                  is_token_assertion = true;
                  let symbol_id = child.terminal_symbol;
                  let assertion_type =
                    (if child.is(TransitionStateType::O_PEEK) { "assert peek" } else { "assert" })
                      .to_string();

                  if child.is(TransitionStateType::O_PEEK) {
                    if (node.is(TransitionStateType::O_PEEK)) {
                      peek_type = PeekType::PeekContinue;
                    } else {
                      peek_type = PeekType::PeekStart;
                    }
                  }

                  let (symbol_bytecode_id, assert_class) = if (!is_scanner) {
                    (symbol_id.bytecode_id(Some(g)), "TOKEN")
                  } else {
                    symbol_id.shift_type(g)
                  };

                  format!(
                    "{} {} [ {} ] ( goto state [ {} ] then goto state [ {} ]{} )",
                    assertion_type,
                    assert_class,
                    symbol_bytecode_id,
                    g.productions.get(&production_id).unwrap().guid_name,
                    child_state.get_name(),
                    post_amble
                  )
                }
                (true /* is a single child */, ..) => {
                  format!(
                    "goto state [ {} ] then goto state [ {} ]{}",
                    g.productions.get(&production_id).unwrap().guid_name,
                    child_state.get_name(),
                    post_amble
                  )
                }
              },
            )
          }
        }
      } else {
        match group[0].terminal_symbol {
          SymbolID::Recovery => {
            for child in group {
              let child_state = resolved_states.get(&child.id).unwrap();
              let state_name = child_state.get_name();
              strings.push(format!("goto state [ {} ]{}", state_name, post_amble))
            }
          }
          SymbolID::Default | SymbolID::EndOfFile => {
            if group.len() > 1 {
              if is_scanner {
                let defined_branches = group
                .iter()
                .filter(|n| matches!(n.items[0].get_origin(), OriginData::Symbol(sym) if sym.is_defined()))
                .collect::<Vec<_>>();

                let defined_generics = group.iter()
              .filter(|n| matches!(n.items[0].get_origin(), OriginData::Symbol(sym) if matches!(sym, SymbolID::GenericNewLine)))
              .collect::<Vec<_>>();

                if !defined_branches.is_empty() {
                  // Use the first defined symbol. TODO - Define and use hierarchy rules to determine the best branch
                  // to use as default
                  let child = defined_branches[0];

                  let shift =
                    (if child.is(TransitionStateType::I_SHIFT) { "shift then " } else { "" })
                      .to_string();

                  let end_string = create_reduce_string(child, g, is_scanner);

                  strings.push(match single_child {
                    true => format!("{}{}{}", shift, end_string, post_amble),
                    false => {
                      format!("default ( {}{}{} )", shift, end_string, post_amble)
                    }
                  })
                } else {
                  let mut origin = group[0];

                  for child in group {
                    let par = t_pack.get_node(child.parent);
                    for item in &child.items {
                      eprintln!("{}", item.debug_string(g));
                    }
                  }

                  // Group based on originating symbol
                  panic!("Multiple DEFAULT branches found!")
                }
              } else {
                panic!("Multiple DEFAULT branches found!")
              }
            } else {
              for child in group {
                let child_state = resolved_states.get(&child.id).unwrap();
                let state_name = child_state.get_name();

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
            }
          }
          _ => {
            for child in group {
              let child_state = resolved_states.get(&child.id).unwrap();
              let symbol_id = child.terminal_symbol;
              let state_name = child_state.get_name();
              is_token_assertion = true;

              let assertion_type =
                (if child.is(TransitionStateType::O_PEEK) { "assert peek" } else { "assert" })
                  .to_string();

              if child.is(TransitionStateType::O_PEEK) {
                if (node.is(TransitionStateType::O_PEEK)) {
                  peek_type = PeekType::PeekContinue;
                } else {
                  peek_type = PeekType::PeekStart;
                }
              }

              let (symbol_id, assert_class) = if (!is_scanner) {
                (symbol_id.bytecode_id(Some(g)), "TOKEN")
              } else {
                symbol_id.shift_type(g)
              };

              let shift = (if child.is(TransitionStateType::I_SHIFT) { "shift then " } else { "" })
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

    stack_depth += max_child_depth;
  }

  let state = if is_scanner {
    let mut code = strings.join("\n");

    if code.is_empty() {
      panic!(
        "[BRANCH] Empty state generated! [{}] [{}] {:?} {}",
        comment,
        t_pack
          .root_prod_ids
          .iter()
          .map(|s| { get_production_plain_name(s, g) })
          .collect::<Vec<_>>()
          .join("   \n"),
        children_tables.get(node.id).cloned().unwrap_or_default(),
        node.debug_string(g)
      );
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
      get_symbols_from_items(item_set.clone(), g, None);

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        strings.push(format!("skip [ {} ]", symbol_id.bytecode_id(Some(g)),))
      }
    }

    let have_symbols = !normal_symbol_set.is_empty();

    let mut code = strings.join("\n");

    if code.is_empty() {
      panic!(
        "[BRANCH] Empty state generated! [{}] [{}] {:?} {}",
        comment,
        t_pack
          .root_prod_ids
          .iter()
          .map(|s| { get_production_plain_name(s, g) })
          .collect::<Vec<_>>()
          .join("   \n"),
        children_tables.get(node.id).cloned().unwrap_or_default(),
        node.debug_string(g)
      );
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
  };

  states.push(state);

  states
}

fn get_symbols_from_items(
  item_set: BTreeSet<Item>,
  g: &GrammarStore,
  seen: Option<BTreeSet<ProductionId>>,
) -> (BTreeSet<SymbolID>, BTreeSet<SymbolID>, BTreeSet<ProductionId>) {
  let mut normal_symbol_set = BTreeSet::new();
  let mut ignore_symbol_set = BTreeSet::new();
  let mut seen = seen.unwrap_or_default();

  for item in item_set {
    match item.get_symbol(g) {
      SymbolID::Production(production_id, ..) => {
        if !seen.insert(production_id) {
          continue;
        }

        let production_items =
          get_production_start_items(&production_id, g).into_iter().collect::<BTreeSet<_>>();

        let (mut norm, mut ignore, new_seen) =
          get_symbols_from_items(production_items, g, Some(seen));

        seen = new_seen;

        normal_symbol_set.append(&mut norm);
        ignore_symbol_set.append(&mut ignore);
      }
      SymbolID::EndOfFile | SymbolID::Default | SymbolID::Undefined => {}
      sym => {
        normal_symbol_set.insert(item.get_symbol(g));
      }
    }

    if let Some(ignored_symbols) = g.item_ignore_symbols.get(&item.to_zero_state()) {
      for ignored_symbol in ignored_symbols {
        ignore_symbol_set.insert(*ignored_symbol);
      }
    }
  }

  let ignore_symbol_set =
    ignore_symbol_set.difference(&normal_symbol_set).cloned().collect::<BTreeSet<_>>();

  (normal_symbol_set, ignore_symbol_set, seen)
}

fn create_reduce_string(node: &TransitionGraphNode, g: &GrammarStore, is_scanner: bool) -> String {
  match (node.items.first(), is_scanner, node.is(TransitionStateType::I_PASS)) {
    (None, false /* not scanner */, true /* default pass state */) => "pass".to_string(),
    (Some(item), true /* is scanner */, false) => {
      let body = g.bodies.get(&item.get_body()).unwrap();
      let production = g.productions.get(&body.prod).unwrap();
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
      let body = g.bodies.get(&item.get_body()).unwrap();
      let production = g.productions.get(&body.prod).unwrap();
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
