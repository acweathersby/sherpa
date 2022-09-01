//! Methods for constructing IRStates from a grammar
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::fmt::format;
use std::thread;

use super::transition_graph::construct_goto;
use super::transition_graph::construct_recursive_descent;
use super::transition_graph::hash_group;
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
use crate::types::Symbol;
use crate::types::SymbolID;
use crate::types::TransitionGraphNode;
use crate::types::TransitionMode;
use crate::types::TransitionPack;
use crate::types::TransitionStateType;

type IROutput = Vec<IRState>;
/// Compiles all production within the grammar into unique IRStates
pub fn compile_states(g: &GrammarStore, num_of_threads: usize) -> BTreeMap<String, IRState> {
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
    let work_chunks = productions_ids.chunks(num_of_threads).collect::<Vec<_>>();

    for state in {
      thread::scope(|s| {
        work_chunks
          .iter()
          .map(|productions| {
            s.spawn(|| {
              let mut deduped_states = BTreeMap::new();
              let mut scanner_names = BTreeSet::new();

              for production_id in *productions {
                let states = generate_production_states(production_id, g);

                for state in states {
                  if let Some(name) = state.get_scanner_state_name() {
                    if scanner_names.insert(name) {
                      for state in
                        generate_scanner_intro_state(state.get_scanner_symbol_set().unwrap(), g)
                      {
                        deduped_states.entry(state.get_name()).or_insert(state);
                      }
                    }
                  }

                  deduped_states.entry(state.get_name()).or_insert(state);
                }
              }

              deduped_states.into_values()
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

  let name = format!("scan_{:02X}", hash_id_value_u64(&symbols));

  generate_states(g, true, &symbol_items, &name, u32::MAX)
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

  let recursive_descent_data = construct_recursive_descent(g, is_scanner, start_items);

  output.append(&mut process_transition_nodes(
    &recursive_descent_data,
    g,
    entry_name,
    production_id,
  ));

  let goto_data = construct_goto(
    g,
    is_scanner,
    start_items,
    &recursive_descent_data.gotos.into_iter().collect::<Vec<_>>(),
  );

  if !goto_data.leaf_nodes.is_empty() {
    output.append(&mut process_transition_nodes(&goto_data, g, entry_name, production_id));
  } else {
    output.push(create_passing_goto_state(entry_name, is_scanner));
  }

  output
}

fn process_transition_nodes<'a>(
  tpack: &'a TransitionPack,
  g: &GrammarStore,
  entry_name: &String,
  prod_id: u32,
) -> Vec<IRState> {
  // We start at leaf nodes and make our way down to the root.
  let number_of_nodes = tpack.get_node_len();

  let mut output = BTreeMap::<usize, IRState>::new();

  let mut children_tables =
    tpack.nodes_iter().map(|_| Vec::<&'a TransitionGraphNode>::new()).collect::<Vec<_>>();

  // Starting with the leaf nodes, construct the reverse
  // edges of our transition graph, converting the relationship
  // child->parent to parent->child

  for child in tpack.nodes_iter() {
    if child.has_parent(tpack) {
      children_tables[child.parent].push(child);

      for proxy_parent in &child.proxy_parents {
        children_tables[*proxy_parent].push(child);
      }
    }
  }

  let mut nodes_pipeline = VecDeque::from_iter(tpack.leaf_nodes.iter().cloned());

  'outer: while let Some(node_id) = nodes_pipeline.pop_front() {
    if output.contains_key(&node_id) {
      // Already dealt with this node. No need to do any more work.
      continue;
    }

    let children_lookup = children_tables.get(node_id).unwrap();

    // Ensure dependencies are met.
    for child in children_lookup {
      if !output.contains_key(&child.id) {
        // Push dependency to be processed, which will cause this node
        // be pushed back into the queue after it is processed
        nodes_pipeline.push_back(child.id);

        continue 'outer;
      }
    }

    let node = tpack.get_node(node_id);

    if node.terminal_symbol != SymbolID::EndOfFile {
      for state in {
        if tpack.mode == TransitionMode::GoTo && node.id == 0 {
          vec![create_goto_start_state(
            g,
            tpack.is_scanner,
            &output,
            children_lookup,
            &tpack.root_prods,
            entry_name,
          )]
        } else {
          create_intermediate_state(node, g, tpack, &output, &children_tables, entry_name, prod_id)
        }
      } {
        output.insert(state.get_graph_id(), state);
      }
    } else {
      // End states are discarded at the end, so we will only use this
      // retrieve state's hash for parent states.
      output.insert(node_id, create_end_state(node, g, tpack.is_scanner));
    }

    if !node.is_orphan(tpack) {
      nodes_pipeline.push_back(node.parent);

      for proxy_parent in &node.proxy_parents {
        nodes_pipeline.push_back(*proxy_parent);
      }
    }
  }

  let mut hash_filter = BTreeSet::<u64>::new();

  output
    .into_values()
    .filter(|s| {
      !(s.get_type() == ProductionEndState || s.get_type() == ScannerEndState)
        && hash_filter.insert(s.get_hash())
    })
    .collect::<Vec<_>>()
}

fn create_fail_state(production_id: &ProductionId, g: &GrammarStore) {}

fn get_goto_name(entry_name: &String) -> String {
  format!("{}_goto", entry_name)
}

fn create_passing_goto_state(entry_name: &String, is_scanner: bool) -> IRState {
  IRState {
    code: "pass".to_string(),
    name: get_goto_name(entry_name),
    state_type: if is_scanner { ScannerGoto } else { ProductionGoto },
    ..Default::default()
  }
  .into_hashed()
}

fn create_goto_start_state(
  g: &GrammarStore,
  is_scanner: bool,
  resolved_states: &BTreeMap<usize, IRState>,
  children: &[&TransitionGraphNode],
  root_productions: &BTreeSet<ProductionId>,
  entry_name: &String,
) -> IRState {
  let mut strings = vec![];
  let mut comment = String::new();
  let post_amble = create_post_amble(entry_name, g);
  let mut contains_root_production = false;

  for child in children {
    let state = resolved_states.get(&child.id).unwrap();
    let symbol = child.terminal_symbol;

    match &symbol {
      SymbolID::Production(production_id, _) => {
        contains_root_production =
          root_productions.contains(production_id) || contains_root_production;

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

  if contains_root_production {
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

  IRState {
    comment,
    code: strings.join("\n"),
    name: get_goto_name(entry_name),
    state_type: if is_scanner { ScannerGoto } else { ProductionGoto },
    ..Default::default()
  }
  .into_hashed()
}

fn create_intermediate_state(
  node: &TransitionGraphNode,
  g: &GrammarStore,
  t_pack: &TransitionPack,
  resolved_states: &BTreeMap<usize, IRState>,
  children_tables: &Vec<Vec<&TransitionGraphNode>>,
  entry_name: &String,
  production_id: u32,
) -> Vec<IRState> {
  let mut strings = vec![];
  let mut comment = String::new();
  let mut item_set = BTreeSet::new();
  let mut states = vec![];
  let mut peek_type: PeekType = PeekType::None;
  let mut is_token_assertion = false;
  let is_scanner = t_pack.is_scanner;
  let mode = t_pack.mode;
  static empty: Vec<&TransitionGraphNode> = vec![];
  let children = children_tables.get(node.id).unwrap_or(&empty);

  let (post_amble, state_name, mut stack_depth, mut state_type) = {
    if node.id == 0 {
      (create_post_amble(entry_name, g), entry_name.clone(), 1, match mode {
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
      })
    } else {
      (String::default(), String::default(), 0, Undefined)
    }
  };

  if node.is(TransitionStateType::I_FORK) {
    if state_type == Undefined {
      state_type = ForkState;
    }
    let mut child_hashes = vec![];

    // Manually create intermediate nodes to handle each initial
    // "forked-to" state that would otherwise be skipped.

    let mut origin = node;

    // while !(origin.is(TransitionStateType::I_PEEK_ORIGIN)) {
    //  origin = t_pack.get_node(origin.parent);
    //}

    for child in children_tables.get(origin.id).unwrap() {
      for item in &child.items {
        println!("{}", item.debug_string(g));
        item.print_blame(g);
      }
    }

    panic!("FORKED!!?!?!?!");

    for child in children.iter() {
      let mut state = create_intermediate_state(
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
        IRState::get_state_name_from_hash(state.last().unwrap().get_hash(),)
      ));

      states.append(&mut state);
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
        SymbolID::EndOfFile => 3,
        SymbolID::Default => 4,
        _ => 5,
      },
    }) {
      for child in &group {
        let child_state = resolved_states.get(&child.id).unwrap();
        max_child_depth = max_child_depth.max(child_state.get_stack_depth());

        for item in &child.items {
          item_set.insert(*item);
        }

        if mode == TransitionMode::GoTo {
          comment += &format!("   node id: {}", node.id);
          comment += "\n GOTO ";
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
            let symbol_id = child.terminal_symbol;
            let symbol_string = symbol_id.to_string(g);
            let state_name = child_state.get_name();

            max_child_depth = max_child_depth.max(child_state.get_stack_depth() + 1);

            if !single_child {
              if group.len() > 1 && matches!(child.terminal_symbol, SymbolID::Production(..)) {
                panic!("Too many unqualified production calls!");
              } else {
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

                let (symbol_bytecode_id, assert_class) = if (!is_scanner) {
                  (symbol_id.bytecode_id(g), "TOKEN")
                } else {
                  get_symbol_consume_type(&symbol_id, g)
                };

                strings.push(format!(
                  "{} {} [ {} /* {} */ ] ( goto state [ {} ] then goto state [ {} ]{} )",
                  assertion_type,
                  assert_class,
                  symbol_bytecode_id,
                  symbol_string,
                  g.productions.get(&production_id).unwrap().guid_name,
                  state_name,
                  post_amble
                ));
              }
            } else {
              strings.push(format!(
                "goto state [ {} ] then goto state [ {} ]{}",
                g.productions.get(&production_id).unwrap().guid_name,
                state_name,
                post_amble
              ));
            }
          } else {
            panic!("WTF!?!?!");
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
          SymbolID::EndOfFile => {
            if group.len() > 1 && is_scanner {
              // Defined have higher precedence than
              // production tokens

              let defined_branches = group
                .iter()
                .filter(|n| matches!(n.items[0].get_origin(), OriginData::Symbol(sym) if sym.isDefinedSymbol()))
                .collect::<Vec<_>>();

              let defined_generics = group.iter()
              .filter(|n| matches!(n.items[0].get_origin(), OriginData::Symbol(sym) if matches!(sym, SymbolID::GenericNewLine)))
              .collect::<Vec<_>>();

              if !defined_branches.is_empty() {
                // Use the first defined symbol. TODO - Define and use hierarchy rules to determine the best branch
                // to use as default
                let child = defined_branches[0];

                let end_string = create_end_string(child, g, is_scanner);
                strings.push(match single_child {
                  true => format!("{}{}", end_string, post_amble),
                  false => {
                    format!("default ( {}{} )", end_string, post_amble)
                  }
                })
              } else {
                let mut origin = group[0];

                while !(origin.is(TransitionStateType::I_PEEK_ORIGIN)) {
                  origin = t_pack.get_node(origin.parent);
                }

                for child in children_tables.get(origin.id).unwrap() {
                  for item in &child.items {
                    item.print_blame(g);
                  }
                }

                // Group based on originating symbol
                panic!("Multiple DEFAULT branches found!")
              }
            } else {
              for child in group {
                let end_string = create_end_string(child, g, is_scanner);
                strings.push(match single_child {
                  true => format!("{}{}", end_string, post_amble),
                  false => {
                    format!("default ( {}{} )", end_string, post_amble)
                  }
                })
              }
            }
          }
          SymbolID::Default => {
            for child in group {
              let child_state = resolved_states.get(&child.id).unwrap();
              let state_name = child_state.get_name();
              let consume =
                (if child.is(TransitionStateType::I_CONSUME) { "consume then " } else { "" })
                  .to_string();

              strings.push(match single_child {
                true => format!("{}goto state [ {} ]{}", consume, state_name, post_amble),
                false => {
                  format!("default ( {}goto state [ {} ]{} )", consume, state_name, post_amble)
                }
              });
            }
          }
          _ => {
            for child in group {
              let child_state = resolved_states.get(&child.id).unwrap();
              let symbol_id = child.terminal_symbol;
              let symbol_string = symbol_id.to_string(g);
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
                (symbol_id.bytecode_id(g), "TOKEN")
              } else {
                get_symbol_consume_type(&symbol_id, g)
              };

              let consume =
                (if child.is(TransitionStateType::I_CONSUME) { "consume then " } else { "" })
                  .to_string();

              strings.push(format!(
                "{} {} [ {} /* {} */ ] ( {}goto state [ {} ]{} )",
                assertion_type,
                assert_class,
                symbol_id,
                symbol_string,
                consume,
                state_name,
                post_amble
              ));
            }
          }
        }
      }
    }

    stack_depth += max_child_depth;
  }

  let state = if is_scanner {
    IRState {
      comment,
      code: strings.join("\n"),
      name: state_name,
      graph_id: node.id,
      state_type,
      stack_depth,
      peek_type,
      ..Default::default()
    }
    .into_hashed()
  } else {
    let (normal_symbol_set, skip_symbols_set, _) =
      get_symbols_from_items(item_set.clone(), g, None);

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        strings.push(format!("skip [ {} ]", symbol_id.bytecode_id(g),))
      }
    }
    let have_symbols = !normal_symbol_set.is_empty();

    IRState {
      comment,
      code: strings.join("\n"),
      name: state_name,
      graph_id: node.id,
      normal_symbols: normal_symbol_set.into_iter().collect(),
      skip_symbols: skip_symbols_set.into_iter().collect(),
      state_type,
      stack_depth,
      peek_type,
      ..Default::default()
    }
    .into_hashed()
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
  let mut peek_symbols_set = BTreeSet::new();
  let mut seen = seen.unwrap_or_default();

  for item in item_set {
    match item.get_symbol(g) {
      SymbolID::Production(production_id, ..) => {
        if !seen.insert(production_id) {
          continue;
        }

        let production_items =
          get_production_start_items(&production_id, g).into_iter().collect::<BTreeSet<_>>();

        let (mut norm, mut peek, new_seen) =
          get_symbols_from_items(production_items, g, Some(seen));

        seen = new_seen;

        normal_symbol_set.append(&mut norm);
        peek_symbols_set.append(&mut peek);
      }
      SymbolID::EndOfFile | SymbolID::Undefined => {}
      sym => {
        normal_symbol_set.insert(item.get_symbol(g));
      }
    }

    if let Some(peek_symbols) = g.item_peek_symbols.get(&item.to_zero_state()) {
      for peek_symbol in peek_symbols {
        peek_symbols_set.insert(*peek_symbol);
      }
    }
  }

  let peek_symbols_set =
    peek_symbols_set.difference(&normal_symbol_set).cloned().collect::<BTreeSet<_>>();

  (normal_symbol_set, peek_symbols_set, seen)
}

fn get_symbol_consume_type(symbol_id: &SymbolID, g: &GrammarStore) -> (u32, &'static str) {
  match symbol_id {
    SymbolID::GenericSpace
    | SymbolID::GenericHorizontalTab
    | SymbolID::GenericNewLine
    | SymbolID::GenericIdentifier
    | SymbolID::GenericNumber
    | SymbolID::GenericSymbol => (symbol_id.bytecode_id(g), "CLASS"),
    SymbolID::DefinedNumeric(id)
    | SymbolID::DefinedIdentifier(id)
    | SymbolID::DefinedSymbol(id) => {
      let symbol = g.symbols.get(symbol_id).unwrap();
      let id = g.symbol_strings.get(symbol_id).unwrap();
      let sym_char = id.as_bytes()[0];
      if symbol.byte_length > 1 || sym_char > 128 {
        (symbol.bytecode_id, "CODEPOINT")
      } else {
        (sym_char as u32, "BYTE")
      }
    }
    _ => (0, "BYTE"),
  }
}

fn create_post_amble(entry_name: &String, g: &GrammarStore) -> String {
  format!(" then goto state [ {}_goto ]", entry_name)
}

fn create_end_string(node: &TransitionGraphNode, g: &GrammarStore, is_scanner: bool) -> String {
  let item = node.items[0];

  let body = g.bodies.get(&item.get_body()).unwrap();

  let production = g.productions.get(&body.prod).unwrap();

  if !item.at_end() {
    panic!("Expected state to be in end state")
  } else if is_scanner {
    let symbol_id = production.symbol_bytecode_id;
    let production_id = production.bytecode_id;

    if symbol_id == 0 {
      format!("set prod to {}", production_id)
    } else {
      format!("assign token [ {} ] then set prod to {}", symbol_id, production_id)
    }
  } else {
    let state_string = format!(
      "set prod to {} then reduce {} symbols to body {}",
      production.bytecode_id, body.len, body.bc_id,
    );

    state_string
  }
}

fn create_end_state(node: &TransitionGraphNode, g: &GrammarStore, is_scanner: bool) -> IRState {
  let item = node.items[0];

  let body = g.bodies.get(&item.get_body()).unwrap();

  let production = g.productions.get(&body.prod).unwrap();

  IRState {
    code: create_end_string(node, g, is_scanner),
    graph_id: node.id,
    state_type: if is_scanner { ProductionEndState } else { ScannerEndState },
    ..Default::default()
  }
  .into_hashed()
}
