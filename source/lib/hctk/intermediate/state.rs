//! Methods for constructing IRStates from a grammar
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::format;
use std::thread;

use super::transition_graph::construct_goto;
use super::transition_graph::construct_recursive_descent;
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
use crate::types::PeekType;
use crate::types::ProductionId;
use crate::types::SymbolID;
use crate::types::TransitionGraphNode;
use crate::types::TransitionMode;
use crate::types::TransitionPack;
use crate::types::TransitionStateType;

type IROutput = Vec<IRState>;
/// Compiles all production in the `grammar` into unique IRStates
pub fn compile_states(
  grammar: &GrammarStore,
  num_of_threads: usize,
) -> BTreeMap<String, IRState>
{
  let mut deduped_states = BTreeMap::new();

  let productions_ids = grammar.production_table.keys().cloned().collect::<Vec<_>>();

  let work_chunks = productions_ids.chunks(num_of_threads).collect::<Vec<_>>();

  for state in {
    thread::scope(|s| {
      work_chunks
        .iter()
        .map(|productions| {
          s.spawn(|| {
            let mut deduped_states = BTreeMap::new();
            let mut scanner_names = HashSet::new();

            for production_id in *productions {
              let states = generate_production_states(production_id, grammar);

              for state in states {
                if let Some(name) = state.get_scanner_state_name() {
                  if scanner_names.insert(name) {
                    for state in generate_scanner_intro_state(
                      state.get_scanner_symbol_set().unwrap(),
                      grammar,
                    ) {
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

pub fn generate_scanner_intro_state(
  symbols: BTreeSet<SymbolID>,
  grammar: &GrammarStore,
) -> IROutput
{
  let symbol_items = symbols
    .iter()
    .flat_map(|s| {
      let (_, production_id, ..) = get_scanner_info_from_defined(s, grammar);
      get_production_start_items(&production_id, grammar)
    })
    .collect::<Vec<_>>();

  let name = format!("scan_{:02X}", hash_id_value_u64(&symbols));

  generate_states(grammar, true, &symbol_items, &name, u32::MAX)
}

pub fn generate_production_states(
  production_id: &ProductionId,
  grammar: &GrammarStore,
) -> IROutput
{
  generate_states(
    grammar,
    get_production(production_id, grammar).is_scanner,
    &get_production_start_items(production_id, grammar),
    &get_production(production_id, grammar).guid_name,
    get_production(production_id, grammar).bytecode_id,
  )
}

fn generate_states(
  grammar: &GrammarStore,
  is_scanner: bool,
  start_items: &[Item],
  entry_state_name: &String,
  production_id: u32,
) -> IROutput
{
  let mut output: IROutput = Vec::new();

  let recursive_descent_data =
    construct_recursive_descent(grammar, is_scanner, start_items);

  output.append(&mut process_transition_nodes(
    &recursive_descent_data,
    grammar,
    entry_state_name,
    production_id,
  ));

  let goto_data = construct_goto(
    grammar,
    is_scanner,
    start_items,
    &recursive_descent_data.goto_items.into_iter().collect::<Vec<_>>(),
  );

  if !goto_data.leaf_nodes.is_empty() {
    output.append(&mut process_transition_nodes(
      &goto_data,
      grammar,
      entry_state_name,
      production_id,
    ));
  } else {
    output.push(create_passing_goto_state(entry_state_name, is_scanner));
  }

  output
}

fn process_transition_nodes<'a>(
  tpack: &'a TransitionPack,
  grammar: &GrammarStore,
  entry_state_name: &String,
  production_id: u32,
) -> Vec<IRState>
{
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

    if node.sym != SymbolID::EndOfFile {
      for state in {
        if tpack.mode == TransitionMode::GoTo && node.id == 0 {
          vec![create_goto_start_state(
            grammar,
            tpack.is_scanner,
            &output,
            children_lookup,
            &tpack.root_productions,
            entry_state_name,
          )]
        } else {
          create_intermediate_state(
            node,
            grammar,
            tpack.is_scanner,
            &output,
            children_lookup,
            &tpack.mode,
            entry_state_name,
            production_id,
          )
        }
      } {
        output.insert(state.get_graph_id(), state);
      }
    } else {
      // End states are discarded at the end, so we will only use this
      // retrieve state's hash for parent states.
      output.insert(node_id, create_end_state(node, grammar, tpack.is_scanner));
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

fn create_fail_state(production_id: &ProductionId, grammar: &GrammarStore) {}

fn get_goto_name(entry_state_name: &String) -> String
{
  format!("{}_goto", entry_state_name)
}

fn create_passing_goto_state(entry_state_name: &String, is_scanner: bool) -> IRState
{
  IRState {
    code: "pass".to_string(),
    name: get_goto_name(entry_state_name),
    state_type: if is_scanner {
      ScannerGoto
    } else {
      ProductionGoto
    },
    ..Default::default()
  }
  .into_hashed()
}

fn create_goto_start_state(
  grammar: &GrammarStore,
  is_scanner: bool,
  resolved_states: &BTreeMap<usize, IRState>,
  children: &[&TransitionGraphNode],
  root_productions: &BTreeSet<ProductionId>,
  entry_state_name: &String,
) -> IRState
{
  let mut strings = vec![];
  let mut comment = String::new();
  let post_amble = create_post_amble(entry_state_name, grammar);
  let mut contains_root_production = false;

  for child in children {
    let state = resolved_states.get(&child.id).unwrap();
    let symbol = child.sym;

    match &symbol {
      SymbolID::Production(production_id, _) => {
        contains_root_production =
          root_productions.contains(production_id) || contains_root_production;

        let production_bytecode_id =
          grammar.production_table.get(production_id).unwrap().bytecode_id;

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

  if contains_root_production && !is_scanner {
    if let Some(root_production) = root_productions.first() {
      strings.push(format!(
        "
on fail state [ {}_goto_failed ]
    assert PRODUCTION [ {} ] ( pass )
",
        entry_state_name,
        grammar.production_table.get(root_production).unwrap().bytecode_id
      ))
    }
  }

  IRState {
    comment,
    code: strings.join("\n"),
    name: get_goto_name(entry_state_name),
    state_type: if is_scanner {
      ScannerGoto
    } else {
      ProductionGoto
    },
    ..Default::default()
  }
  .into_hashed()
}

fn create_intermediate_state(
  node: &TransitionGraphNode,
  grammar: &GrammarStore,
  is_scanner: bool,
  resolved_states: &BTreeMap<usize, IRState>,
  children: &[&TransitionGraphNode],
  mode: &TransitionMode,
  entry_state_name: &String,
  production_id: u32,
) -> Vec<IRState>
{
  let mut strings = vec![];
  let mut comment = String::new();
  let mut item_set = BTreeSet::new();
  let mut states = vec![];
  let mut peek_type: PeekType = PeekType::None;
  let mut is_token_assertion = false;

  let (post_amble, state_name, mut stack_depth, mut state_type) = {
    if node.id == 0 {
      (
        create_post_amble(entry_state_name, grammar),
        entry_state_name.clone(),
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

  if node.is(TransitionStateType::I_FORK) {
    if state_type == Undefined {
      state_type = ForkState;
    }
    let mut child_hashes = vec![];

    // Manually create intermediate nodes to handle each initial
    // "forked-to" state that would otherwise be skipped.

    for child in children.iter() {
      let mut state = create_intermediate_state(
        &TransitionGraphNode::temp(
          child,
          SymbolID::Undefined,
          TransitionGraphNode::OrphanIndex,
          vec![],
        ),
        grammar,
        is_scanner,
        resolved_states,
        &[*child],
        mode,
        entry_state_name,
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

    for child in children {
      let child_state = resolved_states.get(&child.id).unwrap();
      let symbol_id = child.sym;
      let symbol_string = symbol_id.to_string(grammar);
      let state_name = child_state.get_name();

      max_child_depth = max_child_depth.max(child_state.get_stack_depth());

      for item in &child.items {
        item_set.insert(item);
      }

      if *mode == TransitionMode::GoTo {
        comment += &format!("   node id: {}", node.id);
        comment += "\n GOTO ";
      }

      comment += &format!(
        "\n{}",
        &child
          .items
          .iter()
          .map(|i| i.debug_string(grammar))
          .collect::<Vec<_>>()
          .join("\n")
      );

      strings.push(match &symbol_id {
        SymbolID::Production(production_id, _) => {
          max_child_depth = max_child_depth.max(child_state.get_stack_depth() + 1);
          format!(
            "goto state [ {} ] then goto state [ {} ]{}",
            grammar.production_table.get(production_id).unwrap().guid_name,
            state_name,
            post_amble
          )
        }
        SymbolID::Recovery => {
          format!("goto state [ {} ]{}", state_name, post_amble)
        }
        SymbolID::EndOfFile => {
          let end_string = create_end_string(child, grammar, is_scanner);

          match single_child {
            true => format!("{}{}", end_string, post_amble),
            false => {
              format!("default ( {}{} )", end_string, post_amble)
            }
          }
        }
        SymbolID::Default => {
          let consume = (if child.is(TransitionStateType::I_CONSUME) {
            "consume then "
          } else {
            ""
          })
          .to_string();

          match single_child {
            true => format!("{}goto state [ {} ]{}", consume, state_name, post_amble),
            false => format!(
              "default ( {}goto state [ {} ]{} )",
              consume, state_name, post_amble
            ),
          }
        }
        _ => {
          is_token_assertion = true;

          let assertion_type = (if child.is(TransitionStateType::O_PEEK) {
            "assert peek"
          } else {
            "assert"
          })
          .to_string();

          if child.is(TransitionStateType::O_PEEK) {
            if (node.is(TransitionStateType::O_PEEK)) {
              peek_type = PeekType::PeekContinue;
            } else {
              peek_type = PeekType::PeekStart;
            }
          }

          let (symbol_id, assert_class) = if (!is_scanner) {
            (symbol_id.bytecode_id(grammar), "TOKEN")
          } else {
            get_symbol_consume_type(&symbol_id, grammar)
          };

          let consume = (if child.is(TransitionStateType::I_CONSUME) {
            "consume then "
          } else {
            ""
          })
          .to_string();

          format!(
            "{} {} [ {} ] ( {}goto state [ {} ]{} )",
            assertion_type,
            assert_class,
            // symbol_string,
            symbol_id,
            consume,
            state_name,
            post_amble
          )
        }
      });

      stack_depth += max_child_depth;
    }
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
    let (normal_symbol_set, skip_symbols_set) = get_symbols_from_items(item_set, grammar);

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        strings.push(format!("skip [ {} ]", symbol_id.bytecode_id(grammar),))
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
  item_set: BTreeSet<&Item>,
  grammar: &GrammarStore,
) -> (BTreeSet<SymbolID>, BTreeSet<SymbolID>)
{
  let mut normal_symbol_set = BTreeSet::new();
  let mut peek_symbols_set = BTreeSet::new();

  for item in item_set {
    match item.get_symbol(grammar) {
      SymbolID::EndOfFile | SymbolID::Undefined | SymbolID::Production(..) => {}
      sym => {
        normal_symbol_set.insert(item.get_symbol(grammar));
      }
    }

    if let Some(peek_symbols) = grammar.item_peek_symbols.get(&item.to_zero_state()) {
      for peek_symbol in peek_symbols {
        peek_symbols_set.insert(*peek_symbol);
      }
    }
  }

  let peek_symbols_set =
    peek_symbols_set.difference(&normal_symbol_set).cloned().collect::<BTreeSet<_>>();

  (normal_symbol_set, peek_symbols_set)
}

fn get_symbol_consume_type(
  symbol_id: &SymbolID,
  grammar: &GrammarStore,
) -> (u32, &'static str)
{
  match symbol_id {
    SymbolID::GenericSpace
    | SymbolID::GenericHorizontalTab
    | SymbolID::GenericNewLine
    | SymbolID::GenericIdentifier
    | SymbolID::GenericNumber
    | SymbolID::GenericSymbol => (symbol_id.bytecode_id(grammar), "CLASS"),
    SymbolID::DefinedNumeric(id)
    | SymbolID::DefinedIdentifier(id)
    | SymbolID::DefinedSymbol(id) => {
      let symbol = grammar.symbols_table.get(symbol_id).unwrap();
      let id = grammar.symbols_string_table.get(symbol_id).unwrap();
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

fn create_post_amble(entry_state_name: &String, grammar: &GrammarStore) -> String
{
  format!(" then goto state [ {}_goto ]", entry_state_name)
}

fn create_end_string(
  node: &TransitionGraphNode,
  grammar: &GrammarStore,
  is_scanner: bool,
) -> String
{
  let item = node.items[0];

  let body = grammar.bodies_table.get(&item.get_body()).unwrap();

  let production = grammar.production_table.get(&body.production).unwrap();

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
      production.bytecode_id, body.length, body.bytecode_id,
    );

    state_string
  }
}

fn create_end_state(
  node: &TransitionGraphNode,
  grammar: &GrammarStore,
  is_scanner: bool,
) -> IRState
{
  let item = node.items[0];

  let body = grammar.bodies_table.get(&item.get_body()).unwrap();

  let production = grammar.production_table.get(&body.production).unwrap();

  IRState {
    code: create_end_string(node, grammar, is_scanner),
    graph_id: node.id,
    state_type: if is_scanner {
      ProductionEndState
    } else {
      ScannerEndState
    },
    ..Default::default()
  }
  .into_hashed()
}
