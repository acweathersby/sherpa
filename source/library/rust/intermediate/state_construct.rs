//! Construct state IR strings for a given production
//!

use std::collections::{BTreeMap, VecDeque};

use crate::{
    grammar::get_production_plain_name,
    primitives::{
        GrammarStore, IRStateString, ProductionId, SymbolID, TransitionGraphNode, TransitionMode,
        TransitionPack,
    },
};

use super::transition_tree::{construct_goto, construct_recursive_descent};

type IROutput = Vec<IRStateString>;

pub fn generate_production_states(
    production_id: &ProductionId,
    grammar: &GrammarStore,
) -> IROutput {
    let mut output: IROutput = Vec::new();

    let recursive_descent_data = construct_recursive_descent(production_id, grammar);

    /* output.append(&mut process_transition_nodes(
        &recursive_descent_data,
        grammar,
    )); */

    if recursive_descent_data.goto_items.len() > 0 {
        let goto_data = construct_goto(
            production_id,
            grammar,
            &recursive_descent_data
                .goto_items
                .into_iter()
                .collect::<Vec<_>>(),
        );

        output.append(&mut process_transition_nodes(&goto_data, grammar));

        create_fail_state(production_id, grammar, &mut output);
    }

    output
}

fn process_transition_nodes<'a>(
    tpack: &'a TransitionPack,
    grammar: &GrammarStore,
) -> Vec<IRStateString> {
    //We start at leaf nodes and make our way down to the root.
    let number_of_nodes = tpack.get_node_len();
    let mut output = BTreeMap::<usize, IRStateString>::new();
    // Construct utility information
    let mut node_hashes = Vec::<u64>::with_capacity(number_of_nodes);
    for _ in 0..number_of_nodes {
        node_hashes.push(0);
    }

    let mut children_tables = tpack
        .nodes_iter()
        .map(|_| Vec::<&'a TransitionGraphNode>::new())
        .collect::<Vec<_>>();

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
        if node_hashes[node_id] != 0 {
            // Already dealt with this node. No need to
            // do any more work.
            continue;
        }

        let children_lookup = children_tables.get(node_id).unwrap();
        //Ensure dependencies are met.
        for child in children_lookup {
            if node_hashes[child.id] == 0 {
                // Push dependency to be processed, which will cause this
                // node be pushed back into the queue after it is processed
                nodes_pipeline.push_back(child.id);
                continue 'outer;
            }
        }

        let node = tpack.get_node(node_id);

        let state = if node.sym == SymbolID::EndOfFile {
            create_end_state(node, grammar)
        } else {
            create_intermediate_state(node, &node_hashes, children_lookup, grammar, &tpack.mode)
        };

        node_hashes[node_id] = state.get_hash();

        output.insert(node_id, state);

        if !node.is_orphan(tpack) {
            nodes_pipeline.push_back(node.parent);
            for proxy_parent in &node.proxy_parents {
                nodes_pipeline.push_back(*proxy_parent);
            }
        }
    }

    output.into_values().collect::<Vec<_>>()
}

fn create_fail_state(production_id: &ProductionId, grammar: &GrammarStore, output: &mut IROutput) {}

fn create_passing_goto_state(
    production_id: &ProductionId,
    grammar: &GrammarStore,
    output: &mut IROutput,
) {
}

fn create_intermediate_state(
    node: &TransitionGraphNode,
    hashes: &[u64],
    children: &[&TransitionGraphNode],
    grammar: &GrammarStore,
    mode: &TransitionMode,
) -> IRStateString {
    let mut strings = vec![];
    let mut comment = String::new();

    for child in children {
        let hash = hashes[child.id];
        let symbol = child.sym;
        let symbol_id = symbol.bytecode_id(grammar);
        let symbol_string = symbol.to_string(grammar);

        if *mode == TransitionMode::GoTo {
            comment += &format!("   node id: {}", node.id);
            comment += "\n GOTO ";
        }

        comment += &child
            .items
            .iter()
            .map(|i| i.debug_string(grammar))
            .collect::<Vec<_>>()
            .join("\n");
        match &symbol {
            SymbolID::Production(production_id, _) => {
                strings.push(format!(
                    "goto state [ {} ] then goto state [ s{:02x} ] ",
                    get_production_plain_name(production_id, grammar),
                    hash
                ));
            }
            SymbolID::EndOfFile => {
                strings.push(format!("goto state [ s{:02x} ]", hash));
            }
            _ => {
                strings.push(format!(
                    "assert TOKEN [ /* {} */ {} ] {{ consume then goto state [ s{:02x} ] }}",
                    symbol_string, symbol_id, hash
                ));
            }
        }
    }

    IRStateString::new(&comment, &strings.join("\n"))
}

fn create_end_state(node: &TransitionGraphNode, grammar: &GrammarStore) -> IRStateString {
    let item = node.items[0];

    if !item.at_end() {
        panic!("Expected state to be in end state")
    } else {
        let body = grammar.bodies_table.get(&item.get_body()).unwrap();
        let production = grammar.production_table.get(&body.production).unwrap();

        let state_string = format!(
            "reduce {} {} then set prod to {}",
            body.length, body.bytecode_id, production.bytecode_id
        );

        IRStateString::new("", &state_string)
    }
}
#[cfg(test)]
mod state_constructor_tests {
    use crate::{debug::compile_test_grammar, grammar::get_production_by_name};

    use super::generate_production_states;

    #[test]
    pub fn test_generate_production_states_with_basic_grammar() {
        let grammar = compile_test_grammar("<> A > \\h \\e \\l \\l \\o");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        println!("{:#?}", result);
    }

    #[test]
    pub fn test_generate_production_states_with_basic_grammar_with_one_optional_token() {
        let grammar = compile_test_grammar("<> A > \\h ? \\e ? \\l \\l \\o");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        println!("{:#?}", result);
    }

    #[test]
    pub fn test_generate_production_states_with_basic_grammar_with_left_recursion() {
        let grammar = compile_test_grammar("<> A > A \\1 | \\2 ");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        println!("{:#?}", result);
    }
}
