//! Construct state IR strings for a given production
//!

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use crate::{
    grammar::get_production_plain_name,
    primitives::{GrammarStore, ProductionId, SymbolID, TransitionGraphNode},
};

use super::transition_tree::{construct_goto, construct_recursive_descent, Scope, TransitionPack};

#[derive(Debug)]
pub struct IRStateString {
    pub comment: String,
    pub state: String,
    pub hash: u64,
}

type IROutput = BTreeMap<u64, IRStateString>;

pub fn generate_production_states(
    production_id: &ProductionId,
    grammar: &GrammarStore,
) -> IROutput {
    let mut output: IROutput = BTreeMap::new();

    let recursive_descent_data = construct_recursive_descent(production_id, grammar);

    process_transition_nodes(&recursive_descent_data, grammar, &mut output);

    if recursive_descent_data.goto_items.len() > 0 {
        let goto_data = construct_goto(
            production_id,
            grammar,
            recursive_descent_data
                .goto_items
                .into_iter()
                .collect::<Vec<_>>(),
        );

        process_transition_nodes(&goto_data, grammar, &mut output);

        create_fail_state(production_id, grammar, &mut output);
    }

    output
}

fn process_transition_nodes<'a>(
    tpack: &'a TransitionPack,
    grammar: &GrammarStore,
    output: &mut IROutput,
) {
    //We start at leaf nodes and make our way down to the root.
    let number_of_nodes = tpack.nodes.len();
    // Construct utility information
    let mut node_hashes = Vec::<u64>::with_capacity(number_of_nodes);
    for _ in 0..number_of_nodes {
        node_hashes.push(0);
    }

    let mut children_tables = tpack
        .nodes
        .iter()
        .map(|n| Vec::<&'a TransitionGraphNode>::new())
        .collect::<Vec<_>>();

    for child in &tpack.nodes {
        if child.parent != usize::MAX {
            children_tables[child.parent].push(child);
        }
    }

    let mut nodes_pipeline = VecDeque::from_iter(tpack.leaf_nodes.iter().cloned());

    while let Some(node_id) = nodes_pipeline.pop_front() {
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
                continue;
            }
        }

        let node = &tpack.nodes[node_id];
        let mut hash = 1u64;

        if node.sym == SymbolID::EndOfFile {
            hash = create_end_state(node, grammar, output)
        } else {
            hash = create_intermediate_state(
                node,
                &node_hashes,
                children_lookup,
                grammar,
                output,
                &tpack.scope,
            );
        }

        node_hashes[node_id] = hash;
        if node_id != 0 {
            nodes_pipeline.push_back(node.parent);
        }
    }
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
    output: &mut IROutput,
    scope: &Scope,
) -> u64 {
    let mut strings = vec![];
    let mut comment = String::new();

    for child in children {
        let hash = hashes[child.id];
        let symbol = child.sym;
        let symbol_id = symbol.bytecode_id(grammar);
        let symbol_string = symbol.to_string(grammar);
        comment += &child
            .items
            .iter()
            .map(|i| i.debug_string(grammar))
            .collect::<Vec<_>>()
            .join("\n");
        match &symbol {
            SymbolID::Production(production_id, _) => {
                strings.push(format!(
                    "goto state [ {} ] then goto state [ {} ]",
                    get_production_plain_name(production_id, grammar),
                    child.id
                ));
            }
            SymbolID::EndOfFile => {
                strings.push(format!("goto state [ {} ]", child.id));
            }
            _ => {
                strings.push(format!(
                    "assert TOKEN [ /* {} */ {} ] {{ consume then goto state [ {} ] }}",
                    symbol_string, symbol_id, child.id
                ));
            }
        }
    }

    output.insert(
        node.id as u64,
        IRStateString {
            comment: comment,
            state: strings.join("\n"),
            hash: 1,
        },
    );
    1
}

fn create_end_state(
    node: &TransitionGraphNode,
    grammar: &GrammarStore,
    output: &mut IROutput,
) -> u64 {
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

        output.insert(
            node.id as u64,
            IRStateString {
                comment: item.debug_string(grammar),
                state: state_string,
                hash: 1,
            },
        );

        1
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
}
