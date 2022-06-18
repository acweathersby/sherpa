//! Construct state IR strings for a given production
use super::transition_tree::construct_goto;
use super::transition_tree::construct_recursive_descent;
use crate::grammar::get_production_plain_name;
use crate::primitives::GrammarStore;
use crate::primitives::IRStateString;
use crate::primitives::ProductionId;
use crate::primitives::SymbolID;
use crate::primitives::TransitionGraphNode;
use crate::primitives::TransitionMode;
use crate::primitives::TransitionPack;
use crate::primitives::TransitionStateType;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::fmt::format;

type IROutput = Vec<IRStateString>;

pub fn generate_production_states(
    production_id: &ProductionId,
    grammar: &GrammarStore,
) -> IROutput
{
    let mut output: IROutput = Vec::new();

    let recursive_descent_data =
        construct_recursive_descent(production_id, grammar);

    output.append(&mut process_transition_nodes(
        &recursive_descent_data,
        grammar,
    ));

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
) -> Vec<IRStateString>
{
    // We start at leaf nodes and make our way down to the root.
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

    let mut nodes_pipeline =
        VecDeque::from_iter(tpack.leaf_nodes.iter().cloned());

    'outer: while let Some(node_id) = nodes_pipeline.pop_front() {
        if node_hashes[node_id] != 0 {
            // Already dealt with this node. No need to do any more work.
            continue;
        }

        let children_lookup = children_tables.get(node_id).unwrap();

        // Ensure dependencies are met.
        for child in children_lookup {
            if node_hashes[child.id] == 0 {
                // Push dependency to be processed, which will cause this node
                // be pushed back into the queue after it is processed
                nodes_pipeline.push_back(child.id);

                continue 'outer;
            }
        }

        let node = tpack.get_node(node_id);

        let state = if node.sym == SymbolID::EndOfFile {
            create_end_state(node, grammar, tpack.is_scanner)
        } else {
            create_intermediate_state(
                node,
                grammar,
                tpack.is_scanner,
                &node_hashes,
                children_lookup,
                &tpack.mode,
                &tpack.root_production,
            )
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

fn create_fail_state(
    production_id: &ProductionId,
    grammar: &GrammarStore,
    output: &mut IROutput,
)
{
}

fn create_passing_goto_state(
    production_id: &ProductionId,
    grammar: &GrammarStore,
    output: &mut IROutput,
)
{
}

fn create_goto_start_state(
    grammar: &GrammarStore,
    hashes: &[u64],
    children: &[&TransitionGraphNode],
    root_production: &ProductionId,
) -> IRStateString
{
    let mut strings = vec![];

    let mut comment = String::new();

    let post_amble = create_post_amble(root_production, grammar);

    for child in children {
        let hash = hashes[child.id];

        let symbol = child.sym;

        match &symbol {
            SymbolID::Production(production_id, _) => {
                let production_bytecode_id = grammar
                    .production_table
                    .get(production_id)
                    .unwrap()
                    .bytecode_id;

                if child.is(TransitionStateType::I_PASS) {
                    strings.push(format!(
                        "assert PRODUCTION [ {} ] ( pass )",
                        production_bytecode_id,
                        // get_production_plain_name(production_id, grammar),
                    ));
                } else {
                    let state_name =
                        IRStateString::get_state_name_from_hash(hash);

                    strings.push(format!(
                        "assert PRODUCTION [ {}  ] ( goto state [ {} ]{})",
                        production_bytecode_id,
                        get_production_plain_name(production_id, grammar),
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

    IRStateString::new(
        &comment,
        &strings.join("\n"),
        get_production_plain_name(root_production, grammar).to_owned()
            + "_goto",
    )
}

fn create_intermediate_state(
    node: &TransitionGraphNode,
    grammar: &GrammarStore,
    is_scanner: bool,
    hashes: &[u64],
    children: &[&TransitionGraphNode],
    mode: &TransitionMode,
    root_production: &ProductionId,
) -> IRStateString
{
    if node.id == 0 && *mode == TransitionMode::GoTo {
        create_goto_start_state(grammar, hashes, children, root_production)
    } else {
        let mut strings = vec![];

        let mut comment = String::new();

        let post_amble = if node.id == 0 {
            create_post_amble(root_production, grammar)
        } else {
            String::default()
        };

        let state_name = if node.id == 0 {
            get_production_plain_name(root_production, grammar).to_owned()
        } else {
            String::default()
        };

        if node.is(TransitionStateType::I_FORK) {
            let mut string: String = "fork to ( ".to_string();

            string += &children
                .iter()
                .map(|c| IRStateString::get_state_name_from_hash(hashes[c.id]))
                .collect::<Vec<_>>()
                .join(", ");

            string += " )";

            strings.push(string);
        } else {
            let single_child = children.len() == 1;

            for child in children {
                let hash = hashes[child.id];

                let symbol = child.sym;

                let symbol_id = symbol.bytecode_id(grammar);

                let symbol_string = symbol.to_string(grammar);

                let state_name = IRStateString::get_state_name_from_hash(hash);

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
                            "goto state [ {} ] then goto state [ {} ]{}",
                            get_production_plain_name(production_id, grammar),
                            state_name,
                            post_amble
                        ));
                    }
                    SymbolID::Recovery => {
                        strings.push(format!(
                            "goto state [ {} ]{}",
                            state_name, post_amble
                        ));
                    }
                    SymbolID::EndOfFile => {
                        if single_child {
                            strings.push(format!(
                                "goto state [ {} ]{}",
                                state_name, post_amble
                            ));
                        } else {
                            strings.push(format!(
                                "assert [ 9999 ] ( goto state [ {} ]{} )",
                                state_name, post_amble
                            ));
                        }
                    }
                    _ => {
                        let assertion_type =
                            (if child.is(TransitionStateType::O_PEEK) {
                                "peek"
                            } else {
                                "assert"
                            })
                            .to_string();

                        let symbol_type =
                            (if is_scanner { "" } else { "TOKEN" }).to_string();

                        let consume =
                            (if child.is(TransitionStateType::I_CONSUME) {
                                "consume then "
                            } else {
                                ""
                            })
                            .to_string();

                        strings.push(format!(
                            "{} {} [ {} ] ( {}goto state [ {} ]{} )",
                            assertion_type,
                            symbol_type,
                            // symbol_string,
                            symbol_id,
                            consume,
                            state_name,
                            post_amble
                        ));
                    }
                }
            }
        }

        IRStateString::new(&comment, &strings.join("\n"), state_name)
    }
}

fn create_post_amble(
    production_id: &ProductionId,
    grammar: &GrammarStore,
) -> String
{
    format!(
        " then goto state [ {}_goto ]",
        get_production_plain_name(production_id, grammar)
    )
}

fn create_end_state(
    node: &TransitionGraphNode,
    grammar: &GrammarStore,
    is_scanner: bool,
) -> IRStateString
{
    let item = node.items[0];

    let body = grammar.bodies_table.get(&item.get_body()).unwrap();

    let production = grammar.production_table.get(&body.production).unwrap();

    if !item.at_end() {
        panic!("Expected state to be in end state")
    } else if is_scanner {
        let symbol_id = production.symbol_bytecode_id;
        let production_id = production.bytecode_id;

        if symbol_id > 0 {
            IRStateString::new(
                "",
                &format!("set prod to {}", production_id),
                String::default(),
            )
        } else {
            IRStateString::new(
                "",
                &format!(
                    "assign token {} then set prod to {}",
                    symbol_id, production_id
                ),
                String::default(),
            )
        }
    } else {
        let state_string = format!(
            "set prod to {} then reduce {} {}",
            production.bytecode_id, body.length, body.bytecode_id,
        );

        IRStateString::new("", &state_string, String::default())
    }
}

#[cfg(test)]

mod state_constructor_tests
{

    use crate::debug::compile_test_grammar;
    use crate::grammar::get_production_by_name;

    use super::generate_production_states;

    #[test]

    pub fn test_generate_production_states_with_basic_grammar()
    {
        let grammar = compile_test_grammar("<> A > \\h \\e \\l \\l \\o");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        assert_eq!(result.len(), 7);

        println!("{:#?}", result);
    }

    #[test]

    pub fn test_generate_production_states_with_basic_grammar_with_one_optional_token(
    )
    {
        let grammar = compile_test_grammar("<> A > \\h ? \\e ? \\l \\l \\o");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        println!("{:#?}", result);

        assert_eq!(result.len(), 24);
    }

    #[test]

    pub fn test_generate_production_states_with_basic_grammar_with_left_recursion(
    )
    {
        let grammar = compile_test_grammar("<> A > A \\1 | \\2 ");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        println!("{:#?}", result);

        assert_eq!(result.len(), 7);
    }
}
