//! Construct state IR strings for a given production
use super::transition_tree::construct_goto;
use super::transition_tree::construct_recursive_descent;
use crate::grammar::get_production;
use crate::grammar::get_production_plain_name;
use crate::grammar::get_production_start_items;
use crate::primitives::GrammarStore;
use crate::primitives::IRStateString;
use crate::primitives::ProductionId;
use crate::primitives::SymbolID;
use crate::primitives::TransitionGraphNode;
use crate::primitives::TransitionMode;
use crate::primitives::TransitionPack;
use crate::primitives::TransitionStateType;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
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

    // if recursive_descent_data.goto_items.len() > 0 {
    let goto_data = construct_goto(
        production_id,
        grammar,
        &recursive_descent_data
            .goto_items
            .into_iter()
            .collect::<Vec<_>>(),
    );

    if goto_data.leaf_nodes.len() > 0 {
        output.append(&mut process_transition_nodes(&goto_data, grammar));
    } else {
        output.push(create_passing_goto_state(production_id, grammar));
    }
    // }

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

fn create_fail_state(production_id: &ProductionId, grammar: &GrammarStore) {}

fn get_goto_name(production_id: &ProductionId, grammar: &GrammarStore)
    -> String
{
    format!(
        "{}_goto",
        get_production_plain_name(production_id, grammar).to_owned()
    )
}

fn create_passing_goto_state(
    production_id: &ProductionId,
    grammar: &GrammarStore,
) -> IRStateString
{
    IRStateString::new(
        "",
        "pass",
        get_goto_name(production_id, grammar),
        None,
        None,
    )
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
        get_goto_name(root_production, grammar),
        None,
        None,
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
        let mut item_set = BTreeSet::new();

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
                let symbol_id = child.sym;
                let symbol_string = symbol_id.to_string(grammar);
                let state_name = IRStateString::get_state_name_from_hash(hash);

                if !is_scanner {
                    for item in &child.items {
                        item_set.insert(item);
                    }
                }

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

                match &symbol_id {
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
                                "assert PRODUCTION [ 9999 ] ( goto state [ {} ]{} )",
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

                        let (symbol_id, assert_class) = if (!is_scanner) {
                            (symbol_id.bytecode_id(grammar), "TOKEN")
                        } else {
                            get_symbol_consume_type(&symbol_id, grammar)
                        };

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
                            assert_class,
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

        if is_scanner {
            IRStateString::new(
                &comment,
                &strings.join("\n"),
                state_name,
                None,
                None,
            )
        } else {
            let mut normal_symbol_set = BTreeSet::new();
            let mut peek_symbols_set = BTreeSet::new();

            for item in item_set {
                normal_symbol_set.insert(item.get_symbol(grammar));
                println!("{}", &item.debug_string(grammar));
                if let Some(peek_symbols) = grammar.item_peek_symbols.get(item)
                {
                    for peek_symbol in peek_symbols {
                        peek_symbols_set.insert(peek_symbol.clone());
                    }
                }
            }

            let peek_symbols_set = peek_symbols_set
                .difference(&normal_symbol_set)
                .cloned()
                .collect::<BTreeSet<_>>();

            IRStateString::new(
                &comment,
                &strings.join("\n"),
                state_name,
                Some(normal_symbol_set.into_iter().collect()),
                Some(peek_symbols_set.into_iter().collect()),
            )
        }
    }
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
        | SymbolID::GenericSymbol
        | SymbolID::GenericIdentifiers
        | SymbolID::GenericNumbers
        | SymbolID::GenericSymbols => (symbol_id.bytecode_id(grammar), "CLASS"),
        SymbolID::DefinedNumeric(id)
        | SymbolID::DefinedIdentifier(id)
        | SymbolID::DefinedGeneric(id) => {
            let symbol = grammar.symbols_table.get(&symbol_id).unwrap();
            let id = grammar.symbols_string_table.get(&symbol_id).unwrap();
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

        if symbol_id == 0 {
            IRStateString::new(
                "",
                &format!("set prod to {}", production_id),
                String::default(),
                None,
                None,
            )
        } else {
            IRStateString::new(
                "",
                &format!(
                    "assign token [ {} ] then set prod to {}",
                    symbol_id, production_id
                ),
                String::default(),
                None,
                None,
            )
        }
    } else {
        let state_string = format!(
            "set prod to {} then reduce {} {}",
            production.bytecode_id, body.length, body.bytecode_id,
        );

        IRStateString::new("", &state_string, String::default(), None, None)
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
