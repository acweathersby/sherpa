use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    vec,
};

use crate::{
    grammar::{get_closure, get_production_start_items},
    primitives::{
        GrammarId, GrammarStore, Item, ProductionId, SymbolID, TransitionGraphNode,
        TransitionGraphNodeId, TransitionStateType,
    },
};

#[derive(PartialEq, Eq, Debug)]
pub enum Scope {
    RecursiveDescent,
    GOTO,
}
#[derive(Debug)]
pub struct TransitionPack {
    pub scoped_closures: HashMap<Item, Vec<Item>>,
    pub goto_items: HashSet<Item>,
    pub nodes: Vec<TransitionGraphNode>,
    pub leaf_nodes: Vec<TransitionGraphNodeId>,
    pub node_pipeline: VecDeque<TransitionGraphNodeId>,
    pub root_production: ProductionId,
    pub scope: Scope,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
enum TransitionGroupSelector {
    OutOfScope(SymbolID),
    EndItem(u64),
    Symbol(SymbolID),
}

/// Constructs an initial transition tree that parses a production using a recursive
/// descent strategy. Productions that are ambiguous or are left recursive cannot be
/// parsed, so this tries to do its best to define a parse path for a production
/// before we have to resort to LR based parse strategies.
pub fn construct_recursive_descent(
    production_id: &ProductionId,
    grammar: &GrammarStore,
) -> TransitionPack {
    let items = get_production_start_items(production_id, grammar);

    let mut tpack = TransitionPack {
        scoped_closures: HashMap::new(),
        goto_items: HashSet::new(),
        nodes: Vec::new(),
        leaf_nodes: Vec::new(),
        node_pipeline: VecDeque::new(),
        root_production: *production_id,
        scope: Scope::RecursiveDescent,
    };

    for item in &items {
        tpack
            .scoped_closures
            .insert(*item, get_closure(&vec![*item], grammar));
    }

    let mut root_node = create_node(
        &tpack,
        SymbolID::Production(*production_id, GrammarId(0)),
        usize::MAX,
        items,
    );
    let root_index = insert_node(root_node, &mut tpack);
    add_transition_type(root_index, &mut tpack, TransitionStateType::I_DESCENT_START);
    tpack.node_pipeline.push_back(root_index);

    while let Some(node_index) = tpack.node_pipeline.pop_front() {
        process_node(node_index, grammar, &mut tpack, node_index != 0);
    }

    tpack
}

#[cfg(test)]
mod transition_tree_tests {
    use crate::{debug::compile_test_grammar, grammar::get_production_by_name};

    use super::construct_recursive_descent;

    #[test]
    pub fn test_construct_descent_on_basic_grammar() {
        let grammar = compile_test_grammar("<> A > \\h \\e \\l \\l \\o");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = construct_recursive_descent(&prod_id, &grammar);

        assert_eq!(result.nodes.len(), 7);
        assert_eq!(result.leaf_nodes.len(), 1);
    }
}
pub fn construct_goto(
    production_id: &ProductionId,
    grammar: &GrammarStore,
    goto_items: Vec<Item>,
) -> TransitionPack {
    let mut tpack = TransitionPack {
        scoped_closures: HashMap::new(),
        goto_items: HashSet::new(),
        nodes: Vec::new(),
        leaf_nodes: Vec::new(),
        node_pipeline: VecDeque::new(),
        root_production: *production_id,
        scope: Scope::GOTO,
    };

    tpack
}

fn process_node(
    parent_index: usize,
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    allow_increment: bool,
) {
    let node = &mut tpack.nodes[parent_index];

    let mut items: Vec<Item> = node.items.iter().cloned().collect();

    if allow_increment {
        items = items
            .into_iter()
            .map(|i| {
                if i.at_end() {
                    i
                } else {
                    i.increment().unwrap()
                }
            })
            .collect();
    }

    items = items
        .into_iter()
        .filter(|i| {
            i.get_offset() > 0 || i.get_production_id_at_sym(grammar) != tpack.root_production
        })
        .collect();

    let n_term: Vec<Item> = items
        .iter()
        .filter(|i| i.is_term(grammar))
        .cloned()
        .collect();

    let n_nonterm: Vec<Item> = items
        .iter()
        .filter(|i| i.is_nonterm(grammar))
        .cloned()
        .collect();

    let n_end: Vec<Item> = items.iter().filter(|i| i.is_end()).cloned().collect();

    if n_nonterm.len() > 0 {
        let production_ids = HashSet::<ProductionId>::from_iter(
            n_nonterm
                .iter()
                .map(|i| i.get_production_id_at_sym(grammar)),
        );

        if n_term.len() == 0
            && n_end.len() == 0
            && production_ids.len() == 1
            && (node.depth > 1 || non_recursive(&n_nonterm, &tpack.root_production, grammar))
        {
            create_production_call(
                *production_ids.iter().next().unwrap(),
                grammar,
                tpack,
                n_nonterm,
                parent_index,
            );
        } else {
            create_peek(grammar, tpack, parent_index, items);
        }
    } else if n_end.len() == 1 && n_term.len() == 0 {
        // A single end item
        process_end_item(grammar, tpack, n_end[0], parent_index);
    } else if n_end.len() == 0 && n_term.len() == 1 {
        // A single terminal item
        process_terminal_node(grammar, tpack, n_term[0], parent_index);
    } else if n_end.len() == 0 && n_term.len() > 1 {
        // Multiple terminal items
        // TODO: Attempt to disambiguate symbols and create transitions.
        create_peek(grammar, tpack, parent_index, items);
    } else {
        // Multiple terminal and end items
        create_peek(grammar, tpack, parent_index, items);
    }
}
///
/// True if the closure of the givin items does not include
/// the root production on the right of the cursor
fn non_recursive(
    items: &Vec<Item>,
    target_production: &ProductionId,
    grammar: &GrammarStore,
) -> bool {
    for item in get_closure(items, grammar) {
        match item.get_symbol(grammar) {
            SymbolID::Production(production, _) => {
                if production == *target_production {
                    return false;
                }
            }
            _ => {}
        }
    }

    true
}

fn create_peek(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    parent_index: usize,
    items: Vec<Item>,
) {
    let mut depth = 0;

    {
        let parent = &mut tpack.nodes[parent_index];
        depth = parent.depth;
        parent.depth = 99999;
    }

    let goals: Vec<usize> = hash_group(items, |i, item| {
        if item.get_state() == 5000 {
            TransitionGroupSelector::OutOfScope(item.get_symbol(grammar))
        } else if item.at_end() {
            TransitionGroupSelector::EndItem(i as u64)
        } else {
            TransitionGroupSelector::Symbol(item.get_symbol(grammar))
        }
    })
    .iter()
    .enumerate()
    .map(|(i, group)| {
        let state = 10000 * (i as u32 + 1);
        let items: Vec<Item> = group.into_iter().map(|i| i.to_state(state)).collect();
        let mut root_node = create_node(
            tpack,
            items[0].get_symbol(grammar).to_owned(),
            usize::MAX,
            items.clone(),
        );
        root_node.depth = state;
        if tpack.scope == Scope::GOTO {
            if items[0].at_end() {
                root_node.transition_type |= TransitionStateType::I_COMPLETE;
            }
        }

        insert_node(root_node, tpack)
    })
    .collect();

    let mut peek_nodes = vec![];

    for goal_index in goals {
        let items = tpack.nodes[goal_index].items.to_owned();

        let mut term_items = vec![];

        for item in items {
            for closure_item in get_closure(&vec![item], grammar) {
                if !closure_item.is_nonterm(grammar) {
                    term_items.push(closure_item);
                }
            }
        }

        for item in term_items {
            let mut node = create_node(tpack, item.get_symbol(grammar), parent_index, vec![item]);
            node.goal = goal_index;
            node.depth = tpack.nodes[goal_index].depth;
            peek_nodes.push(insert_node(node, tpack));
        }
    }

    let mut leaves: Vec<(usize, usize)> = vec![];

    disambiguate(grammar, tpack, peek_nodes, &mut leaves, 0, parent_index);

    tpack.nodes[parent_index].depth = depth;

    complete_leaves(grammar, tpack, &leaves);
}

fn hash_group<T: Copy + Sized, R: Hash + Sized + Eq, Function: Fn(usize, T) -> R>(
    vector: Vec<T>,
    hash_yielder: Function,
) -> Vec<Vec<T>> {
    let mut hash_groups = HashMap::<R, Vec<T>>::new();

    for (i, val) in vector.iter().enumerate() {
        let hash_value = hash_yielder(i, *val);
        if !hash_groups.contains_key(&hash_value) {
            hash_groups.insert(hash_value, vec![*val]);
        } else {
            hash_groups.get_mut(&hash_value).unwrap().push(*val)
        }
    }

    hash_groups.into_values().collect()
}
/// Pulls roots states into the tip of a leaf state to complete
/// an intermediate parse path. Each leaf tuple pair is a
/// a peek leaf node and its resolved node.
fn complete_leaves(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    leaves: &Vec<(usize, usize)>,
) {
    for (leaf_peek_node_index, resolve_to_node_index) in leaves.into_iter().cloned() {
        let peek_node = get_node(leaf_peek_node_index, tpack);
        let root = peek_node.goal;
        let resolved_node = get_node(resolve_to_node_index, tpack);
        let root_node = get_node(root, tpack);

        let mut copy = (*root_node).to_owned();

        copy.parent = leaf_peek_node_index;

        let copy_index = insert_node(copy, tpack);

        process_node(copy_index, grammar, tpack, true);
    }
}

fn gather_peek_leaf(
    leaf_peek_node_index: usize,
    resolve_to_node_index: usize,
    leaves: &mut Vec<(usize, usize)>,
) {
    leaves.push((leaf_peek_node_index, resolve_to_node_index));
}

fn disambiguate(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    node_ids: Vec<usize>,
    leaves: &mut Vec<(usize, usize)>,
    depth: u32,
    parent_index: usize,
) {
    let mut term_nodes = vec![];
    let mut end_nodes = vec![];

    // We must first complete end-items and generate new
    // nodes that arise from the completion of a production.

    for node_index in node_ids {
        let node = &tpack.nodes[node_index];
        let item = &node.items[0];

        if !item.is_end() {
            term_nodes.push(node_index)
        } else {
            let discard = HashSet::<Item>::new();
            if depth == 0
                && tpack.scope == Scope::GOTO
                && item.decrement().unwrap().is_nonterm(grammar)
            {
                // TODO: Remove items that present in other branches to prevent
                // item aliasing.
            }

            let mut need_to_prune = false;

            for term_node in
                create_term_nodes_from_items(&vec![*item], grammar, tpack, node.id, &discard)
            {
                term_nodes.push(insert_node(term_node, tpack));
                need_to_prune = true;
            }

            if need_to_prune {
                prune_leaf(&node_index, tpack);
            } else {
                end_nodes.push(node_index);
            }
        }
    }

    match end_nodes.len() {
        0 => {}
        1 => {
            prune_leaf(&end_nodes[0], tpack);
            set_transition_type(end_nodes[0], tpack);
        }
        _ => {
            let roots = get_roots(&end_nodes, &tpack);
            if roots.len() == 1 {
                for end_node in end_nodes.iter() {
                    prune_leaf(end_node, tpack);
                }
                set_transition_type(end_nodes[0], tpack);
                gather_peek_leaf(end_nodes[0], roots[0], leaves);
            } else {
                let mut parent = 0;
                for end_node in end_nodes.iter() {
                    parent = prune_leaf(end_node, tpack)
                }
                handle_unresolved_roots(
                    parent,
                    get_node(end_nodes[0], tpack).sym,
                    roots,
                    leaves,
                    grammar,
                    tpack,
                );
            }
        }
    }

    let mut groups = hash_group(term_nodes, |_, n| tpack.nodes[n].sym.clone());
    let mut next_peek_groups = vec![];

    merge_occluding_groups(&mut groups, tpack, grammar);

    for group in groups.iter() {
        let prime_node_index = group[0];
        let roots = get_roots(&group, tpack);
        set_transition_type(prime_node_index, tpack);
        add_transition_type(prime_node_index, tpack, TransitionStateType::I_TEST);

        for node_index in group.iter().cloned() {
            if node_index != prime_node_index {
                let mut items = tpack.nodes[node_index].items.clone();
                prune_leaf(&node_index, tpack);
                tpack.nodes[prime_node_index]
                    .items
                    .append(&mut items.clone());
            }
        }

        if roots.len() > 1 && groups.len() > 1 {
            let mut peek_transition_group = vec![];
            for node_index in group.iter().cloned() {
                let transition_on_skipped_symbol =
                    node_is(node_index, tpack, TransitionStateType::I_SKIPPED_COLLISION);
                for node in create_term_nodes_from_items(
                    &tpack.nodes[node_index]
                        .items
                        .clone()
                        .into_iter()
                        .map(|i| {
                            if transition_on_skipped_symbol {
                                i
                            } else {
                                i.increment().unwrap()
                            }
                        })
                        .collect::<Vec<Item>>(),
                    grammar,
                    tpack,
                    prime_node_index,
                    &HashSet::new(),
                ) {
                    peek_transition_group.push(insert_node(node, tpack));
                }
            }

            if peek_transition_group.is_empty() {
                if get_roots(&group, tpack)
                    .iter()
                    .all(|i| node_is(*i, tpack, TransitionStateType::I_OUT_OF_SCOPE))
                {
                    gather_peek_leaf(prime_node_index, parent_index, leaves)
                } else {
                    panic!("Invalid state, unable to continue disambiguating \n");
                }
            } else {
                next_peek_groups.push(peek_transition_group);
            }
        } else {
            gather_peek_leaf(prime_node_index, roots[0], leaves);
        }
    }

    for peek_group in next_peek_groups {
        if peek_group.len() > 0 {
            if handle_shift_reduce_conflicts(&peek_group, grammar, tpack, leaves) {
                continue;
            } else if groups_are_aliased(&peek_group, tpack)
                || group_is_repeated(&peek_group, tpack)
            {
                handle_transition_collision(&peek_group, grammar, tpack, leaves);
            } else {
                disambiguate(grammar, tpack, peek_group, leaves, depth + 1, parent_index);
            }
        }
    }
}

fn group_is_repeated(peek_group: &Vec<usize>, tpack: &mut TransitionPack) -> bool {
    panic!("Todo: group_is_repeated")
}

fn node_is(node_index: usize, tpack: &TransitionPack, tst: TransitionStateType) -> bool {
    get_node(node_index, tpack).is(tst)
}

fn handle_transition_collision(
    peek_group: &Vec<usize>,
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    leaves: &mut Vec<(usize, usize)>,
) {
    panic!("Todo: handle_shift_reduce_conflicts")
}

fn handle_shift_reduce_conflicts(
    peek_group: &Vec<usize>,
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    leaves: &mut Vec<(usize, usize)>,
) -> bool {
    panic!("Todo: handle_shift_reduce_conflicts")
}

fn groups_are_aliased(peek_group: &Vec<usize>, tpack: &mut TransitionPack) -> bool {
    panic!("Todo: groups_are_aliased")
}

fn add_transition_type(node_index: usize, tpack: &mut TransitionPack, tst: TransitionStateType) {
    let node = get_node(node_index, tpack);
    tpack.nodes[node_index].transition_type |= tst;
}
///
/// Compares the terminal symbols of node groups and merges those
/// groups whose terminal symbols occlude each other.
///
/// For instance, the symbols `g:id` and `t:g` are ambiguous, and so the
/// nodes of a group that has the former symbol are merged into the group
/// with later symbol.
fn merge_occluding_groups(
    groups: &mut Vec<Vec<usize>>,
    tpack: &TransitionPack,
    grammar: &GrammarStore,
) {
    // Clone the from_group store so we are able
    // to merge its members into to_groups without
    // going fowl of the borrow checker.
    for from_group in groups.clone() {
        let from_sym = get_node(from_group[0], tpack).sym;
        for to_group in groups.iter_mut() {
            let to_sym = get_node(to_group[0], tpack).sym;

            if from_sym == to_sym {
                continue;
            }

            if symbols_occlude(&to_sym, &from_sym, grammar) {
                to_group.append(&mut from_group.clone());
            }
        }
    }
}
///
/// Compares whether symbolB occludes symbolA
/// ( produces an ambiguous parse path )
///
/// Symbols that can occlude are as follows
///
/// - `g:id` and any single identifier character.
/// - `g:num` and any single numeric character.
/// - `g:sym` and any single character thats not a numeric, identifier, space, newline, or tab.
///
fn symbols_occlude(symA: &SymbolID, symB: &SymbolID, grammar: &GrammarStore) -> bool {
    match symA {
        SymbolID::DefinedGeneric(_) => match symB {
            SymbolID::GenericSymbol => {
                grammar.symbols_table.get(symA).unwrap().code_point_length == 1
            }
            _ => false,
        },
        SymbolID::DefinedIdentifier(_) => match symB {
            SymbolID::GenericIdentifier => {
                grammar.symbols_table.get(symA).unwrap().code_point_length == 1
            }
            _ => false,
        },
        SymbolID::DefinedNumeric(_) => match symB {
            SymbolID::GenericNumber => {
                grammar.symbols_table.get(symA).unwrap().code_point_length == 1
            }
            _ => false,
        },
        _ => false,
    }
}

fn get_roots(end_nodes: &Vec<usize>, tpack: &TransitionPack) -> Vec<usize> {
    HashSet::<usize>::from_iter(end_nodes.iter().map(|n| tpack.nodes[*n].goal))
        .into_iter()
        .collect::<Vec<usize>>()
}

fn get_node<'a>(node_index: usize, tpack: &'a TransitionPack) -> &'a TransitionGraphNode {
    &tpack.nodes[node_index]
}

fn get_node_mut<'a>(
    node_index: usize,
    tpack: &'a mut TransitionPack,
) -> &'a mut TransitionGraphNode {
    &mut tpack.nodes[node_index]
}

fn handle_unresolved_roots(
    parent_index: usize,
    sym: SymbolID,
    roots: Vec<usize>,
    leaves: &Vec<(usize, usize)>,
    grammar: &GrammarStore,
    tpack: &TransitionPack,
) {
    panic!("TODO: handle_unresolved_roots")
}

///
/// Set the transition type of a peeking based on whether
/// it is first node in the peek path or not. If it is the first
/// node, we do regular ASSERT action on the terminal symbol. Otherwise we use
/// a PEEK action.
fn set_transition_type(node_index: usize, tpack: &mut TransitionPack) {
    let mut node = get_node(node_index, tpack);

    if (node.depth % 10000) > 0 && node.depth >= 10000 {
        add_transition_type(node_index, tpack, TransitionStateType::O_PEEK)
    } else {
        add_transition_type(node_index, tpack, TransitionStateType::O_ASSERT)
    }
}

///
/// Removes the edge from this node to it's parent node
fn prune_leaf(node_index: &usize, tpack: &mut TransitionPack) -> usize {
    let mut node = get_node_mut(*node_index, tpack);
    let parent = node.parent;
    node.parent = usize::MAX;
    parent
}

fn create_term_nodes_from_items(
    items: &Vec<Item>,
    grammar: &GrammarStore,
    tpack: &TransitionPack,
    parent_index: usize,
    discard: &HashSet<Item>,
) -> Vec<TransitionGraphNode> {
    let mut term_nodes = vec![];

    for item in items {
        for closure_item in get_closure(&vec![*item], grammar) {
            if !closure_item.is_nonterm(grammar) && !discard.contains(item) {
                let node = create_node(
                    tpack,
                    item.get_symbol(grammar),
                    parent_index,
                    vec![closure_item],
                );
                term_nodes.push(node);
            }
        }
    }

    term_nodes
}

///
/// Retrieve terminal items derived from a
/// completed item.
fn scan_items(item: &Item, grammar: &GrammarStore) -> Vec<Item> {
    panic!("Todo: scan_items")
}

fn process_end_item(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    end_item: Item,
    parent_index: usize,
) {
    let end_node = create_node(tpack, SymbolID::EndOfFile, parent_index, vec![end_item]);
    let node_index = insert_node(end_node, tpack);
    add_transition_type(node_index, tpack, TransitionStateType::O_ASSERT);
    add_transition_type(node_index, tpack, TransitionStateType::I_END);
    tpack.leaf_nodes.push(node_index);
}

fn process_terminal_node(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    term_item: Item,
    parent_index: usize,
) -> usize {
    let sym = term_item.get_symbol(grammar);
    let new_node = create_node(tpack, sym, parent_index, vec![term_item]);
    let node_index = insert_node(new_node, tpack);
    add_transition_type(node_index, tpack, TransitionStateType::O_ASSERT);
    add_transition_type(node_index, tpack, TransitionStateType::I_CONSUME);
    tpack.node_pipeline.push_back(node_index);
    node_index
}
fn create_production_call(
    production: ProductionId,
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    nonterm_items: Vec<Item>,
    parent_index: usize,
) {
    let mut node = create_node(
        tpack,
        SymbolID::Production(production, GrammarId(0)),
        parent_index,
        nonterm_items,
    );

    node.transition_type |= TransitionStateType::O_PRODUCTION;
    let index = insert_node(node, tpack);
    tpack.node_pipeline.push_back(index);
}

fn create_node(
    tpack: &TransitionPack,
    sym: SymbolID,
    parent_index: usize,
    items: Vec<Item>,
) -> TransitionGraphNode {
    let mut node = TransitionGraphNode::new();

    if tpack.nodes.len() > parent_index {
        let parent = &tpack.nodes[parent_index];
        node.depth = parent.depth + 1;
        node.goal = parent.goal;
    }

    node.sym = sym;
    node.parent = parent_index;
    node.items = items;
    node
}

fn insert_node(mut node: TransitionGraphNode, tpack: &mut TransitionPack) -> usize {
    let id = tpack.nodes.len();
    node.id = id;
    tpack.nodes.push(node);
    id
}
