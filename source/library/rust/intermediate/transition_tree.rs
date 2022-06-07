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

#[derive(PartialEq, Eq)]
pub enum Scope {
    RecursiveDescent,
    GOTO,
}

pub struct TransitionPack {
    pub scoped_closures: HashMap<Item, Vec<Item>>,
    pub goto_items: HashSet<Item>,
    pub nodes: Vec<TransitionGraphNode>,
    pub node_pipeline: VecDeque<TransitionGraphNodeId>,
    pub root_production: ProductionId,
    pub scope: Scope,
}

/// Constructs an initial transition tree
/// that parses a production using a recursive
/// descent strategy. Productions that are ambiguous
/// or are left recursive cannot be parsed, so this
/// tries to get as much of production defined before
/// resorting to goto based LR strategy.
pub fn construct_descent(production: &ProductionId, grammar: &GrammarStore) -> TransitionPack {
    let items = get_production_start_items(production, grammar);

    let mut tpack = TransitionPack {
        scoped_closures: HashMap::new(),
        goto_items: HashSet::new(),
        nodes: Vec::new(),
        node_pipeline: VecDeque::new(),
        root_production: *production,
        scope: Scope::RecursiveDescent,
    };

    for item in &items {
        tpack
            .scoped_closures
            .insert(*item, get_closure(vec![*item], grammar));
    }

    let mut root_node = TransitionGraphNode::new();
    root_node.transition_type |= TransitionStateType::I_DESCENT_START;
    tpack.nodes.push(root_node);

    while let Some(node_index) = tpack.node_pipeline.pop_front() {
        processNode(node_index, grammar, &mut tpack, node_index != 0);
    }

    tpack
}

fn processNode(
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
            && (node.depth > 1 || non_recursive(&n_nonterm, grammar))
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
    } else if n_term.len() == 0 && n_end.len() == 1 {
        process_end_item(grammar, tpack, n_end[0], parent_index);
    } else {
        create_peek(grammar, tpack, parent_index, items);
    }
}

fn non_recursive(item: &Vec<Item>, grammar: &GrammarStore) -> bool {
    panic!("Todo: non_recursive")
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
enum TransitionGroupSelector {
    OutOfScope(SymbolID),
    EndItem(u64),
    Symbol(SymbolID),
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

    let roots: Vec<usize> = hash_group(items, |i, item| {
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
            parent_index,
            items.clone(),
        );
        root_node.depth = state;
        root_node.root = root_node.id;
        if tpack.scope == Scope::GOTO {
            if items[0].at_end() {
                root_node.transition_type |= TransitionStateType::I_COMPLETE;
            }
        }

        insert_node(root_node, tpack)
    })
    .collect();

    let mut peek_nodes = vec![];

    for root_index in roots {
        let items = tpack.nodes[root_index].items.to_owned();

        let mut term_items = vec![];

        for item in items {
            for closure_item in get_closure(vec![item], grammar) {
                if !closure_item.is_nonterm(grammar) {
                    term_items.push(closure_item);
                }
            }
        }

        for item in term_items {
            let mut node = create_node(tpack, item.get_symbol(grammar), parent_index, vec![item]);
            node.depth = tpack.nodes[root_index].depth;
            peek_nodes.push(insert_node(node, tpack));
        }
    }

    let mut leaves: Vec<(usize, usize)> = vec![];

    disambiguate(grammar, tpack, peek_nodes, &mut leaves, 0);

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

fn complete_leaves(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    leaves: &Vec<(usize, usize)>,
) {
}

fn disambiguate(
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    node_ids: Vec<usize>,
    leaves: &mut Vec<(usize, usize)>,
    depth: u32,
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
            set_transition_type(end_nodes[0], grammar, tpack);
        }
        _ => {
            let roots = get_roots(&end_nodes, &tpack);
            if roots.len() == 1 {
                for end_node in end_nodes.iter() {
                    prune_leaf(end_node, tpack);
                }
                set_transition_type(end_nodes[0], grammar, tpack);
                gather_leaves(end_nodes[0], roots[0], leaves, grammar, tpack);
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

    let mut groups = hash_group(term_nodes, |i, n| tpack.nodes[n].sym.clone());
    let mut next_peek_groups = vec![];

    merge_occluding_groups(&mut groups, tpack, grammar);

    for group in groups.iter() {
        let prime_node_index = group[0];
        let mut peek_transition_group = vec![];
        let roots = get_roots(&group, tpack);
        set_transition_type(prime_node_index, grammar, tpack);
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
            for node_index in group.iter().cloned() {
                if false
                /*Skipped Collision*/
                {
                } else {
                    for node in create_term_nodes_from_items(
                        &tpack.nodes[node_index]
                            .items
                            .clone()
                            .into_iter()
                            .map(|i| i.increment().unwrap())
                            .collect::<Vec<Item>>(),
                        grammar,
                        tpack,
                        prime_node_index,
                        &HashSet::new(),
                    ) {
                        peek_transition_group.push(insert_node(node, tpack));
                    }
                }
            }

            if peek_transition_group.is_empty() {
            } else {
                next_peek_groups.push(peek_transition_group);
            }
        } else {
            gather_leaves(prime_node_index, roots[0], leaves, grammar, tpack);
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
                disambiguate(grammar, tpack, peek_group, leaves, depth + 1);
            }
        }
    }
}

fn group_is_repeated(peek_group: &Vec<usize>, tpack: &mut TransitionPack) -> bool {
    panic!("Todo: group_is_repeated")
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
    tpack.nodes[node_index].transition_type |= tst;
}

fn merge_occluding_groups(
    groups: &mut Vec<Vec<usize>>,
    tpack: &TransitionPack,
    grammar: &GrammarStore,
) {
    panic!("Todo: merge_occluding_groups")
}

fn get_roots(end_nodes: &Vec<usize>, tpack: &TransitionPack) -> Vec<usize> {
    HashSet::<usize>::from_iter(end_nodes.iter().map(|n| tpack.nodes[*n].root))
        .into_iter()
        .collect::<Vec<usize>>()
}

fn get_node<'a>(node_index: usize, tpack: &'a TransitionPack) -> &'a TransitionGraphNode {
    &tpack.nodes[node_index]
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

fn gather_leaves(
    node_index: usize,
    root_index: usize,
    leaves: &Vec<(usize, usize)>,
    grammar: &GrammarStore,
    tpack: &TransitionPack,
) {
    panic!("TODO: gather_leaves")
}

fn set_transition_type(node_index: usize, grammar: &GrammarStore, tpack: &TransitionPack) {
    panic!("TODO: set_transition_type")
}

fn prune_leaf(node_index: &usize, tpack: &mut TransitionPack) -> usize {
    panic!("TODO: prune_leaf")
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
        for closure_item in get_closure(vec![*item], grammar) {
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
    let mut end_node = TransitionGraphNode::new();
    end_node.sym = SymbolID::EndOfFile;
    end_node.parent = parent_index;
    end_node.id = tpack.nodes.len();
    end_node.items = vec![end_item];
    end_node.depth = tpack.nodes[parent_index].depth + 1;
    end_node.transition_type |= TransitionStateType::I_END | TransitionStateType::O_TERMINAL;
    tpack.nodes.push(end_node);
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
    let parent = &tpack.nodes[parent_index];
    node.sym = sym;
    node.depth = parent.depth + 1;
    node.root = parent.root;
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
