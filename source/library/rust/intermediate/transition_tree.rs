use bitmask_enum::bitmask;
use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    hash::Hash,
};

use crate::primitives::{item, GrammarId, GrammarStore, Item, ProductionId, SymbolID};

type TransitionGraphNodeId = usize;

#[bitmask]
pub enum TransitionStateType {
    UNDEFINED,
    START,
    ///
    ///Transition has occurred from
    ///the consumption of a terminal
    ///symbol. All transition should
    ///have this set except for the
    ///initial state and the goal state.
    O_TERMINAL,
    ///
    ///Transition has occurred from the
    ///completion of non-terminal symbol.
    O_PRODUCTION,
    ///
    ///Node represents a branch of one or
    ///more sub-nodes. Each sub-node should
    ///should be gated by an assert, peek, or
    ///consume verification instruction.
    MULTI,
    ///
    ///Transition has occurred from the
    ///accepting of a completed root item.
    ACCEPT,

    ///
    ///Transition has occurred from the
    ///accepting of a completed root item.
    AMBIGUOUS,

    ///
    ///State includes items out of the scope of the current production
    ///that should be used for disambiguating states that would
    ///cause a reduction to a production ID other than the current
    ///production.
    I_OUT_OF_SCOPE,
    ///
    ///Consumption of tokens is not allowed
    O_PEEK,

    I_FORK,

    ///
    ///Transition has occurred from the
    ///accepting of a root item.
    I_END,

    ///
    ///The current state represents a completed
    ///production. Used by scanner to determine
    ///when to apply token assignments
    COMPLETED,

    O_GOTO,

    LOOP,

    I_GOTO_START,

    I_DESCENT_START,

    I_CONSUME,

    I_SCANNER,

    I_PASS,

    I_FAIL,

    I_TEST,

    ///
    ///This state is set when the nodes item has a skipped symbol
    ///that occludes another item that consumes that symbol.
    I_SKIPPED_COLLISION,

    I_COMPLETE,
}

pub struct TransitionGraphNode {
    /// The symbols that lead to the
    /// transition to this state.
    pub sym: SymbolID,
    pub transition_type: TransitionStateType,
    pub items: Vec<Item>,
    pub parent: TransitionGraphNodeId,
    pub root: TransitionGraphNodeId,
    pub children: Vec<TransitionGraphNode>,
    pub depth: u32,
    pub id: usize,
}

impl TransitionGraphNode {
    pub fn new() -> Self {
        TransitionGraphNode {
            sym: SymbolID::Undefined,
            transition_type: TransitionStateType::UNDEFINED,
            items: vec![],
            children: vec![],
            parent: 0,
            root: 0,
            depth: 0,
            id: 0,
        }
    }
}

fn get_production_start_items(production: &ProductionId, grammar: &GrammarStore) -> Vec<Item> {
    grammar
        .production_bodies_table
        .get(production)
        .unwrap()
        .iter()
        .map(|id| Item::from_body(id, grammar).unwrap())
        .collect()
}

pub fn get_closure(items: Vec<Item>, grammar: &GrammarStore) -> Vec<Item> {
    let mut seen = HashSet::<Item>::new();
    let mut queue = VecDeque::<Item>::from_iter(items);

    while let Some(item) = queue.pop_front() {
        if seen.insert(item) {
            match &item.get_symbol(grammar) {
                SymbolID::Production(prod_id, _) => {
                    for item in get_production_start_items(prod_id, grammar) {
                        queue.push_back(item)
                    }
                }
                _ => {}
            }
        }
    }

    seen.into_iter().collect()
}

#[derive(PartialEq, Eq)]
enum Scope {
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
    node_index: usize,
    grammar: &GrammarStore,
    tpack: &mut TransitionPack,
    allow_increment: bool,
) {
    let node = &mut tpack.nodes[node_index];

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
            && (node.depth > 1 || non_recursive(n_nonterm, grammar))
        {
            create_production_call(
                *production_ids.iter().next().unwrap(),
                grammar,
                tpack,
                n_nonterm,
                node_index,
            );
        } else {
            create_peek(grammar, tpack, node_index, items);
        }
    } else if n_term.len() == 0 && n_end.len() == 1 {
        process_end_item(opt, node, n_end[0]);
    } else {
        create_peek(grammar, tpack, node_index, items);
    }
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
    let depth = 0;

    {
        let parent = &mut tpack.nodes[parent_index];
        depth = parent.depth;
        parent.depth = 99999;
    }

    let roots: Vec<usize> = hash_group(items, &|i, item| {
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
        let mut root_node = create_node(tpack, items[0].get_symbol(grammar), parent_index, items);
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

    let peek_nodes = vec![];

    for root_index in roots {
        let items = tpack.nodes[root_index].items.to_owned();

        let term_items = vec![];

        for item in items {
            for closure_item in get_closure(vec![item], grammar) {
                if !closure_item.is_nonterm(grammar) {
                    term_items.push(closure_item);
                }
            }
        }

        for item in term_items {
            let node = create_node(tpack, item.get_symbol(grammar), parent_index, vec![item]);
            node.depth = tpack.nodes[root_index].depth;
            peek_nodes.push(insert_node(node, tpack));
        }
    }

    let mut leaves: Vec<(usize, usize)> = vec![];

    disambiguate(grammar, tpack, peek_nodes, &mut leaves);

    tpack.nodes[parent_index].depth = depth;

    complete_leaves(grammar, tpack, &leaves);
}

type HashYielder<T, R: Hash + Sized> = dyn Fn(usize, T) -> R;

fn hash_group<T: Copy, R: Hash + Sized + Eq>(
    vector: Vec<T>,
    hash_yielder: &HashYielder<T, R>,
) -> Vec<Vec<T>> {
    let hash_groups = HashMap::<R, Vec<T>>::new();

    for (i, val) in vector.iter().enumerate() {
        let hash_value = hash_yielder(i, *val);
        if !hash_groups.contains_key(&hash_value) {
            hash_groups.insert(hash_value, vec![*val]);
        } else {
            hash_groups.get(&hash_value).unwrap().push(*val)
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
    nodes: Vec<usize>,
    leaves: &mut Vec<(usize, usize)>,
) {
}

fn process_end_item(
    production: ProductionId,
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

    tpack.node_pipeline.push_back(insert_node(node, tpack));
}

fn create_node(
    tpack: &mut TransitionPack,
    sym: SymbolID,
    parent_index: usize,
    items: Vec<Item>,
) -> TransitionGraphNode {
    let mut node = TransitionGraphNode::new();
    node.id = tpack.nodes.len();
    node.sym = sym;
    node.parent = parent_index;
    node.items = items;
    node.depth = tpack.nodes[parent_index].depth + 1;
    node
}

fn insert_node(node: TransitionGraphNode, tpack: &mut TransitionPack) -> usize {
    let id = tpack.nodes.len();
    node.id = id;
    tpack.nodes.push(node);
    id
}
