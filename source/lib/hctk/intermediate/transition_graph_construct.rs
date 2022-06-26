use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::process::id;
use std::rc::Rc;
use std::vec;

use crate::grammar::get_closure;
use crate::grammar::get_closure_cached;
use crate::grammar::get_production_start_items;
use crate::grammar::hash_id_value_u64;
use crate::primitives::GrammarId;
use crate::primitives::GrammarStore;
use crate::primitives::Item;
use crate::primitives::ItemState;
use crate::primitives::ProductionId;
use crate::primitives::SymbolID;
use crate::primitives::TransitionGraphNode as TGN;
use crate::primitives::TransitionMode;
use crate::primitives::TransitionPack as TPack;
use crate::primitives::TransitionStateType as TST;

#[derive(Hash, PartialEq, Eq, Clone, Copy)]

enum TransitionGroupSelector
{
    OutOfScope(SymbolID),
    EndItem(u64),
    Symbol(SymbolID),
}

/// Constructs an initial transition graph that parses a production
/// using a recursive descent strategy. Productions that are ambiguous
/// or are left recursive cannot be parsed, so this tries to do its
/// best to define a parse path for a production before we have to
/// resort to LR and Forking based parse strategies.

pub fn construct_recursive_descent<'a>(
    grammar: &'a GrammarStore,
    is_scanner: bool,
    start_items: &Vec<Item>,
) -> TPack<'a>
{
    let start_items = apply_state_info(start_items);

    let mut t_pack = TPack::new(
        grammar,
        TransitionMode::RecursiveDescent,
        is_scanner,
        &start_items,
    );

    let non_lr_start_items = start_items
        .iter()
        .filter(|i| {
            !t_pack
                .root_productions
                .contains(&i.get_production_id_at_sym(grammar))
        })
        .cloned()
        .collect::<Vec<_>>();

    for item in &non_lr_start_items {
        t_pack
            .scoped_closures
            .push(get_closure_cached(item, grammar));

        if item.is_nonterm(grammar) {
            t_pack.set_closure_link(*item, item.to_null());
        }
    }

    let root_index = t_pack.insert_node(TGN::new(
        &t_pack,
        SymbolID::Undefined,
        usize::MAX,
        non_lr_start_items,
    ));

    t_pack
        .get_node_mut(root_index)
        .set_type(TST::I_DESCENT_START);

    t_pack.queue_node(root_index);

    while let Some(node_index) = t_pack.get_next_queued() {
        process_node(
            node_index,
            node_index,
            grammar,
            &mut t_pack,
            node_index != 0,
        );
    }

    t_pack.clean()
}

fn apply_state_info(start_items: &Vec<Item>) -> Vec<Item>
{
    let start_items = start_items
        .iter()
        .enumerate()
        .map(|(i, item)| item.to_state(ItemState::new(i as u32, 0)))
        .collect::<Vec<_>>();
    start_items
}

pub fn construct_goto<'a>(
    grammar: &'a GrammarStore,
    is_scanner: bool,
    start_items: &Vec<Item>,
    goto_items: &Vec<Item>,
) -> TPack<'a>
{
    let start_items = apply_state_info(start_items);

    let mut t_pack =
        TPack::new(grammar, TransitionMode::GoTo, is_scanner, &start_items);

    let global_closure = Rc::new(Box::<Vec<Item>>::new(if t_pack.is_scanner {
        vec![]
    } else {
        get_follow_closure(goto_items, grammar, &t_pack.root_productions)
    }));

    let mut parent = TGN::new(
        &t_pack,
        SymbolID::Undefined,
        TGN::OrphanIndex,
        goto_items.clone(),
    );

    parent.set_type(TST::I_GOTO_START);

    let parent_index = t_pack.insert_node(parent);

    for (production_id, group) in
        get_goto_starts(&start_items, grammar, goto_items)
    {
        t_pack.goto_scoped_closure = Some(global_closure.clone());

        let items: Vec<Item> = group
            .iter()
            .map(|i| {
                let stated_item = i.increment().unwrap();

                let stated_item = if stated_item.at_end() {
                    stated_item.to_state(ItemState::OUT_OF_SCOPE_STATE)
                } else {
                    t_pack
                        .scoped_closures
                        .push(get_closure_cached(&stated_item, grammar));

                    stated_item.to_state(ItemState::new(
                        (t_pack.scoped_closures.len() - 1) as u32,
                        0,
                    ))
                };

                stated_item
            })
            .collect();

        let mut goto_node = TGN::new(
            &t_pack,
            SymbolID::Production(production_id, GrammarId::default()),
            parent_index,
            items.clone(),
        );

        goto_node.set_type(TST::O_GOTO);

        if (group.len() > 0
            && (group.iter().any(|i| i.increment().unwrap().at_end())))
            || t_pack.root_productions.contains(&production_id)
        {
            if t_pack.root_productions.contains(&production_id) {
                let mut reducible: Vec<Item> = grammar
                    .lr_items
                    .get(&production_id)
                    .unwrap_or(&HashSet::new())
                    .iter()
                    .filter(|i| !group.iter().any(|g| *g == **i))
                    .map(|i| {
                        i.increment()
                            .unwrap()
                            .to_state(ItemState::OUT_OF_SCOPE_STATE)
                    })
                    .collect();

                goto_node.items.append(&mut reducible);
            }

            create_peek(
                t_pack.insert_node(goto_node),
                items,
                grammar,
                &mut t_pack,
            );
        } else {
            let node_index = t_pack.insert_node(goto_node);
            process_node(node_index, node_index, grammar, &mut t_pack, false);
        }
    }

    while let Some(node_index) = t_pack.get_next_queued() {
        process_node(
            node_index,
            node_index,
            grammar,
            &mut t_pack,
            node_index != 0,
        );
    }

    t_pack.clean()
}

pub fn get_goto_starts(
    // root_production: &ProductionId,
    start_items: &Vec<Item>,
    grammar: &GrammarStore,
    seed_items: &Vec<Item>,
) -> Vec<(ProductionId, HashSet<Item>)>
{
    type GotoMap = BTreeMap<ProductionId, HashSet<Item>>;

    let mut local_lr_items = GotoMap::new();

    let mut output_items = GotoMap::new();

    let mut batch = VecDeque::from_iter(
        seed_items.iter().map(|i| i.get_production_id(grammar)),
    );

    fn insert(
        goto_items: &mut GotoMap,
        production_id: &ProductionId,
        item: Item,
    )
    {
        if !goto_items.contains_key(production_id) {
            goto_items.insert(*production_id, HashSet::<Item>::new());
        }

        goto_items.get_mut(production_id).unwrap().insert(item);
    }

    for start_item in start_items {
        for item in get_closure_cached(start_item, grammar) {
            if !item.at_end() && item.is_nonterm(grammar) {
                insert(
                    &mut local_lr_items,
                    &item.get_production_id_at_sym(grammar),
                    *item,
                )
            }
        }
    }

    while let Some(production_id) = batch.pop_front() {
        if !output_items.contains_key(&production_id)
            && local_lr_items.contains_key(&production_id)
        {
            let items = local_lr_items.get(&production_id).unwrap();

            for item in items {
                batch.push_back(item.get_production_id(grammar));
            }

            output_items.insert(production_id, items.clone());
        }
    }

    output_items.into_iter().collect()
}

pub fn get_follow_closure(
    goto_items: &Vec<Item>,
    grammar: &GrammarStore,
    root_production_ids: &BTreeSet<ProductionId>,
) -> Vec<Item>
{
    let mut productions_to_process = VecDeque::<ProductionId>::new();

    let mut seen_productions = BTreeSet::<ProductionId>::new();

    for production_id in root_production_ids {
        productions_to_process.push_back(production_id.clone());
    }

    let mut output = HashSet::new();

    while let Some(production_id) = productions_to_process.pop_front() {
        if !seen_productions.insert(production_id) {
            continue;
        }

        let items: Vec<Item> = grammar
            .lr_items
            .get(&production_id)
            .unwrap_or(&HashSet::new())
            .iter()
            .map(|i| i.increment().unwrap())
            .collect();

        for item in items {
            if item.is_end() {
                productions_to_process
                    .push_back(item.get_production_id(grammar));
            }

            output.insert(item.decrement().unwrap());
        }

        seen_productions.insert(production_id);
    }

    output.into_iter().collect()
}

fn process_node(
    node_index: usize,
    parent_index: usize,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    allow_increment: bool,
) -> Vec<usize>
{
    let node = t_pack.get_node(node_index).clone();
    let mut items: Vec<Item> = node.items.iter().cloned().collect();

    if allow_increment {
        items = items
            .into_iter()
            .map(|i| {
                if i.at_end() {
                    i
                } else {
                    let item_next = i.increment().unwrap();
                    let item_link = t_pack.get_closure_link(&i);
                    t_pack.set_closure_link(item_next.clone(), item_link);
                    item_next
                }
            })
            .collect();
    }

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

    let n_end: Vec<Item> =
        items.iter().filter(|i| i.is_end()).cloned().collect();

    if n_nonterm.len() > 0 {
        let production_ids = HashSet::<ProductionId>::from_iter(
            n_nonterm
                .iter()
                .map(|i| i.get_production_id_at_sym(grammar)),
        );

        if n_term.len() == 0
            && n_end.len() == 0
            && production_ids.len() == 1
            && (node.depth > 1
                || non_recursive(&n_nonterm, &t_pack.root_productions, grammar))
        {
            create_production_call(
                *production_ids.iter().next().unwrap(),
                grammar,
                t_pack,
                n_nonterm,
                parent_index,
            )
        } else {
            create_peek(parent_index, items, grammar, t_pack)
        }
    } else if n_end.len() == 1 && n_term.len() == 0 {
        // A single end item
        process_end_item(grammar, t_pack, n_end[0], parent_index)
    } else if n_end.len() == 0 && n_term.len() == 1 {
        // A single terminal item
        process_terminal_node(grammar, t_pack, n_term[0], parent_index)
    } else if n_end.len() == 0 && n_term.len() > 1 {
        // Multiple terminal items
        create_peek(parent_index, items, grammar, t_pack)
    } else {
        // Multiple terminal and end items
        create_peek(parent_index, items, grammar, t_pack)
    }
}

/// True if the closure of the givin items does not include
/// the goal production on the right of the cursor

fn non_recursive(
    items: &Vec<Item>,
    target_production: &BTreeSet<ProductionId>,
    grammar: &GrammarStore,
) -> bool
{
    for item in get_closure(items, grammar) {
        match item.get_symbol(grammar) {
            SymbolID::Production(production, _) => {
                if target_production.contains(&production) {
                    return false;
                }
            }
            _ => {}
        }
    }

    true
}

fn create_peek(
    parent_index: usize,
    items: Vec<Item>,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
) -> Vec<usize>
{
    let mut depth = 0;

    {
        let parent = t_pack.get_node_mut(parent_index);
        parent.set_type(TST::I_PEEK_ROOT);
        depth = parent.depth;
        parent.depth = 99999;
    }

    let goals: Vec<usize> = items
        .iter()
        .enumerate()
        .map(|(i, item)| {
            let mut root_node = TGN::new(
                t_pack,
                item.get_symbol(grammar).to_owned(),
                usize::MAX,
                vec![item.clone()],
            );

            root_node.depth = 0;

            if t_pack.mode == TransitionMode::GoTo {
                if item.at_end() {
                    root_node.transition_type |= TST::I_COMPLETE;
                }
            }

            t_pack.insert_node(root_node)
        })
        .collect();

    let mut peek_nodes = vec![];

    for (i, goal_index) in goals.iter().cloned().enumerate() {
        let items = t_pack
            .get_node(goal_index)
            .items
            .to_owned()
            .iter()
            .map(|i| (t_pack.get_closure_link(i), *i))
            .collect::<Vec<_>>();

        for mut node in create_term_nodes_from_items(
            &items,
            grammar,
            t_pack,
            parent_index,
            &HashSet::new(),
        ) {
            node.goal = goal_index;

            node.depth = t_pack.get_node(goal_index).depth;

            peek_nodes.push(t_pack.insert_node(node));
        }
    }

    let mut leaves = vec![];

    disambiguate(grammar, t_pack, peek_nodes, &mut leaves, 0);

    t_pack.get_node_mut(parent_index).depth = depth;

    let resolved_leaves = process_peek_leaves(grammar, t_pack, leaves);

    // All goal nodes can be recycled, as copy operations where used to
    // insert goal nodes as children of peek leaves
    for goal in goals {
        t_pack.drop_node(&goal);
    }

    t_pack.clear_peek_data();

    resolved_leaves
}

fn create_term_nodes_from_items(
    items: &Vec<(Item, Item)>,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    parent_index: usize,
    discard: &HashSet<Item>,
) -> Vec<TGN>
{
    let mut term_nodes = vec![];

    for (link_item, item) in items {
        if item.is_term(grammar) || item.is_end() {
            t_pack.set_closure_link(*item, *link_item);
            let node =
                TGN::new(t_pack, item.get_symbol(grammar), parent_index, vec![
                    item.clone(),
                ]);

            term_nodes.push(node);
        } else {
            for closure_item in get_closure(&vec![*item], grammar) {
                if !closure_item.is_nonterm(grammar) && !discard.contains(item)
                {
                    let closure_item = closure_item.to_state(item.get_state());

                    t_pack.set_closure_link(closure_item.clone(), *item);

                    let node = TGN::new(
                        t_pack,
                        closure_item.get_symbol(grammar),
                        parent_index,
                        vec![closure_item],
                    );

                    term_nodes.push(node);
                }
            }
        }
    }

    term_nodes
}
/// Constructs a Vector of Vectors, each of which contain items that
/// have been grouped by the hash of a common distinguisher.

fn hash_group<
    T: Copy + Sized,
    R: Hash + Sized + Eq,
    Function: Fn(usize, T) -> R,
>(
    vector: Vec<T>,
    hash_yielder: Function,
) -> Vec<Vec<T>>
{
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

/// Places goal nodes onto the tips of peek leaf nodes to complete
/// a peek parse path. We then resume constructing the transition
/// graph from the goal nodes onward.
///
/// ### Notes
/// - #### Graph Structure
/// Multiple peek leaves may resolve to a single goal, so we make sure
/// we only continue construction of the transition graph using a
/// single goal node by allowing that node to point to multiple parent
/// nodes by way of the `proxy_parent` vector.
///
/// - #### Scanner Nodes
/// Scanners do not peek, but instead greedily match characters
/// as they traverse the disambiguating path, yielding the lengths of
/// disambiguated tokens. Thus, when we reach leaf peek nodes of a
/// scanner scope, we simply complete any outstanding items and yield
/// goto productions to be resolved within the goto states. The
/// transitions of all parent nodes are reconfigured as
/// ASSERT_CONSUME.
#[inline]
fn process_peek_leaves(
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    leaves: Vec<usize>,
) -> Vec<usize>
{
    let mut resolved_leaves = Vec::<usize>::new();

    if t_pack.is_scanner {
        for node_index in leaves {
            // for node_index in peek_leaf_group {
            // Instead of resetting our position back to
            // the goal item, we simply continue parsing
            // from whatever position we are at.

            {
                let node = t_pack.get_node_mut(node_index);

                node.unset_type(TST::O_PEEK);
            }

            let mut iter_index = node_index;

            while !t_pack.get_node(iter_index).is(TST::I_PEEK_ROOT) {
                let node = t_pack.get_node_mut(iter_index);

                node.unset_type(TST::O_PEEK);

                node.set_type(TST::I_CONSUME);

                iter_index = node.parent;
            }

            let node = t_pack.get_node_mut(node_index);

            if node.items.iter().any(|i| i.at_end()) {
                node.unset_type(TST::I_CONSUME);
            } else {
                node.set_type(TST::I_CONSUME);
            }

            process_node(node_index, node_index, grammar, t_pack, true);
        }
    } else {
        for peek_leaf_group in
            hash_group(leaves, |_, leaf| t_pack.get_node(leaf).goal)
                .iter()
                .cloned()
        {
            let primary_peek_parent_index = peek_leaf_group[0];

            let prime_node = t_pack.get_node(primary_peek_parent_index);

            let goal_index = prime_node.goal;

            let goal_node = t_pack.get_node(goal_index);

            if goal_node.is(TST::I_OUT_OF_SCOPE) {
                for node_index in peek_leaf_group {
                    t_pack
                        .get_node_mut(node_index)
                        .set_type(TST::I_FAIL | TST::I_OUT_OF_SCOPE)
                }
            } else {
                // Use the goal node as a proxy to generate child nodes that
                // are then linked to the current peek leaf nodes.

                let parent_node_indices = peek_leaf_group
                    .into_iter()
                    .map(|n| {
                        let node_depth = t_pack.get_node(n).depth;
                        let node_parent = t_pack.get_node(n).parent;

                        if node_depth == 0 {
                            t_pack.drop_node(&n);

                            node_parent
                        } else {
                            n
                        }
                    })
                    .collect::<Vec<_>>();

                let primary_parent = parent_node_indices[0];

                let proxy_parents = parent_node_indices[1..].to_owned();

                let have_proxy_parents = proxy_parents.len() > 0;

                for child_index in process_node(
                    goal_index,
                    primary_parent,
                    grammar,
                    t_pack,
                    false,
                ) {
                    let child_node = t_pack.get_node_mut(child_index);

                    child_node.parent = primary_parent;

                    if have_proxy_parents {
                        child_node
                            .proxy_parents
                            .append(&mut proxy_parents.to_owned());
                    }

                    resolved_leaves.push(child_index);
                }

                // Note: Remember all goal nodes are DROPPED at the
                // end of the peek resolution process
            }
        }
    }

    resolved_leaves
}

fn process_end_item(
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    end_item: Item,
    parent_index: usize,
) -> Vec<usize>
{
    let link = t_pack.get_closure_link(&end_item);

    if t_pack.is_scanner && !t_pack.get_closure_link(&end_item).is_null() {
        // We need to be in initial closure before we can allow
        // a complete parse run. Thus we recommission the end_state
        // portion of the disambiguation logic to continue processing
        // the states until we arrive at an end_item that is indeed
        // directly connected to the initial closure.

        let (term_items, end_items) = scan_items(&end_item, grammar, t_pack);

        let items = term_items
            .into_iter()
            .map(|(_, i)| i)
            .chain(end_items.into_iter().map(|(_, i)| i))
            .collect::<Vec<_>>();

        let node = TGN::new(t_pack, SymbolID::EndOfFile, parent_index, items);

        let node_index = t_pack.insert_node(node);

        let results =
            process_node(node_index, parent_index, grammar, t_pack, false);

        t_pack.drop_node(&node_index);

        vec![node_index]
    } else {
        let end_node =
            TGN::new(t_pack, SymbolID::EndOfFile, parent_index, vec![end_item]);

        let node_index = t_pack.insert_node(end_node);

        t_pack
            .get_node_mut(node_index)
            .set_type(TST::I_END | TST::O_ASSERT);
        t_pack.goto_items.insert(end_item);
        t_pack.leaf_nodes.push(node_index);

        vec![node_index]
    }
}

#[inline]
fn disambiguate(
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    node_ids: Vec<usize>,
    leaves: &mut Vec<usize>,
    depth: u32,
)
{
    let mut term_nodes = vec![];
    let mut end_nodes = vec![];

    // We must first complete end-items and generate new
    // nodes that arise from the completion of a production.

    for node_index in node_ids {
        let node = t_pack.get_node(node_index);
        let item = node.items[0];
        let goal = node.goal;
        let parent_index = node.parent;

        if !item.is_end() {
            term_nodes.push(node_index)
        } else {
            let (mut terms, mut final_ends) = get_continue_items(
                item,
                parent_index,
                depth,
                t_pack,
                grammar,
                goal,
            );

            if terms.is_empty() && final_ends.is_empty() {
                end_nodes.push(node_index);
            } else {
                term_nodes.append(&mut terms);
                end_nodes.append(&mut final_ends);
                t_pack.drop_node(&node_index);
            }
        }
    }

    match end_nodes.len() {
        0 => {}
        1 => {
            set_transition_type(end_nodes[0], t_pack, depth);

            leaves.push(end_nodes[0]);
        }
        _ => {
            let goals = get_goals(&end_nodes, &t_pack);

            if goals.len() == 1 {
                for end_node in &end_nodes[1..] {
                    t_pack.drop_node(end_node);
                }

                set_transition_type(end_nodes[0], t_pack, depth);

                leaves.push(end_nodes[0]);
            } else {
                handle_unresolved_nodes(end_nodes, grammar, t_pack, leaves);
            }
        }
    }

    let mut groups =
        hash_group(term_nodes, |_, n| t_pack.get_node(n).sym.clone());

    let mut next_peek_groups = vec![];

    merge_occluding_groups(&mut groups, t_pack, grammar);

    for group in groups.iter() {
        let prime_node_index = group[0];

        let goals = get_goals(&group, t_pack);

        set_transition_type(prime_node_index, t_pack, depth);

        if goals.len() > 1 && group.len() > 1 {
            let mut peek_transition_group = vec![];

            for node_index in group.iter().cloned() {
                let transition_on_skipped_symbol =
                    t_pack.get_node(node_index).is(TST::I_SKIPPED_COLLISION);

                let goal = t_pack.get_node(node_index).goal;

                for mut node in create_term_nodes_from_items(
                    &t_pack
                        .get_node(node_index)
                        .items
                        .clone()
                        .into_iter()
                        .map(|i| {
                            let link = t_pack.get_closure_link(&i);
                            if transition_on_skipped_symbol {
                                (link, i)
                            } else {
                                (link, i.increment().unwrap())
                            }
                        })
                        .collect::<Vec<_>>(),
                    grammar,
                    t_pack,
                    prime_node_index,
                    &HashSet::new(),
                ) {
                    node.goal = goal;
                    peek_transition_group.push(t_pack.insert_node(node));
                }
            }

            if peek_transition_group.is_empty() {
                if get_goals(&group, t_pack)
                    .iter()
                    .all(|i| t_pack.get_node(*i).is(TST::I_OUT_OF_SCOPE))
                {
                    leaves.push(prime_node_index);
                } else {
                    panic!(
                        "Invalid state, unable to continue disambiguating \n"
                    );
                }
            } else {
                next_peek_groups.push(peek_transition_group);
            }
        } else {
            leaves.push(prime_node_index);
        }

        for node_index in group.iter().cloned() {
            if node_index != prime_node_index {
                let items = t_pack.get_node(node_index).items.clone();

                t_pack.drop_node(&node_index);

                t_pack
                    .get_node_mut(prime_node_index)
                    .items
                    .append(&mut items.clone());
            }
        }
    }

    for peek_group in next_peek_groups {
        if peek_group.len() > 0 {
            if handle_shift_reduce_conflicts(
                &peek_group,
                grammar,
                t_pack,
                leaves,
            ) {
                continue;
            } else if groups_are_aliased(&peek_group, t_pack)
                || group_is_repeated(&peek_group, t_pack)
            {
                handle_unresolved_nodes(peek_group, grammar, t_pack, leaves);
            } else {
                disambiguate(grammar, t_pack, peek_group, leaves, depth + 1);
            }
        }
    }
}

fn get_continue_items(
    end_item: Item,
    parent_index: usize,
    peek_depth: u32,
    t_pack: &mut TPack,
    grammar: &GrammarStore,
    goal: usize,
) -> (Vec<usize>, Vec<usize>)
{
    let mut term_nodes = vec![];
    let mut final_nodes = vec![];

    let discard = HashSet::<Item>::new();

    if peek_depth == 0
        && t_pack.mode == TransitionMode::GoTo
        && end_item.decrement().unwrap().is_nonterm(grammar)
    {
        // TODO: Remove items that present in other branches
        // to prevent item aliasing.
    }
    let mut need_to_prune = false;

    let (scan_items, final_items) = scan_items(&end_item, grammar, t_pack);

    for mut term_node in create_term_nodes_from_items(
        &scan_items,
        grammar,
        t_pack,
        parent_index,
        &discard,
    ) {
        term_node.goal = goal;
        term_nodes.push(t_pack.insert_node(term_node));
    }

    for mut final_node in create_term_nodes_from_items(
        &final_items,
        grammar,
        t_pack,
        parent_index,
        &discard,
    ) {
        final_node.goal = goal;
        final_node.set_type(TST::I_END | TST::O_ASSERT);
        final_nodes.push(t_pack.insert_node(final_node));
    }

    (term_nodes, final_nodes)
}

fn group_is_repeated(peek_group: &Vec<usize>, t_pack: &mut TPack) -> bool
{
    // panic!("Todo: group_is_repeated")
    let group_id = peek_group
        .iter()
        .flat_map(|i| {
            let node = t_pack.get_node(*i);
            node.items.iter().map(|i| i.to_zero_state().to_hash())
        })
        .collect::<BTreeSet<_>>();

    let ids = group_id.iter().collect::<Vec<_>>();

    let hash_id = hash_id_value_u64(ids.clone());

    !t_pack.peek_ids.insert(hash_id)
}

fn handle_shift_reduce_conflicts(
    peek_group: &Vec<usize>,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    leaves: &mut Vec<usize>,
) -> bool
{
    let goals = get_goals(peek_group, t_pack)
        .into_iter()
        .map(|i| (i, &t_pack.get_node(i).items))
        .collect::<Vec<_>>();

    if goals.len() == 2 {
        if goals[0].1.len() == 1
            && goals[1].1.len() == 1
            && (t_pack.mode == TransitionMode::GoTo)
            || (goals[0].1[0].get_production_id(grammar)
                == goals[1].1[0].get_production_id(grammar))
        {
            let shift = goals
                .iter()
                .filter(|(_, i)| !i[0].is_end())
                .collect::<Vec<_>>();

            let reduce = goals
                .iter()
                .filter(|(_, i)| i[0].is_end())
                .collect::<Vec<_>>();

            if !shift.is_empty() && !reduce.is_empty() {
                let shift_goal = shift[0].0;

                for node_index in peek_group {
                    if t_pack.get_node(*node_index).goal == shift_goal {
                        leaves.push(*node_index)
                    } else {
                        t_pack.drop_node(node_index);
                    }
                }

                return true;
            }
        }
    }

    false
}

fn groups_are_aliased(peek_group: &Vec<usize>, t_pack: &mut TPack) -> bool
{
    return false;

    hash_group(peek_group.to_owned(), |_, n| {
        t_pack
            .get_node(n)
            .items
            .clone()
            .iter()
            .map(|i| i.to_zero_state())
            .collect::<Vec<_>>()
            .sort()
    })
    .iter()
    .any(|g| g.len() > 1)
}

/// Compares the terminal symbols of node groups and merges those
/// groups whose terminal symbols occlude each other.
///
/// For instance, given a group `A` with the symbol `g:id` and an
/// other group `B` with symbol `\g`, the character `g` could be
/// accepted by either group. As long as group `A` (the "defined"
/// group) is not exclusive, we merge group `B` into `A` to into
/// account the ambiguous nature of the groups.

fn merge_occluding_groups(
    groups: &mut Vec<Vec<usize>>,
    t_pack: &TPack,
    grammar: &GrammarStore,
)
{
    // Clone the from_group store so we are able
    // to merge its members into to_groups without
    // going fowl of the borrow checker.
    for from_group in groups.clone() {
        let from_sym = t_pack.get_node(from_group[0]).sym;

        for to_group in groups.iter_mut() {
            let to_sym = t_pack.get_node(to_group[0]).sym;

            if from_sym == to_sym {
                continue;
            }

            if symbols_occlude(&to_sym, &from_sym, grammar) {
                to_group.append(&mut from_group.clone());
            }
        }
    }
}

/// Compares whether symbolB occludes symbolA
/// ( produces an ambiguous parse path )
///
/// Symbols that can occlude are as follows
///
/// - `g:id` and any single identifier character.
/// - `g:num` and any single numeric character.
/// - `g:sym` and any single character thats not a numeric,
///   identifier, space, newline, or tab.

fn symbols_occlude(
    symA: &SymbolID,
    symB: &SymbolID,
    grammar: &GrammarStore,
) -> bool
{
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

fn get_goals(end_nodes: &Vec<usize>, t_pack: &TPack) -> Vec<usize>
{
    BTreeSet::<usize>::from_iter(
        end_nodes.iter().map(|n| t_pack.get_node(*n).goal),
    )
    .into_iter()
    .collect::<Vec<_>>()
}

fn handle_unresolved_nodes(
    peek_group: Vec<usize>,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    leaves: &mut Vec<usize>,
)
{
    if t_pack.is_scanner {
    } else {
        let goals = get_goals(&peek_group, t_pack);

        if goals.len() < 2 {
            panic!("Unexpectedly ended up here with only one goal!");
        }

        // TODO: Filter out low priority goals.

        // Create a fork state -----------------------------------------------

        let prime_node = peek_group[0];
        let mut parent = 0;
        let mut items = vec![];

        for node_index in &peek_group[0..peek_group.len()] {
            items.push(t_pack.get_node(*node_index).items[0].clone());
            parent = t_pack.drop_node(node_index);
        }

        let mut fork_node = TGN::new(t_pack, SymbolID::Default, parent, items);

        fork_node.set_type(TST::I_FORK);

        fork_node.parent = parent;

        let fork_node_index = t_pack.insert_node(fork_node);

        for goal_index in goals {
            process_node(goal_index, fork_node_index, grammar, t_pack, false);
        }
    }
}

/// Set the transition type of a peeking based on whether
/// it is first node in the peek path or not. If it is the first
/// node, we do regular ASSERT action on the terminal symbol.
/// Otherwise we use a PEEK action.

fn set_transition_type(node_index: usize, t_pack: &mut TPack, depth: u32)
{
    t_pack.get_node_mut(node_index).set_type(match depth {
        0 => TST::O_ASSERT,
        _ => TST::O_PEEK,
    })
}

/// Retrieve terminal items derived from a
/// completed item.
///
/// ### Returns
/// A vector of Item tuples, where the first item is the
/// previous link item for closure resolution, and the second item is
/// a non-end item  produced from the reduction of the
/// `root_end_item`.
fn scan_items(
    root_end_item: &Item,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
) -> (Vec<(Item, Item)>, Vec<(Item, Item)>)
{
    let mut seen = HashSet::<(Item, Item)>::new();
    let mut out = HashSet::<(Item, Item)>::new();
    let mut final_items = HashSet::<(Item, Item)>::new();

    static empty_vec: Vec<Item> = Vec::new();
    // Starting at the top we grab the closure to the nearest
    // non-term link.

    // Stores the end item [1] and its immediate closure item [0]
    let mut end_items: VecDeque<(Item, Item)> = VecDeque::from_iter(vec![(
        t_pack.get_closure_link(&root_end_item),
        *root_end_item,
    )]);
    while let Some((nonterm_closure_link, end_item)) = end_items.pop_front() {
        if seen.insert((nonterm_closure_link, end_item).clone()) {
            let production = end_item.get_production_id(grammar);
            // Grab all productions from the closure that match the end item's
            // production.
            match {
                if nonterm_closure_link.is_null() {
                    if let Some(closures) = t_pack.scoped_closures.get(
                        nonterm_closure_link.get_state().get_closure_index(),
                    ) {
                        closures.iter()
                    } else {
                        empty_vec.iter()
                    }
                } else if nonterm_closure_link.is_out_of_scope() {
                    if let Some(closure) = &t_pack.goto_scoped_closure {
                        closure.iter()
                    } else {
                        empty_vec.iter()
                    }
                } else {
                    get_closure_cached(
                        &nonterm_closure_link.to_zero_state(),
                        grammar,
                    )
                    .iter()
                }
                .filter(|i| i.get_production_id_at_sym(grammar) == production)
                .cloned()
                .collect::<Vec<_>>()
            } {
                p if p.is_empty() => {
                    if nonterm_closure_link.is_null() {
                        t_pack.set_closure_link(
                            end_item.clone(),
                            nonterm_closure_link.clone(),
                        );
                        final_items.insert((
                            nonterm_closure_link.clone(),
                            end_item.clone(),
                        ));
                    } else {
                        end_items.push_back((
                            t_pack.get_closure_link(&nonterm_closure_link),
                            end_item.clone(),
                        ));
                    }
                }
                production_items => {
                    for production_item in production_items {
                        let incremented_item = production_item
                            .to_state(end_item.get_state())
                            .increment()
                            .unwrap();

                        if incremented_item.is_end() {
                            end_items.push_back((
                                nonterm_closure_link.clone(),
                                incremented_item,
                            ));
                        } else {
                            t_pack.set_closure_link(
                                incremented_item,
                                nonterm_closure_link.clone(),
                            );
                            out.insert((
                                nonterm_closure_link.clone(),
                                incremented_item,
                            ));
                        }
                    }
                }
            }
        }
    }

    (
        out.into_iter().collect::<Vec<_>>(),
        final_items.into_iter().collect::<Vec<_>>(),
    )
}

fn process_terminal_node(
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    term_item: Item,
    parent_index: usize,
) -> Vec<usize>
{
    let sym = term_item.get_symbol(grammar);

    let new_node = TGN::new(t_pack, sym, parent_index, vec![term_item]);

    let node_index = t_pack.insert_node(new_node);

    t_pack
        .get_node_mut(node_index)
        .set_type(TST::I_CONSUME | TST::O_TERMINAL);

    t_pack.queue_node(node_index);

    vec![node_index]
}

fn create_production_call(
    production: ProductionId,
    grammar: &GrammarStore,
    t_pack: &mut TPack,
    nonterm_items: Vec<Item>,
    parent_index: usize,
) -> Vec<usize>
{
    let mut node = TGN::new(
        t_pack,
        SymbolID::Production(production, GrammarId(0)),
        parent_index,
        nonterm_items,
    );

    node.transition_type |= TST::O_PRODUCTION;

    let node_index = t_pack.insert_node(node);

    t_pack.queue_node(node_index);

    vec![node_index]
}
