use std::collections::btree_map;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::path::PathBuf;
use std::process::id;
use std::rc::Rc;
use std::vec;

use crate::grammar::create_closure;
use crate::grammar::get_closure_cached;
use crate::grammar::get_production_start_items;
use crate::grammar::hash_id_value_u64;
use crate::types::BlameColor;
use crate::types::GrammarId;
use crate::types::GrammarStore;
use crate::types::Item;
use crate::types::ItemState;
use crate::types::OriginData;
use crate::types::ProductionId;
use crate::types::SymbolID;
use crate::types::TPackResults;
use crate::types::TransitionGraphNode as TGN;
use crate::types::TransitionGraphNodeId;
use crate::types::TransitionMode;
use crate::types::TransitionPack as TPack;
use crate::types::TransitionStateType as TST;

/// Remove items that would cause LL branch conflicts and replace
/// with their derivatives.
///  
/// ## For Example
///
/// ```hcg
///  
/// <> A > B | C
///
/// <> C > \d
///
/// <> B > C \d
/// ```
/// Given the above grammar, the starting items for `A` are
/// `B => C \d` and `C => \d`. If treated directly as RD items
/// then the input `d d` requires us to determine if the resulting
/// Production `C` (from `C => \d *`) should apply to `B => C * \d`
/// or `A => C *`. Pure RD would either require a peek or a backtrack
/// since the Production A must call either C or B. However, if using
/// LR behavior, then we can call `C`, then use A's GOTO state:
/// ```hcg
/// A`Goto
///    1 | B     -> A
///    2 | C     -> A
///    3 | C \d  -> B
/// ```
/// to reduce `C` to `B` (as a result of the next symbol `\d` matching the conditions
/// for the transition to GOTO 3: `C * \d -> B`),
/// then `B` to `A`.
pub fn get_valid_starts(starts: &[Item], g: &GrammarStore) -> (Vec<Item>, Vec<Item>) {
  if starts.len() == 1 {
    // There's is now way the parse of single can encounter conflicts, so it is simply returned.
    (starts.to_vec(), vec![])
  } else {
    let mut goto_seeds = BTreeSet::new();

    let mut valid_term_items =
      starts.iter().flat_map(|i| get_closure_cached(i, g)).cloned().collect::<BTreeSet<_>>();

    let mut valid_non_term =
      valid_term_items.drain_filter(|i| i.is_nonterm(g)).collect::<BTreeSet<_>>();

    // Detects if a specific non-term symbol is parsed by one or more
    // unique productions. If this is the case, then we make the productions
    // invalid.
    let mut invalid_productions = valid_non_term
      .iter()
      .map(|i| (i.get_production_id_at_sym(g), i.get_prod_id(g)))
      .fold(BTreeMap::<ProductionId, BTreeSet<ProductionId>>::new(), |mut b, (sym, p)| {
        match b.entry(sym) {
          Entry::Occupied(mut e) => {
            e.get_mut().insert(p);
          }
          Entry::Vacant(e) => {
            e.insert(BTreeSet::from_iter(vec![p]));
          }
        };
        b
      })
      .into_iter()
      .filter_map(|(a, b)| if b.len() > 1 { Some(a) } else { None })
      .chain(starts.iter().map(|s| s.get_prod_id(g)))
      .collect::<BTreeSet<_>>();

    // Now we filter out the invalid productions.
    // Any production that parses a invalid non-term is itself invalid
    // We remove all items of that production from the output  (placing them instead
    // into the goto seeds set), and place the production into the invalid production set.
    // This continues until there are no new productions added to the invalid set.
    let mut changed = true;
    while changed {
      changed = false;
      goto_seeds.append(
        &mut valid_non_term
          .drain_filter(|item| {
            if invalid_productions.contains(&item.get_production_id_at_sym(g)) {
              invalid_productions.insert(item.get_prod_id(g));
              changed = true;
              true
            } else if invalid_productions.contains(&item.get_prod_id(g)) {
              true
            } else {
              false
            }
          })
          .collect(),
      );
    }

    // Now we have a nearly complete valid set of non-term items. There may be
    // some items that parse a non-term symbol whose items also exist in the
    // valid set. These items are now moved to the goto seeds set.
    let valid_productions =
      valid_non_term.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>();
    goto_seeds.append(
      &mut valid_non_term
        .drain_filter(|i| valid_productions.contains(&i.get_production_id_at_sym(g)))
        .collect(),
    );

    // Now the term items of productions being parsed by non-term items need not
    // be present in the term set. We'll remove them now.
    let parsed_productions =
      valid_non_term.iter().map(|i| i.get_production_id_at_sym(g)).collect::<BTreeSet<_>>();
    valid_term_items.drain_filter(|i| parsed_productions.contains(&i.get_prod_id(g)));

    // The union of valid non-term items and the remaining term items is our
    // start set.
    valid_term_items.append(&mut valid_non_term);

    (valid_term_items.into_iter().collect(), goto_seeds.into_iter().collect())
  }
}

/// Constructs an initial transition graph that parses a production
/// using a recursive descent strategy. Productions that are ambiguous
/// or are left recursive cannot be parsed, so this tries to do its
/// best to define a parse path for a production before we have to
/// resort to LR and Forking based parse strategies.

pub fn construct_recursive_descent(
  g: &GrammarStore,
  is_scanner: bool,
  starts: &[Item],
  root_ids: BTreeSet<ProductionId>,
) -> TPackResults {
  let start_items = apply_state_info(starts);

  let mut t_pack =
    TPack::new(g, TransitionMode::RecursiveDescent, is_scanner, &start_items, root_ids);

  for item in &start_items {
    t_pack.scoped_closures.push(get_closure_cached(item, g).clone());

    if item.is_nonterm(g) {
      t_pack.set_closure_link(*item, item.to_null());
    }
  }

  let root_index =
    t_pack.insert_node(TGN::new(&t_pack, SymbolID::Undefined, usize::MAX, start_items));

  t_pack.get_node_mut(root_index).set_type(TST::I_DESCENT_START);

  t_pack.queue_node(root_index);

  while let Some(node_id) = t_pack.get_next_queued() {
    process_node(g, &mut t_pack, node_id, node_id, node_id != 0);
  }

  t_pack.clean()
}

fn apply_state_info(starts: &[Item]) -> Vec<Item> {
  let start_items = starts
    .iter()
    .enumerate()
    .map(|(i, item)| item.to_state(ItemState::new(i as u32, 0)))
    .collect::<Vec<_>>();
  start_items
}

pub fn construct_goto(
  g: &GrammarStore,
  is_scanner: bool,
  starts: &[Item],
  goto_seeds: &[Item],
  root_ids: BTreeSet<ProductionId>,
) -> (TPackResults, /* True if there exists a non-trivial production branch */ bool) {
  let start_items = apply_state_info(starts);

  let mut t_pack = TPack::new(g, TransitionMode::GoTo, is_scanner, &start_items, root_ids);

  // Get closures of all items that could transition on the same production.

  let global_closure = Rc::new(Box::<Vec<Item>>::new(if t_pack.is_scanner {
    vec![]
  } else {
    get_follow_closure(g, goto_seeds, &t_pack.root_prod_ids)
  }));

  let mut parent = TGN::new(&t_pack, SymbolID::Undefined, TGN::OrphanIndex, goto_seeds.to_vec());

  parent.set_type(TST::I_GOTO_START);

  let parent_index = t_pack.insert_node(parent);

  let mut unfulfilled_root = Some(*t_pack.root_prod_ids.first().unwrap());

  for (production_id, group) in get_goto_starts(g, &start_items, goto_seeds) {
    t_pack.goto_scoped_closure = Some(global_closure.clone());

    let have_root_production = t_pack.root_prod_ids.contains(&production_id);
    let mut have_end_items = false;

    let mut items: Vec<Item> = group
      .iter()
      .map(|i| {
        let stated_item = i.increment().unwrap();

        let stated_item = if stated_item.at_end() {
          have_end_items = true;
          stated_item.to_state(ItemState::GOTO_END_GOAL_STATE)
        } else {
          t_pack.scoped_closures.push(get_closure_cached(&stated_item, g).clone());

          stated_item.to_state(ItemState::new((t_pack.scoped_closures.len() - 1) as u32, 0))
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

    if have_root_production || (group.len() > 1 && have_end_items) {
      t_pack.out_of_scope_closure =
        Some(g.lr_items.iter().flat_map(|(_, i)| i).cloned().collect::<Vec<Item>>());

      if have_root_production {
        unfulfilled_root = None;
        let mut reducible: Vec<Item> = g
          .lr_items
          .get(&production_id)
          .unwrap_or(&Vec::new())
          .iter()
          .filter(|i| !group.iter().any(|g| *g == **i))
          .map(|i| i.increment().unwrap().to_state(ItemState::GOTO_ROOT_END_GOAL_STATE))
          .collect();

        goto_node.items.append(&mut reducible.clone());
        items.append(&mut reducible);
      }

      let node_id = t_pack.insert_node(goto_node);
      create_peek(g, &mut t_pack, node_id, items);
    } else {
      let node_index = t_pack.insert_node(goto_node);
      process_node(g, &mut t_pack, node_index, node_index, false);
    }
  }

  // If the root production is not covered in the goto branches
  // then create a new node that serves as an accepting state
  // if the active production id is the root.

  if let Some(production_id) = unfulfilled_root {
    let mut goto_node = TGN::new(
      &t_pack,
      SymbolID::Production(production_id, GrammarId::default()),
      parent_index,
      vec![],
    );
    goto_node.set_type(TST::O_GOTO_ROOT_ACCEPT | TST::I_PASS);
    let index = t_pack.insert_node(goto_node);
    t_pack.leaf_nodes.push(index);
  }

  while let Some(node_index) = t_pack.get_next_queued() {
    process_node(g, &mut t_pack, node_index, node_index, node_index != 0);
  }

  (t_pack.clean(), unfulfilled_root.is_none())
}

pub fn get_goto_starts(
  g: &GrammarStore,
  starts: &Vec<Item>,
  seeds: &[Item],
) -> Vec<(ProductionId, BTreeSet<Item>)> {
  type GotoMap = BTreeMap<ProductionId, BTreeSet<Item>>;

  let mut output_items = GotoMap::new();

  for item in seeds.iter() {
    output_items
      .entry(item.get_production_id_at_sym(g))
      .and_modify(|e| {
        e.insert(*item);
      })
      .or_insert_with(|| BTreeSet::from_iter(vec![*item]));
  }

  output_items.into_iter().collect()
}

pub fn get_follow_closure(
  g: &GrammarStore,
  gotos: &[Item],
  root_ids: &BTreeSet<ProductionId>,
) -> Vec<Item> {
  let mut pending_prods = VecDeque::<ProductionId>::new();
  let mut seen_prods = BTreeSet::<ProductionId>::new();

  for prod_id in root_ids {
    pending_prods.push_back(*prod_id);
  }

  let mut output = BTreeSet::new();

  while let Some(production_id) = pending_prods.pop_front() {
    if !seen_prods.insert(production_id) {
      continue;
    }

    let items: Vec<Item> = g
      .lr_items
      .get(&production_id)
      .unwrap_or(&Vec::new())
      .iter()
      .map(|i| i.increment().unwrap())
      .collect();

    for item in items {
      if item.is_end() {
        pending_prods.push_back(item.get_prod_id(g));
      }

      output.insert(item.decrement().unwrap());
    }

    seen_prods.insert(production_id);
  }

  output.into_iter().collect()
}

fn process_node(
  g: &GrammarStore,
  t_pack: &mut TPack,
  node_id: usize,
  par_id: usize,
  allow_increment: bool,
) -> Vec<usize> {
  let node = t_pack.get_node(node_id).clone();
  let mut items: Vec<Item> = node.items.clone();

  if allow_increment {
    items = items
      .into_iter()
      .map(|i| {
        if i.at_end() {
          i
        } else {
          let item_next = i.increment().unwrap();
          let item_link = t_pack.get_closure_link(&i);
          t_pack.set_closure_link(item_next, item_link);
          item_next
        }
      })
      .collect();
  }

  let n_term: Vec<Item> = items.iter().filter(|i| i.is_term(g)).cloned().collect();
  let n_nonterm: Vec<Item> = items.iter().filter(|i| i.is_nonterm(g)).cloned().collect();
  let n_end: Vec<Item> = items.iter().filter(|i| i.is_end()).cloned().collect();

  // If the depth is 0 and we are not trying to produce
  // a scanner state tree, then we can attempt to find the
  // highest common production between non-term items. That
  // is the production derivative that is non-term that all
  // or a subset of non-term items share.

  if !n_nonterm.is_empty() {
    let production_ids =
      BTreeSet::<ProductionId>::from_iter(n_nonterm.iter().map(|i| i.get_production_id_at_sym(g)));

    if n_term.is_empty()
      && n_end.is_empty()
      && production_ids.len() == 1
      && (node.shifts > 0 || non_recursive(&n_nonterm, &t_pack.root_prod_ids, g))
    {
      create_production_call(g, t_pack, *production_ids.iter().next().unwrap(), n_nonterm, par_id)
    } else {
      create_peek(g, t_pack, par_id, items)
    }
  } else if n_end.len() == 1 && n_term.is_empty() {
    // A single end item
    process_end_item(g, t_pack, n_end[0], par_id)
  } else if n_end.is_empty() && n_term.len() == 1 {
    // A single terminal item
    process_terminal_node(g, t_pack, &vec![n_term[0]], par_id)
  } else if n_end.is_empty()
    // The terminal symbol is identical in all items.
    && n_term.iter().map(|t| t.get_symbol(g)).collect::<BTreeSet<_>>().len() == 1
  {
    process_terminal_node(g, t_pack, &n_term, par_id)
  } else {
    // else if n_end.is_empty() && n_term.len() > 1  {
    // Multiple terminal items
    create_peek(g, t_pack, par_id, items)
  }
}

/// True if the closure of the givin items does not include
/// the goal production on the right of the cursor

fn non_recursive(items: &[Item], target_prod: &BTreeSet<ProductionId>, g: &GrammarStore) -> bool {
  for item in create_closure(items, g) {
    if let SymbolID::Production(production, _) = item.get_symbol(g) {
      if target_prod.contains(&production) {
        return false;
      }
    }
  }

  true
}

fn create_peek(
  g: &GrammarStore,
  t_pack: &mut TPack,
  par_id: usize,
  items: Vec<Item>,
) -> Vec<usize> {
  let mut cached_depth = 0;
  let mut peek_nodes = vec![];

  {
    let parent = t_pack.get_node_mut(par_id);
    parent.set_type(TST::I_PEEK_ORIGIN);
    cached_depth = parent.shifts;
  }

  let goals: Vec<usize> = hash_group(items.clone(), |ind, i| {
    if i.at_end() {
      format!("at_end_{}", ind)
    } else {
      format!("{:?} {}", i.get_symbol(g), i.is_out_of_scope())
    }
  })
  .iter()
  .enumerate()
  .map(|(i, items)| {
    let item = items[0];

    let mut goal_node = TGN::new(t_pack, item.get_symbol(g).to_owned(), usize::MAX, items.to_vec());

    goal_node.shifts = cached_depth;

    if item.is_out_of_scope() {
      goal_node.trans_type |= TST::I_OUT_OF_SCOPE;
      if item.is_goto_end_origin() {
        goal_node.trans_type |= TST::I_GOTO_END;
      } else {
        goal_node.trans_type |= TST::I_GOTO_ROOT;
      }
    } else if t_pack.mode == TransitionMode::GoTo && item.at_end() {
      goal_node.trans_type |= TST::I_COMPLETE;
    }

    t_pack.insert_node(goal_node)
  })
  .collect();
  for (i, goal_index) in goals.iter().cloned().enumerate() {
    let items = t_pack
      .get_node(goal_index)
      .items
      .to_owned()
      .iter()
      .map(|i| (t_pack.get_closure_link(i), *i))
      .collect::<Vec<_>>();

    for mut node in
      create_term_nodes_from_items(&create_term_item_closure(g, t_pack, &items), g, t_pack, par_id)
    {
      node.goal = goal_index;

      node.shifts = t_pack.get_node(goal_index).shifts;

      node.peek_shifts = 0;

      peek_nodes.push(t_pack.insert_node(node));
    }
  }

  let mut leaves = vec![];

  disambiguate(g, t_pack, peek_nodes, &mut leaves, 0);

  let resolved_leaves = process_peek_leaves(g, t_pack, leaves);

  // All goal nodes can be recycled, as copy operations where used to
  // insert goal nodes as children of peek leaves
  for goal in goals {
    t_pack.drop_node(&goal);
  }

  t_pack.clear_peek_data();

  resolved_leaves
}

fn create_term_item_closure(
  g: &GrammarStore,
  t_pack: &mut TPack,
  items: &Vec<(Item, Item)>,
) -> Vec<(Item, Item)> {
  let mut term_items = BTreeSet::new();
  for (link_item, item) in items {
    if item.is_term(g) || item.is_end() {
      term_items.insert((*link_item, *item));
    } else {
      for closure_item in create_closure(&[*item], g) {
        let closure_item = closure_item.to_origin(item.get_origin());
        if !closure_item.is_nonterm(g) {
          let closure_item = closure_item.to_state(item.get_state());
          term_items.insert((*item, closure_item));
        }
      }
    }
  }
  term_items.into_iter().collect::<Vec<_>>()
}

fn create_term_nodes_from_items(
  items: &Vec<(Item, Item)>,
  g: &GrammarStore,
  t_pack: &mut TPack,
  par_id: usize,
) -> Vec<TGN> {
  let mut term_nodes = vec![];

  for (link_item, item) in items {
    t_pack.set_closure_link(*item, *link_item);
    let node = TGN::new(t_pack, item.get_symbol(g), par_id, vec![*item]);
    term_nodes.push(node);
  }

  term_nodes
}

#[inline]
fn disambiguate(
  g: &GrammarStore,
  t_pack: &mut TPack,
  node_ids: Vec<usize>,
  leaves: &mut Vec<usize>,
  peek_depth: u32,
) {
  let mut term_nodes = vec![];
  let mut end_nodes = vec![];
  let mut exclusive_ended = false;

  // We must first complete end-items and generate new
  // nodes that arise from the completion of a production.

  for node_index in node_ids {
    let node = t_pack.get_node(node_index);
    let origin = node.items[0].get_origin();
    let item = node.items[0];
    let goal = node.goal;
    let parent_index = node.parent;

    if !item.is_end() {
      term_nodes.push(node_index)
    } else {
      let (mut terms, mut final_ends) =
        { get_continue_nodes(g, t_pack, item, parent_index, peek_depth, goal) };

      exclusive_ended = match origin {
        OriginData::Symbol(sym) if sym.is_exclusive() => true,
        _ => exclusive_ended,
      };

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
      set_transition_type(t_pack, end_nodes[0], peek_depth);
      leaves.push(end_nodes[0]);
    }
    _ => {
      if get_goals(&end_nodes, t_pack).len() == 1 || all_nodes_are_out_of_scope(&end_nodes, t_pack)
      {
        for end_node in &end_nodes[1..] {
          t_pack.drop_node(end_node);
        }
        set_transition_type(t_pack, end_nodes[0], peek_depth);
        leaves.push(end_nodes[0]);
      } else {
        handle_unresolved_nodes(g, t_pack, end_nodes, leaves);
      }
    }
  }

  let mut nodes_to_be_dropped: BTreeSet<usize> = BTreeSet::new();

  let term_nodes = if exclusive_ended {
    for node_id in term_nodes {
      nodes_to_be_dropped.insert(node_id);
    }
    vec![]
  } else {
    term_nodes
  };

  let mut groups = hash_group(term_nodes, |_, n| (t_pack.get_node(n).terminal_symbol));
  let mut next_peek_groups = vec![];
  let mut primary_nodes: BTreeSet<usize> = BTreeSet::new();

  merge_occluding_groups(g, t_pack, &mut groups);

  for mut group in groups.iter_mut() {
    let prime_node_index = group[0];

    set_transition_type(t_pack, prime_node_index, peek_depth);

    if group.iter().any(|i| t_pack.get_node(*i).is_out_of_scope()) {
      let first = t_pack.get_node(group[0]);
      let term_is_generic = matches!(
        first.terminal_symbol,
        SymbolID::GenericNumber | SymbolID::GenericIdentifier | SymbolID::GenericSymbol
      );

      if !first.is_out_of_scope() {
        // Remove nodes that have items that alias in-scope items
        let inscope_items = group
          .iter()
          .filter_map(|n| {
            let node = t_pack.get_node(*n);
            if (!node.is_out_of_scope()) {
              Some(node.items.iter().map(|i| i.to_zero_state()).collect::<Vec<_>>())
            } else {
              None
            }
          })
          .flatten()
          .collect::<BTreeSet<_>>();

        // Remove outscope items if the symbol of the merged items and
        // original nodes is generic, or if the outscope item occludes
        // an existing inscope item.
        nodes_to_be_dropped.append(
          &mut group
            .drain_filter(|i| {
              let node = t_pack.get_node(*i);
              !(node.is_out_of_scope()
                && (inscope_items.contains(&node.items[0].to_zero_state())
                  || (term_is_generic && node.terminal_symbol == first.terminal_symbol)))
            })
            .collect(),
        );
      }
    }

    let goals = get_goals(group, t_pack);

    if goals.len() > 1 && group.len() > 1 {
      let mut peek_transition_group = vec![];
      for node_index in group.iter().cloned() {
        let transition_on_skipped_symbol = t_pack.get_node(node_index).is(TST::I_SKIPPED_COLLISION);
        let goal = t_pack.get_node(node_index).goal;

        for mut node in create_term_nodes_from_items(
          &create_term_item_closure(
            g,
            t_pack,
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
          ),
          g,
          t_pack,
          prime_node_index,
        ) {
          node.goal = goal;
          node.peek_shifts = (peek_depth + 1) as i32;
          peek_transition_group.push(t_pack.insert_node(node));
        }
      }

      if peek_transition_group.is_empty() {
        if get_goals(group, t_pack).iter().all(|i| t_pack.get_node(*i).is(TST::I_OUT_OF_SCOPE)) {
          leaves.push(prime_node_index);
        } else {
          panic!("Invalid state, unable to continue disambiguating \n");
        }
      } else {
        next_peek_groups.push(peek_transition_group);
      }
    } else {
      leaves.push(prime_node_index);
    }

    primary_nodes.insert(prime_node_index);

    for node_index in group.iter() {
      if *node_index != prime_node_index {
        let items = t_pack.get_node(*node_index).items.clone();
        nodes_to_be_dropped.insert(*node_index);
        t_pack.get_node_mut(prime_node_index).items.append(&mut items.clone());
      }
    }
  }

  for drop_node in nodes_to_be_dropped {
    if !primary_nodes.contains(&drop_node) {
      t_pack.drop_node(&drop_node);
    }
  }

  for peek_group in next_peek_groups {
    if !peek_group.is_empty() {
      if all_nodes_are_out_of_scope(&peek_group, t_pack) {
        for end_node in &peek_group[1..] {
          t_pack.drop_node(end_node);
        }

        set_transition_type(t_pack, peek_group[0], peek_depth);
        leaves.push(peek_group[0]);
      } else if handle_shift_reduce_conflicts(g, t_pack, &peek_group, leaves) {
        continue;
      } else if groups_are_aliased(&peek_group, t_pack) || group_is_repeated(&peek_group, t_pack) {
        handle_unresolved_nodes(g, t_pack, peek_group, leaves);
      } else {
        disambiguate(g, t_pack, peek_group, leaves, peek_depth + 1);
      }
    }
  }
}

fn all_nodes_are_out_of_scope(peek_group: &Vec<usize>, t_pack: &mut TPack) -> bool {
  peek_group.iter().all(|i| t_pack.get_node(*i).items.iter().all(|i| i.is_out_of_scope()))
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
/// ASSERT_SHIFT.
#[inline]
fn process_peek_leaves(g: &GrammarStore, t_pack: &mut TPack, leaves: Vec<usize>) -> Vec<usize> {
  let mut resolved_leaves = Vec::<usize>::new();

  let mut leaves = if t_pack.is_scanner {
    let mut out_leaves = vec![];
    for node_index in leaves {
      // Instead of resetting our position back to
      // the goal item, we simply continue parsing
      // from whatever position we are at.

      if t_pack.get_node_mut(node_index).peek_shifts <= 0 {
        // Allows recursive descent style calls to be made.
        out_leaves.push(node_index);
      } else {
        // Process all proceeding nodes and remove all peeks,
        // replacing with shifts.

        let mut iter_index = node_index;

        while !t_pack.get_node(iter_index).is(TST::I_PEEK_ORIGIN) {
          let node = t_pack.get_node_mut(iter_index);
          node.unset_type(TST::O_PEEK);
          node.set_type(TST::I_SHIFT);
          iter_index = node.parent;
        }

        // Proceed to find the lowest transition action (the original
        // non-term origin) for the current state. For nodes
        // that have more than one item this will not apply.

        let mut items = t_pack.get_node_mut(node_index).items.clone();

        let mut new_items = items
          .into_iter()
          .map(|mut item| {
            while item.at_start() {
              // get the originating item.
              item = t_pack.get_previous_link(&item);
            }
            item
          })
          .collect::<Vec<_>>();

        let mut node = t_pack.get_node_mut(node_index);

        if node.items.len() == 1 {
          node.items = new_items;
        }

        if node.items.iter().any(|i| i.at_end()) {
          node.unset_type(TST::I_SHIFT);
          process_node(g, t_pack, node_index, node_index, true);
        } else if node.items.iter().any(|i| i.is_nonterm(g)) {
          node.unset_type(TST::I_SHIFT);
          process_node(g, t_pack, node_index, node_index, false);
        } else {
          node.set_type(TST::I_SHIFT);
          process_node(g, t_pack, node_index, node_index, true);
        }
      }
    }
    out_leaves
  } else {
    leaves
  };

  // Handle root out of scope items independently from all other nodes
  // since their action is fail
  let mut root_out_of_scope =
    leaves.drain_filter(|leaf| t_pack.get_goal(*leaf).is(TST::I_GOTO_ROOT)).collect::<Vec<_>>();

  if !root_out_of_scope.is_empty() {
    if !nodes_contain_end_items(&root_out_of_scope, t_pack) {
      let parent = t_pack.get_node(root_out_of_scope[0]).parent;

      let mut default_nod = TGN::new(t_pack, SymbolID::Default, parent, vec![]);

      default_nod.parent = parent;

      root_out_of_scope.push(t_pack.insert_node(default_nod));
    }

    for node_index in root_out_of_scope {
      t_pack.get_node_mut(node_index).set_type(TST::I_FAIL | TST::I_OUT_OF_SCOPE);
      t_pack.leaf_nodes.push(node_index)
    }
  }

  for mut peek_leaf_group in
    hash_group(leaves, |_, leaf| t_pack.get_node(leaf).goal).iter().cloned()
  {
    let primary_peek_parent_index = peek_leaf_group[0];
    let prime_node = t_pack.get_node(primary_peek_parent_index).to_owned();
    let goal_index = prime_node.goal;
    let goal_node = t_pack.get_node(goal_index).clone();

    if goal_node.is(TST::I_GOTO_END) {
      if !nodes_contain_end_items(&peek_leaf_group, t_pack) {
        let parent = t_pack.get_node(peek_leaf_group[0]).parent;
        let mut default_nod = TGN::new(t_pack, SymbolID::Default, parent, vec![]);
        default_nod.parent = parent;
        peek_leaf_group.push(t_pack.insert_node(default_nod));
      }
    }
    // Use the goal node as a proxy to generate child nodes that
    // are then linked to the current peek leaf nodes.

    let primary_parent = peek_leaf_group[0];
    let proxy_parents = peek_leaf_group[1..].to_owned();
    let have_proxy_parents = !proxy_parents.is_empty();

    for child_index in process_node(g, t_pack, goal_index, primary_parent, false) {
      let mut child_node = t_pack.get_node_mut(child_index);

      child_node.parent = primary_peek_parent_index;

      if have_proxy_parents {
        child_node.proxy_parents.append(&mut proxy_parents.to_owned());
      }

      if child_node.prod_sym.is_some() {
        child_node.terminal_symbol = prime_node.terminal_symbol
      }

      resolved_leaves.push(child_index);
    }

    // Note: Remember all goal nodes are DROPPED at the
    // end of the peek resolution process
  }

  resolved_leaves
}

fn nodes_contain_end_items(nodes: &[usize], t_pack: &mut TPack) -> bool {
  nodes.iter().any(|n| t_pack.get_node(*n).items[0].is_end())
}

fn get_continue_nodes(
  g: &GrammarStore,
  t_pack: &mut TPack,
  end_item: Item,
  parent_index: usize,
  peek_depth: u32,
  goal: usize,
) -> (Vec<usize>, Vec<usize>) {
  let mut term_nodes = vec![];
  let mut final_nodes = vec![];
  let mut need_to_prune = false;

  let (mut scan_items, mut final_items) = scan_items(g, t_pack, &end_item);

  if end_item.is_out_of_scope() {
    final_items = vec![];
  }

  let scan_items = if end_item.is_out_of_scope() && peek_depth < 1 {
    // Remove item pairs that are expected to show up
    // within the "in-scope" nodes.
    create_term_item_closure(g, t_pack, &scan_items)
      .into_iter()
      .filter(|(_, item)| {
        let item = item.to_zero_state();
        let bool_val = !(t_pack.starts.contains(&item)
          || (item.is_nonterm(g)
            && (t_pack.root_prod_ids.contains(&item.get_prod_id(g))
              || t_pack.root_prod_ids.contains(&item.get_production_id_at_sym(g)))));
        bool_val
      })
      .collect::<Vec<_>>()
  } else {
    create_term_item_closure(g, t_pack, &scan_items)
  };

  for mut term_node in create_term_nodes_from_items(&scan_items, g, t_pack, parent_index) {
    term_node.goal = goal;

    term_node.peek_shifts = peek_depth as i32;

    term_nodes.push(t_pack.insert_node(term_node));
  }

  for mut final_node in create_term_nodes_from_items(&final_items, g, t_pack, parent_index) {
    final_node.goal = goal;

    final_node.set_type(TST::I_END | TST::O_ASSERT);

    final_node.peek_shifts = peek_depth as i32;

    let node_index = t_pack.insert_node(final_node);

    final_nodes.push(node_index);
  }

  (term_nodes, final_nodes)
}

fn group_is_repeated(peek_group: &[usize], t_pack: &mut TPack) -> bool {
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
  g: &GrammarStore,
  t_pack: &mut TPack,
  peek_group: &Vec<usize>,
  leaves: &mut Vec<usize>,
) -> bool {
  let goals = get_goals(peek_group, t_pack)
    .into_iter()
    .map(|i| (i, &t_pack.get_node(i).items))
    .collect::<Vec<_>>();

  if goals.iter().any(|(i, _)| t_pack.get_node(*i).is(TST::I_OUT_OF_SCOPE)) {
    return false;
  }

  if goals.len() == 2
    && (goals[0].1.len() == 1 && goals[1].1.len() == 1 && (t_pack.mode == TransitionMode::GoTo)
      || (goals[0].1[0].get_prod_id(g) == goals[1].1[0].get_prod_id(g)))
  {
    let shift = goals.iter().filter(|(_, i)| !i[0].is_end()).collect::<Vec<_>>();

    let mut reduce = goals.iter().filter(|(_, i)| i[0].is_end());

    if !shift.is_empty() && reduce.next().is_some() {
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

  false
}

fn groups_are_aliased(peek_group: &Vec<usize>, t_pack: &mut TPack) -> bool {
  return false;

  hash_group(peek_group.to_owned(), |_, n| {
    t_pack.get_node(n).items.clone().iter().map(|i| i.to_zero_state()).collect::<Vec<_>>().sort()
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

fn merge_occluding_groups(g: &GrammarStore, t_pack: &mut TPack, groups: &mut [Vec<usize>]) {
  // Clone the from_group store so we are able
  // to merge its members into to_groups without
  // going fowl of the borrow checker.
  if (!t_pack.is_scanner) {
    return;
  }

  for i in 0..groups.len() {
    for j in 0..groups.len() {
      if i == j {
        continue;
      }

      let from_node = t_pack.get_node(groups[i][0]);
      let to_node = t_pack.get_node(groups[j][0]);

      let from_origin = from_node.items[0].get_origin();
      let to_origin = to_node.items[0].get_origin();

      // Scanner items that originate from the same symbol do not require occlusion
      // checking.
      if matches!(from_origin, OriginData::Symbol(..)) && from_origin == to_origin {
        continue;
      }

      let from_sym = from_node.terminal_symbol;
      let to_sym = to_node.terminal_symbol;

      if symbols_occlude(&to_sym, &from_sym, g)
        && ((!from_node.is_out_of_scope()) || (from_sym.is_defined() || to_sym.is_defined()))
      {
        let mut clone = groups[i].clone();
        groups[j].append(&mut clone);
        t_pack.get_node_mut(groups[j][0]).set_type(TST::I_MERGE_ORIGIN);
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

fn symbols_occlude(symA: &SymbolID, symB: &SymbolID, g: &GrammarStore) -> bool {
  match symA {
    SymbolID::DefinedSymbol(..) | SymbolID::ExclusiveDefinedSymbol(..) => {
      match g.symbol_strings.get(symA).map(|s| s.as_str()) {
        Some("\n") => match symB {
          SymbolID::GenericNewLine => true,
          _ => false,
        },
        Some("\t") => match symB {
          SymbolID::GenericHorizontalTab => true,
          _ => false,
        },
        Some(" ") => match symB {
          SymbolID::GenericSpace => true,
          _ => false,
        },
        Some(_) => match symB {
          SymbolID::GenericSymbol => g.symbols.get(symA).unwrap().cp_len == 1,
          _ => false,
        },
        _ => false,
      }
    }
    SymbolID::DefinedIdentifier(_) | SymbolID::ExclusiveDefinedIdentifier(_) => {
      match g.symbol_strings.get(symA).map(|s| s.as_str()) {
        Some("_") | Some("-") => match symB {
          SymbolID::GenericSymbol => true,
          _ => false,
        },
        Some(_) => match symB {
          SymbolID::GenericIdentifier => g.symbols.get(symA).unwrap().cp_len == 1,
          _ => false,
        },
        _ => false,
      }
    }
    SymbolID::DefinedNumeric(_) | SymbolID::ExclusiveDefinedNumeric(_) => match symB {
      SymbolID::GenericNumber => g.symbols.get(symA).unwrap().cp_len == 1,
      _ => false,
    },
    _ => false,
  }
}

fn get_goals(e_nodes: &[usize], t_pack: &TPack) -> Vec<usize> {
  BTreeSet::<usize>::from_iter(e_nodes.iter().map(|n| t_pack.get_node(*n).goal))
    .into_iter()
    .collect::<Vec<_>>()
}

fn handle_unresolved_scanner_nodes(
  g: &GrammarStore,
  t_pack: &mut TPack,
  nodes: Vec<usize>,
  leaves: &mut Vec<usize>,
) {
  let mut defined = nodes;
  let mut generic = defined
    .drain_filter(|n| match t_pack.get_node(*n).items[0].get_origin() {
      OriginData::Symbol(sym) => !sym.is_defined(),
      _ => false,
    })
    .collect::<Vec<_>>();
  let productions = generic
    .drain_filter(|n| match t_pack.get_node(*n).items[0].get_origin() {
      OriginData::Symbol(sym) => !sym.is_production(),
      OriginData::Production(_) => {
        unreachable!("Origin Data should be a symbol!");
      }
      OriginData::UNDEFINED => {
        unreachable!("Origin Symbols Data not defined!");
      }
      _ => false,
    })
    .collect::<Vec<_>>();

  match (defined.len(), productions.len()) {
    (1, _) => {
      leaves.push(defined[0]);
      for node_id in generic.iter().chain(productions.iter()) {
        t_pack.drop_node(&node_id);
      }
    }
    (0, 1) => {
      leaves.push(productions[0]);
      for node_id in generic.iter() {
        t_pack.drop_node(&node_id);
      }
    }
    (a, b) if a + b > 1 => {
      /// HCTKError::Transition_Invalid_Defined{root_symbols, chains}
      panic!(
        "Invalid combination of defined  \n {}!",
        defined
          .into_iter()
          .chain(productions.into_iter())
          .map(|n| t_pack.get_node(n).debug_string(g))
          .collect::<Vec<_>>()
          .join("\n")
      );
    }
    _ => {
      /// InvalidGenerics
      /// HCTKError::Transition_Invalid_Generics{root_symbols, chains}
      panic!(
        "Invalid combination of generics while creating transition states for [{:#?}]\n {}!",
        t_pack
          .root_prod_ids
          .iter()
          .map(|p_id| g.get_production_plain_name(p_id))
          .collect::<Vec<_>>(),
        generic
          .into_iter()
          .map(|n| t_pack.get_node(n).debug_string(g))
          .collect::<Vec<_>>()
          .join("\n")
      );
    }
  }
}

fn handle_unresolved_nodes(
  g: &GrammarStore,
  t_pack: &mut TPack,
  peek_group: Vec<usize>,
  leaves: &mut Vec<usize>,
) {
  if t_pack.is_scanner {
    handle_unresolved_scanner_nodes(g, t_pack, peek_group, leaves)
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
      let node = t_pack.get_node(*node_index);
      items.push(node.items[0]);
      parent = t_pack.drop_node(node_index);
    }

    let goal_hash =
      hash_id_value_u64(goals.iter().map(|i| t_pack.get_node(*i).first_item()).collect::<Vec<_>>());

    match t_pack.events.get(&goal_hash).to_owned() {
      Some(fork_node_index) => {
        t_pack.get_node_mut(*fork_node_index).proxy_parents.push(parent);
      }

      None => {
        warn_of_ambiguous_productions(g, t_pack, &goals);

        let mut fork_node = TGN::new(t_pack, SymbolID::Default, parent, items);

        fork_node.set_type(TST::I_FORK);

        fork_node.parent = parent;

        let fork_node_index = t_pack.insert_node(fork_node);

        t_pack.events.insert(goal_hash, fork_node_index);

        for goal_index in goals {
          process_node(g, t_pack, goal_index, fork_node_index, false);
        }
      }
    }
  }
}

fn warn_of_ambiguous_productions(g: &GrammarStore, t_pack: &mut TPack, goals: &Vec<usize>) {
  // Look for a common production in each goal. If such production(s) exist,
  // issue warning(s) about production occlusion.
  let mut closures = goals
    .iter()
    .map(|i| {
      get_closure_cached(&t_pack.get_node(*i).first_item(), g)
        .iter()
        .map(|i| (i.get_symbol(g), i))
        .collect::<Vec<_>>()
    })
    .collect::<Vec<_>>();
  let smallest = closures
    .iter()
    .fold(&closures[0], |f, i| match i.len() < f.len() {
      true => i,
      false => f,
    })
    .clone();
  // Get a set of symbols that are present in all closures.
  let common_symbols = smallest
    .iter()
    .filter_map(|(sym, i)| match closures.iter().all(|c| c.iter().any(|(s, _)| s == sym)) {
      true => Some(sym),
      false => None,
    })
    .collect::<BTreeSet<_>>();
  // For each closure, remove all items that do not have a symbols that matches one in common_symbols,
  // or that is of a production whose id is in common_symbols
  closures.iter_mut().for_each(|c| {
    c.drain_filter(|(s, i)| {
      (!common_symbols.contains(s) || common_symbols.contains(&i.get_prod_as_sym_id(g)))
    });
  });
  // At this point we should have isolated the items responsible for the ambiguous parse, provided
  // we have set of non-empty closures. We can now display an appropriate message to the
  // user regarding the nature of the ambiguous parse producing bodies.
  if closures.iter().all(|c| !c.is_empty()) {
    t_pack.errors.push(crate::types::HCError::transition_err_ambiguous_production {
      source_production: g.get_production(t_pack.root_prod_ids.first().unwrap()).unwrap().clone(),
      body_refs:         closures
        .iter()
        .flat_map(|c| {
          c.iter().map(|(_, i)| {
            let prod = i.get_body_ref(g).unwrap();
            (prod.grammar_ref.clone(), prod.tok.clone())
          })
        })
        .collect(),
    });
  }
}

/// Set the transition type of a peeking based on whether
/// it is first node in the peek path or not. If it is the first
/// node, we do regular ASSERT action on the terminal symbol.
/// Otherwise we use a PEEK action.

fn set_transition_type(t_pack: &mut TPack, node_index: usize, depth: u32) {
  t_pack.get_node_mut(node_index).set_type(match depth {
    0 => TST::O_ASSERT,
    _ => TST::O_PEEK,
  })
}

type TermAndEndItemGroups = (Vec<(Item, Item)>, Vec<(Item, Item)>);

/// Retrieve terminal items derived from a
/// completed item.
///
/// ### Returns
/// A vector of Item tuples, where the first item is the
/// previous link item for closure resolution, and the second item is
/// a non-end item  produced from the reduction of the
/// `root_end_item`.
fn scan_items(g: &GrammarStore, t_pack: &mut TPack, root_end_item: &Item) -> TermAndEndItemGroups {
  let mut seen = BTreeSet::<(Item, Item)>::new();
  let mut out = BTreeSet::<(Item, Item)>::new();
  let mut fin_items = BTreeSet::<(Item, Item)>::new();

  static empty_vec: Vec<Item> = Vec::new();
  // Starting at the top we grab the closure to the nearest
  // non-term link.

  // Stores the end item [1] and its immediate closure item [0]
  let mut end_items: VecDeque<(Item, Item)> =
    VecDeque::from_iter(vec![(t_pack.get_closure_link(root_end_item), *root_end_item)]);
  while let Some((closure_link, end_item)) = end_items.pop_front() {
    if seen.insert((closure_link, end_item)) {
      let prod = end_item.get_prod_id(g);
      // Grab all productions from the closure that match the end item's
      // production.
      match {
        if closure_link.is_null() {
          if end_item.is_out_of_scope() {
            t_pack.out_of_scope_closure.as_deref().unwrap_or(&empty_vec).iter()
          } else {
            t_pack
              .scoped_closures
              .get(closure_link.get_state().get_closure_index())
              .unwrap_or(&empty_vec)
              .iter()
          }
        } else if closure_link.is_out_of_scope() {
          if let Some(closure) = &t_pack.goto_scoped_closure {
            closure.iter()
          } else {
            empty_vec.iter()
          }
        } else {
          get_closure_cached(&closure_link, g).iter()
        }
        .filter(|i| i.get_production_id_at_sym(g) == prod)
        .cloned()
        .map(|i| i.to_origin(closure_link.get_origin()))
        .collect::<Vec<_>>()
      } {
        empty if empty.is_empty() => {
          if closure_link.is_null() {
            // t_pack.set_closure_link(end_item, nonterm_closure_link);
            fin_items.insert((closure_link, end_item));
          } else {
            end_items.push_back((t_pack.get_closure_link(&closure_link), end_item));
          }
        }
        prod_items => {
          for p_item in prod_items {
            let inc_item = p_item.increment().unwrap().to_state(end_item.get_state());

            if inc_item.is_end() {
              end_items.push_back((closure_link, inc_item));
            } else {
              t_pack.set_closure_link(inc_item, closure_link);
              out.insert((closure_link, inc_item));
            }
          }
        }
      }
    }
  }
  (out.into_iter().collect::<Vec<_>>(), fin_items.into_iter().collect::<Vec<_>>())
}

fn process_terminal_node(
  g: &GrammarStore,
  t_pack: &mut TPack,
  term_items: &Vec<Item>,
  parent_index: usize,
) -> Vec<usize> {
  let sym = term_items[0].get_symbol(g);

  let new_node = TGN::new(t_pack, sym, parent_index, term_items.to_vec());

  let node_index = t_pack.insert_node(new_node);

  t_pack.get_node_mut(node_index).set_type(TST::I_SHIFT | TST::O_TERMINAL);

  t_pack.queue_node(node_index);

  vec![node_index]
}

fn create_production_call(
  g: &GrammarStore,
  t_pack: &mut TPack,
  prod_id: ProductionId,
  nonterm_items: Vec<Item>,
  par_id: usize,
) -> Vec<usize> {
  let mut node =
    TGN::new(t_pack, SymbolID::Production(prod_id, GrammarId(0)), par_id, nonterm_items);

  node.prod_sym = Some(SymbolID::Production(prod_id, GrammarId(0)));

  node.trans_type |= TST::O_PRODUCTION;

  let node_index = t_pack.insert_node(node);

  t_pack.queue_node(node_index);

  vec![node_index]
}

fn process_end_item(
  g: &GrammarStore,
  t_pack: &mut TPack,
  end_item: Item,
  parent_index: usize,
) -> Vec<usize> {
  if t_pack.is_scanner && !t_pack.starts.contains(&end_item.to_start().to_zero_state()) {
    // We need to be in the initial closure before we can allow
    // a scanner run to exit successfully. Thus, the production of the end state
    // is used to select the next set of items to be scanned, continuing the scan process
    // until we arrive at an end_item that belongs to the root closu.

    let (term_items, end_items) = scan_items(g, t_pack, &end_item);

    // Filter out items automatically handled by goto
    let non_goto_items = term_items; //.iter().filter(|(_, i)| i.get_offset() > 1).cloned().collect::<Vec<_>>();

    if !non_goto_items.is_empty() {
      let items = non_goto_items
        .into_iter()
        .map(|(_, i)| i)
        .chain(end_items.into_iter().map(|(_, i)| i))
        .collect::<Vec<_>>();

      let node = TGN::new(t_pack, SymbolID::EndOfFile, parent_index, items);

      let node_index = t_pack.insert_node(node);

      let results = process_node(g, t_pack, node_index, parent_index, false);

      t_pack.drop_node(&node_index);

      return results;
    }
  }

  t_pack.gotos.insert(end_item);

  let end_node = TGN::new(t_pack, SymbolID::EndOfFile, parent_index, vec![end_item]);

  let node_index = t_pack.insert_node(end_node);

  t_pack.get_node_mut(node_index).set_type(TST::I_END | TST::O_ASSERT);
  t_pack.leaf_nodes.push(node_index);

  vec![node_index]
}

/// Constructs a Vector of Vectors, each of which contains a set items from the
/// original vector that have been grouped by the hash of a common distinguisher.
pub fn hash_group<T: Copy + Sized, R: Hash + Sized + Ord + Eq, Function: Fn(usize, T) -> R>(
  vector: Vec<T>,
  hash_yielder: Function,
) -> Vec<Vec<T>> {
  let mut hash_groups = BTreeMap::<R, Vec<T>>::new();

  for (i, val) in vector.iter().enumerate() {
    match hash_groups.entry(hash_yielder(i, *val)) {
      Entry::Vacant(e) => {
        e.insert(vec![*val]);
      }
      Entry::Occupied(mut e) => e.get_mut().push(*val),
    }
  }

  hash_groups.into_values().collect()
}
