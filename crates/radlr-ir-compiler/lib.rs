mod state;

use std::{
  collections::{BTreeMap, VecDeque},
  sync::Arc,
};

use radlr_compiler_frontend::{
  array_vec::ArrayVec,
  extract_grammar_components,
  item::{Item, ItemIndex, ItemType},
  merge_grammars,
  parse_grammar_source,
  parser_db::ParserDatabase,
  symbol::{PrecedentDBTerm, PrecedentSymbol, SymbolId},
  DBNonTermKey,
  GrammarCompilerError,
  GraphType,
};
use radlr_core_common::{utils::create_u64_hash, OrderedMap};
use state::{Origin, State, StateItem, TransitionPair};

struct StateAllocator {}

impl StateAllocator {
  pub fn allocate(&mut self, state: State) -> Result<*const State, String> {
    Ok(Box::into_raw(Box::new(state)))
  }
}

fn closure_iter_align(state_item: StateItem, db: &'_ ParserDatabase) -> impl Iterator<Item = StateItem> + '_ {
  db.get_closure(&state_item.item).iter().map(move |i| StateItem { item: *i, ..state_item })
}

pub fn closure_iter_align_with_lane_split<'db>(
  item: StateItem,
  other: StateItem,
  db: &'_ ParserDatabase,
) -> impl Iterator<Item = StateItem> + '_ {
  let from = item.as_ref().index;
  db.get_closure(&item.item).iter().map(move |i| StateItem { item: *i, ..item }).map(move |i| i.align(&other).as_from_index(from))
}

/// Builds parser IR blocks from a parser database.
pub fn build_ir_blocks(db: Arc<ParserDatabase>) -> Result<(), GrammarCompilerError> {
  let mut allocator = StateAllocator {};

  let mut oos_entry_states: BTreeMap<DBNonTermKey, *const State> = BTreeMap::new();
  let mut oos_states: BTreeMap<StateItem, *const State> = BTreeMap::new();

  for nterm in &db.nonterm_ids {
    let db_key = nterm.db_key;

    let kernel = db
      .nonterm_follow_items(db_key)
      .map(|i| StateItem {
        item:         i,
        is_oos:       true,
        is_oos_entry: true,
        origin:       Origin::__OOS_ROOT__,
        state:        std::ptr::null(),
      })
      .filter_map(|i| i.increment());

    let state = allocator
      .allocate(State {
        hash_id: 0,
        is_root: true,
        is_leaf: false,
        is_goto: false,
        sym:     Default::default(),
        parent:  std::ptr::null(),
        kernel:  kernel.collect(),
        invalid: Default::default(),
      })
      .unwrap();

    oos_entry_states.insert(db_key, state);
  }

  let mut state_list: VecDeque<StateItem> =
    VecDeque::from_iter(oos_entry_states.values().flat_map(|s| unsafe { s.as_ref().unwrap().kernel() }));

  while let Some(item) = state_list.pop_front() {
    if let Some(mut item) = item.increment() {
      item.is_oos_entry = false;
      let parent = item.state();

      let state = allocator
        .allocate(State {
          hash_id: 0,
          is_root: false,
          is_leaf: false,
          is_goto: false,
          sym:     Default::default(),
          parent:  parent,
          kernel:  ArrayVec::from_iter(vec![item]),
          invalid: Default::default(),
        })
        .unwrap();

      state_list.extend(unsafe { state.as_ref().unwrap().kernel() });

      oos_states.insert(item, state);
    }
  }

  // Build an IR state
  // Build states for each parser non-terminal

  let pending_states = VecDeque::<*const State>::new();
  let graph_type = GraphType::Parser;
  let ALLOW_LOOKAHEAD_SCANNERS = false;
  let is_scanner = false;

  for non_term in &db.nonterm_ids {
    if !non_term.is_terminal {
      let id = non_term.db_key;

      let root_kernel_items = db.nonterm_rules[id.0 as usize].iter().map(|r| {
        let item = Item::from((*r, db.rules.as_slice()));
        StateItem {
          item,
          state: std::ptr::null(),
          origin: Origin::NonTermGoal(id),
          is_oos: false,
          is_oos_entry: false,
        }
      });

      let mut kernel = ArrayVec::from_iter(root_kernel_items);
      kernel.sort();

      let state = allocator
        .allocate(State {
          hash_id: 0,
          is_root: true,
          is_leaf: false,
          is_goto: false,
          sym:     PrecedentSymbol::default(),
          parent:  std::ptr::null(),
          kernel:  kernel,
          invalid: Default::default(),
        })
        .unwrap();
      {
        let state_ref = unsafe { state.as_ref().unwrap() };
        let kernel_items = state_ref.kernel.as_ref();

        let mut items_to_process = ArrayVec::<32, StateItem>::new();
        let mut oos_scan_completed_tokens = ArrayVec::<32, PrecedentDBTerm>::new();
        let mut oos_scan_incompletes = ArrayVec::<32, PrecedentDBTerm>::new();

        for item in kernel_items {
          match item.origin {
            Origin::__OOS_SCANNER_ROOT__(token) if ALLOW_LOOKAHEAD_SCANNERS => {
              if item.as_ref().is_complete() {
                let mut follow = ArrayVec::<512, _>::new();
                let mut _default = ArrayVec::<512, _>::new();

                get_follow_internal(
                  &db,
                  state_ref,
                  *item,
                  FollowType::AllItems,
                  &mut follow,
                  &mut _default,
                  graph_type,
                  &mut oos_entry_states,
                  &mut oos_states,
                );

                if follow.is_empty() {
                  // No follow items indicates a completed OOS token.
                  oos_scan_completed_tokens.insert_ordered(token);
                  items_to_process.push(*item);
                } else {
                  // Continue parsing with the OOS items scanned
                  oos_scan_incompletes.insert_ordered(token);
                  items_to_process.extend(follow.iter().cloned());
                }
              } else {
                if !item.as_ref().is_initial() {
                  oos_scan_incompletes.insert_ordered(token);
                }

                items_to_process.push(*item);
              }
            }
            _ => items_to_process.push(*item),
          }

          let mut max_completed_precedence: u16 = 0;

          let iter = items_to_process
            .iter()
            .filter(|i| match i.origin {
              Origin::TerminalGoal(key, prec) => !oos_scan_completed_tokens.contains(&(key, prec, false).into()),
              _ => true,
            })
            .flat_map(|k_i| {
              let basis = k_i.to_origin_state(state);
              closure_iter_align_with_lane_split(*k_i, basis, &db)
                .filter(|i| match i.item.get_type(db.rules()) {
                  ItemType::Completed(_) | ItemType::Terminal(_) => true,
                  ItemType::TokenNonTerminal(..) if !is_scanner => true,
                  _ => false,
                })
                .map(|t_item| -> TransitionPair {
                  let mut pair: TransitionPair = (*k_i, t_item, graph_type, db.rules()).into();

                  let origin = pair.kernel.origin;

                  if let Origin::TerminalGoal(tok, prec) = origin {
                    let prec: PrecedentDBTerm = (tok, prec, false).into();
                    if oos_scan_incompletes.contains(&prec) {
                      pair.allow_assign = false;
                    }
                  }

                  pair
                })
                .map(|pair| {
                  if pair.sym.is_default() {
                    max_completed_precedence = (max_completed_precedence).max(pair.prec);
                  }
                  pair
                })
            });

          let mut pending_work = ArrayVec::<64, _>::from_iter(iter);

          pending_work.sort();

          let mut seen: ArrayVec<64, SymbolId> = ArrayVec::<64, _>::new();
          let mut invalid: ArrayVec<64, SymbolId> = ArrayVec::<64, _>::new();

          for pair in pending_work.as_slice() {
            let sym = pair.sym;

            if !seen.push_unique(sym) {
              continue;
            }

            // Todo, remove symbol groups that don't meet the precedence criteria before
            // processing or merging any members

            if sym.is_default() {
              let mut default_items = ArrayVec::<64, _>::new();
              // Handle completed items.
              for pair in pending_work.as_slice() {
                if pair.sym == sym {
                  default_items.push(pair.kernel);
                }
              }
            } else {
              fn matches(sym_a: SymbolId, sym_b: SymbolId, graph_type: GraphType) -> bool {
                sym_a == sym_b
              }

              let mut new_work = ArrayVec::<64, _>::new();

              for pair in pending_work.as_slice() {
                if matches(sym, pair.sym, graph_type) {
                  new_work.push(*pair);
                }
              }
            }
          }

          // Perform merging of indices.
        }

        // ## TODO
        // Handle fork

        // ## TODO
        // Handle CST shift

        // Handle completed items
        for item in items_to_process.as_slice().iter() {
          dbg!(item);
        }
      }

      // Each state begins with a kernel, the set of items that distinguish one
      // state from another. From the kernel a closure is generated. For
      // each terminal in the closure a transition edge is created from
      // the initial state to the state containing the kernel of items that
      // transitioned on the given terminal.
      //
      // Transitions can also include reductions, in which an item that has been
      // completed is then reduced to the non-terminal symbol the item
      // generates. In this case, all items in the closure that transition
      // on the same non-terminal yield a new state that contains a kernel
      // of the subsequent items created from shifting over the
      // non-terminal
      //
      // Conflicts occur when either a state contains a mixture of complete and
      // incomplete items, in which case a k=0 shift-reduce conflict is
      // encountered. Shift-shift conflicts occur when
      //``
    }
  }

  // Build states for each scanner as they are created.

  Err(GrammarCompilerError::Todo("build_ir_blocks"))
}

fn handle_completed_items(
  completed: &[Item],
  pred: &SharedGraphNode,
  config: &ParserConfig,
  groups: &mut GroupedFirsts,
  ____is_scan____: bool,
) {
  if let Some(completed) = groups.remove(&SymbolId::Default) {
    let CompletedItemArtifacts { lookahead_pairs, .. } =
      get_completed_item_artifacts(gb, pred, completed.1.iter().map(|i| &i.kernel))?;

    if !lookahead_pairs.is_empty() {
      // Create reduce states for follow items that have not already been covered.
      let mut completed_groups: OrderedMap<SymbolId, Vec<TransitionPair>> =
        hash_group_btree_iter(lookahead_pairs.iter(), |_, fp| match fp.next.get_type(gb.db()) {
          //ItemType::Completed(_) => {
          //  unreachable!("Should be handled outside this path")
          //}
          ItemType::TokenNonTerminal(_, sym) if !____is_scan____ => sym,
          ItemType::Terminal(sym) => sym,
          _ => SymbolId::Undefined,
        });

      completed_groups.remove(&SymbolId::Undefined);

      for (sym, follow_pairs) in completed_groups {
        handle_completed_groups(gb, pred, config, groups, sym, follow_pairs)?;
      }
    }

    // If there is a single rule that is being reduced then we can create a default
    // state for tha rule Otherwise lookaheads are used to disambiguate the
    // completed items, and items that have no lookahead need to be
    // disambiguated dynamically.

    // TODO(anthony) - create the correct filter to identify the number of rules
    // that are being reduced (compare item indices.)
    let default: Lookaheads =
      if completed.1.iter().items_are_the_same_rule() || completed.1.iter().all(|i| i.kernel.origin.is_scanner_oos()) {
        completed.1.clone()
      } else {
        lookahead_pairs.iter().filter(|i| i.is_eoi_complete()).cloned().collect()
      };

    if default.len() > 0 {
      handle_completed_groups(gb, pred, config, groups, SymbolId::Default, default)?;
    } else {
      #[cfg(debug_assertions)]
      debug_assert!(
        !lookahead_pairs.is_empty(),
        "No default reduce! {pred:?} {}",
        completed.1.iter().map(|c| c._debug_string_(gb.db())).collect::<Vec<_>>().join("\n")
      )
    }
  }
}

#[inline]
pub(crate) fn get_completed_item_artifacts<'a, 'follow, T: ItemRefContainerIter<'a>>(
  gb: &mut ConcurrentGraphBuilder,
  pred: &GraphNode,
  completed: T,
) -> RadlrResult<CompletedItemArtifacts> {
  let db = &gb.db_rc();
  let mut follow_pairs = OrderedSet::new();
  let mut default_only_items = ItemSet::new();

  fn create_pair(k_i: Item, i: Item, db: &ParserDatabase) -> TransitionPair {
    TransitionPair {
      kernel:       k_i,
      next:         i,
      allow_assign: true,
      prec:         i.token_precedence(db),
      sym:          i.sym_id(db),
    }
  }

  for k_i in completed {
    if k_i.origin.is_scanner_oos() {
      // Do not create any states for OOS scanner items
      continue;
    }

    let mut f = ArrayVec::<512, _>::new();
    let mut d = ArrayVec::<512, _>::new();

    get_follow_internal(gb, pred, *k_i, FollowType::AllItems, &mut f, &mut d);

    if f.is_empty() {
      debug_assert!(!d.is_empty());
      default_only_items.insert(*k_i);
      follow_pairs.extend([create_pair(*k_i, *k_i, db)]);
    } else {
      for k_follow in f.iter() {
        if let Origin::__OOS_CLOSURE__ = k_follow.origin {
          let state = gb.get_oos_closure_state(*k_follow);
          follow_pairs.extend(state.kernel_items().iter().filter(|i| !i.is_complete()).map(|i| create_pair(*k_i, *i, db)));
        } else {
          follow_pairs.extend(k_follow.closure_iter_align(k_follow.to_origin(k_i.origin), db).map(|i| create_pair(*k_i, i, db)))
        }
      }
    }
  }

  RadlrResult::Ok(CompletedItemArtifacts { lookahead_pairs: follow_pairs, default_only: default_only_items })
}

pub(crate) fn handle_completed_groups(
  gb: &mut ConcurrentGraphBuilder,
  node: &SharedGraphNode,
  config: &ParserConfig,
  groups: &mut GroupedFirsts,
  sym: SymbolId,
  follow_pairs: Lookaheads,
) -> RadlrResult<()> {
  let ____is_scan____ = node.is_scanner();
  let prec_sym: PrecedentSymbol = (sym, follow_pairs.iter().max_precedence()).into();

  match node.state_type() {
    StateType::Peek(level) => handle_peek_complete_groups(gb, node, config, groups, prec_sym, follow_pairs, level),
    _REGULAR_ => handle_regular_complete_groups(gb, node, config, groups, prec_sym, follow_pairs),
  }
}

pub(crate) fn handle_regular_complete_groups(
  gb: &mut ConcurrentGraphBuilder,
  pred: &SharedGraphNode,
  config: &ParserConfig,
  shift_groups: &mut GroupedFirsts,
  prec_sym: PrecedentSymbol,
  mut lookahead_pairs: Lookaheads,
) -> RadlrResult<()> {
  let ____is_scan____ = pred.is_scanner();
  let ____allow_peek____ = config.ALLOW_PEEKING;
  let mut cmpl = lookahead_pairs.iter().to_next().to_vec();
  let sym = prec_sym.sym();
  let mut classification = ParserClassification { max_k: 1, ..Default::default() };
  // Non-Peeking States
  match (lookahead_pairs.len(), shift_groups.remove(&prec_sym.sym())) {
    (1, None) => {
      handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
    }
    (2.., None) => {
      if ____is_scan____ {
        // We may be able to continue parsing using follow items, after we
        // determine whether we have symbol ambiguities.
        resolve_conflicting_tokens(gb, pred, config, sym, lookahead_pairs.iter())?;
      } else if prec_sym.sym() == SymbolId::Default {
        if lookahead_pairs.iter().to_kernel().items_are_the_same_rule() {
          handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
        } else {
          create_peek(gb, pred, config, prec_sym, [].iter(), Some(lookahead_pairs.iter()))?.include_with_goto_state().commit(gb);
        }
      } else if lookahead_pairs.iter().to_kernel().indices().len() == 1 {
        // The same non-terminal is generated from this completed item, regardless
        // of the origins. This is a valid outcome.
        handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
      } else if lookahead_pairs.iter().all(|p| p.is_out_of_scope()) {
        let item: Item = *o_to_r(cmpl.first(), "Item list is empty")?;
        handle_completed_item(gb, pred, config, vec![(item, item, pred.graph_type(), gb.db()).into()], prec_sym)?;
      } else {
        match resolve_reduce_reduce_conflict(gb, pred, config, prec_sym, lookahead_pairs)? {
          ReduceReduceConflictResolution::Nothing => {}
          ReduceReduceConflictResolution::Fork(lookahead_pairs) => {
            create_fork(gb, pred, config, prec_sym, lookahead_pairs.iter().map(|i| i.kernel))?
              .to_classification(classification | ParserClassification { forks_present: true, ..Default::default() })
              .include_with_goto_state()
              .commit(gb);
          }
          ReduceReduceConflictResolution::Reduce(item) => {
            todo!("Reduce result from a reduce-reduce  resolution")
          }
          ReduceReduceConflictResolution::Peek(max_k, follow_pairs) => {
            create_peek(gb, pred, config, prec_sym, [].iter(), Some(follow_pairs.iter()))?.include_with_goto_state().commit(gb);
          }
        }
      }
    }
    (_, Some((prec, mut group))) => {
      if ____is_scan____ {
        let item: Item = cmpl[0];
        lookahead_pairs.extend(group);
        handle_completed_item(gb, pred, config, lookahead_pairs, prec_sym)?;
      } else if group.iter().all(|i| i.is_out_of_scope()) && lookahead_pairs.iter().all(|i| i.is_out_of_scope()) {
        create_out_of_scope_complete_state(gb, pred, lookahead_pairs.iter(), prec_sym);
      } else {
        match resolve_shift_reduce_conflict(gb, pred, config, group.iter(), lookahead_pairs.iter())? {
          ShiftReduceConflictResolution::Shift => {
            shift_groups.insert(sym, (prec, group));
          }
          ShiftReduceConflictResolution::Reduce => {
            handle_completed_groups(gb, pred, config, &mut Default::default(), sym, lookahead_pairs)?;
          }
          ShiftReduceConflictResolution::Peek(max_k) => {
            create_peek(gb, pred, config, prec_sym, group.iter(), Some(lookahead_pairs.iter()))?
              .to_classification(classification)
              .include_with_goto_state()
              .commit(gb);
          }
          ShiftReduceConflictResolution::Fork => {
            create_fork(gb, pred, config, prec_sym, lookahead_pairs.into_iter().chain(group.into_iter()).map(|i| i.kernel))?
              .to_classification(classification | ParserClassification { forks_present: true, ..Default::default() })
              .include_with_goto_state()
              .commit(gb);
          }
        }
      }
    }
    (_len, _collide) => {
      #[cfg(debug_assertions)]
      unimplemented!(
        "\nNot Implemented: len:{_len} collide:{_collide:?} sym:{} \n[ {} ]\n\n{}",
        sym.debug_string(gb.db()),
        cmpl.to_debug_string(gb.db(), "\n"),
        ""
      );
      #[cfg(not(debug_assertions))]
      unimplemented!()
    }
  }
  Ok(())
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum FollowType {
  AllItems,
  FirstReduction,
  ScannerCompleted,
}

/// All states that transition on a particular non-terminal. When used in
/// conjunction with a parse graph, it should represent all items that
/// transition on a on-terminal other than items within the kernel or kernel
/// closure of the given state. Such items are labeled oos (out-of-scope), and
/// are only used to differentiate between transitions that belong within the
/// the parse tree and those that should cause the parse tree to terminate.
const OOS_STATES: () = ();

/// Returns a tuple comprised of:
/// - a vector of all items that follow the given item, provided the given item
///   is in a complete state
/// - a list of a all items that are completed from directly or indirectly
///   transitioning on the nonterminal of the given item.
#[inline]
pub(crate) fn get_follow_internal<const STACK_BUFFER_SIZE_FOLLOW: usize, const STACK_BUFFER_SIZE_COMPLETE: usize>(
  db: &ParserDatabase,
  node: &State,
  item: StateItem,
  caller_type: FollowType,
  follow: &mut ArrayVec<STACK_BUFFER_SIZE_FOLLOW, StateItem>,
  complete: &mut ArrayVec<STACK_BUFFER_SIZE_COMPLETE, StateItem>,
  graph_type: GraphType,
  oos_entry_states: &mut BTreeMap<DBNonTermKey, *const State>,
  oos_states: &mut BTreeMap<StateItem, *const State>,
) {
  if !item.as_ref().is_complete() {
    follow.push(item);
    follow.sort();
    return;
  }

  let ____is_scan____ = graph_type == GraphType::Scanner;
  let mut queue = VecDeque::from_iter(vec![item]);

  let mode = graph_type;

  let root_nterm = item.as_ref().nonterm_index(db.as_ref());
  let mut seen_nonterminal_extents = ArrayVec::<64, _>::new();

  while let Some(c_item) = queue.pop_front() {
    if complete.push_unique(c_item) {
      let nterm: DBNonTermKey = c_item.as_ref().nonterm_index(db.as_ref());

      if caller_type == FollowType::FirstReduction && nterm != root_nterm {
        continue;
      }

      let result = if c_item.is_oos {
        if c_item.is_oos_entry {
          // Create or retrieve a new OOS state for this set of items.
          if seen_nonterminal_extents.push_unique(nterm) {
            let closure_state = unsafe { oos_entry_states.get(&nterm).unwrap().as_ref().unwrap() };
            process_closure(db, closure_state.kernel(), &mut queue, follow)
          } else {
            None
          }
        } else {
          let state = unsafe { oos_states.get(&c_item).unwrap().as_ref().unwrap() };

          let mut closure = state
            .kernel()
            .filter(|i| i.as_ref().nonterm_index_at_sym(mode, db.as_ref()) == Some(nterm))
            .filter_map(|i| i.increment())
            .peekable();

          if closure.peek().is_none() {
            queue.push_back(c_item.to_oos_entry());
            None
          } else {
            process_closure(db, closure, &mut queue, follow)
          }
        }
      } else {
        let origin = c_item.origin;
        let is_scanner_oos = origin.is_scanner_oos();

        let state = c_item.state();

        let closure = state
          .kernel
          .iter()
          .filter(|k_i| c_item.as_ref().is_successor_of(k_i.as_ref()) && (!is_scanner_oos || c_item.origin.is_scanner_oos()))
          .flat_map(|k_i| closure_iter_align(k_i.to_origin(origin).to_origin_state(state), db))
          .filter(|i| i.as_ref().nonterm_index_at_sym(mode, db.as_ref()) == Some(nterm))
          .filter_map(|i| i.increment());

        process_closure(db, closure, &mut queue, follow)
      };

      if let Some(oos_queue) = result {
        for next_item in oos_queue {
          follow.push_unique(next_item);
        }
      } else if !c_item.state().is_root && !c_item.is_oos {
        if let Some(parent_state) = c_item.state().parent() {
          queue.push_back(c_item.to_origin_state(parent_state));
        }
      } else if !____is_scan____ && !c_item.is_oos_entry {
        queue.push_back(c_item.to_oos_entry());
      }
    }
  }

  follow.sort();
  complete.sort();
}

fn process_closure<const STACK_BUFFER_SIZE: usize>(
  db: &ParserDatabase,
  closure: impl Iterator<Item = StateItem>,
  queue: &mut VecDeque<StateItem>,
  follow: &mut ArrayVec<STACK_BUFFER_SIZE, StateItem>,
) -> Option<VecDeque<StateItem>> {
  let mut oos_queue = VecDeque::new();
  let mut closure_yielded_items = false;

  for item in closure {
    closure_yielded_items |= true;
    match item.as_ref().get_type(db.as_ref()) {
      ItemType::Completed(_) => queue.push_back(item),
      _ => {
        if item.is_oos() {
          oos_queue.push_front(item);
        } else {
          follow.push_unique(item);
        };
      }
    }
  }

  closure_yielded_items.then_some(oos_queue)
}

#[test]
fn sanity() {
  println!("Hello World");
}

#[test]
fn regex_expressions() -> Result<(), GrammarCompilerError> {
  let source_data = parse_grammar_source(
    r###"
NAME mecha_grammar

<> B > "a"
  "###,
    Some(std::env::current_dir().unwrap()),
  )?;

  let mut data = extract_grammar_components(&source_data)?;

  let parser_db = merge_grammars(&[&mut data]);

  let shared_db = Arc::new(parser_db?);

  build_ir_blocks(shared_db)?;

  //dbg!(data);

  Ok(())
}
