use crate::{
  grammar::get_production_start_items,
  parser::{hash_group_vec, utils::hash_group_btreemap},
  types::{
    graph::{StateId, *},
    item::ItemType,
    *,
  },
  writer::code_writer::CodeWriter,
  Journal,
  SherpaResult,
};
use std::{
  collections::{btree_map::Entry, BTreeMap, BTreeSet, HashSet, VecDeque},
  hash::Hash,
  vec,
};

use super::graph::get_follow;

pub(crate) fn convert_graph_to_ir(
  j: &mut Journal,
  graph: &Graph,
  entry_name: &str,
) -> SherpaResult<Vec<Box<ParseState>>> {
  let leaf_states = graph.get_leaf_states();
  let mut queue = VecDeque::from_iter(leaf_states.iter().cloned());
  let mut links: BTreeMap<StateId, HashSet<&State>> = BTreeMap::new();
  let mut rebuilds = HashSet::new();
  let mut output = BTreeMap::<StateId, Box<ParseState>>::new();
  let mut seen = BTreeSet::new();
  let empty_hash = HashSet::new();

  while let Some(state) = queue.pop_front() {
    if !seen.insert(state.get_id()) {
      continue;
    }
    let predecesors = state.get_predecessors().clone();
    for predecessor in &predecesors {
      match links.entry(*predecessor) {
        Entry::Occupied(mut e) => {
          e.get_mut().insert(state);
        }
        Entry::Vacant(e) => {
          e.insert(HashSet::from_iter(vec![state]));
        }
      }
      queue.push_back(&graph[*predecessor]);
    }
  }

  queue.extend(leaf_states.iter());
  queue.extend(links.keys().rev().map(|id| &graph[*id]));

  'outer: while let Some(state) = queue.pop_front() {
    if output.contains_key(&state.get_id()) {
      continue;
    }

    let successors = links.get(&state.get_id()).unwrap_or(&empty_hash);

    for successor in successors {
      if successor.get_id() < state.get_id() {
        rebuilds.insert(state);
      } else if successor.get_id() != state.get_id()
        && !output.contains_key(&successor.get_id())
        && !successor.is_leaf_state()
      {
        queue.push_back(state);
        continue 'outer;
      }
    }

    for (id, ir_state) in convert_state_to_ir(j, graph, state, successors, &output, entry_name)? {
      output.insert(id, ir_state);
    }
  }

  // Rebuild nodes that have cycles thet need resolving
  for state in rebuilds.clone() {
    let successors = links.get(&state.get_id()).unwrap_or(&empty_hash);

    for (id, ir_state) in convert_state_to_ir(j, graph, state, successors, &mut output, entry_name)?
    {
      let mut existings_state = output.get_mut(&id)?;
      existings_state.code = ir_state.code;
    }
  }

  debug_assert!(
    !output.is_empty(),
    "This graph did not yield any states! \n{}",
    graph.__debug_string__()
  );

  j.report_mut().ok_or_convert_to_error(output.into_values().collect())
}

fn convert_state_to_ir(
  j: &mut Journal,
  graph: &Graph,
  state: &State,
  successors: &HashSet<&State>,
  resolved_states: &BTreeMap<StateId, Box<ParseState>>,
  entry_name: &str,
) -> SherpaResult<Vec<(StateId, Box<ParseState>)>> {
  let is_scanner = graph.is_scan();
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
  enum S_TYPE {
    GOTO_SUCCESSORS,
    SYMBOL_SUCCESSORS,
  }
  let g = &(j.grammar()?);
  let state_id = state.get_id();

  let successor_groups = hash_group_btreemap(successors.clone(), |_, s| match s.get_type() {
    StateType::GotoPass | StateType::GotoLoop | StateType::KernelGoto => S_TYPE::GOTO_SUCCESSORS,
    _ => S_TYPE::SYMBOL_SUCCESSORS,
  });

  let goto_state = if let Some(successors) = successor_groups.get(&S_TYPE::GOTO_SUCCESSORS) {
    let mut w = CodeWriter::new(vec![]);
    w.increase_indent();
    for (bc_id, (_prod_name, state_name, transition_type)) in successors
      .into_iter()
      .map(|s| {
        if let SymbolID::Production(prod_id, _) = s.get_symbol() {
          let prod = g.get_production(&prod_id).unwrap();
          (
            prod.bytecode_id.unwrap(),
            (&prod.name, get_state_hash_name(s, state, resolved_states), s.get_type()),
          )
        } else {
          panic!("Invalid production type: {}", s.get_symbol().debug_string(g))
        }
      })
      .collect::<BTreeMap<_, _>>()
    {
      match transition_type {
        StateType::KernelGoto => {
          w.write_fmt(format_args!(
            "\n assert PRODUCTION [ {bc_id} ] ( goto state [ {state_name} ] )"
          ))?;
        }
        StateType::GotoLoop => {
          w.write_fmt(format_args!(
            "\n assert PRODUCTION [ {bc_id} ] ( push state [ %%%% ] then goto state [ {state_name} ] )"
          ))?;
        }
        StateType::GotoPass => {
          w.write_fmt(format_args!("\n assert PRODUCTION [ {bc_id} ] ( pass )"))?;
        }
        _ => unreachable!(),
      }
    }

    Some(Box::new(create_ir_state(j, graph, w, state, false)?))
  } else {
    None
  };

  let base_state = if let Some(successors) = successor_groups.get(&S_TYPE::SYMBOL_SUCCESSORS) {
    #[cfg(debug_assertions)]
    {
      for (byte_code, group) in hash_group_btreemap(successors.clone(), |_, s| {
        if s.get_type().is_out_of_scope() {
          OutScopeIndex
        } else {
          get_symbol_info(g, is_scanner, s.get_symbol()).0
        }
      }) {
        if byte_code != OutScopeIndex {
          assert!(
            group.len() == 1,
            "Expected only one state per symbol type. 
            On symbol {} with bytecode id {} the following states are mapped: \n{}\n\n",
            group.iter().next().unwrap().get_symbol().debug_string(g),
            byte_code,
            group.into_iter().map(|s| s.debug_string(g)).collect::<Vec<_>>().join("\n")
          )
        }
      }
    }

    let mut w = CodeWriter::new(vec![]);
    let mut branches = BTreeMap::new();
    let mut is_token_assertion = false;

    w.increase_indent();
    for (bc_id, (_, assert_type, _sym_name, s_type, successor)) in successors
      .into_iter()
      .map(|s| {
        let sym = s.get_symbol();
        let (bc_id, assert_type, sym_name) = get_symbol_info(g, is_scanner, sym);
        (bc_id, (sym, assert_type, sym_name, s.get_type(), s))
      })
      .collect::<BTreeMap<_, _>>()
      .into_iter()
      .rev()
    {
      let mut body_string = vec![];

      if match s_type {
        StateType::Shift => {
          body_string.push(is_scanner.then_some("scan-shift").unwrap_or("shift-token").into());
          true
        }
        StateType::PeekEnd => {
          debug_assert!(!is_scanner, "Peek states should not be present in graph");
          body_string.push("peek-reset".into());
          true
        }
        StateType::Peek => {
          debug_assert!(!is_scanner, "Peek states should not be present in graph");
          body_string.push("peek-token".into());
          true
        }
        StateType::ProductionCompleteOOS => {
          debug_assert!(
            !is_scanner,
            "ProductionCompleteOOS states should only exist in normal parse graphs"
          );
          body_string.push("pop".into());
          false
        }
        StateType::ScannerCompleteOOS => {
          debug_assert!(
            is_scanner,
            "ScannerCompleteOOS states should only exist in scanner parse graphs"
          );
          body_string.push("pass".into());
          false
        }
        StateType::Reduce(rule_id) => {
          body_string.push(create_rule_reduction(g, rule_id));
          false
        }
        StateType::AssignAndFollow(sym_id) | StateType::AssignToken(sym_id) => {
          let bytecode_id = sym_id.bytecode_id(g);
          debug_assert_ne!(
            bytecode_id,
            0,
            "Expected symbol [{}] to be a valid token",
            sym_id.debug_string(g)
          );
          if matches!(s_type, StateType::AssignAndFollow(..)) {
            body_string.push(format!("assign token [ {bytecode_id} ]"));
            true
          } else {
            body_string.push(format!("assign token [ {bytecode_id} ]"));
            false
          }
        }
        StateType::Follow => true,
        StateType::Complete => {
          body_string.push("pass".into());
          false
        }
        _ => true,
      } {
        match (&goto_state, s_type) {
          // Kernal calls can bypass gotos.
          (_, StateType::KernelCall(..)) => {}
          (Some(gt), _) => {
            let name = get_resolved_name(state_id.to_goto(), gt, resolved_states);
            body_string.push(format!("push state [ {name} ]"));
          }
          _ => {}
        }

        match s_type {
          //Ensure production calls are immediately called before any other gotos.
          StateType::KernelCall(prod_id) | StateType::InternalCall(prod_id) => {
            body_string.push(format!(
              "push state [ {} ]",
              get_state_hash_name(successor, state, resolved_states)
            ));
            body_string.push(format!("goto state [ {} ]", g.get_production(&prod_id)?.guid_name));
          }
          _ => {
            body_string.push(format!(
              "goto state [ {} ]",
              get_state_hash_name(successor, state, resolved_states)
            ));
          }
        }
      }

      debug_assert!(
        bc_id != 0,
        "The symbol [{}] was encountered in state \n{}\n of parser {}",
        successor.get_symbol().debug_string(g),
        successor.debug_string(g),
        entry_name
      );

      branches.insert(bc_id, (assert_type.to_string(), body_string));
    }

    let hash_groups = hash_group_vec(branches.clone(), |_, (_, (_, s))| s.clone());

    if hash_groups.len() == 1 && hash_groups[0].contains_key(&DEFAULT_SYM_ID) {
      let (_, body_string) = hash_groups[0].get(&DEFAULT_SYM_ID)?;
      let body_string = body_string.join(" then ");
      w.write_fmt(format_args!("\n{body_string}"))?;
    } else {
      for (bc_id, (assert_type, body_string)) in branches {
        let body_string = body_string.join(" then ");

        if bc_id == DEFAULT_SYM_ID {
          w.write_fmt(format_args!("\ndefault ( {body_string} )"))?;
        } else {
          if !is_scanner {
            is_token_assertion = true;
          }

          let head_string = format!("assert {assert_type} [ {bc_id} ]");
          w.write_fmt(format_args!("\n{head_string} ( {body_string} )"))?;
        }
      }
    }

    Some(Box::new(create_ir_state(j, graph, w, state, is_token_assertion)?))
  } else {
    None
  };

  let mut out = vec![];

  if matches!(
    state.get_type(),
    StateType::Complete
      | StateType::AssignAndFollow(..)
      | StateType::AssignToken(..)
      | StateType::Reduce(..)
  ) {
    let mut w = CodeWriter::new(vec![]);
    w.increase_indent();
    w.insert_newline()?;

    match state.get_type() {
      StateType::AssignAndFollow(sym_id) | StateType::AssignToken(sym_id) => {
        let bytecode_id = sym_id.bytecode_id(g);
        w.write(&format!("assign token [ {bytecode_id} ]"))?;
      }
      StateType::Complete => w.write("pass")?,

      StateType::Reduce(rule_id) => w.write(&create_rule_reduction(g, rule_id))?,
      _ => unreachable!(),
    }

    if let Some(mut base_state) = base_state {
      if state_id.is_root() {
        base_state.name = format!("{entry_name}_then");
      }
      let name = get_resolved_name(state_id.to_post_reduce(), &base_state, resolved_states);

      w.write_fmt(format_args!(" then goto state [ {name} ]"))?;

      out.push((state_id.to_post_reduce(), base_state));
    }
    let mut ir_state = create_ir_state(j, graph, w, state, false)?;
    if state_id.is_root() {
      ir_state.name = entry_name.to_string();
    }
    out.push((state_id, Box::new(ir_state)));
  } else if let Some(mut base_state) = base_state {
    if state_id.is_root() {
      base_state.name = entry_name.to_string()
    }
    out.push((state_id, base_state));
  }

  if let Some(goto_state) = goto_state {
    out.push((state_id.to_goto(), goto_state));
  }

  debug_assert!(
    !out.is_empty()
      || matches!(
        state.get_type(),
        StateType::GotoPass
          | StateType::ProductionCompleteOOS
          | StateType::ScannerCompleteOOS
          | StateType::Complete
          | StateType::AssignToken(..)
      ),
    "Graph state failed to generate ir states:\n{} \nGraph\n{}",
    state.debug_string(g),
    graph.__debug_string__()
  );

  SherpaResult::Ok(out)
}

fn create_rule_reduction(g: &std::sync::Arc<GrammarStore>, rule_id: RuleId) -> String {
  let rule = g.rules.get(&rule_id).unwrap();
  let production = g.productions.get(&rule.prod_id).unwrap();
  match (production.bytecode_id, rule.bytecode_id) {
    (Some(prod_bc_id), Some(rule_bc_id)) => {
      format!("reduce {} symbols to {prod_bc_id} with rule {rule_bc_id}", rule.len)
    }
    (None, _) => {
      unreachable!("Expected production {} to be assigned a production id", production.name)
    }
    (_, None) => {
      unreachable!("Expected rule to have a byte code id:\n {}", Item::from(rule).blame_string(g))
    }
  }
}

fn get_resolved_name(
  state_id: StateId,
  parse_state: &Box<ParseState>,
  resolved_states: &BTreeMap<StateId, Box<ParseState>>,
) -> String {
  if let Some(gt_existing) = resolved_states.get(&state_id) {
    gt_existing.get_name()
  } else {
    parse_state.get_name()
  }
}

fn get_state_hash_name<'a>(
  successor: &State,
  predecessor: &State,
  resolved_states: &BTreeMap<StateId, Box<ParseState>>,
) -> String {
  let state_name = (successor.get_id() != predecessor.get_id())
    .then(|| match resolved_states.get(&successor.get_id()) {
      Some(ir_state) => ir_state.get_name(),
      _ => String::from(format!("LEAF_STATE-{:X}", successor.get_hash())),
      //_ => String::from(format!("LEAF_STATE-{}-{:X}", successor.get_id().0, successor.get_hash())),
    })
    .unwrap_or(String::from("%%%%"));
  state_name
}

fn create_ir_state(
  j: &mut Journal,
  graph: &Graph,
  mut w: CodeWriter<Vec<u8>>,
  state: &State,
  is_token_assertion: bool,
) -> SherpaResult<ParseState> {
  let g = &(j.grammar()?);

  let (normal_symbols, skip_symbols) = if !is_token_assertion || graph.is_scan() {
    (vec![], vec![])
  } else {
    let (normal_symbol_set, skip_symbols_set) = get_symbols_from_state(j, graph, state)?;

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        w.write_fmt(format_args!(
          "\nassert TOKEN [ {} ] ( {} )",
          symbol_id.bytecode_id(&g),
          if state.get_type() == StateType::Peek { "peek-skip" } else { "skip-token" }
        ))?;
      }
    }

    (normal_symbol_set.into_iter().collect(), skip_symbols_set.into_iter().collect())
  };

  let ir_state = ParseState {
    comment: format!(
      "{}",
      state.debug_string(g),
      //  successors.iter().map(|s| s.debug_string(g)).collect::<Vec<_>>().join("\n\n")
    ),
    code: unsafe { String::from_utf8_unchecked(w.into_output()) },
    name: Default::default(),
    state_type: graph.is_scan().then_some(IRStateType::Scanner).unwrap_or(IRStateType::Parser),
    normal_symbols,
    skip_symbols,
    ..Default::default()
  }
  .into_hashed();

  SherpaResult::Ok(ir_state)
}

/// Gathers symbols from a set of items into normal and skipped sets.
pub(crate) fn get_symbols_from_state(
  j: &mut Journal,
  graph: &Graph,
  state: &State,
) -> SherpaResult<(SymbolSet, SymbolSet)> {
  let mut normal_symbol_set = SymbolSet::new();
  let g = &(j.grammar()?);
  let mut ignore_symbol_set = BTreeSet::new();
  let mut seen = HashSet::new();
  let mut queue = VecDeque::from_iter(state.get_closure(g, false));

  while let Some(item) = queue.pop_front() {
    if seen.insert(item) {
      match item.get_type(g) {
        ItemType::ExclusiveCompleted(..) | ItemType::Completed(_) => {
          for item in get_follow(j, graph, item)?.0 {
            queue.push_back(item);
          }
        }
        ItemType::NonTerminal(prod_id) => {
          for new_item in get_production_start_items(&prod_id, g) {
            queue.push_back(new_item.align(&item));
          }
        }
        ItemType::Terminal(sym_id) => {
          normal_symbol_set.insert(sym_id);
        }
        ItemType::TokenProduction(_, sym_id) => {
          normal_symbol_set.insert(sym_id);
        }
      }

      if let Some(ignored_symbols) = g.item_ignore_symbols.get(&item.to_absolute()) {
        for ignored_symbol in ignored_symbols {
          ignore_symbol_set.insert(*ignored_symbol);
        }
      }
    }
  }

  let ignore_symbol_set =
    ignore_symbol_set.difference(&normal_symbol_set).cloned().collect::<BTreeSet<_>>();

  SherpaResult::Ok((normal_symbol_set, ignore_symbol_set))
}

fn get_symbol_info(
  g: &GrammarStore,
  is_scanner: bool,
  sym: SymbolID,
) -> (u32, &'static str, String) {
  let (symbol_bytecode_id, assert_class, sym_comment) = if !is_scanner {
    (sym.bytecode_id(g), "TOKEN", sym.debug_string(g))
  } else {
    let (bc, class) = sym.shift_info(g);
    (bc, class, sym.debug_string(g))
  };
  (symbol_bytecode_id, assert_class, sym_comment)
}
