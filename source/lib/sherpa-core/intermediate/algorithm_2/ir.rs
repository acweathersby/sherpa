use crate::{
  build::bytecode,
  grammar::get_production_start_items2,
  intermediate::utils::hash_group_btreemap,
  types::{
    graph_2::{StateId, *},
    item_2::{Item, ItemType},
    Item as OItem,
    *,
  },
  writer::code_writer::CodeWriter,
  Journal,
  SherpaResult,
};
use std::{
  collections::{btree_map::Entry, BTreeMap, BTreeSet, HashSet, VecDeque},
  vec,
};

use super::graph::get_follow;

pub(crate) fn convert_graph_to_ir(
  j: &mut Journal,
  graph: &Graph,
  entry_name: &str,
  is_scanner: bool,
) -> SherpaResult<Vec<Box<IRState>>> {
  let leaf_states = graph.get_leaf_states();
  let mut queue = VecDeque::from_iter(leaf_states.iter().cloned());
  let mut links: BTreeMap<StateId, HashSet<&State>> = BTreeMap::new();
  let mut rebuilds = HashSet::new();
  let mut output = BTreeMap::<StateId, Box<IRState>>::new();
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
        println!("{:?}  -> {:?}", state.get_id(), successor.get_id());
        queue.push_back(state);
        continue 'outer;
      }
    }

    for (id, ir_state) in
      convert_state_to_ir(j, graph, state, successors, &mut output, entry_name, is_scanner)?
    {
      output.insert(id, ir_state);
    }
  }

  let mut hash_filter = BTreeSet::<u64>::new();
  let mut out = vec![];

  // Rebuild nodes that have cycles to update
  for state in rebuilds {
    let successors = links.get(&state.get_id()).unwrap_or(&empty_hash);

    for (id, ir_state) in
      convert_state_to_ir(j, graph, state, successors, &mut output, entry_name, is_scanner)?
    {
      println!("{:?} {:?}", state.get_id(), id);
      let mut existings_state = output.remove(&id)?;
      if hash_filter.insert(existings_state.get_hash()) {
        existings_state.code = ir_state.code;
        out.push(existings_state);
      }
    }
  }

  for (_, state) in output {
    if hash_filter.insert(state.get_hash()) {
      out.push(state)
    }
  }

  SherpaResult::Ok(out)
}

fn convert_state_to_ir(
  j: &mut Journal,
  graph: &Graph,
  state: &State,
  successors: &HashSet<&State>,
  resolved_states: &mut BTreeMap<StateId, Box<IRState>>,
  entry_name: &str,
  is_scanner: bool,
) -> SherpaResult<Vec<(StateId, Box<IRState>)>> {
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
  enum S_TYPE {
    GOTO_SUCCESSORS,
    SYMBOL_SUCCESSORS,
  }
  let g = &(j.grammar()?);

  let successor_groups =
    hash_group_btreemap(successors.clone(), |_, s| match s.get_transition_type() {
      TransitionType::GotoPass | TransitionType::GotoLoop | TransitionType::Goto => {
        S_TYPE::GOTO_SUCCESSORS
      }
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
            prod.bytecode_id,
            (&prod.name, get_state_hash_name(s, state, resolved_states), s.get_transition_type()),
          )
        } else {
          panic!("Invalid production type: {}", s.get_symbol().debug_string(g))
        }
      })
      .collect::<BTreeMap<_, _>>()
    {
      match transition_type {
        TransitionType::Goto => {
          w.write_fmt(format_args!(
            "\n assert PRODUCTION [ {bc_id} ] ( goto state [ {state_name} ] )"
          ))?;
        }
        TransitionType::GotoLoop => {
          w.write_fmt(format_args!(
            "\n assert PRODUCTION [ {bc_id} ] ( goto state [ {state_name} ] then goto state [ %%%% ] )"
          ))?;
        }
        TransitionType::GotoPass => {
          w.write_fmt(format_args!("\n assert PRODUCTION [ {bc_id} ] ( pass )"))?;
        }
        _ => unreachable!(),
      }
    }

    Some(Box::new(create_ir_state(j, graph, w, state, is_scanner, false, true)?))
  } else {
    None
  };

  let base_state = if let Some(successors) = successor_groups.get(&S_TYPE::SYMBOL_SUCCESSORS) {
    let mut w = CodeWriter::new(vec![]);
    let mut is_token_assertion = false;
    let mut default_body_string = None;
    let mut token_assign_string = String::new();
    w.increase_indent();
    for (bc_id, (sym, assert_type, _sym_name, transition_type, successor)) in successors
      .into_iter()
      .map(|s| {
        let sym = s.get_symbol();
        let (bc_id, assert_type, sym_name) = get_symbol_info(g, is_scanner, sym);
        (bc_id, (sym, assert_type, sym_name, s.get_transition_type(), s))
      })
      .collect::<BTreeMap<_, _>>()
      .into_iter()
      .rev()
    {
      let is_peek = !is_scanner
        && matches!(transition_type, TransitionType::Peek | TransitionType::OutOfScopePass);

      let mut body_string = "(".to_string();

      if match transition_type {
        TransitionType::Call => {
          body_string += &format!(
            " goto state [ {} ] then",
            g.get_production(&successor.get_call_production_id()?)?.guid_name
          );
          true
        }
        TransitionType::Shift => {
          body_string += " shift then";
          true
        }
        TransitionType::OutOfScopePop => {
          body_string += " pop";
          false
        }

        TransitionType::OutOfScopePass => {
          body_string += " pass";
          false
        }
        TransitionType::ReduceProduction => {
          body_string += " ";
          body_string += &create_rule_reduction_string(g, successor);
          false
        }
        TransitionType::AssignToken if successor.get_reduce_item().is_some() => {
          if let Some(tok_string) = create_token_assign_string(g, successor) {
            token_assign_string = tok_string;
            body_string += " ";
            body_string += &token_assign_string;
            token_assign_string += " then";
          } else {
            body_string += " pass";
          }
          false
        }
        _ => true,
      } {
        body_string +=
          &format!(" goto state [ {} ]", get_state_hash_name(successor, state, resolved_states));

        if goto_state.is_some() {
          body_string += &format!(" then goto state [ {} ]", goto_state.as_ref()?.get_name());
        }
      }

      body_string += " )";

      if sym == SymbolID::Default {
        default_body_string = Some(body_string.clone());
        w.write_fmt(format_args!("\ndefault {body_string}"))?;
      } else {
        let head_string = if is_peek {
          format!("assert peek {assert_type} [ {bc_id} ]")
        } else {
          format!("assert {assert_type} [ {bc_id} ]")
        };

        if default_body_string.is_none() || &body_string != default_body_string.as_ref()? {
          is_token_assertion = true;
          w.write_fmt(format_args!("\n{head_string} {token_assign_string}{body_string}"))?;
        }
      }
    }

    Some(Box::new(create_ir_state(j, graph, w, state, is_scanner, is_token_assertion, false)?))
  } else {
    None
  };

  let mut out = vec![];

  if let Some(_) = state.get_reduce_item() {
    let mut w = CodeWriter::new(vec![]);
    w.increase_indent();
    w.insert_newline()?;

    if is_scanner {
      if let Some(tok_assign) = create_token_assign_string(g, state) {
        w.write(&tok_assign)?;
      } else {
        w.write("pass")?;
      }
    } else {
      w.write(&create_rule_reduction_string(g, state))?;
    }

    if let Some(mut base_state) = base_state {
      if state.get_id().is_root() {
        base_state.name = format!("{entry_name}_then");
      }
      w.write_fmt(format_args!(" then goto state [ {} ]", base_state.get_name()))?;
      out.push((state.get_id().to_post_reduce(), base_state));
    }
    let mut ir_state = create_ir_state(j, graph, w, state, is_scanner, false, false)?;
    if state.get_id().is_root() {
      ir_state.name = entry_name.to_string();
    }
    out.push((state.get_id(), Box::new(ir_state)));
  } else if let Some(mut base_state) = base_state {
    if state.get_id().is_root() {
      base_state.name = entry_name.to_string()
    }
    out.push((state.get_id(), base_state));
  }

  if let Some(goto_state) = goto_state {
    out.push((state.get_id().to_goto(), goto_state));
  }

  debug_assert!(
    !out.is_empty()
      || matches!(
        state.get_transition_type(),
        TransitionType::GotoPass | TransitionType::OutOfScopePass | TransitionType::OutOfScopePop
      ),
    "Graph state failed to generate ir states:\n{}",
    state.debug_string(g)
  );

  SherpaResult::Ok(out)
}

fn create_rule_reduction_string(g: &GrammarStore, state: &State) -> String {
  let item = state
    .get_reduce_item()
    .unwrap_or_else(|| panic!("State lacks reduce item ! \n{}", state.debug_string(g)));
  let rule = g.rules.get(&item.get_rule_id()).unwrap();
  let production = g.productions.get(&rule.prod_id).unwrap();
  format!(
    "set prod to {} then reduce {} symbols with rule {}",
    production.bytecode_id, rule.len, rule.bytecode_id,
  )
}

fn create_token_assign_string(g: &GrammarStore, state: &State) -> Option<String> {
  let item = state.get_reduce_item().unwrap();
  let sym = item.origin.get_symbol();
  let bytecode_id = sym.bytecode_id(Some(g));
  (bytecode_id > 0).then(|| format!("assign token [ {bytecode_id} ]"))
}

fn get_state_hash_name<'a>(
  successor: &State,
  predecessor: &State,
  resolved_states: &BTreeMap<StateId, Box<IRState>>,
) -> String {
  let state_name = (successor.get_id() != predecessor.get_id())
    .then(|| match resolved_states.get(&successor.get_id()) {
      Some(ir_state) => ir_state.get_name(),
      _ => String::from(format!("LEAF_STATE{}", successor.get_id().0)),
    })
    .unwrap_or(String::from("%%%%"));
  state_name
}

fn create_ir_state(
  j: &mut Journal,
  graph: &Graph,
  mut w: CodeWriter<Vec<u8>>,
  state: &State,
  is_scanner: bool,
  is_token_assertion: bool,
  is_goto: bool,
) -> SherpaResult<IRState> {
  let g = &(j.grammar()?);

  let (normal_symbols, skip_symbols) = if !is_token_assertion || is_scanner {
    (vec![], vec![])
  } else {
    let (normal_symbol_set, skip_symbols_set) = get_symbols_from_state(j, graph, state)?;

    if is_token_assertion {
      for symbol_id in &skip_symbols_set {
        w.write_fmt(format_args!("\nskip TOKEN [ {} ]", symbol_id.bytecode_id(Some(&g)),))?;
      }
    }

    (normal_symbol_set.into_iter().collect(), skip_symbols_set.into_iter().collect())
  };

  let ir_state = IRState {
    comment: if is_goto {
      format!(
        "{:?}\n    {}",
        state.get_id(),
        state
          .get_nonterminal_items()
          .iter()
          .map(|i| i.debug_string(g))
          .collect::<Vec<_>>()
          .join("\n    ")
      )
    } else {
      format!(
        "{:?}\n    {}",
        state.get_id(),
        state
          .get_kernel_items()
          .iter()
          .map(|i| i.debug_string(g))
          .collect::<Vec<_>>()
          .join("\n    ")
      )
    },
    code: unsafe { String::from_utf8_unchecked(w.into_output()) },
    name: Default::default(),
    state_type: if is_scanner { IRStateType::Scanner } else { IRStateType::Parser },
    graph_id: NodeId::new(0),
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
        ItemType::Completed(_) => {
          for item in get_follow(j, graph, item, false)? {
            queue.push_back(item);
          }
        }
        ItemType::NonTerminal(prod_id) => {
          for new_item in get_production_start_items2(&prod_id, g) {
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

      if let Some(ignored_symbols) = g.item_ignore_symbols.get(
        &OItem { len: item.len, rule: item.rule, off: item.off, ..Default::default() }
          .to_empty_state(),
      ) {
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
    (sym.bytecode_id(Some(g)), "TOKEN", sym.debug_string(g))
  } else {
    let (bc, class) = sym.shift_info(g);
    (bc, class, sym.debug_string(g))
  };
  (symbol_bytecode_id, assert_class, sym_comment)
}
