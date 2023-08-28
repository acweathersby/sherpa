//! Functions for translating parse graphs into Sherpa IR code.
use crate::{journal::Journal, types::*, utils::hash_group_btreemap, writer::code_writer::CodeWriter};
use sherpa_rust_runtime::types::bytecode::InputType;
use std::collections::{BTreeMap, VecDeque};

pub(crate) fn build_ir<'db>(j: &mut Journal, graph: &Graph<'db>, entry_name: IString) -> SherpaResult<Array<Box<ParseState>>> {
  debug_assert!(entry_name.as_u64() != 0);

  let leaf_states = graph.get_leaf_states();
  let mut queue = VecDeque::from_iter(leaf_states.iter().cloned());
  let mut links: OrderedMap<StateId, OrderedSet<&State>> = OrderedMap::new();
  let mut output = OrderedMap::<StateId, Box<ParseState>>::new();
  let mut seen = OrderedSet::new();
  let empty_hash = OrderedSet::new();

  while let Some(state) = queue.pop_front() {
    if !seen.insert(state.get_id()) {
      continue;
    }
    let predecessors = state.get_predecessors().clone();
    for predecessor in &predecessors {
      links.entry(*predecessor).or_insert(OrderedSet::new()).insert(state);
      queue.push_back(&graph[*predecessor]);
    }
  }

  queue.extend(leaf_states.iter());
  queue.extend(links.keys().rev().map(|id| &graph[*id]));

  while let Some(state) = queue.pop_front() {
    if output.contains_key(&state.get_id()) {
      continue;
    }

    let successors = links.get(&state.get_id()).unwrap_or(&empty_hash);

    let goto_name = if let Some(goto) = state.get_goto_state() {
      let goto_pair = convert_goto_state_to_ir(j, graph, &goto, successors)?;
      let out = Some(goto_pair.1.name.clone());
      output.insert(goto_pair.0, goto_pair.1);
      out
    } else {
      None
    };

    for (id, ir_state) in convert_state_to_ir(j, graph, state, successors, entry_name, goto_name)? {
      output.entry(id).or_insert(ir_state);
    }
  }
  #[cfg(debug_assertions)]
  debug_assert!(!output.is_empty(), "This graph did not yield any states! \n{}", graph.debug_string());

  j.report_mut().wrap_ok_or_return_errors(output.into_values().collect())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SType {
  GotoSuccessors,
  SymbolSuccessors,
}

fn convert_goto_state_to_ir<'db>(
  _j: &mut Journal,
  graph: &Graph<'db>,
  state: &State,
  successors: &OrderedSet<&State>,
) -> SherpaResult<(StateId, Box<ParseState>)> {
  let db = graph.get_db();
  let successors =
    successors.iter().filter(|s| matches!(s.get_type(), StateType::GotoPass | StateType::GotoLoop | StateType::KernelGoto));

  let mut w = CodeWriter::new(vec![]);

  (&mut w + "match: " + InputType::PRODUCTION_STR + " {").increase_indent();

  for (bc_id, (_prod_name, s_name, transition_type)) in successors
    .into_iter()
    .map(|s| {
      if let SymbolId::DBNonTerminal { key: index } = s.get_symbol() {
        let prod_id: usize = index.into();
        let prod_name = db.prod_guid_name(index);
        (prod_id, (prod_name, create_ir_state_name(graph, None, s), s.get_type()))
      } else {
        #[cfg(debug_assertions)]
        panic!("Invalid production type: {:?}  {}", s.get_symbol(), s.get_symbol().debug_string(db));
        #[cfg(not(debug_assertions))]
        panic!()
      }
    })
    .collect::<OrderedMap<_, _>>()
  {
    let bc_id = bc_id.to_string();
    match transition_type {
      StateType::KernelGoto => {
        let _ = (&mut w) + "\n( " + bc_id + " ){ goto " + s_name + " }";
      }
      StateType::GotoLoop => {
        let _ = (&mut w) + "\n( " + bc_id + " ){ push %%%% then goto " + s_name + " }";
      }
      StateType::GotoPass => {
        let _ = (&mut w) + "\n( " + bc_id + " ){ pass }";
      }
      _ => unreachable!(),
    }
  }

  let _ = w.dedent() + "\n}";

  let mut goto = Box::new(create_ir_state(graph, w, state)?);

  goto.name = create_ir_state_name(graph, None, state).intern(db.string_store());

  SherpaResult::Ok((state.get_id(), goto))
}

fn convert_state_to_ir<'db>(
  _j: &mut Journal,
  graph: &Graph<'db>,
  state: &State,
  successors: &OrderedSet<&State>,
  entry_name: IString,
  goto_state_id: Option<IString>,
) -> SherpaResult<Vec<(StateId, Box<ParseState>)>> {
  let state_id = state.get_id();
  let db: &ParserDatabase = graph.get_db();
  let s_store = db.string_store();

  let successor_groups = hash_group_btreemap(successors.clone(), |_, s| match s.get_type() {
    StateType::GotoPass | StateType::GotoLoop | StateType::KernelGoto => SType::GotoSuccessors,
    _ => SType::SymbolSuccessors,
  });

  let base_state = if let Some(successors) = successor_groups.get(&SType::SymbolSuccessors) {
    let mut w = CodeWriter::new(vec![]);

    w.indent();

    add_tok_expr(successors, &mut w, db);

    let mut classes = classify_successors(successors, db);

    add_match_expr(&mut w, state, graph, &mut classes, goto_state_id);

    Some(Box::new(create_ir_state(graph, w, state)?))
  } else {
    None
  };

  let mut out = vec![];

  if matches!(
    state.get_type(),
    StateType::Complete | StateType::AssignAndFollow(..) | StateType::AssignToken(..) | StateType::Reduce(..)
  ) {
    let mut w = CodeWriter::new(vec![]);
    w.increase_indent();
    w.insert_newline()?;

    match state.get_type() {
      StateType::AssignAndFollow(tok_id) | StateType::AssignToken(tok_id) => {
        let _ = (&mut w) + "set-tok " + db.tok_val(tok_id).to_string();
      }
      StateType::Complete => w.write("pass")?,

      StateType::Reduce(rule_id) => w.write(&create_rule_reduction(rule_id, db))?,
      _ => unreachable!(),
    }

    if let Some(mut base_state) = base_state {
      if state_id.is_root() {
        base_state.name = (entry_name.to_string(s_store) + "_then").intern(s_store);
      } else {
        base_state.name = (base_state.name.to_string(s_store) + "_then").intern(s_store);
      }

      w.w(" then goto ")?.w(&base_state.name.to_string(s_store))?;

      out.push((state_id.to_post_reduce(), base_state));
    }

    let mut ir_state = create_ir_state(graph, w, state)?;

    if state_id.is_root() {
      ir_state.name = entry_name;
    }

    out.push((state_id, Box::new(ir_state)));
  } else if state.get_type() == StateType::DifferedReduce {
    let (shifts, reduces): (Vec<_>, Vec<_>) = successors.iter().cloned().partition(|s| s.get_type() == StateType::ShiftPrefix);
    let mut w = CodeWriter::new(vec![]);

    w.w(" push ")?.w(&create_ir_state_name(graph, Some(state), &reduces[0]))?;
    w.w(" then goto ")?.w(&create_ir_state_name(graph, Some(state), &shifts[0]))?;
    let base_state = Box::new(create_ir_state(graph, w, state)?);

    out.push((state_id.to_post_reduce(), base_state));
  } else if let Some(mut base_state) = base_state {
    if state_id.is_root() {
      base_state.name = entry_name;
    }

    out.push((state_id, base_state));
  }
  #[cfg(debug_assertions)]
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
    state.debug_string(db),
    graph.debug_string()
  );

  SherpaResult::Ok(out)
}

fn add_tok_expr(successors: &OrderedSet<&State>, w: &mut CodeWriter<Vec<u8>>, db: &ParserDatabase) {
  let mut set_token = successors
    .iter()
    .filter(|s| matches!(s.get_type(), StateType::AssignToken(..) | StateType::AssignAndFollow(..)))
    .collect::<Array<_>>();

  debug_assert!(set_token.len() <= 1);

  if let Some(set_tok) = set_token.pop() {
    match set_tok.get_type() {
      StateType::AssignAndFollow(tok_id) | StateType::AssignToken(tok_id) => {
        (w + "set-tok " + db.tok_val(tok_id).to_string()).prime_join(" then ");
      }
      _ => unreachable!(),
    }
  }
}

fn classify_successors<'graph, 'db>(
  successors: &'graph OrderedSet<&'graph State<'db>>,
  _db: &'db ParserDatabase,
) -> VecDeque<((u32, InputType), OrderedSet<&'graph State<'db>>)> {
  VecDeque::from_iter(
    hash_group_btreemap(successors.clone(), |_, s| match s.get_symbol() {
      SymbolId::EndOfFile { .. } => (0, InputType::EndOfFile),
      SymbolId::DBToken { .. } | SymbolId::DBNonTerminalToken { .. } => (4, InputType::Token),
      SymbolId::Char { .. } => (1, InputType::Byte),
      SymbolId::Codepoint { .. } => (2, InputType::Codepoint),
      SymbolId::Default => (5, InputType::Default),
      sym if sym.is_class() => (3, InputType::Class),
      _sym => {
        #[cfg(debug_assertions)]
        unreachable!("{_sym:?} {}", s.debug_string(_db));
        #[cfg(not(debug_assertions))]
        unreachable!()
      }
    })
    .into_iter(),
  )
}

fn add_match_expr<'db>(
  mut w: &mut CodeWriter<Vec<u8>>,
  state: &State,
  graph: &Graph<'db>,
  branches: &mut VecDeque<((u32, InputType), OrderedSet<&State>)>,
  goto_state_id: Option<IString>,
) {
  let db = graph.get_db();

  if let Some(((_, input_type), successors)) = branches.pop_front() {
    if matches!(input_type, InputType::Default) {
      let successor = successors.into_iter().next().unwrap();

      let string = build_body(state, successor, graph, goto_state_id).join(" then ");

      if !string.is_empty() {
        let _ = w + string;
      }
    } else {
      w = (w + "\nmatch: " + input_type.as_str() + " {").indent();

      // Sort successors

      let peeking = successors.iter().any(|s| matches!(s.get_type(), StateType::PeekEnd | StateType::Peek));

      for (state_val, s) in successors.iter().map(|s| (s.get_symbol().to_state_val(db), s)).collect::<BTreeMap<_, _>>() {
        w = w + "\n\n( " + state_val.to_string() + " ){ ";
        w = w + build_body(state, s, graph, goto_state_id).join(" then ") + " }";
      }

      // Add skips
      if input_type == InputType::Token {
        let syms = successors.iter().map(|s| s.get_symbol().tok_db_key().unwrap()).collect::<OrderedSet<_>>();

        let skipped = successors
          .iter()
          .flat_map(|s| s.kernel_items_ref())
          .flat_map(|i| i.get_skipped())
          .filter_map(|s| {
            let id = s.tok_db_key().unwrap();
            (!syms.contains(&id)).then_some(id)
          })
          .collect::<OrderedSet<_>>();

        if !skipped.is_empty() {
          let vals = skipped.iter().map(|v| v.to_val(db).to_string()).collect::<Array<_>>().join(" | ");
          if vals.len() > 0 {
            w = w + "\n( " + vals + " ){ " + peeking.then_some("peek-skip").unwrap_or("skip") + " }";
          }
        }
      }

      if !branches.is_empty() {
        w = (w + "\n\ndefault { ").indent();
        add_match_expr(w, state, graph, branches, goto_state_id);
        w = w + " }";
        w = w.dedent();
      }

      let _ = w.dedent() + "\n}";
    }
  }
}

fn build_body<'db>(state: &State, successor: &State, graph: &Graph<'db>, goto_state_id: Option<IString>) -> Vec<String> {
  let is_scanner = graph.is_scanner();
  let mut body_string: Vec<String> = Array::new();
  let s_type = successor.get_type();
  let db = graph.get_db();

  if match s_type {
    StateType::Shift => {
      let scan_expr = successor.get_symbol().is_linefeed().then_some("scan then set-line").unwrap_or("scan");
      body_string.push(is_scanner.then_some(scan_expr).unwrap_or("shift").into());
      true
    }
    StateType::PeekEnd => {
      debug_assert!(!is_scanner, "Peek states should not be present in graph");
      body_string.push("reset".into());
      true
    }
    StateType::Peek => {
      debug_assert!(!is_scanner, "Peek states should not be present in graph");
      body_string.push("peek".into());
      true
    }
    StateType::ProductionCompleteOOS => {
      debug_assert!(!is_scanner, "ProductionCompleteOOS states should only exist in normal parse graphs");
      body_string.push("pop".into());
      false
    }
    StateType::ScannerCompleteOOS => {
      debug_assert!(is_scanner, "ScannerCompleteOOS states should only exist in scanner parse graphs");
      body_string.push("pass".into());
      false
    }
    StateType::Reduce(rule_id) => {
      body_string.push(create_rule_reduction(rule_id, db));
      false
    }
    StateType::Follow => true,
    StateType::AssignToken(..) | StateType::Complete => {
      body_string.push("pass".into());
      false
    }
    _ => true,
  } {
    // Add goto expressions

    match (&goto_state_id, s_type) {
      // Kernel calls can bypass gotos.
      (_, StateType::KernelCall(..)) => {}
      (Some(gt), _) => body_string.push("push ".to_string() + &gt.to_string(db.string_store())),
      _ => {}
    }

    match s_type {
      //Ensure production calls are immediately called before any other
      // gotos.
      StateType::KernelCall(prod_id) | StateType::InternalCall(prod_id) => {
        body_string.push("push ".to_string() + &create_ir_state_name(graph, Some(state), successor));
        body_string.push("goto ".to_string() + &db.prod_guid_name(prod_id).to_string(db.string_store()));
      }
      _ => {
        body_string.push("goto ".to_string() + &create_ir_state_name(graph, Some(state), successor));
      }
    }
  }

  body_string
}

fn create_rule_reduction(rule_id: DBRuleKey, db: &ParserDatabase) -> String {
  let rule = db.rule(rule_id);
  let prod = db.rule_prod(rule_id);
  let prod_id: usize = prod.into();
  let rule_id: usize = rule_id.into();
  let mut w = CodeWriter::new(vec![]);

  let _ = &mut w + "reduce " + rule.symbols.len().to_string();
  let _ = &mut w + " symbols to " + prod_id.to_string();
  let _ = &mut w + " with rule " + rule_id.to_string();

  w.to_string()
}

pub(super) fn create_ir_state_name(graph: &Graph, origin_state: Option<&State>, target_state: &State) -> String {
  if origin_state.is_some_and(|s| s.get_id() == target_state.get_id()) {
    "%%%%".to_string()
  } else {
    graph.is_scanner().then_some("s").unwrap_or("p").to_string() + "_" + &target_state.get_hash().to_string()
  }
}

pub(super) fn create_ir_state<'db>(graph: &Graph<'db>, w: CodeWriter<Vec<u8>>, state: &State) -> SherpaResult<ParseState> {
  let ir_state = ParseState {
    code: w.to_string(),
    name: create_ir_state_name(graph, None, state).intern(graph.get_db().string_store()),
    ..Default::default()
  };

  SherpaResult::Ok(ir_state)
}
