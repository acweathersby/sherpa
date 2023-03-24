use sherpa_runtime::types::bytecode::InputType;

use super::types::*;
use crate::{
  grammar::new::types::*,
  parser::{hash_group_btreemap, hash_group_vec},
  types::DEFAULT_SYM_ID,
  writer::code_writer::CodeWriter,
  Journal,
  SherpaResult,
};
use std::collections::VecDeque;

type SymbolSet = OrderedSet<SymbolId>;

pub(crate) fn build_ir<'db: 'follow, 'follow>(
  j: &mut Journal,
  graph: &Graph<'follow, 'db>,
  entry_name: IString,
) -> SherpaResult<Array<Box<ParseState<'db>>>> {
  let leaf_states = graph.get_leaf_states();
  let mut queue = VecDeque::from_iter(leaf_states.iter().cloned());
  let mut links: OrderedMap<StateId, Set<&State>> = OrderedMap::new();
  let mut output = OrderedMap::<StateId, Box<ParseState>>::new();
  let mut seen = OrderedSet::new();
  let empty_hash = Set::new();

  while let Some(state) = queue.pop_front() {
    if !seen.insert(state.get_id()) {
      continue;
    }
    let predecessors = state.get_predecessors().clone();
    for predecessor in &predecessors {
      links.entry(*predecessor).or_insert(Set::new()).insert(state);
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

    for (id, ir_state) in
      convert_state_to_ir(j, graph, state, successors, entry_name, goto_name)?
    {
      output.entry(id).or_insert(ir_state);
    }
  }
  #[cfg(debug_assertions)]
  debug_assert!(
    !output.is_empty(),
    "This graph did not yield any states! \n{}",
    graph.debug_string()
  );

  j.report_mut().ok_or_convert_to_error(output.into_values().collect())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum S_TYPE {
  GOTO_SUCCESSORS,
  SYMBOL_SUCCESSORS,
}

fn convert_goto_state_to_ir<'follow, 'db>(
  j: &mut Journal,
  graph: &Graph<'follow, 'db>,
  state: &State,
  successors: &Set<&State>,
) -> SherpaResult<(StateId, Box<ParseState<'db>>)> {
  let db = graph.get_db();
  let successors = successors.iter().filter(|s| {
    matches!(
      s.get_type(),
      StateType::GotoPass | StateType::GotoLoop | StateType::KernelGoto
    )
  });

  let mut w = CodeWriter::new(vec![]);

  (&mut w + "match: " + InputType::PRODUCTION_STR + " {").increase_indent();

  for (bc_id, (_prod_name, s_name, transition_type)) in successors
    .into_iter()
    .map(|s| {
      if let SymbolId::DBNonTerminal { key: index } = s.get_symbol() {
        let prod_id: usize = index.into();
        let prod_name = db.prod_name(index);
        (prod_id, (prod_name, create_ir_state_name(graph, s), s.get_type()))
      } else {
        #[cfg(debug_assertions)]
        panic!(
          "Invalid production type: {:?}  {}",
          s.get_symbol(),
          s.get_symbol().debug_string(db)
        );
        panic!()
      }
    })
    .collect::<OrderedMap<_, _>>()
  {
    let bc_id = bc_id.to_string();
    match transition_type {
      StateType::KernelGoto => {
        (&mut w) + "\n( " + bc_id + " ){ goto " + s_name + " }";
      }
      StateType::GotoLoop => {
        (&mut w) + "\n( " + bc_id + " ){ push %%%% then goto " + s_name + " }";
      }
      StateType::GotoPass => {
        (&mut w) + "\n( " + bc_id + " ){ pass }";
      }
      _ => unreachable!(),
    }
  }

  w.dedent() + "\n}";

  let mut goto = Box::new(create_ir_state(graph, w, state)?);

  goto.name = create_ir_state_name(graph, state).intern(db.string_store());

  SherpaResult::Ok((state.get_id(), goto))
}

fn convert_state_to_ir<'follow, 'db>(
  j: &mut Journal,
  graph: &Graph<'follow, 'db>,
  state: &State,
  successors: &Set<&State>,
  entry_name: IString,
  goto_state_id: Option<IString>,
) -> SherpaResult<Vec<(StateId, Box<ParseState<'db>>)>> {
  let is_scanner = graph.is_scan();
  let state_id = state.get_id();
  let db: &ParserDatabase = graph.get_db();
  let s_store = db.string_store();

  let successor_groups =
    hash_group_btreemap(successors.clone(), |_, s| match s.get_type() {
      StateType::GotoPass | StateType::GotoLoop | StateType::KernelGoto => {
        S_TYPE::GOTO_SUCCESSORS
      }
      _ => S_TYPE::SYMBOL_SUCCESSORS,
    });

  let base_state = if let Some(successors) =
    successor_groups.get(&S_TYPE::SYMBOL_SUCCESSORS)
  {
    let mut w = CodeWriter::new(vec![]);
    let mut default = String::default();

    w.indent();

    add_tok_expr(successors, &mut w, db);

    let mut classes = classify_successors(successors);

    add_match_expr(&mut w, graph, &mut classes, goto_state_id);

    let mut state = Box::new(create_ir_state(graph, w, state)?);

    Some(state)
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
      StateType::AssignAndFollow(tok_id) | StateType::AssignToken(tok_id) => {
        let bytecode_id = tok_id.to_index();

        (&mut w) + "set-tok " + db.tok_id(tok_id).to_string();
      }
      StateType::Complete => w.write("pass")?,

      StateType::Reduce(rule_id) => {
        w.write(&create_rule_reduction(rule_id, db))?
      }
      _ => unreachable!(),
    }

    if let Some(mut base_state) = base_state {
      if state_id.is_root() {
        base_state.name =
          (entry_name.to_string(s_store) + "_then").intern(s_store);
      } else {
        base_state.name =
          (base_state.name.to_string(s_store) + "_then").intern(s_store);
      }

      w.w(" then goto ")?.w(&base_state.name.to_string(s_store))?;

      out.push((state_id.to_post_reduce(), base_state));
    }

    let mut ir_state = create_ir_state(graph, w, state)?;

    if state_id.is_root() {
      ir_state.name = entry_name;
    }

    out.push((state_id, Box::new(ir_state)));
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

fn add_tok_expr(
  successors: &std::collections::HashSet<&State>,
  w: &mut CodeWriter<Vec<u8>>,
  db: &ParserDatabase,
) {
  let mut set_token = successors
    .iter()
    .filter(|s| {
      matches!(
        s.get_type(),
        StateType::AssignToken(..) | StateType::AssignAndFollow(..)
      )
    })
    .collect::<Array<_>>();

  debug_assert!(set_token.len() <= 1);

  if let Some(set_tok) = set_token.pop() {
    match set_tok.get_type() {
      StateType::AssignAndFollow(tok_id) | StateType::AssignToken(tok_id) => {
        let bytecode_id = tok_id.to_index();

        (w + "set-tok " + db.tok_id(tok_id).to_string()).prime_join(" then ");
      }
      _ => unreachable!(),
    }
  }
}

fn classify_successors<'graph, 'db>(
  successors: &'graph Set<&'graph State<'db>>,
) -> VecDeque<(InputType, Set<&'graph State<'db>>)> {
  VecDeque::from_iter(hash_group_btreemap(successors.clone(), |_, s| {
    match s.get_symbol() {
      SymbolId::EndOfFile { .. } => InputType::EndOfFile,
      SymbolId::DBToken { .. } | SymbolId::DBNonTerminalToken { .. } => {
        InputType::Token
      }
      SymbolId::Char { .. } => InputType::Byte,
      SymbolId::Codepoint { .. } => InputType::Codepoint,
      SymbolId::Default => InputType::Default,
      sym if sym.is_class() => InputType::Class,
      sym => {
        #[cfg(debug_assertions)]
        unreachable!("{sym:?}");
        unreachable!()
      }
    }
  }))
}

fn add_match_expr<'follow, 'db>(
  mut w: &mut CodeWriter<Vec<u8>>,
  graph: &Graph<'follow, 'db>,
  branches: &mut VecDeque<(InputType, Set<&State>)>,
  goto_state_id: Option<IString>,
) {
  let is_scanner = graph.is_scan();
  let db = graph.get_db();

  if let Some((input_type, (successors))) = branches.pop_front() {
    if matches!(input_type, InputType::Default) {
      let successor = successors.into_iter().next().unwrap();

      let string = build_body(successor, graph, goto_state_id).join(" then ");

      if !string.is_empty() {
        w + string;
      }
    } else {
      w = (w + "\nmatch: " + input_type.as_str() + " {").indent();

      for s in successors {
        let sym = s.get_symbol();

        if input_type == InputType::Token {
          //TODO: Add skip at this point as well.
          //  tokens.insert(db.sym_data())
        }

        let s_type = s.get_type();
        w = w + "\n\n( " + sym.to_state_val().to_string() + " ){ ";
        w = w + build_body(s, graph, goto_state_id).join(" then ") + " }";
      }

      if !branches.is_empty() {
        w = (w + "\n\ndefault {").indent();
        add_match_expr(w, graph, branches, goto_state_id);
        w = w.dedent() + "\n}";
      }

      w.dedent() + "\n}";
    }
  }
}

fn build_body<'follow, 'db>(
  successor: &State,
  graph: &Graph<'follow, 'db>,
  goto_state_id: Option<IString>,
) -> Vec<String> {
  let is_scanner = graph.is_scan();
  let mut body_string = Array::new();
  let sym = successor.get_symbol();
  let s_type = successor.get_type();
  let db = graph.get_db();

  if match s_type {
    StateType::Shift => {
      body_string.push(is_scanner.then_some("scan").unwrap_or("shift").into());
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
      body_string.push(create_rule_reduction(rule_id, db));
      false
    }
    StateType::AssignToken(..) => false,
    StateType::Follow => true,
    StateType::Complete => {
      body_string.push("pass".into());
      false
    }
    _ => true,
  } {
    // Add goto expressions

    match (&goto_state_id, s_type) {
      // Kernel calls can bypass gotos.
      (_, StateType::KernelCall(..)) => {}
      (Some(gt), _) => {
        body_string.push("push ".to_string() + &gt.to_string(db.string_store()))
      }
      _ => {}
    }

    match s_type {
      //Ensure production calls are immediately called before any other
      // gotos.
      StateType::KernelCall(prod_id) | StateType::InternalCall(prod_id) => {
        body_string
          .push("push ".to_string() + &create_ir_state_name(graph, successor));
        body_string.push(
          "goto ".to_string()
            + &db.prod_name(prod_id).to_string(db.string_store()),
        );
      }
      _ => {
        body_string
          .push("goto ".to_string() + &create_ir_state_name(graph, successor));
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

  &mut w + "reduce " + rule.symbols.len().to_string();
  &mut w + " symbols to " + prod_id.to_string();
  &mut w + " with rule " + rule_id.to_string();

  w.to_string()
}

fn get_resolved_name(
  state_id: StateId,
  parse_state: &Box<ParseState>,
  resolved_states: &OrderedMap<StateId, Box<ParseState>>,
) -> IString {
  if let Some(gt_existing) = resolved_states.get(&state_id) {
    gt_existing.name
  } else {
    parse_state.name
  }
}

pub(super) fn create_ir_state_name(graph: &Graph, state: &State) -> String {
  graph.is_scan().then_some("s").unwrap_or("p").to_string()
    + "_"
    + &state.get_hash().to_string()
}

pub(super) fn create_ir_state<'follow, 'db>(
  graph: &Graph<'follow, 'db>,
  mut w: CodeWriter<Vec<u8>>,
  state: &State,
) -> SherpaResult<ParseState<'db>> {
  let db = graph.get_db();

  let ir_state = ParseState {
    comment:  Default::default(),
    code:     w.to_string(),
    name:     create_ir_state_name(graph, state)
      .intern(graph.get_db().string_store()),
    ast:      SherpaResult::None,
    scanners: None,
  };

  SherpaResult::Ok(ir_state)
}
