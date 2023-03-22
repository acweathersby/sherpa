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

pub(crate) fn build_ir(
  j: &mut Journal,
  graph: &Graph,
  entry_name: &str,
) -> SherpaResult<Array<Box<ParseState>>> {
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

  debug_assert!(
    !output.is_empty(),
    "This graph did not yield any states! \n{}",
    graph.__debug_string__()
  );

  j.report_mut().ok_or_convert_to_error(output.into_values().collect())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum S_TYPE {
  GOTO_SUCCESSORS,
  SYMBOL_SUCCESSORS,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
#[repr(u32)]
pub enum InputType {
  Production = 0,
  Token,
  EndOfFile,
  Byte,
  Codepoint,
  Class,
  Default,
}

impl InputType {
  fn type_name(&self) -> &'static str {
    use InputType::*;
    match self {
      Production => "PRODUCTION",
      Token => "TOKEN",
      EndOfFile => "ENDOFFILE",
      Byte => "BYTE",
      Codepoint => "CODEPOINT",
      Class => "CLASS",
      Default => "DEFAULT",
    }
  }
}

fn convert_goto_state_to_ir(
  j: &mut Journal,
  graph: &Graph,
  state: &State,
  successors: &Set<&State>,
) -> SherpaResult<(StateId, Box<ParseState>)> {
  let db = graph.get_db();
  let successors = successors.iter().filter(|s| {
    matches!(
      s.get_type(),
      StateType::GotoPass | StateType::GotoLoop | StateType::KernelGoto
    )
  });

  let mut w = CodeWriter::new(vec![]);

  (&mut w + "match: PRODUCTION {").increase_indent();

  for (bc_id, (_prod_name, s_name, transition_type)) in successors
    .into_iter()
    .map(|s| {
      if let SymbolId::DBNonTerminal { key: index } = s.get_symbol() {
        let prod_id: usize = index.into();
        let prod_name = db.prod_name(index);
        (prod_id, (prod_name, create_ir_state_name(graph, s), s.get_type()))
      } else {
        panic!(
          "Invalid production type: {:?}  {}",
          s.get_symbol(),
          s.get_symbol().debug_string(db)
        )
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

  goto.name =
    format!("{}", create_ir_state_name(graph, state)).intern(db.string_store());

  SherpaResult::Ok((state.get_id(), goto))
}

fn convert_state_to_ir(
  j: &mut Journal,
  graph: &Graph,
  state: &State,
  successors: &Set<&State>,
  entry_name: &str,
  goto_state_id: Option<IString>,
) -> SherpaResult<Vec<(StateId, Box<ParseState>)>> {
  let is_scanner = graph.is_scan();
  let state_id = state.get_id();
  let db: &ParserDatabase = graph.get_db();

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

    let mut classes = classify_successors(successors);

    let mut tokens = OrderedSet::new();

    build_branch(&mut w, graph, &mut classes, goto_state_id, &mut tokens);

    let mut state = Box::new(create_ir_state(graph, w, state)?);
    if !tokens.is_empty() {
      state.tokens = Some(tokens);
    }

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
      StateType::AssignAndFollow(sym_id) | StateType::AssignToken(sym_id) => {
        let bytecode_id = sym_id.to_index();
        w.write(&format!("set-tok {bytecode_id}"))?;
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
          (entry_name.to_string() + "_then").intern(db.string_store());
      } else {
        base_state.name = (base_state.name.to_string(db.string_store())
          + "_then")
          .intern(db.string_store());
      }

      w.w(" then goto ")?.w(&base_state.name.to_string(db.string_store()))?;

      out.push((state_id.to_post_reduce(), base_state));
    }

    let mut ir_state = create_ir_state(graph, w, state)?;

    if state_id.is_root() {
      ir_state.name = entry_name.intern(db.string_store());
    }

    out.push((state_id, Box::new(ir_state)));
  } else if let Some(mut base_state) = base_state {
    if state_id.is_root() {
      base_state.name = entry_name.intern(db.string_store());
    }
    out.push((state_id, base_state));
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
    state.debug_string(db),
    graph.__debug_string__()
  );

  SherpaResult::Ok(out)
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

fn build_branch(
  mut w: &mut CodeWriter<Vec<u8>>,
  graph: &Graph,
  branches: &mut VecDeque<(InputType, Set<&State>)>,
  goto_state_id: Option<IString>,
  tokens: &mut OrderedSet<DBTokenData>,
) {
  let is_scanner = graph.is_scan();
  let db = graph.get_db();

  if let Some((input_type, (successors))) = branches.pop_front() {
    if matches!(input_type, InputType::Default) {
      let successor = successors.into_iter().next().unwrap();

      w =
        w + build_body(successor, graph, goto_state_id, tokens).join(" then ");
    } else {
      w = (w + "\nmatch: " + input_type.type_name() + " {").indent();

      for s in successors {
        let sym = s.get_symbol();

        if input_type == InputType::Token {
          //TODO: Add skip at this point as well.
          //  tokens.insert(db.sym_data())
        }

        let s_type = s.get_type();
        w = w + "\n\n( " + sym.to_state_val().to_string() + " ){ ";
        w =
          w + build_body(s, graph, goto_state_id, tokens).join(" then ") + " }";
      }

      if !branches.is_empty() {
        w = w + "\n\ndefault { ";
        build_branch(w, graph, branches, goto_state_id, tokens);
        w = w + " }";
      }

      w.dedent() + "\n}";
    }
  }
}

fn build_body(
  successor: &State,
  graph: &Graph,
  goto_state_id: Option<IString>,
  tokens: &mut OrderedSet<DBTokenData>,
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
    StateType::AssignAndFollow(sym_id) | StateType::AssignToken(sym_id) => {
      let tok_id = db.tok_id(sym_id);

      let string: String = "set-tok ".to_string() + &tok_id.to_string();

      if matches!(s_type, StateType::AssignAndFollow(..)) {
        body_string.push(string);
        true
      } else {
        body_string.push(string);
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
  format!(
    "reduce {} symbols to {prod_id} with rule {rule_id}",
    rule.symbols.len(),
  )
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
  format!(
    "{}_{:0>6X}",
    graph.is_scan().then_some("scanner").unwrap_or("parser"),
    state.get_hash(),
  )
}

pub(super) fn create_ir_state(
  graph: &Graph,
  mut w: CodeWriter<Vec<u8>>,
  state: &State,
) -> SherpaResult<ParseState> {
  let db = graph.get_db();

  let ir_state = ParseState {
    comment: format!("{}", state.debug_string(db),),
    code:    unsafe { String::from_utf8_unchecked(w.into_output()) },
    name:    create_ir_state_name(graph, state)
      .intern(graph.get_db().string_store()),
    ast:     SherpaResult::None,
    tokens:  None,
  };

  SherpaResult::Ok(ir_state)
}
