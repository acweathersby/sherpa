//! Functions for translating parse graphs into Sherpa IR code.

use crate::{
  compile::states::{
    build_graph::graph::{GraphStateReference, ReversedGraph, ScannerData, StateId, StateType},
    build_states::get_scanner_name,
  },
  journal::Journal,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
  writer::code_writer::CodeWriter,
  SherpaGraph,
};
use sherpa_rust_runtime::{types::bytecode::MatchInputType, utf8::lookup_table::CodePointClass};

type States = OrderedMap<IString, Box<ParseState>>;

use super::super::states::build_graph::{
  flow::resolve_token_assign_id,
  graph::{GotoGraphStateRef, GraphStateRef},
};

pub fn build_ir_from_graph(graph: &SherpaGraph) -> SherpaResult<(ParserClassification, ParseStatesMap)> {
  let SherpaGraph { j, db, config, parsers, scanners } = graph;

  let mut j = j.transfer();
  j.set_active_report("States Compile", crate::ReportType::AnyNonTermCompile);

  let mut classification = ParserClassification::default();

  let mut states = States::default();

  // Build entry states -------------------------------------------------------
  for entry in db.entry_points().iter().filter(|i| config.EXPORT_ALL_NONTERMS || i.is_export) {
    let ir = build_entry_ir(entry, db)?;
    for state in ir {
      states.insert(state.guid_name, state);
    }
  }

  // Build parser_states ------------------------------------------------------
  for state in parsers.iter() {
    classification |= state.classification;
    if let Some(graph) = state.graph.as_ref() {
      states.extend(build_ir(&mut j, graph, state.name)?.into_iter().map(|s| (s.guid_name, s)));
    }
  }

  // Build scanner_states -----------------------------------------------------
  for state in scanners.iter() {
    if let Some(graph) = state.graph.as_ref() {
      states.extend(build_ir(&mut j, graph, state.name)?.into_iter().map(|s| (s.guid_name, s)));
    }
  }

  // Build custom states ------------------------------------------------------
  for i in 0..db.nonterms_len() {
    let nonterm_key = DBNonTermKey::from(i);
    if let Some(custom_state) = db.custom_state(nonterm_key) {
      let name = db.nonterm_guid_name(nonterm_key);

      let state = ParseState {
        guid_name:     name,
        comment:       "Custom State".into(),
        code:          custom_state.tok.to_string(),
        ast:           Some(Box::new(custom_state.clone())),
        compile_error: None,
        scanner:       None,
        root:          true,
        precedence:    0,
      };
      states.insert(name, Box::new(state));
    }
  }

  for (_, state) in &mut states {
    state.build_ast(db)?;
  }

  SherpaResult::Ok((classification, states))
}

pub(crate) fn build_entry_ir(
  DBEntryPoint {
    nonterm_name: nterm_name,
    nonterm_entry_name: nterm_entry_name,
    nonterm_exit_name: nterm_exit_name,
    ..
  }: &DBEntryPoint,
  db: &ParserDatabase,
) -> SherpaResult<Array<Box<ParseState>>> {
  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "push " + nterm_exit_name.to_string(db.string_store());
  let _ = (&mut w) + " then goto " + nterm_name.to_string(db.string_store());

  let entry_state = ParseState {
    code: w.to_string(),
    guid_name: *nterm_entry_name,
    ..Default::default()
  };

  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "accept";

  let exit_state = ParseState { code: w.to_string(), guid_name: *nterm_exit_name, ..Default::default() };

  SherpaResult::Ok(Array::from_iter([Box::new(entry_state), Box::new(exit_state)]))
}

pub(crate) fn build_ir(j: &mut Journal, graph: &ReversedGraph, entry_name: IString) -> SherpaResult<Array<Box<ParseState>>> {
  debug_assert!(entry_name.as_u64() != 0);

  let mut output = OrderedMap::<StateId, Box<ParseState>>::new();

  let mut iter = graph.iter();

  while let Some((state, scanner, successors)) = iter.next() {
    let goto_name = if let Some(goto) = state.get_goto_state() {
      let goto_pair = convert_nonterm_shift_state_to_ir(j, goto, &successors)?;
      let out = Some(goto_pair.1.guid_name.clone());
      output.insert(goto_pair.0, goto_pair.1);
      out
    } else {
      None
    };

    for (id, ir_state) in convert_state_to_ir(j, state, scanner, &successors, entry_name, goto_name)? {
      output.entry(id).or_insert(ir_state);
    }
  }
  #[cfg(debug_assertions)]
  debug_assert!(!output.is_empty(), "This graph did not yield any states! \n");

  j.report_mut().wrap_ok_or_return_errors(output.into_values().collect())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SType {
  GotoSuccessors,
  SymbolSuccessors,
}

const GRAPH_STATE_NONE: Option<GraphStateRef> = None;

fn convert_nonterm_shift_state_to_ir<'graph: 'graph>(
  _j: &mut Journal,
  state: GotoGraphStateRef<'graph>,
  successors: &Vec<GraphStateRef<'graph>>,
) -> SherpaResult<(StateId, Box<ParseState>)> {
  let db = state.graph.get_db();

  let successors =
    successors.iter().filter(|s| matches!(s.get_type(), StateType::NonTerminalComplete | StateType::NonTerminalShiftLoop));

  let mut w = CodeWriter::new(vec![]);

  (&mut w + "match: " + MatchInputType::NONTERMINAL_STR + " {").increase_indent();

  for (bc_id, (_nterm_name, s_name, transition_type)) in successors
    .into_iter()
    .map(|s| {
      if let SymbolId::DBNonTerminal { key: index } = s.get_symbol().sym() {
        let nterm: usize = index.into();
        let nterm_name = db.nonterm_guid_name(index);
        (nterm, (nterm_name, create_ir_state_name(GRAPH_STATE_NONE, s), s.get_type()))
      } else {
        #[cfg(debug_assertions)]
        unreachable!("Invalid non-terminal type: {:?}  {}", s.get_symbol().sym(), s.get_symbol().sym().debug_string(db));
        #[cfg(not(debug_assertions))]
        unreachable!()
      }
    })
    .collect::<OrderedMap<_, _>>()
  {
    let bc_id = bc_id.to_string();
    match transition_type {
      StateType::NonTerminalShiftLoop => {
        let _ = (&mut w) + "\n( " + bc_id + " ){ push %%%% then goto " + s_name + " }";
      }
      StateType::NonTerminalComplete => {
        let _ = (&mut w) + "\n( " + bc_id + " ){ pass }";
      }
      _ => unreachable!(),
    }
  }

  let _ = w.dedent() + "\n}";

  let mut goto = Box::new(create_ir_state(w, &state, None)?);

  goto.guid_name = create_ir_state_name(GRAPH_STATE_NONE, &state).intern(db.string_store());

  SherpaResult::Ok((state.get_id(), goto))
}

fn convert_state_to_ir<'graph: 'graph>(
  _j: &mut Journal,
  state: GraphStateRef<'graph>,
  scanner_data: Option<&ScannerData>,
  successors: &Vec<GraphStateRef<'graph>>,
  entry_name: IString,
  goto_state_id: Option<IString>,
) -> SherpaResult<Vec<(StateId, Box<ParseState>)>> {
  let state_id = state.get_id();
  let db: &ParserDatabase = state.graph.get_db();
  let s_store = db.string_store();

  if matches!(state.get_type(), StateType::ForkInitiator) {
    let mut writer = CodeWriter::new(vec![]);
    let mut w = &mut writer;

    w = w + "fork {";

    for successor in successors {
      let name = create_ir_state_name(Some(state), successor);
      w = w + " " + &name;
    }

    _ = w + " }";

    Ok(vec![(state_id, Box::new(create_ir_state(writer, &state, None)?))])
  } else {
    let mut gotos = successors
      .iter()
      .filter_map(|goto_state| match goto_state.get_type() {
        StateType::ShiftFrom(s) => Some((s, create_ir_state_name(GRAPH_STATE_NONE, goto_state).intern(s_store))),
        _ => None,
      })
      .collect::<OrderedMap<_, _>>();

    debug_assert!(gotos.is_empty() || goto_state_id.is_none());

    if gotos.is_empty() {
      if let Some(goto_string) = goto_state_id {
        gotos.extend(successors.iter().map(|i| (i.id, goto_string)))
      }
    }

    let successor_groups = hash_group_btreemap(successors.clone(), |_, s| match s.get_type() {
      StateType::NonTerminalComplete | StateType::NonTerminalShiftLoop => SType::GotoSuccessors,
      _ => SType::SymbolSuccessors,
    });

    let base_state = if let Some(successors) = successor_groups.get(&SType::SymbolSuccessors) {
      let mut w = CodeWriter::new(vec![]);

      w.indent();

      add_tok_expr(state, successors, &mut w);

      let mut classes = classify_successors(successors);

      add_match_expr(&mut w, state, scanner_data, &mut classes, &gotos);

      Some(Box::new(create_ir_state(w, &state, scanner_data.cloned())?))
    } else {
      None
    };

    let mut out = vec![];

    if matches!(
      state.get_type(),
      StateType::CompleteToken | StateType::AssignAndFollow(..) | StateType::AssignToken(..) | StateType::Reduce(..)
    ) {
      let mut w = CodeWriter::new(vec![]);
      w.increase_indent();
      w.insert_newline()?;

      match state.get_type() {
        StateType::AssignAndFollow(tok_id) | StateType::AssignToken(tok_id) => {
          let _ = (&mut w) + "set-tok " + db.tok_val(tok_id).to_string();
        }
        StateType::CompleteToken => w.write("pass")?,

        StateType::Reduce(rule_id, completes) => {
          debug_assert!(!state.get_kernel_items().iter().any(|i| i.is_oos()));

          w.write(&create_rule_reduction(rule_id, db))?;

          if completes > 0 {
            let _ = (&mut w) + "pop " + completes.to_string();
          }
        }
        _ => unreachable!(),
      }

      if let Some(mut base_state) = base_state {
        if state_id.is_root() {
          base_state.guid_name = (entry_name.to_string(s_store) + "_then").intern(s_store);
        } else {
          base_state.guid_name = (base_state.guid_name.to_string(s_store) + "_then").intern(s_store);
        }

        w.w(" then goto ")?.w(&base_state.guid_name.to_string(s_store))?;

        out.push((state_id.to_post_reduce(), base_state));
      }

      let mut ir_state = create_ir_state(w, &state, None)?;

      if state_id.is_root() {
        ir_state.guid_name = entry_name;
        ir_state.root = true;
      }

      out.push((state_id, Box::new(ir_state)));
    } else if let Some(mut base_state) = base_state {
      if state_id.is_root() {
        base_state.guid_name = entry_name;
      }

      out.push((state_id, base_state));
    }
    #[cfg(debug_assertions)]
    debug_assert!(
      !out.is_empty()
        || matches!(
          state.get_type(),
          StateType::NonTerminalComplete
            | StateType::NonTermCompleteOOS
            | StateType::ScannerCompleteOOS
            | StateType::CompleteToken
            | StateType::AssignToken(..)
        ),
      "Graph state failed to generate ir states:\nSTATE\n\n {} \n\nGraph\n{}\n{}",
      state._debug_string_(),
      "", // state.graph._debug_string_(),
      successors.iter().map(|s| s._debug_string_()).collect::<Vec<_>>().join("\n\n")
    );

    Ok(out)
  }
}

fn add_tok_expr<'graph: 'graph>(
  state: GraphStateRef<'graph>,
  successors: &Vec<GraphStateRef<'graph>>,
  w: &mut CodeWriter<Vec<u8>>,
) {
  let db = state.graph.get_db();

  let token_set = successors.iter().filter_map(|s| match s.get_type() {
    StateType::AssignToken(tok) | StateType::AssignAndFollow(tok) => Some(tok),
    _ => None,
  });

  if let Some(tok_id) = resolve_token_assign_id(token_set, db) {
    (w + "set-tok " + db.tok_val(tok_id).to_string()).prime_join(" then ");
  }
}

fn classify_successors<'graph: 'graph>(
  successors: &Vec<GraphStateRef<'graph>>,
) -> Queue<((u32, MatchInputType), Vec<GraphStateRef<'graph>>)> {
  Queue::from_iter(
    hash_group_btree_iter(successors.iter().filter(|i| !matches!(i.get_type(), StateType::ShiftFrom(_))), |_, s| {
      if matches!(s.get_type(), StateType::CSTNodeAccept(_)) {
        (0, MatchInputType::CSTNode)
      } else {
        match s.get_symbol().sym() {
          SymbolId::EndOfFile { .. } => (1, MatchInputType::EndOfFile),
          SymbolId::Char { .. } => (2, MatchInputType::Byte),
          SymbolId::Codepoint { .. } => (3, MatchInputType::Codepoint),
          sym if sym.is_class() => (4, MatchInputType::Class),
          SymbolId::Any => (4, MatchInputType::Class),
          SymbolId::DBToken { .. } | SymbolId::DBNonTerminalToken { .. } => (5, MatchInputType::Token),
          SymbolId::Default => (6, MatchInputType::Default),
          _sym => {
            #[cfg(debug_assertions)]
            unreachable!("{_sym:?} {}", s._debug_string_());
            #[cfg(not(debug_assertions))]
            unreachable!()
          }
        }
      }
    })
    .into_iter(),
  )
}

fn add_match_expr<'graph: 'graph>(
  mut w: &mut CodeWriter<Vec<u8>>,
  state: GraphStateRef<'graph>,
  scanner_data: Option<&ScannerData>,
  branches: &mut Queue<((u32, MatchInputType), Vec<GraphStateRef<'graph>>)>,
  goto_state_id: &OrderedMap<StateId, IString>,
) {
  let db = state.graph.get_db();

  if let Some(((_, input_type), successors)) = branches.pop_front() {
    if matches!(input_type, MatchInputType::Default) {
      let successor = successors.into_iter().next().unwrap();

      let string = build_body(state, successor, goto_state_id).join(" then ");

      if !string.is_empty() {
        let _ = w + string;
      }
    } else {
      w = w + "\nmatch: " + input_type.as_str();

      if input_type == MatchInputType::Token {
        w = w
          + ":"
          + get_scanner_name(scanner_data.expect("Token matches should have accompanying scanner"), db)
            .to_str(db.string_store())
            .as_str();
      }

      w = (w + " {").indent();

      // Sort successors
      let peeking = successors.iter().any(|s| matches!(s.get_type(), StateType::PeekEndComplete(_) | StateType::Peek(_)));

      for (state_val, s) in successors.iter().map(|s| (s.get_symbol().sym().to_state_val(db), s)).collect::<OrderedMap<_, _>>() {
        if state_val == CodePointClass::Any as u32 {
          w = w + "\n\ndefault { ";
          w = w + build_body(state, *s, goto_state_id).join(" then ") + " }";
        } else {
          w = w + "\n\n( " + state_val.to_string() + " ){ ";
          w = w + build_body(state, *s, goto_state_id).join(" then ") + " }";
        }
      }

      // Add skips

      if input_type == MatchInputType::Token {
        let scanner = scanner_data.expect("Token matches should have accompanying scanner");
        if !scanner.skipped.is_empty() {
          let vals = scanner.skipped.iter().map(|v| v.to_val().to_string()).collect::<Array<_>>().join(" | ");
          if vals.len() > 0 {
            w = w + "\n( " + vals + " ){ " + peeking.then_some("peek-skip tok").unwrap_or("shift-skip tok") + " }";
          }
        }
      }

      if !branches.is_empty() {
        w = (w + "\n\ndefault { ").indent();
        add_match_expr(w, state, scanner_data, branches, goto_state_id);
        w = w + " }";
        w = w.dedent();
      }

      let _ = w.dedent() + "\n}";
    }
  }
}

fn build_body<'graph: 'graph>(
  state: GraphStateRef<'graph>,
  successor: GraphStateRef<'graph>,
  goto_state_id: &OrderedMap<StateId, IString>,
) -> Vec<String> {
  let graph = state.graph;
  let is_scanner = state.graph.is_scanner();
  let mut body_string: Vec<String> = Array::new();
  let s_type = successor.get_type();
  let db = graph.get_db();

  #[derive(PartialEq, Eq)]
  enum ParserFlow {
    CONTINUE,
    HALT,
  }

  use ParserFlow::*;

  if CONTINUE
    == match s_type {
      StateType::Shift | StateType::KernelShift => {
        let scan_expr = successor.get_symbol().sym().is_linefeed().then_some("shift char then set-line").unwrap_or("shift char");
        body_string.push(is_scanner.then_some(scan_expr).unwrap_or("shift tok").into());
        CONTINUE
      }
      StateType::PeekEndComplete(_) => {
        debug_assert!(!is_scanner, "Peek states should not be present in graph");
        body_string.push("reset tok".into());
        CONTINUE
      }
      StateType::Peek(_) => {
        debug_assert!(!is_scanner, "Peek states should not be present in graph");
        body_string.push("peek tok".into());
        CONTINUE
      }
      StateType::NonTermCompleteOOS => {
        debug_assert!(!is_scanner, "NonTermCompleteOOS states should only exist in normal parse graphs");
        body_string.push("pop 1".into());
        HALT
      }
      StateType::ScannerCompleteOOS => {
        debug_assert!(is_scanner, "ScannerCompleteOOS states should only exist in scanner parse graphs");
        body_string.push("pass".into());
        HALT
      }
      StateType::Reduce(rule_id, completes) => {
        debug_assert!(!successor.get_kernel_items().iter().any(|i| i.is_oos()));

        body_string.push(create_rule_reduction(rule_id, db));

        if completes > 0 {
          body_string.push("pop ".to_string() + &completes.to_string());
        }

        HALT
      }
      StateType::Follow => CONTINUE,
      StateType::AssignToken(..) | StateType::CompleteToken => {
        body_string.push("pass".into());
        HALT
      }
      _ => CONTINUE,
    }
  {
    // Add goto expressions

    match (&goto_state_id.get(&successor.id), s_type) {
      // Kernel calls can bypass gotos.
      (_, StateType::KernelCall(..)) | (_, StateType::KernelShift) => {}
      (Some(gt), _) => body_string.push("push ".to_string() + &gt.to_string(db.string_store())),
      _ => {}
    }

    match s_type {
      //Ensure non-terminal calls are immediately called before any other
      // gotos.
      StateType::KernelCall(nterm) | StateType::InternalCall(nterm) => {
        body_string.push("push ".to_string() + &create_ir_state_name(Some(state), &successor));
        body_string.push("goto ".to_string() + &db.nonterm_guid_name(nterm).to_string(db.string_store()));
      }
      _ => {
        body_string.push("goto ".to_string() + &create_ir_state_name(Some(state), &successor));
      }
    }
  }

  body_string
}

fn create_rule_reduction(rule_id: DBRuleKey, db: &ParserDatabase) -> String {
  let rule = db.rule(rule_id);
  let nterm = db.rule_nonterm(rule_id);
  let nterm: usize = nterm.into();
  let rule_id: usize = rule_id.into();
  let mut w = CodeWriter::new(vec![]);

  let _ = &mut w + "reduce " + rule.symbols.len().to_string();
  let _ = &mut w + " symbols to " + nterm.to_string();
  let _ = &mut w + " with rule " + rule_id.to_string();

  w.to_string()
}

pub(super) fn create_ir_state_name<'graph: 'graph>(
  origin_state: Option<impl GraphStateReference<'graph>>,
  target_state: &'graph impl GraphStateReference<'graph>,
) -> String {
  let graph = target_state.graph();
  if origin_state.is_some_and(|s| s.get_id() == target_state.get_id()) {
    "%%%%".to_string()
  } else if target_state.get_id().is_goto() {
    "g_".to_string() + &target_state.get_hash().to_string()
  } else {
    graph.is_scanner().then_some("s").unwrap_or("p").to_string() + "_" + &target_state.get_hash().to_string()
  }
}

pub(super) fn create_ir_state<'graph: 'graph>(
  w: CodeWriter<Vec<u8>>,
  state: &'graph impl GraphStateReference<'graph>,
  scanner: Option<ScannerData>,
) -> SherpaResult<ParseState> {
  let ir_state = ParseState {
    code: w.to_string(),
    guid_name: create_ir_state_name(GRAPH_STATE_NONE, state).intern(state.graph().get_db().string_store()),
    scanner,
    ..Default::default()
  };

  SherpaResult::Ok(ir_state)
}
