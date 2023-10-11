//! Functions for translating parse graphs into Sherpa IR code.
use crate::{
  compile::build_graph::graph::{GraphIterator, GraphStateReference, PeekGroup, StateId, StateType},
  journal::Journal,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
  writer::code_writer::CodeWriter,
};
use sherpa_rust_runtime::{types::bytecode::MatchInputType, utf8::lookup_table::CodePointClass};

use super::build_graph::{
  flow::resolve_token_assign_id,
  graph::{GotoGraphStateRef, GraphStateRef},
};

pub(crate) fn build_ir<'a, 'db: 'a>(
  j: &mut Journal,
  mut iter: GraphIterator<'a, 'db>,
  entry_name: IString,
) -> SherpaResult<Array<Box<ParseState>>> {
  debug_assert!(entry_name.as_u64() != 0);

  let mut output = OrderedMap::<StateId, Box<ParseState>>::new();

  while let Some((state, successors)) = iter.next() {
    let goto_name = if let Some(goto) = state.get_goto_state() {
      let goto_pair = convert_nonterm_shift_state_to_ir(j, goto, successors)?;
      let out = Some(goto_pair.1.hash_name.clone());
      output.insert(goto_pair.0, goto_pair.1);
      out
    } else {
      None
    };

    for (id, ir_state) in convert_state_to_ir(j, state, successors, entry_name, goto_name)? {
      output.entry(id).or_insert(ir_state);
    }
  }
  #[cfg(debug_assertions)]
  debug_assert!(!output.is_empty(), "This graph did not yield any states! \n{}", iter.graph._debug_string_());

  j.report_mut().wrap_ok_or_return_errors(output.into_values().collect())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SType {
  GotoSuccessors,
  SymbolSuccessors,
}

const GRAPH_STATE_NONE: Option<GraphStateRef> = None;

fn convert_nonterm_shift_state_to_ir<'graph, 'db: 'graph>(
  _j: &mut Journal,
  state: GotoGraphStateRef<'graph, 'db>,
  successors: &OrderedSet<GraphStateRef<'graph, 'db>>,
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

  goto.hash_name = create_ir_state_name(GRAPH_STATE_NONE, &state).intern(db.string_store());

  SherpaResult::Ok((state.get_id(), goto))
}

fn convert_state_to_ir<'graph, 'db: 'graph>(
  _j: &mut Journal,
  state: GraphStateRef<'graph, 'db>,
  successors: &OrderedSet<GraphStateRef<'graph, 'db>>,
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

      let mut classes = classify_successors(successors, db);

      let scanner_data = add_match_expr(&mut w, state, &mut classes, &gotos);

      Some(Box::new(create_ir_state(w, &state, scanner_data)?))
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
          base_state.hash_name = (entry_name.to_string(s_store) + "_then").intern(s_store);
        } else {
          base_state.hash_name = (base_state.hash_name.to_string(s_store) + "_then").intern(s_store);
        }

        w.w(" then goto ")?.w(&base_state.hash_name.to_string(s_store))?;

        out.push((state_id.to_post_reduce(), base_state));
      }

      let mut ir_state = create_ir_state(w, &state, None)?;

      if state_id.is_root() {
        ir_state.hash_name = entry_name;
        ir_state.root = true;
      }

      out.push((state_id, Box::new(ir_state)));
    } else if let Some(mut base_state) = base_state {
      if state_id.is_root() {
        base_state.hash_name = entry_name;
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

fn add_tok_expr<'graph, 'db: 'graph>(
  state: GraphStateRef<'graph, 'db>,
  successors: &OrderedSet<GraphStateRef<'graph, 'db>>,
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

fn classify_successors<'graph, 'db: 'graph>(
  successors: &OrderedSet<GraphStateRef<'graph, 'db>>,
  _db: &'db ParserDatabase,
) -> Queue<((u32, MatchInputType), OrderedSet<GraphStateRef<'graph, 'db>>)> {
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

fn add_match_expr<'graph, 'db: 'graph>(
  mut w: &mut CodeWriter<Vec<u8>>,
  state: GraphStateRef<'graph, 'db>,
  branches: &mut Queue<((u32, MatchInputType), OrderedSet<GraphStateRef<'graph, 'db>>)>,
  goto_state_id: &OrderedMap<StateId, IString>,
) -> Option<(IString, OrderedSet<PrecedentDBTerm>)> {
  let graph = state.graph;
  let db = state.graph.get_db();

  if let Some(((_, input_type), successors)) = branches.pop_front() {
    if matches!(input_type, MatchInputType::Default) {
      let successor = successors.into_iter().next().unwrap();

      let string = build_body(state, successor, goto_state_id).join(" then ");

      if !string.is_empty() {
        let _ = w + string;
      }

      None
    } else {
      let (symbols, skipped) = if input_type == MatchInputType::Token {
        let syms = state.get_symbols().map(|s| s.clone()).unwrap_or_default();

        debug_assert!(!syms.is_empty());

        // get a collection of skipped symbols.
        let skipped = if let Some(test) = state.get_peek_resolve_items() {
          test.map(|(_, PeekGroup { items, .. })| items).flatten().filter_map(|i| i.get_skipped()).flatten().collect::<Vec<_>>()
        } else {
          state.get_kernel_items().iter().filter_map(|i| i.get_skipped()).flatten().collect::<Vec<_>>()
        }
        .into_iter()
        .filter_map(|s| {
          let id = s.tok_db_key().unwrap();
          (!syms.contains(&id)).then_some(id)
        })
        .collect::<OrderedSet<_>>();

        // Build scanner collection
        let mut symbols = OrderedSet::default();

        for state in &successors {
          symbols.insert(PrecedentDBTerm::from(state.get_symbol(), db, false));
        }

        for sym in &skipped {
          symbols.insert((*sym, 0, true).into());
        }

        let skipped = if successors.iter().all(|s| matches!(s.get_type(), StateType::Reduce(..))) { None } else { Some(skipped) };

        (Some((ParseState::get_interned_scanner_name(&symbols, graph.get_db().string_store()), symbols)), skipped)
      } else {
        (None, None)
      };

      w = w + "\nmatch: " + input_type.as_str();

      if let Some((name, _)) = &symbols {
        w = w + ":" + name.to_str(db.string_store()).as_str();
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
      if let Some(skipped) = skipped {
        if !skipped.is_empty() {
          let vals = skipped.iter().map(|v| v.to_val().to_string()).collect::<Array<_>>().join(" | ");
          if vals.len() > 0 {
            w = w + "\n( " + vals + " ){ " + peeking.then_some("peek-skip tok").unwrap_or("shift-skip tok") + " }";
          }
        }
      }

      if !branches.is_empty() {
        w = (w + "\n\ndefault { ").indent();
        add_match_expr(w, state, branches, goto_state_id);
        w = w + " }";
        w = w.dedent();
      }

      let _ = w.dedent() + "\n}";

      symbols
    }
  } else {
    None
  }
}

fn build_body<'graph, 'db: 'graph>(
  state: GraphStateRef<'graph, 'db>,
  successor: GraphStateRef<'graph, 'db>,
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

pub(super) fn create_ir_state_name<'graph, 'db: 'graph>(
  origin_state: Option<impl GraphStateReference<'graph, 'db>>,
  target_state: &'graph impl GraphStateReference<'graph, 'db>,
) -> String {
  let graph = target_state.graph();
  if origin_state.is_some_and(|s| s.get_id() == target_state.get_id()) {
    "%%%%".to_string()
  } else if false {
    graph.get_state_name(target_state.get_id())
  } else if target_state.get_id().is_goto() {
    "g_".to_string() + &target_state.get_hash().to_string()
  } else {
    graph.is_scanner().then_some("s").unwrap_or("p").to_string() + "_" + &target_state.get_hash().to_string()
  }
}

pub(super) fn create_ir_state<'graph, 'db: 'graph>(
  w: CodeWriter<Vec<u8>>,
  state: &'graph impl GraphStateReference<'graph, 'db>,
  scanner: Option<(IString, OrderedSet<PrecedentDBTerm>)>,
) -> SherpaResult<ParseState> {
  let ir_state = ParseState {
    code: w.to_string(),
    hash_name: create_ir_state_name(GRAPH_STATE_NONE, state).intern(state.graph().get_db().string_store()),
    scanner,
    ..Default::default()
  };

  SherpaResult::Ok(ir_state)
}
