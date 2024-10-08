//! Functions for translating parse graphs into Radlr IR code.

use crate::{
  compile::states::build_graph::graph::{
    create_scanner_name,
    GraphNode,
    Graphs,
    IRPrecursorGroup,
    ScannerData,
    SharedGraphNode,
    StateId,
    StateType,
  },
  types::{worker_pool::WorkerPool, *},
  utils::{hash_group_btree_iter, hash_group_btreemap},
  writer::code_writer::CodeWriter,
};
use radlr_rust_runtime::{types::bytecode::MatchInputType, utf8::lookup_table::CodePointClass};
use std::{
  collections::BTreeMap,
  sync::{Arc, RwLock},
};

use super::super::states::build_graph::flow::resolve_token_assign_id;

pub(crate) fn build_ir_concurrent<T: WorkerPool>(
  pool: &T,
  graphs: Arc<Graphs>,
  config: ParserConfig,
  db: &Arc<ParserDatabase>,
) -> RadlrResult<(ParserClassification, ParseStatesMap)> {
  let states = Arc::new(RwLock::new(OrderedMap::new()));
  let classification = Arc::new(RwLock::new(ParserClassification::default()));

  pool.run(|num_of_threads| {
    let graphs = graphs.clone();
    let db = db.clone();
    let final_states = states.clone();
    let classification = classification.clone();

    move |thread_id| {
      let chunk_size = graphs.successors.len().div_ceil(num_of_threads);
      let chunk_start = chunk_size * thread_id;
      let chunk_end = chunk_start + chunk_size;
      let mut inner_classification = classification.read().unwrap().clone();

      let mut states = Default::default();

      for (index, (par, children)) in graphs.successors.iter().enumerate() {
        if index >= chunk_end {
          break;
        }

        if index >= chunk_start {
          if !(par.get_root().invalid.load(std::sync::atomic::Ordering::Acquire)) {
            let precursor = IRPrecursorGroup {
              node:          par.clone(),
              successors:    children.iter().map(|n| (n.hash_id, n.clone())).collect(),
              non_terminals: graphs.state_nonterms.get(&par.hash_id).cloned(),
              root_name:     par.is_root().then(|| par.root_data.root_name),
            };

            if (!precursor.node.is_scanner()) {
              inner_classification |= precursor.node.get_classification();
            }

            process_precursor(&precursor, &mut states)?;
          }
        }
      }

      for (_, state) in &mut states {
        state.build_ast(&db)?;
      }

      // Perform base level optimizations. Can we do this concurrently?

      final_states.write()?.extend(states);
      *classification.write()? |= inner_classification;
      Ok(())
    }
  })?;

  let mut states = RwLock::into_inner(Arc::into_inner(states).expect("Should have no other owners"))?;
  let classification = RwLock::into_inner(Arc::into_inner(classification).expect("Should have no other owners"))?;

  // Build entry states -------------------------------------------------------
  for entry in db.entry_points().iter().filter(|i| config.EXPORT_ALL_NONTERMS || i.is_export) {
    let ir = build_entry_ir(entry, db)?;
    for mut state in ir {
      state.build_ast(db)?;
      states.insert(state.guid_name, state);
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

  Ok((classification, states))
}

pub(crate) fn process_precursor(
  node_state: &IRPrecursorGroup,
  states: &mut BTreeMap<IString, Box<ParseState>>,
) -> Result<(), RadlrError> {
  let entry_name = node_state.root_name.unwrap_or_default();
  let state = &node_state.node;

  let goto_name = if let Some(_) = node_state.non_terminals.as_ref() {
    let mut precursor = IRPrecursorGroup { ..node_state.clone() };

    precursor.node = precursor.node.to_goto();

    let goto_pair = convert_nonterm_shift_state_to_ir(&precursor)?;
    let out = Some(goto_pair.1.guid_name.clone());
    states.insert(goto_pair.1.guid_name, goto_pair.1);
    out
  } else {
    None
  };

  for (_, ir_state) in convert_state_to_ir(state.symbol_set.as_ref(), node_state, entry_name, goto_name)? {
    states.entry(ir_state.guid_name).or_insert(ir_state);
  }

  Ok(())
}

pub(crate) fn build_entry_ir(
  DBEntryPoint {
    nonterm_name: nterm_name,
    nonterm_entry_name: nterm_entry_name,
    nonterm_exit_name: nterm_exit_name,
    ..
  }: &DBEntryPoint,
  db: &ParserDatabase,
) -> RadlrResult<Array<Box<ParseState>>> {
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

  RadlrResult::Ok(Array::from_iter([Box::new(entry_state), Box::new(exit_state)]))
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SType {
  GotoSuccessors,
  SymbolSuccessors,
}

fn convert_nonterm_shift_state_to_ir(precursor: &IRPrecursorGroup) -> RadlrResult<(StateId, Box<ParseState>)> {
  let db = &precursor.node.db;

  let successors =
    precursor.successors.values().filter(|s| matches!(s.ty, StateType::NonTerminalComplete | StateType::NonTerminalShiftLoop));

  let mut w = CodeWriter::new(vec![]);

  (&mut w + "match: " + MatchInputType::NONTERMINAL_STR + " {").increase_indent();

  for (bc_id, (_nterm_name, s_name, transition_type)) in successors
    .into_iter()
    .map(|s| {
      if let SymbolId::DBNonTerminal { key: index } = s.sym.sym() {
        let nterm: usize = index.into();
        let nterm_name = db.nonterm_guid_name(index);
        (nterm, (nterm_name, create_ir_state_name(None, s), s.ty))
      } else {
        #[cfg(debug_assertions)]
        unreachable!("Invalid non-terminal type: {:?}  {}", s.sym.sym(), s.sym.sym().debug_string(db));
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

  let mut goto = Box::new(create_ir_state(w, &precursor.node, None)?);

  goto.guid_name = create_ir_state_name(None, &precursor.node).intern(db.string_store());

  RadlrResult::Ok((precursor.node.id, goto))
}

fn convert_state_to_ir<'graph: 'graph>(
  scanner_data: Option<&Arc<ScannerData>>,
  precursor: &IRPrecursorGroup,
  entry_name: IString,
  goto_state_id: Option<IString>,
) -> RadlrResult<Vec<(StateId, Box<ParseState>)>> {
  let state = &precursor.node;
  let state_id = state.id();
  let db = &precursor.node.db;
  let s_store = db.string_store();

  if matches!(state.ty, StateType::ForkInitiator) {
    let mut writer = CodeWriter::new(vec![]);
    let mut w = &mut writer;

    w = w + "fork {";

    for successor in precursor.successors.values() {
      let name = create_ir_state_name(Some(state), successor);
      w = w + " " + &name;
    }

    _ = w + " }";

    Ok(vec![(state_id, Box::new(create_ir_state(writer, &state, None)?))])
  } else {
    let mut gotos = precursor
      .successors
      .values()
      .filter_map(|goto_state| match goto_state.ty {
        StateType::LLShiftFrom(s) => Some((s, create_ir_state_name(None, goto_state).intern(s_store))),
        _ => None,
      })
      .collect::<OrderedMap<_, _>>();

    debug_assert!(gotos.is_empty() || goto_state_id.is_none());

    if gotos.is_empty() {
      if let Some(goto_string) = goto_state_id {
        gotos.extend(precursor.successors.values().map(|i| (i.id, goto_string)))
      }
    }

    let successor_groups = hash_group_btreemap(precursor.successors.values().cloned().collect::<Vec<_>>(), |_, s| match s.ty {
      StateType::NonTerminalComplete | StateType::NonTerminalShiftLoop => SType::GotoSuccessors,
      _ => SType::SymbolSuccessors,
    });

    let base_state = if let Some(successors) = successor_groups.get(&SType::SymbolSuccessors) {
      let mut w = CodeWriter::new(vec![]);

      w.indent();

      add_tok_expr(&precursor.node, successors, &mut w);

      let mut classes = classify_successors(successors);

      add_match_expr(&mut w, state, scanner_data, &mut classes, &gotos);

      Some(Box::new(create_ir_state(w, &state, scanner_data)?))
    } else {
      None
    };

    let mut out = vec![];

    if matches!(
      state.ty,
      StateType::CompleteToken | StateType::AssignAndFollow(..) | StateType::AssignToken(..) | StateType::Reduce(..)
    ) {
      let mut w = CodeWriter::new(vec![]);
      w.increase_indent();
      w.insert_newline()?;

      match state.ty {
        StateType::AssignAndFollow(tok_id) | StateType::AssignToken(tok_id) => {
          let _ = (&mut w) + "set-tok " + db.tok_val(tok_id).to_string();
        }
        StateType::CompleteToken => w.write("pass")?,

        StateType::Reduce(rule_id, completes) => {
          debug_assert!(!state.kernel_items().iter().any(|i| i.is_oos()));

          w.write(&create_rule_reduction(rule_id, db))?;

          if completes > 0 {
            let _ = (&mut w) + "pop " + completes.to_string();
          }
        }
        _ => unreachable!(),
      }

      if let Some(mut base_state) = base_state {
        if state.is_root() {
          base_state.guid_name = (entry_name.to_string(s_store) + "_then").intern(s_store);
        } else {
          base_state.guid_name = (base_state.guid_name.to_string(s_store) + "_then").intern(s_store);
        }

        w.w(" then goto ")?.w(&base_state.guid_name.to_string(s_store))?;

        out.push((state_id.to_post_reduce(), base_state));
      }

      let mut ir_state = create_ir_state(w, &state, None)?;

      if state.is_root() {
        ir_state.guid_name = entry_name;
        ir_state.root = true;
      }

      out.push((state_id, Box::new(ir_state)));
    } else if let Some(mut base_state) = base_state {
      if state.is_root() {
        base_state.guid_name = entry_name;
      }

      out.push((state_id, base_state));
    }

    let non_leaf_empty_state = out.is_empty()
      && !matches!(
        state.ty,
        StateType::NonTerminalComplete
          | StateType::NonTermCompleteOOS
          | StateType::ScannerCompleteOOS
          | StateType::CompleteToken
          | StateType::AssignToken(..)
      );

    if non_leaf_empty_state {
      Err(RadlrError::Text(format!("Created a state without children that is not a leaf\n State:\n{:?}", state)))
    } else {
      Ok(out)
    }
  }
}

fn add_tok_expr(state: &GraphNode, successors: &Vec<SharedGraphNode>, w: &mut CodeWriter<Vec<u8>>) {
  let db = &state.db;

  let token_set = successors.iter().filter_map(|s| match s.ty {
    StateType::AssignToken(tok) | StateType::AssignAndFollow(tok) => Some(tok),
    _ => None,
  });

  if let Some(tok_id) = resolve_token_assign_id(token_set, db) {
    (w + "set-tok " + db.tok_val(tok_id).to_string()).prime_join(" then ");
  }
}

fn classify_successors(successors: &Vec<SharedGraphNode>) -> Queue<((u32, MatchInputType), Vec<SharedGraphNode>)> {
  Queue::from_iter(
    hash_group_btree_iter(successors.iter().cloned().filter(|i| !matches!(i.ty, StateType::LLShiftFrom(_))), |_, s| {
      if matches!(s.ty, StateType::CSTNodeAccept(_)) {
        (0u32, MatchInputType::CSTNode)
      } else {
        match s.sym.sym() {
          SymbolId::EndOfFile { .. } => (1, MatchInputType::EndOfFile),
          SymbolId::Char { .. } => (2, MatchInputType::Byte),
          SymbolId::Codepoint { .. } => (3, MatchInputType::Codepoint),
          sym if sym.is_class() => (4, MatchInputType::Class),
          SymbolId::Any => (4, MatchInputType::Class),
          SymbolId::DBToken { .. } | SymbolId::DBNonTerminalToken { .. } => (5, MatchInputType::Token),
          SymbolId::Default => (6, MatchInputType::Default),
          _sym => {
            #[cfg(debug_assertions)]
            unreachable!("{_sym:?} {s:?}");
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
  state: &GraphNode,
  scanner_data: Option<&Arc<ScannerData>>,
  branches: &mut Queue<((u32, MatchInputType), Vec<SharedGraphNode>)>,
  goto_state_id: &OrderedMap<StateId, IString>,
) {
  let db = &state.db;

  if let Some(((_, input_type), successors)) = branches.pop_front() {
    if matches!(input_type, MatchInputType::Default) {
      if successors.len() > 1 {
        println!("\n\nToo many default successors for state {:?} \n successors: {:?}", state, successors);
      }
      // debug_assert!(successors.len() == 1, "Too many default successors for state
      // {:?} \n successors: {:?}", state, successors);
      let successor = successors.into_iter().next().unwrap();

      let string = build_body(state, &successor, goto_state_id).join(" then ");

      if !string.is_empty() {
        let _ = w + string;
      }
    } else {
      w = w + "\nmatch: " + input_type.as_str();

      if input_type == MatchInputType::Token {
        w = w + ":" + {
          match scanner_data {
            Some(scanner_data) => create_scanner_name(db, scanner_data.hash).to_str(db.string_store()).as_str().to_string(),
            None => {
              panic!(
                "Matches on {input_type:?} should have accompanying scanner {}{state:?} {}",
                state.hash_id,
                successors.iter().map(|s| format!("{s:?}")).collect::<Vec<_>>().join("\n")
              )
            }
          }
        };
      }

      w = (w + " {").indent();

      // Sort successors
      let peeking = successors.iter().any(|s| matches!(s.ty, StateType::PeekEndComplete(_) | StateType::Peek(_)));

      for (match_val, s) in successors.iter().map(|s| (s.sym.sym().to_state_val(db), s)).collect::<OrderedMap<_, _>>() {
        if match_val == CodePointClass::Any as u32 {
          w = w + "\n\ndefault { ";
          w = w + build_body(state, s, goto_state_id).join(" then ") + " }";
        } else {
          debug_assert!(
            match_val < 100_000_000,
            "Got a invalid match value of {match_val} in {state:?} \n with successor: {s:?}",
          );
          w = w + "\n\n( " + match_val.to_string() + " ){ ";
          w = w + build_body(state, s, goto_state_id).join(" then ") + " }";
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
  state: &GraphNode,
  successor: &GraphNode,
  goto_state_id: &OrderedMap<StateId, IString>,
) -> Vec<String> {
  let is_scanner = state.is_scanner();
  let mut body_string: Vec<String> = Array::new();
  let s_type = successor.ty;
  let db = &state.db;

  #[derive(PartialEq, Eq)]
  enum ParserFlow {
    CONTINUE,
    HALT,
  }

  use ParserFlow::*;

  if CONTINUE
    == match s_type {
      StateType::Shift | StateType::KernelShift => {
        let scan_expr = successor.sym.sym().is_linefeed().then_some("shift char then set-line").unwrap_or("shift char");
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
        debug_assert!(!successor.kernel_items().iter().any(|i| i.is_oos()));

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

pub(super) fn create_ir_state_name(origin_state: Option<&GraphNode>, target_state: &GraphNode) -> String {
  if origin_state.is_some_and(|s| s.id == target_state.id) {
    "%%%%".to_string()
  } else if target_state.is_goto() {
    format!("g_{:016X}", &target_state.hash_id)
  } else {
    format!("{}_{:016X}", target_state.is_scanner().then_some("s").unwrap_or("p"), &target_state.hash_id)
  }
}

pub(super) fn create_ir_state(
  w: CodeWriter<Vec<u8>>,
  state: &GraphNode,
  scanner: Option<&Arc<ScannerData>>,
) -> RadlrResult<ParseState> {
  let scanner = scanner.map(|s| crate::compile::states::build_graph::graph::ScannerData {
    hash:    s.hash,
    skipped: s.skipped.clone(),
    symbols: s.symbols.clone(),
  });
  let ir_state = ParseState {
    code: w.to_string(),
    guid_name: create_ir_state_name(None, state).intern(state.db.string_store()),
    scanner,
    ..Default::default()
  };

  RadlrResult::Ok(ir_state)
}
