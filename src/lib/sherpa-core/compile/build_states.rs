use super::{
  build_graph::{
    build,
    graph::{GraphIterator, GraphType, Origin},
  },
  build_ir::build_ir,
};
use crate::{
  journal::{Journal, ReportType},
  types::*,
  writer::code_writer::CodeWriter,
};
type States = OrderedMap<IString, Box<ParseState>>;
type Scanners = OrderedSet<(IString, OrderedSet<PrecedentDBTerm>)>;
type Errors = Array<SherpaError>;
use rayon::prelude::*;

pub fn compile_parse_states(
  mut j: Journal,
  db: &ParserDatabase,
  config: ParserConfig,
) -> SherpaResult<(ParserClassification, ParseStatesMap)> {
  j.set_active_report("State Compile", ReportType::NonTerminalCompile(Default::default()));

  #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
  crate::test::utils::write_debug_file(db, "parse_graph.tmp", "", false)?;

  #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
  crate::test::utils::write_debug_file(db, "scanner_graph.tmp", "", false)?;

  let results = db
    .nonterms()
    .iter()
    .enumerate()
    .map(|(index, sym)| (DBNonTermKey::from(index), sym.clone()))
    .filter(|(nt_id, sym)| {
      if !config.ALLOW_RECURSIVE_DESCENT {
        // If we can't make calls to another nonterminal parse graph then it
        // doesn't make sense to create parse graphs for non-terminals
        // that are not exported by the grammar, as the root of those graphs
        // will be unreachable.
        sym.is_term() || db.entry_nterm_keys().contains(nt_id)
      } else {
        true
      }
    })
    .collect::<Vec<_>>()
    .chunks(10)
    .map(|chunks| (j.transfer(), chunks))
    .par_bridge()
    .into_par_iter()
    .map(|(mut local_j, chunks)| {
      let mut states = States::new();
      let mut scanners = Scanners::new();
      let mut errors = Errors::new();
      let mut classification = Default::default();

      for (nterm, nterm_sym) in chunks {
        match create_parse_states_from_prod(&mut local_j, db, *nterm, *nterm_sym, &mut states, &mut scanners, config) {
          SherpaResult::Ok(class) => {
            classification |= class;
          }
          SherpaResult::Err(err) => errors.push(err),
        }
      }
      (classification, states, scanners, errors)
    })
    .collect::<Vec<_>>();

  let (classification, mut states, scanners, mut errors) = results.into_iter().fold(
    (ParserClassification::default(), States::new(), Scanners::new(), Errors::new()),
    |(c_to, mut st_to, mut sc_to, mut er_to), (c_from, mut st_from, mut sc_from, mut er_from)| {
      st_to.append(&mut st_from);
      sc_to.append(&mut sc_from);
      er_to.append(&mut er_from);
      (c_to | c_from, st_to, sc_to, er_to)
    },
  );

  if errors.len() > 0 {
    if errors.len() == 1 {
      return Err(errors.pop().unwrap());
    } else {
      return Err(SherpaError::Multi(errors));
    }
  }

  // Build entry states
  for entry in db.entry_points().iter().filter(|i| config.EXPORT_ALL_NONTERMS || i.is_export) {
    let ir = build_entry_ir(entry, db)?;

    for state in ir {
      states.insert(state.hash_name, state);
    }
  }

  // Build Scanners
  for (scanner, group) in scanners {
    let start_items = group
      .iter()
      .flat_map(|s| {
        ItemSet::start_items(db.token(s.tok()).nonterm_id, db).to_origin(Origin::TerminalGoal(s.tok(), s.precedence()))
      })
      .collect::<ItemSet>();

    let (_, graph) = build(&mut j, scanner, GraphType::Scanner, start_items, db, config)?;

    let ir = build_ir(&mut j, GraphIterator::new(&graph), scanner)?;

    for state in ir {
      states.insert(state.hash_name, state);
    }
  }

  for (_, state) in &mut states {
    // Warn of failed parses
    match state.build_ast(db) {
      SherpaResult::Err(err) => {
        todo!("Add State compile error to Journal {err}");
      }
      _ => {}
    }
  }

  SherpaResult::Ok((classification, states))
}

fn create_parse_states_from_prod<'db>(
  j: &mut Journal,
  db: &'db ParserDatabase,
  nterm_key: DBNonTermKey,
  nterm_sym: SymbolId,
  states: &mut States,
  scanners: &mut Scanners,
  config: ParserConfig,
) -> SherpaResult<ParserClassification> {
  j.set_active_report("Non-terminal Compile", ReportType::NonTerminalCompile(nterm_sym.to_nterm()));

  let mut classification = Default::default();

  if let Some(custom_state) = db.custom_state(nterm_key) {
    let name = db.nonterm_guid_name(nterm_key);

    let state = ParseState {
      hash_name:     name,
      name:          name,
      comment:       "Custom State".into(),
      code:          custom_state.tok.to_string(),
      ast:           Some(Box::new(custom_state.clone())),
      compile_error: None,
      scanner:       None,
      root:          true,
      precedence:    0,
    };
    states.insert(name, Box::new(state));
  } else {
    let start_items = ItemSet::start_items(nterm_key, db).to_origin(Origin::NonTermGoal(nterm_key));
    match nterm_sym {
      SymbolId::NonTerminal { .. } => {
        let (class, graph) = build(j, db.nonterm_guid_name(nterm_key), GraphType::Parser, start_items, db, config)?;

        classification |= class;

        let ir = build_ir(j, GraphIterator::new(&graph), db.nonterm_guid_name(nterm_key))?;

        for mut state in ir {
          if let Some(scanner_data) = state.get_scanner() {
            scanners.insert(scanner_data.clone());
          }
          states.insert(state.hash_name, state);
        }
      }
      SymbolId::NonTerminalToken { .. } => {
        let (_, graph) = build(j, db.nonterm_guid_name(nterm_key), GraphType::Scanner, start_items, db, config)?;

        let ir = build_ir(j, GraphIterator::new(&graph), db.nonterm_guid_name(nterm_key))?;

        for state in ir {
          states.insert(state.hash_name, state);
        }
      }
      SymbolId::NonTerminalState { .. } => { /* Not Processed Here */ }
      _ => unreachable!(),
    }
  }

  SherpaResult::Ok(classification)
}

fn build_entry_ir<'db>(
  EntryPoint {
    nonterm_name: nterm_name,
    nonterm_entry_name: nterm_entry_name,
    nonterm_exit_name: nterm_exit_name,
    ..
  }: &EntryPoint,
  db: &'db ParserDatabase,
) -> SherpaResult<Array<Box<ParseState>>> {
  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "push " + nterm_exit_name.to_string(db.string_store());
  let _ = (&mut w) + " then goto " + nterm_name.to_string(db.string_store());

  let entry_state = ParseState {
    code: w.to_string(),
    hash_name: *nterm_entry_name,
    ..Default::default()
  };

  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "accept";

  let exit_state = ParseState { code: w.to_string(), hash_name: *nterm_exit_name, ..Default::default() };

  SherpaResult::Ok(Array::from_iter([Box::new(entry_state), Box::new(exit_state)]))
}
