use super::{
  _construct_LR,
  construct_recursive_ascent,
  construct_recursive_descent,
  ir::construct_ir,
  utils::{generate_recursive_descent_items, generate_scanner_symbol_items},
};
use crate::{
  grammar::hash_id_value_u64,
  journal::{config::ParseAlgorithm, Journal},
  types::{
    IRState,
    ItemContainer,
    ProductionId,
    ScanType,
    ScannerStateId,
    SherpaResult,
    SymbolSet,
  },
};
use std::{
  collections::{BTreeMap, BTreeSet},
  thread,
};

pub(crate) fn _compile_production_states_LR(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = grammar.get_production_guid_name(&prod_id);

  j.set_active_report(
    &format!("Production [{}] LR IR Compilation", grammar.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::ProductionCompileLR(prod_id),
  );

  j.report_mut().start_timer("Graph Compile");

  let items = generate_recursive_descent_items(j, prod_id);

  let t = match _construct_LR(j, ScanType::None, &items) {
    SherpaResult::Ok((t, _)) => t,
    SherpaResult::Err(err) => {
      j.report_mut().stop_timer("Graph Compile");
      j.report_mut().add_error(err);
      return SherpaResult::None;
    }
    // Only expecting single errors to be ejected by these functions.
    _ => return SherpaResult::None,
  };

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Graph Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
  }

  addIRStateNote(j, &rd_states);

  SherpaResult::Ok(rd_states)
}

fn handle_error<T>(
  error_title: &'static str,
  result: SherpaResult<T>,
  j: &mut Journal,
) -> SherpaResult<T> {
  match result {
    SherpaResult::Err(err) => {
      j.report_mut().add_note(error_title, "An Error Has Occurred".to_string());
      j.report_mut().stop_all_timers();
      j.report_mut().add_error(err);
      SherpaResult::None
    }
    SherpaResult::MultipleErrors(errors) => {
      j.report_mut().add_note(error_title, "Errors Have Occurred".to_string());
      j.report_mut().stop_all_timers();
      for err in errors {
        j.report_mut().add_error(err);
      }
      SherpaResult::None
    }
    SherpaResult::None => {
      j.report_mut().add_note(error_title, "Errors Have Occurred".to_string());
      j.report_mut().stop_all_timers();
      SherpaResult::None
    }
    // Only expecting single errors to be ejected by these functions.
    result => result,
  }
}

// Compile the parser states of a single production
pub(crate) fn compile_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = grammar.get_production_guid_name(&prod_id);

  j.set_active_report(
    &format!("Production [{}] IR Compilation", grammar.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::ProductionCompile(prod_id),
  );

  j.report_mut().start_timer("Recursive Descent Compile");

  let items = generate_recursive_descent_items(j, prod_id);

  let (t, _) = handle_error(
    "Recursive Descent Stopped",
    construct_recursive_descent(j, ScanType::None, &items),
    j,
  )?;

  let mut rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Recursive Descent Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
    j.report_mut().add_note(
      "RD States",
      rd_states.iter().map(|s| s.get_code()).collect::<Vec<String>>().join("\n\n"),
    );
  }

  if j.config().enabled_algorithms.intersects(ParseAlgorithm::RecursiveAscent) {
    j.report_mut().start_timer("Recursive Ascent Compile");

    let (t, _) = handle_error(
      "Recursive Ascent Stopped",
      construct_recursive_ascent(j, t.goto_seeds, t.root_prod_ids),
      j,
    )?;

    let mut ra_states = construct_ir(j, &state_name, &t)?;

    rd_states.append(&mut ra_states);

    j.report_mut().stop_timer("Recursive Ascent Compile");

    #[cfg(debug_assertions)]
    {
      j.report_mut().add_note("RA Graph Nodes", t.write_nodes());
    }

    addIRStateNote(j, &rd_states);

    SherpaResult::Ok(rd_states)
  } else if t.goto_seeds.len() > 0 {
    j.report_mut().stop_timer("Recursive Ascent Compile");
    panic!(
      "Grammar cannot be compiled purely as Recursive Descent. This error has
     occurred because the config as not been set to enable recursive ascent states. 
     The following items require recursive ascent compilation: [\n{}\n]",
      t.goto_seeds.to_debug_string(&grammar, "\n")
    );
  } else {
    addIRStateNote(j, &rd_states);

    SherpaResult::Ok(rd_states)
  }
}

// Compiles a scanner state from a set of symbol ids.
pub(crate) fn compile_token_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = grammar.get_production_guid_name(&prod_id);

  j.set_active_report(
    &format!("Token Production [{}] IR Compilation", grammar.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::TokenProductionCompile(prod_id),
  );

  j.report_mut().start_timer("Recursive Descent Compile");

  let items = generate_recursive_descent_items(j, prod_id)
    .into_iter()
    .map(|i| {
      i.to_origin(crate::types::OriginData::Symbol(
        grammar.get_production(&prod_id).unwrap().sym_id,
      ))
    })
    .collect();

  let (t, _) = handle_error(
    "Recursive Descent Stopped",
    construct_recursive_descent(j, ScanType::ScannerProduction, &items),
    j,
  )?;

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Recursive Descent Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
    j.report_mut().add_note(
      "RD States",
      rd_states.iter().map(|s| s.get_code()).collect::<Vec<String>>().join("\n\n"),
    );
  }

  addIRStateNote(j, &rd_states);

  SherpaResult::Ok(rd_states)
}

// Compiles a scanner state from a set of symbol ids.
pub(crate) fn compile_scanner_states(
  j: &mut Journal,
  symbols: SymbolSet,
) -> SherpaResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = format!("scan_{:02X}", hash_id_value_u64(&symbols));

  j.set_active_report(
    &format!("Scanner [{}] IR Compilation", state_name),
    crate::journal::report::ReportType::ScannerCompile(ScannerStateId::new(&symbols)),
  );

  j.report_mut().add_note(
    "Symbol Info",
    format!(
      "This scanner handles the symbols: \n[ {} ]",
      symbols.iter().map(|s| { s.to_string(&grammar) }).collect::<Vec<_>>().join(" ")
    ),
  );

  j.report_mut().start_timer("Recursive Descent Compile");

  let items = generate_scanner_symbol_items(symbols, j);

  let (t, _) = handle_error(
    "Recursive Descent Stopped",
    construct_recursive_descent(j, ScanType::ScannerEntry, &items),
    j,
  )?;

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Recursive Descent Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
    j.report_mut().add_note(
      "RD States",
      rd_states.iter().map(|s| s.get_code()).collect::<Vec<String>>().join("\n\n"),
    );
  }

  addIRStateNote(j, &rd_states);

  SherpaResult::Ok(rd_states)
}

fn addIRStateNote(j: &mut Journal, rd_states: &Vec<Box<IRState>>) {
  if j.config().debug_add_ir_states_note {
    j.report_mut()
      .add_note("IRStates", rd_states.iter().map(|s| s.get_code()).collect::<Vec<_>>().join("\n"))
  }
}

fn compile_slice_of_states(
  j: &mut Journal,
  deduped_states: &mut BTreeMap<String, Box<IRState>>,
  slice: &[(ProductionId, bool)],
) -> SherpaResult<()> {
  let mut scanner_names = BTreeSet::new();
  let mut have_errors = false;

  #[inline]
  fn insert_states(states: Vec<Box<IRState>>, deduped_states: &mut BTreeMap<String, Box<IRState>>) {
    for state in states {
      deduped_states.entry(state.get_name()).or_insert(state);
    }
  }

  for (prod_id, is_scanner) in slice {
    if *is_scanner {
      if let SherpaResult::Ok(states) = compile_token_production_states(j, *prod_id) {
        insert_states(states, deduped_states);
      } else {
        have_errors = true;
      }
    } else {
      if let SherpaResult::Ok(states) = compile_production_states(j, *prod_id) {
        for state in states {
          if let Some(name) = state.get_scanner_state_name() {
            if scanner_names.insert(name.clone()) {
              if let SherpaResult::Ok(states) =
                compile_scanner_states(j, state.get_scanner_symbol_set()?)
              {
                insert_states(states, deduped_states);
              } else {
                have_errors = true;
              }
            }
          }
          insert_states(vec![state], deduped_states);
        }
      } else {
        have_errors = true;
      }
    }
  }

  if have_errors {
    SherpaResult::None
  } else {
    SherpaResult::Ok(())
  }
}

/// Compiles IRStates from all production and scanner symbol sets within the grammar.
pub fn compile_states(
  j: &mut Journal,
  num_of_threads: usize,
) -> SherpaResult<BTreeMap<String, Box<IRState>>> {
  let g = j.grammar()?;

  let mut deduped_states = BTreeMap::new();
  let productions_ids = g.productions.iter().map(|(id, p)| (*id, p.is_scanner)).collect::<Vec<_>>();

  let work_chunks = productions_ids
    .chunks((productions_ids.len() / (num_of_threads).max(1)).max(1))
    .collect::<Vec<_>>();

  for states in thread::scope(|s| {
    work_chunks
      .into_iter()
      .map(|productions| {
        let mut j = j.transfer();
        s.spawn(move || -> SherpaResult<_> {
          let mut deduped_states = BTreeMap::new();
          compile_slice_of_states(&mut j, &mut deduped_states, productions)?;
          SherpaResult::Ok(deduped_states)
        })
      })
      .collect::<Vec<_>>()
      .into_iter()
      .map(|s| s.join().unwrap())
      .collect::<Vec<_>>()
  }) {
    match states {
      SherpaResult::Ok(mut states) => {
        deduped_states.append(&mut states);
      }
      SherpaResult::Err(err) => {
        // All captured errors in the compilation process are rolled
        // into reports and a SherpaResult::None is returned in its place.
        // Any other type of error is something due to unaccounted
        // behavior (unwrapping a None option, etc), so we do a hard
        // panic to report these incidences.
        panic!("An unexpected error has occurred:\n{}", err);
      }
      SherpaResult::MultipleErrors(errors) => {
        // Same as above
        for error in errors {
          eprintln!("{}", error);
        }
        panic!("Unexpected errors have occurred, cannot continue");
      }
      _ => {
        // Reports in the Journal contain the errors.
        return SherpaResult::None;
      }
    }
  }

  let size = deduped_states.len();
  let mut chunks = deduped_states.values_mut().collect::<Vec<_>>();
  thread::scope(|s| {
    chunks
      .chunks_mut((size / (num_of_threads.max(1))).max(1))
      .map(|chunk| {
        s.spawn(|| {
          for state in chunk {
            let string = state.to_string();
            if let Err(err) = state.compile_ast() {
              eprintln!("\n{} {}", err, string)
            };
          }
        })
      })
      .map(move |s| s.join().unwrap())
      .for_each(drop);
  });

  SherpaResult::Ok(deduped_states)
}
