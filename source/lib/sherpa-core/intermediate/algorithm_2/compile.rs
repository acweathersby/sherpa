use std::{
  collections::{BTreeMap, BTreeSet, HashSet, VecDeque},
  thread,
};

use crate::{
  grammar::{
    compile::finalize::get_scanner_info_from_defined,
    get_production_start_items2,
    hash_id_value_u64,
  },
  types::{item_2::Item, *},
  Journal,
};

use super::{graph, ir};

fn addIRStateNote(j: &mut Journal, rd_states: &Vec<Box<IRState>>) {
  j.report_mut()
    .add_note("IRStates", rd_states.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n"))
}

pub(crate) fn compile_ir_states(
  j: &mut Journal,
  entry_name: &str,
  items: Vec<Item>,
  is_scanner: bool,
) -> SherpaResult<Vec<Box<IRState>>> {
  let g = &(j.grammar()?);

  j.report_mut().start_timer("Compile");
  j.report_mut().start_timer("Graph States");

  let SherpaResult::Ok(graph) = graph::create(j, items, is_scanner) else {
    j.report_mut().stop_timer("Graph States");
    j.report_mut().stop_timer("Compile");
    return SherpaResult::None
  };
  j.report_mut().stop_timer("Graph States");
  j.report_mut().add_note("Graph States", graph.__debug_string__(g));

  j.report_mut().start_timer("Ir States");
  let SherpaResult::Ok(states) = ir::convert_graph_to_ir(j, &graph, entry_name, is_scanner) else {
    j.report_mut().stop_timer("Ir States");
    j.report_mut().stop_timer("Compile");
    return SherpaResult::None
  };

  j.report_mut().stop_timer("Ir States");
  j.report_mut().stop_timer("Compile");

  addIRStateNote(j, &states);

  SherpaResult::Ok(states)
}

pub(crate) fn construct_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<IRState>>> {
  let g = &(j.grammar()?);
  j.set_active_report(
    &format!("Production [{}] IR Compilation", g.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::ProductionCompile(prod_id),
  );

  compile_ir_states(
    j,
    g.get_production_guid_name(&prod_id),
    get_production_start_items2(&prod_id, g)
      .into_iter()
      .map(|i| i.to_origin(graph_2::Origin::ProdGoal(prod_id)))
      .collect(),
    false,
  )
}
pub(crate) fn construct_token_production_state(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<IRState>>> {
  let g = &(j.grammar()?);
  j.set_active_report(
    &format!("Token Production [{}] IR Compilation", g.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::TokenProductionCompile(prod_id),
  );

  compile_ir_states(
    j,
    g.get_production_guid_name(&prod_id),
    get_production_start_items2(&prod_id, g)
      .into_iter()
      .map(|i| i.to_origin(graph_2::Origin::ProdGoal(prod_id)))
      .collect(),
    true,
  )
}

pub(crate) fn construct_scanner_states(
  j: &mut Journal,
  symbols: SymbolSet,
) -> SherpaResult<Vec<Box<IRState>>> {
  let g = &(j.grammar()?);

  let state_name = format!("scan_{:02X}", hash_id_value_u64(&symbols));

  j.set_active_report(
    &format!("Scanner [{}] IR Compilation", state_name),
    crate::journal::report::ReportType::ScannerCompile(ScannerStateId::new(&symbols)),
  );

  j.report_mut().add_note(
    "Symbol Info",
    format!(
      "This scanner handles the symbols: \n[ {} ]",
      symbols.iter().map(|s| { s.debug_string(g) }).collect::<Vec<_>>().join(" ")
    ),
  );

  let items = symbols
    .iter()
    .flat_map(|s| {
      let (_, prod_id, ..) = get_scanner_info_from_defined(s, &g);
      get_production_start_items2(&prod_id, &g)
        .iter()
        .map(|i| i.to_origin(graph_2::Origin::SymGoal(*s)))
        .collect::<Vec<_>>()
    })
    .collect();

  #[cfg(debug_assertions)]
  {
    //check_for_left_recursion(&items, g)
  }

  compile_ir_states(j, &state_name, items, true)
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
      if let SherpaResult::Ok(states) = construct_token_production_state(j, *prod_id) {
        insert_states(states, deduped_states);
      } else {
        have_errors = true;
      }
    } else {
      if let SherpaResult::Ok(states) = construct_production_states(j, *prod_id) {
        for state in states {
          if let Some(name) = state.get_scanner_state_name() {
            if scanner_names.insert(name.clone()) {
              if let SherpaResult::Ok(states) =
                construct_scanner_states(j, state.get_scanner_symbol_set()?)
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
            match state.compile_ast() {
              SherpaResult::Err(err) => {
                eprintln!("\n{} {}", err, string)
              }
              _ => {}
            }
          }
        })
      })
      .map(move |s| s.join().unwrap())
      .for_each(drop);
  });

  SherpaResult::Ok(deduped_states)
}
