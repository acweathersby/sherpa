use super::{
  construct_LR,
  construct_recursive_ascent,
  construct_recursive_descent,
  ir::construct_ir,
  utils::{generate_recursive_descent_items, generate_scanner_symbol_items},
};
use crate::{
  grammar::hash_id_value_u64,
  journal::{config::ResolutionMode, Journal},
  types::{HCResult, IRState, ItemContainer, ProductionId, ScannerId, SymbolID, SymbolSet},
};
use std::{
  collections::{BTreeMap, BTreeSet},
  thread,
};

pub(crate) fn compile_production_states_LR(
  j: &mut Journal,
  prod_id: ProductionId,
) -> HCResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = grammar.get_production_guid_name(&prod_id);

  j.set_active_report(
    &format!("Production [{}] LR IR Compilation", grammar.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::ProductionCompileLR(prod_id),
  );

  j.report_mut().start_timer("Graph Compile");

  let items = generate_recursive_descent_items(j, prod_id);

  let t = match construct_LR(j, false, &items) {
    HCResult::Ok((t, _)) => t,
    HCResult::Err(err) => {
      j.report_mut().stop_timer("Graph Compile");
      j.report_mut().add_error(err);
      return HCResult::None;
    }
    // Only expecting single errors to be ejected by these functions.
    _ => return HCResult::None,
  };

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Graph Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
  }

  addIRStateNote(j, &rd_states);

  HCResult::Ok(rd_states)
}

// Compile the parser states of a single production
pub(crate) fn compile_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> HCResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = grammar.get_production_guid_name(&prod_id);

  j.set_active_report(
    &format!("Production [{}] IR Compilation", grammar.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::ProductionCompile(prod_id),
  );

  j.report_mut().start_timer("Recursive Descent Compile");

  let items = generate_recursive_descent_items(j, prod_id);

  let t = match construct_recursive_descent(j, false, &items) {
    HCResult::Ok((t, _)) => t,
    HCResult::Err(err) => {
      j.report_mut().stop_timer("Recursive Descent Compile");
      j.report_mut().add_error(err);

      return HCResult::None;
    }
    // Only expecting single errors to be ejected by these functions.
    _ => return HCResult::None,
  };

  let mut rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Recursive Descent Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
  }

  if j.config().resolution_mode.intersects(ResolutionMode::RecursiveAscent) {
    j.report_mut().start_timer("Recursive Ascent Compile");

    let (t, _) = construct_recursive_ascent(j, t.goto_seeds, t.root_prod_ids);

    let mut ra_states = construct_ir(j, &state_name, &t)?;

    rd_states.append(&mut ra_states);

    j.report_mut().stop_timer("Recursive Ascent Compile");

    #[cfg(debug_assertions)]
    {
      j.report_mut().add_note("RA Graph Nodes", t.write_nodes());
    }

    addIRStateNote(j, &rd_states);

    HCResult::Ok(rd_states)
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

    HCResult::Ok(rd_states)
  }
}

// Compiles a scanner state from a set of symbol ids.
pub(crate) fn compile_token_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> HCResult<Vec<Box<IRState>>> {
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

  let t = match construct_recursive_descent(j, true, &items) {
    HCResult::Ok((t, _)) => t,
    HCResult::Err(err) => {
      j.report_mut().stop_timer("Recursive Descent Compile");
      j.report_mut().add_error(err);
      return HCResult::None;
    }
    // Only expecting single errors to be ejected by these functions.
    _ => return HCResult::None,
  };

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Recursive Descent Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
  }

  addIRStateNote(j, &rd_states);

  HCResult::Ok(rd_states)
}

// Compiles a scanner state from a set of symbol ids.
pub(crate) fn compile_scanner_states(
  j: &mut Journal,
  symbols: SymbolSet,
) -> HCResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = format!("scan_{:02X}", hash_id_value_u64(&symbols));

  j.set_active_report(
    &format!("Scanner [{}] IR Compilation", state_name),
    crate::journal::report::ReportType::ScannerCompile(ScannerId::new(&symbols)),
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

  let t = match construct_recursive_descent(j, true, &items) {
    HCResult::Ok((t, _)) => t,
    HCResult::Err(err) => {
      j.report_mut().stop_timer("Recursive Descent Compile");
      j.report_mut().add_error(err);
      return HCResult::None;
    }
    // Only expecting single errors to be ejected by these functions.
    _ => return HCResult::None,
  };

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("Recursive Descent Compile");

  #[cfg(debug_assertions)]
  {
    j.report_mut().add_note("RD Graph Nodes", t.write_nodes());
  }

  addIRStateNote(j, &rd_states);

  HCResult::Ok(rd_states)
}

fn addIRStateNote(j: &mut Journal, rd_states: &Vec<Box<IRState>>) {
  if j.config().debug_add_ir_states_note {
    j.report_mut()
      .add_note("IRStates", rd_states.iter().map(|s| s.get_code()).collect::<Vec<_>>().join("\n"))
  }
}

fn compile_state_chunk(
  j: &mut Journal,
  deduped_states: &mut BTreeMap<String, Box<IRState>>,
  chunk: &[(ProductionId, bool)],
) -> HCResult<()> {
  let mut scanner_names = BTreeSet::new();
  for (prod_id, is_scanner) in chunk {
    if *is_scanner {
      let states = compile_token_production_states(j, *prod_id)?;
      for state in states {
        deduped_states.entry(state.get_name()).or_insert(state);
      }
    } else {
      let states = compile_production_states(j, *prod_id)?;

      for state in states {
        if let Some(name) = state.get_scanner_state_name() {
          if scanner_names.insert(name.clone()) {
            let states = compile_scanner_states(j, state.get_scanner_symbol_set()?)?;

            for state in states {
              deduped_states.entry(state.get_name()).or_insert(state);
            }
          }
        }
        deduped_states.entry(state.get_name()).or_insert(state);
      }
    }
  }

  HCResult::Ok(())
}
/// Compiles IRStates from all production and scanner symbol sets within the grammar.
pub fn compile_states(
  j: &mut Journal,
  num_of_threads: usize,
) -> HCResult<BTreeMap<String, Box<IRState>>> {
  let g = j.grammar()?;

  let mut deduped_states = BTreeMap::new();
  let productions_ids = g.productions.iter().map(|(id, p)| (*id, p.is_scanner)).collect::<Vec<_>>();

  if num_of_threads == 1 {
    compile_state_chunk(j, &mut deduped_states, &productions_ids)?;
  } else {
    let work_chunks = productions_ids
      .chunks((productions_ids.len() / (num_of_threads - 1)).max(1))
      .collect::<Vec<_>>();

    for states in thread::scope(|s| {
      work_chunks
        .into_iter()
        .map(|productions| {
          let mut j = j.transfer();
          s.spawn(move || -> HCResult<_> {
            let mut deduped_states = BTreeMap::new();
            compile_state_chunk(&mut j, &mut deduped_states, productions)?;
            HCResult::Ok(deduped_states)
          })
        })
        .collect::<Vec<_>>()
        .into_iter()
        .map(|s| s.join().unwrap())
        .collect::<Vec<_>>()
    }) {
      match states {
        HCResult::Ok(mut states) => {
          deduped_states.append(&mut states);
        }
        HCResult::Err(err) => {
          panic!("{}", err);
        }
        HCResult::MultipleErrors(errors) => {
          for error in errors {
            println!("{}", error);
          }
          return HCResult::None;
        }
        _ => {}
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
              panic!("\n{} {}", err, string)
            };
          }
        })
      })
      .map(move |s| s.join().unwrap())
      .for_each(drop);
  });

  HCResult::Ok(deduped_states)
}
