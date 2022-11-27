use super::{
  ir::construct_ir,
  recursive_ascent::construct_recursive_ascent,
  recursive_descent::construct_recursive_descent,
  utils::{generate_recursive_descent_items, generate_scanner_symbol_items},
};
use crate::{
  grammar::hash_id_value_u64,
  journal::{config::ResolutionMode, Journal},
  types::{HCResult, IRState, ItemContainer, ProductionId, SymbolID},
};
use std::{
  collections::{BTreeMap, BTreeSet},
  thread,
};

// Compile the parser states of a single production
pub(crate) fn compile_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> HCResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = grammar.get_production_guid_name(&prod_id);

  j.set_active_report(
    &format!("Production [{}] IR Compilation", state_name),
    crate::journal::report::ReportType::ProductionCompile,
  );

  j.report_mut().start_timer("RD Compilation");

  let items = generate_recursive_descent_items(j, prod_id);

  let (t, _) = construct_recursive_descent(j, false, &items);

  let mut rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("RD Compilation");

  if j.config().resolution_mode.intersects(ResolutionMode::RecursiveAscent) {
    j.report_mut().start_timer("RA Compilation");

    let (t, _) = construct_recursive_ascent(j, t.goto_seeds, t.root_prod_ids);

    let mut ra_states = construct_ir(j, &state_name, &t)?;

    rd_states.append(&mut ra_states);

    j.report_mut().stop_timer("RA Compilation");

    HCResult::Ok(rd_states)
  } else if t.goto_seeds.len() > 0 {
    j.report_mut().stop_timer("RA Compilation");
    panic!(
      "Grammar cannot be compiled purely as Recursive Descent. This error has
     occurred because the config as not been set to enable recursive ascent states. 
     The following items require recursive ascent compilation: [\n{}\n]",
      t.goto_seeds.to_debug_string(&grammar, "\n")
    );
  } else {
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
    &format!("Token Production [{}] IR Compilation", state_name),
    crate::journal::report::ReportType::ProductionCompile,
  );

  j.report_mut().start_timer("RD Compilation");

  let items = generate_recursive_descent_items(j, prod_id)
    .into_iter()
    .map(|i| {
      i.to_origin(crate::types::OriginData::Symbol(
        grammar.get_production(&prod_id).unwrap().sym_id,
      ))
    })
    .collect();

  let (t, _) = construct_recursive_descent(j, true, &items);

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("RD Compilation");

  HCResult::Ok(rd_states)
}

// Compiles a scanner state from a set of symbol ids.
pub(crate) fn compile_scanner_states(
  j: &mut Journal,
  symbols: BTreeSet<SymbolID>,
) -> HCResult<Vec<Box<IRState>>> {
  let grammar = j.grammar()?;

  let state_name = format!("scan_{:02X}", hash_id_value_u64(&symbols));

  j.set_active_report(
    &format!("Scanner [{}] IR Compilation", state_name),
    crate::journal::report::ReportType::ProductionCompile,
  );

  j.report_mut().add_note(
    "Symbol Info",
    format!(
      "This scanner handles the symbols: \n[ {} ]",
      symbols.iter().map(|s| { s.to_string(&grammar) }).collect::<Vec<_>>().join(" ")
    ),
  );

  j.report_mut().start_timer("RD Compilation");

  let items = generate_scanner_symbol_items(symbols, j);

  let (t, _) = construct_recursive_descent(j, true, &items);

  let rd_states = construct_ir(j, &state_name, &t)?;

  j.report_mut().stop_timer("RD Compilation");

  HCResult::Ok(rd_states)
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
          panic!("Failed due to previous errors");
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
