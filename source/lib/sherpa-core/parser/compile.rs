use std::{
  collections::{btree_map, BTreeMap, BTreeSet, HashSet},
  thread,
};

use crate::{
  grammar::{
    compile::finalize::get_scanner_info_from_defined,
    get_production_start_items,
    hash_id_value_u64,
  },
  types::{
    graph::{GraphMode, Origin},
    item::Item,
    *,
  },
  Journal,
};

use super::{graph, ir};

fn addIRStateNote(j: &mut Journal, rd_states: &Vec<Box<ParseState>>) {
  #[cfg(debug_assertions)]
  {
    let mut seen = HashSet::new();
    j.report_mut().add_note(
      "IRStates",
      rd_states
        .iter()
        .filter_map(|s| match seen.insert(s.get_name()) {
          true => Some(s.to_string()),
          _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n"),
    )
  }
}

pub(crate) fn compile_ir_states(
  j: &mut Journal,
  entry_name: &str,
  items: Vec<Item>,
  graph_mode: GraphMode,
  compile_origin_id: &str,
) -> SherpaResult<Vec<Box<ParseState>>> {
  j.report_mut().start_timer("Compile");
  j.report_mut().start_timer("Graph States");

  let mut graph = graph::create(j, items, graph_mode)?;

  j.report_mut().stop_timer("Graph States");

  #[cfg(debug_assertions)]
  j.report_mut().add_note("Graph States", graph.__debug_string__());

  graph.set_prefix(compile_origin_id);

  j.report_mut().start_timer("Ir States");

  let states = ir::convert_graph_to_ir(j, &graph, entry_name)?;

  j.report_mut().stop_timer("Ir States");
  j.report_mut().stop_timer("Compile");

  addIRStateNote(j, &states);

  SherpaResult::Ok(states)
}

pub(crate) fn construct_production_states(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<ParseState>>> {
  let g = &(j.grammar()?);
  j.set_active_report(
    &format!("Production [{}] IR Compilation", g.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::ProductionCompile(prod_id),
  );

  let prod = g.productions.get(&prod_id)?;

  if prod.bytecode_id.is_none() {
    j.report_mut().add_note(
      "unreferenced-production",
      format!("Production [{}] is not used a parser production", prod.name),
    );
    SherpaResult::Ok(Default::default())
  } else {
    compile_ir_states(
      j,
      g.get_production_guid_name(&prod_id),
      get_production_start_items(&prod_id, g)
        .into_iter()
        .map(|i| i.to_origin(Origin::ProdGoal(prod_id)))
        .collect(),
      GraphMode::SherpaClimber,
      "prod",
    )
  }
}

pub(crate) fn construct_token_production_state(
  j: &mut Journal,
  prod_id: ProductionId,
) -> SherpaResult<Vec<Box<ParseState>>> {
  let g = &(j.grammar()?);
  j.set_active_report(
    &format!("Token Production [{}] IR Compilation", g.get_production_plain_name(&prod_id)),
    crate::journal::report::ReportType::TokenProductionCompile(prod_id),
  );

  compile_ir_states(
    j,
    g.get_production_guid_name(&prod_id),
    get_production_start_items(&prod_id, g)
      .into_iter()
      .map(|i| i.to_origin(Origin::ProdGoal(prod_id)))
      .collect(),
    GraphMode::Scanner,
    "token",
  )
}

pub(crate) fn construct_scanner_states(
  j: &mut Journal,
  symbols: SymbolSet,
) -> SherpaResult<Vec<Box<ParseState>>> {
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
      get_production_start_items(&prod_id, &g)
        .iter()
        .map(|i| i.to_origin(Origin::SymGoal(*s)))
        .collect::<Vec<_>>()
    })
    .collect();
  compile_ir_states(j, &state_name, items, GraphMode::Scanner, "sym")
}

#[inline]
#[track_caller]
fn insert_states<T: Iterator<Item = Box<ParseState>>>(
  j: &mut Journal,
  states: T,
  deduped_states: &mut BTreeMap<String, Box<ParseState>>,
) {
  for state in states {
    match deduped_states.entry(state.get_name()) {
      btree_map::Entry::Occupied(e) => {
        #[cfg(debug_assertions)]
        {
          if e.get().code != state.code {
            let (old_str, new_str) = (e.get().to_string(), state.to_string());
            if j.config().debug.allow_parse_state_name_collisions {
              eprintln!("Expected [\n{old_str}\n] to equal [\n{new_str}\n]");
            } else {
              panic!("Expected [\n{old_str}\n] to equal [\n{new_str}\n]");
            }
          }
        }
      }
      btree_map::Entry::Vacant(e) => {
        e.insert(state);
      }
    }
  }
}
fn compile_slice_of_states(
  j: &mut Journal,
  deduped_states: &mut BTreeMap<String, Box<ParseState>>,
  slice: &[(ProductionId, bool)],
) -> SherpaResult<()> {
  let mut scanner_names = BTreeSet::new();
  let mut have_errors = false;

  for (prod_id, is_scanner) in slice {
    if *is_scanner {
      if let SherpaResult::Ok(states) = construct_token_production_state(j, *prod_id) {
        insert_states(j, states.into_iter(), deduped_states);
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
                insert_states(j, states.into_iter(), deduped_states);
              } else {
                have_errors = true;
              }
            }
          }
          insert_states(j, vec![state].into_iter(), deduped_states);
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
pub fn compile_parse_states(
  j: &mut Journal,
  num_of_threads: usize,
) -> SherpaResult<BTreeMap<String, Box<ParseState>>> {
  let g = j.grammar()?;

  let mut deduped_states = BTreeMap::new();
  let productions_ids = g.productions.iter().map(|(id, p)| (*id, p.is_scanner)).collect::<Vec<_>>();

  let work_chunks = productions_ids
    .chunks((productions_ids.len() / (num_of_threads).max(1)).max(1))
    .collect::<Vec<_>>();

  let entry_state = create_entry_wrapper_states(j)?;
  insert_states(j, entry_state.into_iter(), &mut deduped_states);

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
      .map(|s| match s.join() {
        Result::Ok(result) => result,
        Err(err) => panic!("Error encountered in thread]\n {:?}", err),
      })
      .collect::<Vec<_>>()
  }) {
    match states {
      SherpaResult::Ok(states) => {
        insert_states(j, states.into_values(), &mut deduped_states);
      }
      SherpaResult::Err(err) => {
        // All captured errors in the compilation process are rolled
        // into reports and a SherpaResult::None is returned in its place.
        // Any other type of error is something due to unaccounted
        // behavior (unwrapping a None option, etc), so we do a hard
        // panic to report these incidences.
        panic!("An unexpected error has occurred:\n{}", err);
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
            match state.get_cached_ast() {
              SherpaResult::Err(err) => {
                eprintln!("\n{} {}", err, state.to_string())
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

fn create_entry_wrapper_states(j: &mut Journal) -> SherpaResult<Vec<Box<ParseState>>> {
  let mut states = vec![];

  let g = &(j.grammar()?);

  for ExportedProduction { guid_name: name, production, .. } in &g.get_exported_productions() {
    let entry_name = &g.get_entry_name_from_prod_id(&production.id)?;
    let skip_symbols: Vec<SymbolID> =
      g.production_ignore_symbols.get(&production.id).unwrap().iter().cloned().collect();

    let state_entry = ParseState {
      comment: "".into(),
      code: format!(r#"push state [ {name}_exit ] then goto state [ {name} ]"#),
      name: entry_name.clone(),
      state_type: IRStateType::Parser,
      ..Default::default()
    };
    states.push(Box::new(state_entry));

    if skip_symbols.len() > 0 {
      let scanner_symbols = BTreeSet::from_iter(skip_symbols.iter().cloned());

      let state_exit = ParseState {
        comment: "".into(),
        code: format!(
          "default ( accept )\nassert TOKEN [ {} ] ( accept ){}",
          SymbolID::EndOfFile.bytecode_id(g),
          skip_symbols
            .iter()
            .map(|s| format!("\nassert TOKEN [ {} ] ( skip-token )", s.bytecode_id(g)))
            .collect::<Vec<_>>()
            .join("")
        ),
        name: format!("{name}_exit"),
        state_type: IRStateType::Parser,
        skip_symbols,
        ..Default::default()
      };

      states.push(Box::new(state_exit));
      states.append(&mut construct_scanner_states(j, scanner_symbols)?);
    } else {
      let state_exit = ParseState {
        comment: "".into(),
        code: format!(r#"accept"#),
        name: format!("{name}_exit"),
        state_type: IRStateType::Parser,
        skip_symbols,
        ..Default::default()
      };

      states.push(Box::new(state_exit));
    }
  }

  SherpaResult::Ok(states)
}
