use super::{build_graph::build_graph, build_ir::build_ir};
use crate::{
  journal::{Journal, ReportType},
  types::*,
  writer::code_writer::CodeWriter,
};
type States = OrderedMap<IString, Box<ParseState>>;
type Scanners = OrderedSet<(IString, OrderedSet<DBTokenData>)>;
use rayon::prelude::*;

pub fn compile_parse_states(mut j: Journal, db: &ParserDatabase) -> SherpaResult<ParseStatesMap> {
  j.set_active_report("State Compile", ReportType::ProductionCompile(Default::default()));

  let results = db
    .productions()
    .iter()
    .cloned()
    .enumerate()
    .collect::<Vec<_>>()
    .chunks(10)
    .map(|chunks| (j.transfer(), chunks))
    .par_bridge()
    .into_par_iter()
    .map(|(mut local_j, chunks)| {
      let mut states = States::new();
      let mut scanners = Scanners::new();

      for (prod_id, prod_sym) in chunks {
        match create_parse_states_from_prod(&mut local_j, db, *prod_id, *prod_sym, &mut states, &mut scanners) {
          SherpaResult::Ok(output) => output,
          SherpaResult::Err(_err) => {}
        }
      }
      (states, scanners)
    })
    .collect::<Vec<_>>();

  let (mut states, scanners) =
    results.into_iter().fold((States::new(), Scanners::new()), |(mut st_to, mut sc_to), (mut st_from, mut sc_from)| {
      st_to.append(&mut st_from);
      sc_to.append(&mut sc_from);
      (st_to, sc_to)
    });

  // Build entry states
  for entry in db.entry_points() {
    let ir = build_entry_ir(entry, db)?;

    for state in ir {
      states.insert(state.name, state);
    }
  }

  // Build Scanners
  for (scanner, group) in scanners {
    let start_items =
      group.iter().flat_map(|s| Items::start_items(s.prod_id, db).to_origin(Origin::TokenGoal(s.tok_id))).collect::<Array<_>>();

    let graph = build_graph(&mut j, GraphMode::Scanner, start_items, db)?;

    let ir = build_ir(&mut j, &graph, scanner)?;
    // println!("{}", graph.debug_string());
    for state in ir {
      //  println!("{} {}", state.name.to_str(db.string_store()).as_str(),
      // state.code);
      states.insert(state.name, state);
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

  SherpaResult::Ok(states)
}

pub fn create_parse_states_from_prod<'db>(
  j: &mut Journal,
  db: &'db ParserDatabase,
  prod_id: usize,
  prod_sym: SymbolId,
  states: &mut States,
  scanners: &mut Scanners,
) -> SherpaResult<()> {
  j.set_active_report("Production Compile", ReportType::ProductionCompile(prod_sym.to_prod_id()));

  if let Some(custom_state) = db.custom_state(prod_id.into()) {
    let name = db.prod_guid_name(prod_id.into());
    let state = ParseState {
      name,
      comment: "Custom State".into(),
      code: custom_state.tok.to_string(),
      ast: Some(Box::new(custom_state.clone())),
      compile_error: None,
      scanners: None,
    };
    states.insert(name, Box::new(state));
  } else {
    let start_items = Items::start_items((prod_id as u32).into(), db).to_origin(Origin::ProdGoal(prod_id.into()));

    match prod_sym {
      SymbolId::NonTerminal { .. } => {
        let graph = build_graph(j, GraphMode::Parser, start_items, db)?;

        let ir = build_ir(j, &graph, db.prod_guid_name(prod_id.into()))?;

        for mut state in ir {
          if let Some(scanner_data) = state.build_scanners(db) {
            for (name, syms) in scanner_data {
              scanners.insert((*name, syms.clone()));
            }
          }
          states.insert(state.name, state);
        }
      }
      SymbolId::NonTerminalToken { .. } => {
        let graph = build_graph(j, GraphMode::Scanner, start_items, db)?;

        let ir = build_ir(j, &graph, db.prod_guid_name(prod_id.into()))?;

        for state in ir {
          states.insert(state.name, state);
        }
      }
      SymbolId::NonTerminalState { .. } => {
        // todo!(load state into the states collection)
      }
      _ => unreachable!(),
    }
  }

  SherpaResult::Ok(())
}

fn build_entry_ir<'db>(
  EntryPoint { prod_name, prod_entry_name, prod_exit_name, .. }: &EntryPoint,
  db: &'db ParserDatabase,
) -> SherpaResult<Array<Box<ParseState>>> {
  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "push " + prod_exit_name.to_string(db.string_store());
  let _ = (&mut w) + " then goto " + prod_name.to_string(db.string_store());

  let entry_state = ParseState { code: w.to_string(), name: *prod_entry_name, ..Default::default() };

  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "accept";

  let exit_state = ParseState { code: w.to_string(), name: *prod_exit_name, ..Default::default() };

  SherpaResult::Ok(Array::from_iter([Box::new(entry_state), Box::new(exit_state)]))
}
