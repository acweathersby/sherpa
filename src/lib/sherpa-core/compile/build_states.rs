use super::{build_graph::build_graph, build_ir::build_ir};
use crate::{
  journal::{Journal, ReportType},
  types::*,
  writer::code_writer::CodeWriter,
};
use core::panic;

pub fn compile_parse_states<'db>(
  mut j: Journal,
  db: &'db ParserDatabase,
) -> SherpaResult<ParseStatesMap<'db>> {
  j.set_active_report("test", ReportType::ProductionCompile(Default::default()));

  let follow = create_follow_sets(db);
  let mut states = Map::new();
  let mut scanners = OrderedSet::new();

  // Build entry states
  for entry in db.entry_points() {
    let ir = build_entry_ir(entry, db)?;

    for state in ir {
      states.insert(state.name, state);
    }
  }

  // compile productions
  for (prod_id, prod_sym) in db.productions().iter().enumerate() {
    if let Some(custom_state) = db.custom_state(prod_id.into()) {
      let name = db.prod_guid_name(prod_id.into());
      let state = ParseState {
        name,
        comment: "Custom State".into(),
        code: custom_state.tok.to_string(),
        ast: SherpaResult::Ok(Box::new(custom_state.clone())),
        scanners: None,
      };
      states.insert(name, Box::new(state));
    } else {
      let start_items =
        Items::start_items((prod_id as u32).into(), db).to_origin(Origin::ProdGoal(prod_id.into()));

      match prod_sym {
        SymbolId::NonTerminal { .. } => {
          let graph = build_graph(&mut j, GraphMode::Parser, start_items, db, &follow).unwrap();

          let ir = build_ir(&mut j, &graph, db.prod_guid_name(prod_id.into())).unwrap();

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
          let graph = build_graph(&mut j, GraphMode::Scanner, start_items, db, &follow).unwrap();

          let ir = build_ir(&mut j, &graph, db.prod_guid_name(prod_id.into())).unwrap();

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
  }

  // Build Scanners
  for (scanner, group) in scanners {
    let start_items = group
      .iter()
      .flat_map(|s| Items::start_items(s.prod_id, db).to_origin(Origin::TokenGoal(s.tok_id)))
      .collect::<Array<_>>();

    //    start_items.__debug_print__("Scanner Items");

    let graph = build_graph(&mut j, GraphMode::Scanner, start_items, db, &follow).unwrap();

    let ir = build_ir(&mut j, &graph, scanner).unwrap();
    // println!("{}", graph.debug_string());
    for state in ir {
      //  println!("{} {}", state.name.to_str(db.string_store()).as_str(),
      // state.code);
      states.insert(state.name, state);
    }
  }

  for (_, state) in &mut states {
    // Warn of failed parses
    match state.build_ast(db.string_store()) {
      SherpaResult::Err(err) => {
        #[cfg(debug_assertions)]
        panic!("[Internal Error] Failed to create parse state:\n{}", err);
        panic!("[Internal Error] Failed to create parse state");
      }
      _ => {}
    }
  }

  SherpaResult::Ok(states)
}

fn build_entry_ir<'db>(
  EntryPoint { prod_name, prod_entry_name, prod_exit_name, .. }: &EntryPoint,
  db: &'db ParserDatabase,
) -> SherpaResult<Array<Box<ParseState<'db>>>> {
  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "push " + prod_exit_name.to_string(db.string_store());
  let _ = (&mut w) + " then goto " + prod_name.to_string(db.string_store());

  let entry_state =
    ParseState { code: w.to_string(), name: *prod_entry_name, ..Default::default() };

  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  let _ = (&mut w) + "accept";

  let exit_state = ParseState { code: w.to_string(), name: *prod_exit_name, ..Default::default() };

  SherpaResult::Ok(Array::from_iter([Box::new(entry_state), Box::new(exit_state)]))
}
