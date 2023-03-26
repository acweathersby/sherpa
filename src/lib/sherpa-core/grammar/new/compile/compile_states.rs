use super::{super::types::*, build_ir::create_ir_state, types::*};
use crate::{
  ascript::types::AScriptStore,
  grammar::{
    compile::parser::sherpa::Ascript,
    hash_id_value_u64,
    new::{
      compile::{build_graph, build_ir},
      types::{OrderedMap, ParserDatabase, ProductionSubType, Rule},
    },
  },
  parser::hash_group_btreemap,
  tasks::{new_taskman, Executor, Spawner},
  types::SherpaErrorSeverity,
  writer::code_writer::CodeWriter,
  Journal,
  ReportType,
  SherpaError,
  SherpaResult,
};
use core::panic;
use sherpa_runtime::{types::BlameColor, utf8::lookup_table::CodePointClass};
use std::{collections::VecDeque, ops::Index, path::PathBuf, sync::Arc};

pub async fn compile_parse_states<'db>(
  mut j: Journal,
  db: &'db ParserDatabase,
) -> SherpaResult<Map<IString, Box<ParseState<'db>>>> {
  j.set_active_report(
    "test",
    ReportType::ProductionCompile(Default::default()),
  );

  let follow = super::follow::create_follow_sets(db);
  let mut states = Map::new();
  let mut scanner_groups = OrderedSet::new();
  let entry_keys = db.entry_prod_keys();

  // Build entry states
  for entry in db.entry_points() {
    let ir = build_entry_ir(entry, db)?;

    for state in ir {
      states.insert(state.name, state);
    }
  }

  // compile productions
  for (prod_id, prod_sym) in db.productions().iter().enumerate() {
    let start_items = Items::start_items((prod_id as u32).into(), db)
      .to_origin(Origin::ProdGoal(prod_id.into()));

    #[cfg(debug_assertions)]
    start_items.__debug_print__("\n");

    match prod_sym {
      SymbolId::NonTerminal { id } => {
        //Run parser pass
        let graph =
          build_graph(&mut j, GraphMode::Parser, start_items, db, &follow)
            .unwrap();

        let ir =
          build_ir(&mut j, &graph, db.prod_name(prod_id.into())).unwrap();

        for mut state in ir {
          if let Some(scanner_data) = state.build_scanners(db) {
            for (name, syms) in scanner_data {
              scanner_groups.insert((*name, syms.clone()));
            }
          }
          states.insert(state.name, state);
        }
      }
      SymbolId::NonTerminalToken { id, .. } => {
        //Run scanner
        let graph =
          build_graph(&mut j, GraphMode::Scanner, start_items, db, &follow)
            .unwrap();

        let ir =
          build_ir(&mut j, &graph, db.prod_name(prod_id.into())).unwrap();

        for state in ir {
          states.insert(state.name, state);
        }
      }
      _ => unreachable!(),
    }
  }

  /// Build Scanners
  for (scanner_name, group) in scanner_groups {
    let start_items = group
      .iter()
      .flat_map(|s| {
        Items::start_items(s.prod_id, db)
          .to_origin(Origin::TokenGoal(s.tok_id.into()))
      })
      .collect::<Array<_>>();

    println!("{}", start_items.to_debug_string("\n"));

    //Run scanner
    let graph =
      build_graph(&mut j, GraphMode::Scanner, start_items, db, &follow)
        .unwrap();

    let ir = build_ir(&mut j, &graph, scanner_name).unwrap();

    for state in ir {
      println!("{}", state.debug_string(db));
      states.insert(state.name, state);
    }
  }

  for (_, mut state) in &mut states {
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

  (&mut w) + "push " + prod_exit_name.to_string(db.string_store());
  (&mut w) + " then goto " + prod_name.to_string(db.string_store());

  let entry_state = ParseState {
    code: w.to_string(),
    name: *prod_entry_name,
    ..Default::default()
  };

  let mut w = CodeWriter::new(Vec::<u8>::with_capacity(512));

  (&mut w) + "accept";

  let exit_state = ParseState {
    code: w.to_string(),
    name: *prod_exit_name,
    ..Default::default()
  };

  SherpaResult::Ok(Array::from_iter([
    Box::new(entry_state),
    Box::new(exit_state),
  ]))
}
