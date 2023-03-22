use super::{super::types::*, types::*};
use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
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
  Journal,
  ReportType,
  SherpaError,
  SherpaResult,
};
use core::panic;
use sherpa_runtime::{types::BlameColor, utf8::lookup_table::CodePointClass};
use std::{collections::VecDeque, ops::Index, path::PathBuf, sync::Arc};

pub async fn compile_parse_states(mut j: Journal, db: &ParserDatabase) {
  j.set_active_report(
    "test",
    ReportType::ProductionCompile(Default::default()),
  );

  let follow = super::follow::create_follow_sets(db);

  // compile productions
  for (prod_id, prod_sym) in db.productions().iter().enumerate() {
    let start_items = Items::start_items((prod_id as u32).into(), db);

    #[cfg(debug_assertions)]
    start_items.__debug_print__("\n");

    match prod_sym {
      SymbolId::NonTerminal { id } => {
        //Run parser pass
        let graph =
          build_graph(&mut j, GraphMode::Parser, start_items, db, &follow)
            .unwrap();

        println!("{}", graph.__debug_string__());

        let ir = build_ir(&mut j, &graph, "test").unwrap();

        #[cfg(debug_assertions)]
        for state in ir {
          println!("{}", state.debug_string(db));
        }
      }
      SymbolId::NonTerminalToken { id, .. } => {
        //Run scanner
        let graph =
          build_graph(&mut j, GraphMode::Scanner, start_items, db, &follow)
            .unwrap();

        println!("{}", graph.__debug_string__());

        let ir = build_ir(&mut j, &graph, "test").unwrap();

        #[cfg(debug_assertions)]
        for state in ir {
          println!("{}", state.debug_string(db));
        }
      }
      _ => unreachable!(),
    }
  }
  dbg!(&db);
}
