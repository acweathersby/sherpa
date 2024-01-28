use super::{
  build_graph::graph::{GraphType, Origin, StateId, StateType},
  build_graph_beta::{
    build::handle_kernel_items,
    graph::{ConcurrentGraphBuilder, Graphs, StagedNode},
  },
};
use crate::types::{worker_pool::WorkerPool, *};
use std::sync::Arc;

#[derive(Debug, Clone, Hash)]
pub enum StateConstructionError {
  NonDeterministicPeek(DBNonTermKey),
  OtherErrors(Vec<RadlrError>),
}

/// Add a root node to the graph queue. This type of node is also added to the
/// global pending root nodes pool.
pub fn add_root(
  db_nt_key: DBNonTermKey,
  name: IString,
  graph_type: GraphType,
  kernel_items: ItemSet,
  builder: &mut ConcurrentGraphBuilder,
  config: &ParserConfig,
) -> RadlrResult<()> {
  StagedNode::new(builder)
    .kernel_items(kernel_items.to_vec().into_iter())
    .ty(StateType::Start)
    .graph_ty(graph_type)
    .make_root(name, db_nt_key)
    .commit(builder);

  builder.commit(false, None, &config, false, false)?;
  Ok(())
}

pub(crate) fn compile_parser_states(db: Arc<ParserDatabase>, config: ParserConfig) -> RadlrResult<Graphs> {
  // Create entry nodes.

  let mut gb = ConcurrentGraphBuilder::new(db.clone(), config);

  for result in db.nonterms().iter().enumerate().map(|(index, sym)| {
    let nt_id: DBNonTermKey = (index as u32).into();
    let kernel_items = ItemSet::start_items(nt_id, &db).to_origin(Origin::NonTermGoal(nt_id));
    add_root(
      nt_id,
      db.nonterm_guid_name(nt_id),
      sym.is_term().then_some(GraphType::Scanner).unwrap_or(GraphType::Parser),
      kernel_items,
      &mut gb,
      &config,
    )
  }) {
    result?;
  }

  #[cfg(not(feature = "wasm-target"))]
  let pool: worker_pool::StandardPool = crate::types::worker_pool::StandardPool::new(8).unwrap();

  #[cfg(feature = "wasm-target")]
  let pool = crate::types::worker_pool::SingleThreadPool {};

  let sync_tracker = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));

  pool.run(|_num_of_threads_| {
    let mut gb = gb.clone();

    let sync_tracker = sync_tracker.clone();

    move |_| {
      let mut retries = 0;
      loop {
        if let Some(((node, config), _is_local_work)) =
          gb.get_local_work().map(|w| (w, true)).or_else(|| gb.get_global_work().map(|w| (w, false)))
        {
          if retries > 0 {
            retries = 0;
            sync_tracker.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
          }

          match handle_kernel_items(&mut gb, &node, &config) {
            Err(RadlrError::StateConstructionError(StateConstructionError::NonDeterministicPeek(non_term))) => {
              //-----------------------------------------------------------------------------------
              // This non-terminal is invalid. If this is a root entry state
              // then we can't construct a parser for it.
              // Otherwise, we can mark the parser for this
              // non-term as invalid, And then proceed to "collapse" the general
              // parser, if LR parsing is enabled.
              //
              // This involves the following:

              // let A = the non-terminal whose peek has failed.
              //
              // Dropping all states of A; this will results in a parser that is
              // not enterable at A. Then, mark all exported
              // non-terminals which contains at least
              // one rule that, directly or indirectly, references an A in the
              // right side as "LR only", preventing states with
              // call transitions from being created. Rebuild the
              // states of the effected non-terminals. Repeat this process for
              // any non-terminal, marked as "LR only", that fails
              // to generate states due to undeterministic peek.
              //
              // If, during this iterative process, a non-terminal is
              // encountered that is "LR only", is non-deterministic during
              // peeking, and is a root entry point for the
              // parser, then we have failed to generate a minimum
              // acceptable parser for this grammar configuration.
              // Report parser as failed and produce relevant
              // diagnostic messages.
              //
              // If LR style parsing is disabled, then we cannot perform this
              // process. Report parser construction as failed and
              // produce relevant diagnostic messages.
              //-----------------------------------------------------------------------------------

              let root_data = node.root_data;
              let nonterm_key = root_data.db_key;
              if config.ALLOW_LR {
                match gb.poison_nonterminal(nonterm_key) {
                  Err(err) => {
                    sync_tracker.fetch_add(_num_of_threads_ + 1, std::sync::atomic::Ordering::Relaxed);
                    return Err(err);
                  }
                  _ => {}
                }
                let db = gb.db_rc().clone();

                if db.entry_nterm_map().contains_key(&nonterm_key) {
                  // Ensures all other threads are halted when the active queue is exhausted.
                  sync_tracker.fetch_add(_num_of_threads_ + 1, std::sync::atomic::Ordering::Relaxed);
                  return Err(
                    format!("Entry parser {} is non-deterministic", db.nonterm_friendly_name_string(nonterm_key),).into(),
                  );
                } else {
                  if let Some(nonterms) = db.get_nonterminal_predecessors(nonterm_key) {
                    for nt_id in nonterms {
                      let mut new_config = config;
                      new_config.ALLOW_LR = true;
                      new_config.ALLOW_CALLS = false;

                      let kernel_items = ItemSet::start_items(*nt_id, &db).to_origin(Origin::NonTermGoal(*nt_id));

                      StagedNode::new(&gb)
                        .kernel_items(kernel_items.to_vec().into_iter())
                        .ty(StateType::Start)
                        .graph_ty(node.graph_type())
                        .make_root(db.nonterm_guid_name(*nt_id), *nt_id)
                        .commit(&mut gb);

                      gb.commit(false, None, &new_config, false, false)?;
                    }
                  }
                }
              }
            }
            Err(err) => {
              // Ensures all other threads are halted when the active queue is exhausted.
              sync_tracker.fetch_add(_num_of_threads_ + 1, std::sync::atomic::Ordering::Relaxed);
              return Err("Todo: Report critical failure during compilation: {err}".into());
            }
            _ => {}
          }
        } else {
          let num_of_waiting_threads = if retries == 0 {
            let val = sync_tracker.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            val + 1
          } else {
            sync_tracker.load(std::sync::atomic::Ordering::Relaxed)
          };

          if num_of_waiting_threads >= _num_of_threads_ {
            break;
          }

          retries += 1;

          let sleep_dur = std::time::Duration::from_micros((num_of_waiting_threads as u64 >> 1).max(1));

          std::thread::sleep(sleep_dur);

          std::hint::spin_loop()
        }
      }

      Ok(())
    }
  })?;

  Ok(gb.into())
}

#[cfg(test)]
mod test {
  use super::compile_parser_states;
  use crate::{
    compile::{ir::optimize, ir_beta::build_ir_from_graph},
    ParserConfig,
    RadlrGrammar,
    RadlrResult,
  };

  #[test]
  fn test_build_graph() -> RadlrResult<()> {
    let mut config = ParserConfig::default();
    config.ALLOW_BYTE_SEQUENCES = true;
    let db = RadlrGrammar::new()
      .add_source_from_string(
        r###"
  IGNORE { c:sp }
  

  <> C > A "dd"

  <> A > "test" "d"
  
  "###,
        "",
        false,
      )
      .unwrap()
      .build_db("", config)
      .unwrap()
      .into_internal();

    let graph = compile_parser_states(db.clone(), config)?;

    let mut ir = build_ir_from_graph(config, &db, &graph)?;

    for (_, ir) in &mut ir.1 {
      ir.build_ast(&db)?;
      println!("{}", ir.print(&db, true)?);
    }

    let ir: (Vec<_>, _) = optimize(&db, &config, ir.1, false)?;

    println!("{}", ir.1.to_string());

    for (_, ir) in &ir.0 {
      println!("{}", ir.print(&db, true)?);
    }

    Ok(())
  }
}
