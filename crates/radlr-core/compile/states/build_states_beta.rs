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
  NonDeterministicPeek,
  OtherErrors(Vec<RadlrError>),
}

/// Add a root node to the graph queue. This type of node is also added to the
/// global pending root nodes pool.
pub fn add_root(
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
    .make_root(name)
    .commit(builder);

  builder.commit(false, StateId::default(), &config, false);
  Ok(())
}

pub(crate) fn compile_parser_states(db: Arc<ParserDatabase>, config: ParserConfig) -> RadlrResult<Graphs> {
  // Create entry nodes.

  let mut gb = ConcurrentGraphBuilder::new(db.clone(), config);

  for result in db.nonterms().iter().enumerate().map(|(index, sym)| {
    let nt_id: DBNonTermKey = (index as u32).into();
    let kernel_items = ItemSet::start_items(nt_id, &db).to_origin(Origin::NonTermGoal(nt_id));
    add_root(
      if sym.is_term() { db.nonterm_guid_name(nt_id) } else { db.nonterm_guid_name(nt_id) },
      if sym.is_term() { GraphType::Scanner } else { GraphType::Parser },
      kernel_items,
      &mut gb,
      &config,
    )
  }) {
    result?;
  }

  #[cfg(not(feature = "wasm-target"))]
  let pool: worker_pool::StandardPool = crate::types::worker_pool::StandardPool::new(8).unwrap();
  let sync_tracker = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));

  pool.run(|_num_of_threads_| {
    let mut gb = gb.clone();

    let sync_tracker = sync_tracker.clone();

    move |thread_id| {
      let mut retries = 0;
      loop {
        if let Some(((node, config), local)) =
          gb.get_local_work().map(|w| (w, true)).or_else(|| gb.get_global_work().map(|w| (w, false)))
        {
          if retries > 0 {
            retries = 0;
            sync_tracker.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
          }

          if local {
            println!("Thread {thread_id} -- working on {:?} from local queue", node.id());
          } else {
            println!("Thread {thread_id} -- working on {:?} from global queue", node.id());
          }

          match handle_kernel_items(&mut gb, &node, &config) {
            Err(RadlrError::StateConstructionError(StateConstructionError::NonDeterministicPeek)) => {}
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
          println!("Thread {thread_id} -- sleeping for {:?}", sleep_dur);
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
  use std::collections::BTreeMap;

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
    }

    let ir: (Vec<_>, _) = optimize(&db, &config, ir.1, false)?;

    println!("{}", ir.1.to_string());

    for (_, ir) in &ir.0 {
      println!("{}", ir.print(&db, true)?);
    }

    Ok(())
  }
}
