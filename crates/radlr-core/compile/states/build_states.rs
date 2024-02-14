use super::build_graph::{
  build::handle_kernel_items,
  graph::{ConcurrentGraphBuilder, GraphType, Graphs, Origin, SharedGraphNode, StagedNode, StateType},
};
use crate::types::{worker_pool::WorkerPool, *};
use std::sync::Arc;

#[derive(Debug, Clone, Hash)]
pub(crate) enum StateConstructionError {
  NonDeterministicPeek(SharedGraphNode, Box<RadlrError>),
  OtherErrors(Vec<RadlrError>),
}

const NORMAL_GRAPH: i16 = 0;
const LR_ONLY_GRAPH: i16 = 1;

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
    .make_root(name, db_nt_key, NORMAL_GRAPH)
    .commit(builder);

  builder.commit(false, None, &config, false, false)?;
  Ok(())
}

pub(crate) fn compile_parser_states(db: Arc<ParserDatabase>, config: ParserConfig) -> RadlrResult<Arc<Graphs>> {
  // Create entry nodes.

  let mut gb = ConcurrentGraphBuilder::new(db.clone());

  for result in db.nonterms().iter().enumerate().filter_map(|(index, sym)| {
    let nt_id: DBNonTermKey = (index as u32).into();

    let kernel_items = ItemSet::start_items(nt_id, &db).to_origin(Origin::NonTermGoal(nt_id));

    if db.custom_state(nt_id).is_none() {
      Some(add_root(
        nt_id,
        db.nonterm_guid_name(nt_id),
        sym.is_term().then_some(GraphType::Scanner).unwrap_or(GraphType::Parser),
        kernel_items,
        &mut gb,
        &config,
      ))
    } else {
      None
    }
  }) {
    result?;
  }

  #[cfg(not(feature = "wasm-target"))]
  let pool = crate::types::worker_pool::StandardPool::new(20).unwrap();

  #[cfg(feature = "wasm-target")]
  let pool = crate::types::worker_pool::SingleThreadPool {};

  let sync_tracker = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));

  build_states(&pool, &gb, sync_tracker)?;

  let graphs: Arc<Graphs> = Arc::new(gb.into());

  //let (states) = build_ir_concurrent(&pool, graphs.clone(), config, &db)?;

  // let (states, report) = optimize::<ParseStatesVec>(&db, &config, states,
  // false)?;

  //let r = states.iter().map(|s| s.1.print(&db,
  // false).unwrap()).collect::<Vec<_>>().join("\n"); println!("DONE: {} \n ",
  // report.to_string());

  for state in graphs.create_ir_precursors().iter() {
    //println!("{:?}", state.node);
  }

  Ok(graphs)
}

fn build_states<T: WorkerPool>(
  pool: &T,
  gb: &ConcurrentGraphBuilder,
  sync_tracker: Arc<std::sync::atomic::AtomicUsize>,
) -> Result<(), RadlrError> {
  pool.run(|_num_of_threads_| {
    let mut gb = gb.clone();

    let sync_tracker = sync_tracker.clone();
    let have_errors = std::sync::atomic::AtomicBool::new(false);
    let error_count = _num_of_threads_ + 1;

    move |thread_id: usize| {
      let mut retries = 0;
      loop {
        let have_errors = &have_errors;

        if let Some(((node, config), _is_local_work)) =
          gb.get_local_work().map(|w| (w, true)).or_else(|| gb.get_global_work().map(|w| (w, false)))
        {
          if retries > 0 {
            retries = 0;
            sync_tracker.fetch_sub(1, std::sync::atomic::Ordering::Release);
          }

          match handle_kernel_items(&mut gb, &node, &config) {
            Err(RadlrError::StateConstructionError(StateConstructionError::NonDeterministicPeek(root, _err))) => {
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
                let db = gb.db_rc().clone();

                node.get_root().invalid.store(true, std::sync::atomic::Ordering::Relaxed);

                gb.abandon_uncommited();
                let mut poisoned = vec![root.root_data.db_key];

                if db.entry_nterm_map().contains_key(&nonterm_key) {
                  // Ensures all other threads are halted when the active queue is exhausted.
                  have_errors.store(true, std::sync::atomic::Ordering::Release);
                  sync_tracker.fetch_add(error_count, std::sync::atomic::Ordering::Release);
                  return Err(
                    format!("Entry parser {} is non-deterministic", db.nonterm_friendly_name_string(nonterm_key),).into(),
                  );
                } else {
                  if let Some(nonterms) = db.get_nonterminal_predecessors(nonterm_key) {
                    poisoned.extend(nonterms.iter());

                    match gb.invalidate_nonterms(&poisoned, NORMAL_GRAPH) {
                      Err(err) => {
                        sync_tracker.fetch_add(error_count, std::sync::atomic::Ordering::Relaxed);
                        return Err(err);
                      }
                      _ => {}
                    };

                    for new_nt_key in nonterms {
                      let mut new_config = config;
                      new_config.ALLOW_LR = true;
                      new_config.ALLOW_CALLS = false;

                      let kernel_items = ItemSet::start_items(*new_nt_key, &db).to_origin(Origin::NonTermGoal(*new_nt_key));

                      StagedNode::new(&gb)
                        .kernel_items(kernel_items.to_vec().into_iter())
                        .ty(StateType::Start)
                        .graph_ty(node.graph_type())
                        .make_root(db.nonterm_guid_name(*new_nt_key), *new_nt_key, LR_ONLY_GRAPH)
                        .commit(&mut gb);

                      gb.commit(false, None, &new_config, true, false)?;
                    }
                  } else {
                    sync_tracker.fetch_add(error_count, std::sync::atomic::Ordering::Relaxed);
                    panic!("Somehow encountered an orphaned non-terminal");
                  }
                }
              }
            }
            Err(_) => {
              have_errors.store(true, std::sync::atomic::Ordering::Release);

              // Ensures other threads are halted when the active queue is exhausted.
              sync_tracker.fetch_add(error_count, std::sync::atomic::Ordering::Relaxed);
              return Err("Todo: Report critical failure during compilation: {err}".into());
            }
            _ => {}
          }
        } else {
          let num_of_waiting_threads = if retries == 0 {
            let val = sync_tracker.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
            val + 1
          } else {
            sync_tracker.load(std::sync::atomic::Ordering::Acquire)
          };

          if num_of_waiting_threads == _num_of_threads_ {
            break;
          } else if num_of_waiting_threads > _num_of_threads_ {
            return Err("Error detected in another thread".into());
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

  Ok(())
}

#[cfg(test)]
mod test {
  #![allow(unused)]
  use radlr_rust_runtime::types::StringInput;
  use std::path::PathBuf;

  use super::compile_parser_states;
  use crate::{
    compile::ir::{build_ir_from_graph, optimize},
    ParserConfig,
    RadlrGrammar,
    RadlrResult,
    TestPackage,
  };

  #[test]
  fn build_json_graph() -> RadlrResult<()> {
    let mut config = ParserConfig::default();
    config.ALLOW_BYTE_SEQUENCES = true;
    let db = RadlrGrammar::new()
      .add_source_from_string(include_str!("../../../../grammars/json/json.sg"), "", false)
      .unwrap()
      .build_db("", config)
      .unwrap()
      .into_internal();

    let graph = compile_parser_states(db.clone(), config)?;

    let mut ir = build_ir_from_graph(config, &db, &graph)?;

    for (_, ir) in &mut ir.1 {
      ir.build_ast(&db)?;
      // println!("{}", ir.print(&db, true)?);
    }

    let ir: (Vec<_>, _) = optimize(&db, &config, ir.1, false)?;

    println!("{}", ir.1.to_string());

    for (_, ir) in &ir.0 {
      println!("{} {}", ir.get_canonical_hash(&db, false)?, ir.print(&db, true)?);
    }

    Ok(())
  }

  #[test]
  pub fn peek_hybrid_graph() -> RadlrResult<()> {
    let mut config = ParserConfig::default().enable_fork(true);
    config.ALLOW_BYTE_SEQUENCES = false;
    config.ALLOW_SCANNER_INLINING = false;
    let db = RadlrGrammar::new()
      .add_source_from_string(
        r#"
        IGNORE { c:sp c:nl }

        <> element_block > '<' component_identifier
            ( element_attribute(+) )?
            ( element_attributes | general_data | element_block | general_binding )(*)
            ">"
        
        <> component_identifier >
            identifier ( ':' identifier )?
        
        <> element_attributes >c:nl element_attribute(+)
        
        <> element_attribute > '-' identifier attribute_chars c:sp
        
        
            | '-' identifier ':' identifier
        
            | '-' "store" '{' local_values? '}'
            | '-' "local" '{' local_values? '}'
            | '-' "param" '{' local_values? '}'
            | '-' "model" '{' local_values? '}'
        
        <> general_binding > ':' identifier
        
        <> local_values > local_value(+)
        
        <> local_value > identifier ( '`' identifier )? ( '='  c:num )? ( ',' )(*)
        
        
        <> attribute_chars > ( c:id | c:num | c:sym  )(+)
        <> general_data > ( c:id | c:num  | c:nl  )(+)
        
        <> identifier > tk:tok_identifier
        
        <> tok_identifier > ( c:id | c:num )(+)"#,
        "",
        false,
      )
      .unwrap()
      .build_db("", config)
      .unwrap()
      .into_internal();

    let graph = compile_parser_states(db.clone(), config)?;

    let mut ir = build_ir_from_graph(config, &db, &graph)?;

    //return Ok(());

    for (_, ir) in &ir.1 {
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
