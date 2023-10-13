use super::build_graph::{
  build,
  graph::{GraphType, Origin, ReversedGraph, ScannerData},
};
use crate::{
  journal::Journal,
  types::{worker_pool::WorkerPool, *},
  utils::create_u64_hash,
};
use std::sync::{Arc, RwLock};

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTermGraph {
  pub(crate) name:           IString,
  pub(crate) is_root_goal:   bool,
  pub(crate) nonterm_id:     DBNonTermKey,
  pub(crate) graph:          Option<ReversedGraph>,
  pub(crate) lr_only:        bool,
  pub(crate) classification: ParserClassification,
  pub(crate) start_items:    OrderedSet<Item>,
}

#[allow(unused)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ScannerGraph {
  pub(crate) name:           IString,
  pub(crate) data:           Arc<ScannerData>,
  pub(crate) graph:          Option<ReversedGraph>,
  pub(crate) classification: ParserClassification,
  pub(crate) start_items:    OrderedSet<Item>,
}

pub enum StateConstructionError<T> {
  NonDeterministicPeek(T, Vec<SherpaError>),
  OtherErrors(T, Vec<SherpaError>),
}

#[derive(Clone, Copy)]
struct NonTermGraphJob {
  nonterm_id:    DBNonTermKey,
  lr_only:       bool,
  is_root_entry: bool,
  created:       usize,
  completed:     usize,
  valid:         bool,
  is_scanner:    bool,
}

#[repr(align(64))]
#[derive(Default)]
struct NonTermGraphJobEntry(Option<std::sync::RwLock<NonTermGraphJob>>);

#[repr(align(64))]
#[derive(Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
struct NonTermGraphResultEntry(std::sync::RwLock<Option<Box<NonTermGraph>>>);

pub(crate) fn compile_parser_states(
  mut j: Journal,
  db: Arc<ParserDatabase>,
  config: ParserConfig,
) -> SherpaResult<(Arc<Vec<Box<NonTermGraph>>>, Arc<Vec<Box<ScannerGraph>>>)> {
  j.set_active_report("States Compile", crate::ReportType::AnyNonTermCompile);

  let entry_points = db.entry_nterm_map();
  let (nonterm_job_board, nonterm_results) = db
    .nonterms()
    .iter()
    .enumerate()
    .map(|(index, sym)| {
      let id = DBNonTermKey::from(index);
      if db.custom_state(id).is_some() {
        (Default::default(), Default::default())
      } else {
        let result = NonTermGraphJob {
          created:       1,
          completed:     0,
          is_root_entry: entry_points.get(&id).is_some_and(|e| e.is_export),
          lr_only:       config.ALLOW_LR && !config.ALLOW_CALLS,
          nonterm_id:    id,
          valid:         true,
          is_scanner:    sym.is_term(),
        };
        if sym.is_term() {
          //TODO: We'll process scanners separately.
          (NonTermGraphJobEntry(Some(RwLock::new(result))), Default::default())
        } else {
          if !config.ALLOW_CALLS {
            // If we can't make calls to another non-terminal parse graph then it
            // doesn't make sense to create parse graphs for non-terminals
            // that are not exported by the grammar, as the root of those graphs
            // will be unreachable.
            if db.entry_nterm_keys().contains(&id) {
              (NonTermGraphJobEntry(Some(std::sync::RwLock::new(result))), Default::default())
            } else {
              (Default::default(), Default::default())
            }
          } else {
            (NonTermGraphJobEntry(Some(RwLock::new(result))), Default::default())
          }
        }
      }
    })
    .unzip::<_, _, Array<NonTermGraphJobEntry>, Array<NonTermGraphResultEntry>>();

  let scanner_inbox = Arc::new(RwLock::new(OrderedMap::<u64, Arc<ScannerData>>::new()));
  let scanners = Arc::new(RwLock::new(Array::new()));

  let nonterm_job_board = Arc::new(nonterm_job_board);
  let parsers = Arc::new(nonterm_results);

  let generation = Arc::new(std::sync::atomic::AtomicU32::new(1));
  let pool = crate::types::worker_pool::StandardPool::new(20).unwrap();
  //let pool = crate::types::worker_pool::SingleThreadPool {};

  while generation.load(std::sync::atomic::Ordering::Acquire) > 0 {
    generation.store(0, std::sync::atomic::Ordering::Release);
    let indexer = Arc::new(std::sync::atomic::AtomicU32::new(0));
    pool.run(|_num_of_threads_| {
      let job_board = nonterm_job_board.clone();
      let results = parsers.clone();
      let indexer = indexer.clone();
      let generation = generation.clone();
      let db = db.clone();
      let scanner_inbox = scanner_inbox.clone();
      let scanners = scanners.clone();
      move |thread_id| {
        let mut local_scanner_inbox = OrderedMap::new();
        let indexer = indexer.as_ref();
        let generation = generation.as_ref();
        let board_size = job_board.len();
        let db_ref = db.as_ref();
        let mut job_errors = vec![];
        let mut local_scanners = Array::new();
        loop {
          // acquire the next pointer
          let job_index = indexer.fetch_add(1, std::sync::atomic::Ordering::Relaxed) as usize;

          if job_index < board_size {
            // We have a job! let's go ahead and process it.
            if let NonTermGraphJobEntry(Some(job_flyer)) = &job_board[job_index] {
              let Ok(pending_job) = job_flyer.write().map(|mut d| {
                if d.created > d.completed && d.valid {
                  d.completed = d.created;
                  Some(*d)
                } else {
                  None
                }
              }) else {
                return Err(SherpaError::Text(
                  "Graph compiler worker [".to_string() + &thread_id.to_string() + "] encountered a poisoned job. Exiting",
                ));
              };

              if let Some(NonTermGraphJob {
                nonterm_id, lr_only, is_root_entry: is_root_goal, valid, is_scanner, ..
              }) = pending_job
              {
                if !is_scanner {
                  let graph = Box::new(NonTermGraph {
                    name: db_ref.nonterm_guid_name(nonterm_id),
                    is_root_goal,
                    nonterm_id,
                    graph: None,
                    lr_only,
                    classification: Default::default(),
                    start_items: ItemSet::start_items(nonterm_id, db_ref).to_origin(Origin::NonTermGoal(nonterm_id)),
                  });

                  match process_nonterm_states(&db, config, graph) {
                    Ok(graph) => {
                      // we should make sure we don't need to rebuild this.
                      if let Some(graph) = &graph.graph {
                        for scanner in graph.iter_scanners() {
                          local_scanner_inbox.insert(scanner.hash, Arc::new(scanner.clone()));
                        }
                      }

                      if results[job_index]
                        .0
                        .write()
                        .and_then(move |mut f| {
                          *f = Some(graph);
                          Ok(())
                        })
                        .is_err()
                      {
                        return Err(SherpaError::Text(
                          "Graph compiler worker [".to_string()
                            + &thread_id.to_string()
                            + "] encountered a poisoned job. Exiting",
                        ));
                      }
                    }

                    Err(StateConstructionError::NonDeterministicPeek(work, errors)) => {
                      // Stop the world, find all non-terms that include this
                      //#[cfg(debug_assertions)]
                      //eprintln!(
                      //  "\nCould not create peek states for: {}.\nThis will effect parser extensions
                      // that require random non-terminal entries\n",
                      //  db_ref.nonterm_friendly_name_string(nonterm_id)
                      //);

                      // Invalidate the job we currently have.
                      if let NonTermGraphJobEntry(Some(job_flyer)) = &job_board[job_index] {
                        let Ok(_) = job_flyer.write().map(|mut d| {
                          d.valid = false;
                          ()
                        }) else {
                          return Err(SherpaError::Text(
                            "Graph compiler worker [".to_string()
                              + &thread_id.to_string()
                              + "] encountered a poisoned job. Exiting",
                          ));
                        };
                      }

                      // Proceed to update all non-terminals that reference the one we currently have.
                      if config.ALLOW_LR {
                        if work.is_root_goal {
                          job_errors.extend(errors.into_iter());
                        } else {
                          if let Some(nonterms) = db_ref.get_nonterminal_predecessors(nonterm_id) {
                            for nonterm_id in nonterms {
                              let job_index = nonterm_id.to_val() as usize;
                              if let NonTermGraphJobEntry(Some(job_flyer)) = &job_board[job_index] {
                                let Ok(_) = job_flyer.write().map(|mut d| {
                                  d.created += 2;
                                  d.lr_only = true;
                                }) else {
                                  return Err(SherpaError::Text(
                                    "Graph compiler worker [".to_string()
                                      + &thread_id.to_string()
                                      + "] encountered a poisoned job. Exiting",
                                  ));
                                };
                              }
                              generation.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                            }
                          }
                        }
                      } else {
                        job_errors.extend(errors.into_iter());
                      }
                    }

                    Err(StateConstructionError::OtherErrors(_, errors)) => {
                      job_errors.extend(errors.into_iter());
                    }
                  }
                } else {
                  let hash = create_u64_hash(nonterm_id);
                  let graph: Box<ScannerGraph> = Box::new(ScannerGraph {
                    classification: Default::default(),
                    name:           db.nonterm_guid_name(nonterm_id),
                    data:           Arc::new(ScannerData {
                      hash,
                      symbols: Default::default(),
                      skipped: Default::default(),
                      follow: Default::default(),
                    }),
                    start_items:    ItemSet::start_items(nonterm_id, db_ref).to_origin(Origin::NonTermGoal(nonterm_id)),
                    graph:          None,
                  });

                  match process_scanner_states(&db, config, graph) {
                    Ok(graph) => {
                      local_scanners.push(graph);
                    }
                    Err(StateConstructionError::OtherErrors(_, errors)) => {
                      job_errors.extend(errors.into_iter());
                    }
                    Err(StateConstructionError::NonDeterministicPeek(..)) => {
                      unreachable!();
                    }
                  }
                }
              }
            }
          } else {
            if local_scanners.len() > 0 {
              if scanners
                .write()
                .map(|mut d| {
                  d.extend(local_scanners.into_iter());
                })
                .is_err()
              {
                return Err(SherpaError::Text(
                  "Graph compiler worker [".to_string() + &thread_id.to_string() + "] encountered a poisoned job. Exiting",
                ));
              }
            }
            if local_scanner_inbox.len() > 0 {
              if scanner_inbox
                .write()
                .map(|mut d| {
                  d.extend(local_scanner_inbox.into_iter());
                })
                .is_err()
              {
                return Err(SherpaError::Text(
                  "Graph compiler worker [".to_string() + &thread_id.to_string() + "] encountered a poisoned job. Exiting",
                ));
              }
            }
            if job_errors.len() > 0 {
              return Err(SherpaError::Multi(job_errors).flattened_multi());
            } else {
              return Ok(());
            }
          }
        }
      }
    })?;
  }

  let indexer = Arc::new(std::sync::atomic::AtomicU32::new(0));
  let scanner_inbox = Arc::new(Arc::into_inner(scanner_inbox).unwrap().into_inner().unwrap().into_values().collect::<Array<_>>());
  pool.run(|_num_of_threads_| {
    let scanner_inbox = scanner_inbox.clone();
    let indexer = indexer.clone();
    let scanners = scanners.clone();
    let db = db.clone();
    let mut job_errors = vec![];
    move |thread_id| {
      let indexer = indexer.as_ref();
      let board_size = scanner_inbox.len();
      let db_ref = db.as_ref();
      let mut local_scanners = Array::new();
      loop {
        // acquire the next pointer
        let job_index = indexer.fetch_add(1, std::sync::atomic::Ordering::Relaxed) as usize;

        if job_index < board_size {
          let scanner = scanner_inbox[job_index].clone();

          let graph: Box<ScannerGraph> = Box::new(ScannerGraph {
            classification: Default::default(),
            name:           get_scanner_name(&scanner, db_ref),
            data:           scanner.clone(),
            graph:          None,
            start_items:    scanner
              .symbols
              .iter()
              .flat_map(|s| {
                debug_assert!(!db.token(s.tok()).sym_id.is_default(), "Default symbols should not be in scanners");
                ItemSet::start_items(db.token(s.tok()).nonterm_id, db_ref)
                  .to_origin(Origin::TerminalGoal(s.tok(), s.precedence()))
              })
              .collect::<ItemSet>(),
          });

          match process_scanner_states(&db, config, graph) {
            Ok(graph) => {
              local_scanners.push(graph);
            }
            Err(StateConstructionError::OtherErrors(_, errors)) => {
              job_errors.extend(errors.into_iter());
            }
            Err(StateConstructionError::NonDeterministicPeek(..)) => {
              unreachable!();
            }
          }
        } else {
          if local_scanners.len() > 0 {
            if scanners
              .write()
              .map(|mut d| {
                d.extend(local_scanners.into_iter());
              })
              .is_err()
            {
              return Err(SherpaError::Text(
                "Graph compiler worker [".to_string() + &thread_id.to_string() + "] encountered a poisoned job. Exiting",
              ));
            }
          }
          if job_errors.len() > 0 {
            return Err(SherpaError::Multi(job_errors).flattened_multi());
          } else {
            return Ok(());
          }
        }
      }
    }
  })?;

  Ok((
    Arc::new(Arc::into_inner(parsers).unwrap().into_iter().filter_map(|e| e.0.into_inner().unwrap()).collect::<Array<_>>()),
    Arc::new(Arc::into_inner(scanners).unwrap().into_inner().unwrap()),
  ))
}

pub fn get_scanner_name(scanner: &ScannerData, db: &ParserDatabase) -> IString {
  ("scan".to_string() + &scanner.hash.to_string()).intern(db.string_store())
}

fn process_nonterm_states(
  db: &SharedParserDatabase,
  mut config: ParserConfig,
  mut work: Box<NonTermGraph>,
) -> Result<Box<NonTermGraph>, StateConstructionError<Box<NonTermGraph>>> {
  let nterm_key = work.nonterm_id;

  if work.lr_only {
    config.ALLOW_CALLS = false;
  }

  match build(db.nonterm_guid_name(nterm_key), GraphType::Parser, work.start_items.clone(), db, config) {
    Ok((class, graph)) => {
      work.classification |= class;

      work.graph = Some(ReversedGraph::new(graph));

      Ok(work)
    }
    Err(StateConstructionError::NonDeterministicPeek(_, errors)) => {
      Err(StateConstructionError::NonDeterministicPeek(work, errors))
    }
    Err(StateConstructionError::OtherErrors(_, errors)) => Err(StateConstructionError::OtherErrors(work, errors)),
  }
}

fn process_scanner_states(
  db: &SharedParserDatabase,
  config: ParserConfig,
  mut work: Box<ScannerGraph>,
) -> Result<Box<ScannerGraph>, StateConstructionError<Box<ScannerGraph>>> {
  match build(work.name, GraphType::Scanner, work.start_items.clone(), db, config) {
    Ok((_, graph)) => {
      work.graph = Some(ReversedGraph::new(graph));

      Ok(work)
    }
    Err(StateConstructionError::NonDeterministicPeek(_, errors)) => {
      Err(StateConstructionError::NonDeterministicPeek(work, errors))
    }
    Err(StateConstructionError::OtherErrors(_, errors)) => Err(StateConstructionError::OtherErrors(work, errors)),
  }
}
