use super::build_graph::{
  build,
  graph::{GraphType, Origin, ReversedGraph, ScannerData},
};
use crate::{journal::Journal, types::*, utils::create_u64_hash};
use std::collections::HashSet;

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct NonTermGraph<'db> {
  pub(crate) name:           IString,
  pub(crate) is_root_goal:   bool,
  pub(crate) nonterm_id:     DBNonTermKey,
  pub(crate) graph:          Option<ReversedGraph<'db>>,
  pub(crate) lr_only:        bool,
  pub(crate) classification: ParserClassification,
  pub(crate) start_items:    OrderedSet<Item<'db>>,
}

#[allow(unused)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ScannerGraph<'db> {
  pub(crate) name:           IString,
  pub(crate) data:           ScannerData,
  pub(crate) graph:          Option<ReversedGraph<'db>>,
  pub(crate) classification: ParserClassification,
  pub(crate) start_items:    OrderedSet<Item<'db>>,
}

pub enum StateConstructionError<T> {
  NonDeterministicPeek(T, Vec<SherpaError>),
  OtherErrors(T, Vec<SherpaError>),
}

pub fn compile_parser_states<'db>(
  mut j: Journal,
  db: &'db ParserDatabase,
  config: ParserConfig,
) -> SherpaResult<(Vec<Box<NonTermGraph<'db>>>, Vec<Box<ScannerGraph<'db>>>)> {
  j.set_active_report("States Compile", crate::ReportType::AnyNonTermCompile);

  let mut states_inbox = OrderedMap::<DBNonTermKey, Box<NonTermGraph<'db>>>::new();
  let mut parser_graph_outbox = OrderedMap::<DBNonTermKey, Box<NonTermGraph<'db>>>::new();
  let mut scanner_inbox = OrderedMap::<u64, Box<ScannerGraph<'db>>>::new();
  let mut scanner_outbox = OrderedMap::<u64, Box<ScannerGraph<'db>>>::new();
  let mut lr_only_list = HashSet::new();

  // Prepare our queues, insert entry nonterminals
  let entry_points = db.entry_nterm_map();
  for (nonterm_id, is_root_goal) in db.nonterms().iter().enumerate().filter_map(|(index, sym)| {
    if sym.is_term() {
      //TODO: We'll process scanners independently.
      None
    } else {
      let id = DBNonTermKey::from(index);
      let is_root_goal = entry_points.get(&id).is_some_and(|e| e.is_export);
      if !config.ALLOW_CALLS {
        // If we can't make calls to another non-terminal parse graph then it
        // doesn't make sense to create parse graphs for non-terminals
        // that are not exported by the grammar, as the root of those graphs
        // will be unreachable.
        db.entry_nterm_keys().contains(&id).then_some((id, is_root_goal))
      } else {
        Some((id, is_root_goal))
      }
    }
  }) {
    states_inbox.insert(
      nonterm_id,
      Box::new(NonTermGraph {
        name: db.nonterm_guid_name(nonterm_id),
        is_root_goal,
        nonterm_id,
        graph: None,
        lr_only: config.ALLOW_LR && !config.ALLOW_CALLS,
        classification: Default::default(),
        start_items: ItemSet::start_items(nonterm_id, db).to_origin(Origin::NonTermGoal(nonterm_id)),
      }),
    );
  }

  // Insert nonterminals tokens into scanner states
  for (_, nonterm_tok_id) in db.nonterms().iter().enumerate().filter_map(|(index, sym)| {
    if sym.is_term() {
      Some((SymbolId::DBNonTerminalToken { nonterm_key: DBNonTermKey::from(index), sym_key: None }, DBNonTermKey::from(index)))
    } else {
      None
    }
  }) {
    let hash = create_u64_hash(nonterm_tok_id);

    let work = Box::new(ScannerGraph {
      classification: Default::default(),
      name:           db.nonterm_guid_name(nonterm_tok_id),
      data:           ScannerData {
        hash,
        symbols: Default::default(),
        skipped: Default::default(),
        follow: Default::default(),
      },
      start_items:    ItemSet::start_items(nonterm_tok_id, db).to_origin(Origin::NonTermGoal(nonterm_tok_id)),
      graph:          None,
    });

    scanner_inbox.insert(hash, work);
  }

  while let Some((id, mut work)) = states_inbox.pop_first() {
    work.lr_only |= lr_only_list.contains(&id);

    match process_nonterm_states(db, config, work) {
      Ok(mut work) => {
        if !work.lr_only && lr_only_list.contains(&id) {
          #[cfg(debug_assertions)]
          println!("Resubmitting {} due to LR config change", db.nonterm_friendly_name_string(id));
          // Resubmit work.
          work.lr_only = true;
          work.graph = None;
          states_inbox.insert(id, work);
        } else {
          if let Some(graph) = &work.graph {
            for scanner in graph.iter_scanners() {
              scanner_inbox.insert(
                scanner.hash,
                Box::new(ScannerGraph {
                  classification: Default::default(),
                  name:           get_scanner_name(scanner, db),
                  data:           scanner.clone(),
                  graph:          None,
                  start_items:    scanner
                    .symbols
                    .iter()
                    .flat_map(|s| {
                      debug_assert!(!db.token(s.tok()).sym_id.is_default(), "Default symbols should not be in scanners");
                      ItemSet::start_items(db.token(s.tok()).nonterm_id, db)
                        .to_origin(Origin::TerminalGoal(s.tok(), s.precedence()))
                    })
                    .collect::<ItemSet>(),
                }),
              );
            }
            parser_graph_outbox.insert(id, work);
          }
        }
      }
      Err(StateConstructionError::NonDeterministicPeek(work, errors)) => {
        // Stop the world, find all non-terms that include this
        #[cfg(debug_assertions)]
        eprintln!(
          "\nCould not create peek states for: {}.\nThis will effect parser extensions that require random non-terminal entries\n",
          db.nonterm_friendly_name_string(id)
        );

        if config.ALLOW_LR {
          // Rebuild effected states
          if work.is_root_goal {
            for err in errors {
              j.report_mut().add_error(err)
            }
          } else {
            if let Some(nonterms) = db.get_nonterminal_predecessors(id) {
              let mut new_work = vec![];
              lr_only_list.extend(nonterms.iter().cloned());

              for key in &lr_only_list {
                match parser_graph_outbox.entry(*key) {
                  std::collections::btree_map::Entry::Occupied(e) => {
                    if !e.get().lr_only {
                      let mut work = e.remove();
                      #[cfg(debug_assertions)]
                      println!("-- Resubmitting {} due to LR config change", db.nonterm_friendly_name_string(work.nonterm_id));
                      work.graph = None;
                      work.lr_only = true;
                      new_work.push((*key, work));
                    }
                  }
                  _ => {}
                }
              }

              states_inbox.extend(new_work.into_iter())
            }
          }
        } else {
          for err in errors {
            j.report_mut().add_error(err)
          }
        }
      }
      Err(StateConstructionError::OtherErrors(_, errors)) => {
        for err in errors {
          j.report_mut().add_error(err)
        }
      }
    }
  }

  while let Some((id, work)) = scanner_inbox.pop_first() {
    match process_scanner_states(db, config, work) {
      Ok(work) => {
        scanner_outbox.insert(id, work);
      }
      Err(StateConstructionError::OtherErrors(_, errors)) => {
        for err in errors {
          j.report_mut().add_error(err)
        }
      }
      Err(StateConstructionError::NonDeterministicPeek(..)) => {
        unreachable!();
      }
    }
  }

  j.report_mut().wrap_ok_or_return_errors((parser_graph_outbox.into_values().collect(), scanner_outbox.into_values().collect()))
}

pub fn get_scanner_name<'db>(scanner: &ScannerData, db: &'db ParserDatabase) -> IString {
  ("scan".to_string() + &scanner.hash.to_string()).intern(db.string_store())
}

fn process_nonterm_states<'db>(
  db: &'db ParserDatabase,
  mut config: ParserConfig,
  mut work: Box<NonTermGraph<'db>>,
) -> Result<Box<NonTermGraph<'db>>, StateConstructionError<Box<NonTermGraph<'db>>>> {
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

fn process_scanner_states<'db>(
  db: &'db ParserDatabase,
  config: ParserConfig,
  mut work: Box<ScannerGraph<'db>>,
) -> Result<Box<ScannerGraph<'db>>, StateConstructionError<Box<ScannerGraph<'db>>>> {
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
