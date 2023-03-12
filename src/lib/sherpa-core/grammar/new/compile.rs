#![allow(unused_mut, unused)]

use super::{finalize::finalize_grammar, merge::merge_grammars};
use crate::{
  compile::GrammarStore,
  grammar::{
    multitask::WorkVerifier,
    new::load::{load, pre_load},
  },
  Journal,
  ReportType,
  SherpaResult,
};
use std::{
  collections::{HashSet, VecDeque},
  path::PathBuf,
  sync::Mutex,
};

pub fn compile_grammar_from_str(
  j: &mut Journal,
  grammar_source: &str,
  faux_grammar_path: &PathBuf,
) -> SherpaResult<Box<GrammarStore>> {
  let (mut store, foreign_grammars) = load_runner(j, grammar_source, faux_grammar_path)?;

  merge_grammars(j, &mut store, &foreign_grammars);

  finalize_grammar(j, &mut store);

  SherpaResult::Ok(store)
}

pub fn compile_grammar_from_path(
  j: &mut Journal,
  grammar_path: &PathBuf,
) -> SherpaResult<Box<GrammarStore>> {
  match std::fs::read_to_string(grammar_path) {
    Err(err) => SherpaResult::Err(err.into()),
    Ok(string) => compile_grammar_from_str(j, &string, grammar_path),
  }
}

fn load_runner(
  j: &mut Journal,
  grammar_source: &str,
  path: &PathBuf,
) -> SherpaResult<(Box<GrammarStore>, Vec<Box<GrammarStore>>)> {
  let (mut root_store, root_grammar) = pre_load(j, &grammar_source, &path)?;

  let mut pending_grammar_paths =
    VecDeque::<PathBuf>::from_iter(root_store.imports.values().map(|i| i.path.clone()));
  let mut claimed_grammar_paths = HashSet::<PathBuf>::new();

  #[cfg(feature = "multithread")]
  {
    use crate::util::get_num_of_available_threads;

    let number_of_threads = get_num_of_available_threads();
    let pending_grammar_paths = Mutex::new(pending_grammar_paths);
    let claimed_grammar_paths = Mutex::new(claimed_grammar_paths);
    let work_verifier = Mutex::new(WorkVerifier::new(1));

    let results = std::thread::scope(|s| {
      let temp_j = j.transfer();
      let threads = [0..number_of_threads].into_iter().map(|_| {
        let mut j = temp_j.transfer();
        let claimed_grammar_paths = &claimed_grammar_paths;
        let work_verifier = &work_verifier;
        let pending_grammar_paths = &pending_grammar_paths;
        j.set_active_report("File Load", ReportType::GrammarCompile(Default::default()));
        s.spawn(move || {
          let mut grammars = vec![];
          loop {
            match {
              {
                let val = pending_grammar_paths
                  .lock()
                  .unwrap()
                  .pop_front()
                  .and_then(|path| {
                    (&claimed_grammar_paths).lock().as_mut().map_or(None, |d| {
                      let mut work_verifier = work_verifier.lock().unwrap();
                      if d.insert(path.clone()) {
                        work_verifier.start_one_unit_of_work();
                        Some(path)
                      } else {
                        work_verifier.skip_one_unit_of_work();
                        None
                      }
                    })
                  })
                  .clone();
                val
              }
            } {
              Some(path) => {
                match pre_load(&mut j, &std::fs::read_to_string(&path).unwrap(), &path) {
                  SherpaResult::Ok((mut store, grammar)) => {
                    for import in &store.imports {
                      pending_grammar_paths.lock().unwrap().push_back(import.1.path.clone());
                      work_verifier.lock().unwrap().add_units_of_work(1);
                    }

                    load(&mut j, &mut store, &grammar); // Don't care about result

                    grammars.push(store);
                  }
                  SherpaResult::Err(err) => {
                    j.report_mut().add_error(err);
                  }
                  _ => {}
                }

                work_verifier.lock().unwrap().complete_one_unit_of_work();
              }
              None => {
                if work_verifier.lock().unwrap().is_complete() {
                  break grammars;
                }
              }
            }
          }
        })
      });

      load(j, &mut root_store, &root_grammar);

      (root_store, threads.map(|s| s.join().unwrap()).flatten().collect::<Vec<_>>())
    });

    SherpaResult::Ok(results)
  }
  #[cfg(not(feature = "multithread"))]
  {
    load(j, &mut root_store, &root_grammar);

    let mut grammars = vec![];

    while let Some(path) = pending_grammar_paths.pop_back() {
      if claimed_grammar_paths.insert(path.clone()) {
        match pre_load(j, &std::fs::read_to_string(&path).unwrap(), &path) {
          SherpaResult::Ok((mut store, grammar)) => {
            for import in &store.imports {
              pending_grammar_paths.push_back(import.1.path.clone());
            }

            load(j, &mut store, &grammar); // Don't care about result

            grammars.push(store);
          }
          SherpaResult::Err(err) => {
            j.report_mut().add_error(err);
          }
          _ => {}
        }
      }
    }

    SherpaResult::Ok((root_store, grammars))
  }
}

#[test]
fn load_grammar_from_str() -> SherpaResult<()> {
  let grammar_source = r#"
  
  <> A > "hello" "," "World" pratt_test "!"

  pratt_test catches => pass
  
  "#;

  let mut j = Journal::new(None);

  j.set_active_report("test", ReportType::Any);

  let store = compile_grammar_from_str(&mut j, grammar_source, &PathBuf::from("/"))?;

  j.flush_reports();

  assert!(!j.debug_error_report());

  dbg!(store);

  SherpaResult::Ok(())
}
