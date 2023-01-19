use super::{
  create_store::{get_terminal_id, insert_production, insert_rules},
  parser::{
    sherpa::{ASTNode, Grammar},
    *,
  },
};
use crate::{
  compile::{GrammarId, GrammarStore, ProductionId, SymbolID},
  grammar::{
    create_closure,
    create_defined_scanner_name,
    create_scanner_name,
    data::ast::Import,
    get_production_start_items,
    multitask::WorkVerifier,
  },
  types::*,
  util::get_num_of_available_threads,
  Journal,
  ReportType,
  SherpaError,
};
use std::{
  collections::{btree_map, BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
  fs::read,
  num::NonZeroUsize,
  path::{Path, PathBuf},
  sync::{Arc, Mutex},
  thread,
};

pub fn grammar_from_string(
  j: &mut Journal,
  grammar: &str,
  source_path: PathBuf,
) -> Vec<(PathBuf, ImportedGrammarReferences, Box<Grammar>)> {
  match sherpa::ast::grammar_from(grammar.into()) {
    Err(err) => {
      j.report_mut().add_error(err.into());
      vec![]
    }
    Ok(grammar) => vec![(source_path, HashMap::new(), grammar)],
  }
}

const allowed_extensions: [&str; 3] = ["hc", "hcg", "grammar"];

pub(crate) fn get_usable_thread_count(requested_count: usize) -> usize {
  NonZeroUsize::min(
    NonZeroUsize::new(usize::max(1, requested_count)).unwrap(),
    std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()),
  )
  .get()
}

/// Loads all grammars that are indirectly or directly referenced from a single filepath.
/// Returns a vector grammars in no particular order except the first grammar belongs to
/// the file path
pub(crate) fn load_from_path(
  j: &mut Journal,
  absolute_path: PathBuf,
) -> (Vec<(PathBuf, ImportedGrammarReferences, Box<Grammar>)>) {
  let pending_grammar_paths =
    Mutex::new(VecDeque::<PathBuf>::from_iter(vec![absolute_path.clone()]));
  let claimed_grammar_paths = Mutex::new(HashSet::<PathBuf>::new());
  let work_verifier = Mutex::new(WorkVerifier::new(1));

  let results = thread::scope(|s| {
    [0..get_usable_thread_count(get_num_of_available_threads())]
      .into_iter()
      .map(|_| {
        let mut j = j.transfer();
        let claimed_grammar_paths = &claimed_grammar_paths;
        let work_verifier = &work_verifier;
        let pending_grammar_paths = &pending_grammar_paths;
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
                    claimed_grammar_paths.lock().as_mut().map_or(None, |d| {
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
              Some(path) => match grammar_from_path(&mut j, &path) {
                SherpaResult::Ok((grammar, imports)) => {
                  let mut imports_refs: ImportedGrammarReferences = Default::default();

                  for box sherpa::Import { uri, reference, tok } in imports {
                    let base_path = PathBuf::from(uri);
                    match resolve_grammar_path(
                      &base_path,
                      &path.parent().unwrap_or(Path::new("")).to_path_buf(),
                      &allowed_extensions,
                    ) {
                      SherpaResult::Ok(path) => {
                        imports_refs.insert(
                          reference.to_string(),
                          GrammarRef::new(reference.to_string(), path.clone()),
                        );
                        pending_grammar_paths.lock().unwrap().push_back(path);
                        work_verifier.lock().unwrap().add_units_of_work(1);
                      }
                      SherpaResult::MultipleErrors(new_errors) => {
                        for error in new_errors {
                          j.report_mut().add_error(error)
                        }
                      }
                      SherpaResult::Err(err) => {
                        j.report_mut().add_error(err);
                      }
                      SherpaResult::None => j.report_mut().add_error(SherpaError::SourceError {
                        loc:        tok,
                        path:       path.clone(),
                        id:         "nonexistent-import-source",
                        msg:        format!("Could not load {}", base_path.to_str().unwrap()),
                        inline_msg: "source not found".to_string(),
                        severity:   SherpaErrorSeverity::Critical,
                        ps_msg:     Default::default(),
                      }),
                    }
                  }

                  grammars.push((path, imports_refs, grammar));
                  {
                    work_verifier.lock().unwrap().complete_one_unit_of_work();
                  }
                }
                SherpaResult::Err(err) => j.report_mut().add_error(err),
                _ => {}
              },
              None => {
                if work_verifier.lock().unwrap().is_complete() {
                  break;
                }
              }
            }
          }
          (grammars)
        })
      })
      .map(|s| s.join().unwrap())
      .collect::<Vec<_>>()
  });

  let mut grammars = vec![];

  for (mut g) in results {
    grammars.append(&mut g);
  }

  (grammars)
}

/// Loads and parses a grammar file, returning the parsed grammar node and a vector of Import nodes.
fn grammar_from_path(
  j: &mut Journal,
  absolute_path: &PathBuf,
) -> SherpaResult<(Box<Grammar>, Vec<Box<sherpa::Import>>)> {
  match std::fs::read_to_string(absolute_path) {
    Ok(buffer) => match sherpa::ast::grammar_from(buffer.as_str().into()) {
      Ok(grammar) => {
        let import_paths = grammar
          .preamble
          .iter()
          .filter_map(|a| match a {
            ASTNode::Import(import) => Some(import.clone()),
            _ => None,
          })
          .collect();
        SherpaResult::Ok((grammar, import_paths))
      }
      Err(err) => {
        j.report_mut().add_error(err.into());
        SherpaResult::None
      }
    },
    Err(err) => SherpaResult::Err(err.into()),
  }
}

/// Resolves and verifies a grammar file path acquired from an `@IMPORT` statement exists.
///
/// If the file path does not have an extension, attempts are made to assert
/// the existence of the file path when appended with one of the following extension types
/// appended to it: `.hc`, `.hcg` `.grammar`.
///
/// Additionally, if the given file path is relative, then it is appended to the parent dir
/// path of the current grammar, whose path is provided by the `cgd`, current grammar dir,
/// argument.
pub(crate) fn resolve_grammar_path(
  path: &PathBuf,
  cgd: &PathBuf,
  extension: &[&str],
) -> SherpaResult<PathBuf> {
  SherpaResult::Ok(
    match (
      path.is_file(),
      path.extension().is_some(),
      // Ensure path is is an absolute path
      match path.is_absolute() {
        true => (path.to_owned(), false),
        false => (cgd.join(&path), cgd.join(&path).is_file()),
      },
    ) {
      // Path is relative to the given cgd
      (false, _, (path, true)) => path.canonicalize()?,
      // Attempt to verify the file path with different extensions. First valid
      // path wins.
      (false, false, (path, _)) => extension
        .iter()
        .filter_map(|ext| {
          let mut path = path.clone();
          path.set_extension(ext);
          path.canonicalize().ok()
        })
        .next()
        .ok_or(format!("Tried to load file with these extension {:?}", extension))?,

      // Default
      _ => path.canonicalize()?,
    },
  )
}
