use super::parser::{
  sherpa::{ASTNode, Grammar},
  *,
};
#[allow(unused)]
use crate::{
  grammar::multitask::WorkVerifier,
  types::*,
  Journal,
  ReportType,
  SherpaError,
};
#[allow(unused)]
use std::{
  collections::{HashMap, HashSet, VecDeque},
  num::NonZeroUsize,
  path::{Path, PathBuf},
  sync::Mutex,
  thread,
};

pub fn load_from_string(
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

#[cfg(not(any(feature = "wasm-target", feature = "single-thread")))]
pub(crate) fn get_usable_thread_count(requested_count: usize) -> usize {
  NonZeroUsize::min(
    NonZeroUsize::new(usize::max(1, requested_count)).unwrap(),
    std::thread::available_parallelism()
      .unwrap_or(NonZeroUsize::new(1).unwrap()),
  )
  .get()
}

const allowed_extensions: [&str; 1] = ["sg"];

/// Loads all grammars that are indirectly or directly referenced from a single
/// filepath. Returns a vector grammars in no particular order except the first
/// grammar belongs to the file path
pub(crate) fn load_from_path(
  j: &mut Journal,
  absolute_path: PathBuf,
) -> Vec<(PathBuf, ImportedGrammarReferences, Box<Grammar>)> {
  let pending_grammar_paths =
    Mutex::new(VecDeque::<PathBuf>::from_iter(vec![absolute_path.clone()]));
  let claimed_grammar_paths = Mutex::new(HashSet::<PathBuf>::new());
  let work_verifier = Mutex::new(WorkVerifier::new(1));

  #[cfg(not(any(feature = "wasm-target", feature = "single-thread")))]
  let grammars = {
    use crate::util::get_num_of_available_threads;
    let results = std::thread::scope(|s| {
      [0..get_usable_thread_count(get_num_of_available_threads())]
        .into_iter()
        .map(|_| {
          let mut j = j.transfer();
          let claimed_grammar_paths = &claimed_grammar_paths;
          let work_verifier = &work_verifier;
          let pending_grammar_paths = &pending_grammar_paths;
          j.set_active_report(
            "File Load",
            ReportType::GrammarCompile(Default::default()),
          );
          s.spawn(move || {
            parse_grammar(
              &mut j,
              pending_grammar_paths,
              claimed_grammar_paths,
              work_verifier,
            )
          })
        })
        .map(|s| s.join().unwrap())
        .collect::<Vec<_>>()
    });

    let mut grammars = vec![];

    for mut g in results {
      grammars.append(&mut g);
    }

    grammars
  };

  #[cfg(any(feature = "wasm-target", feature = "single-thread"))]
  let grammars = parse_grammar(
    j,
    &pending_grammar_paths,
    &claimed_grammar_paths,
    &work_verifier,
  );

  j.flush_reports();

  grammars
}

fn parse_grammar(
  j: &mut Journal,
  pending_grammar_paths: &Mutex<VecDeque<PathBuf>>,
  claimed_grammar_paths: &Mutex<HashSet<PathBuf>>,
  work_verifier: &Mutex<WorkVerifier>,
) -> Vec<(PathBuf, HashMap<String, std::sync::Arc<GrammarRef>>, Box<Grammar>)> {
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
      Some(path) => match grammar_from_path(j, &path) {
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
              _ => j.report_mut().add_error(SherpaError::SourceError {
                loc:        tok.clone(),
                path:       path.clone(),
                id:         "invalid-import-source",
                msg:        format!(
                  "Could not resolve filepath {}",
                  base_path.to_str().unwrap()
                ),
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
  grammars
}

/// Loads and parses a grammar file, returning the parsed grammar node and a
/// vector of Import nodes.
fn grammar_from_path(
  j: &mut Journal,
  absolute_path: &PathBuf,
) -> SherpaResult<(Box<Grammar>, Vec<Box<sherpa::Import>>)> {
  j.set_active_report(
    "Grammar Parse",
    crate::ReportType::GrammarCompile(absolute_path.into()),
  );
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
        j.report_mut()
          .add_error(SherpaError::from_parse_error(err, absolute_path.clone()));
        SherpaResult::None
      }
    },
    Err(err) => SherpaResult::Err(err.into()),
  }
}

/// Resolves and verifies a grammar file path acquired from an `@IMPORT`
/// statement exists.
///
/// If the file path does not have an extension, attempts are made to assert
/// the existence of the file path when appended with one of the following
/// extension types appended to it: `.hc`, `.hcg` `.grammar`.
///
/// Additionally, if the given file path is relative, then it is appended to the
/// parent dir path of the current grammar, whose path is provided by the `cgd`,
/// current grammar dir, argument.
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
        .ok_or(format!(
          "Tried to load file with these extension {:?}",
          extension
        ))?,

      // Default
      _ => path.canonicalize()?,
    },
  )
}
