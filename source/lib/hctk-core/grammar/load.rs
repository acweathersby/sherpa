//! Functions for Grammar file path resolution and loading,
//! and initial parse functions

use crate::types::*;
use std::collections::HashMap;
use std::fs::read;
use std::num::NonZeroUsize;
use std::path::Path;
use std::path::PathBuf;
use std::path::{self};
use std::sync::Arc;

use super::data::ast::ASTNode;
use super::data::ast::Grammar;
use super::data::ast::Import;
use super::multitask::WorkVerifier;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::sync::Mutex;
use std::thread::{self};

// TODO: Replace with the new grammar parser
use super::parse::compile_grammar_ast;

const allowed_extensions: [&str; 3] = ["hc", "hcg", "grammar"];

pub(crate) fn get_usable_thread_count(requested_count: usize) -> usize {
  NonZeroUsize::min(
    NonZeroUsize::new(usize::min(1, requested_count)).unwrap(),
    std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()),
  )
  .get()
}

/// Loads all grammars that are indirectly or directly referenced from a single filepath.
/// Returns a vector grammars in no particular order except the first grammar belongs to
/// the file path
pub(crate) fn load_all(
  absolute_path: &PathBuf,
  number_of_threads: usize,
) -> (Vec<(PathBuf, Box<Grammar>)>, Vec<HCError>) {
  let mut pending_grammar_paths =
    Mutex::new(VecDeque::<PathBuf>::from_iter(vec![absolute_path.clone()]));
  let mut claimed_grammar_paths = Mutex::new(HashSet::<PathBuf>::new());
  let mut work_verifier = Mutex::new(WorkVerifier::new(1));

  let results = thread::scope(|s| {
    [0..get_usable_thread_count(number_of_threads)]
      .into_iter()
      .map(|i| {
        s.spawn(|| {
          let mut grammars = vec![];
          let mut errors = vec![];

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
              Some(path) => match load_grammar(&path) {
                HCResult::Ok((grammar, imports)) => {
                  let mut units_of_work = imports.len();

                  for box Import { uri, reference, tok } in imports {
                    let base_path = PathBuf::from(uri);
                    match resolve_grammar_path(
                      &base_path,
                      &path.parent().unwrap_or(Path::new("")).to_path_buf(),
                      &allowed_extensions,
                    ) {
                      HCResult::Ok(path) => {
                        pending_grammar_paths.lock().unwrap().push_back(path);
                        work_verifier.lock().unwrap().add_units_of_work(1);
                      }
                      HCResult::Err(err) => errors.push(HCError::Load_InvalidDependency {
                        path: base_path,
                        requestor: path.to_owned(),
                        tok,
                        err: Some(Box::new(err)),
                      }),
                      HCResult::None => errors.push(HCError::Load_InvalidDependency {
                        path: base_path,
                        requestor: path.to_owned(),
                        tok,
                        err: None,
                      }),
                    }
                  }

                  grammars.push((path, grammar));
                  {
                    work_verifier.lock().unwrap().complete_one_unit_of_work();
                  }
                }
                HCResult::Err(err) => errors.push(err),
                _ => {}
              },
              None => {
                if work_verifier.lock().unwrap().is_complete() {
                  break;
                }
              }
            }
          }
          (grammars, errors)
        })
      })
      .map(|s| s.join().unwrap())
      .collect::<Vec<_>>()
  });

  let mut grammars = vec![];
  let mut errors = vec![];

  for (mut g, mut e) in results {
    grammars.append(&mut g);
    errors.append(&mut e);
  }

  (grammars, errors)
}

#[test]
fn test_load_all() {
  let (grammars, errors) = load_all(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  for err in &errors {
    println!("{}", err);
  }
  assert_eq!(grammars.len(), 2);
  assert_eq!(errors.len(), 0);

  let (grammars, errors) = load_all(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/invalid_load.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  for err in &errors {
    println!("{}", err);
  }
  assert_eq!(grammars.len(), 1);
  assert_eq!(errors.len(), 1);
}

/// Loads and parses a grammar file, returning the parsed grammar node and a vector of Import nodes.
pub(crate) fn load_grammar(absolute_path: &PathBuf) -> HCResult<(Box<Grammar>, Vec<Box<Import>>)> {
  match read(absolute_path) {
    Ok(buffer) => match compile_grammar_ast(buffer) {
      Ok(grammar) => {
        let import_paths = grammar
          .preamble
          .iter()
          .filter_map(|a| match a {
            ASTNode::Import(import) => Some(import.clone()),
            _ => None,
          })
          .collect();
        HCResult::Ok((grammar, import_paths))
      }
      Err(err) => HCResult::Err(err),
    },
    Err(err) => HCResult::Err(err.into()),
  }
}

#[test]
fn test_load_grammar() {
  let result =
    load_grammar(&PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test_grammars/load.hcg"));

  assert!(result.is_faulty());

  let result = load_grammar(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.hcg")
      .canonicalize()
      .unwrap(),
  );

  assert!(result.is_ok());
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
) -> HCResult<PathBuf> {
  HCResult::Ok(
    match ((
      path.is_file(),
      path.extension().is_some(),
      // Ensure path is is an absolute path
      match path.is_absolute() {
        true => (path.to_owned(), false),
        false => (cgd.join(&path), cgd.join(&path).is_file()),
      },
    )) {
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

#[test]
fn test_resolve_cargo_file() {
  let result = resolve_grammar_path(&Default::default(), &Default::default(), &[]);

  assert!(result.is_faulty());

  let result = resolve_grammar_path(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml"),
    &Default::default(),
    &[],
  );

  assert!(result.is_ok());

  let result = resolve_grammar_path(
    &PathBuf::from("./Cargo.toml"),
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")),
    &[],
  );

  assert!(result.is_ok());

  let result =
    resolve_grammar_path(&PathBuf::from("./Cargo"), &PathBuf::from(env!("CARGO_MANIFEST_DIR")), &[
      "toml",
    ]);

  assert!(result.is_ok());
}
