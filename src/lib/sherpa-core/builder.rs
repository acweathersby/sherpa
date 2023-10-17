use std::{
  fs::OpenOptions,
  io::Write,
  path::{Path, PathBuf},
  sync::Arc,
};

use crate::{
  compile::{
    ir::{build_ir_from_graph, optimize, sweep},
    states::build_states::{compile_parser_states, NonTermGraph, ScannerGraph},
  },
  grammar::{build_compile_db, compile_grammar_from_str, load_grammar, remove_grammar_mut},
  o_to_r,
  proxy::{Array, DeduplicateIterator, Queue, Set},
  types::*,
  GrammarIdentities,
  GrammarSoup,
  IString,
  Journal,
  ParseState,
  ParseStatesVec,
  ParserClassification,
  ParserDatabase,
  ParserMetrics,
  SherpaError,
  SherpaResult,
};

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

/** Builds and maintains a grammar compilation context */

pub struct SherpaGrammar {
  j:    Journal,
  soup: std::sync::Arc<GrammarSoup>,
}

impl JournalReporter for SherpaGrammar {
  fn get_journal(&self) -> &Journal {
    &self.j
  }
}

fn blend_soups(soup: std::sync::Arc<GrammarSoup>, other: GrammarSoup) -> SherpaResult<()> {
  let GrammarSoup { grammar_headers, nonterminals, custom_states, .. } = soup.as_ref();

  let mut grammar_headers = grammar_headers.write()?;
  let mut nonterminals = nonterminals.write()?;
  let mut custom_states = custom_states.write()?;

  let other_grammar_headers = std::sync::Arc::into_inner(other.grammar_headers).unwrap().into_inner()?;
  let other_nonterminals = std::sync::Arc::into_inner(other.nonterminals).unwrap().into_inner()?;
  let other_custom_states = std::sync::Arc::into_inner(other.custom_states).unwrap().into_inner()?;

  for (id, header) in other_grammar_headers.into_iter() {
    if grammar_headers.insert(id, header).is_some() {
      let nonterminals_temp = nonterminals.drain(..).collect::<Vec<_>>();
      nonterminals.extend(nonterminals_temp.into_iter().filter(|p| p.g_id != id));

      let custom_states_temp = custom_states.drain().collect::<Vec<_>>();
      custom_states.extend(custom_states_temp.into_iter().filter(|(_, s)| s.g_id != id))
    }
  }

  nonterminals.extend(other_nonterminals.into_iter());
  custom_states.extend(other_custom_states.into_iter());

  Ok(())
}

impl SherpaGrammar {
  pub fn new() -> Self {
    Self { j: Journal::new(), soup: GrammarSoup::new() }
  }

  pub fn path_to_id<T: Into<PathBuf>>(&self, path: T) -> GrammarIdentities {
    GrammarIdentities::from_path(&path.into(), &self.soup.string_store)
  }

  /// Adds a grammar source to the soup
  pub fn add_source<T: Into<PathBuf>>(&mut self, path: T) -> SherpaResult<&mut Self> {
    let id = self.path_to_id(path);

    let SherpaGrammar { soup, j } = self;

    j.flush_reports();

    let mut j = j.transfer();

    j.set_active_report("add_source", crate::ReportType::Any);

    let mut queue = Queue::from_iter([id]);

    let mut known_imports = Set::from_iter(soup.grammar_headers.read().map_err(|e| SherpaError::from(e))?.iter().map(|i| *i.0));

    while let Some(id) = queue.pop_front() {
      if known_imports.insert(id.guid) {
        match load_grammar(&mut j.transfer(), id, soup.string_store.clone()) {
          Ok((new_soup, new_imports)) => {
            blend_soups(
              soup.clone(),
              std::sync::Arc::into_inner(new_soup).expect("There should be only one reference for this"),
            )?;
            queue.append(&mut new_imports.into_iter().collect());
          }
          Err(err) => j.report_mut().add_error(err),
        }
      }
    }

    j.report_mut().wrap_ok_or_return_errors(self)
  }

  /// Adds a grammar to the soup from a source string
  pub fn add_source_from_string_with_imports<T: Into<PathBuf>>(&mut self, source: &str, grammar_path: T) -> SherpaResult<()> {
    let path: PathBuf = grammar_path.into();
    let id = self.path_to_id(&path);

    let SherpaGrammar { soup, j } = self;

    j.flush_reports();

    let mut j = j.transfer();

    j.set_active_report("add_source", crate::ReportType::Any);

    let mut known_imports = Set::from_iter(soup.grammar_headers.read().map_err(|e| SherpaError::from(e))?.iter().map(|i| *i.0));

    if !known_imports.contains(&id.guid) {
      match compile_grammar_from_str(&mut j.transfer(), source, path.to_owned(), soup.string_store.clone()) {
        Ok((soup, ids)) => {
          known_imports.insert(id.guid);

          let mut queue = Queue::from_iter(ids);

          while let Some(id) = queue.pop_front() {
            if known_imports.insert(id.guid) {
              match load_grammar(&mut j.transfer(), id, soup.string_store.clone()) {
                Ok((new_soup, new_imports)) => {
                  blend_soups(
                    soup.clone(),
                    std::sync::Arc::into_inner(new_soup).expect("There should be only one reference for this"),
                  )?;
                  queue.append(&mut new_imports.into_iter().collect());
                }
                Err(err) => j.report_mut().add_error(err),
              }
            }
          }
        }
        Err(err) => j.report_mut().add_error(err),
      }
    }

    j.report_mut().wrap_ok_or_return_errors(())
  }

  pub fn add_source_from_string<T: Into<PathBuf>>(
    &mut self,
    source: &str,
    grammar_path: T,
    replace: bool,
  ) -> SherpaResult<&mut Self> {
    let path: PathBuf = grammar_path.into();
    let id = self.path_to_id(&path);

    let SherpaGrammar { soup, j } = self;

    j.flush_reports();

    let mut j = j.transfer();

    j.set_active_report("add_source", crate::ReportType::Any);

    let known_imports = Set::from_iter(soup.grammar_headers.read().map_err(|e| SherpaError::from(e))?.iter().map(|i| *i.0));

    if replace || !known_imports.contains(&id.guid) {
      match compile_grammar_from_str(&mut j.transfer(), source, path.to_owned(), soup.string_store.clone()) {
        Ok((new_soup, _)) => {
          blend_soups(soup.clone(), std::sync::Arc::into_inner(new_soup).expect("There should be only one reference for this"))?;
        }
        Err(err) => j.report_mut().add_error(err),
      };
    }

    j.report_mut().wrap_ok_or_return_errors(self)
  }

  pub fn remove_grammar<T: Into<PathBuf>>(self, root_grammar: T) -> SherpaResult<SherpaGrammar> {
    let path: PathBuf = root_grammar.into();
    let id = self.path_to_id(&path);

    let SherpaGrammar { soup, j } = self;

    let mut soup = o_to_r(std::sync::Arc::into_inner(soup), "could not get exclusive access to soup")?;

    remove_grammar_mut(id.guid, &mut soup)?;

    Ok(Self { j, soup: std::sync::Arc::new(soup) })
  }

  pub fn build_db<T: Into<PathBuf>>(&self, root_grammar: T, config: ParserConfig) -> SherpaResult<SherpaDatabase> {
    let SherpaGrammar { soup, j } = self;
    let path: PathBuf = root_grammar.into();

    let id = GrammarIdentities::from_path(&path, &soup.string_store);

    let db = build_compile_db(j.transfer(), id, soup, &config)?;

    if !db.is_valid() {
      let mut j = j.transfer();
      j.flush_reports();
      let errors = j.extract_errors();
      if errors.len() > 0 {
        return SherpaResult::Err(SherpaError::Multi(errors));
      }
    }

    Ok(SherpaDatabase { j: j.transfer(), db: std::sync::Arc::new(db) })
  }
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct SherpaDatabase {
  j:  Journal,
  db: SharedParserDatabase,
}

impl JournalReporter for SherpaDatabase {
  fn get_journal(&self) -> &Journal {
    &self.j
  }
}

impl SherpaDatabase {
  /// Returns a reference to the underlying [ParserDatabase].
  pub fn get_internal(&self) -> &ParserDatabase {
    &self.db
  }

  /// Constructs parser and scanner graphs for this variant of the grammar.
  pub fn build_states(&self, config: ParserConfig) -> SherpaResult<SherpaGraph> {
    let SherpaDatabase { j, db } = self;

    match compile_parser_states(j.transfer(), db.clone(), config) {
      Ok((parsers, scanners)) => {
        j.transfer().flush_reports();

        Ok(SherpaGraph { parsers, scanners, j: j.transfer(), db: db.clone(), config })
      }
      Err(err) => {
        let mut errors = err.flatten();
        if errors.len() > 1 {
          Err(errors.into_iter().dedup::<Array<_>>().into_iter().into_multi())
        } else {
          Err(errors.pop().unwrap())
        }
      }
    }
  }

  pub fn print_terminals(&self) {
    let SherpaDatabase { db, .. } = self;

    for tok in db.tokens() {
      println!("{: >5}  {: <10}", tok.tok_id.to_val(), tok.name.to_string(db.string_store()))
    }
  }
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

/// Comprised of the parser and scanner graphs for the given grammar
pub struct SherpaGraph {
  pub(crate) j:        Journal,
  pub(crate) db:       SharedParserDatabase,
  pub(crate) config:   ParserConfig,
  pub(crate) parsers:  Arc<Vec<Box<NonTermGraph>>>,
  pub(crate) scanners: Arc<Vec<Box<ScannerGraph>>>,
}

impl JournalReporter for SherpaGraph {
  fn get_journal(&self) -> &Journal {
    &self.j
  }
}

impl SherpaGraph {
  pub fn build_ir_parser(&self, opt: bool, optimize_for_debugging: bool) -> SherpaResult<SherpaIRParser> {
    match build_ir_from_graph(self) {
      Ok((classification, ir_states)) => {
        let Self { config, db, .. } = self;
        let mut j = self.j.transfer();
        j.flush_reports();

        let (states, report): (Vec<_>, _) = if opt {
          optimize(db, config, ir_states, optimize_for_debugging)?
        } else {
          sweep(db, config, ir_states, optimize_for_debugging)?
        };

        Ok(SherpaIRParser {
          classification,
          config: *config,
          db: db.clone(),
          states,
          is_optimized: opt,
          report,
          j,
        })
      }
      Err(err) => {
        let mut errors = err.flatten();
        if errors.len() > 1 {
          Err(errors.into_iter().dedup::<Array<_>>().into_iter().into_multi())
        } else {
          Err(errors.pop().unwrap())
        }
      }
    }
  }

  pub fn write_debug_file(&self, output_path: &Path) -> SherpaResult<()> {
    let file_path = output_path.join("parser_states.debug.txt");

    let mut file = OpenOptions::new().truncate(true).write(true).create(true).open(file_path)?;

    for parser_state in self.parsers.iter() {
      if let Some(graph) = &parser_state.graph {
        file.write_all(graph.internal()._debug_string_().as_str().as_bytes())?;
      }
    }

    file.flush()?;

    Ok(())
  }

  pub fn build_parse_table(&self) {
    todo!("Generate parser table");
  }

  pub fn get_parser_graph(&self) -> Option<&NonTermGraph> {
    todo!("Generate parser table");
  }
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

pub struct SherpaIRParser {
  j: Journal,
  db: SharedParserDatabase,
  config: ParserConfig,
  states: ParseStatesVec,
  classification: ParserClassification,
  is_optimized: bool,
  pub report: OptimizationReport,
}

impl JournalReporter for SherpaIRParser {
  fn get_journal(&self) -> &Journal {
    &self.j
  }
}

impl ParserStore for SherpaIRParser {
  fn report(&self) -> OptimizationReport {
    self.report
  }

  fn get_config(&self) -> &ParserConfig {
    &self.config
  }

  fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  fn get_states(&self) -> impl Iterator<Item = (IString, &ParseState)> {
    self.get_states()
  }

  fn get_classification(&self) -> ParserClassification {
    self.classification
  }

  fn get_is_optimized(&self) -> bool {
    self.is_optimized
  }
}

impl SherpaIRParser {
  pub fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  /// Returns either optimized or unoptimized states depending on if an
  /// optimization pass has been performed.
  pub fn get_states(&self) -> impl Iterator<Item = (IString, &ParseState)> {
    self.states.iter().map(|(a, b)| (*a, b.as_ref()))
  }
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

#[test]
pub fn empty_source_path() -> SherpaResult<()> {
  assert!(SherpaGrammar::new().add_source(&std::path::PathBuf::new()).is_err());
  Ok(())
}

pub struct TestPackage {
  pub journal: Journal,
  pub states: ParseStatesVec,
  pub db: SharedParserDatabase,
  pub config: ParserConfig,
  pub report: OptimizationReport,
  pub classification: ParserClassification,
}

impl JournalReporter for TestPackage {
  fn get_journal(&self) -> &Journal {
    &self.journal
  }
}

impl ParserStore for TestPackage {
  fn report(&self) -> OptimizationReport {
    self.report
  }

  fn get_config(&self) -> &ParserConfig {
    &self.config
  }

  fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  fn get_states(&self) -> impl Iterator<Item = (IString, &ParseState)> {
    self.states.iter().map(|s| (s.0, s.1.as_ref()))
  }

  fn get_is_optimized(&self) -> bool {
    false
  }

  fn get_classification(&self) -> ParserClassification {
    self.classification
  }
}

impl From<SherpaIRParser> for TestPackage {
  fn from(value: SherpaIRParser) -> Self {
    TestPackage {
      config: value.config,
      journal: value.j.transfer(),
      states: value.states.into_iter().collect(),
      db: value.db,
      report: value.report,
      classification: value.classification,
    }
  }
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

pub struct DBPackage {
  pub journal: Journal,
  pub db:      SharedParserDatabase,
}

impl From<SherpaDatabase> for DBPackage {
  fn from(value: SherpaDatabase) -> Self {
    DBPackage { journal: value.j.transfer(), db: value.db.clone() }
  }
}

#[test]
pub fn source_path_to_default_file() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammar::new().add_source(&grammar_source_path).is_ok());
  Ok(())
}

#[test]
pub fn build_db() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammar::new().add_source(&grammar_source_path)?.build_db(grammar_source_path, Default::default()).is_ok());
  Ok(())
}

#[test]
pub fn build_states() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammar::new()
    .add_source(&grammar_source_path)?
    .build_db(grammar_source_path, Default::default())?
    .build_states(Default::default())
    .is_ok());
  Ok(())
}

#[test]
pub fn build_with_optimized_states() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  SherpaGrammar::new()
    .add_source(&grammar_source_path)?
    .build_db(grammar_source_path, Default::default())?
    .build_states(Default::default())?
    .build_ir_parser(true, false)?;
  Ok(())
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

pub trait JournalReporter {
  fn get_journal(&self) -> &Journal;

  fn dump_errors(&self) -> bool {
    let mut j = self.get_journal().transfer();

    j.flush_reports();

    j.debug_error_report()
  }
}

pub trait ParserStore: JournalReporter {
  fn get_states(&self) -> impl Iterator<Item = (IString, &ParseState)>;
  fn get_db(&self) -> &ParserDatabase;
  fn get_classification(&self) -> ParserClassification;
  fn get_is_optimized(&self) -> bool;
  fn get_config(&self) -> &ParserConfig;
  fn report(&self) -> OptimizationReport;

  fn get_meterics(&self) -> ParserMetrics {
    ParserMetrics {
      classification: self.get_classification(),
      num_of_states:  self.get_states().count(),
      optimized:      self.get_is_optimized(),
    }
  }

  /// Prints the ir code of the parser states to `stdout`
  #[inline(always)]
  fn _print_states_(&self) {
    #[cfg(debug_assertions)]
    {
      let states = self.get_states();
      for state in states {
        if let Ok(string) = state.1.print(self.get_db(), true) {
          println!("{}", string);
        }
      }
    }
  }
}
