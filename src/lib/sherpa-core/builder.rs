use crate::{
  compile::{build_states::compile_parse_states, optimize::optimize},
  grammar::{build_compile_db, compile_grammar_from_str, load_grammar, remove_grammar_mut},
  o_to_r,
  proxy::{Array, DeduplicateIterator, Queue, Set},
  types::{ErrorContainerIter, ParserConfig},
  GrammarIdentities,
  GrammarSoup,
  Journal,
  ParseStatesVec,
  ParserDatabase,
  SherpaError,
  SherpaResult,
};

pub trait JournalReporter {
  fn get_journal(&self) -> &Journal;

  fn dump_errors(&self) -> bool {
    let mut j = self.get_journal().transfer();

    j.flush_reports();

    j.debug_error_report()
  }
}

pub trait ParserStore: JournalReporter {
  fn get_states(&self) -> &ParseStatesVec;
  fn get_db(&self) -> &ParserDatabase;

  /// Writes the parser IR states to a file in the temp directory
  #[cfg(all(debug_assertions))]
  fn write_states_to_temp_file(&self) -> SherpaResult<()> {
    let db = self.get_db();

    for (i, state) in self.get_states().iter().enumerate() {
      crate::test::utils::write_debug_file(db, "ir_states.tmp", state.1.print(db, true)? + "\n", i > 0)?;
    }

    Ok(())
  }

  /// Prints the ir code of the parser states to `stdout`
  #[inline(always)]
  fn print_states(&self) {
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

/** Builds and maintains a grammar compilation context */

pub struct SherpaGrammarBuilder {
  j:    Journal,
  soup: std::sync::Arc<GrammarSoup>,
}

impl JournalReporter for SherpaGrammarBuilder {
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

impl SherpaGrammarBuilder {
  pub fn new() -> Self {
    Self { j: Journal::new(), soup: GrammarSoup::new() }
  }

  pub fn path_to_id(&self, path: &std::path::Path) -> GrammarIdentities {
    GrammarIdentities::from_path(path, &self.soup.string_store)
  }

  /// Adds a grammar source to the soup
  pub fn add_source(&mut self, path: &std::path::Path) -> SherpaResult<&mut Self> {
    let id = self.path_to_id(path);

    let SherpaGrammarBuilder { soup, j } = self;

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
  pub fn add_source_from_string_with_imports(&mut self, source: &str, path: &std::path::Path) -> SherpaResult<()> {
    let id = self.path_to_id(path);

    let SherpaGrammarBuilder { soup, j } = self;

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

  pub fn add_source_from_string(&mut self, source: &str, path: &std::path::Path, replace: bool) -> SherpaResult<&mut Self> {
    let id = self.path_to_id(path);

    let SherpaGrammarBuilder { soup, j } = self;

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

  pub fn remove_grammar(self, root_grammar: &std::path::Path) -> SherpaResult<SherpaGrammarBuilder> {
    let id = self.path_to_id(root_grammar);

    let SherpaGrammarBuilder { soup, j } = self;

    let mut soup = o_to_r(std::sync::Arc::into_inner(soup), "could not get exclusive access to soup")?;

    remove_grammar_mut(id.guid, &mut soup)?;

    Ok(Self { j, soup: std::sync::Arc::new(soup) })
  }

  pub fn build_db(&self, root_grammar: &std::path::Path) -> SherpaResult<SherpaDatabaseBuilder> {
    let SherpaGrammarBuilder { soup, j } = self;

    let id = GrammarIdentities::from_path(root_grammar, &soup.string_store);

    let db = build_compile_db(j.transfer(), id, soup)?;

    if !db.is_valid() {
      let mut j = j.transfer();
      j.flush_reports();
      let errors = j.extract_errors();
      if errors.len() > 0 {
        return SherpaResult::Err(SherpaError::Multi(errors));
      }
    }

    Ok(SherpaDatabaseBuilder { j: j.transfer(), db })
  }
}

pub struct SherpaDatabaseBuilder {
  j:  Journal,
  db: ParserDatabase,
}

impl JournalReporter for SherpaDatabaseBuilder {
  fn get_journal(&self) -> &Journal {
    &self.j
  }
}

impl SherpaDatabaseBuilder {
  pub fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  pub fn build_parser(&self, config: ParserConfig) -> SherpaResult<SherpaParserBuilder> {
    let SherpaDatabaseBuilder { j, db } = self;

    match compile_parse_states(j.transfer(), db, config) {
      Ok(states) => {
        j.transfer().flush_reports();

        Ok(SherpaParserBuilder {
          j: j.transfer(),
          db: db.clone(),
          states: states.into_iter().collect(),
          optimized_states: None,
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

  pub fn print_terminals(&self) {
    let SherpaDatabaseBuilder { db, .. } = self;

    for tok in db.tokens() {
      println!("{: >5}  {: <10}", tok.tok_id.to_val(db), tok.name.to_string(db.string_store()))
    }
  }
}

pub struct SherpaParserBuilder {
  j: Journal,
  db: ParserDatabase,
  states: ParseStatesVec,
  optimized_states: Option<ParseStatesVec>,
}

impl JournalReporter for SherpaParserBuilder {
  fn get_journal(&self) -> &Journal {
    &self.j
  }
}

impl ParserStore for SherpaParserBuilder {
  fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  fn get_states(&self) -> &ParseStatesVec {
    self.get_states()
  }
}

impl SherpaParserBuilder {
  pub fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  pub fn optimize(self, for_debugging: bool) -> SherpaResult<Self> {
    let SherpaParserBuilder { db, j, states, optimized_states } = self;

    Ok(Self {
      optimized_states: match optimized_states {
        None => Some(optimize(&db, states.iter().cloned().collect(), for_debugging)?),
        states => states,
      },
      j,
      db,
      states,
    })
  }

  /// Returns either optimized or unoptimized states depending on if an
  /// optimization pass has been performed.
  pub fn get_states(&self) -> &ParseStatesVec {
    match self.get_optimized_states() {
      Some(states) => states,
      _ => self.get_unoptimized_states(),
    }
  }

  pub fn get_unoptimized_states(&self) -> &ParseStatesVec {
    &self.states
  }

  pub fn get_optimized_states(&self) -> Option<&ParseStatesVec> {
    self.optimized_states.as_ref()
  }
}

#[test]
pub fn empty_source_path() -> SherpaResult<()> {
  assert!(SherpaGrammarBuilder::new().add_source(&std::path::PathBuf::new()).is_err());
  Ok(())
}

pub struct TestPackage {
  pub journal: Journal,
  pub states:  ParseStatesVec,
  pub db:      ParserDatabase,
}

impl JournalReporter for TestPackage {
  fn get_journal(&self) -> &Journal {
    &self.journal
  }
}

impl ParserStore for TestPackage {
  fn get_db(&self) -> &ParserDatabase {
    &self.db
  }

  fn get_states(&self) -> &ParseStatesVec {
    &self.states
  }
}

impl From<SherpaParserBuilder> for TestPackage {
  fn from(value: SherpaParserBuilder) -> Self {
    TestPackage {
      journal: value.j.transfer(),
      states:  match value.optimized_states {
        Some(states) => states.into_iter().collect(),
        _ => value.states.into_iter().collect(),
      },
      db:      value.db,
    }
  }
}

pub struct DBPackage {
  pub journal: Journal,
  pub db:      ParserDatabase,
}

impl From<SherpaDatabaseBuilder> for DBPackage {
  fn from(value: SherpaDatabaseBuilder) -> Self {
    DBPackage { journal: value.j.transfer(), db: value.db }
  }
}

#[test]
pub fn source_path_to_default_file() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammarBuilder::new().add_source(&grammar_source_path).is_ok());
  Ok(())
}

#[test]
pub fn build_db() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammarBuilder::new().add_source(&grammar_source_path)?.build_db(&grammar_source_path).is_ok());
  Ok(())
}

#[test]
pub fn build_states() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammarBuilder::new()
    .add_source(&grammar_source_path)?
    .build_db(&grammar_source_path)?
    .build_parser(Default::default())
    .is_ok());
  Ok(())
}

#[test]
pub fn build_with_optimized_states() -> SherpaResult<()> {
  let grammar_source_path =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/json/json.sg").canonicalize().unwrap();
  assert!(SherpaGrammarBuilder::new()
    .add_source(&grammar_source_path)?
    .build_db(&grammar_source_path)?
    .build_parser(Default::default())?
    .optimize(false)?
    .get_optimized_states()
    .is_some());
  Ok(())
}
