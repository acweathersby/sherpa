use crate::debug::debug_items;
use crate::debug::grammar;
use crate::grammar::data::ast::AnyGroup;
use crate::grammar::data::ast::Ascript;
use crate::grammar::data::ast::Literal;
use crate::grammar::parse::compile_ascript_ast;
use crate::grammar::uuid::hash_id_value_u64;
use crate::types;
use crate::types::*;
use regex::Regex;

use super::create_closure;
use super::create_defined_scanner_name;
use super::create_production_guid_name;
use super::create_scanner_name;
use super::data::ast;
use super::data::ast::ASTNode;
use super::data::ast::ASTNodeTraits;
use super::get_guid_grammar_name;
use super::get_production_plain_name;
use super::get_production_recursion_type;
use super::get_production_start_items;
use super::parse;
use super::parse::compile_grammar_ast;

use core::panic;
use std::collections::btree_map;
use std::collections::btree_map::VacantEntry;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::ffi::OsStr;
use std::fs::read;
use std::num::NonZeroUsize;
use std::path::PathBuf;
use std::sync::Mutex;
use std::thread::{self};
use std::vec;

struct SymbolData<'a> {
  pub annotation:       String,
  pub is_list:          bool,
  pub is_group:         bool,
  pub is_optional:      bool,
  pub is_shift_nothing: bool,
  pub is_meta:          bool,
  pub is_exclusive:     bool,
  pub sym_atom:         Option<&'a ASTNode>,
}

/// Compile a complete grammar given a file path to a root *.hcg file.
pub fn compile_from_path(
  root_grammar_absolute_path: &PathBuf,
  number_of_threads: usize,
) -> (Option<GrammarStore>, Vec<ParseError>) {
  let mut pending_grammar_paths = Mutex::new(VecDeque::<PathBuf>::new());

  let mut claimed_grammar_paths = Mutex::new(HashSet::<PathBuf>::new());

  // Pending Work, Claimed Work, Completed Work
  let mut work_verifier = Mutex::new(WorkVerifier::new(1));

  pending_grammar_paths.lock().unwrap().push_back(root_grammar_absolute_path.to_owned());

  let number_of_threads =
    std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()).get();

  let mut results = thread::scope(|s| {
    [0..number_of_threads]
      .into_iter()
      .map(|i| {
        s.spawn(|| {
          let mut raw_grammars = vec![];
          let mut errors = vec![];

          loop {
            match {
              let val = pending_grammar_paths.lock().unwrap().pop_front();

              val
            } {
              Some(path) => {
                let have_work = {
                  let result = claimed_grammar_paths.lock().unwrap().insert(path.to_owned());

                  {
                    let mut work_verifier = work_verifier.lock().unwrap();

                    if result {
                      work_verifier.start_one_unit_of_work()
                    } else {
                      work_verifier.skip_one_unit_of_work()
                    }
                  }

                  result
                };

                if have_work {
                  let (grammar, mut new_errors) = compile_file_path(&path);

                  errors.append(&mut new_errors);

                  let mut work_verifier = work_verifier.lock().unwrap();

                  work_verifier.complete_one_unit_of_work();

                  if let Some(grammar) = grammar {
                    work_verifier.add_units_of_work(grammar.imports.len() as u32);

                    for (_, b) in grammar.imports.values() {
                      pending_grammar_paths.lock().unwrap().push_back(b.to_owned());
                    }
                    raw_grammars.push(grammar);
                  }
                };
              }
              None => {
                let res = {
                  let work_verifier = work_verifier.lock().unwrap();

                  work_verifier.is_complete()
                };

                if res {
                  break;
                }
              }
            }
          }

          (raw_grammars, errors)
        })
      })
      .map(|s| s.join().unwrap())
      .collect::<Vec<_>>()
      .into_iter()
      .collect::<VecDeque<_>>()
  });

  let mut errors = vec![];
  let mut grammars = VecDeque::new();

  for (mut grammar_results, mut error_results) in results.into_iter() {
    grammars.append(&mut grammar_results.into_iter().collect::<VecDeque<_>>());
    errors.append(&mut error_results);
  }

  if grammars.is_empty() {
    (None, errors)
  } else {
    let mut grammar = grammars.pop_front().unwrap();

    merge_grammars(&mut grammar, &grammars.into_iter().collect::<Vec<_>>(), &mut errors);

    if errors.is_empty() {
      (Some(finalize_grammar(grammar, &mut errors, number_of_threads)), errors)
    } else {
      (None, errors)
    }
  }
}

/// Compile a grammar defined in a string
pub fn compile_from_string(
  string: &str,
  absolute_path: &PathBuf,
) -> (Option<GrammarStore>, Vec<ParseError>) {
  match compile_grammar_ast(Vec::from(string.as_bytes())) {
    Ok(grammar) => {
      let (grammar, mut errors) = pre_process_grammar(
        &grammar,
        absolute_path,
        absolute_path.file_stem().unwrap_or_else(|| OsStr::new("undefined")).to_str().unwrap(),
      );

      let grammar = finalize_grammar(grammar, &mut errors, 1);

      (Some(grammar), errors)
    }
    Err(err) => (None, vec![err]),
  }
}

fn compile_file_path(absolute_path: &PathBuf) -> (Option<GrammarStore>, Vec<ParseError>) {
  match read(absolute_path) {
    Ok(buffer) => match compile_grammar_ast(buffer) {
      Ok(grammar) => {
        let (grammar, errors) = pre_process_grammar(
          &grammar,
          absolute_path,
          absolute_path.file_stem().unwrap_or_else(|| OsStr::new("undefined")).to_str().unwrap(),
        );

        (Some(grammar), errors)
      }
      Err(err) => (None, vec![err]),
    },
    Err(err) => (None, vec![ParseError::IO_ERROR(err)]),
  }
}

/// Create scanner productions, adds ids to tokens, creates cache
/// data.

fn finalize_grammar(
  mut g: GrammarStore,
  mut errors: &mut [ParseError],
  thread_count: usize,
) -> GrammarStore {
  create_scanner_productions_from_symbols(&mut g, errors);

  finalize_symbols(&mut g, errors);

  // Check for missing production symbols in body symbols
  for (id, b) in &g.bodies {
    for sym in &b.syms {
      match sym.sym_id {
        SymbolID::TokenProduction(prod, _) | SymbolID::Production(prod, _) => {
          if !g.productions.contains_key(&prod) {
            panic!(
              "Unable to find production definition \n{}",
              sym.tok.blame(1, 1, "production does not exist", None)
            );
          }
        }
        _ => {}
      }
    }
  }

  finalize_productions(&mut g, thread_count, errors);

  finalize_items(&mut g, thread_count, errors);

  finalize_bytecode_metadata(&mut g, errors);

  g
}

/// Adds bytecode identifiers to relevant objects.
fn finalize_bytecode_metadata(g: &mut GrammarStore, errors: &mut [ParseError]) {
  let GrammarStore { productions: production_table, bodies: bodies_table, .. } = g;

  for (index, body) in bodies_table
    .values_mut()
    .filter(|(b)| !production_table.get(&b.prod).unwrap().is_scanner)
    .enumerate()
  {
    body.bc_id = index as u32;
  }

  for (index, production) in production_table.values_mut().enumerate() {
    production.bytecode_id = index as u32;
  }
}

/// Sets an appropriate `is_recursive` value on all productions.
fn finalize_productions(g: &mut GrammarStore, thread_count: usize, errors: &mut [ParseError]) {
  let production_ids = g.productions.keys().cloned().collect::<Vec<_>>();

  let production_id_chunks =
    production_ids.chunks(thread_count).filter(|s| !s.is_empty()).collect::<Vec<_>>();

  let mut left_right_conversion_candidates = vec![];

  for (production_id, recursion_type) in thread::scope(|s| {
    production_id_chunks
      .iter()
      .map(|work| {
        s.spawn(|| {
          work
            .iter()
            .map(|production_id| (*production_id, get_production_recursion_type(*production_id, g)))
            .collect::<Vec<_>>()
        })
      })
      // Collect now to actually generate the threads
      .collect::<Vec<_>>()
      .into_iter()
      .flat_map(move |s| s.join().unwrap())
      .collect::<Vec<_>>()
  }) {
    let production = g.productions.get_mut(&production_id).unwrap();

    production.recursion_type = recursion_type;

    if production.is_scanner && production.recursion_type.contains(RecursionType::LEFT_DIRECT) {
      left_right_conversion_candidates.push(production.id);
    }
  }

  // Convert left recursive TOKEN productions into right recursion.
  for candidate_id in left_right_conversion_candidates {
    convert_left_to_right(g, candidate_id);
  }
}

/// Creates item closure caches and creates start and goto item groups.
fn finalize_items(g: &mut GrammarStore, thread_count: usize, errors: &mut [ParseError]) {
  // Generate the item closure cache
  let start_items =
    g.productions.keys().flat_map(|p| get_production_start_items(p, g)).collect::<Vec<_>>();

  let start_items_chunks = start_items.chunks(thread_count).collect::<Vec<_>>();

  for (item, closure, peek_symbols) in thread::scope(|s| {
    start_items_chunks
      .iter()
      .map(|work| {
        s.spawn(|| {
          let mut results = vec![];

          let mut pending_items = VecDeque::<Item>::from_iter(work.iter().cloned());

          while let Some(item) = pending_items.pop_front() {
            let item = item.to_zero_state();

            if !item.at_end() {
              let peek_symbols =
                if let Some(peek_symbols) = g.production_ignore_symbols.get(&item.get_prod_id(g)) {
                  peek_symbols.clone()
                } else {
                  vec![]
                };

              results.push((item, create_closure(&[item], g), peek_symbols));

              pending_items.push_back(item.increment().unwrap());
            }
          }

          results
        })
      })
      // Collect now to actually generate the threads
      .collect::<Vec<_>>()
      .into_iter()
      .flat_map(move |s| s.join().unwrap())
      .collect::<Vec<_>>()
  }) {
    g.item_ignore_symbols.insert(item.to_zero_state(), peek_symbols);
    g.closures.insert(item.to_zero_state(), closure);
  }

  for closure in g.closures.values().cloned() {
    for item in closure {
      if item.is_nonterm(g) {
        let production_id = &item.get_production_id_at_sym(g);
        match g.lr_items.entry(*production_id) {
          btree_map::Entry::Vacant(entry) => {
            entry.insert(vec![item]);
          }
          btree_map::Entry::Occupied(mut entry) => {
            let mut vec = entry.get_mut();
            if !vec.contains(&item) {
              vec.push(item);
            }
          }
        }
      }
    }
  }
}

struct WorkVerifier {
  complete: u32,
  pending:  u32,
  progress: u32,
}

impl WorkVerifier {
  pub fn new(pending: u32) -> Self {
    WorkVerifier { complete: 0, pending, progress: 0 }
  }

  pub fn add_units_of_work(&mut self, units_of_work: u32) {
    self.pending += units_of_work;
  }

  pub fn start_one_unit_of_work(&mut self) {
    if self.pending > 0 {
      self.pending -= 1;

      self.progress += 1;
    } else {
      panic!("No pending work")
    }
  }

  pub fn complete_one_unit_of_work(&mut self) {
    if self.progress > 0 {
      self.progress -= 1;

      self.complete += 1;
    } else {
      panic!("No work in progress")
    }
  }

  pub fn skip_one_unit_of_work(&mut self) {
    if self.pending > 0 {
      self.pending -= 1;

      self.complete += 1;
    } else {
      panic!("No pending work")
    }
  }

  pub fn is_complete(&self) -> bool {
    self.pending == 0 || self.progress == 0 || self.complete > 0
  }
}

fn finalize_symbols(g: &mut GrammarStore, errors: &mut [ParseError]) {
  let mut symbol_bytecode_id = SymbolID::DefinedSymbolIndexBasis;

  for sym_id in &SymbolID::Generics {
    let (_, production_id, ..) = get_scanner_info_from_defined(sym_id, g);

    let scanner_id = sym_id.bytecode_id(Some(g));

    g.productions.get_mut(&production_id).unwrap().symbol_bytecode_id = scanner_id;
  }

  for sym_id in g.symbols.keys().cloned().collect::<Vec<_>>() {
    if !g.symbols.get(&sym_id).unwrap().scanner_only {
      if matches!(sym_id, SymbolID::TokenProduction(..)) || sym_id.is_defined() {
        let symbol = g.symbols.get_mut(&sym_id).unwrap();

        symbol.bytecode_id = symbol_bytecode_id;

        let (_, production_id, ..) = get_scanner_info_from_defined(&sym_id, g);

        let scanner_production = g.productions.get_mut(&production_id).unwrap();

        scanner_production.symbol_bytecode_id = symbol_bytecode_id;

        symbol_bytecode_id += 1;
      }
    }

    g.symbols.get_mut(&sym_id).unwrap().friendly_name = sym_id.to_string(g);
  }
}

fn create_scanner_productions_from_symbols(g: &mut GrammarStore, errors: &mut [ParseError]) {
  // Start iterating over known token production references, and add
  // new productions as we encounter them.
  let mut scanner_production_queue =
    VecDeque::from_iter(g.symbols.keys().chain(SymbolID::Generics.iter()).cloned());

  while let Some(sym_id) = scanner_production_queue.pop_front() {
    match &sym_id {
      sym if matches!(sym, SymbolID::GenericSpace) => {
        // Converts the generic symbol `g:sp` into a production that targets the
        // the defined symbol `b'10'`
        let (_, scanner_production_id, scanner_name, symbol_string) =
          get_scanner_info_from_defined(&sym_id, g);

        convert_scanner_symbol_to_production(
          g,
          &[&" ".to_string()],
          scanner_production_id,
          scanner_name,
          Token::empty(),
        );
      }
      sym if matches!(sym, SymbolID::GenericNewLine) => {
        // Converts the generic symbol `g:sp` into a production that targets the
        // the defined symbol `b'10'`
        let (_, scanner_production_id, scanner_name, symbol_string) =
          get_scanner_info_from_defined(&sym_id, g);

        convert_scanner_symbol_to_production(
          g,
          &[&"\n".to_string()],
          scanner_production_id,
          scanner_name,
          Token::empty(),
        );
      }
      sym if SymbolID::Generics.contains(sym) => {
        // Converts a generic symbol into a scanner production if such a production
        // does not yet exist in the grammar.

        let (_, scanner_production_id, scanner_name, symbol_string) =
          get_scanner_info_from_defined(&sym_id, g);

        if let btree_map::Entry::Vacant(e) = g.productions.entry(scanner_production_id) {
          // Insert into grammar any new defined symbol derived from
          // token productions.

          let body_id = BodyId::new(&scanner_production_id, 0);

          g.production_bodies.insert(scanner_production_id, vec![body_id]);

          g.bodies.insert(body_id, types::Body {
            len: 1,
            syms: vec![BodySymbolRef {
              annotation: String::default(),
              consumable: true,
              exclusive: false,
              original_index: 0,
              scanner_index: 0,
              scanner_length: 1,
              sym_id,
              tok: Token::default(),
            }],
            prod: scanner_production_id,
            id: body_id,
            bc_id: 0,
            reduce_fn_ids: vec![],
            origin_location: Token::empty(),
          });

          e.insert(crate::types::Production::new(
            &scanner_name,
            &scanner_name,
            scanner_production_id,
            1,
            Token::empty(),
            true,
          ));
        }
      }
      sym if sym.is_defined() => {
        let (_, scanner_production_id, scanner_name, symbol_string) =
          get_scanner_info_from_defined(&sym_id, g);

        let strings = [symbol_string.as_str()];

        let sym = g.symbols.get(&sym_id).unwrap();

        convert_scanner_symbol_to_production(
          g,
          &strings,
          scanner_production_id,
          scanner_name,
          Token::empty(),
        );
      }

      // This initially process token-production symbols dumped in to the
      // queue, but as we process these productions, we'll inevitably
      // encounter regular production symbols. The process of converting a
      // production symbol into scanner production is identical to that of
      // processing a token-production, so we can just combine the match
      // selectors of TokenProductions and Production
      // symbols.
      SymbolID::Production(prod_id, _) | SymbolID::TokenProduction(prod_id, _) => {
        match g.productions.get(prod_id) {
          Some(production) => {
            let production = production.clone();
            let scanner_name = create_scanner_name(&production.guid_name);
            let scanner_production_id = ProductionId::from(&scanner_name);
            if !g.productions.contains_key(&scanner_production_id) {
              let scanner_bodies: Vec<Body> = g
                .production_bodies
                .get(prod_id)
                .unwrap()
                .iter()
                .enumerate()
                .map(|(body_index, body_id)| {
                  let natural_body = g.bodies.get(body_id).unwrap();

                  let scanner_symbols = natural_body.syms.iter().flat_map(|sym| {
                    let sym_id = &sym.sym_id;
                    match sym_id {
                      // For any production or token
                      // production symbol encountered, create
                      // a new symbol that references the
                      // equivalent scanner production name,
                      // and submit this production for
                      // processing into a new scanner
                      // production.
                      SymbolID::Production(prod_id, grammar_id)
                      | SymbolID::TokenProduction(prod_id, grammar_id) => {
                        let production = g.productions.get(prod_id).unwrap();

                        let scanner_name = create_scanner_name(&production.guid_name);

                        let scanner_production_id = ProductionId::from(&scanner_name);

                        let new_symbol_id =
                          SymbolID::Production(scanner_production_id, *grammar_id);

                        scanner_production_queue.push_back(*sym_id);

                        vec![BodySymbolRef {
                          annotation: String::default(),
                          consumable: true,
                          exclusive: sym.exclusive,
                          original_index: 0,
                          scanner_index: 0,
                          scanner_length: 1,
                          sym_id: new_symbol_id,
                          tok: Token::default(),
                        }]
                      }
                      sym if sym.is_defined() => {
                        let (new_symbol_id, ..) = get_scanner_info_from_defined(sym_id, g);

                        scanner_production_queue.push_back(*sym_id);

                        vec![BodySymbolRef {
                          annotation: String::default(),
                          consumable: true,
                          exclusive: sym.is_exclusive(),
                          original_index: 0,
                          scanner_index: 0,
                          scanner_length: 1,
                          sym_id: new_symbol_id,
                          tok: Token::default(),
                        }]
                      }
                      _ => vec![sym.clone()],
                    }
                  });

                  let symbols: Vec<BodySymbolRef> = scanner_symbols.collect();

                  Body {
                    id: BodyId::new(&scanner_production_id, body_index),
                    len: symbols.len() as u16,
                    prod: scanner_production_id,
                    syms: symbols,
                    bc_id: 0,
                    reduce_fn_ids: vec![],
                    origin_location: production.original_location.clone(),
                  }
                })
                .collect();

              let mut bodies = vec![];

              for body in scanner_bodies {
                bodies.push(body.id);
                g.bodies.insert(body.id, body);
              }

              g.productions.insert(
                scanner_production_id,
                crate::types::Production::new(
                  &scanner_name,
                  &scanner_name,
                  scanner_production_id,
                  bodies.len() as u16,
                  production.original_location.clone(),
                  true,
                ),
              );

              g.production_bodies.insert(scanner_production_id, bodies);
            }
          }
          _ => match g.production_symbols.get(&sym_id) {
            Some(tok) => {
              panic!(
                "Unable to find production definition \n{}",
                tok.blame(1, 1, "production_name", None)
              );
            }
            _ => {
              panic!("Unable to find production definition {:?}", sym_id);
            }
          },
        }
      }
      _ => {}
    }
  }
}

/// Converts an array of strings into scanner bodies
/// for a given scanner production name.
fn convert_scanner_symbol_to_production(
  g: &mut GrammarStore,
  strings: &[&str],
  prod_id: ProductionId,
  scanner_name: String,
  origin_location: Token,
) {
  let GrammarStore {
    symbol_strings: symbols_string_table,
    symbols: symbols_table,
    productions: production_table,
    production_bodies: production_bodies_table,
    bodies: bodies_table,
    ..
  } = g;

  if let btree_map::Entry::Vacant(e) = production_table.entry(prod_id) {
    let number_of_bodies = strings.len() as u16;

    let mut body_ids = vec![];

    for (id, symbol_string) in strings.iter().enumerate() {
      // Insert into grammar any new defined symbol derived from
      // token productions.

      let new_body_symbols =
        create_defined_symbols_from_string(symbol_string, symbols_string_table, symbols_table);

      let body_id = BodyId::new(&prod_id, id);

      body_ids.push(body_id);

      bodies_table.insert(body_id, Body {
        len: new_body_symbols.len() as u16,
        syms: new_body_symbols,
        prod: prod_id,
        id: body_id,
        bc_id: 0,
        reduce_fn_ids: vec![],
        origin_location: origin_location.clone(),
      });
    }

    production_bodies_table.insert(prod_id, body_ids);

    e.insert(crate::types::Production::new(
      &scanner_name,
      &scanner_name,
      prod_id,
      number_of_bodies,
      Token::empty(),
      true,
    ));
  }
}

/// Converts a string sequence into a set of BodySymbolRef references,
/// interning whatever single byte symbol is not already present in the
/// grammar. TODO: This may also split utf8 symbols into byte sequences.
///
/// Expects `symbols_string_table` and `symbols_table` to be mutable
/// references to the corresponding members in a [GrammarStore]
fn create_defined_symbols_from_string(
  symbol_string: &str,
  symbols_string_table: &mut BTreeMap<SymbolID, String>,
  symbols_table: &mut BTreeMap<SymbolID, Symbol>,
) -> Vec<BodySymbolRef> {
  let chars: Vec<char> = symbol_string.chars().collect();

  let new_body_symbols: Vec<BodySymbolRef> = chars
    .iter()
    .enumerate()
    .map(|(index, byte)| {
      let string = byte.to_string();

      let id = get_literal_id(&string, false);

      symbols_table.entry(id).or_insert_with(|| {
        symbols_string_table.insert(id, string);

        Symbol {
          byte_length:   byte.len_utf8() as u32,
          cp_len:        1,
          bytecode_id:   0,
          guid:          id,
          scanner_only:  true,
          friendly_name: id.to_default_string(),
        }
      });

      BodySymbolRef {
        annotation: String::default(),
        consumable: true,
        exclusive: false,
        original_index: 0,
        scanner_index: index as u32,
        scanner_length: chars.len() as u32,
        sym_id: id,
        tok: Token::default(),
      }
    })
    .collect();

  new_body_symbols
}

#[inline]
pub(crate) fn get_scanner_info_from_defined(
  sym_id: &SymbolID,
  root: &GrammarStore,
) -> (SymbolID, ProductionId, String, String) {
  let (scanner_name, symbol_string) = match sym_id {
    sym if sym.is_defined() => {
      let symbol_string = root.symbol_strings.get(sym_id).unwrap().to_owned();

      let escaped_symbol_string = symbol_string
        .chars()
        .into_iter()
        .map(|c| (c as u32).to_string())
        .collect::<Vec<_>>()
        .join("_");

      (create_defined_scanner_name(&escaped_symbol_string), symbol_string)
    }
    SymbolID::TokenProduction(production_id, _) => {
      let symbol_string = root.productions.get(production_id).unwrap().guid_name.clone();
      (create_scanner_name(&symbol_string), symbol_string)
    }
    sym => {
      let symbol_string = sym.to_default_string();
      (create_scanner_name(&symbol_string), symbol_string)
    }
  };

  let scanner_production_id = ProductionId::from(&scanner_name);

  let new_symbol_id = SymbolID::Production(scanner_production_id, root.guid);

  (new_symbol_id, scanner_production_id, scanner_name, symbol_string)
}

/// Merge related grammars into a single GrammarStore
///
/// `root` is assumed to derived from the root source grammar, and
/// grammars are all other GrammarStores derived from grammars
/// imported directly or indirectly from the root source grammar.

fn merge_grammars(
  root: &mut GrammarStore,
  grammars: &[GrammarStore],
  errors: &mut Vec<ParseError>,
) {
  let mut grammars_lookup = HashMap::<GrammarId, &GrammarStore>::new();

  // Merge grammar data into a single store
  for import_grammar in grammars {
    grammars_lookup.insert(import_grammar.guid, import_grammar);

    root
      .production_ignore_symbols
      .extend(import_grammar.production_ignore_symbols.clone().into_iter());

    // Merge all symbols
    for (id, sym) in &import_grammar.symbols {
      if !root.symbols.contains_key(id) {
        root.symbols.insert(*id, sym.clone());

        if id.is_defined() {
          match import_grammar.symbol_strings.get(id) {
            Some(string) => {
              root.symbol_strings.insert(*id, string.to_owned());
            }
            None => {}
          }
        }
      }
    }
  }

  // Merge all referenced foreign productions into the root.
  let mut symbol_queue = VecDeque::from_iter(root.production_symbols.clone());

  while let Some((sym, tok)) = symbol_queue.pop_front() {
    if let Some(grammar_id) = sym.get_grammar_id() {
      if grammar_id != root.guid {
        match grammars_lookup.get(&grammar_id) {
          Some(import_grammar) => {
            if let Some(prod_id) = sym.get_production_id() {
              if let std::collections::btree_map::Entry::Vacant(e) = root.productions.entry(prod_id)
              {
                match import_grammar.productions.get(&prod_id) {
                  Some(production) => {
                    // import the foreign production
                    e.insert(production.clone());

                    let bodies = import_grammar.production_bodies.get(&prod_id).unwrap().clone();

                    // Import all bodies referenced by this
                    // production
                    for body_id in &bodies {
                      let body = import_grammar.bodies.get(body_id).unwrap().clone();

                      // Add every Production symbol to
                      // the queue
                      for sym in &body.syms {
                        match sym.sym_id {
                          SymbolID::Production(..) => {
                            symbol_queue.push_back((sym.sym_id, sym.tok.clone()))
                          }
                          SymbolID::TokenProduction(prod, grammar) => {
                            root.symbols.entry(sym.sym_id).or_insert_with(|| {
                              (import_grammar.symbols.get(&sym.sym_id).unwrap().clone())
                            });

                            // Remap the production token symbol to regular a production symbol and submit as a merge candidate.
                            symbol_queue
                              .push_back((SymbolID::Production(prod, grammar), sym.tok.clone()))
                          }
                          _ => {}
                        }
                      }

                      root.bodies.insert(*body_id, body);
                    }

                    // Import the map of production id to
                    // bodies
                    root.production_bodies.insert(prod_id, bodies);
                  }
                  None => {
                    errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
                      message: format!(
                        "Can't find production {}::{} in {:?} \n{}",
                        get_production_plain_name(&prod_id, root),
                        grammar_id,
                        import_grammar.source_path,
                        tok.blame(1, 1, "", None)
                      ),
                      inline_message: String::new(),
                      loc: Token::empty(),
                    }));
                  }
                }
              }
            }
          }
          None => {}
        }
      }
    }
  }
}

/// Takes a Grammar produces core primitive tables;
///
/// ## Arguments
///
/// - `grammar` - A hcg AST node
/// - `source` - The source string of the hcg.
/// - `absolute_path` - The absolute path of the hcg's source file
///   Used to resolve linked grammars.
///  

pub fn pre_process_grammar<'a>(
  g: &'a ast::Grammar,
  path: &PathBuf,
  mut friendly_name: &'a str,
) -> (GrammarStore, Vec<ParseError>) {
  let mut import_names_lookup = ImportProductionNameTable::new();
  let mut production_bodies_table = ProductionBodiesTable::new();
  let mut production_table = ProductionTable::new();
  let mut bodies_table = BodyTable::new();
  let mut symbols_table = SymbolsTable::new();
  let mut symbols_string_table = SymbolStringTable::new();
  let mut post_process_productions: VecDeque<Box<ast::Production>> = VecDeque::new();
  let mut production_symbols_table = BTreeMap::new();
  let guid_name = get_guid_grammar_name(path).unwrap();
  let guid = GrammarId(hash_id_value_u64(&guid_name));
  let mut reduce_functions = ReduceFunctionTable::new();
  let mut parse_errors = vec![];
  let mut global_ignore_symbols = vec![];
  let mut export_names = vec![];

  {
    let mut tgs = TempGrammarStore {
      local_guid: &guid_name,
      absolute_path: path,
      import_names_lookup: &mut import_names_lookup,
      symbols_table: &mut symbols_table,
      symbols_string_table: &mut symbols_string_table,
      bodies_table: &mut bodies_table,
      prods: &mut production_table,
      prod_syms: &mut production_symbols_table,
      bodies: &mut production_bodies_table,
      errors: &mut parse_errors,
      reduce_functions: &mut reduce_functions,
    };

    // Process meta data, including EXPORT, IMPORT, and IGNORE meta
    // data
    for obj in g.preamble.iter() {
      match obj {
        ASTNode::Name(box ast::Name { name }) => friendly_name = name,
        ASTNode::Ignore(box ast::Ignore { symbols }) => {
          for symbol in symbols {
            if let Some(id) = intern_symbol(symbol, &mut tgs) {
              global_ignore_symbols.push(id)
            }
          }
        }
        ASTNode::Import(import) => {
          let mut uri = PathBuf::from(&import.uri);

          let local_name = import.reference.to_string();

          // Resolve path names. Since this touches the filesystem,
          // it's bypassed when running tests to keep tests pure.

          if !uri.is_absolute() {
            match path.parent() {
              None => {}
              Some(new_path) => {
                let mut new_path = new_path.to_owned();

                new_path.push(uri);

                match new_path.canonicalize() {
                  Ok(result) => {
                    uri = result;
                  }
                  Err(err) => {
                    tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
                      message: format!(
                        "Problem encountered when verifying imported grammar [{}]",
                        local_name
                      ),
                      inline_message: "Could not find imported grammar".to_string(),
                      loc: import.Token(),
                    }));
                    continue;
                  }
                }
              }
            }
          }

          let import_uuid = get_guid_grammar_name(&uri).unwrap();

          // Map the foreign grammar's local name to the uuid and
          // absolute path

          tgs.import_names_lookup.insert(local_name, (import_uuid, uri));
        }
        ASTNode::Export(box ast::Export { production, reference }) => {
          let production_id = get_production_id_from_node(production, &mut tgs);
          export_names.push((production_id, reference.to_string()));
        }
        _ => {}
      }
    }

    // Process main grammar data, which include
    // Productions, IR states, and out of band functions
    for node in g.content.iter() {
      match node {
        ASTNode::Production(_) => {
          let production = pre_process_production(node, &mut tgs, &mut post_process_productions);
          if production != ProductionId::default() && export_names.is_empty() {
            export_names.push((production, "default".to_string()));
          }
        }
        ASTNode::ProductionMerged(prod) => {}
        ASTNode::IR_STATE(ir_state) => {}
        ASTNode::Out_Of_Band(oob_fn) => {}
        _ => {}
      }
    }

    // Continue processing any generated productions. This may loop
    // for a while as any given production may have several nested
    // anonymous productions through lists `...(+) | ...(*)` and
    // groups `(... | ...)`

    while let Some(node) = post_process_productions.pop_front() {
      pre_process_production(&ASTNode::Production(node), &mut tgs, &mut post_process_productions);
    }
  }

  let production_ignore_symbols =
    production_table.keys().map(|k| (*k, global_ignore_symbols.clone())).collect::<HashMap<_, _>>();

  (
    GrammarStore {
      source_path: path.clone(),
      guid,
      guid_name,
      production_bodies: production_bodies_table,
      productions: production_table,
      bodies: bodies_table,
      symbols: symbols_table,
      symbol_strings: symbols_string_table,
      production_symbols: production_symbols_table,
      imports: import_names_lookup,
      closures: HashMap::new(),
      item_ignore_symbols: HashMap::new(),
      production_ignore_symbols,
      lr_items: BTreeMap::new(),
      reduce_functions,
      export_names,
      friendly_name: friendly_name.to_owned(),
    },
    parse_errors,
  )
}

fn pre_process_production(
  production_node: &ASTNode,
  tgs: &mut TempGrammarStore,
  post_process_productions: &mut VecDeque<Box<ast::Production>>,
) -> ProductionId {
  let mut body_index = 0;

  if let ASTNode::Production(prod) = production_node {
    let production_id = get_production_id_from_node(production_node, tgs);

    let production_guid_name = get_resolved_production_name(production_node, tgs).unwrap();
    let mut bodies = vec![];

    if match tgs.prods.get(&production_id) {
      Some(existing_production) => {
        tgs.errors.push({
          ParseError::COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem {
            message:   format!("production {} already exists!", production_guid_name),
            locations: vec![
              CompileProblem {
                inline_message: String::new(),
                loc: production_node.Token(),
                message: format!("Redefinition of {} occurs here.", production_guid_name),
              },
              CompileProblem {
                inline_message: String::new(),
                loc: existing_production.original_location.clone(),
                message: format!("production {} first defined here.", production_guid_name),
              },
            ],
          })
        });
        false
      }
      None => true,
    } {
      // Extract body data and gather symbol information
      let mut list_index = 0;
      for body in &prod.bodies {
        if let ASTNode::Body(ast_body) = body {
          let (new_bodies, productions) =
            pre_process_body(production_node, ast_body, tgs, &mut list_index);

          for prod in productions {
            post_process_productions.push_back(prod);
          }

          for mut body in new_bodies {
            let id = BodyId::new(&production_id, body_index);

            body.id = id;

            tgs.bodies_table.insert(id, body);

            bodies.push(id);

            body_index += 1;
          }
        }
      }

      tgs.prods.insert(
        production_id,
        crate::types::Production::new(
          &prod.symbol.Token().to_string(),
          &production_guid_name,
          production_id,
          bodies.len() as u16,
          production_node.Token(),
          false,
        ),
      );

      tgs.bodies.insert(production_id, bodies);

      production_id
    } else {
      ProductionId::default()
    }
  } else {
    ProductionId::default()
  }
}

/// Get the resolved production of name applicable nodes.
/// Nodes from which a production name can be derived:
/// - Production_Symbol
/// - Production_Token
/// - Production
/// - Import_Production
///
/// This name is guaranteed to be unique amongst all grammars imported by
/// the root grammar.
///
/// ## Panics
/// This function panics if the node is not one of the above.
///
/// This function also panics if a local imported grammar name does
/// not have a matching `@IMPORT` statement.

fn get_resolved_production_name(node: &ASTNode, tgs: &mut TempGrammarStore) -> Option<String> {
  match node {
    ASTNode::Production_Import_Symbol(prod_imp_sym) => {
      let production_name = &prod_imp_sym.name;

      let local_import_grammar_name = &prod_imp_sym.module;

      match tgs.import_names_lookup.get(local_import_grammar_name) {
        None => {
          tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem{
                        inline_message: String::new(),
                        message: format!(
                            "Unable to resolve production: The production \u{001b}[31m{}\u{001b}[0m cannot be found in the imported grammar \u{001b}[31m{}\u{001b}[0m.", 
                            production_name,
                            local_import_grammar_name
                        ),
                        loc: node.Token(),
                    }));
          None
        }
        Some((grammar_guid_name, _)) => {
          Some(create_production_guid_name(grammar_guid_name, production_name))
        }
      }
    }
    ASTNode::Production_Symbol(prod_sym) => {
      Some(create_production_guid_name(tgs.local_guid, &prod_sym.name))
    }
    ASTNode::Production(prod) => get_resolved_production_name(&prod.symbol, tgs),
    ASTNode::Production_Token(prod_tok) => get_resolved_production_name(&prod_tok.production, tgs),
    _ => {
      tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
        inline_message: String::new(),
        message: "Unexpected node: Unable to resolve production name of this node!".to_string(),
        loc: node.Token(),
      }));
      None
    }
  }
}

/// Get the resolved grammar data of applicable nodes.
/// Nodes from which a grammar name can be derived:
/// - Production_Symbol
/// - Production_Token
/// - Production
/// - Import_Production
///
/// ## Returns
/// A Tuple comprised of the grammar 0:uuid_name, 1:local_name, and
/// 2:absolute_path. local_name is `root` if the grammar maps to
/// currently rendered grammar.
///
/// ## Panics
/// This function panics if the node is not one of the above.
///
/// This function also panics if a local imported grammar name does
/// not have a matching `@IMPORT` statement.

fn get_grammar_info_from_node<'a>(
  node: &'a ASTNode,
  tgs: &'a mut TempGrammarStore,
) -> Option<(&'a str, &'a str, &'a PathBuf)> {
  match node {
    ASTNode::Production_Import_Symbol(prod_imp_sym) => {
      let production_name = &prod_imp_sym.name;

      let local_import_grammar_name = &prod_imp_sym.module;

      match tgs.import_names_lookup.get(local_import_grammar_name) {
        None => {
          tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
            inline_message: String::new(),
            message: format!(
              "Unknown Grammar : The local grammar name \u{001b}[31m{}\u{001b}[0m does not match any imported grammar.",
              local_import_grammar_name
            ),
            loc: node.Token(),
          }));
          None
        }
        Some((resolved_grammar_name, path)) => {
          Some((resolved_grammar_name, local_import_grammar_name, path))
        }
      }
    }
    ASTNode::Production_Symbol(prod_sym) => Some((tgs.local_guid, "root", tgs.absolute_path)),
    ASTNode::Production(prod) => get_grammar_info_from_node(&prod.symbol, tgs),
    ASTNode::Production_Token(prod_tok) => get_grammar_info_from_node(&prod_tok.production, tgs),
    _ => {
      tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
        inline_message: String::new(),
        message: "Unexpected node: Unable to resolve production name of this node!".to_string(),
        loc: node.Token(),
      }));

      None
    }
  }
}

fn get_production_id_from_node(production: &ASTNode, tgs: &mut TempGrammarStore) -> ProductionId {
  if let Some(name) = get_resolved_production_name(production, tgs) {
    ProductionId(hash_id_value_u64(name))
  } else {
    ProductionId(0)
  }
}

/// Convert a direct left recursive production into a right recursive production.
///
///  This uses the following process to convert an immediate recursive production
///  to one the is right recursive:
///
/// With `A -> A r | b | ...` Take `B[ A->b, ... ]` `A'[ A-> A r ... ]`
///
/// Replace bodies of `A` with `B'` after `B[ A->b A' ?]` giving `A -> b A' | b | ...`
///
/// Then form `A'` after `A' [ A-> r A' ? ... ]` giving `A' -> r A' | r | ...`
///
/// Warning: This does nothing with existing reduce functions.
/// This should only be applied to production where execution of
/// reduce functions can be ignored.
pub fn convert_left_to_right(
  g: &mut GrammarStore,
  a_prod_id: ProductionId,
) -> (ProductionId, ProductionId) {
  let a_prod = g.productions.get_mut(&a_prod_id).unwrap();
  let a_token = a_prod.original_location.clone();
  // Ensure the production is left recursive.
  if !a_prod.recursion_type.contains(RecursionType::LEFT_DIRECT) {
    panic!("Production is not left direct recursive.");
  }

  // Remove recursion flag as it no longer applies to this production.
  a_prod.recursion_type = a_prod.recursion_type.xor(RecursionType::LEFT_DIRECT);

  let body_ids = g.production_bodies.get(&a_prod_id).unwrap().clone();

  let bodies =
    body_ids.iter().map(|body_id| g.bodies.get(body_id).unwrap().clone()).collect::<Vec<_>>();

  let bodies = body_ids
    .iter()
    .map(|body_id| g.bodies.get(body_id).unwrap())
    .map(|b| match b.syms[0].sym_id {
      SymbolID::Production(p, _) => (b.id, p == a_prod.id),
      SymbolID::TokenProduction(p, _) => (b.id, p == a_prod.id),
      _ => (b.id, false),
    })
    .collect::<Vec<_>>();

  let left_bodies =
    bodies.iter().filter_map(|(i, b)| if *b { Some(i) } else { None }).collect::<Vec<_>>();

  let non_bodies =
    bodies.iter().filter_map(|(i, b)| if *b { None } else { Some(i) }).collect::<Vec<_>>();

  let a_prime_prod_name = (a_prod.original_name.clone() + "_prime");
  let a_prime_prod_guid_name = create_production_guid_name(&g.guid_name, &a_prime_prod_name);
  let a_prime_prod_id = ProductionId::from(&a_prime_prod_guid_name);
  let a_prime_prod = Production::new(
    &a_prime_prod_name,
    &a_prime_prod_guid_name,
    a_prime_prod_id,
    (left_bodies.len() * 2) as u16,
    a_token.clone(),
    a_prod.is_scanner,
  );

  let a_prim_sym = BodySymbolRef {
    sym_id: SymbolID::Production(a_prime_prod_id, g.guid),
    original_index: 0,
    annotation: "".to_string(),
    consumable: true,
    scanner_length: 0,
    scanner_index: 0,
    exclusive: false,
    tok: a_token.clone(),
  };

  let new_B_bodies = non_bodies
    .iter()
    .enumerate()
    .flat_map(|(i, b)| {
      let body = g.bodies.get(b).unwrap();

      let mut body_a = body.clone();
      let mut body_b = body.clone();

      body_b.syms.push(a_prim_sym.clone());
      body_a.id = BodyId::new(&a_prod_id, i * 2);
      body_b.id = BodyId::new(&a_prod_id, i * 2 + 1);
      body_a.len = body_a.syms.len() as u16;
      body_b.len = body_b.syms.len() as u16;

      vec![body_a, body_b]
    })
    .collect::<Vec<_>>();

  let new_A_bodies = left_bodies
    .iter()
    .enumerate()
    .flat_map(|(i, b)| {
      let body = g.bodies.get(b).unwrap();
      let mut body_a = body.clone();
      let mut body_b = body.clone();
      body_a.prod = a_prime_prod_id;
      body_b.prod = a_prime_prod_id;
      body_a.syms.remove(0);
      body_b.syms.remove(0);
      body_b.syms.push(a_prim_sym.clone());
      body_a.id = BodyId::new(&a_prime_prod_id, i * 2);
      body_b.id = BodyId::new(&a_prime_prod_id, i * 2 + 1);
      body_a.len = body_a.syms.len() as u16;
      body_b.len = body_b.syms.len() as u16;
      vec![body_a, body_b]
    })
    .collect::<Vec<_>>();

  // Replace the base production's bodies with B bodies.
  // Add A prime production to the grammar.

  g.productions.insert(a_prime_prod_id, a_prime_prod);
  g.production_bodies.insert(a_prod_id, new_B_bodies.iter().map(|b| b.id).collect::<Vec<_>>());
  g.production_bodies
    .insert(a_prime_prod_id, new_A_bodies.iter().map(|b| b.id).collect::<Vec<_>>());

  for b in new_A_bodies {
    let id = b.id;
    g.bodies.insert(id, b);
  }

  for b in new_B_bodies {
    let id = b.id;
    g.bodies.insert(id, b);
  }

  let prod_sym = SymbolID::Production(a_prime_prod_id, g.guid);

  (a_prod_id, a_prime_prod_id)
}

fn create_production(
  name: &str,
  bodies: &[ASTNode],
  token: Token,
) -> (ASTNode, Box<ast::Production>) {
  // Create a virtual production and symbol to go in its place
  let symbol = ASTNode::Production_Symbol(super::data::ast::Production_Symbol::new(
    name.to_string(),
    token.clone(),
  ));

  let production =
    super::data::ast::Production::new(false, symbol.clone(), bodies.to_vec(), false, token);

  (symbol, production)
}

fn pre_process_body(
  production: &ASTNode,
  body: &ast::Body,
  tgs: &mut TempGrammarStore,
  list_index: &mut u32,
) -> (Vec<types::Body>, Vec<Box<ast::Production>>) {
  if let ASTNode::Returned(ret) = &body.reduce_function {
    // Extract the function and insert into function table?
  }

  let production_name = get_resolved_production_name(production, tgs).unwrap();

  fn create_body_vectors(
    token: &Token,
    symbols: &Vec<(usize, &ASTNode)>,
    production_name: &String,
    tgs: &mut TempGrammarStore,
    list_index: &mut u32,
  ) -> (Vec<(Token, Vec<BodySymbolRef>)>, Vec<Box<ast::Production>>) {
    let mut bodies = vec![];
    let mut productions = vec![];

    bodies.push((token.clone(), vec![]));

    for (index, sym) in symbols {
      let original_bodies = 0..bodies.len();

      let SymbolData {
        annotation,
        is_list,
        is_group,
        is_optional,
        is_shift_nothing,
        is_meta,
        is_exclusive,
        sym_atom,
      } = get_symbol_details(sym, tgs);

      if let Some(mut sym) = sym_atom.to_owned() {
        let mut generated_symbol = ASTNode::NONE;

        if is_meta {
          // TODO: Separate meta data symbols into it's own table that
          // maps meta symbols to a body and its
          // index.
          continue;
        }

        if is_optional {
          // Need to create new bodies that contains all permutations
          // of encountered symbols except for the currently
          // considered symbol. This is achieved by duplicating all
          // body vectors, then adding the current symbol to the
          // original vectors, but not the duplicates.
          for entry in bodies.clone() {
            bodies.push(entry)
          }
        }
        if let ASTNode::AnyGroup(group) = sym {
          // New bodies are created with the values of the any group
          // symbol being distributed to each body.

          let mut pending_bodies = vec![];

          fn get_index_permutations(indice_candidates: Vec<usize>) -> Vec<Vec<usize>> {
            if indice_candidates.len() > 1 {
              let mut out = vec![];
              for (i, candidate) in indice_candidates.iter().enumerate() {
                let mut remainder = indice_candidates.clone();
                remainder.remove(i);
                for mut permutation in get_index_permutations(remainder) {
                  permutation.insert(0, *candidate);
                  out.push(permutation)
                }
              }
              out
            } else {
              vec![indice_candidates]
            }
          }

          let indices = group.symbols.iter().enumerate().map(|(i, _)| i).collect();

          let candidate_symbols =
            group.symbols.iter().enumerate().map(|(i, s)| (i + index, s)).collect::<Vec<_>>();

          for permutation in
            if group.unordered { get_index_permutations(indices) } else { vec![indices] }
          {
            let symbols = permutation.iter().map(|i| candidate_symbols[*i]).collect::<Vec<_>>();

            let (mut new_bodies, mut new_productions) =
              create_body_vectors(token, &symbols, production_name, tgs, list_index);

            pending_bodies.append(&mut new_bodies);

            productions.append(&mut new_productions);
          }

          let mut new_bodies = vec![];

          for pending_body in pending_bodies {
            if pending_body.1.len() == 0 {
              continue;
            }

            for body in &mut bodies[original_bodies.clone()] {
              let mut new_body = body.clone();
              new_body.1.extend(pending_body.1.iter().cloned());
              new_bodies.push(new_body)
            }
          }

          bodies = new_bodies;

          continue;
        } else if is_group {
          // Need to create new production that the virtual group
          // production is bound to, add it to the list of
          // currently considered productions, and replace
          // this symbol with a production symbol pointing
          // to the group.

          // Except, if there are no functions within the production
          // bodies we can simply lower bodies of the group production
          // into the host production.
          if let ASTNode::Group_Production(group) = sym {
            // All bodies are plain without annotations or functions
            if annotation.is_empty() && !some_bodies_have_reduce_functions(&group.bodies) {
              // For each body in the group clone the existing
              // body
              // lists and process each list
              // independently, inserting the new symbols
              // into the existing bodies. We must make sure the
              // indices are preserved since only the
              // last symbol in each body can be bound
              // to the index of the group production
              // symbol.

              let mut pending_bodies = vec![];

              for body in &group.bodies {
                if let ASTNode::Body(body) = body {
                  let (mut new_bodies, mut new_productions) = create_body_vectors(
                    &sym.Token(),
                    &body.symbols.iter().map(|s| (9999, s)).collect::<Vec<_>>(),
                    production_name,
                    tgs,
                    list_index,
                  );

                  pending_bodies.append(&mut new_bodies);

                  productions.append(&mut new_productions);
                }
              }

              let mut new_bodies = vec![];

              for pending_body in pending_bodies {
                for body in &mut bodies[original_bodies.clone()] {
                  let mut new_body = body.clone();
                  new_body.1.extend(pending_body.1.iter().cloned());
                  new_bodies.push(new_body)
                }
              }

              bodies.splice(original_bodies, new_bodies);

              // We do not to process the existing symbol as it is
              // now replaced with
              // it's component body symbols,
              // so we'll skip the rest of the loop
              continue;
            } else {
              let (prod_sym, production) = create_production(
                &(production_name.to_owned() + "_group_" + &index.to_string()),
                &group.bodies,
                group.tok.clone(),
              );

              productions.push(production);
              generated_symbol = prod_sym;
              sym = &generated_symbol;
            }
          } else {
            tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
              inline_message: String::new(),
              message: "I don't know what to do with this.".to_string(),
              loc: sym.Token(),
            }));
          }
        } else if is_list {
          // Create a new production that turns `A => a` into
          // `A => a | A => A a` and produce a symbol id that points
          // to that production.
          static none_: ASTNode = ASTNode::NONE;

          match sym {
            ASTNode::Optional_List_Production(_) | ASTNode::List_Production(_) => {
              let (symbol, terminal_symbol, tok) = match sym {
                ASTNode::Optional_List_Production(list) => {
                  (&list.symbols, &list.terminal_symbol, list.tok.clone())
                }
                ASTNode::List_Production(list) => {
                  (&list.symbols, &list.terminal_symbol, list.tok.clone())
                }
                _ => (&none_, &none_, Token::new()),
              };

              let mut body_a = super::data::ast::Body::new(
                false,
                vec![symbol.clone()],
                None,
                ASTNode::NONE,
                sym.Token(),
              );

              let mut body_b = body_a.clone();

              match terminal_symbol {
                ASTNode::Literal(box Literal { val, .. }) if val == "\"" || val == "\'" => {}
                _ => {
                  // Create new bodies that will be bound to the
                  // symbol.

                  let list_vector_reduce: ASTNode =
                    compile_ascript_ast("[$first, $last]".as_bytes().to_vec()).unwrap();

                  let list_symbol_reduce: ASTNode =
                    compile_ascript_ast("[$first]".as_bytes().to_vec()).unwrap();

                  body_a.reduce_function = ASTNode::Ascript(Ascript::new(
                    list_symbol_reduce.clone(),
                    list_symbol_reduce.Token().clone(),
                  ));

                  body_b.reduce_function = ASTNode::Ascript(Ascript::new(
                    list_vector_reduce.clone(),
                    list_vector_reduce.Token().clone(),
                  ));

                  match terminal_symbol {
                    ASTNode::NONE => {}
                    _ => {
                      body_b.symbols.insert(0, terminal_symbol.clone());
                    }
                  }
                }
              };

              (*list_index) += 1;

              let (prod_sym, mut production) = create_production(
                &(production_name.to_owned() + "_list_" + &(*list_index).to_string()),
                &[ASTNode::Body(body_b), ASTNode::Body(body_a)],
                tok.clone(),
              );

              // Add the production symbol to the front of the body
              // to make the production left recursive
              if let ASTNode::Body(body) = &mut production.bodies[0] {
                body.symbols.insert(0, prod_sym.clone());
              }

              productions.push(production);
              generated_symbol = prod_sym;
              sym = &generated_symbol;
            }
            _ => {
              tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
                inline_message: String::new(),
                message: "I don't know what to do with this.".to_string(),
                loc: sym.Token(),
              }));
            }
          }
        }

        if let Some(id) = intern_symbol(sym, tgs) {
          for (_, vec) in &mut bodies[original_bodies] {
            vec.push(BodySymbolRef {
              original_index: *index as u32,
              sym_id: id,
              annotation: annotation.clone(),
              consumable: !is_shift_nothing,
              exclusive: is_exclusive,
              scanner_index: 0,
              scanner_length: 0,
              tok: sym.Token(),
            });
          }
        }
      }
    }

    (bodies, productions)
  }

  let (bodies, productions) = create_body_vectors(
    &body.Token(),
    &body
      .symbols
      .iter()
      .fold((0, vec![]), |mut b, s| {
        b.1.push((b.0, s));
        b.0 += match s {
          ASTNode::AnyGroup(box s) => s.symbols.len(),
          _ => 1,
        };
        b
      })
      .1,
    &production_name,
    tgs,
    list_index,
  );

  let reduce_fn_ids = match body.reduce_function {
    ASTNode::Reduce(..) | ASTNode::Ascript(..) => {
      let reduce_id = ReduceFunctionId::new(&body.reduce_function);

      tgs
        .reduce_functions
        .entry(reduce_id)
        .or_insert_with(|| ReduceFunctionType::new(&body.reduce_function));

      vec![reduce_id]
    }
    _ => vec![],
  };

  let mut unique_bodies = vec![];
  let mut seen = HashSet::new();

  for (t, b) in bodies {
    let sym = BodyId::from_syms(&b.iter().map(|s| s.sym_id).collect::<Vec<_>>());
    if !seen.contains(&sym) {
      unique_bodies.push(types::Body {
        syms: b.clone(),
        len: b.len() as u16,
        prod: get_production_id_from_node(production, tgs),
        id: BodyId::default(),
        bc_id: 0,
        reduce_fn_ids: reduce_fn_ids.clone(),
        origin_location: t.clone(),
      });
      seen.insert(sym);
    }
  }

  (unique_bodies, productions)
}

fn some_bodies_have_reduce_functions(bodies: &Vec<ASTNode>) -> bool {
  bodies.iter().any(|b| {
    if let ASTNode::Body(body) = b {
      body.reduce_function.GetType() != 0
    } else {
      false
    }
  })
}

/// Returns an appropriate SymbolID::Defined* based on the input
/// string

fn get_literal_id(string: &String, exclusive: bool) -> SymbolID {
  let identifier = Regex::new(r"[\w_-][\w\d_-]*$").unwrap();
  let number = Regex::new(r"\d+$").unwrap();

  if number.is_match(string) {
    if exclusive {
      SymbolID::ExclusiveDefinedNumeric(StringId::from(string))
    } else {
      SymbolID::DefinedNumeric(StringId::from(string))
    }
  } else if identifier.is_match(string) {
    if exclusive {
      SymbolID::ExclusiveDefinedIdentifier(StringId::from(string))
    } else {
      SymbolID::DefinedIdentifier(StringId::from(string))
    }
  } else {
    if exclusive {
      SymbolID::ExclusiveDefinedSymbol(StringId::from(string))
    } else {
      SymbolID::DefinedSymbol(StringId::from(string))
    }
  }
}

/// Adds a symbol to the symbol_table

fn intern_symbol(
  sym: &ASTNode, // , symbols_table,
  tgs: &mut TempGrammarStore,
) -> Option<SymbolID> {
  fn process_literal(string: &String, tgs: &mut TempGrammarStore, is_exclusive: bool) -> SymbolID {
    let mut id = get_literal_id(string, is_exclusive);

    if let std::collections::btree_map::Entry::Vacant(e) = tgs.symbols_table.entry(id) {
      tgs.symbols_string_table.insert(id, string.to_owned());

      let byte_length = string.bytes().len() as u32;
      let code_point_length = string.chars().count() as u32;

      e.insert(Symbol {
        bytecode_id: 0,
        guid: id,
        byte_length,
        cp_len: code_point_length,
        scanner_only: false,
        friendly_name: String::new(),
      });
    }

    id
  }

  fn get_production_hash_ids(
    node: &ASTNode,
    tgs: &mut TempGrammarStore,
  ) -> Option<(ProductionId, GrammarId)> {
    match node {
      ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
        let production_id = get_production_id_from_node(node, tgs);
        get_grammar_info_from_node(node, tgs)
          .map(|data| (production_id, GrammarId(hash_id_value_u64(data.0))))
      }
      _ => {
        tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
          inline_message: "This is not a hashable production symbol.".to_string(),
          message: "[INTERNAL ERROR]".to_string(),
          loc: node.Token(),
        }));
        None
      }
    }
  }

  fn process_production(
    node: &ASTNode,
    tgs: &mut TempGrammarStore,
    tok: Token,
  ) -> Option<SymbolID> {
    get_production_hash_ids(node, tgs).map(|(production_id, grammar_id)| {
      let id = SymbolID::Production(production_id, grammar_id);

      tgs.prod_syms.insert(id, tok);

      id
    })
  }

  fn process_token_production(
    node: &ast::Production_Token,
    tgs: &mut TempGrammarStore,
    tok: Token,
  ) -> Option<SymbolID> {
    match process_production(&node.production, tgs, tok.clone()) {
      Some(SymbolID::Production(prod_id, grammar_id)) => {
        let id = SymbolID::Production(prod_id, grammar_id);
        let tok_id = SymbolID::TokenProduction(prod_id, grammar_id);

        tgs.prod_syms.insert(id, tok.clone());
        tgs.prod_syms.insert(tok_id, tok);

        tgs.symbols_table.entry(tok_id).or_insert(Symbol {
          bytecode_id:   0,
          guid:          id,
          byte_length:   0,
          cp_len:        0,
          scanner_only:  false,
          friendly_name: String::new(),
        });

        Some(tok_id)
      }
      _ => None,
    }
  }

  match sym {
    ASTNode::Generated(gen) => match gen.val.as_str() {
      "sp" => Some(SymbolID::GenericSpace),
      "tab" => Some(SymbolID::GenericHorizontalTab),
      "nl" => Some(SymbolID::GenericNewLine),
      "id" => Some(SymbolID::GenericIdentifier),
      "num" => Some(SymbolID::GenericNumber),
      "sym" => Some(SymbolID::GenericSymbol),
      _ => Some(SymbolID::Undefined),
    },
    ASTNode::Exclusive_Literal(literal) => Some(process_literal(&literal.val, tgs, true)),
    ASTNode::Literal(literal) => Some(process_literal(&literal.val, tgs, false)),
    ASTNode::End_Of_File(_) => Some(SymbolID::EndOfFile),
    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      process_production(sym, tgs, sym.Token())
    }
    ASTNode::Production_Token(token) => process_token_production(token, tgs, sym.Token()),
    _ => {
      tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
        inline_message: "Unexpected ASTNode while attempting to intern symbol".to_string(),
        message: "[INTERNAL ERROR]".to_string(),
        loc: sym.Token(),
      }));
      None
    }
  }
}
/// Get a flattened view of a symbol's immediate AST

fn get_symbol_details<'a>(mut sym: &'a ASTNode, tgs: &mut TempGrammarStore) -> SymbolData<'a> {
  let mut data = SymbolData {
    annotation:       String::new(),
    is_list:          false,
    is_group:         false,
    is_optional:      false,
    is_shift_nothing: false,
    is_meta:          false,
    is_exclusive:     false,
    sym_atom:         None,
  };

  loop {
    match sym {
      ASTNode::AnnotatedSymbol(annotated) => {
        // Removes the dangling `^`, as in `^annotation_name`
        data.annotation = annotated.reference.val[1..].to_owned();
        sym = &annotated.symbol;
      }
      ASTNode::OptionalSymbol(optional) => {
        data.is_optional = true;
        sym = &optional.symbol;
      }
      ASTNode::NonCaptureSymbol(non_cap) => {
        data.is_shift_nothing = true;
        sym = &non_cap.sym;
      }
      ASTNode::Exclude(_) | ASTNode::Look_Ignore(_) => {
        data.is_meta = true;
        break;
      }
      ASTNode::Group_Production(_) => {
        data.is_group = true;
        break;
      }
      ASTNode::List_Production(_) => {
        data.is_list = true;
        break;
      }
      ASTNode::Optional_List_Production(_) => {
        data.is_list = true;
        data.is_optional = true;
        break;
      }
      ASTNode::Exclusive_Literal(_) => {
        data.is_exclusive = true;
        break;
      }
      // This symbol types are "real" symbols, in as much
      // as they represent actual parsable entities which are
      // submitted to the bytecode compiler for evaluation
      ASTNode::Generated(_)
      | ASTNode::AnyGroup(_)
      | ASTNode::Literal(_)
      | ASTNode::Empty(_)
      | ASTNode::End_Of_File(_)
      | ASTNode::Production_Symbol(_)
      | ASTNode::Production_Token(_)
      | ASTNode::Production_Import_Symbol(_) => {
        break;
      }
      _ => {
        tgs.errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
          inline_message: format!("Unexpected ASTNode {}", sym.GetType()),
          message: "[INTERNAL ERROR]".to_string(),
          loc: sym.Token(),
        }));
        break;
      }
    }
  }

  data.sym_atom = Some(sym);

  data
}
