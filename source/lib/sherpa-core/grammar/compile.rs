use super::{
  create_closure,
  create_defined_scanner_name,
  create_production_guid_name,
  create_scanner_name,
  data::{
    ast,
    ast::{ASTNode, ASTNodeTraits, Grammar},
  },
  get_production_start_items,
  parse::compile_grammar_ast,
};
use crate::{
  grammar::{
    data::ast::Ascript,
    load::{get_usable_thread_count, load_all},
    parse::compile_ascript_ast,
  },
  journal::{report::ReportType, Journal},
  types,
  types::*,
};
use core::panic;

use regex::Regex;
use std::{
  collections::{btree_map, BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
  ffi::OsStr,
  path::PathBuf,
  result,
  sync::Arc,
  thread::{self},
  vec,
};

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

/// Takes a vector of PathBufs & grammar AST and compiles them into
/// A single GrammarStore. This function assumes the first Grammar is
/// the root grammar.
pub fn compile_grammars_into_store(
  j: &mut Journal,
  grammars: Vec<(PathBuf, ImportedGrammarReferences, Box<Grammar>)>,
) -> SherpaResult<(Option<Arc<GrammarStore>>, Option<Vec<SherpaError>>)> {
  if grammars.is_empty() {
    return SherpaResult::Err(SherpaError::Text("Received empty grammar vector!".to_string()));
  }

  let number_of_threads = get_usable_thread_count(20);
  let results = thread::scope(|s| {
    grammars
      .chunks(grammars.len().div_ceil(number_of_threads))
      .into_iter()
      .map(|chunk| {
        let mut j = j.transfer();
        s.spawn(move || {
          chunk
            .iter()
            .map(|(absolute_path, import_refs, grammar)| {
              let (grammar, errors) = pre_process_grammar(
                &mut j,
                &grammar,
                absolute_path,
                absolute_path
                  .file_stem()
                  .unwrap_or_else(|| OsStr::new("undefined"))
                  .to_str()
                  .unwrap(),
                import_refs.clone(),
              );
              (Arc::new(grammar), errors)
            })
            .collect::<Vec<_>>()
        })
      })
      .map(|s| s.join().unwrap())
      .collect::<Vec<_>>()
  });

  let (mut grammars, mut errors): (Vec<Arc<GrammarStore>>, Vec<Vec<SherpaError>>) =
    results.into_iter().flatten().unzip();

  let mut errors = errors.into_iter().flatten().collect::<Vec<_>>();

  if grammars.is_empty() {
    SherpaResult::Ok((None, Some(errors)))
  } else {
    let rest = grammars.drain(1..).collect::<Vec<_>>();

    let mut grammar = Arc::try_unwrap(grammars.pop().unwrap()).unwrap();

    merge_grammars(j, &mut grammar, &rest, &mut errors);

    if errors.is_empty() {
      let grammar = finalize_grammar(j, grammar, &mut errors, number_of_threads);

      if errors.is_empty() {
        SherpaResult::Ok((Some(Arc::new(grammar)), None))
      } else {
        SherpaResult::MultipleErrors(errors)
      }
    } else {
      SherpaResult::MultipleErrors(errors)
    }
  }
}

#[test]
fn test_compile_grammars_into_store() {
  let mut j = Journal::new(None);
  let (grammars, errors) = load_all(
    &mut j,
    &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.hcg")
      .canonicalize()
      .unwrap(),
    10,
  );

  let result = compile_grammars_into_store(&mut j, grammars);

  assert!(result.is_ok());

  match result {
    SherpaResult::Ok((Some(grammar), _)) => {
      dbg!(grammar);
    }
    SherpaResult::Ok((_, Some(errors))) => {
      for err in errors {
        println!("{}", err);
      }
      panic!("Errors occurred while compiling")
    }
    _ => {}
  }
}

/// Create scanner productions and data caches, and converts ids to tokens.
fn finalize_grammar(
  j: &mut Journal,
  mut g: GrammarStore,
  mut e: &mut Vec<SherpaError>,
  thread_count: usize,
) -> GrammarStore {
  j.set_active_report(
    &format!("Grammar [{}] Compilation", g.id.name),
    ReportType::GrammarCompile(g.id.guid),
  );

  j.report_mut().start_timer("Finalize");

  create_scanner_productions_from_symbols(&mut g, e);

  if check_for_missing_productions(&g, e) || e.have_critical() {
    return g;
  }

  check_for_missing_productions(&g, e);

  finalize_productions(&mut g, thread_count, e);

  finalize_symbols(&mut g, e);

  finalize_items(&mut g, thread_count, e);

  set_parse_productions(&mut g);

  finalize_bytecode_metadata(&mut g, e);

  j.report_mut().stop_timer("Finalize");

  g
}

fn set_parse_productions(g: &mut GrammarStore) {
  let mut productions = g
    .get_exported_productions()
    .iter()
    .map(|ExportedProduction { production, .. }| production.id.clone())
    .collect::<BTreeSet<_>>();
  let mut queue = VecDeque::from_iter(productions.iter().cloned());

  while let Some(prod_id) = queue.pop_front() {
    let mut items = get_production_start_items(&prod_id, g);
    for mut item in items {
      loop {
        if let SymbolID::Production(prod_id, _) = item.get_symbol(g) {
          if productions.insert(prod_id) {
            queue.push_back(prod_id);
          }
        }

        match item.increment() {
          Some(i) => {
            item = i;
          }
          _ => {
            break;
          }
        }
      }
    }
  }

  g.parse_productions = productions;
}

/// Merge related grammars into a single GrammarStore
///
/// `root` is assumed to derived from the root source grammar, and
/// grammars are all other GrammarStores derived from grammars
/// imported directly or indirectly from the root source grammar.
fn merge_grammars(
  j: &mut Journal,
  g: &mut GrammarStore,
  foreign_grammars: &[Arc<GrammarStore>],
  e: &mut Vec<SherpaError>,
) {
  j.set_active_report(
    &format!("Grammar [{}] Compilation", g.id.name),
    ReportType::GrammarCompile(g.id.guid),
  );

  j.report_mut().start_timer("Merge");

  let grammar_lu = HashMap::<GrammarId, Arc<GrammarStore>>::from_iter(
    foreign_grammars.iter().map(|g| (g.id.guid, g.clone())),
  );

  let mut merge_productions =
    g.merge_productions.drain_filter(|_, _| true).collect::<BTreeMap<_, _>>();

  // Merge grammar data into a single store
  for import_grammar in foreign_grammars {
    // Merge ignore symbols
    g.production_ignore_symbols
      .extend(import_grammar.production_ignore_symbols.clone().into_iter());

    // Merge production names
    g.production_names.extend(import_grammar.production_names.clone().into_iter());

    /*
    // Merge symbols
    for (id, sym) in &import_grammar.symbols {
      if !g.symbols.contains_key(id) {
        g.symbols.insert(*id, sym.clone());

        if id.is_defined() {
          match import_grammar.symbol_strings.get(id) {
            Some(string) => {
              g.symbol_strings.insert(*id, string.to_owned());
            }
            None => {}
          }
        }
      }
    }
    */

    // Merge reduce functions
    g.reduce_functions.append(&mut import_grammar.reduce_functions.clone());

    // Collect all pending merge productions
    for (prod_id, rules) in &import_grammar.merge_productions {
      match merge_productions.entry(*prod_id) {
        btree_map::Entry::Occupied(mut e) => {
          e.get_mut().append(&mut rules.clone());
        }
        btree_map::Entry::Vacant(e) => {
          e.insert(rules.clone());
        }
      }
    }
  }

  // Merge all referenced foreign productions into the root.
  let mut symbol_queue = VecDeque::from_iter(g.production_symbols.clone());

  while let Some((sym, tok)) = symbol_queue.pop_front() {
    let syms_grammar = sym.get_grammar_id();
    match (
      syms_grammar,
      grammar_lu.get(&syms_grammar),
      sym.get_production_id().map(|prod_id| g.productions.entry(prod_id)),
    ) {
      (grammar_id, Some(import_g), Some(std::collections::btree_map::Entry::Vacant(entry)))
        if grammar_id != g.id.guid =>
      {
        let prod_id = entry.key().clone();
        match import_g.productions.get(&prod_id) {
          Some(production) => {
            // Import all bodies referenced by this foreign production
            let rules = import_g.production_bodies.get(&prod_id).unwrap().clone();
            for rule in rules.iter().map(|b| import_g.rules.get(&b).unwrap()).cloned() {
              // Add every Production symbol to the queue
              symbol_queue.append(
                &mut rule
                  .syms
                  .iter()
                  .filter_map(|rule_sym| {
                    let sym_id = rule_sym.sym_id;
                    if sym_id.is_defined() || sym_id.is_token_production() {
                      // Ensure the referenced symbol exists in the root grammar.
                      g.symbols
                        .entry(sym_id)
                        .or_insert_with(|| import_g.symbols.get(&sym_id).unwrap().clone());

                      if sym_id.is_defined() {
                        g.symbol_strings
                          .entry(sym_id)
                          .or_insert_with(|| import_g.symbol_strings.get(&sym_id).unwrap().clone());
                      }
                    }

                    match sym_id {
                      SymbolID::Production(..) => Some((sym_id, rule_sym.tok.clone())),
                      SymbolID::TokenProduction(prod_id, grammar_id) => {
                        // Remap the production token symbol to regular a production symbol and
                        // submit as a merge candidate.
                        Some((SymbolID::Production(prod_id, grammar_id), rule_sym.tok.clone()))
                      }
                      _ => None,
                    }
                  })
                  .collect(),
              );

              g.rules.insert(rule.id, rule);
            }

            // Import the mapping of the foreign production_id to the foreign body_ids
            g.production_bodies.insert(prod_id, rules);

            // Import the foreign production
            entry.insert(production.clone());
          }
          None => {
            e.push(SherpaError::grammar_err_no_production_definition {
              prod_name: g.get_production_plain_name(&prod_id).to_string(),
              loc:       tok,
              path:      g.id.path.clone(),
            });
          }
        }
      }
      _ => {}
    }
  }

  // Extend every production in the grammar that has a merge production
  // entry.
  for (prod_id, bodies) in merge_productions.into_iter() {
    match g.productions.contains_key(&prod_id) {
      true => {
        // extend the existing bodies with the new bodies
        let body_count = insert_bodes(g, &prod_id, bodies).len();

        g.productions.get_mut(&prod_id).unwrap().number_of_rules = body_count as u16;
      }
      false => e.push(SherpaError::grammar_err {
        message: "Warning: Attempt to extend a non existent production".to_string(),
        inline_message: "This produces unreachable code".to_string(),
        loc: bodies[0].tok.clone(),
        path: Default::default(),
      }),
    }
  }

  j.report_mut().stop_timer("Merge");
}

fn check_for_missing_productions(g: &GrammarStore, e: &mut Vec<SherpaError>) -> bool {
  let mut have_missing_production = false;
  // Check for missing productions referenced in rule symbols
  for (id, b) in &g.rules {
    for sym in &b.syms {
      match sym.sym_id {
        SymbolID::TokenProduction(prod_id, _) | SymbolID::Production(prod_id, _) => {
          if !g.productions.contains_key(&prod_id) {
            have_missing_production = true;
            e.push(SherpaError::grammar_err_no_production_definition {
              prod_name: g.get_production_plain_name(&prod_id).to_string(),
              loc:       sym.tok.clone(),
              path:      sym.grammar_ref.path.clone(),
            });
          }
        }
        _ => {}
      }
    }
  }
  have_missing_production
}

/// Adds bytecode identifiers to relevant objects.
fn finalize_bytecode_metadata(g: &mut GrammarStore, e: &mut [SherpaError]) {
  let GrammarStore { parse_productions, productions, rules: bodies, .. } = g;

  for (index, rule) in
    bodies.values_mut().filter(|(b)| parse_productions.contains(&b.prod_id)).enumerate()
  {
    rule.bytecode_id = index as u32;
  }

  for (index, prod_id) in parse_productions.iter().enumerate() {
    match productions.get_mut(prod_id) {
      Some(production) => {
        production.bytecode_id = index as u32;
      }
      _ => {}
    }
  }
}

/// Calculates recursion types of productions, converts direct left recursive scanner productions
/// to right recursive.
fn finalize_productions(g: &mut GrammarStore, thread_count: usize, e: &mut [SherpaError]) {
  let production_ids = g.productions.keys().cloned().collect::<Vec<_>>();

  let conversion_candidate = calculate_recursions_types(production_ids, g, thread_count);

  // Convert left recursive scanner productions into right recursion.
  for candidate_id in &conversion_candidate {
    convert_left_recursion_to_right(g, *candidate_id);
  }

  calculate_recursions_types(conversion_candidate, g, thread_count);
}

/// Calculates and assigns the recursion type for each ProductionId in the vector.
///
/// Additionally, a vector of ProductionIds of scanner productions that are [RecursionType::LEFT_DIRECT]  
/// is returned, with the assumption that these productions will be converted to
/// [RecursionType::RIGHT].
fn calculate_recursions_types(
  production_ids: Vec<ProductionId>,
  g: &mut GrammarStore,
  thread_count: usize,
) -> Vec<ProductionId> {
  let mut conversion_candidates = vec![];

  let production_id_chunks =
    production_ids.chunks(thread_count).filter(|s| !s.is_empty()).collect::<Vec<_>>();

  for (production_id, recursion_type) in thread::scope(|s| {
    production_id_chunks
      .iter()
      .map(|work| {
        s.spawn(|| {
          work
            .iter()
            .map(|production_id| (*production_id, g.get_production_recursion_type(*production_id)))
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
      conversion_candidates.push(production.id);
    }
  }

  conversion_candidates
}

/// Creates item closure caches and creates start and goto item groups.
fn finalize_items(g: &mut GrammarStore, thread_count: usize, errors: &mut [SherpaError]) {
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
            let item = item.to_empty_state();

            if !item.completed() {
              let peek_symbols =
                if let Some(peek_symbols) = g.production_ignore_symbols.get(&item.get_prod_id(g)) {
                  peek_symbols.clone()
                } else {
                  vec![]
                };

              results.push((item, create_closure(&[item], g), peek_symbols));

              pending_items.push_back(item.increment().unwrap());
            } else {
              results.push((item, vec![item], vec![]))
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
    g.item_ignore_symbols.insert(item.to_empty_state(), peek_symbols);
    g.closures.insert(item.to_empty_state(), closure);
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

fn finalize_symbols(g: &mut GrammarStore, e: &mut [SherpaError]) {
  let mut symbol_bytecode_id = SymbolID::DefinedSymbolIndexBasis;

  // Ensure there is a symbol for every token production

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
  for prod in g.productions.values_mut() {
    if prod.sym_id.is_token_production() && !g.symbols.contains_key(&prod.sym_id) {
      #[cfg(debug_assertions)]
      {
        debug_assert!(
          prod.id
            == match prod.sym_id {
              SymbolID::Production(prod_id, _) | SymbolID::TokenProduction(prod_id, _) => prod_id,
              _ => ProductionId(0),
            },
          "TokenSymbols prod id does not match production's id",
        );
      }
    }

    prod.symbol_bytecode_id = symbol_bytecode_id;

    symbol_bytecode_id += 1;
  }
}

fn create_scanner_productions_from_symbols(g: &mut GrammarStore, e: &mut Vec<SherpaError>) {
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
        let (_, prod_id, name, symbol_string) = get_scanner_info_from_defined(&sym_id, g);

        convert_scanner_symbol_to_production(
          g,
          &[&"\n".to_string()],
          prod_id,
          name,
          Token::empty(),
        );
      }
      sym if SymbolID::Generics.contains(sym) => {
        // Converts a generic symbol into a scanner production if such a production
        // does not yet exist in the grammar.

        let (_, prod_id, name, symbol_string) = get_scanner_info_from_defined(&sym_id, g);

        if !g.productions.contains_key(&prod_id) {
          // Insert into grammar any new defined symbol derived from
          // token productions.

          insert_production(
            g,
            crate::types::Production {
              id: prod_id,
              guid_name: name.clone(),
              name,
              is_scanner: true,
              sym_id: SymbolID::Production(prod_id, g.id.guid),
              ..Default::default()
            },
            vec![types::Rule {
              len: 1,
              syms: vec![RuleSymbol {
                consumable: true,
                scanner_length: 1,
                sym_id,
                grammar_ref: g.id.clone(),
                ..Default::default()
              }],
              prod_id: prod_id,
              grammar_ref: g.id.clone(),
              ..Default::default()
            }],
          );
        }
      }
      sym if sym.is_defined() => {
        let (_, scanner_production_id, scanner_name, symbol_string) =
          get_scanner_info_from_defined(&sym_id, g);

        let strings = [symbol_string.as_str()];

        convert_scanner_symbol_to_production(
          g,
          &strings,
          scanner_production_id,
          scanner_name,
          Token::empty(),
        );
      }

      // This initially processes token-production symbols dumped in to the
      // queue, but as we process through these productions, we'll inevitably
      // encounter regular production symbols. The process of converting a
      // production symbol into scanner production is identical to that of
      // processing a token-production, so we can just combine the match
      // selectors of TokenProductions symbols and Production symbols.
      SymbolID::Production(prod_id, _) | SymbolID::TokenProduction(prod_id, _) => {
        match g.productions.get(prod_id) {
          Some(production) => {
            let production = production.clone();
            let scanner_name = create_scanner_name(&production.guid_name);
            let scanner_production_id = ProductionId::from(&scanner_name);
            if !g.productions.contains_key(&scanner_production_id) {
              insert_production(
                g,
                crate::types::Production {
                  id: scanner_production_id,
                  guid_name: scanner_name.clone(),
                  name: format!("tk:{}", production.name),
                  loc: production.loc.clone(),
                  is_scanner: true,
                  sym_id: SymbolID::TokenProduction(scanner_production_id, g.id.guid),
                  ..Default::default()
                },
                g.production_bodies
                  .get(prod_id)
                  .unwrap()
                  .iter()
                  .enumerate()
                  .map(|(body_index, rule_id)| {
                    let natural_body = g.rules.get(rule_id).unwrap();

                    let scanner_symbols = natural_body.syms.iter().flat_map(|sym| {
                      let sym_id = &sym.sym_id;
                      match sym_id {
                        // For any production or token production symbol encountered, create
                        // a new symbol that references the equivalent scanner production name,
                        // and submit this production for conversion into a scanner production.
                        SymbolID::Production(prod_id, grammar_id)
                        | SymbolID::TokenProduction(prod_id, grammar_id) => {
                          let production = g.productions.get(prod_id).unwrap();

                          let scanner_name = create_scanner_name(&production.guid_name);

                          let scanner_production_id = ProductionId::from(&scanner_name);

                          let new_symbol_id =
                            SymbolID::Production(scanner_production_id, *grammar_id);

                          scanner_production_queue.push_back(*sym_id);

                          vec![RuleSymbol {
                            consumable: true,
                            exclusive: sym.exclusive,
                            scanner_length: 1,
                            sym_id: new_symbol_id,
                            grammar_ref: g.id.clone(),
                            ..Default::default()
                          }]
                        }
                        sym if sym.is_defined() => {
                          let (new_symbol_id, ..) = get_scanner_info_from_defined(sym_id, g);

                          scanner_production_queue.push_back(*sym_id);

                          vec![RuleSymbol {
                            consumable: true,
                            exclusive: sym.is_exclusive(),
                            scanner_length: 1,
                            sym_id: new_symbol_id,
                            grammar_ref: g.id.clone(),
                            ..Default::default()
                          }]
                        }
                        _ => vec![sym.clone()],
                      }
                    });

                    let syms: Vec<RuleSymbol> = scanner_symbols.collect();

                    Rule {
                      id: RuleId::new(&scanner_production_id, body_index),
                      len: syms.len() as u16,
                      prod_id: scanner_production_id,
                      syms,
                      tok: production.loc.clone(),
                      ..Default::default()
                    }
                  })
                  .collect(),
              );
            }
          }
          _ => {
            let sym = g.symbols.get(&sym_id).unwrap();
            e.push(SherpaError::grammar_err_no_production_definition {
              prod_name: g.get_production_plain_name(&prod_id).to_string(),
              loc:       sym.loc.clone(),
              path:      sym.g_ref.clone().unwrap_or(g.id.clone()).path.clone(),
            });
          }
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
  name: String,
  origin_location: Token,
) {
  if !g.productions.contains_key(&prod_id) {
    let bodies = strings
      .iter()
      .map(|symbol_string| {
        // Insert into grammar any new defined symbol derived from
        // token productions.
        let new_body_symbols =
          create_defined_symbols_from_string(g, symbol_string, origin_location.clone());
        Rule {
          len: new_body_symbols.len() as u16,
          syms: new_body_symbols,
          prod_id,
          tok: origin_location.clone(),
          ..Default::default()
        }
      })
      .collect();

    insert_production(
      g,
      crate::types::Production {
        id: prod_id,
        name: name.clone(),
        guid_name: name,
        is_scanner: true,
        sym_id: SymbolID::TokenProduction(prod_id, g.id.guid),
        ..Default::default()
      },
      bodies,
    );
  }
}

/// Converts a string sequence into a set of BodySymbolRef references,
/// interning whatever single byte symbol is not already present in the
/// grammar. TODO: This may also split utf8 symbols into byte sequences.
///
/// Expects `symbols_string_table` and `symbols_table` to be mutable
/// references to the corresponding members in a [GrammarStore]
fn create_defined_symbols_from_string(
  g: &mut GrammarStore,
  symbol_string: &str,
  loc: Token,
) -> Vec<RuleSymbol> {
  let chars: Vec<char> = symbol_string.chars().collect();

  let new_body_symbols: Vec<RuleSymbol> = chars
    .iter()
    .enumerate()
    .map(|(index, byte)| {
      let string = byte.to_string();

      let id = get_literal_id(&string, false);

      g.symbols.entry(id).or_insert_with(|| {
        g.symbol_strings.insert(id, string);

        Symbol {
          byte_length:   byte.len_utf8() as u32,
          cp_len:        1,
          bytecode_id:   0,
          guid:          id,
          scanner_only:  true,
          friendly_name: id.to_default_string(),
          loc:           loc.clone(),
          g_ref:         Some(g.id.clone()),
        }
      });

      RuleSymbol {
        consumable: true,
        scanner_index: index as u32,
        scanner_length: chars.len() as u32,
        sym_id: id,
        grammar_ref: g.id.clone(),
        ..Default::default()
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
        .map(|c| format!("{:X}", (c as u32)))
        .collect::<Vec<_>>()
        .join("");

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
  let new_symbol_id = SymbolID::Production(scanner_production_id, root.id.guid);

  (new_symbol_id, scanner_production_id, scanner_name, symbol_string)
}

/// Takes a Grammar produces core primitive tables;
///
/// ## Arguments
///
/// - `grammar` - A hcg AST node
/// - `source` - The source string of the hcg.
/// - `path` - The absolute path of the hcg's source file
///   Used to resolve linked grammars.
///  

pub fn pre_process_grammar<'a>(
  j: &mut Journal,
  ast: &'a ast::Grammar,
  path: &PathBuf,
  name: &'a str,
  imports: ImportedGrammarReferences,
) -> (GrammarStore, Vec<SherpaError>) {
  let mut g = GrammarStore {
    id: GrammarRef::new(name.to_string(), path.clone()),
    imports,
    ..Default::default()
  };

  j.set_active_report(
    &format!("Grammar [{}] Compilation", name),
    ReportType::GrammarCompile(g.id.guid),
  );

  j.report_mut().start_timer("Compile Time");

  let mut global_ignore_symbols = vec![];
  let mut e = vec![];
  let mut post_process_productions: VecDeque<Box<ast::Production>> = VecDeque::new();

  {
    // Process meta data, including EXPORT, IMPORT, and IGNORE meta
    // data
    for obj in ast.preamble.iter() {
      match obj {
        ASTNode::Name(box ast::Name { name }) => {
          g.id = GrammarRef::new(name.to_string(), path.clone());
        }
        ASTNode::Ignore(box ast::Ignore { symbols }) => {
          for symbol in symbols {
            if let Some(id) = intern_symbol(symbol, &mut g, &mut e) {
              global_ignore_symbols.push(id)
            }
          }
        }
        ASTNode::Export(box ast::Export { production, reference }) => {
          let id = get_prod_id(production, &mut g, &mut e);
          g.exports.push((id, reference.to_string()));
        }
        _ => {}
      }
    }

    // Process main grammar data, which include
    // Productions, IR states, and out of band functions
    for node in ast.content.iter() {
      match node {
        ASTNode::Production(_) => {
          let production =
            pre_process_production(node, &mut g, &mut post_process_productions, &mut e);
          if production != ProductionId::default() && g.exports.is_empty() {
            g.exports.push((production, "default".to_string()));
          }
        }
        ASTNode::ProductionMerged(prod) => {
          // - Generate a production id
          // - Create the merge bodies and intern them in the grammar.
          // - When merging, if existing productions exist in the target

          let (prod_id, ..) = get_production_identifiers(&node, &mut g, &mut e);
          let mut list_index = 0;
          for rule in &prod.bodies {
            match rule {
              ASTNode::Body(rule) => {
                let (mut rules, _) = pre_process_body(&node, rule, &mut g, &mut list_index, &mut e);

                match g.merge_productions.entry(prod_id) {
                  btree_map::Entry::Vacant(e) => {
                    e.insert(rules);
                  }
                  btree_map::Entry::Occupied(mut e) => {
                    e.get_mut().append(&mut rules);
                  }
                };
              }
              _ => {}
            }
          }
        }
        _ => {}
      }
    }

    // Continue processing any generated _. This may loop
    // for a while as any given production may have several nested
    // anonymous productions through lists `...(+) | ...(*)` and
    // groups `(... | ...)`

    while let Some(node) = post_process_productions.pop_front() {
      pre_process_production(
        &ASTNode::Production(node),
        &mut g,
        &mut post_process_productions,
        &mut e,
      );
    }
  }

  g.production_ignore_symbols =
    g.productions.keys().map(|k| (*k, global_ignore_symbols.clone())).collect::<HashMap<_, _>>();

  j.report_mut().stop_timer("Compile Time");

  (g, e)
}

#[test]
fn test_pre_process_grammar() {
  let mut j = Journal::new(None);
  let grammar = String::from(
      "\n@IMPORT ./test/me/out.hcg as bob 
      <> a > tk:p?^test a(+,) ( \\1234 | t:sp? ( sp | g:sym g:sp ) f:r { basalt } ) \\nto <> b > tk:p p ",
  );

  if let Ok(grammar) = compile_grammar_ast(Vec::from(grammar.as_bytes())) {
    let (_, errors) =
      pre_process_grammar(&mut j, &grammar, &PathBuf::from("/test"), "test", Default::default());

    for error in &errors {
      eprintln!("{}", error);
    }

    assert_eq!(errors.len(), 1);
  } else {
    panic!("Failed to parse and produce an AST of '<> a > b'");
  }
}

///
fn pre_process_production(
  production_node: &ASTNode,
  g: &mut GrammarStore,
  post_process_productions: &mut VecDeque<Box<ast::Production>>,
  e: &mut Vec<SherpaError>,
) -> ProductionId {
  let (prod_id, guid_name, plain_name) = get_production_identifiers(production_node, g, e);
  match (
    prod_id,
    get_production_symbol(production_node, g),
    production_node,
    g.productions.get(&prod_id),
  ) {
    (_, Some(ASTNode::Production_Import_Symbol(_)), ..) => {
      e.push(SherpaError::grammar_err {
        inline_message: "Invalid production definition".to_string(),
        loc: production_node.Token(),
        message: format!("Cannot define a production of an imported grammar.\n     note: Try using the `+>` operator to extend an existing production with additional bodies."),
        path: g.id.path.clone(),
      });
      prod_id
    }
    (prod, _, ..) if prod == Default::default() => {
      e.push(SherpaError::grammar_err {
        inline_message: String::new(),
        loc: production_node.Token(),
        message: format!("This is not a valid production"),
        path: g.id.path.clone(),
      });
      prod_id
    }
    (_, _, ASTNode::Production(prod), None) => {
      // Extract rule data and gather symbol information
      let mut list_index = 0;
      let bodies = prod
        .bodies
        .iter()
        .flat_map(|rule| match rule {
          ASTNode::Body(ast_body) => {
            let (new_bodies, productions) =
              pre_process_body(production_node, ast_body, g, &mut list_index, e);

            for prod in productions {
              post_process_productions.push_back(prod);
            }

            new_bodies
          }
          _ => vec![],
        })
        .collect();

      insert_production(
        g,
        Production {
          id: prod_id,
          guid_name,
          name: plain_name, // prod.symbol.Token().to_string()
          loc: production_node.Token(),
          sym_id: SymbolID::Production(prod_id, g.id.guid),
          ..Default::default()
        },
        bodies,
      );

      prod_id
    }
    (_, _, node, Some(existing_production)) => {
      e.push({
        SherpaError::grammar_err_multi_location {
          message:   format!("production {} already exists!", plain_name),
          locations: vec![
            SherpaError::grammar_err {
              inline_message: String::new(),
              loc: node.Token(),
              message: format!("Redefinition of {} occurs here.", plain_name),
              path: g.id.path.clone(),
            },
            SherpaError::grammar_err {
              inline_message: String::new(),
              loc: existing_production.loc.clone(),
              message: format!("production {} first defined here.", plain_name),
              path: g.id.path.clone(),
            },
          ],
        }
      });
      ProductionId::default()
    }
    _ => ProductionId::default(),
  }
}

fn get_production_symbol<'a>(node: &'a ASTNode, g: &'a GrammarStore) -> Option<&'a ASTNode> {
  match node {
    ASTNode::Production_Import_Symbol(..) | ASTNode::Production_Symbol(..) => Some(node),
    ASTNode::Production(prod) => get_production_symbol(&prod.symbol, g),
    ASTNode::ProductionMerged(prod) => get_production_symbol(&prod.symbol, g),
    ASTNode::Production_Token(prod_tok) => get_production_symbol(&prod_tok.production, g),
    _ => None,
  }
}

/// Get the guid_name and plain_name of the production resolved from an ASTNode.
/// Nodes from which a production name can be derived:
/// - Production_Symbol
/// - Production_Token
/// - Production
/// - Import_Production
///
/// Returns `Option<(guid_name: String, friendly_name: String)>`
///
/// This guid_name is guaranteed to be unique amongst all grammars imported by
/// the root grammar.
fn get_productions_names(
  node: &ASTNode,
  g: &GrammarStore,
  e: &mut Vec<SherpaError>,
) -> Option<(String, String)> {
  match get_production_symbol(node, g) {
    Some(ASTNode::Production_Import_Symbol(prod_imp_sym)) => {
      let production_name = &prod_imp_sym.name;
      let local_import_grammar_name = &prod_imp_sym.module;

      match g.imports.get(local_import_grammar_name) {
        None => {
          e.push(
            SherpaError::grammar_err {
              inline_message: String::new(),
              message: format!(
                  "Unable to resolve production: The production \u{001b}[31m{}\u{001b}[0m cannot be found in the imported grammar \u{001b}[31m{}\u{001b}[0m.", 
                  production_name,
                  local_import_grammar_name
              ),
              loc: node.Token(),
              path: g.id.path.clone(),
          });
          None
        }
        Some(g_ref) => Some((
          create_production_guid_name(&g_ref.guid_name, production_name),
          prod_imp_sym.Token().to_string(),
        )),
      }
    }
    Some(ASTNode::Production_Symbol(prod_sym)) => {
      Some((create_production_guid_name(&g.id.guid_name, &prod_sym.name), prod_sym.name.clone()))
    }
    _ => {
      e.push(SherpaError::grammar_err {
        inline_message: String::new(),
        message: "Unexpected node: Unable to resolve production name of this node!".to_string(),
        loc: node.Token(),
        path: g.id.path.clone(),
      });
      None
    }
  }
}

/// Get the resolved grammar data from compatible nodes.
///
/// ASTNodes that can be resolved to a grammar:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::ProductionMerged]
/// - [ASTNode::Production_Token]
///
/// ## Returns
/// A Tuple comprised of the grammar 0:uuid_name, 1:local_name, and
/// 2:absolute_path. local_name is `root` if the grammar maps to
/// currently rendered grammar.

fn get_grammar_info_from_node<'a>(
  node: &'a ASTNode,
  g: &'a GrammarStore,
  e: &mut Vec<SherpaError>,
) -> Option<Arc<GrammarRef>> {
  match get_production_symbol(node, g) {
    Some(ASTNode::Production_Import_Symbol(prod_imp_sym)) => {
      let local_import_grammar_name = &prod_imp_sym.module;

      match g.imports.get(local_import_grammar_name) {
        None => {
          e.push(SherpaError::grammar_err {
            inline_message: String::new(),
            message: format!(
              "Unknown Grammar : The local grammar name \u{001b}[31m{}\u{001b}[0m does not match any imported grammar.",
              local_import_grammar_name
            ),
            loc: node.Token(),path: g.id.path.clone(),
          });
          None
        }
        Some(g_ref) => Some(g_ref.clone()),
      }
    }
    Some(ASTNode::Production_Symbol(_)) => Some(g.id.clone()),
    _ => {
      e.push(SherpaError::grammar_err {
        inline_message: String::new(),
        message: "Unexpected node: Unable to resolve production name of this node!".to_string(),
        loc: node.Token(),
        path: g.id.path.clone(),
      });

      None
    }
  }
}

/// Returns the ProductionId, guid name String, and the normal name String of a production symbol.
/// Returns default values if the node cannot be resolved to a production symbol.
///
/// ASTNodes that can be resolved to a production symbol:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::ProductionMerged]
/// - [ASTNode::Production_Token]
#[inline]
fn get_production_identifiers(
  node: &ASTNode,
  g: &mut GrammarStore,
  e: &mut Vec<SherpaError>,
) -> (ProductionId, String, String) {
  match get_productions_names(node, g, e) {
    Some((guid_name, plain_name)) => {
      let id = ProductionId::from(&guid_name);

      g.production_names.try_insert(id, (plain_name.clone(), guid_name.clone()));

      (id, guid_name, plain_name)
    }
    _ => (Default::default(), Default::default(), Default::default()),
  }
}

/// Returns the ProductionId  of a production symbol.
/// Returns an empty ProductionId if the node cannot be resolved to a production symbol.
///
/// ASTNodes that can be resolved to a production symbol:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::ProductionMerged]
/// - [ASTNode::Production_Token]
#[inline]
fn get_prod_id(
  production: &ASTNode,
  g: &mut GrammarStore,
  e: &mut Vec<SherpaError>,
) -> ProductionId {
  get_production_identifiers(production, g, e).0
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
pub fn convert_left_recursion_to_right(
  g: &mut GrammarStore,
  a_prod_id: ProductionId,
) -> (ProductionId, ProductionId) {
  let a_prod = g.productions.get_mut(&a_prod_id).unwrap();

  let a_token = a_prod.loc.clone();

  // Ensure the production is left recursive.
  if !a_prod.recursion_type.contains(RecursionType::LEFT_DIRECT) {
    panic!("Production is not left direct recursive.");
  }

  // Remove recursion flag as it no longer applies to this production.
  a_prod.recursion_type = a_prod.recursion_type.xor(RecursionType::LEFT_DIRECT);

  let rule_ids = g.production_bodies.get(&a_prod_id).unwrap().clone();

  let rules = rule_ids
    .iter()
    .map(|rule_id| g.rules.get(rule_id).unwrap())
    .map(|b| match b.syms[0].sym_id {
      SymbolID::Production(p, _) => (b.id, p == a_prod.id),
      SymbolID::TokenProduction(p, _) => (b.id, p == a_prod.id),
      _ => (b.id, false),
    })
    .collect::<Vec<_>>();

  let left_rules =
    rules.iter().filter_map(|(i, b)| if *b { Some(i) } else { None }).collect::<Vec<_>>();

  let non_rules =
    rules.iter().filter_map(|(i, b)| if *b { None } else { Some(i) }).collect::<Vec<_>>();

  let a_prime_prod_name = format!("{}{}", a_prod.name, prime_symbol);
  let a_prime_prod_guid_name = format!("{}{}", a_prod.guid_name, prime_symbol);
  let a_prime_prod_id = ProductionId::from(&a_prime_prod_guid_name);
  let a_prime_prod = Production {
    id: a_prime_prod_id,
    guid_name: a_prime_prod_guid_name,
    name: a_prime_prod_name,
    number_of_rules: (left_rules.len() * 2) as u16,
    loc: a_token.clone(),
    is_scanner: a_prod.is_scanner,
    sym_id: if a_prod.is_scanner {
      SymbolID::TokenProduction(a_prime_prod_id, g.id.guid)
    } else {
      SymbolID::Production(a_prime_prod_id, g.id.guid)
    },
    ..Default::default()
  };

  let a_prim_sym = RuleSymbol {
    sym_id: SymbolID::Production(a_prime_prod_id, g.id.guid),
    consumable: true,
    tok: a_token.clone(),
    grammar_ref: g.id.clone(),
    ..Default::default()
  };

  let new_B_rules = non_rules
    .iter()
    .enumerate()
    .flat_map(|(i, b)| {
      let rule = g.rules.get(b).unwrap();

      let mut rule_a = rule.clone();
      let mut rule_b = rule.clone();

      rule_b.syms.push(a_prim_sym.clone());
      rule_a.id = RuleId::new(&a_prod_id, i * 2);
      rule_b.id = RuleId::new(&a_prod_id, i * 2 + 1);
      rule_a.len = rule_a.syms.len() as u16;
      rule_b.len = rule_b.syms.len() as u16;

      vec![rule_a, rule_b]
    })
    .collect::<Vec<_>>();

  let new_A_rules = left_rules
    .iter()
    .enumerate()
    .flat_map(|(i, b)| {
      let rule = g.rules.get(b).unwrap();
      let mut rule_a = rule.clone();
      let mut rule_b = rule.clone();
      rule_a.prod_id = a_prime_prod_id;
      rule_b.prod_id = a_prime_prod_id;
      rule_a.syms.remove(0);
      rule_b.syms.remove(0);
      rule_b.syms.push(a_prim_sym.clone());
      rule_a.id = RuleId::new(&a_prime_prod_id, i * 2);
      rule_b.id = RuleId::new(&a_prime_prod_id, i * 2 + 1);
      rule_a.len = rule_a.syms.len() as u16;
      rule_b.len = rule_b.syms.len() as u16;
      vec![rule_a, rule_b]
    })
    .collect::<Vec<_>>();

  // Replace the base production's bodies with B bodies.
  // Add A prime production to the grammar.
  g.production_names
    .try_insert(a_prime_prod_id, (a_prime_prod.name.clone(), a_prime_prod.guid_name.clone()));
  g.productions.insert(a_prime_prod_id, a_prime_prod);
  g.production_bodies.insert(a_prod_id, new_B_rules.iter().map(|b| b.id).collect::<Vec<_>>());
  g.production_bodies.insert(a_prime_prod_id, new_A_rules.iter().map(|b| b.id).collect::<Vec<_>>());

  for b in new_A_rules {
    let id = b.id;
    g.rules.insert(id, b);
  }

  for b in new_B_rules {
    let id = b.id;
    g.rules.insert(id, b);
  }

  (a_prod_id, a_prime_prod_id)
}

fn create_ast_production(
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

fn create_body_vectors(
  token: &Token,
  symbols: &Vec<(usize, &ASTNode)>,
  production_name: &String,
  g: &mut GrammarStore,
  list_index: &mut u32,
  e: &mut Vec<SherpaError>,
) -> (Vec<(Token, Vec<RuleSymbol>)>, Vec<Box<ast::Production>>) {
  let mut rules = vec![];
  let mut productions = vec![];

  rules.push((token.clone(), vec![]));

  for (index, sym) in symbols {
    let original_bodies = 0..rules.len();

    let SymbolData {
      annotation,
      is_list,
      is_group,
      is_optional,
      is_shift_nothing,
      is_meta,
      is_exclusive,
      sym_atom,
    } = get_symbol_details(sym, g, e);

    if let Some(mut sym) = sym_atom.to_owned() {
      let mut generated_symbol = ASTNode::NONE;

      if is_meta {
        // TODO: Separate meta data symbols into it's own table that
        // maps meta symbols to a rule and its
        // index.
        continue;
      }

      if is_optional {
        // Need to create new bodies that contains all permutations
        // of encountered symbols except for the currently
        // considered symbol. This is achieved by duplicating all
        // rule vectors, then adding the current symbol to the
        // original vectors, but not the duplicates.
        for entry in rules.clone() {
          rules.push(entry)
        }
      }
      if let ASTNode::AnyGroup(group) = sym {
        // New bodies are created with the values of the any group
        // symbol being distributed to each rule.

        let mut pending_rules = vec![];

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
            create_body_vectors(token, &symbols, production_name, g, list_index, e);

          pending_rules.append(&mut new_bodies);

          productions.append(&mut new_productions);
        }

        let mut new_rules = vec![];

        for pending_rule in pending_rules {
          if pending_rule.1.len() == 0 {
            continue;
          }

          for rule in &mut rules[original_bodies.clone()] {
            let mut new_body = rule.clone();
            new_body.1.extend(pending_rule.1.iter().cloned());
            new_rules.push(new_body)
          }
        }

        rules = new_rules;

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
            // For each rule in the group clone the existing rule lists and
            // process each list independently, inserting the new symbols
            // into the existing bodies. We must make sure the indices are
            // preserved since only the last symbol in each rule can be bound
            // to the index of the group production symbol.

            let mut pending_bodies = vec![];

            for rule in &group.bodies {
              if let ASTNode::Body(rule) = rule {
                let (mut new_bodies, mut new_productions) = create_body_vectors(
                  &rule.Token(),
                  &rule.symbols.iter().map(|s| (9999, s)).collect(),
                  production_name,
                  g,
                  list_index,
                  e,
                );
                // The last symbol in each of these new bodies is set
                // with the original symbol id

                for rule in &mut new_bodies {
                  rule.1.last_mut().unwrap().original_index = *index as u32;
                }

                pending_bodies.append(&mut new_bodies);
                productions.append(&mut new_productions);
              }
            }

            let mut new_bodies = vec![];

            for pending_body in pending_bodies {
              for rule in &mut rules[original_bodies.clone()] {
                let mut new_body = rule.clone();
                new_body.1.extend(pending_body.1.iter().cloned());
                new_bodies.push(new_body)
              }
            }

            rules.splice(original_bodies, new_bodies);

            // We do not to process the existing symbol as it is
            // now replaced with its component rule symbols,
            // so we'll skip the rest of the loop
            continue;
          } else {
            let (prod_sym, production) = create_ast_production(
              &(production_name.to_owned() + "_group_" + &index.to_string()),
              &group.bodies,
              group.tok.clone(),
            );

            productions.push(production);
            generated_symbol = prod_sym;
            sym = &generated_symbol;
          }
        } else {
          e.push(SherpaError::grammar_err {
            inline_message: String::new(),
            message: "I don't know what to do with this.".to_string(),
            loc: sym.Token(),
            path: g.id.path.clone(),
          });
        }
      } else if is_list {
        // Create a new production that turns
        // `A => a` into `A => a | A => A a`
        // and produce a SymbolId that points to that production.

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
              symbol.Token(),
            );

            let mut body_b = body_a.clone();
            body_b.tok = sym.Token();

            let (list_vector_reduce, list_symbol_reduce) = (
              compile_ascript_ast("[$first, $last]".as_bytes().to_vec()).unwrap(),
              compile_ascript_ast("[$first]".as_bytes().to_vec()).unwrap(),
            );

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

            (*list_index) += 1;

            let (prod_sym, mut production) = create_ast_production(
              &(production_name.to_owned() + "_list_" + &(*list_index).to_string()),
              &[ASTNode::Body(body_b), ASTNode::Body(body_a)],
              tok.clone(),
            );

            // Add the production symbol to the front of the rule
            // to make the production left recursive
            if let ASTNode::Body(rule) = &mut production.bodies[0] {
              rule.symbols.insert(0, prod_sym.clone());
            }

            productions.push(production);
            generated_symbol = prod_sym;
            sym = &generated_symbol;
          }
          _ => {
            e.push(SherpaError::grammar_err {
              inline_message: String::new(),
              message: "I don't know what to do with this.".to_string(),
              loc: sym.Token(),
              path: g.id.path.clone(),
            });
          }
        }
      }

      if let Some(id) = intern_symbol(sym, g, e) {
        for (_, vec) in &mut rules[original_bodies] {
          vec.push(RuleSymbol {
            original_index: *index as u32,
            sym_id: id,
            annotation: annotation.clone(),
            consumable: !is_shift_nothing,
            exclusive: is_exclusive,
            tok: sym.Token(),
            grammar_ref: g.id.clone(),
            ..Default::default()
          });
        }
      }
    }
  }

  (rules, productions)
}

fn pre_process_body(
  production: &ASTNode,
  rule: &ast::Body,
  g: &mut GrammarStore,
  list_index: &mut u32,
  e: &mut Vec<SherpaError>,
) -> (Vec<types::Rule>, Vec<Box<ast::Production>>) {
  match get_productions_names(production, g, e) {
    Some((_, prod_name)) => {
      let (bodies, productions) = create_body_vectors(
        &rule.Token(),
        &rule
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
        &prod_name,
        g,
        list_index,
        e,
      );

      let reduce_fn_ids = match rule.reduce_function {
        ASTNode::Reduce(..) | ASTNode::Ascript(..) => {
          let reduce_id = ReduceFunctionId::new(&rule.reduce_function);

          g.reduce_functions
            .entry(reduce_id)
            .or_insert_with(|| ReduceFunctionType::new(&rule.reduce_function));

          vec![reduce_id]
        }
        _ => vec![],
      };

      let mut unique_bodies = vec![];
      let mut seen = HashSet::new();

      for (t, b) in bodies {
        let sym = RuleId::from_syms(&b.iter().map(|s| s.sym_id).collect::<Vec<_>>());
        if !seen.contains(&sym) {
          unique_bodies.push(types::Rule {
            syms: b.clone(),
            len: b.len() as u16,
            prod_id: get_prod_id(production, g, e),
            reduce_fn_ids: reduce_fn_ids.clone(),
            tok: t.clone(),
            grammar_ref: g.id.clone(),
            ..Default::default()
          });
          seen.insert(sym);
        }
      }

      (unique_bodies, productions)
    }
    _ => (Default::default(), Default::default()),
  }
}

fn some_bodies_have_reduce_functions(rules: &Vec<ASTNode>) -> bool {
  rules.iter().any(|b| match b {
    ASTNode::Body(rule) => rule.reduce_function.GetType() != 0,
    _ => false,
  })
}

lazy_static::lazy_static! {
  static ref identifier_re: Regex = Regex::new(r"[\w_-][\w\d_-]*$").unwrap();
  static ref number_re: Regex = Regex::new(r"\d+$").unwrap();
}

/// Returns an appropriate SymbolID::Defined* based on the input
/// string
#[inline]
fn get_literal_id(string: &String, exclusive: bool) -> SymbolID {
  match (exclusive, number_re.is_match(string), identifier_re.is_match(string)) {
    (true, true, false) => SymbolID::ExclusiveDefinedNumeric(StringId::from(string)),
    (false, true, false) => SymbolID::DefinedNumeric(StringId::from(string)),
    (true, false, true) => SymbolID::ExclusiveDefinedIdentifier(StringId::from(string)),
    (false, false, true) => SymbolID::DefinedIdentifier(StringId::from(string)),
    (true, ..) => SymbolID::ExclusiveDefinedSymbol(StringId::from(string)),
    (false, ..) => SymbolID::DefinedSymbol(StringId::from(string)),
  }
}

fn process_token_production(
  node: &ast::Production_Token,
  g: &mut GrammarStore,
  tok: Token,
  e: &mut Vec<SherpaError>,
) -> Option<SymbolID> {
  match process_production(&node.production, g, tok.clone(), e) {
    Some(SymbolID::Production(prod_id, grammar_id)) => {
      let id = SymbolID::Production(prod_id, grammar_id);
      let tok_id = SymbolID::TokenProduction(prod_id, grammar_id);

      g.production_symbols.insert(id, tok.clone());
      g.production_symbols.insert(tok_id, tok.clone());

      g.symbols.entry(tok_id).or_insert(Symbol {
        bytecode_id:   0,
        guid:          id,
        byte_length:   0,
        cp_len:        0,
        scanner_only:  false,
        friendly_name: String::new(),
        loc:           tok.clone(),
        g_ref:         Some(g.id.clone()),
      });

      Some(tok_id)
    }
    _ => None,
  }
}

fn process_production(
  node: &ASTNode,
  g: &mut GrammarStore,
  tok: Token,
  e: &mut Vec<SherpaError>,
) -> Option<SymbolID> {
  match node {
    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      let production_id = get_prod_id(node, g, e);

      match get_grammar_info_from_node(node, g, e)
        .map(|data| SymbolID::Production(production_id, data.guid))
      {
        Some(id) => {
          g.production_symbols.insert(id, tok);
          Some(id)
        }
        _ => None,
      }
    }
    _ => {
      e.push(SherpaError::grammar_err {
        inline_message: "This is not a hashable production symbol.".to_string(),
        message: "[INTERNAL ERROR]".to_string(),
        loc: node.Token(),
        path: g.id.path.clone(),
      });
      None
    }
  }
}

fn process_literal(
  string: &String,
  g: &mut GrammarStore,
  is_exclusive: bool,
  loc: Token,
) -> SymbolID {
  let mut id = get_literal_id(string, is_exclusive);

  if let std::collections::btree_map::Entry::Vacant(e) = g.symbols.entry(id) {
    g.symbol_strings.insert(id, string.to_owned());

    let byte_length = string.bytes().len() as u32;
    let code_point_length = string.chars().count() as u32;

    e.insert(Symbol {
      bytecode_id: 0,
      guid: id,
      byte_length,
      cp_len: code_point_length,
      scanner_only: false,
      friendly_name: String::new(),
      loc,
      g_ref: Some(g.id.clone()),
    });
  }

  id
}

/// Adds a symbol to the symbol_table
fn intern_symbol(
  sym: &ASTNode,
  g: &mut GrammarStore,
  e: &mut Vec<SherpaError>,
) -> Option<SymbolID> {
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
    ASTNode::Exclusive_Literal(literal) => {
      Some(process_literal(&literal.val, g, true, sym.Token()))
    }
    ASTNode::Literal(literal) => Some(process_literal(&literal.val, g, false, sym.Token())),
    ASTNode::End_Of_File(_) => Some(SymbolID::EndOfInput),
    ASTNode::Production_Symbol(_) | ASTNode::Production_Import_Symbol(_) => {
      process_production(sym, g, sym.Token(), e)
    }
    ASTNode::Production_Token(token) => process_token_production(token, g, sym.Token(), e),
    _ => {
      e.push(SherpaError::grammar_err {
        inline_message: "Unexpected ASTNode while attempting to intern symbol".to_string(),
        message: "[INTERNAL ERROR]".to_string(),
        loc: sym.Token(),
        path: g.id.path.clone(),
      });
      None
    }
  }
}

/// Get a flattened view of a symbol's immediate AST
fn get_symbol_details<'a>(
  mut sym: &'a ASTNode,
  g: &mut GrammarStore,
  e: &mut Vec<SherpaError>,
) -> SymbolData<'a> {
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
        e.push(SherpaError::grammar_err {
          inline_message: format!("Unexpected ASTNode {}", sym.GetType()),
          message: "[INTERNAL ERROR]".to_string(),
          loc: sym.Token(),
          path: g.id.path.clone(),
        });
        break;
      }
    }
  }

  data.sym_atom = Some(sym);

  data
}

#[inline]
fn insert_production(g: &mut GrammarStore, mut prod: Production, bodies: Vec<types::Rule>) {
  let prod_id = prod.id;

  prod.number_of_rules = insert_bodes(g, &prod_id, bodies).len() as u16;

  g.productions.insert(prod_id, prod);
}

#[inline]
fn insert_bodes(
  g: &mut GrammarStore,
  prod_id: &ProductionId,
  bodies: Vec<types::Rule>,
) -> Vec<RuleId> {
  let offset_index = g.production_bodies.get(&prod_id).map_or(0, |b| b.len());

  let body_ids = bodies
    .into_iter()
    .enumerate()
    .map(|(i, mut b)| {
      let id = RuleId::new(&prod_id, offset_index + i);
      b.id = id;
      g.rules.insert(id, b);
      id
    })
    .collect::<Vec<_>>();

  match g.production_bodies.entry(*prod_id) {
    btree_map::Entry::Vacant(e) => {
      e.insert(body_ids.clone());
    }
    btree_map::Entry::Occupied(mut e) => {
      e.get_mut().append(&mut body_ids.clone());
    }
  };

  g.production_bodies.get(prod_id).unwrap().to_owned()
}

/// Loads and compiles a grammar from a source file.
///
/// # Example
///
/// Basic usage:
/// ```
///  # use std::path::PathBuf;
/// use sherpa_core::compile_grammar_from_path;
///
/// let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
///
/// path.push("../../../test/grammars/trivial.hcg");
///
/// let (grammar, errors) = compile_grammar_from_path(path, 10);
///
/// assert_eq!(grammar.unwrap().name, "trivial")
/// ```
pub fn compile_grammar_from_path(
  j: &mut Journal,
  path: PathBuf,
  thread_count: usize,
) -> (Option<Arc<GrammarStore>>, Option<Vec<SherpaError>>) {
  let r_type = ReportType::GrammarCompile(Default::default());
  let r_name = "General Grammar Compile";
  j.set_active_report(r_name, r_type);
  j.report_mut().start_timer("Grammar Compile Time");
  j.report_mut().add_note("Root Grammar Path", path.to_str().unwrap().to_string());
  match load_all(j, &path, thread_count) {
    (_, errors) if !errors.is_empty() => {
      j.set_active_report(r_name, r_type);
      j.report_mut().stop_timer("Grammar Compile Time");
      for error in errors {
        j.report_mut().add_error(error);
      }
      (None, None)
    }
    (grammars, _) => {
      j.set_active_report(r_name, r_type);
      j.report_mut().stop_timer("Grammar Compile Time");
      match compile_grammars_into_store(j, grammars) {
        SherpaResult::Err(err) => {
          j.set_active_report(r_name, r_type);
          j.report_mut().stop_timer("Grammar Compile Time");
          j.report_mut().add_error(err);
          (None, None)
        }
        SherpaResult::MultipleErrors(errors) => {
          j.set_active_report(r_name, r_type);
          j.report_mut().stop_timer("Grammar Compile Time");
          for error in errors {
            j.report_mut().add_error(error);
          }
          (None, None)
        }
        SherpaResult::None => {
          j.report_mut().stop_timer("Grammar Compile Time");
          (None, None)
        }
        SherpaResult::Ok(result) => {
          j.report_mut().stop_timer("Grammar Compile Time");
          result
        }
      }
    }
  }
}

/// Compiles a grammar from a string.
///
/// # Example
///
/// Basic usage:
/// ```
///  # use std::path::PathBuf;
///  use sherpa_core::compile_grammar_from_string;
///
/// let (grammar, errors) = compile_grammar_from_string(
///   "@NAME my_grammar
///   <> A > \\hello \\world ",
///   &PathBuf::default(),
/// );
///
/// assert_eq!(grammar.unwrap().name, "my_grammar")
/// ```
pub fn compile_grammar_from_string(
  j: &mut Journal,
  string: &str,
  absolute_path: &PathBuf,
) -> (Option<Arc<GrammarStore>>, Option<Vec<SherpaError>>) {
  j.set_active_report("General Grammar Compiler", ReportType::GrammarCompile(Default::default()));
  j.report_mut().start_timer("Compile");
  j.report_mut().add_note("Source", string.to_string());
  match compile_grammar_ast(Vec::from(string.as_bytes())) {
    Ok(grammar) => {
      j.report_mut().stop_timer("Compile");
      compile_grammars_into_store(j, vec![(absolute_path.clone(), Default::default(), grammar)])
        .unwrap()
    }
    Err(err) => {
      j.report_mut().stop_timer("Compile");
      (None, Some(vec![err]))
    }
  }
}
