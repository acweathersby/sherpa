use super::create_store::{get_terminal_id, insert_production, insert_rules};
use crate::{
  compile::{GrammarStore, ProductionId, SymbolID},
  grammar::{
    create_closure,
    create_defined_scanner_name,
    create_scanner_name,
    get_production_start_items,
  },
  types::{item::Item, *},
  util::get_num_of_available_threads,
  Journal,
  ReportType,
  SherpaError,
};
use std::{
  collections::{btree_map, BTreeSet, VecDeque},
  thread,
};

/// Create scanner productions and data caches, and converts ids to tokens.
pub fn finalize_grammar(j: &mut Journal, mut g: GrammarStore) -> GrammarStore {
  j.set_active_report(
    &format!("Grammar [{}] Compilation", g.id.name),
    ReportType::GrammarCompile(g.id.guid),
  );

  j.report_mut().start_timer("Finalize");

  create_scanner_productions_from_symbols(j, &mut g);

  if check_for_missing_productions(j, &g)
    || j.report_mut().have_errors_of_type(SherpaErrorSeverity::Critical)
  {
    return g;
  }

  check_for_missing_productions(j, &g);

  finalize_productions(j, &mut g);

  finalize_symbols(j, &mut g);

  finalize_items(j, &mut g);

  set_parse_productions(&mut g);

  finalize_bytecode_metadata(j, &mut g);

  mark_entry_productions(&mut g);

  j.report_mut().stop_timer("Finalize");

  g
}

fn mark_entry_productions(g: &mut GrammarStore) {
  for (p, _) in g.exports.clone().iter() {
    g.productions.get_mut(&p).unwrap().is_entry = true;
  }
}

fn create_scanner_productions_from_symbols(j: &mut Journal, g: &mut GrammarStore) {
  // Start iterating over known token production references, and add
  // new productions as we encounter them.
  let mut scanner_production_queue = VecDeque::from_iter(
    g.symbols
      .iter()
      .map(|(id, sym)| (*id, sym.g_ref.as_ref().unwrap().clone(), sym.loc.clone(), false))
      .chain(SymbolID::Generics.iter().map(|id| (*id, g.id.clone(), Token::new(), false))),
  );

  while let Some((sym_id, g_ref, tok, scanner_only)) = scanner_production_queue.pop_front() {
    match &sym_id {
      sym if matches!(sym, SymbolID::GenericSpace) => {
        // Converts the generic symbol `g:sp` into a production that targets the
        // the defined symbol `b'32'`
        let (_, prod_id, name, _) = get_scanner_info_from_defined(&sym_id, g);

        convert_scan_symbol_to_production(
          g,
          sym_id,
          &[&" ".to_string()],
          prod_id,
          name,
          Token::empty(),
        );
      }
      sym if matches!(sym, SymbolID::GenericNewLine) => {
        // Converts the generic symbol `g:sp` into a production that targets the
        // the defined symbol `b'10'`
        let (_, prod_id, name, _) = get_scanner_info_from_defined(&sym_id, g);

        convert_scan_symbol_to_production(
          g,
          sym_id,
          &[&"\n".to_string()],
          prod_id,
          name,
          Token::empty(),
        );
      }
      sym if SymbolID::Generics.contains(sym) => {
        // Converts a generic symbol into a scanner production if such a production
        // does not yet exist in the grammar.

        let (_, prod_id, name, _) = get_scanner_info_from_defined(&sym_id, g);

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
            vec![Rule {
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

        convert_scan_symbol_to_production(
          g,
          sym_id,
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
      SymbolID::Production(prod_id, grammar_id)
      | SymbolID::TokenProduction(prod_id, grammar_id, ..) => {
        match g.productions.get(prod_id) {
          Some(production) => {
            let scanner_name = create_scanner_name(*prod_id, *grammar_id);
            let scanner_production_id = ProductionId::from(&scanner_name);
            if !g.productions.contains_key(&scanner_production_id) {
              let production = production.clone();
              insert_token_production(
                g,
                crate::types::Production {
                  id: scanner_production_id,
                  guid_name: scanner_name.clone(),
                  name: if production.name.starts_with("tk:") {
                    production.name
                  } else {
                    format!("tk:{}", production.name)
                  },
                  loc: production.loc.clone(),
                  is_scanner: true,
                  sym_id: SymbolID::TokenProduction(*prod_id, *grammar_id, scanner_production_id),
                  ..Default::default()
                },
                g.production_rules
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
                        | SymbolID::TokenProduction(prod_id, grammar_id, ..) => {
                          scanner_production_queue.push_back((
                            *sym_id,
                            sym.grammar_ref.clone(),
                            sym.tok.clone(),
                            true,
                          ));

                          let scanner_name = create_scanner_name(*prod_id, *grammar_id);
                          let scanner_production_id = ProductionId::from(&scanner_name);
                          let new_symbol_id =
                            SymbolID::Production(scanner_production_id, *grammar_id);

                          vec![RuleSymbol {
                            consumable: true,
                            precedence: sym.precedence,
                            scanner_length: 1,
                            sym_id: new_symbol_id,
                            grammar_ref: g.id.clone(),
                            ..Default::default()
                          }]
                        }
                        sym_id if sym_id.is_defined() => {
                          let (new_symbol_id, ..) = get_scanner_info_from_defined(sym_id, g);

                          scanner_production_queue.push_back((
                            *sym_id,
                            sym.grammar_ref.clone(),
                            sym.tok.clone(),
                            true,
                          ));

                          vec![RuleSymbol {
                            consumable: true,
                            precedence: sym_id.is_exclusive().into(),
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

                    debug_assert_ne!(syms.len(), 0);

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
                scanner_only,
              );
            }
          }
          _ => {
            j.report_mut().add_error(SherpaError::SourceError {
              id:         "missing-production-definition",
              msg:        format!("Could not find a definition for production"),
              inline_msg: "could not find".to_string(),
              loc:        tok,
              path:       g_ref.path.clone(),
              severity:   SherpaErrorSeverity::Critical,
              ps_msg:     format!("{prod_id:?}"),
            });
          }
        }
      }
      _ => {}
    }
  }
}

fn check_for_missing_productions(j: &mut Journal, g: &GrammarStore) -> bool {
  let mut have_missing_production = false;
  // Check for missing productions referenced in rule symbols
  for (_, b) in &g.rules {
    for sym in &b.syms {
      match sym.sym_id {
        SymbolID::TokenProduction(.., prod_id) | SymbolID::Production(prod_id, _) => {
          if !g.productions.contains_key(&prod_id) {
            have_missing_production = true;
            j.report_mut().add_error(SherpaError::SourceError {
              id:         "missing-production-definition",
              msg:        format!("Could not find a definition for production"),
              inline_msg: "could not find".to_string(),
              loc:        sym.tok.clone(),
              path:       sym.grammar_ref.path.clone(),
              severity:   SherpaErrorSeverity::Critical,
              ps_msg:     "[B]".to_string(),
            });
          }
        }
        _ => {}
      }
    }
  }
  have_missing_production
}

/// Calculates recursion types of productions, converts direct left recursive scanner productions
/// to right recursive.
fn finalize_productions(_j: &mut Journal, g: &mut GrammarStore) {
  let production_ids = g.productions.keys().cloned().collect::<Vec<_>>();

  let conversion_candidate = calculate_recursions_types(production_ids, g);

  // Convert left recursive scanner productions into right recursion.
  for candidate_id in &conversion_candidate {
    convert_left_recursion_to_right(g, *candidate_id);
  }

  calculate_recursions_types(conversion_candidate, g);
}

fn finalize_symbols(_j: &mut Journal, g: &mut GrammarStore) {
  let mut symbol_bytecode_id = SymbolID::DefinedSymbolIndexBasis;

  // Ensure there is a symbol for every token production

  for sym_id in &SymbolID::Generics {
    let (_, production_id, ..) = get_scanner_info_from_defined(sym_id, g);

    let scanner_id = sym_id.bytecode_id(g);

    g.productions.get_mut(&production_id).unwrap().symbol_bytecode_id = Some(scanner_id);
  }

  for sym_id in g.symbols.keys().cloned().collect::<Vec<_>>() {
    if !g.symbols.get(&sym_id).unwrap().scanner_only {
      if matches!(sym_id, SymbolID::TokenProduction(..)) || sym_id.is_defined() {
        let symbol = g.symbols.get_mut(&sym_id).unwrap();

        symbol.bytecode_id = symbol_bytecode_id;

        g.bytecode_token_lookup.insert(symbol_bytecode_id, sym_id);

        let production_id = symbol.guid.get_production_id().unwrap_or_default();

        let (_, token_production_id, ..) = get_scanner_info_from_defined(&sym_id, g);

        let scanner_production = g.productions.get_mut(&token_production_id).unwrap();

        scanner_production.symbol_bytecode_id = Some(symbol_bytecode_id);

        if let Some(original_production) = g.productions.get_mut(&production_id) {
          original_production.symbol_bytecode_id = Some(symbol_bytecode_id);
        };

        symbol_bytecode_id += 1;
      }
    }

    g.symbols.get_mut(&sym_id).unwrap().friendly_name = sym_id.debug_string(g);
  }
  for prod in g.productions.values_mut() {
    if prod.sym_id.is_token_production() && !g.symbols.contains_key(&prod.sym_id) {
      #[cfg(debug_assertions)]
      {
        debug_assert!(
          prod.id
            == match prod.sym_id {
              SymbolID::Production(prod_id, _) | SymbolID::TokenProduction(.., prod_id) => prod_id,
              _ => ProductionId(0),
            },
          "TokenSymbols prod id does not match production's id",
        );
      }
    }

    if prod.symbol_bytecode_id.is_none() {
      prod.symbol_bytecode_id = Some(symbol_bytecode_id);

      symbol_bytecode_id += 1;
    }
  }
}

/// Creates item closure caches and creates start and goto item groups.
fn finalize_items(_j: &mut Journal, g: &mut GrammarStore) {
  // Generate the item closure cache
  let start_items =
    g.productions.keys().flat_map(|p| get_production_start_items(p, g)).collect::<Vec<_>>();

  let start_items_chunks = start_items.chunks(get_num_of_available_threads()).collect::<Vec<_>>();

  for (item, closure, peek_symbols) in thread::scope(|s| {
    start_items_chunks
      .iter()
      .map(|work| {
        s.spawn(|| {
          let mut results = vec![];

          let mut pending_items = VecDeque::<Item>::from_iter(work.iter().cloned());

          while let Some(item) = pending_items.pop_front() {
            let item = item.to_absolute();
            if !item.is_completed() {
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
    g.item_ignore_symbols.insert(item.to_absolute(), peek_symbols);
    g.closures.insert(item.to_absolute(), closure);
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
            let vec = entry.get_mut();
            if !vec.contains(&item) {
              vec.push(item);
            }
          }
        }
      }
    }
  }
}

fn set_parse_productions(g: &mut GrammarStore) {
  let mut productions = g
    .get_exported_productions()
    .iter()
    .map(|ExportedProduction { production, .. }| production.id.clone())
    .collect::<BTreeSet<_>>();
  let mut queue = VecDeque::from_iter(productions.iter().cloned());

  while let Some(prod_id) = queue.pop_front() {
    for mut item in get_production_start_items(&prod_id, g) {
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

/// Adds bytecode identifiers to relevant objects.
fn finalize_bytecode_metadata(_j: &mut Journal, g: &mut GrammarStore) {
  let GrammarStore { parse_productions, productions, rules: bodies, .. } = g;

  for (index, rule) in
    bodies.values_mut().filter(|b| parse_productions.contains(&b.prod_id)).enumerate()
  {
    rule.bytecode_id = Some(index as u32);
    g.bytecode_rule_lookup.insert(index as u32, rule.id);
  }

  for (index, prod_id) in parse_productions.iter().enumerate() {
    match productions.get_mut(prod_id) {
      Some(production) => {
        production.bytecode_id = Some(index as u32);
        g.bytecode_production_lookup.insert(index as u32, *prod_id);
      }
      _ => {}
    }
  }
}

#[inline]
pub(crate) fn get_scanner_info_from_defined(
  sym_id: &SymbolID,
  root: &GrammarStore,
) -> (SymbolID, ProductionId, String, String) {
  let (scanner_production_id, scanner_name, symbol_string) = match sym_id {
    sym if sym.is_defined() => {
      let symbol_string = root.symbol_strings.get(sym_id).unwrap().to_owned();

      let mut escaped_symbol_string = symbol_string
        .chars()
        .into_iter()
        .map(|c| format!("{:X}", (c as u32)))
        .collect::<Vec<_>>()
        .join("");

      if sym.is_exclusive() {
        escaped_symbol_string = escaped_symbol_string + "_exc";
      }

      let name = create_defined_scanner_name(&escaped_symbol_string);
      (ProductionId::from(&name), name, symbol_string)
    }
    SymbolID::Production(prod_id, grammar_id) => {
      let symbol_string = root.productions.get(prod_id).unwrap().guid_name.clone();
      let scanner_name = create_scanner_name(*prod_id, *grammar_id);
      let tok_prod = ProductionId::from(&scanner_name);
      (tok_prod, scanner_name, symbol_string)
    }
    SymbolID::TokenProduction(prod_id, grammar_id, tok_prod_id) => {
      let symbol_string = root.productions.get(prod_id).unwrap().guid_name.clone();
      (*tok_prod_id, create_scanner_name(*prod_id, *grammar_id), symbol_string)
    }
    sym => {
      let symbol_string = sym.to_scanner_string();
      (ProductionId::from(&symbol_string), symbol_string.clone(), symbol_string)
    }
  };

  let new_symbol_id = SymbolID::Production(scanner_production_id, root.id.guid);

  (new_symbol_id, scanner_production_id, scanner_name, symbol_string)
}
// Global Constants

/// A postfix string added to names of production generated by left to
/// right recursive conversion.
pub const prime_symbol: &'static str = "_prime";

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
  if !a_prod.recursion_type.is_direct_left() {
    panic!("Production is not left direct recursive.");
  }

  // Remove recursion flag as it no longer applies to this production.
  a_prod.recursion_type = a_prod.recursion_type - RecursionType::LEFT_DIRECT;

  let rule_ids = g.production_rules.get(&a_prod_id).unwrap().clone();

  let rules = rule_ids
    .iter()
    .map(|rule_id| g.rules.get(rule_id).unwrap())
    .map(|b| match b.syms[0].sym_id {
      SymbolID::Production(p, _) => (b.id, p == a_prod.id),
      SymbolID::TokenProduction(.., p) => (b.id, p == a_prod.id),
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
      SymbolID::TokenProduction(a_prime_prod_id, g.id.guid, a_prime_prod_id)
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

      debug_assert_ne!(rule_a.len, 0);
      debug_assert_ne!(rule_b.len, 0);

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

      debug_assert_ne!(rule_a.len, 0);
      debug_assert_ne!(rule_b.len, 0);

      vec![rule_a, rule_b]
    })
    .collect::<Vec<_>>();

  // Replace the base production's bodies with B bodies.
  // Add A prime production to the grammar.

  if !g.production_names.contains_key(&a_prime_prod_id) {
    g.production_names
      .insert(a_prime_prod_id, (a_prime_prod.name.clone(), a_prime_prod.guid_name.clone()));
  }
  g.productions.insert(a_prime_prod_id, a_prime_prod);
  g.production_rules.insert(a_prod_id, new_B_rules.iter().map(|b| b.id).collect::<Vec<_>>());
  g.production_rules.insert(a_prime_prod_id, new_A_rules.iter().map(|b| b.id).collect::<Vec<_>>());

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

/// Calculates and assigns the recursion type for each ProductionId in the vector.
///
/// Additionally, a vector of ProductionIds of scanner productions that are [RecursionType::LEFT_DIRECT]  
/// is returned, with the assumption that these productions will be converted to
/// [RecursionType::RIGHT].
fn calculate_recursions_types(
  production_ids: Vec<ProductionId>,
  g: &mut GrammarStore,
) -> Vec<ProductionId> {
  let mut conversion_candidates = vec![];

  let production_id_chunks = production_ids
    .chunks(get_num_of_available_threads())
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>();

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

    if production.is_scanner && production.recursion_type.is_direct_left() {
      conversion_candidates.push(production.id);
    }
  }

  conversion_candidates
}

#[inline]
fn insert_token_production(
  g: &mut GrammarStore,
  mut prod: Production,
  bodies: Vec<Rule>,
  scanner_only: bool,
) {
  let prod_id = prod.id;
  let tok_id = prod.sym_id;

  #[cfg(debug_assertions)]
  {
    assert!(matches!(tok_id, SymbolID::TokenProduction(..)));
  }

  g.symbols
    .entry(tok_id)
    .or_insert_with(|| Symbol {
      byte_length: 0,
      cp_len: 0,
      bytecode_id: 0,
      guid: tok_id,
      scanner_only: true,
      friendly_name: tok_id.to_scanner_string(),
      loc: prod.loc.clone(),
      g_ref: Some(g.id.clone()),
      ..Default::default()
    })
    .scanner_only &= scanner_only;

  prod.number_of_rules = insert_rules(g, &prod_id, bodies).len() as u16;

  g.productions.insert(prod_id, prod);
}

/// Converts an array of strings into scanner bodies
/// for a given scanner production name.
fn convert_scan_symbol_to_production(
  g: &mut GrammarStore,
  sym_id: SymbolID,
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
        let new_body_symbols = create_defined_symbols_from_string(
          g,
          symbol_string,
          origin_location.clone(),
          sym_id.is_exclusive(),
        );

        debug_assert_ne!(new_body_symbols.len(), 0);

        Rule {
          len: new_body_symbols.len() as u16,
          syms: new_body_symbols,
          prod_id,
          tok: origin_location.clone(),
          is_exclusive: sym_id.is_exclusive(),
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
        sym_id, //: SymbolID::TokenProduction(prod_id, g.id.guid, prod_id),
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
  is_exclusive: bool,
) -> Vec<RuleSymbol> {
  let chars: Vec<char> = symbol_string.chars().collect();
  chars
    .iter()
    .enumerate()
    .map(|(index, byte)| {
      let string = byte.to_string();

      let id = get_terminal_id(&string, false);

      g.symbols.entry(id).or_insert_with(|| {
        g.symbol_strings.insert(id, string);
        Symbol {
          byte_length:   byte.len_utf8() as u32,
          cp_len:        1,
          bytecode_id:   0,
          guid:          id,
          scanner_only:  true,
          friendly_name: id.to_scanner_string(),
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
        precedence: is_exclusive.into(),
        ..Default::default()
      }
    })
    .collect()
}
