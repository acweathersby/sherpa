use super::create_store::insert_rules;
use crate::{
  compile::{GrammarId, GrammarStore, SymbolID},
  types::SherpaErrorSeverity,
  Journal,
  ReportType,
  SherpaError,
};
use std::{
  collections::{btree_map, BTreeMap, HashMap, VecDeque},
  sync::Arc,
};

pub fn merge_grammars(
  j: &mut Journal,
  g: &mut GrammarStore,
  foreign_grammars: &[Arc<GrammarStore>],
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

    // Merge reduce functions
    g.reduce_functions.append(&mut import_grammar.reduce_functions.clone());

    // Collect all pending merge productions
    for (prod_id, merge) in &import_grammar.merge_productions {
      match merge_productions.entry(*prod_id) {
        btree_map::Entry::Occupied(mut e) => {
          e.get_mut().1.append(&mut merge.1.clone());
        }
        btree_map::Entry::Vacant(e) => {
          e.insert(merge.clone());
        }
      }
    }
  }

  // Merge all referenced foreign productions into the root.
  let mut symbol_queue = create_symbol_queue(g);

  while let Some((sym, tok)) = symbol_queue.pop_front() {
    let syms_grammar = sym.get_grammar_id();
    match (
      syms_grammar,
      grammar_lu.get(&syms_grammar),
      (match sym {
        SymbolID::Production(prod_id, _) => Some(prod_id),
        SymbolID::TokenProduction(prod_id, ..) => Some(prod_id),
        _ => None,
      })
      .map(|prod_id| g.productions.entry(prod_id)),
    ) {
      (grammar_id, Some(import_g), Some(std::collections::btree_map::Entry::Vacant(entry)))
        if grammar_id != g.id.guid =>
      {
        let imported_prod_id = entry.key().clone();
        match import_g.productions.get(&imported_prod_id) {
          Some(production) => {
            // Import all bodies referenced by this foreign production
            let rules = import_g.production_rules.get(&imported_prod_id).unwrap().clone();
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
                      SymbolID::TokenProduction(prod_id, grammar_id, ..) => {
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
            g.production_rules.insert(imported_prod_id, rules);

            // Import the foreign production
            entry.insert(production.clone());
          }
          None => {
            j.report_mut().add_error(SherpaError::SourceError {
              id:         "nonexistent-import-production",
              msg:        format!(
                "Could not locate production in imported grammar {}",
                import_g.id.path.to_str().unwrap()
              ),
              inline_msg: "could not find".to_string(),
              loc:        tok,
              path:       g.id.path.clone(),
              severity:   SherpaErrorSeverity::Critical,
              ps_msg:     Default::default(),
            });
          }
        }
      }
      _ => {}
    }
  }

  // Extend every production in the grammar that has a merge production
  // entry.
  for (prod_id, (name, rules)) in merge_productions.into_iter() {
    match g.productions.contains_key(&prod_id) {
      true => {
        // extend the existing bodies with the new bodies
        let body_count = insert_rules(g, &prod_id, rules).len();

        g.productions.get_mut(&prod_id).unwrap().number_of_rules = body_count as u16;
      }
      false => j.report_mut().add_error(SherpaError::SourceError {
        id:         "missing-append-host",
        msg:        format!(
          "
Target production for appended rules does not exist.

Append productions must reference an existing production. In this case, the 
production [{0}] should have been defined with a normal production definition 
expression, e.g: `<> {0} > symA ... symN`
",
          name
        ),
        inline_msg: (if rules.len() > 1 {
          "These rules are unreachable"
        } else {
          "This rule is unreachable"
        })
        .to_string(),
        loc:        (&rules[0].tok + &rules.last().unwrap().tok).clone(),
        path:       Default::default(),
        severity:   SherpaErrorSeverity::Critical,
        ps_msg:     Default::default(),
      }),
    }
  }

  j.report_mut().stop_timer("Merge");
}

fn create_symbol_queue(g: &mut GrammarStore) -> VecDeque<(SymbolID, sherpa_runtime::types::Token)> {
  let mut symbol_queue = VecDeque::from_iter(g.rules.iter().flat_map(|(_, r)| {
    r.syms.iter().filter_map(|rule_sym| {
      let sym_id = rule_sym.sym_id;
      match sym_id {
        SymbolID::Production(..) => Some((sym_id, rule_sym.tok.clone())),
        SymbolID::TokenProduction(prod_id, grammar_id, ..) => {
          // Remap the production token symbol to regular a production symbol and
          // submit as a merge candidate.
          Some((SymbolID::Production(prod_id, grammar_id), rule_sym.tok.clone()))
        }
        _ => None,
      }
    })
  }));

  // Merge ignored terminal non terminal symbols
  for (_, syms) in &g.production_ignore_symbols {
    for sym_id in syms {
      match sym_id {
        SymbolID::TokenProduction(prod_id, grammar_id, ..) => {
          // Remap the production token symbol to regular a production symbol and
          // submit as a merge candidate.
          symbol_queue.push_back((SymbolID::Production(*prod_id, *grammar_id), Default::default()));
        }
        _ => {}
      }
    }
  }
  symbol_queue
}
