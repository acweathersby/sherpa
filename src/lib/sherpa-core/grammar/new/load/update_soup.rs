use super::super::types::*;
use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
  grammar::{
    compile::parser::sherpa::Ascript,
    new::types::{
      CachedString,
      CompileDatabase,
      IStringStore,
      IndexedProdId,
      OrderedMap,
      ProductionSubType,
      Rule,
    },
  },
  parser::hash_group_btreemap,
  tasks::{new_taskman, Executor, Spawner},
  types::{graph::Origin, Items},
  Journal,
  ReportType,
  SherpaResult,
};
use core::panic;
use std::{collections::VecDeque, ops::Index, path::PathBuf, sync::Arc};

pub(crate) async fn verify_productions<'a>() {}

pub(crate) async fn compile_parser<'a>(
  j: Journal,
  g: GrammarIdentity,
  gs: &'a GrammarSoup,
  spawner: &Spawner<SherpaResult<()>>,
) -> SherpaResult<CompileDatabase> {
  // Gain read access to all parts of the GrammarCloud.
  // We don't want anything changing during these next steps.

  let GrammarSoup {
    grammar_headers, productions, string_store: s_store, ..
  } = gs;

  let productions = productions.read()?;
  let grammar_headers = grammar_headers.read()?;
  let root_grammar = grammar_headers.get(&g.guid)?.as_ref();

  // Build production list.

  let mut symbols = Map::new();
  let mut production_queue =
    Queue::from_iter(root_grammar.pub_prods.values().cloned());

  // Maps production sym IDs to indices.
  let mut prod_map_owned = Map::with_capacity(productions.len());
  let p_map = &mut prod_map_owned;

  // Stores all used rules
  let mut rule_table_owned: Array<(Rule, IndexedProdId)> = Array::new();
  let r_table = &mut rule_table_owned;

  // Maps prod indices to rules.
  let mut p_r_map_owned = Array::new();
  let p_r_map = &mut p_r_map_owned;

  // Friendly names for productions
  let mut prod_name_lu_owned = Array::new();
  let prod_name_lu = &mut prod_name_lu_owned;

  // Keeps track of the names of token_productions.
  let mut token_names = Map::new();

  let mut token_productions = VecDeque::new();

  while let Some(prod_id) = production_queue.pop_front() {
    if !p_map.contains_key(&prod_id.as_sym()) {
      let prod = productions.get(&prod_id)?;

      let prod_name = prod.name;

      add_prod(prod_id.as_sym(), prod.rules.clone(), p_map, r_table, p_r_map);
      add_prod_name(prod_name_lu, prod_name);

      // Gain references on all sub productions. ----------------------------
      for (index, prod) in prod.sub_prods.iter().enumerate() {
        let prod_id = prod.id;
        if !p_map.contains_key(&prod_id.as_sym()) {
          let rules = prod.rules.clone();
          let name = (prod_name.to_string(s_store)
            + "_"
            + &prod.type_.to_string()
            + "_"
            + &index.to_string())
            .intern(s_store);

          add_prod(prod_id.as_sym(), rules, p_map, r_table, p_r_map);
          add_prod_name(prod_name_lu, name);
        }
      }

      for sym in prod.symbols.iter() {
        insert_symbol(&mut symbols, sym);
      }

      for rule in &prod.rules {
        for (sym, _) in &rule.symbols {
          match sym {
            SymbolId::NonTerminal { id, .. } => match id {
              ProductionId::Standard(..) => {
                production_queue.push_back(*id);
              }
              ProductionId::Sub(..) => {
                // We've already merged in the production's sub-productions
              }
              _ => {}
            },
            SymbolId::NonTerminalToken { id, .. } => {
              let name = productions.get(&id)?.name;
              let name = ("tk:".to_string() + name.to_str(s_store).as_str())
                .intern(s_store);
              insert_symbol(&mut symbols, sym);
              token_names.insert(id.as_parse_prod().as_tok_sym(), name);
              token_productions.push_back(id.as_parse_prod());
            }
            _ => {}
          }
        }
      }
    }
  }

  // Generate token productions -----------------------------------------------
  while let Some(prod_id) = token_productions.pop_front() {
    match prod_id {
      ProductionId::Standard(internal_id, ..)
      | ProductionId::Sub(internal_id, ..) => {
        let prod = productions.get(&prod_id)?;
        // Convert any left immediate recursive rules to right.
        if !p_map.contains_key(&prod_id.as_tok_sym()) {
          if prod_is_immediate_left_recursive(prod) {
            let rules = prod.rules.clone();

            let prime_id = ProductionId::Sub(
              internal_id,
              prod.sub_prods.len() as u32,
              ProductionSubType::Scanner,
            );

            let mut groups =
              hash_group_btreemap(rules, |_, r| match r.symbols[0].0 {
                SymbolId::NonTerminal { id, .. } if id == prod_id => true,
                _ => false,
              });

            let mut prime_rules = groups.remove(&true)?;
            let mut r_rules = groups.remove(&false)?;

            let mut p_rules = prime_rules
              .into_iter()
              .flat_map(|mut r| {
                r.symbols.remove(0);
                let rule_a = r.clone();
                r.symbols.push((prime_id.as_tok_sym(), 0));
                [rule_a, r]
              })
              .collect();

            r_rules.append(
              &mut r_rules
                .clone()
                .into_iter()
                .map(|mut r| {
                  r.symbols.push((prime_id.as_tok_sym(), 0));
                  r
                })
                .collect(),
            );

            insert_token_production(&mut r_rules, &mut token_productions);
            insert_token_production(&mut p_rules, &mut token_productions);

            convert_symbols_to_scanner_symbols(&mut r_rules, s_store);
            convert_symbols_to_scanner_symbols(&mut p_rules, s_store);

            add_prod(prod_id.as_tok_sym(), r_rules, p_map, r_table, p_r_map);
            add_prod(prime_id.as_tok_sym(), p_rules, p_map, r_table, p_r_map);
          } else {
            p_map.insert(prod_id.as_tok_sym(), r_table.len());
            let mut rules = prod.rules.clone();
            convert_symbols_to_scanner_symbols(&mut rules, s_store);
            insert_token_production(&mut rules, &mut token_productions);
            add_prod(prod_id.as_tok_sym(), rules, p_map, r_table, p_r_map);
          }
        }
      }
      _ => unreachable!(),
    }
  }

  // Generate symbol productions and symbol indices ---------------------------
  for sym in symbols.keys() {
    match sym {
      SymbolId::Token { val, precedence } => {
        let prod_id = sym.to_scanner_prod_id();
        let mut rules = Array::from([Rule {
          symbols: Array::from([(*sym, 0 as usize)]),
          ast:     None,
        }]);
        convert_symbols_to_scanner_symbols(&mut rules, s_store);
        add_prod(prod_id.as_tok_sym(), rules, p_map, r_table, p_r_map);
        add_prod_name(prod_name_lu, *val);
        token_names.insert(prod_id.as_tok_sym(), *val);
      }
      _ => {}
    }
  }

  // Convert convert GUID symbol ids to local indices. ------------------------
  for (rule, _) in r_table {
    for (sym, _) in &mut rule.symbols {
      match *sym {
        SymbolId::Token { val, precedence } => {
          let index = symbols.get(sym).unwrap();
          *sym =
            SymbolId::IndexedToken { index: (*index as u32).into(), precedence }
        }
        SymbolId::NonTerminalToken { id, precedence } => {
          let index = p_map.get(sym).unwrap();
          *sym = SymbolId::IndexedNonTerminalToken {
            index: (*index as u32).into(),
            sym_index: symbols.get(sym).map(|i| (*i as u32).into()),
            precedence,
          }
        }
        SymbolId::NonTerminal { .. } => {
          let index = p_map.get(sym).unwrap();
          *sym = SymbolId::IndexedNonTerminal { index: (*index as u32).into() };
        }
        _ => {}
      }
    }
  }

  let entry_points = root_grammar
    .pub_prods
    .iter()
    .map(|(name, prod_id)| (*p_map.get(&prod_id.as_sym()).unwrap(), *name))
    .collect::<Array<_>>();

  let sym_lu =
    convert_index_map_to_vec(symbols.iter().map(|(sym, index)| match sym {
      SymbolId::Token { .. } => {
        let prod_id = sym.to_scanner_prod_id().as_tok_sym();
        ((*sym, p_map.get(&prod_id).map(|i| *i as u32)), *index)
      }
      SymbolId::NonTerminalToken { .. } => {
        let prod_id = sym.to_scanner_prod_id().as_tok_sym();
        ((*sym, p_map.get(&prod_id).map(|i| *i as u32)), *index)
      }
      _ => ((*sym, None), *index),
    }));
  let prod_lu = convert_index_map_to_vec(prod_map_owned);
  let prod_name_lu = prod_lu
    .iter()
    .map(|p| match p {
      SymbolId::NonTerminal { id } => match productions.get(id) {
        Some(prod) => prod.name.clone(),
        _ => IString::default(),
      },
      SymbolId::NonTerminalToken { precedence, id } => token_names
        .get(&id.as_parse_prod().as_tok_sym())
        .cloned()
        .unwrap_or_default(),
      _ => IString::default(),
    })
    .collect::<Array<_>>();

  let db = CompileDatabase::new(
    root_grammar.identity.name,
    prod_lu,
    prod_name_lu_owned,
    p_r_map_owned,
    rule_table_owned,
    sym_lu,
    entry_points,
    s_store.clone(),
  );

  dbg!(&db);

  SherpaResult::Ok(db)
}

fn add_prod_name(prod_name_lu: &mut Vec<IString>, name: IString) {
  prod_name_lu.push(name);
}

/// Inserts production rule data into the appropriate lookup tables.
fn add_prod(
  prod_sym: SymbolId,
  mut prod_rules: Array<Rule>,
  production_map: &mut std::collections::HashMap<SymbolId, usize>,
  rule_lookup: &mut Array<(Rule, IndexedProdId)>,
  prod_rule_map: &mut Array<Array<IndexRuleKey>>,
) {
  let prod_index = production_map.len();
  let rules_ids =
    (rule_lookup.len() as u32)..(rule_lookup.len() + prod_rules.len()) as u32;
  prod_rule_map.push(rules_ids.map(|i| i.into()).collect::<Array<_>>());
  rule_lookup.append(
    &mut prod_rules.into_iter().map(|r| (r, prod_index.into())).collect(),
  );
  production_map.insert(prod_sym, prod_index);
}

fn convert_index_map_to_vec<T, C: IntoIterator<Item = (T, usize)>>(
  production_map: C,
) -> Array<T> {
  production_map
    .into_iter()
    .map(|(k, v)| (v, k))
    .collect::<OrderedMap<_, _>>()
    .into_values()
    .collect::<Array<_>>()
}

fn insert_symbol(symbol_map: &mut Map<SymbolId, usize>, sym: &SymbolId) {
  if !symbol_map.contains_key(sym) {
    symbol_map.insert(*sym, symbol_map.len());
  }
}

/// Converts the symbols a set of rules intended for a scanner production.
/// Symbols are converted into individual character/byte values that allow
/// scanner parsers to scan a single character/byte/codepoint at time to
/// recognize a token.
fn convert_symbols_to_scanner_symbols(
  rules: &mut [Rule],
  s_store: &IStringStore,
) {
  for rule in rules {
    let syms = rule
      .symbols
      .iter()
      .flat_map(|(sym, index)| match sym {
        SymbolId::Token { val, precedence } => {
          let guarded_str = val.to_str(s_store);
          let string = guarded_str.as_str();
          let last = string.len() - 1;
          let mut characters = string
            .as_bytes()
            .iter()
            .enumerate()
            .map(|(i, c)| {
              (
                SymbolId::Char {
                  char:       *c,
                  precedence: if i == last { *precedence } else { 0 },
                },
                if i == last { *index } else { 99999 },
              )
            })
            .collect::<Array<_>>();

          characters
        }
        _ => Array::from([(*sym, *index)]),
      })
      .collect();
    rule.symbols = syms;
  }
}

fn insert_token_production(
  rules: &mut Vec<Rule>,
  token_productions: &mut VecDeque<ProductionId>,
) {
  for rule in rules {
    let syms = rule
      .symbols
      .iter()
      .map(|(sym, index)| match sym {
        SymbolId::NonTerminal { id } => {
          token_productions.push_back(*id);
          (id.as_tok_sym(), *index)
        }
        _ => (*sym, *index),
      })
      .collect();
    rule.symbols = syms;
  }
}

fn prod_is_immediate_left_recursive(
  prod: &super::super::types::Production,
) -> bool {
  for rule in prod.rules.iter() {
    match rule.symbols[0].0 {
      SymbolId::NonTerminal { id, .. } if id == prod.id => {
        //Convert the production right recursive.
        return true;
      }
      _ => {}
    }
  }
  false
}

// Extract all productions referenced by the entry production of the grammar.

// Merge any outstanding production merges to create finalized productions.

// Extract token information

// Extract all rules.

// Remap any rule that needs to be right recursive (for scanner states)

// Prepare Custom states.

// Merge all tokens into a token set.

// ---------------------------------------------

// For each production. Produce a parse graph and parse state IR.

// For each token production. Produce scanner graph and parse state IR

// For each token set in applicable parse states extract token sets, merge
// and produce scanner graphs and parse state IRs

// (invariant) Ensure 1 to 1 mappings between parse_state ids and parse state
// IR.

// (required) Run parse states through one GC pass.

// (optional) Run parse states through optimizer and additional GC passes.

// Return parse state IR as the finalized parser before lowering to
// executable code.

// --------------------------------------------
