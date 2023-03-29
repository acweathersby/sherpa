use super::super::types::*;
use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
  grammar::{
    compile::parser::sherpa::Ascript,
    hash_id_value_u64,
    new::types::{
      CachedString,
      DBProdKey,
      IStringStore,
      OrderedMap,
      ParserDatabase,
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

pub(crate) async fn build_compile_db<'a>(
  j: Journal,
  g: GrammarIdentity,
  gs: &'a GrammarSoup,
  spawner: &Spawner<SherpaResult<()>>,
) -> SherpaResult<ParserDatabase> {
  // Gain read access to all parts of the GrammarCloud.
  // We don't want anything changing during these next steps.

  let GrammarSoup {
    grammar_headers, productions, string_store: s_store, ..
  } = gs;

  let productions = productions.read()?;
  let grammar_headers = grammar_headers.read()?;
  let root_grammar = grammar_headers.get(&g.guid)?.as_ref();

  // Build production list.

  let mut symbols = Map::from_iter(vec![(SymbolId::Default, 0)]);
  let mut production_queue =
    Queue::from_iter(root_grammar.pub_prods.values().cloned());

  // Maps production sym IDs to indices.
  let mut prod_map_owned = Map::with_capacity(productions.len());
  let p_map = &mut prod_map_owned;

  // Stores all used rules
  let mut rule_table_owned: Array<DBRule> = Array::new();
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
      let name = prod.name;
      let rules = prod.rules.clone();

      add_prod(prod_id.as_sym(), rules, p_map, r_table, p_r_map, false);
      add_prod_name(prod_name_lu, name);

      // Gain references on all sub productions. ----------------------------
      for (index, prod) in prod.sub_prods.iter().enumerate() {
        let prod_id = prod.id;
        if !p_map.contains_key(&prod_id.as_sym()) {
          let rules = prod.rules.clone();
          let name = prod.name;

          add_prod(prod_id.as_sym(), rules, p_map, r_table, p_r_map, false);
          add_prod_name(prod_name_lu, name);
          extract_prod_syms(
            &prod.rules,
            &mut production_queue,
            &productions,
            s_store,
            &mut symbols,
            &mut token_names,
            &mut token_productions,
          )?;
        }
      }

      for sym in prod.symbols.iter() {
        insert_symbol(&mut symbols, sym);
      }

      extract_prod_syms(
        &prod.rules,
        &mut production_queue,
        &productions,
        s_store,
        &mut symbols,
        &mut token_names,
        &mut token_productions,
      )?;

      fn extract_prod_syms(
        rules: &[Rule],
        production_queue: &mut VecDeque<ProductionId>,
        productions: &Map<ProductionId, Box<Production>>,
        s_store: &IStringStore,
        symbols: &mut Map<SymbolId, usize>,
        token_names: &mut Map<SymbolId, IString>,
        token_productions: &mut VecDeque<ProductionId>,
      ) -> SherpaResult<()> {
        for rule in rules {
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
                let name = productions.get(&id.as_parse_prod())?.name;
                let name = ("tk:".to_string() + &name.to_string(s_store))
                  .intern(s_store);
                insert_symbol(symbols, sym);
                token_names.insert(id.as_parse_prod().as_tok_sym(), name);
                token_productions.push_back(id.as_parse_prod());
              }
              _ => {}
            }
          }
        }
        SherpaResult::Ok(())
      }
    }
  }

  // Generate token productions -----------------------------------------------
  while let Some(prod_id) = token_productions.pop_front() {
    // Convert any left immediate recursive rules to right.
    if !p_map.contains_key(&prod_id.as_tok_sym()) {
      let (internal_id, rules, g_id, name) = match prod_id {
        ProductionId::Standard(internal_id, ..) => {
          let prod = productions.get(&ProductionId::Standard(
            internal_id,
            ProductionSubType::Parser,
          ))?;
          (internal_id, &prod.rules, prod.g_id, prod.name)
        }
        ProductionId::Sub(internal_id, index, ..) => {
          let prod = &productions
            .get(&ProductionId::Standard(
              internal_id,
              ProductionSubType::Parser,
            ))?
            .sub_prods[index as usize];
          (internal_id, &prod.rules, prod.g_id, prod.name)
        }
      };

      let name = ("tk_".to_string() + &name.to_str(s_store).as_str());
      let name = name.intern(s_store);

      if prod_is_immediate_left_recursive(prod_id, &rules) {
        let rules = rules.clone();

        let prime_id = ProductionId::Sub(
          internal_id,
          p_map.len() as u32 + 9000,
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

        let prod_id = prod_id.as_tok_sym();
        let prime_id = prime_id.as_tok_sym();

        add_prod(prod_id, r_rules, p_map, r_table, p_r_map, true);
        add_prod_name(prod_name_lu, name);

        add_prod(prime_id, p_rules, p_map, r_table, p_r_map, true);
        add_prod_name(
          prod_name_lu,
          (name.to_string(s_store) + "_prime").intern(s_store),
        );
      } else {
        let prod_id = prod_id.as_tok_sym();
        if !p_map.contains_key(&prod_id) {
          let mut rules = rules.clone();

          convert_symbols_to_scanner_symbols(&mut rules, s_store);
          insert_token_production(&mut rules, &mut token_productions);

          add_prod(prod_id, rules, p_map, r_table, p_r_map, true);
          add_prod_name(prod_name_lu, name);
        }
      }
    }
  }

  // Generate symbol productions and symbol indices ---------------------------
  for sym in symbols.keys() {
    if sym.is_default() {
      continue;
    }
    if !p_map.contains_key(&sym.to_scanner_prod_id().as_tok_sym()) {
      match sym {
        SymbolId::Default => {}
        SymbolId::Token { val, precedence } => {
          let prod_id = sym.to_scanner_prod_id();
          let mut rules = Array::from([Rule {
            symbols: Array::from([(*sym, 0 as usize)]),
            skipped: Default::default(),
            ast:     None,
          }]);
          convert_symbols_to_scanner_symbols(&mut rules, s_store);

          add_prod(prod_id.as_tok_sym(), rules, p_map, r_table, p_r_map, true);
          add_prod_name(
            prod_name_lu,
            ("tok_".to_string() + &hash_id_value_u64(val).to_string())
              .intern(s_store),
          );
          token_names.insert(prod_id.as_tok_sym(), *val);
        }
        sym if sym.is_term() => {
          let prod_id = sym.to_scanner_prod_id();
          let rules = Array::from_iter(vec![Rule {
            symbols: vec![(*sym, 0)],
            skipped: Default::default(),
            ast:     None,
          }]);
          add_prod(prod_id.as_tok_sym(), rules, p_map, r_table, p_r_map, true);
          add_prod_name(
            prod_name_lu,
            ("sym_".to_string() + &hash_id_value_u64(sym).to_string())
              .intern(s_store),
          );
        }
        _ => {}
      }
    }
  }

  let sym_lu = convert_index_map_to_vec(symbols.iter().map(|(sym, index)| {
    (
      DBTokenData {
        prod_id: if sym.is_default() {
          Default::default()
        } else {
          let prod_id = sym.to_scanner_prod_id().as_tok_sym();
          p_map.get(&prod_id).map(|i| DBProdKey::from(*i)).unwrap_or_default()
        },
        sym_id:  *sym,
        tok_id:  (*index).into(),
        tok_val: (*index),
      },
      *index,
    )
  }));

  // Convert convert GUID symbol ids to local indices. ------------------------
  convert_rule_symbols(r_table, p_map, symbols);

  let entry_points = root_grammar
    .pub_prods
    .iter()
    .map(|(name, prod_id)| {
      let prod_name = productions.get(prod_id).unwrap().name;
      EntryPoint {
        prod_key: DBProdKey::from(*p_map.get(&prod_id.as_sym()).unwrap()),
        entry_name: *name,
        prod_name,
        prod_entry_name: (prod_name.to_string(s_store) + "_entry")
          .intern(s_store),
        prod_exit_name: (prod_name.to_string(s_store) + "_exit")
          .intern(s_store),
      }
    })
    .collect::<Array<_>>();

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

  let db = ParserDatabase::new(
    root_grammar.identity.name,
    prod_lu,
    prod_name_lu_owned,
    p_r_map_owned,
    rule_table_owned,
    sym_lu,
    entry_points,
    s_store.clone(),
  );

  SherpaResult::Ok(db)
}

fn convert_rule_symbols(
  r_table: &mut Vec<DBRule>,
  p_map: &mut Map<SymbolId, usize>,
  symbols: Map<SymbolId, usize>,
) {
  for DBRule { rule, is_scanner, .. } in r_table {
    for (sym, _) in &mut rule.symbols {
      convert_rule_symbol(sym, p_map, &symbols, *is_scanner);
    }

    if !*is_scanner {
      for sym in &mut rule.skipped {
        convert_rule_symbol(sym, p_map, &symbols, *is_scanner);
      }
    }
  }
}

fn convert_rule_symbol(
  sym: &mut SymbolId,
  p_map: &mut std::collections::HashMap<SymbolId, usize>,
  symbols: &std::collections::HashMap<SymbolId, usize>,
  is_scanner: bool,
) {
  match *sym {
    SymbolId::NonTerminalToken { id, precedence } => {
      if is_scanner {
        let index = p_map.get(&id.as_parse_prod().as_tok_sym()).unwrap();
        *sym = SymbolId::DBNonTerminal { key: (*index as u32).into() }
      } else {
        let index = p_map.get(sym).unwrap();
        *sym = SymbolId::DBNonTerminalToken {
          prod_key: (*index as u32).into(),
          sym_key: symbols.get(sym).map(|i| (*i as u32).into()),
          precedence,
        }
      }
    }
    SymbolId::NonTerminal { .. } => {
      let index = p_map.get(sym).unwrap();
      *sym = SymbolId::DBNonTerminal { key: (*index as u32).into() };
    }
    sym_id if !is_scanner => {
      let index = symbols.get(&sym_id).unwrap();
      *sym = SymbolId::DBToken { key: (*index as u32).into() }
    }
    _ => {}
  }
}

fn add_prod_name(prod_name_lu: &mut Vec<IString>, name: IString) {
  prod_name_lu.push(name);
}

fn set_scanner_on_rules(rule_lookup: &mut Array<DBRule>) {
  for rule in rule_lookup {}
}
/// Inserts production rule data into the appropriate lookup tables.
fn add_prod(
  prod_sym: SymbolId,
  mut prod_rules: Array<Rule>,
  production_map: &mut std::collections::HashMap<SymbolId, usize>,
  rules: &mut Array<DBRule>,
  prod_rule_map: &mut Array<Array<DBRuleKey>>,
  is_scanner: bool,
) {
  let prod_index = production_map.len();

  let rules_ids = (rules.len() as u32)..(rules.len() + prod_rules.len()) as u32;

  prod_rule_map.push(rules_ids.map(|i| i.into()).collect::<Array<_>>());

  rules.append(
    &mut prod_rules
      .into_iter()
      .map(|r| DBRule { rule: r, prod_id: prod_index.into(), is_scanner })
      .collect(),
  );

  let val = production_map.insert(prod_sym, prod_index);

  debug_assert!(val.is_none());
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
          let last = string.chars().count() - 1;
          let mut characters = string
            .chars()
            .enumerate()
            .map(|(i, c)| {
              if c.is_ascii() {
                let char = c as u8;
                (
                  SymbolId::Char { char, precedence: *precedence },
                  if i == last { *index } else { 99999 },
                )
              } else {
                let val = c as u32;
                (
                  SymbolId::Codepoint { precedence: *precedence, val },
                  if i == last { *index } else { 99999 },
                )
              }
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
  prod_id: ProductionId,
  rules: &[Rule],
) -> bool {
  for rule in rules {
    match rule.symbols[0].0 {
      SymbolId::NonTerminal { id, .. } if id == prod_id => {
        //Convert the production right recursive.
        return true;
      }
      _ => {}
    }
  }
  false
}
