use sherpa_rust_runtime::types::{Token, TokenRange};

use crate::{
  journal::Journal,
  parser,
  types::*,
  utils::{create_u64_hash, hash_group_btreemap},
};
use std::collections::{btree_map, hash_map, VecDeque};

pub fn build_compile_db<'a>(mut j: Journal, g: GrammarIdentities, gs: &'a GrammarSoup) -> SherpaResult<ParserDatabase> {
  // Gain read access to all parts of the GrammarCloud.
  // We don't want anything changing during these next steps.

  j.set_active_report("Database Compile", crate::ReportType::GrammarCompile(g.guid));

  let mut is_valid = true;

  let GrammarSoup { grammar_headers, productions, string_store: s_store, custom_states, .. } = gs;

  let custom_states = custom_states.read()?;

  let productions: std::sync::RwLockReadGuard<'_, Vec<Box<Production>>> = productions.read()?;
  let productions = {
    let mut out_prods = OrderedMap::new();
    for prod in productions.iter() {
      match out_prods.entry(prod.id) {
        btree_map::Entry::Vacant(e) => {
          e.insert(prod.clone());
        }
        btree_map::Entry::Occupied(mut e) => {
          fn remap_sub_prod_offsets(rule: &mut Rule, sub_prod_offset: usize) {
            for sym_ref in &mut rule.symbols {
              match sym_ref.id {
                SymbolId::NonTerminal { id } => {
                  if let ProductionId::Sub(p, i, s) = id {
                    sym_ref.id = SymbolId::NonTerminal { id: ProductionId::Sub(p, i + sub_prod_offset as u32, s) };
                  }
                }
                _ => {}
              }
            }
          }

          let existing_prod = e.get_mut();
          let sub_prod_offset = existing_prod.sub_prods.len();

          for mut rule in prod.rules.clone() {
            remap_sub_prod_offsets(&mut rule, sub_prod_offset);
            existing_prod.rules.push(rule);
          }

          for mut sub_prod in prod.sub_prods.clone() {
            if let ProductionId::Sub(p, i, s) = sub_prod.id {
              sub_prod.id = ProductionId::Sub(p, i + sub_prod_offset as u32, s);
            }

            for rule in &mut sub_prod.rules {
              remap_sub_prod_offsets(rule, sub_prod_offset);
            }

            existing_prod.sub_prods.push(sub_prod);
          }

          existing_prod.symbols.extend(prod.symbols.iter());
        }
      };
    }
    out_prods
  };

  let grammar_headers = grammar_headers.read()?;
  let root_grammar = grammar_headers.get(&g.guid)?.as_ref();

  // Build production list.

  let mut symbols = OrderedMap::from_iter(vec![(SymbolId::Default, 0)]);
  let mut production_queue = Queue::from_iter(root_grammar.pub_prods.iter().map(|(_, p)| (p.1.clone(), p.0)));

  // Maps production sym IDs to indices.
  let mut prod_map_owned = OrderedMap::new();
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

  // Processed Custom State
  let mut c_states_owned = Array::new();
  let c_states = &mut c_states_owned;

  // Keeps track of the names of token_productions.
  let mut token_names = OrderedMap::new();
  let mut token_productions = VecDeque::new();

  while let Some((loc, prod_id)) = production_queue.pop_front() {
    if !p_map.contains_key(&prod_id.as_sym()) {
      match (custom_states.get(&prod_id), productions.get(&prod_id)) {
        (None, Some(prod)) => {
          let g_name = prod.guid_name;
          let f_name = prod.friendly_name;
          let rules = prod.rules.clone();

          add_prod_and_rules(prod_id.as_sym(), rules, p_map, r_table, p_r_map, false);
          add_prod_name(prod_name_lu, g_name, f_name);
          add_empty_custom_state(c_states);

          // Gain references on all sub productions. ----------------------------
          for prod in &prod.sub_prods {
            let prod_id = prod.id;

            if !p_map.contains_key(&prod_id.as_sym()) {
              let rules = prod.rules.clone();
              let g_name = prod.guid_name;
              let f_name = prod.friendly_name;

              add_prod_and_rules(prod_id.as_sym(), rules, p_map, r_table, p_r_map, false);
              add_prod_name(prod_name_lu, g_name, f_name);
              add_empty_custom_state(c_states);

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
        }
        (Some(state), None) => {
          let g_name = state.guid_name;
          let f_name = state.friendly_name;
          add_prod_and_rules(prod_id.as_sym(), vec![], p_map, r_table, p_r_map, false);
          add_prod_name(prod_name_lu, g_name, f_name);
          add_custom_state(state.state.clone(), c_states);
        }
        (Some(_), Some(_)) => {
          j.report_mut().add_error(SherpaError::SourceError {
            loc,
            path: Default::default(),
            id: "[2002]",
            inline_msg: Default::default(),
            msg: "Cannot resolve grammar that has production definitions and state definitions with the same name: ".to_string(),
            ps_msg: Default::default(),
            severity: SherpaErrorSeverity::Critical,
          });
          is_valid = false;
        }
        _ => {
          j.report_mut().add_error(SherpaError::SourceError {
            loc,
            path: Default::default(),
            id: "[2001]",
            inline_msg: Default::default(),
            msg: "Cannot find definition for production".into(),
            ps_msg: Default::default(),
            severity: SherpaErrorSeverity::Critical,
          });
          is_valid = false;
        }
      };
    }
  }

  if !is_valid {
    return SherpaResult::None;
  }

  // Generate token productions -----------------------------------------------
  while let Some(prod_id) = token_productions.pop_front() {
    // Convert any left immediate recursive rules to right.
    if !p_map.contains_key(&prod_id.as_tok_sym()) {
      let (internal_id, rules, name) = match prod_id {
        ProductionId::Standard(internal_id, ..) => {
          let prod = productions.get(&ProductionId::Standard(internal_id, ProductionSubType::Parser))?;
          (internal_id, &prod.rules, prod.guid_name)
        }
        ProductionId::Sub(internal_id, index, ..) => {
          let prod = &productions.get(&ProductionId::Standard(internal_id, ProductionSubType::Parser))?.sub_prods[index as usize];
          (internal_id, &prod.rules, prod.guid_name)
        }
      };

      let name = "tk_".to_string() + &name.to_string(s_store).as_str();
      let name = name.intern(s_store);

      if prod_is_immediate_left_recursive(prod_id, &rules) {
        let rules = rules.clone();

        let prime_id = ProductionId::Sub(internal_id, p_map.len() as u32 + 9000, ProductionSubType::Scanner);

        let mut groups = hash_group_btreemap(rules, |_, r| match r.symbols[0].id {
          SymbolId::NonTerminal { id, .. } if id == prod_id => true,
          _ => false,
        });

        let prime_rules = groups.remove(&true)?;
        let mut r_rules = groups.remove(&false)?;

        let mut p_rules = prime_rules
          .into_iter()
          .flat_map(|mut r| {
            r.symbols.remove(0);
            let rule_a = r.clone();
            r.symbols.push(SymbolRef { id: prime_id.as_tok_sym(), original_index: 0, ..Default::default() });
            [rule_a, r]
          })
          .collect();

        r_rules.append(
          &mut r_rules
            .clone()
            .into_iter()
            .map(|mut r| {
              r.symbols.push(SymbolRef { id: prime_id.as_tok_sym(), original_index: 0, ..Default::default() });
              r
            })
            .collect(),
        );

        insert_token_production(&mut r_rules, &mut token_productions);
        insert_token_production(&mut p_rules, &mut token_productions);

        convert_symbols_to_scanner_symbols(&mut r_rules, s_store, false);
        convert_symbols_to_scanner_symbols(&mut p_rules, s_store, false);

        let prod_id = prod_id.as_tok_sym();
        let prime_id = prime_id.as_tok_sym();

        add_prod_and_rules(prod_id, r_rules, p_map, r_table, p_r_map, true);
        add_prod_name(prod_name_lu, name, name);
        add_empty_custom_state(c_states);

        let name = (name.to_string(s_store) + "_prime").intern(s_store);
        add_prod_and_rules(prime_id, p_rules, p_map, r_table, p_r_map, true);
        add_prod_name(prod_name_lu, name, name);
        add_empty_custom_state(c_states);
      } else {
        let prod_id = prod_id.as_tok_sym();
        if !p_map.contains_key(&prod_id) {
          let mut rules = rules.clone();

          convert_symbols_to_scanner_symbols(&mut rules, s_store, false);
          insert_token_production(&mut rules, &mut token_productions);

          add_prod_and_rules(prod_id, rules, p_map, r_table, p_r_map, true);
          add_prod_name(prod_name_lu, name, name);
          add_empty_custom_state(c_states);
        }
      }
    }
  }

  // Generate scanner productions and symbol indices --------------------------
  for sym in symbols.keys() {
    if sym.is_default() {
      continue;
    }

    if !p_map.contains_key(&sym.to_scanner_prod_id().as_tok_sym()) {
      match sym {
        SymbolId::Default => {}

        SymbolId::Token { val, precedence } => {
          let prod_id = sym.to_scanner_prod_id();

          let s = &val.to_string(s_store);
          let o = if *precedence < 1 { "<> '".to_string() + s + "' > " + s } else { "<> \"".to_string() + s + "\" > " + s };

          let tok = TokenRange {
            len:      s.len() as u32,
            off:      s.len() as u32 + 8,
            line_num: 0,
            line_off: 0,
          }
          .to_token_with_string(&o);

          let mut rules = Array::from([Rule {
            symbols: Array::from([SymbolRef {
              id: *sym,
              annotation: IString::default(),
              original_index: 0,
              loc: Token::default(),
            }]),
            skipped: Default::default(),
            ast: None,
            tok,
            g_id: g,
          }]);

          convert_symbols_to_scanner_symbols(&mut rules, s_store, true);

          let name = ("tok_".to_string() + &create_u64_hash(val).to_string()).intern(s_store);

          add_prod_and_rules(prod_id.as_tok_sym(), rules, p_map, r_table, p_r_map, true);

          add_prod_name(prod_name_lu, name, name);
          add_empty_custom_state(c_states);

          token_names.insert(prod_id.as_tok_sym(), *val);
        }

        sym if sym.is_term() => {
          let prod_id = sym.to_scanner_prod_id();
          let rules = Array::from_iter(vec![Rule {
            symbols: vec![SymbolRef {
              id: *sym,
              annotation: IString::default(),
              original_index: 0,
              loc: Token::default(),
            }],
            skipped: Default::default(),
            ast:     None,
            tok:     Token::default(),
            g_id:    g,
          }]);

          let name = ("sym_".to_string() + &create_u64_hash(sym).to_string()).intern(s_store);

          add_prod_and_rules(prod_id.as_tok_sym(), rules, p_map, r_table, p_r_map, true);

          add_prod_name(prod_name_lu, name, name);
          add_empty_custom_state(c_states);
        }
        _ => {}
      }
    }
  }

  let tokens = convert_index_map_to_vec(symbols.iter().map(|(sym, index)| {
    (
      DBTokenData {
        prod_id: if sym.is_default() {
          Default::default()
        } else {
          let prod_id = sym.to_scanner_prod_id().as_tok_sym();
          p_map.get(&prod_id).map(|i| DBProdKey::from(*i)).unwrap_or_default()
        },
        sym_id:  *sym,
        name:    {
          use SymbolId::*;
          match sym {
            Undefined => "Undefine".intern(s_store),
            Default => "Default".intern(s_store),
            EndOfFile { .. } => "{EOF}".intern(s_store),
            ClassSpace { .. } => "c:sp".intern(s_store),
            ClassHorizontalTab { .. } => "c:tab".intern(s_store),
            ClassNewLine { .. } => "c:nl".intern(s_store),
            ClassIdentifier { .. } => "c:id".intern(s_store),
            ClassNumber { .. } => "c:num".intern(s_store),
            ClassSymbol { .. } => "c:sym".intern(s_store),
            Token { val, .. } => ("\"".to_string() + &val.to_string(&s_store) + "\"").intern(s_store),
            NonTerminalState { id } | NonTerminal { id, .. } | NonTerminalToken { id, .. } => {
              let name = productions.get(&id.as_parse_prod()).unwrap().friendly_name.to_string(s_store);

              ("<".to_string() + &name + ">").intern(s_store)
            }
            Codepoint { val, precedence } => {
              if *precedence > 0 {
                (val.to_string() + "{" + &precedence.to_string() + "}").intern(s_store)
              } else {
                val.to_string().intern(s_store)
              }
            }
            Char { char, .. } => {
              if *char < 128 {
                ("[".to_string() + &char::from(*char).to_string() + "]").intern(s_store)
              } else {
                ("[".to_string() + &char.to_string() + "]").intern(s_store)
              }
            }
            _ => "".intern(s_store),
          }
        },
        tok_id:  (*index).into(),
      },
      *index,
    )
  }));

  // Convert convert GUID symbol ids to local indices. ------------------------
  convert_rule_symbols(r_table, p_map, symbols);

  let entry_points = root_grammar
    .pub_prods
    .iter()
    .enumerate()
    .map(|(id, (name, (prod_id, _)))| {
      if let Some(prod_name) = productions
        .get(prod_id)
        .and_then(|d| Some(d.guid_name))
        .or_else(|| custom_states.get(prod_id).and_then(|d| Some(d.guid_name)))
      {
        EntryPoint {
          prod_key: DBProdKey::from(*p_map.get(&prod_id.as_sym()).unwrap()),
          entry_name: *name,
          prod_name,
          prod_entry_name: (prod_name.to_string(s_store) + "_entry").intern(s_store),
          prod_exit_name: (prod_name.to_string(s_store) + "_exit").intern(s_store),
          export_id: id,
        }
      } else {
        todo!("Handle missing entry point")
      }
    })
    .collect::<Array<_>>();

  let prod_lu = convert_index_map_to_vec(prod_map_owned);

  let db = ParserDatabase::new(
    root_grammar.identity.guid_name,
    prod_lu,
    prod_name_lu_owned,
    p_r_map_owned,
    rule_table_owned,
    tokens,
    entry_points,
    s_store.clone(),
    c_states_owned,
    is_valid,
  );

  SherpaResult::Ok(db)
}

fn add_custom_state(state: Box<parser::State>, c_states: &mut Vec<Option<Box<parser::State>>>) {
  c_states.push(Some(state))
}

fn add_empty_custom_state(c_states: &mut Vec<Option<Box<parser::State>>>) {
  c_states.push(None)
}

/// Converts AST Symbols into DB symbols
fn convert_rule_symbols(
  r_table: &mut Vec<DBRule>,
  p_map: &mut OrderedMap<SymbolId, usize>,
  symbols: OrderedMap<SymbolId, usize>,
) {
  for DBRule { rule, is_scanner, .. } in r_table {
    for SymbolRef { id: sym, original_index: index, .. } in &mut rule.symbols {
      *sym = convert_symbol(sym, p_map, &symbols, *is_scanner);
    }

    if !*is_scanner {
      for sym in &mut rule.skipped {
        *sym = convert_symbol(sym, p_map, &symbols, *is_scanner);
      }
    }
  }
}

/// Converts an AST symbol into a DB symbol.
fn convert_symbol(
  sym: &SymbolId,
  p_map: &mut OrderedMap<SymbolId, usize>,
  symbols: &OrderedMap<SymbolId, usize>,
  is_scanner: bool,
) -> SymbolId {
  match *sym {
    SymbolId::NonTerminalToken { id, precedence } => {
      if is_scanner {
        let index = p_map.get(&id.as_parse_prod().as_tok_sym()).unwrap();
        SymbolId::DBNonTerminal { key: (*index as u32).into() }
      } else {
        let index = p_map.get(&sym.to_plain()).unwrap();
        SymbolId::DBNonTerminalToken {
          prod_key: (*index as u32).into(),
          sym_key: symbols.get(sym).map(|i| (*i as u32).into()),
          precedence,
        }
      }
    }
    SymbolId::NonTerminal { .. } => {
      let index = p_map.get(sym).unwrap();
      SymbolId::DBNonTerminal { key: (*index as u32).into() }
    }
    sym_id if !is_scanner => {
      let index = symbols.get(&sym_id).unwrap();
      SymbolId::DBToken { key: (*index as u32).into() }
    }
    _ => *sym,
  }
}

fn extract_prod_syms(
  rules: &[Rule],
  production_queue: &mut VecDeque<(Token, ProductionId)>,
  productions: &OrderedMap<ProductionId, Box<Production>>,
  s_store: &IStringStore,
  symbols: &mut OrderedMap<SymbolId, usize>,
  token_names: &mut OrderedMap<SymbolId, IString>,
  token_productions: &mut VecDeque<ProductionId>,
) -> SherpaResult<()> {
  for rule in rules {
    for (sym, tok) in
      rule.symbols.iter().map(|s| (s.id, s.loc.clone())).chain(rule.skipped.iter().map(|s| (*s, Default::default())))
    {
      match sym {
        SymbolId::NonTerminal { id, .. } => match id {
          ProductionId::Standard(..) => {
            production_queue.push_back((tok.clone(), id));
          }
          ProductionId::Sub(..) => {
            // We've already merged in the production's sub-productions
          }
        },
        SymbolId::NonTerminalToken { id, .. } => {
          let name = productions.get(&id.as_parse_prod())?.guid_name;
          let name = ("tk:".to_string() + &name.to_string(s_store)).intern(s_store);
          insert_symbol(symbols, &sym);
          token_names.insert(id.as_parse_prod().as_tok_sym(), name);
          token_productions.push_back(id.as_parse_prod());
        }
        _ => {}
      }
    }
  }
  SherpaResult::Ok(())
}

fn add_prod_name(prod_name_lu: &mut Vec<(IString, IString)>, guid_name: IString, friendly_name: IString) {
  prod_name_lu.push((guid_name, friendly_name));
}

/// Inserts production rule data into the appropriate lookup tables.
fn add_prod_and_rules(
  prod_sym: SymbolId,
  prod_rules: Array<Rule>,
  production_map: &mut OrderedMap<SymbolId, usize>,
  rules: &mut Array<DBRule>,
  prod_rule_map: &mut Array<Array<DBRuleKey>>,
  is_scanner: bool,
) {
  let prod_index = production_map.len();

  let rules_ids = (rules.len() as u32)..(rules.len() + prod_rules.len()) as u32;

  prod_rule_map.push(rules_ids.map(|i| i.into()).collect::<Array<_>>());

  rules.append(&mut prod_rules.into_iter().map(|r| DBRule { rule: r, prod_id: prod_index.into(), is_scanner }).collect());

  let val = production_map.insert(prod_sym, prod_index);

  debug_assert!(val.is_none());
}

fn convert_index_map_to_vec<T, C: IntoIterator<Item = (T, usize)>>(production_map: C) -> Array<T> {
  production_map.into_iter().map(|(k, v)| (v, k)).collect::<OrderedMap<_, _>>().into_values().collect::<Array<_>>()
}

fn insert_symbol(symbol_map: &mut OrderedMap<SymbolId, usize>, sym: &SymbolId) {
  if !symbol_map.contains_key(sym) {
    symbol_map.insert(*sym, symbol_map.len());
  }
}

/// Converts the symbols a set of rules intended for a scanner production.
/// Symbols are converted into individual character/byte values that allow
/// scanner parsers to scan a single character/byte/codepoint at time to
/// recognize a token.
fn convert_symbols_to_scanner_symbols(rules: &mut [Rule], s_store: &IStringStore, is_token: bool) {
  for rule in rules {
    let syms = rule
      .symbols
      .iter()
      .flat_map(|sym_ref| match sym_ref.id {
        SymbolId::Token { val, precedence } => {
          let precedence = (!is_token).then(|| precedence).unwrap_or_default();

          let guarded_str = val.to_string(s_store);
          let string = guarded_str.as_str();

          debug_assert_ne!(string, "");

          let last = string.chars().count() - 1;
          string
            .chars()
            .enumerate()
            .map(|(i, c)| {
              let mut tok_sym_ref = sym_ref.clone();
              if c.is_ascii() {
                let char = c as u8;
                tok_sym_ref.id = SymbolId::Char { char, precedence: precedence };
                tok_sym_ref.original_index = if i == last { sym_ref.original_index } else { 99999 };
              } else {
                let val = c as u32;
                tok_sym_ref.id = SymbolId::Codepoint { precedence: precedence, val };
                tok_sym_ref.original_index = if i == last { sym_ref.original_index } else { 99999 };
              }
              tok_sym_ref
            })
            .collect::<Array<_>>()
        }
        _ => Array::from([sym_ref.clone()]),
      })
      .collect();
    rule.symbols = syms;
  }
}

fn insert_token_production(rules: &mut Vec<Rule>, token_productions: &mut VecDeque<ProductionId>) {
  for rule in rules {
    let syms = rule
      .symbols
      .iter()
      .map(|sym_ref: &SymbolRef| match sym_ref.id {
        SymbolId::NonTerminalToken { id, .. } | SymbolId::NonTerminal { id } => {
          token_productions.push_back(id);
          let mut tok_sym_ref = sym_ref.clone();
          tok_sym_ref.id = id.as_tok_sym();
          tok_sym_ref
        }
        _ => sym_ref.clone(),
      })
      .collect();
    rule.symbols = syms;
  }
}

fn prod_is_immediate_left_recursive(prod_id: ProductionId, rules: &[Rule]) -> bool {
  for rule in rules {
    match rule.symbols[0].id {
      SymbolId::NonTerminal { id, .. } if id == prod_id => {
        //Convert the production right recursive.
        return true;
      }
      _ => {}
    }
  }
  false
}
