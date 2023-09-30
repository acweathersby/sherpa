use sherpa_rust_runtime::types::{Token, TokenRange};

use crate::{
  journal::Journal,
  parser,
  types::{
    error_types::{invalid_nonterminal_alias, missing_nonterminal_rules},
    *,
  },
  utils::{create_u64_hash, hash_group_btreemap},
};
use std::collections::{btree_map, VecDeque};

pub(crate) fn build_compile_db<'a>(mut j: Journal, g: GrammarIdentities, gs: &'a GrammarSoup) -> SherpaResult<ParserDatabase> {
  // Gain read access to all parts of the GrammarCloud.
  // We don't want anything changing during these next steps.

  j.set_active_report("Database Compile", crate::ReportType::GrammarCompile(g.guid));

  let mut is_valid = true;

  let GrammarSoup {
    grammar_headers, nonterminals, string_store: s_store, custom_states, ..
  } = gs;

  let custom_states = custom_states.read()?;

  let nonterminals: std::sync::RwLockReadGuard<'_, Vec<Box<NonTerminal>>> = nonterminals.read()?;

  let nonterminals = {
    let mut out_nterms = OrderedMap::new();
    for nterm in nonterminals.iter() {
      match out_nterms.entry(nterm.id) {
        btree_map::Entry::Vacant(e) => {
          e.insert(nterm.clone());
        }
        btree_map::Entry::Occupied(mut e) => {
          fn remap_sub_nterm_offsets(rule: &mut Rule, sub_nterm_offset: usize) {
            for sym_ref in &mut rule.symbols {
              match sym_ref.id {
                SymbolId::NonTerminal { id } => {
                  if let NonTermId::Sub(p, i, s) = id {
                    sym_ref.id = SymbolId::NonTerminal { id: NonTermId::Sub(p, i + sub_nterm_offset as u32, s) };
                  }
                }
                _ => {}
              }
            }
          }

          let existing_prod = e.get_mut();
          let sub_nterm_offset = existing_prod.sub_nterms.len();

          for mut rule in nterm.rules.clone() {
            remap_sub_nterm_offsets(&mut rule, sub_nterm_offset);
            existing_prod.rules.push(rule);
          }

          for mut sub_prod in nterm.sub_nterms.clone() {
            if let NonTermId::Sub(p, i, s) = sub_prod.id {
              sub_prod.id = NonTermId::Sub(p, i + sub_nterm_offset as u32, s);
            }

            for rule in &mut sub_prod.rules {
              remap_sub_nterm_offsets(rule, sub_nterm_offset);
            }

            existing_prod.sub_nterms.push(sub_prod);
          }

          existing_prod.symbols.extend(nterm.symbols.iter());
        }
      };
    }
    out_nterms
  };

  let grammar_headers = grammar_headers.read()?;
  let root_grammar = o_to_r(grammar_headers.get(&g.guid), "Could not find grammar")?.as_ref();

  // Build non-terminal list.
  let mut symbols = OrderedMap::from_iter(vec![(SymbolId::Default, 0)]);
  let mut nonterminal_queue =
    Queue::from_iter(root_grammar.pub_nterms.iter().map(|(_, p)| (p.1.clone(), root_grammar.identity.path, p.0)));

  // Maps non-terminal sym IDs to indices.
  let mut nterm_map_owned = OrderedMap::new();
  let p_map = &mut nterm_map_owned;

  // Stores all used rules
  let mut rule_table_owned: Array<DBRule> = Array::new();
  let r_table = &mut rule_table_owned;

  // Maps nterm indices to rules.
  let mut p_r_map_owned = Array::new();
  let p_r_map = &mut p_r_map_owned;

  // Friendly names for nonterminals
  let mut nterm_name_lu_owned = Array::new();
  let nterm_name_lu = &mut nterm_name_lu_owned;

  // Processed Custom State
  let mut c_states_owned = Array::new();
  let c_states = &mut c_states_owned;

  // Keeps track of the names of token_nonterminals.
  let mut token_names = OrderedMap::new();
  let mut token_nonterminals = VecDeque::new();

  while let Some((loc, path, nterm_id)) = nonterminal_queue.pop_front() {
    if !p_map.contains_key(&nterm_id.as_sym()) {
      match (custom_states.get(&nterm_id), nonterminals.get(&nterm_id)) {
        (None, Some(nterm)) => {
          let g_name = nterm.guid_name;
          let f_name = nterm.friendly_name;
          let rules = nterm.rules.clone();

          add_nterm_and_rules(nterm_id.as_sym(), rules, p_map, r_table, p_r_map, false);
          add_nterm_name(nterm_name_lu, g_name, f_name);
          add_empty_custom_state(c_states);

          // Gain references on all sub nonterminals. ----------------------------
          for nterm in &nterm.sub_nterms {
            let nterm_id = nterm.id;

            if !p_map.contains_key(&nterm_id.as_sym()) {
              let rules = nterm.rules.clone();
              let g_name = nterm.guid_name;
              let f_name = nterm.friendly_name;

              add_nterm_and_rules(nterm_id.as_sym(), rules, p_map, r_table, p_r_map, false);
              add_nterm_name(nterm_name_lu, g_name, f_name);
              add_empty_custom_state(c_states);

              extract_nterm_syms(
                &nterm.rules,
                &mut nonterminal_queue,
                &nonterminals,
                s_store,
                &mut symbols,
                &mut token_names,
                &mut token_nonterminals,
              )?;
            }
          }

          for sym in nterm.symbols.iter() {
            insert_symbol(&mut symbols, sym);
          }

          extract_nterm_syms(
            &nterm.rules,
            &mut nonterminal_queue,
            &nonterminals,
            s_store,
            &mut symbols,
            &mut token_names,
            &mut token_nonterminals,
          )?;
        }
        (Some(state), None) => {
          let g_name = state.guid_name;
          let f_name = state.friendly_name;

          nonterminal_queue.extend(state.nterm_refs.iter().cloned());
          add_nterm_and_rules(nterm_id.as_sym(), vec![], p_map, r_table, p_r_map, false);
          add_nterm_name(nterm_name_lu, g_name, f_name);
          add_custom_state(state.state.clone(), c_states);
        }
        (Some(_), Some(_)) => {
          j.report_mut().add_error(invalid_nonterminal_alias(loc, path, s_store));
          is_valid = false;
        }
        _ => {
          j.report_mut().add_error(missing_nonterminal_rules(loc, path, s_store));
          is_valid = false;
        }
      };
    }
  }

  if !is_valid {
    return Ok(ParserDatabase::default());
  }

  // Generate token nonterminals -----------------------------------------------
  while let Some(nterm) = token_nonterminals.pop_front() {
    // Convert any left immediate recursive rules to right.
    if !p_map.contains_key(&nterm.as_tok_sym()) {
      let (internal_id, rules, name) = match nterm {
        NonTermId::Standard(internal_id, ..) => {
          let nterm =
            o_to_r(nonterminals.get(&NonTermId::Standard(internal_id, NonTermSubType::Parser)), "Could not find non-terminal")?;
          (internal_id, &nterm.rules, nterm.guid_name)
        }
        NonTermId::Sub(internal_id, index, ..) => {
          let nterm =
            o_to_r(nonterminals.get(&NonTermId::Standard(internal_id, NonTermSubType::Parser)), "Could not find non-terminal")?;
          let nterm = &nterm.sub_nterms[index as usize];
          (internal_id, &nterm.rules, nterm.guid_name)
        }
      };

      let name = "tk_".to_string() + &name.to_string(s_store).as_str();
      let name = name.intern(s_store);

      if nterm_is_immediate_left_recursive(nterm, &rules) {
        let rules = rules.clone();

        let prime_id = NonTermId::Sub(internal_id, p_map.len() as u32 + 9000, NonTermSubType::Scanner);

        let mut groups = hash_group_btreemap(rules, |_, r| match r.symbols[0].id {
          SymbolId::NonTerminal { id, .. } if id == nterm => true,
          _ => false,
        });

        let prime_rules = o_to_r(groups.remove(&true), "")?;
        let mut r_rules = o_to_r(groups.remove(&false), "")?;

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

        insert_token_nonterminal(&mut r_rules, &mut token_nonterminals);
        insert_token_nonterminal(&mut p_rules, &mut token_nonterminals);

        convert_sym_refs_to_token_sym_refs(&mut r_rules, s_store, 0);
        convert_sym_refs_to_token_sym_refs(&mut p_rules, s_store, 0);

        let nterm = nterm.as_tok_sym();
        let prime_id = prime_id.as_tok_sym();

        add_nterm_and_rules(nterm, r_rules, p_map, r_table, p_r_map, true);
        add_nterm_name(nterm_name_lu, name, name);
        add_empty_custom_state(c_states);

        let name = (name.to_string(s_store) + "_prime").intern(s_store);
        add_nterm_and_rules(prime_id, p_rules, p_map, r_table, p_r_map, true);
        add_nterm_name(nterm_name_lu, name, name);
        add_empty_custom_state(c_states);
      } else {
        let nterm = nterm.as_tok_sym();
        if !p_map.contains_key(&nterm) {
          let mut rules = rules.clone();

          convert_sym_refs_to_token_sym_refs(&mut rules, s_store, 0);
          insert_token_nonterminal(&mut rules, &mut token_nonterminals);

          add_nterm_and_rules(nterm, rules, p_map, r_table, p_r_map, true);
          add_nterm_name(nterm_name_lu, name, name);
          add_empty_custom_state(c_states);
        }
      }
    }
  }

  // Generate scanner nonterminals and symbol indices --------------------------
  for sym in symbols.keys() {
    if sym.is_default() {
      continue;
    }

    if !p_map.contains_key(&sym.to_scanner_nterm().as_tok_sym()) {
      match sym {
        SymbolId::Default => {}

        SymbolId::Token { val } => {
          let nterm = sym.to_scanner_nterm();

          let s = &val.to_string(s_store);
          let o = { "<> \"".to_string() + s + "\" > " + s };

          let tok = TokenRange {
            len:      s.len() as u32,
            off:      s.len() as u32 + 8,
            line_num: 0,
            line_off: 0,
          }
          .to_token_with_string(&o);

          let mut rules = Array::from([Rule {
            symbols: Array::from([SymbolRef { id: *sym, ..Default::default() }]),
            skipped: Default::default(),
            ast: None,
            tok,
            g_id: g,
          }]);

          convert_sym_refs_to_token_sym_refs(&mut rules, s_store, 0);

          let name = ("tok_".to_string() + &create_u64_hash(val).to_string()).intern(s_store);

          add_nterm_and_rules(nterm.as_tok_sym(), rules, p_map, r_table, p_r_map, true);

          add_nterm_name(nterm_name_lu, name, name);
          add_empty_custom_state(c_states);

          token_names.insert(nterm.as_tok_sym(), *val);
        }

        sym if sym.is_term() => {
          let nterm = sym.to_scanner_nterm();
          let rules = Array::from_iter(vec![Rule {
            symbols: vec![SymbolRef {
              id: *sym,
              token_precedence: sym.base_precedence(),
              ..Default::default()
            }],
            skipped: Default::default(),
            ast:     None,
            tok:     Token::default(),
            g_id:    g,
          }]);

          let name = ("sym_".to_string() + &create_u64_hash(sym).to_string()).intern(s_store);

          add_nterm_and_rules(nterm.as_tok_sym(), rules, p_map, r_table, p_r_map, true);

          add_nterm_name(nterm_name_lu, name, name);
          add_empty_custom_state(c_states);
        }
        _ => {}
      }
    }
  }

  let tokens = convert_index_map_to_vec(symbols.iter().map(|(sym, index)| {
    (
      DBTokenData {
        nonterm_id: if sym.is_default() {
          Default::default()
        } else {
          let nterm = sym.to_scanner_nterm().as_tok_sym();
          p_map.get(&nterm).map(|i| DBNonTermKey::from(*i)).unwrap_or_default()
        },
        sym_id:     *sym,
        name:       {
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
              let name = nonterminals.get(&id.as_parse_prod()).unwrap().friendly_name.to_string(s_store);

              ("<".to_string() + &name + ">").intern(s_store)
            }
            Codepoint { val } => val.to_string().intern(s_store),
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
        tok_id:     (*index).into(),
      },
      *index,
    )
  }));

  // Convert convert GUID symbol ids to local indices. ------------------------
  convert_rule_symbol_ids(r_table, p_map, symbols);

  let nterm_lu = convert_index_map_to_vec(p_map.clone());

  let entry_points = root_grammar
    .pub_nterms
    .iter()
    .enumerate()
    .map(|(id, (name, (nterm, _)))| {
      if let Some(nterm_name) = nonterminals
        .get(nterm)
        .and_then(|d| Some(d.guid_name))
        .or_else(|| custom_states.get(nterm).and_then(|d| Some(d.guid_name)))
      {
        EntryPoint {
          nonterm_key:        DBNonTermKey::from(*p_map.get(&nterm.as_sym()).unwrap()),
          entry_name:         *name,
          nonterm_name:       nterm_name,
          nonterm_entry_name: (nterm_name.to_string(s_store) + "_entry").intern(s_store),
          nonterm_exit_name:  (nterm_name.to_string(s_store) + "_exit").intern(s_store),
          export_id:          id,
          is_export:          true,
        }
      } else {
        todo!("Handle missing entry point")
      }
    })
    .chain(
      nterm_lu
        .iter()
        .enumerate()
        .filter_map(|(index, n)| match n {
          SymbolId::NonTerminal { id } => Some(index),
          _ => None,
        })
        .filter_map(|nterm_id| {
          if let Some((guid_name, _)) = nterm_name_lu_owned.get(nterm_id) {
            let nterm_name = guid_name;
            Some(EntryPoint {
              nonterm_key:        DBNonTermKey::from(nterm_id),
              entry_name:         *guid_name,
              nonterm_name:       *guid_name,
              nonterm_entry_name: (nterm_name.to_string(s_store) + "_entry").intern(s_store),
              nonterm_exit_name:  (nterm_name.to_string(s_store) + "_exit").intern(s_store),
              export_id:          Default::default(),
              is_export:          false,
            })
          } else {
            None
          }
        }),
    )
    .collect::<Array<_>>();

  let db = ParserDatabase::new(
    root_grammar.identity.guid_name,
    nterm_lu,
    nterm_name_lu_owned,
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
fn convert_rule_symbol_ids(
  r_table: &mut Vec<DBRule>,
  p_map: &mut OrderedMap<SymbolId, usize>,
  symbols: OrderedMap<SymbolId, usize>,
) {
  for DBRule { rule, is_scanner, .. } in r_table {
    for SymbolRef { id: sym, original_index: _index, .. } in &mut rule.symbols {
      *sym = convert_symbol_into_db_symbol(sym, p_map, &symbols, *is_scanner);
    }

    if !*is_scanner {
      for sym in &mut rule.skipped {
        *sym = convert_symbol_into_db_symbol(sym, p_map, &symbols, *is_scanner);
      }
    }
  }
}

/// Converts an AST symbol into a DB symbol.
fn convert_symbol_into_db_symbol(
  sym: &SymbolId,
  p_map: &mut OrderedMap<SymbolId, usize>,
  symbols: &OrderedMap<SymbolId, usize>,
  is_scanner: bool,
) -> SymbolId {
  match *sym {
    SymbolId::NonTerminalToken { id } => {
      if is_scanner {
        let index = p_map.get(&id.as_parse_prod().as_tok_sym()).unwrap();
        SymbolId::DBNonTerminal { key: (*index as u32).into() }
      } else {
        let index = p_map.get(&sym).unwrap();
        SymbolId::DBNonTerminalToken {
          nonterm_key: (*index as u32).into(),
          sym_key:     symbols.get(sym).map(|i| (*i as u32).into()),
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

fn extract_nterm_syms(
  rules: &[Rule],
  nonterminal_queue: &mut VecDeque<(Token, IString, NonTermId)>,
  nonterminals: &OrderedMap<NonTermId, Box<NonTerminal>>,
  s_store: &IStringStore,
  symbols: &mut OrderedMap<SymbolId, usize>,
  token_names: &mut OrderedMap<SymbolId, IString>,
  token_nonterminals: &mut VecDeque<NonTermId>,
) -> SherpaResult<()> {
  for rule in rules {
    for (sym, tok) in
      rule.symbols.iter().map(|s| (s.id, s.loc.clone())).chain(rule.skipped.iter().map(|s| (*s, Default::default())))
    {
      match sym {
        SymbolId::NonTerminal { id, .. } => match id {
          NonTermId::Standard(..) => {
            nonterminal_queue.push_back((tok.clone(), rule.g_id.path, id));
          }
          NonTermId::Sub(..) => {
            // We've already merged in the non-terminal's sub-nonterminals
          }
        },
        SymbolId::NonTerminalToken { id, .. } => {
          let name = o_to_r(nonterminals.get(&id.as_parse_prod()), "Non-terminal not found")?.guid_name;
          let name = ("tk:".to_string() + &name.to_string(s_store)).intern(s_store);
          insert_symbol(symbols, &sym);
          token_names.insert(id.as_parse_prod().as_tok_sym(), name);
          token_nonterminals.push_back(id.as_parse_prod());
        }
        _ => {}
      }
    }
  }
  SherpaResult::Ok(())
}

fn add_nterm_name(nterm_name_lu: &mut Vec<(IString, IString)>, guid_name: IString, friendly_name: IString) {
  nterm_name_lu.push((guid_name, friendly_name));
}

/// Inserts non-terminal rule data into the appropriate lookup tables.
fn add_nterm_and_rules(
  nterm_sym: SymbolId,
  nterm_rules: Array<Rule>,
  nonterminal_map: &mut OrderedMap<SymbolId, usize>,
  rules: &mut Array<DBRule>,
  nterm_rule_map: &mut Array<Array<DBRuleKey>>,
  is_scanner: bool,
) {
  let nonterm_index = nonterminal_map.len();

  let rules_ids = (rules.len() as u32)..(rules.len() + nterm_rules.len()) as u32;

  nterm_rule_map.push(rules_ids.map(|i| i.into()).collect::<Array<_>>());

  rules.append(&mut nterm_rules.into_iter().map(|r| DBRule { rule: r, nonterm: nonterm_index.into(), is_scanner }).collect());

  let val = nonterminal_map.insert(nterm_sym, nonterm_index);

  debug_assert!(val.is_none());
}

fn convert_index_map_to_vec<T, C: IntoIterator<Item = (T, usize)>>(nonterminal_map: C) -> Array<T> {
  nonterminal_map.into_iter().map(|(k, v)| (v, k)).collect::<OrderedMap<_, _>>().into_values().collect::<Array<_>>()
}

fn insert_symbol(symbol_map: &mut OrderedMap<SymbolId, usize>, sym: &SymbolId) {
  if !symbol_map.contains_key(sym) {
    symbol_map.insert(*sym, symbol_map.len());
  }
}

/// Converts the symbols a set of rules intended for a scanner non-terminal.
/// Symbols are converted into individual character/byte values that allow
/// scanner parsers to scan a single character/byte/codepoint at time to
/// recognize a token.
fn convert_sym_refs_to_token_sym_refs(rules: &mut [Rule], s_store: &IStringStore, _precedence: u16) {
  for rule in rules {
    let syms = rule
      .symbols
      .iter()
      .flat_map(|sym_ref| match sym_ref.id {
        SymbolId::Token { val } => {
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
                tok_sym_ref.id = SymbolId::Char { char };
                tok_sym_ref.original_index = if i == last { sym_ref.original_index } else { 99999 };
              } else {
                let val = c as u32;
                tok_sym_ref.id = SymbolId::Codepoint { val };
                tok_sym_ref.original_index = if i == last { sym_ref.original_index } else { 99999 };
              }
              tok_sym_ref.token_precedence = sym_ref.token_precedence;
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

fn insert_token_nonterminal(rules: &mut Vec<Rule>, token_nonterminals: &mut VecDeque<NonTermId>) {
  for rule in rules {
    let syms = rule
      .symbols
      .iter()
      .map(|sym_ref: &SymbolRef| match sym_ref.id {
        SymbolId::NonTerminalToken { id, .. } | SymbolId::NonTerminal { id } => {
          token_nonterminals.push_back(id);
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

fn nterm_is_immediate_left_recursive(nterm: NonTermId, rules: &[Rule]) -> bool {
  for rule in rules {
    match rule.symbols[0].id {
      SymbolId::NonTerminal { id, .. } if id == nterm => {
        //Convert the non-terminal right recursive.
        return true;
      }
      _ => {}
    }
  }
  false
}
