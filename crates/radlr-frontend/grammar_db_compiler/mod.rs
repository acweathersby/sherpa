//! After parsing, the grammar's AST must be compiled into a form more suitable
//! for analysis and and transformation. The central idea is to extract,
//! flatten, and unify all data pertinent to these downstream processes.
//! This includes separating unionized non-terminal rule definitions into
//! distinct rules, extracting anonymous rules and lists into dedicated
//! non-terminal definitions, normalizing tokens, dead non-terminal elimination,
//! error identification and reporting, and data compaction.
//!
//! Once this process is completed, the end product is an grammar "database"
//! that allows for quick retrieval of grammar information from flat data
//! structures. All inter-data references become indices into these data
//! structures, and the whole set of grammar data is made immutable for the rest
//! of the compilation pipeline.

mod gather;
mod load;
#[cfg(test)]
mod test;

use self::get_non_term_node::GetNonTermNode;

use super::parser_core::ast;
use radlr_core_common::{CachedString, IString, Map, OrderedMap, OrderedSet, Set, StackVec};
use radlr_rust_runtime::{
  deprecate::RadlrParseError,
  types::{BlameColor, ParserError, Token, TokenRange},
};
use std::{
  collections::{BTreeMap, HashMap, VecDeque},
  fmt::{Debug, Display},
  path::PathBuf,
  sync::Arc,
  vec,
};

use crate::{
  array_vec::ArrayVec,
  parser_core::{
    ast::{ast_Value, nonterm_Value, nonterm_declarations_Value, token_Value, ExportPreamble, NontermDeclaration},
    parse_grammar_input,
  },
  parser_db::DBTokenData,
  types::{
    grammar_object_ids::{GrammarId, GrammarIdentities},
    item::{Item, ItemIndex, ItemType},
    parser_db::{ParserDatabase, RecursionType, ReductionType},
    rule::Rule,
    symbol::SymbolId,
    *,
  },
};

/// Creates PreCompileGramarData from a grammar input. `path` may resolve to a a
/// real source file of the grammar, but this is not necessary. Its primary
/// purpose is to provide a CWD value for resolving relative grammar imports.
pub fn parse_grammar_source(input_str: &str, path: Option<PathBuf>) -> Result<PreCompileGrammarData, GrammarCompilerError> {
  let grammar_ast = parse_grammar_input::<Token>(input_str)?;
  Ok(PreCompileGrammarData {
    grammar_ast: grammar_ast,
    path:        path.clone(),
    cwd_path:    path.map(|p| if p.is_file() { p.parent().unwrap().to_owned() } else { p }),
  })
}

pub fn resolve_grammar_imports(
  source_data: &PreCompileGrammarData,
) -> Result<StackVec<16, PreCompileImportReference>, GrammarCompilerError> {
  const EXTENSIONS: [&str; 3] = ["radlr", "sg", "hcg"];

  let mut imports = StackVec::new();

  for preamble in source_data.grammar_ast.preambles.iter() {
    match preamble {
      ast::preamble_Value::ImportPreamble(import) => {
        let init_length = imports.len();
        let mut import_path = PathBuf::from(&import.uri);
        let mut path_candidates = StackVec::<3, _>::new();

        if import_path.is_relative() {
          if let Some(CWD) = &source_data.cwd_path {
            import_path = CWD.join(import_path)
          } else {
            return Err(GrammarCompilerError::SourceError(
              "Invalid CWD for current source, cannot resolve import".to_string(),
              import.tok.clone(),
            ));
          }
        }

        if import_path.extension().is_none() {
          for ext in EXTENSIONS {
            path_candidates.push(import_path.with_extension(ext))
          }
        } else {
          path_candidates.push(import_path);
        }

        for candidate in path_candidates.iter() {
          match candidate.canonicalize() {
            Ok(path) => {
              let mut ids = GrammarIdentities::from_path(&path);
              ids.local_name = import.reference.intern();
              imports.push(PreCompileImportReference { ids, path: path.intern() });
              break;
            }
            _ => {}
          }
        }

        if init_length == imports.len() {
          return Err(GrammarCompilerError::SourceError(
            "Could not locate the source file for this import".to_string(),
            import.tok.clone(),
          ));
        }
      }
      _ => {}
    }
  }

  Ok(imports)
}

#[derive(Debug, Clone)]
pub struct GrammarDatabase {
  ids:   Vec<GrammarIdentities>,
  rules: Vec<Rule>,
  items: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct IRGrammar {
  ids:       Vec<GrammarIdentities>,
  rules:     HashMap<IString, Vec<Rule>>,
  items:     Vec<usize>,
  nt_lookup: BTreeMap<IString, usize>,
  symbols:   Vec<SymbolResolution>,
  exports:   Vec<(IString, nonterm_Value<Token>)>,
}

#[derive(Debug, Clone)]
struct SymbolResolution {
  sym: ast::repetition_Value<Token>,
  id:  SymbolId,
}

pub fn extract_grammar_components(source_data: &PreCompileGrammarData) -> Result<IRGrammar, GrammarCompilerError> {
  let mut grammar_name = "unnamed".to_token();
  let mut exports = Vec::new();

  let imports = resolve_grammar_imports(source_data)?;

  for preamble in source_data.grammar_ast.preambles.iter() {
    match preamble {
      ast::preamble_Value::ExportPreamble(export) => {
        exports.push((export.reference.to_string().intern(), export.production.clone()));
      }
      ast::preamble_Value::IgnorePreamble(global_ignore) => {}
      ast::preamble_Value::NamePreamble(grammar_name_node) => {
        grammar_name = grammar_name_node.name.intern();
      }
      _ => {}
    }
  }

  let source_path = source_data.path.as_ref().map(|p| p.intern()).unwrap_or(grammar_name);
  let guid_name =
    ("g".to_string() + source_path.as_u64().to_string().as_str() + "_" + grammar_name.to_string().as_str()).intern();

  let ids = GrammarIdentities {
    guid: GrammarId(guid_name),
    guid_name,
    local_name: grammar_name,
    path: source_path,
  };

  let mut g_data = SourceGrammarData {
    ids,
    exports: Default::default(),
    imports: imports.iter().map(|i| i.ids.clone()).collect(),
    grammar: source_data.grammar_ast.clone(),
  };

  if exports.is_empty() {
    for nterm in &g_data.grammar.declarations {
      match nterm {
        ast::nonterm_declarations_Value::NontermDeclaration(decl) => {
          exports.push(("default".intern(), nonterm_Value::NonTerm_Symbol(decl.name_sym.clone())));
          break;
        }
        _ => {}
      }
    }
  }

  let mut nt_lookup: BTreeMap<IString, usize> = BTreeMap::<IString, usize>::new();
  let mut symbols: Vec<SymbolResolution> = vec![];
  let mut pending_rules = VecDeque::new();

  let mut output_rules = HashMap::<IString, Vec<Rule>>::new();

  for decl in &source_data.grammar_ast.declarations {
    match decl {
      nonterm_declarations_Value::IgnoreScope(ignore_scope) => {
        let scope = ignore_scope.clause.symbols.clone();
        for decl in &ignore_scope.definitions {
          // process_ast_declaration(decl, &g_data, &mut rules, &mut nt_lookup,
          // &mut symbols);
        }
      }
      nonterm_declarations_Value::NontermDeclaration(decl) => {
        let (uu_name, friendly_name) = create_nonterm_names(decl.name_sym.val.as_str(), &g_data.ids);
        let index = nt_lookup.len();
        let db_key = DBNonTermKey::from(*nt_lookup.entry(uu_name).or_insert(index) as u32);
        let nonterm = NonTermId {
          db_key,
          friendly_name,
          uu_name,
          is_terminal: false,
          sym: SymbolId::default(),
        };

        for rule in &decl.rules {
          pending_rules.push_back((uu_name, friendly_name, rule.clone()));
        }
      }
      _ => {}
    }
  }

  while let Some((uu_name, friendly_name, rule)) = pending_rules.pop_front() {
    let mut rule_set = vec![(0, Rule::new(rule.tok.clone()))];

    let rule_symbols = rule.symbols.iter().enumerate().map(|(i, s)| (i as u32, s.clone())).collect::<Vec<_>>();

    extract_rules(&rule_symbols, &mut symbols, &mut rule_set, uu_name, friendly_name, &mut pending_rules);

    output_rules.entry(uu_name).or_default().extend(rule_set.into_iter().map(|(_, r)| r));
  }

  // Generate non-terminal tokens

  Ok(IRGrammar {
    ids: vec![ids],
    rules: output_rules,
    items: Vec::new(),
    nt_lookup,
    symbols,
    exports,
  })
}

fn extract_rules(
  input_symbols: &Vec<(u32, ast::rule_group_2_Value<Token>)>,
  output_symbols: &mut Vec<SymbolResolution>,
  rule_set: &mut Vec<(usize, Rule)>,
  uu_name: IString,
  friendly_name: IString,
  pending_rules: &mut VecDeque<(IString, IString, Arc<ast::Rule<Token>>)>,
) {
  let mut index_offset = 0;
  for (original_index, symbol) in input_symbols.iter() {
    let original_index = *original_index + index_offset;
    let active_range = 0..rule_set.len();

    match symbol {
      ast::rule_group_2_Value::Annotated_Symbol(anno) => {
        if anno.is_optional {
          let optional_set = rule_set.clone();
          rule_set.extend(optional_set);
        }

        let tok_prec = anno.precedence.tok_prec as u16;
        let sym_prec = anno.precedence.sym_prec as u16;
        let annotation = anno.reference.to_token();

        match &anno.symbol {
          ast::repetition_Value::TokenGroupRules(group) => {
            // If an ast node is present then we will not be able to merge the anonymous
            // rule bodies into the source grammar. Instead, we create a
            // non-terminal definition which will contain the anonymous rules

            // Create a nonterm_id for this group;

            let (uu_name, friendly_name) = sub_nterm_names("tok_group", uu_name, friendly_name, 0);

            for rule in &group.rules {
              pending_rules.push_back((uu_name, friendly_name, rule.clone()));
            }

            let nonterm_symbol = ast::repetition_Value::NonTerm_Symbol(Arc::new(ast::NonTerm_Symbol {
              val: friendly_name.to_string(),
              tok: Default::default(),
            }));

            let index = output_symbols.len();
            output_symbols.push(SymbolResolution { sym: nonterm_symbol, id: SymbolId::Default });

            let sym_ref = SymbolRef {
              id: SymbolId::Pending(index),
              loc: anno.tok.clone(),
              annotation,
              original_index: original_index as u32,
              tok_prec,
              sym_prec,
            };

            for rule in rule_set[active_range].iter_mut() {
              rule.0 += 1;
              rule.1.symbols.push(sym_ref.clone());
            }
          }
          ast::repetition_Value::Grouped_Rules(group) => {
            // If an ast node is present then we will not be able to merge the anonymous
            // rule bodies into the source grammar. Instead, we create a
            // non-terminal definition which will contain the anonymous rules

            // Create a nonterm_id for this group;
            let (uu_name, friendly_name) = sub_nterm_names("group", uu_name, friendly_name, 0);

            for rule in &group.rules {
              pending_rules.push_back((uu_name, friendly_name, rule.clone()));
            }

            let nonterm_symbol = ast::repetition_Value::NonTerm_Symbol(Arc::new(ast::NonTerm_Symbol {
              val: friendly_name.to_string(),
              tok: Default::default(),
            }));

            let index = output_symbols.len();
            output_symbols.push(SymbolResolution { sym: nonterm_symbol, id: SymbolId::Default });

            let sym_ref = SymbolRef {
              id: SymbolId::Pending(index),
              loc: anno.tok.clone(),
              annotation,
              original_index,
              tok_prec,
              sym_prec,
            };

            for rule in rule_set[active_range].iter_mut() {
              rule.0 += 1;
              rule.1.symbols.push(sym_ref.clone());
            }
          }
          ast::repetition_Value::List_Rules(list) => {
            let (uu_name, friendly_name) = sub_nterm_names("list", uu_name, friendly_name, 0);

            let list_rule = ast::Rule {
              ast:     Arc::new(ast::Ascript { tok: Token::default(), ast: ast_Value::None }),
              symbols: vec![ast::rule_group_2_Value::Annotated_Symbol(Arc::new(ast::Annotated_Symbol {
                is_optional: false,
                tok:         list.tok.clone(),
                precedence:  anno.precedence.clone(),
                reference:   Default::default(),
                symbol:      list.symbol.clone().into(),
              }))],
              tok:     Default::default(),
            };

            let mut other_rule = list_rule.clone();

            match &list.terminal_symbol {
              token_Value::None => {}
              sym => other_rule.symbols.insert(
                0,
                ast::rule_group_2_Value::Annotated_Symbol(Arc::new(ast::Annotated_Symbol {
                  is_optional: false,
                  tok:         list.tok.clone(),
                  precedence:  anno.precedence.clone(),
                  reference:   Default::default(),
                  symbol:      match sym {
                    token_Value::Regex_Symbol(regex) => ast::repetition_Value::Regex_Symbol(regex.clone()),
                    token_Value::Token_Symbol(token) => ast::repetition_Value::Token_Symbol(token.clone()),
                    _ => unreachable!(),
                  },
                })),
              ),
            }

            let nonterm_symbol = ast::repetition_Value::NonTerm_Symbol(Arc::new(ast::NonTerm_Symbol {
              val: friendly_name.to_string(),
              tok: Default::default(),
            }));

            other_rule.symbols.insert(
              0,
              ast::rule_group_2_Value::Annotated_Symbol(Arc::new(ast::Annotated_Symbol {
                is_optional: false,
                tok:         list.tok.clone(),
                precedence:  anno.precedence.clone(),
                reference:   Default::default(),
                symbol:      nonterm_symbol.clone(),
              })),
            );

            pending_rules.push_back((uu_name, friendly_name, Arc::new(list_rule)));
            pending_rules.push_back((uu_name, friendly_name, Arc::new(other_rule)));

            let index = output_symbols.len();
            output_symbols.push(SymbolResolution { sym: nonterm_symbol, id: SymbolId::Default });

            let sym_ref = SymbolRef {
              id: SymbolId::Pending(index),
              loc: anno.tok.clone(),
              annotation,
              original_index,
              tok_prec,
              sym_prec,
            };

            for rule in &mut rule_set[active_range] {
              rule.0 += 1;
              rule.1.symbols.push(sym_ref.clone());
            }
          }

          sym => {
            // Class symbols have default offsets in the database. Map
            // the value of this symbol to that offset
            let index = output_symbols.len();
            output_symbols.push(SymbolResolution { sym: sym.clone(), id: SymbolId::Default });

            let sym_ref = SymbolRef {
              id: SymbolId::Pending(index),
              loc: anno.tok.clone(),
              annotation: "".intern(),
              original_index: original_index as u32,
              tok_prec,
              sym_prec,
            };

            for rule in &mut rule_set[active_range.clone()] {
              rule.0 += 1;
              rule.1.symbols.push(sym_ref.clone());
            }
          }
        }
      }
      ast::rule_group_2_Value::NotEmptySet(set) => {
        fn get_index_permutations(indexes: Vec<usize>) -> Vec<Vec<usize>> {
          if indexes.len() > 1 {
            let mut out = vec![];
            for (i, candidate) in indexes.iter().enumerate() {
              let mut remainder = indexes.clone();
              remainder.remove(i);
              for mut permutation in get_index_permutations(remainder) {
                permutation.insert(0, *candidate);
                out.push(permutation)
              }
            }
            out
          } else {
            vec![indexes]
          }
        }

        let indices = set.symbols.iter().enumerate().map(|(i, _)| i).collect::<Vec<_>>();

        if indices.len() > 4 && set.unordered {
          todo!("Create error/warning about factorial explosions with `[]` sets greater than 4: (5+)! combinatorial explosion")
        }

        index_offset += set.symbols.len() as u32;

        let candidate_symbols = set.symbols.iter().map(|s| (s)).collect::<Vec<_>>();

        let mut new_rules_sets = vec![];

        for permutation in if set.unordered { get_index_permutations(indices) } else { vec![indices] } {
          let input_symbols = permutation
            .iter()
            .map(|i| (original_index + *i as u32, ast::rule_group_2_Value::Annotated_Symbol(candidate_symbols[*i].clone())))
            .collect::<Vec<_>>();

          let mut new_set = rule_set[active_range.clone()].to_vec();

          for (i, _) in &mut new_set {
            *i = 0;
          }

          extract_rules(&input_symbols, output_symbols, &mut new_set, uu_name, friendly_name, pending_rules);

          // Removes rules that have not been extended.
          new_rules_sets.extend(new_set.into_iter().filter(|(i, _)| *i > 0));
        }

        let rest = rule_set[active_range.len()..rule_set.len()].to_vec();

        rule_set.clear();
        rule_set.extend(new_rules_sets);
        rule_set.extend(rest);
      }

      ast::rule_group_2_Value::EOFSymbol(eof) => {
        // add the eof symbol to the active rules.
        for rule in rule_set[active_range].iter_mut() {
          rule.0 += 1;
          rule.1.symbols.push(SymbolRef {
            id: SymbolId::EndOfFile,
            loc: eof.tok.clone(),
            annotation: "".intern(),
            original_index,
            tok_prec: 9999,
            sym_prec: 9999,
          });
        }
      }

      rule => todo!("Processs {rule:?}"),
    }
  }
}

/// Creates a globally unique name and friendly name for a non-terminal
pub fn create_nonterm_names(base_name: &str, g_data: &GrammarIdentities) -> (IString, IString) {
  ((g_data.guid_name.to_string() + "____" + base_name).intern(), base_name.intern())
}

/// Creates a globally unique name and friendly name for a sub-non-terminal.
/// `sub_name` should be a string indicating the type of symbol that
/// the sub-non-terminal was derived from.
fn sub_nterm_names(sub_name: &str, uu_name: IString, friendly_name: IString, index: usize) -> (IString, IString) {
  if index == 0 {
    ((uu_name.to_string() + "_" + sub_name).intern(), (friendly_name.to_string() + "_" + sub_name).intern())
  } else {
    (
      (uu_name.to_string() + "_" + sub_name + "_" + &index.to_string()).intern(),
      (friendly_name.to_string() + "_" + sub_name + "_" + &index.to_string()).intern(),
    )
  }
}

mod get_non_term_node {
  use super::*;
  use crate::types::NonTermId;

  pub enum NonTermSymbol<'a> {
    NonTermImportSymbol(&'a ast::Import_NonTerm_Symbol<Token>),
    NonTermSymbol(&'a ast::NonTerm_Symbol<Token>),
  }

  pub trait GetNonTermNode {
    fn get_nonterm_symbol(&self, g_data: &SourceGrammarData) -> Option<NonTermSymbol<'_>>;

    fn get_nonterm_symbol_id(&self, g_data: &SourceGrammarData) -> Option<(NonTermId, Token)> {
      match self.get_nonterm_symbol(g_data) {
        Some(NonTermSymbol::NonTermImportSymbol(sym)) => {
          for import in g_data.imports.iter() {
            if import.local_name == sym.module.to_token() {
              let (uu, f) = create_nonterm_names(sym.name.as_str(), import);
              return Some((
                NonTermId {
                  db_key:        Default::default(),
                  sym:           SymbolId::DBNonTerminal { key: Default::default() },
                  friendly_name: f,
                  uu_name:       uu,
                  is_terminal:   false,
                },
                sym.tok.clone(),
              ));
            }
          }
          None
        }
        Some(NonTermSymbol::NonTermSymbol(sym)) => {
          let (uu, f) = create_nonterm_names(sym.val.as_str(), &g_data.ids);
          return Some((
            NonTermId {
              db_key:        Default::default(),
              sym:           SymbolId::DBNonTerminal { key: Default::default() },
              friendly_name: f,
              uu_name:       uu,
              is_terminal:   false,
            },
            sym.tok.clone(),
          ));
        }
        _ => None,
      }
    }
  }

  impl GetNonTermNode for ast::ExportPreamble<Token> {
    fn get_nonterm_symbol(&self, g_data: &SourceGrammarData) -> Option<NonTermSymbol<'_>> {
      // self.production.get_nonterm_symbol(g_data)
      None
    }
  }

  impl GetNonTermNode for ast::NontermDeclaration<Token> {
    fn get_nonterm_symbol(&self, g_data: &SourceGrammarData) -> Option<NonTermSymbol<'_>> {
      Some(NonTermSymbol::NonTermSymbol(&self.name_sym))
    }
  }

  impl GetNonTermNode for ast::nonterm_Value<Token> {
    fn get_nonterm_symbol(&self, g_data: &SourceGrammarData) -> Option<NonTermSymbol<'_>> {
      match self {
        Self::NonTerm_Symbol(sym) => Some(NonTermSymbol::NonTermSymbol(sym.as_ref())),
        Self::Import_NonTerm_Symbol(sym) => Some(NonTermSymbol::NonTermImportSymbol(&sym)),
        _ => None,
      }
    }
  }
}

pub fn merge_grammars(grammars: &[&IRGrammar]) -> Result<ParserDatabase, GrammarCompilerError> {
  // Identify the rules that need to be accessible

  let root_grammar = &grammars[0];

  let grammar_ids = grammars.iter().map(|i| i.ids[0].clone()).collect::<Vec<_>>();

  // Build rule list

  let mut nonterm_ids: Vec<NonTermId> = vec![];
  let mut nonterm_rules: Vec<Vec<DBRuleKey>> = vec![];
  let mut known_nonterms = Map::new();
  let mut entry_points = vec![];
  let mut skip_sets = vec![];
  let mut tokens = vec![];

  let mut out_rules = Vec::new();

  for (_, sym) in &root_grammar.exports {
    match sym {
      nonterm_Value::Import_NonTerm_Symbol(sym) => {
        if let Some(id) = root_grammar.ids.iter().find(|i| i.local_name == sym.module.to_token()) {
          if let Some((id_index, grammar)) = grammars.iter().enumerate().find(|(_, g)| g.ids[0].guid == id.guid) {
            merge_rules(
              sym.name.as_str(),
              id_index,
              &grammar,
              grammars,
              &mut out_rules,
              &mut nonterm_ids,
              &mut nonterm_rules,
              &mut known_nonterms,
            );
          } else {
            panic!("Could not find grammar")
          }
        } else {
          panic!("Could not find grammar")
        }
      }
      nonterm_Value::NonTerm_Symbol(sym) => {
        merge_rules(
          sym.val.as_str(),
          0,
          root_grammar,
          grammars,
          &mut out_rules,
          &mut nonterm_ids,
          &mut nonterm_rules,
          &mut known_nonterms,
        );
      }
      _ => {}
    }
  }

  let mut symbol_rules = vec![];
  let mut known_symbols = Map::new();
  let outrules_len = out_rules.len();

  for rule in &mut out_rules {
    let grammar = grammars[rule.g_id_index];

    for symbol_ref in &mut rule.symbols {
      if let SymbolId::Pending(sym_index) = symbol_ref.id {
        let symbol_resolution = &grammar.symbols[sym_index];

        let result = match &symbol_resolution.sym {
          ast::repetition_Value::Class_Symbol(sym) => match sym.val.as_str() {
            "sp" => {
              let (uu_name, friendly_name) = create_nonterm_names("__class_space_chars", &root_grammar.ids[0]);
              let non_term_key = DBNonTermKey(nonterm_ids.len() as u32);

              nonterm_rules.push(vec![]);
              nonterm_ids.push(NonTermId {
                db_key: non_term_key,
                friendly_name,
                uu_name,
                is_terminal: true,
                sym: SymbolId::DBNonTerminal { key: non_term_key },
              });
              nonterm_rules[non_term_key.0 as usize].push(DBRuleKey((outrules_len + symbol_rules.len()) as u32));
              nonterm_rules[non_term_key.0 as usize].push(DBRuleKey((outrules_len + symbol_rules.len() + 1) as u32));

              symbol_rules.extend(vec![
                Rule {
                  nonterm_id: DBNonTermKey(non_term_key.0 as u32),
                  symbols: vec![SymbolRef { id: SymbolId::Char { char: 32 }, ..Default::default() }],
                  tok: sym.tok.clone(),
                  ..Default::default()
                },
                Rule {
                  nonterm_id: DBNonTermKey(non_term_key.0 as u32),
                  symbols: vec![SymbolRef { id: SymbolId::Char { char: 32 }, ..Default::default() }],
                  tok: sym.tok.clone(),
                  ..Default::default()
                },
              ]);
              Some(SymbolId::ClassSpace)
            }
            _ => panic!("Class not recognized"),
          },
          ast::repetition_Value::Token_Symbol(sym) => {
            // Can be built on now
            let (uu_name, friendly_name) = create_nonterm_names(("tok_".to_string() + &sym.val).as_str(), &root_grammar.ids[0]);

            match known_symbols.entry(uu_name) {
              std::collections::hash_map::Entry::Vacant(entry) => {
                let non_term_key = DBNonTermKey(nonterm_ids.len() as u32);

                let tok_data = DBTokenData {
                  name:       friendly_name,
                  nonterm_id: non_term_key,
                  sym_id:     SymbolId::Token { val: friendly_name },
                  tok_id:     DBTermKey(tokens.len() as u16),
                };

                tokens.push(tok_data);

                nonterm_rules.push(vec![]);

                nonterm_ids.push(NonTermId {
                  db_key: non_term_key,
                  friendly_name,
                  uu_name,
                  is_terminal: true,
                  sym: SymbolId::DBNonTerminal { key: non_term_key },
                });
                nonterm_rules[non_term_key.0 as usize].push(DBRuleKey((outrules_len + symbol_rules.len()) as u32));

                // create a production for the token symbol
                symbol_rules.push(Rule {
                  nonterm_id: DBNonTermKey(non_term_key.0 as u32),
                  symbols: sym
                    .val
                    .chars()
                    .into_iter()
                    .map(|char| SymbolRef {
                      id: char
                        .is_ascii()
                        .then_some(SymbolId::Char { char: char as u8 })
                        .unwrap_or(SymbolId::Codepoint { val: char as u32 }),
                      loc: sym.tok.clone(),
                      ..Default::default()
                    })
                    .collect(),
                  tok: sym.tok.clone(),
                  ..Default::default()
                });

                let result = SymbolId::DBToken { key: tok_data.tok_id };

                entry.insert(result);

                Some(result)
              }
              std::collections::hash_map::Entry::Occupied(entry) => Some(*entry.get()),
            }
          }
          ast::repetition_Value::Regex_Symbol(sym) => {
            // Create a set of production rules for the regular expression
            todo!("Build symbol for {sym:#?}");
          }
          ast::repetition_Value::Token_NonTerm_Symbol(sym) => {
            todo!("Build this symbol on grammar merge");
          }
          ast::repetition_Value::TokenGroupRules(sym) => {
            todo!("Build symbol for {sym:#?}");
          }
          _ => Default::default(),
        };

        if let Some(id) = result {
          symbol_ref.id = id;
        } else {
          panic!("Could not resolve symbol");
        }
      }
    }
  }

  out_rules.extend(symbol_rules);

  // Build items
  let mut items = vec![];

  for (rule_index, rule) in out_rules.iter().enumerate() {
    let item_index = ItemIndex::from((DBRuleKey(rule_index as u32), 0));
    items.push(Item {
      index:            item_index,
      from:             item_index,
      len:              rule.symbols.len() as u16,
      goto_distance:    0,
      from_goto_origin: false,
    });
  }

  dbg!(items);

  // Calculate closure for all items, and follow sets and recursion type for all
  // non-terminals.

  let mut item_closures: Vec<Vec<ArrayVec<4, Item>>> = Vec::with_capacity(out_rules.len());
  let mut reduction_types: Vec<ReductionType> = vec![Default::default(); out_rules.len()];
  let mut nonterm_predecessors: OrderedMap<DBNonTermKey, OrderedSet<DBNonTermKey>> = OrderedMap::default();
  let mut recursion_types: Vec<u8> = vec![Default::default(); nonterm_ids.len()];
  let mut follow_items_base: Vec<Vec<ItemIndex>> = vec![Default::default(); nonterm_ids.len()];
  let mut follow_items: Vec<Vec<ItemIndex>> = vec![Default::default(); nonterm_ids.len()];
  let mut nonterm_symbol_to_rules: OrderedMap<DBNonTermKey, OrderedSet<DBRuleKey>> = OrderedMap::default();

  for (index, rule) in out_rules.iter().enumerate() {
    item_closures.insert(index, Vec::with_capacity(rule.symbols.len() + 1))
  }

  for (rule_index, rule) in out_rules.iter().enumerate() {
    let item_index = ItemIndex::from((DBRuleKey(rule_index as u32), 0));
    let sym_len = rule.symbols.len();
    let mut item = Item {
      index:            item_index,
      from:             item_index,
      len:              sym_len as u16,
      goto_distance:    0,
      from_goto_origin: false,
    };

    let non_term = nonterm_ids[rule.nonterm_id.0 as usize];
    let is_scanner = non_term.is_terminal;
    let mode = if is_scanner { GraphType::Scanner } else { GraphType::Parser };

    let mut pending_item: Option<Item> = Some(item);

    while let Some(item) = pending_item {
      pending_item = item.increment();

      match item.get_type(&out_rules) {
        ItemType::TokenNonTerminal(nonterm, _) if is_scanner => follow_items_base[nonterm.0 as usize].push(item.index),
        ItemType::NonTerminal(nonterm) => {
          let val = nonterm_symbol_to_rules.entry(nonterm).or_default();
          val.insert(item.rule_id());
          follow_items_base[nonterm.0 as usize].push(item.index)
        }
        _ => {}
      }

      //-------------------------------------------------------------------------------------------
      // Use completed items to file out the reduce types table.
      if item.is_complete() {
        reduction_types[item.rule_id().0 as usize] = if item.rule_is_left_recursive(mode, &out_rules, &nonterm_ids) {
          ReductionType::LeftRecursive
        } else if rule.ast.is_some() {
          ReductionType::SemanticAction
        } else if item.sym_len() == 1 {
          match item.to_initial().is_term(mode, &out_rules) {
            true => ReductionType::SingleTerminal,
            false => ReductionType::SingleNonTerminal,
          }
        } else {
          ReductionType::Mixed
        };
      };
    }

    //-------------------------------------------------------------------------------------------
    // Calculate closures for uncompleted items.

    fn create_closure(value: Item, mode: GraphType, rules: &[Rule], nonterm_rules: &Vec<Vec<DBRuleKey>>) -> Vec<Item> {
      if let Some(nterm) = value.nonterm_index_at_sym(mode, &rules) {
        if let Some(nt_rules) = nonterm_rules.get(nterm.0 as usize) {
          nt_rules.iter().map(|r| Item::from((*r, rules))).collect()
        } else {
          Default::default()
        }
      } else {
        Default::default()
      }
    }

    let root_nonterm = item.nonterm_index(&out_rules);
    let mut recursion_encountered = false;
    let mut closure = ArrayVec::<4, Item>::from_iter([]);
    let mut queue = VecDeque::from_iter([item]);

    while let Some(kernel_item) = queue.pop_front() {
      if closure.insert_ordered(kernel_item).is_ok() {
        match kernel_item.get_type(&out_rules) {
          ItemType::TokenNonTerminal(nonterm, _) => {
            if is_scanner {
              for item in create_closure(kernel_item, mode, &out_rules, &nonterm_rules) {
                recursion_encountered |= nonterm == root_nonterm;
                queue.push_back(item.align(&kernel_item))
              }
            }
          }
          ItemType::NonTerminal(nonterm) => {
            for item in create_closure(kernel_item, mode, &out_rules, &nonterm_rules) {
              recursion_encountered |= nonterm == root_nonterm;
              queue.push_back(item.align(&kernel_item));

              if root_nonterm != nonterm {
                nonterm_predecessors.entry(nonterm).or_insert(Default::default()).insert(root_nonterm);
              }
            }
          }
          _ => {}
        }
      }
    }

    // Don't include the root item.
    closure.remove(closure.find_ordered(&item).unwrap());

    item_closures.get_mut(item.rule_id().0 as usize).unwrap().insert(item.sym_index() as usize, closure);

    //-------------------------------------------------------------------------------------------
    // Update recursion type
    if recursion_encountered {
      if item.is_initial() {
        recursion_types[root_nonterm.0 as usize] |= RecursionType::LeftRecursive as u8;
      } else {
        recursion_types[root_nonterm.0 as usize] |= RecursionType::RightRecursive as u8;
      }
    }
  }

  let db = ParserDatabase {
    tokens,
    nonterm_rules,
    follow_items,
    nonterm_symbol_to_rules,
    nonterm_ids,
    root_grammar_id: root_grammar.ids[0],
    rules: out_rules,
    entry_points,
    item_closures,
    reduction_types,
    nonterm_predecessors,
    recursion_types,
    skip_sets,
    valid: true,
  };

  Ok(db)
}

///
/// non_term_name
fn merge_rules(
  non_term_name: &str,
  grammar_index: usize,
  source_grammar: &IRGrammar,
  grammars: &[&IRGrammar],
  out_rules: &mut Vec<Rule>,
  nts: &mut Vec<NonTermId>,
  nt_rules: &mut Vec<Vec<DBRuleKey>>,
  known_nts: &mut Map<IString, usize>,
) -> DBNonTermKey {
  let (uu_name, friendly_name) = create_nonterm_names(non_term_name, &source_grammar.ids[0]);
  let nonterm_index = nts.len();

  match known_nts.entry(uu_name) {
    std::collections::hash_map::Entry::Occupied(entry) => {
      return DBNonTermKey(*entry.get() as u32);
    }
    std::collections::hash_map::Entry::Vacant(entry) => {
      let db_key = DBNonTermKey(nonterm_index as u32);
      nts.push(NonTermId {
        db_key,
        uu_name,
        friendly_name,
        is_terminal: false,
        sym: SymbolId::DBNonTerminal { key: db_key },
      });
      entry.insert(nonterm_index);
    }
  };

  nt_rules.push(vec![]);

  // A Non-terminal rule can be declared in any grammar, but the source grammar
  // must contain at least one rule for the set of rule declarations to be
  // valid.
  if let None = source_grammar.nt_lookup.get(&uu_name) {
    panic!("Non-terminal {} not found in source grammar!", friendly_name.to_str().as_str())
  }

  for grammar in grammars {
    if let Some(rules) = grammar.rules.get(&uu_name) {
      for rule in rules {
        let mut rule = rule.clone();
        rule.g_id_index = grammar_index;
        rule.nonterm_id = DBNonTermKey(nonterm_index as u32);

        for symbol_ref in &mut rule.symbols {
          if let SymbolId::Pending(sym_index) = symbol_ref.id {
            let sym = &grammar.symbols[sym_index];

            let nonterm_id = match &sym.sym {
              ast::repetition_Value::Token_NonTerm_Symbol(sym) => match &sym.nonterm {
                nonterm_Value::Import_NonTerm_Symbol(sym) => {
                  Some(merge_import_rule(grammar, sym, grammars, out_rules, nts, nt_rules, known_nts))
                }
                nonterm_Value::NonTerm_Symbol(sym) => {
                  Some(merge_rules(&sym.val, grammar_index, grammar, grammars, out_rules, nts, nt_rules, known_nts))
                }
                _ => todo!("handle token group production"),
              },
              ast::repetition_Value::Import_NonTerm_Symbol(sym) => {
                Some(merge_import_rule(grammar, sym, grammars, out_rules, nts, nt_rules, known_nts))
              }
              ast::repetition_Value::NonTerm_Symbol(sym) => {
                Some(merge_rules(&sym.val, grammar_index, grammar, grammars, out_rules, nts, nt_rules, known_nts))
              }
              _ => None,
            };

            if let Some(nonterm_id) = nonterm_id {
              symbol_ref.id = SymbolId::DBNonTerminal { key: nonterm_id };
            }
          }
        }
        nt_rules[nonterm_index].push(DBRuleKey(out_rules.len() as u32));
        out_rules.push(rule);
      }
    }
  }

  DBNonTermKey(nonterm_index as u32)
}

fn merge_import_rule(
  grammar: &IRGrammar,
  sym: &Arc<ast::Import_NonTerm_Symbol<Token>>,
  grammars: &[&IRGrammar],
  out_rules: &mut Vec<Rule>,
  nonterms: &mut Vec<NonTermId>,
  nt_rules: &mut Vec<Vec<DBRuleKey>>,
  known_nonterms: &mut Map<IString, usize>,
) -> DBNonTermKey {
  if let Some(id) = grammar.ids.iter().find(|i| i.local_name == sym.module.to_token()) {
    if let Some((id_index, grammar)) = grammars.iter().enumerate().find(|(_, g)| g.ids[0].guid == id.guid) {
      merge_rules(sym.name.as_str(), id_index, &grammar, grammars, out_rules, nonterms, nt_rules, known_nonterms)
    } else {
      panic!("Could not find grammar")
    }
  } else {
    panic!("Could not find grammar")
  }
}

pub enum GrammarCompilerError {
  /// Error generated by a RADLR parser
  ParserError(ParserError),
  /// Generated from
  SourceError(String, Token),
  /// Generated from
  SourceErrorWithInline(String, String, Token),
  /// Not implemented
  Todo(&'static str),
}

impl Debug for GrammarCompilerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Display::fmt(&self, f)
  }
}

impl Display for GrammarCompilerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      GrammarCompilerError::ParserError(err) => f.write_fmt(format_args!("{:?}\n", err)),
      GrammarCompilerError::SourceError(error_message, token) => {
        f.write_fmt(format_args!("{error_message}\n{}", token.blame(1, 1, "", BlameColor::RED)))
      }
      GrammarCompilerError::SourceErrorWithInline(error_message, inline, token) => {
        f.write_fmt(format_args!("{error_message}\n{}", token.blame(1, 1, inline, BlameColor::RED)))
      }
      GrammarCompilerError::Todo(note) => {
        f.write_str("Feature not implemented: ")?;
        f.write_str(note)
      }
    }
  }
}

impl From<ParserError> for GrammarCompilerError {
  fn from(value: ParserError) -> Self {
    Self::ParserError(value)
  }
}

#[derive(Debug)]
/// Grammar data generated by a successful parse of a grammar input string.
pub struct PreCompileGrammarData {
  grammar_ast: Arc<ast::GrammarDefinition<Token>>,
  path:        Option<PathBuf>,
  cwd_path:    Option<PathBuf>,
}

#[derive(Debug, Clone, Copy)]
/// Import data for a particular grammar object
struct PreCompileImportReference {
  pub ids: GrammarIdentities,
  /// The absolute path of this importee grammar
  path:    IString,
}

#[derive(Debug)]
/// A fully compiled source data for a specific grammar.
struct SourceGrammarData {
  pub ids:     GrammarIdentities,
  pub imports: StackVec<8, GrammarIdentities>,
  pub exports: StackVec<8, (IString, (NonTermId, Token))>,
  pub grammar: Arc<ast::GrammarDefinition<Token>>,
}

#[derive(Clone, Debug)]
pub struct NonTerminal {
  /// The unique identifier of this non-terminal.
  pub id: NonTermId,

  /// The unique identifier of the owning GrammarHEader.
  pub g_id: GrammarId,

  /// All symbols that are referenced by the rules of the
  /// non-terminal and its sub-nonterminals.
  pub symbols: Vec<SymbolId>,

  /// All rules that reduce to this non-terminal
  pub rules: Vec<Rule>,

  /// Non-terminals generated from the expansion of "non-terminal" type
  /// symbols such as groups & lists. These nonterminals are only referenced
  /// by the rules defined by this non-terminal.
  pub sub_nterms: Vec<Box<SubNonTerminal>>,

  /// Non-terminals derived from `tk:` invocations of normal nonterminals.
  /// These nonterminals have the special characteristic where none of
  /// their rules contain left recursions
  pub tok_nterms: Vec<Box<SubNonTerminal>>,

  /// The type of this non-terminal
  pub type_: NonTermType,

  /// The globally unique name string of the non-terminal. Similar to a C++
  /// mangled name
  pub guid_name: IString,

  /// The name of the non-terminal as it is found in the source grammar.
  pub friendly_name: IString,

  pub tok: Token,
  //pub asts: Vec<Box<parser::Ascript>>,
}

/// Non-terminals generated from the expansion of "non-terminal" type
/// symbols such as groups & lists. These nonterminals are only referenced
/// by the rules defined by this non-terminal.
#[derive(Clone, Debug)]
pub struct SubNonTerminal {
  pub id: NonTermId,

  pub g_id: GrammarId,

  /// The globally unique name string of the non-terminal. Similar to a C++
  /// mangled name
  pub guid_name: IString,

  /// The name of the non-terminal as it is found in the source grammar.
  pub friendly_name: IString,

  pub rules: Vec<Rule>,

  pub type_: SubNonTermType,
}

/// Types of [SubNonTerminal]s that may be derived from rule symbols.
#[derive(Clone, Debug)]
pub enum SubNonTermType {
  /// List sub nonterminals are left recursive nonterminals
  /// that are derived from `list` symbols e.g: `A(+) | A(*) | A(+sym) |
  /// A(*sym)` .
  List,
  /// Group nonterminals are derived from group symbols e.g `(...)` and are
  /// created when they are present in rules that have AST definitions to
  /// maintain expected behaviors when referencing symbols in an ast
  /// expression.
  Group,
}

/// A reference to some Ascript AST data that is either automatically generated
/// depending on the reference type, or is stored on a NonTerminal node.
#[derive(Clone, Debug)]
pub enum ASTToken {
  /// Represents the ast expression `:ast [ $1 ]`.
  ///
  ///
  /// Automatically generated when when a list non-terminal (` A(+) | A(*) `) is
  /// processed.
  ListEntry(TokenRange),
  /// Represents the ast expression `:ast [ $1, $--last-- ]`, where `--last--`
  /// represents the last symbol in a rule.
  ///
  /// Automatically generated when a list non-terminal (` A(+) | A(*) `) is
  /// processed.
  ListIterate(TokenRange),
  /// An AST expression defined within a grammar. `0` Is the non-terminal id
  /// in which a copy if the AST expressions is stored. `1` is the index
  /// into the Non-terminals's `asts` array for that stored non-terminal.
  Defined(Arc<ast::Ascript<Token>>),
}
