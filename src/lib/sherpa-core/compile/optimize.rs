use crate::{
  hash_group_btreemap,
  hash_id_value_u64,
  parser::{
    self,
    ASTNode,
    ASTNodeType,
    DefaultMatch,
    GetASTNodeType,
    Gotos,
    IntMatch,
    Matches,
    NonTermMatch,
    ProductionMatches,
    Statement,
    TermMatch,
    TerminalMatches,
  },
  types::*,
};
use sherpa_rust_runtime::types::bytecode::MatchInputType;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

type Map<A, B> = BTreeMap<A, B>;
type Set<A> = BTreeSet<A>;

/// Performance various transformation on the parse state graph
/// to reduce number of steps between transient actions, and to
/// reduce the number of parse states overall.
pub(crate) fn optimize<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  parse_states: ParseStatesMap,
  optimize_for_debugging: bool,
) -> SherpaResult<R> {
  let start_complexity = ComplexityMarker::new(db, parse_states.iter());

  //return garbage_collect(db, parse_states, "initial purge");

  let parse_states = garbage_collect(db, parse_states, "initial purge")?;

  let parse_states = canonicalize_states(db, parse_states, None)?;

  let parse_states = merge_branches(db, parse_states)?;

  let parse_states = combine_state_branches(db, parse_states)?;

  let parse_states = canonicalize_states(db, parse_states, Some("state combine"))?;

  //let parse_states = _inline_states(db, parse_states)?;

  let parse_states = inline_scanners(db, parse_states)?;

  //let parse_states = _create_byte_sequences(db, parse_states)?;

  let parse_states = merge_branches(db, parse_states)?;

  let parse_states = remove_redundant_defaults(db, parse_states)?;

  let parse_states = canonicalize_states(db, parse_states, Some("state combine"))?;

  let parse_states = combine_state_branches(db, parse_states)?;

  //
  // let parse_states = inline_matches(db, parse_states)?;

  let parse_states: ParseStatesMap = parse_states;

  start_complexity.print_comparison(&ComplexityMarker::new(db, parse_states.iter()), "Total optimization result");

  if optimize_for_debugging {
    // Can only do this once we can map scanner ids back to match statements
    // otherwise scanner states will be dropped.
    let parse_states = parse_states.into_iter().map(|(name, state)| {
      let mut state = state.remap_source(db).unwrap_or(Default::default());
      if let Err(err) = state.build_ast(db) {
        todo!("Failed to build AST {}", err)
      }
      (name, Box::new(state))
    });

    garbage_collect(db, parse_states.collect(), "debug-cleanup")
  } else {
    garbage_collect(db, parse_states, "cleanup")
  }
}

struct ComplexityMarker {
  num_of_states:   usize,
  code_complexity: f64,
}

impl ComplexityMarker {
  pub fn new<'i, I: Iterator<Item = (&'i IString, &'i Box<ParseState>)>>(db: &ParserDatabase, states: I) -> Self {
    let (num_of_states, code_complexity) = states
      .enumerate()
      .map(|(i, (_, s))| (i, s.print(db, false).unwrap_or_default().len()))
      .fold((0, 0), |(a, c), (b, d)| (a.max(b), c + d));
    Self { num_of_states, code_complexity: code_complexity as f64 }
  }

  pub fn print_comparison(&self, other: &Self, label: &str) {
    println!(
      "Opt {} ---- {} -> {} State Reduction: {}% Complexity Reduction: {}%",
      label,
      self.num_of_states,
      other.num_of_states,
      ((1.0 - other.num_of_states as f64 / self.num_of_states as f64) * 100.0).round(),
      ((1.0 - other.code_complexity / self.code_complexity) * 100.0).round()
    );
  }
}

fn get_match_statement_mut(node: &mut ASTNode) -> Option<&mut parser::Statement> {
  match node {
    ASTNode::TermMatch(box TermMatch { statement, .. })
    | ASTNode::DefaultMatch(box DefaultMatch { statement, .. })
    | ASTNode::IntMatch(box IntMatch { statement, .. })
    | ASTNode::NonTermMatch(box NonTermMatch { statement, .. }) => Some(statement.as_mut()),
    _ => None,
  }
}

fn get_match_statement(node: &ASTNode) -> Option<&parser::Statement> {
  match node {
    ASTNode::TermMatch(box TermMatch { statement, .. })
    | ASTNode::DefaultMatch(box DefaultMatch { statement, .. })
    | ASTNode::IntMatch(box IntMatch { statement, .. })
    | ASTNode::NonTermMatch(box NonTermMatch { statement, .. }) => Some(statement.as_ref()),
    _ => None,
  }
}

/// Inline trivial scanners.
fn _inline_matches<'db>(db: &'db ParserDatabase, parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  garbage_collect(db, parse_states, "byte-chains")
}

/// Remove default branches that transition to match states that are identical
/// to the root branches.
fn remove_redundant_defaults<'db>(db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  // Get a reference to all root level branches.

  let mut state_branch_lookup: Map<IString, Set<(u64, String, u64)>> = Map::new();

  for (name, state) in &parse_states {
    if let Some(box parser::State { statement, .. }) = state.ast.as_ref() {
      let mut queue = VecDeque::from_iter([statement.as_ref()]);
      let def_matches = state_branch_lookup.entry(*name).or_insert(Default::default());

      while let Some(statement) = queue.pop_front() {
        let parser::Statement { transitive, non_branch, branch, .. } = statement;

        if transitive.is_none() && non_branch.is_empty() {
          if let Some(parser::Matches { matches, mode, .. }) = branch.as_ref().and_then(|b| b.as_Matches()) {
            for m in matches {
              match m {
                ASTNode::IntMatch(box parser::IntMatch { statement, vals }) => {
                  for val in vals {
                    def_matches.insert((
                      *val,
                      mode.to_string(),
                      hash_id_value_u64(print_IR(&ASTNode::Statement(statement.clone()), db)?),
                    ));
                  }
                }
                ASTNode::DefaultMatch(box parser::DefaultMatch { statement }) => queue.push_back(statement.as_ref()),
                _ => {}
              }
            }
          }
        }
      }
    }
  }

  fn remove_redundant_defaults<'db>(
    db: &'db ParserDatabase,
    state: IString,
    root_statement: &mut parser::Statement,
    state_branch_lookup: &Map<IString, Set<(u64, String, u64)>>,
    info: &String,
  ) -> bool {
    if let Some(parser::Matches { matches, .. }) = root_statement.branch.as_mut().and_then(|m| m.as_Matches_mut()) {
      let leaf_default = matches.iter_mut().filter(|m| m.get_type() == ASTNodeType::DefaultMatch).next();
      if let Some(ASTNode::DefaultMatch(box parser::DefaultMatch { statement })) = leaf_default {
        if remove_redundant_defaults(db, state, statement, state_branch_lookup, info) {
          let clone = matches.clone();
          matches.clear();
          matches.append(&mut clone.into_iter().filter(|d| d.get_type() != ASTNodeType::DefaultMatch).collect());
        }
        return false;
      }
    }

    let statement = root_statement;

    if statement.transitive.is_some() || !statement.non_branch.is_empty() {
      return false;
    };

    let Some(parser::Gotos { goto, pushes }) = statement.branch.as_mut().and_then(|s| s.as_Gotos_mut()) else {
      return false;
    };

    if pushes.len() > 0 {
      return false;
    }

    let goto = goto.name.to_token();

    let Some(own) = state_branch_lookup.get(&state) else {
      return false;
    };

    let Some(theirs) = state_branch_lookup.get(&goto) else {
      return false;
    };

    return theirs.is_subset(own);
  }

  for (state_id, state) in &mut parse_states {
    let info = state.print(db, true)?;
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      remove_redundant_defaults(db, *state_id, statement.as_mut(), &state_branch_lookup, &info);
    }
  }

  garbage_collect(db, parse_states, "redundant-defaults")
}

/// Create chained matching scanners that can scan sequences of
/// characters simultaneously.
fn _create_byte_sequences<'db>(db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  let mut single_byte_state = HashMap::new();

  for (name, state) in &parse_states {
    // Only statements with branches that are not Matches can be considered as
    // naked.
    if let Some(box parser::State { statement, .. }) = state.ast.as_ref() {
      if let Some(ASTNode::Matches(box parser::Matches { mode, matches, .. })) = &statement.branch {
        if matches!(mode.as_str(), MatchInputType::BYTE_STR | MatchInputType::CODEPOINT_STR) && matches.len() == 1 {
          if let Some(ASTNode::IntMatch(int_match)) = matches.first() {
            single_byte_state.insert(*name, (**int_match).clone());
          }
        }
      }
    }
  }

  for (_, state) in &mut parse_states {
    let Some(box parser::State { statement, .. }) = state.ast.as_mut() else { continue };

    let Some(ASTNode::Matches(box parser::Matches { mode, matches, .. })) = &mut statement.branch else { continue };

    if !matches!(mode.as_str(), MatchInputType::BYTE_STR | MatchInputType::CODEPOINT_STR) {
      continue;
    }

    if matches.len() > 2 || (matches.len() == 2 && !matches.iter().any(|m| m.get_type() == ASTNodeType::DefaultMatch)) {
      continue;
    };

    let Some(first_match) = matches.iter_mut().filter(|m| m.get_type() == ASTNodeType::IntMatch).next() else { continue };

    let ASTNode::IntMatch(box parser::IntMatch { statement, vals }) = first_match else { continue };

    let mut iter = 0;
    let mut new_vals = vals.clone();
    let mut active_statement = statement.as_ref();
    let transitive_type = statement.transitive.as_ref().map(|t| t.get_type());

    loop {
      let Some(ASTNode::Gotos(box parser::Gotos { goto, pushes })) = &active_statement.branch else { break };

      if pushes.len() > 0 {
        break;
      };

      let id = goto.name.to_token();

      let Some(IntMatch { statement, vals }) = single_byte_state.get(&id) else { break };

      if statement.transitive.as_ref().map(|t| t.get_type()) != transitive_type {
        break;
      };

      new_vals.append(&mut vals.clone());

      iter += 1;

      active_statement = statement.as_ref();

      if statement.non_branch.len() > 0 {
        break;
      }
    }

    if iter > 0 {
      let Statement { branch, non_branch, .. } = active_statement.clone();

      statement.branch = branch;
      statement.non_branch = non_branch;
      vals.clear();
      vals.append(&mut new_vals);

      *mode = "_BYTE_SEQUENCE_".to_string();
    }
  }

  garbage_collect(db, parse_states, "byte-sequences")
}

/// Inline trivial scanners.
fn inline_scanners<'db>(db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  fn inline_scanners(db: &ParserDatabase, statement: &mut parser::Statement) -> SherpaResult<()> {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { mode, scanner, matches, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      if !matches!(mode.as_str(), MatchInputType::TOKEN_STR) {
        return SherpaResult::Ok(());
      };

      let tok_ids = matches
        .iter()
        .map(|m| match m {
          ASTNode::IntMatch(box IntMatch { statement, vals, .. }) => {
            vals.clone().into_iter().map(|v| (db.token((v as u32).into()).sym_id, statement)).collect::<Vec<_>>()
          }
          ASTNode::DefaultMatch(box parser::DefaultMatch { statement }) => vec![(SymbolId::Default, statement)],
          _ => vec![],
        })
        .flatten()
        .collect::<Vec<_>>();

      if tok_ids.iter().all(|(i, _)| match *i {
        SymbolId::Token { val, .. } => val.to_str(db.string_store()).as_str().len() == 1,
        sym if sym.is_class() => true,
        SymbolId::Default => true,
        _ => false,
      }) {
        // convert each match into a local branch
        let mut id_groups = hash_group_btreemap(tok_ids, |_, (i, _)| match *i {
          SymbolId::Default => MatchInputType::Default,
          sym if sym.is_class() => MatchInputType::Class,
          sym if sym.is_codepoint(db.string_store()) => MatchInputType::Codepoint,
          _ => MatchInputType::Byte,
        });

        // Remove the default statement if present. This will be appended to the end of
        // the last Matches block.
        let default = id_groups.remove(&MatchInputType::Default).and_then(|d| d.into_iter().next());

        fn setup_match<'db>(
          db: &'db ParserDatabase,
          queue: &mut VecDeque<(MatchInputType, Vec<(SymbolId, &Box<Statement>)>)>,
          match_stmt: &mut Matches,
          default: Option<(SymbolId, &Box<parser::Statement>)>,
        ) {
          if let Some((mode, group)) = queue.pop_back() {
            match_stmt.mode = mode.to_scanless().as_str().to_string();
            match_stmt.matches = group
              .into_iter()
              .map(|(val, stmt)| ASTNode::IntMatch(Box::new(IntMatch::new(stmt.clone(), vec![val.to_state_val(db) as u64]))))
              .collect();

            if !queue.is_empty() {
              let mut matches =
                parser::Matches::new(Default::default(), Default::default(), Default::default(), Default::default());

              setup_match(db, queue, &mut matches, default);

              let matches = ASTNode::Matches(Box::new(matches));
              let stmt = Box::new(parser::Statement::new(Some(matches), Default::default(), Default::default()));
              let default = ASTNode::DefaultMatch(Box::new(parser::DefaultMatch::new(stmt)));

              match_stmt.matches.push(default);
            } else if let Some((_, statement)) = default {
              match_stmt.matches.push(ASTNode::DefaultMatch(Box::new(DefaultMatch { statement: statement.clone() })))
            }
          }
        }

        let mut match_stmt = parser::Matches::new(Default::default(), Default::default(), Default::default(), Default::default());
        let mut queue = VecDeque::from_iter(id_groups.into_iter());

        setup_match(db, &mut queue, &mut match_stmt, default);

        *mode = match_stmt.mode;
        *scanner = Default::default();
        *matches = match_stmt.matches;
      }
    }
    SherpaResult::Ok(())
  }

  for (_, state) in &mut parse_states {
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      inline_scanners(db, statement)?;
    }
  }

  garbage_collect(db, parse_states, "inline-scanners")
}

/// Inline statements of states that don't have transitive actions or matches.
fn _inline_states<'db>(db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  // Get a reference to all root level branches.

  let mut naked_state_lookup = Map::new();

  for (name, state) in &parse_states {
    // Only statements with branches that are not Matches can be considered as
    // naked.
    if let Some(box parser::State { statement, .. }) = state.ast.as_ref() {
      if !matches!(statement.branch, Some(ASTNode::Matches(..))) {
        naked_state_lookup.insert(*name, *statement.clone());
      }
    }
  }

  fn inline_statement(
    db: &ParserDatabase,
    statement: &mut parser::Statement,
    stmt_lu: &Map<IString, Statement>,
  ) -> SherpaResult<()> {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { matches, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      for m in matches {
        let Some(statement) = get_match_statement_mut(m) else { continue };
        let parser::Statement { branch, non_branch, transitive, .. } = statement;
        loop {
          if let Some(own_gotos) = branch.as_mut().and_then(|b| b.as_Gotos_mut()) {
            let name = own_gotos.goto.name.to_token();

            if let Some(parser::Statement { branch: b, non_branch: nb, transitive: t, .. }) = stmt_lu.get(&name) {
              let mut t = t.clone();

              // Pop the last goto (if present) if the incoming transitive action is `POP`
              if own_gotos.pushes.len() > 0 && t.as_ref().is_some_and(|t| t.as_Pop().is_some()) {
                t = None;
                own_gotos.pushes.pop();
              }

              let replace_branch = match b {
                // We do not want to replace GOTOS if we have a Fail or Accept branch, so we make sure
                // our push list is empty, allowing the only goto instruction to be replaced by the incoming
                // statement's branch.
                Some(ASTNode::Pass(..)) | Some(ASTNode::Fail(..)) | Some(ASTNode::Accept(..) | ASTNode::Pop(..))
                  if own_gotos.pushes.is_empty() =>
                {
                  true
                }
                Some(ASTNode::Gotos(..)) | None => false,
                _ => break,
              };

              if match (&transitive, t.as_ref()) {
                (Some(..), Some(..)) => false,
                (None, None) | (Some(..), None) => true,
                (None, Some(..)) => {
                  *transitive = t;
                  true
                }
              } {
                non_branch.append(&mut nb.clone());

                if replace_branch {
                  *branch = b.clone();
                } else if let Some(g) = b.as_ref().and_then(|b| b.as_Gotos()) {
                  own_gotos.goto = g.goto.clone();
                  own_gotos.pushes.append(&mut g.pushes.clone());
                  // New gotos, we can continue the loop
                  continue;
                } else if own_gotos.pushes.len() > 0 {
                  own_gotos.goto = Box::new(parser::Goto::new(
                    own_gotos.pushes.last().expect("Should have at least one item").name.clone(),
                    Default::default(),
                    Default::default(),
                  ));
                  own_gotos.pushes.pop();
                  // Gotos changed, we can continue the loop
                  continue;
                } else {
                  *branch = None;
                }
              }
            }
          } else if let Some(_) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
            inline_statement(db, statement, stmt_lu)?;
          }
          break;
        }
      }
    }
    SherpaResult::Ok(())
  }

  for (_, state) in &mut parse_states {
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      inline_statement(db, statement, &naked_state_lookup)?;
    }
  }

  garbage_collect(db, parse_states, "inline")
}

/// Merges matching branches of states that only consist of goto/push
/// transitions.
fn merge_branches<'db>(_db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  // Get a reference to all root level branches.

  let mut state_branch_lookup: Map<(IString, IString, u64), Statement> = Map::new();

  for (name, state) in &parse_states {
    if let Some(box parser::State { statement, .. }) = state.ast.as_ref() {
      let mut queue = VecDeque::from_iter([statement.as_ref()]);

      while let Some(statement) = queue.pop_front() {
        let parser::Statement { transitive, non_branch, branch, .. } = statement;

        if transitive.is_none() && non_branch.is_empty() {
          if let Some(parser::Matches { matches, mode, .. }) = branch.as_ref().and_then(|b| b.as_Matches()) {
            for m in matches {
              match m {
                ASTNode::IntMatch(box parser::IntMatch { statement, vals }) => {
                  for val in vals {
                    state_branch_lookup.insert((*name, mode.to_token(), *val), *statement.clone());
                  }
                }
                ASTNode::DefaultMatch(box parser::DefaultMatch { statement }) => queue.push_back(statement.as_ref()),
                _ => {}
              }
            }
          }
        }
      }
    }
  }

  fn merge_branches<'db>(
    db: &'db ParserDatabase,
    statement: &mut parser::Statement,
    state_branch_lookup: &Map<(IString, IString, u64), Statement>,
  ) {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { matches, mode, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      for m in matches {
        match m {
          ASTNode::IntMatch(box parser::IntMatch { statement, vals }) => {
            let _r = statement.clone();
            loop {
              let parser::Statement { branch, non_branch, transitive, .. } = statement.as_mut();
              if vals.len() == 1 && transitive.is_none() && non_branch.is_empty() {
                let val = vals[0];
                if let Some(gotos) = branch.as_mut().and_then(|b| b.as_Gotos_mut()) {
                  let name = gotos.goto.name.clone();
                  let id = (name.to_token(), mode.to_token(), val);

                  if let Some(stmt) = state_branch_lookup.get(&id) {
                    let parser::Statement { branch: b, non_branch: mut nb, transitive: t, .. } = stmt.clone();

                    match b {
                      Some(ASTNode::Gotos(box Gotos { goto, mut pushes })) => {
                        gotos.goto = goto;
                        gotos.pushes.append(&mut pushes);
                        non_branch.append(&mut nb);
                        *transitive = t;
                        continue;
                      }
                      _ => {}
                    }
                  }
                }
              }
              break;
            }
          }
          ASTNode::DefaultMatch(box parser::DefaultMatch { statement }) => {
            merge_branches(db, statement, state_branch_lookup);
          }
          _ => {}
        }
      }
    }
  }

  for (_, state) in &mut parse_states {
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      merge_branches(_db, statement.as_mut(), &state_branch_lookup)
    }
  }

  SherpaResult::Ok(parse_states)
}

/// Joins branches of match statements that differ only in specifier. If
/// joined branches include the default branch, all other item info is
/// scrubbed. Match blocks that only have a default match are lowered into
/// their respective contexts.
fn combine_state_branches<'db>(db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> SherpaResult<ParseStatesMap> {
  fn merge_statements(from: parser::Statement, to: &mut parser::Statement) {
    let parser::Statement { branch, mut non_branch, transitive } = from;

    if let Some(transitive) = transitive {
      debug_assert!(to.transitive.is_none() && to.non_branch.is_empty());
      to.transitive = Some(transitive);
    }

    to.branch = branch;

    to.non_branch.append(&mut non_branch);
  }
  fn combine_branches(db: &ParserDatabase, statement: &mut parser::Statement) -> SherpaResult<Option<Statement>> {
    let parser::Statement { branch, .. } = statement;
    if let Some(branch) = branch {
      match branch {
        ASTNode::Matches(box parser::Matches { matches, .. }) => {
          if let Some(_) = matches.iter_mut().filter_map(|d| d.as_DefaultMatch_mut()).next() {
            //combine_branches(db, &mut default.statement)?;
          }

          let groups = hash_group_btreemap(matches.clone(), |_, node| {
            get_match_statement(node)
              .map(|s| hash_id_value_u64(print_IR(&ASTNode::Statement(Box::new(s.clone())), db).unwrap_or_default()))
          });

          let mut new_matches = vec![];

          for (_, mut group) in groups {
            debug_assert!(!group.is_empty());
            if group.len() > 1 {
              if let Some(default) = group.iter().find(|n| matches!(n, ASTNode::DefaultMatch(..))) {
                new_matches.push(default.clone());
              } else {
                let vals =
                  group.iter().flat_map(|m| m.as_IntMatch().map(|i| i.vals.clone()).unwrap_or(Default::default())).collect();
                let mut first = group.into_iter().next().expect("Should have at least one item");

                // At this point, all matches expressions should have been converted to Int
                first.as_IntMatch_mut().unwrap().vals = vals;

                new_matches.push(first);
              }
            } else {
              new_matches.append(&mut group);
            }
          }

          if new_matches.len() != matches.len() {
            matches.clear();
            matches.append(&mut new_matches);
          }

          for m in matches.iter_mut() {
            let Some(stmt) = get_match_statement_mut(m) else { continue };
            if let Some(statement) = combine_branches(db, stmt)? {
              let Some(stmt) = get_match_statement_mut(m) else { continue };
              merge_statements(statement, stmt)
            }
          }

          if matches.len() == 1 && matches!(matches[0], ASTNode::DefaultMatch(..)) {
            SherpaResult::Ok(Some(*matches[0].clone().to_DefaultMatch().statement))
          } else {
            SherpaResult::Ok(None)
          }
        }
        _ => SherpaResult::Ok(None),
      }
    } else {
      SherpaResult::Ok(None)
    }
  }

  for state in parse_states.values_mut() {
    if let Some(box parser::State { statement, tok: _, .. }) = &mut state.ast {
      if let Some(from) = combine_branches(db, statement)? {
        merge_statements(from, statement);
      }
    }
  }

  SherpaResult::Ok(parse_states)
}

/// Create canonical states by aliasing states that  generate the same canonical
/// hash, e.i: states that differ in name only.
fn canonicalize_states<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  mut parse_states: ParseStatesMap,
  gc_label: Option<&str>,
) -> SherpaResult<R> {
  let mut state_name_to_canonical_state_name = Map::new();
  let mut hash_to_name_set = Map::new();

  // Prefer root names.
  for (name, state) in &parse_states {
    if state.root {
      let hash = state.get_canonical_hash(db)?;
      let canonical_name = hash_to_name_set.entry(hash).or_insert(*name).clone();
      state_name_to_canonical_state_name.insert(*name, canonical_name);
    }
  }

  for (name, state) in &parse_states {
    if !state.root {
      let hash = state.get_canonical_hash(db)?;
      let canonical_name = hash_to_name_set.entry(hash).or_insert(*name).clone();
      state_name_to_canonical_state_name.insert(*name, canonical_name);
    }
  }

  fn canonicalize_goto_name(db: &ParserDatabase, name: String, name_lu: &Map<IString, IString>) -> SherpaResult<String> {
    let iname = name.to_token();
    let canonical_name =
      *name_lu.get(&iname).expect(&("State name should exist: ".to_string() + &iname.to_string(db.string_store())));

    let name = if iname != canonical_name { canonical_name.to_string(db.string_store()) } else { name };
    SherpaResult::Ok(name)
  }

  fn canonicalize_statement(
    db: &ParserDatabase,
    statement: &mut parser::Statement,
    name_lu: &Map<IString, IString>,
  ) -> SherpaResult<()> {
    let parser::Statement { branch, .. } = statement;
    if let Some(branch) = branch.as_mut() {
      match branch {
        ASTNode::Gotos(box parser::Gotos { goto, pushes }) => {
          for push in pushes.iter_mut() {
            push.name = canonicalize_goto_name(db, push.name.clone(), &name_lu)?;
          }

          goto.name = canonicalize_goto_name(db, goto.name.clone(), &name_lu)?;
        }
        ASTNode::Matches(box parser::Matches { matches, .. }) => {
          for m in matches {
            match m {
              ASTNode::TermMatch(..) | ASTNode::DefaultMatch(..) | ASTNode::IntMatch(..) | ASTNode::NonTermMatch(..) => {
                let Some(stmt) = get_match_statement_mut(m) else { continue };
                canonicalize_statement(db, stmt, name_lu)?;
              }
              _ => {}
            }
          }
        }
        _ => {}
      }
    }
    SherpaResult::Ok(())
  }

  for state in parse_states.values_mut() {
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      canonicalize_statement(db, statement, &state_name_to_canonical_state_name)?;
    }
  }

  garbage_collect(db, parse_states, gc_label.unwrap_or("conanicalize"))
}

/// Removes any states that are not referenced, directly or indirectly, by
/// at least one of the entry states.
pub fn garbage_collect<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  mut parse_states: ParseStatesMap,
  _reason: &str,
) -> SherpaResult<R> {
  // let start_complexity = ComplexityMarker::new(db, parse_states.iter());

  let mut out = Array::new();
  let mut queue = VecDeque::from_iter(
    db.entry_points().iter().map(|p| (p.nonterm_entry_name, parse_states.remove(&p.nonterm_entry_name).unwrap())),
  );

  while let Some((name, state)) = queue.pop_front() {
    // traverse the state to find all goto and push references, convert
    // reference name to IString and push respective state to queue
    if let SherpaResult::Ok(ast) = state.get_ast() {
      let stmt = &ast.statement;
      traverse_statement(stmt, &mut parse_states, &mut queue)?;
    }

    out.push((name, state));
  }

  //start_complexity.print_comparison(&ComplexityMarker::new(db,
  // out.iter().map(|(i, b)| (i, b))), reason);

  SherpaResult::Ok(R::from_iter(out))
}

fn traverse_statement<'db>(
  stmt: &Statement,
  parse_states: &mut ParseStatesMap,
  queue: &mut VecDeque<(IString, Box<ParseState>)>,
) -> SherpaResult<()> {
  if let Some(branch) = &stmt.branch {
    match branch {
      ASTNode::Matches(box Matches { scanner, mode, .. }) if mode.as_str() == MatchInputType::TOKEN_STR => {
        enqueue_state(scanner.to_token(), parse_states, queue, false)
      }
      _ => {}
    }

    match branch {
      ASTNode::Matches(box Matches { matches, .. })
      | ASTNode::ProductionMatches(box ProductionMatches { matches, .. })
      | ASTNode::TerminalMatches(box TerminalMatches { matches, .. }) => {
        for m in matches.iter().rev() {
          let Some(stmt) = get_match_statement(m) else { continue };
          match m {
            ASTNode::TermMatch(..) | ASTNode::DefaultMatch(..) | ASTNode::IntMatch(..) | ASTNode::NonTermMatch(..) => {
              traverse_statement(stmt, parse_states, queue)?;
            }
            _ => {}
          }
        }
      }
      ASTNode::Gotos(gotos) => {
        for push in gotos.pushes.iter().rev() {
          let name = push.name.to_token();
          enqueue_state(name, parse_states, queue, true);
        }
        let name = gotos.goto.name.to_token();
        enqueue_state(name, parse_states, queue, true);
      }
      _ => {}
    }
  }
  SherpaResult::Ok(())
}

fn enqueue_state<'db>(
  name: IString,
  parse_states: &mut ParseStatesMap,
  queue: &mut VecDeque<(IString, Box<ParseState>)>,
  push_front: bool,
) {
  if let Some(state) = parse_states.remove(&name) {
    if push_front {
      queue.push_front((name, state))
    } else {
      queue.push_back((name, state))
    }
  }
}
