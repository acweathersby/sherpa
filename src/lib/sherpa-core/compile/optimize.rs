use crate::{
  hash_group_btreemap,
  hash_id_value_u64,
  parser::{
    self,
    ASTNode,
    DefaultMatch,
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
use sherpa_rust_runtime::types::bytecode::InputType;
use std::collections::{HashMap, VecDeque};

/// Performance various transformation on the parse state graph
/// to reduce number of steps between transient actions, and to
/// reduce the number of parse states overall.
pub fn optimize<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<R> {
  let parse_states = garbage_collect(db, parse_states, "initial purge")?;

  let parse_states = canonicalize_states(db, parse_states, None)?;

  let parse_states = merge_branches(db, parse_states)?;

  let parse_states = combine_state_branches(db, parse_states)?;

  let parse_states = canonicalize_states(db, parse_states, Some("state combine"))?;

  let parse_states = inline_states(db, parse_states)?;

  //let parse_states = inline_scanners(db, parse_states)?;

  let parse_states = create_byte_chains(db, parse_states)?;

  let parse_states = inline_matches(db, parse_states)?;

  // Can only do this once we can map scanner ids back to match statements
  // otherwise scanner states will be dropped.
  /*   let parse_states = parse_states
  .into_iter()
  .map(|(name, state)| {
    let mut state = state.remap_source(db).unwrap();
    state.build_ast(db).unwrap();
    (name, Box::new(state))
  })
  .collect(); */

  garbage_collect(db, parse_states, "finalize")
}

/// Extract the target name of a goto or push statement
pub fn get_goto_target_name(node: &ASTNode) -> String {
  match node {
    ASTNode::Goto(box parser::Goto { prod, .. }) | ASTNode::Push(box parser::Push { prod, .. }) => get_goto_target_name(prod),
    ASTNode::Production_Symbol(box parser::Production_Symbol { name, .. }) => name.clone(),
    _ => unreachable!(),
  }
}

fn get_match_statement_mut(node: &mut ASTNode) -> SherpaResult<&mut parser::Statement> {
  match node {
    ASTNode::TermMatch(box TermMatch { statement, .. })
    | ASTNode::DefaultMatch(box DefaultMatch { statement, .. })
    | ASTNode::IntMatch(box IntMatch { statement, .. })
    | ASTNode::NonTermMatch(box NonTermMatch { statement, .. }) => SherpaResult::Ok(statement.as_mut()),
    _ => SherpaResult::None,
  }
}

fn get_match_statement(node: &ASTNode) -> SherpaResult<&parser::Statement> {
  match node {
    ASTNode::TermMatch(box TermMatch { statement, .. })
    | ASTNode::DefaultMatch(box DefaultMatch { statement, .. })
    | ASTNode::IntMatch(box IntMatch { statement, .. })
    | ASTNode::NonTermMatch(box NonTermMatch { statement, .. }) => SherpaResult::Ok(statement.as_ref()),
    _ => SherpaResult::None,
  }
}

/// Inline trivial scanners.
fn inline_matches<'db>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<Map<IString, Box<ParseState>>> {
  garbage_collect(db, parse_states, "byte-chains")
}

/// Create chained matching scanners that can scan and shift multiple
/// characters simultaneously.
fn create_byte_chains<'db>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<Map<IString, Box<ParseState>>> {
  garbage_collect(db, parse_states, "byte-chains")
}

/// Inline trivial scanners.
fn inline_scanners<'db>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<Map<IString, Box<ParseState>>> {
  fn inline_scanners(db: &ParserDatabase, statement: &mut parser::Statement) -> SherpaResult<()> {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { mode, meta, matches, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      if mode.as_str() != InputType::TOKEN_STR {
        return SherpaResult::Ok(());
      };

      let tok_ids = matches
        .iter()
        .map(|m| match m {
          ASTNode::IntMatch(box IntMatch { statement, vals, .. }) => {
            vals.clone().into_iter().map(|v| (v, statement)).collect::<Vec<_>>()
          }
          _ => vec![],
        })
        .flatten()
        .map(|(i, stmt)| (db.token((i as u32).into()), stmt))
        .collect::<Vec<_>>();

      if tok_ids.iter().all(|(i, _)| match i.sym_id {
        SymbolId::Token { val, .. } => val.to_str(db.string_store()).as_str().len() == 1,
        sym if sym.is_class() => true,
        _ => false,
      }) {
        // convert each match into a local branch
        let id_groups = hash_group_btreemap(tok_ids, |_, (i, _)| match i.sym_id {
          sym if sym.is_class() => InputType::Class,
          _ => InputType::Byte,
        });

        fn setup_match<'db>(
          db: &'db ParserDatabase,
          queue: &mut VecDeque<(InputType, Vec<(DBTokenData, &Box<Statement>)>)>,
          match_stmt: &mut Matches,
        ) {
          if let Some((mode, group)) = queue.pop_back() {
            match_stmt.mode = mode.as_str().to_string();
            match_stmt.matches = group
              .into_iter()
              .map(|(val, stmt)| {
                ASTNode::IntMatch(Box::new(IntMatch::new(stmt.clone(), vec![val.sym_id.to_state_val(db) as u64])))
              })
              .collect();

            if !queue.is_empty() {
              let mut matches = parser::Matches::new(Default::default(), Default::default(), Default::default());

              setup_match(db, queue, &mut matches);

              let matches = ASTNode::Matches(Box::new(matches));
              let stmt = Box::new(parser::Statement::new(Some(matches), Default::default(), Default::default()));
              let default = ASTNode::DefaultMatch(Box::new(parser::DefaultMatch::new(stmt)));

              match_stmt.matches.push(default);
            }
          }
        }

        let mut match_stmt = parser::Matches::new(Default::default(), Default::default(), Default::default());
        let mut queue = VecDeque::from_iter(id_groups.into_iter());

        setup_match(db, &mut queue, &mut match_stmt);

        *mode = match_stmt.mode;
        *meta = 0;
        *matches = match_stmt.matches;
      }
    }
    SherpaResult::Ok(())
  }

  for (_, state) in &mut parse_states {
    if let SherpaResult::Ok(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      inline_scanners(db, statement)?;
    }
  }

  garbage_collect(db, parse_states, "inline-scanners")
}

/// Inline statements of states that don't have transitive actions or matches.
fn inline_states<'db>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<Map<IString, Box<ParseState>>> {
  // Get a reference to all root level branches.

  let mut naked_state_lookup = HashMap::new();

  for (name, state) in &parse_states {
    // Only statements with branches that are not Matches can be considered as
    // naked.
    if let SherpaResult::Ok(box parser::State { statement, .. }) = state.ast.as_ref() {
      if !matches!(statement.branch, Some(ASTNode::Matches(..))) {
        naked_state_lookup.insert(*name, *statement.clone());
      }
    }
  }

  fn inline_statement(
    db: &ParserDatabase,
    statement: &mut parser::Statement,
    stmt_lu: &HashMap<IString, Statement>,
  ) -> SherpaResult<()> {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { matches, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      for m in matches {
        let statement = get_match_statement_mut(m)?;
        let parser::Statement { branch, non_branch, transitive, .. } = statement;
        loop {
          if let Some(own_gotos) = branch.as_mut().and_then(|b| b.as_Gotos_mut()) {
            let name = get_goto_target_name(&own_gotos.goto.prod).to_token();

            if let Some(parser::Statement { branch: b, non_branch: nb, transitive: t, .. }) = stmt_lu.get(&name) {
              let replace_branch = match b {
                // We do not want to replace GOTOS if we have a Fail or Accept branch, so we make sure
                // our push list is empty, allowing the only goto instruction to be replaced by the incoming
                // statement's branch.
                Some(ASTNode::Pass(..)) | Some(ASTNode::Fail(..)) | Some(ASTNode::Accept(..)) if own_gotos.pushes.is_empty() => {
                  true
                }
                Some(ASTNode::Pass(..)) | Some(ASTNode::Gotos(..)) | None => false,
                _ => break,
              };

              if match (&transitive, t) {
                (Some(..), Some(..)) => false,
                (None, None) | (Some(..), None) => true,
                (None, Some(..)) => {
                  *transitive = t.clone();
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
                  own_gotos.goto = Box::new(parser::Goto::new(own_gotos.pushes.last()?.prod.clone()));
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
    if let SherpaResult::Ok(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      inline_statement(db, statement, &naked_state_lookup)?;
    }
  }

  garbage_collect(db, parse_states, "inline")
}

/// Merges matching branches of states that consist only of goto/push
/// transitions.
fn merge_branches<'db>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<Map<IString, Box<ParseState>>> {
  // Get a reference to all root level branches.

  let mut state_branch_lookup = HashMap::new();

  for (name, state) in &parse_states {
    if let SherpaResult::Ok(box parser::State { statement, .. }) = state.ast.as_ref() {
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

  for (_, state) in &mut parse_states {
    if let SherpaResult::Ok(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      let parser::Statement { branch, .. } = statement.as_mut();

      if let Some(parser::Matches { matches, mode, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
        for m in matches {
          match m {
            ASTNode::IntMatch(box parser::IntMatch { statement, vals }) => {
              let r = statement.clone();
              loop {
                let parser::Statement { branch, non_branch, transitive, .. } = statement.as_mut();
                if vals.len() == 1 && transitive.is_none() && non_branch.is_empty() {
                  let val = vals[0];
                  if let Some(gotos) = branch.as_mut().and_then(|b| b.as_Gotos_mut()) {
                    let name = get_goto_target_name(&gotos.goto.prod);
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
            ASTNode::DefaultMatch(box parser::DefaultMatch { statement }) => {}
            _ => {}
          }
        }
      }
    }
  }

  SherpaResult::Ok(parse_states)
}

/// Joins branches of match statements that differ only in specifier. If
/// joined branches include the default branch, all other item info is
/// scrubbed. Match blocks that only have a default match are lowered into
/// their respective contexts.
fn combine_state_branches<'db>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
) -> SherpaResult<Map<IString, Box<ParseState>>> {
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
          let groups =
            hash_group_btreemap(matches.clone(), |_, node| get_match_statement(node).to_option().map(|s| hash_id_value_u64(s)));

          let mut new_matches = vec![];

          for (_, mut group) in groups {
            debug_assert!(!group.is_empty());
            if group.len() > 1 {
              if let Some(default) = group.iter().find(|n| matches!(n, ASTNode::DefaultMatch(..))) {
                new_matches.push(default.clone());
              } else {
                let vals =
                  group.iter().flat_map(|m| m.as_IntMatch().map(|i| i.vals.clone()).unwrap_or(Default::default())).collect();
                let mut first = group.into_iter().next()?;
                // At this point, all matches expressions should have been converted to Int
                first.as_IntMatch_mut()?.vals = vals;
                new_matches.push(first);
              }
            } else {
              new_matches.append(&mut group);
            }
          }

          if (new_matches.len() != matches.len()) {
            matches.clear();
            matches.append(&mut new_matches);
          }

          for m in matches.iter_mut() {
            if let Some(statement) = combine_branches(db, get_match_statement_mut(m)?)? {
              merge_statements(statement, get_match_statement_mut(m)?)
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
    if let SherpaResult::Ok(box parser::State { statement, tok, .. }) = &mut state.ast {
      if let Some(from) = combine_branches(db, statement)? {
        merge_statements(from, statement);
      }
    }
  }

  SherpaResult::Ok(parse_states)
}

// Create canonical states by aliasing states that  generate the same canonical
// hash, e.i: states that differ in name only.
fn canonicalize_states<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
  gc_label: Option<&str>,
) -> SherpaResult<R> {
  let mut state_name_to_canonical_state_name = HashMap::new();
  let mut hash_to_name_set = HashMap::new();

  for (name, state) in &parse_states {
    let hash = state.get_canonical_hash();
    let canonical_name = hash_to_name_set.entry(hash).or_insert(*name).clone();
    state_name_to_canonical_state_name.insert(*name, canonical_name);
  }

  fn canonicalize_goto_prod(db: &ParserDatabase, prod: ASTNode, name_lu: &HashMap<IString, IString>) -> SherpaResult<ASTNode> {
    let name = get_goto_target_name(&prod);
    let iname = name.to_token();
    let canonical_name = *name_lu.get(&iname)?;

    let prod = if iname != canonical_name {
      ASTNode::Production_Symbol(Box::new(parser::Production_Symbol::new(
        canonical_name.to_string(db.string_store()),
        prod.to_token().clone(),
      )))
    } else {
      prod
    };
    SherpaResult::Ok(prod)
  }

  fn canonicalize_statement(
    db: &ParserDatabase,
    statement: &mut parser::Statement,
    name_lu: &HashMap<IString, IString>,
  ) -> SherpaResult<()> {
    let parser::Statement { branch, .. } = statement;
    if let Some(branch) = branch.as_mut() {
      match branch {
        ASTNode::Gotos(box parser::Gotos { goto, pushes }) => {
          for push in pushes.iter_mut() {
            push.prod = canonicalize_goto_prod(db, push.prod.clone(), &name_lu)?;
          }

          goto.prod = canonicalize_goto_prod(db, goto.prod.clone(), &name_lu)?;
        }
        ASTNode::Matches(box parser::Matches { matches, .. }) => {
          for m in matches {
            match m {
              ASTNode::TermMatch(..) | ASTNode::DefaultMatch(..) | ASTNode::IntMatch(..) | ASTNode::NonTermMatch(..) => {
                canonicalize_statement(db, get_match_statement_mut(m)?, name_lu)?;
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
    if let SherpaResult::Ok(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      canonicalize_statement(db, statement, &state_name_to_canonical_state_name);
    }
  }

  garbage_collect(db, parse_states, gc_label.unwrap_or("conanicalize"))
}

pub fn get_complexity<'db>(db: &'db ParserDatabase, states: &Map<IString, Box<ParseState>>) -> f64 {
  states.iter().map(|(_, s)| s.print(db, false).unwrap().len()).fold(0, |a, b| a + b) as f64
}

/// Removes any states that are not referenced, directly or indirectly, by
/// at least one of the entry states.
pub fn garbage_collect<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState>>,
  reason: &str,
) -> SherpaResult<R> {
  let start_len = parse_states.len();
  let start_complexity = get_complexity(db, &parse_states);

  let mut out = Array::new();
  let mut queue =
    VecDeque::from_iter(db.entry_points().iter().map(|p| (p.prod_entry_name, parse_states.remove(&p.prod_entry_name).unwrap())));

  while let Some((name, state)) = queue.pop_front() {
    // traverse the state to find all goto and push references, convert
    // reference name to IString and push respective state to queue
    if let SherpaResult::Ok(ast) = state.get_ast() {
      let stmt = &ast.statement;
      traverse_statement(stmt, &mut parse_states, &mut queue);
    }

    out.push((name, state));
  }

  println!(
    "GC {} ---- {} -> {} Complexity Reduction: {}%",
    reason,
    start_len,
    out.len(),
    ((1.0 - get_complexity(db, &out.clone().into_iter().collect()) / start_complexity) * 100.0).round()
  );

  SherpaResult::Ok(R::from_iter(out))
}

fn traverse_statement<'db>(
  stmt: &Statement,
  parse_states: &mut Map<IString, Box<ParseState>>,
  queue: &mut VecDeque<(IString, Box<ParseState>)>,
) -> SherpaResult<()> {
  if let Some(branch) = &stmt.branch {
    match branch {
      ASTNode::Matches(box Matches { meta, mode, .. }) if mode.as_str() == InputType::TOKEN_STR => {
        enqueue_state(IString::from_u64(*meta), parse_states, queue, false)
      }
      _ => {}
    }

    match branch {
      ASTNode::Matches(box Matches { matches, .. })
      | ASTNode::ProductionMatches(box ProductionMatches { matches, .. })
      | ASTNode::TerminalMatches(box TerminalMatches { matches, .. }) => {
        for m in matches.iter().rev() {
          match m {
            ASTNode::TermMatch(..) | ASTNode::DefaultMatch(..) | ASTNode::IntMatch(..) | ASTNode::NonTermMatch(..) => {
              traverse_statement(get_match_statement(m)?, parse_states, queue)?;
            }
            _ => {}
          }
        }
      }
      ASTNode::Gotos(gotos) => {
        for push in gotos.pushes.iter().rev() {
          let name = get_goto_target_name(&push.prod).to_token();
          enqueue_state(name, parse_states, queue, true);
        }
        let name = get_goto_target_name(&gotos.goto.prod).to_token();
        enqueue_state(name, parse_states, queue, true);
      }
      _ => {}
    }
  }
  SherpaResult::Ok(())
}

fn enqueue_state<'db>(
  name: IString,
  parse_states: &mut Map<IString, Box<ParseState>>,
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
