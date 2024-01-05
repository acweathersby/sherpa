use crate::{
  hash_group_btreemap,
  hash_id_value_u64,
  parser::{
    self,
    radlr_bc,
    ASTNode,
    ASTNodeType,
    DefaultMatch,
    GetASTNodeType,
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
use radlr_rust_runtime::types::bytecode::MatchInputType;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

type Map<A, B> = BTreeMap<A, B>;
type Set<A> = BTreeSet<A>;

/// Uses a garbage collection sweep to remove states that have no path to entry
/// states. No optimizations are performed.
pub(crate) fn sweep<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  parse_states: ParseStatesMap,
  optimize_for_debugging: bool,
) -> RadlrResult<(R, OptimizationReport)> {
  let start = ComplexityMarker::from_map_iter(db, parse_states.iter());
  finish(optimize_for_debugging, parse_states, db, config, OptimizationReport { start, ..Default::default() })
}

/// Performance various transformation on the parse state graph
/// to reduce the number of steps between transient actions, and to
/// reduce the number of parse states overall.
pub(crate) fn optimize<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  parse_states: ParseStatesMap,
  optimize_for_debugging: bool,
) -> RadlrResult<(R, OptimizationReport)> {
  let mut report = OptimizationReport {
    start: ComplexityMarker::from_map_iter(db, parse_states.iter()),
    ..Default::default()
  };

  let parse_states = garbage_collect(db, config, parse_states, None)?.0;

  let parse_states = canonicalize_states(db, config, parse_states, false)?.0;

  let parse_states = merge_branches(db, parse_states)?;

  let parse_states = combine_state_branches(db, parse_states)?;

  let parse_states = canonicalize_states(db, config, parse_states, false)?.0;

  let parse_states = inline_states(db, config, parse_states)?;

  let parse_states = if config.ALLOW_SCANNER_INLINING { inline_scanners(db, config, parse_states)? } else { parse_states };

  let parse_states = if config.ALLOW_BYTE_SEQUENCES { create_byte_sequences(db, config, parse_states)? } else { parse_states };

  let parse_states = merge_branches(db, parse_states)?;

  let parse_states = remove_redundant_defaults(db, config, parse_states)?;

  let parse_states = canonicalize_states(db, config, parse_states, false)?.0;

  let parse_states = combine_state_branches(db, parse_states)?;

  // Perform final rounds of canonicalization, removing as many redundant states
  // as possible.
  let parse_states = {
    report.canonical_rounds += 3;
    let mut states = parse_states;

    let mut remove_self_recursive = false;
    loop {
      match canonicalize_states(db, config, states, remove_self_recursive)? {
        (s, true) => {
          report.canonical_rounds += 1;
          states = s;
        }
        (s, false) => {
          if !remove_self_recursive {
            // Perform a rounds of canonicalization that merge states that are
            // identical but are also self-recursive
            remove_self_recursive = true;

            states = s;
          } else {
            break s;
          }
        }
      }
    }
  };

  if cfg!(debug_assertions) {
    // Ensure all states are unique at this point
    let mut hashes = Set::new();
    for (_, state) in parse_states.iter() {
      let hash = state.get_canonical_hash(db, true).unwrap();
      debug_assert!(
        hashes.insert(hash),
        "State {} does not have a unique hash: [{hash}]",
        state.guid_name.to_string(db.string_store())
      )
    }
  }

  let parse_states: ParseStatesMap = parse_states;

  finish(optimize_for_debugging, parse_states, db, config, report)
}

fn finish<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  optimize_for_debugging: bool,
  parse_states: BTreeMap<IString, Box<ParseState>>,
  db: &ParserDatabase,
  config: &ParserConfig,
  report: OptimizationReport,
) -> RadlrResult<(R, OptimizationReport)> {
  if optimize_for_debugging {
    let parse_states = parse_states.into_iter().map(|(name, state)| {
      let mut state = state.remap_source(db).unwrap_or(Default::default());
      if let Err(err) = state.build_ast(db) {
        todo!("Failed to build AST {}", err)
      }
      (name, Box::new(state))
    });

    garbage_collect(db, config, parse_states.collect(), Some(report))
  } else {
    garbage_collect(db, config, parse_states, Some(report))
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
fn _inline_matches<'db>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  parse_states: ParseStatesMap,
) -> RadlrResult<ParseStatesMap> {
  Ok(garbage_collect(db, config, parse_states, None)?.0)
}

/// Remove default branches that transition to match states that are identical
/// to the root branches.
fn remove_redundant_defaults<'db>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  mut parse_states: ParseStatesMap,
) -> RadlrResult<ParseStatesMap> {
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

    let Some(gt) = statement.branch.as_mut().and_then(|s| s.as_Gotos_mut()) else {
      return false;
    };

    let parser::Gotos { goto, pushes, .. } = gt;

    if let Some(goto) = goto {
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
    } else {
      false
    }
  }

  for (state_id, state) in &mut parse_states {
    let info = state.print(db, true)?;
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      remove_redundant_defaults(db, *state_id, statement.as_mut(), &state_branch_lookup, &info);
    }
  }

  Ok(garbage_collect(db, config, parse_states, None)?.0)
}

/// Create chained matching scanners that can scan sequences of
/// characters simultaneously.
fn create_byte_sequences<'db>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  mut parse_states: ParseStatesMap,
) -> RadlrResult<ParseStatesMap> {
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
      let Some(ASTNode::Gotos(gt)) = &active_statement.branch else { break };
      let parser::Gotos { goto, pushes, .. } = gt.as_ref();

      if let Some(goto) = goto {
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

  Ok(garbage_collect(db, config, parse_states, None)?.0)
}

/// Inline trivial scanners.
///
/// Trivial scanners are those that only produce single codepoint tokens.
fn inline_scanners<'db>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  mut parse_states: ParseStatesMap,
) -> RadlrResult<ParseStatesMap> {
  fn inline_scanners(db: &ParserDatabase, statement: &mut parser::Statement) -> RadlrResult<()> {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { mode, scanner, matches, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      if !matches!(mode.as_str(), MatchInputType::TOKEN_STR) {
        return RadlrResult::Ok(());
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
            let stmt =
              Box::new(parser::Statement::new(Some(matches), Default::default(), Default::default(), Default::default()));
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
    RadlrResult::Ok(())
  }

  for (_, state) in &mut parse_states {
    if let Some(scanner) = state.get_scanner() {
      if !scanner.symbols.iter().map(|s| s.tok()).chain(scanner.skipped.iter().cloned()).all(|s| match db.token(s).sym_id {
        SymbolId::Token { val, .. } => val.to_str(db.string_store()).as_str().len() == 1,
        sym if sym.is_class() => true,
        SymbolId::Default => true,
        _ => false,
      }) {
        continue;
      }
      if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
        inline_scanners(db, statement)?;
      }
    }
  }

  Ok(garbage_collect(db, config, parse_states, None)?.0)
}

/// Inline statements of states that don't have transitive actions or matches.
fn inline_states<'db>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  mut parse_states: ParseStatesMap,
) -> RadlrResult<ParseStatesMap> {
  // Get a reference to all root level branches.

  let mut trivial_state_lookup = Map::new();

  for (name, state) in &parse_states {
    // Only statements that don't contain branches can be
    // considered "trivial".
    if let Some(box parser::State { statement, .. }) = state.ast.as_ref() {
      if !matches!(statement.branch, Some(ASTNode::Matches(..))) {
        trivial_state_lookup.insert(*name, *statement.clone());
      }
    }
  }

  fn inline_statement(
    db: &ParserDatabase,
    statement: &mut parser::Statement,
    stmt_lu: &Map<IString, Statement>,
  ) -> RadlrResult<()> {
    let parser::Statement { branch, .. } = statement;

    if let Some(parser::Matches { matches, .. }) = branch.as_mut().and_then(|b| b.as_Matches_mut()) {
      for m in matches {
        let Some(statement) = get_match_statement_mut(m) else { continue };
        let parser::Statement { branch, non_branch, transitive, pop } = statement;

        let mut pop_count = pop.as_ref().map(|p| p.count.max(1)).unwrap_or_default();

        *pop = None;
        loop {
          if let Some(own_gotos) = branch.as_mut().and_then(|b| b.as_Gotos_mut()) {
            if let Some(goto) = &mut own_gotos.goto {
              let name = goto.name.to_token();

              if let Some(parser::Statement { branch: b, non_branch: nb, transitive: t, pop }) = stmt_lu.get(&name) {
                let t = t.clone();

                // Pops and Pushes annihilate each other. For each pop level remove 1 goto in
                // the incoming push list, starting from right to left. Re-create the pop
                // command if all incoming pushes where annihilated.

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
                  non_branch.extend(nb.clone());

                  if let Some(pop) = pop {
                    pop_count += pop.count;
                    let push_len = own_gotos.pushes.len() as u32;
                    let diff = push_len as isize - pop_count as isize;
                    if diff > 0 {
                      // There are more Pushes then Pop. Remove the last `len`
                      // pushes and
                      pop_count = 0;
                      own_gotos.pushes.drain(diff as usize..);
                    } else if diff == 0 {
                      // Pushes and Pops perfectly cancel each other out.
                      own_gotos.pushes.clear();
                      pop_count = 0;
                    } else {
                      // There are more Pops then Pushes. Push the pop
                      // instruction back into the the
                      // states instructions, less the number of pushes
                      pop_count -= push_len;
                      own_gotos.pushes.clear();
                    }
                  }

                  if replace_branch {
                    *branch = b.clone();
                  } else if let Some(g) = b.as_ref().and_then(|b| b.as_Gotos()) {
                    own_gotos.goto = g.goto.clone();
                    own_gotos.pushes.extend(g.pushes.iter().cloned());
                    // New gotos, we can continue the loop
                    continue;
                  } else if own_gotos.pushes.len() > 0 {
                    own_gotos.goto = Some(Box::new(parser::Goto::new(
                      own_gotos.pushes.last().expect("Should have at least one item").name.clone(),
                      Default::default(),
                      Default::default(),
                    )));
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
          } else {
            break;
          }
        }

        if pop_count > 0 {
          // Insert a pop instruction as the last non-branching statement.
          statement.pop = Some(Box::new(radlr_bc::Pop::new(pop_count, Default::default())));
        }
      }
    }
    RadlrResult::Ok(())
  }

  for (_, state) in &mut parse_states {
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      inline_statement(db, statement, &trivial_state_lookup)?;
    }
  }

  Ok(garbage_collect(db, config, parse_states, None)?.0)
}

/// Merges matching branches of states that only consist of goto/push
/// transitions.
fn merge_branches<'db>(_db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> RadlrResult<ParseStatesMap> {
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
                  if let Some(goto) = &mut gotos.goto {
                    let name = goto.name.clone();
                    let id = (name.to_token(), mode.to_token(), val);

                    if let Some(stmt) = state_branch_lookup.get(&id) {
                      let parser::Statement { branch: b, non_branch: mut nb, transitive: t, .. } = stmt.clone();

                      match b {
                        Some(ASTNode::Gotos(mut b_gt)) => {
                          gotos.goto = b_gt.goto;
                          gotos.pushes.append(&mut b_gt.pushes);
                          non_branch.append(&mut nb);
                          *transitive = t;
                          continue;
                        }
                        _ => {}
                      }
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

  RadlrResult::Ok(parse_states)
}

/// Joins branches of match statements that differ only in specifier. If
/// joined branches include the default branch, all other item info is
/// scrubbed. Match blocks that only have a default match without a transitive
/// instruciton are lowered into their respective contexts.
fn combine_state_branches<'db>(db: &'db ParserDatabase, mut parse_states: ParseStatesMap) -> RadlrResult<ParseStatesMap> {
  fn merge_statements(from: parser::Statement, to: &mut parser::Statement) {
    let parser::Statement { branch, mut non_branch, transitive, pop } = from;

    if let Some(transitive) = transitive {
      debug_assert!(to.transitive.is_none() && to.non_branch.is_empty());
      to.transitive = Some(transitive);
    }

    to.branch = branch;
    to.non_branch.append(&mut non_branch);
    to.pop = pop;
  }
  fn combine_branches(db: &ParserDatabase, statement: &mut parser::Statement) -> RadlrResult<Option<Statement>> {
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

          match (matches.len(), &matches[0]) {
            (1, ASTNode::DefaultMatch(default)) => {
              if default.statement.transitive.is_none() {
                RadlrResult::Ok(Some(*default.statement.clone()))
              } else {
                RadlrResult::Ok(None)
              }
            }
            _ => RadlrResult::Ok(None),
          }
        }
        _ => RadlrResult::Ok(None),
      }
    } else {
      RadlrResult::Ok(None)
    }
  }

  for state in parse_states.values_mut() {
    if let Some(box parser::State { statement, tok: _, .. }) = &mut state.ast {
      if let Some(from) = combine_branches(db, statement)? {
        merge_statements(from, statement);
      }
    }
  }

  RadlrResult::Ok(parse_states)
}

/// Create canonical states by aliasing states that  generate the same canonical
/// hash, e.i: states that differ in name only.
fn canonicalize_states<
  'db,
  R: FromIterator<(IString, Box<ParseState>)> + Clone + IntoIterator<Item = (IString, Box<ParseState>)>,
>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  mut parse_states: ParseStatesMap,
  merge_self_recursive: bool,
) -> RadlrResult<(R, bool)> {
  let mut has_changed = false;
  let mut state_name_to_canonical_state_name = Map::new();
  let mut hash_to_name_set = Map::new();

  // Prefer root names.
  for (n, state) in &parse_states {
    #[cfg(debug_assertions)]
    debug_assert_eq!(*n, state.guid_name, "");
    let name = state.guid_name;
    if state.root {
      let hash: u64 = state.get_canonical_hash(db, merge_self_recursive)?;
      let canonical_name = *hash_to_name_set.entry(hash).or_insert(name);
      state_name_to_canonical_state_name.insert(name, canonical_name);
    }
  }

  for (n, state) in &parse_states {
    #[cfg(debug_assertions)]
    debug_assert_eq!(*n, state.guid_name, "");
    let name = state.guid_name;
    if !state.root {
      let hash = state.get_canonical_hash(db, merge_self_recursive)?;
      let canonical_name = *hash_to_name_set.entry(hash).or_insert(name);
      state_name_to_canonical_state_name.insert(name, canonical_name);
    }
  }

  fn canonicalize_goto_name(db: &ParserDatabase, name: &str, name_lu: &Map<IString, IString>) -> Option<String> {
    let iname = name.to_token();
    let canonical_name = *name_lu.get(&iname).expect(&("State name should exist: ".to_string() + &name));
    (iname != canonical_name).then(|| canonical_name.to_string(db.string_store()))
  }

  fn canonicalize_statement(
    db: &ParserDatabase,
    statement: &mut parser::Statement,
    name_lu: &Map<IString, IString>,
  ) -> RadlrResult<bool> {
    let parser::Statement { branch, .. } = statement;
    let mut has_changed = false;

    if let Some(branch) = branch.as_mut() {
      match branch {
        ASTNode::Gotos(gt) => {
          for push in gt.pushes.iter_mut() {
            if let Some(name) = canonicalize_goto_name(db, &push.name, name_lu) {
              push.name = name;
              has_changed = true;
            }
          }
          if let Some(goto) = gt.goto.as_mut() {
            if let Some(name) = canonicalize_goto_name(db, &goto.name, name_lu) {
              goto.name = name;
              has_changed = true
            }
          }
          if let Some(fork) = gt.fork.as_mut() {
            for init in fork.paths.iter_mut() {
              if let Some(name) = canonicalize_goto_name(db, &init.name, name_lu) {
                init.name = name;
                has_changed = true
              }
            }
          }
        }
        ASTNode::Matches(box parser::Matches { scanner, matches, .. }) => {
          if !scanner.is_empty() {
            if let Some(name) = canonicalize_goto_name(db, &scanner, name_lu) {
              *scanner = name;
              has_changed = true
            }
          }

          for m in matches {
            match m {
              ASTNode::TermMatch(..) | ASTNode::DefaultMatch(..) | ASTNode::IntMatch(..) | ASTNode::NonTermMatch(..) => {
                let Some(stmt) = get_match_statement_mut(m) else { continue };
                has_changed |= canonicalize_statement(db, stmt, name_lu)?;
              }
              _ => {}
            }
          }
        }
        _ => {}
      }
    }
    RadlrResult::Ok(has_changed)
  }

  for state in parse_states.values_mut() {
    if let Some(box parser::State { statement, .. }) = &mut state.as_mut().ast {
      has_changed |= canonicalize_statement(db, statement, &state_name_to_canonical_state_name)?;
    }
  }

  if has_changed {
    Ok((garbage_collect(db, config, parse_states, None)?.0, true))
  } else {
    Ok((parse_states.into_iter().collect(), false))
  }
}

/// Removes any states that are not referenced, directly or indirectly, by
/// at least one of the entry states.
pub fn garbage_collect<'db, R: FromIterator<(IString, Box<ParseState>)>>(
  db: &'db ParserDatabase,
  config: &ParserConfig,
  mut parse_states: ParseStatesMap,
  report: Option<OptimizationReport>,
) -> RadlrResult<(R, OptimizationReport)> {
  // let start_complexity = ComplexityMarker::new(db, parse_states.iter());

  let mut out = Array::new();
  let mut queue =
    VecDeque::from_iter(db.entry_points().iter().filter(|e| config.EXPORT_ALL_NONTERMS || e.is_export).filter_map(|p| {
      if let Some(state) = parse_states.remove(&p.nonterm_entry_name) {
        Some((p.nonterm_entry_name, state))
      } else {
        None
      }
    }));

  while let Some((name, state)) = queue.pop_front() {
    // traverse the state to find all goto and push references, convert
    // reference name to IString and push respective state to queue
    if let RadlrResult::Ok(ast) = state.get_ast() {
      let stmt = &ast.statement;
      traverse_statement(stmt, &mut parse_states, &mut queue)?;
    }

    out.push((name, state));
  }

  if let Some(mut report) = report {
    report.end = ComplexityMarker::from_vec_iter(db, out.iter());
    RadlrResult::Ok((R::from_iter(out), report))
  } else {
    RadlrResult::Ok((R::from_iter(out), Default::default()))
  }
}

fn traverse_statement<'db>(
  stmt: &Statement,
  parse_states: &mut ParseStatesMap,
  queue: &mut VecDeque<(IString, Box<ParseState>)>,
) -> RadlrResult<()> {
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

        if let Some(goto) = gotos.goto.as_ref() {
          let name = goto.name.to_token();
          enqueue_state(name, parse_states, queue, true);
        }

        if let Some(fork) = gotos.fork.as_ref() {
          for goto in &fork.paths {
            let name = goto.name.to_token();
            enqueue_state(name, parse_states, queue, true);
          }
        }
      }
      _ => {}
    }
  }
  RadlrResult::Ok(())
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
