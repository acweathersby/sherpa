use crate::{
  hash_group_btreemap,
  hash_id_value_u64,
  journal::Journal,
  parser::{
    self,
    ASTNode,
    DefaultMatch,
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
use std::collections::{BTreeMap, VecDeque};

/// Removes any states that are not referenced, directly or indirectly, by
/// at least one of the entry states.
pub fn garbage_collect<'db, R: FromIterator<(IString, Box<ParseState<'db>>)>>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState<'db>>>,
) -> SherpaResult<R> {
  let mut out = Array::new();
  let mut queue = VecDeque::from_iter(
    db.entry_points()
      .iter()
      .map(|p| (p.prod_entry_name, parse_states.remove(&p.prod_entry_name).unwrap())),
  );

  while let Some((name, state)) = queue.pop_front() {
    // traverse the state to find all goto and push references, convert
    // reference name to IString and push respective state to queue
    if let SherpaResult::Ok(ast) = state.get_ast() {
      let stmt = &ast.statement;
      traverse_statement(stmt, &mut parse_states, &mut queue);
    }

    out.push((name, state));
  }

  SherpaResult::Ok(R::from_iter(out))
}

fn traverse_statement<'db>(
  stmt: &Statement,
  parse_states: &mut Map<IString, Box<ParseState<'db>>>,
  queue: &mut VecDeque<(IString, Box<ParseState<'db>>)>,
) {
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
            ASTNode::TermMatch(box TermMatch { statement, .. })
            | ASTNode::DefaultMatch(box DefaultMatch { statement, .. })
            | ASTNode::IntMatch(box IntMatch { statement, .. })
            | ASTNode::NonTermMatch(box NonTermMatch { statement, .. }) => {
              traverse_statement(statement, parse_states, queue)
            }
            _ => {}
          }
        }
      }
      ASTNode::Gotos(gotos) => {
        for push in gotos.pushes.iter().rev() {
          let name = push.prod.to_token().to_string().to_token();
          enqueue_state(name, parse_states, queue, true);
        }
        let name = gotos.goto.prod.to_token().to_string().to_token();
        enqueue_state(name, parse_states, queue, true);
      }
      _ => {}
    }
  }
}

fn enqueue_state<'db>(
  name: IString,
  parse_states: &mut Map<IString, Box<ParseState<'db>>>,
  queue: &mut VecDeque<(IString, Box<ParseState<'db>>)>,
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

pub struct _OptConfig {}

/// Performance various transformation on the parse state graph
/// to reduce number of steps between transient actions, and to the
/// reduce number of parse states overall.
pub fn optimize<'db, R: FromIterator<(IString, Box<ParseState<'db>>)>>(
  db: &'db ParserDatabase,
  parse_states: Map<IString, Box<ParseState<'db>>>,
) -> SherpaResult<R> {
  let mut parse_states: Map<IString, Box<ParseState<'db>>> = garbage_collect(db, parse_states)?;

  // Proceed through each state and apply optimizations them. If any optimizations
  // occur that can may change inter-state references, garbage collect and
  // perform another optimization pass.

  for (_, state) in &mut parse_states {
    let ast = state.get_ast_mut()?;

    // first attempt is to combine branches
    let parser::State { statement, .. } = ast.as_mut();
    let parser::Statement { branch, .. } = statement.as_mut();
    if let Some(branch) = branch.as_mut() {
      match branch {
        parser::ASTNode::Matches(box parser::Matches { matches, .. }) => {
          // group matches based on hash id
          let groups = hash_group_btreemap(matches.clone(), |_, m| match m {
            ASTNode::IntMatch(box parser::IntMatch { statement, .. })
            | ASTNode::DefaultMatch(box parser::DefaultMatch { statement, .. })
            | ASTNode::TermMatch(box parser::TermMatch { statement, .. }) => {
              hash_id_value_u64(statement)
            }
            _ => hash_id_value_u64(m),
          });

          if groups.iter().any(|g| g.1.len() > 1) {
            let mut new_set = vec![];
            for (_, group) in groups {
              if let Some(default) = group.iter().filter_map(|g| g.as_DefaultMatch()).next() {
                new_set.push(ASTNode::DefaultMatch(Box::new(default.clone())));
              } else if let Some(int) = group.first()?.as_IntMatch() {
                let vals = group
                  .iter()
                  .filter_map(|g| g.as_IntMatch())
                  .map(|g| g.vals.clone())
                  .flatten()
                  .collect::<Vec<_>>();

                let mut int = int.clone();
                int.vals = vals;
                new_set.push(ASTNode::IntMatch(Box::new(int.clone())));
              } else {
                panic!(
                  "Only default and int matches should be present in post graph compile states"
                )
              }
            }

            if new_set.len() == 1 && new_set[0].as_DefaultMatch().is_some() {
            } else {
            }
          }
        }
        _ => {}
      }
    }
  }

  garbage_collect(db, parse_states)
}

pub fn __print_states__<'db>(
  db: &'db ParserDatabase,
  parse_states: &Map<IString, Box<ParseState<'db>>>,
) {
  for (_, state) in parse_states {
    println!("\n{}\n", state.code)
  }
}
