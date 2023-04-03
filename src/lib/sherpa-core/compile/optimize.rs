use crate::{
  journal::Journal,
  parser::{
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
use sherpa_runtime::types::bytecode::InputType;
use std::collections::VecDeque;

/// Removes any states that are not referenced, directly or indirectly, by
/// at least one of the entry states.
pub fn garbage_collect<
  'db,
  R: FromIterator<(IString, Box<ParseState<'db>>)>,
>(
  db: &'db ParserDatabase,
  mut parse_states: Map<IString, Box<ParseState<'db>>>,
) -> SherpaResult<R> {
  let mut out = Array::new();
  let mut queue = VecDeque::from_iter(db.entry_points().iter().map(|p| {
    (p.prod_entry_name, parse_states.remove(&p.prod_entry_name).unwrap())
  }));

  while let Some((name, mut state)) = queue.pop_front() {
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
      ASTNode::Matches(box Matches { meta, mode, .. })
        if mode.as_str() == InputType::TOKEN_STR =>
      {
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

/// Performance various transformation of the parse state graph
/// to reduce number of steps between transient actions, and to the
/// reduce number of parse states overall.
pub fn optimize<'db>(
  j: &mut Journal,
  db: &'db ParserDatabase,
  parse_states: Map<IString, Box<ParseState<'db>>>,
) -> SherpaResult<Map<IString, Box<ParseState<'db>>>> {
  let parse_states = garbage_collect(db, parse_states)?;

  SherpaResult::Ok(parse_states)
}
