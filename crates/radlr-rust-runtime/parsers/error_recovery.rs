use crate::{
  parsers::fork::{fork_meta_kernel, CHAR_USAGE_SCORE},
  types::*,
};
use std::{collections::VecDeque, rc::Rc};

use super::{
  fork::{attempt_merge, create_merge_groups, create_token, insert_node, reduce_symbols},
  Parser,
};

const _TOKEN_SYNTHESIS_PENALTY: isize = 1;

/// Maximum number of subsequent synthetic tokens
const SYNTH_LIMIT: usize = 10;

pub trait ErrorRecoveringDatabase<I: ParserInput>: ParserProducer<I> + Sized {
  /// Parse while attempting to recover from any errors encountered in the
  /// input.
  ///
  /// This extends the fork parser by applying recovery methods
  /// when the base parser encounters input that prevent it from continuing. As
  /// several error recovery strategies are employed, the resulting parse
  /// forest may include several trees containing error corrections of varying
  /// quality. In this case, the tree containing the least number of error
  /// correction assumptions will be ordered in front of trees that employ more
  /// guesswork to recover parsing.
  fn parse_with_recovery(&self, input: &mut I, entry: EntryPoint, store: &CSTStore) -> Result<Vec<RecCTX>, ParserError> {
    parse_with_recovery(input, entry, self, store)
  }
}

impl<I: ParserInput, T: ParserProducer<I> + Sized> ErrorRecoveringDatabase<I> for T {}

pub fn parse_with_recovery<I: ParserInput, DB: ParserProducer<I>>(
  input: &mut I,
  entry: EntryPoint,
  db: &DB,
  store: &CSTStore,
) -> Result<Vec<RecCTX>, ParserError> {
  let mut parser = db.get_parser()?;

  let mut pending = ContextQueue::new_with_capacity(64)?;

  pending.push_back(0, create_recovery_ctx::<I, DB>(input, &mut parser, entry)?);

  pending.swap_buffers();

  let mut failed_contexts: Vec<(ParserState, RecCTX)> = Vec::new();
  let mut completed: Vec<RecCTX> = Vec::new();
  let mut best_failure = None;

  while !pending.pop_is_empty() {
    fork_meta_kernel(input, parser.as_mut(), &mut pending, &mut completed, &mut failed_contexts, store)?;
    handle_failed_contexts(&mut failed_contexts, input, db, &mut best_failure, &mut parser, &mut pending, store);
    pending.swap_buffers();
  }

  completed.sort();

  // if completed is empty then we should take our best failed context wrap the
  // remaining input into an errata symbol, create an error non-terminal that
  // matches the goal, and wrap all remaining symbols underneath that nonterminal.

  Ok(completed)
}

fn handle_failed_contexts<I: ParserInput, DB: ParserProducer<I>>(
  failed_contexts: &mut Vec<(ParserState, RecCTX)>,
  input: &mut I,
  db: &DB,
  best_failure: &mut Option<RecCTX>,
  parser: &mut Box<dyn Parser<I>>,
  pending: &mut ContextQueue<RecCTX>,
  store: &CSTStore,
) {
  if failed_contexts.len() > 0 {
    let mut to_process = VecDeque::new();

    for rec_ctx in attempt_merge(
      create_merge_groups(failed_contexts.drain(..).map(|(ps, mut s)| {
        s.last_failed_state = ps;
        (s.last_failed_state.address as u32, s, None)
      })),
      "pre-error",
      store,
    )
    .collect::<Vec<_>>()
    {
      // Need to halt forward progression until we have resolved all
      // contexts that have errors.

      let last_state = rec_ctx.last_failed_state;

      match rec_ctx.mode {
        RecoveryMode::Normal | RecoveryMode::SyntheticInput { .. } => {
          let offset = rec_ctx.ctx().sym_ptr;
          let ctx = &rec_ctx.ctx;

          // ---------------------------------------------------------------
          // 2. Drop bytes/codepoints until the input is positioned at the start of an
          //    acceptable token.
          drop_codepoints(&rec_ctx, &mut to_process, best_failure, input, 0, offset, last_state);

          // ---------------------------------------------------------------
          // 3. Drop symbols until a prior state is reached where the current input is
          //    accepted.

          drop_symbols(&rec_ctx, &mut to_process, best_failure, 0, ctx.sym_ptr);

          // We fork our contexts into different recovery modes. This may create a large
          // number of independent contexts, but we'll likely prune many of the recovered
          // paths as they become unrecoverable or, score poorly relative to other paths,
          // or are eventually merged back into each other.

          // We can do several actions to recover from this error.
          // ---------------------------------------------------------------
          // 1. Create a zero length token whose id matches one of the expected tokens at
          //    this state. The failing state address can be used as a key into a lookup
          //    table that contains all expected token ids.

          inject_synthetics(&rec_ctx, db, last_state, &mut to_process);

          // ---------------------------------------------------------------
          // 4(parser dependent): Find a recovery state within the stack
          // history. If such a state exists, pop all states
          // following that state off the stack and then resume
          // parsing from the recovery state.

          // Discard the old context
          // --------------------------------------
          // pick_best_failure(&mut best_failure, rec_ctx);
        }
        _ => unimplemented!(),
      }
    }

    // Need to sort our context so that we are using only contexts that have
    // the best potential (lowest error). This is also the point where
    // we can join contexts that differ only in symbols.
    let resolved = resolve_errored_contexts(input, parser, &mut to_process, store);

    let continued = attempt_merge(
      create_merge_groups(resolved.into_iter().map(|s| (s.last_failed_state.address as u32, s, None))),
      "post-error",
      store,
    )
    .collect::<Vec<_>>();

    pending.swap_buffers();

    let mut all: Vec<_> = pending.take_pop().chain(continued).collect();

    all.sort();

    for rec_ctx in all.into_iter().take(32) {
      pending.push_with_priority(rec_ctx.prority(), rec_ctx)
    }
  }
}

fn resolve_errored_contexts<I: ParserInput>(
  input: &mut I,
  parser: &mut Box<dyn Parser<I>>,
  contexts: &mut VecDeque<RecCTX>,
  store: &CSTStore,
) -> Vec<RecCTX> {
  let mut to_continue = vec![];
  let mut best_failure = None;

  while let Some(mut rec_ctx) = contexts.pop_front() {
    if let Some(action) = parser.next(input, &mut rec_ctx.ctx) {
      match action {
        ParseAction::Shift {
          byte_offset: token_byte_offset,
          byte_length: token_byte_length,
          token_id,
          emitting_state,
          ..
        } => {
          rec_ctx.last_failed_state = emitting_state;
          match rec_ctx.mode {
            RecoveryMode::CodepointDiscard { start_offset, .. } => {
              let length = token_byte_offset - start_offset as u32;

              create_errata(input, &mut rec_ctx, length, start_offset as u32);
              insert_node(
                emitting_state,
                &mut rec_ctx,
                create_token(input, token_id, token_byte_length, token_byte_offset),
                store,
              );

              rec_ctx.mode = RecoveryMode::Normal;
            }
            RecoveryMode::SymbolDiscard { start_offset, end_offset, .. } => {
              let length: u32 = end_offset as u32 - start_offset as u32;

              create_errata(input, &mut rec_ctx, length, start_offset as u32);
              insert_node(
                emitting_state,
                &mut rec_ctx,
                create_token(input, token_id, token_byte_length, token_byte_offset),
                store,
              );

              rec_ctx.mode = RecoveryMode::Normal;
            }
            RecoveryMode::SyntheticInput { tok_id, count, .. } => {
              debug_assert_eq!(token_byte_length, 0);
              debug_assert_eq!(token_id, tok_id);
              let entropy = count as isize;
              rec_ctx
                .symbols
                .push((emitting_state, Rc::new(CSTNode::Token(TokenNode::missing_type(tok_id as u16, entropy as usize)))));
              rec_ctx.entropy += entropy;
            }
            _ => unreachable!(),
          }
          to_continue.push(rec_ctx);
        }

        ParseAction::Reduce { nonterminal_id, rule_id, symbol_count } => {
          reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id, store);
          contexts.push_back(rec_ctx);
        }

        ParseAction::Accept { .. } => {
          drop(rec_ctx); /* drop this context? */
        }

        ParseAction::Skip { byte_length, .. } => {
          rec_ctx.entropy -= byte_length as isize * CHAR_USAGE_SCORE;
          contexts.push_back(rec_ctx);
        }

        ParseAction::Error { last_state, .. } => match rec_ctx.mode {
          RecoveryMode::Unrecoverable(..) | RecoveryMode::SyntheticInput { .. } => {
            pick_best_failure(&mut best_failure, rec_ctx);
          }
          RecoveryMode::CodepointDiscard { count, start_offset } => {
            drop_codepoints(&rec_ctx, contexts, &mut best_failure, input, count, start_offset, last_state);
          }
          RecoveryMode::SymbolDiscard { count, end_offset, .. } => {
            drop_symbols(&rec_ctx, contexts, &mut best_failure, count, end_offset);
          }
          _ => unreachable!(),
        },

        _ => unreachable!(),
      }
    }
  }

  to_continue
}

fn create_errata<I: ParserInput>(input: &mut I, rec_ctx: &mut RecCTX, token_byte_length: u32, token_byte_offset: u32) {
  rec_ctx.symbols.push((
    Default::default(),
    Rc::new(CSTNode::Token(TokenNode::error_type(
      &input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
    ))),
  ));
  rec_ctx.entropy += token_byte_length as isize;
}

fn inject_synthetics<I: ParserInput, DB: ParserProducer<I>>(
  rec_ctx: &RecCTX,
  db: &DB,
  last_state: ParserState,
  failed_contexts: &mut VecDeque<RecCTX>,
) {
  let new_origin_state = last_state.info.state_id as usize;
  let new_offset = rec_ctx.offset;

  // A check to make sure where not creating an infinite series of tokens due to
  // production rules such as `A => a A?`  or `A => A 'a' | A => 'a'`
  let (same_state, curr_id) = match rec_ctx.mode {
    RecoveryMode::SyntheticInput { origin_state, offset, tok_id, .. } => {
      (origin_state == new_origin_state && offset == new_offset, tok_id)
    }
    _ => (false, 0),
  };

  if matches!(rec_ctx.mode, RecoveryMode::SyntheticInput { .. }) {}

  let count = match rec_ctx.mode {
    RecoveryMode::SyntheticInput { count, .. } => count,
    _ => 0,
  } + 1;
  if count < SYNTH_LIMIT {
    if let Some(ids) = db.get_expected_tok_ids_at_state(new_origin_state as u32) {
      for id in ids {
        if same_state && curr_id == *id {
          continue;
        }

        let mut rec_ctx = rec_ctx.split();
        rec_ctx.mode = RecoveryMode::SyntheticInput { tok_id: *id, count, origin_state: new_origin_state, offset: new_offset };
        rec_ctx.ctx.push_state(last_state);
        rec_ctx.ctx.recovery_tok_id = *id;
        rec_ctx.ctx.byte_len = 0;
        rec_ctx.ctx.tok_byte_len = 0;
        failed_contexts.push_back(rec_ctx);
      }
    }
  }
}

fn drop_codepoints<I: ParserInput>(
  rec_ctx: &RecCTX,
  contexts: &mut VecDeque<RecCTX>,
  best_failure: &mut Option<RecCTX>,
  input: &mut I,
  mut count: usize,
  start_offset: usize,
  last_state: ParserState,
) {
  let end = input.len();
  let offset = rec_ctx.ctx().sym_ptr;
  // Continue advancing the input, unless already at EOI. If EOI
  // advance the one byte

  if offset < end {
    count += 1;

    let mut rec_ctx = rec_ctx.split();

    rec_ctx.mode = RecoveryMode::CodepointDiscard { start_offset, count };

    let ctx = &mut rec_ctx.ctx;

    if ctx.sym_ptr < input.len() {
      // Restore the context to the previous state
      ctx.push_state(last_state);
      ctx.is_finished = false;

      // advance the input by one codepoint
      ctx.sym_ptr += input.codepoint_len(ctx.sym_ptr) as usize;

      ctx.byte_len = 0;
      ctx.tok_byte_len = 0;

      contexts.push_back(rec_ctx);
    } else {
      pick_best_failure(best_failure, rec_ctx);
    }
  }
}

fn drop_symbols(
  rec_ctx: &RecCTX,
  contexts: &mut VecDeque<RecCTX>,
  best_failure: &mut Option<RecCTX>,
  count: usize,
  end_offset: usize,
) {
  if rec_ctx.symbols.len() == 0 {
    return;
  };

  let mut rec_ctx = rec_ctx.split();

  let count = count + 1;
  let mut start_offset = end_offset;
  loop {
    if let Some((state, token)) = rec_ctx.symbols.pop() {
      start_offset -= token.len() as usize;
      match token.as_ref() {
        CSTNode::Token(tok) => {
          match tok.ty() {
            NodeType::Errata => {
              rec_ctx.entropy -= tok.entropy() as isize;
            }
            NodeType::Skipped => {
              rec_ctx.entropy += tok.entropy() as isize * CHAR_USAGE_SCORE;
            }
            ty @ NodeType::Missing | ty @ NodeType::Token => {
              match ty {
                NodeType::Missing => {
                  rec_ctx.entropy -= tok.entropy() as isize;
                }
                NodeType::Token => {
                  rec_ctx.entropy += tok.entropy() as isize * CHAR_USAGE_SCORE;
                }
                _ => unreachable!(),
              }

              rec_ctx.mode = RecoveryMode::SymbolDiscard { count, end_offset, start_offset };

              // Remove any states that have been pushed to the stack since this token was
              // introduced.
              let ctx = &mut rec_ctx.ctx;
              while ctx.stack.len() > state.info.stack_address as usize {
                ctx.stack.pop();
              }

              if ctx.stack.len() <= 1 {
                pick_best_failure(best_failure, rec_ctx);
                break;
              }

              let ctx = &mut rec_ctx.ctx;

              // Restore the context to the previous state
              ctx.push_state(state);
              ctx.is_finished = false;
              ctx.byte_len = 0;
              ctx.tok_byte_len = 0;
              contexts.push_back(rec_ctx);

              break;
            }
            _ => unreachable!(),
          }
        }
        CSTNode::NonTerm(..) | CSTNode::Alts(..) => {
          // Reduce or increase errata based on the contents of the node

          let mut queue = VecDeque::from_iter(vec![token]);
          let mut entropy_delta = 0;

          while let Some(token) = queue.pop_front() {
            match token.as_ref() {
              CSTNode::Token(tok) => match tok.ty() {
                NodeType::Missing | NodeType::Errata => {
                  entropy_delta -= tok.entropy() as isize;
                }
                NodeType::Skipped | NodeType::Token => {
                  entropy_delta += tok.entropy() as isize * CHAR_USAGE_SCORE;
                }
                _ => unreachable!(),
              },
              CSTNode::NonTerm(node) => queue.extend(node.symbols.iter().cloned()),
              CSTNode::Alts(node) => queue.extend(node.alternatives.first().unwrap().symbols.iter().cloned()),
            }
          }

          rec_ctx.entropy += entropy_delta;
        }
      }
    } else {
      pick_best_failure(best_failure, rec_ctx);
      break;
    }
  }
}

fn pick_best_failure(best_failure: &mut Option<RecCTX>, rec_ctx: RecCTX) {
  if let Some(failed_ctx) = best_failure {
    if failed_ctx.ctx().sym_ptr < rec_ctx.ctx().sym_ptr || failed_ctx.entropy > rec_ctx.entropy {
      *best_failure = Some(rec_ctx);
    }
  } else {
    *best_failure = Some(rec_ctx);
  }
}
