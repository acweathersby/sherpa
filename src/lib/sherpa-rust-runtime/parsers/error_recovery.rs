use crate::{parsers::fork::fork_meta_kernel, types::*};
use std::collections::VecDeque;

use super::{
  fork::{attempt_merge, create_merge_groups, create_token, insert_node, reduce_symbols},
  Parser,
};

const _TOKEN_SYNTHESIS_PENALTY: isize = 1;

/// Maximum number of subsequent synthetic tokens
const SYNTH_LIMIT: usize = 8;

pub trait ErrorRecoveringDatabase<I: ParserInput>: ParserProducer<I> + Sized {
  /// Parse while attempting to recover from any errors encountered in the
  /// input.
  ///
  /// This extends the fork parser by allowing recovery methods to be applied
  /// when the base parser encounters input that prevent it from continuing. As
  /// several error recovery strategies are employed, the resulting parse
  /// forest may include several trees containing error corrections of varying
  /// quality. In this case, the tree containing the least number of error
  /// correction assumptions will be ordered in front of trees that employ more
  /// guesswork to recover parsing.
  fn parse_with_recovery(&self, input: &mut I, entry: EntryPoint) -> Result<(), ParserError> {
    parse_with_recovery(input, entry, self)
  }
}

impl<I: ParserInput, T: ParserProducer<I> + Sized> ErrorRecoveringDatabase<I> for T {}

pub fn parse_with_recovery<I: ParserInput, DB: ParserProducer<I>>(
  input: &mut I,
  entry: EntryPoint,
  db: &DB,
) -> Result<(), ParserError> {
  let mut parser = db.get_parser()?;

  let mut pending = ContextQueue::new_with_capacity(64)?;
  pending.push_back(Box::new(RecoverableContext {
    offset: 0,
    entropy: input.len() as isize,
    symbols: vec![],
    ctx: parser.init(entry)?,
    mode: RecoveryMode::Normal,
    last_failed_state: Default::default(),
  }));
  pending.swap_buffers();

  let mut failed_contexts: Vec<(ParserState, Box<RecoverableContext>)> = Vec::new();
  let mut completed = Vec::new();
  let mut best_failure = None;

  while !pending.pop_is_empty() {
    fork_meta_kernel(input, parser.as_mut(), &mut pending, &mut completed, &mut failed_contexts)?;
    handle_failed_contexts(&mut failed_contexts, input, db, &mut best_failure, &mut parser, &mut pending);
    pending.swap_buffers();
  }

  completed.sort();

  // if completed is empty then we should take our best failed context wrap the
  // remaining input into an errata symbol, create an error non-terminal that
  // matches the goal, and wrap all remaining symbols underneath that nonterminal.

  #[cfg(debug_assertions)]
  dbg!(&completed);

  if let Some(best) = completed.first() {
    println!("\n");
    for sym in &best.symbols {
      Printer::new(sym, db).print();
      println!("\n");
      Printer::new(sym, db).print_all();
    }
    println!("\n");
  }

  Ok(())
}

fn handle_failed_contexts<I: ParserInput, DB: ParserProducer<I>>(
  failed_contexts: &mut Vec<(ParserState, Box<RecoverableContext>)>,
  input: &mut I,
  db: &DB,
  best_failure: &mut Option<Box<RecoverableContext>>,
  parser: &mut Box<dyn Parser<I>>,
  pending: &mut ContextQueue<Box<RecoverableContext>>,
) {
  if failed_contexts.len() > 0 {
    let mut to_process = VecDeque::new();

    for rec_ctx in attempt_merge(
      create_merge_groups(failed_contexts.drain(..).map(|(ps, mut s)| {
        s.last_failed_state = ps;
        (s.last_failed_state.address as u32, s, None)
      })),
      "pre-error",
    )
    .collect::<Vec<_>>()
    {
      // Need to halt forward progression until we have resolved all
      // contexts that have errors.
      let end = input.len();

      let last_state = rec_ctx.last_failed_state;

      match rec_ctx.mode {
        RecoveryMode::Normal | RecoveryMode::SyntheticInput { .. } => {
          let offset = rec_ctx.get_offset();
          let ctx = &rec_ctx.ctx;

          // We fork our contexts into different recovery modes. This may create a large
          // number of independent contexts, but we'll likely prune many of the recovered
          // paths as they become unrecoverable or, score poorly relative to other paths,
          // or are eventually merged back into each other.

          // We can do several actions to recover from this error.
          // ---------------------------------------------------------------
          // 1. Create a zero length token whose id matches one of the expected tokens at
          //    this point. The failing state address can be used as a key into a lookup
          //    table that contains all possible token id's expected at this point.

          inject_synthetics(&rec_ctx, db, last_state, &mut to_process);

          // ---------------------------------------------------------------
          // 2. Drop bytes/codepoints until the input is positioned at the start of an
          //    acceptable token.
          if offset < end {
            drop_codepoints(rec_ctx.split(), &mut to_process, best_failure, input, 0, offset, last_state);
          }

          // ---------------------------------------------------------------
          // 3. Drop symbols until a prior state is reached where the current input is
          //    accepted.
          if rec_ctx.symbols.len() > 0 {
            drop_symbols(rec_ctx.split(), &mut to_process, best_failure, 0, ctx.sym_ptr);
          }

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
    let resolved = resolve_errored_contexts(input, db, parser, &mut to_process);

    let continued = attempt_merge(
      create_merge_groups(resolved.into_iter().map(|s| (s.last_failed_state.address as u32, s, None))),
      "post-error",
    )
    .collect::<Vec<_>>();

    pending.swap_buffers();

    let mut all: Vec<_> = pending.take_pop().chain(continued).collect();

    all.sort();

    for rec_ctx in all.into_iter().take(32) {
      pending.push_with_priority(rec_ctx)
    }
  }
}

fn resolve_errored_contexts<I: ParserInput, DB: ParserProducer<I>>(
  input: &mut I,
  db: &DB,
  parser: &mut Box<dyn Parser<I>>,
  contexts: &mut VecDeque<Box<RecoverableContext>>,
) -> Vec<Box<RecoverableContext>> {
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
              insert_node(&mut rec_ctx, create_token(input, emitting_state, token_id, token_byte_length, token_byte_offset));

              rec_ctx.mode = RecoveryMode::Normal;
            }
            RecoveryMode::SymbolDiscard { start_offset, end_offset, .. } => {
              let length = end_offset as u32 - start_offset as u32;

              create_errata(input, &mut rec_ctx, length, start_offset as u32);
              insert_node(&mut rec_ctx, create_token(input, emitting_state, token_id, token_byte_length, token_byte_offset));

              rec_ctx.mode = RecoveryMode::Normal;
            }
            RecoveryMode::SyntheticInput { tok_id, count } => {
              debug_assert_eq!(token_byte_length, 0);
              debug_assert_eq!(token_id, tok_id);
              let entropy = count as isize;
              rec_ctx.symbols.push(TokenNode::missing_type(
                emitting_state,
                tok_id,
                token_byte_offset,
                db.token_id_to_str(tok_id).unwrap_or_default().to_string(),
                entropy as u32,
              ));
              rec_ctx.entropy += entropy;
            }

            _ => unreachable!(),
          }
          to_continue.push(rec_ctx);
        }

        ParseAction::Reduce { nonterminal_id, rule_id, symbol_count } => {
          reduce_symbols(symbol_count, rec_ctx.as_mut(), nonterminal_id, rule_id);
          contexts.push_back(rec_ctx);
        }

        ParseAction::Accept { .. } => {
          drop(rec_ctx); /* drop this context? */
        }

        ParseAction::Skip { byte_length, .. } => {
          rec_ctx.entropy -= byte_length as isize;
          contexts.push_back(rec_ctx);
        }

        ParseAction::Error { last_state, .. } => match rec_ctx.mode {
          RecoveryMode::Unrecoverable(..) | RecoveryMode::SyntheticInput { .. } => {
            pick_best_failure(&mut best_failure, rec_ctx);
          }
          RecoveryMode::CodepointDiscard { count, start_offset } => {
            drop_codepoints(rec_ctx, contexts, &mut best_failure, input, count, start_offset, last_state);
          }
          RecoveryMode::SymbolDiscard { count, end_offset, .. } => {
            drop_symbols(rec_ctx, contexts, &mut best_failure, count, end_offset);
          }
          _ => unreachable!(),
        },

        _ => unreachable!(),
      }
    }
  }

  to_continue
}

fn create_errata<I: ParserInput>(
  input: &mut I,
  rec_ctx: &mut Box<RecoverableContext>,
  token_byte_length: u32,
  token_byte_offset: u32,
) {
  rec_ctx.symbols.push(TokenNode::error_type(
    token_byte_length,
    token_byte_offset,
    input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
  ));
  rec_ctx.entropy += token_byte_length as isize;
}

fn inject_synthetics<I: ParserInput, DB: ParserProducer<I>>(
  rec_ctx: &Box<RecoverableContext>,
  db: &DB,
  last_state: ParserState,
  failed_contexts: &mut VecDeque<Box<RecoverableContext>>,
) {
  let count = match rec_ctx.mode {
    RecoveryMode::SyntheticInput { count, .. } => count,
    _ => 0,
  } + 1;
  if count < SYNTH_LIMIT {
    if let Some(ids) = db.get_expected_tok_ids_at_state(last_state.info.state_id) {
      for id in ids {
        let mut rec_ctx = rec_ctx.split();
        rec_ctx.mode = RecoveryMode::SyntheticInput { tok_id: *id, count };
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
  mut rec_ctx: Box<RecoverableContext>,
  contexts: &mut VecDeque<Box<RecoverableContext>>,
  best_failure: &mut Option<Box<RecoverableContext>>,
  input: &mut I,
  mut count: usize,
  start_offset: usize,
  last_state: ParserState,
) {
  // Continue advancing the input, unless already at EOI. If EOI
  // advance the token by one codepoint
  count += 1;

  rec_ctx.mode = RecoveryMode::CodepointDiscard { start_offset, count };

  let ctx = &mut rec_ctx.ctx;

  if ctx.sym_ptr < input.len() {
    // Restore the context to the previous state
    ctx.push_state(last_state);
    ctx.is_finished = false;

    // advance the token by one codepoint
    ctx.sym_ptr += input.codepoint_len(ctx.sym_ptr) as usize;

    ctx.byte_len = 0;
    ctx.tok_byte_len = 0;

    contexts.push_back(rec_ctx);
  } else {
    pick_best_failure(best_failure, rec_ctx);
  }
}

fn drop_symbols(
  mut rec_ctx: Box<RecoverableContext>,
  contexts: &mut VecDeque<Box<RecoverableContext>>,
  best_failure: &mut Option<Box<RecoverableContext>>,
  count: usize,
  end_offset: usize,
) {
  let count = count + 1;

  loop {
    if let Some(token) = rec_ctx.symbols.pop() {
      match &token {
        CSTNode::Errata(tok) => {
          rec_ctx.entropy -= tok.length() as isize;
        }
        CSTNode::Skipped(tok) => {
          rec_ctx.entropy += tok.length() as isize;
        }
        CSTNode::MissingToken(.., state, tok) | CSTNode::Token(state, tok) => {
          match token {
            CSTNode::MissingToken(entropy, ..) => {
              rec_ctx.entropy -= entropy as isize;
            }
            CSTNode::Token(..) => {
              rec_ctx.entropy += tok.length() as isize;
            }
            _ => unreachable!(),
          }

          let TokenNode { offset, .. } = tok.as_ref();

          rec_ctx.mode = RecoveryMode::SymbolDiscard { count, end_offset, start_offset: *offset as usize };

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
          ctx.push_state(*state);
          ctx.is_finished = false;
          ctx.byte_len = 0;
          ctx.tok_byte_len = 0;
          contexts.push_back(rec_ctx);

          break;
        }
        CSTNode::NonTerm(..) | CSTNode::Multi(..) => {
          // Reduce or increase errata based on the contents of the node

          let mut queue = VecDeque::from_iter(vec![token]);
          let mut entropy_delta = 0;

          while let Some(token) = queue.pop_front() {
            match token {
              CSTNode::Errata(tok) => {
                entropy_delta -= tok.length() as isize;
              }
              CSTNode::MissingToken(entropy, ..) => {
                entropy_delta -= entropy as isize;
              }
              CSTNode::Skipped(tok) => {
                entropy_delta += tok.length() as isize;
              }
              tok @ CSTNode::Token(..) => {
                entropy_delta += tok.length() as isize;
              }
              CSTNode::NonTerm(node) => queue.extend(node.symbols.iter().cloned()),
              CSTNode::Multi(node) => queue.extend(node.alternatives.first().unwrap().symbols.iter().cloned()),
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

fn pick_best_failure(best_failure: &mut Option<Box<RecoverableContext>>, rec_ctx: Box<RecoverableContext>) {
  if let Some(failed_ctx) = best_failure.as_deref() {
    if failed_ctx.get_offset() < rec_ctx.get_offset() || failed_ctx.entropy > rec_ctx.entropy {
      *best_failure = Some(rec_ctx);
    }
  } else {
    *best_failure = Some(rec_ctx);
  }
}
