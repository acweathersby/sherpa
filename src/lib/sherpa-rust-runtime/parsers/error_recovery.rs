use crate::types::*;
use std::{
  cmp::Ordering,
  collections::{hash_map::DefaultHasher, HashMap, HashSet, VecDeque},
  fmt::Debug,
  hash::Hasher,
  ops::Range,
};

use super::Parser;

const TOKEN_SYNTHESIS_PENALTY: isize = 1;

/// Maximum number of subsequent synthetic tokens
const SYNTH_LIMIT: usize = 5;

#[derive(Debug, Clone, Copy)]
enum RecoveryMode {
  Normal,
  CodepointDiscard { start_offset: usize, count: usize },
  SymbolDiscard { start_offset: usize, end_offset: usize, count: usize },
  SyntheticInput { tok_id: u32, count: usize },
  Unrecoverable(usize),
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct RecoverableContext {
  entropy: isize,
  ctx: ParserContext,
  symbols: Vec<CSTNode>,
  mode: RecoveryMode,
  last_tok_end: u32,
  last_failed_state: ParserState,
}

impl PartialEq for RecoverableContext {
  fn eq(&self, other: &Self) -> bool {
    false
  }
}

impl PartialOrd for RecoverableContext {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a = (self.entropy, u32::MAX - self.last_tok_end);
    let b = (other.entropy, u32::MAX - other.last_tok_end);

    if a < b {
      Some(Ordering::Less)
    } else {
      Some(Ordering::Greater)
    }
  }
}

impl Eq for RecoverableContext {}
impl Ord for RecoverableContext {
  fn cmp(&self, other: &Self) -> Ordering {
    self.partial_cmp(other).unwrap()
  }
}

impl RecoverableContext {
  pub fn split(&self) -> Box<Self> {
    let mut ctx = self.ctx.clone();
    ctx.is_finished = false;
    Box::new(Self {
      ctx,
      mode: RecoveryMode::Unrecoverable(0),
      symbols: self.symbols.clone(),
      ..*self
    })
  }
}

pub trait ErrorRecoveringDatabase<I: ParserInput>: ParserProducer<I> + Sized {
  fn parse_with_recovery(&self, input: &mut I, entry: EntryPoint) -> Result<(), ParseError> {
    parse_with_recovery(input, entry, self)
  }
}

impl<I: ParserInput, T: ParserProducer<I> + Sized> ErrorRecoveringDatabase<I> for T {}

pub fn parse_with_recovery<I: ParserInput, DB: ParserProducer<I>>(
  input: &mut I,
  entry: EntryPoint,
  db: &DB,
) -> Result<(), ParseError> {
  let mut parser = db.get_parser()?;

  let mut active = VecDeque::from_iter(vec![Box::new(RecoverableContext {
    entropy: input.len() as isize,
    symbols: vec![],
    ctx: parser.init(entry)?,
    mode: RecoveryMode::Normal,
    last_tok_end: 0,
    last_failed_state: Default::default(),
  })]);

  let mut pending = VecDeque::<Box<RecoverableContext>>::new();
  let mut reduction_stage = VecDeque::new();
  let mut failed_contexts = VecDeque::new();
  let mut completed = Vec::new();
  let mut best_failure = None;

  loop {
    let mut least_advanced_reduction = u32::MAX;
    let mut min_advance = u32::MAX;
    println!(
      "{:?}",
      active.iter().map(|d| format!("{}-{}: {}", d.ctx.sym_ptr, d.last_tok_end, d.ctx.nonterm)).collect::<Vec<_>>()
    );
    while let Some(mut rec_ctx) = active.pop_front() {
      if !rec_ctx.ctx.is_finished {
        if rec_ctx.last_tok_end > min_advance {
          sort_and_enque(&mut pending, rec_ctx);
          continue;
        }

        min_advance = min_advance.min(rec_ctx.last_tok_end);

        if let Some(action) = parser.next(input, &mut rec_ctx.ctx) {
          match action {
            ParseAction::Skip { byte_offset, byte_length, token_id, .. } => {
              insert_node(&mut rec_ctx, create_skip(input, token_id, byte_length, byte_offset));
              sort_and_enque(&mut pending, rec_ctx);
            }

            ParseAction::Shift { byte_offset, byte_length, token_id, emitting_state, .. } => {
              insert_node(&mut rec_ctx, create_token(input, emitting_state, token_id, byte_length, byte_offset));
              sort_and_enque(&mut pending, rec_ctx);
            }

            ParseAction::Reduce { nonterminal_id, symbol_count, rule_id, .. } => {
              // Collect all tokens that belong to this nonterminal
              reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id);

              least_advanced_reduction = least_advanced_reduction.min(rec_ctx.last_tok_end);

              // Continue reducing this non-terminal until we encounter a non-reducing action.
              let mut non_terminal_id = nonterminal_id;
              loop {
                if let Some(action) = parser.next(input, &mut rec_ctx.ctx) {
                  match action {
                    ParseAction::Reduce { nonterminal_id, symbol_count, rule_id, .. } => {
                      non_terminal_id = nonterminal_id;
                      reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id);
                    }

                    ParseAction::Skip { byte_offset, byte_length, token_id, .. } => {
                      reduction_stage.push_back((
                        non_terminal_id as u32,
                        rec_ctx,
                        Some(create_skip(input, token_id, byte_length, byte_offset)),
                      ));
                      break;
                    }

                    ParseAction::Shift { byte_offset, byte_length, token_id, emitting_state, .. } => {
                      reduction_stage.push_back((
                        non_terminal_id as u32,
                        rec_ctx,
                        Some(create_token(input, emitting_state, token_id, byte_length, byte_offset)),
                      ));
                      break;
                    }

                    ParseAction::Error { last_state, .. } => {
                      rec_ctx.last_failed_state = last_state;
                      failed_contexts.push_back(rec_ctx);
                      break;
                    }

                    ParseAction::Accept { .. } => {
                      let diff = input.len() as isize - rec_ctx.last_tok_end as isize;

                      if diff > 0 && matches!(rec_ctx.mode, RecoveryMode::SyntheticInput { .. }) {
                        // Drop contexts that synthesize trailing tokens. We're not
                        // creating a fuzzer here.
                        drop(rec_ctx);
                      } else {
                        rec_ctx.entropy += diff;
                        reduction_stage.push_back((non_terminal_id as u32, rec_ctx, None));
                      }
                      break;
                    }

                    _ => unreachable!(),
                  }
                } else {
                  drop(rec_ctx);
                  break;
                }
              }
            }

            ParseAction::Error { last_state, .. } => {
              rec_ctx.last_failed_state = last_state;
              failed_contexts.push_back(rec_ctx);
            }

            ParseAction::Accept { .. } => {
              let diff = input.len() as isize - rec_ctx.last_tok_end as isize;

              if diff > 0 && matches!(rec_ctx.mode, RecoveryMode::SyntheticInput { .. }) {
                // Drop contexts that synthesize trailing tokens. We're not
                // creating a fuzzer here.
                drop(rec_ctx);
              } else {
                rec_ctx.entropy += diff;
                completed.push(rec_ctx);
              }
            }

            _ => {
              sort_and_enque(&mut pending, rec_ctx);
            }
          }
        }
      }
    }

    handle_failed_contexts(&mut failed_contexts, input, db, &mut best_failure, &mut parser, &mut pending);

    if reduction_stage.len() > 0 {
      for rec_ctx in
        attempt_merge(create_merge_groups(reduction_stage.drain(..).chain(completed.drain(..).map(|n| (0, n, None)))), "reduced")
      {
        if rec_ctx.ctx.is_finished {
          completed.push(rec_ctx);
        } else {
          sort_and_enque(&mut pending, rec_ctx);
        }
      }
    }

    if pending.len() == 0 {
      break;
    }

    std::mem::swap(&mut active, &mut pending);
  }

  completed.sort();

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
  failed_contexts: &mut VecDeque<(Box<RecoverableContext>)>,
  input: &mut I,
  db: &DB,
  best_failure: &mut Option<Box<RecoverableContext>>,
  parser: &mut Box<dyn Parser<I>>,
  pending: &mut VecDeque<Box<RecoverableContext>>,
) {
  if failed_contexts.len() > 0 {
    let mut to_process = VecDeque::new();

    for mut rec_ctx in attempt_merge(
      create_merge_groups(failed_contexts.drain(..).map(|s| (s.last_failed_state.address as u32, s, None))),
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
          let ctx = &rec_ctx.ctx;
          let sym_ptr = ctx.sym_ptr;

          // We fork our contexts into different recovery modes. This may create a large
          // parse tree, but we'll likely prune many of the recovery
          // paths as they become unrecoverable or score poorly relative to other paths.

          // We can do several actions to recover from this error.
          // ---------------------------------------------------------------
          // 1. Create a zero length token whose id matches one of the expected terminal
          //    at this point. The failing state address can be used as a key into a
          //    lookup table that contains all possible token id's expected at this point.

          inject_synthetics(&rec_ctx, db, last_state, &mut to_process);

          // ---------------------------------------------------------------
          // 2. Drop bytes/codepoints until incoming is positioned at the start of an
          //    acceptable token.
          if ctx.sym_ptr < end {
            drop_codepoints(rec_ctx.split(), &mut to_process, best_failure, input, 0, sym_ptr, last_state);
          }

          // ---------------------------------------------------------------
          // 3. Drop symbols until a state is reached where the input is accepted.
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

    let mut all: Vec<_> = pending.drain(..).chain(continued).collect();

    all.sort();

    for rec_ctx in all.into_iter().take(4) {
      sort_and_enque(pending, rec_ctx)
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
          reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id);
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

/// Inserts a node into the
fn insert_node(rec_ctx: &mut Box<RecoverableContext>, node: CSTNode) {
  match node {
    CSTNode::Errata(..) => {
      rec_ctx.last_tok_end = node.offset() + node.length();
      rec_ctx.entropy += node.length() as isize;
    }
    _ => {
      rec_ctx.last_tok_end = node.offset() + node.length();
      rec_ctx.entropy -= node.length() as isize;
    }
  }

  rec_ctx.symbols.push(node);
}

type MergeGroups = HashMap<u64, Vec<MergeCandidate>>;

struct MergeCandidate {
  ctx:          Box<RecoverableContext>,
  sym_range:    Range<usize>,
  start_offset: u32,
  follow:       Option<CSTNode>,
}

fn create_merge_groups<I: Iterator<Item = (u32, Box<RecoverableContext>, Option<CSTNode>)>>(
  follow_contexts: I,
) -> HashMap<u64, Vec<MergeCandidate>> {
  let mut groups = HashMap::new();
  for (distinguisher, ctx, following) in follow_contexts {
    sort_candidate(distinguisher, following, ctx, &mut groups);
  }
  groups
}

fn sort_candidate(distinguisher: u32, follow: Option<CSTNode>, ctx: Box<RecoverableContext>, groups: &mut MergeGroups) {
  use std::hash::Hash;
  let (hash, entry) = if ctx.symbols.len() > 0 {
    let last = ctx.symbols.last().expect("");
    let mut end_index = ctx.symbols.len() - 1;
    let mut start_offset = last.offset();

    for index in (0..=end_index).rev() {
      let node = &ctx.symbols[index];
      end_index = index;
      if matches!(node, CSTNode::Token(..) | CSTNode::MissingToken(..) | CSTNode::Multi(..) | CSTNode::NonTerm(..)) {
        break;
      }
    }

    let mut start_index = end_index;

    for index in (0..end_index).rev() {
      let node = &ctx.symbols[index];
      if matches!(
        node,
        CSTNode::Token(..) | CSTNode::Skipped(..) | CSTNode::MissingToken(..) | CSTNode::Multi(..) | CSTNode::NonTerm(..)
      ) {
        break;
      } else {
        start_offset = node.offset();
        start_index = index;
      }
    }

    let mut hasher = DefaultHasher::new();
    ctx.symbols[0..start_index].iter().for_each(|a| a.canonical_hash(&mut hasher));
    ctx.symbols[end_index + 1..].iter().for_each(|a| a.dedup_hash(&mut hasher));
    follow.hash(&mut hasher);
    distinguisher.hash(&mut hasher);
    ctx.last_tok_end.hash(&mut hasher);
    ctx.ctx.stack.iter().for_each(|i| i.address.hash(&mut hasher));

    (hasher.finish(), MergeCandidate { ctx, follow, start_offset, sym_range: start_index..(end_index + 1) })
  } else {
    let mut hasher = DefaultHasher::new();
    ctx.last_tok_end.hash(&mut hasher);
    ctx.ctx.stack.iter().for_each(|i| i.address.hash(&mut hasher));

    (hasher.finish(), MergeCandidate { ctx, follow, start_offset: 0, sym_range: 0..0 })
  };

  match groups.entry(hash) {
    std::collections::hash_map::Entry::Vacant(e) => {
      e.insert(vec![entry]);
    }
    std::collections::hash_map::Entry::Occupied(mut e) => {
      e.get_mut().push(entry);
    }
  }
}

fn attempt_merge(groups: MergeGroups, merge_type: &'static str) -> impl Iterator<Item = Box<RecoverableContext>> {
  groups.into_iter().map(move |(_, mut group)| {
    let mut lowest_entropy = isize::MAX;

    let (mut first, following) = if group.len() > 1 {
      group.sort_by(|a, b| a.ctx.entropy.cmp(&b.ctx.entropy));
      struct AltCandidate {
        insert_point: usize,
        alt:          Vec<Alternative>,
        follow:       Option<CSTNode>,
        ctx:          Box<RecoverableContext>,
      }

      let mut iter = group.into_iter().map(|MergeCandidate { start_offset, mut ctx, sym_range, follow }| {
        let insert_point = sym_range.start;
        let mut syms = ctx.symbols.drain(sym_range);

        let alt = match (syms.next(), syms) {
          (Some(CSTNode::Multi(multi)), _) => multi.alternatives.clone(),
          (Some(sym), syms) => {
            let mut syms_ = vec![sym];
            syms_.extend(syms);
            let alt: Alternative = Alternative {
              length:  0, //ctx.last_tok_end - start_offset,
              offset:  start_offset,
              symbols: syms_,
              entropy: ctx.entropy,
            };
            vec![alt]
          }
          _ => unreachable!(),
        };

        lowest_entropy = lowest_entropy.min(ctx.entropy);

        AltCandidate { insert_point, alt, ctx, follow }
      });

      let AltCandidate { insert_point, alt, follow, mut ctx } = iter.next().expect("should be at least one");

      let mut hash_cache = HashSet::new();

      let mut alternates: Vec<_> = vec![alt]
        .into_iter()
        .chain(iter.map(|AltCandidate { alt, .. }| alt))
        .flatten()
        .filter_map(|alt| {
          let mut hasher = DefaultHasher::default();
          alt.symbols.iter().for_each(|s| s.dedup_hash(&mut hasher));
          if hash_cache.insert(hasher.finish()) {
            Some(alt)
          } else {
            None
          }
        })
        .collect();

      if alternates.len() == 1 {
        for (offset, symbol) in alternates.pop().unwrap().symbols.into_iter().enumerate() {
          ctx.symbols.insert(insert_point + offset, symbol);
        }
      } else {
        alternates.sort_by(|a, b| a.entropy.cmp(&b.entropy));
        ctx.symbols.insert(insert_point, Multi::typed(alternates, merge_type));
      }

      ctx.entropy = lowest_entropy;

      (ctx, follow)
    } else {
      let MergeCandidate { ctx, follow, .. } = group.pop().expect("should be 1");
      (ctx, follow)
    };

    if let Some(following) = following {
      insert_node(&mut first, following);
    }

    first
  })
}

fn sort_and_enque(pending: &mut VecDeque<Box<RecoverableContext>>, rec_ctx: Box<RecoverableContext>) {
  pending.push_back(rec_ctx);
  let mut pends = pending.drain(..).collect::<Vec<_>>();
  pends.sort_by(|a, b| a.last_tok_end.cmp(&b.last_tok_end));
  pending.extend(pends)
}

pub fn create_token<I: ParserInput>(
  input: &mut I,
  emitting_state: ParserState,
  token_id: u32,
  token_byte_length: u32,
  token_byte_offset: u32,
) -> CSTNode {
  TokenNode::token_type(
    emitting_state,
    token_id,
    token_byte_length,
    token_byte_offset,
    input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
  )
}

fn create_skip<I: ParserInput>(input: &mut I, token_id: u32, token_byte_length: u32, token_byte_offset: u32) -> CSTNode {
  TokenNode::skipped_type(
    token_id,
    token_byte_length,
    token_byte_offset,
    input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
  )
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

fn reduce_symbols(mut symbol_count: u32, rec_ctx: &mut Box<RecoverableContext>, nonterminal_id: u32, rule_id: u32) {
  let mut symbols = vec![];

  while symbol_count > 0 {
    let sym = rec_ctx.symbols.pop().expect(&format!("Should have enough symbols to complete this Non-Terminal"));
    match sym {
      CSTNode::Errata { .. } | CSTNode::Skipped { .. } => {}
      _ => {
        symbol_count -= 1;
      }
    }
    symbols.push(sym);
  }

  symbols.reverse();

  let start = symbols.first().expect("Should have at least one symbol");
  let end = symbols.last().expect("Should have at least one symbol");

  let offset = start.offset();
  let length = end.offset() + end.length() - offset;

  rec_ctx.symbols.push(NonTermNode::typed(nonterminal_id as u16, rule_id as u16, symbols, offset, length));
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
        CSTNode::NonTerm(node) => {
          //rec_ctx.symbols.extend(node.symbols.clone())
        }
        CSTNode::Multi(node) => {
          if let Some(node) = node.alternatives.first() {
            rec_ctx.symbols.extend(node.symbols.clone())
          }
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
    if failed_ctx.last_tok_end < rec_ctx.last_tok_end || failed_ctx.entropy > rec_ctx.entropy {
      *best_failure = Some(rec_ctx);
    }
  } else {
    *best_failure = Some(rec_ctx);
  }
}
