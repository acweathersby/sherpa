use crate::types::*;
use std::{
  collections::{hash_map::DefaultHasher, HashMap, HashSet, VecDeque},
  hash::Hasher,
  ops::Range,
};

pub trait ForkableParser<I: ParserInput>: ParserIterator<I> + ParserInitializer {
  fn fork_parse(&mut self, input: &mut I, entry: EntryPoint) -> Result<(), ParseError> {
    let f_ctx = Box::new(ForkContext {
      entropy: (input.len() as isize),
      ctx:     self.init(entry)?,
      symbols: Vec::new(),
    });

    let mut pending = VecDeque::from_iter(vec![f_ctx]);
    let mut errored = Default::default();
    let mut completed = Default::default();

    while pending.len() > 0 {
      fork_kernel(input, self, &mut pending, &mut completed, &mut errored)?;
    }

    #[cfg(debug_assertions)]
    dbg!(completed);

    Ok(())
  }
}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> ForkableParser<I> for T {}

/// Takes a series of in progress contexts and attempts to advances them by at
/// least one parse action, placing them back into the pending queue, the
/// `completed` if they have been completed, or `errored` if an
/// error was encountered.
pub(crate) fn fork_kernel<I: ParserInput, P: ForkableParser<I> + ?Sized, CTX: ForkableContext>(
  input: &mut I,
  parser: &mut P,
  pending: &mut VecDeque<CTX>,
  completed: &mut Vec<CTX>,
  errored: &mut Vec<(ParserState, CTX)>,
) -> Result<(), ParseError> {
  let mut pending_array = pending.drain(..).rev().collect::<Vec<_>>();
  let mut least_advanced_reduction = usize::MAX;
  let mut min_advance = usize::MAX;
  let mut reduction_stage = VecDeque::new();

  while let Some(mut rec_ctx) = pending_array.pop() {
    if !rec_ctx.ctx().is_finished {
      if rec_ctx.ctx().anchor_ptr > min_advance {
        sort_and_enque(pending, rec_ctx);
        continue;
      }

      min_advance = min_advance.min(rec_ctx.ctx().anchor_ptr);

      if let Some(action) = parser.next(input, rec_ctx.ctx_mut()) {
        match action {
          ParseAction::Skip { byte_offset, byte_length, token_id, .. } => {
            insert_node(&mut rec_ctx, create_skip(input, token_id, byte_length, byte_offset));
            sort_and_enque(pending, rec_ctx);
          }

          ParseAction::Shift { byte_offset, byte_length, token_id, emitting_state, .. } => {
            insert_node(&mut rec_ctx, create_token(input, emitting_state, token_id, byte_length, byte_offset));
            sort_and_enque(pending, rec_ctx);
          }

          ParseAction::Error { last_state, .. } => {
            errored.push((last_state, rec_ctx));
          }

          ParseAction::Accept { .. } => {
            let diff = input.len() as isize - rec_ctx.ctx().sym_ptr as isize;
            *rec_ctx.entropy_mut() += diff;
            completed.push(rec_ctx);
          }

          ParseAction::Fork(states) => {
            for state in states {
              let mut new_state = rec_ctx.split();
              new_state.ctx_mut().push_state(state);
              sort_and_enque(pending, new_state)
            }
          }

          ParseAction::Reduce { nonterminal_id, symbol_count, rule_id, .. } => {
            // Collect all tokens that belong to this nonterminal
            reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id);

            least_advanced_reduction = least_advanced_reduction.min(rec_ctx.ctx().sym_ptr);

            // Continue reducing this non-terminal until we encounter a non-reducing action.
            let mut non_terminal_id = nonterminal_id;

            loop {
              if let Some(action) = parser.next(input, rec_ctx.ctx_mut()) {
                match action {
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
                    errored.push((last_state, rec_ctx));
                    break;
                  }

                  ParseAction::Accept { .. } => {
                    let diff = input.len() as isize - rec_ctx.ctx().sym_ptr as isize;
                    *rec_ctx.entropy_mut() += diff;
                    reduction_stage.push_back((non_terminal_id as u32, rec_ctx, None));
                    break;
                  }

                  ParseAction::Reduce { nonterminal_id, symbol_count, rule_id, .. } => {
                    non_terminal_id = nonterminal_id;
                    reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id);
                  }

                  ParseAction::Fork(states) => {
                    for state in states {
                      let mut new_state = rec_ctx.split();
                      new_state.ctx_mut().push_state(state);
                      sort_and_enque(pending, new_state)
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

          _ => unreachable!(),
        }
      }
    }
  }

  if reduction_stage.len() > 0 {
    for rec_ctx in
      attempt_merge(create_merge_groups(reduction_stage.drain(..).chain(completed.drain(..).map(|n| (0, n, None)))), "reduced")
    {
      if rec_ctx.ctx().is_finished {
        completed.push(rec_ctx);
      } else {
        sort_and_enque(pending, rec_ctx);
      }
    }
  }

  Ok(())
}

fn sort_and_enque<CTX: ForkableContext>(pending: &mut VecDeque<CTX>, rec_ctx: CTX) {
  pending.push_back(rec_ctx);
  let mut pends = pending.drain(..).collect::<Vec<_>>();
  pends.sort_by(|a, b| a.ctx().sym_ptr.cmp(&b.ctx().sym_ptr));
  pending.extend(pends)
}

fn insert_node<CTX: ForkableContext>(rec_ctx: &mut CTX, node: CSTNode) {
  match node {
    CSTNode::Errata(..) => {
      //rec_ctx.last_tok_end = node.offset() + node.length();
      *rec_ctx.entropy_mut() += node.length() as isize;
    }
    _ => {
      //rec_ctx.last_tok_end = node.offset() + node.length();
      *rec_ctx.entropy_mut() -= node.length() as isize;
    }
  }

  rec_ctx.symbols().push(node);
}

fn create_skip<I: ParserInput>(input: &mut I, token_id: u32, token_byte_length: u32, token_byte_offset: u32) -> CSTNode {
  TokenNode::skipped_type(
    token_id,
    token_byte_length,
    token_byte_offset,
    input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
  )
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

fn reduce_symbols<CTX: ForkableContext>(mut symbol_count: u32, rec_ctx: &mut CTX, nonterminal_id: u32, rule_id: u32) {
  let mut symbols = vec![];

  while symbol_count > 0 {
    let sym = rec_ctx.symbols().pop().expect(&format!("Should have enough symbols to complete this Non-Terminal"));
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

  rec_ctx.symbols().push(NonTermNode::typed(nonterminal_id as u16, rule_id as u16, symbols, offset, length));
}

type MergeGroups<CTX> = HashMap<u64, Vec<MergeCandidate<CTX>>>;
struct MergeCandidate<CTX: ForkableContext> {
  ctx:          CTX,
  sym_range:    Range<usize>,
  start_offset: u32,
  follow:       Option<CSTNode>,
}

fn create_merge_groups<CTX: ForkableContext, I: Iterator<Item = (u32, CTX, Option<CSTNode>)>>(
  follow_contexts: I,
) -> HashMap<u64, Vec<MergeCandidate<CTX>>> {
  let mut groups = MergeGroups::new();
  for (distinguisher, ctx, following) in follow_contexts {
    sort_candidate(distinguisher, following, ctx, &mut groups);
  }
  groups
}

fn sort_candidate<CTX: ForkableContext>(
  distinguisher: u32,
  follow: Option<CSTNode>,
  mut ctx: CTX,
  groups: &mut MergeGroups<CTX>,
) {
  use std::hash::Hash;
  let (hash, entry) = if ctx.symbols().len() > 0 {
    let symbols = ctx.symbols();
    let last = symbols.last().expect("");
    let mut end_index = symbols.len() - 1;
    let mut start_offset = last.offset();

    for index in (0..=end_index).rev() {
      let node = &ctx.symbols()[index];
      end_index = index;
      if matches!(node, CSTNode::Token(..) | CSTNode::MissingToken(..) | CSTNode::Multi(..) | CSTNode::NonTerm(..)) {
        break;
      }
    }

    let mut start_index = end_index;

    for index in (0..end_index).rev() {
      let node = &ctx.symbols()[index];
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
    ctx.symbols()[0..start_index].iter().for_each(|a| a.canonical_hash(&mut hasher));
    ctx.symbols()[end_index + 1..].iter().for_each(|a| a.dedup_hash(&mut hasher));
    follow.hash(&mut hasher);
    distinguisher.hash(&mut hasher);
    ctx.ctx().sym_ptr.hash(&mut hasher);
    ctx.ctx().stack.iter().for_each(|i| i.address.hash(&mut hasher));

    (hasher.finish(), MergeCandidate { ctx, follow, start_offset, sym_range: start_index..(end_index + 1) })
  } else {
    let mut hasher = DefaultHasher::new();
    ctx.ctx().sym_ptr.hash(&mut hasher);
    ctx.ctx().stack.iter().for_each(|i| i.address.hash(&mut hasher));

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

fn attempt_merge<CTX: ForkableContext>(groups: MergeGroups<CTX>, merge_type: &'static str) -> impl Iterator<Item = CTX> {
  groups.into_iter().map(move |(_, mut group)| {
    let mut lowest_entropy = isize::MAX;

    let (mut first, following) = if group.len() > 1 {
      group.sort_by(|a, b| a.ctx.entropy().cmp(&b.ctx.entropy()));
      struct AltCandidate<CTX: ForkableContext> {
        insert_point: usize,
        alt:          Vec<Alternative>,
        follow:       Option<CSTNode>,
        ctx:          CTX,
      }

      let mut iter = group.into_iter().map(|MergeCandidate { start_offset, mut ctx, sym_range, follow }| {
        let insert_point = sym_range.start;
        let entropy = *ctx.entropy();
        let mut syms = ctx.symbols().drain(sym_range);

        let alt = match (syms.next(), syms) {
          (Some(CSTNode::Multi(multi)), _) => multi.alternatives.clone(),
          (Some(sym), syms) => {
            let mut syms_ = vec![sym];
            syms_.extend(syms);
            let alt: Alternative = Alternative {
              length:  0, //ctx.last_tok_end - start_offset,
              offset:  start_offset,
              symbols: syms_,
              entropy: entropy,
            };
            vec![alt]
          }
          _ => unreachable!(),
        };

        lowest_entropy = lowest_entropy.min(entropy);

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
          ctx.symbols().insert(insert_point + offset, symbol);
        }
      } else {
        alternates.sort_by(|a, b| a.entropy.cmp(&b.entropy));
        ctx.symbols().insert(insert_point, Multi::typed(alternates, merge_type));
      }

      *ctx.entropy_mut() = lowest_entropy;

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
