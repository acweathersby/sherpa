use crate::types::*;
use std::{
  collections::{hash_map::DefaultHasher, HashMap, HashSet, VecDeque},
  hash::Hasher,
  ops::Range,
  rc::Rc,
};

pub const CHAR_USAGE_SCORE: isize = 100;

pub trait ForkableParser<I: ParserInput>: ParserIterator<I> + ParserInitializer {
  fn fork_parse(&mut self, input: &mut I, entry: EntryPoint, store: &CSTStore) -> Result<(), ParserError> {
    let mut pending = ContextQueue::new_with_capacity(64)?;
    pending.push_back(Box::new(ForkContext {
      offset:  0,
      entropy: (input.len() as isize * CHAR_USAGE_SCORE),
      ctx:     self.init(entry)?,
      symbols: Vec::new(),
    }));
    pending.swap_buffers();

    let mut errored = Default::default();
    let mut completed = Default::default();

    while !pending.pop_is_empty() {
      fork_meta_kernel(input, self, &mut pending, &mut completed, &mut errored, store)?;
      pending.swap_buffers();
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
pub(crate) fn fork_meta_kernel<I: ParserInput, P: ForkableParser<I> + ?Sized, CTX: ForkableContext>(
  input: &mut I,
  parser: &mut P,
  pending: &mut ContextQueue<CTX>,
  completed: &mut Vec<CTX>,
  errored: &mut Vec<(ParserState, CTX)>,
  store: &CSTStore,
) -> Result<(), ParserError> {
  let mut least_advanced_reduction = usize::MAX;
  let mut min_advance = usize::MAX;
  let mut reduction_stage = VecDeque::new();

  while let Some(mut rec_ctx) = pending.pop_front() {
    if !rec_ctx.ctx().is_finished {
      if rec_ctx.ctx().sym_ptr > min_advance {
        pending.push_with_priority(rec_ctx);
        continue;
      }

      min_advance = min_advance.min(rec_ctx.ctx().sym_ptr);

      if let Some(action) = parser.next(input, rec_ctx.ctx_mut()) {
        match action {
          ParseAction::Skip { byte_offset, byte_length, token_id, .. } => {
            insert_node(Default::default(), &mut rec_ctx, create_skip(input, token_id, byte_length, byte_offset), store);
            pending.push_with_priority(rec_ctx);
          }

          ParseAction::Shift { byte_offset, byte_length, token_id, emitting_state, .. } => {
            insert_node(emitting_state, &mut rec_ctx, create_token(input, token_id, byte_length, byte_offset), store);
            pending.push_with_priority(rec_ctx);
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
              let mut new_ctx = rec_ctx.split();
              new_ctx.ctx_mut().push_state(state);
              pending.push_with_priority(new_ctx);
            }
          }

          ParseAction::Reduce { nonterminal_id, symbol_count, rule_id, .. } => {
            // Collect all tokens that belong to this nonterminal
            reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id, store);

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
                      Some((ParserState::default(), create_skip(input, token_id, byte_length, byte_offset))),
                    ));
                    break;
                  }

                  ParseAction::Shift { byte_offset, byte_length, token_id, emitting_state, .. } => {
                    reduction_stage.push_back((
                      non_terminal_id as u32,
                      rec_ctx,
                      Some((emitting_state, create_token(input, token_id, byte_length, byte_offset))),
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
                    reduce_symbols(symbol_count, &mut rec_ctx, nonterminal_id, rule_id, store);
                  }

                  ParseAction::Fork(states) => {
                    for state in states {
                      let mut new_ctx = rec_ctx.split();
                      new_ctx.ctx_mut().push_state(state);
                      pending.push_with_priority(new_ctx);
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
    for rec_ctx in attempt_merge(
      create_merge_groups(reduction_stage.drain(..).chain(completed.drain(..).map(|n| (0, n, None)))),
      "reduced",
      store,
    ) {
      if rec_ctx.ctx().is_finished {
        completed.push(rec_ctx);
      } else {
        pending.push_with_priority(rec_ctx);
      }
    }
  }

  Ok(())
}

pub fn insert_node<CTX: ForkableContext>(parser_state: ParserState, rec_ctx: &mut CTX, node: CSTNode, store: &CSTStore) {
  //rec_ctx.set_offset((node.offset() + node.len()) as usize);

  match &node {
    CSTNode::Token(tok) if tok.ty() == NodeType::Errata => {
      *rec_ctx.entropy_mut() += tok.len() as isize;
    }
    _ => {
      *rec_ctx.entropy_mut() -= node.len() as isize * CHAR_USAGE_SCORE;
    }
  }

  rec_ctx.symbols().push((parser_state, store.get_unique(node)));
}

fn create_skip<I: ParserInput>(input: &mut I, token_id: u32, token_byte_length: u32, token_byte_offset: u32) -> CSTNode {
  CSTNode::Token(TokenNode::skipped_type(
    token_id as u16,
    &input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
  ))
}

pub fn create_token<I: ParserInput>(input: &mut I, token_id: u32, token_byte_length: u32, token_byte_offset: u32) -> CSTNode {
  CSTNode::Token(TokenNode::token_type(
    token_id as u16,
    &input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
  ))
}

pub fn reduce_symbols<CTX: ForkableContext>(
  mut symbol_count: u32,
  rec_ctx: &mut CTX,
  nonterminal_id: u32,
  rule_id: u32,
  store: &CSTStore,
) {
  let mut symbols = vec![];

  while symbol_count > 0 {
    let (_, sym) = rec_ctx.symbols().pop().expect(&format!("Should have enough symbols to complete this Non-Terminal"));
    match sym.as_ref() {
      CSTNode::Token(tok) if matches!(tok.ty(), NodeType::Skipped | NodeType::Errata) => {}
      _ => {
        symbol_count -= 1;
      }
    }
    symbols.push(sym);
  }

  symbols.reverse();

  let length = symbols.iter().fold(0, |a, s| a + s.len());

  rec_ctx
    .symbols()
    .push((Default::default(), store.get_unique(NonTermNode::typed(nonterminal_id as u16, rule_id as u16, symbols, length))));
}

pub type MergeGroups<CTX> = HashMap<u64, Vec<MergeCandidate<CTX>>>;
pub struct MergeCandidate<CTX: ForkableContext> {
  ctx:       CTX,
  sym_range: Range<usize>,
  follow:    Option<(ParserState, CSTNode)>,
}

pub fn create_merge_groups<CTX: ForkableContext, I: Iterator<Item = (u32, CTX, Option<(ParserState, CSTNode)>)>>(
  follow_contexts: I,
) -> HashMap<u64, Vec<MergeCandidate<CTX>>> {
  let mut groups = MergeGroups::new();
  for (distinguisher, ctx, following) in follow_contexts {
    sort_candidate(distinguisher, following, ctx, &mut groups);
  }
  groups
}

pub fn sort_candidate<CTX: ForkableContext>(
  distinguisher: u32,
  follow: Option<(ParserState, CSTNode)>,
  mut ctx: CTX,
  groups: &mut MergeGroups<CTX>,
) {
  use std::hash::Hash;
  let (hash, entry) = if ctx.symbols().len() > 0 {
    let symbols = ctx.symbols();
    let (state, last) = symbols.last().expect("");
    let mut end_index = symbols.len() - 1;
    let mut start_offset = 0;

    for index in (0..=end_index).rev() {
      let (_, node) = &ctx.symbols()[index];
      end_index = index;
      if matches!(node.as_ref(), CSTNode::Token(..) | CSTNode::Alts(..) | CSTNode::NonTerm(..)) {
        break;
      }
    }

    let mut start_index = end_index;

    for index in (0..end_index).rev() {
      let (_, node) = &ctx.symbols()[index];
      if matches!(node.as_ref(), CSTNode::Token(..) | CSTNode::Alts(..) | CSTNode::NonTerm(..)) {
        break;
      } else {
        start_index = index;
      }
    }

    let mut hasher = DefaultHasher::new();
    ctx.symbols()[0..start_index].iter().for_each(|(_, a)| a.canonical_hash(&mut hasher));
    ctx.symbols()[end_index + 1..].iter().for_each(|(_, a)| a.dedup_hash(&mut hasher));
    follow.hash(&mut hasher);
    distinguisher.hash(&mut hasher);
    ctx.ctx().sym_ptr.hash(&mut hasher);
    ctx.ctx().stack.iter().for_each(|i| i.address.hash(&mut hasher));

    (hasher.finish(), MergeCandidate { ctx, follow, sym_range: start_index..(end_index + 1) })
  } else {
    let mut hasher = DefaultHasher::new();
    ctx.ctx().sym_ptr.hash(&mut hasher);
    ctx.ctx().stack.iter().for_each(|i| i.address.hash(&mut hasher));

    (hasher.finish(), MergeCandidate { ctx, follow, sym_range: 0..0 })
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

pub fn attempt_merge<'a, CTX: ForkableContext + 'a>(
  groups: MergeGroups<CTX>,
  merge_type: &'static str,
  store: &'a CSTStore,
) -> impl Iterator<Item = CTX> + 'a {
  groups.into_iter().map(move |(_, mut group)| {
    let mut lowest_entropy = isize::MAX;

    let (mut first, following) = if group.len() > 1 {
      group.sort_by(|a, b| a.ctx.entropy().cmp(&b.ctx.entropy()));
      struct AltCandidate<CTX: ForkableContext> {
        insert_point: usize,
        alt:          Vec<Rc<Alternative>>,
        follow:       Option<(ParserState, CSTNode)>,
        ctx:          CTX,
      }

      let mut iter = group.into_iter().map(|MergeCandidate { mut ctx, sym_range, follow }| {
        let insert_point = sym_range.start;
        let entropy = *ctx.entropy();
        let mut syms = ctx.symbols().drain(sym_range);

        let base_sym = syms.next();

        let alt = match (base_sym, syms) {
          (Some((_, node)), syms) => match node.as_ref() {
            CSTNode::Alts(multi) => multi.alternatives.clone(),
            _ => {
              let mut syms_ = vec![node];
              syms_.extend(syms.map(|(_, s)| s));
              let alt: Alternative = Alternative {
                length:  0, //ctx.last_tok_end - start_offset,
                symbols: syms_,
                entropy: entropy,
              };
              vec![Rc::new(alt)]
            }
          },
          _ => unreachable!(),
        };

        lowest_entropy = lowest_entropy.min(entropy);

        AltCandidate { insert_point, alt, ctx, follow: follow }
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
        let alt = alternates.pop().unwrap();
        for (offset, symbol) in alt.symbols.iter().enumerate() {
          ctx.symbols().insert(insert_point + offset, (Default::default(), symbol.clone()));
        }
      } else {
        alternates.sort_by(|a, b| a.entropy.cmp(&b.entropy));
        ctx.symbols().insert(insert_point, (Default::default(), store.get_unique(Alts::typed(alternates, merge_type))));
      }

      *ctx.entropy_mut() = lowest_entropy;

      (ctx, follow)
    } else {
      let MergeCandidate { ctx, follow, .. } = group.pop().expect("should be 1");
      (ctx, follow)
    };

    if let Some((state, following)) = following {
      insert_node(state, &mut first, following, store);
    }

    first
  })
}
