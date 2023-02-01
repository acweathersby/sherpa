use crate::types::{ast::Reducer, *};

pub enum DebugEvent<'a, R: ByteReader, M> {
  ExecuteState {
    bc:      &'a [u32],
    offset:  usize,
    len:     usize,
    address: usize,
    ctx:     &'a ParseContext<R, M>,
  },
  ExecuteInstruction {
    bc:          &'a [u32],
    address:     usize,
    instruction: Instruction,
    ctx:         &'a ParseContext<R, M>,
  },
  SkipToken {
    offset: usize,
    len:    usize,
    ctx:    &'a ParseContext<R, M>,
  },
  ShiftToken {
    offset: usize,
    len:    usize,
    ctx:    &'a ParseContext<R, M>,
  },
}

pub type DebugFn<R, M> = dyn Fn(DebugEvent<R, M>);

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<'a, R: ByteReader + MutByteReader, M>(
  state: u32,
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
  debug: Option<&DebugFn<R, M>>,
) -> (ParseAction, u32) {
  use ParseAction::*;

  let mut s = state & STATE_ADDRESS_MASK;

  #[cfg(debug_assertions)]
  if let Some(debug) = debug {
    let (offset, len) =
      if ctx.in_peek_mode { (ctx.peek_off, ctx.peek_len) } else { (ctx.token_off, ctx.token_len) };
    debug(DebugEvent::ExecuteState {
      bc,
      address: s as usize,
      ctx,
      offset: offset as usize,
      len: len as usize,
    });
  }

  loop {
    let instruction = Instruction::from(bc, s as usize);

    #[cfg(debug_assertions)]
    if let Some(debug) = debug {
      debug(DebugEvent::ExecuteInstruction { bc, address: s as usize, instruction, ctx });
    }

    use InstructionType::*;

    s = match instruction.to_type() {
      Shift => {
        break shift(s, instruction, ctx, debug);
      }
      Goto => goto(s, instruction, stack),
      SetProd => set_production(s, instruction, ctx),
      Reduce => {
        break reduce(s, instruction, ctx);
      }
      Token => set_token_state(s, instruction, ctx),
      ForkTo => {
        break fork(s, instruction);
      }
      Scan => scan(),
      EatCrumbs => noop(s),
      VectorBranch => vector_jump(s, ctx, bc, debug),
      HashBranch => hash_jump(s, ctx, bc, debug),
      SetFailState => set_fail(),
      Repeat => repeat(),
      Noop13 => noop(s),
      AssertShift => DEFAULT_FAIL_INSTRUCTION_ADDRESS,
      Fail => break (FailState, 0),
      _ => break (CompleteState, 0),
    };
  }
}
/// Produces a parse action that
/// contains a token that is the
#[inline]
fn shift<'a, R: ByteReader + MutByteReader, M>(
  i: u32,
  instr: Instruction,
  ctx: &mut ParseContext<R, M>,
  debug: Option<&DebugFn<R, M>>,
) -> (ParseAction, u32) {
  if instr.get_value() & 0x1 == 1 {
    ctx.token_len = 0;
  }

  let action = ParseAction::Shift {
    anchor_byte_offset: ctx.anchor_off,
    token_byte_offset:  ctx.token_off,
    token_byte_length:  ctx.token_len,
    token_line_count:   ctx.start_line_num,
    token_line_offset:  ctx.start_line_off,
  };

  if ctx.is_scanner() {
    ctx.scan_off += ctx.scan_len;
    ctx.scan_len = 0;
    let scan_off = ctx.scan_off;
    let peek_line_num = ctx.scan_line_num;

    ctx.get_reader_mut().set_cursor_to(scan_off, peek_line_num, peek_line_num);
  } else {
    #[cfg(debug_assertions)]
    if let Some(debug) = debug {
      debug(DebugEvent::ShiftToken {
        offset: ctx.token_off as usize,
        len: ctx.token_len as usize,
        ctx,
      });
    }
    ctx.anchor_off = ctx.token_off + ctx.token_len;
    ctx.token_off = ctx.anchor_off;
    ctx.token_len = 0;
    ctx.tok_id = 0;
  }

  (action, i + 1)
}

#[inline]
fn reduce<'a, T: 'a + ByteReader + MutByteReader, M>(
  i: u32,
  instr: Instruction,
  ctx: &mut ParseContext<T, M>,
) -> (ParseAction, u32) {
  let symbol_count = instr.get_contents() >> 16 & 0x0FFF;
  let rule_id = instr.get_contents() & 0xFFFF;
  let production_id = ctx.get_production();

  (
    if symbol_count == 0x0FFF {
      todo!("Acquire symbol count from symbol accumulator");
    } else {
      ParseAction::Reduce { production_id, rule_id, symbol_count }
    },
    i + 1,
  )
}

#[inline]
fn goto(i: u32, instr: Instruction, stack: &mut Vec<u32>) -> u32 {
  stack.push(instr.get_value());
  i + 1
}

#[inline]
fn _eat_crumbs<'a, T: 'a + ByteReader + MutByteReader, R: ByteReader + MutByteReader, M>(
  i: u32,
  instr: Instruction,
  stack: &mut Vec<u32>,
) -> u32 {
  stack.push(instr.get_value());
  // The collapse function takes the current production, retasked
  // to be a `lane` selector, and compares that against the a
  // sentinal value. If the value matches the production,
  // then one of three things will occur:
  // 1. A shift is issued of length N, which is defined in
  //    the next instruction on the stack.
  // 2. A rule reduction is issued, with rule id and production
  //    id information stored in the next instruction on the
  //    stack.
  // 3. A lane merge occurs, and the production value is changed
  //    to the value of the new lane (which is really an earlier
  //    generation lane).
  // If the lane does not match, we still pop off the next value
  // and let the compiler proceed.
  i + 1
}

#[inline]
fn set_production<'a, T: 'a + ByteReader + MutByteReader, M>(
  i: u32,
  instr: Instruction,
  ctx: &mut ParseContext<T, M>,
) -> u32 {
  ctx.set_production_to(instr.get_contents());
  i + 1
}

#[inline]
fn set_token_state<'a, T: 'a + ByteReader + MutByteReader, M>(
  i: u32,
  instr: Instruction,
  ctx: &mut ParseContext<T, M>,
) -> u32 {
  ctx.tok_id = instr.get_contents() & 0x00FF_FFFF;

  ctx.scan_anchor_off = ctx.scan_off;

  i + 1
}

#[inline]
fn fork(i: u32, instr: Instruction) -> (ParseAction, u32) {
  let contents = instr.get_contents();

  let target_production = contents & 0xFFFF;

  let num_of_states = (contents >> 16) & 0xFFFF;

  (
    ParseAction::Fork { num_of_states, states_start_offset: i + 1, target_production },
    i + num_of_states + 1,
  )
}

fn scan() -> u32 {
  0
}

fn repeat() -> u32 {
  0
}

#[inline]
fn set_fail() -> u32 {
  0
}

#[inline]
fn noop(i: u32) -> u32 {
  i + 1
}

#[inline]
fn skip_token<'a, T: 'a + ByteReader + MutByteReader, M>(
  ctx: &mut ParseContext<T, M>,
  debug: Option<&DebugFn<T, M>>,
) {
  if ctx.is_scanner() {
    ctx.scan_off += ctx.scan_input_len;
    ctx.scan_input_len = 0;
  } else {
    if ctx.in_peek_mode() {
      #[cfg(debug_assertions)]
      if let Some(debug) = debug {
        debug(DebugEvent::SkipToken {
          offset: ctx.peek_off as usize,
          len: ctx.peek_len as usize,
          ctx,
        });
      }
      ctx.peek_off += ctx.peek_len;
      ctx.peek_len = 0;
    } else {
      #[cfg(debug_assertions)]
      if let Some(debug) = debug {
        debug(DebugEvent::SkipToken {
          offset: ctx.token_off as usize,
          len: ctx.token_len as usize,
          ctx,
        });
      }
      ctx.token_off += ctx.token_len;
      ctx.token_len = 0;
    }
    ctx.tok_id = 0;
  }
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<R: ByteReader + MutByteReader, M>(
  i: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: Option<&DebugFn<R, M>>,
) -> u32 {
  let i = i as usize;
  // Decode data

  let TableHeaderData {
    input_type,
    lexer_type,
    table_meta: modulo_base,
    scan_state_entry_instruction: scan_index,
    ..
  } = TableHeaderData::from_bytecode(i, bc);

  let hash_mask = (1 << modulo_base) - 1;
  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      InputType::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(lexer_type, input_type, scan_index.get_address() as u32, ctx, bc, debug)
        as u32,
    };
    let mut hash_index = (input_value & hash_mask) as usize;

    loop {
      let cell = unsafe { *bc.get_unchecked(table_start + hash_index) };
      let value = cell & 0x7FF;
      let off = (cell >> 11) & 0x7FF;
      let next = ((cell >> 22) & 0x3FF) as i32 - 512;

      if value == input_value {
        if off == 0x7FF {
          skip_token(ctx, debug);
          break;
        } else {
          return i as u32 + off;
        }
      } else if next != 0 {
        hash_index = ((hash_index as i32) + next) as usize;
      } else {
        return i as u32 + bc[i + 3];
      }
    }
  }
}
#[inline]
pub fn vector_jump<R: ByteReader + MutByteReader, M>(
  i: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: Option<&DebugFn<R, M>>,
) -> u32 {
  let i = i as usize;
  // Decode data

  let TableHeaderData {
    input_type,
    lexer_type,
    table_length,
    table_meta: value_offset,
    scan_state_entry_instruction: scan_index,
  } = TableHeaderData::from_bytecode(i, bc);

  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      InputType::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(lexer_type, input_type, scan_index.get_address() as u32, ctx, bc, debug)
        as u32,
    };

    let value_index = (input_value as i32 - value_offset as i32) as u32;

    if value_index < table_length {
      let off = unsafe { *bc.get_unchecked(table_start + value_index as usize) };
      if off == 0xFFFF_FFFF {
        skip_token(ctx, debug);
        continue;
      } else {
        return i as u32 + off;
      }
    } else {
      return i as u32 + bc[i + 3];
    }
  }
}

#[inline]
fn get_token_value<R: ByteReader + MutByteReader, M>(
  lex_type: u32,
  input_type: u32,
  scan_index: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: Option<&DebugFn<R, M>>,
) -> i32 {
  match input_type {
    InputType::T03_CLASS => {
      debug_assert!(ctx.is_scanner());
      let scan_len = ctx.get_reader().codepoint_byte_length();
      ctx.scan_len = scan_len;
      ctx.get_reader().class() as i32
    }

    InputType::T04_CODEPOINT => {
      debug_assert!(ctx.is_scanner());
      let scan_len = ctx.get_reader().codepoint_byte_length();
      ctx.scan_len = scan_len;
      ctx.get_reader().codepoint() as i32
    }

    InputType::T05_BYTE => {
      debug_assert!(ctx.is_scanner());
      ctx.scan_len = 1;
      ctx.get_reader().byte() as i32
    }

    _ => {
      debug_assert!(!ctx.is_scanner());

      let active_offset = match lex_type {
        LexerType::PEEK => {
            let (_, len) = if ctx.in_peek_mode() {
                (ctx.peek_off, ctx.peek_len)
            } else {
              ctx.peek_off = ctx.token_off;
              (ctx.token_off, ctx.token_len)
            };

            ctx.peek_off += len;
            ctx.tok_id = 0;

            ctx.set_peek_mode_to(true);

            let peek_off = ctx.peek_off;
            let peek_line_num = ctx.scan_line_num;
            let peek_line_off = ctx.scan_line_off;

            ctx.get_reader_mut().set_cursor_to(peek_off, peek_line_num, peek_line_off);

            ctx.peek_off
        }
        _ /*| LEXER_TYPE::ASSERT*/ => {
            if ctx.in_peek_mode() {
                ctx.set_peek_mode_to(false);

                let token_off = ctx.token_off;
                let tok_line_off = ctx.start_line_off;
                let tok_line_num = ctx.start_line_num;
                ctx.tok_id = 0;

                ctx.get_reader_mut().set_cursor_to(token_off, tok_line_off, tok_line_num);
            }
            ctx.token_off
        }
    };

      if ctx.tok_id == 0 {
        let token_length = token_scan(active_offset, scan_index, ctx, bc, debug);

        if ctx.in_peek_mode() {
          ctx.peek_len = token_length;
        } else {
          ctx.token_len = token_length;
        }
      }

      ctx.tok_id as i32
    }
  }
}

fn _scan_for_improvised_token<R: ByteReader + MutByteReader, M>(ctx: &mut ParseContext<R, M>) {
  let byte = ctx.get_reader().byte();
  let mut off = ctx.get_reader().cursor() as u32;
  // Scan to next break point and produce an undefined
  // token. If we are already at a break point then just
  // return the single character token.
  if byte == (b'\n' as u32)
    || byte == (b'\t' as u32)
    || byte == (b'\r' as u32)
    || byte == (b' ' as u32)
  {
    off += 1;
  } else {
    let byte = ctx.get_reader().byte();
    while byte != b'\n' as u32
      && byte != b'\t' as u32
      && byte != b'\r' as u32
      && byte != b' ' as u32
      && !ctx.get_reader().at_end()
    {
      off += ctx.get_reader().codepoint_byte_length();
      ctx.get_reader_mut().set_cursor_to(off, 0, 0);
    }
  }

  ctx.scan_anchor_off = off;

  set_token_state(0, Instruction::default(), ctx);
}

fn token_scan<R: ByteReader + MutByteReader, M>(
  offset: u32,
  scan_index: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: Option<&DebugFn<R, M>>,
) -> u32 {
  ctx.tok_id = 0;

  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  if ctx.get_reader().at_end() {
    return 0;
  }

  // Initialize Scanner

  // We are done with the state reference, so we
  // invalidate variable by moving the reference to
  // an unused name to prevent confusion with the
  // `scan_state` variable.
  let mut stack = vec![0, scan_index | NORMAL_STATE_FLAG];
  ctx.set_is_scanner(true);

  match {
    let mut state = stack.pop().unwrap();
    let ctx_fail_mode = ctx.in_fail_mode();

    let line_data = ctx.get_reader().get_line_data();
    let (line_num, line_count) = ((line_data >> 32) as u32, (line_data & 0xFFFF_FFFF) as u32);

    ctx.scan_line_num = line_num;
    ctx.scan_line_off = line_count;
    ctx.scan_anchor_off = offset;
    ctx.scan_off = offset;

    loop {
      if state < 1 {
        ctx.set_fail_mode_to(ctx_fail_mode);
        break Some(ctx.scan_anchor_off - offset);
      } else {
        let mask_gate = NORMAL_STATE_FLAG << (ctx.in_fail_mode() as u32);

        if (state & mask_gate) != 0 {
          let fail_mode = loop {
            match dispatch(state, ctx, &mut stack, bc, debug) {
              (ParseAction::CompleteState, _) => {
                break false;
              }

              (ParseAction::FailState, _) => {
                break true;
              }
              (_, next_state) => state = next_state,
            }
          };
          ctx.set_fail_mode_to(fail_mode);
        }
        state = stack.pop().unwrap();
      }
    }
  } {
    Some(token_length) => {
      ctx.set_is_scanner(false);
      token_length
    }
    _ => panic!("Unusable State"),
  }
}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<'a, R: ByteReader + MutByteReader, M>(
  ctx: &'a mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
  debug: Option<&DebugFn<R, M>>,
) -> ParseAction {
  let mut state = stack.pop().unwrap();

  loop {
    if state < 1 {
      let off = ctx.token_off;
      if ctx.get_reader().offset_at_end(off) {
        break ParseAction::Accept { production_id: ctx.get_production() };
      } else {
        break ParseAction::Error {
          last_production: ctx.get_production(),
          last_input:      TokenRange {
            len:      ctx.token_len,
            off:      ctx.token_off,
            line_num: ctx.start_line_num,
            line_off: ctx.start_line_off,
          },
        };
      }
    } else {
      let mask_gate = NORMAL_STATE_FLAG << (ctx.in_fail_mode() as u32);

      if (state & mask_gate) != 0 {
        match dispatch(state, ctx, stack, bc, debug) {
          (ParseAction::CompleteState, _) => {
            ctx.set_fail_mode_to(false);
            state = stack.pop().unwrap();
          }
          (ParseAction::FailState, _) => {
            ctx.set_fail_mode_to(true);
            state = stack.pop().unwrap();
          }
          (action, next_state) => {
            stack.push(next_state | NORMAL_STATE_FLAG);
            break action;
          }
        }
      } else {
        state = stack.pop().unwrap();
      }
    }
  }
}

pub fn parse_ast<R: ByteReader + MutByteReader, M, Node: AstObject>(
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
  reducers: &[Reducer<R, M, Node>],
  debug: Option<&DebugFn<R, M>>,
) -> Result<AstSlot<Node>, SherpaParseError> {
  let mut ast_stack: Vec<AstSlot<Node>> = vec![];
  loop {
    match get_next_action(ctx, stack, bc, debug) {
      ParseAction::Accept { .. } => {
        return Ok(ast_stack.pop().unwrap());
      }
      ParseAction::Reduce { rule_id, symbol_count, .. } => {
        let reduce_fn = reducers[rule_id as usize];
        let len = ast_stack.len();
        let count = symbol_count as usize;
        reduce_fn(&ctx, &AstStackSlice::from_slice(&mut ast_stack[(len - count)..len]));
        ast_stack.resize(len - (count - 1), AstSlot::<Node>::default());
      }
      ParseAction::Shift {
        anchor_byte_offset,
        token_byte_offset,
        token_byte_length,
        token_line_offset,
        token_line_count,
      } => {
        let peek = TokenRange {
          len: token_byte_offset - anchor_byte_offset,
          off: anchor_byte_offset,
          ..Default::default()
        };

        let tok = TokenRange {
          len:      token_byte_length,
          off:      token_byte_offset,
          line_num: token_line_count,
          line_off: token_line_offset,
        };
        ast_stack.push(AstSlot(Node::default(), tok, peek));
      }
      ParseAction::Error { .. } => {
        return Err(SherpaParseError {
          inline_message: Default::default(),
          last_production: 0,
          loc: Default::default(),
          message: Default::default(),
        });
      }
      _ => {
        return Err(SherpaParseError {
          inline_message: Default::default(),
          last_production: 0,
          loc: Default::default(),
          message: Default::default(),
        });
      }
    }
  }
}
