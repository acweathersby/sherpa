use crate::types::*;

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  r: &mut T,
  ctx: &mut LLVMParseContext<T, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
) -> ParseAction {
  use ParseAction::*;

  let mut i = ctx.get_active_state() & STATE_ADDRESS_MASK;

  loop {
    let instr = INSTRUCTION::from(bc, i as usize);

    ctx.set_active_state_to(i);

    use InstructionType::*;

    i = match instr.to_type() {
      Shift => {
        let (action, i) = shift(i, instr, ctx, r);
        ctx.set_active_state_to(i);
        break action;
      }
      Goto => goto(i, instr, stack),
      SetProd => set_production(i, instr, ctx),
      Reduce => {
        let (action, i) = reduce(i, instr, ctx);
        ctx.set_active_state_to(i);
        break action;
      }
      Token => set_token_state(i, instr, ctx),
      ForkTo => {
        let (action, i) = fork(i, instr);
        ctx.set_active_state_to(i);
        break action;
      }
      Scan => scan(),
      EatCrumbs => noop(i),
      VectorBranch => vector_jump(i, r, ctx, bc),
      HashBranch => hash_jump(i, r, ctx, bc),
      SetFailState => set_fail(),
      Repeat => repeat(),
      Noop13 => noop(i),
      AssertShift => DEFAULT_FAIL_INSTRUCTION_ADDRESS,
      Fail => break FailState,
      _ => break CompleteState,
    };
  }
}
/// Produces a parse action that
/// contains a token that is the
#[inline]
fn shift<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut LLVMParseContext<T, M>,
  r: &mut T,
) -> (ParseAction, u32) {
  if instr.get_value() & 0x1 == 1 {
    ctx.tok_input_len = 0;
  }

  let action = ParseAction::Shift {
    anchor_byte_offset: ctx.anchor_off,
    token_byte_offset:  ctx.token_off,
    token_byte_length:  ctx.tok_input_len,
    token_line_count:   ctx.tok_line_num,
    token_line_offset:  ctx.tok_line_off,
  };

  if ctx.is_scanner() {
    ctx.scan_off += ctx.tok_input_len;
    ctx.scan_len = 0;
    r.set_cursor_to(ctx.scan_off, ctx.peek_line_num, ctx.peek_line_num);
  } else {
    ctx.anchor_off = ctx.token_off + ctx.tok_input_len;
    ctx.token_off = ctx.anchor_off;
    ctx.tok_input_len = 0;
  }

  (action, i + 1)
}

fn next_token(shift: (TokenRange, u32)) -> (TokenRange, u32) {
  let mut next_token = shift.0;
  next_token.off += next_token.len;
  next_token.len = 0;
  (next_token, shift.1)
}

#[inline]
fn reduce<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut LLVMParseContext<T, M>,
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
fn goto(i: u32, instr: INSTRUCTION, stack: &mut Vec<u32>) -> u32 {
  stack.push(instr.get_value());
  i + 1
}

#[inline]
fn _eat_crumbs<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  instr: INSTRUCTION,
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
fn set_production<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut LLVMParseContext<T, M>,
) -> u32 {
  ctx.set_production_to(instr.get_contents());
  i + 1
}

#[inline]
fn set_token_state<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut LLVMParseContext<T, M>,
) -> u32 {
  ctx.tok_id = instr.get_contents() & 0x00FF_FFFF;

  ctx.scan_anchor_off = ctx.scan_off;

  i + 1
}

#[inline]
fn fork(i: u32, instr: INSTRUCTION) -> (ParseAction, u32) {
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
fn skip_token<T: LLVMByteReader + ByteReader + MutByteReader, M>(ctx: &mut LLVMParseContext<T, M>) {
  if ctx.is_scanner() {
    ctx.scan_off += ctx.scan_input_len;
    ctx.scan_input_len = 0;
  } else {
    if ctx.in_peek_mode() {
      ctx.peek_off += ctx.peek_input_len;
      ctx.peek_input_len = 0;
    } else {
      ctx.token_off += ctx.tok_input_len;
      ctx.tok_input_len = 0;
    }
  }
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  r: &mut T,
  ctx: &mut LLVMParseContext<T, M>,
  bc: &[u32],
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
      _ => {
        get_token_value(lexer_type, input_type, r, scan_index.get_address() as u32, ctx, bc) as u32
      }
    };
    let mut hash_index = (input_value & hash_mask) as usize;

    loop {
      let cell = unsafe { *bc.get_unchecked(table_start + hash_index) };
      let value = cell & 0x7FF;
      let off = (cell >> 11) & 0x7FF;
      let next = ((cell >> 22) & 0x3FF) as i32 - 512;

      if value == input_value {
        if off == 0x7FF {
          skip_token(ctx);
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
pub fn vector_jump<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  i: u32,
  r: &mut T,
  ctx: &mut LLVMParseContext<T, M>,
  bc: &[u32],
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
      _ => {
        get_token_value(lexer_type, input_type, r, scan_index.get_address() as u32, ctx, bc) as u32
      }
    };

    let value_index = (input_value as i32 - value_offset as i32) as u32;

    if value_index < table_length {
      let off = unsafe { *bc.get_unchecked(table_start + value_index as usize) };
      if off == 0xFFFF_FFFF {
        skip_token(ctx);
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
fn get_token_value<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  lex_type: u32,
  input_type: u32,
  r: &mut T,
  scan_index: u32,
  ctx: &mut LLVMParseContext<T, M>,
  bc: &[u32],
) -> i32 {
  match input_type {
    InputType::T03_CLASS => {
      debug_assert!(ctx.is_scanner());
      ctx.scan_len = r.codepoint_byte_length();
      r.class() as i32
    }

    InputType::T04_CODEPOINT => {
      debug_assert!(ctx.is_scanner());
      ctx.scan_len = r.codepoint_byte_length();
      r.codepoint() as i32
    }

    InputType::T05_BYTE => {
      debug_assert!(ctx.is_scanner());
      ctx.scan_len = 1;
      r.byte() as i32
    }

    _ => {
      debug_assert!(!ctx.is_scanner());

      let active_offset = match lex_type {
        LexerType::PEEK => {
            let (offset, len) = if ctx.in_peek_mode() {
                (ctx.peek_off, ctx.peek_input_len)
            } else {
              ctx.peek_off = ctx.token_off;
              (ctx.token_off, ctx.tok_input_len)
            };

            ctx.peek_off += len;

            ctx.set_peek_mode_to(true);

            r.set_cursor_to(ctx.peek_off, ctx.peek_line_num, ctx.peek_line_off);

            ctx.peek_off
        }
        _ /*| LEXER_TYPE::ASSERT*/ => {
            if ctx.in_peek_mode() {
                ctx.set_peek_mode_to(false);

                r.set_cursor_to(ctx.token_off, ctx.tok_line_off, ctx.tok_line_num);
            }
            ctx.token_off
        }
    };

      let token_length = token_scan(active_offset, scan_index, ctx, r, bc);

      if ctx.in_peek_mode() {
        ctx.peek_input_len = token_length;
      } else {
        ctx.tok_input_len = token_length;
      }

      ctx.tok_id as i32
    }
  }
}

fn scan_for_improvised_token<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  scan_ctx: &mut LLVMParseContext<T, M>,
  r: &mut T,
) {
  let mut byte = r.byte();
  let mut off = r.cursor() as u32;
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
    while byte != b'\n' as u32
      && byte != b'\t' as u32
      && byte != b'\r' as u32
      && byte != b' ' as u32
      && !r.at_end()
    {
      off += r.codepoint_byte_length();
    }
  }

  scan_ctx.scan_anchor_off = off;

  set_token_state(0, INSTRUCTION::default(), scan_ctx);
}

fn token_scan<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  offset: u32,
  scan_index: u32,
  ctx: &mut LLVMParseContext<T, M>,
  r: &mut T,
  bc: &[u32],
) -> u32 {
  if ctx.tok_id != 0 {
    return offset;
  }
  ctx.tok_id = 0;

  #[cfg(test)]
  {
    if scan_index == 0 {
      // Use a RNG to produce a token that is assigned the value
      // of state production id, decrementing production_id each time
      // we hit this function

      let id = ctx.get_production();

      if id > 0 {
        ctx.set_production_to(id - 1);
      }

      return (Default::default(), END_OF_INPUT_TOKEN_ID);
    }
  }

  r.set_cursor_to(offset, 0, 0);

  if r.at_end() {
    return 0;
  }

  // Initialize Scanner

  // We are done with the state reference, so we
  // invalidate variable by moving the reference to
  // an unused name to prevent confusion with the
  // `scan_state` variable.
  let mut stack = vec![0, scan_index];

  match {
    let mut state = stack.pop().unwrap();

    let line_data = r.get_line_data();
    let (line_num, line_count) = ((line_data >> 32) as u32, (line_data & 0xFFFF_FFFF) as u32);

    ctx.peek_line_num = line_num;
    ctx.peek_line_off = line_count;

    loop {
      if state < 1 {
        if ctx.tok_id == 0 {
          scan_for_improvised_token(ctx, r);
        }

        break Some(ctx.scan_anchor_off - offset);
      } else {
        let mask_gate = NORMAL_STATE_FLAG << (ctx.in_fail_mode() as u32);

        if (state & mask_gate) != 0 {
          let fail_mode = loop {
            match dispatch(r, ctx, &mut stack, bc) {
              ParseAction::CompleteState => {
                break false;
              }

              ParseAction::FailState => {
                break true;
              }
              _ => {}
            }
          };
          ctx.set_fail_mode_to(fail_mode);
        }
        state = stack.pop().unwrap();
      }
    }
  } {
    Some(token) => token,
    _ => panic!("Unusable State"),
  }
}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<T: LLVMByteReader + ByteReader + MutByteReader, M>(
  r: &mut T,
  ctx: &mut LLVMParseContext<T, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
) -> ParseAction {
  let mut state = stack.pop().unwrap();

  loop {
    if state < 1 {
      if r.offset_at_end(ctx.token_off) {
        break ParseAction::Accept { production_id: ctx.get_production() };
      } else {
        break ParseAction::Error {
          last_production: ctx.get_production(),
          last_input:      TokenRange {
            len:      ctx.tok_input_len,
            off:      ctx.token_off,
            line_num: ctx.tok_line_num,
            line_off: ctx.tok_line_off,
          },
        };
      }
    } else {
      let mask_gate = NORMAL_STATE_FLAG << (ctx.in_fail_mode() as u32);

      if (state & mask_gate) != 0 {
        match dispatch(r, ctx, stack, bc) {
          ParseAction::CompleteState => {
            ctx.set_fail_mode_to(false);
            state = stack.pop().unwrap();
          }
          ParseAction::FailState => {
            ctx.set_fail_mode_to(true);
            ctx.set_active_state_to(stack.pop().unwrap());
          }
          action => break action,
        }
      } else {
        state = stack.pop().unwrap();
      }
    }
  }
}
