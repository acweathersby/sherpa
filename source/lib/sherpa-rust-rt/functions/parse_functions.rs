use crate::types::*;

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<T: ByteReader + MutByteReader>(
  r: &mut T,
  ctx: &mut ParseContext<T>,
  bc: &[u32],
) -> ParseAction {
  use ParseAction::*;

  let mut i = (ctx.get_active_state() & STATE_ADDRESS_MASK);

  loop {
    let instr = INSTRUCTION::from(bc, i as usize);

    ctx.set_active_state_to(i);

    use InstructionType::*;

    i = match instr.to_type() {
      SHIFT => {
        let (action, i) = shift(i, instr, ctx, r);
        ctx.set_active_state_to(i);
        break action;
      }
      GOTO => goto(i, instr, ctx),
      SET_PROD => set_production(i, instr, ctx),
      REDUCE => {
        let (action, i) = reduce(i, instr, ctx);
        ctx.set_active_state_to(i);
        break action;
      }
      TOKEN => set_token_state(i, instr, ctx),
      FORK_TO => {
        let (action, i) = fork(i, instr);
        ctx.set_active_state_to(i);
        break action;
      }
      SCAN => scan(),
      EAT_CRUMBS => noop(i),
      VECTOR_BRANCH => vector_jump(i, r, ctx, bc),
      HASH_BRANCH => hash_jump(i, r, ctx, bc),
      SET_FAIL_STATE => set_fail(),
      REPEAT => repeat(),
      NOOP13 => noop(i),
      ASSERT_SHIFT => DEFAULT_FAIL_INSTRUCTION_ADDRESS,
      FAIL => break FailState,
      _ => break CompleteState,
    };
  }
}
/// Produces a parse action that
/// contains a token that is the
#[inline]
fn shift<T: ByteReader + MutByteReader + MutByteReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
  r: &mut T,
) -> (ParseAction, u32) {
  if instr.get_value() & 0x1 == 1 {
    ctx.assert.0.len = 0;
  }

  let mut skip = ctx.anchor;
  let mut shift = ctx.assert;

  let next_token = next_token(shift);

  ctx.assert = next_token;

  if ctx.is_scanner() {
    r.next(shift.0.len as i32);
  } else {
    ctx.anchor = next_token;
  }

  skip.0.len = shift.0.off - skip.0.off;

  (ParseAction::Shift { 
    anchor_byte_offset:skip.0.off,
    token_byte_offset:shift.0.off,
    token_byte_length:shift.0.off,
    token_line_count: shift.0.line_num,
    token_line_offset: shift.0.line_off,
   }, i + 1)
}

fn next_token(shift: (TokenRange, u32)) -> (TokenRange, u32) {
    let mut next_token = shift.0;
    next_token.off +=next_token.len;
    next_token.len = 0;
    (next_token, shift.1)
}

#[inline]
fn reduce<T: ByteReader + MutByteReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> (ParseAction, u32) {
  let symbol_count = instr.get_contents() >> 16 & 0x0FFF;
  let rule_id = instr.get_contents() & 0xFFFF;
  let production_id = ctx.get_production();

  (
    if symbol_count == 0x0FFF {
      todo!("Acquire symbol count from symbol accumulator");
      ParseAction::Reduce { production_id, rule_id, symbol_count: 0 }
    } else {
      ParseAction::Reduce { production_id, rule_id, symbol_count }
    },
    i + 1,
  )
}

#[inline]
fn goto<T: ByteReader + MutByteReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  ctx.push_state(instr.get_value());
  i + 1
}

#[inline]
fn eat_crumbs<T: ByteReader + MutByteReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  ctx.push_state(instr.get_value());
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
fn set_production<T: ByteReader + MutByteReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  ctx.set_production_to(instr.get_contents());
  i + 1
}

#[inline]
fn set_token_state<T: ByteReader + MutByteReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  let value = instr.get_contents() & 0x00FF_FFFF;

  let mut anchor = ctx.anchor;

  let assert = ctx.assert;

  anchor.1 = value;

  anchor.0.len = assert.0.off - anchor.0.off;

  ctx.anchor = anchor;

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
fn skip_token<T: ByteReader + MutByteReader>(ctx: &mut ParseContext<T>, r: &mut T) {
  if ctx.in_peek_mode() {
    ctx.peek = next_token(ctx.peek);
  } else {
    ctx.assert = next_token(ctx.assert);
  }
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<T: ByteReader + MutByteReader>(
  i: u32,
  r: &mut T,
  ctx: &mut ParseContext<T>,
  bc: &[u32],
) -> u32 {
  let i = i as usize;
  // Decode data

  let TableHeaderData {
    input_type,
    lexer_type,
    table_length,
    table_meta: modulo_base,
    scan_state_entry_instruction: scan_index,
  } = TableHeaderData::from_bytecode(i, bc);

  let hash_mask = (1 << modulo_base) - 1;
  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      INPUT_TYPE::T01_PRODUCTION => ctx.get_production(),
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
          skip_token(ctx, r);
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
pub fn vector_jump<T: ByteReader + MutByteReader>(
  i: u32,
  r: &mut T,
  ctx: &mut ParseContext<T>,
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
      INPUT_TYPE::T01_PRODUCTION => ctx.get_production(),
      _ => {
        get_token_value(lexer_type, input_type, r, scan_index.get_address() as u32, ctx, bc) as u32
      }
    };

    let value_index = (input_value as i32 - value_offset as i32) as u32;

    if value_index < table_length {
      let off = unsafe { *bc.get_unchecked(table_start + value_index as usize) };
      if off == 0xFFFF_FFFF {
        skip_token(ctx, r);
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
fn get_token_value<T: ByteReader + MutByteReader>(
  lex_type: u32,
  input_type: u32,
  r: &mut T,
  scan_index: u32,
  ctx: &mut ParseContext<T>,
  bc: &[u32],
) -> i32 {
  let mut active_token = match lex_type {
        LEXER_TYPE::PEEK => {
            let basis_token = if ctx.in_peek_mode() {
                ctx.peek
            } else {
                ctx.assert
            };

            ctx.set_peek_mode_to(true);

            r.set_cursor_to(&next_token(basis_token).0);

            next_token(basis_token)
        }
        _ /*| LEXER_TYPE::ASSERT*/ => {
            if ctx.in_peek_mode() {
                
                ctx.set_peek_mode_to(false);

                r.set_cursor_to(&ctx.assert.0 );
            }

            ctx.assert
        }
    };

  match input_type {
    INPUT_TYPE::T03_CLASS => {
      active_token.0.len = r.codepoint_byte_length();

      if ctx.in_peek_mode() {
        ctx.peek = active_token;
      } else {
        ctx.assert = active_token;
      }

      r.class() as i32
    }

    INPUT_TYPE::T04_CODEPOINT => {
      active_token.0.len = r.codepoint_byte_length();

      if ctx.in_peek_mode() {
        ctx.peek = active_token;
      } else {
        ctx.assert = active_token;
      }

      r.codepoint() as i32
    }

    INPUT_TYPE::T05_BYTE => {
      active_token.0.len = 1;

      if ctx.in_peek_mode() {
        ctx.peek = active_token;
      } else {
        ctx.assert = active_token;
      }
      r.byte() as i32
    }

    _ => {
      debug_assert!(!ctx.is_scanner());

      let scanned_token = token_scan(active_token, scan_index, ctx, r, bc);

      if ctx.in_peek_mode() {
        ctx.peek = scanned_token;
      } else {
        ctx.assert = scanned_token;
      }

      scanned_token.1 as i32
    }
  }
}

fn scan_for_improvised_token<T: ByteReader + MutByteReader>(
  scan_ctx: &mut ParseContext<T>,
  r: &mut T,
) {
  let mut assert = scan_ctx.assert;
  assert.0.len = r.codepoint_byte_length();
  let mut byte = r.byte();
  // Scan to next break point and produce an undefined
  // token. If we are already at a break point then just
  // return the single character token.
  if byte == (b'\n' as u32)
    || byte == (b'\t' as u32)
    || byte == (b'\r' as u32)
    || byte == (b' ' as u32)
  {
    assert = next_token(assert);
  } else {
    while byte != b'\n' as u32
      && byte != b'\t' as u32
      && byte != b'\r' as u32
      && byte != b' ' as u32
      && !r.at_end()
    {
      r.next(assert.0.len as i32);
      byte = r.byte();
      assert = next_token(assert);
      assert.0.len = r.codepoint_byte_length();
    }
  }
  scan_ctx.assert = assert;

  set_token_state(0, INSTRUCTION::default(), scan_ctx);
}

fn token_scan<T: ByteReader + MutByteReader>(
  token: (TokenRange, u32),
  scan_index: u32,
  ctx: &mut ParseContext<T>,
  r: &mut T,
  bc: &[u32],
) -> (TokenRange, u32) {
  if (token.1 != 0) {
    return token;
  }

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

      return (
        Default::default(), END_OF_INPUT_TOKEN_ID
      );
    }
  }

  r.set_cursor_to(&token.0);

  if r.at_end() {
    return (token.0, 0);
  }
  // Initialize Scanner

  let mut scan_ctx = ParseContext::bytecode_context();
  scan_ctx.make_scanner();
  scan_ctx.init_normal_state(scan_index);
  scan_ctx.anchor = token;
  scan_ctx.assert = token;

  // We are done with the state reference, so we
  // invalidate variable by moving the reference to
  // an unused name to prevent confusion with the
  // `scan_state` variable.
  let _state = ctx;

  match {
    if scan_ctx.get_active_state() == 0 {
      // Decode the next scanner_state.
      let state = scan_ctx.pop_state();
      scan_ctx.set_active_state_to(state);
    }

    let line_data = r.get_line_data();
    let (line_num, line_count) = ((line_data >> 32) as u32, (line_data & 0xFFFF_FFFF) as u32);

    scan_ctx.anchor.0.line_num = line_num;
    scan_ctx.anchor.0.line_off = line_count;

    loop {
      if scan_ctx.get_active_state() < 1 {
        if scan_ctx.anchor.1 == 0 {
          scan_for_improvised_token(&mut scan_ctx, r);
        }

        break Some(scan_ctx.anchor.into());
      } else {
        let mask_gate = NORMAL_STATE_FLAG << (scan_ctx.in_fail_mode() as u32);

        if ((scan_ctx.get_active_state() & mask_gate) != 0) {
          let fail_mode = loop {
            match dispatch(r, &mut scan_ctx, bc) {
              ParseAction::CompleteState => {
                break false;
              }

              ParseAction::FailState => {
                break true;
              }
              _ => {}
            }
          };
          scan_ctx.set_fail_mode_to(fail_mode);
        }
        let state = scan_ctx.pop_state();
        scan_ctx.set_active_state_to(state);
      }
    }
  } {
    Some(token) => (token),
    _ => panic!("Unusable State"),
  }}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<T: ByteReader + MutByteReader>(
  r: &mut T,
  ctx: &mut ParseContext<T>,
  bc: &[u32],
) -> ParseAction {
  if ctx.get_active_state() == 0 {
    let state = ctx.pop_state();
    ctx.set_active_state_to(state);
  }

  loop {
    if ctx.get_active_state() < 1 {
      if r.offset_at_end(ctx.assert.0.len) {
        break ParseAction::Accept { production_id: ctx.get_production() };
      } else {
        break ParseAction::Error {
          last_production: ctx.get_production(),
          last_input:      ctx.assert.0,
        };
      }
    } else {
      let mask_gate = NORMAL_STATE_FLAG << (ctx.in_fail_mode() as u32);

      if (ctx.is_interrupted() || (ctx.get_active_state() & mask_gate) != 0) {
        ctx.set_interrupted_to(true);

        match dispatch(r, ctx, bc) {
          ParseAction::CompleteState => {
            ctx.set_interrupted_to(false);
            ctx.set_fail_mode_to(false);
            let state = ctx.pop_state();
            ctx.set_active_state_to(state);
          }
          ParseAction::FailState => {
            ctx.set_interrupted_to(false);
            ctx.set_fail_mode_to(true);
            let state = ctx.pop_state();
            ctx.set_active_state_to(state);
          }
          action => break action,
        }
      } else {
        let state = ctx.pop_state();
        ctx.set_active_state_to(state);
      }
    }
  }
}
