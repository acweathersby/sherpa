use std::hash;

use crate::types::*;

use ParseAction::*;

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<T: ImmutCharacterReader + MutCharacterReader>(
  r: &mut T,
  ctx: &mut ParseContext<T>,
  bc: &[u32],
) -> ParseAction {
  use ParseAction::*;
  use INSTRUCTION as I;

  let mut i = (ctx.get_active_state() & STATE_ADDRESS_MASK);

  loop {
    println!("{:x} {} {}", i, (r.byte() as u8).to_string(), r.cursor());

    let instr = INSTRUCTION::from(bc, i as usize);

    ctx.set_active_state_to(i);

    use InstructionType::*;

    i = match instr.to_type() {
      CONSUME => {
        let (action, i) = consume(i, instr, ctx, r);
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
      NOOP8 => noop(i),
      VECTOR_BRANCH => vector_jump(i, r, ctx, bc),
      HASH_BRANCH => hash_jump(i, r, ctx, bc),
      SET_FAIL_STATE => set_fail(),
      REPEAT => repeat(),
      NOOP13 => noop(i),
      ASSERT_CONSUME => DEFAULT_FAIL_INSTRUCTION_ADDRESS,
      FAIL => break FailState,
      _ => break CompleteState,
    }
  }
}
/// Produces a parse action that
/// contains a token that is the
#[inline]
fn consume<T: ImmutCharacterReader + MutCharacterReader + MutCharacterReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
  r: &mut T,
) -> (ParseAction, u32) {
  if instr.get_value() & 0x1 == 1 {
    ctx.assert.byte_length = 0;
    ctx.assert.cp_length = 0;
  }

  let mut skip = ctx.anchor;
  let mut shift = ctx.assert;

  let next_token = shift.next();

  ctx.assert = next_token;

  if ctx.is_scanner() {
    r.next(shift.byte_length as i32);
  } else {
    ctx.anchor = next_token;
    println!("{:?}", shift);
  }

  skip.cp_length = shift.cp_offset - skip.cp_offset;
  skip.byte_length = shift.byte_offset - skip.byte_offset;

  (ParseAction::Shift { skipped_characters: skip, token: shift }, i + 1)
}

#[inline]
fn reduce<T: ImmutCharacterReader + MutCharacterReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> (ParseAction, u32) {
  let symbol_count = instr.get_contents() >> 16 & 0x0FFF;
  let body_id = instr.get_contents() & 0xFFFF;
  let production_id = ctx.get_production();

  (
    if symbol_count == 0x0FFF {
      todo!("Acquire symbol count from symbol accumulator");
      ParseAction::Reduce { production_id, body_id, symbol_count: 0 }
    } else {
      ParseAction::Reduce { production_id, body_id, symbol_count }
    },
    i + 1,
  )
}

#[inline]
fn goto<T: ImmutCharacterReader + MutCharacterReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  ctx.push_state(instr.get_value());
  i + 1
}

#[inline]
fn set_production<T: ImmutCharacterReader + MutCharacterReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  ctx.set_production_to(instr.get_contents());
  i + 1
}

#[inline]
fn set_token_state<T: ImmutCharacterReader + MutCharacterReader>(
  i: u32,
  instr: INSTRUCTION,
  ctx: &mut ParseContext<T>,
) -> u32 {
  let value = instr.get_contents() & 0x00FF_FFFF;

  let mut anchor = ctx.anchor;

  let assert = ctx.assert;

  anchor.token_type = value;

  anchor.byte_length = assert.byte_offset - anchor.byte_offset;
  anchor.cp_length = assert.cp_offset - anchor.cp_offset;

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
fn skip_token<T: ImmutCharacterReader + MutCharacterReader>(
  ctx: &mut ParseContext<T>,
  r: &mut T,
) {
  if ctx.in_peek_mode() {
    ctx.peek = ctx.peek.next();
  } else {
    ctx.assert = ctx.assert.next();
  }
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<T: ImmutCharacterReader + MutCharacterReader>(
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
    scan_index,
  } = TableHeaderData::from_bytecode(i, bc);

  let hash_mask = (1 << modulo_base) - 1;
  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      INPUT_TYPE::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(
        lexer_type,
        input_type,
        r,
        scan_index.get_address() as u32,
        ctx,
        bc,
      ) as u32,
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
pub fn vector_jump<T: ImmutCharacterReader + MutCharacterReader>(
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
    scan_index,
  } = TableHeaderData::from_bytecode(i, bc);

  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      INPUT_TYPE::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(
        lexer_type,
        input_type,
        r,
        scan_index.get_address() as u32,
        ctx,
        bc,
      ) as u32,
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
fn get_token_value<T: ImmutCharacterReader + MutCharacterReader>(
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
                ctx.anchor
            };

            ctx.set_peek_mode_to(true);

            basis_token.next()
        }
        _ /*| LEXER_TYPE::ASSERT*/ => {
            if ctx.in_peek_mode() {
                
                ctx.set_peek_mode_to(false);

                r.set_cursor_to(&ctx.assert );
            }

            ctx.assert
        }
    };

  if ctx.is_scanner() {
    match input_type {
      INPUT_TYPE::T03_CLASS => {
        active_token.byte_length = r.codepoint_byte_length();
        active_token.cp_length = r.codepoint_length();

        if ctx.in_peek_mode() {
          ctx.peek = active_token;
        } else {
          ctx.assert = active_token;
        }

        r.class() as i32
      }
      INPUT_TYPE::T04_CODEPOINT => {
        active_token.byte_length = r.codepoint_byte_length();
        active_token.cp_length = r.codepoint_length();

        if ctx.in_peek_mode() {
          ctx.peek = active_token;
        } else {
          ctx.assert = active_token;
        }

        r.codepoint() as i32
      }
      INPUT_TYPE::T05_BYTE => {
        active_token.byte_length = 1;
        active_token.cp_length = 1;

        if ctx.in_peek_mode() {
          ctx.peek = active_token;
        } else {
          ctx.assert = active_token;
        }
        r.byte() as i32
      }
      _ => 0,
    }
  } else {
    let scanned_token = token_scan(active_token, scan_index, ctx, r, bc);

    if ctx.in_peek_mode() {
      ctx.peek = scanned_token;
    } else {
      ctx.assert = scanned_token;
    }

    scanned_token.token_type as i32
  }
}

fn scan_for_improvised_token<T: ImmutCharacterReader + MutCharacterReader>(
  scan_ctx: &mut ParseContext<T>,
  r: &mut T,
) {
  let mut assert = scan_ctx.assert;
  assert.byte_length = r.codepoint_byte_length();
  assert.cp_length = 1;
  let mut byte = r.byte();
  // Scan to next break point and produce an undefined
  // token. If we are already at a break point then just
  // return the single character token.
  if byte == (b'\n' as u32)
    || byte == (b'\t' as u32)
    || byte == (b'\r' as u32)
    || byte == (b' ' as u32)
  {
    assert = assert.next();
  } else {
    while byte != b'\n' as u32
      && byte != b'\t' as u32
      && byte != b'\r' as u32
      && byte != b' ' as u32
      && !r.at_end()
    {
      r.next(assert.byte_length as i32);
      byte = r.byte();
      assert = assert.next();
      assert.byte_length = r.codepoint_byte_length();
      assert.cp_length += 1;
    }
  }
  scan_ctx.assert = assert;

  set_token_state(0, INSTRUCTION::default(), scan_ctx);
}

fn token_scan<T: ImmutCharacterReader + MutCharacterReader>(
  token: ParseToken,
  scan_index: u32,
  ctx: &mut ParseContext<T>,
  r: &mut T,
  bc: &[u32],
) -> ParseToken {
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

      return ParseToken { token_type: id, ..Default::default() };
    }
  }
  // Initialize Scanner

  let mut scan_ctx = ParseContext::bytecode_context();
  scan_ctx.make_scanner();
  scan_ctx.init_normal_state(scan_index);
  scan_ctx.anchor = token;
  scan_ctx.assert = token;
  r.set_cursor_to(&token);

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

    scan_ctx.anchor.line_number = line_num;
    scan_ctx.anchor.line_offset = line_count;

    loop {
      if scan_ctx.get_active_state() < 1 {
        if scan_ctx.in_fail_mode() || scan_ctx.anchor.token_type == 0 {
          scan_for_improvised_token(&mut scan_ctx, r);
        }

        break ParseAction::ScannerToken(scan_ctx.anchor);
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
    ParseAction::ScannerToken(token) => token,
    _ => panic!("Unusable State"),
  }
}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<T: ImmutCharacterReader + MutCharacterReader>(
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
      if r.offset_at_end(ctx.assert.byte_offset) {
        break ParseAction::Accept { production_id: ctx.get_production() };
      } else {
        break ParseAction::Error {
          last_production: ctx.get_production(),
          last_input:      ctx.assert,
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
