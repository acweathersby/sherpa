use crate::types::{
  ast::{AstObject, AstSlot, Reducer},
  *,
};

pub enum DebugEvent<'a> {
  ExecuteState {
    bc:      &'a [u32],
    offset:  usize,
    len:     usize,
    address: usize,
  },
  ExecuteInstruction {
    bc:          &'a [u32],
    string:      &'a str,
    address:     usize,
    instruction: Instruction,
    is_scanner:  bool,
    end_ptr:     usize,
    head_ptr:    usize,
    tail_ptr:    usize,
    base_ptr:    usize,
    anchor_ptr:  usize,
    tok_len:     usize,
    tok_id:      u32,
    sym_len:     u32,
  },
  SkipToken {
    offset_start: usize,
    offset_end:   usize,
    string:       &'a str,
  },
  ShiftToken {
    offset_start: usize,
    offset_end:   usize,
    string:       &'a str,
  },
  ByteValue {
    input_value: u32,
    start:       usize,
    end:         usize,
    string:      &'a str,
  },
  CodePointValue {
    input_value: u32,
    start:       usize,
    end:         usize,
    string:      &'a str,
  },
  ClassValue {
    input_value: u32,
    start:       usize,
    end:         usize,
    string:      &'a str,
  },
  TokenValue {
    input_value: u32,
    start:       usize,
    end:         usize,
    string:      &'a str,
  },
  GotoValue {
    production_id: u32,
  },
  Reduce {
    rule_id: u32,
  },
  Complete {
    production_id: u32,
  },
  Failure {},
}

pub type DebugFn = Box<dyn FnMut(&DebugEvent)>;

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  state: u32,
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
  debug: &mut Option<DebugFn>,
) -> (ParseAction, u32) {
  use ParseAction::*;

  let mut s = state & STATE_ADDRESS_MASK;

  let root = s;

  #[cfg(debug_assertions)]
  if let Some(debug) = debug.as_mut() {
    debug(&DebugEvent::ExecuteState {
      bc,
      address: s as usize,
      offset: ctx.head_ptr as usize,
      len: ctx.tail_ptr - ctx.head_ptr,
    });
  }

  loop {
    let i = Instruction::from(bc, s as usize);

    use InstructionType::*;

    match match i.to_type() {
      ShiftToken => shift_token(s, i, ctx),
      Goto => (None, goto(s, i, stack)),
      SetProd => (None, set_production(s, i, ctx)),
      Reduce => reduce(s, i, ctx),
      Token => (None, set_token_state(s, i, ctx)),
      ForkTo => fork(s, i),
      ResetPeek => (None, reset_peek(s, ctx)),
      Pop => {
        pop(stack, s);
        (CompleteState, 0)
      }
      VectorBranch => (None, vector_jump(s, ctx, bc, debug)),
      HashBranch => (None, hash_jump(s, ctx, bc, debug)),
      SetFailState => (None, set_fail()),
      Skip => (None, skip(i, root, ctx)),
      ShiftScanner => (None, shift_scanner(s, ctx)),
      PeekToken => (None, peek_token(s, ctx, debug)),
      Fail => (FailState, 0),
      _ => (CompleteState, 0),
    } {
      (None, val) => {
        #[cfg(debug_assertions)]
        if let Some(debug) = debug {
          debug(&DebugEvent::ExecuteInstruction {
            bc,
            address: s as usize,
            string: ctx.get_str(),
            instruction: i,
            is_scanner: ctx.is_scanner(),
            end_ptr: ctx.end_ptr,
            head_ptr: ctx.head_ptr,
            tail_ptr: ctx.tail_ptr,
            base_ptr: ctx.base_ptr,
            anchor_ptr: ctx.anchor_ptr,
            tok_id: ctx.tok_id,
            sym_len: ctx.sym_len,
            tok_len: ctx.tok_len,
          });
        }
        s = val;
      }
      (any, out) => {
        #[cfg(debug_assertions)]
        if let Some(debug) = debug {
          debug(&DebugEvent::ExecuteInstruction {
            bc,
            address: s as usize,
            string: ctx.get_str(),
            instruction: i,
            is_scanner: ctx.is_scanner(),
            end_ptr: ctx.end_ptr,
            head_ptr: ctx.head_ptr,
            tail_ptr: ctx.tail_ptr,
            base_ptr: ctx.base_ptr,
            anchor_ptr: ctx.anchor_ptr,
            tok_id: ctx.tok_id,
            sym_len: ctx.sym_len,
            tok_len: ctx.tok_len,
          });
        }
        break (any, out);
      }
    }
  }
}
// Pops the top state of the stack. This is expected to be
// a goto state
#[inline]
fn pop(stack: &mut Vec<u32>, s: u32) -> u32 {
  //unimplemented!("POPPING!");
  stack.pop().unwrap();
  s + 1
}
/// Produces a parse action that
/// contains a token that is the
#[inline]
fn shift_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: u32,
  instr: Instruction,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, u32) {
  if instr.get_value() & 0x1 == 1 {
    ctx.tok_len = ctx.sym_len as usize;
  }

  let action = ParseAction::Shift {
    anchor_byte_offset: ctx.anchor_ptr as u32,
    token_byte_offset:  ctx.head_ptr as u32,
    token_byte_length:  ctx.tok_len as u32,
    token_line_count:   ctx.start_line_num,
    token_line_offset:  ctx.start_line_off,
  };

  let new_offset = ctx.head_ptr + ctx.tok_len;

  ctx.base_ptr = new_offset;
  ctx.head_ptr = new_offset;
  ctx.tail_ptr = new_offset;
  ctx.anchor_ptr = new_offset;
  ctx.tok_id = 0;

  let (a, b, c) = (new_offset, 0, 0);
  ctx.get_reader_mut().set_cursor_to(a, b, c);

  (action, i + 1)
}

/// Produces a parse action that
/// contains a token that is the
#[inline]
fn peek_token<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: u32,
  ctx: &mut ParseContext<R, M>,
  _debug: &mut Option<DebugFn>,
) -> u32 {
  let offset = ctx.head_ptr + ctx.tok_len;
  ctx.head_ptr = offset;
  ctx.tail_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);
  ctx.in_peek_mode = true;
  i + 1
}

/// Produces a parse action that
/// contains a token that is the
#[inline]
fn shift_scanner<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: u32,
  ctx: &mut ParseContext<R, M>,
) -> u32 {
  ctx.tail_ptr = ctx.tail_ptr + ctx.sym_len as usize;
  ctx.sym_len = 0;
  let offset = ctx.tail_ptr;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);
  i + 1
}

#[inline]
fn reduce<'a, R: 'a + ByteReader + MutByteReader, M>(
  i: u32,
  instr: Instruction,
  ctx: &mut ParseContext<R, M>,
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
  ctx.tok_len = (ctx.tail_ptr - ctx.head_ptr) as usize;
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

fn reset_peek<'a, T: 'a + ByteReader + MutByteReader, M>(
  i: u32,
  ctx: &mut ParseContext<T, M>,
) -> u32 {
  let offset = ctx.base_ptr;
  ctx.head_ptr = offset;
  ctx.tail_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;
  ctx.sym_len = 0;
  ctx.in_peek_mode = false;
  i + 1
}

fn skip<'a, T: 'a + ByteReader + MutByteReader, M>(
  i: Instruction,
  root: u32,
  ctx: &mut ParseContext<T, M>,
) -> u32 {
  if i.get_contents() > 0 {
    ctx.tok_len = ctx.sym_len as usize;
  }

  let offset = ctx.head_ptr + ctx.tok_len as usize;
  ctx.tail_ptr = offset;
  ctx.head_ptr = offset;
  ctx.tok_id = 0;

  if !ctx.in_peek_mode {
    ctx.base_ptr = offset;
  }

  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  root
}

#[inline]
fn set_fail() -> u32 {
  0
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: &mut Option<DebugFn>,
) -> u32 {
  let i = i as usize;
  // Decode data

  let TableHeaderData {
    input_type,
    table_meta: modulo_base,
    scan_state_entry_instruction: scan_index,
    ..
  } = TableHeaderData::from_bytecode(i, bc);

  let hash_mask = (1 << modulo_base) - 1;
  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      InputType::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(input_type, scan_index.get_address() as u32, ctx, bc, debug) as u32,
    };

    #[cfg(debug_assertions)]
    emit_debug_value(ctx, debug, input_type, input_value);

    let mut hash_index = (input_value & hash_mask) as usize;

    loop {
      let cell = unsafe { *bc.get_unchecked(table_start + hash_index) };
      let value = cell & 0x7FF;
      let off = (cell >> 11) & 0x7FF;
      let next = ((cell >> 22) & 0x3FF) as i32 - 512;

      if value == input_value {
        return i as u32 + off;
      } else if next != 0 {
        hash_index = ((hash_index as i32) + next) as usize;
      } else {
        return i as u32 + bc[i + 3];
      }
    }
  }
}

fn emit_debug_value<R: ByteReader + MutByteReader + UTF8Reader, M>(
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<Box<dyn FnMut(&DebugEvent)>>,
  input_type: u32,
  input_value: u32,
) {
  let start = ctx.head_ptr;
  let end = ctx.head_ptr + ctx.tok_len;
  if let Some(debug) = debug {
    match input_type {
      InputType::T01_PRODUCTION => debug(&DebugEvent::GotoValue { production_id: input_value }),
      InputType::T05_BYTE => {
        debug(&DebugEvent::ClassValue { input_value, start, end, string: ctx.get_str() })
      }
      InputType::T03_CLASS => {
        debug(&DebugEvent::ClassValue { input_value, start, end, string: ctx.get_str() })
      }
      InputType::T02_TOKEN => {
        debug(&DebugEvent::CodePointValue { input_value, start, end, string: ctx.get_str() })
      }
      InputType::T04_CODEPOINT => {
        debug(&DebugEvent::ByteValue { input_value, start, end, string: ctx.get_str() })
      }
      _ => unreachable!(),
    };
  }
}
#[inline]
pub fn vector_jump<R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: &mut Option<DebugFn>,
) -> u32 {
  let i = i as usize;
  // Decode data

  let TableHeaderData {
    input_type,
    table_length,
    table_meta: value_offset,
    scan_state_entry_instruction: scan_index,
    ..
  } = TableHeaderData::from_bytecode(i, bc);

  let table_start = i + 4;

  loop {
    let input_value = match input_type {
      InputType::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(input_type, scan_index.get_address() as u32, ctx, bc, debug) as u32,
    };

    #[cfg(debug_assertions)]
    emit_debug_value(ctx, debug, input_type, input_value);

    let value_index = (input_value as i32 - value_offset as i32) as u32;

    if value_index < table_length {
      let off = unsafe { *bc.get_unchecked(table_start + value_index as usize) };
      return i as u32 + off;
    } else {
      return i as u32 + bc[i + 3];
    }
  }
}

#[inline]
fn get_token_value<R: ByteReader + MutByteReader + UTF8Reader, M>(
  input_type: u32,
  scan_index: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: &mut Option<DebugFn>,
) -> i32 {
  match input_type {
    InputType::T03_CLASS => {
      let scan_len = ctx.get_reader().codepoint_byte_length();
      ctx.sym_len = scan_len;
      ctx.get_reader().class() as i32
    }

    InputType::T04_CODEPOINT => {
      let scan_len = ctx.get_reader().codepoint_byte_length();
      ctx.sym_len = scan_len;
      ctx.get_reader().codepoint() as i32
    }

    InputType::T05_BYTE => {
      let byte = ctx.get_reader().byte();

      if byte > 0 {
        ctx.sym_len = 1;
      } else {
        ctx.sym_len = 0;
      }
      byte as i32
    }
    _ => {
      debug_assert!(!ctx.is_scanner());

      token_scan(scan_index, ctx, bc, debug);

      ctx.tok_id as i32
    }
  }
}

fn _scan_for_improvised_token<R: ByteReader + MutByteReader + UTF8Reader, M>(
  ctx: &mut ParseContext<R, M>,
) {
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
      ctx.get_reader_mut().set_cursor_to(off as usize, 0, 0);
    }
  }

  ctx.tail_ptr = off as usize;

  set_token_state(0, Instruction::default(), ctx);
}

fn token_scan<R: ByteReader + MutByteReader + UTF8Reader, M>(
  scan_index: u32,
  ctx: &mut ParseContext<R, M>,
  bc: &[u32],
  debug: &mut Option<DebugFn>,
) {
  ctx.tok_id = 0;
  ctx.tail_ptr = ctx.head_ptr;
  let offset = ctx.tail_ptr;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  if ctx.get_reader().at_end() {
    return;
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

    loop {
      if state < 1 {
        ctx.set_fail_mode_to(ctx_fail_mode);
        break Some(());
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
    Some(()) => {
      ctx.tail_ptr = ctx.head_ptr;
      ctx.set_is_scanner(false);
      let offset = ctx.tail_ptr;
      ctx.get_reader_mut().set_cursor_to(offset, 0, 0);
    }
    _ => panic!("Unusable State"),
  }
}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  ctx: &'a mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
  debug: &mut Option<DebugFn>,
) -> ParseAction {
  let mut state = stack.pop().unwrap();

  loop {
    if state < 1 {
      let off = ctx.head_ptr;
      if ctx.get_reader().offset_at_end(off) {
        break ParseAction::Accept { production_id: ctx.get_production() };
      } else {
        break ParseAction::Error {
          last_production: ctx.get_production(),
          last_input:      TokenRange {
            len:      ctx.head_ptr as u32,
            off:      (ctx.tail_ptr - ctx.head_ptr) as u32,
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

pub fn parse_ast<R: ByteReader + MutByteReader + UTF8Reader, M, Node: AstObject>(
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u32],
  reducers: &[Reducer<R, M, Node>],
  debug: &mut Option<DebugFn>,
) -> Result<AstSlot<Node>, SherpaParseError> {
  let mut ast_stack: Vec<AstSlot<Node>> = vec![];
  loop {
    match get_next_action(ctx, stack, bc, debug) {
      ParseAction::Accept { .. } => {
        return Ok(ast_stack.pop().unwrap());
      }
      ParseAction::Reduce { rule_id, symbol_count, .. } => {
        let _reduce_fn = reducers[rule_id as usize];
        let len = ast_stack.len();
        let count = symbol_count as usize;
        // reduce_fn(&ctx, &AstStackSlice::from_slice(&mut ast_stack[(len - count)..len]));
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
