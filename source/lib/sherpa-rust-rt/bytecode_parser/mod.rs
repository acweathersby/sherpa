use crate::types::{
  ast::{AstObject, AstSlot, Reducer},
  bytecode::{ByteCodeIterator, Instruction, Opcode},
  *,
};

pub enum DebugEvent<'a> {
  ExecuteState {
    base_instruction: Instruction<'a>,
  },
  ExecuteInstruction {
    instruction: Instruction<'a>,
    string:      &'a str,
    is_scanner:  bool,
    end_ptr:     usize,
    head_ptr:    usize,
    scan_ptr:    usize,
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
  base_address: usize,
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &'a [u8],
  debug: &mut Option<DebugFn>,
) -> (ParseAction, Option<Instruction<'a>>) {
  use ParseAction::*;

  let block_base: Instruction = (bc, base_address).into();

  #[cfg(debug_assertions)]
  if let Some(debug) = debug.as_mut() {
    debug(&DebugEvent::ExecuteState { base_instruction: block_base });
  }

  let mut i: Instruction = block_base;

  loop {
    use Opcode::*;

    i = match match i.get_opcode() {
      ShiftToken => shift_token(i, ctx),
      ShiftTokenScanless => shift_token_scanless(i, ctx),
      ScanShift => scan_shift(i, ctx),
      SkipToken => skip_token(block_base, ctx),
      SkipTokenScanless => skip_token_scanless(block_base, ctx),
      SkipPeekToken => skip_peek_token(block_base, ctx),
      SkipPeekTokenScanless => skip_peek_token_scanless(block_base, ctx),
      PeekToken => peek_token(i, ctx),
      PeekTokenScanless => peek_token_scanless(i, ctx),
      PeekReset => peek_reset(i, ctx),
      Reduce => reduce(i, ctx),
      Goto => goto(i),
      PushGoto => push_goto(i, stack),
      PushExceptionHandler => push_exception_handler(i, stack),
      PopGoto => pop_goto(i, stack),
      AssignToken => assign_token(i, ctx),
      VectorBranch => vector_branch(i, ctx, debug),
      HashBranch => hash_branch(i, ctx, debug),
      Fail => (FailState, Option::None),
      Pass => (CompleteState, Option::None),
      Accept => (CompleteState, Option::None),
      NoOp => (None, i.next()),
    } {
      (None, Option::None) => {
        unreachable!("Expected next instruction!")
      }
      (None, Some(next_instruction)) => {
        #[cfg(debug_assertions)]
        if let Some(debug) = debug {
          debug(&DebugEvent::ExecuteInstruction {
            string:      ctx.get_str(),
            instruction: i,
            is_scanner:  ctx.is_scanner(),
            end_ptr:     ctx.end_ptr,
            head_ptr:    ctx.head_ptr,
            scan_ptr:    ctx.scan_ptr,
            base_ptr:    ctx.base_ptr,
            anchor_ptr:  ctx.anchor_ptr,
            tok_id:      ctx.tok_id,
            sym_len:     ctx.sym_len,
            tok_len:     ctx.tok_len,
          });
        }
        next_instruction
      }
      (any, out) => {
        #[cfg(debug_assertions)]
        if let Some(debug) = debug {
          debug(&DebugEvent::ExecuteInstruction {
            string:      ctx.get_str(),
            instruction: i,
            is_scanner:  ctx.is_scanner(),
            end_ptr:     ctx.end_ptr,
            head_ptr:    ctx.head_ptr,
            scan_ptr:    ctx.scan_ptr,
            base_ptr:    ctx.base_ptr,
            anchor_ptr:  ctx.anchor_ptr,
            tok_id:      ctx.tok_id,
            sym_len:     ctx.sym_len,
            tok_len:     ctx.tok_len,
          });
        }
        break (any, out);
      }
    }
  }
}
/// Performs the [Opcode::TokenShift] operation
#[inline]
fn shift_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::ShiftToken;

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
  ctx.scan_ptr = new_offset;
  ctx.anchor_ptr = new_offset;
  ctx.tok_id = 0;

  let (a, b, c) = (new_offset, 0, 0);
  ctx.get_reader_mut().set_cursor_to(a, b, c);

  (action, i.next())
}

/// Performs the [Opcode::ShiftTokenScanless] operation
#[inline]
fn shift_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::ShiftTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  shift_token(i, ctx)
}

/// Performs the [Opcode::ScanShift] operation
#[inline]
fn scan_shift<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::ScanShift;

  ctx.scan_ptr = ctx.scan_ptr + ctx.sym_len as usize;
  ctx.sym_len = 0;
  let offset = ctx.scan_ptr;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  (ParseAction::None, i.next())
}

/// Performs the [Opcode::PeekToken] operation
#[inline]
fn peek_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PeekToken;

  let offset = ctx.head_ptr + ctx.tok_len;
  ctx.head_ptr = offset;
  ctx.scan_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  (ParseAction::None, i.next())
}

/// Performs the [Opcode::PeekTokenScanless] operation
#[inline]
fn peek_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PeekTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  peek_token(i, ctx)
}

#[inline]
fn __skip_token_core__<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  let offset = ctx.head_ptr + ctx.tok_len as usize;
  ctx.scan_ptr = offset;
  ctx.head_ptr = offset;
  ctx.tok_id = 0;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  (ParseAction::None, Some(base_instruction))
}
/// Performs the [Opcode::SkipToken] operation
#[inline]
fn skip_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::SkipToken;
  let result = __skip_token_core__(base_instruction, ctx);
  ctx.base_ptr = ctx.head_ptr;
  result
}

/// Performs the [Opcode::SkipTokenScanless] operation
#[inline]
fn skip_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::SkipTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  skip_token(base_instruction, ctx)
}

/// Performs the [Opcode::SkipPeekToken] operation
#[inline]
fn skip_peek_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::SkipPeekToken;
  __skip_token_core__(base_instruction, ctx)
}

/// Performs the [Opcode::SkipPeekTokenScanless] operation
#[inline]
fn skip_peek_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::SkipPeekTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  skip_peek_token(base_instruction, ctx)
}

/// Performs the [Opcode::Reduce] operation
#[inline]
fn reduce<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::Reduce;
  let mut iter = i.iter();
  let production_id = iter.next_u32_le().unwrap();
  let rule_id = iter.next_u32_le().unwrap();
  let symbol_count = iter.next_u16_le().unwrap() as u32;

  ctx.sym_len = symbol_count;
  ctx.prod_id = production_id;

  (ParseAction::Reduce { production_id, rule_id, symbol_count }, i.next())
}

/// Performs the [Opcode::PushGoto] operation
#[inline]
fn push_goto<'a>(
  i: Instruction<'a>,
  stack: &mut Vec<u32>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PushGoto;
  let mut iter = i.iter();
  let state_mode = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  stack.push(state_mode as u32);
  stack.push(address);

  (ParseAction::None, i.next())
}

/// Performs the [Opcode::PushExceptionHandler] operation
#[inline]
fn push_exception_handler<'a>(
  i: Instruction<'a>,
  stack: &mut Vec<u32>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PushExceptionHandler;
  let mut iter = i.iter();
  let state_mode = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  stack.push(state_mode as u32);
  stack.push(address);

  (ParseAction::None, i.next())
}

/// Performs the [Opcode::Goto] operation
#[inline]
fn goto<'a>(i: Instruction<'a>) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::Goto;
  let mut iter = i.iter();
  let _state_mode_ = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  (ParseAction::None, Some((i.bytecode(), address as usize).into()))
}

/// Performs the [Opcode::PopGoto] operation
#[inline]
fn pop_goto<'a>(
  i: Instruction<'a>,
  stack: &mut Vec<u32>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PopGoto;

  stack.pop();
  stack.pop();

  (ParseAction::None, i.next())
}

/// Performs the [Opcode::AssignToken] operation
#[inline]
fn assign_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::AssignToken;
  let mut iter = i.iter();
  ctx.tok_id = iter.next_u32_le().unwrap();

  ctx.tok_len = ctx.scan_ptr - ctx.head_ptr;
  (ParseAction::None, i.next())
}

/// Performs the [Opcode::PeekReset] operation
#[inline]
fn peek_reset<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  let offset = ctx.base_ptr;
  ctx.head_ptr = offset;
  ctx.scan_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;
  ctx.sym_len = 0;
  ctx.in_peek_mode = false;
  (ParseAction::None, i.next())
}

/// Performs the [Opcode::HashBranch] operation
#[inline]
pub fn hash_branch<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<DebugFn>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::HashBranch;

  // Decode data
  let TableHeaderData {
    input_type,
    table_meta: modulo_base,
    scan_block_instruction,
    default_block,
    table_start,
    ..
  } = i.into();

  let hash_mask = (1 << modulo_base) - 1;

  loop {
    let input_value = match input_type {
      InputType::T01_PRODUCTION => ctx.get_production(),
      _ => get_token_value(input_type, scan_block_instruction, ctx, debug) as u32,
    };

    #[cfg(debug_assertions)]
    emit_debug_value(ctx, debug, input_type, input_value);

    let mut hash_index = (input_value & hash_mask) as usize;

    loop {
      let mut iter: ByteCodeIterator = (i.bytecode(), table_start + hash_index * 4).into();
      let cell = iter.next_u32_le().unwrap();
      let value = cell & 0x7FF;
      let off = (cell >> 11) & 0x7FF;
      let next = ((cell >> 22) & 0x3FF) as i32 - 512;

      if value == input_value {
        return (ParseAction::None, Some((i.bytecode(), i.address() + off as usize).into()));
      } else if next != 0 {
        hash_index = ((hash_index as i32) + next) as usize;
      } else {
        return (ParseAction::None, Some(default_block));
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
pub fn vector_branch<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<DebugFn>,
) -> (ParseAction, Option<Instruction<'a>>) {
  // Decode data

  let TableHeaderData {
    input_type,
    table_meta: value_offset,
    scan_block_instruction,
    default_block,
    table_start,
    table_length,
    ..
  } = i.into();

  let input_value = match input_type {
    InputType::T01_PRODUCTION => ctx.get_production(),
    _ => get_token_value(input_type, scan_block_instruction, ctx, debug) as u32,
  };

  #[cfg(debug_assertions)]
  emit_debug_value(ctx, debug, input_type, input_value);

  let value_index = (input_value as i32 - value_offset as i32) as usize;

  if value_index < table_length as usize {
    let mut iter: ByteCodeIterator = (i.bytecode(), table_start + value_index * 4).into();
    let address_offset = iter.next_u32_le().unwrap();
    (ParseAction::None, Some((i.bytecode(), i.address() + address_offset as usize).into()))
  } else {
    (ParseAction::None, Some(default_block))
  }
}

#[inline]
fn get_token_value<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  input_type: u32,
  scan_index: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
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

      token_scan(scan_index, ctx, debug);

      ctx.tok_id as i32
    }
  }
}

fn token_scan<'a, R: ByteReader + MutByteReader + UTF8Reader, M>(
  scan_index: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<DebugFn>,
) {
  ctx.tok_id = 0;
  ctx.scan_ptr = ctx.head_ptr;
  let offset = ctx.scan_ptr;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  if ctx.get_reader().at_end() {
    return;
  }

  // Initialize Scanner

  // We are done with the state reference, so we
  // invalidate variable by moving the reference to
  // an unused name to prevent confusion with the
  // `scan_state` variable.
  let mut stack = vec![0, 0, NORMAL_STATE_FLAG, scan_index.address() as u32];
  let bc = scan_index.bytecode();

  ctx.set_is_scanner(true);

  match {
    let mut address = stack.pop().unwrap() as usize;
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
            match dispatch(address as usize, ctx, &mut stack, bc, debug) {
              (ParseAction::CompleteState, _) => {
                break false;
              }

              (ParseAction::FailState, _) => {
                break true;
              }
              (_, next_block) => {
                if let Some(next_block) = next_block {
                  stack.push(NORMAL_STATE_FLAG);
                  stack.push(next_block.address() as u32);
                }
              }
            }
          };
          ctx.set_fail_mode_to(fail_mode);
        }
        address = stack.pop().unwrap() as usize;
        state = stack.pop().unwrap();
      }
    }
  } {
    Some(()) => {
      ctx.scan_ptr = ctx.head_ptr;
      ctx.set_is_scanner(false);
      let offset = ctx.scan_ptr;
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
  bc: &[u8],
  debug: &mut Option<DebugFn>,
) -> ParseAction {
  let mut address = stack.pop().unwrap();
  let mut state = stack.pop().unwrap();

  loop {
    if state < 1 {
      let off = ctx.get_token_offset();
      if ctx.get_reader().offset_at_end(off as usize) {
        break ParseAction::Accept { production_id: ctx.get_production() };
      } else {
        break ParseAction::Error {
          last_production: ctx.get_production(),
          last_input:      TokenRange {
            len:      (ctx.scan_ptr - ctx.head_ptr) as u32,
            off:      off,
            line_num: ctx.start_line_num,
            line_off: ctx.start_line_off,
          },
        };
      }
    } else {
      let mask_gate = NORMAL_STATE_FLAG << (ctx.in_fail_mode() as u32);

      if (state & mask_gate) != 0 {
        match dispatch(address as usize, ctx, stack, bc, debug) {
          (ParseAction::CompleteState, _) => {
            ctx.set_fail_mode_to(false);
            address = stack.pop().unwrap();
            state = stack.pop().unwrap();
          }
          (ParseAction::FailState, _) => {
            ctx.set_fail_mode_to(true);
            address = stack.pop().unwrap();
            state = stack.pop().unwrap();
          }
          (action, next_state) => {
            if let Some(next_state) = next_state {
              stack.push(NORMAL_STATE_FLAG);
              stack.push(next_state.address() as u32);
            }
            break action;
          }
        }
      } else {
        address = stack.pop().unwrap();
        state = stack.pop().unwrap();
      }
    }
  }
}

pub fn parse_ast<R: ByteReader + MutByteReader + UTF8Reader, M, Node: AstObject>(
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u8],
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
