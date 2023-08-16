use crate::types::{
  bytecode::{ByteCodeIterator, InputType, Instruction, Opcode, NORMAL_STATE_FLAG},
  *,
};

pub enum DebugEvent<'a> {
  ExecuteState {
    base_instruction: Instruction<'a>,
  },
  ExecuteInstruction {
    instruction: Instruction<'a>,
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
  },
  ShiftToken {
    offset_start: usize,
    offset_end:   usize,
  },
  ByteValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  CodePointValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  ClassValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  TokenValue {
    input_value: u32,
    start:       usize,
    end:         usize,
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
  EndOfFile,
}

pub type DebugFn = dyn FnMut(&DebugEvent, &str);

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
pub fn dispatch<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  base_address: usize,
  ctx: &mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &'a [u8],
  debug: &mut Option<&'debug mut DebugFn>,
) -> (ParseAction, Option<Instruction<'a>>) {
  use ParseAction::*;

  let mut block_base: Instruction = (bc, base_address).into();

  #[cfg(debug_assertions)]
  if let Some(debug) = debug.as_mut() {
    debug(&DebugEvent::ExecuteState { base_instruction: block_base }, ctx.get_str());
  }

  let mut i: Instruction = block_base.clone();

  loop {
    use Opcode::*;

    i = match match i.get_opcode() {
      DebugExpectedSymbols => debug_expected_symbols(i),
      DebugStateName => debug_symbol(i),
      ShiftToken => shift_token(i, ctx),
      ShiftTokenScanless => shift_token_scanless(i, ctx),
      ScanShift => scan_shift(i, ctx),
      SkipToken => skip_token(block_base, ctx),
      SkipTokenScanless => skip_token_scanless(block_base, ctx),
      PeekSkipToken => peek_skip_token(block_base, ctx),
      PeekSkipTokenScanless => peek_skip_token_scanless(block_base, ctx),
      PeekToken => peek_token(i, ctx),
      PeekTokenScanless => peek_token_scanless(i, ctx),
      PeekReset => peek_reset(i, ctx),
      Reduce => reduce(i, ctx),
      Goto => {
        let (parse_action, Some(new_state)) = goto(i) else { unreachable!()};
        block_base = new_state;
        #[cfg(debug_assertions)]
        if let Some(debug) = debug.as_mut() {
          debug(&DebugEvent::ExecuteState { base_instruction: block_base }, ctx.get_str());
        }
        (parse_action, Some(new_state))
      }
      PushGoto => push_goto(i, stack),
      PushExceptionHandler => push_exception_handler(i, stack),
      PopGoto => pop_goto(i, stack),
      AssignToken => assign_token(i, ctx),
      VectorBranch => vector_branch(i, ctx, debug),
      HashBranch => hash_branch(i, ctx, debug),
      Fail => (FailState, Option::None),
      Pass => (CompleteState, Option::None),
      Accept => (ParseAction::Accept { production_id: ctx.prod_id }, Option::None),
      NoOp | DebugTokenLocation => (None, i.next()),
    } {
      (None, Option::None) => {
        unreachable!("Expected next instruction!")
      }
      (None, Some(next_instruction)) => {
        #[cfg(debug_assertions)]
        if let Some(debug) = debug {
          debug(
            &DebugEvent::ExecuteInstruction {
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
            },
            ctx.get_str(),
          );
        }
        next_instruction
      }
      (any, out) => {
        #[cfg(debug_assertions)]
        if let Some(debug) = debug {
          debug(
            &DebugEvent::ExecuteInstruction {
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
            },
            ctx.get_str(),
          );
        }
        break (any, out);
      }
    }
  }
}

fn debug_expected_symbols<'a>(i: Instruction<'a>) -> (ParseAction, Option<Instruction<'a>>) {
  (ParseAction::None, i.next())
}

fn debug_symbol<'a>(i: Instruction<'a>) -> (ParseAction, Option<Instruction<'a>>) {
  (ParseAction::None, i.next())
}

/// Performs the [Opcode::TokenShift] operation
fn shift_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::ShiftToken;

  debug_assert!(
    ctx.start_line_off as usize <= ctx.head_ptr,
    "
The start line offset should never be advanced further than the head_ptr at this point
head_ptr: {}  start_line_off:{}",
    ctx.head_ptr,
    ctx.start_line_off
  );

  let action = ParseAction::Shift {
    token_byte_offset: ctx.head_ptr as u32,
    token_byte_length: ctx.tok_len as u32,
    token_line_count:  ctx.start_line_num,
    token_line_offset: ctx.start_line_off,
    token_id:          ctx.tok_id,
  };

  ctx.start_line_num = ctx.chkp_line_num;
  ctx.start_line_off = ctx.chkp_line_off;
  ctx.end_line_num = ctx.start_line_num;
  ctx.end_line_off = ctx.end_line_off;

  let new_offset = ctx.head_ptr + ctx.tok_len;

  ctx.base_ptr = new_offset;
  ctx.head_ptr = new_offset;
  ctx.scan_ptr = new_offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;

  let (a, b, c) = (new_offset, 0, 0);
  ctx.get_reader_mut().set_cursor_to(a, b, c);

  (action, i.next())
}

/// Performs the [Opcode::ShiftTokenScanless] operation
fn shift_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::ShiftTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  shift_token(i, ctx)
}

/// Performs the [Opcode::ScanShift] operation
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
fn peek_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PeekTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  peek_token(i, ctx)
}

fn __skip_token_core__<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  let original_offset = ctx.head_ptr;
  let offset = ctx.head_ptr + ctx.tok_len as usize;
  let tok_len = ctx.tok_len;
  let token_id = ctx.tok_id;
  ctx.scan_ptr = offset;
  ctx.head_ptr = offset;
  ctx.tok_id = 0;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

  (
    ParseAction::Skip {
      token_byte_offset: original_offset as u32,
      token_byte_length: tok_len as u32,
      token_line_count:  ctx.start_line_num,
      token_line_offset: ctx.start_line_off,
      token_id:          token_id as u32,
    },
    Some(base_instruction),
  )
}
/// Performs the [Opcode::SkipToken] operation
fn skip_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::SkipToken;
  let result = __skip_token_core__(base_instruction, ctx);
  ctx.start_line_num = ctx.chkp_line_num;
  ctx.start_line_off = ctx.chkp_line_off;
  ctx.end_line_num = ctx.start_line_num;
  ctx.end_line_off = ctx.end_line_off;
  //ctx.base_ptr = ctx.head_ptr;
  result
}

/// Performs the [Opcode::SkipTokenScanless] operation
fn skip_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::SkipTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  skip_token(base_instruction, ctx)
}

/// Performs the [Opcode::PeekSkipToken] operation
fn peek_skip_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PeekSkipToken;
  __skip_token_core__(base_instruction, ctx);
  (ParseAction::None, Some(base_instruction))
}

/// Performs the [Opcode::PeekSkipTokenScanless] operation
fn peek_skip_token_scanless<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  base_instruction: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PeekSkipTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  peek_skip_token(base_instruction, ctx)
}

/// Performs the [Opcode::Reduce] operation
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
fn goto<'a>(i: Instruction<'a>) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::Goto;
  let mut iter = i.iter();
  let _state_mode_ = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  (ParseAction::None, Some((i.bytecode(), address as usize).into()))
}

/// Performs the [Opcode::PopGoto] operation
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
fn assign_token<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::AssignToken;
  let mut iter = i.iter();
  ctx.tok_id = iter.next_u32_le().unwrap();
  ctx.tok_len = ctx.scan_ptr - ctx.head_ptr;
  ctx.chkp_line_num = ctx.end_line_num;
  ctx.chkp_line_off = ctx.end_line_off;
  (ParseAction::None, i.next())
}

/// Performs the [Opcode::PeekReset] operation
fn peek_reset<'a, R: ByteReader + MutByteReader + UTF8Reader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
) -> (ParseAction, Option<Instruction<'a>>) {
  const __HINT__: Opcode = Opcode::PeekReset;
  let offset = ctx.base_ptr;
  ctx.head_ptr = offset;
  ctx.scan_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;
  ctx.sym_len = 0;
  ctx.end_line_off = ctx.start_line_off;
  ctx.end_line_num = ctx.end_line_num;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);
  (ParseAction::None, i.next())
}

/// Performs the [Opcode::HashBranch] operation
pub fn hash_branch<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<&'debug mut DebugFn>,
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
    let input_value = get_input_value(input_type, scan_block_instruction, ctx, debug);

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

pub fn vector_branch<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  i: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<&'debug mut DebugFn>,
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

  let input_value = get_input_value(input_type, scan_block_instruction, ctx, debug);

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

fn get_input_value<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  input_type: InputType,
  scan_index: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<&'debug mut DebugFn>,
) -> u32 {
  match input_type {
    InputType::Production => ctx.get_production() as u32,
    InputType::EndOfFile => (ctx.scan_ptr >= ctx.end_ptr) as u32,
    InputType::Class => {
      let scan_len = ctx.get_reader().codepoint_byte_length();
      ctx.sym_len = scan_len;
      ctx.get_reader().class() as u32
    }
    InputType::Codepoint => {
      let scan_len = ctx.get_reader().codepoint_byte_length();
      ctx.sym_len = scan_len;
      ctx.get_reader().codepoint() as u32
    }
    InputType::Byte => {
      let byte = ctx.get_reader().byte();

      if byte == 10 {
        ctx.end_line_num += 1;
        ctx.end_line_off = ctx.scan_ptr as u32;
      }

      if byte > 0 {
        ctx.sym_len = 1;
      } else {
        ctx.sym_len = 0;
      }
      byte as u32
    }
    InputType::Token => {
      debug_assert!(!ctx.is_scanner());

      token_scan(scan_index, ctx, debug);

      ctx.tok_id as u32
    }
    _ => unreachable!(),
  }
}

fn emit_debug_value<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<&'debug mut DebugFn>,
  input_type: InputType,
  input_value: u32,
) {
  let start = ctx.scan_ptr;
  let end = ctx.scan_ptr + ctx.tok_len;
  if let Some(debug) = debug {
    match input_type {
      InputType::Production => {
        debug(&DebugEvent::GotoValue { production_id: input_value }, ctx.get_str())
      }
      InputType::Byte => debug(&DebugEvent::ByteValue { input_value, start, end }, ctx.get_str()),
      InputType::Class => debug(&DebugEvent::ClassValue { input_value, start, end }, ctx.get_str()),
      InputType::Token => debug(&DebugEvent::TokenValue { input_value, start, end }, ctx.get_str()),
      InputType::Codepoint => {
        debug(&DebugEvent::CodePointValue { input_value, start, end }, ctx.get_str())
      }
      InputType::EndOfFile => debug(&DebugEvent::EndOfFile, ctx.get_str()),
      _ => unreachable!(),
    };
  }
}

fn token_scan<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  scan_index: Instruction<'a>,
  ctx: &mut ParseContext<R, M>,
  debug: &mut Option<&'debug mut DebugFn>,
) {
  ctx.tok_id = 0;
  ctx.scan_ptr = ctx.head_ptr;
  let offset = ctx.scan_ptr;
  ctx.get_reader_mut().set_cursor_to(offset, 0, 0);

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
pub fn get_next_action<'a, 'debug, R: ByteReader + MutByteReader + UTF8Reader, M>(
  ctx: &'a mut ParseContext<R, M>,
  stack: &mut Vec<u32>,
  bc: &[u8],
  debug: &mut Option<&'debug mut DebugFn>,
) -> ParseAction {
  let mut address = stack.pop().unwrap();
  let mut state = stack.pop().unwrap();

  loop {
    if state < 1 {
      //Accept never encountered.
      break ParseAction::Error {
        last_production: ctx.prod_id,
        last_input:      TokenRange {
          len:      ctx.tok_len as u32,
          off:      ctx.head_ptr as u32,
          line_num: 0,
          line_off: 0,
        },
      };
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

pub struct ByteCodeParser<'a, R: ByteReader + MutByteReader, M> {
  ctx:   ParseContext<R, M>,
  stack: Vec<u32>,
  bc:    &'a [u8],
}

impl<'a, R: ByteReader + MutByteReader, M> ByteCodeParser<'a, R, M> {
  pub fn new(reader: &mut R, bc: &'a [u8]) -> Self {
    ByteCodeParser { ctx: ParseContext::<R, M>::new_bytecode(reader), stack: vec![], bc }
  }
}

impl<'a, R: ByteReader + MutByteReader + UTF8Reader, M> SherpaParser<R, M, true>
  for ByteCodeParser<'a, R, M>
{
  fn get_ctx(&self) -> &ParseContext<R, M> {
    &self.ctx
  }

  fn get_ctx_mut(&mut self) -> &mut ParseContext<R, M> {
    &mut self.ctx
  }

  fn head_at_end(&self) -> bool {
    self.ctx.head_ptr == self.get_reader().len()
  }

  fn get_token_length(&self) -> u32 {
    self.ctx.get_token_length()
  }

  fn get_token_id(&self) -> u32 {
    self.ctx.tok_id
  }

  fn get_token_offset(&self) -> u32 {
    self.ctx.get_token_offset()
  }

  fn get_token_line_number(&self) -> u32 {
    self.ctx.start_line_num
  }

  fn get_token_line_offset(&self) -> u32 {
    self.ctx.start_line_off
  }

  fn get_production_id(&self) -> u32 {
    self.ctx.prod_id
  }

  fn get_reader(&self) -> &R {
    self.ctx.get_reader()
  }

  fn get_reader_mut(&mut self) -> &mut R {
    self.ctx.get_reader_mut()
  }

  fn get_input(&self) -> &str {
    unsafe { std::str::from_utf8_unchecked(self.get_reader().get_bytes()) }
  }

  fn init_parser(&mut self, entry_point: u32) {
    self.stack = vec![0, 0, NORMAL_STATE_FLAG, entry_point];
    self.ctx.end_ptr = self.get_reader().len();
    self.ctx.anchor_ptr = 0;
    self.ctx.base_ptr = 0;
    self.ctx.head_ptr = 0;
    self.ctx.scan_ptr = 0;
    self.get_reader_mut().set_cursor(0);
    self.get_reader_mut().next(0);
  }

  fn get_next_action<'debug>(&mut self, debug: &mut Option<&'debug mut DebugFn>) -> ParseAction {
    let ByteCodeParser { ctx, stack, bc } = self;
    get_next_action(ctx, stack, bc, debug)
  }
}
