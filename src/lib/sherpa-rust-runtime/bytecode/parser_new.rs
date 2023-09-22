use crate::{
  types::{
    bytecode::{ByteCodeIterator, Instruction, MatchInputType, Opcode, NORMAL_STATE_FLAG, STATE_HEADER},
    *,
  },
  utf8::{get_token_class_from_codepoint, get_utf8_byte_length_from_code_point},
};
use std::{collections::HashMap, rc::Rc};

#[allow(unused)]
pub enum DebugEventNew<'ctx> {
  ExecuteState { base_instruction: Instruction<'ctx> },
  ExecuteInstruction { instruction: Instruction<'ctx>, cursor: usize, is_scanner: bool },
  Complete { nonterminal_id: u32 },
  ActionShift { offset_start: usize, offset_end: usize, token_id: u32 },
  ActionReduce { rule_id: u32 },
  ActionAccept,
  ActionError,
  EndOfFile,
}

pub type DebugFnNew = dyn FnMut(&DebugEventNew, &dyn ParserInput);

fn emit_state_debug(debug: &mut Option<&mut DebugFnNew>, bc: &[u8], address: usize, input: &dyn ParserInput) {
  if let Some(debug) = debug {
    debug(&DebugEventNew::ExecuteState { base_instruction: (bc, address as usize).into() }, input);
  }
}

fn emit_instruction_debug(
  debug: &mut Option<&mut DebugFnNew>,
  i: Instruction<'_>,
  input: &dyn ParserInput,
  cursor: usize,
  is_scanner: bool,
) {
  if let Some(debug) = debug {
    debug(&DebugEventNew::ExecuteInstruction { instruction: i, cursor, is_scanner }, input);
  }
}

type OpReturnVal<'a> = (ParseAction, Option<Instruction<'a>>, bool, bool);

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
fn dispatch<'a, 'debug>(
  base_address: usize,
  ctx: &mut ParseCTX,
  input: &impl ParserInput,
  bc: &'a [u8],
  debug: &mut Option<&'debug mut DebugFnNew>,
) -> (ParseAction, Option<Instruction<'a>>) {
  use ParseAction::*;

  let mut block_base: Instruction = (bc, base_address).into();
  let mut i: Instruction = block_base.clone();

  loop {
    use Opcode::*;

    i = match match i.get_opcode() {
      ByteSequence => byte_sequence(i, ctx, input),
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
      Goto => goto(i),
      PushGoto => push_goto(i, &mut ctx.stack),
      PushExceptionHandler => push_exception_handler(i, &mut ctx.stack),
      PopGoto => pop_goto(i, &mut ctx.stack),
      AssignToken => assign_token(i, ctx),
      VectorBranch => vector_branch(i, ctx, input, debug),
      HashBranch => hash_branch(i, ctx, input, debug),
      Fail => (FailState, Option::None, false, true),
      Pass => (CompleteState, Option::None, false, true),
      Accept => {
        ctx.is_finished = true;
        (ParseAction::Accept { nonterminal_id: ctx.non_terminal, final_offset: ctx.sym_ptr }, Option::None, false, true)
      }
      NoOp => (None, i.next(), false, true),
    } {
      (None, Option::None, ..) => {
        unreachable!("Expected next instruction!")
      }
      (None, Some(next_instruction), is_goto, instruction_debug) => {
        #[cfg(any(debug_assertions, feature = "wasm-lab"))]
        if instruction_debug {
          emit_instruction_debug(debug, i, input, ctx.sym_ptr, ctx.is_scanner);
        }
        if is_goto {
          block_base = next_instruction;
          #[cfg(any(debug_assertions, feature = "wasm-lab"))]
          emit_state_debug(debug, bc, block_base.address(), input);
        }
        next_instruction
      }
      (any, out, _, instruction_debug) => {
        #[cfg(any(debug_assertions, feature = "wasm-lab"))]
        if instruction_debug {
          emit_instruction_debug(debug, i, input, ctx.sym_ptr, ctx.is_scanner);
        }
        break (any, out);
      }
    }
  }
}

fn byte_sequence<'a>(
  i: Instruction<'a>,
  ctx: &mut ParseCTX,
  input: &impl ParserInput,
) -> (ParseAction, Option<Instruction<'a>>, bool, bool) {
  let mut iter = i.iter();
  let length = iter.next_u16_le().unwrap() as usize;
  let default_offset = iter.next_u32_le().unwrap() as usize;
  let offset = ctx.tok_ptr;
  let mut line_incr = 0;
  let mut line_offset = ctx.end_line_off;
  for b in 0..length {
    let byte = iter.next_u8();
    if byte != Some(input.byte(offset + b)) {
      if default_offset > 0 {
        return (ParseAction::None, Some((i.bytecode(), i.address() + default_offset).into()), false, true);
      } else {
        return (ParseAction::FailState, Option::None, false, true);
      }
    }
    if byte == Some(10) {
      line_incr += 1;
      line_offset = (offset + b) as u32;
    }
  }

  ctx.end_line_off = line_offset;
  ctx.end_line_num += line_incr;

  ctx.sym_len = length as u32;
  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::TokenShift] operation
fn shift_token<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::ShiftToken;

  debug_assert!(
    ctx.start_line_off as usize <= ctx.sym_ptr,
    "
  The `start_line_offset` should not be advanced further than the `head_ptr` at this point
  head_ptr: {}  start_line_off:{}",
    ctx.sym_ptr,
    ctx.start_line_off
  );

  let action = ParseAction::Shift {
    token_byte_offset: ctx.sym_ptr as u32,
    token_byte_length: ctx.tok_len as u32,
    token_line_count:  ctx.start_line_num,
    token_line_offset: ctx.start_line_off,
    token_id:          ctx.tok_id,
  };

  ctx.start_line_num = ctx.chkp_line_num;
  ctx.start_line_off = ctx.chkp_line_off;
  ctx.end_line_num = ctx.start_line_num;
  ctx.end_line_off = ctx.end_line_off;

  let new_offset = ctx.sym_ptr + ctx.tok_len;

  ctx.base_ptr = new_offset;
  ctx.sym_ptr = new_offset;
  ctx.tok_ptr = new_offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;

  (action, i.next(), false, true)
}

/// Performs the [Opcode::ShiftTokenScanless] operation
fn shift_token_scanless<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::ShiftTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  shift_token(i, ctx)
}

/// Performs the [Opcode::ScanShift] operation
fn scan_shift<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::ScanShift;

  ctx.tok_ptr = ctx.tok_ptr + ctx.sym_len as usize;
  ctx.sym_len = 0;
  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::PeekToken] operation
fn peek_token<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PeekToken;

  let offset = ctx.sym_ptr + ctx.tok_len;
  ctx.sym_ptr = offset;
  ctx.tok_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;

  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::PeekTokenScanless] operation
fn peek_token_scanless<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PeekTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  peek_token(i, ctx)
}

fn __skip_token_core__<'a>(base_instruction: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  let original_offset = ctx.sym_ptr;
  let offset = ctx.sym_ptr + ctx.tok_len as usize;
  let tok_len = ctx.tok_len;
  let token_id = ctx.tok_id;
  ctx.tok_ptr = offset;
  ctx.sym_ptr = offset;
  ctx.tok_id = 0;

  (
    ParseAction::Skip {
      token_byte_offset: original_offset as u32,
      token_byte_length: tok_len as u32,
      token_line_count:  ctx.start_line_num,
      token_line_offset: ctx.start_line_off,
      token_id:          token_id as u32,
    },
    Some(base_instruction),
    false,
    false,
  )
}
/// Performs the [Opcode::SkipToken] operation
fn skip_token<'a>(base_instruction: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
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
fn skip_token_scanless<'a>(base_instruction: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::SkipTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  skip_token(base_instruction, ctx)
}

/// Performs the [Opcode::PeekSkipToken] operation
fn peek_skip_token<'a>(base_instruction: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PeekSkipToken;
  __skip_token_core__(base_instruction, ctx);
  (ParseAction::None, Some(base_instruction), false, true)
}

/// Performs the [Opcode::PeekSkipTokenScanless] operation
fn peek_skip_token_scanless<'a>(base_instruction: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PeekSkipTokenScanless;
  ctx.tok_len = ctx.sym_len as usize;
  peek_skip_token(base_instruction, ctx)
}

/// Performs the [Opcode::Reduce] operation
fn reduce<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::Reduce;
  let mut iter = i.iter();
  let nonterminal_id = iter.next_u32_le().unwrap();
  let rule_id = iter.next_u32_le().unwrap();
  let symbol_count = iter.next_u16_le().unwrap() as u32;

  ctx.sym_len = symbol_count;
  ctx.non_terminal = nonterminal_id;

  (ParseAction::Reduce { nonterminal_id, rule_id, symbol_count }, i.next(), false, true)
}

/// Performs the [Opcode::PushGoto] operation
fn push_goto<'a, 'debug>(i: Instruction<'a>, stack: &mut Vec<u32>) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PushGoto;
  let mut iter = i.iter();
  let state_mode = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  stack.push(state_mode as u32 | STATE_HEADER);
  stack.push(address);

  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::PushExceptionHandler] operation
fn push_exception_handler<'a, 'debug>(i: Instruction<'a>, stack: &mut Vec<u32>) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PushExceptionHandler;
  let mut iter = i.iter();
  let state_mode = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  stack.push(state_mode as u32);
  stack.push(address);

  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::Goto] operation
fn goto<'a, 'debug>(i: Instruction<'a>) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::Goto;
  let mut iter = i.iter();
  let _state_mode_ = iter.next_u8().unwrap();
  let address = iter.next_u32_le().unwrap();

  (ParseAction::None, Some((i.bytecode(), address as usize).into()), true, true)
}

/// Performs the [Opcode::PopGoto] operation
fn pop_goto<'a, 'debug>(i: Instruction<'a>, stack: &mut Vec<u32>) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PopGoto;

  stack.pop();
  stack.pop();

  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::AssignToken] operation
fn assign_token<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::AssignToken;
  let mut iter = i.iter();
  ctx.tok_id = iter.next_u32_le().unwrap();
  ctx.tok_len = ctx.tok_ptr - ctx.sym_ptr;
  ctx.chkp_line_num = ctx.end_line_num;
  ctx.chkp_line_off = ctx.end_line_off;
  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::PeekReset] operation
fn peek_reset<'a>(i: Instruction<'a>, ctx: &mut ParseCTX) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::PeekReset;
  let offset = ctx.base_ptr;
  ctx.sym_ptr = offset;
  ctx.tok_ptr = offset;
  ctx.tok_id = 0;
  ctx.tok_len = 0;
  ctx.sym_len = 0;
  ctx.end_line_off = ctx.start_line_off;
  ctx.end_line_num = ctx.end_line_num;
  (ParseAction::None, i.next(), false, true)
}

/// Performs the [Opcode::HashBranch] operation
fn hash_branch<'a, 'debug>(
  i: Instruction<'a>,
  ctx: &mut ParseCTX,
  input: &impl ParserInput,
  debug: &mut Option<&mut DebugFnNew>,
) -> OpReturnVal<'a> {
  const __HINT__: Opcode = Opcode::HashBranch;

  emit_instruction_debug(debug, i, input, ctx.sym_ptr, ctx.is_scanner);

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
    let input_value = get_input_value(input_type, scan_block_instruction, ctx, input, debug);

    let mut hash_index = (input_value & hash_mask) as usize;

    loop {
      let mut iter: ByteCodeIterator = (i.bytecode(), table_start + hash_index * 4).into();
      let cell = iter.next_u32_le().unwrap();
      let value = cell & 0x7FF;
      let off = (cell >> 11) & 0x7FF;
      let next = ((cell >> 22) & 0x3FF) as i32 - 512;

      if value == input_value {
        return (ParseAction::None, Some((i.bytecode(), i.address() + off as usize).into()), false, false);
      } else if next != 0 {
        hash_index = ((hash_index as i32) + next) as usize;
      } else {
        return (ParseAction::None, Some(default_block), false, false);
      }
    }
  }
}

fn vector_branch<'a, 'debug>(
  i: Instruction<'a>,
  ctx: &mut ParseCTX,
  input: &impl ParserInput,
  debug: &mut Option<&'debug mut DebugFnNew>,
) -> OpReturnVal<'a> {
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

  let input_value = get_input_value(input_type, scan_block_instruction, ctx, input, debug);

  let value_index = (input_value as i32 - value_offset as i32) as usize;

  if value_index < table_length as usize {
    let mut iter: ByteCodeIterator = (i.bytecode(), table_start + value_index * 4).into();
    let address_offset = iter.next_u32_le().unwrap();
    (ParseAction::None, Some((i.bytecode(), i.address() + address_offset as usize).into()), false, false)
  } else {
    (ParseAction::None, Some(default_block), false, false)
  }
}

fn get_input_value<'a, 'debug>(
  input_type: MatchInputType,
  scan_index: Instruction<'a>,
  ctx: &mut ParseCTX,
  input: &impl ParserInput,
  debug: &mut Option<&'debug mut DebugFnNew>,
) -> u32 {
  match input_type {
    MatchInputType::NonTerminal => ctx.non_terminal as u32,
    MatchInputType::EndOfFile => (ctx.tok_ptr >= input.len()) as u32,
    MatchInputType::Token => {
      debug_assert!(!ctx.is_scanner);
      token_scan(scan_index, ctx, input, debug);
      ctx.tok_id as u32
    }
    MatchInputType::CSTNode => {
      /* Disabled for the time being. */
      u32::MAX
    }
    MatchInputType::Byte => {
      let byte = input.byte(ctx.tok_ptr);

      if byte == 10 {
        ctx.end_line_num += 1;
        ctx.end_line_off = ctx.tok_ptr as u32;
      }

      if byte > 0 {
        ctx.sym_len = 1;
      } else {
        ctx.sym_len = 0;
      }
      byte as u32
    }
    MatchInputType::ByteScanless => {
      let byte = input.byte(ctx.tok_ptr);

      if byte == 10 {
        ctx.end_line_num += 1;
        ctx.end_line_off = ctx.tok_ptr as u32;
      }

      if byte > 0 {
        ctx.tok_len = 1;
      } else {
        ctx.tok_len = 0;
      }
      byte as u32
    }
    input_type => {
      let cp: u32 = input.codepoint(ctx.tok_ptr);

      let len = get_utf8_byte_length_from_code_point(cp);

      if cp == 10 {
        ctx.end_line_num += 1;
        ctx.end_line_off = ctx.tok_ptr as u32;
      }

      match input_type {
        MatchInputType::ClassScanless => {
          ctx.tok_len = len as usize;
          if cp > 0 {
            get_token_class_from_codepoint(cp)
          } else {
            0
          }
        }
        MatchInputType::Class => {
          ctx.sym_len = len;
          if cp > 0 {
            get_token_class_from_codepoint(cp)
          } else {
            0
          }
        }
        MatchInputType::CodepointScanless => {
          ctx.tok_len = len as usize;
          cp
        }
        MatchInputType::Codepoint => {
          ctx.sym_len = len as u32;
          cp
        }
        i_type => unreachable!("{}", i_type),
      }
    }
  }
}

fn token_scan<'a, 'debug>(
  scan_index: Instruction<'a>,
  ctx: &mut ParseCTX,
  input: &impl ParserInput,
  debug: &mut Option<&'debug mut DebugFnNew>,
) {
  ctx.tok_id = 0;
  ctx.tok_ptr = ctx.sym_ptr;

  // Initialize Scanner

  // We are done with the state reference, so we
  // invalidate variable by moving the reference to
  // an unused name to prevent confusion with the
  // `scan_state` variable.
  let mut stack = vec![0, 0, NORMAL_STATE_FLAG | STATE_HEADER, scan_index.address() as u32];
  let bc = scan_index.bytecode();

  ctx.is_scanner = true;

  match {
    let mut address = stack.pop().unwrap() as usize;
    let mut state = stack.pop().unwrap();
    let ctx_fail_mode = ctx.fail_mode;

    loop {
      if state < 1 {
        ctx.fail_mode = ctx_fail_mode;
        break Some(());
      } else {
        let mask_gate = NORMAL_STATE_FLAG << (ctx.fail_mode as u32);
        if (state & mask_gate) != 0 {
          #[cfg(any(debug_assertions, feature = "wasm-lab"))]
          if state & STATE_HEADER == STATE_HEADER {
            emit_state_debug(debug, bc, address, input);
          }
          let fail_mode = loop {
            match dispatch(address, ctx, input, bc, debug) {
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
          ctx.fail_mode = fail_mode;
        }
        address = stack.pop().unwrap() as usize;
        state = stack.pop().unwrap();
      }
    }
  } {
    Some(()) => {
      ctx.tok_ptr = ctx.sym_ptr;
      ctx.is_scanner = false;
    }
    _ => panic!("Unusable State"),
  }
}

#[repr(C)]
#[derive(Clone, Default)]
pub struct ParseCTX {
  // Input data ----------
  /// The head of the input window.
  pub begin_ptr:      usize,
  /// The
  pub anchor_ptr:     usize,
  /// The the end of the last shifted token
  pub base_ptr:       usize,
  /// The the start of the token currently being evaluated.
  pub sym_ptr:        usize,
  /// The the start of the token currently being evaluated.
  pub sym_pk_ptr:     usize,
  /// The start of all unevaluated characters
  pub tok_ptr:        usize,
  /// The start of all unevaluated characters
  pub tok_pk_ptr:     usize,
  /// The end of the input window. This is a fixed reference that should
  /// not change during parsing unless the end of the input window has been
  /// reached and a larger window is requested.
  pub end_ptr:        usize,
  /// The number of characters that comprize the current
  /// token. This should be 0 if the tok_id is also 0
  pub tok_len:        usize,
  // Line info ------------
  /// The offset of the last line character recognized that proceeds the anchor
  pub start_line_off: u32,
  /// The offset of the last line character recognized that proceeds the chkp
  pub chkp_line_off:  u32,
  /// The offset of the last line character recognized that proceeds the tail
  pub end_line_off:   u32,
  /// The number of line character recognized that proceed the anchor
  pub start_line_num: u32,
  /// The number of line character recognized that proceed the chkp
  pub chkp_line_num:  u32,
  /// The number of line character recognized that proceed the tail
  pub end_line_num:   u32,
  // Parser State ----------
  /// When reducing, stores the the number of of symbols to reduce.
  pub sym_len:        u32,
  /// Tracks whether the context is a fail mode or not.
  pub state:          u32,
  /// Set to the value of a non-terminal when a rule is reduced, or
  pub non_terminal:   u32,
  /// Set to the value of a token when one is recognized. Also stores the
  /// number of symbols that are to be reduced.
  pub tok_id:         u32,
  /// When reducing, stores the rule id that is being reduced.
  pub rule_id:        u32,
  pub line_incr:      u8,
  pub is_active:      bool,
  // Miscellaneous ---------
  pub in_peek_mode:   bool,
  /// True if the last block requested input window represent data up to
  /// and including the end of input.
  pub block_is_eoi:   bool,
  // Goto stack data -----
  pub stack:          Vec<u32>,
  pub fail_mode:      bool,
  pub is_scanner:     bool,
  pub is_finished:    bool,
  // Symbol Stack
}

pub struct ByteCodeParserNew {
  stacks: Vec<ParseCTX>,
  bc: Rc<dyn AsRef<[u8]>>,
  non_terminal_lookup: HashMap<u32, u32>,
  debugger: Option<Box<DebugFnNew>>,
}

impl ByteCodeParserNew {
  pub fn new(bc: Rc<dyn AsRef<[u8]>>, non_terminal_lookup: HashMap<u32, u32>) -> Self {
    debug_assert!(bc.as_ref().as_ref().len() > 0, "Bytecode is empty!");
    ByteCodeParserNew { stacks: vec![], bc, non_terminal_lookup, debugger: None }
  }
}

impl ParserInitializer for ByteCodeParserNew {
  fn set_debugger(&mut self, debugger: Option<Box<DebugFnNew>>) {
    self.debugger = debugger;
  }

  fn get_debugger(&mut self) -> &mut Option<Box<DebugFnNew>> {
    &mut self.debugger
  }

  fn init_range(&mut self, nonterminal_goal_id: u32, start: usize, end: usize) -> Result<(), ParseError> {
    if let Some(address) = self.non_terminal_lookup.get(&nonterminal_goal_id) {
      if *address == 0 {
        Err(ParseError::InvalidNonTerminal)
      } else {
        let mut root = ParseCTX::default();
        root.base_ptr = start;
        root.anchor_ptr = start;
        root.sym_ptr = start;
        root.end_ptr = end;
        root.stack = vec![0, 0, NORMAL_STATE_FLAG | STATE_HEADER, *address];
        self.stacks = vec![root];
        Ok(())
      }
    } else {
      Err(ParseError::InvalidNonTerminal)
    }
  }

  fn init(&mut self, nonterminal_goal_id: u32) -> Result<(), ParseError> {
    if let Some(address) = self.non_terminal_lookup.get(&nonterminal_goal_id) {
      if *address == 0 {
        Err(ParseError::InvalidNonTerminal)
      } else {
        let mut root = ParseCTX::default();
        root.stack = vec![0, 0, NORMAL_STATE_FLAG | STATE_HEADER, *address];
        self.stacks = vec![root];
        Ok(())
      }
    } else {
      Err(ParseError::InvalidNonTerminal)
    }
  }
}

impl<T: ParserInput> ParserIterator<T> for ByteCodeParserNew {
  fn next(&mut self, input: &mut T) -> Option<ParseAction> {
    let Self { stacks, bc, debugger, .. } = self;

    let ctx = &mut stacks[0];

    if ctx.is_finished {
      return None;
    }

    let mut debugger = debugger.as_mut().map(|i| i.as_mut());

    let bc = bc.as_ref().as_ref();

    let mut address = unsafe { ctx.stack.pop().unwrap_unchecked() };
    let mut state = unsafe { ctx.stack.pop().unwrap_unchecked() };

    loop {
      if state < 1 {
        //Accept never encountered.
        ctx.is_finished = true;
        break Some(ParseAction::Error {
          last_nonterminal: ctx.non_terminal,
          last_input:       TokenRange {
            len:      ctx.tok_len as u32,
            off:      ctx.sym_ptr as u32,
            line_num: 0,
            line_off: 0,
          },
        });
      } else {
        let mask_gate = NORMAL_STATE_FLAG << (ctx.fail_mode as u32);
        if (state & mask_gate) != 0 {
          #[cfg(any(debug_assertions, feature = "wasm-lab"))]
          if state & STATE_HEADER == STATE_HEADER {
            //emit_state_debug(debug, bc, address as usize, stack);
          }
          match dispatch(address as usize, ctx, input, bc, &mut debugger) {
            (ParseAction::CompleteState, _) => {
              ctx.fail_mode = false;
              address = ctx.stack.pop().unwrap();
              state = ctx.stack.pop().unwrap();
            }
            (ParseAction::FailState, _) => {
              ctx.fail_mode = true;
              address = ctx.stack.pop().unwrap();
              state = ctx.stack.pop().unwrap();
            }
            (action, next_state) => {
              if let Some(next_state) = next_state {
                ctx.stack.push(NORMAL_STATE_FLAG);
                ctx.stack.push(next_state.address() as u32);
              }
              break Some(action);
            }
          }
        } else {
          address = ctx.stack.pop().unwrap();
          state = ctx.stack.pop().unwrap();
        }
      }
    }
  }
}
