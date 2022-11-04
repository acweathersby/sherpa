use crate::types::*;
use crate::utf8::get_token_class_from_codepoint;
use regex::internal::Input;
use std::alloc::alloc;
use std::alloc::dealloc;
use std::alloc::Layout;
use std::collections::VecDeque;
use std::fmt::Debug;

const stack_32_bit_size: usize = 128;
pub struct ParseContext<T: BaseCharacterReader + MutCharacterReader> {
  pub(crate) peek: ParseToken,
  pub(crate) anchor: ParseToken,
  pub(crate) assert: ParseToken,
  pub action_ptr: *mut ParseAction,
  pub reader: *mut T,
  pub input_block: usize,
  pub stack_base: *const u64,
  pub stack_top: usize,
  pub stack_size: u32,
  pub input_block_length: u32,
  pub input_block_offset: u32,
  pub production: u32,
  pub state: u32,
  pub in_peek_mode: u32,
  pub local_state_stack: [u32; stack_32_bit_size],
}

impl<T: BaseCharacterReader + MutCharacterReader> ParseContext<T> {
  pub fn new(reader: &mut T) -> Self {
    let mut ctx = Self {
      peek: ParseToken::default(),
      anchor: ParseToken::default(),
      assert: ParseToken::default(),
      action_ptr: 0 as *mut ParseAction,
      stack_base: [].as_mut_ptr(),
      stack_top: 0,
      state: NORMAL_STATE_FLAG,
      production: 0,
      input_block: 0,
      input_block_length: 0,
      input_block_offset: 0,
      stack_size: 0,
      reader,
      in_peek_mode: 0,
      local_state_stack: [0; stack_32_bit_size],
    };
    ctx
  }

  pub fn bytecode_context() -> Self {
    let mut ctx = Self {
      peek: ParseToken::default(),
      anchor: ParseToken::default(),
      assert: ParseToken::default(),
      action_ptr: 0 as *mut ParseAction,
      stack_base: [].as_mut_ptr(),
      stack_top: 0,
      stack_size: (stack_32_bit_size as u32) >> 1,
      state: 0,
      production: 0,
      input_block: 0,
      input_block_length: 0,
      input_block_offset: 0,
      reader: 0 as *mut T,
      local_state_stack: [0; stack_32_bit_size],
      in_peek_mode: 0,
    };

    ctx
  }

  /// The following methods are used exclusively by the
  /// the rust functions in [hctk::runtime::parser_functions.rs]

  #[inline]
  pub(crate) fn in_fail_mode(&self) -> bool {
    self.input_block_offset > 0
  }

  #[inline]
  pub(crate) fn set_fail_mode_to(&mut self, is_in_fail_mode: bool) {
    self.input_block_offset = if is_in_fail_mode { 1 } else { 0 }
  }

  #[inline]
  pub(crate) fn in_peek_mode(&self) -> bool {
    self.in_peek_mode > 0
  }

  #[inline]
  pub(crate) fn set_peek_mode_to(&mut self, is_in_peek_mode: bool) {
    self.in_peek_mode = is_in_peek_mode as u32;
  }

  #[inline]
  pub(crate) fn is_interrupted(&self) -> bool {
    self.action_ptr as usize > 0
  }

  #[inline]
  pub(crate) fn set_interrupted_to(&mut self, is_interrupted: bool) {
    self.action_ptr = is_interrupted as usize as *mut ParseAction
  }

  /// Used by the bytecode interpreter
  #[inline]
  pub(crate) fn is_scanner(&self) -> bool {
    self.input_block > 0
  }

  /// Used by the bytecode interpreter
  #[inline]
  pub(crate) fn make_scanner(&mut self) {
    self.input_block = 1;
  }

  #[inline]
  pub(crate) fn get_active_state(&mut self) -> u32 {
    self.state as u32
  }

  #[inline]
  pub(crate) fn set_active_state_to(&mut self, state: u32) {
    self.state = state;
  }

  #[inline]
  pub(crate) fn get_production(&mut self) -> u32 {
    self.production
  }

  #[inline]
  pub(crate) fn set_production_to(&mut self, production: u32) {
    self.production = production;
  }

  #[inline]
  pub(crate) fn pop_state(&mut self) -> u32 {
    if self.stack_top > 0 {
      self.stack_top -= 1;
      self.local_state_stack[self.stack_top] as u32
    } else {
      0
    }
  }

  #[inline]
  pub(crate) fn push_state(&mut self, state: u32) {
    if (self.stack_top >= self.stack_size as usize) {
      panic!("Out of parse stack space! {} {}", self.stack_top, self.stack_size);
    }

    self.local_state_stack[self.stack_top] = state;

    self.stack_top += 1;
  }

  #[inline]
  pub fn init_normal_state(&mut self, entry_point: u32) {
    self.stack_top = 0;

    self.push_state((NORMAL_STATE_FLAG | entry_point));
  }
}

#[derive(Clone, Debug, Copy)]
#[repr(C)]
pub struct CodepointInfo {
  pub val: u32,
  pub len: u32,
}

#[derive(Clone, Debug, Copy)]
#[repr(C)]
pub struct Goto {
  pub goto_fn: *const usize,
  pub state:   u32,
  pub meta:    u32,
}

impl Default for Goto {
  fn default() -> Self {
    Self { goto_fn: 0 as *const usize, state: 0, meta: 0 }
  }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct InputBlock {
  /// The pointer to the beginning of the block window slice.
  pub block:        *const u8,
  /// The offset of this block, calculated as the relative distance
  /// from the start of the input string to the InputBlock's pointer
  /// position.
  pub off:          u32,
  /// The number of bytes the block window can view
  pub len:          u32,
  /// Indicates the input continues outside the block's boundary,
  /// but such input is not accessible at this point. Should result
  /// in an End_Of_Input parse action if parsing fails on the block's
  /// upper boundary.
  pub is_truncated: bool,
}

impl Default for InputBlock {
  fn default() -> Self {
    Self {
      block:        0 as *const u8,
      off:          0,
      len:          0,
      is_truncated: false,
    }
  }
}

#[derive(Clone)]
#[repr(C)]
pub struct LLVMParseContext<T: LLVMCharacterReader + ByteCharacterReader + BaseCharacterReader> {
  pub local_goto_stack: [Goto; 8],
  pub anchor_token: ParseToken,
  pub assert_token: ParseToken,
  pub peek_token: ParseToken,
  pub input_block: InputBlock,
  pub stack_base: *const Goto,
  pub stack_top: *const Goto,
  pub get_byte_block_at_cursor: fn(&mut T, &mut InputBlock),
  pub reader: *mut T,
  pub stack_size: u32,
  pub production: u32,
  pub state: u32,
  pub in_peek_mode: u32,
}

impl<T: LLVMCharacterReader + ByteCharacterReader + BaseCharacterReader> Debug
  for LLVMParseContext<T>
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut dbgstr = f.debug_struct("LLVMParseContext");
    dbgstr.field("local_goto_stack", &self.local_goto_stack);
    dbgstr.field("anchor_token", &self.anchor_token);
    dbgstr.field("assert_token", &self.assert_token);
    dbgstr.field("peek_token", &self.peek_token);
    dbgstr.field("input_block", &self.input_block);
    dbgstr.field("stack_base", &self.stack_base);
    dbgstr.field("stack_top", &self.stack_top);
    dbgstr.field("get_byte_block_at_cursor", &"MASKED");
    dbgstr.field("reader", &self.reader);
    dbgstr.field("stack_size", &self.stack_size);
    dbgstr.field("production", &self.production);
    dbgstr.field("state", &self.state);
    dbgstr.field("in_peek_mode", &self.in_peek_mode);
    dbgstr.finish()
  }
}

impl<T: LLVMCharacterReader + ByteCharacterReader + BaseCharacterReader> LLVMParseContext<T> {
  pub fn new(reader: &mut T) -> Self {
    let mut ctx = Self {
      peek_token: ParseToken::default(),
      anchor_token: ParseToken::default(),
      assert_token: ParseToken::default(),
      stack_base: 0 as *const Goto,
      stack_top: 0 as *const Goto,
      state: 0,
      production: 0,
      input_block: InputBlock::default(),
      stack_size: 0,
      reader: reader,
      get_byte_block_at_cursor: T::get_byte_block_at_cursor,
      in_peek_mode: 0,
      local_goto_stack: [Goto::default(); 8],
    };
    ctx
  }
}

#[no_mangle]
pub extern "C" fn hctk_allocate_stack(num_of_slots: u32) -> *mut Goto {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.
  let layout = Layout::from_size_align((num_of_slots << 4) as usize, 16).unwrap();

  unsafe { alloc(layout) as *mut Goto }
}

#[no_mangle]
pub extern "C" fn hctk_free_stack(stack_base: *mut Goto, num_of_slots: u32) {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.
  let layout = Layout::from_size_align((num_of_slots << 4) as usize, 16).unwrap();

  unsafe { dealloc(stack_base as *mut u8, layout) }
}

#[no_mangle]
pub extern "C" fn hctk_get_token_class_from_codepoint(codepoint: u32) -> u32 {
  get_token_class_from_codepoint(codepoint)
}
