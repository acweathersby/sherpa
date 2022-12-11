use super::*;
use crate::utf8::get_token_class_from_codepoint;
use std::{
  alloc::{alloc, dealloc, Layout},
  fmt::Debug,
  num,
  time::Instant,
};

const STACK_32_BIT_SIZE: usize = 128;
pub struct ParseContext<T: BaseCharacterReader + MutCharacterReader> {
  pub peek: ParseToken,
  pub anchor: ParseToken,
  pub assert: ParseToken,
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
  pub local_state_stack: Vec<u32>,
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
      local_state_stack: vec![],
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
      stack_size: (STACK_32_BIT_SIZE as u32) >> 1,
      state: 0,
      production: 0,
      input_block: 0,
      input_block_length: 0,
      input_block_offset: 0,
      reader: 0 as *mut T,
      local_state_stack: vec![],
      in_peek_mode: 0,
    };

    ctx
  }

  /// The following methods are used exclusively by the
  /// the rust functions in [sherpa::runtime::parser_functions.rs]

  #[inline]
  pub fn in_fail_mode(&self) -> bool {
    self.input_block_offset > 0
  }

  #[inline]
  pub fn set_fail_mode_to(&mut self, is_in_fail_mode: bool) {
    self.input_block_offset = if is_in_fail_mode { 1 } else { 0 }
  }

  #[inline]
  pub fn in_peek_mode(&self) -> bool {
    self.in_peek_mode > 0
  }

  #[inline]
  pub fn set_peek_mode_to(&mut self, is_in_peek_mode: bool) {
    self.in_peek_mode = is_in_peek_mode as u32;
  }

  #[inline]
  pub fn is_interrupted(&self) -> bool {
    self.action_ptr as usize > 0
  }

  #[inline]
  pub fn set_interrupted_to(&mut self, is_interrupted: bool) {
    self.action_ptr = is_interrupted as usize as *mut ParseAction
  }

  /// Used by the bytecode interpreter
  #[inline]
  pub fn is_scanner(&self) -> bool {
    self.input_block > 0
  }

  /// Used by the bytecode interpreter
  #[inline]
  pub fn make_scanner(&mut self) {
    self.input_block = 1;
  }

  #[inline]
  pub fn get_active_state(&mut self) -> u32 {
    self.state as u32
  }

  #[inline]
  pub fn set_active_state_to(&mut self, state: u32) {
    self.state = state;
  }

  #[inline]
  pub fn get_production(&mut self) -> u32 {
    self.production
  }

  #[inline]
  pub fn set_production_to(&mut self, production: u32) {
    self.production = production;
  }

  #[inline]
  pub fn pop_state(&mut self) -> u32 {
    if self.stack_top > 0 {
      self.stack_top -= 1;
      return self.local_state_stack.pop().unwrap();
    } else {
      0
    }
  }

  #[inline]
  pub fn push_state(&mut self, state: u32) {
    self.local_state_stack.push(state);

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
  pub block: *const u8,
  /// The global index of the beginning by of this block, when counted from
  /// from the beginning of the input string.
  pub start: u32,
  /// The global index of the position just after the last byte of this block,
  /// when counted from the beginning of the input string.
  pub end: u32,
  /// The number of bytes that can be ready from this block.
  pub readable_bytes: u32,
  /// Indicates the input continues outside the block's boundary,
  /// but such input is not accessible at this point. Should result
  /// in an End_Of_Input parse action if parsing fails due to lack of
  /// additional input.
  pub is_truncated: bool,
}

impl Default for InputBlock {
  fn default() -> Self {
    Self {
      block: 0 as *const u8,
      start: 0,
      end: 0,
      readable_bytes: 0,
      is_truncated: true,
    }
  }
}

pub const LLVM_BASE_STACK_SIZE: usize = 8;

#[derive(Clone)]
#[repr(C)]
pub struct LLVMParseContext<T: LLVMCharacterReader + ByteCharacterReader + BaseCharacterReader> {
  pub local_goto_stack: [Goto; LLVM_BASE_STACK_SIZE],
  pub anchor_token: ParseToken,
  pub assert_token: ParseToken,
  pub peek_token: ParseToken,
  pub input_block: InputBlock,
  pub goto_stack_ptr: *const Goto,
  pub goto_stack_size: u32,
  pub goto_stack_remaining: u32,
  pub get_byte_block_at_cursor: fn(&mut T, &mut InputBlock),
  pub reader: *mut T,
  pub production: u32,
  pub state: u32,
  pub in_peek_mode: bool,
  pub is_active: bool,
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
    dbgstr.field("goto_stack_base", &self.goto_stack_ptr);
    dbgstr.field("goto_stack_size", &self.goto_stack_size);
    dbgstr.field("goto_stack_remaining", &self.goto_stack_remaining);
    dbgstr.field("get_byte_block_at_cursor", &"MASKED");
    dbgstr.field("reader", &self.reader);
    dbgstr.field("production", &self.production);
    dbgstr.field("state", &self.state);
    dbgstr.field("in_peek_mode", &self.in_peek_mode);
    dbgstr.finish()
  }
}

impl<T: LLVMCharacterReader + ByteCharacterReader + BaseCharacterReader> LLVMParseContext<T> {
  pub fn new() -> Self {
    Self {
      peek_token: ParseToken::default(),
      anchor_token: ParseToken::default(),
      assert_token: ParseToken::default(),
      goto_stack_ptr: 0 as *const Goto,
      goto_stack_size: 0,
      goto_stack_remaining: 0,
      state: 0,
      production: 0,
      input_block: InputBlock::default(),
      reader: 0 as *mut T,
      get_byte_block_at_cursor: T::get_byte_block_at_cursor,
      local_goto_stack: [Goto::default(); LLVM_BASE_STACK_SIZE],
      in_peek_mode: false,
      is_active: false,
    }
  }
}

#[no_mangle]
pub extern "C" fn sherpa_allocate_stack(num_of_slots: u32) -> *mut Goto {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.

  let layout = Layout::from_size_align((num_of_slots << 4) as usize, 16).unwrap();

  unsafe {
    let ptr = alloc(layout) as *mut Goto;

    #[cfg(debug_assertions)]
    {
      println!(
        "ALLOCATION OF {} bytes for {} slots at address: {:p}",
        num_of_slots << 4,
        num_of_slots,
        ptr
      );
    }

    ptr
  }
}

#[no_mangle]
pub extern "C" fn sherpa_free_stack(ptr: *mut Goto, num_of_slots: u32) {
  #[cfg(debug_assertions)]
  {
    println!("Freeing {} bytes for {} slots at address {:p}", num_of_slots << 4, num_of_slots, ptr);
  }
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.
  let layout = Layout::from_size_align((num_of_slots << 4) as usize, 16).unwrap();

  unsafe { dealloc(ptr as *mut u8, layout) }
}

#[no_mangle]
pub extern "C" fn sherpa_get_token_class_from_codepoint(codepoint: u32) -> u32 {
  get_token_class_from_codepoint(codepoint)
}
