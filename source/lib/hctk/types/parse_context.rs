use std::collections::VecDeque;

use super::*;
#[derive(Clone)]
#[repr(C)]
pub struct ParseContext<T: CharacterReader>
{
  pub(crate) peek_token: ParseToken,
  pub(crate) anchor_token: ParseToken,
  pub(crate) assert_token: ParseToken,
  get_byte_block_at_cursor: fn(&mut T, &mut *const u8, u32, u32) -> u32,
  action_ptr: *mut ParseAction,
  reader: *mut T,
  input_block: usize,
  stack_base: *const u64,
  stack_top: usize,
  stack_size: u32,
  input_block_length: u32,
  input_block_offset: u32,
  production: u32,
  state: u32,
  in_peek_mode: u32,
  local_state_stack: [u32; 32],
}

impl<T: CharacterReader> ParseContext<T>
{
  pub fn new(reader: &mut T) -> Self
  {
    let mut ctx = Self {
      peek_token: ParseToken::default(),
      anchor_token: ParseToken::default(),
      assert_token: ParseToken::default(),
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
      get_byte_block_at_cursor: T::get_byte_block_at_cursor,
      in_peek_mode: 0,
      local_state_stack: [0; 32],
    };
    ctx
  }

  pub fn bytecode_context() -> Self
  {
    const stack_32_bit_size: usize = 32;
    let mut ctx = Self {
      peek_token: ParseToken::default(),
      anchor_token: ParseToken::default(),
      assert_token: ParseToken::default(),
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
      get_byte_block_at_cursor: T::get_byte_block_at_cursor,
      local_state_stack: [0; stack_32_bit_size],
      in_peek_mode: 0,
    };

    ctx
  }

  /// The following methods are used exclusively by the
  /// the rust functions in [hctk::runtime::parser_functions.rs]

  #[inline]
  pub(crate) fn in_fail_mode(&self) -> bool
  {
    self.input_block_offset > 0
  }

  #[inline]
  pub(crate) fn set_fail_mode_to(&mut self, is_in_fail_mode: bool)
  {
    self.state = if is_in_fail_mode {
      FAIL_STATE_FLAG
    } else {
      NORMAL_STATE_FLAG
    }
  }

  #[inline]
  pub(crate) fn in_peek_mode(&self) -> bool
  {
    self.in_peek_mode > 0
  }

  #[inline]
  pub(crate) fn set_peek_mode_to(&mut self, is_in_peek_mode: bool)
  {
    self.in_peek_mode = is_in_peek_mode as u32;
  }

  #[inline]
  pub(crate) fn is_interrupted(&self) -> bool
  {
    self.action_ptr as usize > 0
  }

  #[inline]
  pub(crate) fn set_interrupted_to(&mut self, is_interrupted: bool)
  {
    self.action_ptr = is_interrupted as usize as *mut ParseAction
  }

  /// Used by the bytecode interpreter
  #[inline]
  pub(crate) fn is_scanner(&self) -> bool
  {
    self.input_block > 0
  }

  /// Used by the bytecode interpreter
  #[inline]
  pub(crate) fn make_scanner(&mut self)
  {
    self.input_block = 1;
  }

  #[inline]
  pub(crate) fn get_active_state(&mut self) -> u32
  {
    self.state as u32
  }

  #[inline]
  pub(crate) fn set_active_state_to(&mut self, state: u32)
  {
    self.state = state;
  }

  #[inline]
  pub(crate) fn get_production(&mut self) -> u32
  {
    self.production
  }

  #[inline]
  pub(crate) fn set_production_to(&mut self, production: u32)
  {
    self.production = production;
  }

  #[inline]
  pub(crate) fn pop_state(&mut self) -> u32
  {
    if self.stack_top > 0 {
      self.stack_top -= 1;
      self.local_state_stack[self.stack_top] as u32
    } else {
      0
    }
  }

  #[inline]
  pub(crate) fn push_state(&mut self, state: u32)
  {
    if (self.stack_top >= self.stack_size as usize) {
      panic!("Out of parse stack space!");
    }

    self.local_state_stack[self.stack_top] = state;

    self.stack_top += 1;
  }

  #[inline]
  pub fn init_normal_state(&mut self, entry_point: u32)
  {
    self.stack_top = 0;

    self.push_state((NORMAL_STATE_FLAG | entry_point));
  }
}

#[no_mangle]
pub extern "C" fn hctk_extend_stack(stack: &mut Vec<usize>) -> usize
{
  let old_size = stack.len();
  if let Err(err) = stack.try_reserve(stack.len() << 1) {
    println!("Error on parse stack extension {}", err);
    0
  } else {
    // pad out stack if there is more
    // then double the original size
    for _ in 0..(stack.capacity() - (old_size << 1)) {
      stack.push(0);
    }

    // move all element to back of stack.
    for i in 0..old_size {
      stack.push(stack[i]);
    }
    1
  }
}
#[no_mangle]
pub extern "C" fn hctk_get_stack_pointer<'a>(stack: &mut Vec<usize>) -> *mut usize
{
  let ptr = stack.as_mut_ptr();

  ptr
}
#[no_mangle]
pub extern "C" fn hctk_get_stack_size(stack: &Vec<usize>) -> usize
{
  let size = stack.len() << 3;

  size
}
