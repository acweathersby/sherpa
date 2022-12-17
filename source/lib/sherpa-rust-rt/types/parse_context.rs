use super::*;
use crate::utf8::get_token_class_from_codepoint;
use std::{
  alloc::{alloc, dealloc, Layout},
  fmt::Debug,
  num,
  time::Instant,
};

const STACK_32_BIT_SIZE: usize = 128;
pub struct ParseContext<T: ByteReader + MutByteReader> {
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

impl<T: ByteReader + MutByteReader> ParseContext<T> {
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
pub struct LLVMParseContext<T: LLVMByteReader + ByteReader> {
  pub input_block: InputBlock,
  pub goto_stack_ptr: *mut Goto,
  pub reader: *mut T,
  pub get_input_block: fn(&mut T, &mut InputBlock),
  //pub custom_lex: fn(&mut T, &LLVMParseContext<T>) -> u32,
  pub anchor_offset: u64,
  pub token_offset: u64,
  pub peek_offset: u64,
  pub token_length: u64,
  pub token_type: u64,
  pub line_data: u64,
  pub goto_stack_size: u32,
  pub goto_stack_remaining: u32,
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
    dbgstr.field("anchor_offset", &self.token_offset);
    dbgstr.field("peek_offset", &self.token_offset);
    dbgstr.field("token_offset", &self.token_offset);
    dbgstr.field("token_length", &self.line_data);
    dbgstr.field("line_data", &self.line_data);
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

impl<T: LLVMByteReader + ByteReader> LLVMParseContext<T> {
  pub fn new() -> Self {
    Self {
      goto_stack_ptr: 0 as *mut Goto,
      goto_stack_size: 0,
      goto_stack_remaining: 0,
      token_offset: 0,
      line_data: 0,
      anchor_offset: 0,
      peek_offset: 0,
      token_length: 0,
      token_type: 0,
      state: 0,
      production: 0,
      input_block: InputBlock::default(),
      reader: 0 as *mut T,
      get_input_block: T::get_byte_block_at_cursor,
      in_peek_mode: false,
      is_active: false,
    }
  }

  fn get_source(&mut self) -> SharedSymbolBuffer {
    unsafe { (*self.reader).get_source() }
  }
}

impl<T: UTF8Reader + LLVMByteReader + ByteReader> LLVMParseContext<T> {
  pub fn get_str<'a>(&'a mut self) -> &'a str {
    unsafe { (*self.reader).get_str() }
  }
}

#[no_mangle]
pub extern "C" fn sherpa_allocate_stack(byte_size: usize) -> *mut Goto {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.

  let layout = Layout::from_size_align(byte_size, 16).unwrap();

  unsafe {
    let ptr = alloc(layout) as *mut Goto;

    #[cfg(debug_assertions)]
    {
      println!(
        "ALLOCATION OF {} bytes for {} slots at address: {:p}",
        byte_size,
        byte_size >> 4,
        ptr
      );
    }

    ptr
  }
}

pub trait AstSlot: Debug + Clone + Default + Sized {}

/// Used within an LLVM parser to provide access to intermediate AST
/// data stored on the stack within a dynamically resizable array.
#[repr(C)]
pub struct AstSlots<T: AstSlot> {
  stack_data: *mut T,
  stack_size: u32,
}

impl<T: AstSlot> AstSlots<T> {
  #[track_caller]
  fn get_pointer(&self, position: usize) -> *mut T {
    if position >= (self.stack_size as usize) {
      panic!(
        "Could not get AST node at slot ${} from stack with a length of {}",
        position, self.stack_size
      );
    }
    let slot_size = std::mem::size_of::<T>();
    // We are using the stack space for these slots,
    // which we ASSUME grows downward, hence the "higher" slots
    // are accessed through lower addresses.
    (self.stack_data as usize - (position * slot_size)) as *mut T
  }

  /// Assigns the given data to a garbage slot, ignoring any existing value
  /// the slot may contain. This is only used when shifting token data into
  /// an "empty" slot through the Shift action.
  unsafe fn assign_to_garbage(&mut self, position: usize, val: T) {
    let pointer = self.get_pointer(position);
    std::mem::forget(std::mem::replace(&mut (*pointer), val));
  }

  /// Moves the last slot into the first's slots position,
  /// and drops the values of all other slots.
  pub fn drop_all_but_last(&mut self) {
    if self.len() == 1 {
      return;
    }

    let last = self.take(self.len() - 1);
    self.assign(0, last);

    for index in 1..self.len() {
      self.take(index);
    }
  }

  pub fn assign(&mut self, position: usize, val: T) {
    unsafe {
      let pointer = self.get_pointer(position);
      *pointer = val;
    }
  }

  /// Removes the value at the given position from the stack and returns it.
  ///
  pub fn take(&mut self, position: usize) -> T {
    unsafe { std::mem::take(&mut (*self.get_pointer(position))) }
  }

  pub fn clone(&self, position: usize) -> T {
    unsafe { (*self.get_pointer(position)).clone() }
  }

  pub fn len(&self) -> usize {
    self.stack_size as usize
  }

  pub fn destroy(mut self) {
    self.to_vec();
  }

  pub fn to_vec(&mut self) -> Vec<T> {
    let mut output = vec![];
    for i in 0..self.stack_size {
      output.push(self.take(i as usize));
    }
    output
  }
}

impl<T: AstSlot> Debug for AstSlots<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut dbgstr = f.debug_struct("SlotSlice");
    dbgstr.field("stack_size", &self.stack_size);
    let slot_size = std::mem::size_of::<T>();
    dbgstr.field("[slot byte size]", &slot_size);
    for i in 0..self.stack_size {
      dbgstr.field(&format!("slot[{}]", i), &(self.clone(i as usize)));
    }

    dbgstr.finish()
  }
}

impl AstSlot for u32 {}

impl<V: AstSlot> AstSlot for (V, TokenRange, TokenRange) {}

pub unsafe fn llvm_map_shift_action<
  'a,
  T: LLVMByteReader + ByteReader + MutByteReader,
  V: AstSlot,
>(
  ctx: &mut LLVMParseContext<T>,
  action: &ParseAction,
  slots: &mut AstSlots<(V, TokenRange, TokenRange)>,
) {
  match *action {
    ParseAction::Shift {
      anchor_byte_offset,
      token_byte_offset,
      token_byte_length,
      token_line_offset,
      token_line_count,
      ..
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

      slots.assign_to_garbage(0, (V::default(), tok, peek));
    }
    _ => slots.assign_to_garbage(0, (V::default(), TokenRange::default(), TokenRange::default())),
  }
}

pub unsafe fn llvm_map_result_action<
  'a,
  T: LLVMByteReader + ByteReader + MutByteReader,
  Node: AstSlot,
>(
  ctx: &mut LLVMParseContext<T>,
  action: &ParseAction,
  slots: &mut AstSlots<(Node, TokenRange, TokenRange)>,
) -> ParseResult<Node> {
  match *action {
    ParseAction::Accept { .. } => {
      ParseResult::Complete(slots.take(0))
    }
    ParseAction::EndOfInput { .. } => {
      ParseResult::NeedMoreInput(slots.to_vec())
    }
    ParseAction::Error {last_input, .. } => {
      let mut last_token = Token::from_parse_token(&last_input);
      last_token.set_source(ctx.get_source());
      let vec = slots.to_vec();
      ParseResult::Error(last_token, vec)
    }
    _ => unreachable!("This function should only be called when the parse action is  [Error, Accept, or EndOfInput]"),
  }
}

#[no_mangle]
pub extern "C" fn sherpa_free_stack(ptr: *mut Goto, byte_size: usize) {
  #[cfg(debug_assertions)]
  {
    println!("Freeing {} bytes for {} slots at address {:p}", byte_size, byte_size >> 4, ptr);
  }
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.
  let layout = Layout::from_size_align(byte_size, 16).unwrap();

  unsafe { dealloc(ptr as *mut u8, layout) }
}

#[no_mangle]
pub extern "C" fn sherpa_get_token_class_from_codepoint(codepoint: u32) -> u32 {
  get_token_class_from_codepoint(codepoint)
}

#[derive(Debug)]
#[repr(C, u64)]
pub enum ParseResult<Node> {
  Complete((Node, TokenRange, TokenRange)),
  Error(Token, Vec<(Node, TokenRange, TokenRange)>),
  NeedMoreInput(Vec<(Node, TokenRange, TokenRange)>),
}
