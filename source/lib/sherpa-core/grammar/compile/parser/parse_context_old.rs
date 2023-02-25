use sherpa_runtime::{
  llvm_parser::{InputInfo, LLVMByteReader},
  types::{
    ast::{AstObject, AstSlot, AstStackSlice},
    bytecode::{FAIL_STATE_FLAG, NORMAL_STATE_FLAG},
    ByteReader,
    MutByteReader,
    ParseAction,
    ParseActionType,
    ParseResult,
    TokenRange,
    UTF8Reader,
    UTF8StringReader,
  },
};
use std::fmt::Debug;

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

type GetBlockFunction<T> = extern "C" fn(&mut T, u32, u32) -> InputInfo;

pub const LLVM_BASE_STACK_SIZE: usize = 8;

#[repr(C)]
pub struct ParseContextOld<T: ByteReader, M = u32> {
  // Input data ----------
  pub token_ptr:       *mut u8,
  pub peek_ptr:        *mut u8,
  pub scan_ptr:        *mut u8,
  pub scan_checkpoint: u32,
  pub peek_len:        u32,
  pub scan_input_len:  u32,
  pub tok_input_trun:  bool,
  pub peek_input_trun: bool,
  pub scan_input_trun: bool,
  // Miscellaneous
  pub in_peek_mode:    bool,
  // Offset info ----------
  /// The start of the portion of characters currently being recognized
  pub anchor_off:      u32,
  /// Maintains the start position of a token. The difference between this and the anchor
  /// offset determines the number characters that have been skipped.
  pub token_off:       u32,
  /// Represents the most advanced offset of  peeked characters
  pub peek_off:        u32,
  /// Maintains the reference to then end of a recognized tokens when in a scan context
  pub scan_anchor_off: u32,
  /// Represents the most advanced portion of scanned characters
  pub scan_head:       u32,
  /// Represents the byte length of the currently recognized symbol
  pub scan_len:        u32,
  /// Set to the value of a production when a rule is reduced, or
  pub prod_id:         u32,
  /// Set to the value of a token when one is recognized. Also stores the number
  /// of symbols that are to be reduced.
  pub tok_id:          u32,
  // Line info ------------
  /// The offset of the last line character recognized that proceeds the anchor offset
  pub end_line_off:    u32,
  /// The number of line character recognized that proceed the anchor offset
  pub end_line_num:    u32,
  /// The offset of the last line character recognized that proceeds the token offset
  pub start_line_off:  u32,
  /// The number of line character recognized that proceed the token offset
  pub start_line_num:  u32,
  /// The offset of the last line character recognized that proceeds the peek offset
  pub scan_line_off:   u32,
  /// The number of line character recognized that proceed the peek offset
  pub scan_line_num:   u32,
  // Goto stack data -----
  pub goto_stack_ptr:  *mut Goto,
  pub goto_size:       u32,
  pub goto_free:       u32,
  // Input data ----------
  pub get_input_info:  GetBlockFunction<T>,
  // Reader --------------
  pub reader:          *mut T,
  // User context --------
  pub meta_ctx:        *mut M,
  pub custom_lex:      fn(&mut T, &mut M, &ParseContextOld<T, M>) -> (u32, u32, u32),
  /// Tracks whether the context is a fail mode or not.
  pub state:           u32,
  /// When reducing, stores the the number of of symbols to reduce into one.
  pub meta_a:          u32,
  /// When reducing, stores the rule id that is being reduced.
  pub meta_b:          u32,
  pub is_active:       bool,
}

#[test]
fn llvm_context_is_160_bytes() {
  assert_eq!(std::mem::size_of::<ParseContextOld<UTF8StringReader, u64>>(), 160)
}

impl<T: LLVMByteReader + ByteReader, M> Debug for ParseContextOld<T, M> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut dbgstr = f.debug_struct("ParseContext");
    dbgstr.field("token_ptr", &self.token_ptr);
    dbgstr.field("peek_ptr", &self.peek_ptr);
    dbgstr.field("scan_ptr", &self.scan_ptr);
    dbgstr.field("tok_input_len", &self.scan_checkpoint);
    dbgstr.field("peek_input_len", &self.peek_len);
    dbgstr.field("scan_input_len", &self.scan_input_len);
    dbgstr.field("anchor_off", &self.anchor_off);
    dbgstr.field("token_off", &self.token_off);
    dbgstr.field("peek_off", &self.peek_off);
    dbgstr.field("scan_anchor_off", &self.scan_anchor_off);
    dbgstr.field("scan_off", &self.scan_head);
    dbgstr.field("scan_len", &self.scan_len);
    dbgstr.field("tok_or_prod_id", &self.prod_id);
    dbgstr.field("anchor_line_off", &self.end_line_off);
    dbgstr.field("anchor_line_num", &self.end_line_num);
    dbgstr.field("tok_line_off", &self.start_line_off);
    dbgstr.field("tok_line_num", &self.start_line_num);
    dbgstr.field("peek_line_off", &self.scan_line_off);
    dbgstr.field("peek_line_num", &self.scan_line_num);
    dbgstr.field("state", &self.state);
    dbgstr.field("in_peek_mode", &self.in_peek_mode);
    dbgstr.field("is_active", &self.is_active);
    dbgstr.field("goto_stack_ptr", &self.goto_stack_ptr);
    dbgstr.field("goto_size", &self.goto_size);
    dbgstr.field("goto_used", &self.goto_free);
    dbgstr.field("get_input_info", &"FN Pointer".to_string());
    dbgstr.field("reader", &((self.reader) as usize).to_string());
    dbgstr.field("meta_ctx", &self.meta_ctx);
    dbgstr.field("custom_lex", &"FN Pointer".to_string());
    dbgstr.finish()
  }
}

impl<T: ByteReader + LLVMByteReader, M> ParseContextOld<T, M> {
  pub fn new_llvm() -> Self {
    Self {
      token_ptr:       0 as *mut u8,
      peek_ptr:        0 as *mut u8,
      scan_ptr:        0 as *mut u8,
      peek_input_trun: true,
      scan_input_trun: true,
      tok_input_trun:  true,
      scan_checkpoint: 0,
      peek_len:        0,
      scan_input_len:  0,
      anchor_off:      0,
      token_off:       0,
      peek_off:        0,
      scan_anchor_off: 0,
      scan_head:       0,
      scan_len:        0,
      prod_id:         0,
      end_line_off:    0,
      end_line_num:    0,
      start_line_off:  0,
      start_line_num:  0,
      scan_line_off:   0,
      scan_line_num:   0,
      state:           0,
      tok_id:          0,
      meta_a:          0,
      meta_b:          0,
      goto_size:       0,
      goto_free:       0,
      in_peek_mode:    false,
      is_active:       false,
      goto_stack_ptr:  0 as *mut Goto,
      get_input_info:  T::get_byte_block_at_cursor_old,
      reader:          0 as *mut T,
      meta_ctx:        0 as *mut M,
      custom_lex:      Self::default_custom_lex,
    }
  }
}

impl<T: ByteReader, M> ParseContextOld<T, M> {
  pub fn new(reader: &mut T) -> Self {
    Self {
      token_ptr:       0 as *mut u8,
      peek_ptr:        0 as *mut u8,
      scan_ptr:        0 as *mut u8,
      peek_input_trun: true,
      scan_input_trun: true,
      tok_input_trun:  true,
      scan_checkpoint: 0,
      peek_len:        0,
      scan_input_len:  0,
      anchor_off:      0,
      token_off:       0,
      peek_off:        0,
      scan_anchor_off: 0,
      scan_head:       0,
      scan_len:        0,
      prod_id:         0,
      end_line_off:    0,
      end_line_num:    0,
      start_line_off:  0,
      start_line_num:  0,
      scan_line_off:   0,
      scan_line_num:   0,
      state:           0,
      tok_id:          0,
      meta_a:          0,
      meta_b:          0,
      goto_size:       0,
      goto_free:       0,
      in_peek_mode:    false,
      is_active:       false,
      goto_stack_ptr:  0 as *mut Goto,
      get_input_info:  Self::default_get_input_info,
      reader:          reader,
      meta_ctx:        0 as *mut M,
      custom_lex:      Self::default_custom_lex,
    }
  }

  extern "C" fn default_get_input_info(_: &mut T, _: u32, _: u32) -> InputInfo {
    InputInfo(0 as *const u8, 0, false)
  }

  fn default_custom_lex(_: &mut T, _: &mut M, _: &Self) -> (u32, u32, u32) {
    (0, 0, 0)
  }

  pub fn get_shift_data(&self) -> ParseAction {
    ParseAction::Shift {
      anchor_byte_offset: self.anchor_off,
      token_byte_offset:  self.token_off,
      token_byte_length:  self.scan_len,
      token_line_offset:  self.start_line_off,
      token_line_count:   self.start_line_num,
    }
  }

  /// The following methods are used exclusively by the
  /// the rust functions in [sherpa::runtime::parser_functions.rs]

  #[inline]
  pub fn in_fail_mode(&self) -> bool {
    self.state == FAIL_STATE_FLAG
  }

  #[inline]
  pub fn set_fail_mode_to(&mut self, is_in_fail_mode: bool) {
    self.state = if is_in_fail_mode { FAIL_STATE_FLAG } else { NORMAL_STATE_FLAG }
  }

  #[inline]
  pub fn in_peek_mode(&self) -> bool {
    self.in_peek_mode
  }

  #[inline]
  pub fn set_peek_mode_to(&mut self, is_in_peek_mode: bool) {
    self.in_peek_mode = is_in_peek_mode;
  }

  #[inline]
  pub fn get_production(&self) -> u32 {
    self.prod_id
  }

  #[inline]
  pub fn set_production_to(&mut self, production: u32) {
    self.prod_id = production;
  }

  #[inline]
  pub fn is_scanner(&self) -> bool {
    self.meta_a > 0
  }

  pub fn set_is_scanner(&mut self, is_scanner: bool) {
    self.meta_a = is_scanner as u32;
  }
}

impl<T: MutByteReader + ByteReader, M> ParseContextOld<T, M> {
  pub fn get_reader_mut(&mut self) -> &mut T {
    unsafe { (&mut *self.reader) as &mut T }
  }
}

impl<T: ByteReader, M> ParseContextOld<T, M> {
  pub fn get_reader(&self) -> &T {
    unsafe { (&*self.reader) as &T }
  }
}

impl<T: ByteReader + UTF8Reader, M> ParseContextOld<T, M> {
  pub fn get_str(&self) -> &str {
    unsafe { (*self.reader).get_str() }
  }
}

pub type Reducer<R, M, Node> = fn(&ParseContextOld<R, M>, &AstStackSlice<AstSlot<Node>>);

pub unsafe fn llvm_map_shift_action_old<
  'a,
  R: LLVMByteReader + ByteReader + MutByteReader,
  ExtCTX,
  ASTNode: AstObject,
>(
  ctx: &ParseContextOld<R, ExtCTX>,
  slots: &mut AstStackSlice<(ASTNode, TokenRange, TokenRange)>,
) {
  match ctx.get_shift_data() {
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

      slots.assign_to_garbage(0, (ASTNode::default(), tok, peek));
    }
    _ => unreachable!(),
  }
}

pub unsafe fn llvm_map_result_action_old<
  'a,
  T: LLVMByteReader + ByteReader + MutByteReader,
  M,
  Node: AstObject,
>(
  _ctx: &ParseContextOld<T, M>,
  action: ParseActionType,
  slots: &mut AstStackSlice<(Node, TokenRange, TokenRange)>,
) -> ParseResult<Node> {
  match action {
    ParseActionType::Accept =>{
      ParseResult::Complete(slots.take(0))
    }
    ParseActionType::Error => {
      let vec = slots.to_vec();
      let last_input = vec.last().cloned().unwrap_or_default().1;
      ParseResult::Error(last_input, vec)
    }
    ParseActionType::NeedMoreInput => {
      ParseResult::NeedMoreInput(slots.to_vec())
    }

    _ => unreachable!("This function should only be called when the parse action is  [Error, Accept, or EndOfInput]"),
  }
}
