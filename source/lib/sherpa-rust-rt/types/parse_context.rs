use super::{
  ast::{AstObject, AstStackSlice},
  *,
};
use crate::{
  bytecode_parser::{dispatch, DebugEvent, DebugFn},
  utf8::get_token_class_from_codepoint,
};
use std::{
  alloc::{alloc, dealloc, Layout},
  fmt::Debug,
  sync::Arc,
};

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

type GetBlockFunction<T> = extern "C" fn(
  self_: &mut T,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
);

#[repr(C)]
pub struct ParseContext<T: ByteReader, M = u32> {
  // Input data ----------
  /// The head of the input block
  pub begin_ptr: usize,
  /// The the end of the last shifted token
  pub anchor_ptr: usize,
  /// The the start of the evaluated token, which may be
  /// the same as base_ptr unless we are using peek shifts.
  pub base_ptr: usize,
  /// The the start of the evaluated token, which may be
  /// the same as base_ptr unless we are using peek shifts.
  pub head_ptr: usize,
  /// The start of all unevaluated characters
  pub tail_ptr: usize,
  /// The end of the input block
  pub end_ptr: usize,
  /// The number of characters that comprize the current
  /// token. This should be 0 if the tok_id is also
  pub tok_len: usize,
  /// The number of characters that can be read
  /// from the input block.
  pub chars_remaining_len: usize,
  // Goto stack data -----
  pub goto_stack_ptr: *mut Goto,
  pub goto_size: u32,
  pub goto_free: u32,
  // Parse objects ----------------
  pub get_input_info: GetBlockFunction<T>,
  pub reader: *mut T,
  // User context --------
  pub meta_ctx: *mut M,
  pub custom_lex: fn(&mut T, &mut M, &ParseContext<T, M>) -> (u32, u32, u32),
  // Line info ------------
  /// The offset of the last line character recognized that proceeds the anchor
  pub start_line_off: u32,
  /// The offset of the last line character recognized that proceeds the chkp
  pub chkp_line_off: u32,
  /// The offset of the last line character recognized that proceeds the tail
  pub end_line_off: u32,
  /// The number of line character recognized that proceed the anchor
  pub start_line_num: u32,
  /// The number of line character recognized that proceed the chkp
  pub chkp_line_num: u32,
  /// The number of line character recognized that proceed the tail
  pub end_line_num: u32,
  // Parser State ----------
  /// When reducing, stores the the number of of symbols to reduce.
  pub sym_len: u32,
  /// Tracks whether the context is a fail mode or not.
  pub state: u32,
  /// Set to the value of a production when a rule is reduced, or
  pub prod_id: u32,
  /// Set to the value of a token when one is recognized. Also stores the number
  /// of symbols that are to be reduced.
  pub tok_id: u32,
  /// When reducing, stores the rule id that is being reduced.
  pub rule_id: u32,
  pub line_incr: u8,
  pub is_active: bool,
  // Miscellaneous
  pub in_peek_mode: bool,
}

impl<T: ByteReader, M> ParseContext<T, M> {
  pub fn reset(&mut self) {
    self.anchor_ptr = 0;
    self.tail_ptr = 0;
    self.tok_len = 0;
    self.head_ptr = 0;
    self.base_ptr = 0;
    self.end_ptr = 0;
    self.begin_ptr = 0;
    self.chars_remaining_len = 0;
    self.goto_size = 0;
    self.goto_free = 0;
    self.start_line_num = 0;
    self.chkp_line_num = 0;
    self.end_line_num = 0;
    self.state = 0;
    self.is_active = true;
    self.in_peek_mode = false;
    self.goto_stack_ptr = 0 as usize as *mut Goto;
  }

  pub fn set_meta(&mut self, meta: *mut M) {
    self.meta_ctx = meta;
  }

  pub unsafe fn get_meta_mut(&mut self) -> &mut M {
    &mut *self.meta_ctx
  }

  pub unsafe fn get_meta(&self) -> &M {
    &*self.meta_ctx
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
    self.rule_id > 0
  }

  pub fn set_is_scanner(&mut self, is_scanner: bool) {
    self.rule_id = is_scanner as u32;
  }

  pub fn get_curr_line_num(&self) -> u32 {
    self.start_line_num
  }

  pub fn get_curr_line_offset(&self) -> u32 {
    self.start_line_off
  }

  pub fn get_anchor_offset(&self) -> u32 {
    (self.anchor_ptr - self.begin_ptr) as u32
  }

  pub fn get_token_length(&self) -> u32 {
    (self.tok_len) as u32
  }

  pub fn get_token_offset(&self) -> u32 {
    println!("{} {}", self.begin_ptr, self.head_ptr);
    (self.head_ptr - self.begin_ptr) as u32
  }

  pub fn get_token_line_number(&self) -> u32 {
    self.start_line_num
  }

  pub fn get_token_line_offset(&self) -> u32 {
    self.start_line_off
  }

  pub fn get_production_id(&self) -> u32 {
    self.prod_id
  }

  /// Returns shift data from current context state.
  pub fn get_shift_data(&self) -> ParseAction {
    ParseAction::Shift {
      anchor_byte_offset: self.get_anchor_offset(),
      token_byte_offset:  self.get_token_offset(),
      token_byte_length:  self.get_token_length(),
      token_line_offset:  self.get_curr_line_offset(),
      token_line_count:   self.get_curr_line_num(),
    }
  }
}

impl<T: ByteReader, M> Default for ParseContext<T, M> {
  fn default() -> Self {
    Self {
      anchor_ptr: 0,
      tail_ptr: 0,
      tok_len: 0,
      head_ptr: 0,
      chars_remaining_len: 0,
      base_ptr: 0,
      end_ptr: 0,
      prod_id: 0,
      begin_ptr: 0,
      end_line_num: 0,
      start_line_num: 0,
      chkp_line_num: 0,
      chkp_line_off: 0,
      end_line_off: 0,
      start_line_off: 0,
      state: 0,
      tok_id: 0,
      sym_len: 0,
      rule_id: 0,
      goto_size: 0,
      goto_free: 0,
      line_incr: 0,
      in_peek_mode: false,
      is_active: false,
      goto_stack_ptr: 0 as *mut Goto,
      meta_ctx: 0 as *mut M,
      custom_lex: Self::default_custom_lex,
      get_input_info: Self::default_get_input_info,
      reader: 0 as *mut T,
    }
  }
}

impl<T: ByteReader + LLVMByteReader, M> Debug for ParseContext<T, M> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut dbgstr = f.debug_struct("ParseContext");
    dbgstr.field("anchor_ptr", &self.anchor_ptr);
    dbgstr.field("tail_ptr", &self.tail_ptr);
    dbgstr.field("tok_len", &self.tok_len);
    dbgstr.field("head_ptr", &self.head_ptr);
    dbgstr.field("input_block_len", &self.chars_remaining_len);
    dbgstr.field("base_ptr", &self.base_ptr);
    dbgstr.field("end_ptr", &self.end_ptr);
    dbgstr.field("prod_id", &self.prod_id);
    dbgstr.field("begin_ptr", &self.begin_ptr);
    dbgstr.field("end_line_num", &self.end_line_num);
    dbgstr.field("start_line_num", &self.start_line_num);
    dbgstr.field("chkp_line_num", &self.chkp_line_num);
    dbgstr.field("chkp_line_off", &self.chkp_line_off);
    dbgstr.field("end_line_off", &self.end_line_off);
    dbgstr.field("start_line_off", &self.start_line_off);
    dbgstr.field("state", &self.state);
    dbgstr.field("tok_id", &self.tok_id);
    dbgstr.field("sym_len", &self.sym_len);
    dbgstr.field("rule_id", &self.rule_id);
    dbgstr.field("goto_size", &self.goto_size);
    dbgstr.field("goto_free", &self.goto_free);
    dbgstr.field("line_incr", &self.line_incr);
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

impl<T: ByteReader + LLVMByteReader, M> ParseContext<T, M> {
  pub fn new_llvm() -> Self {
    Self {
      get_input_info: T::get_byte_block_at_cursor,
      custom_lex: Self::default_custom_lex,
      ..Default::default()
    }
  }
}

impl<T: ByteReader, M> ParseContext<T, M> {
  pub fn new_bytecode(reader: &mut T) -> Self {
    Self {
      custom_lex: Self::default_custom_lex,
      get_input_info: Self::default_get_input_info,
      reader: reader,
      ..Default::default()
    }
  }

  extern "C" fn default_get_input_info(
    _: &mut T,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
  ) {
  }

  fn default_custom_lex(_: &mut T, _: &mut M, _: &Self) -> (u32, u32, u32) {
    (0, 0, 0)
  }
}

impl<T: MutByteReader + ByteReader, M> ParseContext<T, M> {
  pub fn get_reader_mut(&mut self) -> &mut T {
    unsafe { (&mut *self.reader) as &mut T }
  }
}

impl<T: ByteReader, M> ParseContext<T, M> {
  pub fn get_reader(&self) -> &T {
    unsafe { (&*self.reader) as &T }
  }
}

impl<T: ByteReader + UTF8Reader, M> ParseContext<T, M> {
  pub fn get_str(&self) -> &str {
    unsafe { (*self.reader).get_str() }
  }
}

pub unsafe fn llvm_map_shift_action_2<
  'a,
  R: LLVMByteReader + ByteReader + MutByteReader,
  ExtCTX,
  ASTNode: AstObject,
>(
  ctx: &ParseContext<R, ExtCTX>,
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

pub unsafe fn llvm_map_result_action_2<
  'a,
  T: LLVMByteReader + ByteReader + MutByteReader,
  M,
  Node: AstObject,
>(
  _ctx: &ParseContext<T, M>,
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

#[derive(Debug)]
#[repr(C, u64)]
pub enum ParseResult<Node> {
  Complete((Node, TokenRange, TokenRange)),
  Error(TokenRange, Vec<(Node, TokenRange, TokenRange)>),
  NeedMoreInput(Vec<(Node, TokenRange, TokenRange)>),
}

pub enum ShiftsAndSkipsResult {
  Accepted {
    shifts: Vec<String>,
    skips:  Vec<String>,
  },

  IncorrectProduction {
    shifts: Vec<String>,
    skips: Vec<String>,
    expected_prod_id: u32,
    actual_prod_id: u32,
  },

  FailedParse(SherpaParseError),
}

pub trait SherpaParser<R: ByteReader + MutByteReader, M> {
  /// Returns the byte length of activee token
  fn get_token_length(&self) -> u32;

  /// Returns the byte offset the head of the activee token
  fn get_token_offset(&self) -> u32;

  /// Returns the 0 indexed line number active token
  fn get_token_line_number(&self) -> u32;

  /// Returns the offset the newline character preoceeding
  /// the active token
  fn get_token_line_offset(&self) -> u32;

  /// Returns the production id of the most recently reduced symbols
  fn get_production_id(&self) -> u32;

  /// Parse input up to the next required parse action and return
  /// its value.
  fn get_next_action(&mut self, debug: &mut Option<DebugFn>) -> ParseAction;

  /// Returns a reference to the internal Reader
  fn get_reader(&self) -> &R;

  /// Returns a reference to the input string
  fn get_input(&self) -> &str;

  fn init_parser(&mut self, entry_point: u32);

  fn collect_shifts_and_skips(
    &mut self,
    entry_point: u32,
    target_production_id: u32,
    debug: &mut Option<DebugFn>,
  ) -> ShiftsAndSkipsResult {
    self.init_parser(entry_point);

    let mut shifts = vec![];
    let mut skips = vec![];
    loop {
      match self.get_next_action(debug) {
        ParseAction::Accept { production_id } => {
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::Complete { production_id });
          }
          break if production_id != target_production_id {
            ShiftsAndSkipsResult::IncorrectProduction {
              shifts,
              skips,
              expected_prod_id: target_production_id,
              actual_prod_id: production_id,
            }
          } else {
            ShiftsAndSkipsResult::Accepted { shifts, skips }
          };
        }
        ParseAction::Error { last_input, .. } => {
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::Failure {});
          }
          let mut token: Token = last_input.to_token(self.get_reader());

          token.set_source(Arc::new(Vec::from(self.get_input().to_string().as_bytes())));
          break ShiftsAndSkipsResult::FailedParse(SherpaParseError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_production: self.get_production_id(),
          });
        }
        ParseAction::Fork { .. } => {
          panic!("No implementation of fork resolution is available")
        }
        ParseAction::Shift { anchor_byte_offset, token_byte_length, token_byte_offset, .. } => {
          if (token_byte_offset - anchor_byte_offset) > 0 {
            skips.push(
              self.get_input()[anchor_byte_offset as usize..(token_byte_offset) as usize]
                .to_string(),
            );
            #[cfg(debug_assertions)]
            if let Some(debug) = debug {
              debug(&DebugEvent::SkipToken {
                offset_start: anchor_byte_offset as usize,
                offset_end:   token_byte_offset as usize,
                string:       self.get_input(),
              });
            }
          }
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;

          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ShiftToken { offset_start, offset_end, string: self.get_input() });
          }
          shifts.push(self.get_input()[offset_start..offset_end].to_string());
        }
        ParseAction::Reduce { rule_id, .. } =>
        {
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::Reduce { rule_id });
          }
        }
        _ => panic!("Unexpected Action!"),
      }
    }
  }
}

pub struct ByteCodeParser<'a, R: ByteReader + MutByteReader, M> {
  ctx:   ParseContext<R, M>,
  stack: Vec<u32>,
  bc:    &'a [u32],
}

impl<'a, R: ByteReader + MutByteReader, M> ByteCodeParser<'a, R, M> {
  pub fn new(reader: &'a mut R, bc: &'a [u32]) -> Self {
    ByteCodeParser { ctx: ParseContext::<R, M>::new_bytecode(reader), stack: vec![], bc }
  }
}

impl<'a, R: ByteReader + MutByteReader + UTF8Reader, M> SherpaParser<R, M>
  for ByteCodeParser<'a, R, M>
{
  fn get_token_length(&self) -> u32 {
    self.ctx.get_token_length()
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

  fn get_input(&self) -> &str {
    unsafe { std::str::from_utf8_unchecked(self.get_reader().get_bytes()) }
  }

  fn init_parser(&mut self, entry_point: u32) {
    self.stack = vec![0, entry_point | NORMAL_STATE_FLAG];
  }

  fn get_next_action(&mut self, debug: &mut Option<DebugFn>) -> ParseAction {
    let ByteCodeParser { ctx, stack, bc } = self;

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
              len:      (ctx.tail_ptr - ctx.head_ptr) as u32,
              off:      off,
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
}

#[no_mangle]
pub extern "C" fn sherpa_free_stack(ptr: *mut Goto, byte_size: usize) {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.
  let layout = Layout::from_size_align(byte_size, 16).unwrap();

  unsafe { dealloc(ptr as *mut u8, layout) }
}

#[no_mangle]
pub extern "C" fn sherpa_get_token_class_from_codepoint(codepoint: u32) -> u32 {
  get_token_class_from_codepoint(codepoint)
}

#[no_mangle]
pub extern "C" fn sherpa_allocate_stack(byte_size: usize) -> *mut Goto {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the bytes size of
  // the stack.

  let layout = Layout::from_size_align(byte_size, 16).unwrap();

  unsafe {
    let ptr = alloc(layout) as *mut Goto;

    ptr
  }
}
