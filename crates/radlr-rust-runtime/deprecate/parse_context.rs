#[cfg(debug_assertions)]
use std::fmt::Debug;
use std::sync::Arc;

use crate::types::{
  bytecode::{FAIL_STATE_FLAG, NORMAL_STATE_FLAG},
  ParseAction,
  Token,
  TokenRange,
};

use super::{
  ast::{AstObject, AstSlot, AstStackSlice, Reducer},
  ByteReader,
  DebugEvent,
  DebugFn,
  MutByteReader,
  RadlrParseError,
  UTF8Reader,
};

#[derive(Clone, Copy, Debug)]
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

/* enum Symbol {
  Skip(Token),
  Terminal(Token),
  Nonterminal(u32, Box<Symbol>, Box<Vec<Symbol>>),
  Fork(Box<ParseStack>),
}

pub struct ParseStack {
  states:           Vec<u32>,
  symbols:          Vec<Symbol>,
  nonterminal_goal: u32,
  error_weight:     u32,
  prefix_sibling:   Option<Box<ParseStack>>,
  /// The head of the input window
  pub begin_ptr:    usize,
  pub anchor_ptr:   usize,
  /// The the end of the last shifted token
  pub base_ptr:     usize,
  /// The the start of the token currently being evaluated.
  pub sym_ptr:      usize,
  /// The the start of the token currently being evaluated.
  pub sym_pk_ptr:   usize,
  /// The start of all unevaluated characters
  pub tok_ptr:      usize,
  /// The start of all unevaluated characters
  pub tok_pk_ptr:   usize,
  /// The end of the input window. This is a fixed reference that should
  /// not change during parsing unless the end of the input window has been
  /// reached and a larger window is requested.
  pub end_ptr:      usize,
  /// The number of characters that comprize the current
  /// token. This should be 0 if the tok_id is also 0
  pub tok_len:      usize,
}

pub struct ParseStacks {
  stacks: Box<ParseStack>,
} */

/// This function should set up a new input window,
/// Respecting the the relative offsets of the parsing pointers.
/// All pointer offsets should be relative to the anchor pointer
/// which should at least point into some valid input data.
///
/// If the tail pointer is positioned at the end of the input stream
/// then this should return true, false otherwise.
///
/// If there is not enough input to fulfill the request, but there
/// will be after some external event occurs, then the tail pointer
/// should be set to usize::Max and `false` returned.
type GetBlockFunction<T> = extern "C" fn(
  self_: *mut T,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
  &mut *const u8,
) -> bool;

#[repr(C)]
pub struct ParseContext<T: ByteReader, M = u32> {
  // Input data ----------
  /// The head of the input window
  pub begin_ptr:      usize,
  ///
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
  // Goto stack data -----
  pub goto_stack_ptr: *mut Goto,
  pub goto_size:      u32,
  pub goto_free:      u32,
  // Parse objects ----------------
  pub get_input_info: GetBlockFunction<T>,
  pub reader:         *mut T,
  // User context --------
  pub meta_ctx:       *mut M,
  pub custom_lex:     fn(&mut T, &mut M, &ParseContext<T, M>) -> (u32, u32, u32),
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
  pub nterm:          u32,
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
}

impl<T: ByteReader, M> ParseContext<T, M> {
  pub fn reset(&mut self) {
    self.anchor_ptr = 0;
    self.tok_ptr = 0;
    self.tok_len = 0;
    self.sym_ptr = 0;
    self.base_ptr = 0;
    self.end_ptr = 0;
    self.begin_ptr = 0;
    self.goto_size = 0;
    self.goto_free = 0;
    self.start_line_num = 0;
    self.chkp_line_num = 0;
    self.end_line_num = 0;
    self.state = 0;
    self.is_active = true;
    self.in_peek_mode = false;
    self.goto_stack_ptr = 0 as usize as *mut Goto;
    self.block_is_eoi = false;
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
  /// the rust functions in [radlr::runtime::parser_functions.rs]

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
  pub fn get_nonterminal(&self) -> u32 {
    self.nterm
  }

  #[inline]
  pub fn set_nonterminal_to(&mut self, non_terminal: u32) {
    self.nterm = non_terminal;
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
    (self.sym_ptr - self.begin_ptr) as u32
  }

  pub fn get_token_line_number(&self) -> u32 {
    self.start_line_num
  }

  pub fn get_token_line_offset(&self) -> u32 {
    self.start_line_off
  }

  pub fn get_nonterminal_id(&self) -> u32 {
    self.nterm
  }

  /// Returns shift data from current context state.
  #[inline(always)]
  pub fn get_shift_data(&self) -> ParseAction {
    ParseAction::Shift {
      byte_offset:              self.get_token_offset(),
      byte_length:              self.get_token_length(),
      token_line_offset:        self.get_curr_line_offset(),
      token_line_count:         self.get_curr_line_num(),
      token_id:                 self.tok_id,
      emitting_state:           Default::default(),
      next_instruction_address: Default::default(),
      is_recovery:              false,
    }
  }

  /// Returns shift data from current context state.
  #[inline(always)]
  pub fn get_peek_data(&self) -> ParseAction {
    ParseAction::Skip {
      byte_offset:       self.get_token_offset(),
      byte_length:       self.get_token_length(),
      token_line_offset: self.get_curr_line_offset(),
      token_line_count:  self.get_curr_line_num(),
      token_id:          self.tok_id,
    }
  }
}

impl<T: ByteReader, M> Default for ParseContext<T, M> {
  fn default() -> Self {
    Self {
      anchor_ptr:     0,
      tok_ptr:        0,
      tok_len:        0,
      sym_ptr:        0,
      base_ptr:       0,
      end_ptr:        0,
      sym_pk_ptr:     0,
      tok_pk_ptr:     0,
      nterm:          0,
      begin_ptr:      0,
      end_line_num:   0,
      start_line_num: 0,
      chkp_line_num:  0,
      chkp_line_off:  0,
      end_line_off:   0,
      start_line_off: 0,
      state:          0,
      tok_id:         0,
      sym_len:        0,
      rule_id:        0,
      goto_size:      0,
      goto_free:      0,
      line_incr:      0,
      in_peek_mode:   false,
      is_active:      false,
      goto_stack_ptr: 0 as *mut Goto,
      meta_ctx:       0 as *mut M,
      custom_lex:     Self::default_custom_lex,
      get_input_info: Self::default_get_input_info,
      reader:         0 as *mut T,
      block_is_eoi:   false,
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
    _: *mut T,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
    _: &mut *const u8,
  ) -> bool {
    false
  }

  pub(crate) fn default_custom_lex(_: &mut T, _: &mut M, _: &Self) -> (u32, u32, u32) {
    (0, 0, 0)
  }
}

impl<T: MutByteReader + ByteReader, M> ParseContext<T, M> {
  #[inline(always)]
  pub fn get_reader_mut(&mut self) -> &mut T {
    unsafe { (&mut *self.reader) as &mut T }
  }
}

impl<T: ByteReader, M> ParseContext<T, M> {
  #[inline(always)]
  pub fn get_reader(&self) -> &T {
    unsafe { (&*self.reader) as &T }
  }
}

impl<T: ByteReader + UTF8Reader, M> ParseContext<T, M> {
  #[inline(always)]
  pub fn get_str(&self) -> &str {
    unsafe { (*self.reader).get_str() }
  }
}

#[derive(Debug)]
#[repr(C, u64)]
pub enum ParseResult<Node: AstObject> {
  Complete(AstSlot<Node>),
  Error(TokenRange, Vec<AstSlot<Node>>),
  NeedMoreInput(Vec<AstSlot<Node>>),
}

#[derive(Debug)]
pub enum ShiftsAndSkipsResult {
  Accepted { shifts: Vec<String>, skips: Vec<String> },

  IncorrectNonTerminal { shifts: Vec<String>, skips: Vec<String>, expected_nterm: u32, actual_nterm: u32 },

  FailedParse(RadlrParseError),
}

pub trait RadlrParser<R: ByteReader + MutByteReader, M, const UPWARD_STACK: bool> {
  /// Returns true of the `head_ptr` is positioned at the end of the input.
  ///
  /// That is `head_ptr - beg_ptr == input.len()`
  fn head_at_end(&self) -> bool;

  /// Returns the byte length of active token
  fn get_token_length(&self) -> u32;

  /// Returns the id of the active token
  fn get_token_id(&self) -> u32;

  /// Returns the byte offset of the head of the active token
  fn get_token_offset(&self) -> u32;

  /// Returns the 0 indexed line number active token
  fn get_token_line_number(&self) -> u32;

  /// Returns the offset the newline character proceeding
  /// the active token
  fn get_token_line_offset(&self) -> u32;

  /// Returns the non-terminal id of the most recently reduced symbols
  fn get_nonterminal_id(&self) -> u32;

  /// Parse input up to the next required parse action and return
  /// its value.
  fn get_next_action<'debug>(&mut self, debug: &mut Option<&'debug mut DebugFn<R, M>>) -> ParseAction;

  /// Returns a reference to the internal Reader
  fn get_reader(&self) -> &R;

  /// Returns a mutable reference to the internal Reader
  fn get_reader_mut(&mut self) -> &mut R;

  /// Returns a reference to the input string
  fn get_input(&self) -> &str;

  fn init_parser(&mut self, entry_point: u32);

  // Returns a reference to the ParseContext
  fn get_ctx(&self) -> &ParseContext<R, M>;

  // Returns a mutable reference to the ParseContext
  fn get_ctx_mut(&mut self) -> &mut ParseContext<R, M>;

  fn parse_cst() {}

  // Givin a shift and reduce function, compile an AST struct
  fn parse_ast<'a, 'debug, Node: AstObject>(
    &mut self,
    reducers: &[Reducer<R, M, Node, UPWARD_STACK>],
    debug: &mut Option<&'debug mut DebugFn<R, M>>,
  ) -> Result<AstSlot<Node>, RadlrParseError> {
    let mut ast_stack: Vec<AstSlot<Node>> = vec![];
    loop {
      match self.get_next_action(debug) {
        ParseAction::Accept { .. } => {
          return Ok(ast_stack.pop().unwrap());
        }
        ParseAction::Reduce { rule_id, symbol_count, .. } => {
          let reduce_fn = reducers[rule_id as usize];
          let len = ast_stack.len();
          let count = symbol_count as usize;
          reduce_fn(self.get_ctx_mut(), &AstStackSlice::from_slice(&mut ast_stack[(len - count)..len]));
          ast_stack.resize(len - (count - 1), AstSlot::<Node>::default());
        }
        ParseAction::Skip { .. } => {}
        ParseAction::Shift {
          byte_offset: token_byte_offset,
          byte_length: token_byte_length,
          token_line_offset,
          token_line_count,
          ..
        } => {
          let tok = TokenRange {
            len:      token_byte_length,
            off:      token_byte_offset,
            line_num: token_line_count,
            line_off: token_line_offset,
          };

          ast_stack.push(AstSlot(Node::default(), tok, Default::default()));
        }
        ParseAction::Error { .. } => {
          let ctx = self.get_ctx();
          let last_input = TokenRange {
            off:      ctx.sym_ptr as u32,
            len:      ctx.sym_len as u32,
            line_num: ctx.start_line_num,
            line_off: ctx.start_line_off,
          };
          let string = self.get_reader().get_bytes();

          let mut start = last_input.off as usize;
          while start < string.len() && string[start] == 32 {
            start += 1;
          }
          let mut end = start;
          while end < string.len() && string[end] != 32 && string[end] != 10 {
            end += 1;
          }

          return Err(RadlrParseError {
            inline_message:   "Unrecognized Token (1)".into(),
            last_nonterminal: 0,
            loc:              TokenRange {
              line_num: last_input.line_num,
              line_off: last_input.line_off,
              len:      (end - start) as u32,
              off:      start as u32,
            }
            .to_token(self.get_reader_mut()),
            message:          "Unrecognized Token (2)".into(),
          });
        }
        _ => {
          return Err(RadlrParseError {
            inline_message:   Default::default(),
            last_nonterminal: 0,
            loc:              Default::default(),
            message:          "Unrecognized Token (3)".into(),
          });
        }
      }
    }
  }

  /// Returns an empty success result if the entire input was successfully
  /// parsed
  fn completes<'debug>(
    &mut self,
    entry_point: u32,
    target_nonterminal_id: u32,
    debug: &mut Option<&'debug mut DebugFn<R, M>>,
  ) -> Result<(), RadlrParseError> {
    self.init_parser(entry_point);
    loop {
      match self.get_next_action(debug) {
        ParseAction::Accept { nonterminal_id, .. } => {
          break if !self.head_at_end() {
            Err(RadlrParseError {
              inline_message:   format!("Failed to read entire input {} {}", self.get_ctx().end_ptr, self.get_ctx().sym_ptr),
              last_nonterminal: nonterminal_id,
              loc:              Default::default(),
              message:          "Failed to read entire input".to_string(),
            })
          } else if nonterminal_id != target_nonterminal_id {
            Err(RadlrParseError {
              inline_message:   "Top symbol did not match the target nonterminal".to_string(),
              last_nonterminal: nonterminal_id,
              loc:              Default::default(),
              message:          "CST is incorrect".to_string(),
            })
          } else {
            Ok(())
          };
        }
        ParseAction::Shift {
          byte_length: token_byte_length,
          byte_offset: token_byte_offset,
          token_id,
          ..
        } => {
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ActionShift { offset_start, offset_end, token_id }, self.get_ctx());
          }
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } =>
        {
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ActionReduce { rule_id: _rule_id }, self.get_ctx());
          }
        }
        ParseAction::Error { .. } => {
          let last_input = TokenRange::default();
          let mut token: Token = last_input.to_token(self.get_reader_mut());
          token.set_source(self.get_input().to_string().as_bytes().into());
          break Err(RadlrParseError {
            message:          "Could not recognize the following input:".to_string(),
            inline_message:   "".to_string(),
            loc:              token,
            last_nonterminal: self.get_nonterminal_id(),
          });
        }
        _ => {}
      }
    }
  }

  ///  Gathers all groups of tokens that have been shifted and skipped during
  /// the parsing of an input.
  fn collect_shifts_and_skips<'debug>(
    &mut self,
    entry_point: u32,
    target_nonterminal_id: u32,
    debug: &mut Option<&'debug mut DebugFn<R, M>>,
  ) -> ShiftsAndSkipsResult {
    self.init_parser(entry_point);

    let mut shifts = vec![];
    let mut skips = vec![];

    loop {
      match self.get_next_action(debug) {
        ParseAction::Accept { nonterminal_id, .. } => {
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ActionAccept {}, self.get_ctx());
          }
          break if nonterminal_id != target_nonterminal_id {
            ShiftsAndSkipsResult::IncorrectNonTerminal {
              shifts,
              skips,
              expected_nterm: target_nonterminal_id,
              actual_nterm: nonterminal_id,
            }
          } else if !self.head_at_end() {
            ShiftsAndSkipsResult::FailedParse(RadlrParseError {
              inline_message:   "Failed to read entire input".to_string(),
              last_nonterminal: nonterminal_id,
              loc:              Default::default(),
              message:          "Failed to read entire input".to_string(),
            })
          } else {
            ShiftsAndSkipsResult::Accepted { shifts, skips }
          };
        }
        ParseAction::Error { .. } => {
          let last_input = TokenRange::default();
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ActionError {}, self.get_ctx());
          }
          let mut token: Token = last_input.to_token(self.get_reader_mut());

          token.set_source(self.get_input().to_string().as_bytes().into());
          break ShiftsAndSkipsResult::FailedParse(RadlrParseError {
            message:          "Could not recognize the following input:".to_string(),
            inline_message:   "".to_string(),
            loc:              token,
            last_nonterminal: self.get_nonterminal_id(),
          });
        }
        ParseAction::Fork { .. } => {
          panic!("No implementation of fork resolution is available")
        }
        ParseAction::Skip { byte_offset: token_byte_offset, byte_length: token_byte_length, .. } => {
          skips.push(self.get_input()[token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize].to_string());
        }
        ParseAction::Shift {
          byte_length: token_byte_length,
          byte_offset: token_byte_offset,
          token_id,
          ..
        } => {
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;

          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ActionShift { offset_start, offset_end, token_id }, self.get_ctx());
          }
          shifts.push(self.get_input()[offset_start..offset_end].to_string());
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } =>
        {
          #[cfg(debug_assertions)]
          if let Some(debug) = debug {
            debug(&DebugEvent::ActionReduce { rule_id: _rule_id }, self.get_ctx());
          }
        }
        _ => panic!("Unexpected Action!"),
      }
    }
  }
}
