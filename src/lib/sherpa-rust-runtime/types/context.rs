use std::cmp::Ordering;

use super::*;

#[repr(C)]
#[derive(Clone, Eq, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
/// Stores a stack of active states and lexer/parser symbol pointers.
pub struct ParserContext {
  // Goto stack data ----------------------------------------------------------
  pub stack:        Vec<ParserState>,
  // Input pointers -----------------------------------------------------------
  /// The head of the input window.
  pub begin_ptr:    usize,
  /// Positioned at the end of the last shifted token
  pub anchor_ptr:   usize,
  /// Positioned at the start of the current incoming token, and following any
  /// characters that have been skipped. (This only differs from `anchor_ptr`
  /// when peeking, in which case there may have been skipped tokens that the
  /// peeking process has encountered and shifted over)
  pub sym_ptr:      usize,
  /// The start of all unevaluated bytes. This differs from `sym_ptr` when
  /// using scanner states to evaluate incoming bytes
  pub input_ptr:    usize,
  /// The end of the input window. This is a fixed reference that should
  /// not change during parsing unless the end of the input window has been
  /// reached and a larger window is requested.
  pub end_ptr:      usize,
  /// The number of characters that comprize the current
  /// token. This should be 0 if the tok_id is also 0
  pub tok_id:       u32,
  /// The byte length of the current token
  pub tok_byte_len: u32,
  /// The byte length of the most current input character. This is usually 1
  /// byte unless the input contains UTF codepoints outside the ASCII range.
  pub byte_len:     u32,

  /// Flags -------------------------------------------------------------------
  pub is_finished: bool,

  pub nonterm: u32,

  pub goal_nonterm: u32,

  // Line info ------------ TO BE DEPRECATED
  /// The offset of the last line character recognized that proceeds the anchor
  pub start_line_off:  u32,
  /// The offset of the last line character recognized that proceeds the chkp
  pub chkp_line_off:   u32,
  /// The offset of the last line character recognized that proceeds the tail
  pub end_line_off:    u32,
  /// The number of line character recognized that proceed the anchor
  pub start_line_num:  u32,
  /// The number of line character recognized that proceed the chkp
  pub chkp_line_num:   u32,
  /// The number of line character recognized that proceed the tail
  pub end_line_num:    u32,
  /// If  this is a non-zero value, then when the parser fails to match an
  /// incoming token, it will insert a zero-length token with an id of
  /// `default_id` into the token stream, and then attempt to continue parsing.
  pub recovery_tok_id: u32,
}

impl Default for ParserContext {
  fn default() -> Self {
    ParserContext {
      stack:           vec![],
      begin_ptr:       0,
      anchor_ptr:      0,
      sym_ptr:         0,
      input_ptr:       0,
      end_ptr:         0,
      tok_byte_len:    0,
      byte_len:        0,
      chkp_line_num:   0,
      chkp_line_off:   0,
      end_line_num:    0,
      end_line_off:    0,
      nonterm:         0,
      start_line_num:  0,
      start_line_off:  0,
      tok_id:          0,
      recovery_tok_id: 0,
      goal_nonterm:    u32::MAX,
      is_finished:     false,
    }
  }
}

impl ParserContext {
  pub fn current_tok(&self) -> TokenRange {
    TokenRange {
      len:      self.tok_byte_len as u32,
      off:      self.sym_ptr as u32,
      line_num: self.start_line_num,
      line_off: self.start_line_off,
    }
  }

  pub fn push_state(&mut self, mut state: ParserState) {
    state.info.stack_address = self.stack.len() as u16;
    self.stack.push(state);
  }

  pub fn pop_state(&mut self) -> ParserState {
    unsafe { self.stack.pop().unwrap_unchecked() }
  }
}

pub trait ForkableContext: QueuedContext {
  fn symbols(&mut self) -> &mut Vec<CSTNode>;
  fn ctx(&self) -> &ParserContext;
  fn ctx_mut(&mut self) -> &mut ParserContext;
  fn entropy(&self) -> &isize;
  fn entropy_mut(&mut self) -> &mut isize;
  fn split(&self) -> Self;
  fn set_offset(&mut self, offset: usize);
  fn get_offset(&self) -> usize;
}

impl<T: ForkableContext> QueuedContext for T {
  fn queued_priority(&self) -> usize {
    usize::MAX - self.get_offset()
  }
}

impl<CTX: ForkableContext> ForkableContext for Box<CTX> {
  #[inline]
  fn ctx(&self) -> &ParserContext {
    self.as_ref().ctx()
  }

  #[inline]
  fn ctx_mut(&mut self) -> &mut ParserContext {
    self.as_mut().ctx_mut()
  }

  #[inline]
  fn entropy(&self) -> &isize {
    self.as_ref().entropy()
  }

  #[inline]
  fn entropy_mut(&mut self) -> &mut isize {
    self.as_mut().entropy_mut()
  }

  #[inline]
  fn symbols(&mut self) -> &mut Vec<CSTNode> {
    self.as_mut().symbols()
  }

  #[inline]
  fn split(&self) -> Self {
    Box::new(self.as_ref().split())
  }

  #[inline]
  fn get_offset(&self) -> usize {
    self.as_ref().get_offset()
  }

  #[inline]
  fn set_offset(&mut self, offset: usize) {
    self.as_mut().set_offset(offset);
  }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ForkContext {
  pub(crate) entropy: isize,
  pub(crate) offset:  usize,
  pub(crate) ctx:     ParserContext,
  pub(crate) symbols: Vec<CSTNode>,
}

impl ForkableContext for ForkContext {
  #[inline]
  fn ctx(&self) -> &ParserContext {
    &self.ctx
  }

  #[inline]
  fn ctx_mut(&mut self) -> &mut ParserContext {
    &mut self.ctx
  }

  #[inline]
  fn entropy(&self) -> &isize {
    &self.entropy
  }

  #[inline]
  fn entropy_mut(&mut self) -> &mut isize {
    &mut self.entropy
  }

  #[inline]
  fn symbols(&mut self) -> &mut Vec<CSTNode> {
    &mut self.symbols
  }

  #[inline]
  fn split(&self) -> Self {
    Self {
      offset:  self.offset,
      entropy: self.entropy,
      ctx:     self.ctx.clone(),
      symbols: self.symbols.clone(),
    }
  }

  fn get_offset(&self) -> usize {
    self.offset
  }

  fn set_offset(&mut self, offset: usize) {
    self.offset = offset
  }
}

#[derive(Debug, Clone, Copy)]
pub enum RecoveryMode {
  Normal,
  CodepointDiscard { start_offset: usize, count: usize },
  SymbolDiscard { start_offset: usize, end_offset: usize, count: usize },
  SyntheticInput { tok_id: u32, count: usize },
  Unrecoverable(usize),
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct RecoverableContext {
  pub(crate) offset: usize,
  pub entropy: isize,
  pub ctx: ParserContext,
  pub symbols: Vec<CSTNode>,
  pub mode: RecoveryMode,
  pub last_failed_state: ParserState,
}

impl ForkableContext for RecoverableContext {
  #[inline]
  fn ctx(&self) -> &ParserContext {
    &self.ctx
  }

  #[inline]
  fn ctx_mut(&mut self) -> &mut ParserContext {
    &mut self.ctx
  }

  #[inline]
  fn entropy(&self) -> &isize {
    &self.entropy
  }

  #[inline]
  fn entropy_mut(&mut self) -> &mut isize {
    &mut self.entropy
  }

  #[inline]
  fn symbols(&mut self) -> &mut Vec<CSTNode> {
    &mut self.symbols
  }

  #[inline]
  fn split(&self) -> Self {
    let mut ctx = self.ctx.clone();
    ctx.is_finished = false;
    Self {
      ctx,
      mode: RecoveryMode::Unrecoverable(0),
      symbols: self.symbols.clone(),
      ..*self
    }
  }

  fn get_offset(&self) -> usize {
    self.offset
  }

  fn set_offset(&mut self, offset: usize) {
    self.offset = offset
  }
}

impl PartialEq for RecoverableContext {
  fn eq(&self, _other: &Self) -> bool {
    false
  }
}

impl PartialOrd for RecoverableContext {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a = (self.entropy, usize::MAX - self.offset);
    let b = (other.entropy, usize::MAX - other.offset);

    if a < b {
      Some(Ordering::Less)
    } else {
      Some(Ordering::Greater)
    }
  }
}

impl Eq for RecoverableContext {}
impl Ord for RecoverableContext {
  fn cmp(&self, other: &Self) -> Ordering {
    self.partial_cmp(other).unwrap()
  }
}

impl RecoverableContext {}
