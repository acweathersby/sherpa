use std::default;

use super::*;
use crate::bytecode::{DebugEventNew, DebugFnNew};

pub trait ParserInitializer {
  /// Creates a new parse context configured to recognize and produce parse
  /// actions for the given non-terminal id
  fn init(&mut self, entry: EntryPoint) -> Result<ParserContext, ParseError>;

  /// Resets the parser and prepares it to accept in input that should yield the
  /// given non-terminal. A parser should be initialized before attempting to
  /// iterate.
  ///
  /// An error is returned if `nonterminal_goal_id` is an invalid entry point
  /// of the parser.
  fn init_range(&mut self, nonterminal_goal_id: u32, start: usize, end: usize) -> Result<ParserContext, ParseError>;

  fn get_debugger(&mut self) -> &mut Option<Box<DebugFnNew>>;

  fn set_debugger(&mut self, debugger: Option<Box<DebugFnNew>>);
}

pub trait ParserIterator<T: ParserInput> {
  /// Given a parse context, returns the next ParseAction for that context,
  /// mutating the context as needed.
  ///
  /// Returns None if the context has already entered a finished state
  fn next<'ctx>(&mut self, input: &mut T, context: &'ctx mut ParserContext) -> Option<ParseAction>;
}

/// A simple reference parser that determines whether a givin input is a valid
/// string or not.
pub trait ParserCompleter<T: ParserInput>: ParserIterator<T> + ParserInitializer {
  /// Attempts to recognize the given input in its entirerty
  fn completes(&mut self, input: &mut T, entry: EntryPoint) -> Result<(), ParseError> {
    let mut ctx = self.init(entry)?;

    while let Some(action) = self.next(input, &mut ctx) {
      match action {
        ParseAction::Accept { nonterminal_id, final_offset } => {
          return if final_offset != input.len() {
            Err(ParseError::InputError {
              inline_message: format!(
                "\nFailed to read entire input \"{}\" \n     end pos: {} \n     expected end pos: {}",
                input.string_range(0..input.len()),
                final_offset,
                input.len(),
              ),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "Failed to read entire input".to_string(),
            })
          } else if nonterminal_id != entry.nonterm_id {
            Err(ParseError::InputError {
              inline_message: "Top symbol did not match the target nonterminal".to_string(),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "CST is incorrect".to_string(),
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
          if let Some(debug) = self.get_debugger() {
            debug(&DebugEventNew::ActionShift { offset_start, offset_end, token_id }, input);
          }
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } =>
        {
          #[cfg(debug_assertions)]
          if let Some(debug) = self.get_debugger() {
            debug(&DebugEventNew::ActionReduce { rule_id: _rule_id }, input);
          }
        }
        ParseAction::Error { last_nonterminal, .. } => {
          let last_input = TokenRange {
            len:      ctx.tok_byte_len as u32,
            off:      ctx.sym_ptr as u32,
            line_num: 0,
            line_off: 0,
          };
          let token: Token = last_input.to_token_from_ref(input.get_owned_ref());
          return Err(ParseError::InputError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_nonterminal,
          });
        }
        _ => {}
      }
    }

    Err(ParseError::Unexpected)
  }
}

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ShiftsAndSkips {
  pub shifts: Vec<String>,
  pub skips:  Vec<String>,
}

pub trait ArtifactProducer<T: ParserInput>: ParserIterator<T> + ParserInitializer {
  fn collect_shifts_and_skips<'debug>(&mut self, input: &mut T, entry: EntryPoint) -> Result<ShiftsAndSkips, ParseError> {
    let mut shifts = vec![];
    let mut skips = vec![];

    let mut ctx = self.init(entry)?;

    while let Some(action) = self.next(input, &mut ctx) {
      match action {
        ParseAction::Accept { nonterminal_id, final_offset } => {
          return if final_offset != input.len() {
            Err(ParseError::InputError {
              inline_message: format!("Failed to read entire input {} {}", input.len(), final_offset),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "Failed to read entire input".to_string(),
            })
          } else if nonterminal_id != entry.nonterm_id {
            Err(ParseError::InputError {
              inline_message: "Top symbol did not match the target nonterminal".to_string(),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "CST is incorrect".to_string(),
            })
          } else {
            return Ok(ShiftsAndSkips { shifts, skips });
          };
        }
        ParseAction::Error { last_nonterminal, .. } => {
          let last_input = TokenRange {
            len:      ctx.tok_byte_len as u32,
            off:      ctx.sym_ptr as u32,
            line_num: 0,
            line_off: 0,
          };
          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionError {}, self.get_ctx());
          //}
          let mut token: Token = last_input.to_token_from_ref(input.get_owned_ref());

          return Err(ParseError::InputError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_nonterminal,
          });
        }
        ParseAction::Fork { .. } => {
          panic!("No implementation of fork resolution is available")
        }
        ParseAction::Skip { byte_offset: token_byte_offset, byte_length: token_byte_length, .. } => {
          skips.push(input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize));
        }
        ParseAction::Shift {
          byte_length: token_byte_length,
          byte_offset: token_byte_offset,
          token_id,
          ..
        } => {
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;

          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionShift { offset_start, offset_end, token_id },
          // self.get_ctx());
          //}
          shifts.push(input.string_range(offset_start..offset_end));
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } => {
          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionReduce { rule_id: _rule_id },
          // self.get_ctx());
          //}
        }
        _ => panic!("Unexpected Action!"),
      }
    }
    return Err(ParseError::Unexpected);
  }
}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> ParserCompleter<I> for T {}
impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> ArtifactProducer<I> for T {}

pub trait Parser<T: ParserInput>: ParserIterator<T> + ParserCompleter<T> + ArtifactProducer<T> {}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> Parser<I> for T {}

/// Provides information on artifacts consumed and produced by a parser
pub trait RuntimeDatabase {
  fn get_entry_data_from_name(&self, entry_name: &str) -> Result<EntryPoint, ParseError>;

  /// Return the normal token ids (those that are not skipped) expected at the
  /// given state.
  #[allow(unused)]
  fn get_expected_tok_ids_at_state(&self, state_id: u32) -> Option<&[u32]>;

  /// Returns a human friendly string representation of the given token id.
  fn token_id_to_str<'str>(&'str self, id: u32) -> Option<&'str str>;
}

/// An object capable of instantiating a parser
pub trait ParserProducer<T: ParserInput>: RuntimeDatabase {
  /// Creates a new parser or `Err` if there is an issue creating the parser.
  fn get_parser(&self) -> Result<Box<dyn Parser<T>>, ParseError> {
    Err(ParseError::NoData)
  }
}

#[repr(u8)]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum StateType {
  #[default]
  Normal,
  ErrorRecovery,
}

#[repr(C)]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct UniqueParseState {
  pub state_id: u16,
  pub address:  u16,
}

#[repr(C)]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct StateInfo {
  pub stack_address: u16,
  pub state_type:    StateType,
  pub is_header:     bool,
  pub state_id:      u32,
}

#[repr(C)]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ParserState {
  pub address: usize,
  pub info:    StateInfo,
}

/// Combination of StateId and stack address.
pub struct ParseStateRef(u32);

impl ParserState {
  pub fn state_entry(address: usize) -> Self {
    Self {
      address,
      info: StateInfo {
        state_id:      address as u32,
        stack_address: 0,
        state_type:    StateType::Normal,
        is_header:     true,
      },
    }
  }

  pub fn goto_entry(address: usize) -> Self {
    Self {
      address,
      info: StateInfo {
        state_id:      address as u32,
        stack_address: 0,
        state_type:    StateType::Normal,
        is_header:     false,
      },
    }
  }

  pub fn get_address(&self) -> usize {
    self.address as usize
  }

  pub fn get_info(&self) -> StateInfo {
    self.info
  }
}

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
