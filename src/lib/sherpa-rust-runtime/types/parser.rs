use super::*;
use crate::parsers::Parser;

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
