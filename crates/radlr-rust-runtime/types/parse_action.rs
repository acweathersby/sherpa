use std::rc::Rc;

use super::*;

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseActionType {
  None,
  Error,
  Reduce,
  Shift,
  Accept,
  Fork,
  Skip,
  NeedMoreInput,
}

impl Into<u32> for ParseActionType {
  fn into(self) -> u32 {
    self as u32
  }
}

impl Into<u64> for ParseActionType {
  fn into(self) -> u64 {
    self as u64
  }
}

#[derive(Clone, PartialEq, Eq, Debug)]
#[repr(C, u32)]
pub enum ParseAction {
  Undefined,
  CompleteState,
  FailState,
  ScannerToken(TokenRange),
  Fork(Vec<ParserState>),
  ShiftNode {
    node: Rc<CSTNode>,
  },
  Shift {
    byte_offset:              u32,
    byte_length:              u32,
    token_line_offset:        u32,
    token_line_count:         u32,
    token_id:                 u32,
    /// The state that emitted this token.
    emitting_state:           ParserState,
    /// The instruction that follows this
    next_instruction_address: usize,
  },
  Skip {
    byte_offset:       u32,
    byte_length:       u32,
    token_line_offset: u32,
    token_line_count:  u32,
    token_id:          u32,
  },
  RecoveredError {
    token_byte_offset: u32,
    token_byte_length: u32,
    token_line_offset: u32,
    token_line_count:  u32,
    nonterminal_id:    u32,
  },
  Reduce {
    nonterminal_id: u32,
    rule_id:        u32,
    symbol_count:   u32,
  },
  Accept {
    nonterminal_id:    u32,
    final_offset:      usize,
    token_line_offset: u32,
    token_line_count:  u32,
  },
  Error {
    last_state:        ParserState,
    last_nonterminal:  u32,
    byte_offset:       u32,
    byte_length:       u32,
    token_line_offset: u32,
    token_line_count:  u32,
  },
  EndOfInput {
    current_cursor_offset: u32,
  },
  NonTerminalParseStart,
  None,
}

#[test]
fn parse_actions_size_is_24() {
  assert_eq!(std::mem::size_of::<ParseAction>(), 24)
}

impl Default for ParseAction {
  fn default() -> Self {
    ParseAction::Undefined
  }
}
impl ParseAction {
  pub const PA_ACCEPT: u64 = 7;
  pub const PA_COMPLETE_STATE: u64 = 1;
  pub const PA_END_OF_INPUT: u64 = 9;
  pub const PA_ERROR: u64 = 8;
  pub const PA_FAIL_STATE: u64 = 2;
  pub const PA_FORK: u64 = 4;
  pub const PA_PRODUCTION_PARSE_START: u64 = 10;
  pub const PA_REDUCE: u64 = 6;
  pub const PA_SCANNER_TOKEN: u64 = 3;
  pub const PA_SHIFT: u64 = 5;
  pub const PA_UNDEFINED: u64 = 0;
}
