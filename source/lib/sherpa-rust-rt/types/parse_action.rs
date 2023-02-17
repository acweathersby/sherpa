use super::*;

#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseActionType {
  None,
  Error,
  Reduce,
  Shift,
  Accept,
  Fork,
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, u32)]
pub enum ParseAction {
  Undefined,
  CompleteState,
  FailState,
  ScannerToken(TokenRange),
  Fork {
    states_start_offset: u32,
    num_of_states:       u32,
    target_production:   u32,
  },
  Shift {
    anchor_byte_offset: u32,
    token_byte_offset:  u32,
    token_byte_length:  u32,
    token_line_offset:  u32,
    token_line_count:   u32,
  },
  Reduce {
    production_id: u32,
    rule_id:       u32,
    symbol_count:  u32,
  },
  Accept {
    production_id: u32,
    // reached_EOF:   bool,
  },
  Error {
    last_production: u32,
    last_input:      TokenRange,
  },
  EndOfInput {
    current_cursor_offset: u32,
  },
  ProductionParseStart,
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
