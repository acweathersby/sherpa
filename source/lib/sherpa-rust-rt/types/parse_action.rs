use super::*;

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
  pub const des_Accept: u64 = 7;
  pub const des_CompleteState: u64 = 1;
  pub const des_EndOfInput: u64 = 9;
  pub const des_Error: u64 = 8;
  pub const des_FailState: u64 = 2;
  pub const des_Fork: u64 = 4;
  pub const des_ProductionParseStart: u64 = 10;
  pub const des_Reduce: u64 = 6;
  pub const des_ScannerToken: u64 = 3;
  pub const des_Shift: u64 = 5;
  pub const des_Undefined: u64 = 0;
}
