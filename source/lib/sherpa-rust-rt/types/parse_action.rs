use super::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, u64)]
pub enum ParseAction {
  Undefined,
  CompleteState,
  FailState,
  ScannerToken(ParseToken),
  Fork {
    states_start_offset: u32,
    num_of_states:       u32,
    target_production:   u32,
  },
  Shift {
    anchor_byte_offset: u32,
    anchor_cp_offset:   u32,
    token_byte_offset:  u32,
    token_cp_offset:    u32,
    token_byte_length:  u32,
    token_cp_length:    u32,
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
    last_input:      ParseToken,
    last_production: u32,
  },
  EndOfInput {
    current_cursor_offset: u32,
  },
  ProductionParseStart,
}

impl Default for ParseAction {
  fn default() -> Self {
    ParseAction::Undefined
  }
}
