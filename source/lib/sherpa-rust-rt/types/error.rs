use std::fmt::{Debug, Display};

use crate::types::BlameColor;

use super::Token;

#[derive(Debug)]
pub enum SherpaParseError {
  InvalidParse { message: String, inline_message: String, loc: Token, last_production: u32 },
  None,
}

impl Display for SherpaParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use SherpaParseError::*;

    match self {
      InvalidParse { message, inline_message, loc, .. } => f.write_fmt(format_args!(
        "{}\n{}",
        message,
        loc.blame(1, 1, inline_message, BlameColor::Red)
      )),
      _ => f.write_str("An error occurred"),
    }
  }
}
