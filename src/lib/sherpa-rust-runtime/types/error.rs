#[cfg(debug_assertions)]
use std::fmt::Debug;

use std::fmt::Display;

use crate::types::BlameColor;

use super::Token;

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct SherpaParseError {
  pub message: String,
  pub inline_message: String,
  pub loc: Token,
  pub last_nonterminal: u32,
}

impl Display for SherpaParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let SherpaParseError { message, inline_message, loc, .. } = self;
    f.write_fmt(format_args!("{}\n{}", message, loc.blame(1, 1, inline_message, BlameColor::RED)))
  }
}
