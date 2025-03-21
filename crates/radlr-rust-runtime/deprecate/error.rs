use crate::types::{BlameColor, Token};
#[cfg(debug_assertions)]
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug)]
pub struct RadlrParseError {
  pub message:          String,
  pub inline_message:   String,
  pub loc:              Token,
  pub last_nonterminal: u32,
}

impl Display for RadlrParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let RadlrParseError { message, inline_message, loc, .. } = self;
    f.write_fmt(format_args!("{}\n{}", message, loc.blame(1, 1, inline_message, BlameColor::RED)))
  }
}
