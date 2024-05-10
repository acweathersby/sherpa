use super::*;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum ParserError {
  InputError { message: String, inline_message: String, loc: Token, last_nonterminal: u32 },
  Unexpected,
  InvalidNonTerminal,
  InvalidEntryName,
  NoData,
  OutOfMemory,
}

impl std::fmt::Debug for ParserError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParserError::InputError { message, inline_message, loc, .. } => f.write_fmt(format_args!(
        "\n{}\n\n{}\n\n{}\n",
        message.trim(),
        loc.loc_stub(),
        loc.blame(1, 1, &inline_message.trim(), BlameColor::RED),
      )),
      ParserError::OutOfMemory => f.write_str("Out of memory"),
      ParserError::NoData => f.write_str("Out of input data"),
      ParserError::InvalidEntryName => f.write_str("Invalid Entry Name"),
      _ => f.write_str("Unexpected error"),
    }
  }
}
