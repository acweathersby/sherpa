use super::*;

pub enum ParseError {
  InputError { message: String, inline_message: String, loc: Token, last_nonterminal: u32 },
  Unexpected,
  InvalidNonTerminal,
  NoData,
}

impl std::fmt::Debug for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParseError::InputError { message, inline_message, loc, .. } => f.write_fmt(format_args!(
        "\n{}\n\n{}\n\n{}\n",
        message.trim(),
        loc.loc_stub(),
        loc.blame(1, 1, &inline_message.trim(), BlameColor::RED),
      )),
      _ => f.write_str("Unexpected error"),
    }
  }
}
