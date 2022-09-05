/// The default token structure used within hc parsers. Differs from
/// the more general [Token](crate::types::Token) as [ParseToken] can only identify an arbitrary character
/// sequence, and is disassociated with the original input string that was used
/// to derive the token.
///
/// ### Advance Token Methods
///
/// Use `Token::from_parse_token` combined with `Token::set_source` to convert ParseToken
/// data into a more useful form for analysis and manipulation:
///
/// ```
/// # use hctk::types::*;
///
/// let mut reader = UTF8StringReader::new("<hello dave>");
///
/// let mut  parse_token = ParseToken::new();
/// parse_token.byte_offset = 1;
/// parse_token.cp_offset = 1;
/// parse_token.byte_length = 10;
/// parse_token.cp_length = 10;
///
/// let mut token = Token::from_parse_token(&parse_token);
/// token.set_source(reader.get_source());
///
/// eprintln!("{}", token.blame(1,1, "test"));
/// ```

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
#[repr(C)]
pub struct ParseToken {
  // WARNING: DO NOT change the order of these properties
  // without also adjusting the LLVM compiler code that makes use
  // of this structure.
  pub byte_offset: u32,
  pub cp_offset:   u32,

  pub byte_length: u32,
  pub cp_length:   u32,

  pub token_type: u32,
  pub garbage:    u32,

  pub line_number: u32,
  pub line_offset: u32,
}

impl ParseToken {
  pub fn new() -> ParseToken {
    ParseToken::default()
  }

  #[inline]
  pub fn next(&self) -> ParseToken {
    ParseToken {
      cp_offset: self.cp_offset + self.cp_length,
      byte_offset: self.byte_offset + self.byte_length,
      line_number: self.line_number,
      line_offset: self.line_offset,
      ..Default::default()
    }
  }
}
