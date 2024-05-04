use std::{ops::Range, sync::Arc};

use crate::utf8::{get_token_class_from_codepoint, get_utf8_byte_length_from_code_point};

/// A multi-reader, multi-writer view of the underlying parser input
/// data, used to distribute access to the input string over multiple
/// Tokens and SymbolReaders.
pub type SharedSymbolBuffer = Arc<[u8]>;

pub trait ParserInput {
  /// The number of bytes available for reading. This is allowed
  /// to change if the reader supports streaming.
  fn len(&self) -> usize;

  fn byte(&self, cursor: usize) -> u8;

  /// Returns a reference to the input string
  fn bytes(&self) -> &[u8];

  fn get_owned_ref(&self) -> SharedSymbolBuffer;

  fn codepoint(&self, cursor: usize) -> u32 {
    let header_byte = self.byte(cursor);
    let leading_ones = header_byte.leading_ones();

    if leading_ones == 0 {
      header_byte as u32
    } else {
      let mut num = (header_byte & (0b0111_1111 >> leading_ones)) as u32;
      let cursor = cursor + 1;
      for i in 0..(leading_ones - 1) as usize {
        let byte = self.byte(cursor + i);
        num = (num << 6) | (byte & 0b0011_1111) as u32
      }

      num
    }
  }

  fn class(&self, cursor: usize) -> u32 {
    let cp = self.codepoint(cursor);
    if cp > 0 {
      get_token_class_from_codepoint(cp)
    } else {
      0
    }
  }

  fn codepoint_len(&self, cursor: usize) -> u32 {
    get_utf8_byte_length_from_code_point(self.codepoint(cursor))
  }

  /// Attempts to return the given range of bytes as a utf8 encoded String.
  /// Returns an empty string if the range contains invalid ut8 sequences.
  fn string_range(&self, range: Range<usize>) -> String {
    let mut bytes = vec![];
    for i in range {
      bytes.push(self.byte(i))
    }
    String::from_utf8(bytes).unwrap_or_default()
  }
}

#[derive(Debug)]
pub struct StringInput {
  input: Arc<[u8]>,
}

impl From<String> for StringInput {
  fn from(value: String) -> Self {
    Self { input: value.into_bytes().into() }
  }
}

impl From<&str> for StringInput {
  fn from(value: &str) -> Self {
    Self { input: value.as_bytes().into() }
  }
}

impl ParserInput for StringInput {
  fn len(&self) -> usize {
    self.input.len()
  }

  fn byte(&self, cursor: usize) -> u8 {
    if self.len() > cursor {
      self.input[cursor]
    } else {
      0
    }
  }

  fn get_owned_ref(&self) -> SharedSymbolBuffer {
    self.input.clone()
  }

  fn bytes(&self) -> &[u8] {
    &self.input
  }
}

#[test]
fn test_code_point_lookup() {
  let input = "🙂";
  let string_input = StringInput::from(input);
  assert_eq!(string_input.codepoint(0), input.chars().next().unwrap() as u32);
  assert_eq!(string_input.codepoint_len(0), 4);

  let input = "A";
  let string_input = StringInput::from(input);
  assert_eq!(string_input.codepoint(0), input.chars().next().unwrap() as u32);

  let input = "です";

  let string_input = StringInput::from(input);
  assert_eq!(string_input.codepoint(0), input.chars().next().unwrap() as u32)
}
