use std::rc::Rc;
use std::sync::Arc;

use crate::types::ParseToken;
use crate::utf8::*;
#[deprecated]
pub trait ByteReader {
  /// Returns true if the cursor has reached the end of
  /// the input stream.

  fn at_end(&self) -> bool;

  fn offset_at_end(&self, offset: u32) -> bool {
    self.len() <= offset
  }

  /// Advances the cursor up-to 4-bytes forward.

  fn next(&mut self, amount: u32);

  /// Returns the word at the current cursor position, little
  /// Endian

  fn word(&self) -> u32;

  /// Returns the byte at the current cursor position.

  fn byte(&self) -> u8;

  /// Returns the byte at the current cursor position.

  fn len(&self) -> u32;

  /// Returns the number of lines encountered.

  fn line_count(&self) -> u32;

  /// Returns the offset of the most recent line character.

  fn line_offset(&self) -> u32;

  /// Resets the cursor back to the value of the `offset`
  /// argument. Should the offset value exceed the limits
  /// of the underlying implementation, `false` is returned
  /// , indicating a parse failure as the input stream can
  /// no longer satisfy the requirements of the parser.

  fn set_cursor_to(&mut self, token: &ParseToken) -> bool;

  fn set_line_data(&mut self, token: &ParseToken);

  /// Return a new instance of byte reader with the same
  /// state as the source reader. Implementation should provide
  /// adequate shared buffers or other resources used to cache the
  /// input stream data, as multiple ByteReaders may be required
  /// read data at different cursor positions.

  fn clone(&self) -> Self;

  /// Returns UTF8 codepoint information at the current cursor
  /// position.

  fn codepoint(&self) -> u32 {
    0
  }

  fn codepoint_byte_length(&self) -> u32 {
    get_utf8_byte_length_from_code_point(self.codepoint())
  }

  fn codepoint_length(&self) -> u32 {
    get_token_length_from_code_point(self.codepoint())
  }

  fn class(&self) -> u32 {
    get_token_class_from_codepoint(self.codepoint())
  }

  fn cursor(&self) -> u32;

  /// Returns an optional vector of the input string data.

  fn get_source(&self) -> Arc<Vec<u8>>;
}

pub struct UTF8FileReader {
  length:      usize,
  cursor:      usize,
  line_count:  usize,
  line_offset: usize,
  string:      Rc<Vec<u8>>,
  word:        u32,
  codepoint:   u32,
}
