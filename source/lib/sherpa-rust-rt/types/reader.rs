use crate::utf8::*;
use std::sync::Arc;

use super::TokenRange;

/// A multi-reader, multi-writer view of the underlying parser input
/// data, used to distribute access to the input string over multiple
/// Tokens and SymbolReaders.
pub type SharedSymbolBuffer = Arc<Vec<u8>>;

/// A reader maintains a read cursor which can be moved bidirectionally
/// over an array of bytes and yields information on the following:
///
/// - The byte offset and Unicode codepoint offset of the cursor.
/// - The 8bit value of the current byte
/// - The Unicode codepoint value at the cursor, if there is a valid symbol there.
/// - The Hydrocarbon character class of the current symbol, if
///   available.
/// This loosely correlates to certain Unicode character classes.
/// - The UTF8 codepoint length, if it exists.
/// - The number of newline characters encountered up until the
///   current location of the cursor.
/// - Both the codepoint offset and byte offset of the last line
/// encountered in the input.
pub trait ByteReader {
  /// Returns true if the cursor has reached the end of
  /// the input stream.

  fn at_end(&self) -> bool {
    self.cursor() >= self.len()
  }

  #[inline(always)]
  fn offset_at_end(&self, offset: u32) -> bool {
    self.len() <= offset as usize
  }

  /// Returns the word at the current cursor position, little
  /// Endian

  fn qword(&self) -> u32;

  /// Returns the byte at the current cursor position.
  fn byte(&self) -> u32;

  /// Returns the length of the source input. If this unknown
  /// then returns 0

  fn len(&self) -> usize {
    0
  }

  /// Returns the number of lines encountered.
  fn line_count(&self) -> u32 {
    0
  }

  /// Returns the offset of the most recent line character.
  fn line_offset(&self) -> u32 {
    0
  }

  /// Return a tuple comprised of the current line count
  /// and the current line offset, respectively.
  fn get_line_data(&self) -> u64 {
    0
  }
  /// Return a u64 where the high 32bits represents
  /// the the byte length of the current character
  /// and the low 32bits represent the UTF16 codepoint
  /// length.
  fn get_length_data(&self) -> u64 {
    0
  }

  /// Resets the cursor back to the position of the token. Returns
  /// the same value as `get_type_info`.
  fn set_cursor_to(&mut self, off: u32, line_num: u32, line_off: u32) -> u64;

  /// Return a packed u64 containing codepoint info the higher 32 bits,
  /// class in the high 16, codepoint length in the high 8 bits,
  /// and byte data in the low 8
  #[inline(always)]
  fn get_type_info(&self) -> u64 {
    if self.at_end() {
      0
    } else {
      ((self.codepoint() as u64) << 32)
        | ((self.class() as u64) << 16)
        | ((self.codepoint_byte_length() as u64) << 8)
        | (self.byte() as u64)
    }
  }

  /// Returns UTF8 codepoint information at the current cursor
  /// position.

  fn codepoint(&self) -> u32;

  #[inline(always)]
  fn codepoint_byte_length(&self) -> u32 {
    return get_utf8_byte_length_from_code_point(self.codepoint());
  }

  #[inline(always)]
  fn codepoint_length(&self) -> u32 {
    return get_token_length_from_code_point(self.codepoint());
  }

  #[inline(always)]
  fn class(&self) -> u32 {
    if self.codepoint() > 0 {
      get_token_class_from_codepoint(self.codepoint())
    } else {
      0
    }
  }

  fn cursor(&self) -> usize;

  /// Returns an optional vector of the input string data.
  fn get_source(&self) -> SharedSymbolBuffer;

  fn get_bytes(&self) -> &[u8];
}

pub trait MutByteReader {
  fn set_cursor(&mut self, cursor: usize);
  fn set_codepoint(&mut self, code_point: u32);
  fn set_dword(&mut self, dword: u32);
  fn set_line_count(&mut self, line_count: u32);
  fn set_line_offset(&mut self, line_offset: u32);
  fn next(&mut self, amount: i32) -> u64;
}

pub trait UTF8Reader {
  fn next_utf8<T: ByteReader + MutByteReader + UTF8Reader>(self_: &mut T, amount: i32) -> u64 {
    self_.set_cursor((self_.cursor() as i32 + amount) as usize);

    if self_.at_end() {
      self_.set_dword(0);
      self_.set_codepoint(0);
      0
    } else {
      if amount == 1 {
        self_.set_dword((self_.qword() >> 8) | ((self_.byte() as u32) << 24));

        if self_.get_bytes()[self_.cursor()] == 10 {
          self_.set_line_count(self_.line_count() + 1);

          self_.set_line_offset(self_.cursor() as u32);
        }
      } else {
        let diff = std::cmp::max(std::cmp::min(4, (self_.len() - self_.cursor()) as i32), 0) as u32;

        let start = self_.cursor() as u32;

        let end = self_.cursor() as u32 + (diff as u32);

        let mut dword = 0;

        let mut offset = 32;

        for i in start..end {
          offset -= 8;

          let byte = self_.get_bytes()[i as usize];

          dword |= (byte as u32) << offset;
        }

        for i in (((self_.cursor() as i32) - amount) as usize + 1)
          ..std::cmp::min(self_.len() as usize, self_.cursor() + 1)
        {
          let byte = self_.get_bytes()[i as usize];

          if byte == 10 {
            self_.set_line_count(self_.line_count() + 1);

            self_.set_line_offset(self_.cursor() as u32);
          }
        }

        self_.set_dword(dword);
      }

      self_.set_codepoint(get_utf8_code_point_from(self_.qword()));

      self_.get_type_info()
    }
  }

  fn get_str<'a>(&'a self) -> &'a str;
}

#[repr(C)]
#[derive(Debug)]
pub struct InputInfo(*const u8, u32, bool);

pub trait LLVMByteReader {
  /// Get a pointer to a sequence of bytes that can be read from the input given
  /// the cursor position. The second tuple values should be the length bytes that
  ///  can be read from the block.
  extern "C" fn get_byte_block_at_cursor<T: ByteReader>(
    self_: &mut T,
    start_offset: u32,
    _end_offset: u32,
  ) -> InputInfo {
    let cursor = start_offset;
    let size = ((self_.len() as i64) - (cursor as i64)).max(0) as u32;

    if size > 0 {
      let ptr = ((self_.get_bytes().as_ptr() as usize) + cursor as usize) as *const u8;
      InputInfo(ptr, self_.len() as u32, false)
    } else {
      InputInfo(0 as *const u8, 0, false)
    }
  }
}
