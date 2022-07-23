use std::rc::Rc;
use std::sync::Arc;
use std::sync::RwLock;

use crate::types::ParseToken;
use crate::utf8::get_utf8_code_point_from;

use super::reader::CharacterReader;
use super::reader::SharedSymbolBuffer;

#[derive(Debug, Clone)]

pub struct UTF8StringReader
{
  length:      usize,
  cursor:      usize,
  line_count:  usize,
  line_offset: usize,
  string:      Vec<u8>,
  word:        u32,
  codepoint:   u32,
}

impl UTF8StringReader
{
  pub fn from_string(string: &str) -> Self
  {
    Self::new(string.into())
  }

  pub fn new(string: String) -> UTF8StringReader
  {
    let mut reader = UTF8StringReader {
      length:      string.len(),
      string:      Vec::<u8>::from(string),
      cursor:      0,
      word:        0,
      line_count:  0,
      line_offset: 0,
      codepoint:   0,
    };

    reader.next(0);

    reader
  }
}

impl CharacterReader for UTF8StringReader
{
  #[inline(always)]
  fn get_source(&self) -> SharedSymbolBuffer
  {
    let vec = self.string.clone();

    SharedSymbolBuffer::new(RwLock::new(vec![]))
  }

  #[inline(always)]
  fn at_end(&self) -> bool
  {
    self.cursor >= self.length
  }

  #[inline(always)]
  fn get_line_data(&self) -> u64
  {
    ((self.line_count as u64) << 32) | self.line_offset as u64
  }

  #[inline(always)]
  fn get_length_data(&self) -> u64
  {
    ((self.codepoint_byte_length() as u64) << 32) | self.codepoint_length() as u64
  }

  #[inline(always)]
  fn set_cursor_to(&mut self, token: &ParseToken) -> u64
  {
    let ParseToken {
      byte_offset,
      line_number,
      line_offset,
      ..
    } = *token;

    if self.cursor != byte_offset as usize {
      let diff = byte_offset as i32 - self.cursor as i32;

      self.line_count = line_number as usize;

      self.line_offset = line_offset as usize;

      self.next(diff)
    } else {
      self.get_type_info()
    }
  }

  fn clone(&self) -> Self
  {
    UTF8StringReader {
      length:      self.length,
      string:      self.string.clone(),
      cursor:      self.cursor,
      word:        self.word,
      codepoint:   self.codepoint,
      line_count:  self.line_count,
      line_offset: self.line_offset,
    }
  }

  #[inline(always)]
  fn length(&self) -> u32
  {
    self.length as u32
  }

  #[inline(always)]
  fn byte(&self) -> u32
  {
    unsafe { *self.string.get_unchecked(self.cursor) as u32 }
  }

  #[inline(always)]
  fn word(&self) -> u32
  {
    self.word
  }

  #[inline(always)]
  fn line_offset(&self) -> u32
  {
    self.line_offset as u32
  }

  #[inline(always)]
  fn line_count(&self) -> u32
  {
    self.line_count as u32
  }

  #[inline(always)]
  fn codepoint(&self) -> u32
  {
    self.codepoint
  }

  fn next(&mut self, amount: i32) -> u64
  {
    self.cursor = (self.cursor as i32 + amount) as usize;

    self.codepoint = 0;

    if self.at_end() {
      0
    } else {
      if amount == 1 {
        self.word = (self.word >> 8) | ((self.byte() as u32) << 24);

        if self.string[self.cursor] == 10 {
          self.line_count += 1;

          self.line_offset = self.cursor;
        }
      } else {
        let diff =
          std::cmp::max(std::cmp::min(4, (self.length - self.cursor) as i32), 0) as u32;

        let start = self.cursor as u32;

        let end = self.cursor as u32 + (diff as u32);

        let mut word = 0;

        let mut offset = 32;

        for i in start..end {
          offset -= 8;

          let byte = self.string[i as usize];

          word |= (byte as u32) << offset;
        }

        for i in (((self.cursor as i32) - amount) as usize + 1)
          ..std::cmp::min(self.length, self.cursor + 1)
        {
          let byte = self.string[i as usize];

          if byte == 10 {
            self.line_count += 1;

            self.line_offset = i as usize;
          }
        }

        self.word = word;
      }

      self.codepoint = get_utf8_code_point_from(self.word);

      self.get_type_info()
    }
  }

  #[inline(always)]
  fn cursor(&self) -> u32
  {
    self.cursor as u32
  }

  fn get_byte_block_at_cursor(
    &mut self,
    block_ptr: &mut *const u8,
    token_offset: u32,
    requested_size: u32,
  ) -> u32
  {
    let size = ((self.length as i64) - (token_offset as i64)).max(0);

    if size > 0 {
      let ptr = (self.string.as_ptr() as usize + token_offset as usize) as *const u8;
      *block_ptr = ptr;
      size as u32
    } else {
      *block_ptr = self.string.as_ptr();
      0
    }
  }
}

pub struct UTF8StringReader2<'a>
{
  length:      usize,
  cursor:      usize,
  line_count:  usize,
  line_offset: usize,
  string:      &'a [u8],
  word:        u32,
  codepoint:   u32,
}

impl<'a> UTF8StringReader2<'a>
{
  pub fn from_string(string: &'a str) -> Self
  {
    Self::new(string)
  }

  pub fn new(string: &'a str) -> UTF8StringReader2<'a>
  {
    let mut reader = UTF8StringReader2 {
      string:      string.as_bytes(),
      length:      string.len(),
      cursor:      0,
      word:        0,
      line_count:  0,
      line_offset: 0,
      codepoint:   0,
    };

    reader.next(0);

    reader
  }
}

impl<'a> CharacterReader for UTF8StringReader2<'a>
{
  #[inline(always)]
  fn get_source(&self) -> SharedSymbolBuffer
  {
    let vec = self.string.clone();

    SharedSymbolBuffer::new(RwLock::new(vec![]))
  }

  #[inline(always)]
  fn at_end(&self) -> bool
  {
    self.cursor >= self.length
  }

  #[inline(always)]
  fn get_line_data(&self) -> u64
  {
    ((self.line_count as u64) << 32) | self.line_offset as u64
  }

  #[inline(always)]
  fn get_length_data(&self) -> u64
  {
    ((self.codepoint_byte_length() as u64) << 32) | self.codepoint_length() as u64
  }

  #[inline(always)]
  fn set_cursor_to(&mut self, token: &ParseToken) -> u64
  {
    let ParseToken {
      byte_offset,
      line_number,
      line_offset,
      ..
    } = *token;

    if self.cursor != byte_offset as usize {
      let diff = byte_offset as i32 - self.cursor as i32;

      self.line_count = line_number as usize;

      self.line_offset = line_offset as usize;

      self.next(diff)
    } else {
      self.get_type_info()
    }
  }

  fn clone(&self) -> Self
  {
    UTF8StringReader2 {
      length:      self.length,
      string:      self.string.clone(),
      cursor:      self.cursor,
      word:        self.word,
      codepoint:   self.codepoint,
      line_count:  self.line_count,
      line_offset: self.line_offset,
    }
  }

  #[inline(always)]
  fn length(&self) -> u32
  {
    self.length as u32
  }

  #[inline(always)]
  fn byte(&self) -> u32
  {
    unsafe { *self.string.get_unchecked(self.cursor) as u32 }
  }

  #[inline(always)]
  fn word(&self) -> u32
  {
    self.word
  }

  #[inline(always)]
  fn line_offset(&self) -> u32
  {
    self.line_offset as u32
  }

  #[inline(always)]
  fn line_count(&self) -> u32
  {
    self.line_count as u32
  }

  #[inline(always)]
  fn codepoint(&self) -> u32
  {
    self.codepoint
  }

  fn next(&mut self, amount: i32) -> u64
  {
    self.cursor = (self.cursor as i32 + amount) as usize;

    self.codepoint = 0;

    if self.at_end() {
      0
    } else {
      if amount == 1 {
        self.word = (self.word >> 8) | ((self.byte() as u32) << 24);

        if self.string[self.cursor] == 10 {
          self.line_count += 1;

          self.line_offset = self.cursor;
        }
      } else {
        let diff =
          std::cmp::max(std::cmp::min(4, (self.length - self.cursor) as i32), 0) as u32;

        let start = self.cursor as u32;

        let end = self.cursor as u32 + (diff as u32);

        let mut word = 0;

        let mut offset = 32;

        for i in start..end {
          offset -= 8;

          let byte = self.string[i as usize];

          word |= (byte as u32) << offset;
        }

        for i in (((self.cursor as i32) - amount) as usize + 1)
          ..std::cmp::min(self.length, self.cursor + 1)
        {
          let byte = self.string[i as usize];

          if byte == 10 {
            self.line_count += 1;

            self.line_offset = i as usize;
          }
        }

        self.word = word;
      }

      self.codepoint = get_utf8_code_point_from(self.word);

      self.get_type_info()
    }
  }

  #[inline(always)]
  fn cursor(&self) -> u32
  {
    self.cursor as u32
  }

  fn get_byte_block_at_cursor(
    &mut self,
    block_ptr: &mut *const u8,
    token_offset: u32,
    requested_size: u32,
  ) -> u32
  {
    let size = ((self.length as i64) - (token_offset as i64)).max(0);

    if size > 0 {
      let ptr = (self.string.as_ptr() as usize + token_offset as usize) as *const u8;
      *block_ptr = ptr;
      size as u32
    } else {
      *block_ptr = self.string.as_ptr();
      0
    }
  }
}
