use std::io::Read;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::RwLock;

use crate::types::ParseToken;
use crate::utf8::get_utf8_code_point_from;

use super::reader::ImmutCharacterReader;
use super::reader::SharedSymbolBuffer;
use super::ByteCharacterReader;
use super::InputBlock;
use super::LLVMCharacterReader;
use super::MutCharacterReader;
use super::UTF8CharacterReader;

#[derive(Debug, Clone)]
pub struct TestUTF8StringReader<'a>
{
  pub length:      usize,
  pub cursor:      usize,
  pub line_count:  usize,
  pub line_offset: usize,
  pub string:      &'a [u8],
  pub word:        u32,
  pub codepoint:   u32,
}

impl<'a> LLVMCharacterReader for TestUTF8StringReader<'a>
{
  /// Get a pointer to a sequence of bytes that can be read from the input given
  /// the cursor position. The second tuple values should be the length bytes that
  ///  can be read from the block.
  fn get_byte_block_at_cursor<T: ImmutCharacterReader + ByteCharacterReader>(
    self_: &mut T,
    input_block: &mut InputBlock,
  )
  {
    let cursor = input_block.offset;
    let size = ((self_.length() as i64) - (cursor as i64)).max(0);

    if size > 0 {
      let ptr = ((self_.get_bytes().as_ptr() as usize) + cursor as usize) as *const u8;
      input_block.block = ptr;
      input_block.length = size as u32;
      input_block.is_truncated = false;
    } else {
      input_block.block = 0 as *const u8;
      input_block.length = 0;
      input_block.is_truncated = true;
    }
  }
}

impl<'a> ByteCharacterReader for TestUTF8StringReader<'a>
{
  fn get_bytes(&self) -> &[u8]
  {
    self.string
  }
}

impl<'a> UTF8CharacterReader for TestUTF8StringReader<'a> {}

impl<'a> MutCharacterReader for TestUTF8StringReader<'a>
{
  fn next(&mut self, amount: i32) -> u64
  {
    Self::next_utf8(self, amount)
  }

  fn set_cursor(&mut self, cursor: usize)
  {
    self.cursor = cursor;
  }

  fn set_codepoint(&mut self, code_point: u32)
  {
    self.codepoint = code_point;
  }

  fn set_dword(&mut self, dword: u32)
  {
    self.word = dword;
  }

  fn set_line_count(&mut self, line_count: u32)
  {
    self.line_count = line_count as usize;
  }

  fn set_line_offset(&mut self, line_offset: u32)
  {
    self.line_offset = line_offset as usize;
  }
}

impl<'a> ImmutCharacterReader for TestUTF8StringReader<'a>
{
  #[inline(always)]
  fn length(&self) -> usize
  {
    self.length
  }

  #[inline(always)]
  fn byte(&self) -> u32
  {
    self.get_bytes()[self.cursor()] as u32
  }

  #[inline(always)]
  fn dword(&self) -> u32
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

  #[inline(always)]
  fn cursor(&self) -> usize
  {
    self.cursor
  }

  #[inline(always)]
  fn get_source(&self) -> SharedSymbolBuffer
  {
    let vec = self.string.clone();

    SharedSymbolBuffer::new(Vec::from(vec))
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
    let ParseToken { byte_offset, line_number, line_offset, .. } = *token;

    if self.cursor != byte_offset as usize {
      let diff = byte_offset as i32 - self.cursor as i32;

      self.line_count = line_number as usize;

      self.line_offset = line_offset as usize;

      self.next(diff)
    } else {
      self.get_type_info()
    }
  }
}

impl<'a> TestUTF8StringReader<'a>
{
  pub fn from_string(string: &'a str) -> Self
  {
    Self::new(string)
  }

  pub fn new(string: &'a str) -> TestUTF8StringReader<'a>
  {
    let mut reader = TestUTF8StringReader {
      string:      string.as_bytes(),
      length:      string.len(),
      cursor:      0,
      word:        0,
      line_count:  0,
      line_offset: 0,
      codepoint:   0,
    };

    Self::next(&mut reader, 0);

    reader
  }
}
