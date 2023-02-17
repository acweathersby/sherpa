use sherpa_runtime::types::{
  ByteReader,
  LLVMByteReader,
  MutByteReader,
  SharedSymbolBuffer,
  UTF8Reader,
};

#[derive(Debug, Clone)]
pub struct TestUTF8StringReader<'a> {
  pub len:      usize,
  pub cursor:   usize,
  pub line_num: usize,
  pub line_off: usize,
  pub string:   &'a str,
  pub word:     u32,
  pub cp:       u32,
}

impl<'a> LLVMByteReader for TestUTF8StringReader<'a> {}

impl<'a> UTF8Reader for TestUTF8StringReader<'a> {
  fn get_str(&self) -> &str {
    self.string
  }
}

impl<'a> MutByteReader for TestUTF8StringReader<'a> {
  fn next(&mut self, amount: i32) -> u64 {
    Self::next_utf8(self, amount)
  }

  fn set_cursor(&mut self, cursor: usize) {
    self.cursor = cursor;
  }

  fn set_codepoint(&mut self, code_point: u32) {
    self.cp = code_point;
  }

  fn set_dword(&mut self, dword: u32) {
    self.word = dword;
  }

  fn set_line_count(&mut self, line_count: u32) {
    self.line_num = line_count as usize;
  }

  fn set_line_offset(&mut self, line_offset: u32) {
    self.line_off = line_offset as usize;
  }
}

impl<'a> ByteReader for TestUTF8StringReader<'a> {
  fn get_bytes(&self) -> &[u8] {
    self.string.as_bytes()
  }

  #[inline(always)]
  fn len(&self) -> usize {
    self.len
  }

  #[inline(always)]
  fn byte(&self) -> u32 {
    self.get_bytes()[self.cursor()] as u32
  }

  #[inline(always)]
  fn qword(&self) -> u32 {
    self.word
  }

  #[inline(always)]
  fn line_offset(&self) -> u32 {
    self.line_off as u32
  }

  #[inline(always)]
  fn line_count(&self) -> u32 {
    self.line_num as u32
  }

  #[inline(always)]
  fn codepoint(&self) -> u32 {
    self.cp
  }

  #[inline(always)]
  fn cursor(&self) -> usize {
    self.cursor
  }

  #[inline(always)]
  fn get_source(&self) -> SharedSymbolBuffer {
    let vec = self.string.clone();

    SharedSymbolBuffer::new(Vec::from(vec))
  }

  #[inline(always)]
  fn get_line_data(&self) -> u64 {
    ((self.line_num as u64) << 32) | self.line_off as u64
  }

  #[inline(always)]
  fn get_length_data(&self) -> u64 {
    ((self.codepoint_byte_length() as u64) << 32) | self.codepoint_length() as u64
  }

  #[inline(always)]
  fn set_cursor_to(&mut self, off: usize, line_num: u32, line_off: u32) -> u64 {
    if self.cursor != off {
      let diff = off as i32 - self.cursor as i32;

      self.line_num = line_num as usize;

      self.line_off = line_off as usize;

      self.next(diff)
    } else {
      self.get_type_info()
    }
  }
}

impl<'a> TestUTF8StringReader<'a> {
  pub fn new(string: &'a str) -> TestUTF8StringReader<'a> {
    let mut reader = TestUTF8StringReader {
      string:   string,
      len:      string.len(),
      cursor:   0,
      word:     0,
      line_num: 0,
      line_off: 0,
      cp:       0,
    };

    Self::next(&mut reader, 0);

    reader
  }
}
