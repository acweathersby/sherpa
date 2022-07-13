use std::io::Result;
use std::io::Write;

use super::x86_64_writer::X8664Writer;

pub struct NasmWriter<W: Write>
{
  writer: W,
}

impl<W: Write> X8664Writer<W> for NasmWriter<W>
{
  #[inline(always)]
  fn get_writer_mut(&mut self) -> &mut W
  {
    &mut self.writer
  }

  fn into_writer(self) -> W
  {
    self.writer
  }
}

impl<W: Write> NasmWriter<W>
{
  pub fn new(writer: W) -> Self
  {
    Self { writer }
  }
}
