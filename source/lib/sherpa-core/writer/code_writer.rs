use std::io::{Result, Write};

/// Chainable writer for formatted source code
pub struct CodeWriter<W: Write> {
  output:        W,
  indent:        usize,
  indent_spaces: usize,
}

pub type StringBuffer = CodeWriter<Vec<u8>>;

impl<W: Write> CodeWriter<W> {
  fn internal_write(&mut self, string: &str) -> Result<()> {
    let indent = " ".repeat(self.indent * self.indent_spaces);

    let mut encountered_first = false;

    for section in string.split('\n') {
      if encountered_first {
        self.output.write(&[b'\n'])?;
        self.output.write(indent.as_bytes())?;
      }

      encountered_first = true;

      if let Err(err) = self.output.write(section.as_bytes()) {
        return Err(err);
      }
    }
    Ok(())
  }

  pub fn indent_spaces(&mut self, count: usize) {
    self.indent_spaces = count;
  }

  pub fn default() -> StringBuffer {
    StringBuffer { output: vec![], indent: 0, indent_spaces: 2 }
  }

  pub fn new(output: W) -> Self {
    CodeWriter { output, indent: 0, indent_spaces: 2 }
  }

  pub fn indent(&mut self) -> &mut Self {
    self.increase_indent();
    self
  }

  pub fn dedent(&mut self) -> &mut Self {
    self.decrease_indent();
    self
  }

  pub fn newline(&mut self) -> Result<&mut Self> {
    self.insert_newline()?;
    Ok(self)
  }

  /// Chainable shorthand for `write_line`
  pub fn wrtln(&mut self, string: &str) -> Result<&mut Self> {
    self.write_line(string)?;
    Ok(self)
  }

  /// Chainable shorthand for `write`
  pub fn wrt(&mut self, string: &str) -> Result<&mut Self> {
    self.write(string)?;
    Ok(self)
  }

  pub fn increase_indent(&mut self) {
    self.indent += 1
  }

  pub fn decrease_indent(&mut self) {
    self.indent -= 1;
  }

  pub fn insert_newline(&mut self) -> Result<()> {
    self.internal_write("\n")
  }

  pub fn write_line(&mut self, string: &str) -> Result<()> {
    self.insert_newline()?;
    self.internal_write(string)
  }

  pub fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> Result<()> {
    if let Some(string) = fmt.as_str() {
      self.internal_write(string)
    } else {
      self.internal_write(&fmt.to_string())
    }
  }

  pub fn write(&mut self, string: &str) -> Result<()> {
    self.internal_write(string)
  }

  pub fn checkpoint(&self) -> StringBuffer {
    let mut string_buffer = Self::default();

    string_buffer.indent = self.indent;

    string_buffer
  }

  pub fn merge_checkpoint(&mut self, checkpoint: StringBuffer) -> Result<usize> {
    self.indent = checkpoint.indent;

    self.output.write(&checkpoint.output)
  }

  pub fn into_output(mut self) -> W {
    drop(self.output.flush());

    self.output
  }
}
