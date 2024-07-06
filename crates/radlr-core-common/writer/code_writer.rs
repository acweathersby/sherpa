#![allow(unused)]

use std::{
  io::{Result, Write},
  ops::Add,
};

use crate::{GuardedStr, IString};

/// Chainable writer for formatted source code
pub struct CodeWriter<W: Write> {
  output:        W,
  indent:        usize,
  indent_spaces: usize,
  join:          Option<&'static str>,
}

pub type StringBuffer = CodeWriter<Vec<u8>>;

impl<W: Write + Clone> CodeWriter<W> {
  pub fn get_data(&self) -> W {
    self.output.clone()
  }
}

impl<'w, W: Write> Add<&str> for &'w mut CodeWriter<W> {
  type Output = Self;

  fn add(self, rhs: &str) -> Self::Output {
    self.w(rhs).unwrap()
  }
}

impl<'w, W: Write> Add<&String> for &'w mut CodeWriter<W> {
  type Output = Self;

  fn add(self, rhs: &String) -> Self::Output {
    self.w(rhs).unwrap()
  }
}

impl<'w, W: Write> Add<String> for &'w mut CodeWriter<W> {
  type Output = Self;

  fn add(self, rhs: String) -> Self::Output {
    self.w(&rhs).unwrap()
  }
}

impl<'w, 'istore: 'w, W: Write> Add<GuardedStr<'istore>> for &'w mut CodeWriter<W> {
  type Output = Self;

  fn add(self, rhs: GuardedStr<'istore>) -> Self::Output {
    self.w(rhs.as_str()).unwrap()
  }
}

impl<W: Write> CodeWriter<W> {
  pub fn checkpoint<B: Write + Default>(&self) -> CodeWriter<B> {
    CodeWriter {
      output:        B::default(),
      indent:        self.indent,
      indent_spaces: self.indent_spaces,
      join:          None,
    }
  }
}

impl CodeWriter<Vec<u8>> {
  pub fn to_string(self) -> String {
    String::from_utf8(self.into_output()).unwrap_or_default()
  }
}

impl<W: Write> CodeWriter<W> {
  fn internal_write(&mut self, string: &str) -> Result<()> {
    let indent = " ".repeat(self.indent * self.indent_spaces);

    let mut encountered_first = false;

    if let Some(join) = self.join {
      if !string.trim().is_empty() {
        self.join = None;
        for section in join.split('\n') {
          if encountered_first {
            self.output.write(&[b'\n'])?;
            self.output.write(indent.as_bytes())?;
          }

          encountered_first = true;

          if let Err(err) = self.output.write(section.as_bytes()) {
            return Err(err);
          }
        }
      }
    }

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

  pub fn prime_join(&mut self, join: &'static str) {
    self.join = Some(join)
  }

  /// Sets the number of spaces to use for indentation
  pub fn indent_spaces(&mut self, count: usize) {
    self.indent_spaces = count;
  }

  pub fn default() -> StringBuffer {
    StringBuffer {
      output:        vec![],
      indent:        0,
      indent_spaces: 2,
      join:          None,
    }
  }

  pub fn new(output: W) -> Self {
    CodeWriter { output, indent: 0, indent_spaces: 2, join: None }
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
  pub fn w(&mut self, string: &str) -> Result<&mut Self> {
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

  pub fn merge_checkpoint(&mut self, checkpoint: StringBuffer) -> Result<usize> {
    self.indent = checkpoint.indent;

    self.output.write(&checkpoint.output)
  }

  pub fn into_output(mut self) -> W {
    drop(self.output.flush());

    self.output
  }
}
