use std::fmt;
use std::hash::Hash;
use std::ops::Add;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use crate::grammar::data::ast::HASH_NAME;

use super::parse_token::ParseToken;

/// Simple wrapper for source data and system specific origin path.
pub struct TokenSource {
  origin_path: PathBuf,
  data:        Vec<u8>,
}

pub type SharedTokenSource = Arc<TokenSource>;

#[derive(Clone)]
pub struct Range {
  /// The line number at which the range starts
  pub start_line:   u32,
  /// The line number at which the range ends
  pub end_line:     u32,
  /// The column of the first character in the range
  pub start_column: u32,
  /// The column following the last character in the range
  pub end_column:   u32,
}

/// Stores color setting code for terminal text coloring of token blame strings.
#[derive(Debug, Clone, Copy)]
pub struct BlameColor {
  highlight: &'static str,
  reset:     &'static str,
}

impl BlameColor {
  /// Sets the color of the highlight to red
  pub const Red: Option<BlameColor> = Some(BlameColor::new("\u{001b}[31m", "\u{001b}[0m"));

  pub const fn new(highlight: &'static str, reset: &'static str) -> BlameColor {
    Self { highlight, reset }
  }
}

#[derive(Clone)]
pub struct Token {
  len:      u32,
  /// The byte offset
  off:      u32,
  /// The 0th indexed line number at which this Token resides
  line_num: u32,
  /// The number of characters following the last encountered
  /// [\n] up to the current offset.
  line_off: u32,
  range:    Option<Range>,
  input:    Option<Arc<Vec<u8>>>,
}

impl Hash for Token {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.line_num.hash(state);
    self.line_off.hash(state);
    self.off.hash(state);
    self.len.hash(state);
    state.finish();
  }
}

impl PartialEq for Token {
  fn eq(&self, other: &Self) -> bool {
    matches!(self.cmp(other), std::cmp::Ordering::Equal)
  }
}

impl PartialOrd for Token {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl Eq for Token {
  fn assert_receiver_is_total_eq(&self) {}
}

impl Ord for Token {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    match self.off == other.off {
      true => match self.len == other.len {
        true => std::cmp::Ordering::Equal,
        false => match self.len < other.len {
          true => std::cmp::Ordering::Less,
          false => std::cmp::Ordering::Greater,
        },
      },
      false => match self.off < other.off {
        true => std::cmp::Ordering::Less,
        false => std::cmp::Ordering::Greater,
      },
    }
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.input.is_some() {
      f.write_str(&self.slice(0, self.len as i32))
    } else {
      f.write_str("")
    }
  }
}

impl fmt::Debug for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut bug = f.debug_struct("Token");

    bug
      .field("length", &self.len)
      .field("offset", &self.off)
      .field("line_number", &self.line_num)
      .field("line_offset", &self.line_off);

    if self.input.is_some() {
      bug.field("value", &self.to_string());
    }

    bug.finish()
  }
}

impl Add for &Token {
  type Output = Token;

  fn add(self, rhs: Self) -> Self::Output {
    let (min, max) = if self < rhs { (self, rhs) } else { (rhs, self) };

    Token {
      len:      max.off - min.off + max.len,
      off:      min.off,
      line_num: min.line_num,
      line_off: min.line_off,
      range:    None,
      input:    self.input.clone(),
    }
  }
}

impl Token {
  pub fn new() -> Token {
    Token {
      len:      0,
      off:      0,
      line_num: 0,
      line_off: 0,
      range:    None,
      input:    None,
    }
  }

  pub fn from_parse_token(tok: &ParseToken) -> Token {
    Token {
      len:      tok.cp_length,
      off:      tok.cp_offset,
      line_num: tok.line_number,
      line_off: tok.line_offset,
      input:    None,
      range:    None,
    }
  }

  pub fn from_range(start: &Token, end: &Token) -> Token {
    Token {
      len:      end.off - start.off + end.len,
      off:      start.off,
      line_num: start.line_num,
      input:    start.input.clone(),
      line_off: start.line_off,
      range:    None,
    }
  }

  pub fn empty() -> Token {
    Token {
      len:      0,
      off:      0,
      line_num: 0,
      line_off: 0,
      input:    None,
      range:    None,
    }
  }

  pub fn set_path() {}

  /// Defines the source string for this token. Certain Token
  /// methods will not work correctly if the Token has not been
  /// attached to its source.
  pub fn set_source(&mut self, source: Arc<Vec<u8>>) {
    self.input = Some(source);
  }

  pub fn get_line_char(&mut self) -> usize {
    if let Some(source) = self.input.clone() {}

    0
  }

  /// Returns the line number of the token location. This may be
  /// zero if the token is not attached to its source.
  pub fn get_line(&mut self) -> usize {
    if self.line_num < 1 {
      if let Some(source) = self.input.clone() {
        let mut root = self.off as usize;
        let mut i = 0;
        let mut lines = 0;

        while i < root {
          if source[i] == 10 {
            lines += 1
          }

          i += 1
        }

        self.line_num = lines;

        self.get_line()
      } else {
        0
      }
    } else {
      self.line_num as usize
    }
  }

  fn get_slice_range(&self, mut start: i32, mut end: i32) -> (usize, usize) {
    use std::cmp::max;
    use std::cmp::min;

    if start < 0 {
      start = max(self.off as i32, (self.off + self.len) as i32 + start)
    } else {
      start = min(self.off as i32 + start, (self.off + self.len) as i32)
    }

    if end < 0 {
      end = max(self.off as i32, (self.off + self.len) as i32 + end)
    } else {
      end = min(self.off as i32 + end, (self.off + self.len) as i32)
    }

    if end < start {
      end = self.off as i32;

      start = self.off as i32;
    }

    (start as usize, end as usize)
  }

  pub fn len(&self) -> usize {
    self.len as usize
  }

  pub fn is_empty(&self) -> bool {
    self.len == 0
  }

  pub fn to_length(&self, length: usize) -> Self {
    Self {
      len:      length as u32,
      off:      self.off,
      line_num: self.line_num,
      line_off: self.line_off,
      range:    None,
      input:    self.input.clone(),
    }
  }

  pub fn get_range(&self) -> Range {
    if let Some(source) = self.input.clone() {
      let start_line = self.line_num + 1;

      let start_column: u32 = self.off - self.line_off + 1;

      let mut end_line = start_line;

      let mut end_column = start_column as u32;

      for i in self.off..(self.off + self.len) {
        if source[i as usize] == 10 {
          end_line += 1;

          end_column = 0;
        }

        end_column += 1;
      }

      Range { start_line, end_line, start_column, end_column }
    } else {
      Range {
        end_column:   self.off - self.line_off + 1 + self.len,
        start_column: self.off - self.line_off + 1,
        end_line:     self.line_num + 1,
        start_line:   self.line_num + 1,
      }
    }
  }

  fn get_range_mut(&mut self) -> Range {
    if let Some(range) = &self.range {
      range.to_owned()
    } else {
      let range = self.get_range_mut();

      self.range = Some(range.to_owned());

      range
    }
  }

  fn slice(&self, start: i32, end: i32) -> String {
    if let Some(input) = &self.input {
      let (adjusted_start, adjusted_end) = self.get_slice_range(start, end);

      unsafe { String::from_utf8_unchecked(Vec::from(&input[adjusted_start..adjusted_end])) }
    } else {
      String::default()
    }
  }

  pub fn to_f32(&self) -> f32 {
    self.to_numeric_or_length() as f32
  }

  pub fn to_f64(&self) -> f64 {
    self.to_numeric_or_length() as f64
  }

  pub fn to_i8(&self) -> i8 {
    self.to_numeric_or_length() as i8
  }

  pub fn to_i16(&self) -> i16 {
    self.to_numeric_or_length() as i16
  }

  pub fn to_i32(&self) -> i32 {
    self.to_numeric_or_length() as i32
  }

  pub fn to_i64(&self) -> i64 {
    self.to_numeric_or_length() as i64
  }

  pub fn to_u8(&self) -> u8 {
    self.to_numeric_or_length() as u8
  }

  pub fn to_u16(&self) -> u16 {
    self.to_numeric_or_length() as u16
  }

  pub fn to_u32(&self) -> u32 {
    self.to_numeric_or_length() as u32
  }

  pub fn to_u64(&self) -> u64 {
    self.to_numeric_or_length() as u64
  }

  pub fn to_numeric_or_length(&self) -> f64 {
    match self.to_string().parse::<f64>() {
      Ok(num) => num,
      Err(_) => self.len as f64,
    }
  }

  /// Creates a diagram of the position of this token within the
  /// source string.
  ///
  /// ### Arguments:
  /// - `max_pre` - The maximum number of lines to render before the
  ///   token line.
  /// - `max_post` - The maximum number of lines to render after the
  ///   token line.
  ///
  /// ### Returns:
  /// - `Option<String>` - A `String` of the blame diagram or `None`
  ///   if source is
  /// not defined.
  pub fn blame(
    &self,
    max_pre: usize,
    max_post: usize,
    inline_comment: &str,
    colors: Option<BlameColor>,
  ) -> String {
    fn find_next_line(source: &[u8], mut line: i64) -> i64 {
      line += 1;
      while (line as usize) < source.len() && source[line as usize] as char != '\n' {
        line += 1;
      }
      line
    }

    fn find_prev_line(source: &[u8], mut line: i64) -> i64 {
      line -= 1;
      while line >= 0 && source[line as usize] as char != '\n' {
        line -= 1;
      }

      line
    }

    fn create_line(
      source: &Arc<Vec<u8>>,
      prev_line: i64,
      next_line: i64,
      line_number: usize,
    ) -> String {
      if let Ok(utf_string) =
        String::from_utf8(Vec::from(&source[(prev_line + 1) as usize..next_line as usize]))
      {
        format!("{: >4}: {}\n", line_number, utf_string,)
      } else {
        String::from("")
      }
    }

    if let Some(source) = self.input.clone() {
      let mut string = String::from("");
      let mut prev_line = (self.line_off) as i64;
      let mut line_num = (self.line_num + 1) as usize;

      if source[0] as char == '\n' {
        line_num -= 1;
      } else if prev_line == 0 {
        prev_line -= 1;
      }

      let mut next_line = find_next_line(&source, prev_line);

      if let Ok(utf_string) =
        String::from_utf8(Vec::from(&source[(prev_line + 1) as usize..next_line as usize]))
      {
        let lines_str = format!("{: >4}", line_num);

        string += &format!(
          "{}: {}\n{}\n",
          &lines_str,
          utf_string,
          String::from(" ").repeat(4 + 1 + self.off as usize - (prev_line + 1) as usize)
            + &if let Some(BlameColor { highlight, reset }) = colors {
              " ".to_string()
                + highlight
                + &String::from("^").repeat(self.len as usize)
                + " "
                + inline_comment
                + reset
            } else {
              " ".to_string() + &String::from("^").repeat(self.len as usize) + " " + inline_comment
            },
        );

        if prev_line > 0 {
          for a in 1..=max_pre {
            if prev_line <= 0 {
              break;
            }
            let next_line = prev_line;
            prev_line = find_prev_line(&source, prev_line);
            string = create_line(&source, prev_line, next_line, line_num - a) + &string;
          }
        }

        for a in 1..=max_post {
          if next_line as usize >= source.len() {
            break;
          }
          let prev_line = next_line;
          next_line = find_next_line(&source, next_line);
          string += &create_line(&source, prev_line as i64, next_line, line_num + a);
        }
      }

      string
    } else {
      "[Token Source Not Valid]".to_string()
    }
  }
}

impl Default for Token {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod test {
  use crate::types::BlameColor;
  use crate::types::Token;
  use std::sync::Arc;

  #[test]
  pub fn blame_string_places_cursor_in_correct_position() {
    let mut tok = Token {
      input:    Some(Arc::new("\n start \n\n test \n final ".to_string().as_bytes().to_vec())),
      len:      4,
      off:      11,
      range:    None,
      line_num: 3,
      line_off: 9,
    };

    let blame_string = tok.blame(2, 1, "test", None);
    let lines = blame_string.split("\n").collect::<Vec<_>>();

    println!("{}", blame_string);

    assert_eq!(lines.len(), 6);

    assert_eq!(lines[0], "   1:  start ");
    assert_eq!(lines[1], "   2: ");
    assert_eq!(lines[2], "   3:  test ");
    assert_eq!(lines[3], "       ^^^^ test");
    assert_eq!(lines[4], "   4:  final ");
  }

  #[test]
  pub fn blame_string_places_cursor_in_correct_position2() {
    let mut tok = Token {
      input:    Some(Arc::new(" start \n\n test \n final ".to_string().as_bytes().to_vec())),
      len:      5,
      off:      1,
      range:    None,
      line_num: 0,
      line_off: 0,
    };

    let blame_string = tok.blame(2, 3, "start", None);
    let lines = blame_string.split("\n").collect::<Vec<_>>();

    assert_eq!(lines.len(), 6);

    assert_eq!(lines[0], "   1:  start ");
    assert_eq!(lines[1], "       ^^^^^ start");
    assert_eq!(lines[2], "   2: ");
    assert_eq!(lines[3], "   3:  test ");
    assert_eq!(lines[4], "   4:  final ");
  }
}
