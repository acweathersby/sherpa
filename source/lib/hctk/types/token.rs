use std::fmt;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

use crate::grammar::data::ast::HASH_NAME;

use super::parse_token::ParseToken;

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

#[derive(Clone)]

pub struct Token {
  len:      u32,
  off:      u32,
  line_num: u32,
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
  pub fn blame(&self, max_pre: usize, max_post: usize, inline_comment: &str) -> String {
    fn increment_end(source: &[u8], mut end: usize) -> usize {
      while (end as usize) < source.len() && source[end as usize] as char != '\n' {
        end += 1;
      }
      end
    }

    fn decrement_beg(source: &[u8], mut beg: usize) -> usize {
      while beg > 0 && source[beg] != 10 {
        beg -= 1;
      }

      if source[beg as usize] == 10 {
        beg + 1
      } else {
        beg
      }
    }

    fn create_line(source: &Arc<Vec<u8>>, beg: usize, end: usize, line_number: usize) -> String {
      if let Ok(utf_string) = String::from_utf8(Vec::from(&source[beg..end])) {
        let lines = format!("{}", line_number);

        format!("   {}: {}\n", &lines, utf_string,)
      } else {
        String::from("")
      }
    }

    if let Some(source) = self.input.clone() {
      let leading_newline = source[0] as char == '\n';

      let mut beg = (self.line_off) as usize;
      let mut end = increment_end(&source, beg + 1);
      let mut line_num: usize = (self.line_num + 1) as usize;
      let nl_char_adjust = (self.line_num > 0) as usize;
      let mut i = 0;
      let mut string = String::from("");
      let root = self.off as usize;
      let len = self.len as usize;
      let range = self.get_range();

      let slice = &source[(beg + nl_char_adjust)..end];

      if let Ok(utf_string) = String::from_utf8(Vec::from(slice)) {
        let lines_str = format!("{}", line_num);

        string += &format!(
          "   {}: {}\n{}\n",
          &lines_str,
          utf_string,
          String::from(" ").repeat(lines_str.len() + 3 + (1 - nl_char_adjust) + root - beg)
            + " \u{001b}[31m"
            + &String::from("^").repeat(len)
            + " "
            + inline_comment
            + "\u{001b}[0m",
        );

        let mut beg_root = beg;
        let mut end_root = end;

        if beg_root > 0 {
          for a in 1..(max_pre + 1) {
            if beg_root - 1 == 0 {
              string = String::from("   1:\n") + &string;

              break;
            }

            let end = beg_root;

            beg_root = decrement_beg(&source, beg_root - 2);

            string = create_line(&source, beg_root, end - 1, line_num - a) + &string;
          }
        }

        for a in 1..(max_post + 1) {
          if end_root >= source.len() {
            break;
          }

          let beg = end_root + 1;

          end_root = increment_end(&source, end_root + 1);

          string += &create_line(&source, beg, end_root, line_num + a);
        }
      }

      string
    } else {
      "[Token Source Not Valid]".to_string()
    }
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

  pub fn to_f64(&self) -> f64 {
    if let Ok(result) = self.to_string().parse::<f64>() {
      result
    } else {
      0.0
    }
  }

  pub fn to_f32(&self) -> f32 {
    if let Ok(result) = self.to_string().parse::<f32>() {
      result
    } else {
      0.0
    }
  }

  pub fn to_i32(&self) -> i32 {
    if let Ok(result) = self.to_string().parse::<i32>() {
      result
    } else {
      0
    }
  }

  pub fn to_i64(&self) -> i64 {
    if let Ok(result) = self.to_string().parse::<i64>() {
      result
    } else {
      0
    }
  }
}

impl Default for Token {
  fn default() -> Self {
    Self::new()
  }
}

#[test]
pub fn blame_string_places_cursor_in_correct_position() {
  let mut tok = Token {
    input:    Some(Arc::new("\n test".to_string().as_bytes().to_vec())),
    len:      4,
    off:      2,
    range:    None,
    line_num: 1,
    line_off: 0,
  };

  println!("{}", tok.blame(1, 1, "test"))
}
