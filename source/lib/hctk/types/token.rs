use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use super::parse_token::ParseToken;

#[derive(Clone)]

pub struct Range
{
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

pub struct Token
{
    length:      u32,
    offset:      u32,
    line_number: u32,
    line_offset: u32,
    range:       Option<Range>,
    input:       Option<Arc<Vec<u8>>>,
}

impl fmt::Display for Token
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if self.input.is_some() {
            f.write_str(&self.slice(0, self.length as i32))
        } else {
            f.write_str("")
        }
    }
}

impl fmt::Debug for Token
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let mut bug = f.debug_struct("Token");

        bug.field("length", &self.length)
            .field("offset", &self.offset)
            .field("line_number", &self.line_number)
            .field("line_offset", &self.line_offset);

        if self.input.is_some() {
            bug.field("value", &self.to_string());
        }

        bug.finish()
    }
}

impl Token
{
    pub fn new() -> Token
    {
        Token {
            length:      0,
            offset:      0,
            line_number: 0,
            line_offset: 0,
            range:       None,
            input:       None,
        }
    }

    pub fn from_kernel_token(tok: &ParseToken) -> Token
    {
        Token {
            length:      tok.cp_length,
            offset:      tok.cp_offset,
            line_number: tok.line_number,
            line_offset: tok.line_offset,
            input:       None,
            range:       None,
        }
    }

    pub fn from_range(start: &Token, end: &Token) -> Token
    {
        Token {
            length:      end.offset - start.offset + end.length,
            offset:      start.offset,
            line_number: start.line_number,
            input:       start.input.clone(),
            line_offset: start.line_offset,
            range:       None,
        }
    }

    pub fn empty() -> Token
    {
        Token {
            length:      0u32,
            offset:      0u32,
            line_number: 0u32,
            line_offset: 0u32,
            input:       None,
            range:       None,
        }
    }

    pub fn set_path() {}

    /// Defines the source string for this token. Certain Token
    /// methods will not work correctly if the Token has not been
    /// attached to its source.
    pub fn set_source(&mut self, source: Arc<Vec<u8>>)
    {
        self.input = Some(source);
    }

    pub fn get_line_char(&mut self) -> usize
    {
        if let Some(source) = self.input.clone() {}

        0
    }

    /// Returns the line number of the token location. This may be
    /// zero if the token is not attached to its source.
    pub fn get_line(&mut self) -> usize
    {
        if self.line_number < 1 {
            if let Some(source) = self.input.clone() {
                let mut root = self.offset as usize;
                let mut i = 0;
                let mut lines = 0;

                while i < root {
                    if source[i] == 10 {
                        lines += 1
                    }

                    i += 1
                }

                self.line_number = lines;

                self.get_line()
            } else {
                0
            }
        } else {
            self.line_number as usize
        }
    }

    fn get_slice_range(&self, mut start: i32, mut end: i32) -> (usize, usize)
    {
        use std::cmp::max;
        use std::cmp::min;

        if start < 0 {
            start = max(
                self.offset as i32,
                (self.offset + self.length) as i32 + start,
            )
        } else {
            start = min(
                self.offset as i32 + start,
                (self.offset + self.length) as i32,
            )
        }

        if end < 0 {
            end = max(
                self.offset as i32,
                (self.offset + self.length) as i32 + end,
            )
        } else {
            end = min(
                self.offset as i32 + end,
                (self.offset + self.length) as i32,
            )
        }

        if end < start {
            end = self.offset as i32;

            start = self.offset as i32;
        }

        (start as usize, end as usize)
    }

    /// Creates a diagram of the position of this token within the
    /// source string.
    ///
    /// ### Arguments:
    /// - `max_pre` - The maximum number of lines to render before the
    ///   token line.
    /// - `max_pre` - The maximum number of lines to render after the
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
    ) -> Option<String>
    {
        fn increment_end(source: &[u8], mut end: usize) -> usize
        {
            while (end as usize) < source.len() && source[end as usize] != 10 {
                end += 1;
            }

            end
        }

        fn decrement_beg(source: &[u8], mut beg: usize) -> usize
        {
            while beg > 0 && source[beg] != 10 {
                beg -= 1;
            }

            if source[beg as usize] == 10 {
                beg + 1
            } else {
                beg
            }
        }

        fn create_line(
            source: &Arc<Vec<u8>>,
            beg: usize,
            end: usize,
            line_number: usize,
        ) -> String
        {
            if let Ok(utf_string) =
                String::from_utf8(Vec::from(&source[beg..end]))
            {
                let lines = format!("{}", line_number);

                format!("   {}: {}\n", &lines, utf_string,)
            } else {
                String::from("")
            }
        }

        if let Some(source) = self.input.clone() {
            let mut string = String::from("");

            let root = self.offset as usize;

            let len = self.length as usize;

            let range = self.get_range();

            let mut beg = (self.line_offset) as usize;

            let mut end = (self.line_offset + 1) as usize;

            let mut i = 0;

            let mut lines: usize = (self.line_number + 1) as usize;

            end = increment_end(&source, end);

            let slice = &source[(beg + 1)..end];

            if let Ok(utf_string) = String::from_utf8(Vec::from(slice)) {
                {
                    let lines = format!("{}", lines);

                    string += &format!(
                        "   {}: {}\n{}\n",
                        &lines,
                        utf_string,
                        String::from(" ").repeat(lines.len() + 3 + root - beg)
                            + " \u{001b}[31m"
                            + &String::from("^").repeat(len)
                            + " "
                            + inline_comment
                            + "\u{001b}[0m",
                    );
                }

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

                        string =
                            create_line(&source, beg_root, end - 1, lines - a)
                                + &string;
                    }
                }

                for a in 1..(max_post + 1) {
                    if end_root >= source.len() {
                        break;
                    }

                    let beg = end_root + 1;

                    end_root = increment_end(&source, end_root + 1);

                    string += &create_line(&source, beg, end_root, lines + a);
                }
            }

            Some(string)
        } else {
            None
        }
    }

    pub fn len(&self) -> usize
    {
        self.length as usize
    }

    pub fn is_empty(&self) -> bool
    {
        self.length == 0
    }

    pub fn to_length(&self, length: usize) -> Self
    {
        Self {
            length:      length as u32,
            offset:      self.offset,
            line_number: self.line_number,
            line_offset: self.line_offset,
            range:       None,
            input:       self.input.clone(),
        }
    }

    pub fn get_range(&self) -> Range
    {
        if let Some(source) = self.input.clone() {
            let start_line = self.line_number + 1;

            let start_column: u32 = self.offset - self.line_offset + 1;

            let mut end_line = start_line;

            let mut end_column = start_column as u32;

            for i in self.offset..(self.offset + self.length) {
                if source[i as usize] == 10 {
                    end_line += 1;

                    end_column = 0;
                }

                end_column += 1;
            }

            Range {
                start_line,
                end_line,
                start_column,
                end_column,
            }
        } else {
            Range {
                end_column:   self.offset - self.line_offset + 1 + self.length,
                start_column: self.offset - self.line_offset + 1,
                end_line:     self.line_number + 1,
                start_line:   self.line_number + 1,
            }
        }
    }

    fn get_range_mut(&mut self) -> Range
    {
        if let Some(range) = &self.range {
            range.to_owned()
        } else {
            let range = self.get_range_mut();

            self.range = Some(range.to_owned());

            range
        }
    }

    fn slice(&self, start: i32, end: i32) -> String
    {
        if let Some(input) = &self.input {
            let (adjusted_start, adjusted_end) =
                self.get_slice_range(start, end);

            unsafe {
                String::from_utf8_unchecked(Vec::from(
                    &input[adjusted_start..adjusted_end],
                ))
            }
        } else {
            String::default()
        }
    }

    pub fn to_f64(&self) -> f64
    {
        if let Ok(result) = self.to_string().parse::<f64>() {
            result
        } else {
            0.0
        }
    }

    pub fn to_f32(&self) -> f32
    {
        if let Ok(result) = self.to_string().parse::<f32>() {
            result
        } else {
            0.0
        }
    }

    pub fn to_i32(&self) -> i32
    {
        if let Ok(result) = self.to_string().parse::<i32>() {
            result
        } else {
            0
        }
    }

    pub fn to_i64(&self) -> i64
    {
        if let Ok(result) = self.to_string().parse::<i64>() {
            result
        } else {
            0
        }
    }
}

impl Default for Token
{
    fn default() -> Self
    {
        Self::new()
    }
}
