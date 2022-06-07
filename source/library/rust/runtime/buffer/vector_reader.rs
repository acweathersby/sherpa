use std::{rc::Rc, sync::Arc};

use crate::{primitives::KernelToken, utf8::get_utf8_code_point_from};

use super::ByteReader;

#[derive(Debug, Clone)]
pub struct UTF8StringReader {
    length: usize,
    cursor: usize,
    line_count: usize,
    line_offset: usize,
    string: Rc<Vec<u8>>,
    word: u32,
    codepoint: u32,
}

impl UTF8StringReader {
    pub fn new(string: Vec<u8>) -> UTF8StringReader {
        let mut reader = UTF8StringReader {
            length: string.len(),
            string: Rc::new(string),
            cursor: 0,
            word: 0,
            line_count: 0,
            line_offset: 0,
            codepoint: 0,
        };
        reader.next(0);
        reader
    }
}

impl ByteReader for UTF8StringReader {
    fn get_source(&self) -> Arc<Vec<u8>> {
        let vec = (*self.string).clone();
        Arc::new(vec)
    }

    fn at_end(&self) -> bool {
        self.cursor >= self.length
    }

    fn set_cursor_to(&mut self, token: &KernelToken) -> bool {
        if self.cursor != token.byte_offset as usize {
            self.cursor = token.byte_offset as usize;
            self.set_line_data(token);
            self.next(0);
        }
        true
    }

    fn set_line_data(&mut self, token: &KernelToken) {
        self.line_count = token.line_number as usize;
        self.line_offset = token.line_offset as usize;
    }

    fn clone(&self) -> Self {
        UTF8StringReader {
            length: self.length,
            string: self.string.clone(),
            cursor: self.cursor,
            word: self.word,
            codepoint: self.codepoint,
            line_count: self.line_count,
            line_offset: self.line_offset,
        }
    }

    fn length(&self) -> u32 {
        self.length as u32
    }

    fn byte(&self) -> u8 {
        self.string[self.cursor]
    }

    fn word(&self) -> u32 {
        self.word
    }

    fn line_offset(&self) -> u32 {
        self.line_offset as u32
    }

    fn line_count(&self) -> u32 {
        self.line_count as u32
    }

    fn codepoint(&self) -> u32 {
        self.codepoint
    }

    fn next(&mut self, amount: u32) {
        self.cursor += amount as usize;
        self.codepoint = 0;

        if self.at_end() {
            return;
        }

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

            let mut word = 0 as u32;

            let mut offset = 32 as u32;

            for i in start..end {
                offset -= 8;
                let byte = self.string[i as usize];
                word |= (byte as u32) << offset;
            }
            for i in
                (self.cursor - amount as usize + 1)..std::cmp::min(self.length, self.cursor + 1)
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
    }

    fn cursor(&self) -> u32 {
        self.cursor as u32
    }
}
