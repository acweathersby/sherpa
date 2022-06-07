use std::{path::PathBuf, rc::Rc, sync::Arc};

use crate::{primitives::KernelToken, utf8::get_utf8_code_point_from};

use super::ByteReader;

#[derive(Debug, Clone)]
pub struct FileReader {
    cursor: usize,
    line_count: usize,
    line_offset: usize,
    word: u32,
    codepoint: u32,
}

impl FileReader {
    pub fn new(absolute_file_path: &PathBuf) -> FileReader {
        let mut reader = FileReader {
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

impl ByteReader for FileReader {
    fn get_source(&self) -> Arc<Vec<u8>> {
        let vec = vec![];
        Arc::new(vec)
    }

    fn at_end(&self) -> bool {
        true
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
        FileReader {
            cursor: self.cursor,
            word: self.word,
            codepoint: self.codepoint,
            line_count: self.line_count,
            line_offset: self.line_offset,
        }
    }

    fn length(&self) -> u32 {
        0
    }

    fn byte(&self) -> u8 {
        0
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

    fn next(&mut self, amount: u32) {}

    fn cursor(&self) -> u32 {
        self.cursor as u32
    }
}
