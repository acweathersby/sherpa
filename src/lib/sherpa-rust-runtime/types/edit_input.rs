use super::*;
use crate::utf8::{get_token_class_from_codepoint, get_utf8_byte_length_from_code_point};
use std::{ops::Range, rc::Rc, sync::Arc};

struct EditorInsertInput {
  insert:        StringInput,
  insert_offset: usize,
  curr_offset:   usize,
  nodes:         Vec<Rc<CSTNode>>,
}

pub trait EditInput: ParserInput {
  fn non_term() -> Option<Rc<CSTNode>>;
  fn token() -> Option<Rc<CSTNode>>;
}

impl ParserInput for EditorInsertInput {
  fn len(&self) -> usize {
    self.nodes.iter().fold(self.insert.len(), |a, b| a + b.len())
  }

  fn byte(&self, cursor: usize) -> u8 {
    self.insert.byte(cursor)
  }

  fn bytes(&self) -> &[u8] {
    self.insert.bytes()
  }

  fn class(&self, cursor: usize) -> u32 {
    self.insert.class(cursor)
  }

  fn codepoint(&self, cursor: usize) -> u32 {
    self.insert.codepoint(cursor)
  }

  fn codepoint_len(&self, cursor: usize) -> u32 {
    self.insert.codepoint_len(cursor)
  }

  fn get_owned_ref(&self) -> SharedSymbolBuffer {
    self.insert.get_owned_ref()
  }

  fn string_range(&self, range: Range<usize>) -> String {
    self.insert.string_range(range)
  }
}
