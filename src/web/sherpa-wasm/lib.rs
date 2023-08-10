use serde::{Deserialize, Serialize};
use sherpa_core::{
  compile_grammar_from_str,
  parser,
  GrammarSoup,
  Journal,
  SherpaError,
  SherpaResult,
};
use sherpa_rust_runtime::{
  bytecode::ByteCodeParser,
  types::{
    bytecode,
    cst,
    ByteReader,
    MutByteReader,
    ParseAction,
    SherpaParseError,
    SherpaParser,
    UTF8Reader,
    UTF8StringReader,
  },
};
use std::{
  borrow::BorrowMut,
  cell::{Cell, RefCell},
  rc::Rc,
};
use wasm_bindgen::prelude::*;

fn window() -> web_sys::Window {
  web_sys::window().expect("no global `window` exists")
}

fn document() -> web_sys::Document {
  window().document().expect("should have a document on window")
}

fn body() -> web_sys::HtmlElement {
  document().body().expect("document should have a body")
}

/// A Handle to a grammar soup.
#[wasm_bindgen]
pub struct JSSoup(Box<GrammarSoup>);

#[wasm_bindgen]
impl JSSoup {
  /// Adds a grammar to the soup, or throw's an error
  /// if the grammar is invalid. Returns the grammar
  /// name if successful.
  pub fn add_grammar(&mut self, grammar: String, path: String) -> Result<String, JsError> {
    let mut j = Journal::new(Default::default());
    match compile_grammar_from_str(&mut j, grammar.as_str(), path.into(), &self.0) {
      SherpaResult::Ok(g_id) => {
        let soup = &self.0;
        let name = soup
          .grammar_headers
          .read()
          .unwrap()
          .get(&g_id)
          .unwrap()
          .identity
          .name
          .to_string(&soup.string_store);
        Ok(name)
      }
      _ => Err(JsError::new("Failed to build grammar")),
    }
  }

  /// Adds a production targeting a specific grammar
  pub fn add_production(&mut self, grammar_name: String) -> Result<(), JsError> {
    Ok(())
  }
}

#[wasm_bindgen]
pub fn create_soup() -> Result<JSSoup, JsError> {
  Ok(JSSoup(Box::new(Default::default())))
}

trait JSONError {
  fn convert_to_js_err(&self) -> Option<String>;
}

impl JSONError for SherpaError {
  fn convert_to_js_err(&self) -> Option<String> {
    match self {
      SherpaError::SourceError { loc, id, msg, .. } => Some(format!(
        r#"{{ "type" : "{id}", "msg": "{}", "start": {}, "end": {} }}"#,
        msg.replace("\"", "\\\"").replace("\n", "\\n"),
        loc.get_tok_range().start_offset(),
        loc.get_tok_range().end_offset()
      )),
      _ => None,
    }
  }
}

#[wasm_bindgen]
pub struct JSGrammarParser {
  _reader:         Rc<RefCell<StringReader::StringReader>>,
  bytecode_parser: ByteCodeParser<'static, StringReader::StringReader, u32>,
}

#[wasm_bindgen]
impl JSGrammarParser {
  pub fn new(input: String) -> Self {
    let mut reader = Rc::new(RefCell::new(StringReader::StringReader::new(input)));
    let other_reader = reader.clone();
    let reader_ptr = reader.borrow_mut();
    Self {
      _reader:         other_reader,
      bytecode_parser: ByteCodeParser::new(unsafe { &mut *reader_ptr.as_ptr() }, &parser::bytecode),
    }
  }

  pub fn init(&mut self) {
    self.bytecode_parser.init_parser(60)
  }

  pub fn next(&mut self) -> JsValue {
    match self.bytecode_parser.get_next_action(&mut None) {
      ParseAction::Accept { production_id } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Accept).unwrap()
      }
      ParseAction::EndOfInput { current_cursor_offset } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::EndOfInput).unwrap()
      }
      ParseAction::Shift { token_byte_offset, token_byte_length, token_id, .. } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Shift { len: token_byte_length }).unwrap()
      }
      ParseAction::Skip { token_byte_offset, token_byte_length, token_id, .. } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Skip { len: token_byte_length }).unwrap()
      }
      ParseAction::Reduce { production_id, rule_id, symbol_count } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Reduce {
          len:     symbol_count,
          prod_id: production_id,
        })
        .unwrap()
      }
      ParseAction::Error { last_production, last_input } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Error).unwrap()
      }
      _ => panic!("Unexpected Action!"),
    }
  }
}
#[wasm_bindgen]
pub fn get_codemirror_parse_tree(input: String) -> JsValue {
  let mut reader = StringReader::StringReader::new(input);
  let mut bytecode_parser = ByteCodeParser::<'static, _, u32>::new(&mut reader, &parser::bytecode);
  bytecode_parser.init_parser(60);

  let mut output = vec![];
  let mut acc_stack: Vec<u32> = vec![];

  const LAST_TOKEN_INDEX: usize = parser::meta::production_names.len();
  loop {
    match bytecode_parser.get_next_action(&mut None) {
      ParseAction::Accept { .. } => {
        break;
      }
      ParseAction::EndOfInput { .. } => {
        break;
      }
      ParseAction::Error { .. } => {
        break;
      }
      ParseAction::Shift { token_byte_offset, token_byte_length, .. } => {
        output.push(LAST_TOKEN_INDEX as u32);
        output.push(token_byte_offset);
        output.push(token_byte_offset + token_byte_length);
        output.push(4);
        acc_stack.push(1);
      }
      ParseAction::Skip { .. } => {}
      ParseAction::Reduce { production_id, symbol_count, .. } => {
        let len = acc_stack.len();
        let mut total_nodes = 0;
        for size in acc_stack.drain(len - symbol_count as usize..) {
          total_nodes += size;
        }
        acc_stack.push(total_nodes + 1);
        let adjust_size = total_nodes as usize * 4;
        let start_offset = output[output.len() - adjust_size + 1];
        let end_offset = output[output.len() - 2];
        output.push(production_id);
        output.push(start_offset);
        output.push(end_offset);
        output.push(adjust_size as u32 + 4);
      }
      action => panic!("Unexpected Action! {:?}", action),
    }
  }

  serde_wasm_bindgen::to_value(&ParseTree(output)).unwrap()
}

#[derive(Serialize, Deserialize)]
struct ParseTree(Vec<u32>);

#[derive(Serialize, Deserialize)]
struct ProdNames(Vec<String>);

#[wasm_bindgen]
pub fn get_production_names() -> JsValue {
  serde_wasm_bindgen::to_value(&ProdNames(
    parser::meta::production_names.iter().map(|i| (*i).to_string()).collect(),
  ))
  .unwrap()
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum JsonParseAction {
  Accept,
  EndOfInput,
  Shift { len: u32 },
  Skip { len: u32 },
  Reduce { len: u32, prod_id: u32 },
  Error,
}

mod StringReader {
  use sherpa_rust_runtime::types::{ByteReader, MutByteReader, SharedSymbolBuffer, UTF8Reader};

  #[derive(Debug, Clone)]
  pub struct StringReader {
    len:      usize,
    cursor:   usize,
    line_num: usize,
    line_off: usize,
    data:     String,
    word:     u32,
    cp:       u32,
    source:   Option<SharedSymbolBuffer>,
  }

  impl From<String> for StringReader {
    fn from(string: String) -> Self {
      Self::new(string)
    }
  }

  impl UTF8Reader for StringReader {
    fn get_str(&self) -> &str {
      self.data.as_str()
    }
  }

  impl MutByteReader for StringReader {
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

  impl ByteReader for StringReader {
    fn get_bytes(&self) -> &[u8] {
      self.data.as_bytes()
    }

    #[inline(always)]
    fn len(&self) -> usize {
      self.len
    }

    #[inline(always)]
    fn byte(&self) -> u32 {
      if self.at_end() {
        0
      } else {
        self.get_bytes()[self.cursor()] as u32
      }
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
    fn get_source(&mut self) -> SharedSymbolBuffer {
      self.source.get_or_insert(SharedSymbolBuffer::new(Vec::from(self.data.clone()))).clone()
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

  impl StringReader {
    pub fn from_string(string: String) -> Self {
      Self::new(string)
    }

    ///
    pub fn new(data: String) -> StringReader {
      let len = data.len();
      StringReader {
        data,
        len,
        cursor: 0,
        word: 0,
        line_num: 0,
        line_off: 0,
        cp: 0,
        source: None,
      }
    }
  }
}
