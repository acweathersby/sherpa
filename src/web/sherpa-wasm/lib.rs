use serde::{Deserialize, Serialize};

pub mod grammar;

use grammar::{JSGrammarIdentities, JSSoup};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::{
  build_compile_db,
  compile_parse_states,
  optimize,
  parser,
  Journal,
  SherpaError,
  SherpaResult,
};
use sherpa_rust_runtime::{
  bytecode::{generate_disassembly, ByteCodeParser},
  types::{ParseAction, SherpaParser},
};

use std::{borrow::BorrowMut, cell::RefCell, rc::Rc};
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

/// Temporary simple disassembly implementation.
#[wasm_bindgen]
pub fn create_bytecode_disassembly(
  grammar_id: &JSGrammarIdentities,
  soup: &JSSoup,
) -> Result<String, JsError> {
  let mut j = Journal::new(None);

  j.set_active_report("bytecode compile", sherpa_core::ReportType::Any);

  let gs = soup.0.as_ref();

  let id = grammar_id.0.as_ref().to_owned();

  let SherpaResult::Ok(db) = build_compile_db(j.transfer(), id, gs) else {
    
    j.flush_reports();

    return match j.string_error_report() {
      Some(str) => Result::Err(JsError::new(str.as_str())),
      _ => Result::Err(JsError::new("Failed to build parser_db"))
    };
  };

  let SherpaResult::Ok(parser_states) = compile_parse_states(j.transfer(), &db) else {
    return Result::Err(JsError::new("Failed to compile parser states"));
  };

  let SherpaResult::Ok(parser_states) = optimize::<Vec<_>>(&db, parser_states) else {
    return Result::Err(JsError::new("Failed to compile parser states"));
  };

  let SherpaResult::Ok((bc, _)) = compile_bytecode(&db, parser_states) else {
    return Result::Err(JsError::new("Failed to compile bytecode"));
  };

  Ok(generate_disassembly(&bc))
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

/// A step-able parser
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
      action => {
        //panic!("Unexpected Action! {:?}", action)
        panic!("Unexpected Action!")
      }
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
