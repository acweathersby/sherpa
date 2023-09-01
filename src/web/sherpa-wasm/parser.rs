use crate::grammar::{JSBytecode, JSParserDB};
use serde::{Deserialize, Serialize};
use sherpa_core::parser;
use sherpa_rust_runtime::{
  bytecode::{ByteCodeParser, DebugEvent, DebugFn},
  types::{ParseAction, SherpaParser},
};
use std::{
  borrow::BorrowMut,
  cell::RefCell,
  rc::Rc,
  sync::{LockResult, RwLock},
};
use wasm_bindgen::prelude::*;

/// A stepable parser for the sherpa grammar
#[wasm_bindgen]
pub struct JSGrammarParser {
  _reader:         Rc<RefCell<string_reader::StringReader>>,
  bytecode_parser: ByteCodeParser<'static, string_reader::StringReader, u32>,
}

#[wasm_bindgen]
impl JSGrammarParser {
  pub fn new(input: String) -> Self {
    let mut reader = Rc::new(RefCell::new(string_reader::StringReader::new(input)));
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
      ParseAction::Accept { .. } => serde_wasm_bindgen::to_value(&JsonParseAction::Accept).unwrap(),
      ParseAction::EndOfInput { .. } => serde_wasm_bindgen::to_value(&JsonParseAction::EndOfInput).unwrap(),
      ParseAction::Shift { token_byte_length, .. } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Shift { len: token_byte_length }).unwrap()
      }
      ParseAction::Skip { token_byte_length, .. } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Skip { len: token_byte_length }).unwrap()
      }
      ParseAction::Reduce { nonterminal_id, symbol_count, .. } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Reduce { len: symbol_count, nterm: nonterminal_id }).unwrap()
      }
      ParseAction::Error { .. } => serde_wasm_bindgen::to_value(&JsonParseAction::Error).unwrap(),
      _ => panic!("Unexpected Action!"),
    }
  }
}

/// A step-able parser
#[wasm_bindgen]
pub struct JSByteCodeParser {
  running:         bool,
  _bytecode:       Rc<RefCell<Vec<u8>>>,
  _reader:         Rc<RefCell<string_reader::StringReader>>,
  bytecode_parser: ByteCodeParser<'static, string_reader::StringReader, u32>,
}

#[wasm_bindgen]
impl JSByteCodeParser {
  pub fn new(input: String, bytecode: &JSBytecode) -> Self {
    let mut reader = Rc::new(RefCell::new(string_reader::StringReader::new(input)));
    let other_reader = reader.clone();
    let reader_ptr = reader.borrow_mut();

    let mut bytecode = Rc::new(RefCell::new(bytecode.0 .0.clone()));
    let other_bytecode = bytecode.clone();
    let bytecode_ptr = bytecode.borrow_mut();

    Self {
      running:         false,
      _reader:         other_reader,
      _bytecode:       other_bytecode,
      bytecode_parser: ByteCodeParser::new(unsafe { &mut *reader_ptr.as_ptr() }, unsafe { &*bytecode_ptr.as_ptr() }),
    }
  }

  pub fn init(&mut self, entry_name: String, bytecode: &JSBytecode, db: &JSParserDB) {
    let db = db.0.as_ref().get_db();

    let offset = db.get_entry_offset(&entry_name, &bytecode.0 .1).expect("Could not find entry point");

    self.bytecode_parser.init_parser(offset as u32);

    self.running = true;
  }

  pub fn next(&mut self) -> JsValue {
    if !self.running {
      return JsValue::UNDEFINED;
    }
    let values = Rc::new(RwLock::new(vec![]));

    {
      let v = values.clone();

      let mut debugger: Option<Box<DebugFn>> = Some(Box::new(move |e: &DebugEvent<'_>, _| {
        if let LockResult::Ok(mut values) = v.write() {
          values.push(JSDebugEvent::from(e));
        }
      }));

      match self.bytecode_parser.get_next_action(&mut debugger.as_deref_mut()) {
        ParseAction::Accept { .. } => {
          self.running = false;
          serde_wasm_bindgen::to_value(&JsonParseAction::Accept).unwrap()
        }
        ParseAction::EndOfInput { .. } => serde_wasm_bindgen::to_value(&JsonParseAction::EndOfInput).unwrap(),
        ParseAction::Shift { token_byte_offset, token_byte_length, .. } => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::from(JSDebugEvent::ShiftToken {
              offset_end:   (token_byte_offset + token_byte_length) as usize,
              offset_start: token_byte_offset as usize,
            }));
          }
          serde_wasm_bindgen::to_value(&JsonParseAction::Shift { len: token_byte_length }).unwrap()
        }
        ParseAction::Skip { token_byte_length, .. } => {
          serde_wasm_bindgen::to_value(&JsonParseAction::Skip { len: token_byte_length }).unwrap()
        }
        ParseAction::Reduce { nonterminal_id, symbol_count, .. } => {
          serde_wasm_bindgen::to_value(&JsonParseAction::Reduce { len: symbol_count, nterm: nonterminal_id }).unwrap()
        }
        ParseAction::Error { .. } => {
          self.running = false;
          serde_wasm_bindgen::to_value(&JsonParseAction::Error).unwrap()
        }
        _ => panic!("Unexpected Action!"),
      }
    };

    let val = unsafe { Rc::try_unwrap(values).unwrap_unchecked().into_inner().expect("to work").into_iter().collect::<Vec<_>>() };

    serde_wasm_bindgen::to_value(&val).unwrap()
  }
}

#[wasm_bindgen]
pub fn get_codemirror_parse_tree(input: String) -> JsValue {
  let mut reader = string_reader::StringReader::new(input);
  let mut bytecode_parser: ByteCodeParser<'_, string_reader::StringReader, u32> =
    ByteCodeParser::<'static, _, u32>::new(&mut reader, &parser::bytecode);
  bytecode_parser.init_parser(46600);

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
      ParseAction::Reduce { nonterminal_id, symbol_count, .. } => {
        let len = acc_stack.len();
        let mut total_nodes = 0;
        for size in acc_stack.drain(len - symbol_count as usize..) {
          total_nodes += size;
        }
        acc_stack.push(total_nodes + 1);
        let adjust_size = total_nodes as usize * 4;
        let start_offset = output[output.len() - adjust_size + 1];
        let end_offset = output[output.len() - 2];
        output.push(nonterminal_id);
        output.push(start_offset);
        output.push(end_offset);
        output.push(adjust_size as u32 + 4);
      }
      _ => {
        //panic!("Unexpected Action! {:?}", action)
        panic!("Unexpected Action!")
      }
    }
  }

  serde_wasm_bindgen::to_value(&ParseTree(output)).unwrap()
}

#[wasm_bindgen]
pub fn get_nonterminal_names() -> JsValue {
  serde_wasm_bindgen::to_value(&ProdNames(parser::meta::production_names.iter().map(|i| (*i).to_string()).collect())).unwrap()
}

#[derive(Serialize, Deserialize)]
struct ParseTree(Vec<u32>);

#[derive(Serialize, Deserialize)]
struct ProdNames(Vec<String>);

#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum JsonParseAction {
  Accept,
  EndOfInput,
  Shift { len: u32 },
  Skip { len: u32 },
  Reduce { len: u32, nterm: u32 },
  Error,
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum JSDebugEvent {
  ExecuteState {
    base_instruction: usize,
  },
  ExecuteInstruction {
    instruction: u32,
    is_scanner:  bool,
    end_ptr:     usize,
    head_ptr:    usize,
    scan_ptr:    usize,
    base_ptr:    usize,
    anchor_ptr:  usize,
    tok_len:     usize,
    tok_id:      u32,
    sym_len:     u32,
  },
  SkipToken {
    offset_start: usize,
    offset_end:   usize,
  },
  ShiftToken {
    offset_start: usize,
    offset_end:   usize,
  },
  ByteValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  CodePointValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  ClassValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  TokenValue {
    input_value: u32,
    start:       usize,
    end:         usize,
  },
  GotoValue {
    nonterminal_id: u32,
  },
  Reduce {
    rule_id: u32,
  },
  Complete {
    nonterminal_id: u32,
  },
  Failure {},
  EndOfFile,
}

impl<'a> From<&DebugEvent<'a>> for JSDebugEvent {
  fn from(value: &DebugEvent<'a>) -> Self {
    match *value {
      DebugEvent::ExecuteState { base_instruction } => {
        JSDebugEvent::ExecuteState { base_instruction: base_instruction.address() }
      }
      DebugEvent::ExecuteInstruction {
        instruction,
        is_scanner,
        end_ptr,
        head_ptr,
        scan_ptr,
        base_ptr,
        anchor_ptr,
        tok_len,
        tok_id,
        sym_len,
      } => JSDebugEvent::ExecuteInstruction {
        instruction: instruction.address() as u32,
        is_scanner,
        end_ptr,
        head_ptr,
        scan_ptr,
        base_ptr,
        anchor_ptr,
        tok_len,
        tok_id,
        sym_len,
      },
      DebugEvent::SkipToken { offset_start, offset_end } => JSDebugEvent::SkipToken { offset_start, offset_end },
      DebugEvent::ShiftToken { offset_start, offset_end, .. } => JSDebugEvent::ShiftToken { offset_start, offset_end },
      DebugEvent::ByteValue { input_value, start, end } => JSDebugEvent::ByteValue { input_value, start, end },
      DebugEvent::CodePointValue { input_value, start, end } => JSDebugEvent::CodePointValue { input_value, start, end },
      DebugEvent::ClassValue { input_value, start, end } => JSDebugEvent::ClassValue { input_value, start, end },
      DebugEvent::TokenValue { input_value, start, end } => JSDebugEvent::TokenValue { input_value, start, end },
      DebugEvent::GotoValue { nonterminal_id } => JSDebugEvent::GotoValue { nonterminal_id },
      DebugEvent::Reduce { rule_id } => JSDebugEvent::Reduce { rule_id },
      DebugEvent::Complete { nonterminal_id } => JSDebugEvent::Complete { nonterminal_id },
      DebugEvent::Failure {} => JSDebugEvent::Failure {},
      DebugEvent::EndOfFile => JSDebugEvent::EndOfFile,
    }
  }
}

mod string_reader {
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
