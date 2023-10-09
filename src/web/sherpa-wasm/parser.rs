use crate::grammar::{JSBytecodePackage, JSParserDB};
use serde::{Deserialize, Serialize};
use sherpa_core::parser;
use sherpa_rust_runtime::types::*;

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
  bytecode_parser: ByteCodeParser<string_reader::StringReader, u32, &'static [u8]>,
}

#[wasm_bindgen]
impl JSGrammarParser {
  pub fn new(input: String) -> Self {
    let mut reader = Rc::new(RefCell::new(string_reader::StringReader::new(input)));
    let other_reader = reader.clone();
    let reader_ptr = reader.borrow_mut();
    Self {
      _reader:         other_reader,
      bytecode_parser: ByteCodeParser::new(unsafe { &mut *reader_ptr.as_ptr() }, parser::bytecode.as_slice()),
    }
  }

  pub fn init(&mut self) {
    self.bytecode_parser.init_parser(60)
  }

  pub fn next(&mut self) -> JsValue {
    match self.bytecode_parser.get_next_action(&mut None) {
      ParseAction::Accept { .. } => serde_wasm_bindgen::to_value(&JsonParseAction::Accept).unwrap(),
      ParseAction::EndOfInput { .. } => serde_wasm_bindgen::to_value(&JsonParseAction::EndOfInput).unwrap(),
      ParseAction::Shift { byte_length: token_byte_length, .. } => {
        serde_wasm_bindgen::to_value(&JsonParseAction::Shift { len: token_byte_length }).unwrap()
      }
      ParseAction::Skip { byte_length: token_byte_length, .. } => {
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
  _bytecode:       JSBytecodePackage,
  _reader:         Rc<RefCell<string_reader::StringReader>>,
  bytecode_parser: ByteCodeParser<string_reader::StringReader, u32, JSBytecodePackage>,
}

#[wasm_bindgen]
impl JSByteCodeParser {
  pub fn new(input: String, bytecode: &JSBytecodePackage) -> Self {
    let mut reader = Rc::new(RefCell::new(string_reader::StringReader::new(input)));
    let other_reader = reader.clone();
    let reader_ptr = reader.borrow_mut();
    Self {
      running:         false,
      _reader:         other_reader,
      _bytecode:       bytecode.clone(),
      bytecode_parser: ByteCodeParser::new(unsafe { &mut *reader_ptr.as_ptr() }, bytecode.clone()),
    }
  }

  pub fn init(&mut self, entry_name: String, bytecode: &JSBytecodePackage, db: &JSParserDB) {
    let db = db.0.as_ref().get_db();

    let offset = db.get_entry_offset(&entry_name, &bytecode.0.state_name_to_address).expect("Could not find entry point");

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

      let mut debugger: Option<Box<DebugFn<_, _>>> = Some(Box::new(move |e, ctx| {
        if let LockResult::Ok(mut values) = v.write() {
          values.push(JSDebugEvent::from((e, ctx)));
        }
      }));

      match self.bytecode_parser.get_next_action(&mut debugger.as_deref_mut()) {
        ParseAction::Accept { nonterminal_id, .. } => {
          self.running = false;
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::Complete { nonterminal_id, ctx: self.bytecode_parser.get_ctx().into() });
          }
        }
        ParseAction::EndOfInput { .. } => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::EndOfFile {});
          }
        }
        ParseAction::Shift { byte_offset: token_byte_offset, byte_length: token_byte_length, .. } => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::Shift {
              offset_end:   (token_byte_offset + token_byte_length) as usize,
              offset_start: token_byte_offset as usize,
            });
          }
        }
        ParseAction::Skip { byte_length: token_byte_length, byte_offset: token_byte_offset, .. } => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::Skip {
              offset_end:   (token_byte_offset + token_byte_length) as usize,
              offset_start: token_byte_offset as usize,
            });
          }
        }
        ParseAction::Reduce { nonterminal_id, rule_id, symbol_count, .. } => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::Reduce { nonterminal_id, rule_id, symbol_count });
          }
        }
        ParseAction::Error { .. } => {
          self.running = false;
          if let LockResult::Ok(mut values) = values.write() {
            values.push(JSDebugEvent::Error {});
          }
        }
        _ => {
          panic!("Unexpected Action!")
        }
      }
    };

    let val = unsafe { Rc::try_unwrap(values).unwrap_unchecked().into_inner().expect("to work").into_iter().collect::<Vec<_>>() };

    serde_wasm_bindgen::to_value(&val).unwrap()
  }
}

#[wasm_bindgen]
pub fn get_codemirror_parse_tree(input: String) -> JsValue {
  let mut reader = string_reader::StringReader::new(input);
  let mut bytecode_parser: ByteCodeParser<string_reader::StringReader, u32, _> =
    ByteCodeParser::<_, u32, _>::new(&mut reader, parser::bytecode.as_slice());
  bytecode_parser.init_parser(31287);

  let mut output = vec![];
  let mut acc_stack: Vec<u32> = vec![];

  const LAST_TOKEN_INDEX: usize = parser::meta::nonterm_names.len();
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
      ParseAction::Shift { byte_offset: token_byte_offset, byte_length: token_byte_length, .. } => {
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
  serde_wasm_bindgen::to_value(&ProdNames(parser::meta::nonterm_names.iter().map(|i| (*i).to_string()).collect())).unwrap()
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
#[wasm_bindgen]
pub struct JSCTXState {
  pub is_scanner: bool,
  pub end_ptr:    usize,
  pub head_ptr:   usize,
  pub scan_ptr:   usize,
  pub base_ptr:   usize,
  pub anchor_ptr: usize,
  pub tok_len:    usize,
  pub tok_id:     u32,
  pub sym_len:    u32,
}

impl<R: ByteReader + UTF8Reader, M> From<&ParseContext<R, M>> for JSCTXState {
  fn from(ctx: &ParseContext<R, M>) -> Self {
    Self {
      anchor_ptr: ctx.anchor_ptr,
      base_ptr:   ctx.base_ptr,
      end_ptr:    ctx.end_ptr,
      head_ptr:   ctx.sym_ptr,
      is_scanner: ctx.is_scanner(),
      scan_ptr:   ctx.tok_ptr,
      sym_len:    ctx.sym_len,
      tok_id:     ctx.tok_id,
      tok_len:    ctx.tok_len,
    }
  }
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum JSDebugEvent {
  ExecuteState { instruction: usize, ctx: JSCTXState },
  ExecuteInstruction { instruction: u32, ctx: JSCTXState },
  Skip { offset_start: usize, offset_end: usize },
  Shift { offset_start: usize, offset_end: usize },
  Reduce { nonterminal_id: u32, rule_id: u32, symbol_count: u32 },
  Complete { nonterminal_id: u32, ctx: JSCTXState },
  Error {},
  EndOfFile,
  Undefined,
}

impl<'a, R: ByteReader + UTF8Reader, M> From<(&DebugEvent<'a>, &ParseContext<R, M>)> for JSDebugEvent {
  fn from((value, ctx): (&DebugEvent<'a>, &ParseContext<R, M>)) -> Self {
    match *value {
      DebugEvent::ExecuteState { base_instruction } => {
        JSDebugEvent::ExecuteState { instruction: base_instruction.address(), ctx: ctx.into() }
      }
      DebugEvent::ExecuteInstruction { instruction } => {
        JSDebugEvent::ExecuteInstruction { instruction: instruction.address() as u32, ctx: ctx.into() }
      }
      DebugEvent::EndOfFile => JSDebugEvent::EndOfFile,
      _ => JSDebugEvent::Undefined,
    }
  }
}

mod string_reader {
  use sherpa_rust_runtime::{deprecate::*, types::*};

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
