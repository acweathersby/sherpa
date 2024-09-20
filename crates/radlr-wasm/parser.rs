use crate::grammar::{JSBytecodeParserDB, JSParserDB};
use js_sys::Array;
use radlr_core::parser;
use radlr_rust_runtime::{
  parsers::Parser,
  types::{bytecode::Opcode, *},
};
use serde::{Deserialize, Serialize};

use std::{
  borrow::BorrowMut,
  cell::RefCell,
  collections::VecDeque,
  default,
  rc::Rc,
  sync::{LockResult, RwLock},
};

use wasm_bindgen::prelude::*;

/// Am iterable parser
#[wasm_bindgen]
pub struct JSByteCodeParser {
  running:         bool,
  db:              Rc<BytecodeParserDB>,
  reader:          StringInput,
  bytecode_parser: Box<dyn Parser<StringInput>>,
  ctx:             ParserContext,
  values:          Rc<RwLock<VecDeque<JSDebugPacket>>>,
}

#[wasm_bindgen]
impl JSByteCodeParser {
  pub fn new(input: String, bytecode: &JSBytecodeParserDB) -> Self {
    Self {
      running:         false,
      reader:          StringInput::from(input),
      bytecode_parser: bytecode.0.get_parser().unwrap(),
      db:              bytecode.0.clone(),
      ctx:             Default::default(),
      values:          Rc::new(RwLock::new(VecDeque::new())),
    }
  }

  pub fn init(&mut self, entry_name: String) {
    let entry = self.db.get_entry_data_from_name(&entry_name).expect("Could not find entry point");

    self.ctx = self.bytecode_parser.init(entry).expect("Could not create context");

    let v = self.values.clone();
    let debugger: Option<Box<DebugFnNew>> = Some(Box::new(move |e, ctx, i| {
      if let LockResult::Ok(mut values) = v.write() {
        match e {
          DebugEventNew::ExecuteInstruction { instruction, is_scanner } => {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::ExecuteInstruction,
              ctx: JSCTXState::from(&ctx),
              instruction: instruction.address(),
              is_scanner: *is_scanner,
              complete: (instruction.get_opcode() == Opcode::Accept) as u32,
              ..Default::default()
            });
          }
          DebugEventNew::ExecuteState { base_instruction, is_scanner } => {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::ExecuteState,
              ctx: JSCTXState::from(&ctx),
              instruction: base_instruction.address(),
              is_scanner: *is_scanner,
              ..Default::default()
            });
          }
          _ => {}
        }
      }
    }));

    self.bytecode_parser.set_debugger(debugger);

    self.running = true;
  }

  pub fn next(&mut self) -> JsValue {
    if let LockResult::Ok(mut values) = self.values.write() {
      if let Some(debug_event) = values.pop_front() {
        return debug_event.into();
      }
    };

    if self.running {
      self.next_internal();
      self.next()
    } else {
      JsValue::UNDEFINED
    }
  }

  fn next_internal(&mut self) {
    let values = self.values.clone();

    {
      let Self { ctx, reader, bytecode_parser, .. } = self;
      match bytecode_parser.next(reader, ctx) {
        Some(ParseAction::Accept { nonterminal_id, .. }) => {
          self.running = false;
          if let LockResult::Ok(mut values) = values.write() {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::Complete,
              ctx: JSCTXState::from(&ParserStackTrackers::from(&*ctx)),
              nonterminal_id,
              ..Default::default()
            });
          }
        }
        Some(ParseAction::EndOfInput { .. }) => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::EndOfFile,
              ctx: JSCTXState::from(&ParserStackTrackers::from(&*ctx)),
              ..Default::default()
            });
          }
        }
        Some(ParseAction::Shift { byte_offset: token_byte_offset, byte_length: token_byte_length, .. }) => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::Shift,
              ctx: JSCTXState::from(&ParserStackTrackers::from(&*ctx)),
              offset_end: (token_byte_offset + token_byte_length) as usize,
              offset_start: token_byte_offset as usize,
              ..Default::default()
            });
          }
        }
        Some(ParseAction::Skip { byte_length: token_byte_length, byte_offset: token_byte_offset, .. }) => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::Skip,
              ctx: JSCTXState::from(&ParserStackTrackers::from(&*ctx)),
              offset_end: (token_byte_offset + token_byte_length) as usize,
              offset_start: token_byte_offset as usize,
              ..Default::default()
            });
          }
        }
        Some(ParseAction::Reduce { nonterminal_id, rule_id, symbol_count, .. }) => {
          if let LockResult::Ok(mut values) = values.write() {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::Reduce,
              ctx: JSCTXState::from(&ParserStackTrackers::from(&*ctx)),
              nonterminal_id,
              rule_id,
              symbol_count,
              ..Default::default()
            });
          }
        }
        Some(ParseAction::Error { .. }) => {
          self.running = false;
          if let LockResult::Ok(mut values) = values.write() {
            values.push_back(JSDebugPacket {
              event: JSDebugEvent::Error,
              ctx: JSCTXState::from(&ParserStackTrackers::from(&*ctx)),
              ..Default::default()
            });
          }
        }
        _ => {
          panic!("Unexpected Action!")
        }
      }
    };
  }
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

#[derive(Serialize, Deserialize, Clone, Copy, Default)]
#[wasm_bindgen]
pub struct JSCTXState {
  pub is_scanner: bool,
  pub end_ptr:    usize,
  pub input_ptr:  usize,
  pub sym_ptr:    usize,
  pub begin_ptr:  usize,
  pub anchor_ptr: usize,
  pub tok_len:    usize,
  pub tok_id:     u32,
  pub sym_len:    u32,
}

impl From<&ParserStackTrackers> for JSCTXState {
  fn from(ctx: &ParserStackTrackers) -> Self {
    Self {
      anchor_ptr: ctx.anchor_ptr,
      begin_ptr:  ctx.begin_ptr,
      end_ptr:    ctx.end_ptr,
      input_ptr:  ctx.input_ptr,
      is_scanner: false,
      sym_ptr:    ctx.sym_ptr,
      sym_len:    ctx.tok_byte_len,
      tok_id:     ctx.tok_id,
      tok_len:    ctx.byte_len as usize,
    }
  }
}

#[derive(Serialize, Deserialize, Clone, Copy, Default)]
#[wasm_bindgen]
pub enum JSDebugEvent {
  ExecuteState,
  ExecuteInstruction,
  Skip,
  Shift,
  Reduce,
  Complete,
  Error,
  EndOfFile,
  #[default]
  Undefined,
}

#[derive(Serialize, Deserialize, Clone, Copy, Default)]
#[wasm_bindgen]
pub struct JSDebugPacket {
  pub event:          JSDebugEvent,
  pub ctx:            JSCTXState,
  pub instruction:    usize,
  pub offset_start:   usize,
  pub offset_end:     usize,
  pub nonterminal_id: u32,
  pub rule_id:        u32,
  pub symbol_count:   u32,
  pub complete:       u32,
  pub is_scanner:     bool,
}

mod string_reader {
  use std::sync::Arc;

  use radlr_rust_runtime::{deprecate::*, types::*};

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
      self.source.get_or_insert(self.data.as_bytes().into()).clone()
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
