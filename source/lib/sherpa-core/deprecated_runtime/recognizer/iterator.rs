#[deprecated]
use std::result;

use crate::{
  deprecated_runtime::{
    buffer::ByteReader,
    parse_token::ParseToken,
    recognizer::stack::KernelStack,
  },
  types::*,
};

// Global Constants

const STATE_INDEX_MASK: u32 = (1 << 24) - 1;

const _FAIL_STATE_MASK: u32 = 1 << 27;

const _NORMAL_STATE_FLAG: u32 = 1 << 26;

const _GOTO_STATE_MASK: u32 = 1 << 25;

const _ALPHA_INCREMENT_STACK_POINTER_MASK: u32 = 1 << 0;

const _ALPHA_HAVE_DEFAULT_ACTION_MASK: u32 = 1 << 1;

const _PRODUCTION_SCOPE_POP_POINTER: u32 = 2;

const INSTRUCTION_POINTER_MASK: u32 = 0xFFFFFF;

const SKIPPED_SCAN_PROD: u32 = 9009;

const DEFAULT_PASS_INSTRUCTION: usize = 1;

const NORMAL_STATE_FLAG: u32 = 1 << 26;

#[derive(Debug, Copy, Clone)]
#[deprecated]
pub enum ParseErrorCode {
  NORMAL,
  CANNOT_FORK,
  INVALID_RETURN,
}
#[derive(Debug, Copy, Clone)]
#[deprecated]
pub enum ParseAction {
  NONE {},
  REDUCE { body: u32, production: u32, length: u32 },
  ACCEPT {},
  ERROR { production: u32, pointer: u32, error_code: ParseErrorCode },
  SHIFT { token: ParseToken },
  SKIP { length: u32, line: u32, token_type: u32 },
  TOKEN { token: ParseToken },
  FORK {},
}

/// Represents a single Iterator
/// that can parse an input when
/// with parser byte code,  entry pointer
/// and parse action handler function.
#[deprecated]
pub trait ParseIterator<T: ByteReader> {
  fn new(reader: T, entry_pointer: u32, bytecode: &'static [u32]) -> Self;

  /// Starts parsing the input and emits
  /// parse actions to the handleAction function

  fn start(&mut self, handle_action: &mut impl FnMut(ParseAction)) -> ParseToken;

  fn reader(&self) -> &T;
}
#[deprecated]
pub struct ReferenceIterator<T: ByteReader> {
  bytecode:      &'static [u32],
  entry_pointer: u32,
  reader:        T,
}

impl<T: ByteReader> ParseIterator<T> for ReferenceIterator<T> {
  fn new(reader: T, entry_pointer: u32, bytecode: &'static [u32]) -> Self {
    ReferenceIterator { entry_pointer, reader, bytecode }
  }

  fn start(&mut self, handle_action: &mut impl FnMut(ParseAction)) -> ParseToken {
    let mut parser: StateIterator<T> =
      StateIterator::new(self.reader.clone(), self.entry_pointer, handle_action);

    parser.parse(self.bytecode);

    parser.get_tok(1)
  }

  fn reader(&self) -> &T {
    &self.reader
  }
}

// pub struct ThreadedIterator<T> {
// job_start: Sender<bool>,
// data: Receiver<ParseAction>,
// _reader_: PhantomData<T>,
// }
//
// impl<T: 'static + ByteReader + Send> ParseIterator<T> for
// ThreadedIterator<T> { fn new(reader: T, entry_pointer: u32,
// bytecode: &'static [u32]) -> Self { let (job_tx, job_rx) =
// mpsc::channel::<bool>(); let (tx, rx) =
// mpsc::channel::<ParseAction>();
//
// let iterator = ThreadedIterator {
// data: rx,
// job_start: job_tx,
// _reader_: PhantomData,
// };
//
// let handle = thread::spawn(move || match job_rx.recv() {
// Ok(job) => {
// let i = 0;
//
// let mut action_handler = |action| {
// tx.send(action);
// };
//
// let mut parser = StateIterator::new(reader, entry_pointer, &mut
// action_handler);
//
// parser.parse(bytecode);
// }
// Err(e) => {}
// });
//
// iterator
// }
//
// fn start(&mut self, handle_action: &mut impl FnMut(ParseAction)) {
// self.job_start.send(true);
// loop {
// match self.data.recv() {
// Ok(action) => {
// handle_action(action);
// }
// Err(e) => {
// println!("{:?}", e);
// break;
// }
// }
// }
// }
// }

// This represents the core context in which a parser machine
// operators.
//
// It yields parse Actions and reduce information that can be
// consumed by completers to apply parse actions on the token
// values
#[deprecated]
pub struct StateIterator<'a, T: ByteReader> {
  stack: KernelStack,

  reader: T,

  // forks: [u32; 8],
  tokens: [ParseToken; 8],

  symbol_accumulator: u32,

  token_end: usize,

  production_id: u32,

  scanner_iterator: ScannerIterator<T>,

  emit: &'a mut dyn FnMut(ParseAction),
}

impl<'a, T: ByteReader> StateIterator<'a, T> {
  pub fn new(
    reader: T,
    entry_pointer: u32,
    emit: &'a mut impl FnMut(ParseAction),
  ) -> StateIterator<'a, T> {
    let cloned_reader = reader.clone();

    let mut iterator = StateIterator {
      stack: KernelStack::new(),
      reader,
      // forks: [0; 8],
      tokens: [ParseToken::new(); 8],
      symbol_accumulator: 0,
      production_id: 0,
      token_end: 1,
      scanner_iterator: ScannerIterator::new(cloned_reader, 0),
      emit,
    };

    iterator.stack.reset(entry_pointer);

    iterator
  }

  pub fn parse(&mut self, bytecode: &[u32]) {
    let mut fail_mode: u32 = 0;

    let mut last_good_state: u32 = 0;

    loop {
      let state = self.stack.pop_state();

      if state < 1 {
        if self.reader.offset_at_end(self.tokens[1].byte_offset) {
          self.emit_action(ParseAction::ACCEPT {});

          return;
        } else {
          self.emit_action(ParseAction::ERROR {
            production: self.production_id,
            pointer:    last_good_state,
            error_code: ParseErrorCode::NORMAL,
          });

          return;
        }
      } else {
        let mask_gate = NORMAL_STATE_FLAG << fail_mode;

        if fail_mode == 0 {
          last_good_state = state;
        }

        if (state & mask_gate) != 0 {
          fail_mode = self.instruction_executor(state, fail_mode, bytecode);
        }
      }
    }
  }

  fn scanner(
    &mut self,
    mut current_token: ParseToken,
    scanner_start_pointer: u32,
    bytecode: &[u32],
  ) -> ParseToken {
    if current_token.token_type == 0 {
      let scanner = &mut self.scanner_iterator;

      scanner.stack.reset(scanner_start_pointer);

      scanner.reader.set_cursor_to(&current_token);

      scanner.tokens[1] = current_token;

      scanner.tokens[0] = current_token;

      loop {
        let result = self.scanner_iterator.parse(bytecode);

        match &result {
          ParseAction::ACCEPT {} => {
            break;
          }
          ParseAction::TOKEN { token } => {
            if token.token_type == SKIPPED_SCAN_PROD {
              current_token.cp_offset += token.cp_length;

              current_token.byte_offset += token.byte_length;

              current_token.line_offset = token.line_offset;

              current_token.line_number = token.line_number;

              self.scanner_iterator.stack.reset(scanner_start_pointer);

              self.scanner_iterator.tokens[0] = self.scanner_iterator.tokens[1];
            } else {
              current_token.cp_length = token.cp_length;

              current_token.byte_length = token.byte_length;

              current_token.token_type = token.token_type;

              current_token.line_offset = token.line_offset;

              current_token.line_number = token.line_number;

              return current_token;
            }
          }
          ParseAction::FORK {} => {
            self.emit_action(ParseAction::ERROR {
              production: self.production_id,
              error_code: ParseErrorCode::CANNOT_FORK,
              pointer:    0,
            });

            break;
          }
          ParseAction::ERROR { pointer: 0, error_code: ParseErrorCode::NORMAL, production: _ } => {
            self.emit_action(result);

            break;
          }
          _ => {
            self.emit_action(ParseAction::ERROR {
              production: self.production_id,
              error_code: ParseErrorCode::INVALID_RETURN,
              pointer:    0,
            });

            break;
          }
        }
      }
    }

    current_token
  }
}

impl<'a, T: ByteReader> ParserCoreIterator<T> for StateIterator<'a, T> {
  fn push_state(&mut self, state: u32) {
    self.stack.push_state(state)
  }

  fn pop_state(&mut self) -> u32 {
    self.stack.pop_state()
  }

  fn read_state(&self) -> u32 {
    self.stack.read_state()
  }

  fn swap_state(&mut self, state: u32) {
    self.stack.swap_state(state);
  }

  fn get_prod(&self) -> u32 {
    self.production_id
  }

  fn set_prod(&mut self, val: u32) {
    self.production_id = val;
  }

  fn get_acc(&mut self) -> u32 {
    self.symbol_accumulator
  }

  fn set_acc(&mut self, val: u32) {
    self.symbol_accumulator = val;
  }

  fn get_tok_top(&self) -> usize {
    self.token_end
  }

  fn set_tok_top(&mut self, index: usize) {
    self.token_end = index;
  }

  fn get_tok(&mut self, index: usize) -> ParseToken {
    self.tokens[index]
  }

  fn set_tok(&mut self, index: usize, token: ParseToken) {
    self.tokens[index] = token;
  }

  fn get_reader(&mut self) -> &mut T {
    &mut self.reader
  }

  fn emit_action(&mut self, action: ParseAction) {
    (self.emit)(action);
  }

  fn consume(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize {
    let mut token = self.get_tok(1);

    let instruction = bytecode[index];

    if (instruction & 1) != 0 {
      token.cp_length = 0;

      token.byte_length = 0;

      self.set_tok(1, token);
    }

    self.emit_shift();

    token.cp_offset += token.cp_length;

    token.byte_offset += token.byte_length;

    token.cp_length = 0;

    token.byte_length = 0;

    token.token_type = 0;

    self.set_tok(0, token);

    self.set_tok(1, token);

    index + 1
  }

  fn get_input_value(
    &mut self,
    input_type: u32,
    lexer_type: u32,
    scanner_start_pointer: u32,
    bytecode: &[u32],
  ) -> i32 {
    if input_type > 0 {
      // Lexer token id input

      if lexer_type == 1 {
        // set next peek lexer

        let token = self.tokens[self.token_end];

        self.token_end += 1;

        self.tokens[self.token_end] = token.next();
      } else {
        // set primary lexer

        if self.token_end > 1 {
          self.token_end = 1;

          if !self.reader.set_cursor_to(&self.tokens[1]) {
            return 0;
          };
        }
      }

      let index = self.token_end;

      let mut tok = self.get_tok(index);

      match input_type {
        1 => {
          tok = self.scanner(tok, scanner_start_pointer, bytecode);

          self.tokens[index] = tok;

          tok.token_type as i32
        }
        _ => 0,
      }
    } else {
      // Production id input
      self.get_prod() as i32
    }
  }
}
#[deprecated]
pub struct ScannerIterator<T: ByteReader> {
  stack: KernelStack,

  reader: T,

  tokens: [ParseToken; 8],

  symbol_accumulator: u32,

  token_end: usize,

  production_id: u32,
}

impl<T: ByteReader> ScannerIterator<T> {
  pub fn new(reader: T, entry_pointer: u32) -> ScannerIterator<T> {
    let mut iterator = ScannerIterator {
      stack: KernelStack::new(),
      reader,
      tokens: [ParseToken::new(); 8],
      symbol_accumulator: 0,
      production_id: 0,
      token_end: 1,
    };

    iterator.stack.reset(entry_pointer);

    iterator
  }

  pub fn parse(&mut self, bytecode: &[u32]) -> ParseAction {
    let mut fail_mode: u32 = 0;

    // To preserve line count and offset information,
    // we assign the eventual outgoing token with the
    // the current line information from the reader.

    let mut token = &mut self.tokens[0];
    token.line_number = self.reader.line_count();

    token.line_offset = self.reader.line_offset();

    loop {
      let state = self.stack.pop_state();

      if state < 1 {
        let token = self.tokens[0];

        return ParseAction::TOKEN { token };
      } else {
        let mask_gate = NORMAL_STATE_FLAG << fail_mode;

        if (state & mask_gate) != 0 {
          fail_mode = self.instruction_executor(state, fail_mode, bytecode);
        }
      }
    }
  }
}

impl<T: ByteReader> ParserCoreIterator<T> for ScannerIterator<T> {
  fn push_state(&mut self, state: u32) {
    self.stack.push_state(state)
  }

  fn pop_state(&mut self) -> u32 {
    self.stack.pop_state()
  }

  fn read_state(&self) -> u32 {
    self.stack.read_state()
  }

  fn swap_state(&mut self, state: u32) {
    self.stack.swap_state(state);
  }

  fn get_prod(&self) -> u32 {
    self.production_id
  }

  fn set_prod(&mut self, val: u32) {
    self.production_id = val;
  }

  fn get_acc(&mut self) -> u32 {
    self.symbol_accumulator
  }

  fn set_acc(&mut self, val: u32) {
    self.symbol_accumulator = val;
  }

  fn get_tok_top(&self) -> usize {
    self.token_end
  }

  fn set_tok_top(&mut self, index: usize) {
    self.token_end = index;
  }

  fn get_tok(&mut self, index: usize) -> ParseToken {
    self.tokens[index]
  }

  fn set_tok(&mut self, index: usize, token: ParseToken) {
    self.tokens[index] = token;
  }

  fn get_reader(&mut self) -> &mut T {
    &mut self.reader
  }

  fn consume(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize {
    let mut token = self.get_tok(1);

    let anchor = self.get_tok(0);

    let instruction = bytecode[index];

    if (instruction & 1) != 0 {
      token.cp_length = 0;

      token.byte_length = 0;
    }

    self.reader.next(token.byte_length);

    token.cp_offset += token.cp_length;

    token.byte_offset += token.byte_length;

    token.cp_length = 0;

    token.byte_length = 0;

    token.line_offset = self.reader.line_offset();

    token.line_number = self.reader.line_count();

    token.token_type = 0;

    self.set_tok(1, token);

    index + 1
  }

  fn get_input_value(
    &mut self,
    input_type: u32,
    lexer_type: u32,
    _scanner_start_pointer: u32,
    _bytecode: &[u32],
  ) -> i32 {
    if input_type > 0 {
      // Lexer token id input

      if lexer_type == 1 {
        // set next peek lexer

        let prev_token = self.tokens[self.token_end];

        self.reader.next(prev_token.byte_length);

        if self.reader.at_end() {
          return 0;
        }

        self.token_end += 1;

        self.tokens[self.token_end] = prev_token.next();
      } else {
        if self.reader.at_end() {
          return 1;
        }

        if self.token_end > 1 {
          self.token_end = 1;

          if !self.reader.set_cursor_to(&self.tokens[1]) {
            // self.error();
            return 0;
          };

          self.tokens[1].token_type = 0;
        }
      }

      let token = &mut self.tokens[self.token_end];

      token.line_number = self.reader.line_count();
      token.line_offset = self.reader.line_offset();

      match input_type {
        2 => {
          token.byte_length = self.reader.codepoint_byte_length();

          token.cp_length = self.reader.codepoint_length();

          self.reader.class() as i32
        }
        3 => {
          token.byte_length = self.reader.codepoint_byte_length();

          token.cp_length = self.reader.codepoint_length();

          self.reader.codepoint() as i32
        }
        4 => {
          token.byte_length = 1;

          token.cp_length = 1;

          self.reader.byte() as i32
        }
        _ => 0,
      }
    } else {
      // Production id input
      self.get_prod() as i32
    }
  }
}
#[deprecated]
trait ParserCoreIterator<R: ByteReader> {
  fn consume(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize;

  fn emit_action(&mut self, _: ParseAction) {}

  fn get_acc(&mut self) -> u32;

  fn get_prod(&self) -> u32;

  fn get_reader(&mut self) -> &mut R;

  fn get_tok_top(&self) -> usize;

  fn get_tok(&mut self, index: usize) -> ParseToken;

  fn pop_state(&mut self) -> u32;

  fn push_state(&mut self, state: u32);

  fn read_state(&self) -> u32;

  fn set_acc(&mut self, val: u32);

  fn set_prod(&mut self, val: u32);

  fn set_tok_top(&mut self, index: usize);

  fn set_tok(&mut self, index: usize, token: ParseToken);

  fn swap_state(&mut self, state: u32);

  fn emit_shift(&mut self) {
    let token = self.get_tok(1);

    let prev_token = self.get_tok(0);

    if prev_token.byte_offset + prev_token.byte_length != token.byte_offset {
      self.emit_action(ParseAction::SKIP {
        length:     token.cp_offset - prev_token.cp_offset,
        line:       prev_token.line_number,
        token_type: 0,
      });
    }

    self.emit_action(ParseAction::SHIFT { token });
  }

  fn emit_reduce(&mut self, symbol_length: u32, body_id: u32) {
    self.emit_action(ParseAction::REDUCE {
      body:       body_id,
      length:     symbol_length,
      production: self.get_prod(),
    });
  }

  //*
  fn instruction_executor(&mut self, state_pointer: u32, fail_mode: u32, bytecode: &[u32]) -> u32 {
    let mut index = (state_pointer & STATE_INDEX_MASK) as usize;

    loop {
      match bytecode[index] & 0xF0000000 as u32 {
        0x10000000 => {
          index = self.consume(index, fail_mode, bytecode);
        }

        0x20000000 => {
          index = self.goto(index, fail_mode, bytecode);
        }

        0x30000000 => {
          index = self.set_production(index, fail_mode, bytecode);
        }

        0x40000000 => {
          index = self.reduce(index, fail_mode, bytecode);
        }

        0x50000000 => {
          index = self.set_token(index, fail_mode, bytecode);
        }

        0x60000000 => {
          index = self.fork(index, fail_mode, bytecode);
        }

        0x70000000 => {
          index = self.scan_to(index, fail_mode);
        }

        0x80000000 => {
          index = self.noop(index, fail_mode);
        }

        0x90000000 => {
          index = self.index_jump(index, fail_mode, bytecode);
        }

        0xA0000000 => {
          index = self.hash_jump(index, fail_mode, bytecode);
        }

        0xB0000000 => {
          index = self.push_fail_state(index, fail_mode, bytecode);
        }

        0xC0000000 => {
          index = self.repeat(index, fail_mode, bytecode);
        }

        0xD0000000 => {
          index = self.noop(index, fail_mode);
        }

        0xE0000000 => {
          index = self.assert_consume(index, fail_mode, bytecode);
        }

        0xF0000000 => break self.advanced_return(index, fail_mode, bytecode) as u32,
        _ => break self.pass(index + 1, fail_mode) as u32,
      };
    }
  } //*/
  fn noop(&mut self, index: usize, _: u32) -> usize {
    index + 1
  }

  fn pass(&mut self, mut __: usize, _: u32) -> usize {
    0
  }

  fn assert_consume(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    let mode = instruction & 0x0F000000;

    let val = instruction & 0x00FFFFFF;

    let mut token = self.get_tok(1);

    let mut does_not_match = false;

    {
      let reader = self.get_reader();

      match mode {
        0x00000000 => {
          // CLASS
          token.byte_length = reader.codepoint_byte_length();

          token.cp_length = reader.codepoint_length();

          does_not_match = val != reader.class();
        }
        0x01000000 => {
          // CODEPOINT
          token.byte_length = reader.codepoint_byte_length();

          token.cp_length = reader.codepoint_length();

          does_not_match = val != reader.codepoint();
        }
        0x02000000 => {
          // BYTE
          token.byte_length = 1;

          token.cp_length = 1;

          does_not_match = val != reader.byte() as u32;
        }
        _ => {}
      }
    }

    self.set_tok(1, token);

    if does_not_match {
      2
    } else {
      token.line_number = self.get_reader().line_count();
      token.line_offset = self.get_reader().line_offset();

      self.consume(1, 0, bytecode);

      index + 1
    }
  }

  fn goto(&mut self, mut index: usize, a: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    self.push_state(instruction);

    index += 1;

    if (bytecode[index] & 0xF0000000) == 0x20000000 {
      return self.goto(index, a, bytecode);
    }

    index
  }

  fn reduce(&mut self, index: usize, recover_data: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    let body_id = (instruction) & 0xFFFF;

    let length = (instruction >> 16) & 0xFFF;

    if (body_id & 0xFFFF) == 0xFFFF {
      let accumulated_symbols = self.get_acc() - (recover_data & 0xFFFF0000);

      let len = accumulated_symbols >> 16;

      let fn_id = (instruction >> 16) & 0x0FFF;

      // Extract accumulated symbols inform
      self.emit_reduce(len, fn_id);
    } else {
      self.emit_reduce(length, body_id);
    }

    if (bytecode[index + 1] & 0xF0000000) == 0x30000000 {
      self.set_production(index + 1, recover_data, bytecode)
    } else {
      index + 1
    }
  }

  fn set_production(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    self.set_prod(instruction & 0xFFFFFFF);

    index + 1
  }

  fn repeat(&mut self, mut index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    let origin_offset = 0xFFFFFFF & instruction;

    index -= origin_offset as usize;

    index + 1
  }

  fn push_fail_state(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    let fail_state_pointer = instruction;

    let current_state = self.read_state() & INSTRUCTION_POINTER_MASK;

    // Only need to set new failure state if the previous state
    // Is not identical to the pending fail state.
    if current_state != ((fail_state_pointer) & INSTRUCTION_POINTER_MASK) {
      self.push_state(fail_state_pointer);
    } else {
      self.swap_state(fail_state_pointer);
    }

    index + 1
  }

  fn set_token(&mut self, index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    let val = instruction & 0xFFFFFF;

    if (instruction & 0x1000000) != 0 {
      self.consume(DEFAULT_PASS_INSTRUCTION, 0, bytecode);
    }

    if (instruction & 0x08000000) != 0 {
      let mut root_token = self.get_tok(0);

      let scan_token = self.get_tok(1);

      self.set_prod(val);

      root_token.token_type = val;

      root_token.byte_length = scan_token.byte_offset - root_token.byte_offset;

      root_token.cp_length = scan_token.cp_offset - root_token.cp_offset;

      self.set_tok(0, root_token);
    } else {
      let mut scan_token = self.get_tok(1);

      scan_token.cp_length = val;

      scan_token.byte_length = val;

      self.set_tok(1, scan_token);
    }

    index + 1
  }

  fn advanced_return(&mut self, index: usize, fail_mode: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    if (instruction & 1) != 0 {
      fail_mode as usize
    } else {
      1
    }
  }

  fn fork(&mut self, _index: usize, _: u32, _bytecode: &[u32]) -> usize {
    // let instruction = bytecode[index];
    DEFAULT_FAIL_INSTRUCTION_ADDRESS as usize
  }

  fn scan_to(&mut self, index: usize, _: u32) -> usize {
    // let length = instruction & 0xFFFF;
    //
    // let scanner_start_pointer = self.bytecode[index];
    //
    // index += 1;
    //
    // let scan_back = (instruction & 0x00100000) > 0;
    //
    // let lexer = &mut self.state.lexer;
    //
    // let start_byte_offset = lexer.prev_byte_offset;
    //
    // let start_token_offset = lexer.prev_token_offset;
    //
    // lexer.byte_length = 1;
    //
    // let mut RUN = true;
    //
    // let start = index;
    //
    // let end = index + length as usize;
    //
    // let mut end_offset = lexer.input.len() as u32;
    //
    // index += length as usize;
    //
    // let mut temp_lexer = lexer.copy_in_place();
    //
    // if scan_back {
    // scan "backwards" towards the previously accepted token.
    // really we just set the scan start position to
    // lexer.previous_byte and end to the current position of
    // the lexer and rescan forward.
    // end_offset = temp_lexer.byte_offset;
    // temp_lexer.byte_offset = temp_lexer.prev_byte_offset;
    // temp_lexer.token_offset = temp_lexer.prev_token_offset;
    // temp_lexer.byte_length = 0;
    // temp_lexer.token_length = 0;
    // temp_lexer.next();
    // }
    //
    // while RUN {
    // {
    // scanner(
    // self.state,
    // &mut { *(&self.state.lexer) },
    // scanner_start_pointer,
    // );
    // };
    // for i in start..end {
    // if temp_lexer.token_type == self.bytecode[i] as i32 {
    // RUN = false;
    // break;
    // }
    // }
    //
    // if !RUN {
    // break;
    // }
    //
    // if temp_lexer.byte_offset >= end_offset {
    // return 1;
    // }
    //
    // temp_lexer.next();
    // }
    //
    // if !scan_back {
    // Reset peek stack;
    // self.state.lexer.peek_unroll_sync(&mut temp_lexer);
    // self.state.lexer.prev_byte_offset = start_byte_offset;
    // self.state.lexer.prev_token_offset = start_token_offset;
    // }

    index
  }

  fn hash_jump(&mut self, mut index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    index += 1;

    let input_type = (instruction >> 22) & 0x7;

    let lexer_type = (instruction >> 26) & 0x3;

    let scanner_start_pointer = bytecode[index];

    let table_data = bytecode[index + 1];

    index += 2;

    let modulus = (1 << ((table_data >> 16) & 0xFFFF)) - 1;

    let table_size = (table_data) & 0xFFFF;

    let hash_table_start = index as u32;

    let instruction_field_start = (hash_table_start as u32) + table_size;

    let instruction_field_size = instruction & 0xFFFF;

    let input_value =
      self.get_input_value(input_type, lexer_type, scanner_start_pointer, bytecode) as u32;

    let mut hash_index = input_value & modulus;

    loop {
      let cell = bytecode[(hash_table_start + hash_index) as usize] as u32;

      let value = (cell & 0x7FF) as u32;

      let next = (((cell >> 22) & 0x3FF) as i32) - 512;

      if value == input_value {
        let instruction_start = ((cell >> 11) & 0x7FF) as u32;

        return (instruction_field_start + instruction_start) as usize;
      }

      if next == 0 {
        // Failure
        return (instruction_field_size + instruction_field_start) as usize;
      }

      hash_index = ((hash_index as i32) + next) as u32;
    }
  }

  fn index_jump(&mut self, mut index: usize, _: u32, bytecode: &[u32]) -> usize {
    let instruction = bytecode[index];

    index += 1;

    let scanner_start_pointer = bytecode[index];

    let table_data = bytecode[index + 1];

    let basis__ = instruction & 0xFFFF;

    let input_type = (instruction >> 22) & 0x7;

    let lexer_type = (instruction >> 26) & 0x3;

    let mut input_value =
      self.get_input_value(input_type, lexer_type, scanner_start_pointer, bytecode);

    input_value -= basis__ as i32;

    let input_value = input_value as u32;

    let number_of_rows = table_data >> 16;

    let row_size = table_data & 0xFFFF;

    index += 2;

    if input_value < number_of_rows {
      index + (input_value * row_size + row_size) as usize
    } else {
      // Use default behavior found at the beginning of the
      // jump table
      index
    }
  }

  fn get_input_value(
    &mut self,
    input_type: u32,
    lexer_type: u32,
    scanner_start_pointer: u32,
    bytecode: &[u32],
  ) -> i32;
}
