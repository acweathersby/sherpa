//! Convenient types for working with bytecode parser data.

#[cfg(debug_assertions)]
use std::fmt::Debug;
use std::fmt::Display;

#[cfg(feature = "wasm-lab")]
use wasm_bindgen::prelude::*;

/// The current set of instruction opcodes
#[cfg_attr(feature = "wasm-lab", wasm_bindgen)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum Opcode {
  /// Default value for unrecognized opcode values.
  NoOp                 = 0,
  /// A sentinel instruction that ends the execution of a parse block, similar
  /// to a x86 `ret` instruction.
  ///
  /// This is a single byte instruction.
  Pass                 = 1,
  /// Ends the execution of the current parse block, and causes the parser to
  /// enter exception mode, popping parse blocks of the goto stack until it
  /// encounters an exception handling parse block that resets the execution
  /// mode to normal.
  ///
  /// If the execution mode is not reset to normal by the last parse block,
  /// then the parser emits an `Error` event and halts further parsing.
  ///
  /// This is a single byte instruction.
  Fail                 = 2,
  /// Increments the `scan_ptr` by the value of `sym_len` and then sets
  /// `sym_len` to 0.
  ///
  /// This is a single byte instruction.
  ShiftChar            = 3,
  /// Shifts the current token value, moving  the `anchor_ptr`, `base_ptr`,
  /// `scan_ptr`, and `head_ptr` to the position of `head_ptr + tok_len`.
  /// Also set `tok_id` to 0.
  ///
  /// This also causes a `Shift` event to be emitted, pausing the parser until
  /// it is resumed through a call to `next`.
  ///
  /// This is a single byte instruction.
  ShiftToken           = 4,
  /// Same as `ShiftToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `ShiftToken` instructions.
  ///
  /// This also causes a `Shift` event to be emitted, pausing the parser until
  /// it is resumed through a call to `next`.
  ///
  /// This is a single byte instruction.
  ShiftTokenScanless   = 5,
  /// Assigns the value `head_ptr + tok_len` to `head_ptr` and `scan_ptr`. Also
  /// sets the values of `tok_len` and `tok_id` to 0.
  ///
  /// This is a single byte instruction.
  PeekToken            = 6,
  /// Same as `PeekToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `PeekToken` instructions.
  ///
  /// This is a single byte instruction.
  PeekTokenScanless    = 7,
  /// Assigns `head_ptr + tok_len` to `head_ptr`, `scan_ptr`, and `base_ptr`.
  /// Then jumps to the first instruction of the current parse block.
  ///
  /// This is a single byte instruction.
  SkipToken            = 8,
  /// Same as `SkipToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `SkipToken` instruction.
  ///
  /// This is a single byte instruction.
  SkipTokenScanless    = 9,
  /// Assigns `head_ptr + tok_len` to `head_ptr` and `scan_ptr`. Then jumps to
  /// the first instruction of the current arse block.
  ///
  /// This is a single byte instruction.
  PeekSkipToken        = 10,
  /// Same as `PeekSkipToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `PeekSkipToken` instructions.
  ///
  /// This is a single byte instruction.
  PeekSkipTokenScanless = 11,
  /// Assigns the value of `base_ptr` to `scan_ptr` and `head_ptr`. Also sets
  /// `tok_len`, `sym_len`, and `tok_id` to `0`.
  ///
  /// This is a single byte instruction.
  PeekReset            = 12,
  /// Accepts the parsed input as a valid string of the grammar.
  ///
  /// This is a single byte instruction
  Accept               = 13,
  /// Removes the top goto from the goto stack
  ///
  /// This is a single byte instruction.
  PopGoto              = 14,
  /// Pushes the address of a parse block onto the goto stack.
  ///
  /// # Operands
  /// - u8: Parse Mode - The mode the parser must be in to allow execution of
  ///   this block.
  /// - u32: Parse Block Address -The address of the parse block to jump to.
  ///
  /// This is a 6 byte instruction.
  PushGoto             = 15,
  /// Pushes the address of an exception handler block onto the goto stack.
  ///
  /// # Operands
  /// - u8: Parse Mode - The mode the parser must be in to allow execution of
  ///   this block.
  /// - u32: Parse Block Address -The address of the parse block to jump to.
  ///
  /// This is a 6 byte instruction.
  PushExceptionHandler = 16,
  /// Jumps to the location specified.
  ///
  /// # Operands
  /// - u8: Parse Mode - The mode the parser must be in to allow execution of
  ///   this block.
  /// - u32: Parse Block Address -The address of the parse block to jump to.
  ///
  /// This is a 6 byte instruction.
  Goto                 = 17,
  /// Assigns a token_id value to `tok_id` and assigns `scan_ptr - head_ptr` to
  /// `tok_len`.
  ///
  /// # Operands
  /// - u32: TokenID
  ///
  /// This is a 5 byte instruction.
  AssignToken          = 18,

  /// Assigns a nonterminal_id to `nterm`, a rule_id to `rule_id` and the
  /// number of values to pop of the stack to `sym_len`.
  ///
  /// This also causes a `Reduce` event to be emitted, pausing the parser until
  /// it is resumed through a call to `next`.

  ///
  /// This is an 11 byte instruction.
  Reduce               = 19,
  /// This is a complex instruction that branches to sub parse blocks based on
  /// an input value, which may be:

  ///
  /// Using a simple lookup table, one or more expected values are mapped to
  /// addresses of parse blocks. Execution continues from the block address
  /// whose expected value matches the input value.
  ///
  /// If the input value does not match any of the expected values,then
  /// execution jumps to a default parse block.
  ///
  /// # Operands
  /// - `u8`: Input type enum value:
  ///
  ///   | Input Type | enum val | |
  ///   |:-|:-:|:-|
  ///   | PRODUCTION | 0 |  The value of `nterm`|
  ///   | TOKEN | 1|   The value of `tok_id`|
  ///   | BYTE | 2 | The current byte located at `scan_ptr`|
  ///   | CODEPOINT |3 |  The current ut8 codepoint located at `scan_ptr`|
  ///   | CLASS |4 |   The radlr character class of the utf8 value located at
  /// `scan_ptr`|
  /// - `u32`: Default Block Address - The relative offset the table's default
  ///   block. The absolute address can be calculated by adding this value to
  ///   the offset of the table instruction.
  /// - `u32`: Scanner Address - The address of the scanner parse block or 0.
  /// - `u32`: Lookup Table Size -The byte size of the lookup table.
  /// - `u32`: offset operand - The right hand operand of the subtract
  ///   expression used to derive the jump table index from the input value. eg
  ///   `table_index = <input value>-<offset>`
  ///
  /// This is variable length instruction.
  ///
  /// The table data starts at offset 18. Table data starts at byte offset (18
  /// + <Lookup Table Size> * 4)
  VectorBranch         = 20,
  /// Same as `VectorBranch` but use a hash table to match input values to sub
  /// parse block jump locations.
  ///
  /// # Operands
  /// - `u8`: Input type enum value:
  ///
  ///   | Input Type | enum val | |
  ///   |:-|:-:|:-|
  ///   | PRODUCTION | 0 |  The value of `nterm`|
  ///   | TOKEN | 1|   The value of `tok_id`|
  ///   | BYTE | 2 | The current byte located at `scan_ptr`|
  ///   | CODEPOINT |3 |  The current ut8 codepoint located at `scan_ptr`|
  ///   | CLASS |4 |   The radlr character class of the utf8 value located at
  /// `scan_ptr`|
  /// - `u32`: Default Block Address - The relative offset the table's default
  ///   block. The absolute address can be calculated by adding this value to
  ///   the offset of the table instruction.
  /// - `u32`: Scanner Address - The address of the scanner parse block or 0.
  /// - `u32`: Lookup Table Size -The byte size of the lookup table.
  /// - `u32`: mod operand - The right hand operand of the modulus expression
  ///   used to derive the jump table index from the input value. eg `hash_slot
  ///   = <input value>%<Mod>`
  ///
  /// This is variable length instruction.
  ///
  /// The table data starts at offset 18. Table data starts at byte offset (18
  /// + `Lookup Table Size` * 4)
  HashBranch           = 21,
  /// Matches a sequence of bytes with the input and sets the tok_len to the
  /// number of bytes matched.
  /// # Operands
  /// - 1 - [u16]: Number of bytes that need to be match.
  /// - 2 - [u32]: Offset from the base of the instruction to the default branch
  ///   if matching fails.
  /// - 3 - var_len[[u8]]: Bytes to match against the input
  ///
  /// This is variable length instruction. Its base length is 7 bytes
  ByteSequence         = 22,
  /// Directs the parse runner to split parsing into 2 or more paths.
  ///
  /// # Operands
  /// - 1 - [u16]: Number of states to initiate parsing paths.
  /// - 2 - var_len[[u32]]: A list of parsing states
  ///
  /// This is variable length instruction. Its base length is 3 bytes
  Fork                 = 23,
  /// Reads one codepoint from the input, priming line_counter and tok_len
  /// as needed. Fails if the value of the codepoint is 0
  /// This is a 1 byte instruction.
  ReadCodepoint        = 24,
}

impl From<u8> for Opcode {
  fn from(value: u8) -> Self {
    use Opcode::*;

    const LU_TABLE: [Opcode; 25] = [
      NoOp,
      Pass,
      Fail,
      ShiftChar,
      ShiftToken,
      ShiftTokenScanless,
      PeekToken,
      PeekTokenScanless,
      SkipToken,
      SkipTokenScanless,
      PeekSkipToken,
      PeekSkipTokenScanless,
      PeekReset,
      Accept,
      PopGoto,
      PushGoto,
      PushExceptionHandler,
      Goto,
      AssignToken,
      Reduce,
      VectorBranch,
      HashBranch,
      ByteSequence,
      Fork,
      ReadCodepoint,
    ];

    if (value as usize) < LU_TABLE.len() {
      LU_TABLE[value as usize]
    } else {
      NoOp
    }
  }
}

impl Into<u8> for Opcode {
  fn into(self) -> u8 {
    self as u8
  }
}

impl Opcode {
  #[track_caller]
  pub fn len(self) -> usize {
    match self {
      Opcode::Fork => {
        unimplemented!("Forks do not have fixed lengths")
      }
      Opcode::ByteSequence => {
        unimplemented!("ByteSequences do not have fixed lengths")
      }
      Opcode::HashBranch => {
        unimplemented!("HashBranches do not have fixed lengths")
      }
      Opcode::VectorBranch => {
        unimplemented!("VectorBranches do not have fixed lengths")
      }
      Opcode::Reduce => 11,
      Opcode::Goto | Opcode::PushGoto | Opcode::PushExceptionHandler => 6,
      Opcode::AssignToken => 5,
      _ => 1,
    }
  }
}

/// An instruction is a variable length slice positioned
/// at the start of an instruction opcode byte. This type
/// can be used to extract varied length octets from the
/// underlying bytecode buffer object.
#[derive(Clone, Copy)]
pub struct Instruction<'a> {
  address: usize,
  bc:      &'a [u8],
}

impl<'a> From<(&'a [u8], usize)> for Instruction<'a> {
  fn from((bytecode, opcode_start): (&'a [u8], usize)) -> Self {
    Instruction { address: opcode_start, bc: bytecode }
  }
}

impl<'a> From<&(&'a [u8], usize)> for Instruction<'a> {
  fn from((bytecode, opcode_start): &(&'a [u8], usize)) -> Self {
    Instruction { address: *opcode_start, bc: *bytecode }
  }
}

impl<'a> std::fmt::Debug for Instruction<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let opcode = self.get_opcode();
    f.write_fmt(format_args!("Instruction({:?})", opcode))
  }
}

impl<'a> Instruction<'a> {
  fn is_valid_offset(&self, byte_delta: usize) -> bool {
    self.bc.len() > self.address + byte_delta
  }

  /// Returns true if the offset of the instruction plus its size is within
  /// the bounds of the bytecode buffer
  pub fn is_valid(&self) -> bool {
    if self.get_opcode() == Opcode::Fork {
      self.next().is_some_and(|i| i.address() <= self.bc.len())
    } else if self.get_opcode() == Opcode::ByteSequence {
      self.next().is_some_and(|i| i.address() <= self.bc.len())
    } else {
      let len = self.len();
      len > 0 && (self.address + len <= self.bc.len())
    }
  }

  /// Returns the opcode found at the head position of this instruction.
  ///
  /// # Example
  ///
  /// ```
  /// use radlr_rust_runtime::types::bytecode::*;
  ///
  /// let bc = [0u8];
  ///
  /// let i :Instruction = (bc.as_slice(), 0).into();
  ///
  /// println!("{:?}", i.get_opcode()); // -> "Opcode::Noop"
  /// ```
  pub fn get_opcode(&self) -> Opcode {
    self.bc[self.address].into()
  }

  /// Returns the next instruction following this one or `None`.
  ///
  /// # Example
  ///
  /// ```
  /// use radlr_rust_runtime::types::bytecode::*;
  ///
  /// let bc = [1u8,1];
  ///
  /// let i :Instruction = (bc.as_slice(), 0).into();
  ///
  /// let i2 = i.next();
  ///
  /// assert!(i2.is_some());
  ///
  /// println!("{:?}", i2); // -> "Some(Instruction(Opcode::TokenShift))"
  /// ```
  pub fn next(self) -> Option<Instruction<'a>> {
    let Instruction { address: opcode_start, bc } = self;
    match self.get_opcode() {
      Opcode::Fork => {
        let mut iter = self.iter();
        let length = iter.next_u16_le()? as usize;
        Some((bc, opcode_start + 3 + (length << 2)).into())
      }
      Opcode::ByteSequence => {
        let mut iter = self.iter();
        let length = iter.next_u16_le()? as usize;
        Some((bc, opcode_start + 7 + length).into())
      }
      Opcode::VectorBranch | Opcode::HashBranch => {
        // Extract the address of the default branch
        let mut iter = self.iter();
        iter.next_u8(); // Skip the input enum value
        iter.next_u32_le().and_then(|v| self.is_valid_offset(v as usize).then_some((bc, opcode_start + v as usize).into()))
      }
      op => {
        let op_len = op.len();
        self.is_valid_offset(op_len).then_some((bc, opcode_start + op_len).into())
      }
    }
  }

  pub fn len(&self) -> usize {
    match self.get_opcode() {
      Opcode::VectorBranch | Opcode::HashBranch => {
        // Extract the address of the default branch
        let mut iter = self.iter();
        iter.next_u8(); // Skip the input enum value
        iter.next_u32_le().and_then(|v| self.is_valid_offset(v as usize).then_some(v as usize)).unwrap_or(0)
      }
      op => op.len(),
    }
  }

  /// Returns an `InstructionIterator` that can be used to extract operand
  /// values
  ///
  /// # Example
  /// ```
  /// use radlr_rust_runtime::types::bytecode::*;
  ///
  /// let bc = [0u8,0x10,0x32,2, 0x78, 0x56, 0x34, 0x12];
  ///
  /// let i :Instruction = (bc.as_slice(), 0).into();
  /// let mut iter = i.iter();
  ///
  /// assert_eq!(iter.next_u16_le(), Some(0x3210));
  /// assert_eq!(iter.next_u8(), Some(2));
  /// assert_eq!(iter.next_u32_le(), Some(0x12345678));
  /// assert_eq!(iter.next_u32_le(), None);
  /// ```
  pub fn iter(&self) -> ByteCodeIterator {
    ByteCodeIterator { offset: self.address + 1, bc: self.bc }
  }

  /// Returns a reference to the bytecode buffer
  pub fn bytecode(&self) -> &'a [u8] {
    self.bc
  }

  /// Returns the index of this Instruction
  pub fn address(&self) -> usize {
    self.address
  }
}

#[derive(Clone, Copy, Debug)]
pub struct ByteCodeIterator<'a> {
  offset: usize,
  bc:     &'a [u8],
}

impl<'a> From<(&'a [u8], usize)> for ByteCodeIterator<'a> {
  fn from((bc, offset): (&'a [u8], usize)) -> Self {
    Self { offset, bc }
  }
}

impl<'a> From<&(&'a [u8], usize)> for ByteCodeIterator<'a> {
  fn from((b, o): &(&'a [u8], usize)) -> Self {
    Self { offset: *o, bc: *b }
  }
}

impl<'a> ByteCodeIterator<'a> {
  fn in_bounds(&self, byte_delta: usize) -> bool {
    self.bc.len() >= self.offset + byte_delta
  }

  pub fn next_u32_le(&mut self) -> Option<u32> {
    if self.in_bounds(4) {
      let root = self.offset;
      self.offset += 4;
      let bc = self.bc;
      Some(bc[root] as u32 | ((bc[root + 1] as u32) << 8) | ((bc[root + 2] as u32) << 16) | ((bc[root + 3] as u32) << 24))
    } else {
      None
    }
  }

  pub fn next_u16_le(&mut self) -> Option<u16> {
    if self.in_bounds(2) {
      let root = self.offset;
      self.offset += 2;
      let bc = self.bc;
      Some(bc[root] as u16 | ((bc[root + 1] as u16) << 8) as u16)
    } else {
      None
    }
  }

  pub fn next_u8(&mut self) -> Option<u8> {
    if self.in_bounds(1) {
      let root = self.offset;
      self.offset += 1;
      Some(self.bc[root])
    } else {
      None
    }
  }
}

pub fn insert_op(bc: &mut Vec<u8>, val: Opcode) {
  bc.push(val as u8);
}

pub fn insert_u8(bc: &mut Vec<u8>, val: u8) {
  bc.push(val as u8);
}

pub fn insert_u16_le(bc: &mut Vec<u8>, val: u16) {
  bc.push((val & 0xFF) as u8);
  bc.push((val >> 8 & 0xFF) as u8);
}

pub fn insert_u32_le(bc: &mut Vec<u8>, val: u32) {
  bc.push((val & 0xFF) as u8);
  bc.push((val >> 8 & 0xFF) as u8);
  bc.push((val >> 16 & 0xFF) as u8);
  bc.push((val >> 24 & 0xFF) as u8);
}

pub fn set_u32_le(bc: &mut Vec<u8>, offset: usize, val: u32) {
  bc[offset + 0] = (val & 0xFF) as u8;
  bc[offset + 1] = (val >> 8 & 0xFF) as u8;
  bc[offset + 2] = (val >> 16 & 0xFF) as u8;
  bc[offset + 3] = (val >> 24 & 0xFF) as u8;
}

// Bit mask for bytecode states that are active during failure
// recovery mode
pub const FAIL_STATE_FLAG: u32 = 1 << 1;

/// Bit mask for bytecode states that are active during normal parse
/// mode
pub const NORMAL_STATE_FLAG: u32 = 1 << 0;

pub const STATE_HEADER: u32 = 1 << 2;

/// The offset of the first state within any radlr bytecode buffer.
pub const FIRST_PARSE_BLOCK_ADDRESS: u32 = 8;

pub const TOKEN_ASSIGN_FLAG: u32 = 0x04000000;

pub const END_OF_INPUT_TOKEN_ID: u32 = 0x1;

/// Internal input types used in IR [parser::Matches] statements.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
#[repr(u32)]
#[cfg_attr(feature = "wasm-lab", wasm_bindgen)]
pub enum MatchInputType {
  /// Matches the last reduced nonterminal id
  NonTerminal       = 0,
  /// Matches the token id set from a scanner call
  Token             = 1,
  /// Matches the class of a character in the input
  Class             = 2,
  /// Matches a utf8 codepoint byte sequence in the input
  Codepoint         = 3,
  /// Matches a byte in the input
  Byte              = 4,
  /// Matches the virtual $eof token in the input stream
  EndOfFile         = 5,
  /// Matches anything
  Default           = 6,
  /// Matches a byte in the input
  ByteScanless      = 7,
  /// Matches a utf8 codepoint byte sequence in the input
  CodepointScanless = 8,
  /// Matches the class of a character in the input
  ClassScanless     = 9,
  /// Matches a distinct sequence of bytes.
  ByteSequence      = 10,
  /// Matches the top node on the output stack.
  CSTNode           = 11,
}

impl MatchInputType {
  pub const BYTE_SCANLESS_STR: &'static str = "_BYTE_SCANLESS_";
  pub const BYTE_SEQUENCE_STR: &'static str = "_BYTE_SEQUENCE_";
  pub const BYTE_STR: &'static str = "_BYTE_";
  pub const CLASS_SCANLESS_STR: &'static str = "_CLASS_SCANLESS_";
  pub const CLASS_STR: &'static str = "_CLASS_";
  pub const CODEPOINT_SCANLESS_STR: &'static str = "_CODEPOINT_SCANLESS_";
  pub const CODEPOINT_STR: &'static str = "_CODEPOINT_";
  pub const CST_NODE: &'static str = "_CST_NODE_";
  pub const END_OF_FILE_STR: &'static str = "_EOF_";
  pub const NONTERMINAL_STR: &'static str = "_NONTERMINAL_";
  pub const TOKEN_STR: &'static str = "_TOKEN_";

  pub fn as_str(&self) -> &'static str {
    match self {
      Self::NonTerminal => MatchInputType::NONTERMINAL_STR,
      Self::Token => MatchInputType::TOKEN_STR,
      Self::Class => MatchInputType::CLASS_STR,
      Self::ClassScanless => MatchInputType::CLASS_SCANLESS_STR,
      Self::Codepoint => MatchInputType::CODEPOINT_STR,
      Self::CodepointScanless => MatchInputType::CODEPOINT_SCANLESS_STR,
      Self::Byte => MatchInputType::BYTE_STR,
      Self::ByteScanless => MatchInputType::BYTE_SCANLESS_STR,
      Self::EndOfFile => MatchInputType::END_OF_FILE_STR,
      Self::ByteSequence => MatchInputType::BYTE_SEQUENCE_STR,
      Self::CSTNode => MatchInputType::CST_NODE,
      Self::Default => "",
    }
  }

  pub fn to_scanless(&self) -> Self {
    match self {
      Self::Class => Self::ClassScanless,
      Self::Byte => Self::ByteScanless,
      Self::Codepoint => Self::CodepointScanless,
      _ => *self,
    }
  }
}

impl Display for MatchInputType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(self.as_str())
  }
}

impl From<u32> for MatchInputType {
  fn from(value: u32) -> Self {
    match value {
      0 => Self::NonTerminal,
      1 => Self::Token,
      2 => Self::Class,
      3 => Self::Codepoint,
      4 => Self::Byte,
      5 => Self::EndOfFile,
      7 => Self::ByteScanless,
      8 => Self::CodepointScanless,
      9 => Self::ClassScanless,
      10 => Self::ByteSequence,
      11 => Self::CSTNode,
      _ => unreachable!(),
    }
  }
}

impl From<&str> for MatchInputType {
  fn from(value: &str) -> Self {
    match value {
      Self::BYTE_SCANLESS_STR => Self::ByteScanless,
      Self::BYTE_STR => Self::Byte,
      Self::CLASS_SCANLESS_STR => Self::ClassScanless,
      Self::CLASS_STR => Self::Class,
      Self::CODEPOINT_SCANLESS_STR => Self::CodepointScanless,
      Self::CODEPOINT_STR => Self::Codepoint,
      Self::END_OF_FILE_STR => Self::EndOfFile,
      Self::NONTERMINAL_STR => Self::NonTerminal,
      Self::TOKEN_STR => Self::Token,
      Self::BYTE_SEQUENCE_STR => Self::ByteSequence,
      Self::CST_NODE => Self::CSTNode,
      "PRODUCTION" => Self::NonTerminal,
      "TOKEN" => Self::Token,
      "CLASS" => Self::Class,
      "CODEPOINT" => Self::Codepoint,
      "BYTE" => Self::Byte,
      "EOF" => Self::EndOfFile,
      _ => {
        #[cfg(debug_assertions)]
        unreachable!("Unexpected InputType (for IR Matches) specifier: [{}];\nExpected one of {:?}", value, [
          MatchInputType::CLASS_STR,
          MatchInputType::CODEPOINT_STR,
          MatchInputType::BYTE_STR,
          MatchInputType::END_OF_FILE_STR,
        ]);
        #[cfg(not(debug_assertions))]
        unreachable!()
      }
    }
  }
}

impl From<String> for MatchInputType {
  fn from(value: String) -> Self {
    Self::from(value.as_str())
  }
}

pub enum BranchSelector {
  Hash,
  Vector,
}

/// values - The set of keys used to select a branch to jump to.
/// branches - An vector of branch bytecode vectors.
pub type GetBranchSelector = fn(values: &[u32], max_span: u32, branches: &[Vec<u8>]) -> BranchSelector;

pub fn default_get_branch_selector(values: &[u32], max_span: u32, branches: &[Vec<u8>]) -> BranchSelector {
  // Hash table limitations:
  // Max supported item value: 2046 with skip set to 2048
  // Max number of values: 1024 (maximum jump span)
  // Max instruction offset from table header 2042

  let total_instruction_length = branches.iter().map(|b| b.len()).sum::<usize>();

  let has_unsupported_value = values.iter().cloned().any(|v| v > 2046);

  if (max_span < 2) || total_instruction_length > 2042 || has_unsupported_value {
    BranchSelector::Vector
  } else {
    BranchSelector::Hash
  }
}
