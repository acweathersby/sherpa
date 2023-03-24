//! Convenient types for working with bytecode parser data.

use std::fmt::{Debug, Display};

/// The current set of instruction opcodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Opcode {
  /// Default value for unrecognized opcode values.
  NoOp = 0,
  /// A sentinel instruction that ends the execution of a parse block, similar
  /// to a x86 `ret` instruction.
  ///
  /// This is a single byte instruction.
  Pass,
  /// Ends the execution of the current parse block, and causes the parser to
  /// enter exception mode, popping parse blocks of the goto stack until it
  /// encounters an exception handling parse block that resets the execution
  /// mode to normal.
  ///
  /// If the execution mode is not reset to normal by the last parse block,
  /// then the parser emits an `Error` event and halts further parsing.
  ///
  /// This is a single byte instruction.
  Fail,
  /// Increments the `scan_ptr` by the value of `sym_len` and then sets
  /// `sym_len` to 0.
  ///
  /// This is a single byte instruction.
  ScanShift,
  /// Shifts the current token value, moving  the `anchor_ptr`, `base_ptr`,
  /// `scan_ptr`, and `head_ptr` to the position of `head_ptr + tok_len`.
  /// Also set `tok_id` to 0.
  ///
  /// This also causes a `Shift` event to be emitted, pausing the parser until
  /// it is resumed through a call to `next`.
  ///
  /// This is a single byte instruction.
  ShiftToken,
  /// Same as `ShiftToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `ShiftToken` instructions.
  ///
  /// This also causes a `Shift` event to be emitted, pausing the parser until
  /// it is resumed through a call to `next`.
  ///
  /// This is a single byte instruction.
  ShiftTokenScanless,
  /// Assigns the value `head_ptr + tok_len` to `head_ptr` and `scan_ptr`. Also
  /// sets the values of `tok_len` and `tok_id` to 0.
  ///
  /// This is a single byte instruction.
  PeekToken,
  /// Same as `PeekToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `PeekToken` instructions.
  ///
  /// This is a single byte instruction.
  PeekTokenScanless,
  /// Assigns `head_ptr + tok_len` to `head_ptr`, `scan_ptr`, and `base_ptr`.
  /// Then jumps to the first instruction of the current parse block.
  ///
  /// This is a single byte instruction.
  SkipToken,
  /// Same as `SkipToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `SkipToken` instruction.
  ///
  /// This is a single byte instruction.
  SkipTokenScanless,
  /// Assigns `head_ptr + tok_len` to `head_ptr` and `scan_ptr`. Then jumps to
  /// the first instruction of the current arse block.
  ///
  /// This is a single byte instruction.
  PeekSkipToken,
  /// Same as `PeekSkipToken`, but assigns `sym_len` to `tok_len` before
  /// executing the `PeekSkipToken` instructions.
  ///
  /// This is a single byte instruction.
  PeekSkipTokenScanless,
  /// Assigns the value of `base_ptr` to `scan_ptr` and `head_ptr`. Also sets
  /// `tok_len`, `sym_len`, and `tok_id` to `0`.
  ///
  /// This is a single byte instruction.
  PeekReset,
  /// Accepts the parsed input as a valid string of the grammar.
  ///
  /// This is a single byte instruction
  Accept,
  /// Removes the top goto from the goto stack
  ///
  /// This is a single byte instruction.
  PopGoto,
  /// Pushes the address of a parse block onto the goto stack.
  ///
  /// # Operands
  /// - u8: Parse Mode - The mode the parser must be in to allow execution of
  ///   this block.
  /// - u32: Parse Block Address -The address of the parse block to jump to.
  ///
  /// This is a 6 byte instruction.
  PushGoto,
  /// Pushes the address of an exception handler block onto the goto stack.
  ///
  /// # Operands
  /// - u8: Parse Mode - The mode the parser must be in to allow execution of
  ///   this block.
  /// - u32: Parse Block Address -The address of the parse block to jump to.
  ///
  /// This is a 6 byte instruction.
  PushExceptionHandler,
  /// Jumps to the location specified.
  ///
  /// # Operands
  /// - u8: Parse Mode - The mode the parser must be in to allow execution of
  ///   this block.
  /// - u32: Parse Block Address -The address of the parse block to jump to.
  ///
  /// This is a 6 byte instruction.
  Goto,
  /// Assigns a token_id value to `tok_id` and assigns `scan_ptr - head_ptr` to
  /// `tok_len`.
  ///
  /// # Operands
  /// - u32: TokenID
  ///
  /// This is a 5 byte instruction.
  AssignToken,

  /// Assigns a production_id to `prod_id`, a rule_id to `rule_id` and the
  /// number of values to pop of the stack to `sym_len`.
  ///
  /// This also causes a `Reduce` event to be emitted, pausing the parser until
  /// it is resumed through a call to `next`.
  ///
  /// # Operands
  /// - u32: ProductionId - The value that will be assigned to `prod_id`.
  /// - u32: RuleId -The value that will be assigned to `rule_id`.
  /// - u16: Num of Symbols - The value that will be assigned to `sym_len`
  ///
  /// This is an 11 byte instruction.
  Reduce,
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
  ///   | PRODUCTION | 0 |  The value of `prod_id`|
  ///   | TOKEN | 1|   The value of `tok_id`|
  ///   | BYTE | 2 | The current byte located at `scan_ptr`|
  ///   | CODEPOINT |3 |  The current ut8 codepoint located at `scan_ptr`|
  ///   | CLASS |4 |   The sherpa character class of the utf8 value located at
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
  VectorBranch,
  /// Same as `VectorBranch` but use a hash table to match input values to sub
  /// parse block jump locations.
  ///
  /// # Operands
  /// - `u8`: Input type enum value:
  ///
  ///   | Input Type | enum val | |
  ///   |:-|:-:|:-|
  ///   | PRODUCTION | 0 |  The value of `prod_id`|
  ///   | TOKEN | 1|   The value of `tok_id`|
  ///   | BYTE | 2 | The current byte located at `scan_ptr`|
  ///   | CODEPOINT |3 |  The current ut8 codepoint located at `scan_ptr`|
  ///   | CLASS |4 |   The sherpa character class of the utf8 value located at
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
  HashBranch,
}

impl From<u8> for Opcode {
  fn from(value: u8) -> Self {
    use Opcode::*;

    const LU_TABLE: [Opcode; 22] = [
      NoOp,
      Pass,
      Fail,
      ScanShift,
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

impl<'a> Debug for Instruction<'a> {
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
    let len = self.len();
    len > 0 && (self.address + len <= self.bc.len())
  }

  /// Returns the opcode found at the head position of this instruction.
  ///
  /// # Example
  ///
  /// ```
  /// use sherpa_runtime::types::bytecode::*;
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
  /// use sherpa_runtime::types::bytecode::*;
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
    let Instruction { address: opcode_start, bc: bytecode } = self;
    match self.get_opcode() {
      Opcode::VectorBranch | Opcode::HashBranch => {
        // Extract the address of the default branch
        let mut iter = self.iter();
        iter.next_u8(); // Skip the input enum value
        iter.next_u32_le().and_then(|v| {
          self
            .is_valid_offset(v as usize)
            .then_some((bytecode, opcode_start + v as usize).into())
        })
      }
      op => {
        let op_len = op.len();
        self
          .is_valid_offset(op_len)
          .then_some((bytecode, opcode_start + op_len).into())
      }
    }
  }

  pub fn len(&self) -> usize {
    match self.get_opcode() {
      Opcode::VectorBranch | Opcode::HashBranch => {
        // Extract the address of the default branch
        let mut iter = self.iter();
        iter.next_u8(); // Skip the input enum value
        iter
          .next_u32_le()
          .and_then(|v| self.is_valid_offset(v as usize).then_some(v as usize))
          .unwrap_or(0)
      }
      op => op.len(),
    }
  }

  /// Returns an `InstructionIterator` that can be used to extract operand
  /// values
  ///
  /// # Example
  /// ```
  /// use sherpa_runtime::types::bytecode::*;
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
      Some(
        bc[root] as u32
          | ((bc[root + 1] as u32) << 8)
          | ((bc[root + 2] as u32) << 16)
          | ((bc[root + 3] as u32) << 24),
      )
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
      let bc = self.bc;
      Some(bc[root])
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

/// The offset of the first state within any HC bytecode buffer.
pub const FIRST_PARSE_BLOCK_ADDRESS: u32 = 8;

pub const TOKEN_ASSIGN_FLAG: u32 = 0x04000000;

pub const END_OF_INPUT_TOKEN_ID: u32 = 0x1;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
#[repr(u32)]
pub enum InputType {
  Production = 0,
  Token,
  Class,
  Codepoint,
  Byte,
  EndOfFile,
  Default,
}

impl InputType {
  pub const BYTE_STR: &'static str = "_BYTE_";
  pub const CLASS_STR: &'static str = "_CLASS_";
  pub const CODEPOINT_STR: &'static str = "_CODEPOINT_";
  pub const END_OF_FILE_STR: &'static str = "_EOF_";
  pub const PRODUCTION_STR: &'static str = "_PRODUCTION_";
  pub const TOKEN_STR: &'static str = "_TOKEN_";

  pub fn as_str(&self) -> &'static str {
    match self {
      Self::Production => InputType::PRODUCTION_STR,
      Self::Token => InputType::TOKEN_STR,
      Self::Class => InputType::CLASS_STR,
      Self::Codepoint => InputType::CODEPOINT_STR,
      Self::Byte => InputType::BYTE_STR,
      Self::EndOfFile => InputType::END_OF_FILE_STR,
      Self::Default => "",
    }
  }
}

impl Display for InputType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(self.as_str())
  }
}

impl From<u32> for InputType {
  fn from(value: u32) -> Self {
    match value {
      0 => Self::Production,
      1 => Self::Token,
      2 => Self::Class,
      3 => Self::Codepoint,
      4 => Self::Byte,
      5 => Self::EndOfFile,
      _ => unreachable!(),
    }
  }
}

impl From<&str> for InputType {
  fn from(value: &str) -> Self {
    match value {
      "PRODUCTION" => Self::Production,
      "TOKEN" => Self::Token,
      "CLASS" => Self::Class,
      "CODEPOINT" => Self::Codepoint,
      "BYTE" => Self::Byte,
      "EOF" => Self::EndOfFile,
      _ => unreachable!(),
    }
  }
}

impl From<String> for InputType {
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
pub type GetBranchSelector =
  fn(values: &[u32], max_span: u32, branches: &[Vec<u8>]) -> BranchSelector;

pub fn default_get_branch_selector(
  values: &[u32],
  max_span: u32,
  branches: &[Vec<u8>],
) -> BranchSelector {
  // Hash table limitations:
  // Max supported item value: 2046 with skip set to 2048
  // Max number of values: 1024 (maximum jump span)
  // Max instruction offset from table header 2042

  let total_instruction_length =
    branches.iter().map(|b| b.len()).sum::<usize>();

  let has_unsupported_value = values.iter().cloned().any(|v| v > 2046);

  if (max_span < 2) || total_instruction_length > 2042 || has_unsupported_value
  {
    BranchSelector::Vector
  } else {
    BranchSelector::Hash
  }
}
