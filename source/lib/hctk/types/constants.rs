// Global Constants
pub const STATE_ADDRESS_MASK: u32 = (1 << 24) - 1;

/// The portion of a GOTO instruction that contains the state offset.
/// Alias of [STATE_INDEX_MASK]
pub const GOTO_STATE_ADDRESS_MASK: u32 = STATE_ADDRESS_MASK;

/// The portion of an instruction that stores inline data.
/// Masks out the instruction header.
pub const INSTRUCTION_CONTENT_MASK: u32 = 0xFFF_FFFF;

/// The portion of a instruction that contains the instruction's
/// type.
pub const INSTRUCTION_HEADER_MASK: u32 = 0xF000_0000;

pub const SKIPPED_SCAN_PROD: u16 = 9009;

// Bit mask for bytecode states that are active during failure
// recovery mode
pub const FAIL_STATE_MASK: u32 = 1 << 27;

/// Mask the part of the state metadata that stores the
/// production id.
pub const PRODUCTION_META_MASK: u64 = 0xFFFFF;

/// Mask the part of the state metadata that stores the
/// production id.
pub const PRODUCTION_META_MASK_INVERT: u64 = !PRODUCTION_META_MASK;

/// Bit mask for bytecode states that are active during normal parse
/// mode
pub const NORMAL_STATE_MASK: u32 = 1 << 26;

pub const PEEK_MODE_FLAG: u32 = 1 << 28;

/// This is the standard location of a `fail` instruction that is
/// present in all bytecode blocks produced by Hydrocarbon.
pub const DEFAULT_FAIL_INSTRUCTION_ADDRESS: u32 = 2;

/// This is the standard location of a `pass-through` instruction that
/// is present in all bytecode blocks produced by Hydrocarbon.
pub const DEFAULT_PASS_THROUGH_INSTRUCTION_ADDRESS: u32 = 0;

/// This is the standard location of a `pass` instruction that is
/// present in all bytecode blocks produced by Hydrocarbon.
pub const DEFAULT_PASS_INSTRUCTION_ADDRESS: u32 = 1;

/// The offset of the first state within any HC bytecode buffer.
pub const FIRST_STATE_ADDRESS: u32 = 6;

///  A "magic" number assigned to a reduce node's length
/// value to indicate that it is to use the symbol accumulator
/// to determine how many symbols are to removed from the
/// parse stack when the reduce function is called.
pub const IR_REDUCE_NUMERIC_LEN_ID: u32 = 0x90FA0102;

pub const END_ITEM_ADDENDUM: u32 = 1 << 20;

pub const LOCAL_STATE: u32 = 0;

pub const GLOBAL_STATE: i32 = -1;

pub const DEFAULT_CASE_INDICATOR: u32 = 9009;

pub const STATE_BYTECODE_BYTE_START: u32 = 24;

/// Bit mask for bytecode states that are GOTO states
pub const GOTO_STATE_MASK: u32 = 1 << 25;

/// Bit mask for bytecode states that are SCANNER states
pub const SCANNER_STATE_MASK: u32 = 1 << 24;

pub const ALPHA_INCREMENT_STACK_POINTER_MASK: u32 = 1 << 0;

pub const ALPHA_HAVE_DEFAULT_ACTION_MASK: u32 = 1 << 1;

pub const PRODUCTION_SCOPE_POP_POINTER: u32 = 2;

pub const TOKEN_ASSIGN_FLAG: u32 = 0x04000000;

/// Bytecode instruction constants
pub struct INSTRUCTION(pub u32);

impl INSTRUCTION
{
  pub const I00_PASS: u32 = 0;
  pub const I01_CONSUME: u32 = 1 << 28;
  pub const I02_GOTO: u32 = 2 << 28;
  pub const I03_SET_PROD: u32 = 3 << 28;
  pub const I04_REDUCE: u32 = 4 << 28;
  pub const I05_TOKEN: u32 = 5 << 28;
  pub const I05_TOKEN_ASSIGN: u32 = INSTRUCTION::I05_TOKEN | TOKEN_ASSIGN_FLAG;
  pub const I05_TOKEN_ASSIGN_CONSUME: u32 = INSTRUCTION::I05_TOKEN | 0x09000000;
  pub const I05_TOKEN_LENGTH: u32 = INSTRUCTION::I05_TOKEN | 0x08000000;
  pub const I06_FORK_TO: u32 = 6 << 28;
  pub const I07_SCAN: u32 = 7 << 28;
  pub const I07_SCAN_BACK_UNTIL: u32 = INSTRUCTION::I07_SCAN | 0x00100000;
  pub const I08_NOOP: u32 = 8 << 28;
  pub const I09_VECTOR_BRANCH: u32 = 9 << 28;
  pub const I10_HASH_BRANCH: u32 = 10 << 28;
  pub const I11_SET_FAIL_STATE: u32 = 11 << 28;
  pub const I12_REPEAT: u32 = 12 << 28;
  pub const I13_NOOP: u32 = 13 << 28;
  pub const I14_ASSERT_CONSUME: u32 = 14 << 28;
  pub const I15_FAIL: u32 = 15 << 28;
  pub const I15_FALL_THROUGH: u32 = 15 << 28 | 1;

  pub fn is_I00_PASS(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I00_PASS
  }

  pub fn is_I01_CONSUME(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I01_CONSUME
  }

  pub fn is_I02_GOTO(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I02_GOTO
  }

  pub fn is_I03_SET_PROD(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I03_SET_PROD
  }

  pub fn is_I04_REDUCE(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I04_REDUCE
  }

  pub fn is_I05_TOKEN(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I05_TOKEN
  }

  pub fn is_I06_FORK_TO(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I06_FORK_TO
  }

  pub fn is_I07_SCAN(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I07_SCAN
  }

  pub fn is_I08_NOOP(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I08_NOOP
  }

  pub fn is_I09_VECTOR_BRANCH(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I09_VECTOR_BRANCH
  }

  pub fn is_I10_HASH_BRANCH(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I10_HASH_BRANCH
  }

  pub fn is_I11_SET_FAIL_STATE(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I11_SET_FAIL_STATE
  }

  pub fn is_I12_REPEAT(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I12_REPEAT
  }

  pub fn is_I13_NOOP(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I13_NOOP
  }

  pub fn is_I14_ASSERT_CONSUME(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I14_ASSERT_CONSUME
  }

  pub fn is_I15_FAIL(&self) -> bool
  {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I15_FAIL
  }

  pub fn get_contents(&self) -> u32
  {
    self.0 & INSTRUCTION_CONTENT_MASK
  }

  pub fn get_type(&self) -> u32
  {
    self.0 & INSTRUCTION_HEADER_MASK
  }

  pub fn to_str(&self) -> &str
  {
    match (self.0 & INSTRUCTION_HEADER_MASK) {
      Self::I00_PASS => "I00_PASS",
      Self::I01_CONSUME => "I01_CONSUME",
      Self::I02_GOTO => "I02_GOTO",
      Self::I03_SET_PROD => "I03_SET_PROD",
      Self::I04_REDUCE => "I04_REDUCE",
      Self::I05_TOKEN => "I05_TOKEN",
      Self::I06_FORK_TO => "I06_FORK_TO",
      Self::I07_SCAN => "I07_SCAN",
      Self::I08_NOOP => "I08_NOOP",
      Self::I09_VECTOR_BRANCH => "I09_VECTOR_BRANCH",
      Self::I10_HASH_BRANCH => "I10_HASH_BRANCH",
      Self::I11_SET_FAIL_STATE => "I11_SET_FAIL_STATE",
      Self::I12_REPEAT => "I12_REPEAT",
      Self::I13_NOOP => "I13_NOOP",
      Self::I14_ASSERT_CONSUME => "I14_ASSERT_CONSUME",
      Self::I15_FAIL => "I15_FAIL",
      _ => "Undefined",
    }
  }
}

#[non_exhaustive]

pub struct INPUT_TYPE;

impl INPUT_TYPE
{
  pub const T01_PRODUCTION: u32 = 0;
  pub const T02_TOKEN: u32 = 1;
  pub const T03_CLASS: u32 = 2;
  pub const T04_CODEPOINT: u32 = 3;
  pub const T05_BYTE: u32 = 4;
}

#[non_exhaustive]
pub struct LEXER_TYPE;
impl LEXER_TYPE
{
  pub const ASSERT: u32 = 1;
  pub const PEEK: u32 = 2;
}

pub enum BranchSelector
{
  Hash,
  Vector,
}

/// values - The set of keys used to select a branch to jump to.
/// branches - An vector of branch bytecode vectors.
pub type GetBranchSelector =
  fn(values: &[u32], max_span: u32, branches: &[Vec<u32>]) -> BranchSelector;

pub fn default_get_branch_selector(
  values: &[u32],
  max_span: u32,
  branches: &[Vec<u32>],
) -> BranchSelector
{
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
