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

/// Mask the part of the state metadata that stores the
/// production id.
pub const PRODUCTION_META_MASK: u64 = 0xFFFFF;

/// Mask the part of the state metadata that stores the
/// production id.
pub const PRODUCTION_META_MASK_INVERT: u64 = !PRODUCTION_META_MASK;

// Bit mask for bytecode states that are active during failure
// recovery mode
pub const FAIL_STATE_FLAG: u32 = 1 << 27;

/// Bit mask for bytecode states that are active during normal parse
/// mode
pub const NORMAL_STATE_FLAG: u32 = 1 << 26;

pub const STATE_MODE_MASK: u32 = FAIL_STATE_FLAG | NORMAL_STATE_FLAG;

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

pub const TOKEN_ASSIGN_FLAG: u32 = 0x04000000;

pub const END_OF_INPUT_TOKEN_ID: u32 = 0x1;

pub enum InstructionType {
  PASS       = 0,
  SHIFT      = 1,
  GOTO       = 2,
  SET_PROD   = 3,
  REDUCE     = 4,
  TOKEN      = 5,
  FORK_TO    = 6,
  SCAN       = 7,
  EAT_CRUMBS = 8,
  VECTOR_BRANCH = 9,
  HASH_BRANCH = 10,
  SET_FAIL_STATE = 11,
  REPEAT     = 12,
  NOOP13     = 13,
  ASSERT_SHIFT = 14,
  FAIL       = 15,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
/// Bytecode instruction constants
pub struct INSTRUCTION(pub u32, usize);

impl INSTRUCTION {
  pub const I00_PASS: u32 = 0;
  pub const I01_SHIFT: u32 = 1 << 28;
  pub const I02_GOTO: u32 = 2 << 28;
  pub const I03_SET_PROD: u32 = 3 << 28;
  pub const I04_REDUCE: u32 = 4 << 28;
  pub const I05_TOKEN: u32 = 5 << 28;
  pub const I05_TOKEN_ASSIGN: u32 = INSTRUCTION::I05_TOKEN | TOKEN_ASSIGN_FLAG;
  pub const I05_TOKEN_ASSIGN_SHIFT: u32 = INSTRUCTION::I05_TOKEN | 0x09000000;
  pub const I05_TOKEN_LENGTH: u32 = INSTRUCTION::I05_TOKEN | 0x08000000;
  pub const I06_FORK_TO: u32 = 6 << 28;
  pub const I07_SCAN: u32 = 7 << 28;
  pub const I07_SCAN_BACK_UNTIL: u32 = INSTRUCTION::I07_SCAN | 0x00100000;
  pub const I08_EAT_CRUMBS: u32 = 8 << 28;
  pub const I09_VECTOR_BRANCH: u32 = 9 << 28;
  pub const I10_HASH_BRANCH: u32 = 10 << 28;
  pub const I11_SET_FAIL_STATE: u32 = 11 << 28;
  pub const I12_REPEAT: u32 = 12 << 28;
  pub const I13_NOOP: u32 = 13 << 28;
  pub const I14_ASSERT_SHIFT: u32 = 14 << 28;
  pub const I15_FAIL: u32 = 15 << 28;
  pub const I15_FALL_THROUGH: u32 = 15 << 28 | 1;

  pub fn Pass() -> INSTRUCTION {
    INSTRUCTION(0, 1)
  }

  pub fn Fail() -> INSTRUCTION {
    INSTRUCTION(0, 2)
  }

  pub fn is_valid(&self) -> bool {
    self.1 > 0
  }

  pub fn from(bc: &[u32], address: usize) -> Self {
    if (address > bc.len()) {
      INSTRUCTION(0, 0)
    } else {
      INSTRUCTION(bc[address], address)
    }
  }

  pub fn next(&self, bc: &[u32]) -> Self {
    if (self.1 >= bc.len() - 1) {
      INSTRUCTION(0, 0)
    } else {
      INSTRUCTION(bc[self.1 + 1], self.1 + 1)
    }
  }

  pub fn goto(&self, bc: &[u32]) -> Self {
    match self.to_type() {
      InstructionType::GOTO => Self::from(bc, (self.0 & GOTO_STATE_ADDRESS_MASK) as usize),
      _ => INSTRUCTION(0, 0),
    }
  }

  pub fn get_address(&self) -> usize {
    self.1
  }

  pub fn get_value(&self) -> u32 {
    self.0
  }

  pub fn is_PASS(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I00_PASS
  }

  pub fn is_SHIFT(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I01_SHIFT
  }

  pub fn is_GOTO(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I02_GOTO
  }

  pub fn is_SET_PROD(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I03_SET_PROD
  }

  pub fn is_REDUCE(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I04_REDUCE
  }

  pub fn is_TOKEN(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I05_TOKEN
  }

  pub fn is_FORK(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I06_FORK_TO
  }

  pub fn is_SCAN(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I07_SCAN
  }

  pub fn is_NOOP8(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I08_EAT_CRUMBS
  }

  pub fn is_VECTOR_BRANCH(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I09_VECTOR_BRANCH
  }

  pub fn is_HASH_BRANCH(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I10_HASH_BRANCH
  }

  pub fn is_SET_FAIL_STATE(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I11_SET_FAIL_STATE
  }

  pub fn is_REPEAT(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I12_REPEAT
  }

  pub fn is_NOOP13(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I13_NOOP
  }

  pub fn is_ASSERT_SHIFT(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I14_ASSERT_SHIFT
  }

  pub fn is_FAIL(&self) -> bool {
    (self.0 & INSTRUCTION_HEADER_MASK) == Self::I15_FAIL
  }

  pub fn get_contents(&self) -> u32 {
    self.0 & INSTRUCTION_CONTENT_MASK
  }

  pub fn get_type(&self) -> u32 {
    self.0 & INSTRUCTION_HEADER_MASK
  }

  pub fn to_type(&self) -> InstructionType {
    match self.0 & INSTRUCTION_HEADER_MASK {
      Self::I00_PASS => InstructionType::PASS,
      Self::I01_SHIFT => InstructionType::SHIFT,
      Self::I02_GOTO => InstructionType::GOTO,
      Self::I03_SET_PROD => InstructionType::SET_PROD,
      Self::I04_REDUCE => InstructionType::REDUCE,
      Self::I05_TOKEN => InstructionType::TOKEN,
      Self::I06_FORK_TO => InstructionType::FORK_TO,
      Self::I07_SCAN => InstructionType::SCAN,
      Self::I08_EAT_CRUMBS => InstructionType::EAT_CRUMBS,
      Self::I09_VECTOR_BRANCH => InstructionType::VECTOR_BRANCH,
      Self::I10_HASH_BRANCH => InstructionType::HASH_BRANCH,
      Self::I11_SET_FAIL_STATE => InstructionType::SET_FAIL_STATE,
      Self::I12_REPEAT => InstructionType::REPEAT,
      Self::I13_NOOP => InstructionType::NOOP13,
      Self::I14_ASSERT_SHIFT => InstructionType::ASSERT_SHIFT,
      Self::I15_FAIL => InstructionType::FAIL,
      _ => InstructionType::PASS,
    }
  }

  pub fn to_str(&self) -> &str {
    match self.0 & INSTRUCTION_HEADER_MASK {
      Self::I00_PASS => "I00_PASS",
      Self::I01_SHIFT => "I01_SHIFT",
      Self::I02_GOTO => "I02_GOTO",
      Self::I03_SET_PROD => "I03_SET_PROD",
      Self::I04_REDUCE => "I04_REDUCE",
      Self::I05_TOKEN => "I05_TOKEN",
      Self::I06_FORK_TO => "I06_FORK_TO",
      Self::I07_SCAN => "I07_SCAN",
      Self::I08_EAT_CRUMBS => "I08_NOOP",
      Self::I09_VECTOR_BRANCH => "I09_VECTOR_BRANCH",
      Self::I10_HASH_BRANCH => "I10_HASH_BRANCH",
      Self::I11_SET_FAIL_STATE => "I11_SET_FAIL_STATE",
      Self::I12_REPEAT => "I12_REPEAT",
      Self::I13_NOOP => "I13_NOOP",
      Self::I14_ASSERT_SHIFT => "I14_ASSERT_SHIFT",
      Self::I15_FAIL => "I15_FAIL",
      _ => "Undefined",
    }
  }
}

#[non_exhaustive]

pub struct INPUT_TYPE;

impl INPUT_TYPE {
  pub const T01_PRODUCTION: u32 = 0;
  pub const T02_TOKEN: u32 = 1;
  pub const T03_CLASS: u32 = 2;
  pub const T04_CODEPOINT: u32 = 3;
  pub const T05_BYTE: u32 = 4;
}

#[non_exhaustive]
pub struct LEXER_TYPE;
impl LEXER_TYPE {
  pub const ASSERT: u32 = 1;
  pub const PEEK: u32 = 2;
}

pub enum BranchSelector {
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
) -> BranchSelector {
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
