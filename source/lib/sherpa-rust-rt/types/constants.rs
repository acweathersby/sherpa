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

#[non_exhaustive]

pub struct InputType;

impl InputType {
  pub const T01_PRODUCTION: u32 = 0;
  pub const T02_TOKEN: u32 = 1;
  pub const T03_CLASS: u32 = 2;
  pub const T04_CODEPOINT: u32 = 3;
  pub const T05_BYTE: u32 = 4;

  pub fn to_string(val: u32) -> &'static str {
    match val {
      Self::T01_PRODUCTION => "PRODUCTION",
      Self::T02_TOKEN => "TOKEN",
      Self::T03_CLASS => "CLASS",
      Self::T04_CODEPOINT => "CODEPOINT",
      Self::T05_BYTE => "BYTE",
      _ => "",
    }
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

  let total_instruction_length = branches.iter().map(|b| b.len()).sum::<usize>();

  let has_unsupported_value = values.iter().cloned().any(|v| v > 2046);

  if (max_span < 2) || total_instruction_length > 2042 || has_unsupported_value {
    BranchSelector::Vector
  } else {
    BranchSelector::Hash
  }
}
