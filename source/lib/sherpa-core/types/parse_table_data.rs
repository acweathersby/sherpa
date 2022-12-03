use super::*;

#[derive(Debug, Clone, Copy)]
pub struct TableHeaderData {
  pub input_type:   u32,
  pub lexer_type:   u32,
  pub table_length: u32,
  pub table_meta:   u32,
  pub scan_index:   INSTRUCTION,
}

impl TableHeaderData {
  #[inline(always)]
  pub fn from_bytecode(offset: usize, bc: &[u32]) -> Self {
    let i = offset;

    let (first, scanner_address, third) = unsafe {
      let v = bc.get_unchecked(i..i + 3);
      (v[0], v[1], v[2])
    };

    let input_type = (first >> 22) & 0x7;
    let lexer_type = (first >> 26) & 0x3;
    let table_length = (third >> 16) & 0xFFFF;
    let table_meta = third & 0xFFFF;

    Self {
      input_type,
      lexer_type,
      table_length,
      table_meta,
      scan_index: INSTRUCTION::from(bc, scanner_address as usize),
    }
  }
}
