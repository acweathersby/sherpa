use super::{
  bytecode::{ByteCodeIterator, Instruction, MatchInputType},
  *,
};

/// Deconstructed bytecode table information.
#[derive(Clone, Copy, Debug)]
pub struct TableHeaderData<'a> {
  pub input_type:             MatchInputType,
  pub table_length:           u32,
  pub table_meta:             u32,
  /// The instruction of the scanner state, if this table has
  /// one.
  pub scan_block_instruction: bytecode::Instruction<'a>,
  /// The absolute address of start of the instruction data.
  pub default_block:          bytecode::Instruction<'a>,
  /// The absolute address of start of the table data.
  ///
  /// For both Hash and Vector tables, the table data section is
  /// a [\[u32; table_length\]] buffer.
  pub table_start:            usize,
  /// A ByteCodeIterator positioned at the absolute address of the table data
  /// section.
  pub table_start_iter:       ByteCodeIterator<'a>,
  /// The absolute address of the first instruction block following the
  /// table data.
  pub parse_block_address:    usize,
}

impl<'a> From<Instruction<'a>> for TableHeaderData<'a> {
  #[track_caller]
  fn from(i: Instruction<'a>) -> Self {
    debug_assert!(matches!(i.get_opcode(), bytecode::Opcode::HashBranch | bytecode::Opcode::VectorBranch));

    let mut iter = i.iter();
    let input_type = iter.next_u8().unwrap() as u32;
    let default_delta = iter.next_u32_le().unwrap();
    let scan_address = iter.next_u32_le().unwrap();
    let table_length = iter.next_u32_le().unwrap();
    let table_meta = iter.next_u32_le().unwrap();
    let table_start = i.address() + 18;
    let table_start_iter = (i.bytecode(), table_start as usize).into();

    Self {
      input_type: input_type.into(),
      table_length,
      table_meta,
      table_start,
      table_start_iter,
      parse_block_address: table_start + (table_length * 4) as usize,
      scan_block_instruction: if scan_address > 0 {
        (i.bytecode(), scan_address as usize).into()
      } else {
        (i.bytecode(), 0).into()
      },
      default_block: (i.bytecode(), i.address() + default_delta as usize).into(),
    }
  }
}
