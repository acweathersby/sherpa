use super::{bytecode::Instruction, ParserInput};

#[allow(unused)]
pub enum DebugEventNew<'ctx> {
  ExecuteState { base_instruction: Instruction<'ctx> },
  ExecuteInstruction { instruction: Instruction<'ctx>, cursor: usize, is_scanner: bool },
  Complete { nonterminal_id: u32 },
  ActionShift { offset_start: usize, offset_end: usize, token_id: u32 },
  ActionReduce { rule_id: u32 },
  ActionAccept,
  ActionError,
  EndOfFile,
}

pub type DebugFnNew = dyn FnMut(&DebugEventNew, &dyn ParserInput);

pub fn emit_state_debug(debug: &mut Option<&mut DebugFnNew>, bc: &[u8], address: usize, input: &dyn ParserInput) {
  if let Some(debug) = debug {
    debug(&DebugEventNew::ExecuteState { base_instruction: (bc, address as usize).into() }, input);
  }
}

pub fn emit_instruction_debug(
  debug: &mut Option<&mut DebugFnNew>,
  i: Instruction<'_>,
  input: &dyn ParserInput,
  cursor: usize,
  is_scanner: bool,
) {
  if let Some(debug) = debug {
    debug(&DebugEventNew::ExecuteInstruction { instruction: i, cursor, is_scanner }, input);
  }
}
