use inkwell::{
  builder::Builder,
  context::Context,
  execution_engine::ExecutionEngine,
  module::Module,
  types::{FunctionType, StructType},
  values::FunctionValue,
};

use crate::compile::BytecodeOutput;

pub(crate) struct FunctionPack<'a> {
  pub(crate) fun:        &'a FunctionValue<'a>,
  pub(crate) output:     &'a BytecodeOutput,
  pub(crate) is_scanner: bool,
}

pub const FAIL_STATE_FLAG_LLVM: u32 = 2;
pub const NORMAL_STATE_FLAG_LLVM: u32 = 1;

#[derive(Debug)]
pub struct LLVMTypes<'a> {
  pub reader:      StructType<'a>,
  pub parse_ctx:   StructType<'a>,
  pub token:       StructType<'a>,
  pub goto:        StructType<'a>,
  pub goto_fn:     FunctionType<'a>,
  pub action:      StructType<'a>,
  pub input_block: StructType<'a>,
  pub cp_info:     StructType<'a>,
}

#[derive(Debug)]
pub struct CTXGEPIndices {
  pub goto_base:       u32,
  pub goto_stack:      u32,
  pub goto_stack_len:  u32,
  pub goto_top:        u32,
  pub tok_anchor:      u32,
  pub tok_assert:      u32,
  pub tok_peek:        u32,
  pub input_block:     u32,
  pub reader:          u32,
  pub state:           u32,
  pub production:      u32,
  pub peek_mode:       u32,
  pub get_input_block: u32,
}
#[derive(Debug)]
pub struct PublicFunctions<'a> {
  pub(crate) next: FunctionValue<'a>,
  pub(crate) init: FunctionValue<'a>,
  pub(crate) pop_state: FunctionValue<'a>,
  pub(crate) push_state: FunctionValue<'a>,
  pub(crate) emit_accept: FunctionValue<'a>,
  pub(crate) emit_error: FunctionValue<'a>,
  pub(crate) emit_eoi: FunctionValue<'a>,
  pub(crate) emit_eop: FunctionValue<'a>,
  pub(crate) emit_shift: FunctionValue<'a>,
  pub(crate) emit_reduce: FunctionValue<'a>,
  pub(crate) prime: FunctionValue<'a>,
  pub(crate) scan: FunctionValue<'a>,
  pub(crate) memcpy: FunctionValue<'a>,
  pub(crate) memset: FunctionValue<'a>,
  pub(crate) min: FunctionValue<'a>,
  pub(crate) max: FunctionValue<'a>,
  pub(crate) get_adjusted_input_block: FunctionValue<'a>,
  pub(crate) extend_stack_if_needed: FunctionValue<'a>,
  pub(crate) allocate_stack: FunctionValue<'a>,
  pub(crate) free_stack: FunctionValue<'a>,
  pub(crate) get_utf8_codepoint_info: FunctionValue<'a>,
  pub(crate) merge_utf8_part: FunctionValue<'a>,
  pub(crate) ctlz_i8: FunctionValue<'a>,
  pub(crate) get_token_class_from_codepoint: FunctionValue<'a>,
}

#[derive(Debug)]
pub struct LLVMParserModule<'a> {
  pub(crate) ctx:        &'a Context,
  pub(crate) module:     Module<'a>,
  pub(crate) builder:    Builder<'a>,
  pub(crate) types:      LLVMTypes<'a>,
  pub(crate) fun:        PublicFunctions<'a>,
  pub(crate) exe_engine: Option<ExecutionEngine<'a>>,
}

pub mod token_indices {
  pub const TokLength: u32 = 1;
  pub const TokLine: u32 = 3;
  pub const TokOffset: u32 = 0;
  pub const TokType: u32 = 2;
}

pub mod input_block_indices {
  pub const InputBlockPtr: u32 = 0;
  pub const InputBlockStart: u32 = 1;
  pub const InputBlockEnd: u32 = 2;
  pub const InputBlockSize: u32 = 3;
  pub const InputBlockTruncated: u32 = 4;
}

pub mod parse_ctx_indices {
  pub const CTX_goto_stack: u32 = 0; // 0
  pub const CTX_tok_anchor: u32 = 1; // 1
  pub const CTX_tok_assert: u32 = 2; // 2
  pub const CTX_tok_peek: u32 = 3; // 3
  pub const CTX_input_block: u32 = 4; // 4
  pub const CTX_goto_ptr: u32 = 5; // 5
  pub const CTX_goto_stack_len: u32 = 6; // 6
  pub const CTX_goto_stack_remaining: u32 = 7; // 7
  pub const CTX_get_input_block: u32 = 8; // 8
  pub const CTX_reader: u32 = 9; // 9
  pub const CTX_production: u32 = 10; // 10
  pub const CTX_state: u32 = 11; // 11
  pub const CTX_peek_mode: u32 = 12; // 12
  pub const CTX_is_active: u32 = 13; // 13
}
