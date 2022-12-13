use inkwell::{
  builder::Builder,
  context::Context,
  execution_engine::ExecutionEngine,
  module::Module,
  types::{FunctionType, StructType},
  values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::SherpaResult;

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
pub struct PublicFunctions<'a> {
  /// Called within a parse loop to get the next parse action.
  pub(crate) next: FunctionValue<'a>,
  pub(crate) dispatch: FunctionValue<'a>,
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
  pub(crate) TAIL_CALLABLE_PARSE_FUNCTION: FunctionType<'a>,
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

pub mod input_block_indices {
  pub const InputBlockPtr: u32 = 0;
  pub const InputBlockStart: u32 = 1;
  pub const InputBlockEnd: u32 = 2;
  pub const InputBlockSize: u32 = 3;
  pub const InputBlockTruncated: u32 = 4;
}

#[derive(Clone, Copy, Debug)]
pub enum CTX_AGGREGATE_INDICES {
  /// ```rust
  /// pub goto_stack: [Goto; LLVM_BASE_STACK_SIZE]
  /// ```
  goto_stack = 0,
  /// ```rust
  /// pub input_block: InputBlock
  /// ```
  input_block,
  /// ```rust
  /// pub goto_stack_ptr: *const Goto
  /// ```
  goto_stack_ptr,
  /// ```rust
  /// pub anchor_offset: u64
  /// ```
  anchor_offset,
  /// ```rust
  /// pub token_offset: u64
  /// ```
  token_offset,
  /// ```rust
  /// pub peek_offset: u64
  /// ```
  peek_offset,
  /// ```rust
  /// pub token_length: u64
  /// ```
  token_length,
  /// ```rust
  /// pub token_type: u64
  /// ```
  token_type,
  /// ```rust
  /// pub line_data: u64
  /// ```
  line_data,
  /// ```rust
  /// pub goto_stack_size: u32
  /// ```
  goto_stack_size,
  /// ```rust
  /// pub goto_stack_remaining: u32
  /// ```
  goto_remaining,
  /// ```rust
  /// pub get_byte_block_at_cursor: fn(&mut T, &mut InputBlock)
  /// ```
  get_input_block,
  /// ```rust
  /// pub reader: *mut T
  /// ```
  reader,
  /// ```rust
  /// pub production: u32
  /// ```
  production,
  /// ```rust
  /// pub state: u32
  /// ```
  state,
  /// ```rust
  /// pub in_peek_mode: bool
  /// ```
  in_peek_mode,
  /// ```rust
  /// pub is_active: bool
  /// ```
  is_active,
}

impl Into<u32> for CTX_AGGREGATE_INDICES {
  fn into(self) -> u32 {
    self as u32
  }
}

impl CTX_AGGREGATE_INDICES {
  pub fn get_ptr<'a>(
    &self,
    module: &'a LLVMParserModule,
    parse_ctx: PointerValue<'a>,
  ) -> SherpaResult<PointerValue<'a>> {
    SherpaResult::Ok(module.builder.build_struct_gep(
      parse_ctx,
      (*self).into(),
      &format!("{:?}_ptr", self),
    )?)
  }

  pub fn load<'a>(
    &self,
    module: &'a LLVMParserModule,
    parser_context: PointerValue<'a>,
  ) -> SherpaResult<BasicValueEnum<'a>> {
    let val =
      module.builder.build_load(self.get_ptr(module, parser_context)?, &format!("{:?}_val", self));

    SherpaResult::Ok(val)
  }

  pub fn store<'a, V>(
    &self,
    module: &'a LLVMParserModule,
    parser_context: PointerValue<'a>,
    value: V,
  ) -> SherpaResult<()>
  where
    V: BasicValue<'a>,
  {
    module.builder.build_store(self.get_ptr(module, parser_context)?, value);

    SherpaResult::Ok(())
  }
}
/// Index value for a tail calling convention
pub const fastCC: u32 = 8;
