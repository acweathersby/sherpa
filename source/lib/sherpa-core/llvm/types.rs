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
  pub reader: StructType<'a>,
  pub parse_ctx: StructType<'a>,
  pub token: StructType<'a>,
  pub goto: StructType<'a>,
  pub goto_fn: FunctionType<'a>,
  pub action: StructType<'a>,
  pub input_info: StructType<'a>,
  pub cp_info: StructType<'a>,
  /// The form of all functions that can be tail called.
  pub(crate) TAIL_CALLABLE_PARSE_FUNCTION: FunctionType<'a>,
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
  /// An internal function to free the goto stack if it's size is greater
  /// than 0
  pub(crate) internal_free_stack: FunctionValue<'a>,
  pub(crate) drop: FunctionValue<'a>,
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

pub mod input_block_indices {
  pub const InputBlockPtr: u32 = 0;
  pub const InputBlockStart: u32 = 1;
  pub const InputBlockEnd: u32 = 2;
  pub const InputBlockSize: u32 = 3;
  pub const InputBlockTruncated: u32 = 4;
}

#[derive(Clone, Copy, Debug)]
pub enum CTX_AGGREGATE_INDICES {
  // Input data ----------
  /// ```rust
  /// pub token_ptr:       *mut u8,
  /// ```
  tok_ptr = 0,
  /// ```rust
  /// pub peek_ptr:        *mut u8,
  /// ```
  peek_ptr,
  /// ```rust
  /// pub scan_ptr:        *mut u8,
  /// ```
  scan_ptr,
  /// ```rust
  /// pub tok_input_len:   u32,
  /// ```
  tok_input_len,
  /// ```rust
  /// pub peek_input_len:  u32,
  /// ```
  peek_input_len,
  /// ```rust
  /// pub scan_input_len:  u32,
  /// ```
  scan_input_len,
  /// ```rust
  /// pub tok_input_trun:  bool,
  /// ```
  tok_input_trun,
  /// ```rust
  /// pub peek_input_trun: bool,
  /// ```
  peek_input_trun,
  /// ```rust
  /// pub scan_input_trun: bool,
  /// ```
  scan_input_trun,
  // Miscellaneous
  /// ```rust
  /// pub in_peek_mode:    bool,
  /// ```
  in_peek_mode,
  // Offset info ----------
  /// The start of the portion of characters currently being recognized
  /// ```rust
  /// pub anchor_off:      u32,
  /// ```
  anchor_off,
  /// Maintains the start position of a token. The difference between this and the anchor
  /// offset determines the number characters that have been skipped.
  /// ```rust
  /// pub token_off:       u32,
  /// ```
  token_off,
  /// Represents the most advanced offset of  peeked characters
  /// ```rust
  /// pub peek_off:        u32,
  /// ```
  peek_off,
  /// Maintains the reference to then end of a recognized tokens when in a scan context
  /// ```rust
  /// pub scan_anchor_off: u32,
  /// ```
  scan_anchor_off,
  /// Represents the most advanced portion of scanned characters
  /// ```rust
  /// pub scan_off:        u32,
  /// ```
  scan_off,
  /// Represents the byte length of the currently recognized symbol
  /// ```rust
  /// pub scan_len:        u32,
  /// ```
  scan_len,
  /// Set to the value of a production when a rule is reduced, or
  /// ```rust
  /// pub prod_id:  u32,
  /// ```
  prod_id,
  /// Set to the value of a token when one is recognized.
  /// ```rust
  /// pub tok_id:  u32,
  /// ```
  tok_id,
  // Line info ------------
  /// The offset of the last line character recognized that proceeds the anchor offset
  /// ```rust
  /// pub anchor_line_off: u32,
  /// ```
  anchor_line_off,
  /// The number of line character recognized that proceed the anchor offset
  /// ```rust
  /// pub anchor_line_num: u32,
  /// ```
  anchor_line_num,
  /// The offset of the last line character recognized that proceeds the token offset
  /// ```rust
  /// pub tok_line_off:    u32,
  /// ```
  tok_line_off,
  /// The number of line character recognized that proceed the token offset
  /// ```rust
  /// pub tok_line_num:    u32,
  /// ```
  tok_line_num,
  /// The offset of the last line character recognized that proceeds the peek offset
  /// ```rust
  /// pub peek_line_off:   u32,
  /// ```
  peek_line_off,
  /// The number of line character recognized that proceed the peek offset
  /// ```rust
  /// pub peek_line_num:   u32,
  /// ```
  peek_line_num,
  // Goto stack data -----
  /// ```rust
  /// pub goto_stack_ptr:  *mut Goto,
  /// ```
  goto_stack_ptr,
  /// ```rust
  /// pub goto_size:       u32,
  /// ```
  goto_size,
  /// ```rust
  /// pub goto_free:       u32,
  /// ```
  goto_free,
  // Input data ----------
  /// ```rust
  /// pub get_input_info:  extern "C" fn(&mut T, u32, u32) -> (*const u8, u32, bool),
  /// ```
  get_input_info,
  // Reader --------------
  /// ```rust
  /// pub reader:          *mut T,
  /// ```
  reader,
  // User context --------
  /// ```rust
  /// pub meta_ctx:        *mut M,
  /// ```
  meta_ctx,
  /// ```rust
  /// pub custom_lex:      fn(&mut T, &mut M, &LLVMParseContext<T, M>) -> (u32, u32, u32),
  /// ```
  custom_lex,
  /// Tracks whether the context is a fail mode or not.
  /// ```rust
  /// pub state:           u32,
  /// ```
  state,
  /// ```rust
  /// pub is_active:       bool,
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
    parse_ctx: PointerValue<'a>,
  ) -> SherpaResult<BasicValueEnum<'a>> {
    let val =
      module.builder.build_load(self.get_ptr(module, parse_ctx)?, &format!("{:?}_val", self));

    SherpaResult::Ok(val)
  }

  pub fn store<'a, V>(
    &self,
    module: &'a LLVMParserModule,
    parse_ctx: PointerValue<'a>,
    value: V,
  ) -> SherpaResult<()>
  where
    V: BasicValue<'a>,
  {
    module.builder.build_store(self.get_ptr(module, parse_ctx)?, value);

    SherpaResult::Ok(())
  }
}
/// Index value for a tail calling convention
pub const fastCC: u32 = 8;
