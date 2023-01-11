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
  pub(crate) handle_eop: FunctionValue<'a>,
  pub(crate) prime: FunctionValue<'a>,
  pub(crate) scan: FunctionValue<'a>,
  /// LLVM [`memcpy`](https://llvm.org/docs/LangRef.html#llvm-memcpy-intrinsic) intrinsic.
  /// Copies data of a certain number of bytes from one memory location to another.
  pub(crate) memcpy: FunctionValue<'a>,
  /// LLVM [`ctlz`](https://llvm.org/docs/LangRef.html#llvm-ctlz-intrinsic) intrinsic.
  /// Counts the number of leading zeros.
  pub(crate) ctlz_i8: FunctionValue<'a>,
  pub(crate) get_adjusted_input_block: FunctionValue<'a>,
  pub(crate) extend_stack_if_needed: FunctionValue<'a>,
  pub(crate) allocate_stack: FunctionValue<'a>,
  pub(crate) free_stack: FunctionValue<'a>,
  /// An internal function to free the goto stack if it's size is greater
  /// than 0
  pub(crate) internal_free_stack: FunctionValue<'a>,
  pub(crate) drop: FunctionValue<'a>,
  /// Helper function to join bytes of a UTF-8 encoded codepoint
  pub(crate) merge_utf8_part: FunctionValue<'a>,
  /// Given a pointer to character data, returns the UTF codepoint based on UTF-8 encoding.
  ///
  /// ### Function signature:
  /// ```ignore
  ///
  /// #[repr(C)]
  /// struct CodePointInfo {
  ///   codepoint: u32,
  ///   byte_length: u32
  /// }
  ///
  /// extern "C" fn get_utf8_codepoint_info( character_data: const * u8 ) -> CodePointInfo;
  /// ```
  pub(crate) get_utf8_codepoint_info: FunctionValue<'a>,
  /// Used to lookup token classes.
  ///
  /// ### Function signature
  /// ```ignore
  /// extern "C" fn get_token_class_from_codepoint( utf8_codepoint: u32 ) -> u32;
  /// ```
  pub(crate) get_token_class_from_codepoint: FunctionValue<'a>,
  pub(crate) simd_token_parse: FunctionValue<'a>,
}

#[derive(Debug)]
pub struct LLVMParserModule<'a> {
  pub(crate) ctx:         &'a Context,
  pub(crate) module:      Module<'a>,
  pub(crate) builder:     Builder<'a>,
  pub(crate) types:       LLVMTypes<'a>,
  pub(crate) fun:         PublicFunctions<'a>,
  pub(crate) _exe_engine: Option<ExecutionEngine<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub enum CTX_AGGREGATE_INDICES {
  // Input data ----------
  /// ```ignore
  /// pub token_ptr:       *mut u8,
  /// ```
  tok_ptr = 0,
  /// ```ignore
  /// pub peek_ptr:        *mut u8,
  /// ```
  peek_ptr,
  /// ```ignore
  /// pub scan_ptr:        *mut u8,
  /// ```
  scan_ptr,
  /// ```ignore
  /// pub tok_input_len:   u32,
  /// ```
  tok_input_len,
  /// ```ignore
  /// pub peek_input_len:  u32,
  /// ```
  peek_input_len,
  /// ```ignore
  /// pub scan_input_len:  u32,
  /// ```
  scan_input_len,
  /// ```ignore
  /// pub tok_input_trun:  bool,
  /// ```
  tok_input_trun,
  /// ```ignore
  /// pub peek_input_trun: bool,
  /// ```
  peek_input_trun,
  /// ```ignore
  /// pub scan_input_trun: bool,
  /// ```
  scan_input_trun,
  // Miscellaneous
  /// ```ignore
  /// pub in_peek_mode:    bool,
  /// ```
  in_peek_mode,
  // Offset info ----------
  /// The start of the portion of characters currently being recognized
  /// ```ignore
  /// pub anchor_off:      u32,
  /// ```
  anchor_off,
  /// Maintains the start position of a token. The difference between this and the anchor
  /// offset determines the number characters that have been skipped.
  /// ```ignore
  /// pub token_off:       u32,
  /// ```
  token_off,
  /// Represents the most advanced offset of  peeked characters
  /// ```ignore
  /// pub peek_off:        u32,
  /// ```
  peek_off,
  /// Maintains the reference to then end of a recognized tokens when in a scan context
  /// ```ignore
  /// pub scan_anchor_off: u32,
  /// ```
  scan_anchor_off,
  /// Represents the most advanced portion of scanned characters
  /// ```ignore
  /// pub scan_off:        u32,
  /// ```
  scan_off,
  /// Represents the byte length of the currently recognized symbol
  /// ```ignore
  /// pub scan_len:        u32,
  /// ```
  scan_len,
  /// Set to the value of a production when a rule is reduced, or
  /// ```ignore
  /// pub prod_id:  u32,
  /// ```
  prod_id,
  /// Set to the value of a token when one is recognized.
  /// ```ignore
  /// pub tok_id:  u32,
  /// ```
  tok_id,
  // Line info ------------
  /// The offset of the last line character recognized that proceeds the anchor offset
  /// ```ignore
  /// pub anchor_line_off: u32,
  /// ```
  anchor_line_off,
  /// The number of line character recognized that proceed the anchor offset
  /// ```ignore
  /// pub anchor_line_num: u32,
  /// ```
  anchor_line_num,
  /// The offset of the last line character recognized that proceeds the token offset
  /// ```ignore
  /// pub tok_line_off:    u32,
  /// ```
  tok_line_off,
  /// The number of line character recognized that proceed the token offset
  /// ```ignore
  /// pub tok_line_num:    u32,
  /// ```
  tok_line_num,
  /// The offset of the last line character recognized that proceeds the peek offset
  /// ```ignore
  /// pub peek_line_off:   u32,
  /// ```
  peek_line_off,
  /// The number of line character recognized that proceed the peek offset
  /// ```ignore
  /// pub peek_line_num:   u32,
  /// ```
  peek_line_num,
  // Goto stack data -----
  /// ```ignore
  /// pub goto_stack_ptr:  *mut Goto,
  /// ```
  goto_stack_ptr,
  /// ```ignore
  /// pub goto_size:       u32,
  /// ```
  goto_size,
  /// ```ignore
  /// pub goto_free:       u32,
  /// ```
  goto_free,
  // Input data ----------
  /// ```ignore
  /// pub get_input_info:  extern "C" fn(&mut T, u32, u32) -> (*const u8, u32, bool),
  /// ```
  get_input_info,
  // Reader --------------
  /// ```ignore
  /// pub reader:          *mut T,
  /// ```
  reader,
  // User context --------
  /// ```ignore
  /// pub meta_ctx:        *mut M,
  /// ```
  meta_ctx,
  /// ```ignore
  /// pub custom_lex:      fn(&mut T, &mut M, &ParseContext<T, M>) -> (u32, u32, u32),
  /// ```
  custom_lex,
  /// Tracks whether the context is a fail mode or not.
  /// ```ignore
  /// pub state:           u32,
  /// ```
  state,
  /// When reducing, stores the the number of of symbols to reduce into one.
  /// ```ignore
  /// pub meta_a:          u32,
  /// ```
  meta_a,
  /// When reducing, stores the rule id that is being reduced.
  /// ```ignore
  /// pub meta_b:          u32,
  /// ```
  meta_b,
  /// ```ignore
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
