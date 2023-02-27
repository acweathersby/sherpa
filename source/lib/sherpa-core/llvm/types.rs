use inkwell::{
  builder::Builder,
  context::Context,
  module::Module,
  types::{FunctionType, IntType, StructType},
  values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
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
  pub(crate) dispatch_unwind: FunctionValue<'a>,
  pub(crate) init: FunctionValue<'a>,
  /// Removes the top most goto from the stack. This serves as a function
  /// that can be pushed to the goto stack to act as sentinel that prevents
  /// the calling of the next goto. This can can be used to prevent states
  /// such `catch` from being called during normal parse behavior.
  pub(crate) pop_state: FunctionValue<'a>,
  pub(crate) push_state: FunctionValue<'a>,
  pub(crate) handle_eop: FunctionValue<'a>,
  pub(crate) prime: FunctionValue<'a>,
  /// LLVM [`memcpy`](https://llvm.org/docs/LangRef.html#llvm-memcpy-intrinsic) intrinsic.
  /// Copies data of a certain number of bytes from one memory location to another.
  pub(crate) memcpy: FunctionValue<'a>,
  /// LLVM [`ctlz`](https://llvm.org/docs/LangRef.html#llvm-ctlz-intrinsic) intrinsic.
  /// Counts the number of leading zeros.
  pub(crate) ctlz_i8: FunctionValue<'a>,
  pub(crate) get_adjusted_input_block: FunctionValue<'a>,
  pub(crate) extend_stack: FunctionValue<'a>,
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
}

#[derive(Debug)]
pub struct LLVMParserModule<'a> {
  pub(crate) ctx:    &'a Context,
  pub(crate) module: Module<'a>,
  pub(crate) b:      Builder<'a>,
  pub(crate) types:  LLVMTypes<'a>,
  pub(crate) fun:    PublicFunctions<'a>,
  pub(crate) iptr:   IntType<'a>,
  pub(crate) i64:    IntType<'a>,
  pub(crate) i32:    IntType<'a>,
  pub(crate) i8:     IntType<'a>,
  pub(crate) bool:   IntType<'a>,
}

#[derive(Clone, Copy, Debug)]
pub enum CTX_AGGREGATE_INDICES {
  /// Input data --------------------
  /// ```ignore
  /// pub end_ptr:        *mut u8,
  /// ```
  beg_ptr = 0,
  // Input data ----------
  /// ```ignore
  /// pub token_ptr:       *mut u8,
  /// ```
  anchor_ptr,
  /// ```ignore
  /// pub base_ptr:        *mut u8,
  /// ```
  base_ptr,
  /// ```ignore
  /// pub head_ptr:        *mut u8,
  /// ```
  head_ptr,
  /// ```ignore
  /// pub scan_ptr:        *mut u8,
  /// ```
  scan_ptr,
  /// ```ignore
  /// pub end_ptr:        *mut u8,
  /// ```
  end_ptr,
  /// ```ignore
  /// pub tok_len:       usize,
  /// ```
  tok_len,
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
  _meta_ctx,
  /// ```ignore
  /// pub custom_lex:      fn(&mut T, &mut M, &ParseContext<T, M>) -> (u32, u32, u32),
  /// ```
  _custom_lex,
  // Line info ------------
  /// The offset of the last line character recognized that proceeds the anchor
  /// ```ignore
  /// pub start_line_ptr: usize,
  /// ```
  start_line_off,
  /// The offset of the last line character recognized that proceeds the chkp
  /// ```ignore
  /// pub chkp_line_ptr:  usize,
  /// ```
  chkp_line_off,
  /// The offset of the last line character recognized that proceeds the tail
  /// ```ignore
  /// pub end_line_ptr:   usize,
  /// ```
  end_line_off,
  /// The number of line character recognized that proceed the anchor
  /// ```ignore
  /// pub start_line_num: u32,
  /// ```
  start_line_num,
  /// The number of line character recognized that proceed the chkp
  /// ```ignore
  /// pub chkp_line_num:  u32,
  /// ```
  chkp_line_num,
  /// The number of line character recognized that proceed the tail
  /// ```ignore
  /// pub end_line_num:   u32,
  /// ```
  end_line_num,
  // Parser State ----------
  /// When reducing, stores the the number of of symbols to reduce into one.
  /// ```ignore
  /// pub meta_a:          u32,
  /// ```
  sym_len,
  /// Tracks whether the context is a fail mode or not.
  /// ```ignore
  /// pub state:           u32,
  /// ```
  state,
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
  /// When reducing, stores the rule id that is being reduced.
  /// ```ignore
  /// pub meta_b:          u32,
  /// ```
  rule_id,
  /// ```ignore
  /// pub line_incr:       u8,
  /// ```
  line_incr,
  /// ```ignore
  /// pub is_active:       bool,
  /// ```
  is_active,
  // Miscellaneous
  /// ```ignore
  /// pub in_peek_mode:    bool,
  /// ```
  _in_peek_mode,
}

impl Into<u32> for CTX_AGGREGATE_INDICES {
  fn into(self) -> u32 {
    self as u32
  }
}

impl CTX_AGGREGATE_INDICES {
  pub fn get_ptr<'a>(
    &self,
    b: &Builder<'a>,
    parse_ctx: PointerValue<'a>,
  ) -> SherpaResult<PointerValue<'a>> {
    SherpaResult::Ok(b.build_struct_gep(parse_ctx, (*self).into(), &format!("{:?}_ptr", self))?)
  }

  pub fn load<'a>(
    &self,
    b: &Builder<'a>,
    parse_ctx: PointerValue<'a>,
  ) -> SherpaResult<BasicValueEnum<'a>> {
    let val = b.build_load(self.get_ptr(b, parse_ctx)?, &format!("{:?}_val", self));

    SherpaResult::Ok(val)
  }

  pub fn load_ptr_as_int<'a>(
    &self,
    b: &Builder<'a>,
    parse_ctx: PointerValue<'a>,
    int: IntType<'a>,
  ) -> SherpaResult<IntValue<'a>> {
    let val =
      b.build_load(self.get_ptr(b, parse_ctx)?, &format!("{:?}_val", self)).into_pointer_value();
    SherpaResult::Ok(b.build_ptr_to_int(val, int, &format!("{:?}_val", self)).into())
  }

  pub fn store<'a, V>(
    &self,
    b: &Builder<'a>,
    parse_ctx: PointerValue<'a>,
    value: V,
  ) -> SherpaResult<()>
  where
    V: BasicValue<'a>,
  {
    b.build_store(self.get_ptr(b, parse_ctx)?, value);

    SherpaResult::Ok(())
  }
}
/// Index value for a tail calling convention
pub const fastCC: u32 = 8;
