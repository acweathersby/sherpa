use super::{
  fastCC,
  parse_functions::{compile_states, ensure_space_on_goto_stack},
  CTX_AGGREGATE_INDICES as CTX,
  FAIL_STATE_FLAG_LLVM,
};
use crate::{
  llvm::{LLVMParserModule, LLVMTypes, PublicFunctions, NORMAL_STATE_FLAG_LLVM},
  types::*,
  Journal,
};
use inkwell::{
  builder::Builder,
  context::Context,
  module::{Linkage, Module},
  targets::TargetData,
  values::{
    AnyValue,
    BasicMetadataValueEnum,
    CallSiteValue,
    CallableValue,
    FunctionValue,
    PointerValue,
  },
};

pub(crate) fn construct_module<'a>(
  j: &mut Journal,
  ctx: &'a Context,
  target_data: &TargetData,
  module: Module<'a>,
) -> LLVMParserModule<'a> {
  let builder = ctx.create_builder();
  let i8 = ctx.i8_type();
  let i8_ptr = i8.ptr_type(0.into()).into();
  let i8_ptr_ptr = i8.ptr_type(0.into()).ptr_type(0.into()).into();
  let bool = ctx.bool_type();
  let i64 = ctx.i64_type();
  let i32 = ctx.i32_type();
  let ptr_int = ctx.ptr_sized_int_type(target_data, None);
  let CP_INFO = ctx.opaque_struct_type("s.CP_INFO");
  let READER = ctx.opaque_struct_type("s.READER");
  let CTX = ctx.opaque_struct_type("s.CTX");
  let GOTO = ctx.opaque_struct_type("s.Goto");
  let TOKEN = ctx.opaque_struct_type("s.Token");
  let CTX_PTR = CTX.ptr_type(0.into());
  let internal_linkage = if j.config().opt_llvm { Some(Linkage::Private) } else { None };
  let TAIL_CALLABLE_PARSE_FUNCTION = i32.fn_type(&[CTX_PTR.into()], false);
  let GOTO_FN = i32.fn_type(&[CTX_PTR.into()], false);

  GOTO.set_body(
    &[TAIL_CALLABLE_PARSE_FUNCTION.ptr_type(0.into()).into(), i32.into(), i32.into()],
    false,
  );

  CP_INFO.set_body(&[i32.into(), i32.into()], false);

  TOKEN.set_body(&[i64.into(), i64.into(), i64.into(), i64.into()], false);

  let get_input_info_fun = ctx
    .bool_type()
    .fn_type(
      &[
        READER.ptr_type(0.into()).into(),
        i8_ptr_ptr,
        i8_ptr_ptr,
        i8_ptr_ptr,
        i8_ptr_ptr,
        i8_ptr_ptr,
        i8_ptr_ptr,
      ],
      false,
    )
    .ptr_type(0.into());

  // Refer to notes in [CTX_AGGREGATE_INDICES] and
  // [ParserContext] for property details.
  CTX.set_body(
    &[
      // Input data ----------
      i8_ptr,
      i8_ptr,
      i8_ptr,
      i8_ptr,
      i8_ptr,
      i8_ptr,
      ptr_int.into(),
      // Goto stack data -----
      GOTO.ptr_type(0.into()).into(),
      i32.into(),
      i32.into(),
      // Parse objects ----------------
      get_input_info_fun.into(),
      READER.ptr_type(0.into()).into(),
      // User context --------
      bool.ptr_type(0.into()).into(),
      bool.ptr_type(0.into()).into(),
      // Line info ------------
      i32.into(),
      i32.into(),
      i32.into(),
      i32.into(),
      i32.into(),
      i32.into(),
      i32.into(),
      // Parser State ----------
      i32.into(),
      i32.into(),
      i32.into(),
      i32.into(),
      i8.into(),
      bool.into(),
      bool.into(),
      bool.into(),
    ],
    false,
  );
  let fun = PublicFunctions {
    /// Public functions
    init: module.add_function(
      "init",
      ctx.void_type().fn_type(&[CTX_PTR.into(), READER.ptr_type(0.into()).into()], false),
      Some(Linkage::External),
    ),
    drop: module.add_function(
      "drop",
      ctx.void_type().fn_type(&[CTX_PTR.into()], false),
      Some(Linkage::External),
    ),
    prime: module.add_function(
      "prime",
      ctx.void_type().fn_type(&[CTX_PTR.into(), i32.into()], false),
      Some(Linkage::External),
    ),
    next: module.add_function("next", TAIL_CALLABLE_PARSE_FUNCTION, None),
    /// Provided by parser host -------------------------------------------------
    get_token_class_from_codepoint: module.add_function(
      "sherpa_get_token_class_from_codepoint",
      i32.fn_type(&[i32.into()], false),
      Some(Linkage::External),
    ),
    allocate_stack: module.add_function(
      "sherpa_allocate_stack",
      GOTO.ptr_type(0.into()).fn_type(&[i64.into()], false),
      Some(Linkage::External),
    ),
    free_stack: module.add_function(
      "sherpa_free_stack",
      ctx.void_type().fn_type(&[GOTO.ptr_type(0.into()).into(), i64.into()], false),
      Some(Linkage::External),
    ),
    /// ------------------------------------------------------------------------
    // These functions can be tail called, as they all use the same interface
    dispatch: module.add_function("dispatch_normal", TAIL_CALLABLE_PARSE_FUNCTION, internal_linkage),
    dispatch_unwind: module.add_function("dispatch_unwind", TAIL_CALLABLE_PARSE_FUNCTION, internal_linkage),
    handle_eop: module.add_function("emit_eop", TAIL_CALLABLE_PARSE_FUNCTION, internal_linkage),

    get_utf8_codepoint_info: module.add_function(
      "get_utf8_codepoint_info",
      CP_INFO.fn_type(&[i8.ptr_type(0.into()).into()], false),
      internal_linkage,
    ),
    merge_utf8_part: module.add_function(
      "merge_utf8_part",
      i32.fn_type(&[i8.ptr_type(0.into()).into(), i32.into(), i32.into()], false),
      internal_linkage,
    ),
    internal_free_stack: module.add_function(
      "internal_free_stack",
      ctx.void_type().fn_type(&[CTX.ptr_type(0.into()).into()], false),
      internal_linkage,
    ),
    get_adjusted_input_block: module.add_function(
      "get_adjusted_input_block",
      ctx.void_type().fn_type(&[
        CTX.ptr_type(0.into()).into(),
        i64.into(),
      ], false),
      internal_linkage,
    ),
    push_state: module.add_function(
      "push_state",
      ctx.void_type().fn_type(
        &[
          CTX.ptr_type(0.into()).into(),
          i32.into(),
          TAIL_CALLABLE_PARSE_FUNCTION.ptr_type(0.into()).into(),
        ],
        false,
      ),
      internal_linkage,
    ),
    pop_state: module.add_function(
      "pop_state",
      TAIL_CALLABLE_PARSE_FUNCTION,
      internal_linkage,
    ),
    extend_stack: module.add_function(
      "extend_stack",
      i32.fn_type(&[CTX.ptr_type(0.into()).into(), i32.into()], false),
      internal_linkage,
    ),
    /// LLVM intrinsics ------------------------------------------------------------
    memcpy: module.add_function(
      "llvm.memcpy.p0i8.p0i8.i32",
      ctx.void_type().fn_type(
        &[
          i8.ptr_type(0.into()).into(),
          i8.ptr_type(0.into()).into(),
          i32.into(),
          ctx.bool_type().into(),
        ],
        false,
      ),
      None,
    ),
    ctlz_i8: module.add_function(
      "llvm.ctlz.i8",
      i8.fn_type(&[i8.into(), ctx.bool_type().into()], false),
      None,
    ),
  };
  // Set all functions that are not part of the public interface to fastCC.
  fun.dispatch.set_call_conventions(fastCC);
  fun.dispatch_unwind.set_call_conventions(fastCC);
  fun.internal_free_stack.set_call_conventions(fastCC);
  fun.handle_eop.set_call_conventions(fastCC);
  fun.pop_state.set_call_conventions(fastCC);
  fun.push_state.set_call_conventions(fastCC);
  fun.get_utf8_codepoint_info.set_call_conventions(fastCC);
  fun.get_token_class_from_codepoint.set_call_conventions(fastCC);
  fun.get_adjusted_input_block.set_call_conventions(fastCC);
  fun.extend_stack.set_call_conventions(fastCC);
  LLVMParserModule {
    b: builder,
    ctx,
    types: LLVMTypes {
      TAIL_CALLABLE_PARSE_FUNCTION,
      reader: READER,
      token: TOKEN,
      parse_ctx: CTX,
      goto: GOTO,
      goto_fn: GOTO_FN,
      cp_info: CP_INFO,
    },
    fun,
    module,
    i64: ctx.i64_type(),
    i32: ctx.i32_type(),
    i8: ctx.i8_type(),
    bool: ctx.bool_type(),
    iptr: ctx.ptr_sized_int_type(target_data, None),
  }
}
pub(crate) unsafe fn construct_emit_end_of_parse(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun: funct, .. } = module;
  let i32 = ctx.i32_type();
  let bool = ctx.bool_type();
  let fn_value = funct.handle_eop;
  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "SuccessfulParse");
  let failure = ctx.append_basic_block(fn_value, "FailedParse");
  let parse_ctx = fn_value.get_nth_param(0)?.into_pointer_value();
  b.position_at_end(entry);
  // Update the active state to be inactive
  CTX::is_active.store(b, parse_ctx, bool.const_zero())?;
  let comparison = b.build_int_compare(
    inkwell::IntPredicate::NE,
    CTX::state.load(b, parse_ctx)?.into_int_value(),
    i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false).into(),
    "",
  );
  b.build_conditional_branch(comparison, success, failure);
  b.position_at_end(success);
  b.build_return(Some(&module.ctx.i32_type().const_int(ParseActionType::Accept.into(), false)));
  b.position_at_end(failure);
  b.build_return(Some(&module.ctx.i32_type().const_int(ParseActionType::Error.into(), false)));
  if funct.handle_eop.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("\n\nCould not build emit_eop function"))
  }
}
pub(crate) unsafe fn construct_get_adjusted_input_block_function(
  m: &LLVMParserModule,
) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun: funct, .. } = m;
  let i8 = ctx.i8_type();
  let fn_value = funct.get_adjusted_input_block;

  // Set the context's goto pointers to point to the goto block;
  let entry_block = ctx.append_basic_block(fn_value, "entry");

  b.position_at_end(entry_block);
  let p_ctx = fn_value.get_nth_param(0)?.into_pointer_value();
  let needed = fn_value.get_nth_param(1)?.into_int_value();
  CTX::end_ptr.store(b, p_ctx, b.build_int_to_ptr(needed, i8.ptr_type(0.into()), ""));

  let beg_ptr = CTX::beg_ptr.get_ptr(b, p_ctx)?;
  let anchor_ptr = CTX::anchor_ptr.get_ptr(b, p_ctx)?;
  let base_ptr = CTX::base_ptr.get_ptr(b, p_ctx)?;
  let head_ptr = CTX::head_ptr.get_ptr(b, p_ctx)?;
  let scan_ptr = CTX::scan_ptr.get_ptr(b, p_ctx)?;
  let end_ptr = CTX::end_ptr.get_ptr(b, p_ctx)?;

  let input_complete = b
    .build_call(
      CallableValue::try_from(CTX::get_input_info.load(b, p_ctx)?.into_pointer_value())?,
      &[
        CTX::reader.load(b, p_ctx)?.into(),
        beg_ptr.into(),
        anchor_ptr.into(),
        base_ptr.into(),
        head_ptr.into(),
        scan_ptr.into(),
        end_ptr.into(),
      ],
      "",
    )
    .try_as_basic_value()
    .left()?
    .into_int_value();

  CTX::block_is_eoi.store(b, p_ctx, input_complete);

  b.build_return(None);

  validate(fn_value)
}
pub(crate) unsafe fn construct_drop(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun: funct, .. } = module;
  let fn_value = funct.drop;
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  b.position_at_end(ctx.append_basic_block(fn_value, "Entry"));
  build_fast_call(b, funct.internal_free_stack, &[parse_ctx.into()]);
  b.build_return(None);
  if funct.drop.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("\n\nCould not validate drop"))
  }
}
pub(crate) unsafe fn construct_internal_free_stack(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, types, fun: funct, .. } = module;
  let i32 = ctx.i32_type();
  let fn_value = funct.internal_free_stack;
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  b.position_at_end(ctx.append_basic_block(fn_value, "Entry"));
  let empty_stack = ctx.append_basic_block(fn_value, "empty_stack");
  let free_stack = ctx.append_basic_block(fn_value, "free_stack");
  let goto_slot_count = CTX::goto_size.load(b, parse_ctx)?.into_int_value();
  let c = b.build_int_compare(inkwell::IntPredicate::NE, goto_slot_count, i32.const_zero(), "");
  b.build_conditional_branch(c, free_stack, empty_stack);
  b.position_at_end(free_stack);
  let goto_byte_size =
    b.build_left_shift(goto_slot_count, ctx.i32_type().const_int(4, false), "goto_byte_size");
  let goto_total_bytes_64 = b.build_int_cast(goto_byte_size, ctx.i64_type(), "");
  let goto_free = CTX::goto_free.load(b, parse_ctx)?.into_int_value();
  let goto_free_bytes =
    b.build_left_shift(goto_free, ctx.i32_type().const_int(4, false), "remaining_bytes");
  let goto_used_bytes = b.build_int_sub(goto_byte_size, goto_free_bytes, "goto_used_bytes");
  let goto_used_bytes_64 = b.build_int_cast(goto_used_bytes, ctx.i64_type(), "");
  let goto_top_ptr = CTX::goto_stack_ptr.load(b, parse_ctx)?.into_pointer_value();
  let goto_base_ptr_int = b.build_int_sub(
    b.build_ptr_to_int(goto_top_ptr, ctx.i64_type().into(), ""),
    goto_used_bytes_64,
    "goto_base",
  );
  let goto_base_ptr =
    b.build_int_to_ptr(goto_base_ptr_int, types.goto.ptr_type(0.into()), "goto_base");
  b.build_call(funct.free_stack, &[goto_base_ptr.into(), goto_total_bytes_64.into()], "");
  CTX::goto_size.store(b, parse_ctx, i32.const_zero());
  b.build_unconditional_branch(empty_stack);
  b.position_at_end(empty_stack);
  b.build_return(None);

  validate(fn_value)
}
pub(crate) unsafe fn construct_extend_stack(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, types, ctx, fun: funct, .. } = module;
  let i32 = ctx.i32_type();

  let fn_value = funct.extend_stack;
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let needed_slot_count = fn_value.get_nth_param(1).unwrap().into_int_value();

  b.position_at_end(ctx.append_basic_block(fn_value, "Entry"));

  // Compare the number of needed slots with the number of available slots
  let goto_free = CTX::goto_free.load(b, parse_ctx)?.into_int_value();

  // Create a new stack, copy data from old stack to new one
  // and, if the old stack was not the original stack,
  // delete the old stack.
  let goto_slot_count = CTX::goto_size.load(b, parse_ctx)?.into_int_value();
  let goto_byte_size =
    b.build_left_shift(goto_slot_count, ctx.i32_type().const_int(4, false), "goto_byte_size");

  let goto_free_bytes =
    b.build_left_shift(goto_free, ctx.i32_type().const_int(4, false), "remaining_bytes");

  let goto_used_bytes = b.build_int_sub(goto_byte_size, goto_free_bytes, "goto_used_bytes");
  let goto_used_bytes_64 = b.build_int_cast(goto_used_bytes, ctx.i64_type(), "");
  let goto_top_ptr = CTX::goto_stack_ptr.load(b, parse_ctx)?.into_pointer_value();
  let goto_base_ptr_int = b.build_int_sub(
    b.build_ptr_to_int(goto_top_ptr, ctx.i64_type().into(), ""),
    goto_used_bytes_64,
    "goto_base",
  );
  let goto_base_ptr =
    b.build_int_to_ptr(goto_base_ptr_int, types.goto.ptr_type(0.into()), "goto_base");

  // create a size that is equal to the needed amount rounded up to the nearest 64bytes
  let new_slot_count = b.build_int_add(goto_slot_count, needed_slot_count, "new_size");
  let new_slot_count = b.build_left_shift(new_slot_count, i32.const_int(2, false), "new_size");
  let new_slot_byte_size =
    b.build_left_shift(new_slot_count, ctx.i32_type().const_int(4, false), "total_bytes");

  let new_slot_byte_size_64 = b.build_int_cast(new_slot_byte_size, ctx.i64_type(), "");

  let new_ptr = b
    .build_call(funct.allocate_stack, &[new_slot_byte_size_64.into()], "")
    .try_as_basic_value()
    .unwrap_left()
    .into_pointer_value();

  // Copy the old stack to the new stack.

  b.build_call(
    funct.memcpy,
    &[
      b.build_bitcast(new_ptr, ctx.i8_type().ptr_type(0.into()), "").into(),
      b.build_bitcast(goto_base_ptr, ctx.i8_type().ptr_type(0.into()), "").into(),
      goto_used_bytes.into(),
      ctx.bool_type().const_int(0, false).into(),
    ],
    "",
  );
  // Free the old stack
  build_fast_call(b, funct.internal_free_stack, &[parse_ctx.into()]);

  // Update parse context values for the goto stack.

  let new_stack_top_ptr = b.build_ptr_to_int(new_ptr, ctx.i64_type(), "new_top");
  let new_stack_top_ptr = b.build_int_add(new_stack_top_ptr, goto_used_bytes_64, "new_top");
  let new_stack_top_ptr =
    b.build_int_to_ptr(new_stack_top_ptr, types.goto.ptr_type(0.into()), "new_top");
  CTX::goto_stack_ptr.store(b, parse_ctx, new_stack_top_ptr)?;
  CTX::goto_size.store(b, parse_ctx, new_slot_count)?;

  let slot_diff = b.build_int_sub(new_slot_count, goto_slot_count, "slot_diff");
  let new_remaining_count = b.build_int_add(slot_diff, goto_free, "remaining");
  CTX::goto_free.store(b, parse_ctx, new_remaining_count);

  b.build_return(Some(&i32.const_int(1, false)));

  validate(fn_value)
}
pub(crate) unsafe fn construct_init(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun: funct, .. } = module;
  let i32 = ctx.i32_type();
  let fn_value = funct.init;
  let parse_ctx = fn_value.get_first_param().unwrap().into_pointer_value();
  let reader_ptr = fn_value.get_last_param().unwrap().into_pointer_value();
  b.position_at_end(ctx.append_basic_block(fn_value, "Entry"));
  CTX::reader.store(b, parse_ctx, reader_ptr)?;
  CTX::goto_size.store(b, parse_ctx, i32.const_int(0, false))?;
  CTX::goto_free.store(b, parse_ctx, i32.const_int(0, false))?;
  CTX::state.store(b, parse_ctx, i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false))?;
  b.build_return(None);

  validate(fn_value)
}

pub(crate) unsafe fn construct_pop_function(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun: funct, .. } = module;
  let fn_value = funct.pop_state;
  // Set the context's goto pointers to point to the goto block;
  let entry_block = ctx.append_basic_block(fn_value, "Entry");
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();

  b.position_at_end(entry_block);

  add_goto_pop_instructions(module, parse_ctx)?;

  build_tail_call_with_return(b, fn_value, funct.dispatch)?;

  validate(fn_value)
}

pub(crate) fn add_goto_pop_instructions<'a>(
  module: &'a LLVMParserModule,
  parse_ctx: PointerValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, .. } = module;

  let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(b, parse_ctx)?;
  let goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();
  let goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "") };
  b.build_store(goto_stack_ptr, goto_top);

  let goto_free_ptr = CTX::goto_free.get_ptr(b, parse_ctx)?;
  let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
  let goto_free = b.build_int_add(goto_free, i32.const_int(1, false), "");
  b.build_store(goto_free_ptr, goto_free);

  SherpaResult::Ok(())
}

pub(crate) fn construct_push_state_function(module: &LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, types, ctx, fun: funct, .. } = module;
  let i32 = ctx.i32_type();
  let fn_value = funct.push_state;
  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let goto_state = fn_value.get_nth_param(1).unwrap().into_int_value();
  let goto_pointer = fn_value.get_nth_param(2).unwrap().into_pointer_value();

  b.position_at_end(entry);

  ensure_space_on_goto_stack(1, module, parse_ctx, fn_value);

  let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(b, parse_ctx)?;
  let goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();

  // Create new goto struct
  let new_goto = b.build_insert_value(types.goto.get_undef(), goto_state, 1, "").unwrap();
  let new_goto = b.build_insert_value(new_goto, goto_pointer, 0, "").unwrap();

  // Store in the current slot
  b.build_store(goto_top, new_goto);

  // Increment the slot
  let goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false)], "") };

  // Store the top slot pointer
  b.build_store(goto_stack_ptr, goto_top);

  let goto_free_ptr = CTX::goto_free.get_ptr(b, parse_ctx)?;
  let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
  let goto_free = b.build_int_sub(goto_free, i32.const_int(1, false), "");

  b.build_store(goto_free_ptr, goto_free);

  b.build_return(None);

  validate(fn_value)
}
pub(crate) fn construct_utf8_lookup_function(module: &LLVMParserModule) -> SherpaResult<()> {
  let i32 = module.ctx.i32_type();
  let i8 = module.ctx.i8_type();
  let zero = i32.const_int(0, false);
  let bool = module.ctx.bool_type();
  let b = &module.b;
  let funct = &module.fun;
  let fn_value = funct.get_utf8_codepoint_info;
  let input_ptr = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let block_entry = module.ctx.append_basic_block(fn_value, "Entry");
  let block_return_ascii = module.ctx.append_basic_block(fn_value, "return_ascii");
  let block_build_code_point = module.ctx.append_basic_block(fn_value, "build_code_point");
  let block_4bytes = module.ctx.append_basic_block(fn_value, "_4bytes");
  let block_3bytes = module.ctx.append_basic_block(fn_value, "_3bytes");
  let block_2bytes = module.ctx.append_basic_block(fn_value, "_2bytes");
  let block_invalid = module.ctx.append_basic_block(fn_value, "invalid");

  b.position_at_end(block_entry);
  let codepoint_info =
    b.build_insert_value(module.types.cp_info.get_undef(), zero, 0, "cp_info").unwrap();
  let codepoint_info_base = b.build_insert_value(codepoint_info, zero, 1, "cp_info").unwrap();
  // Determine number of leading bits set
  let byte = b.build_load(input_ptr, "header_byte").into_int_value();
  let invert = b.build_xor(byte, i8.const_int(255, false), "inverted_header_byte");
  let bit_count = b
    .build_call(
      funct.ctlz_i8,
      &[invert.into(), bool.const_int(0, false).into()],
      "header_bit_count",
    )
    .try_as_basic_value()
    .unwrap_left()
    .into_int_value();
  let comparison =
    b.build_int_compare(inkwell::IntPredicate::EQ, bit_count, i8.const_int(0, false), "");
  b.build_conditional_branch(comparison, block_return_ascii, block_build_code_point);

  // --- Build ASCII Block
  b.position_at_end(block_return_ascii);

  // Insert the codepoint into the CP_INFO struct
  let codepoint_info =
    b.build_insert_value(codepoint_info_base, b.build_int_z_extend(byte, i32, ""), 0, "").unwrap();
  // Insert the codepoint byte length into the CP_INFO struct
  let codepoint_info =
    b.build_insert_value(codepoint_info, i32.const_int(1, false), 1, "").unwrap();
  b.build_return(Some(&codepoint_info));
  // --- Build CodePoint Block
  b.position_at_end(block_build_code_point);
  let off_ptr = b.build_alloca(i32, "offset_ptr");
  let val_ptr = b.build_alloca(i32, "val_ptr");
  let mask = b.build_right_shift(i8.const_int(255, false), bit_count, false, "mask");
  let base_val = b.build_and(byte, mask, "base_val");
  let val = b.build_int_z_extend(base_val, i32, "val_A");
  b.build_store(val_ptr, val);
  b.build_store(off_ptr, i32.const_int(1, false));
  b.build_switch(bit_count, block_invalid, &[
    (i8.const_int(2, false), block_2bytes),
    (i8.const_int(3, false), block_3bytes),
    (i8.const_int(4, false), block_4bytes),
  ]);
  // --- Build 4byte CP  block
  b.position_at_end(block_4bytes);
  let off = b.build_load(off_ptr, "off").into_int_value();
  let val = b.build_load(val_ptr, "val").into_int_value();
  let val = b.build_left_shift(val, i32.const_int(6, false), "val");
  let val = b
    .build_call(funct.merge_utf8_part, &[input_ptr.into(), val.into(), off.into()], "val")
    .try_as_basic_value()
    .unwrap_left()
    .into_int_value();
  let off = b.build_int_add(off, i32.const_int(1, false), "off");
  b.build_store(val_ptr, val);
  b.build_store(off_ptr, off);
  b.build_unconditional_branch(block_3bytes);
  // --- Build 3byte CP  block
  b.position_at_end(block_3bytes);
  let off = b.build_load(off_ptr, "off").into_int_value();
  let val = b.build_load(val_ptr, "val").into_int_value();
  let val = b.build_left_shift(val, i32.const_int(6, false), "val");
  let val = b
    .build_call(funct.merge_utf8_part, &[input_ptr.into(), val.into(), off.into()], "val")
    .try_as_basic_value()
    .unwrap_left()
    .into_int_value();
  let off = b.build_int_add(off, i32.const_int(1, false), "off");
  b.build_store(val_ptr, val);
  b.build_store(off_ptr, off);
  b.build_unconditional_branch(block_2bytes);
  // --- Build 2byte CP  block
  b.position_at_end(block_2bytes);
  let off = b.build_load(off_ptr, "off").into_int_value();
  let val = b.build_load(val_ptr, "val").into_int_value();
  let val = b.build_left_shift(val, i32.const_int(6, false), "val");
  let val = b
    .build_call(funct.merge_utf8_part, &[input_ptr.into(), val.into(), off.into()], "val")
    .try_as_basic_value()
    .unwrap_left()
    .into_int_value();
  let byte_length = b.build_int_z_extend(bit_count, i32, "");
  let codepoint_info = b.build_insert_value(codepoint_info_base, val, 0, "").unwrap();
  let codepoint_info = b.build_insert_value(codepoint_info, byte_length, 1, "").unwrap();
  b.build_return(Some(&codepoint_info));
  // --- Build Invalid Block
  b.position_at_end(block_invalid);
  b.build_return(Some(&codepoint_info_base));

  validate(fn_value)
}
/// Constructs PublicFunctions::merge_utf8_part
pub(crate) unsafe fn construct_merge_utf8_part_function(
  module: &LLVMParserModule,
) -> SherpaResult<()> {
  let i32 = module.ctx.i32_type();
  let b = &module.b;
  let funct = &module.fun;
  let fn_value = funct.merge_utf8_part;
  let input_ptr = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let val = fn_value.get_nth_param(1).unwrap().into_int_value();
  let off = fn_value.get_nth_param(2).unwrap().into_int_value();
  let block_entry = module.ctx.append_basic_block(fn_value, "Entry");

  b.position_at_end(block_entry);
  let byte_ptr = b.build_gep(input_ptr, &[off], "byte_ptr");
  let byte = b.build_load(byte_ptr, "byte").into_int_value();
  let dword = b.build_int_z_extend(byte, i32, "dword");
  let dword = b.build_and(dword, i32.const_int(63, false), "dword");
  let cp = b.build_or(val, dword, "codepoint");

  b.build_return(Some(&cp));

  validate(fn_value)
}
pub(crate) unsafe fn construct_next_function<'a>(module: &'a LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun, .. } = module;
  let fn_value = fun.next;
  b.position_at_end(ctx.append_basic_block(fn_value, "Entry"));
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let call_site = b.build_call(fun.dispatch, &[parse_ctx.into()], "");
  call_site.set_tail_call(false);
  call_site.set_call_convention(fastCC);
  b.build_return(Some(&call_site.try_as_basic_value().left()?.into_int_value()));

  validate(fn_value)
}
pub(crate) unsafe fn construct_dispatch_functions<'a>(m: &'a LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, fun, .. } = m;
  let i32 = ctx.i32_type();
  // Normal Dispatch
  {
    let fn_value = fun.dispatch;
    let entry_block = ctx.append_basic_block(fn_value, "entry");
    let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
    // Insert next instruction near top

    // Set the context's goto pointers to point to the goto block;

    b.position_at_end(entry_block);

    let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(b, parse_ctx)?;
    let goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();
    let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "");
    b.build_store(goto_stack_ptr, goto_top);

    let goto_free_ptr = CTX::goto_free.get_ptr(b, parse_ctx)?;
    let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
    let goto_free = b.build_int_add(goto_free, i32.const_int(1, false), "");
    b.build_store(goto_free_ptr, goto_free);

    let goto = b.build_load(goto_top, "").into_struct_value();

    let parse_function =
      CallableValue::try_from(b.build_extract_value(goto, 0, "").unwrap().into_pointer_value())
        .unwrap();

    build_tail_call_with_return(&m.b, fn_value, parse_function);

    validate(fn_value)?;
  }

  // Unwind Dispatch
  {
    // Insert next instruction near top
    let fn_value = fun.dispatch_unwind;
    let entry_block = ctx.append_basic_block(fn_value, "entry");
    let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();

    let i32 = ctx.i32_type();
    let zero = i32.const_int(0, false);

    // Set the context's goto pointers to point to the goto block;
    let block_dispatch = ctx.append_basic_block(fn_value, "dispatch");
    let block_is_fail_state = ctx.append_basic_block(fn_value, "is_fail_state");

    b.position_at_end(entry_block);
    b.build_unconditional_branch(block_dispatch);
    b.position_at_end(block_dispatch);

    let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(b, parse_ctx)?;
    let goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();
    let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "");
    b.build_store(goto_stack_ptr, goto_top);

    let goto_free_ptr = CTX::goto_free.get_ptr(b, parse_ctx)?;
    let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
    let goto_free = b.build_int_add(goto_free, i32.const_int(1, false), "");
    b.build_store(goto_free_ptr, goto_free);

    let goto = b.build_load(goto_top, "").into_struct_value();
    let goto_state = b.build_extract_value(goto, 1, "")?;

    let masked_state = b.build_and(
      i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false),
      goto_state.into_int_value(),
      "",
    );

    let condition = b.build_int_compare(inkwell::IntPredicate::NE, masked_state, zero, "");
    b.build_conditional_branch(condition, block_is_fail_state, block_dispatch);
    b.position_at_end(block_is_fail_state);
    let parse_function =
      CallableValue::try_from(b.build_extract_value(goto, 0, "").unwrap().into_pointer_value())
        .unwrap();
    build_tail_call_with_return(&m.b, fn_value, parse_function);

    validate(fn_value)
  }
}

pub fn build_fast_call<'a, T>(
  builder: &Builder<'a>,
  callee_fun: T,
  args: &[BasicMetadataValueEnum<'a>],
) -> SherpaResult<CallSiteValue<'a>>
where
  T: Into<CallableValue<'a>>,
{
  let call_site = builder.build_call(callee_fun, args, "FAST_CALL_SITE");
  call_site.set_call_convention(fastCC);
  SherpaResult::Ok(call_site)
}

pub fn build_tail_call_with_return<'a, T>(
  builder: &Builder<'a>,
  caller_fun: FunctionValue<'a>,
  callee_fun: T,
) -> SherpaResult<()>
where
  T: Into<CallableValue<'a>>,
{
  let call_site = builder.build_call(
    callee_fun,
    &[caller_fun.get_nth_param(0)?.into_pointer_value().into()],
    "TAIL_CALL_SITE",
  );
  call_site.set_tail_call(true);
  call_site.set_call_convention(fastCC);

  let value = call_site.try_as_basic_value().left()?.into_int_value();

  builder.build_return(Some(&value));

  SherpaResult::Ok(())
}
/// Compile a LLVM parser module from Sherpa bytecode.
pub fn compile_llvm_module_from_parse_states<'a>(
  j: &mut Journal,
  module: &LLVMParserModule<'a>,
  states: &Vec<(String, Box<ParseState>)>,
) -> SherpaResult<()> {
  unsafe {
    construct_init(module)?;
    construct_emit_end_of_parse(module)?;
    construct_dispatch_functions(module)?;
    construct_pop_function(module)?;
    construct_get_adjusted_input_block_function(module)?;
    construct_push_state_function(module)?;
    construct_extend_stack(module)?;
    construct_merge_utf8_part_function(module)?;
    construct_utf8_lookup_function(module)?;
    construct_next_function(module)?;
    construct_internal_free_stack(module)?;
    construct_drop(module)?;
    compile_states(j, module, states)?;
  }
  module.fun.push_state.add_attribute(
    inkwell::attributes::AttributeLoc::Function,
    module.ctx.create_string_attribute("alwaysinline", ""),
  );
  SherpaResult::Ok(())
}
pub fn validate(func: FunctionValue) -> SherpaResult<()> {
  if func.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from(format!(
      "\n\nCould not validate {} \n{}",
      func.get_name().to_str().unwrap(),
      func.print_to_string().to_str().unwrap()
    )))
  }
}
