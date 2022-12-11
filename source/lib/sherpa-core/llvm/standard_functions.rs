use super::{
  input_block_indices::{
    InputBlockEnd,
    InputBlockPtr,
    InputBlockSize,
    InputBlockStart,
    InputBlockTruncated,
  },
  parse_ctx_indices::*,
  parser_functions::{construct_parse_functions, get_parse_function},
  token_indices::{TokLength, TokOffset, TokType},
  FAIL_STATE_FLAG_LLVM,
};
use crate::{
  compile::BytecodeOutput,
  llvm::{LLVMParserModule, LLVMTypes, PublicFunctions, NORMAL_STATE_FLAG_LLVM},
  types::*,
};
use inkwell::{attributes::AttributeLoc, context::Context, module::Linkage, values::CallableValue};

pub(crate) fn construct_context<'a>(module_name: &str, ctx: &'a Context) -> LLVMParserModule<'a> {
  use inkwell::AddressSpace::*;
  let module = ctx.create_module(module_name);
  let builder = ctx.create_builder();

  let i8 = ctx.i8_type();
  let bool = ctx.bool_type();
  let i64 = ctx.i64_type();
  let i32 = ctx.i32_type();
  let CP_INFO = ctx.opaque_struct_type("s.CP_INFO");
  let READER = ctx.opaque_struct_type("s.READER");
  let ACTION = ctx.opaque_struct_type("s.ACTION");
  let CTX = ctx.opaque_struct_type("s.CTX");
  let GOTO = ctx.opaque_struct_type("s.Goto");
  let TOKEN = ctx.opaque_struct_type("s.Token");
  let INPUT_BLOCK = ctx.opaque_struct_type("s.InputBlock");
  let GOTO_FN =
    i32.fn_type(&[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into()], false);

  ACTION.set_body(&[i32.into(), i32.into()], false);

  GOTO.set_body(&[GOTO_FN.ptr_type(Generic).into(), i32.into(), i32.into()], false);

  CP_INFO.set_body(&[i32.into(), i32.into()], false);

  TOKEN.set_body(&[i64.into(), i64.into(), i64.into(), i64.into()], false);

  INPUT_BLOCK.set_body(
    &[
      // Input pointer
      i8.ptr_type(Generic).into(),
      // start index
      i32.into(),
      // end index
      i32.into(),
      // readable bytes
      i32.into(),
      // is truncated
      ctx.bool_type().into(),
    ],
    false,
  );
  let get_input_block_type = ctx
    .void_type()
    .fn_type(&[READER.ptr_type(Generic).into(), INPUT_BLOCK.ptr_type(Generic).into()], false)
    .ptr_type(Generic);

  CTX.set_body(
    &[
      GOTO.array_type(LLVM_BASE_STACK_SIZE as u32).into(),
      TOKEN.into(),
      TOKEN.into(),
      TOKEN.into(),
      INPUT_BLOCK.into(),
      GOTO.ptr_type(Generic).into(),
      i32.into(),
      i32.into(),
      get_input_block_type.into(),
      READER.ptr_type(Generic).into(),
      i32.into(),
      i32.into(),
      bool.into(),
      bool.into(),
    ],
    false,
  );

  let emit_function_type =
    i32.fn_type(&[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into()], false);

  LLVMParserModule {
    builder,
    ctx,
    types: LLVMTypes {
      reader:      READER,
      action:      ACTION,
      token:       TOKEN,
      parse_ctx:   CTX,
      goto:        GOTO,
      goto_fn:     GOTO_FN,
      input_block: INPUT_BLOCK,
      cp_info:     CP_INFO,
    },
    fun: PublicFunctions {
      get_token_class_from_codepoint: module.add_function(
        "sherpa_get_token_class_from_codepoint",
        i32.fn_type(&[i32.into()], false),
        None,
      ),
      get_utf8_codepoint_info: module.add_function(
        "get_utf8_codepoint_info",
        CP_INFO.fn_type(&[i8.ptr_type(Generic).into()], false),
        None,
      ),
      merge_utf8_part: module.add_function(
        "merge_utf8_part",
        i32.fn_type(&[i8.ptr_type(Generic).into(), i32.into(), i32.into()], false),
        None,
      ),
      allocate_stack: module.add_function(
        "sherpa_allocate_stack",
        GOTO.ptr_type(Generic).fn_type(&[i32.into()], false),
        Some(Linkage::External),
      ),
      free_stack: module.add_function(
        "sherpa_free_stack",
        ctx.void_type().fn_type(&[GOTO.ptr_type(Generic).into(), i32.into()], false),
        Some(Linkage::External),
      ),
      get_adjusted_input_block: module.add_function(
        "get_adjusted_input_block",
        INPUT_BLOCK.fn_type(&[CTX.ptr_type(Generic).into(), i32.into(), i32.into()], false),
        None,
      ),
      scan: module.add_function(
        "scan",
        TOKEN.fn_type(
          &[CTX.ptr_type(Generic).into(), GOTO_FN.ptr_type(Generic).into(), i64.into()],
          false,
        ),
        None,
      ),
      next: module.add_function(
        "next",
        ctx
          .void_type()
          .fn_type(&[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into()], false),
        None,
      ),
      init: module.add_function(
        "init",
        ctx
          .void_type()
          .fn_type(&[CTX.ptr_type(Generic).into(), READER.ptr_type(Generic).into()], false),
        None,
      ),
      push_state: module.add_function(
        "push_state",
        ctx.void_type().fn_type(
          &[CTX.ptr_type(Generic).into(), i32.into(), GOTO_FN.ptr_type(Generic).into()],
          false,
        ),
        None,
      ),
      pop_state: module.add_function(
        "pop_state",
        GOTO.fn_type(&[CTX.ptr_type(Generic).into()], false),
        None,
      ),
      emit_reduce: module.add_function(
        "emit_reduce",
        i32.fn_type(
          &[
            CTX.ptr_type(Generic).into(),
            ACTION.ptr_type(Generic).into(),
            i32.into(),
            i32.into(),
            i32.into(),
          ],
          false,
        ),
        None,
      ),
      emit_eop: module.add_function("emit_eop", emit_function_type, None),
      emit_shift: module.add_function("emit_shift", emit_function_type, None),
      emit_eoi: module.add_function(
        "emit_eoi",
        i32.fn_type(
          &[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into(), i32.into()],
          false,
        ),
        None,
      ),
      emit_accept: module.add_function("emit_accept", emit_function_type, None),
      emit_error: module.add_function("emit_error", emit_function_type.clone(), None),
      prime: module.add_function(
        "prime",
        ctx.void_type().fn_type(&[CTX.ptr_type(Generic).into(), i32.into()], false),
        None,
      ),
      memcpy: module.add_function(
        "llvm.memcpy.p0i8.p0i8.i32",
        ctx.void_type().fn_type(
          &[
            i8.ptr_type(Generic).into(),
            i8.ptr_type(Generic).into(),
            i32.into(),
            ctx.bool_type().into(),
          ],
          false,
        ),
        None,
      ),
      memset: module.add_function(
        "llvm.memset.p0.i32",
        ctx.void_type().fn_type(
          &[i8.ptr_type(Generic).into(), i8.into(), i32.into(), ctx.bool_type().into()],
          false,
        ),
        None,
      ),
      max: module.add_function(
        "llvm.umax.i32",
        i32.fn_type(&[i32.into(), i32.into()], false),
        None,
      ),
      min: module.add_function(
        "llvm.umin.i32",
        i32.fn_type(&[i32.into(), i32.into()], false),
        None,
      ),
      ctlz_i8: module.add_function(
        "llvm.ctlz.i8",
        i8.fn_type(&[i8.into(), ctx.bool_type().into()], false),
        None,
      ),
      extend_stack_if_needed: module.add_function(
        "extend_stack_if_needed",
        i32.fn_type(&[CTX.ptr_type(Generic).into(), i32.into()], false),
        None,
      ),
    },
    module,
    exe_engine: None,
  }
}

pub(crate) fn construct_emit_end_of_input(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_eoi;

  let eoi_action = ctx.struct_type(&[i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let current_offset = fn_value.get_nth_param(2).unwrap();

  b.position_at_end(entry);

  let eoi = b
    .build_bitcast(basic_action, eoi_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let eoi_struct = b.build_load(eoi, "").into_struct_value();
  let eoi_struct = b.build_insert_value(eoi_struct, i32.const_int(9, false), 0, "").unwrap();
  let eoi_struct = b.build_insert_value(eoi_struct, current_offset, 2, "").unwrap();

  b.build_store(eoi, eoi_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_eoi.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_emit_end_of_parse(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();
  let bool = ctx.bool_type();

  let fn_value = funct.emit_eop;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "SuccessfulParse");
  let failure = ctx.append_basic_block(fn_value, "FailedParse");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  // Update the active state to be inactive
  let active_ptr = b.build_struct_gep(parse_ctx, CTX_is_active, "").unwrap();
  b.build_store(active_ptr, bool.const_zero());

  let state_ptr = b.build_struct_gep(parse_ctx, CTX_state, "").unwrap();
  let state = b.build_load(state_ptr, "");

  let comparison = b.build_int_compare(
    inkwell::IntPredicate::NE,
    state.into_int_value(),
    i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false).into(),
    "",
  );
  b.build_conditional_branch(comparison, success, failure);

  b.position_at_end(success);

  b.build_call(funct.emit_accept, &[parse_ctx.into(), basic_action.into()], "");

  b.build_return(Some(&i32.const_int(1, false)));

  b.position_at_end(failure);

  b.build_call(funct.emit_error, &[parse_ctx.into(), basic_action.into()], "");

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_eop.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_get_adjusted_input_block_function(
  ctx: &LLVMParserModule,
) -> SherpaResult<()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();
  let bool = ctx.bool_type();

  let fn_value = funct.get_adjusted_input_block;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let can_extend_check = ctx.append_basic_block(fn_value, "CanExtendCheck");
  let attempt_extend = ctx.append_basic_block(fn_value, "Attempt_Extend");
  let valid_window = ctx.append_basic_block(fn_value, "Valid_Window");

  let parse_ctx = fn_value.get_nth_param(0)?.into_pointer_value();
  let byte_offset = fn_value.get_nth_param(1)?.into_int_value();
  let needed_size = fn_value.get_nth_param(2)?.into_int_value();

  b.position_at_end(entry);

  let ctx_input_block = b.build_struct_gep(parse_ctx, CTX_input_block, "")?;

  let requested_end_index = b.build_int_add(byte_offset, needed_size, "");

  // if the requested_end_cursor_position is > the blocks end position, and the
  // the block is truncated, then request a new block.

  let end_byte_index_ptr = b.build_struct_gep(ctx_input_block, InputBlockEnd, "end_ptr")?;
  let end_byte_index = b.build_load(end_byte_index_ptr, "end").into_int_value();

  let comparison =
    b.build_int_compare(inkwell::IntPredicate::UGE, end_byte_index, requested_end_index, "");

  b.build_conditional_branch(comparison, valid_window, can_extend_check);

  b.position_at_end(can_extend_check);

  let truncated_ptr = b.build_struct_gep(ctx_input_block, InputBlockTruncated, "truncated_ptr")?;
  let truncated = b.build_load(truncated_ptr, "end").into_int_value();

  let comparison =
    b.build_int_compare(inkwell::IntPredicate::EQ, truncated, bool.const_int(1, false), "");

  b.build_conditional_branch(comparison, attempt_extend, valid_window);

  // If the truncated value is `true`, then we can extend:

  b.position_at_end(attempt_extend);

  let block_start_ptr = b.build_struct_gep(ctx_input_block, InputBlockStart, "")?;

  b.build_store(block_start_ptr, byte_offset);

  let reader = b.build_struct_gep(parse_ctx, CTX_reader, "")?;
  let reader = b.build_load(reader, "");
  let get_byte_block = b.build_struct_gep(parse_ctx, CTX_get_input_block, "")?;
  let get_byte_block = b.build_load(get_byte_block, "").into_pointer_value();
  let get_byte_block = CallableValue::try_from(get_byte_block)?;

  b.build_call(get_byte_block, &[reader.into(), ctx_input_block.into()], "");

  b.build_unconditional_branch(valid_window);

  // otherwise we can only adjust the remaining value:

  b.position_at_end(valid_window);

  let block = b.build_load(ctx_input_block, "").into_struct_value();

  let ptr = b.build_extract_value(block, InputBlockPtr, "")?.into_pointer_value();
  let start = b.build_extract_value(block, InputBlockStart, "")?.into_int_value();
  let end = b.build_extract_value(block, InputBlockEnd, "")?.into_int_value();
  let diff = b.build_int_sub(byte_offset, start, "");
  // offset the pointer by the difference between the token_offset and
  // and the block offset
  let adjusted_size = b.build_int_sub(end, byte_offset, "");
  let adjusted_ptr = b.build_gep(ptr, &[diff.into()], "");
  let block = b.build_insert_value(block, adjusted_ptr, InputBlockPtr, "")?;
  let block = b.build_insert_value(block, adjusted_size, InputBlockSize, "")?;

  b.build_return(Some(&block));

  if funct.get_adjusted_input_block.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not validate get_adjusted_input_block"))
  }
}

pub(crate) fn construct_emit_reduce_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_reduce;

  let eoi_action =
    ctx.struct_type(&[i32.into(), i32.into(), i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let production_id = fn_value.get_nth_param(2).unwrap().into_int_value();
  let rule_id = fn_value.get_nth_param(3).unwrap().into_int_value();
  let symbol_count = fn_value.get_nth_param(4).unwrap().into_int_value();

  b.position_at_end(entry);

  let reduce = b
    .build_bitcast(basic_action, eoi_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let reduce_struct = b.build_load(reduce, "").into_struct_value();
  let reduce_struct = b.build_insert_value(reduce_struct, i32.const_int(6, false), 0, "").unwrap();
  let reduce_struct = b.build_insert_value(reduce_struct, production_id, 2, "").unwrap();
  let reduce_struct = b.build_insert_value(reduce_struct, rule_id, 3, "").unwrap();
  let reduce_struct = b.build_insert_value(reduce_struct, symbol_count, 4, "").unwrap();

  b.build_store(reduce, reduce_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_reduce.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_extend_stack_if_needed(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = ctx;
  let i32 = ctx.i32_type();

  let fn_value = funct.extend_stack_if_needed;
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let needed_slot_count = fn_value.get_nth_param(1).unwrap().into_int_value();

  let entry = ctx.append_basic_block(fn_value, "Entry");
  b.position_at_end(entry);

  // Compare the number of needed slots with the number of available slots
  let goto_remaining_ptr =
    b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "remaining").unwrap();
  let goto_remaining = b.build_load(goto_remaining_ptr, "remaining").into_int_value();

  let comparison =
    b.build_int_compare(inkwell::IntPredicate::ULT, goto_remaining, needed_slot_count, "");

  let extend_block = ctx.append_basic_block(fn_value, "Extend");
  let free_block = ctx.append_basic_block(fn_value, "FreeStack");
  let update_block = ctx.append_basic_block(fn_value, "UpdateStack");
  let return_block = ctx.append_basic_block(fn_value, "Return");

  b.build_conditional_branch(comparison, extend_block, return_block);

  // If the difference is less than the amount requested:
  b.position_at_end(extend_block);
  // Create a new stack, copy data from old stack to new one
  // and, if the old stack was not the original stack,
  // delete the old stack.
  let goto_top_ptr_ptr =
    b.build_struct_gep(parse_ctx, CTX_goto_ptr, "goto_top_stack_pointer").unwrap();
  let goto_top_ptr = b.build_load(goto_top_ptr_ptr, "goto_top_ptr").into_pointer_value();

  let goto_slot_size_ptr =
    b.build_struct_gep(parse_ctx, CTX_goto_stack_len, "goto_size_ptr").unwrap();
  let goto_slot_count = b.build_load(goto_slot_size_ptr, "goto_size").into_int_value();

  let goto_byte_size =
    b.build_left_shift(goto_slot_count, ctx.i32_type().const_int(4, false), "goto_byte_size");
  let goto_remaining_bytes =
    b.build_left_shift(goto_remaining, ctx.i32_type().const_int(4, false), "remaining_bytes");

  let goto_used_bytes = b.build_int_sub(goto_byte_size, goto_remaining_bytes, "goto_used_bytes");
  let goto_used_bytes_64 = b.build_int_cast(goto_used_bytes, ctx.i64_type(), "");

  let goto_base_ptr_int = b.build_int_sub(
    b.build_ptr_to_int(goto_top_ptr, ctx.i64_type().into(), ""),
    goto_used_bytes_64,
    "goto_base",
  );
  let goto_base_ptr = b.build_int_to_ptr(
    goto_base_ptr_int,
    types.goto.ptr_type(inkwell::AddressSpace::Generic),
    "goto_base",
  );

  // create a size that is equal to the needed amount rounded up to the nearest 64bytes
  let new_slot_count = b.build_int_add(goto_slot_count, needed_slot_count, "new_size");
  let new_slot_count = b.build_left_shift(new_slot_count, i32.const_int(2, false), "new_size");

  let new_ptr = b
    .build_call(funct.allocate_stack, &[new_slot_count.into()], "")
    .try_as_basic_value()
    .unwrap_left()
    .into_pointer_value();

  b.build_call(
    funct.memcpy,
    &[
      b.build_bitcast(new_ptr, ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic), "").into(),
      b.build_bitcast(goto_base_ptr, ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic), "")
        .into(),
      goto_used_bytes.into(),
      ctx.bool_type().const_int(0, false).into(),
    ],
    "",
  );

  let comparison = b.build_int_compare(
    inkwell::IntPredicate::NE,
    goto_slot_count,
    i32.const_int(8, false).into(),
    "",
  );

  b.build_conditional_branch(comparison, free_block, update_block);

  b.position_at_end(free_block);

  b.build_call(funct.free_stack, &[goto_base_ptr.into(), goto_slot_count.into()], "");

  b.build_unconditional_branch(update_block);
  b.position_at_end(update_block);

  b.build_store(goto_top_ptr_ptr, new_ptr);

  let new_stack_top_ptr = b.build_ptr_to_int(new_ptr, ctx.i64_type(), "new_top");
  let new_stack_top_ptr = b.build_int_add(new_stack_top_ptr, goto_used_bytes_64, "new_top");
  let new_stack_top_ptr = b.build_int_to_ptr(
    new_stack_top_ptr,
    types.goto.ptr_type(inkwell::AddressSpace::Generic),
    "new_top",
  );

  b.build_store(goto_top_ptr_ptr, new_stack_top_ptr);
  b.build_store(goto_slot_size_ptr, new_slot_count);

  let slot_diff = b.build_int_sub(new_slot_count, goto_slot_count, "slot_diff");
  let new_remaining_count = b.build_int_add(slot_diff, goto_remaining, "remaining");
  b.build_store(goto_remaining_ptr, new_remaining_count);

  b.build_unconditional_branch(return_block);
  b.position_at_end(return_block);
  b.build_return(Some(&i32.const_int(1, false)));

  if funct.extend_stack_if_needed.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_scan(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();
  let i64 = ctx.i64_type();

  let fn_value = funct.scan;

  //## Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "Produce_Scan_Token");
  let failure = ctx.append_basic_block(fn_value, "Produce_Failed_Token");

  //## Extract Params
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let scanner_entry_goto = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let start_offset = fn_value.get_nth_param(2).unwrap().into_int_value();

  //## Entry Block
  b.position_at_end(entry);

  let scan_ctx = b.build_alloca(types.parse_ctx, "");

  // The scan context inherits its goto stack and current input block
  // from the main parser context. These instruction copy data from one
  // context to the other.

  // Character Reader
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_reader, "").unwrap(),
    b.build_load(b.build_struct_gep(parse_ctx, CTX_reader, "").unwrap(), ""),
  );

  // Goto Stack Data
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_goto_ptr, "").unwrap(),
    b.build_load(b.build_struct_gep(parse_ctx, CTX_goto_ptr, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_goto_stack_remaining, "").unwrap(),
    b.build_load(b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_goto_stack_len, "").unwrap(),
    b.build_load(b.build_struct_gep(parse_ctx, CTX_goto_stack_len, "").unwrap(), ""),
  );

  // Input Block data
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_input_block, "").unwrap(),
    b.build_load(b.build_struct_gep(parse_ctx, CTX_input_block, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_get_input_block, "").unwrap(),
    b.build_load(b.build_struct_gep(parse_ctx, CTX_get_input_block, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_state, "").unwrap(),
    i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false),
  );

  // Copy input token to the Assert and Anchor token slots of the scan context.
  let input_token_ptr = b.build_alloca(types.token, "input_token");
  let input_token_offset_ptr =
    b.build_struct_gep(input_token_ptr, TokOffset, "input_token_offset").unwrap();
  b.build_store(input_token_offset_ptr, start_offset);

  let input_token_len_ptr =
    b.build_struct_gep(input_token_ptr, TokLength, "input_token_length").unwrap();
  b.build_store(input_token_len_ptr, i64.const_int(0, false));

  let input_token = b.build_load(input_token_ptr, "input_token");
  let assert_token = b.build_struct_gep(scan_ctx, CTX_tok_assert, "").unwrap();
  let anchor_token = b.build_struct_gep(scan_ctx, CTX_tok_anchor, "").unwrap();

  b.build_store(assert_token, input_token);
  b.build_store(anchor_token, input_token);

  b.build_call(
    funct.push_state,
    &[
      scan_ctx.into(),
      i32.const_int((NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64, false).into(),
      funct.emit_eop.as_global_value().as_pointer_value().into(),
    ],
    "",
  );

  b.build_call(
    funct.push_state,
    &[
      scan_ctx.into(),
      i32.const_int((NORMAL_STATE_FLAG_LLVM) as u64, false).into(),
      scanner_entry_goto.into(),
    ],
    "",
  );

  // Reserve enough space on the stack for an Action enum
  let action = b.build_alloca(types.action.array_type(8), "");

  let action = b.build_bitcast(action, types.action.ptr_type(inkwell::AddressSpace::Generic), "");

  b.build_call(funct.next, &[scan_ctx.into(), action.into()], "");

  // Copy updated data from the scan context back to the main context
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_goto_ptr, "").unwrap(),
    b.build_load(b.build_struct_gep(scan_ctx, CTX_goto_ptr, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "").unwrap(),
    b.build_load(b.build_struct_gep(scan_ctx, CTX_goto_stack_remaining, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_goto_stack_len, "").unwrap(),
    b.build_load(b.build_struct_gep(scan_ctx, CTX_goto_stack_len, "").unwrap(), ""),
  );
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_input_block, "").unwrap(),
    b.build_load(b.build_struct_gep(scan_ctx, CTX_input_block, "").unwrap(), ""),
  );

  // Produce either a failure token or a success token based on
  // outcome of the `next` call.

  let action_type = b.build_struct_gep(action.into_pointer_value(), 0, "").unwrap();

  let action_type = b.build_load(action_type, "");

  let comparison = b.build_int_compare(
    inkwell::IntPredicate::EQ,
    action_type.into_int_value(),
    i32.const_int(7, false),
    "",
  );
  b.build_conditional_branch(comparison, success, failure);

  //## Success Block
  b.position_at_end(success);
  let offset_min = b.build_struct_gep(anchor_token, TokOffset, "").unwrap();
  let offset_min = b.build_load(offset_min, "");
  let offset_max = b.build_struct_gep(assert_token, TokOffset, "").unwrap();
  let offset_max = b.build_load(offset_max, "");

  let offset_diff = b.build_int_sub(offset_max.into_int_value(), offset_min.into_int_value(), "");

  let len = b.build_struct_gep(anchor_token, TokLength, "").unwrap();

  b.build_store(len, offset_diff);

  let token = b.build_load(anchor_token, "");

  b.build_return(Some(&token));

  //## Failure Block
  b.position_at_end(failure);

  let type_ = b.build_struct_gep(anchor_token, TokType, "").unwrap();

  b.build_store(type_, i64.const_int(0, false));

  let token = b.build_load(anchor_token, "");
  b.build_return(Some(&token));

  if funct.scan.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_emit_shift(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_shift;

  let eoi_action = ctx.struct_type(&[i32.into(), types.token.into(), types.token.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  // load the anchor token to be used as the skipped symbols

  let anchor_token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_anchor, "").unwrap();
  let skip_token = b.build_load(anchor_token_ptr, "").into_struct_value();

  // load the anchor token to be used as the skipped symbols

  let assert_token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_assert, "").unwrap();
  let assert_token = b.build_load(assert_token_ptr, "").into_struct_value();

  // The length of the skip token is equal to the tokens offset minus the
  // assert token's offset

  let shift_offset = b.build_extract_value(assert_token, TokOffset, "").unwrap().into_int_value();
  let skip_offset = b.build_extract_value(skip_token, TokOffset, "").unwrap().into_int_value();

  let skip_length = b.build_int_sub(shift_offset, skip_offset, "");

  let skip_token = b.build_insert_value(skip_token, skip_length, TokLength, "").unwrap();

  let shift = b
    .build_bitcast(basic_action, eoi_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let shift_struct = b.build_load(shift, "").into_struct_value();
  let shift_struct = b.build_insert_value(shift_struct, i32.const_int(5, false), 0, "").unwrap();

  let shift_struct = b.build_insert_value(shift_struct, skip_token, 1, "").unwrap();

  let shift_struct = b.build_insert_value(shift_struct, assert_token, 2, "").unwrap();

  b.build_store(shift, shift_struct);

  let assert_length = b.build_extract_value(assert_token, 1, "").unwrap().into_int_value();

  let assert_offset = b.build_int_add(assert_length, shift_offset, "");

  let assert_token = b.build_insert_value(assert_token, assert_offset, 0, "").unwrap();
  let assert_token =
    b.build_insert_value(assert_token, ctx.i64_type().const_int(0, false), TokType, "").unwrap();

  b.build_store(anchor_token_ptr, assert_token);
  b.build_store(assert_token_ptr, assert_token);

  let prod_slot = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();

  b.build_store(prod_slot, i32.const_int((u32::max_value() - 1) as u64, false));

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_shift.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_emit_accept(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_accept;

  let accept_action = ctx.struct_type(&[i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let production = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();
  let production = b.build_load(production, "");
  let accept = b
    .build_bitcast(basic_action, accept_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let accept_struct = b.build_load(accept, "").into_struct_value();
  let accept_struct = b.build_insert_value(accept_struct, i32.const_int(7, false), 0, "").unwrap();
  let accept_struct = b.build_insert_value(accept_struct, production, 2, "").unwrap();

  b.build_store(accept, accept_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_accept.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_emit_error(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_error;

  let error_action = ctx.struct_type(&[i32.into(), types.token.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  // load the anchor token as the error token

  let error_token = b.build_struct_gep(parse_ctx, CTX_tok_anchor, "").unwrap();
  let error_token = b.build_load(error_token, "");

  // load the last production value

  let production = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();
  let production = b.build_load(production, "");

  // build the ParseAction::Error struct

  let error = b
    .build_bitcast(basic_action, error_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let error_struct = b.build_load(error, "").into_struct_value();
  let error_struct = b.build_insert_value(error_struct, i32.const_int(8, false), 0, "").unwrap();
  let error_struct = b.build_insert_value(error_struct, error_token, 1, "").unwrap();
  let error_struct = b.build_insert_value(error_struct, production, 2, "").unwrap();

  b.build_store(error, error_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_error.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_init(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();
  let zero = i32.const_int(0, false);

  let fn_value = funct.init;

  let parse_ctx_ptr = fn_value.get_first_param().unwrap().into_pointer_value();
  let reader_ptr = fn_value.get_last_param().unwrap().into_pointer_value();

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "entry");

  builder.position_at_end(entry);

  let goto_stack = builder.build_struct_gep(parse_ctx_ptr, CTX_goto_stack, "")?;
  let goto_start = builder.build_gep(goto_stack, &[zero, zero], "");
  let goto_top = builder.build_struct_gep(parse_ctx_ptr, CTX_goto_ptr, "")?;
  let goto_remaining = builder.build_struct_gep(parse_ctx_ptr, CTX_goto_stack_remaining, "")?;
  let goto_len = builder.build_struct_gep(parse_ctx_ptr, CTX_goto_stack_len, "")?;
  let reader_ctx_ptr = builder.build_struct_gep(parse_ctx_ptr, CTX_reader, "")?;
  let state = builder.build_struct_gep(parse_ctx_ptr, CTX_state, "")?;

  builder.build_store(reader_ctx_ptr, reader_ptr);
  builder.build_store(goto_top, goto_start);
  builder.build_store(goto_remaining, i32.const_int(8, false));
  builder.build_store(goto_len, i32.const_int(8, false));
  builder.build_store(state, i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false));
  builder.build_return(None);

  if funct.init.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_pop_state_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.pop_state;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let goto_top_ptr = b.build_struct_gep(parse_ctx, CTX_goto_ptr, "")?;
  // let goto_top = b.build_in_bounds_gep(goto_top_ptr, &[zero], "");
  let goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();
  let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "");
  b.build_store(goto_top_ptr, goto_top);

  let goto_remaining_ptr = b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "")?;
  let goto_remaining = b.build_load(goto_remaining_ptr, "").into_int_value();
  let goto_remaining = b.build_int_add(goto_remaining, i32.const_int(1, false), "");
  b.build_store(goto_remaining_ptr, goto_remaining);

  let old_goto = b.build_load(goto_top, "");

  b.build_return(Some(&old_goto));

  if funct.pop_state.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_next_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let LLVMParserModule { module: m, builder: b, types: t, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();
  let zero = i32.const_int(0, false);

  let fn_value = funct.next;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  // Set the context's goto pointers to point to the goto block;
  let block_entry = ctx.append_basic_block(fn_value, "Entry");
  let block_dispatch = ctx.append_basic_block(fn_value, "Dispatch");
  let block_useful_state = ctx.append_basic_block(fn_value, "ModeAppropriateState");
  let block_emit = ctx.append_basic_block(fn_value, "Emit");

  b.position_at_end(block_entry);
  b.build_unconditional_branch(block_dispatch);

  b.position_at_end(block_dispatch);
  let state = b.build_load(b.build_struct_gep(parse_ctx, CTX_state, "state")?, "state");
  let goto = b
    .build_call(funct.pop_state, &[parse_ctx.into()], "")
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();
  let goto_state = b.build_extract_value(goto, 1, "").unwrap();
  let masked_state = b.build_and(state.into_int_value(), goto_state.into_int_value(), "");
  let condition = b.build_int_compare(inkwell::IntPredicate::NE, masked_state, zero, "");
  b.build_conditional_branch(condition, block_useful_state, block_dispatch);

  b.position_at_end(block_useful_state);
  let gt_fn =
    CallableValue::try_from(b.build_extract_value(goto, 0, "").unwrap().into_pointer_value())
      .unwrap();

  let should_emit = b.build_call(gt_fn, &[parse_ctx.into(), action.into()], "");

  // should_emit.set_call_convention(11);

  let should_emit_return = should_emit.try_as_basic_value().unwrap_left().into_int_value();

  let condition = b.build_int_compare(inkwell::IntPredicate::EQ, should_emit_return, zero, "");
  b.build_conditional_branch(condition, block_dispatch, block_emit);

  b.position_at_end(block_emit);
  b.build_return(None);

  if funct.next.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) fn create_offset_label(offset: usize) -> String {
  format!("off_{:X}", offset)
}

pub(crate) unsafe fn construct_push_state_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.push_state;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let goto_state = fn_value.get_nth_param(1).unwrap().into_int_value();
  let goto_fn = fn_value.get_nth_param(2).unwrap().into_pointer_value();

  b.position_at_end(entry);
  let new_goto = b.build_insert_value(types.goto.get_undef(), goto_state, 1, "").unwrap();
  let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "").unwrap();

  let goto_top_ptr = b.build_struct_gep(parse_ctx, CTX_goto_ptr, "")?;
  let goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();
  b.build_store(goto_top, new_goto);

  let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false)], "");
  b.build_store(goto_top_ptr, goto_top);

  let goto_remaining_ptr = b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "")?;
  let goto_remaining = b.build_load(goto_remaining_ptr, "").into_int_value();
  let goto_remaining = b.build_int_sub(goto_remaining, i32.const_int(1, false), "");
  b.build_store(goto_remaining_ptr, goto_remaining);

  b.build_return(None);

  if funct.push_state.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) fn construct_utf8_lookup_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let i32 = ctx.ctx.i32_type();
  let i8 = ctx.ctx.i8_type();
  let zero = i32.const_int(0, false);
  let bool = ctx.ctx.bool_type();
  let b = &ctx.builder;
  let funct = &ctx.fun;
  let fn_value = funct.get_utf8_codepoint_info;

  let input_ptr = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let block_entry = ctx.ctx.append_basic_block(fn_value, "Entry");
  let block_return_ascii = ctx.ctx.append_basic_block(fn_value, "return_ascii");
  let block_build_code_point = ctx.ctx.append_basic_block(fn_value, "build_code_point");
  let block_4bytes = ctx.ctx.append_basic_block(fn_value, "_4bytes");
  let block_3bytes = ctx.ctx.append_basic_block(fn_value, "_3bytes");
  let block_2bytes = ctx.ctx.append_basic_block(fn_value, "_2bytes");
  let block_invalid = ctx.ctx.append_basic_block(fn_value, "invalid");
  b.position_at_end(block_entry);

  let codepoint_info =
    b.build_insert_value(ctx.types.cp_info.get_undef(), zero, 0, "cp_info").unwrap();
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

  if fn_value.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) unsafe fn construct_merge_utf8_part_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  let i32 = ctx.ctx.i32_type();

  let b = &ctx.builder;
  let funct = &ctx.fun;
  let fn_value = funct.merge_utf8_part;

  let input_ptr = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let val = fn_value.get_nth_param(1).unwrap().into_int_value();
  let off = fn_value.get_nth_param(2).unwrap().into_int_value();

  let block_entry = ctx.ctx.append_basic_block(fn_value, "Entry");
  b.position_at_end(block_entry);

  let byte_ptr = b.build_gep(input_ptr, &[off], "byte_ptr");
  let byte = b.build_load(byte_ptr, "byte").into_int_value();

  let dword = b.build_int_z_extend(byte, i32, "dword");
  let dword = b.build_and(dword, i32.const_int(63, false), "dword");

  let cp = b.build_or(val, dword, "codepoint");

  b.build_return(Some(&cp));

  if fn_value.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

/// Compile a LLVM parser module from Hydrocarbon bytecode.
pub fn compile_from_bytecode<'a>(
  module_name: &str,
  g: &GrammarStore,
  llvm_context: &'a Context,
  output: &BytecodeOutput,
) -> SherpaResult<LLVMParserModule<'a>> {
  let mut parse_context = construct_context(module_name, &llvm_context);
  let ctx = &mut parse_context;

  unsafe {
    construct_emit_accept(ctx)?;
    construct_emit_end_of_input(ctx)?;
    construct_emit_end_of_parse(ctx)?;
    construct_emit_reduce_function(ctx)?;
    construct_emit_shift(ctx)?;
    construct_get_adjusted_input_block_function(ctx)?;
    construct_init(ctx)?;
    construct_next_function(ctx)?;
    construct_pop_state_function(ctx)?;
    construct_push_state_function(ctx)?;
    construct_scan(ctx)?;
    construct_emit_error(ctx)?;
    construct_extend_stack_if_needed(ctx)?;
    construct_merge_utf8_part_function(ctx)?;
    construct_utf8_lookup_function(ctx)?;
  }

  // Add optimizations here
  ctx
    .fun
    .pop_state
    .add_attribute(AttributeLoc::Function, ctx.ctx.create_string_attribute("", "alwaysinline"));

  construct_parse_functions(g, ctx, output)?;

  parse_context.fun.push_state.add_attribute(
    inkwell::attributes::AttributeLoc::Function,
    parse_context.ctx.create_string_attribute("alwaysinline", ""),
  );

  SherpaResult::Ok(parse_context)
}
