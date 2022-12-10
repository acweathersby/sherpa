use super::{
  parse_ctx_indices::*,
  token_indices::{TokLength, TokOffset, TokType},
  FunctionPack,
  FAIL_STATE_FLAG_LLVM,
};
use crate::{
  compile::BytecodeOutput,
  llvm::{
    inkwell_branch_ir::construct_instruction_branch,
    LLVMParserModule,
    LLVMTypes,
    PublicFunctions,
    NORMAL_STATE_FLAG_LLVM,
  },
  types::*,
};
use inkwell::{
  attributes::{Attribute, AttributeLoc},
  builder::Builder,
  context::Context,
  module::Linkage,
  values::{CallableValue, FunctionValue, IntValue, PointerValue},
};
use std::collections::{BTreeSet, VecDeque};

pub(crate) fn construct_context<'a>(module_name: &str, ctx: &'a Context) -> LLVMParserModule<'a> {
  use inkwell::AddressSpace::*;
  let module = ctx.create_module(module_name);
  let builder = ctx.create_builder();

  let i8 = ctx.i8_type();
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
    &[i8.ptr_type(Generic).into(), i32.into(), i32.into(), ctx.bool_type().into()],
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
      i32.into(),
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
        INPUT_BLOCK.fn_type(
          &[CTX.ptr_type(Generic).into(), TOKEN.ptr_type(Generic).into(), i32.into()],
          false,
        ),
        None,
      ),
      scan: module.add_function(
        "scan",
        TOKEN.fn_type(
          &[
            CTX.ptr_type(Generic).into(),
            GOTO_FN.ptr_type(Generic).into(),
            TOKEN.ptr_type(Generic).into(),
          ],
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

  let fn_value = funct.emit_eop;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "SuccessfulParse");
  let failure = ctx.append_basic_block(fn_value, "FailedParse");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let state = b.build_struct_gep(parse_ctx, CTX_state, "").unwrap();
  let state = b.build_load(state, "");

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
) -> std::result::Result<(), ()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.get_adjusted_input_block;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let attempt_extend = ctx.append_basic_block(fn_value, "Attempt_Extend");
  let valid_window = ctx.append_basic_block(fn_value, "Valid_Window");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let offset_token = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let requested_size = fn_value.get_nth_param(2).unwrap().into_int_value();

  b.position_at_end(entry);

  let ctx_input_block = b.build_struct_gep(parse_ctx, CTX_input_block, "").unwrap();

  let block_offset_ptr = b.build_struct_gep(ctx_input_block, 1, "").unwrap();
  let block_offset = b.build_load(block_offset_ptr, "").into_int_value();

  let block_size_ptr = b.build_struct_gep(ctx_input_block, 2, "").unwrap();
  let block_size = b.build_load(block_size_ptr, "").into_int_value();

  let token_offset = b.build_struct_gep(offset_token, TokOffset, "").unwrap();
  let token_offset = b.build_load(token_offset, "").into_int_value();
  let token_offset = b.build_int_truncate(token_offset, i32.into(), "");

  let needed_size = b.build_int_add(token_offset, requested_size, "");
  let needed_size = b.build_int_sub(needed_size, block_offset, "");

  let comparison = b.build_int_compare(inkwell::IntPredicate::UGE, block_size, needed_size, "");

  b.build_conditional_branch(comparison, valid_window, attempt_extend);

  b.position_at_end(attempt_extend);

  b.build_store(block_offset_ptr, token_offset);

  let reader = b.build_struct_gep(parse_ctx, CTX_reader, "").unwrap();
  let reader = b.build_load(reader, "");
  let get_byte_block = b.build_struct_gep(parse_ctx, CTX_get_input_block, "").unwrap();
  let get_byte_block = b.build_load(get_byte_block, "").into_pointer_value();
  let get_byte_block = CallableValue::try_from(get_byte_block).unwrap();

  b.build_call(get_byte_block, &[reader.into(), ctx_input_block.into()], "");

  b.build_unconditional_branch(valid_window);

  b.position_at_end(valid_window);

  let block = b.build_load(ctx_input_block, "").into_struct_value();

  let ptr = b.build_extract_value(block, 0, "").unwrap().into_pointer_value();
  let offset = b.build_extract_value(block, 1, "").unwrap().into_int_value();
  let size = b.build_extract_value(block, 2, "").unwrap().into_int_value();
  let diff = b.build_int_sub(token_offset, offset, "");
  // offset the pointer by the difference between the token_offset and
  // and the block offset
  let adjusted_size = b.build_int_sub(size, diff, "");
  let adjusted_ptr = b.build_gep(ptr, &[diff.into()], "");
  let block = b.build_insert_value(block, adjusted_ptr, 0, "").unwrap();
  let block = b.build_insert_value(block, adjusted_size, 2, "").unwrap();

  b.build_return(Some(&block));

  if funct.get_adjusted_input_block.verify(true) {
    Ok(())
  } else {
    Err(())
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

  /*/ Pseudo code ----------------------------------------

  GOTO {
    top,
    base,
    size, -- number of [slots] taken by the current goto stack
    count, -- number of [slots] remaining
  }


  num_of_ele = ctx.top_goto - cxt.base_goto

  remaining_size = ctx.goto_size - num_of_ele

  if remaining_size < needed_size:

    new_size = (needed_size + num_of_ele) << 4

    new_base_pointer = extend_stack(new_size)

    memcpy(new_base_pointer, ctx.base_goto, num_of_ele)

    if ctx.base_goto != ctx.root_base_goto
      extend_stack(cxt.base_goto, ctx.goto_size)

    cxt.base_goto = new_base_pointer
    cxt.top_goto =  cxt.base_goto + num_of_ele


  -- End Pseudo code -------------------------------------
  */

  let fn_value = funct.extend_stack_if_needed;
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let needed_slot_count = fn_value.get_nth_param(1).unwrap().into_int_value();

  let entry = ctx.append_basic_block(fn_value, "Entry");
  b.position_at_end(entry);

  // Compare the number of needed slots with the current top

  let goto_remaining_ptr =
    b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "remaining").unwrap();
  let goto_remaining = b.build_load(goto_remaining_ptr, "remaining").into_int_value();

  // Compare to the stack size
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
  let new_slot_count = b.build_left_shift(new_slot_count, i32.const_int(3, false), "new_size");

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
  let input_token_ptr = fn_value.get_nth_param(2).unwrap().into_pointer_value();

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

  let input_token = b.build_load(input_token_ptr, "");
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
  let LLVMParserModule { module, builder: b, types, ctx, fun: funct, .. } = ctx;

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

pub(crate) unsafe fn construct_push_state(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
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

pub(crate) unsafe fn construct_pop_state_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()> {
  use inkwell::AddressSpace::*;

  let LLVMParserModule { module, builder: b, types, ctx, fun: funct, .. } = ctx;

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

/// The prime function's purpose is fist create a base goto state that emits a failure
/// to prevent stack underflow when descending to the bottom of the goto stack, and also
/// insert the first GOTO entry that will initiate the parser to start parsing based on
/// an entry production id.
pub(crate) fn construct_prime_function(
  ctx: &LLVMParserModule,
  sp: &Vec<(usize, INSTRUCTION, String)>,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) -> Result<(), ()> {
  let i32 = ctx.ctx.i32_type();
  let b = &ctx.builder;
  let funct = &ctx.fun;
  let fn_value = funct.prime;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let selector = fn_value.get_nth_param(1).unwrap().into_int_value(); // Set the context's goto pointers to point to the goto block;
  let block_entry = ctx.ctx.append_basic_block(fn_value, "Entry");
  b.position_at_end(block_entry);

  let blocks = sp
    .iter()
    .map(|(id, instruction, ..)| {
      (
        *id,
        ctx.ctx.append_basic_block(fn_value, &create_offset_label(instruction.get_address())),
        get_parse_function(*instruction, ctx, referenced).as_global_value().as_pointer_value(),
      )
    })
    .collect::<Vec<_>>();

  // Push the End-Of-Parse goto onto the stack. This will prevent underflow of the stack
  b.build_call(
    funct.push_state,
    &[
      parse_ctx.into(),
      i32.const_int((NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64, false).into(),
      funct.emit_eop.as_global_value().as_pointer_value().into(),
    ],
    "",
  );

  if (!blocks.is_empty()) {
    b.build_switch(
      selector,
      blocks[0].1.into(),
      &blocks.iter().map(|b| (i32.const_int(b.0 as u64, false), b.1)).collect::<Vec<_>>(),
    );

    for (_, block, fn_ptr) in &blocks {
      b.position_at_end(*block);
      b.build_call(
        funct.push_state,
        &[
          parse_ctx.into(),
          i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
          (*fn_ptr).into(),
        ],
        "",
      );

      b.build_return(None);
    }
  } else {
    b.build_return(None);
  }

  if funct.prime.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

pub(crate) fn create_offset_label(offset: usize) -> String {
  format!("off_{:X}", offset)
}

pub(crate) fn construct_parse_functions(
  g: &GrammarStore,
  ctx: &LLVMParserModule,
  output: &BytecodeOutput,
) -> Result<(), ()> {
  let mut seen = BTreeSet::new();
  let mut goto_fn = BTreeSet::new();

  // start points
  let start_points = g
    .get_exported_productions()
    .iter()
    .enumerate()
    .map(|(i, p)| {
      let instruction = INSTRUCTION::from(
        &output.bytecode,
        *(output.state_name_to_offset.get(p.guid_name).unwrap()) as usize,
      );

      let name = create_offset_label(instruction.get_address());
      (i, instruction, name)
    })
    .collect::<Vec<_>>();

  construct_prime_function(ctx, &start_points, &mut Vec::new())?;

  let sp_lu = start_points.iter().map(|(_, instruction, _)| *instruction).collect::<BTreeSet<_>>();

  let mut instructions = output
    .state_data
    .values()
    .filter(|s| !s.is_scanner())
    .map(|s| {
      let instruction = INSTRUCTION::from(
        &output.bytecode,
        *output.state_name_to_offset.get(&s.get_name()).unwrap() as usize,
      );

      let pushed_to_stack = sp_lu.contains(&instruction);

      (instruction, pushed_to_stack)
    })
    .collect::<VecDeque<_>>();

  while let Some((instruction, pushed_to_stack)) = instructions.pop_front() {
    if pushed_to_stack {
      goto_fn.insert(instruction);
    }

    if seen.insert(instruction) {
      let mut references = Vec::new();

      let function = get_parse_function(instruction, ctx, &mut references);
      let block_entry = ctx.ctx.append_basic_block(function, "Entry");
      let mut is_scanner = false;
      ctx.builder.position_at_end(block_entry);

      let parse_cxt = function.get_nth_param(0).unwrap().into_pointer_value();

      if let Some(ir_state_name) =
        output.offset_to_state_name.get(&(instruction.get_address() as u32))
      {
        if let Some(state) = output.state_data.get(ir_state_name) {
          if state.get_goto_depth() > 1 {
            ctx.builder.build_call(
              ctx.fun.extend_stack_if_needed,
              &[
                parse_cxt.into(),
                ctx.ctx.i32_type().const_int((state.get_goto_depth() + 2) as u64, false).into(),
              ],
              "",
            );
          }

          is_scanner = state.is_scanner();
        }
      }

      construct_parse_function_statements(
        instruction,
        g,
        ctx,
        &FunctionPack { fun: &function, output, is_scanner },
        &mut references,
      )?;

      for referenced in references {
        instructions.push_front(referenced);
      }
    }
  }

  Ok(())
}

pub(super) fn construct_parse_function_statements(
  mut instruction: INSTRUCTION,
  g: &GrammarStore,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) -> Result<(INSTRUCTION, String), ()> {
  let FunctionPack { output, is_scanner, .. } = pack;

  let mut return_val = None;

  if !instruction.is_valid() {
    return Ok((instruction, "".to_string()));
  }

  while instruction.is_valid() {
    use InstructionType::*;
    match instruction.to_type() {
      SHIFT => {
        if *is_scanner {
          instruction = construct_scanner_instruction_shift(instruction, ctx, pack);
        } else {
          construct_instruction_shift(instruction, ctx, pack, referenced);
          break;
        }
      }
      GOTO => {
        (instruction, return_val) = construct_instruction_goto(instruction, ctx, pack, referenced);

        match return_val {
          Some(val) => {
            break;
          }
          None => {}
        }
      }
      SET_PROD => {
        instruction = construct_instruction_prod(instruction, ctx, pack);
      }
      REDUCE => {
        construct_instruction_reduce(instruction, ctx, pack, referenced);
        break;
      }
      TOKEN => {
        instruction = construct_instruction_token(instruction, ctx, pack);
      }
      FORK_TO => {
        // TODO
        break;
      }
      EAT_CRUMBS | NOOP13 | SCAN | REPEAT | ASSERT_SHIFT | SET_FAIL_STATE => {
        instruction = instruction.next(&output.bytecode);
      }
      VECTOR_BRANCH | HASH_BRANCH => {
        construct_instruction_branch(instruction, g, ctx, pack, referenced, Default::default())?;
        break;
      }
      FAIL => {
        construct_instruction_fail(ctx, pack);
        break;
      }
      PASS => {
        construct_instruction_pass(ctx, pack, return_val);
        break;
      }
    }
  }

  Ok((instruction, String::default()))
}

fn write_emit_reentrance<'a>(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) {
  let bytecode = &pack.output.bytecode;

  use InstructionType::*;

  let next_instruction = match instruction.to_type() {
    PASS => INSTRUCTION::Pass(),
    GOTO => match instruction.next(bytecode).to_type() {
      PASS => instruction.goto(bytecode),
      _ => instruction,
    },
    _ => instruction,
  };

  if !next_instruction.is_PASS() {
    ctx.builder.build_call(
      ctx.fun.push_state,
      &[
        pack.fun.get_first_param().unwrap().into_pointer_value().into(),
        ctx.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
        get_parse_function(next_instruction, ctx, referenced)
          .as_global_value()
          .as_pointer_value()
          .into(),
      ],
      "",
    );
  }
}

pub(crate) fn get_parse_function<'a>(
  instruction: INSTRUCTION,
  ctx: &'a LLVMParserModule,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) -> FunctionValue<'a> {
  let name = format!("parse_fn_{:X}", instruction.get_address());
  match ctx.module.get_function(&name) {
    Some(function) => function,
    None => {
      referenced.push((instruction, true));
      ctx.module.add_function(&name, ctx.types.goto_fn, None)
    }
  }
}

pub(crate) fn create_skip_code(
  b: &Builder,
  token_ptr: PointerValue,
  i64: inkwell::types::IntType,
  table_block: inkwell::basic_block::BasicBlock,
) {
  let off_ptr = b.build_struct_gep(token_ptr, TokOffset, "").unwrap();
  let len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();
  let off = b.build_load(off_ptr, "offset").into_int_value();
  let len = b.build_load(len_ptr, "length").into_int_value();
  let new_off = b.build_int_add(off, len, "new_offset");
  b.build_store(off_ptr, new_off);
  b.build_store(len_ptr, i64.const_int(0, false));
  b.build_unconditional_branch(table_block);
}

pub(crate) fn construct_scanner_instruction_shift(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) -> INSTRUCTION {
  let b = &ctx.builder;

  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();

  let assert_tok_ptr = b.build_struct_gep(parse_ctx, CTX_tok_assert, "").unwrap();
  let assert_tok = b.build_load(assert_tok_ptr, "").into_struct_value();

  let assert_off = b.build_extract_value(assert_tok, 0, "").unwrap().into_int_value();
  let assert_len = b.build_extract_value(assert_tok, 1, "").unwrap().into_int_value();
  let assert_off = b.build_int_add(assert_len, assert_off, "");
  let assert_tok = b.build_insert_value(assert_tok, assert_off, TokOffset, "").unwrap();
  let assert_tok =
    b.build_insert_value(assert_tok, ctx.ctx.i64_type().const_int(0, false), TokType, "").unwrap();

  b.build_store(assert_tok_ptr, assert_tok);

  instruction.next(&pack.output.bytecode)
}

pub(crate) fn construct_instruction_shift(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) {
  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();

  write_emit_reentrance(instruction.next(&pack.output.bytecode), ctx, pack, referenced);

  let b = &ctx.builder;
  let val = b
    .build_call(
      ctx.fun.emit_shift,
      &[parse_ctx.into(), pack.fun.get_nth_param(1).unwrap().into_pointer_value().into()],
      "",
    )
    .try_as_basic_value()
    .unwrap_left();

  b.build_return(Some(&val));
}

pub(crate) fn construct_instruction_reduce(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) {
  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();
  let symbol_count = instruction.get_value() >> 16 & 0x0FFF;
  let rule_id = instruction.get_value() & 0xFFFF;

  write_emit_reentrance(instruction.next(&pack.output.bytecode), ctx, pack, referenced);

  let b = &ctx.builder;
  let prod = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();
  let prod = b.build_load(prod, "").into_int_value();
  let val = b
    .build_call(
      ctx.fun.emit_reduce,
      &[
        parse_ctx.into(),
        pack.fun.get_nth_param(1).unwrap().into_pointer_value().into(),
        prod.into(),
        ctx.ctx.i32_type().const_int(rule_id as u64, false).into(),
        ctx.ctx.i32_type().const_int(symbol_count as u64, false).into(),
      ],
      "",
    )
    .try_as_basic_value()
    .unwrap_left();

  b.build_return(Some(&val));
}

pub(crate) fn construct_instruction_goto<'a>(
  instruction: INSTRUCTION,
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) -> (INSTRUCTION, Option<IntValue<'a>>) {
  let bytecode = &pack.output.bytecode;

  let goto_instruction = instruction.goto(bytecode);

  let goto_function = get_parse_function(goto_instruction, ctx, referenced);

  let LLVMParserModule { ctx, builder, fun, .. } = ctx;

  if (instruction.get_value() & FAIL_STATE_FLAG) > 0 {
    builder.build_call(
      fun.push_state,
      &[
        pack.fun.get_first_param().unwrap().into_pointer_value().into(),
        ctx.i32_type().const_int(FAIL_STATE_FLAG_LLVM as u64, false).into(),
        goto_function.as_global_value().as_pointer_value().into(),
      ],
      "",
    );
    (instruction.next(bytecode), None)
  } else {
    match instruction.next(bytecode).to_type() {
      InstructionType::PASS => {
        // Call the function directly. This should end up as a tail call.
        let return_val = builder
          .build_call(
            goto_function,
            &[
              pack.fun.get_first_param().unwrap().into_pointer_value().into(),
              pack.fun.get_nth_param(1).unwrap().into_pointer_value().into(),
            ],
            "",
          )
          .try_as_basic_value()
          .unwrap_left()
          .into_int_value();

        builder.build_return(Some(&return_val));

        (instruction.next(bytecode), Some(return_val))
      }
      _ => {
        builder.build_call(
          fun.push_state,
          &[
            pack.fun.get_first_param().unwrap().into_pointer_value().into(),
            ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
            goto_function.as_global_value().as_pointer_value().into(),
          ],
          "",
        );

        (instruction.next(bytecode), None)
      }
    }
  }
}

pub(crate) fn construct_instruction_prod(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) -> INSTRUCTION {
  let production_id = instruction.get_contents();
  let parse_ctx = pack.fun.get_nth_param(0).unwrap().into_pointer_value();
  let b = &ctx.builder;
  let production_ptr = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();
  b.build_store(production_ptr, ctx.ctx.i32_type().const_int(production_id as u64, false));
  instruction.next(&pack.output.bytecode)
}

fn construct_instruction_token(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) -> INSTRUCTION {
  let token_value = instruction.get_contents() & 0x00FF_FFFF;
  let parse_ctx = pack.fun.get_nth_param(0).unwrap().into_pointer_value();
  let b = &ctx.builder;
  let anchor_token = b.build_struct_gep(parse_ctx, CTX_tok_anchor, "").unwrap();
  let anchor_type = b.build_struct_gep(anchor_token, TokType, "").unwrap();

  b.build_store(anchor_type, ctx.ctx.i64_type().const_int(token_value as u64, false));

  instruction.next(&pack.output.bytecode)
}

pub(crate) fn construct_instruction_pass(
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
  return_val: Option<IntValue>,
) {
  let parse_ctx = pack.fun.get_nth_param(0).unwrap().into_pointer_value();
  let b = &ctx.builder;
  let state_ptr = b.build_struct_gep(parse_ctx, CTX_state, "").unwrap();
  b.build_store(state_ptr, ctx.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false));

  if let Some(return_val) = return_val {
    b.build_return(Some(&return_val));
  } else {
    b.build_return(Some(&ctx.ctx.i32_type().const_int(0, false)));
  }
}

pub(crate) fn construct_instruction_fail(ctx: &LLVMParserModule, pack: &FunctionPack) {
  let parse_ctx = pack.fun.get_nth_param(0).unwrap().into_pointer_value();
  let b = &ctx.builder;
  let state_ptr = b.build_struct_gep(parse_ctx, CTX_state, "").unwrap();
  b.build_store(state_ptr, ctx.ctx.i32_type().const_int(FAIL_STATE_FLAG_LLVM as u64, false));
  b.build_return(Some(&ctx.ctx.i32_type().const_int(0, false)));
}

/// Compile a LLVM parser module from Hydrocarbon bytecode.
pub fn compile_from_bytecode<'a>(
  module_name: &str,
  g: &GrammarStore,
  llvm_context: &'a Context,
  output: &BytecodeOutput,
) -> core::result::Result<LLVMParserModule<'a>, ()> {
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
    construct_push_state(ctx)?;
    construct_scan(ctx)?;
    construct_emit_error(ctx)?;
    construct_extend_stack_if_needed(ctx)?;
    construct_merge_utf8_part(ctx)?;
    construct_utf8_lookup(ctx)?;
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

  Ok(parse_context)
}

pub(crate) fn construct_utf8_lookup(ctx: &LLVMParserModule) -> std::result::Result<(), ()> {
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

pub(crate) unsafe fn construct_merge_utf8_part(
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
