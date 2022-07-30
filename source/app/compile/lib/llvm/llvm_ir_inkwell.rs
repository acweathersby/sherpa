use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::types::StructType;
use inkwell::values::AnyValue;
use inkwell::values::CallSiteValue;
use inkwell::values::CallableValue;
use inkwell::values::FunctionValue;
use std::io::Result;
use std::io::Write;

use crate::writer::code_writer::CodeWriter;
use hctk::types::*;

const FAIL_STATE_FLAG_LLVM: u32 = 2;
const NORMAL_STATE_FLAG_LLVM: u32 = 1;

pub fn _undefined<W: Write>(
  _grammar: &GrammarStore,
  _bytecode: &[u32],
  _writer: &mut CodeWriter<W>,
) -> Result<()>
{
  Ok(())
}

#[derive(Debug)]
pub struct LLVMTypes<'a>
{
  reader:      StructType<'a>,
  parse_ctx:   StructType<'a>,
  token:       StructType<'a>,
  goto:        StructType<'a>,
  goto_fn:     FunctionType<'a>,
  action:      StructType<'a>,
  input_block: StructType<'a>,
}
#[derive(Debug)]
pub struct CTXGEPIndices
{
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
pub struct PublicFunctions<'a>
{
  next: FunctionValue<'a>,
  init: FunctionValue<'a>,
  pop_state: FunctionValue<'a>,
  push_state: FunctionValue<'a>,
  emit_accept: FunctionValue<'a>,
  emit_error: FunctionValue<'a>,
  emit_eoi: FunctionValue<'a>,
  emit_eop: FunctionValue<'a>,
  emit_shift: FunctionValue<'a>,
  emit_reduce: FunctionValue<'a>,
  prime: FunctionValue<'a>,
  scan: FunctionValue<'a>,
  get_adjusted_input_block: FunctionValue<'a>,
}

#[derive(Debug)]
pub struct LLVMParserModule<'a>
{
  ctx:         &'a Context,
  module:      Module<'a>,
  builder:     Builder<'a>,
  types:       LLVMTypes<'a>,
  ctx_indices: CTXGEPIndices,
  pub_funct:   PublicFunctions<'a>,
  exe_engine:  Option<ExecutionEngine<'a>>,
}

pub fn construct_context<'a>(ctx: &'a Context) -> LLVMParserModule<'a>
{
  use inkwell::AddressSpace::*;
  let module = ctx.create_module("parser");
  let builder = ctx.create_builder();

  let packed = false;

  let i8 = ctx.i8_type();
  let i64 = ctx.i64_type();
  let i32 = ctx.i32_type();
  let READER = ctx.opaque_struct_type("s.READER");
  let ACTION = ctx.opaque_struct_type("s.ACTION");
  let CTX = ctx.opaque_struct_type("s.CTX");
  let GOTO = ctx.opaque_struct_type("s.Goto");
  let TOKEN = ctx.opaque_struct_type("s.Token");
  let INPUT_BLOCK = ctx.opaque_struct_type("s.InputBlock");
  let GOTO_FN = i32.fn_type(
    &[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into()],
    false,
  );

  ACTION.set_body(&[i32.into(), i32.into()], false);

  GOTO.set_body(&[GOTO_FN.ptr_type(Generic).into(), i32.into(), i32.into()], false);

  TOKEN.set_body(&[i64.into(), i64.into(), i64.into(), i64.into()], false);

  INPUT_BLOCK.set_body(&[i8.ptr_type(Generic).into(), i32.into(), i32.into()], false);
  let get_input_block_type = ctx
    .void_type()
    .fn_type(
      &[READER.ptr_type(Generic).into(), INPUT_BLOCK.ptr_type(Generic).into()],
      false,
    )
    .ptr_type(Generic);

  CTX.set_body(
    &[
      GOTO.array_type(8).into(),
      TOKEN.into(),
      TOKEN.into(),
      TOKEN.into(),
      INPUT_BLOCK.into(),
      GOTO.ptr_type(Generic).into(),
      GOTO.ptr_type(Generic).into(),
      get_input_block_type.into(),
      READER.ptr_type(Generic).into(),
      i32.into(),
      i32.into(),
      i32.into(),
      i32.into(),
    ],
    false,
  );

  let emit_function_type = i32.fn_type(
    &[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into()],
    false,
  );

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
    },
    ctx_indices: CTXGEPIndices {
      goto_stack:      0,  // 0
      tok_anchor:      1,  // 1
      tok_assert:      2,  // 2
      tok_peek:        3,  // 3
      input_block:     4,  // 4
      goto_base:       5,  // 5
      goto_top:        6,  // 6
      get_input_block: 7,  // 7
      reader:          8,  // 8
      goto_stack_len:  9,  // 9
      production:      10, // 10
      state:           11, // 11
      peek_mode:       12, // 12
    },
    pub_funct: PublicFunctions {
      get_adjusted_input_block: module.add_function(
        "get_adjusted_input_block",
        INPUT_BLOCK.fn_type(
          &[
            CTX.ptr_type(Generic).into(),
            TOKEN.ptr_type(Generic).into(),
            i32.into(),
          ],
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
        ctx.void_type().fn_type(
          &[CTX.ptr_type(Generic).into(), ACTION.ptr_type(Generic).into()],
          false,
        ),
        None,
      ),
      init: module.add_function(
        "init",
        ctx
          .void_type()
          .fn_type(&[CTX.ptr_type(Generic).into()], false),
        None,
      ),
      push_state: module.add_function(
        "push_state",
        ctx.void_type().fn_type(
          &[
            CTX.ptr_type(Generic).into(),
            i32.into(),
            GOTO_FN.ptr_type(Generic).into(),
          ],
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
          &[
            CTX.ptr_type(Generic).into(),
            ACTION.ptr_type(Generic).into(),
            i32.into(),
          ],
          false,
        ),
        None,
      ),
      emit_accept: module.add_function("emit_accept", emit_function_type, None),
      emit_error: module.add_function("emit_error", emit_function_type.clone(), None),
      prime: module.add_function(
        "prime",
        ctx
          .void_type()
          .fn_type(&[CTX.ptr_type(Generic).into(), i32.into()], false),
        None,
      ),
    },
    module,
    exe_engine: None,
  }
}

fn construct_emit_end_of_input(ctx: &LLVMParserModule) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_eoi;

  let eoi_action = ctx.struct_type(&[i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let current_offset = fn_value.get_nth_param(2).unwrap();

  b.position_at_end(entry);

  let eoi = b
    .build_bitcast(basic_action, eoi_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let eoi_struct = b.build_load(eoi, "").into_struct_value();
  let eoi_struct = b
    .build_insert_value(eoi_struct, i32.const_int(9, false), 0, "")
    .unwrap();
  let eoi_struct = b
    .build_insert_value(eoi_struct, current_offset, 2, "")
    .unwrap();

  b.build_store(eoi, eoi_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_eoi.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_emit_end_of_parse(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_eop;

  let eoi_action = ctx.struct_type(&[i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "SuccessfulParse");
  let failure = ctx.append_basic_block(fn_value, "FailedParse");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let state = b.build_struct_gep(parse_ctx, ci.state, "").unwrap();
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

unsafe fn construct_get_adjusted_input_block_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

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

  let ctx_input_block = b.build_struct_gep(parse_ctx, ci.input_block, "").unwrap();

  let block_offset_ptr = b.build_struct_gep(ctx_input_block, 1, "").unwrap();
  let block_offset = b.build_load(block_offset_ptr, "").into_int_value();

  let block_size_ptr = b.build_struct_gep(ctx_input_block, 2, "").unwrap();
  let block_size = b.build_load(block_size_ptr, "").into_int_value();

  let token_offset = b.build_struct_gep(offset_token, 0, "").unwrap();
  let token_offset = b.build_load(token_offset, "").into_int_value();
  let token_offset = b.build_int_truncate(token_offset, i32.into(), "");

  let needed_size = b.build_int_add(token_offset, requested_size, "");
  let needed_size = b.build_int_sub(needed_size, block_offset, "");

  let comparison =
    b.build_int_compare(inkwell::IntPredicate::UGT, block_size, needed_size, "");

  b.build_conditional_branch(comparison, valid_window, attempt_extend);

  b.position_at_end(attempt_extend);

  b.build_store(block_offset_ptr, token_offset);

  let reader = b.build_struct_gep(parse_ctx, ci.reader, "").unwrap();
  let reader = b.build_load(reader, "");
  let get_byte_block = b
    .build_struct_gep(parse_ctx, ci.get_input_block, "")
    .unwrap();
  let get_byte_block = b.build_load(get_byte_block, "").into_pointer_value();
  let get_byte_block = CallableValue::try_from(get_byte_block).unwrap();

  b.build_call(get_byte_block, &[reader.into(), ctx_input_block.into()], "");

  b.build_unconditional_branch(valid_window);

  b.position_at_end(valid_window);

  let block = b.build_load(ctx_input_block, "").into_struct_value();

  //   let ptr = b
  // .build_extract_value(block, 0, "")
  // .unwrap()
  // .into_pointer_value();
  //
  // let offset = b
  // .build_extract_value(block, 1, "")
  // .unwrap()
  // .into_int_value();
  //
  // let size = b
  // .build_extract_value(block, 2, "")
  // .unwrap()
  // .into_int_value();
  //
  // let diff = b.build_int_sub(token_offset, offset, "");
  // offset the pointer by the difference between the token_offset and
  // and the block  offset
  //
  // let adjusted_size = b.build_int_sub(size, diff, "");
  // let adjusted_ptr = b.build_gep(ptr, &[diff.into()], "");
  //
  // let block = b.build_insert_value(block, adjusted_ptr, 0, "").unwrap();
  // let block = b.build_insert_value(block, adjusted_size, 2, "").unwrap();

  b.build_return(Some(&block));

  if funct.get_adjusted_input_block.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

fn construct_emit_reduce_function(ctx: &LLVMParserModule) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_reduce;

  let eoi_action =
    ctx.struct_type(&[i32.into(), i32.into(), i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let production_id = fn_value.get_nth_param(2).unwrap().into_int_value();
  let body_id = fn_value.get_nth_param(3).unwrap().into_int_value();
  let symbol_count = fn_value.get_nth_param(4).unwrap().into_int_value();

  b.position_at_end(entry);

  let reduce = b
    .build_bitcast(basic_action, eoi_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let reduce_struct = b.build_load(reduce, "").into_struct_value();
  let reduce_struct = b
    .build_insert_value(reduce_struct, i32.const_int(6, false), 0, "")
    .unwrap();
  let reduce_struct = b
    .build_insert_value(reduce_struct, production_id, 2, "")
    .unwrap();
  let reduce_struct = b.build_insert_value(reduce_struct, body_id, 3, "").unwrap();
  let reduce_struct = b
    .build_insert_value(reduce_struct, symbol_count, 4, "")
    .unwrap();

  b.build_store(reduce, reduce_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_reduce.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_scan_function(ctx: &LLVMParserModule) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.scan;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "Produce_Scan_Token");
  let failure = ctx.append_basic_block(fn_value, "Produce_Failed_Token");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let scanner_entry_goto = fn_value.get_nth_param(1).unwrap().into_pointer_value();
  let token_basis = fn_value.get_nth_param(2).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let scan_ctx = b.build_alloca(types.parse_ctx, "");

  b.build_call(funct.init, &[scan_ctx.into()], "");

  let parse_ctx_reader = b.build_struct_gep(parse_ctx, ci.reader, "").unwrap();
  let scan_ctx_reader = b.build_struct_gep(scan_ctx, ci.reader, "").unwrap();

  let parse_input_block = b.build_struct_gep(parse_ctx, ci.input_block, "").unwrap();
  let scan_input_block = b.build_struct_gep(scan_ctx, ci.input_block, "").unwrap();

  let parse_get_input_block = b
    .build_struct_gep(parse_ctx, ci.get_input_block, "")
    .unwrap();
  let scan_get_input_block = b
    .build_struct_gep(scan_ctx, ci.get_input_block, "")
    .unwrap();

  b.build_store(scan_get_input_block, b.build_load(parse_get_input_block, ""));
  b.build_store(scan_input_block, b.build_load(parse_input_block, ""));
  b.build_store(scan_ctx_reader, b.build_load(parse_ctx_reader, ""));
  b.build_store(
    b.build_struct_gep(scan_ctx, ci.state, "").unwrap(),
    i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false),
  );

  let root_token = b.build_load(token_basis, "");
  let assert_token = b.build_struct_gep(scan_ctx, ci.tok_assert, "").unwrap();
  let anchor_token = b.build_struct_gep(scan_ctx, ci.tok_anchor, "").unwrap();

  b.build_store(assert_token, root_token);
  b.build_store(anchor_token, root_token);

  b.build_call(
    funct.push_state,
    &[
      scan_ctx.into(),
      i32
        .const_int((NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64, false)
        .into(),
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
  // reserve enough space on the stack for an Action enum

  let action = b.build_alloca(types.action.array_type(8), "");
  let action =
    b.build_bitcast(action, types.action.ptr_type(inkwell::AddressSpace::Generic), "");

  b.build_call(funct.next, &[scan_ctx.into(), action.into()], "");

  // copy the input data from the scan context to the parse context

  b.build_store(parse_input_block, b.build_load(scan_input_block, ""));

  // Produce either a failure token or a success token based on
  // outcome of the `next` call.

  let action_type = b
    .build_struct_gep(action.into_pointer_value(), 0, "")
    .unwrap();

  let action_type = b.build_load(action_type, "");

  let comparison = b.build_int_compare(
    inkwell::IntPredicate::EQ,
    action_type.into_int_value(),
    i32.const_int(7, false),
    "",
  );
  b.build_conditional_branch(comparison, success, failure);

  b.position_at_end(success);
  let offset_min = b.build_struct_gep(assert_token, 0, "").unwrap();
  let offset_min = b.build_load(offset_min, "");
  let offset_max = b.build_struct_gep(anchor_token, 0, "").unwrap();
  let offset_max = b.build_load(offset_max, "");

  let offset_diff =
    b.build_int_sub(offset_max.into_int_value(), offset_min.into_int_value(), "");

  let length = b.build_struct_gep(anchor_token, 1, "").unwrap();

  b.build_store(length, offset_diff);

  let token = b.build_load(anchor_token, "");

  b.build_return(Some(&token));

  b.position_at_end(failure);
  let token = b.build_load(anchor_token, "");
  b.build_return(Some(&token));

  if funct.scan.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_emit_shift_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_shift;

  let eoi_action =
    ctx.struct_type(&[i32.into(), types.token.into(), types.token.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  // load the anchor token to be used as the skipped symbols

  let skip_token = b.build_struct_gep(parse_ctx, ci.tok_anchor, "").unwrap();
  let skip_token = b.build_load(skip_token, "").into_struct_value();

  // load the anchor token to be used as the skipped symbols

  let shift_token = b.build_struct_gep(parse_ctx, ci.tok_assert, "").unwrap();
  let shift_token = b.build_load(shift_token, "").into_struct_value();

  // The length of the skip token is equal to the tokens offset minus the
  // assert token's offset

  let shift_offset = b
    .build_extract_value(shift_token, 0, "")
    .unwrap()
    .into_int_value();
  let skip_offset = b
    .build_extract_value(skip_token, 0, "")
    .unwrap()
    .into_int_value();

  let skip_length = b.build_int_sub(shift_offset, skip_offset, "");

  let skip_token = b
    .build_insert_value(skip_token, skip_length, 1, "")
    .unwrap();

  let shift = b
    .build_bitcast(basic_action, eoi_action.ptr_type(inkwell::AddressSpace::Generic), "")
    .into_pointer_value();

  let shift_struct = b.build_load(shift, "").into_struct_value();
  let shift_struct = b
    .build_insert_value(shift_struct, i32.const_int(5, false), 0, "")
    .unwrap();

  let shift_struct = b
    .build_insert_value(shift_struct, skip_token, 1, "")
    .unwrap();

  let shift_struct = b
    .build_insert_value(shift_struct, shift_token, 2, "")
    .unwrap();

  b.build_store(shift, shift_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_shift.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_emit_accept_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_accept;

  let accept_action = ctx.struct_type(&[i32.into(), i32.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let production = b.build_struct_gep(parse_ctx, ci.production, "").unwrap();
  let production = b.build_load(production, "");
  let accept = b
    .build_bitcast(
      basic_action,
      accept_action.ptr_type(inkwell::AddressSpace::Generic),
      "",
    )
    .into_pointer_value();

  let accept_struct = b.build_load(accept, "").into_struct_value();
  let accept_struct = b
    .build_insert_value(accept_struct, i32.const_int(7, false), 0, "")
    .unwrap();
  let accept_struct = b
    .build_insert_value(accept_struct, production, 2, "")
    .unwrap();

  b.build_store(accept, accept_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_accept.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_emit_error_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.emit_error;

  let error_action =
    ctx.struct_type(&[i32.into(), types.token.into(), i32.into()], false);

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let basic_action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  b.position_at_end(entry);

  // load the anchor token as the error token

  let error_token = b.build_struct_gep(parse_ctx, ci.tok_anchor, "").unwrap();
  let error_token = b.build_load(error_token, "");

  // load the last production value

  let production = b.build_struct_gep(parse_ctx, ci.production, "").unwrap();
  let production = b.build_load(production, "");

  // build the ParseAction::Error struct

  let error = b
    .build_bitcast(
      basic_action,
      error_action.ptr_type(inkwell::AddressSpace::Generic),
      "",
    )
    .into_pointer_value();

  let error_struct = b.build_load(error, "").into_struct_value();
  let error_struct = b
    .build_insert_value(error_struct, i32.const_int(8, false), 0, "")
    .unwrap();
  let error_struct = b
    .build_insert_value(error_struct, error_token, 1, "")
    .unwrap();
  let error_struct = b
    .build_insert_value(error_struct, production, 2, "")
    .unwrap();

  b.build_store(error, error_struct);

  b.build_return(Some(&i32.const_int(1, false)));

  if funct.emit_error.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_init_function(ctx: &LLVMParserModule) -> std::result::Result<(), ()>
{
  use inkwell::AddressSpace::*;

  let LLVMParserModule {
    module,
    builder,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();
  let zero = i32.const_int(0, false);

  let fn_value = funct.init;

  let parse_ctx_ptr = fn_value.get_first_param().unwrap().into_pointer_value();

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "entry");

  builder.position_at_end(entry);

  let goto_stack = builder.build_struct_gep(parse_ctx_ptr, ci.goto_stack, "")?;
  let goto_start = builder.build_gep(goto_stack, &[zero, zero], "");
  let goto_base = builder.build_struct_gep(parse_ctx_ptr, ci.goto_base, "")?;
  let goto_top = builder.build_struct_gep(parse_ctx_ptr, ci.goto_top, "")?;
  let goto_len = builder.build_struct_gep(parse_ctx_ptr, ci.goto_stack_len, "")?;
  let state = builder.build_struct_gep(parse_ctx_ptr, ci.state, "")?;

  builder.build_store(goto_base, goto_start);
  builder.build_store(goto_top, goto_start);
  builder.build_store(goto_len, i32.const_int(8, false));
  builder.build_store(state, i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false));
  builder.build_return(None);

  if funct.init.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_push_state_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.push_state;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let goto_state = fn_value.get_nth_param(1).unwrap().into_int_value();
  let goto_fn = fn_value.get_nth_param(2).unwrap().into_pointer_value();

  b.position_at_end(entry);
  let new_goto = b
    .build_insert_value(types.goto.get_undef(), goto_state, 1, "")
    .unwrap();
  let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "").unwrap();

  let goto_top_ptr = b.build_struct_gep(parse_ctx, ci.goto_top, "")?;
  // let goto_top = b.build_in_bounds_gep(goto_top_ptr, &[zero], "");
  let goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();

  b.build_store(goto_top, new_goto);

  let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false)], "");

  b.build_store(goto_top_ptr, goto_top);

  b.build_return(None);

  if funct.push_state.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_pop_state_function(
  ctx: &LLVMParserModule,
) -> std::result::Result<(), ()>
{
  use inkwell::AddressSpace::*;

  let LLVMParserModule {
    module,
    builder: b,
    types,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();

  let fn_value = funct.pop_state;

  // Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();

  b.position_at_end(entry);

  let goto_top_ptr = b.build_struct_gep(parse_ctx, ci.goto_top, "")?;
  // let goto_top = b.build_in_bounds_gep(goto_top_ptr, &[zero], "");
  let goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();
  let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "");
  b.build_store(goto_top_ptr, goto_top);

  let old_goto = b.build_load(goto_top, "");

  b.build_return(Some(&old_goto));

  if funct.pop_state.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

unsafe fn construct_next_function(ctx: &LLVMParserModule) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module: m,
    builder: b,
    types: t,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

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
  let state = b.build_load(b.build_struct_gep(parse_ctx, ci.state, "state")?, "state");
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
  let gt_fn = CallableValue::try_from(
    b.build_extract_value(goto, 0, "")
      .unwrap()
      .into_pointer_value(),
  )
  .unwrap();

  let should_emit = b.build_call(gt_fn, &[parse_ctx.into(), action.into()], "");

  // should_emit.set_call_convention(11);

  let should_emit_return = should_emit
    .try_as_basic_value()
    .unwrap_left()
    .into_int_value();

  let condition =
    b.build_int_compare(inkwell::IntPredicate::EQ, should_emit_return, zero, "");
  b.build_conditional_branch(condition, block_dispatch, block_emit);

  b.position_at_end(block_emit);
  b.build_return(None);

  if funct.next.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

fn construct_prime_function(ctx: &LLVMParserModule) -> std::result::Result<(), ()>
{
  let LLVMParserModule {
    module: m,
    builder: b,
    types: t,
    ctx,
    ctx_indices: ci,
    pub_funct: funct,
    ..
  } = ctx;

  let i32 = ctx.i32_type();
  let zero = i32.const_int(0, false);

  let fn_value = funct.prime;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let target_production = fn_value.get_nth_param(1).unwrap().into_int_value();

  // Set the context's goto pointers to point to the goto block;
  let block_entry = ctx.append_basic_block(fn_value, "Entry");

  b.position_at_end(block_entry);

  b.build_call(
    funct.push_state,
    &[
      parse_ctx.into(),
      i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
      funct
        .emit_accept
        .as_global_value()
        .as_pointer_value()
        .into(),
    ],
    "",
  );

  b.build_return(None);

  if funct.prime.verify(true) {
    Ok(())
  } else {
    Err(())
  }
}

#[cfg(test)]
mod test
{
  use hctk::types::Goto;
  use hctk::types::InputBlock;
  use hctk::types::LLVMParseContext;
  use hctk::types::ParseAction;
  use hctk::types::ParseToken;
  use hctk::types::TestUTF8StringReader;
  use inkwell::context::Context;
  use inkwell::execution_engine::JitFunction;

  use crate::llvm::llvm_ir_inkwell::NORMAL_STATE_FLAG_LLVM;

  type Init = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>);
  type PushState = unsafe extern "C" fn(
    *mut LLVMParseContext<TestUTF8StringReader<'static>>,
    u32,
    usize,
  );

  type EmitReduce = unsafe extern "C" fn(
    *mut LLVMParseContext<TestUTF8StringReader<'static>>,
    *mut ParseAction,
    u32,
    u32,
    u32,
  ) -> u32;

  type GetInputBlock = unsafe extern "C" fn(
    *mut LLVMParseContext<TestUTF8StringReader<'static>>,
    *const ParseToken,
    u32,
  ) -> InputBlock;

  type EmitAccept = unsafe extern "C" fn(
    *mut LLVMParseContext<TestUTF8StringReader<'static>>,
    *mut ParseAction,
  ) -> u32;

  type EmitShift = EmitAccept;

  type Next = unsafe extern "C" fn(
    *mut LLVMParseContext<TestUTF8StringReader<'static>>,
    *mut ParseAction,
  );

  type Prime =
    unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>, u32);

  type PopState =
    unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>) -> Goto;

  unsafe fn get_parse_function<'a, T: inkwell::execution_engine::UnsafeFunctionPointer>(
    ctx: &'a super::LLVMParserModule,
    function_name: &str,
  ) -> Result<JitFunction<'a, T>, ()>
  {
    let init = ctx
      .exe_engine
      .as_ref()
      .unwrap()
      .get_function::<T>(function_name)
      .ok()
      .ok_or("Failed To Compile")
      .unwrap();

    Ok(init)
  }

  fn setup_exec_engine(ctx: &mut super::LLVMParserModule)
  {
    if ctx.exe_engine.is_none() {
      ctx.exe_engine = Some(
        ctx
          .module
          .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
          .unwrap(),
      );
    }
  }

  #[test]
  fn verify_construction_of_init_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_init_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_initialize_context()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_init_function(&parse_context).is_ok()) };

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = Box::new(LLVMParseContext::new(&mut reader));
      let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();

      init_fn.call(rt_ctx.as_mut());

      let root = rt_ctx.as_ref() as *const LLVMParseContext<TestUTF8StringReader<'static>>
        as usize;

      assert_eq!(rt_ctx.stack_top as usize, root);
      assert_eq!(rt_ctx.stack_base as usize, root);
      assert_eq!(rt_ctx.stack_size as usize, 8);
      assert_eq!(rt_ctx.state, super::NORMAL_STATE_FLAG_LLVM);

      println!("{:?}:{:#?}", root, rt_ctx);
    };
  }

  #[test]
  fn verify_construction_of_push_state_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_push_state_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_push_new_state()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_init_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_push_state_function(&parse_context).is_ok()) }

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = LLVMParseContext::new(&mut reader);
      let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
      let push_state_fn =
        get_parse_function::<PushState>(&parse_context, "push_state").unwrap();

      init_fn.call(&mut rt_ctx);
      push_state_fn.call(&mut rt_ctx, super::NORMAL_STATE_FLAG_LLVM, 0x10101010_01010101);
      push_state_fn.call(&mut rt_ctx, super::NORMAL_STATE_FLAG_LLVM, 0x01010101_10101010);

      println!("{:#?}", rt_ctx);
      assert_eq!(rt_ctx.local_goto_stack[0].goto_fn as usize, 0x10101010_01010101);
      assert_eq!(rt_ctx.local_goto_stack[0].state, super::NORMAL_STATE_FLAG_LLVM);

      assert_eq!(rt_ctx.local_goto_stack[1].goto_fn as usize, 0x01010101_10101010);
      assert_eq!(rt_ctx.local_goto_stack[1].state, super::NORMAL_STATE_FLAG_LLVM);
    };
  }

  #[test]
  fn verify_construction_of_emit_accept_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_accept_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn verify_construction_of_emit_shift_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_shift_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_emit_shift()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_shift_function(&parse_context).is_ok()) }

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = LLVMParseContext::new(&mut reader);
      let emit_shift =
        get_parse_function::<EmitShift>(&parse_context, "emit_shift").unwrap();

      rt_ctx.anchor_token.byte_offset = 5;
      rt_ctx.anchor_token.byte_length = 0;
      rt_ctx.assert_token.byte_offset = 10;

      let mut action = ParseAction::Undefined;

      emit_shift.call(&mut rt_ctx, &mut action);

      match action {
        ParseAction::Shift {
          skipped_characters,
          token,
        } => {
          assert_eq!(skipped_characters.byte_length, 5);
          assert_eq!(skipped_characters.byte_offset, 5);
          assert_eq!(token.byte_offset, 10);
        }
        _ => panic!("Incorrect ParseAction enum type assigned"),
      }
    };
  }

  #[test]
  fn verify_construction_of_emit_reduce_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_reduce_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_emit_reduce()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_reduce_function(&parse_context).is_ok()) }

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = LLVMParseContext::new(&mut reader);
      let emit_reduce =
        get_parse_function::<EmitReduce>(&parse_context, "emit_reduce").unwrap();

      let mut action = ParseAction::Undefined;

      emit_reduce.call(&mut rt_ctx, &mut action, 1, 2, 3);

      match action {
        ParseAction::Reduce {
          production_id,
          body_id,
          symbol_count,
        } => {
          assert_eq!(production_id, 1);
          assert_eq!(body_id, 2);
          assert_eq!(symbol_count, 3);
        }
        _ => panic!("Incorrect ParseAction enum type assigned"),
      }
    };
  }

  #[test]
  fn verify_construction_of_get_adjusted_input_block_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe {
      assert!(super::construct_get_adjusted_input_block_function(&parse_context).is_ok())
    }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_produce_extended_block()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_init_function(&parse_context).is_ok()) }
    unsafe {
      assert!(super::construct_get_adjusted_input_block_function(&parse_context).is_ok())
    }

    // Create a helper function to overcome the struct passing as value between the Jit  code and Rust

    use inkwell::AddressSpace::*;

    let shim = parse_context.module.add_function(
      "shim",
      context.void_type().fn_type(
        &[
          parse_context.types.parse_ctx.ptr_type(Generic).into(),
          parse_context.types.token.ptr_type(Generic).into(),
          context.i32_type().into(),
          parse_context.types.input_block.ptr_type(Generic).into(),
        ],
        false,
      ),
      None,
    );
    parse_context
      .builder
      .position_at_end(context.append_basic_block(shim, "Entry"));
    let input_block = parse_context
      .builder
      .build_call(
        parse_context.pub_funct.get_adjusted_input_block,
        &[
          shim.get_nth_param(0).unwrap().into_pointer_value().into(),
          shim.get_nth_param(1).unwrap().into_pointer_value().into(),
          shim.get_nth_param(2).unwrap().into_int_value().into(),
        ],
        "",
      )
      .try_as_basic_value()
      .unwrap_left()
      .into_struct_value();
    parse_context
      .builder
      .build_store(shim.get_nth_param(3).unwrap().into_pointer_value(), input_block);
    parse_context.builder.build_return(None);

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = LLVMParseContext::new(&mut reader);
      let init = get_parse_function::<Init>(&parse_context, "init").unwrap();

      type GetInputBlockShim = unsafe extern "C" fn(
        *mut LLVMParseContext<TestUTF8StringReader<'static>>,
        *const ParseToken,
        u32,
        *mut InputBlock,
      ) -> InputBlock;

      let get_ib =
        get_parse_function::<GetInputBlockShim>(&parse_context, "shim").unwrap();

      init.call(&mut rt_ctx);

      let mut token = rt_ctx.anchor_token;

      token.byte_offset = 3;

      let mut block = InputBlock::default();

      get_ib.call(&mut rt_ctx, &token, 2, &mut block);

      println!("{:?} {:?}", rt_ctx.input_block, block);
      assert_eq!(*block.block, b't');
      assert_eq!(block.offset, 3);
      assert_eq!(block.length, 1);
    };
  }

  #[test]
  fn verify_construction_of_scan_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_scan_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn verify_construction_of_emit_error_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_error_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn verify_construction_of_emit_eop_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_end_of_parse(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn verify_construction_of_emit_end_of_input_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_emit_end_of_input(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn verify_construction_of_pop_state_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_pop_state_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_pop_new_state()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_init_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_push_state_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_pop_state_function(&parse_context).is_ok()) }

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = LLVMParseContext::new(&mut reader);
      let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
      let push_state_fn =
        get_parse_function::<PushState>(&parse_context, "push_state").unwrap();
      let pop_state =
        get_parse_function::<PopState>(&parse_context, "pop_state").unwrap();

      init_fn.call(&mut rt_ctx);
      push_state_fn.call(&mut rt_ctx, 20, 0x10101010_01010101);
      push_state_fn.call(&mut rt_ctx, 40, 0x10101010_01010101);

      let second = pop_state.call(&mut rt_ctx);
      let first = pop_state.call(&mut rt_ctx);

      assert_eq!(second.state, 40);
      assert_eq!(first.state, 20);

      println!("{:#?}", rt_ctx);
    };
  }
  #[test]
  fn verify_construct_of_prime_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_prime_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn verify_construct_of_next_function()
  {
    let context = Context::create();

    let parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_next_function(&parse_context).is_ok()) }

    println!("{}", parse_context.module.to_string());
  }

  #[test]
  fn should_call_next_and_emit_accept()
  {
    let context = Context::create();

    let mut parse_context = super::construct_context(&context);

    unsafe { assert!(super::construct_init_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_push_state_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_pop_state_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_next_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_emit_accept_function(&parse_context).is_ok()) }
    unsafe { assert!(super::construct_prime_function(&parse_context).is_ok()) }

    unsafe {
      setup_exec_engine(&mut parse_context);
      let mut reader = TestUTF8StringReader::new("test");
      let mut rt_ctx = LLVMParseContext::new(&mut reader);
      let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
      let push_state_fn =
        get_parse_function::<PushState>(&parse_context, "push_state").unwrap();
      let next = get_parse_function::<Next>(&parse_context, "next").unwrap();
      let emit_accept =
        get_parse_function::<EmitAccept>(&parse_context, "emit_accept").unwrap();
      let prime = get_parse_function::<Prime>(&parse_context, "prime").unwrap();

      init_fn.call(&mut rt_ctx);

      prime.call(&mut rt_ctx, 0);

      rt_ctx.production = 202020;

      let mut action = ParseAction::Undefined;

      next.call(&mut rt_ctx, &mut action);

      println!("{:#?}", action);

      assert!(
        matches!(action, ParseAction::Accept { production_id } if production_id == 202020),
      );
    };
  }
}
