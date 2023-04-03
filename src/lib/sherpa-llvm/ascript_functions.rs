use crate::{
  build_fast_call,
  LLVMParserModule,
  LLVMTypes,
  CTX_AGGREGATE_INDICES,
};
use inkwell::{
  module::Linkage,
  types::StructType,
  values::{CallableValue, IntValue, PointerValue},
};
use sherpa_core::*;
use sherpa_runtime::types::{
  ast::{AstObject, AstSlot},
  ParseActionType,
  ParseResult,
};

pub(crate) unsafe fn construct_ast_builder<ASTNode: AstObject>(
  module: &LLVMParserModule,
) -> SherpaResult<()> {
  let slot_size = std::mem::size_of::<AstSlot<ASTNode>>() as u32;

  let LLVMParserModule { ctx, types, b, .. } = module;
  let LLVMTypes { parse_ctx, .. } = types;
  let i8 = ctx.i8_type();
  let i32 = ctx.i32_type();
  let i64 = ctx.i64_type();
  let CTX_PTR = parse_ctx.ptr_type(0.into());

  // Struct types --------------------------------------------------

  let ast_slot = ctx.opaque_struct_type("s.AstObject");
  let parse_result = ctx.opaque_struct_type("s.ParseResult");
  let ast_slot_stack_slice = ctx.opaque_struct_type("s.SlotStackSlice");

  // Struct Definitions --------------------------------------------------

  ast_slot_stack_slice.set_body(
    &[ast_slot.ptr_type(0.into()).into(), i32.into(), i8.into()],
    false,
  );

  parse_result.set_body(
    &[i8
      .array_type((std::mem::size_of::<ParseResult<ASTNode>>()) as u32)
      .into()],
    false,
  );

  ast_slot.set_body(&[i8.array_type(slot_size).into()], false);

  // Injected Functions ---------------------------------------------------

  let reducer_function = ctx.void_type().fn_type(
    &[CTX_PTR.into(), ast_slot_stack_slice.ptr_type(0.into()).into()],
    false,
  );

  let shift_handler = ctx.void_type().fn_type(
    &[CTX_PTR.into(), ast_slot_stack_slice.ptr_type(0.into()).into()],
    false,
  );

  let result_handler = parse_result.fn_type(
    &[
      CTX_PTR.into(),
      i32.into(),
      ast_slot_stack_slice.ptr_type(0.into()).into(),
    ],
    false,
  );

  // Main Function ---------------------------------------------------

  let ast_builder = module.module.add_function(
    "ast_parse",
    parse_result.fn_type(
      &[
        parse_ctx.ptr_type(0.into()).into(),
        reducer_function.ptr_type(0.into()).ptr_type(0.into()).into(),
        shift_handler.ptr_type(0.into()).into(),
        result_handler.ptr_type(0.into()).into(),
      ],
      false,
    ),
    Some(Linkage::External),
  );

  let parse_ctx = ast_builder.get_nth_param(0)?.into_pointer_value();
  let reducers = ast_builder.get_nth_param(1)?.into_pointer_value();
  let shift_handler = ast_builder.get_nth_param(2)?.into_pointer_value();
  let result_handler = ast_builder.get_nth_param(3)?.into_pointer_value();

  b.position_at_end(ctx.append_basic_block(ast_builder, "Preamble"));

  let parse_loop = ctx.append_basic_block(ast_builder, "ParseLoop");
  let shift = ctx.append_basic_block(ast_builder, "Shift");
  let skip = ctx.append_basic_block(ast_builder, "Skip");
  let shift_assign_base_pointer =
    ctx.append_basic_block(ast_builder, "AssignStackPointer");
  let shift_add_slot = ctx.append_basic_block(ast_builder, "AddSlot");
  let shift_new_object = ctx.append_basic_block(ast_builder, "ShiftNewObject");
  let reduce = ctx.append_basic_block(ast_builder, "Reduce");
  let default = ctx.append_basic_block(ast_builder, "Default");

  let stack_capacity_ptr = b.build_alloca(i32, "stack_capacity");
  b.build_store(stack_capacity_ptr, i32.const_zero());

  let stack_top_ptr = b.build_alloca(i32, "stack_top");
  b.build_store(stack_top_ptr, i32.const_zero());

  let ast_slot_slice_ptr =
    b.build_alloca(ast_slot_stack_slice, "slot_lookup_ptr"); // Stores the stack lookup structure

  // UNUSED PROPERTY
  // --------------------------------------------------------------------
  // - let slot_direction = b.build_struct_gep(ast_slot_slice_ptr, 2, "")?;
  // - b.build_store(slot_direction, i8.const_int(1, false)); // Directory of
  //   slot is decreasing

  let slot_ptr_ptr =
    b.build_alloca(ast_slot.ptr_type(0.into()), "slot_ptr_ptr"); // Store the pointer to the bottom of the AST stack

  b.build_store(slot_ptr_ptr, ast_slot.ptr_type(0.into()).const_null());

  b.build_unconditional_branch(parse_loop);

  // Parse Loop --------------------------------------------------------

  b.position_at_end(parse_loop);
  // Begin by calling the dispatch function.

  let discriminant =
    build_fast_call(b, module.fun.dispatch, &[parse_ctx.into()])?
      .try_as_basic_value()
      .left()?
      .into_int_value();

  b.build_switch(discriminant, default, &[
    (i32.const_int(ParseActionType::Skip.into(), false), skip),
    (i32.const_int(ParseActionType::Shift.into(), false), shift),
    (i32.const_int(ParseActionType::Reduce.into(), false), reduce),
  ]);

  // Skip --------------------------------------------------------
  b.position_at_end(skip);
  //Ignoring skip actions for the time being.
  b.build_unconditional_branch(parse_loop);

  // SHIFT --------------------------------------------------------
  b.position_at_end(shift);

  let top = b.build_load(stack_top_ptr, "top").into_int_value();
  let capacity = b.build_load(stack_capacity_ptr, "capacity").into_int_value();

  let c = b.build_int_compare(inkwell::IntPredicate::UGE, top, capacity, "");
  b.build_conditional_branch(c, shift_add_slot, shift_new_object);
  // ADD SLOT --------------------------------------------------------
  b.position_at_end(shift_add_slot);

  // Need bottom and top pointer for the internally maintained stack.
  let slot_ptr = b.build_alloca(ast_slot, "stack");
  let capacity = b.build_int_add(capacity, i32.const_int(1, false), "");
  b.build_store(stack_capacity_ptr, capacity);

  // If the stack pointer is zero, assign the first slot address to this
  // pointer.
  let stack_ptr = b.build_load(slot_ptr_ptr, "").into_pointer_value();
  let stack_ptr_val = b.build_ptr_to_int(stack_ptr, i64, "");
  let zero_ptr = ast_slot.ptr_type(0.into()).const_null();
  let zero_ptr_val = b.build_ptr_to_int(zero_ptr, i64, "");

  let c = b.build_int_compare(
    inkwell::IntPredicate::EQ,
    stack_ptr_val,
    zero_ptr_val,
    "",
  );
  b.build_conditional_branch(c, shift_assign_base_pointer, shift_new_object);

  b.position_at_end(shift_assign_base_pointer);

  b.build_store(slot_ptr_ptr, slot_ptr);

  b.build_unconditional_branch(shift_new_object);

  // SHIFT OBJECT --------------------------------------------------------
  b.position_at_end(shift_new_object);

  // Increment the top to look into the next slot.
  let top = b.build_int_add(top, i32.const_int(1, false), "");
  b.build_store(stack_top_ptr, top);

  // Call shift handler
  // Calculate the position of the empty object's first field
  let slot_ptr = build_stack_offset_ptr(
    module,
    b.build_load(slot_ptr_ptr, "slot").into_pointer_value(),
    b.build_load(stack_top_ptr, "top").into_int_value(),
    ast_slot,
  );
  // Store slot and symbol info in lookup structure
  let slot_lookup_entry_ptr = b.build_struct_gep(ast_slot_slice_ptr, 0, "")?;
  b.build_store(slot_lookup_entry_ptr, slot_ptr);
  let slot_length = b.build_struct_gep(ast_slot_slice_ptr, 1, "")?;
  b.build_store(slot_length, i32.const_int(1, false));

  b.build_call(
    CallableValue::try_from(shift_handler)?,
    &[parse_ctx.into(), ast_slot_slice_ptr.into()],
    "",
  );
  b.build_unconditional_branch(parse_loop);
  // REDUCE --------------------------------------------------------
  b.position_at_end(reduce);
  // Get slice size
  let symbol_count_original =
    CTX_AGGREGATE_INDICES::sym_len.load(b, parse_ctx)?.into_int_value();
  let rule_index =
    CTX_AGGREGATE_INDICES::rule_id.load(b, parse_ctx)?.into_int_value();

  // Calculate the position of the first element and the last element.
  let top = b.build_load(stack_top_ptr, "top").into_int_value();

  let symbol_count =
    b.build_int_sub(symbol_count_original, i32.const_int(1, false), "");
  let top = b.build_int_sub(top, symbol_count, "");
  b.build_store(stack_top_ptr, top);

  let bottom_slot_ptr = build_stack_offset_ptr(
    module,
    b.build_load(slot_ptr_ptr, "slot").into_pointer_value(),
    b.build_load(stack_top_ptr, "top").into_int_value(),
    ast_slot,
  );

  // Load the parse function and pass the stack into it.
  let reducer = b.build_gep(reducers, &[rule_index.into()], "");
  let reducer = b.build_load(reducer, "").into_pointer_value();

  // Store slot and symbol info in lookup structure
  let slot_lookup_entry_ptr = b.build_struct_gep(ast_slot_slice_ptr, 0, "")?;
  b.build_store(slot_lookup_entry_ptr, bottom_slot_ptr);
  let slot_lookup_size_ptr = b.build_struct_gep(ast_slot_slice_ptr, 1, "")?;
  b.build_store(slot_lookup_size_ptr, symbol_count_original);

  b.build_call(
    CallableValue::try_from(reducer)?,
    &[parse_ctx.into(), ast_slot_slice_ptr.into()],
    "",
  );

  b.build_unconditional_branch(parse_loop);

  // DEFAULT --------------------------------------------------------
  b.position_at_end(default);
  let top = b.build_load(stack_top_ptr, "top").into_int_value();

  // Store slot and symbol info in lookup structure
  let slot_lookup_entry_ptr = b.build_struct_gep(ast_slot_slice_ptr, 0, "")?;
  b.build_store(
    slot_lookup_entry_ptr,
    b.build_load(slot_ptr_ptr, "slot").into_pointer_value(),
  );
  let slot_lookup_size_ptr = b.build_struct_gep(ast_slot_slice_ptr, 1, "")?;
  b.build_store(slot_lookup_size_ptr, top);

  let return_value = b.build_call(
    CallableValue::try_from(result_handler)?,
    &[parse_ctx.into(), discriminant.into(), ast_slot_slice_ptr.into()],
    "",
  );

  b.build_return(Some(
    &return_value.try_as_basic_value().unwrap_left().into_struct_value(),
  ));

  if ast_builder.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("\n\nCould not validate ast_builder"))
  }
}

fn build_stack_offset_ptr<'a>(
  module: &'a LLVMParserModule,
  ast_stack_ptr: PointerValue<'a>,
  top: IntValue<'a>,
  ast_slot: StructType<'a>,
) -> PointerValue<'a> {
  let b = &module.b;
  let i64 = module.ctx.i64_type();
  let i32 = module.ctx.i32_type();
  let ast_slot_size = ast_slot.size_of().unwrap();
  let top = b.build_int_sub(top, i32.const_int(1, false), "");
  let data_pointer_int = b.build_ptr_to_int(ast_stack_ptr, i64.into(), "");
  let top_64 = b.build_int_z_extend(top, i64, "");
  let top_64 = b.build_int_mul(top_64, ast_slot_size, "");
  let data_pointer_int = b.build_int_sub(data_pointer_int, top_64, "");
  let data_pointer =
    b.build_int_to_ptr(data_pointer_int, ast_stack_ptr.get_type(), "");
  data_pointer
}
