use crate::{
  llvm::{build_fast_call, LLVMParserModule, LLVMTypes},
  types::*,
};
use inkwell::{
  module::Linkage,
  types::StructType,
  values::{CallableValue, IntValue, PointerValue},
};

pub(crate) unsafe fn construct_ast_builder<ASTNode: Sized>(
  module: &LLVMParserModule,
) -> SherpaResult<()> {
  use inkwell::AddressSpace::*;

  let slot_size = std::mem::size_of::<(ASTNode, TokenRange, TokenRange)>() as u32;

  let LLVMParserModule { ctx, types, builder: b, .. } = module;
  let LLVMTypes { parse_ctx, action, .. } = types;
  let i8 = ctx.i8_type();
  let i32 = ctx.i32_type();
  let i64 = ctx.i64_type();
  let ACTION_PTR = action.ptr_type(Generic);
  let CTX_PTR = parse_ctx.ptr_type(Generic);

  // Struct types --------------------------------------------------

  let ast_slot = ctx.opaque_struct_type("s.AstSlot");
  let parse_result = ctx.opaque_struct_type("s.ParseResult");
  let ast_slot_stack_slice = ctx.opaque_struct_type("s.SlotStackSlice");

  // Struct Definitions --------------------------------------------------

  ast_slot_stack_slice.set_body(&[ast_slot.ptr_type(Generic).into(), i32.into()], false);

  parse_result
    .set_body(&[i8.array_type((std::mem::size_of::<ParseResult<ASTNode>>()) as u32).into()], false);

  ast_slot.set_body(&[i8.array_type(slot_size).into()], false);

  let discriminated_action = module
    .ctx
    .struct_type(
      &[i32.into(), i8.array_type((std::mem::size_of::<ParseAction>() - 4) as u32).into()],
      false,
    )
    .ptr_type(Generic);

  let reduce_action = ctx
    .struct_type(&[i32.into(), i32.into(), i32.into(), i32.into(), i32.into(), i32.into()], false);

  // Injected Functions ---------------------------------------------------

  let reducer_function = ctx
    .void_type()
    .fn_type(&[CTX_PTR.into(), ast_slot_stack_slice.ptr_type(Generic).into()], false);

  let shift_handler = ctx.void_type().fn_type(
    &[CTX_PTR.into(), ACTION_PTR.into(), ast_slot_stack_slice.ptr_type(Generic).into()],
    false,
  );

  let result_handler = parse_result.fn_type(
    &[CTX_PTR.into(), ACTION_PTR.into(), ast_slot_stack_slice.ptr_type(Generic).into()],
    false,
  );

  // Main Function ---------------------------------------------------

  let ast_builder = module.module.add_function(
    "ast_parse",
    parse_result.fn_type(
      &[
        parse_ctx.ptr_type(Generic).into(),
        reducer_function.ptr_type(Generic).ptr_type(Generic).into(),
        shift_handler.ptr_type(Global).into(),
        result_handler.ptr_type(Global).into(),
      ],
      false,
    ),
    Some(Linkage::External),
  );

  let parse_context = ast_builder.get_nth_param(0)?.into_pointer_value();
  let reducers = ast_builder.get_nth_param(1)?.into_pointer_value();
  let shift_handler = ast_builder.get_nth_param(2)?.into_pointer_value();
  let result_handler = ast_builder.get_nth_param(3)?.into_pointer_value();

  b.position_at_end(ctx.append_basic_block(ast_builder, "Preamble"));

  let parse_loop = ctx.append_basic_block(ast_builder, "ParseLoop");
  let shift = ctx.append_basic_block(ast_builder, "Shift");
  let shift_assign_base_pointer = ctx.append_basic_block(ast_builder, "AssignStackPointer");
  let shift_add_slot = ctx.append_basic_block(ast_builder, "AddSlot");
  let shift_new_object = ctx.append_basic_block(ast_builder, "ShiftNewObject");
  let reduce = ctx.append_basic_block(ast_builder, "Reduce");
  let default = ctx.append_basic_block(ast_builder, "Default");

  let stack_capacity_ptr = b.build_alloca(i32, "stack_capacity");
  b.build_store(stack_capacity_ptr, i32.const_zero());

  let stack_top_ptr = b.build_alloca(i32, "stack_top");
  b.build_store(stack_top_ptr, i32.const_zero());

  let action = b.build_alloca(*action, "action");
  let discriminated_action = b.build_pointer_cast(action, discriminated_action, "");
  let discriminant_ptr = b.build_struct_gep(discriminated_action, 0, "discriminant")?;

  let ast_slot_slice_ptr = b.build_alloca(ast_slot_stack_slice, "slot_lookup_ptr"); // Stores the stack lookup structure
  let slot_ptr_ptr = b.build_alloca(ast_slot.ptr_type(Generic), "slot_ptr_ptr"); // Store the pointer to the bottom of the AST stack

  b.build_store(slot_ptr_ptr, ast_slot.ptr_type(Generic).const_null());

  b.build_unconditional_branch(parse_loop);

  // Parse Loop --------------------------------------------------------

  b.position_at_end(parse_loop);
  // Begin by calling the dispatch function.

  build_fast_call(module, module.fun.dispatch, &[parse_context.into(), action.into()])?;

  // Load the discriminant from the action.
  let discriminant = b.build_load(discriminant_ptr, "").into_int_value();

  b.build_switch(discriminant, default, &[
    (i32.const_int(ParseAction::des_Shift, false), shift),
    (i32.const_int(ParseAction::des_Reduce, false), reduce),
  ]);

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

  // If the stack pointer is zero, assign the first slot address to this pointer.
  let stack_ptr = b.build_load(slot_ptr_ptr, "").into_pointer_value();
  let stack_ptr_val = b.build_ptr_to_int(stack_ptr, i64, "");
  let zero_ptr = ast_slot.ptr_type(Generic).const_null();
  let zero_ptr_val = b.build_ptr_to_int(zero_ptr, i64, "");

  let c = b.build_int_compare(inkwell::IntPredicate::EQ, stack_ptr_val, zero_ptr_val, "");
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
  let slot_lookup_size_ptr = b.build_struct_gep(ast_slot_slice_ptr, 1, "")?;
  b.build_store(slot_lookup_size_ptr, i32.const_int(1, false));

  b.build_call(
    CallableValue::try_from(shift_handler)?,
    &[parse_context.into(), action.into(), ast_slot_slice_ptr.into()],
    "",
  );
  b.build_unconditional_branch(parse_loop);
  // REDUCE --------------------------------------------------------
  b.position_at_end(reduce);
  // Get slice size
  let reduce_action = b
    .build_bitcast(action, reduce_action.ptr_type(Generic), "reduce_action_ptr")
    .into_pointer_value();

  let production_id_ptr = b.build_struct_gep(reduce_action, 1, "")?;
  let rule_id_ptr = b.build_struct_gep(reduce_action, 2, "")?;
  let symbol_count_ptr = b.build_struct_gep(reduce_action, 3, "")?;
  let symbol_count_original = b.build_load(symbol_count_ptr, "").into_int_value();

  // Calculate the position of the first element and the last element.
  let top = b.build_load(stack_top_ptr, "top").into_int_value();

  let symbol_count = b.build_int_sub(symbol_count_original, i32.const_int(1, false), "");
  let top = b.build_int_sub(top, symbol_count, "");
  b.build_store(stack_top_ptr, top);

  let bottom_slot_ptr = build_stack_offset_ptr(
    module,
    b.build_load(slot_ptr_ptr, "slot").into_pointer_value(),
    b.build_load(stack_top_ptr, "top").into_int_value(),
    ast_slot,
  );

  let rule_index = b.build_load(rule_id_ptr, "").into_int_value();

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
    &[parse_context.into(), ast_slot_slice_ptr.into()],
    "",
  );

  b.build_unconditional_branch(parse_loop);

  // DEFAULT --------------------------------------------------------
  b.position_at_end(default);
  let top = b.build_load(stack_top_ptr, "top").into_int_value();

  // Store slot and symbol info in lookup structure
  let slot_lookup_entry_ptr = b.build_struct_gep(ast_slot_slice_ptr, 0, "")?;
  b.build_store(slot_lookup_entry_ptr, b.build_load(slot_ptr_ptr, "slot").into_pointer_value());
  let slot_lookup_size_ptr = b.build_struct_gep(ast_slot_slice_ptr, 1, "")?;
  b.build_store(slot_lookup_size_ptr, top);

  let return_value = b.build_call(
    CallableValue::try_from(result_handler)?,
    &[parse_context.into(), action.into(), ast_slot_slice_ptr.into()],
    "",
  );

  b.build_return(Some(&return_value.try_as_basic_value().unwrap_left().into_struct_value()));

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
  let b = &module.builder;
  let i64 = module.ctx.i64_type();
  let i32 = module.ctx.i32_type();
  let ast_slot_size = ast_slot.size_of().unwrap();
  let top = b.build_int_sub(top, i32.const_int(1, false), "");
  let data_pointer_int = b.build_ptr_to_int(ast_stack_ptr, i64.into(), "");
  let top_64 = b.build_int_z_extend(top, i64, "");
  let top_64 = b.build_int_mul(top_64, ast_slot_size, "");
  let data_pointer_int = b.build_int_sub(data_pointer_int, top_64, "");
  let data_pointer = b.build_int_to_ptr(data_pointer_int, ast_stack_ptr.get_type(), "");
  data_pointer
}
