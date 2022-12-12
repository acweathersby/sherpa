use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};

use super::{
  create_offset_label,
  input_block_indices::{InputBlockPtr, InputBlockSize, InputBlockStart, InputBlockTruncated},
  parse_ctx_indices::*,
  token_indices::{TokLength, TokOffset, TokType},
};
use crate::{
  build::table::BranchTableData,
  compile::BytecodeOutput,
  llvm::{LLVMParserModule, FAIL_STATE_FLAG_LLVM},
  types::*,
};
use inkwell::{
  basic_block::BasicBlock,
  builder::Builder,
  types::IntType,
  values::{FunctionValue, IntValue, PointerValue},
};

pub(crate) struct FunctionPack<'a> {
  pub(crate) fun: &'a FunctionValue<'a>,
  pub(crate) output: &'a BytecodeOutput,
  pub(crate) is_scanner: bool,
  pub(crate) dispatch_block: BasicBlock<'a>,
  pub(crate) state: LLVMStateData<'a>,
  pub(crate) states: &'a BTreeMap<INSTRUCTION, LLVMStateData<'a>>,
}

pub const _FAIL_STATE_FLAG_LLVM: u32 = 2;
pub const NORMAL_STATE_FLAG_LLVM: u32 = 1;

#[derive(Clone, Copy)]
pub struct LLVMStateData<'a> {
  pub entry_instruction: INSTRUCTION,
  pub block_address: PointerValue<'a>,
  pub entry_block: BasicBlock<'a>,
  pub fun: FunctionValue<'a>,
}

impl<'a> LLVMStateData<'a> {
  pub fn generate_name(bc_address: usize) -> String {
    format!("parse_fn_{:X}", bc_address)
  }

  pub fn new(
    bc: &[u32],
    bc_address: usize,
    ctx: &'a LLVMParserModule,
    fun: &'a FunctionValue,
  ) -> Self {
    let entry_instruction = INSTRUCTION::from(bc, bc_address);
    let entry_block = ctx.ctx.append_basic_block(*fun, &Self::generate_name(bc_address));
    let block_address = unsafe { entry_block.get_address().unwrap() };
    Self { entry_block, block_address, entry_instruction, fun: *fun }
  }

  pub fn get_name(&self) -> String {
    Self::generate_name(self.entry_instruction.get_address())
  }

  pub fn generate_block(
    &self,
    ctx: &'a LLVMParserModule,
    base_name: &str,
    block_address: usize,
  ) -> BasicBlock<'a> {
    ctx.ctx.append_basic_block(
      self.fun,
      &format!("{}_{}_{:X}", self.get_name(), base_name, block_address),
    )
  }
}

#[derive(Default)]
pub(crate) struct BranchStateCache<'a> {
  input_buffer: Option<PointerValue<'a>>,
  input_buffer_length_int: Option<IntValue<'a>>,
  parse_ctx: Option<PointerValue<'a>>,
  action_pointer: Option<PointerValue<'a>>,
  input_block: Option<InputBlockRef<'a>>,
  input_buffer_length: usize,
}

impl<'a> BranchStateCache<'a> {
  pub fn next(&self) -> Self {
    BranchStateCache {
      input_buffer: None,
      input_buffer_length_int: None,
      input_block: None,
      ..*self
    }
  }
}

pub(crate) fn get_state_data<'a>(
  instruction: INSTRUCTION,
  state: &'a BTreeMap<INSTRUCTION, LLVMStateData<'a>>,
) -> LLVMStateData<'a> {
  println!("Getting info for block {:X}", instruction.get_address());
  *state.get(&instruction).unwrap()
}

pub(crate) fn construct_parse_functions<'a>(
  g: &GrammarStore,
  ctx: &'a LLVMParserModule,
  output: &BytecodeOutput,
) -> SherpaResult<FunctionValue<'a>> {
  let mut seen = BTreeSet::new();
  let mut states = BTreeMap::new();

  // Create the task master function;
  let main_fun = ctx.module.add_function("task_master", ctx.types.goto_fn, None);

  let entry_block = ctx.ctx.append_basic_block(main_fun, "Entry");
  let parse_ctx = main_fun.get_nth_param(0).unwrap().into_pointer_value();

  for (_, address) in &output.state_name_to_offset {
    let state = LLVMStateData::new(&output.bytecode, *address as usize, ctx, &main_fun);
    states.insert(state.entry_instruction, state);
  }

  // Find all instructions that create synthetic states.
  let mut instruction = INSTRUCTION::from(&output.bytecode, FIRST_STATE_ADDRESS as usize);
  loop {
    if !instruction.is_valid() {
      break;
    }
    use InstructionType::*;
    match instruction.to_type() {
      REDUCE | SHIFT => {
        let state = LLVMStateData::new(
          &output.bytecode,
          instruction.next(&output.bytecode).get_address(),
          ctx,
          &main_fun,
        );
        states.insert(state.entry_instruction, state);
      }
      _ => {}
    }
    instruction = instruction.next(&output.bytecode);
  }

  // Build emit_end_of_parse
  let end_of_parse_block = unsafe {
    let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

    let i32 = ctx.i32_type();

    let end_of_parse_block = ctx.append_basic_block(main_fun, "EndOfParse");

    b.position_at_end(end_of_parse_block);

    b.build_call(
      funct.emit_eop,
      &[parse_ctx.into(), main_fun.get_nth_param(1)?.into_pointer_value().into()],
      "",
    );

    b.build_return(Some(&i32.const_int(0, false)));

    end_of_parse_block
  };

  // Insert next instruction near top
  let dispatch_block = unsafe {
    let LLVMParserModule { builder: b, ctx, fun: funct, .. } = ctx;

    let i32 = ctx.i32_type();
    let zero = i32.const_int(0, false);

    // Set the context's goto pointers to point to the goto block;
    let block_dispatch = ctx.append_basic_block(main_fun, "Dispatch");
    let block_useful_state = ctx.append_basic_block(main_fun, "ModeAppropriateState");

    b.position_at_end(entry_block);
    b.build_unconditional_branch(block_dispatch);

    b.position_at_end(block_dispatch);
    let state = b.build_load(b.build_struct_gep(parse_ctx.into(), CTX_state, "state")?, "state");

    let goto_top_ptr = b.build_struct_gep(parse_ctx, CTX_goto_ptr, "")?;
    // let goto_top = b.build_in_bounds_gep(goto_top_ptr, &[zero], "");
    let goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();
    let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "");
    b.build_store(goto_top_ptr, goto_top);

    let goto_remaining_ptr = b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "")?;
    let goto_remaining = b.build_load(goto_remaining_ptr, "").into_int_value();
    let goto_remaining = b.build_int_add(goto_remaining, i32.const_int(1, false), "");

    b.build_store(goto_remaining_ptr, goto_remaining);

    let goto = b.build_load(goto_top, "").into_struct_value();

    let goto_state = b.build_extract_value(goto, 1, "")?;

    let masked_state = b.build_and(state.into_int_value(), goto_state.into_int_value(), "");
    let condition = b.build_int_compare(inkwell::IntPredicate::NE, masked_state, zero, "");
    b.build_conditional_branch(condition, block_useful_state, block_dispatch);

    b.position_at_end(block_useful_state);

    let block_address = b.build_extract_value(goto, 0, "")?.into_int_value();

    let block_address = b.build_int_to_ptr(
      block_address,
      block_dispatch.get_address().unwrap().get_type(),
      "block_address",
    );

    b.build_indirect_branch(
      block_address,
      &states.values().map(|v| v.entry_block).chain(vec![end_of_parse_block]).collect::<Vec<_>>(),
    );

    block_dispatch
  };

  let mut states_queue = states.keys().cloned().collect::<VecDeque<_>>();

  while let Some(instruction) = states_queue.pop_front() {
    if seen.insert(instruction) {
      let state = { *states.get(&instruction)? };

      let mut is_scanner = false;

      if state.get_name() == "parse_fn_D" {
        println!("Here We Are!");
      }

      ctx.builder.position_at_end(state.entry_block);

      if let Some(ir_state_name) =
        output.offset_to_state_name.get(&(instruction.get_address() as u32))
      {
        if let Some(state) = output.state_data.get(ir_state_name) {
          if state.get_goto_depth() > 1 {
            ctx.builder.build_call(
              ctx.fun.extend_stack_if_needed,
              &[
                parse_ctx.into(),
                ctx.ctx.i32_type().const_int((state.get_goto_depth() + 2) as u64, false).into(),
              ],
              "",
            );
          }

          is_scanner = state.is_scanner();
        }
      }

      let fn_pack = FunctionPack {
        fun: &main_fun,
        output,
        is_scanner,
        states: &states,
        state: state,
        dispatch_block,
      };

      construct_parse_function_statements(instruction, g, ctx, &fn_pack)?;
    }
  }

  if !main_fun.verify(true) {
    return SherpaResult::Ok(main_fun);
    return SherpaResult::Err(SherpaError::from("Could not build parse function"));
  }

  // Construct the prime function with the addresses of the entry blocks
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

  unsafe {
    construct_prime_function(ctx, &start_points, &states, &end_of_parse_block)?;

    construct_scan(ctx, &end_of_parse_block)?;
  }

  SherpaResult::Ok(main_fun)
}

pub(super) fn construct_parse_function_statements(
  mut instruction: INSTRUCTION,
  g: &GrammarStore,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
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
          construct_instruction_shift(instruction, ctx, pack);
          break;
        }
      }
      GOTO => {
        let branched;
        (instruction, branched) = construct_instruction_gotos(instruction, ctx, pack);

        if branched {
          break;
        }
      }
      SET_PROD => {
        instruction = construct_instruction_prod(instruction, ctx, pack);
      }
      REDUCE => {
        construct_instruction_reduce(instruction, ctx, pack);
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
        construct_instruction_branch(instruction, g, ctx, pack, Default::default())?;
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

pub(crate) fn construct_instruction_branch<'a>(
  instruction: INSTRUCTION,
  g: &GrammarStore,
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
  mut state: BranchStateCache<'a>,
) -> Result<(), ()> {
  if let Some(data) = BranchTableData::from_bytecode(instruction, g, pack.output) {
    let b = &ctx.builder;
    let i32 = ctx.ctx.i32_type();
    let i64 = ctx.ctx.i64_type();
    let bool = ctx.ctx.bool_type();

    let parse_ctx = *state
      .parse_ctx
      .get_or_insert_with(|| pack.fun.get_nth_param(0).unwrap().into_pointer_value());

    let action_pointer = *state
      .action_pointer
      .get_or_insert_with(|| pack.fun.get_nth_param(1).unwrap().into_pointer_value());

    let TableHeaderData { input_type, lexer_type, scan_index: scanner_address, .. } = data.data;

    let cache_offset_ptr = b.build_alloca(i64, "byte_offset");
    let cache_length_ptr = b.build_alloca(i64, "byte_length");

    let trivial_token_comparisons =
      input_type == INPUT_TYPE::T02_TOKEN && data.has_trivial_comparisons();

    // Retrieve the maximum number of bytes that will be read
    // per round within this function.
    let (max_size, is_goto_branch) = match input_type {
      INPUT_TYPE::T02_TOKEN => {
        if trivial_token_comparisons {
          (
            getBranchTokenData(g, &data)
              .iter()
              .flat_map(|(_, _, s)| s)
              .fold(0, |a, s| usize::max(a, s.len())),
            false,
          )
        } else {
          (0, false)
        }
      }
      INPUT_TYPE::T05_BYTE => (1, false),
      INPUT_TYPE::T03_CLASS | INPUT_TYPE::T04_CODEPOINT => (4, false),
      _ => (0, true),
    };

    // Construct a buffer, if necessary, that will hold up to `max_size` bytes.
    let mut buffer_ptr = if state.input_buffer.is_some() && state.input_buffer_length >= max_size {
      state.input_buffer.unwrap()
    } else if max_size > 0 {
      state.input_buffer_length = max_size;
      let i32 = ctx.ctx.i32_type();
      let i8 = ctx.ctx.i8_type();
      let b = &ctx.builder;
      let buff_size = i32.const_int(max_size as u64, false);
      // Allocate memory on the stack
      b.build_array_alloca(ctx.ctx.i8_type(), buff_size, "")
    } else {
      i32.ptr_type(inkwell::AddressSpace::Generic).const_null()
    };

    let table_block = pack.state.generate_block(ctx, "_Table", instruction.get_address());

    // Creates blocks for each branch, skip, and default.
    let default_block = pack.state.generate_block(ctx, "_Table_Default", instruction.get_address());

    let branches = &data.branches;
    let mut token_ptr = i32.ptr_type(inkwell::AddressSpace::Generic).const_null();
    let mut value = i32.const_int(0, false);

    // Prepare the input token if we are working with
    // Token based branch types (TOKEN, BYTE, CODEPOINT, CLASS)

    if is_goto_branch {
      b.build_unconditional_branch(table_block);
      b.position_at_end(table_block);
    } else {
      let peek_mode_ptr = b.build_struct_gep(parse_ctx, CTX_peek_mode, "").unwrap();

      if lexer_type == LEXER_TYPE::ASSERT {
        token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_assert, "").unwrap();

        if !pack.is_scanner {
          // Set peek mode to `false`
          b.build_store(peek_mode_ptr, bool.const_int(0, false));
        }

        build_offset_assign(b, token_ptr, cache_offset_ptr)?;

        b.build_unconditional_branch(table_block);
      } else {
        // Need to increment the peek token by either the previous length of the peek
        // token, or the current length of the assert token.
        let is_peeking_block =
          pack.state.generate_block(ctx, "is_peeking", instruction.get_address());

        let not_peeking_block =
          pack.state.generate_block(ctx, "not_peeking", instruction.get_address());

        token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_peek, "").unwrap();

        let peek_mode = b.build_load(peek_mode_ptr, "").into_int_value();

        let comparison =
          b.build_int_compare(inkwell::IntPredicate::EQ, peek_mode, bool.const_int(1, false), "");

        let peek_token_off_ptr = b.build_struct_gep(token_ptr, TokOffset, "").unwrap();

        b.build_conditional_branch(comparison, is_peeking_block, not_peeking_block);

        b.position_at_end(is_peeking_block);
        let prev_token_len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();
        let prev_token_len = b.build_load(prev_token_len_ptr, "").into_int_value();
        let prev_token_off_ptr = b.build_struct_gep(token_ptr, TokOffset, "").unwrap();
        let prev_token_off = b.build_load(prev_token_off_ptr, "").into_int_value();
        let new_off = b.build_int_add(prev_token_len, prev_token_off, "");
        b.build_store(peek_token_off_ptr, new_off);
        build_offset_assign(b, token_ptr, cache_offset_ptr)?;
        b.build_unconditional_branch(table_block);

        b.position_at_end(not_peeking_block);
        let assert_token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_assert, "").unwrap();
        let prev_token_len_ptr = b.build_struct_gep(assert_token_ptr, TokLength, "").unwrap();
        let prev_token_len = b.build_load(prev_token_len_ptr, "").into_int_value();
        let prev_token_off_ptr = b.build_struct_gep(assert_token_ptr, TokOffset, "").unwrap();
        let prev_token_off = b.build_load(prev_token_off_ptr, "").into_int_value();
        let new_off = b.build_int_add(prev_token_len, prev_token_off, "");
        build_offset_assign(b, token_ptr, cache_offset_ptr)?;
        b.build_store(peek_token_off_ptr, new_off);

        b.build_unconditional_branch(table_block);
      };

      b.position_at_end(table_block);

      // Only initialize input buffer if we are directly comparing it's data with
      // token values, otherwise we are using scanner functions, and there
      // is no need to access input data.
      if (input_type != INPUT_TYPE::T02_TOKEN) || trivial_token_comparisons {
        let byte_offset = b.build_load(cache_offset_ptr, "byte_offset").into_int_value();
        let byte_offset = b.build_int_truncate(byte_offset, i32.into(), "");

        // Build Input lookup
        let input_block = write_get_input_ptr_lookup(ctx, pack, max_size, byte_offset);

        // If the block size is less than 1 then jump to default
        let comparison = b.build_int_compare(
          inkwell::IntPredicate::EQ,
          i32.const_int(0, false),
          input_block.size,
          "",
        );

        let branch_block =
          pack.state.generate_block(ctx, "table_branches", instruction.get_address());

        b.build_conditional_branch(comparison, default_block, branch_block);
        b.position_at_end(branch_block);

        buffer_ptr = input_block.pointer.clone();

        // Prepare a pointer to the token's type for later reuse in the switch block
        state.input_block = Some(input_block);
        state.input_buffer = Some(buffer_ptr.clone().into());
      }
    }

    let mut build_switch = true;
    // Check to see if we need to create a local data buffer.

    let mut blocks = BTreeMap::new();

    for branch in branches.values() {
      if branch.is_skipped {
        blocks.entry(INSTRUCTION::default()).or_insert_with(|| {
          (branch.value as u64, pack.state.generate_block(ctx, "skip", branch.address))
        });
      } else {
        blocks
          .entry(INSTRUCTION::from(&pack.output.bytecode, branch.address as usize))
          .or_insert_with(|| {
            (branch.value as u64, pack.state.generate_block(ctx, "branch", branch.address))
          });
      }
    }

    match input_type {
      INPUT_TYPE::T02_TOKEN => {
        if trivial_token_comparisons {
          build_switch = false;

          let branches = getBranchTokenData(g, &data);

          // Build a buffer to store the largest need register size, rounded to 4 bytes.

          let token_type_ptr = b.build_struct_gep(token_ptr, TokType, "").unwrap();

          fn string_to_byte_num_and_mask(string: &str, _: &Symbol) -> (usize, usize) {
            string.as_bytes().iter().enumerate().fold((0, 0), |(val, mask), (i, v)| {
              let shift_amount = 8 * i;
              (val | ((*v as usize) << shift_amount), mask | (0xFF << shift_amount))
            })
          }

          for (index, (address, branch, strings)) in branches.iter().enumerate() {
            let sym = data.get_branch_symbol(branch).unwrap();

            let mut comparison = ctx.ctx.i8_type().const_int(0, false);
            for string in strings {
              match sym.byte_length {
                len if len == 1 => {
                  let value = b.build_load(buffer_ptr, "").into_int_value();
                  comparison = b.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    value,
                    value
                      .get_type()
                      .const_int(string_to_byte_num_and_mask(string, sym).0 as u64, false),
                    "",
                  );
                }
                len if len <= 8 => {
                  let (byte_string, mask) = string_to_byte_num_and_mask(string, sym);
                  let adjusted_byte = b
                    .build_bitcast(buffer_ptr, i64.ptr_type(inkwell::AddressSpace::Generic), "")
                    .into_pointer_value();
                  let value = b.build_load(adjusted_byte, "").into_int_value();
                  let masked_value =
                    b.build_and(value, value.get_type().const_int(mask as u64, false), "");
                  comparison = b.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    masked_value,
                    value.get_type().const_int(byte_string as u64, false),
                    "",
                  );
                }
                _ => {}
              }

              let this_block = pack.state.generate_block(ctx, "this_", **address);

              let next_block = if index == branches.len() - 1 {
                default_block
              } else {
                pack.state.generate_block(ctx, "next_", **address)
              };

              b.build_conditional_branch(comparison, this_block, next_block);
              b.position_at_end(this_block);

              b.build_store(
                cache_length_ptr,
                ctx
                  .ctx
                  .i64_type()
                  .const_int((sym.byte_length as u64) | ((sym.cp_len as u64) << 32), false),
              );

              b.build_unconditional_branch(if branch.is_skipped {
                blocks.get(&INSTRUCTION::default()).unwrap().1
              } else {
                blocks.get(&INSTRUCTION::from(&pack.output.bytecode, branch.address)).unwrap().1
              });
              b.position_at_end(next_block);
            }
          }
        } else {
          let scan_tok = b
            .build_call(
              ctx.fun.scan,
              &[
                (parse_ctx).into(),
                block_ptr_to_int(scanner_address, pack, ctx).into(),
                b.build_load(cache_offset_ptr, "").into_int_value().into(),
              ],
              "",
            )
            .try_as_basic_value()
            .unwrap_left()
            .into_struct_value();

          // b.build_store(token_ptr, scan_tok);

          b.build_store(
            cache_length_ptr,
            b.build_extract_value(scan_tok, TokLength, "").unwrap().into_int_value(),
          );

          b.build_store(
            cache_offset_ptr,
            b.build_extract_value(scan_tok, TokOffset, "").unwrap().into_int_value(),
          );

          value = b.build_extract_value(scan_tok, TokType, "").unwrap().into_int_value();
        }
      }
      INPUT_TYPE::T01_PRODUCTION => {
        let production_ptr = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();
        value = b.build_load(production_ptr, "").into_int_value();
      }
      INPUT_TYPE::T05_BYTE => {
        // let tok_len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();
        // b.build_store(tok_len_ptr, ctx.ctx.i64_type().const_int((1 << 32) | 1, false));
        b.build_store(cache_length_ptr, ctx.ctx.i64_type().const_int((1 << 32) | 1, false));
        value = b.build_load(buffer_ptr, "").into_int_value();
      }
      INPUT_TYPE::T03_CLASS => {
        let cp_val = construct_cp_lu_with_token_len_store(ctx, buffer_ptr, cache_length_ptr);
        value = b
          .build_call(ctx.fun.get_token_class_from_codepoint, &[cp_val.into()], "")
          .try_as_basic_value()
          .unwrap_left()
          .into_int_value();
      }
      INPUT_TYPE::T04_CODEPOINT => {
        value = construct_cp_lu_with_token_len_store(ctx, buffer_ptr, cache_length_ptr);
      }
      _ => {}
    }

    if build_switch {
      // Create Switch statements.
      let value_type = value.get_type();
      let mut cases = vec![];

      for (instruction, (value, block)) in &blocks {
        if *instruction == INSTRUCTION::default() {
          cases.push((value_type.const_int(*value, false), *block));
        } else {
          cases.push((value_type.const_int(*value, false), *block));
        }
      }

      b.build_switch(value, default_block, &cases);
    }

    // Write branches, ending with the default branch.

    for (instruction, (_, block)) in &blocks {
      b.position_at_end(*block);
      if *instruction == INSTRUCTION::default() {
        create_skip_code(b, cache_offset_ptr, cache_length_ptr, i64, table_block);
      } else {
        if !is_goto_branch {
          // Copy byte length and offset to token;
          let tok_off_ptr = b.build_struct_gep(token_ptr, TokOffset, "").unwrap();
          let tok_len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();

          b.build_store(tok_off_ptr, b.build_load(cache_offset_ptr, "offset").into_int_value());
          b.build_store(tok_len_ptr, b.build_load(cache_length_ptr, "length").into_int_value());
        }

        match instruction.to_type() {
          InstructionType::HASH_BRANCH | InstructionType::VECTOR_BRANCH => {
            construct_instruction_branch(*instruction, g, ctx, pack, state.next())?;
          }
          _ => {
            construct_parse_function_statements(*instruction, g, ctx, pack)?;
          }
        }
      }
    }
    let instruction = INSTRUCTION::from(
      &pack.output.bytecode,
      (pack.output.bytecode[instruction.get_address() + 3] as usize) + instruction.get_address(),
    );

    b.position_at_end(default_block);
    match instruction.to_type() {
      InstructionType::HASH_BRANCH | InstructionType::VECTOR_BRANCH => {
        construct_instruction_branch(instruction, g, ctx, pack, state)?;
      }
      _ => {
        // Build code that deals with the outcomes that arrive when the
        // input block does not have enough bytes to fulfill all branches
        // but the reader could deliver more input.
        if state.input_buffer.is_some() {
          let good_size_block =
            pack.state.generate_block(ctx, "have_sizable_block", instruction.get_address());

          let truncated_block =
            pack.state.generate_block(ctx, "block_is_truncated", instruction.get_address());

          let comparison = b.build_int_compare(
            inkwell::IntPredicate::EQ,
            state.input_block.as_ref().unwrap().is_truncated,
            ctx.ctx.bool_type().const_int(1 as u64, false),
            "",
          );

          b.build_conditional_branch(comparison, truncated_block, good_size_block);

          //-- Truncated block
          b.position_at_end(truncated_block);
          b.build_call(
            ctx.fun.push_state,
            &[
              parse_ctx.into(),
              i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
              b.build_ptr_to_int(pack.state.block_address, i64.into(), "").into(),
            ],
            "",
          );

          b.build_call(
            ctx.fun.emit_eoi,
            &[parse_ctx.into(), action_pointer.into(), state.input_block.unwrap().start.into()],
            "",
          );
          b.build_return(Some(&i32.const_int(1, false)));

          //-- Enough bytes present block
          b.position_at_end(good_size_block);
        }

        construct_parse_function_statements(instruction, g, ctx, pack)?;
      }
    }
  }

  Ok(())
}

fn block_ptr_to_int<'a>(
  scanner_address: INSTRUCTION,
  pack: &'a FunctionPack,
  ctx: &'a LLVMParserModule,
) -> IntValue<'a> {
  ctx.builder.build_ptr_to_int(
    get_state_data(scanner_address, pack.states).block_address,
    ctx.ctx.i64_type().into(),
    "",
  )
}

fn build_offset_assign(
  b: &inkwell::builder::Builder,
  token_ptr: PointerValue,
  byte_offset_ptr: PointerValue,
) -> Result<(), ()> {
  let token_offset_byte_offset = b.build_struct_gep(token_ptr, TokOffset, "token_cursor")?;
  let token_offset_byte_offset = b.build_load(token_offset_byte_offset, "").into_int_value();
  b.build_store(byte_offset_ptr, token_offset_byte_offset);
  Ok(())
}

/// Store branch data in tuples comprized of (branch address, branch data,  token string)
fn getBranchTokenData<'a>(
  g: &'a GrammarStore,
  data: &'a BranchTableData,
) -> Vec<(&'a usize, &'a crate::build::table::BranchData, Vec<&'a str>)> {
  let branches = data
    .branches
    .iter()
    .map(|(address, branch)| {
      let sym = data.get_branch_symbol(branch).unwrap();
      let string = match sym.guid {
        id if id.is_defined() => {
          vec![g.symbol_strings.get(&id).unwrap().as_str()]
        }
        SymbolID::GenericSpace => {
          vec![" "]
        }
        _ => vec![""],
      };

      (address, branch, string)
    })
    .collect::<Vec<_>>();
  branches
}

fn construct_cp_lu_with_token_len_store<'a>(
  ctx: &'a LLVMParserModule,
  buffer: PointerValue<'a>,
  byte_length_ptr: PointerValue,
) -> IntValue<'a> {
  let b = &ctx.builder;
  let cp_info = b
    .build_call(ctx.fun.get_utf8_codepoint_info, &[buffer.into()], "")
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();
  let cp_val = b.build_extract_value(cp_info, 0, "cp_val").unwrap().into_int_value();

  let cp_len = b.build_extract_value(cp_info, 1, "cp_len").unwrap().into_int_value();

  let tok_len = b.build_int_z_extend(cp_len, ctx.ctx.i64_type(), "tok_len");

  let tok_len = b.build_or(tok_len, ctx.ctx.i64_type().const_int(1 << 32, false), "tok_len");

  b.build_store(byte_length_ptr, tok_len);

  cp_val
}

fn construct_buffer_data<'a>(
  ctx: &'a LLVMParserModule,
  max_size: usize,
  state: &mut BranchStateCache<'a>,
  pack: &'a FunctionPack,
  token_ptr: PointerValue<'a>,
) -> PointerValue<'a> {
  if state.input_buffer.is_some() && state.input_buffer_length >= max_size {
    state.input_buffer.unwrap()
  } else {
    let i32 = ctx.ctx.i32_type();
    let i8 = ctx.ctx.i8_type();
    let b = &ctx.builder;
    let buff_size = i32.const_int(max_size as u64, false);
    // Allocate memory on the stack
    b.build_array_alloca(ctx.ctx.i8_type(), buff_size, "")
  }
}
struct InputBlockRef<'a> {
  pointer:      PointerValue<'a>,
  /// The number of bytes that can be read
  /// from this block
  size:         IntValue<'a>,
  /// The offset of the block
  start:        IntValue<'a>,
  is_truncated: IntValue<'a>,
}

fn write_get_input_ptr_lookup<'a>(
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
  max_length: usize,
  byte_offset: IntValue<'a>,
) -> InputBlockRef<'a> {
  let i32 = ctx.ctx.i32_type();
  let b = &ctx.builder;
  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();

  let input_block = b
    .build_call(
      ctx.fun.get_adjusted_input_block,
      &[parse_ctx.into(), byte_offset.into(), i32.const_int(max_length as u64, false).into()],
      "",
    )
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();

  InputBlockRef {
    pointer:      b
      .build_extract_value(input_block, InputBlockPtr, "input_ptr")
      .unwrap()
      .into_pointer_value(),
    start:        b
      .build_extract_value(input_block, InputBlockStart, "input_offset")
      .unwrap()
      .into_int_value(),
    size:         b
      .build_extract_value(input_block, InputBlockSize, "input_size")
      .unwrap()
      .into_int_value(),
    is_truncated: b
      .build_extract_value(input_block, InputBlockTruncated, "input_truncated")
      .unwrap()
      .into_int_value(),
  }
}

pub(crate) fn create_skip_code(
  b: &Builder,
  byte_offset_ptr: PointerValue,
  byte_len_ptr: PointerValue,
  i64: IntType,
  table_block: BasicBlock,
) {
  let off = b.build_load(byte_offset_ptr, "offset").into_int_value();
  let len = b.build_load(byte_len_ptr, "length").into_int_value();
  b.build_store(byte_offset_ptr, b.build_int_add(off, len, "new_offset"));
  b.build_store(byte_len_ptr, i64.const_int(0, false));
  b.build_unconditional_branch(table_block);
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
    b.build_unconditional_branch(pack.dispatch_block);
    //b.build_return(Some(&return_val));
  } else {
    b.build_unconditional_branch(pack.dispatch_block);
    //b.build_return(Some(&ctx.ctx.i32_type().const_int(0, false)));
  }
}

pub(crate) fn construct_instruction_fail(ctx: &LLVMParserModule, pack: &FunctionPack) {
  let parse_ctx = pack.fun.get_nth_param(0).unwrap().into_pointer_value();
  let b = &ctx.builder;
  let state_ptr = b.build_struct_gep(parse_ctx, CTX_state, "").unwrap();
  b.build_store(state_ptr, ctx.ctx.i32_type().const_int(FAIL_STATE_FLAG_LLVM as u64, false));
  b.build_unconditional_branch(pack.dispatch_block);
  //b.build_return(Some(&ctx.ctx.i32_type().const_int(0, false)));
}

pub(crate) fn construct_instruction_gotos<'a>(
  instruction: INSTRUCTION,
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
) -> (INSTRUCTION, bool) {
  fn get_goto_state<'a>(last: INSTRUCTION, ctx: &'a LLVMParserModule) -> IntValue<'a> {
    if (last.get_value() & FAIL_STATE_FLAG) > 0 {
      ctx.ctx.i32_type().const_int(FAIL_STATE_FLAG_LLVM as u64, false)
    } else {
      ctx.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false)
    }
  }

  let bytecode = &pack.output.bytecode;
  let mut address = instruction.get_address();
  let b = &ctx.builder;
  let i32 = ctx.ctx.i32_type();

  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();

  let mut gotos = vec![];

  // Extract all gotos from the function.
  while INSTRUCTION::from(bytecode, address).is_GOTO() {
    gotos.push(INSTRUCTION::from(bytecode, address));
    address += 1;
  }

  let last = if matches!(gotos.last().unwrap().next(bytecode).to_type(), InstructionType::PASS) {
    gotos.pop()
  } else {
    None
  };

  if gotos.len() > 0 {
    let goto_top_ptr = b.build_struct_gep(parse_ctx, CTX_goto_ptr, "").unwrap();
    let mut goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();
    for goto in &gotos {
      // Create new goto struct
      let goto_fn = get_state_data(goto.goto(bytecode), pack.states).block_address;
      let goto_fn = b.build_ptr_to_int(goto_fn, ctx.ctx.i64_type(), "goto_block");
      let goto_state = get_goto_state(*goto, ctx);
      let new_goto = b.build_insert_value(ctx.types.goto.get_undef(), goto_state, 1, "").unwrap();
      let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "").unwrap();

      // Store in the current slot
      b.build_store(goto_top, new_goto);

      // Increment the slot
      goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false)], "") };
    }
    // Store the top slot
    b.build_store(goto_top_ptr, goto_top);

    // Decrement goto remaining
    let goto_remaining_ptr = b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "").unwrap();
    let goto_remaining = b.build_load(goto_remaining_ptr, "").into_int_value();
    let goto_remaining =
      b.build_int_sub(goto_remaining, i32.const_int(gotos.len() as u64, false), "");
    b.build_store(goto_remaining_ptr, goto_remaining);
  }

  if let Some(last) = last {
    ctx
      .builder
      .build_unconditional_branch(get_state_data(last.goto(bytecode), pack.states).entry_block);
    (last.next(bytecode), true)
  } else {
    (gotos.last().unwrap().next(bytecode), false)
  }
}

pub(crate) fn construct_instruction_reduce(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) {
  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();
  let symbol_count = instruction.get_value() >> 16 & 0x0FFF;
  let rule_id = instruction.get_value() & 0xFFFF;

  write_emit_reentrance(instruction.next(&pack.output.bytecode), ctx, pack);

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

fn write_emit_reentrance<'a>(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
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
        ctx
          .builder
          .build_ptr_to_int(
            get_state_data(next_instruction, pack.states).block_address,
            ctx.ctx.i64_type(),
            "",
          )
          .into(),
      ],
      "",
    );
  }
}

pub(crate) fn construct_instruction_shift(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) {
  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();

  write_emit_reentrance(instruction.next(&pack.output.bytecode), ctx, pack);

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

/// The prime function's purpose is fist create a base goto state that emits a failure
/// to prevent stack underflow when descending to the bottom of the goto stack, and also
/// insert the first GOTO entry that will initiate the parser to start parsing based on
/// an entry production id.
pub(crate) unsafe fn construct_prime_function(
  ctx: &LLVMParserModule,
  sp: &Vec<(usize, INSTRUCTION, String)>,
  states: &BTreeMap<INSTRUCTION, LLVMStateData>,
  eop_block: &BasicBlock,
) -> SherpaResult<()> {
  let i32 = ctx.ctx.i32_type();
  let i64 = ctx.ctx.i64_type();
  let bool = ctx.ctx.bool_type();
  let b = &ctx.builder;
  let funct = &ctx.fun;
  let fn_value = funct.prime;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let selector = fn_value.get_nth_param(1).unwrap().into_int_value(); // Set the context's goto pointers to point to the goto block;
  let entry = ctx.ctx.append_basic_block(fn_value, "Entry");
  b.position_at_end(entry);

  let active_ptr = b.build_struct_gep(parse_ctx, CTX_is_active, "").unwrap();
  b.build_store(active_ptr, bool.const_int(1, false));

  let blocks = sp
    .iter()
    .map(|(id, instruction, ..)| {
      (
        *id,
        ctx.ctx.append_basic_block(fn_value, &create_offset_label(instruction.get_address())),
        get_state_data(*instruction, states).block_address,
      )
    })
    .collect::<Vec<_>>();

  // Push the End-Of-Parse goto onto the stack. This will prevent underflow of the stack
  b.build_call(
    funct.push_state,
    &[
      parse_ctx.into(),
      i32.const_int((NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64, false).into(),
      b.build_ptr_to_int(eop_block.get_address()?, i64.into(), "").into(),
    ],
    "",
  );

  if !blocks.is_empty() {
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
          b.build_ptr_to_int(*fn_ptr, i64.into(), "").into(),
        ],
        "",
      );

      b.build_return(None);
    }
  } else {
    b.build_return(None);
  }

  if funct.prime.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build prime function"))
  }
}

pub(crate) unsafe fn construct_scan(
  ctx: &LLVMParserModule,
  eop_block: &BasicBlock,
) -> SherpaResult<()> {
  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = ctx;

  let i32 = ctx.i32_type();
  let i64 = ctx.i64_type();

  let fn_value = funct.scan;

  //## Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");
  let success = ctx.append_basic_block(fn_value, "Produce_Scan_Token");
  let failure = ctx.append_basic_block(fn_value, "Produce_Failed_Token");

  //## Extract Params
  let parse_ctx = fn_value.get_nth_param(0)?.into_pointer_value();
  let scanner_entry_goto = fn_value.get_nth_param(1)?.into_int_value();
  let start_offset = fn_value.get_nth_param(2)?.into_int_value();

  //## Entry Block
  b.position_at_end(entry);

  let scan_ctx = b.build_alloca(types.parse_ctx, "");

  // The scan context inherits its goto stack and current input block
  // from the main parser context. These instruction copy data from one
  // context to the other.

  // Character Reader
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_reader, "")?,
    b.build_load(b.build_struct_gep(parse_ctx, CTX_reader, "")?, ""),
  );

  // Goto Stack Data
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_goto_ptr, "")?,
    b.build_load(b.build_struct_gep(parse_ctx, CTX_goto_ptr, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_goto_stack_remaining, "")?,
    b.build_load(b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_goto_stack_len, "")?,
    b.build_load(b.build_struct_gep(parse_ctx, CTX_goto_stack_len, "")?, ""),
  );

  // Input Block data
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_input_block, "")?,
    b.build_load(b.build_struct_gep(parse_ctx, CTX_input_block, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_get_input_block, "")?,
    b.build_load(b.build_struct_gep(parse_ctx, CTX_get_input_block, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(scan_ctx, CTX_state, "")?,
    i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false),
  );

  // Copy input token to the Assert and Anchor token slots of the scan context.
  let input_token_ptr = b.build_alloca(types.token, "input_token");
  let input_token_offset_ptr =
    b.build_struct_gep(input_token_ptr, TokOffset, "input_token_offset")?;
  b.build_store(input_token_offset_ptr, start_offset);

  let input_token_len_ptr = b.build_struct_gep(input_token_ptr, TokLength, "input_token_length")?;
  b.build_store(input_token_len_ptr, i64.const_int(0, false));

  let input_token = b.build_load(input_token_ptr, "input_token");
  let assert_token = b.build_struct_gep(scan_ctx, CTX_tok_assert, "")?;
  let anchor_token = b.build_struct_gep(scan_ctx, CTX_tok_anchor, "")?;

  b.build_store(assert_token, input_token);
  b.build_store(anchor_token, input_token);

  b.build_call(
    funct.push_state,
    &[
      scan_ctx.into(),
      i32.const_int((NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64, false).into(),
      b.build_ptr_to_int(eop_block.get_address()?, i64.into(), "").into(),
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
    b.build_struct_gep(parse_ctx, CTX_goto_ptr, "")?,
    b.build_load(b.build_struct_gep(scan_ctx, CTX_goto_ptr, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_goto_stack_remaining, "")?,
    b.build_load(b.build_struct_gep(scan_ctx, CTX_goto_stack_remaining, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_goto_stack_len, "")?,
    b.build_load(b.build_struct_gep(scan_ctx, CTX_goto_stack_len, "")?, ""),
  );
  b.build_store(
    b.build_struct_gep(parse_ctx, CTX_input_block, "")?,
    b.build_load(b.build_struct_gep(scan_ctx, CTX_input_block, "")?, ""),
  );

  // Produce either a failure token or a success token based on
  // outcome of the `next` call.

  let action_type = b.build_struct_gep(action.into_pointer_value(), 0, "")?;

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
  let offset_min = b.build_struct_gep(anchor_token, TokOffset, "")?;
  let offset_min = b.build_load(offset_min, "");
  let offset_max = b.build_struct_gep(assert_token, TokOffset, "")?;
  let offset_max = b.build_load(offset_max, "");

  let offset_diff = b.build_int_sub(offset_max.into_int_value(), offset_min.into_int_value(), "");

  let len = b.build_struct_gep(anchor_token, TokLength, "")?;

  b.build_store(len, offset_diff);

  let token = b.build_load(anchor_token, "");

  b.build_return(Some(&token));

  //## Failure Block
  b.position_at_end(failure);

  let type_ = b.build_struct_gep(anchor_token, TokType, "")?;

  b.build_store(type_, i64.const_int(0, false));

  let token = b.build_load(anchor_token, "");
  b.build_return(Some(&token));

  if funct.scan.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build scan function"))
  }
}
