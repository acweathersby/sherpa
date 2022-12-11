use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};

use super::{
  create_offset_label,
  input_block_indices::{InputBlockPtr, InputBlockSize, InputBlockStart, InputBlockTruncated},
  parse_ctx_indices::*,
  token_indices::{TokLength, TokOffset, TokType},
  FunctionPack,
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
  values::{FunctionValue, IntValue, PointerValue},
};

pub const _FAIL_STATE_FLAG_LLVM: u32 = 2;
pub const NORMAL_STATE_FLAG_LLVM: u32 = 1;

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

pub(crate) fn construct_parse_functions<'a>(
  g: &GrammarStore,
  ctx: &'a LLVMParserModule,
  output: &BytecodeOutput,
) -> SherpaResult<()> {
  let mut seen = BTreeSet::new();
  let mut goto_fn = BTreeSet::new();
  let mut functions = HashSet::new();

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

      functions.insert(function);

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

  for function in functions {
    if !function.verify(true) {
      return SherpaResult::Err(SherpaError::from("Could not build parse function"));
    }
  }

  SherpaResult::Ok(())
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
        (instruction, return_val) = construct_instruction_gotos(instruction, ctx, pack, referenced);

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

pub(crate) fn construct_instruction_branch<'a>(
  instruction: INSTRUCTION,
  g: &GrammarStore,
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
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

    // Convert the instruction data into table data.
    let table_name = create_offset_label(instruction.get_address() + 800000);

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

    let table_block = ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Table"));

    // Creates blocks for each branch, skip, and default.
    let default_block =
      ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Table_Default"));

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
          ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Is_Peeking"));

        let not_peeking_block =
          ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Not_Peeking"));

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
          ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Table_Branches"));

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
          (
            branch.value as u64,
            ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_skip")),
          )
        });
      } else {
        blocks
          .entry(INSTRUCTION::from(&pack.output.bytecode, branch.address as usize))
          .or_insert_with(|| {
            (
              branch.value as u64,
              ctx.ctx.append_basic_block(
                *pack.fun,
                &(table_name.clone() + "_" + &create_offset_label(branch.address)),
              ),
            )
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

              let this_block = ctx.ctx.append_basic_block(*pack.fun, &format!("this_{}", address));

              let next_block = if index == branches.len() - 1 {
                default_block
              } else {
                ctx.ctx.append_basic_block(*pack.fun, &format!("next_{}", address))
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
          let fun = get_parse_function(scanner_address, ctx, referenced)
            .as_global_value()
            .as_pointer_value()
            .into();

          let scan_tok = b
            .build_call(
              ctx.fun.scan,
              &[
                (parse_ctx).into(),
                fun,
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
            construct_instruction_branch(*instruction, g, ctx, pack, referenced, state.next())?;
          }
          _ => {
            construct_parse_function_statements(*instruction, g, ctx, pack, referenced)?;
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
        construct_instruction_branch(instruction, g, ctx, pack, referenced, state)?;
      }
      _ => {
        // Build code that deals with the outcomes that arrive when the
        // input block does not have enough bytes to fulfill all branches
        // but the reader could deliver more input.
        if state.input_buffer.is_some() {
          let good_size_block =
            ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_have_sizable_block"));

          let truncated_block =
            ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_block_is_truncated"));

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
              pack.fun.as_global_value().as_pointer_value().into(),
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

        construct_parse_function_statements(instruction, g, ctx, pack, referenced)?;
      }
    }
  }

  Ok(())
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
  i64: inkwell::types::IntType,
  table_block: inkwell::basic_block::BasicBlock,
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

pub(crate) fn construct_instruction_gotos<'a>(
  instruction: INSTRUCTION,
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) -> (INSTRUCTION, Option<IntValue<'a>>) {
  fn get_goto_fun<'a>(
    instruction: INSTRUCTION,
    bytecode: &Vec<u32>,
    ctx: &'a LLVMParserModule,
    referenced: &mut Vec<(INSTRUCTION, bool)>,
  ) -> FunctionValue<'a> {
    get_parse_function(instruction.goto(bytecode), ctx, referenced)
  }

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

  let last = gotos.pop().unwrap();

  if gotos.len() > 0 {
    let goto_top_ptr = b.build_struct_gep(parse_ctx, CTX_goto_ptr, "").unwrap();
    let mut goto_top = b.build_load(goto_top_ptr, "").into_pointer_value();
    for goto in &gotos {
      // Create new goto struct
      let goto_fn =
        get_goto_fun(*goto, bytecode, ctx, referenced).as_global_value().as_pointer_value();
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

  let goto_function = get_goto_fun(last, bytecode, ctx, referenced);
  match last.next(bytecode).to_type() {
    InstructionType::PASS => {
      // Call the function directly. This should end up as a tail call.
      let return_val = ctx
        .builder
        .build_call(
          goto_function,
          &[parse_ctx.into(), pack.fun.get_nth_param(1).unwrap().into_pointer_value().into()],
          "",
        )
        .try_as_basic_value()
        .unwrap_left()
        .into_int_value();

      b.build_return(Some(&return_val));

      (last.next(bytecode), Some(return_val))
    }
    _ => {
      b.build_call(
        ctx.fun.push_state,
        &[
          parse_ctx.into(),
          get_goto_state(last, ctx).into(),
          goto_function.as_global_value().as_pointer_value().into(),
        ],
        "",
      );

      (last.next(bytecode), None)
    }
  }
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
pub(crate) fn construct_prime_function(
  ctx: &LLVMParserModule,
  sp: &Vec<(usize, INSTRUCTION, String)>,
  referenced: &mut Vec<(INSTRUCTION, bool)>,
) -> Result<(), ()> {
  let i32 = ctx.ctx.i32_type();
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
