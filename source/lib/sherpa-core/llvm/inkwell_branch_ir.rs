use std::collections::BTreeMap;

use super::{
  create_offset_label,
  create_skip_code,
  get_parse_function,
  parse_ctx_indices::*,
  token_indices::{TokLength, TokOffset, TokType},
  FunctionPack,
};
use crate::{build::table::BranchTableData, llvm::LLVMParserModule, types::*};
use inkwell::values::{IntValue, PointerValue};

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

    let parse_ctx = *state
      .parse_ctx
      .get_or_insert_with(|| pack.fun.get_nth_param(0).unwrap().into_pointer_value());

    let action_pointer = *state
      .action_pointer
      .get_or_insert_with(|| pack.fun.get_nth_param(1).unwrap().into_pointer_value());

    // Convert the instruction data into table data.
    let table_name = create_offset_label(instruction.get_address() + 800000);

    // TODO: Construct buffer

    let table_block = ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Table"));

    let TableHeaderData { input_type, lexer_type, scan_index: scanner_address, .. } = data.data;

    let branches = &data.branches;
    let mut token_ptr = i32.ptr_type(inkwell::AddressSpace::Generic).const_null();
    let input_offset = i32.const_int(0, false);
    let mut value = i32.const_int(0, false);

    // Prepare the input token if we are working with
    // Token based branch types (TOKEN, BYTE, CODEPOINT, CLASS)
    match input_type {
      INPUT_TYPE::T01_PRODUCTION => {
        b.build_unconditional_branch(table_block);
        b.position_at_end(table_block);
      }
      _ => {
        let peek_mode_ptr = b.build_struct_gep(parse_ctx, CTX_peek_mode, "").unwrap();

        if lexer_type == LEXER_TYPE::ASSERT {
          token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_assert, "").unwrap();

          if !pack.is_scanner {
            b.build_store(peek_mode_ptr, i32.const_int(0, false));
          }
          b.build_unconditional_branch(table_block);
          b.position_at_end(table_block);
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
            b.build_int_compare(inkwell::IntPredicate::EQ, peek_mode, i32.const_int(1, false), "");

          let peek_token_off_ptr = b.build_struct_gep(token_ptr, TokOffset, "").unwrap();

          b.build_conditional_branch(comparison, is_peeking_block, not_peeking_block);

          b.position_at_end(is_peeking_block);
          let prev_token_len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();
          let prev_token_len = b.build_load(prev_token_len_ptr, "").into_int_value();
          let prev_token_off_ptr = b.build_struct_gep(token_ptr, TokOffset, "").unwrap();
          let prev_token_off = b.build_load(prev_token_off_ptr, "").into_int_value();
          let new_off = b.build_int_add(prev_token_len, prev_token_off, "");
          b.build_store(peek_token_off_ptr, new_off);

          b.build_unconditional_branch(table_block);

          b.position_at_end(not_peeking_block);
          let assert_token_ptr = b.build_struct_gep(parse_ctx, CTX_tok_assert, "").unwrap();
          let prev_token_len_ptr = b.build_struct_gep(assert_token_ptr, TokLength, "").unwrap();
          let prev_token_len = b.build_load(prev_token_len_ptr, "").into_int_value();
          let prev_token_off_ptr = b.build_struct_gep(assert_token_ptr, TokOffset, "").unwrap();
          let prev_token_off = b.build_load(prev_token_off_ptr, "").into_int_value();
          let new_off = b.build_int_add(prev_token_len, prev_token_off, "");

          b.build_store(peek_token_off_ptr, new_off);

          b.build_unconditional_branch(table_block);
          b.position_at_end(table_block);
        };
      }
    }

    let mut build_switch = true;

    // Creates blocks for each branch, skip, and default.
    let default_block =
      ctx.ctx.append_basic_block(*pack.fun, &(table_name.clone() + "_Table_Default"));

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
        if data.has_trivial_comparisons() {
          build_switch = false;

          // Store branch data in tuples comprized of (branch address, branch data,  token string)
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

          // Build a buffer to store the largest need register size, rounded to 4 bytes.

          let max_size =
            branches.iter().flat_map(|(_, _, s)| s).fold(0, |a, s| usize::max(a, s.len()));

          let buffer_ptr = construct_buffer(ctx, max_size, &mut state, pack, token_ptr);

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

              let token_length_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();

              b.build_store(
                token_length_ptr,
                ctx
                  .ctx
                  .i64_type()
                  .const_int((sym.byte_length as u64) | ((sym.cp_len as u64) << 32), false),
              );

              if !branch.is_skipped {
                b.build_store(
                  token_type_ptr,
                  ctx.ctx.i64_type().const_int(branch.value as u64, false),
                );
              }

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
            .build_call(ctx.fun.scan, &[(parse_ctx).into(), fun, token_ptr.into()], "")
            .try_as_basic_value()
            .unwrap_left()
            .into_struct_value();

          b.build_store(token_ptr, scan_tok);

          let type_ptr = b.build_struct_gep(token_ptr, TokType, "").unwrap();

          value = b.build_load(type_ptr, "").into_int_value();
        }
      }
      INPUT_TYPE::T01_PRODUCTION => {
        let production_ptr = b.build_struct_gep(parse_ctx, CTX_production, "").unwrap();
        value = b.build_load(production_ptr, "").into_int_value();
      }
      _ => {
        match input_type {
          INPUT_TYPE::T05_BYTE => {
            let buffer = construct_buffer(ctx, 1, &mut state, pack, token_ptr);

            let tok_len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();

            b.build_store(tok_len_ptr, ctx.ctx.i64_type().const_int((1 << 32) | 1, false));

            value = b.build_load(buffer, "").into_int_value();
          }

          INPUT_TYPE::T03_CLASS => {
            let buffer = construct_buffer(ctx, 4, &mut state, pack, token_ptr);

            let cp_val = construct_cp_lu_with_token_len_store(ctx, buffer, token_ptr);

            value = b
              .build_call(ctx.fun.get_token_class_from_codepoint, &[cp_val.into()], "")
              .try_as_basic_value()
              .unwrap_left()
              .into_int_value();
          }

          INPUT_TYPE::T04_CODEPOINT => {
            let buffer = construct_buffer(ctx, 4, &mut state, pack, token_ptr);
            value = construct_cp_lu_with_token_len_store(ctx, buffer, token_ptr);
          }
          _ => {}
        };
      }
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
        create_skip_code(b, token_ptr, i64, table_block);
      } else {
        match instruction.to_type() {
          InstructionType::HASH_BRANCH | InstructionType::VECTOR_BRANCH => {
            construct_instruction_branch(*instruction, g, ctx, pack, referenced, state.next())?;
          }
          _ => {
            super::construct_parse_function_statements(*instruction, g, ctx, pack, referenced)?;
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
            &[parse_ctx.into(), action_pointer.into(), state.input_block.unwrap().offset.into()],
            "",
          );
          b.build_return(Some(&i32.const_int(1, false)));

          //-- Enough bytes present block
          b.position_at_end(good_size_block);
        }

        super::construct_parse_function_statements(instruction, g, ctx, pack, referenced)?;
      }
    }
  }

  Ok(())
}

fn construct_cp_lu_with_token_len_store<'a>(
  ctx: &'a LLVMParserModule,
  buffer: PointerValue<'a>,
  token_ptr: PointerValue,
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

  let tok_len_ptr = b.build_struct_gep(token_ptr, TokLength, "").unwrap();

  b.build_store(tok_len_ptr, tok_len);

  cp_val
}

/// Builds instructions that copy the data of the input block
/// to a stack allocated buffer.
fn construct_buffer<'a>(
  ctx: &'a LLVMParserModule,
  max_size: usize,
  state: &mut BranchStateCache<'a>,
  pack: &'a FunctionPack,
  token_ptr: PointerValue<'a>,
) -> PointerValue<'a> {
  if state.input_buffer.is_some() && state.input_buffer_length >= max_size {
    state.input_buffer.unwrap()
  } else {
    let input_block = write_get_input_ptr_lookup(ctx, pack, max_size, token_ptr);

    let i32 = ctx.ctx.i32_type();
    let i8 = ctx.ctx.i8_type();
    let b = &ctx.builder;

    let buff_size = i32.const_int(max_size as u64, false);

    // TODO: move this section outside of the skip loop so we aren't continuously
    // allocate space on the stack.

    // Allocate memory on the stack
    let buffer_pointer = b.build_array_alloca(ctx.ctx.i8_type(), buff_size, "");

    // Zero fill the buffer.
    b.build_call(
      ctx.fun.memset,
      &[
        buffer_pointer.into(),
        i8.const_int(0, false).into(),
        buff_size.into(),
        ctx.ctx.bool_type().const_int(0, false).into(),
      ],
      "",
    );

    // Perform a memcpy between the input ptr and the buffer, using the smaller
    // of the two input block length and max_size as the amount of bytes to
    // copy.
    let min_size = b
      .build_call(
        ctx.fun.min,
        &[i32.const_int(max_size as u64, false).into(), input_block.size.into()],
        "",
      )
      .try_as_basic_value()
      .unwrap_left()
      .into_int_value();
    // Prepare a pointer to the token's type for later reuse in the switch block
    b.build_call(
      ctx.fun.memcpy,
      &[
        buffer_pointer.into(),
        input_block.pointer.into(),
        min_size.into(),
        ctx.ctx.bool_type().const_int(0, false).into(),
      ],
      "",
    );

    state.input_block = Some(input_block);
    state.input_buffer_length_int = Some(min_size);
    state.input_buffer = Some(buffer_pointer);
    state.input_buffer_length = max_size;

    buffer_pointer
  }
}

struct InputBlockRef<'a> {
  pointer:      PointerValue<'a>,
  size:         IntValue<'a>,
  offset:       IntValue<'a>,
  is_truncated: IntValue<'a>,
}

fn write_get_input_ptr_lookup<'a>(
  ctx: &'a LLVMParserModule,
  pack: &'a FunctionPack,
  max_length: usize,
  token_ptr: PointerValue<'a>,
) -> InputBlockRef<'a> {
  let i32 = ctx.ctx.i32_type();
  let b = &ctx.builder;
  let parse_ctx = pack.fun.get_first_param().unwrap().into_pointer_value();

  let input_block = b
    .build_call(
      ctx.fun.get_adjusted_input_block,
      &[parse_ctx.into(), token_ptr.into(), i32.const_int(max_length as u64, false).into()],
      "",
    )
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();

  InputBlockRef {
    pointer:      b.build_extract_value(input_block, 0, "input_ptr").unwrap().into_pointer_value(),
    offset:       b.build_extract_value(input_block, 1, "input_offset").unwrap().into_int_value(),
    size:         b.build_extract_value(input_block, 2, "input_size").unwrap().into_int_value(),
    is_truncated: b
      .build_extract_value(input_block, 3, "input_truncated")
      .unwrap()
      .into_int_value(),
  }
}
