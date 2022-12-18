use super::{build_fast_call, create_offset_label, fastCC, CTX_AGGREGATE_INDICES as CTX};
use crate::{
  build::table::BranchTableData,
  compile::BytecodeOutput,
  debug::address_string,
  grammar::data::ast::Generated,
  llvm::{LLVMParserModule, FAIL_STATE_FLAG_LLVM},
  types::*,
};
use inkwell::{
  basic_block::BasicBlock,
  module::Linkage,
  values::{CallableValue, FunctionValue, IntValue, PointerValue},
};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub(crate) struct FunctionPack<'a> {
  pub(crate) fun:        &'a FunctionValue<'a>,
  pub(crate) output:     &'a BytecodeOutput,
  pub(crate) is_scanner: bool,
  pub(crate) state:      LLVMStateData<'a>,
  pub(crate) states:     &'a BTreeMap<INSTRUCTION, LLVMStateData<'a>>,
}

pub const _FAIL_STATE_FLAG_LLVM: u32 = 2;
pub const NORMAL_STATE_FLAG_LLVM: u32 = 1;

#[derive(Clone, Copy, Debug)]
pub struct LLVMStateData<'a> {
  pub entry_instruction: INSTRUCTION,
  pub function_pointer:  PointerValue<'a>,
  pub function:          FunctionValue<'a>,
  pub is_scanner:        bool,
}

impl<'a> LLVMStateData<'a> {
  pub fn generate_name(bc_address: usize) -> String {
    format!("parse_fn_{}", address_string(bc_address))
  }

  pub fn new(
    bc: &[u32],
    bc_address: usize,
    module: &'a LLVMParserModule,
    is_scanner: bool,
  ) -> Self {
    let entry_instruction = INSTRUCTION::from(bc, bc_address);

    let entry_function = module.module.add_function(
      &Self::generate_name(bc_address),
      module.types.TAIL_CALLABLE_PARSE_FUNCTION,
      //None,
      Some(Linkage::Private),
    );

    entry_function.set_call_conventions(fastCC);

    Self {
      function: entry_function,
      function_pointer: entry_function.as_global_value().as_pointer_value(),
      entry_instruction,
      is_scanner,
    }
  }

  pub fn get_name(&self) -> String {
    Self::generate_name(self.entry_instruction.get_address())
  }

  pub fn generate_block(
    &self,
    module: &'a LLVMParserModule,
    base_name: &str,
    block_address: usize,
  ) -> BasicBlock<'a> {
    module.ctx.append_basic_block(
      self.function,
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
  if !state.contains_key(&instruction) {
    dbg!(instruction);
  }
  *state.get(&instruction).unwrap()
}

pub(crate) unsafe fn construct_parse_function<'a>(
  g: &GrammarStore,
  module: &'a LLVMParserModule,
  output: &BytecodeOutput,
) -> SherpaResult<()> {
  let start_points = get_start_points(g, output);

  // from the entry states, get a list of all states that can be accessed in
  // the LLVM based parser. Note: this will be different than bytecode states for
  // two reasons:
  // - Token branch states MAY directly compare binary values instead of calling their
  //   scanner function, leading to dead scanner states.
  // - Emit instructions (SHIFT & REDUCE) bifurcate their states, leading to new synthetic
  //   states being created that are jumped to when parsing is resumed.

  // Starting with the root states, collect a list of all states that can be reached.
  let states = {
    let mut seen = BTreeSet::new();
    let mut states_queue =
      VecDeque::from_iter(start_points.iter().map(|(_, instr)| (*instr, false)));
    let mut states = BTreeMap::new();

    while let Some((instruction, is_scanner)) = states_queue.pop_front() {
      if seen.insert(instruction) {
        let state =
          LLVMStateData::new(&output.bytecode, instruction.get_address(), module, is_scanner);
        states.insert(state.entry_instruction, state);
        let bc = &output.bytecode;

        let mut instructions = VecDeque::from_iter(vec![(instruction)]);

        while let Some(mut instruction) = instructions.pop_back() {
          while instruction.is_valid() {
            use InstructionType::*;
            match instruction.to_type() {
              SET_PROD | TOKEN | FORK_TO | EAT_CRUMBS | NOOP13 | SCAN | REPEAT | ASSERT_SHIFT
              | SET_FAIL_STATE => {
                instruction = instruction.next(bc);
              }
              GOTO => {
                states_queue.push_front((instruction.goto(bc), is_scanner));
                instruction = instruction.next(bc);
              }
              SHIFT => {
                if !is_scanner {
                  states_queue.push_front((instruction.next(bc), is_scanner)); // Synthetic state
                  break;
                } else {
                  instruction = instruction.next(bc);
                }
              }
              REDUCE => {
                states_queue.push_front((instruction.next(bc), is_scanner)); // Synthetic state
                break;
              }
              VECTOR_BRANCH | HASH_BRANCH => {
                let data = BranchTableData::from_bytecode(instruction, g, output)?;

                if data.data.input_type == INPUT_TYPE::T02_TOKEN && !data.has_trivial_comparisons()
                {
                  states_queue.push_front((data.data.scan_state_entry_instruction, true));
                }

                for (address, _) in data.branches.iter().filter(|b| !b.1.is_skipped) {
                  states_queue.push_front((INSTRUCTION::from(bc, *address), is_scanner));
                }

                states_queue.push_front((instruction.branch_default(bc), is_scanner));
                break;
              }
              FAIL | PASS => {
                break;
              }
            }
          }
        }
      }
    }
    states
  };

  for instruction in states.keys().copied() {
    let state = { *states.get(&instruction)? };

    let fun = state.function;

    let entry_block = module.ctx.append_basic_block(fun, "Entry");

    module.builder.position_at_end(entry_block);

    let fn_pack = FunctionPack {
      fun: &fun,
      output,
      is_scanner: state.is_scanner,
      states: &states,
      state: state,
    };

    construct_parse_function_statements(instruction, g, module, &fn_pack)?;

    if !fun.verify(true) {
      return SherpaResult::Err(SherpaError::from(format!(
        "Could not build parse function {}",
        state.get_name(),
      )));
    }
  }

  // Construct the prime function with the addresses of the entry blocks
  construct_prime_function(module, &start_points, &states)?;

  SherpaResult::Ok(())
}

fn get_start_points(g: &GrammarStore, output: &BytecodeOutput) -> Vec<(usize, INSTRUCTION)> {
  let start_points = g
    .get_exported_productions()
    .iter()
    .enumerate()
    .map(|(i, p)| {
      let instruction = INSTRUCTION::from(
        &output.bytecode,
        *(output.state_name_to_offset.get(p.guid_name).unwrap()) as usize,
      );
      (i, instruction)
    })
    .collect::<Vec<_>>();
  start_points
}

pub(super) fn construct_parse_function_statements(
  mut instruction: INSTRUCTION,
  g: &GrammarStore,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
  let FunctionPack { output, is_scanner, .. } = pack;

  if !instruction.is_valid() {
    SherpaResult::Ok(())
  } else {
    while instruction.is_valid() {
      use InstructionType::*;
      match instruction.to_type() {
        SHIFT => {
          if *is_scanner {
            instruction = construct_scanner_shift(instruction, module, pack)?;
          } else {
            construct_shift(instruction, module, pack);
            break;
          }
        }
        GOTO => {
          let branched;
          (instruction, branched) = construct_goto(instruction, module, pack)?;

          if branched {
            break;
          }
        }
        SET_PROD => {
          instruction = construct_instruction_prod(instruction, module, pack)?;
        }
        REDUCE => {
          construct_reduce(instruction, module, pack)?;
          break;
        }
        TOKEN => {
          instruction = construct_token(instruction, module, pack)?;
        }
        FORK_TO => {
          // TODO
          break;
        }
        EAT_CRUMBS | NOOP13 | SCAN | REPEAT | ASSERT_SHIFT | SET_FAIL_STATE => {
          instruction = instruction.next(&output.bytecode);
        }
        VECTOR_BRANCH | HASH_BRANCH => {
          construct_instruction_branch(instruction, g, module, pack, Default::default())?;
          break;
        }
        FAIL => {
          construct_fail(module, pack)?;
          break;
        }
        PASS => {
          construct_instruction_pass(module, pack)?;
          break;
        }
      }
    }

    SherpaResult::Ok(())
  }
}

pub(crate) fn construct_instruction_branch<'a>(
  instruction: INSTRUCTION,
  g: &GrammarStore,
  module: &'a LLVMParserModule,
  p: &'a FunctionPack,
  mut state: BranchStateCache<'a>,
) -> SherpaResult<()> {
  if let Some(data) = BranchTableData::from_bytecode(instruction, g, p.output) {
    let b = &module.builder;
    let i32 = module.ctx.i32_type();
    let i64 = module.ctx.i64_type();
    let bool = module.ctx.bool_type();

    let parse_ctx =
      *state.parse_ctx.get_or_insert_with(|| p.fun.get_nth_param(0).unwrap().into_pointer_value());
    let cache_offset_ptr = CTX::token_off.get_ptr(module, parse_ctx)?;
    let cache_length_ptr = CTX::scan_len.get_ptr(module, parse_ctx)?;
    let cache_line_ptr = CTX::anchor_line_off.get_ptr(module, parse_ctx)?;
    let cache_peek_offset = CTX::peek_off.get_ptr(module, parse_ctx)?;

    let action_pointer = *state
      .action_pointer
      .get_or_insert_with(|| p.fun.get_nth_param(1).unwrap().into_pointer_value());

    let TableHeaderData {
      input_type,
      lexer_type,
      scan_state_entry_instruction: scanner_address,
      ..
    } = data.data;

    let is_peek = matches!(lexer_type, LEXER_TYPE::PEEK);
    let is_scanner = p.is_scanner;

    let (input_ptr_ptr, input_size_ptr, input_truncated_ptr) = match (is_peek, is_scanner) {
      (true, _) => (
        CTX::peek_ptr.get_ptr(module, parse_ctx)?,
        CTX::peek_input_len.get_ptr(module, parse_ctx)?,
        CTX::peek_input_trun.get_ptr(module, parse_ctx)?,
      ),
      (_, true) => (
        CTX::scan_ptr.get_ptr(module, parse_ctx)?,
        CTX::scan_input_len.get_ptr(module, parse_ctx)?,
        CTX::scan_input_trun.get_ptr(module, parse_ctx)?,
      ),
      _ => (
        CTX::tok_ptr.get_ptr(module, parse_ctx)?,
        CTX::tok_input_len.get_ptr(module, parse_ctx)?,
        CTX::tok_input_trun.get_ptr(module, parse_ctx)?,
      ),
    };

    let trivial_token_comparisons =
      input_type == INPUT_TYPE::T02_TOKEN && data.has_trivial_comparisons();

    // Retrieve the maximum number of bytes that will be read
    // per round within this function.
    let (max_length, is_goto_branch) = match input_type {
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
    let mut buffer_ptr = None;
    let branches = &data.branches;
    let table_block = p.state.generate_block(module, "_Table", instruction.get_address());
    let default_block = p.state.generate_block(module, "_Table_Default", instruction.get_address());
    let mut active_offset = cache_offset_ptr;
    let mut value = i32.const_int(0, false);

    // Prepare the input token if we are working with
    // Token based branch types (TOKEN, BYTE, CODEPOINT, CLASS)

    if is_goto_branch {
      b.build_unconditional_branch(table_block);
      b.position_at_end(table_block);
    } else {
      let peek_mode_ptr = CTX::in_peek_mode.get_ptr(module, parse_ctx)?;

      if lexer_type == LEXER_TYPE::ASSERT {
        b.build_store(peek_mode_ptr, bool.const_int(0, false));
        b.build_unconditional_branch(table_block);
      } else {
        // Need to increment the peek token by either the previous length of the peek
        // token, or the current length of the assert token.
        let (continue_peeking, start_peeking) = (
          p.state.generate_block(module, "continue_peeking", instruction.get_address()),
          p.state.generate_block(module, "start_peeking", instruction.get_address()),
        );

        let peek_mode = b.build_load(peek_mode_ptr, "").into_int_value();

        let comparison =
          b.build_int_compare(inkwell::IntPredicate::EQ, peek_mode, bool.const_int(1, false), "");

        b.build_conditional_branch(comparison, continue_peeking, start_peeking);

        b.position_at_end(continue_peeking);
        {
          let prev_token_len = b.build_load(cache_length_ptr, "").into_int_value();
          let prev_token_off = b.build_load(cache_peek_offset, "").into_int_value();
          let new_off = b.build_int_add(prev_token_len, prev_token_off, "");

          unsafe {
            let input_ptr = b.build_load(input_ptr_ptr, "").into_pointer_value();
            let input_ptr = b.build_gep(input_ptr, &[prev_token_len.into()], "");
            b.build_store(input_ptr_ptr, input_ptr);
          }

          b.build_store(cache_peek_offset, new_off);
          b.build_unconditional_branch(table_block);
        }

        b.position_at_end(start_peeking);
        {
          let prev_token_len = b.build_load(cache_length_ptr, "").into_int_value();
          let prev_token_off = b.build_load(active_offset, "").into_int_value();
          let new_off = b.build_int_add(prev_token_len, prev_token_off, "");

          unsafe {
            let input_ptr = CTX::tok_ptr.load(module, parse_ctx)?.into_pointer_value();
            let input_ptr = b.build_gep(input_ptr, &[prev_token_len.into()], "");
            b.build_store(input_ptr_ptr, input_ptr);
          }
          b.build_store(cache_peek_offset, new_off);
          b.build_unconditional_branch(table_block);
        }

        active_offset = cache_peek_offset;
      };

      b.position_at_end(table_block);

      // Only initialize input buffer if we are directly comparing it's data with
      // token values, otherwise we are using scanner functions, and there
      // is no need to access input data.
      if (input_type != INPUT_TYPE::T02_TOKEN) || trivial_token_comparisons {
        let byte_offset = b.build_load(active_offset, "byte_offset").into_int_value();

        check_for_input_acceptability(
          module,
          p,
          input_ptr_ptr,
          input_size_ptr,
          input_truncated_ptr,
          byte_offset,
          i32.const_int(max_length as u64, false),
        );

        let input_size = b.build_load(input_size_ptr, "").into_int_value();

        // If the block size is less than 1 then jump to default
        let comparison =
          b.build_int_compare(inkwell::IntPredicate::EQ, i32.const_int(0, false), input_size, "");

        let branch_block =
          p.state.generate_block(module, "table_branches", instruction.get_address());

        b.build_conditional_branch(comparison, default_block, branch_block);
        b.position_at_end(branch_block);

        buffer_ptr = Some(b.build_load(input_ptr_ptr, "").into_pointer_value());
      }
    }

    let mut build_switch = true;
    // Check to see if we need to create a local data buffer.

    let mut blocks = BTreeMap::new();
    let mut skip_block = None;

    for branch in branches.values() {
      if branch.is_skipped {
        blocks.entry((branch.value, true)).or_insert_with(|| {
          (
            INSTRUCTION::invalid(),
            *skip_block.get_or_insert_with(|| p.state.generate_block(module, "skip", 0xFFFF_FFFF)),
          )
        });
      } else {
        blocks.entry((branch.value, false)).or_insert_with(|| {
          (
            INSTRUCTION::from(&p.output.bytecode, branch.address),
            p.state.generate_block(module, "branch", branch.address),
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

          fn string_to_byte_num_and_mask(string: &str, _: &Symbol) -> (usize, usize) {
            string.as_bytes().iter().enumerate().fold((0, 0), |(val, mask), (i, v)| {
              let shift_amount = 8 * i;
              (val | ((*v as usize) << shift_amount), mask | (0xFF << shift_amount))
            })
          }

          for (index, (address, branch, strings)) in branches.iter().enumerate() {
            let sym = data.get_branch_symbol(branch).unwrap();

            for string in strings {
              let comparison = match sym.byte_length {
                len if len == 1 => {
                  let value = b.build_load(buffer_ptr?, "").into_int_value();
                  b.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    value,
                    value
                      .get_type()
                      .const_int(string_to_byte_num_and_mask(string, sym).0 as u64, false),
                    "",
                  )
                }
                len if len <= 8 => {
                  let (byte_string, mask) = string_to_byte_num_and_mask(string, sym);
                  let adjusted_byte = b
                    .build_bitcast(buffer_ptr?, i64.ptr_type(inkwell::AddressSpace::Generic), "")
                    .into_pointer_value();
                  let value = b.build_load(adjusted_byte, "").into_int_value();
                  let masked_value =
                    b.build_and(value, value.get_type().const_int(mask as u64, false), "");
                  b.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    masked_value,
                    value.get_type().const_int(byte_string as u64, false),
                    "",
                  )
                }
                _ => unreachable!(),
              };

              let this_block = p.state.generate_block(module, "this_", **address);

              let next_block = if index == branches.len() - 1 {
                default_block
              } else {
                p.state.generate_block(module, "next_", **address)
              };

              b.build_conditional_branch(comparison, this_block, next_block);
              b.position_at_end(this_block);

              b.build_store(cache_length_ptr, i32.const_int(sym.cp_len as u64, false));

              b.build_unconditional_branch(if branch.is_skipped {
                blocks.get(&(branch.value, true)).unwrap().1
              } else {
                blocks.get(&(branch.value, false)).unwrap().1
              });

              b.position_at_end(next_block);
            }
          }
        } else {
          build_fast_call(module, module.fun.scan, &[
            (parse_ctx).into(),
            parse_fun_ptr(scanner_address, p, module).into(),
            b.build_load(input_ptr_ptr.into(), "").into_pointer_value().into(),
            b.build_load(input_size_ptr.into(), "").into_int_value().into(),
            b.build_load(active_offset, "").into_int_value().into(),
            b.build_load(cache_line_ptr, "").into_int_value().into(),
            b.build_load(cache_line_ptr, "").into_int_value().into(),
          ])?;
          value = CTX::tok_id.load(module, parse_ctx)?.into_int_value();
        }
      }
      INPUT_TYPE::T01_PRODUCTION => {
        value = CTX::prod_id.load(module, parse_ctx)?.into_int_value();
      }
      INPUT_TYPE::T05_BYTE => {
        b.build_store(cache_length_ptr, i32.const_int(1, false));
        value = b.build_load(buffer_ptr?, "").into_int_value();
      }
      INPUT_TYPE::T03_CLASS => {
        let cp_val = construct_cp_lu_with_token_len_store(module, buffer_ptr?, p)?;
        value =
          build_fast_call(module, module.fun.get_token_class_from_codepoint, &[cp_val.into()])?
            .try_as_basic_value()
            .unwrap_left()
            .into_int_value();
      }
      INPUT_TYPE::T04_CODEPOINT => {
        value = construct_cp_lu_with_token_len_store(module, buffer_ptr?, p)?;
      }
      _ => {}
    }

    if build_switch {
      // Create Switch statements.
      let value_type = value.get_type();
      let mut cases = vec![];

      for ((value, _), (_, block)) in &blocks {
        cases.push((value_type.const_int(*value as u64, false), *block));
      }

      b.build_switch(value, default_block, &cases);
    }

    // Write branches, ending with the default branch.

    for ((_, is_skipped), (instruction, block)) in &blocks {
      if !is_skipped {
        b.position_at_end(*block);
        match instruction.to_type() {
          InstructionType::HASH_BRANCH | InstructionType::VECTOR_BRANCH => {
            construct_instruction_branch(*instruction, g, module, p, state.next())?;
          }
          _ => {
            construct_parse_function_statements(*instruction, g, module, p)?;
          }
        }
      }
    }

    if let Some(skip_block) = skip_block {
      b.position_at_end(skip_block);
      unsafe {
        let off = b.build_load(active_offset, "offset").into_int_value();
        let len = b.build_load(cache_length_ptr, "length").into_int_value();
        let new_off = b.build_int_add(off, len, "new_offset");
        b.build_store(active_offset, new_off);
        b.build_store(cache_length_ptr, i32.const_int(0, false));

        // Increment pointer and offset by len;
        let input_ptr = b.build_load(input_ptr_ptr, "").into_pointer_value();
        let input_ptr = b.build_gep(input_ptr, &[len.into()], "");
        b.build_store(input_ptr_ptr, input_ptr);
        b.build_unconditional_branch(table_block);
      }
    }

    let default_instruction = instruction.branch_default(&p.output.bytecode);

    b.position_at_end(default_block);
    match default_instruction.to_type() {
      InstructionType::HASH_BRANCH | InstructionType::VECTOR_BRANCH => {
        construct_instruction_branch(default_instruction, g, module, p, state)?;
      }
      _ => {
        // Build code that deals with the outcomes that arrive when the
        // input block does not have enough bytes to fulfill all branches
        // but the reader could deliver more input.
        if state.input_buffer.is_some() {
          let good_size_block =
            p.state.generate_block(module, "have_sizable_block", default_instruction.get_address());

          let truncated_block =
            p.state.generate_block(module, "block_is_truncated", default_instruction.get_address());

          let comparison = b.build_int_compare(
            inkwell::IntPredicate::EQ,
            state.input_block.as_ref().unwrap().is_truncated,
            module.ctx.bool_type().const_int(1 as u64, false),
            "",
          );

          b.build_conditional_branch(comparison, truncated_block, good_size_block);

          //-- Truncated block
          b.position_at_end(truncated_block);

          build_fast_call(module, module.fun.push_state, &[
            parse_ctx.into(),
            i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
            p.state.function_pointer.into(),
          ])?;

          build_fast_call(module, module.fun.emit_eoi, &[
            parse_ctx.into(),
            action_pointer.into(),
            state.input_block.unwrap().start.into(),
          ])?;

          b.build_return(None);

          //-- Enough bytes present block
          b.position_at_end(good_size_block);
        }

        construct_parse_function_statements(default_instruction, g, module, p)?;
      }
    }
  }

  SherpaResult::Ok(())
}

fn parse_fun_ptr<'a>(
  scanner_address: INSTRUCTION,
  pack: &'a FunctionPack,
  ctx: &'a LLVMParserModule,
) -> PointerValue<'a> {
  get_state_data(scanner_address, pack.states).function_pointer
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
        SymbolID::GenericHorizontalTab => {
          vec!["\t"]
        }
        SymbolID::GenericNewLine => {
          vec!["\n"]
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
  module: &'a LLVMParserModule,
  buffer: PointerValue<'a>,
  pack: &'a FunctionPack,
) -> SherpaResult<IntValue<'a>> {
  let b = &module.builder;
  let cp_info = build_fast_call(module, module.fun.get_utf8_codepoint_info, &[buffer.into()])?
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();

  let cp_val = b.build_extract_value(cp_info, 0, "cp_val")?.into_int_value();
  let cp_byte_len = b.build_extract_value(cp_info, 1, "cp_len")?.into_int_value();

  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();
  let cache_length_ptr = CTX::scan_len.get_ptr(module, parse_ctx)?;

  b.build_store(cache_length_ptr, cp_byte_len);

  SherpaResult::Ok(cp_val)
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

pub(crate) fn construct_instruction_prod(
  instruction: INSTRUCTION,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<INSTRUCTION> {
  let production_id = instruction.get_contents();
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();
  let b = &ctx.builder;
  let production_ptr = b.build_struct_gep(parse_ctx, CTX::prod_id.into(), "")?;
  b.build_store(production_ptr, ctx.ctx.i32_type().const_int(production_id as u64, false));

  SherpaResult::Ok(instruction.next(&pack.output.bytecode))
}

fn construct_token(
  instruction: INSTRUCTION,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<INSTRUCTION> {
  let token_value = instruction.get_contents() & 0x00FF_FFFF;
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();

  let i32 = module.ctx.i32_type();

  CTX::tok_id.store(module, parse_ctx, i32.const_int(token_value as u64, false))?;

  CTX::scan_anchor_off.store(module, parse_ctx, CTX::scan_off.load(module, parse_ctx)?)?;

  SherpaResult::Ok(instruction.next(&pack.output.bytecode))
}

pub(crate) fn construct_instruction_pass(
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();

  module.builder.build_store(
    CTX::state.get_ptr(module, parse_ctx)?,
    module.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false),
  );

  build_tail_call_with_return(module, *pack.fun, module.fun.dispatch)
}

pub(crate) fn construct_fail(module: &LLVMParserModule, pack: &FunctionPack) -> SherpaResult<()> {
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();

  module.builder.build_store(
    CTX::state.get_ptr(module, parse_ctx)?,
    module.ctx.i32_type().const_int(FAIL_STATE_FLAG_LLVM as u64, false),
  );

  build_tail_call_with_return(module, *pack.fun, module.fun.dispatch)
}

pub(crate) fn construct_goto<'a>(
  instruction: INSTRUCTION,
  module: &'a LLVMParserModule,
  pack: &'a FunctionPack,
) -> SherpaResult<(INSTRUCTION, bool)> {
  fn get_goto_state<'a>(last: INSTRUCTION, ctx: &'a LLVMParserModule) -> IntValue<'a> {
    if (last.get_value() & FAIL_STATE_FLAG) > 0 {
      ctx.ctx.i32_type().const_int(FAIL_STATE_FLAG_LLVM as u64, false)
    } else {
      ctx.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false)
    }
  }

  let bytecode = &pack.output.bytecode;
  let mut address = instruction.get_address();
  let b = &module.builder;
  let i32 = module.ctx.i32_type();

  let parse_ctx = pack.fun.get_first_param()?.into_pointer_value();

  let mut gotos = vec![];

  // Extract all gotos from the function.
  while INSTRUCTION::from(bytecode, address).is_GOTO() {
    gotos.push(INSTRUCTION::from(bytecode, address));
    address += 1;
  }

  let last = if matches!(gotos.last()?.next(bytecode).to_type(), InstructionType::PASS) {
    gotos.pop()
  } else {
    None
  };

  if gotos.len() > 0 {
    build_fast_call(module, module.fun.extend_stack_if_needed, &[
      parse_ctx.into(),
      module
        .ctx
        .i32_type()
        .const_int((2 << gotos.len().checked_ilog2().unwrap_or(0)) as u64, false)
        .into(),
    ])?;

    let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(module, parse_ctx)?;
    let mut goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();
    for goto in &gotos {
      // Create new goto struct
      let goto_fn = get_state_data(goto.goto(bytecode), pack.states).function_pointer;
      let goto_state = get_goto_state(*goto, module);
      let new_goto = b.build_insert_value(module.types.goto.get_undef(), goto_state, 1, "")?;
      let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "")?;

      // Store in the current slot
      b.build_store(goto_top, new_goto);

      // Increment the slot
      goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false)], "") };
    }
    // Store the top slot
    b.build_store(goto_stack_ptr, goto_top);

    // Decrement goto remaining
    let goto_free_ptr = CTX::goto_free.get_ptr(module, parse_ctx)?;
    let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
    let goto_free = b.build_int_sub(goto_free, i32.const_int(gotos.len() as u64, false), "");
    b.build_store(goto_free_ptr, goto_free);
  }

  if let Some(last) = last {
    build_tail_call_with_return(
      module,
      *pack.fun,
      get_state_data(last.goto(bytecode), pack.states).function,
    );
    SherpaResult::Ok((last.next(bytecode), true))
  } else {
    SherpaResult::Ok((gotos.last()?.next(bytecode), false))
  }
}

pub(crate) fn construct_reduce(
  instruction: INSTRUCTION,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
  let parse_ctx = pack.fun.get_first_param()?.into_pointer_value();
  let symbol_count = instruction.get_value() >> 16 & 0x0FFF;
  let rule_id = instruction.get_value() & 0xFFFF;

  write_emit_reentrance(instruction.next(&pack.output.bytecode), module, pack)?;

  build_fast_call(module, module.fun.emit_reduce, &[
    parse_ctx.into(),
    pack.fun.get_nth_param(1)?.into_pointer_value().into(),
    CTX::prod_id.load(module, parse_ctx)?.into(),
    module.ctx.i32_type().const_int(rule_id as u64, false).into(),
    module.ctx.i32_type().const_int(symbol_count as u64, false).into(),
  ])?;

  module.builder.build_return(None);

  SherpaResult::Ok(())
}

fn write_emit_reentrance<'a>(
  instruction: INSTRUCTION,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
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
    build_fast_call(module, module.fun.push_state, &[
      pack.fun.get_first_param().unwrap().into_pointer_value().into(),
      module.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
      get_state_data(next_instruction, pack.states).function_pointer.into(),
    ])?;
  }

  SherpaResult::Ok(())
}

pub(crate) fn construct_shift(
  instruction: INSTRUCTION,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();
  let action_ptr = pack.fun.get_nth_param(1)?.into_pointer_value();
  let cache_offset_ptr = CTX::token_off.get_ptr(module, parse_ctx)?;
  let cache_anchor_offset_ptr = CTX::anchor_off.get_ptr(module, parse_ctx)?;
  let cache_line_ptr = CTX::anchor_line_off.get_ptr(module, parse_ctx)?;

  write_emit_reentrance(instruction.next(&pack.output.bytecode), module, pack)?;

  let offset = module.builder.build_load(cache_offset_ptr, "").into_int_value();
  let length = CTX::scan_len.load(module, parse_ctx)?.into_int_value();

  let line = module.builder.build_load(cache_line_ptr, "").into_int_value();

  let b = &module.builder;

  build_fast_call(module, module.fun.emit_shift, &[
    action_ptr.into(),
    b.build_load(cache_anchor_offset_ptr, "").into_int_value().into(),
    offset.into(),
    length.into(),
    line.into(),
  ])?;

  let new_offset = module.builder.build_int_add(length, offset, "");
  CTX::token_off.store(module, parse_ctx, new_offset)?;
  CTX::anchor_off.store(module, parse_ctx, new_offset)?;

  unsafe {
    let input_ptr = CTX::tok_ptr.load(module, parse_ctx)?.into_pointer_value();
    let input_ptr = b.build_gep(input_ptr, &[length.into()], "");
    CTX::tok_ptr.store(module, parse_ctx, input_ptr)?;
  }

  b.build_return(None);

  SherpaResult::Ok(())
}

pub(crate) fn construct_scanner_shift(
  instruction: INSTRUCTION,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<INSTRUCTION> {
  let b = &module.builder;
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();
  let length = CTX::scan_len.load(module, parse_ctx)?.into_int_value();
  let scan_offset_ptr = CTX::scan_off.get_ptr(module, parse_ctx)?;
  let scan_off = b.build_load(scan_offset_ptr, "").into_int_value();

  b.build_store(scan_offset_ptr, b.build_int_add(length, scan_off, ""));

  unsafe {
    let input_ptr = CTX::scan_ptr.load(module, parse_ctx)?.into_pointer_value();
    let input_ptr = b.build_gep(input_ptr, &[length.into()], "");
    CTX::scan_ptr.store(module, parse_ctx, input_ptr)?;
  }

  SherpaResult::Ok(instruction.next(&pack.output.bytecode))
}

/// The prime function's purpose is fist create a base goto state that emits a failure
/// to prevent stack underflow when descending to the bottom of the goto stack, and also
/// insert the first GOTO entry that will initiate the parser to start parsing based on
/// an entry production id.
pub(crate) unsafe fn construct_prime_function(
  module: &LLVMParserModule,
  sp: &Vec<(usize, INSTRUCTION)>,
  states: &BTreeMap<INSTRUCTION, LLVMStateData>,
) -> SherpaResult<()> {
  let i32 = module.ctx.i32_type();
  let bool = module.ctx.bool_type();
  let b = &module.builder;
  let funct = &module.fun;
  let fn_value = funct.prime;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let selector = fn_value.get_nth_param(1).unwrap().into_int_value(); // Set the context's goto pointers to point to the goto block;

  b.position_at_end(module.ctx.append_basic_block(fn_value, "Entry"));
  let blocks = sp
    .iter()
    .map(|(id, instruction)| {
      (
        *id,
        module.ctx.append_basic_block(fn_value, &create_offset_label(instruction.get_address())),
        get_state_data(*instruction, states).function_pointer,
      )
    })
    .collect::<Vec<_>>();

  build_fast_call(module, funct.extend_stack_if_needed, &[
    parse_ctx.into(),
    i32.const_int(8, false).into(),
  ]);

  CTX::is_active.store(module, parse_ctx, bool.const_int(1, false))?;

  // Push the End-Of-Parse goto onto the stack. This will prevent underflow of the stack
  build_push_fn_state(
    module,
    parse_ctx,
    module.fun.emit_eop.as_global_value().as_pointer_value(),
    (NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM),
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
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build prime function"))
  }
}

pub(crate) unsafe fn construct_scan<'a>(module: &LLVMParserModule<'a>) -> SherpaResult<()> {
  // The scan mode reuses the parse context, and pushes a sentinel states
  // that guard against the scanner context consuming the states of the normal
  // context. Though not actually implemented in this function, it worth it
  // to note that the scanner states use the scan_* fields to track progression
  // of a recognized token, and this data is then used to calculate the length
  // of the recognized token when a shift action is emitted.

  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = module;

  // Build a null function that will be used to break out of the scanner
  // context dispatch loop
  let null_fn = {
    let null_fn = module.module.add_function(
      "scan_stop",
      ctx.void_type().fn_type(
        &[
          types.parse_ctx.ptr_type(inkwell::AddressSpace::Generic).into(),
          types.action.ptr_type(inkwell::AddressSpace::Generic).into(),
        ],
        false,
      ),
      Some(Linkage::Private),
    );

    null_fn.set_call_conventions(fastCC);

    let entry = ctx.append_basic_block(null_fn, "Entry");

    b.position_at_end(entry);
    b.build_return(None);

    null_fn
  };

  let fn_value = funct.scan;
  //## Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  //## Extract Params
  let parse_ctx = fn_value.get_nth_param(0)?.into_pointer_value();
  let scanner_parse_function_ptr = fn_value.get_nth_param(1)?.into_pointer_value();
  let input_ptr = fn_value.get_nth_param(2)?.into_pointer_value();
  let input_len = fn_value.get_nth_param(3)?.into_int_value();
  let offset = fn_value.get_nth_param(4)?.into_int_value();
  //let line_offset = fn_value.get_nth_param(5)?.into_int_value();
  //let line_count = fn_value.get_nth_param(6)?.into_int_value();

  //## Entry Block
  b.position_at_end(entry);

  // Push the scanner sentinel state and the entry scanner state onto
  // the goto stack.

  CTX::scan_ptr.store(module, parse_ctx, input_ptr)?;
  CTX::scan_input_len.store(module, parse_ctx, input_len)?;
  CTX::scan_input_trun.store(module, parse_ctx, ctx.bool_type().const_zero())?;
  CTX::scan_off.store(module, parse_ctx, offset)?;
  CTX::scan_anchor_off.store(module, parse_ctx, offset)?;

  build_push_fn_state(
    module,
    parse_ctx,
    null_fn.as_global_value().as_pointer_value(),
    (NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM),
  );

  build_push_fn_state(module, parse_ctx, scanner_parse_function_ptr, NORMAL_STATE_FLAG_LLVM);

  // Reserve enough space on the stack for an Action enum
  let action = b.build_alloca(types.action, "");

  // Dispatch!
  b.build_call(funct.dispatch, &[parse_ctx.into(), action.into()], "");

  // Convert scan_anchor_off and tok_off / peek_off into a length offset

  CTX::scan_len.store(
    module,
    parse_ctx,
    b.build_int_sub(
      CTX::scan_anchor_off.load(module, parse_ctx)?.into_int_value(),
      offset.into(),
      "",
    ),
  );

  // Now the scan_len contains the length of a recognized token
  // and tok_prod_id contains the id of that recognized token. The calling state
  // can handle these values from here, so simply return.

  b.build_return(None);

  if funct.scan.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build scan function"))
  }
}

pub(crate) unsafe fn construct_dispatch_function<'a>(
  module: &'a LLVMParserModule,
) -> SherpaResult<()> {
  // Insert next instruction near top
  let c = module;
  let LLVMParserModule { builder: b, ctx, fun, .. } = module;

  let fn_value = fun.dispatch;
  let entry_block = ctx.append_basic_block(fn_value, "Entry");

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();

  let i32 = ctx.i32_type();
  let zero = i32.const_int(0, false);

  // Set the context's goto pointers to point to the goto block;
  let block_dispatch = ctx.append_basic_block(fn_value, "Dispatch");
  let block_useful_state = ctx.append_basic_block(fn_value, "ModeAppropriateState");

  b.position_at_end(entry_block);
  b.build_unconditional_branch(block_dispatch);

  b.position_at_end(block_dispatch);

  let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(c, parse_ctx)?;
  let goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();
  let goto_top = b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "");
  b.build_store(goto_stack_ptr, goto_top);

  let goto_free_ptr = CTX::goto_free.get_ptr(c, parse_ctx)?;
  let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
  let goto_free = b.build_int_add(goto_free, i32.const_int(1, false), "");

  b.build_store(goto_free_ptr, goto_free);

  let goto = b.build_load(goto_top, "").into_struct_value();

  let goto_state = b.build_extract_value(goto, 1, "")?;

  let masked_state = b.build_and(
    CTX::state.load(module, parse_ctx)?.into_int_value(),
    goto_state.into_int_value(),
    "",
  );
  let condition = b.build_int_compare(inkwell::IntPredicate::NE, masked_state, zero, "");
  b.build_conditional_branch(condition, block_useful_state, block_dispatch);

  b.position_at_end(block_useful_state);

  let parse_function =
    CallableValue::try_from(b.build_extract_value(goto, 0, "").unwrap().into_pointer_value())
      .unwrap();

  build_tail_call_with_return(module, fun.dispatch, parse_function);

  if fun.dispatch.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build dispatch function"))
  }
}

pub(crate) unsafe fn construct_next_function<'a>(module: &'a LLVMParserModule) -> SherpaResult<()> {
  // Insert next instruction near top
  let c = module;
  let LLVMParserModule { builder: b, ctx, fun, .. } = module;

  let fn_value = fun.next;

  b.position_at_end(ctx.append_basic_block(fn_value, "Entry"));

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  let action = fn_value.get_nth_param(1).unwrap().into_pointer_value();

  let call_site = b.build_call(fun.dispatch, &[parse_ctx.into(), action.into()], "");
  call_site.set_tail_call(false);
  call_site.set_call_convention(fastCC);

  b.build_return(None);

  if fun.next.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build next function"))
  }
}

pub fn build_tail_call_with_return<'a, T>(
  module: &'a LLVMParserModule,
  caller_fun: FunctionValue<'a>,
  callee_fun: T,
) -> SherpaResult<()>
where
  T: Into<CallableValue<'a>>,
{
  let call_site = module.builder.build_call(
    callee_fun,
    &[
      caller_fun.get_nth_param(0)?.into_pointer_value().into(),
      caller_fun.get_nth_param(1)?.into_pointer_value().into(),
    ],
    "TAIL_CALL_SITE",
  );
  call_site.set_tail_call(true);
  call_site.set_call_convention(fastCC);

  module.builder.build_return(None);

  SherpaResult::Ok(())
}

fn build_push_fn_state<'a>(
  module: &'a LLVMParserModule,
  parse_ctx: PointerValue<'a>,
  function_ptr: PointerValue<'a>,
  state_mask: u32,
) -> SherpaResult<()> {
  module.builder.build_call(
    module.fun.push_state,
    &[
      parse_ctx.into(),
      module.ctx.i32_type().const_int(state_mask as u64, false).into(),
      function_ptr.into(),
    ],
    "",
  );

  SherpaResult::Ok(())
}

pub(crate) fn check_for_input_acceptability<'a>(
  module: &'a LLVMParserModule,
  p: &'a FunctionPack,
  input_ptr_ptr: PointerValue<'a>,
  input_len_ptr: PointerValue<'a>,
  input_truncated_ptr: PointerValue<'a>,
  input_off: IntValue<'a>,
  needed_bytes: IntValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = module;

  let parse_ctx = p.fun.get_nth_param(0).unwrap().into_pointer_value();
  let state = p.state;

  let bool = ctx.bool_type();

  // Set the context's goto pointers to point to the goto block;
  let attempt_extend = state.generate_block(module, "Attempt_Extend", 0);
  let truncation_check = state.generate_block(module, "Check_Truncation", 0);
  let valid_window = state.generate_block(module, "Valid_Window", 0);

  let input_len = b.build_load(input_len_ptr, "len").into_int_value();
  let requested_end_off = b.build_int_add(input_off, needed_bytes, "end_offset");

  // if the requested_end_cursor_position is > the blocks end position, then try
  // requesting more input buffer.

  let comparison =
    b.build_int_compare(inkwell::IntPredicate::UGE, input_len, requested_end_off, "");
  b.build_conditional_branch(comparison, valid_window, truncation_check);

  b.position_at_end(truncation_check);

  let truncated = b.build_load(input_truncated_ptr, "len").into_int_value();
  let comparison =
    b.build_int_compare(inkwell::IntPredicate::EQ, truncated, bool.const_int(1, false), "");

  b.build_conditional_branch(comparison, attempt_extend, valid_window);
  b.position_at_end(attempt_extend);

  build_fast_call(module, funct.get_adjusted_input_block, &[
    parse_ctx.into(),
    input_ptr_ptr.into(),
    input_len_ptr.into(),
    input_truncated_ptr.into(),
    input_off.into(),
    requested_end_off.into(),
  ]);

  b.build_unconditional_branch(valid_window);
  b.position_at_end(valid_window);

  SherpaResult::Ok(())
}
