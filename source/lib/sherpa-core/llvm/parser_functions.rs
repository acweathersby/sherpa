use super::{build_fast_call, create_offset_label, fastCC, CTX_AGGREGATE_INDICES as CTX};
use crate::{
  build::table::{BranchData, BranchTableData},
  compile::BytecodeOutput,
  debug::{address_string, disassemble_state, BytecodeGrammarLookups},
  llvm::{LLVMParserModule, FAIL_STATE_FLAG_LLVM},
  types::*,
  Journal,
};
use inkwell::{
  basic_block::BasicBlock,
  module::Linkage,
  values::{CallableValue, FunctionValue, IntValue, PointerValue},
};
use std::{
  collections::{BTreeMap, BTreeSet, VecDeque},
  sync::Arc,
};

pub(crate) struct FunctionPack<'a> {
  pub(crate) fun:        &'a FunctionValue<'a>,
  pub(crate) output:     &'a BytecodeOutput,
  pub(crate) is_scanner: bool,
  pub(crate) state:      &'a LLVMStateData<'a>,
  pub(crate) states:     &'a BTreeMap<Instruction, LLVMStateData<'a>>,
}

pub const _FAIL_STATE_FLAG_LLVM: u32 = 2;
pub const NORMAL_STATE_FLAG_LLVM: u32 = 1;

#[derive(Clone, Debug)]
pub struct LLVMStateData<'a> {
  pub(crate) entry_instruction: Instruction,
  pub(crate) function_pointer:  PointerValue<'a>,
  pub(crate) function:          FunctionValue<'a>,
  pub(crate) is_scanner:        bool,
  pub(crate) branch_data:       BTreeMap<Instruction, BranchTableData>,
  pub(crate) entry_block:       BasicBlock<'a>,
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
    internal_linkage: Option<Linkage>,
  ) -> Self {
    let entry_instruction = Instruction::from(bc, bc_address);

    let entry_function = module.module.add_function(
      &Self::generate_name(bc_address),
      module.types.TAIL_CALLABLE_PARSE_FUNCTION,
      internal_linkage,
    );

    entry_function.set_call_conventions(fastCC);

    let entry_block = module.ctx.append_basic_block(entry_function, "Entry");

    Self {
      function: entry_function,
      function_pointer: entry_function.as_global_value().as_pointer_value(),
      entry_instruction,
      is_scanner,
      branch_data: BTreeMap::new(),
      entry_block,
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

pub(crate) fn get_state_data<'a>(
  instruction: Instruction,
  state: &'a BTreeMap<Instruction, LLVMStateData<'a>>,
) -> &LLVMStateData<'a> {
  if !state.contains_key(&instruction) {
    dbg!(instruction);
  }
  state.get(&instruction).unwrap()
}

pub(crate) unsafe fn construct_parse_function<'a>(
  j: &mut Journal,
  module: &'a LLVMParserModule,
  output: &BytecodeOutput,
) -> SherpaResult<()> {
  let grammar = j.grammar()?;
  let g = &grammar;

  let start_points = get_start_points(g, output);

  let linkage = if j.config().opt_llvm { Some(Linkage::Private) } else { None };

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
        let mut state = LLVMStateData::new(
          &output.bytecode,
          instruction.get_address(),
          module,
          is_scanner,
          linkage,
        );
        let bc = &output.bytecode;

        let mut instructions = VecDeque::from_iter(vec![(instruction)]);

        while let Some(mut instruction) = instructions.pop_back() {
          while instruction.is_valid() {
            use InstructionType::*;
            match instruction.to_type() {
              SetProd | Token | ForkTo | EatCrumbs | Noop13 | Scan | Repeat | AssertShift
              | SetFailState => {
                instruction = instruction.next(bc);
              }
              Goto => {
                states_queue.push_front((instruction.goto(bc), is_scanner));
                instruction = instruction.next(bc);
              }
              Shift => {
                if !is_scanner {
                  states_queue.push_front((instruction.next(bc), is_scanner)); // Synthetic state
                  break;
                } else {
                  instruction = instruction.next(bc);
                }
              }
              Reduce => {
                states_queue.push_front((instruction.next(bc), is_scanner)); // Synthetic state
                break;
              }
              VectorBranch | HashBranch => {
                let data = BranchTableData::from_bytecode(instruction, g, output)?;

                if data.data.input_type == InputType::T02_TOKEN && !data.has_trivial_comparisons() {
                  states_queue.push_front((data.data.scan_state_entry_instruction, true));
                }

                for (_, BranchData { address, .. }) in
                  data.branches.iter().filter(|b| !b.1.is_skipped)
                {
                  instructions.push_back(Instruction::from(bc, *address));
                }

                instructions.push_back(instruction.branch_default(bc));
                state.branch_data.insert(instruction, data);
                break;
              }
              Fail | Pass => {
                break;
              }
            }
          }
        }

        states.insert(state.entry_instruction, state);
      }
    }
    states
  };

  for instruction in states.keys().copied() {
    let state = { states.get(&instruction)? };

    let fun = state.function;

    module.builder.position_at_end(state.entry_block);

    let fn_pack = FunctionPack {
      fun: &fun,
      output,
      is_scanner: state.is_scanner,
      states: &states,
      state: state,
    };

    construct_parse_function_statements(instruction, g, module, &fn_pack)?;

    if !fun.verify(true) {
      fun.print_to_stderr();
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

fn get_start_points(g: &GrammarStore, output: &BytecodeOutput) -> Vec<(usize, Instruction)> {
  let start_points = g
    .get_exported_productions()
    .iter()
    .enumerate()
    .map(|(i, p)| {
      let instruction = Instruction::from(
        &output.bytecode,
        *(output.state_name_to_offset.get(p.guid_name).unwrap()) as usize,
      );
      (i, instruction)
    })
    .collect::<Vec<_>>();
  start_points
}

pub(super) fn construct_parse_function_statements(
  mut instruction: Instruction,
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
        Shift => {
          if *is_scanner {
            instruction = construct_scanner_shift(instruction, module, pack)?;
          } else {
            construct_shift(instruction, module, pack);
            break;
          }
        }
        Goto => {
          let branched;
          (instruction, branched) = construct_goto(instruction, module, pack)?;

          if branched {
            break;
          }
        }
        SetProd => {
          instruction = construct_instruction_prod(instruction, module, pack)?;
        }
        Reduce => {
          construct_reduce(instruction, module, pack)?;
          break;
        }
        Token => {
          instruction = construct_token(instruction, module, pack)?;
        }
        ForkTo => {
          // TODO
          break;
        }
        EatCrumbs | Noop13 | Scan | Repeat | AssertShift | SetFailState => {
          instruction = instruction.next(&output.bytecode);
        }
        VectorBranch | HashBranch => {
          construct_instruction_branch(
            instruction,
            g,
            module,
            pack,
            true,
            None,
            pack.state.entry_block,
          )?;
          break;
        }
        Fail => {
          construct_fail(module, pack)?;
          break;
        }
        Pass => {
          construct_pass(module, pack)?;
          break;
        }
      }
    }

    SherpaResult::Ok(())
  }
}

#[derive(Clone, Copy)]
struct Pointers<'a> {
  line_ptr:           PointerValue<'a>,
  input_ptr_ptr:      PointerValue<'a>,
  input_byte_len_ptr: PointerValue<'a>,
  input_off_ptr:      PointerValue<'a>,
  input_trun:         PointerValue<'a>,
  token_len_ptr:      PointerValue<'a>,
  entry_table_block:  BasicBlock<'a>,
}

fn construct_instruction_branch<'a>(
  instruction: Instruction,
  g: &GrammarStore,
  m: &'a LLVMParserModule,
  p: &'a FunctionPack,
  entry_table: bool,
  mut ptrs: Option<Pointers<'a>>,
  mut last_defult_block: BasicBlock<'a>,
) -> SherpaResult<()> {
  let b = &m.builder;
  let i32 = m.ctx.i32_type();
  let i64 = m.ctx.i64_type();
  let bool = m.ctx.bool_type();
  let p_ctx = p.fun.get_nth_param(0)?.into_pointer_value();

  let table_block = p.state.generate_block(m, "_Table", instruction.get_address());
  let default_block = p.state.generate_block(m, "_Table_Default", instruction.get_address());

  // Construct a buffer, if necessary, that will hold up to `max_size` bytes.
  let mut value = i32.const_int(0, false);

  let Some(data) = p.state.branch_data.get(&instruction) else {
    let lu = BytecodeGrammarLookups::new(Arc::new(g.clone()));

    panic!("No table data found for state: \ninstruction:\n{:?} \nbytecode:\n{} \nrecognized_branches:\n{}",
    instruction,
    disassemble_state(&p.output.bytecode, instruction.get_address(), Some(&lu)).0,
    p.state.branch_data.iter().map(|(i,_)| {
      disassemble_state(&p.output.bytecode, i.get_address(), Some(&lu)).0
    }).collect::<Vec<_>>().join("\n\n")
  );
  };

  let representative_state = data;
  let lexer_type = representative_state.data.lexer_type;
  let input_type = representative_state.data.input_type;
  let is_peek = matches!(lexer_type, LexerType::PEEK);
  let is_scanner = p.is_scanner;

  #[cfg(debug_assertions)]
  {
    // Some assertion to establish the invariants of branching states.

    // ------------------------------------------------------------------------
    // branching states that have more than one table are comprised of a combination
    // BYTE | CLASS | CODEPOINT tables only. That is, the state is a scanner state.
    assert!(
      p.state.branch_data.len() == 1 || p.is_scanner,
      "None scanner branch states should only contain one table."
    );
    assert!(
      !p.is_scanner
        || !p
          .state
          .branch_data
          .iter()
          .any(|t| matches!(t.1.data.input_type, InputType::T01_PRODUCTION | InputType::T02_TOKEN)),
      "Scanner states should not contain PRODUCTION or TOKEN tables"
    );
    // This also leads to the conclusion that PRODUCTION and TOKEN states have
    // only one table.

    // ------------------------------------------------------------------------
    // Peek states only exist outside of scanner states
    assert!(
      data.data.lexer_type != LexerType::PEEK || !p.is_scanner,
      "Scanner states should not contain peek tables"
    );
  }

  if entry_table {
    last_defult_block = p.state.generate_block(m, "last_default", instruction.get_address());

    let max_length = {
      let mut max_size = 1;
      for table in p.state.branch_data.values() {
        // Retrieve the maximum number of bytes that will be read
        // per round within this function.
        let data = table.data;
        max_size = max_size.max(match data.input_type {
          InputType::T02_TOKEN => {
            if table.has_trivial_comparisons() {
              getBranchTokenData(g, table)
                .iter()
                .flat_map(|(_, _, s)| s)
                .fold(0, |a, s| usize::max(a, s.len()))
            } else {
              0
            }
          }
          InputType::T05_BYTE => 1,
          InputType::T03_CLASS | InputType::T04_CODEPOINT => 4,
          _ => 0,
        });
      }
      max_size
    };

    if lexer_type > 0 {
      let line_ptr = CTX::end_line_off.get_ptr(m, p_ctx)?;
      let token_len_ptr = CTX::scan_len.get_ptr(m, p_ctx)?;
      let (input_ptr_ptr, input_byte_len_ptr, input_truncated_ptr, input_off_ptr) =
        match (is_peek, is_scanner) {
          (true, _) => (
            CTX::peek_ptr.get_ptr(m, p_ctx)?,
            CTX::peek_input_len.get_ptr(m, p_ctx)?,
            CTX::peek_input_trun.get_ptr(m, p_ctx)?,
            CTX::peek_off.get_ptr(m, p_ctx)?,
          ),
          (_, true) => (
            CTX::scan_ptr.get_ptr(m, p_ctx)?,
            CTX::scan_input_len.get_ptr(m, p_ctx)?,
            CTX::scan_input_trun.get_ptr(m, p_ctx)?,
            CTX::scan_off.get_ptr(m, p_ctx)?,
          ),
          _ => (
            CTX::tok_ptr.get_ptr(m, p_ctx)?,
            CTX::tok_input_len.get_ptr(m, p_ctx)?,
            CTX::tok_input_trun.get_ptr(m, p_ctx)?,
            CTX::token_off.get_ptr(m, p_ctx)?,
          ),
        };

      let peek_mode_ptr = CTX::in_peek_mode.get_ptr(m, p_ctx)?;
      if lexer_type == LexerType::ASSERT {
        b.build_store(peek_mode_ptr, bool.const_int(0, false));
        b.build_unconditional_branch(table_block);
      } else {
        // Need to increment the peek token by either the previous length of the peek
        // token, or the current length of the assert token.
        let (continue_peeking, start_peeking) = (
          p.state.generate_block(m, "continue_peeking", instruction.get_address()),
          p.state.generate_block(m, "start_peeking", instruction.get_address()),
        );

        let peek_mode = b.build_load(peek_mode_ptr, "").into_int_value();
        let comparison =
          b.build_int_compare(inkwell::IntPredicate::EQ, peek_mode, bool.const_int(1, false), "");

        b.build_conditional_branch(comparison, continue_peeking, start_peeking);

        b.position_at_end(continue_peeking);

        {
          let prev_token_len = b.build_load(token_len_ptr, "").into_int_value();
          let prev_token_off = b.build_load(input_off_ptr, "").into_int_value();
          let new_off = b.build_int_add(prev_token_len, prev_token_off, "");

          unsafe {
            let input_ptr = b.build_load(input_ptr_ptr, "").into_pointer_value();
            let input_ptr = b.build_gep(input_ptr, &[prev_token_len.into()], "");
            b.build_store(input_ptr_ptr, input_ptr);
          }

          b.build_store(input_off_ptr, new_off);
          b.build_unconditional_branch(table_block);
        }

        b.position_at_end(start_peeking);
        {
          let prev_token_len = b.build_load(token_len_ptr, "").into_int_value();
          let prev_token_off = CTX::token_off.load(m, p_ctx)?.into_int_value();
          let new_off = b.build_int_add(prev_token_len, prev_token_off, "");

          unsafe {
            let input_ptr = CTX::tok_ptr.load(m, p_ctx)?.into_pointer_value();
            let input_ptr = b.build_gep(input_ptr, &[prev_token_len.into()], "");
            b.build_store(input_ptr_ptr, input_ptr);
          }
          b.build_store(input_off_ptr, new_off);
          b.build_unconditional_branch(table_block);
        }
      };

      b.position_at_end(table_block);

      ptrs = Some(Pointers {
        token_len_ptr,
        input_off_ptr,
        input_ptr_ptr,
        input_trun: input_truncated_ptr,
        line_ptr,
        input_byte_len_ptr,
        entry_table_block: table_block,
      });

      if input_type != InputType::T01_PRODUCTION {
        // Only initialize input buffer if we are directly comparing it's data with values.
        let byte_offset = b.build_load(input_off_ptr, "byte_offset").into_int_value();

        let input_available = check_for_input_acceptability(
          m,
          p,
          input_ptr_ptr,
          input_byte_len_ptr,
          input_truncated_ptr,
          byte_offset,
          i32.const_int(max_length as u64, false),
        )?;

        // If the block size is less than 1 then jump to default
        let comparison = b.build_int_compare(
          inkwell::IntPredicate::SGE,
          input_available,
          i32.const_int(1, false),
          "",
        );

        let branch_block = p.state.generate_block(m, "table_branches", instruction.get_address());

        b.build_conditional_branch(comparison, branch_block, last_defult_block);
        b.position_at_end(branch_block);
      }
    } else {
      b.build_unconditional_branch(table_block);
      b.position_at_end(table_block);
    };
  } else {
    b.build_unconditional_branch(table_block);
    b.position_at_end(table_block);
  }

  let TableHeaderData { input_type, scan_state_entry_instruction: scanner_address, .. } = data.data;

  let trivial_token_comparisons =
    input_type == InputType::T02_TOKEN && data.has_trivial_comparisons();

  // Prepare the input token if we are working with
  // Token based branch types (TOKEN, BYTE, CODEPOINT, CLASS)

  let mut build_switch = true;
  // Check to see if we need to create a local data buffer.

  let mut blocks = BTreeMap::new();
  let mut skip_block = None;
  let mut branch_blocks = BTreeMap::new();
  let branches = &data.branches;

  // Prepare blocks for each possible value type
  for branch in branches.values() {
    let (instruction, skipped) = if branch.is_skipped {
      skip_block.get_or_insert_with(|| p.state.generate_block(m, "skip", 0xFFFF_FFFF));
      (Instruction::invalid(), true)
    } else {
      (Instruction::from(&p.output.bytecode, branch.address), false)
    };

    blocks.entry((branch.value, skipped)).or_insert_with(|| {
      (
        instruction,
        *branch_blocks
          .entry((branch.address, data.get_line_data(branch), skipped))
          .or_insert_with(|| p.state.generate_block(m, "branch", branch.address)),
      )
    });
  }

  // Add check for end of input.

  match input_type {
    InputType::T01_PRODUCTION => {
      value = CTX::prod_id.load(m, p_ctx)?.into_int_value();
    }
    //*
    InputType::T05_BYTE => {
      let buffer_ptr = b.build_load(ptrs?.input_ptr_ptr, "").into_pointer_value();
      b.build_store(ptrs?.token_len_ptr, i32.const_int(1, false));
      value = b.build_load(buffer_ptr, "").into_int_value();
    }
    InputType::T03_CLASS => {
      let buffer_ptr = b.build_load(ptrs?.input_ptr_ptr, "").into_pointer_value();
      let cp_val = construct_cp_lu_with_token_len_store(m, buffer_ptr, p)?;
      value = build_fast_call(m, m.fun.get_token_class_from_codepoint, &[cp_val.into()])?
        .try_as_basic_value()
        .unwrap_left()
        .into_int_value();
    }
    InputType::T04_CODEPOINT => {
      let buffer_ptr = b.build_load(ptrs?.input_ptr_ptr, "").into_pointer_value();
      value = construct_cp_lu_with_token_len_store(m, buffer_ptr, p)?;
    } //*/
    InputType::T02_TOKEN => {
      if trivial_token_comparisons {
        build_switch = false;

        let branches = getBranchTokenData(g, &data);

        let buffer_ptr = b.build_load(ptrs?.input_ptr_ptr, "").into_pointer_value();

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
                let value = b.build_load(buffer_ptr, "").into_int_value();
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
                let adjusted_byte =
                  b.build_bitcast(buffer_ptr, i64.ptr_type(0.into()), "").into_pointer_value();
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

            let this_block = p.state.generate_block(m, "this_", **address);

            let next_block = if index == branches.len() - 1 {
              default_block
            } else {
              p.state.generate_block(m, "next_", **address)
            };

            b.build_conditional_branch(comparison, this_block, next_block);
            b.position_at_end(this_block);

            b.build_store(ptrs?.token_len_ptr, i32.const_int(sym.cp_len as u64, false));

            b.build_unconditional_branch(if branch.is_skipped {
              blocks.get(&(branch.value, true)).unwrap().1
            } else {
              blocks.get(&(branch.value, false)).unwrap().1
            });

            b.position_at_end(next_block);
          }
        }
      } else {
        build_fast_call(m, m.fun.scan, &[
          (p_ctx).into(),
          parse_fun_ptr(scanner_address, p).into(),
          b.build_load(ptrs?.input_ptr_ptr.into(), "").into_pointer_value().into(),
          ptrs?.input_byte_len_ptr.into(),
          ptrs?.input_trun.into(),
          b.build_load(ptrs?.input_off_ptr.into(), "").into_int_value().into(),
        ])?;
        value = CTX::tok_id.load(m, p_ctx)?.into_int_value();
      }
    }
    _ => value = i32.const_zero(),
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

  for ((address, line_data, skipped), block) in branch_blocks {
    b.position_at_end(block);

    if line_data.num_of_lines > 0 && (trivial_token_comparisons || is_scanner) {
      CTX::scan_line_num.store(
        m,
        p_ctx,
        b.build_int_add(
          CTX::scan_line_num.load(m, p_ctx)?.into_int_value(),
          i32.const_int(line_data.num_of_lines as u64, false),
          "",
        ),
      )?;
      CTX::scan_line_off.store(
        m,
        p_ctx,
        b.build_int_add(
          b.build_load(ptrs?.input_off_ptr, "").into_int_value(),
          i32.const_int(line_data.last_offset as u64, false),
          "",
        ),
      )?;
    }

    if skipped {
      b.build_unconditional_branch(skip_block.unwrap());
    } else {
      let instruction = Instruction::from(&p.output.bytecode, address);

      if !is_peek {
        CTX::end_line_num.store(m, p_ctx, CTX::scan_line_num.load(m, p_ctx)?.into_int_value())?;
        CTX::end_line_off.store(m, p_ctx, CTX::scan_line_off.load(m, p_ctx)?.into_int_value())?;
      }

      match instruction.to_type() {
        InstructionType::HashBranch | InstructionType::VectorBranch => {
          construct_instruction_branch(instruction, g, m, p, false, ptrs, last_defult_block)?;
        }
        _ => {
          construct_parse_function_statements(instruction, g, m, p)?;
        }
      }
    }
  }

  if let Some(skip_block) = skip_block {
    b.position_at_end(skip_block);
    let len = b.build_load(ptrs?.token_len_ptr, "length").into_int_value();

    // Increment pointer and offset by len;
    increment_input_offset_and_ptr(b, ptrs?.input_ptr_ptr, ptrs?.input_off_ptr, len);

    if !is_scanner && !is_peek {
      // Move scan line data into token line data
      CTX::start_line_num.store(m, p_ctx, CTX::scan_line_num.load(m, p_ctx)?.into_int_value())?;
      CTX::start_line_off.store(m, p_ctx, CTX::scan_line_off.load(m, p_ctx)?.into_int_value())?;
      CTX::end_line_num.store(m, p_ctx, CTX::scan_line_num.load(m, p_ctx)?.into_int_value())?;
      CTX::end_line_off.store(m, p_ctx, CTX::scan_line_off.load(m, p_ctx)?.into_int_value())?;
    }

    b.build_store(ptrs?.token_len_ptr, i32.const_int(0, false));
    b.build_unconditional_branch(ptrs?.entry_table_block);
  }

  b.position_at_end(default_block);

  let default_instruction = instruction.branch_default(&p.output.bytecode);

  match default_instruction.to_type() {
    InstructionType::HashBranch | InstructionType::VectorBranch => {
      construct_instruction_branch(default_instruction, g, m, p, false, ptrs, last_defult_block)?;
    }
    _ => {
      b.build_unconditional_branch(last_defult_block);
      b.position_at_end(last_defult_block);
      // Build the input load structures

      // Build code that deals with the outcomes that arrive when the
      // input block does not have enough bytes to fulfill all branches
      // but the reader could deliver more input.
      if entry_table {
        // TODO: Handle case where there is not enough input to handle all matches.
      }
      construct_parse_function_statements(default_instruction, g, m, p)?;
    }
  }

  SherpaResult::Ok(())
}

fn parse_fun_ptr<'a>(scanner_address: Instruction, pack: &'a FunctionPack) -> PointerValue<'a> {
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

pub fn increment_input_offset_and_ptr<'a>(
  b: &inkwell::builder::Builder,
  input_ptr_ptr: PointerValue<'a>,
  off_ptr: PointerValue<'a>,
  len: IntValue<'a>,
) {
  let off = b.build_load(off_ptr, "offset").into_int_value();
  let new_off = b.build_int_add(off, len, "new_offset");
  b.build_store(off_ptr, new_off);
  unsafe {
    let input_ptr = b.build_load(input_ptr_ptr, "").into_pointer_value();
    let input_ptr = b.build_gep(input_ptr, &[len.into()], "");
    b.build_store(input_ptr_ptr, input_ptr);
  }
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

pub(crate) fn construct_instruction_prod(
  instruction: Instruction,
  ctx: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<Instruction> {
  let production_id = instruction.get_contents();
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();
  let b = &ctx.builder;
  let production_ptr = b.build_struct_gep(parse_ctx, CTX::prod_id.into(), "")?;
  b.build_store(production_ptr, ctx.ctx.i32_type().const_int(production_id as u64, false));

  SherpaResult::Ok(instruction.next(&pack.output.bytecode))
}

fn construct_token(
  instruction: Instruction,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<Instruction> {
  let token_value = instruction.get_token_value();
  let parse_ctx = pack.fun.get_nth_param(0)?.into_pointer_value();

  let i32 = module.ctx.i32_type();

  CTX::tok_id.store(module, parse_ctx, i32.const_int(token_value as u64, false))?;
  CTX::scan_anchor_off.store(module, parse_ctx, CTX::scan_off.load(module, parse_ctx)?)?;

  SherpaResult::Ok(instruction.next(&pack.output.bytecode))
}

pub(crate) fn construct_pass(module: &LLVMParserModule, pack: &FunctionPack) -> SherpaResult<()> {
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
  instruction: Instruction,
  module: &'a LLVMParserModule,
  pack: &'a FunctionPack,
) -> SherpaResult<(Instruction, bool)> {
  fn get_goto_state<'a>(last: Instruction, ctx: &'a LLVMParserModule) -> IntValue<'a> {
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
  while Instruction::from(bytecode, address).is_goto() {
    gotos.push(Instruction::from(bytecode, address));
    address += 1;
  }

  let last = if matches!(gotos.last()?.next(bytecode).to_type(), InstructionType::Pass) {
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

fn write_reentrance<'a>(
  instruction: Instruction,
  module: &LLVMParserModule,
  pack: &FunctionPack,
  force_goto: bool,
) -> SherpaResult<()> {
  let bytecode = &pack.output.bytecode;

  use InstructionType::*;

  let next_instruction = match instruction.to_type() {
    Pass => Instruction::pass(),
    _ => instruction,
  };

  if !next_instruction.is_pass() || force_goto {
    build_fast_call(module, module.fun.push_state, &[
      pack.fun.get_first_param().unwrap().into_pointer_value().into(),
      module.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
      get_state_data(next_instruction, pack.states).function_pointer.into(),
    ])?;
  }

  SherpaResult::Ok(())
}

pub(crate) fn construct_reduce(
  instruction: Instruction,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
  let parse_ctx = pack.fun.get_first_param()?.into_pointer_value();
  let sym_count = instruction.get_value() >> 16 & 0x0FFF;
  let rule_id = instruction.get_value() & 0xFFFF;

  write_reentrance(instruction.next(&pack.output.bytecode), module, pack, false)?;

  CTX::meta_a.store(module, parse_ctx, module.ctx.i32_type().const_int(sym_count as u64, false))?;

  CTX::meta_b.store(module, parse_ctx, module.ctx.i32_type().const_int(rule_id as u64, false))?;

  module
    .builder
    .build_return(Some(&module.ctx.i32_type().const_int(ParseActionType::Reduce.into(), false)));

  SherpaResult::Ok(())
}

pub(crate) fn construct_shift(
  instruction: Instruction,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<()> {
  let next = instruction.next(&pack.output.bytecode);
  write_reentrance(next, module, pack, true)?;

  // Output the shift parse action. The following lines are run after
  // the user reinter's the parser's ongoing parser context.

  module
    .builder
    .build_return(Some(&module.ctx.i32_type().const_int(ParseActionType::Shift.into(), false)));

  let data = get_state_data(next, pack.states);

  let fun = data.function;
  let b = &module.builder;

  match data.entry_block.get_first_instruction() {
    Some(instruction) => b.position_before(&instruction),
    None => b.position_at_end(data.entry_block),
  }

  let parse_ctx = fun.get_nth_param(0)?.into_pointer_value();
  let cache_offset_ptr = CTX::token_off.get_ptr(module, parse_ctx)?;

  let offset = module.builder.build_load(cache_offset_ptr, "").into_int_value();
  let length = CTX::scan_len.load(module, parse_ctx)?.into_int_value();
  let new_offset = module.builder.build_int_add(length, offset, "");
  CTX::token_off.store(module, parse_ctx, new_offset)?;
  CTX::anchor_off.store(module, parse_ctx, new_offset)?;

  CTX::start_line_num.store(
    module,
    parse_ctx,
    CTX::end_line_num.load(module, parse_ctx)?.into_int_value(),
  )?;

  CTX::start_line_off.store(
    module,
    parse_ctx,
    CTX::end_line_off.load(module, parse_ctx)?.into_int_value(),
  )?;

  unsafe {
    let input_ptr = CTX::tok_ptr.load(module, parse_ctx)?.into_pointer_value();
    let input_ptr = b.build_gep(input_ptr, &[length.into()], "");
    CTX::tok_ptr.store(module, parse_ctx, input_ptr)?;
  }

  SherpaResult::Ok(())
}

pub(crate) fn construct_scanner_shift(
  instruction: Instruction,
  module: &LLVMParserModule,
  pack: &FunctionPack,
) -> SherpaResult<Instruction> {
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
  sp: &Vec<(usize, Instruction)>,
  states: &BTreeMap<Instruction, LLVMStateData>,
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
    module.fun.handle_eop.as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM,
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
    &[caller_fun.get_nth_param(0)?.into_pointer_value().into()],
    "TAIL_CALL_SITE",
  );
  call_site.set_tail_call(true);
  call_site.set_call_convention(fastCC);

  let value = call_site.try_as_basic_value().left()?.into_int_value();

  module.builder.build_return(Some(&value));

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
  needed_num_bytes: IntValue<'a>,
) -> SherpaResult<IntValue<'a>> {
  let LLVMParserModule { builder: b, ctx, fun: funct, .. } = module;

  let parse_ctx = p.fun.get_nth_param(0).unwrap().into_pointer_value();
  let state = p.state;

  let bool = ctx.bool_type();
  let i32 = ctx.i32_type();

  let all_good = b.build_alloca(i32, "all_good");

  // Set the context's goto pointers to point to the goto block;
  let attempt_extend = state.generate_block(module, "Attempt_Extend", 0);
  let truncation_check = state.generate_block(module, "Check_Truncation", 0);
  let valid_window = state.generate_block(module, "Valid_Window", 0);

  let input_len = b.build_load(input_len_ptr, "len").into_int_value();
  let input_available = b.build_int_sub(input_len, input_off, "");

  // if the requested_end_cursor_position is > the blocks end position, then try
  // requesting more input buffer.
  b.build_store(all_good, input_available);

  let comparison =
    b.build_int_compare(inkwell::IntPredicate::SGE, input_available, needed_num_bytes, "");
  b.build_conditional_branch(comparison, valid_window, truncation_check);

  b.position_at_end(truncation_check);

  let truncated = b.build_load(input_truncated_ptr, "len").into_int_value();
  let comparison =
    b.build_int_compare(inkwell::IntPredicate::EQ, truncated, bool.const_int(1, false), "");

  b.build_conditional_branch(comparison, attempt_extend, valid_window);
  b.position_at_end(attempt_extend);

  let requested_end_off = b.build_int_add(input_off, needed_num_bytes, "end_offset");
  build_fast_call(module, funct.get_adjusted_input_block, &[
    parse_ctx.into(),
    input_ptr_ptr.into(),
    input_len_ptr.into(),
    input_truncated_ptr.into(),
    input_off.into(),
    requested_end_off.into(),
  ]);

  let input_len = b.build_load(input_len_ptr, "len").into_int_value();
  let input_available = b.build_int_sub(input_len, input_off, "");
  b.build_store(all_good, input_available);

  b.build_unconditional_branch(valid_window);
  b.position_at_end(valid_window);

  SherpaResult::Ok(b.build_load(all_good, "").into_int_value())
}

pub(crate) unsafe fn construct_scan<'a>(m: &LLVMParserModule<'a>) -> SherpaResult<()> {
  // The scan mode reuses the parse context, and pushes a sentinel state
  // that guards against the scanner context consuming the states of the normal
  // context.

  let LLVMParserModule { builder: b, types, ctx, fun: funct, .. } = m;

  // Build a null function that will be used to break out of the scanner
  // context dispatch loop
  let null_fn = {
    let null_fn = m.module.add_function(
      "scan_stop",
      ctx.i32_type().fn_type(&[types.parse_ctx.ptr_type(0.into()).into()], false),
      Some(Linkage::Private),
    );

    null_fn.set_call_conventions(fastCC);

    let entry = ctx.append_basic_block(null_fn, "Entry");

    b.position_at_end(entry);

    b.build_return(Some(&ctx.i32_type().const_zero()));

    null_fn
  };

  let fn_value = funct.scan;
  //## Set the context's goto pointers to point to the goto block;
  let entry = ctx.append_basic_block(fn_value, "Entry");

  //## Extract Params
  let p_ctx = fn_value.get_nth_param(0)?.into_pointer_value();
  let scanner_parse_function_ptr = fn_value.get_nth_param(1)?.into_pointer_value();
  let input_ptr = fn_value.get_nth_param(2)?.into_pointer_value();
  let input_len_ptr = fn_value.get_nth_param(3)?.into_pointer_value();
  let input_trun_ptr = fn_value.get_nth_param(4)?.into_pointer_value();
  let input_offset = fn_value.get_nth_param(5)?.into_int_value();

  //## Entry Block
  b.position_at_end(entry);

  // Push the scanner sentinel state and the entry scanner state onto
  // the goto stack.

  CTX::scan_ptr.store(m, p_ctx, input_ptr)?;
  CTX::scan_input_len.store(m, p_ctx, b.build_load(input_len_ptr, "").into_int_value())?;
  CTX::scan_input_trun.store(m, p_ctx, b.build_load(input_trun_ptr, "").into_int_value())?;
  CTX::scan_off.store(m, p_ctx, input_offset)?;
  CTX::scan_anchor_off.store(m, p_ctx, input_offset)?;
  CTX::tok_id.store(m, p_ctx, ctx.i32_type().const_zero())?;
  CTX::scan_line_num.store(m, p_ctx, CTX::end_line_num.load(m, p_ctx)?.into_int_value())?;
  CTX::scan_line_off.store(m, p_ctx, CTX::end_line_off.load(m, p_ctx)?.into_int_value())?;

  let local_state = CTX::state.load(m, p_ctx)?;

  build_push_fn_state(
    m,
    p_ctx,
    null_fn.as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM,
  );

  build_push_fn_state(m, p_ctx, scanner_parse_function_ptr, NORMAL_STATE_FLAG_LLVM);

  // Dispatch!
  b.build_call(funct.dispatch, &[p_ctx.into()], "");

  // Convert scan_anchor_off and tok_off / peek_off into a length offset

  CTX::scan_len.store(
    m,
    p_ctx,
    b.build_int_sub(CTX::scan_anchor_off.load(m, p_ctx)?.into_int_value(), input_offset.into(), ""),
  );
  //b.build_store(input_len_ptr, CTX::scan_input_len.load(m, p_ctx)?);
  //b.build_store(input_trun_ptr, CTX::scan_input_trun.load(m, p_ctx)?);

  // Ensure the the local context state is set.
  CTX::state.store(m, p_ctx, local_state);

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
