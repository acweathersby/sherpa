use super::{
  build_fast_call,
  build_tail_call_with_return,
  fastCC,
  validate,
  CTX_AGGREGATE_INDICES as CTX,
  NORMAL_STATE_FLAG_LLVM,
};
use crate::{
  grammar::compile::parser::sherpa::{Goto, *},
  llvm::{add_goto_pop_instructions, LLVMParserModule, FAIL_STATE_FLAG_LLVM},
  parser::{hash_group_btreemap, hash_group_vec},
  types::*,
  Journal,
};
use inkwell::{
  basic_block::BasicBlock,
  module::Linkage,
  types::IntType,
  values::{FunctionValue, IntValue, PointerValue},
};
use sherpa_runtime::{types::bytecode::Opcode, utf8::lookup_table::CodePointClass};
use std::{
  collections::{hash_map::DefaultHasher, BTreeMap},
  hash::{Hash, Hasher},
};

pub(crate) fn compile_states<'a>(
  j: &mut Journal,
  m: &'a LLVMParserModule,
  states: &Vec<(String, Box<ParseState>)>,
) -> SherpaResult<()> {
  let mut state_lu = states
    .iter()
    .map(|(name, _)| (name.to_owned(), create_parse_function(j, m, name)))
    .collect::<BTreeMap<_, _>>();

  construct_prime_function(j, m, &state_lu)?;

  let scan_stop = create_scanner_stop_fn(m);

  for (_, state) in states {
    compile_state(j, m, state.get_ast()?, &mut state_lu, scan_stop)?;
  }

  for (_, fun) in state_lu {
    validate(fun)?
  }

  SherpaResult::Ok(())
}

fn create_scanner_stop_fn<'a>(m: &'a LLVMParserModule) -> FunctionValue<'a> {
  let null_fn = m.module.add_function(
    "scan_stop",
    m.ctx.i32_type().fn_type(&[m.types.parse_ctx.ptr_type(0.into()).into()], false),
    Some(Linkage::Private),
  );
  null_fn.set_call_conventions(fastCC);
  let entry = m.ctx.append_basic_block(null_fn, "Entry");
  m.b.position_at_end(entry);
  m.b.build_return(Some(&m.ctx.i32_type().const_zero()));

  null_fn
}

fn create_parse_function<'a>(
  j: &mut Journal,
  module: &'a LLVMParserModule,
  name: &str,
) -> FunctionValue<'a> {
  let linkage = if j.config().opt_llvm { Some(Linkage::Private) } else { None };
  module.module.add_function(&name, module.types.TAIL_CALLABLE_PARSE_FUNCTION, linkage)
}

pub(crate) fn compile_state<'a>(
  j: &mut Journal,
  m: &'a LLVMParserModule,
  state: &IR_STATE,
  s_lu: &mut BTreeMap<String, FunctionValue<'a>>,
  scan_stop: FunctionValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, i64, i32, i8, .. } = m;
  let u32_1 = i32.const_int(1, false);
  let state_name = &state.id;
  let s_fun = *s_lu.get(state_name)?;
  let p_ctx = s_fun.get_first_param()?.into_pointer_value();
  let entry_block = ctx.append_basic_block(s_fun, "entry");
  let loop_head = ctx.append_basic_block(s_fun, "loop_head");

  b.position_at_end(entry_block);
  b.build_unconditional_branch(loop_head);
  b.position_at_end(loop_head);

  let mut last_block = loop_head;

  if matches!(state.instructions[0].get_type(), ASTNodeType::ASSERT | ASTNodeType::DEFAULT) {
    // split instructions into groups
    let groups =
      hash_group_btreemap(state.instructions.iter().collect::<Vec<_>>(), |_, i| match i {
        ASTNode::ASSERT(box ASSERT { mode, .. }) => mode.clone(),
        ASTNode::DEFAULT(_) => "DEFAULT".into(),
        _ => unreachable!(),
      });

    if let Some(production_branches) = groups.get("PRODUCTION") {
      debug_assert!(groups.len() == 1 || groups.contains_key("DEFAULT"));

      let (cases, branches) =
        deconstruct_branches("production_", production_branches, m, s_fun, i32)?;

      let default_block = ctx.append_basic_block(s_fun, &format!("production_default"));
      b.build_switch(
        CTX::prod_id.load(b, p_ctx)?.into_int_value(),
        default_block,
        &convert_to_case_entry(cases),
      );

      for (block, i) in branches {
        b.position_at_end(block);
        compile_branchless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
      }

      b.position_at_end(default_block);
      last_block = default_block;
    }

    if let Some(token_branches) = groups.get("TOKEN") {
      let (cases, branches) = deconstruct_branches("token_", token_branches, m, s_fun, i32)?;

      construct_scan(m, p_ctx, s_fun, *s_lu.get(&state.scanner)?, scan_stop)?;

      let value = CTX::tok_id.load(b, p_ctx)?.into_int_value();

      let default_block = ctx.append_basic_block(s_fun, &format!("token_default"));
      b.build_switch(value, default_block, &convert_to_case_entry(cases));

      for (block, i) in branches {
        b.position_at_end(block);
        compile_branchless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
      }

      b.position_at_end(default_block);
      last_block = default_block;
    }

    if groups.keys().any(|k| matches!(k.as_str(), "BYTE" | "CODEPOINT" | "CLASS" | "EOF")) {
      // If the block size is less than 1 then jump to default
      let active_block = last_block;
      let symbol_branches_start = ctx.append_basic_block(s_fun, "table_branches");

      last_block = symbol_branches_start;

      b.position_at_end(symbol_branches_start);

      let scan_ptr_cache = CTX::scan_ptr.load(b, p_ctx)?.into_pointer_value();

      if let Some(byte_branches) = groups.get("BYTE") {
        let (mut cases, branches) = deconstruct_branches("byte_", byte_branches, m, s_fun, i8)?;

        let value = b.build_load(scan_ptr_cache, "").into_int_value();

        CTX::sym_len.store(b, p_ctx, u32_1)?;

        // if one of the cases is the val `[\n] == 10` then we want to
        // increment the line data. We do this by creating a new block
        // that is `increment_line_data`, and set the target of this block
        // to the existing block of the case.

        cases.iter_mut().filter(|(i, ..)| *i == 10).for_each(|i| {
          create_line_increment(m, s_fun, p_ctx, i8.const_int(1, false), i);
        });

        for (block, i) in branches {
          b.position_at_end(block);
          compile_branchless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
        }

        b.position_at_end(last_block);

        let default_block = ctx.append_basic_block(s_fun, &format!("byte_default"));
        b.build_switch(value, default_block, &convert_to_case_entry(cases));

        b.position_at_end(default_block);
        last_block = default_block;
      }

      if groups.keys().any(|k| matches!(k.as_str(), "CODEPOINT" | "CLASS")) {
        let (cp_val, tok_len) = construct_cp_lu_with_token_len_store(m, scan_ptr_cache)?;

        CTX::sym_len.store(b, p_ctx, tok_len)?;

        if let Some(cp_branches) = groups.get("CODEPOINT") {
          let (cases, branches) = deconstruct_branches("codepoint_", cp_branches, m, s_fun, i32)?;

          let default_block = ctx.append_basic_block(s_fun, &format!("codepoint_default"));

          b.build_switch(cp_val, default_block, &convert_to_case_entry(cases));

          for (block, i) in branches {
            b.position_at_end(block);
            compile_branchless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
          }

          b.position_at_end(default_block);
          last_block = default_block;
        }

        if let Some(class_branches) = groups.get("CLASS") {
          let (mut cases, branches) =
            deconstruct_branches("class_", class_branches, m, s_fun, i32)?;

          cases.iter_mut().filter(|(i, ..)| *i == CodePointClass::NewLine as i64).for_each(|i| {
            create_line_increment(m, s_fun, p_ctx, i8.const_int(1, false), i);
          });

          for (block, i) in branches {
            b.position_at_end(block);
            compile_branchless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
          }

          b.position_at_end(last_block);

          let cp_val = build_fast_call(b, m.fun.get_token_class_from_codepoint, &[cp_val.into()])?
            .try_as_basic_value()
            .unwrap_left()
            .into_int_value();

          let default_block = ctx.append_basic_block(s_fun, &format!("class_default"));
          b.build_switch(cp_val, default_block, &convert_to_case_entry(cases));

          b.position_at_end(default_block);
          last_block = default_block;
        }
      }

      let last_block = if let Some(eof_branches) = groups.get("EOF") {
        b.position_at_end(last_block);
        let eof_start = last_block;

        // Assert that the head is in the end-of file position
        let chars = get_chars_remaining(m, p_ctx)?;
        let default_block = ctx.append_basic_block(s_fun, "default");
        let eof_block = ctx.append_basic_block(s_fun, "eof");

        let c =
          b.build_int_compare(inkwell::IntPredicate::EQ, chars, m.iptr.const_int(0, false), "");
        b.build_conditional_branch(c, eof_block, default_block);
        b.position_at_end(eof_block);

        match eof_branches[0] {
          ASTNode::ASSERT(box ASSERT { instructions: i, .. }) => {
            compile_branchless_instructions(j, m, &i, p_ctx, s_fun, loop_head, s_lu, state_name)?
          }
          _ => unreachable!(),
        };

        last_block = default_block;
        eof_start
      } else {
        last_block
      };

      b.position_at_end(active_block);
      check_for_input_acceptability(
        m,
        s_fun,
        p_ctx,
        i64.const_int(1, false),
        symbol_branches_start,
        last_block,
      )?;
    }

    b.position_at_end(last_block);

    if let Some(default_branches) = groups.get("DEFAULT") {
      debug_assert!(default_branches.len() == 1);
      let DEFAULT { instructions: i } = default_branches[0].as_DEFAULT()?;
      compile_branchless_instructions(j, m, &i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
    } else {
      fail(m, p_ctx, s_fun)?;
    }
  } else {
    let i = &state.instructions;
    compile_branchless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
  }

  SherpaResult::Ok(())
}

fn create_line_increment<'a>(
  m: &'a LLVMParserModule,
  state_fun: FunctionValue<'a>,
  p: PointerValue<'a>,
  increment_amount: IntValue<'a>,
  i: &mut (i64, IntValue<'a>, BasicBlock<'a>),
) -> SherpaResult<()> {
  let LLVMParserModule { ctx, b, .. } = m;
  let i64 = ctx.i64_type();
  let i32 = ctx.i32_type();

  let block = ctx.append_basic_block(state_fun, "increment_line_data");
  b.position_at_end(block);

  let beg = b.build_ptr_to_int(CTX::beg_ptr.load(b, p)?.into_pointer_value(), i64, "");
  let tail = b.build_ptr_to_int(CTX::scan_ptr.load(b, p)?.into_pointer_value(), i64, "");
  let val = b.build_int_sub(tail, beg, "");
  let val = b.build_int_truncate(val, i32, "");
  CTX::end_line_off.store(b, p, val)?;
  CTX::line_incr.store(b, p, increment_amount)?;

  b.build_unconditional_branch(i.2);
  (i.2) = block;

  SherpaResult::Ok(())
}

fn convert_to_case_entry<'a>(
  cases: Vec<(i64, IntValue<'a>, BasicBlock<'a>)>,
) -> Vec<(IntValue<'a>, BasicBlock<'a>)> {
  cases.into_iter().map(|(_, a, b)| (a, b)).collect::<Vec<_>>()
}

fn deconstruct_branches<'llvm, 'grammar>(
  branch_prefix: &str,
  branches: &'grammar Vec<&'grammar ASTNode>,
  m: &'llvm LLVMParserModule,
  func: FunctionValue<'llvm>,
  val_type: &IntType<'llvm>,
) -> SherpaResult<(
  Vec<(i64, IntValue<'llvm>, BasicBlock<'llvm>)>,
  Vec<(BasicBlock<'llvm>, &'grammar Vec<ASTNode>)>,
)> {
  let (cases, branches): (
    Vec<Vec<(i64, IntValue<'llvm>, BasicBlock<'llvm>)>>,
    Vec<(BasicBlock<'llvm>, &'grammar Vec<ASTNode>)>,
  ) =
    hash_group_vec(branches.iter().map(|b| b.as_ASSERT().unwrap()).collect::<Vec<_>>(), |_, b| {
      let mut hasher = DefaultHasher::new();
      b.instructions.hash(&mut hasher);
      hasher.finish()
    })
    .iter()
    .map(|i| {
      let assert = i[0];
      let block = m.ctx.append_basic_block(
        func,
        &format!(
          "{branch_prefix}{}",
          i.iter().map(|i| i.ids.val.to_string()).collect::<Vec<_>>().join("_")
        ),
      );
      let cases = i
        .iter()
        .map(|i| (i.ids.val, val_type.const_int(i.ids.val as u64, false).into(), block))
        .collect::<Vec<_>>();
      (cases, (block, &assert.instructions))
    })
    .unzip();

  SherpaResult::Ok((cases.into_iter().flatten().collect(), branches))
}

pub(crate) fn compile_branchless_instructions<'a>(
  j: &mut Journal,
  m: &'a LLVMParserModule,
  instructions: &Vec<ASTNode>,
  mut p_ctx: PointerValue<'a>,
  mut state_fun: FunctionValue<'a>,
  start_block: BasicBlock<'a>,
  state_lu: &mut BTreeMap<String, FunctionValue<'a>>,
  state_name: &str,
) -> SherpaResult<()> {
  // reverse gotos so jumps operate correctly in a stack structure.

  let mut resolved_end = false;
  let mut goto_encountered = 0;
  let goto_count = instructions.iter().filter(|i| i.get_type() == ASTNodeType::PushGoto).count();
  let pass_last = matches!(instructions.last()?.get_type(), ASTNodeType::Pass);

  for (i, instruction) in instructions.iter().enumerate() {
    let is_second_to_last = i as i64 == ((instructions.len() as i64) - 2);
    let no_reenter = is_second_to_last && pass_last;
    match instruction {
      ASTNode::Accept(_) => {
        accept(m)?;
        resolved_end = true;
        break;
      }

      ASTNode::Pass(_) => {
        pass(m, p_ctx, state_fun)?;
        resolved_end = true;
        break;
      }

      ASTNode::Fail(_) => {
        fail(m, p_ctx, state_fun)?;
        resolved_end = true;
        break;
      }

      ASTNode::Pop(_) => {
        pop_goto(m, p_ctx, state_fun)?;
        resolved_end = true;
        break;
      }

      ASTNode::Goto(g) => {
        goto(g, m, state_fun, &state_lu)?;
        resolved_end = true;
        break;
      }

      ASTNode::PushGoto(pg) => {
        if goto_encountered == 0 {
          ensure_space_on_goto_stack(goto_count, m, p_ctx, state_fun)?;
        }
        goto_encountered += 1;
        if goto_encountered == goto_count {
          update_goto_remaining(goto_count, m, p_ctx)?;
        }

        push_goto(&pg, m, p_ctx, &state_lu)?;
      }

      ASTNode::PushExceptHandler(except) => {
        ensure_space_on_goto_stack(2, m, p_ctx, state_fun)?;
        update_goto_remaining(2, m, p_ctx)?;
        push_except(&except, m, p_ctx, &state_lu)?;
      }

      ASTNode::TokenAssign(box TokenAssign { ids }) => {
        construct_assign_token_id(m, p_ctx, ids[0].val as u64)?;
        transfer_end_line_to_chkp_line(m, p_ctx)?;
      }

      ASTNode::Reduce(r) => {
        if let Some((s, p)) = reduce(r, j, m, p_ctx, no_reenter, state_name)? {
          state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
      }

      ASTNode::PeekReset(_) => {
        peek_reset(m, p_ctx)?;
      }

      ASTNode::PeekToken(_) => {
        peek_token(m, p_ctx)?;
      }

      ASTNode::SkipPeekToken(_) => {
        skip_token(m, p_ctx, true)?;
        construct_jump_to_table_start(m, start_block);
        resolved_end = true;
        break;
      }

      ASTNode::SkipToken(_) => {
        skip_token(m, p_ctx, false)?;
        construct_jump_to_table_start(m, start_block);
        resolved_end = true;
        break;
      }

      ASTNode::ShiftToken(box ShiftToken { .. }) => {
        if let Some((s, p)) = construct_token_shift(j, m, p_ctx)? {
          state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
      }

      ASTNode::ScanShift(..) => {
        incr_scan_ptr_by_sym_len(m, p_ctx)?;
        update_end_line_num(m, p_ctx)?;
      }

      ASTNode::PeekTokenScanless(_) => {
        incr_scan_ptr_by_sym_len(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0)?;
        peek_token(m, p_ctx)?;
      }

      ASTNode::SkipPeekTokenScanless(_) => {
        incr_scan_ptr_by_sym_len(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0)?;

        //update_end_line_num(m, p_ctx)?;
        //transfer_end_line_to_chkp_line(m, p_ctx)?;

        skip_token(m, p_ctx, true)?;
        construct_jump_to_table_start(m, start_block);
        resolved_end = true;
        break;
      }

      ASTNode::SkipTokenScanless(_) => {
        incr_scan_ptr_by_sym_len(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0)?;

        update_end_line_num(m, p_ctx)?;
        transfer_end_line_to_chkp_line(m, p_ctx)?;

        skip_token(m, p_ctx, false)?;
        construct_jump_to_table_start(m, start_block);
        resolved_end = true;
        break;
      }

      ASTNode::ShiftTokenScanless(_) => {
        incr_scan_ptr_by_sym_len(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0)?;

        update_end_line_num(m, p_ctx)?;
        transfer_end_line_to_chkp_line(m, p_ctx)?;

        if let Some((s, p)) = construct_token_shift(j, m, p_ctx)? {
          state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
      }
      _ => {}
    }
  }

  if !resolved_end {
    pass(m, p_ctx, state_fun);
  }

  SherpaResult::Ok(())
}

pub(crate) fn add_goto_slot(
  LLVMParserModule { b, ctx, i32, types, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  goto_fn: PointerValue,
  goto_state_val: u64,
) -> SherpaResult<()> {
  let goto_state = ctx.i32_type().const_int(goto_state_val, false);

  // Get the top slot ptr ptr;
  let goto_top = CTX::goto_stack_ptr.load(b, p_ctx)?.into_pointer_value();

  // Create new goto struct
  let new_goto = b.build_insert_value(types.goto.get_undef(), goto_state, 1, "")?;
  let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "")?;

  // Store in the current slot
  b.build_store(goto_top, new_goto);

  // Increment the slot
  let goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false)], "") };

  // Store the top slot pointer
  CTX::goto_stack_ptr.store(b, p_ctx, goto_top)?;

  SherpaResult::Ok(())
}

pub(crate) fn push_except<'a>(
  PushExceptHandler { state }: &PushExceptHandler,
  m: &LLVMParserModule,
  p_ctx: PointerValue,
  state_lu: &BTreeMap<String, FunctionValue>,
) -> SherpaResult<()> {
  add_goto_slot(
    m,
    p_ctx,
    (*state_lu.get(&state.val)?).as_global_value().as_pointer_value(),
    FAIL_STATE_FLAG_LLVM as u64,
  );

  add_goto_slot(
    m,
    p_ctx,
    m.fun.pop_state.as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM as u64,
  );

  SherpaResult::Ok(())
}

pub(crate) fn push_goto<'a>(
  PushGoto { state }: &PushGoto,
  m: &LLVMParserModule,
  p_ctx: PointerValue,
  state_lu: &BTreeMap<String, FunctionValue>,
) -> SherpaResult<()> {
  add_goto_slot(
    m,
    p_ctx,
    (*state_lu.get(&state.val)?).as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM as u64,
  );

  SherpaResult::Ok(())
}

/// Subtracts `goto_slots_consumed` from the `goto_free` counter.
fn update_goto_remaining<'a>(
  goto_slots_consumed: usize,
  LLVMParserModule { b, i32, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  // Decrement goto remaining
  let goto_free_ptr = CTX::goto_free.get_ptr(b, p_ctx)?;
  let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
  let goto_free = b.build_int_sub(goto_free, i32.const_int(goto_slots_consumed as u64, false), "");
  b.build_store(goto_free_ptr, goto_free);
  SherpaResult::Ok(())
}

pub(crate) fn goto<'a>(
  Goto { state }: &Goto,
  LLVMParserModule { b, .. }: &LLVMParserModule,
  state_fun: FunctionValue<'a>,
  state_lu: &BTreeMap<String, FunctionValue>,
) -> SherpaResult<()> {
  build_tail_call_with_return(b, state_fun, *state_lu.get(&state.val)?)
}

pub(crate) fn ensure_space_on_goto_stack<'a>(
  needed_slot_count: usize,
  LLVMParserModule { b, fun, ctx, i32, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  state_fn: FunctionValue,
) -> SherpaResult<()> {
  // Compare the number of needed slots with the number of available slots
  let goto_free = CTX::goto_free.load(b, p_ctx)?.into_int_value();

  let needed_slots = i32.const_int(needed_slot_count as u64, false);
  let comparison = b.build_int_compare(inkwell::IntPredicate::UGE, goto_free, needed_slots, "");

  let extend_block = ctx.append_basic_block(state_fn, "Extend");
  let ready_block = ctx.append_basic_block(state_fn, "Return");

  b.build_conditional_branch(comparison, ready_block, extend_block);

  b.position_at_end(extend_block);

  build_fast_call(b, fun.extend_stack, &[
    p_ctx.into(),
    i32.const_int((2 << needed_slot_count.checked_ilog2().unwrap_or(0)) as u64, false).into(),
  ])?;

  b.build_unconditional_branch(ready_block);
  b.position_at_end(ready_block);

  SherpaResult::Ok(())
}

/// Causes the execution to jump back to the beginning of the `loop_start` block.
fn construct_jump_to_table_start(m: &LLVMParserModule, start_block: BasicBlock) {
  m.b.build_unconditional_branch(start_block);
}

/// Extends the `scan_ptr` by `sym_len`
///
/// Normally used by scanner to move the `scan_ptr` head forward.
///
/// Also used by scanless states to position `scan_ptr` in preparation
/// to calculate to `tok_len` value, which is ultimately used to move
/// the `head_ptr` ( and possibly the `base_ptr`) forward to the end of
/// the token, and also to issue a `Shift` event with a sized
/// token.
///
fn incr_scan_ptr_by_sym_len<'a>(
  LLVMParserModule { b, .. }: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<()> {
  let scan_ptr_cache = CTX::scan_ptr.load(b, p_ctx)?.into_pointer_value();

  CTX::scan_ptr.store(b, p_ctx, unsafe {
    b.build_gep(scan_ptr_cache, &[CTX::sym_len.load(b, p_ctx)?.into_int_value()], "")
  });

  SherpaResult::Ok(())
}

/// Update the `end_line*`  values by incrementing by the `line_incr` and `l`
fn update_end_line_num<'a>(
  LLVMParserModule { b, i32, i8, .. }: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<()> {
  // Increment line number;
  let val = CTX::line_incr.load(b, p_ctx)?.into_int_value();
  CTX::line_incr.store(b, p_ctx, i8.const_zero())?;

  let new_val = b.build_int_add(
    CTX::end_line_num.load(b, p_ctx)?.into_int_value(),
    b.build_int_z_extend(val, *i32, ""),
    "",
  );
  CTX::end_line_num.store(b, p_ctx, new_val)?;

  SherpaResult::Ok(())
}

pub(crate) fn pop_goto<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
  state_fun: FunctionValue<'a>,
) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::PopGoto;

  add_goto_pop_instructions(m, p_ctx)?;

  pass(m, p_ctx, state_fun)
}

pub(crate) fn reduce<'a>(
  Reduce { rule_id, prod_id, len, .. }: &Reduce,
  j: &mut Journal,
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
  no_reenter: bool,
  state_name: &str,
) -> SherpaResult<Option<(FunctionValue<'a>, PointerValue<'a>)>> {
  const __HINT__: Opcode = Opcode::Reduce;

  let LLVMParserModule { b, ctx, fun, i32, .. } = m;

  CTX::sym_len.store(b, p_ctx, i32.const_int(*len as u64, false))?;
  CTX::rule_id.store(b, p_ctx, i32.const_int(*rule_id as u64, false))?;
  CTX::prod_id.store(b, p_ctx, i32.const_int(*prod_id as u64, false))?;

  let f = if !no_reenter {
    let f = create_parse_function(j, m, &format!("reduce_{state_name}"));

    build_fast_call(b, fun.push_state, &[
      p_ctx.into(),
      i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
      f.as_global_value().as_pointer_value().into(),
    ])?;

    b.build_return(Some(&i32.const_int(ParseActionType::Reduce.into(), false)));

    b.position_at_end(ctx.append_basic_block(f, "entry"));
    let p_ctx = f.get_first_param()?.into_pointer_value();

    Some((f, p_ctx))
  } else {
    b.build_return(Some(&i32.const_int(ParseActionType::Reduce.into(), false)));
    None
  };

  SherpaResult::Ok(f)
}

/// Resets peek side-effects by assigning `base_ptr` to `head_ptr` and `scan_ptr`. Also
pub(crate) fn peek_reset<'a>(
  LLVMParserModule { b, i32, iptr, .. }: &'a LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::PeekReset;
  let offset = CTX::base_ptr.load(b, p_ctx)?.into_pointer_value();
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::scan_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());
  CTX::tok_len.store(b, p_ctx, iptr.const_zero());
  CTX::sym_len.store(b, p_ctx, i32.const_zero());
  SherpaResult::Ok(())
}
/// Assigns the value head_ptr + tok_len to head_ptr and scan_ptr.
/// Also sets the values of tok_len and tok_id to 0.
pub(crate) fn peek_token<'a>(m: &'a LLVMParserModule, p_ctx: PointerValue) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::PeekToken;
  let LLVMParserModule { b, i32, iptr, .. } = m;
  let offset = get_offset_to_end_of_token(m, p_ctx)?;
  CTX::scan_ptr.store(b, p_ctx, offset);
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());
  CTX::tok_len.store(b, p_ctx, iptr.const_zero());
  SherpaResult::Ok(())
}

/// Return the difference between the `scan_ptr` and `end_ptr`.
/// Value type is `iptr`
pub(crate) fn get_chars_remaining<'a>(
  sp: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<IntValue<'a>> {
  let LLVMParserModule { b, iptr, .. } = sp;
  let scan_int = b.build_ptr_to_int(CTX::scan_ptr.load(b, p_ctx)?.into_pointer_value(), *iptr, "");
  let end_int = b.build_ptr_to_int(CTX::end_ptr.load(b, p_ctx)?.into_pointer_value(), *iptr, "");
  SherpaResult::Ok(b.build_int_sub(end_int, scan_int, "").into())
}

/// Assigns `head_ptr + tok_len` to head_ptr, scan_ptr, and base_ptr.
/// Also assign 0  to `tok_id`
pub(crate) fn skip_token<'a>(
  sp: &LLVMParserModule,
  p_ctx: PointerValue,
  is_peek: bool,
) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::SkipToken;
  let offset = get_offset_to_end_of_token(sp, p_ctx)?;
  let LLVMParserModule { b, i32, .. } = sp;
  CTX::scan_ptr.store(b, p_ctx, offset);
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());

  if !is_peek {
    CTX::base_ptr.store(b, p_ctx, offset);
    CTX::start_line_num.store(b, p_ctx, CTX::chkp_line_num.load(b, p_ctx)?.into_int_value())?;
    CTX::start_line_off.store(b, p_ctx, CTX::chkp_line_off.load(b, p_ctx)?.into_int_value())?;
  }
  SherpaResult::Ok(())
}

/// Calculates an offset to the end of the current token.
/// `offset_ptr = head_ptr + tok_len`. This also sets the
/// `tok_len` to 0.
pub(crate) fn get_offset_to_end_of_token<'a>(
  sp: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<PointerValue<'a>> {
  let LLVMParserModule { b, i8, iptr, .. } = sp;
  let head_ptr = CTX::head_ptr.load(b, p_ctx)?.into_pointer_value();
  let tok_len = CTX::tok_len.load(b, p_ctx)?.into_int_value();
  let offset = b.build_ptr_to_int(head_ptr, *iptr, "");
  let offset = b.build_int_add(offset, tok_len, "");
  let offset = b.build_int_to_ptr(offset, i8.ptr_type(0.into()), "");
  CTX::tok_len.store(b, p_ctx, iptr.const_zero())?;
  SherpaResult::Ok(offset)
}

pub(crate) fn construct_token_shift<'a>(
  j: &mut Journal,
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<Option<(FunctionValue<'a>, PointerValue<'a>)>> {
  let LLVMParserModule { b, i32, .. } = m;

  let fun = create_parse_function(j, m, "shift");

  build_fast_call(b, m.fun.push_state, &[
    p_ctx.into(),
    m.ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
    fun.as_global_value().as_pointer_value().into(),
  ])?;

  b.build_return(Some(&m.ctx.i32_type().const_int(ParseActionType::Shift.into(), false)));

  b.position_at_end(m.ctx.append_basic_block(fun, "entry"));

  let p_ctx = fun.get_first_param()?.into_pointer_value();

  CTX::start_line_num.store(b, p_ctx, CTX::chkp_line_num.load(b, p_ctx)?.into_int_value())?;
  CTX::start_line_off.store(b, p_ctx, CTX::chkp_line_off.load(b, p_ctx)?.into_int_value())?;

  let offset = get_offset_to_end_of_token(m, p_ctx)?;
  CTX::scan_ptr.store(b, p_ctx, offset);
  CTX::anchor_ptr.store(b, p_ctx, offset);
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::base_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());

  SherpaResult::Ok(Some((fun, p_ctx)))
}

/// Transfers the difference between `scan_ptr` and `head_ptr` to `tok_len`.
/// If `token_value` is not 0, then assigns it's value to `tok_id`.
fn construct_assign_token_id(
  LLVMParserModule { b, iptr, i32, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  token_value: u64,
) -> SherpaResult<()> {
  if token_value > 0 {
    CTX::tok_id.store(b, p_ctx, i32.const_int(token_value as u64, false))?;
  }

  let len = b.build_int_sub(
    CTX::scan_ptr.load_ptr_as_int(b, p_ctx, *iptr)?,
    CTX::head_ptr.load_ptr_as_int(b, p_ctx, *iptr)?,
    "",
  );

  CTX::tok_len.store(b, p_ctx, len)?;

  SherpaResult::Ok(())
}

/// Transfers the `end_line*` values to the `chkp_line*` variables.
fn transfer_end_line_to_chkp_line(
  LLVMParserModule { b, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  CTX::chkp_line_num.store(b, p_ctx, CTX::end_line_num.load(b, p_ctx)?.into_int_value())?;
  CTX::chkp_line_off.store(b, p_ctx, CTX::end_line_off.load(b, p_ctx)?.into_int_value())?;
  SherpaResult::Ok(())
}

/// Builds a tail call return to the `dispatch` function.
pub(crate) fn pass<'a>(
  LLVMParserModule { b, fun, i32, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  state_fun: FunctionValue,
) -> SherpaResult<()> {
  b.build_store(CTX::state.get_ptr(b, p_ctx)?, i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false));
  build_tail_call_with_return(b, state_fun, fun.dispatch)
}

/// Builds a normal return with the Accept event.
pub(crate) fn accept<'a>(LLVMParserModule { b, i32, .. }: &LLVMParserModule) -> SherpaResult<()> {
  b.build_return(Some(&i32.const_int(ParseActionType::Accept.into(), false)));
  SherpaResult::Ok(())
}

/// Resets any peek side-effects and builds a tail call return to the `dispatch_unwind` function.
///
pub(crate) fn fail<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue,
  state_fun: FunctionValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, i32, .. } = m;
  peek_reset(m, p_ctx);
  b.build_store(CTX::state.get_ptr(b, p_ctx)?, i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false));
  build_tail_call_with_return(b, state_fun, fun.dispatch_unwind)
}

fn construct_cp_lu_with_token_len_store<'a>(
  LLVMParserModule { b, fun, .. }: &'a LLVMParserModule,
  buffer: PointerValue<'a>,
) -> SherpaResult<(IntValue<'a>, IntValue<'a>)> {
  let cp_info = build_fast_call(b, fun.get_utf8_codepoint_info, &[buffer.into()])?
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();

  let cp_val = b.build_extract_value(cp_info, 0, "cp_val")?.into_int_value();
  let cp_byte_len = b.build_extract_value(cp_info, 1, "cp_len")?.into_int_value();

  SherpaResult::Ok((cp_val, cp_byte_len))
}

pub(crate) fn check_for_input_acceptability<'a>(
  m: &'a LLVMParserModule,
  state_fun: FunctionValue<'a>,
  p_ctx: PointerValue<'a>,
  needed_num_bytes: IntValue<'a>,
  valid_input: BasicBlock<'a>,
  invalid_input: BasicBlock<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, ctx, bool, .. } = m;
  let i64 = ctx.i64_type();

  // Set the context's goto pointers to point to the goto block;
  let try_extend = ctx.append_basic_block(state_fun, "attempt_extend");
  let check_if_eof = ctx.append_basic_block(state_fun, "check_if_eof");

  let input_available = get_chars_remaining(m, p_ctx)?;
  let input_available = b.build_int_cast(input_available, i64, "");
  let c = b.build_int_compare(inkwell::IntPredicate::SGE, input_available, needed_num_bytes, "");
  b.build_conditional_branch(c, valid_input, check_if_eof);

  // Check to see if the EOF flag is set -------------------------------------------------
  b.position_at_end(check_if_eof);
  let input_complete = CTX::block_is_eoi.load(b, p_ctx)?.into_int_value();
  let c =
    b.build_int_compare(inkwell::IntPredicate::EQ, input_complete, bool.const_int(1, false), "");
  b.build_conditional_branch(c, invalid_input, try_extend);

  // Try to get another block of input data ----------------------------------------------
  b.position_at_end(try_extend);
  build_fast_call(b, fun.get_adjusted_input_block, &[p_ctx.into(), needed_num_bytes.into()]);

  let input_available = get_chars_remaining(m, p_ctx)?;
  let input_available = b.build_int_cast(input_available, i64, "");
  let c = b.build_int_compare(inkwell::IntPredicate::SGE, input_available, needed_num_bytes, "");

  // TODO:
  // If input is bad then return a NEEDED_INPUT event, after pushing the current state to the stack.
  // This should pause the parser until some external signal indicates more data is available and
  // parsing can be resumed.

  b.build_conditional_branch(c, valid_input, invalid_input);

  SherpaResult::Ok(())
}

pub(crate) fn construct_scan<'a>(
  sp: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
  state_fn: FunctionValue<'a>,
  scan_fn: FunctionValue<'a>,
  null_fn: FunctionValue<'a>,
) -> SherpaResult<()> {
  // The scan mode reuses the parse context, and pushes a sentinel state
  // that guards against the scanner context consuming the states of the normal
  // context.

  let LLVMParserModule { b, fun, ctx, .. } = sp;

  // Build a null function that will be used to break out of the scanner
  // context dispatch loop

  // Push the scanner sentinel state and the entry scanner state onto
  // the goto stack.

  let local_state = CTX::state.load(b, p_ctx)?;

  let block = ctx.append_basic_block(state_fn, "scan");
  b.build_unconditional_branch(block);
  b.position_at_end(block);

  ensure_space_on_goto_stack(2, sp, p_ctx, state_fn)?;
  add_goto_slot(
    sp,
    p_ctx,
    null_fn.as_global_value().as_pointer_value(),
    (NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64,
  )?;
  add_goto_slot(
    sp,
    p_ctx,
    scan_fn.as_global_value().as_pointer_value(),
    (NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64,
  )?;
  update_goto_remaining(2, sp, p_ctx)?;

  // Dispatch!
  b.build_call(fun.dispatch, &[p_ctx.into()], "");

  // Ensure the the local context state is reset.
  CTX::state.store(b, p_ctx, local_state);

  // Reset scanner back to head
  CTX::scan_ptr.store(b, p_ctx, CTX::head_ptr.load(b, p_ctx)?.into_pointer_value());

  SherpaResult::Ok(())
}

fn build_push_fn_state<'a>(
  sp: &'a LLVMParserModule,
  parse_ctx: PointerValue<'a>,
  function_ptr: PointerValue<'a>,
  state_mask: u32,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, i32, .. } = sp;
  b.build_call(
    fun.push_state,
    &[parse_ctx.into(), i32.const_int(state_mask as u64, false).into(), function_ptr.into()],
    "",
  );

  SherpaResult::Ok(())
}

/// The prime function's purpose is fist create a base goto state that emits a failure
/// to prevent stack underflow when descending to the bottom of the goto stack, and also
/// insert the first GOTO entry that will initiate the parser to start parsing based on
/// an entry production id.
pub(crate) fn construct_prime_function(
  j: &mut Journal,

  sp: &LLVMParserModule,
  state_lu: &BTreeMap<String, FunctionValue>,
) -> SherpaResult<()> {
  let g = &(j.grammar()?);
  let LLVMParserModule { b, i32, bool, .. } = sp;
  let funct = &sp.fun;
  let fn_value = funct.prime;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  // Set the context's goto pointers to point to the goto block;
  let selector = fn_value.get_nth_param(1).unwrap().into_int_value();

  b.position_at_end(sp.ctx.append_basic_block(fn_value, "entry"));

  let options = g
    .get_exported_productions()
    .iter()
    .filter_map(|p| {
      let name = &g.get_entry_name_from_prod_id(&p.production.id).unwrap();
      let fun = state_lu.get(name).unwrap();
      let block = sp.ctx.append_basic_block(fn_value, &format!("prime_{}", name));
      Some((p.export_id, block, fun))
    })
    .collect::<Vec<_>>();

  build_fast_call(b, funct.extend_stack, &[parse_ctx.into(), i32.const_int(8, false).into()]);

  CTX::is_active.store(b, parse_ctx, bool.const_int(1, false))?;

  // Push the End-Of-Parse goto onto the stack. This will prevent underflow of the stack
  build_push_fn_state(
    sp,
    parse_ctx,
    sp.fun.handle_eop.as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM,
  );

  if !options.is_empty() {
    b.build_switch(
      selector,
      options[0].1.into(),
      &options.iter().map(|b| (i32.const_int(b.0 as u64, false), b.1)).collect::<Vec<_>>(),
    );

    for (_, block, fn_ptr) in &options {
      b.position_at_end(*block);
      b.build_call(
        funct.push_state,
        &[
          parse_ctx.into(),
          i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
          fn_ptr.as_global_value().as_pointer_value().into(),
        ],
        "",
      );

      b.build_return(None);
    }
  } else {
    b.build_return(None);
  }

  validate(fn_value)
}
