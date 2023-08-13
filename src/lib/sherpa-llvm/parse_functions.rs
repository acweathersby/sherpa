use super::{CtxAggregateIndices as CTX, *};
use crate::{LLVMParserModule, FAIL_STATE_FLAG_LLVM};
use inkwell::{
  basic_block::BasicBlock,
  module::Linkage,
  values::{FunctionValue, IntValue, PointerValue},
};
use sherpa_core::{proxy::Array, *};
use sherpa_rust_runtime::types::{
  bytecode::{InputType, Opcode},
  ParseActionType,
};
use std::collections::BTreeMap;

pub struct BuildArgs<'a, 'llvm: 'a> {
  pub state_name: &'a str,
  pub state:      &'a ParseState,
  pub j:          &'a Journal,
  pub db:         &'a ParserDatabase,
  pub m:          &'a LLVMParserModule<'llvm>,
  pub state_lu:   &'a mut BTreeMap<String, FunctionValue<'llvm>>,
  pub scan_stop:  FunctionValue<'llvm>,
}

pub(crate) fn compile_states<'llvm, 'db>(
  j: &mut Journal,
  m: &LLVMParserModule<'llvm>,
  db: &ParserDatabase,
  states: &ParseStatesVec,
) -> SherpaResult<()> {
  let mut state_lu = states
    .iter()
    .map(|(name, _)| {
      let name = name.to_string(db.string_store());
      (name.clone(), create_parse_function(j, m, name.as_str()))
    })
    .collect::<BTreeMap<_, _>>();

  construct_prime_function(j, db, m, &state_lu)?;

  let state_lu = &mut state_lu;

  let scan_stop = create_scanner_stop_fn(m);

  for (_, state) in states {
    compile_state(j, m, db, state, state_lu, scan_stop)?;
  }

  for (_, fun) in state_lu {
    validate(*fun)?
  }

  SherpaResult::Ok(())
}

fn create_scanner_stop_fn<'a>(m: &LLVMParserModule<'a>) -> FunctionValue<'a> {
  let null_fn = m.module.add_function(
    "scan_stop",
    m.i32.fn_type(&[m.types.parse_ctx.ptr_type(0.into()).into()], false),
    Some(Linkage::Private),
  );
  null_fn.set_call_conventions(fastCC);
  let entry = m.ctx.append_basic_block(null_fn, "Entry");
  m.b.position_at_end(entry);
  m.b.build_return(Some(&m.i32.const_zero()));

  null_fn
}

fn compile_state<'llvm, 'db>(
  j: &Journal,
  m: &LLVMParserModule<'llvm>,
  db: &ParserDatabase,
  parse_state: &ParseState,
  state_lu: &mut BTreeMap<String, FunctionValue<'llvm>>,
  scan_stop: FunctionValue<'llvm>,
) -> SherpaResult<()> {
  let LLVMParserModule { ctx, b, .. } = m;
  let state = parse_state.get_ast()?;
  let state_name = state.id.tok.to_string();
  let state_fun = *state_lu.get(&state_name)?;
  let p_ctx = state_fun.get_first_param()?.into_pointer_value();
  let entry_block = ctx.append_basic_block(state_fun, "entry");
  let loop_head = ctx.append_basic_block(state_fun, "loop_head");
  b.position_at_end(entry_block);
  b.build_unconditional_branch(loop_head);
  b.position_at_end(loop_head);

  let mut build_args = BuildArgs {
    j,
    m,
    db,
    scan_stop,
    state_lu: state_lu,
    state: parse_state,
    state_name: &state_name,
  };

  compile_statement(&mut build_args, &state.statement, p_ctx, state_fun, loop_head)?;

  SherpaResult::Ok(())
}

fn compile_statement<'a, 'llvm: 'a>(
  args: &mut BuildArgs<'a, 'llvm>,
  stmt: &parser::Statement,
  mut p_ctx: PointerValue<'llvm>,
  mut state_fun: FunctionValue<'llvm>,
  start_block: BasicBlock<'llvm>,
) -> SherpaResult<()> {
  let parser::Statement { branch, non_branch, transitive } = stmt;
  let mut resolved_end = false;
  let no_reenter = false;

  if let Some(transitive) = transitive {
    match transitive {
      parser::ASTNode::Pop(..) => {
        pop_goto(args, p_ctx, state_fun)?;
        resolved_end = true;
      }
      parser::ASTNode::Skip(..) => {
        transfer_chkp_line_to_start_line(args, p_ctx)?;
        transfer_start_line_to_end_line(args, p_ctx)?;
        skip_token(args, p_ctx, state_fun)?;
        //construct_jump_to_table_start(args, start_block);
        resolved_end = true;
      }
      parser::ASTNode::Scan(..) => {
        incr_scan_ptr_by_sym_len(args, p_ctx)?;
        update_end_line_data(args, p_ctx)?;
      }
      parser::ASTNode::Shift(..) => {
        if let Some((s, p)) = construct_token_shift(args, p_ctx)? {
          args.state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
        transfer_chkp_line_to_start_line(args, p_ctx)?;
        transfer_start_line_to_end_line(args, p_ctx)?;
      }
      parser::ASTNode::Peek(..) => {
        peek_token(args, p_ctx)?;
      }
      parser::ASTNode::Reset(..) => {
        peek_reset(args, p_ctx)?;
        transfer_start_line_to_end_line(args, p_ctx)?;
      }
      node => {
        #[cfg(debug_assertions)]
        dbg!(node);
        unreachable!();
      }
    }
  }

  for non_branch in non_branch {
    match non_branch {
      parser::ASTNode::ReduceRaw(r) => {
        if let Some((s, p)) = reduce(r.as_ref(), args, p_ctx, no_reenter)? {
          args.state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
      }
      parser::ASTNode::SetTokenId(tok) => {
        construct_assign_token_id(args, p_ctx, tok.id as u64)?;
        transfer_end_line_to_chkp_line(args, p_ctx)?;
      }
      node => {
        #[cfg(debug_assertions)]
        dbg!(node);
        unreachable!();
      }
    }
  }

  if let Some(branch) = branch {
    match branch {
      parser::ASTNode::Pass(..) => {
        pass(args, p_ctx, state_fun)?;
        resolved_end = true;
      }
      parser::ASTNode::Fail(..) => {
        fail(args, p_ctx, state_fun)?;
        resolved_end = true;
      }
      parser::ASTNode::Accept(..) => {
        accept(args)?;
        resolved_end = true;
      }
      parser::ASTNode::Gotos(gotos) => {
        let num_of_pushes = gotos.pushes.len();
        if num_of_pushes > 0 {
          ensure_space_on_goto_stack(num_of_pushes, args.m, p_ctx, state_fun)?;
          for push in &gotos.pushes {
            push_goto(&push, args, p_ctx)?;
          }
          update_goto_remaining(num_of_pushes, args.m, p_ctx)?;
        }

        goto(&gotos.goto, args, state_fun)?;
        resolved_end = true;
      }
      matches => {
        let matches = matches.as_Matches().unwrap();

        compile_match(matches, args, p_ctx, state_fun, start_block)?;

        resolved_end = true;
      }
    }
  }

  if !resolved_end {
    pass(args, p_ctx, state_fun);
  }

  SherpaResult::Ok(())
}

/// Return the difference between the `scan_ptr` and `end_ptr`.
/// Value type is `iptr`
pub(crate) fn get_chars_remaining<'a, 'llvm: 'a>(
  args: &mut BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<IntValue<'a>> {
  let LLVMParserModule { b, iptr, .. } = args.m;
  let scan_int = b.build_ptr_to_int(CTX::scan_ptr.load(b, p_ctx)?.into_pointer_value(), *iptr, "");
  let end_int = b.build_ptr_to_int(CTX::end_ptr.load(b, p_ctx)?.into_pointer_value(), *iptr, "");
  SherpaResult::Ok(b.build_int_sub(end_int, scan_int, "").into())
}

fn compile_match<'a, 'llvm: 'a>(
  matches_ast: &parser::Matches,
  args: &mut BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue<'llvm>,
  state_fun: FunctionValue<'llvm>,
  start_block: BasicBlock<'llvm>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, i64, i32, i8, .. } = args.m;
  let u32_1 = i32.const_int(1, false);
  let parser::Matches { matches, mode, .. } = matches_ast;
  let mut is_raw_char = false;

  let symbol_branches_start = ctx.append_basic_block(state_fun, "match");
  let default_block = ctx.append_basic_block(state_fun, "default");

  if matches!(mode.as_str(), InputType::BYTE_STR | InputType::CLASS_STR | InputType::CODEPOINT_STR)
  {
    check_for_input_acceptability(
      args,
      state_fun,
      p_ctx,
      i64.const_int(1, false),
      symbol_branches_start,
      default_block,
    )?;
  } else {
    b.build_unconditional_branch(symbol_branches_start);
  }

  b.position_at_end(symbol_branches_start);

  let (cp_val, int_type) = match mode.as_str() {
    InputType::PRODUCTION_STR => {
      let val = CtxAggregateIndices::prod_id.load(b, p_ctx)?.into_int_value();
      (val, i32)
    }

    InputType::TOKEN_STR => {
      construct_scan(
        &args,
        p_ctx,
        state_fun,
        *args.state_lu.get(&ParseState::get_scanner_name_from_matches(matches, args.db))?,
      )?;
      let val = CtxAggregateIndices::tok_id.load(b, p_ctx)?.into_int_value();
      (val, i32)
    }

    InputType::BYTE_STR => {
      is_raw_char = true;
      CtxAggregateIndices::sym_len.store(b, p_ctx, u32_1)?;
      let scan_ptr_cache = CtxAggregateIndices::scan_ptr.load(b, p_ctx)?.into_pointer_value();
      (b.build_load(scan_ptr_cache, "").into_int_value(), i8)
    }

    InputType::CODEPOINT_STR => {
      is_raw_char = true;
      let scan_ptr_cache = CtxAggregateIndices::scan_ptr.load(b, p_ctx)?.into_pointer_value();
      let (cp_val, tok_len) = construct_cp_lu_with_token_len_store(args, scan_ptr_cache)?;
      CtxAggregateIndices::sym_len.store(b, p_ctx, tok_len)?;

      (cp_val, i32)
    }

    InputType::CLASS_STR => {
      is_raw_char = true;
      let scan_ptr_cache = CtxAggregateIndices::scan_ptr.load(b, p_ctx)?.into_pointer_value();
      let (cp_val, tok_len) = construct_cp_lu_with_token_len_store(args, scan_ptr_cache)?;
      CtxAggregateIndices::sym_len.store(b, p_ctx, tok_len)?;
      let cp_val = build_fast_call(b, args.m.fun.get_token_class_from_codepoint, &[cp_val.into()])?
        .try_as_basic_value()
        .unwrap_left()
        .into_int_value();

      (cp_val, i32)
    }
    InputType::END_OF_FILE_STR => {
      let val = CtxAggregateIndices::tok_id.load(b, p_ctx)?.into_int_value();
      (val, i32)
    }
    _ => unreachable!(),
  };

  let mut default_defined = false;
  let mut branches = Array::new();
  let mut pending_build = Array::new();

  for _match in matches {
    match _match {
      parser::ASTNode::DefaultMatch(def) => {
        let stmt = def.statement.as_ref();
        pending_build.push((default_block, Some(stmt), false));
        default_defined = true;
      }
      parser::ASTNode::IntMatch(int_match) => {
        let mut is_nl = false;
        let statement = &int_match.statement;
        let branch_block = ctx.append_basic_block(state_fun, "branch");
        for val in &int_match.vals {
          // If we are working with character values (Byte or Codepoint)
          // add line increment if value is 10 (ASCII Line Feed)
          if is_raw_char && (*val == 5 || *val == 10) {
            is_nl = true;
          }
          branches.push((int_type.const_int(*val, false), branch_block));
        }
        pending_build.push((branch_block, Some(statement.as_ref()), is_nl));
      }
      _ => {}
    }
  }

  if !default_defined {
    pending_build.push((default_block, None, false));
  };

  b.build_switch(cp_val, default_block, &branches);

  for (block, maybe_stmt, is_new_line) in pending_build {
    b.position_at_end(block);
    if let Some(stmt) = maybe_stmt {
      if (is_new_line) {
        prime_line_data(args, p_ctx, i8.const_int(1, false));
      }
      compile_statement(args, stmt, p_ctx, state_fun, start_block)?;
    } else {
      fail(args, p_ctx, state_fun);
    }
  }

  SherpaResult::Ok(())
}

fn prime_line_data<'a, 'llvm: 'a>(
  args: &BuildArgs<'a, 'llvm>,
  p: PointerValue<'llvm>,
  increment_amount: IntValue<'llvm>,
) -> SherpaResult<()> {
  let LLVMParserModule { ctx, b, .. } = args.m;
  let i64 = ctx.i64_type();
  let i32 = ctx.i32_type();

  let beg = b.build_ptr_to_int(CTX::beg_ptr.load(b, p)?.into_pointer_value(), i64, "");

  let scan = b.build_ptr_to_int(CTX::scan_ptr.load(b, p)?.into_pointer_value(), i64, "");

  let val = b.build_int_sub(scan, beg, "");
  let val = b.build_int_truncate(val, i32, "");

  CTX::end_line_off.store(b, p, val)?;
  CTX::line_num_incr.store(b, p, increment_amount)?;

  SherpaResult::Ok(())
}

fn construct_cp_lu_with_token_len_store<'a, 'llvm: 'a>(
  args: &BuildArgs<'a, 'llvm>,
  buffer: PointerValue<'llvm>,
) -> SherpaResult<(IntValue<'llvm>, IntValue<'llvm>)> {
  let LLVMParserModule { b, fun, .. } = args.m;
  let cp_info = build_fast_call(b, fun.get_utf8_codepoint_info, &[buffer.into()])?
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();

  let cp_val = b.build_extract_value(cp_info, 0, "cp_val")?.into_int_value();
  let cp_byte_len = b.build_extract_value(cp_info, 1, "cp_len")?.into_int_value();

  SherpaResult::Ok((cp_val, cp_byte_len))
}

fn goto<'a, 'llvm: 'a>(
  sherpa_core::parser::Goto { prod }: &sherpa_core::parser::Goto,
  args: &BuildArgs<'a, 'llvm>,
  state_fun: FunctionValue<'llvm>,
) -> SherpaResult<()> {
  build_tail_call_with_return(
    &args.m.b,
    state_fun,
    *args.state_lu.get(&prod.to_token().to_string())?,
  )
}

fn push_goto<'a, 'llvm: 'a>(
  parser::Push { prod, .. }: &parser::Push,
  args: &BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  add_goto_slot(
    args.m,
    p_ctx,
    (*args.state_lu.get(&prod.to_token().to_string())?).as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM as u64,
  );

  SherpaResult::Ok(())
}

fn add_goto_slot<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
  goto_fn: PointerValue<'a>,
  goto_state_val: u64,
) -> SherpaResult<()> {
  let LLVMParserModule { b, ctx, i32, types, .. } = m;
  let goto_state = ctx.i32_type().const_int(goto_state_val, false);

  // Get the top slot ptr ptr;
  let goto_top = CtxAggregateIndices::goto_stack_ptr.load(b, p_ctx)?.into_pointer_value();

  // Create new goto struct
  let new_goto = b.build_insert_value(types.goto.get_undef(), goto_state, 1, "")?;
  let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "")?;

  // Store in the current slot
  b.build_store(goto_top, new_goto);

  // Increment the slot
  let goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false)], "") };

  // Store the top slot pointer
  CtxAggregateIndices::goto_stack_ptr.store(b, p_ctx, goto_top)?;

  SherpaResult::Ok(())
}

/// Subtracts `goto_slots_consumed` from the `goto_free` counter.
fn update_goto_remaining<'a>(
  goto_slots_consumed: usize,
  m: &'a LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, .. } = m;
  // Decrement goto remaining
  let goto_free_ptr = CtxAggregateIndices::goto_free.get_ptr(b, p_ctx)?;
  let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
  let goto_free = b.build_int_sub(goto_free, i32.const_int(goto_slots_consumed as u64, false), "");
  b.build_store(goto_free_ptr, goto_free);
  SherpaResult::Ok(())
}

pub(crate) fn ensure_space_on_goto_stack<'a>(
  needed_slot_count: usize,
  m: &'a LLVMParserModule,
  p_ctx: PointerValue,
  state_fn: FunctionValue,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, ctx, i32, .. } = m;
  // Compare the number of needed slots with the number of available slots
  let goto_free = CtxAggregateIndices::goto_free.load(b, p_ctx)?.into_int_value();

  let needed_slots = i32.const_int(needed_slot_count as u64, false);
  let comparison = b.build_int_compare(inkwell::IntPredicate::UGE, goto_free, needed_slots, "");

  let extend_block = ctx.append_basic_block(state_fn, "extend_goto_stack");
  let ready_block = ctx.append_basic_block(state_fn, "valid_goto_stack");

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

/// Resets any peek side-effects and builds a tail call return to the
/// `dispatch_unwind` function.
fn fail<'a, 'llvm: 'a>(
  args: &BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue,
  state_fun: FunctionValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, i32, .. } = args.m;
  peek_reset(args, p_ctx)?;
  transfer_start_line_to_end_line(args, p_ctx)?;
  b.build_store(
    CtxAggregateIndices::state.get_ptr(b, p_ctx)?,
    i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false),
  );
  build_tail_call_with_return(b, state_fun, fun.dispatch_unwind)
}

/// Builds a normal return with the Accept event.
fn accept(args: &BuildArgs) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, .. } = args.m;
  b.build_return(Some(&i32.const_int(ParseActionType::Accept.into(), false)));
  SherpaResult::Ok(())
}

/// Transfers the `end_line*` values to the `chkp_line*` variables.
fn transfer_end_line_to_chkp_line(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, .. } = args.m;
  CtxAggregateIndices::chkp_line_num.store(
    b,
    p_ctx,
    CtxAggregateIndices::end_line_num.load(b, p_ctx)?.into_int_value(),
  )?;
  CtxAggregateIndices::chkp_line_off.store(
    b,
    p_ctx,
    CtxAggregateIndices::end_line_off.load(b, p_ctx)?.into_int_value(),
  )?;
  SherpaResult::Ok(())
}

/// Transfers the difference between `scan_ptr` and `head_ptr` to `tok_len`.
/// If `token_value` is not 0, then assigns it's value to `tok_id`.
fn construct_assign_token_id(
  args: &BuildArgs,
  p_ctx: PointerValue,
  token_value: u64,
) -> SherpaResult<()> {
  let LLVMParserModule { b, iptr, i32, .. } = args.m;
  if token_value > 0 {
    CtxAggregateIndices::tok_id.store(b, p_ctx, i32.const_int(token_value as u64, false))?;
  }

  let len = b.build_int_sub(
    CtxAggregateIndices::scan_ptr.load_ptr_as_int(b, p_ctx, *iptr)?,
    CtxAggregateIndices::head_ptr.load_ptr_as_int(b, p_ctx, *iptr)?,
    "",
  );

  CtxAggregateIndices::tok_len.store(b, p_ctx, len)?;

  SherpaResult::Ok(())
}

fn reduce<'a, 'llvm: 'a>(
  parser::ReduceRaw { rule_id, prod_id, len, .. }: &parser::ReduceRaw,
  args: &BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue<'llvm>,
  no_reenter: bool,
) -> SherpaResult<Option<(FunctionValue<'llvm>, PointerValue<'llvm>)>> {
  const __HINT__: Opcode = Opcode::Reduce;

  let LLVMParserModule { b, ctx, fun, i32, .. } = args.m;

  CtxAggregateIndices::sym_len.store(b, p_ctx, i32.const_int(*len as u64, false))?;
  CtxAggregateIndices::rule_id.store(b, p_ctx, i32.const_int(*rule_id as u64, false))?;
  CtxAggregateIndices::prod_id.store(b, p_ctx, i32.const_int(*prod_id as u64, false))?;

  let f = if !no_reenter {
    let f = create_parse_function(args.j, args.m, &format!("reduce_{}", args.state_name));

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

fn create_parse_function<'a>(
  j: &Journal,
  m: &LLVMParserModule<'a>,
  name: &str,
) -> FunctionValue<'a> {
  let linkage = if j.config().opt_llvm { Some(Linkage::Private) } else { None };
  m.module.add_function(&name, m.types.tail_callable_parse_function, linkage)
}

fn construct_token_shift<'a, 'llvm: 'a>(
  args: &BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue<'llvm>,
) -> SherpaResult<Option<(FunctionValue<'llvm>, PointerValue<'llvm>)>> {
  let LLVMParserModule { b, i32, fun, .. } = args.m;

  let post_shift = create_parse_function(args.j, args.m, "post-shift");

  build_fast_call(b, fun.pre_shift_emit, &[
    p_ctx.into(),
    post_shift.as_global_value().as_pointer_value().into(),
  ])?;

  b.build_return(Some(&i32.const_int(ParseActionType::Shift.into(), false)));

  b.position_at_end(args.m.ctx.append_basic_block(post_shift, "entry"));
  let p_ctx = post_shift.get_first_param()?.into_pointer_value();

  SherpaResult::Ok(Some((post_shift, p_ctx)))
}

fn transfer_chkp_line_to_start_line(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, .. } = args.m;
  CtxAggregateIndices::start_line_num.store(
    b,
    p_ctx,
    CtxAggregateIndices::chkp_line_num.load(b, p_ctx)?.into_int_value(),
  )?;
  CtxAggregateIndices::start_line_off.store(
    b,
    p_ctx,
    CtxAggregateIndices::chkp_line_off.load(b, p_ctx)?.into_int_value(),
  )?;
  SherpaResult::Ok(())
}

/// Assigns start line trackers to  tail trackers
fn transfer_start_line_to_end_line(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, .. } = args.m;
  CtxAggregateIndices::end_line_num.store(
    b,
    p_ctx,
    CtxAggregateIndices::start_line_num.load(b, p_ctx)?.into_int_value(),
  )?;
  CtxAggregateIndices::end_line_off.store(
    b,
    p_ctx,
    CtxAggregateIndices::start_line_off.load(b, p_ctx)?.into_int_value(),
  )?;
  SherpaResult::Ok(())
}

/// Resets peek side-effects by assigning `base_ptr` to `head_ptr` and
/// `scan_ptr`. Also
fn peek_reset(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, i8, iptr, .. } = args.m;
  const __HINT__: Opcode = Opcode::PeekReset;
  let offset = CtxAggregateIndices::base_ptr.load(b, p_ctx)?.into_pointer_value();
  CtxAggregateIndices::head_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::scan_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::tok_id.store(b, p_ctx, i32.const_zero());
  CtxAggregateIndices::tok_len.store(b, p_ctx, iptr.const_zero());
  CtxAggregateIndices::sym_len.store(b, p_ctx, i32.const_zero());
  CtxAggregateIndices::line_num_incr.store(b, p_ctx, i8.const_zero());
  SherpaResult::Ok(())
}

/// Update the `end_line*`  values by incrementing by the `line_nume+incr` and
/// `end_line_num`
fn update_end_line_data(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, i8, .. } = args.m;
  // Increment line number;
  let val = CtxAggregateIndices::line_num_incr.load(b, p_ctx)?.into_int_value();
  CtxAggregateIndices::line_num_incr.store(b, p_ctx, i8.const_zero())?;

  let new_val = b.build_int_add(
    CtxAggregateIndices::end_line_num.load(b, p_ctx)?.into_int_value(),
    b.build_int_z_extend(val, *i32, ""),
    "",
  );
  CtxAggregateIndices::end_line_num.store(b, p_ctx, new_val)?;

  SherpaResult::Ok(())
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
fn incr_scan_ptr_by_sym_len(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, .. } = args.m;
  let scan_ptr_cache = CtxAggregateIndices::scan_ptr.load(b, p_ctx)?.into_pointer_value();

  CtxAggregateIndices::scan_ptr.store(b, p_ctx, unsafe {
    b.build_gep(
      scan_ptr_cache,
      &[CtxAggregateIndices::sym_len.load(b, p_ctx)?.into_int_value()],
      "",
    )
  });

  SherpaResult::Ok(())
}

/// Assigns `head_ptr + tok_len` to `head_ptr`, `scan_ptr`, and `base_ptr`.
/// Also assign __0__ to `tok_id`
fn skip_token(args: &BuildArgs, p_ctx: PointerValue, state_fun: FunctionValue) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::SkipToken;
  let LLVMParserModule { b, i32, fun, ctx, .. } = args.m;

  let skip_fun = create_parse_function(args.j, args.m, "skip");

  build_fast_call(b, fun.push_state, &[
    p_ctx.into(),
    i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false).into(),
    skip_fun.as_global_value().as_pointer_value().into(),
  ])?;

  b.build_return(Some(&i32.const_int(ParseActionType::Skip.into(), false)));

  b.position_at_end(ctx.append_basic_block(skip_fun, "entry"));
  let p_ctx = skip_fun.get_first_param()?.into_pointer_value();

  let offset = get_offset_to_end_of_token(args.m, p_ctx)?;
  CTX::scan_ptr.store(b, p_ctx, offset);
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());

  build_tail_call_with_return(b, skip_fun, state_fun)?;

  validate(skip_fun)
}

/// Calculates an offset to the end of the current token.
/// `offset_ptr = head_ptr + tok_len`. This also sets the
/// `tok_len` to 0.
fn get_offset_to_end_of_token<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<PointerValue<'a>> {
  let LLVMParserModule { b, i8, iptr, .. } = m;
  let head_ptr = CtxAggregateIndices::head_ptr.load(b, p_ctx)?.into_pointer_value();
  let tok_len = CtxAggregateIndices::tok_len.load(b, p_ctx)?.into_int_value();
  let offset = b.build_ptr_to_int(head_ptr, *iptr, "");
  let offset = b.build_int_add(offset, tok_len, "");
  let offset = b.build_int_to_ptr(offset, i8.ptr_type(0.into()), "");
  CtxAggregateIndices::tok_len.store(b, p_ctx, iptr.const_zero())?;
  SherpaResult::Ok(offset)
}

/// Causes the execution to jump back to the beginning of the `loop_start`
/// block.
fn construct_jump_to_table_start(args: &BuildArgs, start_block: BasicBlock) {
  args.m.b.build_unconditional_branch(start_block);
}

fn pop_goto(args: &BuildArgs, p_ctx: PointerValue, state_fun: FunctionValue) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::PopGoto;

  add_goto_pop_instructions(args.m, p_ctx)?;

  pass(args, p_ctx, state_fun)
}

/// Builds a tail call return to the `dispatch` function.
fn pass(args: &BuildArgs, p_ctx: PointerValue, state_fun: FunctionValue) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, i32, .. } = args.m;
  b.build_store(
    CtxAggregateIndices::state.get_ptr(b, p_ctx)?,
    i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false),
  );
  build_tail_call_with_return(b, state_fun, fun.dispatch)
}

/// Assigns the value head_ptr + tok_len to head_ptr and scan_ptr.
/// Also sets the values of tok_len and tok_id to 0.
pub(crate) fn peek_token(args: &BuildArgs, p_ctx: PointerValue) -> SherpaResult<()> {
  const __HINT__: Opcode = Opcode::PeekToken;
  let LLVMParserModule { b, i32, iptr, .. } = args.m;
  let offset = get_offset_to_end_of_token(args.m, p_ctx)?;
  CtxAggregateIndices::scan_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::head_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::tok_id.store(b, p_ctx, i32.const_zero());
  CtxAggregateIndices::tok_len.store(b, p_ctx, iptr.const_zero());
  SherpaResult::Ok(())
}

fn build_push_fn_state<'a>(
  m: &'a LLVMParserModule,
  parse_ctx: PointerValue<'a>,
  function_ptr: PointerValue<'a>,
  state_mask: u32,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, i32, .. } = m;
  b.build_call(
    fun.push_state,
    &[parse_ctx.into(), i32.const_int(state_mask as u64, false).into(), function_ptr.into()],
    "",
  );

  SherpaResult::Ok(())
}

fn scan_is_less_than_end<'a, 'llvm: 'a>(
  args: &BuildArgs<'a, 'llvm>,
  p_ctx: PointerValue<'llvm>,
) -> SherpaResult<IntValue<'llvm>> {
  let LLVMParserModule { b, iptr, .. } = args.m;
  let scan_int = b.build_ptr_to_int(
    CtxAggregateIndices::scan_ptr.load(b, p_ctx)?.into_pointer_value(),
    *iptr,
    "",
  );
  let end_int = b.build_ptr_to_int(
    CtxAggregateIndices::end_ptr.load(b, p_ctx)?.into_pointer_value(),
    *iptr,
    "",
  );
  SherpaResult::Ok(b.build_int_compare(
    inkwell::IntPredicate::ULT,
    scan_int,
    end_int,
    "scan_is_less_than_end",
  ))
}

pub(crate) fn check_for_input_acceptability<'a, 'llvm: 'a>(
  args: &BuildArgs<'a, 'llvm>,
  state_fun: FunctionValue<'llvm>,
  p_ctx: PointerValue<'llvm>,
  needed_num_bytes: IntValue<'llvm>,
  valid_input: BasicBlock<'llvm>,
  invalid_input: BasicBlock<'llvm>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, ctx, bool, .. } = args.m;
  let i64 = ctx.i64_type();

  // Set the context's goto pointers to point to the goto block;
  let try_extend = ctx.append_basic_block(state_fun, "attempt_extend");
  let check_if_eof = ctx.append_basic_block(state_fun, "check_if_eof");

  let c = scan_is_less_than_end(args, p_ctx)?;
  b.build_conditional_branch(c, valid_input, check_if_eof);

  // Check to see if the EOF flag is set
  // -------------------------------------------------
  b.position_at_end(check_if_eof);
  let input_complete = CtxAggregateIndices::block_is_eoi.load(b, p_ctx)?.into_int_value();
  let c =
    b.build_int_compare(inkwell::IntPredicate::EQ, input_complete, bool.const_int(1, false), "");
  b.build_conditional_branch(c, invalid_input, try_extend);

  // Try to get another block of input data
  // ----------------------------------------------
  b.position_at_end(try_extend);
  build_fast_call(b, fun.get_adjusted_input_block, &[p_ctx.into(), needed_num_bytes.into()]);

  let input_available = get_chars_remaining(args.m, p_ctx)?;
  let input_available = b.build_int_cast(input_available, i64, "");
  let c = b.build_int_compare(inkwell::IntPredicate::SGE, input_available, needed_num_bytes, "");

  /// Return the difference between the `scan_ptr` and `end_ptr`.
  /// Value type is `iptr`
  pub(crate) fn get_chars_remaining<'a>(
    sp: &'a LLVMParserModule,
    p_ctx: PointerValue<'a>,
  ) -> SherpaResult<IntValue<'a>> {
    let LLVMParserModule { b, iptr, .. } = sp;
    let scan_int = b.build_ptr_to_int(
      CtxAggregateIndices::scan_ptr.load(b, p_ctx)?.into_pointer_value(),
      *iptr,
      "",
    );
    let end_int = b.build_ptr_to_int(
      CtxAggregateIndices::end_ptr.load(b, p_ctx)?.into_pointer_value(),
      *iptr,
      "",
    );
    SherpaResult::Ok(b.build_int_sub(end_int, scan_int, "").into())
  }

  // TODO:
  // If input is bad then return a NEEDED_INPUT event, after pushing the current
  // state to the stack. This should pause the parser until some external
  // signal indicates more data is available and parsing can be resumed.

  b.build_conditional_branch(c, valid_input, invalid_input);

  SherpaResult::Ok(())
}

pub(crate) fn construct_scan(
  args: &BuildArgs,
  p_ctx: PointerValue,
  state_fn: FunctionValue,
  scan_fn: FunctionValue,
) -> SherpaResult<()> {
  // The scan mode reuses the parse context, and pushes a sentinel state
  // that guards against the scanner context consuming the states of the normal
  // context.

  let LLVMParserModule { b, fun, ctx, .. } = args.m;

  // Build a null function that will be used to break out of the scanner
  // context dispatch loop

  // Push the scanner sentinel state and the entry scanner state onto
  // the goto stack.

  let local_state = CtxAggregateIndices::state.load(b, p_ctx)?;

  let block = ctx.append_basic_block(state_fn, "scan");
  b.build_unconditional_branch(block);
  b.position_at_end(block);

  ensure_space_on_goto_stack(2, args.m, p_ctx, state_fn)?;
  add_goto_slot(
    args.m,
    p_ctx,
    args.scan_stop.as_global_value().as_pointer_value(),
    (NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64,
  )?;
  add_goto_slot(
    args.m,
    p_ctx,
    scan_fn.as_global_value().as_pointer_value(),
    (NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM) as u64,
  )?;
  update_goto_remaining(2, args.m, p_ctx)?;

  // Dispatch!
  b.build_call(fun.dispatch, &[p_ctx.into()], "");

  // Ensure the the local context state is reset.
  CtxAggregateIndices::state.store(b, p_ctx, local_state);

  // Reset scanner back to head
  CtxAggregateIndices::scan_ptr.store(
    b,
    p_ctx,
    CtxAggregateIndices::head_ptr.load(b, p_ctx)?.into_pointer_value(),
  );

  SherpaResult::Ok(())
}

/// The prime function's purpose is fist create a base goto state that emits a
/// failure to prevent stack underflow when descending to the bottom of the goto
/// stack, and also insert the first GOTO entry that will initiate the parser to
/// start parsing based on an entry production id.
pub(crate) fn construct_prime_function(
  j: &mut Journal,
  db: &ParserDatabase,
  sp: &LLVMParserModule,
  state_lu: &BTreeMap<String, FunctionValue>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, bool, .. } = sp;
  let funct = &sp.fun;
  let fn_value = funct.prime;

  let parse_ctx = fn_value.get_nth_param(0).unwrap().into_pointer_value();
  // Set the context's goto pointers to point to the goto block;
  let selector = fn_value.get_nth_param(1).unwrap().into_int_value();

  b.position_at_end(sp.ctx.append_basic_block(fn_value, "entry"));

  let options = db
    .entry_points()
    .iter()
    .filter_map(|p| {
      let name = p.prod_name.to_string(db.string_store());
      let fun = state_lu.get(&name).unwrap();
      let block = sp.ctx.append_basic_block(fn_value, &format!("prime_{}", name));
      Some((p.export_id, block, fun))
    })
    .collect::<Vec<_>>();

  build_fast_call(b, funct.extend_stack, &[parse_ctx.into(), i32.const_int(8, false).into()]);

  CtxAggregateIndices::is_active.store(b, parse_ctx, bool.const_int(1, false))?;

  // Push the End-Of-Parse goto onto the stack. This will prevent underflow of
  // the stack
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

/// Dispatched after a shift event, that is this should be pushed onto the goto
/// stack before emitting a shift event.
pub(crate) fn construct_shift_post_emit<'a>(m: &'a LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, fun, .. } = m;
  let fn_value = fun.post_shift_emit;

  b.position_at_end(m.ctx.append_basic_block(fn_value, "entry"));

  let p_ctx = fn_value.get_first_param()?.into_pointer_value();

  let offset = get_offset_to_end_of_token(m, p_ctx)?;
  CtxAggregateIndices::scan_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::head_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::base_ptr.store(b, p_ctx, offset);
  CtxAggregateIndices::tok_id.store(b, p_ctx, i32.const_zero());

  build_tail_call_with_return(b, fn_value, fun.dispatch)?;

  validate(fn_value)
}

/// Dispatched after a shift event, that is this should be pushed onto the goto
/// stack before emitting a shift event.
pub(crate) fn construct_shift_pre_emit<'a>(m: &'a LLVMParserModule) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, .. } = m;
  let fn_value = fun.pre_shift_emit;

  b.position_at_end(m.ctx.append_basic_block(fn_value, "entry"));

  let p_ctx = fn_value.get_first_param()?.into_pointer_value();
  let goto_fn = fn_value.get_last_param()?.into_pointer_value();

  ensure_space_on_goto_stack(2, m, p_ctx, fn_value)?;
  add_goto_slot(m, p_ctx, goto_fn.into(), NORMAL_STATE_FLAG_LLVM as u64);
  add_goto_slot(
    m,
    p_ctx,
    fun.post_shift_emit.as_global_value().as_pointer_value().into(),
    NORMAL_STATE_FLAG_LLVM as u64,
  );
  update_goto_remaining(2, m, p_ctx)?;

  b.build_return(None);

  validate(fn_value)
}
