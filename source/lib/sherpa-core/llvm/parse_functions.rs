use super::{
  build_fast_call,
  build_tail_call_with_return,
  fastCC,
  validate,
  CTX_AGGREGATE_INDICES as CTX,
  NORMAL_STATE_FLAG_LLVM,
};
use crate::{
  grammar::compile::parser::sherpa::{self, *},
  llvm::{LLVMParserModule, FAIL_STATE_FLAG_LLVM},
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
use sherpa_runtime::utf8::lookup_table::CodePointClass;
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

  if state.is_except_handler {
    // Add a check to make sure the context is in fail mode.
    let normal_mode = ctx.append_basic_block(s_fun, "is_normal_mode");

    let state = CTX::state.load(b, p_ctx)?.into_int_value();
    let fail_int = i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false);
    let c = b.build_int_compare(inkwell::IntPredicate::EQ, state, fail_int, "");
    b.build_conditional_branch(c, loop_head, normal_mode);

    b.position_at_end(normal_mode);
    construct_pass(m, p_ctx, s_fun)?;
  } else {
    b.build_unconditional_branch(loop_head);
  }

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
        compile_brancheless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
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
        compile_brancheless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
      }

      b.position_at_end(default_block);
      last_block = default_block;
    }

    if groups.keys().any(|k| matches!(k.as_str(), "BYTE" | "CODEPOINT" | "CLASS")) {
      // If the block size is less than 1 then jump to default
      let active_block = last_block;
      let branch_block = ctx.append_basic_block(s_fun, "table_branches");
      last_block = branch_block;
      b.position_at_end(branch_block);

      let tail_ptr_cache = CTX::tail_ptr.load(b, p_ctx)?.into_pointer_value();

      if let Some(byte_branches) = groups.get("BYTE") {
        let (mut cases, branches) = deconstruct_branches("byte_", byte_branches, m, s_fun, i8)?;

        let value = b.build_load(tail_ptr_cache, "").into_int_value();

        CTX::symbol_len.store(b, p_ctx, u32_1)?;

        // if one of the cases is the val `[\n] == 10` then we want to
        // increment the line data. We do this by creating a new block
        // that is `increment_line_data`, and set the target of this block
        // to the existing block of the case.

        cases.iter_mut().filter(|(i, ..)| *i == 10).for_each(|i| {
          create_line_increment(m, s_fun, p_ctx, i8.const_int(1, false), i);
        });

        for (block, i) in branches {
          b.position_at_end(block);
          compile_brancheless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
        }

        b.position_at_end(last_block);

        let default_block = ctx.append_basic_block(s_fun, &format!("byte_default"));
        b.build_switch(value, default_block, &convert_to_case_entry(cases));

        b.position_at_end(default_block);
        last_block = default_block;
      }

      if groups.keys().any(|k| matches!(k.as_str(), "CODEPOINT" | "CLASS")) {
        let (cp_val, tok_len) = construct_cp_lu_with_token_len_store(m, tail_ptr_cache)?;

        CTX::symbol_len.store(b, p_ctx, tok_len)?;

        if let Some(cp_branches) = groups.get("CODEPOINT") {
          let (cases, branches) = deconstruct_branches("codepoint_", cp_branches, m, s_fun, i32)?;

          let default_block = ctx.append_basic_block(s_fun, &format!("codepoint_default"));

          b.build_switch(cp_val, default_block, &convert_to_case_entry(cases));

          for (block, i) in branches {
            b.position_at_end(block);
            compile_brancheless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
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
            compile_brancheless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
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

      b.position_at_end(active_block);
      check_for_input_acceptability(
        m,
        s_fun,
        p_ctx,
        i64.const_int(1, false),
        branch_block,
        last_block,
      )?;
    }

    b.position_at_end(last_block);

    if let Some(default_branches) = groups.get("DEFAULT") {
      debug_assert!(default_branches.len() == 1);
      let DEFAULT { instructions: i } = default_branches[0].as_DEFAULT()?;
      compile_brancheless_instructions(j, m, &i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
    } else {
      construct_fail(m, p_ctx, s_fun)?;
    }
  } else {
    let i = &state.instructions;
    compile_brancheless_instructions(j, m, i, p_ctx, s_fun, loop_head, s_lu, state_name)?;
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
  let tail = b.build_ptr_to_int(CTX::tail_ptr.load(b, p)?.into_pointer_value(), i64, "");
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

pub(crate) fn compile_brancheless_instructions<'a>(
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
  let pass_last = matches!(instructions.last()?.get_type(), ASTNodeType::Pass);

  for (i, instruction) in instructions.iter().filter(|i| !matches!(i, ASTNode::Goto(_))).enumerate()
  {
    let is_last = i == instructions.len() - 1;
    let no_reenter = is_last && pass_last;
    match instruction {
      ASTNode::Fail(_) => {
        construct_fail(m, p_ctx, state_fun)?;
        resolved_end = true;
        break;
      }

      ASTNode::ForkTo(box ForkTo { .. }) => {
        unimplemented!("Fork not implemented");
      }

      ASTNode::Pass(_) => {
        construct_pass(m, p_ctx, state_fun)?;
        resolved_end = true;
        break;
      }

      ASTNode::Pop(_) => {
        construct_pop(m, p_ctx, state_fun)?;
        resolved_end = true;
        break;
      }

      ASTNode::SetProd(box SetProd { id: box Num { val } }) => {
        construct_set_prod(j, m, state_fun, p_ctx, *val as u64)?
      }

      ASTNode::PeekReset(_) => {
        construct_peek_reset(m, p_ctx)?;
        construct_update_remaining(m, p_ctx)?;
      }

      ASTNode::PeekScanToken(_) => {
        construct_update_tail(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0)?;
        construct_increment_remaining(m, p_ctx)?;
        construct_peek(m, p_ctx)?;
      }

      ASTNode::PeekToken(_) => {
        construct_peek(m, p_ctx)?;
        construct_update_remaining(m, p_ctx)?;
      }

      ASTNode::Skip(box Skip { is_peek }) => {
        construct_skip(m, p_ctx, *is_peek)?;
        construct_update_remaining(m, p_ctx)?;
        construct_jump_to_table_start(m, start_block);
        resolved_end = true;
        break;
      }

      ASTNode::SkipScanToken(box SkipScanToken { is_peek }) => {
        construct_update_tail(m, p_ctx)?;
        construct_increment_remaining(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0);
        construct_skip(m, p_ctx, *is_peek)?;
        construct_jump_to_table_start(m, start_block);
        resolved_end = true;
        break;
      }

      ASTNode::TokenAssign(box TokenAssign { ids }) => {
        construct_assign_token_id(m, p_ctx, ids[0].val as u64)?;
      }

      ASTNode::ShiftScanner(..) => {
        construct_update_tail(m, p_ctx)?;
        construct_increment_remaining(m, p_ctx)?;
      }

      ASTNode::ShiftScanToken(_) => {
        construct_update_tail(m, p_ctx)?;
        construct_increment_remaining(m, p_ctx)?;
        construct_assign_token_id(m, p_ctx, 0)?;
        if let Some((s, p)) = construct_token_shift(j, m, p_ctx)? {
          state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
      }

      ASTNode::ShiftToken(box ShiftToken { .. }) => {
        if let Some((s, p)) = construct_token_shift(j, m, p_ctx)? {
          state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
        construct_update_remaining(m, p_ctx);
      }

      ASTNode::Reduce(box Reduce { rule_id, len, .. }) => {
        let mut hasher = DefaultHasher::new();
        instructions[i..].hash(&mut hasher);
        if let Some((s, p)) =
          construct_reduce(j, m, p_ctx, *len as u64, *rule_id as u64, no_reenter, state_name)?
        {
          state_lu.insert(s.get_name().to_str().unwrap().to_string(), s);
          state_fun = s;
          p_ctx = p;
        }
      }
      _ => {}
    }
  }

  let gotos = instructions
    .iter()
    .filter_map(|i| match i {
      ASTNode::Goto(goto) => Some(goto),
      _ => None,
    })
    .rev()
    .collect::<Vec<_>>();

  let resolved_end = if !gotos.is_empty() {
    construct_goto(gotos, m, p_ctx, state_fun, state_lu);
    true
  } else {
    resolved_end
  };

  if !resolved_end {
    construct_pass(m, p_ctx, state_fun);
  }

  SherpaResult::Ok(())
}

fn construct_jump_to_table_start(m: &LLVMParserModule, start_block: BasicBlock) {
  m.b.build_unconditional_branch(start_block);
}
pub(crate) fn construct_update_tail<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, i8, .. } = m;

  let tail_ptr_cache = CTX::tail_ptr.load(b, p_ctx)?.into_pointer_value();

  CTX::tail_ptr.store(b, p_ctx, unsafe {
    b.build_gep(tail_ptr_cache, &[CTX::symbol_len.load(b, p_ctx)?.into_int_value()], "")
  });

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

pub(crate) fn construct_pop<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
  state_fun: FunctionValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, .. } = m;

  let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(b, p_ctx)?;
  let goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();
  let goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false).const_neg()], "") };
  b.build_store(goto_stack_ptr, goto_top);

  let goto_free_ptr = CTX::goto_free.get_ptr(b, p_ctx)?;
  let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
  let goto_free = b.build_int_add(goto_free, i32.const_int(1, false), "");
  b.build_store(goto_free_ptr, goto_free);

  construct_pass(m, p_ctx, state_fun)
}

pub(crate) fn construct_reduce<'a>(
  j: &mut Journal,
  m: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
  sym_count: u64,
  rule_id: u64,
  no_reenter: bool,
  state_name: &str,
) -> SherpaResult<Option<(FunctionValue<'a>, PointerValue<'a>)>> {
  let LLVMParserModule { b, ctx, fun, i32, .. } = m;

  CTX::symbol_len.store(b, p_ctx, i32.const_int(sym_count, false))?;
  CTX::rule_id.store(b, p_ctx, i32.const_int(rule_id, false))?;

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

pub(crate) fn construct_set_prod<'a>(
  j: &mut Journal,
  LLVMParserModule { ctx, b, i32, .. }: &'a LLVMParserModule,
  func: FunctionValue<'a>,
  p_ctx: PointerValue,
  production_id: u64,
) -> SherpaResult<()> {
  let block = ctx.append_basic_block(
    func,
    &format!(
      "set_production_to_{}",
      j.grammar()?.get_production_by_bytecode_id(production_id as u32)?.name
    ),
  );
  b.build_unconditional_branch(block);
  b.position_at_end(block);

  CTX::prod_id.store(b, p_ctx, i32.const_int(production_id, false))?;
  SherpaResult::Ok(())
}

pub(crate) fn construct_peek_reset<'a>(
  LLVMParserModule { b, i32, .. }: &'a LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  let offset = CTX::base_ptr.load(b, p_ctx)?.into_pointer_value();
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::tail_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());
  SherpaResult::Ok(())
}

pub(crate) fn construct_peek<'a>(m: &'a LLVMParserModule, p_ctx: PointerValue) -> SherpaResult<()> {
  let LLVMParserModule { b, i32, .. } = m;
  let offset = construct_get_tok_offset(m, p_ctx)?;
  CTX::tail_ptr.store(b, p_ctx, offset);
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());
  SherpaResult::Ok(())
}

/// Update the remaining chars value by calculating its
/// value using tail_ptr and end_ptr
pub(crate) fn construct_update_remaining(
  sp: &LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  let LLVMParserModule { b, iptr, .. } = sp;
  let tail_int = b.build_ptr_to_int(CTX::tail_ptr.load(b, p_ctx)?.into_pointer_value(), *iptr, "");
  let end_int = b.build_ptr_to_int(CTX::end_ptr.load(b, p_ctx)?.into_pointer_value(), *iptr, "");
  CTX::chars_remaining_len.store(b, p_ctx, b.build_int_sub(end_int, tail_int, ""))?;
  SherpaResult::Ok(())
}
/// Update the remaining chars value by incrementing it by
/// the value of symbol_len
pub(crate) fn construct_increment_remaining(
  LLVMParserModule { b, iptr, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
) -> SherpaResult<()> {
  let sym_len = CTX::symbol_len.load(b, p_ctx)?.into_int_value();
  let sym_len = b.build_int_cast(sym_len, *iptr, "");
  let char_remaining = CTX::chars_remaining_len.load(b, p_ctx)?.into_int_value();
  CTX::chars_remaining_len.store(b, p_ctx, b.build_int_add(char_remaining, sym_len, ""))?;

  SherpaResult::Ok(())
}

pub(crate) fn construct_skip<'a>(
  sp: &LLVMParserModule,
  p_ctx: PointerValue,
  is_peek: bool,
) -> SherpaResult<()> {
  let offset = construct_get_tok_offset(sp, p_ctx)?;

  let LLVMParserModule { b, i32, .. } = sp;
  CTX::tail_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());
  CTX::head_ptr.store(b, p_ctx, offset);

  if !is_peek {
    CTX::base_ptr.store(b, p_ctx, offset);
    CTX::start_line_num.store(b, p_ctx, CTX::chkp_line_num.load(b, p_ctx)?.into_int_value())?;
    CTX::start_line_off.store(b, p_ctx, CTX::chkp_line_off.load(b, p_ctx)?.into_int_value())?;
  }
  SherpaResult::Ok(())
}

pub(crate) fn construct_get_tok_offset<'a>(
  sp: &'a LLVMParserModule,
  p_ctx: PointerValue<'a>,
) -> SherpaResult<PointerValue<'a>> {
  let LLVMParserModule { b, i8, iptr, .. } = sp;
  let offset = CTX::head_ptr.load(b, p_ctx)?.into_pointer_value();
  let tok_len = CTX::tok_len.load(b, p_ctx)?.into_int_value();
  let offset = b.build_ptr_to_int(offset, *iptr, "");
  let offset = b.build_int_add(offset, tok_len, "");
  CTX::tok_len.store(b, p_ctx, iptr.const_zero())?;
  let offset = b.build_int_to_ptr(offset, i8.ptr_type(0.into()), "");
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

  let offset = construct_get_tok_offset(m, p_ctx)?;
  CTX::tail_ptr.store(b, p_ctx, offset);
  CTX::anchor_ptr.store(b, p_ctx, offset);
  CTX::head_ptr.store(b, p_ctx, offset);
  CTX::base_ptr.store(b, p_ctx, offset);
  CTX::tok_id.store(b, p_ctx, i32.const_zero());

  SherpaResult::Ok(Some((fun, p_ctx)))
}

fn construct_assign_token_id(
  LLVMParserModule { b, iptr, i32, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  token_value: u64,
) -> SherpaResult<()> {
  if token_value > 0 {
    CTX::tok_id.store(b, p_ctx, i32.const_int(token_value as u64, false))?;
  }

  let len = b.build_int_sub(
    CTX::tail_ptr.load_ptr_as_int(b, p_ctx, *iptr)?,
    CTX::head_ptr.load_ptr_as_int(b, p_ctx, *iptr)?,
    "",
  );
  CTX::tok_len.store(b, p_ctx, len)?;

  CTX::chkp_line_num.store(b, p_ctx, CTX::end_line_num.load(b, p_ctx)?.into_int_value())?;
  CTX::chkp_line_off.store(b, p_ctx, CTX::end_line_off.load(b, p_ctx)?.into_int_value())?;
  SherpaResult::Ok(())
}

pub(crate) fn construct_pass<'a>(
  LLVMParserModule { b, fun, i32, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  state_fun: FunctionValue,
) -> SherpaResult<()> {
  b.build_store(CTX::state.get_ptr(b, p_ctx)?, i32.const_int(NORMAL_STATE_FLAG_LLVM as u64, false));
  build_tail_call_with_return(b, state_fun, fun.dispatch)
}

pub(crate) fn construct_fail<'a>(
  m: &'a LLVMParserModule,
  p_ctx: PointerValue,
  state_fun: FunctionValue<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, i32, .. } = m;

  construct_peek_reset(m, p_ctx);

  b.build_store(CTX::state.get_ptr(b, p_ctx)?, i32.const_int(FAIL_STATE_FLAG_LLVM as u64, false));

  build_tail_call_with_return(b, state_fun, fun.dispatch_unwind)
}

pub(crate) fn construct_goto<'a>(
  mut gotos: Vec<&Box<sherpa::Goto>>,
  LLVMParserModule { b, fun, ctx, i32, types, .. }: &LLVMParserModule,
  p_ctx: PointerValue,
  state_fun: FunctionValue<'a>,
  state_lu: &BTreeMap<String, FunctionValue>,
) -> SherpaResult<()> {
  let last = gotos.pop();

  if gotos.len() > 0 {
    build_fast_call(b, fun.extend_stack_if_needed, &[
      p_ctx.into(),
      i32.const_int((2 << gotos.len().checked_ilog2().unwrap_or(0)) as u64, false).into(),
    ])?;

    let goto_stack_ptr = CTX::goto_stack_ptr.get_ptr(b, p_ctx)?;
    let mut goto_top = b.build_load(goto_stack_ptr, "").into_pointer_value();

    for box sherpa::Goto { state } in &gotos {
      // Create new goto struct
      let goto_fn = (*state_lu.get(&state.val)?).as_global_value().as_pointer_value();
      let goto_state = ctx.i32_type().const_int(NORMAL_STATE_FLAG_LLVM as u64, false);
      let new_goto = b.build_insert_value(types.goto.get_undef(), goto_state, 1, "")?;
      let new_goto = b.build_insert_value(new_goto, goto_fn, 0, "")?;

      // Store in the current slot
      b.build_store(goto_top, new_goto);

      // Increment the slot
      goto_top = unsafe { b.build_gep(goto_top, &[i32.const_int(1, false)], "") };
    }

    // Store the top slot
    b.build_store(goto_stack_ptr, goto_top);

    // Decrement goto remaining
    let goto_free_ptr = CTX::goto_free.get_ptr(b, p_ctx)?;
    let goto_free = b.build_load(goto_free_ptr, "").into_int_value();
    let goto_free = b.build_int_sub(goto_free, i32.const_int(gotos.len() as u64, false), "");
    b.build_store(goto_free_ptr, goto_free);
  }

  if let Some(box sherpa::Goto { state }) = last {
    build_tail_call_with_return(b, state_fun, *state_lu.get(&state.val)?);
  }

  SherpaResult::Ok(())
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
  m: &LLVMParserModule,
  state_fun: FunctionValue<'a>,
  p_ctx: PointerValue<'a>,
  needed_num_bytes: IntValue<'a>,
  valid_input: BasicBlock<'a>,
  invalid_input: BasicBlock<'a>,
) -> SherpaResult<()> {
  let LLVMParserModule { b, fun, ctx, .. } = m;
  let i64 = ctx.i64_type();

  // Set the context's goto pointers to point to the goto block;
  let try_extend = ctx.append_basic_block(state_fun, "attempt_extend");

  let input_available = CTX::chars_remaining_len.load(b, p_ctx)?.into_int_value();
  let input_available = b.build_int_cast(input_available, i64, "");
  let c = b.build_int_compare(inkwell::IntPredicate::SGE, input_available, needed_num_bytes, "");
  b.build_conditional_branch(c, valid_input, try_extend);

  b.position_at_end(try_extend);
  build_fast_call(b, fun.get_adjusted_input_block, &[p_ctx.into(), needed_num_bytes.into()]);
  construct_update_remaining(m, p_ctx);

  let input_available = CTX::chars_remaining_len.load(b, p_ctx)?.into_int_value();
  let input_available = b.build_int_cast(input_available, i64, "");
  let c = b.build_int_compare(inkwell::IntPredicate::SGE, input_available, needed_num_bytes, "");

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

  build_push_fn_state(
    sp,
    p_ctx,
    null_fn.as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM | FAIL_STATE_FLAG_LLVM,
  );

  build_push_fn_state(
    sp,
    p_ctx,
    scan_fn.as_global_value().as_pointer_value(),
    NORMAL_STATE_FLAG_LLVM,
  );
  // Dispatch!
  b.build_call(fun.dispatch, &[p_ctx.into()], "");

  // Ensure the the local context state is reset.
  CTX::state.store(b, p_ctx, local_state);

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
  let selector = fn_value.get_nth_param(1).unwrap().into_int_value(); // Set the context's goto pointers to point to the goto block;

  b.position_at_end(sp.ctx.append_basic_block(fn_value, "entry"));

  let options = g
    .get_exported_productions()
    .iter()
    .filter_map(|p| match p.production.bytecode_id {
      Some(bc_id) => {
        let name = &p.guid_name.to_string();
        let fun = state_lu.get(name).unwrap();
        let block = sp.ctx.append_basic_block(fn_value, &format!("prime_{}", name));
        Some((bc_id, block, fun))
      }
      None => unreachable!("All exported productions should have a bytecode_id"),
    })
    .collect::<Vec<_>>();

  build_fast_call(b, funct.extend_stack_if_needed, &[
    parse_ctx.into(),
    i32.const_int(8, false).into(),
  ]);

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

  if funct.prime.verify(true) {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Could not build prime function"))
  }
}
