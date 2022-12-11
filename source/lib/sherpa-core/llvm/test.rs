use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore},
  llvm::{compile_from_bytecode, test_reader::TestUTF8StringReader},
  Journal,
  SherpaResult,
};
use inkwell::{context::Context, execution_engine::JitFunction};
use sherpa_runtime::types::{
  sherpa_allocate_stack,
  sherpa_free_stack,
  Goto,
  InputBlock,
  LLVMParseContext,
  ParseAction,
  ParseToken,
  LLVM_BASE_STACK_SIZE,
};
use std::{fs::File, io::Write};

use super::{inkwell_ir::*, types::*};

type Init = unsafe extern "C" fn(
  *mut LLVMParseContext<TestUTF8StringReader<'static>>,
  *mut TestUTF8StringReader<'static>,
);
type PushState =
  unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>, u32, usize);

type EmitReduce = unsafe extern "C" fn(
  *mut LLVMParseContext<TestUTF8StringReader<'static>>,
  *mut ParseAction,
  u32,
  u32,
  u32,
) -> u32;

type EmitAccept = unsafe extern "C" fn(
  *mut LLVMParseContext<TestUTF8StringReader<'static>>,
  *mut ParseAction,
) -> u32;

type EmitShift = EmitAccept;

type Next =
  unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>, *mut ParseAction);

type Prime = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>, u32);

type Extend = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>, u32);

type PopState = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>>) -> Goto;

unsafe fn get_parse_function<'a, T: inkwell::execution_engine::UnsafeFunctionPointer>(
  ctx: &'a LLVMParserModule,
  function_name: &str,
) -> Result<JitFunction<'a, T>, ()> {
  let init = ctx
    .exe_engine
    .as_ref()
    .unwrap()
    .get_function::<T>(function_name)
    .ok()
    .ok_or("Failed To Compile")
    .unwrap();

  Ok(init)
}

fn setup_exec_engine(ctx: &mut LLVMParserModule) {
  if ctx.exe_engine.is_none() {
    ctx.exe_engine =
      Some(ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive).unwrap());
  }
}

#[test]
fn verify_construction_of_init_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construction_of_push_state_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_push_state(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_push_new_state() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) }
  unsafe { assert!(construct_push_state(&parse_context).is_ok()) }

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();
    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
    let push_state_fn = get_parse_function::<PushState>(&parse_context, "push_state").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    push_state_fn.call(&mut rt_ctx, NORMAL_STATE_FLAG_LLVM, 0x10101010_01010101);
    push_state_fn.call(&mut rt_ctx, NORMAL_STATE_FLAG_LLVM, 0x01010101_10101010);

    eprintln!("{:#?}", rt_ctx);
    assert_eq!(rt_ctx.local_goto_stack[0].goto_fn as usize, 0x10101010_01010101);
    assert_eq!(rt_ctx.local_goto_stack[0].state, NORMAL_STATE_FLAG_LLVM);

    assert_eq!(rt_ctx.local_goto_stack[1].goto_fn as usize, 0x01010101_10101010);
    assert_eq!(rt_ctx.local_goto_stack[1].state, NORMAL_STATE_FLAG_LLVM);
  };
}

#[test]
fn verify_construction_of_emit_accept_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_accept(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construction_of_emit_shift_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_shift(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_emit_shift() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_shift(&parse_context).is_ok()) }

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();
    let emit_shift = get_parse_function::<EmitShift>(&parse_context, "emit_shift").unwrap();

    rt_ctx.anchor_token.byte_offset = 5;
    rt_ctx.anchor_token.byte_length = 0;
    rt_ctx.assert_token.byte_offset = 10;

    let mut action = ParseAction::Undefined;

    emit_shift.call(&mut rt_ctx, &mut action);

    match action {
      ParseAction::Shift { skipped_characters, token } => {
        assert_eq!(skipped_characters.byte_length, 5);
        assert_eq!(skipped_characters.byte_offset, 5);
        assert_eq!(token.byte_offset, 10);
      }
      _ => panic!("Incorrect ParseAction enum type assigned"),
    }
  };
}

#[test]
fn verify_construction_of_emit_reduce_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_reduce_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_emit_reduce() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_reduce_function(&parse_context).is_ok()) }

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();
    let emit_reduce = get_parse_function::<EmitReduce>(&parse_context, "emit_reduce").unwrap();

    let mut action = ParseAction::Undefined;

    emit_reduce.call(&mut rt_ctx, &mut action, 1, 2, 3);

    match action {
      ParseAction::Reduce { production_id, rule_id, symbol_count } => {
        assert_eq!(production_id, 1);
        assert_eq!(rule_id, 2);
        assert_eq!(symbol_count, 3);
      }
      _ => panic!("Incorrect ParseAction enum type assigned"),
    }
  };
}

#[test]
fn verify_construction_of_get_adjusted_input_block_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_get_adjusted_input_block_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_produce_extended_block() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) }
  unsafe { assert!(construct_get_adjusted_input_block_function(&parse_context).is_ok()) }

  // Create a helper function to overcome the struct passing as value between the Jit code and Rust

  use inkwell::AddressSpace::*;

  let shim = parse_context.module.add_function(
    "shim",
    context.void_type().fn_type(
      &[
        parse_context.types.parse_ctx.ptr_type(Generic).into(),
        context.i32_type().into(),
        context.i32_type().into(),
        parse_context.types.input_block.ptr_type(Generic).into(),
      ],
      false,
    ),
    None,
  );
  parse_context.builder.position_at_end(context.append_basic_block(shim, "Entry"));
  let input_block = parse_context
    .builder
    .build_call(
      parse_context.fun.get_adjusted_input_block,
      &[
        shim.get_nth_param(0).unwrap().into_pointer_value().into(),
        shim.get_nth_param(1).unwrap().into_int_value().into(),
        shim.get_nth_param(2).unwrap().into_int_value().into(),
      ],
      "",
    )
    .try_as_basic_value()
    .unwrap_left()
    .into_struct_value();

  parse_context
    .builder
    .build_store(shim.get_nth_param(3).unwrap().into_pointer_value(), input_block);

  parse_context.builder.build_return(None);

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();
    let init = get_parse_function::<Init>(&parse_context, "init").unwrap();

    type GetInputBlockShim = unsafe extern "C" fn(
      *mut LLVMParseContext<TestUTF8StringReader<'static>>,
      u32,
      u32,
      *mut InputBlock,
    );

    let get_ib = get_parse_function::<GetInputBlockShim>(&parse_context, "shim").unwrap();

    init.call(&mut rt_ctx, &mut reader);

    let mut token = rt_ctx.anchor_token;

    token.byte_offset = 3;

    println!(
      "context:{:p}, fn:{:p} off:{:X}",
      &rt_ctx,
      &rt_ctx.get_byte_block_at_cursor,
      (&rt_ctx.get_byte_block_at_cursor as *const _) as usize - (&rt_ctx as *const _) as usize
    );

    let mut block = InputBlock::default();

    println!(
      "context:{:p}, fn:{:p} off:{:X}",
      &rt_ctx,
      &rt_ctx.get_byte_block_at_cursor,
      (&rt_ctx.get_byte_block_at_cursor as *const _) as usize - (&rt_ctx as *const _) as usize
    );

    println!("context:{:p} block:{:p} token:{:p}", &mut rt_ctx, &mut block, &token);
    get_ib.call(&mut rt_ctx, 3, 2, &mut block);

    eprintln!("{:?} {:?}", rt_ctx.input_block, block);
    assert_eq!(*block.block, b't');
    assert_eq!(block.start, 3);
    assert_eq!(block.end, 4);
  };
}

#[test]
fn verify_construction_of_scan_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_scan(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construction_of_emit_error_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_error(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construction_of_emit_eop_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_end_of_parse(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construction_of_emit_end_of_input_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_emit_end_of_input(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construction_of_pop_state_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_pop_state_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_pop_new_state() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) }
  unsafe { assert!(construct_push_state(&parse_context).is_ok()) }
  unsafe { assert!(construct_pop_state_function(&parse_context).is_ok()) }

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();
    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
    let push_state_fn = get_parse_function::<PushState>(&parse_context, "push_state").unwrap();
    let pop_state = get_parse_function::<PopState>(&parse_context, "pop_state").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    push_state_fn.call(&mut rt_ctx, 20, 0x10101010_01010101);
    push_state_fn.call(&mut rt_ctx, 40, 0x10101010_01010101);
    assert_eq!(rt_ctx.goto_stack_remaining, 6);

    let second = pop_state.call(&mut rt_ctx);
    let first = pop_state.call(&mut rt_ctx);

    assert_eq!(second.state, 40);
    assert_eq!(first.state, 20);
    assert_eq!(rt_ctx.goto_stack_remaining, 8);

    eprintln!("{:#?}", rt_ctx);
  };
}
#[test]
fn verify_construct_of_prime_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_prime_function(&parse_context, &vec![], &mut vec![]).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn verify_construct_of_next_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_next_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_call_next_and_emit_accept() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) }
  unsafe { assert!(construct_push_state(&parse_context).is_ok()) }
  unsafe { assert!(construct_pop_state_function(&parse_context).is_ok()) }
  unsafe { assert!(construct_next_function(&parse_context).is_ok()) }
  unsafe { assert!(construct_emit_accept(&parse_context).is_ok()) }
  unsafe { assert!(construct_emit_end_of_parse(&parse_context).is_ok()) }
  unsafe { assert!(construct_prime_function(&parse_context, &vec![], &mut vec![]).is_ok()) }

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();
    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
    let push_state_fn = get_parse_function::<PushState>(&parse_context, "push_state").unwrap();
    let next = get_parse_function::<Next>(&parse_context, "next").unwrap();
    let emit_accept = get_parse_function::<EmitAccept>(&parse_context, "emit_accept").unwrap();
    let prime = get_parse_function::<Prime>(&parse_context, "prime").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    prime.call(&mut rt_ctx, 0);

    rt_ctx.production = 202020;

    let mut action = ParseAction::Undefined;

    next.call(&mut rt_ctx, &mut action);

    eprintln!("{:#?}", action);

    assert!(matches!(action, ParseAction::Accept { production_id } if production_id == 202020),);
  };
}

#[test]
fn should_initialize_context() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) };

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = Box::new(LLVMParseContext::new());
    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();

    init_fn.call(rt_ctx.as_mut(), &mut reader);

    let root = rt_ctx.as_ref() as *const LLVMParseContext<TestUTF8StringReader<'static>> as usize;

    assert_eq!(rt_ctx.goto_stack_ptr as usize, root);
    assert_eq!(rt_ctx.goto_stack_remaining as usize, 8);
    assert_eq!(rt_ctx.goto_stack_size as usize, 8);
    assert_eq!(rt_ctx.state, NORMAL_STATE_FLAG_LLVM);

    eprintln!("{:?}:{:#?}", root, rt_ctx);
  };
}

#[test]
fn verify_utf8_lookup_functions() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_utf8_lookup(&parse_context).is_ok()) }
  unsafe { assert!(construct_merge_utf8_part(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_yield_correct_CP_values_for_inputs() {
  let context = Context::create();
  let mut parse_context = construct_context("test", &context);

  assert!(construct_utf8_lookup(&parse_context).is_ok());
  unsafe { assert!(construct_merge_utf8_part(&parse_context).is_ok()) }

  unsafe {
    #[derive(Clone, Debug, Copy)]
    #[repr(C)]
    struct CodepointInfo {
      pub val: u32,
      pub len: u32,
    }

    type GetUtf8CP = unsafe extern "C" fn(*const u8) -> CodepointInfo;

    setup_exec_engine(&mut parse_context);

    let get_code_point =
      get_parse_function::<GetUtf8CP>(&parse_context, "get_utf8_codepoint_info").unwrap();

    dbg!(get_code_point.call(" ".as_ptr()));
    assert_eq!(get_code_point.call(" ".as_ptr()).val, 32);
    // assert_eq!(get_code_point.call(" ".as_ptr()).len, 1);
    assert_eq!(get_code_point.call("☺".as_ptr()).val, 0x263A);
    // assert_eq!(get_code_point.call("☺".as_ptr()).len, 3);
  };
}

#[test]
fn verify_construct_extend_stack_if_needed() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_extend_stack_if_needed(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_extend_stack() {
  let context = Context::create();

  let mut parse_context = construct_context("test", &context);

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();

    unsafe { assert!(construct_init(&parse_context).is_ok()) }
    unsafe { assert!(construct_push_state(&parse_context).is_ok()) }
    unsafe { assert!(construct_extend_stack_if_needed(&parse_context).is_ok()) }

    parse_context
      .exe_engine
      .as_ref()
      .unwrap()
      .add_global_mapping(&parse_context.fun.allocate_stack, sherpa_allocate_stack as usize);

    parse_context
      .exe_engine
      .as_ref()
      .unwrap()
      .add_global_mapping(&parse_context.fun.free_stack, sherpa_free_stack as usize);

    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();
    let push_state_fn = get_parse_function::<PushState>(&parse_context, "push_state").unwrap();
    let extend = get_parse_function::<Extend>(&parse_context, "extend_stack_if_needed").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    push_state_fn.call(&mut rt_ctx, 10, 200);
    push_state_fn.call(&mut rt_ctx, 30, 400);
    extend.call(&mut rt_ctx, 10);

    assert_eq!(rt_ctx.goto_stack_size, (18 << 3));

    push_state_fn.call(&mut rt_ctx, 50, 600);
    push_state_fn.call(&mut rt_ctx, 70, 800);

    extend.call(&mut rt_ctx, 200);

    assert_eq!(rt_ctx.goto_stack_size - rt_ctx.goto_stack_remaining, 4);

    for v in 4..(34 << 1) {
      push_state_fn.call(&mut rt_ctx, v, v as usize);
    }

    assert_eq!(rt_ctx.goto_stack_size - rt_ctx.goto_stack_remaining, (34 << 1));

    let stack = unsafe {
      let ptr = (rt_ctx.goto_stack_ptr as usize)
        - (((rt_ctx.goto_stack_size as usize) - (rt_ctx.goto_stack_remaining as usize)) << 4);
      std::slice::from_raw_parts(ptr as *const Goto, rt_ctx.goto_stack_size as usize)
    };

    assert_eq!(stack[67].state, 67);

    extend.call(&mut rt_ctx, 2);

    assert_eq!(rt_ctx.goto_stack_size, 2752);

    eprintln!("{:#?}", stack);
  };
}

#[test]
fn test_compile_from_bytecode() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
  @IGNORE g:sp
 
  <> test > \\hello \\world
  ",
  )
  .unwrap();
  let ir_states = compile_states(&mut j, 1)?;
  let ir_states = optimize_ir_states(&mut j, ir_states);
  let bytecode_output = compile_bytecode(&mut j, ir_states);

  if let SherpaResult::Ok(mut ctx) =
    compile_from_bytecode("test", &g, &Context::create(), &bytecode_output)
  {
    let mut file = File::create("../test.ll");

    if let Ok(mut file) = file {
      file.write_all(ctx.module.to_string().as_bytes());

      unsafe {
        setup_exec_engine(&mut ctx);
        let mut reader = TestUTF8StringReader::new("hello world");
        let mut rt_ctx = LLVMParseContext::new();

        ctx
          .exe_engine
          .as_ref()
          .unwrap()
          .add_global_mapping(&ctx.fun.allocate_stack, sherpa_allocate_stack as usize);

        ctx
          .exe_engine
          .as_ref()
          .unwrap()
          .add_global_mapping(&ctx.fun.free_stack, sherpa_free_stack as usize);

        let init_fn = get_parse_function::<Init>(&ctx, "init").unwrap();
        let prime_fn = get_parse_function::<Prime>(&ctx, "prime").unwrap();
        let next_fn = get_parse_function::<Next>(&ctx, "next").unwrap();

        init_fn.call(&mut rt_ctx, &mut reader);

        prime_fn.call(&mut rt_ctx, 0);

        let mut action = ParseAction::Undefined;

        next_fn.call(&mut rt_ctx, &mut action);

        assert!(
          matches!(action, ParseAction::Shift {  token, .. } if token.byte_offset == 0 && token.byte_length == 5)
        );

        next_fn.call(&mut rt_ctx, &mut action);

        assert!(matches!(action, ParseAction::Shift {  token,skipped_characters  } if
          (token.byte_offset == 6 && token.byte_length == 5)
          &&
          (skipped_characters.byte_length == 1 && skipped_characters.byte_offset == 5 )
        ));

        next_fn.call(&mut rt_ctx, &mut action);

        assert!(
          matches!(action, ParseAction::Reduce { production_id, symbol_count, .. } if production_id == 0 && symbol_count == 2)
        );

        next_fn.call(&mut rt_ctx, &mut action);

        assert!(matches!(action, ParseAction::Accept { .. }));
      };

      SherpaResult::Ok(())
    } else {
      drop(ctx);
      SherpaResult::None
    }
  } else {
    SherpaResult::None
  }
}

#[test]
fn test_compile_from_bytecode2() -> SherpaResult<()> {
  use crate::llvm::compile_from_bytecode;
  use inkwell::context::Context;
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
      @IGNORE g:sp

      @EXPORT statement as entry
      
      @NAME llvm_language_test
      
      <> statement > expression
      
      <> expression > sum 
      
      <> sum > mul \\+ sum
          | mul
      
      <> mul > term \\* expression
          | term
      
      <> term > g:num
          | \\( expression \\)
      
      
",
  )
  .unwrap();

  let mut ir_states = compile_states(&mut j, 1)?;
  let ir_states = optimize_ir_states(&mut j, ir_states);
  let bytecode_output = compile_bytecode(&mut j, ir_states);

  if let SherpaResult::Ok(mut ctx) =
    compile_from_bytecode("test", &g, &Context::create(), &bytecode_output)
  {
    println!("{:#?}", ctx);
    SherpaResult::Ok(())
  } else {
    SherpaResult::None
  }
}
