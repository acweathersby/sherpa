use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore},
  journal,
  llvm::{
    ascript_functions::construct_ast_builder,
    compile_from_bytecode,
    parser_functions::construct_parse_function,
    *,
  },
  Journal,
  SherpaResult,
};
use inkwell::{context::Context, execution_engine::JitFunction};
use sherpa_runtime::{
  types::{
    llvm_map_result_action,
    llvm_map_shift_action,
    sherpa_allocate_stack,
    sherpa_free_stack,
    sherpa_get_token_class_from_codepoint,
    AstSlots,
    BlameColor,
    ByteReader,
    Goto,
    InputInfo,
    InputType,
    LLVMParseContext,
    ParseActionType,
    ParseResult,
    Token,
    TokenRange,
  },
  utf8::lookup_table::CodePointClass,
};
use std::{fs::File, io::Write};

use super::test_reader::TestUTF8StringReader;

type Init = unsafe extern "C" fn(
  *mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
  *mut TestUTF8StringReader<'static>,
);
type PushState =
  unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>, u32>, u32, usize);

type Next = unsafe extern "C" fn(
  *mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
) -> ParseActionType;

type Prime = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>, u32>, u32);

type Extend = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>, u32>, u32);

type Drop = unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>, u32>);

type TailCallFunction =
  unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>, u32>) -> u32;

type PopState =
  unsafe extern "C" fn(*mut LLVMParseContext<TestUTF8StringReader<'static>, u32>) -> Goto;

unsafe fn get_parse_function<'a, T: inkwell::execution_engine::UnsafeFunctionPointer>(
  ctx: &'a LLVMParserModule,
  function_name: &str,
) -> Result<JitFunction<'a, T>, ()> {
  let init = ctx
    ._exe_engine
    .as_ref()
    .unwrap()
    .get_function::<T>(function_name)
    .ok()
    .ok_or("Failed To Compile")
    .unwrap();

  Ok(init)
}

fn setup_exec_engine(ctx: &mut LLVMParserModule) {
  if ctx._exe_engine.is_none() {
    ctx._exe_engine =
      Some(ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive).unwrap());
  }
}

fn build_fast_call_shim<'a>(
  module: &LLVMParserModule<'a>,
  fun: inkwell::values::FunctionValue<'a>,
) -> SherpaResult<()> {
  let name = fun.get_name().to_str().unwrap();
  let builder = &module.builder;
  let shim = module.module.add_function(&format!("{}_shim", name), fun.get_type(), None);
  shim.set_call_conventions(7);
  builder.position_at_end(module.ctx.append_basic_block(shim, "Entry"));

  let result = build_fast_call(
    module,
    fun,
    &shim.get_params().into_iter().map(|i| i.into()).collect::<Vec<_>>(),
  )?;

  match result.try_as_basic_value().left() {
    Some(value) => builder.build_return(Some(&value)),
    _ => builder.build_return(None),
  };

  SherpaResult::Ok(())
}

#[test]
fn verify_construction_of_init_function() {
  let context = Context::create();

  let module = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_init(&module).is_ok()) }

  eprintln!("{}", module.module.to_string());
}

#[test]
fn verify_construction_of_ast_builder() {
  let context = Context::create();

  let module = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_ast_builder::<u64>(&module).is_ok()) }

  eprintln!("{}", module.module.to_string());
}

#[test]
fn verify_construction_of_push_state_function() {
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_push_state_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_push_new_state() -> SherpaResult<()> {
  let context = Context::create();

  let mut module = construct_module(&mut Journal::new(None), "test", &context);

  unsafe {
    setup_exec_engine(&mut module);

    module
      ._exe_engine
      .as_ref()
      .unwrap()
      .add_global_mapping(&module.fun.allocate_stack, sherpa_allocate_stack as usize);

    module
      ._exe_engine
      .as_ref()
      .unwrap()
      .add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);

    construct_init(&module)?;
    construct_push_state_function(&module)?;
    construct_extend_stack_if_needed(&module)?;
    construct_internal_free_stack(&module)?;
    construct_drop(&module)?;

    build_fast_call_shim(&module, module.fun.push_state)?;
    build_fast_call_shim(&module, module.fun.extend_stack_if_needed)?;

    let mut reader = TestUTF8StringReader::new("test");

    let mut rt_ctx = LLVMParseContext::new();

    let push_state_fn = get_parse_function::<PushState>(&module, "push_state_shim").unwrap();

    let init_fn = get_parse_function::<Init>(&module, "init").unwrap();

    let extend_stack_if_needed =
      get_parse_function::<Extend>(&module, "extend_stack_if_needed_shim").unwrap();

    let drop_fn = get_parse_function::<Drop>(&module, "drop").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    extend_stack_if_needed.call(&mut rt_ctx, 8);

    push_state_fn.call(&mut rt_ctx, NORMAL_STATE_FLAG_LLVM, 0x10101010_01010101);
    push_state_fn.call(&mut rt_ctx, NORMAL_STATE_FLAG_LLVM, 0x01010101_10101010);

    let stack = std::slice::from_raw_parts(
      {
        (rt_ctx.goto_stack_ptr as usize - ((rt_ctx.goto_size - rt_ctx.goto_free) << 4) as usize)
          as *mut Goto
      },
      rt_ctx.goto_size as usize,
    );

    eprintln!("{:#?}", rt_ctx);
    assert_eq!(stack[0].goto_fn as usize, 0x10101010_01010101);
    assert_eq!(stack[0].state, NORMAL_STATE_FLAG_LLVM);

    assert_eq!(stack[1].goto_fn as usize, 0x01010101_10101010);
    assert_eq!(stack[1].state, NORMAL_STATE_FLAG_LLVM);

    drop_fn.call(&mut rt_ctx);

    assert_eq!(rt_ctx.goto_size, 0);
  };
  SherpaResult::Ok(())
}

#[test]
fn verify_construction_of_get_adjusted_input_block_function() {
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_get_adjusted_input_block_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_produce_extended_block() {
  let context = Context::create();

  let mut parse_context = construct_module(&mut Journal::new(None), "test", &context);

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
        parse_context.types.input_info.ptr_type(Generic).into(),
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
      *mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
      u32,
      u32,
      *mut InputInfo,
    );

    let get_ib = get_parse_function::<GetInputBlockShim>(&parse_context, "shim").unwrap();

    init.call(&mut rt_ctx, &mut reader);

    rt_ctx.token_off = 3;

    eprintln!(
      "context:{:p}, fn:{:p} off:{:X}",
      &rt_ctx,
      &rt_ctx.get_input_info,
      (&rt_ctx.get_input_info as *const _) as usize - (&rt_ctx as *const _) as usize
    );

    /*     let mut block = InputInfo::default();

    eprintln!(
      "context:{:p}, fn:{:p} off:{:X}",
      &rt_ctx,
      &rt_ctx.get_input_info,
      (&rt_ctx.get_input_info as *const _) as usize - (&rt_ctx as *const _) as usize
    );

    eprintln!("context:{:p} block:{:p} token:{}", &rt_ctx, &mut block, &rt_ctx.token_off);
    get_ib.call(&mut rt_ctx, 3, 2, &mut block);

    //eprintln!("{:?} {:?}", rt_ctx.input_block, block);
    assert_eq!(*block.block, b't');
    assert_eq!(block.start, 3);
    assert_eq!(block.end, 4); */
  };
}

/* #[test]
fn verify_construction_of_scan_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_scan(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
} */

#[test]
fn verify_construction_of_emit_eop_function() {
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_emit_end_of_parse(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

/* #[test]
fn verify_construct_of_prime_function() {
  let context = Context::create();

  let parse_context = construct_context("test", &context);

  unsafe { assert!(construct_prime_function(&parse_context, &vec![], &mut vec![]).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
} */

#[test]
fn should_initialize_context() {
  let context = Context::create();

  let mut parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) };

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = Box::new(LLVMParseContext::new());
    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();

    init_fn.call(rt_ctx.as_mut(), &mut reader);

    let root =
      rt_ctx.as_ref() as *const LLVMParseContext<TestUTF8StringReader<'static>, u32> as usize;

    assert_eq!(rt_ctx.goto_stack_ptr as usize, root);
    assert_eq!(rt_ctx.goto_free as usize, 8);
    assert_eq!(rt_ctx.goto_size as usize, 8);
    assert_eq!(rt_ctx.state, NORMAL_STATE_FLAG_LLVM);

    eprintln!("{:?}:{:#?}", root, rt_ctx);
  };
}

#[test]
fn verify_utf8_lookup_functions() {
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_utf8_lookup_function(&parse_context).is_ok()) }
  unsafe { assert!(construct_merge_utf8_part_function(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_yield_correct_CP_values_for_inputs() {
  let context = Context::create();
  let mut module = construct_module(&mut Journal::new(None), "test", &context);

  assert!(construct_utf8_lookup_function(&module).is_ok());
  unsafe { assert!(construct_merge_utf8_part_function(&module).is_ok()) }

  unsafe {
    #[derive(Clone, Debug, Copy)]
    #[repr(C)]
    struct CodepointInfo {
      pub val: u32,
      pub len: u32,
    }

    type GetUtf8CP = unsafe extern "C" fn(*const u8) -> CodepointInfo;

    build_fast_call_shim(&module, module.fun.get_utf8_codepoint_info);

    setup_exec_engine(&mut module);

    let get_code_point =
      get_parse_function::<GetUtf8CP>(&module, "get_utf8_codepoint_info_shim").unwrap();

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

  let module = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_extend_stack_if_needed(&module).is_ok()) }

  eprintln!("{}", module.module.to_string());
}

#[test]
fn should_extend_stack() -> SherpaResult<()> {
  let context = Context::create();

  let mut module = construct_module(&mut Journal::new(None), "test", &context);

  unsafe {
    setup_exec_engine(&mut module);

    build_fast_call_shim(&module, module.fun.push_state);
    build_fast_call_shim(&module, module.fun.extend_stack_if_needed);

    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = LLVMParseContext::new();

    construct_init(&module)?;
    construct_push_state_function(&module)?;
    construct_extend_stack_if_needed(&module)?;
    construct_internal_free_stack(&module)?;

    module
      ._exe_engine
      .as_ref()?
      .add_global_mapping(&module.fun.allocate_stack, sherpa_allocate_stack as usize);

    module
      ._exe_engine
      .as_ref()?
      .add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);

    let init_fn = get_parse_function::<Init>(&module, "init").unwrap();
    let push_state_fn = get_parse_function::<PushState>(&module, "push_state_shim").unwrap();
    let extend = get_parse_function::<Extend>(&module, "extend_stack_if_needed_shim").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    push_state_fn.call(&mut rt_ctx, 10, 200);
    push_state_fn.call(&mut rt_ctx, 30, 400);
    extend.call(&mut rt_ctx, 10);

    assert_eq!(rt_ctx.goto_size, (18 << 2));

    push_state_fn.call(&mut rt_ctx, 50, 600);
    push_state_fn.call(&mut rt_ctx, 70, 800);

    extend.call(&mut rt_ctx, 200);

    assert_eq!(rt_ctx.goto_size - rt_ctx.goto_free, 4);

    for v in 4..(34 << 1) {
      push_state_fn.call(&mut rt_ctx, v, v as usize);
    }

    assert_eq!(rt_ctx.goto_size - rt_ctx.goto_free, (34 << 1));

    let stack = unsafe {
      let ptr = (rt_ctx.goto_stack_ptr as usize)
        - (((rt_ctx.goto_size as usize) - (rt_ctx.goto_free as usize)) << 4);
      std::slice::from_raw_parts(ptr as *const Goto, rt_ctx.goto_size as usize)
    };

    assert_eq!(stack[67].state, 67);

    extend.call(&mut rt_ctx, 2);

    assert_eq!(rt_ctx.goto_size, 1088);

    eprintln!("{:#?}", stack);
  };

  SherpaResult::Ok(())
}

#[test]
fn test_compile_parse_function() -> SherpaResult<()> {
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
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe {
    construct_parse_function(&mut j, &parse_context, &bytecode_output)?;
  }

  eprintln!("{}", parse_context.module.to_string());

  SherpaResult::Ok(())
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
  )?;

  let ir_states = compile_states(&mut j, 1)?;
  let ir_states = optimize_ir_states(&mut j, ir_states);
  let bytecode_output = compile_bytecode(&mut j, ir_states);
  let ctx = Context::create();

  let mut ctx = compile_from_bytecode("test", &mut j, &ctx, &bytecode_output)?;

  let mut file = File::create("../test.ll")?;
  file.write_all(ctx.module.to_string().as_bytes())?;

  unsafe {
    setup_exec_engine(&mut ctx);
    let mut reader = TestUTF8StringReader::new("hello world");
    let mut rt_ctx = LLVMParseContext::new();

    ctx
      ._exe_engine
      .as_ref()?
      .add_global_mapping(&ctx.fun.allocate_stack, sherpa_allocate_stack as usize);

    ctx._exe_engine.as_ref()?.add_global_mapping(&ctx.fun.free_stack, sherpa_free_stack as usize);

    let init_fn = get_parse_function::<Init>(&ctx, "init")?;
    let prime_fn = get_parse_function::<Prime>(&ctx, "prime")?;
    let next_fn = get_parse_function::<Next>(&ctx, "next")?;

    init_fn.call(&mut rt_ctx, &mut reader);

    prime_fn.call(&mut rt_ctx, 0);

    let action = next_fn.call(&mut rt_ctx);

    dbg!(action);

    assert!(matches!(action, ParseActionType::Shift));

    let action = next_fn.call(&mut rt_ctx);

    dbg!(action);

    assert!(matches!(action, ParseActionType::Shift));

    let action = next_fn.call(&mut rt_ctx);

    assert!(matches!(action, ParseActionType::Reduce));

    let action = next_fn.call(&mut rt_ctx);

    assert!(matches!(action, ParseActionType::Accept));
  };

  SherpaResult::Ok(())
}

type ASTNode = u32;
type ASTSlot = (ASTNode, TokenRange, TokenRange);
type AstBuilder<'a> = unsafe extern "C" fn(
  *mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
  *const fn(ctx: &mut LLVMParseContext<TestUTF8StringReader<'static>, u32>, &mut AstSlots<ASTSlot>),
  unsafe fn(
    &LLVMParseContext<TestUTF8StringReader<'static>, u32>,
    &mut AstSlots<(ASTNode, TokenRange, TokenRange)>,
  ),
  unsafe fn(
    &LLVMParseContext<TestUTF8StringReader<'static>, u32>,
    ParseActionType,
    &mut AstSlots<(ASTNode, TokenRange, TokenRange)>,
  ) -> ParseResult<ASTNode>,
) -> ParseResult<ASTNode>;

#[test]
fn test_compile_from_bytecode1() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
  @IGNORE g:sp g:nl

  <> test > \\hello P 

  <> P > \\world \\goodby B

  <> B > \\mango
  ",
  )?;

  let ir_states = compile_states(&mut j, 1)?;
  let ir_states = optimize_ir_states(&mut j, ir_states);
  let bytecode_output = compile_bytecode(&mut j, ir_states);
  let ctx = Context::create();

  type ASTNode = u32;
  type ASTSlot = (ASTNode, TokenRange, TokenRange);

  let test_functions = [
    |ctx: &mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
     slots: &mut AstSlots<ASTSlot>| {
      let _a = slots.take(0);
      let _b = slots.take(1);

      assert_eq!(_b.1.len(), 18, "Expected the token of [P] to be 18 bytes long");

      let final_token = _a.1 + _b.1;

      eprintln!(
        "{}",
        final_token.to_token(unsafe { (*ctx.reader).get_source() }).blame(
          0,
          0,
          "Completed Parse of [test]",
          BlameColor::RED
        )
      );

      slots.assign(0, (0, final_token, TokenRange::default()));
    },
    |ctx: &mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
     slots: &mut AstSlots<ASTSlot>| {
      eprintln!("<B> 02 - {}  {:#?}", 0, slots);
      // Do nothing
    },
    |ctx: &mut LLVMParseContext<TestUTF8StringReader<'static>, u32>,
     slots: &mut AstSlots<ASTSlot>| {
      eprintln!("<P> 03 - {}  {:#?}", 0, slots);
      let (_, _a, _) = slots.take(0);
      slots.take(1);
      let (_, _c, _) = slots.take(2);
      slots.assign(0, (0, (_a + _c), TokenRange::default()));
    },
  ];

  unsafe {
    let mut module = compile_from_bytecode("test", &mut j, &ctx, &bytecode_output)?;

    construct_ast_builder::<ASTNode>(&module)?;

    setup_exec_engine(&mut module);
    let input = "hello world\ngoodby mango";
    let mut reader = TestUTF8StringReader::new(input);
    let mut rt_ctx = LLVMParseContext::new();

    let mut file = File::create("../test.ll")?;
    file.write_all(module.module.to_string().as_bytes())?;

    module
      ._exe_engine
      .as_ref()?
      .add_global_mapping(&module.fun.allocate_stack, sherpa_allocate_stack as usize);

    module
      ._exe_engine
      .as_ref()?
      .add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);

    let init_fn = get_parse_function::<Init>(&module, "init")?;
    let prime_fn = get_parse_function::<Prime>(&module, "prime")?;
    let ast_builder_fn = get_parse_function::<AstBuilder>(&module, "ast_parse")?;
    let drop_ = get_parse_function::<Drop>(&module, "drop")?;

    init_fn.call(&mut rt_ctx, &mut reader);
    prime_fn.call(&mut rt_ctx, 0);
    let parse_result = ast_builder_fn.call(
      &mut rt_ctx,
      test_functions.as_ptr(),
      llvm_map_shift_action::<TestUTF8StringReader, ASTNode, u32>,
      llvm_map_result_action::<TestUTF8StringReader, ASTNode, u32>,
    );

    drop_.call(&mut rt_ctx);

    eprintln!("{:#?}", parse_result);

    assert!(matches!(parse_result, ParseResult::Complete(..)));

    if let ParseResult::Complete((_, tok, _)) = parse_result {
      assert_eq!(tok.len(), input.len());
    }
  };

  SherpaResult::Ok(())
}

#[test]
fn test_compile_json_parser() -> SherpaResult<()> {
  use crate::llvm::compile_from_bytecode;
  use inkwell::context::Context;
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    r##"
    @IGNORE g:sp g:nl

@EXPORT json as entry

@NAME llvm_language_test

<> json > 
        object                              f:ast { { t_Json, v: $1 } }
        | 
        array                               f:ast { { t_Json, v: $1 } }

<> array > \[  value(*\, )  \]              f:ast { { t_Array, entries: $2 } }

<> object > \{ key_value(*\, ) \}           f:ast { { t_Object, entries: $2 } }

<> key_value > str \: value              f:ast { { t_KeyVal, k:$1, v:$3 } }

<> value > num | bool | str | null

<> null > t:null                            f:ast { { t_Null, v:false } }

<> bool > 
    t:false                                 f:ast { { t_Bool, v:false } }
    |   
    t:true                                  f:ast { { t_Bool, v:true } }

<> str > tk:string                          f:ast { { t_Str } }

<> num > tk:number                          f:ast { { t_Number } }

<> number > ( \+ | \- )? g:num(+) ( \. g:num(+) )? ( ( \e | \E ) ( \+ | \i ) g:num(+) )?

<> string > \" ( g:id | g:sym | g:num | g:sp )(+) \" 
"##,
  )
  .unwrap();

  let mut ir_states = compile_states(&mut j, 1)?;
  let ir_states = optimize_ir_states(&mut j, ir_states);

  let bytecode_output = compile_bytecode(&mut j, ir_states);

  unsafe {
    let ctx = Context::create();

    let module = compile_from_bytecode("test", &mut j, &ctx, &bytecode_output)?;

    eprintln!("{}", module.module.to_string());

    SherpaResult::Ok(())
  }
}
