use super::{test_reader::TestUTF8StringReader, utils::build_grammar_from_str};
use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states, BytecodeOutput, GrammarStore},
  llvm::{
    ascript_functions::construct_ast_builder,
    compile_module_from_bytecode,
    parser_functions::construct_parse_function,
    *,
  },
  types::JitParseContext,
  Journal,
  SherpaResult,
};
use inkwell::{context::Context, execution_engine::JitFunction};
use sherpa_runtime::types::{
  sherpa_allocate_stack,
  sherpa_free_stack,
  AstStackSlice,
  BlameColor,
  ByteReader,
  Goto,
  ParseActionType,
  ParseContext,
  ParseResult,
  TokenRange,
};

type Init<R = TestUTF8StringReader<'static>, T = u32> =
  unsafe extern "C" fn(*mut ParseContext<R, T>, *mut R);
type PushState<R = TestUTF8StringReader<'static>, T = u32> =
  unsafe extern "C" fn(*mut ParseContext<R, T>, u32, usize);

type Next<R = TestUTF8StringReader<'static>, T = u32> =
  unsafe extern "C" fn(*mut ParseContext<R, T>) -> ParseActionType;

type Prime<R = TestUTF8StringReader<'static>, T = u32> =
  unsafe extern "C" fn(*mut ParseContext<R, T>, u32);

type Extend<R = TestUTF8StringReader<'static>, T = u32> =
  unsafe extern "C" fn(*mut ParseContext<R, T>, u32);

type Drop<R = TestUTF8StringReader<'static>, T = u32> =
  unsafe extern "C" fn(*mut ParseContext<R, T>);

type ASTNode = u32;
type ASTSlot = (ASTNode, TokenRange, TokenRange);
type AstBuilder<'a, R = TestUTF8StringReader<'static>, M = ASTNode> =
  unsafe extern "C" fn(
    *mut ParseContext<R, M>,
    *const fn(ctx: &mut ParseContext<R, M>, &mut AstStackSlice<(M, TokenRange, TokenRange)>),
    unsafe fn(&ParseContext<R, M>, &mut AstStackSlice<(M, TokenRange, TokenRange)>),
    unsafe fn(
      &ParseContext<R, M>,
      ParseActionType,
      &mut AstStackSlice<(M, TokenRange, TokenRange)>,
    ) -> ParseResult<M>,
  ) -> ParseResult<M>;

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

    let mut rt_ctx = ParseContext::new_llvm();

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
fn verify_construction_of_emit_eop_function() {
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_emit_end_of_parse(&parse_context).is_ok()) }

  eprintln!("{}", parse_context.module.to_string());
}

#[test]
fn should_initialize_context() {
  let context = Context::create();

  let mut parse_context = construct_module(&mut Journal::new(None), "test", &context);

  unsafe { assert!(construct_init(&parse_context).is_ok()) };

  unsafe {
    setup_exec_engine(&mut parse_context);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = Box::new(ParseContext::new_llvm());
    let init_fn = get_parse_function::<Init>(&parse_context, "init").unwrap();

    init_fn.call(rt_ctx.as_mut(), &mut reader);

    let root = rt_ctx.as_ref() as *const ParseContext<TestUTF8StringReader<'static>, u32> as usize;

    assert_eq!(rt_ctx.reader.as_ref().unwrap().string, reader.string);

    eprintln!("{:?}:{:#?}", root, rt_ctx);
  };
}

#[test]
fn verify_utf8_lookup_functions() {
  let context = Context::create();

  let parse_context = construct_module(&mut Journal::new(None), "test", &context);

  assert!(construct_utf8_lookup_function(&parse_context).is_ok());

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
    let mut rt_ctx = ParseContext::new_llvm();

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
    extend.call(&mut rt_ctx, 10);

    push_state_fn.call(&mut rt_ctx, 10, 200);
    push_state_fn.call(&mut rt_ctx, 30, 400);

    assert_eq!(rt_ctx.goto_size, 40);

    push_state_fn.call(&mut rt_ctx, 50, 600);
    push_state_fn.call(&mut rt_ctx, 70, 800);

    extend.call(&mut rt_ctx, 200);

    assert_eq!(rt_ctx.goto_size - rt_ctx.goto_free, 4);

    for v in 4..(34 << 1) {
      push_state_fn.call(&mut rt_ctx, v, v as usize);
    }

    assert_eq!(rt_ctx.goto_size - rt_ctx.goto_free, (34 << 1));

    let stack = {
      let ptr = (rt_ctx.goto_stack_ptr as usize)
        - (((rt_ctx.goto_size as usize) - (rt_ctx.goto_free as usize)) << 4);
      std::slice::from_raw_parts(ptr as *const Goto, rt_ctx.goto_size as usize)
    };

    assert_eq!(stack[67].state, 67);

    extend.call(&mut rt_ctx, 2);

    assert_eq!(rt_ctx.goto_size, 960);

    eprintln!("{:#?}", stack);
  };

  SherpaResult::Ok(())
}

#[test]
fn test_compile_parse_function() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
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
  let ctx = Context::create();

  let module = construct_module(&mut Journal::new(None), "test", &ctx);

  unsafe {
    construct_parse_function(&mut j, &module, &bytecode_output)?;
  }

  eprintln!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn test_compile_from_bytecode() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
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
  let mut module = construct_module(&mut j, "test", &ctx);
  compile_module_from_bytecode(&mut module, &mut j, &bytecode_output)?;

  unsafe {
    setup_exec_engine(&mut module);
    let mut reader = TestUTF8StringReader::new("hello world");
    let mut rt_ctx = ParseContext::new_llvm();

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
    let next_fn = get_parse_function::<Next>(&module, "next")?;

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

#[test]
fn line_tracking_with_scanner_tokens() -> SherpaResult<()> {
  let ctx = Context::create();
  let (mut parser, j) = JitParseContext::<TestUTF8StringReader, u32, u32>::from_grammar_str(
    r##"
    @IGNORE g:sp g:nl
    <> A > tk:B P 
    
    <> P > \world \goodby C 


    <> C > tk:B

    <> B > t:" (g:id | g:nl)(+) t:"

    "##,
    &ctx,
    Default::default(),
  )?;

  assert!(!j.debug_error_report());

  //parser.print_code();

  let result = parser.build_ast(
    0,
    &mut TestUTF8StringReader::new("\"\nh\n\"\nworld\n\n\ngoodby\n\"\nmango\""),
    &[
      /*P*/
      |ctx, slots| {
        assert_eq!(slots[0].1.to_string_slice(ctx.get_str()), "world");
        assert_eq!(slots[0].1.line_num, 3, "Line number of `world` should be 3");
        assert_eq!(slots[0].1.line_off, 5, "Line offset of `world` should be 4");
      },
      /*A*/
      |ctx, slots| {
        assert_eq!(slots[0].1.to_string_slice(ctx.get_str()), "\"\nh\n\"");
        assert_eq!(slots[0].1.line_num, 0, "Line number of `\"\\nh\\n\"` should be 0");
        assert_eq!(slots[0].1.line_off, 0, "Line offset of `\"\\nh\\n\"` should be 0");
        slots.assign(0, (1010101, Default::default(), Default::default()))
      },
      /*C*/
      |ctx, slots| {
        assert_eq!(slots[0].1.to_string_slice(ctx.get_str()), "\"\nmango\"");
        assert_eq!(slots[0].1.line_num, 7, "Line number of `world` should be 7");
        assert_eq!(slots[0].1.line_off, 20, "Line offset of `world` should be 20");
      },
      |_, _| {},
    ],
  );

  assert!(matches!(result, ParseResult::Complete((1010101, ..))));

  SherpaResult::Ok(())
}

#[test]
fn simple_newline_tracking() -> SherpaResult<()> {
  let ctx = Context::create();
  let (mut parser, mut j) = JitParseContext::<TestUTF8StringReader, u32, u32>::from_grammar_str(
    r##"
    @IGNORE g:sp g:nl
    
    <> test > \hello P 
    
    <> P > \world \goodby B
    
    <> B > \mango
    "##,
    &ctx,
    Default::default(),
  )?;

  assert!(!j.debug_error_report());

  let result =
    parser.build_ast(0, &mut TestUTF8StringReader::new("hello\nworld\n\ngoodby\nmango"), &[
      /*test*/
      |ctx, slots| {
        assert_eq!(slots[0].1.to_string_slice(ctx.get_str()), "hello");
        assert_eq!(slots[0].1.line_num, 0, "Line number of `hello` should be 0");
        assert_eq!(slots[0].1.line_off, 0, "Line offset of `hello` should be 0");

        slots.assign(0, (1010101, Default::default(), Default::default()))
      },
      /*B*/
      |ctx, slots| {
        assert_eq!(slots[0].1.to_string_slice(ctx.get_str()), "mango");
        assert_eq!(slots[0].1.line_num, 4, "Line number of `mango` should be 4");
        assert_eq!(slots[0].1.line_off, 19, "Line offset of `mango` should be 19");
      },
      /*P*/
      |ctx, slots| {
        assert_eq!(slots[0].1.to_string_slice(ctx.get_str()), "world");
        assert_eq!(slots[0].1.line_num, 1, "Line number of `world` should be 1");
        assert_eq!(slots[0].1.line_off, 5, "Line offset of `world` should be 5");
      },
    ]);

  assert!(matches!(result, ParseResult::Complete((1010101, ..))));

  SherpaResult::Ok(())
}

#[test]
fn test_compile_from_bytecode1() -> SherpaResult<()> {
  let ctx = Context::create();

  let (mut parser, mut j) = JitParseContext::<TestUTF8StringReader, u32, u32>::from_grammar_str(
    "
    @IGNORE g:sp g:nl
    
    <> test > \\hello P 
    
    <> P > \\world \\goodby B
    
    <> B > \\mango
    ",
    &ctx,
    Default::default(),
  )?;

  assert!(!j.debug_error_report());

  let input = "hello world\ngoodby mango";

  let parse_result = parser.build_ast(0, &mut TestUTF8StringReader::new(input), &[
    |ctx, slots| {
      let _a = slots.take(0);
      let _b = slots.take(1);

      assert_eq!(_b.1.len(), 18, "Expected the token of [P] to be 18 bytes long");

      let final_token = _a.1 + _b.1;

      eprintln!(
        "{}",
        final_token.to_token(unsafe { &*ctx.reader }).blame(
          0,
          0,
          "Completed Parse of [test]",
          BlameColor::RED
        )
      );

      slots.assign(0, (0, final_token, TokenRange::default()));
    },
    |_, slots| {
      eprintln!("<B> 02 - {}  {:#?}", 0, slots);
      // Do nothing
    },
    |_, slots| {
      eprintln!("<P> 03 - {}  {:#?}", 0, slots);
      let (_, _a, _) = slots.take(0);
      slots.take(1);
      let (_, _c, _) = slots.take(2);
      slots.assign(0, (0, (_a + _c), TokenRange::default()));
    },
  ]);
  eprintln!("{:#?}", parse_result);

  assert!(matches!(parse_result, ParseResult::Complete(..)));

  if let ParseResult::Complete((_, tok, _)) = parse_result {
    assert_eq!(tok.len(), input.len());
  }

  SherpaResult::Ok(())
}

#[test]
fn test_compile_json_parser() -> SherpaResult<()> {
  use crate::llvm::compile_module_from_bytecode;
  use inkwell::context::Context;
  let mut j = Journal::new(None);
  GrammarStore::from_str(
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

  let ir_states = compile_states(&mut j, 1)?;
  let ir_states = optimize_ir_states(&mut j, ir_states);

  let bytecode_output = compile_bytecode(&mut j, ir_states);

  let ctx = Context::create();

  let mut module = construct_module(&mut j, "test", &ctx);
  compile_module_from_bytecode(&mut module, &mut j, &bytecode_output)?;

  eprintln!("{}", module.module.to_string());

  SherpaResult::Ok(())
}
