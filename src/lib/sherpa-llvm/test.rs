use crate::{
  ascript_functions::construct_ast_builder,
  parse_functions::compile_states,
  *,
};
use inkwell::{
  context::Context,
  execution_engine::{ExecutionEngine, JitFunction},
  module::Module,
  targets::{
    CodeModel,
    InitializationConfig,
    RelocMode,
    Target,
    TargetMachine,
  },
  OptimizationLevel,
};
use sherpa_core::{
  test::{
    frame::{build_parse_states_from_source_str, TestPackage},
    test_reader::TestUTF8StringReader,
  },
  *,
};
use sherpa_runtime::{
  llvm_parser::{sherpa_allocate_stack, sherpa_free_stack},
  types::{Goto, ParseActionType, ParseContext},
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

unsafe fn get_parse_function<
  'a,
  T: inkwell::execution_engine::UnsafeFunctionPointer,
>(
  engine: &'a ExecutionEngine,
  function_name: &str,
) -> Result<JitFunction<'a, T>, ()> {
  Ok(
    engine
      .get_function::<T>(function_name)
      .ok()
      .ok_or("Failed To Compile")
      .unwrap(),
  )
}

fn setup_exec_engine<'a>(module: &Module<'a>) -> ExecutionEngine<'a> {
  module
    .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
    .unwrap()
}

fn build_fast_call_shim<'a>(
  module: &LLVMParserModule<'a>,
  fun: inkwell::values::FunctionValue<'a>,
) -> SherpaResult<()> {
  let name = fun.get_name().to_str().unwrap();
  let builder = &module.b;
  let shim =
    module.module.add_function(&format!("{}_shim", name), fun.get_type(), None);
  shim.set_call_conventions(7);
  builder.position_at_end(module.ctx.append_basic_block(shim, "Entry"));

  let result = build_fast_call(
    &module.b,
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
fn verify_construction_of_init_function() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe { assert!(construct_init(&module).is_ok()) }

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn verify_construction_of_ast_builder() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe { assert!(construct_ast_builder::<u32>(&module).is_ok()) }

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn verify_construction_of_push_state_function() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  assert!(construct_push_state_function(&module).is_ok());

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn should_push_new_state() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = context.create_module("test");

  unsafe {
    let jit_engine = setup_exec_engine(&module);
    let module =
      construct_module(&mut Journal::new(None), &context, &target_data, module);

    jit_engine.add_global_mapping(
      &module.fun.allocate_stack,
      sherpa_allocate_stack as usize,
    );
    jit_engine
      .add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);

    construct_init(&module)?;
    construct_push_state_function(&module)?;
    construct_extend_stack(&module)?;
    construct_internal_free_stack(&module)?;
    construct_drop(&module)?;

    build_fast_call_shim(&module, module.fun.push_state)?;
    build_fast_call_shim(&module, module.fun.extend_stack)?;

    let mut reader = TestUTF8StringReader::new("test");

    let mut rt_ctx = ParseContext::new_llvm();

    let push_state_fn =
      get_parse_function::<PushState>(&jit_engine, "push_state_shim").unwrap();

    let init_fn = get_parse_function::<Init>(&jit_engine, "init").unwrap();

    let extend_stack =
      get_parse_function::<Extend>(&jit_engine, "extend_stack_shim").unwrap();

    let drop_fn = get_parse_function::<Drop>(&jit_engine, "drop").unwrap();

    init_fn.call(&mut rt_ctx, &mut reader);
    extend_stack.call(&mut rt_ctx, 8);

    push_state_fn.call(
      &mut rt_ctx,
      NORMAL_STATE_FLAG_LLVM,
      0x10101010_01010101,
    );
    push_state_fn.call(
      &mut rt_ctx,
      NORMAL_STATE_FLAG_LLVM,
      0x01010101_10101010,
    );

    let stack = std::slice::from_raw_parts(
      {
        (rt_ctx.goto_stack_ptr as usize
          - ((rt_ctx.goto_size - rt_ctx.goto_free) << 4) as usize)
          as *mut Goto
      },
      rt_ctx.goto_size as usize,
    );

    println!("{:#?}", rt_ctx);
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
fn verify_construction_of_get_adjusted_input_block_function() -> SherpaResult<()>
{
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe {
    assert!(construct_get_adjusted_input_block_function(&module).is_ok())
  }

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn verify_construction_of_emit_eop_function() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe { assert!(construct_emit_end_of_parse(&module).is_ok()) }

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn should_initialize_context() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe { assert!(construct_init(&module).is_ok()) };

  unsafe {
    let jit_engine = setup_exec_engine(&module.module);
    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = Box::new(ParseContext::new_llvm());
    let init_fn = get_parse_function::<Init>(&jit_engine, "init").unwrap();

    init_fn.call(rt_ctx.as_mut(), &mut reader);

    let root = rt_ctx.as_ref()
      as *const ParseContext<TestUTF8StringReader<'static>, u32>
      as usize;

    assert_eq!(rt_ctx.reader.as_ref().unwrap().string, reader.string);

    println!("{:?}:{:#?}", root, rt_ctx);
  };
  SherpaResult::Ok(())
}

#[test]
fn verify_utf8_lookup_functions() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  assert!(construct_utf8_lookup_function(&module).is_ok());

  unsafe { assert!(construct_merge_utf8_part_function(&module).is_ok()) }

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn should_yield_correct_CP_values_for_inputs() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

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

    let jit_engine = setup_exec_engine(&module.module);

    let get_code_point = get_parse_function::<GetUtf8CP>(
      &jit_engine,
      "get_utf8_codepoint_info_shim",
    )
    .unwrap();

    dbg!(get_code_point.call(" ".as_ptr()));
    assert_eq!(get_code_point.call(" ".as_ptr()).val, 32);
    // assert_eq!(get_code_point.call(" ".as_ptr()).len, 1);
    assert_eq!(get_code_point.call("☺".as_ptr()).val, 0x263A);
    // assert_eq!(get_code_point.call("☺".as_ptr()).len, 3);
  };

  SherpaResult::Ok(())
}

#[test]
fn verify_construct_extend_stack() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe { assert!(construct_extend_stack(&module).is_ok()) }

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
}

#[test]
fn should_extend_stack() -> SherpaResult<()> {
  let context = Context::create();
  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();
  let module = construct_module(
    &mut Journal::new(None),
    &context,
    &target_data,
    context.create_module("test"),
  );

  unsafe {
    let jit_engine = setup_exec_engine(&module.module);

    build_fast_call_shim(&module, module.fun.push_state);
    build_fast_call_shim(&module, module.fun.extend_stack);

    let mut reader = TestUTF8StringReader::new("test");
    let mut rt_ctx = ParseContext::new_llvm();

    construct_init(&module)?;
    construct_push_state_function(&module)?;
    construct_extend_stack(&module)?;
    construct_internal_free_stack(&module)?;

    jit_engine.add_global_mapping(
      &module.fun.allocate_stack,
      sherpa_allocate_stack as usize,
    );
    jit_engine
      .add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);

    let init_fn = get_parse_function::<Init>(&jit_engine, "init").unwrap();
    let push_state_fn =
      get_parse_function::<PushState>(&jit_engine, "push_state_shim").unwrap();
    let extend =
      get_parse_function::<Extend>(&jit_engine, "extend_stack_shim").unwrap();

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

    // extend.call(&mut rt_ctx, 2);

    assert_eq!(rt_ctx.goto_size, 960);

    println!("{:#?}", stack);
  };

  SherpaResult::Ok(())
}

#[test]
fn test_compile_parse_function() -> SherpaResult<()> {
  build_parse_states_from_source_str(
    "
IGNORE { c:sp }

<> test > 'hello' 'world'
  ",
    "/test".into(),
    Default::default(),
    |TestPackage { mut journal, db, states, .. }| {
      let context = Context::create();
      let target_machine = crate_target_test_machine()?;
      let target_data = target_machine.get_target_data();
      let module = construct_module(
        &mut Journal::new(None),
        &context,
        &target_data,
        context.create_module("test"),
      );

      compile_states(&mut journal, &module, db, &states)?;

      println!("{}", module.module.to_string());

      SherpaResult::Ok(())
    },
  )
}
/*
#[test]
fn test_compile_from_bytecode() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
IGNORE { c:sp }

<> test > 'hello' 'world'
  ",
  )?;

  let states = compile_parse_states(&mut j, 1)?;
  let states = optimize_parse_states(&mut j, states);
  let context = Context::create();
  let module = context.create_module("test");

  unsafe {
    let engine = setup_exec_engine(&module);
    let target_data = engine.get_target_data();
    let module =
      construct_module(&mut Journal::new(None), &context, &target_data, module);

    engine.add_global_mapping(
      &module.fun.allocate_stack,
      sherpa_allocate_stack as usize,
    );
    engine
      .add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);
    engine.add_global_mapping(
      &module.fun.get_token_class_from_codepoint,
      sherpa_get_token_class_from_codepoint as usize,
    );

    compile_llvm_module_from_parse_states(&mut j, &module, &states)?;

    let init_fn = get_parse_function::<Init>(&engine, "init")?;
    let prime_fn = get_parse_function::<Prime>(&engine, "prime")?;
    let next_fn = get_parse_function::<Next>(&engine, "next")?;
    let mut reader = TestUTF8StringReader::new("hello world");

    let mut rt_ctx = ParseContext::new_llvm();

    init_fn.call(&mut rt_ctx, &mut reader);

    prime_fn.call(&mut rt_ctx, 0);

    let action = next_fn.call(&mut rt_ctx);

    dbg!(action);

    assert!(matches!(action, ParseActionType::Shift));

    let action = next_fn.call(&mut rt_ctx);

    dbg!(action);

    assert!(matches!(action, ParseActionType::Skip));

    let action = next_fn.call(&mut rt_ctx);

    dbg!(action);

    assert!(matches!(action, ParseActionType::Shift));

    let action = next_fn.call(&mut rt_ctx);

    assert!(matches!(action, ParseActionType::Reduce));

    let action = next_fn.call(&mut rt_ctx);

    assert!(matches!(action, ParseActionType::Accept));
  };

  SherpaResult::Ok(())
} */

#[test]
fn line_tracking_with_scanner_tokens() -> SherpaResult<()> {
  let ctx = Context::create();

  let jit: SherpaResult<JitParser<TestUTF8StringReader, u32, u32>> = ((
    None,
    r##"
IGNORE { c:sp c:nl }
<> A > tk:B P

<> P > 'world' 'goodby' C

<> C > tk:B

<> B > "\"" (c:id | c:nl)(+) "\""
"##,
    &ctx,
  ))
    .into();

  let mut jit = jit?;
  jit.print_states();
  jit.print_disassembly();
  jit.print_code();

  let mut reduced = 0;

  jit.get_ctx_mut().set_meta(&mut reduced);

  let result = jit.build_ast(
    0,
    &mut TestUTF8StringReader::new("\"\nh\n\"\nworld\n\n\ngoodby\n\"\nmango\""),
    &map_reduce_function(&jit.grammar(), vec![
      /* P */
      ("P", 0, |ctx, slots| {
        let counter = unsafe { ctx.get_meta_mut() };
        *counter += 1;
        println!("Reduced P");
        assert_eq!(slots[0].1.to_slice(ctx.get_str()), "world");
        assert_eq!(
          slots[0].1.line_num, 3,
          "Line number of `world` should be 3"
        );
        assert_eq!(
          slots[0].1.line_off, 5,
          "Line offset of `world` should be 4"
        );
      }),
      ("A", 0 /* A */, |ctx, slots| {
        let counter = unsafe { ctx.get_meta_mut() };
        *counter += 1;
        println!("Reduced A");
        assert_eq!(slots[0].1.to_slice(ctx.get_str()), "\"\nh\n\"");
        assert_eq!(
          slots[0].1.line_num, 0,
          "Line number of `\"\\nh\\n\"` should be 0"
        );
        assert_eq!(
          slots[0].1.line_off, 0,
          "Line offset of `\"\\nh\\n\"` should be 0"
        );
        slots
          .assign(0, AstSlot(1010101, Default::default(), Default::default()))
      }),
      /* C */
      ("C", 0, |ctx, slots| {
        let counter = unsafe { ctx.get_meta_mut() };
        *counter += 1;
        println!("Reduced C");
        assert_eq!(slots[0].1.to_slice(ctx.get_str()), "\"\nmango\"");
        assert_eq!(
          slots[0].1.line_num, 7,
          "Line number of `world` should be 7"
        );
        assert_eq!(
          slots[0].1.line_off, 20,
          "Line offset of `world` should be 20"
        );
      }),
      ("B", 0, |_, _| {}),
    ]),
  );

  assert_eq!(
    unsafe { *jit.get_ctx().get_meta() },
    3,
    "Number of reduced productions should be 3"
  );

  assert!(matches!(result, ParseResult::Complete(AstSlot(1010101, ..))));

  SherpaResult::Ok(())
}
/*
#[test]
fn simple_newline_tracking() -> SherpaResult<()> {
  let ctx = Context::create();

  let jit: SherpaResult<JitParser<TestUTF8StringReader, u32, u32>> = ((
    None,
    r##"
    IGNORE { c:sp c:nl }

    <> test > 'hello' P

    <> P > 'world' 'goodby' B

    <> B > 'mango'
        "##,
    &ctx,
  ))
    .into();
  let mut jit = jit?;

  jit.print_disassembly();

  let result = jit.build_ast(
    0,
    &mut TestUTF8StringReader::new("hello\nworld\n\ngoodby\nmango"),
    &map_reduce_function(&jit.grammar(), vec![
      ("test", 0, |ctx, slots| {
        assert_eq!(slots[0].1.to_slice(ctx.get_str()), "hello");
        assert_eq!(
          slots[0].1.line_num, 0,
          "Line number of `hello` should be 0"
        );
        assert_eq!(
          slots[0].1.line_off, 0,
          "Line offset of `hello` should be 0"
        );

        slots
          .assign(0, AstSlot(1010101, Default::default(), Default::default()))
      }),
      ("B", 0, |ctx, slots| {
        assert_eq!(slots[0].1.to_slice(ctx.get_str()), "mango");
        assert_eq!(
          slots[0].1.line_num, 4,
          "Line number of `mango` should be 4"
        );
        assert_eq!(
          slots[0].1.line_off, 19,
          "Line offset of `mango` should be 19"
        );
      }),
      ("P", 0, |ctx, slots| {
        assert_eq!(slots[0].1.to_slice(ctx.get_str()), "world");
        assert_eq!(
          slots[0].1.line_num, 1,
          "Line number of `world` should be 1"
        );
        assert_eq!(
          slots[0].1.line_off, 5,
          "Line offset of `world` should be 5"
        );
      }),
    ]),
  );

  assert!(matches!(result, ParseResult::Complete(AstSlot(1010101, ..))));

  SherpaResult::Ok(())
} */
/*
#[test]
fn test_compile_from_bytecode1() -> SherpaResult<()> {
  let ctx = Context::create();

  let jit: SherpaResult<JitParser<TestUTF8StringReader, u32, u32>> = ((
    None,
    "
IGNORE { c:sp c:nl }

<> test > 'hello' P

<> P > 'world' 'goodby' B

<> B > 'mango'
    ",
    &ctx,
  ))
    .into();
  let mut jit = jit?;

  let g = &(jit.grammar());

  let input = "hello world\ngoodby mango";

  let parse_result = jit.build_ast(
    0,
    &mut TestUTF8StringReader::new(input),
    &map_reduce_function(&g, vec![
      ("test", 0, |ctx, slots| {
        let _a = slots.take(0);
        let _b = slots.take(1);

        assert_eq!(
          _b.1.len(),
          18,
          "Expected the token of [P] to be 18 bytes long"
        );

        let final_token = _a.1 + _b.1;

        println!(
          "{}",
          final_token.to_token(unsafe { &mut *ctx.reader }).blame(
            0,
            0,
            "Completed Parse of [test]",
            BlameColor::RED
          )
        );

        slots.assign(0, AstSlot(0, final_token, TokenRange::default()));
      }),
      ("B", 0, |_, slots| {
        println!("<B> 02 - {}  {:#?}", 0, slots);
        // Do nothing
      }),
      ("P", 0, |_, slots| {
        println!("<P> 03 - {}  {:#?}", 0, slots);
        let AstSlot(_, _a, _) = slots.take(0);
        slots.take(1);
        let AstSlot(_, _c, _) = slots.take(2);
        slots.assign(0, AstSlot(0, _a + _c, TokenRange::default()));
      }),
    ]),
  );

  println!("{:#?}", parse_result);

  assert!(matches!(parse_result, ParseResult::Complete(..)));

  if let ParseResult::Complete(AstSlot(_, tok, _)) = parse_result {
    assert_eq!(tok.len(), input.len());
  }

  SherpaResult::Ok(())
}

// Sorts reduce functions according to their respective
// rules. This assumes the number of rules in the array
// matches the number of rules in the parser.
fn map_reduce_function<'a, R, ExtCTX, ASTNode>(
  g: &GrammarStore,
  fns: Vec<(
    &str,
    usize,
    fn(&mut ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>),
  )>,
) -> Vec<fn(&mut ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>)>
where
  R: ByteReader + LLVMByteReader + MutByteReader,
  ASTNode: AstObject,
{
  fns
    .into_iter()
    .filter_map(|(name, rule_number, b)| {
      let prod = g.get_production_by_name(name).unwrap();
      let rule_id = g.production_rules.get(&prod.id).unwrap()[rule_number];
      let rule = g.get_rule(&rule_id).unwrap();
      if let Some(bc_id) = rule.bytecode_id {
        Some((bc_id, b))
      } else {
        None
      }
    })
    .collect::<BTreeMap<_, _>>()
    .into_values()
    .collect::<Vec<_>>()
} */
/*
#[test]
fn test_compile_json_parser() -> SherpaResult<()> {
  use crate::llvm::compile_llvm_module_from_parse_states;
  use inkwell::context::Context;
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    r##"
IGNORE { c:sp c:nl }
EXPORT json as entry
NAME llvm_language_test

<> json >
        object                              :ast { t_Json, v: $1 }
        |
        array                               :ast { t_Json, v: $1 }

<> array > '['  value(*",")  ']'              :ast { t_Array, entries: $2 }

<> object > '{' key_value(*",") '}'           :ast { t_Object, entries: $2 }

<> key_value > str ':' value              :ast { t_KeyVal, k:$1, v:$3 }

<> value > num | bool | str | null

<> null > "null"                            :ast { t_Null, v:false }

<> bool >
    "false"                                 :ast { t_Bool, v:false }
    |
    "true"                                  :ast { t_Bool, v:true }

<> str > tk:string                          :ast { t_Str }

<> num > tk:number                          :ast { t_Number }

<> number > ( '+' | '-' )? c:num(+) ( '.' c:num(+) )? ( ( 'e' | 'E' ) ( '+' | 'i' ) c:num(+) )?

<> string > '"' ( c:id | c:sym | c:num | c:sp )(+) "\""
"##,
  );

  assert!(!j.debug_error_report());

  let ir_states = compile_parse_states(&mut j, 1)?;
  let ir_states = optimize_parse_states(&mut j, ir_states);

  let ctx = Context::create();

  let target_machine = crate_target_test_machine()?;
  let target_data = target_machine.get_target_data();

  let module =
    construct_module(&mut j, &ctx, &target_data, ctx.create_module("test"));
  compile_llvm_module_from_parse_states(&mut j, &module, &ir_states)?;

  println!("{}", module.module.to_string());

  SherpaResult::Ok(())
} */

fn crate_target_test_machine() -> SherpaResult<TargetMachine> {
  Target::initialize_native(&InitializationConfig::default())?;
  let native_triple = TargetMachine::get_default_triple();
  let target = Target::from_triple(&native_triple).unwrap();
  let reloc = RelocMode::PIC;
  let model = CodeModel::Small;
  let opt = OptimizationLevel::None;
  SherpaResult::Ok(target.create_target_machine(
    &native_triple,
    "generic",
    "",
    opt,
    reloc,
    model,
  )?)
}
