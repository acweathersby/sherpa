#![allow(unused)]
use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states, BytecodeOutput},
  llvm::{ascript_functions::construct_ast_builder, compile_module_from_bytecode, *},
  Config,
  Journal,
  SherpaResult,
};
use inkwell::{context::Context, execution_engine::JitFunction};
use sherpa_runtime::types::{
  llvm_map_result_action,
  llvm_map_shift_action,
  sherpa_allocate_stack,
  sherpa_free_stack,
  sherpa_get_token_class_from_codepoint,
  AstObject,
  AstStackSlice,
  ByteReader,
  LLVMByteReader,
  MutByteReader,
  ParseAction,
  ParseActionType,
  ParseContext,
  ParseResult,
  SherpaParser,
  TokenRange,
};

use super::GrammarStore;

type Init<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, *mut R);
type Prime<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, u32);
type Drop<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>);
type Next<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>) -> ParseActionType;

type AstBuilder<'a, R, ExtCTX, ASTNode> = unsafe extern "C" fn(
  *mut ParseContext<R, ExtCTX>,
  *const fn(
    ctx: &mut ParseContext<R, ExtCTX>,
    &mut AstStackSlice<(ASTNode, TokenRange, TokenRange)>,
  ),
  unsafe fn(&ParseContext<R, ExtCTX>, &mut AstStackSlice<(ASTNode, TokenRange, TokenRange)>),
  unsafe fn(
    &ParseContext<R, ExtCTX>,
    ParseActionType,
    &mut AstStackSlice<(ASTNode, TokenRange, TokenRange)>,
  ) -> ParseResult<ASTNode>,
) -> ParseResult<ASTNode>;

pub(crate) struct JitParser<'a, R, ExtCTX = u32, ASTNode = u32>
where
  R: ByteReader,
  ASTNode: AstObject,
{
  reader:     *mut R,
  module:     LLVMParserModule<'a>,
  init_fn:    JitFunction<'a, Init<R, ExtCTX>>,
  next_fn:    JitFunction<'a, Next<R, ExtCTX>>,
  prime_fn:   JitFunction<'a, Prime<R, ExtCTX>>,
  ast_build:  JitFunction<'a, AstBuilder<'a, R, ExtCTX, ASTNode>>,
  drop_fn:    JitFunction<'a, Drop<R, ExtCTX>>,
  sherpa_ctx: ParseContext<R, ExtCTX>,
}

impl<'a, R, ExtCTX, ASTNode> JitParser<'a, R, ExtCTX, ASTNode>
where
  R: ByteReader + LLVMByteReader,
  ASTNode: AstObject,
{
  pub(crate) fn new(j: &mut Journal, bc: &BytecodeOutput, ctx: &'a Context) -> SherpaResult<Self> {
    let mut module = construct_module(j, "", ctx);

    unsafe {
      Self::setup_exec_engine(&mut module);

      let engine = module._exe_engine.as_ref()?;

      engine.add_global_mapping(&module.fun.allocate_stack, sherpa_allocate_stack as usize);
      engine.add_global_mapping(&module.fun.free_stack, sherpa_free_stack as usize);
      engine.add_global_mapping(
        &module.fun.get_token_class_from_codepoint,
        sherpa_get_token_class_from_codepoint as usize,
      );

      compile_module_from_bytecode(&module, j, bc)?;
      construct_ast_builder::<ASTNode>(&module)?;

      let prime_fn = engine.get_function("prime").unwrap();
      let init_fn = engine.get_function("init").unwrap();
      let next_fn = engine.get_function("next").unwrap();
      let ast_build = engine.get_function("ast_parse").unwrap();
      let drop_fn = engine.get_function("drop").unwrap();

      SherpaResult::Ok(Self {
        reader: (0 as usize) as *mut R,
        init_fn,
        prime_fn,
        ast_build,
        drop_fn,
        next_fn,
        module,
        sherpa_ctx: ParseContext::<R, ExtCTX>::new_llvm(),
      })
    }
  }

  pub(crate) fn set_reader(&mut self, reader: &'a mut R) {
    self.reader = reader;
  }

  fn setup_exec_engine(ctx: &mut LLVMParserModule) {
    if ctx._exe_engine.is_none() {
      ctx._exe_engine =
        Some(ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap());
    }
  }

  pub(crate) fn next(&mut self) -> ParseActionType {
    unsafe { self.next_fn.call(&mut self.sherpa_ctx) }
  }
}

impl<'a, R, ExtCTX, ASTNode> JitParser<'a, R, ExtCTX, ASTNode>
where
  R: ByteReader + LLVMByteReader + MutByteReader,
  ASTNode: AstObject,
{
  pub(crate) fn from_grammar_str(
    grammar_source: &str,
    ctx: &'a Context,
    config: Config,
  ) -> SherpaResult<(Self, Journal)> {
    let mut j = Journal::new(Some(config));

    GrammarStore::from_str(&mut j, grammar_source)?;

    let ir_states = compile_states(&mut j, 1)?;
    let ir_states = optimize_ir_states(&mut j, ir_states);
    let bytecode_output = compile_bytecode(&mut j, ir_states);

    j.flush_reports();

    SherpaResult::Ok((Self::new(&mut j, &bytecode_output, &ctx)?, j))
  }

  pub(crate) fn init(&mut self) {
    unsafe {
      let Self { sherpa_ctx, reader, .. } = self;
      self.init_fn.call(sherpa_ctx, *reader);
    }
  }

  pub(crate) fn prime(&mut self, entry_index: u32) {
    unsafe {
      self.prime_fn.call(&mut self.sherpa_ctx, entry_index);
    }
  }

  /// Prints the LLVM code to std::err.
  pub(crate) fn print_code(&self) {
    eprintln!("{}", self.module.module.to_string())
  }

  pub(crate) fn build_ast(
    &mut self,
    entry_index: u32,
    reader: &mut R,
    functions: &[fn(
      &mut ParseContext<R, ExtCTX>,
      &mut AstStackSlice<(ASTNode, TokenRange, TokenRange)>,
    )],
  ) -> ParseResult<ASTNode> {
    unsafe {
      self.init();
      self.prime(entry_index);

      self.ast_build.call(
        &mut self.sherpa_ctx,
        functions.as_ptr(),
        llvm_map_shift_action::<R, ExtCTX, ASTNode>,
        llvm_map_result_action::<R, ExtCTX, ASTNode>,
      )
    }
  }
}

impl<'a, R, ExtCTX, ASTNode> std::ops::Drop for JitParser<'a, R, ExtCTX, ASTNode>
where
  R: ByteReader,
  ASTNode: AstObject,
{
  fn drop(&mut self) {
    unsafe {
      let drop_fn = self.drop_fn.clone();
      drop_fn.call(&mut self.sherpa_ctx);
    }
  }
}

impl<'a, R: ByteReader + LLVMByteReader + MutByteReader, M> SherpaParser<R, M>
  for JitParser<'a, R, M>
{
  fn get_token_length(&self) -> u32 {
    self.sherpa_ctx.token_len
  }

  fn get_token_offset(&self) -> u32 {
    self.sherpa_ctx.token_off
  }

  fn get_token_line_number(&self) -> u32 {
    self.sherpa_ctx.start_line_num
  }

  fn get_token_line_offset(&self) -> u32 {
    self.sherpa_ctx.start_line_off
  }

  fn get_production_id(&self) -> u32 {
    self.sherpa_ctx.prod_id
  }

  fn get_reader(&self) -> &R {
    self.sherpa_ctx.get_reader()
  }

  fn get_input(&self) -> &str {
    unsafe { std::str::from_utf8_unchecked(self.get_reader().get_bytes()) }
  }

  fn init_parser(&mut self, entry_point: u32) {
    self.init();
    self.prime(entry_point);
  }

  fn get_next_action(
    &mut self,
    debug: &Option<sherpa_runtime::functions::DebugFn<R, M>>,
  ) -> ParseAction {
    match self.next() {
      ParseActionType::Shift => ParseAction::Shift {
        anchor_byte_offset: self.sherpa_ctx.anchor_off,
        token_byte_offset:  self.sherpa_ctx.token_off,
        token_byte_length:  self.sherpa_ctx.token_len,
        token_line_offset:  self.sherpa_ctx.start_line_off,
        token_line_count:   self.sherpa_ctx.start_line_num,
      },
      ParseActionType::Reduce => ParseAction::Reduce {
        production_id: self.sherpa_ctx.prod_id,
        rule_id:       self.sherpa_ctx.meta_a,
        symbol_count:  self.sherpa_ctx.meta_b,
      },
      ParseActionType::Accept => ParseAction::Accept { production_id: self.sherpa_ctx.prod_id },
      ParseActionType::Error => ParseAction::Error {
        last_production: self.sherpa_ctx.prod_id,
        last_input:      TokenRange {
          len:      self.get_token_length(),
          off:      self.get_token_offset(),
          line_num: self.get_token_line_number(),
          line_off: self.get_token_line_offset(),
        },
      },
      _ => ParseAction::FailState,
    }
  }
}
