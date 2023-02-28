#![allow(unused)]
use std::sync::Arc;

use crate::{
  build::{
    bytecode::{self, build_bytecode_parser},
    disassembly,
  },
  bytecode::compile_bytecode,
  debug::generate_disassembly,
  llvm::{ascript_functions::construct_ast_builder, compile_llvm_module_from_parse_states, *},
  parser::{compile_parse_states, optimize_parse_states},
  util::get_num_of_available_threads,
  Config,
  Journal,
  SherpaResult,
};
use inkwell::{
  context::Context,
  execution_engine::{ExecutionEngine, JitFunction},
};
use sherpa_runtime::{
  llvm_parser::{
    llvm_map_result_action,
    llvm_map_shift_action,
    sherpa_allocate_stack,
    sherpa_free_stack,
    sherpa_get_token_class_from_codepoint,
    LLVMByteReader,
  },
  types::{
    ast::{AstObject, AstSlot, AstStackSlice},
    ByteReader,
    MutByteReader,
    ParseAction,
    ParseActionType,
    ParseContext,
    ParseResult,
    SherpaParser,
    TokenRange,
  },
};

use super::{GrammarStore, ParseState};

type Init<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, *mut R);
type Prime<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, u32);
type Drop<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>);
type Next<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>) -> ParseActionType;

type AstBuilder<'a, R, ExtCTX, ASTNode> = unsafe extern "C" fn(
  *mut ParseContext<R, ExtCTX>,
  *const fn(ctx: &mut ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>),
  unsafe fn(&ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>),
  unsafe fn(
    &ParseContext<R, ExtCTX>,
    ParseActionType,
    &mut AstStackSlice<AstSlot<ASTNode>>,
  ) -> ParseResult<ASTNode>,
) -> ParseResult<ASTNode>;

pub(crate) struct JitParser<'a, R, ExtCTX = u32, ASTNode = u32>
where
  R: ByteReader,
  ASTNode: AstObject,
{
  reader:    *mut R,
  module:    LLVMParserModule<'a>,
  init_fn:   JitFunction<'a, Init<R, ExtCTX>>,
  next_fn:   JitFunction<'a, Next<R, ExtCTX>>,
  prime_fn:  JitFunction<'a, Prime<R, ExtCTX>>,
  ast_build: JitFunction<'a, AstBuilder<'a, R, ExtCTX, ASTNode>>,
  drop_fn:   JitFunction<'a, Drop<R, ExtCTX>>,
  engine:    ExecutionEngine<'a>,
  ctx:       ParseContext<R, ExtCTX>,
  j:         Journal,
  states:    Vec<(String, Box<ParseState>)>,
}

impl<'a, R, ExtCTX, ASTNode> JitParser<'a, R, ExtCTX, ASTNode>
where
  R: ByteReader + LLVMByteReader,
  ASTNode: AstObject,
{
  pub(crate) fn new(
    j: &mut Journal,
    states: Vec<(String, Box<ParseState>)>,
    ctx: &'a Context,
  ) -> SherpaResult<Self> {
    unsafe {
      let module = ctx.create_module("JIT_PARSER");
      let engine = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();
      let target_data = engine.get_target_data();

      let mut llvm_mod = construct_module(j, ctx, &target_data, module);

      engine.add_global_mapping(&llvm_mod.fun.allocate_stack, sherpa_allocate_stack as usize);
      engine.add_global_mapping(&llvm_mod.fun.free_stack, sherpa_free_stack as usize);
      engine.add_global_mapping(
        &llvm_mod.fun.get_token_class_from_codepoint,
        sherpa_get_token_class_from_codepoint as usize,
      );

      compile_llvm_module_from_parse_states(j, &llvm_mod, &states)?;

      construct_ast_builder::<ASTNode>(&llvm_mod)?;

      let init_fn = engine.get_function("init").unwrap();
      let drop_fn = engine.get_function("drop").unwrap();
      let next_fn = engine.get_function("next").unwrap();
      let ast_build = engine.get_function("ast_parse").unwrap();
      let prime_fn = engine.get_function("prime").unwrap();

      SherpaResult::Ok(Self {
        reader: (0 as usize) as *mut R,
        init_fn,
        prime_fn,
        engine,
        ast_build,
        drop_fn,
        next_fn,
        module: llvm_mod,
        ctx: ParseContext::<R, ExtCTX>::new_llvm(),
        j: j.transfer(),
        states: states,
      })
    }
  }

  pub(crate) fn get_ctx_mut(&mut self) -> &mut ParseContext<R, ExtCTX> {
    &mut self.ctx
  }

  pub(crate) fn get_ctx(&self) -> &ParseContext<R, ExtCTX> {
    &self.ctx
  }

  pub(crate) fn grammar(&self) -> Arc<GrammarStore> {
    self.j.grammar().unwrap()
  }

  pub(crate) fn set_reader(&mut self, reader: &mut R) {
    self.reader = reader;
  }

  pub(crate) fn next(&mut self) -> ParseActionType {
    unsafe { self.next_fn.call(&mut self.ctx) }
  }
}

impl<'a, R, ExtCTX, ASTNode> From<(Option<Config>, &'a str, &'a Context)>
  for SherpaResult<JitParser<'a, R, ExtCTX, ASTNode>>
where
  R: ByteReader + LLVMByteReader,
  ASTNode: AstObject,
{
  fn from((config, string, context): (Option<Config>, &'a str, &'a Context)) -> Self {
    let mut j = Journal::new(config);
    GrammarStore::from_str(&mut j, string)?;
    let states = compile_parse_states(&mut j, get_num_of_available_threads())?;
    let states = optimize_parse_states(&mut j, states);
    JitParser::new(&mut j, states, &context)
  }
}

impl<'a, R, ExtCTX, ASTNode> JitParser<'a, R, ExtCTX, ASTNode>
where
  R: ByteReader + LLVMByteReader + MutByteReader,
  ASTNode: AstObject,
{
  pub(crate) fn init(&mut self) {
    self.ctx.reset();

    unsafe {
      let Self { ctx: sherpa_ctx, reader, .. } = self;
      self.init_fn.call(sherpa_ctx, *reader);
    }
  }

  pub(crate) fn prime(&mut self, entry_index: u32) {
    unsafe {
      self.prime_fn.call(&mut self.ctx, entry_index);
    }
  }

  /// Prints the LLVM code to std::err.
  pub(crate) fn print_code(&self) {
    println!("{}", self.module.module.print_to_string().to_string());
  }

  /// Prints intermediate representation of the parser states. If optimization
  /// has occurred then the states may not represent the actual parser.
  pub(crate) fn print_states(&mut self) {
    for (_, state) in &self.states {
      println!("{}", state.to_string())
    }
  }

  /// Prints the equivalent bytecode parser disassembly to to stderr.
  pub(crate) fn print_disassembly(&mut self) {
    let bytecode_output = compile_bytecode(&mut self.j, &self.states).unwrap();
    println!("{}", &generate_disassembly(&bytecode_output, &mut self.j));
  }

  pub(crate) fn build_ast(
    &mut self,
    entry_index: u32,
    reader: &mut R,
    functions: &[fn(&mut ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>)],
  ) -> ParseResult<ASTNode> {
    unsafe {
      self.ctx.reset();
      self.set_reader(reader);
      self.init();
      self.prime(entry_index);

      self.ast_build.call(
        &mut self.ctx,
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
      drop_fn.call(&mut self.ctx);
    }
  }
}

impl<'a, R: ByteReader + LLVMByteReader + MutByteReader, M> SherpaParser<R, M>
  for JitParser<'a, R, M>
{
  fn get_ctx(&self) -> &ParseContext<R, M> {
    &self.ctx
  }

  fn get_ctx_mut(&mut self) -> &mut ParseContext<R, M> {
    &mut self.ctx
  }

  fn head_at_end(&self) -> bool {
    (self.ctx.anchor_ptr - self.ctx.begin_ptr) == self.get_reader().len()
  }

  fn get_token_length(&self) -> u32 {
    self.ctx.get_token_length()
  }

  fn get_token_offset(&self) -> u32 {
    self.ctx.get_token_offset()
  }

  fn get_token_line_number(&self) -> u32 {
    self.ctx.get_token_line_number()
  }

  fn get_token_line_offset(&self) -> u32 {
    self.ctx.get_token_line_offset()
  }

  fn get_production_id(&self) -> u32 {
    self.ctx.prod_id
  }

  fn get_reader(&self) -> &R {
    self.ctx.get_reader()
  }

  fn get_reader_mut(&mut self) -> &mut R {
    unsafe { &mut *self.reader }
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
    debug: &mut Option<sherpa_runtime::bytecode_parser::DebugFn>,
  ) -> ParseAction {
    match self.next() {
      ParseActionType::Shift => ParseAction::Shift {
        anchor_byte_offset: (self.ctx.anchor_ptr - self.ctx.begin_ptr) as u32,
        token_byte_offset:  self.get_token_offset(),
        token_byte_length:  self.get_token_length(),
        token_line_offset:  self.get_token_line_offset(),
        token_line_count:   self.get_token_line_number(),
      },
      ParseActionType::Reduce => ParseAction::Reduce {
        production_id: self.ctx.prod_id,
        rule_id:       self.ctx.rule_id,
        symbol_count:  self.ctx.sym_len,
      },
      ParseActionType::Accept => ParseAction::Accept { production_id: self.ctx.prod_id },
      ParseActionType::Error => ParseAction::Error {
        last_production: self.ctx.prod_id,
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
