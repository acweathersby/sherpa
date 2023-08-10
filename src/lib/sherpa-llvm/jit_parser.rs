#![allow(unused)]
use inkwell::{
  context::Context,
  execution_engine::{ExecutionEngine, JitFunction},
};
use sherpa_core::{parser::ASTNode, *};
use sherpa_rust_runtime::{
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
use std::{path::Path, sync::Arc};

use crate::{
  ascript_functions::construct_ast_builder,
  compile_llvm_module_from_parse_states,
  construct_module,
  LLVMParserModule,
};

type Init<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, *mut R);
type Prime<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, u32);
type Drop<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>);
type Next<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>) -> ParseActionType;

type AstBuilder<'llvm, R, ExtCTX, ASTNode> = unsafe extern "C" fn(
  *mut ParseContext<R, ExtCTX>,
  *const fn(ctx: &mut ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>),
  unsafe fn(&ParseContext<R, ExtCTX>, &mut AstStackSlice<AstSlot<ASTNode>>),
  unsafe fn(
    &ParseContext<R, ExtCTX>,
    ParseActionType,
    &mut AstStackSlice<AstSlot<ASTNode>>,
  ) -> ParseResult<ASTNode>,
) -> ParseResult<ASTNode>;

pub(crate) struct JitParser<'llvm, R, ExtCTX = u32, ASTNode = u32>
where
  R: ByteReader,
  ASTNode: AstObject,
{
  reader:    *mut R,
  module:    LLVMParserModule<'llvm>,
  init_fn:   JitFunction<'llvm, Init<R, ExtCTX>>,
  next_fn:   JitFunction<'llvm, Next<R, ExtCTX>>,
  prime_fn:  JitFunction<'llvm, Prime<R, ExtCTX>>,
  ast_build: JitFunction<'llvm, AstBuilder<'llvm, R, ExtCTX, ASTNode>>,
  drop_fn:   JitFunction<'llvm, Drop<R, ExtCTX>>,
  engine:    ExecutionEngine<'llvm>,
  ctx:       ParseContext<R, ExtCTX>,
  j:         Journal,
  db:        ParserDatabase,
}

impl<'llvm, R, ExtCTX, ASTNode> JitParser<'llvm, R, ExtCTX, ASTNode>
where
  R: ByteReader + LLVMByteReader,
  ASTNode: AstObject,
{
  pub(crate) fn new<'db>(
    j: &mut Journal,
    db: ParserDatabase,
    ctx: &'llvm Context,
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

      let states = compile_parse_states(j.transfer(), &db).unwrap();
      let states = garbage_collect::<ParseStatesVec>(&db, states).unwrap();
      compile_llvm_module_from_parse_states(j, &llvm_mod, &db, &states)?;

      construct_ast_builder::<ASTNode>(&llvm_mod)?;

      let init_fn = engine.get_function("init").unwrap();
      let drop_fn = engine.get_function("drop").unwrap();
      let next_fn = engine.get_function("next").unwrap();
      let ast_build = engine.get_function("ast_parse").unwrap();
      let prime_fn = engine.get_function("prime").unwrap();

      SherpaResult::Ok(Self {
        db,
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
      })
    }
  }

  pub(crate) fn db(&self) -> &ParserDatabase {
    &self.db
  }

  pub(crate) fn get_ctx_mut(&mut self) -> &mut ParseContext<R, ExtCTX> {
    &mut self.ctx
  }

  pub(crate) fn get_ctx(&self) -> &ParseContext<R, ExtCTX> {
    &self.ctx
  }

  pub(crate) fn set_reader(&mut self, reader: &mut R) {
    self.reader = reader;
  }

  pub(crate) fn next(&mut self) -> ParseActionType {
    unsafe { self.next_fn.call(&mut self.ctx) }
  }
}

impl<'llvm, R, ExtCTX, ASTNode> From<(Option<Config>, &'llvm str, &'llvm Context)>
  for JitParser<'llvm, R, ExtCTX, ASTNode>
where
  R: ByteReader + LLVMByteReader,
  ASTNode: AstObject,
{
  fn from((config, source, context): (Option<Config>, &'llvm str, &'llvm Context)) -> Self {
    let mut journal = Journal::new(config);

    let soup = GrammarSoup::new();

    journal.set_active_report("test", ReportType::Any);

    let id = compile_grammar_from_str(&mut journal, source, "/grammar.sg".into(), &soup).unwrap();

    journal.flush_reports();

    journal.flush_reports();

    let db = build_compile_db(journal.transfer(), id, &soup).unwrap();

    journal.flush_reports();

    JitParser::new(&mut journal, db, &context).unwrap()
  }
}

impl<'llvm, R, ExtCTX, ASTNode> JitParser<'llvm, R, ExtCTX, ASTNode>
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
  #[cfg(debug_assertions)]
  pub(crate) fn print_states(&mut self) {
    let states = compile_parse_states(self.j.transfer(), &self.db).unwrap();
    let states = garbage_collect::<ParseStatesVec>(&self.db, states).unwrap();
    for (_, state) in &states {
      println!("{}", state.debug_string(&self.db))
    }
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

impl<'llvm, R, ExtCTX, ASTNode> std::ops::Drop for JitParser<'llvm, R, ExtCTX, ASTNode>
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

impl<'llvm, R: ByteReader + LLVMByteReader + MutByteReader, M> SherpaParser<R, M>
  for JitParser<'llvm, R, M>
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

  fn get_token_id(&self) -> u32 {
    self.ctx.tok_id
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
    debug: &mut Option<sherpa_rust_runtime::bytecode::DebugFn>,
  ) -> ParseAction {
    match self.next() {
      ParseActionType::Shift => ParseAction::Shift {
        token_byte_offset: self.get_token_offset(),
        token_byte_length: self.get_token_length(),
        token_line_offset: self.get_token_line_offset(),
        token_line_count:  self.get_token_line_number(),
        token_id:          self.get_token_length(),
      },
      ParseActionType::Skip => ParseAction::Skip {
        token_byte_offset: self.get_token_offset(),
        token_byte_length: self.get_token_length(),
        token_line_offset: self.get_token_line_offset(),
        token_line_count:  self.get_token_line_number(),
        token_id:          self.get_token_length(),
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
