use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states, BytecodeOutput},
  debug::generate_disassembly,
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
  ParseActionType,
  ParseContext,
  ParseResult,
  TokenRange,
};

use super::GrammarStore;

type Init<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, *mut R);
type Prime<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>, u32);
type Drop<R, ExtCTX> = unsafe extern "C" fn(*mut ParseContext<R, ExtCTX>);

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

pub(crate) struct JitParseContext<'a, R, ExtCTX = u32, ASTNode = u32>
where
  R: ByteReader,
  ASTNode: AstObject,
{
  module:     LLVMParserModule<'a>,
  init_fn:    JitFunction<'a, Init<R, ExtCTX>>,
  prime_fn:   JitFunction<'a, Prime<R, ExtCTX>>,
  ast_build:  JitFunction<'a, AstBuilder<'a, R, ExtCTX, ASTNode>>,
  drop_fn:    JitFunction<'a, Drop<R, ExtCTX>>,
  sherpa_ctx: ParseContext<R, ExtCTX>,
}

impl<'a, R, ExtCTX, ASTNode> JitParseContext<'a, R, ExtCTX, ASTNode>
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

  fn setup_exec_engine(ctx: &mut LLVMParserModule) {
    if ctx._exe_engine.is_none() {
      ctx._exe_engine =
        Some(ctx.module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap());
    }
  }

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
      let ast_build = engine.get_function("ast_parse").unwrap();
      let drop_fn = engine.get_function("drop").unwrap();

      SherpaResult::Ok(Self {
        init_fn,
        prime_fn,
        ast_build,
        drop_fn,
        module,
        sherpa_ctx: ParseContext::<R, ExtCTX>::new_llvm(),
      })
    }
  }

  pub(crate) fn init(&mut self, reader: &mut R) {
    unsafe {
      self.init_fn.call(&mut self.sherpa_ctx, reader);
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
      self.init(reader);
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

impl<'a, R, ExtCTX, ASTNode> std::ops::Drop for JitParseContext<'a, R, ExtCTX, ASTNode>
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
