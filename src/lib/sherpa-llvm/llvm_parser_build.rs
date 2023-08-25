use crate::{ascript_functions::construct_ast_builder, compile_llvm_module_from_parse_states, construct_module};

use inkwell::{
  context::Context,
  passes::{PassManager, PassManagerBuilder},
  targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple},
  OptimizationLevel,
};
use sherpa_core::{ParserStore, SherpaError, SherpaResult};
use sherpa_rust_runtime::types::{ast::AstObject, Token};
use std::{io::Write, path::PathBuf, process::Command};
/// Constructs a task that outputs a Rust parse context interface
/// for the llvm parser.

pub enum OutputType {
  Rust,
  _Cpp,
  _TypeScript,
  _JavaScript,
  _Java,
}

/// Used to calculate the max size of an
/// AST node
#[derive(Debug, Clone)]
#[repr(C, u32)]
pub(crate) enum DummyASTEnum {
  _None,
  _U64(u64),
  _NODE(Box<u64>),
  _VEC(Vec<u64>),
  _STRING(String),
  _TOKEN(Token),
}

impl Default for DummyASTEnum {
  fn default() -> Self {
    DummyASTEnum::_None
  }
}

impl AstObject for DummyASTEnum {}

/// Build artifacts for a LLVM based parser.
///
/// # Args
/// `target_triple` - A LLVM compatible target triple
///
/// `clang_command` - The name of the clang executable that can be called from a
/// terminal
///
/// `output_cargo_build_commands` - Set to true to output build commands that
/// inform the rust compiler of the link targets.
///
/// `output_llvm_ir_file` - Output a purely decorational version of the LLVM
/// code in intermediate representational form.
pub fn build_llvm_parser<T: ParserStore>(
  store: &T,
  parser_name: &str,
  output_dir: &PathBuf,
  target_triple: Option<String>,
  enable_ascript: bool,
) -> SherpaResult<()> {
  let ar_command = "llvm-ar-14";
  std::fs::create_dir_all(output_dir)?;
  let _ll_file_path = output_dir.join(parser_name.to_string() + ".ll");
  let _bitcode_path = output_dir.join("lib".to_string() + &parser_name + ".bc");
  let object_path = output_dir.join("lib".to_string() + &parser_name + ".o");
  let archive_path = output_dir.join("lib".to_string() + &parser_name + ".a");
  let ll_file_path = output_dir.join(parser_name.to_string() + ".ll");
  let ctx = Context::create();

  let (target_data, target_machine, target_triple) = create_target(target_triple);

  let module = ctx.create_module(&parser_name);
  module.set_triple(&target_triple);
  module.set_data_layout(&target_data.get_data_layout());
  let sherpa_mod = &construct_module(&ctx, &target_data, module);

  let opt = OptimizationLevel::Aggressive;

  compile_llvm_module_from_parse_states(store, sherpa_mod)?;

  if enable_ascript {
    unsafe {
      construct_ast_builder::<DummyASTEnum>(&sherpa_mod).unwrap();
    }
  }

  apply_llvm_optimizations(opt, &sherpa_mod);

  if let Ok(mut file) = std::fs::File::create(&ll_file_path) {
    file.write_all(sherpa_mod.module.to_string().as_bytes())?;
    file.flush()?;
  }

  match target_machine.write_to_file(&sherpa_mod.module, FileType::Object, &object_path) {
    Ok(_) => {
      if !(Command::new(ar_command)
        .args(&["rc", archive_path.to_str().unwrap(), object_path.to_str().unwrap()])
        .status()
        .unwrap()
        .success())
      {
        SherpaResult::Err(SherpaError::from("Unable to compile llvm bitcode: Incorrect path to LLVM-AR executable"))
      } else {
        SherpaResult::Ok(())
      }
    }
    Err(err) => SherpaResult::Err(SherpaError::from(err.to_string())),
  }
}

/// Sets up the target machine
fn create_target(target_triple: Option<String>) -> (TargetData, TargetMachine, TargetTriple) {
  // -------------------------------------------------------------------
  Target::initialize_x86(&InitializationConfig::default());
  let target_triple = if let Some(target_triple) = &target_triple {
    TargetTriple::create(target_triple)
  } else if let Ok(target_triple) = std::env::var("TARGET") {
    TargetTriple::create(&target_triple)
  } else {
    TargetMachine::get_default_triple()
  };
  let reloc = RelocMode::PIC;
  let model = CodeModel::Small;
  let target = Target::from_triple(&target_triple).unwrap();
  let opt = OptimizationLevel::Aggressive;
  let target_machine = target.create_target_machine(&target_triple, "generic", "", opt, reloc, model).unwrap();

  let target_data = target_machine.get_target_data();

  (target_data, target_machine, target_triple)
}

fn apply_llvm_optimizations(opt: OptimizationLevel, ctx: &crate::LLVMParserModule) {
  //return;
  let pass_manager_builder = PassManagerBuilder::create();
  let pass_manager = PassManager::create(());
  pass_manager_builder.set_optimization_level(opt);
  //pass_manager_builder.populate_module_pass_manager(&pass_manager);
  //*

  //pass_manager.add_function_inlining_pass();
  pass_manager.add_always_inliner_pass();
  pass_manager.add_partially_inline_lib_calls_pass();
  pass_manager.add_tail_call_elimination_pass();
  pass_manager.run_on(&ctx.module);

  pass_manager.add_merge_functions_pass();
  pass_manager.add_lower_expect_intrinsic_pass();
  pass_manager.add_jump_threading_pass();
  pass_manager.add_memcpy_optimize_pass();
  pass_manager.add_cfg_simplification_pass();
  pass_manager.run_on(&ctx.module);
  //---------------------------------------------------------------------------
  pass_manager.add_scalarizer_pass();
  pass_manager.add_slp_vectorize_pass();
  pass_manager.add_loop_vectorize_pass();
  pass_manager.add_merged_load_store_motion_pass();
  //---------------------------------------------------------------------------
  pass_manager.add_gvn_pass();
  pass_manager.add_lower_switch_pass();

  pass_manager.add_licm_pass();
  pass_manager.run_on(&ctx.module);

  pass_manager.add_function_inlining_pass();
  pass_manager.run_on(&ctx.module);

  pass_manager.add_global_optimizer_pass();
  pass_manager.add_global_dce_pass();
  pass_manager.add_instruction_simplify_pass();
  pass_manager.add_aggressive_dce_pass();
  pass_manager.run_on(&ctx.module);

  //  pass_manager.add_demote_memory_to_register_pass();
  //  pass_manager.run_on(&ctx.module);
}
