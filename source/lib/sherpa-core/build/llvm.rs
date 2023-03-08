use inkwell::{
  context::Context,
  passes::{PassManager, PassManagerBuilder},
  targets::{
    CodeModel,
    FileType,
    InitializationConfig,
    RelocMode,
    Target,
    TargetMachine,
    TargetTriple,
  },
  OptimizationLevel,
};
use std::{io::Write, process::Command};

use crate::{
  ascript::{
    output_base::AscriptWriter,
    rust::{create_rust_writer_utils, write_rust_llvm_parser_file},
    types::AScriptStore,
  },
  grammar::compile::DummyASTEnum,
  llvm::construct_module,
  types::*,
  writer::code_writer::CodeWriter,
};

use super::pipeline::PipelineTask;

/// Constructs a task that outputs a Rust parse context interface
/// for the llvm parser.
pub fn build_llvm_parser_interface<'a>() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let mut j = task_ctx.get_journal();
      let grammar = j.grammar().unwrap();
      let parser_name = grammar.id.name.clone();

      let dummy = AScriptStore::dummy(&mut j).unwrap();
      let store = if let Some(store) = task_ctx.get_ascript() { store } else { &dummy };

      let output_type = OutputType::Rust;

      match output_type {
        OutputType::Rust => {
          let writer = CodeWriter::new(vec![]);
          let utils = create_rust_writer_utils(store);
          let w = AscriptWriter::new(&utils, writer);
          match write_rust_llvm_parser_file(w, &parser_name, &parser_name) {
            SherpaResult::Err(err) => Err(vec![SherpaError::from(err)]),
            SherpaResult::Ok(w) => {
              Ok(Some((20, unsafe { String::from_utf8_unchecked(w.into_writer().into_output()) })))
            }
            _ => unreachable!(),
          }
        }
        _ => Ok(None),
      }
    }),
    require_ascript: false,
    require_bytecode: false,
    require_states: true,
  }
}

pub enum OutputType {
  Rust,
  _Cpp,
  _TypeScript,
  _JavaScript,
  _Java,
}

/// Build artifacts for a LLVM based parser.
///
/// # Args
/// `target_triple` - A LLVM compatible target triple
///
/// `clang_command` - The name of the clang executable that can be called from a terminal
///
/// `output_cargo_build_commands` - Set to true to output build commands that inform the rust compiler of the link
/// targets.
///
/// `output_llvm_ir_file` - Output a purely decorational version of the LLVM code in intermediate representational form.
pub fn build_llvm_parser(
  target_triple: Option<String>,
  output_cargo_build_commands: bool,
  output_llvm_ir_file: bool,
) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let output_path = task_ctx.get_build_output_dir();
      let mut j = task_ctx.get_journal();
      let grammar = j.grammar().unwrap();
      let parser_name = grammar.id.name.clone();

      let light_lto = j.config().llvm_light_lto;
      let clang_command = j.config().llvm_clang_path.clone();
      let ar_command = j.config().llvm_ar_path.clone();

      let Some(states) = task_ctx.get_states() else {
        return Err(vec![SherpaError::from("Cannot compile LLVM parse: Parse states are not available")]);
      };

      let ll_file_path = output_path.join(parser_name.clone() + ".ll");
      let bitcode_path = output_path.join("lib".to_string() + &parser_name + ".bc");
      let object_path = output_path.join("lib".to_string() + &parser_name + ".o");
      let archive_path = output_path.join(format!("./lib{}.a", &parser_name));

      if output_cargo_build_commands {
        println!("cargo:rustc-link-search=native={}", output_path.to_str().unwrap());
        println!("cargo:rustc-link-lib=static={}", parser_name);
      }

      let ctx = Context::create();

      // Setup Target Machine -------------------------------------------------------------------
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
      let opt = OptimizationLevel::Less;
      let target_machine =
        target.create_target_machine(&target_triple, "generic", "", opt, reloc, model).unwrap();

      let target_data = target_machine.get_target_data();

      // Setup module ---------------------------------------------------------------------------
      let module = ctx.create_module(&parser_name);
      module.set_triple(&target_triple);
      module.set_data_layout(&target_data.get_data_layout());

      let mut llvm_mod = construct_module(&mut j, &ctx, &target_data, module);

      match crate::llvm::compile_llvm_module_from_parse_states(&mut j, &mut llvm_mod, states) {
        SherpaResult::Ok(()) => {
          if task_ctx.get_journal().config().enable_ascript {
            unsafe {
              crate::llvm::ascript_functions::construct_ast_builder::<DummyASTEnum>(&llvm_mod)
                .unwrap();
            }
          }

          if false && light_lto {
            if output_llvm_ir_file {
              if let Ok(mut file) = task_ctx.create_file(ll_file_path.clone()) {
                file.write_all(llvm_mod.module.to_string().as_bytes()).unwrap();
                file.flush().unwrap();
              }
            }

            task_ctx.add_artifact_path(bitcode_path.clone());
            if false && llvm_mod.module.write_bitcode_to_path(&bitcode_path) {
              match Command::new(clang_command.clone())
                .args(&["-c", "-o", object_path.to_str().unwrap(), bitcode_path.to_str().unwrap()])
                .status()
              {
                Ok(_) => {
                  task_ctx.add_artifact_path(object_path.clone());
                  if !(Command::new(ar_command)
                    .args(&["rc", archive_path.to_str().unwrap(), object_path.to_str().unwrap()])
                    .status()
                    .unwrap()
                    .success())
                  {
                    Err(vec![SherpaError::from("Unable to compile llvm bitcode")])
                  } else {
                    Ok(None)
                  }
                }
                Err(err) => Err(vec![SherpaError::from(err)]),
              }
            } else {
              Err(vec![SherpaError::from("test")])
            }
          } else {
            if output_llvm_ir_file {
              if let Ok(mut file) = task_ctx.create_file(ll_file_path.clone()) {
                file.write_all(llvm_mod.module.to_string().as_bytes()).unwrap();
                file.flush().unwrap();
              }
            }

            //if j.config().opt_llvm {
            apply_llvm_optimizations(opt, &llvm_mod);
            //}

            match target_machine.write_to_file(&llvm_mod.module, FileType::Object, &object_path) {
              Ok(_) => {
                if !(Command::new(ar_command)
                  .args(&["rc", archive_path.to_str().unwrap(), object_path.to_str().unwrap()])
                  .status()
                  .unwrap()
                  .success())
                {
                  Err(vec![SherpaError::from(
                    "Unable to compile llvm bitcode: Incorrect path to LLVM-AR executable",
                  )])
                } else {
                  Ok(None)
                }
              }
              Err(err) => Err(vec![SherpaError::from(err.to_string())]),
            }
          }
        }
        _ => Err(vec![SherpaError::from("Unable to compile llvm bitcode")]),
      }
    }),
    require_ascript: false,
    require_bytecode: false,
    require_states: true,
  }
}

fn apply_llvm_optimizations(opt: OptimizationLevel, ctx: &crate::llvm::LLVMParserModule) {
  println!("Applying optimizations1");
  //return;
  let pass_manager_builder = PassManagerBuilder::create();

  pass_manager_builder.set_optimization_level(opt);
  pass_manager_builder.set_size_level(1);
  //pass_manager_builder.populate_module_pass_manager(&pass_manager);
  //*
  // ---------------------------------
  let pass_manager = PassManager::create(());
  pass_manager.add_global_dce_pass();
  pass_manager.add_strip_symbol_pass();
  pass_manager.add_argument_promotion_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("1");
    break;
  }
  let pass_manager = PassManager::create(());
  pass_manager.add_reassociate_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("1a");
    break;
  }
  // ---------------------------------
  let pass_manager = PassManager::create(());
  pass_manager.add_always_inliner_pass();
  pass_manager.add_partially_inline_lib_calls_pass();
  pass_manager.add_tail_call_elimination_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("2");
  }
  // ---------------------------------
  let pass_manager = PassManager::create(());
  pass_manager.add_merge_functions_pass();
  pass_manager.add_lower_expect_intrinsic_pass();
  pass_manager.add_memcpy_optimize_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("3");
    break;
  }
  // ---------------------------------
  let pass_manager = PassManager::create(());
  ////pass_manager.add_slp_vectorize_pass();
  pass_manager.add_loop_vectorize_pass();
  pass_manager.add_merged_load_store_motion_pass();
  pass_manager.add_lower_switch_pass();
  pass_manager.add_licm_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("4");
  }
  let pass_manager = PassManager::create(());
  pass_manager.add_gvn_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("5");
  }
  let pass_manager = PassManager::create(());
  pass_manager.add_promote_memory_to_register_pass();
  pass_manager.add_instruction_simplify_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("5a");
  }
  // ---------------------------------
  let pass_manager = PassManager::create(());
  pass_manager.add_dead_store_elimination_pass();
  pass_manager.add_dead_arg_elimination_pass();
  pass_manager.add_cfg_simplification_pass();
  pass_manager.add_aggressive_dce_pass();
  pass_manager.add_global_dce_pass();
  pass_manager.add_bit_tracking_dce_pass();
  while pass_manager.run_on(&ctx.module) {
    println!("6");
  }

  //  pass_manager.add_demote_memory_to_register_pass();
  //  pass_manager.run_on(&ctx.module);
}
