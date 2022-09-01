use std::collections::BTreeMap;
use std::fs::File;

use crate::builder::common;
use crate::builder::disclaimer::DISCLAIMER;
use crate::writer::code_writer::CodeWriter;
use crate::CompileError;
use hctk::types::*;
use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::passes::PassManagerBuilder;
use inkwell::targets::CodeModel;
use inkwell::targets::FileType;
use inkwell::targets::InitializationConfig;
use inkwell::targets::RelocMode;
use inkwell::targets::Target;
use inkwell::targets::TargetTriple;
use inkwell::OptimizationLevel;
use std::io::BufWriter;
use std::io::Write;
use std::process::Command;

use crate::builder::pipeline::PipelineTask;

/// Build artifacts for a LLVM based parser.
pub fn build_llvm_parser(
  target_triple: Option<String>,
  clang_command: &str,
  ar_command: &str,
  light_LTO: bool,
  output_cargo_build_commands: bool,
  output_llvm_ir_file: bool,
) -> PipelineTask {
  let clang_command = clang_command.to_string();
  let ar_command = ar_command.to_string();

  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let output_path = task_ctx.get_build_output_dir();
      let parser_name = task_ctx.get_parser_name();
      let grammar = task_ctx.get_grammar();
      let bytecode_output = task_ctx.get_bytecode();

      Target::initialize_x86(&InitializationConfig::default());

      let target_triple =
        target_triple.clone().unwrap_or(std::env::var("TARGET").unwrap_or(String::default()));

      let ll_file_path = output_path.join(parser_name.clone() + ".ll");
      let bitcode_path = output_path.join("lib".to_string() + &parser_name + ".bc");
      let object_path = output_path.join("lib".to_string() + &parser_name + ".o");
      let archive_path = output_path.join(format!("./lib{}.a", &parser_name));

      let target_triple = TargetTriple::create(&target_triple);

      if output_cargo_build_commands {
        println!("cargo:rustc-link-search=native={}", output_path.to_str().unwrap());
        println!("cargo:rustc-link-lib=static={}", parser_name);
      }

      // Write out llvm module to file

      match crate::llvm::compile_from_bytecode(
        &parser_name,
        grammar,
        &Context::create(),
        &bytecode_output,
      ) {
        Ok(ctx) => {
          let opt = OptimizationLevel::Default;

          if output_llvm_ir_file {
            if let Ok(mut file) = task_ctx.create_file(ll_file_path.clone()) {
              file.write_all(ctx.module.to_string().as_bytes());
              file.flush();
            }
          }
          if light_LTO {
            task_ctx.add_artifact_path(bitcode_path.clone());
            if ctx.module.write_bitcode_to_path(&bitcode_path) {
              match Command::new(clang_command.clone())
                .args(&[
                  "-flto=thin",
                  "-c",
                  "-o",
                  object_path.to_str().unwrap(),
                  ll_file_path.to_str().unwrap(),
                ])
                .status()
              {
                Ok(_) => {
                  task_ctx.add_artifact_path(object_path.clone());
                  if !(Command::new(ar_command.clone())
                    .args(&["rc", archive_path.to_str().unwrap(), object_path.to_str().unwrap()])
                    .status()
                    .unwrap()
                    .success())
                  {
                    Err(CompileError::from_string("Unable to compile llvm bitcode"))
                  } else {
                    Ok(())
                  }
                }
                Err(err) => Err(CompileError::from_io_error(&err)),
              }
            } else {
              Err(CompileError::from_string("test"))
            }
          } else {
            // apply_llvm_optimizations(opt, &ctx);
            let reloc = RelocMode::PIC;
            let model = CodeModel::Small;
            let target = Target::from_triple(&target_triple).unwrap();
            let target_machine = target
              .create_target_machine(&target_triple, "generic", "", opt, reloc, model)
              .unwrap();

            ctx.module.set_data_layout(&target_machine.get_target_data().get_data_layout());

            ctx.module.set_triple(&target_triple);

            match target_machine.write_to_file(&ctx.module, FileType::Object, &object_path) {
              Ok(_) => {
                if !(Command::new("llvm-ar-14")
                  .args(&["rc", archive_path.to_str().unwrap(), object_path.to_str().unwrap()])
                  .status()
                  .unwrap()
                  .success())
                {
                  Err(CompileError::from_string("Unable to compile llvm bitcode"))
                } else {
                  Ok(())
                }
              }
              Err(err) => Err(CompileError::from_string(&err.to_string())),
            }
          }
        }
        Err(()) => Err(CompileError::from_string("Unable to compile llvm bitcode")),
      }
    }),
    require_ascript: false,
    require_bytecode: true,
  }
}

/// Constructs a task that outputs a Rust parse context interface
/// for the llvm parser.
pub fn build_llvm_parser_interface<'a>() -> PipelineTask {
  PipelineTask {
    fun: Box::new(|p| {
      let source_path = p.get_source_output_dir();
      let parser_name = p.get_parser_name();
      let grammar_name = p.get_grammar_name();
      let grammar = p.get_grammar();
      let bytecode_output = p.get_bytecode();

      let data_path = source_path.join(format!("./{}.rs", parser_name));

      match std::fs::File::create(data_path) {
        Ok(parser_data_file) => {
          let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

          match writer.write(&DISCLAIMER(&grammar_name, "Parser Data", "//!")) {
            Ok(_) => {}
            Err(err) => return Err(CompileError::from_io_error(&err)),
          }

          let output_type = OutputType::Rust;

          match output_type {
            OutputType::Rust => {
              match write_rust_parser(
                writer,
                &bytecode_output.state_name_to_offset,
                grammar,
                &grammar_name,
                &parser_name,
              ) {
                Err(err) => Err(CompileError::from_io_error(&err)),
                Ok(_) => Ok(()),
              }
            }
            _ => Ok(()),
          }
        }
        Err(err) => Err(CompileError::from_io_error(&err)),
      }
    }),
    require_ascript: false,
    require_bytecode: true,
  }
}

pub enum OutputType {
  Rust,
  Cpp,
  TypeScript,
  JavaScript,
  Java,
}

fn apply_llvm_optimizations(opt: OptimizationLevel, ctx: &crate::llvm::LLVMParserModule) {
  let pass_manager_builder = PassManagerBuilder::create();
  let pass_manager = PassManager::create(());
  pass_manager_builder.set_optimization_level(opt);
  // pass_manager_builder.populate_module_pass_manager(&pass_manager);
  //*
  pass_manager.add_cfg_simplification_pass();
  pass_manager.add_function_inlining_pass();
  pass_manager.add_always_inliner_pass();
  pass_manager.add_partially_inline_lib_calls_pass();
  pass_manager.add_merge_functions_pass();
  pass_manager.add_lower_expect_intrinsic_pass();
  pass_manager.add_jump_threading_pass();
  pass_manager.add_memcpy_optimize_pass();
  pass_manager.run_on(&ctx.module);
  // pass_manager.add_demote_memory_to_register_pass();
  pass_manager.add_scalarizer_pass();
  //------------------------------------------------------------------------
  pass_manager.add_slp_vectorize_pass();
  pass_manager.add_loop_vectorize_pass();
  pass_manager.add_merged_load_store_motion_pass();
  // ---------------------------------
  pass_manager.add_gvn_pass();
  pass_manager.add_lower_switch_pass();
  pass_manager.add_licm_pass();
  pass_manager.run_on(&ctx.module);
  pass_manager.add_function_inlining_pass();
  // */
  pass_manager.run_on(&ctx.module);
  pass_manager.add_global_optimizer_pass();
  pass_manager.add_global_dce_pass();
  pass_manager.add_aggressive_dce_pass();
  pass_manager.run_on(&ctx.module);
}

fn write_rust_parser<W: Write>(
  mut writer: CodeWriter<W>,
  states: &BTreeMap<String, u32>,
  g: &GrammarStore,
  grammar_name: &str,
  parser_name: &str,
) -> std::io::Result<()> {
  writer
    .wrt(&format!(
      "
use hctk::types::*;

#[link(name = \"{}\", kind = \"static\" )]
extern \"C\" {{
    fn init(ctx: *mut u8);
    fn next(ctx: *mut u8, action:*mut u8);
    fn prime(ctx: *mut u8, start_point: u32);
}}",
      parser_name
    ))?
    .wrtln(&format!(
      "pub struct Context<T: LLVMCharacterReader + ByteCharacterReader + ImmutCharacterReader>(LLVMParseContext<T>, bool);

impl<T: LLVMCharacterReader + ByteCharacterReader + ImmutCharacterReader> Iterator for Context<T> {{
    type Item = ParseAction;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {{
        unsafe {{
            if(!self.1) {{
                None
            }} else {{
                let _ptr = &mut self.0 as *const LLVMParseContext<T>;
                let mut action = ParseAction::Undefined;
                let _action = &mut action as *mut ParseAction;
                next(_ptr as *mut u8, _action as *mut u8);

                self.1 = !matches!(action, ParseAction::Accept{{..}}| ParseAction::Error {{ .. }} | ParseAction::EndOfInput {{ .. }});

                Some(action)
            }}
        }}
    }}
}}

impl<T: LLVMCharacterReader + ByteCharacterReader + ImmutCharacterReader> Context<T> {{
    /// Create a new parser context to parser the input with 
    /// the grammar `{0}`
    #[inline(always)]
    fn new(reader: &mut T) -> Self {{
        let mut parser = Self(LLVMParseContext::<T>::new(reader), true);
        parser.construct_context();
        parser
    }}
    
    /// Initialize the parser to recognize the given starting production
    /// within the input. This method is chainable.
    #[inline(always)]
    fn set_start_point(&mut self, start_point: u64) -> &mut Self {{
        unsafe {{
            let _ptr = &mut self.0 as *const LLVMParseContext<T>;
            prime(_ptr as *mut u8, start_point as u32);
        }}

        self
    }}
    #[inline(always)]
    fn construct_context(&mut self) {{
        unsafe {{
            let _ptr = &mut self.0 as *const LLVMParseContext<T>;
            init(_ptr as *mut u8 );
        }}
    }}
    #[inline(always)]
    fn destroy_context(&mut self) {{

    }}",
      grammar_name
    ))?
    .indent();

  common::write_rust_entry_function(g, states, &mut writer);

  writer.dedent().wrtln(&format!(
    "}}

impl<T: LLVMCharacterReader + ByteCharacterReader + ImmutCharacterReader> Drop for Context<T> {{
    fn drop(&mut self) {{
        self.destroy_context();
    }}
}}
",
  ));

  Ok(())
}
