use std::collections::BTreeMap;
use std::fs::File;
use std::path::PathBuf;

use crate::builder::common;
use crate::builder::disclaimer::DISCLAIMER;
use crate::options::Architecture;
use crate::options::BuildOptions;
use crate::options::Recognizer;
use crate::writer::code_writer::CodeWriter;
use hctk::bytecode::compile_bytecode;
use hctk::debug;
use hctk::debug::BytecodeGrammarLookups;
use hctk::get_num_of_available_threads;
use hctk::grammar::compile_from_path;
use hctk::types::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::passes::PassManagerBuilder;
use inkwell::targets::CodeModel;
use inkwell::targets::FileType;
use inkwell::targets::InitializationConfig;
use inkwell::targets::RelocMode;
use inkwell::targets::Target;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::targets::TargetTriple;
use inkwell::OptimizationLevel;
use std::io::BufWriter;
use std::io::Write;
use std::process::Command;
use std::thread;

pub enum OutputType
{
  Rust,
  Cpp,
  TypeScript,
  JavaScript,
  Java,
}

fn initializeLLVMTargets()
{
  Target::initialize_x86(&InitializationConfig::default());
}

/// Compile grammar into a machine-code based parser for the given architecture and language
pub fn compile_llvm_files(
  input_path: &PathBuf,
  output_path: &PathBuf,
  build_ast: bool,
  target_triple: Option<String>,
)
{
  let build_options = BuildOptions {
    recognizer: Recognizer::Assembly,
    architecture: Architecture::X8664,
    ..Default::default()
  };

  initializeLLVMTargets();

  eprintln!("Input file: {:?}\n Output file: {:?}", input_path, output_path);

  let target_triple =
    target_triple.unwrap_or(std::env::var("TARGET").unwrap_or(String::default()));

  let threads = get_num_of_available_threads();

  let (grammar, errors) = compile_from_path(input_path, threads);

  if !errors.is_empty() {
    for error in errors {
      println!("cargo:error=\n{}", error);
    }
  } else if let Some(grammar) = grammar {
    let (grammar_name, parser_name) = common::get_parser_names(&grammar);

    let bytecode_output = compile_bytecode(&grammar, 1);

    thread::scope(|scope| {
      if build_ast {
        scope.spawn(|| {
          let output_path = if let Ok(output_path) =
            std::env::var("OUT_DIR").map(|d| PathBuf::from(&d))
          {
            output_path
          } else {
            output_path.clone()
          };

          let log_file = output_path.join(parser_name.clone() + ".log");
          let ll_file = output_path.join(parser_name.clone() + ".ll");
          let object_path = output_path.join("lib".to_string() + &parser_name + ".o");
          let archive_path = output_path.join(format!("./lib{}.a", &parser_name));

          let target_triple = TargetTriple::create(&target_triple);
          let mut target_err = String::default();

          if let Ok(parser_data_file) = std::fs::File::create(log_file) {
            let mut writer = BufWriter::new(parser_data_file);
            writer.write_fmt(format_args!("target triple: {}\n", target_triple));
            writer.write_fmt(format_args!("parse name: {}\n", &parser_name));
            writer.write_fmt(format_args!(
              "production count: {}\n",
              &grammar.production_table.len()
            ));
            writer
              .write_fmt(format_args!("body count: {}\n", &grammar.bodies_table.len()));

            writer.write_fmt(format_args!(
              "symbol count: {}\n",
              &grammar.symbols_table.len()
            ));
            writer.write_fmt(format_args!("target: {:?}\n", target_err));
            writer.flush();
          }

          if let Ok(ctx) = crate::llvm::compile_from_bytecode(
            &parser_name,
            &Context::create(),
            &build_options,
            &bytecode_output,
          ) {
            let mut file = File::create(ll_file).unwrap();
            file.write_all(ctx.module.to_string().as_bytes());
            file.flush();
            drop(file);

            let opt = OptimizationLevel::Less;
            let reloc = RelocMode::PIC;
            let model = CodeModel::Small;
            let target = Target::from_triple(&target_triple).unwrap();
            let target_machine = target
              .create_target_machine(&target_triple, "generic", "", opt, reloc, model)
              .unwrap();

            // apply_llvm_optimizations(opt, &ctx, &target_machine);

            ctx
              .module
              .set_data_layout(&target_machine.get_target_data().get_data_layout());

            ctx.module.set_triple(&target_triple);

            match target_machine.write_to_file(
              &ctx.module,
              FileType::Object,
              &object_path,
            ) {
              Ok(_) => {
                println!(
                  "cargo:rustc-link-search=native={}",
                  output_path.to_str().unwrap()
                );
                println!("cargo:rustc-link-lib=static={}", parser_name);
                if !(Command::new("llvm-ar-14")
                  .args(&[
                    "rc",
                    archive_path.to_str().unwrap(),
                    object_path.to_str().unwrap(),
                  ])
                  .status()
                  .unwrap()
                  .success())
                {
                  panic!("failed");
                }
              }
              Err(err) => {
                panic!("failed");
              }
            }
          }
        });
        scope.spawn(|| {
          let data_path = output_path.join(format!("./{}.rs", parser_name));
          if let Ok(parser_data_file) = std::fs::File::create(data_path) {
            let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

            writer.write(&DISCLAIMER(&grammar_name, "Parser Data", "//!"));

            let output_type = OutputType::Rust;

            match output_type {
              OutputType::Rust => {
                write_rust_parser(
                  writer,
                  &bytecode_output.state_name_to_offset,
                  &grammar,
                  &grammar_name,
                  &parser_name,
                );
              }
              _ => {}
            };
          }
        });
      }
    })
  }
}

fn apply_llvm_optimizations(
  opt: OptimizationLevel,
  ctx: &crate::llvm::LLVMParserModule,
  target_machine: &TargetMachine,
)
{
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
  target_machine.add_analysis_passes(&pass_manager);
}

fn write_rust_parser<W: Write>(
  mut writer: CodeWriter<W>,
  state_lookups: &BTreeMap<String, u32>,
  grammar: &GrammarStore,
  grammar_name: &str,
  parser_name: &str,
) -> std::io::Result<()>
{
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

  common::write_rust_entry_function(grammar, state_lookups, &mut writer);

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
