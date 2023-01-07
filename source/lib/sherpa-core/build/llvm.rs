use std::{collections::BTreeMap, io::Write, process::Command};

use inkwell::{
  context::Context,
  passes::{PassManager, PassManagerBuilder},
  targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
  OptimizationLevel,
};

use crate::{
  ascript::{self, rust::create_type_initializer_value},
  grammar::data::ast_node::DummyASTEnum,
  types::*,
  writer::code_writer::CodeWriter,
};

use super::{ascript::get_ascript_export_data, pipeline::PipelineTask};

/// Constructs a task that outputs a Rust parse context interface
/// for the llvm parser.
pub fn build_llvm_parser_interface<'a>() -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| {
      let grammar = task_ctx.get_journal().grammar().unwrap();
      let parser_name = grammar.id.name.clone();

      let Some(bytecode) = task_ctx.get_bytecode() else {
        return Err(vec![SherpaError::from("Cannot compile LLVM Parser Interface: Bytecode is not available")]);
      };

      let output_type = OutputType::Rust;

      match output_type {
        OutputType::Rust => {
          let mut writer = CodeWriter::new(vec![]);
          match write_rust_parser(
            &mut writer,
            &bytecode.state_name_to_offset,
            &grammar,
            &parser_name,
            &parser_name,
            task_ctx.get_ascript(),
          ) {
            Err(err) => Err(vec![SherpaError::from(err)]),
            Ok(_) => Ok(Some((20, unsafe { String::from_utf8_unchecked(writer.into_output()) }))),
          }
        }
        _ => Ok(None),
      }
    }),
    require_ascript: false,
    require_bytecode: true,
  }
}

pub enum OutputType {
  Rust,
  _Cpp,
  _TypeScript,
  _JavaScript,
  _Java,
}

fn write_rust_parser<W: Write>(
  writer: &mut CodeWriter<W>,
  states: &BTreeMap<String, u32>,
  g: &GrammarStore,
  grammar_name: &str,
  parser_name: &str,
  ascript: Option<&ascript::types::AScriptStore>,
) -> std::io::Result<()> {
  writer
    .wrt(&format!(
      "

#[link(name = \"{}\", kind = \"static\" )]
extern \"C\" {{
    fn init(ctx: *mut u8, reader: *mut u8);
    fn next(ctx: *mut u8) -> ParseActionType;
    fn prime(ctx: *mut u8, start_point: u32);
    fn drop(ctx: *mut u8);
}}",
      parser_name
    ))?
    .wrtln(&format!(
      r###"
pub trait Reader:
  ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug
  {{}}

impl<T> Reader for T
  where T: ByteReader + LLVMByteReader + MutByteReader + std::fmt::Debug 
  {{}}
      
pub struct Parser<T: Reader, M>(ParseContext<T, M>, T);


impl<T: Reader, M> Iterator for Parser<T, M> {{
    type Item = ParseActionType;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {{
        
        unsafe {{
            if !self.0.is_active {{
                None
            }} else {{
                let _ptr = &mut self.0 as *const ParseContext<T, M>;
                Some(next(_ptr as *mut u8))
            }}
        }}
    }}
}}


impl<T: Reader, M> Parser<T, M> {{
    /// Create a new parser context to parser the input with 
    /// the grammar `{0}`
    #[inline(always)]
    fn new(mut reader: T) -> Self {{
        let mut parser = Self(ParseContext::<T, M>::new_llvm(), reader);
        parser.construct_context();
        parser
    }}
    
    /// Initialize the parser to recognize the given starting production
    /// within the input. This method is chainable.
    #[inline(always)]
    fn set_start_point(&mut self, start_point: u64) -> &mut Self {{
        unsafe {{
            let _ptr = &mut self.0 as *const ParseContext<T, M>;
            prime(_ptr as *mut u8, start_point as u32);
        }}

        self
    }}
    #[inline(always)]
    fn construct_context(&mut self) {{
        unsafe {{
            let _ptr = &mut self.0 as *const ParseContext<T, M>;
            let _rdr = &mut self.1 as *const T;
            init(_ptr as *mut u8, _rdr as *mut u8);
        }}
    }}
    #[inline(always)]
    fn destroy_context(&mut self) {{
      let _ptr = &mut self.0 as *const ParseContext<T, M>;
      unsafe {{ drop(_ptr as *mut u8); }}
    }}"###,
      grammar_name
    ))?
    .indent();

  for (i, ExportedProduction { export_name, production, .. }) in
    g.get_exported_productions().iter().enumerate()
  {
    writer
      .newline()?
      .wrtln(&format!("/// `{}`", production.loc.to_string().replace("\n", "\n// ")))?
      .wrtln(&format!("pub fn new_{}_parser(reader: T) -> Self{{", export_name))?
      .indent()
      .wrtln("let mut ctx = Self::new(reader);")?
      .wrtln(&format!("ctx.set_start_point({});", i))?
      .wrtln("ctx")?
      .dedent()
      .wrtln("}")?
      .newline()?;
  }

  writer.dedent().wrtln(
    "}

impl<T: Reader, M> Drop for Parser<T, M> {
    fn drop(&mut self) {
        self.destroy_context();
    }
}",
  )?;

  if let Some(ascript) = ascript {
    let export_node_data = get_ascript_export_data(g, ascript);

    writer.wrtln("pub mod ast  {")?.indent();
    writer.wrtln("use super::*; ")?;

    // Create a module that will store convience functions for compiling AST
    // structures based on on grammar entry points.

    for (ref_, ast_type, ast_type_string, export_name, _) in &export_node_data {
      writer
        .newline()?
        .wrt(&format!(
          "
impl AstObject for {0} {{}}
type ASTSlot = ({0}, TokenRange, TokenRange);

#[link(name = \"{1}\", kind = \"static\" )]
extern \"C\" {{
  fn ast_parse(
    ctx: *mut u8,
    reducers: *const u8,
    shift_handler: *const u8,
    result_handler: *const u8,
  ) -> ParseResult<{0}>;
}}
",
          ascript.ast_type_name, parser_name
        ))?
        .newline()?
        .wrtln(&format!(
          "pub fn {0}_from(reader: UTF8StringReader)  -> Result<{1}, SherpaParseError> {{ ",
          export_name, ast_type_string
        ))?
        .indent()
        .wrtln(&format!(
          "
const reduce_functions: ReduceFunctions::<UTF8StringReader, u32> = ReduceFunctions::<UTF8StringReader, u32>::new();

let mut ctx = Parser::new_{0}_parser(reader);
let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;
let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, {1}> as *const u8;
let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, {1}> as *const u8;
let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;
",
          export_name, ascript.ast_type_name
        ))?
        .wrtln(
          "match unsafe{ ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) } {",
        )?
        .indent()
        .wrtln("ParseResult::Complete((i0, _, _))  => {")?
        .indent()
        .wrtln(&{
          let (string, ref_) =
            create_type_initializer_value(ref_.clone(), &ast_type, false, ascript);
          if let Some(exp) = ref_ {
            format!("{}\nOk({})", exp.to_init_string(), string)
              .split("\n")
              .map(|i| i.trim())
              .collect::<Vec<_>>()
              .join("\n")
          } else {
            "Ok(i0)".to_string()
          }
        })?
        .dedent()
        .wrtln("}")?
        .wrtln(
          "ParseResult::Error(..) => Err(SherpaParseError::None),
_ => unreachable!()",
        )?
        .dedent()
        .wrtln("}")?
        .dedent()
        .wrtln("}")?
        .dedent()
        .wrtln("}")?;
    }
  }

  Ok(())
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

      let Some(bytecode) = task_ctx.get_bytecode() else {
        return Err(vec![SherpaError::from("Cannot compile LLVM parse: Bytecode is not available")]);
      };

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

      match crate::llvm::compile_from_bytecode(&parser_name, &mut j, &Context::create(), &bytecode)
      {
        SherpaResult::Ok(ctx) => {
          if task_ctx.get_journal().config().enable_ascript {
            unsafe {
              crate::llvm::ascript_functions::construct_ast_builder::<DummyASTEnum>(&ctx).unwrap();
            }
          }

          let opt = OptimizationLevel::Aggressive;

          if light_lto {
            if output_llvm_ir_file {
              if let Ok(mut file) = task_ctx.create_file(ll_file_path.clone()) {
                file.write_all(ctx.module.to_string().as_bytes()).unwrap();
                file.flush().unwrap();
              }
            }

            task_ctx.add_artifact_path(bitcode_path.clone());
            if ctx.module.write_bitcode_to_path(&bitcode_path) {
              match Command::new(clang_command.clone())
                .args(&[
                  "-flto=thin",
                  "-c",
                  "-o",
                  object_path.to_str().unwrap(),
                  bitcode_path.to_str().unwrap(),
                ])
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
                file.write_all(ctx.module.to_string().as_bytes()).unwrap();
                file.flush().unwrap();
              }
            }

            let reloc = RelocMode::PIC;
            let model = CodeModel::Small;
            let target = Target::from_triple(&target_triple).unwrap();
            let target_machine = target
              .create_target_machine(&target_triple, "generic", "", opt, reloc, model)
              .unwrap();

            ctx.module.set_triple(&target_triple);
            ctx.module.set_data_layout(&target_machine.get_target_data().get_data_layout());

            if j.config().opt_llvm {
              apply_llvm_optimizations(opt, &ctx);
            }

            match target_machine.write_to_file(&ctx.module, FileType::Object, &object_path) {
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
    require_bytecode: true,
  }
}

fn apply_llvm_optimizations(opt: OptimizationLevel, ctx: &crate::llvm::LLVMParserModule) {
  //return;
  let pass_manager_builder = PassManagerBuilder::create();
  let pass_manager = PassManager::create(());
  pass_manager_builder.set_optimization_level(opt);
  //pass_manager_builder.populate_module_pass_manager(&pass_manager);
  //*

  pass_manager.add_function_inlining_pass();
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
  //------------------------------------------------------------------------
  pass_manager.add_scalarizer_pass();
  pass_manager.add_slp_vectorize_pass();
  pass_manager.add_loop_vectorize_pass();
  pass_manager.add_merged_load_store_motion_pass();
  // ---------------------------------
  pass_manager.add_gvn_pass();
  pass_manager.add_lower_switch_pass();

  pass_manager.add_licm_pass();
  pass_manager.run_on(&ctx.module);

  pass_manager.add_function_inlining_pass();
  pass_manager.run_on(&ctx.module);

  pass_manager.add_global_optimizer_pass();
  pass_manager.add_global_dce_pass();
  pass_manager.add_aggressive_dce_pass();
  pass_manager.run_on(&ctx.module);

  pass_manager.add_instruction_simplify_pass();
  pass_manager.run_on(&ctx.module);
  //  pass_manager.add_demote_memory_to_register_pass();
  //  pass_manager.run_on(&ctx.module);
}
