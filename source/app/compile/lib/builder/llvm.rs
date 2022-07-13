use std::collections::BTreeMap;
use std::path::PathBuf;

use crate::builder::common;
use crate::builder::disclaimer::DISCLAIMER;
use crate::options::Architecture;
use crate::options::BuildOptions;
use crate::options::Recognizer;
use crate::writer::code_writer::CodeWriter;
use crate::writer::nasm_writer::NasmWriter;
use crate::writer::x86_64_writer::X8664Writer;
use hctk::bytecode::compile_bytecode;
use hctk::get_num_of_available_threads;
use hctk::grammar::compile_from_path;
use hctk::types::*;
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

/// Compile grammar into a machine-code based parser for the given architecture and language
pub fn compile_llvm_files(
  input_path: &PathBuf,
  output_path: &PathBuf,
  build_ast: bool,
  // Target Architecture
  // Target Language
)
{
  let build_options = BuildOptions {
    recognizer: Recognizer::Assembly,
    architecture: Architecture::X8664,
    ..Default::default()
  };

  eprintln!("Input file: {:?}\n Output file: {:?}", input_path, output_path);

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

          let llvm_source_path = output_path.join(parser_name.clone() + ".ll");
          let bit_code_path = output_path.join(parser_name.clone() + ".bc");
          let object_path = output_path.join(parser_name.clone() + ".o");
          let archive_path = output_path.join(format!("./lib{}.a", &parser_name));

          if let Ok(asm_file) = std::fs::File::create(&llvm_source_path) {
            let mut writer = NasmWriter::new(BufWriter::new(asm_file));

            writer.line(&DISCLAIMER(&parser_name, "Parse LLVM IR", "; "));

            if crate::llvm::compile_from_bytecode(
              &build_options,
              &bytecode_output,
              &mut writer,
            )
            .is_ok()
            {
              let mut file_writer = writer.into_writer();

              file_writer.flush();

              drop(file_writer);

              match Command::new("clang-14")
                .args(&[
                  "-flto=thin",
                  "-O3",
                  "-c",
                  "-o",
                  object_path.to_str().unwrap(),
                  llvm_source_path.to_str().unwrap(),
                ])
                .status()
              {
                Ok(_) => {
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
                  } else {
                    println!(
                      "cargo:rustc-link-search=native={}",
                      output_path.to_str().unwrap()
                    );
                    println!("cargo:rustc-link-lib=static={}", parser_name);
                  }
                }
                Err(err) => {
                  println!("cargo:error={}", err);
                }
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

type AnonymousPtr = u64;

#[link(name = \"{}\", kind = \"static\" )]
extern \"C\" {{
    fn construct_context(ctx: AnonymousPtr);
    fn next<'a>(ctx: AnonymousPtr, action_ref:&'a mut ParseAction);
    fn destroy_context(ctx: AnonymousPtr);
    fn prime_context(ctx: AnonymousPtr, sp:u64);
}}",
      parser_name
    ))?
    .wrtln(&format!(
      "pub struct Context<T: CharacterReader>(ParseContext<T>, bool);

impl<T: CharacterReader> Iterator for Context<T> {{
    type Item = ParseAction;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {{
        unsafe {{
            if(!self.1) {{
                None
            }} else {{
                let _ptr = &mut self.0 as *const ParseContext<T>;
                let mut action = ParseAction::Undefined;
                next(_ptr as u64, &mut action);

                self.1 = !matches!(action, ParseAction::Accept{{..}});

                Some(action)
            }}
        }}
    }}
}}

impl<T: CharacterReader> Context<T> {{
    /// Create a new parser context to parser the input with 
    /// the grammar `{0}`
    #[inline(always)]
    fn new(reader: &mut T) -> Self {{
        let mut parser = Self(ParseContext::<T>::new(reader), true);
        parser.construct_context();
        parser
    }}
    
    /// Initialize the parser to recognize the given starting production
    /// within the input. This method is chainable.
    #[inline(always)]
    fn set_start_point(&mut self, start_point: u64) -> &mut Self {{
        unsafe {{
            let _ptr = &mut self.0 as *const ParseContext<T>;
            prime_context(_ptr as u64, start_point);
        }}

        self
    }}
    #[inline(always)]
    fn construct_context(&mut self) {{
        unsafe {{
            let _ptr = &mut self.0 as *const ParseContext<T>;
            construct_context(_ptr as u64);
        }}
    }}
    #[inline(always)]
    fn destroy_context(&mut self) {{
        unsafe {{
            let _ptr = &mut self.0 as *const ParseContext<T>;
            destroy_context(_ptr as u64);
        }};
    }}",
      grammar_name
    ))?
    .indent();

  common::write_rust_entry_function(grammar, state_lookups, &mut writer);

  writer.dedent().wrtln(&format!(
    "}}

impl<T: CharacterReader> Drop for Context<T> {{
    fn drop(&mut self) {{
        self.destroy_context();
    }}
}}
",
  ));

  Ok(())
}
