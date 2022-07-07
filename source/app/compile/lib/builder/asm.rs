use std::path::PathBuf;

use crate::builder::disclaimer::DISCLAIMER;
use crate::writer::code_writer::CodeWriter;
use crate::writer::nasm_writer::NasmWriter;
use crate::writer::x86_64_writer::X8664Writer;
use hctk::bytecode::compile_bytecode;
use hctk::bytecode::BytecodeOutput;
use hctk::get_num_of_available_threads;
use hctk::grammar::compile_from_path;
use hctk::grammar::get_exported_productions;
use hctk::grammar::ExportedProduction;
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
pub fn compile_asm_files(
    input_path: &PathBuf,
    output_path: &PathBuf,
    build_ast: bool,
    // Target Architecture
    // Target Language
)
{
    eprintln!("Input file: {:?}\n Output file: {:?}", input_path, output_path);

    let threads = get_num_of_available_threads();

    let (grammar, errors) = compile_from_path(input_path, threads);

    if !errors.is_empty() {
        for error in errors {
            println!("cargo:error=\n{}", error);
        }
    } else if let Some(grammar) = grammar {
        let grammar_name = grammar.friendly_name.to_owned();
        let parser_name = grammar_name.to_owned() + "_parser";

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

                    let asm_path =
                        output_path.join(parser_name.clone() + ".asm");
                    let object_path =
                        output_path.join(parser_name.clone() + ".o");
                    let archive_path =
                        output_path.join(format!("./lib{}.a", &parser_name));

                    if let Ok(asm_file) = std::fs::File::create(&asm_path) {
                        let mut writer =
                            NasmWriter::new(BufWriter::new(asm_file));

                        writer.line(&DISCLAIMER(
                            &parser_name,
                            "Parse x86 Assembly",
                            "; ",
                        ));

                        if crate::asm::x86_64_asm::compile_from_bytecode(
                            &bytecode_output,
                            &mut writer,
                        )
                        .is_ok()
                        {
                            let mut file_writer = writer.into_writer();
                            file_writer.flush();
                            drop(file_writer);

                            match Command::new("nasm")
                                .args(&[
                                    "-g",
                                    "-f",
                                    "elf64",
                                    "-o",
                                    object_path.to_str().unwrap(),
                                    asm_path.to_str().unwrap(),
                                ])
                                .status()
                            {
                                Ok(_) => {
                                    if !(Command::new("ar")
                                        .args(&[
                                            "-crus",
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
                                        println!(
                                            "cargo:rustc-link-lib=static={}",
                                            parser_name
                                        );
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
                    let data_path =
                        output_path.join(format!("./{}.rs", parser_name));
                    if let Ok(parser_data_file) =
                        std::fs::File::create(data_path)
                    {
                        let mut writer =
                            CodeWriter::new(BufWriter::new(parser_data_file));

                        writer.write(&DISCLAIMER(
                            &grammar_name,
                            "Parser Data",
                            "//!",
                        ));

                        let output_type = OutputType::Rust;

                        match output_type {
                            OutputType::Rust => {
                                write_rust_parser(
                                    writer,
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
    fn next(ctx: AnonymousPtr) -> &'static ParseAction;
    fn destroy_context(ctx: AnonymousPtr);
    fn prime_context(ctx: AnonymousPtr, sp:u64);
}}",
            parser_name
        ))?
        .wrt(
            "
pub enum StartPoint {",
        )?
        .indent();

    for (
        i,
        ExportedProduction {
            export_name,
            production,
            ..
        },
    ) in get_exported_productions(grammar).iter().enumerate()
    {
        writer
            .wrtln(&format!(
                "/// `{}`",
                production
                    .original_location
                    .to_string()
                    .replace("\n", "\n// ")
            ))?
            .wrtln(&format!("{} = {},", export_name.to_uppercase(), i))?;
    }

    writer.dedent().wrtln("}")?.wrtln(&format!(
        "pub struct Context<T: SymbolReader>(ASMParserContext<T>);

impl<T: SymbolReader> Iterator for Context<T> {{
    type Item = ParseAction;

    fn next(&mut self) -> Option<Self::Item> {{
        unsafe {{
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            Some(*next(_ptr as u64))
        }}
    }}
}}

impl<T: SymbolReader> Context<T> {{
    /// Create a new parser context to parser the input with 
    /// the grammar `{0}`
    pub fn new(reader: &mut T) -> Self {{
        let mut parser = Self(ASMParserContext::<T>::new(reader));
        parser.construct_context();
        parser
    }}
    
    /// Initialize the parser to recognize the given starting production
    /// within the input. This method is chainable.
    pub fn set_start_point(&mut self, start_point: StartPoint) -> &mut Self {{
        unsafe {{
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            prime_context(_ptr as u64, start_point as u64);
        }}

        self
    }}

    fn construct_context(&mut self) {{
        unsafe {{
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            construct_context(_ptr as u64);
        }}
    }}

    fn destroy_context(&mut self) {{
        unsafe {{
            let _ptr = &mut self.0 as *const ASMParserContext<T>;
            destroy_context(_ptr as u64);
        }};
    }}
}}

impl<T: SymbolReader> Drop for Context<T> {{
    fn drop(&mut self) {{
        self.destroy_context();
    }}
}}
",
        grammar_name
    ));

    Ok(())
}
