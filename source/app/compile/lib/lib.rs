#![feature(scoped_threads)]
#![feature(is_some_with)]
#![feature(const_format_args)]
#![feature(const_fmt_arguments_new)]
#![feature(box_patterns)]
/// Functions for building parsers
use std::collections::BTreeMap;
use std::io::BufWriter;
use std::io::Result;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::thread;
mod asm;
mod ast;
mod writer;

use ast::*;
use writer::*;

use code_writer::*;

use hctk::ascript::compile_ascript::compile_reduce_function_expressions;
use hctk::bytecode::compile_bytecode::build_byte_code_buffer;
use hctk::debug::BytecodeGrammarLookups;
use hctk::get_num_of_available_threads;
use hctk::grammar::compile_from_path;
use hctk::grammar::get_production_plain_name;
use hctk::grammar::parse::compile_ir_ast;
use hctk::intermediate::state_construction::compile_states;
use hctk::types::*;

static DISCLAIMER: fn(&str, &str) -> String = |source_file, file_type| {
    format!(
        "//! ### `{1}` {2}
//!
//! - **GENERATOR** :   hc-compile {0}
//! - **SOURCE FILE**:  {1}
//!
//! #### WARNING:
//!
//! This is a generated file. Any changes to this file may be
//! overwritten without notice.
//!
//! #### Copyright
//! 
//! (C) 2022 Anthony Weathersby and the Hydrocarbon Toolkit Authors.

",
        env!("CARGO_PKG_VERSION"),
        source_file,
        file_type
    )
};

/// Compile grammar into files written to output_path.
pub fn build_files(input_path: &PathBuf, output_path: &PathBuf, build_ast: bool)
{
    eprintln!("Input file: {:?}\n Output file: {:?}", input_path, output_path);

    let threads = get_num_of_available_threads();

    let input_file_name = input_path.file_name().unwrap().to_str().unwrap();

    eprintln!("Number of threads used: {}", threads);

    let (grammar, errors) = compile_from_path(input_path, threads);

    if !errors.is_empty() {
        for error in errors {
            eprintln!("{}", error);
        }
    } else if let Some(grammar) = grammar {
        thread::scope(|scope| {
            if build_ast {
                scope.spawn(|| {
                    if let Ok(ast_data_file) =
                        std::fs::File::create(output_path.join("./ast.rs"))
                    {
                        let mut writer =
                            CodeWriter::new(BufWriter::new(ast_data_file));

                        writer.write(&DISCLAIMER(input_file_name, "AST Data"));

                        if let Err(err) =
                            write_ascript_data(&grammar, writer, rust::write)
                        {
                            eprintln!("Problem writing ast.rs:\n{}", err);
                        }
                    } else {
                        println!("cargo:warning=Could not write ast.rs");
                    }
                });
            }

            scope.spawn(|| {
                if let Ok(parser_data_file) =
                    std::fs::File::create(output_path.join("./parser_data.rs"))
                {
                    let mut writer =
                        CodeWriter::new(BufWriter::new(parser_data_file));

                    writer.write(&DISCLAIMER(input_file_name, "Parser Data"));

                    write_parser_data(
                        writer,
                        &grammar,
                        &output_path.join("./parser_data.rs"),
                        // Leave two threads available for building
                        // the
                        // ascript code if necessary
                        1.max(
                            threads
                                .checked_sub(build_ast as usize * 2)
                                .unwrap_or(1),
                        ),
                    );
                }
            });
        })
    }
}

/// Produce the disassembly of grammar.
pub fn generate_disassembly(input_path: &PathBuf) -> String
{
    eprintln!("Input file: {:?}\n ", input_path);

    let threads = get_num_of_available_threads();

    eprintln!("Number of threads used: {}", threads);

    let (grammar, errors) = compile_from_path(input_path, threads);

    if !errors.is_empty() {
        for error in errors {
            eprintln!("{}", error);
        }
        String::new()
    } else if let Some(grammar) = grammar {
        let BytecodeOutput { bytecode, .. } =
            compile_bytecode(&grammar, threads);

        hctk::debug::bytecode::generate_disassembly(
            &bytecode,
            Some(&BytecodeGrammarLookups::new(&grammar)),
        )
    } else {
        String::new()
    }
}

type AScriptSyntaxWriter<W> =
    fn(&GrammarStore, &AScriptStore, &mut CodeWriter<W>) -> Result<()>;

fn write_ascript_data<W: Write>(
    grammar: &GrammarStore,
    mut writer: CodeWriter<W>,
    syntax_writer: AScriptSyntaxWriter<W>,
) -> std::io::Result<()>
{
    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(grammar, &mut ascript);

    if !errors.is_empty() {
        for error in &errors {
            println!("{}", error);
        }
    } else {
        syntax_writer(grammar, &ascript, &mut writer)?;
        writer.into_output();
    }

    Ok(())
}

fn write_parser_data<W: Write>(
    mut writer: CodeWriter<W>,
    grammar: &GrammarStore,
    output_path: &Path,
    threads: usize,
) -> std::io::Result<()>
{
    let BytecodeOutput {
        bytecode,
        state_name_to_instruction_offset: state_lookups,
    } = compile_bytecode(grammar, threads);

    if let Err(err) =
        write_rust_data_file(writer, &state_lookups, grammar, &bytecode)
    {
        println!("{}", err);
    }

    Ok(())
}

struct BytecodeOutput
{
    bytecode: Vec<u32>,
    state_name_to_instruction_offset: BTreeMap<String, u32>,
}

fn compile_bytecode(grammar: &GrammarStore, threads: usize) -> BytecodeOutput
{
    let state_asts = compile_states(grammar, threads)
        .values()
        .map(|s| {
            let string = s.get_code();

            match compile_ir_ast(Vec::from(string.as_bytes())) {
                Ok(ast) => *ast,
                Err(err) => {
                    panic!("\n{}", err);
                }
            }
        })
        .collect::<Vec<_>>();

    let state_refs = state_asts.iter().collect::<Vec<_>>();

    let (bytecode, state_lookups) = build_byte_code_buffer(state_refs);

    BytecodeOutput {
        bytecode,
        state_name_to_instruction_offset: state_lookups,
    }
}

fn write_rust_data_file<W: Write>(
    mut writer: CodeWriter<W>,
    state_lookups: &BTreeMap<String, u32>,
    grammar: &GrammarStore,
    bytecode: &Vec<u32>,
) -> std::io::Result<()>
{
    for (production_id, export_name) in &grammar.export_names {
        let production_name =
            get_production_plain_name(production_id, grammar).to_string();

        if let Some(bytecode_offset) = state_lookups.get(&production_name) {
            writer
                .wrt(&format!(
                    "pub const EntryPoint_{}: u32 = {};",
                    export_name, bytecode_offset
                ))?
                .newline()?;
        } else {
            println!(
                "Unable to get bytecode offset for entry {} ",
                production_name
            );
        }
    }

    writer
        .wrtln(&format!("pub static BYTECODE: [u32; {}] = [", bytecode.len()))?
        .indent();

    for chunk in bytecode.chunks(9) {
        writer.insert_newline()?;
        for val in chunk {
            writer.wrt(&val.to_string())?.wrt(", ")?;
        }
    }

    writer.dedent().write_line("];")?;

    writer.into_output();

    Ok(())
}

#[test]
fn test_output_rust_on_practical_grammar()
{
    use hctk::debug::compile_test_grammar;

    let grammar = compile_test_grammar(
        "
        <> A > \\vec num num^tom num f:ast { { t_Vec, x:f32($tom), y:f32($3), z:f32($4), first: { t_Num, val:u32($1) } } }
        
        <> num > \\temp g:num 
        ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut ascript);

    for error in &errors {
        println!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.struct_table.len(), 2);

    let mut writer = StringBuffer::default();

    rust::write(&grammar, &ascript, &mut writer);

    println!("{}", String::from_utf8(writer.into_output()).unwrap());
}

#[test]
fn test_output_rust_on_trivial_grammar()
{
    use hctk::debug::compile_test_grammar;

    let grammar = compile_test_grammar(
        "
        <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
        | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
        ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut ascript);

    for error in &errors {
        println!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.struct_table.len(), 1);

    let mut writer = StringBuffer::default();

    rust::write(&grammar, &ascript, &mut writer);

    println!("{}", String::from_utf8(writer.into_output()).unwrap());
}
