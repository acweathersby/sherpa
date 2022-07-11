use std::path::PathBuf;

use hctk::bytecode::compile_bytecode;

use hctk::bytecode::BytecodeOutput;
use hctk::get_num_of_available_threads;
use hctk::grammar::compile_from_path;

use hctk::types::*;
use std::collections::BTreeMap;
use std::io::BufWriter;

use std::io::Write;
use std::thread;

use crate::ast::compile_ast_data;

use crate::builder::common;
use crate::builder::disclaimer::DISCLAIMER;
use crate::writer::code_writer::CodeWriter;

/// Compile grammar into a bytecode based parser for the given language
pub fn compile_bytecode_files(
    input_path: &PathBuf,
    output_path: &PathBuf,
    build_ast: bool,
)
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
        let (grammar_name, parser_name) = common::get_parser_names(&grammar);
        thread::scope(|scope| {
            if build_ast {
                scope.spawn(|| {
                    compile_ast_data(output_path, input_file_name, &grammar);
                });
            }

            scope.spawn(|| {
                if let Ok(parser_data_file) = std::fs::File::create(
                    output_path.join(format!("./{}.rs", parser_name)),
                ) {
                    let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

                    writer.write(&DISCLAIMER(&grammar_name, "Parser Data", "//!"));

                    write_parser_file(
                        writer,
                        &grammar,
                        // Leave two threads available for building
                        // the
                        // ascript code if necessary
                        1.max(threads.checked_sub(build_ast as usize * 2).unwrap_or(1)),
                    );
                }
            });
        })
    }
}
fn write_parser_file<W: Write>(
    mut writer: CodeWriter<W>,
    grammar: &GrammarStore,
    threads: usize,
) -> std::io::Result<()>
{
    let BytecodeOutput {
        bytecode,
        state_name_to_offset: state_lookups,
        ..
    } = compile_bytecode(grammar, threads);

    if let Err(err) = write_rust_parser_file(writer, &state_lookups, grammar, &bytecode) {
        println!("{}", err);
    }

    Ok(())
}

fn write_rust_parser_file<W: Write>(
    mut writer: CodeWriter<W>,
    state_lookups: &BTreeMap<String, u32>,
    grammar: &GrammarStore,
    bytecode: &Vec<u32>,
) -> std::io::Result<()>
{
    writer
        .wrt(
            "use hctk::runtime::*;
use hctk::types::*;

pub struct Context<'a, T: CharacterReader>(ParseContext<T>, &'a mut T, bool);

impl<'a, T: CharacterReader> Iterator for Context<'a, T>
{
    type Item = ParseAction;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item>
    {
        let Context(ctx, reader, active) = self;

        if *active {
            let action = get_next_action::<T>(reader, ctx, &bytecode);
            match action {
                ParseAction::Error { .. } | ParseAction::Accept { .. } => {
                    *active = false;
                    Some(action)
                }
                action => Some(action),
            }
        } else {
            None
        }
    }
}

impl<'a, T: CharacterReader> Context<'a, T>
{
    #[inline(always)]
    fn new(reader: &'a mut T) -> Self
    {
        Self(ParseContext::<T>::bytecode_context(), reader, true)
    }
    ",
        )?
        .indent();

    common::write_rust_entry_function_bytecode(grammar, state_lookups, &mut writer)?;

    writer.dedent().wrtln("}")?;

    writer
        .wrtln(&format!("static bytecode: [u32; {}] = [", bytecode.len()))?
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

#[cfg(test)]
mod test
{
    use crate::ast::rust;
    use crate::writer::code_writer::StringBuffer;
    use hctk::ascript::compile::compile_reduce_function_expressions;
    use hctk::types::AScriptStore;
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
}
