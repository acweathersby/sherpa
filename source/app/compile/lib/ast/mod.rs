pub mod rust;
use std::io::BufWriter;
use std::io::Result;
use std::io::Write;
use std::path::PathBuf;

use hctk::ascript::compile::compile_reduce_function_expressions;
use hctk::types::AScriptStore;
use hctk::types::GrammarStore;

use crate::builder::disclaimer::DISCLAIMER;
use crate::writer::code_writer::CodeWriter;

pub fn compile_ast_data(
    output_path: &PathBuf,
    input_file_name: &str,
    grammar: &GrammarStore,
)
{
    if let Ok(ast_data_file) =
        std::fs::File::create(output_path.join("./ast.rs"))
    {
        let mut writer = CodeWriter::new(BufWriter::new(ast_data_file));

        writer.write(&DISCLAIMER(input_file_name, "AST Data", "//!"));

        if let Err(err) = write_ascript_data(grammar, writer, rust::write) {
            eprintln!("Problem writing ast.rs:\n{}", err);
        }
    } else {
        println!("cargo:warning=Could not write ast.rs");
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
