pub mod rust;
use std::io::BufWriter;
use std::io::Result;
use std::io::Write;
use std::path::PathBuf;

use hctk::ascript::compile::compile_reduce_function_expressions;
use hctk::types::AScriptStore;
use hctk::types::GrammarStore;

use crate::builder::disclaimer::DISCLAIMER;
use crate::builder::pipeline::PipelineTask;
use crate::writer::code_writer::CodeWriter;
use crate::CompileError;
use crate::SourceType;

/// Constructs a task that compiles a grammar's Ascript into an AST module of the given `source_type`.
/// The module is placed at `<source_output_dir>/<grammar_name>_parser_ast.rs`.
pub fn build_ast(source_type: SourceType) -> PipelineTask
{
  PipelineTask {
    fun: Box::new(move |ctx| match source_type {
      SourceType::Rust => match ctx.create_file(
        ctx.get_source_output_dir().join(format!("./{}_ast.rs", ctx.get_parser_name())),
      ) {
        Ok(ast_data_file) => {
          let mut writer = CodeWriter::new(BufWriter::new(ast_data_file));
          match rust::write(&ctx.get_grammar(), &ctx.get_ascript(), &mut writer) {
            Ok(_) => {
              drop(writer);
              Ok(())
            }
            Err(err) => Err(CompileError::from_io_error(&err)),
          }
        }
        Err(err) => Err(CompileError::from_io_error(&err)),
      },
      _ => Err(CompileError::from_string(&format!(
        "Unable to build an AST output for the source type {:?}",
        source_type
      ))),
    }),
    require_ascript: true,
    require_bytecode: false,
  }
}

pub fn compile_ast_data(
  output_path: &PathBuf,
  input_file_name: &str,
  grammar: &GrammarStore,
)
{
  if let Ok(ast_data_file) = std::fs::File::create(output_path.join("./ast.rs")) {
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

#[cfg(test)]
mod test
{
  use hctk::ascript::compile::compile_reduce_function_expressions;
  use hctk::debug::compile_test_grammar;
  use hctk::types::AScriptStore;

  use crate::writer::code_writer::CodeWriter;
  use crate::writer::code_writer::StringBuffer;

  use super::compile_ast_data;
  use super::rust;

  #[test]
  fn test_grammar()
  {
    let grammar = compile_test_grammar(
      "
@IGNORE g:sp

@EXPORT statement as entry

@NAME llvm_language_test

<> statement > expression       f:ast { { t_Stmt, v:$1 } }

<> expression > sum             

<> sum > mul \\+ sum             f:ast { { t_Sum, l:$1, r:$3 } }
    | mul

<> mul > term \\* expression     f:ast { { t_Mul, l:$1, r:$3 } }
    | term

<> term > \\2                f:ast { { t_Num, v: u16($1) } }

    | \\( expression \\)          f:ast { { t_Paren, v: $2 } }
",
    );
    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut ascript);
    for error in &errors {
      println!("{}", error);
    }

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &ascript, &mut writer);

    print!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
