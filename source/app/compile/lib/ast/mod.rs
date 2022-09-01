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
pub fn build_ast(source_type: SourceType) -> PipelineTask {
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

pub fn compile_ast_data(out_path: &PathBuf, input_name: &str, g: &GrammarStore) {
  if let Ok(ast_data_file) = std::fs::File::create(out_path.join("./ast.rs")) {
    let mut writer = CodeWriter::new(BufWriter::new(ast_data_file));

    writer.write(&DISCLAIMER(input_name, "AST Data", "//!"));

    if let Err(err) = write_ascript_data(g, writer, rust::write) {
      eprintln!("Problem writing ast.rs:\n{}", err);
    }
  } else {
    println!("cargo:warning=Could not write ast.rs");
  }
}

type AScriptSyntaxWriter<W> = fn(&GrammarStore, &AScriptStore, &mut CodeWriter<W>) -> Result<()>;

fn write_ascript_data<W: Write>(
  g: &GrammarStore,
  mut w: CodeWriter<W>,
  syntax_w: AScriptSyntaxWriter<W>,
) -> std::io::Result<()> {
  let mut ascript = AScriptStore::new();

  let errors = compile_reduce_function_expressions(g, &mut ascript);

  if !errors.is_empty() {
    for error in &errors {
      eprintln!("{}", error);
    }
  } else {
    syntax_w(g, &ascript, &mut w)?;
    w.into_output();
  }

  Ok(())
}

#[cfg(test)]
mod test {
  use hctk::ascript::compile::compile_reduce_function_expressions;
  use hctk::debug::compile_test_grammar;
  use hctk::types::AScriptStore;

  use crate::writer::code_writer::CodeWriter;
  use crate::writer::code_writer::StringBuffer;

  use super::compile_ast_data;
  use super::rust;

  #[test]
  fn test_grammar() {
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
      eprintln!("{}", error);
    }

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types2() {
    let grammar = compile_test_grammar(

"@NAME wick_element

@IGNORE g:sp g:nl

<> element_block > \\< component_identifier
    ( element_attribute(+)  f:r { { t_Attributes, c_Attribute, attributes: $1 } } )? 
    ( element_attributes | general_data | element_block | general_binding )(*) 
    \\>

                                                                f:ast { { t_Element, id:$2, children: [$3, $4], tok } }
<> component_identifier > 
    identifier ( \\: identifier )?
                                                                f:ast { { t_Ident, name:str($1), sub_name:str($2), tok } }

<> element_attributes >g:nl element_attribute(+)               
                                                                f:ast { { t_Attributes, c_Attribute, attributes: $2 } }

<> element_attribute > \\- identifier attribute_chars ( ?=g:sp | ?=\\> | ?=g:nl )

                                                                f:ast { { t_GeneralAttr, c_Attribute, key:str($2), val: str($3) } }

    | \\- identifier \\: identifier 
                                                                f:ast { { t_BindingAttr, c_Attribute, key:str($2), val: str($4) } }

    | \\- t:store \\{ local_values? \\} 
                                                                f:ast { { t_StoreAttr, c_Attribute, children: $4 } }
    | \\- t:local \\{ local_values? \\} 
                                                                f:ast { { t_LocalAttr, c_Attribute, children: $4 } }
    | \\- t:param \\{ local_values? \\} 
                                                                f:ast { { t_ParamAttr, c_Attribute, children: $4 } }
    | \\- t:model \\{ local_values? \\} 
                                                                f:ast { { t_ModelAttr, c_Attribute, children: $4 } }

<> general_binding > \\: identifier               
                                                                f:ast { { t_OutputBinding, val:str($2) } }

<> local_values > local_value

<> local_value > identifier ( \\` identifier )? ( \\=  g:num )? ( \\, )(*)

                                                                f:ast { { t_Var, c_Attribute, name:str($1), meta:str($2), value:$3 } }

<> attribute_chars > ( g:id | g:num | g:sym  )(+)
                                                                f:ast { { t_AttributeData, tok } }
<> general_data > ( g:id | g:num  | g:nl  )(+)
                                                                f:ast { { t_GeneralData, tok } }

<> identifier > tk:tok_identifier 

<> tok_identifier > ( g:id) ( g:id | g:num )(+)",
    );

    let mut store = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut store);

    for error in &errors {
      eprintln!("{}", error);
    }

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &store, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types3() {
    let grammar = compile_test_grammar(
      " 
      <> B > num f:ast{ { t_Tsest, b:$1 } }      
      <> num > \\temp f:ast{ { t_Test, tok } }              
      ",
    );

    let mut store = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut store);

    for error in &errors {
      eprintln!("{}", error);
    }

    eprintln!("{:#?}", store);

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &store, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
