use std::io::Write;

use crate::{
  ascript::{
    rust::{ascript_type_to_string, create_type_initializer_value, render_expression, write},
    types::*,
  },
  grammar::data::ast::{ASTNode, AST_NamedReference},
  pipeline::SourceType,
  types::*,
  writer::code_writer::CodeWriter,
  *,
};

use super::pipeline::PipelineTask;

/// Build artifacts for a Bytecode based parser
pub fn build_ascript_types_and_functions(source_type: SourceType) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| match source_type {
      SourceType::Rust => {
        let mut writer = CodeWriter::new(vec![]);

        if let Some(ascript) = task_ctx.get_ascript() {
          match add_ascript_functions(
            ascript,
            &task_ctx.get_journal().grammar().unwrap(),
            &mut writer,
          ) {
            Err(err) => Err(vec![SherpaError::from(err)]),
            _ => match write(ascript, &mut writer) {
              Err(err) => Err(vec![SherpaError::from(err)]),
              _ => Ok(Some(unsafe { String::from_utf8_unchecked(writer.into_output()) })),
            },
          }
        } else {
          Err(vec![SherpaError::from("Could not acquire an Ascript source")])
        }
      }
      _ => Err(vec![SherpaError::from(format!(
        "Unable to build an AST output for the source type {:?}",
        source_type
      ))]),
    }),
    require_ascript: true,
    require_bytecode: false,
  }
}

pub fn add_ascript_functions<W: Write>(
  ascript: &AScriptStore,
  g: &GrammarStore,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  let export_node_data = get_ascript_export_data(g, ascript);

  // Create impl for all exported productions that can be mapped to a ascript single
  // AScripT type. For those that map to multiple outputs, create an impl on the main
  // AST enum for named parsers on those types.
  for (Ref, ast_type, ast_type_string, export_name) in &export_node_data {
    match ast_type {
      AScriptTypeVal::Struct(id) => {
        let AScriptStruct { type_name, .. } = ascript.structs.get(id).unwrap();
        // Bulding a parser impl for this type.
        writer.write_fmt(format_args!("impl {} {{", type_name))?;
        writer
          .indent()
          .wrt(&format!(
            "
/// Create a [{2}] node from a `String` input.
pub fn from_string(input: String) -> Result<{1}, SherpaParseError> {{
  let reader = UTF8StringReader::from(&input);
  ast_compile::{0}_from(reader)
}}

/// Create a [{2}] node from a `&str` input.
pub fn from_str(input: &str) -> Result<{1}, SherpaParseError> {{
  let reader = UTF8StringReader::from(input);
  ast_compile::{0}_from(reader)
}}",
            export_name, ast_type_string, type_name
          ))?
          .dedent()
          .write_line("}\n")?;
      }
      _ => {
        // Building a parse function on the AST enum for this function.
        writer.write("impl ASTNode {")?;
        writer
          .indent()
          .wrt(&format!(
            "
/// Create a [{2}] from a `String` input.
pub fn parse_string_as_{0}(input: String) -> Result<{1}, SherpaParseError> {{
  let reader = UTF8StringReader::from(&input);
  ast_compile::{0}_from(reader)
}}

/// Create a [{2}] from a `&str` input.
pub fn parse_str_as_{0}(input: &str) -> Result<{1}, SherpaParseError> {{
  let reader = UTF8StringReader::from(input);
  ast_compile::{0}_from(reader)
}}",
            export_name,
            ast_type_string,
            ast_type.debug_string(Some(g))
          ))?
          .dedent()
          .write_line("}\n")?;
      }
    }
  }

  // Write out the base Conversion Trait for the AST parsers. --------------
  // -----------------------------------------------------------------------

  writer.wrtln("pub trait ASTParse<T>  {")?.indent();

  for (_, _, ast_type_string, export_name) in &export_node_data {
    writer.newline()?.wrtln(&format!(
      "fn {}_from(input:T) -> Result<{}, SherpaParseError>;",
      export_name, ast_type_string
    ))?;
  }

  writer.dedent().wrtln("}")?.insert_newline()?;

  // Write the ASTParse implementations for common types of inputs ---------
  // -----------------------------------------------------------------------

  //   for (_, input_type, reader_type, param_name, arg_expression) in [
  // ("<'a>", "String", "UTF8StringReader", "input", "input.as_bytes()"),
  // ("<'a>", "&str", "UTF8StringReader", "input", "input.as_bytes()"),
  // ] {
  // writer.wrtln(&format!("impl ASTParse<{}> for AST{{", input_type))?.indent();
  // for (_, _, ast_type_string, export_name) in &export_node_data {
  // writer
  // .newline()?
  // .wrtln(&format!(
  // "fn {}_from({}: {}) -> Result<{}, SherpaParseError> {{",
  // export_name, param_name, input_type, ast_type_string
  // ))?
  // .indent()
  // .wrtln(&format!("let data = {};", arg_expression))?
  // .wrtln(&format!("let reader = {}::new(data);", reader_type))?
  // .wrtln(&format!("AST::base_{}_from(reader)", export_name))?
  // .dedent()
  // .write_line("}")?;
  // }
  // writer.dedent().wrtln("}")?;
  // }
  Result::Ok(())
}

pub fn get_ascript_export_data(
  g: &GrammarStore,
  ascript: &AScriptStore,
) -> Vec<(Option<ascript::rust::Ref>, AScriptTypeVal, String, String)> {
  let export_node_data = g
    .get_exported_productions()
    .iter()
    .map(|ExportedProduction { export_name, production, .. }| {
      let mut ref_index = 0;
      let ref_ = render_expression(
        &ascript,
        &ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
          tok:   Token::default(),
          value: "first".to_string(),
        })),
        &Rule {
          syms: vec![RuleSymbol {
            scanner_index: 1,
            scanner_length: 1,
            sym_id: SymbolID::Production(production.id, GrammarId(0)),
            grammar_ref: g.id.clone(),
            ..Default::default()
          }],
          len: 1,
          prod_id: production.id,
          id: RuleId(0),
          bytecode_id: 0,
          reduce_fn_ids: vec![],
          grammar_ref: g.id.clone(),
          tok: Token::default(),
        },
        &mut ref_index,
        0,
      );
      let ast_type = ref_.as_ref().unwrap().get_type();
      let ast_type_string = ascript_type_to_string(&ast_type, ascript);
      (ref_, ast_type, ast_type_string, export_name.to_string())
    })
    .collect::<Vec<_>>();
  export_node_data
}
