use std::collections::BTreeMap;
use std::io::Write;

use hctk_core::grammar::data::ast::ASTNode;
use hctk_core::grammar::data::ast::AST_NamedReference;
use hctk_core::types::*;

use hctk_core::types::Token;
use hctk_core::writer::code_writer::CodeWriter;

use crate::ascript::rust::ascript_type_to_string;
use crate::ascript::rust::create_type_initializer_value;
use crate::ascript::rust::render_expression;
use crate::ascript::types::AScriptStore;

pub(crate) fn write_rust_entry_functions_bytecode<W: Write>(
  g: &GrammarStore,
  states: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(
    for ExportedProduction { export_name, guid_name, production } in g.get_exported_productions() {
      if let Some(bytecode_offset) = states.get(guid_name) {
        writer
          .wrt(&format!("pub fn new_{}_parser(reader: T) -> Self{{", export_name))?
          .indent()
          .wrtln("let mut ctx = Self::new(reader);")?
          .wrtln(&format!("ctx.0.init_normal_state(NORMAL_STATE_FLAG | {});", bytecode_offset))?
          .wrtln("ctx")?
          .dedent()
          .wrtln("}")?
          .newline()?;
      } else {
        println!("Unable to get bytecode offset for production {} ", production.name,);
      }
    },
  )
}

pub(crate) fn write_rust_entry_functions<W: Write>(
  g: &GrammarStore,
  _states: &BTreeMap<String, u32>,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(
    for (i, ExportedProduction { export_name, production, .. }) in
      g.get_exported_productions().iter().enumerate()
    {
      writer
        .newline()?
        .wrtln(&format!("/// `{}`", production.location.to_string().replace("\n", "\n// ")))?
        .wrtln(&format!("pub fn new_{}_parser(reader: T) -> Self{{", export_name))?
        .indent()
        .wrtln("let mut ctx = Self::new(reader);")?
        .wrtln(&format!("ctx.set_start_point({});", i))?
        .wrtln("ctx")?
        .dedent()
        .wrtln("}")?
        .newline()?;
    },
  )
}

pub fn add_ascript_functions<W: Write>(
  ast: Option<&AScriptStore>,
  g: &GrammarStore,
  writer: &mut CodeWriter<W>,
) -> Result<(), std::io::Error> {
  Ok(if let Some(ascript) = ast {
    let export_node_data = g
      .get_exported_productions()
      .iter()
      .map(|ExportedProduction { export_name, production, .. }| {
        let mut ref_index = 0;
        let ref_ = render_expression(
          &ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
            tok:   Token::default(),
            value: "first".to_string(),
          })),
          &Body {
            syms: vec![BodySymbolRef {
              scanner_index: 1,
              scanner_length: 1,
              sym_id: SymbolID::Production(production.id, GrammarId(0)),
              grammar_ref: g.id.clone(),
              ..Default::default()
            }],
            len: 1,
            prod: production.id,
            id: BodyId(0),
            bc_id: 0,
            reduce_fn_ids: vec![],
            origin_location: Token::default(),
          },
          &ascript,
          g,
          &mut ref_index,
          0,
        );
        let ast_type = ref_.as_ref().unwrap().get_type();
        let ast_type_string = ascript_type_to_string(&ast_type, ascript);
        (ref_, ast_type, ast_type_string, export_name.to_string())
      })
      .collect::<Vec<_>>();

    // Write out the ast parser implementations for Parser -------------------
    // -----------------------------------------------------------------------

    writer.write_line(
      "
/// Static only object providing convenient methods for parsing 
/// inputs into AST trees.
pub struct AST { }
    ",
    )?;
    writer.wrtln("impl AST  {")?.indent();

    for (ref_, ast_type, ast_type_string, export_name) in &export_node_data {
      writer
        .newline()?
        .wrtln(&format!(
          "fn base_{}_from<R>(mut reader: R)  -> Result<{}, HCError> 
          where R: ByteCharacterReader + BaseCharacterReader + MutCharacterReader{{ ",
          export_name, ast_type_string
        ))?
        .indent()
        .wrtln("let source = reader.get_source();")?
        .wrtln(&format!(
          "
        let mut ctx = Parser::new_{}_parser(reader);
        ",
          export_name
        ))?
        .wrtln(&format!(
          "
let mut nodes = Vec::new();
let mut tokens = Vec::new();
loop {{
  match ctx.next() {{
    Some(ParseAction::Error {{ last_input, last_production }}) => {{
      let mut error_token = Token::from_parse_token(&last_input);
      error_token.set_source(source.clone());
      return Err(HCError::Runtime_InvalidParse{{
          message: \"Unable to parse input\".to_string(),
          inline_message: \"Invalid Token\".to_string(),
          loc: error_token,
          last_production
        }}
      );
    }}
    Some(ParseAction::Shift {{ skipped_characters: skip, token }}) => {{
      let mut tok = Token::from_parse_token(&token);
      tok.set_source(source.clone());
      nodes.push({}::TOKEN(tok.clone()));
      tokens.push(tok);
    }}
    Some(ParseAction::Reduce {{ body_id, symbol_count, .. }}) => {{
      let len = symbol_count as usize;
      let pos_a = &tokens[tokens.len() - len as usize];
      let pos_b = &tokens[tokens.len() - 1];
      let tok = Token::from_range(pos_a, pos_b);
      let root = tokens.len() - len;
      tokens[root] = tok.clone();

      unsafe {{
        tokens.set_len(root + 1);
      }}

      REDUCE_FUNCTIONS[body_id as usize](&mut nodes, tok);

    }}
    Some(ParseAction::Accept {{ production_id }}) => {{
      break;
    }}
    _ => {{
      break;
    }}
  }}
}}
",
          ascript.gen_name()
        ))?
        .wrtln(&{
          let (string, ref_) =
            create_type_initializer_value(ref_.clone(), &ast_type, false, ascript);

          if let Some(exp) = ref_ {
            format!(
              "let i0  = nodes.into_iter().next().unwrap(); {}\nOk({})",
              exp.to_init_string(),
              string
            )
          } else {
            "Ok(nodes.into_iter().next().unwrap())".to_string()
          }
        })?
        .dedent()
        .wrtln("}")?;
    }

    writer.dedent().wrtln("}")?.insert_newline()?;

    // Write out the base Conversion Trait for the AST parsers. --------------
    // -----------------------------------------------------------------------

    writer.wrtln("pub trait ASTParse<T>  {")?.indent();

    for (_, _, ast_type_string, export_name) in &export_node_data {
      writer.newline()?.wrtln(&format!(
        "fn {}_from(input:T) -> Result<{}, HCError>;",
        export_name, ast_type_string
      ))?;
    }

    writer.dedent().wrtln("}")?.insert_newline()?;

    // Write the ASTParse implementations for common types of inputs ---------
    // -----------------------------------------------------------------------

    for (_, input_type, reader_type, param_name, arg_expression) in [
      ("<'a>", "String", "UTF8StringReader", "input", "input.as_bytes()"),
      ("<'a>", "&str", "UTF8StringReader", "input", "input.as_bytes()"),
    ] {
      writer.wrtln(&format!("impl ASTParse<{}> for AST{{", input_type))?.indent();
      for (_, _, ast_type_string, export_name) in &export_node_data {
        writer
          .newline()?
          .wrtln(&format!(
            "fn {}_from({}: {}) -> Result<{}, HCError> {{",
            export_name, param_name, input_type, ast_type_string
          ))?
          .indent()
          .wrtln(&format!("let data = {};", arg_expression))?
          .wrtln(&format!("let reader = {}::new(data);", reader_type))?
          .wrtln(&format!("AST::base_{}_from(reader)", export_name))?
          .dedent()
          .write_line("}")?;
      }
      writer.dedent().wrtln("}")?;
    }
  })
}
