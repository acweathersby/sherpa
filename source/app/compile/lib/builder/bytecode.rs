use hctk::ascript::compile::get_resolved_type;
use hctk::bytecode::compile_bytecode;

use hctk::bytecode::BytecodeOutput;

use hctk::debug::grammar;
use hctk::grammar::data::ast::ASTNode;
use hctk::grammar::data::ast::AST_NamedReference;
use hctk::grammar::get_exported_productions;
use hctk::grammar::ExportedProduction;
use hctk::types::*;
use std::any::Any;
use std::collections::BTreeMap;
use std::io::BufWriter;

use std::io::Write;

use crate::CompileError;
use crate::SourceType;

use crate::ast::rust::ascript_type_to_string;
use crate::ast::rust::create_type_initializer_value;
use crate::ast::rust::render_expression;
use crate::builder::common;
use crate::builder::disclaimer::DISCLAIMER;
use crate::writer::code_writer::CodeWriter;

use super::pipeline::PipelineTask;

/// Build artifacts for a LLVM based parser.
pub fn build_byte_code_parse(
  source_type: SourceType,
  include_ascript_mixins: bool,
) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |task_ctx| match source_type {
      SourceType::Rust => {
        let output_path = task_ctx.get_source_output_dir().clone();
        let parser_name = task_ctx.get_parser_name().clone();

        if let Ok(parser_data_file) =
          task_ctx.create_file(output_path.join(format!("./{}_bc.rs", parser_name)))
        {
          let mut writer = CodeWriter::new(BufWriter::new(parser_data_file));

          writer.write(&DISCLAIMER("Parser Data", "//!", task_ctx));

          if include_ascript_mixins {
            // writer.wrtln(&format!("mod super::{}_ast;", parser_name));
            writer.wrtln(&format!("use super::{}_ast::*;", parser_name));
          }

          if let Err(err) = write_parser_file(
            writer,
            &task_ctx.get_grammar(),
            // Leave two threads available for building
            // the
            // ascript code if necessary
            1,
            if include_ascript_mixins { Some(task_ctx.get_ascript()) } else { None },
          ) {
            Err(CompileError::from_io_error(&err))
          } else {
            Ok(())
          }
        } else {
          Err(CompileError::from_string(&format!(
            "Unable to build an AST output for the source type {:?}",
            source_type
          )))
        }
      }
      _ => Err(CompileError::from_string(&format!(
        "Unable to build an AST output for the source type {:?}",
        source_type
      ))),
    }),
    require_ascript: include_ascript_mixins,
    require_bytecode: true,
  }
}

fn write_parser_file<W: Write>(
  mut writer: CodeWriter<W>,
  g: &GrammarStore,
  thread_count: usize,
  ascript: Option<&AScriptStore>,
) -> std::io::Result<()> {
  let BytecodeOutput { bytecode, state_name_to_offset: state_lookups, .. } =
    compile_bytecode(g, thread_count);

  if let Err(err) = write_rust_parser_file(writer, &state_lookups, g, &bytecode, ascript) {
    eprintln!("{}", err);
  }

  Ok(())
}

fn write_rust_parser_file<W: Write>(
  mut writer: CodeWriter<W>,
  state_lookups: &BTreeMap<String, u32>,
  g: &GrammarStore,
  bc: &Vec<u32>,
  ast: Option<&AScriptStore>,
) -> std::io::Result<()> {
  writer
    .wrt(
      "
use hctk::runtime::*;
use hctk::types::*;

pub struct Context<'a, T: ByteCharacterReader + ImmutCharacterReader + MutCharacterReader>(ParseContext<T>, &'a mut T, bool);

impl<'a, T: ByteCharacterReader + ImmutCharacterReader + MutCharacterReader> Iterator for Context<'a, T>
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

impl<'a, T: ByteCharacterReader + ImmutCharacterReader + MutCharacterReader> Context<'a, T>
{
    #[inline(always)]
    fn new(reader: &'a mut T) -> Self
    {
        Self(ParseContext::<T>::bytecode_context(), reader, true)
    }
    ",
    )?
    .indent();

  common::write_rust_entry_function_bytecode(g, state_lookups, &mut writer)?;

  if let Some(ascript) = ast {
    for (_, ExportedProduction { export_name, production, .. }) in
      get_exported_productions(g).iter().enumerate()
    {
      let mut ref_index = 0;
      let ref_ = render_expression(
        &ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
          tok:   Token::default(),
          value: "first".to_string(),
        })),
        &Body {
          syms: vec![BodySymbolRef {
            consumable: false,
            annotation: String::default(),
            exclusive: false,
            original_index: 0,
            scanner_index: 1,
            scanner_length: 1,
            sym_id: SymbolID::Production(production.id, GrammarId(0)),
            tok: Token::new(),
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
      );

      let ast_type = ref_.as_ref().unwrap().get_type();
      writer
        .newline()?
        .wrtln(&format!(
          "pub fn parse_{}(reader: &'a mut T) -> Result<{}, ParseError>{{ ",
          export_name,
          ascript_type_to_string(&ast_type, ascript)
        ))?
        .indent()
        .wrtln(&format!(
          "
        let mut ctx = Self::new_{}_parser(reader);
        ",
          export_name
        ))?
        .wrtln(
          "
let mut nodes = Vec::new();
let mut tokens = Vec::new();
loop {
  match ctx.next() {
    Some(ParseAction::Error { last_input, .. }) => {
      let mut error_token = Token::from_parse_token(&last_input);
      error_token.set_source(ctx.1.get_source());
      return Err(ParseError::COMPILE_PROBLEM(
        CompileProblem {
          message: \"Unable to parse input\".to_string(),
          inline_message: \"Invalid Token\".to_string(),
          loc: error_token
        }
      ));
    }
    Some(ParseAction::Shift { skipped_characters: skip, token }) => {
      let mut tok = Token::from_parse_token(&token);
      tok.set_source(ctx.1.get_source());
      nodes.push(HCO::TOKEN(tok.clone()));
      tokens.push(tok);
    }
    Some(ParseAction::Reduce { body_id, symbol_count, .. }) => {
      let len = symbol_count as usize;
      let pos_a = &tokens[tokens.len() - len as usize];
      let pos_b = &tokens[tokens.len() - 1];
      let tok = Token::from_range(pos_a, pos_b);
      let root = tokens.len() - len;
      tokens[root] = tok.clone();

      unsafe {
        tokens.set_len(root + 1);
      }

      REDUCE_FUNCTIONS[body_id as usize](&mut nodes, tok);
    }
    Some(ParseAction::Accept { production_id }) => {
      break;
    }
    _ => {
      break;
    }
  }
}",
        )?
        .wrtln(&{
          let (string, ref_) = create_type_initializer_value(ref_, &ast_type, false, ascript);

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
  }

  writer.dedent().wrtln("}")?;

  writer.wrtln(&format!("static bytecode: [u32; {}] = [", bc.len()))?.indent();

  for chunk in bc.chunks(9) {
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
mod test {
  use crate::ast::rust;
  use crate::writer::code_writer::StringBuffer;
  use hctk::ascript::compile::compile_reduce_function_expressions;
  use hctk::types::AScriptStore;
  #[test]
  fn test_output_rust_on_practical_grammar() {
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
      eprintln!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.structs.len(), 2);

    let mut writer = StringBuffer::default();

    rust::write(&grammar, &ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
