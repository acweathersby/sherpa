use std::collections::BTreeMap;
use std::io::Result;

use hctk::ascript::compile::get_struct_type_from_node;
use hctk::grammar::data::ast::ASTNode;
use hctk::grammar::data::ast::AST_IndexReference;
use hctk::grammar::data::ast::AST_NamedReference;
use hctk::grammar::data::ast::AST_Struct;
use hctk::grammar::data::ast::AST_Token;
use hctk::grammar::data::ast::AST_BOOL;
use hctk::grammar::data::ast::AST_F32;
use hctk::grammar::data::ast::AST_F64;
use hctk::grammar::data::ast::AST_I16;
use hctk::grammar::data::ast::AST_I32;
use hctk::grammar::data::ast::AST_I64;
use hctk::grammar::data::ast::AST_I8;
use hctk::grammar::data::ast::AST_NUMBER;
use hctk::grammar::data::ast::AST_STRING;
use hctk::grammar::data::ast::AST_U16;
use hctk::grammar::data::ast::AST_U32;
use hctk::grammar::data::ast::AST_U64;
use hctk::grammar::data::ast::AST_U8;
use hctk::types::*;
use std::io::Write;

use crate::writer::code_writer::*;

pub fn write<W: Write>(
  grammar: &GrammarStore,
  ascript: &AScriptStore,
  writer: &mut CodeWriter<W>,
) -> Result<()>
{
  writer.wrtln("use hctk::types::*;")?.newline()?;

  writer.wrtln("#[derive(Debug)]\npub enum ASTNode {")?.indent();

  for _struct in ascript.struct_table.values() {
    writer.write_line(&format!("{0}(Box<{0}>),", _struct.type_name))?;
  }

  writer
    .dedent()
    .wrtln("}")?
    .newline()?
    .wrtln("type HCO = HCObj<ASTNode>;")?
    .newline()?
    .wrtln("impl HCObjTrait for ASTNode {}")?
    .newline()?;

  build_structs(grammar, ascript, writer)?;

  build_functions(grammar, ascript, writer)?;

  Ok(())
}

fn build_functions<W: Write>(
  grammar: &GrammarStore,
  ascript: &AScriptStore,
  writer: &mut CodeWriter<W>,
) -> Result<()>
{
  let ordered_bodies = grammar
    .bodies_table
    .iter()
    .filter_map(|(id, b)| {
      if !grammar.production_table.get(&b.production).unwrap().is_scanner {
        Some((b.bytecode_id, b))
      } else {
        None
      }
    })
    .collect::<BTreeMap<_, _>>();

  // Build reduce functions -------------------------------------

  for (id, body) in &ordered_bodies {
    let mut temp_writer = writer.checkpoint();

    if body.reduce_fn_ids.is_empty() {
      create_default_function(&mut temp_writer, body)?;
    } else {
      for function_id in &body.reduce_fn_ids {
        match grammar.reduce_functions.get(function_id) {
          Some(ReduceFunctionType::Ascript(function)) => match &function.ast {
            ASTNode::AST_Struct(box ast_struct) => {
              if let AScriptTypeVal::Struct(struct_type) =
                get_struct_type_from_node(ast_struct)
              {
                temp_writer
                  .wrtln(&format!(
                    "fn ast_fn{:0>3}(args: &mut Vec<HCO>, tok: Token) -> HCO {{",
                    body.bytecode_id,
                  ))?
                  .indent();

                let mut struct_writer = temp_writer.checkpoint();

                let (struct_name, ref_name, refs) = build_struct_constructor(
                  grammar,
                  ascript,
                  body,
                  &struct_type,
                  ast_struct,
                  &mut struct_writer,
                )?;

                for (i, _) in body.symbols.iter().enumerate().rev() {
                  if refs.contains(&(i as u32)) {
                    temp_writer.wrtln(&format!("let i{} = args.pop().unwrap();", i))?;
                  } else {
                    temp_writer.wrtln("args.pop();")?;
                  }
                }

                temp_writer.merge_checkpoint(struct_writer)?;

                temp_writer.write_line(&format!(
                  "HCO::NODE(ASTNode::{}(Box::new({})))",
                  struct_name, ref_name
                ))?;

                temp_writer.dedent().wrtln("}")?;
                break;
              }
            }
            _ => {
              create_default_function(&mut temp_writer, body)?;
            }
          },
          _ => {
            create_default_function(&mut temp_writer, body)?;
          }
        }
      }
    }

    temp_writer.newline()?;

    writer.merge_checkpoint(temp_writer)?;
  }

  // Reduce Function Array -----------------------

  writer
    .wrt(&format!(
      "pub const REDUCE_FUNCTIONS:[ReduceFunction<ASTNode>; {}] = [",
      ordered_bodies.len()
    ))?
    .indent()
    .wrt("\n")?;

  ordered_bodies.iter().for_each(|(id, _)| {
    writer.write(&format!("ast_fn{:0>3},", *id)).unwrap();
  });

  writer.dedent().wrtln("];")?.newline()?;

  Ok(())
}

fn create_default_function(
  temp_writer: &mut CodeWriter<Vec<u8>>,
  body: &&Body,
) -> Result<()>
{
  temp_writer
    .wrtln(&format!(
      "fn ast_fn{:0>3}(args: &mut Vec<HCO>, tok: Token) -> HCO {{",
      body.bytecode_id,
    ))?
    .indent();

  temp_writer.wrtln("let out = args.pop().unwrap();")?;

  for _ in 0..body.length - 1 {
    temp_writer.wrtln("args.pop();")?;
  }

  temp_writer.wrtln("out")?.dedent().wrtln("}")?;

  Ok(())
}

fn build_struct_constructor<W: Write>(
  grammar: &GrammarStore,
  ascript: &AScriptStore,
  body: &Body,
  struct_type: &AScriptStructId,
  ast_struct: &AST_Struct,
  writer: &mut CodeWriter<W>,
) -> Result<(String, String, Vec<u32>)>
{
  let archetype_struct = ascript.struct_table.get(struct_type).unwrap();
  let mut used_props = vec![];
  let mut statements = vec![];
  let ast_struct_props = ast_struct
    .props
    .iter()
    .filter_map(|p| {
      if let ASTNode::AST_Property(prop) = p {
        Some((prop.id.clone(), prop))
      } else {
        None
      }
    })
    .collect::<BTreeMap<_, _>>();

  for prop in &archetype_struct.properties {
    if let Some(ast_prop) = ast_struct_props.get(&prop.name) {
      let (reference, init_string, _, body_index) =
        render_expression(&ast_prop.value, body, ascript, grammar)?;

      if let Some(mut body_index) = body_index {
        used_props.append(&mut body_index);
      }

      statements.push(format!("{}\nlet {} = {};", init_string, prop.name, reference));
    } else {
      statements.push(format!(
        "let {} = {};",
        prop.name,
        get_default_value(prop, ascript)
      ));
    }
  }

  for statement in statements {
    writer.wrtln(&statement)?;
  }
  writer
    .wrt(&format!("\nlet _struct = {0}::new(", archetype_struct.type_name))?
    .indent()
    .wrt("\n")?;
  for prop in &archetype_struct.properties {
    writer.wrt(&format!("{}, ", prop.name))?;
  }

  writer.dedent().write_line("\n);")?;

  Ok((archetype_struct.type_name.clone(), "_struct".to_string(), used_props))
}

fn build_structs<W: Write>(
  grammar: &GrammarStore,
  ascript: &AScriptStore,
  output: &mut CodeWriter<W>,
) -> Result<()>
{
  // Build structs
  for (_, AScriptStruct { include_token, properties, type_name, .. }) in
    &ascript.struct_table
  {
    let mut props = properties
      .iter()
      .map(|p| {
        let AScriptProp { type_val, .. } = ascript.props_table.get(p).unwrap();

        (p.name.clone(), ascript_type_to_string(type_val, ascript, grammar))
      })
      .collect::<Vec<_>>();

    if *include_token {
      props.push(("tok".to_string(), "Token".to_string()));
    }

    output.wrtln(&format!("#[derive(Debug)]\npub struct {} {{", type_name))?.indent();

    for (name, type_) in &props {
      output.write_line(&format!("pub {}: {},", name, type_))?;
    }

    output
      .dedent()
      .wrtln("}")?
      .wrtln(&format!("impl {} {{", type_name))?
      .indent()
      .wrtln("#[inline]\npub fn new (")?
      .indent();

    for (name, type_) in &props {
      output.write_line(&format!("{}:{},", name, type_))?;
    }

    output.dedent().wrtln(") -> Self {")?.indent().wrtln("Self{")?.indent();

    for (name, _) in &props {
      output.write_line(&format!("{},", name))?;
    }

    output.dedent().wrtln("}")?.dedent().wrtln("}")?.dedent().wrtln("}")?;
  }

  Ok(())
}

/// returns: (expression_ref: String, expression_data: String)
pub fn render_expression(
  ast_expression: &ASTNode,
  body: &Body,
  store: &AScriptStore,
  grammar: &GrammarStore,
) -> Result<(String, String, AScriptTypeVal, Option<Vec<u32>>)>
{
  let (b, s, g) = (body, store, grammar);

  match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(ast_struct) {
        let mut writer = StringBuffer::default();

        let (_, ref_name, refs) = build_struct_constructor(
          grammar,
          store,
          body,
          &struct_type,
          ast_struct,
          &mut writer,
        )?;
        Ok((
          ref_name,
          String::from_utf8(writer.into_output()).unwrap(),
          AScriptTypeVal::Struct(struct_type),
          Some(refs),
        ))
      } else {
        Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None))
      }
    }
    ASTNode::AST_Token(box AST_Token {}) => {
      Ok(("tok".to_string(), String::new(), AScriptTypeVal::Token, None))
    }
    ASTNode::AST_Add(..) => {
      Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None))
    }
    ASTNode::AST_Vector(..) => {
      Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None))
    }
    ASTNode::AST_STRING(box AST_STRING { value, .. }) => match value {
      ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Ok((
        "r".to_string(),
        format!("let r:{} = {};", "String", value),
        AScriptTypeVal::String(Some(value.to_string())),
        None,
      )),
      _ => {
        let (r, e, t, i) = render_expression(value, b, s, g)?;
        Ok((
          r.clone() + "_str",
          format!("{1}\nlet {0}_str = {0}.to_string();", r, e),
          AScriptTypeVal::String(None),
          i,
        ))
      }
    },
    ASTNode::AST_BOOL(box AST_BOOL { value, .. }) => Ok((
      "r".to_string(),
      format!("let r:{} = {};", "bool", *value),
      AScriptTypeVal::Bool(Some(*value)),
      None,
    )),
    ASTNode::AST_U64(box AST_U64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU64>(initializer, b, s, g)
    }
    ASTNode::AST_U32(box AST_U32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU32>(initializer, b, s, g)
    }
    ASTNode::AST_U16(box AST_U16 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU16>(initializer, b, s, g)
    }
    ASTNode::AST_U8(box AST_U8 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU8>(initializer, b, s, g)
    }
    ASTNode::AST_I64(box AST_I64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI64>(initializer, b, s, g)
    }
    ASTNode::AST_I32(box AST_I32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI32>(initializer, b, s, g)
    }
    ASTNode::AST_I16(box AST_I16 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI16>(initializer, b, s, g)
    }
    ASTNode::AST_I8(box AST_I8 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI8>(initializer, b, s, g)
    }
    ASTNode::AST_F32(box AST_F32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValF32>(initializer, b, s, g)
    }
    ASTNode::AST_F64(box AST_F64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValF64>(initializer, b, s, g)
    }
    ASTNode::AST_NUMBER(..) => {
      Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None))
    }
    ASTNode::AST_Member(..) => {
      Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None))
    }
    ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) => {
      match body.symbols.iter().enumerate().filter(|(i, s)| s.annotation == *value).last()
      {
        Some((i, sym)) => match &sym.sym_id {
          SymbolID::Production(prod_id, ..) => {
            let types = s.production_types.get(prod_id).unwrap();
            if types.is_empty() {
              Ok((
                format!("ref_i{}", i),
                format!("let ref_i{0} = i{0}.to_tok();", i),
                AScriptTypeVal::Token,
                Some(vec![i as u32]),
              ))
            } else {
              Ok((
                String::new(),
                String::new(),
                AScriptTypeVal::Undefined,
                Some(vec![i as u32]),
              ))
            }
          }
          _ => Ok((
            format!("i{}", i),
            String::new(),
            AScriptTypeVal::String(None),
            Some(vec![i as u32]),
          )),
        },
        None => Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None)),
      }
    }
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      match body
        .symbols
        .iter()
        .enumerate()
        .filter(|(i, s)| s.original_index == (*value - 1.0) as u32)
        .last()
      {
        Some((i, sym)) => match &sym.sym_id {
          SymbolID::Production(prod_id, ..) => {
            let types = s.production_types.get(prod_id).unwrap();
            if types.is_empty() {
              Ok((
                format!("ref_i{}", i),
                format!("let ref_i{0} = i{0}.to_tok();", i),
                AScriptTypeVal::Token,
                Some(vec![i as u32]),
              ))
            } else {
              Ok((
                String::new(),
                String::new(),
                AScriptTypeVal::Undefined,
                Some(vec![i as u32]),
              ))
            }
          }
          _ => Ok((
            format!("i{}", i),
            String::new(),
            AScriptTypeVal::String(None),
            Some(vec![i as u32]),
          )),
        },
        None => Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None)),
      }
    }
    _ => Ok((String::new(), String::new(), AScriptTypeVal::Undefined, None)),
  }
}

fn convert_numeric<T: AScriptNumericType>(
  initializer: &ASTNode,
  body: &Body,
  script: &AScriptStore,
  grammar: &GrammarStore,
) -> Result<(String, String, AScriptTypeVal, Option<Vec<u32>>)>
{
  let rust_type = T::prim_type_name();
  let tok_conversion_fn = T::to_fn_name();
  let default_type_value = T::none();

  match initializer {
    ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Ok((
      "r".to_string(),
      format!("let r:{} = {};", rust_type, T::string_from_f64(*value)),
      T::from_f64(*value),
      None,
    )),
    _ => {
      let (r, expression, t, i) = render_expression(initializer, body, script, grammar)?;

      let refname = format!("num_{}", r);
      match t {
        AScriptTypeVal::F64(..) | AScriptTypeVal::F32(..) => Ok((
          refname.clone(),
          format!("{2}\nlet {1} = {0} as {3};", r, refname, expression, rust_type),
          default_type_value,
          i,
        )),
        AScriptTypeVal::Bool(..)
        | AScriptTypeVal::I8(..)
        | AScriptTypeVal::I16(..)
        | AScriptTypeVal::I32(..)
        | AScriptTypeVal::I64(..)
        | AScriptTypeVal::U8(..)
        | AScriptTypeVal::U16(..)
        | AScriptTypeVal::U32(..)
        | AScriptTypeVal::U64(..) => Ok((
          refname.clone(),
          format!("{2}\nlet {1} = {0} as {3};", r, refname, expression, rust_type),
          default_type_value,
          i,
        )),
        _ => Ok((
          refname.clone(),
          format!("{2}\nlet {1} = {0}.{3}();", r, refname, expression, tok_conversion_fn),
          default_type_value,
          i,
        )),
      }
    }
  }
}

fn ascript_type_to_string(
  ascript_type: &AScriptTypeVal,
  ascript: &AScriptStore,
  grammar: &GrammarStore,
) -> String
{
  match ascript_type {
    AScriptTypeVal::Vector(..) => "Vec<Undefined>".to_string(),
    AScriptTypeVal::Struct(id) => ascript.struct_table.get(id).unwrap().type_name.clone(),
    AScriptTypeVal::String(..) => "String".to_string(),
    AScriptTypeVal::Bool(..) => "bool".to_string(),
    AScriptTypeVal::F64(..) => "f64".to_string(),
    AScriptTypeVal::F32(..) => "f32".to_string(),
    AScriptTypeVal::I64(..) => "i64".to_string(),
    AScriptTypeVal::I32(..) => "i32".to_string(),
    AScriptTypeVal::I16(..) => "i16".to_string(),
    AScriptTypeVal::I8(..) => "i8".to_string(),
    AScriptTypeVal::U64(..) => "u64".to_string(),
    AScriptTypeVal::U32(..) => "u32".to_string(),
    AScriptTypeVal::U16(..) => "u16".to_string(),
    AScriptTypeVal::U8(..) => "u8".to_string(),
    AScriptTypeVal::Undefined => "Undefined".to_string(),
    AScriptTypeVal::Token => "Token".to_string(),
    _ => {
      panic!("Could not resolve compiled ascript type")
    }
  }
}

fn get_default_value(prop_id: &AScriptPropId, ascript: &AScriptStore) -> String
{
  if let Some(prop) = ascript.props_table.get(prop_id) {
    match prop.type_val {
      AScriptTypeVal::Vector(..) => "vec![]".to_string(),
      AScriptTypeVal::Struct(id) => {
        if let Some(ascript_struct) = ascript.struct_table.get(&id) {
          format!("{}::default()", ascript_struct.type_name)
        } else {
          "ASTNode::None".to_string()
        }
      }
      AScriptTypeVal::String(..) => "String::new()".to_string(),
      AScriptTypeVal::Bool(..) => "CCCfalse".to_string(),
      AScriptTypeVal::F64(..) => "0f64".to_string(),
      AScriptTypeVal::F32(..) => "0f32".to_string(),
      AScriptTypeVal::I64(..) => "0i64".to_string(),
      AScriptTypeVal::I32(..) => "0i32".to_string(),
      AScriptTypeVal::I16(..) => "0i16".to_string(),
      AScriptTypeVal::I8(..) => "0i8".to_string(),
      AScriptTypeVal::U64(..) => "0u64".to_string(),
      AScriptTypeVal::U32(..) => "0u32".to_string(),
      AScriptTypeVal::U16(..) => "0u16".to_string(),
      AScriptTypeVal::U8(..) => "0u8".to_string(),
      AScriptTypeVal::Undefined => "None".to_string(),
      AScriptTypeVal::Token => "Token::new()".to_string(),
      _ => {
        panic!("Could not resolve compiled ascript type")
      }
    }
  } else {
    "None".to_string()
  }
}
