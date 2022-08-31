use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::io::Result;
use std::vec;

use hctk::ascript::compile::get_index_body_ref;
use hctk::ascript::compile::get_named_body_ref;
use hctk::ascript::compile::get_production_types;
use hctk::ascript::compile::get_specified_vector_from_generic_vec_values;
use hctk::ascript::compile::get_struct_type_from_node;
use hctk::ascript::compile::production_types_are_structs;
use hctk::grammar::data::ast::ASTNode;
use hctk::grammar::data::ast::AST_IndexReference;
use hctk::grammar::data::ast::AST_NamedReference;
use hctk::grammar::data::ast::AST_Struct;
use hctk::grammar::data::ast::AST_Token;
use hctk::grammar::data::ast::AST_Vector;
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
    .wrtln("pub type HCO = HCObj<ASTNode>;")?
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

  let fn_args = "args: &mut Vec<HCO>, tok: Token";

  // Build reduce functions -------------------------------------
  writer.wrtln(&format!("fn noop_fn({}){{}}", fn_args))?.newline()?;

  let mut refs = vec![];

  for (id, body) in &ordered_bodies {
    let mut temp_writer = writer.checkpoint();
    let mut noop = 0;
    let fn_name = format!("ast_fn{:0>3}", body.bytecode_id);

    temp_writer.wrtln(&format!("fn {}({}){{", fn_name, fn_args))?.indent();

    if body.reduce_fn_ids.is_empty() {
      noop = 1;
    } else {
      let mut ref_index = body.symbols.len();

      for function_id in &body.reduce_fn_ids {

        match grammar.reduce_functions.get(function_id) {

          Some(ReduceFunctionType::Ascript(function)) => match &function.ast {

            ASTNode::AST_Struct(box ast_struct) => {
              if let AScriptTypeVal::Struct(struct_type) =
                get_struct_type_from_node(ast_struct)
              {
                let _ref = build_struct_constructor(
                  grammar,
                  ascript,
                  body,
                  &struct_type,
                  ast_struct,
                  &mut ref_index,
                )?;

                let indices = _ref.get_indices();

                for (i, _) in body.symbols.iter().enumerate().rev() {
                  if indices.contains(&i) {
                    temp_writer.wrtln(&format!("let i{} = args.pop().unwrap();", i))?;
                  } else {
                    temp_writer.wrtln("args.pop();")?;
                  }
                }

                temp_writer.write_line(&_ref.to_init_string())?;

                temp_writer.write_line(&format!(
                  "args.push(HCO::NODE(ASTNode::{}(Box::new({}))))",
                  ascript.struct_table.get(&struct_type).unwrap().type_name,
                  _ref.get_ref_string()
                ))?;
                break;
              }
            }
            ASTNode::AST_Statements(box statements) => {
              let mut reference = String::new();
              let mut return_type = AScriptTypeVal::Undefined;
              let mut refs = BTreeSet::new();
              let mut statement_writer = temp_writer.checkpoint();

              for statement in &statements.statements {
                match render_expression(statement, body, ascript, grammar, &mut ref_index)
                {
                  Some(_ref) => {
                    refs.append(&mut _ref.get_indices());
                    return_type = _ref.ast_type.clone();
                    reference = _ref.get_ref_string();
                    statement_writer.write_line(&_ref.to_init_string())?;
                  }
                  None => {}
                }
              }

              for i in 0..body.symbols.len() {
                if refs.contains(&i) {
                  temp_writer.wrtln(&format!("let i{} = args.pop().unwrap();", i))?;
                } else {
                  temp_writer.wrtln("args.pop();")?;
                }
              }

              temp_writer.merge_checkpoint(statement_writer)?;

              match return_type {
                AScriptTypeVal::GenericStructVec(..)
                | AScriptTypeVal::TokenVec
                | AScriptTypeVal::StringVec
                | AScriptTypeVal::Bool(..)
                | AScriptTypeVal::F32(..)
                | AScriptTypeVal::F64(..)
                | AScriptTypeVal::U64(..)
                | AScriptTypeVal::U32(..)
                | AScriptTypeVal::U16(..)
                | AScriptTypeVal::U8(..)
                | AScriptTypeVal::I64(..)
                | AScriptTypeVal::I32(..)
                | AScriptTypeVal::I16(..)
                | AScriptTypeVal::I8(..)
                | AScriptTypeVal::U8Vec
                | AScriptTypeVal::U16Vec
                | AScriptTypeVal::U32Vec
                | AScriptTypeVal::U64Vec
                | AScriptTypeVal::I8Vec
                | AScriptTypeVal::I16Vec
                | AScriptTypeVal::I32Vec
                | AScriptTypeVal::I64Vec
                | AScriptTypeVal::F32Vec
                | AScriptTypeVal::F64Vec
                | AScriptTypeVal::String(..)
                | AScriptTypeVal::Token => temp_writer.write_line(&format!(
                  "args.push(HCO::{}({}))",
                  return_type.hcobj_type_name(None),
                  &reference
                ))?,
                _ => temp_writer.write_line(&reference)?,
              };
            }
            _ => {
              noop = 2;
            }
          },
          _ => {
            noop = 3;
          }
        }
      }
    }

    temp_writer.dedent().wrtln("}")?;
    temp_writer.newline()?;

    if noop == 0 {
      writer.merge_checkpoint(temp_writer)?;
      refs.push(format!("/* {} */ {}",id, fn_name));
    } else {
      refs.push(format!("/* {} {} */ noop_fn",id, noop));
    }
  }

  // Reduce Function Array -----------------------

  writer
    .wrt(&format!(
      "pub const REDUCE_FUNCTIONS:[ReduceFunction<ASTNode>; {}] = [",
      ordered_bodies.len()
    ))?
    .indent()
    .wrt("\n")?;

  writer.write(&refs.join(",\n"))?;

  writer.dedent().wrtln("];")?.newline()?;

  Ok(())
}

fn build_struct_constructor(
  grammar: &GrammarStore,
  store: &AScriptStore,
  body: &Body,
  struct_type: &AScriptStructId,
  ast_struct: &AST_Struct,
  ref_index: &mut usize,
) -> Result<Ref>
{
  let archetype_struct = store.struct_table.get(struct_type).unwrap();
  let mut writer = StringBuffer::default();
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

  writer.wrt(&format!("{0}::new(", archetype_struct.type_name))?.indent().wrt("\n")?;

  let mut predecessors = vec![];

  for (_, val_ref) in archetype_struct.properties.iter().map(|prop_id| {
    let struct_prop_ref = if let Some(ast_prop) = ast_struct_props.get(&prop_id.name) {

      let property = store.props_table.get(prop_id).unwrap();
      
      match render_expression(&ast_prop.value, body, store, grammar, ref_index) {
        Some(mut ref_) => {

          let mut string = match &property.type_val {
            AScriptTypeVal::GenericStructVec(structs_ids) if structs_ids.len() == 1 => {
              format!(
                "{}.into_iter().map(|v|match v {{ ASTNode::{}(node) => node, _ => panic!(\"could not convert\")}}).collect::<Vec<_>>()",
                ref_.get_ref_string(),
                store.struct_table.get(structs_ids.first().unwrap()).unwrap().type_name
              )
            }
            _ => ref_.get_ref_string(),
          };

          let ref_ = match property.type_val {
          AScriptTypeVal::Struct(..)  => {
            node_to_struct(ref_, store)
          }
            _ => ref_
          };

          predecessors.push(ref_);

          if property.optional && matches!(property.type_val, AScriptTypeVal::Struct(..)) {
            string = format!("Some({})", string);
          }

          string
        }
        None => {
          if property.optional && matches!(property.type_val, AScriptTypeVal::Struct(..)) {
            "None".to_string()
          } else {
            use AScriptTypeVal::*;
            match property.type_val {
              Bool(_) => "false".to_string(),
              U16(_) => "0u16".to_string(),
              Token => "Token::new()".to_string(),
              TokenVec  | StringVec
              | U8Vec
              | U16Vec
              | U32Vec
              | U64Vec
              | I8Vec
              | I16Vec
              | I32Vec
              | I64Vec
              | F32Vec
              | F64Vec
              | GenericStructVec(..) => "Vec::new()".to_string(),
              _ => panic!(
                "Unresolvable undefined prop initializer on prop {} with type {}",
                prop_id.name,
                property.type_val.hcobj_type_name(Some(grammar))
              ),
            }
          }
        }
      }
    } else {
      get_default_value(prop_id, store)
    };

    (prop_id.name.clone(), struct_prop_ref)
  }) {
    writer.wrt(&format!("{}, ", val_ref))?;
  }

  if archetype_struct.include_token {
    writer.wrt(&format!("{}, ", "tok"))?;
  }

  writer.dedent().write_line("\n)")?;

  (*ref_index) += 1;

  let mut ref_ = Ref::new(
    *ref_index,
    String::from_utf8(writer.into_output()).unwrap(),
    AScriptTypeVal::Struct(*struct_type),
  );

  ref_.add_predecessors(predecessors);

  Ok(ref_)
}

fn build_structs<W: Write>(
  _grammar: &GrammarStore,
  ascript: &AScriptStore,
  output: &mut CodeWriter<W>,
) -> Result<()>
{
  // Build structs
  for (_, AScriptStruct { include_token, properties, type_name, .. }) in
    &ascript.struct_table
  {
    let mut properties = properties
      .iter()
      .map(|p| {
        let AScriptProp { type_val, optional, .. } = ascript.props_table.get(p).unwrap();
        eprintln!(
          "prop:{}---{:?} {}",
          p.name,
          type_val,
          ascript_type_to_string(type_val, ascript)
        );
        (
          p.name.clone(),
          ascript_type_to_string(type_val, ascript),
          type_val.clone(),
          *optional,
        )
      })
      .collect::<Vec<_>>();

    if *include_token {
      properties.push(("tok".to_string(), "Token".to_string(), AScriptTypeVal::Token, false));
    }

    output.wrtln(&format!("#[derive(Debug)]\npub struct {} {{", type_name))?.indent();

    for (name, type_string, type_val, optional) in &properties {
      match optional {
        true => match type_val {
          AScriptTypeVal::Struct(..) => {
            output.write_line(&format!("pub {}: Optional<Box<{}>>,", name, type_string))?
          }
          _ => output.write_line(&format!("pub {}: {},", name, type_string))?,
        },
        false => match type_val {
          AScriptTypeVal::Struct(..) => {
            output.write_line(&format!("pub {}:Box<{}>,", name, type_string))?
          }
          _ => output.write_line(&format!("pub {}: {},", name, type_string))?,
        }
      };
    }

    output
      .dedent()
      .wrtln("}")?
      .wrtln(&format!("impl {} {{", type_name))?
      .indent()
      .wrtln("#[inline]\npub fn new (")?
      .indent();

    for (name, type_string, type_val, optional) in &properties {
      match optional {
        true => match type_val {
          AScriptTypeVal::Struct(..) => {
            output.write_line(&format!("{}:Optional<Box<{}>>,", name, type_string))?
          }
          _ => output.write_line(&format!("{}:{},", name, type_string))?,
        },
        false => match type_val {
          AScriptTypeVal::Struct(..) => {
            output.write_line(&format!("{}:Box<{}>,", name, type_string))?
          }
          _ => output.write_line(&format!("{}:{},", name, type_string))?,
        }
      };
    }

    output.dedent().wrtln(") -> Self {")?.indent().wrtln("Self{")?.indent();

    for (name, ..) in &properties {
      output.write_line(&format!("{},", name))?;
    }

    output.dedent().wrtln("}")?.dedent().wrtln("}")?.dedent().wrtln("}")?;
  }

  Ok(())
}

/// returns: (expression_ref: String, expression_data: String)
fn render_expression(
  ast_expression: &ASTNode,
  body: &Body,
  store: &AScriptStore,
  grammar: &GrammarStore,
  ref_index: &mut usize,
) -> Option<Ref>
{
  let (b, s, g) = (body, store, grammar);

  match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(ast_struct) {
        if let Ok(_ref) = build_struct_constructor(
          grammar,
          store,
          body,
          &struct_type,
          ast_struct,
          ref_index,
        ) {
          Some(_ref)
        } else {
          None
        }
      } else {
        None
      }
    }
    ASTNode::AST_Token(box AST_Token {}) => Some(Ref::token(bump_ref_index(ref_index))),
    ASTNode::AST_Add(..) => Some(Ref::token(bump_ref_index(ref_index))),
    ASTNode::AST_Vector(box AST_Vector { initializer, .. }) => {
      let mut results = initializer
        .iter()
        .filter_map(|n| render_expression(n, body, store, grammar, ref_index))
        .collect::<VecDeque<_>>();

      if results.is_empty() {
        Some(Ref::new(
          bump_ref_index(ref_index),
          "vec![];".to_string(),
          AScriptTypeVal::GenericVec(None),
        ))
      } else {
        let types = results.iter().map(|t| t.ast_type.clone()).collect::<BTreeSet<_>>();

        let mut vector_ref = if results[0].ast_type.is_vec() {
          results.pop_front().unwrap()
        } else {
          Ref::new(
            bump_ref_index(ref_index),
            "vec![]".to_string(),
            get_specified_vector_from_generic_vec_values(&types),
          )
        };

        for mut _ref in results {
          
          if _ref.ast_type.is_vec() {
            _ref.make_mutable();
            let val_ref = _ref.get_ref_string();
            vector_ref.add_post_init_expression(format!("%%.append(&mut {})", val_ref)).make_mutable()
          } else {
            let val_ref = _ref.get_ref_string();
            vector_ref.add_post_init_expression(format!("%%.push({})", val_ref)).make_mutable()
          };

          vector_ref.add_predecessor(_ref);
        }

        Some(vector_ref)
      }
    }
    ASTNode::AST_STRING(box AST_STRING { value, .. }) => match value {
      ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Some(Ref::new(
        bump_ref_index(ref_index),
        format!("\"{}\".to_string()", value),
        AScriptTypeVal::String(Some(value.to_string())),
      )),
      _ => Some(
        render_expression(value, b, s, g, ref_index)?
          .from("%%.to_string()".to_string(), AScriptTypeVal::String(None)),
      ),
    },
    ASTNode::AST_BOOL(box AST_BOOL { value, .. }) => Some(Ref::new(
      bump_ref_index(ref_index),
      format!("{}", value),
      AScriptTypeVal::Bool(Some(*value)),
    )),
    ASTNode::AST_U64(box AST_U64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU64>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_U32(box AST_U32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU32>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_U16(box AST_U16 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU16>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_U8(box AST_U8 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU8>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_I64(box AST_I64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI64>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_I32(box AST_I32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI32>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_I16(box AST_I16 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI16>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_I8(box AST_I8 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI8>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_F32(box AST_F32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValF32>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_F64(box AST_F64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValF64>(initializer, b, s, g, ref_index)
    }
    ASTNode::AST_NUMBER(..) => None,
    ASTNode::AST_Member(..) => None,
    ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) => {
      match get_named_body_ref(body, value) {
        Some((index, sym_id)) => render_body_symbol(sym_id, store, index),
        None => None,
      }
    }
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      match get_index_body_ref(body, value) {
        Some((index, sym_id)) => render_body_symbol(sym_id, store, index),
        None => None,
      }
    }
    _ => None,
  }
}

fn node_to_struct(ref_: Ref, store: &AScriptStore) -> Ref
{
  use AScriptTypeVal::*;
  match ref_.ast_type.clone() {
    Struct(struct_type) => {
      let struct_name = store.struct_table.get(&struct_type).unwrap().type_name.clone();
      ref_.from(format!("if let ASTNode::{}(obj) = %%
      {{ obj }}
      else {{panic!(\"invalid node\")}}", struct_name), Struct(struct_type))
    }
    GenericStruct(struct_types) if struct_types.len() == 1 => {
      let struct_type = *struct_types.first().unwrap();
      let struct_name = store.struct_table.get(&struct_type).unwrap().type_name.clone();
      ref_.from(format!("if let ASTNode::{}(obj) = %%
      {{ obj }}
      else {{panic!(\"invalid node\")}}", struct_name), Struct(struct_type))
    }
    _ => ref_,
  }
}

fn render_body_symbol(
  sym: &BodySymbolRef,
  store: &AScriptStore,
  index: usize,
) -> Option<Ref>
{
  use AScriptTypeVal::*;
  let mut ref_ = match &sym.sym_id {
    SymbolID::Production(prod_id, ..) => {
      let types = get_production_types(store, prod_id);
      if types.len() == 1 {
        let _type = types.first().unwrap().clone();

        if let Some(init_string) = match _type {
          F64(..) => Some(format!("i{0}.to_f64()", index)),
          F32(..) => Some(format!("i{0}.to_f32()", index)),
          U64(..) => Some(format!("i{0}.to_u64()", index)),
          I64(..) => Some(format!("i{0}.to_i64()", index)),
          U32(..) => Some(format!("i{0}.to_u32()", index)),
          I32(..) => Some(format!("i{0}.to_i32()", index)),
          U16(..) => Some(format!("i{0}.to_u16()", index)),
          I16(..) => Some(format!("i{0}.to_i16()", index)),
          U8(..) => Some(format!("i{0}.to_u8()", index)),
          I8(..) => Some(format!("i{0}.to_i8()", index)),
          Token => Some(format!("i{0}.to_token()", index)),
          GenericStructVec(..) => Some(format!("i{0}.into_nodes()", index)),
          StringVec => Some(format!("i{0}.into_strings()", index)),
          TokenVec => Some(format!("i{0}.into_tokens()", index)),
          F64Vec => Some(format!("i{0}.into_f64_vec()", index)),
          F32Vec => Some(format!("i{0}.into_f32_vec()", index)),
          U64Vec => Some(format!("i{0}.into_u64_vec()", index)),
          I64Vec => Some(format!("i{0}.into_i64_vec()", index)),
          U32Vec => Some(format!("i{0}.into_u32_vec()", index)),
          I32Vec => Some(format!("i{0}.into_i32_vec()", index)),
          U16Vec => Some(format!("i{0}.into_u16_vec()", index)),
          I16Vec => Some(format!("i{0}.into_i16_vec()", index)),
          U8Vec => Some(format!("i{0}.into_u8_vec()", index)),
          I8Vec => Some(format!("i{0}.into_i8_vec()", index)),
          GenericStruct(..) => Some(format!("i{0}.into_node().unwrap()", index)),
          Struct(..) => Some(format!("i{0}.into_node().unwrap()", index)),
          _ => None,
        } {
          Ref::new(index, init_string, _type)
        } else {
          Ref::new(index, "".to_string(), match _type.to_owned() {
            GenericVec(types) => {
              get_specified_vector_from_generic_vec_values(&types.unwrap())
            }
            _type => _type,
          })
        }
      } else if production_types_are_structs(&types) {
        Ref::new(
          index,
          format!("i{0}.into_node().unwrap()", index),
          GenericStruct(extract_struct_types(&types)),
        )
      } else {
        Ref::new(index, format!("i{0}.to_token()", index), Token)
      }
    }
    _ => Ref::new(index, format!("i{0}.to_token()", index), Token),
  };

  ref_.add_body_index(index);

  Some(ref_)
}

fn extract_struct_types(types: &BTreeSet<AScriptTypeVal>) -> BTreeSet<AScriptStructId>
{
  types
    .iter()
    .filter_map(|t| match t {
      AScriptTypeVal::Struct(id) => Some(*id),
      _ => None,
    })
    .collect::<BTreeSet<_>>()
}

fn convert_numeric<T: AScriptNumericType>(
  initializer: &ASTNode,
  body: &Body,
  script: &AScriptStore,
  grammar: &GrammarStore,
  ref_index: &mut usize,
) -> Option<Ref>
{
  let rust_type = T::prim_type_name();
  let tok_conversion_fn = T::to_fn_name();

  match initializer {
    ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Some(Ref::new(
      bump_ref_index(ref_index),
      format!("{}{}", T::string_from_f64(*value), rust_type,),
      T::from_f64(*value),
    )),
    _ => {
      let ref_ = render_expression(initializer, body, script, grammar, ref_index)?;

      match ref_.ast_type {
        AScriptTypeVal::F64(..)
        | AScriptTypeVal::F32(..)
        | AScriptTypeVal::Bool(..)
        | AScriptTypeVal::I8(..)
        | AScriptTypeVal::I16(..)
        | AScriptTypeVal::I32(..)
        | AScriptTypeVal::I64(..)
        | AScriptTypeVal::U8(..)
        | AScriptTypeVal::U16(..)
        | AScriptTypeVal::U32(..)
        | AScriptTypeVal::U64(..) => {
          Some(ref_.from(format!("%% as {};", rust_type), T::from_f64(0.0)))
        }
        _ => Some(ref_.from(format!("%%.{}();", tok_conversion_fn), T::from_f64(0.0))),
      }
    }
  }
}

fn bump_ref_index(ref_index: &mut usize) -> usize
{
  *ref_index += 1;
  *ref_index
}

fn ascript_type_to_string(ascript_type: &AScriptTypeVal, ascript: &AScriptStore)
  -> String
{
  use AScriptTypeVal::*;
  match ascript_type {
    GenericVec(types) => format!("Vec<{:?}>", types),
    Struct(id) => ascript.struct_table.get(id).unwrap().type_name.clone(),
    String(..) => "String".to_string(),
    Bool(..) => "bool".to_string(),
    F64(..) => "f64".to_string(),
    F32(..) => "f32".to_string(),
    I64(..) => "i64".to_string(),
    I32(..) => "i32".to_string(),
    I16(..) => "i16".to_string(),
    I8(..) => "i8".to_string(),
    U64(..) => "u64".to_string(),
    U32(..) => "u32".to_string(),
    U16(..) => "u16".to_string(),
    U8(..) => "u8".to_string(),
    Undefined => "Undefined".to_string(),
    Token => "Token".to_string(),
    UnresolvedProduction(prod_id) => {
      let production_types = get_production_types(ascript, prod_id);
      if production_types.len() > 1 {
        if production_types_are_structs(&production_types) {
          "ASTNode".to_string()
        } else {
          "HCObj::None".to_string()
        }
      } else {
        ascript_type_to_string(&production_types.first().unwrap(), ascript)
      }
    }
    F64Vec => "Vec<f64>".to_string(),
    F32Vec => "Vec<f32>".to_string(),
    I64Vec => "Vec<i64>".to_string(),
    I32Vec => "Vec<i32>".to_string(),
    I16Vec => "Vec<i16>".to_string(),
    I8Vec => "Vec<i8>".to_string(),
    U64Vec => "Vec<u64>".to_string(),
    U32Vec => "Vec<u32>".to_string(),
    U16Vec => "Vec<u16>".to_string(),
    U8Vec => "Vec<u8>".to_string(),
    GenericStructVec(struct_ids) => {
      if struct_ids.len() > 1 {
        "Vec<ASTNode>".to_string()
      } else {
        format!(
          "Vec<Box<{}>>",
          ascript.struct_table.get(struct_ids.first().unwrap()).unwrap().type_name
        )
      }
    }
    _ => {
      panic!("Could not resolve compiled ascript type")
    }
  }
}

fn get_default_value_(ascript_type: &AScriptTypeVal, ascript: &AScriptStore) -> String
{
  use AScriptTypeVal::*;
  match ascript_type {
    GenericVec(..) => "vec![]".to_string(),
    Struct(id) => {
      if let Some(ascript_struct) = ascript.struct_table.get(&id) {
        format!("{}::default()", ascript_struct.type_name)
      } else {
        "ASTNode::None".to_string()
      }
    }
    String(..) => "String::new()".to_string(),
    Bool(..) => "false".to_string(),
    F64(..) => "0f64".to_string(),
    F32(..) => "0f32".to_string(),
    I64(..) => "0i64".to_string(),
    I32(..) => "0i32".to_string(),
    I16(..) => "0i16".to_string(),
    I8(..) => "0i8".to_string(),
    U64(..) => "0u64".to_string(),
    U32(..) => "0u32".to_string(),
    U16(..) => "0u16".to_string(),
    U8(..) => "0u8".to_string(),
    Undefined => "None".to_string(),
    Token => "Token::new()".to_string(),
    UnresolvedProduction(prod_id) => {
      let production_types = get_production_types(ascript, prod_id);
      if production_types.len() > 1 {
        if production_types_are_structs(&production_types) {
          "ASTNode".to_string()
        } else {
          "HCObj::None".to_string()
        }
      } else {
        get_default_value_(&production_types.first().unwrap(), ascript)
      }
    }
    F64Vec | F32Vec | U64Vec | I64Vec | U32Vec | I32Vec | U16Vec | I16Vec | U8Vec
    | I8Vec | GenericStructVec(..) | StringVec | TokenVec => "Vec::new()".to_string(),
    _ => {
      panic!("Could not resolve compiled ascript type")
    }
  }
}

fn get_default_value(prop_id: &AScriptPropId, ascript: &AScriptStore) -> String
{
  if let Some(prop) = ascript.props_table.get(prop_id) {
    get_default_value_(&prop.type_val, ascript)
  } else {
    "None".to_string()
  }
}



#[derive(Clone)]
struct Ref
{
  init_index: usize,
  body_indices: BTreeSet<usize>,
  init_expression: String,
  ast_type: AScriptTypeVal,
  predecessors: Option<Vec<Box<Ref>>>,
  post_init_statements: Option<Vec<String>>,
  is_mutable: bool
}

impl Ref
{
  pub fn new(init_index: usize, init_expression: String, ast_type: AScriptTypeVal)
    -> Self
  {
    Ref {
      init_index,
      body_indices: BTreeSet::from_iter(vec![init_index]),
      init_expression,
      ast_type,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false
    }
  }

  pub fn token(init_index: usize) -> Self
  {
    Ref {
      init_index,
      body_indices: BTreeSet::new(),
      init_expression: "tok".to_string(),
      ast_type: AScriptTypeVal::Token,
      predecessors: None,
      post_init_statements: None,is_mutable: false
    }
  }

  pub fn from(self, init_expression: String, ast_type: AScriptTypeVal) -> Self
  {
    Ref {
      init_index: self.init_index,
      init_expression,
      ast_type,
      body_indices: BTreeSet::new(),
      predecessors: Some(vec![Box::new(self)]),
      post_init_statements: None,is_mutable: false
    }
  }

  pub fn make_mutable(&mut self) -> &mut Self {
    self.is_mutable = true;
    self
  }

  pub fn add_body_index(&mut self, index: usize)
  {
    self.body_indices.insert(index);
  }

  pub fn get_ref_string(&self) -> String
  {
    format!("ref_{}", self.init_index)
  }

  pub fn get_indices(&self) -> BTreeSet<usize>
  {
    let mut indices = self.body_indices.clone();

    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        indices.append(&mut predecessor.get_indices())
      }
    }

    indices
  }

  pub fn add_post_init_expression(&mut self, string: String) -> &mut Self
  {
    self.post_init_statements.get_or_insert(vec![]).push(string);

    self
  }

  pub fn to_init_string(&self) -> String
  {
    let mut string = String::new();

    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        string = string + &predecessor.to_init_string()
      }
    }

    let ref_string = self.get_ref_string();

    string += &format!(
      "\nlet{}{} = {};",
      if self.is_mutable {
        " mut "
      } else {
        " "
      },
      ref_string,
      self.init_expression.replace("%%", &ref_string)
    );

    if let Some(statements) = &self.post_init_statements {
      string = string + "\n" + &statements.join(";\n").replace("%%", &ref_string) + ";";
    }

    string
  }

  pub fn add_predecessor(&mut self, predecessor: Ref) -> &mut Self
  {
    self.predecessors.get_or_insert(vec![]).push(Box::new(predecessor));

    self
  }

  pub fn add_predecessors(&mut self, predecessors: Vec<Ref>) -> &mut Self
  {
    let mut prev = self.predecessors.get_or_insert(vec![]);

    for predecessor in predecessors {
      prev.push(Box::new(predecessor))
    }

    self
  }
}
