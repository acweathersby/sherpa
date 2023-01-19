use sherpa_runtime::types::ast::DEFAULT_AST_TYPE_NAMES;

use super::{
  compile::{
    get_indexed_body_ref,
    get_named_body_ref,
    get_production_types,
    get_specified_vector_from_generic_vec_values,
    get_struct_type_from_node,
    production_types_are_structs,
  },
  types::*,
};
use crate::{
  grammar::data::ast::{
    ASTNode,
    AST_IndexReference,
    AST_NamedReference,
    AST_Struct,
    AST_Token,
    AST_Vector,
    AST_BOOL,
    AST_F32,
    AST_F64,
    AST_I16,
    AST_I32,
    AST_I64,
    AST_I8,
    AST_NUMBER,
    AST_STRING,
    AST_U16,
    AST_U32,
    AST_U64,
    AST_U8,
  },
  types::*,
  writer::code_writer::*,
};
use std::{
  collections::{BTreeMap, BTreeSet, VecDeque},
  fmt::format,
  io::{Result, Write},
  vec,
};

pub fn write<W: Write>(ast: &AScriptStore, w: &mut CodeWriter<W>) -> Result<()> {
  w.indent_spaces(2);

  build_astnode_enum(w, ast)?;

  build_types_utils(w, ast)?;

  build_structs(ast, w)?;

  build_functions(ast, w)?;

  Ok(())
}

fn build_astnode_enum<W: Write>(w: &mut CodeWriter<W>, ast: &AScriptStore) -> Result<()> {
  w.wrtln(&format!(
    r##"
macro_rules! into_vec {{
  ($fn_name:ident, $out_type: ty, $type:ident) => {{
    pub fn $fn_name(self) -> Vec<$out_type> {{
      if let {0}::$type(v) = self {{
        v
      }} else {{
        vec![]
      }}
    }}
  }};
}}

macro_rules! to_numeric {{
  ($fn_name:ident,  $Num:ty) => {{
    fn $fn_name(&self) -> $Num {{
      if self.is_numeric() || matches!(self, {0}::STRING(..) | {0}::TOKEN(..)) {{
        match self {{
          {0}::STRING(str) => str.parse::<i64>().unwrap_or(0) as $Num,
          {0}::TOKEN(tok) => tok.to_string().parse::<i64>().unwrap_or(0) as $Num,
          {0}::F64(val) => *val as $Num,
          {0}::F32(val) => *val as $Num,
          {0}::I64(val) => *val as $Num,
          {0}::I32(val) => *val as $Num,
          {0}::I16(val) => *val as $Num,
          {0}::U64(val) => *val as $Num,
          {0}::U32(val) => *val as $Num,
          {0}::U16(val) => *val as $Num,
          {0}::U8(val) => *val as $Num,
          {0}::BOOL(val) => (*val as usize) as $Num,
          _ => 0 as $Num,
        }}
      }} else {{
        0 as $Num
      }}
    }}
  }};
}}"##,
    ast.ast_type_name
  ))?;

  w.wrtln(&format!("#[derive(Debug, Clone)]\n#[repr(C, u32)]\npub enum {} {{", ast.ast_type_name))?
    .indent();
  // Dump all the base types into this enum
  w.write_line(&format!(
    "NONE,
NODES(Vec<{}>),
STRING(String),
STRINGS(Vec<String>),
F64(f64),
F32(f32),
I64(i64),
I32(i32),
I16(i16),
I8(i8),
U64(u64),
U32(u32),
U16(u16),
U8(u8),
BOOL(bool),
F32Vec(Vec<f32>),
F64Vec(Vec<f64>),
I64Vec(Vec<i64>),
I32Vec(Vec<i32>),
I16Vec(Vec<i16>),
I8Vec(Vec<i8>),
U64Vec(Vec<u64>),
U32Vec(Vec<u32>),
U16Vec(Vec<u16>),
U8Vec(Vec<u8>),
TOKEN(Token),
TOKENS(Vec<Token>),",
    ast.ast_type_name
  ))?;

  for _struct in ast.structs.values() {
    w.write_line(&format!("{0}(Box<{0}>),", _struct.type_name))?;
  }

  w.dedent().wrtln("}")?;

  w.wrtln(&format!("impl {} {{", ast.ast_type_name))?.indent();
  for _struct in ast.structs.values() {
    w.wrtln(&format!("pub fn as_{0}(&self) -> Option<&{0}> {{", _struct.type_name))?
      .indent()
      .wrtln("match self {")?
      .indent()
      .wrtln(&format!("Self::{}(val) => Some(val.as_ref()),", _struct.type_name))?
      .wrtln("_ => None")?
      .dedent()
      .wrtln("}")?
      .dedent()
      .wrtln("}")?;
    w.wrtln(&format!("pub fn as_{0}_mut(&mut self) -> Option<&mut {0}> {{", _struct.type_name))?
      .indent()
      .wrtln("match self {")?
      .indent()
      .wrtln(&format!("Self::{}(val) => Some(val.as_mut()),", _struct.type_name))?
      .wrtln("_ => None")?
      .dedent()
      .wrtln("}")?
      .dedent()
      .wrtln("}")?;
  }
  w.wrtln(&format!(
    r##"
into_vec!(into_nodes, {0}, NODES);
into_vec!(into_f64_vec, f64, F64Vec);
into_vec!(into_f32_vec, f32, F32Vec);
into_vec!(into_i64_vec, i64, I64Vec);
into_vec!(into_i32_vec, i32, I32Vec);
into_vec!(into_i16_vec, i16, I16Vec);
into_vec!(into_i8_vec, i8, I8Vec);
into_vec!(into_u64_vec, u64, U64Vec);
into_vec!(into_u32_vec, u32, U32Vec);
into_vec!(into_u16_vec, u16, U16Vec);
into_vec!(into_u8_vec, u8, U8Vec);
into_vec!(into_tokens, Token, TOKENS);
to_numeric!(to_i8, i8);
to_numeric!(to_i16, i16);
to_numeric!(to_i32, i32);
to_numeric!(to_i64, i64);
to_numeric!(to_u8, u8);
to_numeric!(to_u16, u16);
to_numeric!(to_u32, u32);
to_numeric!(to_u64, u64);
to_numeric!(to_f32, f32);
to_numeric!(to_f64, f64);
fn into_strings(self) -> Vec<String> {{
  match self {{
    {0}::STRINGS(strings) => strings,
    _ => Default::default(),
  }}
}}

pub fn to_string(&self) -> String {{
  match self {{
    &{0}::BOOL(val) => {{
      if val {{
        String::from("true")
      }} else {{
        String::from("false")
      }}
    }}
    {0}::STRING(string) => string.to_owned(),
    {0}::TOKEN(val) => val.to_string(),
    _ => self.to_token().to_string(),
  }}
}}

pub fn to_token(&self) -> Token {{
  match self {{
    {1}{0}::TOKEN(val) => val.clone(),
    _ => Token::empty(),
  }}
}}

pub fn to_bool(&self) -> bool {{
  self.to_u8() != 0
}}
pub fn is_numeric(&self) -> bool {{
  use {0}::*;
  matches!(
    self,
    F64(_) | F32(_)| I64(_)| I32(_)| I16(_)| I8(_)| U64(_)| U32(_)| U16(_)| U8(_)
  )
}}
  "##,
    ast.ast_type_name,
    ast
      .structs
      .iter()
      .filter_map(|s| s.1.include_token.then(|| {
        format!("{0}::{1} (node) => node.tok.clone()\n,", ast.ast_type_name, s.1.type_name)
      }))
      .collect::<Vec<_>>()
      .join("")
  ))?
  .dedent()
  .wrtln("}")?
  .wrtln(&format!(
    "
impl Default for {0} {{
  fn default() -> Self {{
    Self::NONE
  }}
}}
  ",
    ast.ast_type_name
  ))?;

  Ok(())
}

fn build_types_utils<W: Write>(w: &mut CodeWriter<W>, ast: &AScriptStore) -> Result<()> {
  w.wrtln(&format!(
    "#[derive(Eq, PartialEq, Clone, Copy, Debug)]\npub enum {}Type {{",
    ast.ast_type_name
  ))?
  .indent()
  .write_line("Undefined,")?;
  for name in DEFAULT_AST_TYPE_NAMES {
    w.wrtln(name)?.wrt(",")?;
  }
  for AScriptStruct { type_name, .. } in ast.structs.values() {
    w.wrtln(type_name)?.wrt(",")?;
  }
  w.dedent().wrtln("}")?;
  w.wrtln(&format!("pub trait Get{0} {{ fn get_type(&self) -> {0}; }}", ast.type_name()))?;
  w.wrtln(&format!("impl Get{0} for {1} {{", ast.type_name(), ast.ast_type_name))?.indent();
  w.wrtln(&format!("fn get_type(&self) -> {} {{", ast.type_name()))?.indent();
  w.wrtln("match self{")?.indent();
  for AScriptStruct { type_name, .. } in ast.structs.values() {
    w.wrtln(&format!("{0}::{2}(..) => {1}::{2}", ast.ast_type_name, ast.type_name(), type_name))?
      .wrt(",")?;
  }
  /*   for name in DEFAULT_AST_TYPE_NAMES {
    w.write_line(&format!("{0}::{2}(..) => {1}::{2},", ast.ast_type_name, ast.type_name(), name))?;
  } */
  w.write_line(&format!("_ => {}::NONE,", ast.type_name()))?;
  w.dedent().wrtln("}")?.dedent().wrtln("}")?.dedent().wrtln("}")?;
  Ok(())
}

fn build_functions<W: Write>(ast: &AScriptStore, w: &mut CodeWriter<W>) -> Result<()> {
  let g = ast.g.clone();
  let ordered_bodies = g
    .rules
    .iter()
    .filter_map(|(_, b)| {
      if g.parse_productions.contains(&b.prod_id) {
        Some((b.bytecode_id, b))
      } else {
        None
      }
    })
    .collect::<BTreeMap<_, _>>();

  let fn_args =
    format!("_ctx_: &ParseContext<R, M>, slots: &AstStackSlice<AstSlot<{}>>", ast.ast_type_name);
  let fn_template = format!("<R: Reader + UTF8Reader, M>");

  // Build reduce functions -------------------------------------
  w.wrtln(&format!(
    "\nfn default_fn{}({}){{
  if slots.len() > 1 {{
    let AstSlot (_, rng, _) = slots.take(0);
    let last = slots.take(slots.len() - 1);
    for index in 1..slots.len()-1 {{
      slots.take(index);
    }}
    slots.assign(0, AstSlot (last.0, rng + last.1, TokenRange::default()));
  }}
}}",
    fn_template, fn_args
  ))?
  .newline()?;

  let mut refs = vec![];

  for (id, rule) in &ordered_bodies {
    let prod_id = rule.prod_id;
    let prod_data = ast.prod_types.get(&prod_id).unwrap();

    if prod_data.len() != 1 {
      unreachable!(
        "\n\nProduction result not been resolved\n[{}] == {}\n\n\n{}\n\n",
        ast.g.get_production_plain_name(&prod_id),
        rule.tok.blame(1, 1, "", BlameColor::RED),
        prod_data
          .iter()
          .map(|(p, _)| { p.debug_string(Some(&ast.g)) })
          .collect::<Vec<_>>()
          .join("\n")
      )
    };

    let mut temp_writer = w.checkpoint();
    let mut noop = 0;
    let fn_name = format!("ast_fn{:0>3}", rule.bytecode_id);

    temp_writer
      .wrtln(&format!(
        "/*\n{}\n*/\nfn {}{}({}){{",
        rule.item().rule_string(&g),
        fn_name,
        fn_template,
        fn_args
      ))?
      .indent();

    if rule.reduce_fn_ids.is_empty() {
      noop = 1;
    } else {
      let mut ref_index = rule.syms.len();

      for function_id in &rule.reduce_fn_ids {
        match g.reduce_functions.get(function_id) {
          Some(ReduceFunctionType::AscriptOld(function)) => match &function.ast {
            ASTNode::AST_Struct(box ast_struct) => {
              if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(ast_struct) {
                let _ref =
                  build_struct_constructor(ast, rule, &struct_type, ast_struct, &mut ref_index, 0)?;

                let indices = _ref.get_indices();
                let token_indices = _ref.get_tokens();

                write_slot_extraction(rule, indices, token_indices, &mut temp_writer)?;

                write_node_token(&mut temp_writer, rule)?;

                temp_writer.write_line(&_ref.to_init_string())?;

                temp_writer.write_line(&format!(
                  "slots.assign(0, AstSlot ({}::{}(Box::new({})), rng, TokenRange::default()))",
                  ast.ast_type_name,
                  ast.structs.get(&struct_type).unwrap().type_name,
                  _ref.get_ref_string(),
                ))?;
                break;
              }
            }
            ASTNode::AST_Statements(box statements) => {
              let mut reference = String::new();
              let mut return_type = AScriptTypeVal::Undefined;
              let mut refs = BTreeSet::new();
              let mut tokens = BTreeSet::new();
              let mut statement_writer = temp_writer.checkpoint();

              for (i, statement) in statements.statements.iter().enumerate() {
                match render_expression(ast, statement, rule, &mut ref_index, i) {
                  Some(_ref) => {
                    refs.append(&mut _ref.get_indices());
                    tokens.append(&mut _ref.get_tokens());
                    return_type = _ref.ast_type.clone();
                    reference = _ref.get_ref_string();
                    statement_writer.write_line(&_ref.to_init_string())?;
                  }
                  None => {}
                }
              }

              write_slot_extraction(rule, refs, tokens, &mut temp_writer)?;

              write_node_token(&mut temp_writer, rule)?;

              temp_writer.merge_checkpoint(statement_writer)?;

              let return_type = match return_type {
                AScriptTypeVal::Undefined | AScriptTypeVal::GenericVec(None) => {
                  prod_data.iter().next().unwrap().0.into()
                }
                r => r,
              };

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
                  "slots.assign(0, AstSlot ({}::{}({}), {}, TokenRange::default()))",
                  ast.ast_type_name,
                  return_type.hcobj_type_name(None),
                  &reference,
                  "rng".to_string()
                ))?,
                r => {
                  eprintln!("{}", r.debug_string(None));
                  temp_writer.write_line(&reference)?
                }
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
      w.merge_checkpoint(temp_writer)?;
      refs.push(format!("/* {} */ {}", id, fn_name));
    } else {
      refs.push(format!("/* {} {} */ default_fn", id, noop));
    }
  }

  // Reduce Function Array -----------------------

  w.wrt(&format!(
    "
struct ReduceFunctions<R: Reader + UTF8Reader, M>(pub [Reducer<R, M, {0}>; {1}]);
impl<R: Reader + UTF8Reader, M> ReduceFunctions<R, M> {{
  pub const fn new() -> Self {{
    Self([
      {2}
    ])
  }}
}}
",
    ast.ast_type_name,
    ordered_bodies.len(),
    &refs.iter().map(|r| format!("{}::<R, M>", r)).collect::<Vec<String>>().join(",\n")
  ))?
  .newline()?;

  Ok(())
}

fn write_node_token(temp_writer: &mut CodeWriter<Vec<u8>>, rule: &&Rule) -> Result<()> {
  temp_writer.write_line(&format!(
    "let rng = {};",
    if rule.syms.len() > 1 {
      format!("rng0 + rng{}", rule.syms.len() - 1)
    } else {
      "rng0".to_string()
    }
  ))?;
  Ok(())
}

fn write_slot_extraction<'a>(
  rule: &Rule,
  indices: BTreeSet<usize>,
  used_tokens: BTreeSet<usize>,
  temp_writer: &'a mut CodeWriter<Vec<u8>>,
) -> Result<()> {
  Ok(for i in 0..rule.syms.len() {
    let assignment_string = match (
      match (i, used_tokens.contains(&i)) {
        (0, _) => Some("rng0".to_string()),
        (i, _) if i == (rule.syms.len() - 1) => Some(format!("rng{}", i)),
        (i, true) => Some(format!("rng{}", i)),
        rng => None,
      },
      indices.contains(&i),
    ) {
      (Some(range_str), true) => format!("let AstSlot (i{}, {}, _) = ", i, range_str),
      (Some(range_str), false) => format!("let AstSlot (_, {}, _) = ", range_str),
      (None, true) => format!("let AstSlot (i{}, ..) = ", i),
      (None, false) => "".to_string(),
    };

    temp_writer.wrtln(&(assignment_string + &format!("slots.take({0});", i)))?;
  })
}

fn build_struct_constructor(
  ast: &AScriptStore,
  rule: &Rule,
  struct_type: &AScriptStructId,
  ast_struct: &AST_Struct,
  ref_index: &mut usize,
  type_slot: usize,
) -> Result<Ref> {
  let archetype_struct = ast.structs.get(struct_type).unwrap();
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

  for (_, val_ref) in archetype_struct.prop_ids.iter().enumerate().map(|(i, prop_id)| {
    let struct_prop_ref = if let Some(ast_prop) = ast_struct_props.get(&prop_id.name) {
      let property = ast.props.get(prop_id).unwrap();
      let ref_ = render_expression(ast, &ast_prop.value, rule, ref_index, i + type_slot * 100);
      let (string, ref_) =
        create_type_initializer_value(ref_, &(&property.type_val).into(), property.optional, ast);
      if let Some(ref_) = ref_ {
        predecessors.push(ref_);
      }

      string
    } else {
      get_default_value(prop_id, ast)
    };

    (prop_id.name.clone(), struct_prop_ref)
  }) {
    writer.wrt(&format!("{}, ", val_ref))?;
  }

  if archetype_struct.include_token {
    writer.wrt(&format!("{}, ", "rng.to_token(_ctx_.get_reader())"))?;
  }

  writer.dedent().write_line("\n)")?;

  (*ref_index) += 1;

  let mut ref_ = Ref::new(
    *ref_index,
    type_slot,
    String::from_utf8(writer.into_output()).unwrap(),
    AScriptTypeVal::Struct(*struct_type),
  );

  ref_.add_predecessors(predecessors);

  Ok(ref_)
}

pub(crate) fn create_type_initializer_value(
  ref_: Option<Ref>,
  type_val: &AScriptTypeVal,
  optional: bool,
  ast: &AScriptStore,
) -> (String, Option<Ref>) {
  match ref_ {
    Some(ref_) => {
      let mut string = match type_val {
        AScriptTypeVal::GenericStructVec(structs_ids) if structs_ids.len() == 1 => {
          format!(
                "{}.into_iter().map(|v|match v {{ {}::{}(node) => node, _ => panic!(\"could not convert\")}}).collect::<Vec<_>>()",
                ref_.get_ref_string(),
                ast.ast_type_name,
                ast.structs.get(&structs_ids.first().unwrap().into()).unwrap().type_name
              )
        }
        _ => ref_.get_ref_string(),
      };

      let ref_ = match type_val {
        AScriptTypeVal::Struct(..) => node_to_struct(ref_, ast),
        _ => ref_,
      };

      if optional
        && matches!(type_val, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..))
      {
        string = format!("Some({})", string);
      }

      (string, Some(ref_))
    }
    None => ("Default::default()".to_string(), None),
  }
}

fn build_structs<W: Write>(ast: &AScriptStore, o: &mut CodeWriter<W>) -> Result<()> {
  // Build structs
  for (_, AScriptStruct { include_token, prop_ids: properties, type_name, .. }) in &ast.structs {
    let mut properties = properties
      .iter()
      .map(|p| {
        let AScriptProp { type_val, optional, .. } = ast.props.get(p).unwrap();
        (p.name.clone(), ascript_type_to_string(&type_val.into(), ast), type_val.into(), *optional)
      })
      .collect::<Vec<_>>();

    if *include_token {
      properties.push(("tok".to_string(), "Token".to_string(), AScriptTypeVal::Token, false));
    }

    o.wrtln(&format!("#[derive(Debug, Clone)]\npub struct {} {{", type_name))?.indent();

    for (name, type_string, type_val, optional) in &properties {
      match optional {
        true => match type_val {
          AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..) => {
            o.write_line(&format!("pub {}: Option<{}>,", name, type_string))?
          }
          _ => o.write_line(&format!("pub {}: {},", name, type_string))?,
        },
        false => o.write_line(&format!("pub {}: {},", name, type_string))?,
      };
    }

    o.dedent().wrtln("}")?;

    // Create the Nodes Member functions

    o.wrtln(&format!("impl {} {{", type_name))?.indent();

    // NODE::new

    o.wrtln("#[inline]\npub fn new (")?.indent();

    for (name, type_string, type_val, optional) in &properties {
      match optional {
        true => match type_val {
          AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..) => {
            o.write_line(&format!("{}:Option<{}>,", name, type_string))?
          }
          _ => o.write_line(&format!("{}:{},", name, type_string))?,
        },
        false => o.write_line(&format!("{}:{},", name, type_string))?,
      };
    }
    o.dedent().wrtln(") -> Self {")?.indent().wrtln("Self{")?.indent();

    for (name, ..) in &properties {
      o.write_line(&format!("{},", name))?;
    }

    o.dedent().wrtln("}")?.dedent().wrtln("}")?;

    // NODE::get_type
    o.write_line(&format!(
      "pub fn get_type(&self) -> {0}Type {{ {0}Type::{1} }}",
      ast.ast_type_name, type_name
    ))?;

    o.dedent().wrtln("}")?;
  }

  Ok(())
}

pub(crate) fn render_expression(
  ast: &AScriptStore,
  ast_expression: &ASTNode,
  rule: &Rule,
  ref_index: &mut usize,
  type_slot: usize,
) -> Option<Ref> {
  let (b, s) = (rule, ast);

  match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(ast_struct) {
        if let Ok(_ref) =
          build_struct_constructor(ast, rule, &struct_type, ast_struct, ref_index, type_slot)
        {
          Some(_ref)
        } else {
          None
        }
      } else {
        None
      }
    }
    ASTNode::AST_Token(box AST_Token {}) => {
      Some(Ref::node_token(bump_ref_index(ref_index), type_slot))
    }
    ASTNode::AST_Add(..) => Some(Ref::token(bump_ref_index(ref_index), type_slot)),
    ASTNode::AST_Vector(box AST_Vector { initializer, .. }) => {
      let mut results = initializer
        .iter()
        .filter_map(|n| render_expression(ast, n, rule, ref_index, type_slot))
        .collect::<VecDeque<_>>();

      if results.is_empty() {
        Some(Ref::new(
          bump_ref_index(ref_index),
          type_slot,
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
            type_slot,
            "vec![]".to_string(),
            get_specified_vector_from_generic_vec_values(&types),
          )
        };

        for mut _ref in results {
          if _ref.ast_type.is_vec() {
            _ref.make_mutable();
            let val_ref = _ref.get_ref_string();
            vector_ref
              .add_post_init_expression(format!("%%.append(&mut {})", val_ref))
              .make_mutable()
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
        type_slot,
        format!("\"{}\".to_string()", value),
        AScriptTypeVal::String(Some(value.to_string())),
      )),
      _ => {
        let ref_ = render_expression(s, value, b, ref_index, type_slot)?;
        match ref_.ast_type {
          AScriptTypeVal::Struct(..)
          | AScriptTypeVal::TokenRange
          | AScriptTypeVal::GenericStruct(..) => Some(ref_.to_string(AScriptTypeVal::String(None))),
          AScriptTypeVal::TokenVec => {
            // Merge the last and first token together
            // get the string value from the resulting span of the union
            Some(ref_.from(
              "(%%.first().unwrap() + %%.last().unwrap()).to_string()".to_string(),
              AScriptTypeVal::String(None),
            ))
          }
          AScriptTypeVal::String(..) => Some(ref_),
          _ => Some(ref_.from("%%.to_string()".to_string(), AScriptTypeVal::String(None))),
        }
      }
    },
    ASTNode::AST_BOOL(box AST_BOOL { value, initializer, .. }) => match initializer {
      ASTNode::NONE => Some(Ref::new(
        bump_ref_index(ref_index),
        type_slot,
        format!("{}", value),
        AScriptTypeVal::Bool(Some(*value)),
      )),
      ast => match render_expression(s, ast, rule, ref_index, type_slot) {
        Some(_) => Some(Ref::new(
          bump_ref_index(ref_index),
          type_slot,
          "true".to_string(),
          AScriptTypeVal::Bool(Some(true)),
        )),
        None => Some(Ref::new(
          bump_ref_index(ref_index),
          type_slot,
          "false".to_string(),
          AScriptTypeVal::Bool(Some(false)),
        )),
      },
    },
    ASTNode::AST_U64(box AST_U64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU64>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_U32(box AST_U32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU32>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_U16(box AST_U16 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU16>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_U8(box AST_U8 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValU8>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_I64(box AST_I64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI64>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_I32(box AST_I32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI32>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_I16(box AST_I16 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI16>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_I8(box AST_I8 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValI8>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_F32(box AST_F32 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValF32>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_F64(box AST_F64 { initializer, .. }) => {
      convert_numeric::<AScriptTypeValF64>(initializer, b, s, ref_index, type_slot)
    }
    ASTNode::AST_NUMBER(..) => None,
    ASTNode::AST_Member(..) => None,
    ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) => {
      match get_named_body_ref(rule, value) {
        Some((index, sym_id)) => render_body_symbol(sym_id, ast, index, type_slot),
        None => None,
      }
    }
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      match get_indexed_body_ref(rule, value) {
        Some((index, sym_id)) => render_body_symbol(sym_id, ast, index, type_slot),
        None => None,
      }
    }
    _ => None,
  }
}

fn node_to_struct(ref_: Ref, ast: &AScriptStore) -> Ref {
  use AScriptTypeVal::*;
  match ref_.ast_type.clone() {
    Struct(struct_type) => {
      let struct_name = ast.structs.get(&struct_type).unwrap().type_name.clone();
      ref_.from(
        format!(
          "if let {}::{}(obj) = %%
      {{ obj }}
      else {{panic!(\"invalid node\")}}",
          ast.ast_type_name, struct_name
        ),
        Struct(struct_type),
      )
    }
    GenericStruct(struct_types) if struct_types.len() == 1 => {
      let struct_type = struct_types.first().unwrap();
      let struct_name = ast.structs.get(&(struct_type.into())).unwrap().type_name.clone();
      ref_.from(
        format!(
          "if let {}::{}(obj) = %%
      {{ obj }}
      else {{panic!(\"invalid node\")}}",
          ast.ast_type_name, struct_name
        ),
        Struct(struct_type.into()),
      )
    }
    _ => ref_,
  }
}

fn render_body_symbol(
  sym: &RuleSymbol,
  ast: &AScriptStore,
  i: usize,
  type_slot: usize,
) -> Option<Ref> {
  use AScriptTypeVal::*;
  let mut ref_ = match &sym.sym_id {
    SymbolID::Production(prod_id, ..) => {
      let types = get_production_types(ast, prod_id);
      if types.len() == 1 {
        let _type = types.first().unwrap().clone();
        if Token == _type {
          Ref::token(i, type_slot)
        } else {
          if let Some(init_string) = match _type {
            F64(..) => Some(format!("i{0}.to_f64()", i)),
            F32(..) => Some(format!("i{0}.to_f32()", i)),
            U64(..) => Some(format!("i{0}.to_u64()", i)),
            I64(..) => Some(format!("i{0}.to_i64()", i)),
            U32(..) => Some(format!("i{0}.to_u32()", i)),
            I32(..) => Some(format!("i{0}.to_i32()", i)),
            U16(..) => Some(format!("i{0}.to_u16()", i)),
            I16(..) => Some(format!("i{0}.to_i16()", i)),
            U8(..) => Some(format!("i{0}.to_u8()", i)),
            I8(..) => Some(format!("i{0}.to_i8()", i)),
            String(..) => Some(format!("i{0}.to_string()", i)),
            GenericStructVec(..) => Some(format!("i{0}.into_nodes()", i)),
            StringVec => Some(format!("i{0}.into_strings()", i)),
            TokenVec => Some(format!("i{0}.into_tokens()", i)),
            F64Vec => Some(format!("i{0}.into_f64_vec()", i)),
            F32Vec => Some(format!("i{0}.into_f32_vec()", i)),
            U64Vec => Some(format!("i{0}.into_u64_vec()", i)),
            I64Vec => Some(format!("i{0}.into_i64_vec()", i)),
            U32Vec => Some(format!("i{0}.into_u32_vec()", i)),
            I32Vec => Some(format!("i{0}.into_i32_vec()", i)),
            U16Vec => Some(format!("i{0}.into_u16_vec()", i)),
            I16Vec => Some(format!("i{0}.into_i16_vec()", i)),
            U8Vec => Some(format!("i{0}.into_u8_vec()", i)),
            I8Vec => Some(format!("i{0}.into_i8_vec()", i)),
            GenericStruct(..) => Some(format!("i{0}", i)),
            Struct(..) => Some(format!("i{0}", i)),
            _ => None,
          } {
            Ref::new(i, type_slot, init_string, _type)
          } else {
            Ref::new(i, type_slot, "".to_string(), match _type.to_owned() {
              GenericVec(types) => get_specified_vector_from_generic_vec_values(
                &types.unwrap().iter().map(|t| t.into()).collect(),
              ),
              _type => _type,
            })
          }
        }
      } else if production_types_are_structs(&types) {
        Ref::new(i, type_slot, format!("i{0}", i), GenericStruct(extract_struct_types(&types)))
      } else {
        Ref::token(i, type_slot)
      }
    }
    _ => Ref::token(i, type_slot),
  };

  ref_.add_body_index(i);

  Some(ref_)
}

fn extract_struct_types(types: &BTreeSet<AScriptTypeVal>) -> BTreeSet<TaggedType> {
  types
    .iter()
    .filter_map(|t| match t {
      AScriptTypeVal::Struct(_) => Some(TaggedType { type_: t.clone(), ..Default::default() }),
      _ => None,
    })
    .collect::<BTreeSet<_>>()
}

fn convert_numeric<T: AScriptNumericType>(
  init: &ASTNode,
  rule: &Rule,
  ast: &AScriptStore,
  ref_index: &mut usize,
  type_slot: usize,
) -> Option<Ref> {
  let rust_type = T::prim_type_name();
  let tok_conversion_fn = T::to_fn_name();
  let range_conversion_fn = T::from_tok_range_name();

  match init {
    ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Some(Ref::new(
      bump_ref_index(ref_index),
      type_slot,
      format!("{}{}", T::string_from_f64(*value), rust_type,),
      T::from_f64(*value),
    )),
    _ => {
      let ref_ = render_expression(ast, init, rule, ref_index, type_slot)?;

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
          Some(ref_.from(format!("%% as {}", rust_type), T::from_f64(0.0)))
        }
        AScriptTypeVal::TokenRange => {
          Some(ref_.from(format!("%%.{}(_ctx_.get_str())", range_conversion_fn), T::from_f64(0.0)))
        }
        _ => Some(ref_.from(format!("%%.{}()", tok_conversion_fn), T::from_f64(0.0))),
      }
    }
  }
}

fn bump_ref_index(ref_index: &mut usize) -> usize {
  *ref_index += 1;
  *ref_index
}

pub fn ascript_type_to_string(ast_type: &AScriptTypeVal, ast: &AScriptStore) -> String {
  use AScriptTypeVal::*;
  match ast_type {
    GenericVec(types) => format!("Vec<{:?}>", types),
    Struct(id) => format!("Box<{}>", ast.structs.get(id).unwrap().type_name),
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
      let production_types = get_production_types(ast, prod_id);
      if production_types.len() > 1 {
        if production_types_are_structs(&production_types) {
          ast.ast_type_name.clone()
        } else {
          format!("{}::None", ast.ast_type_name)
        }
      } else {
        ascript_type_to_string(&production_types.first().unwrap(), ast)
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
    TokenVec => "Vec<Token>".to_string(),
    StringVec => "Vec<String>".to_string(),
    GenericStruct(struct_ids) => {
      if struct_ids.len() > 1 {
        ast.ast_type_name.clone()
      } else {
        format!("Box<{}>", ast.structs.get(&struct_ids.first().unwrap().into()).unwrap().type_name)
      }
    }
    GenericStructVec(struct_ids) => {
      if struct_ids.len() > 1 {
        format!("Vec<{}>", ast.ast_type_name)
      } else {
        format!(
          "Vec<Box<{}>>",
          ast.structs.get(&struct_ids.first().unwrap().into()).unwrap().type_name
        )
      }
    }
    _ => {
      panic!("Could not resolve compiled ascript type {:?}", ast_type)
    }
  }
}

fn get_default_value_(ast_type: &AScriptTypeVal, ast: &AScriptStore) -> String {
  use AScriptTypeVal::*;
  match ast_type {
    GenericVec(..) => "vec![]".to_string(),
    Struct(..) => "None".to_string(),
    GenericStruct(..) => "None".to_string(),
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
      let production_types = get_production_types(ast, prod_id);
      if production_types.len() > 1 {
        if production_types_are_structs(&production_types) {
          ast.ast_type_name.clone()
        } else {
          "HCObj::None".to_string()
        }
      } else {
        get_default_value_(&production_types.first().unwrap(), ast)
      }
    }
    F64Vec | F32Vec | U64Vec | I64Vec | U32Vec | I32Vec | U16Vec | I16Vec | U8Vec | I8Vec
    | GenericStructVec(..) | StringVec | TokenVec => "Vec::new()".to_string(),
    _ => {
      panic!("Could not resolve compiled ascript type {:?}", ast_type)
    }
  }
}

fn get_default_value(prop_id: &AScriptPropId, ast: &AScriptStore) -> String {
  if let Some(prop) = ast.props.get(prop_id) {
    get_default_value_(&(&prop.type_val).into(), ast)
  } else {
    "None".to_string()
  }
}

#[derive(Clone)]
pub(crate) struct Ref {
  init_index: usize,
  type_slot: usize,
  body_indices: BTreeSet<usize>,
  init_expression: String,
  ast_type: AScriptTypeVal,
  predecessors: Option<Vec<Box<Ref>>>,
  post_init_statements: Option<Vec<String>>,
  is_mutable: bool,
  is_token: bool,
}

impl Ref {
  pub fn new(
    init_index: usize,
    type_slot: usize,
    init_expression: String,
    ast_type: AScriptTypeVal,
  ) -> Self {
    Ref {
      init_index,
      type_slot,
      body_indices: BTreeSet::from_iter(vec![init_index]),
      init_expression,
      ast_type,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
      is_token: false,
    }
  }

  pub(crate) fn token(init_index: usize, type_slot: usize) -> Self {
    Ref {
      init_index,
      type_slot,
      body_indices: BTreeSet::new(),
      init_expression: format!("rng{0}.to_token(_ctx_.get_reader())", init_index),
      ast_type: AScriptTypeVal::Token,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
      is_token: true,
    }
  }

  pub(crate) fn node_token(init_index: usize, type_slot: usize) -> Self {
    Ref {
      init_index,
      type_slot,
      body_indices: BTreeSet::new(),
      init_expression: format!("rng.to_token(_ctx_.get_reader())"),
      ast_type: AScriptTypeVal::Token,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
      is_token: false,
    }
  }

  pub(crate) fn to_string(self, ast_type: AScriptTypeVal) -> Self {
    let index = self.get_root_index();
    Ref {
      init_index: index,
      type_slot: self.type_slot,
      init_expression: format!("rng{0}.to_token(_ctx_.get_reader()).to_string()", index),
      ast_type,
      body_indices: BTreeSet::new(),
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
      is_token: true,
    }
  }

  pub(crate) fn from(self, init_expression: String, ast_type: AScriptTypeVal) -> Self {
    Ref {
      init_index: self.init_index,
      type_slot: self.type_slot,
      init_expression,
      ast_type,
      body_indices: BTreeSet::new(),
      predecessors: Some(vec![Box::new(self)]),
      post_init_statements: None,
      is_mutable: false,
      is_token: false,
    }
  }

  pub(crate) fn get_type(&self) -> AScriptTypeVal {
    self.ast_type.clone()
  }

  pub(crate) fn make_mutable(&mut self) -> &mut Self {
    self.is_mutable = true;
    self
  }

  pub(crate) fn add_body_index(&mut self, index: usize) {
    self.body_indices.insert(index);
  }

  pub(crate) fn get_ref_string(&self) -> String {
    format!("ref_{}_{}", self.init_index, self.type_slot)
  }

  pub(crate) fn get_indices(&self) -> BTreeSet<usize> {
    let mut indices = self.body_indices.clone();

    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        indices.append(&mut predecessor.get_indices())
      }
    }

    indices
  }

  pub(crate) fn get_root_index(&self) -> usize {
    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        return predecessor.get_root_index();
      }
    }
    self.init_index
  }

  pub(crate) fn get_tokens(&self) -> BTreeSet<usize> {
    let mut set = BTreeSet::new();

    if self.is_token {
      set.insert(self.init_index);
    }

    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        set.append(&mut predecessor.get_tokens());
      }
    }

    set
  }

  pub(crate) fn add_post_init_expression(&mut self, string: String) -> &mut Self {
    self.post_init_statements.get_or_insert(vec![]).push(string);

    self
  }

  pub(crate) fn to_init_string(&self) -> String {
    let mut string = String::new();

    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        string = string + &predecessor.to_init_string()
      }
    }

    let ref_string = self.get_ref_string();

    string += &format!(
      "\nlet{}{} = {};",
      if self.is_mutable { " mut " } else { " " },
      ref_string,
      self.init_expression.replace("%%", &ref_string)
    );

    if let Some(statements) = &self.post_init_statements {
      string = string + "\n" + &statements.join(";\n").replace("%%", &ref_string) + ";";
    }

    string
  }

  pub(crate) fn add_predecessor(&mut self, predecessor: Ref) -> &mut Self {
    self.predecessors.get_or_insert(vec![]).push(Box::new(predecessor));

    self
  }

  pub(crate) fn add_predecessors(&mut self, predecessors: Vec<Ref>) -> &mut Self {
    let prev = self.predecessors.get_or_insert(vec![]);

    for predecessor in predecessors {
      prev.push(Box::new(predecessor))
    }

    self
  }
}
