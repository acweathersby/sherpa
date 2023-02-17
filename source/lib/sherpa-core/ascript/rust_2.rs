use sherpa_runtime::types::ast::DEFAULT_AST_TYPE_NAMES;

use super::{
  compile::get_named_body_ref,
  output_base::AscriptPropHandler,
  types::AScriptNumericType,
};
use crate::{
  ascript::{
    compile::{
      get_indexed_body_ref,
      get_production_types,
      get_specified_vector_from_generic_vec_values,
      get_struct_type_from_node,
      production_types_are_structs,
    },
    output_base::{
      ASTExprHandler,
      AscriptTypeHandler,
      AscriptWriter,
      AscriptWriterUtils,
      Ref,
      TokenCreationType,
    },
    types::{
      AScriptStore,
      AScriptTypeVal,
      AScriptTypeValF32,
      AScriptTypeValF64,
      AScriptTypeValI16,
      AScriptTypeValI32,
      AScriptTypeValI64,
      AScriptTypeValI8,
      AScriptTypeValU16,
      AScriptTypeValU32,
      AScriptTypeValU64,
      AScriptTypeValU8,
      TaggedType,
    },
  },
  grammar::compile::parser::sherpa::{
    self,
    ASTNode,
    AST_IndexReference,
    AST_NamedReference,
    AST_Token,
    AST_Vector,
    Init,
    Range,
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
  writer::code_writer::CodeWriter,
};
use std::{
  collections::{BTreeSet, VecDeque},
  io::Write,
};

pub fn build_rust<W: Write>(
  store: &AScriptStore,
  writer: CodeWriter<W>,
) -> SherpaResult<CodeWriter<W>> {
  let node_type = &store.ast_type_name;

  let mut u = AscriptWriterUtils::new(
    store,
    // General Assignment
    &|_, _, name, value, mutable| {
      if mutable {
        format!("let mut {name} = {value};")
      } else {
        format!("let {name} = {value};")
      }
    },
    // Slot Assignment
    &|utils, type_, ref_| match type_ {
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
      | AScriptTypeVal::Token => format!(
        "slots.assign(0, AstSlot({}::{}({}), {}, TokenRange::default()));",
        utils.store.ast_type_name,
        type_.hcobj_type_name(None),
        &ref_,
        (utils.get_token_name)(0)
      ),
      AScriptTypeVal::Struct(struct_id) => {
        let struct_name = utils.store.structs.get(struct_id).unwrap();
        format!(
          "slots.assign(0, AstSlot({}::{}(Box::new({})), {}, TokenRange::default()));",
          utils.store.ast_type_name,
          struct_name.type_name,
          ref_,
          (utils.get_token_name)(0)
        )
      }
      AScriptTypeVal::Any => format!(
        "slots.assign(0, AstSlot({}, {}, TokenRange::default()));",
        &ref_,
        (utils.get_token_name)(0)
      ),
      _ => "INVALID_ASSIGNMENT".to_string(),
    },
    &|first, last| format!("{first} + {last}"),
    // Slot Extraction
    &|token, node, index| match (node, token) {
      (Some(n), Some(t)) => format!("let AstSlot ({n}, {t}, _) = slots.take({});", index),
      (None, Some(t)) => format!("let AstSlot (_, {t}, _) = slots.take({});", index),
      (Some(n), None) => format!("let AstSlot ({n}, _, _) = slots.take({});", index),
      _ => format!("slots.take({});", index),
    },
    // Token Creation from Token Range
    &|tok_name, token_type| match token_type {
      TokenCreationType::String => format!("{tok_name}.to_token(_ctx_.get_reader()).to_string()"),
      TokenCreationType::Token => format!("{tok_name}.to_token(_ctx_.get_reader())"),
    },
    //Token Range Name
    &|i| match i {
      0 => "__rule_rng".into(),
      _ => format!("__tok_rng_{i}"),
    },
    &|u, w, node_name, prop_assignments, include_token| {
      w.write(&format!("{node_name}::new("))?;
      let mut entries: Vec<String> =
        prop_assignments.iter().map(|(_, val, _)| val.to_string()).collect();
      if include_token {
        entries.push((u.get_token_name)(0) + ".to_token(_ctx_.get_reader())")
      }
      if entries.len() > 0 {
        w.increase_indent();
        for entry in entries {
          w.write_line(&format!("{entry},"))?;
        }
        w.decrease_indent();
        w.write_line(")")?;
      } else {
        w.write(")")?;
      }

      SherpaResult::Ok(())
    },
  );
  u.add_type_handler(AScriptTypeVal::TokenVec, AscriptTypeHandler {
    default: &|_, _, _| "Vec::new()".into(),
    name:    &|_, _, _| "Vec<Token>".into(),
  });
  u.add_type_handler(AScriptTypeVal::Token, AscriptTypeHandler {
    default: &|_, _, _| "Defualt::defualt()".into(),
    name:    &|_, _, _| "Token".into(),
  });
  u.add_type_handler(AScriptTypeVal::Bool(None), AscriptTypeHandler {
    default: &|_, _, _| "false".into(),
    name:    &|_, _, _| "bool".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_BOOL, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_BOOL(box AST_BOOL { value, initializer, .. }) => match initializer {
        None => Some(Ref::new(
          u.bump_ref_index(ref_index),
          type_slot,
          format!("{}", value),
          AScriptTypeVal::Bool(Some(*value)),
        )),
        Some(box init) => match u.ast_expr_to_ref(&init.expression, r, ref_index, type_slot) {
          Some(_) => Some(Ref::new(
            u.bump_ref_index(ref_index),
            type_slot,
            "true".to_string(),
            AScriptTypeVal::Bool(Some(true)),
          )),
          None => Some(Ref::new(
            u.bump_ref_index(ref_index),
            type_slot,
            "false".to_string(),
            AScriptTypeVal::Bool(Some(false)),
          )),
        },
      },
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::String(None), AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "String".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_STRING, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_STRING(box AST_STRING { value, .. }) => {
        match value {
          None => Some(Ref::new(
            u.bump_ref_index(ref_index),
            type_slot,
            "String::new()".to_string(),
            AScriptTypeVal::String(None),
          )),
          Some(box init) => {
            match &init.expression {
              ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Some(Ref::new(
                u.bump_ref_index(ref_index),
                type_slot,
                format!("\"{}\".to_string()", value),
                AScriptTypeVal::String(Some(value.to_string())),
              )),
              expr => {
                let ref_ = u.ast_expr_to_ref(expr, r, ref_index, type_slot)?;
                match ref_.ast_type {
                  AScriptTypeVal::Struct(..)
                  | AScriptTypeVal::TokenRange
                  | AScriptTypeVal::GenericStruct(..) => {
                    Some(ref_.to_string(u, AScriptTypeVal::String(None)))
                  }
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
            }
          }
        }
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::I8(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i8".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_I8, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I8(box AST_I8 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI8>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::I16(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i16".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_I16, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I16(box AST_I16 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI16>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::I32(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i32".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_I32, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I32(box AST_I32 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI32>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::I64(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i64".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_I64, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I64(box AST_I64 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI64>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::U8(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u8".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_U8, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U8(box AST_U8 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU8>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::U16(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u16".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_U16, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U16(box AST_U16 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU16>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::U32(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u32".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_U32, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U32(box AST_U32 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU32>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::U64(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u64".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_U64, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U64(box AST_U64 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU64>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::F32(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "f32".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_F32, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_F32(box AST_F32 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValF32>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::F64(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "f64".into(),
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_F64, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_F64(box AST_F64 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValF64>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_type_handler(AScriptTypeVal::Struct(Default::default()), AscriptTypeHandler {
    default: &|_, _, optional| match optional {
      true => "None".into(),
      _ => "[NO OPTIONAL]".into(),
    },
    name:    &|s, a, optional| match a {
      AScriptTypeVal::Struct(struct_id) => {
        let name = &s.structs.get(&struct_id).unwrap().type_name;
        match optional {
          true => format!("Option<Box<{name}>>"),
          _ => format!("Box<{name}>"),
        }
      }
      _ => Default::default(),
    },
  });
  u.add_type_handler(AScriptTypeVal::GenericStruct(Default::default()), AscriptTypeHandler {
    default: &|s, _, optional| match optional {
      true => "None".into(),
      false => format!("{}::NONE", s.ast_type_name),
    },
    name:    &|s, _, optional| match optional {
      true => format!("Option<{}>", s.ast_type_name),
      false => format!("{}", s.ast_type_name),
    },
  });
  u.add_type_handler(AScriptTypeVal::GenericStructVec(Default::default()), AscriptTypeHandler {
    default: &|_, _, _| format!("vec![]"),
    name:    &|u, val, _| match &val {
      AScriptTypeVal::GenericStructVec(vec) => match vec.len() {
        1 => {
          format!("Vec<Box<{}>>", u.structs.get(&vec.first().unwrap().into()).unwrap().type_name)
        }
        _ => format!("Vec<{}>", u.ast_type_name),
      },
      _ => "[GenericStructVec]".into(),
    },
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_Struct, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_Struct(ast_struct) => {
        if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(ast_struct) {
          match u.build_struct_constructor(r, &struct_type, ast_struct, ref_index, type_slot) {
            SherpaResult::Ok(_ref) => Some(_ref),
            _ => None,
          }
        } else {
          None
        }
      }
      _ => None,
    },
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_Vector, ASTExprHandler {
    expr: &|utils, ast, rule, ref_index, type_slot| {
      if let ASTNode::AST_Vector(box AST_Vector { initializer, .. }) = ast {
        let mut results = initializer
          .iter()
          .filter_map(|n| utils.ast_expr_to_ref(n, rule, ref_index, type_slot))
          .collect::<VecDeque<_>>();

        if results.is_empty() {
          Some(Ref::new(
            utils.bump_ref_index(ref_index),
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
              utils.bump_ref_index(ref_index),
              type_slot,
              "vec![]".to_string(),
              get_specified_vector_from_generic_vec_values(&types),
            )
          };

          for mut _ref in results {
            if _ref.ast_type.is_vec() {
              _ref.make_mutable();
              let val_ref = _ref.get_ref_string();
              vector_ref.add_post_init_stmt(format!("%%.append(&mut {});", val_ref)).make_mutable()
            } else {
              let val_ref = _ref.get_ref_string();
              vector_ref.add_post_init_stmt(format!("%%.push({});", val_ref)).make_mutable()
            };

            vector_ref.add_predecessor(_ref);
          }

          Some(vector_ref)
        }
      } else {
        None
      }
    },
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_Token, ASTExprHandler {
    expr: &|u, ast, _, ref_index, type_slot| {
      if let ASTNode::AST_Token(box AST_Token { range, .. }) = ast {
        let ref_ = Ref::node_token(u, u.bump_ref_index(ref_index), type_slot);
        if let Some(box Range { start_trim, end_trim }) = range {
          let mut trimed_ref = Ref::new(
            *ref_index,
            type_slot,
            format!("%%.trim({start_trim}, {end_trim})"),
            AScriptTypeVal::TokenRange,
          );
          trimed_ref.add_predecessor(ref_);
          Some(trimed_ref)
        } else {
          Some(ref_)
        }
      } else {
        None
      }
    },
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_IndexReference, ASTExprHandler {
    expr: &|u, ast, rule, _, type_slot| {
      if let ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) = ast {
        match get_indexed_body_ref(rule, value) {
          Some((index, sym_id)) => render_body_symbol(u, sym_id, u.store, index, type_slot),
          None => None,
        }
      } else {
        None
      }
    },
  });
  u.add_ast_handler(sherpa::ASTNodeType::AST_NamedReference, ASTExprHandler {
    expr: &|u, ast, rule, _, type_slot| {
      if let ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) = ast {
        match get_named_body_ref(rule, value) {
          Some((index, sym_id)) => render_body_symbol(u, sym_id, u.store, index, type_slot),
          None => None,
        }
      } else {
        None
      }
    },
  });

  // Convert Vec<AstNode> to Vec<<Specific Node>> when the Generic Structs Vecs
  // only have one type.
  u.add_prop_handler(
    AScriptTypeVal::GenericStructVec(Default::default()),
    AscriptPropHandler {
      expr: &|u, ref_, prop_type_, _|
        match ref_ {
        Some(ref_) => match prop_type_ {
            AScriptTypeVal::GenericStructVec(structs_ids) if structs_ids.len() == 1 => {
                (format!(
                      "{}.into_iter().map(|v|match v {{ {}::{}(node) => node, _ => panic!(\"could not convert\")}}).collect::<Vec<_>>()",
                      ref_.get_ref_string(),
                      u.store.ast_type_name,
                      u.store.structs.get(&structs_ids.first().unwrap().into()).unwrap().type_name
                    ), Some(ref_))
              }
              _ => (ref_.get_ref_string(), Some(ref_)),
        }
        None => ("Default::default()".into(), Default::default()),
      }
    },
  );

  fn handle_struct_props(
    utils: &AscriptWriterUtils,
    ref_: Option<Ref>,
    prop_type_: &AScriptTypeVal,
    optional: bool,
  ) -> (String, Option<Ref>) {
    let store = utils.store;
    use AScriptTypeVal::*;
    match ref_ {
      Some(ref_) => {
        let ref_string = ref_.get_ref_string();
        let out_ref_ = if let Struct(..) = prop_type_ {
          match ref_.ast_type.clone() {
            Struct(struct_type) => {
              let struct_name = store.structs.get(&struct_type).unwrap().type_name.clone();
              ref_.from(
                format!(
                  "if let {}::{}(obj) = %%
                  {{ obj }}
                  else {{panic!(\"invalid node\")}}",
                  store.ast_type_name, struct_name
                ),
                Struct(struct_type),
              )
            }
            GenericStruct(struct_types) if struct_types.len() == 1 => {
              let struct_type = struct_types.first().unwrap();
              let struct_name = store.structs.get(&(struct_type.into())).unwrap().type_name.clone();
              ref_.from(
                format!(
                  "if let {}::{}(obj) = %%
                  {{ obj }}
                  else {{unsafe {{ panic!(\"invalid node {{:?}}\", %%) }}}}",
                  store.ast_type_name, struct_name
                ),
                Struct(struct_type.into()),
              )
            }
            _ => ref_,
          }
        } else {
          ref_.clone()
        };

        match optional {
          true => (format!("Some({})", ref_string), Some(out_ref_)),
          false => (ref_string, Some(out_ref_)),
        }
      }
      None => ("Default::default()".into(), Default::default()),
    }
  }

  u.add_prop_handler(AScriptTypeVal::Struct(Default::default()), AscriptPropHandler {
    expr: &handle_struct_props,
  });

  u.add_prop_handler(AScriptTypeVal::GenericStruct(Default::default()), AscriptPropHandler {
    expr: &handle_struct_props,
  });

  let mut w = AscriptWriter::new(&store, &u, writer);

  // Write macros
  w.stmt(format!(
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
    node_type
  ))?;

  // Write Node Types
  w.block(
    Some(format!("#[derive(Debug, Clone)]\n#[repr(C, u32)]\npub enum {}", node_type)),
    "{",
    "}",
    &|w| {
      let structs: Vec<_> =
        w.store.structs.values().map(|s| format!("{0}(Box<{0}>)", s.type_name)).collect();
      w.list(
        ",",
        vec![
          "NONE",
          format!("NODES(Vec<{}>)", node_type).as_str(),
          "STRING(String)",
          "STRINGS(Vec<String>)",
          "F64(f64)",
          "F32(f32)",
          "I64(i64)",
          "I32(i32)",
          "I16(i16)",
          "I8(i8)",
          "U64(u64)",
          "U32(u32)",
          "U16(u16)",
          "U8(u8)",
          "BOOL(bool)",
          "F32Vec(Vec<f32>)",
          "F64Vec(Vec<f64>)",
          "I64Vec(Vec<i64>)",
          "I32Vec(Vec<i32>)",
          "I16Vec(Vec<i16>)",
          "I8Vec(Vec<i8>)",
          "U64Vec(Vec<u64>)",
          "U32Vec(Vec<u32>)",
          "U16Vec(Vec<u16>)",
          "U8Vec(Vec<u8>)",
          "TOKEN(Token)",
          "TOKENS(Vec<Token>)",
        ]
        .into_iter()
        .chain(structs.iter().map(|s| s.as_str()))
        .collect::<Vec<_>>(),
      );
    },
  )?;

  w.block(
    Some(
      format!("#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]\npub enum {node_type}Type")
        .into(),
    ),
    "{",
    "}",
    &|w| {
      w.list(",", DEFAULT_AST_TYPE_NAMES.to_vec());
      // Write Struct Types
      w.write_struct_data(&|w, s| {
        w.stmt(format!("{},", s.name));
      });
    },
  );

  w.block(Some(format!("pub trait Get{node_type}Type")), "{", "}", &|w| {
    w.stmt(format!("fn get_type(&self) -> {node_type}Type;"));
  });

  w.block(Some(format!("impl Get{node_type}Type for {node_type}")), "{", "}", &|w| {
    let extended_type = node_type.clone() + "Type";
    w.method(
      "fn get_type",
      "(",
      ")",
      ",",
      &|_| vec!["&self".into()],
      &format!("-> {extended_type}"),
      "{",
      "}",
      &mut |w| {
        w.block(Some("match self".into()), "{", "}", &|w| {
          w.write_struct_data(&|w, s| {
            w.stmt(format!("{node_type}::{0}(..) => {extended_type}::{0},", s.name));
          });
          w.stmt(format!("_ => {extended_type}::NONE,"));
        })?;
        SherpaResult::Ok(())
      },
    );
  })?;

  w.block(Some(format!("impl Default for {node_type}")), "{", "}", &|w| {
    w.method("fn default", "(", ")", ",", &|_| vec![], "-> Self", "{", "}", &mut |w| {
      w.stmt(format!("{}::NONE", w.store.ast_type_name))?;
      SherpaResult::Ok(())
    });
  });

  // Write Node Implementation
  w.block(Some(format!("impl {node_type}")), "{", "}", &|w| {
    w.stmt(
      format!("
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
to_numeric!(to_f64, f64);", w.store.ast_type_name)
    );
    w.method(
      "pub fn is_numeric",
      "(",
      ")",
      ",",
      &|_| vec!["&self".into()],
      "-> bool",
      "{",
      "}",
      &mut |w| {
        w.stmt(format!("use {node_type}::*;").into())?;
        w.stmt(
          "matches!(self, F64(_) | F32(_)| I64(_)| I32(_)| I16(_)| I8(_)| U64(_)| U32(_)| U16(_)| U8(_))"
          .into(),
        )?;
        SherpaResult::Ok(())
      },
    );

    w.method(
      "pub fn to_bool",
      "(",
      ")",
      ",",
      &|_| vec!["&self".into()],
      "-> bool",
      "{",
      "}",
      &mut |w| {
        w.stmt("self.to_u8() != 0".into())?;
        SherpaResult::Ok(())
      },
    );

    w.method(
      "pub fn into_strings",
      "(",
      ")",
      ",",
      &|_| vec!["self".into()],
      "-> Vec<String>",
      "{",
      "}",
      &mut |w| {
        w.block(Some("match self".into()), "{", "}", &|w| {
          w.list(",", vec![
            format!("{}::STRINGS(strings) => strings", w.store.ast_type_name),
            "_ => Default::default()".into(),
          ]);
        })?;
        SherpaResult::Ok(())
      },
    );

    w.method(
      "pub fn to_string",
      "(",
      ")",
      ",",
      &|_| vec!["&self".into()],
      "-> String",
      "{",
      "}",
      &mut |w| {
        w.block(Some("match self".into()), "{", "}", &|w| {
          w.list(",", vec![
            format!("{}::BOOL(val) => val.to_string()", w.store.ast_type_name),
            format!("{}::STRING(string) => string.to_owned()", w.store.ast_type_name),
            format!("{}::TOKEN(val) => val.to_string()", w.store.ast_type_name),
            "_ => self.to_token().to_string()".into(),
          ]);
        })?;
        SherpaResult::Ok(())
      },
    );

    w.method(
      "pub fn to_token",
      "(",
      ")",
      ",",
      &|_| vec!["&self".into()],
      "-> Token",
      "{",
      "}",
      &mut |w| {
        w.block(Some("match self".into()), "{", "}", &|w| {
          w.write_struct_data(&|w, s| {
            if s.tokenized {
              w.stmt(format!("{}::{}(node) => node.tok.clone(),", w.store.ast_type_name, s.name));
            }
          });
          w.list(",", vec![
            format!("{}::TOKEN(val) => val.to_owned()", w.store.ast_type_name),
            "_ => Token::empty()".into(),
          ]);
        })?;
        SherpaResult::Ok(())
      },
    );
  })?;

  // Write Struct Types
  w.write_struct_data(&|w, s| {
    // Struct declaration
    w.block(Some(format!("#[derive(Debug, Clone)]\npub struct {}", s.name)), "{", "}", &|w| {
      let prop_declarations = s.props.iter().map(|p| match (p.optional, p.type_.into()) {
        (true, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..)) => {
          format!("pub {}: Option<{}>", p.name, p.type_string)
        }
        (true, _) => format!("pub {}: {}", p.name, p.type_string),
        _ => format!("pub {}: {}", p.name, p.type_string),
      });
      w.list(",", prop_declarations.collect());
    });

    // Struct implementation
    w.block(Some(format!("impl {}", s.name)), "{", "}", &|w| {
      w.method(
        "pub fn new",
        "(",
        ")",
        ",",
        &|_| {
          s.props
            .iter()
            .map(|p| match (p.optional, p.type_.into()) {
              (true, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..)) => {
                format!("{}: Option<{}>", p.name, p.type_string)
              }
              (true, _) => format!("{}: {}", p.name, p.type_string),
              _ => format!("{}: {}", p.name, p.type_string),
            })
            .collect()
        },
        "-> Self",
        "{",
        "}",
        &mut |w| {
          w.block(Some("Self".into()), "{", "}", &|w| {
            w.list(",", s.props.iter().map(|p| p.name.clone()).collect());
          })?;
          SherpaResult::Ok(())
        },
      );

      w.method(
        "pub fn get_type",
        "(",
        ")",
        ",",
        &|_| vec!["self".to_string()],
        &format!("-> {0}Type", w.store.ast_type_name),
        "{",
        "}",
        &mut |w| {
          w.stmt(format!("{0}Type::{1}", w.store.ast_type_name, s.name));
          SherpaResult::Ok(())
        },
      );
    });

    // Struct type implementation
    w.block(Some(format!("impl {}", store.ast_type_name)), "{", "}", &|w| {
      w.method(
        &format!("pub fn as_{}", s.name),
        "(",
        ")",
        ",",
        &|_| vec!["&self".into()],
        &format!("-> Option<&{}>", s.name),
        "{",
        "}",
        &mut |w| {
          w.block(Some("match self".into()), "{", "}", &mut |w| {
            w.stmt(format!("Self::{0}(val) => Some(val.as_ref()),", s.name));
            w.stmt(format!("_ => None"));
          });
          SherpaResult::Ok(())
        },
      );
      w.method(
        &format!("pub fn as_{}_mut", s.name),
        "(",
        ")",
        ",",
        &|_| vec!["&mut self".into()],
        &format!("-> Option<&mut {}>", s.name),
        "{",
        "}",
        &mut |w| {
          w.block(Some("match self".into()), "{", "}", &mut |w| {
            w.stmt(format!("Self::{0}(val) => Some(val.as_mut()),", s.name));
            w.stmt(format!("_ => None"));
          });
          SherpaResult::Ok(())
        },
      );
    });
  });

  // Write Reduce Functions
  w.write_reduce_functions(
    "fn %% <R: Reader + UTF8Reader, M>",
    "(",
    ")",
    ",",
    &|w| {
      vec![
        "_ctx_: &ParseContext<R, M>".to_string(),
        format!("slots: &AstStackSlice<AstSlot<{}>>", w.store.ast_type_name),
      ]
    },
    "",
    "{",
    "}",
    &|w, reduce_functions_map| {
      w.block(Some("struct ReduceFunctions<R: Reader + UTF8Reader, M>".into()), "(", ");", &|w| {
        w.stmt(format!(
          "pub [Reducer<R, M, {0}>; {1}]",
          w.utils.store.ast_type_name,
          reduce_functions_map.len()
        ));
      })?;
      w.block(
        Some("impl<R: Reader + UTF8Reader, M> ReduceFunctions<R, M>".into()),
        "{",
        "}",
        &|w| {
          w.method("pub const fn new", "(", ")", ",", &|_| vec![], "-> Self", "{", "}", &mut |w| {
            w.block(Some("Self".into()), "([", "])", &|w| {
              w.list(",", reduce_functions_map.iter().map(|f| format!("{f}::<R, M>")).collect());
            });
            SherpaResult::Ok(())
          });
        },
      )?;
      SherpaResult::Ok(())
    },
  );

  SherpaResult::Ok(w.into_writer())
}

fn render_body_symbol(
  utils: &AscriptWriterUtils,
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
          Ref::token(utils, i, type_slot)
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
        Ref::token(utils, i, type_slot)
      }
    }
    _ => Ref::token(utils, i, type_slot),
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
  utils: &AscriptWriterUtils,
  init: &Option<Box<Init>>,
  rule: &Rule,
  ref_index: &mut usize,
  type_slot: usize,
) -> Option<Ref> {
  let rust_type = T::prim_type_name();
  let tok_conversion_fn = T::to_fn_name();
  let range_conversion_fn = T::from_tok_range_name();

  match init {
    None => None,
    Some(init) => match &init.expression {
      ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => Some(Ref::new(
        utils.bump_ref_index(ref_index),
        type_slot,
        format!("{}{}", T::string_from_f64(*value), rust_type,),
        T::from_f64(*value),
      )),
      expr => {
        let ref_ = utils.ast_expr_to_ref(expr, rule, ref_index, type_slot)?;

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
          AScriptTypeVal::TokenRange => Some(
            ref_.from(format!("%%.{}(_ctx_.get_str())", range_conversion_fn), T::from_f64(0.0)),
          ),
          _ => Some(ref_.from(format!("%%.{}()", tok_conversion_fn), T::from_f64(0.0))),
        }
      }
    },
  }
}
