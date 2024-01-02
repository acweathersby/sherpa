use radlr_ascript::{
  compile::{
    get_indexed_body_ref,
    get_named_body_ref,
    get_nonterm_types,
    get_specified_vector_from_generic_vec_values,
    get_struct_type_from_node,
    nonterminal_types_are_structs,
  },
  output_base::{
    get_ascript_export_data,
    ASTExprHandler,
    AscriptPropHandler,
    AscriptTypeHandler,
    AscriptWriter,
    AscriptWriterUtils,
    StructProp,
    TokenCreationType,
  },
  slot_ref::{SlotIndex, SlotRef},
  types::{
    AScriptNumericType,
    AScriptStore,
    AScriptStruct,
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
    StructuredFloat,
    TaggedType,
  },
};
use radlr_core::{
  parser::{ASTNode, ASTNodeType, Init, *},
  Rule,
  *,
};
use radlr_rust_runtime::deprecate::*;
use std::{
  collections::{BTreeMap, BTreeSet, VecDeque},
  io::Write,
};

pub(crate) fn write_rust_ast2<W: Write>(mut w: AscriptWriter<W>) -> RadlrResult<AscriptWriter<W>> {
  // --------------------------------------------------------------------------
  // Struct Types
  w.write_struct_data(&|w, s| {
    let struct_name = s.name.clone();
    let ast_type_name = w.store.ast_type_name.clone();

    // Struct declaration
    w.block(&("#[derive(Clone, Debug)]\npub struct ".to_string() + &s.name), "{", "}", &|w| {
      let prop_declarations =
        s.props.iter().map(|StructProp { name, type_string, .. }| "pub ".to_string() + name + ":" + type_string);
      w.list(
        ", ",
        prop_declarations.chain(vec![s.tokenized].into_iter().filter_map(|v| v.then_some("pub tok: Token".into()))).collect(),
      )?;
      RadlrResult::Ok(())
    })?;

    // Struct implementation
    w.block(&("impl ".to_string() + &s.name), "{", "}", &|w| {
      w.method(
        "pub fn new",
        "(",
        ")",
        ", ",
        &|_| {
          s.props
            .iter()
            .map(|p| match (p.optional, p.type_.into()) {
              (true, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..)) => p.name.clone() + ": " + &p.type_string,
              (true, _) => format!("{}: {}", p.name, p.type_string),
              _ => p.name.clone() + ": " + &p.type_string,
            })
            .chain(vec![s.tokenized].into_iter().filter_map(|v| v.then_some("tok: Token".into())))
            .collect()
        },
        "-> Self",
        "{",
        "}",
        &mut |w| {
          w.block("Self", "{", "}", &|w| {
            w.list(
              ",",
              s.props
                .iter()
                .map(|p| p.name.clone())
                .chain(vec![s.tokenized].into_iter().filter_map(|v| v.then_some("tok".into())))
                .collect(),
            )?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;

      w.method(
        "pub fn get_type",
        "(",
        ")",
        ",",
        &|_| vec!["&self".to_string()],
        &format!("-> {ast_type_name}Type"),
        "{",
        "}",
        &mut |w| {
          w.stmt(format!("{ast_type_name}Type::{struct_name}"))?;
          RadlrResult::Ok(())
        },
      )?;
      RadlrResult::Ok(())
    })?;

    // Struct type implementation
    w.block(&format!("impl {ast_type_name}"), "{", "}", &|w| {
      w.method(
        &format!("pub fn to_{struct_name}"),
        "(",
        ")",
        ", ",
        &|_| vec!["self".into()],
        &format!("-> Box::<{struct_name}>"),
        "{",
        "}",
        &mut |w| {
          w.block("match self", "{", "}", &mut |w| {
            w.stmt(format!("Self::{0}(val) => val,", s.name))?;
            w.stmt(format!("_ => panic!()"))?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;
      w.method(
        &format!("pub fn as_{struct_name}"),
        "(",
        ")",
        ", ",
        &|_| vec!["&self".into()],
        &format!("-> Option<&{struct_name}>"),
        "{",
        "}",
        &mut |w| {
          w.block("match self", "{", "}", &mut |w| {
            w.stmt(format!("Self::{0}(val) => Some(val.as_ref()),", s.name))?;
            w.stmt(format!("_ => None"))?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;
      w.method(
        &format!("pub fn as_{struct_name}_mut"),
        "(",
        ")",
        ",",
        &|_| vec!["&mut self".into()],
        &format!("-> Option<&mut {struct_name}>"),
        "{",
        "}",
        &mut |w| {
          w.block("match self", "{", "}", &mut |w| {
            w.stmt(format!("Self::{struct_name}(val) => Some(val.as_mut()),"))?;
            w.stmt(format!("_ => None"))?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;
      RadlrResult::Ok(())
    })?;

    // Struct hash implementation
    w.block(&format!("impl Hash for {struct_name}"), "{", "}", &|w| {
      w.method(
        &format!("fn hash<H: std::hash::Hasher>"),
        "(",
        ")",
        ",",
        &|_| vec!["&self".into(), "hasher: &mut H".into()],
        "",
        "{",
        "}",
        &mut |w| {
          w.stmt("self.get_type().hash(hasher);".into())?;
          for StructProp { name, type_, .. } in &s.props {
            use AScriptTypeVal::*;
            match (*type_).into() {
              F64(..) | F32(..) => {
                w.stmt(format!("self.{name}.to_le_bytes().hash(hasher);"))?;
              }
              F64Vec | F32Vec => {
                w.block(&format!("for val in &self.{name}"), "{", "}", &|w| {
                  w.stmt(format!("val.to_le_bytes().hash(hasher);"))?;
                  RadlrResult::Ok(())
                })?;
              }
              GenericStruct(..) | Struct(..) | Bool(..) | StringVec | U8Vec | I8Vec | String(..) | U64Vec | I64Vec | U32Vec
              | I32Vec | U16Vec | I16Vec | U64(..) | I64(..) | U32(..) | I32(..) | U16(..) | I16(..) | U8(..) | I8(..) => {
                w.stmt(format!("self.{name}.hash(hasher);"))?;
              }
              GenericStructVec(..) => {
                w.block(&format!("for val in &self.{name}"), "{", "}", &|w| {
                  w.stmt(format!("val.hash(hasher);"))?;
                  RadlrResult::Ok(())
                })?;
              }
              TokenVec => {
                w.block(&format!("for val in &self.{name}"), "{", "}", &|w| {
                  w.stmt(format!("val.to_string().replace(\" \", \"\").replace(\"\\n\", \"\").hash(hasher);"))?;
                  RadlrResult::Ok(())
                })?;
              }
              Token => {
                w.stmt(format!("self.{name}.to_string().replace(\" \", \"\").replace(\"\\n\", \"\").hash(hasher);"))?;
              }
              Undefined => { /* Ignore undefined properties */ }
              _ => unreachable!("Did not expect node in this context when creating struct definition: {struct_name}"),
            }
          }
          RadlrResult::Ok(())
        },
      )?;
      RadlrResult::Ok(())
    })?;

    RadlrResult::Ok(())
  })?;

  // --------------------------------------------------------------------------
  // Reduce Functions
  w.write_reduce_functions(
    "fn %% <R: Reader + UTF8Reader, M, const UP: bool>",
    "(",
    ")",
    ",",
    &|w| {
      vec!["_ctx_: *mut ParseContext<R, M>".to_string(), format!("slots: &AstStackSlice<AstSlot<{}>, UP>", w.store.ast_type_name)]
    },
    "",
    "{",
    "}",
    &|w, reduce_functions_map| {
      w.block("struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>", "(", ");", &|w| {
        w.stmt(format!("pub [Reducer<R, M, {0}, UP>; {1}]", w.utils.store.ast_type_name, reduce_functions_map.len()))?;
        RadlrResult::Ok(())
      })?;
      w.block("impl<R: Reader + UTF8Reader, M, const UP: bool> ReduceFunctions<R, M, UP>", "{", "}", &|w| {
        w.method("pub const fn new", "(", ")", ",", &|_| vec![], "-> Self", "{", "}", &mut |w| {
          w.block("Self", "([", "])", &|w| {
            w.list(",", reduce_functions_map.iter().map(|f| format!("{f}::<R, M, UP>")).collect())?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        })?;
        RadlrResult::Ok(())
      })?;
      RadlrResult::Ok(())
    },
  )?;

  RadlrResult::Ok(w)
}

pub(crate) fn write_rust_ast<W: Write>(mut w: AscriptWriter<W>) -> RadlrResult<AscriptWriter<W>> {
  let node_type = &w.store.ast_type_name;
  // --------------------------------------------------------------------------
  // Macros
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

  // --------------------------------------------------------------------------
  // ASTNode Enum
  w.block(&format!("#[derive(Clone, Debug)]\n#[repr(C, u32)]\npub enum {}", node_type), "{", "}", &|w| {
    let structs: Vec<_> = w.store.structs.values().map(|s| format!("{0}(Box<{0}>)", s.type_name)).collect();
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
    )
  })?;

  // --------------------------------------------------------------------------
  // ASTNodeType Enum
  w.block(&format!("#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]\npub enum {node_type}Type"), "{", "}", &|w| {
    w.list(",", DEFAULT_AST_TYPE_NAMES.to_vec())?;
    // Write Struct Types
    w.write_struct_data(&|w, s| w.stmt(format!("{},", s.name)))
  })?;

  // --------------------------------------------------------------------------
  // AstType Implementation
  w.block(&format!("impl {node_type}"), "{", "}", &|w| {
    w.stmt(format!(
      "
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
to_numeric!(to_f64, f64);",
      w.store.ast_type_name
    ))?;
    w.method("pub fn is_numeric", "(", ")", ",", &|_| vec!["&self".into()], "-> bool", "{", "}", &mut |w| {
      w.stmt(format!("use {node_type}::*;").into())?;
      w.stmt("matches!(self, F64(_) | F32(_)| I64(_)| I32(_)| I16(_)| I8(_)| U64(_)| U32(_)| U16(_)| U8(_))".into())
    })?;

    w.method("pub fn to_bool", "(", ")", ",", &|_| vec!["&self".into()], "-> bool", "{", "}", &mut |w| {
      w.stmt("self.to_u8() != 0".into())
    })?;

    w.method("pub fn into_strings", "(", ")", ",", &|_| vec!["self".into()], "-> Vec<String>", "{", "}", &mut |w| {
      w.block("match self", "{", "}", &|w| {
        w.list(",", vec![format!("{}::STRINGS(strings) => strings", w.store.ast_type_name), "_ => Default::default()".into()])
      })
    })?;

    w.method("pub fn to_string", "(", ")", ",", &|_| vec!["&self".into()], "-> String", "{", "}", &mut |w| {
      w.block("match self", "{", "}", &|w| {
        w.list(",", vec![
          format!("{}::BOOL(val) => val.to_string()", w.store.ast_type_name),
          format!("{}::STRING(string) => string.to_owned()", w.store.ast_type_name),
          format!("{}::TOKEN(val) => val.to_string()", w.store.ast_type_name),
          "_ => self.to_token().to_string()".into(),
        ])
      })
    })?;

    w.method("pub fn to_token", "(", ")", ",", &|_| vec!["&self".into()], "-> Token", "{", "}", &mut |w| {
      w.block("match self", "{", "}", &|w| {
        w.write_struct_data(&|w, s| {
          if s.tokenized {
            w.stmt(format!("{}::{}(node) => node.tok.clone(),", w.store.ast_type_name, s.name))?;
          }
          RadlrResult::Ok(())
        })?;
        w.list(",", vec![format!("{}::TOKEN(val) => val.to_owned()", w.store.ast_type_name), "_ => Token::empty()".into()])
      })
    })
  })?;

  // --------------------------------------------------------------------------
  // Get NodeType trait
  w.block(&format!("pub trait Get{node_type}Type"), "{", "}", &|w| w.stmt(format!("fn get_type(&self) -> {node_type}Type;")))?;

  // --------------------------------------------------------------------------
  // Get NodeType trait implementation
  w.block(&format!("impl Get{node_type}Type for {node_type}"), "{", "}", &|w| {
    let extended_type = node_type.clone() + "Type";
    w.method("fn get_type", "(", ")", ",", &|_| vec!["&self".into()], &format!("-> {extended_type}"), "{", "}", &mut |w| {
      w.block("match self", "{", "}", &|w| {
        w.write_struct_data(&|w, s| w.stmt(format!("{node_type}::{0}(..) => {extended_type}::{0},", s.name)))?;
        w.stmt(format!("_ => {extended_type}::NONE,"))
      })
    })
  })?;

  // --------------------------------------------------------------------------
  // Default trait implementation
  w.block(&format!("impl Default for {node_type}"), "{", "}", &|w| {
    w.method("fn default", "(", ")", ",", &|_| vec![], "-> Self", "{", "}", &mut |w| {
      w.stmt(format!("{}::NONE", w.store.ast_type_name))
    })
  })?;

  // --------------------------------------------------------------------------
  // Hash Trait implementation
  w.block(&format!("impl Hash for {node_type}"), "{", "}", &|w| {
    w.method(
      "fn hash<H: std::hash::Hasher>",
      "(",
      ")",
      ",",
      &|_| vec!["&self".into(), "hasher: &mut H".into()],
      "",
      "{",
      "}",
      &mut |w| {
        w.stmt("use ASTNode::*;".into())?;
        w.block("match self", "{", "}", &|w| {
          let structs = w.store.structs.values().map(|s| format!("{}(node) => node.hash(hasher)", s.type_name));
          w.list(
            ",",
            vec![
              "NONE => {}",
              "F32(val) => val.to_le_bytes().hash(hasher)",
              "F64(val) => val.to_le_bytes().hash(hasher)",
              "U8(val) => val.hash(hasher)",
              "U16(val) => val.hash(hasher)",
              "U32(val) => val.hash(hasher)",
              "U64(val) => val.hash(hasher)",
              "I8(val) => val.hash(hasher)",
              "I32(val) => val.hash(hasher)",
              "I16(val) => val.hash(hasher)",
              "I64(val) => val.hash(hasher)",
              "BOOL(val) => val.hash(hasher)",
              "I8Vec(val) => val.hash(hasher)",
              "I16Vec(val) => val.hash(hasher)",
              "I32Vec(val) => val.hash(hasher)",
              "I64Vec(val) => val.hash(hasher)",
              "U8Vec(val) => val.hash(hasher)",
              "U16Vec(val) => val.hash(hasher)",
              "U32Vec(val) => val.hash(hasher)",
              "U64Vec(val) => val.hash(hasher)",
              "STRING(string) => string.hash(hasher)",
              "STRINGS(strings) => strings.hash(hasher)",
            ]
            .into_iter()
            .map(|s| s.into())
            .chain(structs)
            .collect::<Vec<_>>(),
          )?;
          w.block("TOKEN(tk) =>", "{", "}", &|w| {
            w.stmt("tk.to_string().replace(\" \", \"\").replace(\"\\n\", \"\").hash(hasher);".into())
          })?;
          w.block("TOKENS(tks) =>", "{", "}", &|w| {
            w.block("for tk in tks", "{", "}", &|w| {
              w.stmt("tk.to_string().replace(\" \", \"\").replace(\"\\n\", \"\").hash(hasher);".into())
            })
          })?;
          w.block("NODES(nodes) =>", "{", "}", &|w| {
            w.block("for node in nodes", "{", "}", &|w| w.stmt("node.hash(hasher);".into()))
          })?;
          w.block("F32Vec(vals) =>", "{", "}", &|w| {
            w.block("for v in vals", "{", "}", &|w| w.stmt("v.to_le_bytes().hash(hasher);".into()))
          })?;
          w.block("F64Vec(vals) =>", "{", "}", &|w| {
            w.block("for v in vals", "{", "}", &|w| w.stmt("v.to_le_bytes().hash(hasher);".into()))
          })
        })
      },
    )
  })?;

  // --------------------------------------------------------------------------
  // Struct Types
  w.write_struct_data(&|w, s| {
    let struct_name = s.name.clone();
    let ast_type_name = w.store.ast_type_name.clone();

    // Struct declaration
    w.block(&("#[derive(Clone, Debug)]\npub struct ".to_string() + &s.name), "{", "}", &|w| {
      let prop_declarations =
        s.props.iter().map(|StructProp { name, type_string, .. }| "pub ".to_string() + name + ":" + type_string);
      w.list(
        ", ",
        prop_declarations.chain(vec![s.tokenized].into_iter().filter_map(|v| v.then_some("pub tok: Token".into()))).collect(),
      )?;
      RadlrResult::Ok(())
    })?;

    // Struct implementation
    w.block(&("impl ".to_string() + &s.name), "{", "}", &|w| {
      w.method(
        "pub fn new",
        "(",
        ")",
        ", ",
        &|_| {
          s.props
            .iter()
            .map(|p| match (p.optional, p.type_.into()) {
              (true, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..)) => p.name.clone() + ": " + &p.type_string,
              (true, _) => format!("{}: {}", p.name, p.type_string),
              _ => p.name.clone() + ": " + &p.type_string,
            })
            .chain(vec![s.tokenized].into_iter().filter_map(|v| v.then_some("tok: Token".into())))
            .collect()
        },
        "-> Self",
        "{",
        "}",
        &mut |w| {
          w.block("Self", "{", "}", &|w| {
            w.list(
              ",",
              s.props
                .iter()
                .map(|p| p.name.clone())
                .chain(vec![s.tokenized].into_iter().filter_map(|v| v.then_some("tok".into())))
                .collect(),
            )?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;

      w.method(
        "pub fn get_type",
        "(",
        ")",
        ",",
        &|_| vec!["&self".to_string()],
        &format!("-> {ast_type_name}Type"),
        "{",
        "}",
        &mut |w| {
          w.stmt(format!("{ast_type_name}Type::{struct_name}"))?;
          RadlrResult::Ok(())
        },
      )?;
      RadlrResult::Ok(())
    })?;

    // Struct type implementation
    w.block(&format!("impl {ast_type_name}"), "{", "}", &|w| {
      w.method(
        &format!("pub fn to_{struct_name}"),
        "(",
        ")",
        ", ",
        &|_| vec!["self".into()],
        &format!("-> Box::<{struct_name}>"),
        "{",
        "}",
        &mut |w| {
          w.block("match self", "{", "}", &mut |w| {
            w.stmt(format!("Self::{0}(val) => val,", s.name))?;
            w.stmt(format!("_ => panic!()"))?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;
      w.method(
        &format!("pub fn as_{struct_name}"),
        "(",
        ")",
        ", ",
        &|_| vec!["&self".into()],
        &format!("-> Option<&{struct_name}>"),
        "{",
        "}",
        &mut |w| {
          w.block("match self", "{", "}", &mut |w| {
            w.stmt(format!("Self::{0}(val) => Some(val.as_ref()),", s.name))?;
            w.stmt(format!("_ => None"))?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;
      w.method(
        &format!("pub fn as_{struct_name}_mut"),
        "(",
        ")",
        ",",
        &|_| vec!["&mut self".into()],
        &format!("-> Option<&mut {struct_name}>"),
        "{",
        "}",
        &mut |w| {
          w.block("match self", "{", "}", &mut |w| {
            w.stmt(format!("Self::{struct_name}(val) => Some(val.as_mut()),"))?;
            w.stmt(format!("_ => None"))?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        },
      )?;
      RadlrResult::Ok(())
    })?;

    // Struct hash implementation
    w.block(&format!("impl Hash for {struct_name}"), "{", "}", &|w| {
      w.method(
        &format!("fn hash<H: std::hash::Hasher>"),
        "(",
        ")",
        ",",
        &|_| vec!["&self".into(), "hasher: &mut H".into()],
        "",
        "{",
        "}",
        &mut |w| {
          w.stmt("self.get_type().hash(hasher);".into())?;
          for StructProp { name, type_, .. } in &s.props {
            use AScriptTypeVal::*;
            match (*type_).into() {
              F64(..) | F32(..) => {
                w.stmt(format!("self.{name}.to_le_bytes().hash(hasher);"))?;
              }
              F64Vec | F32Vec => {
                w.block(&format!("for val in &self.{name}"), "{", "}", &|w| {
                  w.stmt(format!("val.to_le_bytes().hash(hasher);"))?;
                  RadlrResult::Ok(())
                })?;
              }
              GenericStruct(..) | Struct(..) | Bool(..) | StringVec | U8Vec | I8Vec | String(..) | U64Vec | I64Vec | U32Vec
              | I32Vec | U16Vec | I16Vec | U64(..) | I64(..) | U32(..) | I32(..) | U16(..) | I16(..) | U8(..) | I8(..) => {
                w.stmt(format!("self.{name}.hash(hasher);"))?;
              }
              GenericStructVec(..) => {
                w.block(&format!("for val in &self.{name}"), "{", "}", &|w| {
                  w.stmt(format!("val.hash(hasher);"))?;
                  RadlrResult::Ok(())
                })?;
              }
              TokenVec => {
                w.block(&format!("for val in &self.{name}"), "{", "}", &|w| {
                  w.stmt(format!("val.to_string().replace(\" \", \"\").replace(\"\\n\", \"\").hash(hasher);"))?;
                  RadlrResult::Ok(())
                })?;
              }
              Token => {
                w.stmt(format!("self.{name}.to_string().replace(\" \", \"\").replace(\"\\n\", \"\").hash(hasher);"))?;
              }
              Undefined => { /* Ignore undefined properties */ }
              _ => unreachable!("Did not expect node in this context when creating struct definition: {struct_name}"),
            }
          }
          RadlrResult::Ok(())
        },
      )?;
      RadlrResult::Ok(())
    })?;

    RadlrResult::Ok(())
  })?;

  // --------------------------------------------------------------------------
  // Reduce Functions
  w.write_reduce_functions(
    "fn %% <R: Reader + UTF8Reader, M, const UP: bool>",
    "(",
    ")",
    ",",
    &|w| {
      vec!["_ctx_: *mut ParseContext<R, M>".to_string(), format!("slots: &AstStackSlice<AstSlot<{}>, UP>", w.store.ast_type_name)]
    },
    "",
    "{",
    "}",
    &|w, reduce_functions_map| {
      w.block("struct ReduceFunctions<R: Reader + UTF8Reader, M, const UP: bool>", "(", ");", &|w| {
        w.stmt(format!("pub [Reducer<R, M, {0}, UP>; {1}]", w.utils.store.ast_type_name, reduce_functions_map.len()))?;
        RadlrResult::Ok(())
      })?;
      w.block("impl<R: Reader + UTF8Reader, M, const UP: bool> ReduceFunctions<R, M, UP>", "{", "}", &|w| {
        w.method("pub const fn new", "(", ")", ",", &|_| vec![], "-> Self", "{", "}", &mut |w| {
          w.block("Self", "([", "])", &|w| {
            w.list(",", reduce_functions_map.iter().map(|f| format!("{f}::<R, M, UP>")).collect())?;
            RadlrResult::Ok(())
          })?;
          RadlrResult::Ok(())
        })?;
        RadlrResult::Ok(())
      })?;
      RadlrResult::Ok(())
    },
  )?;

  RadlrResult::Ok(w)
}

pub(crate) fn create_rust_writer_utils<'a>(store: &'a AScriptStore, db: &'a ParserDatabase) -> AscriptWriterUtils<'a> {
  let mut u = AscriptWriterUtils {
    db,
    store,
    // General Assignment
    assignment_writer: &|_, _, name, value, mutable| {
      if mutable {
        format!("let mut {name} = {value};")
      } else {
        format!("let {name} = {value};")
      }
    },
    // Slot Assignment
    slot_assign: &|utils, type_, ref_, local_var: bool| match type_ {
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
        type_.hcobj_type_name(),
        &ref_,
        (utils.get_token_name)(SlotIndex::Rule)
      ),
      AScriptTypeVal::Struct(struct_id) => {
        if local_var {
          let struct_name = utils.store.structs.get(struct_id).unwrap();
          format!(
            "slots.assign(0, AstSlot({}::{}(Box::new({})), {}, TokenRange::default()));",
            utils.store.ast_type_name,
            struct_name.type_name,
            ref_,
            (utils.get_token_name)(SlotIndex::Rule)
          )
        } else {
          format!("slots.assign(0, AstSlot({}, {}, TokenRange::default()));", ref_, (utils.get_token_name)(SlotIndex::Rule))
        }
      }
      AScriptTypeVal::Any => {
        format!("slots.assign(0, AstSlot({}, {}, TokenRange::default()));", &ref_, (utils.get_token_name)(SlotIndex::Rule))
      }
      _type => {
        #[cfg(debug_assertions)]
        {
          dbg!(_type, ref_, &utils.store.ast_type_name);
        }
        "INVALID_ASSIGNMENT".to_string()
      }
    },
    token_concat: &|first, last| format!("{first} + {last}"),

    slot_extract: &|token, node, index| match (node, token) {
      (Some(n), Some(t)) => {
        format!("let AstSlot ({n}, {t}, _) = slots.take({});", index)
      }
      (None, Some(t)) => {
        format!("let AstSlot (_, {t}, _) = slots.take({});", index)
      }
      (Some(n), None) => {
        format!("let AstSlot ({n}, _, _) = slots.take({});", index)
      }
      _ => format!("slots.take({});", index),
    },
    // Token Creation from Token Range
    create_token: &|tok_name, token_type| match token_type {
      TokenCreationType::String => {
        format!("{tok_name}.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut()).to_string()")
      }
      TokenCreationType::Token => {
        format!("{tok_name}.to_token(unsafe{{&mut*_ctx_}}.get_reader_mut())")
      }
    },

    get_token_name: &|i: SlotIndex| match i {
      SlotIndex::Rule => "__rule_rng__".into(),
      SlotIndex::Sym(i) | SlotIndex::Constructed(i) => "__tok_rng_".to_string() + &i.to_string(),
    },
    get_slot_obj_name: &|i| match i {
      SlotIndex::Rule => unreachable!(),
      SlotIndex::Sym(i) => format!("ref_{i}"),
      SlotIndex::Constructed(i) => format!("var_{i}"),
    },
    struct_construction: &|u, w, node_name, prop_assignments, tokenized| {
      w.write(&format!("{node_name}::new("))?;
      let mut entries: Vec<String> = prop_assignments.iter().map(|(_, val, _)| val.to_string()).collect();
      if tokenized {
        entries.push((u.get_token_name)(SlotIndex::Rule) + ".to_token(unsafe{{&mut*_ctx_}}.get_reader_mut())")
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

      RadlrResult::Ok(())
    },
    handlers: Default::default(),
  };

  u.add_type_handler(AScriptTypeVal::String(None), AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "String".into(),
  });

  u.add_type_handler(AScriptTypeVal::StringVec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<String>".into(),
  });

  u.add_type_handler(AScriptTypeVal::F64Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<f64>".into(),
  });

  u.add_type_handler(AScriptTypeVal::F32Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<f32>".into(),
  });

  u.add_type_handler(AScriptTypeVal::U8Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<u8>".into(),
  });

  u.add_type_handler(AScriptTypeVal::U16Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<u16>".into(),
  });

  u.add_type_handler(AScriptTypeVal::U32Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<u32>".into(),
  });

  u.add_type_handler(AScriptTypeVal::U64Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<u64>".into(),
  });

  u.add_type_handler(AScriptTypeVal::I8Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<i8>".into(),
  });

  u.add_type_handler(AScriptTypeVal::I16Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<i16>".into(),
  });

  u.add_type_handler(AScriptTypeVal::I32Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<i32>".into(),
  });

  u.add_type_handler(AScriptTypeVal::I64Vec, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Vec<i64>".into(),
  });

  u.add_type_handler(AScriptTypeVal::TokenVec, AscriptTypeHandler {
    default: &|_, _, _| "Vec::new()".into(),
    name:    &|_, _, _| "Vec<Token>".into(),
  });

  u.add_type_handler(AScriptTypeVal::Token, AscriptTypeHandler {
    default: &|_, _, _| "Default::default()".into(),
    name:    &|_, _, _| "Token".into(),
  });

  u.add_type_handler(AScriptTypeVal::Bool(None), AscriptTypeHandler {
    default: &|_, _, _| "false".into(),
    name:    &|_, _, _| "bool".into(),
  });

  u.add_type_handler(AScriptTypeVal::I16(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i16".into(),
  });
  u.add_type_handler(AScriptTypeVal::I8(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i8".into(),
  });
  u.add_type_handler(AScriptTypeVal::I32(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i32".into(),
  });
  u.add_type_handler(AScriptTypeVal::I64(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "i64".into(),
  });
  u.add_type_handler(AScriptTypeVal::U8(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u8".into(),
  });
  u.add_type_handler(AScriptTypeVal::U16(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u16".into(),
  });
  u.add_type_handler(AScriptTypeVal::U32(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u32".into(),
  });
  u.add_type_handler(AScriptTypeVal::U64(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "u64".into(),
  });
  u.add_type_handler(AScriptTypeVal::F32(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "f32".into(),
  });
  u.add_type_handler(AScriptTypeVal::F64(None), AscriptTypeHandler {
    default: &|_, _, _| "0".into(),
    name:    &|_, _, _| "f64".into(),
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

  u.add_ast_handler(ASTNodeType::AST_Bool, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_Bool(box AST_Bool { initializer, .. }) => match initializer {
        None => Some(SlotRef::ast_obj(
          SlotIndex::Sym(u.bump_ref_index(ref_index)),
          type_slot,
          format!("{}", false),
          AScriptTypeVal::Bool(Some(false)),
        )),
        Some(box init) => match u.ast_expr_to_ref(&init.expression, r, ref_index, type_slot) {
          Some(_) => Some(SlotRef::ast_obj(
            SlotIndex::Sym(u.bump_ref_index(ref_index)),
            type_slot,
            "true".to_string(),
            AScriptTypeVal::Bool(Some(true)),
          )),
          None => Some(SlotRef::ast_obj(
            SlotIndex::Sym(u.bump_ref_index(ref_index)),
            type_slot,
            "false".to_string(),
            AScriptTypeVal::Bool(Some(false)),
          )),
        },
      },
      _ => None,
    },
  });

  u.add_ast_handler(ASTNodeType::AST_Token, ASTExprHandler {
    expr: &|u, ast, _, _, type_slot| {
      if let ASTNode::AST_Token(box AST_Token { range, .. }) = ast {
        let ref_ = SlotRef::node_range(u, type_slot);
        if let Some(box parser::Range { start_trim, end_trim }) = range {
          Some(ref_.to(format!("%%.trim({start_trim}, {end_trim})"), AScriptTypeVal::AdjustedTokenRange))
        } else {
          match ref_.ast_type {
            AScriptTypeVal::TokenRange | AScriptTypeVal::AdjustedTokenRange => {
              Some(ref_.to_range(u).to("%%.to_token(unsafe{&mut *_ctx_}.get_reader_mut())".to_string(), AScriptTypeVal::Token))
            }
            AScriptTypeVal::TokenVec => {
              // Merge the last and first token together
              // get the string value from the resulting span of the union
              Some(ref_.to("(%%.first().unwrap() + %%.last().unwrap())".to_string(), AScriptTypeVal::Token))
            }
            AScriptTypeVal::Token => Some(ref_),
            _ => None,
          }
        }
      } else {
        None
      }
    },
  });
  u.add_ast_handler(ASTNodeType::AST_String, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_String(box AST_String { initializer, .. }) => {
        match initializer {
          None => Some(SlotRef::ast_obj(
            SlotIndex::Sym(u.bump_ref_index(ref_index)),
            type_slot,
            "String::new()".to_string(),
            AScriptTypeVal::String(None),
          )),
          Some(box init) => {
            match &init.expression {
              ASTNode::AST_NumberLiteral(box AST_NumberLiteral { value, .. }) => Some(SlotRef::ast_obj(
                SlotIndex::Sym(u.bump_ref_index(ref_index)),
                type_slot,
                format!("\"{}\".to_string()", value),
                AScriptTypeVal::String(Some(value.to_string())),
              )),
              expr => {
                let ref_ = u.ast_expr_to_ref(expr, r, ref_index, type_slot)?;
                match ref_.ast_type {
                  AScriptTypeVal::AdjustedTokenRange => {
                    Some(ref_.to("%%.to_slice(unsafe{&*_ctx_}.get_str()).to_string()".to_string(), AScriptTypeVal::String(None)))
                  }
                  AScriptTypeVal::Struct(..) | AScriptTypeVal::TokenRange | AScriptTypeVal::GenericStruct(..) => Some(
                    ref_
                      .to_range(u)
                      .to("%%.to_slice(unsafe{&*_ctx_}.get_str()).to_string()".to_string(), AScriptTypeVal::String(None)),
                  ),
                  AScriptTypeVal::TokenVec => {
                    // Merge the last and first token together
                    // get the string value from the resulting span of the union
                    Some(
                      ref_.to("(%%.first().unwrap() + %%.last().unwrap()).to_string()".to_string(), AScriptTypeVal::String(None)),
                    )
                  }
                  AScriptTypeVal::String(..) => Some(ref_),
                  _ => Some(ref_.to("%%.to_string()".to_string(), AScriptTypeVal::String(None))),
                }
              }
            }
          }
        }
      }
      _ => None,
    },
  });

  u.add_ast_handler(ASTNodeType::AST_I8, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I8(box AST_I8 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI8>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });

  u.add_ast_handler(ASTNodeType::AST_I16, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I16(box AST_I16 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI16>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });

  u.add_ast_handler(ASTNodeType::AST_I32, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I32(box AST_I32 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI32>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_I64, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_I64(box AST_I64 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValI64>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_U8, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U8(box AST_U8 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU8>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_U16, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U16(box AST_U16 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU16>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_U32, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U32(box AST_U32 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU32>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_U64, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_U64(box AST_U64 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValU64>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_F32, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_F32(box AST_F32 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValF32>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_F64, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_F64(box AST_F64 { initializer, .. }) => {
        convert_numeric::<AScriptTypeValF64>(u, initializer, r, ref_index, type_slot)
      }
      _ => None,
    },
  });

  u.add_ast_handler(ASTNodeType::AST_NumberLiteral, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_NumberLiteral(box AST_NumberLiteral { value }) => Some(SlotRef::ast_obj(
        SlotIndex::Sym(u.bump_ref_index(ref_index)),
        type_slot,
        value.to_string() + ".0",
        AScriptTypeVal::F64(Some(StructuredFloat(*value as f64))),
      )),
      _ => None,
    },
  });

  u.add_ast_handler(ASTNodeType::AST_Struct, ASTExprHandler {
    expr: &|u, ast, r, ref_index, type_slot| match ast {
      ASTNode::AST_Struct(ast_struct) => {
        if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(ast_struct) {
          match u.build_struct_constructor(r, &struct_type, ast_struct, ref_index, type_slot) {
            RadlrResult::Ok(_ref) => Some(_ref),
            _ => None,
          }
        } else {
          None
        }
      }
      _ => None,
    },
  });
  u.add_ast_handler(ASTNodeType::AST_Vector, ASTExprHandler {
    expr: &|utils, ast, rule, ref_index, type_slot| {
      if let ASTNode::AST_Vector(box AST_Vector { initializer, .. }) = ast {
        let mut results =
          initializer.iter().filter_map(|n| utils.ast_expr_to_ref(n, rule, ref_index, type_slot)).collect::<VecDeque<_>>();

        if results.is_empty() {
          Some(SlotRef::ast_obj(
            SlotIndex::Sym(utils.bump_ref_index(ref_index)),
            type_slot,
            "vec![];".to_string(),
            AScriptTypeVal::GenericVec(None),
          ))
        } else {
          let types = results.iter().map(|t| t.ast_type.clone()).collect::<BTreeSet<_>>();

          let mut vector_ref = if results[0].ast_type.is_vec() {
            results.pop_front().unwrap()
          } else {
            SlotRef::ast_obj(
              SlotIndex::Sym(utils.bump_ref_index(ref_index)),
              type_slot,
              "vec![]".to_string(),
              get_specified_vector_from_generic_vec_values(&types),
            )
          };

          for mut _ref in results {
            let mut _ref = _ref.ensure_ast_obj(utils);

            if _ref.ast_type.is_vec() {
              _ref.make_mutable();
              let val_ref = _ref.get_ref_name();
              vector_ref.add_post_init_stmt(format!("%%.append(&mut {});", val_ref)).make_mutable()
            } else {
              let val_ref = _ref.get_ref_name();
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
  u.add_ast_handler(ASTNodeType::AST_IndexReference, ASTExprHandler {
    expr: &|u, ast, rule, _, type_slot| {
      if let ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) = ast {
        match get_indexed_body_ref(rule, (*value - 1) as usize) {
          Some((index, SymbolRef { id, .. })) => render_body_symbol(u, &id, u.store, SlotIndex::Sym(index), type_slot),
          None => None,
        }
      } else {
        None
      }
    },
  });
  u.add_ast_handler(ASTNodeType::AST_NamedReference, ASTExprHandler {
    expr: &|u, ast, rule, _, type_slot| {
      if let ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) = ast {
        match get_named_body_ref(u.db, rule, value) {
          Some((index, SymbolRef { id, .. })) => render_body_symbol(u, &id, u.store, SlotIndex::Sym(index), type_slot),
          None => None,
        }
      } else {
        None
      }
    },
  });

  // Convert Vec<AstNode> to Vec<<Specific Node>> when the Generic Structs Vecs
  // only have one type.
  u.add_prop_handler(AScriptTypeVal::GenericStructVec(Default::default()), AscriptPropHandler {
    expr: &|u, ref_, prop_type_, _| match ref_ {
      Some(ref_) => match prop_type_ {
        AScriptTypeVal::GenericStructVec(structs_ids) if structs_ids.len() == 1 => (
          format!(
            "{}.into_iter().map(|v|match v {{ {}::{}(node) => node, _ => panic!(\"could not convert\")}}).collect::<Vec<_>>()",
            ref_.get_ref_name(),
            u.store.ast_type_name,
            u.store.structs.get(&structs_ids.first().unwrap().into()).unwrap().type_name
          ),
          Some(ref_),
        ),
        _ => (ref_.get_ref_name(), Some(ref_)),
      },
      None => ("Default::default()".into(), Default::default()),
    },
  });
  fn handle_struct_props(
    utils: &AscriptWriterUtils,
    ref_: Option<SlotRef>,
    prop_type_: &AScriptTypeVal,
    optional: bool,
  ) -> (String, Option<SlotRef>) {
    let store = utils.store;
    use AScriptTypeVal::*;
    match ref_ {
      Some(ref_) => {
        let ref_string = ref_.get_ref_name();
        let out_ref_ = if let Struct(..) = prop_type_ {
          match ref_.ast_type.clone() {
            Struct(struct_type) => {
              let struct_name = store.structs.get(&struct_type).unwrap().type_name.clone();
              ref_.to(format!("%%.to_{struct_name}()"), Struct(struct_type))
            }
            GenericStruct(struct_types) if struct_types.len() == 1 => {
              let struct_type = struct_types.first().unwrap();
              let struct_name = store.structs.get(&(struct_type.into())).unwrap().type_name.clone();
              ref_.to(format!("%%.to_{struct_name}()"), Struct(struct_type.into()))
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

  u.add_prop_handler(AScriptTypeVal::Struct(Default::default()), AscriptPropHandler { expr: &handle_struct_props });

  u.add_prop_handler(AScriptTypeVal::GenericStruct(Default::default()), AscriptPropHandler { expr: &handle_struct_props });
  u
}

fn render_body_symbol(
  utils: &AscriptWriterUtils,
  sym: &SymbolId,
  ast: &AScriptStore,
  slot_index: SlotIndex,
  type_slot: usize,
) -> Option<SlotRef> {
  use AScriptTypeVal::*;
  let ref_ = match &sym {
    SymbolId::DBNonTerminal { key: nterm } => {
      let types = get_nonterm_types(ast, nterm);
      let ref_name = (&utils.get_slot_obj_name)(slot_index);
      if types.len() == 1 {
        let _type = types.first().unwrap().clone();
        if Token == _type {
          SlotRef::token(utils, slot_index, type_slot)
        } else {
          if let Some(init_string) = match _type {
            F64(..) => Some(format!("{ref_name}.to_f64()")),
            F32(..) => Some(format!("{ref_name}.to_f32()")),
            U64(..) => Some(format!("{ref_name}.to_u64()")),
            I64(..) => Some(format!("{ref_name}.to_i64()")),
            U32(..) => Some(format!("{ref_name}.to_u32()")),
            I32(..) => Some(format!("{ref_name}.to_i32()")),
            U16(..) => Some(format!("{ref_name}.to_u16()")),
            I16(..) => Some(format!("{ref_name}.to_i16()")),
            U8(..) => Some(format!("{ref_name}.to_u8()")),
            I8(..) => Some(format!("{ref_name}.to_i8()")),
            String(..) => Some(format!("{ref_name}.to_string()")),
            GenericStructVec(..) => Some(format!("{ref_name}.into_nodes()")),
            StringVec => Some(format!("{ref_name}.into_strings()")),
            TokenVec => Some(format!("{ref_name}.into_tokens()")),
            F64Vec => Some(format!("{ref_name}.into_f64_vec()")),
            F32Vec => Some(format!("{ref_name}.into_f32_vec()")),
            U64Vec => Some(format!("{ref_name}.into_u64_vec()")),
            I64Vec => Some(format!("{ref_name}.into_i64_vec()")),
            U32Vec => Some(format!("{ref_name}.into_u32_vec()")),
            I32Vec => Some(format!("{ref_name}.into_i32_vec()")),
            U16Vec => Some(format!("{ref_name}.into_u16_vec()")),
            I16Vec => Some(format!("{ref_name}.into_i16_vec()")),
            U8Vec => Some(format!("{ref_name}.into_u8_vec()")),
            I8Vec => Some(format!("{ref_name}.into_i8_vec()")),
            GenericStruct(..) => Some(format!("{ref_name}")),
            Struct(..) => Some(format!("{ref_name}")),
            _ => None,
          } {
            SlotRef::ast_obj(slot_index, type_slot, init_string, _type)
          } else {
            SlotRef::ast_obj(slot_index, type_slot, "".to_string(), match _type.to_owned() {
              GenericVec(types) => {
                get_specified_vector_from_generic_vec_values(&types.unwrap().iter().map(|t| t.into()).collect())
              }
              _type => _type,
            })
          }
        }
      } else if nonterminal_types_are_structs(&types) {
        SlotRef::ast_obj(slot_index, type_slot, ref_name, GenericStruct(extract_struct_types(&types)))
      } else {
        SlotRef::token(utils, slot_index, type_slot)
      }
    }
    _ => SlotRef::range(utils, slot_index, type_slot),
  };

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
  u: &AscriptWriterUtils,
  init: &Option<Box<Init>>,
  rule: &Rule,
  ref_index: &mut usize,
  type_slot: usize,
) -> Option<SlotRef> {
  let rust_type = T::prim_type_name();
  let tok_conversion_fn = T::to_fn_name();
  let range_conversion_fn = T::from_tok_range_name();

  fn default_value<T: AScriptNumericType>(
    u: &AscriptWriterUtils,
    ref_index: &mut usize,
    rust_type: &str,
    type_slot: usize,
  ) -> Option<SlotRef> {
    Some(SlotRef::ast_obj(
      SlotIndex::Sym(u.bump_ref_index(ref_index)),
      type_slot,
      format!("0 as {}", rust_type,),
      T::from_f64(0.0),
    ))
  }

  match init {
    None => default_value::<T>(u, ref_index, rust_type, type_slot),
    Some(init) => match &init.expression {
      ASTNode::AST_NumberLiteral(box AST_NumberLiteral { value, .. }) => Some(SlotRef::ast_obj(
        SlotIndex::Sym(u.bump_ref_index(ref_index)),
        type_slot,
        format!("{}{}", T::string_from_f64(*value as f64), rust_type,),
        T::from_f64(*value as f64),
      )),
      expr => match u.ast_expr_to_ref(expr, rule, ref_index, type_slot) {
        Some(ref_) => match ref_.ast_type {
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
          | AScriptTypeVal::U64(..) => Some(ref_.to(format!("%% as {}", rust_type), T::from_f64(0.0))),
          AScriptTypeVal::TokenRange => {
            Some(ref_.to(format!("%%.{}(unsafe{{&*_ctx_}}.get_str())", range_conversion_fn), T::from_f64(0.0)))
          }
          _ => Some(ref_.to(format!("%%.{}()", tok_conversion_fn), T::from_f64(0.0))),
        },
        _ => default_value::<T>(u, ref_index, rust_type, type_slot),
      },
    },
  }
}

pub(crate) fn add_ascript_functions_for_rust<W: Write>(w: &mut AscriptWriter<W>, _db: &ParserDatabase) -> RadlrResult<()> {
  let export_node_data = get_ascript_export_data(&w.utils);

  // Create impl for all exported non-terminals that can be mapped to a ascript
  // single AScripT type. For those that map to multiple outputs, create an
  // impl on the main AST enum for named parsers on those types.
  for (_, ast_type, ast_type_string, export_name, ..) in &export_node_data {
    let type_name = match ast_type {
      AScriptTypeVal::Struct(id) => {
        let AScriptStruct { type_name, .. } = w.store.structs.get(id).unwrap();
        type_name.clone()
      }
      _ => continue,
    };

    w.block(&format!("impl {type_name}"), "{", "}", &|w| {
      w.stmt(format!("/// Create a [{type_name}] node from a `String` input."))?;
      w.method(
        "pub fn from_string",
        "(",
        ")",
        ", ",
        &|_| vec!["input: String".into()],
        &format!("-> Result<{ast_type_string}, RadlrParseError>"),
        "{",
        "}",
        &mut |w| {
          w.stmt("let reader = UTF8StringReader::from(&input);".into())?;
          w.stmt(format!("ast::{export_name}_from(reader)"))
        },
      )
    })?;

    w.block(&format!("impl {type_name}"), "{", "}", &|w| {
      w.stmt(format!("/// Create a [{type_name}] node from a `String` input."))?;
      w.method(
        "pub fn from_str",
        "(",
        ")",
        ", ",
        &|_| vec!["input: &str".into()],
        &format!("-> Result<{ast_type_string}, RadlrParseError>"),
        "{",
        "}",
        &mut |w| {
          w.stmt("let reader = UTF8StringReader::from(input);".into())?;
          w.stmt(format!("ast::{export_name}_from(reader)"))
        },
      )
    })?;
  }

  w.block("pub trait ASTParse<T>", "{", "}", &|w| {
    for (_, _, ast_type_string, export_name, ..) in &export_node_data {
      w.stmt(format!("fn {export_name}_from(input:T) -> Result<{ast_type_string}, RadlrParseError>;",))?;
    }
    RadlrResult::Ok(())
  })?;

  Result::Ok(())
}

pub(crate) fn write_rust_bytecode_parser_file<'a, W: Write>(
  mut w: AscriptWriter<'a, W>,
  state_lookups: &BTreeMap<String, u32>,
  bc: &Vec<u8>,
) -> RadlrResult<AscriptWriter<'a, W>> {
  let ast_type_name = w.store.ast_type_name.clone();
  let parse_nonterms = w.db.parser_nonterms();
  w.stmt(
    "    
pub trait Reader: ByteReader + MutByteReader + UTF8Reader {}

impl<T: ByteReader + MutByteReader + UTF8Reader> Reader for T {}

pub type Parser<T, UserCTX, Bytecode> = radlr_rust_runtime::deprecate::ByteCodeParser<T, UserCTX, Bytecode>;"
      .into(),
  )?;

  w.block("pub mod meta", "{", "}", &|w| {
    w.block(&format!("pub const nonterm_names: [&'static str;{}] = ", parse_nonterms.len()), "[", "];", &|w| {
      w.list(
        ",",
        parse_nonterms
          .iter()
          .map(|nonterm| (nonterm, format!("\"{}\"", w.db.nonterm_friendly_name_string(*nonterm),)))
          .collect::<BTreeMap<_, _>>()
          .into_values()
          .collect(),
      )
    })?;

    let symbol_string = w
      .db
      .tokens()
      .iter()
      .map(|tok| (tok.tok_id, format!("r####\"{}\"####", tok.sym_id.debug_string(w.db))))
      .collect::<BTreeMap<_, _>>();

    let len = symbol_string.len();

    w.block(&format!("pub const symbol_string: [&'static str;{len}] = ",), "[", "];", &move |w| {
      w.list(",", symbol_string.values().collect())
    })
  })?;
  for DBEntryPoint { nonterm_entry_name, entry_name, .. } in w.db.entry_points().into_iter().filter(|e| e.is_export) {
    let entry_name = entry_name.to_string(w.db.string_store());
    let nonterm_entry_name = nonterm_entry_name.to_string(w.db.string_store());
    //println!("{} {:#?}", nonterm_entry_name,
    // state_lookups.keys().collect::<Vec<_>>());
    let bytecode_offset = o_to_r(state_lookups.get(&nonterm_entry_name), "could not find state")?;
    w.method(
      &format!("pub fn new_{entry_name}_parser<'a, T: Reader, UserCTX>"),
      "(",
      ")",
      ", ",
      &|_| vec!["reader: &'a mut T".into()],
      "-> Parser<T, UserCTX, &'static [u8]>",
      "{",
      "}",
      &mut |w| {
        w.stmt(format!("let mut parser = Parser::<T, UserCTX, &'static [u8]>::new(reader, &bytecode);"))?;
        w.stmt(format!("parser.init_parser({bytecode_offset});"))?;
        w.stmt(format!("parser"))
      },
    )?;
  }

  // Export
  w.block(&format!("pub static bytecode: [u8; {}] = ", bc.len()), "[", "];", &|w| {
    w.list(
      ", ",
      bc.chunks(60).into_iter().map(|i| i.into_iter().map(|i| format!("{i}")).collect::<Vec<_>>().join(",")).collect(),
    )?;
    RadlrResult::Ok(())
  })?;

  if !w.store.is_dummy {
    let export_node_data = get_ascript_export_data(w.utils);

    w.block("pub mod ast", "{", "}", &|w| {
      w.stmt(format!("impl AstObject for {ast_type_name} {{}}"))?;
      w.stmt(format!("type ASTSlot = ({ast_type_name}, TokenRange, TokenRange);"))?;
      w.stmt("use super::*; ".into())?;
      w.stmt(format!("type Node = {};", w.store.ast_type_name))?;

      // Create a module that will store convenience functions for compiling AST
      // structures based on on grammar entry points.
      for (ref_, type_, ast_type_string, export_name, _guid_name, nonterm_entry_name) in &export_node_data {
        w.method(
          &format!("pub fn {export_name}_from<'a>"),
          "(",
          ")",
          ", ",
          &|_| vec!["mut reader: UTF8StringReader".into()],
          &format!("-> Result<{ast_type_string}, RadlrParseError>"),
          "{",
          "}",
          &mut |w| {
            w.stmt(format!("let reduce_functions = ReduceFunctions::<_, u32, true>::new();"))?;
            w.stmt(format!("let mut parser = Parser::new(&mut reader, &bytecode);"))?;
            w.stmt(format!("parser.init_parser({});", state_lookups.get(nonterm_entry_name).unwrap()))?;

            w.stmt(format!(
              "let AstSlot ({}, __rule_rng__, _) = parser.parse_ast(&reduce_functions.0, &mut None)?;",
              (&w.utils.get_slot_obj_name)(SlotIndex::Sym(0))
            ))?;
            if ref_.is_some() {
              let (ref_name, ref_) = w.utils.create_type_initializer_value(ref_.clone(), type_, false);
              if let Some(ref_) = ref_ {
                w.stmt(ref_.to_init_string(w.utils))?;
                w.stmt(format!("Ok({ref_name})"))
              } else {
                w.stmt("Ok(i0)".to_string())
              }
            } else {
              w.stmt("Ok(i0)".to_string())
            }
          },
        )?;
      }
      RadlrResult::Ok(())
    })?;
  }

  RadlrResult::Ok(w)
}

pub(crate) fn write_rust_llvm_parser_file<'a, W: Write>(
  mut w: AscriptWriter<'a, W>,
  grammar_name: &str,
  parser_name: &str,
) -> RadlrResult<AscriptWriter<'a, W>> {
  w.stmt(format!(
    r###"
#[link(name = "{parser_name}", kind ="static" )]
extern "C" {{
  fn init(ctx: *mut u8, reader: *mut u8);
  fn next(ctx: *mut u8) -> ParseActionType;
  fn prime(ctx: *mut u8, start_point: u32);
  fn drop(ctx: *mut u8);
}}
pub trait Reader: ByteReader + LLVMByteReader + MutByteReader {{}}
impl<T:ByteReader + LLVMByteReader + MutByteReader > Reader for T {{}}

      
pub struct Parser<T: Reader, M>(ParseContext<T, M>, T);
"###
  ))?;

  w.block("impl<T: Reader, M> Parser<T, M>", "{", "}", &|w| {
    w.stmt(format!(
      "/// Create a new parser context to parser the input with 
    /// the grammar `{grammar_name}"
    ))?;
    w.stmt("#[inline(always)]".into())?;
    w.method("fn new", "(", ")", ", ", &|_| vec!["mut reader: T".into()], "-> Self", "{", "}", &mut |w| {
      w.stmt("let mut parser = Self(ParseContext::<T, M>::new_llvm(), reader);".into())?;
      w.stmt("parser.construct_context();".into())?;
      w.stmt("parser".into())
    })?;
    w.stmt(
      "/// Initialize the parser to recognize the given starting non-terminal
      /// within the input. This method is chainable."
        .into(),
    )?;
    w.stmt("#[inline(always)]".into())?;
    w.method(
      "fn set_start_point",
      "(",
      ")",
      ", ",
      &|_| vec!["&mut self".into(), "start_point: u64".into()],
      "-> &mut Self",
      "{",
      "}",
      &mut |w| {
        w.block("unsafe", "{", "}", &|w| {
          w.stmt("let _ptr = &mut self.0 as *const ParseContext<T, M>;".into())?;
          w.stmt("prime(_ptr as *mut u8, start_point as u32);".into())
        })?;
        w.stmt("self".into())
      },
    )?;
    w.stmt("#[inline(always)]".into())?;
    w.method("fn construct_context", "(", ")", ", ", &|_| vec!["&mut self".into()], "", "{", "}", &mut |w| {
      w.block("unsafe", "{", "}", &|w| {
        w.stmt("let _ptr = &mut self.0 as *const ParseContext<T, M>;".into())?;
        w.stmt("let _rdr = &mut self.1 as *const T;".into())?;
        w.stmt("init(_ptr as *mut u8, _rdr as *mut u8);".into())
      })
    })?;
    w.stmt("#[inline(always)]".into())?;
    w.method("fn destroy_context", "(", ")", ", ", &|_| vec!["&mut self".into()], "", "{", "}", &mut |w| {
      w.stmt("let _ptr = &mut self.0 as *const ParseContext<T, M>;".into())?;
      w.block("unsafe", "{", "}", &|w| w.stmt("drop(_ptr as *mut u8);".into()))
    })?;
    for DBEntryPoint { export_id, nonterm_entry_name, entry_name, .. } in
      //for ExportedNon-terminal { export_name, export_id, .. } in
      w.db.entry_points().into_iter().filter(|e| e.is_export)
    {
      let entry_name = entry_name.to_string(w.db.string_store());
      let _nonterm_entry_name = nonterm_entry_name.to_string(w.db.string_store());
      w.stmt("#[inline(always)]".into())?;
      w.method(
        &format!("pub fn new_{entry_name}_parser"),
        "(",
        ")",
        ", ",
        &|_| vec!["reader: T".into()],
        "-> Self",
        "{",
        "}",
        &mut |w| {
          w.stmt(format!("let mut ctx = Self::new(reader);"))?;
          w.stmt(format!("ctx.set_start_point({export_id});"))?;
          w.stmt(format!("ctx"))
        },
      )?;
    }
    RadlrResult::Ok(())
  })?;

  w.block("impl<T: Reader, M> Iterator for Parser<T, M> ", "{", "}", &|w| {
    w.stmt("type Item = ParseActionType;".into())?;
    w.stmt("#[inline(always)]".into())?;
    w.method("fn next", "(", ")", ", ", &|_| vec!["&mut self".into()], "-> Option<Self::Item>", "{", "}", &mut |w| {
      w.block("unsafe", "{", "}", &|w| {
        w.block("if !self.0.is_active", "{", "}", &|w| w.stmt("None".into()))?;
        w.block("else", "{", "}", &|w| {
          w.stmt("let _ptr = &mut self.0 as *const ParseContext<T, M>;".into())?;
          w.stmt("Some(next(_ptr as *mut u8))".into())
        })
      })
    })
  })?;

  w.block("impl<T: Reader, M> Drop for Parser<T, M> ", "{", "}", &|w| {
    w.method("fn drop", "(", ")", ", ", &|_| vec!["&mut self".into()], "", "{", "}", &mut |w| {
      w.block("unsafe", "{", "}", &|w| w.stmt("self.destroy_context();".into()))
    })
  })?;

  if !w.store.is_dummy {
    let export_node_data = get_ascript_export_data(w.utils);
    let ast_type_name = w.store.ast_type_name.clone();

    w.block("pub mod ast", "{", "}", &|w| {
      w.stmt("use super::*; ".into())?;
      w.stmt(format!(
        r##"
impl AstObject for {ast_type_name} {{}}
type ASTSlot = ({ast_type_name}, TokenRange, TokenRange);

#[link(name = "{parser_name}", kind = "static")]
extern "C" {{
  fn ast_parse(
  ctx: *mut u8,
  reducers: *const u8,
  shift_handler: *const u8,
  result_handler: *const u8,
  ) -> ParseResult<{ast_type_name}>;
}}"##))?;

      // Create a module that will store convenience functions for compiling AST
      // structures based on on grammar entry points.
      for (ref_, type_, ast_type_string, export_name, ..) in &export_node_data {
        w.method(
          &format!("pub fn {export_name}_from<'a>"),
          "(",
          ")",
          ", ",
          &|_| vec!["reader: UTF8StringReader".into()],
          &format!("-> Result<{ast_type_string}, RadlrParseError>"),
          "{",
          "}",
          &mut |w| {
            w.stmt(format!("const reduce_functions: ReduceFunctions::<UTF8StringReader, u32, false> = ReduceFunctions::<UTF8StringReader, u32, false>::new();"))?;
            w.stmt(format!("let mut ctx = Parser::new_{export_name}_parser(reader);"))?;
            w.stmt(format!("let reducers_ptr = (&reduce_functions.0).as_ptr() as *const u8;"))?;
            w.stmt(format!("let shifter_ptr = llvm_map_shift_action::<UTF8StringReader, u32, {ast_type_name}> as *const u8;"))?;
            w.stmt(format!("let result_ptr = llvm_map_result_action::<UTF8StringReader, u32, {ast_type_name}> as *const u8;"))?;
            w.stmt(format!("let ctx_ptr = (&mut ctx.0) as *const ParseContext<UTF8StringReader, u32>;"))?;

            w.block("match unsafe{ ast_parse(ctx_ptr as *mut u8, reducers_ptr, shifter_ptr, result_ptr) }", "{", "}", &|w|{
              w.block(&format!("ParseResult::Complete(AstSlot({}, _, _))  =>", (&w.utils.get_slot_obj_name)(SlotIndex::Sym(0))), "{", "}", &|w|{
                if ref_.is_some() {
                  let (ref_name, ref_) =
                    w.utils.create_type_initializer_value(ref_.clone(), type_, false);
                  if let Some(ref_) = ref_ {
                    w.stmt(ref_.to_init_string(w.utils))?;
                    w.stmt(format!("Ok({ref_name})"))
                  } else {
                    w.stmt("Ok(i0)".to_string())
                  }
                } else {
                  w.stmt("Ok(i0)".to_string())
                }
              })?;
              w.block("ParseResult::Error(err_tok, _) =>", "{", "}", &|w|{
                w.block("Err", "(", ")", &|w| {
                  w.block("RadlrParseError", "{", "}", &|w| {
                    w.list(",", vec![
                      "inline_message: \"Token not recognized\".to_string()",
                      "last_nonterm: 0",
                      "loc: err_tok.to_token(&mut ctx.1)",
                      "message: \"Failed to parse\".to_string()",
                    ])
                  })
                })
              })?;
              w.stmt("_ => unreachable!()".into())
            })
          },
        )?;
      }
      RadlrResult::Ok(())
    })?;

    // Create a module that will store convince functions for compiling AST
    // structures based on on grammar entry points.
  }

  RadlrResult::Ok(w)
}
