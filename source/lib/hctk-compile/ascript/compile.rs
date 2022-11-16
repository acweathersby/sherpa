use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::vec;

use hctk_core::grammar::data::ast::ASTNode;
use hctk_core::grammar::data::ast::ASTNodeTraits;
use hctk_core::grammar::data::ast::AST_Add;
use hctk_core::grammar::data::ast::AST_IndexReference;
use hctk_core::grammar::data::ast::AST_NamedReference;
use hctk_core::grammar::data::ast::AST_Struct;
use hctk_core::grammar::data::ast::AST_Vector;
use hctk_core::grammar::data::ast::Ascript as AST_AScript;
use hctk_core::grammar::get_production_plain_name;
use hctk_core::types::*;

use crate::ascript::types::*;

use super::types::AScriptStore;

pub fn compile_ascript_store<'a>(g: &'a GrammarStore, ast: &mut AScriptStore) -> Vec<ParseError> {
  let mut errors = vec![];

  // Separate all bodies into a list of  of tuple of body id's and
  // Ascript reference nodes.

  let normal_parse_bodies: Vec<(BodyId, Option<&'a AST_AScript>)> = g
    .bodies
    .iter()
    .filter_map(|(id, body)| {
      if g.productions.get(&body.prod).unwrap().is_scanner {
        None
      } else {
        for function in &body.reduce_fn_ids {
          if let ReduceFunctionType::Ascript(ascript) = g.reduce_functions.get(function).unwrap() {
            return Some((*id, Some(ascript)));
          }
        }
        Some((*id, None))
      }
    })
    .collect::<Vec<_>>();

  // For reduce function in each body divide into those that resolve
  // into atomic types and those that don't (those that resolve into
  // productions). Add the types of the atomic functions to the
  // production types. Add any structs encountered into a separate
  // table, again adding these atomic struct types to the production
  // types.

  let mut struct_bodies: Vec<(BodyId, &'a AST_AScript)> = vec![];

  for (body_id, ascript_option_fn) in normal_parse_bodies {
    if let Some(body) = g.bodies.get(&body_id) {
      if let Some(ascript_fn) = &ascript_option_fn {
        match &ascript_fn.ast {
          ASTNode::AST_Struct(box ast_struct) => {
            if let AScriptTypeVal::Struct(id) = get_struct_type_from_node(ast_struct) {
              struct_bodies.push((body_id, ascript_fn));
              add_production_type(
                g,
                ast,
                body.prod,
                AScriptTypeVal::Struct(id),
                &body.origin_location,
              );
            }
          }
          ASTNode::AST_Statements(ast_stmts) => {
            let (sub_types, mut sub_errors) =
              compile_expression_type(g, ast, ast_stmts.statements.last().unwrap(), body);

            for sub_type in sub_types {
              add_production_type(g, ast, body.prod, sub_type, &body.origin_location);
            }

            errors.append(&mut sub_errors);
          }
          _ => {}
        }
      } else if body.len > 1 {
        // A token comprised the production is returned
        let item = Item::from_body(&body_id, g).unwrap();
        let sym = item.to_last_sym().get_symbol(g);
        add_production_type(g, ast, body.prod, AScriptTypeVal::Token, &body.origin_location);
      } else {
        // Evaluate the last new_return_type symbol in the production.
        let sym = Item::from_body(&body_id, g).unwrap().to_last_sym().get_symbol(g);
        let return_type = match sym {
          SymbolID::Production(id, ..) => add_production_type(
            g,
            ast,
            body.prod,
            AScriptTypeVal::UnresolvedProduction(id),
            &body.origin_location,
          ),
          _ => add_production_type(g, ast, body.prod, AScriptTypeVal::Token, &body.origin_location),
        };
      }
    }
  }

  resolve_production_return_types(ast);

  for key in Vec::from_iter(ast.prod_types.keys().cloned()) {
    let _types = ast.prod_types.get(&key).unwrap().to_owned();

    if _types.len() == 1 {
      let (_type, tokens) = (_types.iter().next().unwrap());
      match _type {
        AScriptTypeVal::GenericVec(Some(_types)) => {
          let resolved_vector_type = get_specified_vector_from_generic_vec_values(_types);

          ast
            .prod_types
            .insert(key, HashMap::from_iter(vec![(resolved_vector_type, tokens.to_owned())]));
        }
        _ => {}
      }
    }
  }

  // Ensure all non-scanner productions have been added to the ascript data.
  assert_eq!(ast.prod_types.len(), g.productions.iter().filter(|p| !p.1.is_scanner).count());

  // We now have the base types for all productions. We can now do a
  // thorough analysis of struct types and production return
  // functions.

  // We'll now finish parsing struct data and declaring or resolving
  // type conflicts

  for (body_id, ascript_fn) in struct_bodies {
    if let Some(body) = g.bodies.get(&body_id) {
      if let ASTNode::AST_Struct(box ast_struct) = &ascript_fn.ast {
        errors.append(&mut compile_struct_type(g, ast, ast_struct, body).1);
      }
    }
  }

  resolve_prop_types(ast);

  errors
}

fn resolve_prop_types(ascript: &mut AScriptStore) {
  // Ensure each property entry has a resolved data type.
  for prop_key in ascript.props.keys().cloned().collect::<Vec<_>>() {
    let type_val = ascript.props.get(&prop_key).unwrap().type_val.clone();

    ascript.props.get_mut(&prop_key).unwrap().type_val = get_resolved_type(ascript, &type_val).0;
  }
}

fn resolve_production_return_types(ast: &mut AScriptStore) {
  let mut per_production_refs = HashMap::new();

  let mut pending_prods = VecDeque::from_iter(ast.prod_types.keys().cloned());

  while let Some(prod_id) = pending_prods.pop_front() {
    let map = ast.prod_types.get(&prod_id).unwrap();
    let mut new_map = HashMap::new();

    // Production return types of vectors should be combined into a single type
    let vector_types =
      map.iter().filter(|(a, b)| matches!(a, AScriptTypeVal::GenericVec(..))).collect::<Vec<_>>();

    let non_vector_types =
      map.iter().filter(|(a, b)| !matches!(a, AScriptTypeVal::GenericVec(..))).collect::<Vec<_>>();

    if !vector_types.is_empty() && !non_vector_types.is_empty() {
      panic!("Incompatible types");
    } else if !non_vector_types.is_empty() {
      // Handle non_vectors
      for (_type, token) in non_vector_types {
        match _type {
          AScriptTypeVal::UnresolvedProduction(foreign_prod_id) => {
            if per_production_refs
              .entry(prod_id)
              .or_insert_with(|| BTreeSet::new())
              .insert(*foreign_prod_id)
            {
              let other_production_types = ast.prod_types.get(foreign_prod_id).unwrap();

              for (_type, token) in other_production_types {
                new_map.insert(_type.clone(), token.clone());
              }

              pending_prods.push_back(prod_id);
            }
          }
          _ => {
            new_map.insert(_type.clone(), token.clone());
          }
        }
      }
    } else {
      // Handle vectors
      let mut combined_vector_types = BTreeSet::new();
      let mut new_tokens = BTreeSet::new();

      for (vector_type, origin_tokens) in vector_types {
        match vector_type {
          AScriptTypeVal::GenericVec(Some(types)) => {
            new_tokens.append(&mut origin_tokens.clone());

            let mut resubmit = false;
            let mut queue = VecDeque::from_iter(types);

            while let Some(_type) = queue.pop_front() {
              match _type {
                AScriptTypeVal::GenericVec(Some(types)) => {
                  for _type in types {
                    queue.push_back(_type);
                  }
                }
                AScriptTypeVal::GenericVec(None) => {}
                AScriptTypeVal::UnresolvedProduction(foreign_prod_id) => {
                  if per_production_refs
                    .entry(prod_id)
                    .or_insert_with(|| BTreeSet::new())
                    .insert(*foreign_prod_id)
                  {
                    let other_production_types = ast.prod_types.get(&foreign_prod_id).unwrap();

                    for (_type, token) in other_production_types {
                      combined_vector_types.insert(_type.clone());
                    }

                    resubmit = true;
                  }
                }
                _type => {
                  combined_vector_types.insert(_type.clone());
                }
              }
            }

            if resubmit {
              pending_prods.push_back(prod_id);
            }
          }
          _ => {}
        }
      }

      new_map.insert(AScriptTypeVal::GenericVec(Some(combined_vector_types)), new_tokens);
    }

    ast.prod_types.insert(prod_id, new_map);
  }
}

/// Retrieve the resolved type of the base type. For most ascript types
/// this returns a clone of the `base_type`. For vectors and unresolved
/// productions types, this attempts to replace such types with resolved
/// versions
pub fn get_resolved_type(
  ascript: &AScriptStore,
  base_type: &AScriptTypeVal,
) -> (AScriptTypeVal, bool) {
  match base_type {
    AScriptTypeVal::UnresolvedProduction(production_id) => {
      if let Some(types) = ascript
        .prod_types
        .get(production_id)
        .and_then(|t| Some(t.keys().cloned().collect::<Vec<_>>()))
      {
        if types.len() == 1 {
          (types[0].clone(), false)
        } else if types
          .iter()
          .all(|t| matches!(t, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..)))
        {
          let nodes = types
            .iter()
            .flat_map(|t| match t.clone() {
              AScriptTypeVal::Struct(id) => vec![id],
              AScriptTypeVal::GenericStruct(ids) => Vec::from_iter(ids.iter().cloned()),
              _ => vec![],
            })
            .collect::<BTreeSet<_>>();

          (AScriptTypeVal::GenericStruct(nodes), false)
        } else {
          (AScriptTypeVal::Any, false)
        }
      } else {
        (AScriptTypeVal::Undefined, false)
      }
    }

    AScriptTypeVal::GenericVec(Some(vector_sub_types)) => {
      let contents = BTreeSet::from_iter(get_resolved_vec_contents(ascript, base_type));
      // Flatten the subtypes into one array and get the resulting type from that
      (get_specified_vector_from_generic_vec_values(&contents), false)
    }

    _ => (base_type.clone(), false),
  }
}

pub fn get_resolved_vec_contents(
  ast: &AScriptStore,
  base_type: &AScriptTypeVal,
) -> Vec<AScriptTypeVal> {
  use AScriptTypeVal::*;

  match base_type {
    F64Vec => vec![F64(None)],
    F32Vec => vec![F32(None)],
    I64Vec => vec![I64(None)],
    I32Vec => vec![I32(None)],
    I16Vec => vec![I16(None)],
    I8Vec => vec![I8(None)],
    U64Vec => vec![U64(None)],
    U32Vec => vec![U32(None)],
    U16Vec => vec![U16(None)],
    U8Vec => vec![U8(None)],
    GenericStructVec(types) => types.iter().map(|t| AScriptTypeVal::Struct(*t)).collect(),
    GenericVec(Some(types)) => {
      types.iter().flat_map(|t| get_resolved_vec_contents(ast, t)).collect()
    }
    TokenVec => vec![Token],
    StringVec => vec![String(None)],
    UnresolvedProduction(_) => get_resolved_vec_contents(ast, &get_resolved_type(ast, base_type).0),
    none_vec_type => {
      vec![none_vec_type.clone()]
    }
  }
}

pub fn add_production_type(
  g: &GrammarStore,
  ast: &mut AScriptStore,
  prod_id: ProductionId,
  new_return_type: AScriptTypeVal,
  new_origin: &Token,
) {
  ast.prod_types.entry(prod_id).or_insert_with(HashMap::new);

  let mut table = ast.prod_types.get_mut(&prod_id).unwrap();

  match table.entry(new_return_type) {
    Entry::Occupied(mut entry) => {
      entry.get_mut().insert(new_origin.clone());
    }
    Entry::Vacant(mut entry) => {
      entry.insert(BTreeSet::from_iter(vec![new_origin.clone()]));
    }
  }
}

pub fn merge_production_type(
  g: &GrammarStore,
  ast: &mut AScriptStore,
  prod_id: ProductionId,
  new_return_type: AScriptTypeVal,
  new_origin: &Token,
) -> Vec<ParseError> {
  let mut errors = vec![];

  ast.prod_types.entry(prod_id).or_insert_with(HashMap::new);

  if let Some(types) = ast.prod_types.get_mut(&prod_id) {
    // Warn about incompatible types
    for (existing_type, origins) in types.iter() {
      match existing_type {
        AScriptTypeVal::UnresolvedProduction(..) => {}

        existing_type => {
          let mut locations = vec![CompileProblem {
            message: "".to_string(),
            loc: new_origin.clone(),
            inline_message: "Derived here".to_string(),
          }];

          locations.append(
            &mut origins
              .iter()
              .map(|t| CompileProblem {
                message: format!(
                  "Existing incompatible type {}",
                  existing_type.hcobj_type_name(Some(g))
                ),
                loc: t.clone(),
                inline_message: "Derived here".to_string(),
              })
              .collect::<Vec<_>>(),
          );

          if !new_return_type.is_same_type(existing_type) {
            errors.push(ParseError::COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem {
              message: format!(
                "Incompatible production return type {} on production {}",
                new_return_type.hcobj_type_name(Some(g)),
                get_production_plain_name(&prod_id, g)
              ),
              locations,
            }));
          }
        }
      }
    }

    types.insert(new_return_type, BTreeSet::from_iter(vec![new_origin.clone()]));
  }

  errors
}

pub fn compile_expression_type(
  g: &GrammarStore,
  ast: &mut AScriptStore,
  ast_expression: &ASTNode,
  body: &Body,
) -> (Vec<AScriptTypeVal>, Vec<ParseError>) {
  let mut errors = vec![];

  let types = match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      let (struct_type, mut error) = compile_struct_type(g, ast, ast_struct, body);

      errors.append(&mut error);

      vec![struct_type]
    }
    ASTNode::AST_Token(..) => vec![AScriptTypeVal::Token],
    ASTNode::AST_Add(box AST_Add { left, .. }) => {
      let (sub_types, mut sub_errors) = compile_expression_type(g, ast, left, body);
      errors.append(&mut sub_errors);
      sub_types
    }
    ASTNode::AST_Vector(box AST_Vector { initializer, .. }) => {
      let mut types = BTreeSet::new();

      for node in initializer {
        let (sub_types, mut sub_errors) = compile_expression_type(g, ast, node, body);

        for sub_type in sub_types {
          match sub_type {
            AScriptTypeVal::GenericVec(sub_types) => match sub_types {
              Some(mut sub_type) => {
                types.append(&mut sub_type.clone());
              }
              None => {}
            },
            other => {
              types.insert(other.clone());
            }
          }
        }

        errors.append(&mut sub_errors);
      }
      if types.is_empty() {
        vec![AScriptTypeVal::GenericVec(None)]
      } else {
        vec![AScriptTypeVal::GenericVec(Some(types))]
      }
    }
    ASTNode::AST_STRING(..) => vec![AScriptTypeVal::String(None)],
    ASTNode::AST_BOOL(..) => vec![AScriptTypeVal::Bool(None)],
    ASTNode::AST_U8(..) => vec![AScriptTypeVal::U8(None)],
    ASTNode::AST_U16(..) => vec![AScriptTypeVal::U16(None)],
    ASTNode::AST_U32(..) => vec![AScriptTypeVal::U32(None)],
    ASTNode::AST_U64(..) => vec![AScriptTypeVal::U64(None)],
    ASTNode::AST_I8(..) => vec![AScriptTypeVal::I8(None)],
    ASTNode::AST_I16(..) => vec![AScriptTypeVal::I16(None)],
    ASTNode::AST_I32(..) => vec![AScriptTypeVal::I32(None)],
    ASTNode::AST_I64(..) => vec![AScriptTypeVal::I64(None)],
    ASTNode::AST_F32(..) => vec![AScriptTypeVal::F32(None)],
    ASTNode::AST_F64(..) => vec![AScriptTypeVal::F64(None)],
    ASTNode::AST_NUMBER(..) => vec![AScriptTypeVal::F64(None)],
    ASTNode::AST_Member(..) => vec![AScriptTypeVal::Undefined],
    ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) => {
      match get_named_body_ref(body, value) {
        Some((_, sym_ref)) => match sym_ref.sym_id {
          SymbolID::Production(id, ..) => {
            vec![AScriptTypeVal::UnresolvedProduction(id)]
          }
          _ => vec![AScriptTypeVal::Token],
        },
        None => vec![AScriptTypeVal::Undefined],
      }
    }
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      match get_indexed_body_ref(body, value) {
        Some((_, sym_ref)) => match sym_ref.sym_id {
          SymbolID::Production(id, ..) => {
            vec![AScriptTypeVal::UnresolvedProduction(id)]
          }
          _ => vec![AScriptTypeVal::Token],
        },
        None => vec![AScriptTypeVal::Undefined],
      }
    }
    _ => vec![AScriptTypeVal::Undefined],
  };

  (types, errors)
}

pub fn compile_struct_type(
  g: &GrammarStore,
  ast: &mut AScriptStore,
  ast_struct: &AST_Struct,
  body: &Body,
) -> (AScriptTypeVal, Vec<ParseError>) {
  let mut errors = vec![];

  let types = ast_struct
    .props
    .iter()
    .filter_map(|node| match node {
      ASTNode::AST_TypeId(id) => Some(id),
      _ => None,
    })
    .collect::<Vec<_>>();

  let classes = ast_struct
    .props
    .iter()
    .filter_map(|node| match node {
      ASTNode::AST_ClassId(id) => Some(id),
      _ => None,
    })
    .collect::<Vec<_>>();

  // Use the last type as the official type name of the struct.
  let type_name =
    if let Some(node) = types.last() { node.value.clone() } else { "unknown".to_string() }[2..]
      .to_string();

  // Validate struct type is singular

  if types.len() > 1 {
    errors.push(ParseError::COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem {
      message:   "Struct Type Redefined".to_string(),
      locations: types
        .iter()
        .enumerate()
        .map(|(i, node)| {
          if i == 0 {
            CompileProblem {
              message: "".to_string(),
              loc: node.Token(),
              inline_message: "First Defined Here".to_string(),
            }
          } else {
            CompileProblem {
              message: "".to_string(),
              loc: node.Token(),
              inline_message: "Redefined Here".to_string(),
            }
          }
        })
        .collect::<Vec<_>>(),
    }));
  } else if types.is_empty() {
    errors.push(ParseError::COMPILE_PROBLEM(CompileProblem {
      message: "Struct defined without a type name".to_string(),
      loc: ast_struct.Token(),
      inline_message: "".to_string(),
    }))
  }

  // Check to see if this struct is already defined. If so, we'll
  // append new properties to it. otherwise we create a new
  // struct entry and add props.

  let id = AScriptStructId::new(&type_name);
  let mut prop_ids = BTreeSet::new();
  let mut include_token = false;

  for prop in &ast_struct.props {
    match prop {
      ASTNode::AST_Token(..) => include_token = true,
      ASTNode::AST_Property(box prop) => {
        let name = &prop.id;
        let prop_id = AScriptPropId::new(id, name);

        prop_ids.insert(prop_id.clone());

        let (prop_types, sub_errors) = compile_expression_type(g, ast, &prop.value, body);

        for prop_type in &prop_types {
          if let Some(existing) = ast.props.get_mut(&prop_id) {
            if !existing.type_val.is_same_type(&prop_type) {
              if existing.type_val.is_undefined() {
                let define_count = existing.define_count + 1;
                ast.props.insert(prop_id.clone(), AScriptProp {
                  type_val: prop_type.to_owned(),
                  define_count,
                  first_declared_location: prop.value.Token(),
                  optional: true,
                });
              } else if prop_type.is_undefined() {
                existing.define_count += 1;
                existing.optional = true;
              } else {
                errors.push(ParseError::COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem {
                  message: format!("Redefinition of the property {} in struct {}", name, type_name),

                  locations: vec![
                    CompileProblem {
                      message: String::new(),
                      loc: existing.first_declared_location.clone(),
                      inline_message: format!(
                        "First defined as {}.",
                        existing.type_val.hcobj_type_name(Some(g))
                      ),
                    },
                    CompileProblem {
                      message: String::new(),
                      loc: prop.value.Token(),
                      inline_message: format!(
                        "Redefined as {}.",
                        prop_type.hcobj_type_name(Some(g))
                      ),
                    },
                  ],
                }))
              }
            } else {
              existing.define_count += 1;
            }
          } else {
            ast.props.insert(prop_id.clone(), AScriptProp {
              type_val: prop_type.to_owned(),
              define_count: 1,
              first_declared_location: prop.value.Token(),
              optional: false,
            });
          }
        }
      }
      _ => {}
    }
  }

  let mut struct_define_count = 1;

  for prop_id in if let Some(ascript_struct) = ast.structs.get_mut(&id) {
    ascript_struct.define_count += 1;
    struct_define_count = ascript_struct.define_count;
    ascript_struct.definition_locations.push(ast_struct.Token());
    ascript_struct.prop_ids.append(&mut prop_ids);
    ascript_struct.include_token = include_token || ascript_struct.include_token;
    ascript_struct.prop_ids.clone()
  } else {
    ast.structs.insert(id, AScriptStruct {
      id,
      type_name,
      define_count: 1,
      definition_locations: vec![ast_struct.Token()],
      prop_ids: prop_ids.clone(),
      include_token,
    });
    prop_ids
  } {
    let prop = ast.props.get_mut(&prop_id).unwrap();
    if prop.define_count != struct_define_count {
      prop.optional = true;
    }
  }

  (AScriptTypeVal::Struct(id), errors)
}

pub fn production_types_are_structs(production_types: &BTreeSet<AScriptTypeVal>) -> bool {
  production_types.iter().all(|t| matches!(t.clone(), AScriptTypeVal::Struct(..)))
}

pub fn get_production_types(
  ast: &AScriptStore,
  prod_id: &ProductionId,
) -> BTreeSet<AScriptTypeVal> {
  ast.prod_types.get(prod_id).unwrap().keys().cloned().collect::<BTreeSet<_>>()
}
/// Returns a specified vector type from a generic vector
pub fn get_specified_vector_from_generic_vec_values(
  vals: &BTreeSet<AScriptTypeVal>,
) -> AScriptTypeVal {
  if vals.len() > 1 {
    if vals.iter().all(|t| {
      matches!(
        t,
        AScriptTypeVal::Struct(..)
          | AScriptTypeVal::GenericStructVec(..)
          | AScriptTypeVal::GenericStruct(..)
      )
    }) {
      AScriptTypeVal::GenericStructVec(
        vals
          .iter()
          .flat_map(|n| match n {
            AScriptTypeVal::Struct(id) => {
              vec![id.clone()]
            }
            AScriptTypeVal::GenericStruct(struct_ids) => Vec::from_iter(struct_ids.iter().cloned()),
            _ => vec![],
          })
          .collect::<BTreeSet<_>>(),
      )
    } else if vals
      .iter()
      .all(|t| matches!(t, AScriptTypeVal::String(..) | AScriptTypeVal::StringVec))
    {
      AScriptTypeVal::StringVec
    } else if vals.iter().all(|t| matches!(t, AScriptTypeVal::Token | AScriptTypeVal::TokenVec)) {
      AScriptTypeVal::TokenVec
    } else if vals.iter().all(|t| {
      matches!(
        t,
        AScriptTypeVal::U8(..)
          | AScriptTypeVal::U8Vec
          | AScriptTypeVal::U16(..)
          | AScriptTypeVal::U16Vec
          | AScriptTypeVal::U32(..)
          | AScriptTypeVal::U32Vec
          | AScriptTypeVal::U64(..)
          | AScriptTypeVal::U64Vec
          | AScriptTypeVal::I8(..)
          | AScriptTypeVal::I8Vec
          | AScriptTypeVal::I16(..)
          | AScriptTypeVal::I16Vec
          | AScriptTypeVal::I32(..)
          | AScriptTypeVal::I32Vec
          | AScriptTypeVal::I64(..)
          | AScriptTypeVal::I64Vec
          | AScriptTypeVal::F32(..)
          | AScriptTypeVal::F32Vec
          | AScriptTypeVal::F64(..)
          | AScriptTypeVal::F64Vec
      )
    }) {
      match vals
        .iter()
        .map(|v| match v {
          AScriptTypeVal::U8(..) | AScriptTypeVal::U8Vec => 1,
          AScriptTypeVal::I8(..) | AScriptTypeVal::I8Vec => 2,
          AScriptTypeVal::U16(..) | AScriptTypeVal::U16Vec => 3,
          AScriptTypeVal::I16(..) | AScriptTypeVal::I16Vec => 4,
          AScriptTypeVal::U32(..) | AScriptTypeVal::U32Vec => 5,
          AScriptTypeVal::I32(..) | AScriptTypeVal::I32Vec => 6,
          AScriptTypeVal::U64(..) | AScriptTypeVal::U64Vec => 7,
          AScriptTypeVal::I64(..) | AScriptTypeVal::I64Vec => 8,
          AScriptTypeVal::F32(..) | AScriptTypeVal::F32Vec => 9,
          AScriptTypeVal::F64(..) | AScriptTypeVal::F64Vec => 10,
          _ => 0,
        })
        .fold(0, |a, b| a.max(b))
      {
        1 => AScriptTypeVal::U8Vec,
        2 => AScriptTypeVal::I8Vec,
        3 => AScriptTypeVal::U16Vec,
        4 => AScriptTypeVal::I16Vec,
        5 => AScriptTypeVal::U32Vec,
        6 => AScriptTypeVal::I32Vec,
        7 => AScriptTypeVal::U64Vec,
        8 => AScriptTypeVal::I64Vec,
        9 => AScriptTypeVal::F32Vec,
        10 => AScriptTypeVal::F64Vec,
        _ => AScriptTypeVal::Undefined,
      }
    } else {
      AScriptTypeVal::Undefined
    }
  } else {
    match vals.first().unwrap() {
      AScriptTypeVal::Struct(id) => {
        AScriptTypeVal::GenericStructVec(BTreeSet::from_iter(vec![*id]))
      }
      AScriptTypeVal::GenericStruct(ids) => AScriptTypeVal::GenericStructVec(ids.clone()),
      AScriptTypeVal::U8(..) => AScriptTypeVal::U8Vec,
      AScriptTypeVal::U16(..) => AScriptTypeVal::U16Vec,
      AScriptTypeVal::U32(..) => AScriptTypeVal::U32Vec,
      AScriptTypeVal::U64(..) => AScriptTypeVal::U64Vec,
      AScriptTypeVal::I8(..) => AScriptTypeVal::I8Vec,
      AScriptTypeVal::I16(..) => AScriptTypeVal::I16Vec,
      AScriptTypeVal::I32(..) => AScriptTypeVal::I32Vec,
      AScriptTypeVal::I64(..) => AScriptTypeVal::I64Vec,
      AScriptTypeVal::F32(..) => AScriptTypeVal::F32Vec,
      AScriptTypeVal::F64(..) => AScriptTypeVal::F64Vec,
      AScriptTypeVal::Token => AScriptTypeVal::TokenVec,
      AScriptTypeVal::String(..) => AScriptTypeVal::StringVec,
      _ => AScriptTypeVal::Undefined,
    }
  }
}

pub fn get_named_body_ref<'a>(body: &'a Body, val: &str) -> Option<(usize, &'a BodySymbolRef)> {
  if val == "first" {
    Some((0, body.syms.first().unwrap()))
  } else if val == "last" {
    Some((body.syms.len() - 1, body.syms.last().unwrap()))
  } else {
    body.syms.iter().enumerate().filter(|(_, s)| s.annotation == *val).last()
  }
}

pub fn get_indexed_body_ref<'a>(body: &'a Body, i: &f64) -> Option<(usize, &'a BodySymbolRef)> {
  body.syms.iter().enumerate().filter(|(_, s)| s.original_index == (*i - 1.0) as u32).last()
}

pub fn get_struct_type_from_node(ast_struct: &AST_Struct) -> AScriptTypeVal {
  let types = ast_struct
    .props
    .iter()
    .filter_map(|node| match node {
      ASTNode::AST_TypeId(id) => Some(id),
      _ => None,
    })
    .collect::<Vec<_>>();

  // Use the last type as the official type name of the struct.
  if let Some(node) = types.last() {
    AScriptTypeVal::Struct(AScriptStructId::new(&node.value.clone()[2..]))
  } else {
    AScriptTypeVal::Undefined
  }
}
