use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::vec;

use crate::ascript;
use crate::debug::grammar;
use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::ASTNodeTraits;
use crate::grammar::data::ast::AST_Add;
use crate::grammar::data::ast::AST_IndexReference;
use crate::grammar::data::ast::AST_NamedReference;
use crate::grammar::data::ast::AST_Statements;
use crate::grammar::data::ast::AST_Struct;
use crate::grammar::data::ast::AST_Vector;
use crate::grammar::data::ast::Ascript as AST_AScript;
use crate::grammar::data::ast::AST_BOOL;
use crate::grammar::data::ast::AST_F32;
use crate::grammar::data::ast::AST_F64;
use crate::grammar::data::ast::AST_I16;
use crate::grammar::data::ast::AST_I32;
use crate::grammar::data::ast::AST_I64;
use crate::grammar::data::ast::AST_I8;
use crate::grammar::data::ast::AST_NUMBER;
use crate::grammar::data::ast::AST_STRING;
use crate::grammar::data::ast::AST_U16;
use crate::grammar::data::ast::AST_U32;
use crate::grammar::data::ast::AST_U64;
use crate::grammar::data::ast::AST_U8;
use crate::grammar::get_production_plain_name;
use crate::grammar::hash_id_value_u64;
use crate::types::*;
use std::mem::discriminant;

pub fn compile_reduce_function_expressions<'a>(
  grammar: &'a GrammarStore,
  ascript: &mut AScriptStore,
) -> Vec<ParseError>
{
  let mut errors = vec![];
  let mut temp_ascript = AScriptStore::new();

  // Separate all bodies into a list of  of tuple of body id's and
  // Ascript refence nodes.

  let bodies_map: Vec<(BodyId, Option<&'a AST_AScript>)> = grammar
    .bodies_table
    .iter()
    .filter_map(|(id, body)| {
      if grammar.production_table.get(&body.production).unwrap().is_scanner {
        None
      } else {
        for function in &body.reduce_fn_ids {
          if let ReduceFunctionType::Ascript(ascript) =
            grammar.reduce_functions.get(function).unwrap()
          {
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

  let mut post_anaylsis_productions = vec![];

  for (body_id, ascript_option_fn) in bodies_map {
    if let Some(body) = grammar.bodies_table.get(&body_id) {
      if let Some(ascript_fn) = &ascript_option_fn {
        match &ascript_fn.ast {
          ASTNode::AST_Struct(box ast_struct) => {
            if let AScriptTypeVal::Struct(id) = get_struct_type_from_node(ast_struct) {
              struct_bodies.push((body_id, ascript_fn));
              add_production_type(
                body.production,
                AScriptTypeVal::Struct(id),
                &body.origin_location,
                ascript,
                grammar,
              );
            }
          }
          ASTNode::AST_Statements(ast_stmts) => {
            let (sub_types, mut sub_errors) = compile_expression_type(
              ast_stmts.statements.last().unwrap(),
              body,
              ascript,
              grammar,
            );

            for sub_type in sub_types {
              add_production_type(
                body.production,
                sub_type,
                &body.origin_location,
                ascript,
                grammar,
              );
            }

            errors.append(&mut sub_errors);
          }
          _ => {}
        }
      } else {
        // Evaluate the lastnew_return_type symbol in the production.
        let item = Item::from_body(&body_id, grammar).unwrap();
        let sym = item.to_last_sym().get_symbol(grammar);
        let return_type = match sym {
          SymbolID::Production(id, ..) => {
            post_anaylsis_productions.push((body.production, id));
          }
          _ => add_production_type(
            body.production,
            AScriptTypeVal::Token,
            &body.origin_location,
            ascript,
            grammar,
          ),
        };
      }
    }
  }

  for (production_id, return_type_production) in post_anaylsis_productions {
    if let Some(new_types) = ascript.production_types.get(&return_type_production) {
      for (val, _type) in new_types.clone() {
        add_production_type(production_id, val, &_type, ascript, grammar)
      }
    } else {
    }
  }

  // We now process productions by merging all type information
  // that results from accessing atomic types within productions.

  for production_id in ascript.production_types.keys().cloned().collect::<Vec<_>>() {
    let mut seen = BTreeSet::<ProductionId>::new();

    seen.insert(production_id);

    let mut queue =
      VecDeque::from_iter(ascript.production_types.get(&production_id).unwrap().clone());

    ascript.production_types.insert(production_id, HashMap::new());

    let resolved_table = HashMap::new();

    while let Some((ast_type, token)) = queue.pop_front() {
      match &ast_type {
        AScriptTypeVal::UnresolvedProduction(production_id) => {
          if seen.insert(*production_id) {
            queue.append(&mut VecDeque::from_iter(
              ascript.production_types.get(production_id).unwrap().clone(),
            ));
          }
        }

        ast_type => {
          errors.append(&mut merge_production_type(
            production_id,
            ast_type.clone(),
            &token,
            ascript,
            grammar,
          ));
        }
      }
    }

    ascript.production_types.insert(production_id, resolved_table);
  }

  // Ensure all non-scanner productions have been added to the ascript data.
  assert_eq!(
    ascript.production_types.len(),
    grammar.production_table.iter().filter(|p| !p.1.is_scanner).count()
  );

  // We now have the base types for all productions. We can now do a
  // thorough analysis of struct types and production return
  // functions.

  // We'll now finish parsing struct data and declaring or resolving
  // type conflicts

  for (body_id, ascript_fn) in struct_bodies {
    if let Some(body) = grammar.bodies_table.get(&body_id) {
      if let ASTNode::AST_Struct(box ast_struct) = &ascript_fn.ast {
        errors.append(&mut compile_struct_type(ast_struct, body, ascript, grammar).1);
      }
    }
  }

  errors
}

pub fn add_production_type(
  production_id: ProductionId,
  new_return_type: AScriptTypeVal,
  new_origin: &Token,
  ascript: &mut AScriptStore,
  grammar: &GrammarStore,
)
{
  ascript.production_types.entry(production_id).or_insert_with(HashMap::new);

  let mut table = ascript.production_types.get_mut(&production_id).unwrap();

  table.entry(new_return_type).or_insert_with(|| new_origin.clone());
}

pub fn merge_production_type(
  production_id: ProductionId,
  new_return_type: AScriptTypeVal,
  new_origin: &Token,
  ascript: &mut AScriptStore,
  grammar: &GrammarStore,
) -> Vec<ParseError>
{
  let mut errors = vec![];

  ascript.production_types.entry(production_id).or_insert_with(HashMap::new);

  if let Some(types) = ascript.production_types.get_mut(&production_id) {
    // Warn about incompatible types
    for (existing_type, origin) in types.iter() {
      match existing_type {
        AScriptTypeVal::UnresolvedProduction(..) => {}
        existing_type => {
          if !new_return_type.is_same_type(existing_type) {
            errors.push(ParseError::COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem {
              message:   format!(
                "Incompatible production return type {} on production {}",
                new_return_type.type_name(grammar),
                get_production_plain_name(&production_id, grammar)
              ),
              locations: vec![
                CompileProblem {
                  message: "".to_string(),
                  loc: new_origin.clone(),
                  inline_message: "Derived here".to_string(),
                },
                CompileProblem {
                  message: format!(
                    "Existing incompatible type {}",
                    existing_type.type_name(grammar)
                  ),
                  loc: origin.clone(),
                  inline_message: "Derived here".to_string(),
                },
              ],
            }));
          }
        }
      }
    }

    types.insert(new_return_type, new_origin.clone());
  }

  errors
}
pub fn compile_expression_type(
  ast_expression: &ASTNode,
  body: &Body,
  store: &mut AScriptStore,
  grammar: &GrammarStore,
) -> (Vec<AScriptTypeVal>, Vec<ParseError>)
{
  let mut errors = vec![];

  let types = match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      let (struct_type, mut error) =
        compile_struct_type(ast_struct, body, store, grammar);

      errors.append(&mut error);

      vec![struct_type]
    }
    ASTNode::AST_Token(..) => vec![AScriptTypeVal::Token],
    ASTNode::AST_Add(box AST_Add { left, .. }) => {
      let (sub_types, mut sub_errors) =
        compile_expression_type(left, body, store, grammar);
      errors.append(&mut sub_errors);
      sub_types
    }
    ASTNode::AST_Vector(box AST_Vector { initializer, .. }) => {
      let mut types = vec![];
      for node in initializer {
        let (sub_types, mut sub_errors) =
          compile_expression_type(ast_expression, body, store, grammar);

        for sub_type in sub_types {
          match sub_type {
            AScriptTypeVal::Vector(sub_types) => match sub_types {
              Some(mut sub_type) => {
                types.append(&mut sub_type.clone());
              }
              None => {}
            },
            other => {
              types.push(other.clone());
            }
          }
        }

        errors.append(&mut sub_errors);
      }
      if types.is_empty() {
        vec![AScriptTypeVal::Vector(None)]
      } else {
        vec![AScriptTypeVal::Vector(Some(types))]
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
      let reference_sym =
        body.symbols.iter().filter(|s| s.annotation == *value).collect::<Vec<_>>();

      if let Some(BodySymbolRef { sym_id, .. }) = reference_sym.first() {
        match sym_id {
          SymbolID::Production(id, ..) => {
            vec![AScriptTypeVal::UnresolvedProduction(*id)]
          }
          _ => vec![AScriptTypeVal::Token],
        }
      } else {
        vec![AScriptTypeVal::Undefined]
      }
    }
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      let reference_sym = body
        .symbols
        .iter()
        .filter(|s| s.original_index as u32 == *value as u32)
        .collect::<Vec<_>>();

      if let Some(BodySymbolRef { sym_id, .. }) = reference_sym.first() {
        match sym_id {
          SymbolID::Production(id, ..) => {
            vec![AScriptTypeVal::UnresolvedProduction(*id)]
          }
          _ => vec![AScriptTypeVal::Token],
        }
      } else {
        vec![AScriptTypeVal::Undefined]
      }
    }
    _ => vec![AScriptTypeVal::Undefined],
  };

  (types, errors)
}

pub fn get_struct_type_from_node(ast_struct: &AST_Struct) -> AScriptTypeVal
{
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

pub fn compile_struct_type(
  ast_struct: &AST_Struct,
  body: &Body,
  store: &mut AScriptStore,
  grammar: &GrammarStore,
) -> (AScriptTypeVal, Vec<ParseError>)
{
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
  let type_name = if let Some(node) = types.last() {
    node.value.clone()
  } else {
    "unknown".to_string()
  }[2..]
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

  let mut properties = BTreeSet::new();

  let mut include_token = false;

  for prop in &ast_struct.props {
    match prop {
      ASTNode::AST_Token(..) => include_token = true,
      ASTNode::AST_Property(box prop) => {
        let name = &prop.id;
        let prop_id = AScriptPropId::new(id, name);

        properties.insert(prop_id.clone());

        let (prop_types, sub_errors) =
          compile_expression_type(&prop.value, body, store, grammar);

        for prop_type in prop_types {
          if let Some(existing) = store.props_table.get(&prop_id) {
            if !existing.type_val.is_same_type(&prop_type) {
              errors.push(ParseError::COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem {
                message: format!(
                  "Redefinition of the property {} in struct {}",
                  name, type_name
                ),

                locations: vec![
                  CompileProblem {
                    message: String::new(),
                    loc: existing.first_declared_location.clone(),
                    inline_message: format!(
                      "First defined as a {} here.",
                      existing.type_val.type_name(grammar)
                    ),
                  },
                  CompileProblem {
                    message: String::new(),
                    loc: prop.value.Token(),
                    inline_message: format!(
                      "Redefined as a {} here.",
                      prop_type.type_name(grammar)
                    ),
                  },
                ],
              }))
            }
          } else {
            store.props_table.insert(prop_id.clone(), AScriptProp {
              type_val: prop_type,
              first_declared_location: prop.value.Token(),
            });
          }
        }
      }
      _ => {}
    }
  }

  if let Some(ascript_struct) = store.struct_table.get_mut(&id) {
    ascript_struct.definition_locations.push(ast_struct.Token());
    ascript_struct.properties.append(&mut properties);
    ascript_struct.include_token = include_token || ascript_struct.include_token;
  } else {
    store.struct_table.insert(id, AScriptStruct {
      id,
      type_name,
      definition_locations: vec![ast_struct.Token()],
      properties,
      include_token,
    });
  }

  (AScriptTypeVal::Struct(id), errors)
}

fn get_ascript_type(node: &ASTNode) -> AScriptTypeVal
{
  match node {
    ASTNode::AST_STRING(box AST_STRING { .. }) => AScriptTypeVal::String(None),
    ASTNode::AST_Token(..) => AScriptTypeVal::Token,
    ASTNode::AST_U8(box AST_U8 { .. }) => AScriptTypeVal::U8(None),
    ASTNode::AST_U16(box AST_U16 { .. }) => AScriptTypeVal::U16(None),
    ASTNode::AST_U32(box AST_U32 { .. }) => AScriptTypeVal::U32(None),
    ASTNode::AST_U64(box AST_U64 { .. }) => AScriptTypeVal::U64(None),
    ASTNode::AST_I8(box AST_I8 { .. }) => AScriptTypeVal::I8(None),
    ASTNode::AST_I16(box AST_I16 { .. }) => AScriptTypeVal::I16(None),
    ASTNode::AST_I32(box AST_I32 { .. }) => AScriptTypeVal::I32(None),
    ASTNode::AST_I64(box AST_I64 { .. }) => AScriptTypeVal::I64(None),
    ASTNode::AST_F32(box AST_F32 { .. }) => AScriptTypeVal::F32(None),
    ASTNode::AST_F64(box AST_F64 { .. }) => AScriptTypeVal::F64(None),
    ASTNode::AST_BOOL(box AST_BOOL { .. }) => AScriptTypeVal::Bool(None),
    _ => AScriptTypeVal::Undefined,
  }
}

fn compile_type() {}
