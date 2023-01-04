use super::types::{
  AScriptPropId,
  AScriptStore,
  AScriptStruct,
  AScriptStructId,
  AScriptTypeVal,
  ProductionTypesTable,
  TaggedType,
};
use crate::{
  ascript::{
    errors::{
      ErrIncompatibleProductionScalerTypes,
      ErrIncompatibleProductionVectorTypes,
      ErrPropRedefinition,
      ErrUnionOfScalarsAndVectors,
    },
    types::AScriptProp,
  },
  grammar::data::ast::{
    ASTNode,
    ASTNodeTraits,
    AST_Add,
    AST_IndexReference,
    AST_NamedReference,
    AST_Struct,
    AST_Vector,
    Ascript,
  },
  types::*,
};
use std::{
  collections::{btree_map, hash_map::Entry, BTreeSet, HashMap, VecDeque},
  vec,
};

pub(crate) fn compile_ascript_store(ast: &mut AScriptStore) -> Vec<SherpaError> {
  let mut e = vec![];
  let mut temp_production_types = ProductionTypesTable::new();

  gather_ascript_info_from_grammar(ast, &mut e, &mut temp_production_types);

  resolve_production_reduce_types(ast, &mut e, temp_production_types);

  match e.have_critical() {
    true => e,
    _ => {
      resolve_structure_properties(ast, &mut e);

      e
    }
  }
}

fn gather_ascript_info_from_grammar(
  ast: &mut AScriptStore,
  e: &mut Vec<SherpaError>,
  prod_types: &mut ProductionTypesTable,
) {
  // Separate all bodies into a list of  of tuple of RuleId's and
  // Ascript reference nodes.

  let g = ast.g.clone();

  let normal_parse_bodies: Vec<(RuleId, Option<&Ascript>)> = ast
    .g
    .rules
    .iter()
    .filter_map(|(id, rule)| match g.clone().parse_productions.contains(&rule.prod_id) {
      true => {
        for function in &rule.reduce_fn_ids {
          if let ReduceFunctionType::Ascript(ascript) = g.reduce_functions.get(function).unwrap() {
            return Some((*id, Some(ascript)));
          }
        }
        Some((*id, None))
      }
      false => None,
    })
    .collect::<Vec<_>>();

  // For reduce function in each rule divide into those that resolve
  // into atomic types and those that don't (those that resolve into
  // productions). Add the types of the atomic functions to the
  // production types. Add any structs encountered into a separate
  // table, again adding these atomic struct types to the production
  // types.

  let mut struct_bodies: Vec<(RuleId, &Ascript)> = vec![];
  for (rule_id, ascript_option_fn) in normal_parse_bodies {
    if let Some(rule) = g.clone().rules.get(&rule_id) {
      if let Some(ascript_fn) = &ascript_option_fn {
        match &ascript_fn.ast {
          ASTNode::AST_Struct(ast_struct) => {
            let (id, mut sub_errors) = compile_struct_type(ast, ast_struct, rule);
            struct_bodies.push((rule_id, ascript_fn));
            add_production_type(prod_types, &rule, TaggedType {
              type_:        AScriptTypeVal::Struct(id),
              tag:          rule_id,
              symbol_index: 0,
            });
            e.append(&mut sub_errors);
          }
          ASTNode::AST_Statements(ast_stmts) => {
            let (sub_types, mut sub_errors) =
              compile_expression_type(ast, ast_stmts.statements.last().unwrap(), rule);

            for sub_type in sub_types {
              add_production_type(prod_types, &rule, sub_type);
            }

            e.append(&mut sub_errors);
          }
          _ => {}
        }
      } else {
        match rule.last_symbol_id() {
          SymbolID::Production(id, ..) => add_production_type(prod_types, &rule, TaggedType {
            type_:        AScriptTypeVal::UnresolvedProduction(id),
            tag:          rule_id,
            symbol_index: (rule.len - 1) as u32,
          }),
          _ => add_production_type(prod_types, &rule, TaggedType {
            type_:        AScriptTypeVal::Token,
            tag:          rule_id,
            symbol_index: (rule.len - 1) as u32,
          }),
        };
      }
    }
  }
}

fn add_production_type(
  prod_types: &mut ProductionTypesTable,
  rule: &Rule,
  new_return_type: TaggedType,
) {
  let table = prod_types.entry(rule.prod_id).or_insert_with(HashMap::new);

  match table.entry(new_return_type.clone()) {
    Entry::Occupied(mut entry) => {
      entry.get_mut().insert(new_return_type.into());
    }
    Entry::Vacant(entry) => {
      entry.insert(BTreeSet::from_iter(vec![new_return_type.into()]));
    }
  }
}

fn resolve_production_reduce_types(
  ast: &mut AScriptStore,
  e: &mut Vec<SherpaError>,
  mut prod_types: ProductionTypesTable,
) {
  let mut pending_prods = VecDeque::from_iter(ast.g.parse_productions.iter().cloned().rev());

  while let Some(prod_id) = pending_prods.pop_front() {
    debug_assert!(prod_types.contains_key(&prod_id), "All production should be accounted for");

    let mut resubmit = false;
    let mut new_map = HashMap::new();
    let mut vector_types = prod_types.remove(&prod_id).unwrap().into_iter().collect::<Vec<_>>();
    let scalar_types = vector_types.drain_filter(|(a, _)| !a.type_.is_vec()).collect::<Vec<_>>();

    if !scalar_types.is_empty() {
      use AScriptTypeVal::*;
      let (mut prime, mut prime_body_ids) = (TaggedType::default(), BTreeSet::new());

      let mut insert_production_types =
        |ast: &mut AScriptStore,
         foreign_prod_id: ProductionId,
         original: TaggedType,
         body_ids: BTreeSet<RuleId>| {
          if foreign_prod_id != prod_id {
            match ast.prod_types.get(&foreign_prod_id) {
              Some(types_) if !types_.is_empty() => {
                new_map.extend(types_.clone());
              }
              Some(_) => {
                panic!(
                  "Production [{}] does not produce any types",
                  ast.g.get_production_plain_name(&foreign_prod_id)
                )
              }
              _ => {
                // Remap the production type and resubmit
                new_map.insert(original, body_ids);
              }
            }

            true
          } else {
            false
          }
        };

      for (other, mut body_ids) in scalar_types {
        prime = match (&(prime.type_), &(other.type_)) {
          (Struct(typeA), Struct(typeB)) if typeA != typeB => {
            prime_body_ids.append(&mut body_ids);
            TaggedType {
              type_: GenericStruct(BTreeSet::from_iter(vec![prime, other])),
              ..Default::default()
            }
          }
          (Struct(_), GenericStruct(btree_set)) => {
            let mut btree_set = btree_set.clone();
            btree_set.insert(prime);
            prime_body_ids.append(&mut body_ids);
            TaggedType {
              type_: GenericStruct(BTreeSet::from_iter(btree_set)),
              ..Default::default()
            }
          }
          (GenericStruct(btree_set), Struct(_)) => {
            let mut btree_set = btree_set.clone();
            btree_set.insert(other);
            prime_body_ids.append(&mut body_ids);
            TaggedType {
              type_: GenericStruct(BTreeSet::from_iter(btree_set)),
              ..Default::default()
            }
          }
          (_, UnresolvedProduction(foreign_prod_id)) => {
            resubmit =
              resubmit.max(insert_production_types(ast, *foreign_prod_id, other, body_ids));
            prime
          }
          (UnresolvedProduction(foreign_prod_id), _) => {
            resubmit = resubmit.max(insert_production_types(
              ast,
              *foreign_prod_id,
              prime,
              prime_body_ids.clone(),
            ));
            other
          }
          (Undefined, _) => {
            prime_body_ids.append(&mut body_ids);
            other
          }
          (a, b) if a.is_same_type(&b) => {
            prime_body_ids.append(&mut body_ids);
            prime
          }
          (a, b) => {
            e.push(ErrIncompatibleProductionScalerTypes::new(
              prod_id,
              ast.g.clone(),
              (a.clone(), prime_body_ids.iter().cloned().collect()),
              (b.clone(), body_ids.iter().cloned().collect()),
              ast.get_type_names(),
            ));
            prime
          }
        }
      }

      if !prime.type_.is_undefined() {
        new_map.insert(prime, prime_body_ids);
      }
    }

    if !vector_types.is_empty() {
      use AScriptTypeVal::*;
      let (mut prime, mut prime_body_ids) = (TaggedType::default(), BTreeSet::new());
      let mut vector_types = VecDeque::from_iter(vector_types);

      let mut remap_vector = |mut known_types: BTreeSet<TaggedType>,
                              vector_types: &mut VecDeque<(TaggedType, BTreeSet<RuleId>)>|
       -> BTreeSet<TaggedType> {
        vector_types.extend(
          known_types
            .drain_filter(|t| matches!(t.into(), GenericVec(..)))
            .map(|t| (t.into(), BTreeSet::new()))
            .collect::<VecDeque<_>>(),
        );

        for production in known_types.drain_filter(|t| matches!(t.into(), UnresolvedProduction(..)))
        {
          if let UnresolvedProduction(foreign_prod_id) = production.type_.clone() {
            if foreign_prod_id != prod_id {
              match ast.prod_types.get(&foreign_prod_id) {
                Some(other_production_types) => {
                  new_map.insert(
                    TaggedType {
                      type_: AScriptTypeVal::GenericVec(Some(
                        other_production_types.keys().cloned().collect(),
                      )),
                      ..Default::default()
                    },
                    other_production_types.values().flatten().cloned().collect(),
                  );
                }
                None => {
                  new_map.insert(
                    TaggedType {
                      type_: AScriptTypeVal::GenericVec(Some(BTreeSet::from_iter(vec![
                        production,
                      ]))),
                      ..Default::default()
                    },
                    BTreeSet::new(),
                  );
                }
              }
              resubmit = true;
            }
          }
        }
        known_types
      };

      while let Some((other, mut body_ids)) = vector_types.pop_front() {
        prime = match (prime.type_, other.type_) {
          (GenericVec(Some(vecA)), GenericVec(Some(vecB))) => {
            // Check for compatibility, and extract productions from vectors
            let mut known_types = remap_vector(vecB, &mut vector_types);
            known_types.extend(vecA);
            prime_body_ids.append(&mut body_ids);
            TaggedType { type_: GenericVec(Some(known_types)), ..Default::default() }
          }
          (GenericVec(Some(vecA)), GenericVec(None)) => {
            prime_body_ids.append(&mut body_ids);
            TaggedType { type_: GenericVec(Some(vecA)), ..Default::default() }
          }
          (Undefined, GenericVec(Some(vecB))) | (GenericVec(None), GenericVec(Some(vecB))) => {
            let known_types = remap_vector(vecB, &mut vector_types);
            TaggedType { type_: GenericVec(Some(known_types)), ..Default::default() }
          }
          (Undefined, GenericVec(None)) | (GenericVec(None), GenericVec(None)) => {
            prime_body_ids.append(&mut body_ids);
            TaggedType { type_: GenericVec(None), ..Default::default() }
          }
          _ => unreachable!(
            "Failed Invariant: Only GenericVector types should be encountered at this point."
          ),
        }
      }

      if !prime.type_.is_undefined() {
        new_map.insert(prime, prime_body_ids);
      }
    }

    if resubmit {
      pending_prods.push_back(prod_id);
      prod_types.insert(prod_id, new_map);
    } else {
      // Only when the production is fully resolved do
      // we add the the types to the ast store.
      ast.prod_types.insert(prod_id, new_map);
    }
  }

  // Ensure all non-scanner productions have been added to the ascript data.
  debug_assert_eq!(ast.prod_types.len(), ast.g.parse_productions.len());

  // Do final check for incompatible types
  for prod_id in ast.prod_types.keys().cloned().collect::<Vec<_>>() {
    let mut vector_types = ast.prod_types.get(&prod_id).unwrap().iter().collect::<Vec<_>>();
    let scalar_types = vector_types.drain_filter(|(a, ..)| !a.type_.is_vec()).collect::<Vec<_>>();

    debug_assert!(
      !scalar_types
        .iter()
        .any(|(a, _)| matches!((*a).into(), AScriptTypeVal::UnresolvedProduction(_))),
      "Production [{}] has not been fully resolved \n{:#?}",
      ast.g.get_production_plain_name(&prod_id),
      ast
        .prod_types
        .get(&prod_id)
        .unwrap()
        .iter()
        .map(|(t, _)| { t.debug_string(Some(&ast.g)) })
        .collect::<Vec<_>>()
    );
    match (!vector_types.is_empty(), !scalar_types.is_empty()) {
      (true, true) => {
        e.push(ErrUnionOfScalarsAndVectors::new(
          ast.g.clone(),
          prod_id,
          scalar_types
            .iter()
            .flat_map(|(type_, bodies)| {
              bodies.iter().map(|b| ((*type_).into(), *b)).collect::<Vec<_>>()
            })
            .collect(),
          vector_types
            .iter()
            .flat_map(|(type_, bodies)| {
              bodies.iter().map(|b| ((*type_).into(), *b)).collect::<Vec<_>>()
            })
            .collect(),
          ast.get_type_names(),
        ));
      }
      (true, false) => {
        debug_assert!(
          vector_types.len() == 1,
          "Failed Invariant: All productions should have a single resolved type"
        );
        let (_type, tokens) = vector_types.into_iter().next().unwrap();
        match _type.into() {
          AScriptTypeVal::GenericVec(Some(_types)) => {
            let resolved_vector_type = get_specified_vector_from_generic_vec_values(
              &_types.iter().map(|v| v.into()).collect(),
            );
            if resolved_vector_type.is_undefined() {
              e.push(ErrIncompatibleProductionVectorTypes::new(
                prod_id,
                ast.g.clone(),
                _types.iter().cloned().collect(),
                ast.get_type_names(),
              ));
            } else {
              ast.prod_types.insert(
                prod_id,
                HashMap::from_iter(vec![(
                  TaggedType { type_: resolved_vector_type, ..Default::default() },
                  tokens.to_owned(),
                )]),
              );
            }
          }
          _ => {}
        }
      }
      (false, true) => {}
      _ => {}
    }
  }
}

fn resolve_structure_properties(ast: &mut AScriptStore, e: &mut Vec<SherpaError>) {
  let g = ast.g.clone();

  for struct_id in ast.structs.keys().cloned().collect::<Vec<_>>() {
    let bodies = ast.structs.get(&struct_id).unwrap().body_ids.clone();
    for rule_id in bodies {
      let rule = g.get_rule(&rule_id).unwrap();
      for function in &rule.reduce_fn_ids {
        if let ReduceFunctionType::Ascript(ascript) = g.reduce_functions.get(function).unwrap() {
          if let ASTNode::AST_Struct(ast_struct) = &ascript.ast {
            e.append(&mut compile_struct_props(ast, &struct_id, ast_struct, &rule).1);
          }
        }
      }
    }

    verify_property_presence(ast, &struct_id);
  }

  // Ensure each property entry has a resolved data type.
  for prop_id in ast.props.keys().cloned().collect::<Vec<_>>() {
    let type_val = ast.props.get(&prop_id).unwrap().type_val.clone();

    debug_assert!(
      get_resolved_type(ast, &type_val.clone().into()) == type_val.type_.clone(),
      "Assumption Failed: All prop types are resolved at this point"
    );

    // ast.props.get_mut(&prop_id).unwrap().type_val =
    //  TaggedType { type_: get_resolved_type(ast, &type_val.into()), ..Default::default() };
  }
}

pub(crate) fn verify_property_presence(ast: &mut AScriptStore, struct_id: &AScriptStructId) {
  let struct_ = ast.structs.get(&struct_id).unwrap();
  for prop_id in &struct_.prop_ids {
    let prop = ast.props.get_mut(&prop_id).unwrap();
    if prop.body_ids.len() != struct_.body_ids.len() {
      prop.optional = true;
    }
  }
}

/// Retrieve the resolved type of the base type. For most ascript types
/// this returns a clone of the `base_type`. For vectors and unresolved
/// productions types, this attempts to replace such types with resolved
/// versions
pub fn get_resolved_type(ascript: &AScriptStore, base_type: &AScriptTypeVal) -> AScriptTypeVal {
  match base_type {
    AScriptTypeVal::UnresolvedProduction(production_id) => {
      let Some(types) = ascript
        .prod_types
        .get(production_id)
        .and_then(|t| Some(t.keys().cloned().collect::<Vec<_>>())) else {
          return base_type.clone()
        };

      if types.len() == 1 {
        (&types[0]).into()
      } else if types
        .iter()
        .all(|t| matches!(t.into(), AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..)))
      {
        let nodes = types
          .iter()
          .flat_map(|t| match t.into() {
            AScriptTypeVal::Struct(_) => vec![t.clone()],
            AScriptTypeVal::GenericStruct(ids) => ids.iter().cloned().collect(),
            _ => vec![],
          })
          .collect::<BTreeSet<_>>();

        AScriptTypeVal::GenericStruct(nodes)
      } else {
        AScriptTypeVal::Any
      }
    }

    AScriptTypeVal::GenericVec(Some(_)) => {
      let contents = BTreeSet::from_iter(get_resolved_vec_contents(ascript, base_type));
      // Flatten the subtypes into one array and get the resulting type from that
      get_specified_vector_from_generic_vec_values(&contents)
    }

    _ => base_type.clone(),
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
    GenericStructVec(types) => types.iter().map(|t| t.into()).collect(),
    GenericVec(Some(types)) => {
      types.iter().flat_map(|t| get_resolved_vec_contents(ast, &t.into())).collect()
    }
    TokenVec => vec![Token],
    StringVec => vec![String(None)],
    UnresolvedProduction(_) => get_resolved_vec_contents(ast, &get_resolved_type(ast, base_type)),
    none_vec_type => {
      vec![none_vec_type.clone()]
    }
  }
}

pub fn compile_expression_type(
  ast: &mut AScriptStore,
  ast_expression: &ASTNode,
  rule: &Rule,
) -> (Vec<TaggedType>, Vec<SherpaError>) {
  use AScriptTypeVal::*;
  let mut errors = vec![];

  let types = match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      let (struct_type, mut error) = compile_struct_type(ast, ast_struct, rule);

      errors.append(&mut error);

      vec![TaggedType {
        symbol_index: 9999,
        tag:          rule.id,
        type_:        Struct(struct_type),
      }]
    }
    ASTNode::AST_Token(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        Token,
    }],
    ASTNode::AST_Add(box AST_Add { left, .. }) => {
      let (sub_types, mut sub_errors) = compile_expression_type(ast, left, rule);
      errors.append(&mut sub_errors);
      sub_types
    }
    ASTNode::AST_Vector(box AST_Vector { initializer, .. }) => {
      let mut types = BTreeSet::new();

      for node in initializer {
        let (sub_types, mut sub_errors) = compile_expression_type(ast, node, rule);

        for sub_type in sub_types {
          match (&sub_type).into() {
            GenericVec(sub_types) => match sub_types {
              Some(mut sub_type) => {
                types.append(&mut sub_type);
              }
              None => {}
            },
            // We ignore undefined types, since we can represent such types with an empty vector.
            Undefined => {}
            _ => {
              types.insert(sub_type);
            }
          }
        }

        errors.append(&mut sub_errors);
      }
      if types.is_empty() {
        vec![TaggedType {
          symbol_index: rule.syms.len() as u32,
          tag:          rule.id,
          type_:        GenericVec(None),
        }]
      } else {
        vec![TaggedType {
          symbol_index: rule.syms.len() as u32,
          tag:          rule.id,
          type_:        GenericVec(Some(types)),
        }]
      }
    }
    ASTNode::AST_STRING(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        String(None),
    }],
    ASTNode::AST_BOOL(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        Bool(None),
    }],
    ASTNode::AST_U8(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        U8(None),
    }],
    ASTNode::AST_U16(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        U16(None),
    }],
    ASTNode::AST_U32(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        U32(None),
    }],
    ASTNode::AST_U64(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        U64(None),
    }],
    ASTNode::AST_I8(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        I8(None),
    }],
    ASTNode::AST_I16(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        I16(None),
    }],
    ASTNode::AST_I32(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        I32(None),
    }],
    ASTNode::AST_I64(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        I64(None),
    }],
    ASTNode::AST_F32(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        F32(None),
    }],
    ASTNode::AST_F64(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        F64(None),
    }],
    ASTNode::AST_NUMBER(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        F64(None),
    }],
    ASTNode::AST_Member(..) => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        Undefined,
    }],
    ASTNode::AST_NamedReference(_) | ASTNode::AST_IndexReference(_) => {
      match get_body_symbol_reference(rule, ast_expression) {
        Some((_, sym_ref)) => match sym_ref.sym_id {
          SymbolID::Production(id, ..) => match ast.prod_types.get(&id) {
            Some(types) => types
              .keys()
              .map(|t| TaggedType {
                symbol_index: sym_ref.original_index,
                tag:          rule.id,
                type_:        t.type_.clone(),
              })
              .collect(),
            None => vec![TaggedType {
              symbol_index: sym_ref.original_index,
              tag:          rule.id,
              type_:        UnresolvedProduction(id),
            }],
          },
          _ => vec![TaggedType {
            symbol_index: sym_ref.original_index,
            tag:          rule.id,
            type_:        Token,
          }],
        },
        None => vec![TaggedType {
          symbol_index: rule.syms.len() as u32,
          tag:          rule.id,
          type_:        Undefined,
        }],
      }
    }
    _ => vec![TaggedType {
      symbol_index: rule.syms.len() as u32,
      tag:          rule.id,
      type_:        Undefined,
    }],
  };

  (types, errors)
}

/// Compiles a struct type from a production rule and
/// ascript struct node.
pub fn compile_struct_type(
  ast: &mut AScriptStore,
  ast_struct: &AST_Struct,
  rule: &Rule,
) -> (AScriptStructId, Vec<SherpaError>) {
  let mut errors = vec![];
  let mut types = vec![];
  let mut classes = vec![];
  let mut include_token = false;

  for prop in ast_struct.props.iter() {
    match prop {
      ASTNode::AST_TypeId(id) => types.push(id),
      ASTNode::AST_ClassId(id) => classes.push(id),
      ASTNode::AST_Token(..) => include_token = true,
      // Precompile property to ensure we gather all sub-structs;
      // We don't care about the actual value at this point.
      ASTNode::AST_Property(box prop) => {
        compile_expression_type(ast, &prop.value, rule);
      }
      _ => {}
    }
  }

  // Use the last type as the official type name of the struct.
  let type_name =
    if let Some(node) = types.last() { node.value.clone() } else { "unknown".to_string() }[2..]
      .to_string();

  let id = AScriptStructId::new(&type_name);

  // Validate struct type is singular
  match types.len() {
    2.. => {
      errors.push(SherpaError::grammar_err_multi_location {
        message:   "Struct Type Redefined".to_string(),
        locations: types
          .iter()
          .enumerate()
          .map(|(i, node)| {
            if i == 0 {
              SherpaError::grammar_err {
                message: "".to_string(),
                loc: node.Token(),
                inline_message: "First Defined Here".to_string(),
                path: Default::default(),
              }
            } else {
              SherpaError::grammar_err {
                message: "".to_string(),
                loc: node.Token(),
                inline_message: "Redefined Here".to_string(),
                path: Default::default(),
              }
            }
          })
          .collect::<Vec<_>>(),
      });
    }
    0 => errors.push(SherpaError::grammar_err {
      message: "Struct defined without a type name".to_string(),
      loc: ast_struct.Token(),
      inline_message: "".to_string(),
      path: Default::default(),
    }),
    _ => {}
  }
  match ast.structs.entry(id.clone()) {
    btree_map::Entry::Occupied(mut entry) => {
      let struct_ = entry.get_mut();
      struct_.body_ids.insert(rule.id);
      struct_.definition_locations.insert(ast_struct.Token());
      struct_.include_token = struct_.include_token.max(include_token);
    }
    btree_map::Entry::Vacant(entry) => {
      entry.insert(AScriptStruct {
        id,
        type_name,
        body_ids: BTreeSet::from_iter(vec![rule.id]),
        definition_locations: BTreeSet::from_iter(vec![ast_struct.Token()]),
        prop_ids: BTreeSet::new(),
        include_token,
      });
    }
  }

  (id.clone(), errors)
}

/// Completes the compilation of struct type by defining the properties
/// of a struct.
pub fn compile_struct_props(
  ast: &mut AScriptStore,
  id: &AScriptStructId,
  ast_struct: &AST_Struct,
  rule: &Rule,
) -> (AScriptTypeVal, Vec<SherpaError>) {
  let mut errors = vec![];

  // Check to see if this struct is already defined. If so, we'll
  // append new properties to it. otherwise we create a new
  // struct entry and add props.

  if ast.structs.get(&id).unwrap().type_name == "SetProd" {
    eprintln!(
      "Configuring SetProd:\nbody:{}\ncurr_struct:\n{:#?}\nprops:[\n{}\n]\nast:{:#?}",
      rule.blame_string(&ast.g),
      ast.structs.get(&id),
      ast
        .structs
        .get(&id)
        .and_then(|s| Some(
          s.prop_ids
            .iter()
            .map(|i| format!("{:#?}", ast.props.get(i)))
            .collect::<Vec<_>>()
            .join("\n")
        ))
        .unwrap_or_default(),
      ast_struct
    );
    print!("");
  }

  let mut prop_ids = BTreeSet::new();
  let mut include_token = false;

  for prop in &ast_struct.props {
    match prop {
      ASTNode::AST_Token(..) => include_token = true,
      ASTNode::AST_Property(box prop) => {
        let name = &prop.id;
        let prop_id = AScriptPropId::new(id.clone(), name);

        prop_ids.insert(prop_id.clone());

        for prop_type in compile_expression_type(ast, &prop.value, rule).0 {
          match ast.props.get_mut(&prop_id) {
            Some(existing) => {
              use AScriptTypeVal::*;
              match ((&existing.type_val).into(), (&prop_type).into()) {
                (Struct(typeA), Struct(typeB), ..) if typeA != typeB => {
                  existing.type_val = TaggedType {
                    type_: GenericStruct(BTreeSet::from_iter(vec![
                      existing.type_val.clone(),
                      prop_type,
                    ])),
                    ..Default::default()
                  };
                  existing.body_ids.insert(rule.id);
                }
                (GenericStruct(mut btree_set), Struct(_), ..) => {
                  btree_set.insert(prop_type);
                  existing.type_val =
                    TaggedType { type_: GenericStruct(btree_set), ..Default::default() };
                  existing.body_ids.insert(rule.id);
                }
                (Struct(_), GenericStruct(mut btree_set), ..) => {
                  btree_set.insert(existing.type_val.clone());
                  existing.type_val =
                    TaggedType { type_: GenericStruct(btree_set), ..Default::default() };
                  existing.body_ids.insert(rule.id);
                }
                (GenericStructVec(mut vecA), GenericStructVec(mut vecB), ..) => {
                  vecA.append(&mut vecB);
                  existing.type_val =
                    TaggedType { type_: GenericStructVec(vecA), ..Default::default() };
                  existing.body_ids.insert(rule.id);
                }
                (Undefined, _) => {
                  existing.body_ids.insert(rule.id);
                  existing.type_val = prop_type.to_owned();
                  existing.location = prop.value.Token();
                  existing.grammar_ref = rule.grammar_ref.clone();
                  existing.optional = true;
                }
                (_, Undefined) => {
                  existing.body_ids.insert(rule.id);
                  existing.optional = true;
                }
                (a, b) if a.is_same_type(&b) => {
                  existing.body_ids.insert(rule.id);
                }
                _ => {
                  errors.push(ErrPropRedefinition::new(
                    ast.structs.get(id).unwrap().type_name.clone(),
                    name.clone(),
                    existing.clone(),
                    AScriptProp {
                      type_val: prop_type.into(),
                      location: prop.value.Token(),
                      grammar_ref: rule.grammar_ref.clone(),
                      ..Default::default()
                    },
                  ));
                }
              }
            }
            _ => {
              ast.props.insert(prop_id.clone(), AScriptProp {
                type_val: prop_type.into(),
                body_ids: BTreeSet::from_iter(vec![rule.id]),
                location: prop.value.Token(),
                grammar_ref: rule.grammar_ref.clone(),
                ..Default::default()
              });
            }
          }
        }
      }
      _ => {}
    }
  }

  match ast.structs.entry(id.clone()) {
    btree_map::Entry::Occupied(mut entry) => {
      let struct_ = entry.get_mut();
      struct_.body_ids.insert(rule.id);
      struct_.definition_locations.insert(ast_struct.Token());
      struct_.prop_ids.append(&mut prop_ids);
      struct_.include_token = include_token || struct_.include_token;
    }
    btree_map::Entry::Vacant(_) => unreachable!("Struct should be defined at this point"),
  }

  (AScriptTypeVal::Struct(id.clone()), errors)
}

pub fn get_production_types(
  ast: &AScriptStore,
  prod_id: &ProductionId,
) -> BTreeSet<AScriptTypeVal> {
  ast.prod_types.get(prod_id).unwrap().keys().map(|t| t.into()).collect::<BTreeSet<_>>()
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
            AScriptTypeVal::Struct(_) => {
              vec![TaggedType { type_: n.clone(), ..Default::default() }]
            }
            AScriptTypeVal::GenericStruct(struct_ids) => struct_ids.iter().cloned().collect(),
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
        AScriptTypeVal::GenericStructVec(BTreeSet::from_iter(vec![TaggedType {
          type_: AScriptTypeVal::Struct(*id),
          ..Default::default()
        }]))
      }
      AScriptTypeVal::GenericStruct(ids) => {
        AScriptTypeVal::GenericStructVec(ids.iter().cloned().collect())
      }
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

pub fn get_body_symbol_reference<'a>(
  rule: &'a Rule,
  reference: &ASTNode,
) -> Option<(usize, &'a RuleSymbol)> {
  match reference {
    ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) => {
      get_named_body_ref(rule, value)
    }
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      get_indexed_body_ref(rule, value)
    }
    _ => None,
  }
}

pub fn get_named_body_ref<'a>(rule: &'a Rule, val: &str) -> Option<(usize, &'a RuleSymbol)> {
  if val == "first" {
    Some((0, rule.syms.first().unwrap()))
  } else if val == "last" {
    Some((rule.syms.len() - 1, rule.syms.last().unwrap()))
  } else {
    rule.syms.iter().enumerate().filter(|(_, s)| s.annotation == *val).last()
  }
}

pub fn get_indexed_body_ref<'a>(rule: &'a Rule, i: &f64) -> Option<(usize, &'a RuleSymbol)> {
  rule.syms.iter().enumerate().filter(|(_, s)| s.original_index == (*i - 1.0) as u32).last()
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

pub fn production_types_are_structs(production_types: &BTreeSet<AScriptTypeVal>) -> bool {
  production_types.iter().all(|t| matches!(t.clone(), AScriptTypeVal::Struct(..)))
}
