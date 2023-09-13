#![allow(non_snake_case)]
use super::types::{
  AScriptPropId,
  AScriptStore,
  AScriptStruct,
  AScriptStructId,
  AScriptTypeVal,
  NonTerminalTypesTable,
  TaggedType,
  ASCRIPT_FIRST_NODE_ID,
  ASCRIPT_LAST_NODE_ID,
};
use crate::{
  errors::{
    add_incompatible_nonterm_scalar_types_error,
    add_incompatible_nonterm_types_error,
    add_incompatible_nonterm_vector_types_error,
    add_prop_redefinition_error,
    ascript_error_class,
  },
  types::AScriptProp,
};
use sherpa_core::{
  parser::*,
  proxy::{OrderedMap, OrderedSet},
  Rule,
  *,
};
use std::{
  collections::{btree_map, BTreeSet, HashSet, VecDeque},
  fmt::format,
  vec,
};

pub(crate) fn compile_ascript_store(j: &mut Journal, ast: &mut AScriptStore, db: &ParserDatabase) -> SherpaResult<()> {
  let mut temp_nonterm_types = NonTerminalTypesTable::new();

  gather_ascript_info_from_grammar(j, ast, db, &mut temp_nonterm_types)?;

  resolve_nonterm_reduce_types(j, ast, db, temp_nonterm_types)?;

  if !j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
    resolve_structure_properties(j, ast, db);
  }

  SherpaResult::Ok(())
}

fn gather_ascript_info_from_grammar(
  j: &mut Journal,
  store: &mut AScriptStore,
  db: &ParserDatabase,
  nterm_types: &mut NonTerminalTypesTable,
) -> SherpaResult<()> {
  // Separate all bodies into a list of  of tuple of RuleId's and
  // Ascript reference nodes.

  let normal_parse_rules_refs = db
    .rules()
    .iter()
    .enumerate()
    .filter_map(|(i, rule)| (!rule.is_scanner).then(|| (DBRuleKey::from(i), rule)))
    .collect::<Vec<_>>();

  // For reduce function in each rule divide into those that resolve
  // into atomic types and those that don't (those that resolve into
  // nonterminals). Add the types of the atomic functions to the
  // non-terminal types. Add any structs encountered i    nto a separate
  // table, again adding these atomic struct types to the non-terminal
  // types.

  let mut struct_bodies: Vec<(DBRuleKey, &Ascript)> = vec![];
  for (rule_id, rule_ref) in normal_parse_rules_refs {
    match &rule_ref.rule.ast {
      Some(ASTToken::Defined(ascript)) => match &ascript.ast {
        ASTNode::AST_Struct(ast_struct) => {
          let id = compile_struct_type(j, store, db, ast_struct, rule_id);
          struct_bodies.push((rule_id, ascript));
          add_nonterm_type(nterm_types, &rule_ref, TaggedType {
            type_:        AScriptTypeVal::Struct(id),
            tag:          rule_id,
            symbol_index: 0,
          });
        }
        ASTNode::AST_Statements(ast_stmts) => {
          if let Some(sym) = ast_stmts.statements.last() {
            for sub_type in compile_expression_type(j, store, db, sym, rule_id) {
              add_nonterm_type(nterm_types, &rule_ref, sub_type);
            }
          } else {
            j.report_mut().add_error(SherpaError::Text("There should be at least one symbol".into()))
          }
        }
        ast_expr => {
          for sub_type in compile_expression_type(j, store, db, ast_expr, rule_id) {
            add_nonterm_type(nterm_types, &rule_ref, sub_type);
          }
        }
      },
      Some(ASTToken::ListEntry(_)) => {
        // Create a vector node that contains the first symbol of this rule.
        add_nonterm_type(nterm_types, &rule_ref, TaggedType {
          type_:        AScriptTypeVal::GenericVec(Some(OrderedSet::from_iter(convert_ref_result(
            get_indexed_body_ref(&rule_ref.rule, 0),
            store,
            db,
            rule_id,
          )))),
          tag:          rule_id,
          symbol_index: 0,
        })
      }
      Some(ASTToken::ListIterate(_)) => {
        // Create a vector that contains the first and last symbols of the
        // rule.
        // Create a vector node that contains the first symbol of this rule.
        let mut types = convert_ref_result(get_indexed_body_ref(&rule_ref.rule, 0), store, db, rule_id);
        types.append(&mut convert_ref_result(
          get_indexed_body_ref(&rule_ref.rule, rule_ref.rule.symbols.len() - 1),
          store,
          db,
          rule_id,
        ));
        add_nonterm_type(nterm_types, &rule_ref, TaggedType {
          type_:        AScriptTypeVal::GenericVec(Some(OrderedSet::from_iter(types))),
          tag:          rule_id,
          symbol_index: 0,
        })
      }
      _ => {
        match rule_ref.rule.symbols.last().map(|s| s.id) {
          Some(SymbolId::DBNonTerminal { key: id }) => add_nonterm_type(nterm_types, &rule_ref, TaggedType {
            type_:        AScriptTypeVal::UnresolvedNonTerminal(id),
            tag:          rule_id,
            symbol_index: (rule_ref.rule.symbols.len() - 1) as u32,
          }),
          Some(_) => add_nonterm_type(nterm_types, &rule_ref, TaggedType {
            type_:        AScriptTypeVal::Token,
            tag:          rule_id,
            symbol_index: (rule_ref.rule.symbols.len() - 1) as u32,
          }),
          None => j.report_mut().add_error(SherpaError::Text("[1] There should be at least one symbol".into())),
        };
      }
    }
  }

  SherpaResult::Ok(())
}

fn add_nonterm_type(nterm_types: &mut NonTerminalTypesTable, rule: &DBRule, new_return_type: TaggedType) {
  let table = nterm_types.entry(rule.nonterm).or_insert_with(OrderedMap::new);

  match table.entry(new_return_type.clone()) {
    btree_map::Entry::Occupied(mut entry) => {
      entry.get_mut().insert(new_return_type.into());
    }
    btree_map::Entry::Vacant(entry) => {
      entry.insert(BTreeSet::from_iter(vec![new_return_type.into()]));
    }
  }
}

/// Walks entire AST graph and resolves the types that any givin non-terminal
/// can reduce to. Creates errors if a non-terminal reduces to incompatible
/// types.
fn resolve_nonterm_reduce_types(
  j: &mut Journal,
  ast: &mut AScriptStore,
  db: &ParserDatabase,
  mut nterm_types: NonTerminalTypesTable,
) -> SherpaResult<()> {
  let mut pending_nterms = VecDeque::from_iter(db.parser_nonterms().into_iter().rev());

  while let Some(nterm) = pending_nterms.pop_front() {
    let mut resubmit = false;
    let mut new_map = OrderedMap::new();

    let Some(base_types) = nterm_types.remove(&nterm) else {
      j.report_mut().add_error(SherpaError::Text("Could not get type of nterm".into()));
      return Err(SherpaError::Text("Failure".to_string()));
    };

    let base_types = base_types.into_iter().collect::<Vec<_>>();
    let (vector_types, scalar_types) = base_types.into_iter().partition::<Vec<_>, _>(|(a, _)| a.type_.is_vec());

    if !scalar_types.is_empty() {
      use AScriptTypeVal::*;
      let (mut prime, mut prime_body_ids) = (TaggedType::default(), OrderedSet::new());

      let mut insert_nonterm_types =
        |ast: &mut AScriptStore, foreign_nterm: DBNonTermKey, original: TaggedType, body_ids: OrderedSet<DBRuleKey>| {
          if foreign_nterm != nterm {
            match ast.nonterm_types.get(&foreign_nterm) {
              Some(types_) if !types_.is_empty() => {
                new_map.extend(types_.clone());
              }
              Some(_) => {
                return Err(SherpaError::Text(format!(
                  "Non-terminal [{}] does not produce any types",
                  db.nonterm_friendly_name_string(foreign_nterm)
                )));
              }
              _ => {
                // Remap the non-terminal type and resubmit
                new_map.insert(original, body_ids);
              }
            }

            Ok(true)
          } else {
            Ok(false)
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
          (_, UnresolvedNonTerminal(foreign_nterm)) => {
            resubmit = resubmit.max(insert_nonterm_types(ast, *foreign_nterm, other, body_ids)?);
            prime
          }
          (UnresolvedNonTerminal(foreign_nterm), _) => {
            resubmit = resubmit.max(insert_nonterm_types(ast, *foreign_nterm, prime, prime_body_ids.clone())?);
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
            add_incompatible_nonterm_scalar_types_error(
              j,
              ast,
              db,
              &nterm,
              (a.clone(), prime_body_ids.iter().cloned().collect()),
              (b.clone(), body_ids.iter().cloned().collect()),
            );
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

      let mut remap_vector = |known_types: BTreeSet<TaggedType>,
                              vector_types: &mut VecDeque<(TaggedType, BTreeSet<DBRuleKey>)>|
       -> BTreeSet<TaggedType> {
        let (vectors, known_types) = known_types.into_iter().partition::<Vec<_>, _>(|t| matches!(t.into(), GenericVec(..)));

        vector_types.extend(vectors.into_iter().map(|t| (t.into(), BTreeSet::new())).collect::<VecDeque<_>>());

        let (unresolved_nterms, known_types) =
          known_types.into_iter().partition::<BTreeSet<_>, _>(|t| matches!(t.into(), UnresolvedNonTerminal(..)));

        for nonterminal in unresolved_nterms {
          if let UnresolvedNonTerminal(foreign_nterm) = nonterminal.type_.clone() {
            if foreign_nterm != nterm {
              match ast.nonterm_types.get(&foreign_nterm) {
                Some(other_nonterm_types) => {
                  new_map.insert(
                    TaggedType {
                      type_: AScriptTypeVal::GenericVec(Some(other_nonterm_types.keys().cloned().collect())),
                      ..Default::default()
                    },
                    other_nonterm_types.values().flatten().cloned().collect(),
                  );
                }
                None => {
                  new_map.insert(
                    TaggedType {
                      type_: AScriptTypeVal::GenericVec(Some(BTreeSet::from_iter(vec![nonterminal]))),
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
            // Check for compatibility, and extract nonterminals from vectors
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
          _ => {
            return Err(SherpaError::Text(format!(
              "Failed Invariant: Only GenericVector types should be encountered at this point."
            )))
          }
        }
      }

      if !prime.type_.is_undefined() {
        new_map.insert(prime, prime_body_ids);
      }
    }

    // Only when the non-terminal is fully resolved do
    // we add the the types to the ast store.resolve_structure_properties.
    // Otherwise, the non-terminal is resubmitted to resolve unresolved derived
    // types.
    if resubmit {
      pending_nterms.push_back(nterm);
      nterm_types.insert(nterm, new_map);
    } else {
      ast.nonterm_types.insert(nterm, new_map);
    }
  }

  // Ensure all non-scanner nonterminals have been added to the ascript data.
  //debug_assert_eq!(ast.nonterm_types.len(), db.parser_nonterms().len());

  // Do final check for incompatible types
  for nterm in ast.nonterm_types.keys().cloned().collect::<Vec<_>>() {
    if let Some(vector_types) = ast.nonterm_types.get(&nterm) {
      let vector_types = vector_types.iter().collect::<Vec<_>>();
      let (vector_types, scalar_types) = vector_types.into_iter().partition::<Vec<_>, _>(|(a, ..)| a.type_.is_vec());

      /*   debug_assert!(
        !scalar_types.iter().any(|(a, _)| matches!((*a).into(), AScriptTypeVal::UnresolvedNonTerminal(_))),
        "Non-terminal [{}] has not been fully resolved \n{:#?}",
        db.nonterm_friendly_name_string(nterm),
        ast.nonterm_types.get(&nterm).unwrap().iter().map(|(t, _)| { t.debug_string() }).collect::<Vec<_>>()
      ); */
      match (!vector_types.is_empty(), !scalar_types.is_empty()) {
        (true, true) => {
          add_incompatible_nonterm_types_error(
            j,
            ast,
            db,
            &nterm,
            scalar_types
              .iter()
              .flat_map(|(type_, bodies)| bodies.iter().map(|b| ((*type_).into(), *b)).collect::<Vec<_>>())
              .collect(),
            vector_types
              .iter()
              .flat_map(|(type_, bodies)| bodies.iter().map(|b| ((*type_).into(), *b)).collect::<Vec<_>>())
              .collect(),
          );
        }
        (true, false) => {
          debug_assert!(vector_types.len() == 1, "Failed Invariant: All nonterminals should have a single resolved type");
          let (_type, tokens) = vector_types.into_iter().next().unwrap();
          match _type.into() {
            AScriptTypeVal::GenericVec(Some(_types)) => {
              let resolved_vector_type = get_specified_vector_from_generic_vec_values(&_types.iter().map(|v| v.into()).collect());
              if resolved_vector_type.is_undefined() {
                add_incompatible_nonterm_vector_types_error(j, ast, db, &nterm, _types);
              } else {
                ast.nonterm_types.insert(
                  nterm,
                  OrderedMap::from_iter(vec![(
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
    } else {
      j.report_mut().add_error(SherpaError::Text(format!("Could not extract nterm type")));
    }
  }

  Ok(())
}

fn resolve_structure_properties(j: &mut Journal, store: &mut AScriptStore, db: &ParserDatabase) {
  for struct_id in store.structs.keys().cloned().collect::<Vec<_>>() {
    if let Some(strct) = store.structs.get(&struct_id) {
      let rules = strct.rule_ids.clone();
      for rule_id in rules {
        let rule = db.rule(rule_id);
        match &rule.ast {
          Some(ASTToken::Defined(ast)) => match &ast.ast {
            ASTNode::AST_Struct(ast_struct) => match compile_struct_props(j, store, db, &struct_id, ast_struct, rule_id) {
              Ok(_) => {}
              Err(err) => j.report_mut().add_error(err),
            },
            _ => {}
          },
          _ => {}
        }
      }

      if !j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
        verify_property_presence(store, &struct_id);
      }
    } else {
      j.report_mut().add_error(SherpaError::Text(format!("Could not extract nterm type")));
    }
  }

  if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
    return;
  }

  let mut undefined_props = HashSet::new();

  // Ensure each property entry has a resolved data type.
  for prop_id in store.props.keys().cloned().collect::<Vec<_>>() {
    if let Some(prop) = store.props.get(&prop_id) {
      match get_resolved_type(store, &prop.type_val.clone().into()) {
        AScriptTypeVal::Undefined => {
          undefined_props.insert(prop_id.clone());
          // Property is undefined and should be removed from its respective
          // struct. A warning should also be generated indicating the
          // type is ignored.
          eprintln!("Warning, the property \n{}\n is undefined and will be ignored", prop.loc.blame(0, 0, "Test", None));
        }
        type_ => {
          store.props.get_mut(&prop_id).unwrap().type_val = TaggedType { type_, ..Default::default() };
        }
      }
    }
  }
  // Remove undefined properties
  for struct_ in store.structs.values_mut() {
    let new_ids = struct_.prop_ids.iter().cloned().filter(|id| !undefined_props.contains(&id)).collect();
    struct_.prop_ids = new_ids;
  }
}

pub(crate) fn verify_property_presence(ast: &mut AScriptStore, struct_id: &AScriptStructId) {
  if let Some(struct_) = ast.structs.get(&struct_id) {
    for prop_id in &struct_.prop_ids {
      if let Some(prop) = ast.props.get_mut(&prop_id) {
        if prop.rule_ids.len() != struct_.rule_ids.len() {
          prop.optional = true;
        }
      }
    }
  }
}

/// Retrieve the resolved type of the base type. For most ascript types
/// this returns a clone of the `base_type`. For vectors and unresolved
/// nonterminals types, this attempts to replace such types with resolved
/// versions
pub fn get_resolved_type(ascript: &AScriptStore, base_type: &AScriptTypeVal) -> AScriptTypeVal {
  match base_type {
    AScriptTypeVal::UnresolvedNonTerminal(nonterm_id) => {
      let Some(types) = ascript.nonterm_types.get(nonterm_id).and_then(|t| Some(t.keys().cloned().collect::<Vec<_>>())) else {
        return base_type.clone();
      };

      if types.len() == 1 {
        (&types[0]).into()
      } else if types.iter().all(|t| matches!(t.into(), AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStruct(..))) {
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
      // Flatten the subtypes into one array and get the resulting type from
      // that
      get_specified_vector_from_generic_vec_values(&contents)
    }

    _ => base_type.clone(),
  }
}

pub fn get_resolved_vec_contents(store: &AScriptStore, base_type: &AScriptTypeVal) -> Vec<AScriptTypeVal> {
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
    GenericVec(Some(types)) => types.iter().flat_map(|t| get_resolved_vec_contents(store, &t.into())).collect(),
    TokenVec => vec![Token],
    StringVec => vec![String(None)],
    UnresolvedNonTerminal(_) => get_resolved_vec_contents(store, &get_resolved_type(store, base_type)),
    none_vec_type => {
      vec![none_vec_type.clone()]
    }
  }
}

pub fn compile_expression_type(
  j: &mut Journal,
  store: &mut AScriptStore,
  db: &ParserDatabase,
  ast_expression: &ASTNode,
  rule_id: DBRuleKey,
) -> Vec<TaggedType> {
  use AScriptTypeVal::*;

  let rule = db.rule(rule_id);
  let rule_len = rule.symbols.len();

  let types = match ast_expression {
    ASTNode::AST_Struct(ast_struct) => {
      let struct_type = compile_struct_type(j, store, db, ast_struct, rule_id);

      vec![TaggedType {
        symbol_index: 9999,
        tag:          rule_id,
        type_:        Struct(struct_type),
      }]
    }
    ASTNode::AST_Token(..) => vec![TaggedType {
      symbol_index: rule.symbols.len() as u32,
      tag:          rule_id,
      type_:        Token,
    }],
    ASTNode::AST_Add(box AST_Add { left, .. }) => compile_expression_type(j, store, db, left, rule_id),
    ASTNode::AST_Vector(box AST_Vector { initializer, .. }) => {
      let mut types = BTreeSet::new();

      for node in initializer {
        for sub_type in compile_expression_type(j, store, db, node, rule_id) {
          match (&sub_type).into() {
            GenericVec(sub_types) => match sub_types {
              Some(mut sub_type) => {
                types.append(&mut sub_type);
              }
              None => {}
            },
            // We ignore undefined types, since we can represent such types with
            // an empty vector.
            Undefined => {}
            _ => {
              types.insert(sub_type);
            }
          }
        }
      }
      if types.is_empty() {
        vec![TaggedType {
          symbol_index: rule_len as u32,
          tag:          rule_id,
          type_:        GenericVec(None),
        }]
      } else {
        vec![TaggedType {
          symbol_index: rule_len as u32,
          tag:          rule_id,
          type_:        GenericVec(Some(types)),
        }]
      }
    }
    ASTNode::AST_STRING(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        String(None),
    }],
    ASTNode::AST_BOOL(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        Bool(None),
    }],
    ASTNode::AST_U8(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        U8(None),
    }],
    ASTNode::AST_U16(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        U16(None),
    }],
    ASTNode::AST_U32(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        U32(None),
    }],
    ASTNode::AST_U64(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        U64(None),
    }],
    ASTNode::AST_I8(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        I8(None),
    }],
    ASTNode::AST_I16(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        I16(None),
    }],
    ASTNode::AST_I32(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        I32(None),
    }],
    ASTNode::AST_I64(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        I64(None),
    }],
    ASTNode::AST_F32(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        F32(None),
    }],
    ASTNode::AST_F64(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        F64(None),
    }],
    ASTNode::AST_NUMBER(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        F64(None),
    }],
    ASTNode::AST_Member(..) => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        Undefined,
    }],
    ASTNode::AST_NamedReference(_) | ASTNode::AST_IndexReference(_) => {
      convert_ref_result(get_body_symbol_reference(&db, rule_id, ast_expression), store, db, rule_id)
    }
    _ => vec![TaggedType {
      symbol_index: rule_len as u32,
      tag:          rule_id,
      type_:        Undefined,
    }],
  };

  types
}

/// A rule symbols and it's offset based on a reference value
/// e.g `$name` or `$3`
type RefResult = Option<(usize, SymbolRef)>;

fn convert_ref_result(
  ref_result: RefResult,
  store: &mut AScriptStore,
  db: &ParserDatabase,
  rule_id: DBRuleKey,
) -> Vec<TaggedType> {
  use AScriptTypeVal::*;
  let rule = db.rule(rule_id);
  match ref_result {
    Some((index, SymbolRef { id, .. })) => match id {
      SymbolId::DBNonTerminal { key } => match store.nonterm_types.get(&key) {
        Some(types) => types
          .keys()
          .map(|t| TaggedType {
            symbol_index: index as u32,
            tag:          rule_id,
            type_:        t.type_.clone(),
          })
          .collect(),
        None => vec![TaggedType {
          symbol_index: index as u32,
          tag:          rule_id,
          type_:        UnresolvedNonTerminal(key),
        }],
      },
      _ => {
        vec![TaggedType { symbol_index: index as u32, tag: rule_id, type_: Token }]
      }
    },
    None => vec![TaggedType {
      symbol_index: rule.symbols.len() as u32,
      tag:          rule_id,
      type_:        Undefined,
    }],
  }
}

/// Compiles a struct type from a non-terminal rule and
/// ascript struct node.
pub fn compile_struct_type(
  j: &mut Journal,
  store: &mut AScriptStore,
  db: &ParserDatabase,
  ast_struct: &AST_Struct,
  rule_id: DBRuleKey,
) -> AScriptStructId {
  let mut include_token = false;

  for prop in ast_struct.props.iter() {
    match prop {
      ASTNode::AST_Token(..) => include_token = true,
      // Precompile property to ensure we gather all sub-structs;
      // We don't care about the actual value at this point.
      ASTNode::AST_Property(box prop) => {
        if let Some(value) = &prop.value {
          compile_expression_type(j, store, db, value, rule_id);
        }
      }
      _ => {}
    }
  }

  // Use the last type as the official type name of the struct.
  let type_name = get_struct_name_from_node(ast_struct);

  let id = AScriptStructId::new(&type_name);

  match store.structs.entry(id.clone()) {
    btree_map::Entry::Vacant(entry) => {
      entry.insert(AScriptStruct {
        id,
        type_name,
        rule_ids: BTreeSet::from_iter(vec![rule_id]),
        definition_locations: BTreeSet::from_iter(vec![ast_struct.tok.clone()]),
        prop_ids: BTreeSet::new(),
        tokenized: include_token,
      });
    }
    btree_map::Entry::Occupied(mut entry) => {
      let struct_ = entry.get_mut();
      struct_.rule_ids.insert(rule_id);
      struct_.definition_locations.insert(ast_struct.tok.clone());
      struct_.tokenized = struct_.tokenized.max(include_token);
    }
  }

  id.clone()
}

/// Completes the compilation of struct type by defining the properties
/// of a struct.
#[track_caller]
pub fn compile_struct_props(
  j: &mut Journal,
  store: &mut AScriptStore,
  db: &ParserDatabase,
  id: &AScriptStructId,
  ast: &AST_Struct,
  rule_id: DBRuleKey,
) -> SherpaResult<AScriptTypeVal> {
  // Check to see if this struct is already defined. If so, we'll
  // append new properties to it. otherwise we create a new
  // struct entry and add props.

  let mut prop_ids = BTreeSet::new();
  let mut include_token = false;
  let rule = db.rule(rule_id);

  for prop in &ast.props {
    match prop {
      ASTNode::AST_Token(..) => include_token = true,
      ASTNode::AST_Property(box prop) => {
        let name = &prop.id;
        let prop_id = AScriptPropId::new(id.clone(), name);

        prop_ids.insert(prop_id.clone());

        for mut prop_type in match &prop.value {
          Some(value) => compile_expression_type(j, store, db, value, rule_id),
          _ => convert_ref_result(get_named_body_ref(db, db.rule(rule_id), prop.id.as_str()), store, db, rule_id),
        } {
          if prop_type.type_.is_vec() {
            prop_type.type_ = get_specified_vector_from_generic_vec_values(&prop_type.type_.get_subtypes().into_iter().collect())
          }

          match store.props.get_mut(&prop_id) {
            Some(existing) => {
              use AScriptTypeVal::*;
              match ((&existing.type_val).into(), (&prop_type).into()) {
                (Struct(typeA), Struct(typeB), ..) if typeA != typeB => {
                  existing.type_val = TaggedType {
                    type_: GenericStruct(BTreeSet::from_iter(vec![existing.type_val.clone(), prop_type])),
                    ..Default::default()
                  };
                  existing.rule_ids.insert(rule_id);
                }
                (GenericStruct(mut btree_set), Struct(_), ..) => {
                  btree_set.insert(prop_type);
                  existing.type_val = TaggedType { type_: GenericStruct(btree_set), ..Default::default() };
                  existing.rule_ids.insert(rule_id);
                }
                (Struct(_), GenericStruct(mut btree_set), ..) => {
                  btree_set.insert(existing.type_val.clone());
                  existing.type_val = TaggedType { type_: GenericStruct(btree_set), ..Default::default() };
                  existing.rule_ids.insert(rule_id);
                }
                (GenericStructVec(mut vecA), GenericStructVec(mut vecB), ..) => {
                  vecA.append(&mut vecB);
                  existing.type_val = TaggedType { type_: GenericStructVec(vecA), ..Default::default() };
                  existing.rule_ids.insert(rule_id);
                }
                (Undefined, _) => {
                  existing.rule_ids.insert(rule_id);
                  existing.type_val = prop_type.to_owned();
                  existing.loc = prop.tok.clone();
                  existing.grammar_ref = rule.g_id.clone();
                  existing.optional = true;
                }
                (_, Undefined) => {
                  existing.rule_ids.insert(rule_id);
                  existing.optional = true;
                }
                (a, b) if a.is_same_type(&b) => {
                  existing.rule_ids.insert(rule_id);
                }
                _ => {
                  add_prop_redefinition_error(
                    j,
                    db,
                    store.structs.get(id).unwrap().type_name.clone(),
                    name.clone(),
                    existing.clone(),
                    AScriptProp {
                      type_val: prop_type.into(),
                      loc: prop.tok.clone(),
                      grammar_ref: rule.g_id.clone(),
                      ..Default::default()
                    },
                  );
                }
              }
            }
            _ => {
              store.props.insert(prop_id.clone(), AScriptProp {
                type_val: prop_type.into(),
                rule_ids: BTreeSet::from_iter(vec![rule_id]),
                loc: prop.tok.clone(),
                grammar_ref: rule.g_id.clone(),
                ..Default::default()
              });
            }
          }
        }
      }
      _ => {}
    }
  }

  match store.structs.entry(id.clone()) {
    btree_map::Entry::Occupied(mut entry) => {
      let struct_ = entry.get_mut();
      struct_.rule_ids.insert(rule_id);
      struct_.definition_locations.insert(ast.tok.clone());
      struct_.prop_ids.append(&mut prop_ids);
      struct_.tokenized = include_token || struct_.tokenized;
    }
    btree_map::Entry::Vacant(_) => {
      return Err(SherpaError::SourceError {
        loc:        ast.tok.clone(),
        path:       rule.g_id.path.to_path(db.string_store()),
        id:         (ascript_error_class(), 0, "undefined-struct").into(),
        msg:        "Struct should be defined at this point".into(),
        inline_msg: Default::default(),
        ps_msg:     Default::default(),
        severity:   SherpaErrorSeverity::Critical,
      });
    }
  }

  Ok(AScriptTypeVal::Struct(id.clone()))
}

pub fn get_nonterm_types(store: &AScriptStore, nterm: &DBNonTermKey) -> BTreeSet<AScriptTypeVal> {
  store.nonterm_types.get(nterm).unwrap().keys().map(|t| t.into()).collect::<BTreeSet<_>>()
}

/// Returns a specified vector type from a generic vector
pub fn get_specified_vector_from_generic_vec_values(vals: &BTreeSet<AScriptTypeVal>) -> AScriptTypeVal {
  if vals.len() > 1 {
    if vals
      .iter()
      .all(|t| matches!(t, AScriptTypeVal::Struct(..) | AScriptTypeVal::GenericStructVec(..) | AScriptTypeVal::GenericStruct(..)))
    {
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
    } else if vals.iter().all(|t| matches!(t, AScriptTypeVal::String(..) | AScriptTypeVal::StringVec)) {
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
    match vals.first() {
      Some(AScriptTypeVal::Struct(id)) => AScriptTypeVal::GenericStructVec(BTreeSet::from_iter(vec![TaggedType {
        type_: AScriptTypeVal::Struct(*id),
        ..Default::default()
      }])),
      Some(AScriptTypeVal::GenericStruct(ids)) => AScriptTypeVal::GenericStructVec(ids.iter().cloned().collect()),
      Some(AScriptTypeVal::U8(..)) => AScriptTypeVal::U8Vec,
      Some(AScriptTypeVal::U16(..)) => AScriptTypeVal::U16Vec,
      Some(AScriptTypeVal::U32(..)) => AScriptTypeVal::U32Vec,
      Some(AScriptTypeVal::U64(..)) => AScriptTypeVal::U64Vec,
      Some(AScriptTypeVal::I8(..)) => AScriptTypeVal::I8Vec,
      Some(AScriptTypeVal::I16(..)) => AScriptTypeVal::I16Vec,
      Some(AScriptTypeVal::I32(..)) => AScriptTypeVal::I32Vec,
      Some(AScriptTypeVal::I64(..)) => AScriptTypeVal::I64Vec,
      Some(AScriptTypeVal::F32(..)) => AScriptTypeVal::F32Vec,
      Some(AScriptTypeVal::F64(..)) => AScriptTypeVal::F64Vec,
      Some(AScriptTypeVal::Token) => AScriptTypeVal::TokenVec,
      Some(AScriptTypeVal::String(..)) => AScriptTypeVal::StringVec,
      _ => AScriptTypeVal::Undefined,
    }
  }
}

/// Returns the symbol annotated with the givin reference name
pub fn get_body_symbol_reference<'a>(db: &ParserDatabase, rule_id: DBRuleKey, reference: &ASTNode) -> RefResult {
  match reference {
    ASTNode::AST_NamedReference(box AST_NamedReference { value, .. }) => get_named_body_ref(db, db.rule(rule_id), value),
    ASTNode::AST_IndexReference(box AST_IndexReference { value, .. }) => {
      get_indexed_body_ref(db.rule(rule_id), (*value - 1) as usize)
    }
    _ => None,
  }
}

/// Takes an index value from [0..n], where n is the number of symbols in the
/// rule, and matches it to a symbol within the givin rule, returning the
/// matching symbol and its original index.
///
/// Note: This matches the _original_ index of the symbol, which may be
/// different than the actual positional index of the symbol.
///
/// Example: In the case of a derived rule `<> A > B D` generated from the
/// original rule `<> A > B C? D`, the symbol B has an original index of 0, and
/// the symbol D has an original index of 2, since the removed symbol `C`
/// occupied the original index 1.
///
/// The returned index will be in the range [0..n)
///
/// Returns `None` if the index is greater then the number of symbols.  
pub fn get_indexed_body_ref(rule: &Rule, i: usize) -> RefResult {
  rule.symbols.iter().enumerate().filter(|(_, sym_ref)| sym_ref.original_index == i as u32).map(|(a, b)| (a, b.clone())).last()
}

pub fn get_named_body_ref(db: &ParserDatabase, rule: &Rule, val: &str) -> RefResult {
  if val == ASCRIPT_FIRST_NODE_ID {
    Some((0, rule.symbols.first()?.clone()))
  } else if val == ASCRIPT_LAST_NODE_ID {
    Some((rule.symbols.len(), rule.symbols.last()?.clone()))
  } else {
    match rule.symbols.iter().enumerate().filter(|(_, SymbolRef { annotation: a, .. })| *a == val.to_token()).last() {
      // The symbols annotation matched
      Some((i, result)) => Some((i, result.clone())),
      // The nonterminal name matched.
      _ => rule
        .symbols
        .iter()
        .enumerate()
        .filter_map(|(i, sym)| match sym.id {
          SymbolId::DBNonTerminal { key } => (db.nonterm_friendly_name(key) == val.to_token()).then(|| (i, sym.clone())),
          _ => None,
        })
        .last(),
    }
  }
}

pub fn get_struct_name_from_node(ast_struct: &AST_Struct) -> String {
  ast_struct.typ.to_string()[2..].to_string()
}

pub fn get_struct_type_from_node(ast_struct: &AST_Struct) -> AScriptTypeVal {
  AScriptTypeVal::Struct(AScriptStructId::new(&get_struct_name_from_node(ast_struct)))
}

pub fn nonterminal_types_are_structs(nonterm_types: &BTreeSet<AScriptTypeVal>) -> bool {
  nonterm_types.iter().all(|t| matches!(t.clone(), AScriptTypeVal::Struct(..)))
}
