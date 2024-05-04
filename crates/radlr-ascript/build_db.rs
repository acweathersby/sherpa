use std::{
  collections::{BTreeSet, HashSet, VecDeque},
  rc::Rc,
};

use radlr_core::{
  parser::{ASTNode, AST_Struct},
  proxy::{OrderedMap, OrderedSet},
  ASTToken,
  CachedString,
  DBNonTermKey,
  DBRuleKey,
  GrammarIdentities,
  Item,
  ParserDatabase,
  RadlrDatabase,
  RadlrError,
  RadlrResult,
  SymbolRef,
};

use crate::{errors::add_prop_redefinition_error, types::*};

pub fn build_database(db: RadlrDatabase) -> AscriptDatabase {
  let mut adb = AscriptDatabase {
    errors:      Default::default(),
    structs:     Default::default(),
    rules:       Default::default(),
    types:       Default::default(),
    any_types:   Default::default(),
    any_type_lu: Default::default(),
    db:          db.into_internal(),
  };

  let db = adb.db.clone();
  let db = &db;

  extract_structs(db, &mut adb);

  let nonterm_types = resolve_nonterm_types(db, &mut adb);

  if adb.errors.is_empty() {
    match resolve_expressions(&mut adb, nonterm_types) {
      Ok(()) => {
        fill_out_rules(&mut adb);

        //add_token_nodes(&mut adb);

        collect_types(&mut adb);
      }
      Err(missing_nonterm_definition) => adb.errors.push(RadlrError::Text(format!(
        "Could not resolve Node type for Non-Term [{}]",
        db.nonterm_friendly_name(missing_nonterm_definition).to_string(db.string_store())
      ))),
    }
  }

  adb
}

fn fill_out_rules(adb: &mut AscriptDatabase) {
  let AscriptDatabase { rules, structs, db, .. } = adb;

  for rule in &mut rules.0 {
    match rule {
      AscriptRule::Struct(_, init) => {
        if let Some(s) = structs.get(&init.name) {
          for (prop_name, prop) in &s.properties.0 {
            match init.props.0.entry(*prop_name) {
              std::collections::btree_map::Entry::Vacant(init_prop) => {
                init_prop.insert(Initializer {
                  ty:           prop.ty,
                  name:         *prop_name,
                  output_graph: None,
                  ast:          None,
                  g_id:         db.root_grammar_id,
                });
              }
              std::collections::btree_map::Entry::Occupied(_) => {}
            }
          }
        }
      }
      _ => {}
    }
  }
}

#[allow(unused)]
fn add_token_nodes(adb: &mut AscriptDatabase) {
  let AscriptDatabase { rules, structs, types, db, .. } = adb;

  let prop_name = StringId("tok".intern(db.string_store()));
  let tok_type = AscriptType::Scalar(AscriptScalarType::Token);
  for (_, s) in &mut structs.0 {
    if !s.has_token {
      s.has_token = true;
      s.properties.insert(prop_name, AscriptProp {
        is_optional: false,
        name:        "tok".to_string(),
        ty:          tok_type,
        tok:         Default::default(),
        g_id:        db.root_grammar_id,
      });
    }
  }

  for rule in &mut rules.0 {
    match rule {
      AscriptRule::Struct(_, init) => match init.props.0.entry(prop_name) {
        std::collections::btree_map::Entry::Vacant(prop) => {
          prop.insert(Initializer {
            ty:           tok_type,
            name:         prop_name,
            output_graph: Some(GraphNode::TokRule(AscriptType::Scalar(AscriptScalarType::Token))),
            ast:          None,
            g_id:         db.root_grammar_id,
          });
        }
        std::collections::btree_map::Entry::Occupied(_) => {}
      },
      _ => {}
    }
  }
}

/// Collect non-struct type information
fn collect_types(adb: &mut AscriptDatabase) {
  let AscriptDatabase { rules, structs, types, .. } = adb;

  for (_, str) in &structs.0 {
    for (_, prop) in &str.properties.0 {
      extract_type_data(prop.ty.to_cardinal(), types);
    }
  }
  for rule in &mut rules.0 {
    match rule {
      AscriptRule::Expression(_, init)
      | AscriptRule::ListInitial(_, init)
      | AscriptRule::ListContinue(_, init)
      | AscriptRule::LastSymbol(_, init) => match &init.output_graph {
        Some(node) => {
          extract_type_data(node.get_type().to_cardinal(), types);
        }
        None => {
          panic!("GraphNode should be resolved");
        }
      },

      _ => {}
    }
  }
}

fn extract_type_data(ty: AscriptType, types: &mut AscriptTypes) {
  match ty {
    ty @ AscriptType::Aggregate(agg) => {
      types.0.insert(ty);
      match agg {
        AscriptAggregateType::Map { val_type, .. } | AscriptAggregateType::Vec { val_type } => {
          extract_type_data(AscriptType::Scalar(val_type).to_cardinal(), types)
        }
      }
    }
    AscriptType::Scalar(AscriptScalarType::Struct(..)) | AscriptType::Scalar(AscriptScalarType::Token) => {}
    ty => {
      types.0.insert(ty);
    }
  }
}

pub fn extract_structs(db: &ParserDatabase, adb: &mut AscriptDatabase) {
  for (id, db_rule) in db.rules().iter().enumerate().filter(|(_, r)| !r.is_scanner) {
    let rule = &db_rule.rule;
    let g_id = db_rule.rule.g_id;

    let ast_rule = match &rule.ast {
      None => AscriptRule::LastSymbol(id, Initializer {
        ty: AscriptType::Undefined,
        name: Default::default(),
        output_graph: None,
        ast: None,
        g_id,
      }),
      Some(ast) => match ast {
        ASTToken::Defined(ast) => match &ast.ast {
          ASTNode::AST_Struct(strct) => match process_struct_node(adb, strct, g_id) {
            Ok(struct_initializer) => AscriptRule::Struct(id, struct_initializer),
            Err(err) => {
              adb.errors.push(err);
              AscriptRule::Invalid(id)
            }
          },
          ASTNode::AST_Statement(stmt) => AscriptRule::Expression(id, Initializer {
            ty: AscriptType::Undefined,
            name: Default::default(),
            output_graph: None,
            ast: Some(stmt.expression.clone()),
            g_id,
          }),
          _ => unreachable!(),
        },
        ASTToken::ListEntry(_) => AscriptRule::ListInitial(id, Initializer {
          ty: AscriptType::Undefined,
          name: Default::default(),
          output_graph: None,
          ast: None,
          g_id,
        }),
        ASTToken::ListIterate(_) => AscriptRule::ListContinue(id, Initializer {
          ty: AscriptType::Undefined,
          name: Default::default(),
          output_graph: None,
          ast: None,
          g_id,
        }),
      },
    };

    for init in &mut adb.rules.0 {
      match init {
        AscriptRule::Struct(_, s) => {
          if let Some(strct) = adb.structs.get(&s.name) {
            s.has_token = strct.has_token;
          } else {
            panic!("Initializer's structure is not defined!");
          }
        }
        _ => {}
      }
    }

    adb.rules.push(ast_rule);
  }

  #[allow(irrefutable_let_patterns)]
  if let AscriptDatabase { structs, rules, .. } = adb {
    for rule in rules.iter_mut() {
      match rule {
        AscriptRule::Struct(_, rule) => {
          if let Some(s) = structs.get(&rule.name) {
            if s.properties.len() != rule.props.len() {
              rule.complete = false;
            } else {
              rule.complete = true;
            }
          } else {
            unreachable!()
          }
        }
        _ => {}
      }
    }
  }
}

pub fn process_struct_node(
  adb: &mut AscriptDatabase,
  strct: &AST_Struct,
  g_id: GrammarIdentities,
) -> RadlrResult<StructInitializer> {
  let s_store = adb.db.string_store();
  let name = &strct.ty[2..];
  let struct_id = StringId(name.intern(s_store));

  let mut initializer = StructInitializer {
    name:      struct_id,
    props:     Default::default(),
    complete:  false,
    has_token: false,
  };

  let mut seen: BTreeSet<StringId> = OrderedSet::new();
  let mut existing_struct = true;

  let ast_struct = adb.structs.entry(struct_id).or_insert_with(|| {
    existing_struct = false;

    AscriptStruct {
      id:         struct_id,
      name:       name.to_string(),
      properties: Default::default(),
      has_token:  false,
    }
  });

  for prop in &strct.props {
    match prop {
      ASTNode::AST_Property(prop) => {
        let prop_name: StringId = prop.id.as_str().into();
        let mut ast_prop = AscriptProp {
          is_optional: false,
          name: prop.id.to_string(),
          tok: prop.tok.clone(),
          ty: AscriptType::Undefined,
          g_id,
        };

        seen.insert(prop_name);

        match ast_struct.properties.get_mut(&prop_name) {
          Some(..) => {}
          None => {
            if existing_struct {
              ast_prop.is_optional = true;
            }
            ast_struct.properties.insert(prop_name, ast_prop);
          }
        }

        initializer.props.insert(StringId(prop.id.to_string().intern(s_store)), Initializer {
          ty: AscriptType::Undefined,
          name: prop_name,
          output_graph: None,
          ast: prop.value.clone(),
          g_id,
        });
      }
      ast @ ASTNode::AST_Token(..) => {
        let tok_id = StringId("tok".intern(s_store));
        initializer.props.insert(tok_id, Initializer {
          ty: AscriptType::Undefined,
          name: tok_id,
          output_graph: None,
          ast: Some(ast.clone()),
          g_id,
        });

        let mut ast_prop = AscriptProp {
          is_optional: false,
          name: "tok".to_string(),
          tok: Default::default(),
          ty: AscriptType::Undefined,
          g_id,
        };

        match ast_struct.properties.get_mut(&tok_id) {
          Some(..) => {}
          None => {
            if existing_struct {
              ast_prop.is_optional = true;
            }
            ast_struct.properties.insert(tok_id, ast_prop);
          }
        }

        ast_struct.has_token = true;
      }
      _ => {}
    }
  }

  for (name, prop) in adb.structs.get_mut(&struct_id).unwrap().properties.iter_mut() {
    if !seen.contains(name) {
      prop.is_optional |= true;
    }
  }

  Ok(initializer)
}

/// Derives the AST types of all parser NonTerminals.
pub fn resolve_nonterm_types(db: &ParserDatabase, adb: &mut AscriptDatabase) -> OrderedMap<DBNonTermKey, AscriptType> {
  let AscriptDatabase { errors, any_type_lu, any_types, .. } = adb;

  let mut resolved_nonterms = OrderedMap::new();
  //let pending_nonterms = OrderedMap::new();
  let mut rule_nonterms = OrderedMap::new();

  // Collect all non_term_dependency_of_a_rule

  for (i, _) in db.rules().iter().enumerate() {
    match &adb.rules.get(i) {
      Some(AscriptRule::Expression(_, node)) => {
        let item = Item::from((DBRuleKey::from(i), db));
        let set = rule_nonterms.entry(item.rule_id()).or_insert_with(|| OrderedSet::new());
        get_nonterm_refs(
          GraphResolveData {
            db,
            item,
            node: node.ast.as_ref().unwrap(),
            nonterm_types: &resolved_nonterms,
            default_nonterm_type: None,
          },
          set,
        )
      }
      Some(AscriptRule::LastSymbol(..)) => {
        let item = Item::from((DBRuleKey::from(i), db));
        let item = item.to_penultimate();
        match item.nonterm_index_at_sym(Default::default(), db) {
          Some(nonterm_key) => {
            let set = rule_nonterms.entry(item.rule_id()).or_insert_with(|| OrderedSet::new());
            set.insert(nonterm_key);
          }
          None => {
            rule_nonterms.entry(item.rule_id()).or_insert_with(|| OrderedSet::new());
          }
        }
      }
      Some(_) => {
        rule_nonterms.entry(DBRuleKey::from(i as u32)).or_insert_with(|| OrderedSet::new());
      }

      _ => {}
    }
  }

  // Start with rules that do not rely on non-term-values;
  let mut queue = VecDeque::from_iter(rule_nonterms.iter().filter_map(|(r, s)| s.is_empty().then_some(*r)));

  let max_iterations = db.rules().len().pow(2);
  let mut total_iterations = 0;

  while let Some(rule_id) = queue.pop_front() {
    total_iterations += 1;

    if total_iterations > max_iterations {
      panic!(
        "Could not resolve rule {}",
        db.db_rule(rule_id).rule.tok.blame(1, 1, "could not resolve the AST of this rule", None)
      )
    }

    let index: usize = rule_id.into();
    let item = Item::from((rule_id, db));

    let ty = match &adb.rules.get(index) {
      Some(AscriptRule::Invalid(..)) => AscriptType::Undefined,
      Some(AscriptRule::Struct(_, id)) => AscriptType::Scalar(AscriptScalarType::Struct(
        id.name,
        adb.structs.get(&id.name).map(|a| a.properties.len() == 0).unwrap_or_default(),
      )),
      Some(AscriptRule::LastSymbol(..)) => match graph_type_from_item(item.to_penultimate(), db, &resolved_nonterms, None) {
        (ty, _) => ty,
      },
      Some(AscriptRule::Expression(_, node)) => match get_graph_type(
        GraphResolveData {
          db,
          item,
          node: node.ast.as_ref().unwrap(),
          nonterm_types: &resolved_nonterms,
          default_nonterm_type: None,
        },
        &mut GraphMutData {
          selected_indices: &mut Default::default(),
          any_indices:      any_type_lu,
          any_maps:         any_types,
        },
      ) {
        Ok(ty) => ty,
        Err(err) => {
          errors.push(err);
          continue;
        }
      },
      Some(AscriptRule::ListContinue(..)) | Some(AscriptRule::ListInitial(..)) => {
        match item.to_penultimate().nonterm_index_at_sym(Default::default(), db) {
          Some(nonterm_key) => match resolved_nonterms.get(&nonterm_key) {
            Some(ty) => match ty {
              AscriptType::Scalar(ty) => AscriptType::Aggregate(AscriptAggregateType::Vec { val_type: *ty }),
              agg @ AscriptType::Aggregate(..) => *agg,
              _ => AscriptType::Undefined,
            },
            None => AscriptType::Undefined,
          },
          None => AscriptType::Aggregate(AscriptAggregateType::Vec { val_type: AscriptScalarType::Token }),
        }
      }
      _ => unreachable!(""),
    };

    let nonterm_id = db.db_rule(rule_id).nonterm;

    if ty.is_unknown() {
      queue.extend(rule_nonterms.iter().filter_map(|(r, s)| s.contains(&nonterm_id).then_some(*r)));
      queue.push_back(rule_id);
      continue;
    }

    let was_empty = resolved_nonterms.get(&nonterm_id).is_none();
    let existing_type = resolved_nonterms.entry(nonterm_id).or_insert(ty.clone());

    if was_empty || *existing_type != ty {
      queue.extend(rule_nonterms.iter().filter_map(|(r, s)| s.contains(&nonterm_id).then_some(*r)));

      let ref_name = db.nonterm_friendly_name_string(nonterm_id);

      match get_resolved_type(existing_type, &ty, any_type_lu, any_types, &ref_name) {
        Ok(resolved_type) => {
          *existing_type = resolved_type;
        }
        Err(err) => errors.push(err),
      }
    }
  }

  resolved_nonterms
}

#[track_caller]
fn resolve_expressions(
  adb: &mut AscriptDatabase,
  nonterm_types: OrderedMap<DBNonTermKey, AscriptType>,
) -> Result<(), DBNonTermKey> {
  let AscriptDatabase { structs, rules, errors, any_type_lu, any_types, .. } = adb;
  let db = adb.db.clone();
  let db = db.as_ref();

  for (index, ast_rule) in rules.iter_mut().enumerate() {
    let item = Item::from((DBRuleKey::from(index), db));
    let mut selected_indices = HashSet::new();
    let selected_indices = &mut selected_indices;
    match ast_rule {
      AscriptRule::Expression(_, init) => {
        let node = create_graph_node(
          GraphResolveData {
            db,
            item,
            node: init.ast.as_ref().unwrap(),
            nonterm_types: &nonterm_types,
            default_nonterm_type: None,
          },
          &mut GraphMutData {
            selected_indices: selected_indices,
            any_indices:      any_type_lu,
            any_maps:         any_types,
          },
        )?;
        init.ty = node.get_type().clone();
        init.output_graph = Some(node);
      }
      AscriptRule::ListInitial(_, init) => {
        let Initializer { output_graph, ty, .. } = init;
        let last = match get_item_at_sym_ref(item, db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, db, &nonterm_types, selected_indices, None)?,
          None => GraphNode::Undefined(AscriptType::Undefined),
        };

        match last.get_type() {
          AscriptType::Aggregate(_) => {
            *ty = last.get_type().clone();
            *output_graph = Some(last);
          }
          _ => {
            let last = GraphNode::Vec(
              GraphNodeVecInits(vec![last.clone()]),
              AscriptType::Aggregate(AscriptAggregateType::Vec { val_type: last.get_type().as_scalar().unwrap() }),
            );
            *ty = last.get_type().clone();
            *output_graph = Some(last);
          }
        };
      }
      AscriptRule::ListContinue(_, init) => {
        let Initializer { output_graph, ty, .. } = init;

        let first = match get_item_at_sym_ref(item, &adb.db, |_, _| true /* matches first item */) {
          Some(item) => graph_node_from_item(item, &adb.db, &nonterm_types, selected_indices, None)?,
          None => GraphNode::Undefined(AscriptType::Undefined),
        };
        let last = match get_item_at_sym_ref(item, &adb.db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, &adb.db, &nonterm_types, selected_indices, None)?,
          None => GraphNode::Undefined(AscriptType::Undefined),
        };

        debug_assert!(matches!(first.get_type(), AscriptType::Aggregate(..)));

        *ty = first.get_type().clone();

        let join = GraphNode::Add(Rc::new(first), Rc::new(last), *ty);
        *output_graph = Some(join);
      }
      AscriptRule::LastSymbol(_, init) => {
        let Initializer { output_graph, ty, .. } = init;

        let graph_node = match get_item_at_sym_ref(item, &adb.db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, &adb.db, &nonterm_types, selected_indices, None)?,
          None => GraphNode::Undefined(AscriptType::Undefined),
        };

        *ty = *graph_node.get_type();
        *output_graph = Some(graph_node);
      }
      AscriptRule::Struct(_, rule_struct) => {
        let struct_name: StringId = rule_struct.name;
        for (prop_name, init) in rule_struct.props.iter_mut().rev() {
          if let Some(node) = &init.ast {
            let rule_tok = node.to_token();
            let node = create_graph_node(
              GraphResolveData {
                db,
                item,
                node: init.ast.as_ref().unwrap(),
                nonterm_types: &nonterm_types,
                default_nonterm_type: None,
              },
              &mut GraphMutData {
                selected_indices: selected_indices,
                any_indices:      any_type_lu,
                any_maps:         any_types,
              },
            )?;

            if let Some(archetype_struct) = structs.get_mut(&struct_name) {
              if let Some(archetype_prop) = archetype_struct.properties.get_mut(prop_name) {
                let rule_prop_type = node.get_type();

                match (&archetype_prop.ty, rule_prop_type, archetype_prop.ty == *rule_prop_type) {
                  (_, _, true) => {}
                  (_, AscriptType::Undefined, _) => {
                    archetype_prop.is_optional = true;
                  }
                  (AscriptType::Undefined, ..) => {
                    archetype_prop.ty = *rule_prop_type;
                    archetype_prop.tok = rule_tok;
                    archetype_prop.g_id = init.g_id;
                  }
                  (..) => {
                    add_prop_redefinition_error(
                      errors,
                      &adb.db,
                      struct_name.as_ref().to_string(adb.db.string_store()),
                      prop_name.as_ref().to_string(adb.db.string_store()),
                      archetype_prop,
                      (*rule_prop_type, init.g_id, rule_tok),
                    );
                  }
                }
                init.ty = *rule_prop_type;
              }
            }

            match node {
              GraphNode::Undefined(_) => {}
              node => {
                init.output_graph = Some(node);
              }
            }
          }
        }
      }
      _r => todo!("handle rule {{_r:?}}"),
    }
  }

  Ok(())
}

fn get_item_at_sym_ref<'db, F: Fn(Item, &SymbolRef) -> bool>(item: Item, db: &'db ParserDatabase, funct: F) -> Option<Item> {
  let mut i = item;
  while let Some(sym) = i.sym(db) {
    if funct(i, sym) {
      return Some(i);
    }
    i = i.try_increment();
  }
  None
}

fn graph_type_from_item(
  item: Item,
  db: &ParserDatabase,
  nonterm_types: &std::collections::BTreeMap<DBNonTermKey, AscriptType>,
  default_type: Option<(DBNonTermKey, &AscriptType)>,
) -> (AscriptType, Option<DBNonTermKey>) {
  if let Some(nonterm_id) = item.nonterm_index_at_sym(Default::default(), db) {
    match nonterm_types.get(&nonterm_id) {
      Some(ty) => (*ty, Some(nonterm_id)),
      None => match default_type {
        Some((id, ty)) => {
          if id == nonterm_id {
            (*ty, Some(nonterm_id))
          } else {
            (AscriptType::Undefined, Some(nonterm_id))
          }
        }
        None => (AscriptType::Undefined, Some(nonterm_id)),
      },
    }
  } else {
    (AscriptType::Scalar(AscriptScalarType::Token), None)
  }
}

fn graph_node_from_item(
  item: Item,
  db: &ParserDatabase,
  nonterm_types: &std::collections::BTreeMap<DBNonTermKey, AscriptType>,
  selected_indices: &mut HashSet<usize>,
  default_type: Option<(DBNonTermKey, &AscriptType)>,
) -> Result<GraphNode, DBNonTermKey> {
  let index = item.sym_index() as usize;
  if let Some(nonterm_id) = item.nonterm_index_at_sym(Default::default(), db) {
    match nonterm_types.get(&nonterm_id) {
      Some(ty) => Ok(GraphNode::Sym(index, selected_indices.insert(index), *ty)),
      None => match default_type {
        Some((id, ty)) => {
          if id == nonterm_id {
            Ok(GraphNode::Sym(index, selected_indices.insert(index), *ty))
          } else {
            Err(nonterm_id)
          }
        }
        None => Err(nonterm_id),
      },
    }
  } else {
    Ok(GraphNode::TokSym(index, selected_indices.insert(index), AscriptType::Scalar(AscriptScalarType::Token)))
  }
}

struct GraphMutData<'a> {
  selected_indices: &'a mut HashSet<usize>,
  any_indices:      &'a mut Vec<usize>,
  any_maps:         &'a mut Vec<AnyTypeRef>,
}

#[derive(Clone, Copy)]
struct GraphResolveData<'a> {
  item:                 Item,
  db:                   &'a ParserDatabase,
  node:                 &'a ASTNode,
  nonterm_types:        &'a OrderedMap<DBNonTermKey, AscriptType>,
  default_nonterm_type: Option<(DBNonTermKey, &'a AscriptType)>,
}

impl<'a> GraphResolveData<'a> {
  pub fn to_node(self, node: &'a ASTNode) -> GraphResolveData {
    Self { node, ..self }
  }
}

/// Returns the type of the root node in ascript node graph;
fn get_nonterm_refs<'a>(args: GraphResolveData<'a>, nonterms: &mut OrderedSet<DBNonTermKey>) {
  match args.node {
    ASTNode::AST_NamedReference(rf) => {
      match get_item_at_sym_ref(args.item, args.db, |_, sym| sym.annotation == rf.value.to_token()) {
        Some(item) => match graph_type_from_item(item, args.db, args.nonterm_types, args.default_nonterm_type) {
          (_, Some(nonterm)) => {
            nonterms.insert(nonterm);
          }
          _ => {}
        },
        None => {}
      }
    }

    ASTNode::AST_IndexReference(rf) => {
      match get_item_at_sym_ref(args.item, args.db, |_, sym| sym.original_index as isize == (rf.value - 1) as isize) {
        Some(item) => match graph_type_from_item(item, args.db, args.nonterm_types, args.default_nonterm_type) {
          (_, Some(nonterm)) => {
            nonterms.insert(nonterm);
          }
          _ => {}
        },
        None => {}
      }
    }
    ASTNode::AST_Vector(vec) => {
      for node in vec.initializer.iter() {
        get_nonterm_refs(args.to_node(node), nonterms);
      }
    }

    ASTNode::AST_Map(map) => {
      get_nonterm_refs(args.to_node(&map.key), nonterms);
      get_nonterm_refs(args.to_node(&map.val), nonterms);
    }

    ASTNode::AST_Add(add) => {
      get_nonterm_refs(args.to_node(&add.left), nonterms);
      get_nonterm_refs(args.to_node(&add.right), nonterms);
    }
    ASTNode::AST_Mul(mul) => {
      get_nonterm_refs(args.to_node(&mul.left), nonterms);
      get_nonterm_refs(args.to_node(&mul.right), nonterms);
    }
    ASTNode::AST_Token(..)
    | ASTNode::AST_String(..)
    | ASTNode::AST_Bool(..)
    | ASTNode::AST_U8(..)
    | ASTNode::AST_U16(..)
    | ASTNode::AST_U32(..)
    | ASTNode::AST_U64(..)
    | ASTNode::AST_I8(..)
    | ASTNode::AST_I16(..)
    | ASTNode::AST_I32(..)
    | ASTNode::AST_I64(..)
    | ASTNode::AST_F32(..)
    | ASTNode::AST_F64(..) => {}
    #[cfg(debug_assertions)]
    node => todo!("handle graph type resolve of node {node:#?}"),
    #[cfg(not(debug_assertions))]
    _ => panic!("Unresolved node type"),
  };
}

/// Returns the type of the root node in ascript node graph;
fn get_graph_type<'a>(args: GraphResolveData<'a>, mut_args: &mut GraphMutData) -> RadlrResult<AscriptType> {
  let ty = match args.node {
    ASTNode::AST_NamedReference(rf) => {
      match get_item_at_sym_ref(args.item, args.db, |_, sym| sym.annotation == rf.value.to_token()) {
        Some(item) => graph_type_from_item(item, args.db, args.nonterm_types, args.default_nonterm_type).0,
        None => AscriptType::Undefined,
      }
    }

    ASTNode::AST_IndexReference(rf) => {
      match get_item_at_sym_ref(args.item, args.db, |_, sym| sym.original_index as isize == (rf.value - 1) as isize) {
        Some(item) => graph_type_from_item(item, args.db, args.nonterm_types, args.default_nonterm_type).0,
        None => AscriptType::Undefined,
      }
    }
    ASTNode::AST_Vector(vec) => {
      let mut ty = AscriptType::Undefined;
      let mut initializers = vec![];

      for node in vec.initializer.iter() {
        let node = get_graph_type(args.to_node(node), mut_args)?;
        ty = get_resolved_type(&node, &ty, mut_args.any_indices, mut_args.any_maps, "__TEMP___").unwrap();
        initializers.push(node);
      }

      let ty = if let Some(ty_) = ty.as_aggregate() {
        match ty_ {
          AscriptAggregateType::Map { val_type, .. } => AscriptType::Scalar(val_type),
          AscriptAggregateType::Vec { val_type } => AscriptType::Scalar(val_type),
        }
      } else {
        ty
      };

      debug_assert!(ty.as_scalar().is_some(), "Type: {ty:#?}");

      AscriptType::Aggregate(AscriptAggregateType::Vec { val_type: ty.as_scalar().unwrap() })
    }

    ASTNode::AST_Map(map) => {
      let key = get_graph_type(args.to_node(&map.key), mut_args)?;
      let val = get_graph_type(args.to_node(&map.val), mut_args)?;

      if key.is_unknown() || val.is_unknown() {
        AscriptType::Undefined
      } else {
        let ascript_type = AscriptType::Aggregate(AscriptAggregateType::Map {
          key_type: key.as_scalar().unwrap(),
          val_type: val.as_scalar().unwrap(),
        });

        ascript_type
      }
    }

    ASTNode::AST_Add(add) => {
      let l_ty = get_graph_type(args.to_node(&add.left), mut_args)?;
      let r_ty = get_graph_type(args.to_node(&add.right), mut_args)?;

      if l_ty.is_unknown() || r_ty.is_unknown() {
        AscriptType::Undefined
      } else if l_ty.is_numeric() && r_ty.is_numeric() {
        let l_scalar = l_ty.as_scalar().unwrap();
        let r_scalar = r_ty.as_scalar().unwrap();

        (l_scalar.precedence() > r_scalar.precedence()).then_some(l_ty).unwrap_or(r_ty)
      } else if l_ty.is_token() || r_ty.is_token() {
        if l_ty.is_string() || r_ty.is_string() {
          AscriptType::Scalar(AscriptScalarType::String(None))
        } else if l_ty.is_token() && r_ty.is_token() {
          AscriptType::Scalar(AscriptScalarType::Token)
        } else {
          AscriptType::Undefined
        }
      } else if l_ty.is_string() || r_ty.is_string() {
        AscriptType::Scalar(AscriptScalarType::String(None))
      } else {
        get_resolved_type(&l_ty, &r_ty, mut_args.any_indices, mut_args.any_maps, "__TEMP___").unwrap()
      }
    }
    ASTNode::AST_Mul(mul) => {
      let l_ty = get_graph_type(args.to_node(&mul.left), mut_args)?;
      let r_ty = get_graph_type(args.to_node(&mul.right), mut_args)?;

      if l_ty.is_unknown() || r_ty.is_unknown() {
        AscriptType::Undefined
      } else if l_ty.is_numeric() && r_ty.is_numeric() {
        let l_scalar = l_ty.as_scalar().unwrap();
        let r_scalar = r_ty.as_scalar().unwrap();

        (l_scalar.precedence() > r_scalar.precedence()).then_some(l_ty).unwrap_or(r_ty)
      } else {
        AscriptType::Undefined
      }
    }
    ASTNode::AST_Token(..) => AscriptType::Scalar(AscriptScalarType::Token),
    ASTNode::AST_String(..) => AscriptType::Scalar(AscriptScalarType::String(None)),
    ASTNode::AST_Bool(..) => AscriptType::Scalar(AscriptScalarType::Bool(false)),
    ASTNode::AST_U8(..) => AscriptType::Scalar(AscriptScalarType::U8(None)),
    ASTNode::AST_U16(..) => AscriptType::Scalar(AscriptScalarType::U16(None)),
    ASTNode::AST_U32(..) => AscriptType::Scalar(AscriptScalarType::U32(None)),
    ASTNode::AST_U64(..) => AscriptType::Scalar(AscriptScalarType::U64(None)),
    ASTNode::AST_I8(..) => AscriptType::Scalar(AscriptScalarType::I8(None)),
    ASTNode::AST_I16(..) => AscriptType::Scalar(AscriptScalarType::I16(None)),
    ASTNode::AST_I32(..) => AscriptType::Scalar(AscriptScalarType::I32(None)),
    ASTNode::AST_I64(..) => AscriptType::Scalar(AscriptScalarType::I64(None)),
    ASTNode::AST_F32(..) => AscriptType::Scalar(AscriptScalarType::F32(None)),
    ASTNode::AST_F64(..) => AscriptType::Scalar(AscriptScalarType::F64(None)),
    #[cfg(debug_assertions)]
    node => todo!("handle graph type resolve of node {node:#?}"),
    #[cfg(not(debug_assertions))]
    _ => panic!("Unresolved node type"),
  };
  Ok(ty)
}

/// Resolves AST expression nodes into AscriptGraph nodes.
///
/// # Invariant:
/// Any type incompatibilities will have been reported before we
/// start building graph nodes. We should be able to assign nodes with having to
/// verify whether there are type conflicts.
fn create_graph_node<'a>(args: GraphResolveData<'a>, mut_args: &mut GraphMutData) -> Result<GraphNode, DBNonTermKey> {
  match args.node {
    ASTNode::AST_Vector(vec) => {
      let mut ty = AscriptType::Undefined;
      let mut initializers = vec![];

      for node in vec.initializer.iter() {
        let node = create_graph_node(args.to_node(node), mut_args)?;
        ty = get_resolved_type(node.get_type(), &ty, mut_args.any_indices, mut_args.any_maps, "__TEMP___").unwrap();
        initializers.push(node);
      }

      let ty = if let Some(ty_) = ty.as_aggregate() {
        match ty_ {
          AscriptAggregateType::Map { val_type, .. } => AscriptType::Scalar(val_type),
          AscriptAggregateType::Vec { val_type } => AscriptType::Scalar(val_type),
        }
      } else {
        ty
      };

      debug_assert!(ty.as_scalar().is_some(), "Type: {ty:#?}");

      let ascript_type = AscriptType::Aggregate(AscriptAggregateType::Vec { val_type: ty.as_scalar().unwrap() });

      Ok(GraphNode::Vec(GraphNodeVecInits(initializers), ascript_type))
    }

    ASTNode::AST_Map(map) => {
      let key = create_graph_node(args.to_node(&map.key), mut_args)?;

      let val = create_graph_node(args.to_node(&map.val), mut_args)?;

      let ascript_type = AscriptType::Aggregate(AscriptAggregateType::Map {
        key_type: key.get_type().as_scalar().unwrap(),
        val_type: val.get_type().as_scalar().unwrap(),
      });

      Ok(GraphNode::Map(Rc::new(key), Rc::new(val), ascript_type))
    }

    ASTNode::AST_NamedReference(rf) => {
      match get_item_at_sym_ref(args.item, args.db, |_, sym| sym.annotation == rf.value.to_token()) {
        Some(item) => {
          Ok(graph_node_from_item(item, args.db, args.nonterm_types, mut_args.selected_indices, args.default_nonterm_type)?)
        }
        None => Ok(GraphNode::Undefined(AscriptType::Undefined)),
      }
    }

    ASTNode::AST_IndexReference(rf) => {
      match get_item_at_sym_ref(args.item, args.db, |_, sym| sym.original_index as isize == (rf.value - 1) as isize) {
        Some(item) => {
          graph_node_from_item(item, args.db, args.nonterm_types, mut_args.selected_indices, args.default_nonterm_type)
        }
        None => Ok(GraphNode::Undefined(AscriptType::Undefined)),
      }
    }

    ASTNode::AST_Token(tok) => {
      if let Some(range) = &tok.range {
        let node = GraphNode::TokRule(AscriptType::Scalar(AscriptScalarType::Token));

        Ok(GraphNode::Trim(
          Rc::new(node),
          range.start_trim as isize,
          range.end_trim as isize,
          AscriptType::Scalar(AscriptScalarType::Token),
        ))
      } else {
        Ok(GraphNode::TokRule(AscriptType::Scalar(AscriptScalarType::Token)))
      }
    }

    ASTNode::AST_String(str) => {
      if let Some(init) = &str.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Str(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::String(None))))
      } else {
        Ok(GraphNode::Str(None, AscriptType::Scalar(AscriptScalarType::String(None))))
      }
    }

    ASTNode::AST_Add(add) => {
      let l = create_graph_node(args.to_node(&add.left), mut_args)?;
      let r = create_graph_node(args.to_node(&add.right), mut_args)?;
      let ty = get_resolved_type(l.get_type(), r.get_type(), mut_args.any_indices, mut_args.any_maps, "TEST").unwrap();
      Ok(GraphNode::Add(Rc::new(l), Rc::new(r), ty))
    }

    ASTNode::AST_Sub(sub) => {
      let l = create_graph_node(args.to_node(&sub.left), mut_args)?;
      let r = create_graph_node(args.to_node(&sub.right), mut_args)?;
      let ty = get_resolved_type(l.get_type(), r.get_type(), mut_args.any_indices, mut_args.any_maps, "TEST").unwrap();
      let (l, r) = convert_binary_op_types(l, ty, r);
      Ok(GraphNode::Sub(Rc::new(l), Rc::new(r), ty))
    }

    ASTNode::AST_Mul(mul) => {
      let l = create_graph_node(args.to_node(&mul.left), mut_args)?;
      let r = create_graph_node(args.to_node(&mul.right), mut_args)?;
      let ty = get_resolved_type(l.get_type(), r.get_type(), mut_args.any_indices, mut_args.any_maps, "TEST").unwrap();
      let (l, r) = convert_binary_op_types(l, ty, r);
      Ok(GraphNode::Mul(Rc::new(l), Rc::new(r), ty))
    }

    ASTNode::AST_Div(div) => {
      let l = create_graph_node(args.to_node(&div.left), mut_args)?;
      let r = create_graph_node(args.to_node(&div.right), mut_args)?;
      let ty = get_resolved_type(l.get_type(), r.get_type(), mut_args.any_indices, mut_args.any_maps, "TEST").unwrap();
      let (l, r) = convert_binary_op_types(l, ty, r);
      Ok(GraphNode::Div(Rc::new(l), Rc::new(r), ty))
    }

    ASTNode::AST_Pow(..) => {
      todo!("Create Pow Graph node")
    }

    ASTNode::AST_Mod(..) => {
      todo!("Create Mod Graph node")
    }

    ASTNode::AST_Bool(bool) => {
      if let Some(init) = &bool.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Bool(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::Bool(false))))
      } else {
        Ok(GraphNode::Bool(None, AscriptType::Scalar(AscriptScalarType::Bool(false))))
      }
    }

    ASTNode::AST_U8(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::U8(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::U8(None))))
      }
    }

    ASTNode::AST_U16(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::U16(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::U16(None))))
      }
    }

    ASTNode::AST_U32(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::U32(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::U32(None))))
      }
    }

    ASTNode::AST_U64(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::U64(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::U64(None))))
      }
    }

    ASTNode::AST_I8(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::I8(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::I8(None))))
      }
    }

    ASTNode::AST_I16(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::I16(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::I16(None))))
      }
    }

    ASTNode::AST_I32(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::I32(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::I32(None))))
      }
    }

    ASTNode::AST_I64(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::I64(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::I64(None))))
      }
    }

    ASTNode::AST_F32(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::F32(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::F32(None))))
      }
    }

    ASTNode::AST_F64(val) => {
      if let Some(init) = &val.initializer {
        let gn = create_graph_node(args.to_node(&init.expression), mut_args)?;
        Ok(GraphNode::Num(Some(Rc::new(gn)), AscriptType::Scalar(AscriptScalarType::F64(None))))
      } else {
        Ok(GraphNode::Num(None, AscriptType::Scalar(AscriptScalarType::F64(None))))
      }
    }

    #[cfg(debug_assertions)]
    node => todo!("handle graph resolve of node {node:#?}"),
    #[cfg(not(debug_assertions))]
    _ => panic!("Unresolved node type"),
  }
}

fn convert_binary_op_types(l: GraphNode, ty: AscriptType, r: GraphNode) -> (GraphNode, GraphNode) {
  (
    (l.get_type().to_cardinal() != ty.to_cardinal()).then(|| GraphNode::Num(Some(Rc::new(l.clone())), ty)).unwrap_or(l),
    (r.get_type().to_cardinal() != ty.to_cardinal()).then(|| GraphNode::Num(Some(Rc::new(r.clone())), ty)).unwrap_or(r),
  )
}

/// Attempts to merge two different types into a single, compatible type.
///
/// Returns an Err if the the combination of types cannot be reasonably merged,
/// such as Vec<u32> and Vec<String>, a Struct and a String, or a Struct and a
/// Struct.
fn get_resolved_type(
  a: &AscriptType,
  b: &AscriptType,
  any_i: &mut Vec<usize>,
  any_m: &mut Vec<AnyTypeRef>,
  ref_name: &str,
) -> RadlrResult<AscriptType> {
  use AscriptAggregateType::*;
  use AscriptType::*;
  match b {
    Undefined => Ok(*a),
    Aggregate(b_agg) => match a {
      Aggregate(a_agg) => match b_agg {
        Map { key_type: b_key_type, val_type: b_val_type } => match a_agg {
          Map { key_type: a_key_type, val_type: a_val_type } => {
            let key_type = get_resolved_type(&Scalar(*a_key_type), &Scalar(*b_key_type), any_i, any_m, ref_name)?
              .as_scalar()
              .unwrap_or_default();
            let val_type = get_resolved_type(&Scalar(*a_val_type), &Scalar(*b_val_type), any_i, any_m, ref_name)?
              .as_scalar()
              .unwrap_or_default();
            Ok(Aggregate(Map { key_type, val_type }))
          }
          Vec { val_type: a_base_type } => {
            let base_type = get_resolved_type(&Scalar(*a_base_type), &Scalar(*b_val_type), any_i, any_m, ref_name)?
              .as_scalar()
              .unwrap_or_default();
            Ok(Aggregate(Vec { val_type: base_type }))
          }
        },
        Vec { val_type: b_base_type } => match a_agg {
          Map { val_type: a_val_type, .. } => {
            let base_type = get_resolved_type(&Scalar(*b_base_type), &Scalar(*a_val_type), any_i, any_m, ref_name)?
              .as_scalar()
              .unwrap_or_default();
            Ok(Aggregate(Vec { val_type: base_type }))
          }
          Vec { val_type: a_base_type } => {
            let base_type = get_resolved_type(&Scalar(*b_base_type), &Scalar(*a_base_type), any_i, any_m, ref_name)?
              .as_scalar()
              .unwrap_or_default();
            Ok(Aggregate(Vec { val_type: base_type }))
          }
        },
      },
      Scalar(_) => match b_agg {
        Vec { val_type: base_type } => Ok(Aggregate(Vec {
          val_type: get_resolved_type(a, &Scalar(*base_type), any_i, any_m, ref_name)?.as_scalar().unwrap_or_default(),
        })),
        Map { .. } => todo!("Issue invalid joining of scalar and map"),
      },

      Undefined => Ok(*b),
      _ => todo!("resolve different types {a:?} {b:?}"),
    },
    Scalar(b_scalar) => match a {
      Undefined => Ok(*b),
      Scalar(a_scalar) => {
        use BaseType::*;

        let a_base_type = BaseType::from(*a_scalar);
        let b_base_type = BaseType::from(*b_scalar);

        let types = if a_base_type < b_base_type {
          ((a_base_type, a_scalar, a), (b_base_type, b_scalar, b))
        } else {
          ((b_base_type, b_scalar, b), (a_base_type, a_scalar, a))
        };

        match types {
          (
            (_, _, AscriptType::Scalar(AscriptScalarType::Any(any_a))),
            (_, _, AscriptType::Scalar(AscriptScalarType::Any(any_b))),
          ) => {
            let a_index = any_i[*any_a];
            let b_index = any_i[*any_b];

            if a_index != b_index {
              let used_index = a_index.min(b_index);

              any_i[*any_a] = used_index;
              any_i[*any_b] = used_index;

              let mut set = OrderedSet::new();

              set.extend(&any_m[a_index].1);
              set.extend(&any_m[b_index].1);

              any_m[used_index].1 = set;
            }

            Ok(*a)
          }
          ((_, _, AscriptType::Scalar(AscriptScalarType::Any(any))), (_, _, AscriptType::Scalar(scalar)))
          | ((_, _, AscriptType::Scalar(scalar)), (_, _, AscriptType::Scalar(AscriptScalarType::Any(any)))) => {
            let a_index = any_i[*any];

            any_m[a_index].1.insert(*scalar);

            Ok(AscriptType::Scalar(AscriptScalarType::Any(*any)))
          }

          ((_, _, a), (_, _, b)) if a == b => Ok(*a),
          ((a_base, a_scl, a), (b_base, b_scl, b)) if a_base == b_base && a_base != BaseType::Other => {
            if a_scl.byte_size() > b_scl.byte_size() {
              Ok(*a)
            } else {
              Ok(*b)
            }
          }
          ((Bool, a_scl, _), (Int, b_scl, _)) | ((Uint, a_scl, _), (Int, b_scl, _)) => {
            match (a_scl.byte_size() + 1).max(b_scl.byte_size()) {
              1..=2 => Ok(Scalar(AscriptScalarType::I16(None))),
              3..=4 => Ok(Scalar(AscriptScalarType::I32(None))),
              _ => Ok(Scalar(AscriptScalarType::I64(None))),
            }
          }
          ((Bool, a_scl, _), (Float, b_scl, _))
          | ((Uint, a_scl, _), (Float, b_scl, _))
          | ((Int, a_scl, _), (Float, b_scl, _)) => match (a_scl.byte_size() + 1).max(b_scl.byte_size()) {
            1..=4 => Ok(Scalar(AscriptScalarType::F32(None))),
            _ => Ok(Scalar(AscriptScalarType::F64(None))),
          },
          _ => {
            let types = BTreeSet::from_iter([*a_scalar, *b_scalar]);
            let index = any_i.len();

            any_i.push(any_m.len());

            if ref_name.len() > 1 {
              any_m
                .push((format!("{}{}", ref_name.chars().next().unwrap_or_default().to_ascii_uppercase(), &ref_name[1..]), types));
            } else {
              any_m.push((format!("{}", ref_name.chars().next().unwrap_or_default()), types));
            }

            return Ok(AscriptType::Scalar(AscriptScalarType::Any(index)));
          }
        }
      }
      Aggregate(a_gg) => match b_scalar {
        AscriptScalarType::Struct(..) => Err(RadlrError::StaticText("Incompatible Types")),
        _ => todo!("Resolve types agg_a:{a_gg:?} ty:{b_scalar:?}"),
      },
      _ => todo!("Resolve types scaler:{a:?} ty:{b:?}"),
    },
    _ => todo!("Resolve types ty:{a:?} ty:{b:?}"),
  }
}
