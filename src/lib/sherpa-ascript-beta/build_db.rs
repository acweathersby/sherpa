use std::{collections::VecDeque, rc::Rc};

use sherpa_core::{
  parser::{ASTNode, AST_Struct},
  proxy::{OrderedMap, OrderedSet},
  ASTToken,
  CachedString,
  DBNonTermKey,
  DBRuleKey,
  Item,
  ParserDatabase,
  SherpaDatabase,
  SherpaResult,
  SymbolRef,
};

use crate::types::*;

pub fn build_database(db: SherpaDatabase) -> AscriptDatabase {
  let db = db.get_internal();

  let mut adb = AscriptDatabase {
    errors:  Default::default(),
    structs: Default::default(),
    rules:   Default::default(),
  };

  extract_structs(db, &mut adb);

  let nonterm_types = resolve_types(db, &mut adb);

  resolve_expressions(db, &mut adb, nonterm_types);

  adb
}

pub fn extract_structs(db: &ParserDatabase, adb: &mut AscriptDatabase) {
  for (rule_id, db_rule) in db.rules().iter().enumerate().map(|(id, r)| (DBRuleKey::from(id), r)) {
    let rule = &db_rule.rule;

    let rule = match &rule.ast {
      None => AscriptRule::LastSymbol(Initializer {
        ty:           AscriptType::Undefined,
        output_graph: None,
        ast:          None,
      }),
      Some(ast) => match ast {
        ASTToken::Defined(ast) => match &ast.ast {
          ASTNode::AST_Struct(strct) => match process_struct(adb, strct) {
            Ok(struct_initializer) => AscriptRule::Struct(struct_initializer),
            Err(err) => {
              adb.errors.push(err);
              AscriptRule::Invalid
            }
          },
          ASTNode::AST_Statements(stmt) => AscriptRule::Expression(Initializer {
            ty:           AscriptType::Undefined,
            output_graph: None,
            ast:          Some(ASTNode::AST_Statements(stmt.clone())),
          }),
          _ => unreachable!(),
        },
        ASTToken::ListEntry(tok) => AscriptRule::ListInitial(Initializer {
          ty:           AscriptType::Undefined,
          output_graph: None,
          ast:          None,
        }),
        ASTToken::ListIterate(tok) => AscriptRule::ListContinue(Initializer {
          ty:           AscriptType::Undefined,
          output_graph: None,
          ast:          None,
        }),
      },
    };

    adb.rules.push(rule);
  }
}

pub fn process_struct(adb: &mut AscriptDatabase, strct: &AST_Struct) -> SherpaResult<StructInitializer> {
  let name = &strct.typ[2..];
  let struct_id = StringId::from(name);
  let mut initializer = StructInitializer { ty: struct_id, props: Default::default() };
  let mut seen = OrderedSet::new();

  for prop in &strct.props {
    if let Some(prop) = prop.as_AST_Property() {
      let prop_name: StringId = prop.id.as_str().into();
      let mut ast_prop = AScriptProp {
        is_optional: false,
        name:        prop.id.to_string(),
        tok:         prop.tok.clone(),
        ty:          AscriptType::Undefined,
      };

      seen.insert(prop_name);

      {
        let mut existing_struct = true;
        let ast_struct = adb.structs.entry(struct_id).or_insert_with(|| {
          existing_struct = false;
          AscriptStruct {
            id:         struct_id,
            name:       name.to_string(),
            properties: Default::default(),
          }
        });

        match ast_struct.properties.get_mut(&prop_name) {
          Some(..) => {}
          None => {
            if existing_struct {
              ast_prop.is_optional = true;
            }
            ast_struct.properties.insert(prop_name, ast_prop);
          }
        }
      }

      initializer.props.insert(StringId::from(prop.id.to_string()), Initializer {
        ty:           AscriptType::Undefined,
        output_graph: None,
        ast:          prop.value.clone(),
      });
    }
  }

  for (name, prop) in adb.structs.get_mut(&struct_id).unwrap().properties.iter_mut() {
    if !seen.contains(name) {
      prop.is_optional |= true;
    }
  }

  Ok(initializer)
}

pub fn resolve_types(db: &ParserDatabase, adb: &mut AscriptDatabase) -> OrderedMap<DBNonTermKey, AscriptType> {
  let mut pending = adb
    .rules
    .0
    .iter()
    .enumerate()
    .map(|(i, _)| {
      (i, DBRuleKey::from(i), PendingType::Unresolved {
        non_terms:  Default::default(),
        scalars:    Default::default(),
        aggregates: Default::default(),
      })
    })
    .collect::<Vec<_>>();

  let mut nonterms = OrderedMap::new();
  for rule in db.rules() {
    nonterms.entry(rule.nonterm).or_insert((AscriptType::Undefined, 0)).1 += 1
  }

  loop {
    let mut complete = true;

    for (index, rule_id, type_data) in &mut pending {
      println!("{index}");
      if db.rules()[*index].is_scanner {
        continue;
      }

      match type_data {
        PendingType::Unresolved { non_terms: pending_nonterm, scalars, aggregates } => {
          let rule = &db.rules()[*index];
          let nonterm_id = rule.nonterm;

          if pending_nonterm.len() > 0 {
            let mut queue = VecDeque::from_iter(pending_nonterm.iter().cloned());
            while let Some(non_term) = queue.pop_front() {
              match nonterms.get(&non_term) {
                Some((_, 0)) => {
                  pending_nonterm.remove(&non_term);
                }
                Some((_, 1)) if non_term == nonterm_id => {
                  // Self referential
                  pending_nonterm.remove(&non_term);
                }
                _ => {}
              }
            }
          }

          let item = Item::from((*rule_id, db));

          let ty = match &mut adb.rules[*index] {
            AscriptRule::LastSymbol(..) => match item.to_complete().decrement() {
              Some(item) => match item.nonterm_index_at_sym(Default::default(), db) {
                Some(nonterm_key) => match nonterms.get(&nonterm_key) {
                  Some((ty, v)) => {
                    if *v == 0 {
                      *ty
                    } else {
                      pending_nonterm.insert(nonterm_key);
                      AscriptType::Undefined
                    }
                  }
                  None => AscriptType::Undefined,
                },
                None => AscriptType::Scalar(AscriptScalarType::Token),
              },
              None => AscriptType::Undefined,
            },
            AscriptRule::ListContinue(..) | AscriptRule::ListInitial(..) => match item.to_complete().decrement() {
              Some(item) => match item.nonterm_index_at_sym(Default::default(), db) {
                Some(nonterm_key) => match nonterms.get(&nonterm_key) {
                  Some((ty, v)) => {
                    if *v == 0 {
                      match ty {
                        AscriptType::Scalar(ty) => AscriptType::Aggregate(AscriptAggregateType::Vec { base_type: *ty }),
                        AscriptType::Aggregate(ty) => match ty {
                          AscriptAggregateType::Map { val_type, .. } => {
                            AscriptType::Aggregate(AscriptAggregateType::Vec { base_type: *val_type })
                          }
                          AscriptAggregateType::Vec { base_type } => {
                            AscriptType::Aggregate(AscriptAggregateType::Vec { base_type: *base_type })
                          }
                        },
                        _ => AscriptType::Undefined,
                      }
                    } else {
                      pending_nonterm.insert(nonterm_key);
                      AscriptType::Undefined
                    }
                  }
                  None => AscriptType::Undefined,
                },
                None => AscriptType::Aggregate(AscriptAggregateType::Vec { base_type: AscriptScalarType::Token }),
              },
              None => AscriptType::Undefined,
            },
            AscriptRule::Struct(id) => AscriptType::Scalar(AscriptScalarType::Struct(id.ty)),
            AscriptRule::Invalid => AscriptType::Undefined,
            AscriptRule::Expression(init) => {
              todo!("Resolve stmt type");
            }
          };

          if pending_nonterm.len() > 0 {
            // Keep this alive until all non_terminal requirements are
            // resolved.
            complete = false;
            continue;
          }

          match ty {
            AscriptType::Undefined => {
              *type_data = PendingType::Resolved(AscriptType::Undefined);
              continue;
            }
            a_ty => match nonterms.get_mut(&nonterm_id) {
              Some((nterm_ty, v)) => {
                *type_data = PendingType::Resolved(*nterm_ty);
                match &mut adb.rules[*index] {
                  AscriptRule::ListContinue(init) => init.ty = a_ty,
                  AscriptRule::ListInitial(init) => init.ty = a_ty,
                  AscriptRule::Expression(init) => init.ty = a_ty,
                  AscriptRule::LastSymbol(init) => init.ty = a_ty,
                  _ => {}
                }
                *v -= 1;
                match get_resolved_type(a_ty, *nterm_ty) {
                  Some(ty) => *nterm_ty = ty,
                  None => todo!("Create Error for failed type"),
                }
              }
              None => unreachable!(),
            },
          }
        }
        _ => {}
      }
    }

    if complete {
      break;
    }
  }

  nonterms.into_iter().map(|(k, (v, _))| (k, v)).collect()
}

fn resolve_expressions(db: &ParserDatabase, adb: &mut AscriptDatabase, nonterm_types: OrderedMap<DBNonTermKey, AscriptType>) {
  let AscriptDatabase { structs, errors, rules } = adb;

  for (index, ast_rule) in rules.iter_mut().enumerate() {
    let rule = &db.rules()[index].rule;
    let item = Item::from((DBRuleKey::from(index), db));
    match ast_rule {
      AscriptRule::Expression(init) => {
        let Initializer { ast, output_graph, .. } = init;

        if let Some(node) = &ast {
          *output_graph = Some(resolve_node(db, node, item, &nonterm_types));
        }
      }
      AscriptRule::ListInitial(init) => {
        let Initializer { output_graph, ty, .. } = init;

        let last = match get_item_at_sym_ref(item, db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, db, &nonterm_types),
          None => GraphNode::Undefined,
        };

        //*ty = AscriptType::Aggregate(AscriptAggregateType::Vec { base_type: last.get_type().as_scalar().unwrap() });

        let last = GraphNode::Vec(Rc::new(last), *ty);

        *output_graph = Some(last);
      }
      AscriptRule::ListContinue(init) => {
        let Initializer { output_graph, ty, .. } = init;
        let first = match get_item_at_sym_ref(item, db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, db, &nonterm_types),
          None => GraphNode::Undefined,
        };
        let last = match get_item_at_sym_ref(item, db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, db, &nonterm_types),
          None => GraphNode::Undefined,
        };

        //*ty = AscriptType::Aggregate(AscriptAggregateType::Vec { base_type: last.get_type().as_scalar().unwrap() });

        let first = GraphNode::Vec(Rc::new(first), *ty);
        let join = GraphNode::Add(Rc::new(first), Rc::new(last), *ty);
        *output_graph = Some(join);
      }
      AscriptRule::LastSymbol(init) => {
        let Initializer { output_graph, ty, .. } = init;

        let graph_node = match get_item_at_sym_ref(item, db, |item, _| item.is_penultimate()) {
          Some(item) => graph_node_from_item(item, db, &nonterm_types),
          None => GraphNode::Undefined,
        };

        *ty = graph_node.get_type();
        *output_graph = Some(graph_node);
      }
      AscriptRule::Struct(strct) => {
        let id: StringId = strct.ty;
        for (name, init) in &mut strct.props {
          if let Some(node) = &init.ast {
            let node = resolve_node(db, node, item, &nonterm_types);
            if let Some(strct) = structs.get_mut(&id) {
              if let Some(prop) = strct.properties.get_mut(name) {
                prop.ty = node.get_type()
              }
            }
            init.output_graph = Some(node);
          }
        }
      }
      r => todo!("handle rule {r:?}"),
    }
  }
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
fn graph_node_from_item(
  item: Item,
  db: &ParserDatabase,
  nonterm_types: &std::collections::BTreeMap<DBNonTermKey, AscriptType>,
) -> GraphNode {
  if let Some(nonterm_id) = item.nonterm_index_at_sym(Default::default(), db) {
    let ty = nonterm_types.get(&nonterm_id).unwrap();
    GraphNode::Sym(item.sym_index() as usize, *ty)
  } else {
    GraphNode::TokSym(item.sym_index() as usize)
  }
}

fn resolve_node(
  db: &ParserDatabase,
  node: &ASTNode,
  item: Item,
  nonterm_types: &OrderedMap<DBNonTermKey, AscriptType>,
) -> GraphNode {
  match node {
    ASTNode::AST_NamedReference(rf) => match get_item_at_sym_ref(item, db, |_, sym| sym.annotation == rf.value.to_token()) {
      Some(item) => graph_node_from_item(item, db, nonterm_types),
      None => GraphNode::Undefined,
    },
    ASTNode::AST_IndexReference(rf) => {
      match get_item_at_sym_ref(item, db, |_, sym| sym.original_index as usize == rf.value as usize) {
        Some(item) => graph_node_from_item(item, db, nonterm_types),
        None => GraphNode::Undefined,
      }
    }
    ASTNode::AST_Add(add) => {
      let l = resolve_node(db, &add.left, item, nonterm_types);
      let r = resolve_node(db, &add.right, item, nonterm_types);
      match l.get_type() {
        AscriptType::Scalar(l_type) => match l_type {
          _ => todo!("resolve add of {l_type:?} {r:?}"),
        },
        AscriptType::Aggregate(l_type) => match l_type {
          _ => todo!("resolve add of {l_type:?} {r:?}"),
        },
        _ => unreachable!(),
      }
    }
    ASTNode::AST_STRING(str) => {
      if let Some(init) = &str.initializer {
        let gn = resolve_node(db, &init.expression, item, nonterm_types);
        GraphNode::Str(Some(Rc::new(gn)))
      } else {
        GraphNode::Str(None)
      }
    }
    ASTNode::AST_BOOL(bool) => {
      if let Some(init) = &bool.initializer {
        let gn = resolve_node(db, &init.expression, item, nonterm_types);
        GraphNode::Bool(Some(Rc::new(gn)))
      } else {
        GraphNode::Bool(None)
      }
    }
    node => todo!("handle graph resolve of node {node:?}"),
  }
}

fn get_resolved_type(a: AscriptType, b: AscriptType) -> Option<AscriptType> {
  match b {
    AscriptType::Undefined => Some(a),
    AscriptType::Aggregate(b_agg) => match a {
      AscriptType::Aggregate(a_agg) => match b_agg {
        AscriptAggregateType::Map { key_type: b_key_type, val_type: b_val_type } => match a_agg {
          AscriptAggregateType::Map { key_type: a_key_type, val_type: a_val_type } => {
            let key_type = get_resolved_type(AscriptType::Scalar(a_key_type), AscriptType::Scalar(b_key_type))?.as_scalar()?;
            let val_type = get_resolved_type(AscriptType::Scalar(a_val_type), AscriptType::Scalar(b_val_type))?.as_scalar()?;
            Some(AscriptType::Aggregate(AscriptAggregateType::Map { key_type, val_type }))
          }
          AscriptAggregateType::Vec { base_type: a_base_type } => {
            let base_type = get_resolved_type(AscriptType::Scalar(a_base_type), AscriptType::Scalar(b_val_type))?.as_scalar()?;
            Some(AscriptType::Aggregate(AscriptAggregateType::Vec { base_type }))
          }
        },
        AscriptAggregateType::Vec { base_type: b_base_type } => match a_agg {
          AscriptAggregateType::Map { key_type: a_key_type, val_type: a_val_type } => {
            let base_type = get_resolved_type(AscriptType::Scalar(b_base_type), AscriptType::Scalar(a_val_type))?.as_scalar()?;
            Some(AscriptType::Aggregate(AscriptAggregateType::Vec { base_type }))
          }
          AscriptAggregateType::Vec { base_type: a_base_type } => {
            let base_type = get_resolved_type(AscriptType::Scalar(b_base_type), AscriptType::Scalar(a_base_type))?.as_scalar()?;
            Some(AscriptType::Aggregate(AscriptAggregateType::Vec { base_type }))
          }
        },
      },
      AscriptType::Undefined => Some(b),
      A_ => todo!("resolve different scalar types {a:?} {b:?}"),
    },
    AscriptType::Scalar(a_scalar) => match a {
      AscriptType::Undefined => Some(b),
      AscriptType::Scalar(b_scalar) => {
        if std::mem::discriminant(&a_scalar) == std::mem::discriminant(&b_scalar) {
          Some(a)
        } else {
          todo!("resolve different scalar types {a:?} {b:?}")
        }
      }
      _ => todo!("Resolve types scaler:{a:?} ty:{b:?}"),
    },
    _ => todo!("Resolve types ty:{a:?} ty:{b:?}"),
  }
}
