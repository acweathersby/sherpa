use crate::{
  output_base::{AscriptWriterUtils, TokenCreationType},
  types::AScriptTypeVal,
};

use std::collections::BTreeSet;

// Writing stages.
// Preamble data -
//  - Base type info

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy)]
pub enum RefIndex {
  Tok(SlotIndex),
  Obj(SlotIndex),
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SlotIndex {
  /// Indicates the entire output of the rule is to be referenced. This is only
  /// valid when referencing the recognized substring of the rule, such as the
  /// token the spans the rule's characters.
  Rule,
  /// Reference the substring of a symbol found at a specific positional index
  /// of the rule. Also represents derivative variables within a reducer
  /// function.
  Sym(usize),
  /// An object constructed within the reduce function.
  Constructed(usize),
}
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone)]
pub struct SlotRef {
  pub(crate) slot_type: RefIndex,
  pub(crate) type_slot: usize,
  pub(crate) init_expression: String,
  pub ast_type: AScriptTypeVal,
  pub(crate) predecessors: Option<Vec<Box<SlotRef>>>,
  pub(crate) post_init_statements: Option<Vec<String>>,
  pub(crate) is_mutable: bool,
}

impl SlotRef {
  pub fn ast_obj(slot_index: SlotIndex, type_slot: usize, init_expression: String, ast_type: AScriptTypeVal) -> Self {
    SlotRef {
      slot_type: RefIndex::Obj(slot_index),
      type_slot,
      init_expression,
      ast_type,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
    }
  }

  pub fn token(utils: &AscriptWriterUtils, slot_index: SlotIndex, type_slot: usize) -> Self {
    SlotRef {
      slot_type: RefIndex::Tok(slot_index),
      type_slot,
      init_expression: (utils.create_token)((utils.get_token_name)(slot_index), TokenCreationType::Token),
      ast_type: AScriptTypeVal::Token,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
    }
  }

  pub fn range(utils: &AscriptWriterUtils, slot_index: SlotIndex, type_slot: usize) -> Self {
    SlotRef {
      slot_type: RefIndex::Tok(slot_index),
      type_slot,
      init_expression: (utils.get_token_name)(slot_index),
      ast_type: AScriptTypeVal::TokenRange,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
    }
  }

  pub fn node_token(utils: &AscriptWriterUtils, type_slot: usize) -> Self {
    SlotRef {
      slot_type: RefIndex::Tok(SlotIndex::Rule),
      type_slot,
      init_expression: (utils.create_token)((utils.get_token_name)(SlotIndex::Rule), TokenCreationType::Token),
      ast_type: AScriptTypeVal::Token,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
    }
  }

  pub fn node_range(utils: &AscriptWriterUtils, type_slot: usize) -> Self {
    SlotRef {
      slot_type: RefIndex::Tok(SlotIndex::Rule),
      type_slot,
      init_expression: (utils.get_token_name)(SlotIndex::Rule),
      ast_type: AScriptTypeVal::TokenRange,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
    }
  }

  pub fn to_range(self, utils: &AscriptWriterUtils) -> Self {
    let i = match self.get_root_slot_index() {
      RefIndex::Obj(i) | RefIndex::Tok(i) => i,
    };

    Self::range(utils, i, self.type_slot)
  }

  pub fn is_local(&self) -> bool {
    match self.slot_type {
      RefIndex::Obj(SlotIndex::Constructed(_i)) | RefIndex::Tok(SlotIndex::Constructed(_i)) => true,
      _ => false,
    }
  }

  pub fn to_token(self, utils: &AscriptWriterUtils) -> Self {
    let i = match self.get_root_slot_index() {
      RefIndex::Obj(i) | RefIndex::Tok(i) => i,
    };

    SlotRef {
      slot_type: RefIndex::Tok(i),
      type_slot: self.type_slot,
      init_expression: (utils.create_token)((utils.get_token_name)(i), TokenCreationType::Token),
      ast_type: AScriptTypeVal::Token,
      predecessors: None,
      post_init_statements: None,
      is_mutable: false,
    }
  }

  pub fn get_type(&self) -> AScriptTypeVal {
    self.ast_type.clone()
  }

  pub fn to(self, conversion_expr: String, ast_type: AScriptTypeVal) -> Self {
    SlotRef {
      slot_type: self.slot_type,
      type_slot: self.type_slot,
      init_expression: conversion_expr,
      ast_type,
      predecessors: Some(vec![Box::new(self)]),
      post_init_statements: None,
      is_mutable: false,
    }
  }

  /// Unsure the slot type is a ASTNode object, that is, if the current type
  /// is a `TokenRange` convert it into a `Token`
  pub fn ensure_ast_obj(self, utils: &AscriptWriterUtils) -> Self {
    if self.is(AScriptTypeVal::TokenRange) {
      self.to_token(utils)
    } else {
      self
    }
  }

  pub fn make_mutable(&mut self) -> &mut Self {
    self.is_mutable = true;
    self
  }

  pub fn is(&self, type_: AScriptTypeVal) -> bool {
    self.ast_type == type_
  }

  pub fn get_ref_name(&self) -> String {
    match self.slot_type {
      RefIndex::Obj(SlotIndex::Constructed(i)) => format!("var_{i}_{}", self.type_slot),
      RefIndex::Obj(SlotIndex::Sym(i)) => format!("obj_{i}_{}", self.type_slot),
      RefIndex::Tok(SlotIndex::Sym(i)) => format!("tok_{i}_{}", self.type_slot),
      RefIndex::Tok(SlotIndex::Rule) => format!("tok_rule_{}", self.type_slot),
      _ => unreachable!("Invalid Configuration"),
    }
  }

  pub fn get_root_slot_index(&self) -> RefIndex {
    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        return predecessor.get_root_slot_index();
      }
    }
    self.slot_type
  }

  pub fn get_ast_obj_indices(&self) -> BTreeSet<SlotIndex> {
    let mut set = BTreeSet::new();

    if let RefIndex::Obj(index) = self.slot_type {
      set.insert(index);
    }
    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        set.append(&mut predecessor.get_ast_obj_indices());
      }
    }

    set
  }

  pub fn get_token_indices(&self) -> BTreeSet<SlotIndex> {
    let mut set = BTreeSet::new();

    if let RefIndex::Tok(index) = self.slot_type {
      set.insert(index);
    } else {
      if let Some(predecessors) = &self.predecessors {
        for predecessor in predecessors {
          set.append(&mut predecessor.get_token_indices());
        }
      }
    }

    set
  }

  pub fn add_post_init_stmt(&mut self, string: String) -> &mut Self {
    self.post_init_statements.get_or_insert(vec![]).push(string);
    self
  }

  /// Convert the ref into a string of statements that convert original
  /// type into it current form.
  pub fn to_init_string(&self, utils: &AscriptWriterUtils) -> String {
    let mut strings = Vec::new();

    if let Some(predecessors) = &self.predecessors {
      for predecessor in predecessors {
        strings.push(predecessor.to_init_string(utils));
      }
    }

    let ref_string = self.get_ref_name();

    strings.push((utils.assignment_writer)(
      utils,
      &self.ast_type,
      ref_string.clone(),
      self.init_expression.replace("%%", &ref_string),
      self.is_mutable,
    ));

    if let Some(statements) = &self.post_init_statements {
      strings.append(&mut statements.clone());
    }

    strings.join("\n").replace("%%", &ref_string)
  }

  pub fn add_predecessor(&mut self, predecessor: SlotRef) -> &mut Self {
    self.predecessors.get_or_insert(vec![]).push(Box::new(predecessor));

    self
  }

  pub fn add_predecessors(&mut self, predecessors: Vec<SlotRef>) -> &mut Self {
    let prev = self.predecessors.get_or_insert(vec![]);

    for predecessor in predecessors {
      prev.push(Box::new(predecessor))
    }

    self
  }
}
