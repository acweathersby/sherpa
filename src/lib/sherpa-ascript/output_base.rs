use crate::{
  errors::ascript_error_class,
  slot_ref::{SlotIndex, SlotRef},
};
use sherpa_core::{
  parser::{ASTNode, ASTNodeType, AST_IndexReference, AST_NamedReference, AST_Struct, AST_Vector, GetASTNodeType},
  proxy::Array,
  *,
};
use sherpa_rust_runtime::types::Token;

use super::{compile::get_struct_type_from_node, types::*};
use std::{
  collections::{hash_map, BTreeMap, BTreeSet, HashMap},
  fmt::Display,
  io::Write,
  mem::Discriminant,
  path::PathBuf,
  vec,
};

#[derive(Clone)]
pub struct AscriptTypeHandler<'a> {
  pub name:    &'a dyn Fn(&AScriptStore, &AScriptTypeVal, bool) -> String,
  pub default: &'a dyn Fn(&AScriptStore, &AScriptTypeVal, bool) -> String,
}

#[derive(Clone)]
pub struct ASTExprHandler<'a> {
  /// ```no_compile
  /// pub fn render_expression(
  ///      utils: &AscriptWriterUtils,
  ///      ast: &ASTNode,
  ///      rule: &Rule,
  ///      ref_index: &mut usize,
  ///      type_slot: usize,
  /// ) -> Option<Ref>
  /// ```
  pub expr: &'a dyn Fn(&AscriptWriterUtils, &ASTNode, &Rule, &mut usize, usize) -> Option<SlotRef>,
}

pub type PropHandlerFn = dyn Fn(&AscriptWriterUtils, Option<SlotRef>, &AScriptTypeVal, bool) -> (String, Option<SlotRef>);

pub struct AscriptPropHandler<'a> {
  /// ```no_compile
  /// pub fn render_expression(
  ///      utils: &AscriptWriterUtils,
  ///      ast: &ASTNode,
  ///      rule: &Rule,
  ///      ref_index: &mut usize,
  ///      type_slot: usize,
  /// ) -> Option<Ref>
  /// ```
  pub expr: &'a PropHandlerFn,
}

type AssignmentWriter<'a> = dyn Fn(&AscriptWriterUtils, &AScriptTypeVal, String, String, bool) -> String;

type SlotAssign<'a> = dyn Fn(&AscriptWriterUtils, &AScriptTypeVal, String, bool) -> String;

/// Called when a Node struct needs to be constructed.
type StructConstructorExpr =
  dyn Fn(&AscriptWriterUtils, &mut CodeWriter<Vec<u8>>, String, Vec<(String, String, AScriptTypeVal)>, bool) -> SherpaResult<()>;

/// Token Concatenation
///
/// The expressions used to concatenate two parse tokens. The
/// resulting token should represent a span of characters from
/// the beginning of the first token to the end of the last token.
///
/// Example Function:
/// ```no_compile
/// fn token_concat(first:String, last:String) -> String {
///     format!("{first} + {last}")
/// }
/// ```
type TokenConcat = dyn Fn(String, String) -> String;

type GetName = dyn Fn(SlotIndex) -> String;

/// Slot Extraction:
///
/// Expression to extract and assign either the node value or token from a
/// parse slot, or both.
type SlotExtract = dyn Fn(Option<String>, Option<String>, usize) -> String;

/// Slot Extraction:
///
/// Expression to extract and assign either the node value or token from a
/// parse slot, or both.
type CreateToken = dyn Fn(String, TokenCreationType) -> String;

pub enum TokenCreationType {
  #[allow(unused)]
  String,
  Token,
}

pub struct StructProp<'db> {
  pub name:        String,
  pub type_string: String,
  pub type_:       &'db TaggedType,
  pub optional:    bool,
}
pub struct StructData<'a> {
  pub name:      String,
  pub props:     Vec<StructProp<'a>>,
  pub tokenized: bool,
}

#[derive(Default)]
pub struct Handlers<'db> {
  type_handlers: HashMap<Discriminant<AScriptTypeVal>, AscriptTypeHandler<'db>>,
  prop_handlers: HashMap<Discriminant<AScriptTypeVal>, AscriptPropHandler<'db>>,
  expr_handlers: HashMap<ASTNodeType, ASTExprHandler<'db>>,
}

pub struct AscriptWriterUtils<'a> {
  /// Internal use. Assign to `Default::default()`
  pub handlers: Handlers<'a>,
  /// General Assignment
  pub assignment_writer: &'a AssignmentWriter<'a>,
  pub slot_assign: &'a SlotAssign<'a>,
  pub token_concat: &'a TokenConcat,
  /// Slot Extraction:
  ///
  /// Expression to extract and assign either the node value or token, or both,
  /// from a parse slot.
  pub slot_extract: &'a SlotExtract,
  pub create_token: &'a CreateToken,
  /// Name used for token variables
  pub get_token_name: &'a GetName,
  pub get_slot_obj_name: &'a GetName,
  pub struct_construction: &'a StructConstructorExpr,

  pub store: &'a AScriptStore,
  pub db:    &'a ParserDatabase,
}

impl<'a> AscriptWriterUtils<'a> {
  #[track_caller]
  pub fn add_type_handler(&mut self, type_: AScriptTypeVal, handler: AscriptTypeHandler<'a>) {
    match self.handlers.type_handlers.entry(type_.get_discriminant()) {
      hash_map::Entry::Occupied(_) => {
        #[cfg(debug_assertions)]
        panic!("Type handler already registered for [{}]", type_.debug_string());
        #[cfg(not(debug_assertions))]
        panic!()
      }
      hash_map::Entry::Vacant(e) => {
        e.insert(handler);
      }
    }
  }

  #[track_caller]
  pub fn add_ast_handler(&mut self, type_: ASTNodeType, handler: ASTExprHandler<'a>) {
    match self.handlers.expr_handlers.entry(type_) {
      hash_map::Entry::Occupied(_) => {
        #[cfg(debug_assertions)]
        panic!("Type handler already registered for [{:?}]", type_);
        #[cfg(not(debug_assertions))]
        panic!()
      }
      hash_map::Entry::Vacant(e) => {
        e.insert(handler);
      }
    }
  }

  #[track_caller]
  pub fn add_prop_handler(&mut self, type_: AScriptTypeVal, handler: AscriptPropHandler<'a>) {
    match self.handlers.prop_handlers.entry(type_.get_discriminant()) {
      hash_map::Entry::Occupied(_) => {
        #[cfg(debug_assertions)]
        panic!("Prop handler already registered for [{:?}]", type_);
        #[cfg(not(debug_assertions))]
        panic!()
      }
      hash_map::Entry::Vacant(e) => {
        e.insert(handler);
      }
    }
  }

  /// Increase the value of the monotonic reference index counter.
  pub fn bump_ref_index(&self, ref_index: &mut usize) -> usize {
    *ref_index += 1;
    *ref_index
  }

  pub fn ascript_type_to_string(&self, type_: &AScriptTypeVal, optional: bool) -> String {
    let discriminant = type_.get_discriminant();
    if let Some(type_handler) = self.handlers.type_handlers.get(&discriminant) {
      (*type_handler.name)(self.store, type_, optional)
    } else {
      #[cfg(debug_assertions)]
      {
        format!("[UNHANDLED {}]", type_.debug_string())
      }
      #[cfg(not(debug_assertions))]
      {
        "[UNHANDLED]".into()
      }
    }
  }

  pub fn ascript_type_to_default_string(&self, type_: &AScriptTypeVal, optional: bool) -> String {
    let discriminant = type_.get_discriminant();
    if let Some(type_handler) = self.handlers.type_handlers.get(&discriminant) {
      (*type_handler.default)(self.store, type_, optional)
    } else {
      #[cfg(debug_assertions)]
      {
        format!("[UNHANDLED {}]", type_.debug_string())
      }
      #[cfg(not(debug_assertions))]
      {
        "[UNHANDLED]".into()
      }
    }
  }

  pub fn ast_expr_to_ref(&self, ast: &ASTNode, rule: &Rule, ref_index: &mut usize, type_slot: usize) -> Option<SlotRef>
  where
    Self: Sized,
  {
    if let Some(expr_handler) = self.handlers.expr_handlers.get(&ast.get_type()) {
      (*expr_handler.expr)(self, ast, rule, ref_index, type_slot)
    } else {
      #[cfg(debug_assertions)]
      {
        panic!("{}", SherpaError::SourceError {
          loc:        ast.to_token(),
          path:       PathBuf::from(rule.g_id.path.to_string(self.db.string_store())),
          id:         (ascript_error_class(), 0, "ascript-writer-utils-unhandled-ast-node").into(),
          msg:        format!("An unhandled ast node has been encountered"),
          inline_msg: format!("Node type [{:?}] lacks an ASTExprHandler", ast.get_type()),
          ps_msg:     "Add an ASTExprHandler for this type using AscriptWriterUtils::add_ast_handler".into(),
          severity:   SherpaErrorSeverity::Warning,
        })
      }
      #[cfg(not(debug_assertions))]
      panic!("{}", SherpaError::SourceError {
        loc:        ast.to_token(),
        path:       PathBuf::from(rule.g_id.path.to_string(self.db.string_store())),
        id:         (ascript_error_class(), 0, "ascript-writer-utils-unhandled-ast-node").into(),
        msg:        format!("An unhandled ast node has been encountered"),
        inline_msg: format!("Node type lacks an ASTExprHandler"),
        ps_msg:     "Add an ASTExprHandler for this type using AscriptWriterUtils::add_ast_handler".into(),
        severity:   SherpaErrorSeverity::Warning,
      })
    }
  }

  /// Creates struct constructor from a specific struct configuration.
  /// Per configuration, certain props may or may not be initialized.
  pub fn build_struct_constructor(
    &self,
    rule: &Rule,
    struct_type: &AScriptStructId,
    ast_struct: &AST_Struct,
    ref_index: &mut usize,
    type_slot: usize,
  ) -> SherpaResult<SlotRef> {
    let store = self.store;
    let archetype_struct = store.structs.get(struct_type).unwrap();
    let ast_struct_props = ast_struct
      .props
      .iter()
      .filter_map(|p| if let ASTNode::AST_Property(prop) = p { Some((prop.id.clone(), prop)) } else { None })
      .collect::<BTreeMap<_, _>>();

    let mut predecessors = vec![];

    let prop_assignments = archetype_struct
      .prop_ids
      .iter()
      .enumerate()
      .map(|(i, prop_id)| {
        let prop_val = store.props.get(prop_id);
        let struct_prop_val = match prop_val {
          Some(prop) => {
            if let Some(ast_prop) = ast_struct_props.get(&prop_id.name) {
              let property = store.props.get(prop_id).unwrap();

              let ref_val = ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
                value: ast_prop.id.clone(),
                tok:   ast_prop.tok.clone(),
              }));

              let value = match &ast_prop.value {
                Some(value) => value,
                _ => &ref_val,
              };

              match self.ast_expr_to_ref(value, rule, ref_index, i + type_slot * 100) {
                Some(ref_) => {
                  let (string, ref_) =
                    self.create_type_initializer_value(Some(ref_), &(&property.type_val).into(), property.optional);
                  if let Some(ref_) = ref_ {
                    predecessors.push(ref_);
                  }

                  string
                }
                _ => self.ascript_type_to_default_string(&(&prop.type_val).into(), prop.optional),
              }
            } else {
              self.ascript_type_to_default_string(&(&prop.type_val).into(), prop.optional)
            }
          }
          _ => self.ascript_type_to_default_string(&(&AScriptTypeVal::Undefined), false),
        };

        (prop_id.name.clone(), struct_prop_val, AScriptTypeVal::Undefined)
      })
      .collect();

    let mut writer = CodeWriter::new(vec![]);

    (self.struct_construction)(
      self,
      &mut writer,
      archetype_struct.type_name.clone(),
      prop_assignments,
      archetype_struct.tokenized,
    )?;

    (*ref_index) += 1;

    let mut ref_ = SlotRef::ast_obj(
      SlotIndex::Constructed(*ref_index),
      type_slot,
      String::from_utf8(writer.into_output()).unwrap(),
      AScriptTypeVal::Struct(*struct_type),
    );

    ref_.add_predecessors(predecessors);

    SherpaResult::Ok(ref_)
  }

  /// Used to convert a value into an appropriate form to assign as
  /// a struct's  value.
  pub fn create_type_initializer_value(
    &self,
    ref_: Option<SlotRef>,
    type_val: &AScriptTypeVal,
    optional: bool,
  ) -> (String, Option<SlotRef>) {
    match self.handlers.prop_handlers.get(&type_val.get_discriminant()) {
      Some(AscriptPropHandler { expr }) => (*expr)(&self, ref_, type_val, optional),
      _ => (ref_.clone().map(|ref_| ref_.get_ref_name()).unwrap_or_default(), ref_),
    }
  }
}

pub struct AscriptWriter<'a, W: Write> {
  pub store: &'a AScriptStore,
  pub db:    &'a ParserDatabase,
  pub utils: &'a AscriptWriterUtils<'a>,
  writer:    CodeWriter<W>,
}

impl<'a, W: Write> AscriptWriter<'a, W> {
  /// Writes a multiline block structure to the output.
  pub fn new(utils: &'a AscriptWriterUtils, writer: CodeWriter<W>) -> Self {
    Self { store: utils.store, db: utils.db, writer, utils }
  }

  fn get_struct_data(&self) -> BTreeMap<AScriptStructId, StructData<'a>> {
    self
      .store
      .structs
      .values()
      .map(|s| {
        (s.id, StructData {
          name:      s.type_name.clone(),
          tokenized: s.tokenized,
          props:     s
            .prop_ids
            .iter()
            .filter_map(|p_id| {
              self.store.props.get(p_id).map(|p| StructProp {
                name:        p_id.name.clone(),
                type_string: self.utils.ascript_type_to_string(&(&p.type_val).into(), p.optional),
                optional:    p.optional,
                type_:       &p.type_val,
              })
            })
            .collect(),
        })
      })
      .collect()
  }

  pub fn block(
    &mut self,
    block_header: &str,
    open_delim: &str,
    closing_delim: &str,
    content_writer: &dyn Fn(&mut Self) -> SherpaResult<()>,
  ) -> SherpaResult<()> {
    self.writer.write_line("\n")?;

    if !block_header.is_empty() {
      self.writer.write(&block_header)?;
    }

    self.writer.write(open_delim)?;
    self.writer.increase_indent();

    content_writer(self)?;

    self.writer.decrease_indent();
    self.writer.write_line(closing_delim)?;

    SherpaResult::Ok(())
  }

  fn checkpoint<B: Write + Default>(&self) -> AscriptWriter<'a, B> {
    AscriptWriter {
      store:  self.store,
      db:     self.db,
      writer: self.writer.checkpoint::<B>(),
      utils:  self.utils,
    }
  }

  /// Writes a single stmt string on a new line.
  pub fn stmt(&mut self, stmt: String) -> SherpaResult<()> {
    self.writer.write_line(&stmt)?;
    SherpaResult::Ok(())
  }

  pub fn method(
    &mut self,
    preamble: &str,
    args_open_delim: &str,
    args_close_delim: &str,
    args_seperator: &str,
    args: &dyn Fn(&mut Self) -> Vec<String>,
    postamble: &str,
    open_delim: &str,
    closing_delim: &str,
    content_writer: &mut dyn FnMut(&mut Self) -> SherpaResult<()>,
  ) -> SherpaResult<()> {
    self.writer.write_line("\n")?;
    self.writer.write(preamble)?;
    self.writer.write(" ")?;
    self.writer.write(args_open_delim)?;
    let args = args(self).join(args_seperator);
    self.writer.write(&args)?;
    self.writer.write(args_close_delim)?;
    self.writer.write(postamble)?;
    self.writer.write(" ")?;
    self.writer.write(open_delim)?;
    self.writer.increase_indent();
    content_writer(self)?;
    self.writer.decrease_indent();
    self.writer.write_line(closing_delim)?;
    SherpaResult::Ok(())
  }

  pub fn list<S: Display>(&mut self, delim: &str, data: Vec<S>) -> SherpaResult<()> {
    for datum in data {
      self.writer.write_line(&datum.to_string())?;
      self.writer.write(delim)?;
    }

    SherpaResult::Ok(())
  }

  pub fn write_struct_data<'b: 'a>(
    &mut self,
    struct_write_script: &dyn Fn(&mut Self, &StructData) -> SherpaResult<()>,
  ) -> SherpaResult<()> {
    for (_, struct_data) in self.get_struct_data() {
      struct_write_script(self, &struct_data)?;
    }

    SherpaResult::Ok(())
  }

  fn write_slot_extraction(
    &mut self,
    rule: &Rule,
    obj_indices: BTreeSet<SlotIndex>,
    token_indices: BTreeSet<SlotIndex>,
  ) -> SherpaResult<()> {
    for (slot_index, _) in rule.symbols.iter().enumerate() {
      let slot_id = SlotIndex::Sym(slot_index);
      let (token_ref, node_ref) = (
        match (slot_index, token_indices.contains(&slot_id)) {
          (0, _) => Some((self.utils.get_token_name)(slot_id)),
          (i, _) if i == (rule.symbols.len() - 1) => Some((self.utils.get_token_name)(slot_id)),
          (_, true) => Some((self.utils.get_token_name)(slot_id)),
          _ => None,
        },
        obj_indices.contains(&slot_id).then(|| (&self.utils.get_slot_obj_name)(slot_id)),
      );

      self.writer.wrtln(&(self.utils.slot_extract)(token_ref, node_ref, slot_index))?;
    }
    SherpaResult::Ok(())
  }

  fn write_node_token(&mut self, rule: &Rule) -> SherpaResult<()> {
    let type_ = AScriptTypeVal::Token;
    let string = (self.utils.assignment_writer)(
      self.utils,
      &type_,
      (self.utils.get_token_name)(SlotIndex::Rule),
      if rule.symbols.len() > 1 {
        (self.utils.token_concat)(
          (self.utils.get_token_name)(SlotIndex::Sym(0)),
          (self.utils.get_token_name)(SlotIndex::Sym(rule.symbols.len() - 1)),
        )
      } else {
        (self.utils.get_token_name)(SlotIndex::Sym(0))
      },
      false,
    );
    self.writer.write_line(&string)?;
    SherpaResult::Ok(())
  }

  pub fn write_reduce_functions(
    &mut self,
    preamble: &str,
    args_open_delim: &str,
    args_close_delim: &str,
    args_seperator: &str,
    args: &dyn Fn(&mut AscriptWriter<Vec<u8>>) -> Vec<String>,
    postamble: &str,
    open_delim: &str,
    closing_delim: &str,
    reduce_fn_writer: &dyn Fn(&mut Self, &Vec<String>) -> SherpaResult<()>,
  ) -> SherpaResult<()> {
    let store = self.store;
    let db = self.db;
    let rules = db.rules();
    let ordered_rules = rules
      .iter()
      .enumerate()
      .filter_map(|(id, rule)| {
        let nterm_sym = db.nonterm_sym(rule.nonterm);
        match nterm_sym {
          SymbolId::NonTerminal { .. } => Some((id, rule)),
          _ => None,
        }
      })
      .collect::<Array<_>>();

    let mut reduce_functions_map = Vec::new();

    for (bc_id, rule) in &ordered_rules {
      let nterm = rule.nonterm;
      let nterm_data = store.nonterm_types.get(&nterm).unwrap();
      let rule = &rule.rule;

      #[cfg(debug_assertions)]
      {
        if nterm_data.len() != 1 {
          unreachable!(
            "\n\nNon-terminal result not been resolved\n[{}] == {}\n\n\n{}\n\n",
            db.nonterm_friendly_name_string(nterm),
            rule.tok.blame(1, 1, "", sherpa_rust_runtime::types::BlameColor::RED),
            nterm_data.iter().map(|(p, _)| { p.debug_string() }).collect::<Vec<_>>().join("\n")
          )
        };
      }

      let mut w = self.checkpoint::<Vec<u8>>();
      let fn_name = format!("reducer_{:0>3}", bc_id);

      match w.method(
        &format!("\n/* {} */\n{}", rule.tok.to_string().replace("*/", "* /"), preamble.replace("%%", &fn_name)),
        args_open_delim,
        args_close_delim,
        args_seperator,
        args,
        postamble,
        open_delim,
        closing_delim,
        &mut move |w| -> SherpaResult<()> {
          let mut ref_index = rule.symbols.len();
          match &rule.ast {
            Some(ASTToken::Defined(ascript)) => match &ascript.ast {
              ASTNode::AST_Struct(box ast_struct) => {
                if let AScriptTypeVal::Struct(struct_type) = get_struct_type_from_node(&ast_struct) {
                  let _ref = w.utils.build_struct_constructor(rule, &struct_type, &ast_struct, &mut ref_index, 0)?;

                  let obj_indices = _ref.get_ast_obj_indices();
                  let token_indices = _ref.get_token_indices();

                  w.write_slot_extraction(rule, obj_indices, token_indices)?;
                  w.write_node_token(rule)?;
                  w.writer.write_line(&_ref.to_init_string(w.utils))?;
                  w.writer.wrtln(&(w.utils.slot_assign)(
                    w.utils,
                    &AScriptTypeVal::Struct(struct_type),
                    _ref.get_ref_name(),
                    _ref.is_local(),
                  ))?;
                }
                SherpaResult::Ok(())
              }
              ASTNode::AST_Statements(box statements) => {
                let mut reference = String::new();
                let mut return_type = AScriptTypeVal::Undefined;
                let mut refs = BTreeSet::new();
                let mut tokens = BTreeSet::new();
                let mut stmt = w.checkpoint();
                let mut is_local: bool = false;

                for (i, statement) in statements.statements.iter().enumerate() {
                  match stmt.utils.ast_expr_to_ref(statement, rule, &mut ref_index, i) {
                    Some(_ref) => {
                      refs.append(&mut _ref.get_ast_obj_indices());
                      tokens.append(&mut _ref.get_token_indices());
                      return_type = _ref.ast_type.clone();
                      reference = _ref.get_ref_name();
                      is_local = _ref.is_local();
                      stmt.writer.write_line(&_ref.to_init_string(w.utils))?;
                    }
                    _ => {
                      #[cfg(debug_assertions)]
                      panic!("{}", SherpaError::SourceError {
                        loc:        statement.to_token(),
                        id:         (ascript_error_class(), 2, "invalid-expression-value").into(),
                        msg:        "Could not resolve this expression".to_string(),
                        inline_msg: Default::default(),
                        ps_msg:     Default::default(),
                        severity:   Default::default(),
                        path:       Default::default(),
                      });
                      #[cfg(not(debug_assertions))]
                      panic!()
                    }
                  }
                }

                w.write_slot_extraction(rule, refs, tokens)?;
                w.write_node_token(rule)?;
                w.writer.merge_checkpoint(stmt.writer)?;

                let return_type = match return_type {
                  AScriptTypeVal::Undefined | AScriptTypeVal::GenericVec(None) => nterm_data.iter().next().unwrap().0.into(),
                  r => r,
                };

                w.writer.wrtln(&(w.utils.slot_assign)(w.utils, &return_type, reference, is_local))?;

                SherpaResult::Ok(())
              }
              _ => unreachable!("Type should not be a root ascript node."),
            },

            Some(ASTToken::ListIterate(_)) | Some(ASTToken::ListEntry(_)) => {
              let mut items = vec![ASTNode::AST_IndexReference(Box::new(AST_IndexReference::new(1, Default::default())))];

              if matches!(rule.ast, Some(ASTToken::ListIterate(_))) {
                items.push(ASTNode::AST_IndexReference(Box::new(AST_IndexReference::new(
                  (rule.symbols.len()) as i64,
                  Default::default(),
                ))));
              }

              let node = ASTNode::AST_Vector(Box::new(AST_Vector::new(items, Default::default())));
              let mut refs = BTreeSet::new();
              let mut tokens = BTreeSet::new();
              let mut stmt = w.checkpoint();
              let reference: String;
              let return_type: AScriptTypeVal;
              let is_local: bool;

              match stmt.utils.ast_expr_to_ref(&node, rule, &mut ref_index, 0) {
                Some(_ref) => {
                  refs.append(&mut _ref.get_ast_obj_indices());
                  tokens.append(&mut _ref.get_token_indices());
                  return_type = _ref.ast_type.clone();
                  reference = _ref.get_ref_name();
                  is_local = _ref.is_local();
                  stmt.writer.write_line(&_ref.to_init_string(w.utils))?;
                }
                _ => unreachable!(),
              }

              w.write_slot_extraction(rule, refs, tokens)?;
              w.write_node_token(rule)?;
              w.writer.merge_checkpoint(stmt.writer)?;

              let return_type = match return_type {
                AScriptTypeVal::Undefined | AScriptTypeVal::GenericVec(None) => nterm_data.iter().next().unwrap().0.into(),
                r => r,
              };

              w.writer.wrtln(&(w.utils.slot_assign)(w.utils, &return_type, reference, is_local))?;

              SherpaResult::Ok(())
            }
            None => {
              let last_index = rule.symbols.len() - 1;
              let last_index_name = (&w.utils.get_slot_obj_name)(SlotIndex::Sym(last_index));
              w.write_slot_extraction(
                rule,
                BTreeSet::from_iter(vec![SlotIndex::Sym(last_index)]),
                BTreeSet::from_iter(vec![SlotIndex::Rule, SlotIndex::Sym(last_index)]),
              )?;
              w.write_node_token(rule)?;
              w.writer.wrtln(&(w.utils.slot_assign)(w.utils, &AScriptTypeVal::Any, last_index_name, false))?;
              SherpaResult::Ok(())
            }
          }
        },
      ) {
        SherpaResult::Ok(()) => {
          self.writer.merge_checkpoint(w.writer)?;
          reduce_functions_map.push(fn_name);
        }
        _ => panic!("Invalid Result"),
      }
    }

    reduce_fn_writer(self, &reduce_functions_map)?;

    SherpaResult::Ok(())
  }

  pub fn into_writer(self) -> CodeWriter<W> {
    self.writer
  }
}

pub fn get_ascript_export_data(
  utils: &AscriptWriterUtils,
) -> Vec<(Option<SlotRef>, AScriptTypeVal, String, String, String, String)> {
  let db = utils.db;
  let export_node_data = db
    .entry_points()
    .into_iter()
    .filter(|e| e.is_export)
    .map(|DBEntryPoint { entry_name, nonterm_name, nonterm_entry_name, nonterm_key, .. }| {
      let mut ref_index = 0;
      let ref_ = utils.ast_expr_to_ref(
        &ASTNode::AST_NamedReference(Box::new(AST_NamedReference {
          tok:   Token::default(),
          value: ASCRIPT_FIRST_NODE_ID.to_string(),
        })),
        &Rule {
          symbols: vec![SymbolRef {
            id: SymbolId::DBNonTerminal { key: *nonterm_key },
            ..Default::default()
          }],
          g_id:    db.rule(db.nonterm_rules(*nonterm_key).unwrap_or_else(|_| panic!("Incorrect db key"))[0]).g_id,
          skipped: Default::default(),
          tok:     Default::default(),
          ast:     None,
        },
        &mut ref_index,
        0,
      );
      let ast_type = ref_.as_ref().unwrap().get_type();
      let ast_type_string = utils.ascript_type_to_string(&ast_type, false);
      (
        ref_,
        ast_type,
        ast_type_string,
        entry_name.to_string(db.string_store()).to_string(),
        nonterm_name.to_string(db.string_store()).to_string(),
        nonterm_entry_name.to_string(db.string_store()).to_string(),
      )
    })
    .collect::<Vec<_>>();
  export_node_data
}
