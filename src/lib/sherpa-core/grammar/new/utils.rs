use super::{
  errors::create_missing_import_name_error,
  parser::{
    ASTNode,
    CFProduction,
    PegProduction,
    PrattProduction,
    Production_Import_Symbol,
    Production_Symbol,
    State,
  },
};
use crate::{
  compile::{GrammarId, GrammarRef, GrammarStore, ProductionId, Rule},
  types::{self, RuleId},
  Journal,
  SherpaResult,
};
use std::{collections::btree_map, path::PathBuf, sync::Arc};

pub type ProductionGUIDName = String;
pub type ProductionName = String;

/// Resolves and verifies a grammar file path acquired from an `@IMPORT` statement exists.
///
/// If the file path does not have an extension, attempts are made to assert
/// the existence of the file path when appended with one of the following extension types
/// appended to it: `.hc`, `.hcg` `.grammar`.
///
/// Additionally, if the given file path is relative, then it is appended to the parent dir
/// path of the current grammar, whose path is provided by the `cgd`, current grammar dir,
/// argument.
pub(crate) fn resolve_grammar_path(
  path: &PathBuf,
  cgd: &PathBuf,
  extension: &[&str],
) -> SherpaResult<PathBuf> {
  SherpaResult::Ok(
    match (
      path.is_file(),
      path.extension().is_some(),
      // Ensure path is is an absolute path
      match path.is_absolute() {
        true => (path.to_owned(), false),
        false => (cgd.join(&path), cgd.join(&path).is_file()),
      },
    ) {
      // Path is relative to the given cgd
      (false, _, (path, true)) => path.canonicalize()?,
      // Attempt to verify the file path with different extensions. First valid
      // path wins.
      (false, false, (path, _)) => extension
        .iter()
        .filter_map(|ext| {
          let mut path = path.clone();
          path.set_extension(ext);
          path.canonicalize().ok()
        })
        .next()
        .ok_or(format!("Tried to load file with these extension {:?}", extension))?,

      // Default
      _ => path.canonicalize()?,
    },
  )
}

/// Return the `Production_Import_Symbol` or `Production_Symbol` from a valid node tree.
/// Accepts
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production_Terminal_Symbol]
/// - [ASTNode::State]
/// - [ASTNode::PrattProduction]
/// - [ASTNode::PegProduction]
/// - [ASTNode::CFProduction]
fn get_production_symbol<'a>(
  node: &'a ASTNode,
  g: &'a GrammarStore,
) -> (Option<&'a Production_Symbol>, Option<&'a Production_Import_Symbol>) {
  match node {
    ASTNode::Production_Import_Symbol(prod_import) => (None, Some(prod_import.as_ref())),
    ASTNode::Production_Symbol(prod_sym) => (Some(prod_sym.as_ref()), None),
    ASTNode::Production_Terminal_Symbol(prod_tok) => get_production_symbol(&prod_tok.production, g),
    ASTNode::State(box State { id, .. })
    | ASTNode::PrattProduction(box PrattProduction { name_sym: id, .. })
    | ASTNode::PegProduction(box PegProduction { name_sym: id, .. })
    | ASTNode::CFProduction(box CFProduction { name_sym: id, .. }) => (Some(id.as_ref()), None),
    _ => unreachable!(),
  }
}

/// Get the guid_name and plain_name of the production name symbol.
///
/// Returns `Option<(guid_name: String, friendly_name: String)>`
///
/// This guid_name is guaranteed to be unique amongst all grammars imported by
/// the root grammar.
pub fn get_productions_names_from_ast_node(
  j: &mut Journal,
  g: &GrammarStore,
  name_sym: &ASTNode,
) -> Option<(ProductionGUIDName, ProductionName)> {
  use super::parser::GetASTNodeType;
  match get_production_symbol(name_sym, g) {
    (Some(prod_sym), None) => get_production_names_from_production_symbol(g, prod_sym),
    (None, Some(prod_imp_sym)) => {
      let production_name = &prod_imp_sym.name;
      let local_import_grammar_name = &prod_imp_sym.module;

      match g.imports.get(local_import_grammar_name) {
        None => {
          create_missing_import_name_error(j, g, prod_imp_sym);
          None
        }
        Some(g_ref) => Some((
          create_production_guid_name(&g_ref.guid_name, production_name),
          prod_imp_sym.tok.to_string(),
        )),
      }
    }
    _ => unreachable!("Node cannot be resolved to a production name [{:?}]", name_sym.get_type()),
  }
}

pub fn get_production_names_from_production_symbol(
  g: &GrammarStore,
  prod_sym: &Production_Symbol,
) -> Option<(ProductionGUIDName, ProductionName)> {
  Some((create_production_guid_name(&g.id.guid_name, &prod_sym.name), prod_sym.name.clone()))
}

/// Generate a UUID name using the grammars uuid_name and the
/// productions name (omitting local import name portion of a
/// production)

pub fn create_production_guid_name(grammar_uuid_name: &str, production_name: &str) -> String {
  grammar_uuid_name.to_owned() + GUID_NAME_DELIMITER + production_name
}

/// Used to separate a grammar's uuid name from a production's name
const GUID_NAME_DELIMITER: &str = "_";

pub struct SymbolData<'a> {
  pub annotation:       String,
  pub is_list:          bool,
  pub is_group:         bool,
  pub is_optional:      bool,
  pub is_shift_nothing: bool,
  pub is_eof:           bool,
  pub precedence:       u32,
  pub sym_atom:         Option<&'a ASTNode>,
}

/// Get a flattened view of a symbol's immediate AST
pub fn get_symbol_details<'a>(mut sym: &'a ASTNode) -> SymbolData<'a> {
  let mut data = SymbolData {
    annotation:       String::new(),
    is_list:          false,
    is_group:         false,
    is_optional:      false,
    is_shift_nothing: false,
    is_eof:           false,
    precedence:       0,
    sym_atom:         None,
  };

  loop {
    match sym {
      ASTNode::AnnotatedSymbol(annotated) => {
        if annotated.reference.len() > 0 {

          debug_assert_eq!(
            &annotated.reference[0..1], "^",
            "Annotation values are no longer prefixed with \"^\". The following line needs to be changed to:
            data.annotation = annotated.reference.clone(); 
            This assert can be removed after the change is made.");

            data.annotation = annotated.reference[1..].to_string();
          }
        //data.precedence = annotated.precedence.is_some();
        data.is_optional |= annotated.is_optional;
        sym = &annotated.symbol;
      }
      ASTNode::GroupProduction(_) => {
        data.is_group = true;
        break;
      }
      ASTNode::List_Production(box p) => {
        data.is_list = true;
        data.is_optional |= p.optional;
        break;
      }
      ASTNode::TerminalToken(box t) => {
        data.precedence = data.precedence.max(t.is_exclusive as u32);
        break;
      }
      ASTNode::EOFSymbol(_) => {
        data.is_eof = true;
        break;
      }
      // This symbol types are "real" symbols, in as much
      // as they represent actual parsable entities which are
      // submitted to the bytecode compiler for evaluation
      ASTNode::ClassSymbol(_)
      | ASTNode::NotEmptySet(_)
      | ASTNode::Production_Terminal_Symbol(_)
      //| ASTNode::TemplateProductionSymbol(_)
      | ASTNode::Production_Symbol(_)
      | ASTNode::Production_Import_Symbol(_) => {
        break;
      }
      _ => unreachable!("Did not expect node [{:?}]", sym)
    }
  }

  data.sym_atom = Some(sym);

  data
}

/// Get the resolved grammar data from compatible nodes.
///
/// ASTNodes that can be resolved to a grammar:
/// - [ASTNode::Production_Import_Symbol]
/// - [ASTNode::Production_Symbol]
/// - [ASTNode::Production]
/// - [ASTNode::Production_Terminal_Symbol]
///
/// ## Returns
/// A Tuple comprised of the grammar 0:uuid_name, 1:local_name, and
/// 2:absolute_path. local_name is `root` if the grammar maps to
/// currently rendered grammar.

pub fn get_grammar_info_from_symbol<'a>(
  g: &'a GrammarStore,
  node: &'a ASTNode,
) -> Option<Arc<GrammarRef>> {
  match get_production_symbol(node, g) {
    (None, Some(prod_imp_sym)) => {
      let local_import_grammar_name = &prod_imp_sym.module;

      match g.imports.get(local_import_grammar_name) {
        Some(g_ref) => Some(g_ref.clone()),
        None => unreachable!(),
      }
    }
    (Some(_), None) => Some(g.id.clone()),
    _ => unreachable!(),
  }
}

pub fn insert_rules(g: &mut GrammarStore, prod_id: &ProductionId, rules: Vec<Rule>) -> Vec<RuleId> {
  let offset_index = g.production_rules.get(&prod_id).map_or(0, |b| b.len());

  let body_ids = rules
    .into_iter()
    .enumerate()
    .map(|(i, mut b)| {
      let id = RuleId::new(&prod_id, offset_index + i);
      b.id = id;
      g.rules.insert(id, b);
      id
    })
    .collect::<Vec<_>>();

  match g.production_rules.entry(*prod_id) {
    btree_map::Entry::Vacant(e) => {
      e.insert(body_ids.clone());
    }
    btree_map::Entry::Occupied(mut e) => {
      e.get_mut().append(&mut body_ids.clone());
    }
  };

  g.production_rules.get(prod_id).unwrap().to_owned()
}

pub fn insert_production(g: &mut GrammarStore, mut prod: types::Production, bodies: Vec<Rule>) {
  let prod_id = prod.id;

  prod.number_of_rules = insert_rules(g, &prod_id, bodies).len() as u16;

  g.productions.insert(prod_id, prod);
}

/// Generate a unique scanner production name givin a uuid production
/// name

pub fn create_scanner_name(production_id: ProductionId, grammar_id: GrammarId) -> String {
  format!("scan_tok_{:X}_{:X}", production_id.0, grammar_id.0)
}

pub fn create_defined_scanner_name(uuid_production_name: &String) -> String {
  format!("scan_def_{}_", uuid_production_name)
}
