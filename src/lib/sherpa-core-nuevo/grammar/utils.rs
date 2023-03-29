use super::super::parser::ASTNode;
use crate::types::SherpaResult;
use std::path::PathBuf;

/// Resolves and verifies a grammar file path acquired from an `@IMPORT`
/// statement exists.
///
/// If the file path does not have an extension, attempts are made to assert
/// the existence of the file path when appended with one of the following
/// extension types appended to it: `.hc`, `.hcg` `.grammar`.
///
/// Additionally, if the given file path is relative, then it is appended to the
/// parent dir path of the current grammar, whose path is provided by the `cgd`,
/// current grammar dir, argument.
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
        .ok_or(format!(
          "Tried to load file with these extension {:?}",
          extension
        ))?,

      // Default
      _ => path.canonicalize()?,
    },
  )
}

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
      _ => unreachable!()
    }
  }

  data.sym_atom = Some(sym);

  data
}
