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
/// parent dir path of the current grammar, whose path is provided by the
/// current grammar dir argument.
pub(crate) fn resolve_grammar_path(path: &PathBuf, current_grammar_dir: &PathBuf, extension: &[&str]) -> SherpaResult<PathBuf> {
  SherpaResult::Ok(
    match (
      path.is_file(),
      path.extension().is_some(),
      // Ensure path is is an absolute path
      match path.is_absolute() {
        true => (path.to_owned(), false),
        false => (current_grammar_dir.join(&path), current_grammar_dir.join(&path).is_file()),
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

#[derive(Default)]
pub struct SymbolData<'a> {
  pub annotation:       String,
  pub is_list:          bool,
  pub is_group:         bool,
  pub is_optional:      bool,
  pub is_shift_nothing: bool,
  pub is_eof:           bool,
  pub sym_precedence:   u16,
  pub tok_precedence:   u16,
  pub sym_atom:         Option<&'a ASTNode>,
}

/// Get a flattened view of a symbol's immediate AST
pub fn get_symbol_details<'a>(mut sym: &'a ASTNode) -> SymbolData<'a> {
  let mut data = SymbolData::default();

  loop {
    match sym {
      ASTNode::AnnotatedSymbol(annotated) => {
        data.is_optional |= annotated.is_optional;
        let (sym_prec, tok_prec) = annotated.precedence.as_ref().and_then(|p| Some((p.sym_prec, p.kot_prec)) ).unwrap_or_default();
        data.sym_precedence = sym_prec as u16;
        data.tok_precedence = tok_prec as u16;
        sym = &annotated.symbol;
      }
      ASTNode::GroupProduction(_) => {
        data.is_group = true;
        break;
      }
      ASTNode::List_Production( p) => {
        data.is_list = true;
        data.is_optional |= p.optional;
        break;
      }
      ASTNode::TerminalToken( t) => {
        data.tok_precedence = data.tok_precedence.max(t.is_exclusive as u16);
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
