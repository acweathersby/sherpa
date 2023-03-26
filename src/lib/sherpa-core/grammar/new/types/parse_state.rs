use sherpa_runtime::types::bytecode::InputType;

use super::*;
use crate::{
  grammar::{
    hash_id_value_u64,
    new::parser::{self, ASTNode, Matches, State, Statement},
  },
  writer::code_writer::CodeWriter,
  SherpaResult,
};
use std::fmt::Debug;

#[derive(Default)]
/// The IR of a sherpa
pub struct ParseState<'db> {
  pub name:     IString,
  pub comment:  String,
  pub code:     String,
  pub ast:      SherpaResult<Box<State>>,
  /// Collections of scanner based on TOKEN match statements
  pub scanners: Option<Map<IString, OrderedSet<&'db DBTokenData>>>,
}

impl<'db> ParseState<'db> {
  pub fn get_scanners(
    &mut self,
  ) -> Option<&Map<IString, OrderedSet<&'db DBTokenData>>> {
    self.scanners.as_ref()
  }

  pub fn build_scanners(
    &mut self,
    db: &'db ParserDatabase,
  ) -> Option<&Map<IString, OrderedSet<&'db DBTokenData>>> {
    fn get_token_scanner_data<'db>(
      statement: &mut Statement,
      db: &'db ParserDatabase,
      scanners: &mut Map<IString, OrderedSet<&'db DBTokenData>>,
    ) {
      match &mut statement.branch {
        Some(ASTNode::Matches(box Matches { mode, matches, meta }))
          if mode == InputType::TOKEN_STR =>
        {
          let mut scanner_data = OrderedSet::new();
          for m in matches {
            match m {
              ASTNode::IntMatch(m) => {
                for id in &m.vals {
                  scanner_data.insert(db.tok_data((*id as u32).into()));
                }
                get_token_scanner_data(&mut m.statement, db, scanners);
              }
              _ => {}
            }
          }
          let name =
            ParseState::get_scanner_name(&scanner_data, db.string_store());

          (*meta) = name.as_u64();

          scanners.insert(name, scanner_data);
        }
        _ => {}
      }
    }

    if self.scanners.is_none() {
      let mut scanners = Map::new();
      //Ensure we have build the ast of the IR code.
      self.build_ast(db.string_store());
      if let SherpaResult::Ok(ast) = self.get_ast_mut() {
        get_token_scanner_data(&mut ast.statement, db, &mut scanners);
      }
      self.scanners = Some(scanners);
    }
    self.scanners.as_ref()
  }

  pub fn get_scanner_name(
    scanner_syms: &OrderedSet<&'db DBTokenData>,
    string_store: &IStringStore,
  ) -> IString {
    ("scan".to_string()
      + &hash_id_value_u64(
        scanner_syms.iter().map(|g| g.sym_id).collect::<OrderedSet<_>>(),
      )
      .to_string())
      .intern(string_store)
  }

  /// Returns a reference to the AST.
  ///
  /// May be `None` if the ast
  /// has not yet been built through `build_ast`.
  ///
  /// May also be an `Err`
  /// if there was a problem building the ast.
  pub fn get_ast(&self) -> SherpaResult<&Box<State>> {
    self.ast.as_ref()
  }

  pub fn get_ast_mut(&mut self) -> SherpaResult<&mut Box<State>> {
    self.ast.as_mut()
  }

  /// Builds and returns a reference to the AST.
  ///
  /// May be an `Err` if there was a problem building the ast.
  pub fn build_ast(&mut self, s: &IStringStore) -> SherpaResult<&Box<State>> {
    if self.ast.is_none() {
      let mut w = CodeWriter::new(vec![]);

      &mut w + self.name.to_str(s) + " =>\n" + self.code.as_str();

      let code = String::from_utf8(w.into_output())?;

      self.ast = SherpaResult::from(parser::ast::ir_from((&code).into()));
    }

    self.get_ast()
  }
}

#[cfg(debug_assertions)]
impl<'db> ParseState<'db> {
  pub fn debug_string(&self, db: &ParserDatabase) -> String {
    format!(
      "ParseState{{
  name: {}
  code: [\n    {}\n  ]
  ast: {:#?}
  scan_fn: {:?}
}}",
      self.name.to_string(db.string_store()),
      &self.code.split("\n").collect::<Vec<_>>().join("\n    "),
      &self.get_ast(),
      self.scanners.as_ref().map(|s| s
        .iter()
        .map(|(name, _)| { name.to_string(db.string_store()) })
        .collect::<Vec<_>>())
    )
  }
}
