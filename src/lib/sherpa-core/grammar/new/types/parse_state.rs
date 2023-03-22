use super::*;
use crate::{
  grammar::new::parser::{self, State},
  writer::code_writer::CodeWriter,
  SherpaResult,
};
use std::fmt::Debug;

/// The IR of a sherpa
pub struct ParseState {
  pub name:    IString,
  pub comment: String,
  pub code:    String,
  pub ast:     SherpaResult<Box<State>>,
  pub tokens:  Option<OrderedSet<DBTokenData>>,
}

impl ParseState {
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
impl ParseState {
  pub fn debug_string(&self, db: &ParserDatabase) -> String {
    format!(
      "ParseState{{
  name: {}
  code: [\n    {}\n  ]
}}",
      self.name.to_string(db.string_store()),
      &self.code.split("\n").collect::<Vec<_>>().join("\n    ")
    )
  }
}
