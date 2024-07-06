use std::fmt::Debug;

use super::{grammar_object_ids::GrammarIdentities, symbol::SymbolId, DBNonTermKey, NonTermId, SymbolRef};
use crate::grammar_db_compiler::ASTToken;
use radlr_core_common::IString;
use radlr_rust_runtime::types::Token;

#[derive(Clone, Default)]
pub struct Rule {
  /// A list of [SymbolId]s and their position within the source grammar
  //pub nonterm:    NonTermId,
  pub nonterm_id: DBNonTermKey,
  pub symbols:    Vec<SymbolRef>,
  pub skipped:    usize,
  pub ast:        Option<ASTToken>,
  pub tok:        Token,
  pub g_id_index: usize,
}

impl Rule {
  pub fn new(tok: Token) -> Self {
    Self {
      nonterm_id: Default::default(),
      g_id_index: 0,
      symbols: Default::default(),
      skipped: Default::default(),
      ast: Default::default(),
      tok,
    }
  }
}

impl Debug for Rule {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.nonterm_id.fmt(f)?;
    f.write_str(" => ")?;

    for sym in &self.symbols {
      sym.fmt(f)?;
      f.write_str(" ")?;
    }

    Ok(())
  }
}
