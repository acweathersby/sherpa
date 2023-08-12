use sherpa_core::{
  compile_grammar_from_str,
  remove_grammar_mut,
  GrammarIdentity,
  GrammarSoup,
  Journal,
  SherpaResult,
};
use std::path::PathBuf;
use wasm_bindgen::prelude::*;

/// A Grammar Identity
#[wasm_bindgen]
pub struct JSGrammarIdentities(pub(crate) Box<GrammarIdentity>);

///
#[wasm_bindgen]
pub struct JSSoup(pub(crate) Box<GrammarSoup>);

#[wasm_bindgen]
impl JSSoup {
  /// Adds or replaces grammar to the soup, or throw's an error
  /// if the grammar is invalid. Returns the grammar
  /// name if successful.
  pub fn add_grammar(
    &mut self,
    grammar: String,
    path: String,
  ) -> Result<JSGrammarIdentities, JsError> {
    let mut j = Journal::new(Default::default());

    if remove_grammar_mut((&PathBuf::from(&path)).into(), &mut self.0).is_faulty() {
      return Err(JsError::new("Could not replace grammar"));
    }

    match compile_grammar_from_str(&mut j, grammar.as_str(), path.into(), &self.0) {
      SherpaResult::Ok(g_id) => Ok(JSGrammarIdentities(Box::new(g_id))),
      _ => Err(JsError::new("Failed to build grammar")),
    }
  }

  /// Adds a production targeting a specific grammar
  pub fn add_production(&mut self, grammar_name: String) -> Result<(), JsError> {
    Ok(())
  }
}

/// Creates an empty grammar soup object.
/// Use soup modifiers to add grammars and productions
///
/// Pass soup to parser compiler functions to create parsers, generate bytecode,
/// and construct ASCript AST and CST structures.
#[wasm_bindgen]
pub fn create_soup() -> Result<JSSoup, JsError> {
  Ok(JSSoup(Box::new(Default::default())))
}
