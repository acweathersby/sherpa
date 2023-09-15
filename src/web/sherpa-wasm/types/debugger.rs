use sherpa_core::ParserConfig;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct JSParserConfig {
  /// When enable, recursive descent style `Call` states will be generated
  pub ALLOW_RECURSIVE_DESCENT: bool,
  /// When enable, LR style states can be produced, in general
  /// allowing more advanced grammar constructs to be parsed, such
  /// as left recursive rules.
  ///
  /// When disabled, grammars with rules that require LR style parse states
  /// will be rejected, and relevant errors will be reported.
  pub ALLOW_LR: bool,
  /// When enabled, unrestricted lookahead states states will be generated
  ///
  /// When disabled, grammars with rules that require a lookahead that is
  ///  `k>1` will be rejected, and relevant errors will be reported.
  pub ALLOW_PEEKING: bool,
  /// The maximum number of lookead symbols allowed before parser construction
  /// is aborted or a different disambiguating strategy is employed.
  pub max_k: usize,
  /// Allow merging of states that only differ in their look ahead sets. This
  /// will lead to a decrease in accuracy of error messages.
  pub ALLOW_LOOKAHEAD_MERGE: bool,
  /// Allow the parser to split its context to handle ambiguous rules. This may
  /// lead to a CSF (Concrete Syntax Forest) or a CSDAG (Concrete Syntax DAG)
  /// being returned by the parser instead of a CST
  pub ALLOW_FORKING: bool,
  /// Creates a single scanner instead of multiple contextual scanners. More
  /// likely to report terminal conflicts.
  pub CONTEXT_FREE: bool,
}

#[wasm_bindgen]
impl JSParserConfig {
  #[wasm_bindgen(constructor)]
  pub fn new() -> Self {
    Self::default()
  }
}

impl Default for JSParserConfig {
  fn default() -> Self {
    Self {
      ALLOW_RECURSIVE_DESCENT: true,
      ALLOW_LR: true,
      ALLOW_PEEKING: true,
      max_k: 8,
      ALLOW_LOOKAHEAD_MERGE: true,
      ALLOW_FORKING: false,
      CONTEXT_FREE: false,
    }
  }
}

impl Into<ParserConfig> for JSParserConfig {
  fn into(self) -> ParserConfig {
    ParserConfig {
      ALLOW_RECURSIVE_DESCENT: self.ALLOW_RECURSIVE_DESCENT,
      ALLOW_LR: self.ALLOW_LR,
      ALLOW_PEEKING: self.ALLOW_PEEKING,
      max_k: self.max_k,
      ALLOW_LOOKAHEAD_MERGE: self.ALLOW_LOOKAHEAD_MERGE,
      ALLOW_FORKING: self.ALLOW_FORKING,
      CONTEXT_FREE: self.CONTEXT_FREE,
    }
  }
}
