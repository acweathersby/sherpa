use sherpa_core::{ParserClassification, ParserConfig, ParserMetrics};

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[allow(non_snake_case)]
#[derive(Clone, Copy)]
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
  /// Creates states that directly handle transitions on terminals, allowing the
  /// creation of parsers that can patch existing CST structures.
  pub AllOW_CST_MERGING: bool,
  /// Creates states that handle erroneous inputs, allowing a parser to recover
  /// from unexpected or missing tokens and continune parsing an input.
  pub AllOW_ERROR_RECOVERY: bool,
  /// Allow the parser to shift on CST non-term nodes.
  pub ALLOW_CST_NOTERM_SHIFT: bool,
  pub EXPORT_ALL_NONTERMS: bool,
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
      AllOW_CST_MERGING: false,
      AllOW_ERROR_RECOVERY: false,
      ALLOW_CST_NOTERM_SHIFT: false,
      EXPORT_ALL_NONTERMS: false,
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
      AllOW_CST_MERGING: self.AllOW_CST_MERGING,
      AllOW_ERROR_RECOVERY: self.AllOW_ERROR_RECOVERY,
      ALLOW_CST_NONTERM_SHIFT: self.ALLOW_CST_NOTERM_SHIFT,
      EXPORT_ALL_NONTERMS: self.EXPORT_ALL_NONTERMS,
    }
  }
}

impl Into<ParserConfig> for &JSParserConfig {
  fn into(self) -> ParserConfig {
    (*self).into()
  }
}

#[wasm_bindgen]
#[derive(Default, Clone, Copy)]
pub struct JSParserClassification {
  ///
  pub bottom_up:     bool,
  /// Maximum peek level used to disambiguate conflicting phrases. If this is
  /// equal to `u16::MAX`, then peeking failed or a fork was used in its place.
  pub max_k:         u16,
  /// If set to true then the parser has at least one state that transitions on
  /// non-terminals as well terminals.
  pub gotos_present: bool,
  /// If set to true, then the parser has at least one state that jumps to the
  /// head state of a specific non-terminal
  pub calls_present: bool,
  /// If set to true, the parser has at least one state that performs k>1
  /// lookaheads before selecting an appropriate alternative action.
  pub peeks_present: bool,
  /// If set to true, the parser has at least one state that forks the parse
  /// tree, and performs parsing on separate alternatives in parallel
  pub forks_present: bool,
}

#[wasm_bindgen]
impl JSParserClassification {
  pub fn get_type(&self) -> String {
    Into::<ParserClassification>::into(*self).get_type()
  }
}

impl From<ParserClassification> for JSParserClassification {
  fn from(value: ParserClassification) -> Self {
    Self {
      max_k:         value.max_k,
      bottom_up:     value.bottom_up,
      gotos_present: value.gotos_present,
      calls_present: value.calls_present,
      peeks_present: value.peeks_present,
      forks_present: value.forks_present,
    }
  }
}

impl Into<ParserClassification> for JSParserClassification {
  fn into(self) -> ParserClassification {
    ParserClassification {
      max_k:         self.max_k,
      bottom_up:     self.bottom_up,
      gotos_present: self.gotos_present,
      calls_present: self.calls_present,
      peeks_present: self.peeks_present,
      forks_present: self.forks_present,
    }
  }
}

#[wasm_bindgen]
#[derive(Default, Clone, Copy)]
pub struct JSParserMetrics {
  pub classification: JSParserClassification,
  pub num_of_states:  usize,
  pub optimized:      bool,
}

impl From<ParserMetrics> for JSParserMetrics {
  fn from(value: ParserMetrics) -> Self {
    Self {
      classification: value.classification.into(),
      num_of_states:  value.num_of_states,
      optimized:      value.optimized,
    }
  }
}
