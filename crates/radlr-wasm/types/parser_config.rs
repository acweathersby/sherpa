use radlr_core::{ParserClassification, ParserConfig, ParserMetrics};

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[allow(non_snake_case)]
#[derive(Clone, Copy)]
pub struct JSParserConfig {
  _config: ParserConfig,
}

#[wasm_bindgen]
impl JSParserConfig {
  #[wasm_bindgen(constructor)]
  pub fn new() -> Self {
    Self::default()
  }

  pub fn cst_editor() -> Self {
    Self { _config: ParserConfig::default().cst_editor() }
  }

  /// When enable, recursive descent style `Call` states will be generated
  #[wasm_bindgen(getter = ALLOW_CALLS)]
  pub fn ALLOW_CALLS(&self) -> bool {
    self._config.ALLOW_CALLS
  }

  #[wasm_bindgen(setter = ALLOW_CALLS)]
  pub fn set_ALLOW_CALLS(&mut self, val: bool) {
    self._config.ALLOW_CALLS = val;
  }

  /// When enable, LR style states can be produced, in general
  /// allowing more advanced grammar constructs to be parsed, such
  /// as left recursive rules.
  ///
  /// When disabled, grammars with rules that require LR style parse states
  /// will be rejected, and relevant errors will be reported.
  #[wasm_bindgen(getter = ALLOW_LR)]
  pub fn ALLOW_LR(&self) -> bool {
    self._config.ALLOW_LR
  }

  #[wasm_bindgen(setter = ALLOW_LR)]
  pub fn set_ALLOW_LR(&mut self, val: bool) {
    self._config.ALLOW_LR = val;
  }

  /// When enabled, unrestricted lookahead states states will be generated
  ///
  /// When disabled, grammars with rules that require a lookahead that is
  ///  `k>1` will be rejected, and relevant errors will be reported.
  #[wasm_bindgen(getter = ALLOW_PEEKING)]
  pub fn ALLOW_PEEKING(&self) -> bool {
    self._config.ALLOW_PEEKING
  }

  #[wasm_bindgen(setter = ALLOW_PEEKING)]
  pub fn set_ALLOW_PEEKING(&mut self, val: bool) {
    self._config.ALLOW_PEEKING = val;
  }

  /// The maximum number of lookead symbols allowed before parser construction
  /// is aborted or a different disambiguating strategy is employed.
  #[wasm_bindgen(getter = max_k)]
  pub fn max_k(&self) -> usize {
    self._config.max_k
  }

  #[wasm_bindgen(setter = max_k)]
  pub fn set_max_k(&mut self, val: usize) {
    self._config.max_k = val;
  }

  /// Allow the parser to split its context to handle ambiguous rules. This may
  /// lead to a CSF (Concrete Syntax Forest) or a CSDAG (Concrete Syntax DAG)
  /// being returned by the parser instead of a CST
  #[wasm_bindgen(getter = ALLOW_CONTEXT_SPLITTING)]
  pub fn ALLOW_CONTEXT_SPLITTING(&self) -> bool {
    self._config.ALLOW_CONTEXT_SPLITTING
  }

  #[wasm_bindgen(setter = ALLOW_CONTEXT_SPLITTING)]
  pub fn set_ALLOW_CONTEXT_SPLITTING(&mut self, val: bool) {
    self._config.ALLOW_CONTEXT_SPLITTING = val;
  }

  /// Creates a single scanner instead of multiple contextual scanners. More
  /// likely to report terminal conflicts.
  #[wasm_bindgen(getter = CONTEXT_FREE)]
  pub fn CONTEXT_FREE(&self) -> bool {
    self._config.CONTEXT_FREE
  }

  #[wasm_bindgen(setter = CONTEXT_FREE)]
  pub fn set_CONTEXT_FREE(&mut self, val: bool) {
    self._config.CONTEXT_FREE = val;
  }

  /// Creates states that directly handle transitions on terminals, allowing the
  /// creation of parsers that can patch existing CST structures.
  #[wasm_bindgen(getter = AllOW_CST_MERGING)]
  pub fn AllOW_CST_MERGING(&self) -> bool {
    self._config.AllOW_CST_MERGING
  }

  #[wasm_bindgen(setter = AllOW_CST_MERGING)]
  pub fn set_AllOW_CST_MERGING(&mut self, val: bool) {
    self._config.AllOW_CST_MERGING = val;
  }

  /// Allow the parser to shift on CST non-term nodes.
  #[wasm_bindgen(getter = ALLOW_CST_NONTERM_SHIFT)]
  pub fn ALLOW_CST_NONTERM_SHIFT(&self) -> bool {
    self._config.ALLOW_CST_NONTERM_SHIFT
  }

  #[wasm_bindgen(setter = ALLOW_CST_NONTERM_SHIFT)]
  pub fn set_ALLOW_CST_NONTERM_SHIFT(&mut self, val: bool) {
    self._config.ALLOW_CST_NONTERM_SHIFT = val;
  }

  /// Makes entry points for all non-terminals defined in the grammar.
  #[wasm_bindgen(getter = EXPORT_ALL_NONTERMS)]
  pub fn EXPORT_ALL_NONTERMS(&self) -> bool {
    self._config.EXPORT_ALL_NONTERMS
  }

  #[wasm_bindgen(setter = EXPORT_ALL_NONTERMS)]
  pub fn set_EXPORT_ALL_NONTERMS(&mut self, val: bool) {
    self._config.EXPORT_ALL_NONTERMS = val;
  }
}

impl Default for JSParserConfig {
  fn default() -> Self {
    Self { _config: Default::default() }
  }
}

impl Into<ParserConfig> for JSParserConfig {
  fn into(self) -> ParserConfig {
    self._config
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
    Into::<ParserClassification>::into(*self).to_string()
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
