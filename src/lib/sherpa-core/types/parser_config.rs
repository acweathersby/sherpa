#[derive(Clone, Copy)]
/// Settings for configuring the type of parser Sherpa will generate.
pub struct ParserConfig {
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
  /// Allow merging of states that only differ in their look ahead sets. This
  /// will lead to a decrease in accuracy of error messages.
  pub ALLOW_LOOKAHEAD_MERGE: bool,
  /// Allow the parser to split its context to handle ambiguous rules. This may
  /// lead to a CSF (Concrete Syntax Forest) or a CSDAG (Concrete Syntax DAG)
  /// being returned by the parser instead of a CST
  pub ALLOW_FORKING: bool,

  pub max_k: usize,
}

impl Default for ParserConfig {
  fn default() -> Self {
    Self {
      ALLOW_RECURSIVE_DESCENT: true,
      ALLOW_LR: true,
      ALLOW_LOOKAHEAD_MERGE: true,
      ALLOW_PEEKING: true,
      ALLOW_FORKING: false,
      max_k: usize::MAX,
    }
  }
}

impl ParserConfig {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn hybrid(self) -> Self {
    Self::new()
  }

  pub fn lr_only(mut self) -> Self {
    self.ALLOW_RECURSIVE_DESCENT = false;
    self.ALLOW_LR = true;
    self
  }

  pub fn recursive_descent(mut self) -> Self {
    self.ALLOW_RECURSIVE_DESCENT = true;
    self.ALLOW_LR = false;
    self.ALLOW_LOOKAHEAD_MERGE = false;
    self.ALLOW_PEEKING = false;
    self
  }

  pub fn ll_only(mut self) -> Self {
    self.ALLOW_RECURSIVE_DESCENT = false;
    self.ALLOW_LR = false;
    self
  }

  pub fn enable_peeking(mut self, enable: bool) -> Self {
    self.ALLOW_PEEKING = enable;
    self
  }

  pub fn enable_la(mut self, enable: bool) -> Self {
    self.ALLOW_LOOKAHEAD_MERGE = enable;
    self
  }
}
