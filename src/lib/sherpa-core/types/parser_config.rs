#[derive(Clone, Copy)]
/// Settings for configuring the type of parser Sherpa will generate.
pub struct ParserConfig {
  /// When enable, recursive descent style `Call` states will be generated
  pub ALLOW_RECURSIVE_DESCENT_CALLS: bool,
  /// When enable, LR style states can be produced, in general
  /// allowing more advanced grammar constructs to be parsed, such
  /// as left recursive rules.
  ///
  /// When disabled, grammars that rules that require LR style parse states
  /// will be rejected, and relevant errors will be reported.
  pub ALLOW_LR_RECURSIVE_ASCENT: bool,
  /// When enabled, advanced unrestricted peek lookahead states states will
  /// be generated
  ///
  /// When disabled, grammars that rules that require a lookahead that is
  ///  `k>1` will be rejected, and relevant errors will be reported.
  pub ALLOW_PEEKING: bool,
}

impl Default for ParserConfig {
  fn default() -> Self {
    Self {
      ALLOW_RECURSIVE_DESCENT_CALLS: true,
      ALLOW_LR_RECURSIVE_ASCENT: true,
      ALLOW_PEEKING: true,
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
    self.ALLOW_RECURSIVE_DESCENT_CALLS = false;
    self.ALLOW_LR_RECURSIVE_ASCENT = true;
    self
  }

  pub fn ll_only(mut self) -> Self {
    self.ALLOW_RECURSIVE_DESCENT_CALLS = true;
    self.ALLOW_LR_RECURSIVE_ASCENT = false;
    self
  }

  pub fn enable_peeking(mut self, enable: bool) -> Self {
    self.ALLOW_PEEKING = enable;
    self
  }
}
