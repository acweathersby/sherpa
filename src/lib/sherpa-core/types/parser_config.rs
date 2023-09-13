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

impl Default for ParserConfig {
  fn default() -> Self {
    Self {
      ALLOW_RECURSIVE_DESCENT: true,
      ALLOW_LR: true,
      ALLOW_LOOKAHEAD_MERGE: false,
      ALLOW_PEEKING: true,
      ALLOW_FORKING: false,
      CONTEXT_FREE: false,
      max_k: usize::MAX,
    }
  }
}

impl ParserConfig {
  pub fn new() -> Self {
    Self::default().set_k(8).enable_fork(false)
  }

  pub fn hybrid(self) -> Self {
    Self::new()
  }

  pub fn g_hybrid(self) -> Self {
    self.hybrid().set_k(8).enable_fork(true)
  }

  pub fn g_recursive_descent_k(self) -> Self {
    self.recursive_descent_k(8).enable_fork(true)
  }

  pub fn recursive_descent_k(mut self, k: usize) -> Self {
    self.ALLOW_RECURSIVE_DESCENT = true;
    self.ALLOW_LR = false;
    self.ALLOW_LOOKAHEAD_MERGE = false;
    self.set_k(k)
  }

  pub fn glr(mut self) -> Self {
    self = self.lrk(8);
    self.ALLOW_FORKING = true;
    self
  }

  pub fn gll(mut self) -> Self {
    self = self.llk(8);
    self.ALLOW_FORKING = true;
    self
  }

  pub fn lrk(mut self, k: usize) -> Self {
    self.ALLOW_LOOKAHEAD_MERGE = false;
    self.ALLOW_LR = true;
    self.ALLOW_RECURSIVE_DESCENT = false;
    self.ALLOW_FORKING = false;
    self.set_k(k)
  }

  pub fn llk(mut self, k: usize) -> Self {
    self.ALLOW_RECURSIVE_DESCENT = false;
    self.ALLOW_LR = false;
    self.ALLOW_FORKING = false;
    self.set_k(k)
  }

  pub fn ll1(mut self) -> Self {
    self.ALLOW_RECURSIVE_DESCENT = false;
    self.ALLOW_LR = false;
    self.ALLOW_PEEKING = false;
    self
  }

  pub fn lalr(self) -> Self {
    self.llk(1).enable_lookahead_merge(true)
  }

  pub fn set_k(mut self, k: usize) -> Self {
    self.ALLOW_PEEKING = k > 1;
    self.max_k = k;
    self
  }

  pub fn enable_lookahead_merge(mut self, enable: bool) -> Self {
    self.ALLOW_LOOKAHEAD_MERGE = enable;
    self
  }

  pub fn enable_fork(mut self, enable: bool) -> Self {
    self.ALLOW_FORKING = enable;
    self
  }

  pub fn enable_context_free(mut self, enable: bool) -> Self {
    self.CONTEXT_FREE = enable;
    self
  }
}
