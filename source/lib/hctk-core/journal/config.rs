use bitmask_enum::bitmask;

#[bitmask]
pub enum ResolutionMode {
  /// Allow the use of LR states to resolve conflicts when building regular parsers
  LR_Resolution = 0b1,
  /// Allow the use of recursive ascent states to resolve left recursion
  RecursiveAscent = 0b10,
  /// Resort to creating Fork states when there are ambiguities.
  /// If this is not enabled, ambiguous grammar will fail to compile.
  Fork = 0b100,
  /// Allow the use of LR states to resolve
  /// conflicts when building token parsers
  Token_LR_Resolution = 0b1000,
  /// Use all methods available to resolve
  /// conflicts
  All  = 0b1111,
}

impl Default for ResolutionMode {
  fn default() -> Self {
    Self::All
  }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Config {
  pub resolution_mode:         ResolutionMode,
  /// If true, the parser will produce peek productions
  /// for symbols that occlude. An extra compile step must
  /// be taken to produce the symbol occlusion table.
  pub allow_occluding_symbols: bool,
}
