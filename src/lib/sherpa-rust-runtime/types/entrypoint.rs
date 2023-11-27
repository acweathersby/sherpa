#[derive(Clone, Copy, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]

pub struct EntryPoint {
  /// A pointer to the initial entry function for the given non-terminal.
  pub nonterm_id: u32,
}
