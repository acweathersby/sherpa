#[derive(Clone, Copy, Default, Debug)]

pub struct EntryPoint {
  /// A pointer to the initial entry function for the given non-terminal.
  pub nonterm_id: u32,
}
