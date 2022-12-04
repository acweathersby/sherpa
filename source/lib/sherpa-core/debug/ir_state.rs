use std::collections::BTreeMap;

use crate::types::IRState;

// Print an iterator of IRState references to stderr
pub(crate) fn print_ir_states(states: &BTreeMap<String, Box<IRState>>) {
  for state in states.values() {
    eprintln!("{}", state.to_string());
  }
}
