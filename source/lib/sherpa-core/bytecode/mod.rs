use std::collections::BTreeMap;

use crate::{
  debug::{self, BytecodeGrammarLookups},
  grammar::data::ast::IR_STATE,
};

use crate::{
  journal::{report::ReportType, Journal},
  types::{GrammarStore, IRState, IRStateType, Symbol},
};

use self::compile::build_byte_code_buffer;
pub mod compile;

#[cfg(test)]
mod test;

/// Store metadata for each state present in the bytecode.
#[derive(Debug)]
pub struct StateData {
  state_type:  IRStateType,
  stack_depth: u32,
  _is_scanner: bool,
  name:        String,
}

impl StateData {
  pub fn from_ir_state(state: &IRState) -> Self {
    Self {
      state_type:  state.state_type,
      stack_depth: state.stack_depth,
      _is_scanner: state.is_scanner(),
      name:        state.get_name(),
    }
  }

  pub fn get_type(&self) -> IRStateType {
    self.state_type
  }

  pub fn get_stack_depth(&self) -> u32 {
    self.stack_depth
  }

  pub fn is_scanner(&self) -> bool {
    self._is_scanner
  }

  pub fn get_name(&self) -> String {
    self.name.clone()
  }
}

#[derive(Debug)]
pub struct BytecodeOutput {
  /// The bytecode.
  pub bytecode: Vec<u32>,
  /// The intermediate representation states that the bytecode
  /// is based on.
  pub states: Vec<IR_STATE>,
  /// Maps plain state names to the offset within the bytecode
  /// vector.
  pub state_name_to_offset: BTreeMap<String, u32>,
  pub offset_to_state_name: BTreeMap<u32, String>,
  pub bytecode_id_to_symbol_lookup: BTreeMap<u32, Symbol>,
  pub state_data: BTreeMap<String, StateData>,
}

pub fn compile_bytecode<'a>(
  j: &mut Journal,
  mut ir_states: Vec<(String, Box<IRState>)>,
) -> BytecodeOutput {
  let ir_ast_states = ir_states
    .iter_mut()
    .map(|(_, s)| match s.compile_ast() {
      Ok(ast) => (*ast).clone(),
      Err(err) => {
        panic!("\n{}", err);
      }
    })
    .collect::<Vec<_>>();

  let bytecode = compile_ir_states_into_bytecode(j, ir_states, ir_ast_states);

  if j.config().build_disassembly {
    let grammar = j.grammar().unwrap();
    j.set_active_report(&format!("[{}] Disassembly", &grammar.id.name), ReportType::Disassembly);

    j.report_mut().start_timer("Build Time");

    let disassembly = debug::generate_disassembly(&bytecode, Some(j));

    j.report_mut().stop_timer("Build Time");

    j.report_mut().add_note("Output", disassembly);
  }

  bytecode
}

pub(crate) fn compile_ir_states_into_bytecode<'a>(
  j: &mut Journal,
  ir_states: Vec<(String, Box<IRState>)>,
  ir_ast_states: Vec<IR_STATE>,
) -> BytecodeOutput {
  let state_refs = ir_ast_states.iter().collect::<Vec<_>>();

  let (bytecode, state_lookups) = build_byte_code_buffer(state_refs);

  BytecodeOutput {
    bytecode,
    states: ir_ast_states,
    offset_to_state_name: state_lookups
      .iter()
      .map(|(a, b)| (*b, a.clone()))
      .collect::<BTreeMap<_, _>>(),
    state_name_to_offset: state_lookups,
    bytecode_id_to_symbol_lookup: j
      .grammar()
      .unwrap()
      .symbols
      .values()
      .chain(Symbol::Generics)
      .map(|s| (s.bytecode_id, s.clone()))
      .collect::<BTreeMap<_, _>>(),
    state_data: ir_states
      .iter()
      .map(|(s, state)| (s.clone(), StateData::from_ir_state(state)))
      .collect(),
  }
}
