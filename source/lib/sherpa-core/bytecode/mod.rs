use self::compile::build_byte_code_buffer;
use crate::{
  compile::get_branches,
  debug::{self},
  grammar::data::ast::{ASTNode, IR_STATE},
  journal::{report::ReportType, Journal},
  types::{IRState, IRStateType, Symbol},
};
use std::collections::BTreeMap;

pub mod compile;

// mod test;
#[derive(Debug)]
/// An opaque store of Sherpa parser bytecode and related metadata.
pub struct BytecodeOutput {
  /// The bytecode.
  pub bytecode: Vec<u32>,
  /// Maps plain state names to the offset within the bytecode
  /// vector.
  pub state_name_to_offset: BTreeMap<String, u32>,
  /// TODO: DOC
  pub offset_to_state_name: BTreeMap<u32, String>,
  /// TODO: DOC
  pub bytecode_id_to_symbol_lookup: BTreeMap<u32, Symbol>,
  /// TODO: DOC
  pub state_data: BTreeMap<String, StateData>,
  /// The intermediate representation states that the bytecode is based on.
  pub(crate) _states: Vec<IR_STATE>,
}

/// Store metadata for each state present in the bytecode.
#[derive(Debug)]
pub struct StateData {
  state_type:  IRStateType,
  goto_depth:  u32,
  _is_scanner: bool,
  name:        String,
}

impl StateData {
  pub(crate) fn from_ir_state(state: &IRState) -> Self {
    let mut max_gotos = 0;

    for (_, instr) in get_branches(state) {
      max_gotos =
        max_gotos.max(instr.iter().fold(0, |a, i| a + (matches!(i, ASTNode::Goto(..)) as u32)));
    }

    Self {
      state_type:  state.state_type,
      goto_depth:  max_gotos,
      _is_scanner: state.is_scanner(),
      name:        state.get_name(),
    }
  }

  pub fn get_type(&self) -> IRStateType {
    self.state_type
  }

  /// The maximum number of GOTO's this state will push
  pub fn get_goto_depth(&self) -> u32 {
    self.goto_depth
  }

  pub fn is_scanner(&self) -> bool {
    self._is_scanner
  }

  pub fn get_name(&self) -> String {
    self.name.clone()
  }
}
/// TODO: DOC
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
    _states: ir_ast_states,
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
      .into_iter()
      .map(|(s, state)| (s.clone(), StateData::from_ir_state(&state)))
      .collect(),
  }
}
