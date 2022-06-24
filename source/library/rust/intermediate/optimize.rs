use std::collections::BTreeMap;

use crate::debug::grammar;
use crate::grammar::data::ast::IR_STATE;
use crate::primitives::GrammarStore;
use crate::primitives::IRState;

pub fn optimize_states<'a>(
    states: &'a mut BTreeMap<String, IRState>,
    grammar: &'a GrammarStore,
) -> Vec<&'a IR_STATE>
{
    states
        .iter()
        .map(|(_, s)| s.get_ast().unwrap())
        .collect::<Vec<_>>()
}
