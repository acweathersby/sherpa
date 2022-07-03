use std::collections::BTreeMap;

use crate::grammar::data::ast::IR_STATE;
use crate::grammar::parse::compile_ir_ast;
use crate::intermediate::state::compile_states;
use crate::types::GrammarStore;

use self::compile::build_byte_code_buffer;

pub mod compile;
pub mod constants;
pub mod optimize;

pub struct BytecodeOutput
{
    /// The bytecode.
    pub bytecode:       Vec<u32>,
    /// The intermediate representation states that the bytecode
    /// is based on.
    pub states:         Vec<IR_STATE>,
    /// Maps plain state names to the offset within the bytecode
    /// vector.
    pub name_to_offset: BTreeMap<String, u32>,
}

impl BytecodeOutput
{
    pub fn get_inverted_state_lookup(&self) -> BTreeMap<u32, String>
    {
        self.name_to_offset
            .iter()
            .map(|(a, b)| (*b, a.clone()))
            .collect::<BTreeMap<_, _>>()
    }
}

pub fn compile_bytecode(
    grammar: &GrammarStore,
    threads: usize,
) -> BytecodeOutput
{
    let states = compile_states(grammar, threads)
        .values()
        .map(|s| {
            let string = s.get_code();

            match compile_ir_ast(Vec::from(string.as_bytes())) {
                Ok(ast) => *ast,
                Err(err) => {
                    panic!("\n{}", err);
                }
            }
        })
        .collect::<Vec<_>>();

    let state_refs = states.iter().collect::<Vec<_>>();

    let (bytecode, state_lookups) = build_byte_code_buffer(state_refs);

    BytecodeOutput {
        bytecode,
        states,
        name_to_offset: state_lookups,
    }
}
