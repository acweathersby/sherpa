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
    /// Returns the inverted version of `name_to_offset`
    pub fn get_offset_to_name(&self) -> BTreeMap<u32, String>
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

#[cfg(test)]
mod byte_code_creation_tests
{

    use std::collections::HashMap;

    use crate::bytecode::compile::compile_ir_state_to_bytecode;
    use crate::bytecode::constants::default_get_branch_selector;
    use crate::debug::compile_test_grammar;
    use crate::grammar::data::ast::ASTNode;
    use crate::grammar::get_production_id_by_name;
    use crate::grammar::parse::compile_ir_ast;
    use crate::intermediate::state::generate_production_states;

    #[test]
    pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production(
    )
    {
        let grammar = compile_test_grammar("<> A > \\h");

        let prod_id = get_production_id_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        println!("{:#?}", result);

        let state = result[0].get_code();

        let result = compile_ir_ast(Vec::from(state.as_bytes()));

        println!("{:#?}", result);

        assert!(result.is_ok());

        let result = compile_ir_state_to_bytecode(
            &result.unwrap(),
            default_get_branch_selector,
            &HashMap::new(),
        );

        println!("{:#?}", result);
    }
}
