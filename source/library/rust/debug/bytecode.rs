use crate::bytecode::constants::GOTO_STATE_MASK;
use crate::bytecode::constants::INSTRUCTION;
use crate::bytecode::constants::INSTRUCTION_POINTER_MASK;

pub fn disassembly_header(state_pointer: usize) -> String
{
    format!("{:0>6}: ", state_pointer)
}

pub fn disassemble_state(
    states: &[u32],
    state_pointer: usize,
    depth: usize,
) -> String
{
    use super::disassemble_state as ds;
    use super::disassembly_header as dh;

    let sp = state_pointer;

    if state_pointer >= states.len() {
        "".to_string()
    } else {
        match states[state_pointer] & 0xF000_0000 {
            INSTRUCTION::I00_PASS => format!("\n{}PASS", dh(sp)),
            INSTRUCTION::I01_CONSUME => {
                format!("\n{}EAT", dh(sp)) + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I02_GOTO => {
                format!(
                    "\n{}GOTO [{}]",
                    dh(sp),
                    states[sp] & INSTRUCTION_POINTER_MASK
                ) + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I03_SET_PROD => {
                "\n set prod".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I04_REDUCE => {
                "\n reduce".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I05_TOKEN => {
                "\n token".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I06_FORK_TO => {
                "\n fork to".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I07_SCAN => {
                "\n scan".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I08_NOOP => format!("\n{}NOOP", dh(sp)),
            INSTRUCTION::I09_JUMP_OFFSET_TABLE => {
                let a = states[sp];
                let b = states[sp + 1];
                let c = states[sp + 2];

                let table_len = c & 0xFFFF;
                let mod_base = c & 0xFFFF;

                [&format!("JMP OFFSET"), "JMP HASH"].join("\n").to_string()
                    + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I10_JUMP_HASH_TABLE => {
                let a = states[sp];
                let b = states[sp + 1];
                let c = states[sp + 2];
                let default_offset = states[sp + 3] as usize;

                let table_len = c & 0xFFFF;
                let mod_base = (c >> 16) & 0xFFFF;
                let input_type = (a >> 22) & 0xF;
                let lexer_type = (a >> 26) & 0x1;

                [
                    format!("\n------- BEGIN TABLE [{}]", sp),
                    format!(
                        "{}JMP HASH - mod {} | table length {}",
                        dh(sp),
                        mod_base,
                        table_len
                    ),
                    format!("{}    pointer {}", dh(sp + 1), c),
                    format!(
                        "{}    input_type {} | lexer_type {}",
                        dh(sp + 2),
                        input_type,
                        lexer_type
                    ),
                ]
                .into_iter()
                .chain(
                    states[(sp + 4)..(sp + 4 + table_len as usize)]
                        .into_iter()
                        .map(|v| {
                            let val = *v & 0x7FF;
                            let offset = *v >> 11 & 0x7FF;
                            (format!("\n Value: {}", val)
                                + &ds(states, sp + offset as usize, depth))
                                .to_owned()
                        }),
                )
                .collect::<Vec<_>>()
                .join("\n")
                .to_string()
                    + &format!("\n------- DEFAULT ACTIONS FOR TABLE [{}]", sp)
                    + &ds(states, sp + default_offset, depth)
                    + &format!("\n------- END TABLE [{}]", sp)
            }
            INSTRUCTION::I11_SET_FAIL_STATE => {
                "\nset fail state".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I12_REPEAT => {
                "\nrepeat".to_string() + &ds(states, sp + 1, depth)
            }
            INSTRUCTION::I13_NOOP => format!("\n{}NOOP", dh(sp)),
            INSTRUCTION::I15_FAIL => format!("\n{}FAIL", dh(sp)),
            _ => "".to_string(),
        }
    }
}

mod bytecode_debugging_tests
{
    use crate::bytecode::compiler::compile_ir_state_to_bytecode;
    use crate::bytecode::compiler::GenerateHashTable;
    use crate::debug::compile_test_grammar;
    use crate::debug::disassemble_state;
    use crate::grammar::get_production_by_name;
    use crate::grammar::parse::compile_ir_ast;
    use crate::intermediate::state_construct::generate_production_states;

    #[test]
    pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production(
    )
    {
        let grammar = compile_test_grammar("<> A > \\h");

        let prod_id = get_production_by_name("A", &grammar).unwrap();

        let result = generate_production_states(&prod_id, &grammar);

        let state = result[0].get_code();

        let result = compile_ir_ast(Vec::from(state.as_bytes()));

        assert!(result.is_ok());

        let bytecode = compile_ir_state_to_bytecode(&result.unwrap(), |_| {
            GenerateHashTable::Yes
        });

        println!("{}", disassemble_state(&bytecode, 0, 0));
    }
}
