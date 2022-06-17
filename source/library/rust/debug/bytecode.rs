use std::collections::BTreeMap;

use crate::bytecode::constants::GOTO_INSTRUCTION_OFFSET_MASK;
use crate::bytecode::constants::GOTO_STATE_MASK;
use crate::bytecode::constants::INSTRUCTION;
use crate::bytecode::constants::INSTRUCTION_CONTENT_MASK;
use crate::bytecode::constants::INSTRUCTION_HEADER_MASK;
use crate::grammar;
use crate::primitives::Body;
use crate::primitives::GrammarStore;
use crate::primitives::Production;

pub fn disassembly_header(state_pointer: usize) -> String
{
    format!("{:0>6}: ", state_pointer)
}

struct Lookups<'a>
{
    production_offset_to_production: BTreeMap<u32, &'a Production>,
    bytecode_id_to_body: BTreeMap<u32, &'a Body>,
}



pub fn disassemble_state(
    states: &[u32],
    state_pointer: usize,
    depth: usize,
    grammar: Option<&GrammarStore>,
) -> (String, usize)
{
    use super::disassemble_state as ds;
    use super::disassembly_header as dh;

    let sp = state_pointer;
    let g = grammar;

    if state_pointer >= states.len() {
        ("".to_string(), sp)
    } else {
        let instruction = states[state_pointer];
        match instruction & INSTRUCTION_HEADER_MASK {
            INSTRUCTION::I00_PASS => (format!("\n{}PASS", dh(sp)), sp + 1),
            INSTRUCTION::I01_CONSUME => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (format!("\n{}SHFT", dh(sp)) + &string, offset + 1)
            }
            INSTRUCTION::I02_GOTO => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (
                    format!(
                        "\n{}GOTO [{}]",
                        dh(sp),
                        states[sp] & GOTO_INSTRUCTION_OFFSET_MASK
                    ) + &string,
                    offset,
                )
            }
            INSTRUCTION::I03_SET_PROD => {
                let production_id = instruction & INSTRUCTION_CONTENT_MASK;
                let (string, offset) = ds(states, sp + 1, depth, g);
                (
                    format!("\n{}PROD set to {}", production_id, dh(sp))
                        + &string,
                    offset,
                )
            }
            INSTRUCTION::I04_REDUCE => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                let symbol_count = instruction >> 16 & 0x0FFF;
                let body_id = instruction & 0xFFFF;

                if symbol_count == 0xFFF {
                    (
                        format!(
                            "\n{}REDU accumulated symbols to {}",
                            dh(sp),
                            body_id
                        ) + &string,
                        offset,
                    )
                } else {
                    let pluralized = if symbol_count == 1 {
                        "symbol"
                    } else {
                        "symbols"
                    };
                    (
                        format!(
                            "\n{}REDU {} {} to {}",
                            dh(sp),
                            symbol_count,
                            pluralized,
                            body_id
                        ) + &string,
                        offset,
                    )
                }
            }
            INSTRUCTION::I05_TOKEN => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (format!("\n{}TOKV", dh(sp)) + &string, offset)
            }
            INSTRUCTION::I06_FORK_TO => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (format!("\n{}FORK", dh(sp)) + &string, offset)
            }
            INSTRUCTION::I07_SCAN => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (format!("\n{}SCAN", dh(sp)) + &string, offset)
            }
            INSTRUCTION::I08_NOOP => (format!("\n{}NOOP", dh(sp)), sp + 1),
            INSTRUCTION::I09_JUMP_BRANCH => {
                let a = states[sp];
                let b = states[sp + 1];
                let c = states[sp + 2];
                let table_len = c >> 16 & 0xFFFF;
                let value_offset = c & 0xFFFF;
                let mut strings = vec![];
                let default_offset = states[sp + 3] as usize;

                for i in (4 + sp)..(sp + 4 + table_len as usize) {
                    let offset = states[i as usize] as usize;

                    if offset == 0xFFFF_FFFF {
                        strings.push(format!("\n SKIP"))
                    } else if offset == default_offset {
                        strings.push(format!("\nDEFAULT"))
                    } else {
                        let (string, offset) =
                            ds(states, sp + offset, depth, g);
                        strings.push(format!(
                            "\n-- [{}]:",
                            i as u32 + value_offset
                        ));
                        strings.push(string);
                    }
                }

                let (default_string, offset) =
                    ds(states, sp + default_offset, depth, g);

                let string = [&format!("\nJUMP OFFSET"), "JMP HASH"]
                    .join("\n")
                    .to_string()
                    + &strings.join("")
                    + "\n\n-- [DEFAULT]:"
                    + &default_string;

                (string, offset)
            }
            INSTRUCTION::I10_HASH_BRANCH => {
                let a = states[sp];
                let b = states[sp + 1];
                let c = states[sp + 2];
                let table_len = c >> 16 & 0xFFFF;
                let mod_base = c & 0xFFFF;
                let mut strings = vec![];
                let default_offset = states[sp + 3] as usize;

                for i in (4 + sp)..(sp + 4 + table_len as usize) {
                    let offset = (states[i as usize] >> 11 & 0x7FF) as usize;
                    if offset == 0x7FF {
                        strings.push(format!("\n SKIP"))
                    } else if offset == default_offset {
                        strings.push(format!("\n[DEFAULT]"))
                    } else {
                        let (string, offset) =
                            ds(states, sp + offset, depth, g);

                        strings.push(string);
                    }
                }

                let (default_string, offset) =
                    ds(states, sp + default_offset, depth, g);

                let string = [&format!("\nJUMP OFFSET"), "JMP HASH"]
                    .join("\n")
                    .to_string()
                    + &strings.join("")
                    + "\n--DEFAULT:"
                    + &default_string;
                (string, offset + 1)
            }
            INSTRUCTION::I11_SET_FAIL_STATE => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (format!("\n{}FSET", dh(sp)) + &string, offset)
            }
            INSTRUCTION::I12_REPEAT => {
                let (string, offset) = ds(states, sp + 1, depth, g);
                (format!("\n{}REPT", dh(sp)) + &string, offset)
            }
            INSTRUCTION::I13_NOOP => (format!("\n{}NOOP", dh(sp)), sp + 1),
            INSTRUCTION::I14_ASSERT_CONSUME => {
                (format!("\n{}ASTC", dh(sp)), sp + 1)
            }
            INSTRUCTION::I15_FAIL => (format!("\n{}FAIL", dh(sp)), sp + 1),
            _ => (format!("\n{}UNDF", dh(sp)), sp + 1),
        }
    }
}

mod bytecode_debugging_tests
{
    use std::collections::HashMap;

    use crate::bytecode::compiler::build_byte_code_buffer;
    use crate::bytecode::compiler::compile_ir_state_to_bytecode;
    use crate::bytecode::constants::default_get_branch_selector;
    use crate::bytecode::constants::BranchSelector;
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

        let states = result
            .into_iter()
            .map(|s| {
                let string = s.get_code();
                println!("{}", string);
                let result = compile_ir_ast(Vec::from(string.as_bytes()));
                assert!(result.is_ok());
                *result.unwrap()
            })
            .collect::<Vec<_>>();

        let state_refs = states.iter().collect::<Vec<_>>();

        let (bytecode, state_lookup) = build_byte_code_buffer(state_refs);

        println!("{:?}", state_lookup);

        let mut offset: usize = 0;
        while offset < bytecode.len() {
            print!("\n----------");
            let (string, next) =
                disassemble_state(&bytecode, offset, 0, Some(&grammar));
            offset = next;
            print!("{}", string);
        }

        println!("")
    }
}
