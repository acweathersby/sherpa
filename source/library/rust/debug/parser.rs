use crate::bytecode::compiler::build_byte_code_buffer;
use crate::debug::compile_test_grammar;
use crate::debug::print_states;
use crate::debug::BytecodeGrammarLookups;
use crate::get_num_of_available_threads;
use crate::grammar::get_production_by_name;
use crate::grammar::get_production_id_by_name;
use crate::intermediate::optimize::optimize_states;
use crate::intermediate::state_construct::compile_states;
use crate::primitives::KernelState;
use crate::primitives::Token;
use crate::runtime::parser::get_next_action;
use crate::runtime::parser::Action;
use crate::runtime::parser::SymbolReader;
use crate::runtime::parser::UTF8StringReader;
use std::sync::Arc;

pub fn collect_shifts_and_skips(
    input: &str,
    entry_point: u32,
    target_production_id: u32,
    bytecode: Vec<u32>,
) -> (UTF8StringReader, KernelState, Vec<String>, Vec<String>)
{
    let mut reader = UTF8StringReader::from_str(input);
    let source = reader.get_source();
    let mut state: KernelState = KernelState::new();
    state.init_normal_state(entry_point);
    let mut shifts = vec![];
    let mut skips = vec![];

    loop {
        match get_next_action(&mut reader, &mut state, &bytecode) {
            Action::Accept { production_id } => {
                assert_eq!(production_id, target_production_id);
                break;
            }
            Action::Error {
                message,
                last_input,
            } => {
                let mut token = Token::from_kernel_token(&last_input);
                token.set_source(Arc::new(Vec::from(
                    input.to_string().as_bytes(),
                )));
                panic!(
                    "{} [{}]:\n{}",
                    message,
                    token.String(),
                    token.blame(1, 1, "").unwrap()
                );
                break;
            }
            Action::Fork {
                states_start_offset,
                num_of_states,
                target_production,
            } => panic!("No implementation of fork resolution is available"),
            Action::Shift((skip_token, shift_token)) => {
                if skip_token.byte_offset > 0 {
                    unsafe {
                        skips.push(
                            input
                                .to_string()
                                .get_unchecked(
                                    skip_token.byte_offset as usize
                                        ..(skip_token.byte_offset
                                            + skip_token.byte_length)
                                            as usize,
                                )
                                .to_owned(),
                        );
                    }
                }

                unsafe {
                    shifts.push(
                        input
                            .to_string()
                            .get_unchecked(
                                shift_token.byte_offset as usize
                                    ..(shift_token.byte_offset
                                        + shift_token.byte_length)
                                        as usize,
                            )
                            .to_owned(),
                    );
                }
            }
            Action::Reduce {
                production_id,
                body_id,
                symbol_count,
            } => {}
            _ => panic!("Unexpected Action!"),
        }
    }
    (reader, state, shifts, skips)
}
