use crate::bytecode::compile_bytecode::build_byte_code_buffer;
use crate::debug::compile_test_grammar;
use crate::debug::print_states;
use crate::debug::BytecodeGrammarLookups;
use crate::get_num_of_available_threads;
use crate::grammar::get_production_by_name;
use crate::grammar::get_production_id_by_name;
use crate::intermediate::optimize::optimize_states;
use crate::intermediate::state_construction::compile_states;
use crate::runtime::parser::get_next_action;
use crate::types::*;
use std::sync::Arc;

pub fn collect_shifts_and_skips(
    input: &str,
    entry_point: u32,
    target_production_id: u32,
    bytecode: Vec<u32>,
) -> (UTF8StringReader, ParseState, Vec<String>, Vec<String>)
{
    let mut reader = UTF8StringReader::from_string(input);
    let source = reader.get_source();
    let mut state: ParseState = ParseState::new();
    state.init_normal_state(entry_point);
    let mut shifts = vec![];
    let mut skips = vec![];

    loop {
        match get_next_action(&mut reader, &mut state, &bytecode) {
            ParseAction::Accept { production_id } => {
                assert_eq!(production_id, target_production_id);
                break;
            }
            ParseAction::Error {
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
                    token,
                    token.blame(1, 1, "").unwrap()
                );
                break;
            }
            ParseAction::Fork {
                states_start_offset,
                num_of_states,
                target_production,
            } => panic!("No implementation of fork resolution is available"),
            ParseAction::Shift {
                skipped_characters,
                token,
            } => {
                if skipped_characters.byte_offset > 0 {
                    unsafe {
                        skips.push(
                            input
                                .to_string()
                                .get_unchecked(
                                    skipped_characters.byte_offset as usize
                                        ..(skipped_characters.byte_offset
                                            + skipped_characters.byte_length)
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
                                token.byte_offset as usize
                                    ..(token.byte_offset + token.byte_length)
                                        as usize,
                            )
                            .to_owned(),
                    );
                }
            }
            ParseAction::Reduce {
                production_id,
                body_id,
                symbol_count,
            } => {}
            _ => panic!("Unexpected Action!"),
        }
    }
    (reader, state, shifts, skips)
}
