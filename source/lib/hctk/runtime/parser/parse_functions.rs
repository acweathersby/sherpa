use std::hash;

use crate::bytecode::constants::DEFAULT_FAIL_INSTRUCTION_OFFSET;
use crate::bytecode::constants::INPUT_TYPE;
use crate::bytecode::constants::INSTRUCTION as I;
use crate::bytecode::constants::INSTRUCTION_CONTENT_MASK;
use crate::bytecode::constants::INSTRUCTION_HEADER_MASK;
use crate::bytecode::constants::LEXER_TYPE;
use crate::bytecode::constants::NORMAL_STATE_MASK;
use crate::bytecode::constants::STATE_INDEX_MASK;
use crate::debug::print_state;
use crate::runtime::recognizer::stack::KernelStack;
use crate::types::*;

use ParseAction::*;

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<T: SymbolReader>(
    reader: &mut T,
    state: &mut ParseState,
    bytecode: &[u32],
) -> ParseAction
{
    use ParseAction::*;

    let mut index = (state.get_active_state() & STATE_INDEX_MASK) as u32;

    loop {
        let instruction = unsafe { *bytecode.get_unchecked(index as usize) };

        state.set_active_state(index);

        index = match instruction & INSTRUCTION_HEADER_MASK {
            I::I01_CONSUME => {
                let (action, index) =
                    consume(index, instruction, state, reader);
                state.set_active_state(index);
                break action;
            }
            I::I02_GOTO => goto(index, instruction, state),
            I::I03_SET_PROD => set_production(index, instruction, state),
            I::I04_REDUCE => {
                let (action, index) = reduce(index, instruction, state);
                state.set_active_state(index);
                break action;
            }
            I::I05_TOKEN => set_token_state(index, instruction, state),
            I::I06_FORK_TO => {
                let (action, index) = fork(index, instruction);
                state.set_active_state(index);
                break action;
            }
            I::I07_SCAN => scan(),
            I::I08_NOOP => noop(index),
            I::I09_VECTOR_BRANCH => vector_jump(index, reader, state, bytecode),
            I::I10_HASH_BRANCH => hash_jump(index, reader, state, bytecode),
            I::I11_SET_FAIL_STATE => set_fail(),
            I::I12_REPEAT => repeat(),
            I::I13_NOOP => noop(index),
            I::I14_ASSERT_CONSUME => DEFAULT_FAIL_INSTRUCTION_OFFSET,
            I::I15_FAIL => break FailState,
            _ => break CompleteState,
        }
    }
}
/// Produces a parse action that
/// contains a token that is the
#[inline]
fn consume<T: SymbolReader>(
    index: u32,
    instruction: u32,
    state: &mut ParseState,
    reader: &mut T,
) -> (ParseAction, u32)
{
    if instruction & 0x1 == 1 {
        state.tokens[1].byte_length = 0;
        state.tokens[1].cp_length = 0;
    }

    let mut skip = state.get_anchor_token();
    let mut shift = state.get_assert_token();

    let next_token = shift.next();
    state.set_assert_token(next_token);

    if state.is_scanner() {
        reader.next(shift.byte_length)
    } else {
        state.set_anchor_token(next_token);
    }

    skip.cp_length = shift.cp_offset - skip.cp_offset;
    skip.byte_length = shift.byte_offset - skip.byte_offset;

    (
        ParseAction::Shift {
            skipped_characters: skip,
            token: shift,
        },
        index + 1,
    )
}

#[inline]
fn reduce(
    index: u32,
    instruction: u32,
    state: &mut ParseState,
) -> (ParseAction, u32)
{
    let symbol_count = instruction >> 16 & 0x0FFF;
    let body_id = instruction & 0xFFFF;
    let production_id = state.get_production();

    (
        if symbol_count == 0x0FFF {
            todo!("Acquire symbol count from symbol accumulator");
            ParseAction::Reduce {
                production_id,
                body_id,
                symbol_count: 0,
            }
        } else {
            ParseAction::Reduce {
                production_id,
                body_id,
                symbol_count,
            }
        },
        index + 1,
    )
}

#[inline]
fn goto(index: u32, instruction: u32, state: &mut ParseState) -> u32
{
    state.stack.push_state(instruction);
    index + 1
}

#[inline]
fn set_production(index: u32, instruction: u32, state: &mut ParseState) -> u32
{
    let production_id = instruction & INSTRUCTION_CONTENT_MASK;
    state.production_id = production_id;
    index + 1
}

#[inline]
fn set_token_state(index: u32, instruction: u32, state: &mut ParseState)
    -> u32
{
    let value = instruction & 0x00FF_FFFF;

    let mut anchor = state.get_anchor_token();

    let assert = state.get_assert_token();

    anchor.token_type = value;

    anchor.byte_length = assert.byte_offset - anchor.byte_offset;
    anchor.cp_length = assert.cp_offset - anchor.cp_offset;

    state.set_anchor_token(anchor);

    index + 1
}

#[inline]
fn fork(index: u32, instruction: u32) -> (ParseAction, u32)
{
    let instruction = instruction & INSTRUCTION_CONTENT_MASK;
    let target_production = instruction & 0xFFFF;
    let num_of_states = (instruction >> 16) & 0xFFFF;

    (
        ParseAction::Fork {
            num_of_states,
            states_start_offset: index + 1,
            target_production,
        },
        index + num_of_states + 1,
    )
}

fn scan() -> u32
{
    0
}

fn repeat() -> u32
{
    0
}

#[inline]
fn set_fail() -> u32
{
    0
}

#[inline]
fn noop(index: u32) -> u32
{
    index + 1
}

#[inline]
fn skip_token<T: SymbolReader>(state: &mut ParseState, reader: &mut T)
{
    let index = state.in_peek_mode as usize + 1;
    // let mut token = unsafe { state.tokens.get_unchecked_mut(index) };
    // if state.is_scanner {
    //    reader.next(token.byte_length)
    //}
    state.tokens[index] =
        unsafe { state.tokens.get_unchecked_mut(index).next() };
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<T: SymbolReader>(
    index: u32,
    reader: &mut T,
    state: &mut ParseState,
    bytecode: &[u32],
) -> u32
{
    let i = index as usize;
    // Decode data
    let (first, scan_index, third) = unsafe {
        let v = bytecode.get_unchecked(i..i + 3);
        (v[0], v[1], v[2])
    };
    let input_type = (first >> 22) & 0x7;
    let lexer_type = (first >> 26) & 0x3;
    let table_length = third >> 16 & 0xFFFF;
    let modulo_base = third & 0xFFFF;

    let hash_mask = 1 << (modulo_base - 1);
    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            INPUT_TYPE::T01_PRODUCTION => state.production_id,
            _ => get_token_value(
                lexer_type, input_type, reader, scan_index, state, bytecode,
            ) as u32,
        };
        let mut hash_index = (input_value & hash_mask) as usize;

        loop {
            let cell =
                unsafe { *bytecode.get_unchecked(table_start + hash_index) };
            let value = cell & 0x7FF;
            let offset = (cell >> 11) & 0x7FF;
            let next = ((cell >> 22) & 0x3FF) as i32 - 512;

            if value == input_value {
                if offset == 0x7FF {
                    skip_token(state, reader);
                    break;
                } else {
                    return index + offset;
                }
            } else if next != 0 {
                hash_index = ((hash_index as i32) + next) as usize;
            } else {
                return index + bytecode[table_start - 1];
            }
        }
    }
}
#[inline]
pub fn vector_jump<T: SymbolReader>(
    index: u32,
    reader: &mut T,
    state: &mut ParseState,
    bytecode: &[u32],
) -> u32
{
    let i = index as usize;
    // Decode data
    let (first, scan_index, third) = unsafe {
        let v = bytecode.get_unchecked(i..i + 3);
        (v[0], v[1], v[2])
    };
    let input_type = (first >> 22) & 0x7;
    let lexer_type = (first >> 26) & 0x3;
    let table_length = third >> 16 & 0xFFFF;
    let value_offset = third & 0xFFFF;

    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            INPUT_TYPE::T01_PRODUCTION => state.production_id,
            _ => get_token_value(
                lexer_type, input_type, reader, scan_index, state, bytecode,
            ) as u32,
        };

        let value_index = (input_value as i32 - value_offset as i32) as u32;

        if value_index < table_length {
            let offset = unsafe {
                *bytecode.get_unchecked(table_start + value_index as usize)
            };
            if offset == 0xFFFF_FFFF {
                skip_token(state, reader);
                continue;
            } else {
                return index + offset;
            }
        } else {
            return index + bytecode[table_start - 1];
        }
    }
}

#[inline]
fn get_token_value<T: SymbolReader>(
    lexer_type: u32,
    input_type: u32,
    reader: &mut T,
    scanner_start_offset: u32,
    state: &mut ParseState,
    bytecode: &[u32],
) -> i32
{
    let active_token = match lexer_type {
        LEXER_TYPE::PEEK => {
            let basis_token = unsafe {
                *state
                    .tokens
                    .get_unchecked((state.in_peek_mode as usize) + 1)
            };

            state.in_peek_mode = true;

            basis_token.next()
        }
        _ /*| LEXER_TYPE::ASSERT*/ => {
            if state.in_peek_mode {
                state.in_peek_mode = false;
                reader.set_cursor_to(unsafe { state.tokens.get_unchecked(1) });
            }
            state.tokens[1]
        }
    };

    let token_index = state.in_peek_mode as usize + 1;

    if state.is_scanner {
        state.tokens[token_index] = active_token;

        let active_token =
            unsafe { state.tokens.get_unchecked_mut(token_index) };

        let (line_number, line_count) = reader.get_line_data();

        match input_type {
            INPUT_TYPE::T03_CLASS => {
                active_token.byte_length = reader.codepoint_byte_length();
                active_token.cp_length = reader.codepoint_length();

                reader.class() as i32
            }
            INPUT_TYPE::T04_CODEPOINT => {
                active_token.byte_length = reader.codepoint_byte_length();
                active_token.cp_length = reader.codepoint_length();

                reader.codepoint() as i32
            }
            INPUT_TYPE::T05_BYTE => {
                active_token.byte_length = 1;
                active_token.cp_length = 1;

                reader.byte() as i32
            }
            _ => 0,
        }
    } else {
        let scanned_token = token_scan(
            active_token,
            scanner_start_offset,
            state,
            reader,
            bytecode,
        );

        state.tokens[token_index] = scanned_token;

        scanned_token.token_type as i32
    }
}

fn scan_for_improvised_token<T: SymbolReader>(
    scan_state: &mut ParseState,
    reader: &mut T,
)
{
    let mut assert = scan_state.get_assert_token();
    assert.byte_length = reader.codepoint_byte_length();
    assert.cp_length = 1;
    let mut byte = reader.byte();
    // Scan to next break point and produce an undefined
    // token. If we are already at a break point then just
    // return the single character token.
    if byte == b'\n' || byte == b'\t' || byte == b'\r' || byte == b' ' {
        assert = assert.next();
    } else {
        while byte != b'\n'
            && byte != b'\t'
            && byte != b'\r'
            && byte != b' '
            && !reader.at_end()
        {
            reader.next(assert.byte_length);
            byte = reader.byte();
            assert = assert.next();
            assert.byte_length = reader.codepoint_byte_length();
            assert.cp_length += 1;
        }
    }
    scan_state.set_assert_token(assert);
    set_token_state(0, 0, scan_state);
}

fn token_scan<T: SymbolReader>(
    token: ParseToken,
    scanner_start_offset: u32,
    state: &mut ParseState,
    reader: &mut T,
    bytecode: &[u32],
) -> ParseToken
{
    #[cfg(test)]
    {
        if scanner_start_offset == 0 {
            // Use a RNG to produce a token that is assigned the value
            // of state production id, decrementing production_id each time
            // we hit this function

            let id = state.production_id;

            if id > 0 {
                state.production_id -= 1;
            }

            return ParseToken {
                byte_length: 0,
                byte_offset: 0,
                cp_length:   0,
                cp_offset:   0,
                line_number: 0,
                line_offset: 0,
                token_type:  id,
                padding:     0,
            };
        }
    }
    // Initialize Scanner

    let mut scan_state = ParseState::new();
    scan_state.make_scanner();
    scan_state.init_normal_state(scanner_start_offset);
    scan_state.set_anchor_token(token);
    scan_state.set_assert_token(token);
    reader.set_cursor_to(&token);

    // We are done with the state reference, so we
    // invalidate variable by moving the reference to
    // an unused name to prevent confusion with the
    // `scan_state` variable.
    let _state = state;

    match {
        if scan_state.active_state == 0 {
            // Decode the next scanner_state.
            scan_state.active_state = scan_state.stack.pop_state();
        }

        let (line_count, line_offset) = reader.get_line_data();

        scan_state.tokens[0].line_number = line_count as u32;
        scan_state.tokens[0].line_offset = line_offset as u32;

        loop {
            if scan_state.active_state < 1 {
                if scan_state.in_fail_mode
                    || scan_state.get_anchor_token().token_type == 0
                {
                    scan_for_improvised_token(&mut scan_state, reader);
                }

                break ParseAction::ScannerToken(scan_state.get_anchor_token());
            } else {
                let mask_gate =
                    NORMAL_STATE_MASK << (scan_state.in_fail_mode as u32);

                if ((scan_state.active_state & mask_gate) != 0) {
                    scan_state.in_fail_mode = loop {
                        match dispatch(reader, &mut scan_state, bytecode) {
                            ParseAction::CompleteState => {
                                break false;
                            }

                            ParseAction::FailState => {
                                break true;
                            }
                            _ => {}
                        }
                    }
                }
                scan_state.active_state = scan_state.stack.pop_state();
            }
        }
    } {
        ParseAction::ScannerToken(token) => token,
        _ => panic!("Unusable State"),
    }
}

pub fn get_next_actionBB(
    reader: &mut UTF8StringReader,
    state: &mut ParseState,
    bytecode: &[u32],
) -> ParseAction
{
    get_next_action(reader, state, bytecode)
}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<T: SymbolReader>(
    reader: &mut T,
    state: &mut ParseState,
    bytecode: &[u32],
) -> ParseAction
{
    if state.active_state == 0 {
        state.active_state = state.stack.pop_state();
    }

    loop {
        if state.active_state < 1 {
            if reader.offset_at_end(state.tokens[1].byte_offset) {
                break ParseAction::Accept {
                    production_id: state.production_id,
                };
            } else {
                break ParseAction::Error {
                    message:    "Cannot parse this symbol",
                    last_input: state.get_assert_token(),
                };
            }
        } else {
            let mask_gate = NORMAL_STATE_MASK << (state.in_fail_mode as u32);

            if (state.interrupted || (state.active_state & mask_gate) != 0) {
                state.interrupted = true;

                match dispatch(reader, state, bytecode) {
                    ParseAction::CompleteState => {
                        state.interrupted = false;
                        state.in_fail_mode = false;
                        state.active_state = state.stack.pop_state();
                    }
                    ParseAction::FailState => {
                        state.interrupted = false;
                        state.in_fail_mode = true;
                        state.active_state = state.stack.pop_state();
                    }
                    action => break action,
                }
            } else {
                state.active_state = state.stack.pop_state();
            }
        }
    }
}
