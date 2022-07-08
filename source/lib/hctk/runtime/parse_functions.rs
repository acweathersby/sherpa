use std::hash;

use crate::bytecode::constants::DEFAULT_FAIL_INSTRUCTION_OFFSET;
use crate::bytecode::constants::INPUT_TYPE;
use crate::bytecode::constants::INSTRUCTION as I;
use crate::bytecode::constants::INSTRUCTION_CONTENT_MASK;
use crate::bytecode::constants::INSTRUCTION_HEADER_MASK;
use crate::bytecode::constants::LEXER_TYPE;
use crate::bytecode::constants::NORMAL_STATE_MASK;
use crate::bytecode::constants::STATE_INDEX_MASK;
use crate::types::*;

use ParseAction::*;

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
#[inline]
pub fn dispatch<T: CharacterReader>(
    reader: &mut T,
    ctx: &mut ParseContext<T>,
    bytecode: &[u32],
) -> ParseAction
{
    use ParseAction::*;

    let mut index = (ctx.get_active_state() & STATE_INDEX_MASK) as u32;

    loop {
        let instruction = unsafe { *bytecode.get_unchecked(index as usize) };

        ctx.set_active_state_to(index);

        index = match instruction & INSTRUCTION_HEADER_MASK {
            I::I01_CONSUME => {
                let (action, index) =
                    consume(index, instruction, ctx, reader);
                ctx.set_active_state_to(index);
                break action;
            }
            I::I02_GOTO => goto(index, instruction, ctx),
            I::I03_SET_PROD => set_production(index, instruction, ctx),
            I::I04_REDUCE => {
                let (action, index) = reduce(index, instruction, ctx);
                ctx.set_active_state_to(index);
                break action;
            }
            I::I05_TOKEN => set_token_state(index, instruction, ctx),
            I::I06_FORK_TO => {
                let (action, index) = fork(index, instruction);
                ctx.set_active_state_to(index);
                break action;
            }
            I::I07_SCAN => scan(),
            I::I08_NOOP => noop(index),
            I::I09_VECTOR_BRANCH => vector_jump(index, reader, ctx, bytecode),
            I::I10_HASH_BRANCH => hash_jump(index, reader, ctx, bytecode),
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
fn consume<T: CharacterReader>(
    index: u32,
    instruction: u32,
    ctx: &mut ParseContext<T>,
    reader: &mut T,
) -> (ParseAction, u32)
{
    if instruction & 0x1 == 1 {
        ctx.assert_token.byte_length = 0;
        ctx.assert_token.cp_length = 0;
    }

    let mut skip = ctx.anchor_token;
    let mut shift = ctx.assert_token;

    let next_token = shift.next();

    ctx.assert_token = next_token;

    if ctx.is_scanner() {
        reader.next(shift.byte_length as i32);
    } else {
        ctx.anchor_token = next_token;
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
fn reduce<T: CharacterReader>(
    index: u32,
    instruction: u32,
    ctx: &mut ParseContext<T>,
) -> (ParseAction, u32)
{
    let symbol_count = instruction >> 16 & 0x0FFF;
    let body_id = instruction & 0xFFFF;
    let production_id = ctx.get_production();

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
fn goto<T: CharacterReader>(
    index: u32,
    instruction: u32,
    ctx: &mut ParseContext<T>,
) -> u32
{
    ctx.push_state(instruction);
    index + 1
}

#[inline]
fn set_production<T: CharacterReader>(
    index: u32,
    instruction: u32,
    ctx: &mut ParseContext<T>,
) -> u32
{
    let production_id = instruction & INSTRUCTION_CONTENT_MASK;
    ctx.set_production_to(production_id);
    index + 1
}

#[inline]
fn set_token_state<T: CharacterReader>(
    index: u32,
    instruction: u32,
    ctx: &mut ParseContext<T>,
) -> u32
{
    let value = instruction & 0x00FF_FFFF;

    let mut anchor = ctx.anchor_token;

    let assert = ctx.assert_token;

    anchor.token_type = value;

    anchor.byte_length = assert.byte_offset - anchor.byte_offset;
    anchor.cp_length = assert.cp_offset - anchor.cp_offset;

    ctx.anchor_token = anchor;

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
fn skip_token<T: CharacterReader>(ctx: &mut ParseContext<T>, reader: &mut T)
{
    if ctx.in_peek_mode() {
        ctx.peek_token = ctx.peek_token.next();
    } else {
        ctx.assert_token = ctx.assert_token.next();
    }
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
pub fn hash_jump<T: CharacterReader>(
    index: u32,
    reader: &mut T,
    ctx: &mut ParseContext<T>,
    bytecode: &[u32],
) -> u32
{
    let i = index as usize;
    // Decode data

    let TableData {
        input_type,
        lexer_type,
        table_length,
        table_meta: modulo_base,
        scanner_offset:scan_index,
    } = TableData::from_bytecode(i, bytecode);

    let hash_mask = 1 << (modulo_base - 1);
    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            INPUT_TYPE::T01_PRODUCTION => ctx.get_production(),
            _ => get_token_value(
                lexer_type, input_type, reader, scan_index, ctx, bytecode,
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
                    skip_token(ctx, reader);
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
pub fn vector_jump<T: CharacterReader>(
    index: u32,
    reader: &mut T,
    ctx: &mut ParseContext<T>,
    bytecode: &[u32],
) -> u32
{
    let i = index as usize;
    // Decode data

    let TableData {
        input_type,
        lexer_type,
        table_length,
        table_meta: value_offset,
        scanner_offset:scan_index,
    } = TableData::from_bytecode(i, bytecode);

    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            INPUT_TYPE::T01_PRODUCTION => ctx.get_production(),
            _ => get_token_value(
                lexer_type, input_type, reader, scan_index, ctx, bytecode,
            ) as u32,
        };

        let value_index = (input_value as i32 - value_offset as i32) as u32;

        if value_index < table_length {
            let offset = unsafe {
                *bytecode.get_unchecked(table_start + value_index as usize)
            };
            if offset == 0xFFFF_FFFF {
                skip_token(ctx, reader);
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
fn get_token_value<T: CharacterReader>(
    lexer_type: u32,
    input_type: u32,
    reader: &mut T,
    scanner_start_offset: u32,
    ctx: &mut ParseContext<T>,
    bytecode: &[u32],
) -> i32
{
    let mut active_token = match lexer_type {
        LEXER_TYPE::PEEK => {
            let basis_token = if ctx.in_peek_mode() {
                ctx.peek_token
            } else {
                ctx.anchor_token
            };

            ctx.set_peek_mode_to(true);

            basis_token.next()
        }
        _ /*| LEXER_TYPE::ASSERT*/ => {
            if ctx.in_peek_mode() {
                
                ctx.set_peek_mode_to(false);

                reader.set_cursor_to(&ctx.assert_token );
            }

            ctx.assert_token
        }
    };

    if ctx.is_scanner() {
        match input_type {
            INPUT_TYPE::T03_CLASS => {
                active_token.byte_length = reader.codepoint_byte_length();
                active_token.cp_length = reader.codepoint_length();
        
                if ctx.in_peek_mode() {
                    ctx.peek_token = active_token;
                } else {
                    ctx.assert_token = active_token;
                }

                reader.class() as i32
            }
            INPUT_TYPE::T04_CODEPOINT => {
                active_token.byte_length = reader.codepoint_byte_length();
                active_token.cp_length = reader.codepoint_length();
        
                if ctx.in_peek_mode() {
                    ctx.peek_token = active_token;
                } else {
                    ctx.assert_token = active_token;
                }

                reader.codepoint() as i32
            }
            INPUT_TYPE::T05_BYTE => {
                active_token.byte_length = 1;
                active_token.cp_length = 1;
        
                if ctx.in_peek_mode() {
                    ctx.peek_token = active_token;
                } else {
                    ctx.assert_token = active_token;
                }
                reader.byte() as i32
            }
            _ => 0,
        }
    } else {
        let scanned_token = token_scan(
            active_token,
            scanner_start_offset,
            ctx,
            reader,
            bytecode,
        );

        if ctx.in_peek_mode() {
            ctx.peek_token = scanned_token;
        } else {
            ctx.assert_token = scanned_token;
        }

        scanned_token.token_type as i32
    }
}

fn scan_for_improvised_token<T: CharacterReader>(
    scan_ctx: &mut ParseContext<T>,
    reader: &mut T,
)
{
    let mut assert = scan_ctx.assert_token;
    assert.byte_length = reader.codepoint_byte_length();
    assert.cp_length = 1;
    let mut byte = reader.byte();
    // Scan to next break point and produce an undefined
    // token. If we are already at a break point then just
    // return the single character token.
    if byte == (b'\n' as u32)
        || byte == (b'\t' as u32)
        || byte == (b'\r' as u32)
        || byte == (b' ' as u32)
    {
        assert = assert.next();
    } else {
        while byte != b'\n' as u32
            && byte != b'\t' as u32
            && byte != b'\r' as u32
            && byte != b' ' as u32
            && !reader.at_end()
        {
            reader.next(assert.byte_length as i32);
            byte = reader.byte();
            assert = assert.next();
            assert.byte_length = reader.codepoint_byte_length();
            assert.cp_length += 1;
        }
    }
    scan_ctx.assert_token = assert;
    set_token_state(0, 0, scan_ctx);
}

fn token_scan<T: CharacterReader>(
    token: ParseToken,
    scanner_start_offset: u32,
    ctx: &mut ParseContext<T>,
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

            let id = ctx.get_production();

            if id > 0 {
                ctx.set_production_to(id - 1);
            }

            return ParseToken {
                token_type: id,
                ..Default::default()
            };
        }
    }
    // Initialize Scanner

    let mut scan_ctx = ParseContext::bytecode_context();
    scan_ctx.make_scanner();
    scan_ctx.init_normal_state(scanner_start_offset);
    scan_ctx.anchor_token = token;
    scan_ctx.assert_token = token;
    reader.set_cursor_to(&token);

    // We are done with the state reference, so we
    // invalidate variable by moving the reference to
    // an unused name to prevent confusion with the
    // `scan_state` variable.
    let _state = ctx;

    match {
        if scan_ctx.get_active_state() == 0 {
            // Decode the next scanner_state.
            let state = scan_ctx.pop_state();
            scan_ctx.set_active_state_to(state);
        }

        let line_data = reader.get_line_data();
        let (line_num, line_count) =
            ((line_data >> 32) as u32, (line_data & 0xFFFF_FFFF) as u32);

        scan_ctx.anchor_token.line_number = line_num;
        scan_ctx.anchor_token.line_offset = line_count;

        loop {
            if scan_ctx.get_active_state() < 1 {
                if scan_ctx.in_fail_mode()
                    || scan_ctx.anchor_token.token_type == 0
                {
                    scan_for_improvised_token(&mut scan_ctx, reader);
                }

                break ParseAction::ScannerToken(scan_ctx.anchor_token);
            } else {
                let mask_gate =
                    NORMAL_STATE_MASK << (scan_ctx.in_fail_mode() as u32);

                if ((scan_ctx.get_active_state() & mask_gate) != 0) {
                    let fail_mode =loop {
                        match dispatch(reader, &mut scan_ctx, bytecode) {
                            ParseAction::CompleteState => {
                                break false;
                            }

                            ParseAction::FailState => {
                                break true;
                            }
                            _ => {}
                        }
                    };
                    scan_ctx.set_fail_mode_to(  fail_mode);
                }
                let state = scan_ctx.pop_state();
                scan_ctx.set_active_state_to(state);
            }
        }
    } {
        ParseAction::ScannerToken(token) => token,
        _ => panic!("Unusable State"),
    }
}

/// Start or continue a parse on an input
#[inline]
pub fn get_next_action<T: CharacterReader>(
    reader: &mut T,
    ctx: &mut ParseContext<T>,
    bytecode: &[u32],
) -> ParseAction
{
    if ctx.get_active_state() == 0 {
        let state = ctx.pop_state();
        ctx.set_active_state_to(state);
    }

    loop {
        if ctx.get_active_state() < 1 {
            if reader.offset_at_end(ctx.assert_token.byte_offset) {
                break ParseAction::Accept {
                    production_id: ctx.get_production(),
                };
            } else {
                break ParseAction::Error {
                    message:    "Cannot parse this symbol",
                    last_input: ctx.assert_token,
                };
            }
        } else {
            let mask_gate = NORMAL_STATE_MASK << (ctx.in_fail_mode() as u32);

            if (ctx.is_interrupted() || (ctx.get_active_state() & mask_gate) != 0) {
                ctx.set_interrupted_to(true);

                match dispatch(reader, ctx, bytecode) {
                    ParseAction::CompleteState => {
                        ctx.set_interrupted_to(false);
                        ctx.set_fail_mode_to(false);
                        let state = ctx.pop_state();
                        ctx.set_active_state_to(state);
                    }
                    ParseAction::FailState => {
                        ctx.set_interrupted_to(false);
                        ctx.set_fail_mode_to(true);
                        let state = ctx.pop_state();
                        ctx.set_active_state_to(state);
                    }
                    action => break action,
                }
            } else {
                let state = ctx.pop_state();
                ctx.set_active_state_to(state);
            }
        }
    }
}
