use std::hash;
mod reader;
mod utf8_string_reader;

use crate::bytecode::constants::DEFAULT_FAIL_INSTRUCTION_OFFSET;
use crate::bytecode::constants::INPUT_TYPE;
use crate::bytecode::constants::INSTRUCTION as I;
use crate::bytecode::constants::INSTRUCTION_CONTENT_MASK;
use crate::bytecode::constants::INSTRUCTION_HEADER_MASK;
use crate::bytecode::constants::LEXER_TYPE;
use crate::bytecode::constants::STATE_INDEX_MASK;
use crate::primitives::KernelToken;
use crate::primitives::Token;
use crate::runtime::parser::reader::SymbolReader;
use crate::runtime::parser::reader::UTF8FileReader;

use super::recognizer::stack::KernelStack;

struct KernelState
{
    stack:           KernelStack,
    tokens:          [KernelToken; 3],
    sym_accumulator: u32,
    production_id:   u32,
    pointer:         u32,
    in_peek_mode:    bool,
    is_scanner:      bool,
}

impl KernelState
{
    pub fn new() -> Self
    {
        Self {
            stack:           KernelStack::new(),
            tokens:          [
                KernelToken::new(),
                KernelToken::new(),
                KernelToken::new(),
            ],
            sym_accumulator: 0,
            production_id:   0,
            pointer:         0,
            in_peek_mode:    false,
            is_scanner:      false,
        }
    }
}

enum Action
{
    Undefined,
    CompleteState,
    FailState,
    Fork,
    Shift((KernelToken, KernelToken)),
    Inter_Reduce
    {
        production_id: u32,
        body_id:       u32,
        symbol_count:  u32,
    },
    Skip(KernelToken),
    Accept,
}

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
fn dispatch<T: SymbolReader>(
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> Action
{
    use Action::*;

    let mut index = (state.pointer & STATE_INDEX_MASK) as u32;

    loop {
        let instruction = unsafe { *bytecode.get_unchecked(index as usize) };

        index = match instruction & INSTRUCTION_HEADER_MASK {
            I::I01_CONSUME => break consume(instruction, state),
            I::I02_GOTO => goto(),
            I::I03_SET_PROD => set_production(index, instruction, state),
            I::I04_REDUCE => break reduce(instruction, state),
            I::I05_TOKEN => set_token_state(),
            I::I06_FORK_TO => break fork(),
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
fn consume(instruction: u32, state: &mut KernelState) -> Action
{
    if instruction & 0x1 == 1 {
        state.tokens[1].byte_length = 0;
        state.tokens[1].cp_length = 0;
    }

    let mut skip_token = state.tokens[0];
    let mut shift_token = state.tokens[1];

    skip_token.cp_length = shift_token.cp_offset - skip_token.cp_offset;
    skip_token.byte_length = shift_token.byte_offset - skip_token.byte_offset;

    state.tokens[1].next();
    state.tokens[0] = state.tokens[1];

    Action::Shift((skip_token, shift_token))
}

#[inline]
fn reduce(instruction: u32, state: &mut KernelState) -> Action
{
    let symbol_count = instruction >> 16 & 0x0FFF;
    let body_id = instruction & 0xFFFF;
    let production_id = state.production_id;

    if symbol_count == 0x0FFF {
        todo!("Acquire symbol count from symbol accumulator");
        Action::Inter_Reduce {
            production_id,
            body_id,
            symbol_count: 0,
        }
    } else {
        Action::Inter_Reduce {
            production_id,
            body_id,
            symbol_count,
        }
    }
}

fn goto() -> u32
{
    0
}

#[inline]
fn set_production(index: u32, instruction: u32, state: &mut KernelState)
    -> u32
{
    let production_id = instruction & INSTRUCTION_CONTENT_MASK;
    state.production_id = production_id;
    index + 1
}

fn set_token_state() -> u32
{
    0
}

fn fork() -> Action
{
    Action::Fork
}

fn scan() -> u32
{
    0
}

fn repeat() -> u32
{
    0
}

/// Performs an instruction branch selection based on an embedded,
/// linear-probing hash table.
#[inline]
fn hash_jump<T: SymbolReader>(
    index: u32,
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> u32
{
    let i = index as usize;
    // Decode data
    let (first, second, third) = unsafe {
        let v = bytecode.get_unchecked(i..i + 3);
        (v[0], v[1], v[2])
    };
    let input_type = (first >> 22) & 0x7;
    let lexer_type = (first >> 26) & 0x3;
    let scan_index = second;
    let table_size = third >> 16 & 0xFFFF;
    let modulo_base = third & 0xFFFF;
    let hash_mask = 1 << (modulo_base - 1);
    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            INPUT_TYPE::T01_PRODUCTION => state.production_id,
            _ => {
                get_token_value(lexer_type, input_type, reader, state, bytecode)
                    as u32
            }
        };
        let mut hash_index = (input_value & hash_mask) as usize;

        loop {
            let cell =
                unsafe { *bytecode.get_unchecked(table_start + hash_index) };
            let value = cell & 0x7FF;
            let offset = (cell >> 11) & 0x7FF;
            let next = ((cell >> 22) & 0x3FF) as i32 - 512;
            if offset == 0x7FF {
                skip_token(state, reader);
                break;
            } else if value == input_value {
                return index + offset;
            } else if next == 0 {
                return index + bytecode[table_start - 1];
            } else {
                hash_index = ((hash_index as i32) + next) as usize;
            }
        }
    }
}
#[inline]
fn vector_jump<T: SymbolReader>(
    index: u32,
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> u32
{
    let i = index as usize;
    // Decode data
    let (first, second, third) = unsafe {
        let v = bytecode.get_unchecked(i..i + 3);
        (v[0], v[1], v[2])
    };
    let input_type = (first >> 22) & 0x7;
    let lexer_type = (first >> 26) & 0x3;
    let scan_index = second;
    let table_length = third >> 16;
    let value_offset = third & 0xFFFF;
    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            INPUT_TYPE::T01_PRODUCTION => state.production_id,
            _ => {
                get_token_value(lexer_type, input_type, reader, state, bytecode)
                    as u32
            }
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
fn skip_token<T: SymbolReader>(state: &mut KernelState, reader: &mut T)
{
    let index = state.in_peek_mode as usize + 1;
    let mut token = unsafe { state.tokens.get_unchecked_mut(index) };
    if state.is_scanner {
        reader.next(token.byte_length)
    }
    token.next();
}

fn get_token_value<T: SymbolReader>(
    lexer_type: u32,
    input_type: u32,
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> i32
{
    let active_token = match lexer_type {
        // Peek mode
        LEXER_TYPE::PEEK => {
            let basis_token = unsafe {
                *state
                    .tokens
                    .get_unchecked((state.in_peek_mode as usize) + 1)
            };

            state.in_peek_mode = true;

            basis_token.next()
        }
        // Assert mode
        _ | LEXER_TYPE::ASSERT => {
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

        active_token.line_number = reader.line_count();
        active_token.line_offset = reader.line_offset();

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
        let scanned_token = scanner(active_token);

        state.tokens[token_index] = scanned_token;

        scanned_token.typ as i32
    }
}

fn scanner(token_start: KernelToken) -> KernelToken
{
    KernelToken::new()
}

fn set_fail() -> u32
{
    0
}

#[inline]
fn noop(index: u32) -> u32
{
    index + 1
}

#[cfg(test)]
mod test
{
    use std::collections::HashMap;

    use crate::bytecode::compiler::compile_ir_state_to_bytecode;
    use crate::bytecode::constants::BranchSelector;
    use crate::debug::compile_test_grammar;
    use crate::debug::disassemble_state;
    use crate::grammar::data::ast::ASTNode;
    use crate::grammar::get_production_by_name;
    use crate::grammar::parse::compile_ir_ast;
    use crate::intermediate::state_construct::generate_production_states;
    use crate::runtime::parser::dispatch;
    use crate::runtime::parser::hash_jump;
    use crate::runtime::parser::reader::SymbolReader;
    use crate::runtime::parser::utf8_string_reader::UTF8StringReader;
    use crate::runtime::parser::vector_jump;
    use crate::runtime::parser::Action;
    use crate::runtime::parser::KernelState;

    #[test]
    fn test_set_production()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
    
    set prod to 222 then pass
    ",
            "0",
        );

        state.production_id = 3;

        dispatch(&mut reader, &mut state, &bytecode);

        assert_eq!(state.production_id, 222);
    }

    #[test]
    fn test_reduce()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
    
    reduce /* these number of symbols: */ 2 /* to production body_id: */ 1
    ",
            "0",
        );

        state.production_id = 3;

        match dispatch(&mut reader, &mut state, &bytecode) {
            Action::Inter_Reduce {
                body_id,
                production_id,
                symbol_count,
            } => {
                assert_eq!(body_id, 1);
                assert_eq!(symbol_count, 2);
                assert_eq!(production_id, 3);
            }
            _ => panic!("Incorrect value returned"),
        }
    }
    #[test]
    fn test_consume_nothing()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
    
    consume nothing
    ",
            "123456781234567812345678",
        );

        reader.next(10);
        state.tokens[1].byte_offset = 10;
        state.tokens[1].cp_offset = 20;
        state.tokens[1].byte_length = 5;
        state.tokens[1].cp_length = 5;

        match dispatch(&mut reader, &mut state, &bytecode) {
            Action::Shift((skip, shift)) => {
                assert_eq!(skip.cp_length, 20);
                assert_eq!(skip.byte_length, 10);

                assert_eq!(shift.cp_offset, 20);
                assert_eq!(shift.byte_offset, 10);

                assert_eq!(shift.byte_length, 0);
                assert_eq!(shift.cp_length, 0);

                // assert_eq!(reader.cursor(), 15);
                assert_eq!(state.tokens[1], state.tokens[0]);
            }
            _ => panic!("Incorrect value returned"),
        }
    }

    #[test]
    fn test_consume()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
    
    consume
    ",
            "123456781234567812345678",
        );

        state.tokens[1].byte_offset = 10;
        state.tokens[1].cp_offset = 20;
        state.tokens[1].byte_length = 5;
        state.tokens[1].cp_length = 5;

        match dispatch(&mut reader, &mut state, &bytecode) {
            Action::Shift((skip, shift)) => {
                assert_eq!(skip.cp_length, 20);
                assert_eq!(skip.byte_length, 10);

                assert_eq!(shift.cp_offset, 20);
                assert_eq!(shift.byte_offset, 10);

                assert_eq!(shift.byte_length, 5);
                assert_eq!(shift.cp_length, 5);

                // assert_eq!(reader.cursor(), 15);
                assert_eq!(state.tokens[1], state.tokens[0]);
            }
            _ => panic!("Incorrect value returned"),
        }
    }

    #[test]
    fn test_hash_table()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
assert PRODUCTION [1] (pass)
assert PRODUCTION [2] (pass)
assert PRODUCTION [3] (pass)",
            "AB",
        );

        state.production_id = 1;
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 7);
        state.production_id = 2;
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 8);
        state.production_id = 3;
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 9);
        state.production_id = 4;
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 10);
        state.production_id = 0;
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 10);
    }

    #[test]
    fn test_hash_table_skip()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
    assert BYTE [65] (skip)
    assert BYTE [66] (pass)",
            "AB",
        );

        state.is_scanner = true;

        assert_eq!(
            bytecode[hash_jump(0, &mut reader, &mut state, &bytecode) as usize],
            0
        );
    }

    #[test]
    fn test_jump_table()
    {
        let state = "
state [test]
assert PRODUCTION [1] (pass)
assert PRODUCTION [2] (pass)
assert PRODUCTION [3] (pass)"
            .to_string();

        let ir_ast = compile_ir_ast(Vec::from(state));
        assert!(ir_ast.is_ok());
        let ir_ast = ir_ast.unwrap();
        let bytecode = compile_ir_state_to_bytecode(
            &ir_ast,
            |_, _, _| BranchSelector::Vector,
            &HashMap::new(),
        );
        let mut reader = UTF8StringReader::from_str("test");
        let mut state = KernelState::new();

        println!("{}", disassemble_state(&bytecode, 0, None).0);

        state.production_id = 1;
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 7);
        state.production_id = 2;
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 8);
        state.production_id = 3;
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 9);
        state.production_id = 4;
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 10);
        state.production_id = 0;
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 10);
    }

    #[test]
    fn test_jump_table_skip()
    {
        let state = "
state [test]
assert BYTE [65] (skip)
assert BYTE [66] (pass)"
            .to_string();
        let ir_ast = compile_ir_ast(Vec::from(state));

        assert!(ir_ast.is_ok());

        let ir_ast = ir_ast.unwrap();

        let bytecode = compile_ir_state_to_bytecode(
            &ir_ast,
            |_, _, _| BranchSelector::Vector,
            &HashMap::new(),
        );

        let index: u32 = 0;
        let mut reader = UTF8StringReader::from_str("AB");
        let mut state = KernelState::new();
        state.is_scanner = true;

        println!("{}", disassemble_state(&bytecode, 0, None).0);

        assert_eq!(
            bytecode[vector_jump(index, &mut reader, &mut state, &bytecode)
                as usize],
            0
        );
    }

    fn setup_state(
        state_ir: &str,
        reader_input: &str,
    ) -> (Vec<u32>, UTF8StringReader, KernelState)
    {
        let ir_ast = compile_ir_ast(Vec::from(state_ir.to_string()));

        assert!(ir_ast.is_ok());

        let ir_ast = ir_ast.unwrap();

        let bytecode = compile_ir_state_to_bytecode(
            &ir_ast,
            |_, _, _| BranchSelector::Hash,
            &HashMap::new(),
        );

        println!("{}", disassemble_state(&bytecode, 0, None).0);

        let mut reader = UTF8StringReader::from_str(reader_input);

        let mut state = KernelState::new();

        (bytecode, reader, state)
    }
}
