use std::hash;
mod reader;
mod utf8_string_reader;

use crate::bytecode::constants::DEFAULT_FAIL_INSTRUCTION_OFFSET;
use crate::bytecode::constants::INPUT_TYPE;
use crate::bytecode::constants::INSTRUCTION as I;
use crate::bytecode::constants::INSTRUCTION_CONTENT_MASK;
use crate::bytecode::constants::INSTRUCTION_HEADER_MASK;
use crate::bytecode::constants::LEXER_TYPE;
use crate::bytecode::constants::NORMAL_STATE_MASK;
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
    active_state:    u32,
    sym_accumulator: u32,
    production_id:   u32,
    pointer:         u32,
    in_peek_mode:    bool,
    in_fail_mode:    bool,
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
            active_state:    0,
            sym_accumulator: 0,
            production_id:   0,
            pointer:         0,
            in_fail_mode:    false,
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
    Fork
    {
        states_start_offset: u32,
        num_of_states:       u32,
        target_production:   u32,
    },
    Shift((KernelToken, KernelToken)),
    Inter_Reduce
    {
        production_id: u32,
        body_id:       u32,
        symbol_count:  u32,
    },
    Skip(KernelToken),
    Accept,
    Error
    {
        message:    &'static str,
        last_input: KernelToken,
    },
}

fn reference_runner<T: SymbolReader>(
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> Action
{
    if state.active_state == 0 {
        // Decode the next state.
        state.active_state = state.stack.pop_state();
    }

    loop {
        if state.active_state < 1 {
            if reader.offset_at_end(state.tokens[1].byte_offset) {
                break Action::Accept;
            } else {
                break Action::Error {
                    message:    "Cannot parse this symbol",
                    last_input: state.tokens[1],
                };
            }
        } else {
            let mask_gate = NORMAL_STATE_MASK << (state.in_fail_mode as u32);

            if !state.in_fail_mode {}

            if (state.active_state & mask_gate) != 0 {
                match dispatch(reader, state, bytecode) {
                    Action::CompleteState => {
                        state.in_fail_mode = false;
                        state.active_state = state.stack.pop_state();
                    }
                    Action::FailState => {
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

/// Yields parser Actions from parsing an input using the
/// current active grammar bytecode.
fn dispatch<T: SymbolReader>(
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> Action
{
    use Action::*;

    let mut index = (state.active_state & STATE_INDEX_MASK) as u32;

    loop {
        let instruction = unsafe { *bytecode.get_unchecked(index as usize) };

        state.active_state = index;

        index = match instruction & INSTRUCTION_HEADER_MASK {
            I::I01_CONSUME => break consume(instruction, state),
            I::I02_GOTO => goto(index, instruction, state),
            I::I03_SET_PROD => set_production(index, instruction, state),
            I::I04_REDUCE => break reduce(instruction, state),
            I::I05_TOKEN => set_token_state(),
            I::I06_FORK_TO => break fork(index, instruction),
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

#[inline]
fn goto(index: u32, instruction: u32, state: &mut KernelState) -> u32
{
    state.stack.push_state(instruction);
    index + 1
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

#[inline]
fn fork(index: u32, instruction: u32) -> Action
{
    let instruction = instruction & INSTRUCTION_CONTENT_MASK;
    let target_production = instruction & 0xFFFF;
    let num_of_states = (instruction >> 16) & 0xFFFF;

    Action::Fork {
        num_of_states,
        states_start_offset: index + 1,
        target_production,
    }
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

#[cfg(test)]
mod test
{
    use std::collections::HashMap;

    use crate::bytecode;
    use crate::bytecode::compiler::build_byte_code_buffer;
    use crate::bytecode::compiler::compile_ir_state_to_bytecode;
    use crate::bytecode::constants::BranchSelector;
    use crate::bytecode::constants::FIRST_STATE_OFFSET;
    use crate::bytecode::constants::NORMAL_STATE_MASK;
    use crate::debug::compile_test_grammar;
    use crate::debug::disassemble_state;
    use crate::debug::print_states;
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

    use super::reference_runner;

    #[test]
    fn test_fork()
    {
        let (bytecode, mut reader, mut state) = setup_states(
            vec![
                "
state [test_start]
    
    fork to ( state [X]  state [Y]  state [Z] ) to complete 2 then pass
    ",
                "
state [X]
    
    set prod to 2 then reduce 0 1
    ",
                "
state [Y]
    
    set prod to 2 then reduce 0 2
    ",
                "
state [Z]
    
    set prod to 2 then reduce 0 3
    ",
            ],
            "",
        );

        state
            .stack
            .push_state(NORMAL_STATE_MASK | FIRST_STATE_OFFSET);

        match reference_runner(&mut reader, &mut state, &bytecode) {
            Action::Fork {
                states_start_offset,
                num_of_states,
                target_production,
            } => {
                assert_eq!(states_start_offset, FIRST_STATE_OFFSET + 1);
                assert_eq!(num_of_states, 3);
                assert_eq!(target_production, 2);
            }
            _ => panic!("Could not complete parse"),
        }
    }

    #[test]
    fn test_goto()
    {
        let (bytecode, mut reader, mut state) = setup_states(
            vec![
                "
state [test_start]
    
    goto state [test_end]
    ",
                "
state [test_end]
    
    set prod to 444 then pass
    ",
            ],
            "",
        );

        state
            .stack
            .push_state(NORMAL_STATE_MASK | FIRST_STATE_OFFSET);

        match reference_runner(&mut reader, &mut state, &bytecode) {
            Action::Accept => {
                assert_eq!(state.production_id, 444);
            }
            _ => panic!("Could not complete parse"),
        }
    }

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

    fn setup_states(
        state_ir: Vec<&str>,
        reader_input: &str,
    ) -> (Vec<u32>, UTF8StringReader, KernelState)
    {
        let is_asts = state_ir
            .into_iter()
            .map(|s| {
                let result = compile_ir_ast(Vec::from(s.to_string()));

                match result {
                    Ok(ast) => *ast,
                    Err(err) => {
                        println!("{}", err);
                        panic!("Could not build state:\n{}", s);
                    }
                }
            })
            .collect::<Vec<_>>();

        let (bytecode, _) = build_byte_code_buffer(is_asts.iter().collect());

        print_states(&bytecode, None);

        let mut reader = UTF8StringReader::from_str(reader_input);
        let mut state = KernelState::new();

        (bytecode, reader, state)
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

        print_states(&bytecode, None);

        let mut reader = UTF8StringReader::from_str(reader_input);
        let mut state = KernelState::new();

        (bytecode, reader, state)
    }
}
