use std::hash;

use crate::bytecode::constants::INSTRUCTION as I;
use crate::bytecode::constants::STATE_INDEX_MASK;
use crate::primitives::KernelToken;
use crate::primitives::Token;
use crate::runtime::buffer::ByteReader;

use super::recognizer::stack::KernelStack;

struct KernelState
{
    stack:           KernelStack,
    anchor_token:    KernelToken,
    assert_token:    KernelToken,
    peek_token:      KernelToken,
    sym_accumulator: u32,
    token_end:       usize,
    production_id:   u32,
    pointer:         u32,
}

impl KernelState
{
    pub fn new() -> Self
    {
        Self {
            stack:           KernelStack::new(),
            anchor_token:    KernelToken::new(),
            assert_token:    KernelToken::new(),
            peek_token:      KernelToken::new(),
            sym_accumulator: 0,
            token_end:       0,
            production_id:   0,
            pointer:         0,
        }
    }
}

enum Action
{
    Undefined,
    CompleteState,
    FailState,
    Fork,
    Shift(KernelToken),
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
fn dispatch<T: ByteReader>(
    reader: &mut T,
    state: &mut KernelState,
    bytecode: &[u32],
) -> Action
{
    use Action::*;

    let mut index = (state.pointer & STATE_INDEX_MASK) as u32;

    loop {
        match bytecode[index as usize] & 0xF000_0000 {
            I::I01_CONSUME => break consume(),
            I::I02_GOTO => index = goto(),
            I::I03_SET_PROD => index = set_production(),
            I::I04_REDUCE => break reduce(),
            I::I05_TOKEN => index = set_token_state(),
            I::I06_FORK_TO => break fork(),
            I::I07_SCAN => index = scan(),
            I::I08_NOOP => index = noop(index),
            I::I09_JUMP_OFFSET_TABLE => {
                index = jump_table(index, reader, state, bytecode)
            }
            I::I10_JUMP_HASH_TABLE => {
                index = hash_table(index, reader, state, bytecode)
            }
            I::I11_SET_FAIL_STATE => index = set_fail(),
            I::I12_REPEAT => index = repeat(),
            I::I13_NOOP => index = noop(index),
            I::I14_ASSERT_CONSUME => {}
            I::I15_FAIL => break FailState,
            _ => break CompleteState,
        }
    }
}

fn consume() -> Action
{
    Action::Undefined
}

fn reduce() -> Action
{
    Action::Undefined
}

fn goto() -> u32
{
    0
}

fn set_production() -> u32
{
    0
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
fn hash_table<T: ByteReader>(
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
    let modulo_base = third >> 16;
    let hash_mask = 1 << (modulo_base - 1);
    let table_size = third & 0xFFFF;
    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            1 => state.production_id,
            _ => get_token_value() as u32,
        };
        let mut hash_index = (input_value & hash_mask) as usize;

        loop {
            let cell =
                unsafe { *bytecode.get_unchecked(table_start + hash_index) };
            let value = cell & 0x7FF;
            let offset = (cell >> 11) & 0x7FF;
            let next = ((cell >> 22) & 0x3FF) as i32 - 512;
            if offset == 0x7FF {
                unimplemented!("Need to implement skip!");
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

fn get_token_value() -> i32
{
    0
}

fn jump_table<T: ByteReader>(
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
    let table_basis = third & 0xFFFF;
    let table_length = (third >> 16) & 0xFFFF;
    let table_start = i + 4;

    loop {
        let input_value = match input_type {
            1 => state.production_id,
            _ => get_token_value() as u32,
        };

        let value_index = (input_value as i32 - table_basis as i32) as u32;

        if value_index < table_length {
            let offset = unsafe {
                *bytecode.get_unchecked(table_start + value_index as usize)
            };
            if offset == 0xFFFF_FFFF {
                unimplemented!("Need to implement skip!");
                continue;
            } else {
                return index + offset;
            }
        } else {
            return index + bytecode[table_start - 1];
        }
    }
}
#[cfg(test)]
mod test
{
    use crate::bytecode::compiler::compile_ir_state_to_bytecode;
    use crate::debug::compile_test_grammar;
    use crate::grammar::data::ast::ASTNode;
    use crate::grammar::get_production_by_name;
    use crate::grammar::parse::compile_ir_ast;
    use crate::intermediate::state_construct::generate_production_states;
    use crate::runtime::buffer::UTF8StringReader;
    use crate::runtime::parser::hash_table;
    use crate::runtime::parser::jump_table;
    use crate::runtime::parser::KernelState;
    #[test]
    fn test_hash_table()
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

        let bytecode = compile_ir_state_to_bytecode(&ir_ast, |_| {
            crate::bytecode::compiler::GenerateHashTable::Yes
        });

        let index: u32 = 0;

        let mut reader = UTF8StringReader::from_str("test");

        let mut state = KernelState::new();

        state.production_id = 1;
        assert_eq!(hash_table(index, &mut reader, &mut state, &bytecode), 7);
        state.production_id = 2;
        assert_eq!(hash_table(index, &mut reader, &mut state, &bytecode), 8);
        state.production_id = 3;
        assert_eq!(hash_table(index, &mut reader, &mut state, &bytecode), 9);
        state.production_id = 4;
        assert_eq!(hash_table(index, &mut reader, &mut state, &bytecode), 10);
        state.production_id = 0;
        assert_eq!(hash_table(index, &mut reader, &mut state, &bytecode), 10);
    }

    #[test]
    fn test_hash_table_skip()
    {
        let state = "
state [test]
assert PRODUCTION [1] (skip)
assert PRODUCTION [2] (pass)"
            .to_string();

        let ir_ast = compile_ir_ast(Vec::from(state));

        assert!(ir_ast.is_ok());

        let ir_ast = ir_ast.unwrap();

        let bytecode = compile_ir_state_to_bytecode(&ir_ast, |_| {
            crate::bytecode::compiler::GenerateHashTable::Yes
        });

        let index: u32 = 0;

        let mut reader = UTF8StringReader::from_str("test");

        let mut state = KernelState::new();

        state.production_id = 1;
        assert_eq!(hash_table(index, &mut reader, &mut state, &bytecode), 1);
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

        let bytecode = compile_ir_state_to_bytecode(&ir_ast, |_| {
            crate::bytecode::compiler::GenerateHashTable::No
        });

        let index: u32 = 0;

        let mut reader = UTF8StringReader::from_str("test");

        let mut state = KernelState::new();

        state.production_id = 1;
        assert_eq!(jump_table(index, &mut reader, &mut state, &bytecode), 7);
        state.production_id = 2;
        assert_eq!(jump_table(index, &mut reader, &mut state, &bytecode), 8);
        state.production_id = 3;
        assert_eq!(jump_table(index, &mut reader, &mut state, &bytecode), 9);
        state.production_id = 4;
        assert_eq!(jump_table(index, &mut reader, &mut state, &bytecode), 10);
        state.production_id = 0;
        assert_eq!(jump_table(index, &mut reader, &mut state, &bytecode), 10);
    }

    #[test]
    fn test_jump_table_skip()
    {
        let state = "
state [test]
assert PRODUCTION [1] (skip)
assert PRODUCTION [2] (pass)"
            .to_string();

        let ir_ast = compile_ir_ast(Vec::from(state));

        assert!(ir_ast.is_ok());

        let ir_ast = ir_ast.unwrap();

        let bytecode = compile_ir_state_to_bytecode(&ir_ast, |_| {
            crate::bytecode::compiler::GenerateHashTable::No
        });

        let index: u32 = 0;

        let mut reader = UTF8StringReader::from_str("test");

        let mut state = KernelState::new();

        state.production_id = 1;
        assert_eq!(jump_table(index, &mut reader, &mut state, &bytecode), 1);
    }
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
