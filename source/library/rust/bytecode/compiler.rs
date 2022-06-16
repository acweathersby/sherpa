use std::any::Any;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;

use std::collections::VecDeque;
use std::default;
use std::hash;
use std::iter::Filter;
use std::thread::{self};
use std::vec;

use crate::bytecode::constants::IR_REDUCE_NUMERIC_LEN_ID;
use crate::bytecode::constants::NORMAL_STATE_MASK;
use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::Consume;
use crate::grammar::data::ast::ForkTo;
use crate::grammar::data::ast::Goto;
use crate::grammar::data::ast::NotInScope;
use crate::grammar::data::ast::Num;
use crate::grammar::data::ast::Pass;
use crate::grammar::data::ast::Reduce;
use crate::grammar::data::ast::ScanUntil;
use crate::grammar::data::ast::SetProd;
use crate::grammar::data::ast::SetScope;
use crate::grammar::data::ast::TokenAssign;
use crate::grammar::data::ast::ASSERT;
use crate::grammar::data::ast::HASH_NAME;
use crate::grammar::data::ast::IR_STATE;
use crate::intermediate::state_construct::generate_production_states;
use crate::primitives::grammar::GrammarStore;
use crate::primitives::IRStateString;
use crate::primitives::ProductionId;

use super::constants::DEFAULT_CASE_INDICATOR;
use super::constants::INPUT_TYPE_KEY;
use super::constants::INSTRUCTION;

fn compile_ir_states_worker(
    grammar: &GrammarStore,
    productions: &[ProductionId],
) -> Vec<IRStateString>
{
    productions
        .into_iter()
        .map(|p| generate_production_states(p, grammar))
        .flatten()
        .collect::<Vec<_>>()
}

/// Builds ir states for every standard production in
/// a grammar.

fn compile_ir_states(
    grammar: &GrammarStore,
    work_group: &[ProductionId],
    number_of_threads: usize,
) -> BTreeMap<u64, IRStateString>
{
    let production_id_chunks = work_group
        .chunks(number_of_threads)
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();

    let mut ir_map = BTreeMap::new();

    for ir_state in thread::scope(|s| {
        production_id_chunks
            .iter()
            .map(|work| s.spawn(|| compile_ir_states_worker(grammar, work)))
            // Collect now to actually generate the threads
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(move |s| s.join().unwrap())
            .collect::<Vec<_>>()
    }) {
        let key = ir_state.get_hash();

        if !ir_map.contains_key(&key) {
            ir_map.insert(key, ir_state);
        }
    }

    ir_map
}

pub enum GenerateHashTable
{
    Yes,
    No,
}

pub type TableSelector = fn(&Vec<Vec<u32>>) -> GenerateHashTable;

/// Builds ir states for every standard production in
/// a grammar.

fn compile_regular_ir_states(
    grammar: &GrammarStore,
    number_of_threads: usize,
) -> BTreeMap<u64, IRStateString>
{
    let states = compile_ir_states(
        grammar,
        &grammar
            .production_table
            .values()
            .filter(|p| !p.is_scanner)
            .map(|p| p.id)
            .collect::<Vec<_>>(),
        number_of_threads,
    );

    states
}

pub fn compile_ir_state_to_bytecode(
    state: &Box<IR_STATE>,
    table_selector: TableSelector,
) -> Vec<u32>
{
    // Determine if we are dealing with a branch state or a single line
    // state. Branch states will always have more than one assert
    // statement.

    if is_branch_state(&state) {
        // We are dealing with a branching state

        // For each branch we compile new vectors separately
        // Then we derive offset for each branch, build a suitable
        // storage structure (either HASH or JUMP) and combine all
        // that data into a single file

        // We first order the statements into blocks based on
        // on the assertion type. The order is
        // 1. Production
        // 2. Token
        // 3. Byte
        // 4. Class
        // 5. CodePoint

        build_branching_bytecode(&state.instructions, table_selector)
    } else {
        // We are dealing with a standard non-branching state
        build_branchless_bytecode(&state.instructions)
    }
}

fn is_branch_state(state: &Box<IR_STATE>) -> bool
{
    state.instructions.iter().all(|i| match i {
        ASTNode::ASSERT(_) => true,
        _ => false,
    })
}

fn build_branching_bytecode(
    instructions: &Vec<ASTNode>,
    table_selector: TableSelector,
) -> Vec<u32>
{
    let branches = instructions
        .iter()
        .filter_map(|n| match n {
            ASTNode::ASSERT(assert) => {
                if let ASTNode::Num(box Num { val }) = assert.ids[0] {
                    if val as u32 == DEFAULT_CASE_INDICATOR {
                        None
                    } else {
                        Some(assert)
                    }
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let default_branches = instructions
        .iter()
        .filter_map(|n| match n {
            ASTNode::ASSERT(assert) => {
                if let ASTNode::Num(box Num { val }) = assert.ids[0] {
                    if val as u32 == DEFAULT_CASE_INDICATOR {
                        Some(assert)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    // Extract the default branch if it exists.

    let output = if default_branches.len() > 0 {
        build_branchless_bytecode(&default_branches[0].instructions)
    } else {
        vec![INSTRUCTION::I15_FAIL]
    };

    let output = make_table(
        branches
            .iter()
            .cloned()
            .filter(|p| p.mode == "BYTE")
            .collect::<Vec<_>>(),
        INPUT_TYPE_KEY::T05_BYTE,
        output,
        table_selector,
    );

    let output = make_table(
        branches
            .iter()
            .cloned()
            .filter(|p| p.mode == "CODEPOINT")
            .collect::<Vec<_>>(),
        INPUT_TYPE_KEY::T04_CODEPOINT,
        output,
        table_selector,
    );

    let output = make_table(
        branches
            .iter()
            .cloned()
            .filter(|p| p.mode == "CLASS")
            .collect::<Vec<_>>(),
        INPUT_TYPE_KEY::T03_CLASS,
        output,
        table_selector,
    );

    let output = make_table(
        branches
            .iter()
            .cloned()
            .filter(|p| p.mode == "TOKEN")
            .collect::<Vec<_>>(),
        INPUT_TYPE_KEY::T02_TOKEN,
        output,
        table_selector,
    );

    let output = make_table(
        branches
            .iter()
            .cloned()
            .filter(|p| p.mode == "PRODUCTION")
            .collect::<Vec<_>>(),
        INPUT_TYPE_KEY::T01_PRODUCTION,
        output,
        table_selector,
    );

    output
}

fn make_table(
    branches: Vec<&Box<ASSERT>>,
    input_type_key: u32,
    mut default: Vec<u32>,
    table_selector: TableSelector,
) -> Vec<u32>
{
    if branches.is_empty() {
        return default;
    }

    use super::constants::INSTRUCTION as I;

    let lexer_type = (branches[0].is_peek as u32) + 1;
    let scanner_pointer = 0;

    let mut val_offset_map = branches
        .iter()
        .enumerate()
        .flat_map(|(index, assert)| {
            assert.ids.iter().filter_map(move |i| match i {
                ASTNode::Num(box Num { val }) => {
                    Some((*val as u32, index as u32))
                }
                _ => None,
            })
        })
        .collect::<BTreeMap<_, _>>();

    let max_spread = {
        let mut val = 0;
        let mut prev = *val_offset_map.first_key_value().unwrap().0;
        for id in val_offset_map.keys().cloned() {
            val = u32::max(id - prev, val);
            prev = id;
        }
        val
    };

    let mut branch_instructions = vec![];

    // Hash table limitations:
    // Max supported item value: 2046 with skip set to 2048
    // Max number of values: 1024 (maximum jump span)
    // Max instruction offset from table header 2042

    for branch in branches {
        if let ASTNode::Skip(_) = &branch.instructions[0] {
            for id in &branch.ids {
                if let ASTNode::Num(box Num { val }) = id {
                    val_offset_map.insert(*val as u32, 0xFFFF_FFFF as u32);
                }
            }
        } else {
            let mut instructions =
                build_branchless_bytecode(&branch.instructions);
            let offset = branch_instructions.len();
            branch_instructions.push(instructions);
            for id in &branch.ids {
                if let ASTNode::Num(box Num { val }) = id {
                    val_offset_map.insert(*val as u32, offset as u32);
                }
            }
        }
    }

    let mut output = vec![];

    match table_selector(&branch_instructions) {
        GenerateHashTable::Yes => {
            let values = val_offset_map.keys().cloned().collect::<Vec<_>>();

            let offset_lookup_table_length = values.len() as u32;

            let instruction_field_start = 4 + offset_lookup_table_length;

            let default_offset =
                (branch_instructions.len() as u32) + instruction_field_start;

            let mut pending_pairs = val_offset_map
                .clone()
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        if v == 0xFFFF_FFFF {
                            0x7FF
                        } else {
                            v + instruction_field_start
                        },
                    )
                })
                .collect::<VecDeque<_>>();

            let mod_base = f64::log2(val_offset_map.len() as f64) as u32;

            let mod_mask = (1 << mod_base) - 1;

            let mut hash_entries = (0..pending_pairs.len())
                .into_iter()
                .map(|_| 0)
                .collect::<Vec<_>>();

            let mut leftover_pairs = vec![];

            // Distribute keys-vals with unique hashes into hash table
            // slots.
            while let Some(pair) = pending_pairs.pop_front() {
                let (val, offset) = pair;
                let hash_index = ((val as u32) & mod_mask) as usize;
                if hash_entries[hash_index] == 0 {
                    hash_entries[hash_index] = ((val as u32) & 0x7FF)
                        | ((offset & 0x7FF) << 11)
                        | (512 << 22);
                } else {
                    leftover_pairs.push(pair);
                }
            }

            // What remains are hash collisions. We use simple linear
            // probing to find the next available slot, and
            // attach it to the probing chain using a signed
            // delta index.
            for (val, offset) in leftover_pairs {
                let mut pointer = 0;
                let mut node = (val & mod_mask) as usize;

                loop {
                    pointer =
                        (((hash_entries[node] >> 22) & 0x3FF) as i32) - 512;

                    if pointer == 0 {
                        break;
                    } else {
                        node += pointer as usize;
                    }
                }

                for i in 0..hash_entries.len() {
                    if hash_entries[i] == 0 {
                        // Update the previous node in the chain with the
                        // diff pointer to the new node.
                        hash_entries[node] =
                            ((((i as i32 - node as i32) + 512) as u32 & 0x3FF)
                                << 22)
                                | (hash_entries[node] & ((1 << 22) - 1));
                        // Add data for the new node.
                        hash_entries[i] = ((val) & 0x7FF)
                            | ((offset & 0x7FF) << 11)
                            | (512 << 22);

                        break;
                    }
                }
            }

            // First word header
            output.push(
                I::I10_JUMP_HASH_TABLE
                    | (input_type_key << 22)
                    | (lexer_type << 26),
            );

            // Second word header
            output.push(scanner_pointer);

            // Third word header
            output.push(
                ((mod_base & 0xFFFF) << 16) | val_offset_map.len() as u32,
            );

            output.push(default_offset);

            output.append(&mut hash_entries);
        }
        GenerateHashTable::No => {
            let values = val_offset_map.keys().cloned().collect::<Vec<_>>();
            let (start, end) =
                (*values.first().unwrap(), *values.last().unwrap());
            let token_basis = start;
            let offset_lookup_table_length = values.len() as u32;
            let instruction_field_start = 4 + offset_lookup_table_length;
            let default_offset =
                (branch_instructions.len() as u32) + instruction_field_start;

            // First word header
            output.push(
                I::I09_JUMP_OFFSET_TABLE
                    | (input_type_key << 22)
                    | (lexer_type << 26),
            );

            // Second word header
            output.push(scanner_pointer);

            // Third word header
            output.push(offset_lookup_table_length << 16 | token_basis);

            // Default Location
            output.push(default_offset);

            for branch in start..=end {
                if let Some(offset) = val_offset_map.get(&branch) {
                    if *offset == 0xFFFF_FFFF {
                        output.push(*offset);
                    } else {
                        output.push(*offset + instruction_field_start);
                    }
                } else {
                    output.push(default_offset)
                }
            }
        }
    }

    output.append(&mut branch_instructions.into_iter().flatten().collect());

    output.append(&mut default);

    output
}

fn build_branchless_bytecode(instructions: &Vec<ASTNode>) -> Vec<u32>
{
    let mut byte_code: Vec<u32> = vec![];
    use super::constants::INSTRUCTION as I;
    for instruction in instructions {
        match instruction {
            ASTNode::TokenAssign(box TokenAssign { ids }) => {}
            ASTNode::Consume(box Consume { EMPTY }) => {
                byte_code.push(I::I01_CONSUME | *EMPTY as u32)
            }
            ASTNode::Goto(box Goto { state }) => {
                if let ASTNode::HASH_NAME(box HASH_NAME { val }) = state {
                    let state_pointer_val = map_state_to_pointer(val);
                    byte_code.push(
                        I::I02_GOTO | NORMAL_STATE_MASK | state_pointer_val,
                    );
                } else {
                    panic!("Invalid state type in goto instruction");
                }
            }
            ASTNode::ScanUntil(box ScanUntil {
                ids,
                SCAN_BACKWARDS,
            }) => {}
            ASTNode::ForkTo(box ForkTo { states }) => {}
            ASTNode::Skip(box Skip) => {}
            ASTNode::Pass(_) => byte_code.push(I::I00_PASS),
            ASTNode::Fail(_) => byte_code.push(I::I15_FAIL),
            ASTNode::NotInScope(box NotInScope { ids }) => {}
            ASTNode::SetScope(box SetScope { scope }) => {}
            ASTNode::SetProd(box SetProd { id }) => {
                if let ASTNode::Num(box Num { val }) = id {
                    byte_code.push(I::I03_SET_PROD | *val as u32)
                }
            }
            ASTNode::Reduce(box Reduce { body_id, len, .. }) => {
                if *len as u32 == IR_REDUCE_NUMERIC_LEN_ID {
                    byte_code
                        .push(I::I04_REDUCE | (*body_id as u32) << 16 | 0xFFFF);
                } else {
                    byte_code.push(
                        I::I04_REDUCE | (*len as u32) << 16 | *body_id as u32,
                    );
                }
            }
            _ => {}
        }
    }

    if let Some(last) = byte_code.last() {
        if *last != I::I00_PASS && *last != I::I15_FAIL {
            byte_code.push(I::I00_PASS);
        }
    }

    byte_code
}

fn map_state_to_pointer(state_name: &String) -> u32
{
    0
}

#[cfg(test)]

mod byte_code_creation_tests
{

    use crate::bytecode::compiler::compile_ir_state_to_bytecode;
    use crate::bytecode::compiler::GenerateHashTable;
    use crate::debug::compile_test_grammar;
    use crate::grammar::data::ast::ASTNode;
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

        println!("{:#?}", result);

        let state = result[0].get_code();

        let result = compile_ir_ast(Vec::from(state.as_bytes()));

        println!("{:#?}", result);

        assert!(result.is_ok());

        let result = compile_ir_state_to_bytecode(&result.unwrap(), |_| {
            GenerateHashTable::No
        });

        println!("{:#?}", result);
    }
}
