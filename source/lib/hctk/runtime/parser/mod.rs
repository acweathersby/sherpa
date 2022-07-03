mod parse_functions;

use crate::types::ParseAction;
use crate::types::ParseState;
use crate::types::SymbolReader;
pub use parse_functions::get_next_action;
pub use parse_functions::get_next_actionBB;

#[cfg(test)]
mod test_parser
{
    use std::collections::HashMap;

    use crate::bytecode;
    use crate::bytecode::compile::build_byte_code_buffer;
    use crate::bytecode::compile::compile_ir_state_to_bytecode;
    use crate::bytecode::constants::BranchSelector;
    use crate::bytecode::constants::FIRST_STATE_OFFSET;
    use crate::bytecode::constants::NORMAL_STATE_MASK;
    use crate::debug::compile_test_grammar;
    use crate::debug::disassemble_state;
    use crate::debug::generate_disassembly;
    use crate::grammar::data::ast::ASTNode;
    use crate::grammar::get_production_id_by_name;
    use crate::grammar::parse::compile_ir_ast;
    use crate::intermediate::state::generate_production_states;
    use crate::runtime::parser::parse_functions::dispatch;
    use crate::runtime::parser::parse_functions::hash_jump;
    use crate::runtime::parser::parse_functions::vector_jump;
    use crate::types::*;

    use super::parse_functions::get_next_action;

    #[test]
    fn test_fork()
    {
        let (bytecode, mut reader, mut state) = setup_states(
            vec![
                "
state [test_start]
    
    fork to ( state [X]  state [Y]  state [Z] ) to complete prod 2 then pass
    ",
                "
state [X]
    
    set prod to 2 then reduce 0 symbols to body 1
    ",
                "
state [Y]
    
    set prod to 2 then reduce 0 symbols to body 2
    ",
                "
state [Z]
    
    set prod to 2 then reduce 0 symbols to body 3
    ",
            ],
            "",
        );

        state.init_normal_state(NORMAL_STATE_MASK | FIRST_STATE_OFFSET);

        match get_next_action(&mut reader, &mut state, &bytecode) {
            ParseAction::Fork {
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
        state.init_normal_state(NORMAL_STATE_MASK | FIRST_STATE_OFFSET);

        match get_next_action(&mut reader, &mut state, &bytecode) {
            ParseAction::Accept { production_id } => {
                assert_eq!(production_id, 444);
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

        state.set_production(3);

        dispatch(&mut reader, &mut state, &bytecode);

        assert_eq!(state.get_production(), 222);
    }

    #[test]
    fn test_reduce()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test]
    
    reduce 2 symbols to body 1
    ",
            "0",
        );

        state.set_production(3);

        match dispatch(&mut reader, &mut state, &bytecode) {
            ParseAction::Reduce {
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

        state.set_assert_token(ParseToken {
            byte_length: 5,
            byte_offset: 10,
            cp_length: 5,
            cp_offset: 20,
            ..Default::default()
        });

        match dispatch(&mut reader, &mut state, &bytecode) {
            ParseAction::Shift {
                skipped_characters,
                token,
            } => {
                assert_eq!(skipped_characters.cp_length, 20);
                assert_eq!(skipped_characters.byte_length, 10);

                assert_eq!(token.cp_offset, 20);
                assert_eq!(token.byte_offset, 10);

                assert_eq!(token.byte_length, 0);
                assert_eq!(token.cp_length, 0);

                // assert_eq!(reader.cursor(), 15);
                assert_eq!(state.get_anchor_token(), state.get_assert_token());
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

        state.set_assert_token(ParseToken {
            byte_length: 5,
            byte_offset: 10,
            cp_length: 5,
            cp_offset: 20,
            ..Default::default()
        });

        match dispatch(&mut reader, &mut state, &bytecode) {
            ParseAction::Shift {
                skipped_characters,
                token,
            } => {
                assert_eq!(skipped_characters.cp_length, 20);
                assert_eq!(skipped_characters.byte_length, 10);

                assert_eq!(token.cp_offset, 20);
                assert_eq!(token.byte_offset, 10);

                assert_eq!(token.byte_length, 5);
                assert_eq!(token.cp_length, 5);

                // assert_eq!(reader.cursor(), 15);
                assert_eq!(state.get_anchor_token(), state.get_assert_token());
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

        state.set_production(1);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 7);
        state.set_production(2);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 8);
        state.set_production(3);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 9);
        state.set_production(4);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 10);
        state.set_production(0);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 10);
    }

    #[test]
    fn test_hash_table_skip()
    {
        let (bytecode, mut reader, mut state) = setup_state(
            "
state [test] scanner [none]
    skip [1] 
    assert TOKEN [0] ( set prod to 44 then pass)",
            "",
        );

        state.set_production(1);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 6);
        state.set_production(2);
        assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 8);
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
        let mut reader = UTF8StringReader::from_string("test");
        let mut state = ParseState::new();

        println!("{}", disassemble_state(&bytecode, 0, None).0);

        state.set_production(1);
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 7);
        state.set_production(2);
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 8);
        state.set_production(3);
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 9);
        state.set_production(4);
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 10);
        state.set_production(0);
        assert_eq!(vector_jump(0, &mut reader, &mut state, &bytecode), 10);
    }

    #[test]
    fn test_jump_table_skip()
    {
        let state = "
state [test] scanner [none]
    skip [1] 
    assert TOKEN [0] ( set prod to 44 then pass)"
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
        let mut reader = UTF8StringReader::from_string("AB");
        let mut state = ParseState::new();

        println!("{}", disassemble_state(&bytecode, 0, None).0);

        state.set_production(1);
        assert_eq!(vector_jump(index, &mut reader, &mut state, &bytecode), 6);

        state.set_production(2);
        assert_eq!(vector_jump(index, &mut reader, &mut state, &bytecode), 8);
    }

    fn setup_states(
        state_ir: Vec<&str>,
        reader_input: &str,
    ) -> (Vec<u32>, UTF8StringReader, ParseState)
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

        generate_disassembly(&bytecode, None);

        let mut reader = UTF8StringReader::from_string(reader_input);
        let mut state = ParseState::new();

        (bytecode, reader, state)
    }
    fn setup_state(
        state_ir: &str,
        reader_input: &str,
    ) -> (Vec<u32>, UTF8StringReader, ParseState)
    {
        let ir_ast = compile_ir_ast(Vec::from(state_ir.to_string()));

        assert!(ir_ast.is_ok());

        let ir_ast = ir_ast.unwrap();

        let bytecode = compile_ir_state_to_bytecode(
            &ir_ast,
            |_, _, _| BranchSelector::Hash,
            &HashMap::new(),
        );

        generate_disassembly(&bytecode, None);

        let mut reader = UTF8StringReader::from_string(reader_input);
        let mut state = ParseState::new();

        (bytecode, reader, state)
    }
}
