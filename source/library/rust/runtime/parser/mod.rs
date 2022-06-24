mod parse_functions;
mod reader;
mod utf8_string_reader;

pub use parse_functions::get_next_action;
pub use parse_functions::Action;
pub use reader::SymbolReader;
pub use utf8_string_reader::UTF8StringReader;

#[cfg(test)]
mod test_parser
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
    use crate::grammar::get_production_id_by_name;
    use crate::grammar::parse::compile_ir_ast;
    use crate::intermediate::state_construct::generate_production_states;
    use crate::primitives::KernelState;
    use crate::primitives::KernelToken;
    use crate::primitives::Token;
    use crate::runtime::parser::parse_functions::dispatch;
    use crate::runtime::parser::parse_functions::hash_jump;
    use crate::runtime::parser::parse_functions::vector_jump;
    use crate::runtime::parser::parse_functions::Action;
    use crate::runtime::parser::reader::SymbolReader;
    use crate::runtime::parser::utf8_string_reader::UTF8StringReader;

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
        state.init_normal_state(NORMAL_STATE_MASK | FIRST_STATE_OFFSET);

        match get_next_action(&mut reader, &mut state, &bytecode) {
            Action::Accept { production_id } => {
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
            Action::Reduce {
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

        state.set_assert_token(KernelToken {
            byte_length: 5,
            byte_offset: 10,
            cp_length:   5,
            cp_offset:   20,
            line_number: 0,
            line_offset: 0,
            typ:         0,
        });

        match dispatch(&mut reader, &mut state, &bytecode) {
            Action::Shift((skip, shift)) => {
                assert_eq!(skip.cp_length, 20);
                assert_eq!(skip.byte_length, 10);

                assert_eq!(shift.cp_offset, 20);
                assert_eq!(shift.byte_offset, 10);

                assert_eq!(shift.byte_length, 0);
                assert_eq!(shift.cp_length, 0);

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

        state.set_assert_token(KernelToken {
            byte_length: 5,
            byte_offset: 10,
            cp_length:   5,
            cp_offset:   20,
            line_number: 0,
            line_offset: 0,
            typ:         0,
        });

        match dispatch(&mut reader, &mut state, &bytecode) {
            Action::Shift((skip, shift)) => {
                assert_eq!(skip.cp_length, 20);
                assert_eq!(skip.byte_length, 10);

                assert_eq!(shift.cp_offset, 20);
                assert_eq!(shift.byte_offset, 10);

                assert_eq!(shift.byte_length, 5);
                assert_eq!(shift.cp_length, 5);

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
        let mut reader = UTF8StringReader::from_str("test");
        let mut state = KernelState::new();

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
        let mut reader = UTF8StringReader::from_str("AB");
        let mut state = KernelState::new();

        println!("{}", disassemble_state(&bytecode, 0, None).0);

        state.set_production(1);
        assert_eq!(vector_jump(index, &mut reader, &mut state, &bytecode), 6);

        state.set_production(2);
        assert_eq!(vector_jump(index, &mut reader, &mut state, &bytecode), 8);
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
