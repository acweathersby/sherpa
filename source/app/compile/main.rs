#![feature(scoped_threads)]

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::thread;

use hctk::bytecode::compiler::build_byte_code_buffer;
use hctk::debug::print_states;
use hctk::debug::BytecodeGrammarLookups;
use hctk::get_num_of_available_threads;
use hctk::grammar::compiler::compile_from_path;
use hctk::grammar::parse::compile_ir_ast;
use hctk::intermediate::state_construct::compile_states;

fn main()
{
    let input_path = PathBuf::from("./grammar.hcg");

    if input_path.exists() {
        let threads = get_num_of_available_threads();

        eprintln!("Number of threads used: {}", threads);

        let (grammar, errors) = compile_from_path(&input_path, threads);

        if !errors.is_empty() {
            for error in errors {
                println!("{}", error);
            }
        } else if let Some(grammar) = grammar {
            let mut deduped_states = compile_states(&grammar, threads);

            let state_asts = deduped_states
                .values()
                .map(|s| {
                    let string = s.get_code();

                    // Done
                    println!("{}", &string);
                    match compile_ir_ast(Vec::from(string.as_bytes())) {
                        Ok(ast) => *ast,
                        Err(err) => {
                            panic!("\n{}", err);
                        }
                    }
                })
                .collect::<Vec<_>>();

            let state_refs = state_asts.iter().collect::<Vec<_>>();

            let (bytecode, _) = build_byte_code_buffer(state_refs);

            print_states(
                &bytecode,
                Some(&BytecodeGrammarLookups::new(&grammar)),
            );

            println!("{:?}", bytecode);
        }
    } else {
        println!("Unable to find path {:?}", input_path);
    }
}
