use std::collections::BTreeMap;
use std::collections::HashMap;
use std::path::PathBuf;

use hctk::bytecode::compiler::build_byte_code_buffer;
use hctk::debug::print_states;
use hctk::debug::BytecodeGrammarLookups;
use hctk::get_num_of_available_threads;
use hctk::grammar::compiler::compile_from_path;
use hctk::grammar::compiler::compile_from_string;
use hctk::grammar::parse::compile_ir_ast;
use hctk::intermediate::state_construct::generate_production_states;

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
            let mut deduped_states = BTreeMap::new();

            for production_id in grammar.production_table.keys() {
                let states =
                    generate_production_states(production_id, &grammar);

                for state in states {
                    if !deduped_states.contains_key(&state.get_name()) {
                        deduped_states.insert(state.get_name(), state);
                    }
                }
            }

            let state_asts = deduped_states
                .values()
                .map(|s| {
                    let string = s.get_code();

                    let (norm, _) = s.get_symbols();

                    for sym in norm {
                        println!("{}", &sym.to_string(&grammar));
                    }

                    // Create intro production scanner and dispatch it.

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

            let (bytecode, state_lookup) = build_byte_code_buffer(state_refs);

            print_states(
                &bytecode,
                Some(&BytecodeGrammarLookups::new(&grammar)),
            );
        }
    } else {
        println!("Unable to find path {:?}", input_path);
    }
}
