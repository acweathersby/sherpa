#![feature(scoped_threads)]

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::thread;

use hctk::bytecode::compiler::build_byte_code_buffer;
use hctk::debug::print_states;
use hctk::debug::BytecodeGrammarLookups;
use hctk::get_num_of_available_threads;
use hctk::grammar::compiler::compile_from_path;
use hctk::grammar::parse::compile_ir_ast;
use hctk::intermediate::state_construct::generate_production_states;
use hctk::intermediate::state_construct::generate_scanner_intro_state;

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
            let productions_ids =
                grammar.production_table.keys().cloned().collect::<Vec<_>>();

            let work_chunks =
                productions_ids.chunks(threads).collect::<Vec<_>>();

            let mut deduped_states = BTreeMap::new();

            for state in thread::scope(|s| {
                work_chunks
                    .iter()
                    .map(|productions| {
                        s.spawn(|| {
                            let mut deduped_states = BTreeMap::new();
                            let mut scanner_names = HashSet::new();

                            for production_id in *productions {
                                let states = generate_production_states(
                                    production_id,
                                    &grammar,
                                );

                                for state in states {
                                    if let Some(name) =
                                        state.get_scanner_state_name()
                                    {
                                        if scanner_names.insert(name) {
                                            for state in
                                                generate_scanner_intro_state(
                                                    state
                                                        .get_scanner_symbol_set(
                                                        )
                                                        .unwrap(),
                                                    &grammar,
                                                )
                                            {
                                                if !deduped_states.contains_key(
                                                    &state.get_name(),
                                                ) {
                                                    deduped_states.insert(
                                                        state.get_name(),
                                                        state,
                                                    );
                                                }
                                            }
                                        }
                                    }

                                    if !deduped_states
                                        .contains_key(&state.get_name())
                                    {
                                        deduped_states
                                            .insert(state.get_name(), state);
                                    }
                                }
                            }

                            deduped_states.into_values()
                        })
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .flat_map(move |s| s.join().unwrap())
                    .collect::<Vec<_>>()
            }) {
                if !deduped_states.contains_key(&state.get_name()) {
                    deduped_states.insert(state.get_name(), state);
                }
            }

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
