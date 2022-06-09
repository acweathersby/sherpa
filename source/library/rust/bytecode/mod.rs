mod constants;

use std::collections::BTreeMap;

use std::thread::{self};

use crate::primitives::IRStateString;
use crate::{
    intermediate::state_construct::generate_production_states,
    primitives::{grammar::GrammarStore, ProductionId},
};

fn compile_ir_states_worker(
    grammar: &GrammarStore,
    productions: &[ProductionId],
) -> Vec<IRStateString> {
    productions
        .into_iter()
        .map(|p| generate_production_states(p, grammar))
        .flatten()
        .collect::<Vec<_>>()
}

///
/// Builds ir states for every standard production in
/// a grammar.
fn compile_ir_states(
    grammar: &GrammarStore,
    work_group: &[ProductionId],
    number_of_threads: usize,
) -> BTreeMap<u64, IRStateString> {
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

///
/// Builds ir states for every standard production in
/// a grammar.
fn compile_regular_ir_states(
    grammar: &GrammarStore,
    number_of_threads: usize,
) -> BTreeMap<u64, IRStateString> {
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
    return states;
}
